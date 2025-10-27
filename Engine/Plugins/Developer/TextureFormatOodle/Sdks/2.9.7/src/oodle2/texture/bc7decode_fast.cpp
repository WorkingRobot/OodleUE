// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetsse4

#include "bc67format.h"
#include "bc67tables.h"
#include "bc7decode_fast.h"
#include "bc7bits.h"
#include "cpux86.h"
#include "rrbits.h"
#include "rrsimd.h"
#include "vec128.inl"
#include "cbradutil.h"
#include "oodlemalloc.h"

#ifdef DO_BUILD_SSE4
#include <smmintrin.h>
#endif

OODLE_NS_START

// For two-subset partitions, one partition representative each for one of the
// 4 equivalence classes wrt anchor pairs (see radtex_subset_anchors[])
static const BC67Partition * two_subset_repr[4] =
{
	&bc67_partitions[1 +  0], // class 0 (second anchor is 15)
	&bc67_partitions[1 + 17], // class 1 (second anchor is 2)
	&bc67_partitions[1 + 18], // class 2 (second anchor is 8)
	&bc67_partitions[1 + 34], // class 3 (second anchor is 6)
};

#if defined(DO_BUILD_SSE4) || defined(DO_BUILD_NEON64)

#include "bc7decode_fast_common.inl"

static RADFORCEINLINE void simd_interpolate_one_subset_twoind(U8 * out_rgba, U32 crot_idxmode, const U8 * lerpf_buf, const Vec128_U16 &endpoints16_in, bool ignore_alpha)
{
	// Determine channel rotate shuffle
	Vec128_U8 crot_shuf = simd_channel_rotate16(crot_idxmode & 3);

	// Apply to endpoints
	Vec128_U8 endpoints16 = endpoints16_in.u8().shuf(crot_shuf);

	// Grab the 8 lerp factors we'll need and replicate them in the pattern
	// we need, stretching them out to 16 bits by left-shifting by 8
	// along the way. Also apply channel rotate.
	static RAD_ALIGN(const S8, lerpf16_shuffles[8][16], 16) =
	{
		// idxMode=0 (no index swap)
		{ -16,0, -16,0, -16,0, -16,1,  -16,2, -16,2, -16,2, -16,3 }, // rot=0: no change
		{ -16,1, -16,0, -16,0, -16,0,  -16,3, -16,2, -16,2, -16,2 }, // rot=1: swap R<->A
		{ -16,0, -16,1, -16,0, -16,0,  -16,2, -16,3, -16,2, -16,2 }, // rot=2: swap G<->A
		{ -16,0, -16,0, -16,1, -16,0,  -16,2, -16,2, -16,3, -16,2 }, // rot=3: swap B<->A

		// idxMode=1 (index swap)
		{ -16,1, -16,1, -16,1, -16,0,  -16,3, -16,3, -16,3, -16,2 }, // rot=0: no change
		{ -16,0, -16,1, -16,1, -16,1,  -16,2, -16,3, -16,3, -16,3 }, // rot=1: swap R<->A
		{ -16,1, -16,0, -16,1, -16,1,  -16,3, -16,2, -16,3, -16,3 }, // rot=2: swap G<->A
		{ -16,1, -16,1, -16,0, -16,1,  -16,3, -16,3, -16,2, -16,3 }, // rot=3: swap B<->A
	};
	Vec128_U8 lerpf16_shuf0 = Vec128_U8::loada(lerpf16_shuffles[crot_idxmode]);
	Vec128_U8 lerpf16_shuf1 = lerpf16_shuf0 + Vec128_U8(4);

	// Shuffle the endpoints around so we have the "low" and "high" end separated out
	Vec128_S16 lo16 = endpoints16.unpack_lo64(endpoints16).s16();
	Vec128_S16 hi16 = endpoints16.unpack_hi64(endpoints16).s16();

	simd_alpha_override_endpoints16(&lo16, &hi16, ignore_alpha);

	// Set up for interpolation
	Vec128_S16 diff16 = lo16 - hi16; // yes, lo-hi!

	// Interpolate the pixel values!
	for (SINTa i = 0; i < 4; ++i)
	{
		// Grab lerp factors
		Vec128_U8 lerpf8 = Vec128_U8::loadu_lo64(lerpf_buf + i*8);
		Vec128_S16 lerpf16_0 = lerpf8.shuf(lerpf16_shuf0).s16();
		Vec128_S16 lerpf16_1 = lerpf8.shuf(lerpf16_shuf1).s16();

		// Interpolate via rounding_shift_right(neg_factor * (lo - hi), 15) + lo
		Vec128_S16 interp16_0 = vinterp16(lerpf16_0, diff16, lo16);
		Vec128_S16 interp16_1 = vinterp16(lerpf16_1, diff16, lo16);

		// Pack down to 8 bits and store
		Vec128_U8 interp8 = interp16_0.to_uint8_sat(interp16_1);
		interp8.storeu(out_rgba + i * 16);
	}
}

static RADFORCEINLINE void simd_interpolate_two_subset(U8 * out_rgba, const BC67Partition * partition, const Vec128_U8 &lerpf8, const Vec128_S16 &lo16, const Vec128_S16 &hi16)
{
	// Set up for interpolation
	Vec128_S16 diff16 = lo16 - hi16; // yes, lo-hi!

	// lo16/diff16 diffs between the subsets
	// we use math to select subsets and not blends/shuffles because we are
	// already doing a ton of those and Haswell through Skylake only have
	// one shuffle port
	Vec128_S16 lo16_subset0 = lo16.s32().xyxy().s16();
	Vec128_S16 lo16_subset1 = lo16.s32().zwzw().s16() - lo16_subset0;
	Vec128_S16 diff16_subset0 = diff16.s32().xyxy().s16();
	Vec128_S16 diff16_subset1 = diff16.s32().zwzw().s16() ^ diff16_subset0;

	// Subset mask for the pixels
#if defined(DO_BUILD_SSE4)
	Vec128_S32 subset_mask { _mm_cvtsi32_si128(partition->subset_mask) };
	subset_mask = Vec128_S32(_mm_unpacklo_epi64(subset_mask, subset_mask));
#else // defined(DO_BUILD_NEON64)
	Vec128_S32 subset_mask { vdupq_n_u64(partition->subset_mask) };
#endif

	const Vec128_S32 in_subset1_mask_0 { 1<<0, 0, 1<<2, 0 };
	const Vec128_S32 in_subset1_mask_1 { 1<<4, 0, 1<<6, 0 };

	// Shuffle for low/high lerp factors
	// replicates them 4x each and also expands to 16 bits by left-shifting by 8
	// -16 here because we keep adding to this value
	Vec128_U8 lerpf16_shuf = Vec128_S8(-16,0, -16,0, -16,0, -16,0, -16,1, -16,1, -16,1, -16,1).u8();
	const Vec128_U8 lerpf16_shuf_incr { 2 };

	// Interpolate the pixel values!
	for (SINTa i = 0; i < 64; i += 16)
	{
		// Determine 64-bit masks for whether a pixel is in subset 1
		Vec128_S16 in_subset1_0 = (subset_mask & in_subset1_mask_0).cmp_eq64(in_subset1_mask_0).s16();
		Vec128_S16 in_subset1_1 = (subset_mask & in_subset1_mask_1).cmp_eq64(in_subset1_mask_1).s16();

		// Replicate the lerp factors 4x each and expand to 16 bits by shifting
		// left by 8
		Vec128_S16 lerpf16_0 = lerpf8.shuf(lerpf16_shuf).s16();
		lerpf16_shuf += lerpf16_shuf_incr;
		Vec128_S16 lerpf16_1 = lerpf8.shuf(lerpf16_shuf).s16();
		lerpf16_shuf += lerpf16_shuf_incr;

		// Interpolate via rounding_shift_right(neg_factor * (lo - hi), 15) + lo
		Vec128_S16 diff16_0 = diff16_subset0 ^ (diff16_subset1 & in_subset1_0);
		Vec128_S16 diff16_1 = diff16_subset0 ^ (diff16_subset1 & in_subset1_1);
		Vec128_S16 interp16_0 = vinterp16(lerpf16_0, diff16_0, lo16_subset0);
		Vec128_S16 interp16_1 = vinterp16(lerpf16_1, diff16_1, lo16_subset0);

		interp16_0 += lo16_subset1 & in_subset1_0;
		interp16_1 += lo16_subset1 & in_subset1_1;

		// Pack down to 8 bits and store
		Vec128_U8 interp8 = interp16_0.to_uint8_sat(interp16_1);
		interp8.storeu(out_rgba + i);

		// Advance to the next group of 4 pixels
		subset_mask = subset_mask.srl<8>();
	}
}

// Expects lo8 = (R0,R2,R4, G0,G2,G4, B0,B2,B4, A0,A2,A4, <ignored>)
// and     hi8 = (R1,R3,R5, G1,G3,G5, B1,B3,B5, A1,A3,A5, <ignored>)
static RADFORCEINLINE void simd_interpolate_three_subset(U8 * out_rgba, const BC67Partition * partition, const Vec128_U8 &lerpf8, const Vec128_U8 &lo8, const Vec128_U8 &hi8)
{
	// Shuffle for low/high lerp factors
	// replicates them 4x each and also expands to 16 bits by left-shifting by 8
	// -16 here because we keep adding to this value
	Vec128_U8 lerpf16_shuf = Vec128_S8(-16,0, -16,0, -16,0, -16,0, -16,1, -16,1, -16,1, -16,1).u8();
	const Vec128_U8 lerpf16_shuf_incr { 2 };

	// Subset mask for the pixels
	Vec128_U32 subset_mask { partition->subset_mask };
	const Vec128_U32 subset_ind_mask { 3<<0, 3<<2, 3<<4, 3<<6 };

	// Interpolate the pixel values!
	for (SINTa i = 0; i < 64; i += 16)
	{
		// Isolate next 4 subset indices
		Vec128_U32 subset_inds = subset_mask & subset_ind_mask;

		// Shift to place in second byte of every DWord
		Vec128_S16 subset_inds_scaled = subset_inds.s16().mullo(Vec128_S16(1<<8, 0, 1<<6, 0, 1<<4, 0, 1<<2, 0));

		// Determine endpoint shuffle; we have the subset index, which we now need to replicate and then add the offset for the channel
		Vec128_U8 endpoint_shuf = subset_inds_scaled.u8().shuf(Vec128_U8(1,1,1,1, 5,5,5,5, 9,9,9,9, 13,13,13,13));
		endpoint_shuf += Vec128_U32(0x09060300).u8();

		// Grab 4x pixels worth of RGBA lo8/hi8
		Vec128_U8 pix_lo8 = lo8.shuf(endpoint_shuf);
		Vec128_U8 pix_hi8 = hi8.shuf(endpoint_shuf);

		// Then expand to 16 bits
		Vec128_S16 lo16_0 = pix_lo8.to_sint16_lo();
		Vec128_S16 lo16_1 = pix_lo8.to_sint16_hi();
		Vec128_S16 hi16_0 = pix_hi8.to_sint16_lo();
		Vec128_S16 hi16_1 = pix_hi8.to_sint16_hi();

		// Set up for interpolation (lo-hi again)
		Vec128_S16 diff16_0 = lo16_0 - hi16_0;
		Vec128_S16 diff16_1 = lo16_1 - hi16_1;

		// Replicate the lerp factors 4x each and expand to 16 bits by shifting
		// left by 8
		Vec128_S16 lerpf16_0 = lerpf8.shuf(lerpf16_shuf).s16();
		lerpf16_shuf += lerpf16_shuf_incr;
		Vec128_S16 lerpf16_1 = lerpf8.shuf(lerpf16_shuf).s16();
		lerpf16_shuf += lerpf16_shuf_incr;

		// Interpolate via rounding_shift_right(neg_factor * (lo - hi), 15) + lo
		Vec128_S16 interp16_0 = vinterp16(lerpf16_0, diff16_0, lo16_0);
		Vec128_S16 interp16_1 = vinterp16(lerpf16_1, diff16_1, lo16_1);

		// Pack down to 8 bits and store
		Vec128_U8 interp8 = interp16_0.to_uint8_sat(interp16_1);
		interp8.storeu(out_rgba + i);

		// Advance to the next group of 4 pixels
		subset_mask = subset_mask.srl<8>();
	}
}

static void simd_decode_mode0(U8 * out_rgba, const U8 * block_bits, bool /* ignore_alpha */)
{
	//       [0]  mode (1b)
	//     [4:1]  partition (4b)
	//    [28:5]  R0..R5 (4b each)
	//   [52:29]  G0..G5 (4b each)
	//   [76:53]  B0..B5 (4b each)
	//   [82:77]  P0..P5 (1b each)
	//  [127:83]  index (45b)
	RR_ASSERT((block_bits[0] & 0x1) == 1); // mode 0

	Vec128_U8 block128 = Vec128_U8::loadu(block_bits);

	Vec128_U8 lo8, hi8;
	simd_decode_mode0_endpoints(&lo8, &hi8, block128, block_bits);

	// Decode partition type and indices
	const BC67Partition * partition = &bc67_partitions[1 + 64 + ((block_bits[0] >> 1) & 0xf)];

	Vec128_U8 lerpf8 = simd_decode_mode0_inds(block128, partition);

	// And interpolate
	// NOTE: alpha is always 255 anyway, ignore_alpha doesn't change anything
	simd_interpolate_three_subset(out_rgba, partition, lerpf8, lo8, hi8);
}

static void simd_decode_mode1(U8 * out_rgba, const U8 * block_bits, bool /* ignore_alpha */)
{
	//     [1:0]  mode (2b)
	//     [7:2]  partition (6b)
	//    [31:8]  R0..R3 (6b each)
	//   [55:32]  G0..G3 (6b each)
	//   [79:56]  B0..B3 (6b each)
	//   [81:80]  P0..P1 (1b each)
	//  [127:82]  index (46b)
	RR_ASSERT((block_bits[0] & 0x3) == 2); // mode 1

	Vec128_U8 block128 = Vec128_U8::loadu(block_bits);

	Vec128_S16 lo16, hi16;
	simd_decode_mode1_endpoints(&lo16, &hi16, block128, block_bits);

	// Decode partition type
	const BC67Partition * partition = &bc67_partitions[1 + (block_bits[0] >> 2)];

	Vec128_U8 lerpf8 = simd_decode_mode1_inds(block128, partition);

	// And interpolate!
	// NOTE: alpha is always 255 anyway, ignore_alpha doesn't change anything
	simd_interpolate_two_subset(out_rgba, partition, lerpf8, lo16, hi16);
}

static void simd_decode_mode2(U8 * out_rgba, const U8 * block_bits, bool /* ignore_alpha */)
{
	//     [2:0]  mode (3b)
	//     [8:3]  partition (6b)
	//    [38:9]  R0..R5 (5b each)
	//   [68:39]  G0..G5 (5b each)
	//   [98:69]  B0..B5 (5b each)
	//  [127:99]  index (29b)
	RR_ASSERT((block_bits[0] & 0x7) == 4); // mode 2

	Vec128_U8 block128 = Vec128_U8::loadu(block_bits);

	Vec128_U8 lo8, hi8;
	simd_decode_mode2_endpoints(&lo8, &hi8, block128, block_bits);

	// Decode partition type and indices
	U16 partition_ind = (RR_GET16_LE_UNALIGNED(block_bits) >> 3) & 0x3f;
	const BC67Partition * partition = &bc67_partitions[1 + 64 + partition_ind];

	Vec128_U8 lerpf8 = simd_decode_mode2_inds(block128, partition);

	// And interpolate!
	// NOTE: alpha is always 255 anyway, ignore_alpha doesn't change anything
	simd_interpolate_three_subset(out_rgba, partition, lerpf8, lo8, hi8);
}

static void simd_decode_mode3(U8 * out_rgba, const U8 * block_bits, bool ignore_alpha)
{
	//     [3:0]  mode (4b)
	//     [9:4]  partition (6b)
	//   [37:10]  R0..R3 (7b each)
	//   [65:38]  G0..G3 (7b each)
	//   [93:66]  B0..B3 (7b each)
	//   [97:94]  P0..P3 (1b each)
	//  [127:98]  index (30b)
	RR_ASSERT((block_bits[0] & 0xf) == 8); // mode 3

	Vec128_U8 block128 = Vec128_U8::loadu(block_bits);

	Vec128_S16 lo16, hi16;
	simd_decode_mode3_endpoints(&lo16, &hi16, block128, block_bits);

	// Decode partition type and indices
	const BC67Partition * partition = &bc67_partitions[1 + (block_bits[0] >> 4) + (block_bits[1] & 3) * 16];
	Vec128_U8 lerpf8 = simd_decode_mode3_inds(block128, partition);

	// And interpolate!
	// NOTE: alpha is always 255 anyway, ignore_alpha doesn't change anything
	simd_interpolate_two_subset(out_rgba, partition, lerpf8, lo16, hi16);
}

static void simd_decode_mode4(U8 * out_rgba, const U8 * block_bits, bool ignore_alpha)
{
	//     [4:0]  mode (5b)
	//     [6:5]  channel rot (2b)
	//     [7:7]  idxMode
	//    [17:8]  R0..R1 (5b each)
	//   [27:18]  G0..G1 (5b each)
	//   [37:28]  B0..B1 (5b each)
	//   [49:38]  A0..A1 (6b each)
	//   [80:50]  index0 (31b)
	//  [127:81]  index1 (47b)
	RR_ASSERT((block_bits[0] & 0x1f) == 0x10); // mode 4

	// First we need to unpack the endpoints to 8 bits each
	Vec128_U8 block128 = Vec128_U8::loadu(block_bits);

	RAD_ALIGN(U8, lerpf_buf[32], 16);
	simd_decode_mode4_inds(lerpf_buf, block128);

	Vec128_U16 endpoints16 = simd_decode_mode4_endpoints(block128);

	simd_interpolate_one_subset_twoind(out_rgba, block_bits[0] >> 5, lerpf_buf, endpoints16, ignore_alpha);
}

static void simd_decode_mode5(U8 * out_rgba, const U8 * block_bits, bool ignore_alpha)
{
	//     [5:0]  mode (6b)
	//     [7:6]  channel rot (2b)
	//    [21:8]  R0..R1 (7b each)
	//   [35:22]  G0..G1 (7b each)
	//   [49:36]  B0..B1 (7b each)
	//   [65:50]  A0..A1 (8b each)
	//   [96:66]  index0 (31b)
	//  [127:97]  index1 (31b)
	RR_ASSERT((block_bits[0] & 0x3f) == 0x20); // mode 5

	Vec128_U8 block128 = Vec128_U8::loadu(block_bits);

	RAD_ALIGN(U8, lerpf_buf[32], 16);
	simd_decode_mode5_inds(lerpf_buf, block128);

	Vec128_U16 endpoints16 = simd_decode_mode5_endpoints(block128);

	simd_interpolate_one_subset_twoind(out_rgba, block_bits[0] >> 6, lerpf_buf, endpoints16, ignore_alpha);
}

static void simd_decode_mode6(U8 * out_rgba, const U8 * block_bits, bool ignore_alpha)
{
	//     [6:0]  mode (7b)
	//    [20:7]  R0..R1 (7b each)
	//   [34:21]  G0..G1 (7b each)
	//   [48:35]  B0..B1 (7b each)
	//   [62:49]  A0..A1 (7b each)
	//   [64:63]  P0..P1 (1b each)
	//  [127:65]  index (63b)
	RR_ASSERT((block_bits[0] & 0x7f) == 0x40); // mode 6

	// First we need to unpack the endpoints to 8 bits each
	Vec128_U8 block128 = Vec128_U8::loadu(block_bits + 0);

	// Decode the endpoints
	Vec128_U8 endpoints16 = simd_decode_mode6_endpoints(block128).u8();

	// Shuffle the endpoints around so we have the "low" and "high" end separated out
	Vec128_S16 lo16 = endpoints16.unpack_lo64(endpoints16).s16();
	Vec128_S16 hi16 = endpoints16.unpack_hi64(endpoints16).s16();

	simd_alpha_override_endpoints16(&lo16, &hi16, ignore_alpha);

	// Set up for interpolation
	Vec128_S16 diff16 = lo16 - hi16; // yes, lo-hi!
	Vec128_U8 lerpf8 = simd_decode_mode6_inds(block128);

	// Shuffle for low/high lerp factors
	// replicates them 4x each and also expands to 16 bits by left-shifting by 8
	// -16 here because we keep adding to this value
	Vec128_U8 lerpf16_shuf0 = Vec128_S8(-16,0, -16,0, -16,0, -16,0, -16,1, -16,1, -16,1, -16,1).u8();
	Vec128_U8 lerpf16_shuf1 = Vec128_S8(-16,2, -16,2, -16,2, -16,2, -16,3, -16,3, -16,3, -16,3).u8();
	const Vec128_U8 lerpf16_shuf_incr { 4 };

	// Interpolate the pixel values!
	for (SINTa i = 0; i < 64; i += 16)
	{
		// Replicate the lerp factors 4x each and expand to 16 bits by shifting
		// left by 8
		Vec128_S16 lerpf16_0 = lerpf8.shuf(lerpf16_shuf0).s16();
		Vec128_S16 lerpf16_1 = lerpf8.shuf(lerpf16_shuf1).s16();

		// Interpolate via rounding_shift_right(neg_factor * (lo - hi), 15) + lo
		Vec128_S16 interp16_0 = vinterp16(lerpf16_0, diff16, lo16);
		Vec128_S16 interp16_1 = vinterp16(lerpf16_1, diff16, lo16);

		// Pack down to 8 bits and store
		Vec128_U8 interp8 = interp16_0.to_uint8_sat(interp16_1);
		interp8.storeu(out_rgba + i);

		// Advance to the next group of 4 lerp factors
		lerpf16_shuf0 += lerpf16_shuf_incr;
		lerpf16_shuf1 += lerpf16_shuf_incr;
	}
}

static void simd_decode_mode7(U8 * out_rgba, const U8 * block_bits, bool ignore_alpha)
{
	//     [7:0]  mode (8b)
	//    [13:8]  partition (6b)
	//   [33:14]  R0..R3 (5b each)
	//   [53:34]  G0..G3 (5b each)
	//   [73:54]  B0..B3 (5b each)
	//   [93:74]  A0..A3 (5b each)
	//   [97:94]  P0..P3 (1b each)
	//  [127:98]  index (30b)
	RR_ASSERT(block_bits[0] == 0x80); // mode 7

	Vec128_U8 block128 = Vec128_U8::loadu(block_bits);

	Vec128_S16 lo16, hi16;
	simd_decode_mode7_endpoints(&lo16, &hi16, block128, block_bits);

	simd_alpha_override_endpoints16(&lo16, &hi16, ignore_alpha);

	// Decode partition type and indices
	const BC67Partition * partition = &bc67_partitions[1 + (block_bits[1] & 63)];
	Vec128_U8 lerpf8 = simd_decode_mode7_inds(block128, partition);

	// And interpolate!
	simd_interpolate_two_subset(out_rgba, partition, lerpf8, lo16, hi16);
}

#endif

static void bc7_decode_and_handle_alpha(U8 * out_rgba, const void * block, bool ignore_alpha)
{
	bc7_decode_block(out_rgba, 16, block);

	if ( ignore_alpha )
	{
		for (int i = 0; i < 16; ++i)
			out_rgba[i*4 + 3] = 255;
	}
}

static void bc7_decode_verify(const U8 * decoded, const void * block, bool ignore_alpha)
{
	U8 ref_result[64];
	bc7_decode_and_handle_alpha(ref_result, block, ignore_alpha);
	RR_ASSERT_ALWAYS(memcmp(decoded, ref_result, 64) == 0);
}

static void simd_decode_malformed(U8 * out_rgba, const U8 * block_bits, bool ignore_alpha)
{
	memset(out_rgba, 0, 64);

	if ( ignore_alpha )
	{
		for (int i = 0; i < 16; ++i)
			out_rgba[i*4 + 3] = 255;
	}
}

#if 0 // toggle to verify fast decoders against known-good ref decoder
#define VERIFY(decoded,block,ignore_alpha) bc7_decode_verify(decoded,block,ignore_alpha)
#else
#define VERIFY(decoded,block,ignore_alpha)
#endif

typedef void BC7BlockDecoder(U8 * out_rgba, const U8 * block_bits, bool ignore_alpha);

// The main dispatcher
void bc7_decode_block_fast(U8 * out_rgba, const void * block, bool ignore_alpha)
{
	const U8 * block_bytes = (const U8 *)block;

#ifdef DO_BUILD_SSE4
	U32 mode = rrCtz32(block_bytes[0] | 0x100); // gives 8 when byte=zero

#ifdef DO_BUILD_AVX2
	if ( rrCPUx86_feature_present(RRX86_CPU_AVX2) )
	{
		static BC7BlockDecoder * const decoders_avx2[9] =
		{
			simd_decode_mode0,
			bc7decode::avx2_decode_mode1,
			simd_decode_mode2,
			bc7decode::avx2_decode_mode3,
			bc7decode::avx2_decode_mode4,
			bc7decode::avx2_decode_mode5,
			bc7decode::avx2_decode_mode6,
			bc7decode::avx2_decode_mode7,
			simd_decode_malformed
		};

		decoders_avx2[mode](out_rgba, block_bytes, ignore_alpha);
		VERIFY(out_rgba, block, ignore_alpha);

		return;
	}
#endif

	static BC7BlockDecoder * const decoders_sse4[9] =
	{
		simd_decode_mode0,
		simd_decode_mode1,
		simd_decode_mode2,
		simd_decode_mode3,
		simd_decode_mode4,
		simd_decode_mode5,
		simd_decode_mode6,
		simd_decode_mode7,
		simd_decode_malformed
	};

	decoders_sse4[mode](out_rgba, block_bytes, ignore_alpha);
	VERIFY(out_rgba, block, ignore_alpha);

#elif defined(DO_BUILD_NEON64)

	U32 mode = rrCtz32(block_bytes[0] | 0x100); // gives 8 when byte=zero

	static BC7BlockDecoder * const decoders_neon[9] =
	{
		simd_decode_mode0,
		simd_decode_mode1,
		simd_decode_mode2,
		simd_decode_mode3,
		simd_decode_mode4,
		simd_decode_mode5,
		simd_decode_mode6,
		simd_decode_mode7,
		simd_decode_malformed
	};

	decoders_neon[mode](out_rgba, block_bytes, ignore_alpha);
	VERIFY(out_rgba, block, ignore_alpha);

#else // no SEE4, no NEON64
	// If all else fails, we can always use the reference decoder.
	// (Never wrong, but generally much slower.)
	bc7_decode_and_handle_alpha(out_rgba, block_bytes, ignore_alpha);
#endif
}

// ---- Block error eval

static U32 endpoints_eval_scalar_basic(const BC7PredecodedEndpointsBlock * block, SINTa inds_from)
{
	bc7bits endpts_bits = bc7bits_load(block->endpt_block);
	bc7bits inds_bits = bc7bits_and(bc7bits_load(block->index_cache + inds_from * 16), bc7bits_load(block->index_mask));
	bc7bits block_bits = bc7bits_or_assert_exclusive(endpts_bits, inds_bits);

	U8 out_rgba[64];
	bc7_decode_block_fast(out_rgba, bc7bits_U8ptr(&block_bits), block->ignore_alpha);

	U32 total_err = 0;
	for LOOP(i,64)
	{
		int diff = out_rgba[i] - block->base[i];
		total_err += diff * diff;
	}

	return total_err;
}

static SINTa find_next_at_most_scalar_basic(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh)
{
	for (SINTa i = 0; i < count; ++i)
	{
		U32 ssd = endpoints_eval_scalar_basic(b, inds_from + i);
		if (ssd <= err_thresh)
		{
			*out_err = ssd;
			return i;
		}
	}

	return count;
}

#if defined(DO_BUILD_SSE4) || defined(DO_BUILD_NEON64)

static U32 endpoints_eval_simd_basic(const BC7PredecodedEndpointsBlock * block, SINTa inds_from)
{
	bc7bits endpts_bits = bc7bits_load(block->endpt_block);
	bc7bits inds_bits = bc7bits_and(bc7bits_load(block->index_cache + inds_from * 16), bc7bits_load(block->index_mask));
	bc7bits block_bits = bc7bits_or_assert_exclusive(endpts_bits, inds_bits);

	U8 out_rgba[64];
	bc7_decode_block_fast(out_rgba, bc7bits_U8ptr(&block_bits), block->ignore_alpha);

	Vec128_S32 total_err = Vec128_S32::zero();
	for (SINTa i = 0; i < 64; i += 16)
	{
		// Load decoded bytes
		Vec128_U8 rgba_u8 = Vec128_U8::loadu(out_rgba + i);

		// Expand to 16 bits
		Vec128_S16 rgba_u16_0 = rgba_u8.to_sint16_lo();
		Vec128_S16 rgba_u16_1 = rgba_u8.to_sint16_hi();

		// Diff against the already-expanded reference pixels in "base"
		Vec128_S16 diff_s16_0 = rgba_u16_0 - Vec128_S16::loadu(block->base + i);
		Vec128_S16 diff_s16_1 = rgba_u16_1 - Vec128_S16::loadu(block->base + i + 8);

#if defined(DO_BUILD_SSE4)
		// First half of dot product: R*R + G*G, B*B + A*A
		// the rest we do as part of the reduction
		Vec128_S32 err0 = diff_s16_0.madd(diff_s16_0);
		Vec128_S32 err1 = diff_s16_1.madd(diff_s16_1);

		// Accumulate
		Vec128_S32 errs = err0 + err1;
		total_err += errs;
#else // defined(DO_BUILD_NEON64)
		total_err = vmlal_s16(total_err, vget_low_s16(diff_s16_0), vget_low_s16(diff_s16_0));
		total_err = vmlal_s16(total_err, vget_high_s16(diff_s16_0), vget_high_s16(diff_s16_0));
		total_err = vmlal_s16(total_err, vget_low_s16(diff_s16_1), vget_low_s16(diff_s16_1));
		total_err = vmlal_s16(total_err, vget_high_s16(diff_s16_1), vget_high_s16(diff_s16_1));
#endif
	}

	// Finish the reduction
	return reduce_add(total_err);
}

static SINTa find_next_at_most_simd_basic(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh)
{
	for (SINTa i = 0; i < count; ++i)
	{
		U32 ssd = endpoints_eval_scalar_basic(b, inds_from + i);
		if (ssd <= err_thresh)
		{
			*out_err = ssd;
			return i;
		}
	}

	return count;
}

static RADFORCEINLINE U32 endpoints_eval_finish_simd_oneindex(const BC7PredecodedEndpointsBlock * b, const Vec128_U8 &lerpf8)
{
	// Shuffle for low/high lerp factors
	// replicates them 4x each and also expands to 16 bits by left-shifting by 8
	static RAD_ALIGN(const S8, lerpf16_shuffles[128], 16) =
	{
#define FOUR(x) -1,(x),-1,(x),-1,(x),-1,(x)
		FOUR( 0), FOUR( 1), FOUR( 2), FOUR( 3),
		FOUR( 4), FOUR( 5), FOUR( 6), FOUR( 7),
		FOUR( 8), FOUR( 9), FOUR(10), FOUR(11),
		FOUR(12), FOUR(13), FOUR(14), FOUR(15),
#undef FOUR
	};

	Vec128_S32 total_err = Vec128_S32::zero();
	for (SINTa i = 0; i < 64; i += 16)
	{
		// Replicate the lerp factors 4x each and expand to 16 bits by shifting left by 8
		Vec128_S16 lerpf16_0 = lerpf8.shuf(Vec128_U8::loada(lerpf16_shuffles + i*2)).s16();
		Vec128_S16 lerpf16_1 = lerpf8.shuf(Vec128_U8::loada(lerpf16_shuffles + i*2 + 16)).s16();

		// Interpolate via rounding_shift_right(neg_factor * (lo - hi), 15) + lo
		// also computes diff to original block values which are subtracted from base
		Vec128_S16 interp16_0 = vinterp16(lerpf16_0, Vec128_S16::loada(b->diff + i + 0), Vec128_S16::loada(b->base + i + 0));
		Vec128_S16 interp16_1 = vinterp16(lerpf16_1, Vec128_S16::loada(b->diff + i + 8), Vec128_S16::loada(b->base + i + 8));

#if defined(DO_BUILD_SSE4)
		// First half of dot product: R*R + G*G, B*B + A*A
		// the rest we do as part of the reduction
		Vec128_S32 err_0 = interp16_0.madd(interp16_0);
		Vec128_S32 err_1 = interp16_1.madd(interp16_1);
		Vec128_S32 err_sum = err_0 + err_1;
		total_err += err_sum;
#else // defined(DO_BUILD_NEON64)
		total_err = vmlal_s16(total_err, vget_low_s16(interp16_0), vget_low_s16(interp16_0));
		total_err = vmlal_s16(total_err, vget_high_s16(interp16_0), vget_high_s16(interp16_0));
		total_err = vmlal_s16(total_err, vget_low_s16(interp16_1), vget_low_s16(interp16_1));
		total_err = vmlal_s16(total_err, vget_high_s16(interp16_1), vget_high_s16(interp16_1));
#endif
	}

	// Finish the reduction
	return reduce_add(total_err);
}

static RADFORCEINLINE U32 endpoints_eval_finish_simd_twoindex(const BC7PredecodedEndpointsBlock * b, const U8 * lerpf_buf, bool index_swap)
{
	// Shuffle the interleaved lerp factors to replicate them in the 3-1 scalar-vector pattern
	// we always work with values in the pre-color-rotate RGB=vector, A=scalar space
	static RAD_ALIGN(const S8, lerpf16_shuffles[2][32], 16) = // [index_swap][i]
	{
#define INDS(x,y) -1,(x),-1,(x),-1,(x),-1,(y)
		{ // index_swap=false
			INDS(0,1), INDS(2,3), INDS(4,5), INDS(6,7),
		},
		{ // index_swap=true
			INDS(1,0), INDS(3,2), INDS(5,4), INDS(7,6),
		},
#undef INDS
	};

	// Shuffle the interleaved lerp factors to replicate them in the 3-1 scalar-vector pattern
	// we always work with values in the pre-color-rotate RGB=vector, A=scalar space
	const Vec128_U8 lerpf16_shuf_0 = Vec128_U8::loada(lerpf16_shuffles[index_swap] + 0);
	const Vec128_U8 lerpf16_shuf_1 = Vec128_U8::loada(lerpf16_shuffles[index_swap] + 16);

	Vec128_S32 total_err = Vec128_S32::zero();
	for (SINTa i = 0; i < 16; i += 4)
	{
		Vec128_U8 lerpf8 = Vec128_U8::loadu_lo64(lerpf_buf + i*2);
		Vec128_S16 lerpf16_0 = lerpf8.shuf(lerpf16_shuf_0).s16();
		Vec128_S16 lerpf16_1 = lerpf8.shuf(lerpf16_shuf_1).s16();

		// Interpolate via rounding_shift_right(neg_factor * (lo - hi), 15) + lo
		// also computes diff to original block values which are subtracted from base
		Vec128_S16 interp16_0 = vinterp16(lerpf16_0, Vec128_S16::loada(b->diff + i*4 + 0), Vec128_S16::loada(b->base + i*4 + 0));
		Vec128_S16 interp16_1 = vinterp16(lerpf16_1, Vec128_S16::loada(b->diff + i*4 + 8), Vec128_S16::loada(b->base + i*4 + 8));

#if defined(DO_BUILD_SSE4)
		// First half of dot product: R*R + G*G, B*B + A*A
		// the rest we do as part of the reduction
		Vec128_S32 err_0 = interp16_0.madd(interp16_0);
		Vec128_S32 err_1 = interp16_1.madd(interp16_1);
		Vec128_S32 err_sum = err_0 + err_1;
		total_err += err_sum;
#else // defined(DO_BUILD_NEON64)
		total_err = vmlal_s16(total_err, vget_low_s16(interp16_0), vget_low_s16(interp16_0));
		total_err = vmlal_s16(total_err, vget_high_s16(interp16_0), vget_high_s16(interp16_0));
		total_err = vmlal_s16(total_err, vget_low_s16(interp16_1), vget_low_s16(interp16_1));
		total_err = vmlal_s16(total_err, vget_high_s16(interp16_1), vget_high_s16(interp16_1));
#endif
	}

	// Finish the reduction
	return reduce_add(total_err);
}

static U32 endpoints_eval_simd_onesubset_twoind(const BC7PredecodedEndpointsBlock * b, SINTa inds_from)
{
	return endpoints_eval_finish_simd_twoindex(b, b->index_cache + inds_from * 32, b->index_swap != 0);
}

static SINTa find_next_at_most_simd_onesubset_twoind(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh)
{
	bool index_swap = b->index_swap != 0;

	for (SINTa i = 0; i < count; ++i)
	{
		U32 ssd = endpoints_eval_finish_simd_twoindex(b, b->index_cache + (inds_from + i) * 32, index_swap);
		if (ssd <= err_thresh)
		{
			*out_err = ssd;
			return i;
		}
	}

	return count;
}

static void preendpoint_init_simd_twoind(BC7PredecodedEndpointsBlock * b, const U8 tgt_pixels_rgba[8], const Vec128_U16 &endpoints16, U32 crot, bool ignore_alpha_chan)
{
	// Set up decode kernel pointers
#ifdef DO_BUILD_AVX2
	if (rrCPUx86_feature_present(RRX86_CPU_AVX2))
	{
		b->eval = bc7decode::endpoints_eval_avx2_onesubset_twoind;
		b->find_next_at_most = bc7decode::find_next_at_most_avx2_onesubset_twoind;
	}
	else
#endif
	{
		b->eval = endpoints_eval_simd_onesubset_twoind;
		b->find_next_at_most = find_next_at_most_simd_onesubset_twoind;
	}

	// For the modes with two indices, we stay fully in the space that BC7 decodes into,
	// where the vector channels are always RGB and the scalar channel is always A.
	//
	// Instead of having the decoder apply the final channel swap at the end, we apply
	// the inverse swap to the "base" values we diff against - comes out to the same
	// result, but we never have to actually swap anything in the decoder.

	// The channel "rotates" are actually always swaps, so idempotent:
	// the swap and its inverse are the same.
	Vec128_U8 crot_shuf = simd_channel_rotate16(crot);
	Vec128_U16 endpoints = endpoints16;

	// In "ignore alpha" mode, make sure we decode the channel that we would
	// ordinarily swap into A to all-255
	if (ignore_alpha_chan)
	{
		const Vec128_U16 base_alpha_all_0xff { 0,0,0,0xff, 0,0,0,0xff };
		Vec128_U16 alpha_all_0xff = base_alpha_all_0xff.u8().shuf(crot_shuf).u16();

		endpoints |= alpha_all_0xff;
	}

	// Shuffle the endpoints around so we have the "low" and "high" end separated out
	Vec128_S16 lo16 = endpoints.u8().unpack_lo64(endpoints.u8()).s16();
	Vec128_S16 hi16 = endpoints.u8().unpack_hi64(endpoints.u8()).s16();
	Vec128_S16 diff16 = lo16 - hi16; // yes, lo-hi!

	// Shuffles to expand pixel values to 16 bits without channel rotate
	const Vec128_U8 base_pix16_lo_shuf = Vec128_S8(0,-1, 1,-1, 2,-1, 3,-1, 4,-1, 5,-1, 6,-1, 7,-1).u8();
	const Vec128_U8 base_pix16_hi_shuf = Vec128_S8(8,-1, 9,-1, 10,-1, 11,-1, 12,-1, 13,-1, 14,-1, 15,-1).u8();

	// Apply channel rotate to them to get our actual shuffles
	Vec128_U8 pix16_lo_shuf = base_pix16_lo_shuf.shuf(crot_shuf);
	Vec128_U8 pix16_hi_shuf = base_pix16_hi_shuf.shuf(crot_shuf);

	for (SINTa i = 0; i < 64; i += 16)
	{
		Vec128_U8 pix8 = Vec128_U8::loadu(tgt_pixels_rgba + i);
		Vec128_S16 pix16_0 = pix8.shuf(pix16_lo_shuf).s16();
		Vec128_S16 pix16_1 = pix8.shuf(pix16_hi_shuf).s16();

		(lo16 - pix16_0).storeu(b->base + i + 0);
		(lo16 - pix16_1).storeu(b->base + i + 8);
		diff16.storeu(b->diff + i + 0);
		diff16.storeu(b->diff + i + 8);
	}
}

static U32 endpoints_eval_simd_twosubset(const BC7PredecodedEndpointsBlock * b, SINTa inds_from)
{
	Vec128_U8 lerpf8 = Vec128_U8::loadu(b->index_cache + inds_from*16 + b->partition_eqv * (b->index_cache_size >> 2));
	return endpoints_eval_finish_simd_oneindex(b, lerpf8);
}

static SINTa find_next_at_most_simd_twosubset(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh)
{
	const U8 * index_cache = b->index_cache + b->partition_eqv * (b->index_cache_size >> 2);
	for (SINTa i = 0; i < count; ++i)
	{
		Vec128_U8 lerpf8 = Vec128_U8::loadu(index_cache + (inds_from + i) * 16);
		U32 ssd = endpoints_eval_finish_simd_oneindex(b, lerpf8);
		if (ssd <= err_thresh)
		{
			*out_err = ssd;
			return i;
		}
	}

	return count;
}

static void preendpoint_init_simd_two_subset(BC7PredecodedEndpointsBlock * b, const U8 tgt_pixels_rgba[8], U8 part_id, const Vec128_S16 &both_lo16, const Vec128_S16 &both_hi16)
{
	// Set up partition info and evaluators
	const BC67Partition * part = &bc67_partitions[1 + part_id];

	b->partition_id = part_id;
	b->partition_eqv = radtex_anchor_eqv[1][part_id];
#ifdef DO_BUILD_AVX2
	if (rrCPUx86_feature_present(RRX86_CPU_AVX2))
	{
		b->eval = bc7decode::endpoints_eval_avx2_twosubset;
		b->find_next_at_most = bc7decode::find_next_at_most_avx2_twosubset;
	}
	else
#endif
	{
		b->eval = endpoints_eval_simd_twosubset;
		b->find_next_at_most = find_next_at_most_simd_twosubset;
	}

	Vec128_S16 both_diff16 = both_lo16 - both_hi16; // yes, lo-hi!

	// lo16/diff16 diffs between the subsets
	Vec128_S32 lo16_subset0 = both_lo16.s32().xyxy();
	Vec128_S32 lo16_subset1 = both_lo16.s32().zwzw();
	Vec128_S32 diff16_subset0 = both_diff16.s32().xyxy();
	Vec128_S32 diff16_subset1 = both_diff16.s32().zwzw();

	// Subset mask for the pixels
#if defined(DO_BUILD_SSE4)
	Vec128_S32 subset_mask { _mm_cvtsi32_si128(part->subset_mask) };
	subset_mask = Vec128_S32(_mm_unpacklo_epi64(subset_mask, subset_mask));
#else // defined(DO_BUILD_NEON64)
	Vec128_S32 subset_mask { vdupq_n_u64(part->subset_mask) };
#endif

	const Vec128_S32 in_subset1_mask { 1<<0, 0, 1<<2, 0 };

	for (SINTa i = 0; i < 64; i += 8)
	{
		// Determine 64-bit masks for whether a pixel is in subset 1
		Vec128_S32 in_subset1 = (subset_mask & in_subset1_mask).cmp_eq64(in_subset1_mask);

		// Select the right lo and diff values for the subset
		Vec128_S16 lo16 = in_subset1.select(lo16_subset1, lo16_subset0).s16();
		Vec128_S16 diff16 = in_subset1.select(diff16_subset1, diff16_subset0).s16();

		// Load the source pixels and convert to 16 bits
		Vec128_U8 pix8 = Vec128_U8::loadu_lo64(tgt_pixels_rgba + i);
		Vec128_S16 pix16 = pix8.to_sint16_lo();

		// Write out the base and diff values
		(lo16 - pix16).storeu(b->base + i);
		diff16.storeu(b->diff + i);

		// Advance to the next group of 2 pixels
		subset_mask = subset_mask.srl<4>();
	}
}

// Expects lo8 = (R0,R2,R4, G0,G2,G4, B0,B2,B4, A0,A2,A4, <ignored>)
// and     hi8 = (R1,R3,R5, G1,G3,G5, B1,B3,B5, A1,A3,A5, <ignored>)
static void preendpoint_init_simd_three_subset(BC7PredecodedEndpointsBlock * b, const U8 tgt_pixels_rgba[8], const BC67Partition * part, const Vec128_U8 &lo8, const Vec128_U8 &hi8)
{
	// Subset mask for the pixels
	Vec128_S32 subset_mask { (S32)part->subset_mask };
	const Vec128_S32 subset_ind_mask { 3<<0, 3<<2, 0, 0 };

	for (SINTa i = 0; i < 64; i += 8)
	{
		// Isolate next 2 subset indices
		Vec128_S16 subset_inds = (subset_mask & subset_ind_mask).s16();

		// Shift to place in second byte of every DWord
		Vec128_S16 subset_inds_scaled = subset_inds.mullo( Vec128_S32(1<<8, 1<<6, 0, 0).s16() );

		// Determine endpoint shuffle; we have the subset index, which we now need to replicate and then add the offset for the channel
		// also set all the odd pixels (=high halves of 16-bit values) to 0
		Vec128_U8 endpoint_shuf = subset_inds_scaled.u8().shuf(Vec128_S8(1,-1, 1,-1, 1,-1, 1,-1,  5,-1, 5,-1, 5,-1, 5,-1));
		endpoint_shuf += Vec128_S8(0,-1, 3,-1, 6,-1, 9,-1, 0,-1, 3,-1, 6,-1, 9,-1).u8();

		// Extract 2 pixels worth of lo16 / hi16
		Vec128_S16 lo16 = lo8.shuf(endpoint_shuf).s16();
		Vec128_S16 hi16 = hi8.shuf(endpoint_shuf).s16();

		Vec128_S16 diff16 = lo16 - hi16;

		// Load the source pixels and convert to 16 bits
		Vec128_U8 pix8 = Vec128_U8::loadu_lo64(tgt_pixels_rgba + i);
		Vec128_S16 pix16 = pix8.to_sint16_lo();

		// Write out the bae and diff values
		(lo16 - pix16).storeu(b->base + i);
		diff16.storeu(b->diff + i);

		// Advance to the next group of 2 pixels
		subset_mask = subset_mask.srl<4>();
	}
}

static U32 endpoints_eval_simd_mode0(const BC7PredecodedEndpointsBlock * b, SINTa inds_from)
{
	// Get indices from inds_from, everything else is already set up
	const BC67Partition * partition = &bc67_partitions[1 + 64 + b->partition_id];
	Vec128_U8 lerpf8 = simd_decode_mode0_inds(Vec128_U8::loadu(b->index_cache + inds_from * 16), partition);
	return endpoints_eval_finish_simd_oneindex(b, lerpf8);
}

static SINTa find_next_at_most_simd_mode0(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh)
{
	const BC67Partition * partition = &bc67_partitions[1 + 64 + b->partition_id];
	for (SINTa i = 0; i < count; ++i)
	{
		Vec128_U8 lerpf8 = simd_decode_mode0_inds(Vec128_U8::loadu(b->index_cache + (inds_from + i) * 16), partition);
		U32 ssd = endpoints_eval_finish_simd_oneindex(b, lerpf8);
		if (ssd <= err_thresh)
		{
			*out_err = ssd;
			return i;
		}
	}

	return count;
}

static void preendpoint_init_simd_mode0(BC7PredecodedEndpointsBlock * b, const U8 tgt_pixels_rgba[8], const U8 * endpts_from)
{
	// Decode the endpoints
	Vec128_U8 lo8, hi8;
	simd_decode_mode0_endpoints(&lo8, &hi8, Vec128_U8::loadu(endpts_from), endpts_from);

	// Set our evaluator
	b->partition_id = (endpts_from[0] >> 1) & 0xf;
	b->eval = endpoints_eval_simd_mode0;
	b->find_next_at_most = find_next_at_most_simd_mode0;

	// Set up for two-subset interpolation, standard way
	const BC67Partition * partition = &bc67_partitions[1 + 64 + b->partition_id];
	preendpoint_init_simd_three_subset(b, tgt_pixels_rgba, partition, lo8, hi8);
}

static void preendpoint_init_simd_mode1(BC7PredecodedEndpointsBlock * b, const U8 tgt_pixels_rgba[8], const U8 * endpts_from)
{
	// Decode the endpoints and initialize "base" and "diff"
	Vec128_S16 lo16, hi16;
	simd_decode_mode1_endpoints(&lo16, &hi16, Vec128_U8::loadu(endpts_from), endpts_from);

	preendpoint_init_simd_two_subset(b, tgt_pixels_rgba, endpts_from[0] >> 2, lo16, hi16);
}

static U32 endpoints_eval_simd_mode2(const BC7PredecodedEndpointsBlock * b, SINTa inds_from)
{
	// Get indices from inds_from, everything else is already set up
	const BC67Partition * partition = &bc67_partitions[1 + 64 + b->partition_id];
	Vec128_U8 lerpf8 = simd_decode_mode2_inds(Vec128_U8::loadu(b->index_cache + inds_from * 16), partition);
	return endpoints_eval_finish_simd_oneindex(b, lerpf8);
}

static SINTa find_next_at_most_simd_mode2(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh)
{
	const BC67Partition * partition = &bc67_partitions[1 + 64 + b->partition_id];
	for (SINTa i = 0; i < count; ++i)
	{
		Vec128_U8 lerpf8 = simd_decode_mode2_inds(Vec128_U8::loadu(b->index_cache + (inds_from + i) * 16), partition);
		U32 ssd = endpoints_eval_finish_simd_oneindex(b, lerpf8);
		if (ssd <= err_thresh)
		{
			*out_err = ssd;
			return i;
		}
	}

	return count;
}

static void preendpoint_init_simd_mode2(BC7PredecodedEndpointsBlock * b, const U8 tgt_pixels_rgba[8], const U8 * endpts_from)
{
	// Decode the endpoints
	Vec128_U8 lo8, hi8;
	simd_decode_mode2_endpoints(&lo8, &hi8, Vec128_U8::loadu(endpts_from), endpts_from);

	// Set our evaluator
	b->partition_id = (RR_GET16_LE_UNALIGNED(endpts_from) >> 3) & 0x3f;
	b->eval = endpoints_eval_simd_mode2;
	b->find_next_at_most = find_next_at_most_simd_mode2;

	// Set up for two-subset interpolation, standard way
	const BC67Partition * partition = &bc67_partitions[1 + 64 + b->partition_id];
	preendpoint_init_simd_three_subset(b, tgt_pixels_rgba, partition, lo8, hi8);
}

// mode 3 shares eval/find_next_at_most funcs with mode 1 because the endpoint
// and index encoding differences are flattened out come eval time

static void preendpoint_init_simd_mode3(BC7PredecodedEndpointsBlock * b, const U8 tgt_pixels_rgba[8], const U8 * endpts_from)
{
	Vec128_S16 lo16, hi16;
	simd_decode_mode3_endpoints(&lo16, &hi16, Vec128_U8::loadu(endpts_from), endpts_from);

	U8 part_id = (endpts_from[0] >> 4) + (endpts_from[1] & 3) * 16;
	preendpoint_init_simd_two_subset(b, tgt_pixels_rgba, part_id, lo16, hi16);
}

static void preendpoint_init_simd_mode4(BC7PredecodedEndpointsBlock * b, const U8 tgt_pixels_rgba[8], const U8 * endpts_from, bool ignore_alpha_chan)
{
	Vec128_U16 endpoints16 = simd_decode_mode4_endpoints(Vec128_U8::loadu(endpts_from));

	b->index_swap = (endpts_from[0] & 0x80) != 0;
	preendpoint_init_simd_twoind(b, tgt_pixels_rgba, endpoints16, (endpts_from[0] >> 5) & 3, ignore_alpha_chan);
}

// mode 5 has different index and endpoint encoding but all that's resolved by eval time;
// actual eval/find_next_at_most funcs are shared with mode 4

static void preendpoint_init_simd_mode5(BC7PredecodedEndpointsBlock * b, const U8 tgt_pixels_rgba[8], const U8 * endpts_from, bool ignore_alpha_chan)
{
	Vec128_U16 endpoints16 = simd_decode_mode5_endpoints(Vec128_U8::loadu(endpts_from));

	b->index_swap = 0;
	preendpoint_init_simd_twoind(b, tgt_pixels_rgba, endpoints16, endpts_from[0] >> 6, ignore_alpha_chan);
}

static U32 endpoints_eval_simd_mode6(const BC7PredecodedEndpointsBlock * b, SINTa inds_from)
{
	// Get indices from inds_from, everything else is already set up
	Vec128_U8 lerpf8 = Vec128_U8::loadu(b->index_cache + inds_from * 16);
	return endpoints_eval_finish_simd_oneindex(b, lerpf8);
}

static SINTa find_next_at_most_simd_mode6(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh)
{
	for (SINTa i = 0; i < count; ++i)
	{
		Vec128_U8 lerpf8 = Vec128_U8::loadu(b->index_cache + (inds_from + i) * 16);
		U32 ssd = endpoints_eval_finish_simd_oneindex(b, lerpf8);
		if (ssd <= err_thresh)
		{
			*out_err = ssd;
			return i;
		}
	}

	return count;
}

static void preendpoint_init_simd_mode6(BC7PredecodedEndpointsBlock * b, const U8 tgt_pixels_rgba[8], const U8 * endpts_from, bool ignore_alpha_chan)
{
	Vec128_U16 endpoints = simd_decode_mode6_endpoints(Vec128_U8::loadu(endpts_from));

	// In "ignore alpha" mode, set things up so we always decode alpha to 0xff no matter what
	if (ignore_alpha_chan)
		endpoints |= Vec128_U16(0,0,0,0xff, 0,0,0,0xff);

	// Shuffle the endpoints around so we have the "low" and "high" end separated out
	Vec128_S16 lo16 = endpoints.u8().unpack_lo64(endpoints.u8()).s16();
	Vec128_S16 hi16 = endpoints.u8().unpack_hi64(endpoints.u8()).s16();
	Vec128_S16 diff16 = lo16 - hi16; // yes, lo-hi!

	for (SINTa i = 0; i < 64; i += 16)
	{
		Vec128_U8 pix8 = Vec128_U8::loadu(tgt_pixels_rgba + i);
		Vec128_S16 pix16_0 = pix8.to_sint16_lo();
		Vec128_S16 pix16_1 = pix8.to_sint16_hi();

		(lo16 - pix16_0).storeu(b->base + i + 0);
		(lo16 - pix16_1).storeu(b->base + i + 8);
		diff16.storeu(b->diff + i + 0);
		diff16.storeu(b->diff + i + 8);
	}

	// Set our evaluator
#ifdef DO_BUILD_AVX2
	if (rrCPUx86_feature_present(RRX86_CPU_AVX2))
	{
		b->eval = bc7decode::endpoints_eval_avx2_mode6;
		b->find_next_at_most = bc7decode::find_next_at_most_avx2_mode6;
	}
	else
#endif
	{
		b->eval = endpoints_eval_simd_mode6;
		b->find_next_at_most = find_next_at_most_simd_mode6;
	}
}

// mode 7 shares eval/find_next_at_most funcs with mode 1 because the endpoint
// and index encoding differences are flattened out come eval time

static void preendpoint_init_simd_mode7(BC7PredecodedEndpointsBlock * b, const U8 tgt_pixels_rgba[8], const U8 * endpts_from, bool ignore_alpha_chan)
{
	// Decode the endpoints and initialize "base" and "diff"
	Vec128_S16 lo16, hi16;
	simd_decode_mode7_endpoints(&lo16, &hi16, Vec128_U8::loadu(endpts_from), endpts_from);

	// In "ignore alpha" mode, set things up so we always decode alpha to 0xff
	if (ignore_alpha_chan)
	{
		const Vec128_S16 alpha_all_0xff { 0,0,0,0xff, 0,0,0,0xff };
		lo16 |= alpha_all_0xff;
		hi16 |= alpha_all_0xff;
	}

	preendpoint_init_simd_two_subset(b, tgt_pixels_rgba, endpts_from[1] & 63, lo16, hi16);
}

#endif

BC7PredecodedEndpointsBlock::BC7PredecodedEndpointsBlock()
{
	index_cache = 0;
	index_cache_size = 0;
	eval = 0;
	find_next_at_most = 0;
	target_mode = 8;
	ignore_alpha = 0;
	partition_id = 0;
	partition_eqv = 0;
	index_swap = 0;
}

BC7PredecodedEndpointsBlock::~BC7PredecodedEndpointsBlock()
{
	if ( index_cache )
		OODLE_FREE_ARRAY( index_cache, index_cache_size );

}

void BC7PredecodedEndpointsBlock::init_indices(int mode, const U8 * inds_from, SINTa inds_stride, SINTa inds_count)
{
	RR_ASSERT(mode >= 0 && mode <= 7);
	target_mode = (U8)mode;

#if defined(DO_BUILD_SSE4) || defined(DO_BUILD_NEON64)
	SINTa group_size;

	// Some modes do their own thing for setup
	switch (target_mode)
	{
		// One-subset modes are easy since we don't need to worry about anchor positions
	case 4:
		index_cache_size = inds_count * 32;
		index_cache = OODLE_MALLOC_ARRAY_ALIGNED(U8,index_cache_size,16);
		for (SINTa i = 0; i < inds_count; ++i)
			simd_decode_mode4_inds(index_cache + i*32, Vec128_U8::loadu(inds_from + i*inds_stride));
		return;

	case 5:
		index_cache_size = inds_count * 32;
		index_cache = OODLE_MALLOC_ARRAY_ALIGNED(U8,index_cache_size,16);
		for (SINTa i = 0; i < inds_count; ++i)
			simd_decode_mode5_inds(index_cache + i*32, Vec128_U8::loadu(inds_from + i*inds_stride));
		return;

	case 6:
		index_cache_size = inds_count * 16;
		index_cache = OODLE_MALLOC_ARRAY_ALIGNED(U8,index_cache_size,16);
		for (SINTa i = 0; i < inds_count; ++i)
			simd_decode_mode6_inds(Vec128_U8::loadu(inds_from + i*inds_stride)).storeu(index_cache + i*16);
		return;

		// Two-subset modes, we need to decode multiple times because the second anchor can be in several positions
	case 1:
		group_size = inds_count * 16;
		index_cache_size = group_size * 4;
		index_cache = OODLE_MALLOC_ARRAY_ALIGNED(U8,index_cache_size,16);
		for (SINTa i = 0; i < inds_count; i++)
		{
			Vec128_U8 raw_inds = Vec128_U8::loadu(inds_from + i*inds_stride);
			for (SINTa j = 0; j < 4; j++)
				simd_decode_mode1_inds(raw_inds, two_subset_repr[j]).storeu(index_cache + i*16 + j*group_size);
		}
		return;

	case 3:
		group_size = inds_count * 16;
		index_cache_size = group_size * 4;
		index_cache = OODLE_MALLOC_ARRAY_ALIGNED(U8,index_cache_size,16);
		for (SINTa i = 0; i < inds_count; i++)
		{
			Vec128_U8 raw_inds = Vec128_U8::loadu(inds_from + i*inds_stride);
			for (SINTa j = 0; j < 4; j++)
				simd_decode_mode3_inds(raw_inds, two_subset_repr[j]).storeu(index_cache + i*16 + j*group_size);
		}
		return;

	case 7:
		group_size = inds_count * 16;
		index_cache_size = group_size * 4;
		index_cache = OODLE_MALLOC_ARRAY_ALIGNED(U8,index_cache_size,16);
		for (SINTa i = 0; i < inds_count; i++)
		{
			Vec128_U8 raw_inds = Vec128_U8::loadu(inds_from + i*inds_stride);
			for (SINTa j = 0; j < 4; j++)
				simd_decode_mode7_inds(raw_inds, two_subset_repr[j]).storeu(index_cache + i*16 + j*group_size);
		}
		return;
	}
#endif

	// General path
	index_cache_size = inds_count * 16;
	index_cache = OODLE_MALLOC_ARRAY_ALIGNED(U8,index_cache_size,16);
	for (SINTa i = 0; i < inds_count; ++i)
		memcpy(index_cache + i*16, inds_from + i*inds_stride, 16);
}

void BC7PredecodedEndpointsBlock::init_endpoints(const U8 * endpts_from, const U8 tgt_pixels_rgba[64], bool ignore_alpha_chan)
{
	// endpt_block = endpoints with pbits, indices zeroed
	bc7bits endpts = bc7bits_load(endpts_from);
	RR_ASSERT(bc7bits_get_mode(endpts) == target_mode);

	bc7bits idxmask(c_bc7bitrange_indices_mask[target_mode]);

	bc7bits_store(endpt_block, bc7bits_andnot(endpts, idxmask));
	bc7bits_store(index_mask, idxmask);

	ignore_alpha = ignore_alpha_chan ? 1 : 0;

#if defined(DO_BUILD_SSE4) || defined(DO_BUILD_NEON64)
	switch (target_mode)
	{
	case 0: preendpoint_init_simd_mode0(this, tgt_pixels_rgba, endpts_from); break;
	case 1: preendpoint_init_simd_mode1(this, tgt_pixels_rgba, endpts_from); break;
	case 2: preendpoint_init_simd_mode2(this, tgt_pixels_rgba, endpts_from); break;
	case 3: preendpoint_init_simd_mode3(this, tgt_pixels_rgba, endpts_from); break;
	case 4: preendpoint_init_simd_mode4(this, tgt_pixels_rgba, endpts_from, ignore_alpha_chan); break;
	case 5: preendpoint_init_simd_mode5(this, tgt_pixels_rgba, endpts_from, ignore_alpha_chan); break;
	case 6: preendpoint_init_simd_mode6(this, tgt_pixels_rgba, endpts_from, ignore_alpha_chan); break;
	case 7: preendpoint_init_simd_mode7(this, tgt_pixels_rgba, endpts_from, ignore_alpha_chan); break;
	default:
		// Placeholder/debug impl!
		eval = endpoints_eval_simd_basic;
		find_next_at_most = find_next_at_most_simd_basic;

		for LOOP(i,64)
			base[i] = tgt_pixels_rgba[i];
		break;
	}
#else
	eval = endpoints_eval_scalar_basic;
	find_next_at_most = find_next_at_most_scalar_basic;

	for LOOP(i,64)
		base[i] = tgt_pixels_rgba[i];
#endif
}

OODLE_NS_END

