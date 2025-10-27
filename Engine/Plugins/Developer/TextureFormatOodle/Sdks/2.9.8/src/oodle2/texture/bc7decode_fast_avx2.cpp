// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetavx2

#include "bc67format.h"
#include "bc7decode_fast.h"
#include "vec256.inl"

#ifdef DO_BUILD_AVX2

OODLE_NS_START

#define rep7x(a) (a),(a),(a),(a),(a),(a),(a)
#define rep8x(a) (a),rep7x(a)

namespace bc7decode {

#include "bc7decode_fast_common.inl"

static RADFORCEINLINE void avx2_alpha_override_endpoints16(Vec256 * lo16, Vec256 * hi16, bool ignore_alpha)
{
	if ( ignore_alpha )
	{
		Vec256 override_alpha = _mm256_setr_epi16(0,0,0,255, 0,0,0,255, 0,0,0,255, 0,0,0,255);
		*lo16 = _mm256_or_si256(*lo16, override_alpha);
		*hi16 = _mm256_or_si256(*hi16, override_alpha);
	}
}
// AVX2 version of multigetbits. Because of the way AVX2 works, this is really two
// 128-bit multigetbits, with the first half of "pos"/"len" indexing the first half of "bytes"
// and the second half of operands addressing the other half.
template<
	int pos0,int len0,
	int pos1,int len1,
	int pos2,int len2,
	int pos3,int len3,
	int pos4,int len4,
	int pos5,int len5,
	int pos6,int len6,
	int pos7,int len7,
	int pos8,int len8,
	int pos9,int len9,
	int posa,int lena,
	int posb,int lenb,
	int posc,int lenc,
	int posd,int lend,
	int pose,int lene,
	int posf,int lenf
>
static RADFORCEINLINE __m256i avx2_multigetbits(const __m256i &bytes)
{
	// Grab the two bytes straddling each field
	const __m256i shuf = _mm256_setr_epi8(
		(pos0 >> 3), (pos0 >> 3) + 1,
		(pos1 >> 3), (pos1 >> 3) + 1,
		(pos2 >> 3), (pos2 >> 3) + 1,
		(pos3 >> 3), (pos3 >> 3) + 1,
		(pos4 >> 3), (pos4 >> 3) + 1,
		(pos5 >> 3), (pos5 >> 3) + 1,
		(pos6 >> 3), (pos6 >> 3) + 1,
		(pos7 >> 3), (pos7 >> 3) + 1,
		(pos8 >> 3), (pos8 >> 3) + 1,
		(pos9 >> 3), (pos9 >> 3) + 1,
		(posa >> 3), (posa >> 3) + 1,
		(posb >> 3), (posb >> 3) + 1,
		(posc >> 3), (posc >> 3) + 1,
		(posd >> 3), (posd >> 3) + 1,
		(pose >> 3), (pose >> 3) + 1,
		(posf >> 3), (posf >> 3) + 1
	);

	// work around cast truncation warnings
	// the real issue here is that _mm_setr_* are all signed and we
	// need U16 constants
#define SHORT_CONST(x) ((short)(((x) & 0x7fff) - ((x) & 0x8000)))

	// Use a multiply to do a per-lane variable shift to align the
	// desired bits at the top
	const __m256i mult = _mm256_setr_epi16(
		SHORT_CONST(1 << (16 - len0 - (pos0 & 7))),
		SHORT_CONST(1 << (16 - len1 - (pos1 & 7))),
		SHORT_CONST(1 << (16 - len2 - (pos2 & 7))),
		SHORT_CONST(1 << (16 - len3 - (pos3 & 7))),
		SHORT_CONST(1 << (16 - len4 - (pos4 & 7))),
		SHORT_CONST(1 << (16 - len5 - (pos5 & 7))),
		SHORT_CONST(1 << (16 - len6 - (pos6 & 7))),
		SHORT_CONST(1 << (16 - len7 - (pos7 & 7))),
		SHORT_CONST(1 << (16 - len8 - (pos8 & 7))),
		SHORT_CONST(1 << (16 - len9 - (pos9 & 7))),
		SHORT_CONST(1 << (16 - lena - (posa & 7))),
		SHORT_CONST(1 << (16 - lenb - (posb & 7))),
		SHORT_CONST(1 << (16 - lenc - (posc & 7))),
		SHORT_CONST(1 << (16 - lend - (posd & 7))),
		SHORT_CONST(1 << (16 - lene - (pose & 7))),
		SHORT_CONST(1 << (16 - lenf - (posf & 7)))
	);

#undef SHORT_CONST

	// ---- Now comes the actual code, which is very short:
	__m256i shuffled = _mm256_shuffle_epi8(bytes, shuf);
	__m256i result = _mm256_mullo_epi16(shuffled, mult);

	return result;
}

static RADFORCEINLINE void avx2_interpolate_one_subset_twoind(U8 * out_rgba, U32 crot_idxmode, const __m256i & lerpf8_256, const Vec128 &endpoints16_in, bool ignore_alpha)
{
	// Determine channel rotate shuffle
	Vec128 crot_shuf = simd_channel_rotate16(crot_idxmode & 3);

	// Apply to endpoints
	Vec256 endpoints16 = _mm256_castsi128_si256(_mm_shuffle_epi8(endpoints16_in, crot_shuf));

	// Grab the 8 lerp factors we'll need and replicate them in the pattern
	// we need, stretching them out to 16 bits by left-shifting by 8
	// along the way. Also apply channel rotate.
	//
	// NOTE: the indices here advance in an odd way because the "pack" operation below
	// packs the 128-bit halves separately.
	static RAD_ALIGN(const S8, lerpf16_shuffles[8][32], 32) =
	{
#define IDXS(a,b,c,d) -16,(a),-16,(b),-16,(c),-16,(d)
#define FOURIDXS(a,b,c,d) IDXS(a,b,c,d), IDXS(a+2,b+2,c+2,d+2), IDXS(a+8,b+8,c+8,d+8), IDXS(a+10,b+10,c+10,d+10)

		// idxMode=0 (no index swap)
		{ FOURIDXS(0,0,0,1) },	 // rot=0: no change
		{ FOURIDXS(1,0,0,0) },	 // rot=1: swap R<->A
		{ FOURIDXS(0,1,0,0) },	 // rot=2: swap G<->A
		{ FOURIDXS(0,0,1,0) },	 // rot=3: swap B<->A

		// idxMode=1 (index swap)
		{ FOURIDXS(1,1,1,0) },	 // rot=0: no change
		{ FOURIDXS(0,1,1,1) },	 // rot=1: swap R<->A
		{ FOURIDXS(1,0,1,1) },	 // rot=2: swap G<->A
		{ FOURIDXS(1,1,0,1) },	 // rot=3: swap B<->A

#undef FOURIDXS
#undef IDXS
	};
	Vec256 lerpf16_shuf0 = load256u(lerpf16_shuffles[crot_idxmode]);
	Vec256 lerpf16_shuf1 = _mm256_add_epi8(lerpf16_shuf0, _mm256_set1_epi8(4));

	// Shuffle the endpoints around so we have the "low" and "high" end separated out
	Vec256 lo16 = shuffle64<0,0,0,0>(endpoints16);
	Vec256 hi16 = shuffle64<1,1,1,1>(endpoints16);
	avx2_alpha_override_endpoints16(&lo16, &hi16, ignore_alpha);

	// Set up for interpolation
	Vec256 diff16 = _mm256_sub_epi16(lo16, hi16); // yes, lo-hi!

	// First, grab the lerp factors
	Vec256 lerpf8_0 = _mm256_permute2x128_si256(lerpf8_256, lerpf8_256, 0x00); // broadcast low half
	Vec256 lerpf8_1 = _mm256_permute2x128_si256(lerpf8_256, lerpf8_256, 0x11); // broadcast high half

	Vec256 lerpf16_0 = _mm256_shuffle_epi8(lerpf8_0, lerpf16_shuf0);
	Vec256 lerpf16_1 = _mm256_shuffle_epi8(lerpf8_0, lerpf16_shuf1);
	Vec256 lerpf16_2 = _mm256_shuffle_epi8(lerpf8_1, lerpf16_shuf0);
	Vec256 lerpf16_3 = _mm256_shuffle_epi8(lerpf8_1, lerpf16_shuf1);

	// Interpolate via rounding_shift_right(neg_factor * (lo - hi), 15) + lo
	Vec256 interp16_0 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_0, diff16), lo16);
	Vec256 interp16_1 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_1, diff16), lo16);
	Vec256 interp16_2 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_2, diff16), lo16);
	Vec256 interp16_3 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_3, diff16), lo16);

	// Pack down to 8 bits and store
	Vec256 interp8_0 = _mm256_packus_epi16(interp16_0, interp16_1);
	Vec256 interp8_1 = _mm256_packus_epi16(interp16_2, interp16_3);

	store256u(out_rgba +  0, interp8_0);
	store256u(out_rgba + 32, interp8_1);
}

static RADFORCEINLINE void avx2_interpolate_two_subset(U8 * out_rgba, const BC67Partition * partition, const Vec128 &lerpf8, const Vec128 &lo16, const Vec128 &hi16)
{
	// Set up for interpolation
	Vec256 lo16_256 = broadcast128_256(lo16);
	Vec256 diff16_256 = broadcast128_256(_mm_sub_epi16(lo16, hi16));

	// Select the values for the respective subsets
	Vec256 lo16_subset0 = shuffle32in128<0,1,0,1>(lo16_256);
	Vec256 lo16_subset1 = shuffle32in128<2,3,2,3>(lo16_256);
	Vec256 diff16_subset0 = shuffle32in128<0,1,0,1>(diff16_256);
	Vec256 diff16_subset1 = shuffle32in128<2,3,2,3>(diff16_256);

	// Shuffle for low/high lerp factors
	// replicates them 4x each and also expands to 16 bits by left-shifting by 8
	static RAD_ALIGN(const S8, lerpf16_shuffles[128], 32) =
	{
#define FOUR(x) -1,(x),-1,(x),-1,(x),-1,(x)
		FOUR( 0), FOUR( 1), FOUR( 4), FOUR( 5),
		FOUR( 2), FOUR( 3), FOUR( 6), FOUR( 7),
		FOUR( 8), FOUR( 9), FOUR(12), FOUR(13),
		FOUR(10), FOUR(11), FOUR(14), FOUR(15),
#undef FOUR
	};

	// Subset mask for the pixels
	const Vec256 subset_mask = _mm256_set1_epi32(partition->subset_mask);

	const Vec256 subset_shift_0 = _mm256_setr_epi32(31,31, 29,29, 23,23, 21,21);
	const Vec256 subset_shift_1 = _mm256_setr_epi32(27,27, 25,25, 19,19, 17,17);
	const Vec256 subset_shift_2 = _mm256_setr_epi32(15,15, 13,13,  7, 7,  5, 5);
	const Vec256 subset_shift_3 = _mm256_setr_epi32(11,11,  9, 9,  3, 3,  1, 1);

	const Vec256 lerpf8_256 = broadcast128_256(lerpf8);

	// Determine masks for whether a pixel is in subset 1
	// this puts the relevant bit in the MSB of every DWord, to be used by VBLENDVPS
	Vec256 in_subset1_0 = _mm256_sllv_epi32(subset_mask, subset_shift_0);
	Vec256 in_subset1_1 = _mm256_sllv_epi32(subset_mask, subset_shift_1);
	Vec256 in_subset1_2 = _mm256_sllv_epi32(subset_mask, subset_shift_2);
	Vec256 in_subset1_3 = _mm256_sllv_epi32(subset_mask, subset_shift_3);

	// Using these masks, select the "lo" and "diff" values to use
	Vec256 lo16_0 = blend32(lo16_subset0, lo16_subset1, in_subset1_0);
	Vec256 lo16_1 = blend32(lo16_subset0, lo16_subset1, in_subset1_1);
	Vec256 lo16_2 = blend32(lo16_subset0, lo16_subset1, in_subset1_2);
	Vec256 lo16_3 = blend32(lo16_subset0, lo16_subset1, in_subset1_3);

	Vec256 diff16_0 = blend32(diff16_subset0, diff16_subset1, in_subset1_0);
	Vec256 diff16_1 = blend32(diff16_subset0, diff16_subset1, in_subset1_1);
	Vec256 diff16_2 = blend32(diff16_subset0, diff16_subset1, in_subset1_2);
	Vec256 diff16_3 = blend32(diff16_subset0, diff16_subset1, in_subset1_3);

	// Grab the lerp factors
	Vec256 lerpf8_0 = _mm256_permute2x128_si256(lerpf8_256, lerpf8_256, 0x00); // broadcast low half
	Vec256 lerpf8_1 = _mm256_permute2x128_si256(lerpf8_256, lerpf8_256, 0x11); // broadcast high half

	Vec256 lerpf16_0 = _mm256_shuffle_epi8(lerpf8_0, load256u(lerpf16_shuffles +  0));
	Vec256 lerpf16_1 = _mm256_shuffle_epi8(lerpf8_0, load256u(lerpf16_shuffles + 32));
	Vec256 lerpf16_2 = _mm256_shuffle_epi8(lerpf8_1, load256u(lerpf16_shuffles + 64));
	Vec256 lerpf16_3 = _mm256_shuffle_epi8(lerpf8_1, load256u(lerpf16_shuffles + 96));

	// Interpolate via rounding_shift_right(neg_factor * (lo - hi), 15) + lo
	Vec256 interp16_0 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_0, diff16_0), lo16_0);
	Vec256 interp16_1 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_1, diff16_1), lo16_1);
	Vec256 interp16_2 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_2, diff16_2), lo16_2);
	Vec256 interp16_3 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_3, diff16_3), lo16_3);

	// Pack down to 8 bits and store
	Vec256 interp8_0 = _mm256_packus_epi16(interp16_0, interp16_1);
	Vec256 interp8_1 = _mm256_packus_epi16(interp16_2, interp16_3);

	store256u(out_rgba +  0, interp8_0);
	store256u(out_rgba + 32, interp8_1);
}

void avx2_decode_mode1(U8 * out_rgba, const U8 * block_bits, bool /* ignore_alpha */)
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

	Vec128 lerpf8 = simd_decode_mode1_inds(block128, partition);

	// And interpolate!
	// NOTE: ignore_alpha is a nop, the output always has all-255 alpha anyway
	avx2_interpolate_two_subset(out_rgba, partition, lerpf8, lo16, hi16);
}

void avx2_decode_mode3(U8 * out_rgba, const U8 * block_bits, bool /* ignore_alpha */)
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
	Vec128 lerpf8 = simd_decode_mode3_inds(block128, partition);

	// And interpolate!
	// NOTE: ignore_alpha is a nop, the output always has all-255 alpha anyway
	avx2_interpolate_two_subset(out_rgba, partition, lerpf8, lo16, hi16);
}

void avx2_decode_mode4(U8 * out_rgba, const U8 * block_bits, bool ignore_alpha)
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

	Vec128 block128 = load128u(block_bits);
	const __m256i block256 = _mm256_broadcastsi128_si256(block128);

	// Decode the indices
	//
	// decode straight into the interleaved (vector index, scalar index)
	// form. This means we get 16 bits per index which dovetails nicely
	// with the 16-bit outputs from multigetbits.
	//
	// Set all field lengths to 8 bits to simplify the logistics below,
	// because it means the fields already start byte-aligned.
	__m256i index0_bits = avx2_multigetbits<
		50,8, // anchor slot for this so 1 bit only
		51,8,
		53,8,
		55,8,
		57,8,
		59,8,
		61,8,
		63,8,
		65,8,
		67,8,
		69,8,
		71,8,
		73,8,
		75,8,
		77,8,
		79,8
	>(block256);

	__m256i index1_bits = avx2_multigetbits<
		81,8, // anchor slot for this so 2 bits only
		83,8,
		86,8,
		89,8,
		92,8,
		95,8,
		98,8,
		101,8,
		104,8,
		107,8,
		110,8,
		113,8,
		116,8,
		119,8,
		122,8,
		125,8
	>(block256);

	// We have all the indices separated already so nothing to do for anchor
	// insertion, we just need to make sure to clear the affected bits.
	index0_bits = _mm256_and_si256(index0_bits, _mm256_setr_epi16(1<<8, rep7x(3<<8), rep8x(3<<8)));
	index1_bits = _mm256_and_si256(index1_bits, _mm256_setr_epi16(3<<8, rep7x(7<<8), rep8x(7<<8)));

	// Then align the pieces and OR in a constant to make the index1 values hit different table slots
	//
	// this puts the index0 values in the low byte of every word and the index1 values
	// in the high byte.
	__m256i index8 = _mm256_or_si256(_mm256_srli_epi16(index0_bits, 8), index1_bits);
	index8 = _mm256_or_si256(index8, _mm256_set1_epi16(0x0800));

	// Table lookup to convert everything into lerp factors
	const __m256i lerp_factor_table = _mm256_setr_epi8(
		0, -42, -86, -128,  0,0,0,0,  0, -18, -36, -54, -74, -92, -110, -128,
		0, -42, -86, -128,  0,0,0,0,  0, -18, -36, -54, -74, -92, -110, -128
	);
	__m256i lerpf8_256 = _mm256_shuffle_epi8(lerp_factor_table, index8);

	// Decode the endpoints
	Vec128 endpoints16 = simd_decode_mode4_endpoints(Vec128_U8(block128));

	// And interpolate!
	avx2_interpolate_one_subset_twoind(out_rgba, block_bits[0] >> 5, lerpf8_256, endpoints16, ignore_alpha);
}

void avx2_decode_mode5(U8 * out_rgba, const U8 * block_bits, bool ignore_alpha)
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

	Vec128 block128 = load128u(block_bits);
	const __m256i block256 = _mm256_broadcastsi128_si256(block128);

	// Index decoding follows the same process as the mode 4 variant.
	__m256i index0_bits = avx2_multigetbits<
		66,8, // anchor slot for this so 1 bit only
		67,8,
		69,8,
		71,8,
		73,8,
		75,8,
		77,8,
		79,8,
		81,8,
		83,8,
		85,8,
		87,8,
		89,8,
		91,8,
		93,8,
		95,8
	>(block256);

	__m256i index1_bits = avx2_multigetbits<
		97,8, // anchor slot for this so 1 bit only
		98,8,
		100,8,
		102,8,
		104,8,
		106,8,
		108,8,
		110,8,
		112,8,
		114,8,
		116,8,
		118,8,
		120,8,
		122,8,
		124,8,
		126,8
	>(block256);

	// We have all the indices separated already so nothing to do for anchor
	// insertion, we just need to make sure to clear the affected bits.
	const __m256i anchor_mask = _mm256_setr_epi16(1<<8, rep7x(3<<8), rep8x(3<<8));
	index0_bits = _mm256_and_si256(index0_bits, anchor_mask);
	index1_bits = _mm256_and_si256(index1_bits, anchor_mask);

	// Then merge the pieces.
	// this puts the index0 values in the low byte of every word and the index1 values
	// in the high byte.
	__m256i index8 = _mm256_or_si256(_mm256_srli_epi16(index0_bits, 8), index1_bits);

	// Table lookup to convert everything into lerp factors
	const __m256i lerp_factor_table = _mm256_setr_epi8(
		0, -42, -86, -128,  0,0,0,0,  0,0,0,0,  0,0,0,0,
		0, -42, -86, -128,  0,0,0,0,  0,0,0,0,  0,0,0,0
	);
	__m256i lerpf8_256 = _mm256_shuffle_epi8(lerp_factor_table, index8);

	Vec128 endpoints16 = simd_decode_mode5_endpoints(Vec128_U8(block128));

	avx2_interpolate_one_subset_twoind(out_rgba, block_bits[0] >> 6, lerpf8_256, endpoints16, ignore_alpha);
}

void avx2_decode_mode6(U8 * out_rgba, const U8 * block_bits, bool ignore_alpha)
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

	// Decode the endpoints and lerp factors
	Vec256 endpoints = _mm256_castsi128_si256(simd_decode_mode6_endpoints(block128));
	Vec128 lerpf8 = simd_decode_mode6_inds(block128);

	// Shuffle the endpoints around so we have the "low" and "high" end separated out
	Vec256 lo16 = shuffle64<0,0,0,0>(endpoints);
	Vec256 hi16 = shuffle64<1,1,1,1>(endpoints);

	avx2_alpha_override_endpoints16(&lo16, &hi16, ignore_alpha);

	// Set up for interpolation
	Vec256 diff16 = _mm256_sub_epi16(lo16, hi16); // yes, lo-hi!
	Vec256 lerpf256 = broadcast128_256(lerpf8);

	// Shuffle for low/high lerp factors
	// replicates them 4x each and also expands to 16 bits by left-shifting by 8
	// strange ordering because of the way AVX2 works
	static RAD_ALIGN(const S8, lerpf16_shuffles[128], 32) =
	{
#define FOUR(x) -1,(x),-1,(x),-1,(x),-1,(x)
		FOUR( 0), FOUR( 1), FOUR( 4), FOUR( 5),
		FOUR( 2), FOUR( 3), FOUR( 6), FOUR( 7),
		FOUR( 8), FOUR( 9), FOUR(12), FOUR(13),
		FOUR(10), FOUR(11), FOUR(14), FOUR(15),
#undef FOUR
	};

	// Replicate the lerp factors 4x each and expand to 16 bits by shifting left by 8
	Vec256 lerpf16_0 = _mm256_shuffle_epi8(lerpf256, load256u(lerpf16_shuffles +  0));
	Vec256 lerpf16_1 = _mm256_shuffle_epi8(lerpf256, load256u(lerpf16_shuffles + 32));
	Vec256 lerpf16_2 = _mm256_shuffle_epi8(lerpf256, load256u(lerpf16_shuffles + 64));
	Vec256 lerpf16_3 = _mm256_shuffle_epi8(lerpf256, load256u(lerpf16_shuffles + 96));

	// Interpolate via rounding_shift_right(neg_factor * (lo - hi), 15) + lo
	Vec256 interp16_0 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_0, diff16), lo16);
	Vec256 interp16_1 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_1, diff16), lo16);
	Vec256 interp16_2 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_2, diff16), lo16);
	Vec256 interp16_3 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_3, diff16), lo16);

	// Pack down to 8 bits and store
	Vec256 interp8_0 = _mm256_packus_epi16(interp16_0, interp16_1);
	Vec256 interp8_1 = _mm256_packus_epi16(interp16_2, interp16_3);

	store256u(out_rgba +  0, interp8_0);
	store256u(out_rgba + 32, interp8_1);
}

void avx2_decode_mode7(U8 * out_rgba, const U8 * block_bits, bool ignore_alpha)
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
	Vec128 lerpf8 = simd_decode_mode7_inds(block128, partition);

	// And interpolate!
	avx2_interpolate_two_subset(out_rgba, partition, lerpf8, lo16, hi16);
}

static RADFORCEINLINE U32 endpoints_eval_finish_avx2_oneindex(const BC7PredecodedEndpointsBlock * b, const Vec128 &lerpf8)
{
	// Shuffle for low/high lerp factors
	// replicates them 4x each and also expands to 16 bits by left-shifting by 8
	static RAD_ALIGN(const S8, lerpf16_shuffles[128], 32) =
	{
#define FOUR(x) -1,(x),-1,(x),-1,(x),-1,(x)
		FOUR( 0), FOUR( 1), FOUR( 2), FOUR( 3),
		FOUR( 4), FOUR( 5), FOUR( 6), FOUR( 7),
		FOUR( 8), FOUR( 9), FOUR(10), FOUR(11),
		FOUR(12), FOUR(13), FOUR(14), FOUR(15),
#undef FOUR
	};

	// Broadcast
	Vec256 lerpf8_256 = broadcast128_256(lerpf8);

	// Replicate the lerp factors 4x each and expand to 16 bits by shifting left by 8
	Vec256 lerpf16_0 = _mm256_shuffle_epi8(lerpf8_256, load256u(lerpf16_shuffles));
	Vec256 lerpf16_1 = _mm256_shuffle_epi8(lerpf8_256, load256u(lerpf16_shuffles + 32));
	Vec256 lerpf16_2 = _mm256_shuffle_epi8(lerpf8_256, load256u(lerpf16_shuffles + 64));
	Vec256 lerpf16_3 = _mm256_shuffle_epi8(lerpf8_256, load256u(lerpf16_shuffles + 96));

	// Interpolate via rounding_shift_right(neg_factor * (lo - hi), 15) + lo
	// also computes diff to original block values which are subtracted from base
	Vec256 interp16_0 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_0, load256u(b->diff +  0)), load256u(b->base +  0));
	Vec256 interp16_1 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_1, load256u(b->diff + 16)), load256u(b->base + 16));
	Vec256 interp16_2 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_2, load256u(b->diff + 32)), load256u(b->base + 32));
	Vec256 interp16_3 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_3, load256u(b->diff + 48)), load256u(b->base + 48));

	// First half of dot product: R*R + G*G, B*B + A*A
	// the rest we do as part of the reduction
	Vec256 err_0 = _mm256_madd_epi16(interp16_0, interp16_0);
	Vec256 err_1 = _mm256_madd_epi16(interp16_1, interp16_1);
	Vec256 err_2 = _mm256_madd_epi16(interp16_2, interp16_2);
	Vec256 err_3 = _mm256_madd_epi16(interp16_3, interp16_3);

	// Finish the reduction
	Vec256 err_01 = _mm256_add_epi32(err_0, err_1);
	Vec256 err_23 = _mm256_add_epi32(err_2, err_3);
	Vec256 total_err = _mm256_add_epi32(err_01, err_23);

	Vec128 sum_total = _mm_add_epi32(lo_half(total_err), hi_half(total_err));

	return reduce_add_s32(sum_total);
}

static RADFORCEINLINE U32 endpoints_eval_finish_avx2_twoindex(const BC7PredecodedEndpointsBlock * b, const U8 * lerpf_buf, bool index_swap)
{
	// Shuffle for low/high lerp factors
	// replicates them 4x each and also expands to 16 bits by left-shifting by 8
	static RAD_ALIGN(const S8, lerpf16_shuffles[2][64], 32) = // [index_swap][i]
	{
#define INDS(x,y) -1,(x),-1,(x),-1,(x),-1,(y)
		{ // index_swap=false
			INDS( 0, 1), INDS( 2, 3), INDS( 4, 5), INDS( 6, 7),
			INDS( 8, 9), INDS(10,11), INDS(12,13), INDS(14,15)
		},
		{ // index_swap=true
			INDS( 1, 0), INDS( 3, 2), INDS( 5, 4), INDS( 7, 6),
			INDS( 9, 8), INDS(11,10), INDS(13,12), INDS(15,14)
		},
#undef INDS
	};
	// Shuffle the interleaved lerp factors to replicate them in the 3-1 scalar-vector pattern
	// we always work with values in the pre-color-rotate RGB=vector, A=scalar space
	const Vec256 lerpf8_lo = broadcast128_256(load128u(lerpf_buf + 0));
	const Vec256 lerpf8_hi = broadcast128_256(load128u(lerpf_buf + 16));
	const Vec256 lshuf0 = load256u(lerpf16_shuffles[index_swap] + 0);
	const Vec256 lshuf1 = load256u(lerpf16_shuffles[index_swap] + 32);

	Vec256 lerpf16_0 = _mm256_shuffle_epi8(lerpf8_lo, lshuf0);
	Vec256 lerpf16_1 = _mm256_shuffle_epi8(lerpf8_lo, lshuf1);
	Vec256 lerpf16_2 = _mm256_shuffle_epi8(lerpf8_hi, lshuf0);
	Vec256 lerpf16_3 = _mm256_shuffle_epi8(lerpf8_hi, lshuf1);

	// Interpolate via rounding_shift_right(neg_factor * (lo - hi), 15) + lo
	// also computes diff to original block values which are subtracted from base
	Vec256 interp16_0 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_0, load256u(b->diff +  0)), load256u(b->base +  0));
	Vec256 interp16_1 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_1, load256u(b->diff + 16)), load256u(b->base + 16));
	Vec256 interp16_2 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_2, load256u(b->diff + 32)), load256u(b->base + 32));
	Vec256 interp16_3 = _mm256_add_epi16(_mm256_mulhrs_epi16(lerpf16_3, load256u(b->diff + 48)), load256u(b->base + 48));

	// First half of dot product: R*R + G*G, B*B + A*A
	// the rest we do as part of the reduction
	Vec256 err_0 = _mm256_madd_epi16(interp16_0, interp16_0);
	Vec256 err_1 = _mm256_madd_epi16(interp16_1, interp16_1);
	Vec256 err_2 = _mm256_madd_epi16(interp16_2, interp16_2);
	Vec256 err_3 = _mm256_madd_epi16(interp16_3, interp16_3);

	// Finish the reduction
	Vec256 err_01 = _mm256_add_epi32(err_0, err_1);
	Vec256 err_23 = _mm256_add_epi32(err_2, err_3);
	Vec256 total_err = _mm256_add_epi32(err_01, err_23);

	Vec128 sum_total = _mm_add_epi32(lo_half(total_err), hi_half(total_err));

	return reduce_add_s32(sum_total);
}

U32 endpoints_eval_avx2_twosubset(const BC7PredecodedEndpointsBlock * b, SINTa inds_from)
{
	Vec128 lerpf8 = load128u(b->index_cache + inds_from*16 + b->partition_eqv * (b->index_cache_size >> 2));
	return endpoints_eval_finish_avx2_oneindex(b, lerpf8);
}

SINTa find_next_at_most_avx2_twosubset(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh)
{
	const U8 * index_cache = b->index_cache + b->partition_eqv * (b->index_cache_size >> 2);
	for (SINTa i = 0; i < count; ++i)
	{
		Vec128 lerpf8 = load128u(index_cache + (inds_from + i) * 16);
		U32 ssd = endpoints_eval_finish_avx2_oneindex(b, lerpf8);
		if (ssd <= err_thresh)
		{
			*out_err = ssd;
			return i;
		}
	}

	return count;
}

U32 endpoints_eval_avx2_onesubset_twoind(const BC7PredecodedEndpointsBlock * b, SINTa inds_from)
{
	return endpoints_eval_finish_avx2_twoindex(b, b->index_cache + inds_from * 32, b->index_swap != 0);
}

SINTa find_next_at_most_avx2_onesubset_twoind(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh)
{
	bool index_swap = b->index_swap != 0;

	for (SINTa i = 0; i < count; ++i)
	{
		U32 ssd = endpoints_eval_finish_avx2_twoindex(b, b->index_cache + (inds_from + i) * 32, index_swap);
		if (ssd <= err_thresh)
		{
			*out_err = ssd;
			return i;
		}
	}

	return count;
}

U32 endpoints_eval_avx2_mode6(const BC7PredecodedEndpointsBlock * b, SINTa inds_from)
{
	// Get indices from inds_from, everything else is already set up
	Vec128 lerpf8 = load128u(b->index_cache + inds_from * 16);
	return endpoints_eval_finish_avx2_oneindex(b, lerpf8);
}

SINTa find_next_at_most_avx2_mode6(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh)
{
	for (SINTa i = 0; i < count; ++i)
	{
		Vec128 lerpf8 = load128u(b->index_cache + (inds_from + i) * 16);
		U32 ssd = endpoints_eval_finish_avx2_oneindex(b, lerpf8);
		if (ssd <= err_thresh)
		{
			*out_err = ssd;
			return i;
		}
	}

	return count;
}

}

OODLE_NS_END

#else // !DO_BUILD_AVX2

OODLE_NS_START

namespace bc7decode {

U32 endpoints_eval_avx2_twosubset(const BC7PredecodedEndpointsBlock * b, SINTa inds_from)
{
	RR_ASSERT_FAILURE_ALWAYS("should not get here");
	return 0;
}

SINTa find_next_at_most_avx2_twosubset(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh)
{
	RR_ASSERT_FAILURE_ALWAYS("should not get here");
	return count;
}

U32 endpoints_eval_avx2_onesubset_twoind(const BC7PredecodedEndpointsBlock * b, SINTa inds_from)
{
	RR_ASSERT_FAILURE_ALWAYS("should not get here");
	return 0;
}

SINTa find_next_at_most_avx2_onesubset_twoind(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh)
{
	RR_ASSERT_FAILURE_ALWAYS("should not get here");
	return count;
}

U32 endpoints_eval_avx2_mode6(const BC7PredecodedEndpointsBlock * b, SINTa inds_from)
{
	RR_ASSERT_FAILURE_ALWAYS("should not get here");
	return 0;
}

SINTa find_next_at_most_avx2_mode6(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh)
{
	RR_ASSERT_FAILURE_ALWAYS("should not get here");
	return count;
}

OODLE_NS_END

}

#endif
