// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "newlz_offsets.h"
#include "newlz_shared.h"
#include "rrlzh_lzhlw_shared.h"
#include "rrlogutil.h"
#include "newlz_simd.h"
#include "newlz_arrays.h"
#include "newlz_shared.h"
#include "newlz_speedfit.h"
#include "rrarenaallocator.h"
#include "rrprefetch.h"

//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"
#include "speedfitter.h"

#include "templates/rralgorithm.h"

#if defined(NEWLZ_OFFSET_ARM64_ASM) && !defined(__RADARM64__)
// on iOS we have NEWLZ_OFFSET_ARM64_ASM_LINKED set for both 32-bit and 64-bit
//	but we only want to use it in the 64-bit build
#undef NEWLZ_OFFSET_ARM64_ASM
#endif

OODLE_NS_START

// @@ HACKY : NEED LRL_MAX here because it's added to excesses
#define NEWLZ_PACKET_LRL_MAX	3


/*
#define CHECK_BITS	7
#define CHECK_VAL	100
/**/

//===============================================================================================
// Offset44 decoder kernels, new-style

// Offset_U8 encoding:
//   0x00 <= offs_u8 <= 0xef: "Small". Low 4 bits of offset given by (offs_u8 & 0xf),
//     (offs_u8 >> 4) + OFFSET_RAW_BITS extra bits in secondary stream
//   0xf0 <= offs_u8 <= 0xff: "Large". (offs_u8 - 0xf0) + 16 extra bits in secondary stream.
//
// Small offset encoding: decoded offset is
//   num_extra_bits = (offs_u8 >> 4) + OFFSET_RAW_BITS
//   temp = GetSecondaryBits(num_extra_bits) + (1 << num_extra_bits) // Exp-Golomb style code
//   offset = (offs_u8 & 0xf)	// low 4 bits...
//          + (temp << 4)		// high bits from pseudo-EG code
//          - (1<<(OFFSET_RAW_BITS + 4))	// subtract smallest possible value of (temp<<4) - min value is now 0
//          + NEWLZ_MIN_OFFSET	// add value of smallest encodable offset
//
// Large offset encoding decoded offset is
//   num_extra_bits = (offs_u8 - 0xf0) + 16
//   temp = GetSecondaryBits(num_extra_bits) + (1 << num_extra_bits) // Exp-Golomb style code
//   offset = temp + ESCAPE_OFFSET_BIAS
//
// Legal offstes must be less than NEWLZ_MAX_OFFSET (1<<30).
//
// Therefore, offs_u8 of 0xfe and 0xff is never valid, since it has num_extra_bits >= 30 and
// the (1 << num_extra_bits) term alone puts us past NEWLZ_MAX_OFFSET.

// Offsets_U8 decoding tables
#define NUMRAW_SMALL(x) (((x) >> 4) + OFFSET_RAW_BITS)
#define NUMRAW_LARGE(x) ((x) - 0xf0 + 16)
#define NUMRAW_ALT(x) ((x) >> OFFSET_ALT_NUM_UNDER_BITS)
#define BIAS_SMALL(x) (-(((x) & 0xf) + (1 << (NUMRAW_SMALL(x) + 4)) - (1 << (OFFSET_RAW_BITS + 4)) + NEWLZ_MIN_OFFSET))
#define BIAS_LARGE(x) (-(ESCAPE_OFFSET_BIAS + (1 << NUMRAW_LARGE(x))))
#define BIAS_ALT(x) (OFFSET_ALT_BIAS - ((((x) & ((1 << OFFSET_ALT_NUM_UNDER_BITS) - 1)) | (1 << OFFSET_ALT_NUM_UNDER_BITS)) << NUMRAW_ALT(x)))

#define NUMRAW_SMALL_4X(base) NUMRAW_SMALL(base+0), NUMRAW_SMALL(base+1), NUMRAW_SMALL(base+2), NUMRAW_SMALL(base+3)
#define NUMRAW_SMALL_ROW(base) NUMRAW_SMALL_4X(base+0), NUMRAW_SMALL_4X(base+4), NUMRAW_SMALL_4X(base+8), NUMRAW_SMALL_4X(base+12)
#define BIAS_SMALL_4X(base) BIAS_SMALL(base+0), BIAS_SMALL(base+1), BIAS_SMALL(base+2), BIAS_SMALL(base+3)
#define BIAS_SMALL_ROW(base) BIAS_SMALL_4X(base+0), BIAS_SMALL_4X(base+4), BIAS_SMALL_4X(base+8), BIAS_SMALL_4X(base+12)
#define BIAS_ALT_4X(base) BIAS_ALT(base+0), BIAS_ALT(base+1), BIAS_ALT(base+2), BIAS_ALT(base+3)
#define BIAS_ALT_ROW(base) BIAS_ALT_4X(base+0), BIAS_ALT_4X(base+4), BIAS_ALT_4X(base+8), BIAS_ALT_4X(base+12)
#define REPEAT_8X(x) (x), (x), (x), (x), (x), (x), (x), (x)

static const U8 newlz_offsets44_count_tab[256] =
{
	NUMRAW_SMALL_ROW(0x00),
	NUMRAW_SMALL_ROW(0x10),
	NUMRAW_SMALL_ROW(0x20),
	NUMRAW_SMALL_ROW(0x30),
	NUMRAW_SMALL_ROW(0x40),
	NUMRAW_SMALL_ROW(0x50),
	NUMRAW_SMALL_ROW(0x60),
	NUMRAW_SMALL_ROW(0x70),
	NUMRAW_SMALL_ROW(0x80),
	NUMRAW_SMALL_ROW(0x90),
	NUMRAW_SMALL_ROW(0xa0),
	NUMRAW_SMALL_ROW(0xb0),
	NUMRAW_SMALL_ROW(0xc0),
	NUMRAW_SMALL_ROW(0xd0),
	NUMRAW_SMALL_ROW(0xe0),
	NUMRAW_LARGE(0xf0), NUMRAW_LARGE(0xf1), NUMRAW_LARGE(0xf2), NUMRAW_LARGE(0xf3),
	NUMRAW_LARGE(0xf4), NUMRAW_LARGE(0xf5), NUMRAW_LARGE(0xf6), NUMRAW_LARGE(0xf7),
	NUMRAW_LARGE(0xf8), NUMRAW_LARGE(0xf9), NUMRAW_LARGE(0xfa), NUMRAW_LARGE(0xfb),
	NUMRAW_LARGE(0xfc), NUMRAW_LARGE(0xfd),
	0,0 // 0xfe / 0xff are not legal
};

static const S32 newlz_offsets44_bias_tab[256] =
{
	BIAS_SMALL_ROW(0x00),
	BIAS_SMALL_ROW(0x10),
	BIAS_SMALL_ROW(0x20),
	BIAS_SMALL_ROW(0x30),
	BIAS_SMALL_ROW(0x40),
	BIAS_SMALL_ROW(0x50),
	BIAS_SMALL_ROW(0x60),
	BIAS_SMALL_ROW(0x70),
	BIAS_SMALL_ROW(0x80),
	BIAS_SMALL_ROW(0x90),
	BIAS_SMALL_ROW(0xa0),
	BIAS_SMALL_ROW(0xb0),
	BIAS_SMALL_ROW(0xc0),
	BIAS_SMALL_ROW(0xd0),
	BIAS_SMALL_ROW(0xe0),
	BIAS_LARGE(0xf0), BIAS_LARGE(0xf1), BIAS_LARGE(0xf2), BIAS_LARGE(0xf3),
	BIAS_LARGE(0xf4), BIAS_LARGE(0xf5), BIAS_LARGE(0xf6), BIAS_LARGE(0xf7),
	BIAS_LARGE(0xf8), BIAS_LARGE(0xf9), BIAS_LARGE(0xfa), BIAS_LARGE(0xfb),
	BIAS_LARGE(0xfc), BIAS_LARGE(0xfd),
	-NEWLZ_MAX_OFFSET, -NEWLZ_MAX_OFFSET // 0xfe / 0xff are not legal
};

static const S32 newlz_offsetsalt_bias_tab[256] =
{
	BIAS_ALT_ROW(0x00),	 // numraw=0..1
	BIAS_ALT_ROW(0x10),	 // numraw=2..3
	BIAS_ALT_ROW(0x20),	 // numraw=4..5
	BIAS_ALT_ROW(0x30),	 // numraw=6..7
	BIAS_ALT_ROW(0x40),	 // numraw=8..9
	BIAS_ALT_ROW(0x50),	 // numraw=10..11
	BIAS_ALT_ROW(0x60),	 // numraw=12..13
	BIAS_ALT_ROW(0x70),	 // numraw=14..15
	BIAS_ALT_ROW(0x80),	 // numraw=16..17
	BIAS_ALT_ROW(0x90),	 // numraw=18..19
	BIAS_ALT_ROW(0xa0),	 // numraw=20..21
	BIAS_ALT_ROW(0xb0),	 // numraw=22..23
	BIAS_ALT_ROW(0xc0),	 // numraw=24..25
	BIAS_ALT_4X(0xd0), BIAS_ALT_4X(0xd4), // numraw=26
	REPEAT_8X(-NEWLZ_MAX_OFFSET), // 0xd8..0xdf (numraw=27) are not legal
	REPEAT_8X(-NEWLZ_MAX_OFFSET), // 0xe0..0xe7 (numraw=28) are not legal
	REPEAT_8X(-NEWLZ_MAX_OFFSET), // 0xe8..0xef (numraw=29) are not legal
	REPEAT_8X(-NEWLZ_MAX_OFFSET), // 0xf0..0xf7 (numraw=30) are not legal
	REPEAT_8X(-NEWLZ_MAX_OFFSET)  // 0xf8..0xff (numraw=31) are not legal
};

#undef NUMRAW_SMALL
#undef NUMRAW_LARGE
#undef NUMRAW_ALT
#undef BIAS_SMALL
#undef BIAS_LARGE
#undef BIAS_ALT
#undef NUMRAW_SMALL_4X
#undef NUMRAW_SMALL_ROW
#undef BIAS_SMALL_4X
#undef BIAS_SMALL_ROW
#undef BIAS_ALT_4X
#undef BIAS_ALT_ROW
#undef REPEAT_8X

// only ever actually called in this module with count<32
static inline U32 get_nbit_mask(U32 count)
{
#ifndef __RADX86__
	// Just use math here on archs where variable shifts are fast and easy.
	return ~(~0u << count);
#else
	// x86 has variable shift funkiness that makes this the preferred way.
	static const U32 masks[33] =
	{
		0x00000000,
		0x00000001, 0x00000003, 0x00000007, 0x0000000f,
		0x0000001f, 0x0000003f, 0x0000007f, 0x000000ff,
		0x000001ff, 0x000003ff, 0x000007ff, 0x00000fff,
		0x00001fff, 0x00003fff, 0x00007fff, 0x0000ffff,
		0x0001ffff, 0x0003ffff, 0x0007ffff, 0x000fffff,
		0x001fffff, 0x003fffff, 0x007fffff, 0x00ffffff,
		0x01ffffff, 0x03ffffff, 0x07ffffff, 0x0fffffff,
		0x1fffffff, 0x3fffffff, 0x7fffffff, 0xffffffff,
	};

	return masks[count];
#endif
}

bool newLZ_offset44_decode_finish(KrakenOffsetState * s)
{
	const U8 * offs_u8 = s->offs_u8;
	const U8 * offs_u8_end = s->offs_u8_end;
	S32 * neg_offs_s32 = s->neg_offs_s32;

	//SIMPLEPROFILE_SCOPE_N(offsets44_dec_finish,offs_u8_end-offs_u8);

	static const SINTa kTailSize = 8;
	// tail_buf is zero-init with {}
	U8 tail_buf[kTailSize * 2] = {}; // forward, backward
	SINTa final_in_bytes = 0;

	const U8 * bitp0 = s->bitp[0];
	const U8 * bitp1 = s->bitp[1];
	const U8 * pre_switch_bitp0 = NULL;
	U32 bitc0 = s->bitc[0];
	U32 bitc1 = s->bitc[1];

	// We can have bitp0 > bitp1 if the pointers crossed in the preceding fast-path decoder
	// (which means the data is definitely malformed)
	if (bitp0 > bitp1)
		return false;

	RR_ASSERT(offs_u8 <= offs_u8_end); // can happen even for offsets_count > 0 when preceding decoder handled all offsets.
	RR_ASSERT(bitp0 <= bitp1);
	RR_ASSERT(bitc0 <= 7 && bitc1 <= 7);

	while (offs_u8 != offs_u8_end)
	{
		// Checkpoint input
		// Establishes invariant: bitp1 - bitp0 >= kTailSize
		SINTa bytes_left = bitp1 - bitp0;
		if (bytes_left < kTailSize)
		{
			if (final_in_bytes != 0)
				return false;

			pre_switch_bitp0 = bitp0;

			memcpy(tail_buf, bitp0, bytes_left);
			memcpy(tail_buf + sizeof(tail_buf) - bytes_left, bitp0, bytes_left);
			bitp0 = tail_buf;
			bitp1 = tail_buf + sizeof(tail_buf);
			final_in_bytes = sizeof(tail_buf) - bytes_left;
			RR_ASSERT(final_in_bytes >= kTailSize);
		}

		// Refill
		RR_ASSERT(bitp1 - bitp0 >= 8);

		U64 bits0 = RR_GET64_BE_UNALIGNED(bitp0 + 0);
		U64 bits1 = RR_GET64_LE_UNALIGNED(bitp1 - 8);

		// Decode offset pair
		U8 packed;
		S32 offs;

		#define DECODE_ONE(bits,bitc) \
			packed = *offs_u8++; \
			if (packed < 0xf0) { \
				U32 num_raw = (packed >> 4) + OFFSET_RAW_BITS; /* <=18 */ \
				U32 mask = get_nbit_mask(num_raw); \
				bitc += num_raw; \
				S32 offs_raw_bits = (S32)(bits >> (64 - bitc)) & mask; \
				offs = ((offs_raw_bits + mask) << 4) + (1 << 4) + (packed & 0xf) - (1<<(OFFSET_RAW_BITS + 4)) + NEWLZ_MIN_OFFSET; \
			} else { \
				U32 num_raw = (packed - 0xf0) + 16; /* <=31 */ \
				if (num_raw >= 30) return false; \
				U32 mask = get_nbit_mask(num_raw); \
				bitc += num_raw; \
				S32 offs_raw_bits = (S32)(bits >> (64 - bitc)) & mask; \
				offs = offs_raw_bits + mask + 1 + ESCAPE_OFFSET_BIAS; \
				if (offs >= NEWLZ_MAX_OFFSET) return false; \
			} \
			*neg_offs_s32++ = -offs

		DECODE_ONE(bits0, bitc0);
		if (offs_u8 != offs_u8_end)
		{
			DECODE_ONE(bits1, bitc1);
		}

		#undef DECODE_ONE

		// Advance
		bitp0 += bitc0 >> 3; bitc0 &= 7;
		bitp1 -= bitc1 >> 3; bitc1 &= 7;
	}

	// We do _not_ require that all input be consumed at this point, merely that the
	// forward and backward regions not overlap.
	SINTa final_bytes_left = bitp1 - bitp0;
	final_bytes_left -= (bitc0 > 0); // stream 0 partial byte
	final_bytes_left -= (bitc1 > 0); // stream 1 partial byte
	if (final_bytes_left < final_in_bytes)
		return false;

	// Update state to record the final bit position
	if (final_in_bytes)
	{
		const U8 * pre_switch_bitp1 = pre_switch_bitp0 + (sizeof(tail_buf) - final_in_bytes);
		s->bitp[0] = pre_switch_bitp0 + (bitp0 - tail_buf);
		s->bitp[1] = pre_switch_bitp1 - (tail_buf + sizeof(tail_buf) - bitp1);
	}
	else
	{
		s->bitp[0] = bitp0;
		s->bitp[1] = bitp1;
	}
	s->bitc[0] = bitc0;
	s->bitc[1] = bitc1;

	return true;
}

// 64-bit decoder, table based
bool newLZ_offset44_decode64_tab(KrakenOffsetState * s)
{
	const U8 * offs_u8 = s->offs_u8;
	const U8 * offs_u8_end = s->offs_u8_end;
	SIMPLEPROFILE_SCOPE_N(offsets44_dec64_tab,offs_u8_end-offs_u8);

	if (offs_u8_end - offs_u8 >= 8 && s->bitp[1] - s->bitp[0] >= 8)
	{
		offs_u8_end -= 7;

		S32 * neg_offs_s32 = s->neg_offs_s32;
		const U8 * bitp0 = s->bitp[0];
		const U8 * bitp1 = s->bitp[1] - 8;
		U32 bitc0 = s->bitc[0];
		U32 bitc1 = s->bitc[1];

		#ifdef __RADX64__

		// x86-64: Rotate left is available and the quickest way to get
		// the bits we want into the low bits with just one variable shift.
		#define BIT_GRAB(result,bits,bitc,count) \
			bitc += count; \
			result = (U32)RR_ROTL64(bits, bitc) & get_nbit_mask(count)

		#elif defined(__RADX86__)

		// x86: 64-bit shifts are synthesized from multiple ops, and
		// regular shifts are cheaper, so do one 64-bit variable right
		// shift then mask.
		#define BIT_GRAB(result,bits,bitc,count) \
			bitc += count; \
			result = (U32)(bits >> (64-bitc)) & get_nbit_mask(count)

		#elif defined(__RAD64REGS__)

		// Other 64-bit targets: assume two variable shifts are not
		// a big deal, in which case we can skip the mask table.
		#define BIT_GRAB(result,bits,bitc,count) \
			result = (U32)((bits << bitc) >> (64 - count)); \
			bitc += count

		#else

		// Generic 32-bit targets: Only the first shift needs to be
		// 64 bits, after that grab the high 32 bits and do a 32-bit
		// shift on 32-bit targets.
		#define BIT_GRAB(result,bits,bitc,count) \
			result = (U32)((bits << bitc) >> 32) >> (32 - count); \
			bitc += count

		#endif

		while (offs_u8 < offs_u8_end)
		{
			// Advance
			bitp0 += bitc0 >> 3; bitc0 &= 7;
			bitp1 -= bitc1 >> 3; bitc1 &= 7;

            if (bitp0 > bitp1)
				break;

			#ifdef __RADARM__
			RR_PREFETCHR_CL(bitp0 + 64);
			RR_PREFETCHR_CL(bitp1 - 64);
			RR_PREFETCHR_CL(offs_u8 + 64);
			#endif

			// Bit buffer refill
			U64 bits0 = RR_GET64_BE_UNALIGNED(bitp0);
			U64 bits1 = RR_GET64_LE_UNALIGNED(bitp1);

			// Check that the next 6 offsets are in bounds
			U64 next_offsets = RR_GET64_NATIVE_UNALIGNED(offs_u8);
			U64 next_offsets_hi = next_offsets & 0x808080808080ull;
			U64 splat8 = ~0ull / 255;
			U64 has_large = (0xef*splat8 - next_offsets) & next_offsets_hi;

			if (!has_large)
			{
				#define DECONE(ind,bits,bitc) do { \
					U8 packed = offs_u8[ind]; \
					U32 num_raw = (packed >> 4) + OFFSET_RAW_BITS; \
					U32 offs_raw_bits; \
					BIT_GRAB(offs_raw_bits, bits, bitc, num_raw); \
					S32 neg_offs = newlz_offsets44_bias_tab[packed] - ((S32)offs_raw_bits << 4); \
					neg_offs_s32[ind] = neg_offs; \
					OFFSET_CHECK(&neg_offs_s32[ind]); \
				} while (0)

				DECONE(0,bits0,bitc0);
				DECONE(1,bits1,bitc1);
				DECONE(2,bits0,bitc0);
				DECONE(3,bits1,bitc1);
				DECONE(4,bits0,bitc0);
				DECONE(5,bits1,bitc1);

				#undef DECONE

				offs_u8 += 6;
				neg_offs_s32 += 6;
			}
			else
			{
				U64 has_bad_codes = (0xfd*splat8 - next_offsets) & next_offsets_hi;
				if (has_bad_codes)
					return false;

				#define DECONE(ind,bits,bitc) do { \
					U8 packed = offs_u8[ind]; \
					U32 num_raw = newlz_offsets44_count_tab[packed]; \
					U32 offs_raw_bits; \
					BIT_GRAB(offs_raw_bits, bits, bitc, num_raw); \
					S32 shifted_offs = (S32)offs_raw_bits << 4; \
					S32 neg_offs = newlz_offsets44_bias_tab[packed] - (packed >= 0xf0 ? (S32)offs_raw_bits : shifted_offs); \
					neg_offs_s32[ind] = neg_offs; \
					OFFSET_CHECK(&neg_offs_s32[ind]); \
				} while (0)

				DECONE(0,bits0,bitc0);
				DECONE(1,bits1,bitc1);
				if (bitc0 >= 64-30 || bitc1 >= 64-30)
				{
					offs_u8 += 2;
					neg_offs_s32 += 2;
					continue;
				}

				DECONE(2,bits0,bitc0);
				DECONE(3,bits1,bitc1);
				if (bitc0 >= 64-30 || bitc1 >= 64-30)
				{
					offs_u8 += 4;
					neg_offs_s32 += 4;
					continue;
				}

				DECONE(4,bits0,bitc0);
				DECONE(5,bits1,bitc1);

				#undef DECONE

				offs_u8 += 6;
				neg_offs_s32 += 6;
			}
		}

		#undef BIT_GRAB

		s->offs_u8 = offs_u8;
		s->neg_offs_s32 = neg_offs_s32;

		// Final advance
		bitp0 += bitc0 >> 3; bitc0 &= 7;
		bitp1 -= bitc1 >> 3; bitc1 &= 7;

		s->bitp[0] = bitp0;
		s->bitp[1] = bitp1 + 8;
		s->bitc[0] = bitc0;
		s->bitc[1] = bitc1;
	}

	return newLZ_offset44_decode_finish(s);
}

#ifdef NEWLZ_OFFSET_ARM64_ASM

extern "C" bool oodle_decode_offset44_a64_kern(KrakenOffsetState * s, const S32 * bias_table);

// 64-bit decoder, no large offsets, table based
bool newLZ_offset44_decode_arm64(KrakenOffsetState * s)
{
	const U8 * offs_u8 = s->offs_u8;
	const U8 * offs_u8_end = s->offs_u8_end;
	SIMPLEPROFILE_SCOPE_N(offsets44_dec_arm64,s->offs_u8_end - s->offs_u8);

	if (!oodle_decode_offset44_a64_kern(s, newlz_offsets44_bias_tab))
		return false;

#if NEWLZ_OFFSET_DECODE_DEBUG
	SINTa count = s->neg_offs_s32 - s->neg_offs_s32_base;
	for (SINTa i = 0; i < count; i++)
		RR_ASSERT_ALWAYS(s->neg_offs_s32_base[i] == s->neg_offs_ref_base[i]);
#endif

	return newLZ_offset44_decode_finish(s);
}

#else

// still provide this symbol for test_offsets
bool newLZ_offset44_decode_arm64(KrakenOffsetState * s)
{
	return newLZ_offset44_decode64_tab(s);
}

#endif

#if defined(__RADNEON__)

// This is a quasi-PSHUFB.
static RADFORCEINLINE uint8x16_t general_shuffle_u8(uint8x16_t tbl, uint8x16_t inds)
{
#ifdef __RADARM64__
	// ARMv8/AArch64 just have this
	return vqtbl1q_u8(tbl, inds);
#else
	// original NEON needs to puzzle it together from pieces
	uint8x8x2_t shuf_in;
	shuf_in.val[0] = vget_low_u8(tbl);
	shuf_in.val[1] = vget_high_u8(tbl);
	uint8x8_t lo = vtbl2_u8(shuf_in, vget_low_u8(inds));
	uint8x8_t hi = vtbl2_u8(shuf_in, vget_high_u8(inds));
	return vcombine_u8(lo, hi);
#endif
}

// Returns x[i] & ((1 << count[i]) - 1) + (1 << count[i]);
static RADFORCEINLINE int32x4_t mask_to_width_and_set_top(uint32x4_t x, uint16x4_t counts)
{
	// Determine mask = ~0 << counts[i] = -1 << counts[i],
	// and then return (x & ~mask) - mask.
	uint32x4_t mask = vshlq_u32(vdupq_n_u32(~(U32)0), vreinterpretq_s32_u32(vmovl_u16(counts)));
	return vreinterpretq_s32_u32(vsubq_u32(vbicq_u32(x, mask), mask));
}

bool newLZ_offset44_decode_neon(KrakenOffsetState * s)
{
	const U8 * offs_u8 = s->offs_u8;
	const U8 * offs_u8_end = s->offs_u8_end;
	SIMPLEPROFILE_SCOPE_N(offsets44_dec_neon, offs_u8_end-offs_u8);

	if (offs_u8_end - offs_u8 >= 8 && s->bitp[1] - s->bitp[0] >= 16)
	{
		offs_u8_end -= 7;

		S32 * neg_offs_s32 = s->neg_offs_s32;
		const U8 * bitp0 = s->bitp[0];
		const U8 * bitp1 = s->bitp[1] - 16;

		int32x4_t offs_bias = vdupq_n_s32((1<<(OFFSET_RAW_BITS + 4)) - NEWLZ_MIN_OFFSET);
		int32x4_t escape_bias = vdupq_n_s32(-ESCAPE_OFFSET_BIAS);

		// microsoft style initializer :
		// static const uint8x16_t byte_mask = {.n128_u8={1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128}};
		
		static const U8 bytesub0_init[16] = { 0,1,2,3, 0,1,2,3, 0,1,2,3, 0,1,2,3 };
		static const U8 bytesub1_init[16] = { 15,16,17,18, 15,16,17,18, 15,16,17,18, 15,16,17,18 };

		uint8x16_t bytesub0 = vld1q_u8(bytesub0_init);
		uint8x16_t bytesub1 = vld1q_u8(bytesub1_init);

		uint8x8_t const_0xf0 = vdup_n_u8(0xf0);
		uint8x8_t initial_bit = vcreate_u8(s->bitc[0] | (s->bitc[1] << 8)); // starting bit position within initial byte in lanes 0/1 (rest zero)

		while (offs_u8 < offs_u8_end && bitp0 <= bitp1)
		{
			// Grab the next 8 offset_u8s and split them up
			uint8x8_t offset_u8 = vld1_u8(offs_u8);
			uint8x8_t offset_lo_nib = vbic_u8(offset_u8, const_0xf0);

			uint8x8_t offs_islarge_u8 = vcge_u8(offset_u8, const_0xf0);
			int32x4_t neg_offs0, neg_offs1;

			RR_PREFETCHR_CL(bitp0 + 64);
			RR_PREFETCHR_CL(bitp1 - 64);

			// Work out raw bit counts for small-count case
			uint8x8_t numraw_small = vsra_n_u8(vdup_n_u8(OFFSET_RAW_BITS), offset_u8, 4); // (offset_u8 >> 4) + OFFSET_RAW_BITS

			// Grab source bytes for both streams
			uint8x16_t bytes0 = vld1q_u8(bitp0);
			uint8x16_t bytes1 = vld1q_u8(bitp1);

			// Do we have any large offsets?
			uint8x8_t offs_islarge_u8_padd = vpadd_u8(offs_islarge_u8, offs_islarge_u8);
			if (vget_lane_u32(vreinterpret_u32_u8(offs_islarge_u8_padd), 0) == 0)
			{
				// Faster path: no large offsets

				// Prefix sum to work out end bit for each field
				// we're summing both the forward and backward stream counts here
				uint8x8_t pfx0 = vadd_u8(numraw_small, initial_bit);
				uint8x8_t pfx1 = vadd_u8(pfx0, vreinterpret_u8_u64(vshl_n_u64(vreinterpret_u64_u8(pfx0), 16)));
				uint8x8_t end_bit_index_u8 = vadd_u8(pfx1, vreinterpret_u8_u64(vshl_n_u64(vreinterpret_u64_u8(pfx1), 32)));

				// Work out byte shuffle indices
				uint8x8_t end_byte_index_u8 = vshr_n_u8(end_bit_index_u8, 3);
				end_bit_index_u8 = vand_u8(end_bit_index_u8, vdup_n_u8(7));

				// First create two copies of each byte index
				// then use a transpose op to get the byte shuffle base indices
				uint16x8_t end_byte_index_u8_doubled = vaddw_u8(vshll_n_u8(end_byte_index_u8, 8), end_byte_index_u8); // 0,0,1,1,2,2,3,3,...
				uint16x8x2_t byte_shuffles = vtrnq_u16(end_byte_index_u8_doubled, end_byte_index_u8_doubled);
				uint8x16_t byte_shuffle0 = vsubq_u8(vreinterpretq_u8_u16(byte_shuffles.val[0]), bytesub0);
				uint8x16_t byte_shuffle1 = vsubq_u8(bytesub1, vreinterpretq_u8_u16(byte_shuffles.val[1]));

				// Bit buffer advance
				U32 advance_amts = vget_lane_u16(vreinterpret_u16_u8(end_byte_index_u8), 3);
				initial_bit = vreinterpret_u8_u64(vshr_n_u64(vreinterpret_u64_u8(end_bit_index_u8), 48));
				bitp0 += advance_amts & 0xff;
				bitp1 -= advance_amts >> 8;

				// Shuffle source bytes for both streams
				uint32x4_t dwords0 = vreinterpretq_u32_u8(general_shuffle_u8(bytes0, byte_shuffle0));
				uint32x4_t dwords1 = vreinterpretq_u32_u8(general_shuffle_u8(bytes1, byte_shuffle1));

				// Left shift the source DWords
				int32x4_t end_bit_index_s32 = vreinterpretq_s32_u16(vmovl_u8(end_bit_index_u8));
				int32x4_t left_shift0 = vandq_s32(end_bit_index_s32, vdupq_n_s32(0xffff));
				int32x4_t left_shift1 = vshrq_n_s32(end_bit_index_s32, 16);
				uint32x4_t offs_raw_bits0 = vshrq_n_u32(vshlq_u32(dwords0, left_shift0), 8);
				uint32x4_t offs_raw_bits1 = vshrq_n_u32(vshlq_u32(dwords1, left_shift1), 8);

				// Interleave the forward/backward stream results
				uint32x4x2_t offs_bits = vzipq_u32(offs_raw_bits0, offs_raw_bits1);

				// Compute the masked, biased offsets
				uint16x8_t numraw_u16 = vmovl_u8(numraw_small);
				int32x4_t offs_masked0 = mask_to_width_and_set_top(offs_bits.val[0], vget_low_u16(numraw_u16));
				int32x4_t offs_masked1 = mask_to_width_and_set_top(offs_bits.val[1], vget_high_u16(numraw_u16));

				// Compute the negated offsets
				int16x8_t offset_lo_nib_s16 = vreinterpretq_s16_u16(vmovl_u8(offset_lo_nib));
				neg_offs0 = vsubq_s32(offs_bias, vshlq_n_s32(offs_masked0, 4));
				neg_offs1 = vsubq_s32(offs_bias, vshlq_n_s32(offs_masked1, 4));

				neg_offs0 = vsubw_s16(neg_offs0, vget_low_s16(offset_lo_nib_s16));
				neg_offs1 = vsubw_s16(neg_offs1, vget_high_s16(offset_lo_nib_s16));
			}
			else
			{
				// Slower path with large offset support
				uint8x8_t offs_escape_u8 = vqsub_u8(offset_u8, vdup_n_u8(0xfd)); // after this, >0 iff value >=0xfe
				offs_escape_u8 = vpadd_u8(offs_escape_u8, offs_escape_u8);
				if (vget_lane_u32(vreinterpret_u32_u8(offs_escape_u8), 0) != 0) // any nonzero values?
					return false;

				// Raw bit count for large offsets is easy
				// we compute it as a correction to the numraw count that was computed
				// with the regular formula for a high nibble of 0xf
				uint8x8_t numraw_large_diff = vsub_u8(offset_lo_nib, vdup_n_u8(0xf + OFFSET_RAW_BITS - 16));
				uint8x8_t numraw = vadd_u8(numraw_small, vand_u8(numraw_large_diff, offs_islarge_u8));

				// Prefix sum to work out end bit for each field
				// we're summing both the forward and backward stream counts here
				uint8x8_t pfx0 = vadd_u8(numraw, initial_bit);
				uint8x8_t pfx1 = vadd_u8(pfx0, vreinterpret_u8_u64(vshl_n_u64(vreinterpret_u64_u8(pfx0), 16)));
				uint8x8_t end_bit_index_u8 = vadd_u8(pfx1, vreinterpret_u8_u64(vshl_n_u64(vreinterpret_u64_u8(pfx1), 32)));

				// Work out byte shuffle indices
				uint8x8_t end_byte_index_u8 = vshr_n_u8(end_bit_index_u8, 3);
				end_bit_index_u8 = vand_u8(end_bit_index_u8, vdup_n_u8(7));

				// First create two copies of each byte index
				// then use a transpose op to get the byte shuffle base indices
				uint16x8_t end_byte_index_u8_doubled = vaddw_u8(vshll_n_u8(end_byte_index_u8, 8), end_byte_index_u8); // 0,0,1,1,2,2,3,3,...
				uint16x8x2_t byte_shuffles = vtrnq_u16(end_byte_index_u8_doubled, end_byte_index_u8_doubled);
				uint8x16_t byte_shuffle0 = vsubq_u8(vreinterpretq_u8_u16(byte_shuffles.val[0]), bytesub0);
				uint8x16_t byte_shuffle1 = vsubq_u8(bytesub1, vreinterpretq_u8_u16(byte_shuffles.val[1]));

				// Bit buffer advance
				U32 advance_amts = vget_lane_u16(vreinterpret_u16_u8(end_byte_index_u8), 3);
				initial_bit = vreinterpret_u8_u64(vshr_n_u64(vreinterpret_u64_u8(end_bit_index_u8), 48));
				bitp0 += advance_amts & 0xff;
				bitp1 -= advance_amts >> 8;

				// Shuffle source bytes for both streams
				uint32x4_t dwords0l = vreinterpretq_u32_u8(general_shuffle_u8(bytes0, byte_shuffle0));
				uint32x4_t dwords1l = vreinterpretq_u32_u8(general_shuffle_u8(bytes1, byte_shuffle1));
				uint32x4_t dwords0h = vreinterpretq_u32_u8(general_shuffle_u8(bytes0, vsubq_u8(byte_shuffle0, vdupq_n_u8(1))));
				uint32x4_t dwords1h = vreinterpretq_u32_u8(general_shuffle_u8(bytes1, vaddq_u8(byte_shuffle1, vdupq_n_u8(1))));

				// Left shift the source DWords
				// the 'l' terms here contribute the low byte of every result word (not DWord!) and may have 0s in the high bits
				// the 'h' terms contribute the high byte of every result word and may have 0s in the low bits
				// OR them together and you get all the bits.
				int32x4_t end_bit_index_s32 = vreinterpretq_s32_u16(vmovl_u8(end_bit_index_u8));
				int32x4_t left_shift0 = vandq_s32(end_bit_index_s32, vdupq_n_s32(0xffff));
				int32x4_t left_shift1 = vshrq_n_s32(end_bit_index_s32, 16);
				uint32x4_t offs_raw_bits0 = vshlq_u32(dwords0h, left_shift0);
				uint32x4_t offs_raw_bits1 = vshlq_u32(dwords1h, left_shift1);
				offs_raw_bits0 = vsriq_n_u32(offs_raw_bits0, vshlq_u32(dwords0l, left_shift0), 8);
				offs_raw_bits1 = vsriq_n_u32(offs_raw_bits1, vshlq_u32(dwords1l, left_shift1), 8);

				// Interleave the forward/backward stream results
				uint32x4x2_t offs_bits = vzipq_u32(offs_raw_bits0, offs_raw_bits1);

				// Compute the masked, biased offsets
				uint16x8_t numraw_u16 = vmovl_u8(numraw);
				int32x4_t offs_masked0 = mask_to_width_and_set_top(offs_bits.val[0], vget_low_u16(numraw_u16));
				int32x4_t offs_masked1 = mask_to_width_and_set_top(offs_bits.val[1], vget_high_u16(numraw_u16));

				// Compute the negated offsets
				int16x8_t offset_lo_nib_s16 = vreinterpretq_s16_u16(vmovl_u8(offset_lo_nib));
				int32x4_t neg_offs_small0 = vsubq_s32(offs_bias, vshlq_n_s32(offs_masked0, 4));
				int32x4_t neg_offs_small1 = vsubq_s32(offs_bias, vshlq_n_s32(offs_masked1, 4));

				neg_offs_small0 = vsubw_s16(neg_offs_small0, vget_low_s16(offset_lo_nib_s16));
				neg_offs_small1 = vsubw_s16(neg_offs_small1, vget_high_s16(offset_lo_nib_s16));

				int32x4_t neg_offs_large0 = vsubq_s32(escape_bias, offs_masked0);
				int32x4_t neg_offs_large1 = vsubq_s32(escape_bias, offs_masked1);

				int16x8_t offs_islarge_s16 = vmovl_s8(vreinterpret_s8_u8(offs_islarge_u8));
				uint32x4_t offs_islarge0 = vreinterpretq_u32_s32(vmovl_s16(vget_low_s16(offs_islarge_s16)));
				uint32x4_t offs_islarge1 = vreinterpretq_u32_s32(vmovl_s16(vget_high_s16(offs_islarge_s16)));

				neg_offs0 = vbslq_s32(offs_islarge0, neg_offs_large0, neg_offs_small0);
				neg_offs1 = vbslq_s32(offs_islarge1, neg_offs_large1, neg_offs_small1);
			}

			// Store offsets
			vst1q_s32(neg_offs_s32 + 0, neg_offs0);
			vst1q_s32(neg_offs_s32 + 4, neg_offs1);
			OFFSET_CHECK(&neg_offs_s32[0]);
			OFFSET_CHECK(&neg_offs_s32[1]);
			OFFSET_CHECK(&neg_offs_s32[2]);
			OFFSET_CHECK(&neg_offs_s32[3]);
			OFFSET_CHECK(&neg_offs_s32[4]);
			OFFSET_CHECK(&neg_offs_s32[5]);
			OFFSET_CHECK(&neg_offs_s32[6]);
			OFFSET_CHECK(&neg_offs_s32[7]);

			// Advance output buffer pointers
			offs_u8 += 8;
			neg_offs_s32 += 8;
		}

		s->offs_u8 = offs_u8;
		s->neg_offs_s32 = neg_offs_s32;

		U16 final_bit_offs = vget_lane_u16(vreinterpret_u16_u8(initial_bit), 0);

		s->bitp[0] = bitp0;
		s->bitp[1] = bitp1 + 16;
		s->bitc[0] = final_bit_offs & 7;
		s->bitc[1] = final_bit_offs >> 8;
	}

	return newLZ_offset44_decode_finish(s);
}

#endif

//===============================================================================================

bool newLZ_offsetalt_decode_finish(KrakenOffsetState * s)
{
	const U8 * offs_u8 = s->offs_u8;
	const U8 * offs_u8_end = s->offs_u8_end;
	S32 * neg_offs_s32 = s->neg_offs_s32;

	//SIMPLEPROFILE_SCOPE_N(offsetsalt_dec_finish,offs_u8_end-offs_u8);

	static const SINTa kTailSize = 8;
	// tail_buf is zero init with {}
	U8 tail_buf[kTailSize * 2] = {}; // forward, backward
	SINTa final_in_bytes = 0;

	const U8 * bitp0 = s->bitp[0];
	const U8 * bitp1 = s->bitp[1];
	const U8 * pre_switch_bitp0 = NULL;
	U32 bitc0 = s->bitc[0];
	U32 bitc1 = s->bitc[1];

	// We can have bitp0 > bitp1 if the pointers crossed in the preceding fast-path decoder
	// (which means the data is definitely malformed)
	if (bitp0 > bitp1)
		return false;

	RR_ASSERT(offs_u8 <= offs_u8_end); // can happen even for offsets_count > 0 when preceding decoder handled all offsets.
	RR_ASSERT(bitp0 <= bitp1);
	RR_ASSERT(bitc0 <= 7 && bitc1 <= 7);

	while (offs_u8 != offs_u8_end)
	{
		// Checkpoint input
		// Establishes invariant: bitp1 - bitp0 >= kTailSize
		SINTa bytes_left = bitp1 - bitp0;
		if (bytes_left < kTailSize)
		{
			if (final_in_bytes != 0)
				return false;

			pre_switch_bitp0 = bitp0;

			memcpy(tail_buf, bitp0, bytes_left);
			memcpy(tail_buf + sizeof(tail_buf) - bytes_left, bitp0, bytes_left);
			bitp0 = tail_buf;
			bitp1 = tail_buf + sizeof(tail_buf);
			final_in_bytes = sizeof(tail_buf) - bytes_left;
			RR_ASSERT(final_in_bytes >= kTailSize);
		}

		// Refill
		RR_ASSERT(bitp1 - bitp0 >= 8);

		U64 bits0 = RR_GET64_BE_UNALIGNED(bitp0 + 0);
		U64 bits1 = RR_GET64_LE_UNALIGNED(bitp1 - 8);

		// Decode offset pair
		U8 packed;
		S32 offs;

		#define DECODE_ONE(bits,bitc) do { \
			packed = *offs_u8++; \
			U32 num_raw = packed >> OFFSET_ALT_NUM_UNDER_BITS; \
			if (num_raw > 26) return false; \
			bitc += num_raw; \
			S32 offs_lo_bits = (S32)(bits >> (64 - bitc)) & get_nbit_mask(num_raw); \
			S32 offs_hi_bits = (packed & ((1 << OFFSET_ALT_NUM_UNDER_BITS) - 1)) | (1 << OFFSET_ALT_NUM_UNDER_BITS); \
			offs = offs_lo_bits | (offs_hi_bits << num_raw); \
			*neg_offs_s32++ = OFFSET_ALT_BIAS - offs; \
		} while (0)

		DECODE_ONE(bits0, bitc0);
		if (offs_u8 != offs_u8_end)
		{
			DECODE_ONE(bits1, bitc1);
		}

		#undef DECODE_ONE

		// Advance
		bitp0 += bitc0 >> 3; bitc0 &= 7;
		bitp1 -= bitc1 >> 3; bitc1 &= 7;
	}

	// We do _not_ require that all input be consumed at this point, merely that the
	// forward and backward regions not overlap.
	SINTa final_bytes_left = bitp1 - bitp0;
	final_bytes_left -= (bitc0 > 0); // stream 0 partial byte
	final_bytes_left -= (bitc1 > 0); // stream 1 partial byte
	if (final_bytes_left < final_in_bytes)
		return false;

	// Update state to record the final bit position
	if (final_in_bytes)
	{
		const U8 * pre_switch_bitp1 = pre_switch_bitp0 + (sizeof(tail_buf) - final_in_bytes);
		s->bitp[0] = pre_switch_bitp0 + (bitp0 - tail_buf);
		s->bitp[1] = pre_switch_bitp1 - (tail_buf + sizeof(tail_buf) - bitp1);
	}
	else
	{
		s->bitp[0] = bitp0;
		s->bitp[1] = bitp1;
	}
	s->bitc[0] = bitc0;
	s->bitc[1] = bitc1;

	return true;
}

// 64-bit decoder, table based
bool newLZ_offsetalt_decode64_tab(KrakenOffsetState * s)
{
	const U8 * offs_u8 = s->offs_u8;
	const U8 * offs_u8_end = s->offs_u8_end;
	SIMPLEPROFILE_SCOPE_N(offsetsalt_dec64_tab,offs_u8_end-offs_u8);

	if (offs_u8_end - offs_u8 >= 8 && s->bitp[1] - s->bitp[0] >= 8)
	{
		offs_u8_end -= 7;

		S32 * neg_offs_s32 = s->neg_offs_s32;
		const U8 * bitp0 = s->bitp[0];
		const U8 * bitp1 = s->bitp[1] - 8;
		U32 bitc0 = s->bitc[0];
		U32 bitc1 = s->bitc[1];

		#ifdef __RADX64__

		// x86-64: Rotate left is available and the quickest way to get
		// the bits we want into the low bits with just one variable shift.
		#define BIT_GRAB(result,bits,bitc,count) \
			bitc += count; \
			result = (U32)RR_ROTL64(bits, bitc) & get_nbit_mask(count)

		#else

		// Other targets: use a 64-bit shift.
		// We use the get_nbit_mask form here because we need to
		// handle count=0.
		#define BIT_GRAB(result,bits,bitc,count) \
			bitc += count; \
			result = (U32)(bits >> (64-bitc)) & get_nbit_mask(count)

		#endif

		while (offs_u8 < offs_u8_end)
		{
			// Advance
			bitp0 += bitc0 >> 3; bitc0 &= 7;
			bitp1 -= bitc1 >> 3; bitc1 &= 7;

            if (bitp0 > bitp1)
				break;

			#ifdef __RADARM__
			RR_PREFETCHR_CL(bitp0 + 64);
			RR_PREFETCHR_CL(bitp1 - 64);
			RR_PREFETCHR_CL(offs_u8 + 64);
			#endif

			// Bit buffer refill
			U64 bits0 = RR_GET64_BE_UNALIGNED(bitp0);
			U64 bits1 = RR_GET64_LE_UNALIGNED(bitp1);

			// Check whether the next 6 offsets have <=18 extra bits
			static const U64 splat8 = ~0ull / 255;
			static const U8 first_large_code = 19 << OFFSET_ALT_NUM_UNDER_BITS;

			U64 next_offsets = RR_GET64_LE_UNALIGNED(offs_u8);
			U64 next_offsets_hi = next_offsets & 0x808080808080ull; // MSB of the first 6 bytes
			U64 has_large_codes = ((first_large_code - 1)*splat8 - next_offsets) & next_offsets_hi;
			if (has_large_codes == 0)
			{
				// All short (<=18 bits), can do 3 decodes without refill.
				#define DECONE(ind,bits,bitc) do { \
					U8 packed = (U8)(next_offsets >> ((ind)*8)); \
					U32 num_raw = packed >> OFFSET_ALT_NUM_UNDER_BITS; \
					U32 offs_raw_bits; \
					BIT_GRAB(offs_raw_bits, bits, bitc, num_raw); \
					S32 neg_offs = newlz_offsetsalt_bias_tab[packed] - offs_raw_bits; \
					neg_offs_s32[ind] = neg_offs; \
					OFFSET_CHECK(&neg_offs_s32[ind]); \
				} while (0)

				DECONE(0,bits0,bitc0);
				DECONE(1,bits1,bitc1);
				DECONE(2,bits0,bitc0);
				DECONE(3,bits1,bitc1);
				DECONE(4,bits0,bitc0);
				DECONE(5,bits1,bitc1);

				#undef DECONE

				offs_u8 += 6;
				neg_offs_s32 += 6;
			}
			else
			{
				// Max legal number of bits to consume is 26.
				static const U8 first_illegal_code = 27 << OFFSET_ALT_NUM_UNDER_BITS;
				U64 has_bad_codes = ((first_illegal_code - 1)*splat8 - next_offsets) & next_offsets_hi;
				if (has_bad_codes)
					return false;

				#define DECONE(ind,bits,bitc) do { \
					U8 packed = (U8)(next_offsets >> ((ind)*8)); \
					U32 num_raw = packed >> OFFSET_ALT_NUM_UNDER_BITS; \
					U32 offs_raw_bits; \
					BIT_GRAB(offs_raw_bits, bits, bitc, num_raw); \
					S32 neg_offs = newlz_offsetsalt_bias_tab[packed] - offs_raw_bits; \
					neg_offs_s32[ind] = neg_offs; \
					OFFSET_CHECK(&neg_offs_s32[ind]); \
				} while (0)

				DECONE(0,bits0,bitc0);
				DECONE(1,bits1,bitc1);
				if (bitc0 >= 64-26 || bitc1 >= 64-26)
				{
					offs_u8 += 2;
					neg_offs_s32 += 2;
					continue;
				}

				DECONE(2,bits0,bitc0);
				DECONE(3,bits1,bitc1);
				if (bitc0 >= 64-26 || bitc1 >= 64-26)
				{
					offs_u8 += 4;
					neg_offs_s32 += 4;
					continue;
				}

				DECONE(4,bits0,bitc0);
				DECONE(5,bits1,bitc1);

				#undef DECONE

				offs_u8 += 6;
				neg_offs_s32 += 6;
			}
		}

		#undef BIT_GRAB

		s->offs_u8 = offs_u8;
		s->neg_offs_s32 = neg_offs_s32;

		// Final advance
		bitp0 += bitc0 >> 3; bitc0 &= 7;
		bitp1 -= bitc1 >> 3; bitc1 &= 7;

		s->bitp[0] = bitp0;
		s->bitp[1] = bitp1 + 8;
		s->bitc[0] = bitc0;
		s->bitc[1] = bitc1;
	}

	return newLZ_offsetalt_decode_finish(s);
}

#ifdef NEWLZ_OFFSET_ARM64_ASM

extern "C" bool oodle_decode_offsetalt_a64_kern(KrakenOffsetState * s, const S32 * bias_table);

// 64-bit decoder, no large offsets, table based
bool newLZ_offsetalt_decode_arm64(KrakenOffsetState * s)
{
	const U8 * offs_u8 = s->offs_u8;
	const U8 * offs_u8_end = s->offs_u8_end;
	SIMPLEPROFILE_SCOPE_N(offsetsalt_dec_arm64,s->offs_u8_end - s->offs_u8);

	if (!oodle_decode_offsetalt_a64_kern(s, newlz_offsetsalt_bias_tab))
		return false;

#if NEWLZ_OFFSET_DECODE_DEBUG
	SINTa count = s->neg_offs_s32 - s->neg_offs_s32_base;
	for (SINTa i = 0; i < count; i++)
		RR_ASSERT_ALWAYS(s->neg_offs_s32_base[i] == s->neg_offs_ref_base[i]);
#endif

	return newLZ_offsetalt_decode_finish(s);
}

#else

// still provide this symbol for test_offsets
bool newLZ_offsetalt_decode_arm64(KrakenOffsetState * s)
{
	return newLZ_offsetalt_decode64_tab(s);
}

#endif

#if defined(__RADNEON__)

static RADFORCEINLINE uint8x8_t narrow_shuffle_u8(uint8x16_t tbl, uint8x8_t inds)
{
	uint8x8x2_t shuf_in;
	shuf_in.val[0] = vget_low_u8(tbl);
	shuf_in.val[1] = vget_high_u8(tbl);
	return vtbl2_u8(shuf_in, inds);
}

bool newLZ_offsetalt_decode_neon(KrakenOffsetState * s)
{
	const U8 * offs_u8 = s->offs_u8;
	const U8 * offs_u8_end = s->offs_u8_end;
	SIMPLEPROFILE_SCOPE_N(offsetsalt_dec_neon, offs_u8_end-offs_u8);

	if (offs_u8_end - offs_u8 >= 8 && s->bitp[1] - s->bitp[0] >= 16)
	{
		offs_u8_end -= 7;

		S32 * neg_offs_s32 = s->neg_offs_s32;
		const U8 * bitp0 = s->bitp[0];
		const U8 * bitp1 = s->bitp[1] - 16;

		uint8x16_t bytesub0 = vreinterpretq_u8_u32(vdupq_n_u32(0x03020100));
		uint8x16_t bytesub1 = vreinterpretq_u8_u32(vdupq_n_u32(0x1211100f));
		uint8x8_t nbytesub0 = vreinterpret_u8_u16(vdup_n_u16(0x0100));
		uint8x8_t nbytesub1 = vreinterpret_u8_u16(vdup_n_u16(0x100f));
		uint8x8_t numraw_bits_mask = vdup_n_u8(0x100 - (1 << OFFSET_ALT_NUM_UNDER_BITS));
		uint8x8_t initial_bit = vcreate_u8(s->bitc[0] | (s->bitc[1] << 8)); // starting bit position within initial byte in lanes 0/1 (rest zero)

		while (offs_u8 < offs_u8_end && bitp0 <= bitp1)
		{
			// Grab the next 8 offset_u8s
			uint8x8_t offset_u8 = vld1_u8(offs_u8);
			uint8x8_t numraw = vshr_n_u8(offset_u8, OFFSET_ALT_NUM_UNDER_BITS);
			uint8x8_t numraw_plus_initial = vadd_u8(numraw, initial_bit);

			// Grab source bytes for both streams
			uint8x16_t bytes0 = vld1q_u8(bitp0);
			uint8x16_t bytes1 = vld1q_u8(bitp1);

			RR_PREFETCHR_CL(bitp0 + 64);
			RR_PREFETCHR_CL(bitp1 - 64);

			// Prefix sum to work out end bit for each field
			// we're summing both the forward and backward stream counts here
			uint8x8_t pfx0 = vadd_u8(numraw_plus_initial, vreinterpret_u8_u64(vshl_n_u64(vreinterpret_u64_u8(numraw_plus_initial), 16)));
			uint8x8_t end_bit_index_u8 = vadd_u8(pfx0, vreinterpret_u8_u64(vshl_n_u64(vreinterpret_u64_u8(pfx0), 32)));

			// Work out byte shuffle indices
			uint8x8_t end_byte_index_u8 = vshr_n_u8(end_bit_index_u8, 3);
			end_bit_index_u8 = vand_u8(end_bit_index_u8, vdup_n_u8(7));

			// Check if any of the counts are out of range
			uint8x8_t numraw_isbad_u8 = vcgt_u8(numraw, vdup_n_u8(26));
			uint8x8_t numraw_isbad_u8_padd = vpadd_u8(numraw_isbad_u8, numraw_isbad_u8);
			if (vget_lane_u32(vreinterpret_u32_u8(numraw_isbad_u8_padd), 0) != 0) // any nonzero values?
				return false;

			// Bit buffer advance
			U32 advance_amts = vget_lane_u16(vreinterpret_u16_u8(end_byte_index_u8), 3);
			initial_bit = vreinterpret_u8_u64(vshr_n_u64(vreinterpret_u64_u8(end_bit_index_u8), 48));
			bitp0 += advance_amts & 0xff;
			bitp1 -= advance_amts >> 8;

			// Check whether we can use the narrower read path
			uint32x4x2_t offs_bits;
			U64 offs_u8_int = RR_GET64_NATIVE_UNALIGNED(offs_u8);
			U64 splat8 = ~0ull / 255;
			if (offs_u8_int & (0x80 * splat8))
			{
				// There are large offsets (>15 extra bits)

				// First create two copies of each byte index
				// then use a transpose op to get the byte shuffle base indices
				uint16x8_t end_byte_index_u8_doubled = vaddw_u8(vshll_n_u8(end_byte_index_u8, 8), end_byte_index_u8); // 0,0,1,1,2,2,3,3,...
				uint16x8x2_t byte_shuffles = vtrnq_u16(end_byte_index_u8_doubled, end_byte_index_u8_doubled);
				uint8x16_t byte_shuffle0 = vsubq_u8(vreinterpretq_u8_u16(byte_shuffles.val[0]), bytesub0);
				uint8x16_t byte_shuffle1 = vsubq_u8(bytesub1, vreinterpretq_u8_u16(byte_shuffles.val[1]));

				// Shuffle source bytes for both streams
				uint32x4_t dwords0l = vreinterpretq_u32_u8(general_shuffle_u8(bytes0, byte_shuffle0));
				uint32x4_t dwords1l = vreinterpretq_u32_u8(general_shuffle_u8(bytes1, byte_shuffle1));
				uint32x4_t dwords0h = vreinterpretq_u32_u8(general_shuffle_u8(bytes0, vsubq_u8(byte_shuffle0, vdupq_n_u8(1))));
				uint32x4_t dwords1h = vreinterpretq_u32_u8(general_shuffle_u8(bytes1, vaddq_u8(byte_shuffle1, vdupq_n_u8(1))));

				// Left shift the source DWords
				// the 'l' terms here contribute the low byte of every result lane and may have 0s in the high bits
				// the 'h' terms contribute the high byte of every result lane and may have 0s in the low bits
				// OR them together and you get all the bits.
				int32x4_t end_bit_index_s32 = vreinterpretq_s32_u16(vmovl_u8(end_bit_index_u8));
				int32x4_t left_shift0 = vandq_s32(end_bit_index_s32, vdupq_n_s32(0xffff));
				int32x4_t left_shift1 = vshrq_n_s32(end_bit_index_s32, 16);
				uint32x4_t offs_raw_bits0 = vshlq_u32(dwords0h, left_shift0);
				uint32x4_t offs_raw_bits1 = vshlq_u32(dwords1h, left_shift1);
				offs_raw_bits0 = vsriq_n_u32(offs_raw_bits0, vshlq_u32(dwords0l, left_shift0), 8);
				offs_raw_bits1 = vsriq_n_u32(offs_raw_bits1, vshlq_u32(dwords1l, left_shift1), 8);

				// Interleave the forward/backward stream results
				offs_bits = vzipq_u32(offs_raw_bits0, offs_raw_bits1);
			}
			else
			{
				// All offsets <=15 extra bits

				// The NEON impls we care about here are mostly 64-bit wide, so there's not a lot of sense
				// in doing a full 16-wide decode like the SSE4 short offset path does; instead, exploit
				// that more of the calculation can be done on narrow vectors.

				uint8x8x2_t byte_shuffles = vtrn_u8(end_byte_index_u8, end_byte_index_u8); // .val[0] has even lanes doubled, .vall[1] has odd lanes doubled
				uint8x8_t byte_shuffle0 = vsub_u8(byte_shuffles.val[0], nbytesub0);
				uint8x8_t byte_shuffle1 = vsub_u8(nbytesub1, byte_shuffles.val[1]);

				// Shuffle source bytes from both streams
				uint16x4_t words0l = vreinterpret_u16_u8(narrow_shuffle_u8(bytes0, byte_shuffle0));
				uint16x4_t words1l = vreinterpret_u16_u8(narrow_shuffle_u8(bytes1, byte_shuffle1));
				uint16x4_t words0h = vreinterpret_u16_u8(narrow_shuffle_u8(bytes0, vsub_u8(byte_shuffle0, vdup_n_u8(1))));
				uint16x4_t words1h = vreinterpret_u16_u8(narrow_shuffle_u8(bytes1, vadd_u8(byte_shuffle1, vdup_n_u8(1))));

				// Combine as above, only this time we only have words
				int16x4_t end_bit_index_s16 = vreinterpret_s16_u8(end_bit_index_u8);
				int16x4_t left_shift0 = vand_s16(end_bit_index_s16, vdup_n_s16(0xff));
				int16x4_t left_shift1 = vshr_n_s16(end_bit_index_s16, 8);
				uint16x4_t offs_raw_bits0 = vshl_u16(words0h, left_shift0);
				uint16x4_t offs_raw_bits1 = vshl_u16(words1h, left_shift1);
				offs_raw_bits0 = vsri_n_u16(offs_raw_bits0, vshl_u16(words0l, left_shift0), 8);
				offs_raw_bits1 = vsri_n_u16(offs_raw_bits1, vshl_u16(words1l, left_shift1), 8);

				// Interleave then widen
				uint16x4x2_t offs_interleaved = vzip_u16(offs_raw_bits0, offs_raw_bits1);
				offs_bits.val[0] = vmovl_u16(offs_interleaved.val[0]);
				offs_bits.val[1] = vmovl_u16(offs_interleaved.val[1]);
			}

			// Compute the bit count masks and insert the high bits above the masked low bits
			uint16x8_t numraw_u16 = vmovl_u8(numraw);
			int32x4_t numraw_s32_0 = vreinterpretq_s32_u32(vmovl_u16(vget_low_u16(numraw_u16)));
			int32x4_t numraw_s32_1 = vreinterpretq_s32_u32(vmovl_u16(vget_high_u16(numraw_u16)));

			uint8x8_t hibits_u8 = vsub_u8(vbic_u8(offset_u8, numraw_bits_mask), numraw_bits_mask);
			int16x8_t hibits_s16 = vreinterpretq_s16_u16(vmovl_u8(hibits_u8));
			int32x4_t hibits0 = vshlq_s32(vmovl_s16(vget_low_s16(hibits_s16)), numraw_s32_0);
			int32x4_t hibits1 = vshlq_s32(vmovl_s16(vget_high_s16(hibits_s16)), numraw_s32_1);

			uint32x4_t count_mask0 = vshlq_u32(vdupq_n_u32(~0u), numraw_s32_0); // 1 bits for the high part, 0 for the low part
			uint32x4_t count_mask1 = vshlq_u32(vdupq_n_u32(~0u), numraw_s32_1);
			int32x4_t offs0 = vbslq_s32(count_mask0, hibits0, vreinterpretq_s32_u32(offs_bits.val[0]));
			int32x4_t offs1 = vbslq_s32(count_mask1, hibits1, vreinterpretq_s32_u32(offs_bits.val[1]));

			// Compute the final negated offsets (taking care of the bias along the way)
			int32x4_t neg_offs0 = vsubq_s32(vdupq_n_s32(OFFSET_ALT_BIAS), offs0);
			int32x4_t neg_offs1 = vsubq_s32(vdupq_n_s32(OFFSET_ALT_BIAS), offs1);

			// Store offsets
			vst1q_s32(neg_offs_s32 + 0, neg_offs0);
			vst1q_s32(neg_offs_s32 + 4, neg_offs1);
			OFFSET_CHECK(&neg_offs_s32[0]);
			OFFSET_CHECK(&neg_offs_s32[1]);
			OFFSET_CHECK(&neg_offs_s32[2]);
			OFFSET_CHECK(&neg_offs_s32[3]);
			OFFSET_CHECK(&neg_offs_s32[4]);
			OFFSET_CHECK(&neg_offs_s32[5]);
			OFFSET_CHECK(&neg_offs_s32[6]);
			OFFSET_CHECK(&neg_offs_s32[7]);

			// Advance output buffer pointers
			offs_u8 += 8;
			neg_offs_s32 += 8;
		}

		s->offs_u8 = offs_u8;
		s->neg_offs_s32 = neg_offs_s32;

		U16 final_bit_offs = vget_lane_u16(vreinterpret_u16_u8(initial_bit), 0);

		s->bitp[0] = bitp0;
		s->bitp[1] = bitp1 + 16;
		s->bitc[0] = final_bit_offs & 7;
		s->bitc[1] = final_bit_offs >> 8;
	}

	return newLZ_offsetalt_decode_finish(s);
}

#endif // __RADNEON__

//===============================================================================================
// GET :


// returns number of excesses (>=0) for success,
// -1 for failure.
//
// vb1 is the forward stream, vb2 is backward. expected_count is either 0 (in which case there is no particular expectation,
// used for the new format) or the signaled excess32_count from the bitstream (old format); if it's non-0, then having a
// different count is an error.
//
// There need to be at least 4 writeable locations at excesses_end before the actual end of the underlying
// storage (because or buffer end checks are only done every 4 decodes).
//
// Decodes both streams until they're exhausted so vb1/vb2 are *not* updated on completion; on success, they must have met,
// so vb1/vb2 are not updated.
SINTa newLZ_get_excesses(rrVarBits * vb1, rrVarBits * vb2, U32 * excesses_base, U32 * excesses_end, SINTa expected_count)
{
	SIMPLEPROFILE_SCOPE(excess_u32_decode);

	rrVarBits_Temps();
	rrVarBits_Locals(lvb1);
	rrVarBits_Locals(lvb2);
	rrVarBits_Copy(lvb1,vb1->m);
	rrVarBits_Copy(lvb2,vb2->m);

	// CB changed 12-6 : does refills before gets now (instead of after)
	//	does not assume that passed in vb's have been refilled
	
	const U8 * vb1_init = rrVarBits_GetEndPtr(lvb1);
	const U8 * vb2_init = rrVarBitsBack_GetEndPtr(lvb2);

	REQUIRE_FUZZ_RETURN( vb1_init <= vb2_init , -1 );
		
	U32 * excesses_ptr = excesses_base;

	#define EXCESS_MAX_BYTES_PER_LOOP (32) // 4 refills (2 per stream)
	
	// As long as the pointers are far away from crossing
	while ( rrPtrDiff(lvb2_cur - lvb1_cur) > EXCESS_MAX_BYTES_PER_LOOP )
	{
		// More excesses than permitted! -> Bad stream.
		if ( excesses_ptr > excesses_end )
			return -1;

		// "Only" unrolling both streams twice here, let's not get carried away...
		RR_VARBITSTYPE excess1, excess2;

		rrVarBits_Refill_Unsafe(lvb1);
		rrVarBits_RefillBack_Unsafe(lvb2);
		
		VARBITS_GET_EXPGOLOMB(lvb1,EXCESS_EXPGOLOMB_SHIFT,excess1,rrVarBits_Refill_Unsafe,0);
		VARBITS_GET_EXPGOLOMB(lvb2,EXCESS_EXPGOLOMB_SHIFT,excess2,rrVarBits_RefillBack_Unsafe,0);

		*excesses_ptr++ = (U32)excess1;
		*excesses_ptr++ = (U32)excess2;

		rrVarBits_Refill_Unsafe(lvb1);
		rrVarBits_RefillBack_Unsafe(lvb2);
		
		VARBITS_GET_EXPGOLOMB(lvb1,EXCESS_EXPGOLOMB_SHIFT,excess1,rrVarBits_Refill_Unsafe,0);
		VARBITS_GET_EXPGOLOMB(lvb2,EXCESS_EXPGOLOMB_SHIFT,excess2,rrVarBits_RefillBack_Unsafe,0);

		*excesses_ptr++ = (U32)excess1;
		*excesses_ptr++ = (U32)excess2;
	}

	// tail_buf is zero init with {}
	#define EXCESS_OVERREAD_PAD_SPACE (8) // one refill
	#define EXCESS_OVERREAD_PRE_PAD (2*EXCESS_OVERREAD_PAD_SPACE) // two refills worth - see explanation below
	U8 tail_buf[EXCESS_OVERREAD_PRE_PAD + EXCESS_MAX_BYTES_PER_LOOP + 2*EXCESS_OVERREAD_PAD_SPACE] = {}; // pre-pad, pad, payload, pad - init so we read 0s on over-read

	// make buffers safe
	// need to be careful here: lvb1_cur and lvb2_cur might have already advanced past the
	// respective ends of their bit streams (due to look-ahead), so we can't just copy the
	// range in between lvb1_cur and lvb2_cur; need a few bytes off either end.
	{
		// NOTE this can be negative!! (if there were only a handful of bytes to begin
		// with lvb1/lvb2 had already advanced past each other)
		SINTa rem_bytes = rrPtrDiff(lvb2_cur - lvb1_cur);

		// how low can rem_bytes get?
		// suppose the excess stream is 0 bytes long (okay, then we wouldn't be here, but let's pretend)
		// lvb1 and lvb2 were both refilled, so are up to 8 bytes past the end
		// -> lvb1 can be at +8 bytes, lvb2 can be at -8 bytes
		// -> rem_bytes can get as low as -16 (!!)
		//
		// and because we init lvb2_cur to newf + rem_bytes below, we need to make sure that
		// we have 16 bytes of extra pre-padding (this is PRE_PAD) in front, then the regular
		// refill padding after that (since we might still refill from this position).

		RR_ASSERT( rem_bytes <= EXCESS_MAX_BYTES_PER_LOOP );
		RR_ASSERT( lvb1_cur >= vb1_init );
		RR_ASSERT( vb2_init >= lvb2_cur );
		SINTa head_pad = RR_MIN( lvb1_cur - vb1_init, EXCESS_OVERREAD_PAD_SPACE );
		SINTa tail_pad = RR_MIN( vb2_init - lvb2_cur, EXCESS_OVERREAD_PAD_SPACE );
		U8 * newf = tail_buf + EXCESS_OVERREAD_PRE_PAD + EXCESS_OVERREAD_PAD_SPACE;

		RR_ASSERT( rem_bytes + head_pad + tail_pad >= 0 );

		memcpy(newf - head_pad, lvb1_cur - head_pad, rem_bytes + head_pad + tail_pad);
		lvb1_cur = newf;
		lvb2_cur = newf + rem_bytes;
	}

	const U8 * vb1_consumed = rrVarBits_GetEndPtr(lvb1);
	const U8 * vb2_consumed = rrVarBitsBack_GetEndPtr(lvb2);
	RR_ASSERT( vb1_consumed <= vb2_consumed ); // loop condition was meant to ensure this

	// Keep reading Exp-Golomb code words one by one until we're past the point where
	// the pointers cross. This works because our output padding at the end of legal
	// streams is with zeros, so whatever comes after the last legal code in a stream
	// is guaranteed to read all-zero bits from the tail end of the last byte of its
	// respective stream; the next 1 bit, if any, has to be past the point where the
	// pointers cross, which guarantees that we consume at least that many bits and the
	// "pointers crossed" check will fire.
	//
	// We do, however, have to be careful with the input validation: we can't just
	// blindly return an error when an over-long string of 0s is detected, because this
	// might now occur in a legal stream, but only at the very end, as the last
	// not-really-a-code that pushes us past the cross-over point.

	// VARBITS_GET_EXPGOLOMB returns an error on too-small inputs, we need to be a bit more careful
	// because we read arbitrary data at the end
	static const U32 max_leading_zeros = 18 - EXCESS_EXPGOLOMB_SHIFT;
	// minval is the smallest integer that has max_leading_zeros leading zeros; anything below
	// it has more.
	RR_VARBITSTYPE expg_minval = ((RR_VARBITSTYPE)1 << (RR_VARBITSTYPELEN - 1 - max_leading_zeros));

	for(;;)
	{
		// More excesses than permitted! -> Bad stream.
		if ( excesses_ptr > excesses_end )
			return -1;

		RR_VARBITSTYPE excess;

		rrVarBits_Refill_Unsafe(lvb1);
		
		if ( rrVarBits_Bits(lvb1) < expg_minval )
		{
			// this is an illegal code unless it puts us past the "streams crossed" marker
			rrVarBits_Use(lvb1, max_leading_zeros + 1);
			vb1_consumed = rrVarBits_GetEndPtr(lvb1);
			if ( vb1_consumed > vb2_consumed )
				break;
			else
				return -1;
		}

		// the check
		//if ( rrVarBits_Bits(lvb1) < expg_minval )
		// is now done again in VARBITS_GET_EXPGOLOMB
		// but will never be true (we wanted the handler above to get it)
		
		VARBITS_GET_EXPGOLOMB(lvb1,EXCESS_EXPGOLOMB_SHIFT,excess,rrVarBits_Refill_Unsafe,0);
		vb1_consumed = rrVarBits_GetEndPtr(lvb1);

		// Pointers have collided -> we're done!
		// (== is allowed, we run until we have an actual collision.)
		if ( vb1_consumed > vb2_consumed )
			break;

		*excesses_ptr++ = (U32)excess;

		rrVarBits_RefillBack_Unsafe(lvb2);
		
		if ( rrVarBits_Bits(lvb2) < expg_minval )
		{
			// this is an illegal code unless it puts us past the "streams crossed" marker
			rrVarBits_Use(lvb2, max_leading_zeros + 1);
			vb2_consumed = rrVarBitsBack_GetEndPtr(lvb2);
			if ( vb1_consumed > vb2_consumed )
				break;
			else
				return -1;
		}

		VARBITS_GET_EXPGOLOMB(lvb2,EXCESS_EXPGOLOMB_SHIFT,excess,rrVarBits_RefillBack_Unsafe,0);
		vb2_consumed = rrVarBitsBack_GetEndPtr(lvb2);

		// Pointers have collided -> we're done!
		if ( vb1_consumed > vb2_consumed )
			break;

		*excesses_ptr++ = (U32)excess;
	}

	// Final validation
	if ( excesses_ptr > excesses_end )
		return -1;

	SINTa actual_count = rrPtrDiff( excesses_ptr - excesses_base );
	if ( expected_count && expected_count != actual_count )
		return -1;

	return actual_count;
}
		
// newLZ_get_offsets_excesses
//	returns >= 0 for success
//	returns -1 for failure	
S32 newLZ_get_offsets_excesses(const U8 * const comp_ptr, const U8 * const chunk_comp_end,
	const U8 * offsets_u8, const U8 * offsets_u8_2, SINTa offsets_count, U32 offset_alt_modulo, 
	const U8 * excesses_u8, SINTa excesses_count,
	S32 * offsets, U32 * excesses,
	SINTa end_of_chunk_pos,
	U8 excess_hdr_byte, S32 excess_stream_bytes)
{
	rrPrintf_v2("newLZ_get_offsets_excesses total : %d\n",(int)(chunk_comp_end - comp_ptr));
	
	// should be >= 1 byte :
	//	(unnecessary extra check, is protected below already)
	// NOTE: not anymore! new-format excesses can have 0-byte offsets_excesses chunks if neither
	// offsets nor excess_u32s are present
	if ( comp_ptr >= chunk_comp_end && excess_hdr_byte == 0x00 )
	{
		return -1;
	}

	/***
	
	note about fuzz safety :
	MAKE_FRONTBACK_SAFE :
	
	done before each read loop
	
	once you call MAKE_FRONTBACK_SAFE, you can do 4 refills each of front & back varbits
	with no risk of overrun
	
	so that's 32 bytes back from tail and 32 bytes forward from head
	
	works by copying into a scratch buffer with enough pad space for one loop of overread
	
	the first time front & back pointer are close enough to be dangerous, the copy into scratch is done
	any further times just check for overrun danger
	
	on corrupt data, over-read is allowed within the scratch space
	but won't go outside of scratch
	and will be detected
	
	-----------------------
	
	An optimization that could be done here is to detect when the entire chunk has no escape offsets
	then no escape check needs to be done at all
	however you still have to be careful about escape offset codes appearing due to fuzz safety
	
	NOTE : (not currently used)
	
	all offsets that I decode should be < end_of_chunk_pos
	(if data is not corrupted)
	
	if end_of_chunk_pos <= ESCAPE_OFFSET_MIN
	then all offsets are < ESCAPE_OFFSET_MIN
	so no escapes can occur
	
	so there should be no U8 offset codes in the >= 0xF0 region
	
	if, due to corruption, any do occur
	and you aren't checking for the escape case
	then they can still just be unpacked as numbits=15 and 4 bottom bits
	
	the only funny thing about this is that the bit buffer uses the knowledge that
	to get 3 offsets can read (numbits+4)*3 bits
	with numbits <= 14 , this is 18*3 = 54 , which is <= 56 for the 64-bit refill
	if numbits=15 is allowed, then 19*3 = 57 is too much
	
	ALSO NOTE :
	
	when the offset u8's come from a huff array
	topSym is known
	so if no values are >= 0xF0
	then the escape check could be skipped
	
	(if offset_u8's come from a raw array that can't be used)
	
	---------------
	
	***/

	
	/**
	
	NOTE : ptr_start/ptr_end/lvb1_cur/lvb2_cur can be changed by MAKE_FRONTBACK_SAFE
	- they may point into the "safe_space" scratch array
	so you cannot use them to measure bytes consumed or compare them back to comp_ptr/chunk_comp_end
		
	**/
	
	if ( excess_stream_bytes > rrPtrDiff( chunk_comp_end - comp_ptr ) )
	{
		ooLogError("corruption : excess_stream_bytes > comp_bytes\n");
		return -1;
	}

	/*
	rrprintfvar(offsets_count);
	rrprintfvar(excesses_count);
	rrprintfvar(excess_stream_bytes);
	SINTa varbits_comp_len = chunk_comp_end - comp_ptr;
	rrprintfvar(varbits_comp_len);
	/**/
	
#undef FRONTBACK_HALF_BYTES_PER_LOOP
#define FRONTBACK_HALF_BYTES_PER_LOOP	(32)  // 4 refills

	FRONTBACK_SAFE_VARS();
	const U8 * ptr_start;
	const U8 * ptr_end;

	rrVarBits_Temps();
	rrVarBits_Locals(lvb1);
	rrVarBits_Locals(lvb2);
	
	RR_VARBITSTYPE excess_u32_count = 0;
	
	#ifdef SPEEDFITTING

	for(int speedfit_iter=0;;speedfit_iter++)
	{
	U64 t1 = speedfitter_ticks_start();
	U64 t2 = t1; // to make sure we set it
	
	#endif
	
	RESET_FRONTBACK_SAFE_VARS(comp_ptr,chunk_comp_end);
	ptr_start = comp_ptr;
	ptr_end = chunk_comp_end-excess_stream_bytes;
	
	rrVarBits_GetOpen_NoRefill(lvb1,ptr_start,ptr_end);
	rrVarBits_GetOpen_NoRefill(lvb2,ptr_end,ptr_end);
	
	MAKE_FRONTBACK_SAFE(lvb1_cur,lvb2_cur);
	
	rrVarBits_Refill_Unsafe(lvb1);
	rrVarBits_RefillBack_Unsafe(lvb2);

	if ( excess_hdr_byte == 0 )
	{
		// old encoding has excess_u32_count at the start of vb2 stream
		VARBITS_GET_EXPGOLOMB(lvb2,EXCESS_U32_COUNT_EXPGOLOMB_SHIFT,excess_u32_count,rrVarBits_RefillBack_Unsafe,0);

		// not needed again, the above one covers me
		//MAKE_FRONTBACK_SAFE(lvb1_cur,lvb2_cur);
		rrVarBits_RefillBack_Unsafe(lvb2);
	}
	
	#ifdef CHECK_BITS
	if ( 1 )
	{
		RR_VARBITSTYPE c1 = rrVarBits_Get_C(lvb1,CHECK_BITS);
		RR_VARBITSTYPE c2 = rrVarBits_Get_C(lvb2,CHECK_BITS);
		RR_ASSERT_ALWAYS( c1 == CHECK_VAL );
		RR_ASSERT_ALWAYS( c2 == CHECK_VAL );
	
		MAKE_FRONTBACK_SAFE(lvb1_cur,lvb2_cur);
		
		rrVarBits_Refill_Unsafe(lvb1);
		rrVarBits_RefillBack_Unsafe(lvb2);
	}
	#endif
	
	//rrprintf("offsets_count : %d , excess_count : %d\n",offsets_count,excess_count);
	
	
	if ( offsets_count > 0 )
	{
		KrakenOffsetState s; // all fields written here
		s.offs_u8 = offsets_u8;
		s.offs_u8_end = offsets_u8 + offsets_count;
		s.neg_offs_s32 = offsets;

		// Translate rrVarBits state to bit extract form
		SINTa len1 = (SINTa) rrVarBits_BitLen(lvb1);
		SINTa len2 = (SINTa) rrVarBits_BitLen(lvb2);
		s.bitp[0] = lvb1_cur - ((len1 + 7) >> 3);
		s.bitp[1] = lvb2_cur + ((len2 + 7) >> 3);
		s.bitc[0] = (U32) ((0 - len1) & 7);
		s.bitc[1] = (U32) ((0 - len2) & 7);

		bool decode_ok = false;

		#if NEWLZ_OFFSET_DECODE_DEBUG
		S32 * ref_offs = new S32[offsets_count + 8]; // some padding
		s.neg_offs_s32_base = offsets;
		s.neg_offs_ref_base = ref_offs;
		#else
		// zero init to be sure
		s.neg_offs_s32_base = NULL;
		s.neg_offs_ref_base = NULL;
		#endif

		if ( offset_alt_modulo == 0 )
		{
			SIMPLEPROFILE_SCOPE_N(offset44_decode,offsets_count);

			#if NEWLZ_OFFSET_DECODE_DEBUG
			// decode offsets via ref decoder first
			KrakenOffsetState ref_s = s;
			ref_s.neg_offs_s32 = ref_offs;
			newLZ_offset44_decode_finish(&ref_s);
			#endif

			// Run the decoder kernel
			#if defined(__RADX86__)

			if ( rrsimd_has_avx2() )
				decode_ok = newLZ_offset44_decode_avx2(&s);
			else if ( rrsimd_has_sse4() )
				decode_ok = newLZ_offset44_decode_sse4(&s);
			else
				decode_ok = newLZ_offset44_decode64_tab(&s);

			#elif defined(NEWLZ_OFFSET_ARM64_ASM)

			decode_ok = newLZ_offset44_decode_arm64(&s);

			#elif defined(__RADNEON__) && !defined(__RADARM64__)

			// NOTE(fg): For ARM64, ARM64_ASM > dec64tab > NEON (but all three beat the old code);
			// for 32-bit ARM, we have no ASM version and NEON beats dec64tab right now.
			decode_ok = newLZ_offset44_decode_neon(&s);

			#else

			decode_ok = newLZ_offset44_decode64_tab(&s);

			#endif
		}
		else
		{
			SIMPLEPROFILE_SCOPE_N(offsetalt_decode,offsets_count);

			#if NEWLZ_OFFSET_DECODE_DEBUG
			// decode offsets via ref decoder first
			KrakenOffsetState ref_s = s;
			ref_s.neg_offs_s32 = ref_offs;
			newLZ_offsetalt_decode_finish(&ref_s);
			#endif

			#if defined(__RADX86__)

			if ( rrsimd_has_avx2() )
				decode_ok = newLZ_offsetalt_decode_avx2(&s);
			else if ( rrsimd_has_sse4() )
				decode_ok = newLZ_offsetalt_decode_sse4(&s);
			else
				decode_ok = newLZ_offsetalt_decode64_tab(&s);

			#elif defined(NEWLZ_OFFSET_ARM64_ASM)

			decode_ok = newLZ_offsetalt_decode_arm64(&s);

			#elif defined(__RADNEON__) && !defined(__RADARM64__)

			decode_ok = newLZ_offsetalt_decode_neon(&s);

			#else

			decode_ok = newLZ_offsetalt_decode64_tab(&s);

			#endif
		}

		if (!decode_ok)
			return -1;

		#if NEWLZ_OFFSET_DECODE_DEBUG
		delete[] ref_offs;
		#endif

		// Translate state back to rrVarBits form
		rrVarBits_GetOpen_NoRefill(lvb1, s.bitp[0], ptr_end);
		rrVarBits_GetOpen_NoRefill(lvb2, s.bitp[1], ptr_end);

		MAKE_FRONTBACK_SAFE(lvb1_cur,lvb2_cur);
		rrVarBits_Refill_Unsafe(lvb1);
		rrVarBits_RefillBack_Unsafe(lvb2);

		rrVarBits_Use(lvb1, s.bitc[0]);
		rrVarBits_Use(lvb2, s.bitc[1]);

#ifdef SPEEDFITTING
		// get end time without simd_mul_s32_sub_u8
		t2 = speedfitter_ticks_end();
#endif

		// NOTE(fg): I investigated always doing the mul inside the core SIMD
		// decoder loop for the alt offsets; that turns out to be about the same
		// speed as this two-pass solution.
		//
		// It might end up slightly faster if we also specialized all the loops
		// to have modulo=1 and modulo!=1 cases, but that seems like complete
		// overkill.

		if ( offset_alt_modulo > 1 )
		{
			// second pass to mul by modulo and add low
			// (offsets is negative already)

			// FUZZ NOTE : this can essentially scramble offset (if the multiply makes it go out of range)
			//	-> it can result in 0 or positive offsets going to the parse loop

			simd_mul_s32_sub_u8(offsets,offsets_count,offset_alt_modulo,offsets_u8_2);
		}
	}

	#ifdef SPEEDFITTING
	
	if ( g_speedfitter_stage != 2 )
		break;
	
	// t2 already fetched
	RR_ASSERT( t2 != t1 );
	
	speedfitter_stage2_collect_get_offsets(speedfit_iter,offset_alt_modulo,(int)offsets_count,t2-t1);
	
	if ( speedfit_iter > c_speedfit_stage2_num_repeats )
		break;

	}
	
	#endif

	//=====================================================================
	// BEGIN EXCESSES

	#ifdef SPEEDFITTING
	
	rrVarBits save_lvb1,save_lvb2;
	
	rrVarBits_Copy(save_lvb1.m,lvb1);
	rrVarBits_Copy(save_lvb2.m,lvb2);
		
	for(int speedfit_iter=0;;speedfit_iter++)
	{
		rrVarBits_Copy(lvb1,save_lvb1.m);
		rrVarBits_Copy(lvb2,save_lvb2.m);
		
		U64 t1 = speedfitter_ticks_start();
	
	#endif


	// for new excesses encoding, need to switch streams here
	if ( excess_hdr_byte != 0 )
	{
		// verify that front & back varbits met (since we should be done with offsets now)
		SINTa ptr_delta = rrPtrDiff(rrVarBits_GetEndPtr(lvb1) - rrVarBitsBack_GetEndPtr(lvb2));
		REQUIRE_FUZZ_RETURN( ptr_delta == 0 , -1 );

		// if an excess stream is present
		if ( excess_stream_bytes != 0 )
		{
			// everything back to the beginning and init stream pair for excesses
			// NOTE excess_stream_bytes was validated earlier

			ptr_start = chunk_comp_end - excess_stream_bytes;
			ptr_end = chunk_comp_end;
			RESET_FRONTBACK_SAFE_VARS(ptr_start,ptr_end);

			rrVarBits_GetOpen_NoRefill(lvb1,ptr_start,ptr_end);
			rrVarBits_GetOpen_NoRefill(lvb2,ptr_end,ptr_end);
			
			// no refill here, newLZ_get_excesses will do it internally
		}
	}

	// old excess path has excess_u32_count > 0 if excesses present and excess_stream_bytes == 0
	// new excess path has excess_u32_count == 0 (not sent) and excess_stream_bytes > 0 if u32 excesses present
	if ( excess_u32_count > 0 || excess_stream_bytes > 0 )
	{
		static const U32 EXCESS_MAX = NEWLZ_CHUNK_LEN/256; // excess runs cover at least 256 bytes
		// NOTE(fg): This is small enough to just put on the stack. (512 plus change entries * 4 bytes = ~2k.)
		U32 excesses_u32_arr[ EXCESS_MAX + 8 ]; // 8: padding for over-read in merging pass (also need 4 padding for get_excesses)
		U32 * excesses_u32_base = excesses_u32_arr;

		// excesses_u32_arr over-read ; does this need to be initialized for valgrind ? not sure

		rrVarBits vb1, vb2;
		rrVarBits_Copy(vb1.m,lvb1);
		rrVarBits_Copy(vb2.m,lvb2);

		SINTa decoded_excess_u32_count = newLZ_get_excesses(&vb1,&vb2,excesses_u32_base,excesses_u32_base+EXCESS_MAX,(SINTa)excess_u32_count);
		if ( decoded_excess_u32_count < 0 )
		{
			ooLogError("corruption : invalid excess stream\n");
			return -1;
		}

		// excess_u32_count should be <= excesses_count
		if ( (SINTa)decoded_excess_u32_count > excesses_count )
		{
			ooLogError("corruption : excess_u32_count > excesses_count\n");
			return -1;
		}

		U32 * excesses_u32_end = excesses_u32_arr + decoded_excess_u32_count;
		
		// NOTE(fg): The excesses we write to excesses_ptr are *biased*; they have NEWLZ_PACKET_LRL_MAX added,
		// since excess LRLs are the more common case, and this saves one instr per packet.
		//
		// Excess MLs already needed to get biased on load anyway; this just changes the value
		// we add.
			
		{
			//SIMPLEPROFILE_SCOPE_N(excess_merge,excesses_count);
		
			// now unpack excesses from u8 to u32 :
			
			const U8 * excesses_u8_ptr = excesses_u8;
			U32 * excesses_u32_ptr = excesses_u32_base;
		
			U32 * excesses_ptr = excesses;
			U32 * excesses_ptr_end = excesses + excesses_count;
			RR_ASSERT( rrIsAlignedPointer(excesses,16) );
			
			#if defined(__RADSSE2__)
			
			// you can't over-read excesses_u8 cuz it might be in compbuf
			U32 * excesses_ptr_end_8 = excesses_ptr_end-7;
			while( excesses_ptr < excesses_ptr_end_8 )
			{
				// load 8 :
				__m128i x = _mm_loadl_epi64((const __m128i *)(excesses_u8_ptr));
				
				// test for any escapes :
				__m128i cmp = _mm_cmpeq_epi8(x, _mm_set1_epi8(-1) );
				int cmp_mask = _mm_movemask_epi8(cmp);
				
				// unpack bytes -> words :
				x = _mm_unpacklo_epi8(x, _mm_setzero_si128());

				// add the bias :
				x = _mm_add_epi16(x, _mm_set1_epi16(NEWLZ_PACKET_LRL_MAX) );
				
  				__m128i lo = _mm_unpacklo_epi16(x, _mm_setzero_si128());
  				__m128i hi = _mm_unpackhi_epi16(x, _mm_setzero_si128());
							
				// excesses are aligned
				_mm_store_si128((__m128i *)excesses_ptr,lo);
				_mm_store_si128((__m128i *)(excesses_ptr+4),hi);
				
				// if any were escapes :
				if ( cmp_mask )
				{
					do
					{
						int i = rrCtz32(cmp_mask);
						RR_ASSERT( excesses_u8_ptr[i] == 255 && excesses_ptr[i] == 255 + NEWLZ_PACKET_LRL_MAX );
						excesses_ptr[i] = ( 255 + NEWLZ_PACKET_LRL_MAX ) + *excesses_u32_ptr++;
						cmp_mask &= (cmp_mask-1); // turn off bottom bit
					} while(cmp_mask);
					
					REQUIRE_FUZZ_RETURN( excesses_u32_ptr <= excesses_u32_end , -1 );
				}
				
				excesses_u8_ptr += 8;
				excesses_ptr += 8;
			}
			
			/*
			
			// 16-wide for the record; seems to be slightly worse, definitely meh
			
			U32 * excesses_ptr_end_16 = excesses_ptr_end-15;
			while( excesses_ptr < excesses_ptr_end_16 )
			{
				// load 16 :
				__m128i x = _mm_loadu_si128((const __m128i *)(excesses_u8_ptr));
				
				// test for any escapes :
				__m128i cmp = _mm_cmpeq_epi8(x, _mm_set1_epi8(-1) );
				int cmp_mask = _mm_movemask_epi8(cmp);
				
				// unpack bytes -> words :
				__m128i w1 = _mm_unpacklo_epi8(x, _mm_setzero_si128());
				__m128i w2 = _mm_unpackhi_epi8(x, _mm_setzero_si128());

				// add the bias :
				w1 = _mm_add_epi16(w1, _mm_set1_epi16(NEWLZ_PACKET_LRL_MAX) );
				w2 = _mm_add_epi16(w2, _mm_set1_epi16(NEWLZ_PACKET_LRL_MAX) );
				
				// excesses are aligned
				_mm_store_si128((__m128i *)(excesses_ptr+0), _mm_unpacklo_epi16(w1, _mm_setzero_si128()) );
				_mm_store_si128((__m128i *)(excesses_ptr+4), _mm_unpackhi_epi16(w1, _mm_setzero_si128()) );
				_mm_store_si128((__m128i *)(excesses_ptr+8), _mm_unpacklo_epi16(w2, _mm_setzero_si128()) );
				_mm_store_si128((__m128i *)(excesses_ptr+12), _mm_unpackhi_epi16(w2, _mm_setzero_si128()) );
				
				// if any were escapes :
				if ( cmp_mask )
				{
					do
					{
						int i = rrCtz32(cmp_mask);
						RR_ASSERT( excesses_u8_ptr[i] == 255 );
						excesses_ptr[i] += *excesses_u32_ptr++;
						cmp_mask &= (cmp_mask-1); // turn off bottom bit
					} while(cmp_mask);
				}
				
				excesses_u8_ptr += 16;
				excesses_ptr += 16;
			}
			
			*/

			#elif defined(__RADNEON__) && defined(__RAD64REGS__)
			
			// you can't over-read excesses_u8 cuz it might be in compbuf
			if ( excesses_ptr_end - excesses_ptr >= 8 )
			{
				U32 * excesses_ptr_end_8 = excesses_ptr_end-7;
				do
				{
					// load 8 :
					uint8x8_t x = vld1_u8(excesses_u8_ptr);
					
					// test for any escapes using int ops:
					static const U64 splat = 0x0101010101010101ULL; // mul values by this
					static const U64 lo7_mask = splat * 0x7f;
					static const U64 msb_mask = splat * 0x80;

					U64 xi = RR_GET64_LE(excesses_u8_ptr);
					U64 escape_where = msb_mask - (~xi & lo7_mask); // MSB in byte set iff bottom 7 bits of byte ==0x7f
					escape_where &= xi & msb_mask; // and we require each source MSB to have been set too

					// unpack u8 -> u16 and add the bias in the process (long add)
					uint16x8_t x16 = vaddl_u8(x, vdup_n_u8(NEWLZ_PACKET_LRL_MAX));
					
					// unpack u16 -> u32 (widening shift left by 0)
					uint32x4_t lo = vshll_n_u16(vget_low_u16(x16), 0);
					uint32x4_t hi = vshll_n_u16(vget_high_u16(x16), 0);
								
					// excesses are aligned
					vst1q_u32(excesses_ptr, lo);
					vst1q_u32(excesses_ptr + 4, hi);
					
					// if any were escapes :
					if ( escape_where )
					{
						do
						{
							int i = rrCtzBytes64(escape_where);
							RR_ASSERT( excesses_u8_ptr[i] == 255 && excesses_ptr[i] == 255 + NEWLZ_PACKET_LRL_MAX );
							excesses_ptr[i] = ( 255 + NEWLZ_PACKET_LRL_MAX ) + *excesses_u32_ptr++;
							escape_where &= (escape_where-1); // turn off bottommost set bit
						} while(escape_where);
						
						REQUIRE_FUZZ_RETURN( excesses_u32_ptr <= excesses_u32_end , -1 );
					}
					
					excesses_u8_ptr += 8;
					excesses_ptr += 8;
				}
				while ( excesses_ptr < excesses_ptr_end_8 );
			}
			#endif		
			
			// tail :
			while( excesses_ptr < excesses_ptr_end )
			{
				U32 x = *excesses_u8_ptr++;
				if ( x == 255 )
				{
					REQUIRE_FUZZ_RETURN( excesses_u32_ptr < excesses_u32_end , -1 );
					x += *excesses_u32_ptr++;
				}
				x += NEWLZ_PACKET_LRL_MAX;
				*excesses_ptr++ = x;
			}
			
			REQUIRE_FUZZ_RETURN( excesses_u32_ptr == excesses_u32_end , -1 );
			//RR_ASSERT( excesses_u32_ptr == excesses_u32_end );
		}
	
	}
	else // excess_u32_count == 0
	{
		// no excess escapes, so no check for 255 overflow
		// still need to do U8 -> U32 expansion & add bias
	
		// on Jaguar having this special case does nothing for me (excess count is super low anyway)
	
		// verify that front & back varbits met :
		SINTa ptr_delta = rrPtrDiff(rrVarBits_GetEndPtr(lvb1) - rrVarBitsBack_GetEndPtr(lvb2));
		REQUIRE_FUZZ_RETURN( ptr_delta == 0 , -1 );
		
		// fuzz safety :
		// there should not be any values of 255 (escape) in excesses_u8
		//	if there are, they will turn into just value 255 excesses
		//	those will be fuzz-protected in the parse, so no check is needed here
		
		// now unpack excesses from u8 to u32 :
		
		const U8 * excesses_u8_ptr = excesses_u8;
	
		U32 * excesses_ptr = excesses;
		U32 * excesses_ptr_end = excesses + excesses_count;
		RR_ASSERT( rrIsAlignedPointer(excesses,16) );
		
		#if defined(__RADSSE2__)
		
		// you can't over-read excesses_u8 cuz it might be in compbuf
		U32 * excesses_ptr_end_8 = excesses_ptr_end-7;
		while( excesses_ptr < excesses_ptr_end_8 )
		{
			// load 8 :
			__m128i x = _mm_loadl_epi64((const __m128i *)(excesses_u8_ptr));
						
			// unpack bytes -> words :
			x = _mm_unpacklo_epi8(x, _mm_setzero_si128());

			// add the bias :
			x = _mm_add_epi16(x, _mm_set1_epi16(NEWLZ_PACKET_LRL_MAX) );
			
			__m128i lo = _mm_unpacklo_epi16(x, _mm_setzero_si128());
			__m128i hi = _mm_unpackhi_epi16(x, _mm_setzero_si128());
						
			// excesses are aligned
			_mm_store_si128((__m128i *)excesses_ptr,lo);
			_mm_store_si128((__m128i *)(excesses_ptr+4),hi);
						
			excesses_u8_ptr += 8;
			excesses_ptr += 8;
		}

		#elif defined(__RADNEON__) && defined(__RAD64REGS__)
		
		// you can't over-read excesses_u8 cuz it might be in compbuf
		if ( excesses_ptr_end - excesses_ptr >= 8 )
		{
			U32 * excesses_ptr_end_8 = excesses_ptr_end-7;
			do
			{
				// load 8 :
				uint8x8_t x = vld1_u8(excesses_u8_ptr);
				
				// unpack u8 -> u16 and add the bias in the process (long add)
				uint16x8_t x16 = vaddl_u8(x, vdup_n_u8(NEWLZ_PACKET_LRL_MAX));
				
				// unpack u16 -> u32 (widening shift left by 0)
				uint32x4_t lo = vshll_n_u16(vget_low_u16(x16), 0);
				uint32x4_t hi = vshll_n_u16(vget_high_u16(x16), 0);
							
				// excesses are aligned
				vst1q_u32(excesses_ptr, lo);
				vst1q_u32(excesses_ptr + 4, hi);
								
				excesses_u8_ptr += 8;
				excesses_ptr += 8;
			}
			while ( excesses_ptr < excesses_ptr_end_8 );
		}
		#endif		
		
		// tail :
		while( excesses_ptr < excesses_ptr_end )
		{
			U32 x = *excesses_u8_ptr++;
			RR_ASSERT_IF_NOT_CORRUPT( x < 255 );
			x += NEWLZ_PACKET_LRL_MAX;
			*excesses_ptr++ = x;
		}		
	}

	#ifdef SPEEDFITTING
	
	if ( g_speedfitter_stage != 2 )
		break;

	U64 t2 = speedfitter_ticks_end();
	
	speedfitter_stage2_collect_get_excesses(speedfit_iter,(int)excesses_count,(int)excess_u32_count,t2-t1);
	
	if ( speedfit_iter > c_speedfit_stage2_num_repeats )
		break;

	}
	
	#endif

	return 1;
}

//===============================================================================================
// PUT :


SINTa newLZ_put_excesses_u32_separate(U8 * comp_ptr, U8 * comp_end,
	const U32 * excess_u32s, int excesses_u32_count)	
{
	if ( comp_end - comp_ptr < 16 ) // space for the varbits below
		return -1;

	// valid 0 byte stream :
	if ( excesses_u32_count == 0 )
		return 0;

	rrVarBits_Locals(lvb1);
	rrVarBits_PutOpen(lvb1,comp_ptr);

	rrVarBits_Locals(lvb2);
	rrVarBits_PutOpen(lvb2,comp_end);

	// put excesses (if any)
	for(int xi=0;xi<excesses_u32_count;xi++)
	{
		U32 x = excess_u32s[xi];

		if ( (xi&1)== 0 )
		{
			VARBITS_PUT_EXPGOLOMB(lvb1,x,EXCESS_EXPGOLOMB_SHIFT,rrVarBits_Output);
		}
		else
		{
			VARBITS_PUT_EXPGOLOMB(lvb2,x,EXCESS_EXPGOLOMB_SHIFT,rrVarBits_OutputBack);
		}

		if ( lvb2_cur - lvb1_cur <= 8 ) return -1;
	}

	rrVarBits_PutFlush8(lvb1);
	rrVarBits_PutFlush8Back(lvb2);

	// now compact the excesses at the end of the output buffer
	U8 * vb_excess_front_end = rrVarBits_PutEndPtr(lvb1);
	U8 * vb_excess_back_end = NVB(lvb2,_cur);
	
	// check stream collision :
	if ( rrPtrDiff(vb_excess_back_end - vb_excess_front_end) <= 8 )
		return -1;
	
	SINTa vb_excess_front_len = rrPtrDiff(vb_excess_front_end - comp_ptr);

	U8 * vb_separate_excess_start = vb_excess_back_end - vb_excess_front_len;
	memmove(vb_separate_excess_start, comp_ptr, vb_excess_front_len);

	// put the excess stream size
	SINTa excess_stream_bytes = rrPtrDiff(comp_end - vb_separate_excess_start);
	RR_ASSERT(excesses_u32_count == 0 || excess_stream_bytes != 0); // should have >0 bytes of excess_u32 if nonzero count

	return excess_stream_bytes;
}


SINTa newLZ_put_offsets_excesses(U8 * comp_ptr, U8 * comp_end,
	const U8 * offsets_u8s, 
	const U32 * offsets, int offsets_count,
	int offset_alt_modulo,
	const U32 * excess_u32s, int excesses_u32_count,
	SINTa excesses_u32_hdr_size,SINTa excess_stream_bytes)	
{

	/*
	rrprintfvar(offsets_count);
	rrprintfvar(excesses_u32_count);
	rrprintfvar(excess_stream_bytes);
	*/
	
	// should have some room :
	if ( 16 >= rrPtrDiff( comp_end - comp_ptr ) )
		return -1;

	U8 * vb_comp_start = comp_ptr;
	U8 * vb_comp_end = comp_end;
	U8 * vb_back_start;

	{
	rrVarBits_Locals(lvb1);
	rrVarBits_PutOpen(lvb1,vb_comp_start);

	rrVarBits_Locals(lvb2);
	rrVarBits_PutOpen(lvb2,vb_comp_end);

	if ( excesses_u32_hdr_size == 0 )
	{
		// old format hasn't encoded excesses yet
		// put excesses count before offsets :
		VARBITS_PUT_EXPGOLOMB(lvb2,excesses_u32_count,EXCESS_U32_COUNT_EXPGOLOMB_SHIFT,rrVarBits_OutputBack);
		rrVarBits_OutputBack(lvb2);
	}

	#ifdef CHECK_BITS
	
	rrVarBits_Put(lvb1,CHECK_VAL,CHECK_BITS);
	rrVarBits_Put(lvb2,CHECK_VAL,CHECK_BITS);
	rrVarBits_Output(lvb1);
	rrVarBits_OutputBack(lvb2);
	
	#endif

	//S64 lvb2_start_size_bits = rrVarBitsBack_PutSizeBits(lvb2,vb_comp_end);
		
	// put all offset raw bits :
	if ( offset_alt_modulo == 0 )
	{
		int offi=0;

		// As long as we haven't seen any large offsets, keep doing 4 at a time.
		// This is worthwhile because:
		// a) if we're using small windows or small chunks, we will _never_ see large offsets
		//    (which start around ~8MB, much larger than any typical chunk size)
		// b) the fastest encoders can't even generate offsets that large
		// c) the loop is pretty clean as long as we have pairs of non-escape offsets that
		//    start at an even offset index; maintaining this invariant in the main arbitrary-offset
		//    loop is annoying, but it's really easy if we just never have escapes.
		//
		// As of this writing (2018-04-23), this loop is about 25% higher throughput than the main
		// offset loop below (on Windows x64).
		for(;offi<offsets_count-3;offi+=4)
		{
			// sufficient to check this once per iter
			if ( lvb2_cur-lvb1_cur <= 8 ) return -1;

			// Grab four offset U8 codes
			U32 four_offs_u8 = RR_GET32_LE_UNALIGNED(offsets_u8s + offi);

			// Is any of them >= 0xf0?
			U32 large_offset_mask = four_offs_u8 & 0x80808080u; // must have MSB set
			large_offset_mask &= ~four_offs_u8 - 0x10101010u; // this has MSB set in least-significant byte where ~four_offs_u8 < 0x10 <=> four_offs_u8 > 0xef
			if ( large_offset_mask )
				break;

			// output the four values
			// each of these writes at most 14+4 = 18 bits
			S32 num_raw,off_raw;

			num_raw = newLZ_offset44_unpack_low_numrawbits((U8)four_offs_u8); four_offs_u8 >>= 8;
			off_raw = newLZ_offset44_pack_low_rawbits(offsets[offi+0],num_raw);
			rrVarBits_Put(lvb1,off_raw,num_raw);

			num_raw = newLZ_offset44_unpack_low_numrawbits((U8)four_offs_u8); four_offs_u8 >>= 8;
			off_raw = newLZ_offset44_pack_low_rawbits(offsets[offi+1],num_raw);
			rrVarBits_Put(lvb2,off_raw,num_raw);

			#if RR_MINBITSAVAILABLE < (2*18)
			// at most 18 bits written per lvb; with 64b, we can keep going
			// but small bit buffers need to output here
			rrVarBits_Output(lvb1);
			rrVarBits_OutputBack(lvb2);
			#endif

			num_raw = newLZ_offset44_unpack_low_numrawbits((U8)four_offs_u8); four_offs_u8 >>= 8;
			off_raw = newLZ_offset44_pack_low_rawbits(offsets[offi+2],num_raw);
			rrVarBits_Put(lvb1,off_raw,num_raw);

			num_raw = newLZ_offset44_unpack_low_numrawbits((U8)four_offs_u8);
			off_raw = newLZ_offset44_pack_low_rawbits(offsets[offi+3],num_raw);
			rrVarBits_Put(lvb2,off_raw,num_raw);

			rrVarBits_Output(lvb1);
			rrVarBits_OutputBack(lvb2);
		}

		for(;offi<offsets_count;offi++)
		{
			if ( lvb2_cur-lvb1_cur <= 8 ) return -1;

			U8 offsets_u8 = offsets_u8s[offi];
			S32 off = offsets[offi];
			RR_ASSERT( off >= NEWLZ_MIN_OFFSET && off < NEWLZ_MAX_OFFSET );

			if ( offsets_u8 >= 0xF0 )
			{
				S32 num_raw = newLZ_offset44_unpack_high_numrawbits(offsets_u8);
				U32 off_raw = newLZ_offset44_pack_high_rawbits(off,num_raw);

				#if RR_MINBITSAVAILABLE < NEWLZ_MAX_OFFSET_BITS
				// 32-bit , have to split the put :
				RR_ASSERT( num_raw >= 16 );
				U32 low = off_raw & 0xFFF;
				off_raw >>= 12;
				num_raw -= 12;
				#endif

				if ( (offi&1) == 0 )
				{
					rrVarBits_Put(lvb1,off_raw,num_raw);
					rrVarBits_Output(lvb1);
				}
				else
				{
					rrVarBits_Put(lvb2,off_raw,num_raw);
					rrVarBits_OutputBack(lvb2);
				}

				#if RR_MINBITSAVAILABLE < NEWLZ_MAX_OFFSET_BITS
				if ( (offi&1) == 0 )
				{
					rrVarBits_Put(lvb1,low,12);
					rrVarBits_Output(lvb1);
				}
				else
				{
					rrVarBits_Put(lvb2,low,12);
					rrVarBits_OutputBack(lvb2);
				}
				#endif
			}
			else
			{
				S32 num_raw = newLZ_offset44_unpack_low_numrawbits(offsets_u8);
				U32 off_raw = newLZ_offset44_pack_low_rawbits(off,num_raw);

				if ( (offi&1) == 0 )
				{
					rrVarBits_Put(lvb1,off_raw,num_raw);
					rrVarBits_Output(lvb1);
				}
				else
				{
					rrVarBits_Put(lvb2,off_raw,num_raw);
					rrVarBits_OutputBack(lvb2);
				}
			}
		}
	}
	else // alt offsets
	{
		// @@ check modulo == 1
	
		for(int offi=0;offi<offsets_count;offi++)
		{
			if ( lvb2_cur-lvb1_cur <= 8 ) return -1;

			U8 top = offsets_u8s[offi];
			S32 off = offsets[offi];
			RR_ASSERT( off >= 1 && off < NEWLZ_MAX_OFFSET );

			U32 num_raw = newLZ_alt_offset_unpack_numrawbits(top);
			
			U32 off_raw = newLZ_alt_offset_pack_high_rawbits(off,num_raw,offset_alt_modulo);
			
			// num_raw has a max of 26 I think?
			
			#if RR_MINBITSAVAILABLE < NEWLZ_MAX_OFFSET_BITS
			if ( num_raw >= RR_MINBITSAVAILABLE )
			{
				// 32-bit , have to split the put :
				RR_ASSERT( num_raw >= 16 );
				U32 low = off_raw & 0xFFF;
				off_raw >>= 12;
				num_raw -= 12;

				if ( (offi&1) == 0 )
				{
					rrVarBits_Put(lvb1,off_raw,num_raw);
					rrVarBits_Output(lvb1);
					
					rrVarBits_Put(lvb1,low,12);
					rrVarBits_Output(lvb1);
				}
				else
				{
					rrVarBits_Put(lvb2,off_raw,num_raw);
					rrVarBits_OutputBack(lvb2);
					
					rrVarBits_Put(lvb2,low,12);
					rrVarBits_OutputBack(lvb2);
				}
			}
			else
			#endif
			{
				if ( (offi&1) == 0 )
				{
					rrVarBits_Put(lvb1,off_raw,num_raw);
					rrVarBits_Output(lvb1);
				}
				else
				{
					rrVarBits_Put(lvb2,off_raw,num_raw);
					rrVarBits_OutputBack(lvb2);
				}
			}
		}
	}

	/*
	if ( 1 )
	{
		// confirm offsets_u8_extrabits calc :
	
		S64 lvb1_end_size_bits = rrVarBits_PutSizeBits(lvb1,vb_comp_start);
		S64 lvb2_end_size_bits = rrVarBitsBack_PutSizeBits(lvb2,vb_comp_end);
		
		S64 offset_extrabits = lvb1_end_size_bits + lvb2_end_size_bits - lvb2_start_size_bits;
		//RR_ASSERT_ALWAYS( offset_extrabits == offsets_u8_extrabits );
		rrprintfvar(offset_extrabits);
	}
	/**/

	if ( excesses_u32_hdr_size == 0 )
	{
		// old format
		// put all excess raw bits :
		for(int xi=0;xi<excesses_u32_count;xi++)
		{
			U32 x = excess_u32s[xi];

			if ( (xi&1)== 0 )
			{
				VARBITS_PUT_EXPGOLOMB(lvb1,x,EXCESS_EXPGOLOMB_SHIFT,rrVarBits_Output);
			}
			else
			{
				VARBITS_PUT_EXPGOLOMB(lvb2,x,EXCESS_EXPGOLOMB_SHIFT,rrVarBits_OutputBack);
			}

			if ( lvb2_cur-lvb1_cur <= 8 ) return -1;
		}
	}

	rrVarBits_PutFlush8(lvb1);
	rrVarBits_PutFlush8Back(lvb2);

	comp_ptr = rrVarBits_PutEndPtr( lvb1 );
	vb_back_start = NVB(lvb2,_cur);
	RR_ASSERT( vb_back_start == rrVarBits_PutEndPtr(lvb2) );
	} // varbits scope

	// collision !
	if ( rrPtrDiff(vb_back_start - comp_ptr) <= 8  )
		return -1;

	// move down the end :
	SINTa vb_back_len = vb_comp_end - vb_back_start;

	memmove(comp_ptr,vb_back_start,vb_back_len);
	comp_ptr += vb_back_len;

	// separate excess stream
	// this was prepared at the very end of the compressed buffer
	// move it up
	if ( excess_stream_bytes > 0 )
	{
		U8 * vb_separate_excess_start = comp_end;
		memmove(comp_ptr,vb_separate_excess_start,excess_stream_bytes);
		comp_ptr += excess_stream_bytes;
	}

	SINTa varbits_comp_len = rrPtrDiff(comp_ptr - vb_comp_start);

	// do NOT add excesses_u32_hdr_size
	//	comp_ptr has already advanced for that
	// we DO need to add it to J still

	return varbits_comp_len;
}
	

// cost_alt_offset_modulo
//	estimate J cost of coding offset_array[] with given mod
static F32 cost_alt_offset_modulo(int mod,
						const U32 * offset_array,int offset_count,
						F32 lambda,const OodleSpeedFit * speedfit)
{
	RR_ASSERT( mod >= 1 && mod <= NEWLZ_ALT_OFFSET_MAX_MOD );

	U32 top_histo[256];
	U32 bottom_histo[NEWLZ_ALT_OFFSET_MAX_MOD];

	RR_ZERO(top_histo);
	RR_ZERO(bottom_histo);

	S32 raw_bits = 0;

	for(int s=0;s<offset_count;s++)
	{
		S32 off = offset_array[s];

		RR_ASSERT( off >= 1 && off < NEWLZ_MAX_OFFSET );
		
		U8 top,bot;
		newLZ_alt_offset_pack_u8s(off,&top,&bot,mod);
		
		raw_bits += newLZ_alt_offset_unpack_numrawbits(top);
		top_histo[top] += 1;
		bottom_histo[bot] += 1;			
	}
			
	S32 top_bytes = (raw_bits + 7)/8;
	top_bytes += newlz_array_estimate_complen_bytes(top_histo,RR_ARRAY_SIZE(top_histo),offset_count);
	
	F32 J = (F32) top_bytes;

	if ( mod > 1 )
	{
		// add get_offsets_time to get the delta for mod=1 vs mod>1
		J += lambda * speedfit->simd_mul_s32_sub_u8(offset_count);
	
		S32 bottom_bytes = newlz_array_estimate_complen_bytes(bottom_histo,RR_ARRAY_SIZE(bottom_histo),offset_count);

		J += bottom_bytes;

		// plus the time to entropy code the bottoms :
		//	(not added for tops cuz we only care about relative J here)
		J += lambda * speedfit_estimate_entropy_array_time(speedfit,offset_count);
	}
	
	return J;
}
	
/**

choose_alt_offset_modulo : choose the best modulo for alt offset encoding

space-speed bias towards mod=1 because then you have top part only, no bottom bits

currently this is slow / brute force , just try all mods and count the bits
could speed it up with some heuristics to make only 1 or 2 guesses at likely good mods
for example histo offsets < 1024 and find the autocorrelation peak
	that generally gives you the struct stride

**/
static int choose_alt_offset_modulo_brute(const U32 * offsets,int offset_count,
									F32 lambda,const OodleSpeedFit * speedfit)
{
	SIMPLEPROFILE_SCOPE(choose_alt_offset_modulo_brute);
	
	if ( offset_count < NEWLZ_HUFF_ARRAY_MIN_SIZE ) // some small number of offsets, don't bother
		return 1;
			
	int best_mod = 0;
	F32 best_mod_J = LAGRANGE_COST_INVALID;
	
	for(int mod=1;mod<=NEWLZ_ALT_OFFSET_MAX_MOD;mod++)
	{
		F32 J = cost_alt_offset_modulo(mod, offsets,offset_count,lambda,speedfit);

		//rrprintf("%3d : %d\n",mod,complen);
		
		if ( J < best_mod_J )
		{
			best_mod_J = J;
			best_mod = mod;
			
			//rrprintf("%3d : %.1f\n",best_mod,best_mod_J);
		}
	}
	
	//rrprintf("%3d : %.1f\n",best_mod,best_mod_J);
	
	return best_mod;
}

/***

choose_alt_offset_modulo_fast :

find 4 most probably offsets <= NEWLZ_ALT_OFFSET_MAX_MOD

just cost them (and also always mod=1)

take the best

gets it right most of the time


at -z7 :

choose_alt_offset_modulo_brute:

Krakenhctest 7 :
total : 31,000,000 ->15,622,002 =  4.031 bpb =  1.984 to 1

PD3D: 
SUM:average : 31,941,800 ->10,459,870 =  2.620 bpb =  3.054 to 1
SUM:total   : 31,941,800 -> 8,199,679 =  2.054 bpb =  3.895 to 1

choose_alt_offset_modulo_fast :

Krakenhctest 7 :
total : 31,000,000 ->15,623,300 =  4.032 bpb =  1.984 to 1

PD3D:
SUM:average : 31,941,800 ->10,465,625 =  2.621 bpb =  3.052 to 1
SUM:total   : 31,941,800 -> 8,212,228 =  2.057 bpb =  3.890 to 1

only 0.001-0.003 bpb worse

===============

the speed of choose_alt_offset_modulo_fast seems to be good enough :

lzhc_pp_offsets       : 1.6514    1869
choose_alt_offset_modulo_fast: 0.0803

it's an insignificant portion of the overall put_offsets time

there certainly are ways to do this even faster, but it doesn't seem to be
necessary at the moment

===============

***/

static int choose_alt_offset_modulo_fast(const U32 * offsets,int offset_count,
									F32 lambda,const OodleSpeedFit * speedfit)
{
	SIMPLEPROFILE_SCOPE(choose_alt_offset_modulo_fast);

	U32 offset_histo[NEWLZ_ALT_OFFSET_MAX_MOD+1];
	
	// put the offsets at the bottom :
	for LOOPINT(o,NEWLZ_ALT_OFFSET_MAX_MOD+1)
	{
		offset_histo[o] = o;
	}
	
	for(int s=0;s<offset_count;s++)
	{
		S32 off = offsets[s];
		if ( off > NEWLZ_ALT_OFFSET_MAX_MOD ) continue;

		// inc histo, in the top 24 bits :
		offset_histo[off] += (1<<8);
	}
	
	// sort to get N most probable
	// could of course be a partial_sort
	//	I doubt that's significant to the time though
	// (one way to partial_sort is to make_heap then pop it a few times)
	stdsort(offset_histo,offset_histo+RR_ARRAY_SIZE(offset_histo),stdgreater<U32>());
	
	// highest count first :
	RR_ASSERT( offset_histo[0] >= offset_histo[1] );
	
	// always compare to mod=1 (no mod)
	int best_mod = 1;
	F32 best_mod_J = cost_alt_offset_modulo(1, offsets,offset_count,lambda,speedfit);

	// top 4 most probable :
	for(int s=0;s<4;s++)
	{
		int mod = offset_histo[s] & 0xFF;
		if ( mod <= 1 ) continue;
		
		F32 J = cost_alt_offset_modulo(mod, offsets,offset_count,lambda,speedfit);
		
		if ( J < best_mod_J )
		{
			best_mod_J = J;
			best_mod = mod;
		}
	}

	return best_mod;
}

void put_alt_offset_u8_arrays(const U32 * offset_array,int offset_count,
								U8 * alt_offsets_u8_top,
								U8 * alt_offsets_u8_bot,
								int * p_alt_offset_extrabits,
								int modulo,
								const U8 * offset_44_u8s,
								int * p_offset44_extrabits)
{
	RR_ASSERT( modulo >= 1 && modulo <= NEWLZ_ALT_OFFSET_MAX_MOD );

	int alt_offset_extrabits = 0;
	int offset44_extrabits = 0;

	// if modulo == 1 , no bot, simpler
	if ( modulo == 1 )
	{
		for LOOP(i,offset_count)
		{
			U32 off = offset_array[i];
			
			offset44_extrabits += newLZ_offset44_unpack_numrawbits(offset_44_u8s[i]);
			
			U8 top = newLZ_alt_offset_pack_u8_modulo1(off);
			alt_offsets_u8_top[i] = top;
			
			alt_offset_extrabits += newLZ_alt_offset_unpack_numrawbits(top);
		}
	}
	else
	{
		for LOOP(i,offset_count)
		{
			U32 off = offset_array[i];
			
			offset44_extrabits += newLZ_offset44_unpack_numrawbits(offset_44_u8s[i]);
			
			U8 top,bot;
			newLZ_alt_offset_pack_u8s(off,&top,&bot,modulo);
			alt_offsets_u8_top[i] = top;
			alt_offsets_u8_bot[i] = bot;
			
			alt_offset_extrabits += newLZ_alt_offset_unpack_numrawbits(top);
		}
	}
	
	*p_alt_offset_extrabits = alt_offset_extrabits;
	*p_offset44_extrabits = offset44_extrabits;
}


SINTa newLZ_put_offset_44_or_alt_arrays(U8 * comp_ptr,U8 * comp_end,
	U8 * offsets_u8_44,U32 * offsets,int offsets_count,
	U32 entropy_flags, F32 lambda,const OodleSpeedFit * speedfit,
	F32 * pJ,F32 deadline_t,
	ENewLZ_MinOffset min_offset,
	rrbool try_alt_offsets,
	int * p_offset_alt_modulo,
	rrArenaAllocator * arena,
	OodleLZ_CompressionLevel level,
	U32 * poptional_histo1,
	U32 * poptional_histo2)
{

	// if min_offset == 1 , only alt offsets are allowed
	
	int offset_alt_modulo = 0;
	SINTa offsets_comp_len = RR_S32_MAX;
	*pJ = LAGRANGE_COST_INVALID;
	
	if ( min_offset == eNewLZ_MinOffset_8 )
	{	
		offsets_comp_len = newLZ_put_array(comp_ptr,comp_end,offsets_u8_44,offsets_count,entropy_flags,lambda,speedfit,pJ,deadline_t,arena,level,
			poptional_histo1
			);

		if ( offsets_comp_len < 0 )
			return -1;

		//	offsets_J should contain the get_offsets varbits read time
		*pJ += lambda * speedfit->get_offsets44(offsets_count);
	}
	else
	{
		// min_offset 1 must have try_alt_offsets
		RR_ASSERT( try_alt_offsets );
	}
	
	//===========================================================

	if ( try_alt_offsets )
	{
		// try alt offsets :
		
		// need space for 2 alt offset u8 arrays + compressed output
		SINTa space_needed = offsets_count*4 + 16;
		RR_SCOPE_ARENA_ARRAY(scratch_space,U8,space_needed,arena);
		
		U8 * alt_offsets_top = scratch_space;
		U8 * alt_offsets_bot = scratch_space + offsets_count;
		U8 * scratch_comp_space = scratch_space + offsets_count*2;
		U8 * scratch_comp_end = scratch_space + space_needed;
		
		offset_alt_modulo = 1;
		
		// @@@@ encode levels for choose_alt_offset_modulo
		if ( level >= OodleLZ_CompressionLevel_Optimal4 )
		{
			// @@ maybe no point to this - perhaps remove for ship, just wasting encoder time for no reason
			offset_alt_modulo = choose_alt_offset_modulo_brute(offsets,offsets_count,lambda,speedfit);
		}
		else if ( level >= OodleLZ_CompressionLevel_Normal )
		{
			// Kraken doesn't even try alt offsets until Optimal1
			//	let Leviathan do this at Normal ?
			// -> NOT EXACTLY - Kraken "Normal" in greedy-for-optimal has alt bitstream flag ON
			offset_alt_modulo = choose_alt_offset_modulo_fast(offsets,offsets_count,lambda,speedfit);
		}
		
		int extrabits_alt;
		int extrabits_44;
		
		put_alt_offset_u8_arrays(offsets,offsets_count,
			alt_offsets_top,alt_offsets_bot,
			&extrabits_alt,offset_alt_modulo,
			offsets_u8_44,&extrabits_44);
		
		// put alt comp stream to offsets_alt_comp_ptr :
		U8 * offsets_alt_comp_ptr = scratch_comp_space;
		
		// flag top bit of offset newlz array header to indicate this is alt offset :
		RR_ASSERT( offset_alt_modulo >= 1 && offset_alt_modulo <= 128 );
		*offsets_alt_comp_ptr++ = U8_check( 0x80 + offset_alt_modulo - 1 );
		
		// don't replace optional_histo1 unless we choose alt
		U32 temp_optional_histo1[256];
		U32 * ptemp_optional_histo1 = NULL;
		if ( poptional_histo1 )
		{
			ptemp_optional_histo1 = temp_optional_histo1;
			RR_ZERO(temp_optional_histo1);
		}
		
		F32 offsets_alt_J1 = LAGRANGE_COST_INVALID;

		SINTa offsets_alt_comp_len1 = newLZ_put_array_small_allowed(offsets_alt_comp_ptr,scratch_comp_end,alt_offsets_top,offsets_count,entropy_flags,lambda,speedfit,&offsets_alt_J1,ARRAY_DEADLINE_HUGE,
			arena,level,ptemp_optional_histo1
			);
		
		if ( offsets_alt_comp_len1 < 0 )
		{
			ooLogError("offsets_alt_comp_len1 failed!");
			return -1;
		}
				
		offsets_alt_comp_ptr += offsets_alt_comp_len1;

		F32 offsets_alt_J2 = 0;
		SINTa offsets_alt_comp_len2 = 0;
		
		// offset_alt_modulo == 1 means no bottom bits
		if ( offset_alt_modulo > 1 )
		{
			offsets_alt_J2 = LAGRANGE_COST_INVALID;
			offsets_alt_comp_len2 = newLZ_put_array_small_allowed(offsets_alt_comp_ptr,scratch_comp_end,alt_offsets_bot,offsets_count,entropy_flags,lambda,speedfit,&offsets_alt_J2,ARRAY_DEADLINE_HUGE,
				arena,level,poptional_histo2
				);
			
			if ( offsets_alt_comp_len2 < 0 )
			{
				ooLogError("offsets_alt_comp_len2 failed!");
				return -1;
			}
			
			offsets_alt_comp_ptr += offsets_alt_comp_len2;
		}
		
		// 1 header byte + two entropy arrays :
		F32 offsets_alt_J = 1 + offsets_alt_J1 + offsets_alt_J2;
		offsets_alt_J += lambda * speedfit_get_offsets_time(speedfit,offsets_count,offset_alt_modulo);
			
		// to choose offsets vs. alt, include the extrabits size in J :
		//	(this does not go in offsets_J though, it will get counted later in "varbits part")
		F32 test_offsets_J = *pJ + ((extrabits_44+7)/8);
		F32 test_offsets_altJ = offsets_alt_J + ((extrabits_alt+7)/8);
		
		SINTa offsets_alt_comp_len = rrPtrDiff(offsets_alt_comp_ptr - scratch_comp_space);
		RR_ASSERT( test_offsets_altJ >= offsets_alt_comp_len );
		RR_ASSERT( offsets_alt_comp_len == offsets_alt_comp_len1 + offsets_alt_comp_len2 + 1 );
							
		RR_ASSERT( offsets_alt_J >= offsets_alt_comp_len );
		RR_ASSERT( offsets_alt_J == offsets_alt_comp_len || lambda > 0 );
		
		if ( test_offsets_altJ < test_offsets_J )
		{
			// alt wins!							
			memmove(comp_ptr,scratch_comp_space,offsets_alt_comp_len);
			*pJ = offsets_alt_J;
			offsets_comp_len = offsets_alt_comp_len;
			//offsets_u8_extrabits = offsets_alt_extrabits;
			
			// @@ the offsets_u8's are passed in to put_offsets
			//	 this is just to avoid doing a clz ; kind of silly
			memcpy(offsets_u8_44,alt_offsets_top,offsets_count);
			
			if ( poptional_histo1 )
				memcpy(poptional_histo1,ptemp_optional_histo1,256*sizeof(U32));
			// poptional_histo2 was already filled
			
			//rrprintfvar(extrabits_alt);
		}
		else
		{
			// flag no alt offsets :
			offset_alt_modulo = 0;
			
			//rrprintfvar(extrabits_44);
		}
	}

	// *pJ filled
	*p_offset_alt_modulo = offset_alt_modulo;

	//comp_ptr += offsets_comp_len;
	return offsets_comp_len;
}

OODLE_NS_END
