// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "vec128.inl"
#include "rrdxtcenums.h"

OODLE_NS_START

#ifdef DO_BUILD_SSE4

namespace internal {

static RADFORCEINLINE Vec128 DeltaSqrRGBA_SSE2(
	const Vec128 & x_br16, const Vec128 & x_ga16, const Vec128 & y_br16, const Vec128 & y_ga16)
{
	Vec128 diff_br16 = _mm_sub_epi16(y_br16, x_br16); // db, dr
	Vec128 diff_ga16 = _mm_sub_epi16(y_ga16, x_ga16); // dg, da

	Vec128 sumsq_br32 = _mm_madd_epi16(diff_br16, diff_br16); // db*db + dr*dr
	Vec128 sumsq_ga32 = _mm_madd_epi16(diff_ga16, diff_ga16); // dg*dg + da*da

	return _mm_add_epi32(sumsq_br32, sumsq_ga32); // db*db + dg*dg + dr*dr + da*da
}

static RADFORCEINLINE Vec128 BC1_FindBestIndices_SSE4(const Vec128 & row_br, const Vec128 & row_ga, const Vec128 & pal_br, const Vec128 & pal_ga)
{
	const Vec128 const32_1 = _mm_set1_epi32(1);
	const Vec128 const32_2 = _mm_set1_epi32(2);

	// Calculate the squared deltas
	Vec128 d0 = DeltaSqrRGBA_SSE2(row_br, row_ga, _mm_shuffle_epi32(pal_br, 0x00), _mm_shuffle_epi32(pal_ga, 0x00));
	Vec128 d1 = DeltaSqrRGBA_SSE2(row_br, row_ga, _mm_shuffle_epi32(pal_br, 0x55), _mm_shuffle_epi32(pal_ga, 0x55));
	Vec128 d2 = DeltaSqrRGBA_SSE2(row_br, row_ga, _mm_shuffle_epi32(pal_br, 0xaa), _mm_shuffle_epi32(pal_ga, 0xaa));
	Vec128 d3 = DeltaSqrRGBA_SSE2(row_br, row_ga, _mm_shuffle_epi32(pal_br, 0xff), _mm_shuffle_epi32(pal_ga, 0xff));

	// Put the right values into the low bits then perform the min reduction
	// written slightly oddly to reduce register pressure by needing 1 fewer constant
	Vec128 best01 = _mm_min_epi32(d0, _mm_add_epi32(d1, const32_1));
	Vec128 best23 = _mm_min_epi32(d2, _mm_add_epi32(d3, const32_1));
	Vec128 best0123 = _mm_min_epi32(best01, _mm_add_epi32(best23, const32_2));

	// Now have 2b best color index in low bits of every lane
	// and squared error in top 30 bits.
	return best0123;
}

static RADFORCEINLINE Vec128 BC1_FindBestErrors_SSE4(const Vec128 & row_br, const Vec128 & row_ga, const Vec128 & pal_br, const Vec128 & pal_ga)
{
	// Error reduction. Computed in permuted order; we only need to test every pixel
	// against all 4 palette entries, which we can use to save a few shuffles.
	Vec128 best;

	best = DeltaSqrRGBA_SSE2(row_br, row_ga, pal_br, pal_ga);
	best = _mm_min_epi32(best, DeltaSqrRGBA_SSE2(row_br, row_ga, shuffle32<1,2,3,0>(pal_br), shuffle32<1,2,3,0>(pal_ga)));
	best = _mm_min_epi32(best, DeltaSqrRGBA_SSE2(row_br, row_ga, shuffle32<2,3,0,1>(pal_br), shuffle32<2,3,0,1>(pal_ga)));
	best = _mm_min_epi32(best, DeltaSqrRGBA_SSE2(row_br, row_ga, shuffle32<3,0,1,2>(pal_br), shuffle32<3,0,1,2>(pal_ga)));

	return best;
}

static RADFORCEINLINE void UnpackBGRAto16_Weighted(Vec128 & out_br, Vec128 & out_ga, const Vec128 & pixels)
{
	const Vec128 mask_br = _mm_set1_epi16(0xff);
	const Vec128 scale_ga = _mm_set1_epi32(0x02000100);

	out_br = _mm_and_si128(pixels, mask_br);
	out_ga = _mm_maddubs_epi16(pixels, scale_ga);
}

static RADFORCEINLINE void UnpackBGRAto16_Weighted_2x(Vec128 & out_br, Vec128 & out_ga, const Vec128 & pixels)
{
	const Vec128 scale_br = _mm_set1_epi16(2);
	const Vec128 scale_ga = _mm_set1_epi32(0x04000200);

	out_br = _mm_maddubs_epi16(pixels, scale_br);
	out_ga = _mm_maddubs_epi16(pixels, scale_ga);
}

// Given the results of BC1_FindBestIndices_SSE4, extract the 2-bit indices in the low bits
// of the 8 lanes of the two given vectors (corresponding to four rows of pixels)
// into a packed 16-bit BC1 index representation.
static RADFORCEINLINE U32 BC1_ExtractIndices_SSE4(const Vec128 & row0, const Vec128 & row1)
{
	const Vec128 const32_3 = _mm_set1_epi32(3);
	const Vec128 magic_mul = _mm_set1_epi16(0x4080);

	Vec128 best_inds0 = _mm_and_si128(row0, const32_3);
	Vec128 best_inds1 = _mm_and_si128(row1, const32_3);
	Vec128 packed = _mm_packs_epi32(best_inds0, best_inds1); // now we have 16-bit fields
	Vec128 magicked = _mm_mullo_epi16(packed, magic_mul); // move bit 0 of index into bit 7 of 16b lane, and bit 1 of index into bit 15
	return _mm_movemask_epi8(magicked); // movemask does the rest
}

// Packs two rrColor32BGRA DWord endpoints in the fashion expected by BC1_ComputePalette_SSE4.
// Namely, as 16-bit lanes.
static RADFORCEINLINE Vec128_S16 BC1_PackEndpointColors_SSE4(U32 ep0, U32 ep1)
{
	const Vec128_U8 ep8(_mm_insert_epi32(_mm_cvtsi32_si128(ep0), ep1, 1));
	return ep8.to_s16_lo();
}

// Takes two endpoints in "U64" format (B, G, R in word 0, 1, 2 of a QWord respectively,
// first QWord for ep0 and second for ep1, with value range of [0,31] for B/R and [0,63] for G)
// and expands to BGRA8888 colors (with 255 alpha), matching BC1 endpoint decoding.
//
// BC1_ComputePalette_SSE4 wants these with 16 bits per channel (but still containing 8-bit values)
// so that's what we return.
static RADFORCEINLINE Vec128_S16 BC1_DequantEndpointsU32_SSE4(const Vec128_S16 & endpoints16)
{
	// Unpacking B and R is (x*33)>>2; unpacking G is (x*65)>>4.
	// Scale the B/R multiplier so we have a single shift-right by 2.
	const Vec128_S16 expanded = (endpoints16 * Vec128_S16::repeat4(33<<2, 65, 33<<2, 0)).srl<4>();

	// Finally, set the alpha channel values to 255
	const Vec128 result = _mm_or_si128(expanded, _mm_setr_epi16(0,0,0,255, 0,0,0,255));

	return Vec128_S16(result);
}

// Takes two endpoints in bitpacked format (as in BC1 blocks) and unpacks
// for use by BC1_ComputePalette_SSE4
static RADFORCEINLINE Vec128_S16 BC1_DequantEndpointsU16_SSE4(U32 packed)
{
	Vec128_S32 packed_in_vec { _mm_cvtsi32_si128(packed) };

	// Shuffle the right bytes and use multiplies to shift so that we put the
	// relevant bits for B, G, R into the top bits of 16-bit lanes
	//
	// First, broadcast the first 16 bits (first endpoint) into the low 4 16-bit lanes, and the
	// high 16 bits into the high 4 16-bit lanes.
	Vec128_U16 packed_eps = packed_in_vec.u8().shuf(Vec128_U8 { 0,1, 0,1, 0,1, 0,1, 2,3, 2,3, 2,3, 2,3 }).u16();

	// Use a multiply to shift B, G, R up so their top aligns with the lane MSB
	Vec128_U16 top_aligned = packed_eps * Vec128_U16::repeat4(1<<11, 1<<5, 1<<0, 0);

	// Mask off the junk bits below
	Vec128_U16 masked = top_aligned & Vec128_U16::repeat4(0xf800, 0xfc00, 0xf800, 0);

	// Use another multiply to do the bit replication and scale everything down so it's in [0,255]
	Vec128_U16 scaled_down = masked.mulhi(Vec128_U16::repeat4((1<<8) + (1<<3), (1<<8) + (1<<2), (1<<8) + (1<<3), 0));

	// Finally, set the alpha channel value to 255
	Vec128_U16 result = scaled_down | Vec128_U16::repeat4(0, 0, 0, 255);

	return result.s16();
}

// "endpoints" needs to have the two endpoints in the first two DWords
// with alpha set to 255.
//
// This implements the blended palette calc we have in rrdxtcblock.cpp DXT1_ComputePalette_RefImpl,
// see comments there.
static RADFORCEINLINE Vec128 BC1_ComputePalette_SSE4(const Vec128_S16 & endpoints16, rrDXT1PaletteMode mode, bool e0_gt_e1)
{
	RR_COMPILER_ASSERT(rrDXT1PaletteMode_NoAlpha == 0);
	RR_COMPILER_ASSERT(rrDXT1PaletteMode_Alpha == 1);
	RR_COMPILER_ASSERT(rrDXT1PaletteMode_FourColor == 2);

	// NOTE: b,g,r,a channel order!
	#define WHALF	(128 << 7),(128 << 7),(128 << 7),(128 << 7)
	#define WTHIRD	( 85 << 7),( 83 << 7),( 85 << 7),( 85 << 7) // NOTE: the 83 is intentional, not a typo!
	#define MRGBA	-1, -1, -1, -1
	#define M000A	0, 0, 0, -1
	#define M0000	0, 0, 0, 0

	static RAD_ALIGN(const S16, constants_table[3][2][16], 16) = // [mode][e0_gt_e1][i]
	{
		// NoAlpha; sets alpha of "transparent black" pixel to 255 so our 4-channel RGBA diffs work (our "free black")
		{
			{ WHALF,  WHALF,  MRGBA, M000A }, // e0 <= e1
			{ WTHIRD, WTHIRD, MRGBA, MRGBA }, // e0 > e1
		},
		// Alpha; decodes as intended, i.e. color 3 in 3-color mode=transparent black
		{
			{ WHALF,  WHALF,  MRGBA, M0000 }, // e0 <= e1
			{ WTHIRD, WTHIRD, MRGBA, MRGBA }, // e0 > e1
		},
		// FourColor; decodes like BC3/BC5 color blocks (always in four-color mode)
		{
			{ WTHIRD, WTHIRD, MRGBA, MRGBA }, // e0 <= e1
			{ WTHIRD, WTHIRD, MRGBA, MRGBA }, // e0 > e1
		},
	};

	#undef WHALF
	#undef WTHIRD
	#undef MRGBA
	#undef M000A
	#undef M0000

	const S16 * constants = constants_table[mode][e0_gt_e1];

	// Swap endpoints 0 and 1
	const Vec128_S16 endpoints16_other(shuffle32<2,3,0,1>(endpoints16)); // ep1.bgra, then ep0.bgra (16b lanes)

	// We interpolate as rounding_shift_right((hi - lo) * lerp_f, 15) + lo
	const Vec128_S16 lerpf = Vec128_S16::loada(constants);
	const Vec128_S16 interp = (endpoints16_other - endpoints16).mulhrs(lerpf) + endpoints16;

	// We need to apply a mask to clear the alpha of the transparent palette
	// entry in rrDXT1PaletteMode_Alpha (in other modes this is a NOP)
	const Vec128_S16 interp_masked = interp & Vec128_S16::loada(constants + 8);

	// Pack the results back into 8 bits (they're in range), merge with the original endpoints
	// and we're done!
	const Vec128_U8 merged = endpoints16.to_u8_sat(interp_masked);

	return merged;
}

} // namespace internal

#endif

#ifdef DO_BUILD_NEON64

namespace internal {

struct NEONKernelTraits
{
	static Vec128_U32 sumsqr4_u8(Vec128_U8 x)
	{
		return Vec128_U32::sqr_sum(x);
	}
};

// RGB function assumes alpha is equal for x and y
template<typename Tkernel=NEONKernelTraits>
static RADFORCEINLINE Vec128_U32 DeltaSqrRGB_NEON(const Vec128_U8 & x, const Vec128_U8 & y)
{
   // per component abs diff as 8-bit
   Vec128_U8 diff = abs_diff(x, y);

   // four lanes with db*db + dg*dg + dr*dr value for four pixels
   return Tkernel::sumsqr4_u8(diff);
}

template<bool t_extra_alpha_penalty, typename Tkernel=NEONKernelTraits>
static RADFORCEINLINE Vec128_U32 DeltaSqrRGBA_NEON(const Vec128_U8 & x, const Vec128_U8 & y)
{
	// Our canonical DeltaSqrRGBA weights squared A error 4x. A values in BC1
	// are always 0 or 255 in both the source and destination, therefore A
	// differences are always 4*255*255, larger than the maximum error
	// contribution from RGB channels (3*255*255). The idea being that this makes
	// us never change transparency state.
	//
	// The cases the actually use this weighting (FindBestErrors, FindBestIndices) immediately
	// do a min reduction right after, and the only point of this extra penalty is to make
	// the palette entries that change A always lose to the ones that don't.
	//
	// In other words, the exact error value we produce there doesn't matter as long as
	// it's larger than 3*255*255, which means even changing R,G,B all 0<->255 is
	// preferable to changing the transparency state.
	//
	// Therefore we can use whatever error value is convenient for that purpose.
	//
	// The remaining user of this function is AnyIndexD, which doesn't do any alpha weighting
	// at all.

	// per component abs diff as 8-bit
	Vec128_U8 diff = abs_diff(x, y);

	// four r/g/b/a sq.diff added as 32-bit value for four pixels of row
	Vec128_U32 sum = Tkernel::sumsqr4_u8(diff);

	if (t_extra_alpha_penalty)
	{
		// Existing RGBA error sums we just computed are <=4*255*255, so we can just OR
		// 1<<24 into the error conditionally. Bit 24 is set in "diff" when pixel A != palette A,
		// and is always clear in "sum". This instruction inserts bit 24 from diff into sum.
		sum = vbslq_u32(vdupq_n_u32(1u << 24), diff, sum);
	}

	return sum;
}

template<typename Tkernel=NEONKernelTraits>
static RADFORCEINLINE Vec128_U32 BC1_FindBestIndices_NEON(const Vec128_U8 & row, const Vec128_U8 & pal)
{
	// DeltaSqr result is scaled by 4 to leave place for 2 index bits
	// put the index bits in with the shift
	Vec128_U32 d0 = DeltaSqrRGBA_NEON<true, Tkernel>(row, pal.dup32<0>());
	Vec128_U32 d1 = DeltaSqrRGBA_NEON<true, Tkernel>(row, pal.dup32<1>());
	Vec128_U32 d2 = DeltaSqrRGBA_NEON<true, Tkernel>(row, pal.dup32<2>());
	Vec128_U32 d3 = DeltaSqrRGBA_NEON<true, Tkernel>(row, pal.dup32<3>());

	// Shift left by 2, put in the index bits
	d0 = d0.shl<2>();
	d1 = d1.sli<2>(Vec128_U32(1));
	d2 = d2.sli<2>(Vec128_U32(2));
	d3 = d3.sli<2>(Vec128_U32(3));

	// Min reduction
	Vec128_U32 best01 = d0.min(d1);
	Vec128_U32 best23 = d2.min(d3);
	Vec128_U32 best = best01.min(best23);

	// Now have 2b best color index in low bits of every lane
	// and squared error in top 30 bits.
	return best;
}

template<typename Tkernel=NEONKernelTraits>
static RADFORCEINLINE Vec128_U32 BC1_FindBestErrors_NEON(const Vec128_U8 & row, const Vec128_U8 & pal)
{
	// Error reduction. Computed in permuted order; we only need to test every pixel
	// against all 4 palette entries, which we can use to save a few shuffles.
	Vec128_U32 best = DeltaSqrRGBA_NEON<true, Tkernel>(row, pal);
	best = best.min(DeltaSqrRGBA_NEON<true, Tkernel>(row, pal.u32().yzwx().u8()));
	best = best.min(DeltaSqrRGBA_NEON<true, Tkernel>(row, pal.u32().zwxy().u8()));
	best = best.min(DeltaSqrRGBA_NEON<true, Tkernel>(row, pal.u32().wxyz().u8()));
	return best;
}

static RADFORCEINLINE U32 BC1_ExtractIndices_NEON(const Vec128_U32 & row0, const Vec128_U32 & row1)
{
	const Vec128_S16 shifts { 0, 2, 4, 6, 8, 10, 12, 14 };

	// 8 components with lowest 16-bits from both rows
	Vec128_U16 packed = pack_narrow(row0, row1);

	// take only index bits
	Vec128_U16 best_inds = packed & Vec128_U16(3);

	// shift them left into non-overlapping expected positions
	Vec128_U16 shifted = best_inds << shifts;

	// add 16-bit components together to extract bits
	return reduce_add(shifted);
}

// Takes two endpoints in "U64" format (B, G, R in word 0, 1, 2 of a QWord respectively,
// first QWord for ep0 and second for ep1, with value range of [0,31] for B/R and [0,63] for G)
// and expands to BGRA8888 colors (with 255 alpha), matching BC1 endpoint decoding.
//
// BC1_ComputePalette_NEON wants these with 16 bits per channel (but still containing 8-bit values)
// so that's what we return.
static RADFORCEINLINE Vec128_S16 BC1_DequantEndpointsU32_NEON(const Vec128_S16 & endpoints16)
{
	// Unpacking B and R is (x*33)>>2; unpacking G is (x*65)>>4.
	// Scale the B/R multiplier so we have a single shift-right by 2.
	const Vec128_S16 mul = { 33<<2, 65, 33<<2, 0,   33<<2, 65, 33<<2, 0 };
	const Vec128_S16 expanded = (endpoints16 * Vec128_S16(mul)).srl<4>();

	// Finally, set the alpha channel values to 255
	const Vec128_S16 amask = { 0,0,0,255,  0,0,0,255 };
	const Vec128_S16 result = expanded | Vec128_S16(amask);

	return result;
}

// Takes two endpoints in bitpacked format (as in BC1 blocks) and unpacks
// for use by BC1_ComputePalette_SSE4
static RADFORCEINLINE Vec128_S16 BC1_DequantEndpointsU16_NEON(U32 packed)
{
	Vec128_U32 packed_in_vec { packed };

	// Shuffle the right bytes and use multiplies to shift so that we put the
	// relevant bits for B, G, R into the top bits of 16-bit lanes
	//
	// First, broadcast the first 16 bits (first endpoint) into the low 4 16-bit lanes, and the
	// high 16 bits into the high 4 16-bit lanes.
	Vec128_U16 packed_eps = packed_in_vec.u8().shuf(Vec128_U8 { 0,1, 0,1, 0,1, 0,1, 2,3, 2,3, 2,3, 2,3 }).u16();

	// Shift B, G, R so their top bit is at bit 7 in their respective lanes
	Vec128_U16 top_aligned = packed_eps << Vec128_S16::repeat4(7 - 4, 7 - 10,7 - 15,0);

	// Mask off the junk bits below
	Vec128_U16 masked = top_aligned & Vec128_U16::repeat4(0xf8, 0xfc, 0xf8, 0);

	// Do another variable shift to do the bit replication and accumulate
	Vec128_U16 replicated = masked + (masked << Vec128_S16::repeat4(-5, -6, -5, 0));

	// Finally, set the alpha channel value to 255
	Vec128_U16 result = replicated | Vec128_U16::repeat4(0, 0, 0, 255);

	return result.s16();
}

// Packs two rrColor32BGRA DWord endpoints in the fashion expected by BC1_ComputePalette_NEON.
static RADFORCEINLINE Vec128_S16 BC1_PackEndpointColors_NEON(U32 ep0, U32 ep1)
{
	uint32x2_t packed = vcreate_u32(U64(ep0) | (U64(ep1) << 32));
	return Vec128_U16(vmovl_u8(vreinterpret_u8_u32(packed))).s16();
}

// "endpoints" needs to have the two endpoints in the first two DWords
// with alpha set to 255.
//
// This implements the blended palette calc we have in rrdxtcblock.cpp DXT1_ComputePalette_RefImpl,
// see comments there.
static RADFORCEINLINE Vec128_U8 BC1_ComputePalette_NEON(const Vec128_S16 & endpoints16, rrDXT1PaletteMode mode, bool e0_gt_e1)
{
	RR_COMPILER_ASSERT(rrDXT1PaletteMode_NoAlpha == 0);
	RR_COMPILER_ASSERT(rrDXT1PaletteMode_Alpha == 1);
	RR_COMPILER_ASSERT(rrDXT1PaletteMode_FourColor == 2);

	// NOTE: b,g,r,a channel order!
	#define WHALF  128, 128, 128, 128
	#define WTHIRD  85,  83,  85,  85 // NOTE: the 83 is intentional, not a typo!

	#define MRGBA   -1,  -1,  -1,  -1
	#define M000A    0,   0,   0,  -1
	#define M0000    0,   0,   0,   0

	alignas(16) static const S16 constants_table[3][2][2][8] = // [mode][e0_gt_e1][lerp/mask]
	{
		// NoAlpha; sets alpha of "transparent black" pixel to 255 so our 4-channel RGBA diffs work (our "free black")
		{
			{ { WHALF,  WHALF  }, { MRGBA, M000A } }, // e0 <= e1
			{ { WTHIRD, WTHIRD }, { MRGBA, MRGBA } }, // e0 > e1
		},
		// Alpha; decodes as intended, i.e. color 3 in 3-color mode=transparent black
		{
			{ { WHALF,  WHALF  }, { MRGBA, M0000 } }, // e0 <= e1
			{ { WTHIRD, WTHIRD }, { MRGBA, MRGBA } }, // e0 > e1
		},
		// FourColor; decodes like BC3/BC5 color blocks (always in four-color mode)
		{
			{ { WTHIRD, WTHIRD }, { MRGBA, MRGBA } }, // e0 <= e1
			{ { WTHIRD, WTHIRD }, { MRGBA, MRGBA } }, // e0 > e1
		},
	};

	#undef WHALF
	#undef WTHIRD
	#undef MRGBA
	#undef M000A
	#undef M0000

	const int16x8_t * constants = (const int16x8_t *)constants_table[mode][e0_gt_e1];

	// Swap endpoints 0 and 1
	const Vec128_S16 endpoints16_other = vextq_s16(endpoints16, endpoints16, 4); // ep1.bgra, then ep0.bgra (8b lanes)

	// We interpolate as rounding_shift_right((hi - lo) * lerp_f, 8) + lo
	const Vec128_S16 lerpf = constants[0];
	const Vec128_S16 interp = ((endpoints16_other - endpoints16) * lerpf).rnd_sra<8>() + endpoints16;

	// We need to apply a mask to clear the alpha of the transparent palette
	// entry in rrDXT1PaletteMode_Alpha (in other modes this is a NOP)
	const Vec128_S16 masked = interp & constants[1];

	// Pack the results back into 8 bits (they're in range), merge with the original endpoints
	// and we're done!
	const Vec128_S8 merged = pack_narrow(endpoints16, masked);

	return merged.u8();
}

} // namespace internal

#endif

OODLE_NS_END

