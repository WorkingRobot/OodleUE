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
	const Vec128 scale_ga = _mm_set1_epi32(0x020001);

	out_br = _mm_and_si128(pixels, mask_br);
	out_ga = _mm_mullo_epi16(_mm_srli_epi16(pixels, 8), scale_ga);
}

static RADFORCEINLINE void UnpackBGRAto16_Weighted_2x(Vec128 & out_br, Vec128 & out_ga, const Vec128 & pixels)
{
	const Vec128 mask_br = _mm_set1_epi16(0xff);
	const Vec128 scale_ga = _mm_set1_epi32(0x040002);

	out_br = _mm_and_si128(pixels, mask_br);
	out_ga = _mm_srli_epi16(pixels, 8);

	out_br = _mm_add_epi16(out_br, out_br);
	out_ga = _mm_mullo_epi16(out_ga, scale_ga);
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
	return ep8.to_sint16_lo();
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
	const Vec128_U8 merged = endpoints16.to_uint8_sat(interp_masked);

	return merged;
}

} // namespace internal

#endif

OODLE_NS_END

