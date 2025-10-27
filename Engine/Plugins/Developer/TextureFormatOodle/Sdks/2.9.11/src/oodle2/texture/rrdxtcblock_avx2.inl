// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "vec256.inl"

#ifdef DO_BUILD_AVX2

OODLE_NS_START

namespace internal {

static RADFORCEINLINE Vec256 DeltaSqrRGBA_AVX2(
	const Vec256 & x_br16, const Vec256 & x_ga16, const Vec256 & y_br16, const Vec256 & y_ga16)
{
	Vec256 diff_br16 = _mm256_sub_epi16(y_br16, x_br16); // db, dr
	Vec256 diff_ga16 = _mm256_sub_epi16(y_ga16, x_ga16); // dg, da

	Vec256 sumsq_br32 = _mm256_madd_epi16(diff_br16, diff_br16); // db*db + dr*dr
	Vec256 sumsq_ga32 = _mm256_madd_epi16(diff_ga16, diff_ga16); // dg*dg + da*da

	return _mm256_add_epi32(sumsq_br32, sumsq_ga32); // db*db + dg*dg + dr*dr + da*da
}

// Applies the A weighting (2x so that its squared error ends up 4x)
static RADFORCEINLINE void UnpackBGRAto16_Weighted(Vec256 & out_br, Vec256 & out_ga, const Vec256 & pixels)
{
	const Vec256 mask_br = _mm256_set1_epi16(0xff);
	const Vec256 scale_ga = _mm256_set1_epi32(0x02000100);

	out_br = _mm256_and_si256(pixels, mask_br);
	out_ga = _mm256_maddubs_epi16(pixels, scale_ga);
}

// Applies the A weighting (2x so that its squared error ends up 4x),
// and also scales everything up 2x in general.
static RADFORCEINLINE void UnpackBGRAto16_Weighted_2x(Vec256 & out_br, Vec256 & out_ga, const Vec256 & pixels)
{
	const Vec256 scale_br = _mm256_set1_epi16(2);
	const Vec256 scale_ga = _mm256_set1_epi32(0x04000200);

	out_br = _mm256_maddubs_epi16(pixels, scale_br);
	out_ga = _mm256_maddubs_epi16(pixels, scale_ga);
}

// Given four Vec256s of pixels:
//   row_br = scaled S16 "blue/red" channels for 8 pixels (b0, r0, b1, r1, ..., b7, r7)
//   row_ga = scaled S16 "green/alpha" channels for 8 pixels (g0, a0, g1, a1, ..., g7, a7)
//   pal_br, pal_ga laid out similarly
//
// determine, per pixel i
//
//   min(len_sq(row[i] - pal[j + 0]) + 0, len_sq(row[i] - pal[j + 1]) + 1, len_sq(row[i] - pal[j + 2]) + 2, len_sq(row[i] - pal[j + 3]) + 3)
//
// and return it as a vector of 8 S32 values, where j is 0 for the bottom 4 lanes
// and 4 for the top 4 lanes. This looks funny but there are several possible
// use cases for this, most of which assume that the per-channel row/pal values are
// pre-scaled by a factor of 2 each. This multiplies the squared distances by a factor of 4,
// meaning the above expression gives the squared distance in the top 30 bits and the
// corresponding index in the bottom 2 bits of each lane.
//
// The channel mapping here is not required, it's just the one we commonly use for BC1.
//
// The two main use cases, which are in a sense dual to each other, are:
//
// 1. row_br and row_ga are 8 independent pixels; pal_br and pal_ga have the same 4 palette
//    entries in their low and high halves. This allows determining the best index for
//    8 BC1 pixels at once.
// 2. row_br and row_ga are 4 independent pixels, with the same 128-bit values replicated
//    in their low and high halves, while pal_br and pal_gr contain two independent palettes.
//    This allows matching the same pixels against two candidate palettes at once.
static RADFORCEINLINE Vec256 BC1_FindBestIndices_AVX2(const Vec256 & row_br, const Vec256 & row_ga, const Vec256 & pal_br, const Vec256 & pal_ga)
{
	const Vec256 const32_1 = _mm256_set1_epi32(1);
	const Vec256 const32_2 = _mm256_set1_epi32(2);

	// Calculate the squared deltas
	Vec256 d0 = DeltaSqrRGBA_AVX2(row_br, row_ga, _mm256_shuffle_epi32(pal_br, 0x00), _mm256_shuffle_epi32(pal_ga, 0x00));
	Vec256 d1 = DeltaSqrRGBA_AVX2(row_br, row_ga, _mm256_shuffle_epi32(pal_br, 0x55), _mm256_shuffle_epi32(pal_ga, 0x55));
	Vec256 d2 = DeltaSqrRGBA_AVX2(row_br, row_ga, _mm256_shuffle_epi32(pal_br, 0xaa), _mm256_shuffle_epi32(pal_ga, 0xaa));
	Vec256 d3 = DeltaSqrRGBA_AVX2(row_br, row_ga, _mm256_shuffle_epi32(pal_br, 0xff), _mm256_shuffle_epi32(pal_ga, 0xff));

	// Put the right values into the low bits then perform the min reduction
	// written slightly oddly to reduce register pressure by needing 1 fewer constant
	Vec256 best01 = _mm256_min_epi32(d0, _mm256_add_epi32(d1, const32_1));
	Vec256 best23 = _mm256_min_epi32(d2, _mm256_add_epi32(d3, const32_1));
	Vec256 best0123 = _mm256_min_epi32(best01, _mm256_add_epi32(best23, const32_2));

	// Now have 2b best color index in low bits of every lane
	// and squared error in top 30 bits.
	return best0123;
}

// Version of the above that calculates just the minimum errors without the
// corresponding indices, which ends up appreciably cheaper if you do many
// of these computations, and we do.
static RADFORCEINLINE Vec256 BC1_FindBestErrors_AVX2(const Vec256 & row_br, const Vec256 & row_ga, const Vec256 & pal_br, const Vec256 & pal_ga)
{
	// The natural thing to do here is to reduce against the 4 one-32bit-lane broadcasts,
	// however all that matters is that we test each pixel against all 4 palette entries.
	// Instead of broadcasts, we can start with the identity permutation and then do 3
	// cyclic permutations instead to get the other 3 options for each pixel, saving
	// two shuffles.
	Vec256 best = DeltaSqrRGBA_AVX2(pal_br, pal_ga, row_br, row_ga);
	best = _mm256_min_epi32(best, DeltaSqrRGBA_AVX2(row_br, row_ga, shuffle32in128<1,2,3,0>(pal_br), shuffle32in128<1,2,3,0>(pal_ga)));
	best = _mm256_min_epi32(best, DeltaSqrRGBA_AVX2(row_br, row_ga, shuffle32in128<2,3,0,1>(pal_br), shuffle32in128<2,3,0,1>(pal_ga)));
	best = _mm256_min_epi32(best, DeltaSqrRGBA_AVX2(row_br, row_ga, shuffle32in128<3,0,1,2>(pal_br), shuffle32in128<3,0,1,2>(pal_ga)));

	return best;
}

// Given the results of the above, extract the 2-bit indices in the low bits of
// the 16 lanes of the two given vectors (corresponding to four rows of pixels)
// into a packed 32-bit BC1 index representation.
static RADFORCEINLINE U32 BC1_ExtractIndices_AVX2(const Vec256 & rows01, const Vec256 & rows23)
{
	const Vec256 const32_3 = _mm256_set1_epi32(3);
	const Vec256 magic_mul = _mm256_set1_epi16(0x4080);
	const Vec256 pack_reorder = _mm256_setr_epi32(0,1, 4,5, 2,3, 6,7);

	// Extract the "best index" field in the low 2 bits of every lane
	Vec256 best01 = _mm256_and_si256(rows01, const32_3);
	Vec256 best23 = _mm256_and_si256(rows23, const32_3);

	// Pack down into a vector with 16-bit fields
	// This gives a funny order, which we reorder back into linear with a permute
	Vec256 packed = _mm256_packs_epi32(best01, best23); // now we have 16-bit fields
	packed = _mm256_permutevar8x32_epi32(packed, pack_reorder); // order fixed!

	// Finally, align the bits so we can grab them with a movemask
	Vec256 magicked = _mm256_mullo_epi16(packed, magic_mul); // move bit 0 of index into bit 7 of 16b lane, and bit 1 of index into bit 15
	return _mm256_movemask_epi8(magicked); // movemask does the rest
}

}

OODLE_NS_END

#endif // DO_BUILD_AVX2

