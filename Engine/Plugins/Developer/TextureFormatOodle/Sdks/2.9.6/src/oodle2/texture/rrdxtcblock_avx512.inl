// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "vec512.inl"

#ifdef DO_BUILD_AVX512

OODLE_NS_START

namespace internal {

static RADFORCEINLINE Vec512 DeltaSqrRGBA_AVX512(
	const Vec512 & x_br16, const Vec512 & x_ga16, const Vec512 & y_br16, const Vec512 & y_ga16)
{
	Vec512 diff_br16 = _mm512_sub_epi16(y_br16, x_br16); // db, dr
	Vec512 diff_ga16 = _mm512_sub_epi16(y_ga16, x_ga16); // dg, da

	Vec512 sumsq_br32 = _mm512_madd_epi16(diff_br16, diff_br16); // db*db + dr*dr
	Vec512 sumsq_ga32 = _mm512_madd_epi16(diff_ga16, diff_ga16); // dg*dg + da*da

	return _mm512_add_epi32(sumsq_br32, sumsq_ga32); // db*db + dg*dg + dr*dr + da*da
}

// Applies the A weighting (2x so that its squared error ends up 4x)
static RADFORCEINLINE void UnpackBGRAto16_Weighted(Vec512 & out_br, Vec512 & out_ga, const Vec512 & pixels)
{
	const Vec512 mask_br = _mm512_set1_epi16(0xff);
	const Vec512 scale_ga = _mm512_set1_epi32(0x2000100); // right-shift G by 8, A by 7 (so A ends up 2x)

	out_br = _mm512_and_si512(pixels, mask_br);
	out_ga = _mm512_mulhi_epu16(_mm512_andnot_si512(mask_br, pixels), scale_ga);
}

// With pixel vectors in the first two array entries, pre-populate the other 6 entries. See below.
static RADFORCEINLINE void Pixel_PreRotate(Vec512 out_prerot[8])
{
	out_prerot[2] = shuffle32in128<3,0,1,2>(out_prerot[0]);
	out_prerot[3] = shuffle32in128<3,0,1,2>(out_prerot[1]);
	out_prerot[4] = shuffle32in128<2,3,0,1>(out_prerot[0]);
	out_prerot[5] = shuffle32in128<2,3,0,1>(out_prerot[1]);
	out_prerot[6] = shuffle32in128<1,2,3,0>(out_prerot[0]);
	out_prerot[7] = shuffle32in128<1,2,3,0>(out_prerot[1]);
}

// Same as the above, but also apply pre-rotation for the FindBestErrorsPrerot kernel below
static RADFORCEINLINE void UnpackBGRAto16_Weighted_Prerot(Vec512 out_prerot[8], const Vec512 & pixels)
{
	UnpackBGRAto16_Weighted(out_prerot[0], out_prerot[1], pixels);
	Pixel_PreRotate(out_prerot);
}

// Applies the A weighting (2x so that its squared error ends up 4x),
// and also scales everything up 2x in general.
static RADFORCEINLINE void UnpackBGRAto16_Weighted_2x(Vec512 & out_br, Vec512 & out_ga, const Vec512 & pixels)
{
	const Vec512 mask_br = _mm512_set1_epi16(0xff);
	const Vec512 shift_ga = _mm512_set1_epi32(0x00060007); // right-shift G by 7, A by 6

	out_br = _mm512_and_si512(pixels, mask_br);
	out_br = _mm512_add_epi16(out_br, out_br);
	out_ga = _mm512_srlv_epi16(_mm512_andnot_si512(mask_br, pixels), shift_ga);
}

// Given four Vec512s of pixels:
//   row_br = scaled S16 "blue/red" channels for 16 pixels (b0, r0, b1, r1, ..., b15, r15)
//   row_ga = scaled S16 "green/alpha" channels for 16 pixels (g0, a0, g1, a1, ..., g15, a15)
//   pal_br, pal_ga laid out similarly
//
// determine, per pixel i
//
//   min(len_sq(row[i] - pal[j + 0]) + 0, len_sq(row[i] - pal[j + 1]) + 1, len_sq(row[i] - pal[j + 2]) + 2, len_sq(row[i] - pal[j + 3]) + 3)
//
// and return it as a vector of 16 S32 values, where j is 0 for the bottom 4 lanes
// and 4 for the top 4 lanes. This looks funny but there are several possible
// use cases for this, most of which assume that the per-channel row/pal values are
// pre-scaled by a factor of 2 each. This multiplies the squared distances by a factor of 4,
// meaning the above expression gives the squared distance in the top 30 bits and the
// corresponding index in the bottom 2 bits of each lane.
//
// The channel mapping here is not required, it's just the one we commonly use for BC1.
static RADFORCEINLINE Vec512 BC1_FindBestIndices_AVX512(const Vec512 & row_br, const Vec512 & row_ga, const Vec512 & pal_br, const Vec512 & pal_ga)
{
	const Vec512 const32_1 = _mm512_set1_epi32(1);
	const Vec512 const32_2 = _mm512_set1_epi32(2);

	// Calculate the squared deltas
	Vec512 d0 = DeltaSqrRGBA_AVX512(row_br, row_ga, _mm512_shuffle_epi32(pal_br, 0x00), _mm512_shuffle_epi32(pal_ga, 0x00));
	Vec512 d1 = DeltaSqrRGBA_AVX512(row_br, row_ga, _mm512_shuffle_epi32(pal_br, 0x55), _mm512_shuffle_epi32(pal_ga, 0x55));
	Vec512 d2 = DeltaSqrRGBA_AVX512(row_br, row_ga, _mm512_shuffle_epi32(pal_br, 0xaa), _mm512_shuffle_epi32(pal_ga, 0xaa));
	Vec512 d3 = DeltaSqrRGBA_AVX512(row_br, row_ga, _mm512_shuffle_epi32(pal_br, 0xff), _mm512_shuffle_epi32(pal_ga, 0xff));

	// Put the right values into the low bits then perform the min reduction
	// written slightly oddly to reduce register pressure by needing 1 fewer constant
	Vec512 best01 = _mm512_min_epi32(d0, _mm512_add_epi32(d1, const32_1));
	Vec512 best23 = _mm512_min_epi32(d2, _mm512_add_epi32(d3, const32_1));
	Vec512 best0123 = _mm512_min_epi32(best01, _mm512_add_epi32(best23, const32_2));

	// Now have 2b best color index in low bits of every lane
	// and squared error in top 30 bits.
	return best0123;
}

// Given the results of the above, extract the 2-bit indices in the low bits of
// the 16 lanes of the two given vectors (corresponding to four rows of pixels)
// into a packed 32-bit BC1 index representation.
static RADFORCEINLINE U32 BC1_ExtractIndices_AVX512(const Vec512 & pixels)
{
	// Extract the two LSBs into mask regs via tests
	// using 16-bit lanes for tests since we want two-bit masks
	__mmask32 mbit0 = _mm512_test_epi16_mask(pixels, _mm512_set1_epi32(1));
	__mmask32 mbit1 = _mm512_test_epi16_mask(pixels, _mm512_set1_epi32(2));

	// Convert
	U32 bit0 = _cvtmask32_u32(mbit0);
	U32 bit1 = _cvtmask32_u32(mbit1);

	return bit0 + 2*bit1; // combine (this can be a single LEA)
}

// Version of the above that calculates just the minimum errors without the
// corresponding indices, which ends up appreciably cheaper if you do many
// of these computations, and we do.
static RADFORCEINLINE Vec512 BC1_FindBestErrors_AVX512(const Vec512 & row_br, const Vec512 & row_ga, const Vec512 & pal_br, const Vec512 & pal_ga)
{
	// The natural thing to do here is to reduce against the 4 one-32bit-lane broadcasts,
	// however all that matters is that we test each pixel against all 4 palette entries.
	// Instead of broadcasts, we can start with the identity permutation and then do 3
	// cyclic permutations instead to get the other 3 options for each pixel, saving
	// two shuffles.
	Vec512 best = DeltaSqrRGBA_AVX512(row_br, row_ga, pal_br, pal_ga);
	best = _mm512_min_epi32(best, DeltaSqrRGBA_AVX512(row_br, row_ga, shuffle32in128<1,2,3,0>(pal_br), shuffle32in128<1,2,3,0>(pal_ga)));
	best = _mm512_min_epi32(best, DeltaSqrRGBA_AVX512(row_br, row_ga, shuffle32in128<2,3,0,1>(pal_br), shuffle32in128<2,3,0,1>(pal_ga)));
	best = _mm512_min_epi32(best, DeltaSqrRGBA_AVX512(row_br, row_ga, shuffle32in128<3,0,1,2>(pal_br), shuffle32in128<3,0,1,2>(pal_ga)));

	return best;
}

// This is the version with pre-rotated pixels. Set up into this with
// UnpackBGRAto16_Weighted_Prerot.
static RADFORCEINLINE Vec512 BC1_FindBestErrorsPrerot_AVX512(const Vec512 pix_prerot[8], const Vec512 & pal_br, const Vec512 & pal_ga)
{
	// Similar idea to the above, but instead of rotating palette entries to the right,
	// we can rotate the pixels to the left (cyclically within 128-bit chunks). We can
	// do this once at setup and reuse the pre-rotated pixels for many evaluations.
	//
	// This makes our lane errors come out in the wrong position, so we need to rotate
	// the results back before we take the min. But when we take the min we're down to
	// vectors of 32-bit lanes (as opposed to two vectors of 32-bit lanes for the input),
	// so doing it this way cuts the number of shuffles in half, as long as you have
	// enough registers to keep the pix_prerot around; we do.
	Vec512 best = DeltaSqrRGBA_AVX512(pal_br, pal_ga, pix_prerot[0], pix_prerot[1]);
	best = _mm512_min_epi32(best, shuffle32in128<1,2,3,0>(DeltaSqrRGBA_AVX512(pal_br, pal_ga, pix_prerot[2], pix_prerot[3])));
	best = _mm512_min_epi32(best, shuffle32in128<2,3,0,1>(DeltaSqrRGBA_AVX512(pal_br, pal_ga, pix_prerot[4], pix_prerot[5])));
	best = _mm512_min_epi32(best, shuffle32in128<3,0,1,2>(DeltaSqrRGBA_AVX512(pal_br, pal_ga, pix_prerot[6], pix_prerot[7])));

	return best;
}

// Given 4 vectors worth of integers, compute the 4 horizontal sums
static RADFORCEINLINE Vec128 HorizontalSum4x(const Vec512 & a, const Vec512 & b, const Vec512 & c, const Vec512 & d)
{
	// The high concept here is to do a 4x4 matrix transpose between the four vectors in every 128-bit quadrant,
	// which lets us sum the 4 elements in each quadrant of each of the 4 vectors simultaneously with regular
	// adds. From there, we then have two more regular reduction steps left to get our 4 final results.
	// This is more efficient already than doing 4 separate horizontal reductions, since the matrix transpose steps
	// let us "share shuffles" (so to speak) between multiple vectors, whereas the regular reduction just keeps
	// getting narrower all the time.
	//
	// However, we can do a bit better still: by interleaving reduction and transpose passes, we can reduce
	// the amount of shuffles necessary in the second transpose pass, which reduces the total work further.

	// First pass of 4x4 transpose
	Vec512 t0 = _mm512_unpacklo_epi32(a, c);
	Vec512 t1 = _mm512_unpacklo_epi32(b, d);
	Vec512 t2 = _mm512_unpackhi_epi32(a, c);
	Vec512 t3 = _mm512_unpackhi_epi32(b, d);

	// First reduction pass
	Vec512 u0 = _mm512_add_epi32(t0, t2);
	Vec512 u1 = _mm512_add_epi32(t1, t3);

	// Second pass of transpose
	Vec512 v0 = _mm512_unpacklo_epi32(u0, u1);
	Vec512 v1 = _mm512_unpackhi_epi32(u0, u1);

	// Second reduction pass
	Vec512 w = _mm512_add_epi32(v0, v1);

	// Final reduction steps that get progressively narrower
	Vec256 x = _mm256_add_epi32(lo_half(w), hi_half(w));
	Vec128 y = _mm_add_epi32(lo_half(x), hi_half(x));

	return y;
}

}

OODLE_NS_END

#endif // DO_BUILD_AVX2

