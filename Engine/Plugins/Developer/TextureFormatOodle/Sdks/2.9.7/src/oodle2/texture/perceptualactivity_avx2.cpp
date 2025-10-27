// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetavx2

// We _MUST NOT_ allow the compiler to contract mul-add operations to FMAs;
// this changes the results between AVX2 and SSE4 versions which we don't allow!
#include "nocontract.h"

#include "perceptualactivity.h"
#include "perceptualactivity.inl"
#include "vec256.inl"

#ifdef DO_BUILD_AVX2

OODLE_NS_START

namespace internal {

static __m256 CalcActivityWeightedErrs(const Vec256 ref_pixels, const Vec256 palette, const Vec256 inds, const float * activity)
{
	// Select the right palette entries
	Vec256 our_pixels = _mm256_permutevar8x32_epi32(palette, inds);

	// Compute the squared errors
	Vec256 diff = _mm256_or_si256(_mm256_subs_epu8(our_pixels, ref_pixels), _mm256_subs_epu8(ref_pixels, our_pixels));

	Vec256 diff16_0 = _mm256_and_si256(diff, _mm256_set1_epi16(0xff)); // even lanes
	Vec256 diff16_1 = _mm256_srli_epi16(diff, 8); // odd lanes

	Vec256 squares32_0 = _mm256_madd_epi16(diff16_0, diff16_0);
	Vec256 squares32_1 = _mm256_madd_epi16(diff16_1, diff16_1);

	Vec256 squares32 = _mm256_add_epi32(squares32_0, squares32_1);

	// Convert to float and apply activity weighting
	__m256 squares32f = _mm256_cvtepi32_ps(squares32);
	__m256 weighted = _mm256_mul_ps(squares32f, _mm256_loadu_ps(activity));

	return weighted;
}

F32 VQD_BC1_AVX2(const rrColorBlock4x4 & colors,const rrColor32BGRA palette[4],const U32 in_indices,const SingleFloatBlock4x4 & activity)
{
	// Palette
	// Make two copies so the high bit of the index doesn't matter
	Vec256 v_palette = broadcast128_256(load128u(palette));

	// Pre-shift the indices
	Vec256 v_inds = _mm256_srlv_epi32(_mm256_set1_epi32(in_indices), _mm256_setr_epi32(0,2,4,6, 8,10,12,14));

	// Calculate the errors for all 16 pixels
	__m256 errs01 = CalcActivityWeightedErrs(load256u(colors.colors + 0), v_palette, v_inds, activity.values + 0);
	__m256 errs23 = CalcActivityWeightedErrs(load256u(colors.colors + 8), v_palette, _mm256_srli_epi32(v_inds, 16), activity.values + 8);

	// Sum them in the same order we do in regular VQD_BC1
	VecF32x4 accum;
	accum = lo_half(errs01);
	accum += hi_half(errs01);
	accum += lo_half(errs23);
	accum += hi_half(errs23);

	// Final sum in the same order as in regular VQD_BC1
	F32 sum = accum.sum_across_inner_outer().scalar_x();

	return sum;
}

} // internal namespace

OODLE_NS_END

#endif // DO_BUILD_AVX2

