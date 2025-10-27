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

static RADFORCEINLINE Vec256_F32 CalcActivityWeightedErrs(const Vec256_U8 pixels1, const Vec256_U8 pixels2, const float *activity)
{
	// Compute the squared errors
	Vec256_S32 squares32 = Vec256_U32::ssd4(pixels1, pixels2).s32();

	// Convert to float and apply activity weighting
	return squares32.to_f32() * Vec256_F32::loadu(activity);
}

static RADFORCEINLINE Vec256_F32 CalcActivityWeightedErrs(const Vec256_U8 ref_pixels, const Vec256_U8 palette, const Vec256_S32 inds, const float * activity)
{
	// Select the right palette entries
	Vec256_U8 our_pixels = palette.s32().permute(inds).u8();
	return CalcActivityWeightedErrs(ref_pixels, our_pixels, activity);
}

static RADFORCEINLINE F32 ActivityErrorSum(Vec256_F32 errs01, Vec256_F32 errs23)
{
	// Sum them in the same order we do in regular VQD
	VecF32x4 accum;
	accum = lo_half(errs01);
	accum += hi_half(errs01);
	accum += lo_half(errs23);
	accum += hi_half(errs23);

	// Final sum in the same order as in regular VQD
	F32 sum = accum.sum_across_inner_outer().scalar_x();

	return sum;
}

F32 VQD_BC1_AVX2(const rrColorBlock4x4 & colors,const rrColor32BGRA palette[4],const U32 in_indices,const SingleFloatBlock4x4 & activity)
{
	// Palette
	// Make two copies so the high bit of the index doesn't matter
	Vec256_U8 v_palette = Vec256_U8::loadu_dup128(palette);

	// Pre-shift the indices
	Vec256_S32 v_inds = Vec256_S32(in_indices) >> Vec256_S32 { 0,2,4,6, 8,10,12,14 };

	// Calculate the errors for all 16 pixels
	Vec256_F32 errs01 = CalcActivityWeightedErrs(Vec256_U8::loadu(colors.colors + 0), v_palette, v_inds, activity.values + 0);
	Vec256_F32 errs23 = CalcActivityWeightedErrs(Vec256_U8::loadu(colors.colors + 8), v_palette, v_inds.srl<16>(), activity.values + 8);

	return ActivityErrorSum(errs01, errs23);
}

F32 VQD_AVX2(const rrColorBlock4x4 & colors1,const rrColorBlock4x4 & colors2,const SingleFloatBlock4x4 & activity)
{
	Vec256_F32 errs01 = CalcActivityWeightedErrs(Vec256_U8::loadu(colors1.colors + 0), Vec256_U8::loadu(colors2.colors + 0), activity.values + 0);
	Vec256_F32 errs23 = CalcActivityWeightedErrs(Vec256_U8::loadu(colors1.colors + 8), Vec256_U8::loadu(colors2.colors + 8), activity.values + 8);

	return ActivityErrorSum(errs01, errs23);
}

} // internal namespace

OODLE_NS_END

#endif // DO_BUILD_AVX2

