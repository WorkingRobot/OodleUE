// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetsse4

#include "bc7kernels.h"
#include "vec128.inl"
#include "bc7kernels.inl"

OODLE_NS_START

namespace internal {

struct SubsetTraitsVec4
{
	static constexpr int LANES = 4;
	typedef Vec128_S32 VecInt;
	typedef Vec128_U16 VecUInt16;
	typedef VecF32x4 VecFlt;
};

} // namespace internal

void BC7SubsetAnalysis::compute(const U8 pixels[], const U32 subset_masks[], const F32 subset_scale[], SINTa subset_first, SINTa subset_count, bool is_rgba, CpuDispatchFlags dispatch)
{
	using namespace internal;

	RR_ASSERT_ALWAYS(0 <= subset_first && 0 <= subset_count && subset_first + subset_count + 8 <= stride);

#ifdef DO_BUILD_AVX2
	if ( dispatch.AVX2() )
	{
		internal::subset_prepare_avx2(storage + subset_first, pixels, subset_masks + subset_first, subset_scale + subset_first, subset_count, stride, is_rgba);
		return;
	}
#endif

#ifdef DO_BUILD_ARM_DOTPROD
	if ( dispatch.ARM_DotProd() )
	{
		internal::subset_prepare_armdp(storage + subset_first, pixels, subset_masks + subset_first, subset_scale + subset_first, subset_count, stride, is_rgba);
		return;
	}
#endif

#if defined(DO_BUILD_SSE4) || defined(DO_BUILD_NEON64)
	if (!is_rgba)
		internal::subset_prepare_rgb<SubsetTraitsVec4>(storage + subset_first, pixels, subset_masks + subset_first, subset_scale + subset_first, subset_count, stride);
	else
		internal::subset_prepare_rgba<SubsetTraitsVec4>(storage + subset_first, pixels, subset_masks + subset_first, subset_scale + subset_first, subset_count, stride);
#else
	RR_BREAK(); // TODO implement
#endif
}

void BC7SubsetAnalysis::compute_1subset(const U8 pixels[], SINTa subset_first, CpuDispatchFlags dispatch)
{
	using namespace internal;

	RR_ASSERT_ALWAYS(0 <= subset_first && subset_first + 8 <= stride);

#ifdef DO_BUILD_AVX2
	if ( dispatch.AVX2() )
	{
		internal::subset_prepare_1subset_avx2(storage + subset_first, pixels, stride);
		return;
	}
#endif

#ifdef DO_BUILD_ARM_DOTPROD
	if ( dispatch.ARM_DotProd() )
	{
		internal::subset_prepare_1subset_armdp(storage + subset_first, pixels, stride);
		return;
	}
#endif

#if defined(DO_BUILD_SSE4) || defined(DO_BUILD_NEON64)
	internal::subset_prepare_rgba_1subset<SubsetTraitsVec4>(storage + subset_first, pixels, stride);
#else
	RR_BREAK(); // TODO implement
#endif
}

OODLE_NS_END

