// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetarmdp

#include "rrdxtcblock.h"
#include "rrdxtcblock.inl"

#ifdef DO_BUILD_ARM_DOTPROD

OODLE_NS_START

namespace internal {

struct ARMDPKernelTraits
{
	static Vec128_U32 sumsqr4_u8(Vec128_U8 x)
	{
		return vdotq_u32(Vec128_U32(0), x, x);
	}
};

U32 DXT1_EvalPalette_ARM_DP(const DXT1_FindErrorsContext * ctx, const rrColor32BGRA palette[4])
{
	const Vec128_U8 * pix = (const Vec128_U8 *)&ctx->packed_pixels;

	// Load the palette
	Vec128_U8 pal = Vec128_U8::loadu(palette);

	// Sum the errors
	Vec128_U32 error_sum;
	error_sum =  BC1_FindBestErrors_NEON<ARMDPKernelTraits>(pix[0], pal);
	error_sum += BC1_FindBestErrors_NEON<ARMDPKernelTraits>(pix[1], pal);
	error_sum += BC1_FindBestErrors_NEON<ARMDPKernelTraits>(pix[2], pal);
	error_sum += BC1_FindBestErrors_NEON<ARMDPKernelTraits>(pix[3], pal);
	return reduce_add(error_sum);
}

void DXT1_EvalPalettes_ARM_DP(const DXT1_FindErrorsContext * ctx, U32 out_ssds[], const rrColor32BGRA palettes[], int count)
{
	for (int i = 0; i < count; ++i)
		out_ssds[i] = DXT1_EvalPalette_ARM_DP(ctx, &palettes[i*4]);
}

}

OODLE_NS_END

#endif
