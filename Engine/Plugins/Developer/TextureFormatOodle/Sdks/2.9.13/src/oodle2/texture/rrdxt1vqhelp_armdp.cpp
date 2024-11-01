// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetarmdp

#include "rrdxtcblock.h"
#include "rrdxt1vqhelp.h"
#include "rrdxtcblock.inl"
#include "rrdxt1vqhelp.inl"

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

void batch_find_best_errors_ARM_DP(
	S32 * dest_ssd,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColorBlock4x4 & colors)
{
	const SINTa count = count32;
	Vec128_U8 pix[4];
	pix[0] = Vec128_U8::loadu(colors.colors + 0);
	pix[1] = Vec128_U8::loadu(colors.colors + 4);
	pix[2] = Vec128_U8::loadu(colors.colors + 8);
	pix[3] = Vec128_U8::loadu(colors.colors + 12);


	for (SINTa i = 0; i < count; ++i)
	{
		SINTa idx = inds[i] & 0xffff;
		Vec128_U8 pal = Vec128_U8::loadu(vqendpoints[idx].palette);

		Vec128_U32 error_sum;
		error_sum =  BC1_FindBestErrors_NEON<ARMDPKernelTraits>(pix[0], pal);
		error_sum += BC1_FindBestErrors_NEON<ARMDPKernelTraits>(pix[1], pal);
		error_sum += BC1_FindBestErrors_NEON<ARMDPKernelTraits>(pix[2], pal);
		error_sum += BC1_FindBestErrors_NEON<ARMDPKernelTraits>(pix[3], pal);

		dest_ssd[i] = reduce_add(error_sum);
	}
}

int filter_endpoint_color_bbox_ARM_DP(
	int * dest_inds,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColor32BGRA bbox[2],
	U32 bbox_min_d, bool require_3c)
{
	return filter_endpoint_color_bbox_NEON_core<ARMDPKernelTraits>(dest_inds, vqendpoints, inds, count32, bbox, bbox_min_d, require_3c);
}

}

OODLE_NS_END

#endif
