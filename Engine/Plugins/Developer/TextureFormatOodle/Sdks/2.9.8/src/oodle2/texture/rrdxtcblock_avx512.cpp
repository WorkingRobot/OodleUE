// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetavx512

#include "rrdxtcblock.h"
#include "rrdxtcblock_avx512.inl"

#ifdef DO_BUILD_AVX512

OODLE_NS_START

namespace internal {

U32 DXT1_FindIndices_AVX512(const rrColorBlock4x4 & block, const rrColor32BGRA palette[4],U32 * pError)
{
	// The basic approach and most of the tricks are explained in rrdxtcblock.cpp; I'll
	// just note the differences from that version in here.
	//
	// The only difference in this version is that instead of 128-bit vectors containing
	// the pixels from one row at a time, we work with 512-bit vectors.

	// Load the palette
	Vec512 pal_br16, pal_ga16;
	UnpackBGRAto16_Weighted_2x(pal_br16, pal_ga16, broadcast128_512(load128u(palette)));

	// Load the pixels
	Vec512 pix_br16, pix_ga16;
	UnpackBGRAto16_Weighted_2x(pix_br16, pix_ga16, _mm512_loadu_si512(block.colors));

	// Find indices
	Vec512 best = BC1_FindBestIndices_AVX512(pix_br16, pix_ga16, pal_br16, pal_ga16);
	U32 indices = BC1_ExtractIndices_AVX512(best);

	// Horizontal reduction for final error sum
	if (pError)
		*pError = _mm512_reduce_add_epi32(_mm512_srli_epi32(best, 2));

	return indices;
}

void DXT1_EvalPalettes_AVX512(const DXT1_FindErrorsContext * ctx, U32 out_ssds[], const rrColor32BGRA palettes[], int count32)
{
	SINTa count = count32;
	SINTa i = 0;

	Vec512 pix_prerot[8];
	pix_prerot[0]  = _mm512_loadu_si512(&ctx->unpacked_pixels[0]);
	pix_prerot[1]  = _mm512_loadu_si512(&ctx->unpacked_pixels[32]);
	Pixel_PreRotate(pix_prerot);

	// Main loop tries to rank 4 palettes at once since that makes for a more efficient reduction
	for (; i < (count & ~3); i += 4)
	{
		Vec512 pal_br, pal_ga;

		// Set up all 4 palettes at once; they're conveniently right next to each other
		UnpackBGRAto16_Weighted(pal_br, pal_ga, _mm512_loadu_si512(palettes + i * 4));

		// Calc errors for the 4 palettes
		Vec512 err0 = BC1_FindBestErrorsPrerot_AVX512(pix_prerot, broadcast128in512<0>(pal_br), broadcast128in512<0>(pal_ga));
		Vec512 err1 = BC1_FindBestErrorsPrerot_AVX512(pix_prerot, broadcast128in512<1>(pal_br), broadcast128in512<1>(pal_ga));
		Vec512 err2 = BC1_FindBestErrorsPrerot_AVX512(pix_prerot, broadcast128in512<2>(pal_br), broadcast128in512<2>(pal_ga));
		Vec512 err3 = BC1_FindBestErrorsPrerot_AVX512(pix_prerot, broadcast128in512<3>(pal_br), broadcast128in512<3>(pal_ga));

		// Reduce and store
		store128u(out_ssds + i, HorizontalSum4x(err0, err1, err2, err3));
	}

	// Tail loop does one at a time
	for (; i < count; ++i)
	{
		Vec512 pal_br, pal_ga;
		UnpackBGRAto16_Weighted(pal_br, pal_ga, _mm512_broadcast_i32x4(load128u(palettes + i * 4)));

		Vec512 err512 = BC1_FindBestErrors_AVX512(pix_prerot[0], pix_prerot[1], pal_br, pal_ga);
		out_ssds[i] = _mm512_reduce_add_epi32(err512);
	}
}

}

OODLE_NS_END

#endif // DO_BUILD_AVX512

