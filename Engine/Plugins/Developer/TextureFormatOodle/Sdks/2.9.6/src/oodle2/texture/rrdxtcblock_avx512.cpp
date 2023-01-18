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

U32 DXT1_FindBestPalette_AVX512(const DXT1_FindErrorsContext * ctx, const rrColor32BGRA palettes[], int count32)
{
	SINTa count = count32;

	Vec512 pix_prerot[8];
	pix_prerot[0]  = _mm512_loadu_si512(&ctx->unpacked_pixels[0]);
	pix_prerot[1]  = _mm512_loadu_si512(&ctx->unpacked_pixels[32]);
	Pixel_PreRotate(pix_prerot);

	Vec128 max_err = _mm_set1_epi32(-1);
	Vec128 best_overall_vec = max_err;
	Vec128 index_vec = _mm_setr_epi32(0, 1, 2, 3);
	Vec512 pal_index_vec = _mm512_setr_epi32(0,0,0,0, 1,1,1,1, 2,2,2,2, 3,3,3,3);
	Vec512 count32_vec = _mm512_set1_epi32(count32);

	for (SINTa i = 0; i < count; i += 4)
	{
		// Set up 4 palettes at once
		Vec512 pal_br, pal_ga;

		__mmask16 m_active = _mm512_cmplt_epu32_mask(pal_index_vec, count32_vec);
		UnpackBGRAto16_Weighted(pal_br, pal_ga, _mm512_maskz_loadu_epi32(m_active, palettes + i*4));

		// Calc errors for the 4 palettes
		Vec512 err0 = BC1_FindBestErrorsPrerot_AVX512(pix_prerot, broadcast128in512<0>(pal_br), broadcast128in512<0>(pal_ga));
		Vec512 err1 = BC1_FindBestErrorsPrerot_AVX512(pix_prerot, broadcast128in512<1>(pal_br), broadcast128in512<1>(pal_ga));
		Vec512 err2 = BC1_FindBestErrorsPrerot_AVX512(pix_prerot, broadcast128in512<2>(pal_br), broadcast128in512<2>(pal_ga));
		Vec512 err3 = BC1_FindBestErrorsPrerot_AVX512(pix_prerot, broadcast128in512<3>(pal_br), broadcast128in512<3>(pal_ga));

		// Horizontal reduction
		Vec128 err = HorizontalSum4x(err0, err1, err2, err3);

		// Make inactive lanes have the max error so they don't get chosen.
		// Original mask was for 128-bit palettes occupying 4 32-bit lanes; we can
		// interpret the same mask as being for 32-bit values occupying 4 8-bit lanes.
		err = _mm_mask_blend_epi8(m_active, max_err, err);

		// Put in the index to keep track of overall best
		Vec128 score = _mm_or_si128(_mm_slli_epi32(err, DXT1_FindErrorsContext::COUNT_SHIFT), index_vec);
		best_overall_vec = _mm_min_epu32(best_overall_vec, score);
		index_vec = _mm_add_epi32(index_vec, _mm_set1_epi32(4));
		pal_index_vec = _mm512_add_epi32(pal_index_vec, _mm512_set1_epi32(4));
	}

	// Finish the min reduction
	best_overall_vec = _mm_min_epu32(best_overall_vec, shuffle32<2,3,0,1>(best_overall_vec));
	best_overall_vec = _mm_min_epu32(best_overall_vec, shuffle32<1,0,3,2>(best_overall_vec));

	return _mm_cvtsi128_si32(best_overall_vec);
}

}

OODLE_NS_END

#endif // DO_BUILD_AVX512

