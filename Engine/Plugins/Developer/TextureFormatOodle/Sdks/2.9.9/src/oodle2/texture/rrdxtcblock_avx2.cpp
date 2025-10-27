// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetavx2

#include "rrdxtcblock.h"
#include "rrdxtcblock_avx2.inl"

#ifdef DO_BUILD_AVX2

OODLE_NS_START

namespace internal {

static RADFORCEINLINE Vec256 FindIndicesInner(const Vec256 & row, const Vec256 & pal_br16, const Vec256 & pal_ga16)
{
	Vec256 row_br16, row_ga16;

	UnpackBGRAto16_Weighted_2x(row_br16, row_ga16, row);
	return BC1_FindBestIndices_AVX2(row_br16, row_ga16, pal_br16, pal_ga16);
}

static RADFORCEINLINE Vec256 FindErrorsInner(const Vec256 & row, const Vec256 & pal_br16, const Vec256 & pal_ga16)
{
	Vec256 row_br16, row_ga16;

	UnpackBGRAto16_Weighted(row_br16, row_ga16, row);
	return BC1_FindBestErrors_AVX2(row_br16, row_ga16, pal_br16, pal_ga16);
}

U32 DXT1_FindIndices_AVX2(const rrColorBlock4x4 & block, const rrColor32BGRA palette[4],U32 * pError)
{
	// The basic approach and most of the tricks are explained in rrdxtcblock.cpp; I'll
	// just note the differences from that version in here.
	//
	// The only difference in this version is that instead of 128-bit vectors containing
	// the pixels from one row at a time, we work with 256-bit vectors with two rows
	// worth of pixels.

	// Load the palette
	Vec256 pal_br16, pal_ga16;
	UnpackBGRAto16_Weighted_2x(pal_br16, pal_ga16, broadcast128_256(load128u(palette)));

	// Process pairs of rows
	const Vec256 best01 = FindIndicesInner(load256u(block.colors + 0), pal_br16, pal_ga16);
	const Vec256 best23 = FindIndicesInner(load256u(block.colors + 8), pal_br16, pal_ga16);

	// Extract index bits
	U32 indices = BC1_ExtractIndices_AVX2(best01, best23);

	// Horizontal reduction for final error sum
	if (pError)
	{
		Vec256 error_sum256 = _mm256_add_epi32(_mm256_srli_epi32(best01, 2), _mm256_srli_epi32(best23, 2));
		Vec128 error_sum = _mm_add_epi32(lo_half(error_sum256), hi_half(error_sum256));

		*pError = reduce_add_s32(error_sum);
	}

	return indices;
}

U32 DXT1_EvalPalette_AVX2(const DXT1_FindErrorsContext * ctx, const rrColor32BGRA palette[4])
{
	const Vec256 * pix_br = (const Vec256 *)&ctx->unpacked_pixels[0];
	const Vec256 * pix_ga = (const Vec256 *)&ctx->unpacked_pixels[32];

	// Load and unpack the palette
	Vec256 pal_br16, pal_ga16;
	UnpackBGRAto16_Weighted(pal_br16, pal_ga16, broadcast128_256(load128u(palette)));

	// Sum the errors
	Vec256 error_sum256;
	error_sum256 = BC1_FindBestErrors_AVX2(pix_br[0], pix_ga[0], pal_br16, pal_ga16);
	error_sum256 = _mm256_add_epi32(error_sum256, BC1_FindBestErrors_AVX2(pix_br[1], pix_ga[1], pal_br16, pal_ga16));

	// Finish horizontal reduction
	Vec128 error_sum = _mm_add_epi32(lo_half(error_sum256), hi_half(error_sum256));
	return reduce_add_s32(error_sum);
}

void DXT1_EvalPalettes_AVX2(const DXT1_FindErrorsContext * ctx, U32 out_ssds[], const rrColor32BGRA palettes[], int count)
{
	Vec256 pix_br[2], pix_ga[2];
	pix_br[0] = load256u(ctx->unpacked_pixels +  0);
	pix_br[1] = load256u(ctx->unpacked_pixels + 16);
	pix_ga[0] = load256u(ctx->unpacked_pixels + 32);
	pix_ga[1] = load256u(ctx->unpacked_pixels + 48);

	for (int i = 0; i < count; ++i)
	{
		// Load and unpack the palette
		Vec256 pal_br16, pal_ga16;
		UnpackBGRAto16_Weighted(pal_br16, pal_ga16, broadcast128_256(load128u(palettes + i * 4)));

		// Sum the errors
		Vec256 error_sum256;
		error_sum256 = BC1_FindBestErrors_AVX2(pix_br[0], pix_ga[0], pal_br16, pal_ga16);
		error_sum256 = _mm256_add_epi32(error_sum256, BC1_FindBestErrors_AVX2(pix_br[1], pix_ga[1], pal_br16, pal_ga16));

		// Finish horizontal reduction
		Vec128 error_sum = _mm_add_epi32(lo_half(error_sum256), hi_half(error_sum256));
		out_ssds[i] = reduce_add_u32(error_sum);
	}
}

}

OODLE_NS_END

#endif
