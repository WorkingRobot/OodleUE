
// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetavx2

// We _MUST NOT_ allow the compiler to contract mul-add operations to FMAs;
// this changes the results between AVX2 and SSE4 versions which we don't allow!
#include "nocontract.h"

#include "rrfixedfloat.h"
#include "vec256.inl"
#include <string.h>

#ifdef DO_BUILD_AVX2

OODLE_NS_START

namespace internal {

// Actually just F16C, but we treat it as AVX2 since we don't have
// dedicated AVX-without-AVX2 paths.

void rrFP16_to_FP32_Array_AVX2(F32 * dst, const rrFP16 * src, SINTa count)
{
	SINTa i;

	// Bulk loop
	for (i = 0; i <= count - 8; i += 8)
	{
		Vec128 halfs = load128u(src + i);
		Vec256_F32{_mm256_cvtph_ps(halfs)}.storeu(dst + i);
	}

	// Tail
	if (SINTa rest = count - i)
	{
		rrFP16 src_buf[8] = {};
		F32 dst_buf[8] = {};

		memcpy(src_buf, src + i, rest * sizeof(rrFP16));
		Vec128 halfs = load128u(src_buf);
		Vec256_F32{_mm256_cvtph_ps(halfs)}.storeu(dst_buf);
		memcpy(dst + i, dst_buf, rest * sizeof(F32));
	}
}

void rrFP32_to_FP16_Array_AVX2(rrFP16 * dst, const F32 * src, SINTa count)
{
	SINTa i;

	// Bulk loop
	for (i = 0; i <= count - 8; i += 8)
	{
		Vec256_F32 floats = Vec256_F32::loadu(src + i);
		store128u(dst + i, _mm256_cvtps_ph(floats, _MM_FROUND_TO_NEAREST_INT));
	}

	// Tail
	if (SINTa rest = count - i)
	{
		F32 src_buf[8] = {};
		rrFP16 dst_buf[8] = {};

		memcpy(src_buf, src + i, rest * sizeof(F32));
		Vec256_F32 floats = Vec256_F32::loadu(src_buf);
		store128u(dst_buf, _mm256_cvtps_ph(floats, _MM_FROUND_TO_NEAREST_INT));
		memcpy(dst + i, dst_buf, rest * sizeof(rrFP16));
	}
}

} // internal namespace

OODLE_NS_END

#endif // DO_BUILD_AVX2

