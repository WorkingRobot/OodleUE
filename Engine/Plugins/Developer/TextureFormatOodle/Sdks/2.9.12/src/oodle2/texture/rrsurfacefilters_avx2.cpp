// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetavx2

// We _MUST NOT_ allow the compiler to contract mul-add operations to FMAs;
// this changes the results between AVX2 and SSE4 versions which we don't allow!
#include "nocontract.h"

#include "rrsurfacefilters.h"
#include "vec256.inl"

#ifdef DO_BUILD_AVX2

OODLE_NS_START

namespace internal {

// returns count actually processed
SINTa rrSurface_MakeGaussianBlurred_Sub_Line_Interior_AVX2(float *to_ptr,const float * fm_ptr,
	SINTa stride, // stride is in floats
	SINTa count,
	const F32 * filter,SINTa filter_width)
{
	SINTa floati = 0;

	// Copy to local var since it doesn't alias
	float lfilter[MAX_FILTER_TAPS];
	for(SINTa i=0;i<filter_width;i++)
		lfilter[i] = filter[i];

	Vec256_F32 coeff0 { filter[0] };
	for(; floati < (count & ~15); floati += 16)
	{
		Vec256_F32 accum0 = coeff0 * Vec256_F32::loadu(fm_ptr);
		Vec256_F32 accum1 = coeff0 * Vec256_F32::loadu(fm_ptr+8);
		for (SINTa i=1;i<filter_width;i++) // no i=0
		{
			Vec256_F32 coeff { lfilter[i] };
			accum0 += coeff * (Vec256_F32::loadu(fm_ptr + i*stride) + Vec256_F32::loadu(fm_ptr - i*stride));
			accum1 += coeff * (Vec256_F32::loadu(fm_ptr + i*stride + 8) + Vec256_F32::loadu(fm_ptr - i*stride + 8));
		}
		
		accum0.storeu(to_ptr);
		accum1.storeu(to_ptr+8);
		to_ptr += 16;
		fm_ptr += 16;
	}

	return floati;
}

// count is in _floats_ not pixels
// returns count actually processed
SINTa rrSurface_MakeGaussianBlurred_Sub_Strip_V_AVX2(
	float * to_row,
	F32 * * window_rows,
	SINTa count,
	const F32 * filter,SINTa filter_width)
{
	SINTa floati = 0;

	// Copy to local var since it doesn't alias
	float lfilter[MAX_FILTER_TAPS];
	for(SINTa i=0;i<filter_width;i++)
		lfilter[i] = filter[i];

	Vec256_F32 coeff0 { lfilter[0] };

	for (; floati < (count & ~15); floati += 16)
	{
		const F32 * row0 = window_rows[0] + floati;
		Vec256_F32 accum0 = coeff0 * Vec256_F32::loadu(row0);
		Vec256_F32 accum1 = coeff0 * Vec256_F32::loadu(row0+8);
		for (SINTa i=1;i<filter_width;i++) // no i=0
		{
			Vec256_F32 coeff { lfilter[i] };
			const F32 * rowpi = window_rows[i] + floati;
			const F32 * rowni = window_rows[-i] + floati;
			accum0 += coeff * (Vec256_F32::loadu(rowpi  ) + Vec256_F32::loadu(rowni  ));
			accum1 += coeff * (Vec256_F32::loadu(rowpi+8) + Vec256_F32::loadu(rowni+8));
		}

		accum0.storeu(to_row+floati);
		accum1.storeu(to_row+floati+8);
	}

	return floati;
}

} // internal namespace 

OODLE_NS_END

#endif // DO_BUILD_AVX2

