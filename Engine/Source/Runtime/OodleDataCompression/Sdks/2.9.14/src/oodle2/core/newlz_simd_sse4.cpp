// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetsse4

#include "newlz_simd.h"
#include "cbradutil.h"

OODLE_NS_START

//==================================================================================================
#ifdef DO_BUILD_SSE4

void simd_mul_s32_sub_u8_sse4(S32 * offsets, SINTa offsets_count, S32 multiplier, const U8 * subtrahend )
{
	SINTa offi = 0;

	RR_ASSERT( rrIsAlignedPointer(offsets,16) );
	
	__m128i v_offset_alt_modulo = _mm_set1_epi32(multiplier);
	
	for(; offi+16 <= offsets_count ;offi += 16)
	{
		__m128i offsets1 = _mm_load_si128((__m128i *)(offsets+offi));
		__m128i offsets2 = _mm_load_si128((__m128i *)(offsets+offi+4));
		__m128i offsets3 = _mm_load_si128((__m128i *)(offsets+offi+8));
		__m128i offsets4 = _mm_load_si128((__m128i *)(offsets+offi+12));
		
		// need 4x 32*32 mul (in sse4, not sse2)
		//	I'm only getting 2x mul's in sse2, so not a big win here
		offsets1 = _mm_mullo_epi32(offsets1,v_offset_alt_modulo);
		offsets2 = _mm_mullo_epi32(offsets2,v_offset_alt_modulo);
		offsets3 = _mm_mullo_epi32(offsets3,v_offset_alt_modulo);
		offsets4 = _mm_mullo_epi32(offsets4,v_offset_alt_modulo);
		
		// unpack 8 -> 32 :
		// this is a single PMOVZXBD with a memory reference each
		const U8 * sub = subtrahend + offi;
		__m128i low1 = _mm_cvtepu8_epi32(_mm_cvtsi32_si128(*(const int*) (sub + 0)));
		__m128i low2 = _mm_cvtepu8_epi32(_mm_cvtsi32_si128(*(const int*) (sub + 4)));
		__m128i low3 = _mm_cvtepu8_epi32(_mm_cvtsi32_si128(*(const int*) (sub + 8)));
		__m128i low4 = _mm_cvtepu8_epi32(_mm_cvtsi32_si128(*(const int*) (sub + 12)));
		
		_mm_store_si128((__m128i *)(offsets+offi   ), _mm_sub_epi32(offsets1,low1) );
		_mm_store_si128((__m128i *)(offsets+offi+4 ), _mm_sub_epi32(offsets2,low2) );
		_mm_store_si128((__m128i *)(offsets+offi+8 ), _mm_sub_epi32(offsets3,low3) );
		_mm_store_si128((__m128i *)(offsets+offi+12), _mm_sub_epi32(offsets4,low4) );
	}
	
	for (;offi<offsets_count;offi++)
	{
		offsets[offi] = (offsets[offi] * multiplier) - (S32)subtrahend[offi];
	}
}


U32 simd_find_max_U32_256_sse4(const U32 * histogram)
{
	__m128i vmax = _mm_setzero_si128();
	
	for LOOP(i,(256/4))
	{
		__m128i vals = _mm_loadu_si128((const __m128i *)(histogram + i*4));
		vmax = _mm_max_epu32(vmax,vals);
	}
	
	// horizontal max :
	
	vmax = _mm_max_epu32(vmax, _mm_shuffle_epi32(vmax, _MM_SHUFFLE(1,0,3,2)));
	///vmax = { max[3,1] , max[2,0], max[1,3], max[2,0] };
	vmax = _mm_max_epu32(vmax, _mm_shuffle_epi32(vmax, _MM_SHUFFLE(2,3,0,1)));
	U32 onemax = _mm_extract_epi32(vmax,0);		
	
	//RR_ASSERT( onemax == highest_histo_count );
			
	return onemax;
}

#endif // DO_BUILD_SSE4

OODLE_NS_END
