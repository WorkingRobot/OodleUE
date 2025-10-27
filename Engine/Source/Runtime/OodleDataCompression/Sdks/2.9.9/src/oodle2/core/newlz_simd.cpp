// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "newlz_simd.h"
#include "cbradutil.h"

OODLE_NS_START

// Some generic NEON utilities first

#ifdef __RADNEON__

static RADFORCEINLINE int neon_reduce_add_s16_to_gpr(int16x8_t vec)
{
	// AArch64 has an instruction for this; 32b NEON needs a reduction tree.
#if defined(__aarch64__)
	return vaddvq_s16(vec);
#else
	int16x4_t sum1 = vadd_s16(vget_low_s16(vec), vget_high_s16(vec)); // sum with 4 away
	int16x4_t sum2 = vpadd_s16(sum1, sum1); // sum with 2 away
	int16x4_t sum3 = vpadd_s16(sum2, sum2); // sum with 1 away
	return vget_lane_s16(sum3, 0);
#endif
}

// Result of reduction is in first int32 lane of the result
// contents of the upper lanes are unspecified
static RADFORCEINLINE int32x2_t neon_reduce_min_s32(int32x4_t vec)
{
	// AArch64 has an instruction for this; 32b NEON needs a reduction tree.
#if defined(__aarch64__)
	return vdup_n_s32(vminvq_s32(vec));
#else // __clang__
	int32x2_t min1 = vmin_s32(vget_low_s32(vec), vget_high_s32(vec)); // min with 2 away
	int32x2_t min2 = vpmin_s32(min1, min1); // min with 1 away
	return min2;
#endif
}

// vec is supposed to be a compare result
// i.e. all lanes are either all-0 or all-1 bits
static RADFORCEINLINE U32 neon_movemask_s16(int16x8_t vec)
{
	static const RAD_ALIGN(S16, const_masks[8], 16) = { 1<<0, 1<<1, 1<<2, 1<<3, 1<<4, 1<<5, 1<<6, 1<<7 };
	int16x8_t masked = vandq_s16(vec, vld1q_s16(const_masks));
	return neon_reduce_add_s16_to_gpr(masked);
}

static RADFORCEINLINE U32 neon_movemask_u16(uint16x8_t vec)
{
	return neon_movemask_s16( vreinterpretq_s16_u16(vec) );
}

#endif

// ----

S32 simd_horizontal_sum_s32(const S32 * const v,int num)
{
	#if defined(__RADSSE2__)
	
	// S32 sum_check = 0; for LOOP(i,num) sum_check += v[i];
	
	const S32 * pv = v;
	const S32 * pv_end = v + num;
	
	__m128i accum = _mm_setzero_si128();
	
	while( rrPtrDiff( pv_end - pv ) >= 16 )
	{
		__m128i v1 = _mm_loadu_si128((const __m128i *)pv); pv += 4;
		__m128i v2 = _mm_loadu_si128((const __m128i *)pv); pv += 4;
		__m128i v3 = _mm_loadu_si128((const __m128i *)pv); pv += 4;
		__m128i v4 = _mm_loadu_si128((const __m128i *)pv); pv += 4;
		
		accum = _mm_add_epi32(accum, _mm_add_epi32( _mm_add_epi32(v1,v2) , _mm_add_epi32(v3,v4) ) );
	}
	
	while( rrPtrDiff( pv_end - pv ) >= 4 )
	{
		accum = _mm_add_epi32(accum, _mm_loadu_si128((const __m128i *)pv)); pv += 4;
	}
		
	S32 sum = hsum_epi32_sse2(accum);
	
	while( pv < pv_end )
	{
		sum += *pv++;
	}
	
	//RR_ASSERT( sum == sum_check );
	
	return sum;

	#elif defined(__RADNEON__)

	//S32 sum_check = 0; for LOOP(i,num) sum_check += v[i];

	const S32 * pv = v;
	const S32 * pv_end = v + num;

	int32x4_t accum1 = vdupq_n_s32(0);
	int32x4_t accum2 = vdupq_n_s32(0);

	while( rrPtrDiff( pv_end - pv ) >= 16 )
	{
		int32x4_t v1 = vld1q_s32(pv +  0);
		int32x4_t v2 = vld1q_s32(pv +  4);
		int32x4_t v3 = vld1q_s32(pv +  8);
		int32x4_t v4 = vld1q_s32(pv + 12);
		pv += 16;

		accum1 = vaddq_s32(accum1, vaddq_s32(v1, v2));
		accum2 = vaddq_s32(accum2, vaddq_s32(v3, v4));
	}

	while( rrPtrDiff( pv_end - pv ) >= 4 )
	{
		accum1 = vaddq_s32(accum1, vld1q_s32(pv));
		pv += 4;
	}

	// finalize reduction
	accum1 = vaddq_s32(accum1, accum2);
	S32 sum = hsum_s32_neon(accum1);

	while( pv < pv_end )
	{
		sum += *pv++;
	}

	//RR_ASSERT( sum == sum_check );

	return sum;

	#else
	
	S32 sum = 0;
	
	for LOOP(i,num)
	{
		sum += v[i];
	}
	
	return sum;
	
	#endif
}

void simd_add_u32_256(U32 * to, const U32 * fm1, const U32 * fm2)
{
	#if defined(__RADSSE2__)
	
	__m128i * vto = (__m128i *) to;
	const __m128i * vfm1 = (const __m128i *) fm1;
	const __m128i * vfm2 = (const __m128i *) fm2;
	
	for LOOP(i,16)
	{
		RR_UNROLL_4( \
			_mm_storeu_si128(vto, _mm_add_epi32( _mm_loadu_si128(vfm1), _mm_loadu_si128(vfm2) ) ); \
			vto++; vfm1++; vfm2++; \
		);
	}

	#elif defined(__RADNEON__)

	for LOOP(i,16)
	{
		RR_UNROLL_4( \
			vst1q_u32(to, vaddq_u32(vld1q_u32(fm1), vld1q_u32(fm2))); \
			to += 4; fm1 += 4; fm2 += 4; \
		);
	}
	
	#else
	
	for LOOP(i,256)
	{
		to[i] = fm1[i] + fm2[i];
	}
	
	#endif
}

void simd_sub_u32_256(U32 * to, const U32 * fm1, const U32 * fm2)
{
	#if defined(__RADSSE2__)
	
	__m128i * vto = (__m128i *) to;
	const __m128i * vfm1 = (const __m128i *) fm1;
	const __m128i * vfm2 = (const __m128i *) fm2;
	
	for LOOP(i,16)
	{
		RR_UNROLL_4( \
			_mm_storeu_si128(vto, _mm_sub_epi32( _mm_loadu_si128(vfm1), _mm_loadu_si128(vfm2) ) ); \
			vto++; vfm1++; vfm2++; \
		);
	}

	#elif defined(__RADNEON__)

	for LOOP(i,16)
	{
		RR_UNROLL_4( \
			vst1q_u32(to, vsubq_u32(vld1q_u32(fm1), vld1q_u32(fm2))); \
			to += 4; fm1 += 4; fm2 += 4; \
		);
	}
	
	#else
	
	for LOOP(i,256)
	{
		to[i] = fm1[i] - fm2[i];
	}
	
	#endif
}

/*

simd_dotproduct_s16_s16_256 is not currently used

but it's the archetype of how simple this is if we fit in S16's
the other variants are used (mainly simd_dotproduct_s32_s16_256)
the difference is mostly packing

*/
S32 simd_dotproduct_s16_s16_256(const S16 * v1, const S16 * v2)
{
	#ifdef __RADSSE2__
	
	__m128i accum = _mm_setzero_si128();
	
	for(int s=0;s<256;s+=8)
	{
		RR_COMPILER_ASSERT( sizeof(v1[0]) == 2 );
		__m128i counts_s16 = _mm_loadu_si128((__m128i *)(v1+s));
		
		RR_COMPILER_ASSERT( sizeof(v2[0]) == 2 );
		__m128i codelens_u16 = _mm_loadu_si128((__m128i *)(v2+s));
		
		// _mm_madd_epi16 is amazing for this
		// Multiplies the 8 signed 16-bit integers from a by the 8 signed 16-bit integers from b.
		// Adds the signed 32-bit integer results pairwise and packs the 4 signed 32-bit integer results.
		__m128i product = _mm_madd_epi16(counts_s16,codelens_u16);
		
		accum = _mm_add_epi32( accum, product );
	}
	
	S32 sum_SSE = hsum_epi32_sse2(accum);
	
	return sum_SSE;
	
	#else
	
	S32 sum = 0;
	
	for LOOP(s,256)
	{
		int n = v1[s];
		int cl = v2[s];
		
		sum += n * cl;
	}
	
	return sum;	
	
	#endif
}
		
S32 simd_dotproduct_s32_s16_256(const S32 * v1, bool v1_fits_in_S16, const S16 * v2)
{
	#if defined(__RADSSE2__)
	
	__m128i accum = _mm_setzero_si128();
	
	if ( v1_fits_in_S16 )
	{
		// v1 fit in S16
		
		for(int s=0;s<256;s+=8)
		{
			RR_COMPILER_ASSERT( sizeof(v1[0]) == 4 );
			__m128i counts1 = _mm_loadu_si128((__m128i *)(v1+s));
			__m128i counts2 = _mm_loadu_si128((__m128i *)(v1+s+4));
			
			RR_COMPILER_ASSERT( sizeof(v2[0]) == 2 );
			__m128i codelens_u16 = _mm_loadu_si128((__m128i *)(v2+s));
			
			// pack 32->16 *signed* so must fit in S16
			__m128i counts_s16 = _mm_packs_epi32(counts1,counts2);
			
			// _mm_madd_epi16 is amazing for this
			// Multiplies the 8 signed 16-bit integers from a by the 8 signed 16-bit integers from b.
			// Adds the signed 32-bit integer results pairwise and packs the 4 signed 32-bit integer results.
			__m128i product = _mm_madd_epi16(counts_s16,codelens_u16);
			
			accum = _mm_add_epi32( accum, product );
		}
	}
	else
	{
	
		for(int s=0;s<256;s+=8)
		{
			RR_COMPILER_ASSERT( sizeof(v1[0]) == 4 );
			__m128i counts1 = _mm_loadu_si128((__m128i *)(v1+s));
			__m128i counts2 = _mm_loadu_si128((__m128i *)(v1+s+4));
			
			RR_COMPILER_ASSERT( sizeof(v2[0]) == 2 );
			__m128i codelens_u16 = _mm_loadu_si128((__m128i *)(v2+s));
			
			__m128i codelens1 = _mm_unpacklo_epi16(codelens_u16, _mm_setzero_si128());
			__m128i codelens2 = _mm_unpackhi_epi16(codelens_u16, _mm_setzero_si128());	
			
			/*
			// SSE4 :
			__m128i product1 = _mm_mullo_epi32(counts1,codelens1);
			__m128i product2 = _mm_mullo_epi32(counts2,codelens2);
			/*/
			__m128i product1 = _mm_mullo_epi32_sse2(counts1,codelens1);
			__m128i product2 = _mm_mullo_epi32_sse2(counts2,codelens2);		
			/**/
			accum = _mm_add_epi32( accum, _mm_add_epi32(product1,product2) );
		}
	}
	
	// final horizontal add of accum :
	S32 sum_SSE = hsum_epi32_sse2(accum);
	
	#if 0
	S32 sum = 0;
	
	for LOOP(s,256)
	{
		int n = v1[s];
		int cl = v2[s];
		
		sum += n * cl;
	}
	
	// check vs scalar :
	RR_ASSERT_ALWAYS( sum == sum_SSE );
	#endif
	
	return sum_SSE;	

	#elif defined(__RADNEON__)

	int32x4_t accum0 = vdupq_n_s32(0);
	int32x4_t accum1 = vdupq_n_s32(0);
	int32x4_t accum2 = vdupq_n_s32(0);
	int32x4_t accum3 = vdupq_n_s32(0);

	if ( v1_fits_in_S16 )
	{
		for(int s=0;s<256;s+=16)
		{
			int32x4_t counts0 = vld1q_s32(v1 + s +  0);
			int32x4_t counts1 = vld1q_s32(v1 + s +  4);
			int32x4_t counts2 = vld1q_s32(v1 + s +  8);
			int32x4_t counts3 = vld1q_s32(v1 + s + 12);
			int16x8_t codelens01_s16 = vld1q_s16(v2 + s + 0);
			int16x8_t codelens23_s16 = vld1q_s16(v2 + s + 8);

			// Multiply-accumulate long with the narrowed counts
			// (long multiply is S16*S16 with S32 result)
			accum0 = vmlal_s16(accum0, vget_low_s16 (codelens01_s16), vmovn_s32(counts0));
			accum1 = vmlal_s16(accum1, vget_high_s16(codelens01_s16), vmovn_s32(counts1));
			accum2 = vmlal_s16(accum2, vget_low_s16 (codelens23_s16), vmovn_s32(counts2));
			accum3 = vmlal_s16(accum3, vget_high_s16(codelens23_s16), vmovn_s32(counts3));
		}
	}
	else
	{
		for(int s=0;s<256;s+=16)
		{
			int32x4_t counts0 = vld1q_s32(v1 + s +  0);
			int32x4_t counts1 = vld1q_s32(v1 + s +  4);
			int32x4_t counts2 = vld1q_s32(v1 + s +  8);
			int32x4_t counts3 = vld1q_s32(v1 + s + 12);
			int16x8_t codelens01_s16 = vld1q_s16(v2 + s + 0);
			int16x8_t codelens23_s16 = vld1q_s16(v2 + s + 8);

			// Multiply-accumulate with the widened codelens
			accum0 = vmlaq_s32(accum0, counts0, vmovl_s16(vget_low_s16 (codelens01_s16)));
			accum1 = vmlaq_s32(accum1, counts1, vmovl_s16(vget_high_s16(codelens01_s16)));
			accum2 = vmlaq_s32(accum2, counts2, vmovl_s16(vget_low_s16 (codelens23_s16)));
			accum3 = vmlaq_s32(accum3, counts3, vmovl_s16(vget_high_s16(codelens23_s16)));
		}
	}

	// sum the partial products, then horizontal sum
	accum0 = vaddq_s32(accum0, accum2);
	accum1 = vaddq_s32(accum1, accum3);

	accum0 = vaddq_s32(accum0, accum1);
	return hsum_s32_neon(accum0);
	
	#else
	
	S32 sum = 0;
	
	for LOOP(s,256)
	{
		int n = v1[s];
		int cl = v2[s];
		
		sum += n * cl;
	}
	
	return sum;	
	
	#endif
}

S32 simd_dotproduct_s32_s8_256(const S32 * v1, bool v1_fits_in_S16, const S8 * v2,int num)
{
	RR_ASSERT( num <= 256 );
	RR_ASSERT( num == 256 || v2[num] == 0 );

	#if defined(__RADSSE2__)
	
	__m128i accum = _mm_setzero_si128();
	
	if ( v1_fits_in_S16 )
	{
		// v1 fit in S16
		
		for(int s=0;s<num;s+=16)
		{
			RR_COMPILER_ASSERT( sizeof(v1[0]) == 4 );
			__m128i counts1 = _mm_loadu_si128((__m128i *)(v1+s));
			__m128i counts2 = _mm_loadu_si128((__m128i *)(v1+s+4));
			__m128i counts3 = _mm_loadu_si128((__m128i *)(v1+s+8));
			__m128i counts4 = _mm_loadu_si128((__m128i *)(v1+s+12));
			
			// pack 32->16 *signed* so must fit in S16
			__m128i counts1_s16 = _mm_packs_epi32(counts1,counts2);
			__m128i counts2_s16 = _mm_packs_epi32(counts3,counts4);
						
			RR_COMPILER_ASSERT( sizeof(v2[0]) == 1 );
			__m128i codelens_s8 = _mm_loadu_si128((__m128i *)(v2+s));
			
			__m128i codelens1_s16 = _mm_unpacklo_epi8(codelens_s8,_mm_setzero_si128());
			__m128i codelens2_s16 = _mm_unpackhi_epi8(codelens_s8,_mm_setzero_si128());
			
			// _mm_madd_epi16 is amazing for this
			// Multiplies the 8 signed 16-bit integers from a by the 8 signed 16-bit integers from b.
			// Adds the signed 32-bit integer results pairwise and packs the 4 signed 32-bit integer results.
			accum = _mm_add_epi32( accum, _mm_madd_epi16(counts1_s16,codelens1_s16) );
			accum = _mm_add_epi32( accum, _mm_madd_epi16(counts2_s16,codelens2_s16) );
		}
	}
	else
	{
	
		for(int s=0;s<num;s+=16)
		{
			RR_COMPILER_ASSERT( sizeof(v1[0]) == 4 );
			__m128i counts1 = _mm_loadu_si128((__m128i *)(v1+s));
			__m128i counts2 = _mm_loadu_si128((__m128i *)(v1+s+4));
			__m128i counts3 = _mm_loadu_si128((__m128i *)(v1+s+8));
			__m128i counts4 = _mm_loadu_si128((__m128i *)(v1+s+12));
			
			RR_COMPILER_ASSERT( sizeof(v2[0]) == 1 );
			__m128i codelens_s8 = _mm_loadu_si128((__m128i *)(v2+s));
			
			__m128i codelens1_s16 = _mm_unpacklo_epi8(codelens_s8,_mm_setzero_si128());
			__m128i codelens2_s16 = _mm_unpackhi_epi8(codelens_s8,_mm_setzero_si128());
			
			__m128i codelens1 = _mm_unpacklo_epi16(codelens1_s16, _mm_setzero_si128());
			__m128i codelens2 = _mm_unpackhi_epi16(codelens1_s16, _mm_setzero_si128());	
			__m128i codelens3 = _mm_unpacklo_epi16(codelens2_s16, _mm_setzero_si128());
			__m128i codelens4 = _mm_unpackhi_epi16(codelens2_s16, _mm_setzero_si128());	
			
			/*
			// SSE4 :
			accum = _mm_add_epi32( accum, _mm_mullo_epi32(counts1,codelens1) );
			accum = _mm_add_epi32( accum, _mm_mullo_epi32(counts2,codelens2) );
			accum = _mm_add_epi32( accum, _mm_mullo_epi32(counts3,codelens3) );
			accum = _mm_add_epi32( accum, _mm_mullo_epi32(counts4,codelens4) );
			/*/
			accum = _mm_add_epi32( accum, _mm_mullo_epi32_sse2(counts1,codelens1) );
			accum = _mm_add_epi32( accum, _mm_mullo_epi32_sse2(counts2,codelens2) );
			accum = _mm_add_epi32( accum, _mm_mullo_epi32_sse2(counts3,codelens3) );
			accum = _mm_add_epi32( accum, _mm_mullo_epi32_sse2(counts4,codelens4) );	
			/**/
		}
	}
	
	// final horizontal add of accum :
	S32 sum_SSE = hsum_epi32_sse2(accum);
	
	#if 0
	S32 sum = 0;
	
	for LOOP(s,num)
	{
		int n = v1[s];
		int cl = v2[s];
		
		sum += n * cl;
	}
	
	// check vs scalar :
	RR_ASSERT_ALWAYS( sum == sum_SSE );
	#endif
	
	return sum_SSE;	

	#elif defined(__RADNEON__)

	int32x4_t accum1 = vdupq_n_s32(0);
	int32x4_t accum2 = vdupq_n_s32(0);
	int32x4_t accum3 = vdupq_n_s32(0);
	int32x4_t accum4 = vdupq_n_s32(0);

	if ( v1_fits_in_S16 )
	{
		// v1 fit in S16

		for(int s=0;s<num;s+=16)
		{
			RR_COMPILER_ASSERT( sizeof(v1[0]) == 4 );
			int32x4_t counts1 = vld1q_s32(v1 + s +  0);
			int32x4_t counts2 = vld1q_s32(v1 + s +  4);
			int32x4_t counts3 = vld1q_s32(v1 + s +  8);
			int32x4_t counts4 = vld1q_s32(v1 + s + 12);

			RR_COMPILER_ASSERT( sizeof(v2[0]) == 1 );
			int8x16_t codelens_s8 = vld1q_s8(v2 + s);

			int16x8_t codelens1_s16 = vmovl_s8(vget_low_s8(codelens_s8));
			int16x8_t codelens2_s16 = vmovl_s8(vget_high_s8(codelens_s8));

			// and accumulate
			// we pack counts 32->16 *signed* so must fit in S16
			accum1 = vmlal_s16(accum1, vmovn_s32(counts1), vget_low_s16(codelens1_s16));
			accum2 = vmlal_s16(accum2, vmovn_s32(counts2), vget_high_s16(codelens1_s16));
			accum3 = vmlal_s16(accum3, vmovn_s32(counts3), vget_low_s16(codelens2_s16));
			accum4 = vmlal_s16(accum4, vmovn_s32(counts4), vget_high_s16(codelens2_s16));
		}
	}
	else
	{

		for(int s=0;s<num;s+=16)
		{
			RR_COMPILER_ASSERT( sizeof(v1[0]) == 4 );
			int32x4_t counts1 = vld1q_s32(v1 + s +  0);
			int32x4_t counts2 = vld1q_s32(v1 + s +  4);
			int32x4_t counts3 = vld1q_s32(v1 + s +  8);
			int32x4_t counts4 = vld1q_s32(v1 + s + 12);

			RR_COMPILER_ASSERT( sizeof(v2[0]) == 1 );
			int8x16_t codelens_s8 = vld1q_s8(v2 + s);

			int16x8_t codelens1_s16 = vmovl_s8(vget_low_s8(codelens_s8));
			int16x8_t codelens2_s16 = vmovl_s8(vget_high_s8(codelens_s8));

			// and accumulate
			accum1 = vmlaq_s32(accum1, counts1, vmovl_s16(vget_low_s16 (codelens1_s16)));
			accum2 = vmlaq_s32(accum2, counts2, vmovl_s16(vget_high_s16(codelens1_s16)));
			accum3 = vmlaq_s32(accum3, counts3, vmovl_s16(vget_low_s16 (codelens2_s16)));
			accum4 = vmlaq_s32(accum4, counts4, vmovl_s16(vget_high_s16(codelens2_s16)));
		}
	}

	// final sum reduction
	accum1 = vaddq_s32(accum1, accum3);
	accum2 = vaddq_s32(accum2, accum4);
	accum1 = vaddq_s32(accum1, accum2);

	S32 sum_NEON = hsum_s32_neon(accum1);

	#if 0
	S32 sum = 0;

	for LOOP(s,num)
	{
		int n = v1[s];
		int cl = v2[s];

		sum += n * cl;
	}

	// check vs scalar :
	RR_ASSERT_ALWAYS( sum == sum_NEON );
	#endif

	return sum_NEON;

	#else
	
	S32 sum = 0;
	
	for LOOP(s,num)
	{
		int n = v1[s];
		int cl = v2[s];
		
		sum += n * cl;
	}
	
	return sum;	
	
	#endif
}



#ifdef __RADSSE2__

void newlz_multiarrays_trellis_core_sse2(
	U64 * switch_flags,
	U8  * cheapest_entropyset,
	const U8 * ptr, const U8 * base, const U8 *end,
	const U16 * entropyset_cost,
	const U16 * histo_codelens_transposed,
	int num_entropysets,int num_entropysets_padded,
	int prev_cost_cheapest,int switch_histo_cost_codelen)
{
	// we will use vector v_prev_cost_cheapest here
	// take scalar prev_cost_cheapest from the pos==0 loop
	// henceforth we never update the scalar costs
	__m128i v_prev_cost_cheapest = _mm_set1_epi16((short)prev_cost_cheapest);
		
	__m128i v_switch_histo_cost_codelen = _mm_set1_epi16((short)switch_histo_cost_codelen);
	
	RR_ASSERT( rrIsAlignedPointer(entropyset_cost,16) );
	RR_ASSERT( rrIsAlignedPointer(histo_codelens_transposed,16) );
	
	if ( num_entropysets <= 8 )
	{
		RR_ASSERT( num_entropysets_padded == 8 );
	
		__m128i v_entropyset_cost = _mm_load_si128((__m128i *)(entropyset_cost));
	
		// scalar entropyset_cost is no longer used, kept in vec
	
		while(ptr<end)
		{			
			SINTa pos = rrPtrDiff(ptr - base);
			U32 sym = *ptr++;

			const U16 * codelens_for_sym = histo_codelens_transposed + 8 * sym;
			RR_ASSERT( rrIsAlignedPointer(codelens_for_sym,16) );
	
			__m128i sym_cost = _mm_load_si128((__m128i *)(codelens_for_sym));
			
			__m128i cost_from_self = _mm_sub_epi16(v_entropyset_cost,v_prev_cost_cheapest);
				
			__m128i min_prev_cost = _mm_min_epi16(cost_from_self,v_switch_histo_cost_codelen);
			
			v_entropyset_cost = _mm_add_epi16(sym_cost,min_prev_cost);
				
			__m128i do_switch_v16 = _mm_cmpgt_epi16(cost_from_self,v_switch_histo_cost_codelen);
			__m128i do_switch_v8 = _mm_packs_epi16(do_switch_v16,_mm_setzero_si128());
			U64 switch_flag_set = _mm_movemask_epi8(do_switch_v8);
			RR_ASSERT( switch_flag_set < 256 );
			
			int cur_i_cheapest;
			{
			// find min cost :
			// just need horizontal min in v_entropyset_cost
				
			// find the min and replicate the min in every lane :			
			__m128i v_min_cost = v_entropyset_cost;				
			v_min_cost = _mm_min_epi16(v_min_cost, _mm_shuffle_epi32(v_min_cost, _MM_SHUFFLE(1,0,3,2)));
			v_min_cost = _mm_min_epi16(v_min_cost, _mm_shuffle_epi32(v_min_cost, _MM_SHUFFLE(2,3,0,1)));
			// just need to swap 16-bit pairs ; no nice way to do this in SSE2 ?
			//	(pshufb obviously in SSSE3)
			v_min_cost = _mm_min_epi16(v_min_cost, _mm_shufflelo_epi16(_mm_shufflehi_epi16(
										v_min_cost, _MM_SHUFFLE(2,3,0,1)),_MM_SHUFFLE(2,3,0,1)));
				
			RR_DURING_ASSERT( U16 cur_cost_cheapest = (U16) _mm_extract_epi16(v_min_cost,0) );

			__m128i cmp = _mm_cmpeq_epi16(v_entropyset_cost,v_min_cost);
			int mask = _mm_movemask_epi8(cmp);
			RR_ASSERT( mask != 0 );
			int which = rrCtz32(mask);
			cur_i_cheapest = (which>>1);
			RR_ASSERT( cur_i_cheapest < num_entropysets );
			RR_ASSERT( ((S16 *)&v_entropyset_cost)[cur_i_cheapest] == cur_cost_cheapest );
			
			v_prev_cost_cheapest = v_min_cost;
			}
			
			switch_flags[pos] = switch_flag_set;
			cheapest_entropyset[pos] = U8_check(cur_i_cheapest);
			
		}
	}
	else
	{				
		while(ptr<end)
		{			
			SINTa pos = rrPtrDiff(ptr - base);
			U32 sym = *ptr++;
			const U16 * codelens_for_sym = histo_codelens_transposed + (U32)num_entropysets_padded * sym;
							
			RR_ASSERT( rrIsAlignedPointer(codelens_for_sym,16) );
			
			U64 switch_flag_set = 0;
	    
			// consider coding current byte with all histos :

			// each entropy set considers two arrivals				
			// arrive from self, or cheapest other

			// v_prev_cost_cheapest is the cheapest cost in all lanes
						
			// start v_min_cost as large
			// will get the vertical min	
			__m128i v_min_cost = _mm_set1_epi16(RR_S16_MAX);
			
			for(int hi = 0; hi < num_entropysets_padded ;hi+=8)
			{
				__m128i sym_cost = _mm_load_si128((__m128i *)(codelens_for_sym+hi));
				__m128i cost_from_self = _mm_load_si128((__m128i *)(entropyset_cost+hi));
			
				// make cost relative to prev cheapest
				//	- this preps for the cmp and also keeps us bounded for S16 storage
				cost_from_self = _mm_sub_epi16(cost_from_self,v_prev_cost_cheapest);
				
				// I can arrive from self or a switch :
				__m128i min_prev_cost = _mm_min_epi16(cost_from_self,v_switch_histo_cost_codelen);
				// add on current symbol cost :
				__m128i cost = _mm_add_epi16(sym_cost,min_prev_cost);
			
				// accumulate vertical min :	
				v_min_cost = _mm_min_epi16(v_min_cost,cost);
				
				_mm_store_si128((__m128i *)(entropyset_cost+hi),cost);
				
				// if cost from self is > switch_cost , flag a switch in that lane :
				__m128i do_switch_v16 = _mm_cmpgt_epi16(cost_from_self,v_switch_histo_cost_codelen);
				__m128i do_switch_v8 = _mm_packs_epi16(do_switch_v16,_mm_setzero_si128());
				U64 do_switch_mask = _mm_movemask_epi8(do_switch_v8);
				RR_ASSERT( do_switch_mask < 256 );						
				switch_flag_set |= do_switch_mask<<hi;
			}
			
			// get min cost :
			
			int cur_i_cheapest;
			
			{					
				// v_min_cost has the vertical min cost (across the 8-set lanes)
				// need to find the horizontal min of it now :

				// find the min and replicate the min in every lane :										
				v_min_cost = _mm_min_epi16(v_min_cost, _mm_shuffle_epi32(v_min_cost, _MM_SHUFFLE(1,0,3,2)));
				v_min_cost = _mm_min_epi16(v_min_cost, _mm_shuffle_epi32(v_min_cost, _MM_SHUFFLE(2,3,0,1)));
				// just need to swap 16-bit pairs ; no nice way to do this in SSE2 ?
				//	(pshufb obviously in SSSE3)
				v_min_cost = _mm_min_epi16(v_min_cost, _mm_shufflelo_epi16(_mm_shufflehi_epi16(
											v_min_cost, _MM_SHUFFLE(2,3,0,1)),_MM_SHUFFLE(2,3,0,1)));
				
				
				RR_DURING_ASSERT( U16 cur_cost_cheapest = (U16) _mm_extract_epi16(v_min_cost,0) );
				
				/*
				U16 test[8];
				_mm_storeu_si128((__m128i *)test,v_min_cost);
				for LOOP(i,8) RR_ASSERT( test[i] == cur_cost_cheapest );
				*/
				
				// find cur_i_cheapest :
				// find which of the 8-set lanes matches v_min_cost
				for(int hi = 0; ;hi+=8)
				{
					// loop terminates when found; should not go past end :
					RR_ASSERT( hi < num_entropysets );
					
					__m128i cost = _mm_loadu_si128((__m128i *)(entropyset_cost+hi));
					__m128i cmp = _mm_cmpeq_epi16(cost,v_min_cost);
					int mask = _mm_movemask_epi8(cmp);
					if ( mask )
					{
						int which = rrCtz32(mask);
						cur_i_cheapest = hi + (which>>1);
						RR_ASSERT( cur_i_cheapest < num_entropysets );
						RR_ASSERT( entropyset_cost[cur_i_cheapest] == cur_cost_cheapest );
						break;
					}
				}
			}
						
			RR_ASSERT( cur_i_cheapest != -1 );

			switch_flags[pos] = switch_flag_set;
			cheapest_entropyset[pos] = U8_check(cur_i_cheapest);
			
			v_prev_cost_cheapest = v_min_cost;
		}
	}
}

#endif // __RADSSE2__


#ifdef __RADNEON__

void newlz_multiarrays_trellis_core_neon(
	U64 * switch_flags,
	U8  * cheapest_entropyset,
	const U8 * ptr, const U8 * base, const U8 *end,
	const U16 * entropyset_cost,
	const U16 * histo_codelens_transposed,
	int num_entropysets,int num_entropysets_padded,
	int prev_cost_cheapest,int switch_histo_cost_codelen)
{
	static const RAD_ALIGN(S16, lane_id[8], 16) = { 0,1,2,3,4,5,6,7 };

	// we will use vector v_prev_cost_cheapest here
	// take scalar prev_cost_cheapest from the pos==0 loop
	// henceforth we never update the scalar costs
	int16x8_t v_prev_cost_cheapest = vdupq_n_s16((short)prev_cost_cheapest);
	int16x8_t v_switch_histo_cost_codelen = vdupq_n_s16((short)switch_histo_cost_codelen);

	RR_ASSERT( rrIsAlignedPointer(entropyset_cost,16) );
	RR_ASSERT( rrIsAlignedPointer(histo_codelens_transposed,16) );

	if ( num_entropysets <= 8 )
	{
		// cost stays in a vector, no loads/stores
		RR_ASSERT( num_entropysets_padded == 8 );

		int16x8_t v_entropyset_cost = vld1q_s16((const S16 *)entropyset_cost);
		int16x8_t v_lane_id = vld1q_s16(lane_id);

		// scalar entropyset_cost is no longer used, kept in vec

		while(ptr<end)
		{
			SINTa pos = rrPtrDiff(ptr - base);
			U32 sym = *ptr++;

			const U16 * codelens_for_sym = histo_codelens_transposed + 8 * sym;
			RR_ASSERT( rrIsAlignedPointer(codelens_for_sym,16) );

			int16x8_t sym_cost = vld1q_s16((const S16 *)codelens_for_sym);
			int16x8_t cost_from_self = vsubq_s16(v_entropyset_cost,v_prev_cost_cheapest);
			int16x8_t min_prev_cost = vminq_s16(cost_from_self,v_switch_histo_cost_codelen);
			v_entropyset_cost = vaddq_s16(sym_cost,min_prev_cost);

			uint16x8_t do_switch_v16 = vcgtq_s16(cost_from_self,v_switch_histo_cost_codelen);
			U64 switch_flag_set = neon_movemask_u16(do_switch_v16);
			RR_ASSERT( switch_flag_set < 256 );

			int cur_i_cheapest;
			{
			// find both min cost and lane index it appears in
			// by reducing a S32 min on (value<<16)+lane_id
			#ifndef __RADLITTLEENDIAN__
			#error expected LE
			#endif

			int16x8x2_t merged = vzipq_s16(v_lane_id, v_entropyset_cost);
			int32x4_t cost0 = vreinterpretq_s32_s16(merged.val[0]);
			int32x4_t cost1 = vreinterpretq_s32_s16(merged.val[1]);

			// reduce
			int16x4_t min_cost_and_lane = vreinterpret_s16_s32(neon_reduce_min_s32(vminq_s32(cost0, cost1)));
			RR_DURING_ASSERT( U16 cur_cost_cheapest = (U16) vget_lane_s16(min_cost_and_lane,1) );

			// broadcast result
			// this grabs the min straight from the top half
			v_prev_cost_cheapest = vdupq_lane_s16(min_cost_and_lane,1);
			cur_i_cheapest = vget_lane_s16(min_cost_and_lane,0);
			RR_ASSERT( cur_i_cheapest < num_entropysets );
			}

			switch_flags[pos] = switch_flag_set;
			cheapest_entropyset[pos] = U8_check(cur_i_cheapest);
		}
	}
	else
	{
		int16x8_t v_base_lane_id = vld1q_s16(lane_id);

		while(ptr<end)
		{
			SINTa pos = rrPtrDiff(ptr - base);
			U32 sym = *ptr++;
			const U16 * codelens_for_sym = histo_codelens_transposed + (U32)num_entropysets_padded * sym;

			RR_ASSERT( rrIsAlignedPointer(codelens_for_sym,16) );

			U64 switch_flag_set = 0;

			// consider coding current byte with all histos :

			// each entropy set considers two arrivals
			// arrive from self, or cheapest other

			// v_prev_cost_cheapest is the cheapest cost in all lanes

			// start v_min_cost_and_lane as large
			int32x4_t v_min_cost_and_lane = vdupq_n_s32(RR_S16_MAX << 16);
			int16x8_t v_lane_id = v_base_lane_id;

			for(int hi = 0; hi < num_entropysets_padded; hi+=8)
			{
				int16x8_t sym_cost = vld1q_s16((const S16 *)(codelens_for_sym+hi));
				int16x8_t cost_from_self = vld1q_s16((const S16 *)(entropyset_cost + hi));

				// make cost relative to prev cheapest
				//	- this preps for the cmp and also keeps us bounded for S16 storage
				cost_from_self = vsubq_s16(cost_from_self,v_prev_cost_cheapest);

				// I can arrive from self or a switch :
				int16x8_t min_prev_cost = vminq_s16(cost_from_self,v_switch_histo_cost_codelen);
				// add on current symbol cost :
				int16x8_t cost = vaddq_s16(sym_cost,min_prev_cost);
				vst1q_s16((S16 *)(entropyset_cost + hi),cost);

				// shove in lane IDs and accumulate mins vertically
				// we use (cost<<16) + lane_id
				#ifndef __RADLITTLEENDIAN__
				#error expected LE
				#endif

				int16x8x2_t merged = vzipq_s16(v_lane_id, cost);
				int32x4_t cost32_0 = vreinterpretq_s32_s16(merged.val[0]);
				int32x4_t cost32_1 = vreinterpretq_s32_s16(merged.val[1]);
				v_lane_id = vaddq_s16(v_lane_id, vdupq_n_s16(8));

				v_min_cost_and_lane = vminq_s32(v_min_cost_and_lane, cost32_0);
				v_min_cost_and_lane = vminq_s32(v_min_cost_and_lane, cost32_1);

				// if cost from self is > switch_cost, flag a switch in that lane
				uint16x8_t do_switch_v16 = vcgtq_s16(cost_from_self,v_switch_histo_cost_codelen);
				U64 do_switch_mask = neon_movemask_u16(do_switch_v16);
				RR_ASSERT( do_switch_mask < 256 );
				switch_flag_set |= do_switch_mask<<hi;
			}

			// finalize reduction
			int cur_i_cheapest;

			{
				int16x4_t min_cost_and_lane = vreinterpret_s16_s32(neon_reduce_min_s32(v_min_cost_and_lane));
				RR_DURING_ASSERT( U16 cur_cost_cheapest = (U16) vget_lane_s16(min_cost_and_lane,1) );

				// broadcast result
				// this grabs the min straight from the top half
				v_prev_cost_cheapest = vdupq_lane_s16(min_cost_and_lane,1);
				cur_i_cheapest = vget_lane_s16(min_cost_and_lane,0);
				RR_ASSERT( cur_i_cheapest < num_entropysets );
			}

			switch_flags[pos] = switch_flag_set;
			cheapest_entropyset[pos] = U8_check(cur_i_cheapest);
		}
	}
}

#endif // __RADNEON__


// offsets = offsets * multiplier - subtrahend
void simd_mul_s32_sub_u8(S32 * offsets, SINTa offsets_count, S32 multiplier, const U8 * subtrahend )
{
	SINTa offi = 0;

	#if defined(__RADSSE2__)
	
	if ( rrsimd_has_sse4() )
	{
		simd_mul_s32_sub_u8_sse4(offsets,offsets_count,multiplier,subtrahend);
		return;
	}
	
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
		offsets1 = _mm_mullo_epi32_sse2(offsets1,v_offset_alt_modulo);
		offsets2 = _mm_mullo_epi32_sse2(offsets2,v_offset_alt_modulo);
		offsets3 = _mm_mullo_epi32_sse2(offsets3,v_offset_alt_modulo);
		offsets4 = _mm_mullo_epi32_sse2(offsets4,v_offset_alt_modulo);
	
		// unpack 8 -> 32 :				
		__m128i low_s8 = _mm_loadu_si128((__m128i *)(subtrahend+offi));
		
		__m128i low1_s16 = _mm_unpacklo_epi8(low_s8,_mm_setzero_si128());
		__m128i low2_s16 = _mm_unpackhi_epi8(low_s8,_mm_setzero_si128());
		
		__m128i low1 = _mm_unpacklo_epi16(low1_s16, _mm_setzero_si128());
		__m128i low2 = _mm_unpackhi_epi16(low1_s16, _mm_setzero_si128());	
		__m128i low3 = _mm_unpacklo_epi16(low2_s16, _mm_setzero_si128());
		__m128i low4 = _mm_unpackhi_epi16(low2_s16, _mm_setzero_si128());	
	
		_mm_store_si128((__m128i *)(offsets+offi   ), _mm_sub_epi32(offsets1,low1) );
		_mm_store_si128((__m128i *)(offsets+offi+4 ), _mm_sub_epi32(offsets2,low2) );
		_mm_store_si128((__m128i *)(offsets+offi+8 ), _mm_sub_epi32(offsets3,low3) );
		_mm_store_si128((__m128i *)(offsets+offi+12), _mm_sub_epi32(offsets4,low4) );
	}

	#elif defined(__RADNEON__)

	RR_ASSERT( rrIsAlignedPointer(offsets,16) );
	int32x4_t v_offset_alt_modulo = vdupq_n_s32(multiplier);
	uint8x8_t v_zero_u8 = vdup_n_u8(0);
	
	for(; offi+16 <= offsets_count ;offi += 16)
	{
		int32x4_t offs0 = vld1q_s32(offsets + offi +  0);
		int32x4_t offs1 = vld1q_s32(offsets + offi +  4);
		int32x4_t offs2 = vld1q_s32(offsets + offi +  8);
		int32x4_t offs3 = vld1q_s32(offsets + offi + 12);

		uint8x16_t sub_u8 = vld1q_u8(subtrahend + offi);

		// negate subtrahend and widen to S16
		int16x8_t add0_s16 = vreinterpretq_s16_u16(vsubl_u8(v_zero_u8, vget_low_u8 (sub_u8)));
		int16x8_t add1_s16 = vreinterpretq_s16_u16(vsubl_u8(v_zero_u8, vget_high_u8(sub_u8)));

		// widen subtrahend once more and multiply-accumulate
		offs0 = vmlaq_s32(vmovl_s16(vget_low_s16 (add0_s16)), offs0, v_offset_alt_modulo);
		offs1 = vmlaq_s32(vmovl_s16(vget_high_s16(add0_s16)), offs1, v_offset_alt_modulo);
		offs2 = vmlaq_s32(vmovl_s16(vget_low_s16 (add1_s16)), offs2, v_offset_alt_modulo);
		offs3 = vmlaq_s32(vmovl_s16(vget_high_s16(add1_s16)), offs3, v_offset_alt_modulo);

		vst1q_s32(offsets + offi +  0, offs0);
		vst1q_s32(offsets + offi +  4, offs1);
		vst1q_s32(offsets + offi +  8, offs2);
		vst1q_s32(offsets + offi + 12, offs3);
	}
	#endif
			
	for (;offi<offsets_count;offi++)
	{
		offsets[offi] = (offsets[offi] * multiplier) - (S32)subtrahend[offi];
	}
}

void simd_interleave_8x2(U16 * off16s,SINTa num_off16s,const U8 * off16s_lo,const U8 * off16s_hi)
{
	#ifdef __RADSSE2__
	{
	int i =0;

	#ifndef __RADLITTLEENDIAN__
	#error expected LE
	#endif
	
	// 16s with SSE2 :
	for(;i<(int)(num_off16s-15);i+=16)
	{
		__m128i lo = _mm_loadu_si128((__m128i *)(off16s_lo+i));
		__m128i hi = _mm_loadu_si128((__m128i *)(off16s_hi+i));
		__m128i a = _mm_unpacklo_epi8(lo,hi);
		__m128i b = _mm_unpackhi_epi8(lo,hi);
		_mm_storeu_si128((__m128i *)(off16s+i),a);
		_mm_storeu_si128((__m128i *)(off16s+i+8),b);
	}
	
	// tail :
	for(;i<(int)num_off16s;i++)
	{
		off16s[i] = (off16s_hi[i]<<8) | off16s_lo[i];
	}
	
	}		
	#elif defined(__RADNEON__)		
	{
	int i =0;
			
	#ifndef __RADLITTLEENDIAN__
	#error expected LE
	#endif

	// on ARM it's just an interleaving store ; vst2
	
	for(;i<(int)(num_off16s-15);i+=16)
	{
		uint8x16x2_t hilo;
		hilo.val[0] = vld1q_u8((uint8_t *)(off16s_lo+i));
		hilo.val[1] = vld1q_u8((uint8_t *)(off16s_hi+i));
		vst2q_u8 ((uint8_t *)(off16s+i), hilo);
	}
	
	// tail :
	for(;i<(int)num_off16s;i++)
	{
		off16s[i] = (off16s_hi[i]<<8) | off16s_lo[i];
	}
	
	}
	#else
		
	// naive :

	#ifdef __RADLITTLEENDIAN__

	for(int i=0;i<(int)num_off16s;i++)
	{
		off16s[i] = (off16s_hi[i]<<8) | off16s_lo[i];
	}

	#else

	for(int i=0;i<(int)num_off16s;i++)
	{
		off16s[i] = (off16s_lo[i]<<8) | off16s_hi[i];
	}

	#endif
	
	#endif	
}


extern U32 simd_find_max_U32_256_sse4(const U32 * histogram);

U32 simd_find_max_U32_256(const U32 * histogram)
{
	#ifdef __RADSSE2__
	if ( rrsimd_has_sse4() )
	{
		return simd_find_max_U32_256_sse4(histogram);
	}
	#endif
	
	// find largest :
	U32 largest0 = 0;
	U32 largest1 = 0;
	U32 largest2 = 0;
	U32 largest3 = 0;
	for LOOP(i,(256/4))
	{
		// @@ looks like simd : (or could be done in the CountHisto loop)
		largest0 = RR_MAX(largest0,histogram[i*4+0]);
		largest1 = RR_MAX(largest1,histogram[i*4+1]);
		largest2 = RR_MAX(largest2,histogram[i*4+2]);
		largest3 = RR_MAX(largest3,histogram[i*4+3]);
	}
	U32 highest_histo_count = RR_MAX4(largest0,largest1,largest2,largest3);
	return highest_histo_count;
}

OODLE_NS_END
