// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrsimd.h"

//=========

OODLE_NS_START

//=========
/**

_256 means do 256

_256 with "num" means array sizes are 256, num <= 256, and you may overrun
	furthermore, the arrays have zeros past "num" so you may over-read

"num" without _256 means don't overrun / don't overread

**/

// to = fm1+fm2 or to = fm1-fm2
// to == fm is okay
void simd_add_u32_256(U32 * to, const U32 * fm1, const U32 * fm2);
void simd_sub_u32_256(U32 * to, const U32 * fm1, const U32 * fm2);

// dotproduct components (v1[i]*v2[i]) must fit in S32
//	overflow is undefined

S32 simd_dotproduct_s16_s16_256(const S16 * v1, const S16 * v2);
S32 simd_dotproduct_s32_s16_256(const S32 * v1, bool v1_fits_in_S16, const S16 * v2);
S32 simd_dotproduct_s32_s8_256( const S32 * v1, bool v1_fits_in_S16, const S8 * v2,int num);

S32 simd_horizontal_sum_s32(const S32 *v,int num);

// offsets = offsets * multiplier - subtrahend
// offsets must be 16-aligned
void simd_mul_s32_sub_u8(S32 * offsets, SINTa offsets_count, S32 multiplier, const U8 * subtrahend );

// simd_mul_s32_sub_u8 calls simd_mul_s32_sub_u8_sse4 for you if possible
void simd_mul_s32_sub_u8_sse4(S32 * offsets, SINTa offsets_count, S32 multiplier, const U8 * subtrahend );

// out = lo,hi,lo,hi
void simd_interleave_8x2(U16 * off16s,SINTa num_off16s,const U8 * off16s_lo,const U8 * off16s_hi);
		
U32 simd_find_max_U32_256(const U32 * histogram);

//=========

#ifdef __RADSSE2__

#endif
	
//=========

#if defined(__RADSSE2__)
	
// horizontal sum
static RADINLINE S32 hsum_epi32_sse2(__m128i x)
{
    __m128i y = _mm_add_epi32(x, _mm_shuffle_epi32(x, _MM_SHUFFLE(1,0,3,2)) );
    __m128i z = _mm_add_epi32(y, _mm_shuffle_epi32(y, _MM_SHUFFLE(2,3,0,1)) );
    // horizontal sum, same value in every lane now
    return _mm_cvtsi128_si32(z);
}

// Inclusive prefix sum
//   xout = simd_prefix_sum_u8(x)
//   xout[0] = x[0]
//   xout[1] = x[0] + x[1] = sum(x[0..1])
//   ...
//   xout[k] = sum(x[0..k])
static RADFORCEINLINE __m128i simd_prefix_sum_u8(__m128i x)
{
	x = _mm_add_epi8(x, _mm_slli_si128(x, 1));
	x = _mm_add_epi8(x, _mm_slli_si128(x, 2));
	x = _mm_add_epi8(x, _mm_slli_si128(x, 4));
	x = _mm_add_epi8(x, _mm_slli_si128(x, 8));
	return x;
}

#ifdef DO_SSE4_ALWAYS

// known SSE4 platform, just use it

#define _mm_mullo_epi32_sse2 _mm_mullo_epi32

#else

// _mm_mullo_epi32 is SSE4 , does 4x 32*32 -> 4x 32
// _mm_mul_epu32 is SSE2 , does 2x 32*32 -> 2x64
static RADINLINE __m128i _mm_mullo_epi32_sse2(const __m128i &a, const __m128i &b)
{
    __m128i mul20 = _mm_mul_epu32(a,b); // mul 2,0
    __m128i mul31 = _mm_mul_epu32( _mm_srli_si128(a,4), _mm_srli_si128(b,4)); // mul 3,1
    return _mm_unpacklo_epi32(
			_mm_shuffle_epi32(mul20, _MM_SHUFFLE (0,0,2,0)), 
			_mm_shuffle_epi32(mul31, _MM_SHUFFLE (0,0,2,0))); 
}

#endif

#elif defined(__RADNEON__)

// Inclusive prefix sum
//   xout = simd_prefix_sum_u8(x)
//   xout[0] = x[0]
//   xout[1] = x[0] + x[1] = sum(x[0..1])
//   ...
//   xout[k] = sum(x[0..k])
static RADFORCEINLINE uint8x16_t simd_prefix_sum_u8(uint8x16_t x)
{
	uint8x16_t zero = vdupq_n_u8(0);
	// Same algorithm as SSE2 version above.
	x = vaddq_u8(x, vextq_u8(zero, x, 16-1));
	x = vaddq_u8(x, vextq_u8(zero, x, 16-2));
	x = vaddq_u8(x, vextq_u8(zero, x, 16-4));
	x = vaddq_u8(x, vextq_u8(zero, x, 16-8));
	return x;
}

// horizontal sum
static RADINLINE S32 hsum_s32_neon(int32x4_t x)
{
	int32x2_t sum_pairs = vadd_s32(vget_low_s32(x), vget_high_s32(x)); // sum with elements 2 away
	int32x2_t final_sum = vpadd_s32(sum_pairs, sum_pairs); // sum with element 1 away
	return vget_lane_s32(final_sum, 0);
}

// Checks whether all lanes after a U8 compare are 0xff
static RADFORCEINLINE bool check_all_set_u8_neon(uint8x16_t compare_result)
{
	// kind of tortured way to test due to lack of a SSE2 PMOVMSKB equivalent.
	// we want 0xff in all lanes (and the lanes post-compare are either 0x00 or 0xff)
	// so sum groups of 4 lanes, then check wheter they're all -4
	uint8x8_t sum1 = vadd_u8(vget_low_u8(compare_result), vget_high_u8(compare_result)); // reduce 16 lanes -> 8
	uint8x8_t sum2 = vpadd_u8(sum1, sum1); // reduce 8 lanes -> 4
	return vget_lane_u32(vreinterpret_u32_u8(sum2), 0) == 0xfcfcfcfcu;
}

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

//=========

// _mm_cmpgt_epi8 is for signed char (S8)
//	need a cmp for U8 :
#define _mm_cmpge_epu8(a,b)	_mm_cmpeq_epi8( a, _mm_max_epu8(a,b))
#define _mm_cmple_epu8(a,b)	_mm_cmpge_epu8(b, a)

// GT is just the NOT of LE
//#define _mm_cmpgt_epu8(a,b) _mm_xor_si128(_mm_cmple_epu8(a,b), _mm_set1_epi8(-1))
//#define _mm_cmplt_epu8(a,b) _mm_cmpgt_epu8(b,a)

// alternative :
//#define _mm_cmpgt_epu8(a,b) _mm_cmpgt_epi8( _mm_add_epi8(a,_mm_set1_epi8(-0x80)) , _mm_add_epi8(b,_mm_set1_epi8(-0x80)) )
//#define _mm_cmplt_epu8(a,b) _mm_cmpgt_epu8(b,a)

//=========

// returns true if any value in [vec] is > vs
// needs to be inline in header so vec of "vs" can become a constant
//	(could pass that in if we had a generic vec mechanism)
static RADINLINE rrbool simd_cmpgt_u8(const U8 * vec, SINTa len, const int vs)
{

#ifdef __RADSSE2__

	// we want cmpgt, we're using cmpge, so do +1
	__m128i vs_plus1 = _mm_set1_epi8((char)(vs+1));

	SINTa i = 0;
	for(;(i+16)<=len;i+=16)
	{
		__m128i x = _mm_loadu_si128((__m128i const *)(vec+i));
		__m128i c = _mm_cmpge_epu8(x,vs_plus1); // x >= vs_plus1
		int m = _mm_movemask_epi8(c);
		if ( m )
			return true;
	}

	for(;i<len;i++)
	{
		if ( vec[i] > vs ) return true;
	}
	return false;

#else

	// @@ need NEON

	for (SINTa i=0;i<len;i++)
	{
		if ( vec[i] > vs ) return true;
	}
	return false;

#endif

}

OODLE_NS_END
