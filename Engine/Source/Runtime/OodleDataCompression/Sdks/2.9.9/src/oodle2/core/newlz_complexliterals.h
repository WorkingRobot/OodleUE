// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once
#include "newlz_subliterals.h"

OODLE_NS_START

#if defined(__RADSSE2__)

typedef __m128i NEWLZ_COPY8SUBAND3_RET;

static RADFORCEINLINE NEWLZ_COPY8SUBAND3_RET newlz_literals_copy8suband3(U8 * to_ptr,const U8 * match_ptr,
	const U8 * ptr0,const U8 * ptr1,const U8 * ptr2,const U8 * ptr3)
{
	// reads 4 bytes from each stream
	// -> some amount of over-read, which our fuzz handling takes care of

	// NOTE: had an earlier Jaguar ver here that stuffed the 4 values into one __m128 directly (e.g.
	// via PINSRD) and then did one PSHUFB, but this code performed slightly better and is stock
	// SSE2, so pure win.

	// read
	__m128i x0 = _mm_cvtsi32_si128(RR_GET32_LE( ptr0 ));
	__m128i x1 = _mm_cvtsi32_si128(RR_GET32_LE( ptr1 ));
	__m128i x2 = _mm_cvtsi32_si128(RR_GET32_LE( ptr2 ));
	__m128i x3 = _mm_cvtsi32_si128(RR_GET32_LE( ptr3 ));

	// interleave
	__m128i x01 = _mm_unpacklo_epi8(x0, x1);
	__m128i x23 = _mm_unpacklo_epi8(x2, x3);
	__m128i x = _mm_unpacklo_epi16(x01, x23);

	__m128i v = _mm_add_epi8( _mm_loadl_epi64( (const __m128i *)match_ptr ), x );
	_mm_storel_epi64((__m128i *)(to_ptr), v);
	return x;
}

static RADFORCEINLINE void newlz_literals_copy8suband3_second(U8 * to_ptr,const U8 * match_ptr,
	const U8 * ptr0,const U8 * ptr1,const U8 * ptr2,const U8 * ptr3, NEWLZ_COPY8SUBAND3_RET prev)
{
	// we already read all the literals
	__m128i v = _mm_srli_si128(prev, 8); // grab the top 8 bytes from prev iter

	v = _mm_add_epi8( _mm_loadl_epi64( (const __m128i *)match_ptr ), v );
	_mm_storel_epi64((__m128i *)(to_ptr), v);
}

#elif defined(__RADNEON__)

typedef uint8x8_t NEWLZ_COPY8SUBAND3_RET;

static RADFORCEINLINE NEWLZ_COPY8SUBAND3_RET newlz_literals_copy8suband3(U8 * to_ptr,const U8 * match_ptr,
	const U8 * ptr0,const U8 * ptr1,const U8 * ptr2,const U8 * ptr3)
{
#if defined(__GNUC__) || defined(__clang__)
	typedef U32 U32un __attribute__((aligned(1))); // unaligned U32 - we need _unaligned_ 32-bit loads here.
#else
	typedef U32 U32un;
#endif

	// reads 4 bytes from each stream
	// -> some amount of over-read, which our fuzz handling takes care of

	// read
	uint8x8_t x0 = vreinterpret_u8_u32(vld1_dup_u32((const U32un *)ptr0));
	uint8x8_t x1 = vreinterpret_u8_u32(vld1_dup_u32((const U32un *)ptr1));
	uint8x8_t x2 = vreinterpret_u8_u32(vld1_dup_u32((const U32un *)ptr2));
	uint8x8_t x3 = vreinterpret_u8_u32(vld1_dup_u32((const U32un *)ptr3));

	// interleave
	uint8x8_t x02 = vzip_u8(x0, x2).val[0];
	uint8x8_t x13 = vzip_u8(x1, x3).val[0];
	uint8x8x2_t x = vzip_u8(x02, x13);

	uint8x8_t v = vadd_u8(vld1_u8(match_ptr), x.val[0]);
	vst1_u8(to_ptr, v);
	return x.val[1];
}

static RADFORCEINLINE void newlz_literals_copy8suband3_second(U8 * to_ptr,const U8 * match_ptr,
	const U8 * ptr0,const U8 * ptr1,const U8 * ptr2,const U8 * ptr3, NEWLZ_COPY8SUBAND3_RET prev)
{
	// we already read and interleaved all the literals
	uint8x8_t v = vadd_u8(vld1_u8(match_ptr), prev);
	vst1_u8(to_ptr, v);
}

#else

typedef int NEWLZ_COPY8SUBAND3_RET; // not used here

static RADFORCEINLINE NEWLZ_COPY8SUBAND3_RET newlz_literals_copy8suband3(U8 * to_ptr,const U8 * match_ptr,
	const U8 * ptr0,const U8 * ptr1,const U8 * ptr2,const U8 * ptr3)
{
	to_ptr[0] = (U8)( ptr0[0] + match_ptr[0] );
	to_ptr[1] = (U8)( ptr1[0] + match_ptr[1] );
	to_ptr[2] = (U8)( ptr2[0] + match_ptr[2] );
	to_ptr[3] = (U8)( ptr3[0] + match_ptr[3] );
	to_ptr[4] = (U8)( ptr0[1] + match_ptr[4] );
	to_ptr[5] = (U8)( ptr1[1] + match_ptr[5] );
	to_ptr[6] = (U8)( ptr2[1] + match_ptr[6] );
	to_ptr[7] = (U8)( ptr3[1] + match_ptr[7] );
	return 0;
}

static RADFORCEINLINE void newlz_literals_copy8suband3_second(U8 * to_ptr,const U8 * match_ptr,
	const U8 * ptr0,const U8 * ptr1,const U8 * ptr2,const U8 * ptr3, NEWLZ_COPY8SUBAND3_RET prev)
{
	newlz_literals_copy8suband3(to_ptr,match_ptr,ptr0,ptr1,ptr2,ptr3);
}

#endif

//=======================================================================

#ifdef NEWLZ_LITERALS_TYPE_O1

// top 4 bits :
// average : 75,770,330 ->39,133,687 =  4.132 bpb =  1.936 to 1
#define NEWLZ_O1_CONTEXT_COUNT	16
#define newlz_o1_context( c )	((c)>>4)
// top3 bits :
// average : 75,770,330 ->39,239,301 =  4.143 bpb =  1.931 to 1
//#define newlz_o1_context( c )	((c)>>5)


// NOTE : newlz_copyliterals_o1 advances literals pointers
static RADFORCEINLINE void newlz_copyliterals_o1(U8 * to_ptr, SINTa lrl, U8 next_literals[], const U8 * literals_ptrs[])
{
	RR_ASSERT( lrl > 0 );

	UINTr val = to_ptr[-1];
	do
	{
		UINTr c = newlz_o1_context( val );
		const U8 * from_ptr = literals_ptrs[c];
		val = next_literals[c];
		*to_ptr++ = static_cast<U8>(val);
		next_literals[c] = *from_ptr++;
		literals_ptrs[c] = from_ptr;
	}
	while( --lrl );
}

#if 0
static RADFORCEINLINE void newlz_copyliteral_o1(U8 * to_ptr, const U8 * literals_ptrs[])
{
	U32 c = newlz_o1_context( to_ptr[-1] );
	const U8 * from_ptr = literals_ptrs[c];
	U8 val = *from_ptr++;
	literals_ptrs[c] = from_ptr;
	*to_ptr = val;
}
#endif

#endif // NEWLZ_LITERALS_TYPE_O1

#ifdef NEWLZ_LITERALS_TYPE_O2

#define NEWLZ_O2_CONTEXT_COUNT		64	// 6 bits

// without :
//total : 31,000,000 ->15,639,464 =  4.036 bpb =  1.982 to 1

#define newlz_o2_context( c1, c2, lam )	((((c1)>>5)<<3)|((c2)>>5)) // 15,617,711
// lzt70.wav	 Leviathan 	 Normal 	 6.026 bpb 	 -0.111 	
// lzt70.wav	 Leviathan 	 Normal 	 6.137 bpb 

// the o1+lam contexts are no good
// the best way is straightforward o2, 3 top bits of last two bytes
// this only helps on wav files & such
//  -> lame, I'd rather have a proper wav filter
//	  this is using contexts to learn a linear filter

//#define newlz_o2_context( c1, c2, lam )	((((c1)>>5)<<3)|((lam)>>5)) // 31,000,000 ->15,639,464

//#define newlz_o2_context( c1, c2, lam )	((((c2)>>6)<<4)|(((c1)>>6)<<2)|((lam)>>6)) // 2+2+2


// NOTE : newlz_copyliterals_o1 advances literals pointers
static RADFORCEINLINE void newlz_copyliterals_o2(U8 * to_ptr, const U8 * match_ptr, SINTa lrl, const U8 * literals_ptrs[])
{
	while(lrl--)
	{
		int lam = *match_ptr++;
		int c = newlz_o2_context( to_ptr[-1], to_ptr[-2] , lam );
		const U8 * from_ptr = literals_ptrs[c];
		*to_ptr++ = *from_ptr++;
		literals_ptrs[c] = from_ptr;
	}
}

#endif // NEWLZ_LITERALS_TYPE_O2

//=======================================================================



static RADFORCEINLINE void newlz_copyliterals_subandF(U8 * to_ptr, const U8 * match_ptr, SINTa lrl, const U8 * literals_ptrs[])
{
	// can be 0 due to tail
	//RR_ASSERT( lrl > 0 );
	
	while(lrl--)
	{
		UINTa index = ((UINTa)to_ptr)&0xF;
		const U8 * from_ptr = literals_ptrs[index];
		*to_ptr++ = (*from_ptr++) + (*match_ptr++);
		literals_ptrs[index] = from_ptr;
	}
}

static RADFORCEINLINE void newlz_copyliteral_subandF(U8 * to_ptr, const U8 * match_ptr, const U8 * literals_ptrs[])
{
	UINTa index = ((UINTa)to_ptr)&0xF;
	const U8 * from_ptr = literals_ptrs[index];
	*to_ptr = (*from_ptr++) + (*match_ptr);
	literals_ptrs[index] = from_ptr;
}

OODLE_NS_END
