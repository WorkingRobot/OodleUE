// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrbase.h"
#include "newlz_simd.h"

OODLE_NS_START

//=====================================================

#define NEWLZ_LITERALS_TYPE_SUB		0
#define NEWLZ_LITERALS_TYPE_RAW		1
#define NEWLZ_LITERALS_TYPE_LAMSUB	2
#define NEWLZ_LITERALS_TYPE_SUBAND3	3
#define NEWLZ_LITERALS_TYPE_O1		4
#define NEWLZ_LITERALS_TYPE_SUBANDF	5
#define NEWLZ_LITERALS_TYPE_MAX		5

extern const char * newlz_literals_type_name[];

#define newlz_literals_type_offset1_ok(lt) ( ( (lt) == NEWLZ_LITERALS_TYPE_RAW ) || ( (lt) == NEWLZ_LITERALS_TYPE_O1 ) )

//=====================================================

#ifdef __RADSSE2__

#define newlz_copyadd16(to,fm1,fm2) do { \
		__m128i v1 = _mm_loadu_si128((const __m128i *)(fm1)); \
		__m128i v2 = _mm_loadu_si128((const __m128i *)(fm2)); \
		__m128i v3 = _mm_add_epi8(v1,v2); \
		_mm_storeu_si128((__m128i *)(to),v3); \
	} while(0)
	
#define newlz_copyadd8(to,fm1,fm2) do { \
		__m128i v1 = _mm_loadl_epi64((const __m128i *)(fm1)); \
		__m128i v2 = _mm_loadl_epi64((const __m128i *)(fm2)); \
		__m128i v3 = _mm_add_epi8(v1,v2); \
		_mm_storel_epi64((__m128i *)(to),v3); \
	} while(0)
						
#define newlz_copyadd4(to,fm1,fm2) do { \
		__m128i v1 = _mm_cvtsi32_si128(*((const U32 *)(fm1))); \
		__m128i v2 = _mm_cvtsi32_si128(*((const U32 *)(fm2))); \
		__m128i v3 = _mm_add_epi8(v1,v2); \
		*((U32 *)(to)) = _mm_cvtsi128_si32(v3); \
	} while(0)
	
#elif defined(__RADNEON__)

// NOTE(fg): I originally saw some weird stuff with this enabled on
// the Apple A8X but it's looking like it's merely the usual good
// code layout/bad code layout fun with branch predictors.
#define newlz_copyadd16(to,fm1,fm2) do { \
	uint8x16_t a = vld1q_u8(fm1); \
	uint8x16_t b = vld1q_u8(fm2); \
	uint8x16_t sum = vaddq_u8(a, b); \
	vst1q_u8(to, sum); \
	} while(0)

#define newlz_copyadd8(to,fm1,fm2) do { \
	uint8x8_t a = vld1_u8(fm1); \
	uint8x8_t b = vld1_u8(fm2); \
	uint8x8_t sum = vadd_u8(a, b); \
	vst1_u8(to, sum); \
	} while(0)

// @@ CB : scalar ; neon here?
#define newlz_copyadd4(to,fm1,fm2) do { \
	U32 lo7mask = 0x7F7F7F7F; \
	U32 a = RR_GET32_NATIVE(fm1); \
	U32 b = RR_GET32_NATIVE(fm2); \
	U32 sum = (a & lo7mask) + (b & lo7mask); \
	sum ^= (a ^ b) & ~lo7mask; \
	RR_PUT32_NATIVE(to, sum); \
	} while(0)

#elif defined(__RADWASM_SIMD128__)

#define newlz_copyadd16(to,fm1,fm2) do { \
		v128_t v1 = wasm_v128_load(fm1); \
		v128_t v2 = wasm_v128_load(fm2); \
		v128_t v3 = wasm_i8x16_add(v1,v2); \
		wasm_v128_store(to,v3); \
	} while(0)

#define newlz_copyadd8(to,fm1,fm2) do { \
		v128_t v1 = wasm_v128_load64_zero(fm1); \
		v128_t v2 = wasm_v128_load64_zero(fm2); \
		v128_t v3 = wasm_i8x16_add(v1,v2); \
		wasm_v128_store64_lane(to,v3,0); \
	} while(0)

#define newlz_copyadd4(to,fm1,fm2) do { \
		v128_t v1 = wasm_i32x4_make(*((const U32 *)(fm1)),0,0,0); \
		v128_t v2 = wasm_i32x4_make(*((const U32 *)(fm2)),0,0,0); \
		v128_t v3 = wasm_i8x16_add(v1,v2); \
		*((U32 *)(to)) = wasm_i32x4_extract_lane(v3,0); \
	} while(0)

#else

// fallback scalar code :

#define newlz_copyadd4(to,fm1,fm2) do { \
	U32 lo7mask = 0x7F7F7F7F; /* low 7 bits of every byte */ \
	U32 a = RR_GET32_NATIVE(fm1); \
	U32 b = RR_GET32_NATIVE(fm2); \
	U32 sum = (a & lo7mask) + (b & lo7mask); /* sum of low 7 bits with carry */ \
	sum ^= (a ^ b) & ~lo7mask; /* carryless sum (XOR) in MSB of bytes */ \
	RR_PUT32_NATIVE(to, sum); \
	} while(0)

#ifdef __RAD64REGS__

#define newlz_copyadd8(to,fm1,fm2) do { \
	U64 lo7mask = 0x7F7F7F7F7F7F7F7Full; /* low 7 bits of every byte */ \
	U64 a = RR_GET64_NATIVE(fm1); \
	U64 b = RR_GET64_NATIVE(fm2); \
	U64 sum = (a & lo7mask) + (b & lo7mask); /* sum of low 7 bits with carry */ \
	sum ^= (a ^ b) & ~lo7mask; /* carryless sum (XOR) in MSB of bytes */ \
	RR_PUT64_NATIVE(to, sum); \
	} while(0)

#else

#define newlz_copyadd8(to,fm1,fm2) do { \
	newlz_copyadd4(to,fm1,fm2); \
	newlz_copyadd4(((U8 *)to)+4,((U8 *)fm1)+4,((U8 *)fm2)+4); \
	} while(0)

#endif
	
#endif	

//=================================================================
// put_sub_literals for encoder side :

static RADINLINE void put_sub_literals8(U8 * to_ptr, const U8 * from_ptr, SINTa lrl, SINTa offset)
{
	RR_ASSERT( lrl <= 8 );
	SINTa sub_literal_offset = - (SINTa)offset;
	
	#if defined(__RADSSE2__)

	__m128i prev = _mm_loadl_epi64((const __m128i *)(from_ptr));
	__m128i delt = _mm_loadl_epi64((const __m128i *)(from_ptr+sub_literal_offset));
	__m128i out = _mm_sub_epi8(prev,delt);
	_mm_storel_epi64((__m128i *)to_ptr,out);		

	#elif defined(__RADNEON__)

	uint8x8_t prev = vld1_u8(from_ptr);
	uint8x8_t delt = vld1_u8(from_ptr+sub_literal_offset);
	uint8x8_t out = vsub_u8(prev,delt);
	vst1_u8(to_ptr,out);
	
	#elif defined(__RADWASM_SIMD128__)

	v128_t prev = wasm_v128_load64_zero(from_ptr);
	v128_t delt = wasm_v128_load64_zero(from_ptr+sub_literal_offset);
	v128_t out = wasm_i8x16_sub(prev,delt);
	wasm_v128_store64_lane(to_ptr,out,0);

	#else
	
	// just unroll 8 ?
	
	RR_UNROLL_8( *to_ptr++ = from_ptr[0] - from_ptr[sub_literal_offset]; from_ptr++ );
	
	#endif
}

// allowed to read up to 7 bytes past the end of the literal run,
// and write up to 7 bytes past to_ptr+lrl
static RADINLINE void put_sub_literals_sloppy(U8 * to_ptr, const U8 * from_ptr, SINTa lrl, SINTa offset)
{
	SINTa sub_literal_offset = - (SINTa)offset;

	#if defined(__RADSSE2__)

	while( lrl >= 16-7 ) // -7 since we're allowed to over-read by up to 7
	{
		__m128i prev = _mm_loadu_si128((const __m128i *)(from_ptr+sub_literal_offset));
		__m128i cur  = _mm_loadu_si128((const __m128i *)(from_ptr));
		__m128i out = _mm_sub_epi8(cur,prev);
		_mm_storeu_si128((__m128i *)to_ptr,out);

		from_ptr += 16;
		to_ptr += 16;
		lrl -= 16;
	}

	// lrl is now in [-7,8]
	if ( lrl > 0 )
	{
		put_sub_literals8(to_ptr,from_ptr,lrl,offset);
	}

	#elif defined(__RADNEON__)

	while( lrl >= 16-7 ) // -7 since we're allowed to over-read by up to 7
	{
		uint8x16_t prev = vld1q_u8(from_ptr+sub_literal_offset);
		uint8x16_t cur = vld1q_u8(from_ptr);
		uint8x16_t out = vsubq_u8(cur,prev);
		vst1q_u8(to_ptr,out);

		from_ptr += 16;
		to_ptr += 16;
		lrl -= 16;
	}

	// lrl is now in [-7,8]
	if ( lrl > 0 )
	{
		put_sub_literals8(to_ptr,from_ptr,lrl,offset);
	}

	#elif defined(__RADWASM_SIMD128__)

	while( lrl >= 16-7 ) // -7 since we're allowed to over-read by up to 7
	{
		v128_t prev = wasm_v128_load(from_ptr+sub_literal_offset);
		v128_t cur  = wasm_v128_load(from_ptr);
		v128_t out = wasm_i8x16_sub(cur,prev);
		wasm_v128_store(to_ptr,out);

		from_ptr += 16;
		to_ptr += 16;
		lrl -= 16;
	}

	// lrl is now in [-7,8]
	if ( lrl > 0 )
	{
		put_sub_literals8(to_ptr,from_ptr,lrl,offset);
	}

	#else

	while(lrl--)
	{
		*to_ptr++ = from_ptr[0] - from_ptr[sub_literal_offset];
		from_ptr++;
	}

	#endif
}

// doesn't over-read; only used near the end of a chunk so not critical
static RADINLINE void put_sub_literals(U8 * to_ptr, const U8 * from_ptr, SINTa lrl, SINTa offset)
{
	SINTa sub_literal_offset = - (SINTa)offset;

	#if defined(__RADSSE2__)
	// meh ; very minor benefit

	// could roll up from_ptr or to_ptr to alignment

	while( lrl >= 16 )
	{
		__m128i prev = _mm_loadu_si128((const __m128i *)(from_ptr+sub_literal_offset));
		__m128i cur  = _mm_loadu_si128((const __m128i *)(from_ptr));
		__m128i out = _mm_sub_epi8(cur,prev);
		_mm_storeu_si128((__m128i *)to_ptr,out);

		from_ptr += 16;
		to_ptr += 16;
		lrl -= 16;
	}

	#elif defined(__RADNEON__)

	while( lrl >= 16 )
	{
		uint8x16_t prev = vld1q_u8(from_ptr+sub_literal_offset);
		uint8x16_t cur = vld1q_u8(from_ptr);
		uint8x16_t out = vsubq_u8(cur,prev);
		vst1q_u8(to_ptr,out);

		from_ptr += 16;
		to_ptr += 16;
		lrl -= 16;
	}

	#elif defined(__RADWASM_SIMD128__)

	while( lrl >= 16 )
	{
		v128_t prev = wasm_v128_load(from_ptr+sub_literal_offset);
		v128_t cur  = wasm_v128_load(from_ptr);
		v128_t out = wasm_i8x16_sub(cur,prev);
		wasm_v128_store(to_ptr,out);

		from_ptr += 16;
		to_ptr += 16;
		lrl -= 16;
	}

	#endif

	while(lrl--)
	{
		*to_ptr++ = from_ptr[0] - from_ptr[sub_literal_offset];
		from_ptr++;
	}
}

//=================================================================

namespace
{

template <int t_literals_type>
RADFORCEINLINE void newlz_literals_copy4(U8 * to_ptr, const U8 * match_ptr, const U8 * literals_ptr)
{
	// I need this to compile but should never get called
	RR_CANT_GET_HERE();
}

template <>
RADFORCEINLINE void newlz_literals_copy4<NEWLZ_LITERALS_TYPE_RAW>(U8 * to_ptr, const U8 * match_ptr, const U8 * literals_ptr)
{
	lz_copy4(to_ptr,literals_ptr);
}

template <>
RADFORCEINLINE void newlz_literals_copy4<NEWLZ_LITERALS_TYPE_SUB>(U8 * to_ptr, const U8 * match_ptr, const U8 * literals_ptr)
{
	newlz_copyadd4(to_ptr,match_ptr,literals_ptr);
}

#ifdef NEWLZ_LITERALS_TYPE_LAMSUB
template <>
RADFORCEINLINE void newlz_literals_copy4<NEWLZ_LITERALS_TYPE_LAMSUB>(U8 * to_ptr, const U8 * match_ptr, const U8 * literals_ptr)
{
	newlz_copyadd4(to_ptr,match_ptr,literals_ptr);
}
#endif


template <int t_literals_type>
RADFORCEINLINE void newlz_literals_copy8(U8 * to_ptr, const U8 * match_ptr, const U8 * literals_ptr)
{
	// I need this to compile but should never get called
	RR_CANT_GET_HERE();
}

template <>
RADFORCEINLINE void newlz_literals_copy8<NEWLZ_LITERALS_TYPE_RAW>(U8 * to_ptr, const U8 * match_ptr, const U8 * literals_ptr)
{
	lz_copy8(to_ptr,literals_ptr);
}

template <>
RADFORCEINLINE void newlz_literals_copy8<NEWLZ_LITERALS_TYPE_SUB>(U8 * to_ptr, const U8 * match_ptr, const U8 * literals_ptr)
{
	newlz_copyadd8(to_ptr,match_ptr,literals_ptr);
}

#ifdef NEWLZ_LITERALS_TYPE_LAMSUB
template <>
RADFORCEINLINE void newlz_literals_copy8<NEWLZ_LITERALS_TYPE_LAMSUB>(U8 * to_ptr, const U8 * match_ptr, const U8 * literals_ptr)
{
	newlz_copyadd8(to_ptr,match_ptr,literals_ptr);
}
#endif

template <int t_literals_type>
RADFORCEINLINE void newlz_literals_copy16(U8 * to_ptr, const U8 * match_ptr, const U8 * literals_ptr)
{
	// I need this to compile but should never get called
	RR_CANT_GET_HERE();
}

template <>
RADFORCEINLINE void newlz_literals_copy16<NEWLZ_LITERALS_TYPE_RAW>(U8 * to_ptr, const U8 * match_ptr, const U8 * literals_ptr)
{
	lz_copy16(to_ptr,literals_ptr);
}

template <>
RADFORCEINLINE void newlz_literals_copy16<NEWLZ_LITERALS_TYPE_SUB>(U8 * to_ptr, const U8 * match_ptr, const U8 * literals_ptr)
{
	#ifdef newlz_copyadd16
	newlz_copyadd16(to_ptr,match_ptr,literals_ptr);
	#else
	newlz_copyadd8(to_ptr,match_ptr,literals_ptr);
	newlz_copyadd8(to_ptr+8,match_ptr+8,literals_ptr+8);
	#endif
}

template <int t_literals_type>
RADFORCEINLINE void newlz_literals_simple(U8 * to_ptr, const U8 * match_ptr, const U8 * literals_ptr,SINTa lrl)
{
	// I need this to compile but should never get called
	RR_CANT_GET_HERE();
}

template <>
RADFORCEINLINE void newlz_literals_simple<NEWLZ_LITERALS_TYPE_RAW>(U8 * to_ptr, const U8 * match_ptr, const U8 * literals_ptr,SINTa lrl)
{
	while( lrl--)
	{
		*to_ptr = *literals_ptr++;
		to_ptr++;
	}
}

template <>
RADFORCEINLINE void newlz_literals_simple<NEWLZ_LITERALS_TYPE_SUB>(U8 * to_ptr, const U8 * match_ptr, const U8 * literals_ptr,SINTa lrl)
{
	while( lrl--)
	{
		*to_ptr = (U8)(*literals_ptr++ + *match_ptr++);			
		to_ptr++;
	}
}

}; // namespace

//=======================================================================

OODLE_NS_END
