// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetsse4

#include "oodlebase.h"
#include "cbradutil.h"
#include <stdlib.h>

#include "oodlelzcompressors.h"
#include "newlzf.h"

#include "newlz_simd.h"

#include "rrbits.h"
#include "rrprefetch.h"
#include "rrlzh_lzhlw_shared.h"
#include "newlz_subliterals.h"
//#include "newlz_vtable.h"
//#include "newlz_speedfit.h"
#include "newlzf_decoder.h"

//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"

#define CHECK(x)

OODLE_NS_START

//==================================================================================================
#ifdef DO_BUILD_SSE4

OODLE_NS_END
#include <smmintrin.h>
OODLE_NS_START

#ifdef __GNUC__
#if ! defined(__SSE4_1__) 
#error need SSE4 set in compiler
#endif
#endif

/*
#ifdef _MSC_VER
#pragma RR_PRAGMA_MESSAGE("newlzf_sse4 enabled")
#endif
*/

#if defined(__RADJAGUAR__) || defined(__RADZEN2__)
OODLE_NS_END
#include <immintrin.h> // for __bextr_u64 intrinsic
OODLE_NS_START
#endif

// This code wants to use BEXTR on Jaguars; on the Zen 2's it's not necessary but doesn't hurt
#if defined(__RADJAGUAR__) || defined(__RADZEN2__)
#define NEWLZF_BEXTR32(x,start,len) _bextr_u32((x),(start),(len))
#define NEWLZF_BEXTR64(x,start,len) _bextr_u64((x),(start),(len))
#else
#define NEWLZF_BEXTR32(x,start,len) (((x) >> (start)) & ((1u << (len)) - 1))
#define NEWLZF_BEXTR64(x,start,len) (((x) >> (start)) & ((1ull << (len)) - 1))
#endif

/***

newlzf sse4 decoder

we take four packets at a time
the top bit of each packet is rep/not (1 for rep,0 for normal)
gather those top bits together using movemask

now we have a 4 bit mask that tells us :

read a 2 byte offset for each bit on in the mask (c_comp_advance[] table)

the bit mask tells us how to propagate through the next U16 offsets and the prev offset

we load a vector with 4 U16 offsets in the low 64 bits, and the prev offset in the top 32
(prev can be > U16)
we look up the 4 bit mask and it tells you how to propagate prev & where to insert new offsets.

eg. if the 4 bits are :

1001

and the next 4 U16 offsets are { ONE,TWO,THREE,FOUR }

that means you want the offsets to be :

{ PREV , ONE , TWO, TWO }

and advance comp by += 4

after each step, the last offset of the round is in slot [3]
which is where you want it to be for the prev offset of the next round

***/

static const int c_comp_advance[16] = { 8 ,6 ,6 ,4 ,6 ,4 ,4 ,2 ,6 ,4 ,4 ,2 ,4 ,2 ,2 ,0 };	
   	
#define Z		-1
#define PREV	12,13,14,15
#define ONE		0,1,Z,Z
#define TWO		2,3,Z,Z
#define THREE	4,5,Z,Z
#define FOUR	6,7,Z,Z
static RAD_ALIGN(const S8, c_offset_shuffles[16][16], 16) =
{
	{ ONE,  TWO,  THREE,FOUR  },
	{ PREV, ONE,  TWO,  THREE },
	{ ONE,  ONE,  TWO,  THREE },
	{ PREV, PREV, ONE,  TWO   },

	{ ONE,  TWO,  TWO,  THREE },
	{ PREV, ONE,  ONE,  TWO   },
	{ ONE,  ONE,  ONE,  TWO   },
	{ PREV, PREV, PREV, ONE   },

	{ ONE,  TWO,  THREE, THREE },
	{ PREV, ONE,  TWO,   TWO   },
	{ ONE,  ONE,  TWO,   TWO   },
	{ PREV, PREV, ONE,   ONE   },

	{ ONE,  TWO,  TWO,   TWO   },
	{ PREV, ONE,  ONE,   ONE   },
	{ ONE,  ONE,  ONE,   ONE   },
	{ PREV, PREV, PREV,  PREV  },
};
#undef Z
#undef PREV
#undef ONE
#undef TWO
#undef THREE
#undef FOUR

#define NEWLZF_FOUR_SIMPLE_PACKETS_SSE4() do { \
	U32 four_packets = *((const U32 *)(packets_ptr)); \
	packets_ptr += 4; \
	__m128i packets = _mm_cvtsi32_si128(four_packets); \
	int offset_masks = _mm_movemask_epi8(packets); \
	__m128i offsets = _mm_loadl_epi64((const __m128i *)(off16_ptr)); \
	offsets = _mm_insert_epi32(offsets,-neg_offset,3); \
	offsets = _mm_shuffle_epi8(offsets,_mm_load_si128((const __m128i *)c_offset_shuffles[offset_masks])); \
	offsets = _mm_sub_epi32(_mm_setzero_si128(),offsets); \
	RAD_ALIGN(U32,offsets_u32,16) [4]; \
	_mm_store_si128((__m128i *)offsets_u32,offsets); \
	off16_ptr += c_comp_advance[offset_masks]; \
	RR_UNROLL_I_4(0, \
		newlz_literals_copy8<NEWLZF_DECODE_LITERALS_TYPE>(to_ptr,to_ptr+neg_offset,literals_ptr); \
		UINTa lrl = four_packets & 7; \
		UINTa ml = NEWLZF_BEXTR32(four_packets, 3, 4); \
		four_packets = NEWLZF_BEXTR32(four_packets, 8, 24); \
		to_ptr += lrl; literals_ptr += lrl; \
		neg_offset = offsets_u32[i]; \
		const U8 * mp = to_ptr + neg_offset; \
		NEWLZF_FIRST_CHUNK_FUZZ( mp >= window_base ); \
		newlzf_copy16(to_ptr,mp); \
		to_ptr += ml; \
	); \
} while(0)


#if defined(__RAD64REGS__)

// offset_masks bit == 0 means get an offset
// offset_masks bit == 1 means LO
	
// do offsets in fours :
// previous (positive) is carried through in offsets[3]
// store negative offsets to offsets_u32

//_mm_set1_epi32(-neg_offset); // just need it in top entry
	
#define NEWLZF_SIXTEEN_SIMPLE_PACKETS_SSE4() do { \
	__m128i packets = _mm_loadu_si128((const __m128i *)packets_ptr); \
	U32 offset_masks16 = _mm_movemask_epi8(packets); \
	__m128i four_offsets[4]; \
	__m128i offsets = _mm_set1_epi32(-neg_offset); \
	RR_UNROLL_I_4(0, \
		U32 offset_masks = (offset_masks16 >> (i*4)) & 0xF; \
		/* loadl_pi: load bottom half (top half is preserved) */ \
		offsets = _mm_castps_si128(_mm_loadl_pi(_mm_castsi128_ps(offsets), (__m64 *)(off16_ptr))); \
		off16_ptr += c_comp_advance[offset_masks]; \
		offsets = _mm_shuffle_epi8(offsets,_mm_load_si128((const __m128i *)c_offset_shuffles[offset_masks])); \
		four_offsets[i] = _mm_sub_epi32(_mm_setzero_si128(),offsets); \
	); \
	U64 packets64; S64 two_offsets; \
	RR_UNROLL_I_16(0, \
		if ((i & 7) == 0) packets64 = RR_GET64_LE(packets_ptr + i); \
		if ((i & 1) == 0) two_offsets = _mm_extract_epi64(four_offsets[i>>2], (i>>1)&1); \
		newlz_literals_copy8<NEWLZF_DECODE_LITERALS_TYPE>(to_ptr,to_ptr+neg_offset,literals_ptr); \
		UINTa ml = NEWLZF_BEXTR64(packets64, 3, 4); \
		UINTa lrl = packets64 & 7; \
		packets64 = NEWLZF_BEXTR64(packets64, 8, 56); \
		to_ptr += lrl; literals_ptr += lrl; \
		neg_offset = (i&1) ? (S32) (two_offsets >> 32) : (S32) ((U32) two_offsets); \
		const U8 * mp = to_ptr + neg_offset; \
		NEWLZF_FIRST_CHUNK_FUZZ( mp >= window_base ); \
		newlzf_copy16(to_ptr,mp); \
		to_ptr += ml; \
	); \
	packets_ptr += 16; \
} while(0)
	
#else

// 32-bit

#define NEWLZF_SIXTEEN_SIMPLE_PACKETS_SSE4() do { \
	NEWLZF_FOUR_SIMPLE_PACKETS_SSE4(); \
	NEWLZF_FOUR_SIMPLE_PACKETS_SSE4(); \
	NEWLZF_FOUR_SIMPLE_PACKETS_SSE4(); \
	NEWLZF_FOUR_SIMPLE_PACKETS_SSE4(); } while(0)

#endif

// end of SSE4 code


#define NEWLZF_SIXTEEN_SIMPLE_PACKETS	NEWLZF_SIXTEEN_SIMPLE_PACKETS_SSE4
#define NEWLZF_FOUR_SIMPLE_PACKETS		NEWLZF_FOUR_SIMPLE_PACKETS_SSE4

//=======================================================================

#define NEWLZF_DECODE_FIRST_CHUNK 0
#define NEWLZF_FIRST_CHUNK_FUZZ(expr)

#define NEWLZF_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_SUB
#define newLZF_decode_parse newLZF_decode_parse_sub_sse4

#include "newlzf_decode_parse_outer.inl"
		
#undef NEWLZF_DECODE_LITERALS_TYPE
#undef newLZF_decode_parse

#define NEWLZF_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_RAW
#define newLZF_decode_parse newLZF_decode_parse_raw_sse4

#include "newlzf_decode_parse_outer.inl"
		
#undef NEWLZF_DECODE_LITERALS_TYPE
#undef newLZF_decode_parse

#undef NEWLZF_DECODE_FIRST_CHUNK
#undef NEWLZF_FIRST_CHUNK_FUZZ
#define NEWLZF_DECODE_FIRST_CHUNK 1
#define NEWLZF_FIRST_CHUNK_FUZZ(expr)	REQUIRE_FUZZ_RETURN( expr , NULL )

#define NEWLZF_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_SUB
#define newLZF_decode_parse newLZF_decode_parse_sub_sse4_first

#include "newlzf_decode_parse_outer.inl"
		
#undef NEWLZF_DECODE_LITERALS_TYPE
#undef newLZF_decode_parse

#define NEWLZF_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_RAW
#define newLZF_decode_parse newLZF_decode_parse_raw_sse4_first

#include "newlzf_decode_parse_outer.inl"
		
#undef NEWLZF_DECODE_LITERALS_TYPE
#undef newLZF_decode_parse

#undef NEWLZF_FIRST_CHUNK_FUZZ
#undef NEWLZF_DECODE_FIRST_CHUNK

//=======================================================================

/**

newlzf_unpack_escape_offsets_sse4

use SIMD shuffle to unpack 24 bits -> 32
ABCABC -> 0ABC0ABC..

scalar :

Mermaid, Jaguar clang-3.5.0, webster.mermaid.ooz
webster : 41,458,703 ->10,352,625 =  1.998 bpb =  4.005 to 1
decode           : 97.914 millis, 3.76 c/b, rate= 423.42 mb/s
CSV:webster,Mermaid, 41458703, 10352625, 97914000, Jaguar clang-3.5.0

sse4 :

Mermaid, Jaguar clang-3.5.0, webster.mermaid.ooz
webster : 41,458,703 ->10,352,625 =  1.998 bpb =  4.005 to 1
decode           : 96.032 millis, 3.69 c/b, rate= 431.72 mb/s
CSV:webster,Mermaid, 41458703, 10352625, 96032000, Jaguar clang-3.5.0

**/

SINTa newlzf_unpack_escape_offsets_sse4(const U8 * comp_base, const U8 * comp_end,
	U32 * offsets, SINTa offset_count , SINTa max_offset)
{
	if ( offset_count == 0 ) return 0;
	
	// offset == max_offset is ok, > max_offset is a fail
	// off24 >= NEWLZF_OFFSET_FOURBYTE_THRESHOLD means get another byte
	
	const U8 * comp_ptr = comp_base;
		
	// can't over-read comp
	// can over-write offsets (in scratch)
	
	
	S32 threshold = NEWLZF_OFFSET_FOURBYTE_THRESHOLD-1;
	// be careful about this in case max_offset is bigger than S32
	if ( max_offset < (SINTa)threshold )
		threshold = (S32)max_offset;
	__m128i thresholdv = _mm_set1_epi32(threshold);
	
	int i=0;
	for(;i<=offset_count-4;)
	{
		// check I can read 16 bytes :
		if ( comp_ptr+16 > comp_end ) break;
	
		// I want 12 bytes for 4 24-bit offsets :
		__m128i x = _mm_loadu_si128((__m128i *)comp_ptr);
		
		// unpack 24 -> 32 bit
		__m128i y = _mm_shuffle_epi8(x,
            _mm_set_epi8(-1, 11, 10, 9, -1, 8, 7, 6, -1, 5, 4, 3, -1, 2, 1, 0));
        
        // store the 32-bit offsets :
        _mm_storeu_si128((__m128i *)(offsets+i),y);
        
        // see if any are >= NEWLZF_OFFSET_FOURBYTE_THRESHOLD
        // for fuzz, compare to the lower of threshold or the 
        //		position in the buffer
        __m128i cmp = _mm_cmpgt_epi32(y, thresholdv );
		int cmp_mask = _mm_movemask_epi8(cmp);
		if ( cmp_mask )
		{
			int n = rrCtz32(cmp_mask);
			// n is a multiple of 4 :
			RR_ASSERT( (n&3) == 0 );
			n >>= 2;
			RR_ASSERT( n >= 0 && n < 4 ); // never all 4
			
			// did n normal 24-bit offsets :
			i += n;
			comp_ptr += 3*n;
			
			// do another with excess :
			
			U32 off24 = RR_GET24_LE_OVERRUNOK(comp_ptr);
			RR_ASSERT( offsets[i] == off24 ); // already there
			comp_ptr += 3;
			
			// off24 is either a signal to 4-byte fetch , OR a fuzz failure
			//	if it's a fuzz failure it will be caught below in REQUIRE_FUZZ_RETURN
			RR_ASSERT_IF_NOT_CORRUPT( off24 >= NEWLZF_OFFSET_FOURBYTE_THRESHOLD );
			U32 hi = *comp_ptr++;
			off24 += (hi << NEWLZF_OFFSET_FOURBYTE_SHIFT);
			REQUIRE_FUZZ_RETURN( ((SINTa)off24 <= max_offset), -1 );
			// now we know it must be a valid 4-byte fetch case
			RR_ASSERT( off24 >= NEWLZF_OFFSET_FOURBYTE_THRESHOLD );
			offsets[i] = off24;
			i ++;
		}
		else
		{
			// did 4 normal 24-bit offsets
			comp_ptr += 12;
			i += 4;
		}
	}
	
	// finish up :
	for(;i<offset_count;i++)
	{
		REQUIRE_FUZZ_RETURN( comp_ptr+3 <= comp_end , -1 );
		U32 off24 = RR_GET24_LE_NOOVERRUN(comp_ptr);
		comp_ptr += 3;
		if ( off24 >= NEWLZF_OFFSET_FOURBYTE_THRESHOLD )
		{
			REQUIRE_FUZZ_RETURN( comp_ptr < comp_end , -1 );
			U32 hi = *comp_ptr++;
			off24 += (hi << NEWLZF_OFFSET_FOURBYTE_SHIFT);
		}
		REQUIRE_FUZZ_RETURN( ((SINTa)off24 <= max_offset), -1 );
		offsets[i] = off24;
	}
	
	return rrPtrDiff( comp_ptr - comp_base );
}

//==================================================================================================
#endif // DO_BUILD_SSE4


OODLE_NS_END
