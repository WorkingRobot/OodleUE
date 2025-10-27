// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.


OODLE_NS_START

// Sloppy memset: Is allowed to write up to 16 bytes extra past tail
//
// puts a pattern of bytes/words/dwords
// ptr must be aligned to the size of the pattern unit (eg. aligned by 2 if putting U16's)

// Prototypes:
// static void SloppyMemset_U8(void * ptr, U8 val, SINTa sizeInBytes);
// static void SloppyMemset_U16(void * ptr, U16 val, SINTa sizeInBytes);
// static void SloppyMemset_U32(void * ptr, U32 val, SINTa sizeInBytes);

#if defined(__RADSSE2__)

OODLE_NS_END
#include <emmintrin.h> // SSE2
OODLE_NS_START

#define SLOPPY_MEMSET_VEC

typedef __m128i SloppyVecType;

#define SloppyVec_SplatU8(val)  			_mm_set1_epi8((char)(val))
#define SloppyVec_SplatU16(val) 			_mm_set1_epi16((short)(val))
#define SloppyVec_SplatU32(val) 			_mm_set1_epi32((short)(val))
#define SloppyVec_StoreAligned(ptr,val)		_mm_store_si128((__m128i *)(ptr), (val))
#define SloppyVec_StoreUnaligned(ptr,val)	_mm_storeu_si128((__m128i *)(ptr), (val))

#elif defined(__RADNEON__)

OODLE_NS_END
#include <arm_neon.h>
OODLE_NS_START

#define SLOPPY_MEMSET_VEC

typedef uint8x16_t SloppyVecType;

#define SloppyVec_SplatU8(val)				vdupq_n_u8(val)
#define SloppyVec_SplatU16(val)				vreinterpretq_u8_u16(vdupq_n_u16(val))
#define SloppyVec_SplatU32(val)				vreinterpretq_u8_u32(vdupq_n_u32(val))
#define SloppyVec_StoreAligned(ptr,val)		vst1q_u8((U8 *)(ptr), (val))
#define SloppyVec_StoreUnaligned(ptr,val)	vst1q_u8((U8 *)(ptr), (val))

#else

// no vector?

#endif

#if defined(SLOPPY_MEMSET_VEC)

static RADINLINE void SloppyMemset_Impl(void * ptr, SloppyVecType splat, SINTa size, SINTa unit)
{
	RR_ASSERT( (((UINTa)ptr) & (unit-1)) == 0 ); // ptr must be aligned to unit
	char * end = ((char *)ptr + size );

	// put partial head :
	U8 * p = (U8 *) ptr;
	SloppyVec_StoreUnaligned(p, splat);

	if ( size > 16 )
	{
		// step ahead but slide down to get on alignment
		p = rrAlignDownPointer(p+16,16);
		RR_ASSERT( (char *)p > (char *)ptr );
	
		do
		{
			SloppyVec_StoreAligned(p, splat);
			p += 16;
		}
		while ( (char *)p < end );
	}
}

#define SloppyMemset_U8(ptr, val, size)  SloppyMemset_Impl(ptr, SloppyVec_SplatU8( val), size,1)
#define SloppyMemset_U16(ptr, val, size) SloppyMemset_Impl(ptr, SloppyVec_SplatU16(val), size,2)
#define SloppyMemset_U32(ptr, val, size) SloppyMemset_Impl(ptr, SloppyVec_SplatU32(val), size,4)

#elif defined(__RAD64REGS__)

// Fallback: just use U64 stores
static void SloppyMemset_Impl(void * ptr, U64 splat, SINTa size, SINTa unit)
{
	RR_ASSERT( (((UINTa)ptr) & (unit-1)) == 0 ); // ptr must be aligned to unit
	char * end = ((char *)ptr + size );

	// put partial head :
	U8 * p = (U8 *) ptr;
	RR_PUT64_NATIVE(p, splat);

	if ( size > 8 )
	{
		// step ahead but slide down to get on alignment
		p = rrAlignDownPointer(p+8,8);
		RR_ASSERT( (char *)p > (char *)ptr );
	
		do
		{
			RR_PUT64_NATIVE(p, splat);
			p += 8;
		}
		while ( (char *)p < end );
	}
}

#define SloppyMemset_U8(ptr, val, size)  SloppyMemset_Impl(ptr, ((val) * 0x0101010101010101ull), size,1)
#define SloppyMemset_U16(ptr, val, size) SloppyMemset_Impl(ptr, ((val) * 0x0001000100010001ull), size,2)
#define SloppyMemset_U32(ptr, val, size) SloppyMemset_Impl(ptr, ((val) * 0x0000000100000001ull), size,4)

#else

// Fallback: just use U32 stores
static void SloppyMemset_Impl(void * ptr, U32 splat, SINTa size, SINTa unit)
{
	RR_ASSERT( (((UINTa)ptr) & (unit-1)) == 0 ); // ptr must be aligned to unit
	char * end = ((char *)ptr + size );

	// put partial head :
	U8 * p = (U8 *) ptr;
	RR_PUT32_NATIVE(p, splat);

	if ( size > 4 )
	{
		// step ahead but slide down to get on alignment
		p = rrAlignDownPointer(p+4,4);
		RR_ASSERT( (char *)p > (char *)ptr );
	
		do
		{
			RR_PUT32_NATIVE(p, splat);
			p += 4;
		}
		while ( (char *)p < end );
	}
}

#define SloppyMemset_U8(ptr, val, size)  SloppyMemset_Impl(ptr, ((val) * 0x01010101u), size, 1)
#define SloppyMemset_U16(ptr, val, size) SloppyMemset_Impl(ptr, ((val) * 0x00010001u), size, 2)
#define SloppyMemset_U32(ptr, val, size) SloppyMemset_Impl(ptr, val, size, 4)

#endif // SLOPPY_MEMSET_VEC
		
OODLE_NS_END

