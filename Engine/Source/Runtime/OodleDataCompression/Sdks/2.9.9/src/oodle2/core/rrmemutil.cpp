// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrmemutil.h"
#include "rrmath.h"
#include <string.h>
#include "rrmem.h"

#ifdef __RADSSE2__
#include <emmintrin.h> // SSE2
#endif

//=========================

OODLE_NS_START

#ifndef rrMemSetLarge
void rrMemSetLarge(void * ptr, int val, SINTa size)
{
	memset(ptr,val,size);
}

void rrMemCpyLarge(void * RADRESTRICT to,const void * RADRESTRICT from, SINTa size)
{
	memcpy(to,from,size);
}
#endif // rrMemSetLarge


#ifndef rrMemSetZero
// special memset to zero :
void rrMemSetZero(void * base, SINTa size)
{
	memset(base,0,size);
}
#endif // rrMemSetZero

// set a U32 value :
void rrMemSet32_Aligned(void * base, U32 val, SINTa bytes)
{
	RR_ASSERT( rrIsAligned((UINTa)base,sizeof(U32)) );
	RR_DURING_ASSERT( UINTa end = (UINTa)base + bytes );

	U32 * to = (U32 *)base;
	
	SINTa count128 = bytes>>7;
	bytes -= count128<<7;
	while(count128 > 0)
	{
		count128 --;
		// big unroll ?
		RR_UNROLL_8( to[0] = val; to[1] = val; to[2] = val; to[3] = val; to += 4; );
	}
	
	SINTa count16 = bytes>>4;
	bytes -= count16<<4;
	while(count16--)
	{
		to[0] = val;
		to[1] = val;
		to[2] = val;
		to[3] = val;
		to += 4;
	}
	
	SINTa count4 = (bytes>>2);
	while(count4--)
	{
		*to++ = val;
	}
	
	RR_ASSERT( (UINTa)to == end );
}

void rrMemSet64_Aligned(void * base, U64 val, SINTa bytes)
{
	RR_ASSERT( rrIsAligned((UINTa)base,sizeof(U64)) );
	RR_DURING_ASSERT( UINTa end = (UINTa)base + bytes );

	U64 * to = (U64 *)base; // *is* aligned
	
	SINTa count128 = bytes>>7;
	bytes -= count128<<7;
	while(count128 > 0)
	{
		count128 --;
		// big unroll ?
		RR_UNROLL_4( to[0] = val; to[1] = val; to[2] = val; to[3] = val; to += 4; );
	}
	
	SINTa count32 = bytes>>5;
	bytes -= count32<<5;
	while(count32--)
	{
		to[0] = val;
		to[1] = val;
		to[2] = val;
		to[3] = val;
		to += 4;
	}
	
	SINTa count8 = (bytes>>3);
	while(count8--)
	{
		*to++ = val;
	}

	RR_ASSERT( (UINTa)to == end );
}

static rrbool rrIsMemset_Simple(const U8 * rawBuf,SINTa rawLen)
{
	U8 c = rawBuf[0];
	for(SINTa i=1;i<rawLen;i++)
	{
		if ( rawBuf[i] != c ) return false;
	}
	return true;
}

static rrbool rrIsMemset_Fast(const U8 * rawBuf,SINTa rawLen)
{
	if ( rawLen <= 16 )
	{
		return rrIsMemset_Simple(rawBuf,rawLen);
	}
	else
	{
		#ifdef __RADSSE2__
	
		RR_ASSERT( rawLen >= 17 );
	
		// 17 bytes :
		// first 16 vs next 16 :
		// this checks rawBuf[0] == rawBuf[1]
		//	and rawBuf[1] == rawBuf[2]
		//	so they must all be equal, eg. a memset
		__m128i first = _mm_loadu_si128((__m128i *)rawBuf);
		__m128i second = _mm_loadu_si128((__m128i *)(rawBuf+1));
	
		int mask = _mm_movemask_epi8( _mm_cmpeq_epi8(first,second) );
		if ( mask != 0xffff ) return false;
		
		// so first 17 are a memset
		// now check if all other 16's == the first 16
		
		const U8 * ptr = rrAlignUpPointer(rawBuf+2,16);
		const U8 * ptr_end = rawBuf+rawLen;
		
		// no need to do == ptr_end cuz of extra tail test below
		while ( (ptr+16) < ptr_end )
		{
			second = _mm_load_si128((__m128i *)(ptr));
	
			mask = _mm_movemask_epi8( _mm_cmpeq_epi8(first,second) );
			if ( mask != 0xffff ) return false;
		
			ptr += 16;
		}
		
		// do tail :
		second = _mm_loadu_si128((__m128i *)(ptr_end-16));
		mask = _mm_movemask_epi8( _mm_cmpeq_epi8(first,second) );
		if ( mask != 0xffff ) return false;
	
		return true;
	
		#else
	
		const U8 * up = rrAlignUpPointer(rawBuf,8);
		U8 one = up[0];
		while ( rawBuf < up )
		{
			if ( one != rawBuf[0] )
				return false;
			rawBuf++;
			rawLen--;
		}
	
		RR_ASSERT( rrIsAligned( (UINTa)rawBuf , 8 ) );
		
		U64 ff = one * 0x0101010101010101ULL;
		
		const U64 * raw64 = (const U64 *)rawBuf; 
		U64 eight = raw64[0];
		
		if ( eight != ff )
			return false;
			
		SINTa raw8 = rawLen/8;
		for(SINTa i=0;i<raw8;i++)
		{
			if ( raw64[i] != eight )
				return false;
		}
		for(SINTa i=raw8;i<rawLen;i++)
		{
			if ( rawBuf[i] != one )
				return false;
		}

		return true;
		
		#endif
	}
}

rrbool rrIsMemset(const U8 * rawBuf,SINTa rawLen)
{
	rrbool r1 = rrIsMemset_Fast(rawBuf,rawLen);
	//rrbool r2 = rrIsMemset_Simple(rawBuf,rawLen);
	//RR_ASSERT_ALWAYS( r1 == r2 );
	return r1;
}

OODLE_NS_END
