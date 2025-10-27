// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef RRLZ_GETMATCHLEN_INL
#define RRLZ_GETMATCHLEN_INL

#include "oodlecore.h"
#include "rrmath.h"
#include "rrbits.h"

OODLE_NS_START

//============================================= 
/*

OODLELZ_PLATFORM_LARGE_WORD_STRINGMATCH tells whether the platform is faster
doing U32/U64 string match steps, or byte-by-byte

generally it corresponds to whether unaligned accesses are fast

*/

// On everything we currently ship on, unaligned accesses are OK
// and word-sized reads are faster
#define OODLELZ_PLATFORM_LARGE_WORD_STRINGMATCH	1

//=============================================    

#ifdef __RAD64REGS__
#define rrClzR rrClz64
#define rrCtzR rrCtz64
#define rrClzBytesR rrClzBytes64
#define rrCtzBytesR rrCtzBytes64
#else
#define rrClzR rrClz32
#define rrCtzR rrCtz32
#define rrClzBytesR rrClzBytes32
#define rrCtzBytesR rrCtzBytes32
#endif

//=============================================    

#if defined(__RADBIGENDIAN__)

// big endian
// so earlier bytes are at the top
// can use CLZ from rrMath

static int RADFORCEINLINE GetNumBytesZeroNeverAllR(UINTr x)
{
	RR_ASSERT( x != 0 );
	
	U32 nb = rrClzBytesR(x);
	
	RR_ASSERT( nb >= 0 && nb < (int)sizeof(UINTr) );
	return nb;
}
static int RADFORCEINLINE GetNumBytesZeroNeverAll32(U32 x)
{
	RR_ASSERT( x != 0 );
	
	U32 nb = rrClzBytes32(x);
	
	RR_ASSERT( nb >= 0 && nb < (int)sizeof(U32) );
	return nb;
}
#ifdef __RAD64REGS__
static int RADFORCEINLINE GetNumBytesZeroNeverAll64(U64 x)
{
	RR_ASSERT( x != 0 );
	
	U32 nb = rrClzBytes64(x);
	
	RR_ASSERT( nb >= 0 && nb < (int)sizeof(U64) );
	return nb;
}
#endif

#elif defined(__RADLITTLEENDIAN__)

static int RADFORCEINLINE GetNumBytesZeroNeverAllR(UINTr x)
{
	RR_ASSERT( x != 0 );
	
	U32 nb = rrCtzBytesR(x);
	
	RR_ASSERT( nb >= 0 && nb < (int)sizeof(UINTr) );
	return nb;
}
static int RADFORCEINLINE GetNumBytesZeroNeverAll32(U32 x)
{
	RR_ASSERT( x != 0 );
	
	U32 nb = rrCtzBytes32(x);
	
	RR_ASSERT( nb >= 0 && nb < (int)sizeof(U32) );
	return nb;
}
#ifdef __RAD64REGS__
static int RADFORCEINLINE GetNumBytesZeroNeverAll64(U64 x)
{
	RR_ASSERT( x != 0 );
	
	U32 nb = rrCtzBytes64(x);
	
	RR_ASSERT( nb >= 0 && nb < (int)sizeof(U64) );
	return nb;
}
#endif

#else // ENDIAN
	#error wtf no endian set
#endif // ENDIAN

//=============================================    

/*

getmatchlen end bounds checking note :

 only p1 is checked against ptr1
	in normal usage p1 always > p2
	(p1 should be cur pointer and p2 is the match-against,
	 which is before you)
	 
 but I abuse things a bit and use these funcs in other places than the main LZ coders
	so the p1 > p2 assert is disabled
	
*/
    
static RADINLINE int getmatchlen_debug(const U8 * p1,const U8 *p2,const U8 * ptrend)
{
	//RR_ASSERT( p1 > p2 );
	RR_ASSERT( p1 <= ptrend );
	RR_ASSERT( rrPtrDiff(ptrend - p1) < (RR_S32_MAX-16) );
	
    int len = 0;
    while( p1+len < ptrend && p1[len] == p2[len] )
    {
        len++;
    }
    return len;
}
		
static int RADINLINE getmatchlen_mml1(const U8 * p1,const U8 *p2,const U8 * ptrend)
{
	// only p1 is checked against ptrend
	//	normally that's because p2 is the "vs" that's *before* p1
	//	but in some places I abuse getmatchlen, so you can't do this assert
	//RR_ASSERT( p1 > p2 );
	RR_ASSERT( p1 <= ptrend );
	RR_ASSERT( rrPtrDiff(ptrend - p1) < (RR_S32_MAX-16) );

	#if OODLELZ_PLATFORM_LARGE_WORD_STRINGMATCH

    const U8 * ptr_big = p1;
    const U8 * ptr_big_end = ptrend-sizeof(UINTr);
    SINTa p2off = p2 - p1;
    //RR_ASSERT( p2off < 0 );
    
    while ( (UINTa)ptr_big <= (UINTa)ptr_big_end )
	{
		UINTr big1 = *((UINTr *)(ptr_big));
		UINTr big2 = *((UINTr *)(ptr_big+p2off));
		UINTr diff = big1 ^ big2;
    
		if ( diff == 0 )
		{
			ptr_big += sizeof(UINTr);
			continue;
		}
		else
		{
			return GetNumBytesZeroNeverAllR(diff) + rrPtrDiff32(ptr_big - p1);
		}
    }
    
	{
		// close to the end; just step bytes to finish 
		int len = (int)(ptr_big - p1);
	    
		while( p1+len < ptrend && p1[len] == p2[len] )
		{
			len++;
		}
		
		return len;
	}
		
		
    #else // not OODLELZ_PLATFORM_LARGE_WORD_STRINGMATCH
    
    // match byte by byte
	
	int len = 0;
	int maxlen = rrPtrDiff32(ptrend - p1);
	
    // main match check loop - not fast -
    while( len < maxlen && p1[len] == p2[len] )
    {
        len++;
    }
    
    return len;

    #endif // OODLELZ_PLATFORM_LARGE_WORD_STRINGMATCH
    
}

static int RADINLINE getmatchlen_mml2(const U8 * p1,const U8 *p2,const U8 * ptrend)
{
	// @@ lame for now
	int ret = getmatchlen_mml1(p1,p2,ptrend);
	if ( ret == 1 ) return 0;
	else return ret;
}

static int RADFORCEINLINE getmatchlen_after4(const U8 * p1,const U8 *p2,const U8 * ptrend)
{
	// already match 4 , now see how much more we match
	//	-> in this case it's more likely that we will match big chunks
	//	so use a large-word matcher here
	//	could perhaps even afford to roll up to alignment
	RR_ASSERT( p1+4 <= ptrend );
	RR_ASSERT( RR_GET32_NATIVE_UNALIGNED(p1) == RR_GET32_NATIVE_UNALIGNED(p2) );

	return 4 + getmatchlen_mml1(p1+4,p2+4,ptrend);
}

static int RADFORCEINLINE getmatchlen_mml8(const U8 * p1,const U8 *p2,const U8 * ptrend)
{
	RR_ASSERT( p1+8 <= ptrend );
	
	#ifdef __RAD64REGS__
	
	U64 p1_64 = *((const U64 *)p1);
	U64 p2_64 = *((const U64 *)p2);
	if ( p1_64 != p2_64 )
		return 0;
	
	#else
	
	U32 p1_32 = RR_GET32_NATIVE_UNALIGNED(p1);
	U32 p2_32 = RR_GET32_NATIVE_UNALIGNED(p2);
	if ( p1_32 != p2_32 )
		return 0;

	p1_32 = RR_GET32_NATIVE_UNALIGNED((p1+4));
	p2_32 = RR_GET32_NATIVE_UNALIGNED((p2+4));
	if ( p1_32 != p2_32 )
		return 0;
		
	#endif

	return 8 + getmatchlen_mml1(p1+8,p2+8,ptrend);
}

static int RADFORCEINLINE getmatchlen_mml4(const U8 * p1,const U8 *p2,const U8 * ptrend)
{
	RR_ASSERT( p1+4 <= ptrend );
	U32 p1_32 = RR_GET32_NATIVE_UNALIGNED(p1);
	U32 p2_32 = RR_GET32_NATIVE_UNALIGNED(p2);
	if ( p1_32 != p2_32 )
		return 0;

	return getmatchlen_after4(p1,p2,ptrend);
}

static int RADFORCEINLINE getmatchlen_mml4_two32(U32 p1_32,U32 p2_32,const U8 * p1,const U8 *p2,const U8 * ptrend)
{
    //RR_ASSERT( p1 > p2 );
    
    if ( p1_32 != p2_32 ) // @@ better to check this before the xor or after?
    {
		return 0;
	}
	
	return getmatchlen_after4(p1,p2,ptrend);
}

static RADFORCEINLINE int getmatchlen_mml3_two32(U32 p1_32,U32 p2_32,const U8 * p1,const U8 *p2,const U8 * ptrend)
{
    //RR_ASSERT( p1 > p2 );
	RR_ASSERT( (p1+4) <= ptrend );
	
	U32 diff = p1_32^p2_32;
		
    if ( diff )
    {
		#ifdef __RADBIGENDIAN__
		return ( diff > 0x000000FF ) ? 0 : 3; // difference in the first three bytes
		#else
		return ( diff & 0x00FFFFFF ) ? 0 : 3; // difference in the first three bytes
		#endif
	}
	
	// ML = 4 + possibly more
	
	return getmatchlen_after4(p1,p2,ptrend);
}

// @@ : getmatchlen is used a lot, could be faster !!
// NOTE : p1 is checked against ptrend ; p2 is NOT
//	that is ,p1 should the current pointer, p2 the match-vs
static RADFORCEINLINE int getmatchlen_mml3_one32(U32 p1_32,const U8 * p1,const U8 *p2,const U8 * ptrend)
{
    //RR_ASSERT( p1 > p2 ); // p1 > p2 so I only have to check p1 for hitting the end
	
	U32 p2_32 = RR_GET32_NATIVE_UNALIGNED(p2);

	return getmatchlen_mml3_two32(p1_32,p2_32,p1,p2,ptrend);
}

static RADFORCEINLINE int getmatchlen_mml4_one32(U32 p1_32,const U8 * p1,const U8 *p2,const U8 * ptrend)
{
    //RR_ASSERT( p1 > p2 ); // p1 > p2 so I only have to check p1 for hitting the end
	
	U32 p2_32 = RR_GET32_NATIVE_UNALIGNED(p2);

	return getmatchlen_mml4_two32(p1_32,p2_32,p1,p2,ptrend);
}

static RADFORCEINLINE int getmatchlen_mml3_no32(const U8 * p1,const U8 *p2,const U8 * ptrend)
{
	U32 p1_32 = RR_GET32_NATIVE_UNALIGNED(p1);
	return getmatchlen_mml3_one32(p1_32,p1,p2,ptrend);
}

static RADFORCEINLINE int getmatchlen_mml2_two32(U32 p1_32,U32 p2_32,const U8 * p1,const U8 *p2,const U8 * ptrend)
{
	U32 diff = p1_32^p2_32;

	if ( diff )
	{
		// alternative :
		// GetNumBytesZeroNeverAll32
		//	but then squash 1 to 0

		#ifdef __RADBIGENDIAN__

		if ( diff > 0x0000FFFF ) return 0; // difference in the first two bytes
		if ( diff > 0x000000FF ) return 2;
		else return 3;

		#else

		if ( diff & 0x0000FFFF ) return 0; // difference in the first two bytes
		if ( diff & 0x00FFFFFF ) return 2;
		else return 3;

		#endif
	}
	
	return getmatchlen_after4(p1,p2,ptrend);
}

static RADFORCEINLINE int getmatchlen_mml2_one32(U32 p1_32,const U8 * p1,const U8 *p2,const U8 * ptrend)
{
    RR_ASSERT( p1 > p2 );
    // this assert is no good because ptrend is not actually the end of the buffer
    //	it's the end of the buffer minus the decoder trash amount
	//RR_ASSERT( (p1+4) <= ptrend );
	
	U32 p2_32 = RR_GET32_NATIVE_UNALIGNED(p2);
	
	return getmatchlen_mml2_two32(p1_32,p2_32,p1,p2,ptrend);
}

// getmatchlen_mml3_one32_better : finds a match len only if its better than previous
static RADFORCEINLINE int getmatchlen_mml3_one32_better(U32 p1_32,const U8 * p1,const U8 *p2,const U8 * ptrend,int prevml)
{
	// yes this helps
	if ( prevml >= 3 )
	{
		// check match of 4 :
		U32 p2_32 = RR_GET32_NATIVE_UNALIGNED(p2);
		if ( p2_32 != p1_32 ) return 0;
		// ml >= 4
		// then check if it can beat the previous :
		if ( prevml >= rrPtrDiff(ptrend-p1) ) return 0;
		if ( p1[prevml] != p2[prevml] ) return 0; // can't beat old
				
		int len = getmatchlen_after4(p1,p2,ptrend);
		
		// @@ ??
		if ( len <= prevml ) return 0;

		return len;
	}
	else
	{
		return getmatchlen_mml3_one32(p1_32,p1,p2,ptrend);
	}
}


// getmatchlen_mml3_one32_better : finds a match len only if its better than previous
static RADFORCEINLINE int getmatchlen_mml4_one32_better(U32 p1_32,const U8 * p1,const U8 *p2,const U8 * ptrend,int prevml)
{
	U32 p2_32 = RR_GET32_NATIVE_UNALIGNED(p2);
	
	// check match of 4 :
	if ( p2_32 != p1_32 ) return 0;
			
	// yes this helps
	if ( prevml >= 4 )
	{
		// ml >= 4
		// then check if it can beat the previous :
		if ( prevml >= rrPtrDiff(ptrend-p1) ) return 0;
		if ( p1[prevml] != p2[prevml] ) return 0; // can't beat old
	}
				
	int len = getmatchlen_after4(p1,p2,ptrend);
	
	// @@ ??
	if ( len <= prevml ) return 0;
	
	return len;
}

OODLE_NS_END

#endif // RRLZ_GETMATCHLEN_INL
