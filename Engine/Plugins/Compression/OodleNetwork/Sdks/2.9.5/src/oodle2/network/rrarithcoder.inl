// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RR_ARITHCODERINL__
#define __RR_ARITHCODERINL__


#include "rrarithcoder.h"
#include "rrmath.h"
#include "rrmem.h"
#include "log2table.h"

OODLE_NS_START

#define RR_ARITHCODER_MINRANGE  (0x01000000UL)  // 1<<24

static RADFORCEINLINE void rrArithEncodePropagateCarry( rrArithEncoder* ac )
{
    // go backwards and apply the carry :
    
    // same speed ?
    
    //*
    
    U8 * ptr = ac->ptr;
    while ( *(--ptr) == 0xFF )
    {
        *ptr = 0; // *ptr += 1;
    }
    RR_ASSERT( ptr >= ac->start );
    ptr[0] ++;
    
    /*/

    U8 * p;            // carry propagation on compressed data buffer
    for (p = ac->ptr - 1; *p == 0xFFU; p--) *p = 0;
    ++*p;
    /**/
}

// the convention here is that the Renorm is done *after* each coding operation
//
static RADFORCEINLINE void rrArithEncodeRenorm( rrArithEncoder* ac )
{
    //*
    
    while( ac->range < RR_ARITHCODER_MINRANGE )
    {
        *ac->ptr++ = (U8) (ac->low>>24);
        ac->low <<= 8;
        ac->range <<= 8;
    }
    
    /*/
    
    // same speed ?
    
    if ( ac->range < RR_ARITHCODER_MINRANGE )
    {
		if ( ac->range < (1<<16) )
		{
			RR_PUT32_BE_UNALIGNED(ac->ptr,ac->low);
			
			if ( ac->range < (1<<8) )
			{
				ac->ptr += 3;
				ac->low <<= 24;
				ac->range <<= 24;
			}
			else
			{
				ac->ptr += 2;
				ac->low <<= 16;
				ac->range <<= 16;
			}
		}
		else
		{
			*ac->ptr++ = (U8) (ac->low>>24);
			ac->low <<= 8;
			ac->range <<= 8;			
		}
		RR_ASSERT( ac->range >= RR_ARITHCODER_MINRANGE );
    }
    
    /**/
}

/*
// Branchless renorm - nope, slower
static RADFORCEINLINE void rrArithEncodeRenormBranchless( rrArithEncoder* ac )
{
	// this if is optional but does help speed
    if ( ac->range < RR_ARITHCODER_MINRANGE )
	{
		RR_ASSERT( ac->range != 0 );
		U32 lz = rrClz32(ac->range);
		U32 lzbytes = lz>>3;
		RR_PUT32_BE(ac->ptr,ac->low);
		ac->ptr += lzbytes;
		lz = lzbytes<<3;
		ac->low <<= lz;
		ac->range <<= lz;
		RR_ASSERT( ac->range >= RR_ARITHCODER_MINRANGE );
	}
}
*/

//*
static RADFORCEINLINE void rrArithDecodeRenorm( rrArithDecoder* ac )
{
    while( ac->range < RR_ARITHCODER_MINRANGE )
    {
        ac->code <<= 8;
        ac->range <<= 8;
        ac->code |= *ac->ptr++;
    }
}
/**/

/*
static RADFORCEINLINE void rrArithDecodeRenorm( rrArithDecoder* ac )
{
    if ( ac->range < RR_ARITHCODER_MINRANGE )
    {
        ac->code <<= 8;
        ac->range <<= 8;
        ac->code |= *ac->ptr++;
		while( ac->range < RR_ARITHCODER_MINRANGE )
		{
			ac->code <<= 8;
			ac->range <<= 8;
			ac->code |= *ac->ptr++;
		}
    }
}
/**/

//=====================================================================================================================

RADFORCEINLINE void rrArithEncodeRange( rrArithEncoder* ac , U32 cumfreqLow, U32 freq, U32 cumfreqTot )
{
    RR_ASSERT( cumfreqLow+freq <= cumfreqTot );
    RR_ASSERT( cumfreqTot <= RR_ARITHCODER_CUMPROBMAX );
    RR_ASSERT( ac->range >= RR_ARITHCODER_MINRANGE );

	// CB note : I don't do the top of range waste check
	//	I find in practice people don't bother to put the MPS at the top so it doesn't help compression
	//	and it costs speed

    // three ways to detect carry :

    /*
    ac->range /= cumfreqTot;
    U32 code = ac->low + cumfreqLow * ac->range;
    ac->range *= freq;

    if ( code < ac->low ) rrArithEncodePropagateCarry(ac);

    ac->low = code;
    /**/

    /*
    ac->range /= cumfreqTot;
    U32 add = cumfreqLow * ac->range;
    ac->low += add;
    ac->range *= freq;

    if ( ac->low < add ) rrArithEncodePropagateCarry(ac);
    /**/
    
    // this way seems to win by 1 clock :
    
    //*
    U32 save_low = ac->low;
    ac->range /= cumfreqTot;
    ac->low += cumfreqLow * ac->range;
    ac->range *= freq;

    if ( ac->low < save_low ) rrArithEncodePropagateCarry(ac);
    /**/
    
    rrArithEncodeRenorm(ac);
    
}

// Peek is mutating ; call it only once, always call Peek/Remove in a pair
RADFORCEINLINE U32 rrArithDecodePeek( rrArithDecoder* ac , U32 cumfreqTot )
{
    RR_ASSERT( cumfreqTot <= RR_ARITHCODER_CUMPROBMAX );
    RR_ASSERT( ac->range >= RR_ARITHCODER_MINRANGE );
    
    ac->range /= cumfreqTot;
    
    U32 cumfreqTarget = ac->code / ac->range;
    
    // @@ CB - this if sucks
    //	note that callers will often be able to handle Target out of bounds
    //	and correctly tell that it is last symbol
    // so we should probably just return cumfreqTarget without doing this
    return (cumfreqTarget >= cumfreqTot) ? (cumfreqTot-1) : cumfreqTarget;
}

RADFORCEINLINE U32 rrArithDecodePeekScaled( rrArithDecoder* ac , U32 cumfreqTot , U32 * pScale)
{
    RR_ASSERT( cumfreqTot <= RR_ARITHCODER_CUMPROBMAX );
    RR_ASSERT( ac->range >= RR_ARITHCODER_MINRANGE );
    
    ac->range /= cumfreqTot;
    
    *pScale = ac->range;

    return ac->code; 
    
}


RADFORCEINLINE void rrArithDecodeRemove( rrArithDecoder* ac , U32 cumfreqLow, U32 freq, U32 cumfreqTot )
{
    RR_ASSERT( cumfreqLow+freq <= cumfreqTot );
    
    // length already got /= symtot
    
    ac->code -= cumfreqLow * ac->range;
    ac->range *= freq;
    
    rrArithDecodeRenorm(ac);
}

//=====================================================================================================================

RADFORCEINLINE void rrArithEncodeRangePow2_NoRenorm( rrArithEncoder* ac , U32 cumfreqLow, U32 freq, U32 cumfreqTotShift )
{
    RR_ASSERT( cumfreqLow+freq <= (1UL<<cumfreqTotShift) );
    RR_ASSERT( cumfreqTotShift <= RR_ARITHCODER_CUMPROBMAX_SHIFT );
    RR_ASSERT( ac->range >= RR_ARITHCODER_MINRANGE );

    U32 save_low = ac->low;
    U32 r = ac->range;
    r >>= cumfreqTotShift;
    U32 new_low = save_low + cumfreqLow * r;
    ac->low = new_low;
    ac->range = r * freq;

    if ( new_low < save_low ) rrArithEncodePropagateCarry(ac);
}

RADFORCEINLINE void rrArithEncodeRangePow2( rrArithEncoder* ac , U32 cumfreqLow, U32 freq, U32 cumfreqTotShift )
{
    RR_ASSERT( cumfreqLow+freq <= (1UL<<cumfreqTotShift) );
    RR_ASSERT( cumfreqTotShift <= RR_ARITHCODER_CUMPROBMAX_SHIFT );
    RR_ASSERT( ac->range >= RR_ARITHCODER_MINRANGE );

    U32 save_low = ac->low;
    U32 r = ac->range;
    r >>= cumfreqTotShift;
    U32 new_low = save_low + cumfreqLow * r;
    ac->low = new_low;
    ac->range = r * freq;

    if ( new_low < save_low ) rrArithEncodePropagateCarry(ac);
    
    rrArithEncodeRenorm(ac);
    
}

// Peek is mutating ; call it only once, always call Peek/Remove in a pair
RADFORCEINLINE U32 rrArithDecodePeekPow2( rrArithDecoder* ac , U32 cumfreqTotShift )
{
    RR_ASSERT( cumfreqTotShift <= RR_ARITHCODER_CUMPROBMAX_SHIFT );
    RR_ASSERT( ac->range >= RR_ARITHCODER_MINRANGE );
    
    ac->range >>= cumfreqTotShift;
    
    U32 cumfreqTarget = ac->code / ac->range;
    
    return cumfreqTarget;
}

RADFORCEINLINE U32 rrArithDecodePeekScaledPow2( rrArithDecoder* ac , U32 cumfreqTotShift , U32 * pScale)
{
    RR_ASSERT( cumfreqTotShift <= RR_ARITHCODER_CUMPROBMAX_SHIFT );
    RR_ASSERT( ac->range >= RR_ARITHCODER_MINRANGE );
    
    ac->range >>= cumfreqTotShift;
    
    *pScale = ac->range;

    return ac->code; 
    
}

RADFORCEINLINE void rrArithDecodeRemovePow2( rrArithDecoder* ac , U32 cumfreqLow, U32 freq, U32 cumfreqTotShift )
{
    RR_ASSERT( cumfreqLow+freq <= (1UL<<cumfreqTotShift) );
    
    // length already got /= symtot
    
    ac->code -= cumfreqLow * ac->range;
    ac->range *= freq;
    
    rrArithDecodeRenorm(ac);
}

RADFORCEINLINE void rrArithDecodeRemovePow2_NoRenorm( rrArithDecoder* ac , U32 cumfreqLow, U32 freq, U32 cumfreqTotShift )
{
    RR_ASSERT( cumfreqLow+freq <= (1UL<<cumfreqTotShift) );
    
    // length already got /= symtot
    
    ac->code -= cumfreqLow * ac->range;
    ac->range *= freq;
}
    
//=====================================================================================================================

RADFORCEINLINE void rrArithEncodeBinary( rrArithEncoder* ac , rrbool bit, U32 p0, U32 ptot )
{
    RR_ASSERT( p0 > 0 && p0 < ptot );
    RR_ASSERT( ptot <= RR_ARITHCODER_CUMPROBMAX );
    RR_ASSERT( ac->range >= RR_ARITHCODER_MINRANGE );

    U32 p0range = p0 * (ac->range / ptot);
    
    if ( bit )
    {
        //U32 save_low = ac->low;
    
        ac->low += p0range;
        ac->range -= p0range;
        
        //if ( ac->low < save_low ) rrArithEncodePropagateCarry(ac);
        if ( ac->low < p0range ) rrArithEncodePropagateCarry(ac);
    }
    else
    {
        ac->range = p0range;
    }
    
    rrArithEncodeRenorm(ac);
}

RADFORCEINLINE U32 rrArithDecodeBinary( rrArithDecoder* ac , U32 p0, U32 ptot )
{
    RR_ASSERT( p0 > 0 && p0 < ptot );
    RR_ASSERT( ptot <= RR_ARITHCODER_CUMPROBMAX );
    RR_ASSERT( ac->range >= RR_ARITHCODER_MINRANGE );

    U32 p0range = p0 * (ac->range / ptot);
    
    if ( ac->code >= p0range )
    {
        ac->code -= p0range;
        ac->range -= p0range;
    
        rrArithDecodeRenorm(ac);
        
        return 1;
    }
    else
    {
        ac->range = p0range;
    
        rrArithDecodeRenorm(ac);
    
        return 0;
    }   
}

RADFORCEINLINE void rrArithEncodeBinaryPow2( rrArithEncoder* ac , rrbool bit, U32 p0, U32 ptotShift )
{
    RR_ASSERT( p0 > 0 && p0 < (1UL<<ptotShift) );
    RR_ASSERT( (1UL<<ptotShift) <= RR_ARITHCODER_CUMPROBMAX );
    RR_ASSERT( ac->range >= RR_ARITHCODER_MINRANGE );

    U32 p0range = p0 * (ac->range >> ptotShift);
    
    if ( bit )
    {
        //U32 save_low = ac->low;
    
        ac->low += p0range;
        ac->range -= p0range;
        
        //if ( ac->low < save_low ) rrArithEncodePropagateCarry(ac);
        if ( ac->low < p0range ) rrArithEncodePropagateCarry(ac);
    }
    else
    {
        ac->range = p0range;
    }
    
    rrArithEncodeRenorm(ac);
}

RADFORCEINLINE U32 rrArithDecodeBinaryPow2( rrArithDecoder* ac , U32 p0, U32 ptotShift )
{
    RR_ASSERT( p0 > 0 && p0 < (1UL<<ptotShift) );
    RR_ASSERT( (1UL<<ptotShift) <= RR_ARITHCODER_CUMPROBMAX );
    RR_ASSERT( ac->range >= RR_ARITHCODER_MINRANGE );

    U32 p0range = p0 * (ac->range >> ptotShift);
    
    if ( ac->code >= p0range )
    {
        ac->code -= p0range;
        ac->range -= p0range;
    
        rrArithDecodeRenorm(ac);
        
        return 1;
    }
    else
    {
        ac->range = p0range;
    
        rrArithDecodeRenorm(ac);
    
        return 0;
    }   
}

//=====================================================================================================================
/**

rrArithBinaryModelInit/rrArithBinaryModelEncode/rrArithBinaryModelDecode

simple binary coder + probability update that keeps tot probability a power of 2

for real use you would want to tweak RR_BINARY_MODEL_TOT_SHIFT and RR_BINARY_MODEL_UPD_SHIFT

A common setting is 

RR_BINARY_MODEL_TOT_SHIFT = 12
RR_BINARY_MODEL_UPD_SHIFT = 5

The probability increment is   1<<(RR_BINARY_MODEL_TOT_SHIFT - RR_BINARY_MODEL_UPD_SHIFT)

That is, small UPD_SHIFT means very large probability steps (fast adaptation, but inaccurate)
Large UPD_SHIFT means small probability steps (slow adaptation, but more accurate in steady state)

**/

#ifndef RR_BINARY_MODEL_TOT_SHIFT
#define RR_BINARY_MODEL_TOT_SHIFT   (14)
#endif
#ifndef RR_BINARY_MODEL_UPD_SHIFT
#define RR_BINARY_MODEL_UPD_SHIFT   (7)
#endif

RR_COMPILER_ASSERT( RR_BINARY_MODEL_TOT_SHIFT <= 16 );

#define RR_BINARY_MODEL_INIT_VAL	((rrArithBinaryModel)1<<(RR_BINARY_MODEL_TOT_SHIFT-1))

#define RR_BINARY_MODEL_TOT         ((rrArithBinaryModel)1<<RR_BINARY_MODEL_TOT_SHIFT)

RR_COMPILER_ASSERT( RR_BINARY_MODEL_TOT <= RR_ARITHCODER_CUMPROBMAX );
RR_COMPILER_ASSERT( RR_BINARY_MODEL_UPD_SHIFT > 0 && RR_BINARY_MODEL_UPD_SHIFT < RR_BINARY_MODEL_TOT_SHIFT );



static RADFORCEINLINE void rrArithBinaryModelInit( rrArithBinaryModel * p0 )
{
    *p0 = RR_BINARY_MODEL_INIT_VAL;
}

/*
static RADFORCEINLINE void rrArithBinaryModelRenorm( rrArithBinaryModel * p0 , int shifts)
{
	RR_ASSERT( shifts >= 0 );
	const rrArithBinaryModel half = 1UL<<(RR_BINARY_MODEL_TOT_SHIFT-1);
    *p0 = ( (*p0) + half * ( (1<<shifts) - 1 ) ) >> shifts;
}

static RADFORCEINLINE void rrArithBinaryModelRenorm( rrArithBinaryModel * p0 , int divisor)
{
	RR_ASSERT( divisor >= 1 );
	const rrArithBinaryModel half = 1UL<<(RR_BINARY_MODEL_TOT_SHIFT-1);
    *p0 = ( (*p0) + half * ( divisor - 1 ) ) / divisor;
}
*/

// binary model "Renorm" ; not really a renorm,
//	it just pushes the probability back towards 50/50 a bit
static RADFORCEINLINE void rrArithBinaryModelRenorm( rrArithBinaryModel * p0 )
{
	const rrArithBinaryModel half = 1UL<<(RR_BINARY_MODEL_TOT_SHIFT-1);
    //*p0 = ( (*p0) + half ) >>1;
    *p0 = ( (*p0)*3 + half ) /4;
}

static RADFORCEINLINE void rrArithBinaryModelEncode( rrArithEncoder* ac , rrbool bit, rrArithBinaryModel * pp0 )
{
    RR_ASSERT( *pp0 < RR_BINARY_MODEL_TOT );
    RR_ASSERT( ac->range >= RR_ARITHCODER_MINRANGE );

    U32 p0range = ( (*pp0) * (ac->range >> RR_BINARY_MODEL_TOT_SHIFT) );
    
    if ( bit )
    {
        //rrArithBinaryModel save_low = ac->low;
    
        ac->low += p0range;
        ac->range -= p0range;
        
        *pp0 -= (*pp0) >> RR_BINARY_MODEL_UPD_SHIFT;
        
        //if ( ac->low < save_low ) rrArithEncodePropagateCarry(ac);
        if ( ac->low < p0range ) rrArithEncodePropagateCarry(ac);

	    rrArithEncodeRenorm(ac);
    }
    else
    {
        ac->range = p0range;
        
        *pp0 += (RR_BINARY_MODEL_TOT - (*pp0)) >> RR_BINARY_MODEL_UPD_SHIFT;

	    rrArithEncodeRenorm(ac);
    }

}

static RADFORCEINLINE rrbool rrArithBinaryModelDecode( rrArithDecoder* ac , rrArithBinaryModel * pp0 )
{
    RR_ASSERT( (*pp0) < RR_BINARY_MODEL_TOT );
    RR_ASSERT( ac->range >= RR_ARITHCODER_MINRANGE );

    U32 p0range = ( (*pp0) * (ac->range >> RR_BINARY_MODEL_TOT_SHIFT) );
    
    if ( ac->code >= p0range )
    {
        ac->code -= p0range;
        ac->range -= p0range;
    
        *pp0 -= (*pp0) >> RR_BINARY_MODEL_UPD_SHIFT;
        
        rrArithDecodeRenorm(ac);
        
        return 1;
    }
    else
    {
        ac->range = p0range;
    
        *pp0 += (RR_BINARY_MODEL_TOT - (*pp0)) >> RR_BINARY_MODEL_UPD_SHIFT;
        
        rrArithDecodeRenorm(ac);
    
        return 0;
    }   
}

static RADFORCEINLINE void rrArithBinaryModelEncodeNoAdapt( rrArithEncoder* ac , rrbool bit, rrArithBinaryModel p0 )
{
    RR_ASSERT( p0 < RR_BINARY_MODEL_TOT );
    RR_ASSERT( ac->range >= RR_ARITHCODER_MINRANGE );

    U32 p0range = (p0) * (ac->range >> RR_BINARY_MODEL_TOT_SHIFT);
    
    if ( bit )
    {
        //rrArithBinaryModel save_low = ac->low;
    
        ac->low += p0range;
        ac->range -= p0range;
        
        //if ( ac->low < save_low ) rrArithEncodePropagateCarry(ac);
        if ( ac->low < p0range ) rrArithEncodePropagateCarry(ac);

	    rrArithEncodeRenorm(ac);
    }
    else
    {
        ac->range = p0range;
        
	    rrArithEncodeRenorm(ac);
    }

}

static RADFORCEINLINE rrArithBinaryModel rrArithBinaryModelDecodeNoAdapt( rrArithDecoder* ac , rrArithBinaryModel p0 )
{
    RR_ASSERT( (p0) < RR_BINARY_MODEL_TOT );
    RR_ASSERT( ac->range >= RR_ARITHCODER_MINRANGE );

    U32 p0range = (p0) * (ac->range >> RR_BINARY_MODEL_TOT_SHIFT);
    
    if ( ac->code >= p0range )
    {
        ac->code -= p0range;
        ac->range -= p0range;
    
        rrArithDecodeRenorm(ac);
        
        return 1;
    }
    else
    {
        ac->range = p0range;
    
        rrArithDecodeRenorm(ac);
    
        return 0;
    }   
}

static RADFORCEINLINE F32  rrArithBinaryModelGetBitsToCode( rrbool bit , rrArithBinaryModel p0 )
{
	RR_ASSERT( p0 < RR_BINARY_MODEL_TOT );
	
	// bits to code a 0 = log2( RR_BINARY_MODEL_TOT / p0 )
	// bits to code a 0 = RR_BINARY_MODEL_TOT_SHIFT - log2 ( p0 )
		
	// bits to code a 1 = log2( RR_BINARY_MODEL_TOT / (RR_BINARY_MODEL_TOT - p0) )
	//			= RR_BINARY_MODEL_TOT_SHIFT - log2 ( RR_BINARY_MODEL_TOT - p0 )
		
	rrArithBinaryModel p = ( bit ) ? (RR_BINARY_MODEL_TOT - p0) : p0;
	
	//F32 bits = RR_BINARY_MODEL_TOT_SHIFT - rrlog2f_approx( (F32) p);

	F32 bits = (F32) rrlog2neg_bk<RR_BINARY_MODEL_TOT_SHIFT>( p );

	RR_ASSERT( bits >= 0 && bits < RR_BINARY_MODEL_TOT_SHIFT );

	return bits;
}

//=====================================================================================================================
// raw bit IO :

RADFORCEINLINE void rrArithPutBit(  rrArithEncoder* ac , rrbool bit )
{
    ac->range >>= 1;
    
    if ( bit )
    {
        ac->low += ac->range;
        
        if ( ac->low < ac->range ) rrArithEncodePropagateCarry(ac);
    }
    
    rrArithEncodeRenorm(ac);
}

RADFORCEINLINE U32  rrArithGetBit(  rrArithDecoder* ac )
{
    ac->range >>= 1;
    
    /*
    U32 bit = ( ac->code >= ac->range ) ? 1 : 0;
    
    // ? : is much better than if
    ac->code -= ( ac->code >= ac->range ) ? ac->range : 0;;
    //ac->code -= ( bit ) ? ac->range : 0;;
        
    //if ( bit )
    //{
    //    ac->code -= ac->range;
    //}
    /*/
    
    // branchless version of GetBit :
    // slightly faster than ?: version
    
    S32 t = (S32)(ac->range - ac->code - 1);
    U32 mask = (U32) ((S32)t>>31); // propagate sign bit
    U32 bit = (mask&1); // return 0 or 1
    ac->code -= (mask & ac->range); // either subtract range or don't
    
    /**/
    
    rrArithDecodeRenorm(ac);
    
    return bit;
}

RADFORCEINLINE void rrArithPutBits(  rrArithEncoder* ac , U32 val, U32 numBits )
{
	RR_ASSERT( numBits > 0 && numBits < 24 );	// from RR_ARITHCODER_MINRANGE
    RR_ASSERT( val < (1UL<<numBits) );

    U32 save_low = ac->low;

    ac->range >>= numBits;
    ac->low += val * ac->range;

    if ( ac->low < save_low ) rrArithEncodePropagateCarry(ac);
    
    rrArithEncodeRenorm(ac);
}

RADFORCEINLINE U32  rrArithGetBits(  rrArithDecoder* ac , U32 numBits )
{
	RR_ASSERT( numBits > 0 );

    ac->range >>= numBits;
    
    U32 val = ac->code / ac->range;
    
    ac->code -= val * ac->range;
    
    rrArithDecodeRenorm(ac);
    
    RR_ASSERT( val < (1UL<<numBits) );
    return val;
}

RADFORCEINLINE U32  rrArithGetBits_NoRenorm(  rrArithDecoder* ac , U32 numBits )
{
	RR_ASSERT( numBits > 0 );

    ac->range >>= numBits;
    
    U32 val = ac->code / ac->range;
    
    ac->code -= val * ac->range;
        
    RR_ASSERT( val < (1UL<<numBits) );
    return val;
}

OODLE_NS_END

#endif //__RR_ARITHCODERINL__
