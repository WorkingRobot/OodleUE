// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrbase.h"
#include "rrmath.h"
#include "oodlecore.h"

OODLE_NS_START

#define RR_LOG2TABLE_ONE_SHIFT	(13)
#define RR_LOG2TABLE_ONE		(1<<RR_LOG2TABLE_ONE_SHIFT)

//extern void LZA_Optimal_InitStatics();

/****

template <int t_one_shift>
S32 log2tabled(U32 x);

log2tabled<t_one_shift>(i) == - RR_LOG2TABLE_ONE * log2(i/(double)(1<<t_one_shift))

that is, x is a probability in fixed point (t_one_shift is 100% probability)
this returns the negative log2 (eg. the codelen)

codelen = - log2( P )

-----------------------------

log2tabled == log2tabled_bk

except :
log2tabled(i) requires i <= "one"
log2tabled_bk is okay with i >= "one"

log2tabled(0) returns a well defined large value
log2tabled_bk is not allowed

log2tabled("one") is okay (==0)
log2tabled_bk("one") is okay (==0)

"one" = 1<<t_one_shift

-----------------

NOTE : "log2tabled" is *negative* log2 ; log2(1/x)

codelen = log2neg(count) - log2neg(sum)

return value is scaled up by RR_LOG2TABLE_ONE

"log2tabled" returns S32 but is strictly positive

-----------------

log2tabled_bk is slower but uses a smaller table
useful when you don't know the scale of your values

log2tabled is better when you know the value is in a [zero,one] range
	(like a fixed point probability with one = 1<<14 or whatever)

*****/

//===========================

// error measurement in test_misc
//#define bk_tablesize_shift	5 // max_err : 0.000241657 ; 9368220
#define bk_tablesize_shift	6 // max_err : 0.000146396 ; lztestset : 9367355
//#define bk_tablesize_shift	7 // max_err : 0.000119627 ; very good ; lztestset : 9367301
#define bk_tablesize		(1<<bk_tablesize_shift)

extern U16 const c_log2table_bk[bk_tablesize+1];
//extern U16 c_log2table_bk[bk_tablesize+1];
    
//extern U32 g_log2tabled_bk_bias;
    
// Fixed-point logarithm using bit scans and a small table.
//	log2tabled_bk_32 is fine and allows values in any range
//	the t_one_shift just adds a constant offset to the log2
template <int t_one_shift>
S32 log2tabled_bk(U32 x)
{
	RR_COMPILER_ASSERT( t_one_shift >= bk_tablesize_shift && t_one_shift <= 32 );

	RR_ASSERT( x > 0 );

    //F64 check = RR_LOG2TABLE_ONE * rrlog2((F64)(1<<t_one_shift)/x);
    
    //U32 log2i = highest_set_bit(x);
    U32 log2i = 31 - rrClz32(x);
    RR_ASSERT( log2i >= 0 );
    //RR_ASSERT( log2i < t_one_shift );
    
    U32 xmant = x << (32 - log2i); // mantissa left-aligned
    // take top bk_tablesize_shift :
    U32 idx = xmant >> (32 - bk_tablesize_shift);
    RR_ASSERT( idx >= 0 && idx <= bk_tablesize );

	// table lookup below and interpolate to next :
    U32 base = (c_log2table_bk[idx]);
    U32 diff = (c_log2table_bk[idx + 1]) - base;
    
    RR_ASSERT( base < RR_LOG2TABLE_ONE );
    RR_ASSERT( (S32)diff >= 0 );
    
    // returning *negative* log2 :
    S32 ret = (t_one_shift - log2i) * RR_LOG2TABLE_ONE;
    ret -= base;
    
    // shift out the "idx" part we already used :
    xmant <<= bk_tablesize_shift;
    // diff is 13 bit, so we can mul by 16 bits :
    //ret -= (diff * (xmant>>16) + (1<<15)) >> 16;
    // alternative :
    // I think this is very slightly more accurate
    //ret -= (U32)(((U64)diff * xmant + g_log2tabled_bk_bias)>>32);
    // min sum of squares error :
    //ret -= (U32)(((U64)diff * xmant + 0xB3FFFFFF)>>32);
    // min sum of relative error :
    ret -= (U32)(((U64)diff * xmant + 0x9FFFFFFF)>>32);
        
    //F64 err = check - ret;
    //RR_ASSERT( RR_ABS(err) < check * 0.5 );
    
    return ret;
}

// rrlog2neg_bk returns a float scaled to 1.0
//	this is the *negative* log2 of ( i / (1<<t_one_shift) ) == (t_one_shift - log2(i))
template <int t_one_shift>
F32 rrlog2neg_bk(U32 i)
{
	S32 t = log2tabled_bk<t_one_shift>(i);
	F32 ret = t * (1.0f / RR_LOG2TABLE_ONE);
	
	#if 0 //def RR_DO_ASSERTS
	{
	// make sure it's what it claims :
	// check the error bounds :
	double x = i / (double) (((S64)1)<<t_one_shift);
	double y = - rrlog2(x);
	double z = RR_ABS(ret - y);
	RR_ASSERT( z < 0.05 );
	RR_ASSERT( z < 0.001 || z/y < 0.0075 );
	}
	#endif
	
	return ret;
}

// log2tabled_bk_32 , first LOG2TABLED_BK_32_TABLE_SIZE values :
#define LOG2TABLED_BK_32_TABLE_SIZE	32
extern const S32 c_log2tabled_bk_32[LOG2TABLED_BK_32_TABLE_SIZE];

static RADINLINE S32 log2tabled_bk_32(U32 x)
{
	if ( x < LOG2TABLED_BK_32_TABLE_SIZE ) return c_log2tabled_bk_32[x];

	return log2tabled_bk<32>(x);
}

// same as rrlog2neg_bk<32> but uses table
static RADINLINE F32 rrlog2neg_bk_32(U32 i)
{
	S32 t = log2tabled_bk_32(i);
	F32 ret = t * (1.0f / RR_LOG2TABLE_ONE);
	return ret;
}

// just a log2(x) int arg, float return  (not a negative log2 like most here)
static RADINLINE F32 rrlog2_bk(U32 i)
{
	return 32.0f - rrlog2neg_bk_32(i);
}

F32 rrlog2_bk_U64(U64 x);

// log2, returns fixed point in specified bits; (not a negative log2 like most here)
template <int t_fp_bits>
U32 rrlog2_bk_fixedpoint(U32 i)
{
	U32 t = 32*RR_LOG2TABLE_ONE - log2tabled_bk_32(i);
	
	if ( t_fp_bits == RR_LOG2TABLE_ONE_SHIFT )
		return t;
	else if ( t_fp_bits > RR_LOG2TABLE_ONE_SHIFT )
		return t << RR_MAX(t_fp_bits - RR_LOG2TABLE_ONE_SHIFT,0);
	else
		return t >> RR_MAX(RR_LOG2TABLE_ONE_SHIFT - t_fp_bits,0);
}

//======================================================================

OODLE_NS_END

//======================================================================

#ifdef OODLE_BUILDING_DATA

// Oodle Data needs biglog2table for encoder choices

// log2tabled == log2tabled_big

#include "biglog2table.h"

OODLE_NS_START

template <int t_one_shift>
S32 log2tabled(U32 x)
{
	return log2tabled_big<t_one_shift>(x);
}

OODLE_NS_END

#else

// Oodle Network, Tex - just use BK

// log2tabled == log2tabled_bk

OODLE_NS_START

template <int t_one_shift>
S32 log2tabled(U32 x)
{
	return log2tabled_bk<t_one_shift>(x);
}

OODLE_NS_END

#endif