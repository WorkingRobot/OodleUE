// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_COMPRESSUTIL_H__
#define __RADRR_COMPRESSUTIL_H__

#include "oodlecore.h"
#include "rrmath.h"
#include "rrmem.h"
#include "rrmemutil.h"

OODLE_NS_START

/**

CompressUtil :

place to put random compression-related helper functions

**/

// disk_speed_hi can be 0.0 for "infinity"
double WeissmanScore(double ratio, double speed, double disk_speed_lo, double disk_speed_hi );


// rrLogCompression does NOT put on a \n , that's up to you where you want that or not
void rrLogCompression(S64 UnPackedLen,S64 PackedLen);

void rrLogHighestValues(const U32 * values,int value_count,int num_to_log);

/**

normalize_counts

to == from is not okay

**/
	
S32 normalize_counts_v5(U32 * to, int to_sum_desired, const U32 * from, int from_sum, int alphabet);
S32 normalize_counts_v6(U32 * to, int to_sum_desired, const U32 * from, int from_sum, int alphabet);
#define normalize_counts_current	normalize_counts_v6
// normalize_counts returns num_non_zero

// from is [alphabet] , to is [alphabet+1] , to[0] = 0 and to[alphabet] = sum
void counts_to_cumfreqs(U32 *to, const U32 *from, int alphabet);

int EntropyOfCountsRice(int * counts,int numCounts,int rice_bits);
int EntropyOfDeltaRice(int * counts,int numCounts,int mean,int rice_bits);

int EntropyOfCountsExpGolomb(const U32 * counts,int numCounts,int expgolomb_k);

//===================================================================

// FoldUpNegatives : positives become even, negatives become odd

// explicit versions :
//RADINLINE U32 rrFoldUpNegatives(S32 i) { if ( i >= 0 ) return i+i; else return (U32)(-i-i-1); }
//RADINLINE S32 rrUnfoldNegatives(U32 i) { if ( i&1 ) return - (S32)((i+1)>>1); else return (i>>1); }

// fast versions :
// using :
// -x = (x-1)^(-1)
// -x = (x^-1) +1

static RADFORCEINLINE U32 rrFoldUpNegatives(S32 i)
{
	U32 x = (U32)i << 1;
	S32 y = i >> 31;
	return x ^ y;
}
		
static RADFORCEINLINE S32 rrUnfoldNegatives(U32 i)
{
	U32 x = i >> 1;
	S32 y = -(S32)(i & 1);
	return x ^ y;
}

//===================================================================

/**

FoldDeltaClamped :

like FoldUpNegatives , but uses the fact that val & pred are in a clamped range [0,range)

avoid wasting output space on impossible values
eg. if val is a byte, so range=256 , and pred = 16, then deltas < -16 are impossible and wasted

here I require pred < (range/2) for simplicity
so I can assume the bottom part of range, below pred, is the smaller interval

the range is divided into 

[0,pred] = valid negative deltas for folding
[pred,2*pred] = positive deltas for folding
[2*pred,range] = positive deltas where there is no corresponding legal negative, so no fold

-------------

Huff codelens

with rrFoldUpNegatives :

lzt99 : 24,700,820 ->10,210,410 =  3.307 bpb =  2.419 to 1 

with rrFoldDeltaClamped : [0,11]

lzt99 : 24,700,820 ->10,210,406 =  3.307 bpb =  2.419 to 1 

woo! 4 bytes!

**/

// returns delta = (val - pred)
static RADFORCEINLINE U32 rrFoldDeltaClamped(S32 val,S32 pred)
{
	// range is not actually needed except for sanity checks :
	//RR_ASSERT( val >= 0 && val < range );
	//RR_ASSERT( pred >= 0 && pred < (range/2) ); // pred not == range/2

	RR_ASSERT( val >= 0 && pred >= 0 );
		
	if ( val <= pred*2 )
	{
		S32 delta = val - pred;
		// normal foldup negatives of delta :
		U32 ret = rrFoldUpNegatives(delta);
		RR_ASSERT( ret <= (U32)2*pred );
		return ret;
	}
	else
	{
		return val;
	}
}

// returns val = (delta + pred)
static RADFORCEINLINE S32 rrUnfoldDeltaClamped(U32 delta,S32 pred)
{
	RR_ASSERT( pred >= 0 );
	
	// range is not actually needed except for sanity checks :
	//RR_ASSERT( delta >= 0 && delta < (S32)range );
	//RR_ASSERT( pred >= 0 && pred < (range/2) ); // pred not == range/2

	if ( (S32)delta <= 2*pred )
	{
		S32 ret = rrUnfoldNegatives(delta) + pred;
		RR_ASSERT( ret <= 2*pred );
		return ret;
	}	
	else
	{
		return delta;
	}
}

//===================================================================

// NOTE : use of BitFlags256 makes this specific to the U8 alphabet !
struct BitFlags256
{
	U32	words[8];
	
	void init()
	{
		RR_UNROLL_I_8(0, words[i] = 0 );
	}
	
	void set(int pos)
	{
		int i = pos/32;
		words[i] |= ((U32)1)<<pos;
	}
	
	U32 test(int pos) const
	{
		int i = pos/32;
		return ( words[i] & (((U32)1)<<pos) );
	}	
};

//===================================================================

OODLE_NS_END

#endif // __RADRR_COMPRESSUTIL_H__
