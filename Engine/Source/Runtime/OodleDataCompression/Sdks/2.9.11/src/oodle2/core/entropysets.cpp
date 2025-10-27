// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "entropysets.h"
#include "log2table.h"
#include "cbradutil.h"
#include "newlz_histo.h"
#include "newlz_simd.h"
#include "newlz_arrays.h"
#include "templates/rrvector_a.h"
#include "templates/rralgorithm.h"
#include "newlz_speedfit.h"
#include "rrarenaallocator.h"

OODLE_NS_START

enum { LOG2TABLED_TO_ENTROPYSET_CODELEN_SHIFT = RR_LOG2TABLE_ONE_SHIFT - ENTROPYSET_CODELEN_ONE_BIT_SHIFT };


/*****

I used to think that ENTROPYSET_SYM_NOT_PRESENT_CL should be > ENTROPYSET_SYM_PRESENT_MAX_CL

the idea was that you would rather put a symbol into a histo that has seen a symbol at least once
than to put it in one that has never seen that symbol at all
try to preserve zeros and purity

but that doesn't seem to be the case in reality

current code assumes ENTROPYSET_SYM_NOT_PRESENT_CL = ENTROPYSET_SYM_PRESENT_MAX_CL

-----------------------

thought on ENTROPYSET_SYM_NOT_PRESENT_CL

this should really depend on the purity of the histo

you're considering adding a symbol to this histo which was not previously in it

if the histo was previously pure (only 1 or 2 symbols) then adding a novel one is very bad

if the histo was previously dense (lots of symbols) adding one more is not bad

novel symbols should NOT have the same cost in those cases

eg.

say you encounter the byte 'A'

you have previous histos

{ B , C }

and 

{ B , D, E , F , G }

you should prefer adding A to the latter !!


*****/


// invSum is (1<<ENTROPYSET_INVSUM_SHIFT)/sum
//	returns bits , scaled by ENTROPYSET_CODELEN_ONE_BIT
// using RR_LOG2TABLE_SIZE_SHIFT instead of ENTROPYSET_INVSUM_SHIFT would be a little faster
static RADFORCEINLINE int simple_codelen_30( U32 count, U32 invSum )
{
	// currently the codelen for count == 0 is the same either way
	//  so this branch can be removed
	RR_COMPILER_ASSERT( (int)ENTROPYSET_SYM_NOT_PRESENT_CL == (int)ENTROPYSET_SYM_PRESENT_MAX_CL );
	
	// 
	// log2tabled(0) returns a large number
	//	log2tabled_bk does NOT !
	// if using log2tabled_bk you need the check for == 0 here
	#ifndef RR_LOG2TABLE_SIZE // do I have the "big" log2table ?
	if ( count == 0 )
	{
		return ENTROPYSET_SYM_NOT_PRESENT_CL;
	}
	#endif
	
	int cl = log2tabled<ENTROPYSET_INVSUM_SHIFT>( count * invSum );
	cl = cl >> LOG2TABLED_TO_ENTROPYSET_CODELEN_SHIFT;
	// round? appears to be slightly worse
	//cl = (cl + (1<<(LOG2TABLED_TO_ENTROPYSET_CODELEN_SHIFT-1))-1 ) >> LOG2TABLED_TO_ENTROPYSET_CODELEN_SHIFT;

#if defined(__clang__) && defined(__RADX86__)
	// Clang on x86 targets compiles the MIN into a branchy construct which gives
	// terrible perf for this use case. (>2x slowdown for entropysets_order0_codelen_bits)
	__asm__(
		"cmpl		%[maxv], %[cl]\n"
		"cmovg		%[maxv], %[cl]\n"
		: [cl]"+r"(cl)
		: [maxv]"r"(ENTROPYSET_SYM_PRESENT_MAX_CL)
	);
#else
	cl = RR_MIN(ENTROPYSET_SYM_PRESENT_MAX_CL,cl);
#endif

	return cl;
}

static RADINLINE bool fits_S32(S64 x)
{
	S32 t = (S32)x;
	return (S64)t == x;
}

static RADINLINE S32 add_S32_check_overflow(S32 x1,S32 x2)
{
	S32 ret = x1 + x2;
	RR_ASSERT( ((S64)x1+x2) == (S64)ret );
	return ret;
}

static void histo_to_codelens_simple(const Histo256 & histo,SINTa histo_sum,
	entropyset_codelens_U16_256 & codelens)
{
	RR_ASSERT( histo_sum > 0 );

	U32 invSum = (1<<ENTROPYSET_INVSUM_SHIFT) / (U32)histo_sum;
	
	for LOOP(s,256)
	{
		U32 count = histo.counts[s];
		int cl = simple_codelen_30(count,invSum);
		RR_ASSERT( count > 0 || cl == ENTROPYSET_SYM_NOT_PRESENT_CL );
		codelens.codelen[s] = U16_check(cl);
	}
}	
	
S32 entropysets_self_codelen(const Histo256 & histo,SINTa histo_sum)
{
	// -> called by entropysets_order0_codelen_bits
	// -> also called directly from the initial seed k-means setup
	
	RR_ASSERT( histo_sum > 0 );
	//SIMPLEPROFILE_SCOPE(entropysets_self_codelen);

	U32 invSum = (1<<ENTROPYSET_INVSUM_SHIFT) / (U32)histo_sum;
	S32 clSum = 0;
	
	for LOOP(s,256)
	{
		U32 count = histo.counts[s];
		int cl = simple_codelen_30(count,invSum);
		clSum += cl * count;
	}	

	return clSum;
}	

// does not include any header bytes of codelen transmission cost
//	just an order0 entropy (scaled by sumCounts)
U32 entropysets_order0_codelen_bits(const Histo256 & histo,SINTa sumCounts)
{
	// -> this is the cost func for the initial N^2 merger
	//	-> that takes 2*target down to target seeds
	//SIMPLEPROFILE_SCOPE(entropysets_order0_codelen_bits);
	
	// rrCodeLenOfHistogramTis in bits, total
	//  prefer my version of this - entropysets_self_codelen
	//U32 cl1 = (U32) rrCodeLenOfHistogramT(histo.counts,256,(U32)sumCounts);

	// NOTE: SSE4 version in entropysets_sse4.cpp!

	U32 cl2 = entropysets_self_codelen(histo,sumCounts);
	cl2 >>= ENTROPYSET_CODELEN_ONE_BIT_SHIFT;
	
	// not quite, but close
	//RR_ASSERT( cl1 == cl2 );
	
	return cl2;
}

// fill "codelens" from histo
void entropysets_histo_to_codelens(const Histo256 & histo,SINTa histo_sum,
	entropyset_codelens_U16_256 * pcodelens)
{
	RR_ASSERT( histo_sum > 0 );
	
	#if 1
	
	histo_to_codelens_simple(histo,histo_sum,*pcodelens);
	
	// simple tweaked :
	// total : 31,000,000 ->15,705,975 =  4.053 bpb =  1.974 to 1
	// simple :
	// total : 31,000,000 ->15,714,468 =  4.055 bpb =  1.973 to 1
	// complex :
	// total : 31,000,000 ->15,712,984 =  4.055 bpb =  1.973 to 1
		
	#else
	
	U32 invSum = (1<<ENTROPYSET_INVSUM_SHIFT) / (U32)histo_sum;
	
	/*
	entropysets_histo_to_codelens
		prefer to put novel symbols in the histo that has more symbols
			(or perhaps the one with lower MPS count)
		so make the codelen for novel symbols be
			(13<<RR_LOG2TABLE_ONE_SHIFT) - num_non_zero_symbols
		that's in the fractional bits
		breaks the tie t
	*/
	
	// find highest_count first :
	U32 highest_count = 0;
	for LOOP(s,256)
	{
		U32 count = histo.counts[s];
		// if ( count ) num_symbols_in_histo++;
		highest_count = RR_MAX(count,highest_count);
	}
	
	U32 highestCountP_30 = invSum * highest_count;
	U32 highestCountP_Log2One = highestCountP_30 >> (ENTROPYSET_INVSUM_SHIFT - ENTROPYSET_CODELEN_ONE_BIT_SHIFT);
	RR_ASSERT( highestCountP_Log2One <= ENTROPYSET_CODELEN_ONE_BIT );
	
	// @@ -> is this idea actually doing anything good?
	//		doing what I want it to?  TEST IT!
	
	for LOOP(s,256)
	{
		U32 count = histo.counts[s];
		
		//codelens.counts[s] = simple_codelen_30(count,invSum);
		
		int cl;
		if ( count == 0 )
		{
			// penalize by highestCountP_Log2One
			//	the purer my histogram is, the worse adding a new symbol is
			cl = ENTROPYSET_SYM_NOT_PRESENT_CL + highestCountP_Log2One;
			// @@ alternative :
			//cl = ENTROPYSET_SYM_NOT_PRESENT_CL - num_symbols_in_histo;
			
			// don't exceed ENTROPYSET_SYM_INVALID_HIGH_CL :
			// (can be exactly == if this histo is pure (highest_count == sum))
			RR_ASSERT( cl <= ENTROPYSET_SYM_INVALID_HIGH_CL );
		}
		else		
		{
			cl = log2tabled<ENTROPYSET_INVSUM_SHIFT>( count * invSum ) >> LOG2TABLED_TO_ENTROPYSET_CODELEN_SHIFT;
			cl = RR_MIN(ENTROPYSET_SYM_PRESENT_MAX_CL,cl);
		}
		codelens.codelen[s] = U16_check(cl);
	}
	
	#endif
}


// return total codelen of "histo" under the codelens in "codelens"
//	this is a cross-cost
S32 entropysets_cross_codelen(const Histo256 & histo,U32 histo_total,const entropyset_codelens_U16_256 & codelens)
{
	// codelen fits in S16 :
	RR_COMPILER_ASSERT( ENTROPYSET_SYM_INVALID_HIGH_CL < 32768 );
	// use histo_total as an over-estimate of histo max count
	bool fits_in_s16 = ( histo_total < 32768 );
	return simd_dotproduct_s32_s16_256((const S32 *)histo.counts,fits_in_s16,(const S16 *)codelens.codelen);
}

U32 entropysets_codelen_of_small_array(const U8 * ptr, SINTa len,const entropyset_codelens_U16_256 & codelens)
{
	U32 clSum = 0;
	
	// for small arrays, just count codelen of each sym in the array

	RR_ASSERT( len < (1<<14) ); // won't overflow U32

	for LOOP(i,len)
	{
		int s = ptr[i];
		U32 cl = codelens.codelen[s];
		clSum += cl;
	}
	
	return clSum;
}
	
	
// entropysets_codelen_of_array : codelen to code the range [ptr,len] using the counts in [histo]
//	this is a cross-cost
//	does not consider how adding [ptr,len] to histo changes its self-cost
S32 entropysets_codelen_of_array(const U8 * ptr, SINTa len,
	const Histo256 & histo,SINTa histo_sum)
{
	// RR_ASSERT( (U32)histo_sum == rrSumOfHistogram(histo.counts,256) );

	U32 invSum = (1<<ENTROPYSET_INVSUM_SHIFT) / (U32)histo_sum;
	S32 clSum = 0;
	
	if ( len < 256 )
	{
		// for small arrays, just count codelen of each sym in the array
	
		for LOOP(i,len)
		{
			int s = ptr[i];
			
			U32 cl = simple_codelen_30(histo.counts[s] , invSum);
			clSum += cl;
		}
		
	}
	else
	{
		// for large arrays, histogram them , and do the log2 on the histogram
	
		Histo256 array_histo;
		CountHistoArrayU8(ptr,len,array_histo.counts,256);
		
		for LOOP(s,256)
		{
			int n = array_histo.counts[s];
			if ( n == 0 ) continue;
			
			U32 cl = simple_codelen_30(histo.counts[s] , invSum);
			//clSum += n * cl;
			clSum = add_S32_check_overflow(clSum,n*cl);
		}
	}
	
	return clSum;
}

/**

There's 3 terms in considering adding a symbol to a coding set :
	1. what is the cost of coding that symbol with the current codelens
	2. what is the cost of adding that symbol to the codelen header transmission
	3. how does adding that symbol affect the cost of other symbols in that histo
		-> this is maximum when the histo was previously "pure"

**/

/**

entropysets_codelen_delta is an approximation in a few ways

1. the chunk we're considering is not removed from histo2 yet
	(could do this, it's not too costly, then put it back if we don't do the move)
	so histo2 very much has an edge in coding the chunk that it includes
	
2. we're not counting how changing the histo sizes affects other codelens
	there's a log2(sum) term
	that changes from log2(sum) to log2(sum +- step)
	log2(len1) - log2(len1+step)
	log2(len2) - log2(len2-step)
	-> when the cost is otherwise close to equal, you want to favor
	moving the chunk towards the larger side
	because you get more win from shrinking the smaller side

**/
		
// compute codelen of sending [ptr,len] bytes with histo1 & histo2 probability models
//	and return the difference
S32 entropysets_codelen_delta(const U8 * ptr, SINTa len,
	const Histo256 & histo1,SINTa len1,
	const Histo256 & histo2,SINTa len2)
{
//	RR_ASSERT( (U32)len1 == rrSumOfHistogram(histo1.counts,256) );
//	RR_ASSERT( (U32)len2 == rrSumOfHistogram(histo2.counts,256) );

	U32 invSum1 = (1<<ENTROPYSET_INVSUM_SHIFT) / (U32)len1;
	U32 invSum2 = (1<<ENTROPYSET_INVSUM_SHIFT) / (U32)len2;
	S32 clDelta = 0;
	
	// [ptr,len] is a slide unit, so typically len <= 128 or 64 , 16 is common
	
	if ( len > 256 ) // @@ what's the switch point?
	{
		Histo256 array_histo;
		CountHistoArrayU8(ptr,len,array_histo.counts,256);
	
		for LOOP(s,256)
		{
			int movec = array_histo.counts[s];
			if ( movec == 0 ) continue;
			
			int c1 = histo1.counts[s];
			int c2 = histo2.counts[s];
			
			// s should at least be in one of the histos, but maybe not both :
			RR_ASSERT( c1 + c2 >= 1 );
			
			U32 cl1 = simple_codelen_30(c1 , invSum1);
			U32 cl2 = simple_codelen_30(c2 , invSum2);		
			
			clDelta = add_S32_check_overflow(clDelta, cl1-cl2 );
		}
	}
	else
	{	
		for LOOP(i,len)
		{
			int s = ptr[i];
			
			int c1 = histo1.counts[s];
			int c2 = histo2.counts[s];
			
			// s should at least be in one of the histos, but maybe not both :
			RR_ASSERT( c1 + c2 >= 1 );
			
			U32 cl1 = simple_codelen_30(c1 , invSum1);
			U32 cl2 = simple_codelen_30(c2 , invSum2);		
			
			clDelta = add_S32_check_overflow(clDelta, cl1-cl2 );
		}
	}
	
	return clDelta;
}

// histo_codelen_delta_removingfrom2
//	like entropysets_codelen_delta
//	but [ptr,len] is being removed from histo2
//	adjust the histos to count the cost *after* the move
// should be slightly more accurate than entropysets_codelen_delta
// BUT this still doesn't account for how moving [ptr,len]
//	affects the codelens of the non-moved bytes
static S32 histo_codelen_delta_movingfrom2to1_1(const U8 * ptr, SINTa len,
	const Histo256 & histo1,SINTa len1,
	const Histo256 & histo2,SINTa len2)
{
	// histo2 as passed in includes the [ptr,len] range
	RR_ASSERT( len2 > len );

	Histo256 array_histo;
	CountHistoArrayU8(ptr,len,array_histo.counts,256);
	
	// [ptr,len] was in range 2
	//	 consider moving it to range 1
	//	so len1 += len and len2 -= len
	
	U32 invSum1_without = (1<<ENTROPYSET_INVSUM_SHIFT) / (U32)(len1);
	U32 invSum2_with    = (1<<ENTROPYSET_INVSUM_SHIFT) / (U32)(len2);
	U32 invSum1_with    = (1<<ENTROPYSET_INVSUM_SHIFT) / (U32)(len1 + len);
	U32 invSum2_without = (1<<ENTROPYSET_INVSUM_SHIFT) / (U32)(len2 - len);
	
	S32 clDelta = 0;
		
	for LOOP(s,256)
	{
		int c1 = histo1.counts[s];
		int c2 = histo2.counts[s];
		int movec = array_histo.counts[s];
		
		RR_ASSERT( c2 >= movec );
		
		// can skip if all zero
		//  (even if movec is zero, the self-costs change)
		if ( c1 + c2 + movec == 0 )
			continue;
		
		// before : [invSum2_with & invSum1_without]
		// after  : [invSum2_without & invSum1_with]
		
		U32 cl1_with = simple_codelen_30(c1 + movec , invSum1_with);		
		U32 cl1_without = simple_codelen_30(c1 , invSum1_without);		
		
		U32 cl2_with = simple_codelen_30(c2 , invSum2_with);		
		U32 cl2_without = simple_codelen_30(c2 - movec , invSum2_without);	
		
		//S32 cl_before = c2 * cl2_with + c1 * cl1_without;
		
		// counts after times codelens after :
		//S32 cl_after  = (c2 - movec) * cl2_without + (c1 + movec) * cl1_with;
		// equal to :
		//S32 cl_after  = c2 * cl2_without + c1 * cl1_with +
		//				movec * (cl1_with - cl2_without);

		// clDelta += cl_after - cl_before;
						
		// you can see there are two changes -
		//	the mid array moves from 2 to 1
		//	and also the self-codelens of 1 &2 change
		S32 cur_clDelta = c2 * (cl2_without - cl2_with) + 
					c1 * (cl1_with - cl1_without) +
					movec * (cl1_with - cl2_without);
		
		clDelta = add_S32_check_overflow(clDelta,cur_clDelta);
		
		// this is the same as :
		//  which may be clearer to see :
		//	c2 (without the moving region) changes from cl2_with to cl2_without
		//	c1 changes from cl1_without to cl1_with
		//	and movec moves from cl2_with to cl1_with
		/*
		clDelta +=	(c2-movec) * (cl2_without - cl2_with) + 
					c1 * (cl1_with - cl1_without) +
					movec * (cl1_with - cl2_with);
		*/
	}
	
	/*
	// account for how length change affects other symbols ?
	// have I got this term right?
	
	// -> it depends entirely on the purity of the histogram
	// if histo1 is random (all counts equal)
	//	and so is [ptr,len]
	//	then removing [ptr,len] doesn't change the self-cost of histo1 *AT ALL*
	// the more non-overlapping the histograms of histo & [ptr,len] are, the bigger the benefit
	//	 of removing it 
	/**/
	
	return clDelta;
}

// histo_codelen_delta_movingfrom2to1_2:
//	full accuracy for reference
//	modify the histo for the move and recompute cost
// array range [ptr,len] was in histo2 ; consider moving it to histo1
static S32 histo_codelen_delta_movingfrom2to1_2(const U8 * ptr, SINTa len,
	const Histo256 & histo1,SINTa len1,
	const Histo256 & histo2,SINTa len2)
{
	RR_ASSERT( len2 > len );
	
	// newlz_array_estimate_complen_bits includes header transmission cost estimate
	
	U32 complen1_before = newlz_array_estimate_complen_bits(histo1.counts,256,(U32)len1);
	U32 complen2_before = newlz_array_estimate_complen_bits(histo2.counts,256,(U32)len2);
	
	// move [ptr,len] from histo2 to histo1
	histo_slide(ptr,len,const_cast<Histo256 &>(histo2),const_cast<Histo256 &>(histo1));
	
	U32 complen1_after = newlz_array_estimate_complen_bits(histo1.counts,256,(U32)(len1+len));
	U32 complen2_after = newlz_array_estimate_complen_bits(histo2.counts,256,(U32)(len2-len));
	
	// move it back to restore histos :
	histo_slide(ptr,len,const_cast<Histo256 &>(histo1),const_cast<Histo256 &>(histo2));
	
	S32 ret = (complen1_after + complen2_after) - (complen1_before + complen2_before);
	
	// ret < 0 means do it - complen after < complen before
	return ret;
}


//=========================================================================

	
//===========================================================

/*******

about the "lazy merge" method

you have N histos
a bottom-up pairwise merge makes N^2 candidates  (N*(N-1)/2)

after you merge I & J to make I <- {I+J}
all other merges with those are now invalidated and need to be re-costed

before you had
merge[I,K] & merge[J,K]
now you need
merge[{I+J},K]

as a candidate

Option 1 : full re-costing

after you do the merge, scan through the merge candidates
find any candidates that refer to I or J and remove them
add new candidate for I+J to all others

this can be O(N^3) with a simple scan for the removal
to avoid the N^3 you need another index that maps I&J to their candidates


Option 2 : lazy update

after a merge, candidates that referred to I & J are marked dirty
(this is done implicitly by storing their length at the time the candidate was made)
when a merge candidate is popped, if it's dirty it is then updated & repushed

The advantage of this is it avoids the N^3
the disadvantage is that it can get the merge order wrong

the mis-ordering occurs if the updated merge cost would ever be *lower* than before.
then we should pop the update merge earlier than we actually do


eg. say we have 4 arrays {0,1,2,3}

first we merge {0,1} -> 0
we had candidates
merge {0,2} with cost 10
merge {0,3} with cost 5

the correct updated costs after the merge should be
{0+1,3} -> 9
{0+1,2} -> 8

we'll pop {0,3} first (cost 5)
we see it's dirty, and update to cost 9
put it back on the heap; it's still best so pop it again
the {0,2} candidate is still pending at cost 10
if we could update it, it would be cost 8 -> we would rather do it than the cost 9 one we have

I can imagine this (cost going down after a merge) happening in a case like this :

array 0 is all AAA
array 1 is all BBB
array 2 is ABABAB
the merge cost of 2 with (0+1) is actually lower than the cost of merge with 0 or 1

so lazy merge will get the heap order wrong in this case


@@ -> I don't actually know if this lazy update heuristic is reasonable

*******/

struct lazy_merge_candidate
{
	F32	gain_J;
	U32 merged_complen_bits;
	int src1,src2;
	U32 src1len,src2len;
};

typedef U32 (histo_cost_bits_func_type)(const Histo256 & histo,SINTa sumCounts);
// histo_cost_bits_func= 
//histo_estimate_complen_bits
//entropysets_order0_codelen_bits

// normal operator less will give a heap that pops largest first :
bool operator < (const lazy_merge_candidate &lhs, const lazy_merge_candidate &rhs)
{
	return lhs.gain_J < rhs.gain_J;
}

void make_lazy_merge_candidate(lazy_merge_candidate & out_merge_candidate,
	entropyset & histo1,int i, entropyset & histo2,int j,
	F32 merge_J_saved,
	histo_cost_bits_func_type * histo_cost_bits_func)
{
	// try merge i+j :
	
	Histo256 merged_histo;
	histo_add(&merged_histo, histo1.histo, histo2.histo);
	SINTa merged_len = (SINTa)histo1.total + histo2.total;
	
	U32 merged_complen_bits = (*histo_cost_bits_func)(merged_histo,merged_len);	
	U32 before_complen_bits = histo1.complen + histo2.complen;
	
	S32 delta_complen_bits = before_complen_bits - merged_complen_bits;
	// delta_complen_bits > 0 is good
	
	// J is in bytes :
	F32 delta_J = (delta_complen_bits/8.f) + merge_J_saved;
	
	/*
	// try to account for the bonus that merging = fewer intervals to send
	// can't find a term here that helps
	F64 interval_len_cost_diff = 
		rrlog2_bk(histo1.total) + rrlog2_bk(histo2.total)
		- rrlog2_bk((U32)merged_len);
	RR_ASSERT( interval_len_cost_diff > 0 );
	// bits -> bytes
	delta_J += (F32)(interval_len_cost_diff)/8;
	*/
	
	//if ( delta_J > 0 ) // push even if not profitable
	// -> you need to do this because other merges might make me dirty,
	//	then I re-compute cost and become profitable
	{
		// put it on the heap :
		out_merge_candidate.gain_J = delta_J;
		out_merge_candidate.merged_complen_bits = merged_complen_bits;
		out_merge_candidate.src1 = i;
		out_merge_candidate.src2 = j;
		out_merge_candidate.src1len = histo1.total;
		out_merge_candidate.src2len = histo2.total;
	}
}

histo_cost_bits_func_type * entropysets_order0_codelen_bits_cpudetect()
{
#if defined(DO_SSE4_ALWAYS)
	return entropysets_order0_codelen_bits_sse4;
#elif defined(DO_SSE4_TEST)
	if ( rrsimd_has_sse4() )
		return entropysets_order0_codelen_bits_sse4;
	else
		return entropysets_order0_codelen_bits;
#else
	// If we don't have anything more specialized, use the default
	return entropysets_order0_codelen_bits;
#endif
}

void merge_entropysets(
	vector_a<entropyset> & histos,
	F32 merge_J_saved, // the J bonus you get from merging (due to saving per-array time)
	F32 min_gain_J, // stop when we cannot beat this gain in J for a merge
	S32 max_histos_target, // stop when num_histos <= max
	histo_cost_bits_func_type * histo_cost_bits_func,
	rrArenaAllocator * arena
	) 
{
	// push all N^2 merges :

	// min_gain_J is always 0 currently
	//	min_gain_J & merge_J_saved are redundant, they offset each other

	// @@ if you make 1000 histos, the N^2 here can be quite large
				
	// assume you gain one huff startup time from merging : // @@@@
	//F32 merge_J_saved = lambda * newLZ_speedfit_huff_time(0); // 13 bytes
	//F32 merge_J_saved = 0.f;
	
	int histos_initial_count = histos.size32();
	
	for(int i=0;i<histos_initial_count;i++)
	{
		entropyset & cur = histos[i];
		cur.complen = (*histo_cost_bits_func)(cur.histo,cur.total);
	}		
			
	vector_a<lazy_merge_candidate> merge_heap_vec;
	int merge_heap_vec_init_size = histos_initial_count * (histos_initial_count-1) / 2;
	rrScopeArenaAlloc merge_heap_vec_alloc( merge_heap_vec_init_size*sizeof(lazy_merge_candidate), arena );
	merge_heap_vec.provide_arena(merge_heap_vec_alloc.m_ptr,merge_heap_vec_alloc.m_size);
	merge_heap_vec.resize( merge_heap_vec_init_size );
	lazy_merge_candidate * merge_heap = merge_heap_vec.data();
	SINTa merge_heap_size = 0;

	for(int i=0;i<histos_initial_count;i++)
	{
		for(int j=i+1;j<histos_initial_count;j++)
		{
			make_lazy_merge_candidate( merge_heap[merge_heap_size], histos[i],i, histos[j],j , merge_J_saved, histo_cost_bits_func);
			merge_heap_size++;
		}
	}
	
	RR_ASSERT( merge_heap_size == merge_heap_vec.size32() );
	
	make_heap(merge_heap,merge_heap+merge_heap_size);
	
	int num_histos_remaining = histos_initial_count;
	
	// while heap, do it :
	while(merge_heap_size > 0 )
	{
		lazy_merge_candidate cur_merge = merge_heap[0];
		popped_heap(merge_heap,merge_heap+merge_heap_size);
		merge_heap_size--;
				
		int i = cur_merge.src1;
		int j = cur_merge.src2;
		
		// is it up to date ?
		if ( histos[i].total != cur_merge.src1len ||
			 histos[j].total != cur_merge.src2len )
		{
			// refresh it
			
			if ( histos[i].total == 0 ||
				 histos[j].total == 0 )
			{
				// gone
				continue;
			}
			else
			{
				make_lazy_merge_candidate( merge_heap[merge_heap_size], histos[i],i, histos[j],j , merge_J_saved, histo_cost_bits_func);
				merge_heap_size++;
				push_heap(merge_heap,merge_heap+merge_heap_size);
				continue;
			}
		}
		
		// check it's up to date :
		RR_ASSERT( histos[i].total == rrSumOfHistogram(histos[i].histo.counts,256) );
		RR_ASSERT( histos[j].total == rrSumOfHistogram(histos[j].histo.counts,256) );
		RR_ASSERT( histos[i].complen == (*histo_cost_bits_func)(histos[i].histo,histos[i].total) );
		RR_ASSERT( histos[j].complen == (*histo_cost_bits_func)(histos[j].histo,histos[j].total) );
		
		// J is in bytes
		
		if ( cur_merge.gain_J < min_gain_J && num_histos_remaining <= max_histos_target )
		{
			// we stop when we are under MAX_NUM_ARRAYS
			//	AND we hit an unprofitable merge
			break;
		}
		
		// merge i & j
		
		histo_add( &(histos[i].histo), histos[i].histo, histos[j].histo );
		histos[i].total += histos[j].total;
		histos[j].total = 0;
		
		RR_DURING_ASSERT( U32 complen_before = histos[i].complen + histos[j].complen );
		
		histos[i].complen = cur_merge.merged_complen_bits;
		RR_ASSERT( cur_merge.merged_complen_bits == (*histo_cost_bits_func)(histos[i].histo,histos[i].total) );
		histos[j].complen = 0;
		
		RR_DURING_ASSERT( S32 complen_gain = complen_before - histos[i].complen );
		RR_DURING_ASSERT( F32 check_gain_J = (complen_gain/8.f + merge_J_saved) );
		// complen_gain can be slightly negative if the J savings make up for it
		RR_ASSERT( check_gain_J >= 0.f || num_histos_remaining > max_histos_target );
		RR_ASSERT( check_gain_J == cur_merge.gain_J );
		
		// all previous merge candidates with i & j are now lazily invalidated
		//	because totals changed
		num_histos_remaining--;
	}
	
	RR_ASSERT( num_histos_remaining >= 1 && num_histos_remaining <= max_histos_target );
	// histos[] now have initial candidates
		
	// histos[] array has holes where merges happened
	// let's densify it :
	int next_histo_dest = 0;
	for(int histo_src=0;histo_src<histos.size32();histo_src++)
	{
		if ( histos[histo_src].total > 0 )
		{
			if ( histo_src != next_histo_dest )
			{
				histos[next_histo_dest] = histos[histo_src];
			}
			next_histo_dest ++;
		}
	}
	RR_ASSERT( next_histo_dest == num_histos_remaining );
	histos.resize(num_histos_remaining);
		
}


void merge_entropysets_for_entropy_arrays(
	vector_a<entropyset> & histos,
	F32 lambda, const OodleSpeedFit * speedfit,
	int target_num_arrays,
	rrArenaAllocator * arena)
{
	F32 second_merge_J_gain = lambda * speedfit_estimate_entropy_array_time(speedfit,0); // you save a huff startup time per merge
	F32 second_merge_min_gain_J = 0.f; // absolute J gain
	// histo_estimate_complen_bits includes 5 byte newlz header and estimate of codelen transmission
	//	so that will automatically go in your gain J
	//	if you used a bit counter that didn't have that, you should add 5 to J_gain
	
	merge_entropysets(histos,second_merge_J_gain,second_merge_min_gain_J, target_num_arrays, histo_estimate_complen_bits,arena);
}

U32 remove_unused_entropysets(
	vector_a<entropyset> & histos)
{
	U32 all_histos_total = 0;
	for LOOPVECBACK(hi,histos)
	{
		all_histos_total += histos[hi].total;
		if ( histos[hi].total == 0 )
		{
			histos[hi] = histos.back();
			histos.pop_back();
		}
	}
	return all_histos_total;
}
	
OODLE_NS_END

