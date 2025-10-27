// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "histogram.h"
#include "templates/rrvector_a.h"

OODLE_NS_START

struct OodleSpeedFit;

//===============================================================================
// entropyset codelen stuff :

// codelens in U16 to make sure the trellis refinement fits in L1
struct entropyset_codelens_U16_256
{
	U16	codelen[256];
};

// "cl" ("codelen") is in bits scaled up by ENTROPYSET_CODELEN_ONE_BIT_SHIFT (fixed point)
enum { ENTROPYSET_CODELEN_ONE_BIT_SHIFT = 8 };
enum { ENTROPYSET_CODELEN_ONE_BIT = 1<<ENTROPYSET_CODELEN_ONE_BIT_SHIFT };

// ENTROPYSET_CODELEN can fit a 128k chunk in a U32
//	17 bits + 4 bit (max codelen 16 bits) + 8 = 29 bits

// WARNING : histo counts over 20 bits (1M) can't be used with EntropySetCodelens!  won't fit in U32 !

// -> ENTROPYSET_CODELEN_ONE_BIT is now scaled so that the *relative* cost always fits in a U16 for trellis
//	that is, the cost of switch_histo + cost of next symbol must fit in U16
//		(-> actually must fit in S16 for SSE ; 15 bits)

// higher than any valid CL :
enum { ENTROPYSET_SYM_INVALID_HIGH_CL = 14<<ENTROPYSET_CODELEN_ONE_BIT_SHIFT };

// NEWLZ_HUFF_CODELEN_LIMIT == 11
// NEWLZ_TANS_L_BITS_MAX == 11

enum { ENTROPYSET_SYM_NOT_PRESENT_CL = 11<<ENTROPYSET_CODELEN_ONE_BIT_SHIFT };
//enum { ENTROPYSET_SYM_NOT_PRESENT_CL = 12<<ENTROPYSET_CODELEN_ONE_BIT_SHIFT }; // > ENTROPYSET_SYM_PRESENT_MAX_CL
enum { ENTROPYSET_SYM_PRESENT_MAX_CL = 11<<ENTROPYSET_CODELEN_ONE_BIT_SHIFT };


// used in calculation of order0_codelen_bits
// invSum is (1<<ENTROPYSET_INVSUM_SHIFT)/sum
//
// want this fairly high to reduce truncation error
enum { ENTROPYSET_INVSUM_SHIFT = 30 };

// compute codelen of sending [ptr,len] bytes with histo1 & histo2 probability models
//	and return the difference
//	(returns an "entropyset codelen")
S32 entropysets_codelen_delta(const U8 * ptr, SINTa len,
	const Histo256 & histo1,SINTa len1,
	const Histo256 & histo2,SINTa len2);
	
// entropysets_codelen_of_array : codelen to code the range [ptr,len] using the counts in [histo]
//	this is a cross-cost
//	does not consider how adding [ptr,len] to histo changes its self-cost
S32 entropysets_codelen_of_array(const U8 * ptr, SINTa len,
	const Histo256 & histo,SINTa histo_sum);
	
// return total codelen of "histo" under the codelens in "codelens"
//	this is a cross-cost
S32 entropysets_cross_codelen(const Histo256 & histo,U32 histo_total,const entropyset_codelens_U16_256 & codelens);

// entropysets_self_codelen does not include any cost to send counts or other nonsense
//	it's just count[sym] * codelen[sym]
S32 entropysets_self_codelen(const Histo256 & histo,SINTa histo_sum);

// entropysets_codelen_of_small_array
//	instead of histogramming and using entropysets_self_codelen
//	faster on small arrays
U32 entropysets_codelen_of_small_array(const U8 * ptr, SINTa len,const entropyset_codelens_U16_256 & codelens);

// fill "codelens" from histo
void entropysets_histo_to_codelens(const Histo256 & histo,SINTa histo_sum,
	entropyset_codelens_U16_256 * pcodelens);
	
// does not include any header bytes of codelen transmission cost
//	just an order0 entropy (scaled by sumCounts)
//	note return value is in *bits* not "entropyset codelen"
U32 entropysets_order0_codelen_bits(const Histo256 & histo,SINTa sumCounts);

// SSE4 version. Only present on x86 targets.
U32 entropysets_order0_codelen_bits_sse4(const Histo256 & histo,SINTa sumCounts);

//===============================================================================

struct entropyset
{
	// you provide :
	Histo256	histo;
	U32			total;
	
	// merge_entropysets scratch :
	U32			complen; // == histo_cost_bits_func_type() called on (histo,total)					
};

typedef U32 (histo_cost_bits_func_type)(const Histo256 & histo,SINTa sumCounts);

histo_cost_bits_func_type * entropysets_order0_codelen_bits_cpudetect();

struct rrArenaAllocator;

void merge_entropysets(
	vector_a<entropyset> & histos,
	F32 merge_J_saved, // the J bonus you get from merging (due to saving per-array time)
	F32 min_gain_J, // stop when we cannot beat this gain in J for a merge
	S32 max_histos_target, // stop when num_histos <= max
	histo_cost_bits_func_type * histo_cost_bits_func,
	rrArenaAllocator * arena
	);

void merge_entropysets_for_entropy_arrays(
	vector_a<entropyset> & histos,
	F32 lambda, const OodleSpeedFit * speedfit,
	int target_num_arrays,
	rrArenaAllocator * arena);
	
// returns sum of all histo totals
U32 remove_unused_entropysets(
	vector_a<entropyset> & histos);
	
//===============================================================================

OODLE_NS_END
