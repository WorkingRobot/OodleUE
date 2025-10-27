// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlecore.h"

OODLE_NS_START

// CountHistoArrayU8 ;
//	if addOn , add onto counts[] ; else sets counts
void CountHistoArrayU8(const U8 * rawArray,SINTa rawLen,U32 * counts,int numSymbols,bool addOn = false);
	
void CountHistoArrayU8_Simple
	(const U8 * rawArray,SINTa rawLen,
	U32 * counts,int numSymbols,bool addOn = false);
	
U32 rrSumOfHistogram(const U32 * counts,int numCounts);

// this is the entropy PER SYMBOL ; that is, it's the number of bits divided by the number of symbols
F32 rrEntropyOfHistogram( const U32 * counts,int numCounts);
F32 rrEntropyOfHistogramT(const U32 * counts,int numCounts,U32 tot);
//  rrCodeLenOfHistogramT = entropy * sum
//	rrCodeLenOfHistogramT is in *bits*
//	"T" means total is passed in so the summing can be avoided
U64 rrCodeLenOfHistogramT(const U32 * counts,int numCounts,U32 sumCounts);

U64 CodeLenOfArrayU8(const U8 * rawArray,SINTa rawLen);

// helper Histo for common byte alphabet case; initialize to sero :

struct Histo256
{
	U32	counts[256];
};

//U32 CodeLenOfHistogram(const U32 * histogram, const U8 * codeLenTable, int alphabet);

U32 CodeLenOfHistogram256(const U32 * histogram, S32 histoSum, const U8 * codeLenTable,int alphabet);

// code counts[] using codelens from counts_for_codelen[]
//	eg. for tans normalized counts
U64 rrCodeLenOfHistogramPow2External(const U32 * counts,int numCounts,const U32 * counts_for_codelen,U32 counts_for_codelen_shift);

//=======================================================

#ifdef RR_DO_ASSERTS

bool verify_histo(const U8 * ptr,SINTa len,const U32 * counts,int numSymbols);
bool histo_check(const U8 * ptr, SINTa len,const Histo256 & histo);
	
#else

#define verify_histo use me in asserts only
#define histo_check  use me in asserts only

#endif

// histo_slide : move [ptr,len] from histo "from" to "to"
void histo_slide(const U8 * ptr, SINTa len,
	Histo256 & from,
	Histo256 & to);
			
void histo_add(const U8 * ptr, SINTa len,
	Histo256 & histo);

void histo_remove(const U8 * ptr, SINTa len,
	Histo256 & histo);
	
void histo_count(Histo256 * pTo,const U8 * ptr, SINTa len);

U32 histo_sum(Histo256 & histo);

// to = lhs + rhs
void histo_add(Histo256 * pTo, const Histo256 & lhs, const Histo256 & rhs);
	
// to = lhs - rhs
void histo_sub(Histo256 * pTo, const Histo256 & lhs, const Histo256 & rhs);

//=======================================================

OODLE_NS_END
