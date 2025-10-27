// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlebase.h"

#if OODLE_PLATFORM_HAS_ADVANCED_MATCHERS

#include "templates/rrvector.h"
#include "rrcompressutil.h"

OODLE_NS_START

struct SuffixTrieMatcher;

struct SuffixTrieMatch
{
	const U8 * ptr;
	S32 length;
	S32 count; 
	S32 suffix_index;
};

struct SuffixTrieSortData
{
	int count;
	vector<int> vsort;
	vector<int> o0_cumcounts;
	vector<BitFlags256> o0_excludes;
	
	// @@ for test_wrt experiment
	const U8 * best_word_ptr;
	int best_word_len;
	
	void swap(SuffixTrieSortData & rhs)
	{
		RR_NAMESPACE_PRE swap(count,rhs.count);
		vsort.swap(rhs.vsort);
		o0_cumcounts.swap(rhs.o0_cumcounts);
		o0_excludes.swap(rhs.o0_excludes);
		
		RR_NAMESPACE_PRE swap(best_word_ptr,rhs.best_word_ptr);
		RR_NAMESPACE_PRE swap(best_word_len,rhs.best_word_len);
	}
};

SuffixTrieMatcher * SuffixTrieMatcher_Create(const U8 * ubuf,SINTa size,SuffixTrieSortData * psuffixsortdata = NULL);
void SuffixTrieMatcher_Destroy(SuffixTrieMatcher * stm);
S32  SuffixTrieMatcher_LookupMatches(const SuffixTrieMatcher * stm,const U8 * ptr,int maxml, vector<SuffixTrieMatch> * pMatches);
bool SuffixTrieMatcher_LookupSuffix(const SuffixTrieMatcher * stm,const U8 * ptr,int maxml, SuffixTrieMatch * pMatch, BitFlags256 * pFollowExcludes = NULL);

OODLE_NS_END
#endif // OODLE_PLATFORM_HAS_ADVANCED_MATCHERS

