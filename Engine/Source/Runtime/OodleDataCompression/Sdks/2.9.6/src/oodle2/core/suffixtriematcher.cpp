// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "suffixtriematcher.h"

#include <templates/rrvector.h>
#include <templates/rralgorithm.h>
#include <templates/rrpool.h>
#include <stdlib.h>
//#include "LeakTracker.h"
#include <limits.h>

#include "longrangematcher.h"
#include "matchfinder.h"
#include "cbradutil.h"
#include "rrbits.h"
#include "oodlejob.h"
#include "oodlelzpub.h" // OodleLZ_Jobify

#include "threadprofiler.h"
#include "rrprefetch.h"

#ifdef __RADSSE2__
#include <emmintrin.h>
#endif

#define DO_LOG_MEMORY_USE 0

#define DO_GETMATCHLEN_CHILD_DESCENT	0

#define ST_LRM_GOOD_ENOUGH_LEN	256

// 64k with a few bytes shaved off for headers :
//#define ST_POOL_BYTES_PER_CHUNK	(65400)
// 1048576
#define ST_POOL_BYTES_PER_CHUNK	(1048000)

#undef ASSERT
#define ASSERT RR_ASSERT
#undef COMPILER_ASSERT
#define COMPILER_ASSERT RR_COMPILER_ASSERT
#undef MAX
#define MAX RR_MAX
#undef MIN
#define MIN RR_MIN
#ifndef uint32
#define uint32 U32
#endif
#define int32 S32
#define int64 S64
#define uint64 U64
#define uint8 U8

// enable :
//#define HEAVY_ASSERT RR_ASSERT

// else disable :
#ifndef HEAVY_ASSERT
#define HEAVY_ASSERT(exp)
#endif

#define LIMIT_PARENT_STEPS	(32)

#if 0 // for test_wrt experiment
extern int g_wrt_hack_escape;
int g_wrt_hack_escape = 0;
#endif

//=====================================================================

#if OODLE_PLATFORM_HAS_ADVANCED_MATCHERS
OODLE_NS_START

namespace SuffixTrieForMatcher
{
// SuffixTrieMatcher must have MML 2
#define SUFFIX2_NUM_FIRST_BYTES 2
#include "suffixtrie2.inl"
};

struct SuffixTrieMatcher : public SuffixTrieForMatcher::SuffixTrieObject
{
};

SuffixTrieMatcher * SuffixTrieMatcher_Create(const U8 * ubuf,SINTa sizeA,SuffixTrieSortData * psuffixsortdata)
{
	S32 size = S32_checkA(sizeA);
	SuffixTrieMatcher * stm = OodleNew(SuffixTrieMatcher);
	stm->Build(ubuf,size,psuffixsortdata);
	return stm;
}

void SuffixTrieMatcher_Destroy(SuffixTrieMatcher * stm)
{
	OodleDelete(stm);
}

S32 SuffixTrieMatcher_LookupMatches(const SuffixTrieMatcher * stm,const U8 * ptr,int maxml, vector<SuffixTrieMatch> * pMatches)
{
	pMatches->clear();
	stm->LookupMatches(ptr,maxml,pMatches);
	return pMatches->size32();
}

bool SuffixTrieMatcher_LookupSuffix(const SuffixTrieMatcher * stm,const U8 * ptr,int maxml, SuffixTrieMatch * pMatch,BitFlags256 * pFollowExcludes)
{
	return stm->LookupSuffix(ptr,maxml,pMatch,pFollowExcludes);
}

OODLE_NS_END
#endif // OODLE_PLATFORM_HAS_ADVANCED_MATCHERS
