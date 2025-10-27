// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "suffixtrie.h"

#include "rrbase.h"
#include <rrlog.h>
#include <rrlogutil.h>
#include <templates/rrvector.h>
#include <templates/rrpool.h>
#include <templates/rralgorithm.h>
#include <stdlib.h>
//#include "LeakTracker.h"
#include <limits.h>

#include "matchfinder.h"
#include "longrangematcher.h"
#include "cbradutil.h"
#include "rrbits.h"
#include "suffixtriematcher.h"
#include "oodlejob.h"

#include "threadprofiler.h"
#include "rrprefetch.h"

#ifdef __RADSSE2__
#include <emmintrin.h>
#endif

//extern int g_wrt_hack_escape; // for test_wrt experiment


/*
// for Suffix3 test :
#include <templates/rrhashfunction.h>
#include <templates/rrhashtable.h>
*/

#define DO_LOG_MEMORY_USE 0
//#define DO_LOG_MEMORY_USE 1

// use "getmatchlen" for steps to children
//	 (eg. runs of 8 bytes instead of byte-at-a-time)
//	much faster in degenerate cases
//	but a little slower on typical files
//#define DO_GETMATCHLEN_CHILD_DESCENT	1
#define DO_GETMATCHLEN_CHILD_DESCENT	0

#if DO_GETMATCHLEN_CHILD_DESCENT
#include "rrlz_getmatchlen.inl"
#endif

#define ST_LRM_GOOD_ENOUGH_LEN	256

// don't use straight pow2s, always reserve a bit for headers etc.
//#define ST_POOL_BYTES_PER_CHUNK	(65400)
#define ST_POOL_BYTES_PER_CHUNK	(262000)
//#define ST_POOL_BYTES_PER_CHUNK	(1048000)

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

/*
RR_NAMESPACE_START
template <>
inline hash_type hash_function<int64>(const int64 & t)
{
	return rrHash64( (U64)t );
}
RR_NAMESPACE_END
*/

/*

24 bit index limit is not really a bad thing
becaues when you hit that you are using over 2 GB of mem
so not something you want to do even if you could
	-> this is not true, you don't necessarily get near 2G

file splitting is the way to go

*/

/*


NOTE / FIXME but not important
	SuffiXTrie was reading past the end of buffer
	so I shortened maxml to be one before end of buffer
			SINTa maxml = size-pos -1;
	which is why I have that hack in LZHLW to detect whole-quantum matches

-> I could probably fix this now that I have special end-of-buffer handling
	and then remove the outer hacks

*/

// For testing you can now not define this at all and it's infinite
//	-> when this is not defined, ST is an exact matcher
//#define LIMIT_PARENT_STEPS	(32)
#define LIMIT_PARENT_STEPS	(16)
// newlzf ST vs. LzFind speed on webster :
// I tried disabling LIMIT_PARENT_STEPS
// to see if the problem was ST not having lowest offset
// -> no big effect, this isn't it

#if OODLE_PLATFORM_HAS_ADVANCED_MATCHERS
OODLE_NS_START

#if DO_LOG_MEMORY_USE
static const char * strcomma(S64 number)
{
	static char buf[64];
	rrsprintfcommas(buf,number);
	return buf;
}
#endif

//=====================================================

namespace SuffixTrie2
{
// unbounded window size
#define SUFFIX2_NUM_FIRST_BYTES 3 // this is runtime selectable in the regular match finder now
#include "suffixtrie2.inl"
};

//=====================================================

IncrementalMatchFinder * SuffixTrie_CreateMatchFinder(const U8 * ubuf,SINTa sizeA,
		SINTa startRecordingPosA,
		LRMSet * lrms,
		OodleLZ_Jobify jobify,
		void * jobifyUserPtr)
{
	RR_ASSERT_ALWAYS( sizeA > 0 && sizeA < RR_S32_MAX );

	S32 size = S32_checkA(sizeA);
	S32 startRecordingPos = S32_checkA(startRecordingPosA);
	RR_ASSERT( startRecordingPos >= 0 && startRecordingPos < size );

	return OodleNewT(SuffixTrie2::SuffixTrie2MatchFinder)(ubuf,size,startRecordingPos,lrms,jobify,jobifyUserPtr, size <= SuffixTrie1_MaxSize ? 2 : 3);
}

//===========================================================

OODLE_NS_END
#endif // OODLE_PLATFORM_HAS_ADVANCED_MATCHERS

