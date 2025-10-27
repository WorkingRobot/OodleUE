// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlebase.h"
#include "oodlelzpub.h" // OodleLZ_Jobify

enum { SuffixTrie1_MaxSize = (1UL<<23) }; // 24 bit indices 

enum { SuffixTrie2_MaxSize = (1UL<<30) }; // S32 indices 

/*

ubuf from [0,startRecordingPos] is used to buid a tree with no output

from [startRecordingPos,size] we output matches at each pos to toMem

(toMem + pos * stride) receives the matches at pos

the matches are a series of pairs, [length,offset] in descending order (longest match first)
  a lengths of 0 terminates the sequence before maxPairs

toStride must be >= maxPairs*2*sizeof(int)

===========================================

WARNING :

SuffixTrie does *NOT* give you the true match len of any match except the *longest* one.
All the lower-len matches may be reported with lengths shorter than their true match length.

*/

// the output data is like this :
//struct SuffixTrie_MatchPair { S32 length; S32 offset; };

struct LRMSet;
class IncrementalMatchFinder;

#if OODLE_PLATFORM_HAS_ADVANCED_MATCHERS

//#include "templates/rrvector.h"
//#include "rrCompressUtil.h"

OODLE_NS_START

IncrementalMatchFinder * SuffixTrie_CreateMatchFinder(const U8 * ubuf,SINTa size,
		SINTa startRecordingPos RADDEFAULT(0),
		LRMSet * lrms RADDEFAULT(NULL),
		OodleLZ_Jobify jobify RADDEFAULT(OodleLZ_Jobify_Default),
		void * jobifyUserPtr RADDEFAULT(NULL));
		
// matchPairWindows can be NULL
//	otherwise they should be of size [maxPairs] and descending
//	matchPairWindows[0] should be infinite
//	matchPairWindows[i+1] <= matchPairWindows[i]	

OODLE_NS_END
#endif // OODLE_PLATFORM_HAS_ADVANCED_MATCHERS
