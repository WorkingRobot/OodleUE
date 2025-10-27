// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrbase.h"

#if OODLE_PLATFORM_HAS_ADVANCED_MATCHERS

#include "ctmf.h"
#include "matchfinder.h"
#include "longrangematcher.h"

OODLE_NS_START

// Original CTMF match finder behavior: reset prevml=0 between the first and
// second hash lookup. In my testing this produces a lot of fairly low-quality
// matches that do basically nothing for compression ratio and waste time.
// Therefore, disable for now.
#define CTMF_RESET_PREVML_BETWEEN_ROWS 0

class CacheTableMatchFinder : public IncrementalMatchFinder
{
	static constexpr int SECOND_HASH_LEN = 8; // eight seems to be best

	typedef CTMF<U32,4,SECOND_HASH_LEN,4> t_CTMF;

	const U8 * m_ubuf;
	const U8 * m_raw;
	const U8 * m_raw_end;
	SINTa m_size;
	SINTa m_pos;
	SINTa m_parse_end_pos;
	LRMSet * m_lrms;

	SINTa longMatchPos;
	SINTa longMatchEnd;
	int longMatchLen;
	int longMatchOffs;

	t_CTMF ctmf;
	LRMScannerWindowed scanner;

	SINTa ReportLongMatch(UnpackedMatchPair * outMatches, SINTa chunkStart, SINTa chunkEnd, SINTa pos, int maxPairs);

	template<typename TMatchEvaluator>
	SINTa ProcessChunkTemplate(SINTa pos, SINTa chunkStart, SINTa chunkEnd, UnpackedMatchPair * outMatches, int maxPairs);

	// Always exists
	struct MatchEvalGeneric;
	SINTa ProcessChunkGeneric(SINTa pos, SINTa chunkStart, SINTa chunkEnd, UnpackedMatchPair * outMatches, int maxPairs);

	// Only on SSE2+ targets (x86)
	struct MatchEvalSSE2;
	SINTa ProcessChunkSSE2(SINTa pos, SINTa chunkStart, SINTa chunkEnd, UnpackedMatchPair * outMatches, int maxPairs);

	// Only on AVX2+ targets (x86)
	struct MatchEvalAVX2;
	SINTa ProcessChunkAVX2(SINTa pos, SINTa chunkStart, SINTa chunkEnd, UnpackedMatchPair * outMatches, int maxPairs);

	// Only on NEON64 targets (ARM AArch64)
	struct MatchEvalNEON64;
	SINTa ProcessChunkNEON64(SINTa pos, SINTa chunkStart, SINTa chunkEnd, UnpackedMatchPair * outMatches, int maxPairs);

public:
	CacheTableMatchFinder(const U8 * ubuf, SINTa size, SINTa startRecordingPos, LRMSet * lrms, rrArenaAllocator * arena);
	~CacheTableMatchFinder();

	void Release();
	int ProcessChunk(int chunkSize, UnpackedMatchPair * matches, int maxPairs);
};

OODLE_NS_END

#endif // OODLE_PLATFORM_HAS_ADVANCED_MATCHERS

