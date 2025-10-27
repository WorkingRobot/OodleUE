// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "templates/rralgorithm.h"
#include "rrlz_getmatchlen.inl"
#include "oodlelzcompressors.h"

OODLE_NS_START

template<typename TMatchEvaluator>
RADFORCEINLINE SINTa CacheTableMatchFinder::ProcessChunkTemplate(SINTa pos, SINTa chunkStart, SINTa chunkEnd, UnpackedMatchPair * outMatches, int maxPairs)
{
	SINTa parse_end_pos = m_parse_end_pos;
	LRMSet * lrms = m_lrms;

	const U8 * ubuf = m_ubuf;
	const U8 * dictionaryBase = ubuf;
	const U8 * raw_end = m_raw_end;
	const U8 * ptr_matchend = raw_end - 4;

	TMatchEvaluator evaluator;

	// @@ do CTMF search at pos+4, and back up match starts where possible
	for(; pos < chunkEnd; pos++)
	{
		const U8 * ptr = ubuf + pos;

		RR_ASSERT( ptr == ctmf.m_next_ptr );
		U32 * row = ctmf.m_next_row;
		U32 hash = ctmf.m_next_hash;
		U32 * row2 = ctmf.m_next_row_second;
		//U32 hash2 = ctmf.next_hash_second;
		
		// prefetch more than 1 byte ahead, we're expecting misses all the way to
		// main mem - we end up doing two hash calcs per byte here, but that's OK
		//
		// PF of 8 here vs. old default of 1: -16% FindAllMatches runtime
		// on i7-7920X.
		static const SINTa kPrefetchDist = 8;
		if (kPrefetchDist == 1)
		{
			ctmf.prefetch_next(ptr+1);
		}
		else
		{
			if ( pos + kPrefetchDist < parse_end_pos )
				ctmf.prefetch_next(ptr+kPrefetchDist);
			ctmf.set_next(ptr+1);
		}
		
		RR_ASSERT( ctmf.m_next_ptr == ptr+1 );
		
		RR_ASSERT( pos == rrPtrDiff(ptr - ctmf.m_base_ptr) );

		#define CTMF_MAX_MATCH_CANDIDATES	(t_CTMF::c_table_depth*2 + 1)
		UnpackedMatchPair matches[CTMF_MAX_MATCH_CANDIDATES];
		SINTa num_matches = 0;				

		if ( lrms )
		{
			SINTa lrm_offset;
			S32 lrm_length = LRMScannerWindowed_FindMatchAndRoll(&scanner,ptr,raw_end,&lrm_offset);
				
			// LRM_MIN_MATCH_LEN == 8 currently
			if ( lrm_length > 0 )
			{
				matches[0].length = lrm_length;
				matches[0].offset = S32_checkA(lrm_offset);
				num_matches++;
			}
		}

		const SINTa neg_offs_limit = rrPtrDiff(dictionaryBase - ptr);
		num_matches = evaluator.EvalMatches(matches,num_matches,pos,row,row2,hash,ptr,ptr_matchend,neg_offs_limit);
		
		// now update the hash rows :
		ctmf.insert(row,(U32)pos,hash);
		if ( t_CTMF::c_do_second_hash )
			ctmf.insert(row2,(U32)pos,hash);
		
		RR_ASSERT( ctmf.m_next_ptr == ptr+1 );

		UnpackedMatchPair * curOutMatch = outMatches + (pos - chunkStart)*maxPairs;
		
		if ( num_matches == 0 )
		{
			curOutMatch[0].length = 0;
			continue;
		}
		
		if ( num_matches > 1 )
		{
			//stdsort(matches,matches+num_matches,UnpackedMatchPair_Sort_HighLength_then_LowOffset());

			// insertion sort of matches
			auto match_less = UnpackedMatchPair_Sort_HighLength_then_LowOffset();
			for (SINTa i = 1; i < num_matches; i++)
			{
				UnpackedMatchPair cur = matches[i];

				SINTa j = i;
				while ( j > 0 && match_less(cur, matches[j - 1]) )
				{
					matches[j] = matches[j - 1];
					--j;
				}

				matches[j] = cur;
			}

			//*
			// remove duplicates : (this helps speed overall)
			//  (can get dupes due to 2nd hash)
			UnpackedMatchPair * matches_end = unique(matches,matches+num_matches,
				//UnpackedMatchPair_Equals());
				UnpackedMatchPair_EqualLength());
			num_matches = (int)( matches_end - matches );
			/**/
		}
		
		int longestml = matches[0].length;
		
		num_matches = RR_MIN(num_matches,maxPairs);
		memcpy(curOutMatch, matches, num_matches*sizeof(*matches));
		if (num_matches < maxPairs)
			curOutMatch[num_matches].length = 0;
		
		// skip ahead for long matches
		// you want a slightly larger threshold than your outer skip-len here :
		if ( longestml >= 77 ) //128 )
		{
			// This needs to be resumeable in case a long match crosses a chunk boundary, so we use
			// a helper func to handle this
			longMatchPos = pos;
			longMatchEnd = pos + longestml;
			longMatchLen = longestml;
			longMatchOffs = matches[0].offset;
			pos = ReportLongMatch(outMatches, chunkStart, chunkEnd, pos, maxPairs) - 1; // -1 because of +1 loop above
		}
	}

	return pos;
}

OODLE_NS_END

