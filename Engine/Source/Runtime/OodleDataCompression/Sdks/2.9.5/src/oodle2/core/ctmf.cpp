// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrbase.h"
#include "rrlog.h"
#include "ctmf.h"

#if OODLE_PLATFORM_HAS_ADVANCED_MATCHERS

#include "cbradutil.h"
#include "templates/rralgorithm.h"
#include "rrlz_getmatchlen.inl"
#include "oodlelzcompressors.h"
#include "matchfinder.h"
#include "longrangematcher.h"

#define CTMF_FAM_MML	4 
//#define CTMF_FAM_MML	3  // <- not really supported

/*
#include "rrsimpleprof.h"
/*/
#include "rrsimpleprofstub.h"
/**/

OODLE_NS_START

/**

CTMF_FindAllMatches :

should be doing match bubble-back and carry forward
	move back match start pos
		need to be finding matches N spots ahead
	carry forward matches
		same offset, length-1 also occurs at next pos

currently locked at MML 4 ; make that variable ?
(use something like t_do_extra_len3_hash but as a runtime variable)

**/

#define SECOND_HASH_LEN	8 // eight seems to be best

class CacheTableMatchFinder : public IncrementalMatchFinder
{
	typedef CTMF<U32,4,SECOND_HASH_LEN,CTMF_FAM_MML> t_CTMF;

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

public:
	CacheTableMatchFinder(const U8 * ubuf, SINTa size, SINTa startRecordingPos, LRMSet * lrms);
	~CacheTableMatchFinder();

	int ProcessChunk(int chunkSize, UnpackedMatchPair * matches, int maxPairs);
};

CacheTableMatchFinder::CacheTableMatchFinder(const U8 * ubuf, SINTa size, SINTa startRecordingPos, LRMSet * lrms)
{
	m_ubuf = ubuf;
	m_raw = ubuf + startRecordingPos;
	m_raw_end = ubuf + size;
	m_size = size;
	m_parse_end_pos = size - 8;
	m_pos = startRecordingPos;
	m_lrms = lrms;

	longMatchPos = 0;
	longMatchEnd = 0;
	longMatchLen = 0;
	longMatchOffs = 0;

	//int table_bits = GetLZMatchTableBits(size,OodleLZ_CompressionLevel_Optimal1,NULL,18,24,18,24);
	S32 log2 = rrIlog2ceil(S32_clampA(size));
	int table_bits = RR_CLAMP(log2,18,24);
	
	ctmf.allocate(table_bits,0,0,NULL,0);

	SINTa maxPreloadLen = startRecordingPos; // all
	ctmf.set_base_and_preload(ubuf,m_raw,maxPreloadLen);
	ctmf.set_next(m_raw);

	LRMScannerWindowed_Init(&scanner,m_lrms,m_raw,m_raw_end,OODLELZ_MAX_OFFSET);
}

CacheTableMatchFinder::~CacheTableMatchFinder()
{
	ctmf.release();
}

int CacheTableMatchFinder::ProcessChunk(int chunkSize, UnpackedMatchPair * outMatches, int maxPairs)
{
	SIMPLEPROFILE_SCOPE_N(ctmf_chunk,chunkSize);

	// Requiring maxPairs >= 2 makes the code easier and is not a serious limitation
	RR_ASSERT(maxPairs >= 2);

	SINTa pos = m_pos;
	SINTa parse_end_pos = m_parse_end_pos;
	SINTa chunkStart = pos;
	SINTa chunkEnd = RR_MIN(pos + chunkSize, parse_end_pos); // last position we can find matches at
	SINTa realChunkEnd = RR_MIN(pos + chunkSize, m_size); // last position we're asked to report matches for
	LRMSet * lrms = m_lrms;

	const U8 * ubuf = m_ubuf;
	const U8 * dictionaryBase = ubuf;
	const U8 * raw_end = m_raw_end;
	const U8 * ptr_matchend = raw_end - 4;

	// if we have a pending long match, resume it first before we try anything else
	if ( longMatchOffs && ( pos < chunkEnd ) )
	{
		pos = ReportLongMatch(outMatches, chunkStart, chunkEnd, pos, maxPairs);
	}

	// @@ do CTMF search at pos+4, and back up match starts where possible
	for(; pos < chunkEnd; pos++)
	{
		const U8 * ptr = ubuf + pos;

		U32 ptr32 = RR_GET32_NATIVE_UNALIGNED(ptr);

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
		int num_matches = 0;				

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

	    // -1 to make 0 high
		U32 posm1 = (U32)pos-1;

		U32 * cur_row = row;
		for(;;) // two hash rows
		{
			S32 prevml = 0;

#ifdef __RADSSE2__
			if (t_CTMF::c_table_depth == 16)
			{
				RAD_ALIGN(U32, offsets[16], 16);
				__m128i vRefHash = _mm_set1_epi32(hash);
				__m128i vRefPos = _mm_set1_epi32(posm1);
				__m128i vOne = _mm_set1_epi32(1);
				__m128i vPosMask = _mm_set1_epi32(CTMF_POS_MASK);
				__m128i vCheckMask = _mm_set1_epi32(CTMF_CHECK_MASK);
				__m128i vOffsBound = _mm_set1_epi32((S32) RR_MIN((UINTa) (ptr - dictionaryBase), CTMF_POS_MASK+1));

				#define CHECKFOUR(i) \
					__m128i rowData##i = _mm_load_si128((const __m128i *) &cur_row[i*4]); \
					/* ((posm1 - row[i]) & CTMF_POS_MASK) + 1) */ \
					__m128i offs##i = _mm_add_epi32(_mm_and_si128(_mm_sub_epi32(vRefPos, rowData##i), vPosMask), vOne); \
					/* (row[i] ^ hash) & CTMF_CHECK_MASK */ \
					__m128i checkbits##i = _mm_and_si128(_mm_xor_si128(rowData##i, vRefHash), vCheckMask); \
					_mm_storeu_si128((__m128i *) (offsets + i*4), offs##i); \
					__m128i offsOutOfRange##i = _mm_cmpgt_epi32(offs##i, vOffsBound); \
					__m128i isInvalid##i = _mm_or_si128(checkbits##i, offsOutOfRange##i); /*nonzero if invalid*/ \
					__m128i vValid##i = _mm_cmpeq_epi32(isInvalid##i, _mm_setzero_si128())

				CHECKFOUR(0);
				CHECKFOUR(1);
				CHECKFOUR(2);
				CHECKFOUR(3);

				__m128i vValid01 = _mm_packs_epi32(vValid0, vValid1);
				__m128i vValid23 = _mm_packs_epi32(vValid2, vValid3);
				int validMask = _mm_movemask_epi8(_mm_packs_epi16(vValid01, vValid23));

				// NOTE(fg): The expected number of bits set in validMask is typically less than 1,
				// so e.g. trying to prefetch ahead or similar is pointless.
				while (validMask)
				{
					int d = rrCtz32(validMask);
					validMask &= validMask - 1;

					U32 offset = offsets[d];
					const U8 * vs_ptr = ptr - offset;

					S32 len;
					#if CTMF_FAM_MML == 4
					len = getmatchlen_mml4_one32_better(ptr32,ptr,vs_ptr,ptr_matchend,prevml);
					#else
					len = getmatchlen_mml3_one32_better(ptr32,ptr,vs_ptr,ptr_matchend,prevml);
					#endif

					if ( len == 0 )
						continue;
					RR_ASSERT( len > prevml );
					prevml = len;

					matches[num_matches].length = len;
					matches[num_matches].offset = offset;
					num_matches++;
				}

				if ( ! t_CTMF::c_do_second_hash )
					break;

				if ( cur_row == row2 )
					break;

				cur_row = row2;
				continue;
			}
#endif
		
			for(int d=0;d<t_CTMF::c_table_depth;d++)
			{
				U32 hash_entry = cur_row[d];
				if ( (hash_entry& CTMF_CHECK_MASK) != (hash&CTMF_CHECK_MASK) )
					continue;
					
				//U32 vs_pos = hash_entry & CTMF_POS_MASK;
				
				// subtract in POS_MASK ring, do +1 to compensate for earlier -1
				U32 offset = ((posm1 - hash_entry)&CTMF_POS_MASK) + 1;
				
				// OODLELZ_MAX_OFFSET bound is guaranteed :
				RR_COMPILER_ASSERT( CTMF_POS_MASK < OODLELZ_MAX_OFFSET );
				//if ( offset >= OODLELZ_MAX_OFFSET )
				//	continue;

				// Need to check if offset goes out of window (can happen on collisions)
				if ( offset > (UINTa)(ptr - dictionaryBase) )
					continue;
				
				// @@ no check for NEWLZ_MIN_OFFSET
				//	trying to make this generic to OodleLZ, not specific to newlz
				// -> would it help?
				
				const U8 * vs_ptr = ptr - offset;
				
				S32 len;
				#if CTMF_FAM_MML == 4
				len = getmatchlen_mml4_one32_better(ptr32,ptr,vs_ptr,ptr_matchend,prevml);
				#else
				len = getmatchlen_mml3_one32_better(ptr32,ptr,vs_ptr,ptr_matchend,prevml);
				#endif
							
				if ( len == 0 )
					continue;
				RR_ASSERT( len > prevml );
				prevml = len;
								
				matches[num_matches].length = len;
				matches[num_matches].offset = offset;
				num_matches++;
			}
			
			if ( ! t_CTMF::c_do_second_hash )
				break;
				
			if ( cur_row == row2 )
				break;
			
			cur_row = row2;
		}
		
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
		
		stdsort(matches,matches+num_matches,UnpackedMatchPair_Sort_HighLength_then_LowOffset());
		
		//*
		// remove duplicates : (this helps speed overall)
		//  (can get dupes due to 2nd hash)
		UnpackedMatchPair * matches_end = unique(matches,matches+num_matches,
			//UnpackedMatchPair_Equals());
			UnpackedMatchPair_EqualLength());
		num_matches = (int)( matches_end - matches );
		/**/
		
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

	// finish up with nones :
	if (pos != realChunkEnd)
	{
		UnpackedMatchPair * matchPairs = outMatches + (pos - chunkStart)*maxPairs;
		for (; pos < realChunkEnd; pos++)
		{
			matchPairs[0].length = 0;
			matchPairs += maxPairs;
		}
	}

	m_pos = pos;
	return (int) ( realChunkEnd - chunkStart );
}

SINTa CacheTableMatchFinder::ReportLongMatch(UnpackedMatchPair * outMatches, SINTa chunkStart, SINTa chunkEnd, SINTa pos, int maxPairs)
{
	// Your pos better be inside the chunk you're requesting
	RR_ASSERT( pos >= chunkStart && pos < chunkEnd );

	// You're only supposed to call this when inside a long match
	RR_ASSERT( pos >= longMatchPos && pos < longMatchEnd );
	RR_ASSERT( longMatchOffs != 0 );

	// Intersect long match extent with current chunk
	SINTa intersectMin = RR_MAX(longMatchPos + 1, chunkStart); // longMatchPos itself was filled out regularly
	SINTa intersectMax = RR_MIN(longMatchEnd, chunkEnd);

	// Default to reporting no matches for most positions
	for (SINTa i = intersectMin; i < intersectMax; i++)
		outMatches[(i - chunkStart) * maxPairs].length = 0;

	// put 1 longest match carried forward :

	// Report a truncated match at pos+1
	if (longMatchPos + 1 >= intersectMin && longMatchPos + 1 < intersectMax)
	{
		UnpackedMatchPair * curOutMatch = &outMatches[(longMatchPos + 1 - chunkStart) * maxPairs];
		curOutMatch[0].length = longMatchLen - 1;
		curOutMatch[0].offset = longMatchOffs;
		curOutMatch[1].length = 0; // one match only
	}

	SINTa step = 4; // must be pow2!
	//SINTa step = 64; // must be pow2!
	
	// Align start up to be a multiple of step from longMatchPos
	// this is to make sure we behave the same way the original code did,
	// which reported matches at
	//   longMatchPos + 1*step,
	//   longMatchPos + 2*step
	// etc., even if the match spans chunk boundaries.
	SINTa loopStart = RR_MAX(longMatchPos + 2, intersectMin);

	// loopStart - longMatchPos is how far we are from the start of the match;
	// we want to go forward by
	//   (step - (loopStart - longMatchPos)) % step
	// bytes (i.e. the number of bytes to make the offset from longMatchPos
	// a multiple of step), when computed using Euclidean (non-negative) mod;
	// step % step == 0, so this is equivalent to
	//    (longMatchPos - loopStart) % step
	loopStart += (longMatchPos - loopStart) & (step - 1);

	// Report this match at the multiples of step along the way
	for (SINTa i = loopStart; i < intersectMax; i += step)
	{
		UnpackedMatchPair * curOutMatch = &outMatches[(i - chunkStart) * maxPairs];
		curOutMatch[0].length = S32( longMatchEnd - i );
		curOutMatch[0].offset = longMatchOffs;
		curOutMatch[1].length = 0; // one match only
	}

	// If this chunk contains the end of the long match, we have extra processing
	if ( intersectMax == longMatchEnd )
	{
		// Do the insert unless we run a risk of running off the end of the buffer
		if ( longMatchEnd < m_parse_end_pos )
		{
			ctmf.step_and_insert(m_ubuf + longMatchPos,longMatchLen);
			RR_ASSERT( ctmf.m_next_ptr == m_ubuf + longMatchEnd );
		}

		if ( m_lrms )
		{
			// reset scanner for jump-ahead
			LRMScannerWindowed_Init(&scanner,m_lrms,m_ubuf + longMatchEnd,m_raw_end,OODLELZ_MAX_OFFSET);
		}

		// long match is done
		longMatchOffs = 0;
	}

	return intersectMax;
}

IncrementalMatchFinder * CTMF_CreateMatchFinder(const U8 * ubuf, SINTa size,
	SINTa startRecordingPos RADDEFAULTX(0),
	LRMSet * lrms RADDEFAULTX(NULL),
	OodleLZ_Jobify jobify RADDEFAULTX(OodleLZ_Jobify_Default),
	void * jobifyUserPtr RADDEFAULTX(NULL))
{
	return OodleNew4(CacheTableMatchFinder, ubuf, size, startRecordingPos, lrms);
}

OODLE_NS_END

#else

EXPORT_SOME_CRAP(cmtf);

#endif // OODLE_PLATFORM_HAS_ADVANCED_MATCHERS
