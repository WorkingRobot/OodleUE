// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrbase.h"
#include "rrlog.h"
#include "ctmf.h"

#if OODLE_PLATFORM_HAS_ADVANCED_MATCHERS

#include "ctmf_internal.h"
#include "cbradutil.h"
#include "cpux86.h"
#include "ctmf_internal.inl"

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

CacheTableMatchFinder::CacheTableMatchFinder(const U8 * ubuf, SINTa size, SINTa startRecordingPos, LRMSet * lrms, rrArenaAllocator * arena)
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
	
	ctmf.allocate(table_bits,0,0,arena,0);

	SINTa maxPreloadLen = startRecordingPos; // all
	ctmf.set_base_and_preload(ubuf,m_raw,maxPreloadLen);
	ctmf.set_next(m_raw);

	LRMScannerWindowed_Init(&scanner,m_lrms,m_raw,m_raw_end,OODLELZ_MAX_OFFSET);
}

CacheTableMatchFinder::~CacheTableMatchFinder()
{
	ctmf.release();
}

void CacheTableMatchFinder::Release()
{
	if (ctmf.m_hash_table_arena)
	{
		rrArenaAllocator* arena = ctmf.m_hash_table_arena;
		destruct_virtual(this);
		arena->Free(this, sizeof(*this));
	}
	else
	{
		OodleDeleteVirtual(this);
	}
}

struct CacheTableMatchFinder::MatchEvalGeneric
{
	RADFORCEINLINE SINTa EvalMatches(UnpackedMatchPair * matches, SINTa num_matches, SINTa pos, const U32 * row, const U32 * row2, U32 hash, const U8 * ptr, const U8 * ptr_matchend, SINTa neg_offs_limit)
	{
		typedef CacheTableMatchFinder::t_CTMF t_CTMF;

		const U32 * cur_row = row;
		SINTa prevml = 3; // forces MML=4

		for(;;) // two hash rows
		{
		#if CTMF_RESET_PREVML_BETWEEN_ROWS
			prevml = 3;
		#endif

			for(int d=0;d<t_CTMF::c_table_depth;d++)
			{
				const U32 hash_entry = cur_row[d];
				if ( (hash_entry & CTMF_CHECK_MASK) != (hash & CTMF_CHECK_MASK) )
					continue;
					
				// Determine the negative offset, then check if that offset
				// is outside our match window.
				const SINTa neg_offset = t_CTMF::resolve_neg_offset(hash_entry, (U32)pos);
				
				// OODLELZ_MAX_OFFSET bound is guaranteed :
				static_assert(CTMF_POS_MASK < OODLELZ_MAX_OFFSET, "offset bound too small");

				// Need to check if offset goes out of window (can happen on collisions)
				if ( neg_offset < neg_offs_limit )
					continue;
				
				SINTa len;
				if ( ! havematch_better(ptr,ptr + neg_offset,ptr_matchend,prevml,&len) )
					continue;

				RR_ASSERT( len > prevml );
				prevml = len;
								
				matches[num_matches].length = S32(len);
				matches[num_matches].offset = S32(-neg_offset);
				num_matches++;
			}
			
			if ( ! t_CTMF::c_do_second_hash )
				break;
				
			if ( cur_row == row2 )
				break;
			
			cur_row = row2;
		}

		return num_matches;
	}
};

SINTa CacheTableMatchFinder::ProcessChunkGeneric(SINTa pos, SINTa chunkStart, SINTa chunkEnd, UnpackedMatchPair * outMatches, int maxPairs)
{
	return ProcessChunkTemplate<MatchEvalGeneric>(pos, chunkStart, chunkEnd, outMatches, maxPairs);
}

#ifdef __RADSSE2__

struct CacheTableMatchFinder::MatchEvalSSE2
{
	RADFORCEINLINE SINTa EvalMatches(UnpackedMatchPair * matches, SINTa num_matches, SINTa pos, const U32 * row, const U32 * row2, U32 hash, const U8 * ptr, const U8 * ptr_matchend, SINTa neg_offs_limit)
	{
		typedef CacheTableMatchFinder::t_CTMF t_CTMF;
		static_assert(t_CTMF::c_table_depth == 16, "Specialized to cache table depth");

		const U32 * cur_row = row;

		const __m128i vRefHash = _mm_set1_epi32(hash & CTMF_CHECK_MASK);
		const __m128i vCheckMask = _mm_set1_epi32(CTMF_CHECK_MASK);
		SINTa prevml = 3; // forces MML=4

		for(;;) // two hash rows
		{
		#if CTMF_RESET_PREVML_BETWEEN_ROWS
			prevml = 3;
		#endif

			#define CHECKFOUR(i) \
				const __m128i rowData##i = _mm_load_si128((const __m128i *) &cur_row[i*4]); \
				const __m128i vCheck##i = _mm_cmpeq_epi32(_mm_and_si128(rowData##i, vCheckMask), vRefHash)

			CHECKFOUR(0);
			CHECKFOUR(1);
			CHECKFOUR(2);
			CHECKFOUR(3);

			const __m128i vCheck01 = _mm_packs_epi32(vCheck0, vCheck1);
			const __m128i vCheck23 = _mm_packs_epi32(vCheck2, vCheck3);
			const __m128i vCheck0123 = _mm_packs_epi16(vCheck01, vCheck23);
			U32 validMask = _mm_movemask_epi8(vCheck0123);

			// NOTE(fg): The expected number of bits set in validMask is typically less than 1,
			// so e.g. trying to prefetch ahead or similar is pointless.
			while (validMask)
			{
				int d = rrCtz32(validMask);
				validMask &= validMask - 1;

				// Determine the negative offset, then check if that offset
				// is outside our match window.
				const SINTa neg_offset = t_CTMF::resolve_neg_offset(cur_row[d], (U32)pos);
				if ( neg_offset < neg_offs_limit )
					continue;

				SINTa len;
				if ( ! havematch_better(ptr,ptr + neg_offset,ptr_matchend,prevml,&len) )
					continue;

				RR_ASSERT( len > prevml );
				prevml = len;

				matches[num_matches].length = S32(len);
				matches[num_matches].offset = S32(-neg_offset);
				num_matches++;
			}

			if ( ! t_CTMF::c_do_second_hash )
				break;

			if ( cur_row == row2 )
				break;

			cur_row = row2;
		}

		return num_matches;
	}
};

SINTa CacheTableMatchFinder::ProcessChunkSSE2(SINTa pos, SINTa chunkStart, SINTa chunkEnd, UnpackedMatchPair * outMatches, int maxPairs)
{
	return ProcessChunkTemplate<MatchEvalSSE2>(pos, chunkStart, chunkEnd, outMatches, maxPairs);
}

#endif

#ifdef DO_BUILD_NEON64

struct CacheTableMatchFinder::MatchEvalNEON64
{
	RADFORCEINLINE SINTa EvalMatches(UnpackedMatchPair * matches, SINTa num_matches, SINTa pos, const U32 * row, const U32 * row2, U32 hash, const U8 * ptr, const U8 * ptr_matchend, SINTa neg_offs_limit)
	{
		typedef CacheTableMatchFinder::t_CTMF t_CTMF;
		static_assert(t_CTMF::c_table_depth == 16, "Specialized to cache table depth");

		const U32 * cur_row = row;

		uint32x4_t vRefHash = vdupq_n_u32(hash & CTMF_CHECK_MASK);
		uint32x4_t vCheckMask = vdupq_n_u32(CTMF_CHECK_MASK);
		SINTa prevml = 3; // forces MML=4

		for(;;) // two hash rows
		{
		#if CTMF_RESET_PREVML_BETWEEN_ROWS
			prevml = 3;
		#endif

			#define CHECKFOUR(i) \
				uint32x4_t rowData##i = vld1q_u32(&cur_row[i*4]); \
				uint32x4_t vCheck##i = vceqq_u32(vandq_u32(rowData##i, vCheckMask), vRefHash); \

			CHECKFOUR(0);
			CHECKFOUR(1);
			CHECKFOUR(2);
			CHECKFOUR(3);

			uint16x8_t vCheck01 = vuzp1q_u16(vreinterpretq_u16_u32(vCheck0), vreinterpretq_u16_u32(vCheck1));
			uint16x8_t vCheck23 = vuzp1q_u16(vreinterpretq_u16_u32(vCheck2), vreinterpretq_u16_u32(vCheck3));
			uint8x16_t vCheck0123 = vuzp1q_u8(vreinterpretq_u8_u16(vCheck01), vreinterpretq_u8_u16(vCheck23));

			// extract "mask valid" bits
			uint8x8_t vCheck_nib = vshrn_n_u16(vreinterpretq_u16_u8(vCheck0123), 4); // 0x0 or 0xf per nibble
			U64 validMask = vget_lane_u64(vreinterpret_u64_u8(vCheck_nib), 0) & 0x1111111111111111ull;

			// NOTE(fg): The expected number of bits set in validMask is typically less than 1,
			// so e.g. trying to prefetch ahead or similar is pointless.
			while (validMask)
			{
				int d = rrCtz64(validMask) >> 2;
				validMask &= validMask - 1;

				// Determine the negative offset, then check if that offset
				// is outside our match window.
				const SINTa neg_offset = t_CTMF::resolve_neg_offset(cur_row[d], (U32)pos);
				if ( neg_offset < neg_offs_limit )
					continue;

				SINTa len;
				if ( ! havematch_better(ptr,ptr + neg_offset,ptr_matchend,prevml,&len) )
					continue;

				RR_ASSERT( len > prevml );
				prevml = len;

				matches[num_matches].length = S32(len);
				matches[num_matches].offset = S32(-neg_offset);
				num_matches++;
			}

			if ( cur_row == row2 )
				break;

			cur_row = row2;
		}

		return num_matches;
	}
};

SINTa CacheTableMatchFinder::ProcessChunkNEON64(SINTa pos, SINTa chunkStart, SINTa chunkEnd, UnpackedMatchPair * outMatches, int maxPairs)
{
	return ProcessChunkTemplate<MatchEvalNEON64>(pos, chunkStart, chunkEnd, outMatches, maxPairs);
}

#endif

int CacheTableMatchFinder::ProcessChunk(int chunkSize, UnpackedMatchPair * outMatches, int maxPairs)
{
	SIMPLEPROFILE_SCOPE_N(ctmf_chunk,chunkSize);

	// Requiring maxPairs >= 2 makes the code easier and is not a serious limitation
	RR_ASSERT(maxPairs >= 2);

	SINTa pos = m_pos;
	SINTa chunkStart = pos;
	SINTa chunkEnd = RR_MIN(pos + chunkSize, m_parse_end_pos); // last position we can find matches at
	SINTa realChunkEnd = RR_MIN(pos + chunkSize, m_size); // last position we're asked to report matches for

	// if we have a pending long match, resume it first before we try anything else
	if ( longMatchOffs && ( pos < chunkEnd ) )
	{
		pos = ReportLongMatch(outMatches, chunkStart, chunkEnd, pos, maxPairs);
	}

#if defined(__RADSSE2__)

	if ( rrCPUx86_feature_present(RRX86_CPU_AVX2) )
	{
#ifdef DO_BUILD_AVX2
		pos = ProcessChunkAVX2(pos, chunkStart, chunkEnd, outMatches, maxPairs);
#else
		pos = ProcessChunkSSE2(pos, chunkStart, chunkEnd, outMatches, maxPairs);
#endif
	}
	else
		pos = ProcessChunkSSE2(pos, chunkStart, chunkEnd, outMatches, maxPairs);

#elif defined(DO_BUILD_NEON64)

	pos = ProcessChunkNEON64(pos, chunkStart, chunkEnd, outMatches, maxPairs);

#else

	pos = ProcessChunkGeneric(pos, chunkStart, chunkEnd, outMatches, maxPairs);

#endif

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
	void * jobifyUserPtr RADDEFAULTX(NULL),
	rrArenaAllocator * scratch_arena RADDEFAULTX(NULL))
{
	CacheTableMatchFinder* mf;
	if ( scratch_arena )
		mf = (CacheTableMatchFinder*)scratch_arena->Alloc(sizeof(*mf));
	else
		mf = OODLE_MALLOC_ONE(CacheTableMatchFinder);

	return construct(mf, ubuf, size, startRecordingPos, lrms, scratch_arena);
}

OODLE_NS_END

#else

EXPORT_SOME_CRAP(ctmf);

#endif // OODLE_PLATFORM_HAS_ADVANCED_MATCHERS

