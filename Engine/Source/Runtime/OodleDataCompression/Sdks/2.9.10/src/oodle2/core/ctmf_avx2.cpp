// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetavx2

#include "rrbase.h"
#include "rrsimd.h"
#include "ctmf.h"

#if OODLE_PLATFORM_HAS_ADVANCED_MATCHERS && defined(DO_BUILD_AVX2)

#include "ctmf_internal.h"
#include "ctmf_internal.inl"

OODLE_NS_START

struct CacheTableMatchFinder::MatchEvalAVX2
{
	RADFORCEINLINE SINTa EvalMatches(UnpackedMatchPair * matches, SINTa num_matches, SINTa pos, const U32 * row, const U32 * row2, U32 hash, const U8 * ptr, const U8 * ptr_matchend, SINTa neg_offs_limit)
	{
		typedef CacheTableMatchFinder::t_CTMF t_CTMF;
		static_assert(t_CTMF::c_table_depth == 16 && t_CTMF::c_do_second_hash, "Specialized to cache table config");

		// Check both hash rows simultaneously
		const __m256i vPos = _mm256_set1_epi32((U32)pos);
		const __m256i vRefHash = _mm256_set1_epi32(hash & CTMF_CHECK_MASK);
		const __m256i vCheckMask = _mm256_set1_epi32(CTMF_CHECK_MASK);

		RAD_ALIGN(S32, neg_offsets[16*2], 32);

		#define CHECKEIGHT(i,from) \
			const __m256i vRowData##i = _mm256_load_si256((const __m256i *) &(from)); \
			/* this is a vectorized t_CTMF::resolve_neg_offset */ \
			const __m256i vNegOffset##i = _mm256_or_si256(_mm256_sub_epi32(vRowData##i, vPos), vCheckMask); \
			_mm256_storeu_si256((__m256i *)&neg_offsets[i*8], vNegOffset##i); \
			const __m256i vCheck##i = _mm256_cmpeq_epi32(_mm256_and_si256(vRowData##i, vCheckMask), vRefHash)

		CHECKEIGHT(0, row[0]);
		CHECKEIGHT(1, row[8]);
		CHECKEIGHT(2, row2[0]);
		CHECKEIGHT(3, row2[8]);

		// Pack compare results down to bytes, which gives us a strange order
		// because AVX2 packs stay within 128b lanes
		const __m256i vCheck01 = _mm256_packs_epi32(vCheck0, vCheck1);
		const __m256i vCheck23 = _mm256_packs_epi32(vCheck2, vCheck3);
		const __m256i vCheck0123 = _mm256_packs_epi16(vCheck01, vCheck23);

		// Shuffle back into natural order, then grab the mask
		// Put back in the right order and grab the mask
		const __m256i vCheckVals = _mm256_permutevar8x32_epi32(vCheck0123, _mm256_setr_epi32(0,4, 1,5, 2,6, 3,7));
		U32 validMask = _mm256_movemask_epi8(vCheckVals);

		// If both rows were identical, only traverse once
		if ( row == row2 )
			validMask &= 0xffff;

		// Setting prevml=3 forces a MML of 4
		SINTa prevml = 3;
	#if CTMF_RESET_PREVML_BETWEEN_ROWS
		SINTa lastd = 0;
	#endif

		// NOTE(fg): The expected number of bits set in validMask is typically less than 1,
		// so e.g. trying to prefetch ahead or similar is pointless.
		while (validMask)
		{
			SINTa d = rrCtz32(validMask);
			validMask &= validMask - 1;

		#if CTMF_RESET_PREVML_BETWEEN_ROWS
			// On crossing from the first to the second row, clear prevml
			prevml = ( ( d ^ lastd ) < 16 ) ? prevml : 3;
			lastd = d;
		#endif

			// Determine the negative offset, then check if that offset
			// is outside our match window.
			const SINTa neg_offset = neg_offsets[d];
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

		return num_matches;
	}
};

SINTa CacheTableMatchFinder::ProcessChunkAVX2(SINTa pos, SINTa chunkStart, SINTa chunkEnd, UnpackedMatchPair * outMatches, int maxPairs)
{
	return ProcessChunkTemplate<MatchEvalAVX2>(pos, chunkStart, chunkEnd, outMatches, maxPairs);
}

OODLE_NS_END

#else

EXPORT_SOME_CRAP(ctmf_avx2);

#endif // OODLE_PLATFORM_HAS_ADVANCED_MATCHERS && defined(DO_BUILD_AVX2)

