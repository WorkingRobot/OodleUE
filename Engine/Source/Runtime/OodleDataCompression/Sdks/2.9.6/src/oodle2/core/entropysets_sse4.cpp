// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetsse4
#include "entropysets.h"
#include "log2table.h"
#include "newlz_simd.h"

#ifdef OODLE_BUILDING_DATA
//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"
#else // non-Oodle-Data never have SimpleProf
#include "rrsimpleprofstub.h"
#endif

#ifdef DO_BUILD_SSE4

#include <smmintrin.h>

OODLE_NS_START

enum { LOG2TABLED_TO_ENTROPYSET_CODELEN_SHIFT = RR_LOG2TABLE_ONE_SHIFT - ENTROPYSET_CODELEN_ONE_BIT_SHIFT };

U32 entropysets_order0_codelen_bits_sse4(const Histo256 & histo,SINTa sumCounts)
{
	SIMPLEPROFILE_SCOPE(entropysets_order0_codelen_bits_sse4);
	RR_ASSERT( sumCounts > 0 );

	U32 invSum = (1<<ENTROPYSET_INVSUM_SHIFT) / (U32)sumCounts;

	__m128i vInvSum = _mm_set1_epi32(invSum);
	__m128i vClSum = _mm_setzero_si128();

	// Do the scaling up front.
	// This is a good deal faster than having the gather in the middle:
	// store the values to L1 and let them cool off for a bit before the
	// table lookups.
	RAD_ALIGN(U32, scaled_counts[256], 16);
	for (int s = 0; s < 256; s += 4)
	{
		__m128i vCounts = _mm_loadu_si128((const __m128i *)&histo.counts[s]);
		__m128i vScaledCounts = _mm_mullo_epi32(vCounts, vInvSum);

		#ifdef RR_LOG2TABLE_SIZE

		// with big log2 table, also do the shift up front
		RR_COMPILER_ASSERT(ENTROPYSET_INVSUM_SHIFT >= RR_LOG2TABLE_SIZE_SHIFT);
		vScaledCounts = _mm_srli_epi32(vScaledCounts, ENTROPYSET_INVSUM_SHIFT - RR_LOG2TABLE_SIZE_SHIFT);

		#endif

		_mm_storeu_si128((__m128i *)&scaled_counts[s], vScaledCounts);
	}

	// Table lookup/gather and finish in second pass
	for (int s = 0; s < 256; s += 4)
	{
		const int sc0 = scaled_counts[s + 0];
		const int sc1 = scaled_counts[s + 1];
		const int sc2 = scaled_counts[s + 2];
		const int sc3 = scaled_counts[s + 3];

		#ifdef RR_LOG2TABLE_SIZE

		const int lt0 = c_rr_log2_table[sc0];
		const int lt1 = c_rr_log2_table[sc1];
		const int lt2 = c_rr_log2_table[sc2];
		const int lt3 = c_rr_log2_table[sc3];

		#else

		int lt0 = log2tabled<ENTROPYSET_INVSUM_SHIFT>(sc0);
		int lt1 = log2tabled<ENTROPYSET_INVSUM_SHIFT>(sc1);
		int lt2 = log2tabled<ENTROPYSET_INVSUM_SHIFT>(sc2);
		int lt3 = log2tabled<ENTROPYSET_INVSUM_SHIFT>(sc3);

		#endif

		const __m128i vLogt0 = _mm_insert_epi32(_mm_cvtsi32_si128(lt0), lt1, 1);
		const __m128i vLogt1 = _mm_insert_epi32(_mm_cvtsi32_si128(lt2), lt3, 1);
		const __m128i vLogt = _mm_unpacklo_epi64(vLogt0, vLogt1);

		__m128i vCl = _mm_srli_epi32(vLogt, LOG2TABLED_TO_ENTROPYSET_CODELEN_SHIFT);
		vCl = _mm_min_epi32(vCl, _mm_set1_epi32(ENTROPYSET_SYM_PRESENT_MAX_CL));

		const __m128i vCounts = _mm_loadu_si128((const __m128i *)&histo.counts[s]);

		// If we don't have the big log2table, we need to fix up missing syms
		#ifndef RR_LOG2TABLE_SIZE // do I have the "big" log2table ?

		// Fix up 0-count lanes
		const __m128i vCountZero = _mm_cmpeq_epi32(vCounts, _mm_setzero_si128());
		const __m128i vMissingCl = _mm_set1_epi32(ENTROPYSET_SYM_NOT_PRESENT_CL);
		vCl = _mm_blendv_epi8(vCl, _mm_set1_epi32(ENTROPYSET_SYM_NOT_PRESENT_CL), vCountZero);

		#endif

		__m128i vClCount = _mm_mullo_epi32(vCl, vCounts);
		vClSum = _mm_add_epi32(vClSum, vClCount);
	}

	U32 clSum = hsum_epi32_sse2(vClSum);
	U32 result = clSum >> ENTROPYSET_CODELEN_ONE_BIT_SHIFT;
	return result;
}

OODLE_NS_END

#endif

