// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetsse4

#include "oodlebase.h"
#include "oodlelzcompressors.h"

#include "rrlzh_lzhlw_shared.h"
#include "newlz.h"
#include "newlz_simd.h"
#include "newlz_subliterals.h"
#include "newlz_offsets.h"
#include "newlz_decoder.h"

#define CHECK(x)

// SimpleProf (this is still a NOP unless explicitly turned on in CDep via -DOODLE_SIMPLEPROF_BUILD)
/*
#include "rrsimpleprof.h"
/*/
#include "rrsimpleprofstub.h"
/**/

//=============================================================================

#ifdef DO_BUILD_SSE4

#include <smmintrin.h>

OODLE_NS_START

// This is the preferred method for LO cache management on SSE4 targets.

static RAD_ALIGN(S32, s_lo_shuffles[4][4], 16) =
{
#define I(i) (i)*0x04040404+0x03020100
	{ I(0),I(1),I(2),I(3) },
	{ I(1),I(0),I(2),I(3) },
	{ I(2),I(0),I(1),I(3) },
	{ I(3),I(0),I(1),I(2) },
#undef I
};

// This does the decode side only
struct newLZ_LOs_SSE4
{
	RR_COMPILER_ASSERT( NEWLZ_NUM_LAST_OFFSETS == 3 );
	__m128i offsets;

	RADFORCEINLINE void Reset_Neg()
	{
		offsets = _mm_set1_epi32(- NEWLZ_MIN_OFFSET);
	}

	RADFORCEINLINE S32 MTF(SINTr index)
	{
		// index can be 1 above last set for normal offset
		RR_ASSERT( index <= NEWLZ_NUM_LAST_OFFSETS );

		offsets = _mm_shuffle_epi8(offsets, _mm_load_si128((const __m128i *) s_lo_shuffles[index]));
		return _mm_cvtsi128_si32(offsets);
	}
	
	S32 LastOffset() const { return _mm_cvtsi128_si32(offsets); }
};

#define newLZ_dec_LOs							newLZ_LOs_SSE4
#define newLZ_dec_LOs_Reset_Neg(lasts)			lasts.Reset_Neg()
#define newLZ_dec_LOs_MTF(lasts,index)			lasts.MTF(index)
#define newLZ_dec_LOs_Add(lastoffsets,above)	lastoffsets.offsets = _mm_insert_epi32(lastoffsets.offsets, above, 3)

//=============================================================================

#define NEWLZ_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_RAW
#define newLZ_decode_parse	newLZ_decode_parse_raw_sse4
#include "newlz_decode_parse_outer.inl"
#undef newLZ_decode_parse
#undef NEWLZ_DECODE_LITERALS_TYPE

#define NEWLZ_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_SUB
#define newLZ_decode_parse	newLZ_decode_parse_sub_sse4
#include "newlz_decode_parse_outer.inl"
#undef newLZ_decode_parse
#undef NEWLZ_DECODE_LITERALS_TYPE

const newLZ_decode_parse_func_set newLZ_decode_parse_funcs_sse4 =
{
	newLZ_decode_parse_raw_sse4,
	newLZ_decode_parse_sub_sse4,
};

OODLE_NS_END

#endif // DO_BUILD_SSE4

