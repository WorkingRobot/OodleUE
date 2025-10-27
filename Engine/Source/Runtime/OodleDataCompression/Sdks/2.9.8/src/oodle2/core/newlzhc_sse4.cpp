// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetsse4

#include "oodlebase.h"
#include "oodlelzcompressors.h"

#include "rrlzh_lzhlw_shared.h"
#include "newlzhc.h"
#include "newlz.h"
#include "newlz_simd.h"
#include "newlz_subliterals.h"
#include "newlz_complexliterals.h"
#include "newlz_offsets.h"
#include "newlzhc_decoder.h"

// SimpleProf (this is still a NOP unless explicitly turned on in CDep via -DOODLE_SIMPLEPROF_BUILD)
/*
#include "rrsimpleprof.h"
/*/
#include "rrsimpleprofstub.h"
/**/

#define CHECK(x)

//=============================================================================

#ifdef DO_BUILD_SSE4

// Fabian's new SSE4 rep cache for Leviathan (Oodle 2.8.6)
// enabled based on presence of sse4 (always enabled on the consoles)
// faster on modern chips, same speed on older sse4 chips

// old way was to actually MTF the 8 offsets = 256 bits = two 128-vecs
//	but there's bad stalls there
// new way :
// the 8 offsets stay in place, once added they don't move
// there's an 8 byte (64 bit) index indirection into the offsets
// the MTF is done on the indirection, not the offsets themselves
// the 64-bit indirection ("mtf_state") is MTF'ed with an sse shuffle from a table

// to get last offsets, it's now offsets[mtf_state[0]]
// and new offset is added at offsets[mtf_state[7]]

#include <smmintrin.h>

OODLE_NS_START

static const RAD_ALIGN(U8, c_lo_shuffles[8][8], 8) =
{
	{ 0,1,2,3,4,5,6,7 },
	{ 1,0,2,3,4,5,6,7 },
	{ 2,0,1,3,4,5,6,7 },
	{ 3,0,1,2,4,5,6,7 },
	{ 4,0,1,2,3,5,6,7 },
	{ 5,0,1,2,3,4,6,7 },
	{ 6,0,1,2,3,4,5,7 },
	{ 7,0,1,2,3,4,5,6 },
};

// This does the decode side only
struct newLZHC_LOs_IndirSSE4
{
	RR_COMPILER_ASSERT( NEWLZHC_NUM_LAST_OFFSETS == 7 );
	S32 offsets[NEWLZHC_NUM_LAST_OFFSETS+1];
	__m128i mtf_state;

	RADFORCEINLINE void Reset_Neg()
	{
		mtf_state = _mm_setr_epi8(0,1,2,3,4,5,6,7, 0,0,0,0,0,0,0,0);
		for(int i=0;i<NEWLZHC_NUM_LAST_OFFSETS;i++)
			offsets[i] = - NEWLZ_MIN_OFFSET;
	}

	RADFORCEINLINE S32 MTF(SINTr index)
	{
		// index can be 1 above last set for normal offset
		RR_ASSERT( index <= NEWLZHC_NUM_LAST_OFFSETS );

		__m128i perm = _mm_loadl_epi64((const __m128i *) c_lo_shuffles[index]);
		mtf_state = _mm_shuffle_epi8(mtf_state, perm);
		U32 slot_id = _mm_extract_epi8(mtf_state, 0);
		return offsets[slot_id];
	}

	S32 LastOffset() const { return offsets[_mm_extract_epi8(mtf_state,0)]; }
};

#define newLZHC_dec_LOs							newLZHC_LOs_IndirSSE4
#define newLZHC_dec_LOs_Reset_Neg(lasts)		lasts.Reset_Neg()
#define newLZHC_dec_LOs_MTF(lasts,index)		lasts.MTF(index)
#define newLZHC_dec_LOs_Add(lastoffsets,above)	lastoffsets.offsets[_mm_extract_epi8(lastoffsets.mtf_state,7)] = above

//=============================================================================

#define NEWLZHC_DECODE_MULTIPACKET	0

#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_RAW
#define newLZHC_decode_parse	newLZHC_decode_parse_raw_sse4
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse
#undef NEWLZHC_DECODE_LITERALS_TYPE

#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_SUB
#define newLZHC_decode_parse	newLZHC_decode_parse_sub_sse4
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse
#undef NEWLZHC_DECODE_LITERALS_TYPE

#ifdef NEWLZ_LITERALS_TYPE_LAMSUB
#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_LAMSUB
#define newLZHC_decode_parse	newLZHC_decode_parse_lamsub_sse4
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse
#undef NEWLZHC_DECODE_LITERALS_TYPE
#endif // NEWLZ_LITERALS_TYPE_LAMSUB


#ifdef NEWLZ_LITERALS_TYPE_SUBAND3
#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_SUBAND3
#define newLZHC_decode_parse	newLZHC_decode_parse_suband3_sse4
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse
#undef NEWLZHC_DECODE_LITERALS_TYPE
#endif // NEWLZ_LITERALS_TYPE_SUBAND3

#ifdef NEWLZ_LITERALS_TYPE_O1
#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_O1
#define newLZHC_decode_parse	newLZHC_decode_parse_o1_sse4
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse
#undef NEWLZHC_DECODE_LITERALS_TYPE
#endif // NEWLZ_LITERALS_TYPE_O1

#ifdef NEWLZ_LITERALS_TYPE_O2
#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_O2
#define newLZHC_decode_parse	newLZHC_decode_parse_o2_sse4
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse
#undef NEWLZHC_DECODE_LITERALS_TYPE
#endif // NEWLZ_LITERALS_TYPE_O2

#ifdef NEWLZ_LITERALS_TYPE_SUBANDF
#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_SUBANDF
#define newLZHC_decode_parse	newLZHC_decode_parse_subandF_sse4
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse
#undef NEWLZHC_DECODE_LITERALS_TYPE
#endif // NEWLZ_LITERALS_TYPE_SUBANDF

//=============================================================================

#undef NEWLZHC_DECODE_MULTIPACKET
#define NEWLZHC_DECODE_MULTIPACKET	1


#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_RAW
#define newLZHC_decode_parse	newLZHC_decode_parse_multipacket_raw_sse4
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse
#undef NEWLZHC_DECODE_LITERALS_TYPE

#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_SUB
#define newLZHC_decode_parse	newLZHC_decode_parse_multipacket_sub_sse4
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse
#undef NEWLZHC_DECODE_LITERALS_TYPE

#ifdef NEWLZ_LITERALS_TYPE_LAMSUB
#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_LAMSUB
#define newLZHC_decode_parse	newLZHC_decode_parse_multipacket_lamsub_sse4
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse
#undef NEWLZHC_DECODE_LITERALS_TYPE
#endif // NEWLZ_LITERALS_TYPE_LAMSUB


#ifdef NEWLZ_LITERALS_TYPE_SUBAND3
#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_SUBAND3
#define newLZHC_decode_parse	newLZHC_decode_parse_multipacket_suband3_sse4
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse
#undef NEWLZHC_DECODE_LITERALS_TYPE
#endif // NEWLZ_LITERALS_TYPE_SUBAND3

#ifdef NEWLZ_LITERALS_TYPE_O1
#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_O1
#define newLZHC_decode_parse	newLZHC_decode_parse_multipacket_o1_sse4
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse
#undef NEWLZHC_DECODE_LITERALS_TYPE
#endif // NEWLZ_LITERALS_TYPE_O1

#ifdef NEWLZ_LITERALS_TYPE_O2
#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_O2
#define newLZHC_decode_parse	newLZHC_decode_parse_multipacket_o2_sse4
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse
#undef NEWLZHC_DECODE_LITERALS_TYPE
#endif // NEWLZ_LITERALS_TYPE_O2

#ifdef NEWLZ_LITERALS_TYPE_SUBANDF
#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_SUBANDF
#define newLZHC_decode_parse	newLZHC_decode_parse_multipacket_subandF_sse4
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse
#undef NEWLZHC_DECODE_LITERALS_TYPE
#endif // NEWLZ_LITERALS_TYPE_SUBANDF

const newLZHC_decode_parse_func_set newLZHC_decode_parse_funcs_sse4 =
{
	newLZHC_decode_parse_raw_sse4,
	newLZHC_decode_parse_sub_sse4,
#ifdef NEWLZ_LITERALS_TYPE_LAMSUB
	newLZHC_decode_parse_lamsub_sse4,
#else
	NULL,
#endif
#ifdef NEWLZ_LITERALS_TYPE_SUBAND3
	newLZHC_decode_parse_suband3_sse4,
#else
	NULL,
#endif
#ifdef NEWLZ_LITERALS_TYPE_O1
	newLZHC_decode_parse_o1_sse4,
#else
	NULL,
#endif
#ifdef NEWLZ_LITERALS_TYPE_O2
	newLZHC_decode_parse_o2_sse4,
#else
	NULL,
#endif
#ifdef NEWLZ_LITERALS_TYPE_SUBANDF
	newLZHC_decode_parse_subandF_sse4,
#else
	NULL,
#endif

	newLZHC_decode_parse_multipacket_raw_sse4,
	newLZHC_decode_parse_multipacket_sub_sse4,
#ifdef NEWLZ_LITERALS_TYPE_LAMSUB
	newLZHC_decode_parse_multipacket_lamsub_sse4,
#else
	NULL,
#endif
#ifdef NEWLZ_LITERALS_TYPE_SUBAND3
	newLZHC_decode_parse_multipacket_suband3_sse4,
#else
	NULL,
#endif
#ifdef NEWLZ_LITERALS_TYPE_O1
	newLZHC_decode_parse_multipacket_o1_sse4,
#else
	NULL,
#endif
#ifdef NEWLZ_LITERALS_TYPE_O2
	newLZHC_decode_parse_multipacket_o2_sse4,
#else
	NULL,
#endif
#ifdef NEWLZ_LITERALS_TYPE_SUBANDF
	newLZHC_decode_parse_multipacket_subandF_sse4,
#else
	NULL,
#endif
};

OODLE_NS_END

#endif // DO_BUILD_SSE4

