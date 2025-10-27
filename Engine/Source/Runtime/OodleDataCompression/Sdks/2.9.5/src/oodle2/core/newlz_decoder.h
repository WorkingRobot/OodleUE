// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

OODLE_NS_START

#if 0 //def _DEBUG
#define DO_CHECK	1
#else
#define DO_CHECK	0
#endif

#if DO_CHECK
#define CHECK(x)	x
#else
#define CHECK(x)
#endif

// assert on stuff that's too slow to leave on all the time :
#define NEWLZ_ASSERT_HEAVY(exp)
//#define NEWLZ_ASSERT_HEAVY	RR_ASSERT
//
//=============================================================================

#define NEWLZ_NUM_LAST_OFFSETS		3
#define NEWLZ_PACKET_OFFSETS_COUNT	(NEWLZ_NUM_LAST_OFFSETS+1)

#define NEWLZ_PACKET_LRL_COUNT		4
#define NEWLZ_PACKET_ML_COUNT		16

#define NEWLZ_PACKET_LRL_MAX	(NEWLZ_PACKET_LRL_COUNT-1)
#define NEWLZ_PACKET_ML_MAX		(NEWLZ_PACKET_ML_COUNT-1)

#define NEWLZ_PACKET_COUNT		(NEWLZ_PACKET_OFFSETS_COUNT*NEWLZ_PACKET_LRL_COUNT*NEWLZ_PACKET_ML_COUNT)

RR_COMPILER_ASSERT(	NEWLZ_PACKET_COUNT <= 256 ); // packet fits in U8

#define NEWLZ_CHUNK_NO_MATCH_ZONE			16
#define NEWLZ_MATCH_END_PAD					8
#define NEWLZ_EXTRA_SCRATCH_MEM_FOR_FUZZ	64

#define NEWLZ_LOMML			2	// <- locked at 2

//=============================================================================

struct newLZ_chunk_arrays
{
	U8 * chunk_ptr; // to verify
	U8 * scratch_ptr;

	S32 * offsets;
	SINTa offsets_count;

	U32 * excesses;
	SINTa excesses_count;

	U8 * packets;
	SINTa packets_count;

	U8 const * literals_ptr;
	SINTa literals_count;
};

//=============================================================================

// ptr_sub_saturate = RR_MAX(base,ptr-sub_amount)
//	but is safe for ptr wrapping invalid math issues
static inline const U8 * ptr_sub_saturate( const U8 * ptr, SINTa sub_amount, const U8 * base )
{
	RR_ASSERT( ptr >= base );
	return ( ptr - base ) < sub_amount ? base : ptr - sub_amount;
}

typedef bool newLZ_decode_parse_func(
	const newLZ_chunk_arrays * arrays,
	U8 * to_ptr, U8 * chunk_base, U8 * chunk_end, U8 * window_base );

struct newLZ_decode_parse_func_set
{
	newLZ_decode_parse_func * raw;
	newLZ_decode_parse_func * sub;
};

#ifdef DO_BUILD_SSE4
extern const newLZ_decode_parse_func_set newLZ_decode_parse_funcs_sse4;
#endif

OODLE_NS_END

