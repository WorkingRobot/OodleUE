// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

OODLE_NS_START

// assert on stuff that's too slow to leave on all the time :
#define NEWLZHC_ASSERT_HEAVY(exp)
//#define NEWLZHC_ASSERT_HEAVY	RR_ASSERT

//=============================================================================

/*

3 packet pos bits seems to be the right amount
makes sense, since 3 bits of ML in packet allows for ML-pos correlation
  (4 bits of packet pos would help if we had 4 bits of LRL or ML in packet)

4 packet pos bits
15,748,709

3 packet pos bits
15,748,617

2 packet pos bits
15,783,698

0 packet pos bits
15,848,223

*/

#define NEWLZHC_PACKET_POS_BITS		3
#define NEWLZHC_PACKET_POS_COUNT	(1<<NEWLZHC_PACKET_POS_BITS)
#define NEWLZHC_PACKET_POS_MASK		(NEWLZHC_PACKET_POS_COUNT-1)

//=============================================================================

#ifdef NEWLZ_LITERALS_TYPE_O1
#define NEWLZHC_O1_ARRAY_COUNT				(NEWLZ_O1_CONTEXT_COUNT)
#define NEWLZHC_LITERAL_ARRAY_COUNT_MAX		RR_MAX(16,NEWLZHC_O1_ARRAY_COUNT)
// 16 for subAndF

#ifdef NEWLZ_LITERALS_TYPE_O2
#define NEWLZHC_O2_ARRAY_COUNT				(NEWLZ_O2_CONTEXT_COUNT)
#undef NEWLZHC_LITERAL_ARRAY_COUNT_MAX
#define NEWLZHC_LITERAL_ARRAY_COUNT_MAX		NEWLZHC_O2_ARRAY_COUNT
#endif

#else
#define NEWLZHC_LITERAL_ARRAY_COUNT_MAX		16
#endif

//=============================================================================

struct newLZHC_chunk_arrays
{
	U8 * chunk_ptr; // to verify
	U8 * scratch_ptr;
	U8 * scratch_end;

	S32 * offsets;
	SINTa offsets_count;

	U32 * excesses;
	SINTa excesses_count;

	U8 * literals_ptrs[NEWLZHC_LITERAL_ARRAY_COUNT_MAX];
	SINTa literals_counts[NEWLZHC_LITERAL_ARRAY_COUNT_MAX];
	SINTa tot_literals_count;

	U8 * packet_pos_ptrs[NEWLZHC_PACKET_POS_COUNT];
	U8 * packet_pos_ends[NEWLZHC_PACKET_POS_COUNT];

	U8 * packets; // packets == NULL if packet_pos is filled
	SINTa packets_count; // == total packet count even if packet_pos is used
};

//=============================================================================

/**

packet :

{7 LO's + 1 general offset}=8
*4 LRL's
*8 ML's
= 256

a bunch of packet values are not used
LO0LRL0
normal matches of len 2

NEWLZHC has ML at the bottom, packed like :

{ offset | lrl | ML }

this means rep0lrl0 is right at the bottom of the byte - bottom 8 values not used

**/

// 3-2-3 with 7 reps
#define NEWLZHC_PACKET_LRL_BITS			2
#define NEWLZHC_PACKET_ML_BITS			3
#define NEWLZHC_NUM_LAST_OFFSETS		7


// offset gets the remaining packet bits :
#define NEWLZHC_PACKET_OFFSET_SHIFT		(NEWLZHC_PACKET_LRL_BITS+NEWLZHC_PACKET_ML_BITS)
#define NEWLZHC_PACKET_OFFSET_BITS		(8 - NEWLZHC_PACKET_OFFSET_SHIFT)

#define NEWLZHC_PACKET_LRL_COUNT		(1<<NEWLZHC_PACKET_LRL_BITS)
#define NEWLZHC_PACKET_ML_COUNT			(1<<NEWLZHC_PACKET_ML_BITS)
#define NEWLZHC_PACKET_OFFSETS_COUNT	(1<<NEWLZHC_PACKET_OFFSET_BITS)


// NEWLZHC_NUM_LAST_OFFSETS must be a raw number of macros
RR_COMPILER_ASSERT( NEWLZHC_NUM_LAST_OFFSETS == (NEWLZHC_PACKET_OFFSETS_COUNT-1) );

#define NEWLZHC_PACKET_LRL_MAX		(NEWLZHC_PACKET_LRL_COUNT-1)
#define NEWLZHC_PACKET_ML_MAX		(NEWLZHC_PACKET_ML_COUNT-1)

#define NEWLZHC_PACKET_COUNT		(NEWLZHC_PACKET_OFFSETS_COUNT*NEWLZHC_PACKET_LRL_COUNT*NEWLZHC_PACKET_ML_COUNT)

#define NEWLZHC_PACKET_NORMAL_MATCH_MIN (NEWLZHC_NUM_LAST_OFFSETS << NEWLZHC_PACKET_OFFSET_SHIFT )

RR_COMPILER_ASSERT(	NEWLZHC_PACKET_COUNT <= 256 ); // packet fits in U8

//=============================================================================

#define NEWLZHC_CHUNK_LEN		128*1024

#define NEWLZHC_CHUNK_NO_MATCH_ZONE		16
#define NEWLZHC_MATCH_END_PAD			8

#define NEWLZHC_MML_NORMAL		4	// <- this is just an encoder parameter, doesn't change code stream
//#define NEWLZHC_MML_NORMAL	3	// <- this is just an encoder parameter, doesn't change code stream

#define NEWLZHC_LOMML			2	// <- locked at 2

//=============================================================================

static inline const U8 * ptr_sub_saturate( const U8 * ptr, SINTa sub_amount, const U8 * base )
{
	return ( ptr - base ) < sub_amount ? base : ptr - sub_amount;
}

//=============================================================================

#if defined(__RADSSE2__)

#define NEWLZHC_SUBAND3_VEC_OFFS

typedef __m128i SubAnd3LitOffs;

static RADFORCEINLINE SubAnd3LitOffs newlzhc_init_suband3_lit_offs(S32 * literals_offs)
{
	SubAnd3LitOffs new_offs = _mm_load_si128((const __m128i *)literals_offs);
	_mm_store_si128((__m128i *) (literals_offs + 4), new_offs);
	return new_offs;
}

static RADFORCEINLINE SubAnd3LitOffs newlzhc_advance_suband3_lit_offs(SubAnd3LitOffs prev_offs, S32 * literals_offs, SINTa pos3, SINTa lrl)
{
	static const RAD_ALIGN(S32, v_advance_tab[4][4], 16) =
	{
		{ 3,2,1,0 },
		{ 0,3,2,1 },
		{ 1,0,3,2 },
		{ 2,1,0,3 },
	};

	// Perform increment
	__m128i v_literals_inc = _mm_load_si128((const __m128i *) v_advance_tab[pos3 & 3]);
	v_literals_inc = _mm_srli_epi32(_mm_add_epi32(v_literals_inc, _mm_set1_epi32((S32)lrl)), 2);
	SubAnd3LitOffs new_offs = _mm_add_epi32(prev_offs, v_literals_inc);

	// Store updated offsets in mem
	_mm_store_si128((__m128i *) (literals_offs + 0), new_offs);
	_mm_store_si128((__m128i *) (literals_offs + 4), new_offs);

	return new_offs;
}

#elif 0 && defined(__RADNEON__) // not a win on Cortex-A57: _very_ slight win on Apple (0.2%-ish); don't do it for now

#define NEWLZHC_SUBAND3_VEC_OFFS

typedef int32x4_t SubAnd3LitOffs;

static RADFORCEINLINE SubAnd3LitOffs newlzhc_init_suband3_lit_offs(S32 * literals_offs)
{
	SubAnd3LitOffs new_offs = vld1q_s32(literals_offs);
	vst1q_s32(literals_offs + 4, new_offs);
	return new_offs;
}

static RADFORCEINLINE SubAnd3LitOffs newlzhc_advance_suband3_lit_offs(SubAnd3LitOffs prev_offs, S32 * literals_offs, SINTa pos3, SINTa lrl)
{
	static const RAD_ALIGN(S32, v_advance_tab[4][4], 16) =
	{
		{ 3,2,1,0 },
		{ 0,3,2,1 },
		{ 1,0,3,2 },
		{ 2,1,0,3 },
	};

	// Perform increment
	int32x4_t v_literals_inc = vld1q_s32(v_advance_tab[pos3 & 3]);
	v_literals_inc = vshrq_n_s32(vaddq_s32(v_literals_inc, vdupq_n_s32(lrl)), 2);
	SubAnd3LitOffs new_offs = vaddq_s32(prev_offs, v_literals_inc);

	// Store updated offsets in mem
	vst1q_s32(literals_offs + 0, new_offs);
	vst1q_s32(literals_offs + 4, new_offs);

	return new_offs;
}

#else

// if no vector impl available, do the advance scalar
#undef NEWLZHC_SUBAND3_VEC_OFFS

#endif

//=============================================================================

typedef bool newLZHC_decode_parse_func(
	const newLZHC_chunk_arrays * arrays,
	U8 * to_ptr, U8 * chunk_base, U8 * chunk_end, U8 * window_base );

struct newLZHC_decode_parse_func_set
{
	newLZHC_decode_parse_func * raw;
	newLZHC_decode_parse_func * sub;
	newLZHC_decode_parse_func * lamsub;
	newLZHC_decode_parse_func * suband3;
	newLZHC_decode_parse_func * o1;
	newLZHC_decode_parse_func * o2;
	newLZHC_decode_parse_func * subandF;

	newLZHC_decode_parse_func * multipacket_raw;
	newLZHC_decode_parse_func * multipacket_sub;
	newLZHC_decode_parse_func * multipacket_lamsub;
	newLZHC_decode_parse_func * multipacket_suband3;
	newLZHC_decode_parse_func * multipacket_o1;
	newLZHC_decode_parse_func * multipacket_o2;
	newLZHC_decode_parse_func * multipacket_subandF;
};

#ifdef DO_BUILD_SSE4
extern const newLZHC_decode_parse_func_set newLZHC_decode_parse_funcs_sse4;
#endif

//=============================================================================

OODLE_NS_END

