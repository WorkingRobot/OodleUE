// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrmath.h"
#include "oodlelzcompressors.h"
#include "cbradutil.h"
#include "rrvarbits.h"

OODLE_NS_START

struct OodleSpeedFit;

// NEWLZ_MIN_OFFSET is *inclusive* ; off >= NEWLZ_MIN_OFFSET is allowed
#define NEWLZ_MIN_OFFSET		8

enum ENewLZ_MinOffset
{
	eNewLZ_MinOffset_1 = 1,
	eNewLZ_MinOffset_8 = 8
};

#define NEWLZ_ALT_OFFSET_MAX_MOD	128
// 128 is the max now because I send mod in 7 bits (+1)

#define OFFSET_ALT_NUM_UNDER_BITS	3
// 3 is the max because offset log2 can be 5 bits
RR_COMPILER_ASSERT( OFFSET_ALT_NUM_UNDER_BITS <= 3 );

#define OFFSET_BOTTOM_BITS		4 // <- 4 for U8 Huff packing

#define OFF_BOT_MASK		((1<<OFFSET_BOTTOM_BITS)-1)

#define OFFSET_RAW_BITS			4	

#define EXCESS_EXPGOLOMB_SHIFT	6	
#define EXCESS_U32_COUNT_EXPGOLOMB_SHIFT	0  // actually Elias Gamma


// offset >= ESCAPE_OFFSET_MIN goes into the 0xF0 excess offset packing
#define ESCAPE_OFFSET_MIN	((1<<23) - 256 + NEWLZ_MIN_OFFSET)
// ESCAPE_OFFSET_BIAS is in the format, DO NOT TOUCH
#define ESCAPE_OFFSET_BIAS	(((1<<23) - 256) - (1<<16))

// OODLELZ_MAX_OFFSET == 1<<30
// NEWLZ_MAX_OFFSET is *exclusive* ; off < NEWLZ_MAX_OFFSET is allowed
//	num_bits must be < MAX_OFFSET_BITS
#define NEWLZ_MAX_OFFSET_BITS	30
#define NEWLZ_MAX_OFFSET		(1<<NEWLZ_MAX_OFFSET_BITS)
RR_COMPILER_ASSERT( NEWLZ_MAX_OFFSET <= OODLELZ_MAX_OFFSET );

//=======================================================

/*
static RADFORCEINLINE void newLZ_offsets_pack_low(S32 off, U8 * poffset_u8 , U32 * poffset_u32 )
{
	RR_ASSERT( off >= NEWLZ_MIN_OFFSET && off < ESCAPE_OFFSET_MIN );
	
	off += (1<<(4 + OFFSET_RAW_BITS)) - NEWLZ_MIN_OFFSET;
	S32 bot = off & 0xF;
	off >>= 4;
	S32 num_raw = rrGetBitLevel_V_NonZero(off) - 1;
	RR_ASSERT( off >= (1<<num_raw) && off < (2<<num_raw) );
	S32 off_raw = off - (1<<num_raw);

	S32 top = num_raw - OFFSET_RAW_BITS;
	RR_ASSERT( 0 <= top && top < 0xF );

	*poffset_u8 = U8_check( (top<<4) | bot );
	*poffset_u32 = off_raw;
}

static RADFORCEINLINE void newLZ_offsets_pack_high(S32 off, U8 * poffset_u8 , U32 * poffset_u32 )
{
	RR_ASSERT( ESCAPE_OFFSET_MIN <= off && off < NEWLZ_MAX_OFFSET );
	off -= ESCAPE_OFFSET_BIAS;
	S32 num_raw = rrGetBitLevel_V_NonZero(off) - 1;
	RR_ASSERT( off >= (1<<num_raw) && off < (2<<num_raw) );
	S32 off_raw = off - (1<<num_raw);

	*poffset_u8 = U8_check( 0xF0 | num_raw );
	*poffset_u32 = off_raw;
}
*/


static RADFORCEINLINE U8 newLZ_offset44_pack_low_u8(S32 off)
{
	RR_ASSERT( off >= NEWLZ_MIN_OFFSET && off < ESCAPE_OFFSET_MIN );
	
	off += (1<<(4 + OFFSET_RAW_BITS)) - NEWLZ_MIN_OFFSET;
	S32 bot = off & 0xF;

	// This is the underlying logic: (GetBitLevel on high part with bottom 4 bits stripped)
	//S32 num_raw = rrGetBitLevel_V_NonZero(off >> 4) - 1;
	//U32 top = num_raw - OFFSET_RAW_BITS;

	// But it's better to write it like this:
	U32 top = rrGetBitLevel_V_NonZero(off) - (1 + 4 + OFFSET_RAW_BITS);
	RR_ASSERT( top < 0xF );

	return U8_check( (top<<4) | bot );
}

static RADFORCEINLINE U8 newLZ_offset44_pack_high_u8(S32 off)
{
	RR_ASSERT( ESCAPE_OFFSET_MIN <= off && off < NEWLZ_MAX_OFFSET );
	off -= ESCAPE_OFFSET_BIAS;
	S32 num_raw = rrGetBitLevel_V_NonZero(off) - 1;

	return U8_check( 0xF0 | num_raw );
}

static RADFORCEINLINE U8 newLZ_offset44_pack_u8(S32 off)
{
	if ( off < ESCAPE_OFFSET_MIN )
	{
		return newLZ_offset44_pack_low_u8(off);
	}
	else
	{
		return newLZ_offset44_pack_high_u8(off);
	}
}
		
static RADFORCEINLINE U32 newLZ_offset44_unpack_low_numrawbits(U8 offset_u8)
{
	RR_ASSERT( offset_u8 < 0xF0 );

	S32 top_log2 = offset_u8 >> 4;
	S32 num_raw = top_log2 + OFFSET_RAW_BITS;
	
	return num_raw;
}

static RADFORCEINLINE U32 newLZ_offset44_unpack_high_numrawbits(U8 offset_u8)
{
	RR_ASSERT( offset_u8 >= 0xF0 );

	S32 num_raw = 16 + (offset_u8 - 0xF0);
	
	return num_raw;
}

static RADFORCEINLINE U32 newLZ_offset44_unpack_numrawbits(U8 offset_u8)
{
	if ( offset_u8 < 0xF0 )
	{
		return newLZ_offset44_unpack_low_numrawbits(offset_u8);
	}
	else
	{
		return newLZ_offset44_unpack_high_numrawbits(offset_u8);
	}
}

static RADFORCEINLINE U32 newLZ_offset44_pack_low_rawbits(U32 off, U32 num_raw )
{
	RR_ASSERT( off >= NEWLZ_MIN_OFFSET && off < ESCAPE_OFFSET_MIN );
	
	off += (1<<(4 + OFFSET_RAW_BITS)) - NEWLZ_MIN_OFFSET;
	off >>= 4;
	RR_ASSERT( num_raw == rrGetBitLevel_V_NonZero(off) - 1 );
	RR_ASSERT( off >= (1U<<num_raw) && off < (2U<<num_raw) );
	S32 off_raw = off - (1<<num_raw);

	return off_raw;
}

static RADFORCEINLINE U32 newLZ_offset44_pack_high_rawbits(U32 off, U32 num_raw )
{
	RR_ASSERT( ESCAPE_OFFSET_MIN <= off && off < NEWLZ_MAX_OFFSET );
	off -= ESCAPE_OFFSET_BIAS;
	RR_ASSERT( num_raw == rrGetBitLevel_V_NonZero(off) - 1 );
	RR_ASSERT( off >= (1U<<num_raw) && off < (2U<<num_raw) );
	S32 off_raw = off - (1<<num_raw);

	return off_raw;
}

// For offsets below NEWLZ_MIN_OFFSET (i.e. repeats), find the smallest
// multiple of the offset that is above the min offset
static RADFORCEINLINE U32 newLZ_offset44_round_up_tiny(U32 offset)
{
	RR_ASSERT( offset < NEWLZ_MIN_OFFSET );
	RR_COMPILER_ASSERT( NEWLZ_MIN_OFFSET == 8 );

	// rounded_up[k] = lowest multiple of k that is >=NEWLZ_MIN_OFFSET
	static U32 rounded_up[8] =
	{
		0, 8, 8, 9,
		8, 10, 12, 14,
	};
	return rounded_up[offset];
}

//=============================================================

struct KrakenOffsetState
{
	// Input: current and end of offsets U8 (buffer is expected to have padding at end)
	const U8 * offs_u8;
	const U8 * offs_u8_end;

	// Output: write cursor to current offset S32 (buffer is expected to have padding at end)
	S32 * neg_offs_s32;

	// Input bit stream state (bit extract-style since it has simpler invariants)
	const U8 * bitp[2];	// Points to byte containing next bit to be read (backward stream: points 1 past that byte)
	U32 bitc[2];		// Number of bits consumed within current byte

	// Used for debugging only
	S32 * neg_offs_s32_base; // Base address for neg offsets
	S32 * neg_offs_ref_base; // Base address for neg offsets reference
};

#define NEWLZ_OFFSET_DECODE_DEBUG 0

#if NEWLZ_OFFSET_DECODE_DEBUG
#define OFFSET_CHECK(offs_ptr) RR_ASSERT_ALWAYS(*offs_ptr == s->neg_offs_ref_base[offs_ptr - s->neg_offs_s32_base])
#else
#define OFFSET_CHECK(offs_ptr)
#endif

bool newLZ_offset44_decode_finish(KrakenOffsetState * s);
bool newLZ_offset44_decode64_tab(KrakenOffsetState * s);
bool newLZ_offset44_decode_sse4(KrakenOffsetState * s); // x86
bool newLZ_offset44_decode_avx2(KrakenOffsetState * s); // x86
bool newLZ_offset44_decode_neon(KrakenOffsetState * s); // ARM only
bool newLZ_offset44_decode_arm64(KrakenOffsetState * s); // ARM64 only

//=============================================================

// the actual legal min alt offset is 1, but the format allows sending a 0

#define OFFSET_ALT_BIAS		(1<<(OFFSET_ALT_NUM_UNDER_BITS))
// this means that the low offsets don't get their bottom bit entropy coded
//	with no bias, offsets in 8-15 are sent with all 3 bits entropy coded
//	we always send the bottom bit raw
//	doesn't seem to hurt in practice

#define OFFSET_ALT_MAX_NUM_RAW_BITS		(NEWLZ_MAX_OFFSET_BITS-OFFSET_ALT_NUM_UNDER_BITS-1)
RR_COMPILER_ASSERT( OFFSET_ALT_MAX_NUM_RAW_BITS == 26 );
//	2 * OFFSET_ALT_MAX_NUM_RAW_BITS fit in a varbits64 refill (56 bits)
//	but one OFFSET_ALT_MAX_NUM_RAW_BITS does not fit in a varbits32 refill (24 bits)

#define OFFSET_ALT_PACKED_MAX	((OFFSET_ALT_MAX_NUM_RAW_BITS<<OFFSET_ALT_NUM_UNDER_BITS)|((1<<OFFSET_ALT_NUM_UNDER_BITS)-1))
RR_COMPILER_ASSERT( OFFSET_ALT_PACKED_MAX == 215 );
// valid packed alts are <= OFFSET_ALT_PACKED_MAX

static RADFORCEINLINE void newLZ_alt_offset_pack_u8s(U32 off, U8 * ptop, U8 * pbot, int modulo)
{
	RR_ASSERT( modulo >= 1 && modulo <= NEWLZ_ALT_OFFSET_MAX_MOD );
	RR_ASSERT( off >= 1 );
	
	//off -= NEWLZ_MIN_OFFSET_ALT;
	//off -= NEWLZ_MIN_OFFSET;
	
	U32 top = off / modulo;					
	U32 bot = off % modulo;
	
	*pbot = U8_check(bot);
	
	top += OFFSET_ALT_BIAS;

	// find the log2 :
	S32 log2 = rrGetBitLevel_V_NonZero(top) - 1;
	RR_ASSERT( (top>>log2) == 1 );
	RR_ASSERT( log2 >= OFFSET_ALT_NUM_UNDER_BITS );
	
	log2 -= OFFSET_ALT_NUM_UNDER_BITS;
	
	// pack log2 + OFFSET_ALT_NUM_UNDER_BITS bits under it :
	S32 under = top >> log2;
	under ^= 1<<OFFSET_ALT_NUM_UNDER_BITS;
	RR_ASSERT( under <= (1<<OFFSET_ALT_NUM_UNDER_BITS) );

	//offsets_alt_extrabits += log2;
	RR_ASSERT( log2 <= OFFSET_ALT_MAX_NUM_RAW_BITS );
	
	*ptop = U8_check( (log2<<OFFSET_ALT_NUM_UNDER_BITS) | under );
	
	RR_ASSERT( *ptop <= OFFSET_ALT_PACKED_MAX );
}


static RADFORCEINLINE U8 newLZ_alt_offset_pack_u8_modulo1(U32 off)
{
	RR_ASSERT( off >= 1 );
	
	//off -= NEWLZ_MIN_OFFSET_ALT;
	//off -= NEWLZ_MIN_OFFSET;
	
	// if modulo == 1 , simplify (no bot)
	U32 top = off;
		
	top += OFFSET_ALT_BIAS;

	// find the log2 :
	S32 log2 = rrGetBitLevel_V_NonZero(top) - 1;
	RR_ASSERT( (top>>log2) == 1 );
	RR_ASSERT( log2 >= OFFSET_ALT_NUM_UNDER_BITS );
	
	log2 -= OFFSET_ALT_NUM_UNDER_BITS;
	
	// pack log2 + OFFSET_ALT_NUM_UNDER_BITS bits under it :
	S32 under = top >> log2;
	under ^= 1<<OFFSET_ALT_NUM_UNDER_BITS;
	RR_ASSERT( under <= (1<<OFFSET_ALT_NUM_UNDER_BITS) );

	//offsets_alt_extrabits += log2;

	RR_ASSERT( log2 <= OFFSET_ALT_MAX_NUM_RAW_BITS );
	
	U8 ret = U8_check( (log2<<OFFSET_ALT_NUM_UNDER_BITS) | under );
	
	RR_ASSERT( ret <= OFFSET_ALT_PACKED_MAX );
	
	return ret;
}

static RADFORCEINLINE U32 newLZ_alt_offset_unpack_numrawbits(U8 top)
{
	RR_ASSERT( top <= OFFSET_ALT_PACKED_MAX );
	
	return (top>>OFFSET_ALT_NUM_UNDER_BITS);
}

static RADFORCEINLINE U32 newLZ_alt_offset_pack_high_rawbits(U32 off, U32 num_raw , U32 offset_alt_modulo)
{
	//off -= NEWLZ_MIN_OFFSET;
	//off -= NEWLZ_MIN_OFFSET_ALT;
	
	off /= offset_alt_modulo;
	off += OFFSET_ALT_BIAS;

	// find the log2 :
	RR_DURING_ASSERT( U32 log2 = rrGetBitLevel_V_NonZero(off) - 1 - OFFSET_ALT_NUM_UNDER_BITS );
	RR_ASSERT( log2 == num_raw );
		
	U32 low = off & ((1<<num_raw)-1);
	return low;
}

//=============================================================

bool newLZ_offsetalt_decode_finish(KrakenOffsetState * s);
bool newLZ_offsetalt_decode64_tab(KrakenOffsetState * s);
bool newLZ_offsetalt_decode_sse4(KrakenOffsetState * s); // x86 only
bool newLZ_offsetalt_decode_avx2(KrakenOffsetState * s); // x86 only
bool newLZ_offsetalt_decode_neon(KrakenOffsetState * s); // ARM only
bool newLZ_offsetalt_decode_arm64(KrakenOffsetState * s); // ARM64 only

//=============================================================

// puts the excesses at comp_end
// returns length (-1 for failure)
SINTa newLZ_put_excesses_u32_separate(U8 * comp_ptr, U8 * comp_end,
	const U32 * excess_u32s, int excesses_u32_count);
	
// reads offsets and excesses
// offset_alt_modulo = 0 to get 44 offsets
//
// NOTE:
// 1) the decoded values written to "offsets" are -match_distance; i.e. they're _negative_.
//    (This is enforced.)
// 2) HOWEVER, for malformed streams, the resulting values in the offsets can end up
//    basically anything. The decode parse SHOULD NOT assume that offsets are in range or
//    trustworthy. The decoder must validate that each offset is valid and in range
//    where it appears. This validation has to happen in the decoder because
//    at get_offsets_excesses time, we don't know where in the stream an offset occurs,
//    and hence don't know whether it references data before the start of the stream or
//    not. Since this range check has to happen in the decoder no matter what, might as well
//    let the decoder handle _all_ range checking.
S32 newLZ_get_offsets_excesses(const U8 * const comp_ptr, const U8 * const chunk_comp_end,
	const U8 * offsets_u8, const U8 * offsets_u8_2, SINTa offsets_count, U32 offset_alt_modulo, 
	const U8 * excesses_u8, SINTa excesses_count,
	S32 * offsets, U32 * excesses,
	SINTa end_of_chunk_pos,
	U8 excess_hdr_byte, S32 excess_stream_bytes);

SINTa newLZ_get_excesses(rrVarBits * vb1, rrVarBits * vb2, U32 * excesses_base, U32 * excesses_end, SINTa expected_count);

// newLZ_put_offsets_excesses
//	doesn't put excesses if excesses_u32_hdr_size > 0
//	return len includes excess_stream_bytes
//	excess_stream_bytes if present should be at comp_end
SINTa newLZ_put_offsets_excesses(U8 * comp_ptr, U8 * comp_end,
	const U8 * offsets_u8s, 
	const U32 * offsets, int offsets_count,
	int offset_alt_modulo,
	const U32 * excess_u32s, int excesses_u32_count,
	SINTa excesses_u32_hdr_size,SINTa excess_stream_bytes);
	

// put_alt_offset_u8_arrays :
//	fills alt_offsets_u8_top & alt_offsets_u8_bot
//	also counts the raw bits for alt & 44 offsets
void put_alt_offset_u8_arrays(const U32 * offset_array,int offset_count,
								U8 * alt_offsets_u8_top,
								U8 * alt_offsets_u8_bot,
								int * p_alt_offset_extrabits,
								int modulo,
								const U8 * offset_44_u8s,
								int * p_offset44_extrabits
								);
																	

// newLZ_put_offset_44_or_alt_arrays
//	puts entropy arrays; puts 0x80 flag if alt is put
//	*p_offset_alt_modulo == 0 if 44 is used , >= 1 for alt
// NOTE : offsets_u8_44 is overwritten with alt offset top if chosen
// the *pJ that is filled contains the newlz array J's
//		AND the offset get_bits time
//		BUT NOT the offset raw bit count 
SINTa newLZ_put_offset_44_or_alt_arrays(U8 * comp_ptr,U8 * comp_end,
	U8 * offsets_u8_44,U32 * offsets,int offsets_count,
	U32 entropy_flags, F32 lambda,const OodleSpeedFit * speedfit,F32 * pJ,F32 deadline_t,
	ENewLZ_MinOffset min_offset,
	rrbool try_alt_offsets,
	int * p_offset_alt_modulo,
	rrArenaAllocator * arena,
	OodleLZ_CompressionLevel level,
	U32 * poptional_histo1,
	U32 * poptional_histo2);																	


// returns number of excesses (>=0) for success,
// -1 for failure.
//
// vb1 is the forward stream, vb2 is backward. expected_count is either 0 (in which case there is no particular expectation,
// used for the new format) or the signaled excess32_count from the bitstream (old format); if it's non-0, then having a
// different count is an error.
//
// There need to be at least 4 writeable locations at excesses_end before the actual end of the underlying
// storage (because or buffer end checks are only done every 4 decodes).
//
// Decodes both streams until they're exhausted so vb1/vb2 are *not* updated on completion; on success, they must have met,
// so vb1/vb2 are not updated.
SINTa newLZ_get_excesses(rrVarBits * vb1, rrVarBits * vb2, U32 * excesses_base, U32 * excesses_end, SINTa expected_count);

OODLE_NS_END
