// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlecore.h"

OODLE_NS_START

#define LAGRANGE_COST_INVALID	(1024*1024*1024.f)	// a larger number than any valid J
#define ARRAY_DEADLINE_HUGE     (1024*1024*1024*1024.f)	// a larger number than any valid time

struct rrArenaAllocator;
struct OodleSpeedFit;

/**

put_array;

entropy_flags specify allowed optional features (NEWLZ_ARRAY_FLAG_*)
  this is so we can still produce backward-compat bitstreams for older vers

 *pJ is filled with J
	*pJ may contain value of previous contents' J
	if I can't beat it, I won't replace *to or change *pJ
	-> important : put_array should not change [to] unless it can improve J
 *pJ should be set to LAGRANGE_COST_INVALID before calling this

(to_end - to) must always be big enough to at least put uncompressed (from_len+3)

**/
SINTa newLZ_put_array_histo(U8 * to,U8 * to_end, const U8 * from, SINTa from_len, 
							const U32 * histogram, U32 entropy_flags, 
							F32 lambda,const OodleSpeedFit * speedfit, F32 * pJ, F32 deadline_t,
							rrArenaAllocator * arena, int compression_level);

SINTa newLZ_put_array(U8 * to,U8 * to_end, const U8 * from, SINTa from_len,
	U32 entropy_flags, F32 lambda,const OodleSpeedFit * speedfit, F32 * pJ, F32 deadline_t,
	rrArenaAllocator * arena,
	int compression_level, // OodleLZ_CompressionLevel
	U32 * optional_histo);

SINTa newLZ_put_array_uncompressed(U8 * to,U8 * to_end, const U8 * from, SINTa from_len);

SINTa newLZ_put_array_memset(U8 * to,U8 * to_end, const U8 * from, SINTa from_len,
							U32 entropy_flags, F32 lambda,const OodleSpeedFit * speedfit, F32 * pJ, rrArenaAllocator * arena);
			
// newLZ_reput_array_small on comp data after newLZ_put_array
//	may change it to a "small" array header
// array_comp_len is the total comp len (includes header)
//	modifies comp & returns new size
// requires version >= 6
//	modifies *pJ to keep it correct
SINTa newLZ_reput_array_small(U8 * comp,SINTa array_comp_len, F32 * pJ);

// use newLZ_put_array_small_allowed instead of newLZ_put_array
//	in places where small is allowed
//	(eg. only on sub arrays, not on top level arrays, where the 0x80 flag is often abused)
//	-> this is mainly for internal multiarrays use
// requires version >= 6
SINTa newLZ_put_array_small_allowed(U8 * to,U8 * to_end, const U8 * from, SINTa from_len,
	U32 entropy_flags, F32 lambda,const OodleSpeedFit * speedfit, F32 * pJ, F32 deadline_t,
	rrArenaAllocator * arena,
	int compression_level,
	U32 * optional_histo);
	

/*

 newLZ_get_array : returns comp len ; fills *pto_len
	passed "from_len" should be the buffer size
	from_len is NOT the accurate compressed size, it's for overrun safety

ptr_to should contain enough space for output
but it MAY BE CHANGED
if the compressed data is raw, *ptr_to just points at it (no copy)

returns comp len
fills *pto_len with raw len

**/
/*
// these are in the .inl :

static SINTa newLZ_get_array(U8ptr * ptr_to, const U8 * from, const U8 * from_end, SINTa * pto_len, SINTa to_len_max,
								bool force_copy_uncompressed);

*/

// just get rawlen & complen without decoding :
SINTa newLZ_get_arraylens(const U8 * from, const U8 * from_end, SINTa * pto_len, SINTa to_len_max);

SINTa newLZ_get_array_comp(U32 array_type, U8ptr * ptr_to, const U8 * from, const U8 * from_end, SINTa * pto_len, SINTa to_len_max,
								U8 * scratch_ptr, U8 * scratch_end);

SINTa newlz_get_array_huff(const U8 * const comp, SINTa comp_len, U8 * const to, SINTa to_len, bool is_huff6);

// NEWLZ_ARRAY_TYPE_ is 3 bits
//
// Oodle >= 2.6.0 :
// it's the 3 bits below the top bit
// the very top bit is reserved for flagging alt arrays & modes & now "small" arrays
// "small" arrays can only be used where no other flagging is done
//	eg. not on outer arrays, only on multi-array and other internal arrays
//
// Oodle >= 2.5.0 :
// it's the top 4 bits of the first byte (top of the big endian header word)
// (top bit on is overlapped with LZ chunk header)
//
// Oodle < 2.5.0 :
// array type is only 2 bits
//	(was considered to be 3, but top bit reserved for LZ chunk type flag)
// Oodle >= 2.5.0 adds the bit under this array type, which was always zero < 2.5.0
//  (hence the old array types, HUFF, have bottom bit off)
//
#define NEWLZ_ARRAY_TYPE_UNCOMPRESSED	0
#define NEWLZ_ARRAY_TYPE_HUFF			2
#define NEWLZ_ARRAY_TYPE_HUFF6			4	// Oodle >= 2.3.0
#define NEWLZ_ARRAY_TYPE_TANS			1	// Oodle >= 2.6.0
#define NEWLZ_ARRAY_TYPE_RLE			3	// Oodle >= 2.6.0
#define NEWLZ_ARRAY_TYPE_SPLIT			5	// Oodle >= 2.6.0
#define NEWLZ_ARRAY_TYPE_COUNT			6
// we run out at 8

#define NEWLZ_ARRAY_SIZE_BITS	18
#define NEWLZ_ARRAY_SIZE_MASK	0x3FFFF

#define NEWLZ_ARRAY_SMALL_SIZE_BITS	10
#define NEWLZ_ARRAY_SMALL_SIZE_MASK	1023

// opt-in array features
#define NEWLZ_ARRAY_FLAG_ALLOW_HUFF6	(1<<0)
#define NEWLZ_ARRAY_FLAG_ALLOW_TANS    	(1<<1)
#define NEWLZ_ARRAY_FLAG_ALLOW_RLE     	(1<<2)
#define NEWLZ_ARRAY_FLAG_ALLOW_RLEHUFF	(1<<3)	// nested Huff inside RLE
#define NEWLZ_ARRAY_FLAG_ALLOW_SPLIT	(1<<4)
#define NEWLZ_ARRAY_FLAG_ALLOW_SPLIT_INDEXED	(1<<5)	// nested inside NEWLZ_ARRAY_FLAG_ALLOW_SPLIT
#define NEWLZ_ARRAY_FLAG_ALLOW_HUFFLENS2	(1<<6)
#define NEWLZ_ARRAY_FLAG_ALLOW_RLE_MEMSET   (1<<7)	// allow using RLE special case for memset arrays
	// RLEHUFF is implicitly disallowed when _RLE is off
	//	but _RLE_MEMSET is *not* , it can be on even with _RLE is off


// size can be up to MASK+1
//	That's 256k
// currently newlz max is 128k

// we want this size limit to be as small as possible
//	let the space-speed decision figure out if it makes sense or not
//	eg. if the user turns lambda down to zero, we just want max compression
// the point of this should just be to rule out tiny arrays where huff can't possibly work
//	because we have a minimum ~16 output bytes due to headers and flushing bitstreams and so on
#define NEWLZ_HUFF_ARRAY_MIN_SIZE	32  // <- this applied to Huff or TANS ; < this size = uncompressed


#define NEWLZ_HUFF_CODELEN_LIMIT		11

#define	NEWLZ_HUFF_DECODE_TABLE_SIZE	(1<<NEWLZ_HUFF_CODELEN_LIMIT)

// 32k makes us uncomfortably close to hitting tot_num_indices limit in multiarrays
//#define NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH		(32*1024)
// get some breathing room :
#define NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH		(48*1024)

OODLE_NS_END
