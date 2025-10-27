// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef NEWLZ_TANS_INL
#define NEWLZ_TANS_INL

#include "oodlecore.h"

OODLE_NS_START

// packed_table_ptr has count pre-substracted so you add (count+index) to deref
/* U16 * packed_table_ptr; */

/*

When tans_encode_entry is 16 bytes,
if L is 2048
that's 32 KB of encode table
dangerously close to L1 size

it might pay to keep encode table small

*/

// not-packed ; 16 byte struct :
//struct tans_encode_entry { UINTa packed_table_ptr_uinta; U16 max_state_thresh; U8 max_state_numbits; };

RR_PACKED_STRUCT_START(tans_encode_entry)
// 11 byte struct :
// { UINTa packed_table_ptr_uinta; U16 max_state_thresh; U8 max_state_numbits; }
// 12 byte struct is definitely faster
// these all seem the same :
// { UINTa packed_table_ptr_uinta; U16 max_state_thresh; U8 max_state_numbits; U8 pad; }
// { UINTa packed_table_ptr_uinta; U8 max_state_numbits; U8 pad; U16 max_state_thresh; }
 { UINTa packed_table_ptr_uinta; U16 max_state_thresh; U16 max_state_numbits; }
RR_PACKED_STRUCT_END;

#ifdef __RAD64__
//RR_COMPILER_ASSERT( sizeof(tans_encode_entry) == 16 );
//RR_COMPILER_ASSERT( sizeof(tans_encode_entry) == 11 );
RR_COMPILER_ASSERT( sizeof(tans_encode_entry) == 12 );
#else
RR_COMPILER_ASSERT( sizeof(tans_encode_entry) == 8 );
#endif

// U16 * tans_encode_table_ptr(ee,state) = ee->packed_table_ptr+state;
#define tans_encode_table_ptr(table,ee,state)  ((U16 *)(((ee)->packed_table_ptr_uinta) + sizeof(U16)*(state)))


/*
// instead of packed_table_ptr_uinta
// use an offset from base
// offset can even fit in 16 bits
// requires an extra base pointer in the core encode loop
// seems to be about 0.5 cycles/byte slower

//typedef S16 t_packed_table_offset; // slower, but tans_encode_entry is 6 bytes
typedef S32 t_packed_table_offset; // tans_encode_entry is 8 bytes
//typedef U32 t_packed_table_offset; // tans_encode_entry is 8 bytes
struct tans_encode_entry { t_packed_table_offset packed_table_offset; U16 max_state_thresh; U8 max_state_numbits; };

// t_packed_table_offset unsigned, do the add in smaller word or mask :
// this way is good on lappy :
//#define tans_encode_table_ptr(base,ee,state)  ((U16 *)((UINTa)(base) + (UINTa)((U32)((ee)->packed_table_offset + sizeof(U16)*((U32)state)))))
//#define tans_encode_table_ptr(base,ee,state)  ((U16 *)((UINTa)(base) + (((ee)->packed_table_offset + sizeof(U16)*(state))&0xFFFFFFFF)))
// t_packed_table_offset is signed, need a small->big sign extend :
#define tans_encode_table_ptr(base,ee,state)  ((U16 *)((UINTa)(base) + ((SINTa)(ee)->packed_table_offset) + sizeof(U16)*(state)))
*/

//	RR_PACKED_STRUCT is a tight-packed struct on all platforms
//	unlike "RADSTRUCT" which is NOT packed on MSVC
	
// RADSTRUCT is defined as "struct" on msvc, and
//   "struct __attribute__((__packed__))" on gcc/clang
//   Used to sort of address generic structure packing
//   (we still require #pragma packs to fix the
//   packing on windows, though)

RADSTRUCT tans_decode_entry_U8 { U32 mask; U8 len; U8 sym; U16 nextst; };

RR_COMPILER_ASSERT( sizeof(tans_decode_entry_U8) == 8 );

//===================================================

struct newlz_tans_Encoder
{
	S32 L, L_bits;
	S32 alphabet, _padding;

	tans_encode_entry encode_sym_table[256];
	U16 * encode_table_packed; // L of them
};

// encode is backwards

// encode start :
// state = L

// encode finish :
// putbits(state,L_bits+1);
	
//=================================================================================	

struct newlz_tans_Decoder
{
	S32 L, L_bits;

	tans_decode_entry_U8 * decode_table_U8;
};

// encode state is true TANS [L,2L)
//		(you need that top bit on, when you shift down it acts like a bit counter)
// decode "state" is actually an "x" in [0,L)
//
// no decode finish

//=================================================================================	

OODLE_NS_END
	
#endif // NEWLZ_TANS_INL

	
