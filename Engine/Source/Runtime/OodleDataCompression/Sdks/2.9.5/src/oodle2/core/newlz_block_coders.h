// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlebase.h"

OODLE_NS_START

// "Block coders": encode/decode homogeneous blocks all using the same
// universal variable-length code.

typedef struct _rrVarBits_ rrVarBits;

// The block readers are most naturally expressed using bit extract-style bit IO,
// so they use this rather than rrVarBits.
struct BlockBitReader
{
	const U8 *ptr;		// current read cursor; <=end
	const U8 *end;		// end of read buffer
	U32 pos_in_byte;	// number of bits in *ptr consumed; in [0,7]
};

// bbr = BlockBitReader*
#define rrVarBits_To_BlockBitReader(vb, bbr) do { \
		UINTa len_in = (UINTa)rrVarBits_BitLen(vb); \
		(bbr)->ptr = NVB(vb,_cur) - ((len_in + 7)>>3); \
		(bbr)->end = NVB(vb,_end); \
		(bbr)->pos_in_byte = (U32) ((0u - len_in) & 7u); /* pos within byte *((bbr)->ptr) - num of bits consumed starting from MSB */ \
	} while (0)

// rrVarBits_GetOpen does a Refill_Safe

#define rrVarBits_From_BlockBitReader(vb, bbr) do { \
		rrVarBits_GetOpen(vb, (bbr)->ptr, (bbr)->end); \
		rrVarBits_Use(vb, (bbr)->pos_in_byte); \
	} while (0)

// Encodes "count" U8 values using a uniform code of the given bit width.
void newLZ_encode_uniform_U8_block(rrVarBits * vb, const U8 * values, SINTa count, int bits_per_val);

// Encodes "count" U8 values of variable bit widths given in a separate array.
void newLZ_encode_variable_U8_block(rrVarBits * vb, const U8 * values, const U8 * nbits, SINTa count);

// Encodes "count" values using unary codes in a block (to be read by newLZ_decode_unary_block)
// The values must be <=240 (this is not required be the encoding, just to catch questionable
// usage).
void newLZ_encode_unary_block(rrVarBits * vb, const U8 * values, SINTa count);

// Decodes "count" unary codes from bit reader br
// Output buffer is written to sloppily, may write up to 7 entries past end, so add padding
//
// Returns the number of codes read, a negative value on error.
SINTa newLZ_decode_unary_block(U8 * unary, SINTa count, BlockBitReader * br);

// Split U8 Rice codes into top and bottom halves, where the top is to be encoded via
// newLZ_encode_unary_block and the bottom via newLZ_encode_uniform_U8_block.
void newLZ_encode_rice_U8_split(U8 * out_top, U8 * out_bottom, const U8 * values, SINTa count, int riceK);

// Decodes the bottom half of Rice codes whose tap half has already
// been decoded via newLZ_decode_unary_block.
//
// riceK must be <=3 for the current implementation.
// Output buffer must have at least 8 bytes of padding after the end.
//
// Returns the number of codes read, a negative value on error.
SINTa newLZ_decode_rice_U8_bottom_block(U8 * codes, SINTa count, int riceK, BlockBitReader * br);

// alphabet runlens *almost* fit in U8
//	one exception where they don't : if all 256 symbols are present
//	then you get a single 256-long run
//typedef U8 t_alphabet_runlen_type;
typedef int t_alphabet_runlen_type;

// Split alphabet shape run lengths into top and bottom half codes (along with bottom half widths).
// This is a split Exp/Golomb code. The top half is the prefix and sent using a unary coder
// (newLZ_encode_unary_block), the bottom half is variable-width codes (up to 8 bits per value)
// to be sent via newLZ_encode_variable_U8_block.
//
// run_lens are the run lengths for the alphabet shape, encoding which of the up to 256 symbols
// in a 8-bit alphabet are present:
//    run_lens[0] is the length of the first "zero run" (symbols with a count of 0)
//    run_lens[1] is the length of the first "nonzero run" (symbols with a count !=0)
//    run_lens[2] is the length of the second "zero run"
//    and so forth.
//
// num_syms is the total number of symbols with a count != 0; it must be >0.
// run_count is the number of runs. It must always be odd (i.e. the first and the last runs are
// both zero runs).
//
// All run lengths but the first (initial zero run) and the last (final zero run) must be >0.
//
// For example, an alphabet that contains symbol IDs 1-3 (inclusive) and 250-255 (inclusive) but no others would turn into:
//    run_lens = { 1, 3, 246, 6, 0 }
//    run_count = 5
//
// The function returns the number of codes to send (i.e. the number of elements in the "top", "bot"
// and "bot_nbits" arrays), which is at most 255. If num_syms != 256, it needs to be sent in the
// bitstream and can be used to reconstruct the number of run pairs and whether the initial run
// is zero or not. (If num_syms == 256, there's only one possible alphabet shape, so no extra
// information is necessary.)
int newLZ_encode_alphabet_shape_runlens_split(U8 * top, U8 * bot, U8 * bot_nbits, int num_syms, const t_alphabet_runlen_type * run_lens, int run_count);

// Sends numEG (the return value of the previos function) for the alphabet shape coder. This
// code has an upper bound based on the number of symbols sent.
//
// Corresponding decode func "newLZ_decode_alphabet_shape_num_EG" is in newlz_block_coders.inl.
void newLZ_encode_alphabet_shape_num_EG(rrVarBits * vb, int num_eg, int num_syms);

// Decode run-lengths for alphabet shape coder (bottom bits of EG codes)
//   run_lens points to an array that, on successful return, contains:
//     run_lens[0] = value of first symbol with nonzero count
//     run_lens[1] = length of first nonzero-symbol run
//     run_lens[2] = first symbol in second nonzero run
//     run_lens[3] = length of second nonzero-symbol run
//     and so forth.
//   num_eg is the number of Exp-Golomb codes signaling the run lengths (0-255, must be signaled outside)
//   run_prefix should point to the already decoded unary prefixes
//     NOTE: run_prefix needs at least 8 bytes of slop past the end, and the unused fields MUST be initialized to 0!
//     (for the SIMD decoders); clean this up?
//
// On successful return, run_lens is validated.
//
// Returns the number of run pairs (>=1), or a negative value on error.
int newLZ_decode_alphabet_shape_runlens(U16 * run_lens, U32 num_nonzero_syms, U32 num_eg, const U8 * run_prefix, rrVarBits * vb);

OODLE_NS_END
