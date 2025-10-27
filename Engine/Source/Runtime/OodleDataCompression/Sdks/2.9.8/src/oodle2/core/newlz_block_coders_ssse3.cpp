// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetssse3

#include "oodlebase.h"
#include <string.h>
#include "newlz_block_coders.h"
#include "newlz_block_coders_ssse3.h"
#include "newlz_simd.h"

OODLE_NS_START

#ifdef DO_BUILD_SSE4

OODLE_NS_END
#include <tmmintrin.h> // SSSE3
OODLE_NS_START

// This is SSSE3, not SSE2.
// Reads 16 sequential bit fields of [0,8] bits each from the same bit stream (widths in field_widths).
// It turns out that the same masks we use to mask the results can be instantly added
// to the results to decode Exp-Golomb codes.
//
// in_ptr should point to the byte containing the first bit to be read, bit_pos contains the index
// of the first bit (0=MSB, counting down, matching bitstream direction).
//
// This function reads 17 bytes starting at in_ptr, i.e. bytes in_ptr+0 through in_ptr+16 inclusive.
//
// Returns the number of bits consumed. The values are returned as 16 word lanes in out0 and out1.
static RADFORCEINLINE U32 get_exp_golomb_tails(__m128i *out0, __m128i *out1, const U8 *in_ptr, U32 bit_pos, __m128i field_widths)
{
	// prefix-sum the field widths and advance bit position pointer
	__m128i summed_widths = simd_prefix_sum_u8(field_widths);
	U32 total_width = (U32)_mm_extract_epi16(summed_widths, 7) >> 8; // no PEXTRB before SSE4.1, and this is the only place where SSE4.1+ would help (PEXTRB)

	// determine starting bit position for every lane
	// and split into bit-within-byte and byte indices
	__m128i basepos_u8 = _mm_shuffle_epi8(_mm_cvtsi32_si128(bit_pos), _mm_setzero_si128());
	__m128i first_bit_index = _mm_add_epi8(basepos_u8, _mm_slli_si128(summed_widths, 1));
	__m128i first_byte_index = _mm_and_si128(_mm_srli_epi16(first_bit_index, 3), _mm_set1_epi8(0x1f)); // no "shift bytes", sigh.

	// source bytes
	__m128i src_byte0 = _mm_loadu_si128((const __m128i *) (in_ptr + 0));
	__m128i src_byte1 = _mm_loadu_si128((const __m128i *) (in_ptr + 1));

	// first/second bytes for every lane
	__m128i byte0 = _mm_shuffle_epi8(src_byte0, first_byte_index);
	__m128i byte1 = _mm_shuffle_epi8(src_byte1, first_byte_index);

	// assemble words: (byte0 << 8) | byte1
	__m128i words0 = _mm_unpacklo_epi8(byte1, byte0);
	__m128i words1 = _mm_unpackhi_epi8(byte1, byte0);

	// now, need to shift
	//	 ((byte0<<8) | byte1) >> (16 - width - (first_bit_index & 7))
	// we don't have per-lane variable shifts in SSSE3, but we do have PMULHUW,
	// and we can do the multiplier table lookup via PSHUFB.
	//
	// NOTE 1: this is a lot less crazy when per-lane shifts are available (e.g. NEON)
	// NOTE 2: try AVX2 variant at some point, which has *DWord* but not word per-lane shifts. (SIGH)
	__m128i shift_amt = _mm_add_epi8(_mm_and_si128(first_bit_index, _mm_set1_epi8(7)), field_widths);

	__m128i shiftm0_lut = _mm_setr_epi8(0x01,0x02,0x04,0x08, 0x10,0x20,0x40,-0x80, 0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00);
	__m128i shiftm1_lut = _mm_setr_epi8(0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00, 0x01,0x02,0x04,0x08, 0x10,0x20,0x40,-0x80);

	__m128i shiftm0 = _mm_shuffle_epi8(shiftm0_lut, shift_amt);
	__m128i shiftm1 = _mm_shuffle_epi8(shiftm1_lut, shift_amt);
	__m128i shift_mul0 = _mm_unpacklo_epi8(shiftm0, shiftm1);
	__m128i shift_mul1 = _mm_unpackhi_epi8(shiftm0, shiftm1);

	__m128i shifted0 = _mm_mulhi_epu16(words0, shift_mul0);
	__m128i shifted1 = _mm_mulhi_epu16(words1, shift_mul1);

	// mask by field width, again using a PSHUFB LUT
	__m128i width_mask_lut = _mm_setr_epi8(0,1,3,7, 15,31,63,127, -1,-1,-1,-1, -1,-1,-1,-1);
	__m128i width_mask = _mm_shuffle_epi8(width_mask_lut, field_widths);

	__m128i width_word0 = _mm_unpacklo_epi8(width_mask, _mm_setzero_si128());
	__m128i width_word1 = _mm_unpackhi_epi8(width_mask, _mm_setzero_si128());

	__m128i masked0 = _mm_and_si128(shifted0, width_word0);
	__m128i masked1 = _mm_and_si128(shifted1, width_word1);

	// add mask as bias: (1 << width) - 1
	*out0 = _mm_add_epi16(masked0, width_word0);
	*out1 = _mm_add_epi16(masked1, width_word1);

	return total_width;
}

static RADFORCEINLINE UINTa subtract_sat(UINTa a, UINTa b)
{
	return (a < b) ? 0 : a - b;
}

bool newLZ_decode_alphabet_shape_run_pairs_ssse3(U16 * runLens, BlockBitReader * bbr, const U8 * run_prefix, U32 num_run_pairs)
{
	// Just check if we're already past the end
	if (bbr->ptr >= bbr->end)
		return false;
	UINTa byte_offs_end = bbr->end - bbr->ptr;

	const U8 * bit_base = bbr->ptr;
	U32 bit_pos = bbr->pos_in_byte;

	// NOTE: zero-run codes with width=8: minimum value is (1<<8)-1 = 255; this can *only* occur for
	// the initial zero run but not in here.
	// Nonzero-run codes with width=8: minimum value is (1<<8) = 256; this can *never* occur, since our
	// alphabets are at most 256 symbols and the last nonzero run is implicit.
	//
	// so we could lower this to 7 if that helps.
	static const S8 kMaxExtraBits = 8;

	// Compute start of region where we have to be careful
	static const UINTa kMaxBytes = (127 /*maxRunPairs*/ * 2 * kMaxExtraBits + 7)/8 + 1 /* partial byte we start in */;
	static const UINTa kFinalSlopBytes = 16; // SIMD code reads up to 16 (not 15!) bytes past last touched byte.
	U8 tail_buf[kFinalSlopBytes*2];
	UINTa byte_offs_mark = byte_offs_end;

	// We only need to initialize the tail buf if there's a risk of us actually using it.
	// We defer this because even just accessing a very far-away vbl_end really hurts!
	if (byte_offs_end < kMaxBytes + kFinalSlopBytes)
	{
		byte_offs_mark = subtract_sat(byte_offs_end, kFinalSlopBytes);
		UINTa num_tail_bytes = byte_offs_end - byte_offs_mark;
		if (num_tail_bytes)
			memcpy(tail_buf, bit_base + byte_offs_mark, num_tail_bytes);
		memset(tail_buf + num_tail_bytes, 0, kFinalSlopBytes);
	}

	// NOTE: can just keep reading past end of numRunPairs, we made sure the "unary" array
	// ends with 16 zeros, so over-reading doesn't do anything wrong
	__m128i max_widths = _mm_set1_epi8(kMaxExtraBits); // initial value for field widths (all OK)

	for (UINTa i = 0; i < num_run_pairs; i += 8)
	{
		__m128i counts = _mm_loadu_si128((const __m128i *)(run_prefix + i*2));
		// zero runs (at odd indices) have k=1 so they read one extra bit
		// use add with saturation so we don't wrap around on (invalid)
		// unary codes of 255, and instead detect the error properly.
		counts = _mm_adds_epu8(counts, _mm_set1_epi16(0x0100));
		// keep track of max width we've seen
		max_widths = _mm_max_epu8(max_widths, counts);
		// then clamp field widths to 8 bits so we can guarantee our kMaxBytes estimate holds
		counts = _mm_min_epu8(counts, _mm_set1_epi8(8));
		// end-of-buffer handling
		UINTa byte_index = bit_pos >> 3;
		const U8 *read_ptr = bit_base + byte_index;
		if (byte_index >= byte_offs_mark)
		{
			if (byte_index >= byte_offs_end)
				return false;
			read_ptr = tail_buf + byte_index - byte_offs_mark;
		}
		// decode
		__m128i tail0, tail1;
		bit_pos += get_exp_golomb_tails(&tail0, &tail1, read_ptr, bit_pos & 7, counts);
		// nonzero runs need an extra +1 boost
		tail0 = _mm_add_epi16(tail0, _mm_set1_epi32(1));
		tail1 = _mm_add_epi16(tail1, _mm_set1_epi32(1));
		// store!
		_mm_storeu_si128((__m128i *) (runLens + i*2 + 0), tail0);
		_mm_storeu_si128((__m128i *) (runLens + i*2 + 8), tail1);
	}

	// if there was any out-of-range code length, that's an error
	if (_mm_movemask_epi8(_mm_cmpeq_epi8(max_widths, _mm_set1_epi8(kMaxExtraBits))) != 0xffff)
		return false;

	// Our golombCount loop ends with a partial iteration that doesn't have all 16 lanes
	// actually active. We zero-padded the unary code array, so we didn't produce total
	// garbage, but we did read one extra bit for every zero run (one per pair). Undo that.
	bit_pos -= (0 - num_run_pairs) & 7;

	// Check if we consumed bits past the end
	if (((bit_pos + 7) >> 3) > byte_offs_end)
		return false;

	bbr->ptr += bit_pos >> 3;
	bbr->pos_in_byte = bit_pos & 7;
	return true;
}

bool g_has_newlz_decode_alphabet_shape_run_pairs_ssse3 = true;

#else

#ifdef _MSC_VER
#pragma RR_PRAGMA_MESSAGE("NOTE: block_coders_ssse3 disabled")
#endif

//U32 newLZ_decode_alphabet_shape_run_pairs_ssse3(U16 *runLens, const U8 *bit_base, const U8 *bit_end, U32 bit_pos, const U8 *run_prefix, U32 num_run_pairs)
bool newLZ_decode_alphabet_shape_run_pairs_ssse3(U16 * runLens, BlockBitReader * bbr, const U8 * run_prefix, U32 num_run_pairs)
{
	RR_CANT_GET_HERE();
#ifndef _MSC_VER
	return ~0u;
#endif
}

bool g_has_newlz_decode_alphabet_shape_run_pairs_ssse3 = false;

#endif // DO_BUILD_SSE4


OODLE_NS_END
