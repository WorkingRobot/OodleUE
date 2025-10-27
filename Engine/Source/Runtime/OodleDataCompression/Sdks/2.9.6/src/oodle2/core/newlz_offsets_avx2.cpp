// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetavx2
#include "newlz_offsets.h"
#include "newlz_simd.h"

//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"

OODLE_NS_START

#ifdef DO_BUILD_AVX2

OODLE_NS_END
#include <immintrin.h>
OODLE_NS_START

// Returns x[i] & ((1 << count[i]) - 1) + (1 << count[i]);
//
// x is U32[4]
// counts are U32[4] in [0,31]
static RADFORCEINLINE __m256i mask_to_width_and_set_top(__m256i x, __m256i counts)
{
	// Determine mask = ~0 << counts[i] = -1 << counts[i],
	// and then return (x & ~mask) - mask.
	__m256i mask = _mm256_sllv_epi32(_mm256_set1_epi32(-1), counts);
	return _mm256_sub_epi32(_mm256_andnot_si256(mask, x), mask);
}

// Interleaves the forward stream values (in bottom half) and backward stream
// values (in top half)
static RADFORCEINLINE __m256i forward_backward_interleave32(__m256i x)
{
	__m128i lo = _mm256_extracti128_si256(x, 0);
	__m128i hi = _mm256_extracti128_si256(x, 1);
	__m128i merged0 = _mm_unpacklo_epi32(lo, hi);
	__m128i merged1 = _mm_unpackhi_epi32(lo, hi);
	return _mm256_setr_m128i(merged0, merged1);
}

bool newLZ_offset44_decode_avx2(KrakenOffsetState * s)
{
	const U8 * offs_u8 = s->offs_u8;
	const U8 * offs_u8_end = s->offs_u8_end;
	SIMPLEPROFILE_SCOPE_N(offsets44_dec_avx2, offs_u8_end-offs_u8);

	if (offs_u8_end - offs_u8 >= 8 && s->bitp[1] - s->bitp[0] >= 16)
	{
		offs_u8_end -= 7;

		S32 * neg_offs_s32 = s->neg_offs_s32;
		const U8 * bitp0 = s->bitp[0];
		const U8 * bitp1 = s->bitp[1] - 16;

		__m128i left_shift_lut = _mm_setr_epi8(1,2,4,8, 16,32,64,-128, 1,2,4,8, 16,32,64,-128);
		__m128i shuf_u8to32_second = _mm_setr_epi8(4,-1,-1,-1, 5,-1,-1,-1, 6,-1,-1,-1, 7,-1,-1,-1);
		__m256i offs_bias = _mm256_set1_epi32((1<<(OFFSET_RAW_BITS + 4)) - NEWLZ_MIN_OFFSET);
		__m256i escape_bias = _mm256_set1_epi32(-ESCAPE_OFFSET_BIAS);
		__m128i const_0xf0 = _mm_set1_epi8(0xf0 - 0x100);
		__m128i initial_bit = _mm_cvtsi32_si128(s->bitc[0] | (s->bitc[1] << 8)); // starting bit position within initial byte in lanes 0/1 (rest zero)

		while (offs_u8 < offs_u8_end && bitp0 <= bitp1)
		{
			// Grab the next 8 offset_u8s and split them up
			__m128i offset_u8 = _mm_loadl_epi64((const __m128i *) offs_u8);
			__m128i offset_hi_nib = _mm_and_si128(offset_u8, const_0xf0);
			__m128i offs_islarge_u8 = _mm_cmpeq_epi8(offset_hi_nib, const_0xf0);
			__m128i offset_lo_nib = _mm_andnot_si128(const_0xf0, offset_u8);

			__m256i neg_offs;

			// Work out raw bit counts for small-count case
			__m128i numraw_small = _mm_add_epi8(_mm_srli_epi16(offset_hi_nib, 4), _mm_set1_epi8(OFFSET_RAW_BITS));

			// Grab source bytes for both streams
			__m128i bytes0 = _mm_loadu_si128((const __m128i *)bitp0);
			__m128i bytes1 = _mm_loadu_si128((const __m128i *)bitp1);

			// Reverse second half then combine
			bytes1 = _mm_shuffle_epi8(bytes1, _mm_setr_epi8(15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0));
			__m256i bytes = _mm256_setr_m128i(bytes0, bytes1);

			if (_mm_testz_si128(offs_islarge_u8, offs_islarge_u8))
			{
				// Faster path: no large offsets

				// Prefix sum to work out end bit for each field
				// we're summing both the forward and backward stream counts here
				__m128i pfx0 = _mm_add_epi8(numraw_small, initial_bit);
				__m128i pfx1 = _mm_add_epi8(pfx0, _mm_slli_epi64(pfx0, 16));
				__m128i end_bit_index_u8 = _mm_add_epi8(pfx1, _mm_slli_epi64(pfx1, 32));

				// Work out byte shuffle indices
				__m128i end_byte_index_u8 = _mm_and_si128(_mm_srli_epi16(end_bit_index_u8, 3), _mm_set1_epi8(0x0f));
				__m256i byte_shuffle = _mm256_shuffle_epi8(
					_mm256_broadcastsi128_si256(end_byte_index_u8),
					_mm256_setr_epi8(0,0,0,0, 2,2,2,2, 4,4,4,4, 6,6,6,6, 1,1,1,1, 3,3,3,3, 5,5,5,5, 7,7,7,7)
				);
				byte_shuffle = _mm256_sub_epi32(byte_shuffle, _mm256_set1_epi32(0x03020100));

				// Bit buffer advance
				U32 advance_amts = _mm_extract_epi16(end_byte_index_u8, 3);
				initial_bit = _mm_and_si128(_mm_srli_epi64(end_bit_index_u8, 48), _mm_set1_epi8(7));
				bitp0 += advance_amts & 0xff;
				bitp1 -= advance_amts >> 8;

				// Shuffle source bytes for both streams
				__m256i dwords = _mm256_shuffle_epi8(bytes, byte_shuffle);

				// Left shift the source DWords
				__m128i left_shift_amt_u8 = _mm_and_si128(end_bit_index_u8, _mm_set1_epi8(7));
				__m256i left_shift_amts_u32 = _mm256_shuffle_epi8(
					_mm256_broadcastsi128_si256(left_shift_amt_u8),
					_mm256_setr_epi8(0,-1,-1,-1, 2,-1,-1,-1, 4,-1,-1,-1, 6,-1,-1,-1, 1,-1,-1,-1, 3,-1,-1,-1, 5,-1,-1,-1, 7,-1,-1,-1)
				);
				__m256i shifted_dwords = _mm256_sllv_epi32(dwords, left_shift_amts_u32);
				__m256i offs_raw_bits = _mm256_srli_epi32(shifted_dwords, 8);

				// Interleave the forward/backward stream results
				__m256i offs_bits = forward_backward_interleave32(offs_raw_bits);

				// Compute the masked, biased offsets
				__m256i offs_masked = mask_to_width_and_set_top(offs_bits, _mm256_cvtepu8_epi32(numraw_small));

				// Compute the negated offsets
				neg_offs = _mm256_sub_epi32(offs_bias, _mm256_slli_epi32(offs_masked, 4));
				neg_offs = _mm256_sub_epi32(neg_offs, _mm256_cvtepu8_epi32(offset_lo_nib));
			}
			else
			{
				// Slower path with large offset support
				__m128i escape = _mm_subs_epu8(offset_u8, _mm_set1_epi8(0xfd - 0x100)); // after this, >0 iff value >=0xfe
				if (!_mm_testz_si128(escape, escape)) // any nonzero values?
					return false;

				// Raw bit count for large offsets is easy
				// we compute it as a correction to the numraw count that was computed
				// with the regular formula for a high nibble of 0xf
				__m128i numraw_large_diff = _mm_add_epi8(offset_lo_nib, _mm_set1_epi8(16 - (0xf + OFFSET_RAW_BITS)));
				__m128i numraw = _mm_add_epi8(numraw_small, _mm_and_si128(numraw_large_diff, offs_islarge_u8));

				// Prefix sum to work out end bit for each field
				// we're summing both the forward and backward stream counts here
				__m128i pfx0 = _mm_add_epi8(numraw, initial_bit);
				__m128i pfx1 = _mm_add_epi8(pfx0, _mm_slli_epi64(pfx0, 16));
				__m128i end_bit_index_u8 = _mm_add_epi8(pfx1, _mm_slli_epi64(pfx1, 32));

				// Work out byte shuffle indices
				__m128i end_byte_index_u8 = _mm_and_si128(_mm_srli_epi16(end_bit_index_u8, 3), _mm_set1_epi8(0x0f));
				__m256i byte_shuffle = _mm256_shuffle_epi8(
					_mm256_broadcastsi128_si256(end_byte_index_u8),
					_mm256_setr_epi8(0,0,0,0, 2,2,2,2, 4,4,4,4, 6,6,6,6, 1,1,1,1, 3,3,3,3, 5,5,5,5, 7,7,7,7)
				);

				// Bit buffer advance
				U32 advance_amts = _mm_extract_epi16(end_byte_index_u8, 3);
				initial_bit = _mm_and_si128(_mm_srli_epi64(end_bit_index_u8, 48), _mm_set1_epi8(7));
				bitp0 += advance_amts & 0xff;
				bitp1 -= advance_amts >> 8;

				// Shuffle source bytes for both streams
				__m256i dwordsl = _mm256_shuffle_epi8(bytes, _mm256_sub_epi32(byte_shuffle, _mm256_set1_epi32(0x03020100)));
				__m256i dwordsh = _mm256_shuffle_epi8(bytes, _mm256_sub_epi32(byte_shuffle, _mm256_set1_epi32(0x04030201)));

				// Left shift the source DWords
				// the 'l' terms here contribute the low byte of every result word (not DWord!) and may have 0s in the high bits
				// the 'h' terms contribute the high byte of every result word and may have 0s in the low bits
				// OR them together and you get all the bits.
				//
				// NOTE: with AVX2 we have variable shifts but in my tests it was still faster to do it this way
				__m128i left_shift_mults = _mm_shuffle_epi8(left_shift_lut, end_bit_index_u8);
				__m256i left_shift_mult = _mm256_shuffle_epi8(
					_mm256_broadcastsi128_si256(left_shift_mults),
					_mm256_setr_epi8(0,-1,0,-1, 2,-1,2,-1, 4,-1,4,-1, 6,-1,6,-1, 1,-1,1,-1, 3,-1,3,-1, 5,-1,5,-1, 7,-1,7,-1)
				);
				__m256i shifted_dwordsl = _mm256_mullo_epi16(dwordsl, left_shift_mult);
				__m256i shifted_dwordsh = _mm256_mullo_epi16(dwordsh, left_shift_mult);
				__m256i offs_raw_bits = _mm256_or_si256(_mm256_srli_epi32(shifted_dwordsl, 8), shifted_dwordsh);

				// Interleave the forward/backward stream results
				__m256i offs_bits = forward_backward_interleave32(offs_raw_bits);

				// Compute the masked, biased offsets
				__m256i offs_masked = mask_to_width_and_set_top(offs_bits, _mm256_cvtepu8_epi32(numraw));

				// Compute the negated offsets for the small and large offset case,
				// then select the right one
				__m256i neg_offs_small = _mm256_sub_epi32(offs_bias, _mm256_slli_epi32(offs_masked, 4));
				neg_offs_small = _mm256_sub_epi32(neg_offs_small, _mm256_cvtepu8_epi32(offset_lo_nib));

				__m256i neg_offs_large = _mm256_sub_epi32(escape_bias, offs_masked);
				neg_offs = _mm256_blendv_epi8(neg_offs_small, neg_offs_large, _mm256_cvtepi8_epi32(offs_islarge_u8));
			}

			// Store offsets
			_mm256_storeu_si256((__m256i *) &neg_offs_s32[0], neg_offs);

			// Advance output buffer pointers
			offs_u8 += 8;
			neg_offs_s32 += 8;
		}

		s->offs_u8 = offs_u8;
		s->neg_offs_s32 = neg_offs_s32;

		U16 final_bit_offs = (U16)_mm_extract_epi16(initial_bit, 0);

		s->bitp[0] = bitp0;
		s->bitp[1] = bitp1 + 16;
		s->bitc[0] = final_bit_offs & 7;
		s->bitc[1] = final_bit_offs >> 8;
	}

	return newLZ_offset44_decode_finish(s);
}

static RADFORCEINLINE void offsetalt_output(KrakenOffsetState * s, S32 * neg_offs_s32, __m256i offs_bits_u32, __m128i hibits_u8, __m128i numraw_u8)
{
	__m256i hibits32 = _mm256_cvtepu8_epi32(hibits_u8);
	__m256i numraw32 = _mm256_cvtepu8_epi32(numraw_u8);

	// Shift high bits into the right position
	hibits32 = _mm256_sllv_epi32(hibits32, numraw32);

	// (-1 << numraw32) has 0's in the bottom numraw32 bits, 1's above, which we use to mask offs32
	// then throw in the high bits
	__m256i count_mask = _mm256_sllv_epi32(_mm256_set1_epi32(-1), numraw32);
	__m256i offs32 = _mm256_or_si256(_mm256_andnot_si256(count_mask, offs_bits_u32), hibits32);

	// Compute the negated offsets, taking care of the bias along the way
	__m256i neg_offs = _mm256_sub_epi32(_mm256_set1_epi32(OFFSET_ALT_BIAS), offs32);

	// Store
	_mm256_storeu_si256((__m256i *)neg_offs_s32, neg_offs);

	OFFSET_CHECK(&neg_offs_s32[0]);
	OFFSET_CHECK(&neg_offs_s32[1]);
	OFFSET_CHECK(&neg_offs_s32[2]);
	OFFSET_CHECK(&neg_offs_s32[3]);
	OFFSET_CHECK(&neg_offs_s32[4]);
	OFFSET_CHECK(&neg_offs_s32[5]);
	OFFSET_CHECK(&neg_offs_s32[6]);
	OFFSET_CHECK(&neg_offs_s32[7]);
}

bool newLZ_offsetalt_decode_avx2(KrakenOffsetState * s)
{
	const U8 * offs_u8 = s->offs_u8;
	const U8 * offs_u8_end = s->offs_u8_end;
	SIMPLEPROFILE_SCOPE_N(offsetsalt_dec_avx2, offs_u8_end-offs_u8);

	if (offs_u8_end - offs_u8 >= 16 && s->bitp[1] - s->bitp[0] >= 16)
	{
		offs_u8_end -= 15;

		S32 * neg_offs_s32 = s->neg_offs_s32;
		const U8 * bitp0 = s->bitp[0];
		const U8 * bitp1 = s->bitp[1] - 16;

		const U8 OFFSET_ALT_UNDER_MASK  = (1 << OFFSET_ALT_NUM_UNDER_BITS) - 1;
		const U8 OFFSET_ALT_NUMRAW_MASK = OFFSET_ALT_UNDER_MASK ^ 0xff;

		const __m128i left_shift_lut = _mm_setr_epi8(1,2,4,8, 16,32,64,-128, 1,2,4,8, 16,32,64,-128);
		const __m128i initial_bit_mask = _mm_setr_epi8(7,7,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0);
		const __m128i numraw_bits_mask = _mm_set1_epi8(0 - (1 << OFFSET_ALT_NUM_UNDER_BITS));
		__m128i initial_bit = _mm_cvtsi32_si128(s->bitc[0] | (s->bitc[1] << 8)); // starting bit position within initial byte in lanes 0/1 (rest zero)

		while (offs_u8 < offs_u8_end && bitp0 <= bitp1)
		{
			// Grab the next 16 offset_u8s
			__m128i offset_u8 = _mm_loadu_si128((const __m128i *) offs_u8);

			// Work out raw bit counts
			__m128i numraw = _mm_srli_epi16(_mm_and_si128(offset_u8, _mm_set1_epi8(OFFSET_ALT_NUMRAW_MASK - 0x100)), OFFSET_ALT_NUM_UNDER_BITS);
			__m128i numraw_plus_initial = _mm_add_epi8(numraw, initial_bit);

			// Grab source bytes for both streams
			__m128i bytes0 = _mm_loadu_si128((const __m128i *)bitp0);
			__m128i bytes1 = _mm_loadu_si128((const __m128i *)bitp1);

			// Reverse second half then combine
			bytes1 = _mm_shuffle_epi8(bytes1, _mm_setr_epi8(15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0));
			__m256i bytes = _mm256_setr_m128i(bytes0, bytes1);

			// Can we do a full blast of 16 offsets with <= 15 numraw bits?
			if (_mm_testz_si128(offset_u8, _mm_set1_epi8(-0x80)))
			{
				// 16 offsets with less than 15 extra bits; can use a multigetbits decoder that supports
				// at most 15 bits read per lane, giving us 16 offsets per iteration instead of 8.

				// Prefix sum to work out end bit for each field
				// we're summing both the forward and backward stream counts here
				__m128i pfx0 = _mm_add_epi8(numraw_plus_initial, _mm_slli_si128(numraw_plus_initial, 2));
				__m128i pfx1 = _mm_add_epi8(pfx0, _mm_slli_si128(pfx0, 4));
				__m128i end_bit_index_u8 = _mm_add_epi8(pfx1, _mm_slli_si128(pfx1, 8));

				// Work out byte shuffle indices
				__m128i end_byte_index_u8 = _mm_and_si128(_mm_srli_epi16(end_bit_index_u8, 3), _mm_set1_epi8(0x0f));
				__m256i byte_shuffle = _mm256_shuffle_epi8(
					_mm256_broadcastsi128_si256(end_byte_index_u8),
					_mm256_setr_epi8(0,0, 2,2, 4,4, 6,6, 8,8, 10,10, 12,12, 14,14, 1,1, 3,3, 5,5, 7,7, 9,9, 11,11, 13,13, 15,15)
				);

				// Bit buffer advance
				U32 advance_amts = _mm_extract_epi16(end_byte_index_u8, 7);
				initial_bit = _mm_and_si128(_mm_srli_si128(end_bit_index_u8, 14), initial_bit_mask);
				bitp0 += advance_amts & 0xff;
				bitp1 -= advance_amts >> 8;

				// Shuffle source bytes for both streams
				__m256i wordsl = _mm256_shuffle_epi8(bytes, _mm256_sub_epi8(byte_shuffle, _mm256_set1_epi16(0x0100)));
				__m256i wordsh = _mm256_shuffle_epi8(bytes, _mm256_sub_epi8(byte_shuffle, _mm256_set1_epi16(0x0201)));

				// Left shift the source words
				// the 'l' terms here contribute the low byte of every result word and may have 0s in the high bits
				// the 'h' terms contribute the high byte of every result word and may have 0s in the low bits
				// OR them together and you get all the bits.
				__m128i left_shift_mults = _mm_shuffle_epi8(left_shift_lut, end_bit_index_u8);
				__m256i left_shift_mult = _mm256_shuffle_epi8(
					_mm256_broadcastsi128_si256(left_shift_mults),
					_mm256_setr_epi8(0,-1, 2,-1, 4,-1, 6,-1, 8,-1, 10,-1, 12,-1, 14,-1,  1,-1, 3,-1, 5,-1, 7,-1, 9,-1, 11,-1, 13,-1, 15,-1)
				);
				__m256i shifted_wordsl = _mm256_mullo_epi16(wordsl, left_shift_mult);
				__m256i shifted_wordsh = _mm256_mullo_epi16(wordsh, left_shift_mult);
				__m256i offs_raw_bits = _mm256_or_si256(_mm256_srli_epi16(shifted_wordsl, 8), shifted_wordsh);

				// Forward stream results are in bottom half, backward stream results in top
				// these need to get interleaved, but we also need to work with the AVX2 shuffle
				// restrictions, so this shakes out differently than it does for SSE4.
				__m128i offs_raw_bits0 = _mm256_extracti128_si256(offs_raw_bits, 0);
				__m128i offs_raw_bits1 = _mm256_extracti128_si256(offs_raw_bits, 1);
				__m128i offs_bits0 = _mm_unpacklo_epi16(offs_raw_bits0, offs_raw_bits1);
				__m128i offs_bits1 = _mm_unpackhi_epi16(offs_raw_bits0, offs_raw_bits1);

				__m128i hibits_u8 = _mm_sub_epi8(_mm_andnot_si128(numraw_bits_mask, offset_u8), numraw_bits_mask);

				offsetalt_output(s, neg_offs_s32 + 0, _mm256_cvtepu16_epi32(offs_bits0), hibits_u8, numraw);
				offsetalt_output(s, neg_offs_s32 + 8, _mm256_cvtepu16_epi32(offs_bits1), _mm_srli_si128(hibits_u8, 8), _mm_srli_si128(numraw, 8));

				// Advance output buffer pointers
				offs_u8 += 16;
				neg_offs_s32 += 16;
			}
			else
			{
				// Large offsets detected.
				// Check if any of the counts are out of range
				__m128i bad_numraws = _mm_subs_epu8(numraw, _mm_set1_epi8(26)); // after this, >0 iff value >=27
				if (!_mm_testz_si128(bad_numraws, bad_numraws)) // any nonzero values?
					return false;

				// Prefix sum to work out end bit for each field
				// we're summing both the forward and backward stream counts here
				__m128i pfx0 = _mm_add_epi8(numraw_plus_initial, _mm_slli_epi64(numraw_plus_initial, 16));
				__m128i end_bit_index_u8 = _mm_add_epi8(pfx0, _mm_slli_epi64(pfx0, 32));

				// Work out byte shuffle indices
				__m128i end_byte_index_u8 = _mm_and_si128(_mm_srli_epi16(end_bit_index_u8, 3), _mm_set1_epi8(0x0f));
				__m256i byte_shuffle = _mm256_shuffle_epi8(
					_mm256_broadcastsi128_si256(end_byte_index_u8),
					_mm256_setr_epi8(0,0,0,0, 2,2,2,2, 4,4,4,4, 6,6,6,6, 1,1,1,1, 3,3,3,3, 5,5,5,5, 7,7,7,7)
				);

				// Bit buffer advance
				U32 advance_amts = _mm_extract_epi16(end_byte_index_u8, 3);
				initial_bit = _mm_and_si128(_mm_srli_epi64(end_bit_index_u8, 48), initial_bit_mask);
				bitp0 += advance_amts & 0xff;
				bitp1 -= advance_amts >> 8;

				// Shuffle source bytes for both streams
				__m256i dwordsl = _mm256_shuffle_epi8(bytes, _mm256_sub_epi32(byte_shuffle, _mm256_set1_epi32(0x03020100)));
				__m256i dwordsh = _mm256_shuffle_epi8(bytes, _mm256_sub_epi32(byte_shuffle, _mm256_set1_epi32(0x04030201)));

				// Left shift the source DWords
				// the 'l' terms here contribute the low byte of every result word (not DWord!) and may have 0s in the high bits
				// the 'h' terms contribute the high byte of every result word and may have 0s in the low bits
				// OR them together and you get all the bits.
				//
				// NOTE: with AVX2 we have variable shifts but in my tests it was still faster to do it this way
				__m128i left_shift_mults = _mm_shuffle_epi8(left_shift_lut, end_bit_index_u8);
				__m256i left_shift_mult = _mm256_shuffle_epi8(
					_mm256_broadcastsi128_si256(left_shift_mults),
					_mm256_setr_epi8(0,-1,0,-1, 2,-1,2,-1, 4,-1,4,-1, 6,-1,6,-1, 1,-1,1,-1, 3,-1,3,-1, 5,-1,5,-1, 7,-1,7,-1)
				);
				__m256i shifted_dwordsl = _mm256_mullo_epi16(dwordsl, left_shift_mult);
				__m256i shifted_dwordsh = _mm256_mullo_epi16(dwordsh, left_shift_mult);
				__m256i offs_raw_bits = _mm256_or_si256(_mm256_srli_epi32(shifted_dwordsl, 8), shifted_dwordsh);

				// Interleave the forward/backward stream results
				__m256i offs_bits = forward_backward_interleave32(offs_raw_bits);

				// Output pass
				__m128i hibits_u8 = _mm_sub_epi8(_mm_andnot_si128(numraw_bits_mask, offset_u8), numraw_bits_mask);
				offsetalt_output(s, neg_offs_s32, offs_bits, hibits_u8, numraw);

				// Advance output buffer pointers
				offs_u8 += 8;
				neg_offs_s32 += 8;
			}
		}

		s->offs_u8 = offs_u8;
		s->neg_offs_s32 = neg_offs_s32;

		U16 final_bit_offs = (U16)_mm_extract_epi16(initial_bit, 0);

		s->bitp[0] = bitp0;
		s->bitp[1] = bitp1 + 16;
		s->bitc[0] = final_bit_offs & 7;
		s->bitc[1] = final_bit_offs >> 8;
	}

	return newLZ_offsetalt_decode_finish(s);
}

#else // DO_BUILD_AVX2

bool newLZ_offset44_decode_avx2(KrakenOffsetState * s)
{
	RR_ASSERT_FAILURE_ALWAYS("should not get here");

	return newLZ_offset44_decode_finish(s);
}

bool newLZ_offsetalt_decode_avx2(KrakenOffsetState * s)
{
	RR_ASSERT_FAILURE_ALWAYS("should not get here");
	
	return newLZ_offsetalt_decode_finish(s);
}

#endif // DO_BUILD_AVX2

OODLE_NS_END
