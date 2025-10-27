// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetsse4
#include "newlz_offsets.h"
#include "newlz_simd.h"

//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"

OODLE_NS_START

#ifdef DO_BUILD_SSE4

OODLE_NS_END
#include <smmintrin.h>
OODLE_NS_START

// Returns x[i] & ((1 << count[i]) - 1) + (1 << count[i]);
//
// x is U32[4]
// counts are U32[4] in [0,31]
static inline __m128i mask_to_width_and_set_top(__m128i x, __m128i counts)
{
	// Determine mask = ~0 << counts[i] = -1 << counts[i],
	// and then return (x & ~mask) - mask.
	__m128i magic = _mm_castps_si128(_mm_set1_ps(-1.0f));
	__m128i shifted_magic = _mm_add_epi32(magic, _mm_slli_epi32(counts, 23));
	__m128i mask = _mm_cvttps_epi32(_mm_castsi128_ps(shifted_magic));
	return _mm_sub_epi32(_mm_andnot_si128(mask, x), mask);
}

bool newLZ_offset44_decode_sse4(KrakenOffsetState * s)
{
	const U8 * offs_u8 = s->offs_u8;
	const U8 * offs_u8_end = s->offs_u8_end;
	SIMPLEPROFILE_SCOPE_N(offsets44_dec_sse4, offs_u8_end-offs_u8);

	if (offs_u8_end - offs_u8 >= 8 && s->bitp[1] - s->bitp[0] >= 16)
	{
		offs_u8_end -= 7;

		S32 * neg_offs_s32 = s->neg_offs_s32;
		const U8 * bitp0 = s->bitp[0];
		const U8 * bitp1 = s->bitp[1] - 16;

		__m128i left_shift_lut = _mm_setr_epi8(1,2,4,8, 16,32,64,-128, 1,2,4,8, 16,32,64,-128);
		__m128i shuf_u8to32_second = _mm_setr_epi8(4,-1,-1,-1, 5,-1,-1,-1, 6,-1,-1,-1, 7,-1,-1,-1);
		__m128i offs_bias = _mm_set1_epi32((1<<(OFFSET_RAW_BITS + 4)) - NEWLZ_MIN_OFFSET);
		__m128i escape_bias = _mm_set1_epi32(-ESCAPE_OFFSET_BIAS);
		__m128i const_0xf0 = _mm_set1_epi8(0xf0 - 0x100);
		__m128i initial_bit = _mm_cvtsi32_si128(s->bitc[0] | (s->bitc[1] << 8)); // starting bit position within initial byte in lanes 0/1 (rest zero)

		while (offs_u8 < offs_u8_end && bitp0 <= bitp1)
		{
			// Grab the next 8 offset_u8s and split them up
			__m128i offset_u8 = _mm_loadl_epi64((const __m128i *) offs_u8);
			__m128i offset_hi_nib = _mm_and_si128(offset_u8, const_0xf0);
			__m128i offs_islarge_u8 = _mm_cmpeq_epi8(offset_hi_nib, const_0xf0);
			__m128i offset_lo_nib = _mm_andnot_si128(const_0xf0, offset_u8);

			__m128i neg_offs0, neg_offs1;

			// Work out raw bit counts for small-count case
			__m128i numraw_small = _mm_add_epi8(_mm_srli_epi16(offset_hi_nib, 4), _mm_set1_epi8(OFFSET_RAW_BITS));

			// Grab source bytes for both streams
			__m128i bytes0 = _mm_loadu_si128((const __m128i *)bitp0);
			__m128i bytes1 = _mm_loadu_si128((const __m128i *)bitp1);

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
				__m128i byte_shuffle0 = _mm_shuffle_epi8(end_byte_index_u8, _mm_setr_epi8(0,0,0,0, 2,2,2,2, 4,4,4,4, 6,6,6,6));
				__m128i byte_shuffle1 = _mm_shuffle_epi8(end_byte_index_u8, _mm_setr_epi8(1,1,1,1, 3,3,3,3, 5,5,5,5, 7,7,7,7));
				byte_shuffle0 = _mm_sub_epi8(byte_shuffle0, _mm_setr_epi8(0,1,2,3, 0,1,2,3, 0,1,2,3, 0,1,2,3));
				byte_shuffle1 = _mm_sub_epi8(_mm_setr_epi8(15,16,17,18, 15,16,17,18, 15,16,17,18, 15,16,17,18), byte_shuffle1);

				// Bit buffer advance
				U32 advance_amts = _mm_extract_epi16(end_byte_index_u8, 3);
				initial_bit = _mm_and_si128(_mm_srli_epi64(end_bit_index_u8, 48), _mm_set1_epi8(7));
				bitp0 += advance_amts & 0xff;
				bitp1 -= advance_amts >> 8;

				// Shuffle source bytes for both streams
				__m128i dwords0 = _mm_shuffle_epi8(bytes0, byte_shuffle0);
				__m128i dwords1 = _mm_shuffle_epi8(bytes1, byte_shuffle1);

				// Left shift the source DWords
				__m128i left_shift_mults = _mm_shuffle_epi8(left_shift_lut, end_bit_index_u8);
				__m128i left_shift_mult0 = _mm_shuffle_epi8(left_shift_mults, _mm_setr_epi8(0,-1,-1,-1, 2,-1,-1,-1, 4,-1,-1,-1, 6,-1,-1,-1));
				__m128i left_shift_mult1 = _mm_shuffle_epi8(left_shift_mults, _mm_setr_epi8(1,-1,-1,-1, 3,-1,-1,-1, 5,-1,-1,-1, 7,-1,-1,-1));
				__m128i shifted_dwords0 = _mm_mullo_epi32(dwords0, left_shift_mult0);
				__m128i shifted_dwords1 = _mm_mullo_epi32(dwords1, left_shift_mult1);
				__m128i offs_raw_bits0 = _mm_srli_epi32(shifted_dwords0, 8);
				__m128i offs_raw_bits1 = _mm_srli_epi32(shifted_dwords1, 8);

				// Interleave the forward/backward stream results
				__m128i offs_bits0 = _mm_unpacklo_epi32(offs_raw_bits0, offs_raw_bits1);
				__m128i offs_bits1 = _mm_unpackhi_epi32(offs_raw_bits0, offs_raw_bits1);

				// Compute the masked, biased offsets
				__m128i offs_masked0 = mask_to_width_and_set_top(offs_bits0, _mm_cvtepu8_epi32(numraw_small));
				__m128i offs_masked1 = mask_to_width_and_set_top(offs_bits1, _mm_shuffle_epi8(numraw_small, shuf_u8to32_second));

				// Compute the negated offsets
				neg_offs0 = _mm_sub_epi32(offs_bias, _mm_slli_epi32(offs_masked0, 4));
				neg_offs1 = _mm_sub_epi32(offs_bias, _mm_slli_epi32(offs_masked1, 4));

				neg_offs0 = _mm_sub_epi32(neg_offs0, _mm_cvtepu8_epi32(offset_lo_nib));
				neg_offs1 = _mm_sub_epi32(neg_offs1, _mm_shuffle_epi8(offset_lo_nib, shuf_u8to32_second));
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
				__m128i byte_shuffle0 = _mm_shuffle_epi8(end_byte_index_u8, _mm_setr_epi8(0,0,0,0, 2,2,2,2, 4,4,4,4, 6,6,6,6));
				__m128i byte_shuffle1 = _mm_shuffle_epi8(end_byte_index_u8, _mm_setr_epi8(1,1,1,1, 3,3,3,3, 5,5,5,5, 7,7,7,7));
				byte_shuffle0 = _mm_sub_epi8(byte_shuffle0, _mm_setr_epi8(0,1,2,3, 0,1,2,3, 0,1,2,3, 0,1,2,3));
				byte_shuffle1 = _mm_sub_epi8(_mm_setr_epi8(15,16,17,18, 15,16,17,18, 15,16,17,18, 15,16,17,18), byte_shuffle1);

				// Bit buffer advance
				U32 advance_amts = _mm_extract_epi16(end_byte_index_u8, 3);
				initial_bit = _mm_and_si128(_mm_srli_epi64(end_bit_index_u8, 48), _mm_set1_epi8(7));
				bitp0 += advance_amts & 0xff;
				bitp1 -= advance_amts >> 8;

				// Shuffle source bytes for both streams
				__m128i dwords0l = _mm_shuffle_epi8(bytes0, byte_shuffle0);
				__m128i dwords1l = _mm_shuffle_epi8(bytes1, byte_shuffle1);
				__m128i dwords0h = _mm_shuffle_epi8(bytes0, _mm_sub_epi8(byte_shuffle0, _mm_set1_epi8(1)));
				__m128i dwords1h = _mm_shuffle_epi8(bytes1, _mm_add_epi8(byte_shuffle1, _mm_set1_epi8(1)));

				// Left shift the source DWords
				// the 'l' terms here contribute the low byte of every result word (not DWord!) and may have 0s in the high bits
				// the 'h' terms contribute the high byte of every result word and may have 0s in the low bits
				// OR them together and you get all the bits.
				__m128i left_shift_mults = _mm_shuffle_epi8(left_shift_lut, end_bit_index_u8);
				__m128i left_shift_mult0 = _mm_shuffle_epi8(left_shift_mults, _mm_setr_epi8(0,-1,0,-1, 2,-1,2,-1, 4,-1,4,-1, 6,-1,6,-1));
				__m128i left_shift_mult1 = _mm_shuffle_epi8(left_shift_mults, _mm_setr_epi8(1,-1,1,-1, 3,-1,3,-1, 5,-1,5,-1, 7,-1,7,-1));
				__m128i shifted_dwords0l = _mm_mullo_epi16(dwords0l, left_shift_mult0);
				__m128i shifted_dwords1l = _mm_mullo_epi16(dwords1l, left_shift_mult1);
				__m128i shifted_dwords0h = _mm_mullo_epi16(dwords0h, left_shift_mult0);
				__m128i shifted_dwords1h = _mm_mullo_epi16(dwords1h, left_shift_mult1);
				__m128i offs_raw_bits0 = _mm_or_si128(_mm_srli_epi32(shifted_dwords0l, 8), shifted_dwords0h);
				__m128i offs_raw_bits1 = _mm_or_si128(_mm_srli_epi32(shifted_dwords1l, 8), shifted_dwords1h);

				// Interleave the forward/backward stream results
				__m128i offs_bits0 = _mm_unpacklo_epi32(offs_raw_bits0, offs_raw_bits1);
				__m128i offs_bits1 = _mm_unpackhi_epi32(offs_raw_bits0, offs_raw_bits1);

				// Compute the masked, biased offsets
				__m128i offs_masked0 = mask_to_width_and_set_top(offs_bits0, _mm_cvtepu8_epi32(numraw));
				__m128i offs_masked1 = mask_to_width_and_set_top(offs_bits1, _mm_shuffle_epi8(numraw, shuf_u8to32_second));

				// Compute the negated offsets
				__m128i neg_offs_small0 = _mm_sub_epi32(offs_bias, _mm_slli_epi32(offs_masked0, 4));
				__m128i neg_offs_small1 = _mm_sub_epi32(offs_bias, _mm_slli_epi32(offs_masked1, 4));

				neg_offs_small0 = _mm_sub_epi32(neg_offs_small0, _mm_cvtepu8_epi32(offset_lo_nib));
				neg_offs_small1 = _mm_sub_epi32(neg_offs_small1, _mm_shuffle_epi8(offset_lo_nib, shuf_u8to32_second));

				__m128i neg_offs_large0 = _mm_sub_epi32(escape_bias, offs_masked0);
				__m128i neg_offs_large1 = _mm_sub_epi32(escape_bias, offs_masked1);

				neg_offs0 = _mm_blendv_epi8(neg_offs_small0, neg_offs_large0, _mm_cvtepi8_epi32(offs_islarge_u8));
				neg_offs1 = _mm_blendv_epi8(neg_offs_small1, neg_offs_large1, _mm_cvtepi8_epi32(_mm_srli_epi64(offs_islarge_u8, 32)));
			}

			// Store offsets
			_mm_storeu_si128((__m128i *) &neg_offs_s32[0], neg_offs0);
			_mm_storeu_si128((__m128i *) &neg_offs_s32[4], neg_offs1);

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

static RADFORCEINLINE __m128i offsetalt_gen_hibits_flthi(__m128i offset_u16)
{
	__m128i hibits_flthi = _mm_slli_epi16(offset_u16, 7 - OFFSET_ALT_NUM_UNDER_BITS); // align value so numraw ends up in float32 exponent field
	hibits_flthi = _mm_add_epi16(hibits_flthi, _mm_set1_epi16((127 + OFFSET_ALT_NUM_UNDER_BITS) << 7)); // =high word of final float32

	return hibits_flthi;
}

static RADFORCEINLINE void offsetalt_output_narrow(KrakenOffsetState * s, S32 * neg_offs_s32, __m128i offs_bits_u16, __m128i offset_u16, __m128i numraw_u8_doubled)
{
	// Generate masks
	// In the narrow path, we can mask the low bits while they're 16 bits wide.
	// Generate bit masks per-byte using a LUT indexed with numraw (low byte)
	// and saturate(numraw-8) (high byte).
	const __m128i mask_lut = _mm_setr_epi8(0x00,0x01,0x03,0x07, 0x0f,0x1f,0x3f,0x7f, -1,-1,-1,-1, -1,-1,-1,-1);

	__m128i numraw_biased = _mm_subs_epu8(numraw_u8_doubled, _mm_set1_epi16(0x0800)); // 0,8, 0,8, 0,8...
	__m128i mask_bits = _mm_shuffle_epi8(mask_lut, numraw_biased); // (1<<numraw)-1 per 16-bit lane
	__m128i offs_bits_masked_u16 = _mm_and_si128(offs_bits_u16, mask_bits);

	__m128i hibits_flthi = offsetalt_gen_hibits_flthi(offset_u16);

	// Combine the offsets
	__m128i offs_bits0 = _mm_unpacklo_epi16(offs_bits_masked_u16, _mm_setzero_si128());
	__m128i hibits_flt0 = _mm_unpacklo_epi16(_mm_setzero_si128(), hibits_flthi);
	__m128i offs_combined0 = _mm_or_si128(offs_bits0, _mm_cvttps_epi32(_mm_castsi128_ps(hibits_flt0)));

	__m128i offs_bits1 = _mm_unpackhi_epi16(offs_bits_masked_u16, _mm_setzero_si128());
	__m128i hibits_flt1 = _mm_unpackhi_epi16(_mm_setzero_si128(), hibits_flthi);
	__m128i offs_combined1 = _mm_or_si128(offs_bits1, _mm_cvttps_epi32(_mm_castsi128_ps(hibits_flt1)));

	// Compute the negated offsets
	__m128i neg_offs0 = _mm_sub_epi32(_mm_set1_epi32(OFFSET_ALT_BIAS), offs_combined0);
	__m128i neg_offs1 = _mm_sub_epi32(_mm_set1_epi32(OFFSET_ALT_BIAS), offs_combined1);

	// Store offsets
	_mm_storeu_si128((__m128i *) (neg_offs_s32 + 0), neg_offs0);
	_mm_storeu_si128((__m128i *) (neg_offs_s32 + 4), neg_offs1);

	OFFSET_CHECK(&neg_offs_s32[0]);
	OFFSET_CHECK(&neg_offs_s32[1]);
	OFFSET_CHECK(&neg_offs_s32[2]);
	OFFSET_CHECK(&neg_offs_s32[3]);
	OFFSET_CHECK(&neg_offs_s32[4]);
	OFFSET_CHECK(&neg_offs_s32[5]);
	OFFSET_CHECK(&neg_offs_s32[6]);
	OFFSET_CHECK(&neg_offs_s32[7]);
}

bool newLZ_offsetalt_decode_sse4(KrakenOffsetState * s)
{
	const U8 * offs_u8 = s->offs_u8;
	const U8 * offs_u8_end = s->offs_u8_end;
	SIMPLEPROFILE_SCOPE_N(offsetsalt_dec_sse4, offs_u8_end-offs_u8);

	if (offs_u8_end - offs_u8 >= 16 && s->bitp[1] - s->bitp[0] >= 16)
	{
		offs_u8_end -= 15;

		S32 * neg_offs_s32 = s->neg_offs_s32;
		const U8 * bitp0 = s->bitp[0];
		const U8 * bitp1 = s->bitp[1] - 16;

		static const U8 OFFSET_ALT_UNDER_MASK  = (1 << OFFSET_ALT_NUM_UNDER_BITS) - 1;
		static const U8 OFFSET_ALT_NUMRAW_MASK = OFFSET_ALT_UNDER_MASK ^ 0xff;

		__m128i left_shift_lut = _mm_setr_epi8(1,2,4,8, 16,32,64,-128, 1,2,4,8, 16,32,64,-128);
		__m128i initial_bit = _mm_cvtsi32_si128(s->bitc[0] | (s->bitc[1] << 8)); // starting bit position within initial byte in lanes 0/1 (rest zero)
		__m128i initial_bit_mask = _mm_setr_epi8(7,7,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0);

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
				__m128i byte_shuffle0 = _mm_shuffle_epi8(end_byte_index_u8, _mm_setr_epi8(0,0, 2,2, 4,4, 6,6, 8,8, 10,10, 12,12, 14,14));
				__m128i byte_shuffle1 = _mm_shuffle_epi8(end_byte_index_u8, _mm_setr_epi8(1,1, 3,3, 5,5, 7,7, 9,9, 11,11, 13,13, 15,15));
				byte_shuffle0 = _mm_sub_epi8(byte_shuffle0, _mm_set1_epi16(0x0100));
				byte_shuffle1 = _mm_sub_epi8(_mm_set1_epi16(0x100f), byte_shuffle1);

				// Bit buffer advance
				U32 advance_amts = _mm_extract_epi16(end_byte_index_u8, 7);
				initial_bit = _mm_and_si128(_mm_srli_si128(end_bit_index_u8, 14), initial_bit_mask);
				bitp0 += advance_amts & 0xff;
				bitp1 -= advance_amts >> 8;

				// Shuffle source bytes for both streams
				__m128i words0l = _mm_shuffle_epi8(bytes0, byte_shuffle0);
				__m128i words1l = _mm_shuffle_epi8(bytes1, byte_shuffle1);
				__m128i words0h = _mm_shuffle_epi8(bytes0, _mm_sub_epi8(byte_shuffle0, _mm_set1_epi8(1)));
				__m128i words1h = _mm_shuffle_epi8(bytes1, _mm_add_epi8(byte_shuffle1, _mm_set1_epi8(1)));

				// Left shift the source words
				// the 'l' terms here contribute the low byte of every result word and may have 0s in the high bits
				// the 'h' terms contribute the high byte of every result word and may have 0s in the low bits
				// OR them together and you get all the bits.
				__m128i left_shift_mults = _mm_shuffle_epi8(left_shift_lut, end_bit_index_u8);
				__m128i left_shift_mult0 = _mm_and_si128(left_shift_mults, _mm_set1_epi16(0xff));
				__m128i left_shift_mult1 = _mm_srli_epi16(left_shift_mults, 8);
				__m128i shifted_words0l = _mm_mullo_epi16(words0l, left_shift_mult0);
				__m128i shifted_words1l = _mm_mullo_epi16(words1l, left_shift_mult1);
				__m128i shifted_words0h = _mm_mullo_epi16(words0h, left_shift_mult0);
				__m128i shifted_words1h = _mm_mullo_epi16(words1h, left_shift_mult1);
				__m128i offs_raw_bits0 = _mm_or_si128(_mm_srli_epi16(shifted_words0l, 8), shifted_words0h);
				__m128i offs_raw_bits1 = _mm_or_si128(_mm_srli_epi16(shifted_words1l, 8), shifted_words1h);

				// Interleave the forward/backward stream results
				__m128i offs_bits0 = _mm_unpacklo_epi16(offs_raw_bits0, offs_raw_bits1);
				__m128i offs_bits1 = _mm_unpackhi_epi16(offs_raw_bits0, offs_raw_bits1);

				// Prepare the magic floats to mask the low bits and combine the offset low/high halves
				__m128i offset_u16_0 = _mm_unpacklo_epi8(offset_u8, _mm_setzero_si128());
				__m128i numraw_doubled_0 = _mm_unpacklo_epi8(numraw, numraw);
				offsetalt_output_narrow(s, neg_offs_s32 + 0, offs_bits0, offset_u16_0, numraw_doubled_0);

				__m128i offset_u16_1 = _mm_unpackhi_epi8(offset_u8, _mm_setzero_si128());
				__m128i numraw_doubled_1 = _mm_unpackhi_epi8(numraw, numraw);
				offsetalt_output_narrow(s, neg_offs_s32 + 8, offs_bits1, offset_u16_1, numraw_doubled_1);

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
				__m128i byte_shuffle0 = _mm_shuffle_epi8(end_byte_index_u8, _mm_setr_epi8(0,0,0,0, 2,2,2,2, 4,4,4,4, 6,6,6,6));
				__m128i byte_shuffle1 = _mm_shuffle_epi8(end_byte_index_u8, _mm_setr_epi8(1,1,1,1, 3,3,3,3, 5,5,5,5, 7,7,7,7));
				byte_shuffle0 = _mm_sub_epi8(byte_shuffle0, _mm_setr_epi8(0,1,2,3, 0,1,2,3, 0,1,2,3, 0,1,2,3));
				byte_shuffle1 = _mm_sub_epi8(_mm_setr_epi8(15,16,17,18, 15,16,17,18, 15,16,17,18, 15,16,17,18), byte_shuffle1);

				// Bit buffer advance
				U32 advance_amts = _mm_extract_epi16(end_byte_index_u8, 3);
				initial_bit = _mm_and_si128(_mm_srli_epi64(end_bit_index_u8, 48), initial_bit_mask);
				bitp0 += advance_amts & 0xff;
				bitp1 -= advance_amts >> 8;

				// Shuffle source bytes for both streams
				__m128i dwords0l = _mm_shuffle_epi8(bytes0, byte_shuffle0);
				__m128i dwords1l = _mm_shuffle_epi8(bytes1, byte_shuffle1);
				__m128i dwords0h = _mm_shuffle_epi8(bytes0, _mm_sub_epi8(byte_shuffle0, _mm_set1_epi8(1)));
				__m128i dwords1h = _mm_shuffle_epi8(bytes1, _mm_add_epi8(byte_shuffle1, _mm_set1_epi8(1)));

				// Left shift the source DWords
				// the 'l' terms here contribute the low byte of every result word (not DWord!) and may have 0s in the high bits
				// the 'h' terms contribute the high byte of every result word and may have 0s in the low bits
				// OR them together and you get all the bits.
				__m128i left_shift_mults = _mm_shuffle_epi8(left_shift_lut, end_bit_index_u8);
				__m128i left_shift_mult0 = _mm_shuffle_epi8(left_shift_mults, _mm_setr_epi8(0,-1,0,-1, 2,-1,2,-1, 4,-1,4,-1, 6,-1,6,-1));
				__m128i left_shift_mult1 = _mm_shuffle_epi8(left_shift_mults, _mm_setr_epi8(1,-1,1,-1, 3,-1,3,-1, 5,-1,5,-1, 7,-1,7,-1));
				__m128i shifted_dwords0l = _mm_mullo_epi16(dwords0l, left_shift_mult0);
				__m128i shifted_dwords1l = _mm_mullo_epi16(dwords1l, left_shift_mult1);
				__m128i shifted_dwords0h = _mm_mullo_epi16(dwords0h, left_shift_mult0);
				__m128i shifted_dwords1h = _mm_mullo_epi16(dwords1h, left_shift_mult1);
				__m128i offs_raw_bits0 = _mm_or_si128(_mm_srli_epi32(shifted_dwords0l, 8), shifted_dwords0h);
				__m128i offs_raw_bits1 = _mm_or_si128(_mm_srli_epi32(shifted_dwords1l, 8), shifted_dwords1h);

				// Interleave the forward/backward stream results
				__m128i offs_bits0 = _mm_unpacklo_epi32(offs_raw_bits0, offs_raw_bits1);
				__m128i offs_bits1 = _mm_unpackhi_epi32(offs_raw_bits0, offs_raw_bits1);

				// Prepare the magic floats to mask the low bits and combine the offset low/high halves
				__m128i offset_u16 = _mm_unpacklo_epi8(offset_u8, _mm_setzero_si128());
				__m128i mask_flthi = _mm_and_si128(offset_u16, _mm_set1_epi16(OFFSET_ALT_NUMRAW_MASK)); // just the exponent part
				mask_flthi = _mm_slli_epi16(mask_flthi, 7 - OFFSET_ALT_NUM_UNDER_BITS); // shift to align with float32 exponent field
				mask_flthi = _mm_add_epi16(mask_flthi, _mm_set1_epi16(0xbf80 - 0x10000)); // adds high word of float32(-1.0)

				__m128i hibits_flthi = offsetalt_gen_hibits_flthi(offset_u16);

				// Combine the offsets
				__m128i hibits_flt0 = _mm_unpacklo_epi16(_mm_setzero_si128(), hibits_flthi);
				__m128i mask_flt0 = _mm_unpacklo_epi16(_mm_setzero_si128(), mask_flthi);
				__m128i hibits_int0 = _mm_cvttps_epi32(_mm_castsi128_ps(hibits_flt0));
				__m128i mask_int0 = _mm_cvttps_epi32(_mm_castsi128_ps(mask_flt0));
				__m128i offs_combined0 = _mm_or_si128(_mm_andnot_si128(mask_int0, offs_bits0), hibits_int0);

				__m128i hibits_flt1 = _mm_unpackhi_epi16(_mm_setzero_si128(), hibits_flthi);
				__m128i mask_flt1 = _mm_unpackhi_epi16(_mm_setzero_si128(), mask_flthi);
				__m128i hibits_int1 = _mm_cvttps_epi32(_mm_castsi128_ps(hibits_flt1));
				__m128i mask_int1 = _mm_cvttps_epi32(_mm_castsi128_ps(mask_flt1));
				__m128i offs_combined1 = _mm_or_si128(_mm_andnot_si128(mask_int1, offs_bits1), hibits_int1);

				// Compute the negated offsets
				__m128i neg_offs0 = _mm_sub_epi32(_mm_set1_epi32(OFFSET_ALT_BIAS), offs_combined0);
				__m128i neg_offs1 = _mm_sub_epi32(_mm_set1_epi32(OFFSET_ALT_BIAS), offs_combined1);

				// Store offsets
				_mm_storeu_si128((__m128i *) (neg_offs_s32 + 0), neg_offs0);
				_mm_storeu_si128((__m128i *) (neg_offs_s32 + 4), neg_offs1);

				OFFSET_CHECK(&neg_offs_s32[0]);
				OFFSET_CHECK(&neg_offs_s32[1]);
				OFFSET_CHECK(&neg_offs_s32[2]);
				OFFSET_CHECK(&neg_offs_s32[3]);
				OFFSET_CHECK(&neg_offs_s32[4]);
				OFFSET_CHECK(&neg_offs_s32[5]);
				OFFSET_CHECK(&neg_offs_s32[6]);
				OFFSET_CHECK(&neg_offs_s32[7]);

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

#else // DO_BUILD_SSE4

bool newLZ_offset44_decode_sse4(KrakenOffsetState * s)
{
	RR_ASSERT_FAILURE_ALWAYS("should not get here");
	
	return newLZ_offset44_decode_finish(s);
}

bool newLZ_offsetalt_decode_sse4(KrakenOffsetState * s)
{
	RR_ASSERT_FAILURE_ALWAYS("should not get here");
	
	return newLZ_offsetalt_decode_finish(s);
}

#endif // DO_BUILD_SSE4

OODLE_NS_END
