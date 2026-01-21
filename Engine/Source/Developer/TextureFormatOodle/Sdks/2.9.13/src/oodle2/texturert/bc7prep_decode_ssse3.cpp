// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetssse3

#include "bc7prep_decode.h"
#include "rrsimd.h"
#include "rrbits.h"

#ifdef DO_BUILD_SSSE3

#define BC7PREP_SSSE3
#include "bc7prep_decode_sse.inl"

OODLE_NS_START
namespace bc7prep {

template<typename Tsplit, bool t_switch_colorspace>
UINTa un_munge_bc7_mode1_ssse3(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target)
{
	UINTa iblock = 0;

	const __m128i mask_6bits	= _mm_set1_epi16(0x3f);
	const __m128i mask_gb		= _mm_set1_epi16(0x3e); // NOTE: bottom bits of g/b left clear
	const __m128i shuf_extra	= _mm_setr_epi8(-1,0, -1,0, -1,0, -1,0, -1,8, -1,8, -1,8, -1,8);
	const __m128i mul_extra		= _mm_setr_epi16(0x100, 0x40, 0x10, 4, 0x100, 0x40, 0x10, 4);
	const __m128i mask_lobit 	= _mm_set1_epi16(1);
	const __m128i mul_compact1	= _mm_setr_epi16(1,1<<6, 1,1<<6, 1,1<<6, 1,1<<6);
	const __m128i mul_compact2	= _mm_setr_epi16(1,1<<12, 1,1<<12, 1,1<<12, 1,1<<12);
	const __m128i mask_compact2	= _mm_setr_epi32(0xffffff, 0, 0xffffff, 0);
	const __m128i shuf_rgfinal	= _mm_setr_epi8(-1,0,1,2, 8,9,10,-1, -1,4,5,6, 12,13,14,-1);
	const __m128i mode_bits		= _mm_setr_epi32(2, 0, 2, 0);
	const __m128i mask_partid	= _mm_setr_epi32(0xfc, 0, 0xfc, 0);
	const __m128i mask_pindex	= _mm_setr_epi32(-(1<<16), -1, -(1<<16), -1);

	for (; iblock < (nblocks & ~1); iblock += 2)
	{
		// Load both halves
		Vec128 rgbs, extra;
		vec_load_pair<Tsplit>(rgbs, extra, first_ptr, second_ptr, iblock);

		// extract the individual payload bits from the R6G5B5 encoding
		Vec128 rbits = _mm_and_si128(rgbs, mask_6bits);
		Vec128 gbits = _mm_and_si128(_mm_srli_epi16(rgbs, 5), mask_gb);
		Vec128 bbits = _mm_and_si128(_mm_srli_epi16(rgbs, 10), mask_gb);

		// bottom bits of g/b are stored in low 8 bits of extra
		// use a shuffle to create four 16-bit copies of the 8 extra bits
		// then a multiply-high to shift everything where it needs to go
		Vec128 extra_spread = _mm_shuffle_epi8(extra, shuf_extra);
		Vec128 extra_shifted = _mm_mulhi_epu16(extra_spread, mul_extra);
		gbits = _mm_or_si128(gbits, _mm_and_si128(extra_shifted, mask_lobit));
		bbits = _mm_or_si128(bbits, _mm_and_si128(_mm_srli_epi16(extra_shifted, 1), mask_lobit));

		// we're now in the perfect spot for the color space transform
		if (t_switch_colorspace)
		{
			Vec128 y = rbits;
			Vec128 cr = gbits;
			Vec128 cb = bbits;
			rbits = _mm_and_si128(_mm_add_epi16(y, cr), mask_6bits);
			gbits = y;
			bbits = _mm_and_si128(_mm_add_epi16(y, cb), mask_6bits);
		}

		// starting with 16-bit lanes with 6 low bits used each, compact r, g, b
		rbits = _mm_madd_epi16(rbits, mul_compact1);
		gbits = _mm_madd_epi16(gbits, mul_compact1);
		bbits = _mm_madd_epi16(bbits, mul_compact1);
		// now have 32-bit lanes with 12 low bits used each

		// compact (second pass); narrow r/g opportunistically here (to increase density)
		Vec128 rgbits = _mm_packs_epi32(rbits, gbits); // now 16-bit lanes with 12 bits used each
		Vec128 rgpacked = _mm_madd_epi16(rgbits, mul_compact2); // back to 32-bit lanes: (a.r0r1r2r3, b.r0r1r2r3, a.g0g1g2g3, b.g0g1g2g3)
		Vec128 rgfinal = _mm_shuffle_epi8(rgpacked, shuf_rgfinal); // assemble r/g output bits

		// b gets handled by itself
		bbits = _mm_and_si128(_mm_or_si128(bbits, _mm_srli_epi64(bbits, 20)), mask_compact2);

		// assemble low halves of output
		Vec128 lo = mode_bits;
		lo = _mm_or_si128(lo, _mm_and_si128(_mm_srli_epi32(extra, 6), mask_partid));
		lo = _mm_or_si128(lo, rgfinal);
		lo = _mm_or_si128(lo, _mm_slli_epi64(bbits, 56));

		// assemble high halves of output
		Vec128 hi = _mm_srli_epi64(bbits, 8);
		hi = _mm_or_si128(hi, _mm_and_si128(extra, mask_pindex));

		// interleave and write
		Vec128 out0 = _mm_unpacklo_epi64(lo, hi);
		Vec128 out1 = _mm_unpackhi_epi64(lo, hi);
		store128u(out_block + bc7prep::target_offs(target[iblock + 0]), out0);
		store128u(out_block + bc7prep::target_offs(target[iblock + 1]), out1);
	}

	return iblock;
}

template<typename Tsplit, bool t_switch_colorspace>
UINTa un_munge_bc7_mode2_ssse3(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target)
{
	UINTa iblock = 0;

	const __m128i mask_chan0 		= _mm_set1_epi16(0x1f << 1);
	const __m128i mask_index		= _mm_setr_epi32(0, 0, 0, -(1<<3));

	const __m128i mul_compactr		= _mm_setr_epi16(1<<0,1<<5, 1<<2,1<<7, 1<<4,1<<9, 0,0);
	const __m128i shuf_compactr1	= _mm_setr_epi8(-1,  0, 1, 8, 9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
	const __m128i shuf_compactr2	= _mm_setr_epi8(-1, -1, 4, 5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);

	const __m128i mul_compactg		= _mm_setr_epi16(1<<6,1<<11, 1<<0,1<<5, 1<<2,1<<7, 0,0);
	const __m128i shuf_compactg1	= _mm_setr_epi8(-1, -1,-1,-1, 0, 1, 2, 8, 9,-1,-1,-1,-1,-1,-1,-1);
	const __m128i shuf_compactg2	= _mm_setr_epi8(-1, -1,-1,-1,-1,-1, 4, 5,-1,-1,-1,-1,-1,-1,-1,-1);

	const __m128i mul_compactb		= _mm_setr_epi16(1<<4,1<<9, 1<<6,1<<11, 1<<0,1<<5, 0,0);
	const __m128i shuf_compactb1	= _mm_setr_epi8(-1,-1,-1,-1,-1,-1,-1,-1,  0,1, -1,8,  9,-1, -1,-1);
	const __m128i shuf_compactb2	= _mm_setr_epi8(-1,-1,-1,-1,-1,-1,-1,-1, -1,4,  5,6, -1,-1, -1,-1);

	for (; iblock < nblocks; iblock++)
	{
		Vec128 endpts, indbits;
		vec_load_single<Tsplit>(endpts, indbits, first_ptr, second_ptr, iblock);

		// Grab partition bits from the LSBs of the endpoint values
		Vec128 partbits16 = _mm_slli_epi16(endpts, 15); // either 0 or -1<<15
		Vec128 partbits8 = _mm_packs_epi16(partbits16, partbits16);
		U32 partbits = _mm_movemask_epi8(partbits8) & 0x3f;

		// Align r/g/b bits to same position (don't fully mask just yet)
		Vec128 rbits = _mm_and_si128(endpts, mask_chan0);
		Vec128 gbits = _mm_srli_epi16(endpts, 5);
		Vec128 bbits = _mm_srli_epi16(endpts, 10);

		// Do the color space switch if desired
		if (t_switch_colorspace)
		{
			Vec128 y = rbits;
			Vec128 cr = gbits;
			Vec128 cb = bbits;

			gbits = y;
			rbits = _mm_add_epi16(y, cr);
			bbits = _mm_add_epi16(cb, y);

			rbits = _mm_and_si128(rbits, mask_chan0);
			bbits = _mm_and_si128(bbits, mask_chan0);
		}
		else
		{
			gbits = _mm_and_si128(gbits, mask_chan0);
			bbits = _mm_and_si128(bbits, mask_chan0);
		}

		// Start assembling output vector
		Vec128 out = _mm_cvtsi32_si128((partbits << 3) + 4); // mode + partbits
		out = _mm_or_si128(out, _mm_and_si128(indbits, mask_index));

		// Red
		Vec128 rcompact = _mm_madd_epi16(rbits, mul_compactr);
		// we now have 10-bit values in 32-bit lanes, aligned to be a shuffle away from where we need them:
		// namely each group starts in the correct position within their respective bytes
		//   rcompact  = (r0r1<<1, r2r3<<3, r4r5<<5, 0)
		Vec128 rfinal = _mm_or_si128(_mm_shuffle_epi8(rcompact, shuf_compactr1), _mm_shuffle_epi8(rcompact, shuf_compactr2));
		out = _mm_or_si128(out, rfinal);

		// Green
		Vec128 gcompact = _mm_madd_epi16(gbits, mul_compactg);
		//   gcompact  = (g0g1<<7, g2g3<<1, g4g5<<3, 0)
		Vec128 gfinal = _mm_or_si128(_mm_shuffle_epi8(gcompact, shuf_compactg1), _mm_shuffle_epi8(gcompact, shuf_compactg2));
		out = _mm_or_si128(out, gfinal);

		// Blue
		Vec128 bcompact = _mm_madd_epi16(bbits, mul_compactb);
		//   bcompact  = (b0b1<<5, b2b3<<7, b4b5<<1, 0)
		Vec128 bfinal = _mm_or_si128(_mm_shuffle_epi8(bcompact, shuf_compactb1), _mm_shuffle_epi8(bcompact, shuf_compactb2));
		out = _mm_or_si128(out, bfinal);

		store128u(out_block + bc7prep::target_offs(target[iblock]), out);
	}

	return iblock;
}

template<typename Tsplit, bool t_switch_colorspace>
UINTa un_munge_bc7_mode3_ssse3(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target)
{
	UINTa iblock = 0;

	const __m128i shuf_ybroad		= _mm_setr_epi8(0,0,0, 3,3,3, 6,6,6, 9,9,9, -1,-1,-1,-1);
	const __m128i shuf_crcb			= _mm_setr_epi8(1,-1,2, 4,-1,5, 7,-1,8, 10,-1,11, -1,-1,-1,-1);
	const __m128i shuf_deint		= _mm_setr_epi8(0,3,6,9, 1,4,7,10, 2,5,8,11, -1,-1,-1,-1);
	const __m128i mask_lo7bits		= _mm_set1_epi8(0x7f);
	const __m128i mul_compact1		= _mm_setr_epi8(1,-128, 1,-128, 1,-128, 1,-128, 1,-128, 1,-128, 1,-128, 1,-128);
	const __m128i mul_compact2		= _mm_setr_epi16(4,1, 4,1, 4,1, 4,1);
	const __m128i mask_bfinal		= _mm_setr_epi32(0, 0, 0x3ffffffc,0);
	const __m128i mask_compact3r	= _mm_setr_epi32(0x3ffffffc, 0, 0, 0);
	const __m128i mask_compact3g 	= _mm_setr_epi32(-(1 << 30), 0x3ffffff, 0, 0);
	const __m128i mask_index		= _mm_setr_epi32(0, 0, 0, -1);

	for (; iblock < nblocks; iblock++)
	{
		Vec128 endpts, indbits;
		vec_load_single<Tsplit>(endpts, indbits, first_ptr, second_ptr, iblock);

		U32 partbits = _mm_movemask_epi8(endpts); // can grab partition bits immediately

		if (t_switch_colorspace)
		{
			Vec128 ybroad = _mm_shuffle_epi8(endpts, shuf_ybroad);
			Vec128 crcb = _mm_shuffle_epi8(endpts, shuf_crcb);
			endpts = _mm_add_epi8(ybroad, crcb);
		}

		// de-interleave R,G,B
		Vec128 deint = _mm_shuffle_epi8(endpts, shuf_deint);
		Vec128 deint7 = _mm_and_si128(deint, mask_lo7bits);
		// we now have 7-bit values low in 8-bit lanes

		// compact pass 1
		Vec128 compact1 = _mm_maddubs_epi16(mul_compact1, deint7);
		// now: 14-bit values low in 16-bit lanes

		// compact pass 2
		Vec128 compact2 = _mm_mullo_epi16(compact1, mul_compact2);
		// now: 28-bit values shifted by 2 in 32-bit lanes

		// compact pass 3: get rid of the 4-bit gap between r and g;
		// we leave b alone here, since the bits are already exactly
		// where we need them!
		Vec128 bfinal = _mm_and_si128(compact2, mask_bfinal);
		Vec128 rmasked = _mm_and_si128(compact2, mask_compact3r);
		Vec128 galigned = _mm_and_si128(_mm_srli_epi64(compact2, 4), mask_compact3g);
		Vec128 rgmerged = _mm_or_si128(rmasked, galigned); // 56 bits of r+g, shifted by 2 in a 64-bit lane

		// build the result block
		U32 mode_and_part = 0x8 | ((partbits & 0x3f) << 4);
		Vec128 result = _mm_or_si128(bfinal, _mm_cvtsi32_si128(mode_and_part));
		Vec128 final_part = _mm_slli_si128(_mm_cvtsi32_si128(partbits & 0xc0), 11);
		Vec128 final_part_and_index = _mm_or_si128(final_part, _mm_and_si128(indbits, mask_index));

		result = _mm_or_si128(result, _mm_slli_si128(rgmerged, 1));
		result = _mm_or_si128(result, final_part_and_index);

		store128u(out_block + bc7prep::target_offs(target[iblock]), result);
	}

	return iblock;
}

template<typename Tsplit, bool t_switch_colorspace>
UINTa un_munge_bc7_mode4_ssse3(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target)
{
	UINTa iblock = 0;

	const __m128i mask_chan0	= _mm_setr_epi32(0xffc, 0, 0xffc, 0);
	const __m128i mul_ybroad	= _mm_setr_epi32(1 + (1<<10) + (1<<20), 0, 1 + (1<<10) + (1<<20), 0);
	const __m128i mask_chan23	= _mm_setr_epi32(-(1 << 22), 0x3ff, -(1 << 22), 0x3ff);
	const __m128i mask_nonmsb	= _mm_setr_epi32(0x7bdef7bf,0x1ef, 0x7bdef7bf,0x1ef);
	const __m128i mask_rgba		= _mm_setr_epi32(-4,0x3ff, -4,0x3ff);
	const __m128i mask_crot		= _mm_setr_epi32(0xc,0, 0xc,0);
	const __m128i shuf_crot		= _mm_setr_epi8(0,0,0,0, 0,0,0,0, 8,8,8,8, 8,8,8,8);
	const __m128i ormask_crot	= _mm_setr_epi8(0,1,2,3, 0,0,0,0, 0,1,2,3, 0,0,0,0);
	const __m128i crot_lut1		= _mm_setr_epi32(0, 1<<30, 1<<20, 1<<10);
	const __m128i crot_lut2		= _mm_setr_epi32(0, 1<<2, 1<<12, 1<<22);
	const __m128i mask_rgbaxor	= _mm_setr_epi32(0,0x3ff, 0,0x3ff);
	const __m128i mul_spread	= _mm_setr_epi16(1,1,2,0, 1,1,2,0);
	const __m128i mask_rgba_mv2	= _mm_setr_epi32(0,0x7c0, 0,0x7c0);
	const __m128i mask_bits01	= _mm_setr_epi32(3,0, 3,0);
	const __m128i mul_alphalo	= _mm_setr_epi16(0x21,0,0,0, 0x21,0,0,0);
	const __m128i mask_alphalo	= _mm_setr_epi16(0x41,0,0,0, 0x41,0,0,0);
	const __m128i mask_bit7		= _mm_setr_epi32(0x80,0, 0x80,0);
	const __m128i mode_bits		= _mm_setr_epi32(0x10,0, 0x10,0);
	const __m128i mask_hi0		= _mm_setr_epi32(0, -(1<<18), 0, -(1<<18));

	for (; iblock < (nblocks & ~1); iblock += 2)
	{
		// Load both halves
		Vec128 lo, hi0, hi1;
		vec_load_split6<Tsplit>(lo, hi0, hi1, first_ptr, second_ptr, iblock);

		Vec128 rgba;
		// change color space if required
		if (t_switch_colorspace)
		{
			Vec128 yval = _mm_and_si128(lo, mask_chan0);
			// broadcast "Y" (G)
			Vec128 ybroad = _mm_mul_epu32(yval, mul_ybroad);
			// shuffle "Cr"/"Cb"/A around
			Vec128 crcba = _mm_or_si128(_mm_and_si128(_mm_srli_epi32(lo, 10), mask_chan0), _mm_and_si128(lo, mask_chan23));

			// and a single packed add to merge everything, which we have to do manually
			rgba = vec_packed_add(ybroad, crcba, mask_nonmsb);
		}
		else
			rgba = lo;

		// Mask to keep only the main RGBA bits
		rgba = _mm_and_si128(rgba, mask_rgba);

		// Rotate color channels back.
		// This is a bit of a production since we don't have the variable shifts we'd like.
		//
		// Unlike the scalar version, we don't align "r" to start at bit 0 here; it's better
		// for our purposes to leave it starting at bit 2 so the "a" group starts at bit 32.

		// materialize 1u << (((0 - crot) & 3) * 10) in low 32 bits of crot_mult 64-bit lanes
		Vec128 crot_masked = _mm_and_si128(_mm_srli_epi64(hi0, 46), mask_crot); // this is crot<<2
		Vec128 crot_idx = _mm_shuffle_epi8(crot_masked, shuf_crot);
		Vec128 crot_inds = _mm_or_si128(crot_idx, ormask_crot);
		Vec128 crot_mult = _mm_shuffle_epi8(crot_lut1, crot_inds);

		// Also start assembling output word
		Vec128 out_lo = _mm_or_si128(mode_bits, _mm_and_si128(hi0, mask_hi0));
		out_lo = _mm_or_si128(out_lo, _mm_slli_epi64(crot_masked, 3));
		out_lo = _mm_or_si128(out_lo, _mm_and_si128(_mm_srli_epi64(lo, 35), mask_bit7));

		// Do the multiply to set up the XOR mask
		Vec128 rgba_shifted = _mm_mul_epu32(rgba, crot_mult);
		Vec128 crot_zero = _mm_cmpeq_epi32(crot_inds, _mm_setzero_si128());
		Vec128 rgba_mask = _mm_andnot_si128(crot_zero, mask_rgbaxor); // to nuke the XOR mask when crot=0 (where we swap nothing)
		Vec128 xor_mask = _mm_and_si128(_mm_xor_si128(rgba_shifted, rgba), rgba_mask);

		// Do the swap using XORs
		Vec128 crot_mult2 = _mm_shuffle_epi8(crot_lut2, crot_inds);
		Vec128 xor_mask_shiftdown = _mm_mul_epu32(_mm_srli_epi64(xor_mask, 32), crot_mult2);
		rgba = _mm_xor_si128(rgba, xor_mask);
		rgba = _mm_xor_si128(rgba, xor_mask_shiftdown);

		// Make space for the extra A bits
		Vec128 rgba_spread1 = _mm_mullo_epi16(rgba, mul_spread); // handles the parts that move left by 0 or 1 bits
		Vec128 rgba_spread = _mm_add_epi16(rgba_spread1, _mm_and_si128(rgba_spread1, mask_rgba_mv2)); // handle the parts that move left by 2 bits

		// Insert the extra A bits
		Vec128 a_lobits = _mm_and_si128(lo, mask_bits01);
		Vec128 a_lobits_spread = _mm_mullo_epi16(a_lobits, mul_alphalo); // yet another "multiple shifts via mul"
		Vec128 a_lobits_final = _mm_and_si128(a_lobits_spread, mask_alphalo);
		rgba_spread = _mm_or_si128(rgba_spread, _mm_shuffle_epi32(a_lobits_final, 0xb1));

		// Insert into final output
		out_lo = _mm_or_si128(out_lo, _mm_slli_epi64(rgba_spread, 6));

		// Interleave to form results
		Vec128 out0 = _mm_unpacklo_epi64(out_lo, hi1);
		Vec128 out1 = _mm_unpackhi_epi64(out_lo, hi1);
		store128u(out_block + bc7prep::target_offs(target[iblock + 0]), out0);
		store128u(out_block + bc7prep::target_offs(target[iblock + 1]), out1);
	}

	return iblock;
}

template<typename Tsplit, bool t_switch_colorspace>
UINTa un_munge_bc7_mode5_ssse3(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target)
{
	UINTa iblock = 0;

	const __m128i mask_nonlsb		= _mm_set1_epi8(-2);
	const __m128i shuf_ybroad		= _mm_setr_epi8(0,0,0,-1, 4,4,4,-1, 8,8,8,-1, 12,12,12,-1);
	const __m128i shuf_crcba		= _mm_setr_epi8(1,-1,2,3, 5,-1,6,7, 9,-1,10,11, 13,-1,14,15);
	const __m128i mask_bit01		= _mm_setr_epi32(3,0, 3,0);
	const __m128i shuf_broadcrot	= _mm_setr_epi8(0,0,0,0, 0,0,0,0, 8,8,8,8, 8,8,8,8);
	const __m128i mask_shufbase0   	= _mm_setr_epi8(0,0,4,4, 8,8,12,12, 0,0,4,4, 8,8,12,12);
	const __m128i lut_chanrots_cs	= _mm_setr_epi8(0,3,0,0, 1,1,3,1, 2,2,2,3, 3,0,1,2); // permutations: 0123, 3120, 0321, 0132
	const __m128i mask_shufbase_cs1	= _mm_setr_epi8(0,4,0,4, 0,4,0,4, 8,12,8,12, 8,12,8,12);
	const __m128i lut_chanrots_nocs	= _mm_setr_epi8(0,6,0,0, 2,2,6,2, 4,4,4,6, 6,0,2,4); // permutations: 0123, 3120, 0321, 0132
	const __m128i mask_shufbase_noc	= _mm_setr_epi8(0,1,0,1, 0,1,0,1, 8,9,8,9, 8,9,8,9);
	const __m128i mode_bits			= _mm_setr_epi32(0x20,0, 0x20,0);
	const __m128i mask_alphas		= _mm_setr_epi16(0,0,0,-1, 0,0,0,-1);
	const __m128i mask_precompact	= _mm_set1_epi8(-2);
	const __m128i mul_compact1		= _mm_setr_epi8(-1,-128, -1,-128, -1,-128, 0,0, -1,-128, -1,-128, -1,-128, 0,0);
	const __m128i mul_compact2		= _mm_setr_epi16(-1,-(1<<14), -(1<<4),0, -1,-(1<<14), -(1<<4),0);
	const __m128i mask_lo32			= _mm_setr_epi32(-1,0, -1,0);

	for (; iblock < (nblocks & ~1); iblock += 2)
	{
		// Load both halves
		Vec128 endpoints, indices;
		vec_load_pair<Tsplit>(endpoints, indices, first_ptr, second_ptr, iblock);

		Vec128 crot = _mm_and_si128(indices, mask_bit01);
		indices = _mm_xor_si128(indices, crot); // keep only the actual index bits

		Vec128 crot_shuffle;

		if (t_switch_colorspace)
		{
			Vec128 endpoints_hi = _mm_and_si128(endpoints, mask_nonlsb);
			Vec128 ybroad = _mm_shuffle_epi8(endpoints_hi, shuf_ybroad);
			Vec128 crcba = _mm_shuffle_epi8(endpoints_hi, shuf_crcba);
			Vec128 sum = _mm_add_epi8(ybroad, crcba);
			endpoints = _mm_or_si128(sum, _mm_andnot_si128(mask_nonlsb, endpoints));

			// deinterleave and channel rotate at the same time
			Vec128 crot_broad = _mm_shuffle_epi8(crot, shuf_broadcrot);
			Vec128 crot_shufind = _mm_or_si128(crot_broad, mask_shufbase0);
			Vec128 crot_shufbase = _mm_shuffle_epi8(lut_chanrots_cs, crot_shufind);
			crot_shuffle = _mm_or_si128(crot_shufbase, mask_shufbase_cs1);
		}
		else
		{
			// Just need to do channel rotate here, we're already deinterleaved
			Vec128 crot_broad = _mm_shuffle_epi8(crot, shuf_broadcrot); // crot in every byte
			Vec128 crot_shufind = _mm_or_si128(crot_broad, mask_shufbase0);
			Vec128 crot_shufbase = _mm_shuffle_epi8(lut_chanrots_nocs, crot_shufind);
			crot_shuffle = _mm_or_si128(crot_shufbase, mask_shufbase_noc);
		}

		// Start preparing the output block
		Vec128 out_lo = _mm_or_si128(mode_bits, _mm_slli_epi64(crot, 6));

		// Do the channel swap and deinterleave
		endpoints = _mm_shuffle_epi8(endpoints, crot_shuffle);

		// Nowe we have the channel bits in (post-rotate) deinterleaved
		// R,G,B,A order:
		//   r0,r1, g0,g1, b0,b1, a0,a1
		// The rgb's are 7-bit values packed into 8-bit lanes, shifted by 1.
		// We need to compact them next (this masks out A for now).
		Vec128 alphas = _mm_and_si128(endpoints, mask_alphas);
		Vec128 masked = _mm_and_si128(endpoints, mask_precompact);
		Vec128 compact1 = _mm_maddubs_epi16(masked, mul_compact1);
		// now we have 16-bit lanes with 14-bit values: (-r0r1 << 1, -g0g1 << 1, -b0b1 << 1)

		// Alpha straddles the 64-bit halves which is a bit awkward
		out_lo = _mm_or_si128(out_lo, _mm_slli_epi64(alphas, 2));
		indices = _mm_or_si128(indices, _mm_srli_epi64(alphas, 62));

		// Second compaction pass
		Vec128 compact2a = _mm_madd_epi16(compact1, mul_compact2);
		Vec128 compact2 = _mm_srli_epi32(compact2a, 1);
		// now we have 32-bit lanes containing (r0r1g0g1, b0b1<<4)

		// Merge the rgb bitfields into the output block
		out_lo = _mm_or_si128(out_lo, _mm_slli_epi64(_mm_and_si128(compact2, mask_lo32), 8)); // r+g
		out_lo = _mm_or_si128(out_lo, _mm_andnot_si128(mask_lo32, compact2)); // b (this could be PBLENDW w/ SSE4)

		// Interleave and write
		Vec128 out0 = _mm_unpacklo_epi64(out_lo, indices);
		Vec128 out1 = _mm_unpackhi_epi64(out_lo, indices);

		store128u(out_block + bc7prep::target_offs(target[iblock + 0]), out0);
		store128u(out_block + bc7prep::target_offs(target[iblock + 1]), out1);
	}

	return iblock;
}

template<typename Tsplit, bool t_switch_colorspace>
UINTa un_munge_bc7_mode6_ssse3(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target)
{
	UINTa iblock = 0;

	const __m128i mask_7bits     = _mm_set1_epi8(0x7f);
	const __m128i mask_p0        = _mm_setr_epi32(0, -2*(1 << 30), 0, -2*(1 << 30)); // this is 1<<31, just written to avoid constant overflow warnings. sigh.
	const __m128i mode_bits      = _mm_setr_epi32(0x40, 0, 0x40, 0);
	const __m128i step1_mult     = _mm_setr_epi16(2,2, 1,1, 2,2, 1,1);
	const __m128i step1_yshuf    = _mm_setr_epi8(0,4, 0,4, 0,4, -1,-1, 8,12, 8,12, 8,12, -1,-1);
	const __m128i step1_crcbshuf = _mm_setr_epi8(1,5, -1,-1, 2,6, 3,7, 9,13, -1,-1, 10,14, 11,15);
	const __m128i step1_mask     = _mm_set1_epi16(0x7ffe);
	const __m128i step2_mult     = _mm_setr_epi16(4,1, 4,1, 4,1, 4,1);
	const __m128i step1_mult_nocs= _mm_setr_epi8(-1,-128, -1,-128, -1,-128, -1,-128, -1,-128, -1,-128, -1,-128, -1,-128);
	const __m128i step2_mult_nocs= _mm_setr_epi16(-4,-1, -4,-1, -4,-1, -4,-1);
	const __m128i step3_rgmask   = _mm_setr_epi32(-0x80, 7, -0x80, 7);
	const __m128i step3_bamask   = _mm_setr_epi32(0, 0x7ffffff8, 0, 0x7ffffff8);

	for (; iblock < (nblocks & ~1); iblock += 2)
	{
		// load both halves
		Vec128 endpoints, indices;
		vec_load_pair<Tsplit>(endpoints, indices, first_ptr, second_ptr, iblock);

		// Throw out the stowaway bits
		Vec128 endpoint_payloads = _mm_and_si128(endpoints, mask_7bits);
		Vec128 mode_p0 = _mm_or_si128(_mm_and_si128(endpoints, mask_p0), mode_bits);

		Vec128 step2_result;

		if (t_switch_colorspace)
		{
			// Step 1: pack to form 14-bit groups within 16-bit lanes
			Vec128 step1_products = _mm_mullo_epi16(endpoint_payloads, step1_mult);

			// do colorspace switching while we have everything still packed in bytes, for convenience
			Vec128 step1_y    = _mm_shuffle_epi8(step1_products, step1_yshuf);
			Vec128 step1_crcb = _mm_shuffle_epi8(step1_products, step1_crcbshuf);
			Vec128 step1_result = _mm_and_si128(_mm_add_epi8(step1_y, step1_crcb), step1_mask);
			// now the 16-bit lanes contain: (r0r1 << 1, g0g1 << 1, b0b1 << 1, ...)

			// Step 2: pack again to form 28-bit groups within 32-bit lanes
			Vec128 step2_shr1 = _mm_srli_epi16(step1_result, 1);
			step2_result = _mm_mullo_epi16(step2_shr1, step2_mult);
			// now the 32-bit lanes contain: (r0r1g0g1 << 2, b0b1a0a1 << 2, ...)
		}
		else
		{
			// Step 1: pack to form 14-bit groups within 16-bit lanes
			Vec128 step1_result = _mm_maddubs_epi16(endpoint_payloads, step1_mult_nocs);
			// now the 16-bit lanes contain (-r0r1, -g0g1, -b0b1, -a0a1)

			// Step 2: pack again to form 28-bit groups within 32-bit lanes
			step2_result = _mm_mullo_epi16(step1_result, step2_mult_nocs);
			// now the 32-bit lanes contain: (r0r1g0g1 << 2, b0b1a0a1 << 2)
		}

		// No point doing a similar 3rd pass here, just move the two halves to their final position
		// manually.
		Vec128 step3_rg = _mm_and_si128(_mm_slli_epi64(step2_result, 5), step3_rgmask);
		Vec128 step3_ba = _mm_and_si128(_mm_add_epi32(step2_result, step2_result), step3_bamask);

		// Combine with P0 and mode bits
		Vec128 lo_result = _mm_or_si128(_mm_or_si128(step3_rg, mode_p0), step3_ba);

		// Now interleave lo and hi halves to produce output
		Vec128 out0 = _mm_unpacklo_epi64(lo_result, indices);
		Vec128 out1 = _mm_unpackhi_epi64(lo_result, indices);
		store128u(out_block + bc7prep::target_offs(target[iblock + 0]), out0);
		store128u(out_block + bc7prep::target_offs(target[iblock + 1]), out1);
	}

	return iblock;
}

template<typename Tsplit, bool t_switch_colorspace>
UINTa un_munge_bc7_mode7_ssse3(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target)
{
	UINTa iblock = 0;

	if (t_switch_colorspace)
	{
		const __m128i mask_lsbs16	= _mm_set1_epi16(1);
		const __m128i mul_pbits0	= _mm_setr_epi16(1,2, 1,2, 1,2, 1,2);
		const __m128i mask_chan0	= _mm_set1_epi16(0x1f);
		const __m128i mul_compact1	= _mm_setr_epi8(1,1<<5, 1,1<<5, 1,1<<5, 1,1<<5, 1,1<<5, 1,1<<5, 1,1<<5, 1,1<<5);
		const __m128i mul_compact2	= _mm_setr_epi16(1<<2,1<<12, 1<<2,1<<12, 1<<2,1<<12, 1<<2,1<<12);
		const __m128i mul_compactb	= _mm_setr_epi16(1<<6,1<<11,1<<0,1<<5, 1<<6,1<<11,1<<0,1<<5);
		const __m128i mode_bits		= _mm_setr_epi32(0x80, 0, 0x80, 0);
		const __m128i mask_partbits	= _mm_setr_epi32(0x3f00, 0, 0x3f00, 0);
		const __m128i mask_rbits	= _mm_setr_epi32(-(1<<14), 3, -(1<<14), 3);
		const __m128i mask_gbits	= _mm_setr_epi32(0, (1<<22)-(1<<2), 0, (1<<22)-(1<<2));
		const __m128i mask_abits	= _mm_setr_epi32((1<<30)-(1<<10), 0, (1<<30)-(1<<10), 0);
		const __m128i mask_pbits	= _mm_setr_epi32(-(1<<30),3, -(1<<30),3);
		const __m128i mask_indbits	= _mm_setr_epi32(0,-(1<<2), 0,-(1<<2));

		for (; iblock < (nblocks & ~1); iblock += 2)
		{
			Vec128 prgbs, rest;
			vec_load_pair<Tsplit>(prgbs, rest, first_ptr, second_ptr, iblock);

			// Grab the P bits from the LSBs of the PRGB values
			Vec128 pbits0 = _mm_and_si128(prgbs, mask_lsbs16);
			Vec128 pbits1 = _mm_madd_epi16(pbits0, mul_pbits0); // collect pbits from 16b lanes into 32-bit lanes with 2 combined pbits
			// now there's a 30-bit gap between the two halves, compact that
			Vec128 pbits2 = _mm_slli_epi64(pbits1, 30);
			Vec128 pbits3 = _mm_or_si128(pbits1, pbits2);

			// Grab y/cr/cb bits (don't mask just yet)
			Vec128 ybits = _mm_srli_epi16(prgbs, 1);
			Vec128 crbits = _mm_srli_epi16(prgbs, 6);
			Vec128 cbbits = _mm_srli_epi16(prgbs, 11);

			// Do the color space change
			Vec128 gbits = ybits;
			Vec128 rbits = _mm_add_epi16(crbits, ybits);
			Vec128 bbits = _mm_add_epi16(cbbits, ybits);

			// Mask down to 5 bits per channel
			rbits = _mm_and_si128(rbits, mask_chan0);
			gbits = _mm_and_si128(gbits, mask_chan0);
			bbits = _mm_and_si128(bbits, mask_chan0);

			// Pack down to 8 bits
			Vec128 rgbits_8 = _mm_packs_epi16(rbits, gbits);

			// We have 5-bit values in 8-bit lanes; time to compact!
			Vec128 rgcompact1 = _mm_maddubs_epi16(rgbits_8, mul_compact1);
			// we now have 10-bit values in 16-bit lanes
			// rg is: (a.r0r1, a.r2r3, b.r0r1, b.r2r3, a.g0g1, a.g2g3, b.g0g1, b.g2g3)

			// Second compaction pass
			Vec128 rgcompact2 = _mm_madd_epi16(rgcompact1, mul_compact2);
			// we now have 32-bit lanes containing 20-bit values:
			//   rgcompact2 = (a.r0r1r2r3<<2, b.r0r1r2r3<<2, a.g0g1g2g3<<2, b.g0g1g2g3<<2)

			// Shuffle into desired order
			Vec128 rgsorted = _mm_shuffle_epi32(rgcompact2, 0xd8);
			// now:
			//   rgsorted   = (a.r0r1r2r3<<2, a.g0g1g2g3<<2, b.r0r1r2r3<<2, b.g0g1g2g3<<2)

			// Blue bits can be done directly
			Vec128 bbits_grp = _mm_madd_epi16(bbits, mul_compactb);
			// we now have 10-bit values in 16-bit lanes, aligned as pieces of a 32-bit value but with gaps in between
			//   bbits_grp  = (a.b0b1<<6, 0, a.b2b3, 0, b.b0b1<<6, 0, b.b2b3)

			// Form output blocks
			Vec128 out_lo = _mm_or_si128(mode_bits, _mm_and_si128(_mm_srli_epi64(rest, 20), mask_partbits));
			out_lo = _mm_or_si128(out_lo, _mm_slli_epi64(bbits_grp, 48));
			out_lo = _mm_or_si128(out_lo, _mm_and_si128(_mm_slli_epi64(rgsorted, 12), mask_rbits));
			out_lo = _mm_or_si128(out_lo, _mm_and_si128(rgsorted, mask_gbits));

			Vec128 out_hi = _mm_srli_epi64(bbits_grp, 32);
			out_hi = _mm_or_si128(out_hi, _mm_and_si128(_mm_slli_epi64(rest, 2), mask_abits));
			out_hi = _mm_or_si128(out_hi, _mm_and_si128(pbits3, mask_pbits));
			out_hi = _mm_or_si128(out_hi, _mm_and_si128(rest, mask_indbits));

			// Interleave and write
			Vec128 out0 = _mm_unpacklo_epi64(out_lo, out_hi);
			Vec128 out1 = _mm_unpackhi_epi64(out_lo, out_hi);

			store128u(out_block + bc7prep::target_offs(target[iblock + 0]), out0);
			store128u(out_block + bc7prep::target_offs(target[iblock + 1]), out1);
		}
	}
	else // if (t_switch_colorspace)
	{
		const __m128i shuf_expand0	= _mm_setr_epi8(0,1,-1,-1, -1,4,5,-1, 8,9,-1,-1, -1,12,13,-1);
		const __m128i mask_expand1  = _mm_setr_epi16(0,-1,0,0, 0,-1,0,0);
		const __m128i mask_expand2  = _mm_setr_epi32(0x0ff000ff, 0x0000ff00, 0x0ff000ff, 0x0000ff00);
		const __m128i mask_expand3	= _mm_setr_epi32(0x0f03c0f0, 0x03c0f03c, 0x0f03c0f0, 0x03c0f03c);
		const __m128i shuf_exp1to5	= _mm_setr_epi8(1,2, 1,2, 2,3, 2,3, 9,10, 9,10, 10,11, 10,11);
		const __m128i mul_exp1to5a	= _mm_setr_epi16(1<<(16-4), 1<<(16-8), 1<<(16-3), 1<<(16-6), 1<<(16-4), 1<<(16-8), 1<<(16-3), 1<<(16-6));
		const __m128i mask_exp1to5a	= _mm_setr_epi16(15, 7, 7, 3,  15, 7, 7, 3);
		const __m128i mul_exp1to5b	= _mm_setr_epi16(0x1111, 0x1110, 0x888, 0x44, 0x1111, 0x1110, 0x888, 0x44);
		const __m128i mask_exp1to5b	= _mm_setr_epi32(0x42108421, 0x00842108, 0x42108421, 0x00842108);
		const __m128i mask_alobits	= _mm_setr_epi32(0xf, 0, 0xf, 0);
		const __m128i mode_bits		= _mm_setr_epi32(0x80, 0, 0x80, 0);
		const __m128i mask_partbits	= _mm_setr_epi32(0x3f00, 0, 0x3f00, 0);
		const __m128i mask_pbits   	= _mm_setr_epi32(-(1 << 30), 3, -(1 << 30), 3);
		const __m128i mask_indbits	= _mm_setr_epi32(0,-(1<<2), 0,-(1<<2));

		for (; iblock < (nblocks & ~1); iblock += 2)
		{
			Vec128 prgbs, rest;
			vec_load_pair<Tsplit>(prgbs, rest, first_ptr, second_ptr, iblock);

			// expand4to5_12x(prgbs)
			// pass 1: move by multiples of 4
			Vec128 expand_rgb0	= _mm_shuffle_epi8(prgbs, shuf_expand0);
			Vec128 expand_rgb1	= _mm_and_si128(prgbs, mask_expand1);
			Vec128 expand_rgb2	= _mm_or_si128(expand_rgb0, _mm_slli_epi64(expand_rgb1, 4));

			// pass 2: move by 2
			Vec128 expand_rgb3	= _mm_and_si128(expand_rgb2, mask_expand2);
			Vec128 expand_rgb4	= _mm_andnot_si128(mask_expand2, expand_rgb2);
			Vec128 expand_rgb5	= _mm_or_si128(expand_rgb3, _mm_slli_epi64(expand_rgb4, 2));

			// pass 3: move by 1
			Vec128 expand_rgb6	= _mm_and_si128(expand_rgb5, mask_expand3);
			Vec128 expand_rgb7	= _mm_add_epi64(expand_rgb5, expand_rgb6);
			Vec128 rgbbits0		= _mm_slli_epi64(expand_rgb7, 1); // expand4to5_12x(prgbs) << 1

			// expand4to5_4x(prgbs >> 48)
			// pass 1: move by 2
			Vec128 prgbhigh		= _mm_srli_epi64(prgbs, 48);
			Vec128 exphi_rgb0	= _mm_and_si128(prgbhigh, mask_expand2);
			Vec128 exphi_rgb1	= _mm_andnot_si128(mask_expand2, prgbhigh);
			Vec128 exphi_rgb2	= _mm_or_si128(exphi_rgb0, _mm_slli_epi64(exphi_rgb1, 2));

			// pass 2: move by 1
			Vec128 exphi_rgb3	= _mm_and_si128(exphi_rgb2, mask_expand3);
			Vec128 exphi_rgb4	= _mm_add_epi64(exphi_rgb2, exphi_rgb3);
			Vec128 abits0		= _mm_slli_epi64(exphi_rgb4, 1); // expand4to5_4x(prgbs >> 48) << 1

			// expand1to5_12x(rest >> 12)
			Vec128 lobits0		= _mm_shuffle_epi8(rest, shuf_exp1to5); // grab words
			Vec128 lobits1		= _mm_mulhi_epu16(lobits0, mul_exp1to5a); // shift right to align relevant bits at bottom
			Vec128 lobits2		= _mm_and_si128(lobits1, mask_exp1to5a); // mask active groups
			Vec128 lobits3		= _mm_mullo_epi16(lobits2, mul_exp1to5b); // mul to spread out
			Vec128 lobits4		= _mm_and_si128(lobits3, mask_exp1to5b); // mask results
			Vec128 rgbbits1		= _mm_or_si128(rgbbits0, lobits4); // rgbbits |= exapnd1to5_12x(rest >> 12)

			// expand1to5_4x(rest >> 24)
			Vec128 alobits0		= _mm_srli_epi64(rest, 24);
			Vec128 alobits1		= _mm_and_si128(alobits0, mask_alobits);
			Vec128 alobits2		= _mm_madd_epi16(alobits1, mul_exp1to5b);
			Vec128 alobits3		= _mm_and_si128(alobits2, mask_exp1to5b);
			Vec128 abits1		= _mm_or_si128(abits0, alobits3); // abits |= expand1to5_4x(rest >> 24)

			// partition bits
			Vec128 partbits		= _mm_and_si128(_mm_srli_epi64(rest, 20), mask_partbits);

			// Form output blocks
			Vec128 out_lo		= _mm_or_si128(mode_bits, partbits);
			out_lo				= _mm_or_si128(out_lo, _mm_slli_epi64(rgbbits1, 14));

			Vec128 out_hi		= _mm_and_si128(rest, mask_indbits);
			out_hi				= _mm_or_si128(out_hi, _mm_and_si128(_mm_slli_epi64(rest, 22), mask_pbits));
			out_hi				= _mm_or_si128(out_hi, _mm_srli_epi64(rgbbits1, 50));
			out_hi				= _mm_or_si128(out_hi, _mm_slli_epi64(abits1, 10));

			// Interleave and write
			Vec128 out0 = _mm_unpacklo_epi64(out_lo, out_hi);
			Vec128 out1 = _mm_unpackhi_epi64(out_lo, out_hi);

			store128u(out_block + bc7prep::target_offs(target[iblock + 0]), out0);
			store128u(out_block + bc7prep::target_offs(target[iblock + 1]), out1);
		}
	}

	return iblock;
}

// for a 8-bit value x, this tabulates the positions of the set bits in x
// (padded out by repeating the last digit)
RAD_ALIGN(U16, sort_compact_table[256][8], 16) =
{
	{0,0,0,0,0,0,0,0}, {0,0,0,0,0,0,0,0}, {1,1,1,1,1,1,1,1}, {0,1,1,1,1,1,1,1},
	{2,2,2,2,2,2,2,2}, {0,2,2,2,2,2,2,2}, {1,2,2,2,2,2,2,2}, {0,1,2,2,2,2,2,2},
	{3,3,3,3,3,3,3,3}, {0,3,3,3,3,3,3,3}, {1,3,3,3,3,3,3,3}, {0,1,3,3,3,3,3,3},
	{2,3,3,3,3,3,3,3}, {0,2,3,3,3,3,3,3}, {1,2,3,3,3,3,3,3}, {0,1,2,3,3,3,3,3},
	{4,4,4,4,4,4,4,4}, {0,4,4,4,4,4,4,4}, {1,4,4,4,4,4,4,4}, {0,1,4,4,4,4,4,4},
	{2,4,4,4,4,4,4,4}, {0,2,4,4,4,4,4,4}, {1,2,4,4,4,4,4,4}, {0,1,2,4,4,4,4,4},
	{3,4,4,4,4,4,4,4}, {0,3,4,4,4,4,4,4}, {1,3,4,4,4,4,4,4}, {0,1,3,4,4,4,4,4},
	{2,3,4,4,4,4,4,4}, {0,2,3,4,4,4,4,4}, {1,2,3,4,4,4,4,4}, {0,1,2,3,4,4,4,4},
	{5,5,5,5,5,5,5,5}, {0,5,5,5,5,5,5,5}, {1,5,5,5,5,5,5,5}, {0,1,5,5,5,5,5,5},
	{2,5,5,5,5,5,5,5}, {0,2,5,5,5,5,5,5}, {1,2,5,5,5,5,5,5}, {0,1,2,5,5,5,5,5},
	{3,5,5,5,5,5,5,5}, {0,3,5,5,5,5,5,5}, {1,3,5,5,5,5,5,5}, {0,1,3,5,5,5,5,5},
	{2,3,5,5,5,5,5,5}, {0,2,3,5,5,5,5,5}, {1,2,3,5,5,5,5,5}, {0,1,2,3,5,5,5,5},
	{4,5,5,5,5,5,5,5}, {0,4,5,5,5,5,5,5}, {1,4,5,5,5,5,5,5}, {0,1,4,5,5,5,5,5},
	{2,4,5,5,5,5,5,5}, {0,2,4,5,5,5,5,5}, {1,2,4,5,5,5,5,5}, {0,1,2,4,5,5,5,5},
	{3,4,5,5,5,5,5,5}, {0,3,4,5,5,5,5,5}, {1,3,4,5,5,5,5,5}, {0,1,3,4,5,5,5,5},
	{2,3,4,5,5,5,5,5}, {0,2,3,4,5,5,5,5}, {1,2,3,4,5,5,5,5}, {0,1,2,3,4,5,5,5},
	{6,6,6,6,6,6,6,6}, {0,6,6,6,6,6,6,6}, {1,6,6,6,6,6,6,6}, {0,1,6,6,6,6,6,6},
	{2,6,6,6,6,6,6,6}, {0,2,6,6,6,6,6,6}, {1,2,6,6,6,6,6,6}, {0,1,2,6,6,6,6,6},
	{3,6,6,6,6,6,6,6}, {0,3,6,6,6,6,6,6}, {1,3,6,6,6,6,6,6}, {0,1,3,6,6,6,6,6},
	{2,3,6,6,6,6,6,6}, {0,2,3,6,6,6,6,6}, {1,2,3,6,6,6,6,6}, {0,1,2,3,6,6,6,6},
	{4,6,6,6,6,6,6,6}, {0,4,6,6,6,6,6,6}, {1,4,6,6,6,6,6,6}, {0,1,4,6,6,6,6,6},
	{2,4,6,6,6,6,6,6}, {0,2,4,6,6,6,6,6}, {1,2,4,6,6,6,6,6}, {0,1,2,4,6,6,6,6},
	{3,4,6,6,6,6,6,6}, {0,3,4,6,6,6,6,6}, {1,3,4,6,6,6,6,6}, {0,1,3,4,6,6,6,6},
	{2,3,4,6,6,6,6,6}, {0,2,3,4,6,6,6,6}, {1,2,3,4,6,6,6,6}, {0,1,2,3,4,6,6,6},
	{5,6,6,6,6,6,6,6}, {0,5,6,6,6,6,6,6}, {1,5,6,6,6,6,6,6}, {0,1,5,6,6,6,6,6},
	{2,5,6,6,6,6,6,6}, {0,2,5,6,6,6,6,6}, {1,2,5,6,6,6,6,6}, {0,1,2,5,6,6,6,6},
	{3,5,6,6,6,6,6,6}, {0,3,5,6,6,6,6,6}, {1,3,5,6,6,6,6,6}, {0,1,3,5,6,6,6,6},
	{2,3,5,6,6,6,6,6}, {0,2,3,5,6,6,6,6}, {1,2,3,5,6,6,6,6}, {0,1,2,3,5,6,6,6},
	{4,5,6,6,6,6,6,6}, {0,4,5,6,6,6,6,6}, {1,4,5,6,6,6,6,6}, {0,1,4,5,6,6,6,6},
	{2,4,5,6,6,6,6,6}, {0,2,4,5,6,6,6,6}, {1,2,4,5,6,6,6,6}, {0,1,2,4,5,6,6,6},
	{3,4,5,6,6,6,6,6}, {0,3,4,5,6,6,6,6}, {1,3,4,5,6,6,6,6}, {0,1,3,4,5,6,6,6},
	{2,3,4,5,6,6,6,6}, {0,2,3,4,5,6,6,6}, {1,2,3,4,5,6,6,6}, {0,1,2,3,4,5,6,6},
	{7,7,7,7,7,7,7,7}, {0,7,7,7,7,7,7,7}, {1,7,7,7,7,7,7,7}, {0,1,7,7,7,7,7,7},
	{2,7,7,7,7,7,7,7}, {0,2,7,7,7,7,7,7}, {1,2,7,7,7,7,7,7}, {0,1,2,7,7,7,7,7},
	{3,7,7,7,7,7,7,7}, {0,3,7,7,7,7,7,7}, {1,3,7,7,7,7,7,7}, {0,1,3,7,7,7,7,7},
	{2,3,7,7,7,7,7,7}, {0,2,3,7,7,7,7,7}, {1,2,3,7,7,7,7,7}, {0,1,2,3,7,7,7,7},
	{4,7,7,7,7,7,7,7}, {0,4,7,7,7,7,7,7}, {1,4,7,7,7,7,7,7}, {0,1,4,7,7,7,7,7},
	{2,4,7,7,7,7,7,7}, {0,2,4,7,7,7,7,7}, {1,2,4,7,7,7,7,7}, {0,1,2,4,7,7,7,7},
	{3,4,7,7,7,7,7,7}, {0,3,4,7,7,7,7,7}, {1,3,4,7,7,7,7,7}, {0,1,3,4,7,7,7,7},
	{2,3,4,7,7,7,7,7}, {0,2,3,4,7,7,7,7}, {1,2,3,4,7,7,7,7}, {0,1,2,3,4,7,7,7},
	{5,7,7,7,7,7,7,7}, {0,5,7,7,7,7,7,7}, {1,5,7,7,7,7,7,7}, {0,1,5,7,7,7,7,7},
	{2,5,7,7,7,7,7,7}, {0,2,5,7,7,7,7,7}, {1,2,5,7,7,7,7,7}, {0,1,2,5,7,7,7,7},
	{3,5,7,7,7,7,7,7}, {0,3,5,7,7,7,7,7}, {1,3,5,7,7,7,7,7}, {0,1,3,5,7,7,7,7},
	{2,3,5,7,7,7,7,7}, {0,2,3,5,7,7,7,7}, {1,2,3,5,7,7,7,7}, {0,1,2,3,5,7,7,7},
	{4,5,7,7,7,7,7,7}, {0,4,5,7,7,7,7,7}, {1,4,5,7,7,7,7,7}, {0,1,4,5,7,7,7,7},
	{2,4,5,7,7,7,7,7}, {0,2,4,5,7,7,7,7}, {1,2,4,5,7,7,7,7}, {0,1,2,4,5,7,7,7},
	{3,4,5,7,7,7,7,7}, {0,3,4,5,7,7,7,7}, {1,3,4,5,7,7,7,7}, {0,1,3,4,5,7,7,7},
	{2,3,4,5,7,7,7,7}, {0,2,3,4,5,7,7,7}, {1,2,3,4,5,7,7,7}, {0,1,2,3,4,5,7,7},
	{6,7,7,7,7,7,7,7}, {0,6,7,7,7,7,7,7}, {1,6,7,7,7,7,7,7}, {0,1,6,7,7,7,7,7},
	{2,6,7,7,7,7,7,7}, {0,2,6,7,7,7,7,7}, {1,2,6,7,7,7,7,7}, {0,1,2,6,7,7,7,7},
	{3,6,7,7,7,7,7,7}, {0,3,6,7,7,7,7,7}, {1,3,6,7,7,7,7,7}, {0,1,3,6,7,7,7,7},
	{2,3,6,7,7,7,7,7}, {0,2,3,6,7,7,7,7}, {1,2,3,6,7,7,7,7}, {0,1,2,3,6,7,7,7},
	{4,6,7,7,7,7,7,7}, {0,4,6,7,7,7,7,7}, {1,4,6,7,7,7,7,7}, {0,1,4,6,7,7,7,7},
	{2,4,6,7,7,7,7,7}, {0,2,4,6,7,7,7,7}, {1,2,4,6,7,7,7,7}, {0,1,2,4,6,7,7,7},
	{3,4,6,7,7,7,7,7}, {0,3,4,6,7,7,7,7}, {1,3,4,6,7,7,7,7}, {0,1,3,4,6,7,7,7},
	{2,3,4,6,7,7,7,7}, {0,2,3,4,6,7,7,7}, {1,2,3,4,6,7,7,7}, {0,1,2,3,4,6,7,7},
	{5,6,7,7,7,7,7,7}, {0,5,6,7,7,7,7,7}, {1,5,6,7,7,7,7,7}, {0,1,5,6,7,7,7,7},
	{2,5,6,7,7,7,7,7}, {0,2,5,6,7,7,7,7}, {1,2,5,6,7,7,7,7}, {0,1,2,5,6,7,7,7},
	{3,5,6,7,7,7,7,7}, {0,3,5,6,7,7,7,7}, {1,3,5,6,7,7,7,7}, {0,1,3,5,6,7,7,7},
	{2,3,5,6,7,7,7,7}, {0,2,3,5,6,7,7,7}, {1,2,3,5,6,7,7,7}, {0,1,2,3,5,6,7,7},
	{4,5,6,7,7,7,7,7}, {0,4,5,6,7,7,7,7}, {1,4,5,6,7,7,7,7}, {0,1,4,5,6,7,7,7},
	{2,4,5,6,7,7,7,7}, {0,2,4,5,6,7,7,7}, {1,2,4,5,6,7,7,7}, {0,1,2,4,5,6,7,7},
	{3,4,5,6,7,7,7,7}, {0,3,4,5,6,7,7,7}, {1,3,4,5,6,7,7,7}, {0,1,3,4,5,6,7,7},
	{2,3,4,5,6,7,7,7}, {0,2,3,4,5,6,7,7}, {1,2,3,4,5,6,7,7}, {0,1,2,3,4,5,6,7}
};

template<typename T>
static inline T * advance_by_bytes(T * ptr, SINTa value)
{
	return (T*) ((U8 *)ptr + value);
}

void sort_pass_ssse3(U16 * mode_cur[16], const U8 * mode_nibbles, SINTa blocks_in_chunk)
{
	RR_ASSERT(blocks_in_chunk <= BC7PREP_CHUNK_BLOCK_COUNT);
	RAD_ALIGN(U8, mode_bytes[BC7PREP_CHUNK_BLOCK_COUNT], 16);
	SINTa i;

	// unpack modes
	for (i = 0; i < (blocks_in_chunk & ~31); i += 32)
	{
		Vec128 modes = load128u(mode_nibbles + (i >> 1));

		Vec128 lonib = _mm_and_si128(modes, _mm_set1_epi8(0xf));
		Vec128 hinib = _mm_and_si128(_mm_srli_epi16(modes, 4), _mm_set1_epi8(0xf));

		store128u(mode_bytes + i +  0, _mm_unpacklo_epi8(lonib, hinib));
		store128u(mode_bytes + i + 16, _mm_unpackhi_epi8(lonib, hinib));
	}

	// no problem with overrun, BC7PREP_CHUNK_BLOCK_COUNT is even so there's always space
	for (; i < blocks_in_chunk; i += 2)
	{
		U8 nibbles = mode_nibbles[i >> 1];
		mode_bytes[i + 0] = nibbles & 0xf;
		mode_bytes[i + 1] = nibbles >> 4;
	}

	// actual sorting loop
	SINTa tail_start_at = blocks_in_chunk - 15; // keep running as long as we have a full vector's worth of valid mode bytes
	U32 processed_mask = 0;
	for (i = 0; i < tail_start_at; )
	{
		Vec128 modes = load128u(mode_bytes + i);
		Vec128 base_i = _mm_set1_epi16((short) i);
		U8 active_mode = mode_bytes[i];

		// check which lanes match active
		Vec128 matching_lanes = _mm_cmpeq_epi8(modes, _mm_shuffle_epi8(modes, _mm_setzero_si128()));

		// turn into mask
		U32 matching_mask = _mm_movemask_epi8(matching_lanes);

		// emit compacted output indices for this mode
		U16 * out_ptr = mode_cur[active_mode];

		// compute pop counts on the matching lanes via PSADBW
		// we sum 2 for each matching lane since that's how many bytes we write.
		Vec128 matching_bit = _mm_and_si128(matching_lanes, _mm_set1_epi8(2));
		Vec128 matching_sums = _mm_sad_epu8(matching_bit, _mm_setzero_si128());

		U32 pop0 = _mm_cvtsi128_si32(matching_sums);
		U32 pop1 = _mm_extract_epi16(matching_sums, 4);

		Vec128 base0 = base_i;
		Vec128 base1 = _mm_add_epi16(base_i, _mm_set1_epi16(8));

		Vec128 inds0 = load128u(sort_compact_table[matching_mask & 0xff]);
		Vec128 inds1 = load128u(sort_compact_table[matching_mask >> 8]);

		store128u(out_ptr, _mm_add_epi16(base0, inds0)); out_ptr = advance_by_bytes(out_ptr, pop0);
		store128u(out_ptr, _mm_add_epi16(base1, inds1)); out_ptr = advance_by_bytes(out_ptr, pop1);

		mode_cur[active_mode] = out_ptr;

		// flag lanes as processed and skip to first unprocessed
		processed_mask |= matching_mask;
		U32 skip = rrCtz32(~processed_mask); // NB this relies on there being 16 always-0 bits above the real mask!
		processed_mask >>= skip;
		i += skip;
	}

	// be careful with the last few mode bytes
	for (; i < blocks_in_chunk; i++, processed_mask >>= 1)
	{
		if (processed_mask & 1)
			continue;

		U8 mode = mode_bytes[i];
		*mode_cur[mode]++ = static_cast<U16>(i);
	}
}

OptimizedDecoderKernelSet opt_kernels_ssse3 =
{
	{
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode0_sse2, SplitAt8),
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode1_ssse3, SplitAt8),
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode2_ssse3, SplitAt12),
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode3_ssse3, SplitAt12),
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode4_ssse3, SplitAt6),
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode5_ssse3, SplitAt8),
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode6_ssse3, SplitAt8),
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode7_ssse3, SplitAt12),
		UNMUNGE_SINGLE_KERNEL(un_munge_bc7_mode8_sse2), // mode 8
		UNMUNGE_NO_KERNEL, // mode 9
	},
	sort_pass_ssse3, // sort
};

} // namespace bc7prep
OODLE_NS_END

#else // DO_BUILD_SSSE3

#ifdef _MSC_VER
#pragma RR_PRAGMA_MESSAGE("NOTE: bc7prep_decode_ssse3 disabled")
#endif

EXPORT_SOME_CRAP(bc7prep_decode_ssse3);

#endif // DO_BUILD_SSSE3

