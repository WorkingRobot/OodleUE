// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetavx2

#include "bc7prep_decode.h"
#include "rrsimd.h"
#include "rrbits.h"
#include "vec256.inl"

#ifdef DO_BUILD_AVX2

#define BC7PREP_SSSE3
#define BC7PREP_AVX2
#include "bc7prep_decode_sse.inl"

OODLE_NS_START
namespace bc7prep {

template<typename Tsplit, bool t_switch_colorspace>
UINTa un_munge_bc7_mode0_avx2(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target)
{
	UINTa iblock = 0;

	const Vec256 mask_chan01	= broadcast128_256(_mm_setr_epi32(-1,0xffff, -1,0xffff));
	const Vec256 mask_chan0		= broadcast128_256(_mm_setr_epi32(0xffffff,0, 0xffffff,0));
	const Vec256 mask_nonmsb	= broadcast128_256(_mm_setr_epi32(0x77777777,0x7777, 0x77777777,0x7777)); // 48 bits worth each
	const Vec256 mode_bits		= broadcast128_256(_mm_setr_epi32(1,0, 1,0));
	const Vec256 mask_partbits	= broadcast128_256(_mm_setr_epi32(0x1e,0, 0x1e,0));
	const Vec256 mask_inds		= broadcast128_256(_mm_setr_epi32(-(1 << 13),-1, -(1 << 13),-1));

	for (; iblock < (nblocks & ~3); iblock += 4)
	{
		// Load both halves
		Vec256 lo, hi;
		vec_load_pair<Tsplit>(lo, hi, first_ptr, second_ptr, iblock);

		// Grab rgb payload bits
		Vec256 rgbits;
		Vec256 bbits = _mm256_and_si256(_mm256_or_si256(_mm256_srli_epi64(lo, 48), _mm256_slli_epi64(hi, 16)), mask_chan0);

		if (t_switch_colorspace)
		{
			Vec256 y = _mm256_and_si256(lo, mask_chan0);
			Vec256 y_2x = _mm256_or_si256(y, _mm256_slli_epi64(y, 24)); // two copies

			rgbits = vec_packed_add(y_2x, _mm256_and_si256(_mm256_srli_epi64(lo, 24), mask_chan0), mask_nonmsb); // r and g
			bbits = vec_packed_add(bbits, y, mask_nonmsb);
		}
		else
			rgbits = _mm256_and_si256(lo, mask_chan01);

		// Assemble output
		Vec256 out_lo = _mm256_or_si256(mode_bits, _mm256_and_si256(_mm256_srli_epi64(hi, 7), mask_partbits));
		out_lo = _mm256_or_si256(out_lo, _mm256_slli_epi64(rgbits, 5));
		out_lo = _mm256_or_si256(out_lo, _mm256_slli_epi64(bbits, 53));

		Vec256 out_hi = _mm256_srli_epi64(bbits, 11);
		out_hi = _mm256_or_si256(out_hi, _mm256_and_si256(hi, mask_inds));

		// Interleave and write
		Vec256 out0 = _mm256_unpacklo_epi64(out_lo, out_hi);
		Vec256 out1 = _mm256_unpackhi_epi64(out_lo, out_hi);

		store128u(out_block + target_offs(target[iblock + 0]), lo_half(out0));
		store128u(out_block + target_offs(target[iblock + 1]), lo_half(out1));
		store128u(out_block + target_offs(target[iblock + 2]), hi_half(out0));
		store128u(out_block + target_offs(target[iblock + 3]), hi_half(out1));
	}

	return iblock;
}

template<typename Tsplit, bool t_switch_colorspace>
UINTa un_munge_bc7_mode1_avx2(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target)
{
	UINTa iblock = 0;

	const Vec256 mask_6bits		= broadcast128_256(_mm_set1_epi16(0x3f));
	const Vec256 mask_gb		= broadcast128_256(_mm_set1_epi16(0x3e)); // NOTE: bottom bits of g/b left clear
	const Vec256 shuf_extra		= broadcast128_256(_mm_setr_epi8(-1,0, -1,0, -1,0, -1,0, -1,8, -1,8, -1,8, -1,8));
	const Vec256 mul_extra		= broadcast128_256(_mm_setr_epi16(0x100, 0x40, 0x10, 4, 0x100, 0x40, 0x10, 4));
	const Vec256 mask_lobit 	= broadcast128_256(_mm_set1_epi16(1));
	const Vec256 mul_compact1	= broadcast128_256(_mm_setr_epi16(1,1<<6, 1,1<<6, 1,1<<6, 1,1<<6));
	const Vec256 mul_compact2	= broadcast128_256(_mm_setr_epi16(1,1<<12, 1,1<<12, 1,1<<12, 1,1<<12));
	const Vec256 mask_compact2	= broadcast128_256(_mm_setr_epi32(0xffffff, 0, 0xffffff, 0));
	const Vec256 shuf_rgfinal	= broadcast128_256(_mm_setr_epi8(-1,0,1,2, 8,9,10,-1, -1,4,5,6, 12,13,14,-1));
	const Vec256 mode_bits		= broadcast128_256(_mm_setr_epi32(2, 0, 2, 0));
	const Vec256 mask_partid	= broadcast128_256(_mm_setr_epi32(0xfc, 0, 0xfc, 0));
	const Vec256 mask_pindex	= broadcast128_256(_mm_setr_epi32(-(1<<16), -1, -(1<<16), -1));

	for (; iblock < (nblocks & ~3); iblock += 4)
	{
		// Load both halves
		Vec256 rgbs, extra;
		vec_load_pair<Tsplit>(rgbs, extra, first_ptr, second_ptr, iblock);

		// extract the individual payload bits from the R6G5B5 encoding
		Vec256 rbits = _mm256_and_si256(rgbs, mask_6bits);
		Vec256 gbits = _mm256_and_si256(_mm256_srli_epi16(rgbs, 5), mask_gb);
		Vec256 bbits = _mm256_and_si256(_mm256_srli_epi16(rgbs, 10), mask_gb);

		// bottom bits of g/b are stored in low 8 bits of extra
		// use a shuffle to create four 16-bit copies of the 8 extra bits
		// then a multiply-high to shift everything where it needs to go
		Vec256 extra_spread = _mm256_shuffle_epi8(extra, shuf_extra);
		Vec256 extra_shifted = _mm256_mulhi_epu16(extra_spread, mul_extra);
		gbits = _mm256_or_si256(gbits, _mm256_and_si256(extra_shifted, mask_lobit));
		bbits = _mm256_or_si256(bbits, _mm256_and_si256(_mm256_srli_epi16(extra_shifted, 1), mask_lobit));

		// we're now in the perfect spot for the color space transform
		if (t_switch_colorspace)
		{
			Vec256 y = rbits;
			Vec256 cr = gbits;
			Vec256 cb = bbits;
			rbits = _mm256_and_si256(_mm256_add_epi16(y, cr), mask_6bits);
			gbits = y;
			bbits = _mm256_and_si256(_mm256_add_epi16(y, cb), mask_6bits);
		}

		// starting with 16-bit lanes with 6 low bits used each, compact r, g, b
		rbits = _mm256_madd_epi16(rbits, mul_compact1);
		gbits = _mm256_madd_epi16(gbits, mul_compact1);
		bbits = _mm256_madd_epi16(bbits, mul_compact1);
		// now have 32-bit lanes with 12 low bits used each

		// compact (second pass); narrow r/g opportunistically here (to increase density)
		Vec256 rgbits = _mm256_packs_epi32(rbits, gbits); // now 16-bit lanes with 12 bits used each
		Vec256 rgpacked = _mm256_madd_epi16(rgbits, mul_compact2); // back to 32-bit lanes: (a.r0r1r2r3, b.r0r1r2r3, a.g0g1g2g3, b.g0g1g2g3)
		Vec256 rgfinal = _mm256_shuffle_epi8(rgpacked, shuf_rgfinal); // assemble r/g output bits

		// b gets handled by itself
		bbits = _mm256_and_si256(_mm256_or_si256(bbits, _mm256_srli_epi64(bbits, 20)), mask_compact2);

		// assemble low halves of output
		Vec256 lo = mode_bits;
		lo = _mm256_or_si256(lo, _mm256_and_si256(_mm256_srli_epi32(extra, 6), mask_partid));
		lo = _mm256_or_si256(lo, rgfinal);
		lo = _mm256_or_si256(lo, _mm256_slli_epi64(bbits, 56));

		// assemble high halves of output
		Vec256 hi = _mm256_srli_epi64(bbits, 8);
		hi = _mm256_or_si256(hi, _mm256_and_si256(extra, mask_pindex));

		// interleave and write
		Vec256 out0 = _mm256_unpacklo_epi64(lo, hi);
		Vec256 out1 = _mm256_unpackhi_epi64(lo, hi);
		store128u(out_block + bc7prep::target_offs(target[iblock + 0]), lo_half(out0));
		store128u(out_block + bc7prep::target_offs(target[iblock + 1]), lo_half(out1));
		store128u(out_block + bc7prep::target_offs(target[iblock + 2]), hi_half(out0));
		store128u(out_block + bc7prep::target_offs(target[iblock + 3]), hi_half(out1));
	}

	return iblock;
}

template<typename Tsplit, bool t_switch_colorspace>
UINTa un_munge_bc7_mode2_avx2(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target)
{
	UINTa iblock = 0;

	const Vec256 mode_bits			= broadcast128_256(_mm_setr_epi32(4, 0, 0, 0));
	const Vec256 mask_chan0 		= broadcast128_256(_mm_set1_epi16(0x1f << 1));
	const Vec256 mask_index			= broadcast128_256(_mm_setr_epi32(0, 0, 0, -(1<<3)));
	const Vec256 mul_compactr		= broadcast128_256(_mm_setr_epi16(1<<0,1<<5, 1<<2,1<<7, 1<<4,1<<9, 0,0));
	const Vec256 shuf_compactr1		= broadcast128_256(_mm_setr_epi8(-1,  0, 1, 8, 9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1));
	const Vec256 shuf_compactr2		= broadcast128_256(_mm_setr_epi8(-1, -1, 4, 5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1));
	const Vec256 mul_compactg		= broadcast128_256(_mm_setr_epi16(1<<6,1<<11, 1<<0,1<<5, 1<<2,1<<7, 0,0));
	const Vec256 shuf_compactg1		= broadcast128_256(_mm_setr_epi8(-1, -1,-1,-1, 0, 1, 2, 8, 9,-1,-1,-1,-1,-1,-1,-1));
	const Vec256 shuf_compactg2		= broadcast128_256(_mm_setr_epi8(-1, -1,-1,-1,-1,-1, 4, 5,-1,-1,-1,-1,-1,-1,-1,-1));
	const Vec256 mul_compactb		= broadcast128_256(_mm_setr_epi16(1<<4,1<<9, 1<<6,1<<11, 1<<0,1<<5, 0,0));
	const Vec256 shuf_compactb1		= broadcast128_256(_mm_setr_epi8(-1,-1,-1,-1,-1,-1,-1,-1,  0,1, -1,8,  9,-1, -1,-1));
	const Vec256 shuf_compactb2		= broadcast128_256(_mm_setr_epi8(-1,-1,-1,-1,-1,-1,-1,-1, -1,4,  5,6, -1,-1, -1,-1));

	for (; iblock < (nblocks & ~1); iblock += 2)
	{
		Vec256 endpts, indbits;
		vec_load_single<Tsplit>(endpts, indbits, first_ptr, second_ptr, iblock);

		// Grab partition bits from the LSBs of the endpoint values
		Vec256 partbits16 = _mm256_slli_epi16(endpts, 15); // either 0 or -1<<15
		Vec256 partbits8 = _mm256_packs_epi16(partbits16, partbits16);
		U32 partbits = _mm256_movemask_epi8(partbits8) & 0x003f003f;

		// Align r/g/b bits to same position (don't fully mask just yet)
		Vec256 rbits = _mm256_and_si256(endpts, mask_chan0);
		Vec256 gbits = _mm256_srli_epi16(endpts, 5);
		Vec256 bbits = _mm256_srli_epi16(endpts, 10);

		// Do the color space switch if desired
		if (t_switch_colorspace)
		{
			Vec256 y = rbits;
			Vec256 cr = gbits;
			Vec256 cb = bbits;

			gbits = y;
			rbits = _mm256_add_epi16(y, cr);
			bbits = _mm256_add_epi16(cb, y);

			rbits = _mm256_and_si256(rbits, mask_chan0);
			bbits = _mm256_and_si256(bbits, mask_chan0);
		}
		else
		{
			gbits = _mm256_and_si256(gbits, mask_chan0);
			bbits = _mm256_and_si256(bbits, mask_chan0);
		}

		// Start assembling output vector
		Vec256 out = _mm256_cvtepu8_epi64(_mm_cvtsi32_si128(partbits)); // cvtepu8_epi64 works to put the partition bits in the right place
		out = _mm256_or_si256(_mm256_slli_epi32(out, 3), mode_bits); // mode + partbits
		out = _mm256_or_si256(out, _mm256_and_si256(indbits, mask_index));

		// Red
		Vec256 rcompact = _mm256_madd_epi16(rbits, mul_compactr);
		// we now have 10-bit values in 32-bit lanes, aligned to be a shuffle away from where we need them:
		// namely each group starts in the correct position within their respective bytes
		//   rcompact  = (r0r1<<1, r2r3<<3, r4r5<<5, 0)
		Vec256 rfinal = _mm256_or_si256(_mm256_shuffle_epi8(rcompact, shuf_compactr1), _mm256_shuffle_epi8(rcompact, shuf_compactr2));
		out = _mm256_or_si256(out, rfinal);

		// Green
		Vec256 gcompact = _mm256_madd_epi16(gbits, mul_compactg);
		//   gcompact  = (g0g1<<7, g2g3<<1, g4g5<<3, 0)
		Vec256 gfinal = _mm256_or_si256(_mm256_shuffle_epi8(gcompact, shuf_compactg1), _mm256_shuffle_epi8(gcompact, shuf_compactg2));
		out = _mm256_or_si256(out, gfinal);

		// Blue
		Vec256 bcompact = _mm256_madd_epi16(bbits, mul_compactb);
		//   bcompact  = (b0b1<<5, b2b3<<7, b4b5<<1, 0)
		Vec256 bfinal = _mm256_or_si256(_mm256_shuffle_epi8(bcompact, shuf_compactb1), _mm256_shuffle_epi8(bcompact, shuf_compactb2));
		out = _mm256_or_si256(out, bfinal);

		store128u(out_block + bc7prep::target_offs(target[iblock + 0]), lo_half(out));
		store128u(out_block + bc7prep::target_offs(target[iblock + 1]), hi_half(out));
	}

	return iblock;
}

template<typename Tsplit, bool t_switch_colorspace>
UINTa un_munge_bc7_mode3_avx2(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target)
{
	UINTa iblock = 0;

	const Vec256 mask_partbits_lo	= broadcast128_256(_mm_setr_epi32(0x3f, 0, 0, 0));
	const Vec256 mask_partbits_hi	= broadcast128_256(_mm_setr_epi32(0xc0, 0, 0, 0));
	const Vec256 mode_bits			= broadcast128_256(_mm_setr_epi32(8, 0, 0, 0));
	const Vec256 shuf_ybroad		= broadcast128_256(_mm_setr_epi8(0,0,0, 3,3,3, 6,6,6, 9,9,9, -1,-1,-1,-1));
	const Vec256 shuf_crcb			= broadcast128_256(_mm_setr_epi8(1,-1,2, 4,-1,5, 7,-1,8, 10,-1,11, -1,-1,-1,-1));
	const Vec256 shuf_deint			= broadcast128_256(_mm_setr_epi8(0,3,6,9, 1,4,7,10, 2,5,8,11, -1,-1,-1,-1));
	const Vec256 mask_lo7bits		= broadcast128_256(_mm_set1_epi8(0x7f));
	const Vec256 mul_compact1		= broadcast128_256(_mm_setr_epi8(1,-128, 1,-128, 1,-128, 1,-128, 1,-128, 1,-128, 1,-128, 1,-128));
	const Vec256 mul_compact2		= broadcast128_256(_mm_setr_epi16(4,1, 4,1, 4,1, 4,1));
	const Vec256 mask_bfinal		= broadcast128_256(_mm_setr_epi32(0, 0, 0x3ffffffc,0));
	const Vec256 mask_compact3r		= broadcast128_256(_mm_setr_epi32(0x3ffffffc, 0, 0, 0));
	const Vec256 mask_compact3g 	= broadcast128_256(_mm_setr_epi32(-(1 << 30), 0x3ffffff, 0, 0));
	const Vec256 mask_index			= broadcast128_256(_mm_setr_epi32(0, 0, 0, -1));

	for (; iblock < (nblocks & ~1); iblock += 2)
	{
		Vec256 endpts, indbits;
		vec_load_single<Tsplit>(endpts, indbits, first_ptr, second_ptr, iblock);

		U32 partbits = _mm256_movemask_epi8(endpts); // can grab partition bits immediately

		if (t_switch_colorspace)
		{
			Vec256 ybroad = _mm256_shuffle_epi8(endpts, shuf_ybroad);
			Vec256 crcb = _mm256_shuffle_epi8(endpts, shuf_crcb);
			endpts = _mm256_add_epi8(ybroad, crcb);
		}

		// de-interleave R,G,B
		Vec256 deint = _mm256_shuffle_epi8(endpts, shuf_deint);
		Vec256 deint7 = _mm256_and_si256(deint, mask_lo7bits);
		// we now have 7-bit values low in 8-bit lanes

		// compact pass 1
		Vec256 compact1 = _mm256_maddubs_epi16(mul_compact1, deint7);
		// now: 14-bit values low in 16-bit lanes

		// compact pass 2
		Vec256 compact2 = _mm256_mullo_epi16(compact1, mul_compact2);
		// now: 28-bit values shifted by 2 in 32-bit lanes

		// compact pass 3: get rid of the 4-bit gap between r and g;
		// we leave b alone here, since the bits are already exactly
		// where we need them!
		Vec256 bfinal = _mm256_and_si256(compact2, mask_bfinal);
		Vec256 rmasked = _mm256_and_si256(compact2, mask_compact3r);
		Vec256 galigned = _mm256_and_si256(_mm256_srli_epi64(compact2, 4), mask_compact3g);
		Vec256 rgmerged = _mm256_or_si256(rmasked, galigned); // 56 bits of r+g, shifted by 2 in a 64-bit lane

		// build the result block
		Vec256 partbits_unpack = _mm256_cvtepu8_epi64(_mm_cvtsi32_si128(partbits)); // cvtepu8_epi8 works to put partition bits in the right place
		Vec256 result = _mm256_and_si256(partbits_unpack, mask_partbits_lo);
		result = _mm256_or_si256(_mm256_slli_epi32(result, 4), mode_bits); // mode + partbits
		result = _mm256_or_si256(bfinal, result);

		Vec256 final_part = _mm256_slli_si256(_mm256_and_si256(partbits_unpack, mask_partbits_hi), 11);
		Vec256 final_part_and_index = _mm256_or_si256(final_part, _mm256_and_si256(indbits, mask_index));

		result = _mm256_or_si256(result, _mm256_slli_si256(rgmerged, 1));
		result = _mm256_or_si256(result, final_part_and_index);

		store128u(out_block + bc7prep::target_offs(target[iblock + 0]), lo_half(result));
		store128u(out_block + bc7prep::target_offs(target[iblock + 1]), hi_half(result));
	}

	return iblock;
}

// AVX2 128b version. This particular mode can really benefit from
// some AVX2-only instructions (namely, per-lane variable shifts).
template<typename Tsplit, bool t_switch_colorspace>
UINTa un_munge_bc7_mode4_avx2_narrow(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target)
{
	UINTa iblock = 0;

	const Vec128 mask_chan0		= _mm_setr_epi32(0xffc, 0, 0xffc, 0);
	const Vec128 mul_ybroad		= _mm_setr_epi32(1 + (1<<10) + (1<<20), 0, 1 + (1<<10) + (1<<20), 0);
	const Vec128 mask_chan23	= _mm_setr_epi32(-(1 << 22), 0x3ff, -(1 << 22), 0x3ff);
	const Vec128 mask_nonmsb	= _mm_setr_epi32(0x7bdef7bf,0x1ef, 0x7bdef7bf,0x1ef);
	const Vec128 mask_rgba		= _mm_setr_epi32(-4,0x3ff, -4,0x3ff);
	const Vec128 mask_crot		= _mm_setr_epi32(3,0, 3,0);
	const Vec128 crot_lut		= _mm_setr_epi8(0,30,20,10, 0,0,0,0, 0,0,0,0, 0,0,0,0);
	const Vec128 mask_rgbaxor	= _mm_setr_epi32(0,0x3ff, 0,0x3ff);
	const Vec128 mul_spread		= _mm_setr_epi16(1,1,2,0, 1,1,2,0);
	const Vec128 mask_rgba_mv2	= _mm_setr_epi32(0,0x7c0, 0,0x7c0);
	const Vec128 mask_bits01	= _mm_setr_epi32(3,0, 3,0);
	const Vec128 mul_alphalo	= _mm_setr_epi16(0x21,0,0,0, 0x21,0,0,0);
	const Vec128 mask_alphalo	= _mm_setr_epi16(0x41,0,0,0, 0x41,0,0,0);
	const Vec128 mask_bit7		= _mm_setr_epi32(0x80,0, 0x80,0);
	const Vec128 mode_bits		= _mm_setr_epi32(0x10,0, 0x10,0);
	const Vec128 mask_hi0		= _mm_setr_epi32(0, -(1<<18), 0, -(1<<18));

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
		// We have per-lane variable shifts in AVX2, which makes things easier than it is
		// with SSSE3.

		// Form ((0 - crot) & 3) * 10) which is our shift amount, using a table lookup.
		Vec128 crot_masked = _mm_and_si128(_mm_srli_epi64(hi0, 48), mask_crot);
		Vec128 crot_shift = _mm_shuffle_epi8(crot_lut, crot_masked);

		// Also start assembling output word
		Vec128 out_lo = _mm_or_si128(mode_bits, _mm_and_si128(hi0, mask_hi0));
		out_lo = _mm_or_si128(out_lo, _mm_slli_epi64(crot_masked, 5));
		out_lo = _mm_or_si128(out_lo, _mm_and_si128(_mm_srli_epi64(lo, 35), mask_bit7));

		// Do the swap using XORs
		Vec128 rgba_shifted = _mm_sllv_epi64(rgba, crot_shift);
		Vec128 xor_mask = _mm_and_si128(_mm_xor_si128(rgba_shifted, rgba), mask_rgbaxor);
		rgba = _mm_xor_si128(rgba, xor_mask);
		rgba = _mm_xor_si128(rgba, _mm_srlv_epi64(xor_mask, crot_shift));

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
UINTa un_munge_bc7_mode4_avx2(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target)
{
	UINTa iblock = 0;

	const Vec256 mask_chan0		= broadcast128_256(_mm_setr_epi32(0xffc, 0, 0xffc, 0));
	const Vec256 mul_ybroad		= broadcast128_256(_mm_setr_epi32(1 + (1<<10) + (1<<20), 0, 1 + (1<<10) + (1<<20), 0));
	const Vec256 mask_chan23	= broadcast128_256(_mm_setr_epi32(-(1 << 22), 0x3ff, -(1 << 22), 0x3ff));
	const Vec256 mask_nonmsb	= broadcast128_256(_mm_setr_epi32(0x7bdef7bf,0x1ef, 0x7bdef7bf,0x1ef));
	const Vec256 mask_rgba		= broadcast128_256(_mm_setr_epi32(-4,0x3ff, -4,0x3ff));
	const Vec256 mask_crot		= broadcast128_256(_mm_setr_epi32(3,0, 3,0));
	const Vec256 crot_lut		= broadcast128_256(_mm_setr_epi8(0,30,20,10, 0,0,0,0, 0,0,0,0, 0,0,0,0));
	const Vec256 mask_rgbaxor	= broadcast128_256(_mm_setr_epi32(0,0x3ff, 0,0x3ff));
	const Vec256 mul_spread		= broadcast128_256(_mm_setr_epi16(1,1,2,0, 1,1,2,0));
	const Vec256 mask_rgba_mv2	= broadcast128_256(_mm_setr_epi32(0,0x7c0, 0,0x7c0));
	const Vec256 mask_bits01	= broadcast128_256(_mm_setr_epi32(3,0, 3,0));
	const Vec256 mul_alphalo	= broadcast128_256(_mm_setr_epi16(0x21,0,0,0, 0x21,0,0,0));
	const Vec256 mask_alphalo	= broadcast128_256(_mm_setr_epi16(0x41,0,0,0, 0x41,0,0,0));
	const Vec256 mask_bit7		= broadcast128_256(_mm_setr_epi32(0x80,0, 0x80,0));
	const Vec256 mode_bits		= broadcast128_256(_mm_setr_epi32(0x10,0, 0x10,0));
	const Vec256 mask_hi0		= broadcast128_256(_mm_setr_epi32(0, -(1<<18), 0, -(1<<18)));

	for (; iblock < (nblocks & ~3); iblock += 4)
	{
		// Load both halves
		Vec256 lo, hi0, hi1;
		vec_load_split6<Tsplit>(lo, hi0, hi1, first_ptr, second_ptr, iblock);

		Vec256 rgba;
		// change color space if required
		if (t_switch_colorspace)
		{
			Vec256 yval = _mm256_and_si256(lo, mask_chan0);
			// broadcast "Y" (G)
			Vec256 ybroad = _mm256_mul_epu32(yval, mul_ybroad);
			// shuffle "Cr"/"Cb"/A around
			Vec256 crcba = _mm256_or_si256(_mm256_and_si256(_mm256_srli_epi32(lo, 10), mask_chan0), _mm256_and_si256(lo, mask_chan23));

			// and a single packed add to merge everything, which we have to do manually
			rgba = vec_packed_add(ybroad, crcba, mask_nonmsb);
		}
		else
			rgba = lo;

		// Mask to keep only the main RGBA bits
		rgba = _mm256_and_si256(rgba, mask_rgba);

		// Rotate color channels back.
		// This is a bit of a production since we don't have the variable shifts we'd like.
		//
		// We have per-lane variable shifts in AVX2, which makes things easier than it is
		// with SSSE3.

		// Form ((0 - crot) & 3) * 10) which is our shift amount, using a table lookup.
		Vec256 crot_masked = _mm256_and_si256(_mm256_srli_epi64(hi0, 48), mask_crot);
		Vec256 crot_shift = _mm256_shuffle_epi8(crot_lut, crot_masked);

		// Also start assembling output word
		Vec256 out_lo = _mm256_or_si256(mode_bits, _mm256_and_si256(hi0, mask_hi0));
		out_lo = _mm256_or_si256(out_lo, _mm256_slli_epi64(crot_masked, 5));
		out_lo = _mm256_or_si256(out_lo, _mm256_and_si256(_mm256_srli_epi64(lo, 35), mask_bit7));

		// Do the swap using XORs
		Vec256 rgba_shifted = _mm256_sllv_epi64(rgba, crot_shift);
		Vec256 xor_mask = _mm256_and_si256(_mm256_xor_si256(rgba_shifted, rgba), mask_rgbaxor);
		rgba = _mm256_xor_si256(rgba, xor_mask);
		rgba = _mm256_xor_si256(rgba, _mm256_srlv_epi64(xor_mask, crot_shift));

		// Make space for the extra A bits
		Vec256 rgba_spread1 = _mm256_mullo_epi16(rgba, mul_spread); // handles the parts that move left by 0 or 1 bits
		Vec256 rgba_spread = _mm256_add_epi16(rgba_spread1, _mm256_and_si256(rgba_spread1, mask_rgba_mv2)); // handle the parts that move left by 2 bits

		// Insert the extra A bits
		Vec256 a_lobits = _mm256_and_si256(lo, mask_bits01);
		Vec256 a_lobits_spread = _mm256_mullo_epi16(a_lobits, mul_alphalo); // yet another "multiple shifts via mul"
		Vec256 a_lobits_final = _mm256_and_si256(a_lobits_spread, mask_alphalo);
		rgba_spread = _mm256_or_si256(rgba_spread, _mm256_shuffle_epi32(a_lobits_final, 0xb1));

		// Insert into final output
		out_lo = _mm256_or_si256(out_lo, _mm256_slli_epi64(rgba_spread, 6));

		// Interleave to form results
		Vec256 out0 = _mm256_unpacklo_epi64(out_lo, hi1);
		Vec256 out1 = _mm256_unpackhi_epi64(out_lo, hi1);
		store128u(out_block + bc7prep::target_offs(target[iblock + 0]), lo_half(out0));
		store128u(out_block + bc7prep::target_offs(target[iblock + 1]), lo_half(out1));
		store128u(out_block + bc7prep::target_offs(target[iblock + 2]), hi_half(out0));
		store128u(out_block + bc7prep::target_offs(target[iblock + 3]), hi_half(out1));
	}

	return iblock;
}

template<typename Tsplit, bool t_switch_colorspace>
UINTa un_munge_bc7_mode5_avx2(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target)
{
	UINTa iblock = 0;

	const Vec256 mask_nonlsb		= broadcast128_256(_mm_set1_epi8(-2));
	const Vec256 shuf_ybroad		= broadcast128_256(_mm_setr_epi8(0,0,0,-1, 4,4,4,-1, 8,8,8,-1, 12,12,12,-1));
	const Vec256 shuf_crcba			= broadcast128_256(_mm_setr_epi8(1,-1,2,3, 5,-1,6,7, 9,-1,10,11, 13,-1,14,15));
	const Vec256 mask_bit01			= broadcast128_256(_mm_setr_epi32(3,0, 3,0));
	const Vec256 shuf_broadcrot		= broadcast128_256(_mm_setr_epi8(0,0,0,0, 0,0,0,0, 8,8,8,8, 8,8,8,8));
	const Vec256 mask_shufbase0   	= broadcast128_256(_mm_setr_epi8(0,0,4,4, 8,8,12,12, 0,0,4,4, 8,8,12,12));
	const Vec256 lut_chanrots_cs	= broadcast128_256(_mm_setr_epi8(0,3,0,0, 1,1,3,1, 2,2,2,3, 3,0,1,2)); // permutations: 0123, 3120, 0321, 0132
	const Vec256 mask_shufbase_cs1	= broadcast128_256(_mm_setr_epi8(0,4,0,4, 0,4,0,4, 8,12,8,12, 8,12,8,12));
	const Vec256 lut_chanrots_nocs	= broadcast128_256(_mm_setr_epi8(0,6,0,0, 2,2,6,2, 4,4,4,6, 6,0,2,4)); // permutations: 0123, 3120, 0321, 0132
	const Vec256 mask_shufbase_noc	= broadcast128_256(_mm_setr_epi8(0,1,0,1, 0,1,0,1, 8,9,8,9, 8,9,8,9));
	const Vec256 mode_bits			= broadcast128_256(_mm_setr_epi32(0x20,0, 0x20,0));
	const Vec256 mask_alphas		= broadcast128_256(_mm_setr_epi16(0,0,0,-1, 0,0,0,-1));
	const Vec256 mask_precompact	= broadcast128_256(_mm_set1_epi8(-2));
	const Vec256 mul_compact1		= broadcast128_256(_mm_setr_epi8(-1,-128, -1,-128, -1,-128, 0,0, -1,-128, -1,-128, -1,-128, 0,0));
	const Vec256 mul_compact2		= broadcast128_256(_mm_setr_epi16(-1,-(1<<14), -(1<<4),0, -1,-(1<<14), -(1<<4),0));
	const Vec256 mask_lo32			= broadcast128_256(_mm_setr_epi32(-1,0, -1,0));

	for (; iblock < (nblocks & ~3); iblock += 4)
	{
		// Load both halves
		Vec256 endpoints, indices;
		vec_load_pair<Tsplit>(endpoints, indices, first_ptr, second_ptr, iblock);

		Vec256 crot = _mm256_and_si256(indices, mask_bit01);
		indices = _mm256_xor_si256(indices, crot); // keep only the actual index bits

		Vec256 crot_shuffle;

		if (t_switch_colorspace)
		{
			Vec256 endpoints_hi = _mm256_and_si256(endpoints, mask_nonlsb);
			Vec256 ybroad = _mm256_shuffle_epi8(endpoints_hi, shuf_ybroad);
			Vec256 crcba = _mm256_shuffle_epi8(endpoints_hi, shuf_crcba);
			Vec256 sum = _mm256_add_epi8(ybroad, crcba);
			endpoints = _mm256_or_si256(sum, _mm256_andnot_si256(mask_nonlsb, endpoints));

			// deinterleave and channel rotate at the same time
			Vec256 crot_broad = _mm256_shuffle_epi8(crot, shuf_broadcrot);
			Vec256 crot_shufind = _mm256_or_si256(crot_broad, mask_shufbase0);
			Vec256 crot_shufbase = _mm256_shuffle_epi8(lut_chanrots_cs, crot_shufind);
			crot_shuffle = _mm256_or_si256(crot_shufbase, mask_shufbase_cs1);
		}
		else
		{
			// Just need to do channel rotate here, we're already deinterleaved
			Vec256 crot_broad = _mm256_shuffle_epi8(crot, shuf_broadcrot); // crot in every byte
			Vec256 crot_shufind = _mm256_or_si256(crot_broad, mask_shufbase0);
			Vec256 crot_shufbase = _mm256_shuffle_epi8(lut_chanrots_nocs, crot_shufind);
			crot_shuffle = _mm256_or_si256(crot_shufbase, mask_shufbase_noc);
		}

		// Start preparing the output block
		Vec256 out_lo = _mm256_or_si256(mode_bits, _mm256_slli_epi64(crot, 6));

		// Do the channel swap and deinterleave
		endpoints = _mm256_shuffle_epi8(endpoints, crot_shuffle);

		// Nowe we have the channel bits in (post-rotate) deinterleaved
		// R,G,B,A order:
		//   r0,r1, g0,g1, b0,b1, a0,a1
		// The rgb's are 7-bit values packed into 8-bit lanes, shifted by 1.
		// We need to compact them next (this masks out A for now).
		Vec256 alphas = _mm256_and_si256(endpoints, mask_alphas);
		Vec256 masked = _mm256_and_si256(endpoints, mask_precompact);
		Vec256 compact1 = _mm256_maddubs_epi16(masked, mul_compact1);
		// now we have 16-bit lanes with 14-bit values: (-r0r1 << 1, -g0g1 << 1, -b0b1 << 1)

		// Alpha straddles the 64-bit halves which is a bit awkward
		out_lo = _mm256_or_si256(out_lo, _mm256_slli_epi64(alphas, 2));
		indices = _mm256_or_si256(indices, _mm256_srli_epi64(alphas, 62));

		// Second compaction pass
		Vec256 compact2a = _mm256_madd_epi16(compact1, mul_compact2);
		Vec256 compact2 = _mm256_srli_epi32(compact2a, 1);
		// now we have 32-bit lanes containing (r0r1g0g1, b0b1<<4)

		// Merge the rgb bitfields into the output block
		out_lo = _mm256_or_si256(out_lo, _mm256_slli_epi64(_mm256_and_si256(compact2, mask_lo32), 8)); // r+g
		out_lo = _mm256_or_si256(out_lo, _mm256_andnot_si256(mask_lo32, compact2)); // b (this could be PBLENDW w/ SSE4)

		// Interleave and write
		Vec256 out0 = _mm256_unpacklo_epi64(out_lo, indices);
		Vec256 out1 = _mm256_unpackhi_epi64(out_lo, indices);

		store128u(out_block + bc7prep::target_offs(target[iblock + 0]), lo_half(out0));
		store128u(out_block + bc7prep::target_offs(target[iblock + 1]), lo_half(out1));
		store128u(out_block + bc7prep::target_offs(target[iblock + 2]), hi_half(out0));
		store128u(out_block + bc7prep::target_offs(target[iblock + 3]), hi_half(out1));
	}

	return iblock;
}

template<typename Tsplit, bool t_switch_colorspace>
UINTa un_munge_bc7_mode6_avx2(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target)
{
	UINTa iblock = 0;

	const Vec256 mask_7bits     = broadcast128_256(_mm_set1_epi8(0x7f));
	const Vec256 mask_p0        = broadcast128_256(_mm_setr_epi32(0, -2*(1 << 30), 0, -2*(1 << 30))); // this is 1<<31, just written to avoid constant overflow warnings. sigh.
	const Vec256 mode_bits      = broadcast128_256(_mm_setr_epi32(0x40, 0, 0x40, 0));
	const Vec256 step1_mult     = broadcast128_256(_mm_setr_epi16(2,2, 1,1, 2,2, 1,1));
	const Vec256 step1_yshuf    = broadcast128_256(_mm_setr_epi8(0,4, 0,4, 0,4, -1,-1, 8,12, 8,12, 8,12, -1,-1));
	const Vec256 step1_crcbshuf = broadcast128_256(_mm_setr_epi8(1,5, -1,-1, 2,6, 3,7, 9,13, -1,-1, 10,14, 11,15));
	const Vec256 step1_mask     = broadcast128_256(_mm_set1_epi16(0x7ffe));
	const Vec256 step2_mult     = broadcast128_256(_mm_setr_epi16(4,1, 4,1, 4,1, 4,1));
	const Vec256 step1_mult_nocs= broadcast128_256(_mm_setr_epi8(-1,-128, -1,-128, -1,-128, -1,-128, -1,-128, -1,-128, -1,-128, -1,-128));
	const Vec256 step2_mult_nocs= broadcast128_256(_mm_setr_epi16(-4,-1, -4,-1, -4,-1, -4,-1));
	const Vec256 step3_rgmask   = broadcast128_256(_mm_setr_epi32(-0x80, 7, -0x80, 7));
	const Vec256 step3_bamask   = broadcast128_256(_mm_setr_epi32(0, 0x7ffffff8, 0, 0x7ffffff8));

	for (; iblock < (nblocks & ~3); iblock += 4)
	{
		// load both halves
		Vec256 endpoints, indices;
		vec_load_pair<Tsplit>(endpoints, indices, first_ptr, second_ptr, iblock);

		// Throw out the stowaway bits
		Vec256 endpoint_payloads = _mm256_and_si256(endpoints, mask_7bits);
		Vec256 mode_p0 = _mm256_or_si256(_mm256_and_si256(endpoints, mask_p0), mode_bits);

		Vec256 step2_result;

		if (t_switch_colorspace)
		{
			// Step 1: pack to form 14-bit groups within 16-bit lanes
			Vec256 step1_products = _mm256_mullo_epi16(endpoint_payloads, step1_mult);

			// do colorspace switching while we have everything still packed in bytes, for convenience
			Vec256 step1_y    = _mm256_shuffle_epi8(step1_products, step1_yshuf);
			Vec256 step1_crcb = _mm256_shuffle_epi8(step1_products, step1_crcbshuf);
			Vec256 step1_result = _mm256_and_si256(_mm256_add_epi8(step1_y, step1_crcb), step1_mask);
			// now the 16-bit lanes contain: (r0r1 << 1, g0g1 << 1, b0b1 << 1, ...)

			// Step 2: pack again to form 28-bit groups within 32-bit lanes
			Vec256 step2_shr1 = _mm256_srli_epi16(step1_result, 1);
			step2_result = _mm256_mullo_epi16(step2_shr1, step2_mult);
			// now the 32-bit lanes contain: (r0r1g0g1 << 2, b0b1a0a1 << 2, ...)
		}
		else
		{
			// Step 1: pack to form 14-bit groups within 16-bit lanes
			Vec256 step1_result = _mm256_maddubs_epi16(endpoint_payloads, step1_mult_nocs);
			// now the 16-bit lanes contain (-r0r1, -g0g1, -b0b1, -a0a1)

			// Step 2: pack again to form 28-bit groups within 32-bit lanes
			step2_result = _mm256_mullo_epi16(step1_result, step2_mult_nocs);
			// now the 32-bit lanes contain: (r0r1g0g1 << 2, b0b1a0a1 << 2)
		}

		// No point doing a similar 3rd pass here, just move the two halves to their final position
		// manually.
		Vec256 step3_rg = _mm256_and_si256(_mm256_slli_epi64(step2_result, 5), step3_rgmask);
		Vec256 step3_ba = _mm256_and_si256(_mm256_add_epi32(step2_result, step2_result), step3_bamask);

		// Combine with P0 and mode bits
		Vec256 lo_result = _mm256_or_si256(_mm256_or_si256(step3_rg, mode_p0), step3_ba);

		// Now interleave lo and hi halves to produce output
		Vec256 out0 = _mm256_unpacklo_epi64(lo_result, indices);
		Vec256 out1 = _mm256_unpackhi_epi64(lo_result, indices);
		store128u(out_block + bc7prep::target_offs(target[iblock + 0]), lo_half(out0));
		store128u(out_block + bc7prep::target_offs(target[iblock + 1]), lo_half(out1));
		store128u(out_block + bc7prep::target_offs(target[iblock + 2]), hi_half(out0));
		store128u(out_block + bc7prep::target_offs(target[iblock + 3]), hi_half(out1));
	}

	return iblock;
}

template<typename Tsplit, bool t_switch_colorspace>
UINTa un_munge_bc7_mode7_avx2(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target)
{
	UINTa iblock = 0;

	if (t_switch_colorspace)
	{
		const Vec256 mask_lsbs16	= broadcast128_256(_mm_set1_epi16(1));
		const Vec256 mul_pbits0		= broadcast128_256(_mm_setr_epi16(1,2, 1,2, 1,2, 1,2));
		const Vec256 mask_chan0		= broadcast128_256(_mm_set1_epi16(0x1f));
		const Vec256 mul_compact1	= broadcast128_256(_mm_setr_epi8(1,1<<5, 1,1<<5, 1,1<<5, 1,1<<5, 1,1<<5, 1,1<<5, 1,1<<5, 1,1<<5));
		const Vec256 mul_compact2	= broadcast128_256(_mm_setr_epi16(1<<2,1<<12, 1<<2,1<<12, 1<<2,1<<12, 1<<2,1<<12));
		const Vec256 mul_compactb	= broadcast128_256(_mm_setr_epi16(1<<6,1<<11,1<<0,1<<5, 1<<6,1<<11,1<<0,1<<5));
		const Vec256 mode_bits		= broadcast128_256(_mm_setr_epi32(0x80, 0, 0x80, 0));
		const Vec256 mask_partbits	= broadcast128_256(_mm_setr_epi32(0x3f00, 0, 0x3f00, 0));
		const Vec256 mask_rbits		= broadcast128_256(_mm_setr_epi32(-(1<<14), 3, -(1<<14), 3));
		const Vec256 mask_gbits		= broadcast128_256(_mm_setr_epi32(0, (1<<22)-(1<<2), 0, (1<<22)-(1<<2)));
		const Vec256 mask_abits		= broadcast128_256(_mm_setr_epi32((1<<30)-(1<<10), 0, (1<<30)-(1<<10), 0));
		const Vec256 mask_pbits		= broadcast128_256(_mm_setr_epi32(-(1<<30),3, -(1<<30),3));
		const Vec256 mask_indbits	= broadcast128_256(_mm_setr_epi32(0,-(1<<2), 0,-(1<<2)));

		for (; iblock < (nblocks & ~3); iblock += 4)
		{
			Vec256 prgbs, rest;
			vec_load_pair<Tsplit>(prgbs, rest, first_ptr, second_ptr, iblock);

			// Grab the P bits from the LSBs of the PRGB values
			Vec256 pbits0 = _mm256_and_si256(prgbs, mask_lsbs16);
			Vec256 pbits1 = _mm256_madd_epi16(pbits0, mul_pbits0); // collect pbits from 16b lanes into 32-bit lanes with 2 combined pbits
			// now there's a 30-bit gap between the two halves, compact that
			Vec256 pbits2 = _mm256_slli_epi64(pbits1, 30);
			Vec256 pbits3 = _mm256_or_si256(pbits1, pbits2);

			// Grab y/cr/cb bits (don't mask just yet)
			Vec256 ybits = _mm256_srli_epi16(prgbs, 1);
			Vec256 crbits = _mm256_srli_epi16(prgbs, 6);
			Vec256 cbbits = _mm256_srli_epi16(prgbs, 11);

			// Do the color space change
			Vec256 gbits = ybits;
			Vec256 rbits = _mm256_add_epi16(crbits, ybits);
			Vec256 bbits = _mm256_add_epi16(cbbits, ybits);

			// Mask down to 5 bits per channel
			rbits = _mm256_and_si256(rbits, mask_chan0);
			gbits = _mm256_and_si256(gbits, mask_chan0);
			bbits = _mm256_and_si256(bbits, mask_chan0);

			// Pack down to 8 bits
			Vec256 rgbits_8 = _mm256_packs_epi16(rbits, gbits);

			// We have 5-bit values in 8-bit lanes; time to compact!
			Vec256 rgcompact1 = _mm256_maddubs_epi16(rgbits_8, mul_compact1);
			// we now have 10-bit values in 16-bit lanes
			// rg is: (a.r0r1, a.r2r3, b.r0r1, b.r2r3, a.g0g1, a.g2g3, b.g0g1, b.g2g3)

			// Second compaction pass
			Vec256 rgcompact2 = _mm256_madd_epi16(rgcompact1, mul_compact2);
			// we now have 32-bit lanes containing 20-bit values:
			//   rgcompact2 = (a.r0r1r2r3<<2, b.r0r1r2r3<<2, a.g0g1g2g3<<2, b.g0g1g2g3<<2)

			// Shuffle into desired order
			Vec256 rgsorted = _mm256_shuffle_epi32(rgcompact2, 0xd8);
			// now:
			//   rgsorted   = (a.r0r1r2r3<<2, a.g0g1g2g3<<2, b.r0r1r2r3<<2, b.g0g1g2g3<<2)

			// Blue bits can be done directly
			Vec256 bbits_grp = _mm256_madd_epi16(bbits, mul_compactb);
			// we now have 10-bit values in 16-bit lanes, aligned as pieces of a 32-bit value but with gaps in between
			//   bbits_grp  = (a.b0b1<<6, 0, a.b2b3, 0, b.b0b1<<6, 0, b.b2b3)

			// Form output blocks
			Vec256 out_lo = _mm256_or_si256(mode_bits, _mm256_and_si256(_mm256_srli_epi64(rest, 20), mask_partbits));
			out_lo = _mm256_or_si256(out_lo, _mm256_slli_epi64(bbits_grp, 48));
			out_lo = _mm256_or_si256(out_lo, _mm256_and_si256(_mm256_slli_epi64(rgsorted, 12), mask_rbits));
			out_lo = _mm256_or_si256(out_lo, _mm256_and_si256(rgsorted, mask_gbits));

			Vec256 out_hi = _mm256_srli_epi64(bbits_grp, 32);
			out_hi = _mm256_or_si256(out_hi, _mm256_and_si256(_mm256_slli_epi64(rest, 2), mask_abits));
			out_hi = _mm256_or_si256(out_hi, _mm256_and_si256(pbits3, mask_pbits));
			out_hi = _mm256_or_si256(out_hi, _mm256_and_si256(rest, mask_indbits));

			// Interleave and write
			Vec256 out0 = _mm256_unpacklo_epi64(out_lo, out_hi);
			Vec256 out1 = _mm256_unpackhi_epi64(out_lo, out_hi);

			store128u(out_block + bc7prep::target_offs(target[iblock + 0]), lo_half(out0));
			store128u(out_block + bc7prep::target_offs(target[iblock + 1]), lo_half(out1));
			store128u(out_block + bc7prep::target_offs(target[iblock + 2]), hi_half(out0));
			store128u(out_block + bc7prep::target_offs(target[iblock + 3]), hi_half(out1));
		}
	}
	else // if (t_switch_colorspace)
	{
		const Vec256 shuf_expand0	= broadcast128_256(_mm_setr_epi8(0,1,-1,-1, -1,4,5,-1, 8,9,-1,-1, -1,12,13,-1));
		const Vec256 mask_expand1	= broadcast128_256(_mm_setr_epi16(0,-1,0,0, 0,-1,0,0));
		const Vec256 mask_expand2	= broadcast128_256(_mm_setr_epi32(0x0ff000ff, 0x0000ff00, 0x0ff000ff, 0x0000ff00));
		const Vec256 mask_expand3	= broadcast128_256(_mm_setr_epi32(0x0f03c0f0, 0x03c0f03c, 0x0f03c0f0, 0x03c0f03c));
		const Vec256 shuf_exp1to5	= broadcast128_256(_mm_setr_epi8(1,2, 1,2, 2,3, 2,3, 9,10, 9,10, 10,11, 10,11));
		const Vec256 mul_exp1to5a	= broadcast128_256(_mm_setr_epi16(1<<(16-4), 1<<(16-8), 1<<(16-3), 1<<(16-6), 1<<(16-4), 1<<(16-8), 1<<(16-3), 1<<(16-6)));
		const Vec256 mask_exp1to5a	= broadcast128_256(_mm_setr_epi16(15, 7, 7, 3,  15, 7, 7, 3));
		const Vec256 mul_exp1to5b	= broadcast128_256(_mm_setr_epi16(0x1111, 0x1110, 0x888, 0x44, 0x1111, 0x1110, 0x888, 0x44));
		const Vec256 mask_exp1to5b	= broadcast128_256(_mm_setr_epi32(0x42108421, 0x00842108, 0x42108421, 0x00842108));
		const Vec256 mask_alobits	= broadcast128_256(_mm_setr_epi32(0xf, 0, 0xf, 0));
		const Vec256 mode_bits		= broadcast128_256(_mm_setr_epi32(0x80, 0, 0x80, 0));
		const Vec256 mask_partbits	= broadcast128_256(_mm_setr_epi32(0x3f00, 0, 0x3f00, 0));
		const Vec256 mask_pbits   	= broadcast128_256(_mm_setr_epi32(-(1 << 30), 3, -(1 << 30), 3));
		const Vec256 mask_indbits	= broadcast128_256(_mm_setr_epi32(0,-(1<<2), 0,-(1<<2)));

		for (; iblock < (nblocks & ~3); iblock += 4)
		{
			Vec256 prgbs, rest;
			vec_load_pair<Tsplit>(prgbs, rest, first_ptr, second_ptr, iblock);

			// expand4to5_12x(prgbs)
			// pass 1: move by multiples of 4
			Vec256 expand_rgb0	= _mm256_shuffle_epi8(prgbs, shuf_expand0);
			Vec256 expand_rgb1	= _mm256_and_si256(prgbs, mask_expand1);
			Vec256 expand_rgb2	= _mm256_or_si256(expand_rgb0, _mm256_slli_epi64(expand_rgb1, 4));

			// pass 2: move by 2
			Vec256 expand_rgb3	= _mm256_and_si256(expand_rgb2, mask_expand2);
			Vec256 expand_rgb4	= _mm256_andnot_si256(mask_expand2, expand_rgb2);
			Vec256 expand_rgb5	= _mm256_or_si256(expand_rgb3, _mm256_slli_epi64(expand_rgb4, 2));

			// pass 3: move by 1
			Vec256 expand_rgb6	= _mm256_and_si256(expand_rgb5, mask_expand3);
			Vec256 expand_rgb7	= _mm256_add_epi64(expand_rgb5, expand_rgb6);
			Vec256 rgbbits0		= _mm256_slli_epi64(expand_rgb7, 1); // expand4to5_12x(prgbs) << 1

			// expand4to5_4x(prgbs >> 48)
			// pass 1: move by 2
			Vec256 prgbhigh		= _mm256_srli_epi64(prgbs, 48);
			Vec256 exphi_rgb0	= _mm256_and_si256(prgbhigh, mask_expand2);
			Vec256 exphi_rgb1	= _mm256_andnot_si256(mask_expand2, prgbhigh);
			Vec256 exphi_rgb2	= _mm256_or_si256(exphi_rgb0, _mm256_slli_epi64(exphi_rgb1, 2));

			// pass 2: move by 1
			Vec256 exphi_rgb3	= _mm256_and_si256(exphi_rgb2, mask_expand3);
			Vec256 exphi_rgb4	= _mm256_add_epi64(exphi_rgb2, exphi_rgb3);
			Vec256 abits0		= _mm256_slli_epi64(exphi_rgb4, 1); // expand4to5_4x(prgbs >> 48) << 1

			// expand1to5_12x(rest >> 12)
			Vec256 lobits0		= _mm256_shuffle_epi8(rest, shuf_exp1to5); // grab words
			Vec256 lobits1		= _mm256_mulhi_epu16(lobits0, mul_exp1to5a); // shift right to align relevant bits at bottom
			Vec256 lobits2		= _mm256_and_si256(lobits1, mask_exp1to5a); // mask active groups
			Vec256 lobits3		= _mm256_mullo_epi16(lobits2, mul_exp1to5b); // mul to spread out
			Vec256 lobits4		= _mm256_and_si256(lobits3, mask_exp1to5b); // mask results
			Vec256 rgbbits1		= _mm256_or_si256(rgbbits0, lobits4); // rgbbits |= exapnd1to5_12x(rest >> 12)

			// expand1to5_4x(rest >> 24)
			Vec256 alobits0		= _mm256_srli_epi64(rest, 24);
			Vec256 alobits1		= _mm256_and_si256(alobits0, mask_alobits);
			Vec256 alobits2		= _mm256_madd_epi16(alobits1, mul_exp1to5b);
			Vec256 alobits3		= _mm256_and_si256(alobits2, mask_exp1to5b);
			Vec256 abits1		= _mm256_or_si256(abits0, alobits3); // abits |= expand1to5_4x(rest >> 24)

			// partition bits
			Vec256 partbits		= _mm256_and_si256(_mm256_srli_epi64(rest, 20), mask_partbits);

			// Form output blocks
			Vec256 out_lo		= _mm256_or_si256(mode_bits, partbits);
			out_lo				= _mm256_or_si256(out_lo, _mm256_slli_epi64(rgbbits1, 14));

			Vec256 out_hi		= _mm256_and_si256(rest, mask_indbits);
			out_hi				= _mm256_or_si256(out_hi, _mm256_and_si256(_mm256_slli_epi64(rest, 22), mask_pbits));
			out_hi				= _mm256_or_si256(out_hi, _mm256_srli_epi64(rgbbits1, 50));
			out_hi				= _mm256_or_si256(out_hi, _mm256_slli_epi64(abits1, 10));

			// Interleave and write
			Vec256 out0 = _mm256_unpacklo_epi64(out_lo, out_hi);
			Vec256 out1 = _mm256_unpackhi_epi64(out_lo, out_hi);

			store128u(out_block + bc7prep::target_offs(target[iblock + 0]), lo_half(out0));
			store128u(out_block + bc7prep::target_offs(target[iblock + 1]), lo_half(out1));
			store128u(out_block + bc7prep::target_offs(target[iblock + 2]), hi_half(out0));
			store128u(out_block + bc7prep::target_offs(target[iblock + 3]), hi_half(out1));
		}
	}

	return iblock;
}

template<typename T>
static inline T * advance_by_bytes(T * ptr, SINTa value)
{
	return (T*) ((U8 *)ptr + value);
}

static void sort_pass_avx2(U16 * mode_cur[16], const U8 * mode_nibbles, SINTa blocks_in_chunk)
{
	RR_ASSERT(blocks_in_chunk <= BC7PREP_CHUNK_BLOCK_COUNT);
	RAD_ALIGN(U8, mode_bytes[BC7PREP_CHUNK_BLOCK_COUNT + 32], 32);
	SINTa i;

	// Unpack modes from nibbles to bytes
	for (i = 0; i < (blocks_in_chunk & ~31); i += 32)
	{
		// Each byte turns into two, so do a 128-bit load with unsinged
		// extend
		Vec256 modes = _mm256_cvtepu8_epi16(load128u(mode_nibbles + (i >> 1)));

		// We have 16-bit words containing packed nibble bytes with the original
		// data. Move the nibbles in place in the output word.
		Vec256 lonib = _mm256_and_si256(modes, _mm256_set1_epi16(0xf));
		Vec256 hinib = _mm256_and_si256(_mm256_slli_epi16(modes, 4), _mm256_set1_epi16(0x0f00));
		Vec256 merged = _mm256_or_si256(lonib, hinib);

		store256u(mode_bytes + i, merged);
	}

	// no problem with overrun, BC7PREP_CHUNK_BLOCK_COUNT is even so there's always space
	for (; i < blocks_in_chunk; i += 2)
	{
		U8 nibbles = mode_nibbles[i >> 1];
		mode_bytes[i + 0] = nibbles & 0xf;
		mode_bytes[i + 1] = nibbles >> 4;
	}
	
	// actual sorting loop
	SINTa tail_start_at = blocks_in_chunk - 31; // keep going as long as we have a full vector's worth of valid mode bytes
	U64 processed_mask = 0; // NOTE it's crucial for this to 64b (or at least, more than 32b)
	for (i = 0; i < tail_start_at; )
	{
		Vec256 modes = load256u(mode_bytes + i);
		Vec128 base_i = _mm_set1_epi16((short) i);
		U8 active_mode = mode_bytes[i];

		// check which lanes match active
		Vec256 matching_lanes = _mm256_cmpeq_epi8(modes, _mm256_broadcastb_epi8(lo_half(modes)));

		// turn into mask
		U32 matching_mask = _mm256_movemask_epi8(matching_lanes);

		// emit compacted output indices for this mode
		U16 * out_ptr = mode_cur[active_mode];

		// Compute pop counts on the matching lanes via PSADBW
		// we sum 2 for each matching lane since that's how many bytes we write.
		Vec256 matching_bit = _mm256_and_si256(matching_lanes, _mm256_set1_epi8(2));
		Vec256 matching_sums = _mm256_sad_epu8(matching_bit, _mm256_setzero_si256());

		// Extract the individual pop counts
		Vec128 sums_lo = lo_half(matching_sums);
		Vec128 sums_hi = hi_half(matching_sums);

		U32 pop0 = _mm_cvtsi128_si32(sums_lo);
		U32 pop1 = _mm_extract_epi32(sums_lo, 2);
		U32 pop2 = _mm_cvtsi128_si32(sums_hi);
		U32 pop3 = _mm_extract_epi32(sums_hi, 2);

		Vec128 base0 = base_i;
		Vec128 base1 = _mm_add_epi16(base_i, _mm_set1_epi16(8));
		Vec128 base2 = _mm_add_epi16(base_i, _mm_set1_epi16(16));
		Vec128 base3 = _mm_add_epi16(base_i, _mm_set1_epi16(24));

		Vec128 inds0 = load128u(sort_compact_table[matching_mask & 0xff]);
		Vec128 inds1 = load128u(sort_compact_table[(matching_mask >> 8) & 0xff]);
		Vec128 inds2 = load128u(sort_compact_table[(matching_mask >> 16) & 0xff]);
		Vec128 inds3 = load128u(sort_compact_table[matching_mask >> 24]);

		store128u(out_ptr, _mm_add_epi16(base0, inds0)); out_ptr = advance_by_bytes(out_ptr, pop0);
		store128u(out_ptr, _mm_add_epi16(base1, inds1)); out_ptr = advance_by_bytes(out_ptr, pop1);
		store128u(out_ptr, _mm_add_epi16(base2, inds2)); out_ptr = advance_by_bytes(out_ptr, pop2);
		store128u(out_ptr, _mm_add_epi16(base3, inds3)); out_ptr = advance_by_bytes(out_ptr, pop3);

		mode_cur[active_mode] = out_ptr;

		// flag lanes as processed and skip to first unprocessed
		processed_mask |= matching_mask;
		U32 skip = rrCtz64(~processed_mask); // processed_mask is 64b so we have guaranteed 0 bits on top
		processed_mask >>= skip; // NOTE this is the main reason we need processed_mask to be 64b: skip==32 is possible!
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

// 256b AVX2
OptimizedDecoderKernelSet opt_kernels_avx2 =
{
	{
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode0_avx2, SplitAt8),
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode1_avx2, SplitAt8),
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode2_avx2, SplitAt12),
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode3_avx2, SplitAt12),
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode4_avx2, SplitAt6),
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode5_avx2, SplitAt8),
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode6_avx2, SplitAt8),
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode7_avx2, SplitAt12),
		UNMUNGE_SINGLE_KERNEL(un_munge_bc7_mode8_sse2), // mode 8
		UNMUNGE_NO_KERNEL, // mode 9
	},
	sort_pass_avx2, // sort
};

// Narrow (128b) AVX2; this is mostly just the SSSE3 kernels, but
// mode 4 gets a sizeable benefit from 128b AVX2 insns.
OptimizedDecoderKernelSet opt_kernels_avx2_narrow =
{
	{
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode0_sse2, SplitAt8),
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode1_ssse3, SplitAt8),
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode2_ssse3, SplitAt12),
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode3_ssse3, SplitAt12),
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode4_avx2_narrow, SplitAt6),
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

#else // DO_BUILD_AVX2

#if defined(_MSC_VER) && !defined(__RADJAGUAR__) // expected to be off on Jaguar targets since they don't have AVX2
#pragma RR_PRAGMA_MESSAGE("NOTE: bc7prep_decode_avx2 disabled")
#endif

EXPORT_SOME_CRAP(bc7prep_decode_avx2);

#endif // DO_BUILD_AVX2


