// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "bc7prep_decode.h"
#include "bc7prep_decode_sse.inl"

OODLE_NS_START
namespace bc7prep {

template<typename Tsplit, bool t_switch_colorspace>
UINTa un_munge_bc7_mode0_sse2(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target)
{
	UINTa iblock = 0;

	// SSE2 version
	const __m128i mask_chan01	= _mm_setr_epi32(-1,0xffff, -1,0xffff);
	const __m128i mask_chan0	= _mm_setr_epi32(0xffffff,0, 0xffffff,0);
	const __m128i mask_nonmsb	= _mm_setr_epi32(0x77777777,0x7777, 0x77777777,0x7777); // 48 bits worth each
	const __m128i mode_bits		= _mm_setr_epi32(1,0, 1,0);
	const __m128i mask_partbits	= _mm_setr_epi32(0x1e,0, 0x1e,0);
	const __m128i mask_inds		= _mm_setr_epi32(-(1 << 13),-1, -(1 << 13),-1);

	for (; iblock < (nblocks & ~1); iblock += 2)
	{
		// Load both halves
		Vec128 lo, hi;
		vec_load_pair<Tsplit>(lo, hi, first_ptr, second_ptr, iblock);

		// Grab rgb payload bits
		Vec128 rgbits;
		Vec128 bbits = _mm_and_si128(_mm_or_si128(_mm_srli_epi64(lo, 48), _mm_slli_epi64(hi, 16)), mask_chan0);

		if (t_switch_colorspace)
		{
			Vec128 y = _mm_and_si128(lo, mask_chan0);
			Vec128 y_2x = _mm_or_si128(y, _mm_slli_epi64(y, 24)); // two copies

			rgbits = vec_packed_add(y_2x, _mm_and_si128(_mm_srli_epi64(lo, 24), mask_chan0), mask_nonmsb); // r and g
			bbits = vec_packed_add(bbits, y, mask_nonmsb);
		}
		else
			rgbits = _mm_and_si128(lo, mask_chan01);

		// Assemble output
		Vec128 out_lo = _mm_or_si128(mode_bits, _mm_and_si128(_mm_srli_epi64(hi, 7), mask_partbits));
		out_lo = _mm_or_si128(out_lo, _mm_slli_epi64(rgbits, 5));
		out_lo = _mm_or_si128(out_lo, _mm_slli_epi64(bbits, 53));

		Vec128 out_hi = _mm_srli_epi64(bbits, 11);
		out_hi = _mm_or_si128(out_hi, _mm_and_si128(hi, mask_inds));

		// Interleave and write
		Vec128 out0 = _mm_unpacklo_epi64(out_lo, out_hi);
		Vec128 out1 = _mm_unpackhi_epi64(out_lo, out_hi);

		store128u(out_block + target_offs(target[iblock + 0]), out0);
		store128u(out_block + target_offs(target[iblock + 1]), out1);
	}

	return iblock;
}

UINTa un_munge_bc7_mode8_sse2(U8 * RADRESTRICT out_block, const U8 * coded_block, const U8 *, UINTa nblocks, const U16 * target)
{
	for (UINTa block = 0; block < nblocks; block++)
		store128u(out_block + target_offs(target[block]), load128u(coded_block + block*16));

	return nblocks;
}

OptimizedDecoderKernelSet opt_kernels_sse2 =
{
	{
		UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode0_sse2, SplitAt8),
		UNMUNGE_NO_KERNEL, // mode 1
		UNMUNGE_NO_KERNEL, // mode 2
		UNMUNGE_NO_KERNEL, // mode 3
		UNMUNGE_NO_KERNEL, // mode 4
		UNMUNGE_NO_KERNEL, // mode 5
		UNMUNGE_NO_KERNEL, // mode 6
		UNMUNGE_NO_KERNEL, // mode 7
		UNMUNGE_SINGLE_KERNEL(un_munge_bc7_mode8_sse2), // mode 8
		UNMUNGE_NO_KERNEL, // mode 9
	},
	0, // sort
};

} // namespace bc7prep
OODLE_NS_END

