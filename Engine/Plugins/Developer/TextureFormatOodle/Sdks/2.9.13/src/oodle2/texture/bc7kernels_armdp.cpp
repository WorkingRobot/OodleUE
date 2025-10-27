// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetarmdp

#include "bc7kernels.h"
#include "vec128.inl"
#include "bc7kernels.inl"

#ifdef DO_BUILD_ARM_DOTPROD

OODLE_NS_START

namespace internal {

struct SubsetTraitsARMDP
{
	static constexpr int LANES = 4;
	typedef Vec128_S32 VecInt;
	typedef Vec128_U16 VecUInt16;
	typedef VecF32x4 VecFlt;
};

template<>
struct SubsetCovarCalcRGB<SubsetTraitsARMDP>
{
	static constexpr int NPIXELS = 16;
	using VecInt = SubsetTraitsARMDP::VecInt;

	alignas(4) U8 expanded[3][NPIXELS]; // [chan][index]

	void init(const U8 pixels_rgba[])
	{
		// Deinterleave pixels and immediately store
		uint8x16x4_t pixels = vld4q_u8(pixels_rgba);
		vst1q_u8(expanded[0], pixels.val[0]);
		vst1q_u8(expanded[1], pixels.val[1]);
		vst1q_u8(expanded[2], pixels.val[2]);
	}

	void query(VecInt sumi[9], const U32 * subset_masks)
	{
		uint32x4_t accu[9];
		for (SINTa i = 0; i < 9; ++i)
			accu[i] = vdupq_n_u32(0);

		Vec128_U32 subset_mask = Vec128_U32::loadu(subset_masks);

		// Shuffle mask to select low 8 bits of subset mask and repeat 4x
		Vec128_S8 subset_mask_shuf { 0,0,0,0, 4,4,4,4, 8,8,8,8, 12,12,12,12 };
		Vec128_U8 subset_mask_test_bits = Vec128_U8::repeat4(1<<0, 1<<1, 1<<2, 1<<3);
		Vec128_U8 ones { 1 };

		// we get four separate subset masks and want to sum across groups of 4 pixels each
		for (SINTa i = 0; i < NPIXELS; i += 4)
		{
			Vec128_U8 subset_mask_active = subset_mask.u8().shuf(subset_mask_shuf);
			Vec128_U8 pixel_enable { vtstq_u8(subset_mask_active, subset_mask_test_bits) };

			// Grab groups of 4x red, green, blue and mask to active pixels only
			Vec128_U8 r = Vec128_U8::loadu_dup32(&expanded[0][i]) & pixel_enable;
			Vec128_U8 g = Vec128_U8::loadu_dup32(&expanded[1][i]) & pixel_enable;
			Vec128_U8 b = Vec128_U8::loadu_dup32(&expanded[2][i]) & pixel_enable;

			// Accumulate
			accu[0] = vdotq_u32(accu[0], r, ones);
			accu[1] = vdotq_u32(accu[1], g, ones);
			accu[2] = vdotq_u32(accu[2], b, ones);

			accu[3] = vdotq_u32(accu[3], r, r);
			accu[4] = vdotq_u32(accu[4], g, g);
			accu[5] = vdotq_u32(accu[5], b, b);
			accu[6] = vdotq_u32(accu[6], r, g);
			accu[7] = vdotq_u32(accu[7], g, b);
			accu[8] = vdotq_u32(accu[8], b, r);

			// Advance mask to next 4 bits
			subset_mask = subset_mask.srl<4>();
		}

		for (SINTa i = 0; i < 9; ++i)
			sumi[i] = VecInt { accu[i] };
	}
};

template<>
struct SubsetCovarCalcRGBA<SubsetTraitsARMDP>
{
	static constexpr int NPIXELS = 16;
	using VecInt = SubsetTraitsARMDP::VecInt;

	alignas(4) U8 expanded[4][NPIXELS]; // [chan][index]

	void init(const U8 pixels_rgba[])
	{
		// Deinterleave pixels and immediately store
		uint8x16x4_t pixels = vld4q_u8(pixels_rgba);
		vst1q_u8(expanded[0], pixels.val[0]);
		vst1q_u8(expanded[1], pixels.val[1]);
		vst1q_u8(expanded[2], pixels.val[2]);
		vst1q_u8(expanded[3], pixels.val[3]);
	}

	void query(VecInt sumi[14], const U32 * subset_masks)
	{
		uint32x4_t accu[14];
		for (SINTa i = 0; i < 14; ++i)
			accu[i] = vdupq_n_u32(0);

		Vec128_U32 subset_mask = Vec128_U32::loadu(subset_masks);

		// Shuffle mask to select low 8 bits of subset mask and repeat 4x
		Vec128_S8 subset_mask_shuf { 0,0,0,0, 4,4,4,4, 8,8,8,8, 12,12,12,12 };
		Vec128_U8 subset_mask_test_bits = Vec128_U8::repeat4(1<<0, 1<<1, 1<<2, 1<<3);
		Vec128_U8 ones { 1 };

		// we get four separate subset masks and want to sum across groups of 4 pixels each
		for (SINTa i = 0; i < NPIXELS; i += 4)
		{
			Vec128_U8 subset_mask_active = subset_mask.u8().shuf(subset_mask_shuf);
			Vec128_U8 pixel_enable { vtstq_u8(subset_mask_active, subset_mask_test_bits) };

			// Grab groups of 4 r/g/b/a values, mask to active pixels only
			Vec128_U8 r = Vec128_U8::loadu_dup32(&expanded[0][i]) & pixel_enable;
			Vec128_U8 g = Vec128_U8::loadu_dup32(&expanded[1][i]) & pixel_enable;
			Vec128_U8 b = Vec128_U8::loadu_dup32(&expanded[2][i]) & pixel_enable;
			Vec128_U8 a = Vec128_U8::loadu_dup32(&expanded[3][i]) & pixel_enable;

			// This does the horizontal summing and summing of product terms
			accu[ 0] = vdotq_u32(accu[ 0], r, ones);
			accu[ 1] = vdotq_u32(accu[ 1], g, ones);
			accu[ 2] = vdotq_u32(accu[ 2], b, ones);
			accu[ 3] = vdotq_u32(accu[ 3], a, ones);

			accu[ 4] = vdotq_u32(accu[ 4], r, r);
			accu[ 5] = vdotq_u32(accu[ 5], g, g);
			accu[ 6] = vdotq_u32(accu[ 6], b, b);
			accu[ 7] = vdotq_u32(accu[ 7], a, a);

			accu[ 8] = vdotq_u32(accu[ 8], r, g);
			accu[ 9] = vdotq_u32(accu[ 9], g, b);
			accu[10] = vdotq_u32(accu[10], b, a);
			accu[11] = vdotq_u32(accu[11], a, r);

			accu[12] = vdotq_u32(accu[12], r, b);
			accu[13] = vdotq_u32(accu[13], g, a);

			// Advance mask to next 4 bits
			subset_mask = subset_mask.srl<4>();
		}

		for (SINTa i = 0; i < 14; ++i)
			sumi[i] = VecInt { accu[i] };
	}
};

template<>
struct SubsetCovarCalc1Subset<SubsetTraitsARMDP>
{
	static constexpr int NPIXELS = 16;
	using VecInt = SubsetTraitsARMDP::VecInt;

	void compute(VecInt sumi[14], const U8 pixels[], const S8 * shuffles)
	{
		uint32x4_t accu[14];
		for (SINTa i = 0; i < 14; ++i)
			accu[i] = vdupq_n_u32(0);

		Vec128_S8 base_shuffle = Vec128_S8::loadu(shuffles);
		Vec128_S8 component_shuffle[4];

		Vec128_S8 prepare_shuf { 0,0,0,0, 4,4,4,4, 8,8,8,8, 12,12,12,12 };

		for (SINTa i = 0; i < 4; ++i)
		{
			// Grab byte index for active channel
			component_shuffle[i] = base_shuffle.shuf(prepare_shuf) + Vec128_U32 { 0x0c080400 }.s8();
	
			// Advance to next channel
			base_shuffle = base_shuffle.s32().srl<8>().s8();
		}

		Vec128_U8 ones { 1 };

		// we get four separate subset masks and want to sum across groups of 4 pixels each
		for (SINTa i = 0; i < NPIXELS; i += 4)
		{
			// Grab four pixels
			Vec128_U8 pixel = Vec128_U8::loadu(&pixels[i*4]);

			// Per 32-bit lane, grab four sequential U8 channel values
			Vec128_U8 r = pixel.shuf(component_shuffle[0]);
			Vec128_U8 g = pixel.shuf(component_shuffle[1]);
			Vec128_U8 b = pixel.shuf(component_shuffle[2]);
			Vec128_U8 a = pixel.shuf(component_shuffle[3]);

			// This does the horizontal summing and summing of product terms
			accu[ 0] = vdotq_u32(accu[ 0], r, ones);
			accu[ 1] = vdotq_u32(accu[ 1], g, ones);
			accu[ 2] = vdotq_u32(accu[ 2], b, ones);
			accu[ 3] = vdotq_u32(accu[ 3], a, ones);

			accu[ 4] = vdotq_u32(accu[ 4], r, r);
			accu[ 5] = vdotq_u32(accu[ 5], g, g);
			accu[ 6] = vdotq_u32(accu[ 6], b, b);
			accu[ 7] = vdotq_u32(accu[ 7], a, a);

			accu[ 8] = vdotq_u32(accu[ 8], r, g);
			accu[ 9] = vdotq_u32(accu[ 9], g, b);
			accu[10] = vdotq_u32(accu[10], b, a);
			accu[11] = vdotq_u32(accu[11], a, r);

			accu[12] = vdotq_u32(accu[12], r, b);
			accu[13] = vdotq_u32(accu[13], g, a);
		}

		for (SINTa i = 0; i < 14; ++i)
			sumi[i] = VecInt { accu[i] };
	}
};

void subset_prepare_armdp(F32 * results, const U8 pixels[], const U32 subset_masks[], const F32 subset_scale[], SINTa subset_count, SINTa stride, bool is_rgba)
{
	if (!is_rgba)
		subset_prepare_rgb<SubsetTraitsARMDP>(results, pixels, subset_masks, subset_scale, subset_count, stride);
	else
		subset_prepare_rgba<SubsetTraitsARMDP>(results, pixels, subset_masks, subset_scale, subset_count, stride);
}

void subset_prepare_1subset_armdp(F32 * results, const U8 pixels[], SINTa stride)
{
	subset_prepare_rgba_1subset<SubsetTraitsARMDP>(results, pixels, stride);
}

} // internal namespace

OODLE_NS_END

#endif // DO_BUILD_ARM_DOTPROD

