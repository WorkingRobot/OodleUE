// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetavx2

// We _MUST NOT_ allow the compiler to contract mul-add operations to FMAs;
// this changes the results between AVX2 and SSE4 versions which we don't allow!
#include "nocontract.h"

#include "bc7kernels.h"
#include "vec256.inl"
#include "bc7kernels.inl"

#ifdef DO_BUILD_AVX2

OODLE_NS_START

namespace internal {

struct SubsetTraitsAVX2
{
	static constexpr int LANES = 8;
	typedef Vec256_S32 VecInt;
	typedef Vec128_U16 VecUInt16;
	typedef Vec256_F32 VecFlt;
};

template<>
struct SubsetCovarCalcRGB<SubsetTraitsAVX2>
{
	static constexpr int NPAIRS = 8;
	using VecInt = SubsetTraitsAVX2::VecInt;

	U32 pixel_prep[3][NPAIRS]; // [chan][pair]

	void init(const U8 pixels[])
	{
		for (SINTa i = 0; i < NPAIRS; ++i)
		{
			for (SINTa j = 0; j < 3; ++j)
				pixel_prep[j][i] = pixels[i*8 + 0 + j] + (pixels[i*8 + 4 + j] << 16);
		}
	}

	void query(VecInt sumi[9], const U32 * subset_masks)
	{
		for (SINTa i = 0; i < 9; ++i)
			sumi[i] = VecInt::zero();

		VecInt subset_mask = VecInt::loadu(subset_masks);
		subset_mask |= subset_mask.shl<16>(); // make two copies

		Vec256_S16 cur_mask = Vec256_S32(0x00020001).s16();
		Vec256_S16 rsum = Vec256_S16::zero();
		Vec256_S16 gsum = Vec256_S16::zero();
		Vec256_S16 bsum = Vec256_S16::zero();

		for (SINTa i = 0; i < NPAIRS; ++i)
		{
			Vec256_S16 mask = (subset_mask.s16() & cur_mask).cmp_eq(cur_mask);

			Vec256_S16 r = VecInt(pixel_prep[0][i]).s16() & mask;
			Vec256_S16 g = VecInt(pixel_prep[1][i]).s16() & mask;
			Vec256_S16 b = VecInt(pixel_prep[2][i]).s16() & mask;

			rsum += r;
			gsum += g;
			bsum += b;
			sumi[3] += r.madd(r);
			sumi[4] += g.madd(g);
			sumi[5] += b.madd(b);
			sumi[6] += r.madd(g);
			sumi[7] += g.madd(b);
			sumi[8] += b.madd(r);

			cur_mask = cur_mask.shl<2>();
		}

		// Finish horizontal reduction for linear terms
		Vec256_S16 ones { 1 };
		sumi[0] = rsum.madd(ones);
		sumi[1] = gsum.madd(ones);
		sumi[2] = bsum.madd(ones);
	}
};

template<>
struct SubsetCovarCalcRGBA<SubsetTraitsAVX2>
{
	static constexpr int NPAIRS = 8;
	using VecInt = SubsetTraitsAVX2::VecInt;

	U32 pixel_prep[4][NPAIRS]; // [chan][pair]

	void init(const U8 pixels[])
	{
		for (SINTa i = 0; i < NPAIRS; ++i)
		{
			for (SINTa j = 0; j < 4; ++j)
				pixel_prep[j][i] = pixels[i*8 + 0 + j] + (pixels[i*8 + 4 + j] << 16);
		}
	}

	void query(VecInt sumi[14], const U32 * subset_masks)
	{
		for (SINTa i = 0; i < 14; ++i)
			sumi[i] = VecInt::zero();

		VecInt subset_mask = VecInt::loadu(subset_masks);
		subset_mask |= subset_mask.shl<16>(); // make two copies

		Vec256_S16 cur_mask = Vec256_S32(0x00020001).s16();
		Vec256_S16 rsum = Vec256_S16::zero();
		Vec256_S16 gsum = Vec256_S16::zero();
		Vec256_S16 bsum = Vec256_S16::zero();
		Vec256_S16 asum = Vec256_S16::zero();

		for (SINTa i = 0; i < NPAIRS; ++i)
		{
			Vec256_S16 mask = (subset_mask.s16() & cur_mask).cmp_eq(cur_mask);

			Vec256_S16 r = VecInt(pixel_prep[0][i]).s16() & mask;
			Vec256_S16 g = VecInt(pixel_prep[1][i]).s16() & mask;
			Vec256_S16 b = VecInt(pixel_prep[2][i]).s16() & mask;
			Vec256_S16 a = VecInt(pixel_prep[3][i]).s16() & mask;

			// The linear terms, we can defer the horizontal reduction for since they're small
			// and even summing the 16-bit values can't possibly overflow over 16 pixels
			rsum += r;
			gsum += g;
			bsum += b;
			asum += a;

			// The squared terms, we handle with PMADDWD
			sumi[ 4] += r.madd(r);
			sumi[ 5] += g.madd(g);
			sumi[ 6] += b.madd(b);
			sumi[ 7] += a.madd(a);

			sumi[ 8] += r.madd(g);
			sumi[ 9] += g.madd(b);
			sumi[10] += b.madd(a);
			sumi[11] += a.madd(r);

			sumi[12] += r.madd(b);
			sumi[13] += g.madd(a);

			cur_mask = cur_mask.shl<2>();
		}

		// Finish horizontal reduction for linear terms
		Vec256_S16 ones { 1 };
		sumi[0] = rsum.madd(ones);
		sumi[1] = gsum.madd(ones);
		sumi[2] = bsum.madd(ones);
		sumi[3] = asum.madd(ones);
	}
};

template<>
struct SubsetCovarCalc1Subset<SubsetTraitsAVX2>
{
	static constexpr int NPIXELS = 16;
	using VecInt = typename SubsetTraitsAVX2::VecInt;

	void compute(VecInt sumi[14], const U8 pixels[], const S8 * shuffles)
	{
		for (SINTa k = 0; k < 14; ++k)
			sumi[k] = VecInt::zero();

		Vec256_S8 base_shuffle = Vec256_S8::loadu(shuffles);
		Vec256_S8 component_shuffle[4];

		constexpr S8 Z = -128;
		Vec256_S8 prepare_shuf {
			0,Z,0,Z, 4,Z,4,Z, 8,Z,8,Z, 12,Z,12,Z,
			0,Z,0,Z, 4,Z,4,Z, 8,Z,8,Z, 12,Z,12,Z
		};

		for (SINTa i = 0; i < 4; ++i)
		{
			// Grab byte index for active channel
			component_shuffle[i] = base_shuffle.shuf_in128(prepare_shuf) + Vec256_S8::repeat4(0, Z, 4, Z);

			// Advance to next channel
			base_shuffle = base_shuffle.s32().srl<8>().s8();
		}

		Vec256_S16 rsum = Vec256_S16::zero();
		Vec256_S16 gsum = Vec256_S16::zero();
		Vec256_S16 bsum = Vec256_S16::zero();
		Vec256_S16 asum = Vec256_S16::zero();

		for (SINTa i = 0; i < NPIXELS; i += 2)
		{
			// Grab two pixels
			Vec256_U8 pixel = Vec256_U8::loadu_dup64(&pixels[i*4]);

			// Per lane, grab two pixel values as 16-bit values
			Vec256_S16 r = pixel.shuf_in128(component_shuffle[0]).s16();
			Vec256_S16 g = pixel.shuf_in128(component_shuffle[1]).s16();
			Vec256_S16 b = pixel.shuf_in128(component_shuffle[2]).s16();
			Vec256_S16 a = pixel.shuf_in128(component_shuffle[3]).s16();

			// The linear terms, we can defer the horizontal reduction for since they're small
			// and even summing the 16-bit values can't possibly overflow over 16 pixels
			rsum += r;
			gsum += g;
			bsum += b;
			asum += a;

			// The rest we resolve in pairs with madds
			sumi[ 4] += r.madd(r);
			sumi[ 5] += g.madd(g);
			sumi[ 6] += b.madd(b);
			sumi[ 7] += a.madd(a);

			sumi[ 8] += r.madd(g);
			sumi[ 9] += g.madd(b);
			sumi[10] += b.madd(a);
			sumi[11] += a.madd(r);

			sumi[12] += r.madd(b);
			sumi[13] += g.madd(a);
		}

		// Finish horizontal reduction for the straight r/g/b/a values
		Vec256_S16 ones { 1 };
		sumi[0] = rsum.madd(ones);
		sumi[1] = gsum.madd(ones);
		sumi[2] = bsum.madd(ones);
		sumi[3] = asum.madd(ones);
	}
};

void subset_prepare_avx2(F32 * results, const U8 pixels[], const U32 subset_masks[], const F32 subset_scale[], SINTa subset_count, SINTa stride, bool is_rgba)
{
	if (!is_rgba)
		subset_prepare_rgb<SubsetTraitsAVX2>(results, pixels, subset_masks, subset_scale, subset_count, stride);
	else
		subset_prepare_rgba<SubsetTraitsAVX2>(results, pixels, subset_masks, subset_scale, subset_count, stride);
}

void subset_prepare_1subset_avx2(F32 * results, const U8 pixels[], SINTa stride)
{
	subset_prepare_rgba_1subset<SubsetTraitsAVX2>(results, pixels, stride);
}

} // internal namespace

OODLE_NS_END

#endif
