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

		for (SINTa i = 0; i < NPAIRS; ++i)
		{
			Vec256_S16 mask = (subset_mask.s16() & cur_mask).cmp_eq(cur_mask);

			Vec256_S16 r = VecInt(pixel_prep[0][i]).s16() & mask;
			Vec256_S16 g = VecInt(pixel_prep[1][i]).s16() & mask;
			Vec256_S16 b = VecInt(pixel_prep[2][i]).s16() & mask;
			Vec256_S16 ones { 1 };

			sumi[0] += r.madd(ones);
			sumi[1] += g.madd(ones);
			sumi[2] += b.madd(ones);
			sumi[3] += r.madd(r);
			sumi[4] += g.madd(g);
			sumi[5] += b.madd(b);
			sumi[6] += r.madd(g);
			sumi[7] += g.madd(b);
			sumi[8] += b.madd(r);

			cur_mask = cur_mask.shl<2>();
		}
	}
};

void subset_prepare_avx2(F32 * results, const U8 pixels[], const U32 subset_masks[], const F32 subset_scale[], SINTa subset_count, SINTa stride, bool is_rgba)
{
	if (!is_rgba)
		subset_prepare_rgb<SubsetTraitsAVX2>(results, pixels, subset_masks, subset_scale, subset_count, stride);
	else
		subset_prepare_rgba<SubsetTraitsAVX2>(results, pixels, subset_masks, subset_scale, subset_count, stride);
}

} // internal namespace

OODLE_NS_END

#endif
