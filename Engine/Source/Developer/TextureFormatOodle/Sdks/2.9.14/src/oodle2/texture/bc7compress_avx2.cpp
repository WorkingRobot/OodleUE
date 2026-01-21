// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetavx2

// We _MUST NOT_ allow the compiler to contract mul-add operations to FMAs.
#include "nocontract.h"

#include "bc7compress.h"
#include "bc7compress_internal.h"
#include "bc67tables.h"
#include "vec256.inl"

#ifdef DO_BUILD_AVX2

OODLE_NS_START

namespace internal {

static RADFORCEINLINE Vec256_S32 dot16x16_8x(Vec256_S16r v0_0, Vec256_S16r v0_1, Vec256_S16r v1_0, Vec256_S16r v1_1)
{
	Vec256_S32 dot0 = v0_0.madd(v1_0); // a.x*b.x + a.y*b.y, a.z*b.z + a.w*b.w (4x)
	Vec256_S32 dot1 = v0_1.madd(v1_1);
	// NOTE: "horizontal" adds in AVX2 aren't really, they're horizontal within 128b slices.
	return Vec256_S32 { _mm256_hadd_epi32(dot0, dot1) };
}

static RADFORCEINLINE Vec256_S32 calc_vector_inds_linear_8pix(
	U8 out_inds[8], const U8 *in_pixels,
	Vec256_S16 lo16, Vec256_S16 diff16, Vec256_S16 chan_weight16, Vec256_S16 diff16_weighted,
	Vec256_S8 weights_tab,
	Vec256_F32 index_scale, Vec256_F32 scalef)
{
	// Load the pixels
	// The strange (0,1,4,5) and (2,3,6,7) order is due to the way the shuffles we do in dot16x16_8x work:
	// we start with pixels with 4 16-bit channels spread over two vectors and ultimately produce one 32-bit
	// dot product in a single vector.
	//
	// The first half of the dot product is a regular PMADDWD (a*b + c*d) type operation, but then we end up
	// with pairs of 32-bit lanes we have to sum together, ideally avoiding shuffles crossing 128-bit boundaries
	// because they are more expensive. Since we want "natural order" output, it ends up being easier having
	// pixels 0 and 1 (which produce the low 64 result bits of each 128b "sector") in one vector, and pixels
	// 2 and 3 (producing the high 64 result bits) in another. So even though the layout looks confusing here,
	// it's the easiest way to get a natural order result.
	Vec256_U8 pixels = Vec256_U8::loadu(in_pixels);
	Vec256_S16 pixrel16_0145 = lo16 - Vec256_S16(zext8to16_lo_in128(pixels));
	Vec256_S16 pixrel16_2367 = lo16 - Vec256_S16(zext8to16_hi_in128(pixels));

	// Compute the per-pixel dot products
	// Result of this is now in natural order (32-bit lanes, lane 0..7 are pixels 0..7)
	Vec256_S32 rgba_dots = dot16x16_8x(pixrel16_0145, pixrel16_2367, diff16_weighted, diff16_weighted);

	// Determine the quantized index
	Vec256_F32 qf = rgba_dots.to_f32() * scalef;
	qf = qf.max(Vec256_F32::zero()).min(index_scale);
	Vec256_S32 q = qf.to_int32_round(); // NOTE rounding not truncating! important.

	// Grab low 8 bits of each lane and store
	Vec256_U8 lo8_grouped { _mm256_shuffle_epi8(q, _mm256_set1_epi32(0x0c080400)) };
	Vec256_S32 lo8_adjacent = lo8_grouped.s32().permute(Vec256_S32 { 0,4, 0,0,0,0,0,0 });
	lo8_adjacent.storeu_lo64(out_inds);

	// look up the weights for q in the table
	Vec256_S8 weights_from_q = weights_tab.shuf_in128(q);

#define IDXS(a) -1,a,-1,a,-1,a,-1,a
#define FOURIDXS(a,b,c,d) IDXS(a), IDXS(b), IDXS(c), IDXS(d)
	// creates 4 copies of each of the weights as 16 bits, shifted by 8
	Vec256_S16 lerpf_0 = Vec256_S16(weights_from_q.shuf_in128(Vec256_S8(FOURIDXS(0, 4, 0, 4))));
	Vec256_S16 lerpf_1 = Vec256_S16(weights_from_q.shuf_in128(Vec256_S8(FOURIDXS(8,12, 8, 12))));
#undef FOURIDXS
#undef IDXS

	// perform the interpolation to determine the error
	// we compute:
	//
	//   rounding_shift_right(lerpf * (lo - hi), 15) + (lo - pixel)
	//
	// where lerpf = -(bc7_weight[q] << 9)
	Vec256_S16 err_0145 = lerpf_0.mulhrs(diff16) + pixrel16_0145;
	Vec256_S16 err_2367 = lerpf_1.mulhrs(diff16) + pixrel16_2367;

	// error calc with weighting
	Vec256_S16 errw_0145 = err_0145 * chan_weight16;
	Vec256_S16 errw_2367 = err_2367 * chan_weight16;

	// Finalize error calc
	Vec256_S32 errs = dot16x16_8x(err_0145, err_2367, errw_0145, errw_2367);
	return errs;
}

BC7Error avx2_calc_inds_linear(U8 out_inds[16], const BC7SubsetInput *in, const BC7CalcVectorIndsDesc *desc, const BC7Color endpoints_q[2])
{
	const Vec256_S8 weights_tab = Vec256_S8::loadu_dup128(desc->lerp_factor_neg2x);
	const Vec256_F32 index_scale { desc->scale_factor[0] };

	// Replicate channel weights and set weight of scalar channel to 0
	// in the modes that have separate scalar channels
	Vec256_S16 chan_weight16 = Vec256_S16::loadu_dup64(in->channel_weights);

	// low/high endpoints
	Vec256_S16 endpoints16 = Vec256_U8::loadu_dup64(&endpoints_q[0]).to_s16_lo();
	Vec256_S16 lo16 = endpoints16.s32().shuffle_in128<0,1,0,1>().s16();
	Vec256_S16 hi16 = endpoints16.s32().shuffle_in128<2,3,2,3>().s16();
	Vec256_S16 diff16 = lo16 - hi16;
	Vec256_S16 diff16_weighted = diff16 * chan_weight16;

	Vec256_F32 scalef;
	{
		Vec256_S32 dots = diff16.madd(diff16_weighted); // r+g, b+a, r+g, b+a dot products
		dots += dots.shuffle_in128<1,0,3,2>(); // 4x r+g+b+a
		scalef = index_scale / dots.to_f32();
	}

	// Need to load those here because the index store potentially aliases them (as far as the
	// compiler is concerned)
	const SINTa num_pixels = in->num_pixels;
	const U8 * in_pixels = in->pixels;
	const U8 * count_mask = in->count_mask;
	Vec256_S32 total_err;

	// Need to mask to active pixels only
	total_err = calc_vector_inds_linear_8pix(out_inds + 0, in_pixels + 0*4,
		lo16, diff16, chan_weight16, diff16_weighted, weights_tab, index_scale, scalef) &
		Vec256_S32(_mm256_cvtepi8_epi32(load64u(count_mask + 0)));

	if (num_pixels > 8)
	{
		total_err += calc_vector_inds_linear_8pix(out_inds + 8, in_pixels + 8*4,
			lo16, diff16, chan_weight16, diff16_weighted, weights_tab, index_scale, scalef) &
			Vec256_S32(_mm256_cvtepi8_epi32(load64u(count_mask + 8)));
	}

	// reduce!
	return reduce_add_s32(total_err);
}

BC7Error avx2_calc_inds_linear_twoindex(BC7Error *pscalar_err, U8 out_indv[16], U8 out_inds[16], const BC7SubsetInput *in, const BC7CalcVectorIndsDesc *desc, const BC7Color endpoints_q[2])
{
	RR_ASSERT(in->num_pixels == 16); // separate scalar channel implies single subset
	BC7Error vector_err;
	const U8 * in_pixels = in->pixels;

	// Vector channels
	{
		const Vec256_S8 weights_tab = Vec256_S8::loadu_dup128(desc->lerp_factor_neg2x);
		const Vec256_F32 index_scale { desc->scale_factor[0] };

		// Replicate channel weights and set weight of scalar channel to 0
		Vec256_S16 chan_weight16 = Vec256_S16::loadu_dup64(in->channel_weights) & Vec256_S16::repeat4(-1, -1, -1, 0);

		// low/high endpoints
		Vec256_S16 endpoints16 = Vec256_U8::loadu_dup64(&endpoints_q[0]).to_s16_lo();
		Vec256_S16 lo16 = endpoints16.s32().shuffle_in128<0,1,0,1>().s16();
		Vec256_S16 hi16 = endpoints16.s32().shuffle_in128<2,3,2,3>().s16();
		Vec256_S16 diff16 = lo16 - hi16;
		Vec256_S16 diff16_weighted = diff16 * chan_weight16;

		Vec256_F32 scalef;
		{
			Vec256_S32 dots = diff16.madd(diff16_weighted); // r+g, b+a, r+g, b+a dot products
			dots += dots.shuffle_in128<1,0,3,2>(); // 4x r+g+b+a
			scalef = index_scale / dots.to_f32();
		}

		// Need to load those here because the index store potentially aliases them (as far as the
		// compiler is concerned)
		Vec256_S32 total_err;

		total_err = calc_vector_inds_linear_8pix(out_indv + 0, in_pixels + 0*4,
			lo16, diff16, chan_weight16, diff16_weighted, weights_tab, index_scale, scalef);

		total_err += calc_vector_inds_linear_8pix(out_indv + 8, in_pixels + 8*4,
			lo16, diff16, chan_weight16, diff16_weighted, weights_tab, index_scale, scalef);

		vector_err = reduce_add_s32(total_err);
	}

	// Scalar channel
	{
		// lo endpoint and difference between endpoints
		const Vec256_S32 lo32 { endpoints_q[0].a };
		const int diff = endpoints_q[0].a - endpoints_q[1].a; // NOTE: lo - hi not hi - lo!

		// Grab all 16 pixel's alpha and extend to 32 bits ("a" is in bits [31:24] of every 32b group)
		// then subtract base value
		Vec256_S32 rel0 = lo32 - Vec256_S32::loadu(in_pixels +  0*4).srl<24>();
		Vec256_S32 rel1 = lo32 - Vec256_S32::loadu(in_pixels +  8*4).srl<24>();
		Vec256_S32 total_scalar_err;

		if (diff != 0) // Non-degenerate?
		{
			Vec256_F32 scalef(desc->scalar_scale_factor[0] / F32(diff));

			// Determine the quantized index via floats, then convert to int via rounding (important)
			Vec256_S32 q0 = (rel0.to_f32() * scalef).to_int32_round();
			Vec256_S32 q1 = (rel1.to_f32() * scalef).to_int32_round();

			// Pack relative difference into 16 bits and indices into 16
			Vec256_S16 rel16 = rel0.to_s16_sat(rel1); // pixels 0-3, 8-b, 4-7, c-f in that order
			Vec256_S16 q16 = q0.to_s16_sat(q1); // pixels 0-3, 8-b, 4-7, c-f in that order

			// Pack indices into 8 bits and reorder them to be in sequence
			// unsigned saturation takes care of clamping at 0
			Vec128_U8 q8 = Vec128_S16(lo_half(q16)).to_u8_sat(Vec128_S16(hi_half(q16)));
			
			// Clamp upper end of indices and finalize
			// store indices in regular order
			q8 = q8.min(Vec128_U8::loada(desc->scalar_inds_max));
			q8.s32().shuf<0,2,1,3>().u8().storeu(out_inds); // shuffle gives pixels 0-3, 4-7, 8-b, c-f.

			// Determine lerp weights via table lookup from final index
			// note the q8 here are still in the 0-3, 8-b, 4-7, c-f order (matching rel16)
			const Vec128_S8 weights_tab = Vec128_S8::loadu(desc->scalar_lerp_factor_neg2x);
			Vec128_S8 lerp_weights = weights_tab.shuf(q8);

			// Expand lerp weights to 16 bits and shift left by 8
			Vec256_S16 lerpf16 = Vec256_S16 { _mm256_cvtepi8_epi16(lerp_weights) }.shl<8>();

			// Perform the interpolation to determine the error
			Vec256_S16 diff16 { S16(diff) };
			Vec256_S16 err16 = lerpf16.mulhrs(diff16) + rel16;

			// Squared error calc
			// Still in 0-3, 8-b, 4-7, c-f pixel order.
			total_scalar_err = err16.madd(err16);
		}
		else
		{
			// Since all our interpolated values are the same, the rel32 are
			// exactly our errors, and the indices can be all-0.
			Vec128_U8::zero().storeu(out_inds);

			Vec256_S16 err16 = rel0.to_s16_sat(rel1); // pixels 0-3, 8-b, 4-7, c-f in that order
			total_scalar_err = err16.madd(err16);
		}

		// Reduce and apply weight!
		// Note we never got out of the permuted pixel order but it's not a problem
		// since all we do is sum things together.
		*pscalar_err = reduce_add_s32(total_scalar_err) * in->channel_weights[3];
	}

	return vector_err;
}

template<int t_base>
static RADFORCEINLINE Vec256_S32 calc_vector_err_8pix(Vec256_S8 weights_from_q,
	const U8 *in_pixels, const U8 *count_mask,
	Vec256_S16 lo16, Vec256_S16 diff16, Vec256_S16 chan_weight16)
{
	// Load the pixels
	// see calc_vector_inds_linear_8pix for an explanation of the odd lane order
	Vec256_U8 pixels = Vec256_U8::loadu(in_pixels);
	Vec256_S16 pixrel16_0145 = Vec256_S16(zext8to16_lo_in128(pixels)) - lo16;
	Vec256_S16 pixrel16_2367 = Vec256_S16(zext8to16_hi_in128(pixels)) - lo16;

#define IDXS(a) -1,a,-1,a,-1,a,-1,a
#define FOURIDXS(a,b,c,d) IDXS(t_base + a), IDXS(t_base + b), IDXS(t_base + c), IDXS(t_base + d)
	// creates 4 copies of each of the weights as 16 bits, shifted by 8
	Vec256_S16 lerpf_0 = Vec256_S16(weights_from_q.shuf_in128(Vec256_S8(FOURIDXS(0, 1, 4, 5))));
	Vec256_S16 lerpf_1 = Vec256_S16(weights_from_q.shuf_in128(Vec256_S8(FOURIDXS(2, 3, 6, 7))));
#undef FOURIDXS
#undef IDXS

	// perform the interpolation to determine the error
	// we compute:
	//
	//   rounding_shift_right(lerpf * (lo - hi), 15) - (pixel - lo)
	//
	// where lerpf = -(bc7_weight[q] << 9)
	Vec256_S16 err_0145 = lerpf_0.mulhrs(diff16) - pixrel16_0145;
	Vec256_S16 err_2367 = lerpf_1.mulhrs(diff16) - pixrel16_2367;

	// error calc with weighting
	Vec256_S16 errw_0145 = err_0145 * chan_weight16;
	Vec256_S16 errw_2367 = err_2367 * chan_weight16;

	// Add errors for active pixels only
	Vec256_S32 errs = dot16x16_8x(err_0145, err_2367, errw_0145, errw_2367);
	return errs & Vec256_S32(_mm256_cvtepi8_epi32(load64u(count_mask)));
}

BC7Error avx2_calc_vector_err(const U8 inds[], const BC7SubsetInput *in, const BC7CalcVectorIndsDesc *desc, const BC7Color endpoints_q[2])
{
	// Translate indices into lerp factors
	const Vec256_S8 weights_tab = Vec256_S8::loadu_dup128(desc->lerp_factor_neg2x);
	const Vec256_S8 lerpf8 = weights_tab.shuf_in128(Vec256_U8::loadu_dup128(inds));

	// Replicate channel weights and set weight of scalar channel to 0
	// in the modes that have separate scalar channels
	Vec256_S16 chan_weight16 = Vec256_S16::loadu_dup64(in->channel_weights) & Vec256_S16::loadu_dup128(desc->vec_channel_mask);

	// low/high endpoints
	Vec256_S16 endpoints16 = Vec256_U8::loadu_dup64(&endpoints_q[0]).to_s16_lo();
	Vec256_S16 lo16 = endpoints16.s32().shuffle_in128<0,1,0,1>().s16();
	Vec256_S16 hi16 = endpoints16.s32().shuffle_in128<2,3,2,3>().s16();
	Vec256_S16 diff16 = lo16 - hi16;

	const SINTa num_pixels = in->num_pixels;
	const U8 * in_pixels = in->pixels;
	const U8 * count_mask = in->count_mask;
	Vec256_S32 total_err;

	total_err = calc_vector_err_8pix<0>(lerpf8, in_pixels + 0*4, count_mask + 0,
		lo16, diff16, chan_weight16);

	if (num_pixels > 8)
	{
		total_err += calc_vector_err_8pix<8>(lerpf8, in_pixels + 8*4, count_mask + 8,
			lo16, diff16, chan_weight16);
	}

	// reduce!
	return reduce_add_s32(total_err);
}

} // internal namespace

OODLE_NS_END

#endif // DO_BUILD_AVX2
