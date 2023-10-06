// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrfixedfloat.h"
#include "vec128.inl"
#include <string.h>

RR_NAMESPACE_START

#if defined(__RADX86__)

#define HAVE_SIMD_CONVERSIONS

// Float->half conversion with round-to-nearest-even, SSE2+
// leaves half-floats in 32-bit lanes (sign extended)
static inline Vec128 F32_to_F16_4x(const VecF32x4 &f)
{
	const VecF32x4 mask_sign		= _mm_set1_ps(-0.0f);
	const Vec128 c_f16max			= _mm_set1_epi32((127 + 16) << 23); // all FP32 values >=this round to +inf
	const Vec128 c_nanbit			= _mm_set1_epi32(0x200);
	const Vec128 c_nanlobits        = _mm_set1_epi32(0x1ff);
	const Vec128 c_infty_as_fp16	= _mm_set1_epi32(0x7c00);
	const Vec128 c_min_normal		= _mm_set1_epi32((127 - 14) << 23); // smallest FP32 that yields a normalized FP16
	const Vec128 c_subnorm_magic	= _mm_set1_epi32(((127 - 15) + (23 - 10) + 1) << 23);
	const Vec128 c_normal_bias		= _mm_set1_epi32(0xfff - ((127 - 15) << 23)); // adjust exponent and add mantissa rounding

	VecF32x4 justsign	= f & mask_sign;
	VecF32x4 absf		= f.andnot(mask_sign); // f & ~mask_sign
	Vec128 absf_int		= _mm_castps_si128(absf); // the cast is "free" (extra bypass latency, but no thruput hit)
	VecF32x4 b_isnan	= _mm_cmpunord_ps(absf, absf); // is this a NaN?
	Vec128 b_isregular	= _mm_cmpgt_epi32(c_f16max, absf_int); // (sub)normalized or special?
	Vec128 nan_payload  = _mm_and_si128(_mm_srli_epi32(absf_int, 13), c_nanlobits); // payload bits for NaNs
	Vec128 nan_quiet    = _mm_or_si128(nan_payload, c_nanbit); // and set quiet bit
	Vec128 nanfinal		= _mm_and_si128(_mm_castps_si128(b_isnan), nan_quiet);
	Vec128 inf_or_nan	= _mm_or_si128(nanfinal, c_infty_as_fp16); // output for specials

	Vec128 b_issub		= _mm_cmpgt_epi32(c_min_normal, absf_int);

	// "result is subnormal" path
	VecF32x4 subnorm1	= absf + VecF32x4(_mm_castsi128_ps(c_subnorm_magic)); // magic value to round output mantissa
	Vec128 subnorm2		= _mm_sub_epi32(_mm_castps_si128(subnorm1), c_subnorm_magic); // subtract out bias

	// "result is normal" path
	Vec128 mantoddbit	= _mm_slli_epi32(absf_int, 31 - 13); // shift bit 13 (mantissa LSB) to sign
	Vec128 mantodd		= _mm_srai_epi32(mantoddbit, 31); // -1 if FP16 mantissa odd, else 0

	Vec128 round1		= _mm_add_epi32(absf_int, c_normal_bias);
	Vec128 round2		= _mm_sub_epi32(round1, mantodd); // if mantissa LSB odd, bias towards rounding up (RTNE)
	Vec128 normal		= _mm_srli_epi32(round2, 13); // rounded result

	// combine the two non-specials
	Vec128 nonspecial	= _mm_or_si128(_mm_and_si128(subnorm2, b_issub), _mm_andnot_si128(b_issub, normal));

	// merge in specials as well
	Vec128 joined		= _mm_or_si128(_mm_and_si128(nonspecial, b_isregular), _mm_andnot_si128(b_isregular, inf_or_nan));

	Vec128 sign_shift	= _mm_srai_epi32(_mm_castps_si128(justsign), 16);
	Vec128 result		= _mm_or_si128(joined, sign_shift);

	return result;
}

// Half->float conversion, SSE2+
// input in 32-bit lanes
static inline VecF32x4 F16_to_F32_4x(const Vec128 &h)
{
	const Vec128 mask_nosign	= _mm_set1_epi32(0x7fff);
	const VecF32x4 magic_mult	= _mm_castsi128_ps(_mm_set1_epi32((254 - 15) << 23));
	const Vec128 was_infnan		= _mm_set1_epi32(0x7bff);
	const VecF32x4 exp_infnan	= _mm_castsi128_ps(_mm_set1_epi32(255 << 23));
	const Vec128 was_nan   		= _mm_set1_epi32(0x7c00);
	const Vec128 nan_quiet		= _mm_set1_epi32(1 << 22);

	Vec128 expmant		= _mm_and_si128(mask_nosign, h);
	Vec128 justsign		= _mm_xor_si128(h, expmant);
	Vec128 shifted		= _mm_slli_epi32(expmant, 13);
	VecF32x4 scaled		= _mm_mul_ps(_mm_castsi128_ps(shifted), magic_mult);
	Vec128 b_wasinfnan  = _mm_cmpgt_epi32(expmant, was_infnan);
	Vec128 sign			= _mm_slli_epi32(justsign, 16);
	VecF32x4 infnanexp	= _mm_and_ps(_mm_castsi128_ps(b_wasinfnan), exp_infnan);
	Vec128 b_wasnan     = _mm_cmpgt_epi32(expmant, was_nan);
	Vec128 nanquiet		= _mm_and_si128(b_wasnan, nan_quiet);
	VecF32x4 infnandone = _mm_or_ps(infnanexp, _mm_castsi128_ps(nanquiet));

	VecF32x4 sign_inf	= _mm_or_ps(_mm_castsi128_ps(sign), infnandone);
	VecF32x4 result		= _mm_or_ps(scaled, sign_inf);

	return result;
}

static void rrFP16_to_FP32_4x(F32 * dst, const rrFP16 * src, SINTa count_divby4)
{
	for (SINTa i = 0; i < count_divby4; ++i)
	{
		Vec128 vals_fp16 = load64u(&src[i*4]);

		// unpack to 32b
		Vec128 vals_fp16_32 = zext16to32_lo(vals_fp16);

		// convert
		VecF32x4 vals_fp32 = F16_to_F32_4x(vals_fp16_32);

		// store
		vals_fp32.storeu(&dst[i*4]);
	}
}

static void rrFP32_to_FP16_4x(rrFP16 * dst, const F32 * src, SINTa count_divby4)
{
	for (SINTa i = 0; i < count_divby4; ++i)
	{
		VecF32x4 vals_fp32 = VecF32x4::loadu(&src[i*4]);

		// convert
		Vec128 vals_fp16_32 = F32_to_F16_4x(vals_fp32);

		// pack (works because it's sign extended)
		Vec128 vals_fp16 = _mm_packs_epi32(vals_fp16_32, vals_fp16_32);

		// store
		store64u(&dst[i*4], vals_fp16);
	}
}

#elif defined(DO_BUILD_NEON64)

#define HAVE_SIMD_CONVERSIONS

static void rrFP16_to_FP32_4x(F32 * dst, const rrFP16 * src, SINTa count_divby4)
{
	for (SINTa i = 0; i < count_divby4; ++i)
	{
		float16x4_t vals_fp16 = vld1_f16((float16_t *)&src[i*4]);
		float32x4_t vals_fp32 = vcvt_f32_f16(vals_fp16);
		vst1q_f32(&dst[i*4], vals_fp32);
	}
}

static void rrFP32_to_FP16_4x(rrFP16 * dst, const F32 * src, SINTa count_divby4)
{
	for (SINTa i = 0; i < count_divby4; ++i)
	{
		float32x4_t vals_fp32 = vld1q_f32(&src[i*4]);
		float16x4_t vals_fp16 = vcvt_f16_f32(vals_fp32);
		vst1_f16((float16_t*) &dst[i*4], vals_fp16);
	}
}

#endif

void rrFP16_to_FP32_Array(F32 * dst, const rrFP16 * src, SINTa count)
{
#ifdef HAVE_SIMD_CONVERSIONS
	rrFP16_to_FP32_4x(dst, src, count >> 2);
	if (SINTa rest = count & 3)
	{
		rrFP16 src_tail[4] = {}; // zero-init
		F32 dst_tail[4];

		memcpy(src_tail, &src[count - rest], rest * sizeof(*src));
		rrFP16_to_FP32_4x(dst_tail, src_tail, 1);
		memcpy(&dst[count - rest], dst_tail, rest * sizeof(*dst));
	}

#else
	for (SINTa i = 0; i < count; ++i)
		dst[i] = rrFP16_GetFloat(&src[i]);
#endif
}

void rrFP32_to_FP16_Array(rrFP16 * dst, const F32 * src, SINTa count)
{
#ifdef HAVE_SIMD_CONVERSIONS
	rrFP32_to_FP16_4x(dst, src, count >> 2);
	if (SINTa rest = count & 3)
	{
		F32 src_tail[4] = {}; // zero-init
		rrFP16 dst_tail[4];

		memcpy(src_tail, &src[count - rest], rest * sizeof(*src));
		rrFP32_to_FP16_4x(dst_tail, src_tail, 1);
		memcpy(&dst[count - rest], dst_tail, rest * sizeof(*dst));
	}
#else
	for (SINTa i = 0; i < count; ++i)
		rrFP16_SetFloat(&dst[i], src[i]);
#endif
}

RR_NAMESPACE_END

