// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrsurfaceblitfastimpl.h"
#include "rrsurfaceblit.h"
#include "rrcolor.h"
#include "rrpixelformat.h"
#include "vec128.inl"
//#include "rrsimpleprof.h"

RR_NAMESPACE_START

//=======================================================================

namespace // local linkage
{

static rrbool rrSurface_Blit_4U8_SwapRB( rrSurface * to, const rrSurface * fm )
{
	int w = RR_MIN( fm->width  , to->width );
	int h = RR_MIN( fm->height , to->height );

	for(int y=0;y<h;y++)
	{
		const U32 * fmPtr = (const U32 *)(fm->data + fm->stride * y);
		U32 * toPtr = (U32 *)(to->data + to->stride * y);

		for(int x=0;x<w;x++)
		{
			U32 t = RR_GET32_LE_UNALIGNED(&fmPtr[x]);
			t = ((t & 0xFF) << 16) | ((t >> 16) & 0xFF) | (t & 0xFF00FF00);
			RR_PUT32_LE_UNALIGNED(&toPtr[x],t);
		}
	}

	return true;
}

static rrbool rrSurface_Blit_4U8_A255( rrSurface * to, const rrSurface * fm )
{
	int w = RR_MIN( fm->width  , to->width );
	int h = RR_MIN( fm->height , to->height );

	for(int y=0;y<h;y++)
	{
		const U32 * fmPtr = (const U32 *)(fm->data + fm->stride * y);
		U32 * toPtr = (U32 *)(to->data + to->stride * y);

		for(int x=0;x<w;x++)
		{
			U32 t = RR_GET32_LE_UNALIGNED(&fmPtr[x]);
			t |= 0xFF000000;
			RR_PUT32_LE_UNALIGNED(&toPtr[x],t);
		}
	}

	return true;
}

static rrbool rrSurface_Blit_4U8_A255_SwapRB( rrSurface * to, const rrSurface * fm )
{
	int w = RR_MIN( fm->width  , to->width );
	int h = RR_MIN( fm->height , to->height );

	for(int y=0;y<h;y++)
	{
		const U32 * fmPtr = (const U32 *)(fm->data + fm->stride * y);
		U32 * toPtr = (U32 *)(to->data + to->stride * y);

		for(int x=0;x<w;x++)
		{
			U32 t = RR_GET32_LE_UNALIGNED(&fmPtr[x]);
			t = ((t & 0xFF) << 16) | ((t >> 16) & 0xFF) | (t & 0xFF00) | 0xFF000000;
			RR_PUT32_LE_UNALIGNED(&toPtr[x],t);
		}
	}

	return true;
}

static rrbool rrSurface_Blit_4F16_to_3F16( rrSurface * to, const rrSurface * fm )
{
	RR_ASSERT( fm->pixelFormat == rrPixelFormat_4_F16 );
	RR_ASSERT( to->pixelFormat == rrPixelFormat_3_F16 );

	int w = RR_MIN( fm->width  , to->width );
	int h = RR_MIN( fm->height , to->height );

	for(int y=0;y<h;y++)
	{
		const U8 * fmPtr = fm->data + fm->stride * y;
		U8 * toPtr = to->data + to->stride * y;

		for(int x=0;x<w;x++)
		{
			U64 t = RR_GET64_LE_UNALIGNED(fmPtr);
			RR_PUT32_LE_UNALIGNED(toPtr,(U32)t);
			RR_PUT16_LE_UNALIGNED(toPtr+4,(U16)(t >> 32));
			toPtr += 6;
			fmPtr += 8;
		}
	}

	return true;
}

static rrbool rrSurface_Blit_4F32_to_3F32( rrSurface * to, const rrSurface * fm )
{
	RR_ASSERT( fm->pixelFormat == rrPixelFormat_4_F32 );
	RR_ASSERT( to->pixelFormat == rrPixelFormat_3_F32 );

	int w = RR_MIN( fm->width  , to->width );
	int h = RR_MIN( fm->height , to->height );

	for(int y=0;y<h;y++)
	{
		const U8 * fmPtr = fm->data + fm->stride * y;
		U8 * toPtr = to->data + to->stride * y;

		for(int x=0;x<w;x++)
		{
			U64 t0 = RR_GET64_NATIVE_UNALIGNED(fmPtr);
			U32 t1 = RR_GET32_NATIVE_UNALIGNED(fmPtr + 8);
			RR_PUT64_NATIVE_UNALIGNED(toPtr,t0);
			RR_PUT32_NATIVE_UNALIGNED(toPtr+8,t1);
			toPtr += 12;
			fmPtr += 16;
		}
	}

	return true;
}

static rrbool rrSurface_Blit_3F32_to_4F32( rrSurface * to, const rrSurface * fm )
{
	RR_ASSERT( fm->pixelFormat == rrPixelFormat_3_F32 );
	RR_ASSERT( to->pixelFormat == rrPixelFormat_4_F32 );

	int w = RR_MIN( fm->width  , to->width );
	int h = RR_MIN( fm->height , to->height );
	F32 alphaConst = 255.0f; // value the alpha channel gets filled with

	for(int y=0;y<h;y++)
	{
		const U8 * fmPtr = fm->data + fm->stride * y;
		U8 * toPtr = to->data + to->stride * y;

		for(int x=0;x<w;x++)
		{
			U64 t0 = RR_GET64_NATIVE_UNALIGNED(fmPtr);
			U32 t1 = RR_GET32_NATIVE_UNALIGNED(fmPtr + 8);
			RR_PUT64_NATIVE_UNALIGNED(toPtr,t0);
			RR_PUT32_NATIVE_UNALIGNED(toPtr+8,t1);
			memcpy(toPtr + 12, &alphaConst, sizeof(alphaConst));
			toPtr += 16;
			fmPtr += 12;
		}
	}

	return true;
}

#ifdef __RADX86__

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

#endif


#if defined(__RADX86__) || defined(DO_BUILD_NEON64)

static rrbool rrSurface_Blit_4F16_to_4F32( rrSurface * to, const rrSurface * fm )
{
	RR_ASSERT( fm->pixelFormat == rrPixelFormat_4_F16 );
	RR_ASSERT( to->pixelFormat == rrPixelFormat_4_F32 );

	int w = RR_MIN( fm->width  , to->width );
	int h = RR_MIN( fm->height , to->height );

	for(int y=0;y<h;y++)
	{
		const U8 * fmPtr = fm->data + fm->stride * y;
		U8 * toPtr = to->data + to->stride * y;

		for(int x=0;x<w;x++)
		{
#if defined(__RADX86__)
			Vec128 rgba_half16 = load64u(fmPtr);

			// unpack to 32b
			Vec128 rgba_half32 = zext16to32_lo(rgba_half16);

			// convert
			VecF32x4 rgba_float32 = F16_to_F32_4x(rgba_half32);

			// store
			rgba_float32.storeu((float*)toPtr);

#else // defined(DO_BUILD_NEON64)

			float16x4_t rgba_half16 = vld1_f16((float16_t*)fmPtr);
			float32x4_t rgba_float32 = vcvt_f32_f16(rgba_half16);
			vst1q_f32((float*)toPtr, rgba_float32);
#endif
			toPtr += 16;
			fmPtr += 8;
		}
	}

	return true;
}

static rrbool rrSurface_Blit_4F16_to_3F32( rrSurface * to, const rrSurface * fm )
{
	RR_ASSERT( fm->pixelFormat == rrPixelFormat_4_F16 );
	RR_ASSERT( to->pixelFormat == rrPixelFormat_3_F32 );

	int w = RR_MIN( fm->width  , to->width );
	int h = RR_MIN( fm->height , to->height );

	for(int y=0;y<h;y++)
	{
		const U8 * fmPtr = fm->data + fm->stride * y;
		U8 * toPtr = to->data + to->stride * y;

		for(int x=0;x<w;x++)
		{
#if defined(__RADX86__)
			Vec128 rgba_half16 = load64u(fmPtr);

			// unpack to 32b
			Vec128 rgba_half32 = zext16to32_lo(rgba_half16);

			// convert
			VecF32x4 rgba_float32 = F16_to_F32_4x(rgba_half32);

			// store
			Vec128 bits = _mm_castps_si128(rgba_float32);
			store64u(toPtr, bits);
			store32u(toPtr + 8, _mm_srli_si128(bits, 8));

#else // defined(DO_BUILD_NEON64)

			float16x4_t rgba_half16 = vld1_f16((float16_t*)fmPtr);
			float32x4_t rgba_float32 = vcvt_f32_f16(rgba_half16);
			vst1q_lane_u64((uint64_t*)toPtr, vreinterpretq_u64_f32(rgba_float32), 0);
			vst1q_lane_f32((float*)(toPtr + 8), rgba_float32, 2);
#endif
			toPtr += 12;
			fmPtr += 8;
		}
	}

	return true;
}

static rrbool rrSurface_Blit_4F32_to_4F16( rrSurface * to, const rrSurface * fm )
{
	RR_ASSERT( fm->pixelFormat == rrPixelFormat_4_F32 );
	RR_ASSERT( to->pixelFormat == rrPixelFormat_4_F16 );

	int w = RR_MIN( fm->width  , to->width );
	int h = RR_MIN( fm->height , to->height );

	for(int y=0;y<h;y++)
	{
		const U8 * fmPtr = fm->data + fm->stride * y;
		U8 * toPtr = to->data + to->stride * y;

		for(int x=0;x<w;x++)
		{
#if defined(__RADX86__)
			Vec128 rgba = load128u(fmPtr);

			// convert
			Vec128 rgba_half32 = F32_to_F16_4x(_mm_castsi128_ps(rgba));

			// pack (works because it's sign extended)
			Vec128 rgba_half16 = _mm_packs_epi32(rgba_half32, rgba_half32);

			// store
			store64u(toPtr, rgba_half16);

#else // defined(DO_BUILD_NEON64)

			float32x4_t rgba_float32 = vld1q_f32((float*)fmPtr);
			float16x4_t rgba_half16 = vcvt_f16_f32(rgba_float32);
			vst1_f16((float16_t*)toPtr, rgba_half16);
#endif
			toPtr += 8;
			fmPtr += 16;
		}
	}

	return true;
}

static rrbool rrSurface_Blit_3F32_to_4F16( rrSurface * to, const rrSurface * fm )
{
	RR_ASSERT( fm->pixelFormat == rrPixelFormat_3_F32 );
	RR_ASSERT( to->pixelFormat == rrPixelFormat_4_F16 );

	int w = RR_MIN( fm->width  , to->width );
	int h = RR_MIN( fm->height , to->height );
	F32 alphaConst = 255.0f; // value the alpha channel gets filled with

#if defined(__RADX86__)
	Vec128 alphaVec = _mm_castps_si128(_mm_setr_ps(0.0f, alphaConst, 0.0f, 0.0f));
#else // defined(DO_BUILD_NEON64)
	float32x2_t alphaVec = { 0.0f, alphaConst };
#endif

	for(int y=0;y<h;y++)
	{
		const U8 * fmPtr = fm->data + fm->stride * y;
		U8 * toPtr = to->data + to->stride * y;

		for(int x=0;x<w;x++)
		{
#if defined(__RADX86__)
			// load pieces
			Vec128 rg = load64u(fmPtr);
			Vec128 b = load32u(fmPtr + 8);
			Vec128 ba = _mm_or_si128(b, alphaVec);

			// combine
			Vec128 rgba = _mm_unpacklo_epi64(rg, ba);

			// convert
			Vec128 rgba_half32 = F32_to_F16_4x(_mm_castsi128_ps(rgba));

			// pack (works because it's sign extended)
			Vec128 rgba_half16 = _mm_packs_epi32(rgba_half32, rgba_half32);

			// store
			store64u(toPtr, rgba_half16);

#else // defined(DO_BUILD_NEON64)

			float32x2_t rg = vld1_f32((float*)fmPtr);
			float32x2_t ba = vld1_lane_f32((float*)(fmPtr + 8), alphaVec, 0);
			float16x4_t rgba_half16 = vcvt_f16_f32(vcombine_f32(rg, ba));
			vst1_f16((float16_t*)toPtr, rgba_half16);
#endif
			toPtr += 8;
			fmPtr += 12;
		}
	}

	return true;
}

#endif // __RADX86__ || DO_BUILD_NEON64

};

void rrSurfaceBlitFastImpl_Install()
{
	// Blits between R8G8B8A8 <-> B8G8R8A8 (in both directions) switch R and B
	rrSurface_RegisterBlitter(rrPixelFormat_B8G8R8A8, rrPixelFormat_R8G8B8A8, rrSurface_Blit_4U8_SwapRB);
	rrSurface_RegisterBlitter(rrPixelFormat_R8G8B8A8, rrPixelFormat_B8G8R8A8, rrSurface_Blit_4U8_SwapRB);
	rrSurface_RegisterBlitter(rrPixelFormat_B8G8R8A8, rrPixelFormat_R8G8B8A8, rrSurface_Blit_4U8_SwapRB);
	rrSurface_RegisterBlitter(rrPixelFormat_R8G8B8A8, rrPixelFormat_B8G8R8A8, rrSurface_Blit_4U8_SwapRB);

	// Blits between R8G8B8A8 <-> R8G8B8x8 (in both directions) just set the alpha channel byte to 255
	rrSurface_RegisterBlitter(rrPixelFormat_B8G8R8A8, rrPixelFormat_B8G8R8x8, rrSurface_Blit_4U8_A255);
	rrSurface_RegisterBlitter(rrPixelFormat_B8G8R8x8, rrPixelFormat_B8G8R8A8, rrSurface_Blit_4U8_A255);
	rrSurface_RegisterBlitter(rrPixelFormat_R8G8B8A8, rrPixelFormat_R8G8B8x8, rrSurface_Blit_4U8_A255);
	rrSurface_RegisterBlitter(rrPixelFormat_R8G8B8x8, rrPixelFormat_R8G8B8A8, rrSurface_Blit_4U8_A255);

	// Blits between R8G8B8A8 <-> B8G8R8x8 (in both directions) switch R and B and set the alpha channel byte to 255
	rrSurface_RegisterBlitter(rrPixelFormat_B8G8R8A8, rrPixelFormat_R8G8B8x8, rrSurface_Blit_4U8_A255_SwapRB);
	rrSurface_RegisterBlitter(rrPixelFormat_R8G8B8x8, rrPixelFormat_B8G8R8A8, rrSurface_Blit_4U8_A255_SwapRB);
	rrSurface_RegisterBlitter(rrPixelFormat_B8G8R8x8, rrPixelFormat_R8G8B8A8, rrSurface_Blit_4U8_A255_SwapRB);
	rrSurface_RegisterBlitter(rrPixelFormat_R8G8B8A8, rrPixelFormat_B8G8R8x8, rrSurface_Blit_4U8_A255_SwapRB);

	rrSurface_RegisterBlitter(rrPixelFormat_4_F16, rrPixelFormat_3_F16, rrSurface_Blit_4F16_to_3F16);
	rrSurface_RegisterBlitter(rrPixelFormat_4_F32, rrPixelFormat_3_F32, rrSurface_Blit_4F32_to_3F32);
	rrSurface_RegisterBlitter(rrPixelFormat_3_F32, rrPixelFormat_4_F32, rrSurface_Blit_3F32_to_4F32);

#if defined(__RADX86__) || defined(DO_BUILD_NEON64)
	rrSurface_RegisterBlitter(rrPixelFormat_4_F16, rrPixelFormat_4_F32, rrSurface_Blit_4F16_to_4F32);
	rrSurface_RegisterBlitter(rrPixelFormat_4_F16, rrPixelFormat_3_F32, rrSurface_Blit_4F16_to_3F32);
	rrSurface_RegisterBlitter(rrPixelFormat_4_F32, rrPixelFormat_4_F16, rrSurface_Blit_4F32_to_4F16);
	rrSurface_RegisterBlitter(rrPixelFormat_3_F32, rrPixelFormat_4_F16, rrSurface_Blit_3F32_to_4F16);
#endif
}


RR_NAMESPACE_END
