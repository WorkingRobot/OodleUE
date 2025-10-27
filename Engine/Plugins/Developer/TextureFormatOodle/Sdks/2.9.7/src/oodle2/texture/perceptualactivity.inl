// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.


#include "rrsimd.h"
#include "vec128.inl"

RR_NAMESPACE_START

#if 0

// straight SSD error, no activity mask
// go back to this if you want to tune for RMSE :
	
static U32 VQD(const rrColorBlock4x4 & colors,const rrDXT1Block & dxtb,const FourFloatBlock4x4 & activity,rrDXT1PaletteMode pal_mode)
{
	//U32 ssd = DXT1_ComputeSSD_RGB(colors,dxtb,pal_mode);
	U32 ssd = DXT1_ComputeSSD_RGBA(colors,dxtb,pal_mode);
	U32 D = 2 * ssd;
	return D;
}	

#else
	
// VQD you can fiddle with and try different ideas :	
static F32 VQD_Research(const rrColorBlock4x4 & colors,const rrColor32BGRA palette[4],const U32 in_indices,const FourFloatBlock4x4 & activity)
{
	F32 sum = 0;
	U32 indices = in_indices;
	
	/*
	rrColor4I src_dc = { };
	rrColor4I dst_dc = { };
	F32 activity_sum = 0;
	*/
	
	for LOOP(i,16)
	{	
		const rrColor32BGRA dxtbc = palette[indices&3]; indices >>= 2;
	
		/*
		rrColor4I_Add(&src_dc,colors.colors[i]);
		rrColor4I_Add(&dst_dc,dxtbc);
		activity_sum += activity.values[i];
		*/
		
		rrColor32BGRA c = colors.colors[i];
		const rrColor4F & act = activity.values[i];
		
		sum += act.r * fsquare((F32)((S32)c.u.r - dxtbc.u.r));
		sum += act.g * fsquare((F32)((S32)c.u.g - dxtbc.u.g));
		sum += act.b * fsquare((F32)((S32)c.u.b - dxtbc.u.b));
		sum += act.a * fsquare((F32)((S32)c.u.a - dxtbc.u.a));
			
		#if 0
		// rdotestset1_activity_masked_second_cut_Wed_Aug_21_17_17_28_2019
		
		U32 ssd = rrColor32BGRA_DeltaSqrRGB( colors.colors[i] , dxtbc );
		
		// assumes PreprocessActivity has been done
		F32 scale = activity.values[i];
		
		sum += ssd * scale;
		#endif
		
		#if 0
		
		// wants raw activity, NOT PreprocessActivity
		
		// rdotestset1_thresholded_sad_squared_Thu_Aug_22_10_31_55_2019
		
		// -> maybe this is slightly perceptually better than SSD version above
		
		// other option I like is SAD with activity as a soft thresh
		//	this looks good perceptually also
		//	this is obviously much worse in RMSE because you want SSD to optimize for RMSE (this could be a red herring)
		//	I believe that I see some anomalies from this (without the squaring at the end)
		//	basically the SAD linear error occasionally lets through individual big pixel changes
		//	 that are quite visible perceptually
		//	SSD does a better job of strongly penalizing big changes
		
		// Thresholding SAD here isn't quite right
		//	in PSNR-HVS-M you threshold only *ACs* not *DCs*
		//	I could fix that by adding an extra error term for DC delta
		//	(now added below)
		
		U32 sad = rrColor32BGRA_DeltaSADRGB( colors.colors[i] , dxtbc );
		
		F32 x = sad * 6.0; // scale sad vs activity ; x is compared to T
		
		// sad & activity are both linear in pixel values
		
		F32 a = activity.values[i];
		F32 T = RR_MIN(a,32.0); // clamp huge activities
		
		F32 K = 1.f; // tweak
		T += K;
		// add K to T, so if T is zero the map is the identity
		
		// sad in [0,T] -> [0,K]
		//	>= T -> K + more
		
		F32 y;
		if ( x < T )
		{
			y = x * K / T;
		}
		else
		{
			y = K + (x - T);
		}	
		
		// scaling for J :
		//y *= 8.0;
		
		// try squaring to increase weighting of large errors
		// AND need a divisor by activity as well
		//	use a weaker activity divisor here because we already thresholded
		//sum += y * y / (1 + a);
		y = y * y / (1 + T);
		y *= 0.7; // scaling for J
		
		sum += y;
		
		#endif
	}
	
	#if 0
	// @@ add another term for DC delta to prevent overall color shift
	//	DC delta should be activity masked but not as strongly
	//	and definitely not thresholded
	//	(maybe log(activity) ?)
	// I can't really see much perceptual benefit from this, but it does help RMSE score a lot
	//	(if used with sad-squared; only helps a little with ssd)
	//	so that's a win
	{
	// src_dc are *16 of average color
	U32 dc_ssd = rrColor4I_DeltaSqrRGB(src_dc,dst_dc);
	// @@ lots of constants and possibilities here
	F32 dc_activity = rrlog2(1.0 + activity_sum); // @@ log or linear in activity ?
	F32 dc_term = dc_ssd / (1.0 + dc_activity);
	sum += dc_term;
	}
	#endif
	
	//U32 D = (U32)(sum + 0.5);
		
	return sum;
}

// Helper for scalar accumulation
struct VQDInternalAccum
{
	F32 sums[4];

	VQDInternalAccum()
	{
		for LOOP(i,4)
			sums[i] = 0.0f;
	}

	RADFORCEINLINE void activity1f(const rrColor32BGRA c0, const rrColor32BGRA c1, F32 act, S32 lane)
	{
		sums[lane] += act * (F32)rrColor32BGRA_DeltaSqrRGBA(c0,c1);
	}

	RADFORCEINLINE void activity4f(const rrColor32BGRA c0, const rrColor32BGRA c1, const rrColor4F & act)
	{
		sums[0] += act.r * fsquare((F32)((S32)c0.u.r - c1.u.r));
		sums[1] += act.g * fsquare((F32)((S32)c0.u.g - c1.u.g));
		sums[2] += act.b * fsquare((F32)((S32)c0.u.b - c1.u.b));
		sums[3] += act.a * fsquare((F32)((S32)c0.u.a - c1.u.a));
	}

	F32 result() const
	{
		// final sum in same order as SIMD code
		return (sums[0] + sums[1]) + (sums[2] + sums[3]);
	}
};

#if defined(DO_BUILD_SSE4)

// Compute difference between U8 RGBA pixels and weight with activity
static RADFORCEINLINE VecF32x4 VQD_SSE4_Diff_4U8_Activity1F(const Vec128 & v1, const Vec128 & v2, const float * activity_row )
{
	Vec128 sub8 = _mm_or_si128( _mm_subs_epu8(v1,v2), _mm_subs_epu8(v2,v1) );
	Vec128 sub16_1 = _mm_and_si128(sub8, _mm_set1_epi16(0xff)); // 16-bit: R, B, R, B, ...
	Vec128 sub16_2 = _mm_srli_epi16(sub8, 8); // 16-bit: G, A, G, A, ...

	// this squares and horizontally adds pairs
	//	we go from 16 bits * 4 colors channels * 4 pixels
	//	-> 32 bits with {R+B} (squares32_1), {G+A} (squares32_2)
	Vec128 squares32_1 = _mm_madd_epi16(sub16_1, sub16_1);
	Vec128 squares32_2 = _mm_madd_epi16(sub16_2, sub16_2);

	// add the halves together to get R+G+B+A
	Vec128 squares32 = _mm_add_epi32(squares32_1, squares32_2);

	// convert to float
	VecF32x4 squares_f32 = VecF32x4::from_int32(squares32);

	// weight
	VecF32x4 activity = VecF32x4::loadu(activity_row);
	return squares_f32 * activity;
}

// Compute difference between U8 RGBA pixels and weight with activity
static RADFORCEINLINE VecF32x4 VQD_SSE4_Diff_4U8_Activity4F(const Vec128 & v1, const Vec128 & v2, const rrColor4F * activity_row )
{
	Vec128 plus_minus = _mm_set1_epi16(0x1FF);
	Vec128 sub16_1 = _mm_maddubs_epi16(_mm_unpacklo_epi8(v1, v2), plus_minus);
	Vec128 sub16_2 = _mm_maddubs_epi16(_mm_unpackhi_epi8(v1, v2), plus_minus);

	// RGBA squares , in 16 bit = 4*4 values in two vectors
	sub16_1 = _mm_mullo_epi16(sub16_1,sub16_1);
	sub16_2 = _mm_mullo_epi16(sub16_2,sub16_2);

	VecF32x4 sum_f32;

	// expand 16 to 32 and convert to floats :
	// NOTE *DO NOT* use fmadd here even if we add variants on targets that support FMA (AVX2+)!
	// We need to be able to guarantee binary identical results between FMA and non-FMA targets!
	sum_f32  = VecF32x4::from_int32( zext16to32_lo(sub16_1) ) * VecF32x4::loadu((const float *)(activity_row+0));
	sum_f32 += VecF32x4::from_int32( zext16to32_hi(sub16_1) ) * VecF32x4::loadu((const float *)(activity_row+1));
	sum_f32 += VecF32x4::from_int32( zext16to32_lo(sub16_2) ) * VecF32x4::loadu((const float *)(activity_row+2));
	sum_f32 += VecF32x4::from_int32( zext16to32_hi(sub16_2) ) * VecF32x4::loadu((const float *)(activity_row+3));

	return sum_f32;
}

#elif defined(DO_BUILD_NEON64)

// Compute difference between U8 RGBA pixels and weight with activity
static RADFORCEINLINE VecF32x4 VQD_NEON_Diff_4U8_Activity1F(const Vec128_U8 & v1, const Vec128_U8 & v2, const float * activity_row )
{
	// Start with absolute differences
	Vec128_U8 diff = abs_diff(v1, v2);

	// four lanes with db*db + dg*dg + dr*dr + da*da value for four pixels
	Vec128_U32 squares32 = Vec128_U32::sqr_sum(diff);

	// convert to float then weight
	VecF32x4 squares_f32 = VecF32x4::from_int32(squares32);
	VecF32x4 activity = VecF32x4::loadu(activity_row);
	return squares_f32 * activity;
}

// Compute difference between U8 RGBA pixels and weight with activity
static RADFORCEINLINE VecF32x4 VQD_NEON_Diff_4U8_Activity4F(const Vec128_U8 & v1, const Vec128_U8 & v2, const rrColor4F * activity_row )
{
	// Start with absolute differences
	Vec128_U8 diff = abs_diff(v1, v2);

	// Square 8-bit values to 16 bits
	Vec128_U16 sq01 = sqr16_lo(diff);
	Vec128_U16 sq23 = sqr16_hi(diff);

	VecF32x4 sum_f32;

	// expand 16 to 32 and convert to floats:
	// NOTE *DO NOT* use fma here since we need to be able to guarantee binary identical results between FMA and non-FMA targets!
	sum_f32  = VecF32x4::from_int32(Vec128_U32(vmovl_u16(vget_low_u16 (sq01)))) * VecF32x4::loadu(activity_row+0);
	sum_f32 += VecF32x4::from_int32(Vec128_U32(vmovl_u16(vget_high_u16(sq01)))) * VecF32x4::loadu(activity_row+1);
	sum_f32 += VecF32x4::from_int32(Vec128_U32(vmovl_u16(vget_low_u16 (sq23)))) * VecF32x4::loadu(activity_row+2);
	sum_f32 += VecF32x4::from_int32(Vec128_U32(vmovl_u16(vget_high_u16(sq23)))) * VecF32x4::loadu(activity_row+3);

	return sum_f32;
}

#endif // SSE4, NEON variants

#endif // VQD

/*
// ! make sure you turn off threading when accumulating totals
double g_total_vqd = 0;
double g_total_ssd = 0;
double g_total_sad = 0;

void log_total_vqd()
{
	F32 reference = 2 * g_total_ssd;
	F32 total_sad_scale = reference / g_total_sad;
	F32 total_vqd_scale = reference / g_total_vqd;
	rrprintfvar(total_sad_scale);
	rrprintfvar(total_vqd_scale);
}
*/

RR_NAMESPACE_END
