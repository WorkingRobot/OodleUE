// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.


#include "rrsimd.h"
#include "vec128.inl"
#ifdef DO_BUILD_SSE4
#include <smmintrin.h>
#endif

RR_NAMESPACE_START

static RADFORCEINLINE F32 VQD(const rrColorBlock4x4 & colors,const rrColor32BGRA palette[4],const U32 in_indices,const FourFloatBlock4x4 & activity);

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

#ifdef DO_BUILD_SSE4

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

#endif // DO_BUILD_SSE4

// VQD optimized not easy to fiddle with
// VQD is just SSD with a scaling on each pixel
//  we can do the SIMD SSD like BC1_Palette_SSD_RGB
//	and just mul through by activity
static RADFORCEINLINE F32 VQD_BC1(const rrColorBlock4x4 & colors,const rrColor32BGRA palette[4],const U32 in_indices,const SingleFloatBlock4x4 & activity)
{
	F32 sum = 0;
	U32 indices = in_indices;
	
	#ifndef DO_BUILD_SSE4
	{
	
	// scalar fallback
	
	for LOOP(i,16)
	{
		const rrColor32BGRA dxtbc = palette[indices&3]; indices >>= 2;
			
		rrColor32BGRA c = colors.colors[i];
		
		#if 0 // 4F
		const rrColor4F & act = activity.values[i];
		F32 lsum;
		
		lsum  = act.r * fsquare((F32)((S32)c.u.r - dxtbc.u.r));
		lsum += act.g * fsquare((F32)((S32)c.u.g - dxtbc.u.g));
		lsum += act.b * fsquare((F32)((S32)c.u.b - dxtbc.u.b));
		lsum += act.a * fsquare((F32)((S32)c.u.a - dxtbc.u.a));

		sum += lsum;
		#endif
		
		// 1F :
		const F32 act = activity.values[i];
		
		sum += act * rrColor32BGRA_DeltaSqrRGBA(c,dxtbc);
	}
	
	}
	#else
	{
	
	// SSE4
	
	Vec128 pal_vec = load128u(palette);

	// Make 4 pre-shifted versions of the index
	__m128i index_vec = _mm_set1_epi32(indices);

	// Shift the index within each byte by 0,2,4,6 bits; we can use a 16-bit multiply for this.
	// (This is a bit counter-intuitive, but note that the bits we want always start up in the
	// byte they end up in, so we needn't worry about what happens across 16b lane boundaries)
	index_vec = _mm_mullo_epi16(index_vec, _mm_setr_epi16(1<<6,1<<6, 1<<4,1<<4, 1<<2,1<<2, 1<<0,1<<0));

	// Now shift the value down from bit 6 of every byte to bit 2 of every byte, and finally mask it,
	// so we have our 2-bit indices in bits [3:2] of every byte
	index_vec = _mm_and_si128(_mm_srli_epi16(index_vec, 4), _mm_set1_epi8(0x0c));
	
	VecF32x4 accum_f32 = VecF32x4::zero();

	for(int r=0;r<4;r++)
	{
		const rrColor32BGRA * row = colors.colors+r*4;
		
		// load 4 colors :		
		Vec128 v1 = load128u(row);
		
		// make 4 copies of each index byte
		Vec128 index_broadcast = _mm_shuffle_epi8(index_vec, _mm_setr_epi8(0,0,0,0, 4,4,4,4, 8,8,8,8, 12,12,12,12));

		// or in RGBA byte index :
		Vec128 pal_index = _mm_or_si128(index_broadcast, _mm_set1_epi32(0x03020100));
		// then select the bytes via shuffle
		Vec128 v2 = _mm_shuffle_epi8(pal_vec,pal_index);

		// indexes >>= 8 for next iter
		index_vec = _mm_srli_epi32(index_vec,8);

		#if 1 // activity 1F

		accum_f32 += VQD_SSE4_Diff_4U8_Activity1F(v1, v2, activity.values + r*4);

		#endif

		#if 0 // activity 4F

		accum_f32 += VQD_SSE4_Diff_4U8_Activity4F(v1, v2, activity.values + r*4);

		#endif
	}
	
	// horizontal sum across the lanes of accum_f32 :
	F32 sum_sse = accum_f32.sum_across_inner_outer().scalar_x();

	//RR_ASSERT_ALWAYS( ssd == ssd_ref );
	//rrprintfvar(sum);
	//rrprintfvar(sum_sse);
	//RR_ASSERT( fequal(sum,sum_sse,0.55f) );

	sum = sum_sse;

	}
	#endif
	
	return sum;
}

// colors-colors variant of VQD :
//	!! BEWARE !! LOTS OF CODE DUPE WITH ABOVE PRIMARY VARIANT
// does RGBA SSD
//	rrColorBlock4x4 is BGRA but that doesn't actually matter , RGBA will give the same result
static RADFORCEINLINE F32 VQD(const rrColorBlock4x4 & colors1,const rrColorBlock4x4 & colors2,const FourFloatBlock4x4 & activity)
{
	F32 sum = 0;
	
	#ifndef DO_BUILD_SSE4
	{
	
	// scalar fallback
	
	for LOOP(i,16)
	{
		rrColor32BGRA c1 = colors1.colors[i];
		rrColor32BGRA c2 = colors2.colors[i];
		const rrColor4F & act = activity.values[i];

		F32 lsum;

		lsum  = act.r * fsquare((F32)((S32)c1.u.r - c2.u.r));
		lsum += act.g * fsquare((F32)((S32)c1.u.g - c2.u.g));
		lsum += act.b * fsquare((F32)((S32)c1.u.b - c2.u.b));
		lsum += act.a * fsquare((F32)((S32)c1.u.a - c2.u.a));
		
		sum += lsum;
	}
	
	}
	#else
	{
	// SSE4
	
	VecF32x4 accum_f32 = VecF32x4::zero();

	for(int r=0;r<4;r++)
	{
		const rrColor32BGRA * row1 = colors1.colors+r*4;
		const rrColor32BGRA * row2 = colors2.colors+r*4;
		const rrColor4F * activity_row = activity.values + r*4;
		
		// load 4 colors :		
		__m128i v1 = _mm_loadu_si128((const __m128i *)row1);
		__m128i v2 = _mm_loadu_si128((const __m128i *)row2);
				
		accum_f32 += VQD_SSE4_Diff_4U8_Activity4F(v1, v2, activity_row);
	}
	
	// horizontal sum across the lanes of accum_f32 :
	F32 sum_sse = accum_f32.sum_across_inner_outer().scalar_x();

	//RR_ASSERT_ALWAYS( ssd == ssd_ref );
	//rrprintfvar(sum);
	//rrprintfvar(sum_sse);
	//RR_ASSERT( fequal(sum,sum_sse,0.55f) );

	sum = sum_sse;

	}
	#endif
	
	return sum;
}

// colors-colors variant of VQD :
//	!! BEWARE !! LOTS OF CODE DUPE WITH ABOVE PRIMARY VARIANT
// does RGBA SSD
//	rrColorBlock4x4 is BGRA but that doesn't actually matter , RGBA will give the same result
static RADFORCEINLINE F32 VQD(const rrColorBlock4x4 & colors1,const rrColorBlock4x4 & colors2,const SingleFloatBlock4x4 & activity)
{
	F32 sum = 0;
	
	#ifndef DO_BUILD_SSE4
	{
	
	// scalar fallback
	
	for LOOP(i,16)
	{
		rrColor32BGRA c1 = colors1.colors[i];
		rrColor32BGRA c2 = colors2.colors[i];
		const F32 act = activity.values[i];
		
		sum += act * rrColor32BGRA_DeltaSqrRGBA(c1,c2);
	}
	
	}
	#else
	{
	
	// SSE2 or 4
		
	VecF32x4 accum_f32 = VecF32x4::zero();

	for(int r=0;r<4;r++)
	{
		const rrColor32BGRA * row1 = colors1.colors+r*4;
		const rrColor32BGRA * row2 = colors2.colors+r*4;
		const F32 * activity_row = activity.values + r*4;
		
		// load 4 colors :		
		__m128i v1 = _mm_loadu_si128((const __m128i *)row1);
		__m128i v2 = _mm_loadu_si128((const __m128i *)row2);

		accum_f32 += VQD_SSE4_Diff_4U8_Activity1F(v1, v2, activity_row);
	}
	
	// horizontal sum across the lanes of accum_f32 :
	F32 sum_sse = accum_f32.sum_across_inner_outer().scalar_x();
    
	//RR_ASSERT_ALWAYS( ssd == ssd_ref );
	//rrprintfvar(sum);
	//rrprintfvar(sum_sse);
	//RR_ASSERT( fequal(sum,sum_sse,0.55f) );

	sum = sum_sse;

	}
	#endif
	
	return sum;
}

// For S16 pixels _that don't use the whole range_ (so we can subtract them w/o overflows)
static RADFORCEINLINE F32 VQD(const S16 colors1[16],const S16 colors2[16],const SingleFloatBlock4x4 & activity)
{
	F32 sum = 0;

	#ifndef __RADSSE2__

	// scalar fallback

	for LOOP(i,16)
	{
		int diff = colors1[i] - colors2[i];
		// assumes PreprocessActivity has been done
		F32 scale = activity.values[i];

		sum += (diff * diff) * scale;
	}

	#else

	// SSE2 or 4

	// load all 16 pixels and diff them
	Vec128 diff01_16 = _mm_sub_epi16(load128u(colors1 + 0), load128u(colors2 + 0));
	Vec128 diff23_16 = _mm_sub_epi16(load128u(colors1 + 8), load128u(colors2 + 8));

	// Sign-extend to 32-bit float
	VecF32x4 diff0_f32 = VecF32x4::from_int32(sext16to32_lo(diff01_16));
	VecF32x4 diff1_f32 = VecF32x4::from_int32(sext16to32_hi(diff01_16));
	VecF32x4 diff2_f32 = VecF32x4::from_int32(sext16to32_lo(diff23_16));
	VecF32x4 diff3_f32 = VecF32x4::from_int32(sext16to32_hi(diff23_16));

	// Square and multiply by activity values
	VecF32x4 ssd0_f32 = (diff0_f32 * diff0_f32) * VecF32x4::loadu(activity.values + 0);
	VecF32x4 ssd1_f32 = (diff1_f32 * diff1_f32) * VecF32x4::loadu(activity.values + 4);
	VecF32x4 ssd2_f32 = (diff2_f32 * diff2_f32) * VecF32x4::loadu(activity.values + 8);
	VecF32x4 ssd3_f32 = (diff3_f32 * diff3_f32) * VecF32x4::loadu(activity.values + 12);

	// Sum rows
	VecF32x4 sum01_f32 = ssd0_f32 + ssd1_f32;
	VecF32x4 sum23_f32 = ssd2_f32 + ssd3_f32;
	VecF32x4 sum_f32 = sum01_f32 + sum23_f32;

	sum = sum_f32.sum_across().scalar_x();

	#endif

	return sum;
}

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
