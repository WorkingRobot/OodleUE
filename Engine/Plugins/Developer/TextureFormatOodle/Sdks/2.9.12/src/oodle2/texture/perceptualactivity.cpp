// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetsse4

#include "perceptualactivity.h"
#include "perceptualactivity.inl"

#include "rrsurfaceutil.h"
#include "rrsurfaceblit.h"
#include "rrsurfacerowcache.h"
#include "rrsurfacefilters.h"

#include "templates/rralgorithm.h"
#include "rrvecc.h"

//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"

//#include "../texutil/rrsurfacebyname.h"
//#include "rrlogutil.h"

/**********

11-20-2019 :

fiddling with this activity scale is hard
you can't do it with the numerical scores

two specific things I'm seeing on "red_blue" that I'd like to fix :

1.

2.


@@@@ the constants in PreprocessActivity are really important

the clamp in PreprocessActivity actually gets hit a lot for high activity
median activity is ~ 50 and the clamp kicks in at 64

==============================================
some relevant notes from the todos:
==============================================

something I'm seeing perceptually :
	in blocks where a flat area meets a noisy area
	the flat part is getting too much error
	I think maybe this could be "activity" mask needs to be tighter
	too much error is being allowed in flats near edges

	the blur that makes activity isn't really what you want
		blur makes sharp edges bleed out activity into flats
	you want something that preserves all of the flat better
	maybe like a bilateral or a median filter kind of thing

	it's very obvious in "red_blue"
	as you dial up lambda
	the nasty looking errors first appear in the smooth areas
		where they abut a hard edge

	see "red_blue_perceptual_problem.bmp"

"red_blue" blockiness :
	VQD and vcdiff are still under-counting that error
	at lambda = 20, the image shouldn't be so blocky yet
	the issue is not the blockiness per se, that's an okay way to get that much rate reduction
	the issue is that we shouldn't be at so much rate reduction yet at lambda = 20
		it's going into low quality too fast
		due to the D in J being too low

	VQD of course doesn't see the block boundaries, it just sees smooth->flat
	vcdiff does see the global boundaries, but they're small steps in value
		maybe under threshold
		the CSF thresholds in vcdiff should get smaller if the region is low energy

=========

perceptual metric: "vcdiff" is under-counting the error on "red_blue"
	the smooth gradients turning blocky should count for more
	it takes a lot of error (lambda 160!) to get up to vcdiff +3 on that image

perceptual metric:
	activity-scaled vs. flat rmse
	could be on a slider instead of just a bool on/off
	then tech artists could play with that to tweak their results
		(people like fiddling)


===========

Improving vcdiff for smooth->blocky like red_blue :
	in areas that are quite flat (low AC everywhere)
	introduction of new AC energy is bad (eg. adding energy)
	generally high freq AC is not very visible
	but that's only if there's other energy to mask it
	just making high freq AC where there was nothing is bad


I like some kind of low-pass on "activity" to smooth it out and fill in holes
	but blur is just wrong
	if you did no low pass at all
	then in an area of total white noise static
	there would be some random spots of low "activity"
		because the average happened to match the current pixel value
	just doing median might be the fix

	beware any change to activity computation may affect VQD average value
	may have to re-scale it

========

maybe there's still some hacky thing I should do about over-flattening in images like "red_blue"
	don't let flats become too attractive
	there's a feedback cycle that makes them explode
	once you have a lot of flats, their codelen becomes very cheap, so you make more flats
	could just cap their codelen
	for codelen purposes, clamp "count" to a max of X
		(nblocks/256) or something like that

==============

	// now blur the activity to spread it around
-> this is crucial, it needs to remove random speckle
 but I don't like the exact way it's done
	
	// blur activity :
	// @@ rather than blurring activity here, something like median might be better
	//		mmm but not median , maybe bilateral?
	//  what I want to accomplish is two-fold :
	//	1. in areas of speckle, there will be isolated pixels where activity is near zero
	//		I want those to go away and get smeared out.  eg. [50,200,100,0,200,50] -> flat
	//		blur gets this right (so does median, bilateral does not)
	//  2. in areas where activity is all low, I want isolated peaks to not spread
	//		eg. if activity is [ 0 0 100 0 0 ] -> the 0's should stay 0
	//		but the 100 should stay 100
	//		eg. [ 0 0 100 0 0 ] -> [ 10 30 60 30 10 ] is bad because it's assigning activity to flat areas
	//		(median would remove the isolated 100, that's not right)
	//  3. next to hard edges, a smooth plateau on each side should stay activity zero (similar to case #2 above)
	//		(median gets the plateaus right, though it would remove the entire edge if it was too narrow)
	//
	
*********/


RR_NAMESPACE_START

void SingleFloatBlock4x4_Fill(SingleFloatBlock4x4 * pTo, const rrSurface & source, int x, int y)
{
	RR_ASSERT( source.pixelFormat == rrPixelFormat_1_F32 );
	
	float * pval = pTo->values;
	
	for LOOP(i,4)
	{
		const F32 * row = (const F32 *) rrSurface_SeekC(&source,x,y+i);
		memcpy(pval,row,4*sizeof(F32));
		pval += 4;
	}
}

void FourFloatBlock4x4_Fill(FourFloatBlock4x4 * pTo, const rrSurface & source, int x, int y)
{
	RR_ASSERT( source.pixelFormat == rrPixelFormat_4_F32 );
	
	rrColor4F * pval = pTo->values;
	
	for LOOP(i,4)
	{
		const rrColor4F * row = (const rrColor4F *) rrSurface_SeekC(&source,x,y+i);
		memcpy(pval,row,4*sizeof(rrColor4F));
		pval += 4;
	}
}

#if 0

static void MakeMedianFilter3x3_1F32(rrSurface * pTo_can_be_source,const rrSurface & source)
{
	RR_ASSERT_ALWAYS( source.pixelFormat == rrPixelFormat_1_F32 );

	int w = source.width;
	int h = source.height;

	rrSurfaceObj dest;
	rrSurface_Alloc(&dest,w,h,rrPixelFormat_1_F32);
	
	for LOOP(y,h)
	{
		F32 * to_row = (F32 *) rrSurface_Seek(&dest,0,y);
		const F32 * fm_row = (const F32 *) rrSurface_SeekC(&source,0,y);
	
		if ( y == 0 || y == h-1 )
		{
			for LOOP(x,w)
			{
				// @@ boundary?
				to_row[x] = fm_row[x];
			}
		}
		else
		{
			const F32 * fm_rowN = (const F32 *) ( (const U8 *)fm_row - source.stride );
			const F32 * fm_rowS = (const F32 *) ( (const U8 *)fm_row + source.stride );
	
			for LOOP(x,w)
			{
				if ( x == 0 || x == w-1 )
				{
					// @@ boundary?
					to_row[x] = fm_row[x];
				}
				else
				{
					F32 vals[9];
					vals[0] = fm_rowN[x-1];
					vals[1] = fm_rowN[x  ];
					vals[2] = fm_rowN[x+1];
					vals[3] = fm_row [x-1];
					vals[4] = fm_row [x  ];
					vals[5] = fm_row [x+1];
					vals[6] = fm_rowS[x-1];
					vals[7] = fm_rowS[x  ];
					vals[8] = fm_rowS[x+1];
					
					// @@ median quick select or something
					stdsort(vals,vals+9);
					to_row[x] = vals[4];
				}		
			}
		}
	}
	
	rrSurface_Swap(pTo_can_be_source,&dest);
}

static void MakeMedianFilter3x3_4F32(rrSurface * pTo_can_be_source,const rrSurface & source)
{
	RR_ASSERT_ALWAYS( source.pixelFormat == rrPixelFormat_4_F32 );

	int w = source.width;
	int h = source.height;

	// alloc to "dest"
	//	this allows pTo == &source
	rrSurfaceObj dest;
	rrSurface_Alloc(&dest,w,h,rrPixelFormat_4_F32);
	
	for LOOP(y,h)
	{
		rrVec4f * to_row = (rrVec4f *) rrSurface_Seek(&dest,0,y);
		const rrVec4f * fm_row = (const rrVec4f *) rrSurface_SeekC(&source,0,y);
	
		if ( y == 0 || y == h-1 )
		{
			for LOOP(x,w)
			{
				// @@ boundary?
				to_row[x] = fm_row[x];
			}
		}
		else
		{
			const rrVec4f * fm_rowN = (const rrVec4f *) ( (const U8 *)fm_row - source.stride );
			const rrVec4f * fm_rowS = (const rrVec4f *) ( (const U8 *)fm_row + source.stride );
	
			for LOOP(x,w)
			{
				if ( x == 0 || x == w-1 )
				{
					// @@ boundary?
					to_row[x] = fm_row[x];
				}
				else
				{
					rrVec4f vecs[9];
					vecs[0] = fm_rowN[x-1];
					vecs[1] = fm_rowN[x  ];
					vecs[2] = fm_rowN[x+1];
					vecs[3] = fm_row [x-1];
					vecs[4] = fm_row [x  ];
					vecs[5] = fm_row [x+1];
					vecs[6] = fm_rowS[x-1];
					vecs[7] = fm_rowS[x  ];
					vecs[8] = fm_rowS[x+1];
					
					rrVec4f out;
					
					for LOOP(c,4)
					{
						F32 vals[9];
						
						RR_UNROLL_I_9(0, vals[i] = vecs[i][c] );
					
						// @@ median quick select or something
						stdsort(vals,vals+9);
					
						out[c] = vals[4];	
					}
					
					to_row[x] = out;
				}		
			}
		}
	}
	
	rrSurface_Swap(pTo_can_be_source,&dest);
}

#endif

static RADINLINE int activity_pf_channels(rrPixelFormat pf)
{
	RR_ASSERT( pf == rrPixelFormat_1_F32 || pf == rrPixelFormat_4_F32 );
	if ( pf == rrPixelFormat_4_F32 )
		return 4;
	else
		return 1;
}

	
void make_activity_mask_constant_for_rmse_1or4F(rrSurface * pTo,const rrSurface & source)
{
	int nc = activity_pf_channels(pTo->pixelFormat);
	rrSurface_AssertSizeAndFormat(pTo, source.width, source.height, pTo->pixelFormat);
	
	// VQD = 2 * SSD
	// that gives us similarish average values to full VQD
	//	which means the lambda scaling is about right
	// 2.0 isn't perfect, but it's pretty close and a nice even number
	const F32 constant_activity_value = 2.f;
	
	for LOOP(y,source.height)
	{
		F32 * activity_row = (F32 *) rrSurface_Seek(pTo,0,y);
		SINTa nx = nc * source.width;
		
		for LOOP(x,nx)
		{
			activity_row[x] = constant_activity_value;
		}
	}	
}

// somewhere in the 2-3 zone seems right for sigma
// larger sigma generally = more areas considered "noisy"
// less than 3 hurts RMSE performace but I think improves perceptual
static const F32 lowpass_sigma = 2.2f;
	
static const F32 activity_blur_sigma = 1.25; // @@ ??


// after PreprocessActivity, activity is on inverse scale
//	higher = less activity = error is more important
//	it's just multiplied into SSD
//	output values are in [1,256]

static void preprocess_activity_from_sqr_to_linear_reciprocal_row(F32 * activity_out, const F32 * activity_in, SINTa float_width, const F32 activity_scaler)
{
	const float C = 1.f; // old
	//const float C = 0.01f; // @@ CB test 03/01/2020 : BC1 and BC7 are fine with much smaller C
	SINTa i = 0;

#if defined(__RADSSE2__) || defined(DO_BUILD_NEON64) // need AArch64 IEEE-compliant vectors
	for (; i < (float_width & ~3); i += 4)
	{
		VecF32x4 activity = VecF32x4::loadu(activity_in + i).sqrt();

		VecF32x4 scale = VecF32x4(256.f) / (VecF32x4(C) + VecF32x4(activity_scaler) * activity);
		scale = scale.max(VecF32x4(1.f));

		scale.storeu(activity_out + i);
	}
#endif

	for (; i < float_width; i++)
	{
		// late sqrt :
		// sqrt the dsqr to make it linear :
		F32 activity = sqrtf(activity_in[i]);

		// activity is linear, ssd is like a square
		//	if you do (ssd/activity) that's back to linear in pixel values, like SAD

		// @@ there are lots of other options here of course
		//	  log scale of activity, lots of scalings and constants

		F32 scale = 256.f / ( C + activity_scaler * activity );

		// @@ clamp scale >= 1 so we never lose ssd ?
		//	  (if you don't do this, it's possible for an ssd step of 1 to have no effect on output D)
		//	-> this is better for rmse; I think it's also slightly better perceptually
		//	 it only affects areas of very high activity (activity 64)
		//	 if you don't do this clamp, the high variance areas can get visible large artifacts
		// @@ ?? haven't really tweaked this ; maybe 0.5 ?
		//	 now that I stay in floats you never just lose ssd diffs
		// -> this clamp actually gets hit a lot for high activity
		//	median activity is ~ 50 and the clamp kicks in at activity 64
		scale = RR_MAX(scale,1.f);

		// if C = 1.f , max scale is 256
		//	if we let C go very small, scale could get huge
		//	that can make VQD bigger than LARGE_DIFF or ERROR_MAX which might do weird things
		//	@@ -> those should be fixed to be much larger
		//	   NOTE : currently does not apply and this MIN should never be hit with C = 1
		RR_ASSERT( scale <= 256.f );
		//scale = RR_MIN(scale,65536.f);

		activity_out[i] = scale;
		//float_sum += scale;
	}

	#if 0
	// for RGB_A do this per channel? separately for RGB and A ?
	int float_count = float_width * pActivity->height;
	F32 average = (F32)float_sum/float_count;
	#define INVACTIVITY_MAX_AVERAGE		8.f
	if ( average > INVACTIVITY_MAX_AVERAGE )
	{
		F32 mul = INVACTIVITY_MAX_AVERAGE / average;

		for LOOP(y,pActivity->height)
		{
			F32 * activity_row = (F32 *) rrSurface_Seek(pActivity,0,y);

			for LOOP(x,float_width)
			{
				activity_row[x] *= mul;
			}
		}
	}
	#endif
}

static void preprocess_activity_from_sqr_to_linear_reciprocal(rrSurface * pActivity,const F32 activity_scaler)
{
	int nc = activity_pf_channels(pActivity->pixelFormat);

	SINTa float_width = pActivity->width * nc;
	// @@ in 4F_RGBA mode we do the exact same value 4 times
	//	 should really make the activity as 1F initially for that
	//	 then replicate to 4F at the last minute (after this)
	//F64 float_sum = 0.f;

	for LOOP(y,pActivity->height)
	{
		F32 * activity_row = (F32 *) rrSurface_Seek(pActivity,0,y);
		preprocess_activity_from_sqr_to_linear_reciprocal_row(activity_row,activity_row,float_width,activity_scaler);
	}
}

struct activity_1or4F_context
{
	rrSurfaceObj source;
	rrSurfaceObj temp_activity;
	rrSurfaceObj final_activity;
	EMakeActivityMode mam;
	F32 activity_scaler;
};

static void OODLE_CALLBACK activity_1or4F_pass1_consumer_default(void * ctx_void, const void * pixels, rrPixelFormat pf, SINTa x0, SINTa y, SINTa width)
{
	activity_1or4F_context * ctx = (activity_1or4F_context *) ctx_void;
	RR_ASSERT(pf == rrPixelFormat_4_F32); // this is for the low-pass data

	// make squared distance from source to lowpass :
	const rrColor4F * lowpass_row = (const rrColor4F *) pixels;
	const rrColor4F * source_row = (const rrColor4F *) rrSurface_SeekC(&ctx->source,x0,y);
	rrColor4F * activity_flt4 = (rrColor4F *) rrSurface_Seek(&ctx->temp_activity,x0,y);
	F32 * activity_flt = (F32 *)activity_flt4;

	switch(ctx->mam)
	{
	case e_make_activity_4F_RGBA:
//		RR_ASSERT(ctx->activity.pixelFormat == rrPixelFormat_4_F32);
		for LOOP(x,width)
		{
			F32 dsqr = rrColor4F_DeltaSqrRGBA(source_row[x],lowpass_row[x]);

			activity_flt4[x].r = dsqr;
			activity_flt4[x].g = dsqr;
			activity_flt4[x].b = dsqr;
			activity_flt4[x].a = dsqr;
		}
		break;
	case e_make_activity_4F_RGB_A:
//		RR_ASSERT(ctx->activity.pixelFormat == rrPixelFormat_4_F32);
		for LOOP(x,width)
		{
			F32 dsqr = rrColor4F_DeltaSqrRGB(source_row[x],lowpass_row[x]);

			activity_flt4[x].r = dsqr;
			activity_flt4[x].g = dsqr;
			activity_flt4[x].b = dsqr;

			/*
			// -> this has no affect
			// because in "bc7rgb" mode IgnoreAlpha makes A error exactly 0
			// so scaling it in VQD doesn't matter
			if ( options & rrDXTCOptions_BC7_IgnoreAlpha )
			{
				// make A inv-activity zero (RGB only error) :
				//	(will hit the inv-activity clamp at 1)
				activity_row[x].a = 9999999999.f;
			}
			else
			*/
			{
				activity_flt4[x].a = fsquare( source_row[x].a - lowpass_row[x].a );
			}
		}
		break;
	case e_make_activity_4F_R_G_B_A:
//		RR_ASSERT(ctx->activity.pixelFormat == rrPixelFormat_4_F32);
		for LOOP(x,width)
		{
			activity_flt4[x].r = fsquare( source_row[x].r - lowpass_row[x].r );
			activity_flt4[x].g = fsquare( source_row[x].g - lowpass_row[x].g );
			activity_flt4[x].b = fsquare( source_row[x].b - lowpass_row[x].b );
			activity_flt4[x].a = fsquare( source_row[x].a - lowpass_row[x].a );
		}
		break;

	case e_make_activity_1F_RGBA:
//		RR_ASSERT(ctx->activity.pixelFormat == rrPixelFormat_1_F32);
		for LOOP(x,width)
		{
			F32 dsqr = rrColor4F_DeltaSqrRGBA(source_row[x],lowpass_row[x]);

			activity_flt[x] = dsqr;
		}
		break;
	case e_make_activity_1F_RGB:
//		RR_ASSERT(ctx->activity.pixelFormat == rrPixelFormat_1_F32);
		for LOOP(x,width)
		{
			F32 dsqr = rrColor4F_DeltaSqrRGB(source_row[x],lowpass_row[x]);

			activity_flt[x] = dsqr;
		}
		break;
	case e_make_activity_1F_A:
//		RR_ASSERT(ctx->activity.pixelFormat == rrPixelFormat_1_F32);
		for LOOP(x,width)
		{
			activity_flt[x] = fsquare( source_row[x].a - lowpass_row[x].a );
		}
		break;

	RR_NO_DEFAULT_CASE
	}
}

static void OODLE_CALLBACK activity_1or4F_pass1_consumer_rgbnarrow(void * ctx_void, const void * pixels, rrPixelFormat pf, SINTa x0, SINTa y, SINTa width)
{
	activity_1or4F_context * ctx = (activity_1or4F_context *) ctx_void;
	RR_ASSERT_ALWAYS(ctx->mam == e_make_activity_1F_RGB); // only support 1F_RGB activity masks here

	// make squared distance from source to lowpass :
	const F32 * lowpass_row = (const F32 *) pixels;
	const F32 * source_row = (const F32 *) rrSurface_SeekC(&ctx->source,x0,y);
	F32 * activity_flt = (F32 *) rrSurface_Seek(&ctx->temp_activity,x0,y);

	switch ( pf )
	{
	case rrPixelFormat_1_F32:
		for LOOP(x,width)
		{
			F32 dsqr = fsquare(source_row[x] - lowpass_row[x]);
			activity_flt[x] = dsqr;
		}
		break;
	case rrPixelFormat_2_F32:
		for LOOP(x,width)
		{
			F32 dsqr;
			dsqr  = fsquare(source_row[x*2 + 0] - lowpass_row[x*2 + 0]);
			dsqr += fsquare(source_row[x*2 + 1] - lowpass_row[x*2 + 1]);
			activity_flt[x] = dsqr;
		}
		break;
	default:
		RR_BREAK(); // not supported here
	}
}

static void OODLE_CALLBACK activity_1or4F_pass2_consumer(void * ctx_void, const void * pixels, rrPixelFormat pf, SINTa x0, SINTa y, SINTa width)
{
	activity_1or4F_context * ctx = (activity_1or4F_context *) ctx_void;

	int nc = activity_pf_channels(pf);
	F32 * activity_row = (F32 *) rrSurface_Seek(&ctx->final_activity,x0,y);

	preprocess_activity_from_sqr_to_linear_reciprocal_row(activity_row,(const F32 *)pixels,width * nc,ctx->activity_scaler);
}

void make_activity_mask_1or4F(rrSurface * pTo,const rrSurface & source_orig,EMakeActivityMode mam,rrDXTCOptions options,int dest_channel_count)
{
	SIMPLEPROFILE_SCOPE_N(make_activity_mask,source_orig.width * source_orig.height);

	const rrPixelFormatInfo * pfi = rrPixelFormat_GetInfo(source_orig.pixelFormat);
	CpuDispatchFlags dispatch = CpuDispatchFlags::init(&options);

	activity_1or4F_context ctx;
	ctx.mam = mam;
	rrPixelFormat pf = EMakeActivityMode_to_PF(mam);

	// default to the standard activity pass1 consumer and pixel format
	rrPixelConsumerFunc * activity_pass1_consumer = activity_1or4F_pass1_consumer_default;
	rrPixelFormat activity_pass1_srcfmt = rrPixelFormat_4_F32;

	// when asked for a 1-channel RGB activity for a 1- or 2-channel format, we don't need to blow up
	// the input data to 4xF32
	if ( mam == e_make_activity_1F_RGB && 1 <= dest_channel_count && dest_channel_count <= 2 )
	{
		activity_pass1_consumer = activity_1or4F_pass1_consumer_rgbnarrow;
		activity_pass1_srcfmt = RR_MAKE_PIXELFORMAT_SIMPLE(dest_channel_count,RR_PIXELFORMAT_TYPE_F32);
	}

	// change source format to our chosen source format
	rrSurface_AllocCopy_ChangeFormatNormalized(&ctx.source,&source_orig,activity_pass1_srcfmt);

	// make lowpass and then initial activity in one go
	// preferably without the 4xF32 data ever hitting memory
	rrSurface_Alloc(&ctx.temp_activity,source_orig.width,source_orig.height,pf);

	// looks a bit weird but pTo is guaranteed to be live after this func
	// and final_activity isn't; I don't want the threaded version to need
	// an extra final job just to do a rrSurface_Swap.
	rrSurface_AssertSizeAndFormat(pTo, source_orig.width, source_orig.height, pf);
	rrSurface_SetView(&ctx.final_activity,pTo);

	// activity_scaler was 4.0 for Non-Normalized U8 input
	//	now activity is normalized so it's *(1/255) lower
	// with normalized we don't need to detect input type and adjust scale
	ctx.activity_scaler = 4.0f * 256.f;

	// the old code left signed formats at their original scale, whereas now (with normalized)
	// processing they're 2x the value range, so compensate for this
	if ( pfi->isSigned && ! pfi->isFloat )
		ctx.activity_scaler *= 0.5f;

	// now run both passes
	// pass 1 is initial activity
	rrSurface_MakeGaussianBlurredWithConsumer(ctx.source,lowpass_sigma,activity_pass1_consumer,&ctx,dispatch);

	// pass 2 is blur activity to spread around then "preprocess" (for consumer, which is a postprocess from our vantage point)
	rrSurface_MakeGaussianBlurredWithConsumer(ctx.temp_activity,activity_blur_sigma,activity_1or4F_pass2_consumer,&ctx,dispatch);
}

struct activity_hdr_1F_context
{
	rrSurfaceObj source;
	rrSurfaceObj avg_intens;
	rrSurfaceObj temp_activity;
	rrSurfaceObj final_activity;
	F32 activity_scaler;
};

static void OODLE_CALLBACK activity_hdr_1F_avgintens_consumer(void * ctx_void, const void * pixels, rrPixelFormat pf, SINTa x0, SINTa y, SINTa width)
{
	activity_hdr_1F_context * ctx = (activity_hdr_1F_context *) ctx_void;
	RR_ASSERT(pf == rrPixelFormat_3_F32);

	const rrVec3f * temp_row = (const rrVec3f *) pixels;
	F32 * avg_intens_row = (F32 *)rrSurface_Seek(&ctx->avg_intens,x0,y);

	for LOOP(x,width)
	{
		// calculate squared length of RGB vector
		F32 len2 = temp_row[x].x*temp_row[x].x + temp_row[x].y*temp_row[x].y + temp_row[x].z*temp_row[x].z;

		// this is for BC6H, value scale is half-float;
		// smallest subnormal half-float is 2^(-24) in input
		// and we look at squares of values
		// @@ CB - maybe use something slightly larger than this here?
		//	to bias against caring about tiny values in black regions?
		const F32 eps = 1.0f / (16777216.0f * 16777216.0f);

		// to normalize for avg intensity, divide through by average intensity over a larger range
		avg_intens_row[x] = 1.0f / (len2 + eps);
	}
}

static void OODLE_CALLBACK activity_hdr_1F_pass1_consumer(void * ctx_void, const void * pixels, rrPixelFormat pf, SINTa x0, SINTa y, SINTa width)
{
	activity_hdr_1F_context * ctx = (activity_hdr_1F_context *) ctx_void;
	RR_ASSERT(pf == rrPixelFormat_3_F32);

	// make squared distance from source to lowpass, dividing by average intensity level
	const rrVec3f * lowpass_row = (const rrVec3f *) pixels;
	const rrVec3f * source_row = (const rrVec3f *) rrSurface_SeekC(&ctx->source,x0,y);
	const F32 * avg_intens_row = (const F32 *) rrSurface_SeekC(&ctx->avg_intens,0,y >> 2); // note uses 0 not x0, we handle x0 below
	F32 * activity_row = (F32 *) rrSurface_Seek(&ctx->temp_activity,x0,y);

	for LOOP(x,width)
	{
		// BC6 does not support Alpha, just RGB delta:
		F32 dsqr = fsquare(source_row[x].x - lowpass_row[x].x) + fsquare(source_row[x].y - lowpass_row[x].y) + fsquare(source_row[x].z - lowpass_row[x].z);

		// and multiply by value scale
		F32 act = dsqr * avg_intens_row[(x + x0) >> 2];

		activity_row[x] = act;
	}
}

static void OODLE_CALLBACK activity_hdr_1F_pass2_consumer(void * ctx_void, const void * pixels, rrPixelFormat pf, SINTa x0, SINTa y, SINTa width)
{
	activity_hdr_1F_context * ctx = (activity_hdr_1F_context *) ctx_void;
	RR_ASSERT(pf == rrPixelFormat_1_F32);

	F32 * activity_row = (F32 *) rrSurface_Seek(&ctx->final_activity,x0,y);

	preprocess_activity_from_sqr_to_linear_reciprocal_row(activity_row,(const F32 *)pixels,width,ctx->activity_scaler);
}

void make_activity_mask_hdr_1F(rrSurface * pTo,const rrSurface & source_orig,rrDXTCOptions options)
{
	activity_hdr_1F_context ctx;
	CpuDispatchFlags dispatch = CpuDispatchFlags::init(&options);

	// Change source format to 3 F32 :
	rrSurface_AllocCopy_ChangeFormatNormalized(&ctx.source,&source_orig,rrPixelFormat_3_F32);

	// Original and subsampled width/height
	SINTa width = ctx.source.width;
	SINTa height = ctx.source.height;
	SINTa width_sub = (width + 3) / 4;
	SINTa height_sub = (height + 3) / 4;

	// Take source and downsample by 4x then blur to get a much larger blur
	// that gives us an average intensity level to normalize by
	rrSurfaceObj temp;
	rrSurface_Alloc(&temp,width_sub,height_sub,rrPixelFormat_3_F32);
	{
		for LOOP(y,height_sub)
		{
			// Clamp y coords
			const rrVec3f * fm0 = (const rrVec3f *) rrSurface_SeekC(&ctx.source,0,RR_MIN(y*4+0, height-1));
			const rrVec3f * fm1 = (const rrVec3f *) rrSurface_SeekC(&ctx.source,0,RR_MIN(y*4+1, height-1));
			const rrVec3f * fm2 = (const rrVec3f *) rrSurface_SeekC(&ctx.source,0,RR_MIN(y*4+2, height-1));
			const rrVec3f * fm3 = (const rrVec3f *) rrSurface_SeekC(&ctx.source,0,RR_MIN(y*4+3, height-1));
			rrVec3f * to = (rrVec3f *) rrSurface_Seek(&temp,0,y);

			for LOOP(x,width_sub)
			{
				rrVec3f sum(0.0f, 0.0f, 0.0f);

				for LOOP(i,4)
				{
					// Clamp x coords pre-average
					SINTa xi = RR_MIN(x*4 + i, width-1);
					sum += fm0[xi] + fm1[xi] + fm2[xi] + fm3[xi];
				}

				to[x] = (1.0f / 16.0f) * sum;
			}
		}
	}

	// turn into average intensity weight that we normalize by
	F32 intens_sigma = 2.2f;

	rrSurface_Alloc(&ctx.avg_intens,width_sub,height_sub,rrPixelFormat_1_F32);
	rrSurface_MakeGaussianBlurredWithConsumer(temp,intens_sigma,activity_hdr_1F_avgintens_consumer,&ctx,dispatch);
	rrSurface_Free(&temp);

	// Allocate buffers for temp activity and destination
	rrSurface_Alloc(&ctx.temp_activity,width,height,rrPixelFormat_1_F32);

	// Final activity is an alias for the destination (but it's in the context)
	rrSurface_AssertSizeAndFormat(pTo, width, height, rrPixelFormat_1_F32);
	rrSurface_SetView(&ctx.final_activity,pTo);

	// 8-bit version uses 4*256=1024 here, but the activity values
	// come out a bit low still, so boost some more.
	ctx.activity_scaler = 1536.0f;

	// Initial activity pass
	// subtracts original from lowpass (so highpass), determines squared distance
	// and divides by average intensity level normalization factor
	rrSurface_MakeGaussianBlurredWithConsumer(ctx.source,lowpass_sigma,activity_hdr_1F_pass1_consumer,&ctx,dispatch);

	// Activity spreading pass, then "preprocess" (postprocess from our vantage point)
	rrSurface_MakeGaussianBlurredWithConsumer(ctx.temp_activity,activity_blur_sigma,activity_hdr_1F_pass2_consumer,&ctx,dispatch);
}

//===========================================

// VQD optimized not easy to fiddle with
// VQD is just SSD with a scaling on each pixel
//  we can do the SIMD SSD like BC1_Palette_SSD_RGB
//	and just mul through by activity
F32 VQD_BC1(CpuDispatchFlags dispatch, const rrColorBlock4x4 & colors,const rrColor32BGRA palette[4],const U32 in_indices,const SingleFloatBlock4x4 & activity)
{
#ifdef DO_BUILD_AVX2
	if (dispatch.AVX2())
		return internal::VQD_BC1_AVX2(colors,palette,in_indices,activity);
#endif

	F32 sum = 0;
	U32 indices = in_indices;

#if defined(DO_BUILD_SSE4)
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
#elif defined(DO_BUILD_NEON64)
	Vec128_U8 pal_vec = Vec128_U8::loadu(palette);

	// Make 4 pre-shifted versions of the index
	// for the 4 index bytes, lanes 0-3 get bits [1:0], [3:2], [5:4], [7:6] respectively
	// and move them to bits [3:2] within their respective bytes
	Vec128_U8 index_vec = Vec128_U32(vshlq_u32(Vec128_U32(indices), int32x4_t { 2, 0, -2, -4 })).u8() & Vec128_U8(0x0c);
	VecF32x4 accum_f32 = VecF32x4::zero();

	for (int r = 0; r < 4; r++)
	{
		// load 4 pixels
		Vec128_U8 src_pixels = Vec128_U8::loadu(colors.colors + r*4);

		// make 4 copies of each index byte and OR in RGBA byte index
		Vec128_U8 pal_index = index_vec.shuf(Vec128_U8 { 0,0,0,0, 4,4,4,4, 8,8,8,8, 12,12,12,12 }) | Vec128_U8 { 0,1,2,3, 0,1,2,3, 0,1,2,3, 0,1,2,3 };

		// select the bytes via shuffle
		Vec128_U8 pal_pixels = pal_vec.shuf(pal_index);

		// advance to next byte for next iter
		index_vec = vextq_u8(index_vec, index_vec, 1);

		#if 1 // activity 1F
		accum_f32 += VQD_NEON_Diff_4U8_Activity1F(src_pixels, pal_pixels, activity.values + r*4);
		#endif

		#if 0 // activity 4F
		accum_f32 += VQD_NEON_Diff_4U8_Activity4F(src_pixels, pal_pixels, activity.values + r*4);
		#endif
	}

	F32 sum_neon = accum_f32.sum_across_inner_outer().scalar_x();

	sum = sum_neon;
#else
	// scalar fallback
	// maintain same summing order as we do in the SIMD versions
	VQDInternalAccum accum;

	for LOOP(y,4)
	{
		for LOOP(x,4)
		{
			const rrColor32BGRA dxtbc = palette[indices&3]; indices >>= 2;

			rrColor32BGRA c = colors.colors[y*4+x];

			#if 0 // 4F
			accum.activity4f(c,dxtbc,activity.values[y*4+x]);
			const rrColor4F & act = activity.values[y*4+x];

			sums[0] += act.r * fsquare((F32)((S32)c.u.r - dxtbc.u.r));
			sums[1] += act.g * fsquare((F32)((S32)c.u.g - dxtbc.u.g));
			sums[2] += act.b * fsquare((F32)((S32)c.u.b - dxtbc.u.b));
			sums[3] += act.a * fsquare((F32)((S32)c.u.a - dxtbc.u.a));
			#endif

			// 1F :
			accum.activity1f(c,dxtbc,activity.values[y*4+x],x);
		}
	}

	sum = accum.result();
#endif

	return sum;
}

// colors-colors variant of VQD :
//	!! BEWARE !! LOTS OF CODE DUPE WITH ABOVE PRIMARY VARIANT
// does RGBA SSD
//	rrColorBlock4x4 is BGRA but that doesn't actually matter , RGBA will give the same result
F32 VQD(const rrColorBlock4x4 & colors1,const rrColorBlock4x4 & colors2,const FourFloatBlock4x4 & activity)
{
	F32 sum = 0;

#if defined(DO_BUILD_SSE4)
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
#elif defined(DO_BUILD_NEON64)
	VecF32x4 accum_f32 = VecF32x4::zero();

	for(int r=0;r<4;r++)
	{
		// load 4 pixels each
		Vec128_U8 v1 = Vec128_U8::loadu(colors1.colors + r*4);
		Vec128_U8 v2 = Vec128_U8::loadu(colors2.colors + r*4);

		accum_f32 += VQD_NEON_Diff_4U8_Activity4F(v1, v2, activity.values + r*4);
	}

	// horizontal sum across the lanes of accum_f32 :
	sum = accum_f32.sum_across_inner_outer().scalar_x();
#else
	// scalar fallback
	// maintain same summing order as we do in the SIMD versions
	VQDInternalAccum accum;

	for LOOP(i,16)
	{
		accum.activity4f(colors1.colors[i],colors2.colors[i],activity.values[i]);
	}

	sum = accum.result();
#endif

	return sum;
}

// colors-colors variant of VQD :
//	!! BEWARE !! LOTS OF CODE DUPE WITH ABOVE PRIMARY VARIANT
// does RGBA SSD
//	rrColorBlock4x4 is BGRA but that doesn't actually matter , RGBA will give the same result
F32 VQD(const rrColorBlock4x4 & colors1,const rrColorBlock4x4 & colors2,const SingleFloatBlock4x4 & activity)
{
	F32 sum = 0;

#if defined(DO_BUILD_SSE4)
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
#elif defined(DO_BUILD_NEON64)
	VecF32x4 accum_f32 = VecF32x4::zero();

	for(int r=0;r<4;r++)
	{
		// load 4 pixels each
		Vec128_U8 v1 = Vec128_U8::loadu(colors1.colors + r*4);
		Vec128_U8 v2 = Vec128_U8::loadu(colors2.colors + r*4);

		accum_f32 += VQD_NEON_Diff_4U8_Activity1F(v1, v2, activity.values + r*4);
	}

	// horizontal sum across the lanes of accum_f32 :
	sum = accum_f32.sum_across_inner_outer().scalar_x();
#else
	// scalar fallback
	// maintain same summing order as we do in the SIMD versions
	VQDInternalAccum accum;

	for LOOP(y,4)
	{
		for LOOP(x,4)
		{
			int i = y*4+x;
			accum.activity1f(colors1.colors[i],colors2.colors[i],activity.values[i],x);
		}
	}

	sum = accum.result();
#endif

	return sum;
}

// For S16 pixels _that don't use the whole range_ (so we can subtract them w/o overflows)
F32 VQD(const S16 colors1[16],const S16 colors2[16],const SingleFloatBlock4x4 & activity)
{
	F32 sum = 0;

#if defined(DO_BUILD_SSE4) || defined(DO_BUILD_NEON64)

	// load all 16 pixels and diff them
	Vec128_S16 diff01_16 = Vec128_S16::loadu(colors1 + 0) - Vec128_S16::loadu(colors2 + 0);
	Vec128_S16 diff23_16 = Vec128_S16::loadu(colors1 + 8) - Vec128_S16::loadu(colors2 + 8);

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

#else

	// scalar fallback
	// maintain same summing order as we do in the SIMD versions
	F32 sums[4] = { 0.f, 0.f, 0.f, 0.f };

	for LOOP(i,16)
	{
		int diff = colors1[i] - colors2[i];
		// assumes PreprocessActivity has been done
		F32 scale = activity.values[i];

		sums[i&3] += (diff * diff) * scale;
	}

	// final sum in same order as SIMD ver
	sum = (sums[0] + sums[2]) + (sums[1] + sums[3]);

#endif

	return sum;
}

//===========================================

RR_NAMESPACE_END
