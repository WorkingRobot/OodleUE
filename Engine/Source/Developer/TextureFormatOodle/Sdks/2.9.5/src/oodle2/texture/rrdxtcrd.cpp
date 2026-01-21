// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrdxtcrd.h"
#include "rrdxt1vq.h"
#include "bc4rd.h"
#include "bc6rd.h"
#include "bc7rd.h"
#include "rrsurfacejobslicer.h"
#include "rrsurfacedxtc.h"
#include "perceptualactivity.h"
#include "layout.h"
#include "templates/rrnew.h"
#include "templates/rrvector_st.h"
#include "oodlebase.h"
#include "oodlejob.h"

//#include "../texutil/rrsurfacebyname.h" // @@ TEMP
//#include "rrsurfaceblit.h"

//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"
#include "threadprofiler.h"

RR_NAMESPACE_START

// High-level RDO encode options per codec
struct RDOEncodeConfig
{
	rrDXTCLevel baseline_effort; // Level to use for the baseline encode
	int max_slice_blocks;
};

static const RDOEncodeConfig * get_rdo_encode_config(rrPixelFormat fmt, rrDXTCLevel level)
{
	static const RDOEncodeConfig c_bc1_configs[rrDXTCLevel_Count] =
	{
		{ rrDXTCLevel_Slow,     4096 }, // Very Fast (not exposed)
		{ rrDXTCLevel_Slow,     4096 }, // Fast ("Low" effort in public API)
		{ rrDXTCLevel_VerySlow, 8192 }, // Slow ("Normal" effort in public API)
		{ rrDXTCLevel_VerySlow, 8192 }, // VerySlow ("High" effort in public API)
		{ rrDXTCLevel_VerySlow, 8192 }, // Reference (not exposed)
	};

	static const RDOEncodeConfig c_bc2_configs[rrDXTCLevel_Count] =
	{
		{ rrDXTCLevel_Slow    , 2048 }, // Very Fast (not exposed)
		{ rrDXTCLevel_Slow    , 2048 }, // Fast ("Low" effort in public API)
		{ rrDXTCLevel_VerySlow, 2048 }, // Slow ("Normal" effort in public API)
		{ rrDXTCLevel_VerySlow, 4096 }, // VerySlow ("High" effort in public API)
		{ rrDXTCLevel_VerySlow, 4096 }, // Reference (not exposed)
	};

	static const RDOEncodeConfig c_bc3_configs[rrDXTCLevel_Count] =
	{
		{ rrDXTCLevel_Slow,     2048 }, // Very Fast (not exposed)
		{ rrDXTCLevel_Slow,     2048 }, // Fast ("Low" effort in public API)
		{ rrDXTCLevel_VerySlow, 2048 }, // Slow ("Normal" effort in public API)
		{ rrDXTCLevel_VerySlow, 4096 }, // VerySlow ("High" effort in public API)
		{ rrDXTCLevel_VerySlow, 4096 }, // Reference (not exposed)
	};

	static const RDOEncodeConfig c_bc4_configs[rrDXTCLevel_Count] =
	{
		{ rrDXTCLevel_VerySlow, 2048 }, // Very Fast (not exposed)
		{ rrDXTCLevel_VerySlow, 2048 }, // Fast ("Low" effort in public API)
		{ rrDXTCLevel_VerySlow, 4096 }, // Slow ("Normal" effort in public API)
		{ rrDXTCLevel_VerySlow, 8192 }, // VerySlow ("High" effort in public API)
		{ rrDXTCLevel_VerySlow, 8192 }, // Reference (not exposed)
	};

	static const RDOEncodeConfig c_bc5_configs[rrDXTCLevel_Count] =
	{
		{ rrDXTCLevel_VerySlow, 1024 }, // Very Fast (not exposed)
		{ rrDXTCLevel_VerySlow, 1024 }, // Fast ("Low" effort in public API)
		{ rrDXTCLevel_VerySlow, 2048 }, // Slow ("Normal" effort in public API)
		{ rrDXTCLevel_VerySlow, 4096 }, // VerySlow ("High" effort in public API)
		{ rrDXTCLevel_VerySlow, 4096 }, // Reference (not exposed)
	};

	static const RDOEncodeConfig c_bc6_configs[rrDXTCLevel_Count] =
	{
		{ rrDXTCLevel_Slow,     4096 }, // Very Fast (not exposed)
		{ rrDXTCLevel_Slow,     4096 }, // Fast ("Low" effort in public API)
		{ rrDXTCLevel_VerySlow, 4096 }, // Slow ("Normal" effort in public API)
		{ rrDXTCLevel_VerySlow, 4096 }, // VerySlow ("High" effort in public API)
		{ rrDXTCLevel_VerySlow, 4096 }, // Reference (not exposed)
	};

	static const RDOEncodeConfig c_bc7_configs[rrDXTCLevel_Count] =
	{
		{ rrDXTCLevel_Slow,     2048 }, // Very Fast (not exposed)
		{ rrDXTCLevel_Slow,     2048 }, // Fast ("Low" effort in public API)
		{ rrDXTCLevel_Slow,     4096 }, // Slow ("Normal" effort in public API)
		{ rrDXTCLevel_VerySlow, 4096 }, // VerySlow ("High" effort in public API)
		{ rrDXTCLevel_VerySlow, 4096 }, // Reference (not exposed)
	};

	RR_ASSERT(level >= rrDXTCLevel_VeryFast && level <= rrDXTCLevel_Reference);
	switch ( fmt )
	{
	case rrPixelFormat_BC1:	return &c_bc1_configs[level];
	case rrPixelFormat_BC2: return &c_bc2_configs[level];
	case rrPixelFormat_BC3: return &c_bc3_configs[level];
	case rrPixelFormat_BC4U: // fall-through
	case rrPixelFormat_BC4S: return &c_bc4_configs[level];
	case rrPixelFormat_BC5U: // fall-through
	case rrPixelFormat_BC5S: return &c_bc5_configs[level];
	case rrPixelFormat_BC6U: // fall-through
	case rrPixelFormat_BC6S: return &c_bc6_configs[level];
	case rrPixelFormat_BC7:  return &c_bc7_configs[level];
	default:
		RR_ASSERT_FAILURE("not BC?");
	}

	return 0;
}

static bool rrDXTCRD_Single(BlockSurface * to_blocks,
	const BlockSurface * from_blocks,
	const BlockSurface * baseline_blocks,
	const BlockSurface * activity_blocks_1F,
	//const BlockSurface * activity_blocks_1F_A,
	//const BlockSurface * activity_blocks_4F,
	int lambda,
	rrDXTCOptions options,
	const rrDXTCRD_Options & rdopts)
{
	THREADPROFILESCOPE("RDO");

	switch( to_blocks->pixelFormat )
	{
	case rrPixelFormat_BC1:	
		return BC1_RD(to_blocks,from_blocks,baseline_blocks,activity_blocks_1F,lambda,options,rdopts);
		
	case rrPixelFormat_BC2:
	case rrPixelFormat_BC3:
		{
			// @@ the alpha and color encodes are completely separate and don't interfere with
			// each other, could easily run them in parallel if we wanted to

			// Slice destination surface into alpha and color halves
			// and encode them separately.
			BlockSurface to_blocks_alpha, baseline_blocks_alpha;
			BlockSurface to_blocks_color, baseline_blocks_color;

			BlockSurface_SetView(&to_blocks_alpha, to_blocks);
			BlockSurface_SetView(&baseline_blocks_alpha, baseline_blocks);
			BlockSurface_SetView(&to_blocks_color, to_blocks);
			BlockSurface_SetView(&baseline_blocks_color, baseline_blocks);

			// Bump "color" pointers by size of the preceding alpha block
			// so the color blocks look just like regular BC1 blocks, albeit with
			// twice the normal spacing.
			to_blocks_color.blocks += 8;
			baseline_blocks_color.blocks += 8;

			const BlockSurface * activity_blocks_1F_A = activity_blocks_1F;
			bool A_blocks_done = false;

			if ( to_blocks->pixelFormat == rrPixelFormat_BC3 )
			{
				// Encode the alpha part first as quasi-BC4

				int alpha_lambda = lambda;
				if ( rdopts.use_bc3_alpha_lambda )
					alpha_lambda = rdopts.bc3_alpha_lambda;

				if ( alpha_lambda > 0 )
				{
					if ( ! BC4_RD(&to_blocks_alpha,from_blocks,&baseline_blocks_alpha,activity_blocks_1F_A,alpha_lambda,options,rdopts) )
						return false;

					A_blocks_done = true;
				}
				// else if == 0 , drop down and use path below :
			}

			if ( ! A_blocks_done )
			{
				// BC2 or BC3 with RD disabled
				//RR_ASSERT( to_blocks->pixelFormat == rrPixelFormat_BC2 );
				// just copy baseline, no RD for the BC2-alpha

				RR_ASSERT( baseline_blocks_alpha.blockSizeBytes == 16 );
				U8 * toPtr = to_blocks_alpha.blocks;
				const U8 * fmPtr = baseline_blocks_alpha.blocks;
				int n = baseline_blocks_alpha.count;
				while(n--)
				{
					RR_PUT64_NATIVE(toPtr, RR_GET64_NATIVE(fmPtr) );
					toPtr += 16;
					fmPtr += 16;
				}
			}

			// Then the color as quasi-BC1
			// BC1 detects that it's writing to BC3 and knows that it's all four-color mode
			if ( ! BC1_RD(&to_blocks_color,from_blocks,&baseline_blocks_color,activity_blocks_1F,lambda,options,rdopts) )
				return false;

			return true;
		}

	case rrPixelFormat_BC4U:
	case rrPixelFormat_BC4S:
	case rrPixelFormat_BC5U:
	case rrPixelFormat_BC5S:
		return BC4_RD(to_blocks,from_blocks,baseline_blocks,activity_blocks_1F,lambda,options,rdopts);
	
	case rrPixelFormat_BC6U:
	case rrPixelFormat_BC6S:
		return BC6_RD(to_blocks,from_blocks,baseline_blocks,activity_blocks_1F,lambda,options,rdopts);

	case rrPixelFormat_BC7:
		return BC7_RD(to_blocks,from_blocks,baseline_blocks,activity_blocks_1F,lambda,options,rdopts);
		//return BC7_RD(to_blocks,from_blocks,baseline_blocks,activity_blocks_4F,lambda,options,rdopts);

	default:
		ooLogError("rrDXTCRD_Single BCN not supported");
		BlockSurface_Copy(to_blocks,baseline_blocks);
		return false;
	}

}	
	
//===============================================
	
struct rrDXTCRD_Slicer_Data
{
	int lambda;
	rrDXTCOptions options;
	rrDXTCRD_Options rdoptions;
	
	const BlockSurface * full_baseline;
	const BlockSurface * full_activity_1F;
	//const BlockSurface * full_activity_1F_A; // for RGB_A BC3
	//const BlockSurface * full_activity_4F; // for R_G_B_A
};

rrbool rrDXTCRD_Slicer_Func(BlockSurfaceJobSlicer_Data * slice,void * data)
{
	rrDXTCRD_Slicer_Data * pdata = (rrDXTCRD_Slicer_Data *) data;
		
	int slice_start = slice->slice_start;
	int slice_count = slice->to_sliced.count;
	
	BlockSurface baseline_slice;
	BlockSurface_SetView(&baseline_slice,pdata->full_baseline,slice_start,slice_count);
	
	BlockSurface activity_slice_1F;
	if ( pdata->full_activity_1F->count == 0 )
		BlockSurface_Init(&activity_slice_1F);
	else
		BlockSurface_SetView(&activity_slice_1F,pdata->full_activity_1F,slice_start,slice_count);
	
	/*
	BlockSurface activity_slice_1F_A;
	if ( pdata->full_activity_1F_A->count == 0 )
		BlockSurface_Init(&activity_slice_1F_A);
	else
		BlockSurface_SetView(&activity_slice_1F_A,pdata->full_activity_1F_A,slice_start,slice_count);
	*/
	
	/*
	BlockSurface activity_slice_4F;
	if ( pdata->full_activity_4F->count == 0 )
		BlockSurface_Init(&activity_slice_4F);
	else
		BlockSurface_SetView(&activity_slice_4F,pdata->full_activity_4F,slice_start,slice_count);
	*/
	
	bool ret = rrDXTCRD_Single(&(slice->to_sliced),&(slice->from_sliced),&baseline_slice,
			&activity_slice_1F,
			//&activity_slice_1F_A,
			//&activity_slice_4F,
			pdata->lambda,pdata->options,pdata->rdoptions);
	
	return ret;
}

struct rrDXTCRD_Context
{
	BlockSurfaceObj activity_blocks_1F;
	//BlockSurfaceObj activity_blocks_1F_A; // for BC3 RGB_A
	//BlockSurfaceObj activity_blocks_4F;
	BlockSurfaceObj to_baseline;
	BlockSurfaceObj from_blocks_fmt;
	rrDXTCOptions options;
	rrDXTCRD_Options rdoptions;
};

bool rrDXTCRD_Encode_RDO_ReadContext_Ex(
	const rrDXTCRD_Context * ctx,
	BlockSurface * to_blocks,
	int lambda,const rrDXTCRD_Options &rdopts,
	void * jobify_user_ptr, int num_workers)
{
	THREADPROFILESCOPE("Encode_RDO");
	
	if ( num_workers == OODLEJOB_DEFAULT )
		num_workers = OodlePlugins_GetJobTargetParallelism();

	// chunk into 256 KB output LZ chunks (32K blocks (1024x512 pixels))

	// blocks should now be in output hardware swizzled order
	//	so chunking & LZ order is the way the compressor will see it

	// parallel slicing with no sharing across slices hurts about 0.5% (out of ~25%)

	// just slice and don't try to share VQ table across slices
	//	much easier threading structure obviously
	//
	// the idea is that this could actually be *good* in some cases
	// because the VQ assumes a global entropy model
	//	  (overall VQ palette count is used, no locality of reference)
	// whereas the back-end LZ is actually local
	//	  (lower offsets are cheaper)
	// so doing VQ on local regions gives you some locality
	//	better match to real back end LZ = better

	RR_ASSERT( to_blocks->pixelFormat == ctx->to_baseline.pixelFormat );
	RR_ASSERT( to_blocks->count == ctx->from_blocks_fmt.count );

	const RDOEncodeConfig * cfg = get_rdo_encode_config(to_blocks->pixelFormat,rdopts.effort);

	// 32K block chunks , 8K per slice = 4 way parallelism
	//	 this is true for BC1 and BC4 but not the remaining formats which are 16B/block
	// for those the 256k LZ chunks have 16k blocks and we would need to chop into chunks
	// of 4096 blocks for 4-way parallelism
	//
	// we now determine this from the effort level; the default at high quality levels is
	// to target 4 slices per 256k block, but this can be dialed down for speed
	const int max_slice_blocks = cfg->max_slice_blocks;
	
	rrDXTCRD_Slicer_Data data;
	data.lambda = lambda;
	data.options = ctx->options;
	data.rdoptions = rdopts;
	
	data.full_baseline = &ctx->to_baseline;
	data.full_activity_1F = &ctx->activity_blocks_1F;
	//data.full_activity_1F_A = &ctx->activity_blocks_1F_A;
	//data.full_activity_4F = &ctx->activity_blocks_4F;
	
	rrbool ok = BlockSurface_JobSlicer_256K_then_N(to_blocks,&ctx->from_blocks_fmt,num_workers,jobify_user_ptr,rrDXTCRD_Slicer_Func,&data,max_slice_blocks);
	
	return !!ok;
}

bool rrDXTCRD_Encode_RDO_ReadContext(
	const rrDXTCRD_Context * ctx,
	BlockSurface * to_blocks,
	int lambda,void * jobify_user_ptr, int num_workers)
{
	return rrDXTCRD_Encode_RDO_ReadContext_Ex(ctx,to_blocks,lambda,ctx->rdoptions,jobify_user_ptr, num_workers);
}

rrDXTCRD_Context * rrDXTCRD_Context_New()
{
	rrDXTCRD_Context * ctx = OodleNewT(rrDXTCRD_Context) ();
	
	return ctx;
}

void rrDXTCRD_Context_Delete(rrDXTCRD_Context * ctx)
{
	OodleDelete(ctx);
}

const BlockSurface * rrDXTCRD_Context_GetActivity(rrDXTCRD_Context * ctx)
{
	return &ctx->activity_blocks_1F;
}

const BlockSurface * rrDXTCRD_Context_GetBaseline(rrDXTCRD_Context * ctx)
{
	return &ctx->to_baseline;
}

const BlockSurface * rrDXTCRD_Context_GetFrom(rrDXTCRD_Context * ctx)
{
	return &ctx->from_blocks_fmt;
}

rrDXTCOptions rrDXTCRD_Context_GetOptions(rrDXTCRD_Context * ctx)
{
	return ctx->options;
}

struct activity_job_context
{
	rrSurface activity_surf;
	const rrSurface * from_surf;
	rrPixelFormat to_pf;
	EDXTCRD_Metric metric;
	EMakeActivityMode mam;
	rrDXTCOptions options;
};

void OODLE_CALLBACK activity_job_func( void * ptr )
{
	THREADPROFILESCOPE("activity");

	FPStateScope saved_state; // make sure we're in a defined FP state
	activity_job_context * ctx = (activity_job_context *) ptr;
	
	if ( ctx->metric == eDXTCRD_Metric_RMSE_RGBA )
	{
		rrPixelFormat activity_pf = EMakeActivityMode_to_PF(ctx->mam);
	
		make_activity_mask_constant_for_rmse_1or4F(&ctx->activity_surf,*ctx->from_surf,activity_pf);
	}
	else
	{
		// if source format is Float
		//	but to format is not BC6
		//	should we use _hdr activity or scalar?
		//	if we use scalar we have to make an assumption about what the float data holds
		//		(eg. treat [0,1] as [0,256] ?)
		// -> maybe force formats to standard formats before we get here?
		//	so we have it in one place where we can see and control it?
		// -> this is now checked for in OodleTexPub at entry and we refuse to do it
	
		// BC6 is HDR and needs its own activity calculation
		if ( ctx->to_pf == rrPixelFormat_BC6U || ctx->to_pf == rrPixelFormat_BC6S )
		{
			// not required:
			//RR_ASSERT( rrPixelFormat_GetInfo(from_surfaces[i].pixelFormat)->isFloat );
			
			make_activity_mask_hdr_1F(&ctx->activity_surf,*ctx->from_surf);
		}
		else
		{
			// done by OodleTex_Entry_ValidateFormats :
			RR_ASSERT_ALWAYS( ! rrPixelFormat_GetInfo(ctx->from_surf->pixelFormat)->isFloat );
			
			make_activity_mask_1or4F(&ctx->activity_surf,*ctx->from_surf,ctx->mam,ctx->options);
			
			/*
			{
			// write visible activity bmp
			// scale up to be more in [0,255] pixel scale
			rrSurfaceObj activity_save;
			rrSurface_AllocCopy(&activity_save,&activity_surf[i]);
			rrSurface_MultAddF(&activity_save,8.f,0.f);
			rrSurface_Clamp255(&activity_save);
			rrSurface_SaveByName(&activity_save,"r:\\activity.bmp");
			}
			/**/

		}
	}
}

struct activity_context
{
	vector_st<U64,16> activity_handles;
	vector_st<activity_job_context,16> activity_contexts;
	BlockSurface * activity_bs;
	const OodleTex_Layout * layout;
};

static void OODLE_CALLBACK activity_finish_job(void * pdata)
{
	THREADPROFILESCOPE("activity finish");

	// plumbing
	activity_context * args = (activity_context *)pdata;
	vector_st<activity_job_context,16> & activity_contexts = args->activity_contexts;
	BlockSurface * activity_bs = args->activity_bs;
	const OodleTex_Layout * layout = args->layout;

	// actual work
	vector_st<rrSurface,16> activity_surfs;
	int num_surfaces = activity_contexts.size32();
	activity_surfs.resize( num_surfaces );
	for LOOPVEC(i,activity_surfs)
		rrSurface_SetView(&activity_surfs[i],&activity_contexts[i].activity_surf);
		
	BlockSurface_AllocCopy_from_RRS_Layout(activity_bs,activity_surfs.data(),num_surfaces,layout);

	// clean up
	for LOOP(i, activity_contexts.size32())
		rrSurface_Free(&activity_contexts[i].activity_surf);
		
	activity_contexts.release();
}

static OodleJobWaitSet * rrDXTCRD_Context_MakeActivity(
	activity_context * ctx,
	rrPixelFormat to_pf,
	const rrSurface * from_surfaces, int num_surfaces,
	EDXTCRD_Metric metric,EMakeActivityMode mam,rrDXTCOptions options,
	BlockSurface * activity_bs, const OodleTex_Layout * layout,
	void * jobify_user_ptr,int num_workers)
{
	ctx->activity_handles.resize(num_surfaces);
	ctx->activity_handles.memset_zero();
	ctx->activity_contexts.resize(num_surfaces);
	ctx->activity_contexts.memset_zero();

	ctx->activity_bs = activity_bs;
	ctx->layout = layout;

	for ( int i = 0; i < num_surfaces; ++i )
	{
		rrSurface_Init(&ctx->activity_contexts[i].activity_surf);

		ctx->activity_contexts[i].from_surf = &from_surfaces[i];
		ctx->activity_contexts[i].to_pf = to_pf;
		ctx->activity_contexts[i].metric = metric;
		ctx->activity_contexts[i].mam = mam;
		ctx->activity_contexts[i].options = options;

		ctx->activity_handles[i] = OodleJob_Run_MaybeSingleThreaded(activity_job_func,&ctx->activity_contexts[i],NULL,0,jobify_user_ptr,num_workers);
	}

	if (num_workers == OODLEJOB_DISABLE) {
		activity_finish_job(ctx);
		return NULL;
	}

	// create a wait set for all the surface activity jobs
	OodleJobWaitSet * wait_set = OodleJobWaitSet_Create(ctx->activity_handles.data(),num_surfaces,jobify_user_ptr);

	// queue up the finish job right after
	U64 dep = OodleJobWaitSet_RootHandle(wait_set);
	U64 finish_job = OodleJob_Run_MaybeSingleThreaded(activity_finish_job, ctx, &dep, 1, jobify_user_ptr,num_workers);

	// return a wait set for finish job
	return OodleJobWaitSet_CreateChained(wait_set, &finish_job, 1, jobify_user_ptr);
}

rrbool rrDXTCRD_Context_Init(rrDXTCRD_Context * ctx,
				rrPixelFormat to_pf,
				const BlockSurface * from_blocks,
				const rrSurface * from_surfaces,int num_surfaces, const OodleTex_Layout * layout,
				rrDXTCOptions options,void * jobify_user_ptr,int num_workers,
				EDXTCRD_Metric metric,const rrDXTCRD_Options &rdoptions)
{
	SIMPLEPROFILE_SCOPE(RD_Context_Init);
	THREADPROFILESCOPE("RD_Init");

	RR_ASSERT( rrPixelFormat_IsBlockCompressed(to_pf) );

	ctx->options = options;
	ctx->rdoptions = rdoptions;

	// compute activity masks
	// activity mask -> blocks
	// @@ could do that on job threads at same time as the baseline encode

	// make activity in 1F or 4F or both :
	// BC6 and 4/5 want scalar activity
	// BC1 wants 1F for now
	// BC3 wants 1+1
	// BC7 want 4F activity

	// layout must be either not specified, universal, or have the right number of surfaces
	RR_ASSERT( layout == NULL || layout->m_tile_w != 0 || num_surfaces == layout->m_surfaces.size32() );
	RR_ASSERT( num_surfaces >= 1 );

	if ( num_workers == OODLEJOB_DEFAULT )
		num_workers = OodlePlugins_GetJobTargetParallelism();

	activity_context act_ctx;
	OodleJobWaitSet * activity_ws = 0;

	switch(to_pf)
	{
	case rrPixelFormat_BC1:
	case rrPixelFormat_BC4U:
	case rrPixelFormat_BC4S:
	case rrPixelFormat_BC5U:
	case rrPixelFormat_BC5S:
	case rrPixelFormat_BC6U:
	case rrPixelFormat_BC6S:
		activity_ws = rrDXTCRD_Context_MakeActivity(&act_ctx,to_pf,
			from_surfaces,num_surfaces,
			metric,e_make_activity_1F_RGB,options,
			&ctx->activity_blocks_1F,layout,
			jobify_user_ptr,num_workers);
		break;
	case rrPixelFormat_BC7:
	{
		EMakeActivityMode mam;
		mam = e_make_activity_1F_RGBA;
		activity_ws = rrDXTCRD_Context_MakeActivity(&act_ctx,to_pf,
			from_surfaces,num_surfaces,
			metric,mam,options,
			&ctx->activity_blocks_1F,layout,
			jobify_user_ptr,num_workers);
		/*
		switch(metric)
		{
		case eDXTCRD_Metric_RMSE_RGBA:
		case eDXTCRD_Metric_Perceptual_RGBA:
			mam = e_make_activity_4F_RGBA;
			break;
		case eDXTCRD_Metric_Perceptual_RGB_A:
			mam = e_make_activity_4F_RGB_A;
			break;
		case eDXTCRD_Metric_Perceptual_R_G_B_A:
			mam = e_make_activity_4F_R_G_B_A;
			break;
		RR_NO_DEFAULT_CASE
		}
		rrDXTCRD_Context_MakeActivity(&(ctx->activity_blocks_4F),to_pf,from_surfaces,layout,metric,mam,options);
		*/
		break;
	}
	case rrPixelFormat_BC2:
	case rrPixelFormat_BC3: // needs both
	{
		switch(metric)
		{
		case eDXTCRD_Metric_RMSE_RGBA:
		case eDXTCRD_Metric_Perceptual_RGBA:
			// if RGBA, just make one and SetView for the other
			activity_ws = rrDXTCRD_Context_MakeActivity(&act_ctx,to_pf,
				from_surfaces,num_surfaces,
				metric,e_make_activity_1F_RGBA,options,
				&ctx->activity_blocks_1F,layout,
				jobify_user_ptr,num_workers);
			//BlockSurface_SetView(&(ctx->activity_blocks_1F_A),&(ctx->activity_blocks_1F));
			break;
		/*
		case eDXTCRD_Metric_Perceptual_RGB_A:
		case eDXTCRD_Metric_Perceptual_R_G_B_A:
			// separate RGB + A activity , don't mask each other :
			rrDXTCRD_Context_MakeActivity(&(ctx->activity_blocks_1F),to_pf,from_surfaces,layout,metric,e_make_activity_1F_RGB,options);
			rrDXTCRD_Context_MakeActivity(&(ctx->activity_blocks_1F_A),to_pf,from_surfaces,layout,metric,e_make_activity_1F_A,options);
			break;
		*/
		RR_NO_DEFAULT_CASE
		}
		break;
	}
	default:
		RR_ASSERT_FAILURE("not BC?");
		return false;
	}

	BlockSurface_Alloc(&ctx->to_baseline,from_blocks->count,to_pf);

	rrPixelFormat desired_fm_fmt = rrSurfaceDXTC_GetBCNDecompFormat(to_pf);
	{
	SIMPLEPROFILE_SCOPE(bcrd_format_convert);
	THREADPROFILESCOPE("fromblks");
	BlockSurface_AllocCopyOrSetViewIfFormatMatches_Normalized(&ctx->from_blocks_fmt,from_blocks,desired_fm_fmt);
	}

	// fill baseline encoding :
	{
	THREADPROFILESCOPE("baseline");
	const RDOEncodeConfig * cfg = get_rdo_encode_config(to_pf,rdoptions.effort);
	if ( ! rrSurfaceDXTC_CompressBCN_Blocks(&ctx->to_baseline,&ctx->from_blocks_fmt,cfg->baseline_effort,num_workers,options,jobify_user_ptr) )
		return false;
	}

	// wait for activity calc to have completed
	OodleJobWaitSet_WaitAndDestroy(activity_ws);

	return true;
}

void rrDXTCRD_SetDefaults(rrDXTCRD_Options * rdopts)
{
	RR_ZERO(*rdopts);
	// Original default behavior is "high" effort level
	rdopts->effort = rrDXTCLevel_Default;
}

bool rrDXTCRD_Encode_RDO(BlockSurface * to_blocks,const BlockSurface * from_blocks,
				const rrSurface * from_surfaces,int num_from_surfaces, const OodleTex_Layout * layout,
				int lambda,rrDXTCOptions options,void * jobify_user_ptr,int num_workers,EDXTCRD_Metric metric,
				const rrDXTCRD_Options &rdoptions)
{
	// note this is on the outer thread, whereas the others are on the worker threads
	//	so this timer looks 4X shorter
	SIMPLEPROFILE_SCOPE(ERDO);

	// to_blocks should be allocated already
	RR_ASSERT( to_blocks->blocks != NULL );
	rrPixelFormat pf = to_blocks->pixelFormat;
	RR_ASSERT( rrPixelFormat_IsBlockCompressed(pf) );

	rrDXTCRD_Context * ctx = rrDXTCRD_Context_New();

	if ( num_workers == OODLEJOB_DEFAULT )
		num_workers = OodlePlugins_GetJobTargetParallelism();
	
	rrbool ok = rrDXTCRD_Context_Init(ctx,pf,from_blocks,from_surfaces,num_from_surfaces,layout,options,jobify_user_ptr,num_workers,metric,rdoptions);
	if ( ! ok )
	{
		rrDXTCRD_Context_Delete(ctx);
		return false;
	}
	
	// internal code can call with activity mask & baseline coding already done
	
	ok = rrDXTCRD_Encode_RDO_ReadContext(ctx,to_blocks,lambda,jobify_user_ptr,num_workers);

	rrDXTCRD_Context_Delete(ctx);
	
	return !!ok;	
}
		
bool rrDXTCRD_Encode_RDO_LinearSurface(rrSurface * to_surface,
				const rrSurface * from_surface,
				int lambda,rrDXTCOptions options,void * jobify_user_ptr,int num_workers,EDXTCRD_Metric metric,
				const rrDXTCRD_Options &rdoptions)
{
	BlockSurfaceObj to_blocks;
	BlockSurface_SetView_of_RRS_BCN(&to_blocks,to_surface);

	BlockSurfaceObj fm_blocks;
	BlockSurface_AllocCopy_from_RRS(&fm_blocks,from_surface,1);
	
	bool ret = rrDXTCRD_Encode_RDO(&to_blocks,&fm_blocks,from_surface,1,NULL,lambda,options,jobify_user_ptr,num_workers,metric,rdoptions);
	
	return ret;
}				

RR_NAMESPACE_END
