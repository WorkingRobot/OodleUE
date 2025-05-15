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
#include "oodletexpub.h"
#include <atomic>

//#include "../texutil/rrsurfacebyname.h" // @@ TEMP
//#include "rrsurfaceblit.h"

#include "rrsimpleprof.h" // OK to always keep this in, these are super coarse granularity
//#include "rrsimpleprofstub.h"
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
	int lambda,
	rrDXTCOptions options,
	const rrDXTCRD_Options & rdopts)
{
	THREADPROFILESCOPE("RDO");

	switch( to_blocks->pixelFormat )
	{
	case rrPixelFormat_BC1:
		{
			SIMPLEPROFILE_SCOPE_N(bc1rd,from_blocks->count);
			return BC1_RD(to_blocks,from_blocks,baseline_blocks,activity_blocks_1F,lambda,options,rdopts);
		}
		
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
					SIMPLEPROFILE_SCOPE_N(bc4rd,from_blocks->count);
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
				SINTa n = baseline_blocks_alpha.count;
				while(n--)
				{
					RR_PUT64_NATIVE(toPtr, RR_GET64_NATIVE(fmPtr) );
					toPtr += 16;
					fmPtr += 16;
				}
			}

			// Then the color as quasi-BC1
			// BC1 detects that it's writing to BC3 and knows that it's all four-color mode
			{
				SIMPLEPROFILE_SCOPE_N(bc1rd,from_blocks->count);
				if ( ! BC1_RD(&to_blocks_color,from_blocks,&baseline_blocks_color,activity_blocks_1F,lambda,options,rdopts) )
					return false;
			}

			return true;
		}

	case rrPixelFormat_BC4U:
	case rrPixelFormat_BC4S:
	case rrPixelFormat_BC5U:
	case rrPixelFormat_BC5S:
		{
			SIMPLEPROFILE_SCOPE_N(bc4rd,from_blocks->count);
			return BC4_RD(to_blocks,from_blocks,baseline_blocks,activity_blocks_1F,lambda,options,rdopts);
		}
	
	case rrPixelFormat_BC6U:
	case rrPixelFormat_BC6S:
		{
			SIMPLEPROFILE_SCOPE_N(bc6rd,from_blocks->count);
			return BC6_RD(to_blocks,from_blocks,baseline_blocks,activity_blocks_1F,lambda,options,rdopts);
		}

	case rrPixelFormat_BC7:
		{
			SIMPLEPROFILE_SCOPE_N(bc7rd,from_blocks->count);
			return BC7_RD(to_blocks,from_blocks,baseline_blocks,activity_blocks_1F,lambda,options,rdopts);
		}

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
	rrPixelFormat from_desired_format;
	
	const BlockSurface * full_baseline;
	const BlockSurface * full_activity_1F;
	void * jobify_user_ptr;
};

OodleTex_Err rrDXTCRD_Slicer_Func(BlockSurfaceJobSlicer_Data * slice,void * data)
{
	rrDXTCRD_Slicer_Data * pdata = (rrDXTCRD_Slicer_Data *) data;

	if ( g_fp_CancelRequested(pdata->jobify_user_ptr) )
	{
		return OodleTex_Err_Canceled;
	}

	SINTa slice_start = slice->slice_start;
	SINTa slice_count = slice->to_sliced.count;
	
	BlockSurface baseline_slice;
	BlockSurface_SetView(&baseline_slice,pdata->full_baseline,slice_start,slice_count);
	
	BlockSurface activity_slice_1F;
	BlockSurface_SetView(&activity_slice_1F,pdata->full_activity_1F,slice_start,slice_count);
	
	BlockSurfaceObj from_slice;
	BlockSurface_AllocCopyOrSetViewIfFormatMatches_Normalized(&from_slice, &slice->from_sliced, pdata->from_desired_format);
	
	bool ret = rrDXTCRD_Single(&(slice->to_sliced),&from_slice,&baseline_slice,
			&activity_slice_1F,
			pdata->lambda,pdata->options,pdata->rdoptions);
	
	return ret ? OodleTex_Err_OK : OodleTex_Err_Internal;
}

struct rrDXTCRD_Context
{
	BlockSurfaceObj activity_blocks_1F;
	BlockSurfaceObj to_baseline;
	BlockSurfaceObj from_blocks;
	rrPixelFormat from_desired_format;
	rrDXTCOptions options;
	rrDXTCRD_Options rdoptions;
};

OodleTex_Err rrDXTCRD_Encode_RDO_ReadContext_Ex(
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
	RR_ASSERT( to_blocks->count == ctx->from_blocks.count );

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
	data.from_desired_format = ctx->from_desired_format;
	
	data.full_baseline = &ctx->to_baseline;
	data.full_activity_1F = &ctx->activity_blocks_1F;
	data.jobify_user_ptr = jobify_user_ptr;
	
	OodleTex_Err err = BlockSurface_JobSlicer_256K_then_N(to_blocks,&ctx->from_blocks,num_workers,jobify_user_ptr,rrDXTCRD_Slicer_Func,&data,max_slice_blocks);
	
	return err;
}

OodleTex_Err rrDXTCRD_Encode_RDO_ReadContext(
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
	return &ctx->from_blocks;
}

rrDXTCOptions rrDXTCRD_Context_GetOptions(rrDXTCRD_Context * ctx)
{
	return ctx->options;
}

struct activity_context;

struct activity_job_context
{
	// both these surfaces are views (don't own anything)
	rrSurface activity_surf;
	rrSurface from_surf;
	rrPixelFormat to_pf;
	EDXTCRD_Metric metric;
	EMakeActivityMode mam;
	rrDXTCOptions options;
	activity_context * overall_context;
};

struct activity_context
{
	std::atomic<S32> num_outstanding_jobs;
	std::atomic<S32> error_code;

	vector_st<rrSurface,16> activity_surfaces;
	vector_st<U64,16> activity_handles;
	vector_st<activity_job_context,16> activity_contexts;
	BlockSurface * activity_bs;
	const OodleTex_Layout * layout;
	void * jobify_user_ptr;
};

void OODLE_CALLBACK activity_job_func( void * ptr )
{
	activity_job_context * ctx = (activity_job_context *) ptr;
	FPStateScope saved_state; // make sure we're in a defined FP state

	if ( ! g_fp_CancelRequested( ctx->overall_context->jobify_user_ptr ) )
	{
		THREADPROFILESCOPE("activity");
		
		if ( ctx->metric == eDXTCRD_Metric_RMSE_RGBA )
		{
			make_activity_mask_constant_for_rmse_1or4F(&ctx->activity_surf,ctx->from_surf);
		}
		else
		{
			// BC6 takes HDR float data;
			// BC1-5 and 7 now take float data at the public API level (oodletexpub),
			// but we ChangeFormatNormalized to a LDR format before we get here, so
			// those should not see floats and don't need to worry about what the
			// activity calcs does with values outside [0,1]
		
			// BC6 is HDR and needs its own activity calculation
			if ( ctx->to_pf == rrPixelFormat_BC6U || ctx->to_pf == rrPixelFormat_BC6S )
			{
				make_activity_mask_hdr_1F(&ctx->activity_surf,ctx->from_surf,ctx->options);
			}
			else
			{
				// done by OodleTex_Entry_ValidateFormats :
				RR_ASSERT_ALWAYS( ! rrPixelFormat_GetInfo(ctx->from_surf.pixelFormat)->isFloat );
				int channels = 4;

				switch ( ctx->to_pf )
				{
				case rrPixelFormat_BC4U:
				case rrPixelFormat_BC4S:
					channels = 1;
					break;

				case rrPixelFormat_BC5U:
				case rrPixelFormat_BC5S:
					channels = 2;
					break;

				default:
					channels = 4;
					break;
				}
				
				make_activity_mask_1or4F(&ctx->activity_surf,ctx->from_surf,ctx->mam,ctx->options,channels);
				
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
	else
	{
		// Cancellation was requested, note this happened
		// Don't return early, we still need to do the outstanding job tracking and cleanup
		ctx->overall_context->error_code.store(OodleTex_Err_Canceled, std::memory_order_release);
	}

	// Final cleanup tasks
	{
		activity_context * args = ctx->overall_context;
		// Update outstanding job count, bail if we weren't the final job
		// fetch_add returns old value
		if ( args->num_outstanding_jobs.fetch_add(-1, std::memory_order_acq_rel) == 1 ) 
		{
			// We're the final job, do the actual finish
			THREADPROFILESCOPE("activity finish");

			// Actual work; skip if we hit an error earlier.
			if ( args->error_code.load(std::memory_order_acquire) == OodleTex_Err_OK )
			{
				BlockSurface_AllocCopy_from_RRS_Layout(args->activity_bs,args->activity_surfaces.data(),args->activity_surfaces.size32(),args->layout);
			}

			// clean up
			for (rrSurface & surf : args->activity_surfaces)
				rrSurface_Free(&surf);

			args->activity_contexts.release();
		}
	}
}

static OodleJobWaitSet * rrDXTCRD_Context_MakeActivity(
	activity_context * ctx,
	rrPixelFormat to_pf,
	const rrSurface * from_surfaces, int num_surfaces,
	EDXTCRD_Metric metric,EMakeActivityMode mam,rrDXTCOptions options,
	BlockSurface * activity_bs, const OodleTex_Layout * layout,
	void * jobify_user_ptr,int num_workers)
{
	constexpr S32 activity_tile_width = 256;
	constexpr S32 activity_tile_height = 256;
	SINTa num_total_jobs = 0;
	rrPixelFormat activity_pf = EMakeActivityMode_to_PF(mam);

	// Set up the destination activity surfaces
	ctx->activity_surfaces.reserve(num_surfaces);
	for LOOP(i, num_surfaces)
	{
		rrSurface surf = {};
		rrSurface_Alloc(&surf, from_surfaces[i].width, from_surfaces[i].height, activity_pf);
		ctx->activity_surfaces.push_back(surf);

		SINTa width_in_tiles = (from_surfaces[i].width + activity_tile_width - 1) / activity_tile_width;
		SINTa height_in_tiles = (from_surfaces[i].height + activity_tile_height - 1) / activity_tile_height;
		num_total_jobs += width_in_tiles * height_in_tiles;
	}

	ctx->activity_handles.resize(num_total_jobs);
	ctx->activity_handles.memset_zero();
	ctx->activity_contexts.resize(num_total_jobs);
	ctx->activity_contexts.memset_zero();

	ctx->activity_bs = activity_bs;
	ctx->layout = layout;
	ctx->jobify_user_ptr = jobify_user_ptr;

	// Log the number of outstanding jobs
	ctx->num_outstanding_jobs.store(check_value_cast<S32>(num_total_jobs), std::memory_order_release);
	ctx->error_code.store(OodleTex_Err_OK, std::memory_order_release);

	// Determine whether to spawn activity jobs.
	// If we have multiple surfaces to process, always use jobs.
	// If we have just one surface with a small total pixel count, prefer to compute it blocking
	// on calling thread to avoid job dispatch overhead.
	bool should_spawn_activity_jobs = true;
	constexpr S64 min_pixel_count_for_jobs = 4096;

	if ( num_surfaces == 1 && S64(from_surfaces[0].width) * from_surfaces[0].height < min_pixel_count_for_jobs )
		should_spawn_activity_jobs = false;

	const int effective_num_workers = should_spawn_activity_jobs ? num_workers : OODLEJOB_DISABLE;
	bool any_jobs_spawned = false;
	SINTa job_id = 0;

	for ( int i = 0; i < num_surfaces; ++i )
	{
		// Set up one job for every tile
		for (SINTa ty0 = 0; ty0 < from_surfaces[i].height; ty0 += activity_tile_height)
		{
			SINTa ty1 = RR_MIN(ty0 + activity_tile_height, from_surfaces[i].height);

			for (SINTa tx0 = 0; tx0 < from_surfaces[i].width; tx0 += activity_tile_width)
			{
				SINTa tx1 = RR_MIN(tx0 + activity_tile_width, from_surfaces[i].width);
				activity_job_context* job = &ctx->activity_contexts[job_id];

				rrSurface_SetView(&job->activity_surf, &ctx->activity_surfaces[i], tx0, ty0, tx1 - tx0, ty1 - ty0);
				rrSurface_SetView(&job->from_surf, &from_surfaces[i], tx0, ty0, tx1 - tx0, ty1 - ty0);
				job->to_pf = to_pf;
				job->metric = metric;
				job->mam = mam;
				job->options = options;
				job->overall_context = ctx;

				ctx->activity_handles[job_id] = OodleJob_Run_MaybeSingleThreaded(activity_job_func,job,NULL,0,jobify_user_ptr,effective_num_workers);
				if ( ctx->activity_handles[job_id] != 0 )
					any_jobs_spawned = true;

				++job_id;
			}
		}
	}

	RR_ASSERT_ALWAYS(job_id == num_total_jobs);

	// If we didn't spawn any actual jobs, the work is already done.
	if ( ! any_jobs_spawned )
		return NULL;

	// create a wait set for all the surface activity jobs
	OodleJobWaitSet * wait_set = OodleJobWaitSet_Create(ctx->activity_handles.data(),(S32)num_total_jobs,jobify_user_ptr);
	return wait_set;
}

OodleTex_Err rrDXTCRD_Context_Init(rrDXTCRD_Context * ctx,
				rrPixelFormat to_pf,
				const BlockSurface * from_blocks,
				const rrSurface * from_surfaces,int num_surfaces, const OodleTex_Layout * layout,
				rrDXTCOptions options,void * jobify_user_ptr,int num_workers,
				EDXTCRD_Metric metric,const rrDXTCRD_Options &rdoptions)
{
	//SIMPLEPROFILE_SCOPE(RD_Context_Init);
	THREADPROFILESCOPE("RD_Init");

	RR_ASSERT( rrPixelFormat_IsBlockCompressed(to_pf) );

	ctx->options = options;
	ctx->rdoptions = rdoptions;

	// compute activity masks
	// activity mask -> blocks

	// make activity:
	// BC1, 4/5, 6 use scalar activity for RGB channels (really just R and RG for BC4/5, respectively)
	// BC2, BC3, BC7 use scalar activity for RGBA channels

	// layout must be either not specified, universal, or have the right number of surfaces
	RR_ASSERT( layout == NULL || layout->m_tile_w != 0 || num_surfaces == layout->m_surfaces.size32() );
	RR_ASSERT( num_surfaces >= 1 );

	if ( num_workers == OODLEJOB_DEFAULT )
		num_workers = OodlePlugins_GetJobTargetParallelism();

	// If we have too few blocks, don't bother using jobs for the activity setup, it's not worth it
	if ( from_blocks->count <= BLOCKSURFACE_JOBSLICER_MIN_BLOCKS )
		num_workers = OODLEJOB_DISABLE;

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
	case rrPixelFormat_BC2:
	case rrPixelFormat_BC3: // needs both
	case rrPixelFormat_BC7:
		activity_ws = rrDXTCRD_Context_MakeActivity(&act_ctx,to_pf,
			from_surfaces,num_surfaces,
			metric,e_make_activity_1F_RGBA,options,
			&ctx->activity_blocks_1F,layout,
			jobify_user_ptr,num_workers);
		break;
	default:
		RR_ASSERT_FAILURE("not BC?");
		return OodleTex_Err_Internal;
	}

	BlockSurface_Alloc(&ctx->to_baseline,from_blocks->count,to_pf);

	ctx->from_desired_format = rrSurfaceDXTC_GetBCNDecompFormat(to_pf);
	BlockSurface_SetView(&ctx->from_blocks, from_blocks);

	OodleTex_Err err = OodleTex_Err_OK;

	// fill baseline encoding :
	{
		THREADPROFILESCOPE("baseline");
		const RDOEncodeConfig * cfg = get_rdo_encode_config(to_pf,rdoptions.effort);
		err = rrSurfaceDXTC_CompressBCN_Blocks(&ctx->to_baseline,&ctx->from_blocks,cfg->baseline_effort,num_workers,options,jobify_user_ptr);
	}

	// wait for activity calc to have completed
	OodleJobWaitSet_WaitAndDestroy(activity_ws);

	// Check for errors during activity calc.
	// Activity calc is logically after baseline compress, so only report
	// error from activity if baseline encode was error-free.
	if ( err == OodleTex_Err_OK )
	{
		err = (OodleTex_Err) act_ctx.error_code.load(std::memory_order_acquire);
	}

	//rrprintf("activity debug_hash=0x%llx\n", (long long)BlockSurface_DebugHash(&ctx->activity_blocks_1F));

	return err;
}

void rrDXTCRD_SetDefaults(rrDXTCRD_Options * rdopts)
{
	RR_ZERO(*rdopts);
	// Original default behavior is "high" effort level
	rdopts->effort = rrDXTCLevel_Default;
}

OodleTex_Err rrDXTCRD_Encode_RDO(BlockSurface * to_blocks,const BlockSurface * from_blocks,
				const rrSurface * from_surfaces,int num_from_surfaces, const OodleTex_Layout * layout,
				int lambda,rrDXTCOptions options,void * jobify_user_ptr,int num_workers,EDXTCRD_Metric metric,
				const rrDXTCRD_Options &rdoptions)
{
	// to_blocks should be allocated already
	RR_ASSERT( to_blocks->blocks != NULL );
	rrPixelFormat pf = to_blocks->pixelFormat;
	RR_ASSERT( rrPixelFormat_IsBlockCompressed(pf) );

	rrDXTCRD_Context * ctx = rrDXTCRD_Context_New();

	if ( num_workers == OODLEJOB_DEFAULT )
		num_workers = OodlePlugins_GetJobTargetParallelism();
	
	OodleTex_Err err = rrDXTCRD_Context_Init(ctx,pf,from_blocks,from_surfaces,num_from_surfaces,layout,options,jobify_user_ptr,num_workers,metric,rdoptions);

	// internal code can call with activity mask & baseline coding already done

	if ( err == OodleTex_Err_OK )
	{
		err = rrDXTCRD_Encode_RDO_ReadContext(ctx,to_blocks,lambda,jobify_user_ptr,num_workers);
	}

	rrDXTCRD_Context_Delete(ctx);
	
	return err;
}
		
OodleTex_Err rrDXTCRD_Encode_RDO_LinearSurface(rrSurface * to_surface,
				const rrSurface * from_surface,
				int lambda,rrDXTCOptions options,void * jobify_user_ptr,int num_workers,EDXTCRD_Metric metric,
				const rrDXTCRD_Options &rdoptions)
{
	BlockSurfaceObj to_blocks;
	BlockSurface_SetView_of_RRS_BCN(&to_blocks,to_surface);

	BlockSurfaceObj fm_blocks;
	BlockSurface_AllocCopy_from_RRS(&fm_blocks,from_surface,1);
	
	OodleTex_Err err = rrDXTCRD_Encode_RDO(&to_blocks,&fm_blocks,from_surface,1,NULL,lambda,options,jobify_user_ptr,num_workers,metric,rdoptions);
	
	return err;
}				

RR_NAMESPACE_END
