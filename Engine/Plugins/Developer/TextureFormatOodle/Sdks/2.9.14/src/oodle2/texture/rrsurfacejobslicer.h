// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrbase.h"
#include "texbase.h"
#include "rrsurface.h"
#include "blocksurface.h"

/***

JobSlicer cuts image into horizontal slices
and runs a func on the Job system on each slice

you make a t_JobSlicer_Func function
you are called back with info about your slice
+ your user data pointer

use like :

void rrSurfaceDXTC_CompressBCN_JobSlicer_Func(rrSurfaceJobSlicer_Data * slice,void * data)

return rrSurface_JobSlicer(to,from,num_workers,jobify_user_ptr,rrSurfaceDXTC_CompressBCN_JobSlicer_Func,&data);

**/

RR_NAMESPACE_START

struct rrSurfaceJobSlicer_Data;
typedef OodleTex_Err (t_JobSlicer_Func)(rrSurfaceJobSlicer_Data * slice,void * data);

struct rrSurfaceJobSlicer_Data
{
	// pad to avoid cache line sharing :
	U8 pad[RR_CACHE_LINE_SIZE];
	
	// in:
	rrSurface to_sliced;
	rrSurface from_sliced;
	
	rrSurface * to_orig;
	const rrSurface * from_orig;
		
	// out:
	U64 handle;
	OodleTex_Err func_ret;
	
	t_JobSlicer_Func * pfunc;
	void * pdata;
};

OodleTex_Err rrSurface_JobSlicer(rrSurface * to, const rrSurface * from,int num_workers,void * jobify_user_ptr,t_JobSlicer_Func * pfunc,void * pdata);
OodleTex_Err rrSurface_JobSlicer_Rectangles(rrSurface * to, const rrSurface * from,SINTa rect_w,SINTa rect_h,int num_workers,void * jobify_user_ptr,t_JobSlicer_Func * pfunc,void * pdata);

//================================================

struct BlockSurfaceJobSlicer_Data;
typedef OodleTex_Err (t_BlockJobSlicer_Func)(BlockSurfaceJobSlicer_Data * slice,void * data);

struct BlockSurfaceJobSlicer_Data
{
	// pad to avoid cache line sharing :
	U8 pad[RR_CACHE_LINE_SIZE];
	
	// in:
	BlockSurface to_sliced;
	BlockSurface from_sliced;
	
	SINTa slice_start;
	
	// out:
	U64 handle;
	OodleTex_Err func_ret;
	
	t_BlockJobSlicer_Func * pfunc;
	void * pdata;
};

// Don't bother putting less than this many blocks in their own job.
// 256 is good for 64x64 pixels for BCn 4x4 pixel blocks.
//
// This used to be 32 but that was a bit on the low side.
#define BLOCKSURFACE_JOBSLICER_MIN_BLOCKS 256

OodleTex_Err BlockSurface_JobSlicer(BlockSurface * to, const BlockSurface * from,int num_workers,void * jobify_user_ptr,t_BlockJobSlicer_Func * pfunc,void * pdata);

OodleTex_Err BlockSurface_JobSlicer_256K_then_N(BlockSurface * to, const BlockSurface * from,int num_workers,void * jobify_user_ptr,t_BlockJobSlicer_Func * pfunc,void * pdata,
											SINTa max_num_blocks_per_slice);
											
//================================================


RR_NAMESPACE_END
