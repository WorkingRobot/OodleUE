// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrbase.h"
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
typedef rrbool (t_JobSlicer_Func)(rrSurfaceJobSlicer_Data * slice,void * data);

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
	rrbool func_ret;
	
	t_JobSlicer_Func * pfunc;
	void * pdata;
};

rrbool rrSurface_JobSlicer(rrSurface * to, const rrSurface * from,int num_workers,void * jobify_user_ptr,t_JobSlicer_Func * pfunc,void * pdata);
rrbool rrSurface_JobSlicer_Rectangles(rrSurface * to, const rrSurface * from,int rect_w,int rect_h,int num_workers,void * jobify_user_ptr,t_JobSlicer_Func * pfunc,void * pdata);

//================================================

struct BlockSurfaceJobSlicer_Data;
typedef rrbool (t_BlockJobSlicer_Func)(BlockSurfaceJobSlicer_Data * slice,void * data);

struct BlockSurfaceJobSlicer_Data
{
	// pad to avoid cache line sharing :
	U8 pad[RR_CACHE_LINE_SIZE];
	
	// in:
	BlockSurface to_sliced;
	BlockSurface from_sliced;
	
	int slice_start;
	
	// out:
	U64 handle;
	rrbool func_ret;
	
	t_BlockJobSlicer_Func * pfunc;
	void * pdata;
};

rrbool BlockSurface_JobSlicer(BlockSurface * to, const BlockSurface * from,int num_workers,void * jobify_user_ptr,t_BlockJobSlicer_Func * pfunc,void * pdata);

rrbool BlockSurface_JobSlicer_256K_then_N(BlockSurface * to, const BlockSurface * from,int num_workers,void * jobify_user_ptr,t_BlockJobSlicer_Func * pfunc,void * pdata,
											int max_num_blocks_per_slice);
											
//================================================


RR_NAMESPACE_END
