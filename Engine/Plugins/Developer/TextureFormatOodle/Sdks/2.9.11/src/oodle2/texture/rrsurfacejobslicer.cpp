// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrsurfacejobslicer.h"
#include "threadprofiler.h"
#include "oodlejob.h"
#include "templates/rrvector.h"
#include "templates/rrvector_st.h"
#include "rrstackarray.h"
#include "texbase.h"
#include <limits.h>

RR_NAMESPACE_START

//=================================================================================================

// a * b / denom, with extra range
//
// Preconditions:
//   0 < denom < 2^31
//   0 <= a <= denom
//   0 <= b  (might be large)
static SINTa job_mul_div(SINTa a, SINTa b, SINTa denom)
{
	// We'd rather assert than overflow here
	RR_ASSERT_ALWAYS(0 < denom && denom <= 0x7fffffff);
	RR_ASSERT_ALWAYS(0 <= a && a <= denom);
	RR_ASSERT_ALWAYS(0 <= b);

	// Decompose b = bq*denom + br
	SINTa bq = b / denom;
	SINTa br = b % denom;

	// Now floor((a * b) / denom)
	// =floor((a * (bq*denom + br)) / denom)
	// =floor((a*bq*denom + a*br)/denom)
	// =floor(a*bq + (a*br)/denom)
	// =a*bq + floor((a*br) / denom)
	S64 first_term = S64(a) * bq; // =a * floor(b/denom) <= a*b/denom = b*(a/denom) <= b, so no overflow
	S64 second_term = (S64(a) * br) / denom; // a*br < denom*denom since a<=denom and br<denom; denom<2^31 so product is <2^62 (no overflow) and final quotient <denom.
	S64 result = first_term + second_term; // overall, we compute floor((a * b) / denom) <= b, so no overflow

	return (SINTa)result;
}

SINTa get_job_count_for_worker_count(SINTa num_workers, SINTa num_items, SINTa min_items )
{
	RR_ASSERT(num_workers != OODLEJOB_DEFAULT);
	if ( num_workers <= 1 ) return 1; // includes OODLEJOBS_DISABLE
	if ( num_items <= min_items ) return 1;
	
	// scale up num_jobs so we get several per worker
	// finer split gives better load balance sometimes
	
	// doing more rounds per worker reduces the chance of the workers finishing at different times
	//	what you hate to see is :
	//	you have 16 workers
	//	15 finish up just as the 16th is popping its last item
	//	so you have one whole item duration where 15 threads are stalled
		
	if ( num_items >= num_workers * min_items * 8 )
	{
		// -1 is kinda nice
		//	it reduces the chance that one thread who was slightly slower blocks completion
		// what happens is all the workers do 4 rounds of work
		// then everybody does 1 more round, *except* the one that was last to the pop parade
		//	he gets to just chill a little bit
		return num_workers * 6 - 1;
	}
	else if ( num_items >= num_workers * min_items * 5 ) // *5
	{
		return num_workers * 4 - 1;
	}
	else if ( num_items >= num_workers * min_items * 3 )
	{
		return num_workers * 3;
	}
	else if ( num_items >= num_workers * min_items * 2 )
	{
		return num_workers * 2;
	}
	else if ( num_items >= num_workers * min_items )
	{
		return num_workers;
	}

	// limited by min_items :
	// round DOWN so items per job is strictly >= min_items
	SINTa num_jobs = num_items / min_items;
	
	RR_ASSERT( (num_items / num_jobs) >= min_items ); 
	RR_ASSERT( num_jobs >= 1 && num_jobs <= num_workers );
	
	return num_jobs;
}

void OODLE_CALLBACK rrSurfaceJobSlicer_Job( void * pdata)
{
	FPStateScope saved_state; // make sure we're in a defined FP state
	rrSurfaceJobSlicer_Data * d = (rrSurfaceJobSlicer_Data *) pdata;
		
	THREADPROFILESCOPE2("JobSlicer_Job", d->handle);
	
	t_JobSlicer_Func * func = d->pfunc;
	rrbool ret = (*func)(d,d->pdata);
	
	d->func_ret = ret;
}

rrbool rrSurface_JobSlicer(rrSurface * to, const rrSurface * from,int num_workers,void * jobify_user_ptr,t_JobSlicer_Func * pfunc,void * pdata)
{
	SINTa num_block_rows = (from->height+3)/4;
	
	if ( num_workers == OODLEJOB_DEFAULT ) num_workers = OodlePlugins_GetJobTargetParallelism();
	
	if ( num_block_rows < num_workers || num_workers <= 1 )
	{
		rrSurfaceJobSlicer_Data single_slice;
		
		single_slice.from_orig = from;
		single_slice.to_orig = to;
		
		rrSurface_SetView(&(single_slice.from_sliced),from);
		rrSurface_SetView(&(single_slice.to_sliced),to);	
		
		single_slice.pfunc = pfunc;
		single_slice.pdata = pdata;
		
		rrSurfaceJobSlicer_Job(&single_slice);
		
		return single_slice.func_ret;
	}
	
	SINTa num_jobs = get_job_count_for_worker_count(num_workers,num_block_rows,2);
	//while( (num_block_rows/num_jobs) > 8 ) num_jobs += num_workers;
	
	vector_st<rrSurfaceJobSlicer_Data,16> job_data;
	vector_st<U64,16> job_handles;
	job_data.resize(num_jobs);
	job_handles.resize(num_jobs);
	
	SINTa last_block_y = 0;
	
	for LOOP(j,num_jobs)
	{
		SINTa to_block_y = job_mul_div(j + 1, num_block_rows, num_jobs);
		
		if ( j == num_jobs-1 )
			RR_ASSERT( to_block_y == num_block_rows );
		
		// start with success
		job_data[j].func_ret = true;
		
		if ( to_block_y == last_block_y )
		{
			// happens on tiny images, should just not jobify them
			job_data[j].handle = 0;
			//job_data[j].ret = true;
			continue;
		}
		
		SINTa y = last_block_y*4;
		SINTa h = (to_block_y - last_block_y)*4;
		
		if ( y+h > from->height )
		{
			RR_ASSERT( y+h - from->height <= 3 );
			h = from->height - y;
		}
		
		rrSurface_SetView( &(job_data[j].to_sliced), to, 0, y, -1, h );
		rrSurface_SetView( &(job_data[j].from_sliced), from, 0, y, -1, h );
		
		job_data[j].to_orig = to;
		job_data[j].from_orig = from;
		
		job_data[j].pfunc = pfunc;
		job_data[j].pdata = pdata;
		
		last_block_y = to_block_y;
		
		job_data[j].handle = OodleJob_Run_MaybeSingleThreaded(rrSurfaceJobSlicer_Job, &(job_data[j]), 0,0, jobify_user_ptr, num_workers );
		job_handles[j] = job_data[j].handle;
	}
		
	if ( num_workers != OODLEJOB_DISABLE)
		OodleJob_WaitAll(job_handles.data(),num_jobs,jobify_user_ptr);
	
	rrbool ret = true;
	for LOOP(j,num_jobs)
	{
		if ( ! job_data[j].func_ret )
			ret = false;
	}
	
	return ret;
}

rrbool rrSurface_JobSlicer_Rectangles(rrSurface * to, const rrSurface * from,SINTa rect_w,SINTa rect_h,int num_workers,void * jobify_user_ptr,t_JobSlicer_Func * pfunc,void * pdata)
{
	SINTa num_cols = (from->width + rect_w-1) / rect_w;
	SINTa num_rows = (from->height + rect_h-1) / rect_h;
	
	if ( num_cols*num_rows == 1 )
	{
		rrSurfaceJobSlicer_Data single_slice;
		
		single_slice.from_orig = from;
		single_slice.to_orig = to;
		
		rrSurface_SetView(&(single_slice.from_sliced),from);
		rrSurface_SetView(&(single_slice.to_sliced),to);	
		
		single_slice.pfunc = pfunc;
		single_slice.pdata = pdata;
		
		rrSurfaceJobSlicer_Job(&single_slice);
		
		return single_slice.func_ret;
	}
	
	SINTa num_jobs = num_cols * num_rows;
	vector_st<rrSurfaceJobSlicer_Data,16> job_data;
	job_data.resize(num_jobs);

	{
	SINTa j = 0;
	for LOOP(r,num_rows)
	{
		for LOOP(c,num_cols)
		{
			SINTa x = c * rect_w;
			SINTa y = r * rect_h;
			
			SINTa w = RR_MIN(rect_w, from->width - x);
			SINTa h = RR_MIN(rect_h, from->height - y);
	
			RR_ASSERT( w > 0 && h > 0 );
	
			rrSurface_SetView( &(job_data[j].to_sliced), to, x, y, w, h );
			rrSurface_SetView( &(job_data[j].from_sliced), from, x, y, w, h );
			
			// start with success
			job_data[j].func_ret = true;
		
			job_data[j].to_orig = to;
			job_data[j].from_orig = from;
			
			job_data[j].pfunc = pfunc;
			job_data[j].pdata = pdata;
			
			job_data[j].handle = OodleJob_Run_MaybeSingleThreaded(rrSurfaceJobSlicer_Job, &(job_data[j]), 0,0, jobify_user_ptr, num_workers );
			j++;
		}
	}
	RR_ASSERT( j == num_jobs );
	}
		
	//U64 job_handles[MAX_NUM_JOBS];
	RR_STACK_ARRAY(job_handles,U64,num_jobs);
	
	for LOOP(j,num_jobs)
	{
		job_handles[j] = job_data[j].handle;
	}
	
	if ( num_workers != OODLEJOB_DISABLE)
		OodleJob_WaitAll(job_handles,num_jobs,jobify_user_ptr);
	
	rrbool ret = true;
	for LOOP(j,num_jobs)
	{
		if ( ! job_data[j].func_ret )
			ret = false;
	}
	
	return ret;
}

//=================================================================================================

void OODLE_CALLBACK BlockSurfaceJobSlicer_Job( void * pdata)
{
	FPStateScope saved_state; // make sure we're in a defined FP state
	BlockSurfaceJobSlicer_Data * d = (BlockSurfaceJobSlicer_Data *) pdata;
		
	THREADPROFILESCOPE2("BlockSurfJob", d->handle);
	
	t_BlockJobSlicer_Func * func = d->pfunc;
	rrbool ret = (*func)(d,d->pdata);
	
	d->func_ret = ret;
}

rrbool BlockSurface_JobSlicer(BlockSurface * to, const BlockSurface * from,int num_workers,void * jobify_user_ptr,t_BlockJobSlicer_Func * pfunc,void * pdata)
{
	RR_ASSERT( to->count == from->count );

	if ( num_workers == OODLEJOB_DEFAULT )
		num_workers = OodlePlugins_GetJobTargetParallelism();

	SINTa count = from->count;
	
	SINTa num_jobs = get_job_count_for_worker_count(num_workers,count,BLOCKSURFACE_JOBSLICER_MIN_BLOCKS);
	RR_ASSERT( num_jobs > 0 );

	vector_st<BlockSurfaceJobSlicer_Data,16> job_data;
	job_data.resize(num_jobs);

	SINTa last_count = 0;
	
	for LOOP(j,num_jobs)
	{
		SINTa to_count = job_mul_div(j + 1, count, num_jobs);
		
		// start with success
		job_data[j].func_ret = true;
		
		RR_ASSERT( to_count != last_count );
		
		SINTa start = last_count;
		SINTa cur_count = to_count - last_count;
		
		BlockSurface_SetView( &(job_data[j].to_sliced), to, start, cur_count );
		BlockSurface_SetView( &(job_data[j].from_sliced), from, start, cur_count );
		
		job_data[j].slice_start = start;
		
		job_data[j].pfunc = pfunc;
		job_data[j].pdata = pdata;
		
		last_count = to_count;
	
		// Always run the final job immediately on the current thread, there's no reason not to.
		// Our very next step would be to wait anyway. This also makes us not hand out jobs
		// to other threads when we only have one.
		int use_num_workers = (j == num_jobs - 1) ? OODLEJOB_DISABLE : num_workers;
		job_data[j].handle = OodleJob_Run_MaybeSingleThreaded(BlockSurfaceJobSlicer_Job, &(job_data[j]), 0,0, jobify_user_ptr, use_num_workers );
	}
		
	RR_ASSERT( last_count == count );
		
	RR_STACK_ARRAY(job_handles,U64,num_jobs);
	
	for LOOP(j,num_jobs)
	{
		job_handles[j] = job_data[j].handle;
	}
	
	if ( num_workers != OODLEJOB_DISABLE)
		OodleJob_WaitAll(job_handles,num_jobs,jobify_user_ptr);
	
	rrbool ret = true;
	for LOOP(j,num_jobs)
	{
		if ( ! job_data[j].func_ret )
			ret = false;
	}
	
	return ret;
}

rrbool BlockSurface_JobSlicer_256K_then_N(BlockSurface * to, const BlockSurface * from,int num_workers,void * jobify_user_ptr,t_BlockJobSlicer_Func * pfunc,void * pdata,
											SINTa goal_num_blocks_per_slice)
{
	RR_ASSERT( to->count == from->count );

	if ( num_workers == OODLEJOB_DEFAULT )
		num_workers = OodlePlugins_GetJobTargetParallelism();

	// cut chunks at 256 KB byte boundaries in the [to] block format
	//	then within those chunks, cut up to goal_num_blocks_per_slice

	// this job slicing does NOT look at core count
	//	because it affects compression, we want it to be platform agnostic
	// we always cut at 256 KB chunk boundaries and then 4 slices within each chunk
	// 256 KB chunk = 32 K blocks for BC1 , 16 K blocks for BC7

	SINTa bypb = to->blockSizeBytes;
	SINTa blocks_per_256K = (256*1024)/bypb;

	vector<BlockSurfaceJobSlicer_Data> job_data;
	
	SINTa from_count = from->count;
	SINTa start_block = 0;

	for(;;)	
	{
		SINTa blocks_left = from_count - start_block;
		if ( blocks_left == 0 )
			break;
		
		SINTa blocks_this_chunk = RR_MIN(blocks_left,blocks_per_256K);
		
		// odd block number split policy? (when we don't have a full 256K chunk that splits evenly)
		
		// if it's a full 32K (BC1) into 8K slices, I want 4
		// if it's partial, say 26K blocks, do I still want 4 smaller (6.5K)? or 3 that are a bit bigger than target (8.6) ?
		//int slices_this_chunk = 1;
		//while( (blocks_this_chunk/slices_this_chunk) > goal_num_blocks_per_slice )
		//	slices_this_chunk++;
		// this iteration is just the same as rounding up slices :
		//RR_ASSERT( slices_this_chunk == (blocks_this_chunk + goal_num_blocks_per_slice-1)/goal_num_blocks_per_slice );

		// just round instead? :
		//	(this means slices can be a bit more than goal_num_blocks_per_slice)
		// if you just round, it means you can be 50% over target
		//int slices_this_chunk = (blocks_this_chunk + (goal_num_blocks_per_slice/2))/goal_num_blocks_per_slice;
		// only allow 25% over target :
		// eg. if target is 4k , allow 5k rather than going to 2.5/2.5
		SINTa slices_this_chunk = (blocks_this_chunk + (goal_num_blocks_per_slice*3/4))/goal_num_blocks_per_slice;
		slices_this_chunk = RR_MAX(slices_this_chunk,1);
		
		SINTa start_of_chunk = start_block;
		
		for LOOP(i,slices_this_chunk)
		{
			SINTa to_count = start_of_chunk + job_mul_div(i + 1, blocks_this_chunk, slices_this_chunk);
			
			SINTa j = job_data.sizea();
			job_data.push_back();
			
			// start with success
			job_data[j].func_ret = true;
			
			RR_ASSERT( to_count != start_block );
			
			SINTa start = start_block;
			SINTa cur_count = to_count - start_block;
			
			BlockSurface_SetView( &(job_data[j].to_sliced), to, start, cur_count );
			BlockSurface_SetView( &(job_data[j].from_sliced), from, start, cur_count );
			
			job_data[j].slice_start = start;
			
			job_data[j].pfunc = pfunc;
			job_data[j].pdata = pdata;
			
			start_block = to_count;
			
			// don't run job yet, need vector to finish resizing
		}
		
		RR_ASSERT( start_block == start_of_chunk + blocks_this_chunk );
	}
	
	RR_ASSERT( start_block == from_count );
		
	SINTa num_jobs = job_data.sizea();
					
	RR_STACK_ARRAY(job_handles,U64,num_jobs);
		
	for LOOP(j,num_jobs)
	{
		// Always run the final job immediately on the current thread, there's no reason not to.
		// Our very next step would be to wait anyway. This also makes us not hand out jobs
		// to other threads when we only have one.
		int use_num_workers = (j == num_jobs - 1) ? OODLEJOB_DISABLE : num_workers;
		U64 h = OodleJob_Run_MaybeSingleThreaded(BlockSurfaceJobSlicer_Job, &(job_data[j]), 0,0, jobify_user_ptr, use_num_workers );
		job_handles[j] = job_data[j].handle = h;
	}

	if ( num_workers != OODLEJOB_DISABLE)
		OodleJob_WaitAll(job_handles,num_jobs,jobify_user_ptr);
	
	rrbool ret = true;
	for LOOP(j,num_jobs)
	{
		if ( ! job_data[j].func_ret )
			ret = false;
	}
	
	return ret;
}

RR_NAMESPACE_END

