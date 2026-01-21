// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "texrtbase.h"

OODLE_NS_START

struct OodleTexRT_BC7PrepHeader;

// ---- Work memory allocation for BC7Prep

// Determine the amount of workspace memory needed by the GPU BC7Prep decoder.
SINTa bc7prep_gpu_min_decode_work_mem_size(SINTa num_blocks);

// Description of the work memory layout
struct BC7PrepGPUWorkInfo
{
	static const int MAX_CASCADE_LEVELS = 3; // for our subsampled histograms.
	// 3 levels is sufficient for BC7PREP_MAXBLOCKS with current parameters:
	// - one scan table column per 512 blocks (=2^9)
	// - each reduce step multiplies this by a factor of 64 (=2^6)
	// - BC7PREP_MAXBLOCKS is 2^27
	// three levels of reduction gives 2^(9+3*6) = 2^27 so 3 is enough.

	U32 scan_base_offs[MAX_CASCADE_LEVELS]; // index into U32 work_mem[] where corresponding subsample level of scan table starts
	U32 scan_table_rows[MAX_CASCADE_LEVELS]; // rows in scan table at that level

	U32 nblocks32; // number of blocks in this job
	int num_levels; // number of cascade levels

	OodleTexRT_Err init(U32 * work_mem_pos, U32 work_mem_max, const OodleTexRT_BC7PrepHeader * header, SINTa output_size, SINTa bc7prep_data_size);
};

// Flags for internal GPU decode functions
enum BC7PrepGPUDecodeFlags
{
	BC7PREP_GPU_FIRST_IN_RUN	= 1<<0,		// used internally, do not set

	BC7PREP_GPU_TEST_NONE		= 0<<8,		// default: not a test
	BC7PREP_GPU_TEST_INITIAL	= 1<<8,		// set to test the initial count pass alone
	BC7PREP_GPU_TEST_REDUCE64	= 2<<8,		// set to test the reduce64 pass alone
	BC7PREP_GPU_TEST_SORT		= 3<<8,		// set to test the full sorting process but not decoding
	BC7PREP_GPU_TEST_MASK		= 3<<8,		// bit mask for tests
};

OODLE_NS_END

