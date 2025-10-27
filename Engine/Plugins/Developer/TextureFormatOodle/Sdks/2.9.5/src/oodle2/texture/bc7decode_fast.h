// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

OODLE_NS_START

// Decode a BC7 block to RGBA8888 pixels. Always writes to a contiguous
// 64-byte block in memory.
void bc7_decode_block_fast(U8 * out_rgba, const void * block);

// Lets you score many block index candidates while holding endpoints constant
struct BC7PredecodedEndpointsBlock
{
	// An EvalErrorFunc splices in indices (and only indices) from another
	// BC7 block (inds_from) then returns the resulting SSD
	typedef U32 EvalErrorFunc(const BC7PredecodedEndpointsBlock * block, SINTa inds_from);
	typedef SINTa FindNextAtMostFunc(const BC7PredecodedEndpointsBlock * block, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh);

	// The exact meaning of these depends on the mode
	RAD_ALIGN(S16, base[64], 32);
	RAD_ALIGN(S16, diff[64], 32);

	// Original bytes passed to init
	RAD_ALIGN(U8, endpt_block[16], 16);
	// Mask for index bits
	RAD_ALIGN(U8, index_mask[16], 16);

	// Exact contents depend on block mode
	U8 * index_cache;
	SINTa index_cache_size; // in bytes

	// Set up by init to eval the error for our active mode (and CPU features)
	EvalErrorFunc * eval;
	FindNextAtMostFunc * find_next_at_most;
	U8 target_mode;
	U8 ignore_alpha;
	U8 partition_id;
	U8 partition_eqv;
	U8 index_swap;
	U8 padding[11]; // to quiet padding warnings

	BC7PredecodedEndpointsBlock();
	~BC7PredecodedEndpointsBlock();

	// Initializes the index cache
	void init_indices(int mode, const U8 * inds_from, SINTa inds_stride, SINTa inds_count);

	// endpts_from is a BC7 block
	// after calling this, you can call eval() to evaluate the error for
	// the combination of endpoints/mode bits etc. from "endpts_from" (passed to
	// init) and indices from "inds_from" (passed to eval)
	void init_endpoints(const U8 * endpts_from, const U8 tgt_pixels_rgba[64], bool ignore_alpha_chan);
};

// Kernels that are defined in other files because they need special compiler settings
namespace bc7decode
{
	// bc7decode_fast_avx2
	void avx2_decode_mode1(U8 * out_rgba, const U8 * block_bits);
	void avx2_decode_mode3(U8 * out_rgba, const U8 * block_bits);
	void avx2_decode_mode4(U8 * out_rgba, const U8 * block_bits);
	void avx2_decode_mode5(U8 * out_rgba, const U8 * block_bits);
	void avx2_decode_mode6(U8 * out_rgba, const U8 * block_bits);
	void avx2_decode_mode7(U8 * out_rgba, const U8 * block_bits);

	U32 endpoints_eval_avx2_twosubset(const BC7PredecodedEndpointsBlock * b, SINTa inds_from);
	SINTa find_next_at_most_avx2_twosubset(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh);

	U32 endpoints_eval_avx2_onesubset_twoind(const BC7PredecodedEndpointsBlock * b, SINTa inds_from);
	SINTa find_next_at_most_avx2_onesubset_twoind(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh);

	U32 endpoints_eval_avx2_mode6(const BC7PredecodedEndpointsBlock * b, SINTa inds_from);
	SINTa find_next_at_most_avx2_mode6(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh);
}

OODLE_NS_END


