// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "cpux86.h"
#include "rrbits.h"

OODLE_NS_START

// Prototype for a generic BC7 block decoder func
typedef void BC7BlockDecoder(U8 * out_rgba, const U8 * block_bits, bool ignore_alpha);

namespace internal {
	extern BC7BlockDecoder * const g_bc7_decoders_avx2[9];
	extern BC7BlockDecoder * const g_bc7_decoders_baseline[9];

	void bc7_decode_verify(const U8 * decoded, const void * block, bool ignore_alpha);
}

// Looks up the BC7 block decoder for a given mode
RADFORCEINLINE BC7BlockDecoder * const bc7_lookup_block_decoder(U32 mode)
{
#ifdef DO_BUILD_AVX2
	if ( rrCPUx86_feature_present(RRX86_CPU_AVX2) )
	{
		return internal::g_bc7_decoders_avx2[mode];
	}
#endif

	return internal::g_bc7_decoders_baseline[mode];
}

// Decode a BC7 block to RGBA8888 pixels. Always writes to a contiguous
// 64-byte block in memory.
//
// When ignore_alpha is set, the output alpha channel is written
// as all-255.
RADFORCEINLINE void bc7_decode_block_fast(U8 * out_rgba, const void * block, bool ignore_alpha)
{
	const U8 * block_bytes = (const U8 *)block;
	U32 mode = rrCtz32(block_bytes[0] | 0x100); // gives 8 for illegal mode
	BC7BlockDecoder * const decoder = bc7_lookup_block_decoder(mode);

	decoder(out_rgba, block_bytes, ignore_alpha);
	//internal::bc7_decode_verify(out_rgba, block_bytes, ignore_alpha); // un-comment for debug
}

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
	void avx2_decode_mode1(U8 * out_rgba, const U8 * block_bits, bool ignore_alpha);
	void avx2_decode_mode3(U8 * out_rgba, const U8 * block_bits, bool ignore_alpha);
	void avx2_decode_mode4(U8 * out_rgba, const U8 * block_bits, bool ignore_alpha);
	void avx2_decode_mode5(U8 * out_rgba, const U8 * block_bits, bool ignore_alpha);
	void avx2_decode_mode6(U8 * out_rgba, const U8 * block_bits, bool ignore_alpha);
	void avx2_decode_mode7(U8 * out_rgba, const U8 * block_bits, bool ignore_alpha);

	U32 endpoints_eval_avx2_twosubset(const BC7PredecodedEndpointsBlock * b, SINTa inds_from);
	SINTa find_next_at_most_avx2_twosubset(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh);

	U32 endpoints_eval_avx2_onesubset_twoind(const BC7PredecodedEndpointsBlock * b, SINTa inds_from);
	SINTa find_next_at_most_avx2_onesubset_twoind(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh);

	U32 endpoints_eval_avx2_mode6(const BC7PredecodedEndpointsBlock * b, SINTa inds_from);
	SINTa find_next_at_most_avx2_mode6(const BC7PredecodedEndpointsBlock * b, SINTa inds_from, SINTa count, U32 * out_err, U32 err_thresh);
}

OODLE_NS_END


