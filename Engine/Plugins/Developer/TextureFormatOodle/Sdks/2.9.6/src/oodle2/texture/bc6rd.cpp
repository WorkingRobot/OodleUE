// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "bc6rd.h"
#include "blocksurface.h"
#include "perceptualactivity.h"
#include "log2table.h"
#include "bc6compress.h"
#include "bc6compress_internal.h"
#include "bc7bits.h"
#include "templates/rrvector.h"
#include "templates/rralgorithm.h"

//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"

RR_NAMESPACE_START

//#define BC6RD_LAMBDA_SCALE	1.f // old
// 04-30-2020 : lambda normalization
//	1.f looks too conservative
//	but I don't have a very good BC6 HDR testing methodology
//	looking at the RD curves, BC6RD looks pretty good numerically
//	visually I am seeing some obvious flaws.  It suffers from plateauing and just-too-late
//	  like the old BC1 LZ used to.  (you can see this easily on the fridge in Habib)
//	  but those are not caused by high lambda, they start at moderate lambda
// -> just looking at the RDO curves, this lambda scale could probably go higher
//	   (like 2.0 ?)
//	   but I'd like a better way to evaluate before doing that	
#define BC6RD_LAMBDA_SCALE	(1.5f * OO2TEX_GLOBAL_LAMBDA_SCALE)

// The idea: just try for basic all endpoints or all indices reuse for now.
// The 14 BC6 modes are, to our purposes, just two "major modes": either
// two subsets and 3 index bits/pixel or one subset and 4 index bits/pixel.
// The many variants within these modes correspond to different rate allocations
// to endpoint/delta bits, which can be chosen as required to make the endpoints
// fit.
//
// In short, switching between major modes is a big change, but two blocks within
// the same major mode and with reasonably similar endpoints should be able to
// merge even when their endpoint modes are distinct. It should even be possible
// to output results in a different mode than either of the two source blocks.
// (Maybe later.)

// TODOs:
// - Basic skeleton to replace pieces of blocks
// - Activity masks for HDR data
// - Need to figure out what to do in textest BC1RD for BC6
// - Signed data again, sigh

struct BC6Config
{
	// The size of the window to search in endpoints pass.
	int endpoints_window_size;

	// The size of the window to search in indices pass.
	int indices_window_size;
};

static const BC6Config c_config_levels[rrDXTCLevel_Count] =
{
	// rrDXTCLevel_VeryFast=0 (== 1 secret level in OodleTex_ API ; too low quality, not currently well optimized for this quality-speed tradeoff)
	{
		// endpoints_window_size
		256,
		// indices_window_size
		256,
	},

	// rrDXTCLevel_Fast=1,		// == OodleTex_EncodeEffortLevel_Low
	{
		// endpoints_window_size
		 80,
		// indices_window_size
		 40,
	},

	// rrDXTCLevel_Slow=2,		// == OodleTex_EncodeEffortLevel_Normal
	{
		// endpoints_window_size
		150,
		// indices_window_size
		70,
	},

	// DEFAULT :
	// rrDXTCLevel_VerySlow=3  == OodleTex_EncodeEffortLevel_High == OodleTex_EncodeEffortLevel_Default
	{
		// endpoints_window_size
		256,
		// indices_window_size
		256,
	},

	// rrDXTCLevel_Reference=4,// == 99 secret level in OodleTex_ API ; too slow to be practical, not a good time-quality tradeoff; just a max quality reference
	{
		// endpoints_window_size
		256,
		// indices_window_size
		256,
	}
};

static const BC6Config & bc6rd_get_config(rrDXTCLevel effort)
{
	//rrprintf("bc6rd_get_config: effort level=%d (%s)\n", effort, rrDXTCLevel_GetName(effort));

	RR_ASSERT( 0 <= (int)effort && (int)effort < RR_ARRAY_SIZE(c_config_levels) );
	return c_config_levels[effort];
}

struct BC6WindowEntry
{
	BC6BlockState st;
	bc7bits encoded;
	int most_recent_bi; // block index we were most recently used in
};

struct BC6Window
{
	vector<BC6WindowEntry> slots; // not in order, indexed through inds
	vector<U16> inds; // inds[i] = slot index of i'th element in LRU order
	int count; // num blocks in window

	BC6Window(int window_size)
		: count(0)
	{
		slots.resize(window_size);
		inds.resize(window_size);
		for LOOPVEC(i,inds)
			inds[i] = static_cast<U16>(i);
	}

	const BC6WindowEntry& operator[](int x) const
	{
		RR_ASSERT(x >= 0 && x < count);
		return slots[inds[x]];
	}

	// inserts block bi with either a LRU hit (if found_index >= 0) or a new entry
	void update(int bi, int found_index, const BC6BlockState &new_st, const U8 * bytes)
	{
		if ( found_index < 0 )
		{
			if ( count < inds.size32() ) // grow window
				found_index = count++;
			else // replace LRU block
				found_index = inds.size32() - 1;

			BC6WindowEntry * entry = &slots[inds[found_index]];
			entry->st = new_st;
			entry->encoded = bc7bits_load(bytes);
		}

		// move to front
		RR_ASSERT(found_index >= 0 && found_index < count);

		U16 best_slot = inds[found_index];
		memmove(&inds[1], &inds[0], found_index * sizeof(inds[0]));
		inds[0] = best_slot;

		slots[best_slot].most_recent_bi = bi;
	}
};

// uncompressed block is 128 bits
// R is just in bits
#define bc6rd_R_uncompressed	(128)

// bc6rd_matched_bits_to_lz_bits
//	take a number of shared bits
//	return an expected bit len of an lz match using only bytewise matches
//	result is <= matched_bits but >= 8*floor(matched_bits/8)
static F32 bc6rd_matched_bits_to_lz_bits( int matched_bits )
{
	RR_ASSERT( matched_bits >= 24 ); // mml 3

	// a 31 bit match =
	//	3 bytes for sure
	//  + 50% chance of one more
	//	  (if that top bit is the right one to get you a 32 bit match)
	//  = 3.5 bytes on average
	//  = 28 bits
	// less than 31 but more than just truncating down to 24

	int matched_bytes = matched_bits / 8;
	int frac_bits = matched_bits - matched_bytes*8;
	RR_ASSERT( frac_bits >= 0 && frac_bits < 8 );
	F32 probability_of_another_byte = (256 >> (8 - frac_bits)) / 256.f;

	F32 mean_bytes = matched_bytes + probability_of_another_byte;
	F32 mean_bits = mean_bytes * 8.f;
	return mean_bits;
}

static F32 bc6rd_matched_R_lz(int lzi)
{
	// cost of LZ match :
	//	note in BC6 match len and match offset are not constant / multiples of 4
	//	so they take more bits to send than in BC1

	// lzi is in blocks
	// lzi starts at 1
	RR_ASSERT( lzi >= 1 );

	// starts you off at 15 bits with lzi=1
	F32 offset_bits = (F32)rrlog2_bk(lzi+32);
	F32 match_bits = 10 + offset_bits;
	// could fiddle these constants more but it's over-training on one image

	return match_bits;
}

static F32 bc6rd_matched_R( int matched_bits, int offset )
{
	F32 lz_matched_bits = bc6rd_matched_bits_to_lz_bits(matched_bits);

	// portion sent raw unmatched :
	F32 raw_bits = 128 - lz_matched_bits;

	// the "matched_bits" are sent with an LZ match
	// cost of LZ match :
	F32 match_bits = bc6rd_matched_R_lz(offset);

	RR_ASSERT( lz_matched_bits >= match_bits ); // should be saving

	F32 bits = raw_bits + match_bits;

	RR_ASSERT( bits < 128.f );
	return bits;
}

static F32 bc6rd_J(BC6Error D, F32 R, F32 lambda)
{
	// D is the weighted, scaled MRSSE we use in the encoder
	// R is just in bits (unlike bc1rd where it's 16*bits)
	F32 J = static_cast<F32>(D) + lambda * R;

	return J;
}

bool BC6_RD(BlockSurface * to_blocks,
	const BlockSurface * from_blocks_f32,
	const BlockSurface * baseline_blocks,
	const BlockSurface * activity_blocks,
	int lambdai,
	rrDXTCOptions dxtc_options,
	const rrDXTCRD_Options & rd_options)
{
	SIMPLEPROFILE_SCOPE(bc6rd);

	const BC6Config & config = bc6rd_get_config(rd_options.effort);

	RR_ASSERT( from_blocks_f32->pixelFormat == rrPixelFormat_4_F32 );
	RR_ASSERT( to_blocks->pixelFormat == rrPixelFormat_BC6U || to_blocks->pixelFormat == rrPixelFormat_BC6S );
	RR_ASSERT( baseline_blocks->pixelFormat == to_blocks->pixelFormat );
	RR_ASSERT( to_blocks->count == from_blocks_f32->count );

	// encoder wants our pixels in F16
	BlockSurfaceObj from_blocks;
	BlockSurface_AllocCopyOrSetViewIfFormatMatches_Normalized(&from_blocks,from_blocks_f32,rrPixelFormat_4_F16);

	F32 lambda = lambdai * BC6RD_LAMBDA_SCALE;

	bool is_signed = to_blocks->pixelFormat == rrPixelFormat_BC6S;
	int nblocks = from_blocks.count;
	RR_ASSERT( nblocks <= 16*1024 );

	//const int kWindowSize = 256;
	RR_ASSERT( config.endpoints_window_size <= 32*1024 ); // no point having a window larger than max # blocks
	RR_ASSERT( config.indices_window_size <= 32*1024 );

	BC6Window endpoint_window(config.endpoints_window_size);
	BC6Window indices_window(config.indices_window_size);

	BC6EncOptions opts;
	bc6enc_options_init(&opts, rd_options.effort, dxtc_options, is_signed);

	rrRandState zero_state = {}; // we don't actually do any randomized refines, we just need to pass _a_ state

	for LOOP(bi,nblocks)
	{
		const SingleFloatBlock4x4 * pActivity = BlockSurface_SeekC_SingleFloatBlock4x4(activity_blocks,bi);
		const U8 * baseline_block_ptr = BlockSurface_SeekC(baseline_blocks,bi);
		const U16 * fmPtr = (const U16 *)BlockSurface_SeekC(&from_blocks,bi);
		U8 * output_bc6 = BlockSurface_Seek(to_blocks,bi);

		// Initialize the BC6Input for this block
		// BC6Inputs do not depend on mode/partition so we can do this once
		BC6Input input;
		input.init(fmPtr, zero_state, opts.flags);
		input.active_metric = BC6METRIC_MRSSE; // required! the only one that supports the weights

		// Scale the per-pixel weights by the activity weights
		// this makes all error eval we do through the BC6 funcs is activity-weighted
		for (int i = 0; i < 16; ++i)
			input.weightf32[i] *= pActivity->values[i];

		//input.update_lls_weights();

		BC6BlockState baseline_st;
		bc6enc_state_from_bits(&baseline_st, baseline_block_ptr, is_signed);

		// Eval baseline costs
		BC6Error baseline_D = bc6enc_eval_error(&baseline_st, &input, is_signed);
		F32 baseline_R = bc6rd_R_uncompressed;
		F32 baseline_J = bc6rd_J(baseline_D,baseline_R,lambda);

		// start with baseline
		F32 best_J = baseline_J;
		BC6Error best_D = baseline_D;
		BC6BlockState best_st = baseline_st;
		int best_mtf_endpoint_i = -1;
		int best_mtf_indices_i = -1;

		// take past blocks and try substituting endpoints
		{
			SIMPLEPROFILE_SCOPE(bc6rd_endpoints);

			for LOOP(mtfwindowi,endpoint_window.count)
			{
				const BC6WindowEntry * wnd = &endpoint_window[mtfwindowi];

				// start with previous state (mode, endpoints, partition) but compute new
				// indices for our pixels
				BC6BlockState st = wnd->st;
				BC6Error cur_D = bc6enc_reindex_block(&st, &input, is_signed);

				// two-subset modes have 82 bits header/endpoints + 46 bits indices
				// one-subset modes have 65 bits header/endpoints + 63 bits indices
				int cur_matched_bits = bc6_mode_desc[st.mode].is_two_subset ? 83 : 65;
				F32 cur_R = bc6rd_matched_R(cur_matched_bits,bi - wnd->most_recent_bi);
				F32 cur_J = bc6rd_J(cur_D,cur_R,lambda);

				if ( cur_J < best_J )
				{
					best_J = cur_J;
					best_D = cur_D;
					best_st = st;
					best_mtf_endpoint_i = mtfwindowi;
					best_mtf_indices_i = -1;
				}
			}
		}

		// try past indices with lsqr to make new endpoints

		// not sure whether this is any good or not, let's try it
		{
			SIMPLEPROFILE_SCOPE(bc6rd_indices);

			for LOOP(mtfwindowi,indices_window.count)
			{
				const BC6WindowEntry * wnd = &indices_window[mtfwindowi];

				// start with previous state (mode, indices, partition) but compute new
				// endpoints for our pixels
				//
				// NOTE: mode is up for grabs, totally can (and should) consider changing that
				// as suits the end points; keeping indices and partition index tied together does
				// make sense since BC6 stores them right next to each other, 2-subset partition+inds
				// pushes us over a byte boundary (51 bits instead of 46), and the partition index
				// influences the interpretation of index bits.
				//
				// Not completely sure about this, but it doesn't seem like a bad idea.
				BC6BlockState st = wnd->st;
				BC6Error cur_D = bc6enc_new_endpoints_with_indices_fixed(&st, &input, is_signed);

				// two-subset modes have 77 bits header/endpoints + 5 bits partition index + 46 bits indices
				// one-subset modes have 65 bits header/endpoints + 63 bits indices
				int cur_matched_bits = bc6_mode_desc[st.mode].is_two_subset ? 5+46 : 63;
				F32 cur_R = bc6rd_matched_R(cur_matched_bits,bi - wnd->most_recent_bi);
				F32 cur_J = bc6rd_J(cur_D,cur_R,lambda);

				if ( cur_J < best_J )
				{
					best_J = cur_J;
					best_D = cur_D;
					best_st = st;
					best_mtf_endpoint_i = -1;
					best_mtf_indices_i = mtfwindowi;
				}
			}
		}

		bc6enc_emit(output_bc6, &best_st);

		endpoint_window.update(bi, best_mtf_endpoint_i, best_st, output_bc6);
		indices_window.update(bi, best_mtf_indices_i, best_st, output_bc6);
	}

	return true;
}

RR_NAMESPACE_END
