// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrbase.h"
#include "rrdxtcenums.h"

RR_NAMESPACE_START

struct BlockSurface;

struct BC7_RD_Config
{
	// General Features

	// DXTC level to use for new-block encodes from within the RD encoder
	rrDXTCLevel block_encode_effort;

	// doing this on frymire helps a *lot*
	//	the normal bc7rd action on frymire does bad things
	//	need to understand why
	// @@ in theory we should let single color blocks drop through and do RD on them
	//	for example they can sometimes hit their single color exactly
	//	 by using some existing set of endpoints with flat indices
	//  it's not very important if that color value occurs many times
	//	  since you'll match the whole block for all but the first occurance
	// @@ possible opportunisitc way to do it :
	//	 look back in the MTF windows of endpoints, for single-subset blocks only
	//	 try a flat-index encoding, and see if it can hit the desired color exactly
	//   if you find one, then use that for this block (instead of the standard single color encoding)
	//	 then remember it for the future so that if you ever see that same single color again,
	//	  you use the same encoding
 	bool core_fix_single_color_block;
	// recomp block with J bias - yes helps a lot
	bool core_recompress_block_with_J_bias;
	// ALSO turn off modes that are less common than old_mode
	//	(this is very similar to just truncating the above to 1 or 2)
	// @@ not entirely sure about this, but seems to be okay
	bool core_disable_less_common_modes;
	// this (endpoints-from-indices loop) is not uniformly good
	// "frymire" gets much worse with this on
	// "good_record_only" get a bit worse
	//	most others get a bit better with this
	// "red_blue" desperately needs this and pushes the overall score in favor of ON
	bool core_endpoints_from_indices;

	// The size of the MTF window for endpoints & indices
	// @@ TODO : tweak these sizes for space-speed
	//	beware! they are very sensitive to over-training!
	//	quality changes severely and non-mononitically with WINDOW_SIZE tweaks!
	// (wood_003_m in particular is very sensitive in vcdiff, not so much in rmse)
	//
	//#define WINDOW_SIZE	512
	// @@ make things faster and then do more!
	// more = better , but slower
	//	rdotestset1 1/20/2020
	// WINDOW_SIZE 256 : total BPB saved at vcdiff +1.0 : [ 12.613 ]
	// WINDOW_SIZE 512 : total BPB saved at vcdiff +1.0 : [ 13.293 ]
	int mtf_window_endpoints_size;
	int mtf_window_indices_size;
	// seed block window with most common from baseline ?
	// -> currently pretty meh
	//  does smooth out a nasty wiggle on "decanter"
	//	definitely slightly worse on "wood_worn" and "frymire" , nop on test6 & test7
	bool mtf_window_seed_most_common;

	// In-Loop Matrix
	bool do_ilm;
	// hyperbola limited search of In-Loop Matrix:
	// we want to mainly search where either indi or endpi are very low
	//	 like 100x4 and 4x100 , not the whole 100x100 square
	int ilm_hyperbola_max_index_product;

	// Bottom Up Merge
	bool do_bottom_up_merge;

	// Final Matrix
	bool do_final_matrix;
	// Should pairs from different paritions be mixed?
	//
	// considering endp-indi pair mixes from different partitions, in theory
	// should be higher quality but slower to compute.
	bool fm_mix_partitions;
	// Limit the size of the final combined categories matrix
	int fm_combined_size_limit_sqrt;

	// Final Optimize
	bool do_final_optimize;
};

bool BC7_RD(BlockSurface * to_blocks,
	const BlockSurface * from_blocks,
	const BlockSurface * baseline_blocks,
	const BlockSurface * activity_blocks,
	int lambda,rrDXTCOptions options,
	const rrDXTCRD_Options & rdoptions);
	
RR_NAMESPACE_END
