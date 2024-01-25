// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrdxtcenums.h"
#include "rrdxtcblock.h"
#include "cpudispatch.h"

OODLE_NS_START

typedef U64 BC7Flags; // bitflags defined in bc7compress_internal.h

// BC7_Mode_ErrBias error (SSD) is added to block error to choose best block
//	this can be (lambda * bits)/2 to make the block choice pseudo-J
struct BC7_Mode_ErrBias
{
	U32 ssd_err_bias[8];
};

struct BC7SubsetCollection;

struct BC7EncOptions
{
	// Which modes are alowed in the first place
	bool enable_mode[8];
	
	// used in trials phase by bc7enc_refine_subset_partition only if BC7ENC_REFINE_PARTITIONS :
	int partition_lsq_iters = 2;

	// number of best choices of each type to keep into narrow0 refinement (clamped to BC7PartitionRanking::NBEST)
	int nbest_part2 = 8; // number of best 2-subset partitions to keep
	int nbest_part3 = 8; // number of best 3-subset partitions to keep
	
	// number of results to reduce to in the refinement phase :
	int narrow0 = 32;
	int narrow2 = 4;
	
	// intensity of search parameters :
	int max_lsq_iter = 4;
	int max_pca_stretch_passes = 4;
	int optiters_early[2]; // jitter & anneal optimization iterations
	int optiters_late[2]; // "early" is for narrow2 , "late" is final best one

	// alpha channel error weight
	int alpha_weight = 1;

	// forced mode and partition shape (-1 to disable)
	// NOTE: currently, forcing a mode with partitions (0-3, 7) means you also need to
	// force a partition, partition shape detection is disabled with forced modes
	int forced_mode = -1;
	int forced_partition = -1;

	// Subsets to try for partition determination
	const BC7SubsetCollection * subsets = nullptr;

	BC7Flags flags = 0;
	CpuDispatchFlags dispatch;

	BC7EncOptions()
	{
		for (bool & x : enable_mode)
			x = true;
		optiters_early[0] = optiters_early[1] = optiters_late[0] = optiters_late[1] = 0;
	}
};

void bc7_enc_options_set(BC7EncOptions * popt,rrDXTCLevel level, rrDXTCOptions options);

void BC7_CompressBlock(U8 * coded_block, const U8 * pixels_rgba, const BC7EncOptions& opt, const BC7_Mode_ErrBias * mpeb = NULL);

//extern int g_bc7_force_mode;

void bc7comp_log_index_bits_masks();

OODLE_NS_END
