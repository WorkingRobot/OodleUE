// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrdxtcenums.h"
#include "rrdxtcblock.h"

OODLE_NS_START

typedef U32 BC6Flags;

struct BC6EncOptions
{
	// number of partitions to search in trials phase
	int max_part;

	// work done during trials phase
	bool partition_pca;
	int partition_lsq_iters;

	// reduction funnel in the refinement phase
	int narrow0;
	int narrow1;
	int narrow2;

	// search intensity
	int max_lsq_iter;
	int max_pca_stretch_passes;
	int optiters_early[2]; // 0=jitter 1=anneal. iters on narrow2 candidates.
	int optiters_late[2]; // 0=jitter 1=anneal. iters on final best candidate.

	BC6Flags flags;

	BC6EncOptions()
		: 
		max_part(32),
		partition_pca(true),
		partition_lsq_iters(2),
		narrow0(32),
		narrow1(8),
		narrow2(4),
		max_lsq_iter(4),
		max_pca_stretch_passes(4),
		flags(0)
	{
		optiters_early[0] = optiters_early[1] = 0;
		optiters_late[0] = optiters_late[1] = 0;
	}
};

// Initialize the options struct with our defaults
void bc6enc_options_init(BC6EncOptions * opts, rrDXTCLevel level, rrDXTCOptions options, bool isSigned);

// Compress a block with given options
void bc6enc_compress_block(U8 * coded_block, const U16 * pixels_rgba_half_float, const BC6EncOptions & opt);

OODLE_NS_END

