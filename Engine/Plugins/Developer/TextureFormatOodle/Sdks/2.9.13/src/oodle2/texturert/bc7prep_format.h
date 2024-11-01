// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

OODLE_NS_START

// ---- Format definition

enum
{
	BC7PREP_MODE_COUNT = OODLETEXRT_BC7PREP_MODE_COUNT,
};

// stick a little bit of magic in the version so we can quickly rule out data that's not bc7prep at all
#define BC7PREP_HEADER_VERSION	(0x000007BC)

// Max supported number of blocks in a single run
#define BC7PREP_MAXBLOCKS	(1 << 27)		// 2GB of data

enum
{
	BC7PREP_FLAG_SPLIT0				= 1, // "mode0 is split" flag
	BC7PREP_FLAG_ALL_SPLIT			= (BC7PREP_FLAG_SPLIT0 << BC7PREP_MODE_COUNT) - BC7PREP_FLAG_SPLIT0,
	BC7PREP_FLAG_SWITCH_COLORSPACE	= 1<<16,

	BC7PREP_FLAG_ALL_ASSIGNED		= BC7PREP_FLAG_ALL_SPLIT | BC7PREP_FLAG_SWITCH_COLORSPACE,
};

enum
{
	BC7PREP_MODE0_SIZE = 16,
	BC7PREP_MODE1_SIZE = 16,
	BC7PREP_MODE2_SIZE = 16,
	BC7PREP_MODE3_SIZE = 16,
	BC7PREP_MODE4_SIZE = 16,
	BC7PREP_MODE5_SIZE = 16,
	BC7PREP_MODE6_SIZE = 16,
	BC7PREP_MODE7_SIZE = 16,
	BC7PREP_MODE8_SIZE = 16,
	BC7PREP_MODE9_SIZE = 4,

	BC7PREP_MODE0_SPLIT = 8,	// NOTE: not in the right spot (should be at byte 10), but 8 does way better
	BC7PREP_MODE1_SPLIT = 8,	// NOTE: not in the right spot (should be at byte 10), but 8 does way better
	BC7PREP_MODE2_SPLIT = 12,
	BC7PREP_MODE3_SPLIT = 12,
	BC7PREP_MODE4_SPLIT = 6,	// NOTE: this one really _does_ seem to want the split at 6 and not 8
	BC7PREP_MODE5_SPLIT = 8,
	BC7PREP_MODE6_SPLIT = 8,
	BC7PREP_MODE7_SPLIT = 12,
	BC7PREP_MODE8_SPLIT = 0,	// never split
	BC7PREP_MODE9_SPLIT = 0,	// never split
};

OODLE_NS_END

