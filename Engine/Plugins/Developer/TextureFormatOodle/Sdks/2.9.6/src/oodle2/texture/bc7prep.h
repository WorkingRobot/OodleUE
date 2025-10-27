// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlebase.h"
#include "oodletexpub.h"

OODLE_NS_START

#define BC7PREP_DEFAULT_LAMBDA	0.01f

struct BC7PrepSettings
{
	F32 lambda;					// space/speed trade-off dial
	rrbool canonicalize_solid;	// canonicalize solid-color blocks?   (generally true)
	rrbool exhaustive_splits;	// try all options of split/no split? (generally false)
};

void BC7PrepSettings_InitDefault(BC7PrepSettings * s);

// Minimum amount of output and scratch buffer space required
// for a given number of BC7 blocks.
SINTa bc7prep_min_output_size(SINTa nblocks);
SINTa bc7prep_min_scratch_size(SINTa nblocks);

// Puts the header info in out_header and returns number of munged bytes written to output_buf
// on success, a negative value on error.
SINTa bc7prep_encode(
	OodleTexRT_BC7PrepHeader * out_header,
	U8 * output_buf, SINTa output_size,
	const U8 * bc7_bits, SINTa nblocks,
	U8 * scratch_buf, SINTa scratch_size,
	const BC7PrepSettings & settings
);

OODLE_NS_END

