// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrdxtcenums.h"
#include "rrdxtcblock.h"

OODLE_NS_START

// BC7_Mode_ErrBias error (SSD) is added to block error to choose best block
//	this can be (lambda * bits)/2 to make the block choice pseudo-J
struct BC7_Mode_ErrBias
{
	U32 ssd_err_bias[8];
};

void BC7_CompressBlock(U8 * coded_block, const U8 * pixels_rgba, rrDXTCLevel level, rrDXTCOptions options, const BC7_Mode_ErrBias * mpeb = NULL);

//extern int g_bc7_force_mode;

void bc7comp_log_index_bits_masks();

OODLE_NS_END
