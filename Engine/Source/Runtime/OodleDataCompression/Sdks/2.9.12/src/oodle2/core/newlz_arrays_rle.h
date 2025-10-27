// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrbase.h"
#include "newlz_arrays.h"

OODLE_NS_START

// Return value: compressed size; >from_len means failure.
// "to" buffer is always overwritten.
//
// If you want a maximum compressed size, just pass to_end = to + max_comp_size.
SINTa newLZ_put_array_rle(U8 * const to, U8 * const to_end, const U8 * const from, SINTa from_len, 
		F32 lambda, const OodleSpeedFit * speedfit, F32 *pJ, rrArenaAllocator * arena, U32 entropy_flags, int compression_level);

// Returns comp_len on success, -1 on failure.
SINTa newLZ_get_array_rle(const U8 * const comp, SINTa comp_len, U8 * const to, SINTa to_len, U8 * scratch_ptr, U8 * scratch_end);

OODLE_NS_END
