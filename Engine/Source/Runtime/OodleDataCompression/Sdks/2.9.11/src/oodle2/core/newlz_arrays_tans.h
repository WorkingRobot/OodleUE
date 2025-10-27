// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrbase.h"
#include "newlz_arrays.h"

OODLE_NS_START

// kind of nasty two different failure return values ;
//	either -1 or from_len+1 for failure ; WTF me
// -> this is now important to get right
//  < 0 means failure but *to was not modified, so previous contents are still valid
//	> from_len means failure but *to was changed!
SINTa newLZ_put_array_tans(U8 * const to, U8 * const to_end, const U8 * const from, SINTa from_len,
									const U32 * histogram,
									F32 lambda, const OodleSpeedFit * speedfit, F32 *pJ,
									rrArenaAllocator * arena);

SINTa newlz_get_array_tans(const U8 * const comp, SINTa comp_len, U8 * const to, SINTa to_len, U8 * scratch_ptr, U8 * scratch_end);

OODLE_NS_END
