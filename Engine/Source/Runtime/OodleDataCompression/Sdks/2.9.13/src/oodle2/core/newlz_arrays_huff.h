// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrbase.h"
#include "newlz_arrays.h"

OODLE_NS_START

#define NEWLZ_HUFF6_MIN_SIZE	256		// don't bother with huff6 if stream is shorter than this


// kind of nasty two different failure return values ;
//	either -1 or from_len+1 for failure ; WTF me
// -> this is now important to get right
//  < 0 means failure but *to was not modified, so previous contents are still valid
//	> from_len means failure but *to was changed!
SINTa newLZ_put_array_huff(U8 * const to, U8 * const to_end, const U8 * const from, SINTa from_len, 
									const U32 * histogram,
									F32 lambda, const OodleSpeedFit * speedfit, F32 * pJ /* readwrite */, F32 deadline_t,
									U32 * p_huff_type, 
									U32 entropy_flags,
									rrArenaAllocator * arena,
									int compression_level);
// huff_type == NEWLZ_ARRAY_TYPE_HUFF6 or NEWLZ_ARRAY_TYPE_HUFF
									
SINTa newlz_get_array_huff(const U8 * const comp, SINTa comp_len, U8 * const to, SINTa to_len, bool is_huff6);

OODLE_NS_END
