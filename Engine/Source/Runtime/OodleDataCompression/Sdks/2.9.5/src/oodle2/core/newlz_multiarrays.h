// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "newlz_arrays.h"

OODLE_NS_START

struct OodleSpeedFit;

/**

"multi" array : N arrays
	can be merged to fewer entropy arrays
	can be indexed so the N linear arrays are made from segments of entropy arrays

**/

// newLZ_put_multiarray funcs unconditionally stomp [to] and [pJ]

SINTa newLZ_put_multiarray_indexed(U8 * const to, U8 * const to_end,
								const U8 * const * from_ptrs, const SINTa * from_lens, SINTa num_arrays,
								U32 entropy_flags,
								F32 lambda,const OodleSpeedFit * speedfit, F32 * pJ,
								rrArenaAllocator * arena, int compression_level);
								
SINTa newLZ_get_multiarray(const U8 * const comp_start, const U8 * const comp_end, 
							U8 * to_mem, U8 * to_mem_end,
							U8 ** const to_ptrs, SINTa * to_lens, SINTa num_arrays,
							SINTa * ptot_to_len,
							bool force_copy_uncompressed,
							U8 * scratch_ptr, U8 * scratch_end);
							

/**

"split" array : a single array, split into pieces

used for NEWLZ_ARRAY_TYPE_SPLIT

**/

SINTa newLZ_get_array_split(const U8 * const comp, SINTa comp_len, U8 * const to, SINTa to_len,
								U8 * scratch_ptr, U8 * scratch_end);

//=======================

// put_array_sub is called from put_array
//	_sub does not put the initial 5 byte header

// newLZ_put_array_split only writes to [to] if it can make a J < prev_J
// @@ NOTE : newLZ_put_array_split is a sub-newLZ_put_array putter ; eg. it doesn't put header!
SINTa newLZ_put_array_sub_split(U8 * const to, U8 * const to_end, const U8 * const from, SINTa from_len, const U32 * histogram, 
								rrArenaAllocator * arena, int compression_level,
								U32 entropy_flags, F32 lambda, const OodleSpeedFit * speedfit, F32 prev_J, F32 * new_J
								);

// newLZ_put_array_split2_hinted is a top level putter (it does put newlz_array header)
SINTa newLZ_put_array_split2_hinted(U8 * const to, U8 * const to_end, const U8 * const from, SINTa from_len,
								U32 entropy_flags, F32 lambda, const OodleSpeedFit * speedfit, F32 * pJ,
								rrArenaAllocator * arena, int compression_level,
								U32 * opt_histo,
								SINTa hint_split_point);



// @@ temporarily public for testing :
SINTa newLZ_put_multiarray_indexed_sub(U8 * const out_ptr, U8 * const out_end,
								const U8 * const * from_ptrs, const SINTa * from_lens, SINTa num_arrays,
								U32 entropy_flags,
								F32 lambda, const OodleSpeedFit * speedfit, F32 * pJ,
								rrArenaAllocator * arena, int compression_level);
								
OODLE_NS_END

