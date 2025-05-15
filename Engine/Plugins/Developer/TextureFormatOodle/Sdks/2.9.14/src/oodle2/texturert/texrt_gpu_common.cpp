// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "oodletexrtpub.h"
#include "texrt_gpu_common.h"
#include "common_shader_constants.h"
#include "bc7prep_decode.h"
#include "bc7prep_format.h"

OODLE_NS_START

SINTa bc7prep_gpu_min_decode_work_mem_size(SINTa num_blocks)
{
	SINTa size = 0;

	// Each mode has a cascade of scan tables. The first level is one entry per BC7PREP_SORT_BLOCK_SIZE (512)
	// input blocks, the next level in the cascade is 1/64'th the size of the first one, and so on. That gives
	// a full "mip chain" that is, by the geometric series formula, 1/(1 - 1/64) = 64/63'ths the size of the
	// first level. Each level needs to use ceil() so add a bit of fudge extra columns.
	SINTa scan_table_l0_columns = (num_blocks + BC7PREP_SORT_BLOCK_SIZE-1) / BC7PREP_SORT_BLOCK_SIZE;
	SINTa scan_table_total_columns = (scan_table_l0_columns * 64) / 63 + 15; // 15: we need MAX_CASCADE_LEVELS worth for ceil, so this is an overestimate.

	SINTa scan_table_total_entries = scan_table_total_columns * BC7PREP_NUM_MODES;

	// Size of the master scan table: one U32 per entry
	size += scan_table_total_entries * sizeof(U32);

	return size;
}

OodleTexRT_Err BC7PrepGPUWorkInfo::init(U32 * work_mem_pos, U32 work_mem_max, const OodleTexRT_BC7PrepHeader * header, SINTa output_size, SINTa bc7prep_data_size)
{
	// Perform header validation
	SINTa num_blocks;
	SINTa expected_payload_size;
	OodleTexRT_Err err = bc7prep_read_header(header, &num_blocks, &expected_payload_size);
	if ( err != OodleTexRT_Err_OK )
		return err;

	if ( num_blocks > BC7PREP_MAXBLOCKS )
		return OodleTexRT_Err_BC7PrepHeaderCorrupt;

	if ( output_size < num_blocks * 16 )
		return OodleTexRT_Err_BC7PrepOutputBufTooSmall;

	if ( bc7prep_data_size < expected_payload_size )
		return OodleTexRT_Err_BC7PrepInputTooSmall;

	// Determine work memory layout
	nblocks32 = static_cast<U32>(num_blocks);
	U32 nrows_next = (nblocks32 + BC7PREP_SORT_BLOCK_SIZE - 1) / BC7PREP_SORT_BLOCK_SIZE;
	U32 scan_offs = 0;
	U32 scan_base = *work_mem_pos;
	int nlevels = 0;

	// NOTE: we always do at least 1 reduction level even if we have only one sort block (hence the "do" loop)
	// because our initial_count pass writes totals per block and we want an _exclusive_ prefix sum per mode
	// (i.e. we'd want the scan table to be all 0s in that case, not the total mode counts). Just doing a
	// single (if pointless) reduction pass takes care of that.
	do
	{
		RR_ASSERT(nlevels < MAX_CASCADE_LEVELS); // if this triggers, MAX_CASCADE_LEVELS is wrong or num_blocks > BC7PREP_MAXBLOCKS (illegal!)
		if (nlevels >= MAX_CASCADE_LEVELS)
			return OodleTexRT_Err_Internal;

		scan_base_offs[nlevels] = scan_base + scan_offs;
		scan_table_rows[nlevels] = nrows_next;
		scan_offs += BC7PREP_MODE_COUNT * nrows_next;
		nlevels++;
		nrows_next = (nrows_next + 63) >> 6;
	} while (nrows_next > 1);

	num_levels = nlevels;

	// If the scan table we just set up exceeds our work memory, fail.
	if ( work_mem_max - scan_base < scan_offs )
		return OodleTexRT_Err_BC7PrepScratchBufTooSmall;

	*work_mem_pos += scan_offs;
	return OodleTexRT_Err_OK;
}

OODLE_NS_END

