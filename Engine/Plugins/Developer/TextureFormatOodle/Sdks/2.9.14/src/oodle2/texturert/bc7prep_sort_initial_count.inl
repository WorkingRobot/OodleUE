// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

void bc7prep_sort_initial_count(BC7PrepCommonSRT srt, ReadOnlyByteBuffer payload, ReadWriteUintBuffer work_mem, uint group_id, uint group_thread_id)
{
	// This shader's job is to histogram the modes for groups of 512 blocks
	// and store the result.
	uint thread_id = group_id * BC7PREP_INITIAL_COUNT_THREADS + group_thread_id;

	// We process 8 blocks worth of data per invocation, which works out to exactly one mode DWord
	uint base_block_id = thread_id * 8;
	uint modes_packed = payload.Load(srt.modes_offset + (base_block_id >> 1));

	// Set mode nibbles for out-of-bounds blocks to 0xf
	int valid_nblocks = (int)srt.nblocks - (int)base_block_id;
	if (valid_nblocks < 8)
		modes_packed |= ~0u << (max(valid_nblocks, 0) * 4);

	// Accumulate into the histogram bins, arranging things so there are no bank conflicts
	// within aligned groups of 16 threads.
	//
	// We only actually use 16 entries of this, but having the full wavefront width is
	// more convenient for the code (less extra bounds checking) and doesn't actually
	// influence the amount of LDS memory allocated on GCN/RDNA GPUs.
	local_histogram[group_thread_id] = 0;

	// Current mode we're testing, broadcast to every nibble in the DWord
	// we start every thread in a group of 16 at a different value to reduce bank conflicts
	uint mode_seed = (group_thread_id & 15) * 0x11111111u;
	uint modes_test = modes_packed ^ mode_seed;

	uint modes_test_lo3 = modes_test & 0x77777777u;
	uint modes_test_msb0 = ~modes_test & 0x88888888u;
	uint modes_test_msb1 = modes_test & 0x88888888u;

	ThreadGroupMemoryBarrierSync();

	[unroll]
	for (uint i = 0; i < 8; ++i)
	{
		// This thread counts instances of mode (group_thread_id & 15) ^ i
		// and writes them to bin (group_thread_id & 31) ^ i, meaning we end
		// up with two bins for each mode (that we add together in the end).
		// Doing it this way avoids all LDS bank conflicts.
		uint bin = (group_thread_id & 31) ^ i;

		// Test for low mode 3 bits matching i
		uint test_lo3_xored = modes_test_lo3 ^ (i * 0x11111111u);

		// This expression leaves the MSB of each nibble set if and only if the low 3 bits in
		// the corresponding nibble position in modes_test_lo3 are 0.
		uint test_lo3_was0 = 0x88888888u - test_lo3_xored;

		// by ANDing in the corresponding mask for the nibble MSB being 0 or 1 and doing
		// a pop count, we can determine how many instances we have of "mode" and "mode ^ 8"
		uint count0 = CountSetBits(test_lo3_was0 & modes_test_msb0);
		uint count1 = CountSetBits(test_lo3_was0 & modes_test_msb1);

		AtomicAdd(local_histogram[bin], count0);
		AtomicAdd(local_histogram[bin ^ 8], count1);
	}

	ThreadGroupMemoryBarrierSync();

	// Write the result out to the corresponding histogram row
	if (group_thread_id < BC7PREP_NUM_MODES)
	{
		// Add the totals for the two bins for this mode
		uint total = local_histogram[group_thread_id] + local_histogram[group_thread_id + 16];
		work_mem[srt.work_scan_pos + mad24(group_id, BC7PREP_NUM_MODES, group_thread_id)] = total;
	}
}
