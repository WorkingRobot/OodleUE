// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

void bc7prep_sort_reduce64(ReadWriteUintBuffer work_mem, uint workgroup_id, uint2 thread_id, uint nitems_dense, uint work_dense_pos)
{
	// As the name suggests, reduce64 performs an add-reduction on the histograms
	// to divide the number of rows in the scan table by 64.
	// (Invoked in a 2D grid with group y = col in the scan table.)
	uint dispatch_thread_x = workgroup_id * 64 + thread_id.x;
	uint mode_base_offs = work_dense_pos + thread_id.y;

	// Fetch values
	uint val = 0;
	uint dense_offs = mad24(dispatch_thread_x, BC7PREP_NUM_MODES, mode_base_offs);
	if (dispatch_thread_x < nitems_dense)
		val = work_mem[dense_offs];

	// Exclusive prefix sum
	uint pfxsum = ExclusivePrefixSum(val);

	// Every thread writes back the prefix sum to its array entry
	if (dispatch_thread_x < nitems_dense)
		work_mem[dense_offs] = pfxsum;

	// Final thread in the wavefront writes the total to the next-smaller histogram level,
	// if there is one.
	if (thread_id.x == 63 && nitems_dense > 64)
	{
		uint total = pfxsum + val;
		// sparse table starts offset by nitems_dense columns
		work_mem[mad24(nitems_dense + workgroup_id, BC7PREP_NUM_MODES, mode_base_offs)] = total;
	}
}
