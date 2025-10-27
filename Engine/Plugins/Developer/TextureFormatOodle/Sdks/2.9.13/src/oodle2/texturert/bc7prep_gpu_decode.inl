// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// BC7Prep GPU decode core
// meant to be included from several shading langs, so needs to be written to be compatible with all of them!

uint packed_add32(uint a, uint b, uint msb_mask, uint nonmsb_mask)
{
	// can sum low bits directly
	uint low = (a & nonmsb_mask) + (b & nonmsb_mask);
	// need carryless sum (=XOR) in MSB
	return low ^ ((a ^ b) & msb_mask);
}

uint3 decorr_to_RGB_packed32(uint3 ycrcb, uint msb_mask, uint nonmsb_mask)
{
	return uint3(
		packed_add32(ycrcb.x, ycrcb.y, msb_mask, nonmsb_mask),
		ycrcb.x,
		packed_add32(ycrcb.x, ycrcb.z, msb_mask, nonmsb_mask)
	);
}

uint3 decorr_to_RGB_narrow(uint3 ycrcb, int width)
{
	uint mask = (1 << width) - 1;
	return uint3(
		(ycrcb.x + ycrcb.y) & mask,
		ycrcb.x & mask,
		(ycrcb.x + ycrcb.z) & mask
	);
}

uint compact32to7_2x(uint2 x, uint bit_offs)
{
	return bit_extract32(x.x, bit_offs, 7) | (bit_extract32(x.y, bit_offs, 7) << 7);
}

uint compact24to7_2x(uint x)
{
	return (x & 0x7f) | ((x >> 17) & 0x3f80);
}

uint compact16to6_4x(uint2 x)
{
	// NOTE this assumes that the high bits of x are masked off! (they are in all uses.)
	x = (x >> 10) | x; // 2-vector!
	return bit_select(0xff000fffu, x.x, x.y << 12); // uses that top 8 bits of x.x are always 0 due to prior masking.
}

uint compact16to5_2x(uint x)
{
	return ((x >> 11) & 0x3e0u) | (x & 0x1fu);
}

uint compact16to5_4x(uint2 x)
{
	return compact16to5_2x(x.x) | (compact16to5_2x(x.y) << 10);
}

uint compact16to5_6x(uint3 x)
{
	return compact16to5_4x(x.xy) | (compact16to5_2x(x.z) << 20);
}

uint compact16to1_2x(uint x)
{
	return (x & 1u) | ((x >> 15) & 2u);
}

uint compact16to1_4x(uint2 x)
{
	return compact16to1_2x(x.x) | (compact16to1_2x(x.y) << 2);
}

uint compact8to7_2x(uint x)
{
	return ((x & 0x7f00u) >> 1) | (x & 0x007fu);
}

uint compact8to7_4x(uint x)
{
	x = bit_select(0x007f007fu, x, x >> 1); // move by 1
	x = ((x & 0x3fff0000u) >> 2) | (x & 0x00003fffu); // move by 2

	return x;
}

uint compact8to1_8x_hi(uint2 x)
{
	// high nibbles from high half, low nibbles from low half
	uint y = ((x.x & 0x80808080u) >> 4) | (x.y & 0x80808080u);

	y += y << 7; // from groups of 2 bits
	y += y << 14; // form groups of 4 bits
	return y >> 24;
}

uint expand1to5_6x(uint x)
{
	// space groups of 4 bits out correctly
	x = (x & 0xf) | ((x & 0x30) << 16);

	// the rest is a multiply and mask
	x = (x * 0x1111) & 0x2108421;

	return x;
}

uint expand1to5_4x(uint x)
{
	return ((x & 0xf) * 0x1111) & 0x8421;
}

uint expand2to6_4x(uint x)
{
	x = ((x << 8) | x) & 0x0f00f;
	x = ((x << 4) | x) & 0xc30c3;
	return x;
}

uint expand4to5_6x(uint x)
{
	x = bit_select(0x0000ffffu, x, x << 4); // move by 4
	x = bit_select(0x0ff000ffu, x, x << 2); // move by 2
	x = (x & 0x0ff3fcffu) + (x & 0x0f03c0f0u); // move by 1 and mask
	return x;
}

uint expand4to5_4x(uint x)
{
	x = ((x & 0x0ff00u) << 2) | (x & 0x000ff); // move by 2
	x += x & 0x3c0f0; // move by 1
	return x;
}

uint4 resolve_split8(uint4 in0, uint3 in1)
{
	return uint4(in0.xy, in1.xy);
}

uint4 resolve_split12(uint4 in0, uint3 in1)
{
	return uint4(in0.xyz, in1.x);
}

// ---- Individual modes

uint4 bc7_handle_mode0(uint4 block_bytes, bool switch_colorspace)
{
	uint3 rgb = uint3(
		bit_extract32(block_bytes.x, 0, 24),
		bit_extract32from64(block_bytes.xy, 24, 24),
		bit_extract32from64(block_bytes.yz, 16, 24)
	);
	uint partbits = bit_extract32(block_bytes.z, 8, 4);

	if (switch_colorspace)
		rgb = decorr_to_RGB_packed32(rgb, 0x888888, 0x777777);

	return uint4(
		0x01 | (partbits << 1) | (rgb.x << 5) | (rgb.y << 29),
		(rgb.y >> 3) | (rgb.z << 21),
		(rgb.z >> 11) | (block_bytes.z & ~0x1fffu),
		block_bytes.w
	);
}

uint4 bc7_handle_mode1(uint4 block_bytes, bool switch_colorspace)
{
	uint3 rgb = uint3(
		compact16to6_4x((block_bytes.xy >>  0) & 0x003f003f),
		compact16to6_4x((block_bytes.xy >>  5) & 0x003e003e), // NOTE: bottom bits of g left clear
		compact16to6_4x((block_bytes.xy >> 10) & 0x003e003e)  // NOTE: bottom bits of b left clear
	);

	uint expanded_gb = expand2to6_4x(block_bytes.z & 0xff);
	rgb.y |= (expanded_gb >> 0) & 0x41041; // bottom bits of g
	rgb.z |= (expanded_gb >> 1) & 0x41041; // bottom bits of b

	if (switch_colorspace)
		rgb = decorr_to_RGB_packed32(rgb, 0x820820, 0x7df7df);

	return uint4(
		0x02 | ((block_bytes.z >> 6) & 0xfc) | (rgb.x << 8),
		rgb.y | (rgb.z << 24),
		(rgb.z >> 8) | (block_bytes.z & ~0xffffu),
		block_bytes.w
	);
}

uint4 bc7_handle_mode2(uint4 block_bytes, bool switch_colorspace)
{
	uint3 rgb = uint3(
		compact16to5_6x(block_bytes.xyz >>  1),
		compact16to5_6x(block_bytes.xyz >>  6),
		compact16to5_6x(block_bytes.xyz >> 11)
	);
	uint partbits = compact16to1_4x(block_bytes.xy) | (compact16to1_2x(block_bytes.z) << 4);

	if (switch_colorspace)
		rgb = decorr_to_RGB_packed32(rgb, 0x21084210, 0x1ef7bdef);

	return uint4(
		0x04 | (partbits << 3) | (rgb.x << 9),
		(rgb.x >> 23) | (rgb.y << 7),
		(rgb.y >> 25) | (rgb.z << 5),
		(rgb.z >> 27) | (block_bytes.w & ~7u)
	);
}

uint4 bc7_handle_mode3(uint4 block_bytes, bool switch_colorspace)
{
	// r0g0b0r1 g1b1r2g2 b2r3g3b3
	uint3 rgb = uint3(
		compact24to7_2x(block_bytes.x)		| (bit_extract32(block_bytes.y, 16, 7) << 14) | (bit_extract32(block_bytes.z,  8, 7) << 21),
		bit_extract32(block_bytes.x,  8, 7) | (compact24to7_2x(block_bytes.y)	   <<  7) | (bit_extract32(block_bytes.z, 16, 7) << 21),
		bit_extract32(block_bytes.x, 16, 7) | (bit_extract32(block_bytes.y,  8, 7) <<  7) | (compact24to7_2x(block_bytes.z)		 << 14)
	);

	uint partbits = compact8to1_8x_hi(block_bytes.xy);

	if (switch_colorspace)
		rgb = decorr_to_RGB_packed32(rgb, 0x8102040, 0x7efdfbf);

	return uint4(
		0x08 | ((partbits & 0x3f) << 4) | (rgb.x << 10),
		(rgb.x >> 22) | (rgb.y << 6),
		(rgb.y >> 26) | (rgb.z << 2) | ((partbits & 0xc0) << 24),
		block_bytes.w
	);
}

uint4 bc7_handle_mode4(uint4 block_bytes, bool switch_colorspace)
{
	uint low_a = block_bytes.x & 3;
	uint rgb_endpt = block_bytes.x >> 2;

	if (switch_colorspace)
	{
		uint3 rgb = uint3(
			bit_extract32(block_bytes.x,  2, 10),
			bit_extract32(block_bytes.x, 12, 10),
			bit_extract32(block_bytes.x, 22, 10)
		);
		rgb = decorr_to_RGB_packed32(rgb, 0x210, 0x1ef);
		rgb_endpt = rgb.x | (rgb.y << 10) | (rgb.z << 20);
	}

	// re-apply channel rotate
	uint crot = bit_extract32(block_bytes.y, 16, 2);
	if (crot != 0)
	{
		// 3-xor trick for swap
		uint shift_amount = (crot - 1) * 10;
		uint xor_mask = (block_bytes.y ^ (rgb_endpt >> shift_amount)) & 0x3ffu;
		block_bytes.y ^= xor_mask;
		rgb_endpt ^= xor_mask << shift_amount;
	}

	// insert low alpha bits
	uint abits = (low_a & 1) | (bit_extract32(block_bytes.y, 0, 5) << 1);
	abits |= (bit_extract32(low_a, 1, 1) << 6) | (bit_extract32(block_bytes.y, 5, 5) << 7);

	return uint4(
		0x10 | (crot << 5) | (bit_extract32(block_bytes.y, 10, 1) << 7) | (rgb_endpt << 8),
		(rgb_endpt >> 24) | (abits << 6) | (block_bytes.y & ~0x3ffff),
		block_bytes.z,
		block_bytes.w
	);
}

uint bc7_mode5_decorr_to_RGB_endpoint(uint endpoint)
{
	uint3 rgb = uint3(
		bit_extract32(endpoint,  1, 7),
		bit_extract32(endpoint,  9, 7),
		bit_extract32(endpoint, 17, 7)
	);
	rgb = decorr_to_RGB_narrow(rgb, 7);
	return (endpoint & 0xff010101u) | (rgb.x << 1) | (rgb.y << 9) | (rgb.z << 17);
}

uint4 bc7_handle_mode5(uint4 block_bytes, bool switch_colorspace)
{
	uint2 endpoints = block_bytes.xy;

	if (switch_colorspace)
	{
		endpoints.x = bc7_mode5_decorr_to_RGB_endpoint(endpoints.x);
		endpoints.y = bc7_mode5_decorr_to_RGB_endpoint(endpoints.y);

		// apply channel rotate and pack down
		uint crot = block_bytes.z & 3;
		uint rbits = compact32to7_2x(endpoints, (crot == 1 ? 25 :  1));
		uint gbits = compact32to7_2x(endpoints, (crot == 2 ? 25 :  9));
		uint bbits = compact32to7_2x(endpoints, (crot == 3 ? 25 : 17));
		uint ashift = ((3 + crot) & 3) * 8;
		uint a0 = bit_extract32(endpoints.x, ashift, 8);
		uint a1 = bit_extract32(endpoints.y, ashift, 8);

		return uint4(
			0x20 | (crot << 6) | (rbits << 8) | (gbits << 22),
			(gbits >> 10) | (bbits << 4) | (a0 << 18) | (a1 << 26),
			(a1 >> 6) | (block_bytes.z & ~3u),
			block_bytes.w
		);
	}
	else
	{
		// re-apply channel rotate
		uint crot = block_bytes.z & 3;

		uint bits0 = endpoints.x & 0xffffu;
		uint bits1 = endpoints.x >> 16;
		uint bits2 = endpoints.y & 0xffffu;
		uint bits3 = endpoints.y >> 16;

		uint rbits = (crot == 1) ? bits3 : bits0;
		uint gbits = (crot == 2) ? bits3 : bits1;
		uint bbits = (crot == 3) ? bits3 : bits2;

		uint atmp0 = ((crot & 1) != 0) ? bits0 : bits3;
		uint atmp1 = ((crot & 1) != 0) ? bits2 : bits1;
		uint abits = (crot >= 2) ? atmp1 : atmp0;

		// pack down
		uint first4 = compact8to7_4x((rbits >> 1) | (gbits << 15));
		uint last2	= compact8to7_2x(bbits >> 1);

		return uint4(
			0x20 | (crot << 6) | (first4 << 8),
			(first4 >> 24) | (last2 << 4) | (abits << 18),
			(abits >> 14) | (block_bytes.z & ~3u),
			block_bytes.w
		);
	}
}

uint4 bc7_handle_mode6(uint4 block_bytes, bool switch_colorspace)
{
	if (switch_colorspace)
	{
		uint4 rgba = uint4(
			compact32to7_2x(block_bytes.xy,  0),
			compact32to7_2x(block_bytes.xy,  8),
			compact32to7_2x(block_bytes.xy, 16),
			compact32to7_2x(block_bytes.xy, 24)
		);

		rgba.xyz = decorr_to_RGB_packed32(rgba.xyz, 0x2040, 0x1fbf);

		return uint4(
			0x40 | (rgba.x << 7) | (rgba.y << 21),
			(rgba.y >> 11) | (rgba.z << 3) | (rgba.w << 17) | (block_bytes.y & (1u << 31)),
			block_bytes.z,
			block_bytes.w
		);
	}
	else
	{
		uint2 deint = uint2(
			compact8to7_4x(block_bytes.x),
			compact8to7_4x(block_bytes.y)
		);

		return uint4(
			0x40 | (deint.x << 7),
			(deint.x >> 25) | (deint.y << 3) | (block_bytes.y & (1u << 31)),
			block_bytes.z,
			block_bytes.w
		);
	}
}

uint4 bc7_handle_mode7(uint4 block_bytes, bool switch_colorspace)
{
	if (switch_colorspace)
	{
		uint4 rgba = uint4(
			compact16to5_4x(block_bytes.xy >> 1),
			compact16to5_4x(block_bytes.xy >> 6),
			compact16to5_4x(block_bytes.xy >> 11),
			bit_extract32(block_bytes.z, 8, 20)
		);
		uint pbits = compact16to1_4x(block_bytes.xy);

		rgba.xyz = decorr_to_RGB_packed32(rgba.xyz, 0x84210, 0x7bdef);

		return uint4(
			0x80 | (bit_extract32from64(block_bytes.zw, 28, 6) << 8) | (rgba.x << 14),
			(rgba.x >> 18) | (rgba.y << 2) | (rgba.z << 22),
			(rgba.z >> 10) | (rgba.w << 10) | (pbits << 30),
			(pbits >> 2) | (block_bytes.w & ~3u)
		);
	}
	else
	{
		uint pbits = bit_extract32(block_bytes.z, 8, 4);

		uint src0 = block_bytes.x;
		uint src1 = bit_extract32from64(block_bytes.xy, 24, 24);
		uint src2 = bit_extract32(block_bytes.y, 16, 16);

		uint3 rgbabits = uint3(
			(expand4to5_6x(src0) << 1) | expand1to5_6x(block_bytes.z >> 12), // 30 bits payload
			(expand4to5_6x(src1) << 1) | expand1to5_6x(block_bytes.z >> 18), // 30 bits payload
			(expand4to5_4x(src2) << 1) | expand1to5_4x(block_bytes.z >> 24)  // 20 bits payload
		);

		return uint4(
			0x80 | (bit_extract32from64(block_bytes.zw, 28, 6) << 8) | (rgbabits.x << 14),
			(rgbabits.x >> 18) | (rgbabits.y << 12),
			(rgbabits.y >> 20) | (rgbabits.z << 10) | (pbits << 30),
			(pbits >> 2) | (block_bytes.w & ~3u)
		);
	}
}

uint4 bc7_handle_mode9(uint block_bytes, bool switch_colorspace)
{
	uint4 rgba = uint4(
		bit_extract32(block_bytes,	0, 8),
		bit_extract32(block_bytes,	8, 8),
		bit_extract32(block_bytes, 16, 8),
		bit_extract32(block_bytes, 24, 8)
	);

	if (switch_colorspace)
		rgba.xyz = decorr_to_RGB_narrow(rgba.xyz, 8);

	uint bit6_mask = 0x40 | (0x40 << 14);
	uint lo7_mask  = 0x7f | (0x7f << 14);

	// modified from CPU version to not cross 32-bit boundaries
	uint2 color_bits = uint2(rgba.x | (rgba.y << 14), rgba.z);
	uint2 t = color_bits << 6;
	color_bits = ((color_bits >> 1) & lo7_mask) - (color_bits & ~lo7_mask);
	color_bits += t + (t & bit6_mask);

	// now rearrange fields to put everything in the right spots
	return uint4(
		0x20 | (color_bits.x << 8),
		(color_bits.x >> 24) | (color_bits.y << 4) | (rgba.w << 18),
		0xaaaaaaacu,
		0
	);
}

void bc7prep_decode_gather(BC7PrepCommonSRT srt, ReadOnlyByteBuffer payload, ReadWriteByteBuffer blocks_out, ReadWriteUintBuffer work_mem, uint workgroup_id, uint thread_id)
{
	uint dispatch_thread_id = workgroup_id*BC7PREP_SORT_BLOCK_SIZE + thread_id;
	uint local_group_id = thread_id >> 6;

	// Initialize the local histogram: first, zero-clear it.
	if (thread_id < LOCAL_HISTO_SIZE)
		local_histo[thread_id] = 0;

	// Also start reading mode DWord for this block before we do anything else
	uint mode_dword = payload.Load(srt.modes_offset + (dispatch_thread_id >> 3)*4);

	ThreadGroupMemoryBarrierSync();

	// Initialize the histogram entries for column 0 (group 0) from the contents of the scan table.
	if (thread_id < BC7PREP_NUM_MODES*4)
	{
		uint target_mode = thread_id >> 2;

		// Fetch the base address for the scan table subsample level
		uint scan_subsample_level = thread_id & 3;
		uint scan_level_base = read_scan_level_base(srt, scan_subsample_level);

		// Load the entries from all the histogram levels at once
		uint load_row = workgroup_id >> (scan_subsample_level * 6);
		uint scan_vals = work_mem[mad24(load_row, BC7PREP_NUM_MODES, scan_level_base + target_mode)];

		// Final lane sums across the values and writes back the total
		uint total_summed = prefix_sum4(scan_vals, thread_id);
		if (scan_subsample_level == 3)
			local_histo[target_mode * NUM_GROUPS] = total_summed;
	}

	// Decode mode for this block and handle illegal modes
	uint mode = bit_extract32(mode_dword, (dispatch_thread_id * 4) & 31, 4);
	mode = min(mode, BC7PREP_NUM_MODES); // clamp all out-of-bounds modes to the same value (BC7PREP_NUM_MODES)

	// Start fetching the mode info for every block
	// do this now to shorten the wait when we need it
	uint3 mode_info = srt.mode_info[mode].xyz;

	// Peel off unique modes one by one and write out totals for each mode to LDS
	// also determines where we are in the run
	uint2 totals_and_pos_in_run = determine_totals_and_pos_in_run(mode);
	uint pos_in_run = totals_and_pos_in_run.y;
	uint total_this_mode = totals_and_pos_in_run.x;

	// Need previous writes to complete before we start new ones
	ThreadGroupMemoryBarrierSync();

	if (local_group_id < NUM_GROUPS - 1 && pos_in_run == 0)
		local_histo[mode * NUM_GROUPS + local_group_id + 1] = total_this_mode;

	ThreadGroupMemoryBarrierSync();

	// Inclusive prefix sum across groups within the local histogram
	// this gives us the final starting write position for each group
	if (thread_id < LOCAL_HISTO_SIZE)
		local_histo[thread_id] = prefix_sum_groups(local_histo[thread_id], thread_id);

	ThreadGroupMemoryBarrierSync();

	// ---- DECODE

	// If we're past the end of the block array, our work here is done,
	// we're never gonna emit anything.
	if (dispatch_thread_id >= srt.nblocks)
		return;

	// Determine which block inside the source area for the mode we are
	uint local_source_index = local_histo[mode * NUM_GROUPS + local_group_id] + pos_in_run;

	// Sort debug mode just writes the mode-local source index to the destination buffer
	if (srt.global_flags & BC7PREP_DECODE_DEBUG_SORT)
	{
		blocks_out.Store(dispatch_thread_id * 4, local_source_index);
		return;
	}

	// Calculate input offsets for the first/second half of the split for every thread
	uint2 mode_strides = uint2(mode_info.z & 0xff, (mode_info.z >> 8) & 0xff);
	uint2 input_offs = local_source_index * mode_strides + mode_info.xy;

	// Load both halves. We have splits at either 6, 8, 12 or 16 bytes, so the first half
	// needs a Load4 and the second needs a Load3. (We never read past the end because
	// buffer accesses are bounds-checked.)
	uint4 in0 = payload.Load4(input_offs.x & ~3);
	uint3 in1 = payload.Load3(input_offs.y & ~3);

	// Figure out what we should do
	bool switch_colorspace = (srt.global_flags & BC7PREP_DECODE_SWITCH_COLORSPACE) != 0;

	// Do the mode-specific processing. Default is pass-through for unknown modes
	// and mode 8.
	uint4 block_bytes = in0;

	if (mode == 0) block_bytes = bc7_handle_mode0(resolve_split8(in0, in1), switch_colorspace);
	if (mode == 1) block_bytes = bc7_handle_mode1(resolve_split8(in0, in1), switch_colorspace);
	if (mode == 2) block_bytes = bc7_handle_mode2(resolve_split12(in0, in1), switch_colorspace);
	if (mode == 3) block_bytes = bc7_handle_mode3(resolve_split12(in0, in1), switch_colorspace);
	if (mode == 4) block_bytes = bc7_handle_mode4(resolve_split6(in0, in1, input_offs), switch_colorspace);
	if (mode == 5) block_bytes = bc7_handle_mode5(resolve_split8(in0, in1), switch_colorspace);
	if (mode == 6) block_bytes = bc7_handle_mode6(resolve_split8(in0, in1), switch_colorspace);
	if (mode == 7) block_bytes = bc7_handle_mode7(resolve_split12(in0, in1), switch_colorspace);
	// mode 8 is raw passthrough and handled by the default path above
	if (mode == 9) block_bytes = bc7_handle_mode9(in0.x, switch_colorspace);

	// Store results
	blocks_out.Store4(dispatch_thread_id * 16, block_bytes);
}

// vim:ft=cpp

