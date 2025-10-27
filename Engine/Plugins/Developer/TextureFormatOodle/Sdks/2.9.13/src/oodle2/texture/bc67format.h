// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#ifndef BC7FORMAT_NO_INCLUDE
#include "oodlebase.h"
#endif

OODLE_NS_START

// Reference: Khronos data format specification 1.1
//	 https://www.khronos.org/registry/DataFormat/specs/1.1/dataformat.1.1.html#BPTC
// (KDFS for short)

#define BC7_NUM_MODES	8

// Description of a BC7 block mode. Fields use the same names and order as the KDFS.
struct BC7ModeDesc
{
	U8 mode; // mode id
	U8 ns; // num subsets
	U8 pb; // partition bits
	U8 rb; // rotation bits
	U8 isb; // index selection bits
	U8 cb; // color bits
	U8 ab; // alpha bits
	U8 epb; // endpoint P-bits
	U8 spb; // shared P-bits
	U8 ib; // index bits per element
	U8 ib2; // secondary index bits per element
};

struct BC67Partition
{
	U32 subset_mask; // 2 bits/pixel, subset index for that pixel
	U16 anchor0; // position of first non-0 anchor index (0 for none)
	U16 anchor1; // position of second non-0 anchor index (0 for none)
};

// "Friendly" decoded block description, useful for visualizing
// and debug printing
struct BC7BlockDesc
{
	// Pointer to mode and partition description
	const BC7ModeDesc * mode;
	const BC67Partition * partition;

	// Fields from the block itself
	U8 partition_id;
	U8 rot;
	U8 ind_sel;

	// Number of bits per color/alpha index (after ind_sel is applied)
	U8 color_ind_bits;
	U8 alpha_ind_bits;

	// Unpacked endpoint colors and indices
	U8 endpoints[6][4]; // [index][channel]; 0=r 1=g 2=b 3=a
	U8 color_inds[16];
	U8 alpha_inds[16];
};

// "Raw" decoded BC7 block description, useful for the BC7 encoder to turn
// encoded bits back into blocks 1:1
struct BC7BlockRawDesc
{
	const BC7ModeDesc * mode;

	// Mode fields from the block
	U8 partition_id;
	U8 rot;
	U8 ind_sel;

	// Endpoint colors _with channel mapping as in BC7 bits_, i.e.
	// channel rotation is applied to it, and the scalar channel is
	// always in "a" (channel 3).
	U8 endpoints_raw[4][6]; // [channel][index]

	// Index bits for index vector 0 and 1
	U8 inds[2][16];
};

// Table describing all the BC7 modes
extern const BC7ModeDesc bc7_modes[8];

// Table describing all the BC6/7 partition shapes
// 0 = 1-subset, 1-64 = 2-subset, 65-128 = 3-subset
extern const BC67Partition bc67_partitions[1 + 64 + 64];

// Decode a BC7 block to RGBA8888 pixels.
// stride_in_bytes is the distance between rows in bytes (so you can decode
// directly to an arbitrary-size RGBA8888 image.)
void bc7_decode_block(U8 * out_rgba, SINTa stride_in_bytes, const void * block);

// Analyze a given BC7 block. The analysis API gives you a more detailed
// description of the block contents, not just the raw pixels.
void bc7_analyze_block(BC7BlockDesc * desc, const void * block);

// Analyze a given BC7 block in "raw" form which is more useful to round-trip
// back to the same BC7 bits. Returns whether the block was well-formed.
bool bc7_analyze_block_raw(BC7BlockRawDesc * desc, const void * block);

struct BC6HModeDesc
{
	U8 linmode; // linear mode id (D3D-style numbering, except 0-based)
	U8 mode; // mode id (Khronos-style numbering)
	U8 ns; // num subsets (1 or 2)
	U8 pb; // partition bits
	U8 epb; // endpoint bits for color values
	U8 db; // delta bits (0=no delta coding)
	U8 bc; // boost channel (index of channel with 1 more delta bit; 3=none)
	U8 ib; // index bits per element
};

// "Raw" decoded BC6H block description, useful for round-tripping encoded BC6H
// blocks back into encoder state
struct BC6HBlockRawDesc
{
	const BC6HModeDesc * mode;

	// Partition ID is also in the header
	U8 partition_id;

	// Endpoints in BC6H quantized format; these are the integers encoded
	// in the block after delta decoding
	S32 endpoints_quant[3][4]; // [channel][index]

	// Endpoints in BC6H internal format, that is, 17-bit signed and without
	// the final dequantization step applied.
	S32 endpoints_internal[3][4]; // [channel][index]

	// Decoded indices
	U8 inds[16];
};

// Decode a BC6H block to R16G16B16A16_FLOAT.
// stride_in_bytes is the distance between rows in bytes (so you can decode
// directly to an arbitrary-size image.)
// "is_signed" is nonzero for signed textures, 0 otherwise.
void bc6h_decode_block(U16 * out_rgba, SINTa stride_in_bytes, rrbool is_signed, const void * block);

// Analyze a given BC6H block in "raw" form which is designed so we can round-trip
// blocks back to the original BC6H bits in the encoder. Returns whether the block
// was well-formed.
bool bc6h_analyze_block_raw(BC6HBlockRawDesc * desc, rrbool is_signed, const void * block);

OODLE_NS_END
