// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodletexpub.h"
#include "templates/rrvector.h"

OODLE_NS_START

// Block location information for 8-byte block formats
// All-zero blocks must identify padding
struct BlockLocation8
{
	// Largest value allowed in any of the fields
	static constexpr int kLimit = (1 << 21) - 1;

	// Rules we enforce during validation:
	// 1. surface_id_plus_1 is either 0 (padding) or in bounds for the layout
	// 2. x and y are in bounds for the surface they refer to (and both 0 for padding blocks)
	//
	// we require 0 <= x, y <= kLimit (all 21 bits)
	// as well as 0 <= surface_id_plus_1 <= kLimit + 1 for symmetry
	U64 surface_id_plus_1 : 22; // 0=invalid (padding)
	U64 x : 21; // surface x position (pixels)
	U64 y : 21; // surface y position (pixels)
};
RR_COMPILER_ASSERT(sizeof(BlockLocation8) == 8);

struct OodleTex_Layout
{
	explicit OodleTex_Layout(SINTa block_size); // block size in bytes
	~OodleTex_Layout();

	OodleTex_Err GenerateBlockIDs(int surface_index, void * block_ids, SINTa block_id_row_stride_bytes) const;
	OodleTex_Err SetBlockLayout(const void * reordered_block_ids, SINTa num_reordered_blocks);

	// Set up this layout for universal tiling
	OodleTex_Err InitUniversal(OodleTex_RDO_UniversalTiling tiling);

	struct SurfaceInfo
	{
		S32 width;
		S32 height;
	};

	int m_tile_w; // 0 for "proper" layouts; non-0 indicates a pure tiled layout with this tile width
	int m_tile_h; // 0 for "proper" layouts; non-0 indicates a pure tiled layout with this tile height

	SINTa m_block_size;
	SINTa m_nblocks;
	BlockLocation8 * m_block_ids;
	vector<SurfaceInfo> m_surfaces;
};

OODLE_NS_END

