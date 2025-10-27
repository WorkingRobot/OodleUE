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
	// Rules we enforce during validation:
	// 1. surface_id_plus_1 is either 0 (padding) or in bounds for the layout
	// 2. x and y are in bounds for the surface they refer to (and both 0 for padding blocks)
	U32 surface_id_plus_1; // 0=invalid (padding)
	U16 x; // surface x position (pixels)
	U16 y; // surface y position (pixels)
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

