// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "layout.h"
#include "templates/rrnew.h"
#include "templates/rrstl.h"

OODLE_NS_START

OodleTex_Layout::OodleTex_Layout(SINTa block_size)
	: m_tile_w(0), m_tile_h(0), m_block_size(block_size), m_nblocks(0), m_block_ids(NULL)
{
	RR_ASSERT(block_size == 8 || block_size == 16);
}

OodleTex_Layout::~OodleTex_Layout()
{
	if ( m_block_ids )
	{
		OODLE_FREE_ARRAY(m_block_ids, m_nblocks);
	}
}

OodleTex_Err OodleTex_Layout::GenerateBlockIDs(int surface_index, void * block_ids_void, SINTa row_stride_bytes) const
{
	if ( surface_index < 0 || surface_index >= m_surfaces.size32() )
		return OodleTex_Err_InvalidSurfaceIndex;

	U32 surf_id_p1 = check_value_cast<U32>(surface_index) + 1;

	const SurfaceInfo &surf = m_surfaces[surface_index];
	U8 * cur_block_row = (U8 *)block_ids_void;
	S32 width_in_blocks = ( surf.width + 3 ) / 4;

	for ( S32 y = 0; y < surf.height; y += 4 )
	{
		// Clear all to zero for cleanliness even though we don't
		// care about the second half of BlockLocation16's
		memset(cur_block_row, 0, m_block_size * width_in_blocks);

		U8 * cur_block = cur_block_row;
		SINTa step = m_block_size;
		for ( S32 x = 0; x < surf.width; x += 4 )
		{
			// NOTE(fg): as of this writing, all targets we support limit 2D surface sizes to at most 16384x16384,
			// so 16 bits is sufficient. If that ever gets bumped past 65536x65536 but not by much, we can easily
			// store x/4 and y/4 since both these values are always multiples of 4. Beyond that, we would
			// actually need to change BlockLocation8 to have more than 16 bits for x/y, but that seems a ways off.
			BlockLocation8 * loc = (BlockLocation8 *)cur_block;
			loc->surface_id_plus_1 = surf_id_p1;
			loc->x = check_value_cast<U16>(x);
			loc->y = check_value_cast<U16>(y);
			cur_block += step;
		}

		cur_block_row += row_stride_bytes;
	}

	return OodleTex_Err_OK;
}

OodleTex_Err OodleTex_Layout::SetBlockLayout(const void * reordered_block_ids, SINTa num_reordered_blocks)
{
	// Validate the contents first, so that consumers of the data later can rely on it.
	const U8 * block_ids = (const U8 *)reordered_block_ids;
	for (SINTa i = 0; i < num_reordered_blocks; ++i)
	{
		const BlockLocation8 * loc = (BlockLocation8 *)(block_ids + i * m_block_size);

		// Padding blocks must have x, y zero
		if (loc->surface_id_plus_1 == 0)
		{
			if (loc->x != 0 || loc->y != 0)
				return OodleTex_Err_MalformedBlockIDs;
		}
		else
		{
			// NOTE(fg): > not >= because index is 1-based
			if (loc->surface_id_plus_1 > m_surfaces.size())
				return OodleTex_Err_MalformedBlockIDs;

			const SurfaceInfo &surf = m_surfaces[loc->surface_id_plus_1 - 1];
			// blocks are 4x4 so start coordinates should always be multiples of 4
			if ((loc->x | loc->y) & 3)
				return OodleTex_Err_MalformedBlockIDs;

			if (loc->x >= surf.width || loc->y >= surf.height)
				return OodleTex_Err_MalformedBlockIDs;
		}
	}

	// Contents check out! Allocate storage for the replacement array
	BlockLocation8 * new_blocks = OODLE_MALLOC_ARRAY(BlockLocation8, num_reordered_blocks);
	if ( !new_blocks )
		return OodleTex_Err_Internal;

	// ---- nothing can go wrong past this point

	// Copy the location information to our block array
	for (SINTa i = 0; i < num_reordered_blocks; ++i)
	{
		const BlockLocation8 * src = (BlockLocation8 *)(block_ids + i * m_block_size);
		new_blocks[i] = *src;
	}

	// If we already had a layout set, we need to free it.
	// (do this after alloc so we can't fail the alloc after having freed the previous layout)
	if ( m_block_ids )
		OODLE_FREE_ARRAY(m_block_ids, m_nblocks);

	m_nblocks = num_reordered_blocks;
	m_block_ids = new_blocks;
	return OodleTex_Err_OK;
}

OodleTex_Err OodleTex_Layout::InitUniversal(OodleTex_RDO_UniversalTiling tiling)
{
	const int BLOCK_SIZE = 4;

	RR_ASSERT( m_tile_w == 0 && m_tile_h == 0 ); // not initialized as pure tiled yet
	RR_ASSERT( m_nblocks == 0 && m_block_ids == 0 ); // not a conventional layout either

	// Our canonical sizing is for 16B blocks; formats with 8B blocks use
	// tiles of twice the width, other block sizes are unsupported.
	int width_scaling = 0;
	switch ( m_block_size )
	{
	case 8:		width_scaling = 2; break;
	case 16:	width_scaling = 1; break;
	default:	return OodleTex_Err_Internal;
	}

	switch ( tiling )
	{
	case OodleTex_RDO_UniversalTiling_256KB:
		// 256K = 16384 blocks when blocks are 16B, which is 128x128 blocks.
		// We give tile widths in pixels so multiply by block size.
		m_tile_w = 128 * BLOCK_SIZE * width_scaling;
		m_tile_h = 128 * BLOCK_SIZE;
		break;

	case OodleTex_RDO_UniversalTiling_64KB:
		// 64K = 4096 blocks when blocks are 16B, which is 64x64.
		m_tile_w = 64 * BLOCK_SIZE * width_scaling;
		m_tile_h = 64 * BLOCK_SIZE;
		break;

	default:
		return OodleTex_Err_BadUniversalTiling;
	}

	return OodleTex_Err_OK;
}

OODLE_NS_END

