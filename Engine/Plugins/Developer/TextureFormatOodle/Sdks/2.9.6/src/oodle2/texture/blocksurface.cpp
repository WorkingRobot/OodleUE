// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "blocksurface.h"
#include "oodlemalloc.h"
#include "rrsurfaceblit.h"
#include "rrsurfacerowcache.h"
#include "oodletexpub.h" // for OODLETEX_MAX_SURFACE_DIMENSION
#include "layout.h"
#include "threadprofiler.h"

RR_NAMESPACE_START

void BlockSurface_SetView(BlockSurface * bs, const BlockSurface * from)
{	
	*bs = *from;
	bs->freeData = false;
}

void BlockSurface_SetView(BlockSurface * bs, const BlockSurface * from, int start, int count )
{
	BlockSurface_Init(bs);

	RR_ASSERT( start+count <= from->count );
	
	bs->blocks = from->blocks + from->blockSizeBytes * start;
	bs->count = count;
	bs->blockSizeBytes = from->blockSizeBytes;
	bs->freeData = false;
	bs->pixelFormat = from->pixelFormat;
}

void BlockSurface_SetView(BlockSurface * bs, void * blocks, int count, rrPixelFormat pf )
{	
	BlockSurface_Init(bs);
	
	bs->blocks = (U8 *)blocks;
	bs->freeData = false;
	bs->count = count;
	bs->pixelFormat = pf;
	bs->blockSizeBytes = rrPixelFormat_Get4x4BlockSizeBytes(pf);
}

void BlockSurface_Init(BlockSurface * bs)
{
	RR_ZERO(*bs);
	//bs->format = rrPixelFormat_Invalid; // == 0
}

void BlockSurface_Free(BlockSurface * bs)
{
	if ( bs->freeData )
	{
		OodleFree(bs->blocks);
	}
	
	RR_ZERO(*bs);
}

void BlockSurface_AllocCopy(BlockSurface * bs,const BlockSurface * from)
{
	BlockSurface_Alloc(bs, from->count, from->pixelFormat );
	
	SINTa size = BlockSurface_GetDataSizeBytes(bs);
	RR_ASSERT_ALWAYS( size == BlockSurface_GetDataSizeBytes(from) );
	
	memcpy(bs->blocks, from->blocks, size );
}

void BlockSurface_Copy(BlockSurface * to_blocks,const BlockSurface * from_blocks)
{
	RR_ASSERT( to_blocks->blocks != NULL );
	RR_ASSERT( to_blocks->count == from_blocks->count );
	RR_ASSERT( to_blocks->pixelFormat == from_blocks->pixelFormat );
	SINTa bytes = BlockSurface_GetDataSizeBytes(from_blocks);
	RR_ASSERT( bytes == BlockSurface_GetDataSizeBytes(to_blocks) );
	
	memcpy(to_blocks->blocks,from_blocks->blocks,bytes);
}

void BlockSurface_Alloc(BlockSurface * bs,int num_blocks, rrPixelFormat pixelFormat)
{
	if ( bs->count == num_blocks &&
		bs->pixelFormat == pixelFormat )
	{
		return;
	}
	
	BlockSurface_Free(bs);
	
	bs->pixelFormat = pixelFormat;
	bs->freeData = true;
	bs->count = num_blocks;
	bs->blockSizeBytes = rrPixelFormat_Get4x4BlockSizeBytes(pixelFormat);
	
	SINTa size = BlockSurface_GetDataSizeBytes(bs);
	bs->blocks = (U8 *) OodleMallocAligned(size, 64); // 64B fully aligns 16*RGBA8888 blocks, and is decent for larger types
}

rrbool BlockSurface_BlitNormalized(BlockSurface * to_bs,const BlockSurface * fm_bs,rrRangeRemap remap)
{
	RR_ASSERT_ALWAYS( to_bs != fm_bs );
	RR_ASSERT_ALWAYS( to_bs->count == fm_bs->count );

	// do a format changing blit in chunks
	// (to avoid rrSurface size overflows)
	rrSurface rrs_to = { };
	rrSurface rrs_fm = { };
	const SINTa chunk_limit = 1 << 24;
	for (SINTa base = 0; base < fm_bs->count; base += chunk_limit)
	{
		SINTa chunk_size = RR_MIN(fm_bs->count - base, chunk_limit);
		BlockSurface_Set_RRS_View(&rrs_to,to_bs,base,chunk_size);
		BlockSurface_Set_RRS_View(&rrs_fm,fm_bs,base,chunk_size);
		rrbool ok = rrSurface_BlitNormalized(&rrs_to,&rrs_fm);
		if ( ! ok )
			return false;
	}

	return true;
}

void BlockSurface_AllocCopyOrSetViewIfFormatMatches_Normalized(BlockSurface * bs,const BlockSurface * from,rrPixelFormat format,rrRangeRemap remap)
{
	RR_ASSERT( bs != from );
	if ( from->pixelFormat == format && remap == rrRangeRemap_None )
	{
		BlockSurface_SetView(bs,from);
		return;
	}

	// needs a pixel format change
	BlockSurface_Alloc(bs,from->count,format);

	// not for BCN :
	RR_ASSERT( ! rrPixelFormat_IsBlockCompressed(from->pixelFormat) );
	RR_ASSERT( ! rrPixelFormat_IsBlockCompressed(format) );

	// do a format changing blit :
	BlockSurface_BlitNormalized(bs,from,remap);
}

void BlockSurface_SetView_of_RRS_BCN(BlockSurface * bs, const rrSurface * from)
{
	BlockSurface_Init(bs);
	// make this BS a view of the BCN RRS

	// from must be BCN
	RR_ASSERT( rrPixelFormat_IsBlockCompressed(from->pixelFormat) );
	
	//BlockSurface_Free(bs);
	
	bs->blocks = from->data;
	bs->pixelFormat = from->pixelFormat;
	bs->blockSizeBytes = rrPixelFormat_Get4x4BlockSizeBytes(bs->pixelFormat);
	bs->freeData = false;
	bs->count = wh_to_num_blocks(from->width,from->height);
	
	// assumes from stride is tight packed :
	RR_ASSERT_ALWAYS( from->stride == ((from->width + 3) / 4) * bs->blockSizeBytes );
}

void BlockSurface_Set_RRS_View(rrSurface * surf,const BlockSurface * bs,SINTa origin,SINTa count)
{
	// Count == -1 means "until the end"
	if ( count == -1 )
		count = bs->count - origin;

	RR_ASSERT_ALWAYS( 0 <= origin && origin <= bs->count );
	RR_ASSERT_ALWAYS( 0 <= count && count <= bs->count - origin );
	RR_ASSERT_ALWAYS( count <= 0x7fffffff ); // we need width to fit into a S32

	rrSurface_Init(surf);
	// make RRS a view of this BS
	// pixels are in swizzled order because we do not conform to the internal
	// row-major block layout; we could instead set up the surface as
	// 4 x (count * 4), which _does_ confirm to the internal layout, however
	// all our rrSurface processing defaults to line-based, and making those
	// lines be 4 pixels wide would get quite inefficient.

	//rrSurface_Free(surf);
	surf->data = bs->blocks;
	surf->freeData = false;
	surf->width = (S32)count;
	RR_ASSERT( surf->width == count ); // earlier count ASSERT_ALWAYS should have caught this
	surf->height = 16;
	surf->pixelFormat = bs->pixelFormat;
	surf->stride = rrPixelFormat_MakeStride_Minimum(surf->pixelFormat,surf->width);
}

// @@ this file does lots of call memcpy on pixels
//	bypp is in 1-16
//  should be templates with an outer dispatch
// it bothers me philosophically but isn't really practically relevant

static void copy_4x4_block_to_rows_advance(U8ptr to_rows[4],U8cptr & fm_ptr,int bypp)
{
	int bypr = bypp*4;
	
	for LOOP(r,4)
	{
		memcpy(to_rows[r],fm_ptr,bypr);
		to_rows[r] += bypr;
		fm_ptr += bypr;
	}
}

static void copy_4x4_rows_to_block_advance(U8ptr & to_ptr,U8cptr fm_rows[4],int bypp)
{
	int bypr = bypp*4;
	
	for LOOP(r,4)
	{
		memcpy(to_ptr,fm_rows[r],bypr);
		fm_rows[r] += bypr;
		to_ptr += bypr;
	}
}

void BlockSurface_Copy_to_RRS_SameFormat(rrSurface* to_array,int num_surfaces,const BlockSurface * from)
{
	THREADPROFILESCOPE("blksrfcpy");

	RR_ASSERT_ALWAYS( to_array[0].pixelFormat == from->pixelFormat );
	
	int to_block_count = TotalBlockCount(to_array,num_surfaces);
	RR_ASSERT_ALWAYS( from->count == to_block_count );
	
	const U8 * fromPtr = from->blocks;
	const U8 * fromEnd = fromPtr + BlockSurface_GetDataSizeBytes(from);
		
	if ( rrPixelFormat_IsBlockCompressed(from->pixelFormat) )
	{
		for LOOP(i,num_surfaces)
		{
			rrSurface * to = to_array+i;

			// assumes from stride is tight packed :
			//	@@ or copy row by row
			int nbx = (to->width  + 3)/4;
			RR_ASSERT_ALWAYS( to->stride == nbx * from->blockSizeBytes );
		
			SINTa bytes = from->blockSizeBytes * wh_to_num_blocks(to->width,to->height);
			memcpy(to->data,fromPtr,bytes);
			fromPtr += bytes;
		}
	}
	else
	{
		// do 4x4 -> rows
		
		rrSurfaceRowCache toRows;
		int bypp = rrPixelFormat_GetInfo(from->pixelFormat)->bytesPerPixel;
		RR_ASSERT( bypp >= 1 && bypp <= 16 );
		
		for LOOP(i,num_surfaces)
		{
			rrSurface * to = to_array+i;

			toRows.Start(to,from->pixelFormat,8,RR_SURFACE_ROW_CACHE_WRITE,4);
			
			// RowCache padding takes care of partial blocks for us
			
			int nbx = (to->width  + 3)/4;

			// work on rows of blocks :
			for(int y=0;y<to->height;y+=4)
			{			
				toRows.MoveCache(y,4);
			
				U8 * rows[4];
				for(int r=0;r<4;r++)
					rows[r] = (U8 *) toRows.GetRow(y + r);
			
				// @@ todo template switch on bypp
				// bypp is in [1,16]
			
				// for each block :
				for LOOP(bx,nbx)
				{
					copy_4x4_block_to_rows_advance(rows,fromPtr,bypp);
					// fromPtr is advanced
				}
			}

			toRows.FlushWrite();
		}
	}

	RR_ASSERT_ALWAYS( fromPtr == fromEnd );	
}

void BlockSurface_Copy_to_RRS_SameFormat_Layout(rrSurface* to_surfaces,int num_surfaces,const BlockSurface * from,const OodleTex_Layout * layout)
{
	if ( !layout )
	{
		BlockSurface_Copy_to_RRS_SameFormat(to_surfaces,num_surfaces,from);
		return;
	}
	else if ( layout->m_tile_w != 0 )
	{
		RR_ASSERT_FAILURE_ALWAYS("copy of universal layout");
		return;
	}
	
	THREADPROFILESCOPE("blksrfcpy");

	RR_ASSERT( num_surfaces == layout->m_surfaces.size32() );
	RR_ASSERT_ALWAYS( to_surfaces->pixelFormat == from->pixelFormat );

	if ( ! rrPixelFormat_IsBlockCompressed(from->pixelFormat) )
	{
		// do 4x4 -> rows

		const U8 * blockPtr = from->blocks;
		
		int bypp = rrPixelFormat_GetInfo(from->pixelFormat)->bytesPerPixel;
		RR_ASSERT( bypp >= 1 && bypp <= 16 );

		RR_ASSERT( from->blockSizeBytes == bypp*16 );

		for (SINTa bi = 0; bi < layout->m_nblocks; ++bi)
		{
			const BlockLocation8 * loc = layout->m_block_ids + bi;
			if ( loc->surface_id_plus_1 == 0 )
			{
				// intentional null block
				blockPtr += from->blockSizeBytes;
				continue;
			}

			rrSurface * to = to_surfaces + loc->surface_id_plus_1 - 1;

			// just assert not assert_always since layout creation does validate this
			int to_x = loc->x;
			int to_y = loc->y;

			if ( to_x < 0 )
			{
				// intentional null block
				blockPtr += from->blockSizeBytes;
				continue;
			}

			RR_ASSERT( to_x >= 0 && to_x < to->width );
			RR_ASSERT( to_y >= 0 && to_y < to->height );

			// @@ todo template switch on bypp
			// bypp is in [1,16]

			// need to handle partial blocks here, don't have RowCache to help
			if ( to_x+4 > to->width || to_y+4 > to->height )
			{
				for LOOP(by,4)
				{
					for LOOP(bx,4)
					{
						// clamp coordinate, repeat boundary:
						int x = RR_MIN(to_x+bx,to->width-1);
						int y = RR_MIN(to_y+by,to->height-1);

						U8 * toPtr = rrSurface_Seek(to,x,y);

						memcpy(toPtr,blockPtr,bypp);
						blockPtr += bypp;
					}
				}
			}
			else
			{
				U8 * rows[4];
				for(int r=0;r<4;r++)
					rows[r] = rrSurface_Seek(to,to_x,to_y+r);

				copy_4x4_block_to_rows_advance(rows,blockPtr,bypp);
			}
		}
	}
	else
	{
		// do blocks -> blocks
		// for OodleTex_Layout_CopyBCNToLinear

		const U8 * blockPtr = from->blocks;
		
		int bypb = from->blockSizeBytes;
		RR_ASSERT( bypb == 8 || bypb == 16 );

		for (SINTa bi = 0; bi < layout->m_nblocks; ++bi)
		{
			const BlockLocation8 * loc = layout->m_block_ids + bi;
			if ( loc->surface_id_plus_1 == 0 )
			{
				// intentional null block
				blockPtr += bypb;
				continue;
			}

			rrSurface * to = to_surfaces + loc->surface_id_plus_1 - 1;

			// just assert not assert_always since layout creation does validate this
			int to_x = loc->x;
			int to_y = loc->y;

			if ( to_x < 0 )
			{
				// intentional null block
				blockPtr += bypb;
				continue;
			}

			RR_ASSERT( to_x >= 0 && to_x < to->width );
			RR_ASSERT( to_y >= 0 && to_y < to->height );

			U8 * to_ptr = rrSurface_Seek(to,to_x,to_y);
			
			//memcpy(to_ptr,blockPtr,bypb);
			
			RR_ASSERT( bypb == 8 || bypb == 16 );
			if ( bypb == 8 )
				memcpy(to_ptr,blockPtr,8);
			else
				memcpy(to_ptr,blockPtr,16);
			
			blockPtr += bypb;
		}
	}
}

int TotalBlockCount(const rrSurface * from,int num_surfaces)
{
	int block_count = 0;
	for LOOP(i,num_surfaces)
	{
		block_count += wh_to_num_blocks(from[i].width,from[i].height);
	}
	return block_count;
}

int TotalTileCount(const rrSurface * from,int num_surfaces,int tile_w,int tile_h)
{
	int tile_count = 0;
	for LOOP(i,num_surfaces)
	{
		int w_tiles = (from[i].width + tile_w-1) / tile_w;
		int h_tiles = (from[i].height + tile_h-1) / tile_h;
		tile_count += w_tiles * h_tiles;
	}
	return tile_count;
}

void BlockSurface_AllocCopy_from_RRS(BlockSurface* to,const rrSurface * from_array,int num_surfaces)
{
	THREADPROFILESCOPE("blksrfcpy");

	int block_count = TotalBlockCount(from_array,num_surfaces);

	BlockSurface_Alloc(to,block_count,from_array->pixelFormat);

	if ( rrPixelFormat_IsBlockCompressed(from_array->pixelFormat) )
	{
		U8 * toPtr = to->blocks;

		for LOOP(i,num_surfaces)
		{
			RR_ASSERT( from_array[i].pixelFormat == from_array[0].pixelFormat );

			// assumes from stride is tight packed :
			//	@@ or copy row by row
			int nbx = (from_array[i].width + 3)/4;
			RR_ASSERT_ALWAYS( from_array[i].stride == nbx * to->blockSizeBytes );

			int cur_num_blocks = wh_to_num_blocks(from_array[i].width,from_array[i].height);
			SINTa cur_size_bytes = to->blockSizeBytes * cur_num_blocks;
			memcpy(toPtr,from_array[i].data,cur_size_bytes);
			toPtr += cur_size_bytes;
		}

		SINTa bytes = BlockSurface_GetDataSizeBytes(to);
		U8 * toEnd = to->blocks + bytes;
		RR_ASSERT_ALWAYS( toPtr == toEnd );
	}
	else
	{
		// do 4x4 -> rows

		U8 * toPtr = to->blocks;
		int bypp = rrPixelFormat_GetInfo(from_array[0].pixelFormat)->bytesPerPixel;
		RR_ASSERT( bypp >= 1 && bypp <= 16 );

		// @@ todo template switch on bypp
		// bypp is in [1,16]

		rrSurfaceRowCache fmRows;

		for LOOP(i,num_surfaces)
		{
			RR_ASSERT( from_array[i].pixelFormat == from_array[0].pixelFormat );

			const rrSurface * from = &from_array[i];

			int nbx = (from_array[i].width + 3)/4;

			fmRows.StartReadC(from,from->pixelFormat,8,4);

			// RowCache padding takes care of partial blocks for us

			// work on rows of blocks :
			for(int y=0;y<from->height;y+=4)
			{
				fmRows.MoveCache(y,4);

				const U8 * rows[4];
				for(int r=0;r<4;r++)
					rows[r] = (U8 *) fmRows.GetRow(y + r);

				// for each block :
				for LOOP(bx,nbx)
				{
					copy_4x4_rows_to_block_advance(toPtr,rows,bypp);
					// advances toPtr
				}
			}
		}

		U8 * toEnd = to->blocks + to->count * (SINTa) to->blockSizeBytes;
		RR_ASSERT_ALWAYS( toPtr == toEnd );
	}

}

void BlockSurface_AllocCopy_from_RRS_Tiled(BlockSurface* to,const rrSurface * from_array,int num_surfaces,int tile_width,int tile_height)
{
	THREADPROFILESCOPE("blksrfcpy");

	const int BLOCK_SIZE = 4;

	// Tile sizes must be a multiple of 4x4 pixels for this to work correctly
	RR_ASSERT( tile_width > 0 && tile_width % BLOCK_SIZE == 0 );
	RR_ASSERT( tile_height > 0 && tile_height % BLOCK_SIZE == 0 );

	// We pad out to full tiles to match behavior of most texture tiling schemes
	const int tile_width_blocks = tile_width / BLOCK_SIZE;
	const int tile_height_blocks = tile_height / BLOCK_SIZE;
	const int tile_count = TotalTileCount(from_array,num_surfaces,tile_width,tile_height);
	const int blocks_per_tile = tile_width_blocks * tile_height_blocks;
	const int block_count = blocks_per_tile * tile_count;
	
	BlockSurface_Alloc(to,block_count,from_array->pixelFormat);
	
	// Stride between rows within a tile in bytes
	const SINTa tile_row_stride = tile_width_blocks * (SINTa)to->blockSizeBytes;
	const SINTa tile_size_bytes = tile_row_stride * tile_height_blocks;

	if ( rrPixelFormat_IsBlockCompressed(from_array->pixelFormat) )
	{
		U8 * toPtr = to->blocks;

		for LOOP(i,num_surfaces)
		{
			const rrSurface * from = &from_array[i];
			RR_ASSERT( from->pixelFormat == from_array[0].pixelFormat );

			// This is much easier when the pixel format is already using blocks
			// because we just need to memcpy strips around and not also gather
			// 4x4 blocks and handle padding/boundary cases at the same time

			// Traverse over tiles
			for (int ty = 0; ty < from->height; ty += tile_height)
			{
				int this_tile_height = RR_MIN(tile_height, from->height - ty);

				for (int tx = 0; tx < from->width; tx += tile_width)
				{
					// Copy row by row within tiles
					const int this_tile_width = RR_MIN(tile_width, from->width - tx);
					const int this_tile_width_blocks = (this_tile_width + BLOCK_SIZE-1) / BLOCK_SIZE;
					const SINTa tile_content_bytes = to->blockSizeBytes * this_tile_width_blocks;
					const SINTa row_padding_bytes = (tile_width_blocks - this_tile_width_blocks) * to->blockSizeBytes;

					const U8 * fromPtr = rrSurface_SeekC(from,tx,ty);
					int y = 0;
					for (; y < this_tile_height; y += BLOCK_SIZE)
					{
						memcpy(toPtr,fromPtr,tile_content_bytes);
						fromPtr += from->stride;

						// If there's padding at the right, insert all-0 blocks out to the edge of the tile
						if (row_padding_bytes)
							memset(toPtr + tile_content_bytes,0,row_padding_bytes);

						toPtr += tile_row_stride;
					}

					// If this tile is vertically truncated, insert padding at the bottom
					if (y < tile_height)
					{
						int bottom_padding_height_blocks = (tile_height - y) / BLOCK_SIZE;
						SINTa bottom_padding_bytes = bottom_padding_height_blocks * tile_row_stride;
						memset(toPtr,0,bottom_padding_bytes);
						toPtr += bottom_padding_bytes;
					}
				}
			}
		}
		
		SINTa bytes = BlockSurface_GetDataSizeBytes(to);
		U8 * toEnd = to->blocks + bytes;
		RR_ASSERT_ALWAYS( toPtr == toEnd );
	}
	else
	{
		// do 4x4 -> rows

		U8 * toPtr = to->blocks;
		int bypp = rrPixelFormat_GetInfo(from_array[0].pixelFormat)->bytesPerPixel;
		RR_ASSERT( bypp >= 1 && bypp <= 16 );

		// @@ todo template switch on bypp
		// bypp is in [1,16]

		rrSurfaceRowCache fmRows;

		for LOOP(i,num_surfaces)
		{
			const rrSurface * from = &from_array[i];
			RR_ASSERT( from->pixelFormat == from_array[0].pixelFormat );

			// Surface width in blocks and tiles
			const int width_in_blocks = (from->width + BLOCK_SIZE-1) / BLOCK_SIZE;
			const int width_in_tiles = (from->width + tile_width-1) / tile_width;

			// Horizontal padding amount in blocks to make width be a multiple of the tile size
			const int horiz_padding_in_blocks = width_in_tiles * tile_width_blocks - width_in_blocks;
			const SINTa row_padding_bytes = (SINTa)horiz_padding_in_blocks * to->blockSizeBytes;

			// Advance between tile columns, i.e. how far to advance the destination pointer
			// as we skip across a horizontal tile boundary
			const SINTa tile_column_advance = tile_size_bytes - tile_row_stride;

			fmRows.StartReadC(from,from->pixelFormat,8,4);
			// RowCache padding takes care of partial blocks for us

			// We want to copy by tiles, but not keep reallocating new surface caches, so we
			// loop over rows in the original image but need to keep track of the tile structure
			// so we know where to blit to
			for (int ty = 0; ty < from->height; ty += tile_height)
			{
				// Keep track of destination pointer at the top left of this tile to re-seek later,
				// since we skip around a bunch.
				U8 * toPtrTile = toPtr;

				// Compute dimension of this row of tiles in pixels
				const int this_tile_height = RR_MIN(tile_height, from->height - ty);

				// Work on rows of blocks, but staying within this row of tiles(ity = in-tile y)
				int ity = 0;
				for(; ity < this_tile_height; ity += BLOCK_SIZE)
				{
					const int y = ty + ity;

					// Re-compute destination address from stored tile base; we computed the row stride
					// earlier.
					toPtr = toPtrTile + (SINTa)(ity / BLOCK_SIZE) * tile_row_stride;
					fmRows.MoveCache(y,4);

					const U8 * rows[4];
					for(int r=0;r<4;r++)
						rows[r] = (U8 *) fmRows.GetRow(ty + ity + r);

					// Columns left in current tile; whenever we complete a tile, we need to skip
					// our destination pointer ahead.
					int cols_left = tile_width_blocks;

					for LOOP(bx,width_in_blocks)
					{
						copy_4x4_rows_to_block_advance(toPtr,rows,bypp);
						// advances toPtr

						// Whenever we run out of columns in the current tile, need to advance all the way
						// to the next tile
						if (--cols_left == 0)
						{
							toPtr += tile_column_advance;
							cols_left = tile_width_blocks;
						}
					}

					// If there's padding at the end of this row, add it
					// we pad to a tile multiple so this never takes us out of the current tile
					if (row_padding_bytes)
						memset(toPtr,0,row_padding_bytes);
				}

				// If this row of tiles is vertically truncated, insert padding at the bottom
				if (ity < tile_height)
				{
					int bottom_padding_height_blocks = (tile_height - ity) / BLOCK_SIZE;
					SINTa bottom_padding_bytes_per_tile = bottom_padding_height_blocks * tile_row_stride;

					// Determine position within tile to start zeroing
					toPtr = toPtrTile + (SINTa)(ity / BLOCK_SIZE) * tile_row_stride;

					// Zero bottom part of every affected tile
					for LOOP(tx,width_in_tiles)
						memset(toPtr + tx * tile_size_bytes,0,bottom_padding_bytes_per_tile);
				}

				// After this tile row is completed, advance to next row.
				toPtr = toPtrTile + width_in_tiles * tile_size_bytes;
			}
		}

		U8 * toEnd = to->blocks + to->count * to->blockSizeBytes;
		RR_ASSERT_ALWAYS( toPtr == toEnd );
	}
}

void BlockSurface_AllocCopy_from_RRS_Layout(BlockSurface* to,const rrSurface * from_surfaces,int num_surfaces,const OodleTex_Layout * layout)
{
	if ( !layout )
	{
		BlockSurface_AllocCopy_from_RRS(to,from_surfaces,num_surfaces);
		return;
	}
	else if ( layout->m_tile_w != 0)
	{
		BlockSurface_AllocCopy_from_RRS_Tiled(to,from_surfaces,num_surfaces,layout->m_tile_w,layout->m_tile_h);
		return;
	}

	THREADPROFILESCOPE("blksrfcpy");

	RR_ASSERT( layout->m_surfaces.size32() == num_surfaces );

	// TODO int counts seem bad here
	BlockSurface_Alloc(to,check_value_cast<int>(layout->m_nblocks),from_surfaces->pixelFormat);

	U8 * toPtr = to->blocks;
	U8 * toPtrEnd = to->blocks + BlockSurface_GetDataSizeBytes(to);

	if ( rrPixelFormat_IsBlockCompressed(from_surfaces->pixelFormat) )
	{
		int bypb = to->blockSizeBytes;
		RR_ASSERT( bypb == 8 || bypb == 16 );
		
		SINTa num_nulls = 0;
		for (SINTa bi = 0; bi < layout->m_nblocks; ++bi)
		{
			const BlockLocation8 * loc = layout->m_block_ids + bi;

			if ( loc->surface_id_plus_1 == 0 )
			{
				// intentional null block
				memset(toPtr,0,bypb);
				toPtr += bypb;
				num_nulls++;
				continue;
			}

			const rrSurface * from = from_surfaces + loc->surface_id_plus_1 - 1;

			// just assert not assert_always since layout creation does validate this
			S32 fm_x = loc->x;
			S32 fm_y = loc->y;
			RR_ASSERT( fm_x >= 0 && fm_x < from->width );
			RR_ASSERT( fm_y >= 0 && fm_y < from->height );
			
			const U8 * fmPtr = rrSurface_SeekC(from,fm_x,fm_y);

			memcpy(toPtr,fmPtr,bypb);
			toPtr += bypb;
		}
	}
	else
	{
		int bypp = rrPixelFormat_GetInfo(from_surfaces->pixelFormat)->bytesPerPixel;
		RR_ASSERT( bypp >= 1 && bypp <= 16 );

		RR_ASSERT( to->blockSizeBytes == bypp*16 );

		SINTa num_nulls = 0;
		for (SINTa bi = 0; bi < layout->m_nblocks; ++bi)
		{
			const BlockLocation8 * loc = layout->m_block_ids + bi;

			if ( loc->surface_id_plus_1 == 0 )
			{
				// intentional null block
				memset(toPtr,0,to->blockSizeBytes);
				toPtr += to->blockSizeBytes;
				num_nulls++;
				continue;
			}

			const rrSurface * from = from_surfaces + loc->surface_id_plus_1 - 1;

			// just assert not assert_always since layout creation does validate this
			S32 fm_x = loc->x;
			S32 fm_y = loc->y;
			RR_ASSERT( fm_x >= 0 && fm_x < from->width );
			RR_ASSERT( fm_y >= 0 && fm_y < from->height );

			// @@ todo template switch on bypp
			// bypp is in [1,16]

			// need to handle partial blocks here, don't have RowCache to help
			if ( fm_x+4 > from->width || fm_y+4 > from->height )
			{
				for LOOP(by,4)
				{
					for LOOP(bx,4)
					{
						// clamp coordinate, repeat boundary:
						int x = RR_MIN(fm_x+bx,from->width-1);
						int y = RR_MIN(fm_y+by,from->height-1);

						const U8 * fmPtr = rrSurface_SeekC(from,x,y);

						memcpy(toPtr,fmPtr,bypp);
						toPtr += bypp;
					}
				}
			}
			else
			{
				const U8 * rows[4];
				for(int r=0;r<4;r++)
					rows[r] = rrSurface_SeekC(from,fm_x,fm_y+r);

				copy_4x4_rows_to_block_advance(toPtr,rows,bypp);
			}
		}
	}
	
	// make sure we wrote every byte :
	RR_ASSERT_ALWAYS( toPtr == toPtrEnd );
}

void BlockSurface_Copy_Detile(BlockSurface * to_bs,const BlockSurface * fm_bs,const rrSurface * dim_surfaces,int num_surfaces,int tile_width,int tile_height)
{
	THREADPROFILESCOPE("blksrfcpy");

	const int BLOCK_SIZE = 4;

	// Tile sizes must be a multiple of 4x4 pixels for this to work correctly
	RR_ASSERT( tile_width > 0 && tile_width % BLOCK_SIZE == 0 );
	RR_ASSERT( tile_height > 0 && tile_height % BLOCK_SIZE == 0 );

	RR_DURING_ASSERT( int block_count = TotalBlockCount(dim_surfaces,num_surfaces) );
	RR_ASSERT( fm_bs->count >= to_bs->count ); // fm_bs will, in general, have padding
	RR_ASSERT( block_count == to_bs->count ); // to_bs should be linear and dense
	RR_ASSERT( to_bs->pixelFormat == fm_bs->pixelFormat );
	RR_ASSERT( to_bs->blockSizeBytes == fm_bs->blockSizeBytes );

	const int tile_width_blocks = tile_width / BLOCK_SIZE;
	const int tile_height_blocks = tile_height / BLOCK_SIZE;
	const int blocks_per_tile = tile_width_blocks * tile_height_blocks;

	const SINTa tile_row_stride = tile_width_blocks * (SINTa)fm_bs->blockSizeBytes;
	const SINTa tile_size_bytes = tile_row_stride * tile_height_blocks;

	if ( rrPixelFormat_IsBlockCompressed(fm_bs->pixelFormat) )
	{
		const SINTa bypb = to_bs->blockSizeBytes;
		const SINTa to_total_bytes = BlockSurface_GetDataSizeBytes(to_bs);
		const SINTa fm_total_bytes = BlockSurface_GetDataSizeBytes(fm_bs);

		U8 * toPtr = to_bs->blocks;
		U8 * toEnd = to_bs->blocks + to_total_bytes;

		const U8 * fmPtr = fm_bs->blocks;
		const U8 * fmEnd = fm_bs->blocks + fm_total_bytes;

		for LOOP(i,num_surfaces)
		{
			const rrSurface * surf = &dim_surfaces[i]; // only used to get surface dimension

			// Compute width and height in units of blocks and tiles
			const int width_in_blocks = (surf->width + BLOCK_SIZE-1) / BLOCK_SIZE;
			const int height_in_blocks = (surf->height + BLOCK_SIZE-1) / BLOCK_SIZE;

			const int width_in_tiles = (surf->width + tile_width-1) / tile_width;
			const int height_in_tiles = (surf->height + tile_height-1) / tile_height;

			// Check output buffer sizes
			const SINTa nblocks = (SINTa)width_in_blocks * height_in_blocks;
			const SINTa ntiles = (SINTa)width_in_tiles * height_in_tiles;

			RR_ASSERT( toEnd - toPtr >= nblocks * bypb );
			RR_ASSERT( fmEnd - fmPtr >= ntiles * tile_size_bytes );

			// Compute stride between rows in the linear (to) surface
			const SINTa to_row_stride = width_in_blocks * bypb;

			// Traverse over tiles; this is sequential in the tiled (from) surface
			// but skips around in the untiled (to) surface
			for (int ty = 0; ty < surf->height; ty += tile_height)
			{
				int this_tile_height = RR_MIN(tile_height, surf->height - ty);

				for (int tx = 0; tx < surf->width; tx += tile_width)
				{
					// Copy row by row within tiles
					const int this_tile_width = RR_MIN(tile_width, surf->width - tx);
					const int this_tile_width_blocks = (this_tile_width + BLOCK_SIZE-1) / BLOCK_SIZE;
					const SINTa tile_content_bytes = bypb * this_tile_width_blocks;

					// Destination address in row-major order from the start of the surface
					U8 * toTilePtr = toPtr + (ty / BLOCK_SIZE) * to_row_stride + (tx / BLOCK_SIZE) * bypb;

					// ity = in-tile y
					int ity = 0;
					for (; ity < this_tile_height; ity += BLOCK_SIZE)
					{
						memcpy(toTilePtr,fmPtr,tile_content_bytes);
						fmPtr += tile_row_stride;
						toTilePtr += to_row_stride;
					}

					// If this tile is partial height, skip over the ignored rows at the bottom
					fmPtr += tile_row_stride * (tile_height - ity)/BLOCK_SIZE;
				}
			}

			// We did all the blocks!
			toPtr += nblocks * bypb;
		}

		RR_ASSERT_ALWAYS( toPtr == toEnd );
		RR_ASSERT_ALWAYS( fmPtr == fmEnd );
	}
	else
	{
		// NYI, we have no use case for this currently
		// see logic in AllocCopy_from_RRS_Tiled for how it would work
		RR_ASSERT_FAILURE_ALWAYS("detile of non-compressed blocks NYI");
	}
}

RR_NAMESPACE_END
