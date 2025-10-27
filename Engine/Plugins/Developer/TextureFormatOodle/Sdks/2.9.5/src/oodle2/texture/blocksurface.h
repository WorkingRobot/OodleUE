// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrsurface.h"
#include "rrsurfaceutil.h" // rrRangeRemap
#include "rrpixelformat.h"

RR_NAMESPACE_START

//============================================================

// SINTa rrPixelFormat_Get4x4BlockSizeBytes(rrPixelFormat format)

struct OodleTex_Layout;

struct BlockSurface
{
	U8 *	blocks; // data ? 
	rrbool	freeData;
	int		count;
	int		blockSizeBytes;
	rrPixelFormat pixelFormat; // to match rrSurface variable name
};

void BlockSurface_Init(BlockSurface * bs);
void BlockSurface_Free(BlockSurface * bs);

static RADINLINE int wh_to_num_blocks(int w,int h)
{
	int nbx = (w+3)/4;
	int nby = (h+3)/4;
	return nbx*nby;
}

static RADINLINE SINTa BlockSurface_GetDataSizeBytes(const BlockSurface * bs)
{
	RR_ASSERT( bs->blockSizeBytes == rrPixelFormat_Get4x4BlockSizeBytes(bs->pixelFormat) );
	return bs->count * (SINTa) bs->blockSizeBytes;
}

static RADINLINE U8 * BlockSurface_Seek(BlockSurface * bs,int bi)
{
	RR_ASSERT( bi >= 0 && bi < bs->count );
	RR_ASSERT( bs->blockSizeBytes != 0 );
	
	return bs->blocks + bs->blockSizeBytes * (SINTa) bi;
}
static RADINLINE const U8 * BlockSurface_SeekC(const BlockSurface * bs,int bi)
{
	RR_ASSERT( bi >= 0 && bi < bs->count );
	
	return bs->blocks + bs->blockSizeBytes * (SINTa) bi;
}

void BlockSurface_Alloc(BlockSurface * bs,int num_blocks, rrPixelFormat format);

void BlockSurface_AllocCopy(BlockSurface * bs,const BlockSurface * from);

// Copy identical size and format :
void BlockSurface_Copy(BlockSurface * to_blocks,const BlockSurface * from_blocks);

void BlockSurface_SetView(BlockSurface * bs, const BlockSurface * from);
void BlockSurface_SetView(BlockSurface * bs, const BlockSurface * from, int start, int count );

void BlockSurface_SetView(BlockSurface * bs, void * blocks, int count, rrPixelFormat pf );

#ifdef __RAD64__
OOINLINE void BlockSurface_SetView(BlockSurface * bs, void * blocks, SINTa count, rrPixelFormat pf )
{
	int icount = (int)count;
	RR_ASSERT_ALWAYS( (SINTa)icount == count );
	BlockSurface_SetView(bs,blocks,icount,pf);
}
#endif

// Blit can change formats :
//rrbool BlockSurface_Blit_NonNormalized(BlockSurface * to_bs,const BlockSurface * fm_bs);
rrbool BlockSurface_BlitNormalized(BlockSurface * to_bs,const BlockSurface * fm_bs,rrRangeRemap remap=rrRangeRemap_None);

// use Normalized!
//void BlockSurface_AllocCopyOrSetViewIfFormatMatches_NonNormalized(BlockSurface * bs,const BlockSurface * from,rrPixelFormat format);
void BlockSurface_AllocCopyOrSetViewIfFormatMatches_Normalized(BlockSurface * bs,const BlockSurface * from,rrPixelFormat format,rrRangeRemap remap=rrRangeRemap_None);

// for BCN surfaces, BlockSurface is the same as rrSurface, they can just View each other
// for pixel surfaces you have to copy & shuffle

// make a BlockSurface View of a BCN RRS :
void BlockSurface_SetView_of_RRS_BCN(BlockSurface * bs, const rrSurface * from);

// make an RRS that views the BS contents :
//	(for non-BCN the pixels are swizzled in block order)
//	height of the surf produced is always 4
void BlockSurface_Set_RRS_View(rrSurface * surf,const BlockSurface * bs);

// BlockSurface_Copy_to_RRS : RRS should be allocated first!
void BlockSurface_Copy_to_RRS_SameFormat(rrSurface* to,int num_surfaces,const BlockSurface * from);

// Same as above, but traverse destination surfaces in row-major tiels or tile_width*tile_height pixels.
// For universal tiling.
void BlockSurface_Copy_to_RRS_SameFormat_Tiled(rrSurface* to_surfaces,int num_surfaces,const BlockSurface * from,int tile_width,int tile_height);

void BlockSurface_Copy_to_RRS_SameFormat_Layout(rrSurface* to_surfaces,int num_surfaces,const BlockSurface * from,const OodleTex_Layout * layout);


int TotalBlockCount(const rrSurface * from,int num_surfaces);

// allocs [to] in same format as [from]
void BlockSurface_AllocCopy_from_RRS(BlockSurface* to,const rrSurface * from,int num_surfaces);

// Same as above, but traverse source surfaces in row-major tiles of tile_width*tile_height pixels.
// For universal tiling.
void BlockSurface_AllocCopy_from_RRS_Tiled(BlockSurface* to,const rrSurface * from,int num_surfaces,int tile_width,int tile_height);

// [layout] describes the physical storage order of blocks from (several) [from_surfaces]
void BlockSurface_AllocCopy_from_RRS_Layout(BlockSurface* to,const rrSurface * from_surfaces,int num_surfaces,const OodleTex_Layout * layout);

// Copies from a block surface that has blocks in universal tiling order to a linear-layout one.
// Tiling matches the specified set of source rrSurfaces, which are only needed for their
// dimensions.
void BlockSurface_Copy_Detile(BlockSurface * to_bs,const BlockSurface * fm_bs,const rrSurface * dim_surfaces,int num_surfaces,int tile_width,int tile_height);

//============================================================

struct BlockSurfaceObj : public BlockSurface
{
public:
	 BlockSurfaceObj() { BlockSurface_Init(this); }
	~BlockSurfaceObj() { BlockSurface_Free(this); }

private:
	RR_FORBID_CLASS_STANDARDS(BlockSurfaceObj);
};

//============================================================

RR_NAMESPACE_END
