// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRRBITMAP_SURFACEDXTC_H__
#define __RADRRBITMAP_SURFACEDXTC_H__

#include "rrsurface.h"
#include "rrdxtcenums.h"

RR_NAMESPACE_START

//--------------------------------------------------------------
// Compress will alloc dest
//	note that to->widht & height gets the *true* size even if not a multiple of 4
//	but we actually alloc for padded-to-4 and fill that much data

struct BlockSurface;

// int dxt1QualityLevel = 1-3 
// BC1 = DXT1 ; 4 bpp format
rrbool rrSurfaceDXTC_CompressBC1(BlockSurface * to, const BlockSurface * from, rrDXTCLevel dxtcQualityLevel, rrDXTCOptions options = rrDXTCOptions_Default );

// BC2 = DXT3 = 4 bit alpha per pixel ; 8 bpp format
rrbool rrSurfaceDXTC_CompressBC2(BlockSurface * to, const BlockSurface * from, rrDXTCLevel dxtcQualityLevel, rrDXTCOptions options = rrDXTCOptions_Default );

// BC3 = DXT5 = 8 bit alpha interpolated ; 8 bpp format
rrbool rrSurfaceDXTC_CompressBC3(BlockSurface * to, const BlockSurface * from, rrDXTCLevel dxtcQualityLevel, rrDXTCOptions options = rrDXTCOptions_Default );

// BC4 = DXT5 on R channel ; 4 bpp format
rrbool rrSurfaceDXTC_CompressBC4(BlockSurface * to, const BlockSurface * from, rrDXTCLevel dxtcQualityLevel, rrDXTCOptions options = rrDXTCOptions_Default );

// BC5 = DXT5 on R & G channels ; 8 bpp format
rrbool rrSurfaceDXTC_CompressBC5(BlockSurface * to, const BlockSurface * from, rrDXTCLevel dxtcQualityLevel, rrDXTCOptions options = rrDXTCOptions_Default );

//--------------------------------------------------------------
// Decompress *does* alloc dest if it needs a format change

rrbool rrSurfaceDXTC_DecompressBC1(BlockSurface * to, const BlockSurface * from );

rrbool rrSurfaceDXTC_DecompressBC2(BlockSurface * to, const BlockSurface * from );

rrbool rrSurfaceDXTC_DecompressBC3(BlockSurface * to, const BlockSurface * from );

rrbool rrSurfaceDXTC_DecompressBC4(BlockSurface * to, const BlockSurface * from, rrPixelFormat format_hint = rrPixelFormat_Invalid );

rrbool rrSurfaceDXTC_DecompressBC5(BlockSurface * to, const BlockSurface * from, rrPixelFormat format_hint = rrPixelFormat_Invalid );

// in rrSurfaceBC7.h :
// rrSurfaceDXTC_DecompressBC7

//--------------------------------------------------------------

// BlockSurface_DecompressBCN will decode into [to] if it likes the dest format
//	 if it does NOT like the dest format, it will allocate and to->blocks will change!
// format_hint is used to tell BC4/5 whether to decode to 8 or 16 bits/channel (default is 16)
rrbool BlockSurface_DecompressBCN(BlockSurface * to, const BlockSurface * from, rrPixelFormat format_hint = rrPixelFormat_Invalid );

//--------------------------------------------------------------

// allocates to in format of rrSurfaceDXTC_GetBCNDecompFormat(from) if not already allocated
//  uses from->pixelFormat to see which BCN it is
// format_hint is used to tell BC4/5 whether to decode to 8 or 16 bits/channel (default is 16)
rrbool rrSurfaceDXTC_DecompressBCN(rrSurface * to, const rrSurface * from, rrPixelFormat format_hint = rrPixelFormat_Invalid );

// handy utility to remove BCN if present :
rrbool rrSurfaceDXTC_DecompressIfBCNInPlace(rrSurface * surf, rrPixelFormat format_hint = rrPixelFormat_Invalid );

// get natural
rrPixelFormat rrSurfaceDXTC_GetBCNDecompFormat(rrPixelFormat from_pixelFormat);

// rrSurfaceDXTC_Make_StandardPrepForEncoding
//	convert surf to standardized input
// NOTE this is not done automatically by the encoders & should not be necessary
//  it's a convenience for test code
//	it puts the input image into the same form that the decoder will output
//	so it's easier to just RMSE the output
// fm_surf == to_surf is okay
void rrSurfaceDXTC_Make_StandardPrepForEncoding(rrSurface * to_surf, const rrSurface * fm_surf, rrPixelFormat for_bcn, bool kill_alpha);
	
// if from is RGBA8 or BGRA8 , points at it with no blit
//	 (if RGBX_Ok, then also accepts RGBX8 and BGRX8 with no blit)
// if it's something else, blits to format_to_change_to
// returns true for RGBA order, false for BGRA order
bool BlockSurface_SetView_to_RGBA8_or_BGRA8(BlockSurface * to, const BlockSurface * from, bool RGBX_Ok,
														rrPixelFormat format_to_change_to);

//--------------------------------------------------------------

// rrSurfaceDXTC_CompressBCN :
//	[to] must be allocated before calling
//  [to->pixelFormat] selects which BCN

// rrSurfaceDXTC_CompressBCN multi-threads with Jobify
// the explicit rrSurfaceDXTC_CompressBC1 funs are single threaded

struct BlockSurface;

// [to] should already by allocated
//	and to->format should be a BCN
rrbool rrSurfaceDXTC_CompressBCN_Blocks(BlockSurface * to, const BlockSurface * from, rrDXTCLevel dxtcQualityLevel, int num_workers, rrDXTCOptions options, void * jobify_user_ptr );
rrbool rrSurfaceDXTC_CompressBCN_Blocks_SingleThreaded(BlockSurface * to, const BlockSurface * from, rrDXTCLevel dxtcQualityLevel, rrDXTCOptions options );

// calls rrSurfaceDXTC_CompressBCN_Blocks :
rrbool rrSurfaceDXTC_CompressBCN_LinearSurface(rrSurface * to, const rrSurface * from, rrDXTCLevel dxtcQualityLevel, int num_workers, rrDXTCOptions options, void * jobify_user_ptr);

//--------------------------------------------------------------

// rrSurfaceDXTC_MakeOneBitTransparentCanonical forces alpha to BC1 one bit transparency
//	(eg. transparent = black)
// this should be done before MSE so that RGBA error makes sense
bool rrSurfaceDXTC_MakeOneBitTransparentCanonical(rrSurface * surf);

rrbool rrSurfaceBC1_Make_DebugVis(rrSurface * to,const rrSurface * from);

bool rrSurfaceDXTC_OneBitTransparent_Same(const rrSurface * psurf1,const rrSurface * psurf2);

//--------------------------------------------------------------

RR_NAMESPACE_END

#endif // __RADRRBITMAP_SURFACEDXTC_H__
