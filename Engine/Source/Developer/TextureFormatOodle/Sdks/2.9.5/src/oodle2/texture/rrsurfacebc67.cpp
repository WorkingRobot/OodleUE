// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrsurfacebc67.h"
#include "rrsurfacerowcache.h"
#include "rrsurfacedxtc.h"
#include "rrcolor.h"
#include "rrdxtcblock.h"
#include "bc67format.h"
#include "bc6compress.h"
#include "bc7compress.h"
#include "bc7decode_fast.h"
#include "blocksurface.h"

RR_NAMESPACE_START

rrbool rrSurfaceDXTC_DecompressBC6H(BlockSurface * to, const BlockSurface * from )
{
	RR_ASSERT_ALWAYS( from->pixelFormat == rrPixelFormat_BC6U || from->pixelFormat == rrPixelFormat_BC6S );
	bool is_signed = from->pixelFormat == rrPixelFormat_BC6S;

	BlockSurface_Alloc(to,from->count,rrPixelFormat_4_F16);
		
	// for each block :
	for LOOP(blocki,from->count)
	{
		const U8 * inPtr = BlockSurface_SeekC(from,blocki);
		U8 * outPtr = BlockSurface_Seek(to,blocki);
		
		U16 * colors = (U16 *) outPtr;
		bc6h_decode_block(colors,4 * 4 * sizeof(U16),is_signed,inPtr);
	}

	return true;
}

rrbool rrSurfaceDXTC_DecompressBC7(BlockSurface * to, const BlockSurface * from )
{
	RR_ASSERT_ALWAYS( from->pixelFormat == rrPixelFormat_BC7 );
	
	BlockSurface_Alloc(to,from->count,rrPixelFormat_R8G8B8A8);
		
	// for each block :
	for LOOP(blocki,from->count)
	{
		const U8 * inPtr = BlockSurface_SeekC(from,blocki);
		U8 * outPtr = BlockSurface_Seek(to,blocki);

		bc7_decode_block_fast(outPtr,inPtr);
		//RR_NOP();
	}

	return true;
}

rrbool rrSurfaceDXTC_CompressBC6H(BlockSurface * to, const BlockSurface * from, bool isSigned, rrDXTCLevel level, rrDXTCOptions options)
{
	rrPixelFormat to_fmt = isSigned ? rrPixelFormat_BC6S : rrPixelFormat_BC6U;
	RR_ASSERT_ALWAYS( to->pixelFormat == to_fmt );

	BlockSurfaceObj from_F16;
	BlockSurface_AllocCopyOrSetViewIfFormatMatches_Normalized(&from_F16,from,rrPixelFormat_4_F16);

	BC6EncOptions opts;
	bc6enc_options_init(&opts,level,options,isSigned);

	// for each block :
	for LOOP(blocki,from->count)
	{
		const U8 * inPtr = BlockSurface_SeekC(&from_F16,blocki);
		U8 * outPtr = BlockSurface_Seek(to,blocki);
		
		const U16 * pixels = (const U16 *)inPtr;
		
		bc6enc_compress_block(outPtr, pixels, opts);
	}

	return true;
}

rrbool rrSurfaceDXTC_CompressBC7(BlockSurface * to, const BlockSurface * from, rrDXTCLevel level, rrDXTCOptions options)
{
	RR_ASSERT( from->count == to->count );
	RR_ASSERT( to->pixelFormat == rrPixelFormat_BC7 );
	
	// was_rgbx is not the same as ignore alpha
	//	it means you should read A=255 ; you might want to then preserve that value or not
	bool was_rgbx = ( from->pixelFormat == rrPixelFormat_R8G8B8x8 || from->pixelFormat == rrPixelFormat_B8G8R8x8 );
	bool read_alpha = (!( options & rrDXTCOptions_BC7_IgnoreAlpha )) && ( ! was_rgbx );

	// rrColor32RGBA not rrColor32BGRA
	BlockSurfaceObj from_converted;
	bool rgbx_ok = true; // always allow RGBX, we'll KillAlpha on the block read if was_rgbx
	bool is_rgba = BlockSurface_SetView_to_RGBA8_or_BGRA8(&from_converted,from,rgbx_ok,rrPixelFormat_R8G8B8A8);
			
	// for each block :
	
	for LOOP(blocki,from->count)
	{
		const U8 * inPtr = BlockSurface_SeekC(&from_converted,blocki);
		U8 * outPtr = BlockSurface_Seek(to,blocki);
		
		// could skip this if is_rgba && read_alpha, but whatevs
		rrColorBlock4x4 colors = *((const rrColorBlock4x4 *)inPtr);
		if ( ! is_rgba ) SwapRB(&colors);
		if ( ! read_alpha ) KillAlpha(colors);
		// BC7Prep_init does KillAlpha again if rrDXTCOptions_BC7_IgnoreAlpha is set

		BC7_CompressBlock(outPtr, (const U8 *)&colors, level, options);
	}

	return true;
}

RR_NAMESPACE_END
