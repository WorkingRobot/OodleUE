// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrsurfacedxtc.h"
#include "rrsurfacerowcache.h"
#include "rrdxtccompress.h"
#include "rrdxtcblock.h"
#include "bc4compress.h"
#include "bc6compress.h"
#include "bc7compress.h"
#include "bc67format.h"
#include "bc7decode_fast.h"
#include "templates/rrvector.h"
#include "rrsurfacejobslicer.h"
#include "rrsurfaceutil.h"
#include "rrsurfaceblit.h"
#include "rrlog.h"
#include "threadprofiler.h"

#include "rrsimpleprof.h"
//#include "rrsimpleprofstub.h"

RR_NAMESPACE_START

//=================================================================================================

// rrSurfaceDXTC_Make_StandardPrepForEncoding
//	convert surf to standardized input
// NOTE this is not done automatically by the encoders & should not be necessary
//  it's a convenience for test code
//	it puts the input image into the same form that the decoder will output
//	so it's easier to just RMSE the output
void rrSurfaceDXTC_Make_StandardPrepForEncoding(rrSurface * to_surf, const rrSurface * fm_surf, rrPixelFormat for_bcn, bool kill_alpha)
{
	// fm_surf == to_surf is okay
	rrSurfaceObj temp;
	rrSurface_AllocCopy(&temp,fm_surf);
	rrSurfaceDXTC_DecompressIfBCNInPlace(&temp);
	rrSurface_Depalettize(&temp);
	
	rrPixelFormat decomp_format = rrSurfaceDXTC_GetBCNDecompFormat(for_bcn);
	rrSurface_ChangeFormatNormalized(&temp,decomp_format);
	
	if ( kill_alpha)
	{
		rrSurface_ForceAllAlphasOpaqueIf4Channel(&temp);
	}
	else if ( for_bcn == rrPixelFormat_BC1 )
	{
		rrSurfaceDXTC_MakeOneBitTransparentCanonical(&temp);
	}
	
	rrSurface_Swap(&temp,to_surf);
}

bool rrSurfaceDXTC_MakeOneBitTransparentCanonical(rrSurface * surf)
{
	const rrPixelFormatInfo * info = rrPixelFormat_GetInfo(surf->pixelFormat);
	if ( info->bytesPerPixel == 0 )
	{
		rrprintf("ERROR: rrSurfaceDXTC_MakeOneBitTransparentCanonical not signal data\n");
		return false;
	}
	if ( info->channels < 4 )
	{
		// no alpha
		return false;
	}
	
	rrSurfaceRowCache rows;
	rows.Start(surf,rrPixelFormat_4_U8,8,RR_SURFACE_ROW_CACHE_READWRITE,0);
	
	int w = surf->width;
	int h = surf->height;
	
	for(int y=0;y<h;y++)
	{
		rrColor32BGRA * row = (rrColor32BGRA *) rows.GetRow(y);
	
		for(int x=0;x<w;x++)
		{
			rrColor32BGRA_MakeOneBitTransparentCanonical(row+x);
		}
	}	
	
	return true;
}

bool rrSurfaceDXTC_OneBitTransparent_Same(const rrSurface * psurf1,const rrSurface * psurf2)
{
	rrSurfaceObj surf1;
	rrSurface_SetView(&surf1,psurf1);
	rrSurfaceObj surf2;
	rrSurface_SetView(&surf2,psurf2);
	rrSurfaceDXTC_DecompressIfBCNInPlace(&surf1,rrPixelFormat_4_U8);
	rrSurfaceDXTC_DecompressIfBCNInPlace(&surf2,rrPixelFormat_4_U8);
		
	rrSurfaceRowCache rows1;
	rows1.StartReadC(&surf1,rrPixelFormat_4_U8,8,0);
	rrSurfaceRowCache rows2;
	rows2.StartReadC(&surf2,rrPixelFormat_4_U8,8,0);
	
	int w = RR_MIN(surf1.width ,surf2.width);
	int h = RR_MIN(surf1.height,surf2.height);

	for(int y=0;y<h;y++)
	{
		const rrColor32BGRA * row1 = (const rrColor32BGRA *) rows1.GetRow(y);
		const rrColor32BGRA * row2 = (const rrColor32BGRA *) rows2.GetRow(y);
	
		for(int x=0;x<w;x++)
		{
			bool t1 = rrColor32BGRA_IsOneBitTransparent(row1[x]);
			bool t2 = rrColor32BGRA_IsOneBitTransparent(row2[x]);
			if ( t1 != t2 )
				return false;
		}
	}	
	
	return true;
}

//=================================================================================================

// if from is RGBA8 or BGRA8 , points at it with no blit
//	 (if RGBX_Ok, then also accepts RGBX8 and BGRX8 with no blit)
// if it's something else, blits to format_to_change_to
// returns true for RGBA order, false for BGRA order
bool BlockSurface_SetView_to_RGBA8_or_BGRA8(BlockSurface * to, const BlockSurface * from, bool RGBX_Ok,
														rrPixelFormat format_to_change_to)
{
	BlockSurface_SetView(to,from);

	if ( from->pixelFormat == rrPixelFormat_R8G8B8A8 ) return true;
	if ( from->pixelFormat == rrPixelFormat_B8G8R8A8 ) return false;

	if ( RGBX_Ok )
	{
		if ( from->pixelFormat == rrPixelFormat_R8G8B8x8 ) return true;
		if ( from->pixelFormat == rrPixelFormat_B8G8R8x8 ) return false;
	}

	// not a format I can set view to
	
	BlockSurface_AllocCopyOrSetViewIfFormatMatches_Normalized(to,from,format_to_change_to);

	RR_ASSERT( format_to_change_to == rrPixelFormat_R8G8B8A8 || format_to_change_to == rrPixelFormat_B8G8R8A8 );
	
	return ( format_to_change_to == rrPixelFormat_R8G8B8A8 );
}

// int quality = 1-3 
// BC1 = DXT1 ; 4 bpp format
rrbool rrSurfaceDXTC_CompressBC1(BlockSurface * to, const BlockSurface * from, rrDXTCLevel qualityLevel, rrDXTCOptions options )
{
	RR_ASSERT( from->count == to->count );
	RR_ASSERT( to->pixelFormat == rrPixelFormat_BC1 );
	SIMPLEPROFILE_SCOPE_N(compress_bc1, from->count);
	
	// was_rgbx is not the same as ignore alpha
	//	it means you should read A=255 ; you might want to then preserve that value or not	
	bool was_rgbx = ( from->pixelFormat == rrPixelFormat_R8G8B8x8 || from->pixelFormat == rrPixelFormat_B8G8R8x8 );
	bool read_alpha = ( options & rrDXTCOptions_BC1_OneBitAlpha ) && ( ! was_rgbx );

	BlockSurfaceObj from_converted;
	bool rgbx_ok = true; // always allow RGBX, we'll KillAlpha on the block read if was_rgbx
	bool is_rgba = BlockSurface_SetView_to_RGBA8_or_BGRA8(&from_converted,from,rgbx_ok,rrPixelFormat_B8G8R8A8);
			
	// for each block :
	for LOOP(blocki,from->count)
	{
		const U8 * inPtr = BlockSurface_SeekC(&from_converted,blocki);
		U8 * outPtr = BlockSurface_Seek(to,blocki);
		
		// Make sure the color block is fully aligned, the encoder will be reading from it a lot.
		RAD_ALIGN(rrColorBlock4x4, colors, 64) = *((const rrColorBlock4x4 *)inPtr);
		if ( is_rgba ) SwapRB(&colors); // BC1 coder wants old BGRA
		
		if ( read_alpha )
		{
			// Canonicalize, then you can just use RGBA deltas
			//	no need for the special alpha-aware error metric (still using that at the moment though)
			for LOOP(i,16)
				rrColor32BGRA_MakeOneBitTransparentCanonical(&colors.colors[i]);
		}
		else
		{
			// user said no alpha : kill it
			KillAlpha(colors);				
		}
	
		rrDXT1Block block;
		rrCompressDXT1Block(&block,colors,qualityLevel,options,false);
		
		rrDXT1_PutBlock(outPtr,block);
	}

	return true;
}

// BC2 = DXT3 = 4 bit alpha per pixel ; 8 bpp format
rrbool rrSurfaceDXTC_CompressBC2(BlockSurface * to, const BlockSurface * from, rrDXTCLevel dxt1QualityLevel, rrDXTCOptions options )
{
	RR_ASSERT( from->count == to->count );
	RR_ASSERT( to->pixelFormat == rrPixelFormat_BC2 );
	SIMPLEPROFILE_SCOPE_N(compress_bc2, from->count);

	BlockSurfaceObj from_converted;
	bool is_rgba = BlockSurface_SetView_to_RGBA8_or_BGRA8(&from_converted,from,false,rrPixelFormat_B8G8R8A8);
			
	// for each block :
	for LOOP(blocki,from->count)
	{
		const U8 * inPtr = BlockSurface_SeekC(&from_converted,blocki);
		U8 * outPtr = BlockSurface_Seek(to,blocki);
		
		// Make sure the color block is fully aligned, the encoder will be reading from it a lot.
		RAD_ALIGN(rrColorBlock4x4, colors, 64) = *((const rrColorBlock4x4 *)inPtr);
		if ( is_rgba ) SwapRB(&colors); // BC1 coder wants old BGRA

		// Alpha block is first
		BC2_CompressAlpha(outPtr, inPtr);
		outPtr += 8; // alpha block size is 8 bytes

		// Color block is next
		KillAlpha(colors); // this is the input to the BC1 encoder

		rrDXT1Block color_blk;
		rrCompressDXT1Block(&color_blk,colors,(rrDXTCLevel)dxt1QualityLevel,options,true);
		rrDXT1_PutBlock(outPtr,color_blk);
	}

	return true;
}

// BC3 = DXT5 = 8 bit alpha interpolated + BC1 colors ; 8 bpp format
rrbool rrSurfaceDXTC_CompressBC3(BlockSurface * to, const BlockSurface * from, rrDXTCLevel dxt1QualityLevel, rrDXTCOptions options )
{
	RR_ASSERT( from->count == to->count );
	RR_ASSERT( to->pixelFormat == rrPixelFormat_BC3 );
	SIMPLEPROFILE_SCOPE_N(compress_bc3, from->count);
	
	BlockSurfaceObj from_converted;
	bool is_rgba = BlockSurface_SetView_to_RGBA8_or_BGRA8(&from_converted,from,false,rrPixelFormat_B8G8R8A8);
			
	// for each block :
	for LOOP(blocki,from->count)
	{
		const U8 * inPtr = BlockSurface_SeekC(&from_converted,blocki);
		U8 * outPtr = BlockSurface_Seek(to,blocki);
		
		// Make sure the color block is fully aligned, the encoder will be reading from it a lot.
		RAD_ALIGN(rrColorBlock4x4, colors, 64) = *((const rrColorBlock4x4 *)inPtr);
		if ( is_rgba ) SwapRB(&colors); // BC1 coder wants old BGRA

		BC4SourceData alpha_src;
		BC4_ReadSourceFromBlock(&alpha_src, inPtr, BC4SourceFormat_RGBA_U8); // actually BGRA but only the A matters here
		BC4_Compress(outPtr, alpha_src, dxt1QualityLevel, options);
		outPtr += 8; // alpha block size is 8 bytes

		KillAlpha(colors); // this is the input to the BC1 encoder

		rrDXT1Block color_blk;
		rrCompressDXT1Block(&color_blk,colors,dxt1QualityLevel,options,true);
		rrDXT1_PutBlock(outPtr,color_blk);
	}

	return true;
}

// BC4 = DXT5-esque alpha block on R channel ; 4 bpp format
rrbool rrSurfaceDXTC_CompressBC4(BlockSurface * to, const BlockSurface * from, rrDXTCLevel dxtcQualityLevel, rrDXTCOptions options )
{
	rrbool is_signed = rrPixelFormat_GetInfo(from->pixelFormat)->isSigned;
	rrbool src_16bit = rrPixelFormat_IsInt16(from->pixelFormat);

	struct FormatPair
	{
		rrPixelFormat rr_fmt;
		BC4SourceFormat bc4_fmt;
	};

	static const FormatPair formats[2][2] = // [16bit][signed]
	{
		{
			{ rrPixelFormat_1_U8, BC4SourceFormat_U8 },
			{ rrPixelFormat_1_S8, BC4SourceFormat_S8 },
		},
		{
			{ rrPixelFormat_1_U16, BC4SourceFormat_U16 },
			{ rrPixelFormat_1_S16, BC4SourceFormat_S16 },
		},
	};

	rrPixelFormat rr_fmt = formats[src_16bit][is_signed].rr_fmt;
	BC4SourceFormat bc4_src = formats[src_16bit][is_signed].bc4_fmt;

	rrPixelFormat to_fmt = is_signed ? rrPixelFormat_BC4S : rrPixelFormat_BC4U;

	RR_ASSERT_ALWAYS( from->count == to->count );
	RR_ASSERT_ALWAYS( to->pixelFormat == to_fmt );
	SIMPLEPROFILE_SCOPE_N(compress_bc4, from->count);

	BlockSurfaceObj from_rr_fmt;
	BlockSurface_AllocCopyOrSetViewIfFormatMatches_Normalized(&from_rr_fmt,from,rr_fmt);
			
	// for each block :
	for LOOP(blocki,from->count)
	{
		const U8 * inPtr = BlockSurface_SeekC(&from_rr_fmt,blocki);
		U8 * outPtr = BlockSurface_Seek(to,blocki);
		
		BC4SourceData values;
		BC4_ReadSourceFromBlock(&values, inPtr, bc4_src);
		BC4_Compress(outPtr,values,dxtcQualityLevel,options);
	}

	return true;
}

// BC5 = DXT5-esque alpha block on R+G channels ; 8 bpp format
rrbool rrSurfaceDXTC_CompressBC5(BlockSurface * to, const BlockSurface * from, rrDXTCLevel dxtcQualityLevel, rrDXTCOptions options )
{
	rrbool is_signed = rrPixelFormat_GetInfo(from->pixelFormat)->isSigned;
	rrbool src_16bit = rrPixelFormat_IsInt16(from->pixelFormat);

	struct FormatPair
	{
		rrPixelFormat rr_fmt;
		BC4SourceFormat bc4_fmt;
	};

	static const FormatPair formats[2][2] = // [16bit][signed]
	{
		{
			{ rrPixelFormat_2_U8, BC4SourceFormat_U8 },
			{ rrPixelFormat_2_S8, BC4SourceFormat_S8 },
		},
		{
			{ rrPixelFormat_2_U16, BC4SourceFormat_U16 },
			{ rrPixelFormat_2_S16, BC4SourceFormat_S16 },
		},
	};

	rrPixelFormat rr_fmt = formats[src_16bit][is_signed].rr_fmt;
	BC4SourceFormat bc4_src = formats[src_16bit][is_signed].bc4_fmt;

	rrPixelFormat to_fmt = is_signed ? rrPixelFormat_BC5S : rrPixelFormat_BC5U;

	RR_ASSERT_ALWAYS( from->count == to->count );
	SIMPLEPROFILE_SCOPE_N(compress_bc5, from->count);

	// if you try to encode signed ints to BC5U, you hit this assert :
	//	-> should catch this and fail earlier
	//	OodleTex_Entry_ValidateFormats does this for the public API
	//	it's just the private API that's missing an early check
	//	-> for the private API this usually wants to be *change* BC4/5 U to S as appropriate
	//		rather than fail
	//	-> the "textest" private API callers mostly do this themselves
	RR_ASSERT_ALWAYS( to->pixelFormat == to_fmt );

	BlockSurfaceObj from_rr_fmt;
	BlockSurface_AllocCopyOrSetViewIfFormatMatches_Normalized(&from_rr_fmt,from,rr_fmt);
			
	// for each block :
	for LOOP(blocki,from->count)
	{
		const U8 * inPtr = BlockSurface_SeekC(&from_rr_fmt,blocki);
		U8 * outPtr = BlockSurface_Seek(to,blocki);

		BC4SourceData values_r, values_g;
		BC5_ReadSourceFromBlock(&values_r,&values_g,inPtr,bc4_src);

		BC4_Compress(outPtr + 0,values_r,dxtcQualityLevel,options);
		BC4_Compress(outPtr + 8,values_g,dxtcQualityLevel,options);
		outPtr += 16;
	}

	return true;
}

rrbool rrSurfaceDXTC_CompressBC6H(BlockSurface * to, const BlockSurface * from, bool isSigned, rrDXTCLevel level, rrDXTCOptions options)
{
	rrPixelFormat to_fmt = isSigned ? rrPixelFormat_BC6S : rrPixelFormat_BC6U;
	RR_ASSERT_ALWAYS( to->pixelFormat == to_fmt );
	SIMPLEPROFILE_SCOPE_N(compress_bc6, from->count);

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
	SIMPLEPROFILE_SCOPE_N(compress_bc7, from->count);

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


//=================================================================================================

rrbool rrSurfaceDXTC_DecompressBC1(BlockSurface * to, const BlockSurface * from )
{
	SIMPLEPROFILE_SCOPE_N(decompress_bc1, from->count);
	
	RR_ASSERT_ALWAYS( from->pixelFormat == rrPixelFormat_BC1 );

	BlockSurface_Alloc(to,from->count,rrPixelFormat_R8G8B8A8);
		
	// for each block :
	for LOOP(blocki,from->count)
	{
		const U8 * inPtr = BlockSurface_SeekC(from,blocki);
		U8 * outPtr = BlockSurface_Seek(to,blocki);
		
		rrDXT1Block block;
		rrDXT1_GetBlock(inPtr,&block);
			
		rrColorBlock4x4 * pcolors = (rrColorBlock4x4 *)outPtr;
		DXT1_Decompress(pcolors,block,rrDXT1PaletteMode_Alpha);
		SwapRB(pcolors); //DXT1_Decompress wrote BGRA, change to RGBA 
	}

	return true;
}

rrbool rrSurfaceDXTC_DecompressBC2(BlockSurface * to, const BlockSurface * from )
{
	SIMPLEPROFILE_SCOPE_N(decompress_bc2, from->count);

	RR_ASSERT_ALWAYS( from->pixelFormat == rrPixelFormat_BC2 );
		
	BlockSurface_Alloc(to,from->count,rrPixelFormat_R8G8B8A8);
		
	// for each block :
	for LOOP(blocki,from->count)
	{
		const U8 * inPtr = BlockSurface_SeekC(from,blocki);
		U8 * outPtr = BlockSurface_Seek(to,blocki);
		rrColorBlock4x4 * pcolors = (rrColorBlock4x4 *)outPtr;

		rrDXT1Block cblock;
		rrDXT1_GetBlock(inPtr + 8,&cblock); // color block starts 8B after alpha block

		// Decode color (DXT1) portion first
		DXT1_Decompress(pcolors,cblock,rrDXT1PaletteMode_FourColor);
		SwapRB(pcolors); //DXT1_Decompress wrote BGRA, change to RGBA

		// Alpha decode on top
		BC2_DecompressAlpha(outPtr,inPtr);
	}

	return true;
}

rrbool rrSurfaceDXTC_DecompressBC3(BlockSurface * to, const BlockSurface * from )
{
	SIMPLEPROFILE_SCOPE_N(decompress_bc3, from->count);

	RR_ASSERT_ALWAYS( from->pixelFormat == rrPixelFormat_BC3 );
		
	BlockSurface_Alloc(to,from->count,rrPixelFormat_R8G8B8A8);
		
	// for each block :
	for LOOP(blocki,from->count)
	{
		const U8 * inPtr = BlockSurface_SeekC(from,blocki);
		U8 * outPtr = BlockSurface_Seek(to,blocki);
		rrColorBlock4x4 * pcolors = (rrColorBlock4x4 *)outPtr;
		
		rrDXT1Block cblock;
		rrDXT1_GetBlock(inPtr + 8,&cblock); // color block starts 8B after alpha block

		// Decode color (DXT1) portion first
		DXT1_Decompress(pcolors,cblock,rrDXT1PaletteMode_FourColor);
		SwapRB(pcolors); //DXT1_Decompress wrote BGRA, change to RGBA

		// Alpha decode on top
		BC4A_Decompress(outPtr,inPtr);
	}

	return true;
}

rrbool rrSurfaceDXTC_DecompressBC4(BlockSurface * to, const BlockSurface * from, rrPixelFormat format_hint )
{
	SIMPLEPROFILE_SCOPE_N(decompress_bc4, from->count);

	RR_ASSERT_ALWAYS( from->pixelFormat == rrPixelFormat_BC4U || from->pixelFormat == rrPixelFormat_BC4S );

	bool is_signed = from->pixelFormat == rrPixelFormat_BC4S;
	bool dest_is_16bit = true; // if dest format not given, default to int16
	if ( format_hint != rrPixelFormat_Invalid && rrPixelFormat_IsInt8(format_hint) )
		dest_is_16bit = false;		

	rrPixelFormat to_fmt;
	
	if ( is_signed )
		to_fmt = dest_is_16bit ? rrPixelFormat_1_S16 : rrPixelFormat_1_S8;	
	else
		to_fmt = dest_is_16bit ? rrPixelFormat_1_U16 : rrPixelFormat_1_U8;
		
	BlockSurface_Alloc(to,from->count,to_fmt);
		
	if ( !is_signed )
	{
		// for each block :
		for LOOP(blocki,from->count)
		{
			const U8 * inPtr = BlockSurface_SeekC(from,blocki);
			U8 * outPtr = BlockSurface_Seek(to,blocki);
		
			U16 pixels[16];

			BC4U_Decompress16(pixels, inPtr);

			if ( dest_is_16bit )
			{
				memcpy(outPtr,pixels,16*sizeof(U16));
			}
			else
			{
				// convert UNORM16 to UNORM8 with rounding (using same conversion rules as D3D/GL)
				// note 65535 = 255*257

				for LOOP(i,16)
					outPtr[i] = static_cast<U8>((pixels[i] + 128)/257);
			}
		}
	}
	else
	{
		// for each block :
		for LOOP(blocki,from->count)
		{
			const U8 * inPtr = BlockSurface_SeekC(from,blocki);
			U8 * outPtr = BlockSurface_Seek(to,blocki);
		
			S16 pixels[16];

			BC4S_Decompress16(pixels, inPtr);

			if ( dest_is_16bit )
			{
				memcpy(outPtr,pixels,16*sizeof(U16));
			}
			else
			{
				// convert SNORM16 to SNORM8 with rounding (using same conversion rules as D3D/GL)

				for LOOP(i,16)
					outPtr[i] = static_cast<S8>((pixels[i] * 127 + 16384)/32767);
			}
		}
	}

	return true;
}

rrbool rrSurfaceDXTC_DecompressBC5(BlockSurface * to, const BlockSurface * from, rrPixelFormat format_hint )
{
	SIMPLEPROFILE_SCOPE_N(decompress_bc5, from->count);

	RR_ASSERT_ALWAYS( from->pixelFormat == rrPixelFormat_BC5U || from->pixelFormat == rrPixelFormat_BC5S );

	bool is_signed = from->pixelFormat == rrPixelFormat_BC5S;
	bool dest_is_16bit = true; // if dest format not given, default to int16
	if ( format_hint != rrPixelFormat_Invalid && rrPixelFormat_IsInt8(format_hint) )
		dest_is_16bit = false;
		
	rrPixelFormat to_fmt;
	
	if ( is_signed )
		to_fmt = dest_is_16bit ? rrPixelFormat_2_S16 : rrPixelFormat_2_S8;	
	else
		to_fmt = dest_is_16bit ? rrPixelFormat_2_U16 : rrPixelFormat_2_U8;
		
	BlockSurface_Alloc(to,from->count,to_fmt);
		
	if ( !is_signed )
	{
		// for each block :
		for LOOP(blocki,from->count)
		{
			const U8 * inPtr = BlockSurface_SeekC(from,blocki);
			U8 * outPtr = BlockSurface_Seek(to,blocki);
		
			U16 pixels_r[16];
			U16 pixels_g[16];

			BC4U_Decompress16(pixels_r, inPtr + 0);
			BC4U_Decompress16(pixels_g, inPtr + 8);

			if ( dest_is_16bit )
			{
				U16 * ptr = (U16*)outPtr;
				for LOOP(i,16)
				{
					*ptr++ = pixels_r[i];
					*ptr++ = pixels_g[i];
				}
			}
			else
			{
				// convert UNORM16 to UNORM8 with rounding (using same conversion rules as D3D/GL)
				// note 65535 = 255*257
				U8 * ptr = outPtr;
				for LOOP(i,16)
				{
					*ptr++ = static_cast<U8>((pixels_r[i] + 128)/257);
					*ptr++ = static_cast<U8>((pixels_g[i] + 128)/257);
				}
			}
		}
	}
	else
	{
		// for each block :
		for LOOP(blocki,from->count)
		{
			const U8 * inPtr = BlockSurface_SeekC(from,blocki);
			U8 * outPtr = BlockSurface_Seek(to,blocki);
		
			S16 pixels_r[16];
			S16 pixels_g[16];

			BC4S_Decompress16(pixels_r, inPtr + 0);
			BC4S_Decompress16(pixels_g, inPtr + 8);
			inPtr += 16;

			if ( dest_is_16bit )
			{
				S16 * ptr = (S16*)outPtr;
				for LOOP(i,16)
				{
					*ptr++ = pixels_r[i];
					*ptr++ = pixels_g[i];
				}
			}
			else
			{
				// convert SNORM16 to SNORM8 with rounding (using same conversion rules as D3D/GL)
				S8 * ptr = (S8 *)outPtr;
				for LOOP(i,16)
				{
					*ptr++ = static_cast<S8>((pixels_r[i] * 127 + 16384)/32767);
					*ptr++ = static_cast<S8>((pixels_g[i] * 127 + 16384)/32767);
				}
			}
		}
	}

	return true;
}

rrbool rrSurfaceDXTC_DecompressBC6H(BlockSurface * to, const BlockSurface * from )
{
	SIMPLEPROFILE_SCOPE_N(decompress_bc6, from->count);

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
	SIMPLEPROFILE_SCOPE_N(decompress_bc7, from->count);

	RR_ASSERT_ALWAYS( from->pixelFormat == rrPixelFormat_BC7 );

	BlockSurface_Alloc(to,from->count,rrPixelFormat_R8G8B8A8);

	// for each block :
	for LOOP(blocki,from->count)
	{
		const U8 * inPtr = BlockSurface_SeekC(from,blocki);
		U8 * outPtr = BlockSurface_Seek(to,blocki);

		bc7_decode_block_fast(outPtr,inPtr,false);
		//RR_NOP();
	}

	return true;
}

//=================================================================================================

rrPixelFormat rrSurfaceDXTC_GetBCNDecompFormat(rrPixelFormat from_pixelFormat)
{
	switch(from_pixelFormat)
	{
	case rrPixelFormat_BC1:
	case rrPixelFormat_BC2:
	case rrPixelFormat_BC3:
	case rrPixelFormat_BC7:
		return rrPixelFormat_R8G8B8A8; // RGBA to match the public API
		
	case rrPixelFormat_BC4S:
		return rrPixelFormat_1_S16;
	case rrPixelFormat_BC4U:
		return rrPixelFormat_1_U16;

	case rrPixelFormat_BC5S:
		return rrPixelFormat_2_S16;
	case rrPixelFormat_BC5U:
		return rrPixelFormat_2_U16;

	case rrPixelFormat_BC6U:
	case rrPixelFormat_BC6S:
		// there are arguments for using 3_F32 here, or 4_F16
		return rrPixelFormat_4_F32;
		
	default:
		RR_ASSERT_FAILURE_ALWAYS("rrSurfaceDXTC_GetBCNDecompFormat not BCN");
		return rrPixelFormat_Invalid;
	}
}
		
rrbool BlockSurface_DecompressBCN(BlockSurface * to, const BlockSurface * from, rrPixelFormat format_hint )
{	
	switch(from->pixelFormat)
	{
	case rrPixelFormat_BC1:  return rrSurfaceDXTC_DecompressBC1(to,from);
	case rrPixelFormat_BC2:  return rrSurfaceDXTC_DecompressBC2(to,from);
	case rrPixelFormat_BC3:  return rrSurfaceDXTC_DecompressBC3(to,from);
	case rrPixelFormat_BC4S: return rrSurfaceDXTC_DecompressBC4(to,from,format_hint);
	case rrPixelFormat_BC4U: return rrSurfaceDXTC_DecompressBC4(to,from,format_hint);
	case rrPixelFormat_BC5S: return rrSurfaceDXTC_DecompressBC5(to,from,format_hint);
	case rrPixelFormat_BC5U: return rrSurfaceDXTC_DecompressBC5(to,from,format_hint);
	case rrPixelFormat_BC6U:
	case rrPixelFormat_BC6S: return rrSurfaceDXTC_DecompressBC6H(to,from);
	case rrPixelFormat_BC7:  return rrSurfaceDXTC_DecompressBC7(to,from);
	default: RR_ASSERT_FAILURE("bad BCN"); return false;
	}
}

rrbool rrSurfaceDXTC_DecompressBCN(rrSurface * to, const rrSurface * from, rrPixelFormat format_hint )
{
	#if 0
	if ( to->data == NULL )
	{
		// @@ this does not necessarily match the format made by BlockSurface_DecompressBCN
		//	do we want that?
		//	in particular for BC6 it does a F16 -> F32 conversion
		rrPixelFormat to_format = rrSurfaceDXTC_GetBCNDecompFormat(from->pixelFormat);
		if ( ! rrSurface_Alloc(to,from->width,from->height,to_format) )
			return false;
	}
	#endif
		
	BlockSurface from_bs;
	BlockSurface_SetView_of_RRS_BCN(&from_bs,from);
	
	// BlockSurface_DecompressBCN will alloc [decomp] :

	// format_hint is currently just for BC4/5 to know if it should alloc 8 or 16 bit
	//  @@ ?
	//	if [to] was allocated, should we set format_hint to its format?
	//	-> I don't think this "format_hint" does anything useful
	//	you can just always decode to 16 bit (which is the default)
	//	and the blit after will change it to 8 bit if necessary
	BlockSurfaceObj decomp;
	BlockSurface_DecompressBCN(&decomp,&from_bs,format_hint);
	
	if ( to->data == NULL )
	{
		//rrPixelFormat to_format = rrSurfaceDXTC_GetBCNDecompFormat(from->pixelFormat);
		rrPixelFormat to_format = decomp.pixelFormat;
		if ( ! rrSurface_Alloc(to,from->width,from->height,to_format) )
			return false;
	}
	
	// change format if decomp.format != to->format
	BlockSurfaceObj decomp_to_format;
	BlockSurface_AllocCopyOrSetViewIfFormatMatches_Normalized(&decomp_to_format,&decomp,to->pixelFormat);
	
	// swizzle blocks to linear with no format change :
	BlockSurface_Copy_to_RRS_SameFormat(to,1,&decomp_to_format);
	
	return true;
}

// handy utility to remove BCN if present :
rrbool rrSurfaceDXTC_DecompressIfBCNInPlace(rrSurface * surf, rrPixelFormat format_hint )
{
	const rrPixelFormatInfo * info = rrPixelFormat_GetInfo(surf->pixelFormat);
	if ( info->bytesPerPixel == 0 ) // BCN
	{
		// pick the output format to match the BCN :
		rrPixelFormat to_format = rrSurfaceDXTC_GetBCNDecompFormat(surf->pixelFormat);
	
		rrSurfaceObj decomp;
		if ( ! rrSurface_Alloc(&decomp,surf->width,surf->height,to_format) )
			return false;
		if ( ! rrSurfaceDXTC_DecompressBCN(&decomp,surf,format_hint) )
			return false;
		rrSurface_Swap(surf,&decomp);
	}
	return true;
}

//=================================================================================================

rrbool rrSurfaceDXTC_CompressBCN_Blocks_SingleThreaded(BlockSurface * to, const BlockSurface * from, rrDXTCLevel dxtcQualityLevel, rrDXTCOptions options )
{
	RR_ASSERT( to->blocks != NULL );
	RR_ASSERT( to->count == from->count );
	
	rrPixelFormat bc = to->pixelFormat;
	RR_ASSERT( rrPixelFormat_IsBlockCompressed(bc) );
	
	switch(bc)
	{
	case rrPixelFormat_BC1:  return rrSurfaceDXTC_CompressBC1(to,from,dxtcQualityLevel,options);
	case rrPixelFormat_BC2:  return rrSurfaceDXTC_CompressBC2(to,from,dxtcQualityLevel,options);
	case rrPixelFormat_BC3:  return rrSurfaceDXTC_CompressBC3(to,from,dxtcQualityLevel,options);
	case rrPixelFormat_BC4S: // fall-through
	case rrPixelFormat_BC4U: return rrSurfaceDXTC_CompressBC4(to,from,dxtcQualityLevel,options);
	case rrPixelFormat_BC5S: // fall-through
	case rrPixelFormat_BC5U: return rrSurfaceDXTC_CompressBC5(to,from,dxtcQualityLevel,options);
	case rrPixelFormat_BC6U: return rrSurfaceDXTC_CompressBC6H(to,from,false,dxtcQualityLevel,options);
	case rrPixelFormat_BC6S: return rrSurfaceDXTC_CompressBC6H(to,from,true,dxtcQualityLevel,options);
	case rrPixelFormat_BC7:  return rrSurfaceDXTC_CompressBC7(to,from,dxtcQualityLevel,options);
	default: RR_ASSERT_FAILURE("bad BCN"); return false;
	}
}

//===========================================

struct rrSurfaceDXTC_CompressBCN_JobSlicer_Data
{
	// in:
	rrPixelFormat bc;
	rrDXTCLevel dxtcQualityLevel;
	rrDXTCOptions options;
};

rrbool rrSurfaceDXTC_CompressBCN_JobSlicer_Func(BlockSurfaceJobSlicer_Data * slice,void * data)
{
	rrSurfaceDXTC_CompressBCN_JobSlicer_Data * bcn_data = (rrSurfaceDXTC_CompressBCN_JobSlicer_Data *)data;
	
	rrbool ret = rrSurfaceDXTC_CompressBCN_Blocks_SingleThreaded(&slice->to_sliced,&slice->from_sliced,bcn_data->dxtcQualityLevel,bcn_data->options);
	
	return ret;
};

rrbool rrSurfaceDXTC_CompressBCN_Blocks(BlockSurface * to, const BlockSurface * from, rrDXTCLevel dxtcQualityLevel, int num_workers, rrDXTCOptions options, void * jobify_user_ptr)
{
	THREADPROFILESCOPE("CompressBCN_Blocks");

	RR_ASSERT( to->blocks != NULL );
	RR_ASSERT( to->count == from->count );
	
	rrPixelFormat bc = to->pixelFormat;
	RR_ASSERT( rrPixelFormat_IsBlockCompressed(bc) );

	rrSurfaceDXTC_CompressBCN_JobSlicer_Data data;
	data.bc = bc;
	data.dxtcQualityLevel = dxtcQualityLevel;
	data.options = options;
	
	rrbool ok = BlockSurface_JobSlicer(to,from,num_workers,jobify_user_ptr,rrSurfaceDXTC_CompressBCN_JobSlicer_Func,&data);
	
	return ok;
}

rrbool rrSurfaceDXTC_CompressBCN_LinearSurface(rrSurface * to, const rrSurface * from, rrDXTCLevel dxtcQualityLevel, int num_workers, rrDXTCOptions options, void * jobify_user_ptr)
{
	RR_ASSERT( to->data != NULL );
	RR_ASSERT( rrPixelFormat_IsBlockCompressed(to->pixelFormat) );
	RR_ASSERT( ! rrPixelFormat_IsBlockCompressed(from->pixelFormat) );
	
	BlockSurfaceObj to_blocks;
	BlockSurface_SetView_of_RRS_BCN(&to_blocks,to);
	
	BlockSurfaceObj from_blocks;
	BlockSurface_AllocCopy_from_RRS(&from_blocks,from,1);
	
	rrbool ret = rrSurfaceDXTC_CompressBCN_Blocks(&to_blocks,&from_blocks,dxtcQualityLevel,num_workers,options,jobify_user_ptr);
	
	return ret;
}

//=================================================================================================

/********

turn each 4x4 into an 8x8 with a 2x2 of 4x4 blocks :

[ c0        ] [ decoded block ]
[ selectors ] [  c1           ]

reading these images is a bit tough
having 4 separate images for each of those is probably better
could do c0 and c1 side by side as 2x4 blocks

or put the separate vis images side by side in a single large vis image

*********/

rrbool rrSurfaceBC1_Make_DebugVis(rrSurface * to,const rrSurface * from)
{
	RR_ASSERT( from->pixelFormat == rrPixelFormat_BC1 );
	if ( from->pixelFormat != rrPixelFormat_BC1 ) return false;
	
	int padw = rrAlignUp(from->width,4);
	int padh = rrAlignUp(from->height,4);	
	int nbx = padw/4;
	int nby = padh/4;
	
	int vis_elem_size = 8;
	int vis_elem_pad = 2;
	int vis_block_size = vis_elem_size + vis_elem_pad; 
	
	int tow = nbx * vis_block_size;
	int toh = nby * vis_block_size;
	
	if ( ! rrSurface_Alloc(to,tow,toh,rrPixelFormat_B8G8R8A8) )
		return false;
		
	rrColor4I background_color = { 0 };
	background_color.b = 255;
	rrSurface_FillSolidColorI(to,background_color);
	
	// BC1 is : end0, end1, 1/3, 2/3
	//uint8 one_third = linear_float_to_srgb_byte(1.f/3.f); // 156
	//uint8 two_third = linear_float_to_srgb_byte(2.f/3.f); // 213
	rrColor32BGRA selector_colors[4];
	selector_colors[0].dw = 0;
	selector_colors[1].u.r =
	selector_colors[1].u.g =
	selector_colors[1].u.b = 255;
	selector_colors[2].u.r =
	selector_colors[2].u.g =
	selector_colors[2].u.b = 156;
	selector_colors[3].u.r =
	selector_colors[3].u.g =
	selector_colors[3].u.b = 213;
		
	// work on rows of blocks :
	for(int by=0;by<nby;by++)
	{
		rrDXT1Block * inPtr = (rrDXT1Block *) rrPixelFormat_Seek(from->pixelFormat,from->data,from->stride,0,by*4);
		
		// for each block :
		for(int bx=0;bx<nbx;bx++)
		{
			rrDXT1Block block;
			block = *inPtr++;
			
			rrColorBlock4x4 colors;
			DXT1_Decompress(&colors,block,rrDXT1PaletteMode_NoAlpha);

			rrColor32BGRA * pcolors = colors.colors;

			rrColor32BGRA c0b = rrColor565Bits_UnQuantize(block.c0);
			rrColor32BGRA c1b = rrColor565Bits_UnQuantize(block.c1);
			
			int startx = 1 + bx * vis_block_size;
			int starty = 1 + by * vis_block_size;
			
			U8 * to_start_ptr = rrPixelFormat_Seek(to->pixelFormat,to->data,to->stride,startx,starty);
			
			U32 selectors = block.indices;
		
			for(int y=0;y<4;y++)
			{
				rrColor32BGRA * r1 = ((rrColor32BGRA *)(to_start_ptr + to->stride * (y)));
				rrColor32BGRA * r2 = ((rrColor32BGRA *)(to_start_ptr + to->stride * (y+4)));	
				
				for(int x=0;x<4;x++)
				{
					r1[x] = c0b;
					r1[x+4] = *pcolors++;
					r2[x+4] = c1b;
					
					U32 cur = selectors&3; // yes, first at the bottom
					selectors>>=2;
					r2[x] = selector_colors[cur];
				}
			}
		}
	}
	
	// kill alpha cuz I didn't fill it right
	if ( ! rrSurface_ChangeFormatNormalized(to,rrPixelFormat_B8G8R8) )
		return false;
		
	return true;
}


//===========================================

RR_NAMESPACE_END
