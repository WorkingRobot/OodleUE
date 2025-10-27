// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrsurfaceblit.h"
#include "rrsurfaceblitfastimpl.h"
#include "rrsurfacedxtc.h"
#include "rrsurfaceutil.h"
#include "rrcolor.h"
#include "rrbase.h"
#include "threadprofiler.h"
#include <string.h> // memcpy
#include <rrlog.h>

//#include "rrsimpleprof.h"

RR_NAMESPACE_START

// internal fallback :
static rrbool rrSurface_Blit_Slow( rrSurface * to, const rrSurface * from);
static void rrSurface_Blit_SameFormat( rrSurface * to, const rrSurface * from );

//=======================================================================
/**

fast blitters -
this should just be an array of size rrPixelFormat_Count * rrPixelFormat_Count
except that rrPixelFormat is not linear, so bleh

could put a linear index in rrPixelFormatInfo

**/

static rrSurface_Blitter * s_fastBlitters[ RR_PIXELFORMAT_COUNT ][ RR_PIXELFORMAT_COUNT ] = {  };

// Register with cpuCapsBits 
// cpuCapsBits is a bit field specifying needed caps (MMX/SSE/etc).
void rrSurface_RegisterBlitter( rrPixelFormat fromFmt, rrPixelFormat toFmt, rrSurface_Blitter * blitter )
{
	int fromI = rrPixelFormat_GetIndex(fromFmt);
	int toI = rrPixelFormat_GetIndex(toFmt);
	
	RR_ASSERT( fromI >= 0 && fromI < RR_PIXELFORMAT_COUNT );
	RR_ASSERT( toI >= 0 && toI < RR_PIXELFORMAT_COUNT );
	
	RR_ASSERT( s_fastBlitters[fromI][toI] == 0 || s_fastBlitters[fromI][toI] == blitter );
	
	s_fastBlitters[fromI][toI] = blitter;
}

static rrSurface_Blitter * const rrSurface_FindBlitter( rrPixelFormat fromFmt, rrPixelFormat toFmt )
{	
	int fromI = rrPixelFormat_GetIndex(fromFmt);
	int toI   = rrPixelFormat_GetIndex(toFmt);

	RR_ASSERT( fromI >= 0 && fromI < RR_PIXELFORMAT_COUNT );
	RR_ASSERT( toI >= 0 && toI < RR_PIXELFORMAT_COUNT );
		
	rrSurface_Blitter * ret = s_fastBlitters[fromI][toI];
	if ( ret )
		return ret;
	
	{
		// thread safety beware :
		// if this is called by multiple threads
		// rrSurfaceBlitFastImpl_Install can be entered multiple times
		//	that is OK
		// rrSurface_RegisterBlitter will just store the same value again
		static bool s_once = true;
		if ( s_once )
		{
			rrSurfaceBlitFastImpl_Install();
			s_once = false;
			
			// fetch again after Install :
			ret = s_fastBlitters[fromI][toI];
		}
	}
	
	return ret;
}

//=======================================================================

rrbool rrSurface_Blit_NonNormalized( rrSurface * to, const rrSurface * from)
{
	if ( from->pixelFormat == to->pixelFormat )
	{
		rrSurface_Blit_SameFormat(to,from);
		return true;
	}
	else
	{	
		// first see if from->format,to->format is one that we accelerate
		//	look in function pointer table
		
		rrSurface_Blitter * const blitter = rrSurface_FindBlitter(from->pixelFormat,to->pixelFormat);
		rrbool ret;

		// THREADPROFILESCOPE conditional
		ThreadProfiler_Handle threadprof_handle = TP_INVALID_HANDLE;
		if ( to->height > 8 ) // not for RowCache
			threadprof_handle = ThreadProfiler_Push("Blit",0);

		if ( blitter )
		{
			//SIMPLEPROFILE_SCOPE(FastBlitter);

			ret = (*blitter)(to,from);
		}
		else
		{	
			// if not fall back to slow pixel by pixel copy	
			//rrprintf("blit slow %s->%s\n", rrPixelFormat_GetName(from->pixelFormat), rrPixelFormat_GetName(to->pixelFormat));
		
			ret = rrSurface_Blit_Slow(to,from);
		}

		ThreadProfiler_Pop(threadprof_handle);
		return ret;
	}
}

rrbool rrSurface_BlitRect( rrSurface * to, const rrSurface * from,
							S32 from_x, S32 from_y,
							S32 to_x  , S32 to_y  , 
							S32 width,  S32 height )
{
	// Blit subregion :
	//	just make views of the subregions and call normal blit :

	rrSurface fromView;
	rrSurface toView;
	
	rrSurface_SetView(&fromView,from,from_x,from_y,width,height);
	rrSurface_SetView(&toView,to,to_x,to_y,width,height);
	
	return rrSurface_Blit_NonNormalized(&toView,&fromView);
}


// there might not be a fast blit from X->Y
//	but there might be a fast blit from X->Z->Y
// that seems crazy but it is in fact the case often for Z = generic (Color4I or Color4F)


// internal fallback :
rrbool rrSurface_Blit_Slow( rrSurface * to, const rrSurface * fm )
{
	rrSurfaceObj depal;
	if ( fm->pixelFormat == rrPixelFormat_Palette8 )
	{
		// same format path has already been checked
		//	so we must depalettize
		RR_ASSERT( to->pixelFormat != rrPixelFormat_Palette8 );
		rrSurface_DepalettizeTo(fm,&depal,rrPixelFormat_B8G8R8A8);
		fm = &depal;
	}
	
	const rrPixelFormatInfo * fmInfo = rrPixelFormat_GetInfo(fm->pixelFormat);
	const rrPixelFormatInfo * toInfo = rrPixelFormat_GetInfo(to->pixelFormat);
	
	int fmBpp = fmInfo->bytesPerPixel;
	int toBpp = toInfo->bytesPerPixel;
		
	if ( fmBpp != 0 && toBpp != 0 )
	{
		const int CHUNK_SIZE = 128;
		int w = RR_MIN( fm->width  , to->width );
		int h = RR_MIN( fm->height , to->height );
		
		for(int y=0;y<h;y++)
		{
			const U8 * fmPtr = rrPixelFormat_Seek(fm->pixelFormat, fm->data, fm->stride, 0, y );
			U8 * toPtr = rrPixelFormat_Seek(to->pixelFormat, to->data, to->stride, 0, y );
		
			if ( fmInfo->isFloat || toInfo->isFloat )
			{
				// if either is float, use float
				rrColor4F colors[CHUNK_SIZE];
				for(int x=0;x<w;x+=CHUNK_SIZE)
				{
					int count = RR_MIN(w - x, CHUNK_SIZE);
					if (to->pixelFormat == rrPixelFormat_4_F32) // only get?
					{
						rrPixelFormat_GetColors4F(fmPtr, fm->pixelFormat, (rrColor4F *)toPtr, count);
					}
					else if (fm->pixelFormat == rrPixelFormat_4_F32) // only put?
					{
						rrPixelFormat_PutColors4F(toPtr, to->pixelFormat, (const rrColor4F *)fmPtr, count);
					}
					else // general case
					{
						rrPixelFormat_GetColors4F(fmPtr, fm->pixelFormat, colors, count);
						rrPixelFormat_PutColors4F(toPtr, to->pixelFormat, colors, count);
					}
					
					fmPtr += fmBpp * count;
					toPtr += toBpp * count;
				}
			}
			else
			{
				rrColor4I colors[CHUNK_SIZE];
				for(int x=0;x<w;x+=CHUNK_SIZE)
				{
					int count = RR_MIN(w - x, CHUNK_SIZE);
					rrPixelFormat_GetColors4I(fmPtr, fm->pixelFormat, colors, count);
					rrPixelFormat_PutColors4I(toPtr, to->pixelFormat, colors, count);
					
					fmPtr += fmBpp * count;
					toPtr += toBpp * count;
				}
			
			}
		}
		
		return true;
	}
	else if ( rrPixelFormat_IsBlockCompressed(fm->pixelFormat) )
	{
		return rrSurfaceDXTC_DecompressBCN(to,fm);	
	}
	else
	{
		// blitting *to* BCN or to palettized will fail
		//	blitting *from* BCN or palettized is okay
	
		rrprintf(" Blit_Slow FAIL : not byte pixels! (%s -> %s)\n",
			rrPixelFormat_GetName(fm->pixelFormat),
			rrPixelFormat_GetName(to->pixelFormat));
		RR_ASSERT(false);
		return false;
	}
}

void rrSurface_Blit_SameFormat( rrSurface * to, const rrSurface * fm )
{
	RR_ASSERT( fm->pixelFormat == to->pixelFormat );
	const rrPixelFormatInfo * info = rrPixelFormat_GetInfo(fm->pixelFormat);
	
	int w = RR_MIN( fm->width  , to->width );
	int h = RR_MIN( fm->height , to->height );

	// handles blocked formats :

	int blocksPerRow = (w + info->blockSize - 1 ) / info->blockSize;
	int bytesPerBlockRow = blocksPerRow * info->bytesPerBlock;
	
	if ( info->blockSize == 4 )
	{
		for(int y=0;y<h;y += 4)
		{
			const U8 * fmPtr = fm->data + (y/4) * fm->stride;
			U8 * toPtr = to->data + (y/4) * to->stride;
		
			memcpy(toPtr,fmPtr, bytesPerBlockRow);
		}
	}
	else if ( info->blockSize == 1 )
	{
		for(int y=0;y<h;y += 1)
		{
			const U8 * fmPtr = fm->data + y * fm->stride;
			U8 * toPtr = to->data + y * to->stride;
		
			memcpy(toPtr,fmPtr, bytesPerBlockRow);
		}
	}
	else
	{
		RR_ASSERT_FAILURE("block size not 1 or 4");
		
		for(int y=0;y<h;y += info->blockSize)
		{
			const U8 * fmPtr = rrPixelFormat_Seek(fm->pixelFormat, fm->data, fm->stride, 0, y );
			U8 * toPtr = rrPixelFormat_Seek(to->pixelFormat, to->data, to->stride, 0, y );
		
			memcpy(toPtr,fmPtr, bytesPerBlockRow);
		}
	}
}

void rrSurface_GetColor4I( const rrSurface * fm, rrColor4I * color, int x, int y)
{
	const U8 * fmPtr = rrPixelFormat_Seek(fm->pixelFormat, fm->data, fm->stride, x, y );
	if ( fmPtr == NULL )
	{
		RR_ASSERT_FAILURE_ALWAYS(" Blit_Slow : not byte pixels!");
		return;
	}

	rrPixelFormat_GetColors4I(fmPtr, fm->pixelFormat, color, 1);
}

void rrSurface_GetColor4F( const rrSurface * fm, rrColor4F * color, int x, int y)
{
	const U8 * fmPtr = rrPixelFormat_Seek(fm->pixelFormat, fm->data, fm->stride, x, y );
	if ( fmPtr == NULL )
	{
		RR_ASSERT_FAILURE_ALWAYS(" Blit_Slow : not byte pixels!");
		return;
	}

	rrPixelFormat_GetColors4F(fmPtr, fm->pixelFormat, color, 1);
}

void rrSurface_PutColor4I( rrSurface * surf, const rrColor4I * color, int x, int y)
{
	U8 * ptr = rrPixelFormat_Seek(surf->pixelFormat, surf->data, surf->stride, x, y );
	if ( ptr == NULL )
	{
		RR_ASSERT_FAILURE_ALWAYS(" Blit_Slow : not byte pixels!");
		return;
	}

	rrPixelFormat_PutColors4I(ptr, surf->pixelFormat, color, 1);
}

void rrSurface_PutColor4F( rrSurface * surf, const rrColor4F * color, int x, int y)
{
	U8 * ptr = rrPixelFormat_Seek(surf->pixelFormat, surf->data, surf->stride, x, y );
	if ( ptr == NULL )
	{
		RR_ASSERT_FAILURE_ALWAYS(" Blit_Slow : not byte pixels!");
		return;
	}

	rrPixelFormat_PutColors4F(ptr, surf->pixelFormat, color, 1);
}

rrbool rrSurface_AllocCopy(rrSurface * to, const rrSurface * from)
{
	// see also rrSurface_AllocSame
	
	RR_ASSERT( from != to );
	// replacing; free old :
	rrSurface_Free(to);

	if ( from->data == NULL )
	{
		return true;
	}
	
	if ( ! rrSurface_Alloc(to,from->width,from->height,from->pixelFormat) )
	{
		// error 
		rrSurface_Free(to);
		return false;
	}
	
	return rrSurface_Blit_NonNormalized(to,from);
}

rrbool rrSurface_AllocCopy_ChangeFormatNormalized(rrSurface * to, const rrSurface * from, rrPixelFormat format)
{
	RR_ASSERT( from != to );
	// replacing; free old :
	rrSurface_Free(to);

	if ( from->data == NULL )
	{
		return true;
	}
	
	if ( format == rrPixelFormat_Invalid )
		format = from->pixelFormat;
		
	if ( ! rrSurface_Alloc(to,from->width,from->height,format) )
	{
		// error 
		rrSurface_Free(to);
		return false;
	}
	
	return rrSurface_BlitNormalized(to,from);
}

rrbool rrSurface_AllocCopy_ChangeFormatNonNormalized(rrSurface * to, const rrSurface * from, rrPixelFormat format)
{
	RR_ASSERT( from != to );
	// replacing; free old :
	rrSurface_Free(to);

	if ( from->data == NULL )
	{
		return true;
	}
	
	if ( format == rrPixelFormat_Invalid )
		format = from->pixelFormat;
		
	if ( ! rrSurface_Alloc(to,from->width,from->height,format) )
	{
		// error 
		rrSurface_Free(to);
		return false;
	}
	
	return rrSurface_Blit_NonNormalized(to,from);
}

rrbool rrSurface_AllocCopyOrSetViewIfFormatMatches_Normalized(rrSurface * to, const rrSurface * from, rrPixelFormat format)
{
	if ( from->pixelFormat == format )
	{
		rrSurface_SetView(to,from);
		return true;
	}
	else
	{
		return rrSurface_AllocCopy_ChangeFormatNormalized(to,from,format);
	}
}


rrbool rrSurface_AllocCopyOrSetViewIfFormatMatches_NonNormalized(rrSurface * to, const rrSurface * from, rrPixelFormat format)
{
	if ( from->pixelFormat == format )
	{
		rrSurface_SetView(to,from);
		return true;
	}
	else
	{
		return rrSurface_AllocCopy_ChangeFormatNonNormalized(to,from,format);
	}
}


//-------------------------------------------------------------------------

rrbool rrSurface_Depalettize( rrSurface * surf , rrPixelFormat destFormatOverride /* = rrPixelFormat_Invalid*/)
{
	if ( surf->pixelFormat != rrPixelFormat_Palette8 )
		return true;
		
	rrSurfaceObj temp;
	rrSurface_MoveOwnership(&temp,surf);
	
	return rrSurface_DepalettizeTo(&temp,surf,destFormatOverride);
}

rrbool rrSurface_DepalettizeTo( const rrSurface * fm , rrSurface * to , 
			rrPixelFormat destFormatOverride /* = rrPixelFormat_Invalid*/)
{
	if ( fm->pixelFormat != rrPixelFormat_Palette8 )
		return false;
	
//	SIMPLEPROFILE_SCOPE(Depalettize);
		
	// detect the data type from what's in the palette :
	
	// note BMP doesn't really have an L8 , it gets stored as P8 with a grey palette
	//	so this lets us detect the L8
	// (we could get thrown off by palette colors that aren't actually used)
	
	rrbool palHasAlpha = 0;
	rrbool palHasColor = 0;
	
	const rrColor4I * palette = fm->palette;
	
	rrColor32BGRA fastpal[256];
	
	// @@ really need a palette color count
	//	instead of assuming all palettized images have 256 colors
	for(int i=0;i<256;i++)
	{
		// U8_check - NO !
		// palette colors that aren't used by the image are full of crud
		fastpal[i].u.b = (U8)( palette[i].b );
		fastpal[i].u.g = (U8)( palette[i].g );
		fastpal[i].u.r = (U8)( palette[i].r );
		fastpal[i].u.a = (U8)( palette[i].a );
	
		palHasAlpha |= ( palette[i].a != 255 );
		
		palHasColor |= ( palette[i].r != palette[i].g );
		palHasColor |= ( palette[i].r != palette[i].b );
	}
	
	rrPixelFormat destFormat;
	
	if ( destFormatOverride != rrPixelFormat_Invalid )
	{
		destFormat = destFormatOverride;
	}
	else
	{
		destFormat = rrPixelFormat_R8;
		if ( palHasColor )
			destFormat = rrPixelFormat_B8G8R8;
		if ( palHasAlpha )
			destFormat = rrPixelFormat_B8G8R8A8;
	}	
	
	int w = fm->width;
	int h = fm->height;
	
	if ( ! rrSurface_Alloc(to,w,h,destFormat) )
		return false;
		
	//const rrPixelFormatInfo * toInfo = rrPixelFormat_GetInfo(destFormat);
	//int toBpp = toInfo->bytesPerPixel;
	
	switch(destFormat)
	{
		case rrPixelFormat_R8:		
		
		for(int y=0;y<h;y++)
		{
			const U8 * fmPtr = fm->data + fm->stride * y;
			U8 * toPtr = to->data + to->stride * y;
			
			for(int x=0;x<w;x++)
			{
				U8 pal = fmPtr[x];
				
				*toPtr++ = U8_check( palette[pal].r );
			}
		}
		break;
		
		case rrPixelFormat_B8G8R8:	
		
		for(int y=0;y<h;y++)
		{
			const U8 * fmPtr = fm->data + fm->stride * y;
			U8 * toPtr = to->data + to->stride * y;			
			
			for(int x=0;x<w;x++)
			{
				U8 pal = fmPtr[x];
				
				RR_PUT32_NATIVE(toPtr,fastpal[pal].dw);
				toPtr += 3;
			}
		
		}
		break;
			
		case rrPixelFormat_B8G8R8A8:
		
		for(int y=0;y<h;y++)
		{
			const U8 * fmPtr = fm->data + fm->stride * y;
			U8 * toPtr = to->data + to->stride * y;
						
			for(int x=0;x<w;x++)
			{
				U8 pal = fmPtr[x];
				
				RR_PUT32_NATIVE(toPtr,fastpal[pal].dw);
				toPtr += 4;
			}		
		}
		break;
		
	RR_NO_DEFAULT_CASE
	}
	
	return true;
}

RR_NAMESPACE_END
