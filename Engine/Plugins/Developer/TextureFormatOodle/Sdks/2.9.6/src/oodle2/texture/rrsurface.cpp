// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrsurface.h"
#include "rrmath.h"
#include "rrcolor.h"
#include "oodlemalloc.h"
#include "rrmemutil.h"
#include "templates/rrstl.h"
#include "rrmemutil.h"
#include "rrmem.h"
#include <string.h> // for memset

RR_NAMESPACE_START

void rrSurface_Init( rrSurface * surf)
{
	RR_ZERO(*surf); 
}
 
rrbool rrSurface_Alloc(rrSurface * surf, S32 w,S32 h, rrPixelFormat format )
{
	if ( w == 0 || h == 0 )
	{
		// @@ asked to alloc zero bytes ?? free old ??
		rrSurface_Free(surf);
		return false;
	}
	
	//SINTa stride = rrPixelFormat_MakeStride_Aligned(format, w );
	SINTa stride = rrPixelFormat_MakeStride_Minimum(format, w );
		
	// see if we can just leave the existing bits in there :
	if ( surf->pixelFormat == format && 
		surf->width == w && 
		surf->height == h && 
		surf->stride == stride &&
		surf->data != NULL &&
		surf->freeData )
	{
		// surf is preallocated and okay
		//RR_ASSERT_FAILURE_ALWAYS("YIKES I DON'T LIKE THIS BRANCH"); // @@ DELETE ME
		return true;
	}
	else
	{
		// replacing; free old :
		rrSurface_Free(surf);
	}
	
	surf->width = w;
	surf->height = h;
	surf->pixelFormat = format;
	
	// we always align up to 4 to alloc : (@@ ?)
	
	surf->stride = stride;
	
	SINTa bytesForSurface = rrPixelFormat_GetSurfaceSizeBytes(format, stride, h);
	
	// always alloc +8 so I can use U64 splats
	surf->data = (U8 *) OodleMalloc( bytesForSurface + 8 );
	surf->freeData = true;
	
	if ( surf->data == NULL )
		return false;
		
	// alloc palette
	if ( format == rrPixelFormat_Palette8 )
	{
		surf->palette = OODLE_MALLOC_ARRAY(rrColor4I,256);
		if ( ! surf->palette )
		{
			rrSurface_Free(surf);
			return false;
		}
		surf->freePalette = true;
	}
		
	return true;
}

void rrSurface_Free( rrSurface * surf)
{
	if ( surf->freeData && surf->data )
		OodleFree(surf->data);
		
	if ( surf->freePalette && surf->palette )
		OodleFree(surf->palette);
		
	RR_ZERO(*surf);
}

void rrSurface_SetStride_Minimum(rrSurface * surf)
{
	//RR_ASSERT( surf->stride == 0 );
	surf->stride = rrPixelFormat_MakeStride_Minimum(surf->pixelFormat,surf->width);
}

SINTa rrSurface_GetDataSizeBytes(const rrSurface * surf)
{
	return rrPixelFormat_GetSurfaceSizeBytes(surf->pixelFormat, surf->stride, surf->height);	
}

U8 * rrSurface_Seek(rrSurface * surf,int x,int y)
{
	return rrPixelFormat_Seek(surf->pixelFormat,surf->data,surf->stride,x,y);
}

const U8 * rrSurface_SeekC(const rrSurface * surf,int x,int y)
{
	return rrPixelFormat_Seek(surf->pixelFormat,surf->data,surf->stride,x,y);
}

void rrSurface_MoveOwnership( rrSurface * to, rrSurface * from)
{
	// to now owns the data
	*to = *from;
	from->freeData = false;
	from->freePalette = false;
}

void rrSurface_SetView( rrSurface * to, const rrSurface * from,
								S32 x, S32 y,
								S32 w, S32 h)
{
	*to = *from;
	to->freeData = false;
	to->freePalette = false;
	
	// setting "to" to a view of null is fine
	if ( from->data == NULL )
		return;
	
	if ( w == -1 ) w = from->width;
	if ( h == -1 ) h = from->height;
	
	RR_ASSERT( (x+w) <= from->width );
	RR_ASSERT( (y+h) <= from->height );
	
	to->width = w;
	to->height = h;
	
	to->data = rrPixelFormat_Seek(from->pixelFormat, from->data, from->stride, x, y );
}

void   rrSurface_Swap( rrSurface * s1, rrSurface * s2 )
{
	rrSurface tmp(*s1);
	*s1 = *s2;
	*s2 = tmp;
}

void rrSurface_SetPointAt(rrSurface * surf,void * bits,int w,int h,SINTa stride,rrPixelFormat	format)
{
	rrSurface_Init(surf);
	surf->data = (U8 *)bits;
	surf->width = w;
	surf->height = h;
	surf->stride = stride;
	surf->pixelFormat = format;
}

void rrSurface_SetDataBytesZero( rrSurface * surf )
{
	SINTa bytes = rrSurface_GetDataSizeBytes(surf);
	memset(surf->data,0, bytes );
}

// slightly slow version that respects views :
void rrSurface_SetPixelsZero( rrSurface * surf )
{
	const rrPixelFormatInfo * info = rrPixelFormat_GetInfo(surf->pixelFormat);
	
	int blocksPerRow = (surf->width + info->blockSize - 1 ) / info->blockSize;
	int bytesPerBlockRow = blocksPerRow * info->bytesPerBlock;
	
	RR_ASSERT( bytesPerBlockRow <= surf->stride );
	
	for(int y=0; y < surf->height; y += info->blockSize )
	{
		U8 * ptr = rrSurface_Seek(surf,0,y);
		memset(ptr,0, bytesPerBlockRow );
	}
}

void rrSurface_SetNonPixelBytesZero( rrSurface * surf )
{
	const rrPixelFormatInfo * info = rrPixelFormat_GetInfo(surf->pixelFormat);
	
	int blocksPerRow = (surf->width + info->blockSize - 1 ) / info->blockSize;
	int bytesPerBlockRow = blocksPerRow * info->bytesPerBlock;
	
	RR_ASSERT( bytesPerBlockRow <= surf->stride );
	
	for(int y=0; y < surf->height; y += info->blockSize )
	{
		U8 * ptr = rrSurface_Seek(surf,0,y);
		memset(ptr + bytesPerBlockRow ,0, surf->stride - bytesPerBlockRow );
	}
}


rrbool rrSurface_AllocSame(rrSurface * to, const rrSurface * from)
{
	if ( from->data == NULL )
	{
		// make to match from - nothing !
		rrSurface_Free(to);
		return true;
	}
	
	if ( ! rrSurface_Alloc(to,from->width,from->height,from->pixelFormat) )
	{
		// error 
		rrSurface_Free(to);
		return false;
	}
	
	return true;
}

RR_NAMESPACE_END