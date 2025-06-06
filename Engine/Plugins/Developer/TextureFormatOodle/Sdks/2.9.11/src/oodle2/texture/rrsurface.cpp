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
#include "debughash.h"
#include <string.h> // for memset

RR_NAMESPACE_START

void rrSurface_Init( rrSurface * surf)
{
	RR_ZERO(*surf); 
}
 
rrbool rrSurface_Alloc(rrSurface * surf, SINTa w,SINTa h, rrPixelFormat format )
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
	// image surfaces are expected to be relatively large and cache line alignment seems prudent
	surf->data = (U8 *) OodleMallocAligned(bytesForSurface + 8, RR_CACHE_LINE_SIZE);
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

void rrSurface_MoveOwnership( rrSurface * to, rrSurface * from)
{
	// to now owns the data
	*to = *from;
	from->freeData = false;
	from->freePalette = false;
}

void rrSurface_SetView( rrSurface * to, const rrSurface * from,
								SINTa x, SINTa y,
								SINTa w, SINTa h)
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

void rrSurface_SetPointAt(rrSurface * surf,void * bits,SINTa w,SINTa h,SINTa stride,rrPixelFormat format)
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
	
	SINTa blocksPerRow = (surf->width + info->blockSize - 1 ) / info->blockSize;
	SINTa bytesPerBlockRow = blocksPerRow * info->bytesPerBlock;
	
	RR_ASSERT( bytesPerBlockRow <= surf->stride );
	
	for(SINTa y=0; y < surf->height; y += info->blockSize )
	{
		U8 * ptr = rrSurface_Seek(surf,0,y);
		memset(ptr,0, bytesPerBlockRow );
	}
}

void rrSurface_SetNonPixelBytesZero( rrSurface * surf )
{
	const rrPixelFormatInfo * info = rrPixelFormat_GetInfo(surf->pixelFormat);
	
	SINTa blocksPerRow = (surf->width + info->blockSize - 1 ) / info->blockSize;
	SINTa bytesPerBlockRow = blocksPerRow * info->bytesPerBlock;
	
	RR_ASSERT( bytesPerBlockRow <= surf->stride );
	
	for(SINTa y=0; y < surf->height; y += info->blockSize )
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

U64 rrSurface_DebugHash(const rrSurface * surf )
{
	const rrPixelFormatInfo * info = rrPixelFormat_GetInfo(surf->pixelFormat);

	SINTa blocksPerRow = (surf->width + info->blockSize - 1 ) / info->blockSize;
	SINTa bytesPerBlockRow = blocksPerRow * info->bytesPerBlock;
	U64 running_hash = 0;

	RR_ASSERT( bytesPerBlockRow <= surf->stride );

	// Hash only the payload bytes, not the padding between rows
	for(SINTa y=0; y < surf->height; y += info->blockSize )
	{
		const U8 * ptr = rrSurface_SeekC(surf,0,y);
		U64 row_hash = simple_debug_hash(ptr,bytesPerBlockRow);
		U8 hash_bytes[16];
		RR_PUT64_LE_UNALIGNED(hash_bytes + 0, running_hash);
		RR_PUT64_LE_UNALIGNED(hash_bytes + 8, row_hash);
		running_hash = simple_debug_hash(hash_bytes,sizeof(hash_bytes));
	}

	return running_hash;
}

void rrSurface_AssertSizeAndFormat(const rrSurface * surf, SINTa w, SINTa h, rrPixelFormat format)
{
	RR_ASSERT_ALWAYS( surf->width == w );
	RR_ASSERT_ALWAYS( surf->height == h );
	RR_ASSERT_ALWAYS( surf->pixelFormat == format );
}

RR_NAMESPACE_END
