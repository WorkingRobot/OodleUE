// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRRBITMAP_SURFACE_H__
#define __RADRRBITMAP_SURFACE_H__

#include "rrbase.h"
#include "rrpixelformat.h"
#include "cbradutil.h"

/***************

rrSurface 

very basic pixel bucket struct

Standard way to use is to Blit to a surface type that you know how to work with

Recommended optimized way is to Blit out 8-row chunks
  rrSurfaceRowCache provides a helper to do that for you, but you can do it manually easily enough

You can of course work directly on ->data if you know the type.

For example if your pixels are rrPixelFormat_1_F32 , then you can do :

	F32 val = ((F32 *)(surf.data + surf.stride * y))[ x ];

	note how y uses stride to step on data as a U8 pointer - stride is in bytes
	x then steps on the F32 pointer so it is stepping pixels

Surface can of course hold arbitrary data
	Palettized data is not strongly supported; you basically have to Depalettize to do anything
	> 4 channels is not strongly supported; use multiple rrSurfaces instead

rrSurface is designed to be very thin and interchangeable with a dumb array.  For example you can do :

	F32 vals[8][8];
	rrSurface surf;
	rrSurface_PointAt(&surf,vals,8,8,8*sizeof(F32),rrPixelFormat_1_F32);

***************/

RR_NAMESPACE_START

struct rrColor4I;
struct rrColor4F;

struct rrSurface
{
	U8 *	data;	// data is a U8 just to make it more convenient for pointer math
	rrbool	freeData;
	
	rrColor4I *	palette; // always 256 colors or none
	rrbool	freePalette;
	
	// w,h,stride are 32 bit , but # of pixels can be 64 bit (SINTa)
	S32		width,height;
	SINTa	stride; // stride is a number of bytes to get to the next row of *blocks*
	rrPixelFormat	pixelFormat;
};
// rrSurface struct initializer = { 0 } is fine
//	or call Init()

// if you make an rrSurface without = { 0 } or without an rrSurfaceObj YOU MUST INIT !!
void rrSurface_Init( rrSurface * surf);

// NOTEZ : rrSurface_Alloc assumes you have done an Init() or something already on the rrSurface struct - 
//	_Alloc is a NOP if you alloc the same size/format again - it tries to reuse existing memory
// NOTEZ : Alloc does *NOT* set the pixels to any value (use rrSurface_SetDataBytesZero for example)
rrbool rrSurface_Alloc(rrSurface * surf, S32 w,S32 h, rrPixelFormat format );
void   rrSurface_Free( rrSurface * surf);

// MoveOwernship like auto_ptr = assignment
//	from still points at the bits though, just no longer owns them
void rrSurface_MoveOwnership( rrSurface * to, rrSurface * from);

// SetView : to is a non-owning view into from or some portion of from
//	(depending on pixel format, w,h & x,y may have alignment constraints)
// NOTEZ : rrSurface is not ref-counting , you must ensure "from" stays alive longer than "to"
void rrSurface_SetView( rrSurface * to, const rrSurface * from,
								S32 x=0, S32 y=0,
								S32 w=-1, S32 h=-1);

// after you set surf->width && surf->pixelFormat, call this to set ->stride								
void rrSurface_SetStride_Minimum(rrSurface * surf);

// SetDataBytesZero : WARNING : this does not respect width/stride , so it should not be used on a View !
void rrSurface_SetDataBytesZero( rrSurface * surf );

// slightly slower version that respects views :
void rrSurface_SetPixelsZero( rrSurface * surf );

// set the stuff in Stride that's not in Width to zero :
void rrSurface_SetNonPixelBytesZero( rrSurface * surf );

// Swap : 
void   rrSurface_Swap( rrSurface * s1, rrSurface * s2 );

// sugar interfaces to wrap :
SINTa rrSurface_GetDataSizeBytes(const rrSurface * surf);

// for Seek , x&y must be block-aligned :
U8 * rrSurface_Seek(rrSurface * surf,int x,int y);
const U8 * rrSurface_SeekC(const rrSurface * surf,int x,int y);

// make surf point at some existing data plane :

void rrSurface_SetPointAt(rrSurface * surf,void * bits,int w,int h,SINTa stride,rrPixelFormat	format);

// utility to alloc the same as another surface : (does NOT copy bytes - use AllocCopy for that)
rrbool rrSurface_AllocSame(rrSurface * to, const rrSurface * from);

//=======================================================================

// for the the I32 and F32 pixel types, you can access channels like ((F32*)ptr)[channel]
// with these indices :

// order in rrColor :
typedef enum e_rrSurface_EChannel
{
	rrSurface_Channel_R = 0,
	rrSurface_Channel_G = 1,
	rrSurface_Channel_B = 2,
	rrSurface_Channel_A = 3,
	rrSurface_Channel_None = 4
} rrSurface_EChannel;

//=======================================================================
#ifdef __cplusplus

class rrSurfaceObj : public rrSurface
{
public :
	rrSurfaceObj()  { rrSurface_Init(this); }
	~rrSurfaceObj() { rrSurface_Free(this); }

private:
	RR_FORBID_CLASS_STANDARDS(rrSurfaceObj);
};
#endif // __cplusplus

//=======================================================================
RR_NAMESPACE_END

#endif // __RADRRBITMAP_SURFACE_H__
