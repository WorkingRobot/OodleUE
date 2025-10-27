// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRRBITMAP_SURFACEBLIT_H__
#define __RADRRBITMAP_SURFACEBLIT_H__

#include "rrsurface.h"

RR_NAMESPACE_START

//=======================================================================
// rrSurface_Blit
// primary surface bit copier
// to surface should already be allocated to the desired dest format

// blitting *to* BCN or to palettized will fail
//	blitting *from* BCN or palettized is okay

// if image sizes don't match, min overlap area is copied
		
// Blit will eventually be very fast for all common cases
//	the recommended way to edit is to Blit to temp, work on it, then Blit back
//	see RowCache for example

rrbool rrSurface_Blit_NonNormalized( rrSurface * to, const rrSurface * from );

rrbool rrSurface_BlitRect( rrSurface * to, const rrSurface * from, 
							S32 from_x, S32 from_y,
							S32 to_x  , S32 to_y  , 
							S32 width,  S32 height );

//=======================================================================
// Convenience GetColor/PutColor :
//
//	THIS IS VERY SLOW AND WILL NOT BE OPTIMIZED
// just for utility code where you really don't care :

void rrSurface_GetColor4I( const rrSurface * surf, rrColor4I * color, int x, int y);
void rrSurface_GetColor4F( const rrSurface * surf, rrColor4F * color, int x, int y);

void rrSurface_PutColor4I( rrSurface * surf, const rrColor4I * color, int x, int y);
void rrSurface_PutColor4F( rrSurface * surf, const rrColor4F * color, int x, int y);

//=======================================================================

// Convenience to alloc & new surface and Blit from "from"
rrbool rrSurface_AllocCopy(rrSurface * to, const rrSurface * from);
rrbool rrSurface_AllocCopy_ChangeFormatNonNormalized(rrSurface * to, const rrSurface * from, rrPixelFormat format);
rrbool rrSurface_AllocCopy_ChangeFormatNormalized(rrSurface * to, const rrSurface * from, rrPixelFormat format);

rrbool rrSurface_AllocCopyOrSetViewIfFormatMatches_Normalized(rrSurface * to, const rrSurface * from, rrPixelFormat format);
rrbool rrSurface_AllocCopyOrSetViewIfFormatMatches_NonNormalized(rrSurface * to, const rrSurface * from, rrPixelFormat format);

// Depalettize changes surface in place to the most natural non-palettized format
//	Palette cannot be used in Blit so Depalettize first
rrbool rrSurface_DepalettizeTo( const rrSurface * fm , rrSurface * to , rrPixelFormat destFormatOverride = rrPixelFormat_Invalid );
rrbool rrSurface_Depalettize( rrSurface * surf , rrPixelFormat destFormatOverride = rrPixelFormat_Invalid );

//=======================================================================

// rrSurface_RegisterBlitter to register optimized blitters for certain formats
// rrSurface_RegisterBlitter can be done during CINIT with RR_AT_STARTUP() if you want

typedef rrbool (rrSurface_Blitter) ( rrSurface * to, const rrSurface * from );

// cpuCapsBits is a bit field specifying needed caps (MMX/SSE/etc).
//	priority is used to know which blitters of a same format are preferred if multiple are possible
//	(eg. should I use the SSE or 3Dnow blitter?)
void rrSurface_RegisterBlitter( rrPixelFormat fromFmt, rrPixelFormat toFmt, rrSurface_Blitter * fp );

//=======================================================================

RR_NAMESPACE_END

#endif // __RADRRBITMAP_SURFACEBLIT_H__
