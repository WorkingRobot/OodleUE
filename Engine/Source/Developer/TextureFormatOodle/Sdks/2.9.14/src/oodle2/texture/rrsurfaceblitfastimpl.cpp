// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrsurfaceblitfastimpl.h"
#include "rrsurfaceblit.h"
#include "rrcolor.h"
#include "rrpixelformat.h"
//#include "rrsimpleprof.h"

RR_NAMESPACE_START

//=======================================================================

namespace // local linkage
{

static rrbool rrSurface_Blit_4U8_SwapRB( rrSurface * to, const rrSurface * fm )
{
	SINTa w = RR_MIN( fm->width  , to->width );
	SINTa h = RR_MIN( fm->height , to->height );

	for LOOP(y,h)
	{
		const U32 * fmPtr = (const U32 *)(fm->data + fm->stride * y);
		U32 * toPtr = (U32 *)(to->data + to->stride * y);

		for LOOP(x,w)
		{
			U32 t = RR_GET32_LE_UNALIGNED(&fmPtr[x]);
			t = ((t & 0xFF) << 16) | ((t >> 16) & 0xFF) | (t & 0xFF00FF00);
			RR_PUT32_LE_UNALIGNED(&toPtr[x],t);
		}
	}

	return true;
}

static rrbool rrSurface_Blit_4U8_A255( rrSurface * to, const rrSurface * fm )
{
	SINTa w = RR_MIN( fm->width  , to->width );
	SINTa h = RR_MIN( fm->height , to->height );

	for LOOP(y,h)
	{
		const U32 * fmPtr = (const U32 *)(fm->data + fm->stride * y);
		U32 * toPtr = (U32 *)(to->data + to->stride * y);

		for LOOP(x,w)
		{
			U32 t = RR_GET32_LE_UNALIGNED(&fmPtr[x]);
			t |= 0xFF000000;
			RR_PUT32_LE_UNALIGNED(&toPtr[x],t);
		}
	}

	return true;
}

static rrbool rrSurface_Blit_4U8_A255_SwapRB( rrSurface * to, const rrSurface * fm )
{
	SINTa w = RR_MIN( fm->width  , to->width );
	SINTa h = RR_MIN( fm->height , to->height );

	for LOOP(y,h)
	{
		const U32 * fmPtr = (const U32 *)(fm->data + fm->stride * y);
		U32 * toPtr = (U32 *)(to->data + to->stride * y);

		for LOOP(x,w)
		{
			U32 t = RR_GET32_LE_UNALIGNED(&fmPtr[x]);
			t = ((t & 0xFF) << 16) | ((t >> 16) & 0xFF) | (t & 0xFF00) | 0xFF000000;
			RR_PUT32_LE_UNALIGNED(&toPtr[x],t);
		}
	}

	return true;
}

static rrbool rrSurface_Blit_4F16_to_3F16( rrSurface * to, const rrSurface * fm )
{
	RR_ASSERT( fm->pixelFormat == rrPixelFormat_4_F16 );
	RR_ASSERT( to->pixelFormat == rrPixelFormat_3_F16 );

	SINTa w = RR_MIN( fm->width  , to->width );
	SINTa h = RR_MIN( fm->height , to->height );

	for LOOP(y,h)
	{
		const U8 * fmPtr = fm->data + fm->stride * y;
		U8 * toPtr = to->data + to->stride * y;

		for LOOP(x,w)
		{
			U64 t = RR_GET64_LE_UNALIGNED(fmPtr);
			RR_PUT32_LE_UNALIGNED(toPtr,(U32)t);
			RR_PUT16_LE_UNALIGNED(toPtr+4,(U16)(t >> 32));
			toPtr += 6;
			fmPtr += 8;
		}
	}

	return true;
}

} // anon namespace

void rrSurfaceBlitFastImpl_Install()
{
	// Blits between R8G8B8A8 <-> B8G8R8A8 (in both directions) switch R and B
	rrSurface_RegisterBlitter(rrPixelFormat_B8G8R8A8, rrPixelFormat_R8G8B8A8, rrSurface_Blit_4U8_SwapRB);
	rrSurface_RegisterBlitter(rrPixelFormat_R8G8B8A8, rrPixelFormat_B8G8R8A8, rrSurface_Blit_4U8_SwapRB);
	rrSurface_RegisterBlitter(rrPixelFormat_B8G8R8A8, rrPixelFormat_R8G8B8A8, rrSurface_Blit_4U8_SwapRB);
	rrSurface_RegisterBlitter(rrPixelFormat_R8G8B8A8, rrPixelFormat_B8G8R8A8, rrSurface_Blit_4U8_SwapRB);

	// Blits between R8G8B8A8 <-> R8G8B8x8 (in both directions) just set the alpha channel byte to 255
	rrSurface_RegisterBlitter(rrPixelFormat_B8G8R8A8, rrPixelFormat_B8G8R8x8, rrSurface_Blit_4U8_A255);
	rrSurface_RegisterBlitter(rrPixelFormat_B8G8R8x8, rrPixelFormat_B8G8R8A8, rrSurface_Blit_4U8_A255);
	rrSurface_RegisterBlitter(rrPixelFormat_R8G8B8A8, rrPixelFormat_R8G8B8x8, rrSurface_Blit_4U8_A255);
	rrSurface_RegisterBlitter(rrPixelFormat_R8G8B8x8, rrPixelFormat_R8G8B8A8, rrSurface_Blit_4U8_A255);

	// Blits between R8G8B8A8 <-> B8G8R8x8 (in both directions) switch R and B and set the alpha channel byte to 255
	rrSurface_RegisterBlitter(rrPixelFormat_B8G8R8A8, rrPixelFormat_R8G8B8x8, rrSurface_Blit_4U8_A255_SwapRB);
	rrSurface_RegisterBlitter(rrPixelFormat_R8G8B8x8, rrPixelFormat_B8G8R8A8, rrSurface_Blit_4U8_A255_SwapRB);
	rrSurface_RegisterBlitter(rrPixelFormat_B8G8R8x8, rrPixelFormat_R8G8B8A8, rrSurface_Blit_4U8_A255_SwapRB);
	rrSurface_RegisterBlitter(rrPixelFormat_R8G8B8A8, rrPixelFormat_B8G8R8x8, rrSurface_Blit_4U8_A255_SwapRB);

	rrSurface_RegisterBlitter(rrPixelFormat_4_F16, rrPixelFormat_3_F16, rrSurface_Blit_4F16_to_3F16);
}


RR_NAMESPACE_END
