// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRRBITMAP_SURFACEROWCACHE_H__
#define __RADRRBITMAP_SURFACEROWCACHE_H__

#include "rrsurface.h"

RR_NAMESPACE_START

// RowCache needs C++

/***

RowCache : 

automatically caches chunks of rows from a Surf for you

RowCache can be Read or Write or ReadWrite

Read means it auto-blits in rows from the base surface as they enter the base window
Write means it auto-blits out rows as they leave the row window
ReadWrite does both (for read-modify-write usage)

-----------------

This is nice so that you can easily write utils to work on arbitrary formats and be efficient
without making a whole copy and avoiding the GetColor/SetColor slow path.

---------------

Note : GetRow() will MoveCache() for you automatically
if you are only using one row at a time, that's a good way to go
however, GetRow() can invalidate any existing row pointers !!! BEWARE

If you need multiple rows, you must MoveCache() manually and specify numrows :

	MoveCache(y,2);
	row0 = GetRow(y+0);
	row1 = GetRow(y+1); // <- this could invalidate row0 if you don't do the manual MoveCache

------------------

RowCache also supports padding to alignment for you
specify padding in Start() and the image will act as if it is aligned to "padding" bigger
lets you simplify your loops to avoid checking edge cases
For example for DXTC specify padding = 4 and then don't worry about the block padding

@@ CHANGE VS OODLE1 RADBITMAP : padding here is only for alignment!
	not padded all over

*******/

enum ERRSurfaceRowCacheAction
{
	RR_SURFACE_ROW_CACHE_NONE	=   (0),
	RR_SURFACE_ROW_CACHE_READ	=	(1),
	RR_SURFACE_ROW_CACHE_WRITE	=	(2),
	RR_SURFACE_ROW_CACHE_READWRITE	= (RR_SURFACE_ROW_CACHE_READ|RR_SURFACE_ROW_CACHE_WRITE)
};

class rrSurfaceRowCache
{
public:
	 rrSurfaceRowCache() : m_surf(NULL), m_cacheStartY(0), m_cacheRows(0), m_readWrite(RR_SURFACE_ROW_CACHE_NONE), m_is_pass_through(false) { } 
	~rrSurfaceRowCache();

	rrSurfaceRowCache(rrSurface * surf,rrPixelFormat format,int numRows,ERRSurfaceRowCacheAction action) : 
		m_surf(NULL), m_cacheStartY(0), m_cacheRows(0), m_readWrite(action)
	{
		if ( ! Start(surf,format,numRows,action) )
		{
			RR_ASSERT_ALWAYS("rrSurfaceRowCache failed");
			m_surf = NULL;
			m_cacheStartY = 0;
			m_cacheRows = 0;
		}
	}
	
	//-------------------------------------
	// 
	
	rrbool Start(rrSurface * surf,rrPixelFormat format,int numRows,ERRSurfaceRowCacheAction action,int alignment_padding = 0);
	
	rrbool StartReadC(const rrSurface * surf,rrPixelFormat format,int numRows,int alignment_padding = 0);
	
	void * GetRow(int y);

	rrbool MoveCache(int yStart,int numRows);
	rrbool FlushWrite();

	rrbool IsRowInCache(int y) const { return y >= m_cacheStartY && y < (m_cacheStartY+m_cacheRows); }

	//-------------------------------------
	// sugar for common 4I/4F case :

	void Start_4I_8(rrSurface * surf,ERRSurfaceRowCacheAction action,int alignment_padding = 0) { Start(surf,rrPixelFormat_4_S32,8,action,alignment_padding); }
	void Start_4F_8(rrSurface * surf,ERRSurfaceRowCacheAction action,int alignment_padding = 0) { Start(surf,rrPixelFormat_4_F32,8,action,alignment_padding); }

	void Start_ReadC_4I_8(const rrSurface * surf,int alignment_padding = 0) { StartReadC(surf,rrPixelFormat_4_S32,8,alignment_padding); }
	void Start_ReadC_4F_8(const rrSurface * surf,int alignment_padding = 0) { StartReadC(surf,rrPixelFormat_4_F32,8,alignment_padding); }

	rrColor4I * GetRow_4I(int y) { RR_ASSERT( m_rows.pixelFormat == rrPixelFormat_4_S32); return (rrColor4I *) GetRow(y); }
	rrColor4F * GetRow_4F(int y) { RR_ASSERT( m_rows.pixelFormat == rrPixelFormat_4_F32); return (rrColor4F *) GetRow(y); }
	
private:
	rrSurface * m_surf;
	int			m_cacheStartY;
	int			m_cacheRows;
	int			m_paddingX;
	int			m_paddingY;
	ERRSurfaceRowCacheAction			m_readWrite;
	rrSurfaceObj	m_rows;
	bool		m_is_pass_through;
};

// row extension when you're out of bounds :
static int MirrorIndex(int i,int w)
{
	if( (U32)i < (U32)w ) return i;

	int r;
	
	// Mirror with repeated edge, eg. -1 -> 0 , so the value at 0 occurs twice
	if ( i < 0 ) r = -i-1;
	else r = 2*w-i-1; // i >= w
	
	// make sure it worked -
	//RR_ASSERT( r >= 0 && r < w );
	//	this fails if "w" is tiny and you're trying to index way out of bounds
	//	in that case I would have to repeat or clamp; instead just fail
	//	eg. padding of 8 on a 4x4 image is not allowed, padding must be <= size
	// @@@@ : ugly fix - just clamp for now until I can fix this properly
	r = RR_CLAMP(r,0,w-1);
	
	return r;
}

RR_NAMESPACE_END

#endif // __RADRRBITMAP_SURFACEROWCACHE_H__
