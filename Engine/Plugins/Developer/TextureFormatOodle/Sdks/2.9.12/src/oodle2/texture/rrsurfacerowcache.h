// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRRBITMAP_SURFACEROWCACHE_H__
#define __RADRRBITMAP_SURFACEROWCACHE_H__

#include "rrsurface.h"
#include "rrsurfaceutil.h"

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
	rrSurfaceRowCache() { } 
	~rrSurfaceRowCache();

	rrSurfaceRowCache(rrSurface * surf,rrPixelFormat format,int numRows,ERRSurfaceRowCacheAction action) : 
		m_cacheRows(0), m_readWrite(action)
	{
		if ( ! Start(surf,format,numRows,action) )
		{
			RR_ASSERT_ALWAYS(!"rrSurfaceRowCache Start failed");
			m_surf = nullptr;
			m_cacheStartY = 0;
			m_cacheRows = 0;
		}
	}
	
	//-------------------------------------
	// 
	
	rrbool Start(rrSurface * surf,rrPixelFormat format,SINTa numRows,ERRSurfaceRowCacheAction action,SINTa alignment_padding = 0)
	{
		return StartImpl(surf,format,numRows,action,alignment_padding,false,rrRangeRemap_None);
	}

	// "remap" always specifies the remapping done on the "read" side, even if action allows writes.
	// Writes use rrRangeRemap_Inverse(remap). Therefore, when writing a U8 surface with sRGB,
	// somewhat counter-intuitively, you need to specify rrRangeRemap_SRGBToLinear.
	rrbool StartNormalized(rrSurface * surf,rrPixelFormat format,SINTa numRows,ERRSurfaceRowCacheAction action,SINTa alignment_padding = 0,rrRangeRemap remap = rrRangeRemap_None)
	{
		return StartImpl(surf,format,numRows,action,alignment_padding,true,remap);
	}
	
	rrbool StartReadC(const rrSurface * surf,rrPixelFormat format,SINTa numRows,SINTa alignment_padding = 0)
	{
		return StartImpl(const_cast<rrSurface *>(surf),format,numRows,RR_SURFACE_ROW_CACHE_READ,alignment_padding,false,rrRangeRemap_None);
	}

	rrbool StartNormalizedReadC(const rrSurface * surf,rrPixelFormat format,SINTa numRows,SINTa alignment_padding = 0,rrRangeRemap remap = rrRangeRemap_None)
	{
		return StartImpl(const_cast<rrSurface *>(surf),format,numRows,RR_SURFACE_ROW_CACHE_READ,alignment_padding,true,remap);
	}
	
	void * GetRow(SINTa y);

	rrbool MoveCache(SINTa yStart,SINTa numRows);
	rrbool FlushWrite();

	rrbool IsRowInCache(SINTa y) const { return y >= m_cacheStartY && y < (m_cacheStartY+m_cacheRows); }

	//-------------------------------------
	// sugar for common 4I/4F case :

	void Start_4I_8(rrSurface * surf,ERRSurfaceRowCacheAction action,SINTa alignment_padding = 0) { Start(surf,rrPixelFormat_4_S32,8,action,alignment_padding); }
	void Start_4F_8(rrSurface * surf,ERRSurfaceRowCacheAction action,SINTa alignment_padding = 0) { Start(surf,rrPixelFormat_4_F32,8,action,alignment_padding); }

	// "remap" always specifies the remapping done on the "read" side, even if action allows writes.
	// Writes use rrRangeRemap_Inverse(remap). Therefore, when writing a U8 surface with sRGB,
	// somewhat counter-intuitively, you need to specify rrRangeRemap_SRGBToLinear.
	void StartNormalized_4F_8(rrSurface * surf,ERRSurfaceRowCacheAction action,SINTa alignment_padding = 0,rrRangeRemap remap = rrRangeRemap_None)
	{
		StartNormalized(surf,rrPixelFormat_4_F32,8,action,alignment_padding,remap);
	}

	void Start_ReadC_4I_8(const rrSurface * surf,SINTa alignment_padding = 0) { StartReadC(surf,rrPixelFormat_4_S32,8,alignment_padding); }
	void Start_ReadC_4F_8(const rrSurface * surf,SINTa alignment_padding = 0) { StartReadC(surf,rrPixelFormat_4_F32,8,alignment_padding); }
	void StartNormalized_ReadC_4F_8(const rrSurface * surf,SINTa alignment_padding = 0,rrRangeRemap remap = rrRangeRemap_None)
	{
		StartNormalizedReadC(surf,rrPixelFormat_4_F32,8,alignment_padding,remap);
	}

	rrColor4I * GetRow_4I(SINTa y) { RR_ASSERT( m_rows.pixelFormat == rrPixelFormat_4_S32); return (rrColor4I *) GetRow(y); }
	rrColor4F * GetRow_4F(SINTa y) { RR_ASSERT( m_rows.pixelFormat == rrPixelFormat_4_F32); return (rrColor4F *) GetRow(y); }
	
private:
	rrbool StartImpl(rrSurface * surf,rrPixelFormat format,SINTa numRows,ERRSurfaceRowCacheAction action,SINTa alignment_padding,bool normalized,rrRangeRemap remap);

	rrSurface * m_surf = nullptr;
	SINTa		m_cacheStartY = 0;
	SINTa		m_cacheRows = 0;
	SINTa		m_paddingX = 0;
	SINTa		m_paddingY = 0;
	ERRSurfaceRowCacheAction			m_readWrite = RR_SURFACE_ROW_CACHE_NONE;
	rrSurfaceObj	m_rows;
	bool		m_is_pass_through = false;
	bool		m_is_normalized = false;
	rrRangeRemap m_remap = rrRangeRemap_None;
};

RR_NAMESPACE_END

#endif // __RADRRBITMAP_SURFACEROWCACHE_H__
