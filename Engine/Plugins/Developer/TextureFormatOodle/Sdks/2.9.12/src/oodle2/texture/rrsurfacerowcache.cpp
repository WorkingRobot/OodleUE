// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrsurfacerowcache.h"
#include "rrsurfaceblit.h"
#include "rrmath.h"
#include "rrbase.h"
#include <string.h>

RR_NAMESPACE_START

rrSurfaceRowCache::~rrSurfaceRowCache()
{
	FlushWrite();
	
	// m_rows destructor , it's an rrSurfaceObj
}

rrbool rrSurfaceRowCache::StartImpl(rrSurface * surf,rrPixelFormat format,SINTa numRows,ERRSurfaceRowCacheAction action,SINTa alignment_padding,bool normalized,rrRangeRemap remap)
{
	m_is_normalized = normalized;
	m_remap = remap;
	m_paddingX = 0;
	m_paddingY = 0;
	if ( alignment_padding > 0 )
	{
		RR_ASSERT_ALWAYS( rrIsPow2_A(alignment_padding) );
		m_paddingX = rrAlignUpA(surf->width ,alignment_padding) - surf->width;
		m_paddingY = rrAlignUpA(surf->height,alignment_padding) - surf->height;
	}
	
	if ( surf->pixelFormat == format && m_paddingX == 0 && m_paddingY == 0 && remap == rrRangeRemap_None )
	{
		// same format and no padding
		//	just point at the surface
		m_is_pass_through = true;
		
		rrSurface_Free(&m_rows); // it's okay to call Start multiple times
		rrSurface_SetView(&m_rows,surf);
		
		m_readWrite = action;
		m_surf = surf;
		m_cacheStartY = 0;
		m_cacheRows = surf->height;
	}
	else
	{	
		m_is_pass_through = false;
	
		// Alloc will reuse existing if possible
		//rrSurface_Free(&m_rows);
	
		SINTa w = rrAlignUpA(surf->width + m_paddingX,4);
		if ( ! rrSurface_Alloc(&m_rows,w,numRows,format) )
			return false;

		m_readWrite = action;
		m_surf = surf;
		m_cacheStartY = 0;
		m_cacheRows = 0;
	}
		
	return true;
}

void * rrSurfaceRowCache::GetRow(SINTa y)
{
	if ( ! MoveCache(y,1) )
		return NULL;
	
	SINTa iny = y - m_cacheStartY;
	U8 * ptr = rrSurface_Seek(&m_rows,0,iny);
	
	return ptr;
}

rrbool rrSurfaceRowCache::MoveCache(SINTa yStart,SINTa numRows)
{
	RR_ASSERT( numRows <= m_rows.height );
	if ( yStart >= m_cacheStartY && (yStart + numRows) <= (m_cacheStartY + m_cacheRows) )
	{
		// already there :
		return true;
	}

	if ( ! FlushWrite() )
		return false;
	
	RR_ASSERT( ! m_is_pass_through ); // should not get here
	
	m_cacheStartY = yStart;
	m_cacheRows = RR_MIN( m_surf->height + m_paddingY - yStart , m_rows.height );
	
	if ( m_cacheRows < numRows )
	{
		// asked for a block of rows that goes out of bounds
		RR_ASSERT_FAILURE("try padding");
		return false;
	}
	
	if ( m_readWrite & RR_SURFACE_ROW_CACHE_READ )
	{
		RR_ASSERT( m_surf->width <= m_rows.width );
		SINTa w = m_surf->width;
		
		if ( m_cacheStartY >= 0 && (m_cacheStartY + m_cacheRows) <= m_surf->height )
		{
			// simple case, in bounds :

			if ( ! m_is_normalized )
			{
				if ( ! rrSurface_BlitRect_NonNormalized(&m_rows,m_surf,0,m_cacheStartY,0,0,w,m_cacheRows) )
					return false;
			}
			else
			{
				if ( ! rrSurface_BlitRect_Normalized(&m_rows,m_surf,0,m_cacheStartY,0,0,w,m_cacheRows,m_remap) )
					return false;
			}
		}
		else
		{
		
			// ugly logic to handle some rows being out of bounds
			
			for LOOP(r,m_cacheRows)
			{
				SINTa surfX = 0;
				SINTa surfY = yStart + r;
				SINTa cacheX = 0;
				SINTa cacheY = r;
				
				// NOTE(fg): 2020-05-25 clamp not mirror for out-of-bounds Y, to match what we do with layouts
				surfY = RR_MIN(surfY,m_surf->height-1);

				if ( ! m_is_normalized )
				{
					if ( ! rrSurface_BlitRect_NonNormalized(&m_rows,m_surf,surfX,surfY,cacheX,cacheY,w,1) )
						return false;
				}
				else
				{
					if ( ! rrSurface_BlitRect_Normalized(&m_rows,m_surf,surfX,surfY,cacheX,cacheY,w,1,m_remap) )
						return false;
				}
			}
		}
		
		if ( m_paddingX > 0 )
		{
			const rrPixelFormatInfo * pInfo = rrPixelFormat_GetInfo(m_rows.pixelFormat);
			SINTa bypp = pInfo->bytesPerPixel;
			RR_ASSERT_ALWAYS( bypp != 0 );
			
			for LOOP(r,m_cacheRows)
			{
				U8 * pRow = rrPixelFormat_Seek(m_rows.pixelFormat,m_rows.data,m_rows.stride,0,r);
				
				// pad X : 
				for(SINTa p=0;p<m_paddingX;p++)
				{
					SINTa tox = m_surf->width + p;
					//SINTa fmx = m_surf->width - p - 1;
					//if ( fmx < 0 ) fmx = 0; // when width < padding
					SINTa fmx = m_surf->width - 1; // clamp not mirror for X, to match what we do with layouts
					
					memcpy( pRow + tox * bypp , pRow + fmx * bypp, bypp );
				}
			}
		}
	}
	
	return true;
}
	
// FlushWrite :
//	if there are changes in my cache, copy them back to the surface
rrbool rrSurfaceRowCache::FlushWrite()
{
	// readonly cache just drops changes
	if ( ! (m_readWrite & RR_SURFACE_ROW_CACHE_WRITE) )
		return true;

	if ( m_cacheRows == 0 ) // nothing to flush
		return true;
		
	if ( m_is_pass_through ) // already written
		return true;

	RR_ASSERT( m_surf->width <= m_rows.width );
	SINTa w = m_surf->width;

	// cache may be padded, some of its extent may not lie in surface
	// those pixels that were changed in the padding area will just be dropped

	// find the intersection of the cache rect with the surface rect :

	SINTa cacheX = 0;
	SINTa cacheY = 0;
	
	SINTa surfX = 0;
	SINTa surfY = m_cacheStartY;
	
	if ( m_cacheStartY < 0 )
	{
		surfY = 0;
		cacheY = -m_cacheStartY;
	}

	SINTa numRows = RR_MIN( m_cacheRows - cacheY , m_surf->height - surfY );
	if ( numRows > 0 )
	{
		rrRangeRemap writeRemap = rrRangeRemap_None;
		if ( m_is_normalized )
			writeRemap = rrRangeRemap_Inverse(m_remap);

		if ( ! m_is_normalized )
		{
			if ( ! rrSurface_BlitRect_NonNormalized(m_surf,&m_rows,cacheX,cacheY,surfX,surfY,w,numRows) )
				return false;
		}
		else
		{
			if ( ! rrSurface_BlitRect_Normalized(m_surf,&m_rows,cacheX,cacheY,surfX,surfY,w,numRows,writeRemap) )
				return false;
		}
	}
	
	m_cacheRows = 0;
	
	return true;
}

RR_NAMESPACE_END
