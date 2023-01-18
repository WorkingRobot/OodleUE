// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrsurfaceutil.h"
#include "rrcolor.h"
#include "rrsurfacerowcache.h"
#include "rrsurfaceblit.h"
#include "rrsurfacedxtc.h"
#include "rrsurfacefilters.h"
#include "templates/rrstl.h"
#include "rrvecc.h"
#include "threadprofiler.h"
#include "templates/rrvector.h"
#include "templates/rralgorithm.h"
#include "vec128.inl"
#include "log2table.h"
#include <string.h>

//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"

RR_NAMESPACE_START

#if 0
rrbool rrSurface_PadUpToPow2(rrSurface * surf)
{
	S32 new_w = rrNextPow2(surf->width);
	S32 new_h = rrNextPow2(surf->height);
	
	if ( surf->width == new_w && surf->height == new_h )
		return true;
	
	rrSurfaceObj new_surf;
	if ( ! rrSurface_Alloc(&new_surf,new_w,new_h,surf->pixelFormat) )
		return false;
	
	rrSurface_SetDataBytesZero(&new_surf);
	
	rrSurface_Blit(&new_surf,surf);
	
	rrSurface_Swap(&new_surf,surf);
	
	return true;
}
#endif

// change format in place by copying out then swapping pointers
rrbool rrSurface_ChangeFormat_NonNormalized(rrSurface * surf, rrPixelFormat newFormat)
{
	if ( surf->pixelFormat == newFormat )
		return true;
	if ( surf->data == NULL )
		return false;
	
	if ( ! rrSurface_Depalettize(surf,newFormat) )
		return false;
		
	if ( surf->pixelFormat == newFormat )
		return true;
		
	rrSurfaceObj temp;
	if ( ! rrSurface_AllocCopy_ChangeFormatNonNormalized(&temp,surf,newFormat) )
		return false;
		
	rrSurface_Swap(&temp,surf);
	
	return true;
}

static bool calc_normalize_remap_mul_add(F32 * pmult, F32 * padd, rrPixelFormat in_format, rrPixelFormat out_format, rrRangeRemap remap)
{
	// Determine input/output scaling for the given formats
	// returns 0.0f if it's not a "simple" format that we
	// know how to deal with.
	F32 in_scale  = (F32) rrPixelFormat_GetNormalizedScale(in_format);
	F32 out_scale = (F32) rrPixelFormat_GetNormalizedScale(out_format);
	if ( in_scale == 0.0f || out_scale == 0.0f )
		return false;

	// Compute output remapping
	// First: undo input scaling to get us into [0,1] for UNORM
	// or [-1,1] for SNORM
	F32 mult = 1.0f / in_scale;
	F32 add = 0.0f;

	// Second: apply range remap
	// will be used in _MultAddF which does the mult then the add
	switch ( remap )
	{
	case rrRangeRemap_None:
		break;

	case rrRangeRemap_UnormToSnorm: // map [0,1] to [-1,1]
		// x |-> x*2 - 1
		mult *= 2.0f;
		add = add * 2.0f - 1.0f; // intentional funny way of writing add = -1
		break;

	case rrRangeRemap_SnormToUnorm: // map [-1,1] to [0,1]
		// x |-> x*0.5 + 0.5
		mult *= 0.5f;
		add = add * 0.5f + 0.5f;
		break;

	default:
		return false;
	}

	// Third: output scaling
	mult *= out_scale;
	add *= out_scale;

	*pmult = mult;
	*padd = add;
	return true;
}

rrbool rrSurface_ChangeFormatNormalized(rrSurface * surf, rrPixelFormat newFormat, rrRangeRemap remap)
{
	if ( surf->pixelFormat == newFormat && remap == rrRangeRemap_None )
		return true;
	if ( surf->data == NULL )
		return false;

	if ( ( RR_PIXELFORMAT_GET_TYPE(surf->pixelFormat) == RR_PIXELFORMAT_GET_TYPE(newFormat) ) &&
		remap == rrRangeRemap_None )
	{
		// short circuit for non-converting blit
		//	debug check this should be the same as running the below!
		return rrSurface_ChangeFormat_NonNormalized(surf,newFormat);
	}
	
	// Determine remapping to apply
	// we must be simple scalar channels for this
	F32 mult, add;
	if ( ! calc_normalize_remap_mul_add(&mult,&add,surf->pixelFormat,newFormat,remap) )
		return false;

	// this is wrong, it would not be normalized
	//if ( ! rrSurface_Depalettize(surf,newFormat) )
	//	return false;

	// And apply the transform!
	return rrSurface_MultAddF_ChangeFormat(surf, newFormat, mult, add);
}

// same as rrSurface_ChangeFormatNormalized
//	but also accepts BCN input and decompresses it
rrbool rrSurface_ChangeFormatNormalized_AndDecomp(rrSurface * surf, rrPixelFormat newFormat, rrRangeRemap remap)
{
	if ( surf->pixelFormat == newFormat && remap == rrRangeRemap_None )
		return true;
	if ( surf->data == NULL )
		return false;
		
	if ( rrPixelFormat_IsBlockCompressed(surf->pixelFormat) )
	{
		if ( ! rrSurfaceDXTC_DecompressIfBCNInPlace(surf,newFormat) )
			return false;
		
		if ( surf->pixelFormat == newFormat && remap == rrRangeRemap_None )
			return true;
	}
	
	rrSurface_Depalettize(surf);
	
	return rrSurface_ChangeFormatNormalized(surf,newFormat,remap);	
}

rrbool rrSurface_BlitNormalized(rrSurface * to, const rrSurface * from, rrRangeRemap remap)
{
	// Dimensions must match
	if ( to->width != from->width || to->height != from->height )
		return false;

	if ( ( RR_PIXELFORMAT_GET_TYPE(from->pixelFormat) == RR_PIXELFORMAT_GET_TYPE(to->pixelFormat) ) &&
		remap == rrRangeRemap_None )
	{
		// short circuit for non-converting blit
		//	debug check this should be the same as running the below!
		return rrSurface_Blit_NonNormalized(to,from);
	}
	
	THREADPROFILESCOPE("BlitNormalized");
	
	// the old NonNormalized Blit *does* do depal and decomp
	// so we should do it to to match :
	if ( from->pixelFormat == rrPixelFormat_Palette8 )
	{
		rrSurfaceObj depal;
		// same format path has already been checked
		//	so we must depalettize
		RR_ASSERT( to->pixelFormat != rrPixelFormat_Palette8 );
		rrSurface_DepalettizeTo(from,&depal,rrPixelFormat_B8G8R8A8);
		return rrSurface_BlitNormalized(to,&depal,remap);
	}
	   
	if ( rrPixelFormat_IsBlockCompressed(from->pixelFormat) )
	{
		if ( remap == rrRangeRemap_None )
		{
			return rrSurfaceDXTC_DecompressBCN(to,from);
		}
		else
		{
			rrSurfaceObj decomp;
			if ( ! rrSurfaceDXTC_DecompressBCN(&decomp,from) )
				return false;
			return rrSurface_BlitNormalized(to,&decomp,remap);
		}
	}
	
	// Determine remapping to apply (also checks whether formats are supported)
	F32 mult, add;
	if ( ! calc_normalize_remap_mul_add(&mult,&add,from->pixelFormat,to->pixelFormat,remap) )
		return false;

	if ( mult == 1.f && add == 0.f )
	{
		// eg. from/to = F32/F16 & remap = None

		// short circuit for non-converting blit
		//	debug check this should be the same as running the below!
		return rrSurface_Blit_NonNormalized(to,from);
	}
	else
	{
		rrSurface_MultAddF(to,from,mult,add);
	}

	/*
	// debug check the short circuit blit makes the exact same output :
	//	(and if-0 out the short circuit check above to run this)
	if ( ( RR_PIXELFORMAT_GET_TYPE(from->pixelFormat) == RR_PIXELFORMAT_GET_TYPE(to->pixelFormat) ) &&
		remap == rrRangeRemap_None )
	{
		rrSurfaceObj temp;
		rrSurface_AllocCopy(&temp,to);
		rrSurface_Blit(&temp,from);
	
		rrSurface_SetNonPixelBytesZero(&temp);
		rrSurface_SetNonPixelBytesZero(to);
		int cmp = memcmp(to->data,temp.data,rrSurface_GetDataSizeBytes(to));
		RR_ASSERT( cmp == 0 );
		if ( cmp != 0 && to->pixelFormat == rrPixelFormat_4_U8 )
		{
			rrColor32BGRA * pto = (rrColor32BGRA *)to->data;
			rrColor32BGRA * ptemp = (rrColor32BGRA *)temp.data;
			int np = from->width*to->height;
			for LOOP(p,np)
			{
				rrColor32BGRA cto = pto[p];
				rrColor32BGRA ctemp = ptemp[p];
				U32 dsqr = rrColor32BGRA_DeltaSqrRGBA(cto,ctemp);
				RR_ASSERT( dsqr == 0 );
			}
		}
	}
	/**/
	
	return true;
}

void rrSurface_SwapRB(rrSurface * surf)
{
	rrSurfaceRowCache rows;
	rows.Start_4I_8(surf,RR_SURFACE_ROW_CACHE_READWRITE);
	
	int h = surf->height;
	int w = surf->width;
	
	for(int y=0;y<h;y++)
	{
		rrColor4I * row = rows.GetRow_4I(y);
		
		for(int x=0;x<w;x++)
		{
			RR_NAMESPACE::swap(row[x].r,row[x].b);
		}
	}
}

// note - to == from is okay !
void rrSurface_ApplyPow(rrSurface * to,const rrSurface * from,  F32 power)
{
	rrSurfaceRowCache fmRows;
	fmRows.Start_4F_8(const_cast<rrSurface *>(from),RR_SURFACE_ROW_CACHE_READ);
	
	rrSurfaceRowCache toRows;
	toRows.Start_4F_8(to,RR_SURFACE_ROW_CACHE_WRITE);
	
	int h = RR_MIN( from->height, to->height );
	int w = RR_MIN( from->width, to->width );
	
	int numChannels = rrPixelFormat_GetInfo(from->pixelFormat)->channels;
	
	for(int y=0;y<h;y++)
	{
		const rrColor4F * fmRow = fmRows.GetRow_4F(y);
		rrColor4F * toRow = toRows.GetRow_4F(y);
		
		switch(numChannels)
		{
			case 4:
			for(int x=0;x<w;x++)
			{
				toRow[x].r = powf(fmRow[x].r*(1.f/255.f),power) * 255.f;
				toRow[x].g = powf(fmRow[x].g*(1.f/255.f),power) * 255.f;
				toRow[x].b = powf(fmRow[x].b*(1.f/255.f),power) * 255.f;
				toRow[x].a = powf(fmRow[x].a*(1.f/255.f),power) * 255.f;
			}
			break;
			
			case 3:
			for(int x=0;x<w;x++)
			{
				toRow[x].r = powf(fmRow[x].r*(1.f/255.f),power) * 255.f;
				toRow[x].g = powf(fmRow[x].g*(1.f/255.f),power) * 255.f;
				toRow[x].b = powf(fmRow[x].b*(1.f/255.f),power) * 255.f;
				toRow[x].a = fmRow[x].a;
			}
			break;
			
			case 2:
			for(int x=0;x<w;x++)
			{
				toRow[x].r = powf(fmRow[x].r*(1.f/255.f),power) * 255.f;
				toRow[x].g = powf(fmRow[x].g*(1.f/255.f),power) * 255.f;
				toRow[x].b = toRow[x].b;
				toRow[x].a = fmRow[x].a;
			}
			break;
			
			case 1:
			for(int x=0;x<w;x++)
			{
				toRow[x].r = powf(fmRow[x].r*(1.f/255.f),power) * 255.f;
				toRow[x].g = toRow[x].g;
				toRow[x].b = toRow[x].b;
				toRow[x].a = fmRow[x].a;
			}
			break;
		}
	}
}

static F32 mypowf(F32 base, F32 exp)
{
	// avoid C stdlib
	// x^y = e^(y * ln(x))
	double ret = rr_exp_approx( exp * rr_loge(base) );
	return (F32) ret;
}

static F32 Gamma_SRGB_UnitLinearFloat_To_UnitSRGBFloat_Slow(F32 linear)
{
	RR_ASSERT( linear >= 0.f );
	if ( linear <= 0.0031308 )
	{
		return 12.92f * linear;
	}
	else
	{
		return 1.055f * mypowf( linear , 1.f/2.4f ) - 0.055f;
	}
}

void rrSurface_Gamma_SRGB_UnitLinearFloat_To_UnitSRGBFloat_Slow_3F(rrSurface * to,const rrSurface * from)
{
	RR_ASSERT( from != to );
	
	rrSurfaceRowCache fmRows;
	fmRows.StartReadC(from,rrPixelFormat_3_F32,8);
	
	int w = from->width;
	int h = from->height;
	rrSurface_Alloc(to,w,h,rrPixelFormat_3_F32);
	
	rrSurfaceRowCache toRows;
	toRows.Start(to,rrPixelFormat_3_F32,8,RR_SURFACE_ROW_CACHE_WRITE);
		
	for(int y=0;y<h;y++)
	{
		const F32 * fmRow = (F32 *) fmRows.GetRow(y);
		F32 * toRow = (F32 *) toRows.GetRow(y);
		
		int n = w*3;
		for LOOP(i,n)
		{
			toRow[i] = Gamma_SRGB_UnitLinearFloat_To_UnitSRGBFloat_Slow( fmRow[i] );
		}
	}
}

void rrSurface_Gamma_SRGB_UnitLinearFloat_To_255Int(rrSurface * to,const rrSurface * from)
{
	RR_ASSERT( from != to );

	rrSurfaceRowCache fmRows;
	fmRows.Start_4F_8(const_cast<rrSurface *>(from),RR_SURFACE_ROW_CACHE_READ);
	
	int w = from->width;
	int h = from->height;
	rrSurface_Alloc(to,w,h,rrPixelFormat_B8G8R8A8);
	
	rrSurfaceRowCache toRows;
	toRows.Start(to,rrPixelFormat_B8G8R8A8,8,RR_SURFACE_ROW_CACHE_WRITE);
		
	for(int y=0;y<h;y++)
	{
		const rrColor4F * fmRow = fmRows.GetRow_4F(y);
		rrColor32BGRA * toRow = (rrColor32BGRA *) toRows.GetRow(y);
		
		for(int x=0;x<w;x++)
		{
			toRow[x].u.r = linear_unit_float_to_srgb_byte(fmRow[x].r);
			toRow[x].u.g = linear_unit_float_to_srgb_byte(fmRow[x].g);
			toRow[x].u.b = linear_unit_float_to_srgb_byte(fmRow[x].b);
			// A ?
			//toRow[x].u.a = linear_unit_float_to_srgb_byte(fmRow[x].a);
			toRow[x].u.a = 255;
		}
	}
}

void rrSurface_Gamma_SRGB_255Int_To_UnitLinearFloat(rrSurface * to,const rrSurface * from)
{
	RR_ASSERT( from != to );

	rrSurfaceRowCache fmRows;
	fmRows.StartReadC(from,rrPixelFormat_B8G8R8A8,8);
	
	int w = from->width;
	int h = from->height;
	rrSurface_Alloc(to,w,h,rrPixelFormat_4_F32);
	
	rrSurfaceRowCache toRows;
	toRows.Start_4F_8(to,RR_SURFACE_ROW_CACHE_WRITE);
		
	for(int y=0;y<h;y++)
	{
		const rrColor32BGRA * fmRow = (rrColor32BGRA *) fmRows.GetRow(y);
		rrColor4F* toRow = toRows.GetRow_4F(y);
		
		for(int x=0;x<w;x++)
		{
			toRow[x].r = srgb_byte_to_linear_unit_float(fmRow[x].u.r);
			toRow[x].g = srgb_byte_to_linear_unit_float(fmRow[x].u.g);
			toRow[x].b = srgb_byte_to_linear_unit_float(fmRow[x].u.b);
			// A ?
			//toRow[x].u.a = srgb_byte_to_linear_unit_float(fmRow[x].a);
			toRow[x].a = fmRow[x].u.a/255.f;
		}
	}
}

/**

For RGB color spaces that use the ITU-R BT.709 primaries (or sRGB, which defines the same primaries), relative luminance can be calculated from linear RGB components:[2]

    Y = 0.2126 R + 0.7152 G + 0.0722 B 

**/    

void rrSurface_MakeGray(rrSurface * to,const rrSurface * from)
{
	rrSurfaceRowCache fmRows;
	fmRows.Start_4F_8(const_cast<rrSurface *>(from),RR_SURFACE_ROW_CACHE_READ);
	
	rrSurfaceRowCache toRows;
	toRows.Start_4F_8(to,RR_SURFACE_ROW_CACHE_WRITE);
	
	int h = RR_MIN( from->height, to->height );
	int w = RR_MIN( from->width, to->width );
	
	for(int y=0;y<h;y++)
	{
		const rrColor4F * fmRow = fmRows.GetRow_4F(y);
		rrColor4F * toRow = toRows.GetRow_4F(y);
		
		for(int x=0;x<w;x++)
		{
			float luma = 0.60f * fmRow[x].g + 0.15f * fmRow[x].b + 0.25f * fmRow[x].r;
			toRow[x].r = luma;
			toRow[x].g = luma;
			toRow[x].b = luma;
			toRow[x].a = 255.f;
		}
	}	
}									

void rrSurface_FillSolidColorF(rrSurface * to,const rrColor4F & color)
{
	rrSurfaceRowCache toRows;
	toRows.Start_4F_8(to,RR_SURFACE_ROW_CACHE_WRITE);
	
	int h = to->height;
	int w = to->width ;
	
	for(int y=0;y<h;y++)
	{
		rrColor4F * toRow = toRows.GetRow_4F(y);
		
		for(int x=0;x<w;x++)
		{
			toRow[x] = color;
		}
	}	
}

void rrSurface_FillSolidColorI(rrSurface * to,const rrColor4I & color)
{
	rrSurfaceRowCache toRows;
	toRows.Start_4I_8(to,RR_SURFACE_ROW_CACHE_WRITE);
	
	int h = to->height;
	int w = to->width ;
	
	for(int y=0;y<h;y++)
	{
		rrColor4I * toRow = toRows.GetRow_4I(y);
		
		for(int x=0;x<w;x++)
		{
			toRow[x] = color;
		}
	}	
}

// FillChannel is often used to set A to 255
void rrSurface_FillChannelF(rrSurface * surf, rrSurface_EChannel channel, F32 value )
{
	rrSurfaceRowCache toRows;
	toRows.Start_4F_8(surf,RR_SURFACE_ROW_CACHE_READWRITE);
	
	int h = surf->height;
	int w = surf->width ;
	
	for(int y=0;y<h;y++)
	{
		rrColor4F * toRow = toRows.GetRow_4F(y);
		
		for(int x=0;x<w;x++)
		{
			(&toRow[x].r)[channel] = value;
		}
	}	
}

void rrSurface_FillChannelI(rrSurface * surf, rrSurface_EChannel channel, S32 value )
{
	rrSurfaceRowCache toRows;
	toRows.Start_4I_8(surf,RR_SURFACE_ROW_CACHE_READWRITE);
	
	int h = surf->height;
	int w = surf->width ;
	
	for(int y=0;y<h;y++)
	{
		rrColor4I * toRow = toRows.GetRow_4I(y);
		
		for(int x=0;x<w;x++)
		{
			(&toRow[x].r)[channel] = value;
		}
	}	
}

// rrSurface_SelectChannels :
//	specify the channel you want to read from, eg :
//	SelectChannels(R,R,R,R);
//	replicates R in input to all channels of output
void rrSurface_SelectChannels(rrSurface * to,const rrSurface * from, 
									rrSurface_EChannel to_R,
									rrSurface_EChannel to_G,
									rrSurface_EChannel to_B,
									rrSurface_EChannel to_A )
{
	rrSurfaceRowCache fmRows;
	fmRows.Start_4F_8(const_cast<rrSurface *>(from),RR_SURFACE_ROW_CACHE_READ);
	
	rrSurfaceRowCache toRows;
	//toRows.Start_4F_8(to,RR_SURFACE_ROW_CACHE_WRITE);
	toRows.Start_4F_8(to,RR_SURFACE_ROW_CACHE_READWRITE); // must READWRITE in case we have Nones
	
	int h = RR_MIN( from->height, to->height );
	int w = RR_MIN( from->width, to->width );
	
	for(int y=0;y<h;y++)
	{
		const rrColor4F * fmRow = fmRows.GetRow_4F(y);
		rrColor4F * toRow = toRows.GetRow_4F(y);
		
		for(int x=0;x<w;x++)
		{
			// read to a temporary so that we can work in place (to == from) surface
			rrColor4F color = fmRow[x];
			
			// could do a channel-zero this way but not a Channel_None
			//F32 channels[5];
			//memcpy(channels,fmRow+x,sizeof(rrColor4F));
			
			// rrSurface_Channel_None means leave alone
			if ( to_R != rrSurface_Channel_None )
				toRow[x].r = (&color.r)[to_R];
			if ( to_G != rrSurface_Channel_None )
				toRow[x].g = (&color.r)[to_G];
			if ( to_B != rrSurface_Channel_None )
				toRow[x].b = (&color.r)[to_B];
			if ( to_A != rrSurface_Channel_None )
				toRow[x].a = (&color.r)[to_A];
		}
	}	
}									



void rrSurface_GetMoments(const rrSurface * surf, 
				rrSurface_EChannel channel,
				F64 * pMean,
				F64 * pVariance,
				F64 * pMin,
				F64 * pMax )
{
	rrSurfaceRowCache rows;
	rows.Start_4F_8(const_cast<rrSurface *>(surf),RR_SURFACE_ROW_CACHE_READ);
	
	int h = surf->height;
	int w = surf->width;
	
	F64 sum = 0;
	F64 sumSqr = 0;
	F64 lo = 1e30f;
	F64 hi = -1e30f;
	
	for(int y=0;y<h;y++)
	{
		const rrColor4F * row = rows.GetRow_4F(y);
		
		for(int x=0;x<w;x++)
		{
			F32 val = (&(row[x].r))[channel];
			
			sum += val;
			sumSqr += (F64)val*val;
			lo = RR_MIN(lo,val);
			hi = RR_MAX(hi,val);
		}
	}
	
	sum *= 1.0/(w*h);
	sumSqr *= 1.0/(w*h);

	double variance = sumSqr - sum*sum;
	// variance should be >= 0 but can drift negative
	RR_ASSERT( variance >= -0.0001*sumSqr );
	if ( variance < 0 ) variance = 0;
	
	*pMean = sum;
	*pVariance = variance;
	*pMin = lo;
	*pMax = hi;
}

F64 rrSurface_GetL2RGBA(const rrSurface * surf )
{
	rrSurfaceRowCache rows;
	rows.Start_4F_8(const_cast<rrSurface *>(surf),RR_SURFACE_ROW_CACHE_READ);
	
	int h = surf->height;
	int w = surf->width;
	
	rrVec4f sum(0,0,0,0);
	rrVec4f sumSqr(0,0,0,0);
	
	for(int y=0;y<h;y++)
	{
		const rrVec4f * row = (const rrVec4f *) rows.GetRow_4F(y);
		
		for(int x=0;x<w;x++)
		{
			const rrVec4f & C = row[x];
		
			sum += C;

			sumSqr.x += C.x * C.x;
			sumSqr.y += C.y * C.y;
			sumSqr.z += C.z * C.z;
			sumSqr.w += C.w * C.w;
		}
	}
	
	sum *= 1.f/(w*h);
	sumSqr *= 1.f/(w*h);
	
	rrVec4f L2C;
	L2C.x = sumSqr.x - sum.x*sum.x;
	L2C.y = sumSqr.y - sum.y*sum.y;
	L2C.z = sumSqr.z - sum.z*sum.z;
	L2C.w = sumSqr.w - sum.w*sum.w;
	
	return ( L2C.x + L2C.y + L2C.z + L2C.w );
}

F64 rrSurface_GetSSD(
				const rrSurface * surf1, 
				const rrSurface * surf2,
				const F32 * channel_weights
				)
{
	THREADPROFILESCOPE("GetSSD");
	
	rrSurfaceRowCache rows1;
	rrSurfaceRowCache rows2;
	int w = RR_MIN( surf1->width , surf2->width );
	int h = RR_MIN( surf1->height, surf2->height );
	
	const rrPixelFormatInfo * info1 = rrPixelFormat_GetInfo(surf1->pixelFormat);
	const rrPixelFormatInfo * info2 = rrPixelFormat_GetInfo(surf2->pixelFormat);
	
	int c1 = info1->channels;
	int c2 = info2->channels;
	
	int channels = RR_MIN(c1,c2); // ?
	RR_ASSERT( channels > 0 );
	if ( channels == 0 ) return 0.0;
	
	if ( ! ( info1->isFloat || info1->isSigned ) &&
	     ! ( info2->isFloat || info2->isSigned ) &&
		info1->bytesPerPixel == c1 &&
		info2->bytesPerPixel == c2 &&
		! channel_weights)
	{
		// non-float, simple channel data, each channel is 1 byte U8
		// just use standard 4*U8 color
		// everything should have a fast path for 4_U8, that's the standard format
	
		// rrPixelFormat_4_U8 = rrColor32BGRA
		rows1.Start(const_cast<rrSurface *>(surf1),rrPixelFormat_4_U8,8,RR_SURFACE_ROW_CACHE_READ,0);
		rows2.Start(const_cast<rrSurface *>(surf2),rrPixelFormat_4_U8,8,RR_SURFACE_ROW_CACHE_READ,0);
		
		U64 sumSqr = 0;
		
		for(int y=0;y<h;y++)
		{
			const rrColor32BGRA * row1 = (const rrColor32BGRA *) rows1.GetRow(y);
			const rrColor32BGRA * row2 = (const rrColor32BGRA *) rows2.GetRow(y);
			
			switch(channels)
			{
			case 1:
				for(int x=0;x<w;x++)
				{
					U64 dSqr = Square(row1[x].u.r - row2[x].u.r);
					
					sumSqr += dSqr;
				}
				break;
			
			case 2:
				for(int x=0;x<w;x++)
				{
					U64 dSqr = Square(row1[x].u.r - row2[x].u.r) + Square(row1[x].u.g - row2[x].u.g);
					
					sumSqr += dSqr;
				}
				break;
			
			case 3:
				for(int x=0;x<w;x++)
				{
					U64 dSqr = rrColor32BGRA_DeltaSqrRGB(row1[x],row2[x]);
					
					sumSqr += dSqr;
				}
				break;
			
			default:
			case 4:
				for(int x=0;x<w;x++)
				{
					U64 dSqr = rrColor32BGRA_DeltaSqrRGBA(row1[x],row2[x]);
					
					sumSqr += dSqr;
				}
				break;
			}
		}
		
		return (F64) sumSqr;
	}
	else
	{
		rows1.Start_4F_8(const_cast<rrSurface *>(surf1),RR_SURFACE_ROW_CACHE_READ);
		rows2.Start_4F_8(const_cast<rrSurface *>(surf2),RR_SURFACE_ROW_CACHE_READ);

		F64 sumR = 0, sumG = 0, sumB = 0, sumA = 0;
		
		for(int y=0;y<h;y++)
		{
			const rrColor4F * row1 = rows1.GetRow_4F(y);
			const rrColor4F * row2 = rows2.GetRow_4F(y);
			
			switch(channels)
			{
			case 1:
				for(int x=0;x<w;x++)
				{
					sumR += F32_Square(row1[x].r - row2[x].r);
				}
				break;
			
			case 2:
				for(int x=0;x<w;x++)
				{
					sumR += F32_Square(row1[x].r - row2[x].r);
					sumG += F32_Square(row1[x].g - row2[x].g);
				}
				break;
			
			case 3:
				for(int x=0;x<w;x++)
				{
					sumR += F32_Square(row1[x].r - row2[x].r);
					sumG += F32_Square(row1[x].g - row2[x].g);
					sumB += F32_Square(row1[x].b - row2[x].b);
				}
				break;
			
			default:
			case 4:
				for(int x=0;x<w;x++)
				{
					sumR += F32_Square(row1[x].r - row2[x].r);
					sumG += F32_Square(row1[x].g - row2[x].g);
					sumB += F32_Square(row1[x].b - row2[x].b);
					sumA += F32_Square(row1[x].a - row2[x].a);
				}
				break;
			}
		}

		static const F32 unit_weights[4] = { 1.0f, 1.0f, 1.0f, 1.0f };
		const F32 * wf = channel_weights ? channel_weights : unit_weights;

		F64 sumSqr = sumR * wf[0] + sumG * wf[1] + sumB * wf[2] + sumA * wf[3];
		
		return sumSqr;
	}
}

F64 rrSurface_GetSAD(
				const rrSurface * surf1, 
				const rrSurface * surf2
				)
{
	rrSurfaceRowCache rows1;
	rows1.Start_4F_8(const_cast<rrSurface *>(surf1),RR_SURFACE_ROW_CACHE_READ);
	
	rrSurfaceRowCache rows2;
	rows2.Start_4F_8(const_cast<rrSurface *>(surf2),RR_SURFACE_ROW_CACHE_READ);
	
	int w = RR_MIN( surf1->width , surf2->width );
	int h = RR_MIN( surf1->height, surf2->height );
	
	int c1 = rrPixelFormat_GetInfo(surf1->pixelFormat)->channels;
	int c2 = rrPixelFormat_GetInfo(surf2->pixelFormat)->channels;
	
	int channels = RR_MIN(c1,c2); // ?
	RR_ASSERT( channels > 0 );
	if ( channels == 0 ) return 0.0;
	
	F64 sum = 0;
	
	for(int y=0;y<h;y++)
	{
		const rrColor4F * row1 = rows1.GetRow_4F(y);
		const rrColor4F * row2 = rows2.GetRow_4F(y);
		
		switch(channels)
		{
		case 1:
			for(int x=0;x<w;x++)
			{
				sum += fabsf(row1[x].r - row2[x].r);
			}
			break;
		
		case 2:
			for(int x=0;x<w;x++)
			{
				sum += fabsf(row1[x].r - row2[x].r) + fabsf(row1[x].g - row2[x].g);
			}
			break;
		
		case 3:
			for(int x=0;x<w;x++)
			{
				sum += rrColor4F_DeltaL1RGB(row1[x],row2[x]);
			}
			break;
		
		default:
		case 4:
			for(int x=0;x<w;x++)
			{
				sum += rrColor4F_DeltaL1RGBA(row1[x],row2[x]);
			}
			break;
		}
	}
	
	return sum;
}

F64 rrSurface_GetMSE(
				const rrSurface * surf1, 
				const rrSurface * surf2,
				const F32 * channel_weights
				)
{
	F64 sumSqr = rrSurface_GetSSD(surf1,surf2,channel_weights);

	int w = RR_MIN( surf1->width , surf2->width );
	int h = RR_MIN( surf1->height, surf2->height );
	
	// I'm doing average per *pixel*
	// note : error per *sample* is more standard than error per *pixel*
	int numPixels = w*h; 
	//int numSamples = numPixels*channels;
	sumSqr *= 1.0/numPixels;
	
	return sumSqr;
}

// should return the same thing as rrSurface_RMSE_Normalized_BCNAware
F64 rrSurface_GetMSE8BitEquivalent(
	const rrSurface * surf1,
	const rrSurface * surf2,
	const F32 * channel_weights
	)
{
	// does not do cross-type
	RR_ASSERT_ALWAYS( RR_PIXELFORMAT_GET_TYPE(surf1->pixelFormat) == RR_PIXELFORMAT_GET_TYPE(surf2->pixelFormat) );

	F64 mse = rrSurface_GetMSE(surf1,surf2,channel_weights);
	F64 scale = 1.0;
	switch ( RR_PIXELFORMAT_GET_TYPE(surf1->pixelFormat) )
	{
	case RR_PIXELFORMAT_TYPE_U8:	scale = 1.0; break; // already 8 bits!
	case RR_PIXELFORMAT_TYPE_U16:	scale = 255.0 / 65535.0; break;
	case RR_PIXELFORMAT_TYPE_S32:	scale = 1.0; break; // already in 8-bit value range by default
	case RR_PIXELFORMAT_TYPE_F16:	scale = 255.0 / 1.0; break; // assume F16 data in [0,1]. revise?
	case RR_PIXELFORMAT_TYPE_F32:	scale = 255.0 / 1.0; break; // assume F32 data in [0,1]. revise?
	case RR_PIXELFORMAT_TYPE_S8:	scale = 1.0; break; // already 8 bits!
	case RR_PIXELFORMAT_TYPE_S16:	scale = 127.0 / 32767.0; break;
	}

	return scale * scale * mse;
}

static S32 remap_f16(S32 flt)
{
	// positive floats -> positive [0,0x7fff]
	// negative floats -> [-0x8000,-1]
	S32 ret = (flt & 0x7fff) ^ (-(flt >> 15));
	return ret;
}

F64 rrSurface_GetF16BitsSSD_RGBA_Deprecated(const rrSurface * surf1, const rrSurface * surf2)
{
	// DON'T USE ME
	// 
	//	we don't want alpha diffs
	//	could use _3_F16 here

	rrSurfaceRowCache rows1, rows2;
	rows1.StartReadC(surf1, rrPixelFormat_4_F16, 8);
	rows2.StartReadC(surf2, rrPixelFormat_4_F16, 8);

	int w = RR_MIN(surf1->width,  surf2->width);
	int h = RR_MIN(surf1->height, surf2->height);
	F64 sumSqr = 0; // S64 can overflow

	for(int y=0;y<h;y++)
	{
		const U16 * row1 = (const U16 *)rows1.GetRow(y);
		const U16 * row2 = (const U16 *)rows2.GetRow(y);

		// 4 channel RGBA !
		//	BC6 doesn't do A so this can be garbage
		// 0x3C00 vs 0xFC00

		// -> should always have 1.0 in A for both if outer code has done the right thing
		//	we just do some work diffing 1.0's that we always expect to be zero

		for(int i=0;i<w*4;i++)
		{
			S32 cur = Square(remap_f16(row1[i]) - remap_f16(row2[i]));
			sumSqr += cur;
		}
	}

	return static_cast<F64>(sumSqr);
}

F64 rrSurface_GetF16BitsMSE_RGBA_Deprecated(const rrSurface * surf1, const rrSurface * surf2)
{
	// DON'T USE ME

	F64 sumSqr = rrSurface_GetF16BitsSSD_RGBA_Deprecated(surf1,surf2);

	int w = RR_MIN( surf1->width , surf2->width );
	int h = RR_MIN( surf1->height, surf2->height );

	// I'm doing average per *pixel*
	// note : error per *sample* is more standard than error per *pixel*
	int numPixels = w*h;
	//int numSamples = numPixels*channels;
	sumSqr *= 1.0/numPixels;

	return sumSqr;
}

static F64 mrse_component(F32 x, F32 y)
{
	F64 diff = x - y;
	F64 denom = x*x + y*y;
	return (denom != 0.0) ? diff * diff / denom : 0.0;
}

F64 rrSurface_GetMRSE_Deprecated(const rrSurface * surf1, const rrSurface * surf2)
{
	// DON'T USE ME
	// MRSSE instead

	rrSurfaceRowCache rows1, rows2;
	rows1.StartReadC(surf1, rrPixelFormat_4_F32, 8);
	rows2.StartReadC(surf2, rrPixelFormat_4_F32, 8);

	int w = RR_MIN(surf1->width,  surf2->width);
	int h = RR_MIN(surf1->height, surf2->height);

	int c1 = rrPixelFormat_GetInfo(surf1->pixelFormat)->channels;
	int c2 = rrPixelFormat_GetInfo(surf2->pixelFormat)->channels;

	int channels = RR_MIN(c1,c2); // ?
	RR_ASSERT( channels > 0 );
	if ( channels == 0 ) return 0.0;

	F64 sum = 0.0;

	for(int y=0;y<h;y++)
	{
		const rrColor4F * row1 = rows1.GetRow_4F(y);
		const rrColor4F * row2 = rows2.GetRow_4F(y);

		switch(channels)
		{
		case 1:
			for (int x = 0; x < w; ++x)
			{
				sum += mrse_component(row1[x].r, row2[x].r);
			}
			break;

		case 2:
			for (int x = 0; x < w; ++x)
			{
				sum += mrse_component(row1[x].r, row2[x].r);
				sum += mrse_component(row1[x].g, row2[x].g);
			}
			break;

		case 3:
			for (int x = 0; x < w; ++x)
			{
				sum += mrse_component(row1[x].r, row2[x].r);
				sum += mrse_component(row1[x].g, row2[x].g);
				sum += mrse_component(row1[x].b, row2[x].b);
			}
			break;

		default:
		case 4:
			for (int x = 0; x < w; ++x)
			{
				sum += mrse_component(row1[x].r, row2[x].r);
				sum += mrse_component(row1[x].g, row2[x].g);
				sum += mrse_component(row1[x].b, row2[x].b);
				sum += mrse_component(row1[x].a, row2[x].a);
			}
			break;
		}
	}

	// MRSE is definitely defined to average over samples not pixels, so do that
	return sum / ((F64)w * (F64)h * (F64)channels);
}

struct MRSSEAccumulator
{
	F64 num;
	F64 denom;

	MRSSEAccumulator &init()
	{
		num = denom = 0.0;
		return *this;
	}

	MRSSEAccumulator &accum(F32 x, F32 y)
	{
		F64 xd = x;
		F64 yd = y;
		num += (xd - yd) * (xd - yd);
		denom += xd*xd + yd*yd;
		return *this;
	}

	MRSSEAccumulator &accum_weighted(F32 x, F32 y, F32 w)
	{
		F64 xd = x;
		F64 yd = y;
		num += (xd - yd) * (xd - yd) * w;
		denom += w * (xd*xd + yd*yd);
		return *this;
	}

	F64 finish() const
	{
		#if 0
		// bias denom
		return num / (denom + RR_FP16_TINIEST_NORMAL);
		#else
		// if denom == 0 , numerator must be too
		return (denom != 0.0) ? num / denom : 0.0;
		#endif
	}
};

/*
*
* MRSSE = |x - y|^2 / ( |x|^2 + |y|^2 )
* 
* denominator sum of all components
* so a large value in any one component
* will mask small differences in other components
* 
* 
*/
F64 rrSurface_GetMRSSE(const rrSurface * surf1, const rrSurface * surf2)
{
	rrSurfaceRowCache rows1, rows2;
	rows1.StartReadC(surf1, rrPixelFormat_4_F32, 8);
	rows2.StartReadC(surf2, rrPixelFormat_4_F32, 8);

	int w = RR_MIN(surf1->width,  surf2->width);
	int h = RR_MIN(surf1->height, surf2->height);

	int c1 = rrPixelFormat_GetInfo(surf1->pixelFormat)->channels;
	int c2 = rrPixelFormat_GetInfo(surf2->pixelFormat)->channels;

	int channels = RR_MIN(c1,c2); // ?
	RR_ASSERT( channels > 0 );
	if ( channels == 0 ) return 0.0;

	F64 sum = 0.0;

	for(int y=0;y<h;y++)
	{
		const rrColor4F * row1 = rows1.GetRow_4F(y);
		const rrColor4F * row2 = rows2.GetRow_4F(y);
		MRSSEAccumulator acc;

		switch(channels)
		{
		case 1:
			for (int x = 0; x < w; ++x)
			{
				sum += acc.init().accum(row1[x].r, row2[x].r).finish();
			}
			break;

		case 2:
			for (int x = 0; x < w; ++x)
			{
				sum += acc.init()
					.accum(row1[x].r, row2[x].r)
					.accum(row1[x].g, row2[x].g)
					.finish();
			}
			break;

		case 3:
			for (int x = 0; x < w; ++x)
			{
				sum += acc.init()
					.accum(row1[x].r, row2[x].r)
					.accum(row1[x].g, row2[x].g)
					.accum(row1[x].b, row2[x].b)
					.finish();
			}
			break;

		default:
		case 4:
			for (int x = 0; x < w; ++x)
			{
				sum += acc.init()
					.accum(row1[x].r, row2[x].r)
					.accum(row1[x].g, row2[x].g)
					.accum(row1[x].b, row2[x].b)
					.accum(row1[x].a, row2[x].a)
					.finish();
			}
			break;
		}
	}

	return sum / ((F64)w * (F64)h);
}


/**
* rrSurface_GetMRSSE_Neighborhood
* 
* same as MRSSE
* MRSSE = |x - y|^2 / ( |x|^2 + |y|^2 )
* 
* but instead of per-pixel magnitude for denominator
* use the local neighborhood average magnitude
* 
* standard MRSSE rates 0.1 -> 0.2 the same as 1000 -> 2000
* MRSSE_Neighborhood will rate those the same if they are far apart
*   but not if they are nearby
* essentially large values mask small differences in the nearby neighborhood
* 
*/
F64 rrSurface_GetMRSSE_Neighborhood(const rrSurface * surf1, const rrSurface * surf2,int max_channels_to_diff /* =3 */)
{
	rrSurfaceRowCache rows1, rows2;
	rows1.StartReadC(surf1, rrPixelFormat_4_F32, 8);
	rows2.StartReadC(surf2, rrPixelFormat_4_F32, 8);

	int w = RR_MIN(surf1->width,  surf2->width);
	int h = RR_MIN(surf1->height, surf2->height);

	int c1 = rrPixelFormat_GetInfo(surf1->pixelFormat)->channels;
	int c2 = rrPixelFormat_GetInfo(surf2->pixelFormat)->channels;

	int channels = RR_MIN(c1,c2); // ?
	RR_ASSERT( channels > 0 );
	if ( channels == 0 ) return 0.0;

	// max_channels_to_diff = 3 because BC6 doesn't do A
	//	if source has data in A we'll just see huge diffs
	RR_ASSERT( max_channels_to_diff > 0 );
	channels = RR_MIN(channels,max_channels_to_diff);

	// make magnitude_map of (x^2 + y^2) per pixel

	rrSurfaceObj magnitude_map;
	rrSurface_Alloc(&magnitude_map,w,h,rrPixelFormat_1_F32);

	for LOOP(y,h)
	{
		const rrColor4F * row1 = rows1.GetRow_4F(y);
		const rrColor4F * row2 = rows2.GetRow_4F(y);
		F32 * mag_row = (F32 *) rrSurface_Seek(&magnitude_map,0,y);

		for LOOP(x,w)
		{
			F32 mag =0.f;
			switch(channels)
			{
			case 4:
				mag += fsquare( row1[x].a ) + fsquare( row2[x].a );
			case 3:
				mag += fsquare( row1[x].b ) + fsquare( row2[x].b );
			case 2:
				mag += fsquare( row1[x].g ) + fsquare( row2[x].g );
			case 1:
				mag += fsquare( row1[x].r ) + fsquare( row2[x].r );
				break;
			}
			mag_row[x] = mag;
		}
	}

	// blur magnitude_map to make it local neighborhood map :

	#if 1
	// @@ ?? TWEAK ME ?
	//double gaussian_sigma = 2.0;
	double gaussian_sigma = 4.0;

	// if you don't do this Gaussian blur , result should be exactly the same as standard MRSSE
	// 	   using per-pixel magnitude
	rrSurface_MakeGaussianBlurred(&magnitude_map,magnitude_map,gaussian_sigma);
	#endif
	
	//rrSurface_SaveByName(&magnitude_map,"r:\\magmap.exr");

	F64 sum = 0.0;

	for LOOP(y,h)
	{
		const rrColor4F * row1 = rows1.GetRow_4F(y);
		const rrColor4F * row2 = rows2.GetRow_4F(y);
		const F32 * mag_row = (const F32 *) rrSurface_SeekC(&magnitude_map,0,y);

		for LOOP(x,w)
		{
			// N-channel SSD :
			F64 dsqr =0.f;
			switch(channels)
			{
			case 4:
				dsqr += fsquare( (F64) row1[x].a - row2[x].a );
			case 3:
				dsqr += fsquare( (F64) row1[x].b - row2[x].b );
			case 2:
				dsqr += fsquare( (F64) row1[x].g - row2[x].g );
			case 1:
				dsqr += fsquare( (F64) row1[x].r - row2[x].r );
				break;
			}

			// divide my local neighborhood magnitude denominator :
			F64 mag = mag_row[x];

			#if 1
			
			// this makes quite a big difference on some images :
			sum += dsqr / (mag + RR_FP16_TINIEST_NORMAL);
			
			#else

			// matches old MRSSE exactly :
			if ( mag != 0.0 )
				sum += dsqr / mag;

			#endif
		}
	}
	
	return sum / ((F64)w * h);
}

F64 rrSurface_GetWeightedMRSSE(const rrSurface * surf1, const rrSurface * surf2, const F32 weights[4])
{
	rrSurfaceRowCache rows1, rows2;
	rows1.StartReadC(surf1, rrPixelFormat_4_F32, 8);
	rows2.StartReadC(surf2, rrPixelFormat_4_F32, 8);

	int w = RR_MIN(surf1->width,  surf2->width);
	int h = RR_MIN(surf1->height, surf2->height);

	int c1 = rrPixelFormat_GetInfo(surf1->pixelFormat)->channels;
	int c2 = rrPixelFormat_GetInfo(surf2->pixelFormat)->channels;

	int channels = RR_MIN(c1,c2); // ?
	RR_ASSERT( channels > 0 );
	if ( channels == 0 ) return 0.0;

	F64 sum = 0.0;

	for(int y=0;y<h;y++)
	{
		const rrColor4F * row1 = rows1.GetRow_4F(y);
		const rrColor4F * row2 = rows2.GetRow_4F(y);
		MRSSEAccumulator acc;

		switch(channels)
		{
		case 1:
			for (int x = 0; x < w; ++x)
			{
				sum += acc.init().accum_weighted(row1[x].r, row2[x].r, weights[0]).finish();
			}
			break;

		case 2:
			for (int x = 0; x < w; ++x)
			{
				sum += acc.init()
					.accum_weighted(row1[x].r, row2[x].r, weights[0])
					.accum_weighted(row1[x].g, row2[x].g, weights[1])
					.finish();
			}
			break;

		case 3:
			for (int x = 0; x < w; ++x)
			{
				sum += acc.init()
					.accum_weighted(row1[x].r, row2[x].r, weights[0])
					.accum_weighted(row1[x].g, row2[x].g, weights[1])
					.accum_weighted(row1[x].b, row2[x].b, weights[2])
					.finish();
			}
			break;

		default:
		case 4:
			for (int x = 0; x < w; ++x)
			{
				sum += acc.init()
					.accum_weighted(row1[x].r, row2[x].r, weights[0])
					.accum_weighted(row1[x].g, row2[x].g, weights[1])
					.accum_weighted(row1[x].b, row2[x].b, weights[2])
					.accum_weighted(row1[x].a, row2[x].a, weights[3])
					.finish();
			}
			break;
		}
	}

	return sum / ((F64)w * (F64)h);
}

// natural log :
//	(can't use clib log cuz of linux)
static RADINLINE F32 fastloge(F32 x)
{
	// need to cast to into to use log2tabled_bk
	// gives us a limit on max input at 2^16 above or below 1.0
	if ( x >= 32768.f || x <= (1.f/32768) )
	{
		return (F32) rr_loge(x);
	}

	// @@ could use rrlog2_bk_U64
	U32 xi = (U32)(x * 65536.0);
	S32 t = log2tabled_bk<32>(xi); // not log2tabled_bk_32 that does the table for low values
	F32 f = t * (1.0f / RR_LOG2TABLE_ONE);
	f = 16.f - f;
	F32 ret = f * RR_LN2f;
	
	//F64 check = rrlog2_F64(x) * RR_LN2;
	//RR_ASSERT( fequal(ret,(F32)check,0.1f) );

	return ret;
}

// "LinLog" is linear for low values (|x| < 2) then log for x >= 1
//	 value and C1 derivative continuous at the transition
static RADINLINE F32 LinLog(F32 x)
{
	if ( x < -1.0f )
	{
		return -1.0f - fastloge(-x);
	}
	else if ( x > 1.0f )
	{
		return 1.0f + fastloge(x);
	}
	else
	{
		// x in [-1,1]
		return x;
	}
}

F32 rrSurface_DoLinLog_Exposure_GetMedian(const rrSurface * surf)
{
	rrSurfaceRowCache rows;
	rows.Start_ReadC_4F_8(surf);
	
	int w = surf->width;
	int h = surf->height;
	
	vector<F32> vals;
	//vals.reserve(w*h*3);
	vals.reserve(w*h);
	
	for(int y=0;y<h;y++)
	{
		const rrColor4F * row = rows.GetRow_4F(y);

		for(int x=0;x<w;x++)
		{
			const rrColor4F & A = row[x];
			
			/*
			RR_ASSERT( ! rr_isnaninf_f(A.r) );
			RR_ASSERT( ! rr_isnaninf_f(A.g) );
			RR_ASSERT( ! rr_isnaninf_f(A.b) );
			*/
			if ( rr_isnaninf_f(A.r) || rr_isnaninf_f(A.g) || rr_isnaninf_f(A.b) )
			{
				return 0.f;
			}

			float L = 0.2958f * A.r + 0.6231f * A.g + 0.0811f * A.b;
			
			// skip zeros :
			if ( L == 0.f ) continue;

			vals.push_back( RR_ABS(L) );

			/*
			vals.push_back( RR_ABS(A.r) );
			vals.push_back( RR_ABS(A.g) );
			vals.push_back( RR_ABS(A.b) );
			// no .a
			*/
		}
	}

	if ( vals.empty() ) // all zero !
	{
		return 0.f;
	}

	stdsort(vals.begin(),vals.end());

	// median is not really what you want
	//  you want the median of the middle peak
	// eg. if the image is dark in 90% of the values
	//	you want to just ignore all that for exposure
	//	and take the remaining 10% and find the median of that
	
	// (median of the non-zero values)

	F32 median = vals[ vals.size()/2 ];
	//F64 median = vals[ vals.size()*3/4 ];
	
	#if 0
	{
	rrprintf("low (non-zero): %.5f\n", vals[0] );
	rrprintf("median        : %.5f\n", median );
	rrprintf("high          : %.5f\n", vals[ vals.size()-1 ] );
	}
	#endif

	return median;
}

rrbool rrSurface_BlitF32toF16_Clamped(rrSurface * to_surf,const rrSurface *fm_surf)
{
	rrSurfaceRowCache to_rows;
	to_rows.Start(to_surf,rrPixelFormat_4_F16,8,RR_SURFACE_ROW_CACHE_WRITE);

	rrSurfaceRowCache fm_rows;
	fm_rows.Start_ReadC_4F_8(fm_surf);
	
	int w = fm_surf->width;
	int h = fm_surf->height;
	
	rrbool out_of_range_detected = false;

	for LOOP(y,h)
	{
		U16 * to_row = (U16 *) to_rows.GetRow(y);
		const float * fm_row = (const float *) fm_rows.GetRow(y);

		for LOOP(x,w*4)
		{
			float fm_val = fm_row[x];
			if ( fm_val < -RR_FP16_MAX || fm_val > RR_FP16_MAX )
			{
				out_of_range_detected = true;
				fm_val = (fm_val < 0) ? -(float)RR_FP16_MAX : (float)RR_FP16_MAX;
			}
			rrFP16 fp;
			rrFP16_SetFloat(&fp,fm_val);
			to_row[x] = fp.bits;
		}
	}
	
	return out_of_range_detected;
}

// rrSurface_Reinhard_Unreal_Tonemap
// output values are in U8 scale [0,255]
// and already in sRGB , do NOT gamma correct
// just Blit_NonNormalized to U8
void rrSurface_Reinhard_Unreal_Tonemap(rrSurface * surf, F32 median, F32 exposure_multiplier)
{
	rrSurfaceRowCache rows;
	rows.Start_4F_8(surf,RR_SURFACE_ROW_CACHE_READWRITE);

	int w = surf->width;
	int h = surf->height;
	
	/**
	
	from:

	float Tonemap_Unreal(float x) {
		// Unreal 3, Documentation: "Color Grading"
		// Adapted to be close to Tonemap_ACES, with similar range
		// Gamma 2.2 correction is baked in, don't use with sRGB conversion!
		return x / (x + 0.155) * 1.019;
	}

	x / (x+c) exposure with c = median (scaled by exposure)

	*/

	RR_ASSERT( median != 0.f );
	F32 exposure = median*2;
	// median -> 1/3
	
	// higher exposure_multiplier = brighter
	exposure /= exposure_multiplier;
	
	for(int y=0;y<h;y++)
	{
		rrColor4F * row = rows.GetRow_4F(y);

		for(int x=0;x<w;x++)
		{
			rrColor4F & c = row[x];
			
			// no A
			
			// 260 ~ 255 * 1.019

			c.r = RR_CLAMP( 260.f * c.r / (c.r + exposure ) ,0.f,255.f);
			c.g = RR_CLAMP( 260.f * c.g / (c.g + exposure ) ,0.f,255.f);
			c.b = RR_CLAMP( 260.f * c.b / (c.b + exposure ) ,0.f,255.f);
		}
	}

	rows.FlushWrite();
}

// rrSurface_DoLinLog_Exposure outputs surf in unit float scale and not yet gamma corrected
//	 feed to rrSurface_Gamma_SRGB_UnitLinearFloat_To_255Int
void rrSurface_DoLinLog_Exposure(rrSurface * surf, F32 median, F32 exposure_multiplier)
{
	rrSurfaceRowCache rows;
	rows.Start_4F_8(surf,RR_SURFACE_ROW_CACHE_READWRITE);

	int w = surf->width;
	int h = surf->height;
	
	RR_ASSERT( median != 0.f );
	F32 exposure = median*2;
	
	// higher exposure_multiplier = brighter
	exposure /= exposure_multiplier;
	
	F32 invexposure = 1.0f/exposure;

	invexposure *= 0.5;

	for(int y=0;y<h;y++)
	{
		rrColor4F * row = rows.GetRow_4F(y);

		for(int x=0;x<w;x++)
		{
			rrColor4F & c = row[x];
			
			// scale down by exposure before LinLogs
			// this acts to change the linear/log transition point
			
			// LinLog outputs the linear range in [0,2]
			//	it maps the median to 1
			// we want the linear range [0,2] to be the majority of the 256 output
			//	leave just a tiny bit near white for the log range above
			// doing *128 here would map the whole 256 output from the linear portion
			//	  instead do a little bit less

			// no A
			
			#if 1
			
			// much slower, but better exposure on nv/1.hdr :
			
			c.r = (F32)( LinLog(c.r*invexposure) * 100.f/256.f );
			c.g = (F32)( LinLog(c.g*invexposure) * 100.f/256.f );
			c.b = (F32)( LinLog(c.b*invexposure) * 100.f/256.f );
			
			#endif

			#if 0
			// exposure only on max :		
			// causes major RGB color fringes on nv/1 :
			
			F32 m = RR_MAX3(c.r,c.g,c.b);
			if ( m == 0.f ) continue;
			F32 l = (F32) LinLog(m*invexposure);
			F32 scale = (l/m) * (100.f/256.f);
			c.r *= scale;
			c.g *= scale;
			c.b *= scale;
			
			#endif
			
		}
	}

	rows.FlushWrite();
	
	// output is unit float scaled
	// intend to feed to rrSurface_Gamma_SRGB_UnitLinearFloat_To_255Int
}

/*
F64 rrSurface_MSE_to_PSNR(F64 mse)
{
	// 10*log(256*256) = 48.16480
	// 10*log(255*255) = 48.13080
	
	// if mse is 0 we return inf
	
	// @@ ?? should mse be per sample or per pixel ?
	// in the literature per sample is more standard
	
	const F64 max_psnr = (F64) 99.9999;
	
	// 10^(- 9.9 + 4.816480) = (mse)
	if ( mse <= 0.0000080 )
	{
		return max_psnr;
	}

	F64 psnr = ( 48.16480 - 10.0*log10(mse));
	
	return RR_MIN(psnr,max_psnr);
}
*/

/*
static void  rrColor4F_AddScaled(rrColor4F * pTo, const rrColor4F &c, F32 scale)
{
	pTo->r += c.r * scale;
	pTo->g += c.g * scale;
	pTo->b += c.b * scale;
	pTo->a += c.a * scale;
}
*/

static inline const rrColor4F Average4(const rrColor4F &c1, const rrColor4F &c2, const rrColor4F &c3, const rrColor4F & c4)
{
	rrColor4F ret;
	ret.r = ( c1.r + c2.r + c3.r + c4.r ) * 0.25f;
	ret.g = ( c1.g + c2.g + c3.g + c4.g ) * 0.25f;
	ret.b = ( c1.b + c2.b + c3.b + c4.b ) * 0.25f;
	ret.a = ( c1.a + c2.a + c3.a + c4.a ) * 0.25f;
	return ret;
}

// c1 is the nearest, c4 is the farthest :
static inline const rrColor4F Bilinear_Up4(const rrColor4F &c1, const rrColor4F &c2, const rrColor4F &c3, const rrColor4F & c4)
{
	rrColor4F ret;
	ret.r = ( 9.f*c1.r + 3.f*(c2.r + c3.r) + c4.r ) * (1.f/16);
	ret.g = ( 9.f*c1.g + 3.f*(c2.g + c3.g) + c4.g ) * (1.f/16);
	ret.b = ( 9.f*c1.b + 3.f*(c2.b + c3.b) + c4.b ) * (1.f/16);
	ret.a = ( 9.f*c1.a + 3.f*(c2.a + c3.a) + c4.a ) * (1.f/16);
	return ret;
}

// c1 is the nearest, c4 is the farthest :
static inline const rrColor4F Bilinear_Up2(const rrColor4F &c1, const rrColor4F &c4)
{
	rrColor4F ret;
	ret.r = ( 3.f*c1.r + c4.r ) * (1.f/4);
	ret.g = ( 3.f*c1.g + c4.g ) * (1.f/4);
	ret.b = ( 3.f*c1.b + c4.b ) * (1.f/4);
	ret.a = ( 3.f*c1.a + c4.a ) * (1.f/4);
	return ret;
}


// Down/Up simple zoom :
//	note these do not gamma correct or whatever for you, you must do it :
void rrSurface_BoxDownSample2X(rrSurface * to,const rrSurface * from)
{
	rrSurfaceRowCache fmRows;
	fmRows.Start_4F_8(const_cast<rrSurface *>(from),RR_SURFACE_ROW_CACHE_READ,2);
	
	rrSurfaceRowCache toRows;
	toRows.Start_4F_8(to,RR_SURFACE_ROW_CACHE_WRITE);
	
	int h = RR_MIN( from->height/2, to->height );
	int w = RR_MIN( from->width/2 , to->width );
	
	for(int y=0;y<h;y++)
	{
		fmRows.MoveCache(2*y,2);
		const rrColor4F * fmRow0 = fmRows.GetRow_4F(2*y+0);
		const rrColor4F * fmRow1 = fmRows.GetRow_4F(2*y+1);
		rrColor4F * toRow = toRows.GetRow_4F(y);
		
		for(int x=0;x<w;x++)
		{
			toRow[x] = Average4(	fmRow0[2*x+0], fmRow0[2*x+1], 
									fmRow1[2*x+0], fmRow1[2*x+1] );
		}
	}
}

// Down/Up simple zoom :
//	note these do not gamma correct or whatever for you, you must do it :
void rrSurface_BoxUpSample2X(rrSurface * to,const rrSurface * from)
{
	rrSurfaceRowCache fmRows;
	fmRows.Start_4F_8(const_cast<rrSurface *>(from),RR_SURFACE_ROW_CACHE_READ);
	
	rrSurfaceRowCache toRows;
	toRows.Start_4F_8(to,RR_SURFACE_ROW_CACHE_WRITE);
	
	int h = RR_MIN( from->height, to->height/2 );
	int w = RR_MIN( from->width , to->width/2 );
	
	for(int y=0;y<h;y++)
	{
		toRows.MoveCache(2*y,2);
		rrColor4F * toRow0 = toRows.GetRow_4F(2*y+0);
		rrColor4F * toRow1 = toRows.GetRow_4F(2*y+1);
		const rrColor4F * fmRow = fmRows.GetRow_4F(y);
		
		for(int x=0;x<w;x++)
		{
			toRow0[2*x+0] = fmRow[x];
			toRow0[2*x+1] = fmRow[x];
			toRow1[2*x+0] = fmRow[x];
			toRow1[2*x+1] = fmRow[x];
		}
	}
}

void rrSurface_BilinearUpSample2X(rrSurface * to,const rrSurface * from)
{
	rrSurfaceRowCache fmRows;
	fmRows.Start_4F_8(const_cast<rrSurface *>(from),RR_SURFACE_ROW_CACHE_READ);
	
	rrSurfaceRowCache toRows;
	toRows.Start_4F_8(to,RR_SURFACE_ROW_CACHE_WRITE);
	
	int h = RR_MIN( from->height, to->height/2 );
	int w = RR_MIN( from->width , to->width/2 );
	
	// Bilinear "box" upsample
	
	// @@ UpSample for CLAMP edge convention :
	// should do a different one for WRAP
	
	// do the inner meat :
	for(int y=0;y<h;y++)
	{
		if ( y == 0 )
		{
			// do top row :
			const rrColor4F * fmRow = fmRows.GetRow_4F(0);
			rrColor4F * toRow = toRows.GetRow_4F(0);
			
			toRow[0] = fmRow[0];
			
			for(int x=0;x<(w-1);x++)
			{
				toRow[2*x+1] = Bilinear_Up2(fmRow[x],fmRow[x+1]);
				toRow[2*x+2] = Bilinear_Up2(fmRow[x+1],fmRow[x]);
			}
			
			toRow[2*w-1] = fmRow[w-1];
		}
		
		if ( y < (h-1) )
		{
			// must move cache manually cuz we need two rows :
			fmRows.MoveCache(y,2);
			toRows.MoveCache(2*y+1,2);
			
			const rrColor4F * fmRowN = fmRows.GetRow_4F(y);
			const rrColor4F * fmRowS = fmRows.GetRow_4F(y+1);
			rrColor4F * toRow0 = toRows.GetRow_4F(2*y+1);
			rrColor4F * toRow1 = toRows.GetRow_4F(2*y+2);
		
			// do first column custom :
			toRow0[0] = Bilinear_Up2(fmRowN[0],fmRowS[0]);
			toRow1[0] = Bilinear_Up2(fmRowS[0],fmRowN[0]);
				
			// do inner meat where we have all the neighbors :
			for(int x=0;x<w-1;x++)
			{
				const rrColor4F & NW = fmRowN[x];
				const rrColor4F & NE = fmRowN[x+1];
				const rrColor4F & SW = fmRowS[x];
				const rrColor4F & SE = fmRowS[x+1];
				
				// four low res pixels make 4 interior high res pixels between them,
				//	which just different {9,3,3,1} filter orientations  
				toRow0[2*x+1] = Bilinear_Up4(NW,NE,SW,SE);
				toRow0[2*x+2] = Bilinear_Up4(NE,NW,SE,SW);
				toRow1[2*x+1] = Bilinear_Up4(SW,SE,NW,NE);
				toRow1[2*x+2] = Bilinear_Up4(SE,SW,NE,NW);
			}
			
			// last column :
			toRow0[2*w-1] = Bilinear_Up2(fmRowN[w-1],fmRowS[w-1]);
			toRow1[2*w-1] = Bilinear_Up2(fmRowS[w-1],fmRowN[w-1]);
		}
		
		if ( y == h-1 )
		{
			// do the last row :
			
			const rrColor4F * fmRow = fmRows.GetRow_4F(h-1);
			rrColor4F * toRow = toRows.GetRow_4F(2*h-1);
			
			toRow[0] = fmRow[0];
			
			for(int x=0;x<(w-1);x++)
			{
				toRow[2*x+1] = Bilinear_Up2(fmRow[x],fmRow[x+1]);
				toRow[2*x+2] = Bilinear_Up2(fmRow[x+1],fmRow[x]);
			}
			
			toRow[2*w-1] = fmRow[w-1];
		}
	}
}

//=======================================================================================

rrbool rrSurface_ForceOrDetectAlpha(rrSurface * surf, int alpha_mode, rrbool verbose_logs)
{
	const rrPixelFormatInfo * surf_fm_info = rrPixelFormat_GetInfo(surf->pixelFormat);
	if ( surf_fm_info->bytesPerPixel == 0 )
	{
		ooLogError("rrSurface_ForceOrDetectAlpha : not a scalar surface\n");
		return true;
	}

	if ( alpha_mode == 0 )
	{
		// force alpha off :
		if ( surf_fm_info->channels == 4 )
		{
			// discard alpha !
			if ( verbose_logs )
				rrprintf("alpha mode 0 , RGBA source : discarding alpha.\n");
			rrSurface_ChangeNumberOfChannels(surf,3);
		}
	}
	else if ( alpha_mode == 1 )
	{
		// force alpha on :
		rrSurface_ChangeNumberOfChannels(surf,4);
	}
	else
	{
		// alpha_mode = auto

		if ( surf_fm_info->channels == 4 )
		{
			if ( rrSurface_Is4ChannelWithAllAlphasOpaque(surf) )
			{
				if ( verbose_logs )
					rrprintf("auto alpha : RGBA with all Opaque : no alpha\n");
				rrSurface_ChangeNumberOfChannels(surf,3);
				alpha_mode = 0;
			}
			else
			{	
				if ( verbose_logs )
					rrprintf("auto alpha : yes alpha\n");
				alpha_mode = 1;
			}
		}
		else
		{
			if ( verbose_logs )
				rrprintf("auto alpha : no alpha\n");
			alpha_mode = 0;
		}
	}
	// alpha_mode is now a bool
	return alpha_mode;
}

bool rrSurface_HasNonIdentityAlpha(const rrSurface * surf)
{
	const rrPixelFormatInfo * info = rrPixelFormat_GetInfo(surf->pixelFormat);
		
	if ( info->bytesPerPixel == 0 )
	{
		// block compressed, can't tell
		RR_ASSERT(false);
		return false;
	}

	if ( info->channels != 4 ) // < 4 channels
		return false;
		
	if ( rrSurface_Is4ChannelWithAllAlphasOpaque(surf) )
		return false;
		
	return true;
}

bool rrSurface_ChangeNumberOfChannels(rrSurface * surf,int nchan)
{
	const rrPixelFormatInfo * info = rrPixelFormat_GetInfo(surf->pixelFormat);
		
	if ( info->bytesPerPixel == 0 )
	{
		// block compressed, can't tell
		RR_ASSERT(false);
		return false;
	}
	
	if ( info->channels == nchan )
		return true;

	rrPixelFormat newpf = RR_MAKE_PIXELFORMAT_SIMPLE( nchan, RR_PIXELFORMAT_GET_TYPE(surf->pixelFormat) );

	return rrSurface_ChangeFormatNormalized(surf,newpf);
}

bool rrSurface_Is4ChannelWithAllAlphasOpaque(const rrSurface * surf)
{
	const rrPixelFormatInfo * info = rrPixelFormat_GetInfo(surf->pixelFormat);
		
	if ( info->bytesPerPixel == 0 )
	{
		// block compressed, can't tell
		RR_ASSERT(false);
		return false;
	}

	if ( info->channels != 4 ) // < 4 channels
		return false;		

	// yes it's 4 channel signal data
	
	if ( info->isFloat )
	{
		rrSurfaceRowCache rows;
		rows.Start_4F_8(const_cast<rrSurface *>(surf),RR_SURFACE_ROW_CACHE_READ);

		// can be signed or unsigned floats
		// opaqueA is 1.f

		for( int y = 0 ; y < surf->height ; y ++ )
		{
			const rrColor4F * row = rows.GetRow_4F(y);

			for( int x = 0; x<surf->width;x++)
			{
				F32 a = row[x].a;

				// @@ fuzzy tolerance?
				#if 0

				// in F16 the numbers closest to one are
				//	2047/2048 and 1025/1024
				//	so half way between those and 1.0 is the threshold
				//	for the quantization bucket that will round to 1.0
				//	if converted to F16 with round-nearest
				if ( a >= 4095/4096.f && a <= 2049/2048.f )
				{
					// if you converted to F16 with round-nearest
					//	these values would all go to 1.f
					return false;
				}

				#else

				if ( a != 1.0f )
					return false;

				#endif
			}
		}
	}
	else
	{	
		S32 opaqueA = rrPixelFormat_GetNormalizedScale(surf->pixelFormat);

		rrSurfaceRowCache rows;
		rows.Start_4I_8(const_cast<rrSurface *>(surf),RR_SURFACE_ROW_CACHE_READ);

		for( int y = 0 ; y < surf->height ; y ++ )
		{
			const rrColor4I * row = rows.GetRow_4I(y);

			for( int x = 0; x<surf->width;x++)
			{
				int a = row[x].a;
				RR_ASSERT( a <= opaqueA ); //  should be impossible to go over opaqueA
				if ( a != opaqueA )
					return false;
			}
		}
	}
	
	return true;
}


bool rrSurface_ForceAllAlphasOpaqueIf4Channel(rrSurface * surf)
{
	const rrPixelFormatInfo * info = rrPixelFormat_GetInfo(surf->pixelFormat);
		
	if ( info->bytesPerPixel == 0 )
	{
		// block compressed, can't tell
		return false;
	}

	if ( info->channels != 4 ) // < 4 channels
		return false;		
	
	// yes it's 4 channel signal data
	
	if ( info->isFloat )
	{
		rrSurfaceRowCache rows;
		rows.Start_4F_8(surf,RR_SURFACE_ROW_CACHE_READWRITE);

		for( int y = 0 ; y < surf->height ; y ++ )
		{
			rrColor4F * row = rows.GetRow_4F(y);

			for( int x = 0; x<surf->width;x++)
			{
				row[x].a = 1.f;
			}
		}
	}
	else
	{
		S32 opaqueA = rrPixelFormat_GetNormalizedScale(surf->pixelFormat);

		rrSurfaceRowCache rows;
		rows.Start_4I_8(surf,RR_SURFACE_ROW_CACHE_READWRITE);

		for( int y = 0 ; y < surf->height ; y ++ )
		{
			rrColor4I * row = rows.GetRow_4I(y);

			for( int x = 0; x<surf->width;x++)
			{
				row[x].a = opaqueA;
			}
		}
	}
	
	return true;
}

void rrSurface_GetValueRange(const rrSurface * surf, rrColor4F * pLo , rrColor4F * pHi )
{
	rrSurfaceRowCache rows;
	rows.Start_4F_8(const_cast<rrSurface *>(surf),RR_SURFACE_ROW_CACHE_READ);

	rrColor4F lo,hi;
	
	lo.r = lo.g = lo.b = lo.a = 99999999999999999999.f;
	hi.r = hi.g = hi.b = hi.a = -99999999999999999999.f;
	
	for( int y = 0 ; y < surf->height ; y ++ )
	{
		const rrColor4F * row = rows.GetRow_4F(y);

		for( int x = 0; x<surf->width;x++)
		{
			lo.r = RR_MIN(lo.r, row[x].r );
			lo.g = RR_MIN(lo.g, row[x].g );
			lo.b = RR_MIN(lo.b, row[x].b );
			lo.a = RR_MIN(lo.a, row[x].a );
			
			hi.r = RR_MAX(hi.r, row[x].r );
			hi.g = RR_MAX(hi.g, row[x].g );
			hi.b = RR_MAX(hi.b, row[x].b );
			hi.a = RR_MAX(hi.a, row[x].a );
		}
	}
	
	*pLo = lo;
	*pHi = hi;
}

static void mult_add_kernel(rrColor4F * dest, const rrColor4F * src, const rrColor4F & in_mult, const rrColor4F & in_add, SINTa count)
{
#ifdef __RADSSE2__
	VecF32x4 multiplier = VecF32x4::loadu(&in_mult.r);
	VecF32x4 adder = VecF32x4::loadu(&in_add.r);
	for (SINTa x = 0; x < count; ++x)
	{
		VecF32x4 val = VecF32x4::loadu(&src[x].r) * multiplier + adder;
		val.storeu(&dest[x].r);
	}
#else
	const rrColor4F multiplier = in_mult;
	const rrColor4F adder = in_add;

	for (SINTa x = 0; x < count; ++x)
	{
		dest[x].r = src[x].r * multiplier.r + adder.r;
		dest[x].g = src[x].g * multiplier.g + adder.g;
		dest[x].b = src[x].b * multiplier.b + adder.b;
		dest[x].a = src[x].a * multiplier.a + adder.a;
	}
#endif
}

void rrSurface_MultAdd(rrSurface * surf, const rrColor4F & in_multiplier, const rrColor4F & in_adder )
{
	rrSurfaceRowCache rows;
	rows.Start_4F_8(const_cast<rrSurface *>(surf),RR_SURFACE_ROW_CACHE_READWRITE);

	const rrColor4F multiplier = in_multiplier;
	const rrColor4F adder = in_adder;

	for( int y = 0 ; y < surf->height ; y ++ )
	{
		rrColor4F * row = rows.GetRow_4F(y);
		mult_add_kernel(row, row, in_multiplier, in_adder, surf->width);
	}
}

void rrSurface_MultAddF(rrSurface * surf, const float multiplier, const float adder )
{
	rrColor4F mult;
	rrColor4F add;

	mult.r = mult.g = mult.b = mult.a = multiplier;
	add.r = add.g = add.b = add.a = adder;
	rrSurface_MultAdd(surf, mult, add);
}

void rrSurface_MultAddF(rrSurface * dest_surf, const rrSurface * src_surf, const float multiplier, const float adder )
{
	RR_ASSERT_ALWAYS( dest_surf->width == src_surf->width );
	RR_ASSERT_ALWAYS( dest_surf->height == src_surf->height );

	rrColor4F mult;
	rrColor4F add;

	mult.r = mult.g = mult.b = mult.a = multiplier;
	add.r = add.g = add.b = add.a = adder;

	rrSurfaceRowCache rows_rd;
	rrSurfaceRowCache rows_wr;
	rows_rd.Start_4F_8(const_cast<rrSurface *>(src_surf),RR_SURFACE_ROW_CACHE_READ);
	rows_wr.Start_4F_8(dest_surf,RR_SURFACE_ROW_CACHE_WRITE);

	for( int y = 0 ; y < src_surf->height ; y ++ )
	{
		const rrColor4F * row_rd = rows_rd.GetRow_4F(y);
		rrColor4F * row_wr = rows_wr.GetRow_4F(y);

		mult_add_kernel(row_wr, row_rd, mult, add, src_surf->width);
	}
}

rrbool rrSurface_MultAddF_ChangeFormat(rrSurface * surf, rrPixelFormat new_format, const float multiplier, const float adder )
{
	// Do the multiply + add to a scratch surface
	rrSurfaceObj temp_surf;
	if ( ! rrSurface_Alloc(&temp_surf,surf->width,surf->height,new_format) )
		return false;

	rrSurface_MultAddF(&temp_surf,surf,multiplier,adder);

	// Then swap the scratch surface with the original
	rrSurface_Swap(surf,&temp_surf);

	return true;
}

void rrSurface_AddScaled(rrSurface * dest, 
							const rrColor4F & scale_A,
							const rrSurface * pSurfA,
							const rrColor4F & scale_B,
							const rrSurface * pSurfB)
{
	rrSurfaceRowCache rowsDest;
	rowsDest.Start_4F_8(const_cast<rrSurface *>(dest),RR_SURFACE_ROW_CACHE_WRITE);
	rrSurfaceRowCache rowsA;
	rrSurfaceRowCache rowsB;
	rowsA.Start_4F_8(const_cast<rrSurface *>(pSurfA),RR_SURFACE_ROW_CACHE_READ);
	rowsB.Start_4F_8(const_cast<rrSurface *>(pSurfB),RR_SURFACE_ROW_CACHE_READ);

	const rrColor4F scaleA = scale_A;
	const rrColor4F scaleB = scale_B;

	for( int y = 0 ; y < dest->height ; y ++ )
	{
		rrColor4F * rowDest = rowsDest.GetRow_4F(y);
		const rrColor4F * rowA = rowsA.GetRow_4F(y);
		const rrColor4F * rowB = rowsB.GetRow_4F(y);

		for( int x = 0; x < dest->width;x++)
		{
			rowDest[x].r = rowA[x].r * scaleA.r + rowB[x].r * scaleB.r;
			rowDest[x].g = rowA[x].g * scaleA.g + rowB[x].g * scaleB.g;
			rowDest[x].b = rowA[x].b * scaleA.b + rowB[x].b * scaleB.b;
			rowDest[x].a = rowA[x].a * scaleA.a + rowB[x].a * scaleB.a;
		}
	}
}

void rrSurface_Clamp(rrSurface * surf, float lo, float hi)
{
	if ( rrPixelFormat_GetInfo(surf->pixelFormat)->isFloat )
	{
		rrSurfaceRowCache rows;
		rows.Start_4F_8(const_cast<rrSurface *>(surf),RR_SURFACE_ROW_CACHE_READWRITE);

		for( int y = 0 ; y < surf->height ; y ++ )
		{
			rrColor4F * row = rows.GetRow_4F(y);

			for( int x = 0; x<surf->width;x++)
			{
				row[x].r = RR_CLAMP(row[x].r, lo,hi);
				row[x].g = RR_CLAMP(row[x].g, lo,hi);
				row[x].b = RR_CLAMP(row[x].b, lo,hi);
				row[x].a = RR_CLAMP(row[x].a, lo,hi);
			}
		}
	}
	else
	{
		int ilo = rr_ftoi_round(lo);
		int ihi = rr_ftoi_round(hi);
	
		rrSurfaceRowCache rows;
		rows.Start_4I_8(const_cast<rrSurface *>(surf),RR_SURFACE_ROW_CACHE_READWRITE);

		for( int y = 0 ; y < surf->height ; y ++ )
		{
			rrColor4I * row = rows.GetRow_4I(y);

			for( int x = 0; x<surf->width;x++)
			{
				row[x].r = RR_CLAMP(row[x].r, ilo,ihi);
				row[x].g = RR_CLAMP(row[x].g, ilo,ihi);
				row[x].b = RR_CLAMP(row[x].b, ilo,ihi);
				row[x].a = RR_CLAMP(row[x].a, ilo,ihi);
			}
		}
	}
}

void rrSurface_Clamp255(rrSurface * surf)
{
	rrSurface_Clamp(surf,0.f,255.f);
}

void rrSurface_ClampSNORM(rrSurface * surf)
{
	// only signed types are candidates for representing SNORM data
	switch ( RR_PIXELFORMAT_GET_TYPE(surf->pixelFormat) )
	{
	case RR_PIXELFORMAT_TYPE_S8:
		rrSurface_Clamp(surf,-127.f,127.f);
		break;

	case RR_PIXELFORMAT_TYPE_S16:
		rrSurface_Clamp(surf,-32767.f,32767.f);
		break;
		
	RR_NO_DEFAULT_CASE
	}
}

static inline int quant_u16_to_u8(int x)
{
	// unorm16 x -> float: x / 65535.0
	// float -> unorm8: round(x * 255.0)
	// so total is
	//   floor((x / 65535.0) * 255.0 + 0.5)
	// = floor(x / 257 + 0.5)
	return (x + 128) / 257;
}

static inline int quant_s16_to_s8(int x)
{
	// snorm16 -> float: x / 32767.0
	// float -> snorm8: round(x * 127.0)
	// so total is
	//   round((x / 32767.0) * 127)
	// = round(x * 127 / 32767)
	return (x * 127 + (x > 0 ? 16384 : -16384)) / 32767;
}

rrbool rrSurface_QuantizeTo8Bits(rrSurface * dest_surf, const rrSurface * src_surf)
{
	rrSurfaceRowCache read_cache, write_cache;
	int num_chans = RR_PIXELFORMAT_GET_NUMPLANES(src_surf->pixelFormat);

	switch ( RR_PIXELFORMAT_GET_TYPE(src_surf->pixelFormat) )
	{
	case RR_PIXELFORMAT_TYPE_U16:
		if ( ! rrSurface_Alloc(dest_surf, src_surf->width, src_surf->height, (rrPixelFormat)RR_MAKE_PIXELFORMAT_SIMPLE(num_chans,RR_PIXELFORMAT_TYPE_U8)) )
			return false;

		read_cache.Start_4I_8(const_cast<rrSurface *>(src_surf), RR_SURFACE_ROW_CACHE_READ);
		write_cache.Start_4I_8(dest_surf, RR_SURFACE_ROW_CACHE_WRITE);

		for (int y = 0; y < src_surf->height; y++)
		{
			const rrColor4I * src_row = read_cache.GetRow_4I(y);
			rrColor4I * dest_row = write_cache.GetRow_4I(y);

			for (int x = 0; x < src_surf->width; x++)
			{
				dest_row[x].r = quant_u16_to_u8(src_row[x].r);
				dest_row[x].g = quant_u16_to_u8(src_row[x].g);
				dest_row[x].b = quant_u16_to_u8(src_row[x].b);
				dest_row[x].a = quant_u16_to_u8(src_row[x].a);
			}
		}
		break;

	case RR_PIXELFORMAT_TYPE_S16:
		if ( ! rrSurface_Alloc(dest_surf, src_surf->width, src_surf->height, (rrPixelFormat)RR_MAKE_PIXELFORMAT_SIMPLE(num_chans,RR_PIXELFORMAT_TYPE_S8)) )
			return false;

		read_cache.Start_4I_8(const_cast<rrSurface *>(src_surf), RR_SURFACE_ROW_CACHE_READ);
		write_cache.Start_4I_8(dest_surf, RR_SURFACE_ROW_CACHE_WRITE);

		for (int y = 0; y < src_surf->height; y++)
		{
			const rrColor4I * src_row = read_cache.GetRow_4I(y);
			rrColor4I * dest_row = write_cache.GetRow_4I(y);

			for (int x = 0; x < src_surf->width; x++)
			{
				dest_row[x].r = quant_s16_to_s8(src_row[x].r);
				dest_row[x].g = quant_s16_to_s8(src_row[x].g);
				dest_row[x].b = quant_s16_to_s8(src_row[x].b);
				dest_row[x].a = quant_s16_to_s8(src_row[x].a);
			}
		}
		break;

	default:
		return false;
	}

	return true;
}

rrbool rrSurface_CheckExtremesPreserved(const rrSurface * surf1, const rrSurface * surf2 , rrPixelFormat bcn_pf)
{
	// surf1 & 2 should be decompressed
	// bcn_pf is the BCN we were for
	
	/*
	bcn_pf is one of :
	rrPixelFormat_BC3 
	rrPixelFormat_BC4U
	rrPixelFormat_BC4S
	rrPixelFormat_BC5U
	rrPixelFormat_BC5S
	*/

	// Format types should match, else this operation isn't really well-defined
	// 3 vs 4 channel U8 is common for BC3
	int type1 = RR_PIXELFORMAT_GET_TYPE( surf1->pixelFormat );
	int type2 = RR_PIXELFORMAT_GET_TYPE( surf2->pixelFormat );
	if ( type1 != type2 )
	{
		RR_ASSERT_FAILURE_ALWAYS("CheckExtremesPreserved PF mismatch");
		return false;
	}

	// We do allow sizes to be different because of block size padding
	int w = RR_MIN( surf1->width, surf2->width );
	int h = RR_MIN( surf1->height, surf2->height );

	S32 min_val = 0, max_val = 0;
	switch ( type1 )
	{
	case RR_PIXELFORMAT_TYPE_U8:
		min_val = 0;
		max_val = 255;
		break;

	case RR_PIXELFORMAT_TYPE_S8:
		// assumes SNORM semantics, i.e. -128 and -127 mean the same thing
		min_val = -127;
		max_val = 127;
		break;

	case RR_PIXELFORMAT_TYPE_U16:
		min_val = 0;
		max_val = 65535;
		break;

	case RR_PIXELFORMAT_TYPE_S16:
		// assumes SNORM semantics, i.e. -32678 and -32767 mean the same thing
		min_val = -32767;
		max_val = 32767;
		break;

	default:
		// Other formats don't support this (for the time being)
		RR_ASSERT_FAILURE_ALWAYS("CheckExtremesPreserved bad type?");
		return false;
	}

	rrSurfaceRowCache rows1;
	rrSurfaceRowCache rows2;
	rows1.Start_4I_8(const_cast<rrSurface *>(surf1),RR_SURFACE_ROW_CACHE_READ);
	rows2.Start_4I_8(const_cast<rrSurface *>(surf2),RR_SURFACE_ROW_CACHE_READ);
	for(int y = 0; y < h; y++)
	{
		const rrColor4I * row1 = rows1.GetRow_4I(y);
		const rrColor4I * row2 = rows2.GetRow_4I(y);
		
		if ( bcn_pf == rrPixelFormat_BC3 )
		{
			// only applies to the A channel in BC3 :
			for (int x = 0; x < w; x++)
			{
				if ((row1[x].a == min_val || row1[x].a == max_val) && row1[x].a != row2[x].a)
				{
					return false;
				}
			}
		}
		else
		{
			for (int x = 0; x < w; x++)
			{
				// Extremal values must match (we always check 4 channels because the
				// padding, if applied, is consistent)
				if ((row1[x].r == min_val || row1[x].r == max_val) && row1[x].r != row2[x].r)
					return false;
				if ((row1[x].g == min_val || row1[x].g == max_val) && row1[x].g != row2[x].g)
					return false;
				if ((row1[x].b == min_val || row1[x].b == max_val) && row1[x].b != row2[x].b)
					return false;
				if ((row1[x].a == min_val || row1[x].a == max_val) && row1[x].a != row2[x].a)
					return false;
			}
		}
	}

	return true;
}

void rrSurface_ClampToRange(rrSurface * surf, const rrColor4F & inlo, const rrColor4F & inhi )
{
	rrSurfaceRowCache rows;
	rows.Start_4F_8(const_cast<rrSurface *>(surf),RR_SURFACE_ROW_CACHE_READWRITE);

	const rrColor4F lo = inlo;
	const rrColor4F hi = inhi;

	for( int y = 0 ; y < surf->height ; y ++ )
	{
		rrColor4F * row = rows.GetRow_4F(y);

		for( int x = 0; x<surf->width;x++)
		{
			row[x].r = RR_CLAMP(row[x].r, lo.r, hi.r );
			row[x].g = RR_CLAMP(row[x].g, lo.g, hi.g );
			row[x].b = RR_CLAMP(row[x].b, lo.b, hi.b );
			row[x].a = RR_CLAMP(row[x].a, lo.a, hi.a );
		}
	}
}
	
// if uniform is true, the same scale is applied to all components
//	 pScaler is filled out with the previous color range, and can be used to undo
void rrSurface_Scale_To_255(rrSurface * surf, rrSurface_Scaler * pScaler, bool uniform )
{
	rrSurface_Scaler scaler;
	if ( pScaler == NULL ) pScaler = &scaler;
	rrSurface_GetValueRange(surf,&pScaler->lo,&pScaler->hi);
	
	rrColor4F lo,hi;
	lo = pScaler->lo;
	hi = pScaler->hi;

	if ( uniform )
	{
		/*
		float biggest =
		RR_MAX( RR_MAX( hi.r - lo.r , hi.g - lo.g ) ,
				RR_MAX( hi.b - lo.b , hi.a - lo.a ) );
		hi.r = lo.r + biggest;
		hi.g = lo.g + biggest;
		hi.b = lo.b + biggest;
		hi.a = lo.a + biggest;
		*/
		
		RR_ZERO(lo);
		float biggest = RR_MAX3(hi.r,hi.g,hi.b);
		hi.r = biggest;
		hi.g = biggest;
		hi.b = biggest;
		hi.a = RR_MAX(biggest,hi.a);
	}
	
	pScaler->lo = lo;
	pScaler->hi = hi;
	
	rrColor4F scale;
	scale.r = 255.f / RR_MAX(hi.r - lo.r , 0.01f);
	scale.g = 255.f / RR_MAX(hi.g - lo.g , 0.01f);
	scale.b = 255.f / RR_MAX(hi.b - lo.b , 0.01f);
	scale.a = 255.f / RR_MAX(hi.a - lo.a , 0.01f);
	
	rrSurfaceRowCache rows;
	rows.Start_4F_8(const_cast<rrSurface *>(surf),RR_SURFACE_ROW_CACHE_READWRITE);

	for( int y = 0 ; y < surf->height ; y ++ )
	{
		rrColor4F * row = rows.GetRow_4F(y);

		for( int x = 0; x<surf->width;x++)
		{
			row[x].r = (row[x].r - lo.r) * scale.r;
			row[x].g = (row[x].g - lo.g) * scale.g;
			row[x].b = (row[x].b - lo.b) * scale.b;
			row[x].a = (row[x].a - lo.a) * scale.a;
		}
	}
	
}

void rrSurface_Scale_To_255_Undo(rrSurface * surf, const rrSurface_Scaler * pScaler )
{
	rrSurfaceRowCache rows;
	rows.Start_4F_8(const_cast<rrSurface *>(surf),RR_SURFACE_ROW_CACHE_READWRITE);

	rrColor4F lo = pScaler->lo;
	rrColor4F hi = pScaler->hi;
	
	rrColor4F scale;
	scale.r = RR_MAX(hi.r - lo.r , 0.01f) / 255.f;
	scale.g = RR_MAX(hi.g - lo.g , 0.01f) / 255.f;
	scale.b = RR_MAX(hi.b - lo.b , 0.01f) / 255.f;
	scale.a = RR_MAX(hi.a - lo.a , 0.01f) / 255.f;
	
	for( int y = 0 ; y < surf->height ; y ++ )
	{
		rrColor4F * row = rows.GetRow_4F(y);

		for( int x = 0; x< surf->width;x++)
		{
			row[x].r = row[x].r * scale.r + lo.r;
			row[x].g = row[x].g * scale.g + lo.g;
			row[x].b = row[x].b * scale.b + lo.b;
			row[x].a = row[x].a * scale.a + lo.a;
		}
	}
	
}

//=======================================================================================

void rrSurface_AllocCopy_MirrorExtended(rrSurface * to,const rrSurface *from,rrPixelFormat toFmt,int toW,int toH)
{
	RR_ASSERT( toW >= from->width );
	RR_ASSERT( toH >= from->height );
	
	RR_ASSERT( from->data != NULL );
	
	if ( toFmt == rrPixelFormat_Invalid )
		toFmt = from->pixelFormat;
		
	rrSurface_Alloc(to,toW,toH,toFmt);
	
	rrSurface_BlitNormalized(to,from);
	
	rrSurface_MirrorExtend(to,from->width,from->height);
}

void rrSurface_AllocCopy_MirrorExtended_Pad4(rrSurface * to,const rrSurface *from,rrPixelFormat toFmt)
{
	int toW = rrAlignUp(from->width,4);
	int toH = rrAlignUp(from->height,4);
	
	rrSurface_AllocCopy_MirrorExtended(to,from,toFmt,toW,toH);
}


void rrSurface_MirrorExtend(rrSurface * pSurf,int oldW,int oldH)
{
	RR_ASSERT( pSurf->width  >= oldW );
	RR_ASSERT( pSurf->height >= oldH );

	// first blit rows :
	for(int toy=oldH;toy<pSurf->height;toy++)
	{
		int fmy = oldH - (toy - oldH) - 1;
		if ( fmy < 0 ) fmy = 0;
		
		rrSurface_BlitRect(pSurf,pSurf,0,fmy,0,toy, oldW,1);
	}
	
	// then columns for all rows :
	
	/*
	for(int y=0;y<pSurf->height;y++)
	{
		for(int tox=oldW;tox<pSurf->width;tox++)
		{
			int fmx = oldW - (tox - oldW) - 1;
			if ( fmx < 0 ) fmx = 0;
			
			rrSurface_BlitRect(pSurf,pSurf,fmx,y,tox,y, 1,1);
		}
	}
	*/
	
	const rrPixelFormatInfo * pInfo = rrPixelFormat_GetInfo(pSurf->pixelFormat);
	int bypp = pInfo->bytesPerPixel;
	RR_ASSERT_ALWAYS( bypp != 0 );
	
	// then columns for all rows :
	for(int y=0;y<pSurf->height;y++)
	{
		U8 * pRow = rrPixelFormat_Seek(pSurf->pixelFormat,pSurf->data,pSurf->stride,0,y);
		
		for(int tox=oldW;tox<pSurf->width;tox++)
		{
			int fmx = 2*oldW - tox - 1;
			if ( fmx < 0 ) fmx = 0;
			
			//rrSurface_BlitRect(pSurf,pSurf,fmx,y,tox,y, 1,1);
		
			memcpy( pRow + tox * bypp , pRow + fmx * bypp, bypp );
		}
	}
}


RR_NAMESPACE_END
