// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRRBITMAP_COLOR_FUNCTORS_H__
#define __RADRRBITMAP_COLOR_FUNCTORS_H__

#include "rrcolor.h"

RR_NAMESPACE_START

/**

rrSurface convention :

Y is in G
Cr in R, Cb in B

U = Cb
V = Cr

so this is {Cr, Y, Cb} in sample order, or {UYV}

(beware: different that cblib FloatImage that puts Y in plane 0)

**/

// mainly for ColorConvert.cpp

/*

JPEGLS color transform is lossless 8 bit -> 8bit 
but useless because it requires all ops be done on the U8 ring
so downsampling and such breaks unless you do weird shit

*/

/*

struct ColorFunctorI_RGB_to_JPEGLS
{
	const rrColor4I operator() (const rrColor4I & from)
	{
		rrColor4I ret;
		
		ret.g = from.g;
		ret.r = (U8) ( 128 + from.r - from.g );
		ret.b = (U8) ( 128 + from.b - from.g );
		ret.a = from.a;
			
		return ret;
	}
};

struct ColorFunctorI_JPEGLS_to_RGB
{
	const rrColor4I operator() (const rrColor4I & from)
	{
		rrColor4I ret;
		
		ret.g = from.g;
		ret.r = (U8) ( from.r + from.g - 128 );
		ret.b = (U8) ( from.b + from.g - 128 );
		ret.a = from.a;
			
		return ret;
	}
};
*/

/*

lossy YCoCg - no range expansion
drops bits in Co & Cg

@@ less lossy variant
if y < 32 or 64
don't do the extra /2 in CoCg
because they'll fit in 255 without it

but that's a non-linearity that's bad for RDO & interpolation & mips & such

*/

struct ColorFunctorI_RGB_to_YCoCgLossy
{
	const rrColor4I operator() (const rrColor4I & from)
	{
		rrColor4I ret;
		
		/*
		S32 y  = ( from.g + from.g + from.r + from.b ) / 4;
		S32 Co = ( from.r - from.b )/2;
		S32 Cg = ( from.g + from.g - from.r - from.b )/4;
		*/
		// this wins by a lot :
		S32 y  = ( from.g + from.g + from.r + from.b + 2) / 4;
		S32 Co = ( from.r - from.b )/2;
		S32 Cg = ( from.g + from.g - from.r - from.b )/4;
		
		ret.g = y;
		ret.r = Cg + 128;
		ret.b = Co + 128;
		ret.a = from.a;
			
		return ret;
	}
};

struct ColorFunctorI_YCoCgLossy_to_RGB
{
	const rrColor4I operator() (const rrColor4I & from)
	{
		rrColor4I ret;
		
		S32 y  = from.g;
		S32 Cg = from.r - 128;
		S32 Co = from.b - 128;
		
		ret.g = y + Cg;
		ret.r = y - Cg + Co;
		ret.b = y - Cg - Co;
		ret.a = from.a;
			
		return ret;
	}
};

// "Lossless" is bit expanding
//	needs 9 bits for Co and Cg

struct ColorFunctorI_RGB_to_YCoCgLossless
{
	const rrColor4I operator() (const rrColor4I & from)
	{
		rrColor4I ret;
		
		int Co = from.r - from.b;
		int t = from.b + (Co/2);
		int Cg = from.g - t;
		int y = t + (Cg/2);
	   
		ret.g = y;
		ret.r = Co + 128;
		ret.b = Cg + 128;
		ret.a = from.a;
			
		return ret;
	}
};

struct ColorFunctorI_YCoCgLossless_to_RGB
{
	const rrColor4I operator() (const rrColor4I & from)
	{
		rrColor4I ret;
		
		S32 y  = from.g;
		S32 Co = from.r - 128;
		S32 Cg = from.b - 128;
		
		int s = y - (Cg/2);
		ret.g = Cg + s;
		ret.b = s - (Co/2);
		ret.r = ret.b + Co;
		ret.a = from.a;
			
		return ret;
	}
};


// map [0,from_max] to [0,127]
static int quant_onetwo_127(int val,int from_max)
{
	RR_ASSERT( from_max <= 255 );
	RR_ASSERT( val >= 0 && val <= from_max );
	
	// t + (Co_range-t)/2 = 127
	// t/2 = 127 - Co_range/2;
	int twos_start = 2*(127 - from_max/2);
	if ( val < twos_start )
	{
		return val;
	}
	else
	{
		int ret = twos_start + (val - twos_start)/2;
		RR_ASSERT( ret <= 127 );
		return ret;
	}
}	

static int dequant_onetwo_127(int qval,int from_max)
{
	RR_ASSERT( from_max <= 255 );
	RR_ASSERT( qval >= 0 && qval <= 127 );
	// RR_ASSERT( qval <= from_max ); // <- this is true for valid values in gamut
		// but can become un-true after RDO
		// need to make sure that you behave well if qval >	from_max
	
	int twos_start = 2*(127 - from_max/2);
	if ( qval < twos_start )
	{
		return qval;
	}
	else
	{
		int ret = twos_start + (qval - twos_start)*2;
		// @@ +1 ?
		RR_ASSERT( ret <= from_max );
		return ret;
	}
}

static int quant_onetwo_127_signed(int val,int from_max)
{
	if ( val < 0 )
	{
		return - quant_onetwo_127(-val,from_max);
	}
	else
	{
		return quant_onetwo_127(val,from_max);
	}
}
static int dequant_onetwo_127_signed(int val,int from_max)
{
	if ( val < 0 )
	{
		return - dequant_onetwo_127(-val,from_max);
	}
	else
	{
		return dequant_onetwo_127(val,from_max);
	}
}

// SemiLossy
// non-bit-expanding
// is lossless for low Y
//	lossy for high Y

struct ColorFunctorI_RGB_to_YCoCg_SemiLossy
{
	const rrColor4I operator() (const rrColor4I & from)
	{
		rrColor4I ret;
		
		int Co = from.r - from.b;
		int t = from.b + (Co/2);
		int Cg = from.g - t;
		int y = t + (Cg/2);
	   
		ret.g = y;
		
		int Co_range = RR_MIN(255, 4*y+1);
		int Cg_range = RR_MIN(255, 2*y+1);
		
		ret.r = quant_onetwo_127_signed(Co,Co_range) + 128;
		ret.b = quant_onetwo_127_signed(Cg,Cg_range) + 128;
		ret.a = from.a;
			
		return ret;
	}
};

struct ColorFunctorI_YCoCg_SemiLossy_to_RGB
{
	const rrColor4I operator() (const rrColor4I & from)
	{
		rrColor4I ret;
		
		S32 y  = from.g;
		
		int Co_range = RR_MIN(255, 4*y+1);
		int Cg_range = RR_MIN(255, 2*y+1);
		
		S32 Co = dequant_onetwo_127_signed( from.r - 128 , Co_range );
		S32 Cg = dequant_onetwo_127_signed( from.b - 128 , Cg_range);
		
		int s = y - (Cg/2);
		ret.g = Cg + s;
		ret.b = s - (Co/2);
		ret.r = ret.b + Co;
		ret.a = from.a;
			
		return ret;
	}
};


struct ColorFunctorF_RGB_to_YCoCg
{
	const rrColor4F operator() (const rrColor4F & from)
	{
		rrColor4F ret;
		
		F32 y  = ( from.g + from.g + from.r + from.b ) / 4.f;
		F32 Co = ( from.r - from.b )/2.f;
		F32 Cg = ( from.g + from.g - from.r - from.b )/4.f;
   
		ret.g = y;
		ret.r = Cg + 128.f;
		ret.b = Co + 128.f;
		ret.a = from.a;
			
		return ret;
	}
};

struct ColorFunctorF_YCoCg_to_RGB
{
	const rrColor4F operator() (const rrColor4F & from)
	{
		rrColor4F ret;
		
		F32 y  = from.g;
		F32 Cg = from.r - 128.f;
		F32 Co = from.b - 128.f;
		
		ret.g = y + Cg;
		ret.r = y - Cg + Co;
		ret.b = y - Cg - Co;
		ret.a = from.a;
			
		return ret;
	}
};

struct ColorFunctorF_RGB_to_YCrCb601
{
	const rrColor4F operator() (const rrColor4F & from)
	{
		rrColor4F ret;
		
		float R = from.r, G = from.g, B = from.b;
		
		float Y  =  0.29900f * R + 0.58700f * G + 0.11400f * B;
		float Cb = -0.16874f * R - 0.33126f * G + 0.50000f * B  + 128.f;
		float Cr =  0.50000f * R - 0.41869f * G - 0.08131f * B  + 128.f;
 
		ret.r = Cr;
		ret.g = Y;
		ret.b = Cb;
		ret.a = from.a;
			
		return ret;
	}
};

struct ColorFunctorF_YCrCb601_to_RGB
{
	const rrColor4F operator() (const rrColor4F & from)
	{
		rrColor4F ret;
		
		F32 y  = from.g;
		F32 Cr = from.r - 128.f;
		F32 Cb = from.b - 128.f;
		
		ret.r = y + 1.40200f * Cr;
		ret.g = y - 0.34414f * Cb - 0.71414f * Cr;
		ret.b = y + 1.77200f * Cb;
		ret.a = from.a;
			
		return ret;
	}
};

RR_NAMESPACE_END

#endif // __RADRRBITMAP_COLOR_FUNCTORS_H__
