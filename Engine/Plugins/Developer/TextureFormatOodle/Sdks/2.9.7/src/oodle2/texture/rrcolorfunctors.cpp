// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrcolorfunctors.h"

EXPORT_SOME_CRAP(rrcolorfunctors);



#if 0

/**

test_YCoCg_SemiLossy
04-13-2020

idea for loss lossy YCoCg 
(in integers without going out of 8-bit range)


YCoCg Smart Lossy :
	non-bit expanding
	Do YCoCg

	now Co Cg are 9 bits *sometimes*
	when they do exceed the 256 possible values :
		do an interval remap to use all 256 outputs
		the area around [0] they get steps of 1
		near the extremes they get steps of 2
	Co Cg are <= |Y| (or something like that)
		so you can tell their possible range of values
		often they are not actually bit expanding at all

	around gray you don't make any loss at all
	only the very bright saturated colors are out of the 8-bit gamut
		and they get 1 bit quantized in chroma

-> result is not dramatic numerically
but perceptually it does give you exact preservation of dark colors
instead of 2:1 quantization of Co,Cg

**/

#include "rrcolorfunctors.h"

void test_YCoCg_SemiLossy()
{
	F32 Cos = 0.f;
	F32 Cgs = 0.f;
	U32 dsqr_max = 0;
	U32 dsqr_sum = 0;
	U32 lossy_dsqr_sum = 0;

	for LOOP(r,256)
	{
	for LOOP(g,256)
	{
	for LOOP(b,256)
	{
		rrColor4I c;
		c.r = r;
		c.g = g;
		c.b = b;
		c.a = 255;
		rrColor4I ycocg = ColorFunctorI_RGB_to_YCoCgLossless()(c);
		
		int y = ycocg.g;
		int Co = ycocg.r - 128;
		int Cg = ycocg.b - 128;
		
		// |Co| <= 4*y;
		// |Cg| <= 2*y;
		
		RR_ASSERT( RR_ABS(Co) <= 255 );
		RR_ASSERT( RR_ABS(Cg) <= 255 );
		RR_ASSERT( RR_ABS(Co) <= 4*y+1 );
		RR_ASSERT( RR_ABS(Cg) <= 2*y+1 );
		// y==0, Co or Cg = 1 is possible
		
		Cos = RR_MAX(Cos, RR_ABS(Co)/(F32)(y+1) );
		Cgs = RR_MAX(Cgs, RR_ABS(Cg)/(F32)(y+1) );
		
		int Co_range = RR_MIN(255, 4*y+1);
		int Coq = quant_onetwo_127_signed(Co,Co_range);

		int Cg_range = RR_MIN(255, 2*y+1);
		int Cgq = quant_onetwo_127_signed(Cg,Cg_range);
		
		RR_ASSERT( (Coq+128) >= 0 && (Coq+128) <= 255 );
		RR_ASSERT( (Cgq+128) >= 0 && (Cgq+128) <= 255 );
		
		int Codeq = dequant_onetwo_127_signed(Coq,Co_range);
		int Cgdeq = dequant_onetwo_127_signed(Cgq,Cg_range);
		
		int dCo = RR_ABS( Co - Codeq );
		int dCg = RR_ABS( Cg - Cgdeq );
		
		RR_ASSERT( dCo <= 1 );
		RR_ASSERT( dCg <= 1 );
		
		RR_ASSERT( dCo == 0 || y >=32 );
		RR_ASSERT( dCg == 0 || y >=64 );
		
		ycocg.r = Codeq + 128;
		ycocg.b = Cgdeq + 128;
		
		rrColor4I rgb = ColorFunctorI_YCoCgLossless_to_RGB()(ycocg);
		U32 dsqr = rrColor4I_DeltaSqrRGBA(c,rgb);
		// dsqr maximum of 2
		//	that's +-1 in two components, never more than 1 in any component
		RR_ASSERT( dsqr <= 2 );
		
		dsqr_max = RR_MAX(dsqr_max,dsqr);
		dsqr_sum += dsqr;
		
		
		ycocg = ColorFunctorI_RGB_to_YCoCgLossy()(c);
		rgb = ColorFunctorI_YCoCgLossy_to_RGB()(ycocg);
		dsqr = rrColor4I_DeltaSqrRGBA(c,rgb);
		// "Lossy" dsqr is also maximum of 2
		RR_ASSERT( dsqr <= 2 );
		lossy_dsqr_sum += dsqr;
	}
	}
	}

// MSE :
// dsqr_sum/(256.0*256*256) : 0.856189
// lossy_dsqr_sum/(256.0*256*256) : 1.12402

	rrprintfvar(dsqr_max);
	rrprintfvar(dsqr_sum/(256.0*256*256));
	rrprintfvar(lossy_dsqr_sum/(256.0*256*256));
	rrprintfvar(Cos);
	rrprintfvar(Cgs);
}
#endif
