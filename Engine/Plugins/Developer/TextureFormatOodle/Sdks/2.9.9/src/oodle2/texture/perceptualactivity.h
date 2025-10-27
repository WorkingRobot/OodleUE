// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrsurface.h"
#include "blocksurface.h"
#include "rrcolor.h"
#include "rrdxtcblock.h"
#include "rrdxtcenums.h"
#include "cpudispatch.h"

// global multiplier to all the lambdas to scale quality :
#define OO2TEX_GLOBAL_LAMBDA_SCALE	(0.75f)

//=============================================

RR_NAMESPACE_START

struct SingleFloatBlock4x4 // A single-channel 4x4 image, one float per pixel
{
	F32 values[16];
};

void SingleFloatBlock4x4_Fill(SingleFloatBlock4x4 * pTo, const rrSurface & source, int x, int y);

struct FourFloatBlock4x4
{
	rrColor4F values[16];
};

void FourFloatBlock4x4_Fill(FourFloatBlock4x4 * pTo, const rrSurface & source, int x, int y);

static RADINLINE const SingleFloatBlock4x4 * BlockSurface_SeekC_SingleFloatBlock4x4(const BlockSurface * bs,int bi)
{
	RR_ASSERT( bs->pixelFormat == rrPixelFormat_1_F32 );
	return (const SingleFloatBlock4x4 *)BlockSurface_SeekC(bs,bi);
}

static RADINLINE const FourFloatBlock4x4 * BlockSurface_SeekC_FourFloatBlock4x4(const BlockSurface * bs,int bi)
{
	RR_ASSERT( bs->pixelFormat == rrPixelFormat_4_F32 );
	return (const FourFloatBlock4x4 *)BlockSurface_SeekC(bs,bi);
}

// NOTE: BEWARE: FourFloatBlock4x4 is RGBA order
//	BC1 and rrColorBlock4x4 are BGRA order
//  BC7 uses rrColorBlock4x4 but it's actually RGBA order
//	so BC1 and FourFloatBlock4x4 are both RGBA order, but BC1 needs a SwapRB before they are mixed in VQD
//		this would only affect R_G_B_A metric mode
//  -> not currently relevant because BC1 uses 1F , cannot do R_G_B_A

//==========================================

enum EMakeActivityMode
{
	e_make_activity_1F_RGBA,
	e_make_activity_1F_RGB,
	e_make_activity_1F_A,
	e_make_activity_4F_RGBA,
	e_make_activity_4F_RGB_A,
	e_make_activity_4F_R_G_B_A
};

static RADINLINE rrPixelFormat EMakeActivityMode_to_PF(EMakeActivityMode mam)
{
	switch(mam)
	{
	case e_make_activity_1F_RGBA:
	case e_make_activity_1F_RGB:
	case e_make_activity_1F_A:
		return rrPixelFormat_1_F32;
	case e_make_activity_4F_RGBA:
	case e_make_activity_4F_RGB_A:
	case e_make_activity_4F_R_G_B_A:
		return rrPixelFormat_4_F32;
	default:
		return rrPixelFormat_Invalid;
	}
}

// make_activity_mask does do Preprocess, output is inverse scale, just multiply by SSD
void make_activity_mask_1or4F(rrSurface * pTo,const rrSurface & source, EMakeActivityMode mam,rrDXTCOptions options);

// version for HDR formats which needs to deal with huge contrast ratios
void make_activity_mask_hdr_1F(rrSurface * pTo,const rrSurface & source);

void make_activity_mask_constant_for_rmse_1or4F(rrSurface * pTo,const rrSurface & source, rrPixelFormat pf);

//==========================================

// VQD is RGBA
// VQD reproduces SSD if activity == 1
// VQD is ~ 2*SSD
//	   ~ 22*SAD

F32 VQD_BC1(CpuDispatchFlags dispatch, const rrColorBlock4x4 & colors,const rrColor32BGRA palette[4],const U32 in_indices,const SingleFloatBlock4x4 & activity);
F32 VQD_BC1(CpuDispatchFlags dispatch, const rrColorBlock4x4 & colors,const rrColor32BGRA palette[4],const U32 in_indices,const FourFloatBlock4x4 & activity);

F32 VQD(const rrColorBlock4x4 & colors1,const rrColorBlock4x4 & colors2,const SingleFloatBlock4x4 & activity);
F32 VQD(const rrColorBlock4x4 & colors,const rrDXT1Block & dxtb,const SingleFloatBlock4x4 & activity,rrDXT1PaletteMode pal_mode);

F32 VQD(const rrColorBlock4x4 & colors1,const rrColorBlock4x4 & colors2,const FourFloatBlock4x4 & activity);
F32 VQD(const rrColorBlock4x4 & colors,const rrDXT1Block & dxtb,const FourFloatBlock4x4 & activity,rrDXT1PaletteMode pal_mode);

F32 VQD(const S16 colors1[16],const S16 colors2[16],const SingleFloatBlock4x4 & activity);

// do not call directly
namespace internal {

F32 VQD_BC1_AVX2(const rrColorBlock4x4 & colors,const rrColor32BGRA palette[4],const U32 in_indices,const SingleFloatBlock4x4 & activity);

}

//==========================================

RR_NAMESPACE_END
