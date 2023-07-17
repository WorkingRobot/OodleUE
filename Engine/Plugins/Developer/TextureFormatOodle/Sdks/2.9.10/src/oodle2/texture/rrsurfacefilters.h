// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlebase.h"
#include "rrpixelformat.h"
#include "cpudispatch.h"

RR_NAMESPACE_START

struct rrSurface;

typedef void OODLE_CALLBACK rrPixelConsumerFunc(void * ctx,const void * pixels,rrPixelFormat pf,int x0,int y,int width);

// large sigmas aren't actually supported
// fm == to okay
rrbool rrSurface_MakeGaussianBlurred(rrSurface * pTo,const rrSurface & from,double sigma,CpuDispatchFlags dispatch);

// Doesn't write to a destination surface, but rather writes it to temp mem for a rrPixelConsumerFunc
// source _must_ be 1-4 channel float, no other formats supported right now
rrbool rrSurface_MakeGaussianBlurredWithConsumer(const rrSurface & from,double sigma,rrPixelConsumerFunc * consumer,void * consumer_ctx,CpuDispatchFlags dispatch);

// from == center is standard
// sigma_spatial is in pixel distances (eg. 2.5)
// sigma_value is in [0,255] pixel levels (eg. 16)
rrbool rrSurface_MakeBilateralFiltered(rrSurface * pTo_can_be_from,const rrSurface & from,const rrSurface & center,double sigma_spatial, double sigma_value);

// do not call anything in here directly!
namespace internal {

constexpr SINTa MAX_FILTER_TAPS = 64;

// returns count actually processed
SINTa rrSurface_MakeGaussianBlurred_Sub_Line_Interior_AVX2(float *to_ptr,const float * fm_ptr,
	SINTa stride, // stride is in floats
	SINTa count,
	const F32 * filter,SINTa filter_width);

// returns count actually processed
SINTa rrSurface_MakeGaussianBlurred_Sub_Strip_V_AVX2(
	float * to_row,
	F32 * * window_rows,
	SINTa count,
	const F32 * filter,SINTa filter_width);

}

RR_NAMESPACE_END

