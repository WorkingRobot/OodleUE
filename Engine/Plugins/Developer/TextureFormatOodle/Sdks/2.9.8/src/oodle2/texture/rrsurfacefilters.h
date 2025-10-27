// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlebase.h"
#include "rrpixelformat.h"

RR_NAMESPACE_START

struct rrSurface;

typedef void OODLE_CALLBACK rrPixelConsumerFunc(void * ctx,const void * pixels,rrPixelFormat pf,int x0,int y,int width);

// large sigmas aren't actually supported
// fm == to okay
rrbool rrSurface_MakeGaussianBlurred(rrSurface * pTo,const rrSurface & from,double sigma);

// Doesn't write to a destination surface, but rather writes it to temp mem for a rrPixelConsumerFunc
// source _must_ be 1-4 channel float, no other formats supported right now
rrbool rrSurface_MakeGaussianBlurredWithConsumer(const rrSurface & from,double sigma,rrPixelConsumerFunc * consumer,void * consumer_ctx);

// from == center is standard
// sigma_spatial is in pixel distances (eg. 2.5)
// sigma_value is in [0,255] pixel levels (eg. 16)
rrbool rrSurface_MakeBilateralFiltered(rrSurface * pTo_can_be_from,const rrSurface & from,const rrSurface & center,double sigma_spatial, double sigma_value);

RR_NAMESPACE_END

