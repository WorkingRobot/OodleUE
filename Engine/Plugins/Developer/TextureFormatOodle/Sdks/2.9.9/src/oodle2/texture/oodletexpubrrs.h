// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodletexpub.h"
#include "rrpixelformat.h"
#include "rrsurface.h"
#include "rrdxtcenums.h"

/****

OodleTexPub translation back to RRS land

so you can call the public API from RRS

*****/

OODLE_NS_START

rrPixelFormat OodleTex_PixelFormat_to_rrPixelFormat(OodleTex_PixelFormat pf);
rrPixelFormat OodleTex_BC_to_rrPixelFormat(OodleTex_BC pf);
OodleTex_PixelFormat rrPixelFormat_to_OodleTex_PixelFormat(rrPixelFormat pf);
OodleTex_BC rrPixelFormat_to_OodleTex_BC(rrPixelFormat pf);

OodleTex_PixelFormat OodleTex_Surface_from_RRS( OodleTex_Surface * is, const rrSurface * rrs );

// OodleTex_RMSE_Normalized_BCNAware
// bcn_pf can be rrPixelFormat_Invalid
F32 rrSurface_RMSE_Normalized_BCNAware(
	rrPixelFormat bcn_pf,
	rrDXTCOptions options,
	const rrSurface * rrs1,
	const rrSurface * rrs2
	);

OodleTex_BC string_to_bc(const char * str);
		
OODLE_NS_END
