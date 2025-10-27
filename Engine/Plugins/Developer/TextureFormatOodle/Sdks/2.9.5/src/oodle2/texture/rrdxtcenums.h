// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRRBITMAP_DXTC_ENUMS_H__
#define __RADRRBITMAP_DXTC_ENUMS_H__

#include "rrbase.h"

RR_NAMESPACE_START

enum rrDXTCLevel
{
	// levels 1,2,3 are the public API levels
	//	default is level 3 (VerySlow), max quality realistic level
	//	"Reference" level 4 is max quality regardless of speed, to give us a target for VerySlow to aim for
	rrDXTCLevel_VeryFast=0, // == 1 secret level in OodleTex_ API ; too low quality, not currently well optimized for this quality-speed tradeoff
	rrDXTCLevel_Fast=1,		// == OodleTex_EncodeEffortLevel_Low
	rrDXTCLevel_Slow=2,		// == OodleTex_EncodeEffortLevel_Normal
	rrDXTCLevel_VerySlow=3, // == OodleTex_EncodeEffortLevel_High == OodleTex_EncodeEffortLevel_Default
	rrDXTCLevel_Reference=4,// == 99 secret level in OodleTex_ API ; too slow to be practical, not a good time-quality tradeoff; just a max quality reference
	rrDXTCLevel_Count=5,
	rrDXTCLevel_Default = rrDXTCLevel_VerySlow // matches public API default
};

// dxtc options are bit flags that can be combined
enum rrDXTCOptions
{
	rrDXTCOptions_None = 0,
	
	// rrDXTCOptions_BC1_OneBitAlpha just means pay attention to alpha - 
	//	if you know you will decode to a surface without alpha, then turn this flag off and it lets me use {0,0,0,0} as black instead of transparent
	rrDXTCOptions_BC1_OneBitAlpha = 1,
	
	// PreserveExtremes makes sure we keep 0 and 255 ; useful for alpha channels in particular  
	rrDXTCOptions_BC345_PreserveExtremes = 2, // BC3,4,5 respect this, BC6,7 ignore this , BC1&2 it doesn't apply
	
	// Ignore alpha channel when encoding. For BC7 which always codes alpha.
	rrDXTCOptions_BC7_IgnoreAlpha = 4, // applies to BC7 only currently ; a bit redundant with rrDXTCOptions_BC1_OneBitAlpha

	// Dither *for encoder* ; hurts RMSE, sometimes helps visual quality
	//	my dither impl currently sucks ass, just remove it for now
	//rrDXTCOptions_Dither = 8,
	
	// Try to preserve luminance during encoding. Perceptual improvement for RGB data,
	// but not applicable when the channels are not meant to be interpreted as RGB.
	rrDXTCOptions_BC6_FavorLuminance = 16, // currently only used by BC6H

	rrDXTCOptions_Default = 0 // none
	//rrDXTCOptions_Count = 16
};

const char * rrDXTCLevel_GetName(rrDXTCLevel level);
const char * rrDXTCOptions_GetName(rrDXTCOptions options);

struct rrDXTCRD_Options
{
	void * config_override;
	rrDXTCLevel effort;

	bool use_bc3_alpha_lambda;
	int bc3_alpha_lambda;
};

RR_NAMESPACE_END

#endif // __RADRRBITMAP_DXTC_ENUMS_H__
