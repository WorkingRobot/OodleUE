// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrdxtcenums.h"

RR_NAMESPACE_START

const char * rrDXTCLevel_GetName(rrDXTCLevel level)
{
	switch(level)
	{
	case rrDXTCLevel_VeryFast: return "VeryFast";
	case rrDXTCLevel_Fast: return "Fast";
	case rrDXTCLevel_Slow: return "Slow";
	case rrDXTCLevel_VerySlow: return "VerySlow";
	case rrDXTCLevel_Reference: return "Reference";
	default : return "invalid";
	}
}

const char * rrDXTCOptions_GetName(rrDXTCOptions options)
{
	//rrDXTCOptions_BC1_OneBitAlpha = 1,
	//rrDXTCOptions_BC345_PreserveExtremes = 2,
	//rrDXTCOptions_BC7_IgnoreAlpha = 4,
	
	switch((int)options)
	{
	case 0: return "none";
	case 1: return "1bitAlpha";
	case 2: return "dxt5Extremes";
	case 3: return "1bitAlpha|dxt5Extremes";
	case 4: return "IgnoreAlpha";
	case 5: return "IgnoreAlpha|1bitAlpha";
	case 6: return "IgnoreAlpha|dxt5Extremes";
	case 7: return "IgnoreAlpha|1bitAlpha|dxt5Extremes";
	default : return "invalid";
	}
}

RR_NAMESPACE_END
