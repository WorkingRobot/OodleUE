// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "cpuarm.h"

#if defined(__RADARM__) && defined(__RAD64__) && defined(__RADLINUX__)
#include <sys/auxv.h>
#endif

#ifdef __RADARM__

OODLE_NS_START

#ifdef RRARM_CPU_DYNAMIC_DETECT

U32 g_rrCPUARM_feature_flags = 0;

void rrCPUARM_detect()
{
	// if we already detected, we're good!
	U32 features = g_rrCPUARM_feature_flags; // atomic or volatile load?
	if (features & RRARM_CPU_INITIALIZED)
		return;

	features = 0;

#if defined(__RAD64__) && defined(__RADLINUX__)
	unsigned long hwcaps = getauxval(AT_HWCAP);

	if (hwcaps & (1 << 20)) // HWCAP_ASIMDDP
	{
		features |= RRARM_CPU_DOTPROD;
	}
#elif defined(__RAD64__) && defined(__RADMAC__)
	// 64-bit ARM Macs have never shipped without ASIMD dot product
	// support, so we can safely assume it's there
	features |= RRARM_CPU_DOTPROD;
#endif

	// Windows ARM64 IsProcessorFeaturePresent() currently (May 2022)
	// seems to be have no way to query for presence of ASIMD dot
	// product instructions.

	features |= RRARM_CPU_INITIALIZED;

	g_rrCPUARM_feature_flags = features; // atomic or volatile store
}

#endif // RRARM_CPU_DYNAMIC_DETECT

OODLE_NS_END

#endif // __RADARM__

