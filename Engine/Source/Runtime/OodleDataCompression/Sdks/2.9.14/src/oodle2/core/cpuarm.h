// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrbase.h"
#include "oodlecore.h"

#define RRARM_CPU_INITIALIZED	(1U<<0)		// bit gets set on CPUARM_detect() call
#define RRARM_CPU_ROUNDINGMUL	(1U<<1)		// ARMv8.1 SIMD rounding multiply (SQRDMLAH/SQRDMLSH)
#define RRARM_CPU_DOTPROD    	(1U<<2)		// ARMv8.2 SIMD dot product insns

OODLE_NS_START

#ifdef __RADARM__

#if defined(__RADCORTEXA57__)

#define g_rrCPUARM_feature_flags (RRARM_CPU_INITIALIZED)
#define rrCPUARM_detect() /**/

#else

#define RRARM_CPU_DYNAMIC_DETECT

extern U32 g_rrCPUARM_feature_flags;
extern void rrCPUARM_detect();

#endif

static RADINLINE rrbool rrCPUARM_feature_present(U32 bit)
{
	// should have called rrCPUARM_detect :
	U32 flags = g_rrCPUARM_feature_flags;
	RR_ASSERT( flags & RRARM_CPU_INITIALIZED );
	return (flags & bit);
}

static RADINLINE rrbool rrCPUARM_all_features_present(U32 bits)
{
	// should have called rrCPUARM_detect :
	U32 flags = g_rrCPUARM_feature_flags;
	RR_ASSERT( flags & RRARM_CPU_INITIALIZED );
	return (flags & bits) == bits;
}

#else // #ifdef __RADARM__

#define rrCPUARM_detect() /**/
#define rrCPUARM_feature_present(bit) (false)
#define rrCPUARM_all_features_present(bits) (false)

#endif

OODLE_NS_END
