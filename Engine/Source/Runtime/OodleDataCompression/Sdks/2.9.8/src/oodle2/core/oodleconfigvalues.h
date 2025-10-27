// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlecore.h"

OODLE_NS_START

extern S32 g_Oodle_Debug_TweakValue; // = 0

#define OODLE_TWEAK_OR_DEFAULT(def) ( g_Oodle_Debug_TweakValue ? g_Oodle_Debug_TweakValue : (def) )

//=====================================================

extern S32 g_OodleLZ_LW_MinSizeLZHFallback;

extern S32 g_OodleLZ_LW_LRM_step;
extern S32 g_OodleLZ_LW_LRM_hashLength;
extern S32 g_OodleLZ_LW_LRM_jumpbits;

extern S32 g_OodleLZ_Decoder_Max_Stack_Size;
//extern S32 g_OodleLZ_Small_Buffer_LZ_Fallback_Size;
				
extern S32 g_OodleLZ_BackwardsCompatible_MajorVersion;

extern bool g_Oodle_UsageWarningsDisabled;

#define ooLogUsageWarning2(fmt,...)	do { \
	if (!g_Oodle_UsageWarningsDisabled) { \
		rrPrintf_v0("OODLE USAGE WARNING : " fmt "(Usage warnings can be disabled via Oodle_SetUsageWarnings.)\n",__VA_ARGS__); \
	} } while(0)

//=====================================================

OODLE_NS_END
