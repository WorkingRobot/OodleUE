// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlecore.h"

// NOTE(fg): this is old and deprecated, DO NOT USE, we do our fuzz tests with
// libfuzzer or aflfuzz now which is much better

//#define DO_FUZZ_TEST 1 // toggle here

#ifndef DO_FUZZ_TEST
#define DO_FUZZ_TEST 0
#endif

#if DO_FUZZ_TEST

#include "internal/rrfuzztesthelper_internal.h"

#else // DO_FUZZ_TEST off

OODLE_NS_START

#define FUZZ_HELPER( expr )
#define FUZZ_MUNGE( word )

void	rrFuzz_Init();
void	rrFuzz_Shutdown();

OODLE_NS_END

#endif // DO_FUZZ_TEST
