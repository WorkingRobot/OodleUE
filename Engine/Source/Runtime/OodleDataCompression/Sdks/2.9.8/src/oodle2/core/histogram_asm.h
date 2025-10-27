// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlecore.h"

// Used to talk to ASM loops (need to adapt them if you change the layout)
struct OodleHistogramContext
{
	const U8 * inptr;
	SINTa inlen;
	U32 * counts;
	SINTa nsyms_flags;	// nsyms | (should_add_onto_count ? (1<<16) : 0); it's a SINTa for alignment reasons
	U32 scratch[4*256];
};

// OODLE_HISTO_X64GENERIC_ASM defines set by cdep

#ifdef OODLE_HISTO_X64GENERIC_ASM
extern "C" void oodle_histo_x64_generic(OodleHistogramContext * ctx);
#endif

#ifdef OODLE_HISTO_A64_ASM
extern "C" const U8 * oodle_histo_a64_kern(const U8 * rawPtr, SINTa rawLen, U32 * counts);
#endif
