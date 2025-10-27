// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrsimpleprof.h"

OODLE_NS_START

// set by SimpleProf_Init :
U64 (OODLE_CALLBACK *fp_rrSimpleProf_Push)(U32 * pindex, const char * label) = NULL;
void (OODLE_CALLBACK *fp_rrSimpleProf_Pop)(U32 index, U64 push_time, int count) = NULL;

OODLE_NS_END
