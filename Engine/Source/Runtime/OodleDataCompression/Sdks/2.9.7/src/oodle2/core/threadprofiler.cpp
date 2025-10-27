// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "threadprofiler.h"

OODLE_NS_START

//-------------------------------------------------------

ThreadProfiler_Handle	(OODLE_CALLBACK *fp_ThreadProfiler_Push)(const char * label,U64 guid) = NULL;
void	(OODLE_CALLBACK *fp_ThreadProfiler_Pop)(ThreadProfiler_Handle handle) = NULL;
void (OODLE_CALLBACK *fp_ThreadProfiler_Tag)(const char * label,U64 guid) = NULL;

//-------------------------------------------------------

OODLE_NS_END
