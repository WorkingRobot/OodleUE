// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "oodlecoreinit.h"
#include "cpux86.h"

OODLE_NS_START

static volatile rrbool s_OodleCore_Enter_done = 0;

rrbool OodleCore_Enter()
{
	if ( s_OodleCore_Enter_done )
		return true;

	rrCPUx86_detect();
	s_OodleCore_Enter_done = 1;

	return true;
}

OODLE_NS_END
