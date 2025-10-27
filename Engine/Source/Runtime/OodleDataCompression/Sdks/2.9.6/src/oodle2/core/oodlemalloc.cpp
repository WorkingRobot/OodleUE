// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "oodlemalloc.h"
#include "oodlebase.h"

OODLE_NS_START

// Oodle_Core_Malloc_Failed never returns
void Oodle_Core_Malloc_Failed( SINTa bytes )
{
	// Debug only break before we do anything else :
	RR_ASSERT_FAILURE("Oodle_Core_Malloc_Failed");
	
	ooLogError("Oodle_Core_Malloc_Failed ; " RR_SINTa_FMT " bytes\n", bytes );
	
	// release mode break :
	RR_ASSERT_ALWAYS( false );
	
	// don't return :
	for(;;)
	{
	}
	// ?
	// or exit() ?
}

OODLE_NS_END