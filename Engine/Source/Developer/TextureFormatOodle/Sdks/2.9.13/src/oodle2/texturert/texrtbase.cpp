// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(parent,OodleAPI_TextureRT)
//idoc(end)
#include "texrtbase.h"
#include "cpux86.h"

OODLE_NS_START

OodleTexRT_Err OodleTexRT_Enter()
{
	// OodleTexRT_Enter is called by all the public API entry points
	//	it does any needed one-time inits

	// rrCPUx86_detect has its own internal do-once :
	// Texture *encode* requires RRX86_CPU_SAFE_TEXTURE, but we're OK with the smaller _DATA subset
	rrCPUx86_detect(RRX86_CPU_SAFE_DATA);

	// NOTE we have our own separate copy of rrCPUx86 from Oodle Core!
	//	our copy is in oo2tex: ; they need their own inits if you use both

	// we have scalar fallbacks and no minimum required CPU target.
	return OodleTexRT_Err_OK;
}

OODLE_NS_END

