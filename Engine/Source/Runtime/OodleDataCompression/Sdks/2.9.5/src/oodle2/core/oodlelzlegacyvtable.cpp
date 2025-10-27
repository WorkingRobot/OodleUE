// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "oodlelzlegacyvtable.h"

// Core side of Legacy vtable
// Core oodlelzcompressors calls this
// Legacy lib plugs in this vtable

OODLE_NS_START

OodleLZLegacyVTable g_OodleLZLegacyVTable = { 0 };

OOFUNC1 void OOFUNC2 OodleLZLegacyVTable_InstallToCore( const OodleLZLegacyVTable * from )
{
	g_OodleLZLegacyVTable = *from;
}

OODLE_NS_END


