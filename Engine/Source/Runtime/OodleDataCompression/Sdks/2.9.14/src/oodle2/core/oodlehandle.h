// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlecore.h"

OODLE_NS_START

/**

OodleXHandle is actually an Ext-only thing

but LRM in Core stores them in its struct
so I define the type here (in Core)

It's public in ExtBase.h and goes in oodle2x.h publicly

**/

typedef U64 OodleXHandle;

OODLE_NS_END
