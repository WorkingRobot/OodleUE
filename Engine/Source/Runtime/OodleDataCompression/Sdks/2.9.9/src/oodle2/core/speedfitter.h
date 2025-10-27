// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlecore.h"

// toggle here :
/*
#ifndef OODLE_BUILDING_DLL
// speedfitter uses hacky calls up to ext so doesn't work in dll
#define SPEEDFITTING	1
#endif
/**/

#ifdef SPEEDFITTING

#include "speedfitterapi.h"

#else // not SPEEDFITTING

// stubs :

OODLE_NS_START

OODLE_NS_END

#endif

