// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(parent,OodleAPI_TextureRTBase)
#pragma once

#include "oodlecore.h"

OODLE_NS_START

PUBPRI(-10025)
PUBSTART

// header version :
//	the DLL is incompatible when MAJOR is bumped
//	MINOR is for internal revs and bug fixes that don't affect API compatibility
#define OODLE2TEXRT_VERSION_MAJOR			9
#define OODLE2TEXRT_VERSION_MINOR			14

// OodleTextureRTVersion string is 1 . MAJOR . MINOR
//	don't make it from macros cuz the doc tool has to parse the string literal

#define OodleTextureRTVersion "2.9.14"    IDOC
/*
*/

PUBEND

OODLE_NS_END


