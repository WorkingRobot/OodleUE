// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "oodlecoreplugins.h"

#ifdef OODLE_BUILDING_NETWORK

#include "../network/oodlenetplugins_gen.h"
#include "../network/oodlenetplugins_gen.inc"

#elif defined(OODLE_BUILDING_TEXTURE)

#include "../texture/oodletexplugins_gen.h"
#include "../texture/oodletexplugins_gen.inc"

#elif defined(OODLE_BUILDING_TEXTURERT)

#include "../texturert/oodletexrtplugins_gen.h"
#include "../texturert/oodletexrtplugins_gen.inc"

#else

#include "oodlecoreplugins_gen.h"
#include "oodlecoreplugins_gen.inc"

#endif
