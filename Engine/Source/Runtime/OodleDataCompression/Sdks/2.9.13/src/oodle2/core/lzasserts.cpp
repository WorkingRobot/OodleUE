// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "lzasserts.h"

OODLE_NS_START

// changed 01-21-2022 to be OFF by default
// used to be ON by default (in Debug builds only of course)
//
// do clients running Debug builds want early asserts on corrupt data?
// I always thought yes, that's why it was on by default
//
// note because this is not publicly exposed, clients always get the default value
// clients almost always use release builds except when trying to diagnose problems
// we could expose g_do_lz_assert_on_corrupt_data through OodleConfigValues like other globals, but meh

bool g_do_lz_assert_on_corrupt_data = false;

OODLE_NS_END
