// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlebase.h"
#include "rrmem.h"
#include "rrsimd.h"

#ifdef max
#undef max
#endif
#ifdef min
#undef min
#endif

#ifdef __RADSSE2__

#define OODLE_VEC128_IMPL_X86 // #define used to guard against including vec128_x86.inl directly
#include "vec128_x86.inl"
#undef OODLE_VEC128_IMPL_X86

#endif

#ifdef DO_BUILD_NEON64

#define OODLE_VEC128_IMPL_ARM // #define used to guard against including vec128_arm.inl directly
#include "vec128_arm.inl"
#undef OODLE_VEC128_IMPL_ARM

#endif

