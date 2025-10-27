// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

// Oodle platform config

// ========================================================
// This just does basic platform detection

#if defined(ANDROID)
	#define __RADANDROID__ 1
	#define __RADDETECTED__ __RADANDROID__
#endif

#if defined(__QNX__)
	#define __RADQNX__ 2
	#define __RADDETECTED__ __RADQNX__
#endif

#if defined(__linux__) && !defined(ANDROID)
	#define __RADLINUX__ 3
	#define __RADDETECTED__ __RADLINUX__
#endif

#if defined(_WIN32) || defined(WINAPI_FAMILY)
	#ifdef WINAPI_FAMILY
		// If this is #defined, we might be in a Windows Store App. But
		// VC++ by default #defines this to a symbolic name, not an integer
		// value, and those names are defined in "winapifamily.h". So if
		// WINAPI_FAMILY is #defined, #include the header so we can parse it.
		#include <winapifamily.h>
		#define RAD_WINAPI_IS_APP (!WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP))
		#if RAD_WINAPI_IS_APP
			#define __RADWINRTAPI__
			#define __RADWINRT__ 13
			#define __RADDETECTED__ __RADWINRT__
		#endif
	#else
		#define RAD_WINAPI_IS_APP 0
	#endif

	#ifndef __RADWINRT__
		// if we aren't WinRT, then we are plain old NT
		#define __RADNT__ 14
		#define __RADDETECTED__ __RADNT__
	#endif

	#define __RADWIN__
	#define RADLINK __stdcall
#endif

#if defined(__APPLE__)
	#include "TargetConditionals.h"
	#if defined(TARGET_IPHONE_SIMULATOR) && TARGET_IPHONE_SIMULATOR
		#define __RADIPHONE__ 15
		#define __RADIPHONESIM__ 16
		#define __RADDETECTED__ __RADIPHONESIM__
	#elif defined(TARGET_OS_IPHONE) && TARGET_OS_IPHONE
		#define __RADIPHONE__ 15
		#define __RADDETECTED__ __RADIPHONE__
	#else
		#define __RADMAC__ 17
		#define __RADDETECTED__ __RADMAC__
	#endif
	// IOS/TVOIS/WATCHOS are subsets of __RADIPHONE__
	#if defined(TARGET_OS_IOS) && TARGET_OS_IOS
		#define __RADIOS__ 18
	#endif
	#if defined(TARGET_OS_TVOS) && TARGET_OS_TVOS
		#define __RADTVOS__ 19
	#endif
	#if defined(TARGET_OS_WATCHOS) && TARGET_OS_WATCHOS
		#define __RADWATCHOS__ 20
	#endif

	#define __RADMACAPI__
#endif

#if defined(__EMSCRIPTEN__)
	#define __RADEMSCRIPTEN__  22
	#define __RADDETECTED__ __RADEMSCRIPTEN__

	#include <emscripten.h>

	#define RADLINK
	// @@ CB : fix me ; put it OOEXPLINK ?
	//#define RADEXPLINK EMSCRIPTEN_KEEPALIVE
#endif

#if !__RADDETECTED__
  #error "Target platform not recognized!"
#endif

