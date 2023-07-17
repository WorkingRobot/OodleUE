;// Copyright Epic Games, Inc. All Rights Reserved.
;// This source file is licensed solely to users who have
;// accepted a valid Unreal Engine license agreement 
;// (see e.g., https://www.unrealengine.com/eula), and use
;// of this source file is governed by such agreement.

// Helpers for ARM A64 ASM code

#if defined(__RADIPHONESIM__)

#define __RADAPPLEABI__

	.build_version iossimulator, 7, 0

#elif defined(__RADIPHONE__)

#define __RADAPPLEABI__

	.ios_version_min 7, 0

#elif defined(__RADTVOSSIM__)

#define __RADAPPLEABI__

	.build_version tvossimulator, 9, 1

#elif defined(__RADTVOS__)

#define __RADAPPLEABI__

	.tvos_version_min 9, 1

#elif defined(__RADMACARM64__)

#define __RADAPPLEABI__

#else

// Assume AAPCS64 ABI on other targets by default, since it's the standard.
#define __RADAAPCS64__

#endif

#if defined(__RADAAPCS64__)

// Default ARM Procedure Call Standard
#define MANGLE(name) name

#define FUNCTION_PRE_DECL(name) .type MANGLE(name),%function
#define FUNCTION_POST_DECL(name) .size MANGLE(name), .-MANGLE(name)

#elif defined(__RADAPPLEABI__)

#define MANGLE(name) _##name
#define FUNCTION_PRE_DECL(name)
#define FUNCTION_POST_DECL(name)

#else
#error ABI?
#endif
