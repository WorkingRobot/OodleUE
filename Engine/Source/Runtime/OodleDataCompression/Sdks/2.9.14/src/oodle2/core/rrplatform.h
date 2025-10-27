// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.
#pragma once

#ifndef __RADRR_PLATFORMH__
#define __RADRR_PLATFORMH__

#include "../base/oodlebasetypes.h"
#include "oodle_platform.h" // pulled from whichever platform config dir is active

// Mapping from API OO type/macros to internal RADRTL types
typedef OO_U8 U8;
typedef OO_S8 S8;
typedef OO_U16 U16;
typedef OO_S16 S16;
typedef OO_U32 U32;
typedef OO_S32 S32;
typedef OO_U64 U64;
typedef OO_S64 S64;
typedef OO_UINTa UINTa;
typedef OO_SINTa SINTa;
typedef OO_F32 F32;
typedef OO_F64 F64;
typedef OO_BOOL rrbool;

#define RADCOPYRIGHT OOCOPYRIGHT

//-------------------------------------------------
// ISA detection

#if defined(__arm__) || defined( _M_ARM )
	#define __RADARM__ 1
	#define __RADDETECTEDPROC__ __RADARM__
	#define __RADLITTLEENDIAN__
#endif
#if defined(__i386) || defined( __i386__ ) || defined( _M_IX86 ) || defined( _X86_ )
	#define __RADX86__ 2
	#define __RADDETECTEDPROC__ __RADX86__
	#define __RADLITTLEENDIAN__
#endif
#if defined(_x86_64) || defined( __x86_64__ ) || defined( _M_X64 ) || defined( _M_AMD64 )
	#define __RADX86__ 2
	#define __RADX64__ 3
	#define __RADDETECTEDPROC__ __RADX64__
	#define __RADLITTLEENDIAN__
#endif
#if defined(__powerpc) || defined( _M_PPC )
	#define __RADPPC__ 4
	#define __RADALTIVEC__
	#define __RADDETECTEDPROC__ __RADPPC__
	#define __RADBIGENDIAN__
#endif
#if defined( __aarch64__ ) || defined( __arm64__ ) || defined( _M_ARM64 )
	#define __RADARM__ 1
	#define __RADARM64__ 6
	#define __RADDETECTEDPROC__ __RADARM64__
	#define __RADLITTLEENDIAN__
	#define __RADNEON__ // always has NEON/AdvSIMD on A64 state
#endif
#if defined(__wasm32__)
	#define __RADWASM__ 7
	#define __RADDETECTEDPROC__ __RADWASM__
	#define __RADLITTLEENDIAN__
	// wasm32 has 64-bit registers but only 32-bit pointers.
	#define __RAD64REGS__
#endif
#if defined(__wasm64__)
	#define __RADWASM__ 7
	#define __RADWASM64__ 8
	#define __RADDETECTEDPROC__ __RADWASM64__
	#define __RADLITTLEENDIAN__
	#define __RAD64__
	#define __RAD64REGS__
#endif

#if !defined(__RADDETECTEDPROC__)
	#error "rrbase.h failed to detect ISA"
#endif

#if defined(__ppc64__) || defined(__aarch64__) || defined(_M_ARM64) || defined(_M_X64) || defined(__x86_64__) || defined(__x86_64)
	#define __RAD64__
	#define __RAD64REGS__  // also need to set this for platforms that have 64-bit regs but not 64-bit pointers
#endif

#if defined(__RADARM64__) && defined(_MSC_VER) && _MSC_VER < 1930
// Pre-VS2202 MSVC defaults to FP contractions on, we want this off
#pragma fp_contract(off)
#endif

#endif // __RADRR_PLATFORMH__

