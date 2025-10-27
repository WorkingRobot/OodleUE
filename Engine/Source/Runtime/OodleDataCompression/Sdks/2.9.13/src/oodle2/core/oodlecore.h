// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once
#ifndef __OODLE_CORE_H__
#define __OODLE_CORE_H__

//===================
//
// OodleCore
// lower level than OodleBase
//
// should be mostly #defines, eg. does not create a dependency on oodle.lib 
//
//=======================

#include "rrbase.h"
#include "oodlepublicate.h"
#include "../base/oo2baseinc.h"

#if ! defined(OODLE_BUILDING_DATA) && ! defined(OODLE_BUILDING_NETWORK) && ! defined(OODLE_BUILDING_TEXTURE) && ! defined(OODLE_BUILDING_TEXTURERT)
#error no OODLE_BUILDING_ defined
#endif 

#ifdef _MSC_VER
// warning C4505: unreferenced local function has been removed
#pragma warning(disable : 4505)
#endif

//===================================================
// my platform defines :

/**

OODLE_PLATFORM_LARGEMEMORY means virtual memory
	it enables the suffix trie for optimal parse levels
	it sets OODLE_PLATFORM_HAS_ADVANCED_MATCHERS
	(optimal1 CTMF is also turned on by OODLE_PLATFORM_HAS_ADVANCED_MATCHERS, as is LRM)
	
OODLE_PLATFORM_SMALLMEMORY means be very small
	eg. embedded, phones
	SMALLMEMORY caps the size of the LZ encoder hash table
	
You can be "medium" and set neither.

OODLE_PLATFORM_FULLCONIO means you can do console in & out (getc loops)
	if you just have stdout printf and no input, do not set FULLCONIO

**/

#if defined(OODLE_PLATFORM_LARGEMEMORY) && defined(OODLE_PLATFORM_SMALLMEMORY) && defined(OODLE_PLATFORM_FULLCONIO)
// Nothing to do!
#elif defined(__RADANDROID__) || defined(__RADIPHONE__) || defined(__RADEMSCRIPTEN__)
#define OODLE_PLATFORM_LARGEMEMORY	0
#define OODLE_PLATFORM_SMALLMEMORY	1
#define OODLE_PLATFORM_FULLCONIO	0
// check for RADNT must be LAST since it's also set for other targets:
#elif defined(__RADNT__) || defined(__RADMAC__) || defined(__RADLINUX__)
#define OODLE_PLATFORM_LARGEMEMORY	1
#define OODLE_PLATFORM_SMALLMEMORY	0
#define OODLE_PLATFORM_FULLCONIO	1	// has console in & out
#elif defined(__RADWINRT__) // WinRT = UWP , could be desktop, phone, console
#define OODLE_PLATFORM_LARGEMEMORY	1
#define OODLE_PLATFORM_SMALLMEMORY	0
#define OODLE_PLATFORM_FULLCONIO	1
#else
#error bad Oodle platform
#endif

#if defined(OODLE_PLATFORMDEF_CLASS_PC) || defined(OODLE_PLATFORMDEF_CLASS_CONSOLE) || defined(OODLE_PLATFORMDEF_CLASS_EMBEDDED)
// Nothing to do!
#elif defined(__RADNT__) || defined(__RADMAC__) || defined(__RADLINUX__)
#define OODLE_PLATFORMDEF_CLASS_PC			1
#elif defined(__RADWINRT__)
#define OODLE_PLATFORMDEF_CLASS_CONSOLE		1
#elif defined(__RADANDROID__) || defined(__RADIPHONE__) || defined(__RADEMSCRIPTEN__)
#define OODLE_PLATFORMDEF_CLASS_EMBEDDED	1
#else
#error bad Oodle platform
#endif

// OODLE_PLATFORM_HAS_ADVANCED_MATCHERS must match "$hasoptimals" in cdep
// OODLE_PLATFORM_HAS_ADVANCED_MATCHERS could be turned on for the newer consoles
//		but I've disabled it to reduce code size
//		seems rare that you actually want it there

#if OODLE_PLATFORM_LARGEMEMORY
// all PC-types and the newer consoles get good matchers -
#define OODLE_PLATFORM_HAS_ADVANCED_MATCHERS	1
#else
// no SuffixArray or SuffixTrie on limited-memory targets
#define OODLE_PLATFORM_HAS_ADVANCED_MATCHERS	0
#endif

//=========================================================

#ifdef __RAD64__
#define RR_SIZEOF_UINTA	(8)
#else
#define RR_SIZEOF_UINTA	(4)
#endif

#ifdef __RAD64REGS__
#define RR_SIZEOF_UINTR	(8)
#else
#define RR_SIZEOF_UINTR	(4)
#endif

//===================================================

// oodle in namespace :
// same as RR_NAMESPACE
//	oo2,oo2net,oo2tex

#ifndef RR_NAMESPACE
#error RR_NAMESPACE
#endif

#define OODLE_NS		RR_NAMESPACE
#define OODLE_NS_START	namespace OODLE_NS {
#define OODLE_NS_END	};
#define OODLE_NS_PRE	::OODLE_NS::

// OODLE_NS changes to oo2,tex,net
//	OODLEX_NS is always oo2
#define OODLEX_NS		oo2
#define OODLEX_NS_START	namespace OODLEX_NS {
#define OODLEX_NS_END	};
#define OODLEX_NS_PRE	::OODLEX_NS::

//===================================================

#define OOINLINE	static RADINLINE

// make errors on purpose :
//#define OODLE_CALLBACK	__fastcall
//#define OODLE_CALLBACK	__cdecl

#define NOOFUNC1
#define NOOFUNC2


#endif // __OODLE_CORE_H__
