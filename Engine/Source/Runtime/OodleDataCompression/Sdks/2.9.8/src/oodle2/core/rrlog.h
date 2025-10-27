// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlecore.h"
#include "oodlecoreplugins.h"
#include "rrbase.h"
#define __STDC_FORMAT_MACROS
#include <inttypes.h>

OODLE_NS_START

/**

CB : new rrLog 08/10/11 :

rrLog is just a header that provides rrPrintf

There is no implementation here, it just calls to a func pointer (g_fp_OodlePlugin_Printf) if not null.

rrLog supports both compile-time and run-time verbose level settings.  The idea is :

v0 : important information, usually on
v1 : detailed information, rarely on
v2 : very detailed information, for debugging

By setting the compile-time verbose level (RR_LOG_VERBOSE_LEVEL) the higher verbosity prints can
get compiled out completely to save code size.

So for example if RR_LOG_VERBOSE_LEVEL is 1, then v2 prints are compiled out and can't be turned on
at runtime.  Then if you run with verboseLevel = 0 , the v1 prints also won't be shown (*).

(* = but this is up to the implementation, which is not included here)

**/

// builder may set RR_NO_LOG if they like

#if defined(__RADFINAL__)
#define RR_NO_LOG
#endif

// compile time verbose level must be set :
#ifndef RR_LOG_VERBOSE_LEVEL
// set RR_LOG_VERBOSE_LEVEL if not set yet
#if ! defined(_DEBUG) && defined(CDEP) // release build from cdep :
#define RR_LOG_VERBOSE_LEVEL	1	// no v2
#else
#define RR_LOG_VERBOSE_LEVEL	2	// all
#endif
#endif // RR_LOG_VERBOSE_LEVEL

#ifdef RR_NO_LOG
#undef RR_LOG_VERBOSE_LEVEL
#define RR_LOG_VERBOSE_LEVEL	-1	// none
#endif

//-----------------------------------------------------------------------

void rrLog_Disable();

void rrOutputDebugString(const char * str);

//-----------------------------------------------------------------------

//#define rrPrintf_v(verboseLevel,...)		do { if ( g_fp_OodlePlugin_Printf != NULL ) (*g_fp_OodlePlugin_Printf)(verboseLevel,__FILE__,__LINE__,##__VA_ARGS__); } while(0)
#define rrPrintf_v(verboseLevel,...)		do { if ( ::RR_NAMESPACE::g_fp_OodlePlugin_Printf != NULL ) (*::RR_NAMESPACE::g_fp_OodlePlugin_Printf)(verboseLevel,__FILE__,__LINE__,##__VA_ARGS__); } while(0)

#if RR_LOG_VERBOSE_LEVEL >= 2

#define rrPrintf_v0(...)	rrPrintf_v(0,##__VA_ARGS__)
#define rrPrintf_v1(...)	rrPrintf_v(1,##__VA_ARGS__)
#define rrPrintf_v2(...)	rrPrintf_v(2,##__VA_ARGS__)

#elif RR_LOG_VERBOSE_LEVEL >= 1

#define rrPrintf_v0(...)	rrPrintf_v(0,##__VA_ARGS__)
#define rrPrintf_v1(...)	rrPrintf_v(1,##__VA_ARGS__)
#define rrPrintf_v2(...)	::RR_NAMESPACE::rrPrintf_nop()

#elif RR_LOG_VERBOSE_LEVEL >= 0

#define rrPrintf_v0(...)	rrPrintf_v(0,##__VA_ARGS__)
#define rrPrintf_v1(...)	::RR_NAMESPACE::rrPrintf_nop()
#define rrPrintf_v2(...)	::RR_NAMESPACE::rrPrintf_nop()

#else // no log

#define rrPrintf_v0(...)	::RR_NAMESPACE::rrPrintf_nop()
#define rrPrintf_v1(...)	::RR_NAMESPACE::rrPrintf_nop()
#define rrPrintf_v2(...)	::RR_NAMESPACE::rrPrintf_nop()

#endif // RR_LOG_VERBOSE_LEVEL

// unqualified printf is verbose level 1 :
#define rrPrintf	rrPrintf_v1

// alias to all low case ? - todo : kill this (?)
#define rrprintf	rrPrintf

//=================================================================================================

// to kill printfs easily in a file
//	you can do #undef rrPrintf\n #define rrPrintf rrPrintfKill
static RADINLINE void rrPrintfKill(const char * fmt,...) { RR_UNUSED_VARIABLE(fmt); }
static RADINLINE void rrPrintfKill_v(int verboseLevel,...) { RR_UNUSED_VARIABLE(verboseLevel); }
static RADINLINE void rrPrintf_nop() { }

//-----------------------------------

// helper for 64 bit formatting :
#define RR_S64_FMT "%" PRId64
#define RR_U64_FMT "%" PRIu64
#define RR_SINTa_FMT "%" PRIdPTR
#define RR_UINTa_FMT "%" PRIuPTR
#define RR_64_FMT_HEX "%016" PRIX64
#ifdef __RAD64__
#define RR_UINTa_FMT_HEX "%016" PRIXPTR
#else
#define RR_UINTa_FMT_HEX "%08" PRIXPTR
#endif

//-----------------------------------------


OODLE_NS_END
