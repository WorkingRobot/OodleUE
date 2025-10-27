// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrlog.h"

#include <stdarg.h>
#include <stdlib.h>

// implementation is here :
#ifdef _MSC_VER
// Jeff's code is full of 4244's :
#pragma warning(push)
#pragma warning(disable : 4244)
#endif
#define RR_SPRINTF_IMPLEMENTATION
#include "rrsprintf.h"
#ifdef _MSC_VER
#pragma warning(pop)
#endif

//---------------------

OODLE_NS_START


#ifdef __RADFINAL__
#ifndef RR_NO_LOG
#error FINAL = NO_LOG
#endif
#endif

//--------------------------------------------------
// rrOutputDebugString:

#ifdef RR_NO_LOG

void rrOutputDebugString(const char * str)
{
}

#elif defined(__RADWIN__)

#include <windows.h>

void rrOutputDebugString(const char * str)
{
    OutputDebugStringA(str);
}

#elif defined(__RADANDROID__)

#include <android/log.h>

void rrOutputDebugString(const char * str)
{
    __android_log_print(ANDROID_LOG_INFO, "radstdout", "%s", str);
}

#else

void rrOutputDebugString(const char * str)
{
	// no special debug output unless otherwise noted, generally we just print to stdout
}

#endif

//--------------------------------------------------


/*
void RADLINK rrRawPrintf_Nop(int verboseLevel,const char * file,int line,const char * fmt,...)
{
	RR_UNUSED_VARIABLE(verboseLevel);
	RR_UNUSED_VARIABLE(file);
	RR_UNUSED_VARIABLE(line);
	RR_UNUSED_VARIABLE(fmt);
}
*/

void rrLog_Disable()
{
	g_fp_OodlePlugin_Printf = NULL;
}

//=======================================

OODLE_NS_END
