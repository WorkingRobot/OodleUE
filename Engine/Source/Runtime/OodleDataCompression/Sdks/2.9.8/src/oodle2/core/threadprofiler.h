// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

/**

ThreadProfiler , main usage :

THREADPROFILESCOPE(label);
label must be a const string, eg :

THREADPROFILESCOPE("stuff");

this is a stub function pointer redirect that lets Oodle Core call to OodleX
OodleX installs the function pointers in OodleX_Init
they are NULL if ThreadProf is not enabled (eg. in client production use)

**/

#include "oodlebase.h"
#include "rrbase.h"

// toggle :
// BUILD_CONFIG_THREADPROFILER is on by default
//	but it does nothing unless the func pointers are installed by OodleX
#ifndef OODLE_BUILD_CONFIG_THREADPROFILER
#define OODLE_BUILD_CONFIG_THREADPROFILER 1
#endif

OODLE_NS_START

typedef UINTa	ThreadProfiler_Handle;

// this is the invalid handle value I return from Push/Pop :
#define TP_INVALID_HANDLE	((ThreadProfiler_Handle)-1)

// set by ThreadProfiler_Init :
extern ThreadProfiler_Handle	(OODLE_CALLBACK *fp_ThreadProfiler_Push)(const char * label,U64 guid);
extern void	(OODLE_CALLBACK *fp_ThreadProfiler_Pop)(ThreadProfiler_Handle handle);
extern void (OODLE_CALLBACK *fp_ThreadProfiler_Tag)(const char * label,U64 guid);

//==========================================================

#if OODLE_BUILD_CONFIG_THREADPROFILER

// Push then Pop with the handle to mark an interval
//	you can just Push and not Pop to make a zero-duration label <- @@ no not true any more,that screws up depth
//	NOTE : label should be a pointer to a const static !! like Push("hello");
//		we can hold the pointer a long time
//		and we check exact pointer equality (!! you might need to enable string pooling !!)
static RADINLINE ThreadProfiler_Handle	ThreadProfiler_Push(const char * label,U64 guid)
{
	if ( fp_ThreadProfiler_Push != NULL )
		return (*fp_ThreadProfiler_Push)(label,guid);
	else
		return TP_INVALID_HANDLE;
}

static RADINLINE void	ThreadProfiler_Pop(ThreadProfiler_Handle handle)
{
	if ( fp_ThreadProfiler_Pop != NULL )
		(*fp_ThreadProfiler_Pop)(handle);
		
}

// Tag is equivalent to _Pop( Push() )
static RADINLINE void ThreadProfiler_Tag(const char * label,U64 guid)
{
	if ( fp_ThreadProfiler_Tag != NULL )
		(*fp_ThreadProfiler_Tag)(label,guid);
}

#else // OODLE_BUILD_CONFIG_THREADPROFILER

#define ThreadProfiler_Push	ThreadProfiler_Push_Kill
#define ThreadProfiler_Pop	ThreadProfiler_Pop_Kill
#define ThreadProfiler_Tag	ThreadProfiler_Tag_Kill

static RADINLINE ThreadProfiler_Handle  ThreadProfiler_Push_Kill(const char * label,U64 guid) { return TP_INVALID_HANDLE; }
static RADINLINE void ThreadProfiler_Pop_Kill(ThreadProfiler_Handle handle) { }
static RADINLINE void ThreadProfiler_Tag_Kill(const char * label,U64 guid) { }

#endif // OODLE_BUILD_CONFIG_THREADPROFILER



#if OODLE_BUILD_CONFIG_THREADPROFILER

#ifdef __cplusplus
class ThreadProfileScope
{
public:
	explicit ThreadProfileScope(const char * label,U64 guid)
	{
		m_handle = ThreadProfiler_Push(label,guid);
	}
	
	~ThreadProfileScope()
	{
		ThreadProfiler_Pop(m_handle);
	}
	
	ThreadProfiler_Handle m_handle;
};

#define THREADPROFILESCOPE(label)		OODLE_NS_PRE ThreadProfileScope RR_NUMBERNAME(tps) (label,0)
#define THREADPROFILESCOPE2(label,guid)	OODLE_NS_PRE ThreadProfileScope RR_NUMBERNAME(tps) (label,guid)
#endif

#else

#define THREADPROFILESCOPE(label)	
#define THREADPROFILESCOPE2(label,guid)

#endif // OODLE_BUILD_CONFIG_THREADPROFILER

#define THREADPROFILEFUNC()	THREADPROFILESCOPE(RR_FUNCTION_NAME)	


OODLE_NS_END
