// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

/***

SimpleProf

Core-side
minimal func pointer call through to Impl in Ext

basic usage is :
SIMPLEPROFILE_SCOPE(label)

label is a NON-quoted string, like :
SIMPLEPROFILE_SCOPE(Test)

****/

#ifndef __RADRR_SIMPLEPROFH__
#define __RADRR_SIMPLEPROFH__

#include "oodlebase.h"

// BUILD_CONFIG_SIMPLEPROFILER is now on by default everywhere
//	profile build not required
#ifndef OODLE_BUILD_CONFIG_SIMPLEPROFILER
#define OODLE_BUILD_CONFIG_SIMPLEPROFILER 1
#endif

OODLE_NS_START

/**

CB refactor 07-29-2020

no more explicit MakeIndex call, Push does it thread-safely on first use
"pindex" should point to a function static or global
initialize index to 0

eg.

static U32 myindex = 0;
(*fp_rrSimpleProf_Push)(&myindex,"mylabel");

if you need to pre-reserve an index you could still do so by just calling Push then Pop.

A function-local static that's zero-init like this :
static U32 myindex = 0;
has no overhead; it's just stored like a global, isn't init-on-first-use.

this ensures that when Prof is not Init, so fp_rrSimpleProf_Push is NULL, the overhead is very low indeed
it's just one load and test (inlined)

------

function pointers being non-NULL implies SimpleProf is Init'ed
fp_rrSimpleProf_Push

-----

NOTE : the SimpleProf_Init() and the setting of func pointers is NOT thread safe,
that must be done during single-threaded init phase.

The on-first-init index reservation in _Push *is* thread safe.

NOTE _Push does NOT look for label string matches (it could)
so for example if you have a template with a local static it will
instantiate the same label multiple times

**/
//---------------------------------------------------------

// set by SimpleProf_Init :
extern U64 (OODLE_CALLBACK *fp_rrSimpleProf_Push)(U32 * pindex, const char * label);
extern void (OODLE_CALLBACK *fp_rrSimpleProf_Pop)(U32 index, U64 push_time, int count);

//---------------------------------------------------------

#if OODLE_BUILD_CONFIG_SIMPLEPROFILER

static RADINLINE rrbool rrSimpleProf_IsEnabled() { return (fp_rrSimpleProf_Push != NULL); }

static RADINLINE U64 rrSimpleProf_Push(U32 * pindex, const char * label)
{
	if ( fp_rrSimpleProf_Push != NULL )
		return (*fp_rrSimpleProf_Push)(pindex,label);
	else
		return 0;
}

static RADINLINE void rrSimpleProf_Pop(U32 index, U64 push_time, int count)
{
	if ( fp_rrSimpleProf_Pop != NULL )
		(*fp_rrSimpleProf_Pop)(index,push_time,count);
}

#else

static RADINLINE rrbool rrSimpleProf_IsEnabled() { return false; }
static RADINLINE U64 rrSimpleProf_Push(U32 * pindex, const char * label) { return 0; }
static RADINLINE void rrSimpleProf_Pop(U32 index, U64 push_time, int count) { }

#endif

#if 0
// could offer this if needed :
static RADINLINE void rrSimpleProf_ReserveIndex(U32 * pindex, const char * label)
{
	U64 t = rrSimpleProf_Push(pindex,label);
	rrSimpleProf_Pop(*pindex,t,0);
	// does add 1 to callCount incorrectly
}
#endif

//---------------------------------------------------------

class rrScopeProfiler
{
public:
	explicit rrScopeProfiler(U32 * pindex, const char * label,int count) : m_count(count)
	{
		m_start = rrSimpleProf_Push(pindex,label);
		m_index = *pindex;
	}
	
	~rrScopeProfiler()
	{
		rrSimpleProf_Pop(m_index,m_start,m_count);
	}
	
	U32 m_index;
	int m_count;
	U64 m_start;
};

//---------------------------------------------------------

// NOTE :
//  local static caching used for MakeIndex is not thread safe!
//	(well, it is on new compilers, but then it adds a spin-lock!)
// - kind of irrelevant because the whole thing is not thread safe

#if OODLE_BUILD_CONFIG_SIMPLEPROFILER

// profile scope with item count
#define SIMPLEPROFILE_SCOPE_N(label,count)	\
	static U32 RR_STRING_JOIN(label,_prof_index) = 0; \
	::OODLE_NS::rrScopeProfiler RR_STRING_JOIN(label,_scoper) ( &(RR_STRING_JOIN(label,_prof_index)), RR_STRINGIZE(label) , (int)( count ) );

// SIMPLEPROFILE_SCOPE_SETCOUNT changes count within the current scope
//	useful when you don't know count until somewhere within the scope
#define SIMPLEPROFILE_SCOPE_SETCOUNT(label,count)	RR_STRING_JOIN(label,_scoper).m_count = (int)( count )

// basic scope :
#define SIMPLEPROFILE_SCOPE(label)	SIMPLEPROFILE_SCOPE_N(label,1)

//--------

// profile scope with explicit string : NOTE MUST BE A CONST STRING
//	eg. SIMPLEPROFILE_SCOPE_STRING("hello",1)
#define SIMPLEPROFILE_SCOPE_STRING(string,count)	\
	static U32 prof_index = 0; \
	::OODLE_NS::rrScopeProfiler prof_scoper( &prof_index, string, (int)( count ) );

// @@ EXPERIMENTAL
//	do I like something like this for a general purpose SIMPLEPROFILE_SCOPE_INDEXED ?
// the labels[] is static function local so it's made for every template instance
//	alternative : explicitly make global array and use SIMPLEPROFILE_SCOPE_STRING or manual scoper

#define SIMPLEPROFILE_SCOPE_INDEXED(base,idx,N,count) \
	static const char * RR_STRING_JOIN(base,labels) [N] = { RR_MAKE_INDEXED_STRING_ARRAY( RR_STRINGIZE(base), N ) }; \
	RR_ASSERT( (idx) < (N) ); \
	SIMPLEPROFILE_SCOPE_STRING(RR_STRING_JOIN(base,labels)[idx],count);

#else // OODLE_BUILD_CONFIG_SIMPLEPROFILER off

//#define PROFILE_SCOPE_NF(count)			
#define SIMPLEPROFILE_SCOPE_N(label,count)	
#define SIMPLEPROFILE_SCOPE(label)			
#define SIMPLEPROFILE_SCOPE_SETCOUNT(label,count)

#define rrSimpleProf_IsEnabled()	(false)

#define SIMPLEPROFILE_SCOPE_STRING(string,count)
#define SIMPLEPROFILE_SCOPE_INDEXED(base,idx,N,count)

#endif  // OODLE_BUILD_CONFIG_SIMPLEPROFILER

//---------------------------------------------------------

OODLE_NS_END

#endif // __RADRR_SIMPLEPROFH__
