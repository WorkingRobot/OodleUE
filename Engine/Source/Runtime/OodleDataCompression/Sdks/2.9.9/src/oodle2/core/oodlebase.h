// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma	once
#ifndef __OODLE_BASE_H__
#define __OODLE_BASE_H__

/*********
 oodle base

	like rrbase ; the standard first include
	higher level than oodlecore.h

CONFUSINGLY this has nothing to do with the public "oodle2base.h"
 that's made from the base/ dir

***********/

#include <stdlib.h>
#include <string.h> // memset

#include "oodlecore.h"
#include "rrlog.h"

//-------------------------------------------------------------------------------------

OODLE_NS_START

//-------------------------------------------------------------------------------------

#ifdef __RAD64__

RADINLINE SINTa oo64toA( S64 s ) { return s; }

#else

SINTa oo64toA( S64 s );

#endif

//-------------------------------------------------------------------------------------
// use ooLogError to log errors in Oodle

extern t_OodleFPVoidVoid * fp_ooLogErrorPre;
extern t_OodleFPVoidVoid * fp_ooLogErrorPost;

// ooLogError string should have a \n on it

void ooLogErrorPre();
void ooLogErrorPost();

//#define ooLogError(...)       ooLogErrorPre() , rrPrintf_v0("OODLE ERROR : " __VA_ARGS__) , ooLogErrorPost()

#define ooLogError(...)       do { ooLogErrorPre(); rrPrintf_v0("OODLE ERROR : " __VA_ARGS__); ooLogErrorPost(); } while(0)

//-------------------------------------------------------------------------------------

//! Disallows the compiler defined default ctor
#define OODLE_FORBID_DEFAULT_CTOR(x) x()

//! Disallows the compiler defined copy ctor
#define OODLE_FORBID_COPY_CTOR(x)    x(const x&)

//! Disallows the compiler defined assignment operator
#define OODLE_FORBID_ASSIGNMENT(x)   void operator=(const x&)

#define OODLE_FORBID_CLASS_STANDARDS(x)	\
	OODLE_FORBID_ASSIGNMENT(x);	\
	OODLE_FORBID_COPY_CTOR(x)	

#define OODLE_MAKE_COMPARISONS_FROM_LESS_AND_EQUALS(this_type)	\
	bool operator <= (const this_type & other) const { return ! ( other < *this ); } \
	bool operator >= (const this_type & other) const { return ! ( *this < other ); } \
	bool operator >  (const this_type & other) const { return ( other < *this ); }	\
	bool operator != (const this_type & other) const { return ! ( *this == other ); }

// OODLE_ZERO currently uses memset
//	could use special rrMemSetZero in rrMemUtil

//#define OODLE_ZERO_CHECKED(obj)
// OODLE_ZERO_CHECKED(obj) fails if type is not POD
//	to intentionally zero non-POD types use OODLE_ZERO_NOCHECK
#define OODLE_ZERO_NOCHECK(obj)		memset(&(obj),0,sizeof(obj))

#ifdef __cplusplus

#undef RR_ARRAY_SIZE

template<typename T, UINTa N> char (&ArrayCountObj(const T (&)[N]))[N];
#define RR_ARRAY_SIZE(arr)    (sizeof(OODLE_NS_PRE ArrayCountObj(arr)))

template <typename T>
void CheckedZero(T * ptr)
{
	const T zero = { }; // will fail compile if T has constructors
	zero;
	memset(ptr,0,sizeof(T));
}

#define OODLE_ZERO_CHECKED(obj)	OODLE_NS::CheckedZero(&(obj))

#endif

//=========================================================

const char * Oodle_PlatformDesc();

OODLE_NS_END

// set here :
//#define OODLE_BUILD_CONFIG_TRACE_ALL_CALLS	1

#ifndef OODLE_BUILD_CONFIG_TRACE_ALL_CALLS
#define OODLE_BUILD_CONFIG_TRACE_ALL_CALLS 0
#endif

#if OODLE_BUILD_CONFIG_TRACE_ALL_CALLS

#include "threadprofiler.h"

#undef OOFUNCSTART
// OOFUNCSTART is not followed by parens or semicolon
#define OOFUNCSTART		THREADPROFILEFUNC();

#endif


#endif // __OODLE_BASE_H__
