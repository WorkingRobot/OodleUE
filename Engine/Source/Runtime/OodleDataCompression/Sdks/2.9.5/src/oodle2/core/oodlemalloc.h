// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(parent,OodleAPI_OodleCore_Plugins)
#pragma once

#include "rrbase.h"
#include "rrmath.h" // for align checks
#include "oodlecore.h"  // for OODLE_NS_START
#include "oodlecoreplugins.h"  // for OODLE_NS_START

OODLE_NS_START

//===========================================
// NOT PUB :
//  use OodleXMalloc to call publicly

// Oodle_Core_Malloc_Failed never returns
void Oodle_Core_Malloc_Failed( SINTa bytes );

static RADINLINE void * OodleMalloc( SINTa bytes )
{
	RR_ASSERT( g_fp_OodlePlugin_MallocAligned != NULL );
	void * ret_memory = (*g_fp_OodlePlugin_MallocAligned)(bytes,OODLE_MALLOC_MINIMUM_ALIGNMENT);
	// don't pass null out to caller :
	if ( ret_memory == NULL )
		Oodle_Core_Malloc_Failed(bytes);
	return ret_memory;
}

static RADINLINE void * OodleMallocAligned( SINTa bytes , S32 alignment )
{
	RR_ASSERT( g_fp_OodlePlugin_MallocAligned != NULL );
	RR_ASSERT( alignment >= OODLE_MALLOC_MINIMUM_ALIGNMENT );
	RR_ASSERT( rrIsPow2(alignment) );
	// if g_fp_OodlePlugin_MallocAligned is NULL in release mode we just hard crash ??
	//	that's not a recommended usage, you should set g_fp_OodlePlugin_MallocAligned
	//	to something that logs error and has a fallback malloc
	void * ret_memory = (*g_fp_OodlePlugin_MallocAligned)(bytes,alignment);
	// don't pass null out to caller :
	if ( ret_memory == NULL )
		Oodle_Core_Malloc_Failed(bytes);
	return ret_memory;
}

static RADINLINE void OodleFree(void * ptr)
{
	RR_ASSERT( g_fp_OodlePlugin_Free != NULL );
	(*g_fp_OodlePlugin_Free)(ptr);
}

static RADINLINE void OodleFreeSized(void * ptr, SINTa bytes)
{
	RR_ASSERT( g_fp_OodlePlugin_Free != NULL );
	RR_UNUSED_VARIABLE(bytes);
	(*g_fp_OodlePlugin_Free)(ptr);
}

OODLE_NS_END

//===========================================

#define OODLE_MALLOC_ARRAY(type,count)	(type *) OodleMalloc(sizeof(type)*(count))
#define OODLE_MALLOC_ONE(type)			OODLE_MALLOC_ARRAY(type,1)

#define OODLE_FREE_ARRAY(ptr,count)		OodleFreeSized(ptr,sizeof(*ptr)*(count))
#define OODLE_FREE_ONE(ptr)				OODLE_FREE_ARRAY(ptr,1)

#define OODLE_MALLOC_ARRAY_ALIGNED(type,count,alignment)		(type *) OodleMallocAligned( RR_MAX(sizeof(type)*(count),alignment) ,alignment)
#define OODLE_MALLOC_ONE_ALIGNED(type,alignment)				OODLE_MALLOC_ARRAY_ALIGNED(type,1,alignment)

#define OODLE_MALLOC_ARRAY_CACHEALIGNED(type,count)		(type *) OodleMallocAligned( RR_MAX(sizeof(type)*(count),RR_CACHE_LINE_SIZE) ,RR_CACHE_LINE_SIZE)

//===========================================
