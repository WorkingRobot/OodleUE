// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(parent,OodleAPI_OodleCore_Plugins)
//idoc(end)
//#include "oodlecoreplugins.h"

/*********

The source files is "/src/oodlecoreplugins_src.cpp"

This is text-replaced to make oodlecoreplugins_gen & oodlenetplugins_gen
by make_plugins_src.bat  (called by autogen.bat)

The text-replacement is needed because these files are text-scanned to make the docs & public headers.

They are included into the builds via "oodlecoreplugins.cpp/h"

----------

The default on MSVC don't use stdio
because of the 2012-2015 link breakage
now that I'm strictly post-2015
I think that's okay again and I could use stdio on MSVC again

 -> changed 08/02/2020 , now letting MSVC do stdio
 -> changed back 10/15/2020 : removing MSVC stdio again
	while we are all post-2015 now
	there's still the whole /MT /MD thing

**********/

#include "rrlog.h"

OODLE_NS_START

//===============================================================
// OodleMalloc function pointers :
// default to clib :

// installed with OodleCore_Plugins_SetAllocators or from Oodle Ext

t_fp_OodleCore_Plugin_MallocAligned * g_fp_OodleCore_Plugin_MallocAligned = OodleCore_Plugin_MallocAligned_Default;
t_fp_OodleCore_Plugin_Free * g_fp_OodleCore_Plugin_Free = OodleCore_Plugin_Free_Default;

t_fp_OodleCore_Plugin_Printf * g_fp_OodleCore_Plugin_Printf = OodleCore_Plugin_Printf_Default;
t_fp_OodleCore_Plugin_DisplayAssertion * g_fp_OodleCore_Plugin_DisplayAssertion = OodleCore_Plugin_DisplayAssertion_Default;

t_fp_OodleCore_Plugin_RunJob *	g_fp_OodleCore_Plugin_RunJob = OodleCore_Plugin_RunJob_Default;
t_fp_OodleCore_Plugin_WaitJob *	g_fp_OodleCore_Plugin_WaitJob = OodleCore_Plugin_WaitJob_Default;
static int g_OodleCore_Plugin_job_target_parallelism = 0; // 0 means not set

//===============================================================

OOFUNC1 void OOFUNC2 OodleCore_Plugins_SetAllocators(
	t_fp_OodleCore_Plugin_MallocAligned * fp_OodleMallocAligned,
	t_fp_OodleCore_Plugin_Free * fp_OodleFree)
{
	g_fp_OodleCore_Plugin_MallocAligned = fp_OodleMallocAligned;
	g_fp_OodleCore_Plugin_Free = fp_OodleFree;
}


OOFUNC1 t_fp_OodleCore_Plugin_Printf * OOFUNC2 OodleCore_Plugins_SetPrintf(t_fp_OodleCore_Plugin_Printf * fp_rrRawPrintf)
{
	t_fp_OodleCore_Plugin_Printf * prev = g_fp_OodleCore_Plugin_Printf;
	g_fp_OodleCore_Plugin_Printf = fp_rrRawPrintf;
	return prev;
}

OOFUNC1 t_fp_OodleCore_Plugin_DisplayAssertion * OOFUNC2 OodleCore_Plugins_SetAssertion(t_fp_OodleCore_Plugin_DisplayAssertion * fp_rrDisplayAssertion)
{
	t_fp_OodleCore_Plugin_DisplayAssertion * prev = g_fp_OodleCore_Plugin_DisplayAssertion;
	g_fp_OodleCore_Plugin_DisplayAssertion = fp_rrDisplayAssertion;
	return prev;
}

OOFUNC1 void OOFUNC2 OodleCore_Plugins_SetJobSystem(
	t_fp_OodleCore_Plugin_RunJob * fp_RunJob,
	t_fp_OodleCore_Plugin_WaitJob * fp_WaitJob)
{
	rrPrintf_v2("OodleCore_Plugins_SetJobSystem is deprecated; use OodleCore_Plugins_SetJobSystemAndCount\n");

	// target_parallelism == 0 means "not set" , not "none"

	OodleCore_Plugins_SetJobSystemAndCount(fp_RunJob,fp_WaitJob,0);
}
	
OOFUNC1 void OOFUNC2 OodleCore_Plugins_SetJobSystemAndCount(
	t_fp_OodleCore_Plugin_RunJob * fp_RunJob,
	t_fp_OodleCore_Plugin_WaitJob * fp_WaitJob,
	int target_parallelism)
{
	if ( fp_RunJob == NULL )
	{
		// assume you're setting NULL to mean "not threaded"
		
		// fill in synchronous job runners :
		g_fp_OodleCore_Plugin_RunJob = OodleCore_Plugin_RunJob_Default;
		g_fp_OodleCore_Plugin_WaitJob = OodleCore_Plugin_WaitJob_Default;
		g_OodleCore_Plugin_job_target_parallelism = 0; // 0 means not set
	}
	else
	{
		g_fp_OodleCore_Plugin_RunJob = fp_RunJob;
		g_fp_OodleCore_Plugin_WaitJob = fp_WaitJob;
		g_OodleCore_Plugin_job_target_parallelism = target_parallelism;
	}
}
	
int OodleCore_Plugin_GetJobTargetParallelism()
{	
	if ( g_fp_OodleCore_Plugin_RunJob == OodleCore_Plugin_RunJob_Default )
		return 1;

	if ( g_OodleCore_Plugin_job_target_parallelism <= 0 )
	{
		// @@@@ NOT SET
		// default ?
		// we can't ask rrCpu, that's in Ext, and should have passed its number in
		//	  to SetJobSystemAndCount
		//	better to do too many than too few
		//	but this is ugly either way
		// the actual user of this right now is the BCN non-RDO block slice
		//	in which case we just want plenty
		return 32;
	}
	else
	{
		return g_OodleCore_Plugin_job_target_parallelism;
	}
}
	
OODLE_NS_END

//===============================================================
/**

Default implementations :

***/

#include "rrsprintf.h"

#include <stdlib.h>
#include <stdarg.h>

#ifndef _MSC_VER
// MSVC default plugin can't do stdio because of link incompatability
//	I want Oodle Core to be as compatible as possible, so sigh
#include <stdio.h>
#endif

#ifdef _MSC_VER

#define WIN32_LEAN_AND_MEAN 1

#include <windows.h>

#endif // _MSC_VER
	
#ifdef NACL_ANDROID
#include <malloc.h>
#endif

#include "oodlemalloc.h"

//===============================================================

/**

warning : on consoles normal malloc may run out of memory
use the OodleX malloc instead for large-memory runs

**/

OODLE_NS_START

OOFUNC1 void * OOFUNC2 OodleCore_Plugin_MallocAligned_Default(SINTa size,S32 alignment)
{
	RR_ASSERT( alignment >= OODLE_MALLOC_MINIMUM_ALIGNMENT );
	RR_ASSERT( OODLE_MALLOC_MINIMUM_ALIGNMENT >= sizeof(UINTa) );

	#ifdef _MSC_VER
	
	HANDLE heap = GetProcessHeap();
	
	SINTa allocBytes = size + (alignment-1) + sizeof(UINTa);
	
	char * allocPtr = (char *) HeapAlloc(heap,0,allocBytes);
	if ( allocPtr == NULL )
		return NULL;
		
	char * retPtr = rrAlignUpPointer( allocPtr + sizeof(UINTa), alignment );
	
	RR_ASSERT( rrIsAlignedPointer(retPtr,alignment) );
	RR_ASSERT( (retPtr + size) <= (allocPtr + allocBytes) );
	
	((void* *)retPtr)[-1] = allocPtr;

	return retPtr;
	
	#elif defined(__RADMACAPI__)
    
    // Mac doesn't have memalign
	//write our own memalign
	void *mem = malloc( size + (alignment-1) + sizeof(void*) );
	if(mem == 0)
	{
	  return 0;
	}

	char *alignedmem = ((char*)mem) + sizeof(void*);
	alignedmem += alignment - ((UINTa)alignedmem & (alignment - 1));

	((void**)alignedmem)[-1] = mem;
	return alignedmem;
        
	#else
	
	#ifndef _POSIX_C_SOURCE
	#define _POSIX_C_SOURCE 0
	#endif

	#ifndef _XOPEN_SOURCE
	#define _XOPEN_SOURCE 0
	#endif

	//#if _POSIX_ADVISORY_INFO > 0
	#if _POSIX_C_SOURCE >= 200112L || _XOPEN_SOURCE >= 600

	void * ret = NULL;
	posix_memalign(&ret,alignment,size);
	return ret;

	#else

	#ifdef __GNUC__
	
	return memalign(alignment,size);

	#else

	#error no __GNUC__
	
	#endif // GNUC
	#endif // posix
	#endif // platform
}

OOFUNC1 void OOFUNC2 OodleCore_Plugin_Free_Default(void * ptr)
{
	RR_ASSERT( ptr != NULL );
	
	#ifdef _MSC_VER
	
	HANDLE heap = GetProcessHeap();
	
	RR_ASSERT( rrIsAlignedPointer(ptr,OODLE_MALLOC_MINIMUM_ALIGNMENT) );
	
	void * allocPtr = ((void* *)ptr)[-1];

	RR_ASSERT( ((SINTa)ptr) >= ((SINTa)allocPtr) );	
	RR_ASSERT( (((SINTa)ptr) - ((SINTa)allocPtr)) <= 4096 );	
		
	HeapFree(heap,0,allocPtr);

	#elif defined(__RADMACAPI__)
        
	if( ptr )
	{
	  free(((void**)ptr)[-1]);
	}
    
    #else
	
	return free(ptr);
	
	#endif
}



OOFUNC1 void OOFUNC2 OodleCore_Plugin_Printf_Verbose(int verboseLevel,const char * file,int line,const char * fmt,...)
{
	(file);
	(line);
	(verboseLevel);

    va_list arg;
    va_start(arg,fmt);
    
	char buffer[4096];
	rrvsnprintf(buffer,sizeof(buffer),fmt,arg);

    va_end(arg);
	
	#ifdef _MSC_VER
    OutputDebugStringA(buffer);
    #else
    fputs(buffer,stdout);
    #endif
}

OOFUNC1 void OOFUNC2 OodleCore_Plugin_Printf_Default(int verboseLevel,const char * file,int line,const char * fmt,...)
{
	// default is to print verbosity <= 1 :
	if ( verboseLevel >= 2 )
		return;

	// NOTE : code dupe of above (OodleCore_Plugin_Printf_Verbose)
			
	(file);
	(line);

    va_list arg;
    va_start(arg,fmt);
    
	char buffer[4096];
	rrvsnprintf(buffer,sizeof(buffer),fmt,arg);

    va_end(arg);
	
	#ifdef _MSC_VER
    OutputDebugStringA(buffer);
    #else
    fputs(buffer,stdout);
    #endif
}

OOFUNC1 rrbool OOFUNC2 OodleCore_Plugin_DisplayAssertion_Default(const char * file,const int line,const char * function,const char * message)
{
	(function);
	
	#ifdef _MSC_VER
	
	char buffer[4096];
	rrsnprintf(buffer,sizeof(buffer),"%s (%d) : %s\n",file,line,message);
    OutputDebugStringA(buffer);
    
	#else
    fprintf(stderr,"%s (%d) : %s\n",file,line,message);
	#endif
	
	return true; // return true means now do a debug break
}

OOFUNC1 U64 OOFUNC2 OodleCore_Plugin_RunJob_Default( t_fp_Oodle_Job * fp_job, void * job_data, U64 * dependencies, int num_dependencies, void * user_ptr )
{
	// dependencies must all be done already
	// just run it synchronous :
	(*fp_job)(job_data);
	// no handle :
	return 0;
}

OOFUNC1 void OOFUNC2 OodleCore_Plugin_WaitJob_Default( U64 job_handle, void * user_ptr )
{
	// nothing to do
}


OODLE_NS_END


//idoc(begin)

//idoc(page,OodleAPI_OodleCore_Plugins)
/*

	Oodle2 Core needs to be able to call some system services.  This
	is how you provide the function pointers for Oodle2 Core to call out to
	the systems it needs.
	
	NOTE : OodleX installs these for you in $OodleX_Init.  If you use OodleX, then do not use these to install your own function pointers.  (if you install your own before OodleX_Init, yours will be replaced, and that's benign.  If you install your own after OodleX_Init, that will cause problems)
	(auto-install is for Core only - OodleX does not auto-install to Net or Texture.

	These APIs allow you to provide the system services to Oodle Core when you are NOT using OodleX.	
		
	$^TableOfContents
	
*/

//idoc(parent,OodleAPI_OodleCore_Plugins)
//idoc(page,OodleMalloc_About)
/*

	Oodle2 Core malloc

	When Oodle2 Core needs to allocate memory internally, it calls to function pointers provided by the client.

	If none are provided, the default is to call clib or system standard allocators (see $Oodle_About_Platforms for details on each platform).

	To install your own allocators, call $OodleCore_Plugins_SetAllocators

	The Oodle LZ decoders can be used in your runtime with no allocations done by Oodle at all.  So your shipping game run time may be able to run with no Oodle allocator at all if it only does decompression.  See $Oodle_FAQ_UseOodleWithNoAllocator

	The new compressors can encode without allocations if they are given sufficient scratch memory.  If they run out of scratch memory, they will call to the installed allocator.

	If you want to make sure Oodle is not calling any allocator internally, you can call $OodleCore_Plugins_SetAllocators with NULL.

	NOTE : if you are using Oodle Ext , do NOT install your own allocator in Oodle Core.  OodleX will install an allocator for Oodle Core automatically in $OodleX_Init.  Do not mix your own allocator with the OodleX allocator.  See $OodleXAPI_Malloc.

	Oodle does not handle malloc failures gracefully (eg. it will not just return from the function with an error code).  It will stop execution if it needs to allocate memory and can't continue.
	
*/
