// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(parent,OodleAPI_OodleCore_Plugins)
#pragma once

#include "oodlecore.h"

OODLE_NS_START

PUBSTART

// function pointers to mallocs needed :

IDOC OODEFFUNC typedef void * (OODLE_CALLBACK t_fp_OodleCore_Plugin_MallocAligned)( OO_SINTa bytes, OO_S32 alignment);
/* Function pointer type for OodleMallocAligned

	$:bytes		number of bytes to allocate
	$:alignment	required alignment of returned pointer
	$:return	pointer to memory allocated (must not be NULL)

	_alignment_ will always be a power of two

	_alignment_ will always be >= $OODLE_MALLOC_MINIMUM_ALIGNMENT

*/

IDOC OODEFFUNC typedef void (OODLE_CALLBACK t_fp_OodleCore_Plugin_Free)( void * ptr );
/* Function pointer type for OodleFree

	$:ptr	pointer to memory to free

*/

IDOC OOFUNC1 void OOFUNC2 OodleCore_Plugins_SetAllocators(
	t_fp_OodleCore_Plugin_MallocAligned * fp_OodleMallocAligned,
	t_fp_OodleCore_Plugin_Free * fp_OodleFree);
/* Set the function pointers for allocations by Oodle.

	If these are not set, the default implementation on most platforms uses the C stdlib.
	On Microsoft platforms the default implementation uses HeapAlloc.

	These must not be changed once they are set!  Set them once then don't change them.

	NOTE: if you are using Oodle Ext, do NOT call this.  OodleX_Init will install an allocator for Oodle Core.  Do not mix your own allocator with the OodleX allocator.  See $OodleXAPI_Malloc.

	If you want to ensure that Oodle is not doing any allocations, you can call OodleCore_Plugins_SetAllocators(NULL,NULL);
	If you do that, then any time Oodle needs to allocate memory internally, it will stop the process.
	It is STRONGLY not recommended that you ship that way.  You can verify that Oodle is not allocating, but then leave some
	fallback allocator installed when you actually ship just in case.

	Also note that on many consoles the standard allocation practices may not
	leave much heap memory for the C stdlib malloc.  In this case Oodle may fail to allocate.

*/

IDOC OODEFFUNC typedef OO_U64 (OODLE_CALLBACK t_fp_OodleCore_Plugin_RunJob)( t_fp_Oodle_Job * fp_job, void * job_data , OO_U64 * dependencies, int num_dependencies, void * user_ptr );
/* Function pointer type for OodleCore_Plugins_SetJobSystem

	$:dependencies		array of handles of other pending jobs. All guaranteed to be nonzero.
	$:num_dependencies	number of dependencies. Guaranteed to be no more than OODLE_JOB_MAX_DEPENDENCIES.
    $:user_ptr			is passed through from the OodleLZ_CompressOptions.
	$:return			handle to the async job, or 0 if it was run synchronously

	RunJob will call fp_job(job_data)

	it may be done on a thread, or it may run the function synchronously and return 0, indicating the job is already done.
	The returned OO_U64 is a handle passed to WaitJob, unless it is 0, in which case WaitJob won't get called.

	fp_job should not run until all the dependencies are done.  This function should not delete the dependencies.

	RunJob must be callable from within an Oodle Job, i.e. jobs may spawn their own sub-jobs directly.
	However, the matching WaitJob calls will only ever occur on the thread that called the
	internally threaded Oodle API function.

	See $Oodle_About_Job_Threading_Plugins
*/

IDOC OODEFFUNC typedef void (OODLE_CALLBACK t_fp_OodleCore_Plugin_WaitJob)( OO_U64 job_handle, void * user_ptr );
/* Function pointer type for OodleCore_Plugins_SetJobSystem

	$:job_handle	a job handle returned from RunJob. Never 0.
    $:user_ptr      is passed through from the OodleLZ_CompressOptions or OodleTex_Encode call.

	Waits until the job specified by job_handle is done and cleans up any associated resources. Oodle
	will call WaitJob exactly once for every RunJob call that didn't return 0.

	If job_handle was already completed, this should clean it up without waiting.

	A handle value should not be reused by another RunJob until WaitJob has been done with that value.

	WaitJob will not be called from running jobs.  It will be only be called from the original thread that
	invoked Oodle.  If you are running Oodle from a worker thread, ensure that that thread is allowed to wait
	on other job threads.

	See $Oodle_About_Job_Threading_Plugins
*/

IDOC OOFUNC1 void OOFUNC2 OodleCore_Plugins_SetJobSystem(
	t_fp_OodleCore_Plugin_RunJob * fp_RunJob,
	t_fp_OodleCore_Plugin_WaitJob * fp_WaitJob);
/* DEPRECATED use OodleCore_Plugins_SetJobSystemAndCount instead

	See $OodleCore_Plugins_SetJobSystemAndCount
*/


IDOC OOFUNC1 void OOFUNC2 OodleCore_Plugins_SetJobSystemAndCount(
	t_fp_OodleCore_Plugin_RunJob * fp_RunJob,
	t_fp_OodleCore_Plugin_WaitJob * fp_WaitJob,
	int target_parallelism);
/* Set the function pointers for async job system used by Oodle.

	$:fp_RunJob		pointer to RunJob function
	$:fp_WaitJob	pointer to WaitJob function
	$:target_parallelism	goal of number of jobs to run simultaneously

	If these are not set, the default implementation runs jobs synchronously on the calling thread.

	These must not be changed once they are set!  Set them once then don't change them.

	_target_parallelism_ allows you to tell Oodle how many Jobs it should try to keep in flight at once.
	Depending on the operation it may not be able to split work into this many jobs (so fewer will be used),
	but it will not exceed this count.

	For Oodle Data LZ work, typically _target_parallelism_ is usually best at the number of hardware cores
	not including hyper threads).

	For Oodle Texture BCN encoding work, _target_parallelism_ is usually best as the full number of hyper cores.

	In some cases you may wish to reduce _target_parallelism_ by 1 or 2 cores to leave some of the CPU free for
	other work.

	For example on a CPU with 16 cores and 32 hardware threads, for LZ work you might set _target_parallelism_ to 15
	when calling OodleCorePlugins.  For BC7 encoding you might set _target_parallelism_ to 30 when calling OodleTexPlugins.

	NOTE : if you are using Oodle Ext, do NOT call this.  OodleX_Init will install a job system for Oodle Core.
	Note OodleX only installs automatically to Oodle Core, not Net or Tex.  See example_jobify.cpp for manual
	plugin.

	Replaces deprecated $OodleCore_Plugins_SetJobSystem

	See $Oodle_About_Job_Threading_Plugins
*/

// the main func pointer for log :
IDOC OODEFFUNC typedef void (OODLE_CALLBACK t_fp_OodleCore_Plugin_Printf)(int verboseLevel,const char * file,int line,const char * fmt,...);
/* Function pointer to Oodle Core printf

	$:verboseLevel	verbosity of the message; 0-2 ; lower = more important
	$:file			C file that sent the message
	$:line			C line that sent the message
	$:fmt			vararg printf format string

	The logging function installed here must parse varargs like printf.

	_verboseLevel_ may be used to omit verbose messages.
*/

IDOC OOFUNC1 t_fp_OodleCore_Plugin_Printf * OOFUNC2 OodleCore_Plugins_SetPrintf(t_fp_OodleCore_Plugin_Printf * fp_rrRawPrintf);
/* Install the callback used by Oodle Core for logging

	$:fp_rrRawPrintf	function pointer to your log function; may be NULL to disable all logging
	$:return			returns the previous function pointer

	Use this function to install your own printf for Oodle Core.

	The default implementation in debug builds, if you install nothing, uses the C stdio printf for logging.
	On Microsoft platforms, it uses OutputDebugString and not stdio.

	To disable all logging, call OodleCore_Plugins_SetPrintf(NULL)

	WARNING : this function is NOT thread safe!  It should be done only once and done in a place where the caller can guarantee thread safety.

	In the debug build of Oodle, you can install OodleCore_Plugin_Printf_Verbose to get more verbose logging

*/

IDOC OODEFFUNC typedef OO_BOOL (OODLE_CALLBACK t_fp_OodleCore_Plugin_DisplayAssertion)(const char * file,const int line,const char * function,const char * message);
/* Function pointer to Oodle Core assert callback

	$:file			C file that triggered the assert
	$:line			C line that triggered the assert
	$:function		C function that triggered the assert (may be NULL)
	$:message		assert message
	$:return		true to break execution at the assertion site, false to continue

	This callback is called by Oodle Core when it detects an assertion condition.

	This will only happen in debug builds.


*/

IDOC OOFUNC1 t_fp_OodleCore_Plugin_DisplayAssertion * OOFUNC2 OodleCore_Plugins_SetAssertion(t_fp_OodleCore_Plugin_DisplayAssertion * fp_rrDisplayAssertion);
/* Install the callback used by Oodle Core for asserts

	$:fp_rrDisplayAssertion	function pointer to your assert display function
	$:return			returns the previous function pointer

	Use this function to install your own display for Oodle Core assertions.
	This will only happen in debug builds.

	The default implementation in debug builds, if you install nothing, uses the C stderr printf for logging,
	except on Microsoft platforms where it uses OutputDebugString.

	WARNING : this function is NOT thread safe!  It should be done only once and done in a place where the caller can guarantee thread safety.

*/

//=============================================================


OOFUNC1 void * OOFUNC2 OodleCore_Plugin_MallocAligned_Default(OO_SINTa size,OO_S32 alignment);
OOFUNC1 void OOFUNC2 OodleCore_Plugin_Free_Default(void * ptr);
OOFUNC1 void OOFUNC2 OodleCore_Plugin_Printf_Default(int verboseLevel,const char * file,int line,const char * fmt,...);
OOFUNC1 void OOFUNC2 OodleCore_Plugin_Printf_Verbose(int verboseLevel,const char * file,int line,const char * fmt,...);
OOFUNC1 OO_BOOL OOFUNC2 OodleCore_Plugin_DisplayAssertion_Default(const char * file,const int line,const char * function,const char * message);
OOFUNC1 OO_U64 OOFUNC2 OodleCore_Plugin_RunJob_Default( t_fp_Oodle_Job * fp_job, void * job_data, OO_U64 * dependencies, int num_dependencies, void * user_ptr );
OOFUNC1 void OOFUNC2 OodleCore_Plugin_WaitJob_Default( OO_U64 job_handle, void * user_ptr );

//=============================================================

PUBEND

// not pub :

//=============================================================

extern t_fp_OodleCore_Plugin_MallocAligned * g_fp_OodleCore_Plugin_MallocAligned;
extern t_fp_OodleCore_Plugin_Free * g_fp_OodleCore_Plugin_Free;
extern t_fp_OodleCore_Plugin_DisplayAssertion * g_fp_OodleCore_Plugin_DisplayAssertion;
extern t_fp_OodleCore_Plugin_Printf *	g_fp_OodleCore_Plugin_Printf;
extern t_fp_OodleCore_Plugin_RunJob *	g_fp_OodleCore_Plugin_RunJob;
extern t_fp_OodleCore_Plugin_WaitJob *	g_fp_OodleCore_Plugin_WaitJob;

extern int OodleCore_Plugin_GetJobTargetParallelism();

//=============================================================

OODLE_NS_END
