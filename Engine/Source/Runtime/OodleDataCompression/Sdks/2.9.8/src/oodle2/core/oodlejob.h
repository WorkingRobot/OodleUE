// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once
#include "oodlecore.h"
#include "oodlecoreplugins.h"

/**

Oodle Core job system, invokes plugged in job func pointers

(not Oodle X , though that is plugged in here if OodleX_Init is called)

**/

OODLE_NS_START

#define OODLEJOB_DEFAULT  ( 0)
#define OODLEJOB_DISABLE  (-1)

// WaitAll does a single thread wait on N handles
//	to avoid silly wakeups for wait sequences
//	N is unbounded
//	Wait() is done on all handles so they are all deleted after this operation
void OodleJob_WaitAll( U64 * handles, int num, void * user_ptr );

// null job func used for dependency chaining	
void OODLE_CALLBACK Oodle_Null_Job_Func( void * pdata);

// run a function async on the plugin job system
//	t_fp_Oodle_Job fp_job is void OODLE_CALLBACK func(void * job_data);
static RADINLINE U64 OodleJob_Run( t_fp_Oodle_Job * fp_job, void * job_data , U64 * dependencies, int num_dependencies, void * user_ptr )
{
	RR_ASSERT( g_fp_OodlePlugin_RunJob != NULL );

	// filter out 0 dependencies
	U64 deps[OODLE_JOB_MAX_DEPENDENCIES];
	int ndeps = 0;
	for (int i=0; i < num_dependencies; ++i)
		if (dependencies[i] != 0)
			deps[ndeps++] = dependencies[i];
	U64 ret = (*g_fp_OodlePlugin_RunJob)(fp_job,job_data,deps,ndeps,user_ptr);
	return ret;
}

// as above, but can run in-thread if requested by num_job_threads == OODLEJOB_DISABLE
static RADINLINE U64 OodleJob_Run_MaybeSingleThreaded( t_fp_Oodle_Job * fp_job, void * job_data , U64 * dependencies, int num_dependencies, void * user_ptr, int num_job_threads )
{
	if (num_job_threads >= OODLEJOB_DEFAULT)
		return OodleJob_Run(fp_job,job_data,dependencies,num_dependencies,user_ptr);
	// dependencies should be all 0 as well
	fp_job(job_data);
	return 0;
}

// wait for a job started by Job_Run ; also deleted the handle
//	handle is not a weak pointer, you cannot Wait twice; do NOT race dependencies against waits!
static RADINLINE void OodleJob_Wait( U64 job_handle, void * user_ptr )
{
	// @@ optional or automatic ?
	// tag deps to get lines in threadprofiler :
	//ThreadProfiler_Tag("wait",job_handle);
		
	RR_ASSERT( g_fp_OodlePlugin_WaitJob != NULL );
	if (job_handle != 0)
		(*g_fp_OodlePlugin_WaitJob)(job_handle,user_ptr);
}

static RADINLINE bool Oodle_IsJobSystemSet( )
{
	return g_fp_OodlePlugin_RunJob != OodlePlugin_RunJob_Default;
}

// OodleJobs are (potentially) async tasks
// there must be a clearly defined owner at any given time, they cannot be
// copied or assigned.
class OodleJob
{
	U64 handle;

	// no copy ctor/assignment
	// jobs are not copyable since there must be a unique owner at any given
	// time; we could move them though, if we ever need to.
	OodleJob(const OodleJob &);
	OodleJob & operator =(const OodleJob &);

public:
	OodleJob()
		: handle(0)
	{
	}

	~OodleJob()
	{
		// If you ran a job, you must have waited for it to finish
		// before the handle goes out of scope.
		//RR_ASSERT(handle == 0);
		RR_ASSERT_ALWAYS(handle == 0);
	}

	// Run a job.
	//
	// fp_job = t_fp_Oodle_Job points to
	//     void OODLE_CALLBACK job_func(void * job_data);
	//
	// run_asynchronous = true means actually run as a job.
	// run_asynchronous = false just calls the FP immediately; this
	// is convenient for things you want jobified in the general code path
	// but then run synchronously as an optimization in cases where otherwise
	// you'd just dispatch the job and then immediately wait.
	//
	// Dependencies are other OodleJobs. (Must be passed by reference since
	// they can't be copied/assigned.)
	//
	// After you run() a job, it's your responsibility to wait() for the
	// result.
	void run(t_fp_Oodle_Job * fp_job, void * job_data, void * user_ptr,
			 bool run_asynchronous,
			 U64 dep0 = 0,U64 dep1 = 0);
			 //const OodleJob &dep0 = OodleJob(), const OodleJob &dep1 = OodleJob());

	// Waits for the job to be finished.
	//
	// Only does something if there actually was an outstanding job that
	// hasn't been waited for yet. After waiting, we clear the handle to 0.
	// That means we can still pass an already-waited-for OodleJob as a
	// dependency, where it just acts as a nop. (Since the task was already
	// finished at submit time).
	void wait(void * user_ptr)
	{
		if (handle)
		{
			OodleJob_Wait(handle, user_ptr);
			handle = 0; // WaitJob invalidates the job handle
		}
	}
	
	U64 get_handle() const { return handle; }
};

// WaitSets turn a bunch of handles into a single handle you can wait for
// Used as an opaque type here to not pull in extra deps
struct OodleJobWaitSet;

// Creates a wait set for the given number of handles. This gives you a
// single job handle that corresponds to depending on all given handles.
//
// The wait set takes ownership of the given handles, meaning it will be the
// one to call Wait on them, once you call OodleJobWaitSet_WaitAndDestroy.
// You can still declare dependencies on them before then.
OodleJobWaitSet * OodleJobWaitSet_Create(U64 * handles, int nhandles, void * user_ptr);

// Like OodleJobWaitSet_Create, but also takes up responsibility for the parent wait
// set (takes ownership and will clean it up).
//
// The idea is that this allows you to chain multi-pass operations with one or
// more internal sync points and gives you a single wait set that can cover
// all of them.
//
// You still need to flag internal dependencies correctly, this is purely a
// resource ownership thing at cleanup time for API convenience.
OodleJobWaitSet * OodleJobWaitSet_CreateChained(OodleJobWaitSet * parent, U64 * handles, int nhandles, void * user_ptr);

// Determines the root handle for a waitset. This is what you wait for.
U64 OodleJobWaitSet_RootHandle(const OodleJobWaitSet * wait_set);

// Waits for the given wait set to complete then destroys it, and
// does the same for all earlier wait sets in the chain (if any).
void OodleJobWaitSet_WaitAndDestroy(OodleJobWaitSet * wait_set);


//=====================================
/*

in oodlejob.inl :

U64 start_oodle_job(OodleJobReturnValue<t_retval> * pRet,t_func func, .. args ...
	,
	 void * user_ptr,
	 bool run_asynchronous,
	 U64 * deps = 0, int numdeps = 0 );
  );
  
OodleJobBase * make_oodle_job(t_retval * pRetVal,t_func func, .. args ..  );
 
then 
U64 StartOodleJobBase( OodleJobBase * af, 
			 void * user_ptr,
			 bool run_asynchronous,
			 U64 * deps = 0, int numdeps = 0 );

*/

//=============================================
// virtual base to run a function call :

struct OodleJobBase
{
	OodleJobBase() { }
	virtual ~OodleJobBase() { }
	
	virtual void Do() = 0;
};

//=============================================

U64 StartOodleJobBase( OodleJobBase * af, 
			 void * user_ptr,
			 bool run_asynchronous,
			 U64 * deps = 0, int numdeps = 0 );

//=================================

OODLE_NS_END

