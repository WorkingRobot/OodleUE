// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "oodlejob.h"
#include "oodlejob.inl"
#include "cbradutil.h"
#include "threadprofiler.h"
#include "templates/rrvector_st.h"
#include "templates/rrnew.h"

OODLE_NS_START

void OodleJob::run(t_fp_Oodle_Job * fp_job, void * job_data, void * user_ptr, bool run_asynchronous,
		 U64 dep0,U64 dep1)
{
	RR_ASSERT(handle == 0);

	// If run_asynchronous is off, just run the job right now
	// and leave the handle at 0.
	if (!run_asynchronous)
	{
		(*fp_job)(job_data);
		return;
	}

	// Collect any non-trivial dependencies then call the installed RunJob plugin.
	U64 deps[OODLE_JOB_MAX_DEPENDENCIES];
	int ndeps = 0;

	if (dep0) deps[ndeps++] = dep0;
	if (dep1) deps[ndeps++] = dep1;
	RR_ASSERT(ndeps <= OODLE_JOB_MAX_DEPENDENCIES);

	handle = OodleJob_Run(fp_job,job_data,deps,ndeps,user_ptr);
}

void OODLE_CALLBACK StartOodleJobBase_Job( void * job_data )
{
	OodleJobBase * job_object = (OodleJobBase *)job_data;
	job_object->Do();
	// self destruct :
	OodleDeleteVirtual(job_object);
}

U64 StartOodleJobBase( OodleJobBase * job_object, 
			 void * user_ptr,
			 bool run_asynchronous,
			 U64 * deps, int numdeps )
{
	RR_ASSERT( numdeps >= 0 && numdeps <= OODLE_JOB_MAX_DEPENDENCIES );

	U64 handle = OodleJob_Run(StartOodleJobBase_Job,(void *)job_object,
				deps,numdeps,user_ptr);
	return handle;
}			 
			
// null job func used for dependency chaining	
void OODLE_CALLBACK Oodle_Null_Job_Func( void * pdata)
{
}

struct OodleJobWaitSet
{
	OodleJobWaitSet * m_parent;
	void * m_user_ptr;
	int m_nhandles; // number of original handles to wait on
	vector_st<U64,32> m_all_handles; // all handles including our temps

	void init(OodleJobWaitSet * parent, U64 * handles, int nhandles, void * user_ptr);
	U64 root_handle() const;
	void wait();
};

void OodleJobWaitSet::init(OodleJobWaitSet * parent_wait_set, U64 * handles, int num_raw_handles, void * user_ptr)
{
	int nhandles = 0;

	for (int i=0; i < num_raw_handles; ++i)
		nhandles += (handles[i] != 0 ? 1 : 0);

	m_parent = parent_wait_set;
	m_user_ptr = user_ptr;
	m_nhandles = nhandles;

	if ( nhandles <= 1 )
	{
		m_all_handles.resize(nhandles);
		if ( nhandles == 1 )
		{
			m_all_handles[0] = 0;
			for LOOP(i,num_raw_handles)
			{
				if (handles[i] != 0)
				{
					m_all_handles[0] = handles[i];
					break;
				}
			}
			RR_ASSERT(m_all_handles[0] != 0);
		}

		return;
	}

	// make a tree of null jobs
	// each null job can reduce OODLE_JOB_MAX_DEPENDENCIES sub-jobs
	//	(OODLE_JOB_MAX_DEPENDENCIES = 4)
	// so for example if
	// nhandles = 6
	// we make a [4][2] level
	// then another [2] to reduce that level
	// we get a tree with 3 total nodes
	// then we Wait() on the root

	int ntree = 0;
	int ncur = nhandles;
	while( ncur > 1 )
	{
		ncur = (ncur + OODLE_JOB_MAX_DEPENDENCIES-1)/OODLE_JOB_MAX_DEPENDENCIES;
		ntree += ncur;
	}

	// copy existing root handles
	m_all_handles.resize(nhandles + ntree);

	for (int i=0,j=0; i < num_raw_handles; ++i)
	{
		if (handles[i] != 0)
			m_all_handles[j++] = handles[i];
	}

	U64 * all_handles = m_all_handles.data();
	int level_start = 0;
	int t = nhandles;

	for ( ;; )
	{
		// make tree for this level :
		int parent_count = t - level_start;
		if ( parent_count <= 1 )
			break;

		U64 * parent = all_handles + level_start;

		// we're starting a new level
		level_start = t;

		while( parent_count > 0 )
		{
			int n = RR_MIN(parent_count,OODLE_JOB_MAX_DEPENDENCIES);

			// if n == 1 this is silly, but DONT just set handle = parent
			//	because that would make use OodleJob_Wait twice on the same handle
			U64 handle = OodleJob_Run(Oodle_Null_Job_Func,NULL,
				parent,n,user_ptr);
			all_handles[t++] = handle;

			parent += n;
			parent_count -= n;
		}
	}

	RR_ASSERT(t == m_all_handles.size32());
}

U64 OodleJobWaitSet::root_handle() const
{
	return m_all_handles.size() ? m_all_handles.back() : 0;
}

void OodleJobWaitSet::wait()
{
	THREADPROFILESCOPE("OodleJobWaitSet::wait");
	// Wait on the handles in reverse order, root to base
	// only the first wait actually waits, the rest
	// is just to clean up the handdles
	for (int i = m_all_handles.size32() - 1; i >= 0; i--)
	{
		// tag deps to original jobs to get lines in threadprofiler :
		if ( i < m_nhandles )
			ThreadProfiler_Tag("wait",m_all_handles[i]);
		OodleJob_Wait(m_all_handles[i],m_user_ptr);
	}
}

/**

Construct a proper WaitAll for Jobs
using the dependency system
which has a limit on dependency count

this is so we can just do a single thread wait for large batches of work
it's quite a big time difference in practice, so worth the trouble

**/
void OodleJob_WaitAll( U64 * handles, int nhandles, void * user_ptr )
{
	OodleJobWaitSet waitset;
	waitset.init(0, handles, nhandles, user_ptr);
	waitset.wait();
}

OodleJobWaitSet * OodleJobWaitSet_Create(U64 * handles, int nhandles, void * user_ptr)
{
	return OodleJobWaitSet_CreateChained(0, handles, nhandles, user_ptr);
}

OodleJobWaitSet * OodleJobWaitSet_CreateChained(OodleJobWaitSet * parent, U64 * handles, int nhandles, void * user_ptr)
{
	OodleJobWaitSet * wait_set = OodleNew(OodleJobWaitSet);
	if ( wait_set )
		wait_set->init(parent, handles, nhandles, user_ptr);

	return wait_set;
}

U64 OodleJobWaitSet_RootHandle(const OodleJobWaitSet * wait_set)
{
	return wait_set->root_handle();
}

void OodleJobWaitSet_WaitAndDestroy(OodleJobWaitSet * wait_set)
{
	while ( wait_set )
	{
		OodleJobWaitSet * next = wait_set->m_parent;
		wait_set->wait();
		OodleDelete(wait_set);
		wait_set = next;
	}
}

OODLE_NS_END

