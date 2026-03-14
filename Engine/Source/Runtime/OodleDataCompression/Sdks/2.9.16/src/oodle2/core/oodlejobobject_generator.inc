;// Copyright Epic Games, Inc. All Rights Reserved.
;// This source file is licensed solely to users who have
;// accepted a valid Unreal Engine license agreement 
;// (see e.g., https://www.unrealengine.com/eula), and use
;// of this source file is governed by such agreement.


#if 0


//-------------
#if NUM >= 1
	prefix1 
#if NUM >= 2
	prefix2 
#if NUM >= 3
	prefix3 
#if NUM >= 4
	prefix4 
#if NUM >= 5
	prefix5 
#if NUM >= 6
	prefix6 
#if NUM >= 7
	prefix7 
#if NUM >= 8
	prefix8 
#if NUM >= 9
	prefix9 
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
//-------------

//-------------
#if NUM >= 1
	aa1 bb1 
#if NUM >= 2
	aa2 bb2 
#if NUM >= 3
	aa3 bb3 
#if NUM >= 4
	aa4 bb4 
#if NUM >= 5
	aa5 bb5 
#if NUM >= 6
	aa6 bb6 
#if NUM >= 7
	aa7 bb7 
#if NUM >= 8
	aa8 bb8 
#if NUM >= 9
	aa9 bb9 
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
//-------------


#endif

//=======================================================================


template <typename t_retval,typename t_func
//-------------
#if NUM >= 1
	,typename t_arg1 
#if NUM >= 2
	,typename t_arg2 
#if NUM >= 3
	,typename t_arg3 
#if NUM >= 4
	,typename t_arg4 
#if NUM >= 5
	,typename t_arg5 
#if NUM >= 6
	,typename t_arg6 
#if NUM >= 7
	,typename t_arg7 
#if NUM >= 8
	,typename t_arg8 
#if NUM >= 9
	,typename t_arg9 
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
//-------------
	>
struct RR_STRING_JOIN(oodle_job_t,NUM) : public OodleJobBase
{
	t_func		m_func;
	t_retval *	m_pRetval;
	
//-------------
#if NUM >= 1
	t_arg1 m_arg1 ;
#if NUM >= 2
	t_arg2 m_arg2 ;
#if NUM >= 3
	t_arg3 m_arg3 ;
#if NUM >= 4
	t_arg4 m_arg4 ;
#if NUM >= 5
	t_arg5 m_arg5 ;
#if NUM >= 6
	t_arg6 m_arg6 ;
#if NUM >= 7
	t_arg7 m_arg7 ;
#if NUM >= 8
	t_arg8 m_arg8 ;
#if NUM >= 9
	t_arg9 m_arg9 ;
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
//-------------
	
	RR_STRING_JOIN(oodle_job_t,NUM)(t_retval * pRetVal,t_func func
	//-------------
#if NUM >= 1
	,t_arg1 arg1 
#if NUM >= 2
	,t_arg2 arg2 
#if NUM >= 3
	,t_arg3 arg3 
#if NUM >= 4
	,t_arg4 arg4 
#if NUM >= 5
	,t_arg5 arg5 
#if NUM >= 6
	,t_arg6 arg6 
#if NUM >= 7
	,t_arg7 arg7 
#if NUM >= 8
	,t_arg8 arg8 
#if NUM >= 9
	,t_arg9 arg9 
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
//-------------
		) :
		m_func(func),m_pRetval(pRetVal)
//-------------
#if NUM >= 1
	,m_arg1 (arg1) 
#if NUM >= 2
	,m_arg2 (arg2) 
#if NUM >= 3
	,m_arg3 (arg3) 
#if NUM >= 4
	,m_arg4 (arg4) 
#if NUM >= 5
	,m_arg5 (arg5) 
#if NUM >= 6
	,m_arg6 (arg6) 
#if NUM >= 7
	,m_arg7 (arg7) 
#if NUM >= 8
	,m_arg8 (arg8) 
#if NUM >= 9
	,m_arg9 (arg9) 
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
//-------------
	{
	}
	
	~RR_STRING_JOIN(oodle_job_t,NUM)() { }
	
	void Do()
	{
		t_retval retval = (*m_func)( 
//-------------
#if NUM >= 1
	m_arg1 
#if NUM >= 2
	,m_arg2 
#if NUM >= 3
	,m_arg3 
#if NUM >= 4
	,m_arg4 
#if NUM >= 5
	,m_arg5 
#if NUM >= 6
	,m_arg6 
#if NUM >= 7
	,m_arg7 
#if NUM >= 8
	,m_arg8 
#if NUM >= 9
	,m_arg9 
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
//-------------
		 );
		if ( m_pRetval ) *m_pRetval = retval;
	}
};

//=======================================================================

template <typename t_retval,typename t_func
//-------------
#if NUM >= 1
	,typename t_arg1 
#if NUM >= 2
	,typename t_arg2 
#if NUM >= 3
	,typename t_arg3 
#if NUM >= 4
	,typename t_arg4 
#if NUM >= 5
	,typename t_arg5 
#if NUM >= 6
	,typename t_arg6 
#if NUM >= 7
	,typename t_arg7 
#if NUM >= 8
	,typename t_arg8 
#if NUM >= 9
	,typename t_arg9 
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
//-------------

	>
OodleJobBase * make_oodle_job(t_retval * pRetVal,t_func func
 //-------------
 #if NUM >= 1
 	,t_arg1 arg1 
 #if NUM >= 2
 	,t_arg2 arg2 
 #if NUM >= 3
 	,t_arg3 arg3 
 #if NUM >= 4
 	,t_arg4 arg4 
 #if NUM >= 5
 	,t_arg5 arg5 
 #if NUM >= 6
 	,t_arg6 arg6 
 #if NUM >= 7
 	,t_arg7 arg7 
 #if NUM >= 8
 	,t_arg8 arg8 
 #if NUM >= 9
 	,t_arg9 arg9 
 #endif
 #endif
 #endif
 #endif
 #endif
 #endif
 #endif
 #endif
 #endif
 //-------------
  )
{
	typedef RR_STRING_JOIN(oodle_job_t,NUM)<t_retval,t_func
	//-------------
	#if NUM >= 1
		,t_arg1 
	#if NUM >= 2
		,t_arg2 
	#if NUM >= 3
		,t_arg3 
	#if NUM >= 4
		,t_arg4 
	#if NUM >= 5
		,t_arg5 
	#if NUM >= 6
		,t_arg6 
	#if NUM >= 7
		,t_arg7 
	#if NUM >= 8
		,t_arg8 
	#if NUM >= 9
		,t_arg9 
	#endif
	#endif
	#endif
	#endif
	#endif
	#endif
	#endif
	#endif
	#endif
	//-------------
		> t_myAF;
	return construct( OODLE_MALLOC_ONE(t_myAF) , pRetVal,func
//-------------
#if NUM >= 1
	,arg1 
#if NUM >= 2
	,arg2 
#if NUM >= 3
	,arg3 
#if NUM >= 4
	,arg4 
#if NUM >= 5
	,arg5 
#if NUM >= 6
	,arg6 
#if NUM >= 7
	,arg7 
#if NUM >= 8
	,arg8 
#if NUM >= 9
	,arg9 
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
//-------------

 );
}


//=======================================================================

template <typename t_retval,typename t_func
//-------------
#if NUM >= 1
	,typename t_arg1 
#if NUM >= 2
	,typename t_arg2 
#if NUM >= 3
	,typename t_arg3 
#if NUM >= 4
	,typename t_arg4 
#if NUM >= 5
	,typename t_arg5 
#if NUM >= 6
	,typename t_arg6 
#if NUM >= 7
	,typename t_arg7 
#if NUM >= 8
	,typename t_arg8 
#if NUM >= 9
	,typename t_arg9 
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
//-------------

	>
U64 start_oodle_job(OodleJobReturnValue<t_retval> * pRet,t_func func
 //-------------
 #if NUM >= 1
 	,t_arg1 arg1 
 #if NUM >= 2
 	,t_arg2 arg2 
 #if NUM >= 3
 	,t_arg3 arg3 
 #if NUM >= 4
 	,t_arg4 arg4 
 #if NUM >= 5
 	,t_arg5 arg5 
 #if NUM >= 6
 	,t_arg6 arg6 
 #if NUM >= 7
 	,t_arg7 arg7 
 #if NUM >= 8
 	,t_arg8 arg8 
 #if NUM >= 9
 	,t_arg9 arg9 
 #endif
 #endif
 #endif
 #endif
 #endif
 #endif
 #endif
 #endif
 #endif
 //-------------
	,
	 void * user_ptr,
	 bool run_asynchronous,
	 U64 * deps = 0, int num_deps = 0
  )
{
	typedef RR_STRING_JOIN(oodle_job_t,NUM)<t_retval,t_func
	//-------------
	#if NUM >= 1
		,t_arg1 
	#if NUM >= 2
		,t_arg2 
	#if NUM >= 3
		,t_arg3 
	#if NUM >= 4
		,t_arg4 
	#if NUM >= 5
		,t_arg5 
	#if NUM >= 6
		,t_arg6 
	#if NUM >= 7
		,t_arg7 
	#if NUM >= 8
		,t_arg8 
	#if NUM >= 9
		,t_arg9 
	#endif
	#endif
	#endif
	#endif
	#endif
	#endif
	#endif
	#endif
	#endif
	//-------------
		> t_myAF;
	OodleJobBase * af = construct( OODLE_MALLOC_ONE(t_myAF) , pRet ? &(pRet->m_retval) : NULL,func
//-------------
#if NUM >= 1
	,arg1 
#if NUM >= 2
	,arg2 
#if NUM >= 3
	,arg3 
#if NUM >= 4
	,arg4 
#if NUM >= 5
	,arg5 
#if NUM >= 6
	,arg6 
#if NUM >= 7
	,arg7 
#if NUM >= 8
	,arg8 
#if NUM >= 9
	,arg9 
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
//-------------
 );
	U64 h = StartOodleJobBase(af,user_ptr,run_asynchronous,deps,num_deps);
	if ( pRet ) pRet->m_handle = h;
	return h;
}

//=======================================================================

