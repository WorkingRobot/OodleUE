// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once
#include "oodlejob.h"
#include "templates/rrnew.h"

OODLE_NS_START

//=============================================
// OodleJobReturnValue : 
//	just a value and a handle
//	wait for the handle before reading retval
//
// NOTE : no explicit atomic ops for shared variable retval
//	memory flushing for it should be taken care of by OodleXHandle ops
//	but it's a little foodgy

template <typename t_ret>
struct OodleJobReturnValue
{
	typedef		OodleJobReturnValue<t_ret>	this_type;
	
	// @@ volatile ?
	t_ret	m_retval;
	U64		m_handle;
	
	OodleJobReturnValue() : m_handle(0)
	{
	}
	
	~OodleJobReturnValue()
	{
		// the async work will write to m_retval
		//	so do not destruct me until it's done!
		RR_ASSERT( m_handle == 0 );
	}
	
	void Wait(void * user_ptr)
	{
		if (m_handle)
		{
			OodleJob_Wait(m_handle, user_ptr);
			m_handle = 0; // WaitJob invalidates the job handle
		}
	}
	
	U64 GetHandle() const { return m_handle; }
	
	t_ret & Get_Wait()
	{
		Wait();
		return m_retval;
	}	
	
private:
	OodleJobReturnValue(const this_type&);
	void operator=(const this_type&);
};

//=============================================

#ifdef NUM
#error NUM defined
#endif

#define NUM	0
#include "oodlejobobject_generator.inc"
#undef NUM
#define NUM	1
#include "oodlejobobject_generator.inc"
#undef NUM
#define NUM	2
#include "oodlejobobject_generator.inc"
#undef NUM
#define NUM	3
#include "oodlejobobject_generator.inc"
#undef NUM
#define NUM	4
#include "oodlejobobject_generator.inc"
#undef NUM
#define NUM	5
#include "oodlejobobject_generator.inc"
#undef NUM
#define NUM	6
#include "oodlejobobject_generator.inc"
#undef NUM
#define NUM	7
#include "oodlejobobject_generator.inc"
#undef NUM
#define NUM	8
#include "oodlejobobject_generator.inc"
#undef NUM
#define NUM	9
#include "oodlejobobject_generator.inc"
#undef NUM

//=================================

OODLE_NS_END


