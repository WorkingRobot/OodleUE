// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrscopedptr.h"
#include "rrlog.h"

#if 0
//#pragma warning(disable : 4505)


namespace {
	struct junk
	{
	junk() : m_i(0) { rrprintf("junk\n"); }
	junk(int i) : m_i(i) { rrprintf("new junk %d\n",m_i); }
	~junk() { rrprintf("kill junk %d\n",m_i); }

	int m_i;
	};
};

static void rtl_New_test()
{
	//rtlLogSetState( rtlLogGetState() | RR_LOG_FILE_LINE );
	
	junk * p1 = OodleNew( junk(1);
	
	junk * p2 = OodleNew( junk [7];
	
	{
	
		ScopedPtr<junk> ptr;
		
		ptr.set( OodleNew( junk(4) );
		
		ScopedPtr<junk> p2;
		
		p2.swap(ptr);
		
		ptr.set(NULL);
		
		if ( ptr == NULL )
		//if ( ! ptr )
		{
			rrprintf("null\n");
		}
		
		//int i = ptr;
	
	}
	
	OodleDelete(p1);
	
	OodleDeleteArray [] p2;
	//rrDeleteArray(p2);
}

#endif

EXPORT_SOME_CRAP(rrscopedptr)
