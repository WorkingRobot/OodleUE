// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrbase.h"
#include "oodlecoreplugins.h"

#if (defined(__GCC__) && ! defined(__GNUC__))
#error WTF __GCC__ but not __GNUC__
#endif
    
RR_NAMESPACE_START

//===================================================================
// some error checks :

RR_COMPILER_ASSERT( sizeof(UINTa) == sizeof( RR_STRING_JOIN(OO_U,RAD_PTRBITS) ) );
RR_COMPILER_ASSERT( sizeof(UINTa) == RAD_PTRBYTES );
RR_COMPILER_ASSERT( sizeof(UINTr) == RAD_REGBYTES );
RR_COMPILER_ASSERT( sizeof(UINTa) == sizeof(RR_UINTa3264) );

RR_COMPILER_ASSERT( RAD_TWOPTRBYTES == 2* RAD_PTRBYTES );

RR_COMPILER_ASSERT( sizeof(rrbool) == 4 );

//==================================================================

// rrDisplayAssertion might just log, or it might pop a message box, depending on settings
//  rrDisplayAssertion returns whether you should break or not
rrbool rrDisplayAssertion(const char * fileName,const int line,const char * function,const char * message)
{
	if ( OODLE_NS_PRE g_fp_OodlePlugin_DisplayAssertion )
	{
		return (* OODLE_NS_PRE g_fp_OodlePlugin_DisplayAssertion)(fileName,line,function,message);
	}
	else
	{
		// if they set NULL, do nothing
		return false; // don't break;
	}	
}

RR_NAMESPACE_END
