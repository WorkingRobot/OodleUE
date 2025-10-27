// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "oodleversion.h"
#include "oodlecoreinit.h"
#include <string.h>

OODLE_NS_START

static const U32 c_Oodle_Version = OODLE_HEADER_VERSION;

OOFUNC1 rrbool OOFUNC2 Oodle_CheckVersion(U32 oodle_header_version, U32 * pOodleLibVersion RADDEFAULTX(NULL))
{
	OOFUNCSTART
	
	if ( 1 ) // check myself
	{
		const char * test =
"2." RR_STRINGIZE_DELAY(OODLE2_VERSION_MAJOR) "." RR_STRINGIZE_DELAY(OODLE2_VERSION_MINOR);
		const char * OodleVersionStr = OodleVersion;
		if ( strcmp(test,OodleVersionStr) != 0 )
		{
			// usually called before ooLogError is set up?
			RR_ASSERT_FAILURE("Bad version defines");	
			rrPrintf_v0("Bad version defines\n");	
			return false;
		}
	}
	
	if ( pOodleLibVersion )
		*pOodleLibVersion = c_Oodle_Version;
		
	U32 oodle_header_version_major = (oodle_header_version>>16);
	
	if ( oodle_header_version_major != (OODLE_HEADER_VERSION>>16) )
	{
		// don't use OodleLog since it's not init yet
		//fprintf(stderr,"ERROR : Oodle_CheckVersion failed - major mismatch!\n");
		RR_ASSERT_FAILURE("ERROR : Oodle_CheckVersion failed - major mismatch!");
		rrPrintf_v0("ERROR : Oodle_CheckVersion failed - major mismatch!");
		return false;
	}
	
	// minor not checked	
	
	U32 oodle_header_version_sizeof = oodle_header_version & 0xFF;
	SINTa check_size = sizeof(OodleLZ_SeekTable);
	if ( oodle_header_version_sizeof != (U32)check_size )
	{
		// don't use OodleLog since it's not init yet
		//fprintf(stderr,"ERROR : Oodle_CheckVersion failed - sizeof mismatch!\n");
		RR_ASSERT_FAILURE("ERROR : Oodle_CheckVersion failed - sizeof mismatch!");
		rrPrintf_v0("ERROR : Oodle_CheckVersion failed - sizeof mismatch!");
		return false;
	}

	// Oodle_CheckVersion is the first thing done by OodleX Init
	//	Enter here?
	// it's also the unofficial public API for clients to do a one-time init
	if ( ! OodleCore_Enter() )
		return false;
	
	return true;
}

OOFUNC1 void OOFUNC2 Oodle_LogHeader(void)
{
	rrprintf("Oodle %s %s\n",OodleVersion,RADCOPYRIGHT);
}


OODLE_NS_END
