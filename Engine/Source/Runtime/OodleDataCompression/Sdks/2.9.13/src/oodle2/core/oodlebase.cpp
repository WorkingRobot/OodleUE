// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "oodlebase.h"

//===============================================
// check yo self :

#ifdef _MSC_VER
	#if defined(OODLE_BUILDING_LIB)
	//#pragma RR_PRAGMA_MESSAGE("OODLE_BUILDING_LIB")
	#elif defined(OODLE_BUILDING_DLL)
	//#pragma RR_PRAGMA_MESSAGE("OODLE_BUILDING_DLL")
	#else // linkage
	#error invalid - Oodle linkage not set
	#endif

	#if OODLE_BUILD_CONFIG_TRACE_ALL_CALLS
	#pragma RR_PRAGMA_MESSAGE("OODLE_BUILD_CONFIG_TRACE_ALL_CALLS")
	#endif
#endif

//===============================================

OODLE_NS_START

//-------------------------------------------------------------------------------------

// installed with ooxLogErrorInstall
t_OodleFPVoidVoid * fp_ooLogErrorPre = NULL;
t_OodleFPVoidVoid * fp_ooLogErrorPost = NULL;

#define CALL_IF_NOT_NULL(fp)	if ( (*fp) != NULL ) (*fp)

void ooLogErrorPre()
{
	CALL_IF_NOT_NULL(fp_ooLogErrorPre)();
}

void ooLogErrorPost()
{
	CALL_IF_NOT_NULL(fp_ooLogErrorPost)();
}

//-------------------------------------------------------------------------------------

#ifndef __RAD64__
SINTa oo64toA( S64 s )
{
	//RR_ASSERT( s >= 0 ); // OODLEX_FILE_SIZE_INVALID !!
	
	// cast to U64 to get the -1 also :
	if ( ((U64)s) >= (1UL<<31) )
	{
		//if ( s == OODLEX_FILE_SIZE_INVALID )
		if ( s == -1 )
		{
			//ooLogError("OODLEX_FILE_SIZE_INVALID passed to oo64toA!\n");
			// map -1 to -1 ?		
			return (SINTa)-1;
		}
		else
		{
			ooLogError("Over 2 GB buffer size on 32 bit OS!\n");
			//RR_ASSERT_FAILURE_ALWAYS("bad bad");
			RR_ASSERT_FAILURE("bad bad");
		}
		return -1;
	}
	else
	{
		return (SINTa) s;
	}	
}
#endif

/**

Makes a string like :

Win-x64 msvc-1400

**/
const char * Oodle_PlatformDesc()
{
	const char * ret =
		
#if defined __RADPLATFORMNAME__
	__RADPLATFORMNAME__
#elif defined __RADLINUX__
	 "Linux"
#elif defined __RADMAC__
	 "Mac"
#elif defined __RADWIN__
	 "Win"
#elif defined __RADANDROID__
	"Android"
#elif defined __RADIPHONE__
	"IOS"
#elif defined __RADEMSCRIPTEN__
	"EMSCRIPTEN"
#else
	#error need platform string
#endif

#if defined(__RADARM64__)
	"-ARM64"
#elif defined(__RADARM__)
	"-ARM32"
#elif defined(__RADX64__)
	"-x64"
#elif defined(__RADX86__)
	"-x86"
#elif defined(__RADPPC__)
	"-PPC"
#elif defined(__RADWASM__)
	"-WASM"
#else
	#error need cpu string
#endif

//#endif

#ifdef _DEBUG
	" DEBUG"
#endif


#ifdef __RAD_GCC_VERSION__
	
#ifdef __clang__
	// Apple has their own fork of LLVM with different version numbers
	#ifdef __APPLE__
	" xcode-"
	#else
	" clang-" 
	#endif
	
	// __clang_version__
	// but string is too long for my taste :
	
	RR_STRINGIZE_DELAY(__clang_major__)
	"."
	RR_STRINGIZE_DELAY(__clang_minor__)
	"."
	RR_STRINGIZE_DELAY(__clang_patchlevel__)
#else
	" gcc-"
	RR_STRINGIZE_DELAY(__GNUC__)
	"."
	RR_STRINGIZE_DELAY(__GNUC_MINOR__)
	"."
	RR_STRINGIZE_DELAY(__GNUC_PATCHLEVEL__)
#endif

#elif defined _MSC_VER

	" msvc-"

	RR_STRINGIZE_DELAY(_MSC_VER);

#else
	
	#error need compiler
	
#endif

	;


	return ret;	
}

OODLE_NS_END
