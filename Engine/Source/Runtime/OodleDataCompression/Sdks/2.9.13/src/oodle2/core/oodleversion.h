// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(parent,OodleAPI_Base)
#pragma once

#include "oodlepublicate.h"
#include "oodlelzpub.h" // for OodleLZ_SeekTable

OODLE_NS_START

PUBPUSH
PUBPRI(-10025)
PUBSTART

// header version :
//	the DLL is incompatible when MAJOR is bumped
//	MINOR is for internal revs and bug fixes that don't affect API compatibility
#define OODLE2_VERSION_MAJOR			9
#define OODLE2_VERSION_MINOR			13

// OodleVersion string is 1 . MAJOR . MINOR
//	don't make it from macros cuz the doc tool has to parse the string literal

#define OodleVersion "2.9.13"    IDOC
/*
*/

PUBEND
PUBPOP

PUBSTART


#define OODLE_HEADER_VERSION		((46<<24)|(OODLE2_VERSION_MAJOR<<16)|(OODLE2_VERSION_MINOR<<8)|(OO_U32)sizeof(OodleLZ_SeekTable))		IDOC
/* 	OODLE_HEADER_VERSION is used to ensure the Oodle header matches the lib.  Don't copy the value of this macro, it will change when
	the header is rev'ed.

	This is what you pass to $OodleX_Init or $Oodle_CheckVersion
*/

IDOC OOFUNC1 OO_BOOL OOFUNC2 Oodle_CheckVersion(OO_U32 oodle_header_version, OO_U32 * pOodleLibVersion OODEFAULT(NULL));
/* Check the Oodle lib version against the header you are compiling with

	$:oodle_header_version	pass $OODLE_HEADER_VERSION here
	$:pOodleLibVersion		(optional) filled with the Oodle lib version
	$:return				false if $OODLE_HEADER_VERSION is not compatible with this lib
	
	If you use the Oodle2 Ext lib,, $OodleX_Init does it for you.  But if you want to check that you have a
	compatible lib before trying to Init, then use this.
*/

IDOC OOFUNC1 void OOFUNC2 Oodle_LogHeader(void);
/* Log the Oodle version & copyright

	Uses the log set with $OodleCore_Plugins_SetPrintf
*/

PUBEND

OODLE_NS_END
