// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(parent,OodleAPI_Base)
#pragma once

/***

oo2baseinc.h : included from oodlecore.h (and therefore everywhere)

for defines that we want internally/globally
and ALSO in the public oodle2base.h

****/


// oodlecore.h is publicated to oodle2.h AND oodle2net.h
// double definition is protected by OODLE2_PUBLIC_CORE_DEFINES
// -> this is no longer needed and OODLE2_PUBLIC_CORE_DEFINES could go away
//	 I'll just leave it for now

PUBPRI(-51) // OodleCore before anything else except OodleBase

// RADEXPFUNC is always RADDEFFUNC

PUBPUSH
PUBPRI(-10020)
PUBSTART

#ifndef OODLE2_PUBLIC_CORE_DEFINES
#define OODLE2_PUBLIC_CORE_DEFINES 1

#define OOFUNC1	OOEXPFUNC
#define OOFUNC2	OOEXPLINK
#define OOFUNCSTART
#define OODLE_CALLBACK	OOLINK
PUBEND
PUBPOP

// OOFUNC external def comes here at -10010

PUBPUSH
PUBPRI(-10000)
PUBSTART

#ifndef NULL
#define NULL	(0)
#endif

// OODLE_MALLOC_MINIMUM_ALIGNMENT is 8 in 32-bit, 16 in 64-bit
#define OODLE_MALLOC_MINIMUM_ALIGNMENT	((OO_SINTa)(2*sizeof(void *)))

IDOC typedef void (OODLE_CALLBACK t_OodleFPVoidVoid)(void);
/* void-void callback func pointer
	takes void, returns void
*/

IDOC typedef void (OODLE_CALLBACK t_OodleFPVoidVoidStar)(void *);
/* void-void-star callback func pointer
	takes void pointer, returns void
*/

#define OODLE_JOB_MAX_DEPENDENCIES (4) IDOC
/* Maximum number of dependencies Oodle will ever pass to a RunJob callback
*/

#define OODLE_JOB_NULL_HANDLE	 (0) IDOC
/* Value 0 of Jobify handles is reserved to mean none
*	Wait(OODLE_JOB_NULL_HANDLE) is a nop
*   if RunJob returns OODLE_JOB_NULL_HANDLE it means the job
*   was run synchronously and no wait is required
*/

#define t_fp_Oodle_Job	t_OodleFPVoidVoidStar IDOC
/* Job function pointer for Plugin Jobify system

	takes void pointer returns void
*/


PUBEND
PUBPOP

PUBPUSH
PUBPRI(-9000)
PUBSTART

#endif // OODLE2_PUBLIC_CORE_DEFINES

PUBEND
PUBPOP
