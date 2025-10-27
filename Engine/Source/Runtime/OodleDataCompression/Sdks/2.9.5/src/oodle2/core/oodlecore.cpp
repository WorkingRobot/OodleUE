// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "oodlecore.h"

RR_COMPILER_ASSERT( sizeof(U32) == 4 );
RR_COMPILER_ASSERT( sizeof(U64) == 8 );
RR_COMPILER_ASSERT( sizeof(UINTa) == sizeof(void *) );
RR_COMPILER_ASSERT( sizeof(UINTr) == RR_SIZEOF_UINTR );
RR_COMPILER_ASSERT( sizeof(UINTa) == RR_SIZEOF_UINTA );

EXPORT_SOME_CRAP(oodlecore);

/*
// Emscripten is 32-bit only...
#ifdef __RADEMSCRIPTEN__
#ifndef __RAD64REGS__
#error Emscripten 64 bit ?
#endif
#endif
*/

//===========================================================

PUBPUSH
PUBPRI(-10040)
#if 0
PUBTYPESTART

//===================================================
// Oodle2 Core header
// (C) Copyright 1994-2021 Epic Games Tools LLC
//===================================================

#ifndef __OODLE2_H_INCLUDED__
#define __OODLE2_H_INCLUDED__

#ifndef OODLE2_PUBLIC_HEADER
#define OODLE2_PUBLIC_HEADER 1
#endif

#ifndef __OODLE2BASE_H_INCLUDED__
#include "oodle2base.h"
#endif

#ifdef _MSC_VER
#pragma pack(push, Oodle, 8)

#pragma warning(push)
#pragma warning(disable : 4127) // conditional is constant
#endif
PUBTYPEEND
#endif
#endif
PUBPOP

PUBPUSH
PUBPRI(1999)
#if 0
PUBSTART
#ifdef _MSC_VER
#pragma warning(pop)
#pragma pack(pop, Oodle)
#endif

#endif // __OODLE2_H_INCLUDED__
PUBEND
PUBPOP

//===========================================================

PUBPUSH
PUBPRI(1900)
PUBSTART

// define old names so they still compile :
#define OODLECORE_PLUGIN_JOB_MAX_DEPENDENCIES OODLE_JOB_MAX_DEPENDENCIES
#define t_fp_OodleCore_Plugin_Job t_fp_Oodle_Job

PUBEND
PUBPOP

//===========================================================
