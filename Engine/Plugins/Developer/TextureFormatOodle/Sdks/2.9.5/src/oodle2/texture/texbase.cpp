// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(parent,OodleAPI_Texture)
//idoc(end)
#include "texbase.h"
#include "cpux86.h"

#ifdef __RADSSE2__
#include "emmintrin.h"
#endif

OODLE_NS_START

OodleTex_Err OodleTex_Enter()
{
//	OodleTex_Err test;
//	test = OodleTex_Err_OK;
//	S32 t = test;

	// OodleTex_Enter is called by all the public API entry points
	//	it does any needed one-time inits

  // CPU feature check on x86, doesn't make sense on other targets
#ifdef __RADX86__
	// rrCPUx86_detect has its own internal do-once :
	rrCPUx86_detect();

	// NOTE we have our own separate copy of rrCPUx86 from Oodle Core!
	//	our copy is in oo2tex: ; they need their own inits if you use both

	// @@ OodleCore_Enter ?

	// Check for required CPU features
	if ( ! rrCPUx86_feature_present( RRX86_CPU_SSE41 ) )
		return OodleTex_Err_UnsupportedCPU;
#endif

	return OodleTex_Err_OK;
}

#ifdef __RADSSE2__

FPStateScope::FPStateScope()
{
	saved_mxcsr = _mm_getcsr();

	// Set up our expected FP state: no exception flags set,
	// all exceptions masked (suppressed), round to nearest,
	// flush to zero and denormals are zero both off.
	_mm_setcsr(_MM_MASK_MASK /* all exceptions masked */ | _MM_ROUND_NEAREST | _MM_FLUSH_ZERO_OFF);
}

FPStateScope::~FPStateScope()
{
	_mm_setcsr(saved_mxcsr);
}

#else // __RADX86__

FPStateScope::FPStateScope()
{
}

FPStateScope::~FPStateScope()
{
}

#endif

OODLE_NS_END

//==================================================
// publicate the Oodle Texture header wrapper stuff :
// only for public header, so can be in cpp not our .h

PUBPUSH
PUBPRI(-10040)
#if 0
PUBTYPESTART

//===================================================
// Oodle2 Texture header
// (C) Copyright 1994-2021 Epic Games Tools LLC
//===================================================

#ifndef __OODLE2TEX_H_INCLUDED__
#define __OODLE2TEX_H_INCLUDED__

#ifndef OODLE2TEX_PUBLIC_HEADER
#define OODLE2TEX_PUBLIC_HEADER 1
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

#endif // __OODLE2TEX_H_INCLUDED__
PUBEND
PUBPOP

