// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(parent,OodleAPI_TextureRTBase)
#pragma once

#include "oodlebase.h"

PUBPRI(-52) // OodleBase before anything else (before OodleCore)

PUBPUSH
PUBPRI(-10040)
#if 0
PUBTYPESTART

//===================================================
// Oodle2 Texture runtime header
// (C) Copyright 1994-2022 Epic Games Tools LLC
//===================================================

#ifndef __OODLE2TEXRT_H_INCLUDED__
#define __OODLE2TEXRT_H_INCLUDED__

#ifndef OODLE2TEXRT_PUBLIC_HEADER
#define OODLE2TEXRT_PUBLIC_HEADER 1
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

#endif // __OODLE2TEXRT_H_INCLUDED__
PUBEND
PUBPOP

//-------------------------------------------------------------------------------------

#ifndef __OODLE2TEXRT_H_INCLUDED__

PUBTYPESTART

IDOC typedef enum OodleTexRT_Err
{
	OodleTexRT_Err_OK = 0,							// no error
	OodleTexRT_Err_Internal = -101,					// Unspecified internal error (this should not happen; if you can, please report a bug.)

	OodleTexRT_Err_BC7PrepHeaderCorrupt = -102,		// BC7Prep header corrupted (or unsupported versin)
	OodleTexRT_Err_BC7PrepOutputBufTooSmall = -103,	// BC7Prep output buffer too small for given block count
	OodleTexRT_Err_BC7PrepScratchBufTooSmall = -104,// BC7Prep scratch buffer too small for input data
	OodleTexRT_Err_BC7PrepPayloadCorrupt = -105, 		// BC7Prep payload data chunk was corrupted
	OodleTexRT_Err_BC7PrepInputTooSmall = -106, 		// BC7Prep input data to small for given header block count
	OodleTexRT_Err_BC7PrepTooManyBlocks = -107,		// Too many blocks in a single BC7Prep blob for GPU decode (limits exist in some GPU variants)

	OodleTexRT_Err_GPU_OutOfShaderMem = -108,		// Insufficient shader memory passed to GPU init
	OodleTexRT_Err_GPU_BadMemoryType = -109,		// The given GPU resource memory type is not permitted in this context.

	OodleTexRT_Err_Force32 = 0x40000000		// not an actual error!
} OodleTexRT_Err;
/* Error codes for Oodle Texture Runtime API functions.

	Negative values indicate an actual error, non-negative values indicate success.

	Functions that return integers can return errors in negative numbers.
	Non-negative values indicate success, negative values correspond to an $OodleTexRT_Err value.
	The utility function $OodleTexRT_Err_GetName can be used to turn these error codes into strings.
*/

PUBTYPEEND

#endif // __OODLE2TEXRT_H_INCLUDED__

OODLE_NS_START

// not pub :
OodleTexRT_Err OodleTexRT_Enter();
// OodleTex_Enter is called by all the public API entry points
//	it does any needed one-time inits
//	eg. rrCPUx86_detect();

OODLE_NS_END


