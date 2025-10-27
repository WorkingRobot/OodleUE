// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(parent,OodleAPI_TextureBase)
#pragma once

#include "oodlebase.h"

//-------------------------------------------------------------------------------------

#ifndef __OODLE2TEX_H_INCLUDED__

PUBPRI(-52)
PUBTYPESTART
			
IDOC typedef enum OodleTex_Err
{
	OodleTex_Err_OK = 0,							// no error
	OodleTex_Err_UnsupportedCPU = -1,				// CPU is not supported. Oodle Texture encoding requires SSE4.1.
	OodleTex_Err_BadBCnFormat = -2,					// Specified BCn format is not supported.
	OodleTex_Err_BadPixelFormat = -3,				// Specified pixel format is not supported.
	OodleTex_Err_SurfaceCountMismatch = -4,			// The number of surface specified does not match the $OodleTex_Layout (without a layout, surface count must be 1).
	OodleTex_Err_BlockCountMismatch = -5,			// The number of blocks specified does not match the surface dimensions or $OodleTex_Layout.
	OodleTex_Err_LayoutFormatMismatch = -6,			// The given $OodleTex_Layout does not match the target BCn format.
	OodleTex_Err_NegativeLambda = -7,				// The specified Lagrange multiplier (lambda) is negative.
	OodleTex_Err_Internal = -8,						// Unspecified internal error (this should not happen; if you can, please report a bug.)

	OodleTex_Err_BC7PrepHeaderCorrupt = -9,			// BC7Prep header corrupted (or unsupported versin)
	OodleTex_Err_BC7PrepOutputBufTooSmall = -10,	// BC7Prep output buffer too small for given block count
	OodleTex_Err_BC7PrepScratchBufTooSmall = -11,	// BC7Prep scratch buffer too small for input data
	OodleTex_Err_BC7PrepPayloadCorrupt = -12,		// BC7Prep payload data chunk was corrupted
	OodleTex_Err_BC7PrepNoHeader = -13,				// BC7Prep missing output header pointer on encode
	OodleTex_Err_BC7PrepIllegalBlockCount = -14,	// BC7Prep block count outside legal bounds

	OodleTex_Err_InvalidSurfaceIndex = -15,			// The given surface index is out of bounds
	OodleTex_Err_MalformedBlockIDs = -16,  			// The block IDs passed to SetBlockLayout don't have the required form
	
	OodleTex_Err_BadMetric = -17,					// OodleTex_RDO_ErrorMetric invalid
	OodleTex_Err_BadEncodeEffortLevel = -18,		// OodleTex_EncodeEffortLevel invalid

	OodleTex_Err_SurfaceSizeMismatch = -19,			// One of the surfaces passed to the Encode/Decode functions does not match the size of the corresponding surface in the $OodleTex_Layout.

	OodleTex_Err_NoLicense_Unused = -20,			// unused
	OodleTex_Err_BufferTooSmall = -21,				// Provided buffer is too small
	OodleTex_Err_SurfaceTooLarge = -22,				// One of the input surfaces is larger than the maximum supported size (width or height above $OODLETEX_MAX_SURFACE_DIMENSION)
	OodleTex_Err_BadUniversalTiling = -23,			// OodleTex_RDO_UniversalTiling invalid
	OodleTex_Err_LayoutAndUniversalTilingIncompatible = -24,	// Both layout and universal tiling specified, they're mutually exclusive.

	OodleTex_Err_Force32 = 0x40000000		// not an actual error!
} OodleTex_Err;
/* Error codes for Oodle Texture API functions.

	Negative values indicate an actual error, non-negative values indicate success.
	
	Functions that return integers can return errors in negative numbers.
	Non-negative values indicate success, negative values correspond to an $OodleTex_Err value.
	The utility function $OodleTex_Err_GetName can be used to turn these error codes into strings.
*/

PUBTYPEEND

#endif // __OODLE2TEX_H_INCLUDED__

namespace OO2TEX_NS
{

// not pub :
OodleTex_Err OodleTex_Enter();
// OodleTex_Enter is called by all the public API entry points
//	it does any needed one-time inits
//	eg. rrCPUx86_detect();

// scope guard to set up MXCSR as desired and reset it on exit
struct FPStateScope
{
	U32 saved_state;

	FPStateScope();
	~FPStateScope();
};

};



