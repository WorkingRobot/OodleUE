// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once
#include "oodlelzpub.h"
#include "oodlelzcompressors.h"

// Core side of Legacy vtable
// Core oodlelzcompressors calls this
// Legacy lib plugs in this vtable

OODLE_NS_START

struct legacy_OodleLZDecoder;
struct LZBlockHeader;
struct LZQuantumHeader;
struct LRMCascade;

#define LEGACY_DECODER_SIZE	64

struct OodleLZLegacyVTable
{

	OodleLZ_CompressContext * (*fp_legacy_OodleLZ_CompressContext_Alloc)(
						OodleLZ_Compressor	compressor,
						OodleLZ_CompressionLevel level_fast_or_veryfast,
						S32 slidingWindowBits,
						S32 hashTableBits,
						const void * vwindow);
					
	void (*fp_legacy_OodleLZ_CompressContext_Free)(OodleLZ_CompressContext * context);

	void (*fp_legacy_OodleLZ_CompressContext_Reset)(OodleLZ_CompressContext * context, 
			S32 change_slidingWindowBits,
			const void * change_window);
		
	SINTa (*fp_legacy_OodleLZ_CompressWithContext)(OodleLZ_CompressContext * context,
											const void * rawBuf,SINTa rawLen,void * compBuf,
											const OodleLZ_CompressOptions * pOptions,
											OodleLZ_EncoderHeaders headers);
											
	S32 (*fp_legacy_OodleLZDecoder_MemorySizeNeeded_NoPad_Compute)(OodleLZ_Compressor compressor);

	void (*fp_legacy_OodleLZDecoder_Reset)(legacy_OodleLZDecoder * lzhD);

	void (*fp_legacy_OodleLZDecoder_Block_Start)( legacy_OodleLZDecoder * decoder );
	void (*fp_legacy_OodleLZDecoder_Block_Reset)( legacy_OodleLZDecoder * decoder );

	bool (*fp_legacy_OodleLZDecoder_CheckSlidingWindow)(OodleLZ_Compressor compressor,SINTa decBufferSize);

	bool (*fp_legacy_GotHuffFlag)(S32 decodeType,legacy_OodleLZDecoder * decoder,const U8 * compPtr,const U8 * compPtrEnd, S32 * pCompLen);

	int (*fp_legacy_OodleLZDecoder_GetDegenerateMemsetVal)( legacy_OodleLZDecoder * decoder );

	S32 (*fp_legacy_OodleLZ_DecodeOneQ)(S32 decodeType,legacy_OodleLZDecoder * decoder,
		U8 * decPtr,U8 * decPtrEnd,
		const U8 * compPtr,
		const U8 * compPtrQuantumEnd,
		const U8 * compPtrEnd,
		const LZBlockHeader & header,
		const LZQuantumHeader & qh,
		U8 * decBuf,
		SINTa decBufferSize,
		bool isSlidingWindow,
		SINTa pos_since_reset,
		const U8 * checkBuf);

	SINTa (*fp_legacy_OodleLZ_Compress_Sub)(OodleLZ_Compressor compressor,
		const U8 * rawBuf,SINTa rawLen,
		U8 * compBuf,
		OodleLZ_CompressionLevel level,
		const OodleLZ_CompressOptions * pOptions,
		const U8 * dictionaryBase,
		const LRMCascade * lrm,
		rrArenaAllocator * arena);

	S32 (*fp_legacy_OodleLZDecoder_MakeValidCircularWindowSize)(OodleLZ_Compressor compressor,S32 minWindowSize);

};

extern OodleLZLegacyVTable g_OodleLZLegacyVTable;

static RADINLINE bool OodleLZLegacyVTable_IsInstalled()
{
	return g_OodleLZLegacyVTable.fp_legacy_OodleLZ_Compress_Sub != NULL;
}

// legacy lib calls this to install :
// public API but not documented, not in public header
OOFUNC1 void OOFUNC2 OodleLZLegacyVTable_InstallToCore( const OodleLZLegacyVTable * from );		
				

OODLE_NS_END

