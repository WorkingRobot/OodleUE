// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlecore.h"

OODLE_NS_START

struct LRMCascade;

SINTa Kraken_Compress(
	OodleLZ_Compressor compressor,
	const U8 * rawBuf,U8 * compBuf,SINTa rawLen,
	OodleLZ_CompressionLevel level,
	const OodleLZ_CompressOptions * pOptions,
	const U8 * dictionaryBase,
	const LRMCascade * lrmcascade,
    rrArenaAllocator * arena);

SINTa Hydra_Compress(
	OodleLZ_Compressor compressor,
	const U8 * rawBuf,U8 * compBuf,SINTa rawLen,
	OodleLZ_CompressionLevel level,
	const OodleLZ_CompressOptions * pOptions,
	const U8 * dictionaryBase,
	const LRMCascade * lrmcascade,
    rrArenaAllocator * arena);

S32 Kraken_DecodeOneQuantum(U8 * decomp,U8 * decomp_end,const U8 * comp,S32 quantumCompLen,const U8 * compBufEnd,SINTa pos_since_reset,
	void * scratch,SINTa scratch_size,OodleLZ_Decode_ThreadPhase threadPhase);

SINTa Kraken_Decode_Headerless(
	U8 * decomp, SINTa decomp_size,
	const U8 * comp, SINTa comp_size,
	UINTa bytes_since_reset, rrbool isMemcpy, rrbool lzEnable, rrbool subLiteralEnable,
	void * decoderMemory, SINTa decoderMemorySize
	);
/* Decode a single headerless Kraken chunk from memory to memory, synchronously.

    $:decomp             pointer to decompressed data (if not a reset chunk, should be right where previous chunk ended)
	$:decomp_size        expected size of decompressed data (if this doesn't match actual decoded size, it's an error)
	$:comp               pointer to headerless compressed data stream
	$:comp_size          number of compressed bytes available (if this doesn't match actual coded size, it's an error)
	$:bytes_since_reset  number of bytes since the last reset chunk; 0 on reset chunks. (Used for offset validation.)
	$:isMemcpy           is this a memcpy chunk? (If so, comp_size==decomp_size required.)
	$:lzEnable           is LZ enabled? (If not, pure Literal chunk.)
	$:subLiteralEnable   true if LZ-sub, false if LZ-raw mode.
	$:decoderMemory      _must_ point to decoder scratch mem (not optional)
	$:decoderMemorySize  size of decoder scratch mem, must be sufficiently large (not optional)
	$:return		     the number of decompressed bytes output, $OODLELZ_FAILED (0) if none can be decompressed

   The relevant metadata bits and sizes need to be stored in the container format.
*/

OODLE_NS_END
	
//===============================================	
	

