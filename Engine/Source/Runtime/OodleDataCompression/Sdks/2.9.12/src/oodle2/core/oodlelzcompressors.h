// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __OODLELZCOMPRESSORS__
#define __OODLELZCOMPRESSORS__

#include "oodlebase.h"
#include "oodlelzpub.h"

OODLE_NS_START

#define OODLELZ_SLIDING_WINDOW_MIN_SIZE	(1<<OODLELZ_SLIDING_WINDOW_MIN_BITS)

//#define OODLELZ_SUPERFAST_HASH_BITS		13 // 12 or 13 ; fit in L1

#define OODLELZ_MAX_DIC_BACKUP	(1<<29)
#define OODLELZ_MAX_OFFSET		(1<<30)	// 1 GB ; must fit in S32
		
#define OODLELZ_DEFAULT_SSTB	256

struct rrArenaAllocator;

//=========================================================

void OodleLZ_LogBlocks(const void * compBuf,SINTa compLen, SINTa rawLen);

/***

OodleLZ_CompressOptions_GetDefault_Or_Copy_And_Validate :

user provides pOptions (can be NULL)
p_scratch_options should be local scratch workspace
returns either default options (const pointer) if pOptions as NULL
or, copies options then validates and returns pointer to p_scratch_options

result : you get an options pointer that is not NULL and is Validated

***/          
const OodleLZ_CompressOptions * OodleLZ_CompressOptions_GetDefault_Or_Copy_And_Validate(
	const OodleLZ_CompressOptions * pOptions,
	OodleLZ_CompressOptions * p_scratch_options);
	
SINTa OodleLZ_CompressMemcpy_DecodeType(int decodeType,const U8 * rawBuf,SINTa rawLen,U8 * compBuf,const U8 * dicBase,const OodleLZ_CompressOptions * pOptions);
SINTa OodleLZ_CompressMemcpy_Compressor(OodleLZ_Compressor compressor,const U8 * rawBuf,SINTa rawLen,U8 * compBuf,const U8 * dicBase,const OodleLZ_CompressOptions * pOptions);

//=========================================================

OOINLINE rrbool OodleLZ_Compressor_NeedsScratchMem(OodleLZ_Compressor compressor)
{
	return OodleLZ_Compressor_IsNewLZFamily(compressor);
}

RADINLINE rrbool OodleLZ_Compressor_IsEntropyCoded(OodleLZ_Compressor compressor)
{
	// non-entropy-coded :
	const U32 inverse_set =
		OODLELZ_COMPRESSOR_MASK(OodleLZ_Compressor_Selkie) |
		OODLELZ_COMPRESSOR_MASK(OodleLZ_Compressor_LZNIB) |
		OODLELZ_COMPRESSOR_MASK(OodleLZ_Compressor_LZBLW) |
		OODLELZ_COMPRESSOR_MASK(OodleLZ_Compressor_LZB16);
	const U32 set = ~inverse_set;
	return OODLELZ_COMPRESSOR_BOOLBIT(set,compressor);
}

// pre Oodle-26 :
//#define OODLELZ_MAX_SCRATCH_FOR_PHASE_HEADERS_AND_FUZZ	1024
// bigger for Leviathan :
#define OODLELZ_MAX_SCRATCH_FOR_PHASE_HEADERS_AND_FUZZ	4096
#define OODLELZ_SCRATCH_ALIGNMENT_PAD					32   // + 32 for offset & excess simd alignment

SINTa OodleLZ_Compressor_ScratchMemSize(OodleLZ_Compressor compressor,SINTa rawLen);
					
//=========================================================

// warning : I read & write to compBuf, so don't share it across threads !
// padding for parallel compression :
#define OODLELZ_BLOCK_COMPBUF_PAD   (256)

//=========================================================

S32 GetLZMatchTableBits( SINTa rawLen, 
	OodleLZ_CompressionLevel level,
	const OodleLZ_CompressOptions * pOptions,
	S32 vf_lo, S32 vf_hi,
	S32 lo, S32 hi );

//=========================================================

// DLL-exported but not publicated :
	
OOFUNC1 SINTa OOFUNC2 OodleKraken_Decode_Headerless(
	U8 * decomp, SINTa decomp_size,
	const U8 * comp, SINTa comp_size,
	UINTa bytes_since_reset, rrbool isMemcpy, rrbool lzEnable, rrbool subLiteralEnable,
	void * decoderMemory RADDEFAULT(NULL),
	SINTa decoderMemorySIze RADDEFAULT(0)
	);
	
//=========================================================

OODLE_NS_END

#endif // __OODLELZCOMPRESSORS__
