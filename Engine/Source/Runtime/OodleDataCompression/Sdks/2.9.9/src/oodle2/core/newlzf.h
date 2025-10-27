// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlecore.h"

OODLE_NS_START

struct LRMCascade;

SINTa Mermaid_Compress(
	OodleLZ_Compressor compressor,
	const U8 * rawBuf,U8 * compBuf,SINTa rawLen,
	OodleLZ_CompressionLevel level,
	const OodleLZ_CompressOptions * pOptions,
	const U8 * dictionaryBase,
	const LRMCascade * lrmcascade,
    rrArenaAllocator * arena);

S32 Mermaid_DecodeOneQuantum(U8 * decPtr,U8 * decPtrEnd,const U8 * compPtr,S32 quantumCompLen,const U8 * compBufEnd,SINTa pos_since_reset,
	void * scratch,SINTa scratch_size,OodleLZ_Decode_ThreadPhase threadPhase);

struct newlz_vtable;
struct rrArenaAllocator;

void Mermaid_FillVTable(
	newlz_vtable * pvtable,
	OodleLZ_Compressor compressor,
	SINTa raw_len,
	OodleLZ_CompressionLevel level,
	const OodleLZ_CompressOptions * pOptions,
	const U8 * dictionaryBase,	
	const U8 * raw,
    rrArenaAllocator * arena);

OODLE_NS_END

//===========================================================
