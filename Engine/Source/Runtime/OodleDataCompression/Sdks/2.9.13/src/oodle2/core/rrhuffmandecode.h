// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_HUFFMANDECODE_H__
#define __RADRR_HUFFMANDECODE_H__

#include "rrbase.h"
#include "rrvarbits.h"

/**

see comments in rrHuffman.h

Note : to use FastDecode :

you must #define RR_HUFFMAN_UNROLL_DECODE_MAX_CODELEN in your code before you include rrHuffman.inl
so that it can see it as a constant.

You must call rrHuffman_Create() with the same number of fastdecode bits as your #define

eg.

#define RR_HUFFMAN_UNROLL_DECODE_MAX_CODELEN	8
#include "rrhuffman.inl"

rrHuffman_Create(256,RR_HUFFMAN_UNROLL_DECODE_MAX_CODELEN,NULL);

**/

OODLE_NS_START

struct rrHuffman;

//-----------------------------------

// rrHuffman_Create : # of symbols in alphabet , 
//  # of bits for fastdecode table (use 0 on encode)
//  memory : pass this in if you don't want us to allocate, pass  NULL and we'll allocate
//	NOTE : if you pass in memory it must be 16 byte aligned !
rrHuffman * rrHuffman_Create(int numSymbols, int numFastDecodeBits, void * memory);

// duplicate "from" ; if memory is provided, use it, else allocate
rrHuffman * rrHuffman_CreateCopy(const rrHuffman * from, void * memory);

void rrHuffman_Free(rrHuffman * HI); // this is a NOP if you passed in your own memory to Create

// MemorySizeNeeded : tells you how much space you must provide if you pass the memory pointer to Create
int rrHuffman_MemorySizeNeeded(int numSymbols, int numFastDecodeBits);

//-------------------------------------------------------

// if you make your own codelens, I can encode/decode from them :
// codeLens - numSymbols of them - 0 means symbol does not occur
rrbool rrHuffman_ProvideCodeLens(rrHuffman *HI,const U8 *codeLens);

//-------------------------------------------------------

rrbool rrHuffman_BuildDecodeTable(rrHuffman *HI); // generally do not call this, just call BuildFastDecodeTable
rrbool rrHuffman_BuildFastDecodeTable(rrHuffman *HI, rrbool fill_when_degenerate RADDEFAULT(0)); // calls BuildDecodeTable , don't do both
// fill_when_degenerate should be used for rrHuffman_FastDecode_NoOverflow

rrbool rrHuffman_UnPackCodeLens(rrHuffman *HI,rrVarBits * BII);

//-------------------------------------------------------
// in rrHuffman.inl :

//static RADFORCEINLINE UINTr rrHuffman_FastDecode(const rrHuffman * HI,rrVarBits_FuncArgs(vb))
//RADFORCEINLINE UINTr rrHuffman_Decode(const rrHuffman *HI , rrVarBits_FuncArgs(vb))

//-------------------------------------------------------

OODLE_NS_END

#endif

