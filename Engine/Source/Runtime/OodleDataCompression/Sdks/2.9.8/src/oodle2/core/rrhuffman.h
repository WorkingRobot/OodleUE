// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_HUFFMANH__
#define __RADRR_HUFFMANH__

#include "rrbase.h"
#include "rrhuffmandecode.h"


OODLE_NS_START

/* 

rrHuffman

pretty standard huffman routines

typical encode call sequence :

 rrHuffman_Create
 BuildCodeLens
 PackCodeLens
 BuildEncodeTable
 rrHuffman_Encode
 rrHuffman_Encode
 ...
 rrHuffman_Free
 
 decode use :
 
 rrHuffman_Create
 rrHuffman_UnPackCodeLens
 rrHuffman_BuildFastDecodeTable
 rrHuffman_FastDecode
 rrHuffman_FastDecode
 ...
 rrHuffman_Free
 
*/

// basic typedefs and Create/Destroy is in rrHuffmanDecode.h

//-----------------------------------

// ScaleCounts is optional, generally don't use
//	(will be done automatically internally with max = 65535)
//void rrHuffman_ScaleCounts(rrHuffman *HI,U32 * charCounts,int maxCharCount,int maxTotCount);

// RR_HUFFMAN_MAX_CODELEN_LIMIT could be 15 in new code
//	leave it at 16 because we're still allowed to *decode* 16 lens
//	we will only encode up to 15
//	(newlz is 11)
#define RR_HUFFMAN_MAX_CODELEN_LIMIT		16
#define RR_HUFFMAN_ENCODE_CODELEN_LIMIT		15 // <- 15 for new Heuristic

struct rrArenaAllocator;

// WARNING : charCounts can be mutated by BuildCodeLens because it calls ScaleCounts
//	charCounts - there should be numSymbols of them
//	maxCodeLen must fit in varbits (<= 24 is safe)
//	maxCodeLen = RR_HUFFMAN_DEFAULT_CODELEN_LIMIT
void rrHuffman_BuildCodeLens(rrHuffman *HI,const U32 *charCounts,U32 charCountSum, 
								S32 maxCodeLen RADDEFAULT(RR_HUFFMAN_ENCODE_CODELEN_LIMIT),
								rrArenaAllocator * arena RADDEFAULT(NULL),
								rrbool do_package_merge RADDEFAULT(false));

//-----------------------------------
// After you Build or Provide codeLens you can set up for Encode or Decode by calling
//	_BuildEncodeTable or _BuildDecodeTable
//  you can re-call these to switch a Huffman from Encoding to Decoding or vice versa
//	it can only have either an encodeTable or decodeTable - not both

rrbool rrHuffman_BuildEncodeTable(rrHuffman *HI);

// to send the codelens :
void rrHuffman_PackCodeLens(rrHuffman *HI,rrVarBits * BII);

//-----------------------------------
// coding :

// include rrHuffman.inl to get this :
//RADFORCEINLINE void rrHuffman_Encode(const rrHuffman *HI,rrVarBits_FuncArgs(vb),int Symbol)

int rrHuffman_GetCodeLen(const rrHuffman *HI,int Symbol);
int rrHuffman_GetMaxCodeLen(const rrHuffman *HI);

//-----------------------------------

void rrHuffman_PrintCodes(rrHuffman * HI);
void rrHuffman_PrintCodeLens(rrHuffman * HI);
void rrHuffman_PrintEntropies(rrHuffman * HI,U32 * charCounts);

//-------------------------------------------

OODLE_NS_END

#endif // __RADRR_HUFFMANH__
