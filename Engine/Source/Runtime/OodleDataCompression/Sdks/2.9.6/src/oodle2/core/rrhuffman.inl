// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_HUFFMANINL__
#define __RADRR_HUFFMANINL__

//#include "../../spu/rrSPUUtil.h"
//#include "spu/spu_rrSPUUtil.h"
		
// user must #define RR_HUFFMAN_FAST_DECODE_BITS before including this

OODLE_NS_START

// this appears to be fastest on Cell SPE and is lower mem, so WTF :
#define HUFF_FAST_CODELEN_TYPE	U8
#define HUFF_FAST_SYMBOL_TYPE	U16


// rrHuffman should be aligned to 32 or something
struct rrHuffman
{
	// put at the front for align :
    RR_VARBITSTYPE codePrefixByLen[32];
    RR_VARBITSTYPE firstBranchCodeSL[32];

    //UINTr firstBranchCode[32];
    S32 numCodesOfLen[32];
    
    rrbool ownsMemory;

    S32 numSymbols; // # of symbols in alphabet
    S32 gotNumSymbols; // # of symbols with non-zero count
    S32 numFastDecodeBits; // table size is 1<<bits
    S32 oneChar;    // if gotNumSymbols == 1 , they're all this , else this is the MPS (most probable symbol) (this is not set in all pathways)
	S32 topSym;
	
    // you can encode or decode just from this if you want to :
    S32 minCodeLen,maxCodeLen;
    U8 * RADRESTRICT codeLenTable;

    // encodeTable and decodeTable use the same memory, so only one can exist at a time
    U32 * RADRESTRICT encodeTable; // the binary code for each symbol
    //U32 * RADRESTRICT decodeTable; // indexed by a packed code; there are numSymbols of them
    HUFF_FAST_SYMBOL_TYPE * RADRESTRICT decodeTable; // indexed by a packed code; there are numSymbols of them

	// FastDecodeTable :
	//	slightly better as two tables for PPC & SPU
	//  on x86 these could be in a 4-byte struct
    HUFF_FAST_CODELEN_TYPE  * RADRESTRICT fastDecode_CodeLen;
    HUFF_FAST_SYMBOL_TYPE * RADRESTRICT fastDecode_Symbol;
};

//===========================================================================================

#define RR_HUFFMAN_UNROLL_DECODE_MAX_CODELEN	16

// only unroll to 16 cuz maxCodeLen <= 16
#define HUFF_FAST_UNROLL_1_MAX( XX ) \
	XX(1) XX(2) XX(3) XX(4) \
	XX(5) XX(6) XX(7) XX(8) \
	XX(9) XX(10) XX(11) XX(12) \
	XX(13) XX(14) XX(15) XX(16)

#define HUFF_TEST_NBITSON(num)	((RR_VARBITSTYPE)(((1)<<(num))-1))

// rrHuffman_Decode_New2 identical speed to rrHuffman_Decode_New1
static RADFORCEINLINE UINTr rrHuffman_Decode_Unroll_Start1(const rrHuffman *HI , rrVarBits_FuncArgs(vb))
{
	rrVarBits_Temps();
	RR_ASSERT( rrVarBits_BitLen(vb) >= HI->maxCodeLen );
	RR_ASSERT( HI->maxCodeLen <= RR_HUFFMAN_UNROLL_DECODE_MAX_CODELEN );
	RR_ASSERT( HI->gotNumSymbols > 1 );
	
    const RR_VARBITSTYPE * RADRESTRICT codePrefixByLen = HI->codePrefixByLen;
    const HUFF_FAST_SYMBOL_TYPE * RADRESTRICT decodeTable = HI->decodeTable;
	const RR_VARBITSTYPE * RADRESTRICT firstBranchCodeSL = HI->firstBranchCodeSL;
    
	#define rrHuffman_Decode_Unroll_Finish(curCodeLen) return decodeTable[ (rrVarBits_Get_C(vb,curCodeLen)) - codePrefixByLen[curCodeLen] ]
	#define rrHuffman_Decode_Unroll_Branch(x) 	 if ( rrVarBits_Bits(vb) < firstBranchCodeSL[x] ) rrHuffman_Decode_Unroll_Finish(x);

	#define HUFF_FAST_UNROLL(x)	rrHuffman_Decode_Unroll_Branch(x)
	
	HUFF_FAST_UNROLL_1_MAX( HUFF_FAST_UNROLL );

    #undef HUFF_FAST_UNROLL
    
	// must be an all-1's bit buffer of maxCodeLen :
	RR_ASSERT( rrVarBits_Peek(vb,RR_HUFFMAN_UNROLL_DECODE_MAX_CODELEN) == HUFF_TEST_NBITSON(RR_HUFFMAN_UNROLL_DECODE_MAX_CODELEN) ); // can't handle all 1's
	// this is safer for fuzz :
	// all-1's code must be the longest one by canonical order
	RR_BITLENTYPE curCodeLen = HI->maxCodeLen;
	RR_ASSERT( rrVarBits_Peek(vb,curCodeLen) == HUFF_TEST_NBITSON(curCodeLen) );
	RR_ASSERT( ( HUFF_TEST_NBITSON(curCodeLen) - codePrefixByLen[curCodeLen] ) == (RR_VARBITSTYPE)(HI->gotNumSymbols-1) );
	rrVarBits_Use_V(vb,curCodeLen);
	// decodeTable[] is just the symbols in canonical (code length) order :
	return decodeTable[ HI->gotNumSymbols-1 ]; 
}

// you must #define RR_HUFFMAN_FAST_DECODE_BITS to match what you built rrHuffman with
#ifdef RR_HUFFMAN_FAST_DECODE_BITS

// this function must be static, or the compiler will only buildone version of this
//	function even though I have different settings for RR_HUFFMAN_FAST_DECODE_BITS

/*
#ifdef _MSC_VER
#pragma message( "RR_HUFFMAN_FAST_DECODE_BITS = " RR_STRINGIZE_DELAY( RR_HUFFMAN_FAST_DECODE_BITS ) )
#endif
*/

static RADFORCEINLINE UINTr rrHuffman_Decode_Unroll_AfterFast(const rrHuffman * RADRESTRICT HI , rrVarBits_FuncArgs(vb))
{
	// Unroll_AfterFast must handle degenerate case that FastDecode doesn't :
	// seems to be slightly faster to NOT special case this
	// the below general case can also handle the gotNumSymbols == 1 case
	// 356.12 mb/s , 355.8
	/*
	// 354.39 mb/s , 353
	if ( HI->gotNumSymbols == 1 )
		return HI->oneChar;
	/**/

	//RR_ASSERT( HI->gotNumSymbols >= 2 );
	RR_ASSERT( rrVarBits_BitLen(vb) >= HI->maxCodeLen );
	RR_ASSERT( HI->maxCodeLen <= RR_HUFFMAN_UNROLL_DECODE_MAX_CODELEN );
	RR_ASSERT( HI->maxCodeLen > HI->numFastDecodeBits || rrVarBits_Bits(vb) == (~((RR_VARBITSTYPE)0)) || HI->gotNumSymbols == 1 );
	RR_ASSERT( rrVarBits_Bits(vb) >= HI->firstBranchCodeSL[RR_HUFFMAN_FAST_DECODE_BITS] );
	RR_ASSERT( HI->numFastDecodeBits == RR_HUFFMAN_FAST_DECODE_BITS );
	
	rrVarBits_Temps();
	
    const RR_VARBITSTYPE * RADRESTRICT codePrefixByLen = HI->codePrefixByLen;
    const HUFF_FAST_SYMBOL_TYPE * RADRESTRICT decodeTable = HI->decodeTable;
	const RR_VARBITSTYPE * RADRESTRICT firstBranchCodeSL = HI->firstBranchCodeSL;
    
    // comparison to constant ; cases for bits <= RR_HUFFMAN_FAST_DECODE_BITS should disappear
	#define HUFF_FAST_UNROLL(x)  if ( (x) > RR_HUFFMAN_FAST_DECODE_BITS ) rrHuffman_Decode_Unroll_Branch(x);
	
	HUFF_FAST_UNROLL_1_MAX( HUFF_FAST_UNROLL );

    #undef HUFF_FAST_UNROLL

	// did not choose one of the normal decode branches    
	// must be an all-1's bit buffer of maxCodeLen :
	
	RR_ASSERT( rrVarBits_Peek(vb,RR_HUFFMAN_UNROLL_DECODE_MAX_CODELEN) == HUFF_TEST_NBITSON(RR_HUFFMAN_UNROLL_DECODE_MAX_CODELEN) || HI->gotNumSymbols == 1 ); // can't handle all 1's
	
	// this is safer for fuzz resilience :
	RR_BITLENTYPE curCodeLen = HI->maxCodeLen;
	rrVarBits_Use_V(vb,curCodeLen);
	UINTr ret = decodeTable[ HI->gotNumSymbols-1 ];
	
	if ( HI->gotNumSymbols == 1 )
	{
		RR_ASSERT( curCodeLen == 0 );
		RR_ASSERT( ret == (UINTr)HI->oneChar );
	}
	
	return ret;
}

// NOTEZ : rrHuffman_FastDecode MUST BE "static" to re-evaluate RR_HUFFMAN_FAST_DECODE_BITS

static RADFORCEINLINE UINTr rrHuffman_FastDecode(const rrHuffman * RADRESTRICT HI,rrVarBits_FuncArgs(vb))
{
	RR_ASSERT( HI->numFastDecodeBits == RR_HUFFMAN_FAST_DECODE_BITS );
	RR_ASSERT( rrVarBits_BitLen(vb) >= HI->maxCodeLen );
	RR_ASSERT( rrVarBits_BitLen(vb) >= RR_HUFFMAN_FAST_DECODE_BITS );
	//RR_ASSERT( HI->gotNumSymbols > 1 ); // this code does not work with single symbols
	//RR_ASSERT( rrVarBits_Bits(vb) != (~((RR_VARBITSTYPE)0)) ); // can't handle all 1's
	
	const UINTr jumpBranchCodeSL = HI->firstBranchCodeSL[RR_HUFFMAN_FAST_DECODE_BITS];
	
	if ( RAD_LIKELY( rrVarBits_Bits(vb) < jumpBranchCodeSL ) )
	{
		RR_VARBITSTYPE peek = rrVarBits_Peek(vb,RR_HUFFMAN_FAST_DECODE_BITS);
	    
		RAD_HINT_ALIGNED_PTR( const HUFF_FAST_CODELEN_TYPE * RADRESTRICT , fd_CodeLen , HI->fastDecode_CodeLen,16,0);
		RAD_HINT_ALIGNED_PTR( const HUFF_FAST_SYMBOL_TYPE * RADRESTRICT  , fd_Symbol  , HI->fastDecode_Symbol,16,0);
		
		RR_BITLENTYPE used = fd_CodeLen[peek];
		RR_ASSERT( used <= RR_HUFFMAN_FAST_DECODE_BITS );
		rrVarBits_Use_V(vb,used);
		
		return fd_Symbol[peek];
	}
	else
	{        
		return rrHuffman_Decode_Unroll_AfterFast(HI, rrVarBits_PassArgs(vb) ); 
	}
}

static RADFORCEINLINE UINTr rrHuffman_FastDecode_NoOverflow(const rrHuffman * RADRESTRICT HI,rrVarBits_FuncArgs(vb))
{
	RR_ASSERT( HI->numFastDecodeBits == RR_HUFFMAN_FAST_DECODE_BITS );
	RR_ASSERT( HI->maxCodeLen <= RR_HUFFMAN_FAST_DECODE_BITS );
	RR_ASSERT( rrVarBits_BitLen(vb) >= RR_HUFFMAN_FAST_DECODE_BITS );
	
	/*
	// you should have used HUFF_FILL_DEGENERATE when doing BuildFastDecodeTable
	if ( HI->gotNumSymbols == 1 )
	{
		return HI->oneChar;
	}
	*/
	
	RR_VARBITSTYPE peek = rrVarBits_Peek(vb,RR_HUFFMAN_FAST_DECODE_BITS);
    
	RAD_HINT_ALIGNED_PTR( const HUFF_FAST_CODELEN_TYPE * RADRESTRICT , fd_CodeLen , HI->fastDecode_CodeLen,16,0);
	RAD_HINT_ALIGNED_PTR( const HUFF_FAST_SYMBOL_TYPE * RADRESTRICT  , fd_Symbol  , HI->fastDecode_Symbol,16,0);
	
	RR_BITLENTYPE used = fd_CodeLen[peek];
	RR_ASSERT( used <= RR_HUFFMAN_FAST_DECODE_BITS );
	rrVarBits_Use_V(vb,used);
	
	return fd_Symbol[peek];
}
#endif

/*
RADFORCEINLINE UINTr rrHuffman_Decode(const rrHuffman *HI,rrVarBits_FuncArgs(vb))
{
    // grab minCodeLen then continue :
    return rrHuffman_FinishDecode(HI,rrVarBits_PassArgs(vb),HI->minCodeLen);
}
*/
//#define rrHuffman_Decode(HI,vb) rrHuffman_FinishDecode(HI,vb,HI->minCodeLen)

#define rrHuffman_Decode rrHuffman_Decode_Unroll_Start1

static RADFORCEINLINE void rrHuffman_Encode(const rrHuffman *HI,rrVarBits_FuncArgs(vb),SINTr Symbol)
{
	RR_ASSERT( (rrVarBits_BitLen(vb) + HI->maxCodeLen) <= RR_VARBITSTYPELEN );
	
    UINTr curCode    = HI->encodeTable[Symbol];
    RR_BITLENTYPE curCodeLen = HI->codeLenTable[Symbol];

    RR_ASSERT( curCodeLen != 0 || HI->gotNumSymbols < 2 );
    RR_ASSERT( curCode < ((UINTr)1UL<<curCodeLen) );

    rrVarBits_Put(vb,curCode,curCodeLen);

}

OODLE_NS_END

#endif // __RADRR_HUFFMANINL__
