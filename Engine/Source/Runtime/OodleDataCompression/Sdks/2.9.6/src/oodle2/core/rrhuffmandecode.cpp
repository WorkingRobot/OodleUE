// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrhuffman.h"
#include "rrhuffman.inl"
#include "rrvarbits.h"
#include "rrmath.h"
#include "oodlemalloc.h"
#include "rrlog.h"
#include "templates/rrstl.h"
#include "templates/rralgorithm.h"
#include "oodlebase.h"

#define RRVBC_INLINE
#include "rrvarbitcodes.h"
#include "rrvarbitcodes.cpp"
#include "rrcompressutil.h"

//#include "rrSimpleProf.h"
#include "cbradutil.h"

#include <string.h>
#include <stdlib.h>

#include "sloppymemset.inl"

OODLE_NS_START

RR_COMPILER_ASSERT( RR_HUFFMAN_MAX_CODELEN_LIMIT == RR_HUFFMAN_UNROLL_DECODE_MAX_CODELEN );

//#define PROFILE_HUFFDEC(label)	SIMPLEPROFILE_SCOPE(label)
#define PROFILE_HUFFDEC(label)

//#define rrprintcorruption	RR_ASSERT_BREAK(); rrprintf("corruption : "); rrprintf
//#define rrprintcorruption	rrprintf("corruption : "); rrprintf
#define rrprintcorruption(...)	ooLogError( "LZ corruption : "  __VA_ARGS__ )

#define PACKCODES_TOPSYM
/* if this is on, then in PackCodeLens, the value of the highest
    non-zero-count symbol is written, and codes are only written up
    to that symbol ; this helps compression a good deal in most cases */

//functions:

// 16 byte alignment for SIMD
#define AlignedHuffmanSize	rrAlignUp( (U32)sizeof(rrHuffman), 16 )

int rrHuffman_MemorySizeNeeded(int numSymbols, int numFastDecodeBits)
{
	numSymbols = rrAlignUp(numSymbols+1,4);
	
    rrHuffman * HI = (rrHuffman *)0;
	RR_UNUSED_VARIABLE(HI);
    int codeLenTableBytes = numSymbols*sizeof(HI->codeLenTable[0]);
    codeLenTableBytes = rrAlignUp(codeLenTableBytes,16);
    
    int encodeTableBytes = numSymbols*sizeof(HI->encodeTable[0]);
    int decodeTableBytes = numSymbols*sizeof(HI->decodeTable[0]);
    encodeTableBytes = rrAlignUp(encodeTableBytes,16);
    decodeTableBytes = rrAlignUp(decodeTableBytes,16);
    
    if ( numFastDecodeBits > 0 )
    {
		int fastDecodeTableSize = (1<<numFastDecodeBits);
		// 16 pad for SSE 
		int fastDecodeTableBytes = fastDecodeTableSize * (sizeof(HUFF_FAST_CODELEN_TYPE) + sizeof(HUFF_FAST_SYMBOL_TYPE) ) + 16;  
	
		decodeTableBytes += fastDecodeTableBytes;
    }
    
    int codeTableBytes = RR_MAX(encodeTableBytes,decodeTableBytes); // share space
    
    return AlignedHuffmanSize + codeLenTableBytes + codeTableBytes + 16;
}

/*
 */
rrHuffman * rrHuffman_Create(int numSymbols, int numFastDecodeBits, void * memory)
{    
    int allocSize = rrHuffman_MemorySizeNeeded(numSymbols,numFastDecodeBits);
    
    rrbool ownsMemory = false;
    
    if ( memory == NULL )
    {
		ownsMemory = true;
        //memory = OodleMalloc(allocSize);
        memory = OodleMallocAligned(allocSize,16);
        RR_ASSERT( memory != NULL );
        if ( memory == NULL )
            return NULL;
    }
	
	// if outsider gave me memory, it better be fucking aligned :
    RR_ASSERT( (((UINTa)memory)&0xF) == 0 );
    if ( (((UINTa)memory)&0xF) != 0 )
		return NULL;
    
    rrHuffman * HI = (rrHuffman *) memory;
    //memset(HI,0,allocSize); // clear my struct + the arrays after it
    memset(HI,0,sizeof(rrHuffman)); // just clear my struct

    HI->ownsMemory = ownsMemory;

    HI->numFastDecodeBits = numFastDecodeBits;
    HI->numSymbols = numSymbols;

	int tableSize = rrAlignUp(numSymbols+1,4);
	
    int codeLenTableBytes = tableSize*sizeof(HI->codeLenTable[0]);
    codeLenTableBytes = rrAlignUp(codeLenTableBytes,16);
    
    int encodeTableBytes = tableSize*sizeof(HI->encodeTable[0]);
    int decodeTableBytes = tableSize*sizeof(HI->decodeTable[0]);
    encodeTableBytes = rrAlignUp(encodeTableBytes,16);
    decodeTableBytes = rrAlignUp(decodeTableBytes,16);
    
    
    //int fastDecodeTableBytes = sizeof(FastDecodeItem)<<numFastDecodeBits;   
    int fastDecodeTableSize = (numFastDecodeBits == 0) ? 0 : (1<<numFastDecodeBits);
    //int fastDecodeTableBytes = 3 * fastDecodeTableSize;   
    
    char * ptr = ((char *)memory) + AlignedHuffmanSize;
    
    RR_ASSERT( (((UINTa)ptr)&0xF) == 0 );
    
    HI->codeLenTable = (U8 *) ptr;
    ptr += codeLenTableBytes;
    
    RR_ASSERT( (((UINTa)ptr)&0xF) == 0 );
    
    // share space :
    HI->encodeTable = (U32 *) ptr;
    HI->decodeTable = (HUFF_FAST_SYMBOL_TYPE *) ptr;
		
    if ( numFastDecodeBits > 0 )
    {
	    ptr += decodeTableBytes;

		RR_ASSERT( (((UINTa)ptr)&0xF) == 0 );
    	    
	    HI->fastDecode_CodeLen = (HUFF_FAST_CODELEN_TYPE *) ptr;
		ptr += fastDecodeTableSize * sizeof(HUFF_FAST_CODELEN_TYPE);
		ptr += 16;
    
	    HI->fastDecode_Symbol = (HUFF_FAST_SYMBOL_TYPE *) ptr;
		ptr += fastDecodeTableSize * sizeof(HUFF_FAST_SYMBOL_TYPE);
    }
    else
    {
	    ptr += encodeTableBytes;
    }
    
    // pad :
    ptr += 16;
    
    RR_ASSERT( ptr <= ((char *)memory + allocSize) );
    
    return HI;
}

rrHuffman * rrHuffman_CreateCopy(const rrHuffman * from, void * memory)
{
	rrHuffman * ret = rrHuffman_Create(from->numSymbols,from->numFastDecodeBits,memory);
	if ( ! ret )
		return NULL;
	
    int memSize = rrHuffman_MemorySizeNeeded(from->numSymbols,from->numFastDecodeBits);
    int huffSize = AlignedHuffmanSize;
    
    // copy all the stuff after the rrHuffman stuct :
    memcpy( ret+1, from+1, memSize - huffSize );
    
    // copy the stuff in the huff struct but not the pointers :
    ret->gotNumSymbols = from->gotNumSymbols;
    ret->oneChar = from->oneChar;
    ret->topSym = from->topSym;
    ret->minCodeLen = from->minCodeLen;
    ret->maxCodeLen = from->maxCodeLen;
    memcpy(ret->numCodesOfLen,from->numCodesOfLen,sizeof(from->numCodesOfLen));
    memcpy(ret->codePrefixByLen,from->codePrefixByLen,sizeof(from->codePrefixByLen));
	
	return ret;
}

void rrHuffman_Free(rrHuffman * HI)
{
    if ( HI->ownsMemory )
    {
        OodleFree(HI);
    }
    else
    {
        //RR_ZERO(*HI);
    }
}


typedef U32 t_CodeIndexByLen;
    
static void BuildDecodeTable_Core(const U8 * codeLenTable, int numSymbols, t_CodeIndexByLen CodeIndexByLen[32], HUFF_FAST_SYMBOL_TYPE * RADRESTRICT decodeTable )
{
	/*
	    
    // make decodeTable be the symbols in order :
	//	this is a radix sort, and preserves alpha order for same code len
    for(int i=0;i<numSymbols;i++)
    {
        U32 curCodeLen = codeLenTable[i];
        if ( curCodeLen > 0 ) // unconditional is slightly slower
        {
            t_CodeIndexByLen index = CodeIndexByLen[curCodeLen];
            
            decodeTable[index] = i;
            
            //++ CodeIndexByLen[curCodeLen]; // += 1;
            CodeIndexByLen[curCodeLen] = index+1; // += 1;
        }
    }
    
    /*/
    
	RR_ASSERT( rrIsAlignedPointer(codeLenTable,4) );
    
	// make decodeTable be the symbols in order :
	//	this is a radix sort, and preserves alpha order for same code len
	int numSymbols4 = rrAlignDown(numSymbols,4);
    for(int i=0;i<numSymbols4;i+=4)
    {
		// grab four codelens at a time and check them all for zero :
        U32 codeLens4 = RR_GET32_BE((const U32 *) (codeLenTable + i));
        if ( codeLens4 == 0 )
			continue;

		U32 c1 = (codeLens4>>24)&0xFF;
		t_CodeIndexByLen i1 = CodeIndexByLen[c1];
		CodeIndexByLen[c1] = i1+1;
		decodeTable[i1] = (HUFF_FAST_SYMBOL_TYPE)( i );

		U32 c2 = (codeLens4>>16)&0xFF;
		t_CodeIndexByLen i2 = CodeIndexByLen[c2];
		CodeIndexByLen[c2] = i2+1;
		decodeTable[i2] = (HUFF_FAST_SYMBOL_TYPE)( i+1 );

		U32 c3 = (codeLens4>> 8)&0xFF;
		t_CodeIndexByLen i3 = CodeIndexByLen[c3];
		CodeIndexByLen[c3] = i3+1;
		decodeTable[i3] = (HUFF_FAST_SYMBOL_TYPE)( i+2 );

		U32 c4 = (codeLens4>> 0)&0xFF;
		t_CodeIndexByLen i4 = CodeIndexByLen[c4];
		CodeIndexByLen[c4] = i4+1;
		decodeTable[i4] = (HUFF_FAST_SYMBOL_TYPE)( i+3 );
    }
    
    // finish up one by one :
    for(int i=numSymbols4;i<numSymbols;i++)
    {
        int curCodeLen = (int) codeLenTable[i];
        //if ( curCodeLen > 0 ) // unconditional is slightly slower
        {
            t_CodeIndexByLen index = CodeIndexByLen[curCodeLen];
            CodeIndexByLen[curCodeLen] = index+1;
            decodeTable[index] = (HUFF_FAST_SYMBOL_TYPE)( i );
        }
    }
    
    // would be == numSymbols if we didn't skip zero quads
    RR_ASSERT( CodeIndexByLen[0] <= (t_CodeIndexByLen)numSymbols );
    
    /**/
}

//#pragma optimize( "", on )

/*
 * at this point codeLenTable & numCodesOfLen are filled out
 *
 */
rrbool rrHuffman_BuildDecodeTable(rrHuffman * RADRESTRICT HI)
{
	PROFILE_HUFFDEC(BuildDecodeTable);
	
	// only need to go up to topSym :
    //numSymbols =  HI->numSymbols;
    int numSymbols =  HI->topSym+1;
    RR_ASSERT( numSymbols <= HI->numSymbols );
    
    HUFF_FAST_SYMBOL_TYPE * RADRESTRICT decodeTable = HI->decodeTable;
    
    #ifdef DO_MEMCLEARS
    memclear(decodeTable,numSymbols*sizeof(HI->decodeTable[0]));
    #endif

    RR_VARBITSTYPE * RADRESTRICT codePrefixByLen = HI->codePrefixByLen;
    S32 * RADRESTRICT numCodesOfLen = HI->numCodesOfLen;
    //U32 * firstBranchCode = HI->firstBranchCode;
    RR_VARBITSTYPE * RADRESTRICT firstBranchCodeSL = HI->firstBranchCodeSL;
    
    if ( HI->gotNumSymbols < 2 )
	{
		// 0 code decodes to oneChar
		codePrefixByLen[0] = 0;
		//decodeTable[0].codeLen = 0;
		//decodeTable[0].symbol = HI->oneChar;
		decodeTable[0] = (HUFF_FAST_SYMBOL_TYPE)( HI->oneChar );
		
		/*
		// don't bother
		// this case is handled explicitly by rrHuffman_Decode_Unroll_AfterFast
		for(int i=0;i<32;i++)
		{
			//firstBranchCode[i] = (1UL<<i);
			firstBranchCodeSL[i] = ~ ( (RR_VARBITSTYPE) 0 );
		}
		*/
		return true;
	}	

	S32 minCodeLen = HI->minCodeLen;
	S32 maxCodeLen = HI->maxCodeLen;

    if ( minCodeLen < 1 || maxCodeLen == 0 || maxCodeLen > 30 )
    {
		rrprintcorruption("Invalid Min/Max CodeLen!\n");
		return false;
    }

	// CodeIndexByLen to radix sort
    t_CodeIndexByLen CodeIndexByLen[32];
    
	//*
	// this is only necessary if you don't use Fast Decode :
    for(int i=0;i<(minCodeLen);i++)
    {
		//firstBranchCode[i] = 0;
		firstBranchCodeSL[i] = 0;
    }
    /*/
    memset(firstBranchCodeSL,0,minCodeLen*sizeof(firstBranchCodeSL[0]));
    /**/
    
	// codePrefixByLen in the decoder is =
	//	[first code of that len] - [number of lower len codes]
	// used for making PackedCode
    U32 LastCodePrefix = 0;
    int NumCodesOfLowerLen = 0;
    CodeIndexByLen[minCodeLen] = 0;
    UINTr firstBranchCode = 0;
    for(int i=(minCodeLen);i<=(maxCodeLen);i++)
    {
        CodeIndexByLen[i] = NumCodesOfLowerLen;
        codePrefixByLen[i] = LastCodePrefix - NumCodesOfLowerLen;
		firstBranchCode = LastCodePrefix + numCodesOfLen[i];

		// DecodeTable in bounds check :
		RR_ASSERT( firstBranchCode == 0 || ( firstBranchCode - codePrefixByLen[i] ) <= (UINTr)HI->gotNumSymbols );
		//RR_ASSERT( firstBranchCode == 0 || ( numCodesOfLen[i] - NumCodesOfLowerLen ) <= HI->gotNumSymbols );
	
		firstBranchCodeSL[i] = ( (UINTr) firstBranchCode ) << (RR_VARBITSTYPELEN - i);
		
        NumCodesOfLowerLen += numCodesOfLen[i];
        LastCodePrefix = (LastCodePrefix + numCodesOfLen[i]) << 1;
    }
	
	RR_ASSERT( NumCodesOfLowerLen == HI->gotNumSymbols );
	
	// check codelens for Kraftness
	//	!! this is the main way we ensure that corruption is impossible in Decode() !

    //RR_ASSERT( firstBranchCode[maxCodeLen] == (1UL<<maxCodeLen) );
    // this can fail if codelens were bad due to corruption in the data of UnPackCodeLens
    if ( firstBranchCode != ((UINTr)1<<maxCodeLen) )
    {
		rrprintcorruption("Code Lens not Kraft!\n");
		return false;
    }
       
    RR_ASSERT( firstBranchCodeSL[maxCodeLen] == 0 ); //((UINTr)1<<maxCodeLen) )
    
    //*
    // this is necessary because the 1111 unhandled case can go to the end :
    for(int i=(maxCodeLen);i<32;i++)
    {
		//firstBranchCode[i] = ((UINTr)1<<i);
		firstBranchCodeSL[i] = ~ ( (RR_VARBITSTYPE) 0 );
    }
    /*/
    memset(firstBranchCodeSL + maxCodeLen,0xFF,(32 - maxCodeLen)*sizeof(firstBranchCodeSL[0]));
	/**/
	
	{
	PROFILE_HUFFDEC(Build_Table);
	
	CodeIndexByLen[0] = HI->gotNumSymbols; // junk to back
		
	BuildDecodeTable_Core(HI->codeLenTable,numSymbols,CodeIndexByLen,decodeTable);
    
    }
    
    return true;
}


/*
 * The FastDecodeTable is indexed by N bits
 * provides codelen and symbol
 *
 */

#define FASTDECODE_CODELEN(huff,i)	((huff)->fastDecode_CodeLen[i])
#define FASTDECODE_SYMBOL(huff,i)	((huff)->fastDecode_Symbol[i])

rrbool rrHuffman_BuildFastDecodeTable(rrHuffman * RADRESTRICT HI, rrbool fill_when_degenerate)
{
	// I need normal decodeTable for firstBranchCode and sorted symbol list :
	if ( ! rrHuffman_BuildDecodeTable(HI) )
		return false;
	
	RR_ASSERT( HI->numFastDecodeBits > 0 );
			
	int numJumpBits = HI->numFastDecodeBits;
	
	HUFF_FAST_CODELEN_TYPE * RADRESTRICT fastDecode_CodeLen;
	HUFF_FAST_SYMBOL_TYPE * RADRESTRICT fastDecode_Symbol;
    
    fastDecode_CodeLen = HI->fastDecode_CodeLen;
    fastDecode_Symbol = HI->fastDecode_Symbol;
    
	if( HI->gotNumSymbols < 2 )
	{
		// special degenerate case
		RR_ASSERT( HI->maxCodeLen == 0 );
		RR_ASSERT( HI->decodeTable[0] == HI->oneChar );
		
		// ensure the FastDecode branch cannot be used :
		//HI->firstBranchCodeSL[HI->numFastDecodeBits] = 0;
		memset(HI->firstBranchCodeSL,0,sizeof(HI->firstBranchCodeSL));
		
		if( fill_when_degenerate )
		{
			int jumpCount = 1<<numJumpBits;
			memset(fastDecode_CodeLen,0,jumpCount);
			
			U32 sym = HI->oneChar;
			U32 sympair = (sym<<16) | sym;
				
			rrMemSet32_Aligned(fastDecode_Symbol,sympair,jumpCount*sizeof(U16));
		}
			
		return true;		
	}
		
	
	RAD_HINT_ALIGNED_PTR( const HUFF_FAST_SYMBOL_TYPE * RADRESTRICT , decodeTable , HI->decodeTable,16,0);
	    
	    
	#ifdef RR_DO_ASSERTS
	// a lot of work just for asserting :

	int jumpTableSize = (1<<numJumpBits);	
		
	UINTr jumpBranchCode = HI->firstBranchCodeSL[numJumpBits] >> (RR_VARBITSTYPELEN - numJumpBits);
	if ( HI->maxCodeLen <= numJumpBits )
		jumpBranchCode = jumpTableSize;		
	
	RR_ASSERT( (int)jumpBranchCode <= jumpTableSize );
	
	// I only need jumpTable for the non-branching codes of this len :
	//jumpTableSize = RR_MIN(jumpTableSize,(int)jumpBranchCode);
	//jumpTableSize = RR_MAX(jumpTableSize,1);
	#endif
	
	{
		PROFILE_HUFFDEC(BuildFast_New);
	
		// build jumpTable :
		UINTr curCode = 0;
		int i = 0;
	
		int lowCL = HI->minCodeLen;
		
		if ( RAD_UNLIKELY( lowCL > numJumpBits ) )
		{
			// nothing to do, all codes too long for jump table
			return true;
		}
		
		S32 * RADRESTRICT HI_numCodesOfLen = HI->numCodesOfLen;
		int codeLen;
	
		// for codelens up to alignCL we can do big 16-byte memsets without rollup
		// but it doesn't help because those are rare
		// most of the codes are up in the high-codelen zone
		// see CL 53527
		
		for(codeLen= lowCL; codeLen < numJumpBits;codeLen++)
		{
			int numOfLen = HI_numCodesOfLen[codeLen];
			if ( RAD_UNLIKELY( numOfLen == 0 ) )
				continue;
			
			int numEntriesShift = (numJumpBits - codeLen);
			RR_ASSERT( numEntriesShift >= 1 );
			int numEntries = 1 << numEntriesShift;
						
			int codeLenBytes = numOfLen << numEntriesShift;
			
			RR_ASSERT( codeLenBytes == numOfLen * numEntries );
			
			SloppyMemset_U8(fastDecode_CodeLen+curCode,codeLen,codeLenBytes);
			
			for(int l=0;l<numOfLen;l++)
			{
				HUFF_FAST_SYMBOL_TYPE sym = decodeTable[i++];
				
				SloppyMemset_U16(fastDecode_Symbol+curCode,sym,numEntries*2);
				curCode += numEntries;			
			}
		}
	
		// do the last codelen :
		RR_ASSERT( codeLen == numJumpBits );
		{
			int numOfLen = HI_numCodesOfLen[codeLen];
			if ( RAD_LIKELY( numOfLen > 0 ) )
			{			
				//int numEntriesShift = 0;
				//int numEntries = 1;
							
				int codeLenBytes = numOfLen;

				SloppyMemset_U8(fastDecode_CodeLen+curCode,codeLen,codeLenBytes);
							
				/*		
				// this could be just a memcpy if decodeTable was U16
				//  but not important
				// this branch is only used for code lens == fast decode # of bits ; eg. very long codes
				for(int l=0;l<numOfLen;l++)
				{
					HUFF_FAST_SYMBOL_TYPE sym = decodeTable[i++];

					fastDecode_Symbol[curCode++] = (U16)sym;
				}
				*/
				
				memcpy(fastDecode_Symbol+curCode,decodeTable+i,numOfLen*sizeof(U16));
				curCode += numOfLen;
				//i += numOfLen;
			}
		}
		
		RR_ASSERT( curCode == jumpBranchCode );
	}
	
	//===============================================================
	
	// check it :
	
	#ifdef RR_DO_ASSERTS
	{
		U32 curCode = 0;
		for(int i=0;i<HI->gotNumSymbols;i++)
		{
			// decodeTable gives symbols from lowest code len to highest
			int sym = decodeTable[i];
			int codeLen = HI->codeLenTable[sym];
			if ( codeLen > numJumpBits )
				break; // can't decode, remainder of codes will be branches :
				
			// all these entries point at sym :
			//int numEntries = jumpTableSize >> codeLen;
			int numEntries = 1 << (numJumpBits - codeLen);
			
			RR_ASSERT( numEntries > 0 );
				
			for(int j=0;j<numEntries;j++)
			{
				RR_ASSERT( FASTDECODE_CODELEN(HI,curCode+j) == check_value_cast<HUFF_FAST_CODELEN_TYPE>( codeLen ) );
				RR_ASSERT( FASTDECODE_SYMBOL( HI,curCode+j) == check_value_cast<HUFF_FAST_SYMBOL_TYPE>( sym ) );
			}
			
			curCode += numEntries;
			
			RR_ASSERT( curCode <= (UINTr)jumpTableSize );
		}
		RR_ASSERT( curCode == jumpBranchCode );
	}
	#endif
	
	
	return true;
}

int rrHuffman_GetCodeLen(const rrHuffman *HI,int Symbol)
{
    return HI->codeLenTable[Symbol];
}

int rrHuffman_GetMaxCodeLen(const rrHuffman *HI)
{
    return HI->maxCodeLen;
}

//=========================================================================================
// Code Len packers :

#define PACK_RUNLEN_BITS    (5)
#define PACK_RUNLEN         (1<<PACK_RUNLEN_BITS)

#define RUNLEN_RICE_BITS		(3)
#define RUNLEN_EXPGOLOMB_BITS	(1)

/**

PackCodeLens4 is a super simple list of codes

sym:len
sym:len

it sucks on big alphabets but is best for very low symbol counts

**/

static void RADFORCEINLINE rrHuffman_PackCodeLens4(rrHuffman *HI, rrVarBits_FuncArgs(vbl) )
{
    int log2NumSyms = rrIlog2ceil(HI->numSymbols);

	rrVarBits_Output(vbl);
    rrVarBits_Put(vbl,HI->gotNumSymbols,log2NumSyms);
    
    if ( HI->gotNumSymbols == 0 )
    {
        return;
    }
    else if ( HI->gotNumSymbols == 1 )
    {
		rrVarBits_Output(vbl);
        rrVarBits_Put(vbl,HI->oneChar,log2NumSyms);
        return;
    }
    
    RR_ASSERT( HI->maxCodeLen > 0 );
    
    int log2CodeLen = rrIlog2ceil(HI->maxCodeLen);    
    // log2CodeLen is at most 5
    RR_ASSERT( log2CodeLen <= 5 );

    rrVarBits_Put(vbl,log2CodeLen,3);
    
    int count = 0;
    for(int i=0;i<HI->numSymbols;i++) 
    {
        if ( HI->codeLenTable[i] == 0 )
            continue;

        int len = HI->codeLenTable[i] - 1;
	
		rrVarBits_Output(vbl);
        rrVarBits_Put(vbl,i,log2NumSyms);
        rrVarBits_Put(vbl,len,log2CodeLen);
        count++;
    }
    
    RR_ASSERT( count == HI->gotNumSymbols );
}

static rrbool RADFORCEINLINE rrHuffman_UnPackCodeLens4(rrHuffman *HI, rrVarBits_FuncArgs(vbl) )
{
	PROFILE_HUFFDEC(UnPackCodeLens4);
	
    int log2NumSyms = rrIlog2ceil(HI->numSymbols);
	RR_ASSERT( log2NumSyms > 0 );

	rrVarBits_Temps();
    HI->gotNumSymbols = (int) rrVarBits_Get_V(vbl,log2NumSyms);
    
    if ( HI->gotNumSymbols > HI->numSymbols )
    {
        rrprintcorruption("HI->gotNumSymbols > HI->numSymbols\n");
        return false;
    }
    
    if ( HI->gotNumSymbols == 0 )
    {
        HI->minCodeLen = HI->maxCodeLen = 0;
        return true;
    }
    else if ( HI->gotNumSymbols == 1 )
    {
		rrVarBits_Refill_Safe(vbl);
		
		RR_VARBITSTYPE sym = rrVarBits_Get_V(vbl,log2NumSyms);
		
        if ( sym >= (RR_VARBITSTYPE)HI->numSymbols )
        {
            rrprintcorruption("HI->oneChar >= HI->numSymbols\n");
            return false;
        }
        
        HI->minCodeLen = HI->maxCodeLen = 0;
        HI->oneChar =  check_value_cast<U16>( sym );
		HI->topSym = (S32)sym;
		HI->codeLenTable[sym] = 0;
		HI->decodeTable[0] = (HUFF_FAST_SYMBOL_TYPE) sym;
        
        
        return true;
    }
    
    // only clear codeLens for non-degenerate case !
    memset(HI->codeLenTable,0,HI->numSymbols*sizeof(HI->codeLenTable[0]));
    
    #ifdef DO_MEMCLEARS
    memclear(HI->numCodesOfLen,sizeof(HI->numCodesOfLen));
    #endif
   
    int log2CodeLen = (int) rrVarBits_Get_C(vbl,3);
    
    if ( log2CodeLen > 5 )
    {
        rrprintcorruption("log2CodeLen > 5\n");
        return false;
    }
        
    HI->maxCodeLen = 0;
    HI->minCodeLen = 32;
    HI->topSym = -1;

    // these asserts are not true if fuzz and don't need to be checked
    /*if ( log2CodeLen == 0 )
    {
		// this branch is only needed if VarBits_Get can't do 0
		//	using rrVarBits_Get_0Ok now so don't need a special case for this
		RR_ASSERT( HI->gotNumSymbols == 2 );
	}
	else
	{
		RR_ASSERT( HI->gotNumSymbols > 2 );
	}*/
		
    {
		for(int i=0;i<HI->gotNumSymbols;i++) 
		{
			rrVarBits_Refill_Safe(vbl);

			int sym = (int) rrVarBits_Get_V(vbl,log2NumSyms);
			int len = (int) rrVarBits_Get_0Ok(vbl,log2CodeLen);
	        
			int curCodeLen = len+1;
			
			if ( sym >= HI->numSymbols || curCodeLen > RR_HUFFMAN_MAX_CODELEN_LIMIT )
			{
				rrprintcorruption("sym >= HI->numSymbols || len > RR_HUFFMAN_MAX_CODELEN_LIMIT \n");
				return false;
			}
	        	        
			HI->codeLenTable[sym] =  check_value_cast<U8>( curCodeLen );

			//RR_ASSERT( sym > HI->topSym); 
			if ( sym <= HI->topSym )
			{
				rrprintcorruption("sym <= HI->topSym\n");
				return false;
			}
			HI->topSym = sym;
			
			//if ( curCodeLen > 0 )
			{
				//HI->gotNumSymbols ++;
				HI->numCodesOfLen[curCodeLen] ++;
	            
				HI->maxCodeLen = RR_MAX(HI->maxCodeLen,curCodeLen);
				HI->minCodeLen = RR_MIN(HI->minCodeLen,curCodeLen);
			}
		}
    }

	// The codelen check in the main loop enforces this
	RR_ASSERT( 0 < HI->minCodeLen && HI->minCodeLen <= HI->maxCodeLen && HI->maxCodeLen <= RR_HUFFMAN_MAX_CODELEN_LIMIT );

    return true;
}

#define PRED_INIT(log2)			(log2)*4
#define PRED_GET(state)			(((state) + 2)>>2)
#define PRED_UPDATE(state,val)	(((state)*3 + 2)>>2) + (val)

static void RADFORCEINLINE rrHuffman_PackCodeLens8(rrHuffman *HI, rrVarBits_FuncArgs(vbl) )
{
	// use PackCodeLens4 for tiny alphabets
	RR_ASSERT( HI->gotNumSymbols >= 2 );

    int log2NumSyms = rrIlog2ceil(HI->numSymbols);

    // make histo for deltas :  
    int NumDeltaFromPrevious[32] = { 0 };
    int prevCodeLen4 = PRED_INIT(log2NumSyms);
    for(int i=0;i<=HI->topSym;i++)
    {
		int codeLen = HI->codeLenTable[i];
        if ( codeLen > 0 )
        {
            int pred = PRED_GET(prevCodeLen4);
            int delta = codeLen - pred;
            delta = rrFoldUpNegatives(delta);
            RR_ASSERT( delta < 32 );
            NumDeltaFromPrevious[delta] ++;
            prevCodeLen4 = PRED_UPDATE(prevCodeLen4,codeLen);
        }
    }

	rrVarBits_Output(vbl);
		
    // riceBits is usually best at 1, but on 'book1' it's much better at 2
    int riceBits = 1;

    // check if entropy is better if I bump meanCodeLen up or down
    {
        double bestH = 99999999999.f;
        for(int cur=0;cur<4;cur++)
        {
            double H = EntropyOfCountsRice(NumDeltaFromPrevious,32,cur);
            if ( H < bestH )
            {
                bestH = H;
                riceBits = cur;
            }
        }
        
        rrVarBits_Put(vbl,riceBits,2);
    }
    
    bool firstIsOn =  HI->codeLenTable[0] != 0;
    rrVarBits_Put(vbl, firstIsOn?1:0 ,1);
       
    prevCodeLen4 = PRED_INIT(log2NumSyms);
    
    int i =0;
    for(;;)
    {
        // send a run of 0 lens
        int RunLen = 0;

		if ( firstIsOn && i == 0 )
		{
			// no initial zero run
		}
		else
		{
			/*
			if ( i > HI->topSym )
			{
				rrVarBits_WriteExpGolomb( rrVarBits_PassArgs(vbl) ,0,RUNLEN_EXPGOLOMB_BITS);
				break;
			}
			*/
			while ( i < HI->numSymbols && HI->codeLenTable[i] == 0 )
			{
				RunLen++;
				i++;
			}

			RR_ASSERT( RunLen > 0 );
			RunLen--;
			rrVarBits_WriteExpGolomb( rrVarBits_PassArgs(vbl) ,RunLen,RUNLEN_EXPGOLOMB_BITS);
		}
		
		if ( i >= HI->numSymbols )
			break;
					
        // now send a run of non-zero lens
        //int runStartI = i;
        RunLen = 0;
        
        while ( (i+RunLen) < HI->numSymbols && HI->codeLenTable[i+RunLen] != 0 )
        {
            RunLen++;
        }
        
        RR_ASSERT( RunLen > 0 );
        //rrVarBits_WriteRice( rrVarBits_PassArgs(vbl),RunLen-1,RUNLEN_RICE_BITS);
        rrVarBits_WriteExpGolomb( rrVarBits_PassArgs(vbl) ,RunLen-1,RUNLEN_EXPGOLOMB_BITS);

        for(int j=0;j<RunLen;j++)
        {
            // transmit the difference from meanCodeLen
            int pred = PRED_GET(prevCodeLen4);
            int cl = HI->codeLenTable[i];
            int send = rrFoldUpNegatives(cl - pred);
            
            //RR_ASSERT( send < 24 );
            rrVarBits_WriteRice( rrVarBits_PassArgs(vbl) ,send,riceBits);
            
            //prevCodeLen = HI->codeLenTable[i];
            prevCodeLen4 = PRED_UPDATE(prevCodeLen4,cl);
            i++;
        }
        
        if ( i >= HI->numSymbols )
			break;
    }
    
}

#if 0
static int Get_Golomb_K(double r)
{
    double rk = r;
    for(int k =1;;k++)
    {
        if ( (1.0 + r)*rk <= 1.0 )
            return k;
        rk *= r;
    }
}
#endif

// must be OOINLINE for rrVarBits_FuncArgs
static RADFORCEINLINE 
rrbool rrHuffman_UnPackCodeLens8(rrHuffman * RADRESTRICT HI, rrVarBits_FuncArgs(vbl))
{
	PROFILE_HUFFDEC(UnPackCodeLens8);
	
	rrVarBits_Temps();
	
    const int log2NumSyms = rrIlog2ceil(HI->numSymbols);

    #ifdef DO_MEMCLEARS
    memclear(HI->numCodesOfLen,sizeof(HI->numCodesOfLen));
    #endif

	//rrVarBits_Refill_Safe(vbl);
	
    const int riceBits = (int) rrVarBits_Get_C(vbl,2);

    int HI_gotNumSymbols = 0;
    int HI_topSymPlusOne = 1;
    
	rrVarBits_Refill_Safe(vbl);
		
    int prevCodeLen4 = PRED_INIT(log2NumSyms);

	U32 i = 0;
	const U32 numSymbols = HI->numSymbols;
	U8 * RADRESTRICT codeLenTable = HI->codeLenTable;
	S32 * RADRESTRICT HI_numCodesOfLen = HI->numCodesOfLen;

	// Zero it all at the beginning, then runs don't need to do the clearing piecemeal
	memset(codeLenTable, 0, numSymbols);

	const int firstIsOn = (int) rrVarBits_Get1(vbl);
	if ( !firstIsOn )
	{
		i = 1 + rrVarBits_ReadExpGolomb_Small( rrVarBits_PassArgs(vbl), RUNLEN_EXPGOLOMB_BITS);
		if ( i > numSymbols )
		{
			rrprintcorruption("i + RunLen > packNumSymbols \n");
			return false;
		}
	}

	while (i < numSymbols)
    {
		rrVarBits_Refill_Safe(vbl);

		// read RunLen of non-zeros :
		U32 nzRunLen = 1 + rrVarBits_ReadExpGolomb_Small( rrVarBits_PassArgs(vbl) ,RUNLEN_EXPGOLOMB_BITS);
		if ( nzRunLen > numSymbols - i )
		{
			rrprintcorruption("i + RunLen > packNumSymbols \n");
			return false;
		}

		rrVarBits_Refill_Safe(vbl);

		for(U32 j=0;j<nzRunLen;j++)
		{
			//U32 code = rrVarBits_ReadRice(rrVarBits_PassArgs(vbl),riceBits);
			U32 code = rrVarBits_ReadRice_Small(rrVarBits_PassArgs(vbl),riceBits);
			S32 delta = rrUnfoldNegatives(code);

			rrVarBits_Refill_Safe(vbl);

			int pred = PRED_GET(prevCodeLen4);
			S32 curCodeLen = delta + pred;
			if ( curCodeLen < 1 || curCodeLen > RR_HUFFMAN_MAX_CODELEN_LIMIT )
			{
				rrprintcorruption("curCodeLen < 1 || curCodeLen > RR_HUFFMAN_MAX_CODELEN_LIMIT ) \n");
				return false;
			}

			prevCodeLen4 = PRED_UPDATE(prevCodeLen4,curCodeLen);
			codeLenTable[i] =  check_value_cast<U8>( curCodeLen );

			RR_ASSERT( curCodeLen > 0 );
			S32 count = HI_numCodesOfLen[curCodeLen];
			HI_numCodesOfLen[curCodeLen] = count + 1;

			i++;
		}

		HI_gotNumSymbols += nzRunLen;
		HI_topSymPlusOne = i;

		if ( RAD_UNLIKELY( i >= numSymbols ) )
		{
			break;
		}

		// read next zero runlen
		U32 zRunLen = 1 + rrVarBits_ReadExpGolomb_Small( rrVarBits_PassArgs(vbl), RUNLEN_EXPGOLOMB_BITS);
		if ( zRunLen > numSymbols - i )
		{
			rrprintcorruption("i + RunLen > packNumSymbols \n");
			return false;
		}

		i += zRunLen;
    }
    
    HI->topSym = HI_topSymPlusOne - 1;
    HI->gotNumSymbols = HI_gotNumSymbols;
    
    U32 HI_minCodeLen = 0;
    while ( HI_numCodesOfLen[HI_minCodeLen] == 0 ) HI_minCodeLen++;
    U32 HI_maxCodeLen = 31;
    while ( HI_numCodesOfLen[HI_maxCodeLen] == 0 ) HI_maxCodeLen--;

	// The codelen check in the main loop enforces this
	RR_ASSERT( 0 < HI_minCodeLen && HI_minCodeLen <= HI_maxCodeLen && HI_maxCodeLen <= RR_HUFFMAN_MAX_CODELEN_LIMIT );

    HI->maxCodeLen = HI_maxCodeLen;
    HI->minCodeLen = HI_minCodeLen;
				
    return true;
}




/**

We only use method 4 and 8

4 is good for tiny alphabets
8 is fine in all other cases

---

we tried some other things but took them out
our current method of choice is essentially a riced method 8
(in newlz_encode_hufflens2) that goes for batch processing

**/

void rrHuffman_PackCodeLens(rrHuffman *HI,rrVarBits * vb)
{
	rrVarBits_Locals(vbl);
    rrVarBits_Copy(vbl,vb->m);
    rrVarBits_Output(vbl);
    
    if ( HI->gotNumSymbols <= 4 )
    {
        rrVarBits_Puta0(vbl);
        rrHuffman_PackCodeLens4(HI, rrVarBits_PassArgs(vbl) );
    }
    else
    {
        rrVarBits_Puta1(vbl);
        
	    rrHuffman_PackCodeLens8(HI,rrVarBits_PassArgs(vbl) );
	}
	
    rrVarBits_Copy(vb->m,vbl);
}

rrbool rrHuffman_UnPackCodeLens(rrHuffman *HI,rrVarBits * vb)
{
	rrVarBits_Temps();
	rrVarBits_Locals(vbl);
    rrVarBits_Copy(vbl,vb->m);
    rrVarBits_Refill_Safe(vbl);
        
    rrbool ret = false;
    if ( rrVarBits_Get1(vbl) )
    {
		ret = rrHuffman_UnPackCodeLens8(HI, rrVarBits_PassArgs(vbl));
    }
    else
    {
        ret = rrHuffman_UnPackCodeLens4(HI, rrVarBits_PassArgs(vbl));
    }
    
    rrVarBits_Copy(vb->m,vbl);
    
    return ret;
}

OODLE_NS_END
