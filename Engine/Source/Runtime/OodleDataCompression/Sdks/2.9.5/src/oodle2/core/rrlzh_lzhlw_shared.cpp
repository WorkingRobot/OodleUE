// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrlzh_lzhlw_shared.h"
#include "cbradutil.h"
#include "longrangematcher.h"
#include "rrbytepacking.h"
#include "rrhuffman.h"
#include "rrhuffman.inl"
#include "rrarenaallocator.h"
#include "histogram.h"
#include "rrbighash.h"

OODLE_NS_START

#define LZ_HEADER_BOTTOM4_VERSION4	(3<<2)


const int c_overlap_offset_step1add[] = {0, 1, 2, 1, 4, 4, 4, 4};   /* added */
const int c_overlap_offset_step2sub[] = {8, 8, 8, 7, 8, 9,10,11};   /* substracted */

// copygeneric handles overlap and won't overrun
void lz_copygeneric(U8 * dest,SINTa offset,SINTa len)
{
	if ( len < 16 )
	{
		offset = -offset;
		while(len--)
		{
			*dest = dest[offset];
			dest++;
		}
	}
	else
	{
		U8 * end = dest + len;
		
		// back up the len so we don't overrun :
		U8 * wrote = lz_copywordsteptoend_mayoverlap_overrunok(dest,offset,len-7);
		RR_ASSERT( wrote <= end );
		
		offset = -offset;
		while( wrote < end )
		{
			*wrote = wrote[offset];
			wrote++;
		}
	}
}

U32 rrHuffman_ComputeTransmissionCost(const U32 * histogram,int numSymbols,rrArenaAllocator * pAllocator)
{
    U32 histogram_sum = rrSumOfHistogram(histogram,numSymbols);
    if ( histogram_sum <= 1 )
		return 1;
    
	int huffMemSize = rrHuffman_MemorySizeNeeded(numSymbols,0) + 16;
	//void * huffMem = rralloca(NULL,huffMemSize);
	rrScopeArenaAlloc alloc(huffMemSize,pAllocator);
	//rrHuffman_Create expects aligned memory
	void * huffMemPtr = rrAlignUpPointer(alloc.m_ptr,16);
	rrHuffman * HI = rrHuffman_Create(numSymbols,0,huffMemPtr);
	
	// build with no limit; not 100% accurate but much faster :
	//  false = don't change histogram
	rrHuffman_BuildCodeLens(HI,const_cast<U32 *>(histogram),histogram_sum,RR_HUFFMAN_MAX_CODELEN_LIMIT);
	
	U32 bits = 0;
	const U8 * codeLenTable = HI->codeLenTable;
	for(int s=0;s<numSymbols;s++)
	{
		bits += histogram[s] * codeLenTable[s];
	}
		
	//U8 junkBuf[1024];
	RAD_ALIGN(U8,junkBuf[1024],16);
	
	rrVarBits vb;
	rrVarBits_PutOpen(vb.m,junkBuf);
	rrHuffman_PackCodeLens(HI,&vb);
	rrVarBits_PutFlush8(vb.m);
	U32 codeTransmissionSize = rrPtrDiff32( rrVarBits_PutEndPtr( vb.m ) - junkBuf );
	RR_ASSERT( (codeTransmissionSize+8) < sizeof(junkBuf) );
	
	// bits is float :
	U32 costBytes = (bits / 8) + codeTransmissionSize;
	
	return costBytes;
}

// rrLZH_ComputeHuffmanTransmissionCost :
//	cost to send the symbols with huffman - including the huffman code lengths
//	@@ you certain could approximate by just using entropy + a constant code len size estimate
//	rrLZH_ComputeHuffmanTransmissionCost is in BYTES
F64 rrLZH_ComputeHuffmanTransmissionCost(const U32 * histogram,int numSymbols,rrArenaAllocator * pAllocator,const OodleLZ_CompressOptions * pOptions)
{
	F64 cost = rrHuffman_ComputeTransmissionCost(histogram,numSymbols,pAllocator);
	return cost + pOptions->spaceSpeedTradeoffBytes;
}


#if 0
static U8 * LZBlockHeader_Put_v1(const LZBlockHeader & header, U8 * compPtr)
{
    U8 packedHeader = 0;
    
    RR_ASSERT( header.version == RAD_LZ_HEADER_VERSION );
    RR_ASSERT( header.decodeType < RAD_LZ_DECODE_COUNT );
    
    packedHeader |= (header.version); // 2 bits
    
    S32 decodeType = header.decodeType;
    
    if ( decodeType == RAD_LZ_DECODE_LZH )
    {
		decodeType += header.offsetShift;
		RR_ASSERT( decodeType < RAD_LZ_DECODE_LZHLW );
	}
	else
	{
		RR_ASSERT( decodeType >= RAD_LZ_DECODE_LZHLW );
		RR_ASSERT( header.offsetShift == 0 );
	}
			
	// 3 bits :
	packedHeader |= (decodeType)<<2;
	
	RR_ASSERT( packedHeader < (1<<5) );
	packedHeader |= (header.chunkIsMemcpy) ? (1<<5) : 0;
	packedHeader |= (header.chunkIsReset)  ? (1<<6) : 0;
	packedHeader |= (header.chunkHasQuantumCRCs) ? (1<<7) : 0;
	
	*compPtr++ = packedHeader;
	    
	return compPtr;
}

static U8 * LZBlockHeader_Put_v3(const LZBlockHeader & header, U8 * compPtr)
{
    U32 packedHeader = 0;
    
    // V3
    RR_ASSERT( header.version == RAD_LZ_HEADER_VERSION );
    RR_ASSERT( header.decodeType < RAD_LZ_DECODE_COUNT );
    
    packedHeader |= (header.version); // 2 bits
    
    S32 decodeTypeBase = header.decodeType;
    S32 decodeTypeXtra = decodeTypeBase;
    
    // put offsetShift in decodeType for LZH
    if ( decodeTypeBase == RAD_LZ_DECODE_LZH )
    {
		decodeTypeXtra += header.offsetShift;
	}
	else
	{
		RR_ASSERT( header.offsetShift == 0 );
	}
	
	RR_ASSERT( decodeTypeBase < RAD_LZ_DECODE_V3_COUNT_BASE );
	RR_ASSERT( decodeTypeXtra < RAD_LZ_DECODE_V3_COUNT_XTRA );
	
    if ( header.chunkIsMemcpy )
    {
		//RR_ASSERT( header.chunkIsReset ); // NO
		RR_ASSERT( ! header.chunkHasQuantumCRCs );

		U32 top = decodeTypeBase;
		top += (header.chunkIsReset) ? RAD_LZ_DECODE_V3_COUNT_BASE : 0;
		RR_ASSERT( top < (RAD_LZ_DECODE_V3_COUNT_BASE*2) );
		packedHeader |= (top<<2);
    }
    else
    {
		U32 top = 0;
		// 2 bits of flags :
		top |= (header.chunkIsReset) ? 1 : 0;
		top |= (header.chunkHasQuantumCRCs) ? 2 : 0;
		// remainder for decodeType :
		top |= decodeTypeXtra << 2;
		// past memcpy :
		top += RAD_LZ_DECODE_V3_COUNT_BASE*2;
		RR_ASSERT( top < 64 );
		RR_COMPILER_ASSERT( ((RAD_LZ_DECODE_V3_COUNT_XTRA-1)*4 + RAD_LZ_DECODE_V3_COUNT_BASE*2) < 64 );
		packedHeader |= (top<<2);
	}

	RR_ASSERT( packedHeader < 256 );    			
	*compPtr++ = (U8)packedHeader;
	    
	return compPtr;
}
#endif

/*****************

Version 0-3 takes up the whole 2-bit version field
to flag version 4+
I use an illegal value from version 0
bottom 2 bits = version 0
+ 2 more bits of offsetshift = 3

*******************/

U8 * LZBlockHeader_Put(const LZBlockHeader & header, U8 * compPtr)
{
    U32 packedHeader = 0;
    
    // V4
    RR_ASSERT( header.version == RAD_LZ_HEADER_VERSION );
    RR_ASSERT( header.decodeType < RAD_LZ_DECODE_COUNT );
    
    // byte1 : 6 bits of version + 2 bits of flags
    
    // flag version >= 4 ; takes bottom 4 bits
    packedHeader = LZ_HEADER_BOTTOM4_VERSION4; 
    
    packedHeader |= (RAD_LZ_HEADER_VERSION - 4)<<4; // 2 more bits of version
    
    // packedHeader = 12 = 0xC
    
    // memcpy and reset flags :
    packedHeader |= (header.chunkIsMemcpy ? 1 : 0)<<6;
    packedHeader |= (header.chunkIsReset ? 1 : 0)<<7;
    
    // first byte :
    *compPtr++ = U8_check( packedHeader );
    
    // byte2 : decodeType + top 1 bit of flags
    //	  several free bits in the middle can either be more decodeTypes or more flags
    //	  currently decodeType is in 4 bits, so 3 bits are free
    
    S32 decodeTypeXtra = header.decodeType;
    
    // put offsetShift in decodeType for LZH
    if ( decodeTypeXtra == RAD_LZ_DECODE_LZH )
    {
		RR_ASSERT( header.offsetShift >= 0 && header.offsetShift < 3 );
		decodeTypeXtra += header.offsetShift;
	}
	else
	{
		RR_ASSERT( header.offsetShift == 0 );
	}
	
	RR_ASSERT( decodeTypeXtra < RAD_LZ_DECODE_V4_COUNT );
	
	U32 byte2;
	
	byte2 = decodeTypeXtra;
	byte2 |= header.chunkHasQuantumCRCs ? (1<<7) : 0;
	
    *compPtr++ = U8_check( byte2 );
	    
	return compPtr;
}

const U8 * LZBlockHeader_Get(LZBlockHeader * pHeader, const U8 * compPtr)
{
    U8 packedHeader = *compPtr++;
    
    if ( (packedHeader & 0xF) == LZ_HEADER_BOTTOM4_VERSION4 )
    {
		// version >= 4 header
		
		// fail if the file version is newer than my code
		//	but DO load old file versions with new code !
		
		pHeader->version = 4 + ((packedHeader>>4)&3);
		
		if ( pHeader->version > RAD_LZ_HEADER_VERSION )
			return NULL;    
		
		RR_ASSERT( pHeader->version == RAD_LZ_HEADER_VERSION );
		
		pHeader->chunkIsMemcpy = (packedHeader>>6)&1;
		pHeader->chunkIsReset = (packedHeader>>7)&1;
		
		U8 byte2 = *compPtr++;
		
		pHeader->chunkHasQuantumCRCs = (byte2>>7)&1;
		pHeader->decodeType = byte2&0x7F; // 7 bits for decodeType currently
			// (top 3 bits are always zero)
		
		pHeader->offsetShift = 0;
			
		if ( pHeader->decodeType >= RAD_LZ_DECODE_V4_LZH &&
			pHeader->decodeType <= RAD_LZ_DECODE_V4_LZH_MAX )
		{
			pHeader->offsetShift = pHeader->decodeType - RAD_LZ_DECODE_V4_LZH;
			pHeader->decodeType = RAD_LZ_DECODE_LZH;
		}
		
		//RR_ASSERT( pHeader->decodeType < RAD_LZ_DECODE_COUNT );
		if ( pHeader->decodeType >= RAD_LZ_DECODE_COUNT )
			return NULL;
		
		return compPtr;
    }
    else
    {
		// version 0-3 header
    
		// 2 bits :
		pHeader->version = packedHeader & 3;
	    	    	        
		if ( pHeader->version == 0 )
		{
			// this is a v0 header
			
			// 2 bits :
			int decodeType = (packedHeader>>2)&3;
			pHeader->offsetShift = (packedHeader>>4)&3;
			
			RR_ASSERT( decodeType <= RAD_LZ_DECODE_V0_LZHLW );
			
			if ( decodeType == RAD_LZ_DECODE_V0_MEMCPY )
			{
				pHeader->decodeType = RAD_LZ_DECODE_LZH;
				pHeader->chunkIsMemcpy = true;
			}
			else if ( decodeType == RAD_LZ_DECODE_V0_LZH )
			{
				pHeader->decodeType = RAD_LZ_DECODE_LZH;
				pHeader->chunkIsMemcpy = false;
			}
			else if ( decodeType == RAD_LZ_DECODE_V0_LZHLW )
			{
				pHeader->decodeType = RAD_LZ_DECODE_LZHLW;
			}
			else
			{
				// invalid decodeType
				return NULL;
			}
			
			pHeader->chunkIsReset = (packedHeader>>6) & 1;
			pHeader->chunkHasQuantumCRCs = (packedHeader>>7) & 1;
		}
		else if ( pHeader->version == 1 )
		{
			// 3 bits :
			int decodeType = (packedHeader>>2)&7;
			if ( decodeType < RAD_LZ_DECODE_V1_LZHLW )
			{
				RR_ASSERT( decodeType < 4 );
				pHeader->decodeType = RAD_LZ_DECODE_LZH;
				pHeader->offsetShift = decodeType;;
			}
			else
			{
				if ( decodeType == RAD_LZ_DECODE_V1_LZHLW ) decodeType = RAD_LZ_DECODE_LZHLW;
				else if ( decodeType == RAD_LZ_DECODE_V1_LZNIB ) decodeType = RAD_LZ_DECODE_LZNIB;
				else return NULL;
				
				pHeader->decodeType = decodeType;
				pHeader->offsetShift = 0;
			}
				
			// 3 bits of flags :
			pHeader->chunkIsMemcpy = (packedHeader>>5) & 1;
			pHeader->chunkIsReset = (packedHeader>>6) & 1;
			pHeader->chunkHasQuantumCRCs = (packedHeader>>7) & 1;
		}
		else if ( pHeader->version == 2 )
		{	
			pHeader->offsetShift = 0;
				
			U32 top = packedHeader >> 2;
			
			S32 decodeType;
			
			if ( top < RAD_LZ_DECODE_V2_COUNT )
			{
				// memcpy block
				decodeType = top;
				pHeader->chunkIsMemcpy = true;
				pHeader->chunkIsReset = true;
				pHeader->chunkHasQuantumCRCs = false;
			}
			else
			{
				top -= RAD_LZ_DECODE_V2_COUNT;
				
				pHeader->chunkIsMemcpy = false;
				pHeader->chunkIsReset = (top&1);
				pHeader->chunkHasQuantumCRCs = (top>>1)&1;
				decodeType = top >> 2;
			}
			
			if ( decodeType < RAD_LZ_DECODE_V2_LZHLW )
			{
				pHeader->offsetShift = (int) decodeType;
				RR_ASSERT( pHeader->offsetShift >= 0 && pHeader->offsetShift <= 3 );
				pHeader->decodeType = RAD_LZ_DECODE_LZH;
			}
			else
			{
				//RR_ASSERT( decodeType < RAD_LZ_DECODE_V2_COUNT );
			
				// translate :
				switch(decodeType)
				{
				case RAD_LZ_DECODE_V2_LZHLW: decodeType = RAD_LZ_DECODE_LZHLW; break;
				case RAD_LZ_DECODE_V2_LZNIB: decodeType = RAD_LZ_DECODE_LZNIB; break;
				case RAD_LZ_DECODE_V2_LZB16: decodeType = RAD_LZ_DECODE_LZB16; break;
				case RAD_LZ_DECODE_V2_LZBLW: decodeType = RAD_LZ_DECODE_LZBLW; break;
				default: return NULL;
				}
				pHeader->decodeType = decodeType;
			}
			
			RR_ASSERT( pHeader->decodeType < RAD_LZ_DECODE_COUNT );
		}
		else
		{
			RR_ASSERT( pHeader->version == 3 );
			pHeader->offsetShift = 0;
				
			U32 top = packedHeader >> 2;
			
			if ( top < (RAD_LZ_DECODE_V3_COUNT_BASE*2) )
			{
				// memcpy block
				pHeader->decodeType = top%RAD_LZ_DECODE_V3_COUNT_BASE;
				pHeader->chunkIsMemcpy = true;
				pHeader->chunkIsReset = (top>=RAD_LZ_DECODE_V3_COUNT_BASE);
				pHeader->chunkHasQuantumCRCs = false;
			}
			else
			{
				top -= RAD_LZ_DECODE_V3_COUNT_BASE*2;
				
				pHeader->chunkIsMemcpy = false;
				pHeader->chunkIsReset = (top&1);
				pHeader->chunkHasQuantumCRCs = (top>>1)&1;
				pHeader->decodeType = top >> 2;
			
				if ( pHeader->decodeType >=RAD_LZ_DECODE_V3_LZH )
				{
					pHeader->offsetShift = (int) (pHeader->decodeType - RAD_LZ_DECODE_V3_LZH);
					//RR_ASSERT( pHeader->offsetShift >= 0 && pHeader->offsetShift <= 3 );
					if ( pHeader->offsetShift > 3 ) return NULL;
					pHeader->decodeType = RAD_LZ_DECODE_V3_LZH;
				}
			}
			
			RR_ASSERT( pHeader->decodeType < RAD_LZ_DECODE_COUNT );
			
			// translate :
			// (these are expensive nops at the moment)
			switch(pHeader->decodeType)
			{
			case RAD_LZ_DECODE_V3_LZHLW:
				pHeader->decodeType = RAD_LZ_DECODE_LZHLW; break;
			case RAD_LZ_DECODE_V3_LZNIB:
				pHeader->decodeType = RAD_LZ_DECODE_LZNIB; break;
			case RAD_LZ_DECODE_V3_LZB16:
				pHeader->decodeType = RAD_LZ_DECODE_LZB16; break;
			case RAD_LZ_DECODE_V3_LZBLW:
				pHeader->decodeType = RAD_LZ_DECODE_LZBLW; break;
			case RAD_LZ_DECODE_V3_LZA:
				pHeader->decodeType = RAD_LZ_DECODE_LZA; break;
			case RAD_LZ_DECODE_V3_LZNA:
				pHeader->decodeType = RAD_LZ_DECODE_LZNA; break;
			case RAD_LZ_DECODE_V3_LZH+0:
			case RAD_LZ_DECODE_V3_LZH+1:
			case RAD_LZ_DECODE_V3_LZH+2:
			//case RAD_LZ_DECODE_V3_LZH+3:
				pHeader->decodeType = RAD_LZ_DECODE_LZH;
				pHeader->offsetShift = pHeader->decodeType - RAD_LZ_DECODE_V3_LZH;
				break;
			default:
			{
				ooLogError("invalid V3 decodetype\n"); break;
			}
			}
		}
	}
            
    return compPtr;
}

#define PACKEDQH_FLAG_COMPLEN		16383
#define PACKEDQH_FLAG_SHIFT			14
#define PACKEDQH_FLAG_WHOLEMATCH	0
#define PACKEDQH_FLAG_MEMSET		1
#define PACKEDQH_FLAG_MEMCPY		2
#define PACKEDQH_FLAG_UNUSED		3

#define WHOLEMATCH_MOD_BITS_1		15
#define WHOLEMATCH_MOD_BITS_2		7
	
int LZQuantumHeader_PutExpanded(U8 * ptrDest, const U8 * quantumPtr, SINTa quantumLen, rrbool doCRC)
{
	LZQuantumHeader LZQH = { 0 };
	LZQH.compLen = (S32)quantumLen;
	if ( doCRC )
		LZQH.crc = LZQuantumHeader_ComputeCRC(quantumPtr,quantumLen);
	return LZQuantumHeader_Put(ptrDest,&LZQH,doCRC,(S32)quantumLen);
}

int LZQuantumHeader_PutMemset(U8 * ptrDest, const U8 * quantumPtr , SINTa quantumLen)
{
	LZQuantumHeader LZQH = { 0 };
	LZQH.crc = quantumPtr[0];
	return LZQuantumHeader_Put(ptrDest,&LZQH,false,(S32)quantumLen);
}

int LZQuantumHeader_PutWholeMatch(U8 * ptrDest, SINTa offset , SINTa quantumLen)
{
	LZQuantumHeader LZQH = { 0 };
	LZQH.wholeMatchFlag = true;
	LZQH.wholeMatchOffset = offset;
	return LZQuantumHeader_Put(ptrDest,&LZQH,false,(S32)quantumLen);
}

int LZQuantumHeader_Put(U8 * ptrStart,const LZQuantumHeader * pQH, rrbool doCRC, S32 quantumRawLen)
{
	// compLen actually can be ZERO for huff-only Quanta that are all one character!
	RR_ASSERT( pQH->compLen >= 0 );
	RR_ASSERT( pQH->compLen <= quantumRawLen );
	RR_ASSERT( pQH->compLen <= OODLELZ_QUANTUM_LEN );
	RR_ASSERT( quantumRawLen <= OODLELZ_QUANTUM_LEN );
	RR_ASSERT( quantumRawLen > 0 );

	U8 * ptr = ptrStart;
		
	if ( pQH->wholeMatchFlag )
	{
		RR_ASSERT( pQH->huffFlag == false );
		RR_ASSERT( pQH->extraFlag == false );
		
		U16 packed_CompLen = (PACKEDQH_FLAG_WHOLEMATCH<<PACKEDQH_FLAG_SHIFT) | PACKEDQH_FLAG_COMPLEN;
		
		// 2 bytes + 2 or more for match offset
		
		ptr = rrPutBytes_U16(ptr,packed_CompLen);
		
		RR_ASSERT( pQH->wholeMatchOffset > 1 ); // if it was 1, it's a memset
		
		//ptr = rrPutBytes_U24(ptr,pQH->wholeMatchOffset - 1);
		ptr = rrPutVariableModPow2SeriesWBA(ptr,pQH->wholeMatchOffset - 1, WHOLEMATCH_MOD_BITS_1,WHOLEMATCH_MOD_BITS_2); 
	}
	else if ( pQH->compLen == 0 )
	{
		RR_ASSERT( pQH->huffFlag == false );
		RR_ASSERT( pQH->extraFlag == false );
		
		// memset quantum ; 3 bytes
		
		U16 packed_CompLen = (PACKEDQH_FLAG_MEMSET<<PACKEDQH_FLAG_SHIFT) | PACKEDQH_FLAG_COMPLEN;
		
		ptr = rrPutBytes_U16(ptr,packed_CompLen);
		
		// memset case
		// crc is the byte
		U8 val = (U8) pQH->crc;
		RR_ASSERT( val == pQH->crc );
		
		ptr = rrPutBytes_U8(ptr,val);		
	}
	else if ( pQH->compLen >= quantumRawLen )
	{
		RR_ASSERT( pQH->compLen == quantumRawLen );
		RR_ASSERT( pQH->huffFlag == false );
		RR_ASSERT( pQH->extraFlag == false );
		
		// memcpy (uncompressed) case
		// 2 bytes (+ 3 if crc)
				
		U16 packed_CompLen = (PACKEDQH_FLAG_MEMCPY<<PACKEDQH_FLAG_SHIFT) | PACKEDQH_FLAG_COMPLEN;
		
		ptr = rrPutBytes_U16(ptr,packed_CompLen);
		
		if ( doCRC )
		{
			U32 packedCRC = pQH->crc;
	
			ptr = rrPutBytes_U24(ptr,packedCRC);
		}
	}
	else
	{
		// normal case
		U16 packed_compLen = (U16)pQH->compLen - 1;
		RR_ASSERT( packed_compLen < PACKEDQH_FLAG_COMPLEN );
		
		packed_compLen |= pQH->huffFlag ? (1<<14) : 0 ;
		packed_compLen |= pQH->extraFlag ? (1<<15) : 0 ;
		
		ptr = rrPutBytes_U16(ptr,packed_compLen);
		
		// 2 bytes (+ 3 if crc)
		
		if ( doCRC )
		{
			U32 packedCRC = pQH->crc;
	
			ptr = rrPutBytes_U24(ptr,packedCRC);
		}
	}
		
	int compLen = rrPtrDiff32( ptr - ptrStart );
	RR_ASSERT( compLen <= OODLELZ_QUANTUM_HEADER_MAX_SIZE );
	return compLen;
}

int LZQuantumHeader_Get(const U8 * ptrStart,const U8 * ptrEnd,LZQuantumHeader * pQH, rrbool doCRC, S32 quantumRawLen)
{
	const U8 * ptr = ptrStart;
	ptrEnd = RR_MIN(ptrEnd, ptrStart + OODLELZ_QUANTUM_HEADER_MAX_SIZE);

	RR_ZERO(*pQH);	

	if ( ptr+2 > ptrEnd ) return -1;
	U16 packedCompLen16;
	ptr = rrGetBytes_U16(ptr,&packedCompLen16);
	
	FUZZ_MUNGE(packedCompLen16);
	
	U16 packedCompLen14 = packedCompLen16 & PACKEDQH_FLAG_COMPLEN;
	
	if ( packedCompLen14 == PACKEDQH_FLAG_COMPLEN )
	{
		// special case
		int special = packedCompLen16 >> PACKEDQH_FLAG_SHIFT;

		if ( special == PACKEDQH_FLAG_WHOLEMATCH )
		{
			// wholematch
			pQH->wholeMatchFlag = true;
			
			pQH->compLen = 0;
			
			FUZZ_MUNGE(ptr[0]);
			FUZZ_MUNGE(ptr[1]);
	
			//ptr = rrGetBytes_U24(ptr,&pQH->wholeMatchOffset);
			ptr = rrGetVariableModPow2SeriesWBA(ptr,ptrEnd,&pQH->wholeMatchOffset, WHOLEMATCH_MOD_BITS_1,WHOLEMATCH_MOD_BITS_2); 
			if ( ptr == NULL ) return -1;
			
			pQH->wholeMatchOffset ++;
			
			FUZZ_MUNGE((U32 &)pQH->wholeMatchOffset);
			
			RR_ASSERT_IF_NOT_CORRUPT( pQH->wholeMatchOffset > 1 ); // if it was 1, it's a memset
			if ( pQH->wholeMatchOffset <= 1 ) return -1;
		}
		else if ( special == PACKEDQH_FLAG_MEMSET )
		{
			// memset case
			if ( ptr+1 > ptrEnd ) return -1;
			U8 val = *ptr++;
			pQH->crc = val;
		
			pQH->compLen = 0;
		}
		else if ( special == PACKEDQH_FLAG_MEMCPY )
		{
			// uncompressed/memcpy case :
			pQH->compLen = quantumRawLen;
			
			if ( doCRC )
			{
				if ( ptr+3 > ptrEnd ) return -1;
				ptr = rrGetBytes_U24(ptr,&pQH->crc);
			}
		}
		else
		{
			// corruption :
			ooLogError("invalid QH ; unused special value\n");
			return -1;
		}		
	}
	else
	{
		// normal case
		pQH->compLen = packedCompLen14 + 1;
		
		pQH->huffFlag = (packedCompLen16 >> 14)&1;
		pQH->extraFlag = (packedCompLen16 >> 15)&1;
			
		if ( doCRC )
		{
			if ( ptr+3 > ptrEnd ) return -1;
			ptr = rrGetBytes_U24(ptr,&pQH->crc);
		}
	}
	
	return rrPtrDiff32( ptr - ptrStart );	
}

#define PACKED_LQH_FLAG_SHIFT			18
#define PACKED_LQH_FLAG_COMPLEN			((1<<PACKED_LQH_FLAG_SHIFT)-1)
#define PACKED_LQH_FLAG_WHOLEMATCH		0
#define PACKED_LQH_FLAG_MEMSET			1
#define PACKED_LQH_FLAG_MEMCPY			2

int LZLargeQuantumHeader_Put(U8 * ptrStart,const LZQuantumHeader * pQH, rrbool doCRC, S32 quantumRawLen)
{
	// OODLELZ_BLOCK_LEN = 18 bits
	RR_ASSERT( quantumRawLen > 0 && quantumRawLen <= OODLELZ_BLOCK_LEN );
	
	// compLen actually can be ZERO for huff-only Quanta that are all one character!
	RR_ASSERT( pQH->compLen >= 0 && pQH->compLen <= quantumRawLen );
	RR_ASSERT( pQH->compLen <= OODLELZ_BLOCK_LEN );

	U8 * ptr = ptrStart;
	
	if ( pQH->wholeMatchFlag )
	{
		RR_ASSERT( pQH->huffFlag == false );
		RR_ASSERT( pQH->extraFlag == false );
		
		U32 packed_CompLen = (PACKED_LQH_FLAG_WHOLEMATCH<<PACKED_LQH_FLAG_SHIFT) | PACKED_LQH_FLAG_COMPLEN;
		
		// 3 bytes + 2 or more for match offset (max of 5 for offset? = 8 total)
		
		ptr = rrPutBytes_U24(ptr,packed_CompLen);
		
		RR_ASSERT( pQH->wholeMatchOffset > 1 ); // if it was 1, it's a memset
		
		ptr = rrPutVariableModPow2SeriesWBA(ptr,pQH->wholeMatchOffset - 1, WHOLEMATCH_MOD_BITS_1,WHOLEMATCH_MOD_BITS_2); 
	}
	else if ( pQH->compLen == 0 )
	{
		RR_ASSERT( pQH->huffFlag == false );
		RR_ASSERT( pQH->extraFlag == false );
		
		// memset quantum ; 4 bytes
		
		U32 packed_CompLen = (PACKED_LQH_FLAG_MEMSET<<PACKED_LQH_FLAG_SHIFT) | PACKED_LQH_FLAG_COMPLEN;
		
		ptr = rrPutBytes_U24(ptr,packed_CompLen);
		
		// memset case
		// crc is the byte
		U8 val = (U8) pQH->crc;
		RR_ASSERT( val == pQH->crc );
		
		ptr = rrPutBytes_U8(ptr,val);		
	}
	else if ( pQH->compLen >= quantumRawLen )
	{
		RR_ASSERT( pQH->compLen == quantumRawLen );
		RR_ASSERT( pQH->huffFlag == false );
		RR_ASSERT( pQH->extraFlag == false );
		
		// memcpy (uncompressed) case
		// 3 bytes (+ 3 if crc)
		
		// -> this should never be used, if large quantum expands, we'll send the BLOCK as uncompressed
		
		U32 packed_CompLen = (PACKED_LQH_FLAG_MEMCPY<<PACKED_LQH_FLAG_SHIFT) | PACKED_LQH_FLAG_COMPLEN;
		
		ptr = rrPutBytes_U24(ptr,packed_CompLen);
		
		if ( doCRC )
		{
			U32 packedCRC = pQH->crc;
	
			ptr = rrPutBytes_U24(ptr,packedCRC);
		}
	}
	else
	{
		// normal case
		U32 packed_compLen = pQH->compLen - 1;
		RR_ASSERT( packed_compLen < PACKED_LQH_FLAG_COMPLEN );
		
		packed_compLen |= pQH->huffFlag ? (1<<PACKED_LQH_FLAG_SHIFT) : 0 ;
		packed_compLen |= pQH->extraFlag ? (2<<PACKED_LQH_FLAG_SHIFT) : 0 ;
		
		ptr = rrPutBytes_U24(ptr,packed_compLen);
		
		// 3 bytes (+ 3 if crc)
		
		if ( doCRC )
		{
			U32 packedCRC = pQH->crc;
	
			ptr = rrPutBytes_U24(ptr,packedCRC);
		}
	}
		
	int compLen = rrPtrDiff32( ptr - ptrStart );
	RR_ASSERT( compLen <= OODLELZ_QUANTUM_HEADER_MAX_SIZE );
	return compLen;
}

int LZLargeQuantumHeader_Get(const U8 * ptrStart,const U8 * ptrEnd,LZQuantumHeader * pQH, rrbool doCRC, S32 quantumRawLen)
{
	RR_ASSERT( quantumRawLen > 0 && quantumRawLen <= OODLELZ_BLOCK_LEN );
	const U8 * ptr = ptrStart;
	ptrEnd = RR_MIN(ptrEnd, ptrStart + OODLELZ_QUANTUM_HEADER_MAX_SIZE );

	RR_ZERO(*pQH);	

	if ( ptr+3 > ptrEnd ) return -1;
	
	FUZZ_MUNGE(((U8 *)ptr)[0]);
	FUZZ_MUNGE(((U8 *)ptr)[1]);
	FUZZ_MUNGE(((U8 *)ptr)[2]);
	
	U32 header;
	ptr = rrGetBytes_U24(ptr,&header);

	FUZZ_MUNGE(*((U8 *)ptr));

	U32 packedCompLen = header & PACKED_LQH_FLAG_COMPLEN;
	
	if ( packedCompLen == PACKED_LQH_FLAG_COMPLEN )
	{
		// special case
		int special = header >> PACKED_LQH_FLAG_SHIFT;

		if ( special == PACKED_LQH_FLAG_WHOLEMATCH )
		{
			// wholematch
			pQH->wholeMatchFlag = true;
			
			pQH->compLen = 0;
			
			FUZZ_MUNGE(ptr[0]);
			FUZZ_MUNGE(ptr[1]);
			
			//ptr = rrGetBytes_U24(ptr,&pQH->wholeMatchOffset);
			ptr = rrGetVariableModPow2SeriesWBA(ptr,ptrEnd,&pQH->wholeMatchOffset, WHOLEMATCH_MOD_BITS_1,WHOLEMATCH_MOD_BITS_2); 
			if ( ptr == NULL ) return -1;

			pQH->wholeMatchOffset ++;
			
			FUZZ_MUNGE((U32 &)pQH->wholeMatchOffset);
	
			RR_ASSERT_IF_NOT_CORRUPT( pQH->wholeMatchOffset > 1 ); // if it was 1, it's a memset
			if ( pQH->wholeMatchOffset <= 1 ) return -1;
		}
		else if ( special == PACKED_LQH_FLAG_MEMSET )
		{
			// memset case
			if ( ptr+1 > ptrEnd ) return -1;
			U8 val = *ptr++;
			pQH->crc = val;
		
			pQH->compLen = 0;
		}
		else if ( special == PACKED_LQH_FLAG_MEMCPY )
		{
			// uncompressed/memcpy case :
			pQH->compLen = quantumRawLen;
			
			if ( doCRC )
			{
				if ( ptr+3 > ptrEnd ) return -1;
				ptr = rrGetBytes_U24(ptr,&pQH->crc);
			}
		}
		else
		{
			// corruption :
			ooLogError("invalid QH ; unused special value\n");
			return -1;
		}		
	}
	else
	{
		// normal case
		pQH->compLen = packedCompLen + 1;
		
		pQH->huffFlag = (header >> PACKED_LQH_FLAG_SHIFT)&1;
		pQH->extraFlag = (header >> PACKED_LQH_FLAG_SHIFT)&2;
			
		if ( doCRC )
		{
			if ( ptr+3 > ptrEnd ) return -1;
			ptr = rrGetBytes_U24(ptr,&pQH->crc);
		}
	}
	
	return rrPtrDiff32( ptr - ptrStart );	
}

rrbool MemsetQuantum_Test(const U8 * rawBuf,int rawLen)
{
	return rrIsMemset(rawBuf,rawLen);
}


rrbool CheckWholeMatchQuantum_LRM(LZQuantumHeader * pQH,const U8 * raw,SINTa rawPos,S32 quantumLen,const LRMSet * lrmset,SINTa lastWholeMatchOffset,SINTa dictionarySize)
{
	// minimum len for wholeMatch :
	if ( quantumLen < 8 )
		return false;
			
	const U8 * curPtr = raw + rawPos;
	
	if ( lastWholeMatchOffset > 0 )
	{
		// first check lastWholeMatchOffset :
		if ( memcmp(curPtr,curPtr-lastWholeMatchOffset,quantumLen) == 0 )
		{
			// yes, it's good :
			pQH->wholeMatchFlag = true;
			pQH->compLen = 0;
			pQH->wholeMatchOffset = lastWholeMatchOffset;
			return true;
		}
	}
	
	if ( lrmset != NULL )
	{
		SINTa offset = LRMSet_CheckWholeMatchQuantum(lrmset,raw,rawPos,quantumLen);
		if ( offset )
		{
			if ( dictionarySize == 0 || offset < dictionarySize )
			{		
				pQH->wholeMatchFlag = true;
				pQH->compLen = 0;
				pQH->wholeMatchOffset = offset;
				return true;
			}
		}
	}

	return false;
}

U32 rrLZH_CRC_Block(const U8 * pData,SINTa len)
{
	//SIMPLEPROFILE_SCOPE_N(rrLZH_CRC_Block,len);
	//return rrBigHash32_Scalar(pData,len);
	return (U32) rrBigHash64_SIMD(pData,len);
}

U32 LZQuantumHeader_ComputeCRC(const U8 * pData,SINTa len)
{
	U32 crc32 = rrLZH_CRC_Block(pData,len);
	
	//U32 crc16 = ((crc32) + (crc32>>16))&0xFFFF;
	//return crc16;
	
	// NO BitShuffle makes it worse
	//U32 shuff = rrBitShuffle(crc32);
	U32 shuff = crc32;
	
	return shuff & (0xFFFFFF); // 24 bit CRC now
	
	// rrBitShuffle is bad
	//U32 crc15 = rrBitShuffle(crc32) & ((1<<15)-1);
	//U32 crc15 = crc16 & ((1<<15)-1);
	//return crc15;
}

OODLE_NS_END
