// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlebase.h"
#include "rrbase.h"
#include "templates/rrstl.h"
#include "rrmemutil.h"
#include "rrmath.h"
#include "oodlelzpub.h"
#include "oodlelzcompressors.h"
#include "lzasserts.h"

OODLE_NS_START

/***************

01/19/13 : new terminology :

Quantum is 16k and is the streaming unit + huffman reset unit
Block is 256k is the concatenatable chunk unit

"seek chunk" is some multiple of Blocks
decoder can parallelize on seek chunks

"parallel chunking" is used by encoder
must be an integer multiple of Blocks
also must be LRM and dictionary overlap aware

*****************/

#define RR_LZH_MIN_RAW_LEN				(24)    // won't try to pack anything smaller than this

#define RR_LZH_MAX_QUANTA_PER_CHUNK		(OODLELZ_BLOCK_LEN/OODLELZ_QUANTUM_LEN)

// 1/22/13 ! DO_MEMCPY_NEARLY_INCOMPRESSIBLE_QUANTA was on but looks really fishy to me
//	DO_MEMCPY_NEARLY_INCOMPRESSIBLE_QUANTA_SSTBDIV divdes SpaceSpeedTradeoffBytes
//	don't define it to disable that check
#define DO_MEMCPY_NEARLY_INCOMPRESSIBLE_QUANTA_SSTBDIV	2

#define DO_ALLOW_LAST_SHORTER_THAN_MIN 1
//#define DO_ALLOW_LAST_SHORTER_THAN_MIN 0

			
//#define RR_LZH_MAX_HUFF_CODE_LEN	(16)
#define RR_LZH_MAX_HUFF_CODE_LEN	(15)


//=======================================================================================

struct LZBlockHeader // use LZHeader_Put/LZHeader_Get
{
	// 2 bits :
    S32 version;

	// 3 bits :
    S32 decodeType;
    S32 offsetShift; // 0-3 for LZH only

	// @@ variable names use "chunk" - apply to "block"
	// 3 bits :
	rrbool chunkIsMemcpy;
    rrbool chunkIsReset;
    rrbool chunkHasQuantumCRCs;
};

#define RAD_LZ_HEADER_VERSION	(4)  // v0 files can still be read

#define RAD_LZ_DECODE_V0_MEMCPY	 (0)
#define RAD_LZ_DECODE_V0_LZH     (1)
#define RAD_LZ_DECODE_V0_LZHLW   (2)

#define RAD_LZ_DECODE_V1_LZH		(0) // + 4 offsetshits
#define RAD_LZ_DECODE_V1_LZHLW		(4)
#define RAD_LZ_DECODE_V1_LZNIB		(5)
#define RAD_LZ_DECODE_V1_COUNT		(6)

#define RAD_LZ_DECODE_V2_LZH		(0) // + 4 offsetshits
#define RAD_LZ_DECODE_V2_LZHLW		(4)
#define RAD_LZ_DECODE_V2_LZNIB		(5)
#define RAD_LZ_DECODE_V2_LZB16		(6)
#define RAD_LZ_DECODE_V2_LZBLW		(7)
#define RAD_LZ_DECODE_V2_COUNT		(12)

#define RAD_LZ_DECODE_V3_LZHLW		(0)
#define RAD_LZ_DECODE_V3_LZNIB		(1)
#define RAD_LZ_DECODE_V3_LZB16		(2)
#define RAD_LZ_DECODE_V3_LZBLW		(3)
#define RAD_LZ_DECODE_V3_LZA		(4)
#define RAD_LZ_DECODE_V3_LZNA		(5)
#define RAD_LZ_DECODE_V3_LZQ1		(6)
#define RAD_LZ_DECODE_V3_LZH		(7) // + 4 offsetshits
#define RAD_LZ_DECODE_V3_COUNT_BASE		(8)
#define RAD_LZ_DECODE_V3_COUNT_XTRA		(RAD_LZ_DECODE_V3_COUNT_BASE+4)

#define RAD_LZ_DECODE_V4_LZHLW		(0)
#define RAD_LZ_DECODE_V4_LZNIB		(1)
#define RAD_LZ_DECODE_V4_LZB16		(2)
#define RAD_LZ_DECODE_V4_LZBLW		(3)
#define RAD_LZ_DECODE_V4_LZA		(4)
#define RAD_LZ_DECODE_V4_LZNA		(5)
#define RAD_LZ_DECODE_V4_KRAKEN		(6)
#define RAD_LZ_DECODE_V4_LZH		(7) // + 3 offsetshits
#define RAD_LZ_DECODE_V4_LZH_MAX	(9)
#define RAD_LZ_DECODE_V4_MERMAID	(10)
#define RAD_LZ_DECODE_V4_BITKNIT	(11)
#define RAD_LZ_DECODE_V4_LEVIATHAN	(12)
#define RAD_LZ_DECODE_V4_COUNT		(13)


// current version :
#define RAD_LZ_DECODE_LZH		RAD_LZ_DECODE_V4_LZH  
#define RAD_LZ_DECODE_LZHLW		RAD_LZ_DECODE_V4_LZHLW
#define RAD_LZ_DECODE_LZNIB		RAD_LZ_DECODE_V4_LZNIB
#define RAD_LZ_DECODE_LZB16		RAD_LZ_DECODE_V4_LZB16
#define RAD_LZ_DECODE_LZBLW		RAD_LZ_DECODE_V4_LZBLW
#define RAD_LZ_DECODE_LZA		RAD_LZ_DECODE_V4_LZA
#define RAD_LZ_DECODE_LZNA		RAD_LZ_DECODE_V4_LZNA
#define RAD_LZ_DECODE_KRAKEN	RAD_LZ_DECODE_V4_KRAKEN
#define RAD_LZ_DECODE_MERMAID	RAD_LZ_DECODE_V4_MERMAID
#define RAD_LZ_DECODE_BITKNIT	RAD_LZ_DECODE_V4_BITKNIT
#define RAD_LZ_DECODE_LEVIATHAN	RAD_LZ_DECODE_V4_LEVIATHAN
#define RAD_LZ_DECODE_COUNT		RAD_LZ_DECODE_V4_COUNT

// maximum number of bytes needed to write an LZBlockHeader :
// OODLELZ_BLOCK_HEADER_BYTES_MAX was always 1 ; changes to 2 with v4 header
#define OODLELZ_BLOCK_HEADER_BYTES_MAX	(2)

U8 * LZBlockHeader_Put(const LZBlockHeader & header, U8 * compPtr);
const U8 * LZBlockHeader_Get(LZBlockHeader * pHeader, const U8 * compPtr);

OOINLINE rrbool LZBlockHeader_IsReset(SINTa pos, const OodleLZ_CompressOptions * pOptions)
{
	if ( pos == 0 ) return true;
	if ( (pos & (OODLELZ_BLOCK_LEN-1)) != 0 ||
		! pOptions->seekChunkReset ) return false;
	// seekChunkReset is true
	// 
	RR_ASSERT( (pOptions->seekChunkLen % OODLELZ_BLOCK_LEN) == 0 );
	RR_ASSERT( rrIsPow2(pOptions->seekChunkLen) );
	return ( pos & ((SINTa)pOptions->seekChunkLen - 1) ) == 0;
}

//=======================================================================================

// called by Compressor to store the CRC
//	and Decompressor to compare vs whats in the file
U32 rrLZH_CRC_Block(const U8 * pData,SINTa len);
U32 LZQuantumHeader_ComputeCRC(const U8 * pData,SINTa len);

//=======================================================================================

/*

quantum can be :
memset
memcpy
wholematch
normal (compLen in [1,rawLen-1])

*/

struct LZQuantumHeader
{
	S32	compLen; 
		// compLen == 0 is memset quantum
		// compLen == rawLen is memcpy quantum
	U32 crc;
	rrbool wholeMatchFlag;
	SINTa wholeMatchOffset;
	// huffFlag & extraFlag are 2 bits for the compressor to use as it pleases
	//	huffFlag & extraFlag are only sent on "normal" quanta
	rrbool huffFlag;
	rrbool extraFlag;
};
// if compLen == rawLen , it's a memcpy quantum (incompressed)
// if compLen == 0 , it's a memset quantum and crc contains the memset char

// 16 is an over-estimate ; I think the actual max is 8
#define OODLELZ_QUANTUM_HEADER_MAX_SIZE	(16)
		
/**

LZQuantumHeader_Put :

can be variable len based on rawLen (either 4 or 6 bytes)

must not be variable based on compLen, 
because I pre-reserve the space in the compressed stream for this header
before compressing each quantum

-----------

LZH QuantumHeader :
	could use a crc16
	could get rid of rawlen

	1 bit for huff flag
	1 bit for rawlen == 32k
	15 bit complen <= 32k ; if == 32k its memcpy (make 32k zero)
	15 bit crc
	if rawlen != 32k , put 16 more bits


**/

int LZQuantumHeader_Put(U8 * ptrStart,const LZQuantumHeader * pQH, rrbool doCRC, S32 quantumRawLen);

int LZQuantumHeader_Get(const U8 * ptrStart,const U8 * ptrEnd,LZQuantumHeader * pQH, rrbool doCRC, S32 quantumRawLen);

int LZQuantumHeader_PutExpanded(U8 * ptrDest, const U8 * quantumPtr, SINTa quantumLen, rrbool doCRC);

int LZQuantumHeader_PutMemset(U8 * ptrDest, const U8 * quantumPtr , SINTa quantumLen);

int LZQuantumHeader_PutWholeMatch(U8 * ptrDest, SINTa offset , SINTa quantumLen);

//=======================================================================================
// Large Quantum support :

int LZLargeQuantumHeader_Put(U8 * ptrStart,const LZQuantumHeader * pQH, rrbool doCRC, S32 quantumRawLen);
int LZLargeQuantumHeader_Get(const U8 * ptrStart,const U8 * ptrEnd,LZQuantumHeader * pQH, rrbool doCRC, S32 quantumRawLen);

OOINLINE int LZ_DecodeType_IsLargeQuantum(int decodeType)
{
	return	decodeType == RAD_LZ_DECODE_KRAKEN ||
			decodeType == RAD_LZ_DECODE_LEVIATHAN ||
			decodeType == RAD_LZ_DECODE_MERMAID;
}
			
//=======================================================================================

struct rrArenaAllocator;
F64 rrLZH_ComputeHuffmanTransmissionCost(const U32 * histogram,int numSymbols,rrArenaAllocator * pAllocator,const OodleLZ_CompressOptions * pOptions);

/**
rrHuffman_ComputeTransmissionCost
cost to send huffman code lens + symbols with this code
in bytes
**/
U32 rrHuffman_ComputeTransmissionCost(const U32 * histogram,int numSymbols,rrArenaAllocator * pAllocator);

//=========================================================================

rrbool MemsetQuantum_Test(const U8 * rawBuf,int rawLen);

//=========================================================================

#define LZH_LZHLW_HUFFMAN_FAST_DECODE_BITS	10

//=========================================================================

// lzhd_memmov is used for large incompressible blocks
//	it must handle overlap because of "in place" decompression !

#define lzhd_memmov memmove

//=======================================

#ifdef _DEBUG
// so we don't actually call memcpy in debug :
#define lz_copy2(dst,src) RR_PUT16_NATIVE_UNALIGNED(dst, RR_GET16_NATIVE_UNALIGNED(src) )
#define lz_copy4(dst,src) RR_PUT32_NATIVE_UNALIGNED(dst, RR_GET32_NATIVE_UNALIGNED(src) )
#define lz_copy8(dst,src) RR_PUT64_NATIVE_UNALIGNED(dst, RR_GET64_NATIVE_UNALIGNED(src) )
#if defined(_MSC_VER) && defined(__RADX86__)
// do a real 16 copy instead of two 8's so that offset >= 8 issues are stressed
//	(could also probably assert about non-overlap here)
#define lz_copy16(dst,src) _mm_storeu_si128((__m128i *)(dst),_mm_loadu_si128((const __m128i *) (src)))
#else
#define lz_copy16(dst,src) do { lz_copy8(dst,src); lz_copy8(((char *)(dst))+8,((char *)(src))+8); } while(0)
#endif
#elif defined(__RADNEON__) && !defined(__RADARM64__)
// on ARM with NEON, prefer using NEON for copies - much better
OODLE_NS_END
#include <arm_neon.h>
OODLE_NS_START
#define lz_copy2(dst,src) memcpy((char *)(dst), (char *)(src), 2)
#define lz_copy4(dst,src) memcpy((char *)(dst), (char *)(src), 4)
#define lz_copy8(dst,src) vst1_u8((U8*)(dst), vld1_u8((const U8*)(src)))
#define lz_copy16(dst,src) vst1q_u8((U8*)(dst), vld1q_u8((const U8*)(src)))
#elif defined(_MSC_VER) && defined(__RADX86__)
// my VC2005 does not use SSE for memcpy(16)
#define lz_copy2(dst,src) RR_PUT16_NATIVE_UNALIGNED(dst, RR_GET16_NATIVE_UNALIGNED(src) )
#define lz_copy4(dst,src) RR_PUT32_NATIVE_UNALIGNED(dst, RR_GET32_NATIVE_UNALIGNED(src) )
#define lz_copy8(dst,src) RR_PUT64_NATIVE_UNALIGNED(dst, RR_GET64_NATIVE_UNALIGNED(src) )
#define lz_copy16(dst,src) _mm_storeu_si128((__m128i *)(dst),_mm_loadu_si128((const __m128i *) (src)))
#else
// let the optimizer decide how to do the copy :
//	gcc/clang like this
#define lz_copy2(dst,src) memcpy((char *)(dst), (char *)(src), 2)
#define lz_copy4(dst,src) memcpy((char *)(dst), (char *)(src), 4)
#define lz_copy8(dst,src) memcpy((char *)(dst), (char *)(src), 8)
#define lz_copy16(dst,src) memcpy((char *)(dst), (char *)(src), 16)
#endif

#ifdef __RAD64__	
#define lz_copyword lz_copy8
#define lz_wordsize 8
#else
#define lz_copyword lz_copy4
#define lz_wordsize 4
#endif

// dest,source
#define lz_copywordstep(d,s)			do { lz_copyword(d,s); (s) += lz_wordsize; (d) += lz_wordsize; } while(0)
#define lz_copy4step(d,s)				do { lz_copy4(d,s); (s) += 4; (d) += 4; } while(0)
#define lz_copy8step(d,s)				do { lz_copy8(d,s); (s) += 8; (d) += 8; } while(0)
#define lz_copy16step(d,s)				do { lz_copy16(d,s); (s) += 16; (d) += 16; } while(0)

// dest,source,end
// NOTE : can overrun by wordsize-1
#define lz_copywordsteptoend(d,s,e)		do { lz_copywordstep(d,s); } while ((d)<(e))
#define lz_copy4steptoend(d,s,e)		do { lz_copy4step(d,s); } while ((d)<(e))
#define lz_copy8steptoend(d,s,e)		do { lz_copy8step(d,s); } while ((d)<(e))
#define lz_copy16steptoend(d,s,e)		do { lz_copy16step(d,s); } while ((d)<(e))

// lz_copysteptoend_overrunok
// dest,source,len, and then dest = end
#define lz_copywordsteptoend_overrunok(d,s,l)	do { U8 * e=(d)+(l); lz_copywordsteptoend(d,s,e); d=e; } while(0)
#define lz_copy4steptoend_overrunok(d,s,l)	do { U8 * e=(d)+(l); lz_copy4steptoend(d,s,e); d=e; } while(0)
#define lz_copy8steptoend_overrunok(d,s,l)	do { U8 * e=(d)+(l); lz_copy8steptoend(d,s,e); d=e; } while(0)
#define lz_copy16steptoend_overrunok(d,s,l)	do { U8 * e=(d)+(l); lz_copy16steptoend(d,s,e); d=e; } while(0)

#define lz_copysteptoend_overrunok	lz_copywordsteptoend_overrunok

//=======================================

extern const int c_overlap_offset_step1add[8];
extern const int c_overlap_offset_step2sub[8];


static RADFORCEINLINE U8 * lz_copywordsteptoend_mayoverlap_overrunok(U8 * dest,SINTa offset,SINTa len)
{
	RR_ASSERT( len > 8 ); // don't use for tiny lens
	
	const U8 * source = dest - offset;
	U8 * end = dest + len;

    if (offset < 8)
    {
        dest[0] = source[0];
        dest[1] = source[1];
        dest[2] = source[2];
        dest[3] = source[3];
        source += c_overlap_offset_step1add[offset];
        lz_copy4(dest+4, source);
        source -= c_overlap_offset_step2sub[offset];
    }
    else
    {
        lz_copy8(dest,source);
    }
    dest += 8; source += 8;
    
    // lz_copywordsteptoend does its first copy unconditional
    // so we write at least 16 bytes
    lz_copywordsteptoend(dest,source,end);
    
    // returns how far we actually wrote, which may be past end
    return dest;
}

// copygeneric handles overlap and won't overrun
void lz_copygeneric(U8 * dest,SINTa offset,SINTa len);

struct LRMSet;
// dictionarySize = 0 for now limit
rrbool CheckWholeMatchQuantum_LRM(LZQuantumHeader * pQH,const U8 * raw,SINTa rawPos,S32 quantumLen,const LRMSet * lrmset,SINTa lastWholeMatchOffset,SINTa dictionarySize);

OODLE_NS_END
