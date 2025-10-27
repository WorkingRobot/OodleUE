// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(parent,OodleAPI_LZ_Compressors)
#ifndef LZB_H
#define LZB_H

#include "rrbase.h"
#include "oodlelzpub.h"

OODLE_NS_START

struct LRMCascade;

//=======================================================

SINTa LZB_Compress(const U8 * rawBuf,U8 * compBuf,SINTa rawLen,OodleLZ_CompressionLevel level,
                            const OodleLZ_CompressOptions * pOptions RADDEFAULT(NULL),
                            const U8 * dictionaryBase RADDEFAULT(NULL),
                            const LRMCascade * lrm RADDEFAULT(NULL));

//=======================================================

struct LZQuantumHeader;

S32 LZB_DecodeOneQuantum(U8 * decPtr,U8 * decPtrEnd,const U8 * compPtr,const U8 * compPtrEnd,
							const LZQuantumHeader & LZQH,
							const U8 * decBuf, SINTr decBufferSize,
							rrbool isSlidingWindow,
							const U8 * dictionaryBase);

//=======================================================

#define LZB_SLIDING_WINDOW_POW2	16
#define LZB_SLIDING_WINDOW_SIZE	(1<<LZB_SLIDING_WINDOW_POW2)


 typedef struct OodleLZB_CompressFast_Context OodleLZB_CompressFast_Context;
/* Opaque context object for $OodleLZB_CompressFast_Context

	Free with $OodleLZB_CompressFast_FreeContext
*/

#define LZB_BITS_USE_DEFAULT	(-1) 
/* Pass for any of the LZB bits paramemters to use default

*/

  OodleLZB_CompressFast_Context *  OodleLZB_CompressFast_AllocContext(S32 hashTableBits,rrbool slidingWindow,U8 * window RADDEFAULT(NULL));
/* Allocate a $OodleLZB_CompressFast_Context

	$:hashTableBits	log2 of the lzB hash table size, typically 17-20 or $LZB_BITS_USE_DEFAULT
	$:slidingWindow	true if sliding window, else linear compress
	$:window	the sliding window memory, if NULL one will be allocated; if not null, must be at lease two-to-the-slidingWindowBits	
	$:return	context  allocated
	
	Free with $OodleLZB_CompressFast_FreeContext
	
	AllocContext also does $OodleLZB_CompressFast_ResetContext.  You should not do an initial reset right after AllocContext,	it has been done.
*/

  void  OodleLZB_CompressFast_FreeContext(OodleLZB_CompressFast_Context * context);
/* Free a $OodleLZB_CompressFast_Context

	$:context	the OodleLZB_CompressFast_Context to free

*/

  void  OodleLZB_CompressFast_ResetContext(OodleLZB_CompressFast_Context * context);
  
  void  OodleLZB_CompressFast_ResetContext(OodleLZB_CompressFast_Context * context,
												void * window,
									S32 slidingWindowSize,
									rrbool isSlidingWindow);
/* Reset a $OodleLZB_CompressFast_Context
	
	$:context	the OodleLZB_CompressFast_Context to reset
	
	Reset the context so that future calls to $OodleLZB_CompressFast_WithContext produce data that is independent of previous data.
	
	When using a $OodleLZB_CompressFast_Context on a different stream, you must reset the context, either by calling this.

	When moving to the next chunk within a single array, you can make it independent of the previous by calling $OodleLZB_CompressFast_ResetContext.
	
	DO NOT call $OodleLZB_CompressFast_ResetContext and also pass _seekChunkReset_ in the $OodleLZ_CompressOptions ; if you do, it may get reset twice, which is benign but a waste of time.

	DO NOT call $OodleLZB_CompressFast_ResetContext after the initial allocation.

*/
 
  SINTa  OodleLZB_CompressFast_WithContext (OodleLZB_CompressFast_Context * context,
										const void * rawBuf,SINTa rawLen,void * compBuf,
										OodleLZ_CompressionLevel level_fast_or_veryfast,
										const OodleLZ_CompressOptions * pOptions RADDEFAULT(NULL),
										OodleLZ_EncoderHeaders headers RADDEFAULT(OodleLZ_EncoderHeaders_Default));
/* Compress some data, with provided context

	$:context	compression context to use
	$:rawBuf	raw data to compress
	$:rawLen	number of bytes in rawBuf to compress
	$:compBuf	pointer to write compressed data to ; should be at least $OodleLZ_GetCompressedBufferSizeNeeded
	$:level_fast_or_veryfast	level of compression; must be OodleLZ_CompressionLevel_Fast or OodleLZ_CompressionLevel_VeryFast
	$:pOptions	(optional) options; if NULL, $OodleLZ_CompressOptions_GetDefault is used
	$:headers	(optional) which headers to write; see $OodleLZ_EncoderHeaders
	$:return	size of compressed data written, or $OODLELZ_FAILED for failure
	
	Compress data using LZB and supplied context.
	
	To produce normal OodleLZ encoded data that can be decoded with $OodleLZ_Decompress , use OodleLZ_EncoderHeaders_Default.

	To produce data for streaming network transmission, use $OodleLZ_EncoderHeaders_OnlyFirstBlockHeader , and make sure $(OodleLZ_CompressOptions:seekChunkReset) is off.  Streaming data can only be decoded with $OodleLZDecoder_DecodeSome.
	
*/

//=======================================================

OODLE_NS_END

#endif // LZB_H
