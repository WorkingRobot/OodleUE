// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(parent,OodleAPI_LZ)
//idoc(end)
#include "oodlelzpub.h"
#include "oodlelzcompressors.h"

#include "rrlzh_lzhlw_shared.h"
#include "newlz.h"
#include "newlzf.h"
#include "oodlemalloc.h"
#include "rrsimpleprofstub.h"

#include "oodlelzlegacyvtable.h"

#undef RADDEFAULT
#define RADDEFAULT(x)

//===============================================================

OODLE_NS_START


NOOFUNC1 OodleLZ_CompressContext * NOOFUNC2 OodleLZ_CompressContext_Alloc(
					OodleLZ_Compressor	compressor,
					OodleLZ_CompressionLevel level_fast_or_veryfast,
					S32 slidingWindowBits RADDEFAULT(OODLELZCONTEXT_BITS_USE_DEFAULT),
					S32 hashTableBits RADDEFAULT(OODLELZCONTEXT_BITS_USE_DEFAULT),
					const void * window RADDEFAULT(NULL))
{
	if ( ! g_OodleLZLegacyVTable.fp_legacy_OodleLZ_CompressContext_Alloc)
	{
		ooLogError("Legacy LZ vtable not installed");
		return NULL;
	}

	return (*g_OodleLZLegacyVTable.fp_legacy_OodleLZ_CompressContext_Alloc)(compressor,level_fast_or_veryfast,slidingWindowBits,hashTableBits,window);
}

NOOFUNC1 void NOOFUNC2 OodleLZ_CompressContext_Free(OodleLZ_CompressContext * context)
{
	if ( ! g_OodleLZLegacyVTable.fp_legacy_OodleLZ_CompressContext_Free)
	{
		ooLogError("Legacy LZ vtable not installed");
		return;
	}

	return (*g_OodleLZLegacyVTable.fp_legacy_OodleLZ_CompressContext_Free)(context);
}

NOOFUNC1 void NOOFUNC2 OodleLZ_CompressContext_Reset(OodleLZ_CompressContext * context, 
		S32 change_slidingWindowBits RADDEFAULT(OODLELZCONTEXT_BITS_USE_DEFAULT),
		const void * change_window RADDEFAULT(NULL))
{
	if ( ! g_OodleLZLegacyVTable.fp_legacy_OodleLZ_CompressContext_Reset)
	{
		ooLogError("Legacy LZ vtable not installed");
		return;
	}

	return (*g_OodleLZLegacyVTable.fp_legacy_OodleLZ_CompressContext_Reset)(context,change_slidingWindowBits,change_window);
}
 
NOOFUNC1 SINTa NOOFUNC2 OodleLZ_CompressWithContext (OodleLZ_CompressContext * context,
										const void * rawBuf,SINTa rawLen,void * compBuf,
										const OodleLZ_CompressOptions * pOptions RADDEFAULT(NULL),
										OodleLZ_EncoderHeaders headers RADDEFAULT(OodleLZ_EncoderHeaders_Default))
{
	if ( ! g_OodleLZLegacyVTable.fp_legacy_OodleLZ_CompressContext_Reset)
	{
		ooLogError("Legacy LZ vtable not installed");
		-1;
	}

	return (*g_OodleLZLegacyVTable.fp_legacy_OodleLZ_CompressWithContext)(context,rawBuf,rawLen,compBuf,pOptions,headers);
}


void OodleLZ_SeekTable_Log(const OodleLZ_SeekTable * t)
{
	rrprintf("st: compressor : %s , %s \n",
		OodleLZ_Compressor_GetName(t->compressor),
		t->seekChunksIndependent ? "seekChunksIndependent" : "not indep");
		
	rrprintf("st: " RR_S64_FMT " -> " RR_S64_FMT "\n",t->totalRawLen,t->totalCompLen);
	
	rrprintf("st: %d x %d seekchunks\n",t->seekChunkLen,t->numSeekChunks);
	
	//U32 *				seekChunkCompLens;	// array of compressed lengths of seek chunks
	//U32 *				rawCRCs;			// crc of the raw bytes of the chunk (optional; NULL unless $OodleLZSeekTable_Flags_MakeRawCRCs was specified)
}


OODLE_NS_END


//idoc(begin)
	
//idoc(page,OodleLZ_About,About OodleLZ)
//idoc(autolink,on)
//idoc(markdown,on)
/*

## About OodleLZ

OodleLZ consists of two major API groups : $OodleAPI_LZ_Compressors for simple
synchronous memory to memory compression (like $OodleLZ_Compress) (in the Oodle2 Core lib), and $OodleXAPI_LZ_Async
(in the Oodle2 Ext lib)
for helpers that coordinate IO and multi-threaded invocation of the simple compressors
(like $OodleXLZ_ReadAndDecompress_Wide_Async).

The Oodle LZ compressors are lossless generic data compressors.  They offer world-beating decode speed, with
good encode speed and compression ratio.  They are particularly
well suited to binary "structured data" as is typically found in games.

The most basic Oodle LZ APIs ($OodleLZ_Compress and $OodleLZ_Decompress) just compress and decompress from
memory to memory, one whole buffer at a time.

There are several compressors offered in Oodle.  They are listed in the $OodleLZ_Compressor enum.  Each has slightly
different tradeoffs in compression ratio, decode speed, and encode speed.  Some of them have other special purpose uses,
such as sliding windows or incremental encoding.  (feel free to contact oodle@radgametools.com if you have an unusual use case
and need guidance)

For each choice of compressor, you can also vary the $OodleLZ_CompressionLevel.  The compression level just varies the encode speed;
slower encodes give higher compression ratios.  Typically decode speed is unaffected; it's simply trying harder to pack the data
as small as possible in the encoder.

You can also dial decode speed vs compression ratio using the $OodleLZ_CompressOptions:_spaceSpeedTradeoffBytes_ parameter.

Kraken (OodleLZ_Compressor_Kraken) is the best compressor to try first.  It offers superb decode speed with high compression.  It's excellent for data
loading in game, distribution, and most general purpose uses.

Mermaid & Selkie are some of the fastest decompressors in the world.  They provide less compression than Kraken but
super fast decodes.  Selkie is faster than LZ4 and offers more compression.

Leviathan offers very high compression with still excellent decode speed, 10X faster than LZMA/7zip.
Leviathan should be your choice when you need maximum compression and don't mind being 10-50% slower than Kraken.

Hydra is a meta-compressor that selects Leviathan,Kraken, Mermaid or Selkie for you.  See $OodleLZ_About_Hydra

The older Oodle compressors are now deprecated and should not be used (see $Oodle_FAQ_deprecated_compressors).
Stick to the new sea monsters.

Go ahead and try them with the Oodle $examples

All OodleLZ compressors can optionally break their data into "seek chunks" if
$(OodleLZ_CompressOptions:seekChunkReset) is set.  The granularity is set in $(OodleLZ_CompressOptions:seekChunkLen) , but must
be at least 256k ($OODLELZ_BLOCK_LEN).  You must enable this at compress time.
Doing seekChunkReset hurts compression ratio slightly, but makes it possible to seek in the packed data
and decompress just a portion without decompressing the whole thing.  It also
allows parallel decompression.  You may also want an $OodleLZ_SeekTable , which records
the locations of the seek points; it should be created from the compressed buffer at encode time
via $OodleLZ_CreateSeekTable.
This is needed if you want Oodle to parallelize decompression for you via $OodleXLZ_ReadAndDecompress_Wide_Async.

(see $Oodle_FAQ_WhichLZ for a guide to choosing a compressor)

(see $Oodle_FAQ_LZCompareTable for a table of performance for the different choices,
or run $example_lz_chart)

The normal $OodleLZ_Compress and $OodleLZ_Decompress calls require a full buffer to encode
or decode (eg. not streaming or incremental), though you can do individual seek chunks or
$OODLELZ_BLOCK_LEN at a time.  There is also
a streaming decoder, which can decode incrementally (eg. $OodleLZDecoder_Create).

The OodleLZ compressed data does not include any header.  You must store
the raw size and compressed size yourself.
Because OodleLZ data is headerless, it can be concated at seek chunk boundaries.
You can compress any multiple of OODLELZ_BLOCK_LEN bytes separately and simply
concatenate the compressed buffers together.  This is how you can compress a file
that's too big to fit in memory in one piece.  You can also use this to do your own
multi-threaded compression if you don't want to use Oodle's $OodleXLZ_Compress_Async.
This is demonstrated in $example_lz lz_test_9.

The OodleLZ compressors can create a CRC of each compressed chunk so that you can
verify its integrity before decompressing (you must set $OodleLZ_CompressOptions:sendQuantumCRCs).  This CRC is not checked in decode by default,
but can be enabled via the $OodleLZ_CheckCRC option.

Kraken (and other new compressors) can now do parallel decodes without seek points.
See $OodleLZ_About_ThreadPhasedDecode for more.

---------------------------

## How to choose a compressor and options

The best way to choose a compressor is just to try them.  An easy way is to run $example_lz_chart on your data file to get
a sample of how the various Oodle LZ options perform.

Different compression algorithms have different tradeoffs in terms of speed, memory use, and compression ratio.

The OodleLZ compressors are intended to be at the sweet spot for in-game decompression on current game platforms.
Their speed/ratio tradeoff is designed to make loading compressed data and then decompressing it faster than just
loading decompressed data (in most cases; the exact balance depends on the data and how compressible it is). 

Some notes on how to tweak the compressor to meet your goals :

First of all, $OodleLZ_CompressionLevel controls how much work the compressor does to
optimize the stream.  This affects compression time (NOTE : *encoding* time, not *decoding* time), but makes better streams.  The
better streams are both smaller *and* often decompress faster.  So if your goal is to have
the smallest stream or the fastest to decompress, in all cases you want to use the
highest level of $OodleLZ_CompressionLevel that you can tolerate (usually Optimal2 is as high as you need to go).

Note the $OodleLZ_CompressionLevel you pass to $OodleLZ_CompressOptions_GetDefault does
not have to match the one you use for running the compression.

In general, always start with $OodleLZ_Compressor_Kraken as your first choice.

1. If you want maximum compression :

Use $OodleLZ_Compressor_Leviathan .  Don't compress small buffers independently; instead
append them together and compress them as one chunk.
Do not set $(OodleLZ_CompressOptions:seekChunkReset) (make it false).

Dial $(OodleLZ_CompressOptions:spaceSpeedTradeoffBytes) down.  Perhaps try 32.  This trades off decode
speed for smaller compressed files.

2. If you want maximum decompress speed :

Use $OodleLZ_Compressor_Selkie.

For threaded decodes, set $(OodleLZ_CompressOptions:seekChunkReset) to true.  This
ensures that decompression can proceed in parallel.  

Make and transmit a $OodleLZ_SeekTable table so that the parallel decoder can find its start points without scanning the data.
You can do this manually via $OodleLZ_CreateSeekTable , or you can use an OOZ or OOP file to do this for you.

There are options in OodleLZ_CompressOptions which can adjust decode speed, such as minMatchLen, dictionarySize,
and spaceSpeedTradeOffBytes.  Contact Oodle support for advice on these.

3. About manually setting up $OodleLZ_CompressOptions :

Normally you should just use $OodleLZ_CompressOptions_GetDefault , but if you want the last bit of speed or compression ratio,
you can sometimes find some more win by playing with individuals options.  (this is particularly true with unusual data that
does not fit well with the default heuristics).

$* _seekChunkReset_ : makes chunks independent; hurts compression but allows parallel decompression, as well as seeking.

$* _seekChunkLen_ : sets the length of seek chunks.  Generally you want this to be as large as possible,
while providing enough chunks to utilitize all worker threads.  A good way to set it is to call $OodleLZ_MakeSeekChunkLen
with a seek point count of 8 or so.

$* _spaceSpeedTradeoffBytes_ : controls the compressors decision about whether it should favor speed of decompression or
small size.  It has units of bytes per time; roughly it's the number of bytes that must be saved in compression to
make a choice that hurts decode time by some fixed unit of time.  The default is 256.  For maximum speed of decompress you can set _spaceSpeedTradeoffBytes_
to a larger number (512).  For maximum compression set _spaceSpeedTradeoffBytes_ to a small number (32).  In normal use
the values provided by $OodleLZ_CompressOptions_GetDefault are appropriate.

$* _minMatchLen_ : can be used to raise the minimum match length of a compressor (you can't make it lower than
the compressor's default).  Increasing to 6 or 8 (for example) can sometimes be good for compression or decode speed
on some types of data, such as image prediction residuals.

$* _dictionarySize_ : can be used to limit the maximum offset, which may be useful for decode speed on devices with
small caches or slow RAM, and can be used to prepare data for sliding window decoding.

----------

If you specify $OodleLZ_FuzzSafe_Yes then the output buffer will never be exceeded, even on corrupt data.
All the new compressors support Fuzz safe decoding, and it doesn't cost any decode speed.  I recommend
always using $OodleLZ_FuzzSafe_Yes

----------

See also $example_lz , $example_lz_simple, $example_lz_overlap, $example_lz_chart

*/


//idoc(page,OodleLZ_About_ThreadPhasedDecode,About OodleLZ ThreadPhased Decode)
//idoc(autolink,on)
//idoc(markdown,on)
/*

## About OodleLZ ThreadPhased Decode

The new Kraken compressor can be decoded with a new type of parallelism called "ThreadPhased".
(check $OodleLZ_Compressor_CanDecodeThreadPhased)

ThreadPhased decoding works by running the decode operation per block in two phases.  This allows 1-2X
speedup using two threads for decoding, typically around 33%-50% speedup.  (Mermaid can decode ThreadPhased
too, but the benefit isn't as large as it is with Kraken)

ThreadPhased decoding can be done on the normal compressed data made with OodleLZ_Compress.  You
don't need to prepare the data specially for ThreadPhased decoding, or break it into chunks with
the Oodle seek chunk reset system.  (you can combine seek chunks and ThreadPhased decoding for
even more parallelism if you like).

Make compressed data by calling $OodleLZ_Compress just like you normally would.

ThreadPhased decoding requires more memory than normal single-threaded decoding, because it needs
staging space for the two threads to communicate.


The easiest way to try ThreadPhased decoding is to use the helper in OodleX $OodleXLZ_Decompress_ThreadPhased_Narrow_Async .
That runs a 2-thread decode on the OodleX Worker system, freeing the calling thread for other work.

$example_lz_threadphased includes a demonstration of that call.



The basic idea of ThreadPhased decoding is that the OodleLZ_Decompress work on each BLOCK can be
split into two phases.  This can be invoked by just calling OodleLZ_Decompress twice on the same
block, first with OodleLZ_Decode_ThreadPhase1, then with OodleLZ_Decode_ThreadPhase2.

To get parallelism, we can run the two phases on two separate threads.

The rule is that you must run the Phase2 on each block after the Phase1 for that block is done,
and with the same "decoderMem" pointer.  The Phase2 decodes on all blocks must be done in
sequential order (unless they are Seek Resets).  The Phase1 decodes can be done in any order.
The decoder memory used for OodleLZ_Decompress here must be larger than normal, of size
OodleLZ_ThreadPhased_BlockDecoderMemorySizeNeeded().


To perform a portion of a ThreadPhased decode, you simply call $OodleLZ_Decompress , but with some
special argument values :

OodleLZ_Decompress(
	compBuf,compBufferSize,	// should be the compressed data for one block only
	rawBuf,rawLen,  // should be the destination uncompressed pointers for this block
	checkCRc,verbosity,
	decBufBase,decBufSize, // should be the whole destination buffer (or one whole seek chunk)
	fpCallback,callbackUserData,
	decoderMemory,decoderMemorySize, // must be provided, of size OodleLZ_ThreadPhased_BlockDecoderMemorySizeNeeded()
	threadPhase // set to OodleLZ_Decode_ThreadPhase1 or OodleLZ_Decode_ThreadPhase2
	);

The decoderMemory must be allocated by the caller, and must be the same for each Phase on the block.
That is, don't use the decoderMemory for the next block in the same phase, instead use it for the
same block in the next phase.  The decoderMemory is the place where the work of phase1 is passed
to phase2.


The two threads run through the blocks sequentially, passing the results of phase1 to the input of
phase2.  Each phased block must get its own decoderMemory.

\<PRE>

[phase1 block1] [phase1 block2] ...							
              |               |
              |               |
              [phase2 block1] [phase2 block2] ...

\</PRE>


See $example_lz_threadphased for a full client-side implementation


*/


//idoc(page,OodleLZ_About_Hydra,About OodleLZ Hydra)
//idoc(autolink,on)
//idoc(markdown,on)
/*

## About OodleLZ Hydra

Oodle Hydra is a meta-compressor which automatically selects Leviathan, Kraken, Mermaid, or Selkie on a
per-block basis.

When you decode a file that was compressed with Hydra, it will decode as one or several of those other
compressors.

Hydra makes its decision by scoring each compressor for its space-speed performance.  What this means is
Hydra automatically makes good decisions about using the slower compressors only when they are worth it.
That is, they must provide a good return in terms of bytes saved in exchange for the increase in decode
time.  Exactly what qualifies as "worth it" is determined by you, via the $(OodleLZ_CompressOptions:spaceSpeedTradeoffBytes)
parameter.

With spaceSpeedTradeoffBytes around 256 , Hydra is roughly comparable to Kraken.  As you dial it lower,
Hydra will give more compression but slower decodes.  Around 1500 is comparable to Mermaid, and around 50
is comparable to Leviathan.  In between you can hit a balance that's somewhere between those compressors.

Hydra takes more time to encode because it is considering many compressors.

Hydra always beats every other Oodle compressor, it provides the best of them.


*/


//idoc(page,OodleAPI_LZ)
/*

	Oodle2 Core LZ lossless data compression.
	
	$^TableOfContents
	
*/

//idoc(parent,OodleAPI_LZ)
//idoc(page,OodleAPI_LZ_Compressors)
/*

	OodleLZ low level synchronous memory to memory lossless data compression.
	
	$^TableOfContents
	
*/

