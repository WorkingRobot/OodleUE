// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(parent,OodleAPI_TextureRTBase)
#pragma once
#include "texrtbase.h"

OODLE_NS_START

// OodleTexRT_Err is in texrtbase.h!

PUBTYPESTART

//idoc(parent,OodleAPI_TextureRTBC7Prep)

#ifndef OODLETEXRT_BC7PREP_DEFINED
#define OODLETEXRT_BC7PREP_DEFINED

#define OODLETEXRT_BC7PREP_MODE_COUNT 10 IDOC
/* Number of distinct mode streams in BC7Prep data.

   8 regular BC7 modes, one raw passthrough, one special.
*/

IDOC typedef OOSTRUCT OodleTexRT_BC7PrepHeader
{
	OO_U32 version;
	OO_U32 flags;
	OO_U32 mode_counts[OODLETEXRT_BC7PREP_MODE_COUNT];
} OodleTexRT_BC7PrepHeader;
/* Contains metadata required by the decoder to read a BC7Prep data block.

	Header information is always needed and interpreted on the CPU, even
	when actual BC7Prep decoding takes place in a compute shader on the GPU.
	With compute shader decoding, the payload data needs to be in suitably-aligned
	GPU-accessible memory, whereas the header information should be in
	CPU-accessible, cached memory.

	Because of these different requirements, we explicitly store the header
	data elsewhere. We recommend storing it alongside other texture metadata
	in your files.
*/

#endif // OODLETEXRT_BC7PREP_DEFINED

IDOC typedef enum OodleTexRT_BC7PrepDecodeFlags
{
	OodleTexRT_BC7PrepDecodeFlags_None = 0, // No special processing.
	OodleTexRT_BC7PrepDecodeFlags_DestinationIsCached = 1, // Destination is in write-back cached memory.
	OodleTexRT_BC7PrepDecodeFlags_AvoidWideVectors = 2, // Avoid using wide (>=256b) vector instructions.

	OodleTexRT_BC7PrepDecodeFlags_Force32 = 0x40000000
} OodleTexRT_BC7PrepDecodeFlags;
/* OodleTexRT_BC7PrepDecodeFlags controls BC7Prep decoding.

    Several of these flag values may be ORed together.

	`OodleTexRT_BC7PrepDecodeFlags_DestinationIsCached` may be used when the destination address
	is in cached memory, i.e. not uncached or write combined. This enables slightly faster
	decoding, up to about 10%. However, setting this flag when the destination is actually in
	write-combined memory will frequently result in a 2x slowdown instead, so tread carefully.

	`OodleTexRT_BC7PrepDecodeFlags_AvoidWideVectors` disables usage of wide (>=256-bit) vector
	instructions even on CPUs that support it. On certain CPUs (mainly Intel CPUs derived
	from the Broadwell or Skylake microarchitectures) any use of a large class of 256-bit vector
	instructions will reduce the maximum clock rate by up to 20%; furthermore, for most non-server
	SKUs, this reduction affects all cores in the system, even when only a single core uses wide
	vector instructions. BC7Prep decoding generally only runs in short bursts; you can pass this
	flag to avoid use of 256-bit vectors in the decoder if nothing else in your app uses them, to
	avoid penalizing all other threads in the system.
*/

#ifdef __cplusplus
// Operator overload to enable ORing flags together in a type-safe manner in C++.
static inline OodleTexRT_BC7PrepDecodeFlags operator |(const OodleTexRT_BC7PrepDecodeFlags a, const OodleTexRT_BC7PrepDecodeFlags b)
{
	return static_cast<OodleTexRT_BC7PrepDecodeFlags>(static_cast<OO_U32>(a) | static_cast<OO_U32>(b));
}

static inline OodleTexRT_BC7PrepDecodeFlags& operator |=(OodleTexRT_BC7PrepDecodeFlags & a, const OodleTexRT_BC7PrepDecodeFlags b)
{
	a = static_cast<OodleTexRT_BC7PrepDecodeFlags>(static_cast<OO_U32>(a) | static_cast<OO_U32>(b));
	return a;
}
#endif // __cplusplus

PUBTYPEEND

PUBSTART

//idoc(parent,OodleAPI_TextureRTBase)

IDOC OOFUNC1 OodleTexRT_Err OOFUNC2 OodleTexRT_LogVersion();
/* Log the version and build of the Oodle Texture Runtime library.

	Logs with the plugged in logging callback.  See $OodleTexRT_Plugins_SetPrintf.

	Will return an error if internal initialization failed.
*/

IDOC OOFUNC1 const char * OOFUNC2 OodleTexRT_Err_GetName(OodleTexRT_Err error);
/* Maps $OodleTexRT_Err enum values to a corresponding string.

	Intended for when you want to turn an error code into an error message.
*/

//idoc(parent,OodleAPI_TextureRTBC7Prep)

IDOC OOFUNC1 OodleTexRT_Err OOFUNC2 OodleTexRT_BC7Prep_ReadHeader(const OodleTexRT_BC7PrepHeader * header,
	OO_SINTa * out_num_blocks, OO_SINTa * out_payload_size);
/* Reads and validates the header and determines the size of the decoded and coded BC7 blocks.

	$:header			Header for a BC7Prep data chunk.
	$:out_num_blocks	If non-null, receives number of coded BC7 blocks in this blob.
	$:out_payload_size	If non-null, receives size of corresponding BC7Prep chunk in bytes.
	$:return			OodleTexRT_OK on success, an error code if the header is malformed.

	These numbers are useful for memory management. The block count determines how large the
	data will be after decoding (and also how much scratch memory is required as per
	$OodleTexRT_BC7Prep_MinDecodeScratchSize) and the payload size determines the size of the
	BC7Prep data itself, in case you're not storing it anywhere else.

	This function is provided for convenience. If you use a fixed limit on the number of blocks in
	a single BC7Prep chunk and store their sizes elsewhere, you can just allocate all memory buffers
	once up front and don't need to call this function at all.
*/

IDOC OOFUNC1 OO_SINTa OOFUNC2 OodleTexRT_BC7Prep_MinDecodeScratchSize(OO_SINTa nblocks);
/* Returns how much decoder scratch memory (in bytes) is required for the given number of BC7 blocks.

	BC7Prep decoding temporarily needs some memory during decoding, the scratch working buffer.
	Its required size depends on the number of blocks encoded. On error (negative result), returns
	an $OodleTexRT_Err.

	You can either decide on a global maximum _nblocks_ and pre-allocate BC7Prep decoder scratch
	memory once for that many blocks, or manage everything more dynamically and use
	$OodleTexRT_BC7Prep_ReadHeader to figure out how many blocks a given BC7Prep chunk decodes to.

	If you need to decode multiple BC7Prep blocks in sequence, it is recommended to allocate
	the scratch buffer once (for the maximum _nblocks_ required) and reuse it between calls. Note
	that if you're concurrently decoding BC7Prep blocks on multiple threads, each thread neads
	its own scratch buffer.
*/

IDOC OOFUNC1 OO_SINTa OOFUNC2 OodleTexRT_BC7Prep_Decode(void * output_buf, OO_SINTa output_size,
	const void * bc7prep_data, OO_SINTa bc7prep_data_size,
	const OodleTexRT_BC7PrepHeader * header, OodleTexRT_BC7PrepDecodeFlags flags,
	void * scratch_buf, OO_SINTa scratch_size);
/* Decodes a BC7Prep data stream.

   $:output_buf			Where the decoded BC7 blocks get written to.
   $:output_size		Size of _output_buf_ in bytes.
   $:bc7prep_data		The encoded BC7Prep data chunk.
   $:bc7prep_data_size	Size of _bc7prep_data_ in bytes.
   $:header				The header for the data blob.
   $:flags				Decode flags. See notes below. Pass $OodleTexRT_BC7PrepDecodeFlags_None by default.
   $:scratch_buf		Pointer to a scratch working buffer.
   $:scratch_size		Size of _scratch_buf_ in bytes.
   $:return				Number of bytes written to _output_buf_ on success, a negative number ($OodleTexRT_Err) on error.

	Decodes the given BC7Prep data chunk to _output_buf_; _output_size_ must be at least 16 times the number of
	BC7 blocks encoded in the given BC7Prep data chunk.

	Returns number of bytes written; on success this equals the BC7 block size (16 bytes) times the number
	of blocks in the given BC7Prep chunk.

	You can determine the number of blocks and _bc7prep_data_size_ from the header using
	$OodleTexRT_BC7Prep_ReadHeader.

	If _output_buf_ is known to be in cached memory (i.e. not write-combined memory like GPU memory
	on consoles or sometimes GPU upload heaps on PC), you may set _flags_ to
	$OodleTexRT_BC7PrepDecodeFlags_DestinationIsCached. This is up to 20% faster when the destination is
	in cached memory, but sometimes takes 2x as long when it's not. When in doubt, leave it off.

    The scratch buffer is used for all working memory during the decode process. BC7Prep decoding
    does not perform any memory allocations. You can reuse the scratch memory between subsequent
    BC7Prep_Decode calls (as long as it's large enough), but two concurrent BC7Prep_Decodes running
    on different threads need to have their private scratch buffers.

	BC7Prep _encoding_ is performed by $OodleTex_BC7Prep_Encode and lives in the main Oodle Texture
	library, not the runtime.
*/

PUBEND

OODLE_NS_END

