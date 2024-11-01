// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(parent,OodleAPI_Texture)
//idoc(end)
#include "texrtbase.h"
#include "oodletexrtpub.h"
#include "oodletexturertversion.h"

#include "bc7prep_decode.h"

OODLE_NS_START

OOFUNC1 OodleTexRT_Err OOFUNC2 OodleTexRT_LogVersion()
{
	OodleTexRT_Err err = OodleTexRT_Enter();
	
	rrprintf("Oodle Texture Runtime %s %s\n",OodleTextureRTVersion,RADCOPYRIGHT);
	rrprintf("Oodle Texture Runtime %s built %s %s\n",OodleTextureRTVersion,__DATE__,__TIME__);
	
	return err;
}

OOFUNC1 const char * OOFUNC2 OodleTexRT_Err_GetName(OodleTexRT_Err error)
{
	// NOTE: intentionally no OodleTex_Enter() and early-out here
	// this is the message you call on failure of OodleTex_Enter to
	// find out what's wrong!

	switch (error)
	{
	#define ERR(x) case x: return #x;
		ERR(OodleTexRT_Err_OK)
		ERR(OodleTexRT_Err_Internal)

		ERR(OodleTexRT_Err_BC7PrepHeaderCorrupt)
		ERR(OodleTexRT_Err_BC7PrepOutputBufTooSmall)
		ERR(OodleTexRT_Err_BC7PrepScratchBufTooSmall)
		ERR(OodleTexRT_Err_BC7PrepPayloadCorrupt)
		ERR(OodleTexRT_Err_BC7PrepInputTooSmall)
		ERR(OodleTexRT_Err_BC7PrepTooManyBlocks)

		ERR(OodleTexRT_Err_GPU_OutOfShaderMem)
		ERR(OodleTexRT_Err_GPU_BadMemoryType)
	#undef ERR
	default:
		break;
	}

	return "unknown error code!";
}

OOFUNC1 OodleTexRT_Err OOFUNC2 OodleTexRT_BC7Prep_ReadHeader(const OodleTexRT_BC7PrepHeader * header, SINTa * out_num_blocks, SINTa * out_payload_size)
{
	OodleTexRT_Err err = OodleTexRT_Enter();
	if ( err != OodleTexRT_Err_OK )
		return err;

	err = bc7prep_read_header(header, out_num_blocks, out_payload_size);
	return err;
}

OOFUNC1 SINTa OOFUNC2 OodleTexRT_BC7Prep_MinDecodeScratchSize(SINTa nblocks)
{
	OodleTexRT_Err err = OodleTexRT_Enter();
	if ( err != OodleTexRT_Err_OK )
		return err;

	return bc7prep_min_decode_work_mem_size(nblocks);
}

OOFUNC1 SINTa OOFUNC2 OodleTexRT_BC7Prep_Decode(void * output_buf, SINTa output_size, const void * bc7prep_data, SINTa bc7prep_data_size,
	const OodleTexRT_BC7PrepHeader * header, OodleTexRT_BC7PrepDecodeFlags flags, void * scratch_buf, SINTa scratch_size)
{
	RR_UNUSED_VARIABLE(flags);

	OodleTexRT_Err err = OodleTexRT_Enter();
	if ( err != OodleTexRT_Err_OK )
		return err;

	U32 decode_flags = 0;
	if ( flags & OodleTexRT_BC7PrepDecodeFlags_DestinationIsCached )
		decode_flags |= BC7PREP_DECODE_DESTINATION_IS_CACHED;

	if ( flags & OodleTexRT_BC7PrepDecodeFlags_AvoidWideVectors )
		decode_flags |= BC7PREP_DECODE_AVOID_WIDE_VECTORS;

	SINTa result = bc7prep_decode((U8 *)output_buf, output_size, (const U8 *)bc7prep_data, bc7prep_data_size, header, (U8 *)scratch_buf, scratch_size, decode_flags);
	return result;
}

OODLE_NS_END

// OodleAPI_TextureRT is in index.idc

//idoc(begin)
//idoc(page,OodleAPI_TextureRT)
//idoc(autolink,on)
//idoc(markdown,on)
/*

Oodle Texture RT (Runtime) is the runtime component for Oodle Texture.  It contains helpers
to decode the lossless transforms that were used to make texture data more compressible.

Oodle Texture RT can be built into your game and redistributed.

Oodle Texture RT currently contains the BC7Prep decoder.  Both CPU and GPU compute shader decoders
are available.

*/
//idoc(parent,OodleAPI_TextureRT)
//idoc(page,OodleAPI_TextureRTBase,Texture Runtime base)
/*

	Oodle Texture Runtime base layer.

*/

//idoc(parent,OodleAPI_TextureRT)
//idoc(page,OodleAPI_TextureRTBC7Prep,Texture Runtime BC7Prep)
/*

    BC7Prep losslessly rewrites BC7 textures into a different format that needs to be
	turned back into regular BC7 before it can be used on the GPU. These functions
	implement BC7Prep to BC7 decoding.

*/

//idoc(parent,OodleAPI_TextureRT)
//idoc(page,OodleAPI_OodleTexRT_Plugins,Texture Runtime plugins)
