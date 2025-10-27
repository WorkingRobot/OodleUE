// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlecore.h"

OODLE_NS_START

struct OodleKrakenChunkDeadlines
{
	F32 whole_chunk;
	F32 literals_only;

	F32 literal;
	F32 packet;
	F32 offsets;
	F32 excesses_u8;
	F32 parse;
};

// This is the API implemented by the speed fits
struct OodleSpeedFit
{
	// Flags
	bool uses_huff_alphabet_runs;

	// Cost estimates for primitives
	F32 (*memcpy)(SINTa len);
	F32 (*simd_mul_s32_sub_u8)(SINTa len);
	F32 (*simd_interleave_8x2)(SINTa len);

	// High-level bitstream elements
	F32 (*get_offsets44)(SINTa offsets_count);
	F32 (*get_offsetsalt)(SINTa offsets_count);
	F32 (*get_excesses)(SINTa excesses_count, SINTa excesses_u32_count);
	F32 (*get_multiarrays)(SINTa num_intervals, SINTa tot_raw_len);
	F32 (*newlzf_unpack_escape_offsets)(SINTa offsets_count);

	// Array types
	F32 (*huff3)(SINTa len, SINTa num_non_zero, SINTa num_alphabet_runs, F32 bpb);
	F32 (*huff6)(SINTa len, SINTa num_non_zero, SINTa num_alphabet_runs, F32 bpb);
	F32 (*tans)(SINTa len, SINTa num_non_zero, SINTa L);
	F32 (*rle)(SINTa len);

	// Packet parse
	F32 (*parse_Selkie)(SINTa chunk_len, SINTa num_packets, SINTa num_escapes, SINTa num_literals);
	F32 (*parse_Mermaid)(SINTa chunk_len, SINTa num_packets, SINTa num_escapes);
	F32 (*parse_Mermaid_subliterals)(SINTa num_literals);
	F32 (*parse_Kraken)(SINTa chunk_len, SINTa num_packets, SINTa num_excesses);
	F32 (*parse_Kraken_subliterals)(SINTa num_literals);
	F32 (*parse_Leviathan)(SINTa chunk_len, SINTa num_packets, SINTa num_excesses);
	F32 (*parse_Leviathan_literals)(SINTa num_literals, int literal_mode);
	F32 (*parse_Leviathan_packetpos)(SINTa num_packets);

	// Deadline calculations
	void (*deadlines_Kraken)(OodleKrakenChunkDeadlines * deadlines, SINTa chunk_len);
	F32 (*recompute_Kraken_J)(
		SINTa total_comp, F32 lambda,
		SINTa literal_comp_len, F32 literals_J,
		SINTa packet_comp_len, F32 packet_J,
		SINTa offsets_comp_len, F32 offsets_J,
		SINTa excesses_u8_comp_len, F32 excesses_u8_J,
		SINTa parse_complen, F32 parse_time
	);
};

const OodleSpeedFit * speedfit_get_default();

//==================================================================

// this is the time in the newlz_offsets varbits get ; not including the huff arrays time
static RADINLINE F32 speedfit_get_offsets_time(const OodleSpeedFit * speedfit, SINTa count, int modulo )
{
	if ( modulo == 0 )
	{
		// offsets 44
		return speedfit->get_offsets44(count);
	}
	else if ( modulo == 1 )
	{
		// alt offset, no multiply-merge
		return speedfit->get_offsetsalt(count);
	}
	else
	{
		// alt offset general modulo
		return speedfit->get_offsetsalt(count) + speedfit->simd_mul_s32_sub_u8(count);
	}
}

#ifdef NEWLZ_ARRAY_TYPE_HUFF
static RADINLINE F32 speedfit_huff_time(const OodleSpeedFit * speedfit,int huff_type,SINTa len,SINTa num_non_zero,SINTa num_alphabet_runs,F32 bpb)
{
	RR_ASSERT( huff_type == NEWLZ_ARRAY_TYPE_HUFF || huff_type == NEWLZ_ARRAY_TYPE_HUFF6 );
	if ( huff_type == NEWLZ_ARRAY_TYPE_HUFF )
		return speedfit->huff3(len,num_non_zero,num_alphabet_runs,bpb);
	else
		return speedfit->huff6(len,num_non_zero,num_alphabet_runs,bpb);
}
#endif

//==================================================================

static RADINLINE F32 speedfit_estimate_entropy_array_time(const OodleSpeedFit * speedfit,SINTa len)
{
	// used in the "estimate" decision makers before a mode is chosen
	// just pretend all arrays are huff3 ?
	return speedfit->huff3(len,128,20,6.0f);
}

static RADINLINE F32 speedfit_memcpy_array_J(F32 lambda,const OodleSpeedFit * speedfit, SINTa len)
{
	return len + lambda * speedfit->memcpy(len);
}

OODLE_NS_END
