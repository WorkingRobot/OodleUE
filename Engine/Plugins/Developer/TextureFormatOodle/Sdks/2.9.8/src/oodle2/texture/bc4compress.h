// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlebase.h"
#include "rrdxtcenums.h"

OODLE_NS_START

enum BC4SourceFormat
{
	BC4SourceFormat_Invalid,

	// BC4/5
	BC4SourceFormat_U8,
	BC4SourceFormat_S8,
	BC4SourceFormat_U16,
	BC4SourceFormat_S16,

	// BC3 alpha
	BC4SourceFormat_RGBA_U8, // note: works for both RGBA and BGRA byte orders since we only care about the A
};

enum BC4ValueType
{
	BC4ValueType_UNorm,
	BC4ValueType_SNorm,
	BC4ValueType_Alpha,	// BC3 alpha block
};

struct BC4SourceData
{
	S16 values[16];		// [0,0x1000] for unsigned data, [-0x1000,0x1000] for signed, [0,0xff] for alpha
	U8 type;			// BC4ValueType_*
};

// Reads a 4x4 block of contiguous pixels from a block surface
void BC4_ReadSourceFromBlock(BC4SourceData * pBlock, const void * src, BC4SourceFormat format);

// Reads and deinterleaves a 4x4 block of RG data for BC5 processing
void BC5_ReadSourceFromBlock(BC4SourceData * pBlockR, BC4SourceData * pBlockG, const void * src, BC4SourceFormat format);

// These funcs return the internal block error
U32 BC4_Compress_1(U8 * dest, const BC4SourceData & values, rrDXTCOptions options);
U32 BC4_Compress_2(U8 * dest, const BC4SourceData & values, rrDXTCOptions options);
U32 BC4_Compress_3(U8 * dest, const BC4SourceData & values, rrDXTCOptions options);
U32 BC4_Compress_BruteForce(U8 * dest, const BC4SourceData & values, rrDXTCOptions options);

void BC4_Compress(U8 * dest, const BC4SourceData & values, rrDXTCLevel dxtcQualityLevel, rrDXTCOptions options);

// Returns error
U32 BC4_FindIndicesAndEncode(U8 * dest, const BC4SourceData & values, U8 ep0, U8 ep1);

// Decoding function to 8-bit alpha-only format (BC3 alpha)
// overwrites A channel in result (either BGRA or RGBA)
void BC4A_Decompress(U8 result[64], const U8 * encoded);

// Decoding functions to 16-bit SNORM/UNORM
void BC4S_Decompress16(S16 result[16], const U8 * encoded);
void BC4U_Decompress16(U16 result[16], const U8 * encoded);

// Describes how the endpoints are encoded
struct BC4EndpointEncoding
{
	S32 value_mask;		// Value bits in the value type
	S32 sign_mask;		// sign bit in the value type; value is (u8 & value_mask) - (u8 & sign_mask)

	S32 min_q;			// smallest quantized endpoint representable
	S32 max_q;			// largest quantized endpoint representable
	S32 min_deq;		// smallest dequantized endpoint representable
	S32 max_deq;     	// largest dequantized endpoint representable

	S16 dequant[256];	// dequantize raw U8 bytes to their interpreted meaning

	// decodes an 8-bit endpoint value to its signed (but still quantized) interpretation
	int decode_endpoint_q(int endpt) const
	{
		return (endpt & value_mask) - (endpt & sign_mask);
	}
};

// Gives you a description of the endpoint encoding used for the given value_type
void BC4_DescribeEncoding(BC4ValueType value_type, BC4EndpointEncoding * out_encoding);

OODLE_NS_END

