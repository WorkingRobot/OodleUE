// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "bc67format.h"

OODLE_NS_START

// Reference: Khronos data format specification 1.1
//	 https://www.khronos.org/registry/DataFormat/specs/1.1/dataformat.1.1.html#BPTC
// (KDFS for short)

// LSB-first bitpacking, LE byte order.
struct BC67BitBuf
{
	U64 bitbuf; // contains at least 32 valid bits, LSB-aligned.
	U64 refillbuf; // reservoir from which we refill
	int next_refill; // bits until next refill (==num_bits_in_buf - 32)

	void init(const void *data)
	{
		const U8 *bytes = (const U8 *)data;
		bitbuf = refillbuf = 0;
		for (size_t i = 0; i < 8; ++i)
		{
			bitbuf |= (U64)bytes[i + 0] << (i*8);
			refillbuf |= (U64)bytes[i + 8] << (i*8);
		}
		next_refill = 32;
	}

	U32 get(U32 count)
	{
		// grab the bits
		RR_ASSERT(count < 32);
		U32 val = (U32)(bitbuf & ((1u << count) - 1));

		// consume them
		bitbuf >>= count;
		next_refill -= count;
		if (next_refill < 0)
		{
			// shuttle another 32 bits over
			next_refill += 32;
			bitbuf |= refillbuf << next_refill;
			refillbuf >>= 32;
		}

		return val;
	}

	U32 get32()
	{
		U32 val = (U32)bitbuf;

		// we consumed exactly 32 bits, so refill
		bitbuf = (bitbuf >> 32) | (refillbuf << next_refill);
		refillbuf >>= 32;

		return val;
	}

	bool is_finished() const
	{
		return next_refill == 0 && bitbuf == 0 && refillbuf == 0;
	}
};

// BC7 modes (KDFS table 32)
const BC7ModeDesc bc7_modes[8] =
{
	// #,NS,PB,RB,ISB,CB,AB,EPB,SPB,IB,IB2
	{  0, 3, 4, 0,	0, 4, 0,  1,  0, 3,  0 },
	{  1, 2, 6, 0,	0, 6, 0,  0,  1, 3,  0 },
	{  2, 3, 6, 0,	0, 5, 0,  0,  0, 2,  0 },
	{  3, 2, 6, 0,	0, 7, 0,  1,  0, 2,  0 },
	{  4, 1, 0, 2,	1, 5, 6,  0,  0, 2,  3 },
	{  5, 1, 0, 2,	0, 7, 8,  0,  0, 2,  2 },
	{  6, 1, 0, 0,	0, 7, 7,  1,  0, 4,  0 },
	{  7, 2, 6, 0,	0, 5, 5,  1,  0, 2,  0 },
};

// Partitions
#define P4(a,b,c,d) ((a) + (b)*4 + (c)*16 + (d)*64)
#define P(a,b,c,d, e,f,g,h, i,j,k,l, m,n,o,p) (P4(a,b,c,d) + P4(e,f,g,h)*0x100 + P4(i,j,k,l)*0x10000 + P4(m,n,o,p)*0x1000000u)
// We want the table itself to give the anchor values in the same order as the spec for consistency so we can just copy
// over their tables. The spec lists the anchor indices by order of the subset they belong to (i.e. the first anchor is the
// one for subset 1, and the second is the one for subset 2; the anchor for subset 0 is always at position 0).
// We don't care which subset the anchors belong to since we explicitly re-insert the omitted bits in the index bit vector.
// We prefer them in ascending order of pixel index, to match the way we insert the extra bits.
// So use this macro to sort the pair of anchor indices into ascending order.
#define AV(a,b) ((a) < (b) ? (a) : (b)), ((a) < (b) ? (b) : (a))

// These values copy-pasted from a machine-readable version of the spec
const BC67Partition bc67_partitions[1 + 64 + 64] =
{
	// 1-subset
	{ P(0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0),  0,  0 }, // shape 0
	// 2-subset (KDFS tables 34 for partition inds, 36 for anchor index)
	{ P(0,0,1,1, 0,0,1,1, 0,0,1,1, 0,0,1,1), 15,  0 }, // shape 0
	{ P(0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1), 15,  0 }, // shape 1
	{ P(0,1,1,1, 0,1,1,1, 0,1,1,1, 0,1,1,1), 15,  0 }, // shape 2
	{ P(0,0,0,1, 0,0,1,1, 0,0,1,1, 0,1,1,1), 15,  0 }, // shape 3
	{ P(0,0,0,0, 0,0,0,1, 0,0,0,1, 0,0,1,1), 15,  0 }, // shape 4
	{ P(0,0,1,1, 0,1,1,1, 0,1,1,1, 1,1,1,1), 15,  0 }, // shape 5
	{ P(0,0,0,1, 0,0,1,1, 0,1,1,1, 1,1,1,1), 15,  0 }, // shape 6
	{ P(0,0,0,0, 0,0,0,1, 0,0,1,1, 0,1,1,1), 15,  0 }, // shape 7
	{ P(0,0,0,0, 0,0,0,0, 0,0,0,1, 0,0,1,1), 15,  0 }, // shape 8
	{ P(0,0,1,1, 0,1,1,1, 1,1,1,1, 1,1,1,1), 15,  0 }, // shape 9
	{ P(0,0,0,0, 0,0,0,1, 0,1,1,1, 1,1,1,1), 15,  0 }, // shape 10
	{ P(0,0,0,0, 0,0,0,0, 0,0,0,1, 0,1,1,1), 15,  0 }, // shape 11
	{ P(0,0,0,1, 0,1,1,1, 1,1,1,1, 1,1,1,1), 15,  0 }, // shape 12
	{ P(0,0,0,0, 0,0,0,0, 1,1,1,1, 1,1,1,1), 15,  0 }, // shape 13
	{ P(0,0,0,0, 1,1,1,1, 1,1,1,1, 1,1,1,1), 15,  0 }, // shape 14
	{ P(0,0,0,0, 0,0,0,0, 0,0,0,0, 1,1,1,1), 15,  0 }, // shape 15
	{ P(0,0,0,0, 1,0,0,0, 1,1,1,0, 1,1,1,1), 15,  0 }, // shape 16
	{ P(0,1,1,1, 0,0,0,1, 0,0,0,0, 0,0,0,0),  2,  0 }, // shape 17
	{ P(0,0,0,0, 0,0,0,0, 1,0,0,0, 1,1,1,0),  8,  0 }, // shape 18
	{ P(0,1,1,1, 0,0,1,1, 0,0,0,1, 0,0,0,0),  2,  0 }, // shape 19
	{ P(0,0,1,1, 0,0,0,1, 0,0,0,0, 0,0,0,0),  2,  0 }, // shape 20
	{ P(0,0,0,0, 1,0,0,0, 1,1,0,0, 1,1,1,0),  8,  0 }, // shape 21
	{ P(0,0,0,0, 0,0,0,0, 1,0,0,0, 1,1,0,0),  8,  0 }, // shape 22
	{ P(0,1,1,1, 0,0,1,1, 0,0,1,1, 0,0,0,1), 15,  0 }, // shape 23
	{ P(0,0,1,1, 0,0,0,1, 0,0,0,1, 0,0,0,0),  2,  0 }, // shape 24
	{ P(0,0,0,0, 1,0,0,0, 1,0,0,0, 1,1,0,0),  8,  0 }, // shape 25
	{ P(0,1,1,0, 0,1,1,0, 0,1,1,0, 0,1,1,0),  2,  0 }, // shape 26
	{ P(0,0,1,1, 0,1,1,0, 0,1,1,0, 1,1,0,0),  2,  0 }, // shape 27
	{ P(0,0,0,1, 0,1,1,1, 1,1,1,0, 1,0,0,0),  8,  0 }, // shape 28
	{ P(0,0,0,0, 1,1,1,1, 1,1,1,1, 0,0,0,0),  8,  0 }, // shape 29
	{ P(0,1,1,1, 0,0,0,1, 1,0,0,0, 1,1,1,0),  2,  0 }, // shape 30
	{ P(0,0,1,1, 1,0,0,1, 1,0,0,1, 1,1,0,0),  2,  0 }, // shape 31
	{ P(0,1,0,1, 0,1,0,1, 0,1,0,1, 0,1,0,1), 15,  0 }, // shape 32
	{ P(0,0,0,0, 1,1,1,1, 0,0,0,0, 1,1,1,1), 15,  0 }, // shape 33
	{ P(0,1,0,1, 1,0,1,0, 0,1,0,1, 1,0,1,0),  6,  0 }, // shape 34
	{ P(0,0,1,1, 0,0,1,1, 1,1,0,0, 1,1,0,0),  8,  0 }, // shape 35
	{ P(0,0,1,1, 1,1,0,0, 0,0,1,1, 1,1,0,0),  2,  0 }, // shape 36
	{ P(0,1,0,1, 0,1,0,1, 1,0,1,0, 1,0,1,0),  8,  0 }, // shape 37
	{ P(0,1,1,0, 1,0,0,1, 0,1,1,0, 1,0,0,1), 15,  0 }, // shape 38
	{ P(0,1,0,1, 1,0,1,0, 1,0,1,0, 0,1,0,1), 15,  0 }, // shape 39
	{ P(0,1,1,1, 0,0,1,1, 1,1,0,0, 1,1,1,0),  2,  0 }, // shape 40
	{ P(0,0,0,1, 0,0,1,1, 1,1,0,0, 1,0,0,0),  8,  0 }, // shape 41
	{ P(0,0,1,1, 0,0,1,0, 0,1,0,0, 1,1,0,0),  2,  0 }, // shape 42
	{ P(0,0,1,1, 1,0,1,1, 1,1,0,1, 1,1,0,0),  2,  0 }, // shape 43
	{ P(0,1,1,0, 1,0,0,1, 1,0,0,1, 0,1,1,0),  2,  0 }, // shape 44
	{ P(0,0,1,1, 1,1,0,0, 1,1,0,0, 0,0,1,1), 15,  0 }, // shape 45
	{ P(0,1,1,0, 0,1,1,0, 1,0,0,1, 1,0,0,1), 15,  0 }, // shape 46
	{ P(0,0,0,0, 0,1,1,0, 0,1,1,0, 0,0,0,0),  6,  0 }, // shape 47
	{ P(0,1,0,0, 1,1,1,0, 0,1,0,0, 0,0,0,0),  6,  0 }, // shape 48
	{ P(0,0,1,0, 0,1,1,1, 0,0,1,0, 0,0,0,0),  2,  0 }, // shape 49
	{ P(0,0,0,0, 0,0,1,0, 0,1,1,1, 0,0,1,0),  6,  0 }, // shape 50
	{ P(0,0,0,0, 0,1,0,0, 1,1,1,0, 0,1,0,0),  8,  0 }, // shape 51
	{ P(0,1,1,0, 1,1,0,0, 1,0,0,1, 0,0,1,1), 15,  0 }, // shape 52
	{ P(0,0,1,1, 0,1,1,0, 1,1,0,0, 1,0,0,1), 15,  0 }, // shape 53
	{ P(0,1,1,0, 0,0,1,1, 1,0,0,1, 1,1,0,0),  2,  0 }, // shape 54
	{ P(0,0,1,1, 1,0,0,1, 1,1,0,0, 0,1,1,0),  2,  0 }, // shape 55
	{ P(0,1,1,0, 1,1,0,0, 1,1,0,0, 1,0,0,1), 15,  0 }, // shape 56
	{ P(0,1,1,0, 0,0,1,1, 0,0,1,1, 1,0,0,1), 15,  0 }, // shape 57
	{ P(0,1,1,1, 1,1,1,0, 1,0,0,0, 0,0,0,1), 15,  0 }, // shape 58
	{ P(0,0,0,1, 1,0,0,0, 1,1,1,0, 0,1,1,1), 15,  0 }, // shape 59
	{ P(0,0,0,0, 1,1,1,1, 0,0,1,1, 0,0,1,1), 15,  0 }, // shape 60
	{ P(0,0,1,1, 0,0,1,1, 1,1,1,1, 0,0,0,0),  2,  0 }, // shape 61
	{ P(0,0,1,0, 0,0,1,0, 1,1,1,0, 1,1,1,0),  2,  0 }, // shape 62
	{ P(0,1,0,0, 0,1,0,0, 0,1,1,1, 0,1,1,1), 15,  0 }, // shape 63
	// 3-subset (KDFS tables 35 for partition inds, 37 and 38 for first and second anchor inds)
	{ P(0,0,1,1, 0,0,1,1, 0,2,2,1, 2,2,2,2), AV( 3,15) }, // shape 0
	{ P(0,0,0,1, 0,0,1,1, 2,2,1,1, 2,2,2,1), AV( 3, 8) }, // shape 1
	{ P(0,0,0,0, 2,0,0,1, 2,2,1,1, 2,2,1,1), AV(15, 8) }, // shape 2
	{ P(0,2,2,2, 0,0,2,2, 0,0,1,1, 0,1,1,1), AV(15, 3) }, // shape 3
	{ P(0,0,0,0, 0,0,0,0, 1,1,2,2, 1,1,2,2), AV( 8,15) }, // shape 4
	{ P(0,0,1,1, 0,0,1,1, 0,0,2,2, 0,0,2,2), AV( 3,15) }, // shape 5
	{ P(0,0,2,2, 0,0,2,2, 1,1,1,1, 1,1,1,1), AV(15, 3) }, // shape 6
	{ P(0,0,1,1, 0,0,1,1, 2,2,1,1, 2,2,1,1), AV(15, 8) }, // shape 7
	{ P(0,0,0,0, 0,0,0,0, 1,1,1,1, 2,2,2,2), AV( 8,15) }, // shape 8
	{ P(0,0,0,0, 1,1,1,1, 1,1,1,1, 2,2,2,2), AV( 8,15) }, // shape 9
	{ P(0,0,0,0, 1,1,1,1, 2,2,2,2, 2,2,2,2), AV( 6,15) }, // shape 10
	{ P(0,0,1,2, 0,0,1,2, 0,0,1,2, 0,0,1,2), AV( 6,15) }, // shape 11
	{ P(0,1,1,2, 0,1,1,2, 0,1,1,2, 0,1,1,2), AV( 6,15) }, // shape 12
	{ P(0,1,2,2, 0,1,2,2, 0,1,2,2, 0,1,2,2), AV( 5,15) }, // shape 13
	{ P(0,0,1,1, 0,1,1,2, 1,1,2,2, 1,2,2,2), AV( 3,15) }, // shape 14
	{ P(0,0,1,1, 2,0,0,1, 2,2,0,0, 2,2,2,0), AV( 3, 8) }, // shape 15
	{ P(0,0,0,1, 0,0,1,1, 0,1,1,2, 1,1,2,2), AV( 3,15) }, // shape 16
	{ P(0,1,1,1, 0,0,1,1, 2,0,0,1, 2,2,0,0), AV( 3, 8) }, // shape 17
	{ P(0,0,0,0, 1,1,2,2, 1,1,2,2, 1,1,2,2), AV( 8,15) }, // shape 18
	{ P(0,0,2,2, 0,0,2,2, 0,0,2,2, 1,1,1,1), AV(15, 3) }, // shape 19
	{ P(0,1,1,1, 0,1,1,1, 0,2,2,2, 0,2,2,2), AV( 3,15) }, // shape 20
	{ P(0,0,0,1, 0,0,0,1, 2,2,2,1, 2,2,2,1), AV( 3, 8) }, // shape 21
	{ P(0,0,0,0, 0,0,1,1, 0,1,2,2, 0,1,2,2), AV( 6,15) }, // shape 22
	{ P(0,0,0,0, 1,1,0,0, 2,2,1,0, 2,2,1,0), AV(10, 8) }, // shape 23
	{ P(0,1,2,2, 0,1,2,2, 0,0,1,1, 0,0,0,0), AV( 5, 3) }, // shape 24
	{ P(0,0,1,2, 0,0,1,2, 1,1,2,2, 2,2,2,2), AV( 8,15) }, // shape 25
	{ P(0,1,1,0, 1,2,2,1, 1,2,2,1, 0,1,1,0), AV( 8, 6) }, // shape 26
	{ P(0,0,0,0, 0,1,1,0, 1,2,2,1, 1,2,2,1), AV( 6,10) }, // shape 27
	{ P(0,0,2,2, 1,1,0,2, 1,1,0,2, 0,0,2,2), AV( 8,15) }, // shape 28
	{ P(0,1,1,0, 0,1,1,0, 2,0,0,2, 2,2,2,2), AV( 5,15) }, // shape 29
	{ P(0,0,1,1, 0,1,2,2, 0,1,2,2, 0,0,1,1), AV(15,10) }, // shape 30
	{ P(0,0,0,0, 2,0,0,0, 2,2,1,1, 2,2,2,1), AV(15, 8) }, // shape 31
	{ P(0,0,0,0, 0,0,0,2, 1,1,2,2, 1,2,2,2), AV( 8,15) }, // shape 32
	{ P(0,2,2,2, 0,0,2,2, 0,0,1,2, 0,0,1,1), AV(15, 3) }, // shape 33
	{ P(0,0,1,1, 0,0,1,2, 0,0,2,2, 0,2,2,2), AV( 3,15) }, // shape 34
	{ P(0,1,2,0, 0,1,2,0, 0,1,2,0, 0,1,2,0), AV( 5,10) }, // shape 35
	{ P(0,0,0,0, 1,1,1,1, 2,2,2,2, 0,0,0,0), AV( 6,10) }, // shape 36
	{ P(0,1,2,0, 1,2,0,1, 2,0,1,2, 0,1,2,0), AV(10, 8) }, // shape 37
	{ P(0,1,2,0, 2,0,1,2, 1,2,0,1, 0,1,2,0), AV( 8, 9) }, // shape 38
	{ P(0,0,1,1, 2,2,0,0, 1,1,2,2, 0,0,1,1), AV(15,10) }, // shape 39
	{ P(0,0,1,1, 1,1,2,2, 2,2,0,0, 0,0,1,1), AV(15, 6) }, // shape 40
	{ P(0,1,0,1, 0,1,0,1, 2,2,2,2, 2,2,2,2), AV( 3,15) }, // shape 41
	{ P(0,0,0,0, 0,0,0,0, 2,1,2,1, 2,1,2,1), AV(15, 8) }, // shape 42
	{ P(0,0,2,2, 1,1,2,2, 0,0,2,2, 1,1,2,2), AV( 5,15) }, // shape 43
	{ P(0,0,2,2, 0,0,1,1, 0,0,2,2, 0,0,1,1), AV(15, 3) }, // shape 44
	{ P(0,2,2,0, 1,2,2,1, 0,2,2,0, 1,2,2,1), AV(15, 6) }, // shape 45
	{ P(0,1,0,1, 2,2,2,2, 2,2,2,2, 0,1,0,1), AV(15, 6) }, // shape 46
	{ P(0,0,0,0, 2,1,2,1, 2,1,2,1, 2,1,2,1), AV(15, 8) }, // shape 47
	{ P(0,1,0,1, 0,1,0,1, 0,1,0,1, 2,2,2,2), AV( 3,15) }, // shape 48
	{ P(0,2,2,2, 0,1,1,1, 0,2,2,2, 0,1,1,1), AV(15, 3) }, // shape 49
	{ P(0,0,0,2, 1,1,1,2, 0,0,0,2, 1,1,1,2), AV( 5,15) }, // shape 50
	{ P(0,0,0,0, 2,1,1,2, 2,1,1,2, 2,1,1,2), AV( 5,15) }, // shape 51
	{ P(0,2,2,2, 0,1,1,1, 0,1,1,1, 0,2,2,2), AV( 5,15) }, // shape 52
	{ P(0,0,0,2, 1,1,1,2, 1,1,1,2, 0,0,0,2), AV( 8,15) }, // shape 53
	{ P(0,1,1,0, 0,1,1,0, 0,1,1,0, 2,2,2,2), AV( 5,15) }, // shape 54
	{ P(0,0,0,0, 0,0,0,0, 2,1,1,2, 2,1,1,2), AV(10,15) }, // shape 55
	{ P(0,1,1,0, 0,1,1,0, 2,2,2,2, 2,2,2,2), AV( 5,15) }, // shape 56
	{ P(0,0,2,2, 0,0,1,1, 0,0,1,1, 0,0,2,2), AV(10,15) }, // shape 57
	{ P(0,0,2,2, 1,1,2,2, 1,1,2,2, 0,0,2,2), AV( 8,15) }, // shape 58
	{ P(0,0,0,0, 0,0,0,0, 0,0,0,0, 2,1,1,2), AV(13,15) }, // shape 59
	{ P(0,0,0,2, 0,0,0,1, 0,0,0,2, 0,0,0,1), AV(15, 3) }, // shape 60
	{ P(0,2,2,2, 1,2,2,2, 0,2,2,2, 1,2,2,2), AV(12,15) }, // shape 61
	{ P(0,1,0,1, 2,2,2,2, 2,2,2,2, 2,2,2,2), AV( 3,15) }, // shape 62
	{ P(0,1,1,1, 2,0,1,1, 2,2,0,1, 2,2,2,0), AV( 3, 8) }  // shape 63
};
#undef P4
#undef P
#undef AV

// insert a zero bit at position pos, shifting up all higher bits by 1
static U64 bc67_insert_zero_bit(U64 v, int pos)
{
	U64 high_mask = ~0ull << pos;
	return (v & ~high_mask) | ((v & high_mask) << 1);
}

static const BC67Partition *bc67_get_partition(U8 ns, U32 id)
{
	static const U32 first_part[4] = { 0, 0, 1, 65 };
	return bc67_partitions + first_part[ns] + id;
}

static U64 bc67_read_inds(BC67BitBuf * buf, int index_bits, int anchor1_ind, int anchor2_ind)
{
	// index_bits bits per pixel, minus 1 anchor pixel per subset
	int total_index_bits = index_bits*16 - 1 - (anchor1_ind != 0) - (anchor2_ind != 0);
	U64 index = buf->get(total_index_bits & 31);
	if (total_index_bits >= 32)
		index |= static_cast<U64>(buf->get32()) << (total_index_bits & 31);

	// index 0 is always an anchor
	// insert a 0 bit in the MSB position of all anchor inds
	int msb_pos = index_bits - 1;
	index = bc67_insert_zero_bit(index, msb_pos);
	if (anchor1_ind != 0)
		index = bc67_insert_zero_bit(index, anchor1_ind*index_bits + msb_pos);
	if (anchor2_ind != 0)
		index = bc67_insert_zero_bit(index, anchor2_ind*index_bits + msb_pos);

	return index;
}

static void bc7_expand(U8 * endpt_vals, int num_in_bits, int num_endpts)
{
	RR_ASSERT(5 <= num_in_bits && num_in_bits <= 8);
	// multipliers to replicate 5, 6, 7 or 8 bits
	static const int mults[4] = { (1<<11)+(1<<6), (1<<10)+(1<<4), (1<<9)+(1<<2), (1<<8)+(1<<0) };

	int mul = mults[num_in_bits - 5];
	for (int i = 0; i < num_endpts; ++i)
		endpt_vals[i] = static_cast<U8>((endpt_vals[i] * mul) >> 8);
}

// dest is in RGBA layout, but we only handle one channel
static void bc7_interp(U8 * dest, SINTa stride_in_bytes, const U8 * endpts, U64 inds, U32 index_bits, U32 subset_mask)
{
	// KDFS table 30
	static const U8 weight_table[] =
	{
		0,0,0,0,												// padding (ignored)
		0,21,43,64,												// 2bit inds
		0, 9,18,27, 37,46,55,64,								// 3bit inds
		0, 4, 9,13, 17,21,26,30, 34,38,43,47, 51,55,60,64		// 4bit inds
	};

	RR_ASSERT(index_bits >= 2 && index_bits <= 4);

	const U8 * weights = weight_table + (16 >> (4 - index_bits));
	U32 index_mask = (1u << index_bits) - 1;

	for (U32 y = 0; y < 4; ++y)
	{
		U8 *row = dest + y*stride_in_bytes;
		for (U32 x = 0; x < 4; ++x)
		{
			int subset = subset_mask & 3;
			U32 w = weights[inds & index_mask];

			row[x*4] = static_cast<U8>(((64 - w) * endpts[subset*2 + 0] + w * endpts[subset*2 + 1] + 32) >> 6);
			subset_mask >>= 2;
			inds >>= index_bits;
		}
	}
}

static const BC7ModeDesc * bc7_read_mode(BC67BitBuf * buf)
{
	U32 mode = 0;
	while (!buf->get(1))
	{
		if (++mode == 8) // eight consecutive 0 bits is invalid
			return 0;
	}

	return &bc7_modes[mode];
}

static void bc7_decode_endpoints(U8 endpoints[4][6], BC67BitBuf * buf, const BC7ModeDesc * m)
{
	U32 num_endpts = m->ns * 2;

	// RGB endpoints
	U32 num_color_bits = m->cb;
	for (U32 ch = 0; ch < 3; ++ch)
		for (U32 i = 0; i < num_endpts; ++i)
			endpoints[ch][i] = static_cast<U8>(buf->get(num_color_bits));

	// Alpha endpoints
	U32 num_alpha_bits = m->ab;
	for (U32 i = 0; i < num_endpts; ++i)
		endpoints[3][i] = static_cast<U8>(buf->get(num_alpha_bits));

	// P-bits
	U32 num_pbits = m->epb + m->spb;
	if (num_pbits)
	{
		U32 pbits;
		if (m->spb) // shared P-bits per subset?
		{
			pbits = 0;
			for (U32 i = 0; i < num_endpts; i += 2)
				pbits |= (buf->get(1) * 3) << i;
		}
		else // unique P-bit per endpoint
			pbits = buf->get(num_endpts);

		// now add P-bits to encoded endpoint
		for (U32 i = 0; i < num_endpts; ++i)
		{
			U32 pbit = pbits & 1;
			for (U32 ch = 0; ch < 4; ++ch)
				endpoints[ch][i] = (endpoints[ch][i] << 1) | (U8)pbit;

			pbits >>= 1;
		}
	}

	// Expand endpoints to 8 bits
	for (U32 ch = 0; ch < 3; ++ch)
		bc7_expand(endpoints[ch], num_color_bits + num_pbits, num_endpts);

	// if no alpha, make all endpoints' alpha values 1
	if (num_alpha_bits == 0)
	{
		for (U32 i = 0; i < 6; ++i)
			endpoints[3][i] = 255;
	}
	else
		bc7_expand(endpoints[3], num_alpha_bits + num_pbits, num_endpts);
}

void bc7_decode_block(U8 * out_rgba, SINTa stride_in_bytes, const void * block)
{
	BC67BitBuf buf;
	buf.init(block);

	const BC7ModeDesc * m = bc7_read_mode(&buf);
	if (!m) // invalid mode
	{
		for (U32 y = 0; y < 4; ++y)
			memset(out_rgba + y * stride_in_bytes, 0, 4 * 4);
		return;
	}

	U32 partition_id = buf.get(m->pb);
	U32 rot = buf.get(m->rb);
	U32 ind_sel = buf.get(m->isb);

	U8 endpoints[4][6]; // [channel][index]
	bc7_decode_endpoints(endpoints, &buf, m);

	// read indices
	const BC67Partition * part = bc67_get_partition(m->ns, partition_id);
	U64 inds[2];
	U32 indbits[2];

	indbits[0] = m->ib;
	inds[0] = bc67_read_inds(&buf, m->ib, part->anchor0, part->anchor1);
	if (m->ib2)
	{
		indbits[1] = m->ib2;
		inds[1] = bc67_read_inds(&buf, m->ib2, part->anchor0, part->anchor1);
	}
	else
	{
		indbits[1] = m->ib;
		inds[1] = inds[0];
	}

	// interpolate R/G/B/A (taking care of swap)
	U32 subset_mask = part->subset_mask;
	U32 chan_swap = (rot - 1) & 3; // index of channel that's swapped with A

	bc7_interp(out_rgba + (chan_swap == 0 ? 3 : 0), stride_in_bytes, endpoints[0], inds[ind_sel], indbits[ind_sel], subset_mask);
	bc7_interp(out_rgba + (chan_swap == 1 ? 3 : 1), stride_in_bytes, endpoints[1], inds[ind_sel], indbits[ind_sel], subset_mask);
	bc7_interp(out_rgba + (chan_swap == 2 ? 3 : 2), stride_in_bytes, endpoints[2], inds[ind_sel], indbits[ind_sel], subset_mask);
	bc7_interp(out_rgba + chan_swap, stride_in_bytes, endpoints[3], inds[ind_sel ^ 1], indbits[ind_sel ^ 1], subset_mask);

	// at this point, we _should_ have consumed all bits in the block.
	RR_ASSERT(buf.is_finished());
}

static void bc67_unpack_inds(U8 out[16], U64 inds, U32 indbits)
{
	U32 mask = (1 << indbits) - 1;
	for (U32 i = 0; i < 16; ++i)
	{
		out[i] = (U8) (inds & mask);
		inds >>= indbits;
	}
}

void bc7_analyze_block(BC7BlockDesc * desc, const void * block)
{
	BC67BitBuf buf;
	buf.init(block);

	memset(desc, 0, sizeof(*desc));
	const BC7ModeDesc * m = bc7_read_mode(&buf);
	desc->mode = m;
	if (!m) // invalid
		return;

	desc->partition_id = static_cast<U8>(buf.get(m->pb));
	desc->rot = static_cast<U8>(buf.get(m->rb));
	desc->ind_sel = static_cast<U8>(buf.get(m->isb));

	U8 endpoints[4][6]; // [channel][index] order
	bc7_decode_endpoints(endpoints, &buf, m);

	const BC67Partition * part = bc67_get_partition(m->ns, desc->partition_id);
	desc->partition = part;

	// read indices
	U64 inds[2];
	U32 indbits[2];

	indbits[0] = m->ib;
	inds[0] = bc67_read_inds(&buf, m->ib, part->anchor0, part->anchor1);
	if (m->ib2)
	{
		indbits[1] = m->ib2;
		inds[1] = bc67_read_inds(&buf, m->ib2, part->anchor0, part->anchor1);
	}
	else
	{
		indbits[1] = m->ib;
		inds[1] = inds[0];
	}

	desc->color_ind_bits = (U8)indbits[desc->ind_sel];
	desc->alpha_ind_bits = (U8)indbits[desc->ind_sel ^ 1];

	bc67_unpack_inds(desc->color_inds, inds[desc->ind_sel], indbits[desc->ind_sel]);
	bc67_unpack_inds(desc->alpha_inds, inds[desc->ind_sel ^ 1], indbits[desc->ind_sel ^ 1]);

	// swizzle endpoints according to rot
	U32 chan_swap = (desc->rot - 1) & 3; // index of channel that's swapped with A
	for (U32 ch = 0; ch < 4; ++ch)
	{
		U32 out_ch = (chan_swap == ch) ? 3 : ch;
		for (int i = 0; i < m->ns * 2; ++i)
			desc->endpoints[i][out_ch] = endpoints[ch][i];
	}
}

bool bc7_analyze_block_raw(BC7BlockRawDesc * desc, const void * block)
{
	BC67BitBuf buf;
	buf.init(block);

	const BC7ModeDesc * m = bc7_read_mode(&buf);
	desc->mode = m;
	if (!m) // invalid
		return false;

	desc->partition_id = static_cast<U8>(buf.get(m->pb));
	desc->rot = static_cast<U8>(buf.get(m->rb));
	desc->ind_sel = static_cast<U8>(buf.get(m->isb));

	bc7_decode_endpoints(desc->endpoints_raw, &buf, m);

	const BC67Partition * part = bc67_get_partition(m->ns, desc->partition_id);

	// read and unpack indices
	U64 inds0 = bc67_read_inds(&buf, m->ib, part->anchor0, part->anchor1);
	bc67_unpack_inds(desc->inds[0], inds0, m->ib);

	if (m->ib2)
	{
		U64 inds1 = bc67_read_inds(&buf, m->ib2, part->anchor0, part->anchor1);
		bc67_unpack_inds(desc->inds[1], inds1, m->ib2);
	}

	return true;
}

static const BC6HModeDesc *bc6h_read_mode(BC67BitBuf * buf)
{
	// BC6H modes (KDFS table 56)
	static const BC6HModeDesc modes[14] =
	{
		// #L,#K,NS,PB,EPB,DB,BC,IB
		{   0, 0, 2, 5, 10, 5, 3, 3 }, // Khronos mode 0=D3D mode 1
		{   1, 1, 2, 5,  7, 6, 3, 3 }, // Khronos mode 1=D3D mode 2
		{   2, 2, 2, 5, 11, 4, 0, 3 }, // Khronos mode 2=D3D mode 3
		{   3, 6, 2, 5, 11, 4, 1, 3 }, // Khronos mode 6=D3D mode 4
		{   4,10, 2, 5, 11, 4, 2, 3 }, // Khronos mode 10=D3D mode 5
		{   5,14, 2, 5,  9, 5, 3, 3 }, // Khronos mode 14=D3D mode 6
		{   6,18, 2, 5,  8, 5, 0, 3 }, // Khronos mode 18=D3D mode 7
		{   7,22, 2, 5,  8, 5, 1, 3 }, // Khronos mode 22=D3D mode 8
		{   8,26, 2, 5,  8, 5, 2, 3 }, // Khronos mode 26=D3D mode 9
		{   9,30, 2, 5,  6, 0, 3, 3 }, // Khronos mode 30=D3D mode 10
		{  10, 3, 1, 0, 10, 0, 3, 4 }, // Khronos mode 3=D3D mode 11
		{  11, 7, 1, 0, 11, 9, 3, 4 }, // Khronos mode 7=D3D mode 12
		{  12,11, 1, 0, 12, 8, 3, 4 }, // Khronos mode 11=D3D mode 13
		{  13,15, 1, 0, 16, 4, 3, 4 }, // Khronos mode 15=D3D mode 14
	};

	U32 mode = buf->get(2);
	if (mode >= 2)
	{
		// We number modes sequentially as in the D3D spec (differently from the Khronos spec)
		mode = 2 + (mode - 2) * 8 + buf->get(3);
		if (mode >= 14) // invalid mode number?
			return 0;
	}

	return &modes[mode];
}

static U32 bc6h_bitrev6(U32 x)
{
	// Precomputed bit reverse table for 6 bits
	static const U8 tab[64] =
	{
		0x00, 0x20, 0x10, 0x30, 0x08, 0x28, 0x18, 0x38,
		0x04, 0x24, 0x14, 0x34, 0x0c, 0x2c, 0x1c, 0x3c,
		0x02, 0x22, 0x12, 0x32, 0x0a, 0x2a, 0x1a, 0x3a,
		0x06, 0x26, 0x16, 0x36, 0x0e, 0x2e, 0x1e, 0x3e,
		0x01, 0x21, 0x11, 0x31, 0x09, 0x29, 0x19, 0x39,
		0x05, 0x25, 0x15, 0x35, 0x0d, 0x2d, 0x1d, 0x3d,
		0x03, 0x23, 0x13, 0x33, 0x0b, 0x2b, 0x1b, 0x3b,
		0x07, 0x27, 0x17, 0x37, 0x0f, 0x2f, 0x1f, 0x3f,
	};
	return tab[x];
}

static void bc6h_decode_endpoints(S32 endpoints[3][4], S32 e[3][4], BC67BitBuf * buf, const BC6HModeDesc * m, bool is_signed)
{
	U32 extra = 0;

	// reading the endpoint bits is an unholy mess, unfortunately
	// from looking at it, the idea seems to be to reduce the number of muxes
	// needed in a HW decoder by keeping lots of bits in the same location at all
	// times
	//
	// mainly the low bits for R_0,G_0,B_0, R_1,G_1,B_1, R_2,G_2 are always in the same place
	// extra high bits for these fields (when present) as well as G_2, G_3, B_2 and B_3 are
	// spread over the rest of the block and need to be assembled in a mode-specific fashion
	if (m->mode <= 1)
		extra = buf->get(3);

	e[0][0] = buf->get(10); // R_0^(9..0) in mode 0
	e[1][0] = buf->get(10); // G_0^(9..0) in mode 0
	e[2][0] = buf->get(10); // B_0^(9..0) in mode 0
	e[0][1] = buf->get(10); // R_1^(9..0) in mode 3
	e[1][1] = buf->get(10); // G_1^(9..0) in mode 3
	e[2][1] = buf->get(10); // B_1^(9..0) in mode 3

	if (m->ns >= 2)
	{
		e[0][2] = buf->get(6); // R_2^(5..0) in mode 1
		e[0][3] = buf->get(6); // R_3^(5..0) in mode 1

		// the low 4 bits for G_2, G_3, B_2 are also always in the same location
		e[1][2] = e[0][1] >> 6; // G_2^(3..0)
		e[1][3] = e[1][1] >> 6; // G_3^(3..0)
		e[2][2] = e[2][1] >> 6; // B_2^(3..0)
	}

	// we've now read the major bit groups from their "canonical" locations
	// the low bits of the first two endpoints should mostly be in place,
	// but now we need to shuffle around the remaining bits, which is just
	// a per-mode random mess.
	switch (m->linmode)
	{
	case 0:
		e[1][2] |= (extra & 1) << 4; // G_2
		e[1][3] |= (e[0][1] & 0x20) >> 1; // G_3
		e[2][2] |= (extra & 2) << 3; // B_2
		e[2][3] = ((e[1][1] & 0x20) >> 5) | ((e[2][1] & 0x20) >> 4) | ((e[0][2] & 0x20) >> 3) | ((e[0][3] & 0x20) >> 2) | ((extra & 4) << 2); // B_3
		break;

	case 1:
		e[1][2] |= ((e[1][0] & 0x200) >> 5) | ((extra & 1) << 5); // G_2
		e[1][3] |= ((extra & 6) << 3); // G_3
		e[2][2] |= ((e[0][0] & 0x200) >> 5) | ((e[1][0] & 0x80) >> 2); // B_2
		e[2][3] = ((e[0][0] & 0x180) >> 7) | ((e[1][0] & 0x100) >> 6) | ((e[2][0] & 0x80) >> 4) | ((e[2][0] & 0x100) >> 3) | ((e[2][0] & 0x200) >> 5); // B_3
		break;

	case 2:
		e[0][0] |= (e[0][1] & 0x20) << 5; // R_0
		e[1][0] |= (e[1][1] & 0x10) << 6; // G_0
		e[2][0] |= (e[2][1] & 0x10) << 6; // B_0
		e[2][3] = ((e[1][1] & 0x20) >> 5) | ((e[2][1] & 0x20) >> 4) | ((e[0][2] & 0x20) >> 3) | ((e[0][3] & 0x20) >> 2); // B_3
		break;

	case 3: // =Khronos mode 6
		e[0][0] |= (e[0][1] & 0x10) << 6; // R_0
		e[1][0] |= (e[1][1] & 0x20) << 5; // G_0
		e[2][0] |= (e[2][1] & 0x10) << 6; // B_0
		e[1][2] |= (e[0][3] & 0x10); // G_2
		e[1][3] |= (e[0][1] & 0x20) >> 1; // G_3
		e[2][3] = ((e[0][2] & 0x10) >> 4) | ((e[2][1] & 0x20) >> 4) | ((e[0][2] & 0x20) >> 3) | ((e[0][3] & 0x20) >> 2); // B_3
		break;

	case 4: // =Khronos mode 10
		e[0][0] |= (e[0][1] & 0x10) << 6; // R_0
		e[1][0] |= (e[1][1] & 0x10) << 6; // G_0
		e[2][0] |= (e[2][1] & 0x20) << 5; // B_0
		e[2][2] |= (e[0][1] & 0x20) >> 1; // B_2
		e[2][3] = ((e[1][1] & 0x20) >> 5) | ((e[0][2] & 0x30) >> 3) | ((e[0][3] & 0x20) >> 2) | (e[0][3] & 0x10); // B_3
		break;

	case 5: // =Khronos mode 14
		e[1][2] |= (e[1][0] & 0x200) >> 5; // G_2
		e[1][3] |= (e[0][1] & 0x20) >> 1; // G_3
		e[2][2] |= (e[0][0] & 0x200) >> 5; // B_2
		e[2][3] = ((e[1][1] & 0x20) >> 5) | ((e[2][1] & 0x20) >> 4) | ((e[0][2] & 0x20) >> 3) | ((e[0][3] & 0x20) >> 2) | ((e[2][0] & 0x200) >> 5); // B_3
		break;

	case 6: // =Khronos mode 18
		e[1][2] |= (e[1][0] & 0x200) >> 5; // G_2
		e[1][3] |= (e[0][0] & 0x100) >> 4; // G_3
		e[2][2] |= (e[0][0] & 0x200) >> 5; // B_2
		e[2][3] = ((e[1][1] & 0x20) >> 5) | ((e[2][1] & 0x20) >> 4) | ((e[1][0] & 0x100) >> 6) | ((e[2][0] & 0x300) >> 5); // B_3
		break;

	case 7: // =Khronos mode 22
		e[1][2] |= ((e[1][0] & 0x200) >> 5) | ((e[1][0] & 0x100) >> 3); // G_2
		e[1][3] |= ((e[0][1] & 0x20) >> 1) | ((e[2][0] & 0x100) >> 3); // G_3
		e[2][2] |= (e[0][0] & 0x200) >> 5; // B_2
		e[2][3] = ((e[0][0] & 0x100) >> 8) | ((e[2][1] & 0x20) >> 4) | ((e[0][2] & 0x20) >> 3) | ((e[0][3] & 0x20) >> 2) | ((e[2][0] & 0x200) >> 5); // B_3
		break;

	case 8: // =Khronos mode 26
		e[1][2] |= (e[1][0] & 0x200) >> 5; // G_2
		e[1][3] |= (e[0][1] & 0x20) >> 1; // G_3
		e[2][2] |= ((e[0][0] & 0x200) >> 5) | ((e[1][0] & 0x100) >> 3); // B_2
		e[2][3] = ((e[1][1] & 0x20) >> 5) | ((e[0][0] & 0x100) >> 7) | ((e[0][2] & 0x20) >> 3) | ((e[0][3] & 0x20) >> 2) | ((e[2][0] & 0x200) >> 5) | ((e[2][0] & 0x100) >> 3); // B_3
		break;

	case 9: // =Khronos mode 30
		e[1][2] |= ((e[1][0] & 0x200) >> 5) | ((e[1][0] & 0x40) >> 1); // G_2
		e[1][3] |= ((e[0][0] & 0x40) >> 2) | ((e[2][0] & 0x40) >> 1); // G_3
		e[2][2] |= ((e[0][0] & 0x200) >> 5) | ((e[1][0] & 0x80) >> 2); // B_2
		e[2][3] = ((e[0][0] & 0x180) >> 7) | ((e[1][0] & 0x100) >> 6) | ((e[2][0] & 0x80) >> 4) | ((e[2][0] & 0x100) >> 3) | ((e[2][0] & 0x200) >> 5); // B_3
		break;

	case 10: // =Khronos mode 3
		break; // nothing to do!

	case 11: // =Khronos mode 7
	case 12: // =Khronos mode 11
	case 13: // =Khronos mode 15
		// the top bits of R_0, G_0, B_0 are extended with bits from the top of R_1, G_1, B_1 field,
		// in reverse order.
		// Just append the whole top 6 bits that are used in mode 13 (=Khronos 15) to R_0/G_0/B_0,
		// in the modes where these are actually part of R_1/G_1/B_1 they will be ignored below,
		// and it keeps this code simpler.
		e[0][0] |= bc6h_bitrev6(e[0][1] >> 4) << 10; // R_0
		e[1][0] |= bc6h_bitrev6(e[1][1] >> 4) << 10; // G_0
		e[2][0] |= bc6h_bitrev6(e[2][1] >> 4) << 10; // B_0
		break;

	default:
		RR_BREAK(); // should never happen
	}

	// apply deltas to get absolute endpoint values
	int num_ep = m->ns * 2;
	if (m->db)
	{
		for (int chan = 0; chan < 3; ++chan)
		{
			int delta_bits = m->db + (chan == m->bc);
			int sign_bit = 1 << (delta_bits - 1);
			int value_bits = sign_bit - 1;

			for (int i = 1; i < num_ep; ++i)
			{
				// sign extend from source width
				int delta = (e[chan][i] & value_bits) - (e[chan][i] & sign_bit);
				e[chan][i] = e[chan][0] + delta;
			}
		}
	}

	// mask/sign extend endpoint values and dequantize
	if (!is_signed)
	{
		int epb = m->epb;
		S32 zext_mask = (1 << epb) - 1;

		for (int chan = 0; chan < 3; ++chan)
		{
			for (int i = 0; i < num_ep; ++i)
			{
				S32 x, deq;

				x = e[chan][i] & zext_mask;
				e[chan][i] = x; // write back masked, zero-extended value

				if (epb >= 16) // spec says >=15, but that makes no sense with the value range; either way there's only 12 and then 16 bits
					deq = x;
				else if (x == 0)
					deq = 0;
				else if (x == zext_mask)
					deq = 0xffff;
				else
					deq = ((x << 16) + 0x8000) >> epb;

				endpoints[chan][i] = deq;
			}
		}
	}
	else
	{
		int epb = m->epb;
		S32 sign_bit = 1 << (epb - 1);
		S32 value_bits = sign_bit - 1;

		for (int chan = 0; chan < 3; ++chan)
		{
			for (int i = 0; i < num_ep; ++i)
			{
				S32 x, deq;

				// sign extend from source width
				x = (e[chan][i] & value_bits) - (e[chan][i] & sign_bit);
				e[chan][i] = x; // write back masked, sign-extended value

				if (epb >= 16)
					deq = x;
				else
				{
					// extract sign and absolute value
					S32 xsign = x >> 31;
					S32 xabs = (x ^ xsign) - xsign;

					// dequantize
					if (xabs == 0)
						deq = 0;
					else if (xabs >= value_bits)
						deq = 0x7fff;
					else
						deq = ((xabs << 15) + 0x4000) >> (epb - 1);

					// re-apply sign
					deq = (deq ^ xsign) - xsign;
				}

				endpoints[chan][i] = deq;
			}
		}
	}
}

static void bc6h_interp(U16 * dest, SINTa stride_in_bytes, S32 endpts[3][4], U64 inds, U32 index_bits, U32 subset_mask, bool is_signed)
{
	// KDFS table 30
	static const U8 weight_table[] =
	{
		0, 9,18,27, 37,46,55,64,                                // 3bit inds
		0, 4, 9,13, 17,21,26,30, 34,38,43,47, 51,55,60,64       // 4bit inds
	};
	RR_ASSERT(index_bits >= 3 && index_bits <= 4);

	const U8 *weights = weight_table + (index_bits == 4 ? 8 : 0);
	U32 index_mask = (1u << index_bits) - 1;

	for (U32 y = 0; y < 4; ++y)
	{
		U16 *row = (U16 *)((U8 *)dest + y*stride_in_bytes);
		for (U32 x = 0; x < 4; ++x)
		{
			U32 subset = subset_mask & 3;
			S32 w = weights[inds & index_mask];

			for (int chan = 0; chan < 3; ++chan)
			{
				int result;

				// interpolate
				int interp = ((64 - w) * endpts[chan][subset*2 + 0] + w * endpts[chan][subset*2 + 1] + 32) >> 6;

				// final dequantization step
				if (!is_signed)
					result = (interp * 31) >> 6;
				else
				{
					if (interp < 0)
						result = ((-interp * 31) >> 5) | 0x8000;
					else
						result = (interp * 31) >> 5;
				}

				row[x*4 + chan] = (U16)result;
			}

			row[x*4 + 3] = 0x3c00; // a (1.0h)

			subset_mask >>= 2;
			inds >>= index_bits;
		}
	}
}

void bc6h_decode_block(U16 * out_rgba, SINTa stride_in_bytes, rrbool is_signed, const void * block)
{
	BC67BitBuf buf;
	buf.init(block);

	const BC6HModeDesc * m = bc6h_read_mode(&buf);
	if (!m) // invalid mode
	{
		for (U32 y = 0; y < 4; ++y)
		{
			U16 *out_row = (U16 *)((U8 *)out_rgba + y*stride_in_bytes);
			for (U32 x = 0; x < 4; ++x)
			{
				out_row[x*4 + 0] = 0; // r
				out_row[x*4 + 1] = 0; // g
				out_row[x*4 + 2] = 0; // b
				out_row[x*4 + 3] = 0x3c00; // a (1.0h)
			}
		}
		return;
	}

	S32 endpoints[3][4]; // [channel][index]
	S32 endpoints_q[3][4];
	bool signed_flag = is_signed != 0;
	bc6h_decode_endpoints(endpoints, endpoints_q, &buf, m, signed_flag);

	U32 partition_id = buf.get(m->pb);

	// read indices
	const BC67Partition * part = bc67_get_partition(m->ns, partition_id);
	U64 inds = bc67_read_inds(&buf, m->ib, part->anchor0, part->anchor1);

	// at this point, we _should_ have consumed all bits in the block.
	RR_ASSERT(buf.is_finished());

	// interpolate R/G/B (A is always 1.0)
	bc6h_interp(out_rgba, stride_in_bytes, endpoints, inds, m->ib, part->subset_mask, signed_flag);
}

bool bc6h_analyze_block_raw(BC6HBlockRawDesc * desc, rrbool is_signed, const void * block)
{
	BC67BitBuf buf;
	buf.init(block);

	const BC6HModeDesc * m = bc6h_read_mode(&buf);
	desc->mode = m;
	if (!m)
		return false;

	bc6h_decode_endpoints(desc->endpoints_internal, desc->endpoints_quant, &buf, m, is_signed != 0);
	desc->partition_id = static_cast<U8>(buf.get(m->pb));

	// read and unpack indices
	const BC67Partition * part = bc67_get_partition(m->ns, desc->partition_id);
	U64 inds = bc67_read_inds(&buf, m->ib, part->anchor0, part->anchor1);
	bc67_unpack_inds(desc->inds, inds, m->ib);

	// at this point, we _should_ have consumed all bits in the block.
	RR_ASSERT(buf.is_finished());

	return true;
}

OODLE_NS_END
