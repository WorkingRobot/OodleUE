// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "bc7prep_decode.h"
#include "bc7prep_format.h"
#include "rrmath.h"
#include "cpux86.h"

/*
#include "rrsimpleprof.h"
/*/
#include "rrsimpleprofstub.h"
/**/

OODLE_NS_START

namespace bc7prep {

// ---- General support code

static U64 bit_extract(U64 val, U64 start, U64 width)
{
	if (!width)
		return 0;
	U64 mask = (1ull << (width & 63)) - 1;
	return (val >> start) & mask;
}

static void decorr_to_RGB(int out[3], int y, int cr, int cb, int mask)
{
	out[0] = (y + cr) & mask;
	out[1] = y;
	out[2] = (y + cb) & mask;
}

static void decorr_to_RGB(int &y, int &cr, int &cb, int mask)
{
	int r = (y + cr) & mask;
	int g = y;
	int b = (y + cb) & mask;
	y = r;
	cr = g;
	cb = b;
}

static U64 packed_add(U64 a, U64 b, U64 msb_mask, U64 nonmsb_mask)
{
	// can sum low bits directly
	U64 low = (a & nonmsb_mask) + (b & nonmsb_mask);
	// need carryless sum (=XOR) in MSB
	return low ^ ((a ^ b) & msb_mask);
}

static U64 packed_add(U64 a, U64 b, U64 msb_mask)
{
	return packed_add(a, b, msb_mask, ~msb_mask);
}

static U64 packed_sub(U64 a, U64 b, U64 msb_mask, U64 nonmsb_mask)
{
	// sub is a + ~b + 1
	// subtract low bits in a way that's guaranteed not to wrap
	// MSB of results is set if (a & nonmsb) >= (b & nonmsb), clear otherwise
	U64 low = (a | msb_mask) - (b & nonmsb_mask);
	// carryless sum (=XOR) in MSBs; we need a ^ ~b ^ carry_in
	// carry_in is what we just computed in the MSBs of "low"
	return low ^ ((a ^ ~b) & msb_mask);
}

static U64 packed_sub(U64 a, U64 b, U64 msb_mask)
{
	return packed_sub(a, b, msb_mask, ~msb_mask);
}

static void decorr_to_RGB_packed(U64 &r, U64 &g, U64 &b, U64 msb_mask, U64 nonmsb_mask)
{
	U64 y = r;
	U64 cr = g;
	U64 cb = b;
	r = packed_add(y, cr, msb_mask, nonmsb_mask);
	g = y;
	b = packed_add(y, cb, msb_mask, nonmsb_mask);
}

static void decorr_to_RGB_packed(U64 &r, U64 &g, U64 &b, U64 msb_mask)
{
	U64 y = r;
	U64 cr = g;
	U64 cb = b;
	r = packed_add(y, cr, msb_mask);
	g = y;
	b = packed_add(y, cb, msb_mask);
}

/*
static U64 mode4_decorr_ref(U64 rgba)
{
	U64 rbits = (rgba >>  0) & 0x3ff;
	U64 gbits = (rgba >> 10) & 0x3ff;
	U64 bbits = (rgba >> 20) & 0x3ff;
	decorr_to_RGB_packed(rbits, gbits, bbits, 0x210, 0x1ef);
	return rbits | (gbits << 10) | (bbits << 20) | (rgba & 0xffc0000000ull);
}
*/

static U64 mode4_decorr_fast(U64 rgba)
{
	// broadcast the "Y" (really G) values
	U64 ybroad = ((U32)rgba & 0x3ff) * (1u + (1u << 10) + (1u << 20));

	// shuffle the "Cr"/"Cb" values around
	U64 crcba = ((rgba >> 10) & 0x3ff) | (rgba & 0xfffff00000ull);

	// do a single packed add to merge everything
	return packed_add(ybroad, crcba, 0x8421084210ull);
}

static U32 compact32to7_2x(U64 x)
{
	return ((U32)x & 0x7f) | ((U32)(x >> 25) & 0x3f80);
}

static U32 compact24to7_3x(U64 x)
{
	return ((U32)x & 0x7f) | ((U32)(x >> 17) & 0x3f80) | ((U32)(x >> 34) & 0x1fc000);
}

static U32 compact24to7_2x(U64 x)
{
	return ((U32)x & 0x7f) | ((U32)(x >> 17) & 0x3f80);
}

static U32 compact16to1_4x(U64 x)
{
	x &= 0x0001000100010001ull;
	x *= 0x0001000200040008ull;
	return (U32)(x >> 48);
}

static U32 compact16to1_2x(U32 x)
{
	return (x & 1) | ((x >> 15) & 2);
}

static U64 compact16to5_4x(U64 x)
{
	x &= 0x001f001f001f001full;
	x = ((x >> 11) | x) & 0x000003ff000003ffull;
	x = ((x >> 22) | x) & 0xfffff;
	return x;
}

static U32 compact16to5_2x(U32 x)
{
	return (x & 0x1f) | ((x >> 11) & 0x3e0);
}

// non-used bits must be cleared before this func
static U64 compact16to6_4x(U64 x)
{
	//x &= 0x003f003f003f003full; // assumed on input
	x = ((x >> 10) | x) & 0x00000fff00000fffull;
	x = ((x >> 20) | x) & 0xffffff;
	return x;
}

static U64 compact8to7_8x(U64 x)
{
	// move by 1
	const U64 stay1 = 0x007f007f007f007full;
	const U64 move1 = stay1 << 8;
	x = ((x & move1) >> 1) | (x & stay1);
	// now bits occupied: 0x3fff3fff3fff3fff

	// move by 2
	const U64 stay2 = 0x00003fff00003fffull;
	x = ((x & ~stay2) >> 2) | (x & stay2);
	// now bits occupied: 0x0fffffff0fffffff

	// move by 4
	const U64 stay4 = 0x000000000fffffffull;
	x = ((x & ~stay4) >> 4) | (x & stay4);

	return x;
}

static U64 compact8to7_6x(U64 x)
{
	// move by 1
	const U64 stay1 = 0x007f007f007full;
	const U64 move1 = stay1 << 8;
	x = ((x & move1) >> 1) | (x & stay1);
	// now bits occupied: 0x3fff3fff3fff3fff

	// move by multiples of 2
	x = ((x >> 0) & 0x0000000ffffull /* cheaper to mask with than 0x3fff */)
	  | ((x >> 2) & 0x0000fffc000ull)
	  | ((x >> 4) & 0x3fff0000000ull);

	return x;
}

static U32 compact8to1_8x(U64 x)
{
	x &= 0x0101010101010101ull;
	x *= 0x0102040810204080ull;
	return (U32)(x >> 56);
}

static U64 expand4to5_12x(U64 x)
{
	// move by multiples of 4
	x = ((x & 0x0000ffff00000000ull) << 8)
	  | ((x & 0x00000000ffff0000ull) << 4)
	  | ((x & 0x000000000000ffffull) << 0);
	// occupied bits now: 0x00ffff0ffff0ffff

	// move by 2
	const U64 stay2 = 0x0000ff000ff000ffull;
	x = ((x & ~stay2) << 2) | (x & stay2);

	// now within each group of 20 bits
	//   [3:0] val0 (0x0000f) ->   [3:0] (0x0000f)
	//   [7:4] val1 (0x000f0) ->   [8:5] (0x001e0) (move by 1)
	//   [9:8] (unused)
	// [13:10] val2 (0x03c00) -> [13:10] (0x03c00)
	// [17:14] val3 (0x3c000) -> [18:15] (0x78000) (move by 1)
	// [19:18] (unused)

	// move by 1
	// this uses (x << 1) = x + x
	// the bits above the values we shift by 1 are clear to begin with,
	// so we can do this:
	x += x & 0x03c0f03c0f03c0f0ull;

	return x;
}

static U64 expand4to5_4x(U64 x)
{
	// works the same way as expand4to5_12x above, just with fewer values
	x = ((x & 0x0ff00) << 2) | (x & 0x000ff); // move by 2
	x += x & 0x3c0f0; // move by 1

	return x;
}

static U32 expand2to6_4x(U32 x)
{
	// x &= 0xff; // already assumed
	x = ((x << 8) | x) & 0x0f00f;
	x = ((x << 4) | x) & 0xc30c3;
	return x;
}

static U64 expand1to5_12x(U64 x)
{
	// we can spread groups of 4 bits at a time using multiplies
	// and we can get groups of 4 bits where they need to be using another multiply
	x &= 0xfff;
	x = (x * 0x100010001ull) & 0xf0000f0000full;	// mul here does (x << 32) | (x << 16) | x
	x = (x * 0x1111) & 0x084210842108421ull;		// mul here does (x << 12) | (x << 8) | (x << 4) | x
	return x;
}

static U64 expand1to5_4x(U64 x)
{
	// same idea as in expand1to5_12x above, but it takes fewer steps
	return (U32(x & 0xf) * 0x1111) & 0x8421;
}

// ---- Actual decoders

#if OODLE_BUILD_CONFIG_SIMPLEPROFILER // this is always on now if simpleprof.h is included

// this merges the simpleprof blocks for all the template args
//	so it only depends on mode index, not colorspace,split, etc.
//	(as opposed to if you did it as a function-local SIMPLEPROF_INDEXED)

static U32 simpleprof_mode_index[BC7PREP_MODE_COUNT] = { }; // zero init
static const char * simpleprof_mode_label[BC7PREP_MODE_COUNT] = 
{
	RR_MAKE_INDEXED_STRING_ARRAY("bc7prep_mode",10)
};
RR_COMPILER_ASSERT( BC7PREP_MODE_COUNT == 10 );

#define SIMPLEPROFILE_MODE_N(mode_id, count) \
	::OODLE_NS::rrScopeProfiler mode_scope(&simpleprof_mode_index[mode_id],simpleprof_mode_label[mode_id], (int)(count))

#else

#define SIMPLEPROFILE_MODE_N(mode_id, count)

#endif

template<typename Tsplit, bool t_switch_colorspace>
static void un_munge_bc7_mode0(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target, OptimizedDecoderKernel * opt)
{
	SIMPLEPROFILE_MODE_N(0, nblocks);

	UINTa iblock = 0;

	if ( opt )
		iblock = opt(out_block, first_ptr, second_ptr, nblocks, target);

	for (; iblock < nblocks; iblock++)
	{
		U64 in_lo = RR_GET64_LE_UNALIGNED(first_ptr + iblock*Tsplit::first_advance);
		U64 in_hi = RR_GET64_LE_UNALIGNED(second_ptr + iblock*Tsplit::second_advance);

		U64 rbits = bit_extract(in_lo, 0, 24);
		U64 gbits = bit_extract(in_lo, 24, 24);
		U64 bbits = bit_extract(in_lo, 48, 16) | (bit_extract(in_hi, 0, 8) << 16);
		U64 partbits = bit_extract(in_hi, 8, 4);

		if (t_switch_colorspace)
			decorr_to_RGB_packed(rbits, gbits, bbits, 0x888888);

		U64 lo = 1; // mode bits
		lo |= partbits << 1;
		lo |= rbits << 5;
		lo |= gbits << 29;
		lo |= bbits << 53;
		U64 hi = bbits >> 11;
		hi |= in_hi & ~0x1fffull;

		U8 * RADRESTRICT dest = out_block + bc7prep::target_offs(target[iblock]);
		RR_PUT64_LE_UNALIGNED(dest + 0, lo);
		RR_PUT64_LE_UNALIGNED(dest + 8, hi);
	}
}

template<typename Tsplit, bool t_switch_colorspace>
static void un_munge_bc7_mode1(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target, OptimizedDecoderKernel * opt)
{
	SIMPLEPROFILE_MODE_N(1, nblocks);

	UINTa iblock = 0;

	if ( opt )
		iblock = opt(out_block, first_ptr, second_ptr, nblocks, target);

	for (; iblock < nblocks; iblock++)
	{
		U64 rgbs = RR_GET64_LE_UNALIGNED(first_ptr + iblock*Tsplit::first_advance); // 4*RGB655
		U64 extra = RR_GET64_LE_UNALIGNED(second_ptr + iblock*Tsplit::second_advance); // 4*2 extra g/b bits; 6 partition bits; 2 bits unused; 2 P bits; 46 index bits

		U64 rbits = compact16to6_4x((rgbs >>  0) & 0x003f003f003f003full);
		U64 gbits = compact16to6_4x((rgbs >>  5) & 0x003e003e003e003eull); // NOTE: bottom bits of g left clear
		U64 bbits = compact16to6_4x((rgbs >> 10) & 0x003e003e003e003eull); // NOTE: bottom bits of b left clear

		U32 expanded_gb = expand2to6_4x((U32) (extra & 0xff));
		gbits |= (expanded_gb >> 0) & 0x41041; // bottom bits of g
		bbits |= (expanded_gb >> 1) & 0x41041; // bottom bits of b

		if (t_switch_colorspace)
			decorr_to_RGB_packed(rbits, gbits, bbits, 0x820820);

		U64 lo = 0x2; // mode 1
		lo |= (extra >> 6) & 0xfc; // partition ID
		lo |= rbits << 8;
		lo |= gbits << 32;
		lo |= bbits << 56;
		U64 hi = bbits >> 8;
		hi |= extra & ~0xffff; // P bits and index

		U8 * RADRESTRICT dest = out_block + bc7prep::target_offs(target[iblock]);
		RR_PUT64_LE_UNALIGNED(dest + 0, lo);
		RR_PUT64_LE_UNALIGNED(dest + 8, hi);
	}
}

template<typename Tsplit, bool t_switch_colorspace>
static void un_munge_bc7_mode2(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target, OptimizedDecoderKernel * opt)
{
	SIMPLEPROFILE_MODE_N(2, nblocks);

	UINTa iblock = 0;

	if ( opt )
		iblock = opt(out_block, first_ptr, second_ptr, nblocks, target);

	for (; iblock < nblocks; iblock++)
	{
		U64 endpt0 = RR_GET64_LE_UNALIGNED(first_ptr + iblock*Tsplit::first_advance);
		U32 endpt1 = RR_GET32_LE_UNALIGNED(first_ptr + iblock*Tsplit::first_advance + 8);
		U32 index = RR_GET32_LE_UNALIGNED(second_ptr + iblock*Tsplit::second_advance);

		U64 rbits = compact16to5_4x(endpt0 >>  1) | (compact16to5_2x(endpt1 >>  1) << 20);
		U64 gbits = compact16to5_4x(endpt0 >>  6) | (compact16to5_2x(endpt1 >>  6) << 20);
		U64 bbits = compact16to5_4x(endpt0 >> 11) | (compact16to5_2x(endpt1 >> 11) << 20);
		U64 partbits = compact16to1_4x(endpt0)    | (compact16to1_2x(endpt1)       << 4);

		if (t_switch_colorspace)
			decorr_to_RGB_packed(rbits, gbits, bbits, 0x21084210);

		U64 lo = 0x4; // mode 2
		lo |= partbits << 3;
		lo |= rbits << 9;
		lo |= gbits << 39;
		U64 hi = gbits >> 25;
		hi |= bbits << 5;
		hi |= U64(index & ~7) << 32; // NOTE(fg): mask off invalid index bits; we have 3 unused bits here, ensure their value doesn't matter

		U8 * RADRESTRICT dest = out_block + bc7prep::target_offs(target[iblock]);
		RR_PUT64_LE_UNALIGNED(dest + 0, lo);
		RR_PUT64_LE_UNALIGNED(dest + 8, hi);
	}
}

template<typename Tsplit, bool t_switch_colorspace>
static void un_munge_bc7_mode3(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target, OptimizedDecoderKernel * opt)
{
	SIMPLEPROFILE_MODE_N(3, nblocks);

	UINTa iblock = 0;

	if ( opt )
		iblock = opt(out_block, first_ptr, second_ptr, nblocks, target);

	for (; iblock < nblocks; iblock++)
	{
		U64 endpt0 = RR_GET64_LE_UNALIGNED(first_ptr + iblock*Tsplit::first_advance);
		U32 endpt1 = RR_GET32_LE_UNALIGNED(first_ptr + iblock*Tsplit::first_advance + 8);
		U32 index = RR_GET32_LE_UNALIGNED(second_ptr + iblock*Tsplit::second_advance);

		U64 rbits = compact24to7_3x(endpt0 >>  0) | ((endpt1 & 0x007f00) << (21-8));
		U64 gbits = compact24to7_3x(endpt0 >>  8) | ((endpt1 & 0x7f0000) << (21-16));
		U64 bbits = compact24to7_2x(endpt0 >> 16) | (compact24to7_2x(endpt1) << 14);

		U64 partbits = compact8to1_8x(endpt0 >> 7);

		if (t_switch_colorspace)
			decorr_to_RGB_packed(rbits, gbits, bbits, 0x8102040);

		U64 lo = 0x8; // mode 3
		lo |= (partbits & 0x3f) << 4;
		lo |= rbits << 10;
		lo |= gbits << 38;
		U64 hi = gbits >> 26;
		hi |= bbits << 2;
		hi |= (partbits & 0xc0) << 24;
		hi |= (U64)index << 32;

		U8 * RADRESTRICT dest = out_block + bc7prep::target_offs(target[iblock]);
		RR_PUT64_LE_UNALIGNED(dest + 0, lo);
		RR_PUT64_LE_UNALIGNED(dest + 8, hi);
	}
}

template<typename Tsplit, bool t_switch_colorspace>
static void un_munge_bc7_mode4(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target, OptimizedDecoderKernel * opt)
{
	SIMPLEPROFILE_MODE_N(4, nblocks);

	UINTa iblock = 0;

	if ( opt )
		iblock = opt(out_block, first_ptr, second_ptr, nblocks, target);

	for (; iblock < nblocks; iblock++)
	{
		const U8 * lo_ptr = first_ptr + iblock*Tsplit::first_advance;
		U64 in_lo = RR_GET32_LE_UNALIGNED(lo_ptr) + ((U64)RR_GET16_LE_UNALIGNED(lo_ptr + 4) << 32);
		U64 in_hi0 = RR_GET16_LE_UNALIGNED(second_ptr + iblock*Tsplit::second_advance);
		U64 in_hi1 = RR_GET64_LE_UNALIGNED(second_ptr + iblock*Tsplit::second_advance + 2);

		// switch color space if required
		U64 rgba = bit_extract(in_lo, 2, 40);

		if (t_switch_colorspace)
			rgba = mode4_decorr_fast(rgba);

		// rotate back
		U64 crot = bit_extract(in_hi0, 0, 2);
		U64 shift_amount = ((0 - crot) & 3) * 10;

		// 3-xor trick for swap
		U64 xor_mask = (rgba ^ (rgba << shift_amount)) & 0xffc0000000ull;
		rgba ^= xor_mask;
		rgba ^= xor_mask >> shift_amount;

		// Make space for the extra A bits
		rgba += rgba & 0x0ffc0000000ull; // mask of bits that need to move left by >=1 step
		rgba += rgba & 0x1f000000000ull; // mask of bits that need to move left by >=2 steps

		// Now insert the extra A bits
		// The expression here does:
		// 1. Create two copies of the low 2 bits of in_lo, one left-shifted by 0 and one by 5 bits.
		// 2. Grab the low bit from the first copy and the high bit from the second copy.
		// 3. Move to the target position in the bit field
		rgba |= U64((U32(in_lo & 3) * 0x21) & 0x41) << 30;

		U64 lo = 0x10; // mode 4
		lo |= crot << 5;
		lo |= bit_extract(in_lo, 42, 1) << 7;
		lo |= rgba << 8;
		lo |= bit_extract(in_hi0, 2, 14) << 50;
		U64 hi = in_hi1;

		U8 * RADRESTRICT dest = out_block + bc7prep::target_offs(target[iblock]);
		RR_PUT64_LE_UNALIGNED(dest + 0, lo);
		RR_PUT64_LE_UNALIGNED(dest + 8, hi);
	}
}

template<typename Tsplit, bool t_switch_colorspace>
static void un_munge_bc7_mode5(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target, OptimizedDecoderKernel * opt)
{
	SIMPLEPROFILE_MODE_N(5, nblocks);

	UINTa iblock = 0;

	if ( opt )
		iblock = opt(out_block, first_ptr, second_ptr, nblocks, target);

	for (; iblock < nblocks; iblock++)
	{
		U64 endpoints = RR_GET64_LE_UNALIGNED(first_ptr + iblock*Tsplit::first_advance);
		U64 indices = RR_GET64_LE_UNALIGNED(second_ptr + iblock*Tsplit::second_advance);

		U64 lo = 0x20; // mode 5

		if (t_switch_colorspace)
		{
			U64 rbits = (endpoints >>  0) & 0x000000fe000000feull;
			U64 gbits = (endpoints >>  8) & 0x000000fe000000feull;
			U64 bbits = (endpoints >> 16) & 0x000000fe000000feull;
			decorr_to_RGB_packed(rbits, gbits, bbits, 0x8000000080ull, 0x7e0000007eull);
			endpoints = rbits | (gbits << 8) | (bbits << 16) | (endpoints & 0xff010101ff010101ull);

			// re-apply channel rotate: want to swap channel 3 with channel (crot - 1) & 3
			U32 crot = (U32)indices & 3;
			int shift_amount = ((0 - crot) & 3) << 3; // 3 - ((crot - 1) & 3) = (3 - crot + 1) & 3 = (4 - crot) & 3

			// 3-xor trick for swap
			U64 xor_mask = (endpoints ^ (endpoints << shift_amount)) & 0xff000000ff000000ull;
			endpoints ^= xor_mask;
			endpoints ^= xor_mask >> shift_amount;

			// now we have the channel bits in (post-rotate) R,G,B,A order
			// have (LSB->MSB order): r0 g0 b0 a0 r1 g1 b1 a1, 8b/field
			// now pack down!
			lo |= crot << 6;
			lo |= compact32to7_2x(endpoints >> 1) << 8;
			lo |= (U64)compact32to7_2x(endpoints >> 9) << 22;
			lo |= (U64)compact32to7_2x(endpoints >> 17) << 36;
			lo |= ((endpoints >> 24) & 0xff) << 50;
			lo |= ((endpoints >> 56) & 0x3f) << 58;
		}
		else
		{
			// re-apply channel rotate: want to swap channel 3 with channel (crot - 1) & 3
			U32 crot = (U32)indices & 3;
			int shift_amount = ((0 - crot) & 3) << 4; // 3 - ((crot - 1) & 3) = (3 - crot + 1) & 3 = (4 - crot) & 3

			// 3-xor trick for swap
			U64 xor_mask = (endpoints ^ (endpoints << shift_amount)) & 0xffff000000000000ull;
			endpoints ^= xor_mask;
			endpoints ^= xor_mask >> shift_amount;

			// now we have the channel bits in (post-rotate) R,G,B,A order
			// have (LSB->MSB order): r0 g0 b0 a0 r1 g1 b1 a1, 8b/field
			// now pack down!
			lo |= crot << 6;
			lo |= compact8to7_6x(endpoints >> 1) << 8;
			lo |= ((endpoints >> 48) & 0xff) << 50;
			lo |= ((endpoints >> 56) & 0x3f) << 58;
		}

		U64 hi = (endpoints >> 62) & 3;
		hi |= indices & ~3ull;

		U8 * RADRESTRICT dest = out_block + bc7prep::target_offs(target[iblock]);
		RR_PUT64_LE_UNALIGNED(dest + 0, lo);
		RR_PUT64_LE_UNALIGNED(dest + 8, hi);
	}
}

template<typename Tsplit, bool t_switch_colorspace>
static void un_munge_bc7_mode6(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target, OptimizedDecoderKernel * opt)
{
	SIMPLEPROFILE_MODE_N(6, nblocks);

	UINTa iblock = 0;

	if ( opt )
		iblock = opt(out_block, first_ptr, second_ptr, nblocks, target);

	for (; iblock < nblocks; iblock++)
	{
		U64 endpoints = RR_GET64_LE_UNALIGNED(first_ptr + iblock*Tsplit::first_advance);
		U64 indices = RR_GET64_LE_UNALIGNED(second_ptr + iblock*Tsplit::second_advance);

		U64 lo = 0x40; // mode bit

		if (t_switch_colorspace)
		{
			// de-interleave
			U64 rbits = compact32to7_2x(endpoints >>  0);
			U64 gbits = compact32to7_2x(endpoints >>  8);
			U64 bbits = compact32to7_2x(endpoints >> 16);
			U64 abits = compact32to7_2x(endpoints >> 24);
			decorr_to_RGB_packed(rbits, gbits, bbits, 0x2040);

			lo |= rbits << 7;
			lo |= gbits << 21;
			lo |= bbits << 35;
			lo |= abits << 49;
		}
		else
		{
			U64 deint = compact8to7_8x(endpoints);
			lo |= deint << 7;
		}

		lo |= endpoints & (1ull << 63); // P0 bit
		U64 hi = indices; // P1 bit, indices

		U8 * RADRESTRICT dest = out_block + bc7prep::target_offs(target[iblock]);
		RR_PUT64_LE_UNALIGNED(dest + 0, lo);
		RR_PUT64_LE_UNALIGNED(dest + 8, hi);
	}
}

template<typename Tsplit, bool t_switch_colorspace>
static void un_munge_bc7_mode7(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target, OptimizedDecoderKernel * opt)
{
	SIMPLEPROFILE_MODE_N(7, nblocks);

	UINTa iblock = 0;

	if ( opt )
		iblock = opt(out_block, first_ptr, second_ptr, nblocks, target);

	for (; iblock < nblocks; iblock++)
	{
		U64 prgbs = RR_GET64_LE_UNALIGNED(first_ptr + iblock*Tsplit::first_advance);
		U64 rest = RR_GET32_LE_UNALIGNED(first_ptr + iblock*Tsplit::first_advance + 8);
		rest |= U64(RR_GET32_LE_UNALIGNED(second_ptr + iblock*Tsplit::second_advance)) << 32;

		U64 lo, hi;

		if (t_switch_colorspace)
		{
			U64 rbits = compact16to5_4x(prgbs >> 1);
			U64 gbits = compact16to5_4x(prgbs >> 6);
			U64 bbits = compact16to5_4x(prgbs >> 11);
			U64 pbits = compact16to1_4x(prgbs);
			U64 abits = bit_extract(rest, 8, 20);

			decorr_to_RGB_packed(rbits, gbits, bbits, 0x84210);

			lo = 0x80; // mode
			lo |= bit_extract(rest, 28, 6) << 8; // partition
			lo |= rbits << 14;
			lo |= gbits << 34;
			lo |= bbits << 54;
			hi = bbits >> 10;
			hi |= abits << 10;
			hi |= pbits << 30;
		}
		else
		{
			U64 pbits = bit_extract(rest, 8, 4);
			U64 rgbbits, abits;

			rgbbits  = expand4to5_12x(prgbs) << 1;
			rgbbits |= expand1to5_12x(rest >> 12);
			abits  = expand4to5_4x(prgbs >> 48) << 1;
			abits |= expand1to5_4x(rest >> 24);

			lo = 0x80; // mode
			lo |= bit_extract(rest, 28, 6) << 8; // partition
			lo |= rgbbits << 14;
			hi  = rgbbits >> 50;
			hi |= abits << 10;
			hi |= pbits << 30;
		}

		hi |= rest & ~0x3FFFFffffull;

		U8 * RADRESTRICT dest = out_block + bc7prep::target_offs(target[iblock]);
		RR_PUT64_LE_UNALIGNED(dest + 0, lo);
		RR_PUT64_LE_UNALIGNED(dest + 8, hi);
	}
}

static void un_munge_bc7_mode8(U8 * RADRESTRICT out_block, const U8 * coded_block, const U8 *, UINTa nblocks, const U16 * target, OptimizedDecoderKernel * opt)
{
	SIMPLEPROFILE_MODE_N(8, nblocks);

	UINTa iblock = 0;

	if ( opt )
		iblock = opt(out_block, coded_block, NULL, nblocks, target);

	for (; iblock < nblocks; iblock++)
		memcpy(out_block + bc7prep::target_offs(target[iblock]), coded_block + iblock*16, 16);
}

template<bool t_switch_colorspace>
static void un_munge_bc7_mode9(U8 * RADRESTRICT out_block, const U8 * coded_block, const U8 *, UINTa nblocks, const U16 * target, OptimizedDecoderKernel * opt)
{
	SIMPLEPROFILE_MODE_N(9, nblocks);

	RR_ASSERT( opt == NULL );

	const U8 *coded_block_end = coded_block + nblocks*4;
	for (;;)
	{
		int r = coded_block[0];
		int g = coded_block[1];
		int b = coded_block[2];

		if (t_switch_colorspace)
			decorr_to_RGB(r, g, b, 255);

		const U64 bit6_mask = (0x40 << 8) | (0x40 << 22) | (0x40ull << 36);
		const U64 lo7_mask  = (0x7f << 8) | (0x7f << 22) | (0x7full << 36);
		U64 color_bits;

		// color channels (see bc7solid/main.cpp)
		// this is the bit-hack version written to process 3 color chans at once
		color_bits = (r << 8) | (g << 22) | ((U64)b << 36);

		U64 t = color_bits << 6;
		color_bits = ((color_bits >> 1) & lo7_mask) - (color_bits & ~lo7_mask);
		color_bits += t + (t & bit6_mask);

		// alpha: just set endpoint 0 to target value then use all-0 inds
		color_bits |= (U64)coded_block[3] << 50;

		// insert mode bits
		color_bits |= 0x20; // mode 5, rotation=0
		U32 coded_block32 = RR_GET32_LE_UNALIGNED(coded_block);

		// we expect solid blocks to come in runs, so store these output values for as long as we
		// see the same input
		do
		{
			U8 * RADRESTRICT dest = out_block + bc7prep::target_offs(target[0]);
			RR_PUT64_LE_UNALIGNED(dest + 0, color_bits); // this covers 64 out of the 66 mode/color bits
			RR_PUT64_LE_UNALIGNED(dest + 8, 0xaaaaaaac); // last 2 color bits, color index bits, alpha index bits

			target++;
			coded_block += 4;
			if (coded_block == coded_block_end)
				return;
		} while (RR_GET32_LE_UNALIGNED(coded_block) == coded_block32);
	}
}

OptimizedDecoderKernelSet opt_kernels_none =
{
	{
		UNMUNGE_NO_KERNEL, // mode 0
		UNMUNGE_NO_KERNEL, // mode 1
		UNMUNGE_NO_KERNEL, // mode 2
		UNMUNGE_NO_KERNEL, // mode 3
		UNMUNGE_NO_KERNEL, // mode 4
		UNMUNGE_NO_KERNEL, // mode 5
		UNMUNGE_NO_KERNEL, // mode 6
		UNMUNGE_NO_KERNEL, // mode 7
		UNMUNGE_NO_KERNEL, // mode 8
		UNMUNGE_NO_KERNEL, // mode 9
	},
	0, // sort
};

typedef void un_munge_bc7_kernel(U8 * RADRESTRICT out_block, const U8 * coded_first, const U8 * coded_second, UINTa nblocks, const U16 * target, OptimizedDecoderKernel * opt);

static un_munge_bc7_kernel *bc7_un_munge_kernel[BC7PREP_MODE_COUNT][4] =
{
	UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode0, bc7prep::SplitAt8),
	UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode1, bc7prep::SplitAt8),
	UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode2, bc7prep::SplitAt12),
	UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode3, bc7prep::SplitAt12),
	UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode4, bc7prep::SplitAt6),
	UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode5, bc7prep::SplitAt8),
	UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode6, bc7prep::SplitAt8),
	UNMUNGE_DISPATCH_VARIANTS(un_munge_bc7_mode7, bc7prep::SplitAt12),
	{ un_munge_bc7_mode8, un_munge_bc7_mode8, un_munge_bc7_mode8, un_munge_bc7_mode8 },
	{ un_munge_bc7_mode9<false>, un_munge_bc7_mode9<true>, un_munge_bc7_mode9<false>, un_munge_bc7_mode9<true> },
};

// we use split_pos=0 to indicate "never split"
// but for the decoder that actually means "all the data goes in the first half" not the second,
// so treat it as if the split point was at the block size
#define SPLIT(x,sz) ((x) ? (x) : (sz))

const SINTa decode_split_pos[BC7PREP_MODE_COUNT] =
{
	SPLIT(BC7PREP_MODE0_SPLIT, BC7PREP_MODE0_SIZE),
	SPLIT(BC7PREP_MODE1_SPLIT, BC7PREP_MODE1_SIZE),
	SPLIT(BC7PREP_MODE2_SPLIT, BC7PREP_MODE2_SIZE),
	SPLIT(BC7PREP_MODE3_SPLIT, BC7PREP_MODE3_SIZE),
	SPLIT(BC7PREP_MODE4_SPLIT, BC7PREP_MODE4_SIZE),
	SPLIT(BC7PREP_MODE5_SPLIT, BC7PREP_MODE5_SIZE),
	SPLIT(BC7PREP_MODE6_SPLIT, BC7PREP_MODE6_SIZE),
	SPLIT(BC7PREP_MODE7_SPLIT, BC7PREP_MODE7_SIZE),
	SPLIT(BC7PREP_MODE8_SPLIT, BC7PREP_MODE8_SIZE),
	SPLIT(BC7PREP_MODE9_SPLIT, BC7PREP_MODE9_SIZE),
};

#undef SPLIT

const SINTa mode_sizes[BC7PREP_MODE_COUNT] =
{
	BC7PREP_MODE0_SIZE,
	BC7PREP_MODE1_SIZE,
	BC7PREP_MODE2_SIZE,
	BC7PREP_MODE3_SIZE,
	BC7PREP_MODE4_SIZE,
	BC7PREP_MODE5_SIZE,
	BC7PREP_MODE6_SIZE,
	BC7PREP_MODE7_SIZE,
	BC7PREP_MODE8_SIZE,
	BC7PREP_MODE9_SIZE,
};

} // namespace bc7prep

OodleTexRT_Err bc7prep_read_header(const OodleTexRT_BC7PrepHeader * header, SINTa * out_num_blocks, SINTa * out_payload_size)
{
	// Version OK?
	if (header->version != BC7PREP_HEADER_VERSION)
		return OodleTexRT_Err_BC7PrepHeaderCorrupt;

	// All reserved flag bits clear?
	if ((header->flags & ~BC7PREP_FLAG_ALL_ASSIGNED) != 0)
		return OodleTexRT_Err_BC7PrepHeaderCorrupt;

	U32 total_block_count = 0;
	SINTa total_payload_size = 0;

	for (int i = 0; i < BC7PREP_MODE_COUNT; i++)
	{
		U32 count = header->mode_counts[i];
		if (count > BC7PREP_MAXBLOCKS - total_block_count)
			return OodleTexRT_Err_BC7PrepHeaderCorrupt;

		total_block_count += count;
		total_payload_size += count * bc7prep::mode_sizes[i];
	}

	// mode nibbles after payloads
	total_payload_size += (total_block_count + 1) / 2;

	if ( out_num_blocks ) *out_num_blocks = total_block_count;
	if ( out_payload_size ) *out_payload_size = total_payload_size;
	return OodleTexRT_Err_OK;
}

// Allow for sloppy writes off the ends of some arrays for our vector code
// (this is number of elements)
const int BC7PREP_ARRAY_SLOP = 64;

SINTa bc7prep_min_decode_work_mem_size(SINTa num_blocks)
{
	// sort works in chunks, but allow some sloppy writes off the
	// end of the array
	const SINTa adjusted_nblocks = RR_MIN(num_blocks, BC7PREP_CHUNK_BLOCK_COUNT) + BC7PREP_ARRAY_SLOP;

	SINTa work_size = RR_CACHE_LINE_SIZE - 1; // to get to alignment
	work_size += rrAlignUpA(BC7PREP_MODE_COUNT * adjusted_nblocks * sizeof(U16), RR_CACHE_LINE_SIZE); // output indices
	work_size += num_blocks * 16; // chunk output buffer

	return work_size;
}

static RADFORCEINLINE void bc7prep_sort(U16 * mode_cur[16], const U8 * mode_nibbles, SINTa blocks_in_chunk, const bc7prep::OptimizedDecoderKernelSet * opt_kernels)
{
	if ( opt_kernels->sort )
	{
		opt_kernels->sort(mode_cur, mode_nibbles, blocks_in_chunk);
		return;
	}

	SINTa i;

	// the main sorting loop; processes nibble pairs
	for (i = 0; i < (blocks_in_chunk & ~1); i += 2)
	{
		// no bounds check: earlier payload size check verified there's a full-sized
		// modes array after payload
		U8 two_nibbles = *mode_nibbles++;
		U8 mode;

		// no bounds checks:
		// 1. 0 <= mode <= 0xf; always in bounds of mode_cur
		// 2. any given mode can get at most blocks_in_chunk blocks assigned to it, and we
		//    allocate sufficient space for that
		mode = two_nibbles & 0xf;
		*mode_cur[mode]++ = static_cast<U16>(i + 0);

		mode = two_nibbles >> 4;
		*mode_cur[mode]++ = static_cast<U16>(i + 1);
	}

	// process last element if required
	if (i < blocks_in_chunk)
	{
		U8 last_mode = *mode_nibbles & 0xf;
		*mode_cur[last_mode]++ = static_cast<U16>(i);
	}
}

SINTa bc7prep_decode(U8 * output_buf, SINTa output_buf_size,
	const U8 * bc7prep_data, SINTa bc7prep_data_size,
	const OodleTexRT_BC7PrepHeader * header,
	U8 * work_mem, SINTa work_mem_size,
	U32 flags)
{
	SINTa num_blocks;
	SINTa expected_payload_size;
	OodleTexRT_Err err = bc7prep_read_header(header, &num_blocks, &expected_payload_size);
	if (err != OodleTexRT_Err_OK)
		return err;

	if (num_blocks == 0) // valid 0->0 return
		return 0;

	if (output_buf_size < num_blocks * 16)
		return OodleTexRT_Err_BC7PrepOutputBufTooSmall;

	if (work_mem_size < bc7prep_min_decode_work_mem_size(num_blocks))
		return OodleTexRT_Err_BC7PrepScratchBufTooSmall;

	if (bc7prep_data_size < expected_payload_size)
		return OodleTexRT_Err_BC7PrepInputTooSmall;

	// CPU dispatch setup
	const bc7prep::OptimizedDecoderKernelSet * opt_kernels = 0;

#ifdef DO_BUILD_AVX2
	if ( ! opt_kernels && rrCPUx86_feature_present(RRX86_CPU_AVX2))
	{
		// for dynamic voltage/frequency scaling reasons (we don't want texturert
		// to be the only thing in the process using AVX since it never runs long)
		if ( flags & BC7PREP_DECODE_AVOID_WIDE_VECTORS )
			opt_kernels = &bc7prep::opt_kernels_avx2_narrow;
		else
			opt_kernels = &bc7prep::opt_kernels_avx2;
	}
#endif

#ifdef DO_BUILD_SSE4
	if ( ! opt_kernels && rrCPUx86_feature_present(RRX86_CPU_SSSE3))
		opt_kernels = &bc7prep::opt_kernels_ssse3;
#endif

#ifdef __RADX86__
	if ( ! opt_kernels ) // SSE2 is our min spec, we don't support HW without it
		opt_kernels = &bc7prep::opt_kernels_sse2;
#endif

	if ( ! opt_kernels )
		opt_kernels = &bc7prep::opt_kernels_none;

	SIMPLEPROFILE_SCOPE_N(bc7prep_decode, num_blocks);

	SINTa mode_idx[BC7PREP_MODE_COUNT + 1];
	SINTa mode_pos[BC7PREP_MODE_COUNT + 1];

	// prefix sum
	mode_pos[0] = 0;
	mode_idx[0] = 0;
	for (int i = 0; i < BC7PREP_MODE_COUNT; i++)
	{
		mode_pos[i + 1] = mode_pos[i] + header->mode_counts[i] * bc7prep::mode_sizes[i];
		mode_idx[i + 1] = mode_idx[i] + header->mode_counts[i];
	}

	// mode nibbles right after payload
	const U8 * bc7prep_mode_nibbles = bc7prep_data + mode_pos[BC7PREP_MODE_COUNT];

	// lay out work mem
	const SINTa adjusted_nblocks = RR_MIN(num_blocks, BC7PREP_CHUNK_BLOCK_COUNT) + BC7PREP_ARRAY_SLOP;
	U16 * output_index_arrays = (U16 *)rrAlignUpPointer(work_mem, RR_CACHE_LINE_SIZE);
	U8 * chunk_buf = (U8 *)rrAlignUpPointer(output_index_arrays + BC7PREP_MODE_COUNT*adjusted_nblocks, RR_CACHE_LINE_SIZE);

	// determine which mode gets what kernel and what data
	bc7prep::un_munge_bc7_kernel * mode_kernel[BC7PREP_MODE_COUNT];
	bc7prep::OptimizedDecoderKernel * mode_opt_kernel[BC7PREP_MODE_COUNT];
	const U8 * cursor0[BC7PREP_MODE_COUNT];
	const U8 * cursor1[BC7PREP_MODE_COUNT];
	U32 advance0[BC7PREP_MODE_COUNT];
	U32 advance1[BC7PREP_MODE_COUNT];
	for (int i = 0; i < BC7PREP_MODE_COUNT; i++)
	{
		int which = (header->flags & BC7PREP_FLAG_SWITCH_COLORSPACE) ? 1 : 0;
		if (header->flags & (BC7PREP_FLAG_SPLIT0 << i))
		{
			which |= 2;

			SINTa split = bc7prep::decode_split_pos[i];

			cursor0[i] = bc7prep_data + mode_pos[i];
			cursor1[i] = cursor0[i] + split * header->mode_counts[i];
			advance0[i] = static_cast<U32>(split);
			advance1[i] = static_cast<U32>(bc7prep::mode_sizes[i] - split);
		}
		else
		{
			cursor0[i] = bc7prep_data + mode_pos[i];
			cursor1[i] = cursor0[i] + bc7prep::decode_split_pos[i];
			advance0[i] = advance1[i] = static_cast<U32>(bc7prep::mode_sizes[i]);
		}

		mode_kernel[i] = bc7prep::bc7_un_munge_kernel[i][which];
		mode_opt_kernel[i] = opt_kernels->kernel[i][which];
	}

	const bool decode_to_wc_mem = (flags & BC7PREP_DECODE_DESTINATION_IS_CACHED) == 0;

	// decode payloads
	SINTa cur_idx[BC7PREP_MODE_COUNT];
	U16 * mode_output_array[BC7PREP_MODE_COUNT];
	for (int i = 0; i < BC7PREP_MODE_COUNT; i++)
	{
		cur_idx[i] = mode_idx[i];
		mode_output_array[i] = output_index_arrays + i*adjusted_nblocks;
	}

	for (SINTa chunk_base = 0; chunk_base < num_blocks; chunk_base += BC7PREP_CHUNK_BLOCK_COUNT)
	{
		const SINTa blocks_in_chunk = RR_MIN(num_blocks - chunk_base, BC7PREP_CHUNK_BLOCK_COUNT);

		RR_COMPILER_ASSERT(BC7PREP_MODE_COUNT <= 16);
		U16 * mode_cur[16];

		// ---- sorting phase
		{
			SIMPLEPROFILE_SCOPE_N(bc7prep_sort, blocks_in_chunk);
			for (int i = 0; i < BC7PREP_MODE_COUNT; i++)
				mode_cur[i] = mode_output_array[i];

			// just make sure invalid modes write somewhere valid
			for (int i = BC7PREP_MODE_COUNT; i < 16; i++)
				mode_cur[i] = mode_output_array[0];

			bc7prep_sort(mode_cur, bc7prep_mode_nibbles, blocks_in_chunk, opt_kernels);

			// only partial block is the final one, so no problem here
			bc7prep_mode_nibbles += blocks_in_chunk >> 1;

			// if we had any illegal modes, bail
			for (int i = BC7PREP_MODE_COUNT; i < 16; i++)
			{
				if (mode_cur[i] != mode_output_array[0])
					return OodleTexRT_Err_BC7PrepPayloadCorrupt;
			}
		}

		// ---- work phase
		{
			SIMPLEPROFILE_SCOPE_N(bc7prep_work, blocks_in_chunk);

			U8 * dest_buf = decode_to_wc_mem ? chunk_buf : output_buf;

			for (int i = 0; i < BC7PREP_MODE_COUNT; i++)
			{
				const U16 * index_array = mode_output_array[i];
				U32 count = static_cast<U32>(mode_cur[i] - index_array);
				if (!count)
					continue;

				// check for validity: must stay within designated area for this mode
				cur_idx[i] += count;
				if (cur_idx[i] > mode_idx[i + 1]) // if we spill over into next mode's data, that's bad
					return OodleTexRT_Err_BC7PrepPayloadCorrupt;

				mode_kernel[i](dest_buf, cursor0[i], cursor1[i], count, index_array, mode_opt_kernel[i]);
				cursor0[i] += count * advance0[i];
				cursor1[i] += count * advance1[i];
			}
		}

		// copy from chunk buffer to output
		const SINTa bytes_in_chunk = blocks_in_chunk * 16;

		if (decode_to_wc_mem)
		{
			SIMPLEPROFILE_SCOPE_N(bc7prep_copy_out, blocks_in_chunk);
			memcpy(output_buf, chunk_buf, bytes_in_chunk);
		}
		output_buf += bytes_in_chunk;
	}

	// last check: make sure that we had the exact declared counts of every mode
	for (int i = 0; i < BC7PREP_MODE_COUNT; i++)
	{
		if (cur_idx[i] != mode_idx[i + 1])
			return OodleTexRT_Err_BC7PrepPayloadCorrupt;
	}

	return num_blocks * 16;
}

OODLE_NS_END

