// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "bc7prep.h"
#include "../texturert/bc7prep_format.h"
#include "bc67format.h"
#include "rrmem.h"
#include "histogram.h"
#include <math.h>

//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"

OODLE_NS_START

#define ALLOW_OLD_RATE_ESTIMATOR

// ---- General support code

template<typename T>
static void rr_swap(T &a, T &b)
{
	T tmp = a;
	a = b;
	b = tmp;
}

static U64 bit_extract(U64 val, U64 start, U64 width)
{
	if (!width)
		return 0;
	U64 mask = (1ull << (width & 63)) - 1;
	return (val >> start) & mask;
}

// Decorellated color space R,G,B -> (G, R-G, B-G)
// Used a YCoCg-like construction based on wrapping S-transform steps first,
// but this is simpler and about equally good.
static void RGB_to_decorr(int out[3], int r, int g, int b, int mask)
{
	out[0] = g;
	out[1] = (r - g) & mask;
	out[2] = (b - g) & mask;
}

static void RGB_to_decorr(int &r, int &g, int &b, int mask)
{
	int y = g;
	int cr = (r - y) & mask;
	int cb = (b - y) & mask;
	r = y;
	g = cr;
	b = cb;
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

static void RGB_to_decorr_packed(U64 &r, U64 &g, U64 &b, U64 msb_mask, U64 nonmsb_mask)
{
	U64 y = g;
	U64 cr = packed_sub(r, g, msb_mask, nonmsb_mask);
	U64 cb = packed_sub(b, g, msb_mask, nonmsb_mask);
	r = y;
	g = cr;
	b = cb;
}

static void RGB_to_decorr_packed(U64 &r, U64 &g, U64 &b, U64 msb_mask)
{
	U64 y = g;
	U64 cr = packed_sub(r, g, msb_mask);
	U64 cb = packed_sub(b, g, msb_mask);
	r = y;
	g = cr;
	b = cb;
}

static U64 mode5_stretch(U64 x)
{
	return ((x & 0x7f) << 1) | ((x & 0x3f80) << 2);
}

static U64 expand1to8_8x(U64 x)
{
	U64 xlo7 = x & 0x7f; // the low 7 bits, we can spread in a non-overlapping form using a multiply
	x = (xlo7 * 0x0002040810204081ull) | ((x & 0x80) << 49);
	x &= 0x0101010101010101ull;
	return x;
}

static U32 expand1to16_2x(U32 x)
{
	return (x & 1) | ((x & 2) << 15);
}

static U64 expand1to16_4x(U64 x)
{
	x &= 0xf;
	x *= 0x0000200040008001ull;
	x &= 0x0001000100010001ull;
	return x;
}

static U32 compact6to2_4x(U32 x)
{
	x &= 0xc30c3;
	x = ((x >> 4) | x) & 0xf00f;
	x = ((x >> 8) | x) & 0xff;
	return x;
}

static U64 expand6to16_4x(U64 x)
{
	//x &= 0xffffff; // assumed on input
	x = ((x << 20) | x) & 0x00000fff00000fffull;
	x = ((x << 10) | x) & 0x003f003f003f003full;
	return x;
}

static U32 expand5to16_2x(U32 x)
{
	return (x & 0x1f) | ((x & 0x3e0) << 11);
}

static U64 expand5to16_4x(U64 x)
{
	x &= 0xffffff;
	x = ((x << 22) | x) & 0x000003ff000003ffull;
	x = ((x << 11) | x) & 0x001f001f001f001full;
	return x;
}

static U64 expand7to24_2x(U64 x)
{
	return (x & 0x7f) | ((x & 0x3f80) << 17);
}

static U64 expand7to24_3x(U64 x)
{
	return (x & 0x7f) | ((x & 0x3f80) << 17) | ((x & 0x1fc000) << 34);
}

static U64 expand7to32_2x(U64 x)
{
	return (x & 0x7f) | ((x & 0x3f80) << 25);
}

static U64 expand8to32_2x(U64 x)
{
	return (x & 0xff) | ((x & 0xff00) << 24);
}

static U64 expand7to8_8x(U64 x)
{
	// move by 4
	const U64 stay4 = 0x000000000fffffffull;
	x = ((x << 4) & (stay4 << 32)) | (x & stay4);

	// move by 2
	const U64 stay2 = 0x00003fff00003fffull;
	x = ((x << 2) & (stay2 << 16)) | (x & stay2);

	// move by 1
	// this uses that (x<<1) == x+x
	// to simplify
	x += x & 0x3f803f803f803f80ull;

	return x;
}

// ---- Rewrite solid-color blocks into canonical form

static void bc7_encode_all_same_block(U8 *block, const U8 rgba[4])
{
	// Mode 5 can send every ARGB8888 color losslessly with a relatively
	// simple formula, so that's what we use.

	const U64 bit6_mask = 0x40 | (0x40 << 14) | (0x40ull << 28);
	const U64 lo7_mask = 0x7f | (0x7f << 14) | (0x7full << 28);
	U64 color_bits;

	// color channels
	// this is the bit-hack version written to process 3 color chans at once
	color_bits = rgba[0] | (rgba[1] << 14) | ((U64)rgba[2] << 28);

	U64 t = color_bits << 6;
	color_bits = ((color_bits >> 1) & lo7_mask) - (color_bits & ~lo7_mask);
	color_bits += t + (t & bit6_mask);

	// alpha: just set endpoint 0 to target value then use all-0 inds
	color_bits |= (U64)rgba[3] << 42;

	// insert mode bits
	color_bits = (color_bits << 8) | 0x20; // mode 5, rotation=0

	// store color_bits (LE)
	RR_PUT64_LE_UNALIGNED(block + 0, color_bits); // we've now stored 64 out of the 66 mode/color bits
	RR_PUT64_LE_UNALIGNED(block + 8, 0xaaaaaaac); // last 2 color bits, color index bits, alpha index bits
}

static bool bc7_block_is_solid(const void *block_bits, U8 *solid_color = 0)
{
	U8 decoded[16*4];
	bc7_decode_block(decoded, 4*4, block_bits);

	U32 color = RR_GET32_NATIVE_UNALIGNED(decoded);
	for (int i = 4; i < 64; i += 4)
		if (RR_GET32_NATIVE_UNALIGNED(decoded + i) != color)
			return false;

	if (solid_color)
		memcpy(solid_color, decoded, 4);
	return true;
}

static void bc7_canonicalize_solid(U8 *block_bits)
{
	U8 color[4];
	if (bc7_block_is_solid(block_bits, color))
		bc7_encode_all_same_block(block_bits, color);
}

// ---- Encoder support: byte vectors
// probably want to eventually mostly get rid of this

// This is a bump-pointer allocator for the backing store of ByteVecs
class ByteVecStorage
{
	U8 *buf;
	UINTa used;	// number of bytes already used
	UINTa cap;		// total capacity

	UINTa num_vecs;

	void dispose()
	{
		RR_ASSERT(num_vecs == 0);
		buf = 0;
		used = cap = 0;
	}

	// not implemented (intentionally)
	ByteVecStorage(const ByteVecStorage&);
	ByteVecStorage &operator =(const ByteVecStorage&);

	friend class ByteVec;

	U8 *vec_alloc(UINTa sz)
	{
		RR_ASSERT(sz <= cap - used);
		U8 *result = buf + used;
		used += sz;
		return result;
	}

public:
	explicit ByteVecStorage(U8 * buffer, UINTa size)
		: buf(buffer), used(0), cap(size), num_vecs(0)
	{
	}

	~ByteVecStorage()
	{
		dispose();
	}
};

class ByteVec
{
	ByteVecStorage *storage;
	U8 *buf;
	UINTa len;
	UINTa cap;

	// not implemented (intentionally)
	ByteVec(const ByteVec&);
	ByteVec &operator =(const ByteVec&);

public:
	ByteVec()
		: storage(0), buf(0), len(0), cap(0)
	{
	}

	~ByteVec()
	{
		if (storage)
			--storage->num_vecs;
	}

	const U8& operator[](UINTa ind) const
	{
		RR_ASSERT(ind < len);
		return buf[ind];
	}

	U8& operator[](UINTa ind)
	{
		RR_ASSERT(ind < len);
		return buf[ind];
	}

	UINTa size() const
	{
		return len;
	}

	UINTa capacity() const
	{
		return cap;
	}

	void clear()
	{
		len = 0;
	}

	void allocate(ByteVecStorage &target_storage, UINTa target_cap)
	{
		RR_ASSERT(storage == 0);

		storage = &target_storage;
		++storage->num_vecs;

		buf = storage->vec_alloc(target_cap);
		len = 0;
		cap = target_cap;
	}

	void resize(UINTa target_len)
	{
		RR_ASSERT(target_len <= cap);
		if (len < target_len)
			memset(buf + len, 0, target_len - len);
		len = target_len;
	}

	U8 *begin()						{ return buf; }
	const U8 *begin() const			{ return buf; }
	U8 *end()						{ return buf + len; }
	const U8 *end() const			{ return buf + len; }

	void push_back(U8 val)
	{
		RR_ASSERT(len < cap);
		buf[len++] = val;
	}

	void swap(ByteVec &x)
	{
		rr_swap(storage, x.storage);
		rr_swap(buf, x.buf);
		rr_swap(len, x.len);
		rr_swap(cap, x.cap);
	}
	
	void append(const U8 *begin, const U8 *end)
	{
		UINTa extra_bytes = end - begin;
		RR_ASSERT(cap - len >= extra_bytes);
		memcpy(buf + len, begin, extra_bytes);
		len += extra_bytes;
	}
};

static void append(ByteVec &dest, const U8 *begin, const U8 *end)
{
	dest.append(begin, end);
}

static void append(ByteVec &dest, const ByteVec &src)
{
	dest.append(src.begin(), src.end());
}

static void assign(ByteVec &dest, const ByteVec &src)
{
	dest.clear();
	append(dest, src);
}

template<size_t t_block_size, size_t t_first_half>
static void append_split_parts(ByteVec &result, const U8 *in, UINTa nblocks)
{
	UINTa base = result.size();
	result.resize(base + nblocks * t_block_size);

	UINTa second_half = t_block_size - t_first_half;

	for (UINTa i = 0; i < nblocks; i++)
	{
		memcpy(&result[base + i * t_first_half], &in[i * t_block_size + 0], t_first_half);
		memcpy(&result[base + i * second_half + nblocks*t_first_half], &in[i * t_block_size + t_first_half], second_half);
	}
}

// ---- bc7prep encoder

#define BC7_DEFAULT_LAMBDA	(0.01f)		// matching default Kraken lambda

// estimated cycles per block, for Lagrangian mode decisions
// NOTE these are relatively rough, haven't tried to measure these too carefully
// TODO these are stale, update
// (but also, I've yet to see the speed term actually make much of a difference in practice)
static const float bc7_mode_decode_cycles[10][4] =
{
	{ 18.6f, 16.6f, 17.7f, 14.9f }, // mode 0
	{ 15.9f, 16.2f, 13.6f, 14.7f }, // mode 1
	{ 26.0f, 28.9f, 25.3f, 30.2f }, // mode 2
	{ 17.6f, 19.1f, 14.6f, 17.7f }, // mode 3
	{ 15.5f, 23.0f, 12.7f, 21.8f }, // mode 4
	{ 13.3f, 16.7f, 10.7f, 13.2f }, // mode 5
	{  9.0f, 10.0f, 11.5f,  8.7f }, // mode 6
	{ 15.2f, 21.2f, 18.0f, 19.1f }, // mode 7
	{  6.2f,  6.2f,  6.2f,  6.2f }, // mode 8
	{  5.4f,  5.6f,  5.4f,  5.6f }, // mode 9
};

const size_t bc7_mode_sizes[] =
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

const size_t bc7_split_pos[] =
{
	BC7PREP_MODE0_SPLIT,
	BC7PREP_MODE1_SPLIT,
	BC7PREP_MODE2_SPLIT,
	BC7PREP_MODE3_SPLIT,
	BC7PREP_MODE4_SPLIT,
	BC7PREP_MODE5_SPLIT,
	BC7PREP_MODE6_SPLIT,
	BC7PREP_MODE7_SPLIT,
	BC7PREP_MODE8_SPLIT,
	BC7PREP_MODE9_SPLIT,
};

typedef void bc7_splitter_func(ByteVec &result, const U8 *in, UINTa nblocks);

static bc7_splitter_func * const bc7_split_funcs[] =
{
	append_split_parts<16, BC7PREP_MODE0_SPLIT>,
	append_split_parts<16, BC7PREP_MODE1_SPLIT>,
	append_split_parts<16, BC7PREP_MODE2_SPLIT>,
	append_split_parts<16, BC7PREP_MODE3_SPLIT>,
	append_split_parts<16, BC7PREP_MODE4_SPLIT>,
	append_split_parts<16, BC7PREP_MODE5_SPLIT>,
	append_split_parts<16, BC7PREP_MODE6_SPLIT>,
	append_split_parts<16, BC7PREP_MODE7_SPLIT>,
	NULL, // nothing to split
	NULL, // nothing to split
};

static void bc7_choose_modes(ByteVec &mode_bytes, UINTa *mode_counts, const U8 *bc7_bits, UINTa nblocks, bool canonicalize_solid)
{
	// This is the right place to switch blocks with rare modes to be sent raw (mode 8), if we ever do it.

	for (int i = 0; i < BC7PREP_MODE_COUNT; i++)
		mode_counts[i] = 0;

	mode_bytes.clear();

	for (UINTa block = 0; block < nblocks; block++)
	{
		const U8 *source_bits = bc7_bits + block*16;
		
		// check whether block is solid -> mode 9
		U8 solid_color[4];
		bool is_solid = bc7_block_is_solid(source_bits, solid_color);

		// work out which mode this block has
		int mode = 0;
		while (mode < 8 && (source_bits[0] & (1 << mode)) == 0)
			++mode;

		// solid-color blocks -> "mode 9"
		if (is_solid && canonicalize_solid)
			mode = 9;

		RR_ASSERT(mode < BC7PREP_MODE_COUNT);

		// track the result
		mode_bytes.push_back((U8)mode);
		mode_counts[mode]++;
	}

	// add a padding byte at the end if necessary to make mode_bytes count even
	if (nblocks & 1)
		mode_bytes.push_back(0);
}

static void bc7_munge_core(ByteVec &target, const ByteVec& modes, const UINTa *mode_byte_offs, const U8 *bc7_bits, UINTa nblocks, bool switch_colorspace)
{
	UINTa write_offs[BC7PREP_MODE_COUNT];
	for (int i = 0; i < BC7PREP_MODE_COUNT; i++)
		write_offs[i] = mode_byte_offs[i];

	for (UINTa block = 0; block < nblocks; block++)
	{
		int mode = modes[block];
		const U8 *source_bits = bc7_bits + block*16;
		U8 *block_bits = &target[write_offs[mode]];
		write_offs[mode] += bc7_mode_sizes[mode];

		switch (mode)
		{
		case 0:
			{
				//       [0]  mode (1b)
				//     [4:1]  partition (4b)
				//    [28:5]  R0..R5 (4b each)
				//   [52:29]  G0..G5 (4b each)
				//   [76:53]  B0..B5 (4b each)
				//   [82:77]  P0..P5 (1b each)
				//  [127:83]  index (45b)

				U64 lo = RR_GET64_LE_UNALIGNED(source_bits + 0);
				U64 hi = RR_GET64_LE_UNALIGNED(source_bits + 8);

				U64 partbits = bit_extract(lo, 1, 4);
				U64 rbits = bit_extract(lo, 5, 24);
				U64 gbits = bit_extract(lo, 29, 24);
				U64 bbits = bit_extract(lo, 53, 11) | (bit_extract(hi, 0, 13) << 11);

				if (switch_colorspace)
					RGB_to_decorr_packed(rbits, gbits, bbits, 0x888888);

				// to:
				//   [23:0]  R'0..R'5 (4b each)
				//  [47:24]  G'0..G'5 (4b each)
				//  [71:48]  B'0..B'5 (4b each)
				//  [75:72]  partition (4b)
				//     [76]  unused (alawys 0)
				//  [82:77]  P0..P5 (1b each)
				// [127:83]  index (45b)

				U64 new_lo = rbits | (gbits << 24) | (bbits << 48);
				// NOTE: 1-bit gap between end of partbits and index bits
				U64 new_hi = (bbits >> 16) | (partbits << 8) | (hi & ~0x1fffull);

				RR_PUT64_LE_UNALIGNED(block_bits + 0, new_lo);
				RR_PUT64_LE_UNALIGNED(block_bits + 8, new_hi);
			}
			break;

		case 1:
			{
				//     [1:0]  mode (2b)
				//     [7:2]  partition (6b)
				//    [31:8]  R0..R3 (6b each)
				//   [55:32]  G0..G3 (6b each)
				//   [79:56]  B0..B3 (6b each)
				//   [81:80]  P0..P1 (1b each)
				//  [127:82]  index (46b)

				// store RGB655, low bits of g/b in extra byte
				// best yet
				U64 lo = RR_GET64_LE_UNALIGNED(source_bits + 0);
				U64 hi = RR_GET64_LE_UNALIGNED(source_bits + 8);

				U64 partbits = bit_extract(lo, 2, 6);
				U64 rbits = bit_extract(lo, 8, 24);
				U64 gbits = bit_extract(lo, 32, 24);
				U64 bbits = bit_extract(lo, 56, 8) | (bit_extract(hi, 0, 16) << 8);

				// NOTE: not seeing a win from leaving non-switched colorspace data interleaved
				if (switch_colorspace)
					RGB_to_decorr_packed(rbits, gbits, bbits, 0x820820);

				// to:
				//   [63:0]  R'G'B'0..R'G'B'3 (16b each, RGB655 per endpoint)
				//  [71:64]  G'lB'l0..G'lB'l3 (2b each, LSBs of G'/B' from endpoints)
				//  [77:72]  partition (6b)
				//  [79:78]  unused (always 0)
				//  [81:80]  P0..P1 (1b each)
				// [127:82]  index (46b)

				// place the R/G/B bits where they need to go
				U64 new_lo;
				new_lo  = expand6to16_4x(rbits);
				new_lo |= expand6to16_4x(gbits & ~0x41041) << 5;  // clear low-order bits of g
				new_lo |= expand6to16_4x(bbits & ~0x41041) << 10; // clear low-order bits of b

				// grab the low bits of g/b and pack them down
				U64 new_hi;
				new_hi = compact6to2_4x( (U32)((gbits & 0x41041) | ((bbits & 0x41041) << 1)) );

				// NOTE: 2 bits gap before pbits+index (bits 14 and 15)
				new_hi |= (partbits << 8) | (hi & ~0xffffull);

				RR_PUT64_LE_UNALIGNED(block_bits + 0, new_lo);
				RR_PUT64_LE_UNALIGNED(block_bits + 8, new_hi);
			}
			break;

		case 2:
			{
				//     [2:0]  mode (3b)
				//     [8:3]  partition (6b)
				//    [38:9]  R0..R5 (5b each)
				//   [68:39]  G0..G5 (5b each)
				//   [98:69]  B0..B5 (5b each)
				//  [127:99]  index (29b)
				U64 lo = RR_GET64_LE_UNALIGNED(source_bits + 0);
				U64 hi = RR_GET64_LE_UNALIGNED(source_bits + 8);

				U64 partbits = bit_extract(lo, 3, 6);
				U64 rbits = bit_extract(lo, 9, 30);
				U64 gbits = bit_extract(lo, 39, 25) | (bit_extract(hi, 0, 5) << 25);
				U64 bbits = bit_extract(hi, 5, 30);
				U64 indbits = bit_extract(hi, 35, 29);

				if (switch_colorspace)
					RGB_to_decorr_packed(rbits, gbits, bbits, 0x21084210);

				// to:
				//   [95:0]  pR'G'B'0..pR'G'B'5 (16b each, 1 bit from partition index then R'G'B'555)
				//  [98:96]  unused (always 0)
				// [127:99]  index (29b)

				// create 16-bit packets of partition index bit then RGB555 (P1R5G5B5)
				U64 new_lo;
				new_lo  = expand1to16_4x(partbits);
				new_lo |= expand5to16_4x(rbits) << 1;
				new_lo |= expand5to16_4x(gbits) << 6;
				new_lo |= expand5to16_4x(bbits) << 11;

				U64 new_hi;
				new_hi  = expand1to16_2x((U32) (partbits >> 4));
				new_hi |= expand5to16_2x((U32) (rbits >> 20)) << 1;
				new_hi |= expand5to16_2x((U32) (gbits >> 20)) << 6;
				new_hi |= expand5to16_2x((U32) (bbits >> 20)) << 11;

				new_hi |= indbits << 35;

				RR_PUT64_LE_UNALIGNED(block_bits + 0, new_lo);
				RR_PUT64_LE_UNALIGNED(block_bits + 8, new_hi);
			}
			break;

		case 3:
			{
				//     [3:0]  mode (4b)
				//     [9:4]  partition (6b)
				//   [37:10]  R0..R3 (7b each)
				//   [65:38]  G0..G3 (7b each)
				//   [93:66]  B0..B3 (7b each)
				//   [97:94]  P0..P3 (1b each)
				//  [127:98]  index (30b)

				// try to store RGB each byte-aligned and interleaved
				// to achieve this, put the P0..P3 bits into the low bits
				// of the R0..R3 bytes,
				// and the partition index bits into the low bits of
				// G0..G3 and B0..B1.
				U64 lo = RR_GET64_LE_UNALIGNED(source_bits + 0);
				U64 hi = RR_GET64_LE_UNALIGNED(source_bits + 8);

				U64 new_lo = 0;
				U64 new_hi = 0;

				U64 partbits = bit_extract(lo, 4, 6);
				U64 rbits = bit_extract(lo, 10, 28);
				U64 gbits = bit_extract(lo, 38, 26) | ((hi & 3) << 26);
				U64 bbits = bit_extract(hi, 2, 28);
				U64 pbits_lo = bit_extract(hi, 30, 2);
				U64 pbits_hi_index = bit_extract(hi, 32, 32);

				if (switch_colorspace)
					RGB_to_decorr_packed(rbits, gbits, bbits, 0x8102040);

				// to:
				//   [95:0]  R'pG'pB'p0...R'pG'pB'p3 (24b each)
				//  [97:96]  P2..P3 (1b each)
				// [127:98]  index (30b)
				//
				// where p = first partition bits (6) then P0..P1
				// we send 12 p-bits, of which 8 are assigned, so the last 4 are always 0.

				// bytes: r0,g0,b0,r1, g1,b1,r2,g2, b2,r3,g3,b3
				new_lo  = expand1to8_8x(partbits | (pbits_lo << 6)) << 7; // partition bits then pbits_lo go into byte MSBs
				new_lo |= expand7to24_3x(rbits) << 0;
				new_lo |= expand7to24_3x(gbits) << 8;
				new_lo |= expand7to24_2x(bbits) << 16;

				new_hi  = pbits_hi_index << 32; // high partition bits and index go into high half
				new_hi |= expand7to24_2x(bbits >> 14) << 0;
				new_hi |= (rbits >> 21) << 8;
				new_hi |= (gbits >> 21) << 16;

				RR_PUT64_LE_UNALIGNED(block_bits + 0, new_lo);
				RR_PUT64_LE_UNALIGNED(block_bits + 8, new_hi);
			}
			break;

		case 4:
			{
				//     [4:0]  mode (5b)
				//     [6:5]  channel rot (2b)
				//     [7:7]  idxMode
				//    [17:8]  R0..R1 (5b each)
				//   [27:18]  G0..G1 (5b each)
				//   [37:28]  B0..B1 (5b each)
				//   [49:38]  A0..A1 (6b each)
				//   [80:50]  index0 (31b)
				//  [127:81]  index1 (47b)
				U64 lo = RR_GET64_LE_UNALIGNED(source_bits + 0);
				U64 hi = RR_GET64_LE_UNALIGNED(source_bits + 8);

				U64 crot = bit_extract(lo, 5, 2);

				U64 cbits[4];
				cbits[0] = bit_extract(lo, 8, 10);
				cbits[1] = bit_extract(lo, 18, 10);
				cbits[2] = bit_extract(lo, 28, 10);
				cbits[3] = bit_extract(lo, 39, 5) | (bit_extract(lo, 45, 5) << 5);

				// undo channel rotate
				rr_swap(cbits[(crot - 1) & 3], cbits[3]);

				if (switch_colorspace)
					RGB_to_decorr_packed(cbits[0], cbits[1], cbits[2], 0x210);

				// to:
				//    [1:0]  A0..A1 LSBs (1b each)
				//   [11:2]  R'0..R'1 (5b each)
				//  [21:12]  G'0..G'1 (5b each)
				//  [31:22]  B'0..B'1 (5b each)
				//  [41:32]  A'0..A'1 (5b each)
				//     [42]  idxMode
				//  [43:47]  unused (always 0)
				//  [49:48]  channel rot (2b)
				//  [80:50]  index0 (31b)
				// [127:81]  index1 (47b)
				//
				// R',G',B',A' have components swapped to undo the channel rotate

				// no deinterleave, just pass through endpoints regularly
				U64 new_lo;
				new_lo  = bit_extract(lo, 38, 1); // low bit of A0
				new_lo |= bit_extract(lo, 44, 1) << 1; // low bit of A1
				new_lo |= cbits[0] << 2;
				new_lo |= cbits[1] << 12;
				new_lo |= cbits[2] << 22;
				new_lo |= cbits[3] << 32;
				new_lo |= bit_extract(lo, 7, 1) << 42; // idxMode
				// NOTE: 5 unused bits here
				new_lo |= crot << 48;
				new_lo |= bit_extract(lo, 50, 14) << 50; // pass through index0...

				U64 new_hi;
				new_hi = hi; // ...and index1 unmolested

				RR_PUT64_LE_UNALIGNED(block_bits + 0, new_lo);
				RR_PUT64_LE_UNALIGNED(block_bits + 8, new_hi);
			}
			break;

		case 5:
			{
				//     [5:0]  mode (6b)
				//     [7:6]  channel rot (2b)
				//    [21:8]  R0..R1 (7b each)
				//   [35:22]  G0..G1 (7b each)
				//   [49:36]  B0..B1 (7b each)
				//   [65:50]  A0..A1 (8b each)
				//   [96:66]  index0 (31b)
				//  [127:97]  index1 (31b)
				U64 lo = RR_GET64_LE_UNALIGNED(source_bits + 0);
				U64 hi = RR_GET64_LE_UNALIGNED(source_bits + 8);

				U64 crot = bit_extract(lo, 6, 2);
				U64 cbits[4];
				U64 indices = bit_extract(hi, 2, 62);

				cbits[0] = mode5_stretch(bit_extract(lo, 8, 14));
				cbits[1] = mode5_stretch(bit_extract(lo, 22, 14));
				cbits[2] = mode5_stretch(bit_extract(lo, 36, 14));
				cbits[3] = bit_extract(lo, 50, 14) | (bit_extract(hi, 0, 2) << 14);

				// undo channel rotate
				rr_swap(cbits[(crot - 1) & 3], cbits[3]);

				U64 new_lo;

				if (switch_colorspace)
				{
					// do color transform on the top 7 bits (keeping low bit)
					U64 lo0 = cbits[0] & 0x0101; cbits[0] &= 0xfefe;
					U64 lo1 = cbits[1] & 0x0101; cbits[1] &= 0xfefe;
					U64 lo2 = cbits[2] & 0x0101; cbits[2] &= 0xfefe;
					RGB_to_decorr_packed(cbits[0], cbits[1], cbits[2], 0x8080, 0x7e7e);
					cbits[0] |= lo0;
					cbits[1] |= lo1;
					cbits[2] |= lo2;

					// to:
					//   [63:0]  R'G'B'A'0..R'G'B'A'1 (32b each)
					//  [65:64]  channel rot (2b)
					//  [96:66]  index0 (31b)
					// [127:97]  index1 (31b)
					//
					// R', G', B', A' are all 8 bits/value with channel order
					// swapped back to undo channel rotation (but the components
					// that weren't A originally have their LSBs 0).
					new_lo  = expand8to32_2x(cbits[0]);
					new_lo |= expand8to32_2x(cbits[1]) << 8;
					new_lo |= expand8to32_2x(cbits[2]) << 16;
					new_lo |= expand8to32_2x(cbits[3]) << 24;
				}
				else
				{
					new_lo  = cbits[0];
					new_lo |= cbits[1] << 16;
					new_lo |= cbits[2] << 32;
					new_lo |= cbits[3] << 48;
				}

				RR_PUT64_LE_UNALIGNED(block_bits + 0, new_lo);
				RR_PUT64_LE_UNALIGNED(block_bits + 8, crot | (indices << 2));
			}
			break;

		case 6:
			{
				//     [6:0]  mode (7b)
				//    [20:7]  R0..R1 (7b each)
				//   [34:21]  G0..G1 (7b each)
				//   [48:35]  B0..B1 (7b each)
				//   [62:49]  A0..A1 (7b each)
				//   [64:63]  P0..P1 (1b each)
				//  [127:65]  index (63b)
				U64 lo = RR_GET64_LE_UNALIGNED(source_bits + 0);
				U64 hi = RR_GET64_LE_UNALIGNED(source_bits + 8);

				U64 new_lo = lo & (1ull << 63); // pass through P0 bit

				if (switch_colorspace)
				{
					U64 rbits = bit_extract(lo, 7, 14);
					U64 gbits = bit_extract(lo, 21, 14);
					U64 bbits = bit_extract(lo, 35, 14);
					U64 abits = bit_extract(lo, 49, 14);

					RGB_to_decorr_packed(rbits, gbits, bbits, 0x2040);

					// to:
					//   [0:63]  R'pG'pB'pA'p0..R'pG'pB'pA'p1 (32b each)
					//  [64:63]  P0..P1 (1b each)
					// [127:65]  index (63b)
					//
					// all padding bits between R', G', B', A's are 0
					// the "padding" bit after the final value (A'1)
					// keeps P0 (in the location where it was originally)
					//
					// NOTE: high bits of all endpoints except last unused
					new_lo |= expand7to32_2x(rbits) << 0;
					new_lo |= expand7to32_2x(gbits) << 8;
					new_lo |= expand7to32_2x(bbits) << 16;
					new_lo |= expand7to32_2x(abits) << 24;
				}
				else
				{
					U64 endpoints = bit_extract(lo, 7, 14*4);

					new_lo |= expand7to8_8x(endpoints);
				}

				U64 new_hi = hi; // pass through P1 bit and indices

				RR_PUT64_LE_UNALIGNED(block_bits + 0, new_lo);
				RR_PUT64_LE_UNALIGNED(block_bits + 8, new_hi);
			}
			break;

		case 7:
			{
				//     [7:0]  mode (8b)
				//    [13:8]  partition (6b)
				//   [33:14]  R0..R3 (5b each)
				//   [53:34]  G0..G3 (5b each)
				//   [73:54]  B0..B3 (5b each)
				//   [93:74]  A0..A3 (5b each)
				//   [97:94]  P0..P3 (1b each)
				//  [127:98]  index (30b)
				U64 lo = RR_GET64_LE_UNALIGNED(source_bits + 0);
				U64 hi = RR_GET64_LE_UNALIGNED(source_bits + 8);

				U64 partbits = bit_extract(lo, 8, 6);
				U64 rbits = bit_extract(lo, 14, 20);
				U64 gbits = bit_extract(lo, 34, 20);
				U64 bbits = bit_extract(lo, 54, 10) | (bit_extract(hi, 0, 10) << 10);
				U64 abits = bit_extract(hi, 10, 20);
				U64 pbits = bit_extract(hi, 30, 4);
				U64 index = bit_extract(hi, 34, 30);

				U64 new_lo, new_hi;

				if (switch_colorspace)
				{
					RGB_to_decorr_packed(rbits, gbits, bbits, 0x84210);

					// to:
					//   [63:0]  PR'G'B'0..PR'G'B'3 (16b each)
					//  [71:64]  unused (always 0)
					//  [91:72]  A0..A3 (5b each)
					//  [97:92]  partition (6b)
					// [127:98]  index (30b)
					//
					// P-bit, then R', G', B' for each endpoint, to form 16b groups.

					// Also tried high nibbles separated from low bits, both interleaved
					// and deinterleaved. This had better ratios.

					// interleaved P1R5G5B5 tuples, alpha separate
					new_lo =  expand1to16_4x(pbits);
					new_lo |= expand5to16_4x(rbits) << 1;
					new_lo |= expand5to16_4x(gbits) << 6;
					new_lo |= expand5to16_4x(bbits) << 11;

					// leave the low byte 0;
					// put partbits with (random-ish) index
					new_hi = (abits << 8) | (partbits << 28) | (index << 34);
				}
				else
				{
					new_lo = 0;
					new_hi = 0;

					for (int i = 0; i < 4; i++)
					{
						new_lo |= bit_extract(rbits, i*5+1, 4) << (i*4);
						new_lo |= bit_extract(gbits, i*5+1, 4) << (i*4 + 16);
						new_lo |= bit_extract(bbits, i*5+1, 4) << (i*4 + 32);
						new_lo |= bit_extract(abits, i*5+1, 4) << (i*4 + 48);

						new_hi |= bit_extract(rbits, i*5, 1) << (i + 12);
						new_hi |= bit_extract(gbits, i*5, 1) << (i + 16);
						new_hi |= bit_extract(bbits, i*5, 1) << (i + 20);
						new_hi |= bit_extract(abits, i*5, 1) << (i + 24);
					}

					// leave the low byte 0; the rest is high entropy so group it
					// with the partbits and index
					new_hi |= (pbits << 8) | (partbits << 28) | (index << 34);
				}

				RR_PUT64_LE_UNALIGNED(block_bits + 0, new_lo);
				RR_PUT64_LE_UNALIGNED(block_bits + 8, new_hi);
			}
			break;

		case 8:
			memcpy(block_bits, source_bits, 16);
			break;

		case 9: // solid color
			{
				U8 solid_color[4];
				bool is_solid = bc7_block_is_solid(source_bits, solid_color);
				RR_UNUSED_VARIABLE(is_solid); // avoid warning in non-debug builds
				RR_ASSERT(is_solid);

				int r = solid_color[0];
				int g = solid_color[1];
				int b = solid_color[2];

				if (switch_colorspace)
					RGB_to_decorr(r, g, b, 255);

				block_bits[0] = (U8)r;
				block_bits[1] = (U8)g;
				block_bits[2] = (U8)b;
				block_bits[3] = solid_color[3];
			}
			break;

		default:
			RR_ASSERT_FAILURE_ALWAYS("no default");
		}
	}
}

struct Bc7Results
{
	ByteVec mode_bytes; // selected modes go here

	ByteVec best_split;
	ByteVec post_twiddle; // after bit-twiddling done, but before splits
	ByteVec scratch;
	UINTa num_blocks;

	void alloc(ByteVecStorage &storage, UINTa nblocks);
	void modes_selected(const UINTa *mode_counts, UINTa *mode_byte_offs);
};

void Bc7Results::alloc(ByteVecStorage &storage, UINTa nblocks)
{
	// per block: mode nibble, (max) 16 bytes payload
	// but we also keep the mode _bytes_ around
	num_blocks = nblocks;

	// mode bytes gets padded to an even count since we pack nibbles
	mode_bytes.allocate(storage, (num_blocks + 1) & ~1);

	// alloc payload buffers
	UINTa mode_data_size = num_blocks * 16;
	best_split.allocate(storage, mode_data_size);
	post_twiddle.allocate(storage, mode_data_size);
	scratch.allocate(storage, mode_data_size);
}

// assumes scratch contains mode bytes
// determines actual payload size, allocates buffer space,
// and copies the mode bytes to all the output buffers
void Bc7Results::modes_selected(const UINTa *mode_counts, UINTa *mode_byte_offs)
{
	// lay out where the mode sub-arrays will start
	UINTa result_size = 0;
	for (int i = 0; i < BC7PREP_MODE_COUNT; i++)
	{
		mode_byte_offs[i] = result_size;
		result_size += mode_counts[i] * bc7_mode_sizes[i];
	}

	// init all the buffers to empty
	best_split.clear();
	post_twiddle.clear();
	scratch.clear();

	// grow all buffers to the target size
	best_split.resize(result_size);
	post_twiddle.resize(result_size);
	scratch.resize(result_size);
}

static RADFORCEINLINE U32 hash4(const U8 * ptr)
{
	// can use 32-bit mul
	U32 x = RR_GET32_LE_UNALIGNED(ptr) * 0x9E3779B1U;
	return x;
}

static SINTa bc7_pseudolz_estimate_nmatches(const void * bytes_void, SINTa size)
{
	const U8 * bytes = (const U8 *)bytes_void;
	const U32 kHashBits = 13;
	const U32 kHashSize = 1u << kHashBits;

	U8 hash_table[kHashSize]; // 8k on stack, should be OK
	memset(hash_table,0,sizeof(hash_table));

	SINTa nmatch = 0;
	SINTa last = size - 7; // room for U32 grab with 3 bytes offset

	// de-serialized hash lookup for speed
	for (SINTa i = 0; i < last; i += 4)
	{
		U32 h0 = hash4(bytes+i+0);
		U32 h1 = hash4(bytes+i+1);
		U32 h2 = hash4(bytes+i+2);
		U32 h3 = hash4(bytes+i+3);

		U32 ind0 = h0 >> (32 - kHashBits);
		U32 ind1 = h1 >> (32 - kHashBits);
		U32 ind2 = h2 >> (32 - kHashBits);
		U32 ind3 = h3 >> (32 - kHashBits);

		U8 id0 = static_cast<U8>(h0 >> (24 - kHashBits));
		U8 id1 = static_cast<U8>(h1 >> (24 - kHashBits));
		U8 id2 = static_cast<U8>(h2 >> (24 - kHashBits));
		U8 id3 = static_cast<U8>(h3 >> (24 - kHashBits));

		nmatch += ( hash_table[ind0] == id0 );
		nmatch += ( hash_table[ind1] == id1 );
		nmatch += ( hash_table[ind2] == id2 );
		nmatch += ( hash_table[ind3] == id3 );

		hash_table[ind0] = id0;
		hash_table[ind1] = id1;
		hash_table[ind2] = id2;
		hash_table[ind3] = id3;
	}

	return nmatch;
}

static SINTa bc7_combined_rate_estimate(const void * bytes_void, SINTa size)
{
	// Try simple 0th order entropy
	const U8 * bytes = (const U8 *)bytes_void;
	S64 len_in_bits = CodeLenOfArrayU8(bytes,size);
	F64 bpb = (F64)len_in_bits / (F64)size;

	// estimate # of matched bytes and very roughly adjust for them
	// by subtracting an equivalent number of bytes out of the literals,
	// with average bpb
	// (this is very crude)
	SINTa nmatched = bc7_pseudolz_estimate_nmatches(bytes_void, size);
	F64 expected_bits = (size - nmatched) * bpb;

	return (SINTa) ceil(expected_bits / 8.0);
}

#ifdef ALLOW_OLD_RATE_ESTIMATOR

typedef SINTa BC7RateEstimateFunc(const void * bytes, SINTa size);
static BC7RateEstimateFunc * bc7_rate_estimate = NULL;

// not actually a publicly declared API func
OOFUNC1 void OOFUNC2 OodleTex_BC7Prep_SetRateEstimator(BC7RateEstimateFunc * estimator)
{
	bc7_rate_estimate = estimator;
}

static double bc7_decide_splits_old(Bc7Results &res, const UINTa *mode_counts, const UINTa *mode_byte_offs, U32 *out_split_modes, int colorspace, const BC7PrepSettings &settings)
{
	double best_J = 1e+100;
	float lambda = settings.lambda;
	*out_split_modes = 0;

	if (!settings.exhaustive_splits)
	{
		res.scratch.clear();
		double sum_cycles = 0.0;

		// quick mode treats it as separable
		for (int i = 0; i < BC7PREP_MODE_COUNT; i++)
		{
			UINTa block_count = mode_counts[i];
			if (block_count == 0)
				continue;

			if (bc7_mode_sizes[i] == 16 && bc7_split_pos[i] != 0)
			{
				// append the split parts initially, assuming we'll take the split
				UINTa original_size = res.scratch.size();

				bc7_split_funcs[i](res.scratch, &res.post_twiddle[mode_byte_offs[i]], block_count);

				UINTa size_uncomp = block_count * bc7_mode_sizes[i];
				UINTa size_nonsplit = bc7_rate_estimate(&res.post_twiddle[mode_byte_offs[i]], size_uncomp);
				UINTa size_split = bc7_rate_estimate(&res.scratch[original_size], size_uncomp);

				double cycles_nonsplit = bc7_mode_decode_cycles[i][colorspace + 0] * (double)block_count;
				double cycles_split    = bc7_mode_decode_cycles[i][colorspace + 2] * (double)block_count;

				double J_nonsplit = size_nonsplit + lambda * cycles_nonsplit;
				double J_split    = size_split    + lambda * cycles_split;

				bool use_split = J_split < J_nonsplit;
				if (use_split)
				{
					// great, the data's already in place
					sum_cycles += cycles_split;
					*out_split_modes |= BC7PREP_FLAG_SPLIT0 << i;
				}
				else
				{
					// need to reset to the state before and re-add the non-split bytes
					sum_cycles += cycles_nonsplit;
					res.scratch.resize(original_size);
					append(res.scratch, res.post_twiddle.begin() + mode_byte_offs[i], res.post_twiddle.begin() + mode_byte_offs[i] + size_uncomp);
				}
			}
			else
			{
				sum_cycles += bc7_mode_decode_cycles[i][colorspace + 0] * (double)block_count;

				append(res.scratch, res.post_twiddle.begin() + mode_byte_offs[i], res.post_twiddle.begin() + mode_byte_offs[i] + block_count * bc7_mode_sizes[i]);
			}
		}

		res.best_split.swap(res.scratch);
		UINTa size_total = bc7_rate_estimate(&res.best_split[0], res.best_split.size());
		best_J = size_total + lambda*sum_cycles;
	}
	else
	{
		// ---- Brute force: try all combinations of split-or-not for all modes used

		// Prepare a bit mask of which modes are actually used
		RR_COMPILER_ASSERT(BC7PREP_FLAG_SPLIT0 == 1); // we assume the split flags are consecutive from bit 0
		U32 mode_mask = 0;

		for (int i = 0; i < 8; i++) // only the first 8 modes have meaningful splits
		{
			if (mode_counts[i] != 0)
				mode_mask |= 1u << i;
		}

		U32 cur_split_mask = 0;
		do
		{
			res.scratch.clear();

			// append the remaining mode bytes
			double decode_cycles = 0.0;
			for (int i = 0; i < BC7PREP_MODE_COUNT; i++)
			{
				UINTa block_count = mode_counts[i];
				if (block_count == 0)
					continue;

				const U8 *src_bytes = &res.post_twiddle[mode_byte_offs[i]];

				if (cur_split_mask & (1u << i))
				{
					decode_cycles += bc7_mode_decode_cycles[i][colorspace + 2] * (double)block_count;
					bc7_split_funcs[i](res.scratch, src_bytes, block_count);
				}
				else
				{
					decode_cycles += bc7_mode_decode_cycles[i][colorspace + 0] * (double)block_count;
					append(res.scratch, src_bytes, src_bytes + block_count*bc7_mode_sizes[i]);
				}
			}

			// determine packed size
			UINTa comp_size = bc7_rate_estimate(&res.scratch[0], res.scratch.size());
			double J = comp_size + lambda * decode_cycles;

			if (J < best_J)
			{
				best_J = J;
				res.best_split.swap(res.scratch);
				*out_split_modes = cur_split_mask;
			}

			// advance to the next split
			// this counts through the bits set in mode_mask while leaving the rest alone
			cur_split_mask = (cur_split_mask - mode_mask) & mode_mask;
		} while (cur_split_mask != 0);
	}

	return best_J;
}

#endif // ALLOW_OLD_RATE_ESTIMATOR

// fillls out res.best_split, returns J
static double bc7_decide_splits(Bc7Results &res, const UINTa *mode_counts, const UINTa *mode_byte_offs, U32 *out_split_modes, int colorspace, const BC7PrepSettings &settings)
{
#ifdef ALLOW_OLD_RATE_ESTIMATOR
	// old estimate is supported for testing vs kraken
	if (bc7_rate_estimate != NULL)
		return bc7_decide_splits_old(res, mode_counts, mode_byte_offs, out_split_modes, colorspace, settings);
#endif

	float lambda = settings.lambda;
	*out_split_modes = 0;

	res.scratch.clear();
	double sum_cycles = 0.0;

	// quick mode treats it as separable
	for (int i = 0; i < BC7PREP_MODE_COUNT; i++)
	{
		UINTa block_count = mode_counts[i];
		if (block_count == 0)
			continue;

		if (bc7_mode_sizes[i] == 16 && bc7_split_pos[i] != 0)
		{
			// append the split parts initially, assuming we'll take the split
			UINTa original_size = res.scratch.size();

			bc7_split_funcs[i](res.scratch, &res.post_twiddle[mode_byte_offs[i]], block_count);

			double cycles_nonsplit = bc7_mode_decode_cycles[i][colorspace + 0] * (double)block_count;
			double cycles_split    = bc7_mode_decode_cycles[i][colorspace + 2] * (double)block_count;

			UINTa size_uncomp = block_count * bc7_mode_sizes[i];
			SINTa size0 = block_count * bc7_split_pos[i];
			SINTa size1 = size_uncomp - size0;

			// rough estimate of # matched bytes in either half
			// tif we have match-heavy on one side and few matches on the other, we want to split,
			// but if they're both similar, keep them together
			//
			// same with o0 entropy. If either of those is very different, we'd rather split.
			SINTa nmatch0 = bc7_pseudolz_estimate_nmatches(&res.scratch[original_size], size0);
			SINTa nmatch1 = bc7_pseudolz_estimate_nmatches(&res.scratch[original_size + size0], size1);

			double ratio0 = (double)nmatch0 / (double)size0;
			double ratio1 = (double)nmatch1 / (double)size1;
			double entro0 = (double)CodeLenOfArrayU8(&res.scratch[original_size], size0) / (double)size0;
			double entro1 = (double)CodeLenOfArrayU8(&res.scratch[original_size + size0], size1) / (double)size1;

			const double kMatchRatio = 2.0;
			const double kEntroDiff = 1.0;
			bool use_split = (ratio1 > ratio0 * kMatchRatio || ratio0 > ratio1 * kMatchRatio) || // significant difference in prevalence of matches on either side
				fabs(entro0 - entro1) > kEntroDiff; // or significant difference in entropy
			if (use_split)
			{
				// great, the data's already in place
				sum_cycles += cycles_split;
				*out_split_modes |= BC7PREP_FLAG_SPLIT0 << i;
			}
			else
			{
				// need to reset to the state before and re-add the non-split bytes
				sum_cycles += cycles_nonsplit;
				res.scratch.resize(original_size);
				append(res.scratch, res.post_twiddle.begin() + mode_byte_offs[i], res.post_twiddle.begin() + mode_byte_offs[i] + size_uncomp);
			}
		}
		else
		{
			sum_cycles += bc7_mode_decode_cycles[i][colorspace + 0] * (double)block_count;

			append(res.scratch, res.post_twiddle.begin() + mode_byte_offs[i], res.post_twiddle.begin() + mode_byte_offs[i] + block_count * bc7_mode_sizes[i]);
		}
	}

	res.best_split.swap(res.scratch);
	SINTa size_total = bc7_combined_rate_estimate(&res.best_split[0], res.best_split.size());
	return size_total + lambda*sum_cycles;
}

SINTa bc7prep_min_output_size(SINTa nblocks)
{
	// mode bytes are packed into nibbles
	SINTa modes_packed_size = (nblocks + 1) / 2;

	return 16 * nblocks /* payload */ + modes_packed_size;
}

SINTa bc7prep_min_scratch_size(SINTa nblocks)
{
	// add a bit of extra space in case we later want to align things
	SINTa nblocks_aligned = (nblocks + 15) & ~15;

	return nblocks_aligned /* mode bytes */ + 3*16*nblocks /* candidate payload blocks */;
}

SINTa bc7prep_encode(OodleTexRT_BC7PrepHeader * out_header,
	U8 * output_buf, SINTa output_size,
	const U8 * bc7_bits, SINTa nblocks,
	U8 * scratch_buf, SINTa scratch_size,
	const BC7PrepSettings &settings)
{
	// Parameter validation
	if (!out_header)
		return OodleTex_Err_BC7PrepNoHeader;

	if (nblocks < 0 || nblocks > BC7PREP_MAXBLOCKS)
		return OodleTex_Err_BC7PrepIllegalBlockCount;

	if (output_size < bc7prep_min_output_size(nblocks))
		return OodleTex_Err_BC7PrepOutputBufTooSmall;

	if (scratch_size < bc7prep_min_scratch_size(nblocks))
		return OodleTex_Err_BC7PrepScratchBufTooSmall;

	ByteVecStorage output_storage(output_buf, output_size);
	ByteVecStorage scratch_storage(scratch_buf, scratch_size);

	Bc7Results res;
	double best_J = 1e+100;
	UINTa mode_byte_offs[BC7PREP_MODE_COUNT];
	UINTa mode_counts[BC7PREP_MODE_COUNT];

	ByteVec result;
	result.allocate(output_storage, output_size);

	// Figure out modes for every block
	// mostly just extracting the mode field, except for solid blocks.
	res.alloc(scratch_storage, nblocks);
	bc7_choose_modes(res.mode_bytes, mode_counts, bc7_bits, nblocks, settings.canonicalize_solid != 0);
	res.modes_selected(mode_counts, mode_byte_offs);

	// initialize the result
	RR_DURING_ASSERT(SINTa packed_modes_size = (nblocks + 1) / 2); // packed into nibbles at the end, see below
	RR_ASSERT(result.capacity() >= res.scratch.size() + packed_modes_size);

	out_header->version = BC7PREP_HEADER_VERSION;
	for (int i = 0; i < BC7PREP_MODE_COUNT; i++)
		out_header->mode_counts[i] = (U32)mode_counts[i];

	for (int color_space = 0; color_space < 2; color_space++)
	{
		bool switch_colorspace = color_space != 0;
		bc7_munge_core(res.post_twiddle, res.mode_bytes, mode_byte_offs, bc7_bits, nblocks, switch_colorspace);

		U32 split_modes = 0;
		double J = bc7_decide_splits(res, mode_counts, mode_byte_offs, &split_modes, switch_colorspace, settings);

		if (color_space == 0 || J < best_J)
		{
			best_J = J;
			assign(result, res.best_split);

			out_header->flags = split_modes | (switch_colorspace ? BC7PREP_FLAG_SWITCH_COLORSPACE : 0);
		}
	}

	// append nibble-packed mode bytes at the end
	// NOTE mode_bytes was padded to have an even count
	RR_ASSERT((SINTa)res.mode_bytes.size() == ((nblocks + 1) & ~1));
	for (SINTa i = 0; i < nblocks; i += 2)
	{
		U8 m0 = res.mode_bytes[i];
		U8 m1 = res.mode_bytes[i + 1];

		result.push_back(m0 | (m1<<4));
	}

	SINTa result_size = result.size();
	return result_size;
}

void BC7PrepSettings_InitDefault(BC7PrepSettings * s)
{
	s->lambda = BC7PREP_DEFAULT_LAMBDA;
	s->canonicalize_solid = true;
	s->exhaustive_splits = false;
}

OODLE_NS_END

