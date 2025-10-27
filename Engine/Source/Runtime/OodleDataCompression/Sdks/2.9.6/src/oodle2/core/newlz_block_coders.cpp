// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "newlz_block_coders.h"
#include "rrbits.h"
#include "rrvarbits.h"
#include "rrlog.h"
#include "newlz_arrays.h"
#include "newlz_arrays.inl" // for NEWLZ_ARRAY_RETURN_FAILURE - a bit ugly
#include "newlz_block_coders_ssse3.h"
#include "newlz_simd.h"
#include "cpux86.h"

OODLE_NS_START

#ifdef __RADBIGENDIAN__
#define RR_BE64_TO_NATIVE(x) (x)
#else
#define RR_BE64_TO_NATIVE(x) RR_BSWAP64(x)
#endif

void newLZ_encode_uniform_U8_block(rrVarBits * vb, const U8 * values, SINTa count, int bits_per_val)
{
	RR_ASSERT(bits_per_val >= 0 && bits_per_val <= 8);
	if (!bits_per_val)
		return;

	rrVarBits_Temps();
	rrVarBits_Locals(vbl);
	rrVarBits_Copy(vbl, vb->m);

	rrVarBits_Output(vbl);
	for (SINTa i = 0; i < count; i++)
	{
		rrVarBits_Put(vbl, values[i], bits_per_val);
		rrVarBits_Output(vbl);
	}

	rrVarBits_Copy(vb->m, vbl);
}

// not yet implemented: decode_uniform_U8_block (but it could reuse the construction from
// decode_rice_U8_bottom_block, which is the uniform decoding plus a merge step)

void newLZ_encode_variable_U8_block(rrVarBits * vb, const U8 * values, const U8 * nbits, SINTa count)
{
	rrVarBits_Temps();
	rrVarBits_Locals(vbl);
	rrVarBits_Copy(vbl, vb->m);

	rrVarBits_Output(vbl);
	for (SINTa i = 0; i < count; i++)
	{
		RR_ASSERT(nbits[i] <= 8);
		rrVarBits_Put(vbl, values[i], nbits[i]);
		rrVarBits_Output(vbl);
	}

	rrVarBits_Copy(vb->m, vbl);
}

void newLZ_encode_unary_block(rrVarBits * vb, const U8 * values, SINTa count)
{
	rrVarBits_Temps();
	rrVarBits_Locals(vbl);
	rrVarBits_Copy(vbl, vb->m);

	rrVarBits_Output(vbl);
	for (SINTa i = 0; i < count; i++)
	{
		U32 code = values[i];
		RR_ASSERT(code <= 240);

		while (code >= 24)
		{
			rrVarBits_Put(vbl, 0, 24);
			rrVarBits_Output(vbl);
			code -= 24;
		}

		rrVarBits_Put(vbl, 1, code + 1);
		rrVarBits_Output(vbl);
	}

	rrVarBits_Copy(vb->m, vbl);
}

SINTa newLZ_decode_unary_block(U8 * unary, SINTa count, BlockBitReader * br)
{
	// even nibbles: bytes 0-3 (mask with 0x0f0f0f0f to write)
	// odd  nibbles: bytes 4-6 (shift right by 4 then mask with 0x0f0f0f0f)
	// last nibble:  carry
	static const U32 unary_tab[256] =
	{
		0x80000000u,0x00000007u,0x10000006u,0x00000006u,0x20000005u,0x00000105u,0x10000005u,0x00000005u,
		0x30000004u,0x00000204u,0x10000104u,0x00000104u,0x20000004u,0x00010004u,0x10000004u,0x00000004u,
		0x40000003u,0x00000303u,0x10000203u,0x00000203u,0x20000103u,0x00010103u,0x10000103u,0x00000103u,
		0x30000003u,0x00020003u,0x10010003u,0x00010003u,0x20000003u,0x01000003u,0x10000003u,0x00000003u,
		0x50000002u,0x00000402u,0x10000302u,0x00000302u,0x20000202u,0x00010202u,0x10000202u,0x00000202u,
		0x30000102u,0x00020102u,0x10010102u,0x00010102u,0x20000102u,0x01000102u,0x10000102u,0x00000102u,
		0x40000002u,0x00030002u,0x10020002u,0x00020002u,0x20010002u,0x01010002u,0x10010002u,0x00010002u,
		0x30000002u,0x02000002u,0x11000002u,0x01000002u,0x20000002u,0x00000012u,0x10000002u,0x00000002u,
		0x60000001u,0x00000501u,0x10000401u,0x00000401u,0x20000301u,0x00010301u,0x10000301u,0x00000301u,
		0x30000201u,0x00020201u,0x10010201u,0x00010201u,0x20000201u,0x01000201u,0x10000201u,0x00000201u,
		0x40000101u,0x00030101u,0x10020101u,0x00020101u,0x20010101u,0x01010101u,0x10010101u,0x00010101u,
		0x30000101u,0x02000101u,0x11000101u,0x01000101u,0x20000101u,0x00000111u,0x10000101u,0x00000101u,
		0x50000001u,0x00040001u,0x10030001u,0x00030001u,0x20020001u,0x01020001u,0x10020001u,0x00020001u,
		0x30010001u,0x02010001u,0x11010001u,0x01010001u,0x20010001u,0x00010011u,0x10010001u,0x00010001u,
		0x40000001u,0x03000001u,0x12000001u,0x02000001u,0x21000001u,0x01000011u,0x11000001u,0x01000001u,
		0x30000001u,0x00000021u,0x10000011u,0x00000011u,0x20000001u,0x00001001u,0x10000001u,0x00000001u,
		0x70000000u,0x00000600u,0x10000500u,0x00000500u,0x20000400u,0x00010400u,0x10000400u,0x00000400u,
		0x30000300u,0x00020300u,0x10010300u,0x00010300u,0x20000300u,0x01000300u,0x10000300u,0x00000300u,
		0x40000200u,0x00030200u,0x10020200u,0x00020200u,0x20010200u,0x01010200u,0x10010200u,0x00010200u,
		0x30000200u,0x02000200u,0x11000200u,0x01000200u,0x20000200u,0x00000210u,0x10000200u,0x00000200u,
		0x50000100u,0x00040100u,0x10030100u,0x00030100u,0x20020100u,0x01020100u,0x10020100u,0x00020100u,
		0x30010100u,0x02010100u,0x11010100u,0x01010100u,0x20010100u,0x00010110u,0x10010100u,0x00010100u,
		0x40000100u,0x03000100u,0x12000100u,0x02000100u,0x21000100u,0x01000110u,0x11000100u,0x01000100u,
		0x30000100u,0x00000120u,0x10000110u,0x00000110u,0x20000100u,0x00001100u,0x10000100u,0x00000100u,
		0x60000000u,0x00050000u,0x10040000u,0x00040000u,0x20030000u,0x01030000u,0x10030000u,0x00030000u,
		0x30020000u,0x02020000u,0x11020000u,0x01020000u,0x20020000u,0x00020010u,0x10020000u,0x00020000u,
		0x40010000u,0x03010000u,0x12010000u,0x02010000u,0x21010000u,0x01010010u,0x11010000u,0x01010000u,
		0x30010000u,0x00010020u,0x10010010u,0x00010010u,0x20010000u,0x00011000u,0x10010000u,0x00010000u,
		0x50000000u,0x04000000u,0x13000000u,0x03000000u,0x22000000u,0x02000010u,0x12000000u,0x02000000u,
		0x31000000u,0x01000020u,0x11000010u,0x01000010u,0x21000000u,0x01001000u,0x11000000u,0x01000000u,
		0x40000000u,0x00000030u,0x10000020u,0x00000020u,0x20000010u,0x00001010u,0x10000010u,0x00000010u,
		0x30000000u,0x00002000u,0x10001000u,0x00001000u,0x20000000u,0x00100000u,0x10000000u,0x00000000u,
	};

	static const U8 unary_counts[256] =
	{
		0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
		1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
		1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
		2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
		1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
		2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
		2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
		3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8,
	};

	if (count <= 0)
		return count;

	const U8 *bits_ptr = br->ptr;
	const U8 *bits_end = br->end;
	U32 bits_pos = br->pos_in_byte;

	U8 *unary_cur = unary;
	U8 *unary_end = unary + count;

	// First byte is partial
	if (bits_ptr >= bits_end)
		NEWLZ_ARRAY_RETURN_FAILURE();

	U8 val = *bits_ptr++ & (U8)(0xff >> bits_pos); // mask off top bits that were already used
	U32 carry = 0 - bits_pos; // cancel out the top bits we just zeroed
	bits_pos = 0;

	for (;;)
	{
		if (val != 0)
		{
			U32 tabv = unary_tab[val];
			U32 mask = 0x0f0f0f0fu;
			RR_PUT32_LE(unary_cur + 0,(tabv & mask) + carry);
			RR_PUT32_LE(unary_cur + 4,(tabv >> 4) & mask);
			unary_cur += unary_counts[val];
			if (unary_cur >= unary_end)
				break;
			carry = tabv >> 28;
		}
		else
			carry += 8;

		// Check for input overrun
		if (bits_ptr >= bits_end)
			NEWLZ_ARRAY_RETURN_FAILURE();
		val = *bits_ptr++;
	}

	// In general, the decoder will "overshoot" a bit past the last unary code;
	// figure out the position of the '1' bit terminating the last unary code we wanted.
	while (unary_cur > unary_end)
	{
		val &= val - 1; // clear lowest set bit
		unary_cur--;
	}

	RR_ASSERT(val != 0);
	// Now, the lowest set '1' bit in val is precisely the last bit of the final
	// unary code.
	if ((val & 1) == 0) // at least one bit not consumed yet
	{
		bits_ptr--; // rewind to the partially consumed byte
		bits_pos = 8 - rrCtz32(val);
	}

	br->ptr = bits_ptr;
	br->pos_in_byte = bits_pos;
	return count;
}

void newLZ_encode_rice_U8_split(U8 * out_top, U8 * out_bottom, const U8 * values, SINTa count, int riceK)
{
	U32 mask = (1u << riceK) - 1;

	for (SINTa i = 0; i < count; i++)
	{
		U8 val = values[i];
		out_top[i] = static_cast<U8>( val >> riceK );
		out_bottom[i] = static_cast<U8>(val & mask);
	}
}

SINTa newLZ_decode_rice_U8_bottom_block(U8 *codes, SINTa count, int riceK, BlockBitReader *br)
{
	if (riceK < 0 || riceK > 3)
		return -1;

	// k==0 is pure unary, nothing left to do.
	if (riceK == 0)
		return count;

	const U8 *bits_ptr = br->ptr;
	const U8 *bits_end = br->end;
	U32 bits_pos = br->pos_in_byte;

	// Check that we have sufficient input data available
	SINTa num_uni_bits = count*riceK;
	SINTa num_uni_bytes = (bits_pos + num_uni_bits + 7) >> 3; // bytes accessed
	if (num_uni_bytes > bits_end - bits_ptr)
		NEWLZ_ARRAY_RETURN_FAILURE();

	// Prepare for reading loop
	SINTa tail_count = RR_MIN(num_uni_bytes, 4);
	const U8 *read_ptr = bits_ptr;
	const U8 *read_mark = bits_ptr + num_uni_bytes - tail_count;
	U8 *codes_ptr = codes;
	U8 *codes_end = codes + count;
	U32 shift_amt;

	// We always decode groups of 8 bytes. Rather than bother with write masking
	// for the last few bytes, just save their original values here and restore
	// them after we're done.
	U64 orig_unary_past_rice = RR_GET64_NATIVE_UNALIGNED(codes_end);

	// Set up tail buffer by copying the last few bytes into it
	U8 tail_buf[8] = {}; // init to 0
	for (SINTa i = 0; i < tail_count; i++)
		tail_buf[i] = read_mark[i];

	// Decode!
    // @@ TODO 32-bit variant?
	switch (riceK)
	{
	case 1:
		shift_amt = 24 - bits_pos;
		do
		{
			if (read_ptr >= read_mark)
			{
				read_ptr = tail_buf + (read_ptr - read_mark);
				read_mark = tail_buf + 4; // just make sure we don't hit tail loop twice
			}

			U64 x = (RR_GET32_BE_UNALIGNED(read_ptr) >> shift_amt) & 0xff;
			read_ptr++;

			x = (x ^ (x << 28)) & 0x0000000f0000000full;
			x = (x ^ (x << 14)) & 0x0003000300030003ull;
			x = (x ^ (x <<  7)) & 0x0101010101010101ull;
			x = RR_BE64_TO_NATIVE(x);

			U64 orig = RR_GET64_NATIVE_UNALIGNED(codes_ptr);
			U64 out = (orig << 1) + x; // merge
			RR_PUT64_NATIVE_UNALIGNED(codes_ptr, out);
			codes_ptr += 8;
		} while (codes_ptr < codes_end);
		break;

	case 2:
		shift_amt = 16 - bits_pos;
		do
		{
			if (read_ptr >= read_mark)
			{
				read_ptr = tail_buf + (read_ptr - read_mark);
				read_mark = tail_buf + 4; // just make sure we don't hit tail loop twice
			}

			U64 x = (RR_GET32_BE_UNALIGNED(read_ptr) >> shift_amt) & 0xffff;
			read_ptr += 2;

			x = (x ^ (x << 24)) & 0x000000ff000000ffull;
			x = (x ^ (x << 12)) & 0x000f000f000f000full;
			x = (x ^ (x <<  6)) & 0x0303030303030303ull;
			x = RR_BE64_TO_NATIVE(x);

			U64 orig = RR_GET64_NATIVE_UNALIGNED(codes_ptr);
			U64 out = (orig << 2) + x; // merge
			RR_PUT64_NATIVE_UNALIGNED(codes_ptr, out);
			codes_ptr += 8;
		} while (codes_ptr < codes_end);
		break;

	case 3:
		shift_amt = 8 - bits_pos;
		do
		{
			if (read_ptr >= read_mark)
			{
				read_ptr = tail_buf + (read_ptr - read_mark);
				read_mark = tail_buf + 4; // just make sure we don't hit tail loop twice
			}

			U64 x = (RR_GET32_BE_UNALIGNED(read_ptr) >> shift_amt) & 0xffffff;
			read_ptr += 3;

			x = (x ^ (x << 20)) & 0x00000fff00000fffull;
			x = (x ^ (x << 10)) & 0x003f003f003f003full;
			x = (x ^ (x <<  5)) & 0x0707070707070707ull;
			x = RR_BE64_TO_NATIVE(x);

			U64 orig = RR_GET64_NATIVE_UNALIGNED(codes_ptr);
			U64 out = (orig << 3) + x; // merge
			RR_PUT64_NATIVE_UNALIGNED(codes_ptr, out);
			codes_ptr += 8;
		} while (codes_ptr < codes_end);
		break;

	default:
		RR_BREAK();
	}

	// Restore saved bytes
	RR_PUT64_NATIVE_UNALIGNED(codes_end, orig_unary_past_rice);

	// Update current read position
	num_uni_bits += bits_pos;
	bits_ptr += num_uni_bits >> 3;
	bits_pos = (U32) (num_uni_bits & 7);

	br->ptr = bits_ptr;
	br->pos_in_byte = bits_pos;
	return count;
}

int newLZ_encode_alphabet_shape_runlens_split(U8 * top, U8 * bot, U8 * bot_nbits, int num_syms, const t_alphabet_runlen_type * run_lens, int run_count)
{
	RR_ASSERT( num_syms >= 1 && num_syms <= 256 );
	RR_ASSERT( run_count >= 3 && ( run_count & 1 ) == 1 );

	if (num_syms < 256)
	{
		// The last run pair is implied by the overall alphabet size and num_syms (both of which
		// are sent explicitly)
		int num_pairs = (run_count - 3) / 2;
		int initial_zero_run = run_lens[0] != 0;

		// This gives the number of EG codes, but both num_pairs and initial_zero_run can
		// be reconstructed from this value.
		int num_EGs = num_pairs*2 + initial_zero_run;

		int begin = 1 - initial_zero_run;
		for (int j = 0; j < num_EGs; j++)
		{
			int i = j + begin;

			// Determine the EG code
			// Even-indexed runs (zero runs) have k=1, odd-indexed runs (nonzero runs) have k=0
			RR_ASSERT( run_lens[i] >= 1 && run_lens[i] <= 255 );
			U32 k = (i & 1) ? 0 : 1; // zero run lens have k=1, nz lens have k=0
			U32 val = run_lens[i] - 1 + (1<<k);
			U32 main_bits = val >> k;
			U32 prefix = 31 - rrClz32(main_bits);
			U32 num_bottom = prefix + k;

			top[j] = static_cast<U8>(prefix);
			bot[j] = static_cast<U8>(val & ((1u << num_bottom) - 1));
			bot_nbits[j] = static_cast<U8>(num_bottom);
		}

		return num_EGs;
	}
	else
	{
		// Nothing to send, but verify that the run lens are what they should be
		RR_ASSERT( run_lens[0] == 0 );
		RR_ASSERT( run_lens[1] == 256 );
		RR_ASSERT( run_lens[2] == 0 );
		RR_ASSERT( run_count == 3 );
		return 0;
	}
}

void newLZ_encode_alphabet_shape_num_EG(rrVarBits * vb, int num_eg, int num_syms)
{
	RR_ASSERT(0 < num_syms && num_syms <= 256);
	if (num_syms != 256)
	{
		int num_eg_bound = RR_MIN(num_syms, 257-num_syms)*2; // *strictly* larger than num_eg; >=2
		RR_ASSERT(0 <= num_eg && num_eg < num_eg_bound);

		// this is just rrVarBits_WriteFlat
		U32 nbits = 32 - rrClz32(num_eg_bound - 1);
		int large_thresh = (1 << nbits) - num_eg_bound;
		if (num_eg < large_thresh)
			rrVarBits_Put(vb->m, num_eg, (nbits-1));
		else
		{
			int code = num_eg + large_thresh; // now >=2*large_thresh, so top (nbits-1) are >=thres
			rrVarBits_Put(vb->m, code, nbits);
		}

		rrVarBits_Output(vb->m);
	}
}

static RADFORCEINLINE U32 decode_alphabet_shape_zerorun(U32 prefix, rrVarBits_FuncArgs(vb))
{
	if (prefix > 7)
		return 511;
	else
	{
		rrVarBits_Temps();
		U32 nextra = prefix + 1;
		U32 code = (1u << nextra) + (U32)rrVarBits_Get_V(vb,nextra); // smallest value is 2
		return (code - 2) + 1; // run lens are >=1
	}
}

static RADFORCEINLINE U32 decode_alphabet_shape_nonzerorun(U32 prefix, rrVarBits_FuncArgs(vb))
{
	if (prefix > 8)
		return 511;
	else
	{
		rrVarBits_Temps();
		U32 code = (1u << prefix) + (U32)rrVarBits_Get_0Ok(vb,prefix); // smallest value is 1
		return (code - 1) + 1; // run lens are >= 1
	}
}

#if defined(__RADNEON__)

// this is not a general PSHUFB; these are the loads for our byte values,
// where inds[0..7] is guaranteed to be in 0..7 so the first half doesn't
// need a 2-wide "table lookup"!
static RADFORCEINLINE uint8x16_t fetch_shuffle_u8(uint8x16_t tbl, uint8x16_t inds)
{
	uint8x8x2_t shuf_in;
	shuf_in.val[0] = vget_low_u8(tbl);
	shuf_in.val[1] = vget_high_u8(tbl);
	uint8x8_t lo = vtbl1_u8(shuf_in.val[0], vget_low_u8(inds));
	uint8x8_t hi = vtbl2_u8(shuf_in, vget_high_u8(inds));
	return vcombine_u8(lo, hi);
}

static RADFORCEINLINE U32 get_exp_golomb_tails(uint16x8_t *out0, uint16x8_t *out1, const U8 *in_ptr, U32 bit_pos, uint8x16_t field_widths)
{
	// prefix-sum the field widths and advance bit position pointer
	uint8x16_t summed_widths = simd_prefix_sum_u8(field_widths);
	U32 total_width = vgetq_lane_u8(summed_widths, 15);

	// determine starting bit position within every lane
	// and split into bit-within-byte and byte indices
	uint8x16_t basepos_u8 = vdupq_n_u8((U8) bit_pos);
	uint8x16_t first_bit_index = vaddq_u8(basepos_u8, vextq_u8(vdupq_n_u8(0), summed_widths, 16-1));
	uint8x16_t first_byte_index = vshrq_n_u8(first_bit_index, 3);

	// source bytes
	uint8x16_t src_byte0 = vld1q_u8(in_ptr + 0);
	uint8x16_t src_byte1 = vld1q_u8(in_ptr + 1);

	// first/second bytes for every lane
	uint8x16_t byte0 = fetch_shuffle_u8(src_byte0, first_byte_index);
	uint8x16_t byte1 = fetch_shuffle_u8(src_byte1, first_byte_index);

	// let first_idx7 = first_bit_index & 7
	// shift byte0 by (field_width + first_idx7) - 8
	// and   byte1 by (field_width + first_idx7) - 16
	// (for NEON, negative left shift = right shift)
	// then OR them together.
	//
	// we use that (first_bit_index & 7) - 8 = (first_bit_index | -8)
	int8x16_t shift_amt_s8 = vaddq_s8(vreinterpretq_s8_u8(field_widths), vorrq_s8(vreinterpretq_s8_u8(first_bit_index), vdupq_n_s8(-8)));
	uint8x16_t shifted0 = vshlq_u8(byte0, shift_amt_s8);
	uint8x16_t shifted1 = vshlq_u8(byte1, vaddq_s8(shift_amt_s8, vdupq_n_s8(-8)));
	uint8x16_t combined = vorrq_u8(shifted0, shifted1);

	// mask by field width: & ((1 << width) - 1)
	uint8x16_t shifted_one = vshlq_u8(vdupq_n_u8(1), vreinterpretq_s8_u8(field_widths));
	uint8x16_t mask = vsubq_u8(shifted_one, vdupq_n_u8(1));
	uint8x16_t masked = vandq_u8(combined, mask);

	// bias by adding mask
	*out0 = vaddl_u8(vget_low_u8(masked), vget_low_u8(mask));
	*out1 = vaddl_u8(vget_high_u8(masked), vget_high_u8(mask));

	return total_width;
}

static RADFORCEINLINE UINTa subtract_sat(UINTa a, UINTa b)
{
	return (a < b) ? 0 : a - b;
}

bool newLZ_decode_alphabet_shape_run_pairs_neon(U16 * runLens, BlockBitReader * bbr, const U8 * run_prefix, U32 num_run_pairs)
{
	// Just check if we're already past the end
	if (bbr->ptr >= bbr->end)
		return false;
	UINTa byte_offs_end = bbr->end - bbr->ptr;

	const U8 * bit_base = bbr->ptr;
	U32 bit_pos = bbr->pos_in_byte;

	// NOTE: zero-run codes with width=8: minimum value is (1<<8)-1 = 255; this can *only* occur for
	// the initial zero run but not in here.
	// Nonzero-run codes with width=8: minimum value is (1<<8) = 256; this can *never* occur, since our
	// alphabets are at most 256 symbols and the last nonzero run is implicit.
	//
	// so we could lower this to 7 if that helps.
	static const S8 kMaxExtraBits = 8;

	// Compute start of region where we have to be careful
	static const UINTa kMaxBytes = (127 /*maxRunPairs*/ * 2 * kMaxExtraBits + 7)/8 + 1 /* partial byte we start in */;
	static const UINTa kFinalSlopBytes = 16; // SIMD code reads up to 16 (not 15!) bytes past last touched byte.
	U8 tail_buf[kFinalSlopBytes*2];
	UINTa byte_offs_mark = byte_offs_end;

	// We only need to initialize the tail buf if there's a risk of us actually using it.
	// We defer this because even just accessing a very far-away vbl_end really hurts!
	if (byte_offs_end < kMaxBytes + kFinalSlopBytes)
	{
		byte_offs_mark = subtract_sat(byte_offs_end, kFinalSlopBytes);
		UINTa num_tail_bytes = byte_offs_end - byte_offs_mark;
		if (num_tail_bytes)
			memcpy(tail_buf, bit_base + byte_offs_mark, num_tail_bytes);
		memset(tail_buf + num_tail_bytes, 0, kFinalSlopBytes);
	}

	// NOTE: can just keep reading past end of numRunPairs, we made sure the "unary" array
	// ends with 16 zeros, so over-reading doesn't do anything wrong
	uint8x16_t max_widths = vdupq_n_u8(kMaxExtraBits); // initial value for field widths (all OK)

	for (UINTa i = 0; i < num_run_pairs; i += 8)
	{
		uint8x16_t counts = vld1q_u8(run_prefix + i*2);
		// zero runs (at odd indices) have k=1 so they read one extra bit
		// use add with saturation so we don't wrap around on (invalid)
		// unary codes of 255, and instead detect the error properly.
		counts = vqaddq_u8(counts, vreinterpretq_u8_u16(vdupq_n_u16(0x0100)));
		// keep track of max width we've seen
		max_widths = vmaxq_u8(max_widths, counts);
		// then clamp field widths to 8 bits so we can guarantee our kMaxBytes estimate holds
		counts = vminq_u8(counts, vdupq_n_u8(8));
		// end-of-buffer handling
		UINTa byte_index = bit_pos >> 3;
		const U8 *read_ptr = bit_base + byte_index;
		if (byte_index >= byte_offs_mark)
		{
			if (byte_index >= byte_offs_end)
				return false;
			read_ptr = tail_buf + byte_index - byte_offs_mark;
		}
		// decode
		uint16x8_t tail0, tail1;
		bit_pos += get_exp_golomb_tails(&tail0, &tail1, read_ptr, bit_pos & 7, counts);
		// nonzero runs (even lanes) need an extra bias of 1
		tail0 = vaddq_u16(tail0, vreinterpretq_u16_u32(vdupq_n_u32(1)));
		tail1 = vaddq_u16(tail1, vreinterpretq_u16_u32(vdupq_n_u32(1)));
		// store!
		vst1q_u16(runLens + i*2 + 0, tail0);
		vst1q_u16(runLens + i*2 + 8, tail1);
	}

	// if there was any out-of-range code length, that's an error
	if (!check_all_set_u8_neon(vceqq_u8(max_widths, vdupq_n_u8(kMaxExtraBits))))
		return false;

	// Our golombCount loop ends with a partial iteration that doesn't have all 16 lanes
	// actually active. We zero-padded the unary code array, so we didn't produce total
	// garbage, but we did read one extra bit for every zero run (one per pair). Undo that.
	bit_pos -= (0 - num_run_pairs) & 7;

	// Check if we consumed bits past the end
	if (((bit_pos + 7) >> 3) > byte_offs_end)
		return false;

	bbr->ptr += bit_pos >> 3;
	bbr->pos_in_byte = bit_pos & 7;
	return true;
}

#endif

int newLZ_decode_alphabet_shape_runlens(U16 * run_lens, U32 num_nonzero_syms, U32 num_eg, const U8 * run_prefix, rrVarBits * vb)
{
	RR_ASSERT(num_nonzero_syms > 0 && num_nonzero_syms <= 256);
	RR_ASSERT(num_eg <= 255);

	int numRunPairs = num_eg >> 1;

	rrVarBits_Temps();
	rrVarBits_Locals(vbl);
	rrVarBits_Copy(vbl,vb->m);

	U32 curSymbol = 0; // current symbol
	if (num_eg & 1) // initial zero run present
	{
		rrVarBits_Refill_Safe(vbl);
		curSymbol = (U16) decode_alphabet_shape_zerorun(run_prefix[0], rrVarBits_PassArgs(vbl));
		run_prefix++;
	}

	// Decode run len codes
#if defined(__RADNEON__)
	{
		BlockBitReader bbr;
		rrVarBits_To_BlockBitReader(vbl, &bbr);

		if (!newLZ_decode_alphabet_shape_run_pairs_neon(run_lens + 1, &bbr, run_prefix, numRunPairs))
			NEWLZ_ARRAY_RETURN_FAILURE();

		rrVarBits_From_BlockBitReader(vbl, &bbr);
	}
#else
	if ( ! g_has_newlz_decode_alphabet_shape_run_pairs_ssse3 || !rrCPUx86_feature_present(RRX86_CPU_SSSE3))
	{
		for (SINTa pair = 0; pair < numRunPairs; pair++)
		{
			rrVarBits_Refill_Safe(vbl);
			run_lens[pair*2 + 1] = (U16) decode_alphabet_shape_nonzerorun(run_prefix[0], rrVarBits_PassArgs(vbl));
			run_lens[pair*2 + 2] = (U16) decode_alphabet_shape_zerorun(run_prefix[1], rrVarBits_PassArgs(vbl));
			run_prefix += 2;
		}
	}
	else
	{
#ifdef __RADSSE2__
		// I looked into scalar, but even with the shuffle-tastic craziness for SSSE3, 16-way SIMD is *far*
		// fewer ops total, and scalar really has no realistic chance to catch up. (Given competent SIMD
		// units, anyway.)
		BlockBitReader bbr;
		rrVarBits_To_BlockBitReader(vbl, &bbr);

		if (!newLZ_decode_alphabet_shape_run_pairs_ssse3(run_lens + 1, &bbr, run_prefix, numRunPairs))
			NEWLZ_ARRAY_RETURN_FAILURE();

		rrVarBits_From_BlockBitReader(vbl, &bbr);
#else
		RR_BREAK();
#endif
	}
#endif

	// @@ could (and probably should) move this into the SIMD portion too
	// but one thing at a time

	// Perform summing of zero counts and nonzero counts
	// Initial zero run is already in curSymbol
	U32 totalNonzero = 0;
	for (SINTa pair = 0; pair < numRunPairs; pair++)
	{
		U32 nzlen = run_lens[pair*2 + 1]; // <512
		U32 zlen = run_lens[pair*2 + 2]; // <512

		// update run_lens[pair*2 + 0] to indicate first symbol in run
		// (run_lens[pair * 2 + 1] stays unmodified)
		run_lens[pair*2 + 0] = (U16)curSymbol;

		// 32-bit totals can't overflow: the summands are <512 and we add up at most numRunPairs*2 (<=254) of them
		curSymbol += nzlen + zlen;
		totalNonzero += nzlen;
	}

	// Final NZ run is implied and has nonzero length; check that the counts work out.
	if (curSymbol >= 256 || totalNonzero >= num_nonzero_syms)
		NEWLZ_ARRAY_RETURN_FAILURE();

	U32 finalRunLen = num_nonzero_syms - totalNonzero;
	if (curSymbol + finalRunLen > 256)
		NEWLZ_ARRAY_RETURN_FAILURE();

	run_lens[numRunPairs*2 + 0] = (U16)curSymbol;
	run_lens[numRunPairs*2 + 1] = (U16)finalRunLen;

	rrVarBits_Copy(vb->m,vbl);
	return numRunPairs + 1;
}

OODLE_NS_END
