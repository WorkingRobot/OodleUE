// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// Returns the shuffle mask for channel rotation on 16-bit components
static RADFORCEINLINE Vec128 sse4_channel_rotate16(U32 rot)
{
	static RAD_ALIGN(const S8, channel_shuffles[4][16], 16) =
	{
		{ 0,1, 2,3, 4,5, 6,7,   8, 9, 10,11, 12,13, 14,15 }, // rot=0: no change
		{ 6,7, 2,3, 4,5, 0,1,  14,15, 10,11, 12,13,  8, 9 }, // rot=1: swap R<->A
		{ 0,1, 6,7, 4,5, 2,3,   8, 9, 14,15, 12,13, 10,11 }, // rot=2: swap G<->A
		{ 0,1, 2,3, 6,7, 4,5,   8, 9, 10,11, 14,15, 12,13 }, // rot=3: swap B<->A
	};

	return load128a(channel_shuffles[rot]);
}

// Returns 8 bit fields at the given positions (in bits) and of the
// given widths as 16-bit integers, with the values aligned with the
// MSB at the top and garbage in the lower-order bits
template<
	int pos0,int len0,
	int pos1,int len1,
	int pos2,int len2,
	int pos3,int len3,
	int pos4,int len4,
	int pos5,int len5,
	int pos6,int len6,
	int pos7,int len7
>
static RADFORCEINLINE Vec128 sse4_multigetbits(const Vec128 &bytes)
{
	// Grab the two bytes straddling each field
	const Vec128 shuf = _mm_setr_epi8(
		(pos0 >> 3), (pos0 >> 3) + 1,
		(pos1 >> 3), (pos1 >> 3) + 1,
		(pos2 >> 3), (pos2 >> 3) + 1,
		(pos3 >> 3), (pos3 >> 3) + 1,
		(pos4 >> 3), (pos4 >> 3) + 1,
		(pos5 >> 3), (pos5 >> 3) + 1,
		(pos6 >> 3), (pos6 >> 3) + 1,
		(pos7 >> 3), (pos7 >> 3) + 1
	);

	// work around cast truncation warnings
	// the real issue here is that _mm_setr_* are all signed and we
	// need U16 constants
#define SHORT_CONST(x) ((short)(((x) & 0x7fff) - ((x) & 0x8000)))

	// Use a multiply to do a per-lane variable shift to align the
	// desired bits at the top
	const Vec128 mult = _mm_setr_epi16(
		SHORT_CONST(1 << (16 - len0 - (pos0 & 7))),
		SHORT_CONST(1 << (16 - len1 - (pos1 & 7))),
		SHORT_CONST(1 << (16 - len2 - (pos2 & 7))),
		SHORT_CONST(1 << (16 - len3 - (pos3 & 7))),
		SHORT_CONST(1 << (16 - len4 - (pos4 & 7))),
		SHORT_CONST(1 << (16 - len5 - (pos5 & 7))),
		SHORT_CONST(1 << (16 - len6 - (pos6 & 7))),
		SHORT_CONST(1 << (16 - len7 - (pos7 & 7)))
	);

#undef SHORT_CONST

	// ---- Now comes the actual code, which is very short:
	Vec128 shuffled = _mm_shuffle_epi8(bytes, shuf);
	Vec128 result = _mm_mullo_epi16(shuffled, mult);

	return result;
}

// Returns 8 bit fields at the given positions (in bits) and of the
// given widths as 16-bit integers, with the values aligned with the
// MSB at the top and lower-order bits cleared
template<
	int pos0,int len0,
	int pos1,int len1,
	int pos2,int len2,
	int pos3,int len3,
	int pos4,int len4,
	int pos5,int len5,
	int pos6,int len6,
	int pos7,int len7
>
static RADFORCEINLINE Vec128 sse4_multigetbits_masked(const Vec128 &bytes)
{
	const Vec128 mask = _mm_setr_epi16(
		-(1 << (16 - len0)),
		-(1 << (16 - len1)),
		-(1 << (16 - len2)),
		-(1 << (16 - len3)),
		-(1 << (16 - len4)),
		-(1 << (16 - len5)),
		-(1 << (16 - len6)),
		-(1 << (16 - len7))
	);

	Vec128 aligned = sse4_multigetbits<pos0,len0, pos1,len1, pos2,len2, pos3,len3, pos4,len4, pos5,len5, pos6,len6, pos7,len7>(bytes);
	Vec128 result = _mm_and_si128(aligned, mask);

	return result;
}

// in_v is a vector containing 2-bit indices starting at byte b0
template<S8 b0>
static RADFORCEINLINE Vec128 sse4_ind2b_to_lerpf(const Vec128 &in_v)
{
	// Replicate each byte 4x using a shuffle
	const Vec128 repl_shuffle = _mm_setr_epi8(b0,b0,b0,b0, b0+1,b0+1,b0+1,b0+1, b0+2,b0+2,b0+2,b0+2, b0+3,b0+3,b0+3,b0+3);
	Vec128 v = _mm_shuffle_epi8(in_v, repl_shuffle);

	const Vec128 mask_bit0 = _mm_set1_epi32((1<<0) + (1<<10) + (1<<20) + (1<<30));
	const Vec128 mask_bit1 = _mm_set1_epi32((1<<1) + (1<<11) + (1<<21) - 0x80000000);

	Vec128 bit0_set = _mm_cmpeq_epi8(_mm_and_si128(v, mask_bit0), mask_bit0);
	Vec128 bit1_set = _mm_cmpeq_epi8(_mm_and_si128(v, mask_bit1), mask_bit1);

	// Produce the same packed negative lerp factors as radtex_lerp_factor_neg2x.
	// This is easy: bit 0 subtracts 42, and bit 1 subtracts 86. The third
	// table entry works out.
	Vec128 bit0_term = _mm_and_si128(bit0_set, _mm_set1_epi8(-42));
	Vec128 bit1_term = _mm_and_si128(bit1_set, _mm_set1_epi8(-86));
	return _mm_add_epi8(bit0_term, bit1_term);
}

// v is a vector with 3-bit indices in bits [127:80], rest is ignored
static RADFORCEINLINE Vec128 sse4_ind3b_to_lerpf(const Vec128 &v)
{
	// Each 16 bits of the output need to grab 6 bits of the source,
	// which we can do using multigetbits:
	Vec128 bitfields_aligned = sse4_multigetbits<80,6, 86,6, 92,6, 98,6, 104,6, 110,6, 116,6, 122,6>(v);

	// Align the two halves. First shift down, which puts the lower half in the right spot:
	Vec128 lo_aligned = _mm_srli_epi16(bitfields_aligned, 10);

	// Now align the high half using a multiply-add, a similiar idea to what we do for anchor bit
	// insertion. The idea is to mask only the high bits in aligned value, and multiply them
	// by (1 << shift_amount) - 1, then add the result to the original aligned value. This leaves
	// the low bits as they are and shifts the high bits by shift_amount.
	Vec128 hi_masked = _mm_and_si128(lo_aligned, _mm_set1_epi16(7 << 3));
	Vec128 index8 = _mm_add_epi16(lo_aligned, _mm_mullo_epi16(hi_masked, _mm_set1_epi16((1 << 5) - 1)));

	// Now the low bits are in their original position (in the low byte of every word) and the high
	// bits are shifted by 5 from their original position, which puts them right at the high byte of
	// every word.

	// Finally, do the lerp table lookup
	Vec128 lerp_factor_table = _mm_setr_epi8(0, -18, -36, -54, -74, -92, -110, -128, 0,0,0,0,0,0,0,0);
	Vec128 lerpf8 = _mm_shuffle_epi8(lerp_factor_table, index8);
	return lerpf8;
}

// v is a vector with 4-bit indices in bits [127:64], rest is ignored
static RADFORCEINLINE Vec128 sse4_ind4b_to_lerpf(const Vec128 &v)
{
	// Expand indices from 4 bits per pixel up to 8
	Vec128 index8_premask = _mm_unpackhi_epi8(v, _mm_srli_epi16(v, 4));
	Vec128 index8 = _mm_and_si128(index8_premask, _mm_set1_epi8(0xf));

	// Turn indices into lerp factors
	Vec128 lerp_factor_table = _mm_setr_epi8(0, -8, -18, -26, -34, -42, -52, -60, -68, -76, -86, -94, -102, -110, -120, -128);
	Vec128 lerpf8 = _mm_shuffle_epi8(lerp_factor_table, index8);
	return lerpf8;
}

// Insert anchor bits for two-subset modes. Index bits are always in the second (high)
// 64-bit half of the block and are in three slices:
//   [63:top] stays where it is
//   [top-1:mid] moves down by 1 bit
//   [mid-1:2] moves down by 2 bits
template<int t_mid>
static RADFORCEINLINE Vec128 sse4_insert_anchors_two_subset(const Vec128 &block128, int top)
{
	// mid_and_up is a mask with bits [0,t_mid-1] all 0s and the rest 1s
	// This would be simple if VC++ had _mm_setr_epi64 in 32-bit mode, but it doesn't.
	// almost everything in this stupid expression is just to avoid warnings...
	const Vec128 mid_and_up = _mm_setr_epi32(0,0, (t_mid < 32) ? -(1 << (t_mid & 31)) : 0, (t_mid < 32) ? -1 : -(1 << (t_mid & 31)));

	// top_mask is variable, so mid_mask which derives from it is too
	Vec128 top_mask = _mm_sll_epi64(_mm_setr_epi32(0,0, -1,-1), _mm_cvtsi32_si128(top));
	Vec128 mid_mask = _mm_xor_si128(top_mask, mid_and_up);

	// Now move everything into place
	Vec128 result = _mm_and_si128(block128, top_mask);
	result = _mm_or_si128(result, _mm_srli_epi64(_mm_and_si128(block128, mid_mask), 1));
	result = _mm_or_si128(result, _mm_srli_epi64(_mm_andnot_si128(mid_and_up, block128), 2));

	return result;
}

// Insert anchor bits for three-subset modes. Index bits are always in the second (high)
// 64-bit half of the block and are in four slices:
//   [63:top] stays where it is ("top" slice)
//   [top-1:mid] moves down by 1 bit ("mid1" slice)
//   [mid-1:bot] moves down by 2 bits ("mid2" slice)
//   [bot-1:3] moves down by 3 bits ("bot" slice)
template<int t_bot>
static RADFORCEINLINE Vec128 sse4_insert_anchors_three_subset(const Vec128 &block128, int top, int mid)
{
	const Vec128 high64 = _mm_setr_epi32(0,0, -1,-1);

	// bot_and_up is a mask with bits [0,t_bot-1] all 0s and the rest 1s
	// This would be simple if VC++ had _mm_setr_epi64 in 32-bit mode, but it doesn't.
	// almost everything in this stupid expression is just to avoid warnings...
	const Vec128 bot_and_up = _mm_setr_epi32(0,0, (t_bot < 32) ? -(1 << (t_bot & 31)) : 0, (t_bot < 32) ? -1 : -(1 << (t_bot & 31)));

	Vec128 top_mask = _mm_sll_epi64(high64, _mm_cvtsi32_si128(top));
	Vec128 mid_and_up = _mm_sll_epi64(high64, _mm_cvtsi32_si128(mid));

	Vec128 mid2_mask = _mm_xor_si128(bot_and_up, mid_and_up);
	Vec128 mid1_mask = _mm_xor_si128(mid_and_up, top_mask);

	// With the masks computed, we can now move everything
	Vec128 result = _mm_and_si128(block128, top_mask);
	result = _mm_or_si128(result, _mm_srli_epi64(_mm_and_si128(block128, mid1_mask), 1));
	result = _mm_or_si128(result, _mm_srli_epi64(_mm_and_si128(block128, mid2_mask), 2));
	result = _mm_or_si128(result, _mm_srli_epi64(_mm_andnot_si128(bot_and_up, block128), 3));

	return result;
}

// ---- Decoders for endpoints/indices in the various modes

/*
static U64 bit_read(const U8* bytes, int pos, int len)
{
	U64 x = 0;

	RR_ASSERT(pos >= 0 && len > 0 && len < 64);
	RR_ASSERT(pos + len <= 128);

	if (pos < 64)
		x |= RR_GET64_LE_UNALIGNED(bytes) >> pos;

	if (pos + len > 64)
	{
		U64 hi = RR_GET64_LE_UNALIGNED(bytes + 8);
		if (pos >= 64)
			x = hi >> (pos - 64);
		else
			x |= hi << (64 - pos);
	}

	x &= (1ull << len) - 1;
	return x;
}
*/

static RADFORCEINLINE void sse4_decode_mode0_endpoints(Vec128 * out_lo8, Vec128 * out_hi8, const Vec128 & block128, const U8 * block_bits)
{
	// Grab all the endpoints, "low" (even-numbered ones) separate from "high" (odd).
	// This works different than it does in other modes because with the 4-bit fields,
	// we can do everything at the byte level.

	// First, do a 128-bit shift right by 5 so we have the endpoints byte-aligned.
	Vec128 ep_lo_shift = _mm_srli_epi64(block128, 5);
	Vec128 ep_hi_shift = _mm_slli_epi64(block128, 64-5);
	Vec128 ep_shifted = _mm_or_si128(ep_lo_shift, _mm_srli_si128(ep_hi_shift, 8));

	// Extract the low/high halves into separate regs and expand to 8 bits, leaving space for the pbits
	Vec128 ep_lo8mask1 = _mm_and_si128(ep_shifted, _mm_set1_epi8(0xf));
	Vec128 ep_lo8mask2 = _mm_and_si128(ep_shifted, _mm_set1_epi8(0xe));
	Vec128 ep_lo8 = _mm_add_epi8(_mm_slli_epi16(ep_lo8mask1, 4), _mm_srli_epi16(ep_lo8mask2, 1)); // R0,R2,R4, G0,G2,G4, B0,B2,B4
	Vec128 ep_hi8mask1 = _mm_and_si128(ep_shifted, _mm_set1_epi8(-0x10));
	Vec128 ep_hi8mask2 = _mm_and_si128(ep_shifted, _mm_set1_epi8(-0x20));
	Vec128 ep_hi8 = _mm_add_epi8(ep_hi8mask1, _mm_srli_epi16(ep_hi8mask2, 5));

	// Grab the pbits
	Vec128 pbits_raw = sse4_multigetbits<
		77,1,	// P0
		78,1,	// P1
		79,1,	// P2
		80,1,	// P3
		81,1,	// P4
		82,1,	// P5
		82,1,	// P5 (ignore)
		82,1	// P5 (ignore)
	>(block128);

	// Align in the right spot (bit 3) and mask
	pbits_raw = _mm_and_si128(_mm_srli_epi16(pbits_raw, 15-3), _mm_set1_epi16(8));
	pbits_raw = _mm_or_si128(pbits_raw, _mm_setr_epi16(0,0,0,0,0,0, -1,-1));

	// Combine raw endpoints with pbits to get final packed endpoints
	*out_lo8 = _mm_or_si128(ep_lo8, _mm_shuffle_epi8(pbits_raw, _mm_setr_epi8(0,4, 8, 0,4, 8, 0,4, 8, 12,12,12, -1,-1,-1,-1)));
	*out_hi8 = _mm_or_si128(ep_hi8, _mm_shuffle_epi8(pbits_raw, _mm_setr_epi8(2,6,10, 2,6,10, 2,6,10, 12,12,12, -1,-1,-1,-1)));

	// now lo8 = (R0,R2,R4, G0,G2,G4, B0,B2,B4, A0,A2,A4, <ignored>)
	//     hi8 = (R1,R3,R5, G1,G3,G5, B1,B3,B5, A1,A3,A5, <ignored>)

	/*
	U8 ref_lo8[9];
	U8 ref_hi8[9];
	for (int c = 0; c < 3; ++c)
	{
		for (int i = 0; i < 6; ++i)
		{
			int color_raw = (int)bit_read(block_bits, 5 + c*24 + i*4, 4);
			int pbit = (int)bit_read(block_bits, 77 + i, 1);

			U8 color = (U8)((color_raw << 4) | (pbit << 3) | (color_raw >> 1));
			if (i & 1)
				ref_hi8[c*3 + i/2] = color;
			else
				ref_lo8[c*3 + i/2] = color;
		}
	}
	*/
}

static RADFORCEINLINE Vec128 sse4_decode_mode0_inds(const Vec128 &block128, const BC67Partition * partition)
{
	// Figure out the position of the bit _after_ the final anchor bit.
	// This is in the same place before and after the anchor insertion.
	int top_begin = partition->anchor1 * 3 /* 3b inds */ + 3 /* _after_ anchor */ + 16 /* start of (unpacked) inds */;

	// mid_begin is the next-highest anchor, which has 1 bit missing after it
	int mid_begin = partition->anchor0 * 3 /* 3b inds */ + 3 /* _after_ anchor */ + 16 /* start of (unpacked) inds */ + 1 /* 1 bit missing above */;

	// the packed indices start at bit 83 = bit 19 into the top 64 bits;
	// we have 2 bits for the initial index, then one anchor, meaning the "bot" point starts at bit 21
	Vec128 index = sse4_insert_anchors_three_subset<21>(block128, top_begin, mid_begin);

	// With 3-bit indices straightened out, convert them to lerp factors
	Vec128 lerpf8 = sse4_ind3b_to_lerpf(index);

	return lerpf8;
}

static RADFORCEINLINE void sse4_decode_mode1_endpoints(Vec128 *out_lo16, Vec128 *out_hi16, const Vec128 &block128, const U8 * block_bits)
{
	static RAD_ALIGN(const U16, pbit_table[4][8], 16) =
	{
		{ 0,0,0,0, 0,0,0,0 },
		{ 2,2,2,0, 0,0,0,0 },
		{ 0,0,0,0, 2,2,2,0 },
		{ 2,2,2,0, 2,2,2,0 },
	};

	// Grab all the endpoints, "low" (even-numbered ones) seperate from "high" (odd)
	// we use the mode bit to get a free always-1 bit for the alpha channel
	Vec128 raw_lo16 = sse4_multigetbits_masked<
		8,6,  // R0
		32,6, // G0
		56,6, // B0
		1,1,  // "A0"
		20,6, // R2
		44,6, // G2
		68,6, // B2
		1,1   // "A3"
	>(block128);

	Vec128 raw_hi16 = sse4_multigetbits_masked<
		14,6, // R1
		38,6, // G1
		62,6, // B1
		1,1,  // "A1"
		26,6, // R3
		50,6, // G3
		74,6, // B3
		1,1   // "A3"
	>(block128);

	// Use a multiply to handle the top-bit replicatiion and the down-scaling at
	// the same time. (This also replicates the always-1 alpha bit 8 times.)
	const Vec128 downscale_mult = _mm_setr_epi16(
		(1<<8) + (1<<1), (1<<8) + (1<<1), (1<<8) + (1<<1), 0x1fe,
		(1<<8) + (1<<1), (1<<8) + (1<<1), (1<<8) + (1<<1), 0x1fe
	);

	Vec128 lo16 = _mm_mulhi_epu16(raw_lo16, downscale_mult);
	Vec128 hi16 = _mm_mulhi_epu16(raw_hi16, downscale_mult);

	// Add in the p-bits. The shared p-bits mean we have the same mask for both
	// the low and high endpoints in each pair.
	Vec128 pbit_mask = load128a(pbit_table[block_bits[10] & 3]);
	*out_lo16 = _mm_or_si128(lo16, pbit_mask);
	*out_hi16 = _mm_or_si128(hi16, pbit_mask);
}

static RADFORCEINLINE Vec128 sse4_decode_mode1_inds(const Vec128 &block128, const BC67Partition * partition)
{
	// Figure out the position of the bit _after_ the final anchor bit.
	// This is in the same place before and after the anchor insertion.
	int top_begin = partition->anchor0 * 3 /* 3b inds */ + 3 /* _after_ anchor */ + 16 /* start of (unpacked) inds */;
	// the packed indices start at bit 82 = bit 18 into the top 64 bits;
	// we have 2 bits for the initial index, then one anchor, meaning the "mid" point starts at bit 20
	Vec128 index = sse4_insert_anchors_two_subset<20>(block128, top_begin);

	// With 3-bit indices straightened out, convert them to lerp factors
	Vec128 lerpf8 = sse4_ind3b_to_lerpf(index);

	return lerpf8;
}

static RADFORCEINLINE void sse4_decode_mode2_endpoints(Vec128 * out_lo8, Vec128 * out_hi8, const Vec128 &block128, const U8 * block_bits)
{
	// Grab all the endpoints, "low" (even-numbered ones) seperate from "high" (odd)
	// The endpoint order for three-subset modes stays deinterleaved

	// Annoyingly, we need 9 values for either half and can grab at most 8 in one go,
	// so we have a third fetch for the remaing two blue values (which we also
	// use to splice the alpha in)
	Vec128 raw_lo16 = sse4_multigetbits_masked<
		9,5, // R0
		19,5, // R2
		29,5, // R4
		39,5, // G0
		49,5, // G2
		59,5, // G4
		69,5, // B0
		79,5  // B2
	>(block128);

	Vec128 raw_hi16 = sse4_multigetbits_masked<
		14,5, // R1
		24,5, // R3
		34,5, // R5
		44,5, // G1
		54,5, // G3
		64,5, // G5
		74,5, // B1
		84,5  // B3
	>(block128);

	// we use the mode bit to get a free always-1 bit for the alpha channel
	Vec128 raw_ba16 = sse4_multigetbits_masked<
		89,5, // B4
		2,1,  // "A0"
		2,1,  // "A2"
		2,1,  // "A4"
		94,5, // B5
		2,1,  // "A1"
		2,1,  // "A3"
		2,1   // "A5"
	>(block128);

	// Use multiplies to handle the top bit replication and the down-scaling at
	// the same time (also used to expand our single alpha bits)
	const Vec128 downscale_mult = _mm_set1_epi16((1 << 8) + (1 << 3));
	const Vec128 downscale_mult_ba = _mm_setr_epi16(
		(1 << 8) + (1 << 3), 0x1fe, 0x1fe, 0x1fe,
		(1 << 8) + (1 << 3), 0x1fe, 0x1fe, 0x1fe
	);

	Vec128 lo16 = _mm_mulhi_epu16(raw_lo16, downscale_mult);
	Vec128 hi16 = _mm_mulhi_epu16(raw_hi16, downscale_mult);
	Vec128 ba16 = _mm_mulhi_epu16(raw_ba16, downscale_mult_ba);

	// Pack into our 8-bit values
	*out_lo8 = _mm_packus_epi16(lo16, ba16);
	*out_hi8 = _mm_packus_epi16(hi16, shuffle32<2,3,0,1>(ba16));
}

static RADFORCEINLINE Vec128 sse4_decode_mode2_inds(const Vec128 &block128, const BC67Partition * partition)
{
	// Figure out the position of the bit _after_ the final anchor bit.
	// This is in the same place before and after the anchor insertion.
	int top_begin = partition->anchor1 * 2 /* 2b inds */ + 2 /* _after_ anchor */ + 32 /* start of (unpacked) inds */;

	// mid_begin is the next-highest anchor, which has 1 bit missing after it
	int mid_begin = partition->anchor0 * 2 /* 2b inds */ + 2 /* _after_ anchor */ + 32 /* start of (unpacked) inds */ + 1 /* 1 bit missing above */;

	// the packed indices start at bit 99 = bit 35 into the top 64 bits;
	// we have 1 bit for the initial index, then one anchor, meaning the "bot" point starts at bit 36
	Vec128 index = sse4_insert_anchors_three_subset<36>(block128, top_begin, mid_begin);

	// With the 2-bit indices straighted out, convert them to lerp factors
	Vec128 lerpf8 = sse4_ind2b_to_lerpf<12>(index);

	return lerpf8;
}

static RADFORCEINLINE void sse4_decode_mode3_endpoints(Vec128 *out_lo16, Vec128 *out_hi16, const Vec128 &block128, const U8 * block_bits)
{
	// Grab all the endpoints, "low" (even-numbered ones) separate from "high" (odd)
	// we use the mode bit to get a free always-1 bit for the alpha channel
	Vec128 raw_lo16 = sse4_multigetbits_masked<
		10,7, // R0
		38,7, // G0
		66,7, // B0
		3,1,  // "A0"
		24,7, // R2
		52,7, // G2
		80,7, // B2
		3,1   // "A2"
	>(block128);

	Vec128 raw_hi16 = sse4_multigetbits_masked<
		17,7, // R1
		45,7, // G1
		73,7, // B1
		3,1,  // "A1"
		31,7, // R3
		59,7, // G3
		87,7, // B3
		3,1   // "A3"
	>(block128);

	// Use a multiply to handle down-shifting and also create 8 copies
	// each of our single alpha bits.
	const Vec128 downscale_mult = _mm_setr_epi16(
		(1<<8), (1<<8), (1<<8), 0x1fe,
		(1<<8), (1<<8), (1<<8), 0x1fe
	);

	Vec128 lo16 = _mm_mulhi_epu16(raw_lo16, downscale_mult);
	Vec128 hi16 = _mm_mulhi_epu16(raw_hi16, downscale_mult);

	// Handle the p-bits.
	Vec128 pbits_raw = shuffle32<2,3,2,3>(block128); // 32-bit lanes containing the pbits

	// use 64-bit compares to test whether the individual p-bits are set
	const Vec128 mask_pbit02 = _mm_setr_epi32((1<<30),0, 0,1);
	const Vec128 mask_pbit13 = _mm_setr_epi32(-2*(1<<30),0, 0,2);
	const Vec128 mask_lsb = _mm_set1_epi16(1);

	Vec128 pbit02 = _mm_cmpeq_epi64(_mm_and_si128(pbits_raw, mask_pbit02), mask_pbit02);
	Vec128 pbit13 = _mm_cmpeq_epi64(_mm_and_si128(pbits_raw, mask_pbit13), mask_pbit13);
	*out_lo16 = _mm_or_si128(lo16, _mm_and_si128(pbit02, mask_lsb));
	*out_hi16 = _mm_or_si128(hi16, _mm_and_si128(pbit13, mask_lsb));
}

static RADFORCEINLINE Vec128 sse4_decode_mode3_inds(const Vec128 &block128, const BC67Partition * partition)
{
	// Figure out the position of the bit _after_ the final anchor bit.
	// This is in the same place before and after the anchor insertion.
	int top_begin = partition->anchor0 * 2 /* 2b inds */ + 2 /* _after_ anchor */ + 32 /* start of (unpacked) inds */;
	Vec128 index = sse4_insert_anchors_two_subset<35>(block128, top_begin);

	// With the 2-bit indices straighted out, convert them to lerp factors
	Vec128 lerpf8 = sse4_ind2b_to_lerpf<12>(index);

	return lerpf8;
}

static RADFORCEINLINE Vec128 sse4_decode_mode4_endpoints(const Vec128 &block128)
{
	// Grab the endpoints
	Vec128 bitfields_masked = sse4_multigetbits_masked<
		8,5, // R0
		18,5, // G0
		28,5, // B0
		38,6, // A0
		13,5, // R1
		23,5, // G1
		33,5, // B1
		44,6  // A1
	>(block128);

	// Use multiplies to handle the top bit replication and the down-scaling at
	// the same time
	Vec128 endpoints16 = _mm_mulhi_epu16(bitfields_masked, _mm_setr_epi16(
		(1<<8) + (1<<3), (1<<8) + (1<<3), (1<<8) + (1<<3), (1<<8) + (1<<2),
		(1<<8) + (1<<3), (1<<8) + (1<<3), (1<<8) + (1<<3), (1<<8) + (1<<2)
	));

	return endpoints16;
}

static RADFORCEINLINE void sse4_decode_mode4_inds(U8 * lerpf_buf, const Vec128 &block128)
{
	// index0 is 2-bit/pixel; it crosses a 64-bit boundary which
	// requires some extra shuffling from us
	Vec128 index0_noanchor = _mm_srli_epi64(_mm_srli_si128(block128, 6), 2);

	// Insert the always-0 anchor bits at bit 1
	// this add shifts everything above bit 0 left by 1
	Vec128 index0 = _mm_add_epi32(index0_noanchor, _mm_and_si128(index0_noanchor, _mm_set1_epi32(-2)));

	// Turn into lerp factors
	Vec128 lerpf8_ind0 = sse4_ind2b_to_lerpf<0>(index0);

	// index1 is 3 bits/pixel which turns into a bigger production
	// but first we just need to insert the anchor bit which is the easy part:
	// first shift everything right by 1 and then do the add trick again
	// to shift the top part right back
	Vec128 index1_shifted = _mm_srli_epi64(block128, 1);
	Vec128 index1 = _mm_add_epi64(index1_shifted, _mm_and_si128(index1_shifted, _mm_setr_epi32(0, 0, -(4<<16), -1)));
	Vec128 lerpf8_ind1 = sse4_ind3b_to_lerpf(index1);

	// Pack them to memory, interleaved
	store128a(lerpf_buf +  0, _mm_unpacklo_epi8(lerpf8_ind0, lerpf8_ind1));
	store128a(lerpf_buf + 16, _mm_unpackhi_epi8(lerpf8_ind0, lerpf8_ind1));
}

static RADFORCEINLINE Vec128 sse4_decode_mode5_endpoints(const Vec128 &block128)
{
	// Grab the endpoints
	Vec128 bitfields_masked = sse4_multigetbits_masked<
		8,7, // R0
		22,7, // G0
		36,7, // B0
		50,8, // A0
		15,7, // R1
		29,7, // G1
		43,7, // B1
		58,8  // A1
	>(block128);

	// Use multiplies to handle the top bit replication and the down-scaling at
	// the same time
	Vec128 endpoints16 = _mm_mulhi_epu16(bitfields_masked, _mm_setr_epi16(
		(1<<8) + (1<<1), (1<<8) + (1<<1), (1<<8) + (1<<1), 1<<8,
		(1<<8) + (1<<1), (1<<8) + (1<<1), (1<<8) + (1<<1), 1<<8
	));

	return endpoints16;
}

static RADFORCEINLINE void sse4_decode_mode5_inds(U8 * lerpf_buf, const Vec128 &block128)
{
	// Grab the index bits; index0 goes into 32-bit lane 0, index1 into lane 1
	Vec128 index2b = _mm_unpackhi_epi32(_mm_srli_epi64(block128, 2), _mm_srli_epi64(block128, 33));

	// Insert the always-0 anchor bits at bit 1
	// this add shifts everything above bit 0 left by 1
	index2b = _mm_add_epi32(index2b, _mm_and_si128(index2b, _mm_set1_epi32(-2)));

	// Go straight from 2-bit indices to scaled lerp factors; this works out because the
	// structure of the lerp factor table for 2 bits is very simple.
	Vec128 lerpf8_ind0 = sse4_ind2b_to_lerpf<0>(index2b);
	Vec128 lerpf8_ind1 = sse4_ind2b_to_lerpf<4>(index2b);

	store128a(lerpf_buf +  0, _mm_unpacklo_epi8(lerpf8_ind0, lerpf8_ind1));
	store128a(lerpf_buf + 16, _mm_unpackhi_epi8(lerpf8_ind0, lerpf8_ind1));
}

// Returns 16-bit R0,G0,B0,A0, R1,G1,B1,A1
static RADFORCEINLINE Vec128 sse4_decode_mode6_endpoints(const Vec128 &block128)
{
	// Grab the endpoints
	Vec128 bitfields_aligned = sse4_multigetbits<
		7,7, // R0
		21,7, // G0
		35,7, // B0
		49,7, // A0
		14,7, // R1
		28,7, // G1
		42,7, // B1
		56,7  // A1
	>(block128);

	// Now we have the original 7-bit endpoints all shifted left by 9 in 16-bit lanes,
	// with garbage below. Shift right to get rid of the garbage.
	Vec128 endpoints7 = _mm_srli_epi16(bitfields_aligned, 9);

	// Shift left by 1 (by adding to self) to make space for the p-bits
	Vec128 endpoints_no_pbit = _mm_add_epi16(endpoints7, endpoints7);

	// Add in the p-bits.
	const Vec128 mask_pbits = _mm_setr_epi32(0,-2*(1<<30), 1,0);
	const Vec128 mask_lsb = _mm_set1_epi16(1);

	Vec128 pbits_set = _mm_cmpeq_epi64(_mm_and_si128(block128, mask_pbits), mask_pbits);
	Vec128 endpoints = _mm_or_si128(endpoints_no_pbit, _mm_and_si128(pbits_set, mask_lsb));

	return endpoints;
}

static RADFORCEINLINE Vec128 sse4_decode_mode6_inds(const Vec128 &block128)
{
	// Indices: need to insert the anchor bit (which works out to shifting the first
	// 3 real index bits right by 1)
	Vec128 index4_lo3 = _mm_and_si128(block128, _mm_setr_epi32(0, 0, 7<<1, 0));
	Vec128 index4_rest = _mm_and_si128(block128, _mm_setr_epi32(0, 0, -16, -1));
	Vec128 index4_with_anchor = _mm_or_si128(index4_rest, _mm_srli_epi64(index4_lo3, 1));

	// Turn indices into lerp factors
	Vec128 lerpf8 = sse4_ind4b_to_lerpf(index4_with_anchor);

	return lerpf8;
}

static RADFORCEINLINE void sse4_decode_mode7_endpoints(Vec128 * out_lo16, Vec128 * out_hi16, const Vec128 &block128, const U8 * block_bits)
{
	// Grab all the endpoints, "low" (even-numbered ones) seperate from "high" (odd)
	Vec128 raw_lo16 = sse4_multigetbits_masked<
		14,5, // R0
		34,5, // G0
		54,5, // B0
		74,5, // A0
		24,5, // R2
		44,5, // G2
		64,5, // B2
		84,5  // A2
	>(block128);

	Vec128 raw_hi16 = sse4_multigetbits_masked<
		19,5, // R1
		39,5, // G1
		59,5, // B1
		79,5, // A1
		29,5, // R3
		49,5, // G3
		69,5, // B3
		89,5  // A3
	>(block128);

	// Use multiplies to handle the top bit replication and the down-scaling at
	// the same time
	const Vec128 downscale_mult = _mm_set1_epi16((1<<8) + (1<<2));
	Vec128 lo16 = _mm_mulhi_epu16(raw_lo16, downscale_mult);
	Vec128 hi16 = _mm_mulhi_epu16(raw_hi16, downscale_mult);

	// Handle the p-bits.
	Vec128 pbits_raw = shuffle32<2,3,2,3>(block128); // 32-bit lanes containing the pbits

	// use 64-bit compares to test whether the individual p-bits are set
	const Vec128 mask_pbit02 = _mm_setr_epi32((1<<30),0, 0,1);
	const Vec128 mask_pbit13 = _mm_setr_epi32(-2*(1<<30),0, 0,2);
	const Vec128 mask_out_pbit = _mm_set1_epi16(4);

	Vec128 pbit02 = _mm_cmpeq_epi64(_mm_and_si128(pbits_raw, mask_pbit02), mask_pbit02);
	Vec128 pbit13 = _mm_cmpeq_epi64(_mm_and_si128(pbits_raw, mask_pbit13), mask_pbit13);
	*out_lo16 = _mm_or_si128(lo16, _mm_and_si128(pbit02, mask_out_pbit));
	*out_hi16 = _mm_or_si128(hi16, _mm_and_si128(pbit13, mask_out_pbit));
}

static RADFORCEINLINE Vec128 sse4_decode_mode7_inds(const Vec128 &block128, const BC67Partition * partition)
{
	// Figure out the position of the bit _after_ the final anchor bit.
	// This is in the same place before and after the anchor insertion.
	int top_begin = partition->anchor0 * 2 /* 2b inds */ + 2 /* _after_ anchor */ + 32 /* start of (unpacked) inds */;
	Vec128 index = sse4_insert_anchors_two_subset<35>(block128, top_begin);

	// With the 2-bit indices straightened out, convert them to lerp factors
	Vec128 lerpf8 = sse4_ind2b_to_lerpf<12>(index);

	return lerpf8;
}

