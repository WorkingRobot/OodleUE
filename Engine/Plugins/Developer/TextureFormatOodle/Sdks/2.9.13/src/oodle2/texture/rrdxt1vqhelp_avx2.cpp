// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetavx2

// We _MUST NOT_ allow the compiler to contract mul-add operations to FMAs;
// this changes the results between AVX2 and SSE4 versions which we don't allow!
#include "nocontract.h"

#include "rrdxt1vqhelp.h"
#include "rrdxt1vqhelp.inl"
#include "rrdxtcblock_avx2.inl"
#include "vec256.inl"
#include "perceptualactivity.h"
#include "perceptualactivity.inl"

#ifdef DO_BUILD_AVX2

OODLE_NS_START

namespace internal {

U32 ColorBlock4x4_ComputeSSD_RGBA_AVX2(const rrColorBlock4x4 & lhs,const rrColorBlock4x4 & rhs)
{
	const U8 *lhs_pixels = (const U8 *)lhs.colors;
	const U8 *rhs_pixels = (const U8 *)rhs.colors;

	Vec256_U32 squares32 = Vec256_U32::ssd4(Vec256_U8::loadu(lhs_pixels), Vec256_U8::loadu(rhs_pixels));
	squares32 += Vec256_U32::ssd4(Vec256_U8::loadu(lhs_pixels + 32), Vec256_U8::loadu(rhs_pixels + 32));
	return reduce_add(squares32);
}

static RADFORCEINLINE void Transpose4x4_32(Vec256 & a, Vec256 & b, Vec256 & c, Vec256 & d)
{
	// First pass
	Vec256 s0 = _mm256_unpacklo_epi32(a, c);
	Vec256 s1 = _mm256_unpacklo_epi32(b, d);
	Vec256 s2 = _mm256_unpackhi_epi32(a, c);
	Vec256 s3 = _mm256_unpackhi_epi32(b, d);

	// Second pass
	a = _mm256_unpacklo_epi32(s0, s1);
	b = _mm256_unpackhi_epi32(s0, s1);
	c = _mm256_unpacklo_epi32(s2, s3);
	d = _mm256_unpackhi_epi32(s2, s3);
}

static RADFORCEINLINE Vec256 Load256_DeintEvenOdd(const void * src)
{
	Vec256 data = load256u(src);
	return _mm256_permutevar8x32_epi32(data, _mm256_setr_epi32(0,2,4,6, 1,3,5,7));
}

static RADFORCEINLINE Vec256 AnyIndexD_AVX2_Accumulate(const Vec256 & v_ssd, const Vec256 & errs, const Vec256 & v_inds)
{
	Vec256 vals = _mm256_permutevar8x32_epi32(errs, v_inds);
	return _mm256_add_epi32(v_ssd, vals);
}

S32 AnyIndexD_batch_lookup_AVX2(S32 out_d[], const AnyIndexD * aid, const U32 in_indices[], int count32)
{
	const SINTa count = count32;
	if ( count < 1 )
		return 0x7fffffff;

	// Load the SSD scores into regs up front (these names don't match the values yet, but they will soon)
	Vec256 err01 = Load256_DeintEvenOdd(&aid->ssd[0][0]); // ssd[0][0], ssd[0][2], ssd[0][4], ssd[0][6] | ssd[0][1], ssd[0][3], ssd[0][5], ssd[0][7]
	Vec256 err23 = Load256_DeintEvenOdd(&aid->ssd[1][0]); // ssd[1][0], ssd[1][2], ssd[1][4], ssd[1][6] | ssd[1][1], ssd[1][3], ssd[1][5], ssd[1][7]
	Vec256 err45 = Load256_DeintEvenOdd(&aid->ssd[2][0]);
	Vec256 err67 = Load256_DeintEvenOdd(&aid->ssd[3][0]);
	Vec256 err89 = Load256_DeintEvenOdd(&aid->ssd[0][8]);
	Vec256 errab = Load256_DeintEvenOdd(&aid->ssd[1][8]);
	Vec256 errcd = Load256_DeintEvenOdd(&aid->ssd[2][8]);
	Vec256 erref = Load256_DeintEvenOdd(&aid->ssd[3][8]);

	// Finish transpose from [index][pixel] layout to [pixel][index] (making the names right)
	Transpose4x4_32(err01, err23, err45, err67);
	Transpose4x4_32(err89, errab, errcd, erref);

	// Start of last aligned block of 4 candiates. Note we established count >= 1.
	SINTa last_aligned = (count - 1) & ~3;

	// Do the last (partial) iteration first and count down from there
	RAD_ALIGN(S32, tail_buf[4], 16);
	SINTa i = last_aligned;
	S32 * dest_ptr = tail_buf;
	Vec256 v_four_inds = broadcast128_256(_mm_setr_epi32(
		in_indices[i + 0],
		in_indices[RR_MIN(i + 1, count - 1)],
		in_indices[RR_MIN(i + 2, count - 1)],
		in_indices[RR_MIN(i + 3, count - 1)]
	));
	Vec128_S32 v_min_err { 0x7fffffff };

	for (;;) // Loop condition inside, loop structure is a bit unusual
	{
		// Low half of vector handles even-column pixels, high half handles odd ones
		// Pre-select the appropriate index bits, clear irrelevant bits to 0
		v_four_inds = _mm256_and_si256(_mm256_srlv_epi32(v_four_inds, _mm256_setr_epi32(0,0,0,0, 2,2,2,2)), _mm256_set1_epi8(0x33));

		// Set the high bit in the odd-lane inds so VPERMD grabs the values from the second half of
		// the error vectors, which contains the error values for the odd-column pixels
		v_four_inds = _mm256_or_si256(v_four_inds, _mm256_setr_epi32(0,0,0,0, 0x44444444,0x44444444,0x44444444,0x44444444));

		// Grab the lanes with the relevant errors for each pixel using VPERMD and keep accumulating
		Vec256 v_ssd;

		v_ssd = _mm256_permutevar8x32_epi32(err01, v_four_inds); v_four_inds = _mm256_srli_epi32(v_four_inds, 4);
		v_ssd = AnyIndexD_AVX2_Accumulate(v_ssd, err23, v_four_inds); v_four_inds = _mm256_srli_epi32(v_four_inds, 4);
		v_ssd = AnyIndexD_AVX2_Accumulate(v_ssd, err45, v_four_inds); v_four_inds = _mm256_srli_epi32(v_four_inds, 4);
		v_ssd = AnyIndexD_AVX2_Accumulate(v_ssd, err67, v_four_inds); v_four_inds = _mm256_srli_epi32(v_four_inds, 4);
		v_ssd = AnyIndexD_AVX2_Accumulate(v_ssd, err89, v_four_inds); v_four_inds = _mm256_srli_epi32(v_four_inds, 4);
		v_ssd = AnyIndexD_AVX2_Accumulate(v_ssd, errab, v_four_inds); v_four_inds = _mm256_srli_epi32(v_four_inds, 4);
		v_ssd = AnyIndexD_AVX2_Accumulate(v_ssd, errcd, v_four_inds); v_four_inds = _mm256_srli_epi32(v_four_inds, 4);
		v_ssd = AnyIndexD_AVX2_Accumulate(v_ssd, erref, v_four_inds);

		// Reduce from 256b down to 128b; top half contains the results for
		// the odd columns, add those now
		Vec128 v_merged = _mm_add_epi32(lo_half(v_ssd), hi_half(v_ssd));
		v_min_err = v_min_err.min(Vec128_S32(v_merged));
		store128u(dest_ptr, v_merged);

		// Advance and set up next iteration
		i -= 4;
		if ( i < 0 )
			break;

		dest_ptr = out_d + i;
		v_four_inds = broadcast128_256(load128u(in_indices + i));
	}

	// Copy last few results to final destination
	memcpy(out_d + last_aligned, tail_buf, (count - last_aligned) * sizeof(S32));

	return reduce_min(v_min_err);
}

static RADFORCEINLINE Vec256 split64_and_reorder(Vec256 x)
{
	// Put bits [27:0] into the low DWord of each 64-bit value,
	// bits [55:28] into the high DWord, and discard the rest.
	// (56 bits is more than enough for us here.)
	const Vec256 mask_lo = _mm256_setr_epi32((1<<28)-1,0, (1<<28)-1,0, (1<<28)-1,0, (1<<28)-1,0);

	const Vec256 lo_part = _mm256_and_si256(mask_lo, x);
	const Vec256 hi_part = _mm256_andnot_si256(mask_lo, x);

	const Vec256 split = _mm256_or_si256(lo_part, _mm256_slli_epi64(hi_part, 4));

	// Now group all the low bits together in the first half of the vector,
	// and the high bits in the second half.
	const Vec256 reordered = _mm256_permutevar8x32_epi32(split, _mm256_setr_epi32(0,2,4,6, 1,3,5,7));

	return reordered;
}

// Loads 8 64-bit values and splits into low/high "halves" (not quite)
// at the 28-bit point (the top 8 bits of the value are discarded)
static RADFORCEINLINE void load_and_split(Vec256 & out_lo, Vec256 & out_hi, const U64 * values)
{
	// Load the error values
	const Vec256 val64_0123 = load256u(values + 0);
	const Vec256 val64_4567 = load256u(values + 4);

	// Perform the split and separate the low/high halves
	const Vec256 splitval_0123 = split64_and_reorder(val64_0123);
	const Vec256 splitval_4567 = split64_and_reorder(val64_4567);

	// Now all we need to do is recombine the halves
	out_lo = _mm256_permute2x128_si256(splitval_0123, splitval_4567, 0x20); // low halves of both
	out_hi = _mm256_permute2x128_si256(splitval_0123, splitval_4567, 0x31); // high halves of both
}

// this takes indices in a special form because otherwise we're dominated by shuffling the input
void AnyIndex8D64_batch_lookup_AVX2(S64 out_d[], const AnyIndex8D64 * aid, const U8 *indices_scaled_interleaved, int count)
{
	RR_ASSERT((count & 7) == 0);
	memset(out_d, 0, count*sizeof(*out_d)); // 0 memory since we accumulate there

	// to limit registers used, we process 4 pixels per pass
	for (int pix=0; pix < 16; pix += 4)
	{
		const U8 *pix_indices = indices_scaled_interleaved + 8*pix;

		// We're working on 8 blocks at a time
		// pixels are named a,b,c,d, blocks 0-7
		Vec256 erra_lo, erra_hi;
		Vec256 errb_lo, errb_hi;
		Vec256 errc_lo, errc_hi;
		Vec256 errd_lo, errd_hi;

		// We accumulate low and high "halves" (actually 28b numbers)
		// separately since we're working with DWord shuffles and that
		// is the more efficient way to go here.
		//
		// 28b numbers let us sum them over 16 pixels without risking
		// overflows. We finish up with a final pass to complete the
		// sums below.
		load_and_split(erra_lo, erra_hi, aid->ssdx[pix + 0]);
		load_and_split(errb_lo, errb_hi, aid->ssdx[pix + 1]);
		load_and_split(errc_lo, errc_hi, aid->ssdx[pix + 2]);
		load_and_split(errd_lo, errd_hi, aid->ssdx[pix + 3]);

		// In the common case that all the "hi" vals are 0, we can use a simpler loop
		Vec256 any_hi_ab = _mm256_or_si256(erra_hi, errb_hi);
		Vec256 any_hi_cd = _mm256_or_si256(errc_hi, errd_hi);
		Vec256 any_hi = _mm256_or_si256(any_hi_ab, any_hi_cd);

		if (_mm256_testz_si256(any_hi, any_hi)) // all hi portions 0: can use narrow loop
		{
			for (int block = 0; block < count; block += 8)
			{
				// Load indices for pixels a,b,c,d in blocks 0-7 pre-shuffled by caller
				// order is (as bytes) 0a,0b,0c,0d, 4a,4b,4c,4d, 1[abcd], 5[abcd], 2[abcd], ...
				Vec256 ind_bytes = load256u(pix_indices + block*16);

				// Do the index lookups and sum
				Vec256 ssd_lo;

				// Indices are pre-shifted by 1, fix that so we have the indices in the low bits where VPERMD needs them
				ind_bytes = _mm256_srli_epi32(ind_bytes, 1); // ind_a
				ssd_lo = _mm256_permutevar8x32_epi32(erra_lo, ind_bytes);

				ind_bytes = _mm256_srli_epi32(ind_bytes, 8); // ind_b
				ssd_lo = _mm256_add_epi32(ssd_lo, _mm256_permutevar8x32_epi32(errb_lo, ind_bytes));

				ind_bytes = _mm256_srli_epi32(ind_bytes, 8); // ind_c
				ssd_lo = _mm256_add_epi32(ssd_lo, _mm256_permutevar8x32_epi32(errc_lo, ind_bytes));

				ind_bytes = _mm256_srli_epi32(ind_bytes, 8); // ind_d
				ssd_lo = _mm256_add_epi32(ssd_lo, _mm256_permutevar8x32_epi32(errd_lo, ind_bytes));

				// Accumulate in memory
				ssd_lo = _mm256_add_epi32(ssd_lo, load256u(out_d + block + 0));
				store256u(out_d + block + 0, ssd_lo);
			}
		}
		else // general case
		{
			for (int block = 0; block < count; block += 8)
			{
				// Load indices for pixels a,b,c,d in blocks 0-7 pre-shuffled by caller
				// order is (as bytes) 0a,0b,0c,0d, 4a,4b,4c,4d, 1[abcd], 5[abcd], 2[abcd], ...
				Vec256 ind_bytes = load256u(pix_indices + block*16);

				// Do the index lookups for the lo/hi split halves and sum
				Vec256 ssd_lo, ssd_hi;

				// Indices are pre-shifted by 1, fix that so we have the indices in the low bits where VPERMD needs them
				ind_bytes = _mm256_srli_epi32(ind_bytes, 1); // ind_a
				ssd_lo = _mm256_permutevar8x32_epi32(erra_lo, ind_bytes);
				ssd_hi = _mm256_permutevar8x32_epi32(erra_hi, ind_bytes);

				ind_bytes = _mm256_srli_epi32(ind_bytes, 8); // ind_b
				ssd_lo = _mm256_add_epi32(ssd_lo, _mm256_permutevar8x32_epi32(errb_lo, ind_bytes));
				ssd_hi = _mm256_add_epi32(ssd_hi, _mm256_permutevar8x32_epi32(errb_hi, ind_bytes));

				ind_bytes = _mm256_srli_epi32(ind_bytes, 8); // ind_c
				ssd_lo = _mm256_add_epi32(ssd_lo, _mm256_permutevar8x32_epi32(errc_lo, ind_bytes));
				ssd_hi = _mm256_add_epi32(ssd_hi, _mm256_permutevar8x32_epi32(errc_hi, ind_bytes));

				ind_bytes = _mm256_srli_epi32(ind_bytes, 8); // ind_d
				ssd_lo = _mm256_add_epi32(ssd_lo, _mm256_permutevar8x32_epi32(errd_lo, ind_bytes));
				ssd_hi = _mm256_add_epi32(ssd_hi, _mm256_permutevar8x32_epi32(errd_hi, ind_bytes));

				// Accumulate in memory
				ssd_lo = _mm256_add_epi32(ssd_lo, load256u(out_d + block + 0));
				ssd_hi = _mm256_add_epi32(ssd_hi, load256u(out_d + block + 4));
				store256u(out_d + block + 0, ssd_lo);
				store256u(out_d + block + 4, ssd_hi);
			}
		}
	}

	// Final pass that combines the two halves into regular 64-bit numbers
	for (int block = 0; block < count; block += 8)
	{
		const Vec256 ssd_lo = load256u(out_d + block + 0);
		const Vec256 ssd_hi = load256u(out_d + block + 4);
		const Vec256 mask_even = _mm256_setr_epi32(-1,0, -1,0, -1,0, -1,0);

		const Vec256 ssd_lo_evn = _mm256_and_si256(ssd_lo, mask_even);
		const Vec256 ssd_hi_evn = _mm256_and_si256(ssd_hi, mask_even);
		const Vec256 ssd64_0123 = _mm256_add_epi64(ssd_lo_evn, _mm256_slli_epi64(ssd_hi_evn, 28));

		const Vec256 ssd_lo_odd = _mm256_srli_epi64(ssd_lo, 32);
		const Vec256 ssd_hi_odd = _mm256_srli_epi64(ssd_hi, 32);
		const Vec256 ssd64_4567 = _mm256_add_epi64(ssd_lo_odd, _mm256_slli_epi64(ssd_hi_odd, 28));

		store256u(out_d + block + 0, ssd64_0123);
		store256u(out_d + block + 4, ssd64_4567);
	}
}

// block SADs without complete reduction
static RADFORCEINLINE Vec256 block_SAD_nored(const rrColorBlock4x4 * psrc, const Vec256 & colors0, const Vec256 & colors1)
{
	// Sum across the two row pairs, but hold off on the lo/hi half or even/odd row reductions
	Vec256 t = _mm256_sad_epu8(load256u(psrc->colors), colors0);
	t = _mm256_add_epi16(t, _mm256_sad_epu8(load256u(psrc->colors + 8), colors1));
	return t;
}

static RADFORCEINLINE Vec128 compute_chunk_sads_indirect_kernel(const Vec256 colors0, const Vec256 colors1, const rrColorBlock4x4 * pair_colors, const U16 * pInds)
{
	Vec256 s0 = block_SAD_nored(pair_colors + pInds[0], colors0, colors1);
	Vec256 s1 = block_SAD_nored(pair_colors + pInds[1], colors0, colors1);
	Vec256 s2 = block_SAD_nored(pair_colors + pInds[2], colors0, colors1);
	Vec256 s3 = block_SAD_nored(pair_colors + pInds[3], colors0, colors1);

	// Merge the row sums so we have 16-bit row sums for block 0, 1, 2, 3 in the first
	// four 16-bit lanes, and then the pattern repeats for the other 4.
	s0 = _mm256_or_si256(s0, _mm256_slli_epi64(s1, 16));
	s2 = _mm256_or_si256(s2, _mm256_slli_epi64(s3, 16));
	s0 = _mm256_or_si256(s0, _mm256_slli_epi64(s2, 32));

	// Add up the low and high halves of the 256b vector (even and odd rows, respectively)
	Vec128 sum = _mm_add_epi16(lo_half(s0), hi_half(s0));

	// Rearrange so the two 16-bit values for each block are adjacent
	sum = _mm_shuffle_epi8(sum, _mm_setr_epi8(0,1,8,9, 2,3,10,11, 4,5,12,13, 6,7,14,15));

	// Add up the two halves of each of the original 128b (now adjacent 16b lanes) and store
	sum = _mm_madd_epi16(sum, _mm_set1_epi16(1));

	return sum;
}

void compute_chunk_sads_indirect_AVX2(int chunk_sads[], int count32, const U16 indices[], const rrColorBlock4x4 * pair_colors, const rrColorBlock4x4 & rColors)
{
	SINTa count = count32;
	if ( count < 1 )
		return;

	const Vec256 colors0 = load256u(rColors.colors +  0);
	const Vec256 colors1 = load256u(rColors.colors +  8);

	// Do the last (partial) iteration first and count down from there
	U16 last_inds[4];
	SINTa i = (count - 1) & ~3; // start of last aligned group of 4

	Vec128 m_lane_id = _mm_add_epi32(_mm_set1_epi32((int)i), _mm_setr_epi32(0, 1, 2, 3));
	Vec128 m_last_active = _mm_cmpgt_epi32(_mm_set1_epi32(count32), m_lane_id);

	last_inds[0] = indices[i + 0];
	last_inds[1] = indices[RR_MIN(i + 1, count - 1)];
	last_inds[2] = indices[RR_MIN(i + 2, count - 1)];
	last_inds[3] = indices[RR_MIN(i + 3, count - 1)];

	// Final partial iteration
	Vec128 final_sum = compute_chunk_sads_indirect_kernel(colors0, colors1, pair_colors, last_inds);
	_mm_maskstore_epi32(chunk_sads + i, m_last_active, final_sum);

	// Other full iterations
	for (;;)
	{
		i -= 4;
		if ( i < 0 )
			break;

		Vec128 sum = compute_chunk_sads_indirect_kernel(colors0, colors1, pair_colors, indices + i);
		store128u(chunk_sads + i, sum);
	}
}

static RADFORCEINLINE Vec256 filter_by_index_sad_kernel_AVX2(const Vec256 & ref_inds, const U8 * inds_base)
{
	const Vec256 c_even_odd_merge = _mm256_setr_epi32(0,4, 1,5, 2,6, 3,7);

	// NOTE(fg): on SKL derivatives, almost everything here hits the same port (port 5)
	// since the shuffles (used for "combine" and the packs) and PSDADBW are on the
	// same port. That's unfortunate but I don't see a good way around it for this
	// particular loop.

	// Compute 8 blocks worth of index SADs
	const Vec256 s0 = _mm256_sad_epu8(ref_inds, load256u(inds_base + 0*32));
	const Vec256 s1 = _mm256_sad_epu8(ref_inds, load256u(inds_base + 1*32));
	const Vec256 s2 = _mm256_sad_epu8(ref_inds, load256u(inds_base + 2*32));
	const Vec256 s3 = _mm256_sad_epu8(ref_inds, load256u(inds_base + 3*32));

	// merge the row sums (64b -> 32b reduction, then 32b->16b)
	const Vec256 s01 = _mm256_packs_epi32(s0, s1);
	const Vec256 s23 = _mm256_packs_epi32(s2, s3);
	const Vec256 s0123 = _mm256_packs_epi32(s01, s23);

	// add up the two halves of each of the original 128b (now adjacent 16b lanes) and store
	const Vec256 sad32_perm = _mm256_madd_epi16(s0123, _mm256_set1_epi16(1));

	// Due to how the packs work, we now have data separated into SADs for even-index blocks
	// in the low 128b and for odd-index blocks in the high 128b; put that back into natural
	// order.
	const Vec256 sad32s = _mm256_permutevar8x32_epi32(sad32_perm, c_even_odd_merge);

	return sad32s;
}

int filter_by_index_sad_AVX2(int positions[], int count32, const U8 * index_batch, const U8 ref_inds_ptr[16], int sad_thresh)
{
	const Vec256 v_sad_thresh = _mm256_set1_epi32(sad_thresh);
	const Vec256 ref_inds = broadcast128_256(load128u(ref_inds_ptr));
	const SINTa count = count32;
	SINTa out_count = 0;

	const Vec256 c_lane_shifts = _mm256_setr_epi32(0,4,8,12, 16,20,24,28);
	Vec256 base_index = _mm256_setzero_si256();

	// Process groups of 8 blocks at once
	const U8 * inds_base = index_batch;
	const U8 * inds_aligned_end = inds_base + (count & ~7) * 16;

	for (; inds_base != inds_aligned_end; inds_base += 8*16)
	{
		// Run the kernel
		const Vec256 sad32s = filter_by_index_sad_kernel_AVX2(ref_inds, inds_base);

		// store IDs of lanes that have values below sad_thresh
		const Vec256 below = _mm256_cmpgt_epi32(v_sad_thresh, sad32s);
		U32 below_mask = _mm256_movemask_ps(_mm256_castsi256_ps(below));

		// use a table lookup to get the lane IDs for active lanes
		// at this point, out_count <= i < (count & ~7), so we never store out of bounds
		Vec256 table_val = _mm256_broadcastd_epi32(load32u(&c_active_lane_table8[below_mask]));
		Vec256 lane_ids = _mm256_and_si256(_mm256_srlv_epi32(table_val, c_lane_shifts), _mm256_set1_epi32(15));
		store256u(positions + out_count, _mm256_add_epi32(lane_ids, base_index));
		out_count += _mm_popcnt_u32(below_mask);

		base_index = _mm256_add_epi32(base_index, _mm256_set1_epi32(8));
	}

	SINTa i = (inds_base - index_batch) >> 4;

	// tail
	for (; i < count; ++i)
	{
		const Vec128 sad = _mm_sad_epu8(load128u(index_batch + i*16), lo_half(ref_inds));

		// Sum the two halves
		const Vec128 sum = _mm_add_epi16(sad, shuffle32<2,3,0,1>(sad));

		positions[out_count] = (int)i;
		out_count += _mm_cvtsi128_si32(sum) < sad_thresh;
	}

	return (int)out_count;
}

static Vec256 dist_to_bbox_AVX2(const Vec256 & v, const Vec256 & bbox_lo, const Vec256 & bbox_hi)
{
	Vec256 axial_dist8 = _mm256_add_epi8(_mm256_subs_epu8(bbox_lo, v), _mm256_subs_epu8(v, bbox_hi));

	Vec256 even_dist16 = _mm256_and_si256(axial_dist8, _mm256_set1_epi16(0xff));
	Vec256 odd_dist16 = _mm256_srli_epi16(axial_dist8, 8);

	Vec256 dots1 = _mm256_madd_epi16(even_dist16, even_dist16);
	Vec256 dots2 = _mm256_madd_epi16(odd_dist16, odd_dist16);

	return _mm256_add_epi32(dots1, dots2);
}

int filter_endpoint_color_bbox_AVX2(
	int * dest_inds,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColor32BGRA bbox[2],
	U32 bbox_min_d, bool require_3c)
{
	SINTa count = count32;
	SINTa out_count = 0;
	SINTa i = 0;

	RR_ASSERT(count < 65536);

	Vec256 bbox_lo = _mm256_set1_epi32(bbox[0].dw);
	Vec256 bbox_hi = _mm256_set1_epi32(bbox[1].dw);
	Vec256 lane_ids = _mm256_setr_epi32(0, 1<<16, 2<<16, 3<<16, 4<<16, 5<<16, 6<<16, 7<<16);
	Vec256 vec_min_d = _mm256_set1_epi32(bbox_min_d + 1);
	Vec256 mask_require_3c = _mm256_set1_epi32(require_3c ? -1 : 0);

	for (; i < (count & ~7); i += 8)
	{
		// Load and unpack 8 endpoints
		// these are full palettes, and even though we only care about the first two entries,
		// 128-bit loads let us do this more efficiently
		Vec256 ep_in04 = combine(load128u(vqendpoints[inds[i + 0]].palette), load128u(vqendpoints[inds[i + 4]].palette));
		Vec256 ep_in15 = combine(load128u(vqendpoints[inds[i + 1]].palette), load128u(vqendpoints[inds[i + 5]].palette));
		Vec256 ep_in26 = combine(load128u(vqendpoints[inds[i + 2]].palette), load128u(vqendpoints[inds[i + 6]].palette));
		Vec256 ep_in37 = combine(load128u(vqendpoints[inds[i + 3]].palette), load128u(vqendpoints[inds[i + 7]].palette));

		// Transpose into 2 8-vectors of (endpoint0,endpoint1)
		Vec256 xpose0 = _mm256_unpacklo_epi32(ep_in04, ep_in26); // (blk[0].ep0, blk[2].ep0, blk[0].ep1, blk[2].ep1) and same +4 in high half
		Vec256 xpose1 = _mm256_unpacklo_epi32(ep_in15, ep_in37); // (blk[1].ep0, blk[3].ep0, blk[1].ep1, blk[3].ep1) and same +4 in high half

		Vec256 ep0 = _mm256_unpacklo_epi32(xpose0, xpose1); // all ep0s
		Vec256 ep1 = _mm256_unpackhi_epi32(xpose0, xpose1); // all ep1s
		Vec256 avg = _mm256_avg_epu8(ep0, ep1);

		// Compute distances to bbox
		Vec256 dist_to_bbox_ep0 = dist_to_bbox_AVX2(ep0, bbox_lo, bbox_hi);
		Vec256 dist_to_bbox_ep1 = dist_to_bbox_AVX2(ep1, bbox_lo, bbox_hi);
		Vec256 dist_to_bbox_avg = dist_to_bbox_AVX2(avg, bbox_lo, bbox_hi);

		// We want to reject the candidate if the minimum of all 3 distances is > min_d
		// i.e. accept if min_dist <= min_d <=> min_d + 1 > min_dist
		Vec256 min_dist = _mm256_min_epi32(dist_to_bbox_ep0, dist_to_bbox_ep1);
		min_dist = _mm256_min_epi32(min_dist, dist_to_bbox_avg);

		// vec_min_d > min_dist -> accept!
		Vec256 distance_accept = _mm256_cmpgt_epi32(vec_min_d, min_dist);

		// Block mode test: if require_3c is set and the block selects 4-color mode, reject it.
		// 4-color mode is when ep0 > ep1 in the packed 16-bit representation; because unpacking
		// the color endpoints is strictly monotonic and order-preserving, we can compare
		// ep0 to ep1. We would like an unsigned compare, however both ep0 and ep1 always have
		// their alpha components set to 0xff; this makes them appear both negative, but they
		// will still order correctly under a signed compare. (The cases that give incorrect
		// results with the wrong type of compare is when one value is negative and the other is
		// not.)
		Vec256 block_4c = _mm256_cmpgt_epi32(ep0, ep1); // ep0 > ep1 selects 4c mode (see notes above)
		Vec256 mode_reject = _mm256_and_si256(block_4c, mask_require_3c);

		// Put indices into low half of output int,
		// filtered i in high half
		Vec256 indices = load256u(inds + i);
		Vec256 values = _mm256_or_si256(indices, lane_ids);

		// Block is active if distance_accept and no mode_reject
		Vec256 active = _mm256_andnot_si256(mode_reject, distance_accept);
		U32 active_mask = _mm256_movemask_ps(_mm256_castsi256_ps(active));

		// use a table lookup to get the lane IDs for active lanes
		// and shift to grab the correct values in each lane
		Vec256 table_val = _mm256_broadcastd_epi32(load32u(&c_active_lane_table8[active_mask]));
		Vec256 active_lane_ids = _mm256_srlv_epi32(table_val, _mm256_setr_epi32(0,4,8,12, 16,20,24,28));

		// at this point, out_count <= i, and we have enough space to store a full 8 lanes, so this is in
		// bounds
		Vec256 compacted = _mm256_permutevar8x32_epi32(values, active_lane_ids);
		store256u(dest_inds + out_count, compacted);
		out_count += _mm_popcnt_u32(active_mask);
		lane_ids = _mm256_add_epi32(lane_ids, _mm256_set1_epi32(8<<16));
	}

	// tail
	for (; i < count; i++)
	{
		int idx = inds[i];
		const rrDXT1_VQ_Entry & vqendpoint = vqendpoints[idx];

		// Block type check (always check as if in alpha pal mode, callers sets up
		// require_3c appropriately)
		bool vqendpoint_is4c = DXT1_Is4Color(vqendpoint.dw,rrDXT1PaletteMode_Alpha);

		bool rejected = reject_endpoints_color_bbox(vqendpoint.palette,bbox,bbox_min_d);
		rejected |= vqendpoint_is4c & require_3c;

		dest_inds[out_count] = (int) ((i << 16) | idx);
		out_count += rejected ? 0 : 1;
	}

	return (int)out_count;
}

static RADFORCEINLINE __m256 batch_VQD_sum_along_col(const __m256 weighted_errs[], const Vec256 & inds)
{
	__m256 sum;

	sum = _mm256_permutevar8x32_ps(weighted_errs[ 0], inds);
	sum = _mm256_add_ps(sum, _mm256_permutevar8x32_ps(weighted_errs[ 4], _mm256_srli_epi32(inds,  8)));
	sum = _mm256_add_ps(sum, _mm256_permutevar8x32_ps(weighted_errs[ 8], _mm256_srli_epi32(inds, 16)));
	sum = _mm256_add_ps(sum, _mm256_permutevar8x32_ps(weighted_errs[12], _mm256_srli_epi32(inds, 24)));

	return sum;
}

void batch_compute_VQDs_AVX2(
	F32 * dest_vqd,
	const rrDXT1_VQ_Entry * vqindices, const int * inds, int count32,
	const rrColorBlock4x4 & colors,
	const rrColor32BGRA first_palette[4],
	const rrColor32BGRA second_palette[4],
	const SingleFloatBlock4x4 & activity)
{
	const SINTa count = count32;

	if ( count < 1 )
		return;

	// Precompute weighted SSDs for all 4 possible choices of index from either palette
	// at all of the 16 source pixels
	Vec256 pal_vec = combine(load128u(first_palette), load128u(second_palette));
	__m256 weighted_errs[16]; // lanes 0-3 are error from first_palette, lanes 4-7 are second_palette
	RAD_ALIGN(F32, tail_buf[8], 32);
	SINTa i;

	for (i = 0; i < 16; ++i)
	{
		// Broadcast pixel to all 32-bit lanes
		Vec256 pixel = _mm256_set1_epi32(colors.colors[i].dw);

		// Compute the absolute differences in 8 bits and form
		// 16-bit differences for even/odd channel pairs
		Vec256 sub8 = _mm256_or_si256( _mm256_subs_epu8(pixel, pal_vec), _mm256_subs_epu8(pal_vec,pixel) );
		Vec256 sub16_1 = _mm256_and_si256(sub8, _mm256_set1_epi16(0xff)); // 16-bit: R, B, R, B, ...
		Vec256 sub16_2 = _mm256_srli_epi16(sub8, 8); // 16-bit: G, A, G, A, ...

		// this squares and horizontally adds pairs
		//	we go from 16 bits * 4 colors channels * 4 pixels
		//	-> 32 bits with {R+B} (squares32_1), {G+A} (squares32_2)
		Vec256 squares32_1 = _mm256_madd_epi16(sub16_1, sub16_1);
		Vec256 squares32_2 = _mm256_madd_epi16(sub16_2, sub16_2);

		// add the halves together to get R+G+B+A
		Vec256 squares32 = _mm256_add_epi32(squares32_1, squares32_2);

		// convert to float and multiply by weight
		__m256 squares_f32 = _mm256_cvtepi32_ps(squares32);
		weighted_errs[i] = _mm256_mul_ps(squares_f32, _mm256_set1_ps(activity.values[i]));
	}

	// Now our main work loop selects from the precomputed weighted errors using the
	// index vector, for 4 blocks at a time. (Just 4 blocks because we rank both
	// palettes; first_palette values go into even lanes, second_palette values into
	// odd.)

	// Start of last aligned block of 4 candidates.
	// Note we established count >= 1 earlier.
	SINTa last_aligned = (count - 1) & ~3;

	// Do the last (partial) iteration first and count down from there.
	i = last_aligned;
	F32 * dest_ptr = tail_buf;
	Vec128 index_vec128 = _mm_setr_epi32(
		vqindices[inds[i + 0]].dw,
		vqindices[inds[RR_MIN(i + 1, count - 1)]].dw,
		vqindices[inds[RR_MIN(i + 2, count - 1)]].dw,
		vqindices[inds[RR_MIN(i + 3, count - 1)]].dw
	);

	for (;;) // loop condition inside, since the loop structure is a bit unusual
	{
		// We enter here with dest_ptr and index_vec128 set up for this iteration

		// Make two copies of every index because we score two palettes at once
		Vec256 index_vec = _mm256_permutevar8x32_epi32(_mm256_castsi128_si256(index_vec128), _mm256_setr_epi32(0,0, 1,1, 2,2, 3,3));

		// We select errors from an 8-entry set, where the first 4 correspond to the first palette
		// and the last 4 to the second palette. VPERMD uses the low 3 bits of the lane value to
		// select where to get the source from. The bitpacked indices in the BC1 index_vec are
		// 2 bits/pixel. We can set things up more efficiently if we separate even-column from
		// odd-column indices; that spaces them 4 bits apart, which lets us OR in the "4" for the
		// second-palette lanes all at once.
		Vec256 c_mask_even_inds = _mm256_set1_epi8(0x33);
		Vec256 c_mask_index_select = _mm256_setr_epi32(0,0x44444444, 0,0x44444444, 0,0x44444444, 0,0x44444444);

		Vec256 inds_col0 = _mm256_and_si256(index_vec, c_mask_even_inds); // isolate indices in even columns
		Vec256 inds_col1 = _mm256_and_si256(_mm256_srli_epi32(index_vec, 2), c_mask_even_inds); /// odd columns

		// OR in the 4 (=selects lanes for second palette) in the odd-numbered lanes of the vector
		inds_col0 = _mm256_or_si256(inds_col0, c_mask_index_select);
		inds_col1 = _mm256_or_si256(inds_col1, c_mask_index_select);

		// And that's it for prep work, all that's left is selecting options and summing
		// NOTE we sum in 4 chains, one per column, and add them together in the end in
		// a particular order, to match the exact computation regular VQD does.
		// BE VERY CAREFUL HERE! We want to guarantee exactly identical results no
		// matter which VQD impl is used.
		__m256 sum0 = batch_VQD_sum_along_col(weighted_errs + 0, inds_col0);
		__m256 sum1 = batch_VQD_sum_along_col(weighted_errs + 1, inds_col1);
		__m256 sum2 = batch_VQD_sum_along_col(weighted_errs + 2, _mm256_srli_epi32(inds_col0, 4));
		__m256 sum3 = batch_VQD_sum_along_col(weighted_errs + 3, _mm256_srli_epi32(inds_col1, 4));

		// VQD reduction sums inner pairs first, then outer pairs
		sum0 = _mm256_add_ps(sum0, sum1);
		sum2 = _mm256_add_ps(sum2, sum3);

		sum0 = _mm256_add_ps(sum0, sum2);

		_mm256_storeu_ps(dest_ptr, sum0);

		// Advance and set up for next iteration
		i -= 4;
		if ( i < 0 )
			break;

		dest_ptr = dest_vqd + i * 2;
		index_vec128 = _mm_setr_epi32(
			vqindices[inds[i + 0]].dw,
			vqindices[inds[i + 1]].dw,
			vqindices[inds[i + 2]].dw,
			vqindices[inds[i + 3]].dw
		);
	}

	// Copy last few results to final destination
	memcpy(dest_vqd + last_aligned * 2, tail_buf, (count - last_aligned) * 2 * sizeof(F32));
}

void batch_find_best_errors_AVX2(
	S32 * dest_ssd,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColorBlock4x4 & colors)
{
	// Our FindIndices implementation reduces to scoring all 4 possible palette
	// entries per pixel using the computation
	//
	//   ||S(pixel[i] - pal[j])||^2
	//
	// where S=diag(1,1,1,2), to ensure that any difference in A (which is only
	// ever 0 or 255 here) beats any possible error in the RGB channels so
	// we preserve alpha exactly.
	//
	// In this case we don't need the indices and will be using the same
	// pixels many times so we can factor out some common calcs.
	const SINTa count = count32;
	Vec256 pix_br[2], pix_ga[2];

	// Initial pre-scaling of pixels
	for (SINTa i = 0; i < 2; ++i)
		UnpackBGRAto16_Weighted(pix_br[i], pix_ga[i], load256u(colors.colors + i*8));

	// Score one candidate palette at a time
	for (SINTa i = 0; i < count; ++i)
	{
		// Load and set up the palette
		Vec256 pal_br, pal_ga;
		SINTa idx = inds[i] & 0xffff;
		UnpackBGRAto16_Weighted(pal_br, pal_ga, broadcast128_256(load128u(vqendpoints[idx].palette)));

		// Calc the errors
		Vec256 err256;
		err256 = BC1_FindBestErrors_AVX2(pix_br[0], pix_ga[0], pal_br, pal_ga);
		err256 = _mm256_add_epi32(err256, BC1_FindBestErrors_AVX2(pix_br[1], pix_ga[1], pal_br, pal_ga));

		// Finish reduction and store the result
		Vec128 err128 = _mm_add_epi32(lo_half(err256), hi_half(err256));
		dest_ssd[i] = reduce_add_s32(err128);
	}
}

S32 bc4rd_single_block_change_distortion_AVX2(const S16 *src_pixels, const S16 palette[8], const U8 * index8_scaled_ptr)
{
	Vec256 pal = broadcast128_256(load128u(palette));
	Vec256 pixels = load256u(src_pixels);

	// if we arrange the data as:
	//       index8_scaled: a..h, 0..0, i..p, 0..0
	//  then unpacklo, then +1
	//                    a+0,a+1, ... , i+0,i+1, ...

	Vec256 index8_scaled = zext128_256(load128u(index8_scaled_ptr));
	index8_scaled = shuffle64<0,2,1,3>(index8_scaled);

	Vec256 index8_scaled_plus_1 = _mm256_add_epi8(index8_scaled, _mm256_set1_epi8(1));

	Vec256 index16 = _mm256_unpacklo_epi8(index8_scaled, index8_scaled_plus_1);
	Vec256 result = _mm256_shuffle_epi8(pal, index16);

	// diff all 16 pixels
	Vec256 diff_16 = _mm256_sub_epi16(result, pixels);

	// begin dot product with itself
	Vec256 dot256 = _mm256_madd_epi16(diff_16, diff_16);

	return reduce_add_s32(dot256);
}

void bc4rd_single_block_change_distortion_indirect_batch_AVX2(S32 *dest_ssd, const S16 *src_pixels, const S16 palette[8], const U8 * index8_scaled_ptr, const int * inds, int count)
{
	int i=0;
	if (count >= 2)
	{
		Vec256 pal_lo = broadcast128_256(load128u(palette)); // low bytes of palette entries (we index in 2-byte steps)
		Vec256 pal_hi = _mm256_bsrli_epi128(pal_lo, 1); // high bytes of palette entries (bytewise shift by 1)
		Vec256 pixels_0 = broadcast128_256(load128u(src_pixels+0));
		Vec256 pixels_1 = broadcast128_256(load128u(src_pixels+8));
		for (; i < (count&~1); i += 2)
		{
			// load first block in lower 128-bit lane, second block in higher 128-bit lane
			Vec128 lo_index8_scaled = load128u(index8_scaled_ptr + inds[i+0] * 16);
			Vec128 hi_index8_scaled = load128u(index8_scaled_ptr + inds[i+1] * 16);
			Vec256 index8_scaled = combine(lo_index8_scaled, hi_index8_scaled);

			// Look up low and high byte table entries via PSHUFB
			Vec256 pal16_lo = _mm256_shuffle_epi8(pal_lo, index8_scaled);
			Vec256 pal16_hi = _mm256_shuffle_epi8(pal_hi, index8_scaled);

			// Merge results
			Vec256 result_0 = _mm256_unpacklo_epi8(pal16_lo, pal16_hi);
			Vec256 result_1 = _mm256_unpackhi_epi8(pal16_lo, pal16_hi);

			// Diff against all 16 pixels
			Vec256 diff01_16 = _mm256_sub_epi16(result_0, pixels_0);
			Vec256 diff23_16 = _mm256_sub_epi16(result_1, pixels_1);

			// Dot products with itself, each of these contains 2 values summed
			Vec256 dot01 = _mm256_madd_epi16(diff01_16, diff01_16);
			Vec256 dot23 = _mm256_madd_epi16(diff23_16, diff23_16);

			// Horizontal reduction
			Vec256 sum4 = _mm256_add_epi32(dot01, dot23);
			Vec256 sum8 = _mm256_add_epi32(sum4, shuffle32in128<2,3,0,1>(sum4));
			Vec128 sum8b = lo_half(_mm256_permutevar8x32_epi32(sum8, _mm256_setr_epi32(0,4,1,5, 2,6,3,7)));
			Vec128 sum16 = _mm_add_epi32(sum8b, shuffle32<2,3,0,1>(sum8b));
			store64u(&dest_ssd[i], sum16);
		}
	}

	for (; i < count; ++i)
		dest_ssd[i] = bc4rd_single_block_change_distortion_AVX2(src_pixels, palette, index8_scaled_ptr + inds[i]*16);
}


} // internal namespace

OODLE_NS_END

#endif
