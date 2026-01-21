// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetavx512

// We _MUST NOT_ allow the compiler to contract mul-add operations to FMAs!
#include "nocontract.h"

#include "rrdxt1vqhelp.h"
#include "rrdxt1vqhelp.inl"
#include "vec512.inl"
#include "rrdxtcblock_avx512.inl"

#ifdef DO_BUILD_AVX512

OODLE_NS_START

namespace internal {

static RADFORCEINLINE Vec512 Double512(const Vec512 & v)
{
	return _mm512_add_epi32(v, v);
}

void AnyIndexD_batch_lookup_AVX512(S32 out_d[], const AnyIndexD * aid, const U32 in_indices[], int count32)
{
	const SINTa count = count32;
	if ( count < 1 )
		return;

	// Preload the 4 SSD vectors and shuffle into the order we need
	// We transpose from the [index][pixel] in-memory layout to [pixel][index]
	//
	// indices are 0 to 3, pixels are a through p (abcd, efgh, ijkl, mnop)
	//
	// Double the values since AnyIndexD_lookup actually returns 2*SSD
	const Vec512 err_mem0 = Double512(_mm512_loadu_si512(aid->ssd[0])); // a0,b0,c0,d0, ..., p0
	const Vec512 err_mem1 = Double512(_mm512_loadu_si512(aid->ssd[1])); // a1,b1,c1,d1, ..., p1
	const Vec512 err_mem2 = Double512(_mm512_loadu_si512(aid->ssd[2])); // a2,b2,c2,d2, ..., p2
	const Vec512 err_mem3 = Double512(_mm512_loadu_si512(aid->ssd[3])); // a3,b3,c3,d3, ..., p3

	// First transpose pass
	const Vec512 c_shuf0 = _mm512_setr_epi32( 0,16, 1,17, 2,18, 3,19, 4,20, 5,21, 6,22, 7,23);
	const Vec512 c_shuf1 = _mm512_setr_epi32( 8,24, 9,25,10,26,11,27,12,28,13,29,14,30,15,31);

	const Vec512 err_tmp0 = _mm512_permutex2var_epi32(err_mem0, c_shuf0, err_mem2); // a0,a2,b0,b2,c0,c2,d0,d2, ..., h0,h2
	const Vec512 err_tmp1 = _mm512_permutex2var_epi32(err_mem1, c_shuf0, err_mem3); // a1,a3,b1,b3,c1,c3,d1,d3, ..., h1,h3
	const Vec512 err_tmp2 = _mm512_permutex2var_epi32(err_mem0, c_shuf1, err_mem2); // i0,i2,j0,j2,k0,k2,l0,l2, ..., p0,p2
	const Vec512 err_tmp3 = _mm512_permutex2var_epi32(err_mem1, c_shuf1, err_mem3); // i1,i3,j1,j3,k1,k3,l1,l3, ..., p1,p3

	// Second transpose pass
	const Vec512 err_lut0 = _mm512_permutex2var_epi32(err_tmp0, c_shuf0, err_tmp1); // a0,a1,a2,a3,b0,b1,b2,b3, ..., d2,d3
	const Vec512 err_lut1 = _mm512_permutex2var_epi32(err_tmp0, c_shuf1, err_tmp1); // e0,e1,e2,e3,f0,f1,f2,f3, ..., h2,h3
	const Vec512 err_lut2 = _mm512_permutex2var_epi32(err_tmp2, c_shuf0, err_tmp3); // i0,i1,i2,i3,j0,j1,j2,j3, ..., l2,l3
	const Vec512 err_lut3 = _mm512_permutex2var_epi32(err_tmp2, c_shuf1, err_tmp3); // m0,m1,m2,m3,n0,n1,n2,n3, ..., p2,p3

	// Do the last (partial) iteration first and count down from there
	SINTa i = (count - 1) & ~3; // start of last aligned group of 4
	__mmask8 m_active = (1 << (int)(count - i)) - 1;
	Vec512 v_four_inds = broadcast128_512(_mm_maskz_loadu_epi32(m_active, in_indices + i));

	const Vec512 c_index_selectpixel = _mm512_setr_epi32(
		0x00000000, 0x00000000, 0x00000000, 0x00000000,
		0x04040404, 0x04040404, 0x04040404, 0x04040404,
		0x08080808, 0x08080808, 0x08080808, 0x08080808,
		0x0c0c0c0c, 0x0c0c0c0c, 0x0c0c0c0c, 0x0c0c0c0c
	);

	for (;;) // Loop condition inside, unusual loop structure
	{
		// v_four_inds holds our four 32-bit index vectors, arranged like this:
		//   (idx0,idx1,idx2,idx3, idx0,idx1,idx2,idx3, ... )

		// We can process four pixels per vector (one row). Pre-shift indices so each group of 4
		// lanes (corresponding to the source pixels) selects the right index.
		const Vec512 c_index_preshift = _mm512_setr_epi32(0,0,0,0, 2,2,2,2, 4,4,4,4, 6,6,6,6);
		v_four_inds = _mm512_srlv_epi32(v_four_inds, c_index_preshift);

		// Mask to the 2 index bits and set the higher bits to select the right elements in our SSD LUT regs
		v_four_inds = _mm512_ternarylogic_epi32(v_four_inds, _mm512_set1_epi8(3), c_index_selectpixel, (TERNLOG_A & TERNLOG_B) | TERNLOG_C);

		// Grab the lanes with the relevant errros for each pixel and accumulate
		Vec512 v_ssd;

		v_ssd = _mm512_permutexvar_epi32(v_four_inds, err_lut0);
		v_ssd = _mm512_add_epi32(v_ssd, _mm512_permutexvar_epi32(_mm512_srli_epi32(v_four_inds,  8), err_lut1));
		v_ssd = _mm512_add_epi32(v_ssd, _mm512_permutexvar_epi32(_mm512_srli_epi32(v_four_inds, 16), err_lut2));
		v_ssd = _mm512_add_epi32(v_ssd, _mm512_permutexvar_epi32(_mm512_srli_epi32(v_four_inds, 24), err_lut3));

		// Reduce from 512b down to 128b, adding within columns
		Vec256 ssd256 = _mm256_add_epi32(lo_half(v_ssd), hi_half(v_ssd));
		Vec128 result = _mm_add_epi32(lo_half(ssd256), hi_half(ssd256));

		// Store and advance
		_mm_mask_storeu_epi32(out_d + i, m_active, result);
		i -= 4;
		if ( i < 0 )
			break;

		m_active = _cvtu32_mask8(0xf);
		v_four_inds = broadcast128_512(load128u(in_indices + i));
	}
}

void compute_chunk_sads_indirect_AVX512(int chunk_sads[], int count32, const U16 indices[], const rrColorBlock4x4 * pair_colors, const rrColorBlock4x4 & rColors)
{
	SINTa count = count32;
	if ( count < 1 )
		return;

	const Vec512 colors = _mm512_loadu_si512(rColors.colors);
	SINTa i = 0;

	// Do the last (partial) iteration first and count down from there
	U16 last_inds[4];
	const U16 * pInds = last_inds;
	i = (count - 1) & ~3; // start of last aligned group of 4
	__mmask8 m_active = (1 << (int)(count - i)) - 1;
	__mmask8 m_all = 0xf;

	last_inds[0] = indices[i + 0];
	last_inds[1] = indices[RR_MIN(i + 1, count - 1)];
	last_inds[2] = indices[RR_MIN(i + 2, count - 1)];
	last_inds[3] = indices[RR_MIN(i + 3, count - 1)];

	for (;;) // Condition in the middle of the loop
	{
		Vec512 s0 = _mm512_sad_epu8(colors, _mm512_loadu_si512(pair_colors + pInds[0]));
		Vec512 s1 = _mm512_sad_epu8(colors, _mm512_loadu_si512(pair_colors + pInds[1]));
		Vec512 s2 = _mm512_sad_epu8(colors, _mm512_loadu_si512(pair_colors + pInds[2]));
		Vec512 s3 = _mm512_sad_epu8(colors, _mm512_loadu_si512(pair_colors + pInds[3]));

		// Merge the row sums so we have 16-bit row sums for block 0, 1, 2, 3 in the first
		// four 16-bit lanes, and then the pattern repeats for the other 4.
		Vec512 s012 = _mm512_ternarylogic_epi32(s0, _mm512_slli_epi64(s1, 16), _mm512_slli_epi64(s2, 32), TERNLOG_A | TERNLOG_B | TERNLOG_C);
		Vec512 s0123 = _mm512_or_si512(s012, _mm512_slli_epi64(s3, 48));

		// Reduce the vector down to 128 bits
		Vec256 sum0 = _mm256_add_epi16(lo_half(s0123), hi_half(s0123));
		Vec128 sum1 = _mm_add_epi16(lo_half(sum0), hi_half(sum0));

		// Rearrange so the two 16-bit values for each block are adjacent
		Vec128 sum2 = _mm_shuffle_epi8(sum1, _mm_setr_epi8(0,1,8,9, 2,3,10,11, 4,5,12,13, 6,7,14,15));

		// Add up the two halves of each of the original 128b (now adjacent 16b lanes) and store
		Vec128 sum3 = _mm_madd_epi16(sum2, _mm_set1_epi16(1));
		_mm_mask_storeu_epi32(chunk_sads + i, m_active, sum3);

		i -= 4;
		if ( i < 0 )
			break;

		pInds = indices + i;
		m_active = m_all;
	}
}

static RADFORCEINLINE Vec512 filter_by_index_sad_kernel_AVX512(const Vec512 & ref_inds, const U8 * inds_base)
{
	// SADs for 16 blocks
	// gives us two 16-bit values for the low/high halves of the block,
	// which we'll refer to by the letter name for the block followed by 0 (lo) or 1 (hi) respectively
	const Vec512 s0 = _mm512_sad_epu8(ref_inds, _mm512_loadu_si512(inds_base + 0*64)); // blocks a,b,c,d
	const Vec512 s1 = _mm512_sad_epu8(ref_inds, _mm512_loadu_si512(inds_base + 1*64)); // blocks e,f,g,h
	const Vec512 s2 = _mm512_sad_epu8(ref_inds, _mm512_loadu_si512(inds_base + 2*64)); // blocks i,j,k,l
	const Vec512 s3 = _mm512_sad_epu8(ref_inds, _mm512_loadu_si512(inds_base + 3*64)); // blocks m,n,o,p

	// Each of the four vectors only use 16 out of every 64 bits; combine pairs
	// so we use two 32b slots in every 64 bits
	const Vec512 s01 = _mm512_or_si512(s0, _mm512_slli_epi64(s1, 32));
	const Vec512 s23 = _mm512_or_si512(s2, _mm512_slli_epi64(s3, 32));
	// as 32-bit lanes, we now have:
	//   s01 = (a0,e0,a1,e1, b0,f0,b1,f1, ...)
	//   s23 = (i0,m0,i1,m1, j0,n0,j1,n1, ...)

	// We still need to sum the values from the low/high halves of the original 128b blocks.
	// Shuffle blocks into the right order for summing:
	//   presum0 should grab (a0,b0,c0,d0,e0, ..., p0)
	//   presum1 should grab (a1,b1,c1,d1,e1, ..., p1)
	const Vec512 c_shuffle0 = _mm512_setr_epi32( 0, 4, 8,12,  1, 5, 9,13, 16,20,24,28, 17,21,25,29);
	const Vec512 c_shuffle1 = _mm512_setr_epi32( 2, 6,10,14,  3, 7,11,15, 18,22,26,30, 19,23,27,31);

	const Vec512 presum0 = _mm512_permutex2var_epi32(s01, c_shuffle0, s23);
	const Vec512 presum1 = _mm512_permutex2var_epi32(s01, c_shuffle1, s23);

	// Then add!
	const Vec512 sad32s = _mm512_add_epi32(presum0, presum1);

	return sad32s;
}

int filter_by_index_sad_AVX512(int positions[], int count32, const U8 * index_batch, const U8 ref_inds_ptr[16], int sad_thresh)
{
	const Vec512 v_sad_thresh = _mm512_set1_epi32(sad_thresh);
	const Vec512 ref_inds = _mm512_broadcast_i32x4(load128u(ref_inds_ptr));
	const SINTa count = count32;
	SINTa out_count = 0;

	Vec512 lane_ids = _mm512_setr_epi32(0,1,2,3, 4,5,6,7, 8,9,10,11, 12,13,14,15);

	// Process groups of 16 blocks at once
	const U8 * inds_base = index_batch;
	const U8 * inds_aligned_end = inds_base + (count & ~15) * 16;

	for (; inds_base != inds_aligned_end; inds_base += 16*16)
	{
		// Run the kernel
		const Vec512 sad32s = filter_by_index_sad_kernel_AVX512(ref_inds, inds_base);

		// Now, store the IDs of items with values below sad_thresh
		__mmask16 m_below = _mm512_cmplt_epi32_mask(sad32s, v_sad_thresh);
		_mm512_mask_compressstoreu_epi32(positions + out_count, m_below, lane_ids);

		out_count += (U32)_mm_popcnt_u32(m_below);
		lane_ids = _mm512_add_epi32(lane_ids, _mm512_set1_epi32(16));
	}

	SINTa i = (inds_base - index_batch) >> 4;
	const Vec128 ref_inds128 = _mm512_castsi512_si128(ref_inds);

	// tail
	for (; i < count; ++i)
	{
		const Vec128 sad = _mm_sad_epu8(load128u(index_batch + i*16), ref_inds128);

		// Sum the two halves
		const Vec128 sum = _mm_add_epi16(sad, shuffle32<2,3,0,1>(sad));

		positions[out_count] = (int)i;
		out_count += _mm_cvtsi128_si32(sum) < sad_thresh;
	}

	return (int)out_count;
}

int filter_by_index_sad_with_inds_AVX512(int positions[], U32 out_indices[], int count32, const U8 * index_batch, const U32 * in_indices, const U8 ref_inds_ptr[16], int sad_thresh)
{
	const Vec512 v_sad_thresh = _mm512_set1_epi32(sad_thresh);
	const Vec512 ref_inds = _mm512_broadcast_i32x4(load128u(ref_inds_ptr));
	const SINTa count = count32;
	SINTa out_count = 0;

	Vec512 lane_ids = _mm512_setr_epi32(0,1,2,3, 4,5,6,7, 8,9,10,11, 12,13,14,15);

	// Process groups of 16 blocks at once
	const U8 * inds_base = index_batch;
	const U8 * inds_aligned_end = inds_base + (count & ~15) * 16;
	const U32 * inds_ptr = in_indices;

	for (; inds_base != inds_aligned_end; inds_base += 16*16, inds_ptr += 16)
	{
		// Run the kernel
		const Vec512 sad32s = filter_by_index_sad_kernel_AVX512(ref_inds, inds_base);
		const Vec512 indices = _mm512_loadu_si512(inds_ptr);

		// Now, store the IDs of items with values below sad_thresh
		__mmask16 m_below = _mm512_cmplt_epi32_mask(sad32s, v_sad_thresh);
		_mm512_mask_compressstoreu_epi32(positions + out_count, m_below, lane_ids);
		_mm512_mask_compressstoreu_epi32(out_indices + out_count, m_below, indices);

		out_count += (U32)_mm_popcnt_u32(m_below);
		lane_ids = _mm512_add_epi32(lane_ids, _mm512_set1_epi32(16));
	}

	SINTa i = (inds_base - index_batch) >> 4;
	const Vec128 ref_inds128 = _mm512_castsi512_si128(ref_inds);

	// tail
	for (; i < count; ++i)
	{
		const U32 ind = in_indices[i];
		const Vec128 sad = _mm_sad_epu8(load128u(index_batch + i*16), ref_inds128);

		// Sum the two halves
		const Vec128 sum = _mm_add_epi16(sad, shuffle32<2,3,0,1>(sad));

		positions[out_count] = (int)i;
		out_indices[out_count] = ind;
		out_count += _mm_cvtsi128_si32(sum) < sad_thresh;
	}

	return (int)out_count;
}

static Vec512 dist_to_bbox_AVX512(const Vec512 & v, const Vec512 & bbox_lo, const Vec512 & bbox_hi)
{
	Vec512 axial_dist8 = _mm512_add_epi8(_mm512_subs_epu8(bbox_lo, v), _mm512_subs_epu8(v, bbox_hi));

	Vec512 even_dist16 = _mm512_and_si512(axial_dist8, _mm512_set1_epi16(0xff));
	Vec512 odd_dist16 = _mm512_srli_epi16(axial_dist8, 8);

	Vec512 dots1 = _mm512_madd_epi16(even_dist16, even_dist16);
	Vec512 dots2 = _mm512_madd_epi16(odd_dist16, odd_dist16);

	return _mm512_add_epi32(dots1, dots2);
}

int filter_endpoint_color_bbox_AVX512(
	int * dest_inds,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColor32BGRA bbox[2],
	U32 bbox_min_d, bool require_3c)
{
	SINTa count = count32;
	SINTa out_count = 0;
	SINTa i = 0;

	RR_ASSERT(count < 65536);

	if ( ! count )
		return 0;

	Vec512 bbox_lo = _mm512_set1_epi32(bbox[0].dw);
	Vec512 bbox_hi = _mm512_set1_epi32(bbox[1].dw);
	Vec512 lane_ids = _mm512_setr_epi32(0, 1<<16, 2<<16, 3<<16, 4<<16, 5<<16, 6<<16, 7<<16, 8<<16, 9<<16, 10<<16, 11<<16, 12<<16, 13<<16, 14<<16, 15<<16);
	Vec512 last_count = _mm512_set1_epi32((count32 - 1) << 16);
	Vec512 vec_min_d = _mm512_set1_epi32(bbox_min_d);

	Vec512 idx_ep0 = _mm512_setr_epi32(0,2,4,6, 8,10,12,14, 16,18,20,22, 24,26,28,30);
	Vec512 idx_ep1 = _mm512_setr_epi32(1,3,5,7, 9,11,13,15, 17,19,21,23, 25,27,29,31);

	__mmask16 m_require_3c = _cvtu32_mask16(require_3c ? 0xffff : 0);
	__mmask8 m_all = _cvtu32_mask8(0xff);

	for (; i < count; i += 16)
	{
		// Check which lanes are active
		// we mask our loads by this and the stores are implicitly masked by it too
		__mmask16 m_lane_active = _mm512_cmple_epu32_mask(lane_ids, last_count);

		// Load and unpack 16 endpoints
		// 64-bit-lane gathers are preferred here (we need two DWords and fewer distinct loads is better)
		// we snap out-of-bounds inds to 0
		Vec512 v_inds = _mm512_maskz_loadu_epi32(m_lane_active, inds + i);
		//Vec512 v_offs = _mm512_mullo_epi32(v_inds, _mm512_set1_epi32((int)sizeof(rrDXT1_VQ_Entry)));
		RR_COMPILER_ASSERT(sizeof(rrDXT1_VQ_Entry) == 32);
		Vec512 v_offs = _mm512_slli_epi32(v_inds, 5);

		// Gathers for inactive lanes just fetch the palette for endpoint 0 which
		// is safe
		//
		// NOTE: using gathers with an explicit mask here because gathers are
		// _always_ masked; doing it like this allows us to make the
		// destination (which gets overwritten) be a zero-cleared register.
		// If we don't do this, we pick up an implicit dependency on whatever
		// was in that register earlier, which is a recipe for unintentionally
		// introducing inter-iteration dependenices that ruin our perf here.
		Vec512 gather0 = _mm512_mask_i32gather_epi64(_mm512_setzero_si512(), m_all, lo_half(v_offs), &vqendpoints[0].palette, 1);
		Vec512 gather1 = _mm512_mask_i32gather_epi64(_mm512_setzero_si512(), m_all, hi_half(v_offs), &vqendpoints[0].palette, 1);

		// Select endpoint0 and endpoint1 values, respectively
		Vec512 ep0 = _mm512_permutex2var_epi32(gather0, idx_ep0, gather1);
		Vec512 ep1 = _mm512_permutex2var_epi32(gather0, idx_ep1, gather1);
		Vec512 avg = _mm512_avg_epu8(ep0, ep1);

		// Block mode test: if require_3c is set and the block selects 4-color mode, reject it.
		// 4-color mode is when ep0 > ep1 in the packed 16-bit representation; because unpacking
		// the color endpoints is strictly monotonic and order-preserving, we can compare
		// ep0 to ep1 (unsigned).
		__mmask16 m_mode_reject = _mm512_mask_cmpgt_epu32_mask(m_require_3c, ep0, ep1);
		__mmask16 m_mode_accept = _kandn_mask16(m_mode_reject, m_lane_active);

		// Compute distances to bbox
		Vec512 dist_to_bbox_ep0 = dist_to_bbox_AVX512(ep0, bbox_lo, bbox_hi);
		Vec512 dist_to_bbox_ep1 = dist_to_bbox_AVX512(ep1, bbox_lo, bbox_hi);
		Vec512 dist_to_bbox_avg = dist_to_bbox_AVX512(avg, bbox_lo, bbox_hi);

		// Distance accept is accepted when we're <= min_d (NOTE AVX2 and lower code
		// keeps min_d + 1 in reg here because they have no <= available)
		Vec512 min_dist = _mm512_min_epi32(dist_to_bbox_ep0, dist_to_bbox_ep1);
		min_dist = _mm512_min_epi32(min_dist, dist_to_bbox_avg);

		__mmask16 m_active = _mm512_mask_cmple_epu32_mask(m_mode_accept, min_dist, vec_min_d);

		// Put indices into low half of output int, filtered i in high half
		Vec512 values = _mm512_or_si512(v_inds, lane_ids);

		// Store active elements only
		_mm512_mask_compressstoreu_epi32(dest_inds + out_count, m_active, values);

		out_count += (U32)_mm_popcnt_u32(m_active);
		lane_ids = _mm512_add_epi32(lane_ids, _mm512_set1_epi32(16 << 16));
	}

	return (int)out_count;
}

static RADFORCEINLINE Vec512 batch_VQD_load_inds(const rrDXT1_VQ_Entry * vqindices, int i0, int i1, int i2, int i3)
{
	// NOTE(fg): Actual gather (with vector index inputs of course!) did _way_ worse here,
	// so doing it manually.
	const Vec128 ind_01 = _mm_insert_epi32(_mm_cvtsi32_si128(vqindices[i0].dw), vqindices[i1].dw, 1);
	const Vec128 ind_23 = _mm_insert_epi32(_mm_cvtsi32_si128(vqindices[i2].dw), vqindices[i3].dw, 1);

	// Make two copies of every index for the two palettes, and repeat the whole pattern twice
	// because we process two columns at once
	const Vec512 c_shuffle = _mm512_setr_epi32(0,0,1,1, 16,16,17,17, 0,0,1,1, 16,16,17,17);
	return _mm512_permutex2var_epi32(_mm512_castsi128_si512(ind_01), c_shuffle, _mm512_castsi128_si512(ind_23));
}

static RADFORCEINLINE __m512 batch_VQD_sum_along_two_cols(const __m512 weighted_errs[], const Vec512 & inds)
{
	__m512 sum;

	sum = _mm512_permutexvar_ps(inds, weighted_errs[0]);
	sum = _mm512_add_ps(sum, _mm512_permutexvar_ps(_mm512_srli_epi32(inds,  8), weighted_errs[2]));
	sum = _mm512_add_ps(sum, _mm512_permutexvar_ps(_mm512_srli_epi32(inds, 16), weighted_errs[4]));
	sum = _mm512_add_ps(sum, _mm512_permutexvar_ps(_mm512_srli_epi32(inds, 24), weighted_errs[6]));

	return sum;
}

void batch_compute_VQDs_AVX512(
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
	Vec512 pal_vec = broadcast256_512(combine(load128u(first_palette), load128u(second_palette)));
	__m512 weighted_errs[8]; // each group of 8 lanes has errors for one pixel, 0-3 for first_palette, 4-7 for second_palette

	for (SINTa i = 0; i < 8; ++i)
	{
		// Load two pixels and distribute to the lanes
		const Vec512 c_bcast_shuf = _mm512_setr_epi32(0,0,0,0, 0,0,0,0, 1,1,1,1, 1,1,1,1);
		Vec512 pixel = _mm512_permutexvar_epi32(c_bcast_shuf, _mm512_castsi128_si512(load64u(&colors.colors[i*2])));

		// Compute the absolute differences in 8 bits and form
		// 16-bit differences for even/odd channel pairs
		Vec512 sub8 = _mm512_or_si512( _mm512_subs_epu8(pixel, pal_vec), _mm512_subs_epu8(pal_vec,pixel) );
		Vec512 sub16_1 = _mm512_and_si512(sub8, _mm512_set1_epi16(0xff)); // 16-bit: R, B, R, B, ...
		Vec512 sub16_2 = _mm512_srli_epi16(sub8, 8); // 16-bit: G, A, G, A, ...

		// this squares and horizontally adds pairs
		//	we go from 16 bits * 4 colors channels * 4 pixels
		//	-> 32 bits with {R+B} (squares32_1), {G+A} (squares32_2)
		Vec512 squares32_1 = _mm512_madd_epi16(sub16_1, sub16_1);
		Vec512 squares32_2 = _mm512_madd_epi16(sub16_2, sub16_2);

		// add the halves together to get R+G+B+A
		Vec512 squares32 = _mm512_add_epi32(squares32_1, squares32_2);

		// convert to float and multiply by weight
		__m512 squares_f32 = _mm512_cvtepi32_ps(squares32);
		__m128 two_activities = _mm_castpd_ps(_mm_load_sd((const double *)&activity.values[i*2]));
		__m512 activities_f32 = _mm512_permutexvar_ps(c_bcast_shuf, _mm512_castps128_ps512(two_activities));

		weighted_errs[i] = _mm512_mul_ps(squares_f32, activities_f32);
	}

	// Do the last (partial) iteration first and count down from there
	SINTa i = (count - 1) & ~3; // start of last aligned group of 4
	int nlast = (int)(count - i);
	Vec512 v_inds = batch_VQD_load_inds(vqindices, inds[i], inds[RR_MIN(i + 1, count - 1)], inds[RR_MIN(i + 2, count - 1)], inds[RR_MIN(i + 3, count - 1)]);
	__mmask8 m_active = (1 << (2*nlast)) - 1;
	__mmask8 m_all = _cvtu32_mask8(0xff);

	static RAD_ALIGN(const U32, c_index_sel_vals[16], 64) =
	{
		0x00000000, 0x44444444, 0x00000000, 0x44444444, // even columns, inds 0-1
		0x00000000, 0x44444444, 0x00000000, 0x44444444, // even columns, inds 2-3
		0x88888888, 0xcccccccc, 0x88888888, 0xcccccccc, // odd columns, inds 0-1
		0x88888888, 0xcccccccc, 0x88888888, 0xcccccccc  // odd columns, inds 2-3
	};
	const Vec512 c_index_select = _mm512_loadu_si512(c_index_sel_vals);

	// Now our main work loop selects from the precomputed weighted errors using the
	// index vector, for 4 blocks at a time.
	for (;;) // Loop condition inside
	{
		// We can process two pixels per vector (half a row). Pre-shift indices so each group
		// of 8 lanes (corresponding to source pixels from 4 blocks, both repeated twice because
		// of the two palettes) selects the right index.
		const Vec512 c_index_preshift = _mm512_setr_epi32(0,0,0,0, 0,0,0,0, 2,2,2,2, 2,2,2,2);
		v_inds = _mm512_srlv_epi32(v_inds, c_index_preshift);

		// Mask to the 2 index bits and set the higher bits to select the right elements in our VQD LUTs
		v_inds = _mm512_ternarylogic_epi32(v_inds, _mm512_set1_epi8(0x33), c_index_select, (TERNLOG_A & TERNLOG_B) | TERNLOG_C);

		// NOTE summation order here must match the other versions of this function exactly,
		// BE VERY CAREFUL HERE!
		__m512 sum01 = batch_VQD_sum_along_two_cols(weighted_errs + 0, v_inds);
		__m512 sum23 = batch_VQD_sum_along_two_cols(weighted_errs + 1, _mm512_srli_epi32(v_inds, 4));

		// VQD reduction sums inner pairs first, then outer pairs
		__m256 red01 = _mm256_add_ps(lo_half(sum01), hi_half(sum01));
		__m256 red23 = _mm256_add_ps(lo_half(sum23), hi_half(sum23));
		__m256 red0123 = _mm256_add_ps(red01, red23);

		_mm256_mask_storeu_ps(dest_vqd + i*2, m_active, red0123);

		// Advance and set up for next iteration
		i -= 4;
		if ( i < 0 )
			break;

		m_active = m_all;
		v_inds = batch_VQD_load_inds(vqindices, inds[i], inds[i + 1], inds[i + 2], inds[i + 3]);
	}
}

void batch_find_best_errors_AVX512(
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
	SINTa i = 0;

	// Initial pre-scaling of pixels
	Vec512 pix_prerot[8];
	UnpackBGRAto16_Weighted_Prerot(pix_prerot, _mm512_loadu_si512(colors.colors));

	// Main loop does 4 palettes at once, which makes for a more efficient reduction
	// since get to share shuffles between 4 results
	for (; i < (count & ~3); i += 4)
	{
		Vec512 pal_br, pal_ga;

		// Load and set up the palette, then calc errors 4 times
		UnpackBGRAto16_Weighted(pal_br, pal_ga, _mm512_broadcast_i32x4(load128u(vqendpoints[inds[i + 0] & 0xffff].palette)));
		Vec512 err0 = BC1_FindBestErrorsPrerot_AVX512(pix_prerot, pal_br, pal_ga);

		UnpackBGRAto16_Weighted(pal_br, pal_ga, _mm512_broadcast_i32x4(load128u(vqendpoints[inds[i + 1] & 0xffff].palette)));
		Vec512 err1 = BC1_FindBestErrorsPrerot_AVX512(pix_prerot, pal_br, pal_ga);

		UnpackBGRAto16_Weighted(pal_br, pal_ga, _mm512_broadcast_i32x4(load128u(vqendpoints[inds[i + 2] & 0xffff].palette)));
		Vec512 err2 = BC1_FindBestErrorsPrerot_AVX512(pix_prerot, pal_br, pal_ga);

		UnpackBGRAto16_Weighted(pal_br, pal_ga, _mm512_broadcast_i32x4(load128u(vqendpoints[inds[i + 3] & 0xffff].palette)));
		Vec512 err3 = BC1_FindBestErrorsPrerot_AVX512(pix_prerot, pal_br, pal_ga);

		// Reduce and store
		store128u(dest_ssd + i, HorizontalSum4x(err0, err1, err2, err3));
	}

	// Tail loop scores one candidate palette at a time
	for (; i < count; ++i)
	{
		// Load and set up the palette
		Vec512 pal_br, pal_ga;
		UnpackBGRAto16_Weighted(pal_br, pal_ga, _mm512_broadcast_i32x4(load128u(vqendpoints[inds[i] & 0xffff].palette)));

		// Calc the errors
		Vec512 err512 = BC1_FindBestErrors_AVX512(pix_prerot[0], pix_prerot[1], pal_br, pal_ga);

		// Finish reduction and store the result
		dest_ssd[i] = _mm512_reduce_add_epi32(err512);
	}
}

} // internal namespace

OODLE_NS_END

#endif
