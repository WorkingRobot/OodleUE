// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetsse4

#include "rrdxt1vqhelp.h"
#include "rrdxt1vqhelp.inl"
#include "rrdxtcblock.inl"
#include "templates/rralgorithm.h"
#include "perceptualactivity.h"
#include "perceptualactivity.inl"
#include "rrsimd.h"

RR_NAMESPACE_START

void sort_dword_and_count_compare_count_highest_first(vector<dword_and_count> * pcounting)
{
	// sort counting by count :
	stdsort(pcounting->begin(),pcounting->end(),dword_and_count_compare_count_highest_first());
}

void sort_dword_and_count_compare_dword(vector<dword_and_count> * pcounting)
{
	// sort counting by count :
	stdsort(pcounting->begin(),pcounting->end(),dword_and_count_compare_dword());
}

void sort_and_count_uniques(vector<dword_and_count> * pcounting,vector<U32> & endpoints)
{			
	vector<dword_and_count> & counting = *pcounting;
	counting.clear();
	
	if ( endpoints.empty() )
		return;

	// sort & count endpoint use :
	stdsort(endpoints.begin(),endpoints.end());

	counting.reserve(endpoints.size());		
	counting.push_back();
	counting.back().dw = endpoints[0];
	counting.back().count = 1;
	
	for(int epi=1;epi<endpoints.size32();epi++)
	{
		U32 cur = endpoints[epi];
		if ( cur == counting.back().dw )
		{
			counting.back().count++;
		}
		else
		{
			counting.push_back();
			counting.back().dw = cur;
			counting.back().count = 1;
		}
	}
}		
		
void sort_bc7bits_and_count_compare_count_highest_first(vector<bc7bits_and_count> * pcounting)
{
	// sort counting by count :
	stdsort(pcounting->begin(),pcounting->end(),bc7bits_and_count_compare_count_highest_first());
}

void sort_bc7bits_and_count_compare_count_lowest_first(vector<bc7bits_and_count> * pcounting)
{
	// sort counting by count :
	stdsort(pcounting->begin(),pcounting->end(),bc7bits_and_count_compare_count_lowest_first());
}

void sort_bc7bits_and_count_compare_bc7bits(vector<bc7bits_and_count> * pcounting)
{
	// sort counting by count :
	stdsort(pcounting->begin(),pcounting->end(),bc7bits_and_count_compare_bc7bits());
}

void sort_and_count_uniques(vector<bc7bits_and_count> * pcounting,vector<bc7bits> & endpoints)
{			
	vector<bc7bits_and_count> & counting = *pcounting;
	counting.clear();
	if ( endpoints.empty() ) return;

	// sort & count endpoint use :
	stdsort(endpoints.begin(),endpoints.end());

	counting.reserve(endpoints.size());		
	counting.push_back();
	counting.back().val = endpoints[0];
	counting.back().count = 1;
	
	for(int epi=1;epi<endpoints.size32();epi++)
	{
		bc7bits cur = endpoints[epi];
		if ( cur == counting.back().val )
		{
			counting.back().count++;
		}
		else
		{
			counting.push_back();
			counting.back().val = cur;
			counting.back().count = 1;
		}
	}
}

#ifdef DO_BUILD_SSE4

static void AnyIndexD_batch_lookup_SSE4(S32 out_d[], const AnyIndexD * aid, const U32 in_indices[], int count)
{
	int i = 0;

	for (; i < (count & ~3); i += 4)
	{
		// Work on four blocks at a time
		Vec128 v_ssd0 = _mm_setzero_si128();
		Vec128 v_ssd1 = _mm_setzero_si128();
		Vec128 v_ssd2 = _mm_setzero_si128();
		Vec128 v_ssd3 = _mm_setzero_si128();

		Vec128 v_four_inds = load128u(in_indices + i);
		const Vec128 c_column_align = _mm_setr_epi16(0,1<<6, 0,1<<4, 0,1<<2, 0,1<<0);

		// Iterate over the rows in reverse order
		for (int c = 12; c >= 0; c -= 4)
		{
			// Load four sets of SSD scores from the AID
			__m128 ind0err = _mm_loadu_ps((const float *) &aid->ssd[0][c]);
			__m128 ind1err = _mm_loadu_ps((const float *) &aid->ssd[1][c]);
			__m128 ind2err = _mm_loadu_ps((const float *) &aid->ssd[2][c]);
			__m128 ind3err = _mm_loadu_ps((const float *) &aid->ssd[3][c]);
			Vec128 t_ind;
			__m128 t_vals02, t_vals13;

			#define PROCESS_ONE_BLOCK(v_ssd,lane) \
				t_ind = shuffle32<lane,lane,lane,lane>(v_four_inds); \
				/* Shift index for target column into top 2 bits of 32b lane; can use a 16-bit multiply */ \
				/* here since we only need to mess with the high half to accomplish this. */ \
				t_ind = _mm_mullo_epi16(t_ind, c_column_align); \
				/* Select between 4 possible error values first based on the high index bit */ \
				t_vals02 = _mm_blendv_ps(ind0err, ind2err, _mm_castsi128_ps(t_ind)); \
				t_vals13 = _mm_blendv_ps(ind1err, ind3err, _mm_castsi128_ps(t_ind)); \
				/* Double t_ind to get the index LSB mask into lane MSBs */ \
				t_ind = _mm_add_epi32(t_ind, t_ind); \
				t_vals02 = _mm_blendv_ps(t_vals02, t_vals13, _mm_castsi128_ps(t_ind)); \
				/* Correct error term selected, now accumulate! */ \
				v_ssd = _mm_add_epi32(v_ssd, _mm_castps_si128(t_vals02))

			PROCESS_ONE_BLOCK(v_ssd0, 0);
			PROCESS_ONE_BLOCK(v_ssd1, 1);
			PROCESS_ONE_BLOCK(v_ssd2, 2);
			PROCESS_ONE_BLOCK(v_ssd3, 3);

			#undef PROCESS_ONE_BLOCK

			// Shift out the 8 index bits we just consumed
			v_four_inds = _mm_slli_epi32(v_four_inds, 8);
		}

		// Transpose our 4x4 block of results so we have a vertical instead of
		// horizontal final reduction (also sets us up for batched output)
		Vec128 t;
		#define INTERLEAVE(va, vb) \
			t = va; \
			va = _mm_unpacklo_epi32(va, vb); \
			vb = _mm_unpackhi_epi32(t, vb)

		INTERLEAVE(v_ssd0, v_ssd2);
		INTERLEAVE(v_ssd1, v_ssd3);

		INTERLEAVE(v_ssd0, v_ssd1);
		INTERLEAVE(v_ssd2, v_ssd3);

		#undef INTERLEAVE

		// Reduce
		v_ssd0 = _mm_add_epi32(v_ssd0, v_ssd1);
		v_ssd0 = _mm_add_epi32(v_ssd0, v_ssd2);
		v_ssd0 = _mm_add_epi32(v_ssd0, v_ssd3);

		// Finally, double the result, since that's what AnyIndexD_lookup does
		// (D = 2 * ssd)
		v_ssd0 = _mm_add_epi32(v_ssd0, v_ssd0);

		store128u(out_d + i, v_ssd0);
	}

	// tail
	for (; i < count; ++i)
		out_d[i] = AnyIndexD_lookup(aid, in_indices[i]);
}

void AnyIndexD_batch_lookup(CpuDispatchFlags dispatch, S32 out_d[], const AnyIndexD * aid, const U32 in_indices[], int count)
{
#ifdef DO_BUILD_AVX512
	if ( dispatch.AVX512() )
	{
		internal::AnyIndexD_batch_lookup_AVX512(out_d, aid, in_indices, count);
		return;
	}
#endif

#ifdef DO_BUILD_AVX2
	if ( dispatch.AVX2() )
	{
		internal::AnyIndexD_batch_lookup_AVX2(out_d, aid, in_indices, count);
		return;
	}
#endif

	AnyIndexD_batch_lookup_SSE4(out_d, aid, in_indices, count);
}

void AnyIndex8D64_batch_lookup(CpuDispatchFlags dispatch, S64 out_d[], const AnyIndex8D64 * aid, const U8 *indices_interleaved, const U8 *indices_scaled, ptrdiff_t indices_step, int count)
{
#ifdef DO_BUILD_AVX2
	if ( dispatch.AVX2() )
	{
		internal::AnyIndex8D64_batch_lookup_AVX2(out_d, aid, indices_interleaved, count & ~7);
		for (int i = count & ~7; i < count; ++i)
			out_d[i] = AnyIndex8D64_lookup(aid, indices_scaled + i*indices_step);
		return;
	}
#endif	
	for (int i = 0; i < count; ++i)
		out_d[i] = AnyIndex8D64_lookup(aid, indices_scaled + i*indices_step);
}

// block SADs without complete reduction
static RADFORCEINLINE Vec128 block_SAD_nored(const rrColorBlock4x4 * psrc, const Vec128 & colors0, const Vec128 & colors1, const Vec128 & colors2, const Vec128 & colors3)
{
	// Sum across the four rows, but hold off on the lo/hi half reduction
	Vec128 t = _mm_sad_epu8(load128u(psrc->colors), colors0);
	t = _mm_add_epi16(t, _mm_sad_epu8(load128u(psrc->colors +  4), colors1));
	t = _mm_add_epi16(t, _mm_sad_epu8(load128u(psrc->colors +  8), colors2));
	t = _mm_add_epi16(t, _mm_sad_epu8(load128u(psrc->colors + 12), colors3));
	return t;
}

static void compute_chunk_sads_indirect_SSE2(int chunk_sads[], int count, const U16 indices[], const rrColorBlock4x4 * pair_colors, const rrColorBlock4x4 & rColors)
{
	const Vec128 colors0 = load128u(rColors.colors +  0);
	const Vec128 colors1 = load128u(rColors.colors +  4);
	const Vec128 colors2 = load128u(rColors.colors +  8);
	const Vec128 colors3 = load128u(rColors.colors + 12);
	int i = 0;

	// We're summing across 64 bytes in [0,255], so the maximum sum is 64*255 = 16320;
	// we can stay in 16-bit words throughout.
	for (; i < (count & ~3); i += 4)
	{
		Vec128 s0 = block_SAD_nored(pair_colors + indices[i + 0], colors0, colors1, colors2, colors3);
		Vec128 s1 = block_SAD_nored(pair_colors + indices[i + 1], colors0, colors1, colors2, colors3);
		Vec128 s2 = block_SAD_nored(pair_colors + indices[i + 2], colors0, colors1, colors2, colors3);
		Vec128 s3 = block_SAD_nored(pair_colors + indices[i + 3], colors0, colors1, colors2, colors3);

		// merge the row sums (64b -> 32b reduction, then 32b->16b)
		s0 = _mm_packs_epi32(s0, s1);
		s2 = _mm_packs_epi32(s2, s3);
		s0 = _mm_packs_epi32(s0, s2);

		// add up the two halves of each of the original 128b (now adjacent 16b lanes) and store
		s0 = _mm_madd_epi16(s0, _mm_set1_epi16(1));

		store128u(chunk_sads + i, s0);
	}

	// tail
	for (; i < count; ++i)
	{
		const Vec128 sad = block_SAD_nored(pair_colors + indices[i], colors0, colors1, colors2, colors3);

		// Finish reduction by adding the low and high half results
		const Vec128 sum = _mm_add_epi16(sad, shuffle32<2,3,0,1>(sad));
		store32u(chunk_sads + i, sum);
	}
}

void compute_chunk_sads_indirect(CpuDispatchFlags dispatch, int chunk_sads[], int count, const U16 indices[], const rrColorBlock4x4 * pair_colors, const rrColorBlock4x4 & rColors)
{
#ifdef DO_BUILD_AVX512
	if ( dispatch.AVX512() )
	{
		internal::compute_chunk_sads_indirect_AVX512(chunk_sads, count, indices, pair_colors, rColors);
		return;
	}
#endif

#ifdef DO_BUILD_AVX2
	if ( dispatch.AVX2() )
	{
		internal::compute_chunk_sads_indirect_AVX2(chunk_sads, count, indices, pair_colors, rColors);
		return;
	}
#endif

	compute_chunk_sads_indirect_SSE2(chunk_sads, count, indices, pair_colors, rColors);
}

static int filter_by_index_sad_SSE2(int positions[], int count32, const U8 * index_batch, const U8 ref_inds_ptr[16], int sad_thresh)
{
	const Vec128 v_sad_thresh = _mm_set1_epi32(sad_thresh);
	const Vec128 ref_inds = load128u(ref_inds_ptr);
	const SINTa count = count32;
	SINTa i = 0;
	SINTa out_count = 0;

	Vec128 base_index = _mm_setzero_si128();

	for (; i < (count & ~3); i += 4)
	{
		const U8 * inds_base = index_batch + i * 16;

		// Compute 4 blocks worth of index SADs
		Vec128 s0 = _mm_sad_epu8(load128u(inds_base + 0*16), ref_inds);
		Vec128 s1 = _mm_sad_epu8(load128u(inds_base + 1*16), ref_inds);
		Vec128 s2 = _mm_sad_epu8(load128u(inds_base + 2*16), ref_inds);
		Vec128 s3 = _mm_sad_epu8(load128u(inds_base + 3*16), ref_inds);

		// merge the row sums (64b -> 32b reduction, then 32b->16b)
		s0 = _mm_packs_epi32(s0, s1);
		s2 = _mm_packs_epi32(s2, s3);
		s0 = _mm_packs_epi32(s0, s2);

		// add up the two halves of each of the original 128b (now adjacent 16b lanes)
		s0 = _mm_madd_epi16(s0, _mm_set1_epi16(1));

		// store IDs of lanes that have values below sad_thresh
		// at this point, out_count <= i < (count & ~3), so we never store out of bounds
		out_count += store_active_lane_ids(positions + out_count, base_index, _mm_cmpgt_epi32(v_sad_thresh, s0));
		base_index = _mm_add_epi32(base_index, _mm_set1_epi32(4));
	}

	// tail
	for (; i < count; ++i)
	{
		const Vec128 sad = _mm_sad_epu8(load128u(index_batch + i*16), ref_inds);

		// Sum the two halves
		const Vec128 sum = _mm_add_epi16(sad, shuffle32<2,3,0,1>(sad));

		positions[out_count] = (int)i;
		out_count += _mm_cvtsi128_si32(sum) < sad_thresh;
	}

	return (int)out_count;
}

int filter_by_index_sad(CpuDispatchFlags dispatch, int positions[], int count, const U8 * index_batch, const U8 ref_inds[16], int sad_thresh)
{
#ifdef DO_BUILD_AVX512
	if ( dispatch.AVX512() )
		return internal::filter_by_index_sad_AVX512(positions, count, index_batch, ref_inds, sad_thresh);
#endif

#ifdef DO_BUILD_AVX2
	if ( dispatch.AVX2() )
		return internal::filter_by_index_sad_AVX2(positions, count, index_batch, ref_inds, sad_thresh);
#endif

	return filter_by_index_sad_SSE2(positions, count, index_batch, ref_inds, sad_thresh);
}

int filter_by_index_sad_with_inds(CpuDispatchFlags dispatch, int remaining_positions[], U32 out_indices[], int count, const U8 * index_batch, const U32 * in_indices, const U8 ref_inds[16], int threshold)
{
#ifdef DO_BUILD_AVX512
	if ( dispatch.AVX512() )
		return internal::filter_by_index_sad_with_inds_AVX512(remaining_positions, out_indices, count, index_batch, in_indices, ref_inds, threshold);
#endif

#ifdef DO_BUILD_AVX2
	if ( dispatch.AVX2() )
		return internal::filter_by_index_sad_with_inds_AVX2(remaining_positions, out_indices, count, index_batch, in_indices, ref_inds, threshold);
#endif

	// Default implementation just does the filter first and the compaction
	// second in a separate loop.
	int filtered_count = filter_by_index_sad(dispatch, remaining_positions, count, index_batch, ref_inds, threshold);

	for (int i = 0; i < filtered_count; i++)
		out_indices[i] = in_indices[remaining_positions[i]];

	return filtered_count;
}

S32 compute_sad_bound_for_ssd_threshold(S32 delta_ssd_per_pixel_per_sad[16][8], int num_steps_per_pixel[16], S32 base_ssd, S32 ssd_limit, S32 * out_ssd_limit)
{
	// create sentinel
	RAD_ALIGN(U32, delta_ssd[16][8], 16);
	RAD_ALIGN(U32, cur_ssd[16], 16);
	RAD_ALIGN(U32, ssd_mask[4][4], 16) =
	{
		{ 0xffffffff,0,0,0 },
		{ 0,0xffffffff,0,0 },
		{ 0,0,0xffffffff,0 },
		{ 0,0,0,0xffffffff },
	};

	// clamp ssd_limit so that we can use it as a limit per pixel and leave room for lane IDs
	ssd_limit = RR_MIN(ssd_limit, (1 << 28)-2);

	Vec128 vi = _mm_setzero_si128();
	Vec128 limit = _mm_set1_epi32(ssd_limit+1);
	Vec128 one = _mm_set1_epi32(1);
	for (int i=0; i < 16; ++i)
	{
		Vec128 a = _mm_min_epu32(load128u(delta_ssd_per_pixel_per_sad[i]+0),limit);
		Vec128 b = _mm_min_epu32(load128u(delta_ssd_per_pixel_per_sad[i]+4),limit);
		a = _mm_add_epi32(_mm_slli_epi32(a,4),vi);
		b = _mm_add_epi32(_mm_slli_epi32(b,4),vi);
		vi = _mm_add_epi32(vi, one);
		store128a(delta_ssd[i]+0, a);
		store128a(delta_ssd[i]+4, b);
	}

	for (int i=0; i < 16; ++i)
	{
		// create sentinel
		delta_ssd[i][num_steps_per_pixel[i]] = (((U32) ssd_limit+1) << 4)+i;
		cur_ssd[i] = delta_ssd[i][0];
	}

	int cur_off[16] = { 0 };
	S32 total_ssd = base_ssd;

	U32 smallest_ssd;
	int sad;
	for (sad=1;;++sad)
	{
		// find the smallest ssd delta
		#if 0
		smallest_ssd = (ssd_limit+1)<<4;
		for (int p=0; p < 16; ++p)
		{
			if (cur_ssd[p] < smallest_ssd)
				smallest_ssd = cur_ssd[p];
		}
		#else
		Vec128 a = load128a(cur_ssd+0);
		Vec128 b = load128a(cur_ssd+4);
		Vec128 c = load128a(cur_ssd+8);
		Vec128 d = load128a(cur_ssd+12);
		Vec128 ab = _mm_min_epu32(a,b);
		Vec128 cd = _mm_min_epu32(c,d);
		Vec128 abcd4 = _mm_min_epu32(ab,cd);
		Vec128 abcd2 = reduce_min_u32_2away(abcd4);
		Vec128 abcd1 = reduce_min_u32_1away(abcd2);
		smallest_ssd = _mm_cvtsi128_si32(abcd1);
		#endif

		S32 next_ssd = total_ssd + (smallest_ssd>>4);
		if (next_ssd > ssd_limit)
			break;
		total_ssd = next_ssd;
		int smallest_p = smallest_ssd & 15;

		#if 0
		cur_ssd[smallest_p] = delta_ssd[smallest_p][++cur_off[smallest_p]];
		#else
		// writing to a single value might cause a store forwarding stall due to
		// reading a larger value than written, so masking and merging instead makes
		// this function about 20% faster on Skylake
		int slot = smallest_p & 12;
		int field = smallest_p & 3;
		U32 next_value = delta_ssd[smallest_p][++cur_off[smallest_p]];
		Vec128 x = load128a(&cur_ssd[slot]);
		Vec128 mask = load128a(ssd_mask[field]);
		Vec128 all = _mm_set1_epi32(next_value);
		Vec128 merged = _mm_blendv_epi8(x, all, mask);
		store128a(&cur_ssd[slot], merged);
		#endif
	}
	if ((S32) (smallest_ssd >> 4) > ssd_limit)
	{
		if ( out_ssd_limit ) *out_ssd_limit = ssd_limit;
		return 16*256; // maximum index sad if we consumed all entries (indicated by smallest_ssd being a sentinel)
	}
	if ( out_ssd_limit ) *out_ssd_limit = total_ssd;
	return sad-1;
}

static Vec128 dist_to_bbox_SSE4(const Vec128 & v, const Vec128 & bbox_lo, const Vec128 & bbox_hi)
{
	Vec128 axial_dist8 = _mm_add_epi8(_mm_subs_epu8(bbox_lo, v), _mm_subs_epu8(v, bbox_hi));

	Vec128 even_dist16 = _mm_and_si128(axial_dist8, _mm_set1_epi16(0xff));
	Vec128 odd_dist16 = _mm_srli_epi16(axial_dist8, 8);

	Vec128 dots1 = _mm_madd_epi16(even_dist16, even_dist16);
	Vec128 dots2 = _mm_madd_epi16(odd_dist16, odd_dist16);

	return _mm_add_epi32(dots1, dots2);
}

static int filter_endpoint_color_bbox_SSE4(
	int * dest_inds,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColor32BGRA bbox[2],
	U32 bbox_min_d, bool require_3c)
{
	SINTa count = count32;
	SINTa out_count = 0;
	SINTa i = 0;

	RR_ASSERT(count < 65536);

	Vec128 bbox_lo = _mm_set1_epi32(bbox[0].dw);
	Vec128 bbox_hi = _mm_set1_epi32(bbox[1].dw);
	Vec128 lane_ids = _mm_setr_epi32(0, 1<<16, 2<<16, 3<<16);
	Vec128 vec_min_d = _mm_set1_epi32(bbox_min_d + 1);
	Vec128 mask_require_3c = _mm_set1_epi32(require_3c ? -1 : 0);

	for (; i < (count & ~3); i += 4)
	{
		// Load 4 endpoints
		Vec128 ep_in0 = load64u(vqendpoints[inds[i + 0]].palette);
		Vec128 ep_in1 = load64u(vqendpoints[inds[i + 1]].palette);
		Vec128 ep_in2 = load64u(vqendpoints[inds[i + 2]].palette);
		Vec128 ep_in3 = load64u(vqendpoints[inds[i + 3]].palette);

		// Transpose to 2 4-vectors of (endpoint0,endpoint1)
		Vec128 xpose0 = _mm_unpacklo_epi32(ep_in0, ep_in2); // (blk[0].ep0, blk[2].ep0, blk[0].ep1, blk[2].ep1)
		Vec128 xpose1 = _mm_unpacklo_epi32(ep_in1, ep_in3); // (blk[1].ep0, blk[3].ep0, blk[1].ep1, blk[3].ep1)

		Vec128 ep0 = _mm_unpacklo_epi32(xpose0, xpose1); // all ep0s
		Vec128 ep1 = _mm_unpackhi_epi32(xpose0, xpose1); // all ep1s
		Vec128 avg = _mm_avg_epu8(ep0, ep1);

		// Compute distances to bbox
		Vec128 dist_to_bbox_ep0 = dist_to_bbox_SSE4(ep0, bbox_lo, bbox_hi);
		Vec128 dist_to_bbox_ep1 = dist_to_bbox_SSE4(ep1, bbox_lo, bbox_hi);
		Vec128 dist_to_bbox_avg = dist_to_bbox_SSE4(avg, bbox_lo, bbox_hi);

		// We want to reject the candidate if the minimum of all 3 distances is > min_d
		// i.e. accept if min_dist <= min_d <=> min_d + 1 > min_dist
		Vec128 min_dist = _mm_min_epi32(dist_to_bbox_ep0, dist_to_bbox_ep1);
		min_dist = _mm_min_epi32(min_dist, dist_to_bbox_avg);

		// vec_min_d > min_dist -> accept!
		Vec128 distance_accept = _mm_cmpgt_epi32(vec_min_d, min_dist);

		// Block mode test: if require_3c is set and the block selects 4-color mode, reject it.
		// 4-color mode is when ep0 > ep1 in the packed 16-bit representation; because unpacking
		// the color endpoints is strictly monotonic and order-preserving, we can compare
		// ep0 to ep1. We would like an unsigned compare, however both ep0 and ep1 always have
		// their alpha components set to 0xff; this makes them appear both negative, but they
		// will still order correctly under a signed compare. (The cases that give incorrect
		// results with the wrong type of compare is when one value is negative and the other is
		// not.)
		Vec128 block_4c = _mm_cmpgt_epi32(ep0, ep1); // ep0 > ep1 selects 4c mode (see notes above)
		Vec128 mode_reject = _mm_and_si128(block_4c, mask_require_3c);

		// Put indices into top half of output int,
		// filtered i in low half
		Vec128 indices = load128u(inds + i);
		Vec128 values = _mm_or_si128(indices, lane_ids);

		// Block is active if distance_accept and no mode_reject
		Vec128 active = _mm_andnot_si128(mode_reject, distance_accept);

		out_count += store_active_values(dest_inds + out_count, values, active);
		lane_ids = _mm_add_epi32(lane_ids, _mm_set1_epi32(4<<16));
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

int filter_endpoint_color_bbox(
	CpuDispatchFlags dispatch,
	int * dest_inds,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColor32BGRA bbox[2],
	U32 bbox_min_d, bool require_3c)
{
#ifdef DO_BUILD_AVX512
	if ( dispatch.AVX512() )
	{
		return internal::filter_endpoint_color_bbox_AVX512(
			dest_inds,
			vqendpoints, inds, count32,
			bbox,
			bbox_min_d, require_3c);
	}
#endif
#ifdef DO_BUILD_AVX2
	if ( dispatch.AVX2() )
	{
		return internal::filter_endpoint_color_bbox_AVX2(
			dest_inds,
			vqendpoints, inds, count32,
			bbox,
			bbox_min_d, require_3c);
	}
#endif

	return filter_endpoint_color_bbox_SSE4(
		dest_inds,
		vqendpoints, inds, count32,
		bbox,
		bbox_min_d, require_3c);
}

static RADFORCEINLINE VecF32x4 batch_VQD_weight_calc(const Vec128 & pixel, const Vec128 & pal_vec, const VecF32x4 & weight)
{
	// Compute the absolute differences in 8 bits and form
	// 16-bit differences for even/odd channel pairs
	Vec128 sub8 = _mm_or_si128( _mm_subs_epu8(pixel, pal_vec), _mm_subs_epu8(pal_vec,pixel) );
	Vec128 sub16_1 = _mm_and_si128(sub8, _mm_set1_epi16(0xff)); // 16-bit: R, B, R, B, ...
	Vec128 sub16_2 = _mm_srli_epi16(sub8, 8); // 16-bit: G, A, G, A, ...

	// this squares and horizontally adds pairs
	//	we go from 16 bits * 4 colors channels * 4 pixels
	//	-> 32 bits with {R+B} (squares32_1), {G+A} (squares32_2)
	Vec128 squares32_1 = _mm_madd_epi16(sub16_1, sub16_1);
	Vec128 squares32_2 = _mm_madd_epi16(sub16_2, sub16_2);

	// add the halves together to get R+G+B+A
	Vec128 squares32 = _mm_add_epi32(squares32_1, squares32_2);

	// convert to float and multiply with weight
	return VecF32x4::from_int32(squares32) * weight;
}

static RADFORCEINLINE VecF32x4 batch_VQD_shuffle_one(const VecF32x4 vec, const Vec128 & shuffle_inds)
{
	return _mm_castsi128_ps(_mm_shuffle_epi8(_mm_castps_si128(vec), shuffle_inds));
}

static RADFORCEINLINE void batch_VQD_sum_along_col(VecF32x4 & out_sum0, VecF32x4 & out_sum1, const VecF32x4 errors[][2], const Vec128 & inds_shl2)
{
	const Vec128 c_byte_inds = _mm_set1_epi32(0x03020100); // select all 4 bytes of the DWord we need

	// Indices are pre-shfited by 2, but still need to mask the 2 index bits we need
	const Vec128 inds_masked = _mm_and_si128(inds_shl2, _mm_set1_epi8(3 << 2));

	// Now, for our 4 summands, form the final index, and sum down the column
	Vec128 shuf_index;

	shuf_index = _mm_or_si128(_mm_shuffle_epi8(inds_masked, _mm_setr_epi8(0,0,0,0, 4,4,4,4, 8,8,8,8, 12,12,12,12)), c_byte_inds);
	out_sum0 = batch_VQD_shuffle_one(errors[0][0], shuf_index);
	out_sum1 = batch_VQD_shuffle_one(errors[0][1], shuf_index);

	shuf_index = _mm_or_si128(_mm_shuffle_epi8(inds_masked, _mm_setr_epi8(1,1,1,1, 5,5,5,5, 9,9,9,9, 13,13,13,13)), c_byte_inds);
	out_sum0 += batch_VQD_shuffle_one(errors[4][0], shuf_index);
	out_sum1 += batch_VQD_shuffle_one(errors[4][1], shuf_index);

	shuf_index = _mm_or_si128(_mm_shuffle_epi8(inds_masked, _mm_setr_epi8(2,2,2,2, 6,6,6,6, 10,10,10,10, 14,14,14,14)), c_byte_inds);
	out_sum0 += batch_VQD_shuffle_one(errors[8][0], shuf_index);
	out_sum1 += batch_VQD_shuffle_one(errors[8][1], shuf_index);

	shuf_index = _mm_or_si128(_mm_shuffle_epi8(inds_masked, _mm_setr_epi8(3,3,3,3, 7,7,7,7, 11,11,11,11, 15,15,15,15)), c_byte_inds);
	out_sum0 += batch_VQD_shuffle_one(errors[12][0], shuf_index);
	out_sum1 += batch_VQD_shuffle_one(errors[12][1], shuf_index);
}

void batch_compute_VQDs_SSE4(
	F32 * dest_vqd,
	const rrDXT1_VQ_Entry * vqindices, const int * inds, int count32,
	const rrColorBlock4x4 & colors,
	const rrColor32BGRA first_palette[4],
	const rrColor32BGRA second_palette[4],
	const SingleFloatBlock4x4 & activity)
{
	const SINTa count = count32;

	// Precompute weighted SSDs for all 4 possible choices of index from either palette
	// at all of the 16 source pixels
	const Vec128 pal0 = load128u(first_palette);
	const Vec128 pal1 = load128u(second_palette);
	VecF32x4 weighted_errs[16][2]; // [pixel][palette]; lanes are the 4 possible index choices
	RAD_ALIGN(F32, tail_buf[8], 16);
	SINTa i;

	for (i = 0; i < 16; ++i)
	{
		// Broadcast pixel and activity to all lanes
		Vec128 pixel = _mm_set1_epi32(colors.colors[i].dw);
		VecF32x4 weight(activity.values[i]);

		weighted_errs[i][0] = batch_VQD_weight_calc(pixel, pal0, weight);
		weighted_errs[i][1] = batch_VQD_weight_calc(pixel, pal1, weight);
	}

	// Now our main work loop selects from the precomputed weighted errors using the
	// index vector, for 4 blocks at a time.
	for (i = 0; i < count; i += 4)
	{
		F32 * dest_ptr = dest_vqd + i * 2;
		Vec128 index_vec;

		if (i + 3 < count)
		{
			// This batch of indices is fully inside the range
			index_vec = _mm_setr_epi32(
				vqindices[inds[i + 0]].dw,
				vqindices[inds[i + 1]].dw,
				vqindices[inds[i + 2]].dw,
				vqindices[inds[i + 3]].dw
			);
		}
		else
		{
			// Partial run; we know i < count, not sure about the other ones.
			// Also store to tail buf.
			dest_ptr = tail_buf;

			index_vec = _mm_set1_epi32(vqindices[inds[i + 0]].dw);
			if (i + 1 < count) index_vec = _mm_insert_epi32(index_vec, vqindices[inds[i + 1]].dw, 1);
			if (i + 2 < count) index_vec = _mm_insert_epi32(index_vec, vqindices[inds[i + 2]].dw, 2);
		}

		// We process the 4 columns of pixels separately because that's our summation order in
		// regular VQD, which we have to match. Since that means we grab 2-bit indices separated
		// by distances of 8 bits, this is also convenient to set up PSHUFB masks with.
		VecF32x4 sum0p0, sum1p0, sum2p0, sum3p0;
		VecF32x4 sum0p1, sum1p1, sum2p1, sum3p1;

		// That's it for prep work, start summing.
		// NOTE we sum in 4 chains, one per column, and add them together in the end in
		// a particular order, to match the exact computation regular VQD does.
		// BE VERY CAREFUL HERE! We want to guarantee exactly identical results no
		// matter which VQD impl is used.

		// Because the two palettes can share index computation, we do them at the same time.
		batch_VQD_sum_along_col(sum0p0, sum0p1, weighted_errs + 0, _mm_slli_epi32(index_vec, 2));
		batch_VQD_sum_along_col(sum1p0, sum1p1, weighted_errs + 1, index_vec);
		batch_VQD_sum_along_col(sum2p0, sum2p1, weighted_errs + 2, _mm_srli_epi32(index_vec, 2));
		batch_VQD_sum_along_col(sum3p0, sum3p1, weighted_errs + 3, _mm_srli_epi32(index_vec, 4));

		// VQD reduction sums inner pairs first, then outer pairs
		sum0p0 += sum1p0;
		sum0p1 += sum1p1;
		sum2p0 += sum3p0;
		sum2p1 += sum3p1;

		sum0p0 += sum2p0;
		sum0p1 += sum2p1;

		// Store the results interleaved
		_mm_storeu_ps(dest_ptr + 0, _mm_unpacklo_ps(sum0p0, sum0p1));
		_mm_storeu_ps(dest_ptr + 4, _mm_unpackhi_ps(sum0p0, sum0p1));
	}

	// Copy tail results if present
	if (SINTa rest = count & 3)
		memcpy(dest_vqd + (count & ~3) * 2, tail_buf, rest * 2 * sizeof(F32));
}

void batch_compute_VQDs(
	CpuDispatchFlags dispatch,
	F32 * dest_vqd,
	const rrDXT1_VQ_Entry * vqindices, const int * inds, int count32,
	const rrColorBlock4x4 & colors,
	const rrColor32BGRA first_palette[4],
	const rrColor32BGRA second_palette[4],
	const SingleFloatBlock4x4 & activity)
{
#ifdef DO_BUILD_AVX512
	if ( dispatch.AVX512() )
	{
		internal::batch_compute_VQDs_AVX512(dest_vqd,
			vqindices, inds, count32,
			colors, first_palette, second_palette,
			activity);
		return;
	}
#endif

#ifdef DO_BUILD_AVX2
	if ( dispatch.AVX2() )
	{
		internal::batch_compute_VQDs_AVX2(dest_vqd,
			vqindices, inds, count32,
			colors, first_palette, second_palette,
			activity);
		return;
	}
#endif

	batch_compute_VQDs_SSE4(
		dest_vqd,
		vqindices, inds, count32,
		colors, first_palette, second_palette,
		activity);
}

void batch_find_best_errors_SSE4(
	S32 * dest_ssd,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColorBlock4x4 & colors)
{
	using namespace internal;
	const SINTa count = count32;
	Vec128 pix_br[4], pix_ga[4]; // for the rows

	// Initial pre-scaling of pixels
	for (SINTa i = 0; i < 4; ++i)
		UnpackBGRAto16_Weighted(pix_br[i], pix_ga[i], load128u(colors.colors + i*4));

	// Score one candidate palette at a time
	for (SINTa i = 0; i < count; ++i)
	{
		// Load and set up the palette
		Vec128 pal_br16, pal_ga16;
		SINTa idx = inds[i] & 0xffff;
		UnpackBGRAto16_Weighted(pal_br16, pal_ga16, load128u(vqendpoints[idx].palette));

		// Sum the errors
		Vec128 error_sum;
		error_sum = BC1_FindBestErrors_SSE4(pix_br[0], pix_ga[0], pal_br16, pal_ga16);
		error_sum = _mm_add_epi32(error_sum, BC1_FindBestErrors_SSE4(pix_br[1], pix_ga[1], pal_br16, pal_ga16));
		error_sum = _mm_add_epi32(error_sum, BC1_FindBestErrors_SSE4(pix_br[2], pix_ga[2], pal_br16, pal_ga16));
		error_sum = _mm_add_epi32(error_sum, BC1_FindBestErrors_SSE4(pix_br[3], pix_ga[3], pal_br16, pal_ga16));

		// Finish reduction and store the result
		dest_ssd[i] = reduce_add_s32(error_sum);
	}
}

void batch_find_best_errors(
	CpuDispatchFlags dispatch,
	S32 * dest_ssd,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColorBlock4x4 & colors)
{
#ifdef DO_BUILD_AVX512
	if ( dispatch.AVX512() )
	{
		internal::batch_find_best_errors_AVX512(dest_ssd,
			vqendpoints, inds, count32,
			colors);
		return;
	}
#endif

#ifdef DO_BUILD_AVX2
	if ( dispatch.AVX2() )
	{
		internal::batch_find_best_errors_AVX2(dest_ssd,
			vqendpoints, inds, count32,
			colors);
		return;
	}
#endif

	batch_find_best_errors_SSE4(dest_ssd,
		vqendpoints, inds, count32,
		colors);
}

#else // DO_BUILD_SSE4

void AnyIndexD_batch_lookup(CpuDispatchFlags /*dispatch*/, S32 out_d[], const AnyIndexD * aid, const U32 in_indices[], int count)
{
	for (int i = 0; i < count; ++i)
		out_d[i] = AnyIndexD_lookup(aid, in_indices[i]);
}

void AnyIndex8D64_batch_lookup(CpuDispatchFlags /*dispatch*/, S64 out_d[], const AnyIndex8D64 * aid, const U8 *indices_interleaved, const U8 *indices_scaled, ptrdiff_t indices_step, int count)
{
	for (int i = 0; i < count; ++i)
		out_d[i] = AnyIndex8D64_lookup(aid, indices_scaled + i*indices_step);
}

void compute_chunk_sads_indirect(CpuDispatchFlags /*dispatch*/, int chunk_sads[], int count, const U16 indices[], const rrColorBlock4x4 * pair_colors, const rrColorBlock4x4 & rColors)
{
	RAD_ALIGN(const rrColorBlock4x4, colors, 16) = rColors;

	for (int i = 0; i < count; ++i)
		chunk_sads[i] = ColorBlock4x4_ComputeSAD_RGBA(pair_colors[indices[i]],colors);
}

int filter_by_index_sad(CpuDispatchFlags /*dispatch*/, int positions[], int count, const U8 * index_batch, const U8 ref_inds[16], int sad_thresh)
{
	RAD_ALIGN(U8, ref_inds_local[16], 16);
	memcpy(ref_inds_local, ref_inds, 16);
	int out_count = 0;

	for (int i = 0; i < count; ++i)
	{
		int sad = unpacked_16x8_diff(ref_inds_local,index_batch + i*16);
		positions[out_count] = i;
		out_count += sad < sad_thresh;
	}

	return out_count;
}

int filter_by_index_sad_with_inds(CpuDispatchFlags dispatch, int remaining_positions[], U32 out_indices[], int count, const U8 * index_batch, const U32 * in_indices, const U8 ref_inds[16], int threshold)
{
	int filtered_count = filter_by_index_sad(dispatch, remaining_positions, count, index_batch, ref_inds, threshold);

	for (int i = 0; i < filtered_count; i++)
		out_indices[i] = in_indices[remaining_positions[i]];

	return filtered_count;
}

S32 compute_sad_bound_for_ssd_threshold(S32 ssd_for_index_delta[16][8], int index_delta_count[16], S32 base_ssd, S32 ssd_limit, S32 * out_ssd_limit)
{
	int done=0;
	int index_delta_cur[16] = { 0 };
	S32 total_ssd = base_ssd;
	S32 smallest_ssd = 0x7fffffff;
	int sad;
	for (sad=1; done < 16; ++sad)
	{
		// find the smallest ssd delta
		smallest_ssd = (S32) 0x7fffffff;
		int smallest_p=0;
		for (int p=0; p < 16; ++p)
		{
			if (index_delta_cur[p] < index_delta_count[p])
			{
				if (ssd_for_index_delta[p][index_delta_cur[p]] < smallest_ssd)
				{
					smallest_ssd = ssd_for_index_delta[p][index_delta_cur[p]];
					smallest_p = p;
				}
			}
		}
		RR_ASSERT(smallest_ssd != (S32) 0x7fffffff);
		S32 next_ssd = total_ssd + smallest_ssd;
		if (next_ssd > ssd_limit)
			break;
		total_ssd = next_ssd;
	    ++index_delta_cur[smallest_p];
		if (index_delta_cur[smallest_p] == index_delta_count[smallest_p])
			++done;
	}

	if ( smallest_ssd > ssd_limit )
	{
		if ( out_ssd_limit ) *out_ssd_limit = ssd_limit;
		return 16*256; // maximum index SAD if we consumed all entries (indicated by smallest_ssd being a sentinel)
	}

	if ( out_ssd_limit ) *out_ssd_limit = total_ssd;
	return sad-1;
}

int filter_endpoint_color_bbox(
	CpuDispatchFlags /*dispatch*/,
	int * dest_inds,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColor32BGRA bbox[2],
	U32 bbox_min_d, bool require_3c)
{
	SINTa count = count32;
	SINTa out_count = 0;

	RR_ASSERT(count < 65536);

	for (SINTa i = 0; i < count; ++i)
	{
		const int idx = inds[i];
		const rrDXT1_VQ_Entry & vqendpoint = vqendpoints[inds[i]];

		// Block type check (always check as if in alpha pal mode, callers sets up
		// require_3c appropriately)
		bool vqendpoint_is4c = DXT1_Is4Color(vqendpoint.dw,rrDXT1PaletteMode_Alpha);

		// NOTE(fg): this doesn't cull particularly great on many textures, but in this form
		// it's cheap enough that it's definitely worth doing; I tried better (in terms of
		// rejection rate) tests and they ended up slower, but that was before pulling this out.

		#if 0 //def __RADSSE2__
		// TEMP check
		RR_ASSERT( reject_endpoints_color_bbox_sse4(vqendpoint.palette,bbox,bbox_min_d)
			== reject_endpoints_color_bbox_scalar(vqendpoint.palette,bbox,bbox_min_d) );
		#endif

		// palette[0],[1] are endpoints
		//	color_bbox is only the RGB non-black part
		bool rejected = reject_endpoints_color_bbox(vqendpoint.palette,bbox,bbox_min_d);
		rejected |= vqendpoint_is4c & require_3c;

		dest_inds[out_count] = (int) ((i << 16) | idx);
		out_count += rejected ? 0 : 1;
	}

	return (int)out_count;
}

void batch_compute_VQDs(
	CpuDispatchFlags /*dispatch*/,
	F32 * dest_vqd,
	const rrDXT1_VQ_Entry * vqindices, const int * inds, int count32,
	const rrColorBlock4x4 & colors,
	const rrColor32BGRA first_palette[4],
	const rrColor32BGRA second_palette[4],
	const SingleFloatBlock4x4 & activity)
{
	const SINTa count = count32;

	for (SINTa i = 0; i < count; ++i)
	{
		U32 vqp_indices = vqindices[inds[i]].dw;

		dest_vqd[i*2 + 0] = VQD_BC1(colors,first_palette, vqp_indices,activity);
		dest_vqd[i*2 + 1] = VQD_BC1(colors,second_palette,vqp_indices,activity);
	}
}

void batch_find_best_errors(
	CpuDispatchFlags dispatch,
	S32 * dest_ssd,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColorBlock4x4 & colors)
{
	const SINTa count = count32;
	DXT1_FindErrorsContext ctx;

	ctx.init(dispatch, colors);

	for (SINTa i = 0; i < count; ++i)
	{
		SINTa idx = inds[i] & 0xffff;
		dest_ssd[i] = ctx.eval_palette(&ctx, vqendpoints[idx].palette);
	}
}
#endif // DO_BUILD_SSE4

// NOTE(fg): this is in its own section so I can turn the "#ifdef DO_BUILD_SSE4" for the
// above section into a "#if 0" for testing that all the fall-backs work. This breaks BC4RD
// when those two are in there, so separate them out.

#ifdef DO_BUILD_SSE4

S32 bc4rd_single_block_change_distortion_explicit_SSE4(const S16 *src_pixels, const S16 palette[8], const U8 * index8_scaled_ptr)
{
	Vec128 pal = load128u(palette);
	Vec128 index8_scaled = load128u(index8_scaled_ptr);

	// Second index to grab second half of 16-bit word
	Vec128 index8_scaled_plus_1 = _mm_add_epi8(index8_scaled, _mm_set1_epi8(1));

	// Our palette LUT entirely fits in a single vector so all we need to do is look up those
	// table entries via PSHUFB
	Vec128 index16_0 = _mm_unpacklo_epi8(index8_scaled, index8_scaled_plus_1);
	Vec128 index16_1 = _mm_unpackhi_epi8(index8_scaled, index8_scaled_plus_1);

	Vec128 result_0 = _mm_shuffle_epi8(pal, index16_0);
	Vec128 result_1 = _mm_shuffle_epi8(pal, index16_1);

	// load all 16 pixels and diff them
	Vec128 diff01_16 = _mm_sub_epi16(result_0, load128u(src_pixels + 0));
	Vec128 diff23_16 = _mm_sub_epi16(result_1, load128u(src_pixels + 8));

	// Dot products with itself
	Vec128 dot01 = _mm_madd_epi16(diff01_16, diff01_16);
	Vec128 dot23 = _mm_madd_epi16(diff23_16, diff23_16);

	// Horizontal reduction
	Vec128 dots = _mm_add_epi32(dot01, dot23); // add to 4-away
	S32 ssd = reduce_add_s32(dots);
	return ssd;
}

S32 bc4rd_single_block_change_distortion_explicit(const S16 *src_pixels, const S16 palette[8], const U8 * index8_scaled_ptr)
{
#ifdef DO_BUILD_AVX2
	if ( rrCPUx86_feature_present(RRX86_CPU_AVX2) )
		return internal::bc4rd_single_block_change_distortion_AVX2(src_pixels, palette, index8_scaled_ptr);
#endif
	return bc4rd_single_block_change_distortion_explicit_SSE4(src_pixels, palette, index8_scaled_ptr);
}

#endif

void FinalMatrixL1Index::build(const rrColorBlock4x4 * blocks, int nblocks)
{
	// Sentinel value for SIMD lookups.
	// Note actual SADs are in [0,64*255] = [0,16320].
	//
	// This value is 32767 and not 65535 because the SSE2 lookups
	// actually use _signed_ 16-bit compares (since SSE2 has no
	// direct unsigned compares). With the value ranges involved,
	// either is fine.
	static const U16 SENTINEL = 32767;

	RR_ASSERT(0 <= nblocks && nblocks <= CHUNK_SIZE);
	U32 sorted_norm_inds[CHUNK_SIZE];

	count = nblocks;
	rrColorBlock4x4 zero_colors = {}; // zero-init

	// Prepare (norm,index) pairs
	for LOOP(i,nblocks)
		sorted_norm_inds[i] = (ColorBlock4x4_ComputeSAD_RGBA(blocks[i],zero_colors) << 16) | i;

	stdsort(sorted_norm_inds, sorted_norm_inds + nblocks);

	for LOOP(i,nblocks)
	{
		sorted_norms[i] = (U16)(sorted_norm_inds[i] >> 16);
		sorted_inds[i] = (U16)(sorted_norm_inds[i] & 0xffff);
	}

	// Prepare the padding used by the SIMD search
	for LOOP(i,16)
		sorted_norms[nblocks + i] = SENTINEL;

	// And then the subsampled keys
	// keep the largest key from each interval of 16,
	// then pad with the sentinel value.
	int nsub = nblocks / 16;
	for LOOP(i,nsub)
		sorted_norms_sub[i] = sorted_norms[i*16 + 15];

	for (int i = nsub; i < RR_ARRAY_SIZE(sorted_norms_sub); ++i)
		sorted_norms_sub[i] = SENTINEL;
}

// Bit-packed positions of bits that are set in value i. Generated by this Python script:
//
// for i in range(256):
//     value = 0
//     k = 0
//     for j in range(8):
//         if i & (1 << j) != 0:
//             value |= j << (k*4)
//             k += 1
//
//     print('0x{:08x},'.format(value), end=(' ' if (i & 7) != 7 else '\n'))
U32 c_active_lane_table8[256] =
{
	0x00000000, 0x00000000, 0x00000001, 0x00000010, 0x00000002, 0x00000020, 0x00000021, 0x00000210,
	0x00000003, 0x00000030, 0x00000031, 0x00000310, 0x00000032, 0x00000320, 0x00000321, 0x00003210,
	0x00000004, 0x00000040, 0x00000041, 0x00000410, 0x00000042, 0x00000420, 0x00000421, 0x00004210,
	0x00000043, 0x00000430, 0x00000431, 0x00004310, 0x00000432, 0x00004320, 0x00004321, 0x00043210,
	0x00000005, 0x00000050, 0x00000051, 0x00000510, 0x00000052, 0x00000520, 0x00000521, 0x00005210,
	0x00000053, 0x00000530, 0x00000531, 0x00005310, 0x00000532, 0x00005320, 0x00005321, 0x00053210,
	0x00000054, 0x00000540, 0x00000541, 0x00005410, 0x00000542, 0x00005420, 0x00005421, 0x00054210,
	0x00000543, 0x00005430, 0x00005431, 0x00054310, 0x00005432, 0x00054320, 0x00054321, 0x00543210,
	0x00000006, 0x00000060, 0x00000061, 0x00000610, 0x00000062, 0x00000620, 0x00000621, 0x00006210,
	0x00000063, 0x00000630, 0x00000631, 0x00006310, 0x00000632, 0x00006320, 0x00006321, 0x00063210,
	0x00000064, 0x00000640, 0x00000641, 0x00006410, 0x00000642, 0x00006420, 0x00006421, 0x00064210,
	0x00000643, 0x00006430, 0x00006431, 0x00064310, 0x00006432, 0x00064320, 0x00064321, 0x00643210,
	0x00000065, 0x00000650, 0x00000651, 0x00006510, 0x00000652, 0x00006520, 0x00006521, 0x00065210,
	0x00000653, 0x00006530, 0x00006531, 0x00065310, 0x00006532, 0x00065320, 0x00065321, 0x00653210,
	0x00000654, 0x00006540, 0x00006541, 0x00065410, 0x00006542, 0x00065420, 0x00065421, 0x00654210,
	0x00006543, 0x00065430, 0x00065431, 0x00654310, 0x00065432, 0x00654320, 0x00654321, 0x06543210,
	0x00000007, 0x00000070, 0x00000071, 0x00000710, 0x00000072, 0x00000720, 0x00000721, 0x00007210,
	0x00000073, 0x00000730, 0x00000731, 0x00007310, 0x00000732, 0x00007320, 0x00007321, 0x00073210,
	0x00000074, 0x00000740, 0x00000741, 0x00007410, 0x00000742, 0x00007420, 0x00007421, 0x00074210,
	0x00000743, 0x00007430, 0x00007431, 0x00074310, 0x00007432, 0x00074320, 0x00074321, 0x00743210,
	0x00000075, 0x00000750, 0x00000751, 0x00007510, 0x00000752, 0x00007520, 0x00007521, 0x00075210,
	0x00000753, 0x00007530, 0x00007531, 0x00075310, 0x00007532, 0x00075320, 0x00075321, 0x00753210,
	0x00000754, 0x00007540, 0x00007541, 0x00075410, 0x00007542, 0x00075420, 0x00075421, 0x00754210,
	0x00007543, 0x00075430, 0x00075431, 0x00754310, 0x00075432, 0x00754320, 0x00754321, 0x07543210,
	0x00000076, 0x00000760, 0x00000761, 0x00007610, 0x00000762, 0x00007620, 0x00007621, 0x00076210,
	0x00000763, 0x00007630, 0x00007631, 0x00076310, 0x00007632, 0x00076320, 0x00076321, 0x00763210,
	0x00000764, 0x00007640, 0x00007641, 0x00076410, 0x00007642, 0x00076420, 0x00076421, 0x00764210,
	0x00007643, 0x00076430, 0x00076431, 0x00764310, 0x00076432, 0x00764320, 0x00764321, 0x07643210,
	0x00000765, 0x00007650, 0x00007651, 0x00076510, 0x00007652, 0x00076520, 0x00076521, 0x00765210,
	0x00007653, 0x00076530, 0x00076531, 0x00765310, 0x00076532, 0x00765320, 0x00765321, 0x07653210,
	0x00007654, 0x00076540, 0x00076541, 0x00765410, 0x00076542, 0x00765420, 0x00765421, 0x07654210,
	0x00076543, 0x00765430, 0x00765431, 0x07654310, 0x00765432, 0x07654320, 0x07654321, 0x76543210
};

RR_NAMESPACE_END
