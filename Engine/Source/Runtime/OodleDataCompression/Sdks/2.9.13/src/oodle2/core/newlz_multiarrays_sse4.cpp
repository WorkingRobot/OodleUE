// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetsse4

#include "newlz_simd.h"
#include "cbradutil.h"

OODLE_NS_START

//==================================================================================================

#ifdef DO_BUILD_SSE4

void newlz_multiarrays_trellis_core_sse4(
	U64 * switch_flags,
	U8  * cheapest_entropyset,
	const U8 * ptr, const U8 * base, const U8 *end,
	const U16 * entropyset_cost,
	const U16 * histo_codelens_transposed,
	int num_entropysets,int num_entropysets_padded,
	U16 prev_cost_cheapest,U16 switch_histo_cost_codelen)
{
	// we will use vector v_prev_cost_cheapest here
	// take scalar prev_cost_cheapest from the pos==0 loop
	// henceforth we never update the scalar costs
	__m128i v_prev_cost_cheapest = _mm_set1_epi16((short)prev_cost_cheapest);
	__m128i v_switch_histo_cost_codelen = _mm_set1_epi16((short)switch_histo_cost_codelen);

	RR_ASSERT( rrIsAlignedPointer(entropyset_cost,16) );
	RR_ASSERT( rrIsAlignedPointer(histo_codelens_transposed,16) );

	if ( num_entropysets <= 8 )
	{
		// special fast case when num_entropysets_padded <= 8
		//	then cost just stays in a vector, no load/store
		//	you only need one horizontal PHMINPOSUW to find min
		//  it becomes quite tight indeed
		RR_ASSERT( num_entropysets_padded == 8 );

		__m128i v_entropyset_cost = _mm_load_si128((__m128i *)(entropyset_cost));
		__m128i pshufb_broadcast_word0 = _mm_set1_epi16(0x0100);

		// scalar entropyset_cost is no longer used, kept in vec

		while(ptr<end)
		{
			SINTa pos = rrPtrDiff(ptr - base);
			U32 sym = *ptr++;

			const U16 * codelens_for_sym = histo_codelens_transposed + 8 * sym;
			__m128i sym_cost = _mm_load_si128((__m128i *)(codelens_for_sym));

			__m128i cost_from_self = _mm_sub_epi16(v_entropyset_cost,v_prev_cost_cheapest);
			__m128i min_prev_cost = _mm_min_epi16(cost_from_self,v_switch_histo_cost_codelen);

			v_entropyset_cost = _mm_add_epi16(sym_cost,min_prev_cost);

			__m128i do_switch_v16 = _mm_cmpgt_epi16(cost_from_self,v_switch_histo_cost_codelen);
			__m128i do_switch_v8 = _mm_packs_epi16(do_switch_v16,_mm_setzero_si128());
			U64 switch_flag_set = _mm_movemask_epi8(do_switch_v8);
			RR_ASSERT( switch_flag_set < 256 );

			// just need horizontal min in v_entropyset_cost
			// find the min and replicate the min in every lane :

			__m128i phminposuw = _mm_minpos_epu16(v_entropyset_cost);

			// U16 phminposuw[0] = min value
			// U16 phminposuw[1] = min index

			int cur_i_cheapest = _mm_extract_epi16(phminposuw,1);

			RR_DURING_ASSERT( U16 cur_cost_cheapest = (U16) _mm_extract_epi16(phminposuw,0) );
			RR_ASSERT( cur_i_cheapest < num_entropysets );
			RR_ASSERT( ((S16 *)&v_entropyset_cost)[cur_i_cheapest] == cur_cost_cheapest );

			// broadcast min from phminposuw[0] to all lanes :
			v_prev_cost_cheapest = _mm_shuffle_epi8(phminposuw,pshufb_broadcast_word0);

			switch_flags[pos] = switch_flag_set;
			cheapest_entropyset[pos] = U8_check(cur_i_cheapest);
		}
	}
	else
	{
		__m128i v_lane_inc = _mm_setr_epi16(8,0,0,0, 0,0,0,0);
		__m128i v_broadcast_lane1_16b = _mm_setr_epi8(2,3, 2,3, 2,3, 2,3, 2,3, 2,3, 2,3, 2,3);

		while(ptr<end)
		{
			SINTa pos = rrPtrDiff(ptr - base);
			U32 sym = *ptr++;
			const U16 * codelens_for_sym = histo_codelens_transposed + (U32)num_entropysets_padded * sym;

			RR_ASSERT( rrIsAlignedPointer(codelens_for_sym,16) );

			U64 switch_flag_set = 0;

			// consider coding current byte with all histos :

			// each entropy set considers two arrivals
			// arrive from self, or cheapest other

			// v_prev_cost_cheapest is the cheapest cost in all lanes

			// start v_min_cost as large
			__m128i v_min_and_index = _mm_set1_epi32(RR_S32_MAX);
			__m128i v_base_lane = _mm_setzero_si128();

			for(int hi = 0; hi < num_entropysets_padded ;hi+=8)
			{
				// aligned loads
				__m128i sym_cost = _mm_load_si128((__m128i *)(codelens_for_sym+hi));
				__m128i cost_from_self = _mm_load_si128((__m128i *)(entropyset_cost+hi));

				// make cost relative to prev cheapest
				//	- this preps for the cmp and also keeps us bounded for S16 storage
				cost_from_self = _mm_sub_epi16(cost_from_self,v_prev_cost_cheapest);

				// I can arrive from self or a switch :
				__m128i min_prev_cost = _mm_min_epi16(cost_from_self,v_switch_histo_cost_codelen);
				// add on current symbol cost :
				__m128i cost = _mm_add_epi16(sym_cost,min_prev_cost);

				_mm_store_si128((__m128i *)(entropyset_cost+hi),cost);

				// find horizontal min & index :
				__m128i phminposuw = _mm_minpos_epu16(cost);
				// swap min words 0 & 1 :
				phminposuw = _mm_shufflelo_epi16(phminposuw,_MM_SHUFFLE(3,2,0,1));
				// cost min is bits [31:16], index of min lane in bits [15:0]
				// add in base lane ID
				phminposuw = _mm_add_epi16(phminposuw,v_base_lane);
				v_base_lane = _mm_add_epi16(v_base_lane,v_lane_inc);

				// keep track of min
				v_min_and_index = _mm_min_epi32(v_min_and_index,phminposuw);

				// if cost from self is > switch_cost , flag a switch in that lane :
				__m128i do_switch_v16 = _mm_cmpgt_epi16(cost_from_self,v_switch_histo_cost_codelen);
				__m128i do_switch_v8 = _mm_packs_epi16(do_switch_v16,_mm_setzero_si128());
				U64 do_switch_mask = _mm_movemask_epi8(do_switch_v8);
				RR_ASSERT( do_switch_mask < 256 );
				switch_flag_set |= do_switch_mask<<hi;
			}

			// get min cost :
			// min_and_index : first 16b is the index, second 16b is the value
			U8 cur_i_cheapest = (U8)_mm_extract_epi16(v_min_and_index,0);
			v_prev_cost_cheapest = _mm_shuffle_epi8(v_min_and_index,v_broadcast_lane1_16b);

			RR_DURING_ASSERT( int cur_cost_cheapest = _mm_extract_epi16(v_min_and_index,1); );
			RR_ASSERT( cur_i_cheapest < num_entropysets );
			RR_ASSERT( entropyset_cost[cur_i_cheapest] == cur_cost_cheapest );

			switch_flags[pos] = switch_flag_set;
			cheapest_entropyset[pos] = cur_i_cheapest;
		}
	}
}

#endif // DO_BUILD_SSE4

OODLE_NS_END

