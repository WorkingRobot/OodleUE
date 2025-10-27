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

OODLE_NS_END
#include <smmintrin.h>
OODLE_NS_START

#ifdef __GNUC__
#if ! defined(__SSE4_1__) 
#error need SSE4 set in compiler
#endif
#endif

void newlz_multiarrays_trellis_core_sse4(
	U64 * switch_flags,
	U8  * cheapest_entropyset,
	const U8 * ptr, const U8 * base, const U8 *end,
	const U16 * entropyset_cost,
	const U16 * histo_codelens_transposed,
	int num_entropysets,int num_entropysets_padded,
	int prev_cost_cheapest,int switch_histo_cost_codelen)
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
	
		__m128i pshufb_broadcast_word0 = _mm_set_epi8(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0);
			
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



void simd_mul_s32_sub_u8_sse4(S32 * offsets, SINTa offsets_count, S32 multiplier, const U8 * subtrahend )
{
	SINTa offi = 0;

	RR_ASSERT( rrIsAlignedPointer(offsets,16) );
	
	__m128i v_offset_alt_modulo = _mm_set1_epi32(multiplier);
	
	for(; offi+16 <= offsets_count ;offi += 16)
	{
		__m128i offsets1 = _mm_load_si128((__m128i *)(offsets+offi));
		__m128i offsets2 = _mm_load_si128((__m128i *)(offsets+offi+4));
		__m128i offsets3 = _mm_load_si128((__m128i *)(offsets+offi+8));
		__m128i offsets4 = _mm_load_si128((__m128i *)(offsets+offi+12));
		
		// need 4x 32*32 mul (in sse4, not sse2)
		//	I'm only getting 2x mul's in sse2, so not a big win here
		offsets1 = _mm_mullo_epi32(offsets1,v_offset_alt_modulo);
		offsets2 = _mm_mullo_epi32(offsets2,v_offset_alt_modulo);
		offsets3 = _mm_mullo_epi32(offsets3,v_offset_alt_modulo);
		offsets4 = _mm_mullo_epi32(offsets4,v_offset_alt_modulo);
		
		// unpack 8 -> 32 :
		// this is a single PMOVZXBD with a memory reference each
		const U8 * sub = subtrahend + offi;
		__m128i low1 = _mm_cvtepu8_epi32(_mm_cvtsi32_si128(*(const int*) (sub + 0)));
		__m128i low2 = _mm_cvtepu8_epi32(_mm_cvtsi32_si128(*(const int*) (sub + 4)));
		__m128i low3 = _mm_cvtepu8_epi32(_mm_cvtsi32_si128(*(const int*) (sub + 8)));
		__m128i low4 = _mm_cvtepu8_epi32(_mm_cvtsi32_si128(*(const int*) (sub + 12)));
		
		_mm_store_si128((__m128i *)(offsets+offi   ), _mm_sub_epi32(offsets1,low1) );
		_mm_store_si128((__m128i *)(offsets+offi+4 ), _mm_sub_epi32(offsets2,low2) );
		_mm_store_si128((__m128i *)(offsets+offi+8 ), _mm_sub_epi32(offsets3,low3) );
		_mm_store_si128((__m128i *)(offsets+offi+12), _mm_sub_epi32(offsets4,low4) );
	}
	
	for (;offi<offsets_count;offi++)
	{
		offsets[offi] = (offsets[offi] * multiplier) - (S32)subtrahend[offi];
	}
}


U32 simd_find_max_U32_256_sse4(const U32 * histogram)
{
	__m128i vmax = _mm_setzero_si128();
	
	for LOOP(i,(256/4))
	{
		__m128i vals = _mm_loadu_si128((const __m128i *)(histogram + i*4));
		vmax = _mm_max_epu32(vmax,vals);
	}
	
	// horizontal max :
	
	vmax = _mm_max_epu32(vmax, _mm_shuffle_epi32(vmax, _MM_SHUFFLE(1,0,3,2)));
	///vmax = { max[3,1] , max[2,0], max[1,3], max[2,0] };
	vmax = _mm_max_epu32(vmax, _mm_shuffle_epi32(vmax, _MM_SHUFFLE(2,3,0,1)));
	U32 onemax = _mm_extract_epi32(vmax,0);		
	
	//RR_ASSERT( onemax == highest_histo_count );
			
	return onemax;
}

#endif // DO_BUILD_SSE4

OODLE_NS_END
