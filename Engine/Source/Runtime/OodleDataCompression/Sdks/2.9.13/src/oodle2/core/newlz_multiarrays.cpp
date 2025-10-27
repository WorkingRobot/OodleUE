// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "newlz_multiarrays.h"
#include "oodlebase.h"
#include "cbradutil.h"
#include "newlz_histo.h"
#include "rrcompressutil.h"
#include "templates/rrnew.h"
#include "templates/rralgorithm.h"
#include "rrarenaallocator.h"
#include "rrstackarray.h"
#include "log2table.h"
#include "newlz_speedfit.h"
#include "newlz_shared.h"
#include "rrlogutil.h"
#include "newlz_arrays.inl"
#include "templates/rrvector_a.h"
#include "newlz_simd.h"
#include "entropysets.h"

//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"

#define RRVBC_INLINE
#include "rrvarbitcodes.h"
#include "rrvarbitcodes.cpp"

#include "sloppymemset.inl"
#include "speedfitter.h"

OODLE_NS_START

/**

stop calling them "histos" or "arrays" , use "entropysets"

**/

#define NEWLZ_MULTIARRAYS_MAX_FROM_ARRAYS	16
// NEWLZ_MULTIARRAYS_MAX_FROM_ARRAYS is not in the format
// it's just a limit to know the max of "num_arrays" passed in

// the max in the bitstream is 127
// (because I store it in a byte where top bit can't be set)
#define NEWLZ_MULTIARRAYS_FORMAT_MAX_NUM_ARRAYS	63
// @@ I could make this 63 and save myself a flag bit for the future

#define NEWLZ_MULTIARRAYS_ENCODER_MAX_NUM_ARRAYS_STRICT 63
#define NEWLZ_MULTIARRAYS_ENCODER_MAX_NUM_ARRAYS_TARGET (NEWLZ_MULTIARRAYS_ENCODER_MAX_NUM_ARRAYS_STRICT-1)

// @@@@ does 64 ever help much?
//	 if I can limit this to 32 it reduces the size of some things
//	-> U32 switch mask in trellis
//	-> trellis exactly 1 or 2 vectors (not 1-4)

// encoder_max_num_arrays for encoder
//	limit # of arrays for speed & memory use in encoder
static int encoder_max_num_arrays(int compression_level)
{
	// 2^level seems pretty reasonable actually
	// 16 for Normal, 32 for Opt1, 64 for Opt2
	int num_arrays = 1 << compression_level;
		
	// clamping to 32 seems totally fine
	// go ahead and go nuts (63) for optimal4 just for reference
		
	num_arrays = RR_CLAMP(num_arrays,8,32);
	
	// @@ should I do num_arrays--; ?
	//num_arrays--;
	//	because I add the uncompressed array on top of this during the trellis phase
	//	if I want to keep in multiples of 8 for simdness, I want to be 1 under
	// -> no, the -- should be in trellis
	
	// going above 32 doesn't help compression much, but hurts speed a lot
	//	only do it at level 9
	if ( compression_level == 9 ) num_arrays = NEWLZ_MULTIARRAYS_ENCODER_MAX_NUM_ARRAYS_TARGET;
	
	return num_arrays;
}


// see also newlz_array_get_printf
// note : multiarray_printf logs at *encode* time so it is often logging things
//	 that are not actually chosen
// newlz_array_get_printf logs at decode time so it shows you what was done
#define multiarray_printf	rrPrintfKill	
//#define multiarray_printf	rrprintf	

#define MULTIARRAY_INDEXED_EST_INTERVAL_CYCLES	55.f // see speedfit_get_multiarrays
// with current Leviathan lambda (0.002) 100 cycles is worth 0.2 bytes = 1.6 bits

/*=========================================================

DECODERS :

==========================================================*/

/**

newLZ_get_multiarray :

	fills to_ptrs[i] and to_lens[i] for i < num_arrays
	
	the to_ptrs will point into to_mem if the data was compressed
	if some data was uncompressed (and ! force_copy_uncompressed), to_ptrs can point into comp

---------

note:

"multiarray" does not have an overall newlz_array header for the whole set of arrays
	(eg. that would have complen for the whole set)
	
the target "num_arrays" is not transmitted, encoder & decoder must pass in the same thing as argument
  (the number of entropy arrays is transmitted, which may be less than the number of target arrays)
  
the multiarray header is one byte :
	top bit 0x80 = indexed
	7 bits = # of entropy arrays

num_entropy_arrays == num_arrays (non-indexed) is a direct map
num_entropy_arrays = 1 means everything was entropy merged
	-> this is currently allowed in the bitstream but is silly for the encoder to ever make this
	
num_entropy_arrays = 0 is a special value reserved to mean "identity map" 
	for multiarrays ; "identity" just puts each input array as a separate entropy array

-------------

newLZ_get_multiarray :

max scratch usage =

tot to len
+
6 * num_intervals

num_intervals can be as high as (tot_to_len/2)

so the theoretical maximum is actually very high (chunk_len*4)

scratch use of multiarrays IS checked & bounded in the encoder

	// ensure we will fit in decoder scratch :
	if ( tot_num_indices*6 >= NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH )
	
**/
SINTa newLZ_get_multiarray(const U8 * const comp_start, const U8 * const comp_end, 
							U8 * to_mem, U8 * to_mem_end,
							U8 ** const to_ptrs, SINTa * to_lens, SINTa num_arrays,
							SINTa * ptot_to_len,
							bool force_copy_uncompressed,
							U8 * scratch_ptr, U8 * scratch_end)
{
	if ( (comp_end - comp_start) < 4 ) // 1 byte header + 3 = 4 min size
		NEWLZ_ARRAY_RETURN_FAILURE();

	const U8 * comp_ptr = comp_start;
	
	// first byte control :
	int control_byte = *comp_ptr++;
	if ( control_byte < 0x80 ) // top bit should be on
	{
		NEWLZ_ARRAY_RETURN_FAILURE();
	}
	
	int num_entropy_arrays = control_byte & 63;
	bool is_identity = (num_entropy_arrays == 0);
	rrbool unused_flag = control_byte & 0x40; // 0x40 == 64
	unused_flag;
	// unused_flag could be a flag some day, or it could allow num_entropy_arrays up to 127
	
	if ( is_identity )
		num_entropy_arrays = (int)num_arrays;
		
	/*
	// not possible	
	if ( num_entropy_arrays > NEWLZ_MULTIARRAYS_FORMAT_MAX_NUM_ARRAYS )
	{
		NEWLZ_ARRAY_RETURN_FAILURE();
	}
	*/
		
	//multiarray_printf("get_multiarray : %d entropy_arrays\n",num_entropy_arrays);
		
	if ( to_mem == scratch_ptr ) // <- this is the standard usage (from Leviathan direct calling get_multiarrays)
	{
		#if 0
		
		// need to get total output len first so I can divide the scratch
		
		// @@ this mildly sucks but is probably no biggie in the scheme of things
		
		// note that this path is NOT used for single INDEXED_SPLIT arrays
		//	they already know total raw len , have already advanced scratch
		
		const U8 * scan_comp_ptr = comp_ptr;
		
		for LOOP(i,num_entropy_arrays)
		{
			SINTa cur_to_len;
			SINTa cur_comp_len = newLZ_get_arraylens(scan_comp_ptr,comp_end,&cur_to_len,scratch_end - scratch_ptr);
			if ( cur_comp_len < 0 ) NEWLZ_ARRAY_RETURN_FAILURE();
			
			scratch_ptr += cur_to_len;
			scan_comp_ptr += cur_comp_len;
		}

		to_mem_end = scratch_ptr;
		
		// scratch_ptr is now advanced past where the final output will go
		// we can now read entropy arrays into scratch_ptr
		// to_mem continues to point at the beginning of [scratch]
		
		#else
		
		// take the scratch mem
		// divide it into :
		// [final output][entropy array decode space][internal scratch]
		// just take the space available and divide in half to split
		// if this winds up being insufficient, we'll hit a failure later
		// after decoding the
		// [final output]
		// part is kept and scratch is advanced past it
		// the rest is discarded
		
		SINTa scratch_space = scratch_end - scratch_ptr;
		scratch_space -= NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH;
		RR_ASSERT( scratch_space > 0 );
		scratch_space /= 2;
		
		scratch_ptr += scratch_space;
		to_mem_end = scratch_ptr;
		
		#if 0 //def RR_DO_ASSERTS
		
		const U8 * scan_comp_ptr = comp_ptr;
		SINTa tot_to_len = 0;
		
		for LOOP(i,num_entropy_arrays)
		{
			SINTa cur_to_len;
			SINTa cur_comp_len = newLZ_get_arraylens(scan_comp_ptr,comp_end,&cur_to_len,scratch_end - scratch_ptr);
			if ( cur_comp_len < 0 ) NEWLZ_ARRAY_RETURN_FAILURE();
			
			tot_to_len += cur_to_len;
			scan_comp_ptr += cur_comp_len;
		}

		REQUIRE_FUZZ_RETURN( tot_to_len <= scratch_space , -1 );
		
		#endif		
		#endif
	}
		
	U8 * to_ptr;
	U8 * to_ptr_end;
	bool to_ptr_is_scratch;
	
	if ( is_identity )
	{
		to_ptr = to_mem;
		to_ptr_end = to_mem_end;
		to_ptr_is_scratch = false;
	}
	else
	{
		// if indexed, first decode into scratch, then memcpy out the segments to the target buffers
			
		to_ptr = scratch_ptr;
		to_ptr_end = scratch_end;
		to_ptr_is_scratch = true;
	}
	
	SINTa tot_to_len = 0;
	
	// get the entropy arrays :
	U8 * entropy_array_ptrs[NEWLZ_MULTIARRAYS_FORMAT_MAX_NUM_ARRAYS];
	SINTa entropy_array_lens[NEWLZ_MULTIARRAYS_FORMAT_MAX_NUM_ARRAYS];
	
	newlz_array_get_printf("multiarray%s %d->%d : {\n",is_identity ? " identity" : "",num_arrays,num_entropy_arrays);
	
	for LOOP(i,num_entropy_arrays)
	{
		newlz_array_get_printf("  %d : ",i);
	
		// sub array does not need to force copy if we are indexed
		//	because we will memcpy out of it in the intervals mapping phase anywa
		// if we are identity, then you might need to force copy here :
		bool sub_force_copy_uncompressed = is_identity && force_copy_uncompressed;
	
		SINTa cur_to_len;
		U8 * cur_lit_ptr = to_ptr;
		SINTa cur_comp_len = newLZ_get_array(&cur_lit_ptr,comp_ptr,comp_end,&cur_to_len,to_ptr_end - to_ptr,
									sub_force_copy_uncompressed,scratch_ptr,scratch_end);
		if ( cur_comp_len < 0 ) NEWLZ_ARRAY_RETURN_FAILURE();
		
		newlz_array_get_printf("\n");
		
		entropy_array_ptrs[i] = cur_lit_ptr;
		entropy_array_lens[i] = cur_to_len;
		
		// to_ptr scratch advanced even if cur_lit_ptr is uncomp pointing in comp
		to_ptr += cur_to_len;
		if ( to_ptr_is_scratch ) scratch_ptr += cur_to_len;
		tot_to_len += cur_to_len;
		comp_ptr += cur_comp_len;
	}
	RR_ASSERT( to_ptr <= to_ptr_end );
	*ptot_to_len = tot_to_len;
	
	newlz_array_get_printf("} ");
		
	if ( is_identity )
	{
		RR_ASSERT( num_entropy_arrays == num_arrays );
	
		// special case identity map ; entropy arrays = target arrays
		
		for LOOP(i,num_arrays)
		{
			to_ptrs[i] = entropy_array_ptrs[i];
			to_lens[i] = entropy_array_lens[i];
		}
			
		// done!
		
		SINTa comp_len_tot = rrPtrDiff( comp_ptr - comp_start );
		
		return comp_len_tot;
	}

	
	// scratch_ptr was advanced with "to_ptr"
	RR_ASSERT( scratch_ptr == to_ptr );
	
	#ifdef SPEEDFITTING

	const U8 * comp_ptr_save = comp_ptr;
	U8 * scratch_ptr_save = scratch_ptr;

	U8 * entropy_array_ptrs_save[NEWLZ_MULTIARRAYS_FORMAT_MAX_NUM_ARRAYS];
	SINTa entropy_array_lens_save[NEWLZ_MULTIARRAYS_FORMAT_MAX_NUM_ARRAYS];
	
	memcpy(entropy_array_ptrs_save,entropy_array_ptrs,num_entropy_arrays*sizeof(entropy_array_ptrs[0]));
	memcpy(entropy_array_lens_save,entropy_array_lens,num_entropy_arrays*sizeof(entropy_array_lens[0]));
	
	for LOOP(speedfit_iter,c_speedfit_stage2_num_repeats)
	{
	
	comp_ptr = comp_ptr_save;
	scratch_ptr = scratch_ptr_save;
	
	memcpy(entropy_array_ptrs,entropy_array_ptrs_save,num_entropy_arrays*sizeof(entropy_array_ptrs[0]));
	memcpy(entropy_array_lens,entropy_array_lens_save,num_entropy_arrays*sizeof(entropy_array_lens[0]));
	
	U64 t1;
	// t1 time is sampled after get_arrays
	
	#endif
	
	// indexed, copy out chunks

	REQUIRE_FUZZ_RETURN( rrPtrDiff(comp_end - comp_ptr) >= 3 , -1 );
	
	// get varbits complen :
	SINTa varbits_complen = RR_GET16_LE(comp_ptr);
	comp_ptr += 2;
			
	SINTa varbits_complen_flag = varbits_complen & (1U<<15);
	varbits_complen &= 0x3FFF;
				
	SINTa num_indices;
	if ( newLZ_get_arraylens(comp_ptr,comp_end,&num_indices,tot_to_len) <= 0 )
		NEWLZ_ARRAY_RETURN_FAILURE();
		
	SINTa num_intervals = num_indices - num_arrays;
	
	if ( num_intervals < 1 )
		NEWLZ_ARRAY_RETURN_FAILURE();
	
	// interval array scratch usage is 6*num_indices
	//	(two byte arrays + one U32 array)
	// this should be guaranteed by the encoder :
	RR_ASSERT_IF_NOT_CORRUPT( 6*num_indices < NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH );
	
	// get space for interval control data :
	U8 * interval_lenlog2_space = scratch_ptr;
	if ( (scratch_end - scratch_ptr) < num_indices ) NEWLZ_ARRAY_RETURN_FAILURE();
	scratch_ptr += num_indices;
	
	U8 * interval_indices_space = scratch_ptr;
	if ( (scratch_end - scratch_ptr) < num_indices ) NEWLZ_ARRAY_RETURN_FAILURE();
	scratch_ptr += num_indices;
	
	U8 * interval_indices = interval_indices_space;
	U8 * interval_lenlog2 = interval_lenlog2_space;
	
	SINTa num_lens;
	
	if ( ! varbits_complen_flag )
	{
		// indices & len log2 in two separate entropy arrays: 
	
		//get indices array
		
		SINTa to_len;
		SINTa array_len = newLZ_get_array(&interval_indices,comp_ptr,comp_end,&to_len,num_indices,false,scratch_ptr,scratch_end);
		if ( array_len < 0 || to_len != num_indices )
			NEWLZ_ARRAY_RETURN_FAILURE();
			
		comp_ptr += array_len;
		
		//(later : MTF un-transform)
		
		//get len log2 array

		array_len = newLZ_get_array(&interval_lenlog2,comp_ptr,comp_end,&to_len,num_intervals,false,scratch_ptr,scratch_end);
		if ( array_len < 0 || to_len != num_intervals )
			NEWLZ_ARRAY_RETURN_FAILURE();
			
		comp_ptr += array_len;
		
		num_lens = num_intervals;	
		
		#ifdef SPEEDFITTING
		t1 = speedfitter_ticks_start();
		#endif
	
		// no len for EOF indices		
		
		/**
		
		FUZZ SAFETY :
		
		need to validate len log2 <= 16
			only in the non-44 packed case
			can just be a big simd run over the log2's
		
		validate index < num_entropy_arrays
			could just do that in the memcpy loop
			-> currently doing it that way
		
		@@ just like the escape check for newlz_offsets, and the escape check for newlzf packets
			- in some cases (huff decoder) we could know the min & max value of bytes coming out
			- without doing any scan for them
		
		**/
	
		// check if any len > 16 :
		if ( simd_cmpgt_u8(interval_lenlog2,num_lens,16) )
		{
			ooLogError("corruption : interval_lenlog2 > 16\n");
			NEWLZ_ARRAY_RETURN_FAILURE();
		}
		
		#define DO_MTF_TRANSFORM 0
		
		#if DO_MTF_TRANSFORM
		//---------------------------------
		// MTF un-transform the indices :
		
		// interval_indices could be pointing into "comp"
		//	don't modify comp; write to interval_indices_space
		
		/*
		
		 this MTF could be 4 16-byte vectors
			(in common case of num entropy arrays <= 16 or 32, even less)
		 so it should be pretty fast
		
		but the benefit is extremely marginal
		
		with MTF transform :
		total : 31,000,000 ->15,711,955 =  4.055 bpb =  1.973 to 1

		without :
		total : 31,000,000 ->15,712,984 =  4.055 bpb =  1.973 to 1

		
		MTF helps more at -zs1 than at default space-speed
		the more intervals there are, the more it helps
		you get a *lot* of intervals at -zs1 (5000!)
		more like 100 at normal levels

		*/
		
		U8 mtf_table_init[64];
		mtf_table_init[0] = 63; // invalid
		for LOOP(i,64) mtf_table_init[i] = (U8)( i-1 );
		
		U8 mtf_table[64];
		memcpy(mtf_table,mtf_table_init,sizeof(mtf_table_init));
		
		for LOOP(indi,num_indices)
		{
			int mtfi = interval_indices[indi];
			
			if ( mtfi == 0 )
			{
				// EOF
				interval_indices_space[indi] = 0;
				
				// reset mtf table :
				memcpy(mtf_table,mtf_table_init,sizeof(mtf_table_init));
			}
			else
			{
				RR_ASSERT( mtfi < 64 );
				U8 index = mtf_table[mtfi];
				RR_ASSERT( index < 63 );
				interval_indices_space[indi] = index+1; // this +1 could be baked in the mtf_table
			
				memmove(mtf_table+1,mtf_table,mtfi);
				mtf_table[0] = index;
			}
		}
		
		interval_indices = interval_indices_space;
		
		#endif
	}
	else
	{
		// indices & len log2 in packed 44 in one entropy array 
		
		// decode the array into interval_indices space
		// we will then unpack it and overwrite into interval_indices
		U8 * lens_and_indices = interval_indices;
		SINTa to_len;
		SINTa array_len = newLZ_get_array(&lens_and_indices,comp_ptr,comp_end,&to_len,num_indices,false,scratch_ptr,scratch_end);
		if ( array_len < 0 || to_len != num_indices )
			NEWLZ_ARRAY_RETURN_FAILURE();
		
		comp_ptr += array_len;
		
		#ifdef SPEEDFITTING
		t1 = speedfitter_ticks_start();
		#endif
		
		// unpack the nibble-packed lens_and_indices :
		
		int indi = 0;
		
		// do 4 at a time :
		while( indi+4 <= num_indices )
		{
			U32 packed = RR_GET32_NATIVE( lens_and_indices+indi );
			RR_PUT32_NATIVE( interval_lenlog2+indi, (packed>>4) & 0x0F0F0F0FU );
			RR_PUT32_NATIVE( interval_indices+indi, (packed   ) & 0x0F0F0F0FU );
			indi += 4;
		}
		
		while( indi < num_indices )
		{
			// note lens_and_indices may overlap interval_indices
			U8 packed = lens_and_indices[indi];
			interval_lenlog2[indi] = packed >> 4;
			interval_indices[indi] = packed & 0xF;
			indi++;
		}
		
		num_lens = num_indices;		
		
		// there is a (0 bit) len for EOF indices		
	}
	
	// get space for interval_lens array;
	if ( (scratch_end - scratch_ptr) < 4 ) NEWLZ_ARRAY_RETURN_FAILURE();
	scratch_ptr = rrAlignUpPointer(scratch_ptr,4);
	SINTa interval_lens_size = sizeof(U32)*num_lens;
	U32 * interval_lens = (U32 *) scratch_ptr;
	if ( (scratch_end - scratch_ptr) < interval_lens_size ) NEWLZ_ARRAY_RETURN_FAILURE();
	scratch_ptr += interval_lens_size;
	
	// varbits for the len bits under log2 :		
	{
		REQUIRE_FUZZ_RETURN( rrPtrDiff(comp_end - comp_ptr) >= varbits_complen , -1 );
	
		const U8 * varbits_end = comp_ptr + varbits_complen;
						
#undef FRONTBACK_HALF_BYTES_PER_LOOP						
#define FRONTBACK_HALF_BYTES_PER_LOOP	(8)
		FRONTBACK_SAFE_VARS();
		RESET_FRONTBACK_SAFE_VARS(comp_ptr,varbits_end);
		
		rrVarBits_Temps();
		rrVarBits_Locals(lvb1);
		rrVarBits_GetOpen_NoRefill(lvb1,comp_ptr,comp_end);
		
		rrVarBits_Locals(lvb2);
		rrVarBits_GetOpen_NoRefill(lvb2,varbits_end,varbits_end);
				
		// Opens done without refill
		// we'll MAKE_FRONTBACK_SAFE before the first refill
		//  MAKE_FRONTBACK_SAFE does copy-to-scratch, all that
				
		//=============================================
		// get the len under-top bits :
		
		SINTa leni = 0;
		
		#if RR_MINBITSAVAILABLE >= 48
		
		// 16*3 = 48 bits per VB
		
		while( leni+6 <= num_lens )
		{
			MAKE_FRONTBACK_SAFE(lvb1_cur,lvb2_cur);
			
			rrVarBits_Refill_Unsafe(lvb1);
			rrVarBits_RefillBack_Unsafe(lvb2);
		
			RR_BITLENTYPE len1a_log2 = interval_lenlog2[leni+0];
			RR_BITLENTYPE len2a_log2 = interval_lenlog2[leni+1];
			RR_BITLENTYPE len1b_log2 = interval_lenlog2[leni+2];
			RR_BITLENTYPE len2b_log2 = interval_lenlog2[leni+3];
			RR_BITLENTYPE len1c_log2 = interval_lenlog2[leni+4];
			RR_BITLENTYPE len2c_log2 = interval_lenlog2[leni+5];
			
			// fuzz safety range check already done :
			RR_ASSERT( len1a_log2 <= 16 );
			
			U32 len1a; VARBITS_GET_PLUS_TOP_0OK(len1a,lvb1,len1a_log2);
			U32 len2a; VARBITS_GET_PLUS_TOP_0OK(len2a,lvb2,len2a_log2);
			
			U32 len1b; VARBITS_GET_PLUS_TOP_0OK(len1b,lvb1,len1b_log2);
			U32 len2b; VARBITS_GET_PLUS_TOP_0OK(len2b,lvb2,len2b_log2);
			
			U32 len1c; VARBITS_GET_PLUS_TOP_0OK(len1c,lvb1,len1c_log2);
			U32 len2c; VARBITS_GET_PLUS_TOP_0OK(len2c,lvb2,len2c_log2);
								
			interval_lens[leni+0] = len1a;
			interval_lens[leni+1] = len2a;
			interval_lens[leni+2] = len1b;
			interval_lens[leni+3] = len2b;
			interval_lens[leni+4] = len1c;
			interval_lens[leni+5] = len2c;
			
			leni += 6;
		}
		
		#endif
		
		while( leni+2 <= num_lens )
		{
			MAKE_FRONTBACK_SAFE(lvb1_cur,lvb2_cur);
			
			rrVarBits_Refill_Unsafe(lvb1);
			rrVarBits_RefillBack_Unsafe(lvb2);
			
			RR_BITLENTYPE len1_log2 = interval_lenlog2[leni];
			RR_BITLENTYPE len2_log2 = interval_lenlog2[leni+1];
			
			// fuzz safety range check already done :
			RR_ASSERT( len1_log2 <= 16 );
			RR_ASSERT( len2_log2 <= 16 );
			
			U32 len1; VARBITS_GET_PLUS_TOP_0OK(len1,lvb1,len1_log2);
			U32 len2; VARBITS_GET_PLUS_TOP_0OK(len2,lvb2,len2_log2);
					
			interval_lens[leni] = len1;
			interval_lens[leni+1] = len2;
			
			leni += 2;
		}
		if ( leni < num_lens )
		{
			MAKE_FRONTBACK_SAFE(lvb1_cur,lvb2_cur);
			
			rrVarBits_Refill_Unsafe(lvb1);
			
			RR_BITLENTYPE len1_log2 = interval_lenlog2[leni];

			U32 len1; VARBITS_GET_PLUS_TOP_0OK(len1,lvb1,len1_log2);
			
			interval_lens[leni] = len1;
			leni++;
		}
		
		RR_ASSERT( leni == num_lens );
		
		comp_ptr = varbits_end;			
	}
	
	
	//=============================================
	// process the commands; just a bunch of memcpys
	
	// copy into to_mem :	
	to_ptr = to_mem;
	to_ptr_end = to_mem_end;
	
	int leni = 0;
	int indi = 0;
	
	// single-array has lens with the eof, dual array doesn't
	int eof_len_increment = varbits_complen_flag ? 1 : 0;

	// last interval index should always be EOF :
	//	(this check is required to make leni < num_lens pass)
	REQUIRE_FUZZ_RETURN( interval_indices[num_indices-1] == 0 , -1 );
	
	newlz_array_get_printf("{ ");
		
	for LOOP(dest_array_i,num_arrays)
	{
		if ( num_arrays > 1 )
			newlz_array_get_printf("\n  %d : ",dest_array_i);
		
		to_ptrs[dest_array_i] = to_ptr;
		
		// must have an index to get :
		//	(even if this array is empty there will be an EOF index to get)
		REQUIRE_FUZZ_RETURN( indi < num_indices , -1);
			
		for(;;)
		{
			// get source/len command :			
		
			// I have a guaranteed final EOF so I don't need to check this every time :
			RR_ASSERT( indi < num_indices );			
			int source = interval_indices[indi++];
			if ( source == 0 )
			{
				// EOF
				leni += eof_len_increment;
				break;
			}
			else if ( source > num_entropy_arrays ) // source is +1
			{
				// @@ this could be checked with simd_cmpgt_u8 out of loop instead of doing it here
				//	standard array-max-value kind of check
				ooLogError("corruption : interval source > num_entropy_arrays\n");
				NEWLZ_ARRAY_RETURN_FAILURE();
			}
			source--;
							
			REQUIRE_FUZZ_RETURN( leni < num_lens , -1 );
			SINTa cur_len = interval_lens[leni++];
						
			RR_ASSERT( cur_len > 0 );
					
			newlz_array_get_printf("[%d:%d] ",source,cur_len);
			
			if ( cur_len > entropy_array_lens[source] ||
				 cur_len > rrPtrDiff(to_ptr_end - to_ptr) )
			{
				ooLogError("corruption : interval cur_len too large\n");
				NEWLZ_ARRAY_RETURN_FAILURE();
			}
			
			memcpy(to_ptr,entropy_array_ptrs[source],cur_len);
		
			to_ptr += cur_len;
			
			entropy_array_ptrs[source] += cur_len;
			entropy_array_lens[source] -= cur_len;
		}
		
		to_lens[dest_array_i] = rrPtrDiff(to_ptr - to_ptrs[dest_array_i]);		
	}
	
	// verify that we consumed all the interval specification data :	
	REQUIRE_FUZZ_RETURN( indi == num_indices , -1);
	REQUIRE_FUZZ_RETURN( leni == num_lens , -1);
			
	if ( num_arrays > 1 )
		newlz_array_get_printf("\n}");
	else
		newlz_array_get_printf("}");
			
	#ifdef SPEEDFITTING
	
	if ( g_speedfitter_stage != 2 )
		break;
	
	U64 t2 = speedfitter_ticks_end();
	
	speedfitter_stage2_collect_multiarrays(speedfit_iter,(int)num_lens,(int)tot_to_len,t2-t1);
	
	}
	
	#endif
	
	// verify that all entropy_array_lens are now zero?
	// note that *ptot_to_len is filled from the entropy arrays
	//	 so it can be larger than the sum of the arrays you output in fuzz cases
	// go ahead and check
	//	that way there are no surprises in the outer decode_parse about
	//	 the literal sub array sizes not summing to total literal count
	
	for LOOP(i,num_entropy_arrays)
	{
		REQUIRE_FUZZ_RETURN( entropy_array_lens[i] == 0 , -1 );
	}
	
	SINTa comp_len_tot = rrPtrDiff( comp_ptr - comp_start );
	
	//rrprintfvar(comp_len_tot);
	
	return comp_len_tot;
}

//=====================================================

/**

"split" array : a single array, split into pieces

used for NEWLZ_ARRAY_TYPE_SPLIT

**/

SINTa newLZ_get_array_split(const U8 * const comp_start, SINTa comp_len, U8 * const to, SINTa to_len,
								U8 * scratch_ptr, U8 * scratch_end)
{
	if ( comp_len < 6 ) // 1 byte header + 2+3 = 6 min size
		NEWLZ_ARRAY_RETURN_FAILURE();

	const U8 * comp_ptr = comp_start;
	const U8 * comp_end = comp_ptr + comp_len;
	
	// first byte control :
	int num_splits = *comp_ptr;
	
	// top bit is index flag :
	int is_indexed = num_splits & 0x80;
	num_splits &= 0x7F; // 7 bits of splits (could do a +2 here)
	if ( num_splits < 2 )
		NEWLZ_ARRAY_RETURN_FAILURE();
	
	if ( is_indexed )
	{
		// just use the multiarray getter
		
		U8 * to_ptrs[1];
		SINTa to_lens[1];
		SINTa tot_to_len;
		
		// get_multiarray will fill out to_ptrs[0]
		//	using the "to_mem" we provided
		//	we provide that mem as "to"
		//	so it puts that data where we want, in a roundabout way
		
		SINTa used_comp_len = 
			newLZ_get_multiarray(comp_start,comp_end,
			to,to+to_len, // <- this is actually the "to_mem" arg
			to_ptrs,to_lens,1,
			&tot_to_len,
			true,
			scratch_ptr,scratch_end);
		
		if ( used_comp_len < 0 ) NEWLZ_ARRAY_RETURN_FAILURE();
		
		if ( to_lens[0] != to_len ) NEWLZ_ARRAY_RETURN_FAILURE();
		
		// sanity checks, should be guaranteed :
		RR_ASSERT( to_ptrs[0] == to );
		RR_ASSERT( tot_to_len == to_len );
		
		// comp_len == used_comp_len is checked after we return
		return used_comp_len;
	}
	
	comp_ptr++;
	
	// non-indexed :
	
	U8 * to_ptr = to;
	U8 * to_ptr_end = to + to_len;
	
	for LOOP(i,num_splits)
	{
		SINTa cur_to_len;
		// force_copy_uncompressed is true :
		SINTa cur_comp_len = newLZ_get_array(&to_ptr,comp_ptr,comp_end,&cur_to_len,to_ptr_end - to_ptr,true,
			scratch_ptr,scratch_end);
		if ( cur_comp_len < 0 ) NEWLZ_ARRAY_RETURN_FAILURE();
		
		to_ptr += cur_to_len;
		comp_ptr += cur_comp_len;
	}
	if ( to_ptr != to_ptr_end )
		NEWLZ_ARRAY_RETURN_FAILURE();
		
	SINTa got_comp_len = rrPtrDiff( comp_ptr - comp_start );
	
	// comp_len == got_comp_len is checked after we return
	return got_comp_len;
}

/*=========================================================

ENCODERS :

==========================================================*/

//***************************************************************//

// non-indexed splitter :

struct slide_candidate
{
	S32	gain;
	S32 which; // who to grow by sliding boundary
	rrbool right; // or left
	U64 key1,key2; // = slide_array_key , for lazy check
	
	// "which" is growing by sliding into which+1 or which-1 (right or not)
};

// slide_array_key holds the state of an interval
//	used for lazy-dirty
static U64 slide_array_key(S32 start,S32 len)
{
	return ((U64)start<<32) | len;
}

// normal operator less will give a heap that pops largest first :
static bool operator < (const slide_candidate &lhs, const slide_candidate &rhs)
{
	return lhs.gain < rhs.gain;
}

// non-indexed split merger :
//	note NOT N^2 (only merges with neighbors)
//	NOT lazy , just recosts neighbor merges each time
struct merge_candidate
{
	F32	gain_J;
	F32 merged_J;
	S32 src1,src2;
};

// normal operator less will give a heap that pops largest first :
static bool operator < (const merge_candidate &lhs, const merge_candidate &rhs)
{
	return lhs.gain_J < rhs.gain_J;
}

// entropysets_codelen_delta gives better compression & is of course faster
//	-> urg WHY
#define slideN_histo_codelen_delta entropysets_codelen_delta
//#define slideN_histo_codelen_delta histo_codelen_delta_movingfrom2to1_1

// add slide candidates for the ith interval to grow left and/or right
// note this does not add slides that consume the entirety of a neighbor
static rrbool add_slides(int i,
	SINTa num_arrays,
	Histo256 * cur_histos,
	S32 * cur_lens,
	S32 * cur_start, 
	slide_candidate * slide_heap,
	S32 & slide_heap_size,
	const U8 * from,
	int slide_step,
	int force_direction)
{
	//SIMPLEPROFILE_SCOPE(add_slides);
		
	rrbool ret = false;

	// force_direction == 0 for either
	// if force_direction == 1 or -1, then only allow the slide in that direction

	if ( i > 0 && cur_lens[i-1] >= 2*slide_step && force_direction <= 0 )
	{
		// try grow left :
		
		const U8 * ptr = from+cur_start[i] - slide_step;
		
		S32 delta = slideN_histo_codelen_delta
			(ptr,slide_step,cur_histos[i],cur_lens[i],cur_histos[i-1],cur_lens[i-1]);
		if ( delta < 0 )
		{
			slide_candidate & sc = slide_heap[slide_heap_size];
			sc.gain = - delta;
			sc.which = i;
			sc.right = false;
			sc.key1 = slide_array_key(cur_start[i],cur_lens[i]);
			sc.key2 = slide_array_key(cur_start[i-1],cur_lens[i-1]);
			slide_heap_size++;
			ret = true;
		}
	}
	
	if ( i < num_arrays-1 && cur_lens[i+1] >= 2*slide_step && force_direction >= 0 )
	{
		// try grow right :
		
		const U8 * ptr = from+cur_start[i]+cur_lens[i];
		
		S32 delta = slideN_histo_codelen_delta
			(ptr,slide_step,cur_histos[i],cur_lens[i],cur_histos[i+1],cur_lens[i+1]);
		if ( delta < 0 )
		{
			slide_candidate & sc = slide_heap[slide_heap_size];
			sc.gain = - delta;
			sc.which = i;
			sc.right = true;
			sc.key1 = slide_array_key(cur_start[i],cur_lens[i]);
			sc.key2 = slide_array_key(cur_start[i+1],cur_lens[i+1]);
			slide_heap[slide_heap_size] = sc;
			slide_heap_size++;
			ret = true;
		}
	}
	
	return ret;
}

// nice stable, simple cost, single byte slides
// to refine chunky estimated interval boundaries
static void do_greedy_single_byte_slides(
	const U8 * begin,
	const U8 * end,
	Histo256 * cur_histos,
	S32 * cur_lens,
	S32 * cur_start,
	S32 num_arrays)
{
	RR_ASSERT( cur_start[0] == 0 );
	RR_ASSERT( begin + cur_start[num_arrays-1] + cur_lens[num_arrays-1] == end );

	// this is "greedy" in the sense that it scans through and does all profitable slides
	//	it does NOT do best first
	
	// this can slide arrays down to size 0
	// once it makes an array size 0 it can't slide past that anymore
	//	(eg. it won't compact holes)
	//	arrays of size 0 become blockers
	//	after they are removed, more slides might be profitable

	for(int i=1;i<num_arrays;i++)
	{
	
		// consider sliding the boundary between arrays (i-1) and i
		RR_ASSERT( cur_start[i-1] + cur_lens[i-1] == cur_start[i] );
		const U8 * ptr = begin + cur_start[i];
		
		// @@ these absolutely can overflow S32 in extreme cases
		//	 they're a count * count , which can be 17 bits each
		//	in practice that would be extremely rare
		S64 gain_left = 0;
		S64 gain_right = 0;
		
		// see if P[sym] is higher in the other histo than it is in the one it's currently in
		//	P = histo.counts[] / cur_lens[]
		// rather than do a divide, multiply through by lens to get them in the numerator
		// histo has an advantage to try to keep sym, because sym is already in his counts
		//	 this makes the cost stable (sym won't want to move back and forth)
		
		if ( cur_lens[i-1] > 0 )
		{
			//ptr[-1] moving from histo (i-1) to i
			int sym = ptr[-1];
			RR_ASSERT( cur_histos[i-1].counts[sym] >= 1 );
			gain_left = (S64) cur_histos[i].counts[sym] * cur_lens[i-1] - 
						(S64) cur_histos[i-1].counts[sym] * cur_lens[i];
		}		
		
		if ( cur_lens[i] > 0 )
		{
			//ptr[0] moving from histo (i) to (i-1)
			int sym = ptr[0];
			RR_ASSERT( cur_histos[i].counts[sym] >= 1 );
			gain_right = (S64) cur_histos[i-1].counts[sym] * cur_lens[i] - 
						 (S64) cur_histos[i].counts[sym] * cur_lens[i-1];
		}		
	
		// @@ what about gain == 0 ? break ties? favor bigger sets eating smaller ones?
	
		if ( gain_left > 0 && gain_left > gain_right )
		{
			do
			{
				// do a left slide :
				int sym = ptr[-1];

				RR_ASSERT( cur_histos[i-1].counts[sym] >= 1 );
			
				cur_histos[i].counts[sym] ++;
				cur_histos[i-1].counts[sym] --;
				
				cur_lens[i] ++;
				cur_lens[i-1] --;
				
				cur_start[i] --;
				ptr--;
				
				if ( cur_lens[i-1] == 0 )
					break;
				
				// compute next candidate :
				sym = ptr[-1];
				gain_left = (S64) cur_histos[i].counts[sym] * cur_lens[i-1] - 
						(S64) cur_histos[i-1].counts[sym] * cur_lens[i];
			}
			while( gain_left > 0 ); // @@ == 0 ?		
		}
		else if ( gain_right > 0 )
		{
			RR_ASSERT( gain_right >= gain_left );
		
			do
			{
				// do a right slide :
				int sym = ptr[0];

				RR_ASSERT( cur_histos[i].counts[sym] >= 1 );
				
				cur_histos[i-1].counts[sym] ++;
				cur_histos[i].counts[sym] --;
				
				cur_lens[i-1] ++;
				cur_lens[i] --;
				
				cur_start[i] ++;
				ptr++;
				
				if ( cur_lens[i] == 0 )
					break;
				
				// compute next candidate :
				sym = ptr[0];
				gain_right = (S64) cur_histos[i-1].counts[sym] * cur_lens[i] - 
						 (S64) cur_histos[i].counts[sym] * cur_lens[i-1];
			}
			while( gain_right > 0 ); // @@ == 0 ?		
		}
	}	
}

/*******

newLZ_put_array_sub_split_N :
used for NEWLZ_ARRAY_TYPE_SPLIT (not INDEXED)

find N-way split

make initial uniform split (covers all bytes, no gaps)
slide split boundaries with chunky steps
	64 then 16
	heap to take the best step first
	heap is lazy-dirty and cleans itself when it runs out of space
do single byte slides to refine
merge adjacent pairs

(could then slide again, currently not done)

*********/
		
// c_split_N_initial_size is used to decide the # of initial splits
// 512 helps encode speed a lot
// 256 helps compression a little
enum { c_split_N_initial_size = 512 };
//enum { c_split_N_initial_size = 256 };

static SINTa newLZ_put_array_sub_split_N(U8 * const to, U8 * const to_end, const U8 * const from, SINTa from_lenA, const U32 * histogram, 
								rrArenaAllocator * arena, int compression_level, U32 entropy_flags, F32 lambda, const OodleSpeedFit * speedfit, F32 prev_J, F32 * pJ)
{
	S32 from_len = (S32)from_lenA;
	SIMPLEPROFILE_SCOPE_N(put_array_split_N,from_len);
	
	// make initial uniform split of target size c_split_N_initial_size

	// else newLZ_put_array_sub_split_2 is called
	//RR_ASSERT( from_len >= c_split_N_initial_size*3 );

	// Histo is 1024 bytes
	// c_split_N_initial_size is 512 bytes
	//	means a maximum of 2 bytes per input byte

	S32 num_arrays = (from_len + (c_split_N_initial_size/2))/c_split_N_initial_size;

	// even if from_len is small, force trying 3 arrays
	int max_num_arrays = encoder_max_num_arrays(compression_level);
	num_arrays = RR_CLAMP(num_arrays,3,max_num_arrays);

	//const Histo256 * phisto_all = (const Histo256 *)histogram;

	RR_SCOPE_ARENA_ARRAY(cur_histos,Histo256,num_arrays,arena);
		
	S32 cur_lens[NEWLZ_MULTIARRAYS_ENCODER_MAX_NUM_ARRAYS_STRICT];
	S32 cur_start[NEWLZ_MULTIARRAYS_ENCODER_MAX_NUM_ARRAYS_STRICT];
	
	// setup the initial segments :
	S32 segment_len = from_len/num_arrays;
	for LOOPINT(i,num_arrays)
	{
		cur_start[i] = segment_len*i;
		if ( i == num_arrays-1 )
		{
			cur_lens[i] = from_len - cur_start[i];
		}
		else
		{
			cur_lens[i] = segment_len;
		}
		
		CountHistoArrayU8(from + cur_start[i],cur_lens[i],cur_histos[i].counts,256);
		
		RR_ASSERT( histo_check(from + cur_start[i],cur_lens[i],cur_histos[i]) );
	}
	
	RR_ASSERT( histo_check(from + cur_start[1],cur_lens[1],cur_histos[1]) );
		
	// @@ doesn't really need to be arena
	// num_arrays*2 is the max number of slides 
	//	(num_arrays is the max without hysterical degeneracy)
	// pad that up a little higher because we can get full of dirty candidates
	SINTa slide_heap_alloc_count = num_arrays*3;
	RR_SCOPE_ARENA_ARRAY(slide_heap,slide_candidate,slide_heap_alloc_count,arena);
		
	// could iterate the whole process
	//	but the benefit seems very small, so don't
	//for LOOP(slide_repeat_iters,2)
	{
		//SIMPLEPROFILE_SCOPE_N(put_array_split_N_slides,num_arrays);
	
		// first iter is from the initial candidate segments
		// second iter is after merging
	
		RR_ASSERT( cur_start[num_arrays-1] + cur_lens[num_arrays-1] == from_len );
		
		// try sliding the initial segments :
		// each segment can grow left or grow right
		
		// start with any big slides then reduce
		//  not exactly a binary search because each slide step can iterate
		//	trying to avoid local minima by trying the larger steps
		
		// alternative : could push multiple step sizes on the heap at once,
		//	then take the best (take the one that's best total? or best per byte?)
		
		// doing tiny steps seem to hurt here, not help
		//for(int slide_step = 128;slide_step>0;slide_step>>=1)
		// cap at step size >= 16
		// could probably do less step sizes ; maybe just 64 and 16 ?
		//for(int slide_step = 128;slide_step>=16;slide_step>>=1)
		// lots of tweaky here; could be compression_level based?
		for(int slide_step = 64;slide_step>=16;slide_step>>=2) // just 64 then 16
		{
				
		S32 slide_heap_size = 0;
		
		for LOOPINT(i,num_arrays)
		{
			add_slides(i,num_arrays,cur_histos,cur_lens,cur_start,slide_heap,slide_heap_size,from,slide_step,0);
		}
		
		RR_ASSERT( slide_heap_size < slide_heap_alloc_count );
		
		make_heap(slide_heap,slide_heap+slide_heap_size);
		
		int max_num_slides = (int)num_arrays*2; // @@ hacky prevent infinite loop
		
		RR_ASSERT( histo_check(from + cur_start[1],cur_lens[1],cur_histos[1]) );
				
		while(slide_heap_size > 0 )
		{
			slide_candidate sc = slide_heap[0];
			popped_heap(slide_heap,slide_heap+slide_heap_size);
			slide_heap_size--;
			
			// do slide_candidate sc !
			
			RR_ASSERT( sc.gain > 0 ); // we only push productive slides
			int i = sc.which;
			int step_dir = sc.right ? 1 : -1;
			// i is growing
			//  it's taking from [i+step_dir]
			
			RR_ASSERT( i >= 0 && i < num_arrays );
			RR_ASSERT( i+step_dir >= 0 && i+step_dir < num_arrays );
			
			// check lazy update :
			if ( sc.key1 != slide_array_key(cur_start[i],cur_lens[i]) ||
				sc.key2 != slide_array_key(cur_start[i+step_dir],cur_lens[i+step_dir]) )
			{
				// dirty, ignore
				continue;
			}
		
			RR_ASSERT( verify_histo(from + cur_start[i],cur_lens[i],cur_histos[i].counts,256) );
		
			if ( sc.right )
			{
				// grow right :
				
				const U8 * ptr = from+cur_start[i]+cur_lens[i];
				
				histo_slide(ptr,slide_step,cur_histos[i+1],cur_histos[i]);
				cur_lens[i] += slide_step;
				cur_start[i+1] += slide_step;
				cur_lens[i+1] -= slide_step;
				RR_ASSERT( cur_lens[i+1] > 0 );
				
				// if the interval I ate from is now tiny, just absorb the whole thing?
				//	then collapse out the hole?
				//if ( cur_len[i+1] < slide_step )
			}
			else
			{
				// grow left :
				const U8 * ptr = from+cur_start[i] - slide_step;
				
				histo_slide(ptr,slide_step,cur_histos[i-1],cur_histos[i]);
				cur_lens[i] += slide_step;
				cur_start[i] -= slide_step;
				cur_lens[i-1] -= slide_step;
				RR_ASSERT( cur_lens[i-1] > 0 );
			}

			if ( max_num_slides-- == 0 )
				break;
			
			// my neighbors are now dirty
			// I just let them be dirty and stay in the heap
			// I'll kill them when I pop them
			
			if ( slide_heap_size+4 >= slide_heap_alloc_count )
			{
				// needs cleaning , could overflow array
				// -> very rare					
			
				for LOOPBACK(heap_index,slide_heap_size)
				{
					slide_candidate heapsc = slide_heap[heap_index];
					int i1 = heapsc.which;
					int i2 = i1 + ( heapsc.right ? 1 : -1 );
					
					RR_ASSERT( i1 >= 0 && i2 >= 0 );
					RR_ASSERT( i1 < (int)RR_ARRAY_SIZE(cur_start) && i2 < (int)RR_ARRAY_SIZE(cur_start) );
					
					if ( heapsc.key1 != slide_array_key(cur_start[i1],cur_lens[i1]) ||
			 			 heapsc.key2 != slide_array_key(cur_start[i2],cur_lens[i2]) )
					{
						// kill it :
						slide_heap[heap_index] = slide_heap[slide_heap_size-1];
						slide_heap_size--;
					}
				}
				
				// reheap
				make_heap(slide_heap,slide_heap+slide_heap_size);
			}			
			
			// remember slide_heap_size before add_slides :
			S32 slide_heap_i = slide_heap_size;
			
			// try adds :
			add_slides(i,num_arrays,cur_histos,cur_lens,cur_start,slide_heap,slide_heap_size,from,slide_step,0);

			// second slide passes step_dir as force_direction, so it can't undo the op I just did
			//	(the guy that I just grew into will not consider growing back towards me)
			//	(otherwise we can get an infinite ping-pong here)
			add_slides(i+step_dir,num_arrays,cur_histos,cur_lens,cur_start,slide_heap,slide_heap_size,from,slide_step,step_dir);

			RR_ASSERT( slide_heap_size < slide_heap_alloc_count );
			
			// slide_heap_size was incremented by add_slides
			// push those new adds to the heap now
			while( slide_heap_i < slide_heap_size )
			{
				slide_heap_i++;
				push_heap(slide_heap,slide_heap+slide_heap_i);
			}
			
			RR_ASSERT( is_heap(slide_heap,slide_heap+slide_heap_size) );
			
		} // slide_heap_size > 0
		
		RR_ASSERT( cur_start[num_arrays-1] + cur_lens[num_arrays-1] == from_len );
	
		} // slide_step

		// now do single byte slides :				
		do_greedy_single_byte_slides(from,from+from_len,cur_histos,cur_lens,cur_start,num_arrays);
		
		// we can now have empty intervals;
		//	count on the merger to get them
		
		// now merge :
		// only O(N), not a full N^2 merger because we only merge adjacent pairs
		
		merge_candidate merge_list[NEWLZ_MULTIARRAYS_ENCODER_MAX_NUM_ARRAYS_STRICT];
		SINTa merge_list_size = 0;
		
		F32 cur_J[NEWLZ_MULTIARRAYS_ENCODER_MAX_NUM_ARRAYS_STRICT];
		
		F32 merge_best_gain = 0;
		SINTa merge_best_i = -1;
		
		int lasti = -1;
		for LOOPINT(i,num_arrays)
		{
			// skip holes
			if ( cur_lens[i] == 0 ) continue;
			
			// make current J :
			RR_DURING_ASSERT( const U8 * ptr = from + cur_start[i] );
			RR_ASSERT( histo_check(ptr,cur_lens[i],cur_histos[i]) );
			
			cur_J[i] = histo_estimate_J(cur_histos[i],cur_lens[i],lambda,speedfit);
			
			// lasti is the predecessor before i that's not a hole
			if ( lasti > -1 )
			{
				// try merge i,lasti
				
				RR_ASSERT( cur_lens[i] > 0 );
				RR_ASSERT( cur_lens[lasti] > 0 );
				RR_ASSERT( cur_start[lasti] + cur_lens[lasti] == cur_start[i] );
				
				Histo256 merged_histo;
				histo_add(&merged_histo, cur_histos[i], cur_histos[lasti] );
				 
				SINTa merged_len = cur_lens[i]+cur_lens[lasti];
				F32 merged_J = histo_estimate_J(merged_histo,merged_len,lambda,speedfit);
				F32 gain = cur_J[i] + cur_J[lasti] - merged_J;
				if ( gain > 0 )
				{
					merge_list[merge_list_size].gain_J = gain;
					merge_list[merge_list_size].merged_J = merged_J;
					merge_list[merge_list_size].src1 = lasti;
					merge_list[merge_list_size].src2 = i;
					
					if ( gain > merge_best_gain )
					{
						merge_best_gain = gain;
						merge_best_i = merge_list_size;
					}
					
					merge_list_size++;
				}
			}
			
			lasti = i;
		}
				
		// @@ ?? should I force merging of any segment with len < NEWLZ_HUFF_ARRAY_MIN_SIZE
		//	even when the J gain seems bad?
		//	(should be rare)
		// could just add a +1 J bias to the gain for merges that kill tiny segments
		
		while(merge_list_size > 0 )
		{
			RR_ASSERT( merge_list[merge_best_i].gain_J == merge_best_gain );
			
			merge_candidate mc = merge_list[merge_best_i];
			merge_list[merge_best_i] = merge_list[merge_list_size-1];
			merge_list_size--;
			
			merge_best_gain = 0;
			merge_best_i = -1;
		
			int i = mc.src1;
			int j = mc.src2;
			// i precedes j :
			RR_ASSERT( i < j );
			RR_ASSERT( cur_lens[i] > 0 );
			RR_ASSERT( cur_lens[j] > 0 );
			RR_ASSERT( cur_start[i] + cur_lens[i] == cur_start[j] );
			// merge i,j
			
			RR_ASSERT( histo_check(from+cur_start[i],cur_lens[i],cur_histos[i]) );
			RR_ASSERT( histo_check(from+cur_start[j],cur_lens[j],cur_histos[j]) );
			
			{
				// merge will write to slot [i] :
				Histo256 & merged_histo = cur_histos[i];
				
				// @@ a lot of work repeated from when we added :	
				histo_add(&merged_histo, cur_histos[i], cur_histos[j] );
				
				S32 merged_len = cur_lens[i]+cur_lens[j];
				F32 merged_J = mc.merged_J;
				
				RR_DURING_ASSERT( F32 check_merged_J = histo_estimate_J(merged_histo,merged_len,lambda,speedfit) );
				RR_ASSERT( merged_J == check_merged_J );
						
				RR_DURING_ASSERT( F32 gain = cur_J[i] + cur_J[j] - merged_J );
				RR_ASSERT( gain == mc.gain_J );
				
				// replace i with new :
				// (i has the right start)
				// merged_histo already in slot i
				cur_lens[i] = merged_len;
				cur_J[i] = merged_J;
				// wipe out [j] :
				cur_start[j] = 0;
				cur_lens[j] = 0;
				cur_J[j] = 0;
			}
			
			RR_ASSERT( histo_check(from+cur_start[i],cur_lens[i],cur_histos[i]) );
			
			// my neighbor merge candidates are now crap, kill them :
			// also update the best_gain slot
			// this makes us technically O(N^2)
			//	but N is always small (16,32,64 , num entropy arrays)
			//	and this op is cheap, so just do it
			for LOOPBACK(h,(int)merge_list_size)
			{
 				RR_ASSERT( merge_list[h].src1 < merge_list[h].src2 ); 
				RR_ASSERT( merge_list[h].src1 != i );
				RR_ASSERT( merge_list[h].src2 != j );
				if ( merge_list[h].src2 == i ||
					 merge_list[h].src1 == j )
				{
					// I'm dirty, kill me:
					merge_list[h] = merge_list[merge_list_size-1];
					if ( merge_best_i == merge_list_size-1 )
						merge_best_i = h;
					merge_list_size--;
				}
				else
				{
					// should not be straddling :
 					RR_ASSERT( merge_list[h].src2 < i || merge_list[h].src1 > j ); 
 				
					F32 gain = merge_list[h].gain_J;
					if ( gain > merge_best_gain )
					{
						merge_best_gain = gain;
						merge_best_i = h;
					}
				}				
			}
			
			RR_ASSERT( merge_list_size == 0 || merge_list[merge_best_i].gain_J == merge_best_gain );
			
			// add new merges adjacent to i : (skip holes)
			int below = i-1;
			while( below >= 0 && cur_lens[below] == 0 ) below--;
			if ( below >= 0 )
			{
				int k = below;
				
				RR_ASSERT( histo_check(from+cur_start[k],cur_lens[k],cur_histos[k]) );

				Histo256 merged_histo;
				histo_add(&merged_histo, cur_histos[i], cur_histos[k] );
				 
				S32 merged_len = cur_lens[i]+cur_lens[k];
				F32 merged_J = histo_estimate_J(merged_histo,merged_len,lambda,speedfit);
				F32 gain = cur_J[i] + cur_J[k] - merged_J;
				if ( gain > 0 )
				{
					// add to heap :
					RR_ASSERT( merge_list_size < num_arrays );
					merge_list[merge_list_size].gain_J = gain;
					merge_list[merge_list_size].merged_J = merged_J;
					merge_list[merge_list_size].src1 = k;
					merge_list[merge_list_size].src2 = i;
					
					if ( gain > merge_best_gain )
					{
						merge_best_gain = gain;
						merge_best_i = merge_list_size;
					}
					
					merge_list_size++;
				}
			}
			
			int above = j+1;
			while( above < num_arrays && cur_lens[above] == 0 ) above++;
			if ( above < num_arrays )
			{
				int k = above;
				
				RR_ASSERT( histo_check(from+cur_start[k],cur_lens[k],cur_histos[k]) );
			
				Histo256 merged_histo;
				histo_add(&merged_histo, cur_histos[i], cur_histos[k] );
				 
				SINTa merged_len = cur_lens[i]+cur_lens[k];
				F32 merged_J = histo_estimate_J(merged_histo,merged_len,lambda,speedfit);
				F32 gain = cur_J[i] + cur_J[k] - merged_J;
				if ( gain > 0 )
				{
					// add to heap :
					RR_ASSERT( merge_list_size < num_arrays );
					merge_list[merge_list_size].gain_J = gain;
					merge_list[merge_list_size].merged_J = merged_J;
					merge_list[merge_list_size].src1 = i;
					merge_list[merge_list_size].src2 = k;
					
					if ( gain > merge_best_gain )
					{
						merge_best_gain = gain;
						merge_best_i = merge_list_size;
					}
					
					merge_list_size++;
				}
			}
		}
		
		// merges done !

		// removed histos are not condensed; eg there are holes in the array now

		// condense :
		int to_i = 0;
		for LOOP(i,num_arrays)
		{
			if ( cur_lens[i] == 0 ) continue;
			
			if ( i != to_i )
			{
				cur_histos[to_i] = cur_histos[i];
				cur_lens[to_i] = cur_lens[i];
				cur_start[to_i] = cur_start[i];
			}
			
			to_i++;		
		}
		num_arrays = to_i;
		
		if ( num_arrays == 1 ) return -1;
		
	} // iter - do the whole thing again?
	
	//===============================================
	// output it !
	
	// count the number of non-null arrays :
	int num_entropy_arrays = 0;
	for LOOP(i,num_arrays)
	{
		if ( cur_lens[i] == 0 ) continue;
		num_entropy_arrays++;
	}
	
	RR_ASSERT( num_entropy_arrays > 0 );
	if ( num_entropy_arrays == 1 )
	{
		// merged everything! bail out!
		return -1;
	}
		
	//=======================================
	// put to scratch buf so we don't overwrite output unless we beat it :

	RR_SCOPE_ARENA_ARRAY(to_scratch,U8,from_len,arena);
	U8 * to_scratch_end = to_scratch + from_len;
	U8 * to_ptr = to_scratch;

	// put my entropy count header :
	
	*to_ptr++ = (U8)num_entropy_arrays; // N arrays, non-indexed
		
	entropy_flags &= ~ (U32)NEWLZ_ARRAY_FLAG_ALLOW_SPLIT; // not recursive
	
	F32 Jtot = 6; // 6 byte header (5 from newlz header outside, but counted in my J)
	
	RR_DURING_ASSERT(const U8 * from_ptr_check = from);

	for LOOP(i,num_arrays)
	{
		if ( cur_lens[i] == 0 ) continue;
	
		const U8 * from_ptr = from + cur_start[i];
		RR_ASSERT( from_ptr == from_ptr_check );
	
		F32 J1 = LAGRANGE_COST_INVALID;
		SINTa comp_len1 = newLZ_put_array_small_allowed(to_ptr,to_scratch_end,from_ptr,cur_lens[i],
									entropy_flags,lambda,speedfit,&J1,ARRAY_DEADLINE_HUGE,arena,compression_level,NULL);
		if ( comp_len1 < 0 ) return -1;
	
		to_ptr += comp_len1;
		RR_DURING_ASSERT( from_ptr_check += cur_lens[i] );
		
		Jtot += J1;
	}
			
	RR_ASSERT( from_ptr_check == from + from_len );
		
	if ( Jtot >= prev_J )
		return -1;	

	//multiarray_printf("CHOSE : num_entropy_arrays = %d (%.1f - %.1f = %.1f)\n",num_entropy_arrays,Jtot,prev_J,Jtot-prev_J);
	
	// take it!
	
	// make sure it fits :
	SINTa comp_len_tot = rrPtrDiff(to_ptr - to_scratch);
	if ( to+comp_len_tot > to_end )
		return -1;
		
	*pJ = Jtot;
	memcpy(to,to_scratch,comp_len_tot);
	
	return comp_len_tot;
}

/****

newLZ_put_array_sub_split_2 :

try to find a single split point for a 2-way split

===========================

Kraken -z4 pd3d

no splits :

SUM:total : 31,941,800 -> 9,129,592 =  2.287 bpb =  3.499 to 1
SUM:encode           : 1.852 seconds, 100.25 c/b, rate= 17.25 mb/s
SUM:decode           : 68.991 millis, 3.73 c/b, rate= 462.99 mb/s

split2 before :

SUM:total : 31,941,800 -> 9,104,885 =  2.280 bpb =  3.508 to 1
SUM:encode           : 2.112 seconds, 114.35 c/b, rate= 15.12 mb/s
SUM:decode           : 69.960 millis, 3.79 c/b, rate= 456.57 mb/s

64-16 :
SUM:total : 31,941,800 -> 9,105,140 =  2.280 bpb =  3.508 to 1
SUM:encode           : 2.084 seconds, 112.81 c/b, rate= 15.33 mb/s
SUM:decode           : 69.385 millis, 3.76 c/b, rate= 460.36 mb/s

****/

static SINTa newLZ_put_array_sub_split_2(U8 * const to, U8 * const to_end, const U8 * const from, SINTa from_len, const U32 * histogram, 
								rrArenaAllocator * arena, int compression_level, U32 entropy_flags, F32 lambda,const OodleSpeedFit * speedfit, F32 prev_J, F32 * pJ)
{
	//SIMPLEPROFILE_SCOPE(put_array_split_2);
	
	const Histo256 * phisto_all = (const Histo256 *)histogram;
	//RR_ASSERT( verify_histo(from,from_len,histogram,256) );
	
	// histo_estimate_J was perhaps already done on the outside, could save redoing it
	F32 estJ_all = histo_estimate_J(*phisto_all,from_len,lambda,speedfit);
		
	// try a 2-split and optimize the split point
	
	Histo256 cur_histos[2];
	Histo256 & histo1 = cur_histos[0];
	Histo256 & histo2 = cur_histos[1];
		
	F32 estJ_best = estJ_all;
	SINTa len1_best = 0;
	
	// try a few initial split points :
	// scale num_initial_split_points with array size :
	// @@ constants to tweak
	int num_initial_split_points = (int)((from_len + 128) / 256);
	num_initial_split_points = RR_CLAMP(num_initial_split_points,1,8);

	Histo256 histo1_split; int histo1_len = 0;
	Histo256 histo2_split;
		
	for(int i=0;i<num_initial_split_points;i++)
	{
		SINTa len1_split = ((i+1)*from_len) / (num_initial_split_points+1);
		SINTa len2_split = from_len - len1_split;
		
		// just CountHisto the part we're adding to range1 :
		// @@ could be a bit faster counting the smaller of {range1,range2}
		if ( histo1_len == 0 )
			CountHistoArrayU8(from,len1_split,histo1_split.counts,256,false);
		else
			CountHistoArrayU8(from+histo1_len,len1_split-histo1_len,histo1_split.counts,256,true);
		
		RR_ASSERT( verify_histo(from,len1_split,histo1_split.counts,256) );
		
		// don't count the remainder, make it by subtracting :
		 // 2 = all - 1
		//CountHistoArrayU8(from+len1_split,len2_split,histo2.counts,256);
		histo_sub(&histo2_split,*phisto_all,histo1_split);
		
		F32 estJ1 = histo_estimate_J(histo1_split,len1_split,lambda,speedfit);
		F32 estJ2 = histo_estimate_J(histo2_split,len2_split,lambda,speedfit);
		F32 estJ_split = estJ1 + estJ2 + 6; // + 6 byte extra split header
		if ( estJ_split < estJ_best )
		{
			estJ_best = estJ_split;
			len1_best = len1_split;
			histo1 = histo1_split;
			histo2 = histo2_split;
		}
	}
	
	if ( len1_best == 0 ) // estJ_best == estJ_all
	{
		// no split beats unsplit
		return -1;
	}
	
	// @@ could check estJ_best against prevJ here as well for an early-out
	//		-> test the speed/ratio benefit of that
	
	SINTa len1 = len1_best;
	SINTa len2 = from_len - len1;
	
	RR_ASSERT( histo_check(from,len1,histo1) );
	RR_ASSERT( histo_check(from+len1,len2,histo2) );
	
	// slide the split boundary for greedy refinement :
	
	
	// small steps not only don't help, they hurt :
	//  could probably just do step = 64 then step = 16
	//	-> pretty meh for both compression and speed
	//for(int step=128;step>=16;step>>=1)
	for(int step=64;step>=16;step>>=2) // 64 then 16
	{
		
		#define split2_histo_codelen_delta	entropysets_codelen_delta
		
		// histo_codelen_delta_movingfrom2to1_1 should be more accurate but is just worse here
		//#define split2_histo_codelen_delta	histo_codelen_delta_movingfrom2to1_1
		
		
		// slide from 2->1
		while( len2 >= step*2 )
		{
			const U8 * ptr = from+len1;
			
			// consider the block from [ptr2,ptr2+step)
			//	is it cheaper under histo1 or histo2?
			S64 delta = split2_histo_codelen_delta(ptr,step,histo1,len1,histo2,len2);
			if ( delta > 0 )
			{
				break;
			}
			
			// histo1 codes it better, move it :
			
			len1 += step;
			len2 -= step;
			histo_slide(ptr,step,histo2,histo1); // move from histo2 to histo1
		}
				
		// slide from 1->2
		while( len1 >= step*2 )
		{
			const U8 * ptr = from+len1 - step;
			
			S64 delta = split2_histo_codelen_delta(ptr,step,histo2,len2,histo1,len1);
			if ( delta > 0 )
			{
				break;
			}
			
			// histo2 codes it better, move it :
			
			len2 += step;
			len1 -= step;
			histo_slide(ptr,step,histo1,histo2); // move from histo2 to histo1
		}
	}
	
	if ( 1 )
	{
		S32 cur_lens[2] = { (S32)len1, (S32)len2 };
		S32 cur_start[2] = { 0, (S32)len1 };		
		int num_arrays = 2;
		
		do_greedy_single_byte_slides(from,from+from_len,cur_histos,cur_lens,cur_start,num_arrays);
		
		len1 = cur_lens[0];
		len2 = cur_lens[1];
	}
	
	// NEWLZ_HUFF_ARRAY_MIN_SIZE = 32 
	if ( len1 < NEWLZ_HUFF_ARRAY_MIN_SIZE || len2 < NEWLZ_HUFF_ARRAY_MIN_SIZE )
	{
		// slid until one chunk is null or tiny
		return -1;
	}
		
	// NEWLZ_ARRAY_TYPE_SPLIT is put on the outside (5 more bytes of header)
	
	// we don't need to consider merging here, because the alternative of sending the
	//	whole array as one unit is done on the outside
	
	//=======================================
	// put to scratch buf so we don't overwrite output unless we beat it :

	RR_SCOPE_ARENA_ARRAY(to_scratch,U8,from_len,arena);
	U8 * to_scratch_end = to_scratch + from_len;
	U8 * to_ptr = to_scratch;

	// put my entropy count header :	
	*to_ptr++ = 2; // 2 arrays, non-indexed
	
	const U8 * from_ptr = from;
	
	entropy_flags &= ~ (U32)NEWLZ_ARRAY_FLAG_ALLOW_SPLIT; // not recursive
	
	F32 J1 = LAGRANGE_COST_INVALID;
	SINTa comp_len1 = newLZ_put_array_small_allowed(to_ptr,to_scratch_end,from_ptr,len1,entropy_flags,lambda,speedfit,&J1,ARRAY_DEADLINE_HUGE,arena,compression_level,NULL);
	if ( comp_len1 < 0 ) return -1;
	
	to_ptr += comp_len1;
	from_ptr += len1;
	
	F32 J2 = LAGRANGE_COST_INVALID;
	SINTa comp_len2 = newLZ_put_array_small_allowed(to_ptr,to_scratch_end,from_ptr,len2,entropy_flags,lambda,speedfit,&J2,ARRAY_DEADLINE_HUGE,arena,compression_level,NULL);
	if ( comp_len2 < 0 ) return -1;

	to_ptr += comp_len2;
	from_ptr += len2;
	
	RR_ASSERT( J1 >= comp_len1 );
	RR_ASSERT( J2 >= comp_len2 );
	RR_ASSERT( from_ptr == from + from_len );
	
	F32 Jtot = 6 + J1 + J2; // 5 bytes outer header + 1 byte from me
	
	// check that our estimate is on track :
	//	estJ_best should be ballpark of Jtot
	//rrprintfvar(estJ_best);
	//rrprintfvar(Jtot);
	
	if ( Jtot >= prev_J )
		return -1;

	//multiarray_printf("CHOSE 2-split : {%d,%d} : %.1f (was %.1f)\n",len1,len2,Jtot,prev_J);

	// take it!
	
	// make sure it fits :
	SINTa comp_len_tot = rrPtrDiff(to_ptr - to_scratch);
	if ( to+comp_len_tot > to_end )
		return -1;
		
	*pJ = Jtot;
	memcpy(to,to_scratch,comp_len_tot);
	
	return comp_len_tot;
}

//===========================================================

/*********

newLZ_put_array_split2_hinted

try a 2-array split with a given spit point
(used for ML/LRL excesses)

*********/

// newLZ_put_array_split2_hinted is a top level putter
SINTa newLZ_put_array_split2_hinted(U8 * const to, U8 * const to_end, const U8 * const from, SINTa from_len,
								U32 entropy_flags, F32 lambda,const OodleSpeedFit * speedfit, F32 * pJ,
								rrArenaAllocator * arena, int compression_level,
								U32 * opt_histo,
								SINTa hint_split_point)
{
	//SIMPLEPROFILE_SCOPE(put_array_split2_hinted);

	RR_ASSERT( hint_split_point >= 0 && hint_split_point <= from_len );

	// Yes *do* try sub-splits
	//	there can be a high number of LRL excesses in Leviathan
	//	let them use the full indexed path if they want to :
	//entropy_flags &= ~ NEWLZ_ARRAY_FLAG_ALLOW_SPLIT; // don't try sub-splits

	// trivial case :
	if ( hint_split_point == 0 || hint_split_point == from_len || from_len <= NEWLZ_HUFF_ARRAY_MIN_SIZE )
	{
		return newLZ_put_array(to,to_end,from,from_len,entropy_flags,lambda,speedfit,pJ,ARRAY_DEADLINE_HUGE,arena,compression_level,opt_histo);
	}
	
	// the method :
	// just write the split arrays to [to]
	// then try over-writing with non-split
	// the non-split put will only overwrite if it's better (J-wise)
	//
	// @@ this relies on the property that newLZ_put_array doesn't touch [to]
	//	unless it can improve on J
	//	which is something that I need to more clearly enforce
	
	F32 J1 = LAGRANGE_COST_INVALID;
	F32 J2 = LAGRANGE_COST_INVALID;
		
	U8 * to_ptr = to;
	
	// put a 2-split header :
	to_ptr += 5; // 5 bytes for newlz array header
	*to_ptr++ = 2; // 2 arrays, non-indexed
	
	// trying sub-splits on the explicitly hinted split option does not help :
	U32 entrop_flags_sub = entropy_flags & (~NEWLZ_ARRAY_FLAG_ALLOW_SPLIT);
	
	SINTa comp_len1 = newLZ_put_array_small_allowed(to_ptr,to_end,from,hint_split_point,entrop_flags_sub,lambda,speedfit,&J1,ARRAY_DEADLINE_HUGE,arena,compression_level,NULL);
	to_ptr += comp_len1;
	SINTa comp_len2 = newLZ_put_array_small_allowed(to_ptr,to_end,from+hint_split_point,from_len-hint_split_point,entrop_flags_sub,lambda,speedfit,&J2,ARRAY_DEADLINE_HUGE,arena,compression_level,NULL);
	to_ptr += comp_len2;
	
	SINTa comp_len_split = rrPtrDiff(to_ptr - to);
	
	// put header now that I know comp len :
	newLZ_put_array_comp_header(to,NEWLZ_ARRAY_TYPE_SPLIT,from_len,comp_len_split-5);
		
	F32 split_J = J1 + J2 + 6; // 6 bytes of header added
	RR_ASSERT( split_J < *pJ );
	*pJ = split_J;
	
	RR_DURING_ASSERT( U8 check = to[5] );
	
	// then just try putting merged on top of it
	//	because *pJ is filled, put_array won't change [to] if it's not a win :
	//	(is trying sub-splits here)
	SINTa comp_len_merged = newLZ_put_array(to,to_end,from,from_len,entropy_flags,lambda,speedfit,pJ,ARRAY_DEADLINE_HUGE,arena,compression_level,opt_histo);
	if ( comp_len_merged > 0 )
	{
		// put merged :
		RR_ASSERT( *pJ < split_J );
		return comp_len_merged;
	}
	else
	{
		RR_ASSERT( check == to[5] );
	
		// kept split
		
		// opt_histo not filled, just stuff something in there :
		if ( opt_histo )
		{
			CountHistoArrayU8(from,from_len,opt_histo,256);
		}
		
		return comp_len_split;
	}

}

//===================================================================================

/*******

Put N arrays, with no merges

just put the N source arrays as N entropy arrays

***/
// newLZ_put_multiarray_identity unconditionally stomps [to] and [pJ]
static SINTa newLZ_put_multiarray_identity(U8 * const to, U8 * const to_end,
								const U8 * const * from_ptrs, const SINTa * from_lens, SINTa num_arrays,
								U32 entropy_flags,
								F32 lambda,const OodleSpeedFit * speedfit, F32 * pJ,
								rrArenaAllocator * arena, int compression_level)
{
	// put identity map

	U8 * to_ptr = to;
	
	multiarray_printf("put_multiarray : %d identity\n",num_arrays);

	if ( to_ptr >= to_end )
		return -1;
				
	// control byte :
	// 0 means identity :
	// always put top bit on
	*to_ptr++ = 0x80;
	
	F32 tot_J = 1; // starts at 1 for header byte
	
	for LOOP(i,num_arrays)
	{
		F32 cur_J = LAGRANGE_COST_INVALID;
		SINTa cur_comp_len = newLZ_put_array_small_allowed(to_ptr,to_end, from_ptrs[i], from_lens[i], entropy_flags, lambda,speedfit, &cur_J, ARRAY_DEADLINE_HUGE, arena, compression_level, NULL);
		if ( cur_comp_len < 0 )
			return -1;
		to_ptr += cur_comp_len;
		tot_J += cur_J;
	}
	
	*pJ = tot_J;
	
	SINTa tot_comp_len = rrPtrDiff(to_ptr - to);
	return tot_comp_len;
}								

//===================================================================================

// simple greedy slider
// returns slid ptr
//	 ptr stays in [base, end-size]
static const U8 * slide_to_purify(Histo256 &histo, const U8 * ptr, SINTa size,
	const U8 * base, const U8 * end)
{
	// purify the initial histos :
	// slide left or right if I can add a symbol with higher count than I remove :
	
	// add byte at left edge & remove byte at right edge
	// or add at right, remove at left
	// keeps size of interval the same
	// do it if we can add a byte with higher count than the one we remove
	// the one we remove has an inherent advantage because it's in the histo already
	//	so that provides stability
	
	int gain_left = -1;
	if ( (ptr > base) )
		gain_left = histo.counts[ ptr[-1] ] - histo.counts[ ptr[size-1] ];
	
	int gain_right = -1;
	if ( (ptr+size) < end )
		gain_right = histo.counts[ ptr[size] ] - histo.counts[ ptr[0] ];
	
	// sliding when gain == 0 is kind of interesting
	//	it can slide across big runs of identical bytes, for example
	//	when histo is pure and the byte you remove & replace is the same
	
	// when left == right, prefer sliding to the right, to eat forward bytes			
	if ( gain_left > gain_right )
	{
		// @@ maybe while ( ptr > end of previous quantum ) ?
		//	 is there an infinite loop possible if I keep backtracking over and over?
		while( ptr > base )
		{
			RR_ASSERT( histo.counts[ ptr[size-1] ] > 0 );
			
			gain_left = histo.counts[ ptr[-1] ] - histo.counts[ ptr[size-1] ];
			if ( gain_left <= 0 ) // at a gain of 0 do you keep going or stop?
				break; // left == 0 I stop
			
			histo.counts[ ptr[-1] ]++;
			histo.counts[ ptr[size-1] ] --;
			ptr--;		
		}
	}
	else
	{
		while ( (ptr+size) < end )
		{
			RR_ASSERT( histo.counts[ ptr[0] ] > 0 );
			
			gain_right = histo.counts[ ptr[size] ] - histo.counts[ ptr[0] ];
			if ( gain_right < 0 ) // at a gain of 0 do you keep going or stop?
				break; // right == 0 I continue
			
			histo.counts[ ptr[size] ]++;
			histo.counts[ ptr[0] ] --;
			ptr++;		
		}	
	}		
	
	return ptr;
}

//=====================================================================

void newlz_multiarrays_trellis_core_generic(
	U64 * switch_flags,
	U8  * cheapest_entropyset,
	const U8 * ptr, const U8 * base, const U8 *end,
	const U16 * in_entropyset_cost,
	const U16 * histo_codelens_transposed,
	int num_entropysets,int num_entropysets_padded,
	U16 prev_cost_cheapest,U16 switch_histo_cost_codelen)
{
	U16 entropyset_cost[64];

	RR_ASSERT(num_entropysets <= 64);
	for LOOP(i,num_entropysets)
		entropyset_cost[i] = in_entropyset_cost[i];

	while(ptr<end)
	{
		SINTa pos = rrPtrDiff(ptr - base);
		U32 sym = *ptr++;
		const U16 * codelens_for_sym = histo_codelens_transposed + (U32)num_entropysets_padded * sym;

		// the "cost" here can be quite large; it's bits <<13 so can exceed 32 bits
		U16 cur_cost_cheapest = RR_S16_MAX;
		int cur_i_cheapest = -1;
		U64 switch_flag_set = 0;

		// consider coding current byte with all histos :

		// each entropy set considers two arrivals
		// arrive from self, or cheapest other

		for LOOPINT(hi,num_entropysets)
		{
			U16 sym_cost = codelens_for_sym[hi];
			U16 cost_from_self = entropyset_cost[hi];

			// subtract off prev cost :
			// this keeps cost low (fits in U16)
			//	also preps us for the cmp to switch
			RR_ASSERT( cost_from_self >= prev_cost_cheapest );

			//cost_from_self = cost_from_self - prev_cost_cheapest;
			cost_from_self = cost_from_self - prev_cost_cheapest;

			U16 cost = sym_cost + RR_MIN(cost_from_self,switch_histo_cost_codelen);

			U64 do_switch = cost_from_self > switch_histo_cost_codelen;
			switch_flag_set |= do_switch<<hi;
			//cur_quanta[hi].prev = (U8) ( do_switch ? prev_i_cheapest : hi );

			entropyset_cost[hi] = cost;

			if ( cost < cur_cost_cheapest )
			{
				cur_cost_cheapest = cost;
				cur_i_cheapest = hi;
			}
		}

		RR_ASSERT( cur_i_cheapest != -1 );

		switch_flags[pos] = switch_flag_set;
		cheapest_entropyset[pos] = U8_check(cur_i_cheapest);

		prev_cost_cheapest = cur_cost_cheapest;
	}
}

#ifdef __RADSSE2__

void newlz_multiarrays_trellis_core_sse2(
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
		RR_ASSERT( num_entropysets_padded == 8 );

		__m128i v_entropyset_cost = _mm_load_si128((__m128i *)(entropyset_cost));

		// scalar entropyset_cost is no longer used, kept in vec

		while(ptr<end)
		{
			SINTa pos = rrPtrDiff(ptr - base);
			U32 sym = *ptr++;

			const U16 * codelens_for_sym = histo_codelens_transposed + 8 * sym;
			RR_ASSERT( rrIsAlignedPointer(codelens_for_sym,16) );

			__m128i sym_cost = _mm_load_si128((__m128i *)(codelens_for_sym));

			__m128i cost_from_self = _mm_sub_epi16(v_entropyset_cost,v_prev_cost_cheapest);

			__m128i min_prev_cost = _mm_min_epi16(cost_from_self,v_switch_histo_cost_codelen);

			v_entropyset_cost = _mm_add_epi16(sym_cost,min_prev_cost);

			__m128i do_switch_v16 = _mm_cmpgt_epi16(cost_from_self,v_switch_histo_cost_codelen);
			__m128i do_switch_v8 = _mm_packs_epi16(do_switch_v16,_mm_setzero_si128());
			U64 switch_flag_set = _mm_movemask_epi8(do_switch_v8);
			RR_ASSERT( switch_flag_set < 256 );

			int cur_i_cheapest;
			{
			// find min cost :
			// just need horizontal min in v_entropyset_cost

			// find the min and replicate the min in every lane :
			__m128i v_min_cost = v_entropyset_cost;
			v_min_cost = _mm_min_epi16(v_min_cost, _mm_shuffle_epi32(v_min_cost, _MM_SHUFFLE(1,0,3,2)));
			v_min_cost = _mm_min_epi16(v_min_cost, _mm_shuffle_epi32(v_min_cost, _MM_SHUFFLE(2,3,0,1)));
			// just need to swap 16-bit pairs ; no nice way to do this in SSE2 ?
			//	(pshufb obviously in SSSE3)
			v_min_cost = _mm_min_epi16(v_min_cost, _mm_shufflelo_epi16(_mm_shufflehi_epi16(
										v_min_cost, _MM_SHUFFLE(2,3,0,1)),_MM_SHUFFLE(2,3,0,1)));

			RR_DURING_ASSERT( U16 cur_cost_cheapest = (U16) _mm_extract_epi16(v_min_cost,0) );

			__m128i cmp = _mm_cmpeq_epi16(v_entropyset_cost,v_min_cost);
			int mask = _mm_movemask_epi8(cmp);
			RR_ASSERT( mask != 0 );
			int which = rrCtz32(mask);
			cur_i_cheapest = (which>>1);
			RR_ASSERT( cur_i_cheapest < num_entropysets );
			RR_ASSERT( ((S16 *)&v_entropyset_cost)[cur_i_cheapest] == cur_cost_cheapest );

			v_prev_cost_cheapest = v_min_cost;
			}

			switch_flags[pos] = switch_flag_set;
			cheapest_entropyset[pos] = U8_check(cur_i_cheapest);

		}
	}
	else
	{
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
			// will get the vertical min
			__m128i v_min_cost = _mm_set1_epi16(RR_S16_MAX);

			for(int hi = 0; hi < num_entropysets_padded ;hi+=8)
			{
				__m128i sym_cost = _mm_load_si128((__m128i *)(codelens_for_sym+hi));
				__m128i cost_from_self = _mm_load_si128((__m128i *)(entropyset_cost+hi));

				// make cost relative to prev cheapest
				//	- this preps for the cmp and also keeps us bounded for S16 storage
				cost_from_self = _mm_sub_epi16(cost_from_self,v_prev_cost_cheapest);

				// I can arrive from self or a switch :
				__m128i min_prev_cost = _mm_min_epi16(cost_from_self,v_switch_histo_cost_codelen);
				// add on current symbol cost :
				__m128i cost = _mm_add_epi16(sym_cost,min_prev_cost);

				// accumulate vertical min :
				v_min_cost = _mm_min_epi16(v_min_cost,cost);

				_mm_store_si128((__m128i *)(entropyset_cost+hi),cost);

				// if cost from self is > switch_cost , flag a switch in that lane :
				__m128i do_switch_v16 = _mm_cmpgt_epi16(cost_from_self,v_switch_histo_cost_codelen);
				__m128i do_switch_v8 = _mm_packs_epi16(do_switch_v16,_mm_setzero_si128());
				U64 do_switch_mask = _mm_movemask_epi8(do_switch_v8);
				RR_ASSERT( do_switch_mask < 256 );
				switch_flag_set |= do_switch_mask<<hi;
			}

			// get min cost :

			int cur_i_cheapest;

			{
				// v_min_cost has the vertical min cost (across the 8-set lanes)
				// need to find the horizontal min of it now :

				// find the min and replicate the min in every lane :
				v_min_cost = _mm_min_epi16(v_min_cost, _mm_shuffle_epi32(v_min_cost, _MM_SHUFFLE(1,0,3,2)));
				v_min_cost = _mm_min_epi16(v_min_cost, _mm_shuffle_epi32(v_min_cost, _MM_SHUFFLE(2,3,0,1)));
				// just need to swap 16-bit pairs ; no nice way to do this in SSE2 ?
				//	(pshufb obviously in SSSE3)
				v_min_cost = _mm_min_epi16(v_min_cost, _mm_shufflelo_epi16(_mm_shufflehi_epi16(
											v_min_cost, _MM_SHUFFLE(2,3,0,1)),_MM_SHUFFLE(2,3,0,1)));


				RR_DURING_ASSERT( U16 cur_cost_cheapest = (U16) _mm_extract_epi16(v_min_cost,0) );

				/*
				U16 test[8];
				_mm_storeu_si128((__m128i *)test,v_min_cost);
				for LOOP(i,8) RR_ASSERT( test[i] == cur_cost_cheapest );
				*/

				// find cur_i_cheapest :
				// find which of the 8-set lanes matches v_min_cost
				for(int hi = 0; ;hi+=8)
				{
					// loop terminates when found; should not go past end :
					RR_ASSERT( hi < num_entropysets );

					__m128i cost = _mm_loadu_si128((__m128i *)(entropyset_cost+hi));
					__m128i cmp = _mm_cmpeq_epi16(cost,v_min_cost);
					int mask = _mm_movemask_epi8(cmp);
					if ( mask )
					{
						int which = rrCtz32(mask);
						cur_i_cheapest = hi + (which>>1);
						RR_ASSERT( cur_i_cheapest < num_entropysets );
						RR_ASSERT( entropyset_cost[cur_i_cheapest] == cur_cost_cheapest );
						break;
					}
				}
			}

			RR_ASSERT( cur_i_cheapest != -1 );

			switch_flags[pos] = switch_flag_set;
			cheapest_entropyset[pos] = U8_check(cur_i_cheapest);

			v_prev_cost_cheapest = v_min_cost;
		}
	}
}

void newlz_multiarrays_trellis_core_sse4(
	U64 * switch_flags,
	U8  * cheapest_entropyset,
	const U8 * ptr, const U8 * base, const U8 *end,
	const U16 * entropyset_cost,
	const U16 * histo_codelens_transposed,
	int num_entropysets,int num_entropysets_padded,
	U16 prev_cost_cheapest,U16 switch_histo_cost_codelen);

#endif // __RADSSE2__

#ifdef __RADNEON__

void newlz_multiarrays_trellis_core_neon(
	U64 * switch_flags,
	U8  * cheapest_entropyset,
	const U8 * ptr, const U8 * base, const U8 *end,
	const U16 * entropyset_cost,
	const U16 * histo_codelens_transposed,
	int num_entropysets,int num_entropysets_padded,
	U16 prev_cost_cheapest,U16 switch_histo_cost_codelen)
{
	static const RAD_ALIGN(S16, lane_id[8], 16) = { 0,1,2,3,4,5,6,7 };

	// we will use vector v_prev_cost_cheapest here
	// take scalar prev_cost_cheapest from the pos==0 loop
	// henceforth we never update the scalar costs
	int16x8_t v_prev_cost_cheapest = vdupq_n_s16((short)prev_cost_cheapest);
	int16x8_t v_switch_histo_cost_codelen = vdupq_n_s16((short)switch_histo_cost_codelen);

	RR_ASSERT( rrIsAlignedPointer(entropyset_cost,16) );
	RR_ASSERT( rrIsAlignedPointer(histo_codelens_transposed,16) );

	if ( num_entropysets <= 8 )
	{
		// cost stays in a vector, no loads/stores
		RR_ASSERT( num_entropysets_padded == 8 );

		int16x8_t v_entropyset_cost = vld1q_s16((const S16 *)entropyset_cost);
		int16x8_t v_lane_id = vld1q_s16(lane_id);

		// scalar entropyset_cost is no longer used, kept in vec

		while(ptr<end)
		{
			SINTa pos = rrPtrDiff(ptr - base);
			U32 sym = *ptr++;

			const U16 * codelens_for_sym = histo_codelens_transposed + 8 * sym;
			RR_ASSERT( rrIsAlignedPointer(codelens_for_sym,16) );

			int16x8_t sym_cost = vld1q_s16((const S16 *)codelens_for_sym);
			int16x8_t cost_from_self = vsubq_s16(v_entropyset_cost,v_prev_cost_cheapest);
			int16x8_t min_prev_cost = vminq_s16(cost_from_self,v_switch_histo_cost_codelen);
			v_entropyset_cost = vaddq_s16(sym_cost,min_prev_cost);

			uint16x8_t do_switch_v16 = vcgtq_s16(cost_from_self,v_switch_histo_cost_codelen);
			U64 switch_flag_set = neon_movemask_u16(do_switch_v16);
			RR_ASSERT( switch_flag_set < 256 );

			int cur_i_cheapest;
			{
			// find both min cost and lane index it appears in
			// by reducing a S32 min on (value<<16)+lane_id
			#ifndef __RADLITTLEENDIAN__
			#error expected LE
			#endif

			int16x8x2_t merged = vzipq_s16(v_lane_id, v_entropyset_cost);
			int32x4_t cost0 = vreinterpretq_s32_s16(merged.val[0]);
			int32x4_t cost1 = vreinterpretq_s32_s16(merged.val[1]);

			// reduce
			int16x4_t min_cost_and_lane = vreinterpret_s16_s32(neon_reduce_min_s32(vminq_s32(cost0, cost1)));
			RR_DURING_ASSERT( U16 cur_cost_cheapest = (U16) vget_lane_s16(min_cost_and_lane,1) );

			// broadcast result
			// this grabs the min straight from the top half
			v_prev_cost_cheapest = vdupq_lane_s16(min_cost_and_lane,1);
			cur_i_cheapest = vget_lane_s16(min_cost_and_lane,0);
			RR_ASSERT( cur_i_cheapest < num_entropysets );
			}

			switch_flags[pos] = switch_flag_set;
			cheapest_entropyset[pos] = U8_check(cur_i_cheapest);
		}
	}
	else
	{
		int16x8_t v_base_lane_id = vld1q_s16(lane_id);

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

			// start v_min_cost_and_lane as large
			int32x4_t v_min_cost_and_lane = vdupq_n_s32(RR_S16_MAX << 16);
			int16x8_t v_lane_id = v_base_lane_id;

			for(int hi = 0; hi < num_entropysets_padded; hi+=8)
			{
				int16x8_t sym_cost = vld1q_s16((const S16 *)(codelens_for_sym+hi));
				int16x8_t cost_from_self = vld1q_s16((const S16 *)(entropyset_cost + hi));

				// make cost relative to prev cheapest
				//	- this preps for the cmp and also keeps us bounded for S16 storage
				cost_from_self = vsubq_s16(cost_from_self,v_prev_cost_cheapest);

				// I can arrive from self or a switch :
				int16x8_t min_prev_cost = vminq_s16(cost_from_self,v_switch_histo_cost_codelen);
				// add on current symbol cost :
				int16x8_t cost = vaddq_s16(sym_cost,min_prev_cost);
				vst1q_s16((S16 *)(entropyset_cost + hi),cost);

				// shove in lane IDs and accumulate mins vertically
				// we use (cost<<16) + lane_id
				#ifndef __RADLITTLEENDIAN__
				#error expected LE
				#endif

				int16x8x2_t merged = vzipq_s16(v_lane_id, cost);
				int32x4_t cost32_0 = vreinterpretq_s32_s16(merged.val[0]);
				int32x4_t cost32_1 = vreinterpretq_s32_s16(merged.val[1]);
				v_lane_id = vaddq_s16(v_lane_id, vdupq_n_s16(8));

				v_min_cost_and_lane = vminq_s32(v_min_cost_and_lane, cost32_0);
				v_min_cost_and_lane = vminq_s32(v_min_cost_and_lane, cost32_1);

				// if cost from self is > switch_cost, flag a switch in that lane
				uint16x8_t do_switch_v16 = vcgtq_s16(cost_from_self,v_switch_histo_cost_codelen);
				U64 do_switch_mask = neon_movemask_u16(do_switch_v16);
				RR_ASSERT( do_switch_mask < 256 );
				switch_flag_set |= do_switch_mask<<hi;
			}

			// finalize reduction
			int cur_i_cheapest;

			{
				int16x4_t min_cost_and_lane = vreinterpret_s16_s32(neon_reduce_min_s32(v_min_cost_and_lane));
				RR_DURING_ASSERT( U16 cur_cost_cheapest = (U16) vget_lane_s16(min_cost_and_lane,1) );

				// broadcast result
				// this grabs the min straight from the top half
				v_prev_cost_cheapest = vdupq_lane_s16(min_cost_and_lane,1);
				cur_i_cheapest = vget_lane_s16(min_cost_and_lane,0);
				RR_ASSERT( cur_i_cheapest < num_entropysets );
			}

			switch_flags[pos] = switch_flag_set;
			cheapest_entropyset[pos] = U8_check(cur_i_cheapest);
		}
	}
}

#endif // __RADNEON__

void newlz_multiarrays_trellis_core(
	U64 * switch_flags,
	U8  * cheapest_entropyset,
	const U8 * ptr, const U8 * base, const U8 *end,
	const U16 * entropyset_cost,
	const U16 * histo_codelens_transposed,
	int num_entropysets,int num_entropysets_padded,
	U16 prev_cost_cheapest,U16 switch_histo_cost_codelen)
{
#ifdef __RADSSE2__
	
	// CPU detect SSE4
	
	if ( rrsimd_has_sse4() )
	{
		newlz_multiarrays_trellis_core_sse4(
			switch_flags,
			cheapest_entropyset,
			ptr,base,end,
			entropyset_cost,
			histo_codelens_transposed,
			num_entropysets,num_entropysets_padded,
			prev_cost_cheapest,switch_histo_cost_codelen);

		return;
	}

	#ifndef DO_SSE4_ALWAYS

	newlz_multiarrays_trellis_core_sse2(
		switch_flags,
		cheapest_entropyset,
		ptr,base,end,
		entropyset_cost,
		histo_codelens_transposed,
		num_entropysets,num_entropysets_padded,
		prev_cost_cheapest,switch_histo_cost_codelen);

	#endif 

#elif defined(__RADNEON__)
	
	newlz_multiarrays_trellis_core_neon(
		switch_flags,
		cheapest_entropyset,
		ptr,base,end,
		entropyset_cost,
		histo_codelens_transposed,
		num_entropysets,num_entropysets_padded,
		prev_cost_cheapest,switch_histo_cost_codelen);

#else // neither SSE2 nor NEON

	newlz_multiarrays_trellis_core_generic(
		switch_flags,
		cheapest_entropyset,
		ptr,base,end,
		entropyset_cost,
		histo_codelens_transposed,
		num_entropysets,num_entropysets_padded,
		prev_cost_cheapest,switch_histo_cost_codelen);

#endif
}

//=====================================================================

struct indexed_interval
{
	int start,len; // 17 bit
	int histo; // 7 bit
	//U8 histo;
};

struct index_and_count
{
	int index;
	int count;
	
	bool operator > (const index_and_count & rhs) const
	{
		return count > rhs.count;
	}
};

//=====================================================================

/*************

Indexed splits :

	start with lots of short seed histos
		len 64-256
		target count of 100 initial seeds
		semi-random steps between to avoid regular patterns
	
	initial histos are purified with slide_to_purify
	
	Then a K-means++ style initial merge is done
		seeds will be reduced down to target_num_arrays*2
		first a flat histo is added
		then iteratively the further histo from the current set is added
		until target count is reached
		(distance is cross-codelen (KL divergence))
	
	after reduction to target_num_arrays*2			
		bottom-up merge (N^2) to bring them down to target_num_arrays

	now do k-means style refinement ("lloyd iterations")
	3 times :
		add synthetic uncompressed entropyset
		assign bytes to entropysets via trellis dynamic programming
		rebuild histos based on what each entropyset was assigned to
		bottom-up merge the new entropysets
			(merge collapses out unused sets)

	on last iteration, build intervals
		(N bytes go to entropyset I)

how trellis dynamic programming works :
	look up codelen of current byte in all entropysets
	best entropyset assignment is either
	1. retain previous best entropyset (no switch)
	2. switch to current cheapest
	switch cost includes space-speed interval change cost
	the two cost options are in a dynamic programming trellis
	so the switch point can be found optimally non-locally

the trellis optimization is a perfect solution, ignoring :
	1. parse-statistics feedback; eg. your choices feed back to the entropysets
		it pretends the entropysets are constant
	2. the switch cost is not actually a constant
		(depends on interval len and so on)

**************/

/**

Profile : 

at -z7 : (63 arrays)

the initial array set is 2*63
pmis_initial_merge takes that down to 63
it dominates the time
trellis is damn fast in comparison

pmis_multi_indexed    :11.3007
pmis_seed             : 1.4089
pmis_seed_merge       : 1.3037
pmis_initial_merge    : 5.8418
pmis_refine           : 3.5415
pmis_refine_trellis   : 1.7700
pmis_trellis_back     : 0.4492
pmis_refine_merge     : 1.5987
pmis_output           : 0.3322

**/

// only writes to out_ptr if it can improve *pJ

// put_multiarray_indexed_sub = pmis

SINTa newLZ_put_multiarray_indexed_sub(U8 * const out_ptr, U8 * const out_end,
								const U8 * const * from_ptrs, const SINTa * from_lens, SINTa num_arrays,
								U32 entropy_flags,
								F32 lambda,const OodleSpeedFit * speedfit, F32 * pJ,
								rrArenaAllocator * arena, int compression_level)
{	
	//=========================================================

	RR_ASSERT( num_arrays <= NEWLZ_MULTIARRAYS_MAX_FROM_ARRAYS );

	// size is mostly set by count now
	// but they're kept in range :
	enum { c_initial_histo_size_min = 16 }; // <- not actually used for histo sizing any more
	//enum { c_initial_histo_size_max = 256 };
	//enum { c_initial_histo_size_max = 128 };
	//enum { c_initial_histo_size_max = 64 };
	enum { c_initial_histo_step_min = 64 };
	
	// this needs to be low for N^2 merge :
	enum { c_initial_histo_max_count_target = 100 };
	// with k-means++ approach this can be much higher :
	//	but it doesn't really help compression
	//enum { c_initial_histo_max_count_target = 200 };
	
	// c_initial_histo_max_count_target sets the number of initial seed candidates (before kmeans++)
	// target_num_chosen_histos = target_num_arrays*2 is that kmeans++ reduces to
	// then "initial merge" takes it down to target_num_arrays
	// trellis passes work on target_num_arrays
	//	and also do merges when they are profitable
	
	// it looks like 3 trellis iters is the right number
	//	first is very rough, with initial histo seeds
	//  second is the first pass with real histos
	//  third is one refinement pass
	enum { c_num_trellis_lloyd_iters = 3 };
	// trellis is fast enough that going higher isn't a killer
	//	but this isn't really the spot where spending more time gets us wins :
	//enum { c_num_trellis_lloyd_iters = 5 }; // gain is only -0.002 bpb

	int target_num_arrays = encoder_max_num_arrays(compression_level);
	
	// @@ c_initial_histo_max_count_target should probably scale with target_num_arrays
	//	it doesn't make much sense to crank target_num_arrays up to 64 and leave the #seeds at 100
	//	do target_num_arrays*5 or so?

	//=========================================================
	// get initial histos
	//	semi-random small gathers
	
	// the idea is to not take too big a contiguous region
	//	so that if there are high frequency switches to different statistics
	//	we might find some of those pockets

    SINTa tot_from_len = 0;
    SINTa max_from_len = 0;
    for LOOP(array_i,num_arrays)
    {
		tot_from_len += from_lens[array_i];
		max_from_len = RR_MAX(max_from_len, from_lens[array_i] );
    }
    
    if ( tot_from_len < NEWLZ_HUFF_ARRAY_MIN_SIZE*3 )
    {
		return -1;
    }
    if ( max_from_len < c_initial_histo_size_min*2 )
    {
		return -1;
    }
    
    
	SIMPLEPROFILE_SCOPE_N(pmis_multi_indexed,tot_from_len);
	
	// @@ ?? -> this could be a compression level option?
	//		at levels <= Optimal1 kmeans directly to target_num_arrays
	// seed reduce straight to the target count :
	//	-> this is definitely faster , and you can skip initial_merge
	//int target_num_chosen_histos = target_num_arrays;
	// make double target count then use merger : <- a bit better for compression
	//	-0.012 bpb
	int target_num_chosen_histos = target_num_arrays*2;
	
    int initial_histo_step = (int)(tot_from_len/c_initial_histo_max_count_target);
    //initial_histo_step = RR_MAX(initial_histo_step,c_initial_histo_size_min*2);
    // c_initial_histo_size_min is 16 , so that's a min step of 32
    //	let's force it even a little bigger - min step of 64
    initial_histo_step = RR_MAX(initial_histo_step,c_initial_histo_step_min);

	vector_a<entropyset_codelens_U16_256> histo_codelens;
	rrScopeArenaAlloc histo_codelens_alloc( (target_num_chosen_histos+1)*sizeof(entropyset_codelens_U16_256), arena );
	histo_codelens.provide_arena(histo_codelens_alloc.m_ptr,histo_codelens_alloc.m_size);
    
    vector_a<entropyset>	histos;
    rrScopeArenaAlloc histos_alloc;
    
	/**
	
	DO_KMEANSISH_SEEDS is on
	
	make all seeds
	
	pick initial (2*target) count by picking seeds incrementally
		farthest from current seed set each time

	N^2 merger is slightly better, but a lot slower
	
	kmeansish : 15,690,383
	N^2 merge : 15,686,232

	(kmeansish actually still uses the N^2 merger, it just reduces to 2X the
	target seed count and uses N^2 for the final 2X reduction)

	**/
	
	{
	SIMPLEPROFILE_SCOPE(pmis_seed);
    
    vector_a<entropyset>	chosen_histos;
    rrScopeArenaAlloc chosen_histos_alloc(target_num_chosen_histos*sizeof(entropyset),arena);
	chosen_histos.provide_arena(chosen_histos_alloc.m_ptr,chosen_histos_alloc.m_size);
	
    /*
    Histo256 all_arrays_histo;
    RR_ZERO(all_arrays_histo);
    SINTa all_arrays_histo_total = 0;
    */
    
    int max_histo_count = 0;
    
    // estimating histo count is hard because of pseudo-random step
    // just do a pass to count (OVER estimate! because of slide step)
    // then another pass to fill
    for(int first_pass_just_counts=0;first_pass_just_counts<2;first_pass_just_counts++)
    {
		if ( first_pass_just_counts == 1 )
		{
			histos_alloc.Alloc( max_histo_count*sizeof(entropyset) , arena );
			histos.provide_arena(histos_alloc.m_ptr,histos_alloc.m_size);
		}
    
		// PRNG :
		U32 kiss99_x = 0xB5B62FB9;
		kiss99_x = 69069*kiss99_x+12345;  
	
		for LOOP(array_i,num_arrays)
		{
			if ( from_lens[array_i] < c_initial_histo_size_min )
				continue;
				
			const U8 * base = from_ptrs[array_i];
			const U8 * ptr = base;
			const U8 * end = base + from_lens[array_i];
		    	    
			for(;;)
			{
				// step is rand in [initial_histo_step/2,initial_histo_step*3/2]
				int step = (U32)(((U64)kiss99_x * initial_histo_step) >> 32);
				kiss99_x = 69069*kiss99_x+12345;  			    
				step += (initial_histo_step/2);
			    
				step = RR_MIN(step,rrPtrDiff32(end-ptr));
				if ( step < c_initial_histo_size_min )
					break;
		 
				// size = step would be full coverage
				//	  better to have tighter entropysets to try to find pure regions
				// don't let size get too huge (eg. step can be ~1300)
				// was c_initial_histo_size_max = 256
				#if 0
				//enum { c_initial_histo_size_max = 256 };
				enum { c_initial_histo_size_max = 128 };
				//enum { c_initial_histo_size_max = 64 };
				int size = RR_MIN(step,c_initial_histo_size_max);
				#else
				// change 07-07-2019 : randomize size
				//	to try to avoid degenerate traps where you hit the beat frequency of entropset switches
				int size;
				
				// have to always do this, so that first and second pass use the PRNG the same way
				// random in [64,256] , limited by step
				int size_max = RR_MIN(step,256);
				int size_min = 64;
				
				size = (U32)(((U64)kiss99_x * (size_max - size_min)) >> 32);
				kiss99_x = 69069*kiss99_x+12345;			    
				size += size_min;
					
				if ( step < size_min )
				{
					size = step;
				}
				#endif
					
				if ( first_pass_just_counts == 0 )
				{
					// note : doesn't do slide_to_purify
					// that makes this an OVER estimate
					// that's okay
					max_histo_count++;
				}
				else
				{
					RR_ASSERT( histos.size32() < max_histo_count );
					histos.push_back();
					entropyset & cur = histos.back();
													
					CountHistoArrayU8_Simple(ptr,size,cur.histo.counts,256);
					cur.total = size;
					
					#if 0
					// @@?? try a few jitters to see if they are lower entropy
					//	 try to avoid unlucky initial samples
					//	 I can't see a substantial real world benefit to this
					//if ( step > 2*size )
					if ( step > 3*size )
					{
						// use chosen_histos for scratch
						chosen_histos.resize(1);
						Histo256 & histo_alt = chosen_histos[0].histo;
						//Histo256 histo_alt;
						const U8 * ptr_alt = ptr + (step/2);
						
						CountHistoArrayU8_Simple(ptr_alt,size,histo_alt.counts,256);
						U64 cl_orig = rrCodeLenOfHistogramT(cur.histo.counts,256,size);
						U64 cl_alt  = rrCodeLenOfHistogramT(histo_alt.counts,256,size);
						if ( cl_alt < cl_orig )
						{
							cur.histo = histo_alt;
							ptr = ptr_alt;
						}
					}
					#endif
					
					// purify initial histo :
					const U8 * slide_ptr = slide_to_purify(cur.histo,ptr,size,base,end);
					
					// don't take slide_ptr if it went left (can cause an infinite loop)
					ptr = RR_MAX(ptr,slide_ptr);
				}
				
				ptr += step;
			}
		}
    }
    
    
	{
	SIMPLEPROFILE_SCOPE(pmis_seed_merge);
	
	/**
	
	seeds like k-means++
		start with whole array histo as first seed
		form all seed candidates
		pick the next one with highest distance
		add it to seed set
		update distances to all seeds
		distance[i] = MIN( distance[i] , distance to newly added seed )
		stop when you hit K seeds, or highest distance is < threshold
		continue to all remaining seeds but just merge them onto who they're closest to
		-> this should allow more seeds & be faster than the bottom-up merge
		-> O(K*S) instead of O(S^2)

	**/
		    
	// @@ when target_num_chosen_histos is >= histos.size this whole loop does nothing
	//	this does happens all the time in the high optimal levels
	//	target_num_chosen_histos is 124 in Optimal -z9
	
    {
	    // make initial seed :
		chosen_histos.resize(1);
		entropyset & first_seed = chosen_histos.back();
        
		// another alternative for the initial seed : push a flat histo ?
	    // 15,692,300
	    // 31,000,000 ->15,691,573
	    // -> seems to work well, and I like it conceptually
	    //  after choosing a flat histo, the subsequent seeds chosen will be ones
	    //	 that are as far as possible from flat, which is what I want
	    
	    U32 count = 10;
		SloppyMemset_U32( first_seed.histo.counts , count, sizeof(U32)*256 );
		first_seed.total = 256*count;
	    
	    // @@ could skip the entropysets_histo_to_codelens , they should just all be 8 bits
		
		histo_codelens.resize(1);
		entropysets_histo_to_codelens(first_seed.histo,first_seed.total,&histo_codelens.back());	
	}
	
	S32 initial_histo_count = histos.size32();
	
	rrScopeArenaAlloc initial_histo_count_alloc(initial_histo_count*3*sizeof(S32),arena);
	S32 * initial_histo_count_alloc_ptr = (S32 *) initial_histo_count_alloc.m_ptr;
	
	// make distances to chosen seeds :
	S32 * seed_distances = initial_histo_count_alloc_ptr;
	initial_histo_count_alloc_ptr += initial_histo_count;
	
	S32 * seed_distances_closest_i = initial_histo_count_alloc_ptr;
	initial_histo_count_alloc_ptr += initial_histo_count;
	
	S32 * self_codelen = initial_histo_count_alloc_ptr;
	initial_histo_count_alloc_ptr += initial_histo_count;
	
	for LOOPVEC(i,histos)
	{
		seed_distances[i] = RR_S32_MAX;
		seed_distances_closest_i[i] = 0;
		self_codelen[i] = entropysets_self_codelen( histos[i].histo , histos[i].total );
	}
	
	// @@ ?? tweak this?
	// require new seeds are at least this far away from the current seed set
	//	if closer, finish making new seeds
	//	this basically anticipates the post-seed-making merge
	//int min_new_seed_distance = 100 * ENTROPYSET_CODELEN_ONE_BIT; // even 100 bits is not bad
	int min_new_seed_distance = 32 * ENTROPYSET_CODELEN_ONE_BIT; // nearly no difference even at -zs1
	//int min_new_seed_distance = 1 * ENTROPYSET_CODELEN_ONE_BIT;
	// of course the ideal number here goes with SSTB
	//	at default setting 100 bits is okay
	//	at lower SSTB you want a lower # of bits

	// histos[] are possible seeds that are unselected so far
	while( ! histos.empty() )
	{
		RR_ASSERT( histo_codelens.size() == chosen_histos.size() );
		if ( chosen_histos.size32() >= target_num_chosen_histos )
			break;
	
		// chosen_histos.back() was just added
		// update seed distances to it :
				
		S32 max_seed_distance = 0;
		int max_seed_distance_i = 0;
		
		int cur_chosen_i = chosen_histos.size32() - 1;
		const entropyset_codelens_U16_256 & last_codelens_added = histo_codelens[cur_chosen_i];
		for LOOPVEC(i,histos)
		{
			S32 relative_codelen = entropysets_cross_codelen(histos[i].histo,histos[i].total,last_codelens_added);	
			S32 distance = relative_codelen - self_codelen[i];
			// distance >= 0 should be true for entropy
			// not quite true here because codelen is not quite log2(P)
			//RR_ASSERT( distance >= 0 );
			//RR_ASSERT( distance >= -256 );
			// @@ distance divide by histos[i].total to make per symbol ?
			//	if you don't, it's total distance, scaled up by count (count is randomized)
			//distance /= histos[i].total;
			// -> seems slightly better without the div, meh, arbitrary, randomizing
			if ( distance < seed_distances[i] )
			{
				seed_distances[i] = distance;
				seed_distances_closest_i[i] = cur_chosen_i;
			}
			if ( seed_distances[i] > max_seed_distance )
			{
				max_seed_distance = seed_distances[i];
				max_seed_distance_i = check_value_cast<int>(i);
			}
		}
		
		// distance is in (codelen) bits (ENTROPYSET_CODELEN_ONE_BIT)
		if ( max_seed_distance <= min_new_seed_distance )
			break;
		
		// add new seed to chosen set :
		
		chosen_histos.push_back();
		chosen_histos.back() = histos[max_seed_distance_i];
		
		histo_codelens.push_back();
		entropysets_histo_to_codelens(chosen_histos.back().histo,chosen_histos.back().total,&histo_codelens.back());	
		
		// remove from histos[] :
		// (or just flag it as deleted and leave a hole?)
		SINTa histo_back_i = histos.size() - 1;
		histos[max_seed_distance_i] = histos[histo_back_i]; histos.pop_back();
		self_codelen[max_seed_distance_i] = self_codelen[histo_back_i];
		seed_distances[max_seed_distance_i] = seed_distances[histo_back_i];
		seed_distances_closest_i[max_seed_distance_i] = seed_distances_closest_i[histo_back_i];
	}
	
	// all chosen histos selected
	// merge remainder onto their closest :
	for LOOPVEC(i,histos)
	{
		int chosen_i = seed_distances_closest_i[i];
		histo_add(&(chosen_histos[chosen_i].histo),chosen_histos[chosen_i].histo,histos[i].histo);
		chosen_histos[chosen_i].total += histos[i].total;
	}
	
	// move "chosen_histos" -> "histos" which will be used below
	chosen_histos.swap(histos);
	chosen_histos_alloc.swap(histos_alloc);
	
	// chosen_histos not used any more
	chosen_histos.release();
	initial_histo_count_alloc.Release();
	chosen_histos_alloc.Release();
	
	// histo_codelens are not used past this point :
	//	(will be remade for each trellis iter)
	histo_codelens.clear();
	
	} // end k-meansish phase profile scope
    
    } // profile scope pmis_seed
    
	//=========================================================
	// bottom up merge the initial histos :
	
	int histos_initial_count = histos.size32();
	
	multiarray_printf("histos_initial_count = %d\n",histos_initial_count);
	
	if ( histos_initial_count == 0 )
	{
		return -1;
	}	
	
	// @@ this branch is optional, could merge one time even if count is low enough
	if ( histos_initial_count > target_num_arrays )
	{
		SIMPLEPROFILE_SCOPE(pmis_initial_merge);

		// @@ using entropysets_order0_codelen_bits here -> is that good?
		//	-> try newlz_array_estimate_complen_bits instead (with merge_J_gain = 0)
			
		// note : using histo_estimate_complen_bits in the initial phase is a bit wrong
		//	it has the 5*8 header bytes for example
		//	but my histos at this point do NOT correspond to real entropy arrays
		//	they are a sparse sampling (the total count of symbols is less), so the scale is off
	    
		// J is in bytes
		// initial merge uses entropyish entropysets_order0_codelen_bits (doesn't include any header byte size)
		// pretend we gain a tiny bit from merging
		// initial_merge_J_gain higher = more merging
		// -> this is pretty irrelevant to encode time now that trellis is fast
		F32 initial_merge_J_gain = 2.0f; // 2-4 seems reasonable
		
		// keep doing merges past target count if the J gain is >= initial_merge_min_gain_J
		F32 initial_merge_min_gain_J = 0.f;
		
		merge_entropysets(histos,initial_merge_J_gain,initial_merge_min_gain_J, target_num_arrays, entropysets_order0_codelen_bits_cpudetect(), arena);
	}
		
	int num_entropysets = histos.size32();
	
	multiarray_printf("initial merge num_entropysets : %d\n",num_entropysets);
	
	if ( num_entropysets == 1 )
	{
		// degenerate case, did all merges
		return -1;
	}
		
	//-------------------------------------------
	
	// @@
	// "histos" and "histo_codelens" are now smaller
		
	//-------------------------------------------
	
	// trellis walk chunks to choose coding set
	
	// array_intervals is the output of the trellis walk
	// @@@@ this is hard to bound; in theory there could be an interval per byte
	//		(actually the cost clamp I do forces min interval to 2 bytes, but still)
	vector_a<indexed_interval> array_intervals[NEWLZ_MULTIARRAYS_MAX_FROM_ARRAYS];
	
	rrScopeArenaAlloc array_intervals_space_alloc;
	SINTa array_intervals_space_needed = sizeof(indexed_interval) * (tot_from_len + num_arrays) / 2;
	array_intervals_space_alloc.Alloc(array_intervals_space_needed,arena);
	indexed_interval * array_intervals_space_ptr = (indexed_interval *)array_intervals_space_alloc.m_ptr;
	
	for LOOP(array_i,num_arrays)
	{
		SINTa array_len = from_lens[array_i];
		if ( array_len == 0 ) continue;
		
		SINTa max_intervals = (array_len + 1)/2;
		array_intervals[array_i].provide_arena(array_intervals_space_ptr,max_intervals*sizeof(indexed_interval));
		array_intervals_space_ptr += max_intervals;
	}
			
	{ // scope trellis
	
	// each quantum consider arriving from same histo
	//	(continuing a block)
	// or from previous cheapest histo (changing block)
	// just 2 choices
	//	(no reason to consider others, because if you take a change penalty it should be from cheapest)
	
	// instead of doing quanta here, this could just be byte by byte!
	//	why not !?
	//	gives you exact byte boundaries for switching, right?
	//  -> YES
	
	// quanta is a 2d array
	//	[num_codelens * num_quanta]
	
	// paremters :
	// J is in bytes
	// cost to switch codelen sets is :
	//	1 byte to signal the switch (expgol index and len)
	//	+ 100 cycles time for memcpy overhead
	// the actual cost I measure seems to around 1.6 bytes
	// it seems to help, however, to pretend it's a bit higher here -
	int switch_histo_cost_flag_bits = 15;
	U32 switch_histo_cost_flag_codelen = (U32)(switch_histo_cost_flag_bits * ENTROPYSET_CODELEN_ONE_BIT);
	// switch_histo_cost_J is in bytes, change to "codelen" bits :
	U32 switch_histo_cost_time_codelen = (U32)(lambda * MULTIARRAY_INDEXED_EST_INTERVAL_CYCLES * 8 * ENTROPYSET_CODELEN_ONE_BIT);
	
	// trellis SIMD is done in S16
	//	must be able to add switch histo cost plus a sym code len plus a delta codelen :
	U32 switch_histo_cost_max = RR_S16_MAX - ENTROPYSET_SYM_INVALID_HIGH_CL*2;

	// switch_histo_cost_max = 25599
	// Kraken at maximum lambda (zs4096) is close (switch_histo_cost_codelen = 21862)
	//	but not quite hitting threshold
	// Leviathan default is switch_histo_cost_codelen = 4121

	U16 switch_histo_cost_codelen = S16_check( RR_MIN(switch_histo_cost_flag_codelen + switch_histo_cost_time_codelen,switch_histo_cost_max) ); 
	
	// switch histo cost > ENTROPYSET_SYM_INVALID_HIGH_CL (14 bits) prevents len 1 intervals
	//	  (because it makes it better to send that symbol from a zero-count histo rather than switch)
	RR_ASSERT( switch_histo_cost_codelen > ENTROPYSET_SYM_INVALID_HIGH_CL );

	// below we read "histo_codelens" and write "histos"
	
	// switch flags fit in 64 bits :
	RR_COMPILER_ASSERT( NEWLZ_MULTIARRAYS_ENCODER_MAX_NUM_ARRAYS_TARGET <= 63 );
	vector_a<U64> switch_flags_vec;
	vector_a<U8> cheapest_entropyset_vec;
	
	rrScopeArenaAlloc switch_flags_vec_alloc(9*max_from_len,arena);
	
	// reserve to longest array len :
	switch_flags_vec.provide_arena(switch_flags_vec_alloc.m_ptr,max_from_len*sizeof(U64));
	cheapest_entropyset_vec.provide_arena((U8 *)switch_flags_vec_alloc.m_ptr+max_from_len*sizeof(U64),max_from_len);
	
	vector_a<U16> histo_codelens_transposed_vec;
	rrScopeArenaAlloc histo_codelens_transposed_vec_alloc;
	
	{
		// num_entropysets is <= target_num_arrays currently
		// we add the flat set
		// make sure we have room for that
		RR_ASSERT( num_entropysets <= target_num_arrays );
		int num_entropysets_max = target_num_arrays+1;
		int num_entropysets_padded = (num_entropysets_max + 7) & ~7;

		SINTa histo_codelens_transposed_vec_alloc_size = sizeof(U16)*num_entropysets_padded*256;
		histo_codelens_transposed_vec_alloc.Alloc(histo_codelens_transposed_vec_alloc_size,arena);
		histo_codelens_transposed_vec.provide_arena(histo_codelens_transposed_vec_alloc.m_ptr,histo_codelens_transposed_vec_alloc.m_size);
	}
	
	#define DO_UNCOMPRESED_ENTROPY_SET	1
	// -> DO_UNCOMPRESED_ENTROPY_SET does help a very tiny bit (15,705,975 vs 15,718,841)
	
	for(int trellis_iter=0;;trellis_iter++)
	{
		SIMPLEPROFILE_SCOPE(pmis_refine);
	
		bool last_trellis_iter = ( trellis_iter == (c_num_trellis_lloyd_iters-1) );
								
		// set histo_codelens from histos
		// this histo_codelens will be used for the trellis quantum selection
		//	"histo_codelens" here is just temp scratch
		//	  we fill it, then convert to transposed, then free it
		histo_codelens.resize(histos.size());
			
		#if DO_UNCOMPRESED_ENTROPY_SET
		
		// add a synthetic entropyset for uncompressed data (flat histo)
		
		// first remove any existing entropysets which are nearly flat :
		
		// slightly less than 8 bpb :
		// @@@@ is 7.95 too low? maybe 7.99 ?
		U16 codelen_near_uncompressed = (U16) (7.95 * ENTROPYSET_CODELEN_ONE_BIT);
		//U16 codelen_near_uncompressed = (U16) (8 * ENTROPYSET_CODELEN_ONE_BIT - 1);
		
		for LOOPVECBACK(i,histos)
		{
			entropysets_histo_to_codelens(histos[i].histo,histos[i].total,&histo_codelens[i]);
			
			// remove any entropy set that's very close to uncompressed
			// this is because I am explicitly adding an uncompressed set below
			// and I don't want to double them up
			
			// compute entropy :
			U32 codelen_total = entropysets_cross_codelen(histos[i].histo,histos[i].total,histo_codelens[i]);
			
			//double H = (double) codelen_total / (histos[i].total << ENTROPYSET_CODELEN_ONE_BIT_SHIFT);
			//rrprintf("entropy set %d , H = %.3f\n",i,H);
							
			if ( codelen_total >= (histos[i].total * codelen_near_uncompressed) )
			{
				// near uncompressed
				// kill me
				// histos[] will be wiped anyway, just fix codelens :
				histo_codelens[i] = histo_codelens.back();
				histo_codelens.pop_back();
			}
		}
	
		// pretty sure this should always be true :
		RR_ASSERT( histo_codelens.size() < NEWLZ_MULTIARRAYS_ENCODER_MAX_NUM_ARRAYS_STRICT );
		if ( histo_codelens.size() < NEWLZ_MULTIARRAYS_ENCODER_MAX_NUM_ARRAYS_STRICT )
		{
			// add a set for uncompressed :
			// this is to model the option of sending an entropy set as an uncompressed array
			histo_codelens.push_back();
			U16 * uncompressed_set_codelens = histo_codelens.back().codelen;
			// U16 memset
			
			// @@?? super meh
			// pretend I'm slightly cheaper than one byte
			//	to favor this set for space-speed
			SloppyMemset_U16(uncompressed_set_codelens,codelen_near_uncompressed,256*sizeof(U16));
			//SloppyMemset_U16(uncompressed_set_codelens,ENTROPYSET_CODELEN_ONE_BIT*8,256*sizeof(U16));
		}
		
		num_entropysets = histo_codelens.size32();
		
		#else
		
		for LOOPVEC(i,histos)
		{
			entropysets_histo_to_codelens(histos[i].histo,histos[i].total,histo_codelens[i]);
		}
		
		RR_ASSERT( num_entropysets == histo_codelens.size32() );
		
		#endif // DO_UNCOMPRESED_ENTROPY_SET

		// fill histo_codelens_transposed :

		#if defined(__RADSSE2__) || defined(__RADNEON__)
		// for SSE2 :
		//	pad num_entropysets up to next 8
		int num_entropysets_padded = (num_entropysets + 7) & ~7;
		#else
		int num_entropysets_padded = num_entropysets;
		#endif

		histo_codelens_transposed_vec.resize(num_entropysets_padded*256);
		U16 * histo_codelens_transposed = histo_codelens_transposed_vec.data();
		RR_ASSERT( rrIsAlignedPointer(histo_codelens_transposed,16) );
		
		// @@ big transpose ; does this take noticeable time?
		// @@ TODO : fill histo_codelens_transposed directly above in entropysets_histo_to_codelens?
		//    OR do the transpose with a big SIMD thing (yuck)
		for LOOP(sym,256)
		{
			U16 * codelens_for_sym = histo_codelens_transposed + num_entropysets_padded * sym;
			for LOOP(hi,num_entropysets)
			{
				codelens_for_sym[hi] = histo_codelens[hi].codelen[sym];
			}
			for(int hi=num_entropysets;hi<num_entropysets_padded;hi++)
			{
				codelens_for_sym[hi] = ENTROPYSET_SYM_INVALID_HIGH_CL;
			}
		}
		
		histo_codelens.clear(); // non-transposed histo_codelens no longer used

		if ( last_trellis_iter )
		{
			// @@@@ !! could now free histos & alloc intervals
			//	reduces peak mem use
			
			// can't do it easily with the arena allocation pattern
			// -> could put intervals on top of histos in many cases ?
		}

		// NEWLZ_MULTIARRAYS_ENCODER_MAX_NUM_ARRAYS == 63
		//	 +1 uncompressed added above
		//	must fit flags in U64 :
		RR_ASSERT( num_entropysets <= 64 );
		RR_ASSERT( num_entropysets_padded <= 64 );
					
		// wipe the histos now, they will be filled by the trellis pass usage
		histos.resize(num_entropysets);
		histos.memset_zero(); // @@ could skip this on last iter
		
		RAD_ALIGN(U16,entropyset_cost[64],16);
		
		int tot_num_intervals = 0;
		
		for LOOP(array_i,num_arrays)
		{
			const U8 * base = from_ptrs[array_i];
			SINTa array_len = from_lens[array_i];
		 
			SIMPLEPROFILE_SCOPE_N(pmis_refine_trellis,array_len);
			   
			if ( array_len == 0 )
				continue;
		    
			const U8 * ptr = base;
			const U8 * end = ptr + array_len;
			
			switch_flags_vec.resize(array_len);
			cheapest_entropyset_vec.resize(array_len);
			U64 * switch_flags = switch_flags_vec.data();
			U8 * cheapest_entropyset = cheapest_entropyset_vec.data();
		    
			U16 prev_cost_cheapest = RR_S16_MAX;

			// pos == 0 first step		
			// no previous to come from
			{			
				int sym = *ptr++;
				U16 * codelens_for_sym = histo_codelens_transposed + num_entropysets_padded * sym;
						    
				// consider coding current byte with all histos :
				int cur_i_cheapest = -1;
			
				for LOOPINT(hi,num_entropysets)
				{
					U16 sym_cost = codelens_for_sym[hi];
					
					entropyset_cost[hi] = sym_cost;
					
					if ( sym_cost < prev_cost_cheapest )
					{
						prev_cost_cheapest = sym_cost;
						cur_i_cheapest = hi;
					}
				}
				for(int hi=num_entropysets;hi<num_entropysets_padded;hi++)
				{
					entropyset_cost[hi] = ENTROPYSET_SYM_INVALID_HIGH_CL;
				}
				
				RR_ASSERT( cur_i_cheapest != -1 );
			
				switch_flags[0] = 0;
				cheapest_entropyset[0] = U8_check(cur_i_cheapest);
			}

			newlz_multiarrays_trellis_core(
				switch_flags,
				cheapest_entropyset,
				ptr,base,end,
				entropyset_cost,
				histo_codelens_transposed,
				num_entropysets,num_entropysets_padded,
				prev_cost_cheapest,switch_histo_cost_codelen);
				
		    {
			SIMPLEPROFILE_SCOPE(pmis_trellis_back);
		
			// check trellis "cost" estimate vs actual output byte count -
			//	 -> yes they match decently, cost estimate is correct
			//multiarray_printf("array %d trellis iter %d trace cost : %d\n", 
			//	array_i,trellis_iter,prev_cost_cheapest/(8*ENTROPYSET_CODELEN_ONE_BIT) );
			//trellis_cost_total_bytes += prev_cost_cheapest/(8*ENTROPYSET_CODELEN_ONE_BIT);
		    
			// trace backwards to find the best walk :
			// prev_i_cheapest is the final arrival quantum
		    
		    tot_num_intervals++;
		    
		    // in last iter, make the intervals
		    // in earlier iters, make histo for next pass
		    if ( last_trellis_iter )
		    {		    						    
				vector_a<indexed_interval> & intervals = array_intervals[array_i];
				//intervals.reserve(64);
			    
				SINTa pos_back = array_len-1;
				int cur_entropyset = cheapest_entropyset[pos_back];
				SINTa cur_interval_end = array_len;
				U64 cur_entropysetmask = RR_ONE_U64<<cur_entropyset;
						
				while(pos_back >= 0 )
				{
					// prev_i_cheapest is the chosen entropy set for this pos
					
					// @@ no histo in last pass? (needed if we do a final merge)
					/*
					int sym = base[pos_back];
					histos[prev_i_cheapest].histo.counts[sym] ++;
					histos[prev_i_cheapest].total ++;
					*/
													
					// trace :
					//prev_i_cheapest = cur_quanta[prev_i_cheapest].prev;
					U64 switch_flag_set = switch_flags[pos_back];
					if ( switch_flag_set & cur_entropysetmask )
					{
						// do switch
						
						intervals.push_back();
						intervals.back().start = (S32)pos_back;
						intervals.back().len   = (S32)(cur_interval_end - pos_back);
						intervals.back().histo = cur_entropyset;
						
						cur_interval_end = pos_back;
						RR_ASSERT( cur_entropyset != cheapest_entropyset[pos_back-1] );
						cur_entropyset = cheapest_entropyset[pos_back-1];
						cur_entropysetmask = RR_ONE_U64<<cur_entropyset;
						
						tot_num_intervals++;
					}
					pos_back--;
				}
				
				RR_ASSERT( cur_interval_end != 0 );
				intervals.push_back();
				intervals.back().start = 0;
				intervals.back().len   = (S32)cur_interval_end;
				intervals.back().histo = cheapest_entropyset[cur_interval_end-1];
			    
				// intervals are backwards, reverse :
				reverse(intervals.begin(),intervals.end());
			}
			else
			{
				// exact same trace-back
				// but before the last, we update histos rather than build intervals
			
				S32 pos_back = (S32)array_len-1;
				int cur_entropyset = cheapest_entropyset[pos_back];
				U64 cur_entropysetmask = RR_ONE_U64<<cur_entropyset;
				
				while(pos_back >= 0 )
				{
					// prev_i_cheapest is the chosen entropy set for this quantum
					
					// add to histo for next pass :
					int sym = base[pos_back];
					histos[cur_entropyset].histo.counts[sym] ++;
					histos[cur_entropyset].total ++;
													
					// trace :	
					U64 switch_flag_set = switch_flags[pos_back];
					if ( switch_flag_set & cur_entropysetmask )
					{
						// do switch
						RR_ASSERT( cur_entropyset != cheapest_entropyset[pos_back-1] );
						cur_entropyset = cheapest_entropyset[pos_back-1];
						cur_entropysetmask = RR_ONE_U64<<cur_entropyset;
						
						tot_num_intervals++;
					}
					pos_back--;
				}
			}
			
			}
		}
		
		//multiarray_printf("trellis_cost_total_bytes = %d\n",trellis_cost_total_bytes);
		
		// now we have intervals on all the arrays with entropy set assignments
		
		// the histos[] have been updated to match the intervals
		
		if ( last_trellis_iter )
		{
			// on last iter, break here
			//	after intervals choose histos
			//	before merging, so they don't change id
			// (if you did another merge pass now, you would have to
			//	 propagate the index changes to the intervals)
			break;
		}
		
		// some histos may now have a total of 0
		//  remove them :

		U32 all_histos_total = remove_unused_entropysets(histos);
		RR_ASSERT( all_histos_total == (U32)tot_from_len );
		all_histos_total;
		num_entropysets = histos.size32();
		
		if ( num_entropysets == 1 ) return -1;		
		
		{
		SIMPLEPROFILE_SCOPE(pmis_refine_merge);
		
		merge_entropysets_for_entropy_arrays(histos,lambda,speedfit,target_num_arrays,arena);
		}
		
		num_entropysets = histos.size32();
		
		multiarray_printf("after trellis merge num_entropysets : %d\n",num_entropysets);

		if ( num_entropysets == 1 ) return -1;	
	
		//-----------------------
		
		#if 1
		// r:\testsets/matt10gb/10gb/benchmarks/simple/r6004
		// r:\testsets/matt10gb/10gb/benchmarks/simple/r6010
		
		//rrprintfvar(tot_num_intervals);
		if ( tot_num_intervals > 4000 )
		{
			// tot_num_intervals is dangerously high
			// try not to exceed the decoder memory reserved for intervals (~8000)
			//	if we do hit that we'll just fail to output
			//	rather than bail out, try to crank it down first :
			
			// discourage switches :
			//switch_histo_cost_codelen += 8*ENTROPYSET_CODELEN_ONE_BIT;
			// grow faster : (in extreme cases this loops a lot)
			// be careful because switch_histo_cost_codelen is U16
			U32 switch_histo_cost_codelen_32 = (U32)switch_histo_cost_codelen * 2;
			if ( switch_histo_cost_codelen_32 < switch_histo_cost_max )
			{
				switch_histo_cost_codelen = S16_check(switch_histo_cost_codelen_32);
				// force trellis to go again :
				trellis_iter--;
				
				rrPrintf_v2
					("WARNING: tot_num_intervals = %d ; forcing re-trellis; switch_histo_cost_codelen = %d\n",tot_num_intervals,switch_histo_cost_codelen);
			}
			else
			{
				switch_histo_cost_codelen = S16_check(switch_histo_cost_max);
				rrPrintf_v2("WARNING: tot_num_intervals high and hit switch_histo_cost_max!\n");
				// no trellis_iter-- to avoid infinite loop
			}
		}
		#endif
	
		//-----------------------
		#if 0 // <- @@@@ this is marginal
		
		// DISABLED 07-08-2019 :
		//	this causes a feedback that makes tot_num_intervals get very high
		//	as tot_num_intervals gets higher, switch cost goes down, which makes it get higher
		//	that *does* help compression, but it's a very small amount
		// and can make us exceed the decoder memory reserved for intervals (~8000)
		// Leviathan -z9 on DUKEGP.WAD
		
		// first trellis pass uses hard-coded constant switch_histo_cost
		// after first pass, adjust based on our actual array
		//	cost depends on how often we switch & the # of sets
	
		// switching more often makes switch cost go down (probability of switch signal goes up)
		//	
	
		//  -0.003 bpb saved on krakenhctest from this (confirmed on other test sets)
		//	does help but very small
		
		U32 average_interval_len = ((U32)tot_from_len + tot_num_intervals-1) / tot_num_intervals;
		
		// when you send a switch, you have to send the entropyset index you're switching to
		// sending an entropyset index is log2(num_sets)
		U32 cur_switch_histo_cost_codelen = rrlog2_bk_fixedpoint<ENTROPYSET_CODELEN_ONE_BIT_SHIFT>(num_entropysets+1);
		
		
		// when you send a switch, you have to send the next interval len
		// elias gamma to send len is 2*log2+1 bits
		//	but you would have had to send a len anyway (of the interval you're in)
		//	it's a question of do you send N or twice (N/2)
		//	so the cost we add here is 1*log2 , not 2*log2
		cur_switch_histo_cost_codelen += rrlog2_bk_fixedpoint<ENTROPYSET_CODELEN_ONE_BIT_SHIFT>(average_interval_len+1);

		// alternative way to think about it :
		// instead of accounting for our actual scheme (sending interval lens)
		// imagine you are sending a binary event switch/no-switch at each symbol
		// P(switch) = 1/average_interval_len
		// the cost to send a switch is the difference vs sending the imaginary "no switch" flag on each symbol
		// cost = -log2( P(switch) ) - -log2( 1 - P(switch) )

		// The true switch histo cost seems to be around 10-11 bits
		// rrprintf("cur_switch_histo_cost bits : %.3f\n",(double)cur_switch_histo_cost_codelen/ENTROPYSET_CODELEN_ONE_BIT);
		
		// Leviathan, default Lambda, time cost is about 1 bit :
		// switch_histo_cost_time_codelen bits : 1.098
		// rrprintf("switch_histo_cost_time_codelen bits : %.3f\n",(double)switch_histo_cost_time_codelen/ENTROPYSET_CODELEN_ONE_BIT);
		
		// add the lambda time part :
		cur_switch_histo_cost_codelen += switch_histo_cost_time_codelen;
			
		#if 1
		// don't go below 14 bits : it can make us take len 1 intervals which is yuck
		//  -> not doing this clamp helps compression a tiny bit on krakenhctest
		//  -> however doing this clamp helps decode speed
		cur_switch_histo_cost_codelen = RR_MAX(cur_switch_histo_cost_codelen,ENTROPYSET_SYM_INVALID_HIGH_CL);
		// clamp to 15 bits : (to prevent high interval counts)
		//cur_switch_histo_cost_codelen = RR_MAX(cur_switch_histo_cost_codelen,15<<ENTROPYSET_CODELEN_ONE_BIT_SHIFT);
		#endif
		
		// be careful about S16 math :
		cur_switch_histo_cost_codelen = RR_MIN(cur_switch_histo_cost_codelen,switch_histo_cost_max);
			
		switch_histo_cost_codelen = U16_check( cur_switch_histo_cost_codelen );
	
		#endif // switch_histo_cost_codelen adaptive
	
	} // trellis k-means iter
	
	} // scope trellis

	/*
	// histos no longer needed
	// we just use "array_intervals" to output	
	//	(not easy cuz of arena LIFO order)
	histos.release();
	histos_alloc.Release();
	
	histo_codelens.release();
	histo_codelens_alloc.Release();
	*/
			
	{
	SIMPLEPROFILE_SCOPE(pmis_output);
			
	//-------------------------------------------
	
	// sort the histos so that the most often used is first
	//	(not necessarily the longest)
	
	// @@ note this no longer does much of anything useful
	// it was so that I could send entropyset indices with a varbits code
	//	and put the most likely first
	// (also helps with MTF, putting most likely at lower index in the prior)
	// still used just to put the unused sets at the end to collapse them out
	
	index_and_count histo_use_counts[NEWLZ_MULTIARRAYS_ENCODER_MAX_NUM_ARRAYS_STRICT];
	RR_ZERO(histo_use_counts);
	
	RR_ASSERT( num_entropysets <= NEWLZ_MULTIARRAYS_ENCODER_MAX_NUM_ARRAYS_STRICT );
	
	for LOOPINT(i,num_entropysets)
		histo_use_counts[i].index = i;
	
	int tot_num_intervals = 0;
	
	for LOOP(array_i,num_arrays)
	{
	    vector_a<indexed_interval> & intervals = array_intervals[array_i];
	
		tot_num_intervals += intervals.size32();
	
		for LOOPVEC(vi,intervals)
		{
			RR_ASSERT( intervals[vi].len > 0 );
			histo_use_counts[ intervals[vi].histo ].count ++;
		}
	}
	
	// sort histo_use_counts :
	stdsort(histo_use_counts,histo_use_counts+num_entropysets,stdgreater<index_and_count>());
	
	// highest count first :
	RR_ASSERT( histo_use_counts[0].count >= histo_use_counts[1].count );
	
	int old_index_to_new_map[NEWLZ_MULTIARRAYS_ENCODER_MAX_NUM_ARRAYS_STRICT];
	
	for LOOPINT(hi,num_entropysets) 
	{
		int old_index = histo_use_counts[hi].index;
		RR_ASSERT( old_index >= 0 && old_index < num_entropysets );
		old_index_to_new_map[ old_index ] = hi;
	}
	
	//-------------------------------------------
	
	// some of the histos now may be unused
	//	if they were used before the trellis assignment,
	//	but the trellis didn't pick them
	
	// sorted, zeros are at end now :
	while( histo_use_counts[num_entropysets-1].count == 0 )
	{
		num_entropysets--;
		RR_ASSERT( num_entropysets > 0 );
	}
	
	if ( num_entropysets == 1 )
	{
		// degenerate case, did all merges
		return -1;
	}
	
	int num_entropy_arrays = num_entropysets;
	
//	multiarray_printf("@@ num_entropy_arrays = %d tot_num_intervals = %d\n",num_entropy_arrays,tot_num_intervals);
	
	//=====================================================================
	
	int tot_num_indices = tot_num_intervals + (int)num_arrays;

	#if 0
	// track & log the max of num_indices :
	
	//	 (this is at -zs0 ; with normal -zs you get many fewer)
	// tot_num_intervals	5752  on lzt99
	//	JEEBUS 
	// -> that's without the 14 bit clamp
	// with the clamp, much lower :
	// s_tot_num_indices_max : 2404
	// PD3D :
	// s_tot_num_indices_max : 459
	// gametestset ; max is on Fez_Essentials.pak :
	// s_tot_num_indices_max : 3882

	// @@ number of indices is massively effected by switch histo cost
	//	huge difference in 14-16 bits
	//	at 14 bits I've seen over 8000 intervals on DUKEGP.WAD

	static int s_tot_num_indices_max = 0;
	if ( tot_num_indices > s_tot_num_indices_max )
	{
		s_tot_num_indices_max = tot_num_indices;
		if ( tot_num_indices > 100 )
		{
			rrprintfvar(s_tot_num_indices_max);
		}
	}
	#endif

	#if 1
	// ensure we will fit in decoder scratch :
	//	tot_num_indices max of 8K currently
	if ( tot_num_indices*6 >= NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH )
	{
		// encoder will bail out and not send indexed (this is not a compression failure)
		//	we now try to avoid this in the trellis loop
		ooLogError("WARNING: Too many indices for decoder scratch!\n");
		return -1;
	}
	#endif
	
	//=====================================================================
	// all decided - doing output now :
	
	SINTa to_scratch_size = tot_from_len + 32;
	SINTa entropy_array_scratch_size = tot_from_len;
	
	SINTa output_scratch_size = 
		to_scratch_size + 
		entropy_array_scratch_size + 
		tot_num_indices*2 + tot_num_intervals;
	rrScopeArenaAlloc output_scratch_alloc(output_scratch_size,arena);
	U8 * output_scratch_ptr = (U8 *) output_scratch_alloc.m_ptr;
	
	// write to scratch cuz [to] has fallback in it	
	U8 * to = output_scratch_ptr; output_scratch_ptr += to_scratch_size;
	U8 * to_end = output_scratch_ptr;
	
	U8 * to_ptr = to;
	
	// write indexed multi header :
	*to_ptr++ = U8_check(0x80 + num_entropy_arrays);
	
	F32 tot_J = 0;
	tot_J += 1; // header byte
	
	// make the N entropy arrays & output them
	
	vector_a<U8> entropy_array_scratch;
	entropy_array_scratch.provide_arena(output_scratch_ptr,entropy_array_scratch_size);
	output_scratch_ptr += entropy_array_scratch_size;
		
	// no need to try further splits of these entropy arrays :
	entropy_flags &= ~NEWLZ_ARRAY_FLAG_ALLOW_SPLIT;
	
	//S32 entropy_array_lens[NEWLZ_MULTIARRAYS_ENCODER_MAX_NUM_ARRAYS];
		
	for LOOP(newhi,num_entropy_arrays) 
	{
		// we're outputting entropy array "newhi"
		// scan through all the source intervals that chose me, and tack together the bytes
	
		entropy_array_scratch.clear();
	
		for LOOP(array_i,num_arrays)
		{
			vector_a<indexed_interval> & intervals = array_intervals[array_i];
			const U8 * from_ptr = from_ptrs[array_i];
		
			for LOOPVEC(vi,intervals)
			{
				int new_index = old_index_to_new_map[ intervals[vi].histo ];
				RR_ASSERT( new_index >= 0 && new_index < num_entropy_arrays );
			
				if ( new_index == newhi )
				{
					// yes, put him :
					const U8 * ptr = from_ptr + intervals[vi].start;
					const U8 * end = ptr + intervals[vi].len;
					entropy_array_scratch.append(ptr,end);
				}
			}
		}
		
		RR_ASSERT( ! entropy_array_scratch.empty() );
	
		const U8 * entropy_ptr = entropy_array_scratch.data();
		S32 entropy_len = entropy_array_scratch.size32();
	
		//entropy_array_lens[newhi] = entropy_len;
	
		F32 cur_J = LAGRANGE_COST_INVALID;
		SINTa cur_comp_len = newLZ_put_array_small_allowed(to_ptr,to_end,entropy_ptr,entropy_len,
			entropy_flags,lambda,speedfit,&cur_J,ARRAY_DEADLINE_HUGE,arena,compression_level,NULL);
		
		if ( cur_comp_len < 0 )
		{
			return -1;
		}
		
		to_ptr += cur_comp_len;
		tot_J += cur_J;
		
		// as soon as we exceed *pJ , abort :
		if ( tot_J >= *pJ )
			return -1;
	}
	
	multiarray_printf("@@ entropy arrays total : J %.3f len %d (prev J %.3f)\n",tot_J,rrPtrDiff32(to_ptr - to),*pJ);
	
	//-------------------------------------------
	
	// reserve 2 bytes for varbits comp len :
		
	U8 * varbits_comp_len_ptr = to_ptr;
	to_ptr += 2;
	tot_J += 2;		
	
	//-------------------------------------------
	
	// put the interval maps
	// choose the expk's

	#if DO_MTF_TRANSFORM
	U8 mtf_table_init[64];
	mtf_table_init[0] = 63; // invalid
	for LOOP(i,64) mtf_table_init[i] = (U8)( i-1 );
	
	U8 mtf_table[64];
	#endif

	
	// decide how we will output the interval map
	// either two separate entropy arrays
	// or packed together in 44
	
	vector_a<U8> interval_indices;
	vector_a<U8> interval_lens_log2;
	vector_a<U8> interval_indices_and_lens_packed;
		
	interval_indices_and_lens_packed.provide_arena(output_scratch_ptr, tot_num_indices ); output_scratch_ptr += tot_num_indices;
	interval_indices.provide_arena(output_scratch_ptr, tot_num_indices ); output_scratch_ptr += tot_num_indices;
	interval_lens_log2.provide_arena(output_scratch_ptr, tot_num_intervals );output_scratch_ptr += tot_num_intervals;
	
	RR_ASSERT( output_scratch_ptr == (U8 *)output_scratch_alloc.m_ptr + output_scratch_size );

	int max_len_log2 = 0;
	int tot_len_log2 = 0;
	
	for LOOP(array_i,num_arrays)
	{
		vector_a<indexed_interval> & intervals = array_intervals[array_i];
	
	
		#if DO_MTF_TRANSFORM
		memcpy(mtf_table,mtf_table_init,sizeof(mtf_table_init));
		#endif
		
		for LOOPVEC(vi,intervals)
		{
			int new_index = old_index_to_new_map[ intervals[vi].histo ];
			RR_ASSERT( new_index >= 0 && new_index < num_entropy_arrays );
			
			#if DO_MTF_TRANSFORM
			// do MTF transform :
			RR_ASSERT( new_index != mtf_table[0] ); // prev should not repeat
			int mtfi = 1; while( mtf_table[mtfi] != new_index ) mtfi++;
			RR_ASSERT( mtfi < 64 );
			memmove(mtf_table+1,mtf_table,mtfi);
			mtf_table[0] = U8_check(new_index);
			
			interval_indices.push_back( U8_check( mtfi ) ); // mtfi is >= 1 , 0 means EOF
			#else
			
			interval_indices.push_back( U8_check( new_index + 1 ) );
			#endif
			
			// compact out prev :
			//RR_ASSERT( new_index != prev_index );
			//int send_index = new_index - (new_index > prev_index);
			
			int len = intervals[vi].len;
			
			RR_ASSERT( len > 0 && len < (128*1024) );
			
			// @@ only true if switch_histo_cost is clamped up
			//RR_ASSERT( len > 1 || from_lens[array_i] == 1 );
						
			int len_log2 = rrGetBitLevel_V(len)-1;
			interval_lens_log2.push_back( U8_check( len_log2 ) );
			
			max_len_log2 = RR_MAX(len_log2,max_len_log2);
			tot_len_log2 += len_log2;
			
			// interval_indices_and_lens_packed :
			//	just cram 4-bit values in for now
			//	we'll check later if they actually fit
			interval_indices_and_lens_packed.push_back( (U8)( (len_log2<<4) | ( new_index + 1 ) ) );
			
		}
		
		// send the eof :
		interval_indices.push_back( 0 );
		interval_indices_and_lens_packed.push_back( 0 );
	}
	
	RR_ASSERT( interval_indices_and_lens_packed.size() == (size_t)(tot_num_indices) );
	RR_ASSERT( interval_indices.size() == (size_t)(tot_num_indices) );
	RR_ASSERT( interval_lens_log2.size() == (size_t)tot_num_intervals );
	
	//rrprintf("\n");
	
	// len log2's are in [0,16]
	// lens are in [1,2^17-1]
	//	(can never have a len of 0 or the full 128k)
	RR_ASSERT( max_len_log2 <= 16 );
	
	#define VARBITS_FLUSH_END_PAD_BYTES	8
	
	SINTa varbits_est_bits = tot_len_log2;
	
	SINTa varbits_est_bytes = (varbits_est_bits+7)/8;
	varbits_est_bytes += VARBITS_FLUSH_END_PAD_BYTES; // padding to flush a U64
	varbits_est_bytes += VARBITS_FLUSH_END_PAD_BYTES; // just over-estimate for safety
	
	// varbits_est_bytes must be strictly >= the # we actually put
	
	//-------------------------------------------
	// put indices & len log2's :
	
	/**
	
	write the 2 array way first
	compute the J for it

	write the 1 array way on top of it
	will replace only if it beats J

	flag which one was sent in the U16 varbits len

	the log2 payload bits is the same either way

	**/
	
	bool did_packed_single_array = false;
	
	U8 * to_ptr_interval_arrays_start = to_ptr;
	
	F32 J1 = LAGRANGE_COST_INVALID;
	SINTa complen1 = newLZ_put_array_small_allowed(to_ptr,to_end,interval_indices.data(),tot_num_indices,
						entropy_flags,lambda,speedfit,&J1,ARRAY_DEADLINE_HUGE,
						arena,compression_level,NULL);
	if ( complen1 <= 0 ) return -1;
	
	to_ptr += complen1;
	
	F32 J2 = LAGRANGE_COST_INVALID;
	SINTa complen2 = newLZ_put_array_small_allowed(to_ptr,to_end,interval_lens_log2.data(),tot_num_intervals,
						entropy_flags,lambda,speedfit,&J2,ARRAY_DEADLINE_HUGE,
						arena,compression_level,NULL);
	if ( complen2 <= 0 ) return -1;
	
	to_ptr += complen2;
	
	F32 interval_arrays_J = J1+J2;
	
	if ( num_entropy_arrays <= 15 && max_len_log2 <= 15 )
	{
		// only writes if it can beat interval_arrays_J
		SINTa comp_len = newLZ_put_array_small_allowed(to_ptr_interval_arrays_start,to_end,interval_indices_and_lens_packed.data(),tot_num_indices,
						entropy_flags,lambda,speedfit,&interval_arrays_J,ARRAY_DEADLINE_HUGE,
						arena,compression_level,NULL);		
		if ( comp_len > 0 )
		{
			// took it
			to_ptr = to_ptr_interval_arrays_start + comp_len;
			
			// did_packed_single_array is stuffed into a flag bit in the varbits part complen
			did_packed_single_array = true;
		}
	}
	
	tot_J += interval_arrays_J;
	
	//-------------------------------------------

	// check we will have room before starting varbits write :

	// bail out if we won't beat J :
	if ( tot_J+varbits_est_bytes >= *pJ )
		return -1;
	// this should never be hit, the above J check is stricter :
	if ( varbits_est_bytes > rrPtrDiff(to_end - to_ptr) )
		return -1;

	U8 * to_ptr_varbits_part_start = to_ptr;
	SINTa varbits_part_len = 0;

	//-------------------------------------------

	{
		// the rrVarBits_Write do not respect end pointers
		//	 need to be careful about running off the end
		//	the rrVarBits_Writes should never exceed VARBITS_BYTE_END_PADDING
		// -> this is now safe because of the est_bytes check above
		// make sure the U64 overwrites don't crash into eachother
		
		/*
		#define CHECK_VARBITS_FLUSH_END_PAD_BYTES(vb1,vb2) do { \
			RR_ASSERT( vb1_cur+VARBITS_FLUSH_END_PAD_BYTES <= vb2_cur ); \
			if ( vb1_cur+VARBITS_FLUSH_END_PAD_BYTES >= vb2_cur ) \
				NEWLZ_ARRAY_RETURN_FAILURE(); \
			} while(0)
		*/
		
		rrVarBits_Temps();
		rrVarBits_Locals(vb1);
		rrVarBits_PutOpen(vb1,to_ptr);
		
		rrVarBits_Locals(vb2);
		rrVarBits_PutOpen(vb2,to_end);
	
		int whichvb = 0;
		for LOOP(array_i,num_arrays)
		{
			vector_a<indexed_interval> & intervals = array_intervals[array_i];
			for LOOPVEC(vi,intervals)
			{
				U32 len = intervals[vi].len;	
				int len_log2 = rrGetBitLevel_V(len)-1;
				
				U32 topbit = 1U<<len_log2;
				RR_ASSERT( (len >= topbit) && (len < (topbit*2)) );
				len &= topbit-1;
				
				if ( whichvb == 0 )
				{
					rrVarBits_Put(vb1,len,len_log2);
					rrVarBits_Output(vb1);
				}
				else
				{
					rrVarBits_Put(vb2,len,len_log2);
					rrVarBits_OutputBack(vb2);
				}
				
				whichvb ^= 1;
			}
			
			// EOF
				
			if ( did_packed_single_array )
			{
				// put an index with 0 len bits :
				whichvb ^= 1;
			}
		}
		if ( did_packed_single_array )
			RR_ASSERT( whichvb == (tot_num_indices&1) );
	
		rrVarBits_PutFlush8(vb1);
		rrVarBits_PutFlush8Back(vb2);
		
		U8 * vb1_end_ptr = rrVarBits_PutEndPtr(vb1);
		U8 * vb2_end_ptr = NVB(vb2,_cur);
		
		// check for U64 collision :
		RR_ASSERT( rrPtrDiff(vb2_end_ptr - vb1_end_ptr) >= 8 );
		
		SINTa vb1_size_bytes = rrPtrDiff( vb1_end_ptr - to_ptr_varbits_part_start );
		SINTa vb2_size_bytes = rrPtrDiff( to_end - vb2_end_ptr );
		
		memmove(vb1_end_ptr,vb2_end_ptr,vb2_size_bytes);
		
		varbits_part_len = vb1_size_bytes + vb2_size_bytes;
		
		// advance to_ptr to end :
		to_ptr = to_ptr_varbits_part_start + varbits_part_len;

		// varbits_est_bytes should be strictly over :
		//RR_ASSERT( varbits_part_len < varbits_est_bytes );

		// varbits len gets flag bit :
		U16 varbits_comp_u16 = U16_checkA(varbits_part_len);
		if ( did_packed_single_array )
			varbits_comp_u16 |= ((U16)1<<15);
		RR_PUT16_LE(varbits_comp_len_ptr, varbits_comp_u16 );
	}

	tot_J += varbits_part_len;

	//-------------------------------------------
	
	// J time cost estimate of all the interval processing :
	//	(also includes the time for the varbits log2 len decode)
	tot_J += lambda * speedfit->get_multiarrays(tot_num_intervals,tot_from_len);
			
	SINTa tot_comp_len = rrPtrDiff(to_ptr - to);
	
	multiarray_printf("tot_comp_len = %d = entropy arrays %d + selector varbits %d , J time part = %.3f\n",
		tot_comp_len,
		rrPtrDiff32(to_ptr_varbits_part_start - to),
		varbits_part_len,
		tot_J - tot_comp_len);		
	
	RR_ASSERT( tot_J >= tot_comp_len );
	RR_ASSERT( lambda > 0 || tot_J == tot_comp_len );
	
	//=========================

	if ( tot_J >= *pJ )
		return -1;
	
	
	//-------------------------------------------
	
	#if 0 // @@
	
	// log the histo use counts :
	
	multiarray_printf("tot_num_intervals = %d\n",tot_num_intervals);
	
	multiarray_printf("histo use counts : { ");
	for LOOP(i,num_entropy_arrays)
	{
		if ( i == 0 )
			multiarray_printf("%d",histo_use_counts[i].count);
		else
			multiarray_printf(", %d",histo_use_counts[i].count);
	}
	multiarray_printf(" }\n");
	
	// log the intervals :
	
	for LOOP(array_i,num_arrays)
	{
		multiarray_printf("array %2d: ",array_i);
	
		vector<indexed_interval> & intervals = array_intervals[array_i];
	
		for LOOPVEC(vi,intervals)
		{
			int new_index = old_index_to_new_map[ intervals[vi].histo ];
			
			multiarray_printf("[%d - %d] ",new_index,intervals[vi].len);
		}
		multiarray_printf("\n");
	}
		
	#endif
	
	//-------------------------------------------
	
	if ( tot_comp_len > rrPtrDiff(out_end-out_ptr) )
	{
		ooLogError("not enough comp space");
		return -1;
	}

	*pJ = tot_J;
	memcpy(out_ptr,to,tot_comp_len);
	return tot_comp_len;
	} // pmis_output
}
				
					
// newLZ_put_multiarray_indexed unconditionally writes to [out_ptr]
	
SINTa newLZ_put_multiarray_indexed(U8 * const out_ptr, U8 * const out_end,
								const U8 * const * from_ptrs, const SINTa * from_lens, SINTa num_arrays,
								U32 entropy_flags,
								F32 lambda,const OodleSpeedFit * speedfit, F32 * pJ,
								rrArenaAllocator * arena, int compression_level)
{
	SIMPLEPROFILE_SCOPE(put_multiarray_indexed);

	RR_ASSERT( *pJ == LAGRANGE_COST_INVALID );
	U32 sub_entropy_flags = entropy_flags;

	if ( compression_level < 8 )
	{
		// @@@@ try further splits of these entropy arrays ? :
		//	doubles the number of calls to pmis , pretty big affect on encode time
		// leave it on for top level, turn it off otherwise
		//	(pmis internally turns this off, what we're toggling here is whether put_identity does it)
		//	essentially it's an alternate splitter
	
		// could leave normal splits on, they're pretty fast
		// but they also don't seem to help much
		// the compression difference comes from indexed splits
		//sub_entropy_flags &= ~NEWLZ_ARRAY_FLAG_ALLOW_SPLIT;
		sub_entropy_flags &= ~NEWLZ_ARRAY_FLAG_ALLOW_SPLIT_INDEXED;
	}
	
	// set a baseline with put_identity
	// put_identity tries internal non-indexed splits of each array

	// newLZ_put_multiarray_identity writes unconditionally
	SINTa identity_len = newLZ_put_multiarray_identity(out_ptr,out_end,from_ptrs,from_lens,num_arrays,sub_entropy_flags,lambda,speedfit,pJ,arena,compression_level);

	// If indexed splits are disabled in the original entropy flags,
	// identity split is the only thing we ever try.
	if ( ( entropy_flags & NEWLZ_ARRAY_FLAG_ALLOW_SPLIT_INDEXED ) == 0 )
		return identity_len;

	// _indexed_sub will only write [to] if it can beat *pJ

	SINTa indexed_comp_len = 
			newLZ_put_multiarray_indexed_sub(out_ptr,out_end,
				from_ptrs,from_lens,num_arrays,
				sub_entropy_flags,lambda,speedfit,pJ,
				arena,compression_level);
				
	if ( indexed_comp_len > 0 )
		return indexed_comp_len;
	else
		return identity_len;
}
								

// this is what NEWLZ_ARRAY_TYPE_SPLIT calls								
// newLZ_put_array_split only writes to [to] if it can make a J < prev_J
// newLZ_put_array_sub_split does not include writing the new array header, that's done by newLZ_put_array_histo
SINTa newLZ_put_array_sub_split(U8 * const to, U8 * const to_end, const U8 * const from, SINTa from_len, const U32 * histogram, 
								rrArenaAllocator * arena, int compression_level, U32 entropy_flags, F32 lambda,const OodleSpeedFit * speedfit, F32 prev_J, F32 * pJ)
{
	// write to scratch
	// make sure we win J before we overwrite [to]
	// return comp_len if we put anything
	// else return -1

	SIMPLEPROFILE_SCOPE(put_array_split);

	// must be at least size of 3 huff arrays :
	if ( from_len < NEWLZ_HUFF_ARRAY_MIN_SIZE*3 )
		return -1;

	//RR_ASSERT( verify_histo(from,from_len,histogram,256) );
	
	//=======================================
	
	// make split decision :
	
	SINTa comp_len;
	
	if ( from_len >= c_split_N_initial_size*3 ) // enough to get 3 initial splits
	{
		comp_len = newLZ_put_array_sub_split_N(to,to_end,from,from_len,histogram,arena,compression_level,entropy_flags,lambda,speedfit,prev_J,pJ);
	}
	else
	{
		// note newLZ_put_array_sub_split_2 can sometimes beat split_N
		//	even on larger arrays
		//	because it does the thing of trying different initial split points		
	
		comp_len = newLZ_put_array_sub_split_2(to,to_end,from,from_len,histogram,arena,compression_level,entropy_flags,lambda,speedfit,prev_J,pJ);
	}
	
	RR_ASSERT( comp_len == -1 || *pJ >= comp_len+5 );
		
	#if 1
	if ( entropy_flags & NEWLZ_ARRAY_FLAG_ALLOW_SPLIT_INDEXED )
	{
		// try indexed too  
		// -> (this is single arrays indexed)
		
		// @@ if you're trying indexed splits, maybe don't bother trying non-indexed?
		//	could detect the simple case in indexed (num_arrays == 1 && num_intervals == num_entropy_arrays)
		//	just allows you to save the interval packing and the memcpies on decode
		
		prev_J = RR_MIN(prev_J,*pJ);
		F32 indexed_J = prev_J - 5; // take off header size
		
		// we are a multi-array with 1 arrays :
		const U8 * const * from_ptrs = &from;
		SINTa * from_lens = &from_len;
		int num_arrays = 1;
		
		SINTa indexed_comp_len = 
				newLZ_put_multiarray_indexed_sub(to,to_end,
					from_ptrs,from_lens,num_arrays,
					entropy_flags,lambda,speedfit,&indexed_J,
					arena,compression_level);
		
		// add newlz_array header size to J :
		indexed_J += 5;
		
		RR_ASSERT( indexed_comp_len == -1 || indexed_J >= indexed_comp_len+5 );
	
		if ( indexed_comp_len > 0 )
		{		
			RR_ASSERT( indexed_J <= prev_J );
			*pJ = indexed_J;
			
			return indexed_comp_len;
		}
	}
	#endif
	
	return comp_len;
}

OODLE_NS_END
