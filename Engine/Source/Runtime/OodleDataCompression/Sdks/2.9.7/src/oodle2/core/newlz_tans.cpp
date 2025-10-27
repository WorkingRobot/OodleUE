// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "oodlebase.h"
#include "rrvarbits.h"
#include "newlz_tans.h"
#include "newlz_tans.inl"
#include "rrmath.h"
#include "rrmem.h"
#include "cbradutil.h"
#include "templates/rrstl.h"
#include "histogram.h"

#define RRVBC_INLINE
#include "rrvarbitcodes.h"
#include "rrvarbitcodes.cpp"
#include "newlz_block_coders.h"
#include "newlz_block_coders.inl"
#include "rrcompressutil.h"
#include "rrstackarray.h"

//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"

#include "templates/rralgorithm.h"
#include "sloppymemset.inl"
#include "lzasserts.h"

OODLE_NS_START

//=====================================================

/**

NOTE :

 encode state is true TANS [L,2L)
		(you need that top bit on, when you shift down it acts like a bit counter)
 decode "state" is actually an "x" in [0,L)


**/

//=====================================================

SINTa newlz_tans_Encoder_Size( S32 L_bits )
{
	newlz_tans_Encoder * enc = (newlz_tans_Encoder *) 0;
	RR_UNUSED_VARIABLE(enc);

	S32 L = 1<<L_bits;
	
	SINTa size = sizeof(newlz_tans_Encoder);
	size = rrAlignUpA(size,16);
	size += L * sizeof(enc->encode_table_packed[0]);
	size = rrAlignUpA(size,16);
	
	return size;
}

SINTa newlz_tans_Decoder_Size( S32 L_bits )
{
	S32 L = 1<<L_bits;
	
	SINTa size = sizeof(newlz_tans_Decoder);
	size = rrAlignUpA(size,16);
	size += L * sizeof(tans_decode_entry_U8);
	size = rrAlignUpA(size,16);
	
	return size;
}

newlz_tans_Encoder * newlz_tans_Encoder_Init( void * memory, SINTa memSize, S32 L_bits )
{
	if ( memSize < newlz_tans_Encoder_Size(L_bits) )
		return NULL;
	
	newlz_tans_Encoder * enc = (newlz_tans_Encoder *) memory;
	U8 * end = U8_void(memory) + sizeof(*enc);
	end = rrAlignUpPointer(end,16);
	
	enc->alphabet = 256;
	S32 L = 1<<L_bits;
	enc->L = L;
	enc->L_bits = L_bits;
	
	end = rrAlignUpPointer(end,16);
	
	enc->encode_table_packed = (U16 *) end;
	end += L * sizeof(enc->encode_table_packed[0]);
	
	RR_ASSERT( rrPtrDiff( end - U8_void(memory) ) <= newlz_tans_Encoder_Size(L_bits) );

	return enc;	
}

newlz_tans_Decoder * newlz_tans_Decoder_Init( void * memory, SINTa memSize, S32 L_bits )
{
	if ( memSize < newlz_tans_Decoder_Size(L_bits) )
		return NULL;
	
	newlz_tans_Decoder * dec = (newlz_tans_Decoder *) memory;
	U8 * end = U8_void(memory) + sizeof(newlz_tans_Decoder);
	end = rrAlignUpPointer(end,16);
	
	S32 L = 1<<L_bits;
	dec->L = L;
	dec->L_bits = L_bits;
	
	dec->decode_table_U8 = (tans_decode_entry_U8 *) end;
	end += L * sizeof(dec->decode_table_U8[0]);
	
	RR_ASSERT( rrPtrDiff( end - U8_void(memory) ) <= newlz_tans_Decoder_Size(L_bits) );

	return dec;	
}

//===================================

static RADFORCEINLINE U32 bsr( U32 val )
{
	RR_ASSERT( val != 0 );
    int b = rrClz32(val) ;
    return 31-b;
}
    
//===================================
/*********

The "bucket" method does round-robin symbols to buckets

eg. if buckets = 4 and count = 5, does :

[AA...A...A...A...]

this is different than the "radix" method which computes the desired
cumprob target location for each symbol instance, and then sorts them.

With the radix method, more bits of radix = more precise.  It's an approximation
of the true sort.

In this "bucket" method, it is *not* an approximation of the true sort.
More bits is not necessarily better.

(in particular, when symbol count is large, more buckets = better, but when
the symbol's counts are low, more buckets just means the symbol doesn't spread very
well.  The issue is that the symbol's spread is not related to its count)

eg.

if count = 2 , buckets = 4 , does :

[A....A...........]

(depending on where the bucket cursor is)

while "radix" would try to put them at the 1/3 and 2/3 points :

[.....A.....A.....]

at higher bucket numbers, the low count distributions get *worse*

-----------

This "bucket" method is actually just about the same as the FSE prime scatter method
with step = table_size/4 + 1

FSE scatter (two pass / monotonic) and "bucket" get about the same compression, but
bucket can be done a bit faster.

**********/

#define BUCKET_BITS	2 // NOTE: this is part of the bitstream and can't change

#define BUCKET_SIZE	(1<<BUCKET_BITS)
#define BUCKET_MASK  (BUCKET_SIZE-1)

//===================================

void newlz_tans_Encoder_Fill( newlz_tans_Encoder * tables,
	const U32 * normalized_counts, int alphabet )
{
	RR_ASSERT( alphabet <= 256 );
	RR_ASSERT( sum(normalized_counts,normalized_counts+alphabet) == (U32)tables->L );	

	//===================================================

	while( alphabet > 0 && normalized_counts[alphabet-1] == 0 )
		alphabet--;

	RR_ASSERT( alphabet > 0 );
	RR_ASSERT( sum(normalized_counts,normalized_counts+alphabet) == (U32)tables->L );	

	tables->alphabet = alphabet;
	
	int L = tables->L;
	int L_bits = tables->L_bits;
	
	if ( normalized_counts[alphabet-1] == (U32) L )
	{
		// degenerate; only one symbol
		
		int sym = alphabet-1;
		
		tans_encode_entry & ee = tables->encode_sym_table[sym];
		
		ee.max_state_numbits = check_value_cast<U8>(0);	
		ee.max_state_thresh = check_value_cast<U16>( 2*L );
				
		//ee.packed_table_ptr = tables->encode_table_packed - L;
		
		// subtract count now since we will add (count+index) when encoding
		//	this is technically illegal pointer math (could go negative) so force to UINTa
		ee.packed_table_ptr_uinta = ((UINTa)tables->encode_table_packed) - sizeof(U16)*L;
		//ee.packed_table_offset = (t_packed_table_offset)( - (S32)sizeof(U16)*L );
			
		// fill all states to be nops :
		for(int state=L;state<2*L;state++)
		{
			U16 * packed_table_ptr = tans_encode_table_ptr(tables->encode_table_packed,&ee,state);
			*packed_table_ptr = check_value_cast<U16>( state );
		}
				
		return;
	}
	
	//===================================================
	// bucket
			
	U32 bucket_start[BUCKET_SIZE];
	
	/*
	
	// with no singletons :	

	for LOOP(r,BUCKET_SIZE)
	{
		bucket_start[r] = r << (L_bits - BUCKET_BITS);
	}
	
	*/
	
	// YES DO_SINGLETONS_AT_END
	
	int num_singletons = 0;
	for LOOP(sym,alphabet)
	{
		U32 count = normalized_counts[sym];
		if ( count == 1 )
			num_singletons++;
	}
	
	U32 bucket_total = L - num_singletons;
	
	//int run_len = (count + ((cumulative_count + BUCKET_MASK-r) & BUCKET_MASK)) >> BUCKET_BITS;
	U32 bucket_even = bucket_total >> 	BUCKET_BITS;
	U32 bucket_remainder = bucket_total & BUCKET_MASK;
	
	U32 bucket_cum = 0;
	for LOOP(r,BUCKET_SIZE)
	{
		bucket_start[r] = bucket_cum;
		bucket_cum += bucket_even;
		if ( r < (int)bucket_remainder ) bucket_cum ++;
	}
	RR_ASSERT( bucket_cum == bucket_total );
	
	U32 singleton_next = bucket_total;
		
	// bucket_start[r] now tells you where that bucket goes in the array
	
	// read out of buckets and build encode table at the same time :
	int cumulative_count = 0;
	for LOOP(sym,alphabet)
	{
		U32 count = normalized_counts[sym];
		if ( count == 0 )
		{
  			tables->encode_sym_table[sym].packed_table_ptr_uinta = 0;
			//tables->encode_sym_table[sym].packed_table_offset = 0;
		}
		else if ( count == 1 ) // helps speed a tiny bit to special case 1
		{
			tans_encode_entry & ee = tables->encode_sym_table[sym];
			
			ee.max_state_numbits = check_value_cast<U8>(L_bits);	
			ee.max_state_thresh = check_value_cast<U16>( 2*L );
			
			// singletons at end :
			
			U16 * packed_table_ptr = tables->encode_table_packed + singleton_next;
			// subtract count now since we will add (count+index) when encoding
			//	this is technically illegal pointer math (could go negative) so force to UINTa
			//ee.packed_table_ptr = packed_table_ptr - count;
			ee.packed_table_ptr_uinta = ((UINTa)packed_table_ptr) - sizeof(U16)*count;
			//ee.packed_table_offset = (t_packed_table_offset)( sizeof(U16)*(singleton_next - (S32)count) );
			
			U32 to = singleton_next; singleton_next++;
			
			packed_table_ptr[0] = check_value_cast<U16>( L + to );
						
		}
		else
		{		
			U32 num_bits = L_bits - rrIlog2ceil(count);
			RR_ASSERT( num_bits == rrIlog2floor(L/count) );
			RR_ASSERT( num_bits == L_bits - bsr(2*count-1) );
			
			tans_encode_entry & ee = tables->encode_sym_table[sym];
			
			ee.max_state_numbits = check_value_cast<U8>(num_bits);	
			ee.max_state_thresh = check_value_cast<U16>( (count+count)<<num_bits );
					
			// + cumulative_count to pack us in the array
			// - nc because that is the lowest state that will index this array
			//tables->encode_sym_table[a].packed_table_offset = check_value_cast<int16>(cumulative_count - nc);
			U16 * packed_table_ptr = tables->encode_table_packed + cumulative_count;
			// subtract count now since we will add (count+index) when encoding
			//	this is technically illegal pointer math (could go negative) so force to UINTa
			//ee.packed_table_ptr = packed_table_ptr - count;
			//ee.packed_table_offset = (t_packed_table_offset)( sizeof(U16)*(cumulative_count - (S32)count) );
  			ee.packed_table_ptr_uinta = ((UINTa)packed_table_ptr) - sizeof(U16)*count;
			
			for LOOP(r,BUCKET_SIZE)
			{
				int run_len = (count + ((cumulative_count + BUCKET_MASK-r) & BUCKET_MASK)) >> BUCKET_BITS;
				          
				U32 to = bucket_start[r];
					
				for LOOP(run_i,run_len)
				{
					*packed_table_ptr++ = check_value_cast<U16>( L + to );
					to++;
				}
				
				bucket_start[r] += run_len;;
			}
			
			cumulative_count += count;
		}
	}
	
	RR_ASSERT( cumulative_count == (SINTa)bucket_total );
	RR_ASSERT( singleton_next == (U32)L );
}


void newlz_tans_Decoder_Fill( newlz_tans_Decoder * tables,
	const newlz_tans_UnpackedCounts * counts )
{
	SIMPLEPROFILE_SCOPE(tans_tablefill);

	int L = tables->L;
	int L_bits = tables->L_bits;

	RR_ASSERT( 0 <= counts->num_singles && counts->num_singles <= 256 );
	RR_ASSERT( 0 <= counts->num_larger && counts->num_larger <= 256 );
	RR_ASSERT( counts->num_singles + counts->num_larger <= 256 );

	//=====================================

	/**

	SimpleProf            :seconds  calls     count :  clk/call clk/count
	tansfill_table        : 0.0074    780       780 :   16305.1  16305.07
	tansfill_table_hi     : 0.0039  11650   1514300 :     586.1      4.51
	tansfill_table_lo     : 0.0007  11720     58860 :      98.4     19.59
	tansfill_table_1      : 0.0004  24280     24280 :      25.0     24.99

	timings are not interesting because of high overhead
	calls & counts are useful

	"lo" = count 2-8
	"hi" = count > 8

	95% of slots filled are in the high count (count > 8) path
	50% of symbols are in the count==1 path
	25% of symbols are in the count in 2-8 path

	average count in the "hi" case is 137
	average count in the "lo" case is 5
		(5 is exactly the middle of the 2-8 range, so no skew of probabilities there)

	**/

	tans_decode_entry_U8 * detable = tables->decode_table_U8;
	RR_ASSERT( detable != NULL );

	RR_COMPILER_ASSERT( sizeof(tans_decode_entry_U8) == sizeof(U64) );
	U64 * detable64 = (U64 *)detable;

	U64 * bucket_ptr[BUCKET_SIZE];

	U32 bucket_total = L - counts->num_singles;

	U32 bucket_even = bucket_total >> 	BUCKET_BITS;
	U32 bucket_remainder = bucket_total & BUCKET_MASK;

	U32 bucket_cum = 0;
	for LOOP(r,BUCKET_SIZE)
	{
		bucket_ptr[r] = detable64 + bucket_cum;
		bucket_cum += bucket_even;
		if ( r < (int)bucket_remainder ) bucket_cum ++;
	}
	RR_ASSERT( bucket_cum == bucket_total );

	// do singletons
	{
		//SIMPLEPROFILE_SCOPE(tansfill_single);
		tans_decode_entry_U8 count_1_entry;
		count_1_entry.nextst = 0;
		count_1_entry.len = check_value_cast<U8>( L_bits );
		count_1_entry.mask = check_value_cast<U16>( (1<<L_bits)-1 );
		U64 count_1_entry_64 = RR_GET64_NATIVE(&count_1_entry);

		U64 * pdest = detable64 + bucket_total;
		const U8 * singleton_end = counts->singles + counts->num_singles;

		// NOTE(fg); could SIMD this quite easily
		// but profile says not worth it (~70 out of ~2150 cycles
		// in lzt99 -z5 -c13 decode).
		for (const U8 * singleton = counts->singles; singleton != singleton_end; ++singleton)
		{
			*pdest = count_1_entry_64;
			((tans_decode_entry_U8 *)pdest)->sym = *singleton;
			++pdest;
		}
		RR_ASSERT( pdest == detable64 + L );
	}

	// count ranks to histo :
	int cumulative_count = 0;
	const U32 * larger_end = counts->larger + counts->num_larger;
	for (const U32 * cur_count = counts->larger; cur_count != larger_end; ++cur_count)
	{
		SINTa sym = *cur_count >> 16;
		U32 count = *cur_count & 0xffff;
		RR_ASSERT( count > 1 );

		if ( count <= BUCKET_SIZE )
		{
			//SIMPLEPROFILE_SCOPE_N(tansfill_table_lo,count);

			// count <= BUCKET_SIZE so 1 or 0 entries per lane

			// construct a bit mask of which lanes get entries
			// each lane gets 0 or 1 entries
			// make a bit mask of "BUCKET_SIZE" bits (not BUCKET_BITS!)

			RR_COMPILER_ASSERT( BUCKET_SIZE <= 16 );
			// make "count" bits on :
			U32 mask = (1U<<count)-1;
			// they start at cumulative_count
			int base = cumulative_count&BUCKET_MASK;

			mask <<= base;
			// wrap around :
			mask |= mask >> BUCKET_SIZE;
			//mask &= BUCKET_MASK; // higher on bits are ignored, so can skip this

			// now mask has on bits where the bins should go

			// you can loop for(BUCKET_SIZE) and test each bit
			// or you can loop for(count) (which is popcount of mask) and ctz to find the lane

			// NOTE(fg): thing I tried: prepare "prototype" runs for small counts that we can
			// just copy here (note all fields we write except "sym" only depend on count).
			// It works, but isn't measurably faster, so meh.

			for LOOP(c,(int)count)
			{
				RR_ASSERT( mask != 0 );
				int r = rrCtz32(mask);
				RR_ASSERT( r < BUCKET_SIZE );
				mask &= mask-1; // turn off the bit we used (could also use mask & -mask)

				U64 * pdest = bucket_ptr[r]; bucket_ptr[r] += 1;

				int from_state = count+c;
				int num_bits = L_bits - bsr(from_state);

				tans_decode_entry_U8 & entry = *((tans_decode_entry_U8 *)pdest);

				entry.sym = check_value_cast<U8>( sym );
				entry.len = check_value_cast<U8>( num_bits );
				entry.mask = check_value_cast<U16>( (1<<num_bits)-1 );
				entry.nextst = check_value_cast<U16>( (from_state << num_bits) & (L-1) );
			}

			cumulative_count += count;
		}
		else
		{
			//SIMPLEPROFILE_SCOPE_N(tansfill_table_hi,count);

			#if 0

			// simple dimple!

			int c = 0;
			for LOOP(r,BUCKET_SIZE)
			{
				int run_len = (count + ((cumulative_count + BUCKET_MASK-r) & BUCKET_MASK)) >> BUCKET_BITS;

				U64 * pdest = bucket_ptr[r];

				for LOOP(run_i,run_len)
				{
					// decoder numbits :
					int from_state = count+c;
					c++;
					int num_bits = L_bits - bsr(from_state);

					tans_decode_entry_U8 & entry = *((tans_decode_entry_U8 *)pdest);

					entry.sym = check_value_cast<U8>( sym );
					entry.len = check_value_cast<U8>( num_bits );
					entry.mask = check_value_cast<U16>( (1<<num_bits)-1 );
					entry.nextst = check_value_cast<U16>( (from_state << num_bits) & (L-1) );

					pdest++;
				}

				bucket_ptr[r] += run_len;
			}

			RR_ASSERT( c == (int)count );

			#else

			// find the bsr threshold
			// simple loops to fill detable[]

			int bsr_start = bsr(count);
			int num_bits = L_bits - bsr_start;
			int count_threshold = (1<<(bsr_start+1)) - count;
			// first count_threshold entries use "num_bits"
			// next (count-count_threshold) use num_bits-1

			// count_threshold == count when count is a power of 2
			//	in that case all codes have same len
			RR_ASSERT( count_threshold > 0 && count_threshold <= (int)count );

			// tans_decode_entry_U8 is 64 bits
			//	fill one out then just stamp it down over and over
			//	target address is jumping around per the shuffle
			U32 one_shifted_num_bits = 1<<num_bits;
			U32 num_bits_mask = one_shifted_num_bits-1;

			tans_decode_entry_U8 entry;
			entry.sym = check_value_cast<U8>( sym );
			entry.len = check_value_cast<U8>( num_bits );
			entry.mask = check_value_cast<U16>( num_bits_mask );
			entry.nextst = check_value_cast<U16>( (count << num_bits)&(L-1) );

			RR_ASSERT( sizeof(tans_decode_entry_U8) == sizeof(U64) );

			tans_decode_entry_U8 entry_inc = { 0 };
			entry_inc.nextst = check_value_cast<U16>( one_shifted_num_bits );

			// different struct layouts on Jaguar & non-Jaguar ; this handles either way :
			U64 entry64 = RR_GET64_NATIVE(&entry);
			U64 entry64inc = RR_GET64_NATIVE(&entry_inc);

			int countdown = count_threshold;
			for LOOP(r,BUCKET_SIZE)
			{
				int run_len = (count + ((cumulative_count + BUCKET_MASK-r) & BUCKET_MASK)) >> BUCKET_BITS;

				U64 * pdest = bucket_ptr[r];

				// if the num_bits step down can occur within this run :
				if ( countdown < run_len )
				{
					for LOOP(run_i,countdown)
					{
						*pdest++ = entry64;
						entry64 += entry64inc;
					}

					num_bits--;

					entry.len = check_value_cast<U8>( num_bits );
					entry.mask = check_value_cast<U16>( num_bits_mask>>1 );
					//entry.nextst = check_value_cast<U16>( ((count + count_threshold) << num_bits)&(L-1) );
					entry.nextst = 0; // NOTE(fg): the above math works out to (1<<L_bits) & (L-1)

					entry64 = RR_GET64_NATIVE(&entry);

					// increment is just one bit level lower :
					entry64inc >>= 1;

					for LOOP(run_i,(run_len-countdown))
					{
						*pdest++ = entry64;
						entry64 += entry64inc;
					}

					countdown = count; // don't do again
				}
				else
				{
					for LOOP(run_i,run_len)
					{
						*pdest++ = entry64;
						entry64 += entry64inc;
					}

					countdown -= run_len;
				}

				bucket_ptr[r] = pdest;
			}

			#endif

			cumulative_count += count;
		}
	}

	RR_ASSERT( cumulative_count == (int)bucket_total );

	#ifdef RR_DO_ASSERTS
	// check that all the bucket_ptrs got to their ends
	bucket_cum = 0;
	for LOOP(r,BUCKET_SIZE)
	{
		bucket_cum += bucket_even;
		if ( r < (int)bucket_remainder ) bucket_cum ++;
		U64 * end = detable64 + bucket_cum;
		RR_ASSERT( bucket_ptr[r] == end );
	}
	RR_ASSERT( bucket_cum == bucket_total );
	#endif
}

//===================================================================================

/**

PackCodeLens4

intended for when very few symbols have non-zero counts

sends a set of [sym:count] pairs

sorts by increasing count
then can send them as delta from previous
and the last count is implicit from the sum to L

**/

#define NUM_NON_ZERO_PC4	7 // NOTE(fg): this is (indirectly) in the format via c_num_non_zero_bits below, very careful here!

static const int c_num_non_zero_bits = rrGetBitLevel_C(NUM_NON_ZERO_PC4-2);

// intended to be used with ints
template<typename T>
static void small_insertion_sort(T * begin, T * end)
{
	if (begin == end)
		return;

	for (T *cur = begin + 1; cur != end; ++cur)
	{
		T val = *cur;

		// move things out of the way and find insertion pos
		T *pos = cur;
		while (pos > begin && val < *(pos - 1))
		{
			*pos = *(pos - 1);
			pos--;
		}

		*pos = val;
	}
}
	
static void RADFORCEINLINE newlz_tans_PackCounts4(S32 L_bits,
						const U32 * counts, S32 alphabet,
						S32 num_non_zero, rrVarBits_FuncArgs(vbl) )
{
	RR_ASSERT( num_non_zero >= 2 && num_non_zero <= NUM_NON_ZERO_PC4 );
	
	rrVarBits_Output(vbl);
    rrVarBits_Put(vbl,num_non_zero-2,c_num_non_zero_bits);
    
    // alphabet should be reduced :
	RR_ASSERT( counts[alphabet-1] != 0 );
	
	// find the non-zero count symbols :
	U32 got_counts_and_syms[NUM_NON_ZERO_PC4]; // (count<<16) | sym - then sort increasing
    int got = 0;
    
    for(int sym=0;sym<alphabet;sym++)
    {
		if ( counts[sym] == 0 )
			continue;
		
		got_counts_and_syms[got] = (counts[sym] << 16) | sym;
		got++;
    }
    
    RR_ASSERT( got == num_non_zero );

	// sort by count
	small_insertion_sort(got_counts_and_syms, got_counts_and_syms + got);
	   
	// counts increase :
	RR_ASSERT( got_counts_and_syms[1] > got_counts_and_syms[0] );
    
    int icount = num_non_zero;
	icount--; // normalized: don't include last

    //	I don't love this max_delta_bits scheme
    //	could probably just be EliasGamma or something
    //	but I have zero test cases for this
	//
	// NOTE(fg): seeing TANS chosen quite a bit in Leviathan -z5 -zs1 now,
	// and PackCounts4 does get its share of use, so we have some data now.
	//
	// Alternative approaches I've tried for the deltas:
	// - EG codes with a single compile-time fixed k (k=0 or 1 seemed best)
	// - EG codes with per-packcounts k, sent in bitstream (3 bits)
	// - EG codes with adaptive k
	// - a fancier predictor that takes L-sum into account
	//
	// all performed worse than this scheme. So I think this is fairly good.
    S32 max_delta_bits = 0;

    // find the maximum delta count :
    U32 prev = 0;
	for(int i=0;i<icount;i++)
	{
		U32 cur = got_counts_and_syms[i] >> 16;
		RR_ASSERT( cur >= prev );
		S32 delta = cur - prev;
		prev = cur;
		S32 delta_bits = rrGetBitLevel_V(delta);
		max_delta_bits = RR_MAX(max_delta_bits,delta_bits);
	}
	RR_ASSERT( max_delta_bits > 0 );
	if ( max_delta_bits == 0 ) max_delta_bits = 1;
	RR_ASSERT( max_delta_bits <= L_bits );
    
    // max_delta_bits_bits is usually 4
    U32 max_delta_bits_bits = rrGetBitLevel_V(L_bits);
    rrVarBits_Put(vbl,max_delta_bits,max_delta_bits_bits);
    
    // send the deltas :
	prev = 0;
	for(int i=0;i<icount;i++)
	{
		U32 cur = got_counts_and_syms[i] >> 16;
		RR_ASSERT( cur >= prev );
		U32 delta = cur - prev;
		prev = cur;
		
		rrVarBits_Output(vbl);
        rrVarBits_Put(vbl,got_counts_and_syms[i] & 0xff,8);
        rrVarBits_Put(vbl,delta,max_delta_bits);
	}
	
	rrVarBits_Output(vbl);
		
	// put last sym id
	rrVarBits_Put(vbl,got_counts_and_syms[num_non_zero-1] & 0xff,8);
	// don't put last count, it's implicit
}

static bool RADFORCEINLINE newlz_tans_UnPackCounts4(S32 L_bits,
						newlz_tans_UnpackedCounts * counts,
						rrVarBits_FuncArgs(vbl) )
{
	U8 seen[256] = {}; // zeroed
	U32 L = 1U<<L_bits;

	rrVarBits_Temps();
	rrVarBits_Refill_Safe(vbl);
    S32 num_non_zero = (S32) rrVarBits_Get_C(vbl,c_num_non_zero_bits) + 2;

    // max_delta_bits_bits is usually 4
    U32 max_delta_bits_bits = rrGetBitLevel_V(L_bits);
    S32 max_delta_bits = (S32) rrVarBits_Get_V(vbl,max_delta_bits_bits);
    if ( max_delta_bits == 0 )
    {
        ooLogError("max_delta_bits == 0\n");
        return false;
    }
    if ( max_delta_bits > L_bits )
    {
        ooLogError("max_delta_bits > L_bits\n");
        return false;
    }
    
    U32 cur = 0;
    U32 sum = 0;
	U8 * cur_single = counts->singles;
	U32 * cur_larger = counts->larger;

    for(int i=0;i<(num_non_zero-1);i++) 
    {
	    rrVarBits_Refill_Safe(vbl);

        int sym = (int) rrVarBits_Get_C(vbl,8);
        
        REQUIRE_FUZZ_RETURN( ! seen[sym] , false );
	
        U32 delta = (U32) rrVarBits_Get_V(vbl,max_delta_bits);
        
		cur += delta;

        REQUIRE_FUZZ_RETURN( cur > 0 , false );
			
		seen[sym] = 1;
		if ( cur == 1 )
			*cur_single++ = (U8)sym;
		else
			*cur_larger++ = (sym << 16) | cur;

		sum += cur;
        REQUIRE_FUZZ_RETURN( sum < L , false );
    }
	
    // get last :
    rrVarBits_Refill_Safe(vbl);
    int mps = (int) rrVarBits_Get_C(vbl,8);
    
    REQUIRE_FUZZ_RETURN( ! seen[mps] , false );
	
	U32 last = L - sum;
	
	// last=1 -> don't even bother with supporting this
	// (it can't happen for realistic choices of L, i.e. >= 32,
	// since we won't let that many syms going through here)
	if ( last < cur || last <= 1 )
	{
	   ooLogError("last too small\n");
	   return false;
	}

	*cur_larger++ = (mps << 16) | last;

	// Table fill wants symbols in increasing order, so sort
	// them back
	small_insertion_sort(counts->singles, cur_single);
	small_insertion_sort(counts->larger, cur_larger);

	counts->num_singles = rrPtrDiff32( cur_single - counts->singles );
	counts->num_larger = rrPtrDiff32( cur_larger - counts->larger );

	return true;
}

//=============================================================

/**

newlz_tans_PackCounts_CountDelta :

send counts in symbol order
predict count from neighbors
send delta from prediction

The first version of this (newlz_tans_PackCounts8) was copied from Huff
and was doing the log2 of count
then sending bits raw

-> that was totally fine and I could go back to that

doing count delta turns out to not be awesome
works best if the predictor is in log2 space

the issue is that big spikes in count should be treated as rare singletons

===========

eliding MPS count, using the L normalization knowledge :

find MPS and second MPS
replace MPS count with (second MPS count +1)
so it is still the MPS so we can identify it
but count is often much lower

without MPS elision :

test_misc :  1,115,177 ->   826,446 =  5.929 bpb =  1.349 to 1 

with :

test_misc :  1,115,177 ->   826,427 =  5.929 bpb =  1.349 to 1 

-> meh , not worth much for a decent amount of work

**/

//*

// bad !
// prediction in linear count space
// test_misc :  1,115,177 ->   826,882 =  5.932 bpb =  1.349 to 1 

// with MIN in update :
// test_misc :  1,115,177 ->   826,573 =  5.930 bpb =  1.349 to 1 
// test_misc :  1,115,177 ->   826,488 =  5.929 bpb =  1.349 to 1 
// test_misc :  1,115,177 ->   826,446 =  5.929 bpb =  1.349 to 1 
//
// older experiments: predict in log2 count space
// but these are more complex and did worse than MIN with static max,
// and way worse than MIN with pred*2, so not worth it

#define COUNT_PRED_INIT(val)			((val)*4 + 2)
#define COUNT_PRED_GET(state)			((state)>>2)
//#define COUNT_PRED_UPDATE(pred,state,val) ((state)-(pred)+val) // meh
//#define COUNT_PRED_UPDATE(pred,state,val) ((state)-(pred)+RR_MIN(val,8)) // better
#define COUNT_PRED_UPDATE(pred,state,val) ((state)-(pred)+RR_MIN(val,(pred)*2)) // best by quite a margin

static void RADFORCEINLINE newlz_tans_PackCounts_CountDelta(S32 L_bits,
						const U32 * counts, S32 alphabet,
						S32 num_non_zero, rrVarBits_FuncArgs(vbl) )
{
	// use PackCounts4 for tiny alphabets
	RR_ASSERT( num_non_zero > NUM_NON_ZERO_PC4 );

	// alphabet should be reduced :
	RR_ASSERT( counts[alphabet-1] != 0 );
	RR_ASSERT( alphabet == 256 || counts[alphabet] == 0 );

	//int max_pred = (1<<L_bits)/2 - 1;
	// pred must be <= max_pred
	// I think that is guaranteed because pred = recents/4
	// -> small subtlety with COUNT_PRED_INIT (you can't make init value too big)
	// I think the worst case if you have two counts like 
	//	L/2
	//  L/2 -1
	// then the next one is 1

    // make histo for deltas :  
    #define HISTO_DELTA_COUNT	128 
    U32 histo_deltas[HISTO_DELTA_COUNT] = { 0 };
    U32 deltas_over_histo_size[32]; // limited by (2048/HISTO_DELTA_COUNT) = 16 currently
    int num_deltas_over_histo_size = 0;
	U32 deltas[256];
	int num_deltas = 0;

	// Runs: zeros, nonzeros, zeros, nonzeros...
	t_alphabet_runlen_type runLens[1 + 256];
	int sym = 0;
	int gotNumSyms = 0;
	int runCount = 1;

	while (sym < alphabet && counts[sym] == 0)
		sym++;

	RR_ASSERT(sym < alphabet);
	runLens[0] = ( sym );
    
    int predState = COUNT_PRED_INIT(1);
	while (sym < alphabet)
	{
		// nonzero run
		RR_ASSERT(counts[sym] != 0);

		int runBase = sym;
		while (sym < alphabet && counts[sym] != 0)
		{
			int c = counts[sym];
			c--; // make 0 valid

			int pred = COUNT_PRED_GET(predState);
			U32 delta = rrFoldDeltaClamped(c,pred);
			deltas[num_deltas++] = delta;
			
			if ( delta < HISTO_DELTA_COUNT )
			{
				histo_deltas[delta] ++;
			}
			else
			{
				RR_ASSERT( num_deltas_over_histo_size < (int)RR_ARRAY_SIZE(deltas_over_histo_size) );

				deltas_over_histo_size[num_deltas_over_histo_size++] = delta;
			}

			predState = COUNT_PRED_UPDATE(pred,predState,c);

			gotNumSyms++;
			sym++;
		}

		runLens[runCount++] = ( sym - runBase );

		// zero run
		runBase = sym;
		while (sym < alphabet && counts[sym] == 0)
			sym++;
		runLens[runCount++] = ( sym - runBase );
	}

	// Make final zero run go all the way to 256
	RR_ASSERT(gotNumSyms == num_non_zero);
	RR_ASSERT(gotNumSyms >= 2 && gotNumSyms <= 256);
	RR_ASSERT(runCount >= 3);
	RR_ASSERT((runCount & 1) == 1);
	RR_ASSERT(sym <= 256);
	runLens[runCount - 1] = ( runLens[runCount - 1] + 256 - sym );

	rrVarBits_Output(vbl);
		
    int expGolombK = 1;

    {
        S32 bestH = RR_S32_MAX;
        for(int k=0;k<8;k++)
        {
            S32 H = EntropyOfCountsExpGolomb(histo_deltas,HISTO_DELTA_COUNT,k);
            // and add the ones in deltas_over_histo_size
            for(int i=0;i<num_deltas_over_histo_size;i++)
            {
				U32 delta = deltas_over_histo_size[i];
				H += rrVarBits_CountBits_ExpGolomb(delta,k);
            }
            
            //rrprintf("k=%d , H=%d\n",k,H);
            
            if ( H < bestH )
            {
                bestH = H;
                expGolombK = k;
            }
        }
        
        // 3 bit k :
        rrVarBits_Put(vbl,expGolombK,3);
    }

	// Determine Exp-Golomb run lengths for alphabet shape
	U8 runlen_top[256], runlen_bot[256], runlen_bot_nbits[256];
	int num_eg = newLZ_encode_alphabet_shape_runlens_split(runlen_top, runlen_bot, runlen_bot_nbits, gotNumSyms, runLens, runCount);

	rrVarBits_Put(vbl, gotNumSyms-1, 8);
	rrVarBits_Output(vbl);

	rrVarBits vb;
	rrVarBits_Copy(vb.m, vbl);
	newLZ_encode_alphabet_shape_num_EG(&vb, num_eg, gotNumSyms);

	// Determine the unary prefix for the count delta EG codes
	U8 delta_top[256];
	U32 eg_bias = 1u << expGolombK;
	for (int i = 0; i < num_deltas; i++)
	{
		U32 val = deltas[i] + eg_bias;
		U32 prefix = 31 - rrClz32(val >> expGolombK);
		U32 num_bottom = prefix + expGolombK;
		RR_ASSERT(num_bottom <= 15);

		delta_top[i] = static_cast<U8>(prefix);
		deltas[i] = val & ((1u << num_bottom) - 1); // write back the bottom bits
	}
    
	// Send all the unary prefixes of the Exp-Golomb codes
	newLZ_encode_unary_block(&vb, delta_top, num_deltas);
	newLZ_encode_unary_block(&vb, runlen_top, num_eg);

	// Send the run len EG bottom bits
	newLZ_encode_variable_U8_block(&vb, runlen_bot, runlen_bot_nbits, num_eg);

	rrVarBits_Copy(vbl, vb.m);

	// Send the delta EG bottom bits
	for (int i = 0; i < num_deltas; i++)
	{
		U32 nbits = delta_top[i] + expGolombK;
		if (!nbits)
			continue;

		rrVarBits_Output(vbl);
		rrVarBits_Put(vbl, deltas[i], nbits);
	}

	rrVarBits_Output(vbl);
}

// must be OOINLINE for rrVarBits_FuncArgs
static RADFORCEINLINE
bool newlz_tans_UnPackCounts_CountDelta(S32 L_bits,
						newlz_tans_UnpackedCounts * counts,
						rrVarBits_FuncArgs(vbl) )
{
	rrVarBits_Temps();
	//rrVarBits_Refill_Safe(vbl);
	
    const int expGolombK = (int) rrVarBits_Get_C(vbl,3); // 3b read

	U32 gotNumSyms = (U32)rrVarBits_Get_C(vbl, 8) + 1; // 11b read
	if (gotNumSyms < 2)
		return false;

	U32 numEG = newLZ_decode_alphabet_shape_num_EG(gotNumSyms, rrVarBits_PassArgs(vbl)); // <=19b read

	// Read the unary codes all at once, as raw bytes
	BlockBitReader bbr;
	rrVarBits_To_BlockBitReader(vbl, &bbr);

	U32 numUnary = gotNumSyms + numEG;
	U8 unary[256/*numSymbols*/ + 255/*max numEG*/ + 16/*slop to simplify edge cases*/];
	if (newLZ_decode_unary_block(unary, numUnary, &bbr) != (SINTa)numUnary)
		return false;

	RR_ASSERT( bbr.ptr <= bbr.end ); // it returned false if this was not true
	
	// Initialize our padding slots past the end to 0
	// this is important for decode_alphabet_shape_runlens to work right
	RR_PUT64_NATIVE_UNALIGNED(unary + numUnary + 0, 0);
	RR_PUT64_NATIVE_UNALIGNED(unary + numUnary + 8, 0);

	// Decode the rest of the run lens
	rrVarBits vb;
	rrVarBits_From_BlockBitReader(vb.m, &bbr);

	// note vb.m_cur is advanced past the current bitbuf
	//	it is farther ahead than the current bitstream endpoint we have read
	RR_ASSERT_IF_NOT_CORRUPT( vb.m_cur <= vbl_end );

	U16 runLens[128*2 + 1 + 8];
	int numRunPairs = newLZ_decode_alphabet_shape_runlens(runLens, gotNumSyms, numEG, unary + gotNumSyms, &vb);
	if (numRunPairs < 0)
		return false;

	RR_ASSERT_IF_NOT_CORRUPT( vb.m_cur <= vbl_end );
	
	rrVarBits_Copy(vbl, vb.m);
	rrVarBits_Refill_Safe(vbl);

	RR_ASSERT_IF_NOT_CORRUPT( rrVarBits_GetEndPtr(vbl) <= vbl_end );
	
	U32 count_sum = 0;
	U32 L = 1U<<L_bits;
	int max_sym = 0;
    int predState = COUNT_PRED_INIT(1);

	U8 * cur_single = counts->singles;
	U32 * cur_larger = counts->larger;
	U32 golombBias = 1u << expGolombK;
	
	// Process runs
	// NOTE these are pre-validated by decode_alphabet_shape_runlens,
	// so no need for range checks in here
	//  sum of all runlens is guaranteed to be == gotNumSyms
	const U8 * cur_unary = unary;
	for (SINTa pair = 0; pair < numRunPairs; pair++)
	{
		U32 i = runLens[pair*2 + 0];
		U32 runLen = runLens[pair*2 + 1];

		do
		{
			rrVarBits_Refill_Safe(vbl);
		
			int pred = COUNT_PRED_GET(predState);

			// We already read unary prefix using unary block coder, now read extra bits
			U32 nextra = *(cur_unary++) + expGolombK; // 8b value + 3b value = at most 9b (so can't overflow)
			REQUIRE_FUZZ_RETURN( nextra <= 15, false ); // 15 is plenty, for L_bits <= 12 we shouldn't actually need >13 (definitely not >14)
			U32 delta = (1u << nextra) - golombBias; // nextra >= expGolombK and no overflow  => this is non-negative (no overflow)
			delta += (U32)rrVarBits_Get_0Ok(vbl,nextra); // fits in 16 bits, always non-negative

			S32 count = rrUnfoldDeltaClamped(delta,pred); // fits in 16 bits, always non-negative
            		
			// don't bother checking count valid range here,
			//	we'll get it from the sum at the end
		
			predState = COUNT_PRED_UPDATE(pred,predState,count);
               
            count++;

			// count should always be nonzero: certainly impossible for well-formed streams, and the above comments
			// show the argument for all other cases; the only actual tricky cases are caught by the nextra <= 15 check above.
			RR_ASSERT( count > 0 );

			// Emit counts (branchlessly)
			*cur_single = static_cast<U8>( i );
			*cur_larger = (i << 16) + count;
			// increment one or the other :
			//	(for fuzz, these do not increment when count = 0)
			cur_single += (count == 1);
			cur_larger += (count >= 2);

            count_sum += count;

			i++;
		} while (--runLen);

		max_sym = (int)i;
	}

	// refill can take us past ; will be fuzz checked outside this call :
	RR_ASSERT_IF_NOT_CORRUPT( rrPtrDiff(vbl_end - rrVarBits_GetEndPtr(vbl)) >= 0 );
	
	counts->num_singles = rrPtrDiff32( cur_single - counts->singles );
	counts->num_larger = rrPtrDiff32( cur_larger - counts->larger );
	
	// verify it's normalized
	REQUIRE_FUZZ_RETURN( count_sum == L, false );

	return true;
}

//=====================================================================================

void newlz_tans_PackCounts(rrVarBits * vb,
						S32 L_bits,
						const U32 * counts, S32 alphabet,
						S32 num_non_zero)
{
	rrVarBits_Locals(vbl);
    rrVarBits_Copy(vbl,vb->m);
    rrVarBits_Output(vbl);

	// newlz TANS doesn't (need to) handle the degenerate cases
	RR_ASSERT( num_non_zero >= 2 );
    
	// check that counts are properly normalized
	RR_ASSERT( sum(counts,counts+alphabet) == (1U<<L_bits) );    
	
	//rrprintf("newlz_tans_PackCounts num_non_zero = %d\n",num_non_zero);
	
    if ( num_non_zero <= NUM_NON_ZERO_PC4 )
    {
        rrVarBits_Puta0(vbl);
        newlz_tans_PackCounts4(L_bits,counts,alphabet,num_non_zero, rrVarBits_PassArgs(vbl));
    }
    else
    {
        rrVarBits_Puta1(vbl);
	    newlz_tans_PackCounts_CountDelta(L_bits,counts,alphabet,num_non_zero,rrVarBits_PassArgs(vbl));
    }
    
	rrVarBits_Output(vbl);
    rrVarBits_Copy(vb->m,vbl);
}

bool newlz_tans_UnPackCounts(rrVarBits * vb,
						S32 L_bits,
						newlz_tans_UnpackedCounts * counts)
{
	rrVarBits_Temps();
	rrVarBits_Locals(vbl);
    rrVarBits_Copy(vbl,vb->m);
    rrVarBits_Refill_Safe(vbl);
        
	bool ret = false;
    if ( rrVarBits_Get1(vbl) )
    {
	    ret = newlz_tans_UnPackCounts_CountDelta(L_bits,counts, rrVarBits_PassArgs(vbl));
    }
    else
    {
        ret = newlz_tans_UnPackCounts4(L_bits,counts, rrVarBits_PassArgs(vbl));
    }
    
    if ( ! ret )
		return false;
    
    rrVarBits_Copy(vb->m,vbl);
                
    return true;
}

//===================================================================================

OODLE_NS_END

