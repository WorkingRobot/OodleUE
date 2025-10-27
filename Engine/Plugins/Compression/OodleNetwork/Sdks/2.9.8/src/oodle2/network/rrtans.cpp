// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "oodlebase.h"
#include "rrvarbits.h"
#include "rrtans.h"
#include "rrtans.inl"
#include "rrmath.h"
#include "rrmem.h"
#include "cbradutil.h"
#include "templates/rrstl.h"

#define RRVBC_INLINE
#include "rrvarbitcodes.h"
#include "rrvarbitcodes.cpp"
#include "rrcompressutil.h"
#include "rrstackarray.h"

//=====================================================

OODLE_NS_START

#define RR_TANS_DO_CHECK_CORRUPTION 1

/*

	// current construction requires L a power of 2 :
	//  in general that is not required
	
*/

struct rrTANS_Encoder;

struct rrTANS_Decoder;

SINTa rrTANS_Encoder_Size( S32 L_bits, S32 max_alphabet )
{
	rrTANS_Encoder * enc = (rrTANS_Encoder *) 0;
	RR_UNUSED_VARIABLE(enc);

	S32 L = 1<<L_bits;
	
	SINTa size = sizeof(rrTANS_Encoder);
	size = rrAlignUpA(size,16);
	size += L * sizeof(enc->encode_table_packed[0]);
	size = rrAlignUpA(size,16);
	size += (max_alphabet) * sizeof(enc->encode_sym_table[0]);
	size = rrAlignUpA(size,16);
	
	return size;
}

SINTa rrTANS_Decoder_Size( S32 L_bits, S32 max_alphabet )
{
	S32 L = 1<<L_bits;
	bool isU16 = max_alphabet > 256;

	SINTa size = sizeof(rrTANS_Decoder);
	size = rrAlignUpA(size,16);
	
	if( isU16 )
		size += L * sizeof(tans_decode_entry_U16);
	else
		size += L * sizeof(rrtans_decode_entry_U8);
	
	size = rrAlignUpA(size,16);
	
	return size;
}

rrTANS_Encoder * rrTANS_Encoder_Init( void * memory, SINTa memSize, S32 L_bits, S32 max_alphabet )
{
	if ( memSize < rrTANS_Encoder_Size(L_bits,max_alphabet) )
		return NULL;
	
	rrTANS_Encoder * enc = (rrTANS_Encoder *) memory;
	U8 * end = U8_void(memory) + sizeof(*enc);
	end = rrAlignUpPointer(end,16);
	
	enc->alphabet = max_alphabet;
	enc->max_alphabet = max_alphabet;
	S32 L = 1<<L_bits;
	enc->L = L;
	enc->L_bits = L_bits;
	
	end = rrAlignUpPointer(end,16);
	
	enc->encode_sym_table = (rrtans_encode_entry *) end;
	end += (max_alphabet) * sizeof(enc->encode_sym_table[0]);
	
	end = rrAlignUpPointer(end,16);
	
	enc->encode_table_packed = (U16 *) end;
	end += L * sizeof(enc->encode_table_packed[0]);
	
	RR_ASSERT( rrPtrDiff( end - U8_void(memory) ) <= rrTANS_Encoder_Size(L_bits,max_alphabet) );

	return enc;	
}

rrbool rrTANS_Decoder_IsU16( const rrTANS_Decoder * dec )
{
	rrbool ret = ( dec->max_alphabet > 256 );
	return ret;
}

rrTANS_Decoder * rrTANS_Decoder_Init( void * memory, SINTa memSize, S32 L_bits, S32 max_alphabet )
{
	if ( memSize < rrTANS_Decoder_Size(L_bits,max_alphabet) )
		return NULL;
	
	rrTANS_Decoder * dec = (rrTANS_Decoder *) memory;
	U8 * end = U8_void(memory) + sizeof(rrTANS_Decoder);
	end = rrAlignUpPointer(end,16);
	
	dec->alphabet = max_alphabet;
	dec->max_alphabet = max_alphabet;
	S32 L = 1<<L_bits;
	dec->L = L;
	dec->L_bits = L_bits;
	
	bool isU16 = ( max_alphabet > 256 );
	if( isU16 )
	{
		dec->decode_table_U16 = (tans_decode_entry_U16 *) end;
		dec->decode_table_U8 = NULL;
		end += L * sizeof(dec->decode_table_U16[0]);
	}
	else
	{
		dec->decode_table_U8 = (rrtans_decode_entry_U8 *) end;
		dec->decode_table_U16 = NULL;
		end += L * sizeof(dec->decode_table_U8[0]);
	}
	
	RR_ASSERT( rrPtrDiff( end - U8_void(memory) ) <= rrTANS_Decoder_Size(L_bits,max_alphabet) );

	return dec;	
}

//===================================

#define bsr(val)	(rrGetBitLevel_V(val)-1)

//===================================

// radix rank is a fixed point with radix_fpbits bits of precision

/*

// more precise radix
// 1791466.75
#define RADIX_BITS(L) L 
enum { radix_fpbits = 0 };

/**/

//*

// faster
// 1791581.13
#define RADIX_BITS(L) 8
enum { radix_fpbits = 8 };

/**/

//===================================

void rrTANS_Encoder_Fill( rrTANS_Encoder * tables,
	const U32 * normalized_counts, int alphabet )
{
	RR_ASSERT_ALWAYS( alphabet <= tables->max_alphabet );
	RR_ASSERT( sum(normalized_counts,normalized_counts+alphabet) == (U32)tables->L );	

	//===================================================

	while( alphabet > 0 && normalized_counts[alphabet-1] == 0 )
		alphabet--;

	RR_ASSERT_ALWAYS( alphabet > 0 );
	RR_ASSERT( sum(normalized_counts,normalized_counts+alphabet) == (U32)tables->L );	

	tables->alphabet = alphabet;
	
	int L = tables->L;
	int L_bits = tables->L_bits;
	
	if ( normalized_counts[alphabet-1] == (U32) L )
	{
		// degenerate; only one symbol
		
		int sym = alphabet-1;
		
		rrtans_encode_entry & ee = tables->encode_sym_table[sym];
		
		ee.max_state_numbits = check_value_cast<U8>(0);	
		ee.max_state_thresh = check_value_cast<U16>( 2*L );
				
		ee.packed_table_ptr = tables->encode_table_packed - L;
		
		// fill all states to be nops :
		for(int state=L;state<2*L;state++)
		{
			ee.packed_table_ptr[state] = check_value_cast<U16>( state );
		}
				
		return;
	}
	
	//===================================================
	// radix sort
		
	int radix_bits = RADIX_BITS(L_bits);
	int radix_size = 1<<radix_bits;
	
	U32 radix_numer = ((radix_size-1)<<radix_fpbits);	
	//U32 radix_numer = (radix_size<<radix_fpbits)-1;	

	RR_STACK_ARRAY(radix_histo,U32,radix_size);
	memset(radix_histo,0,sizeof(U32)*radix_size);

	// count ranks to histo :
	for LOOP(sym,alphabet)
	{
		U32 count = normalized_counts[sym];
		if ( count == 0 ) continue;
		
		U32 invp = radix_numer / count;
		RR_ASSERT_ALWAYS( invp > 0 || count == (U32)L );

		U32 rank = invp;
		
		for LOOP(c,(int)count)
		{			
			radix_histo[(rank>>radix_fpbits)] ++;
			rank += invp;
		}
	}
	
	// sum radix histo to cumulative prob :
	int radix_cum = 0;
	for LOOP(r,radix_size)
	{
		U32 cur = radix_histo[r];
		radix_histo[r] = radix_cum;
		radix_cum += cur;
	}
	
	RR_ASSERT_ALWAYS( radix_cum == L );
	
	// radix_histo[r] now tells you where that radix goes in the array
	
	// read out of radix and build encode table at the same time :
	SINTa cumulative_count = 0;
	for LOOP(sym,alphabet)
	{
		U32 count = normalized_counts[sym];
		if ( count == 0 )
		{
			tables->encode_sym_table[sym].packed_table_ptr = NULL;
		}
		else if ( count == 1 ) // helps speed a tiny bit to special case 1
		{
			rrtans_encode_entry & ee = tables->encode_sym_table[sym];
			
			ee.max_state_numbits = check_value_cast<U8>(L_bits);	
			ee.max_state_thresh = check_value_cast<U16>( 2*L );
					
			ee.packed_table_ptr = tables->encode_table_packed + (cumulative_count - 1);
			
			cumulative_count += 1;
		
			U32 index = radix_numer>>radix_fpbits;
			
			U32 to = radix_histo[index];
			radix_histo[index] ++;
			
			int to_state = L + to;	
			
			//int from_state = count + c;
			//packed_table_ptr[ from_state ] = check_value_cast<U16>( to_state );

			ee.packed_table_ptr[1] = check_value_cast<U16>( to_state );
		}
		else
		{		
			U32 num_bits = L_bits - rrIlog2ceil(count);
			RR_ASSERT( num_bits == rrIlog2floor(L/count) );
			RR_ASSERT( num_bits == L_bits - bsr(2*count-1) );
			
			rrtans_encode_entry & ee = tables->encode_sym_table[sym];
			
			ee.max_state_numbits = check_value_cast<U8>(num_bits);	
			ee.max_state_thresh = check_value_cast<U16>( (count+count)<<num_bits );
					
			// + cumulative_count to pack us in the array
			// - nc because that is the lowest state that will index this array
			//tables->encode_sym_table[a].packed_table_offset = check_value_cast<int16>(cumulative_count - nc);
			ee.packed_table_ptr = tables->encode_table_packed + (cumulative_count - count);
			
			cumulative_count += count;
		
			U32 invp = radix_numer / count;
			U32 rank = invp;
			
			U16 * packed_table_ptr = ee.packed_table_ptr;
			packed_table_ptr += count;
			
			for LOOP(c,(int)count)
			{
				U32 index = rank>>radix_fpbits;
				rank += invp;
				
				U32 to = radix_histo[index];
				radix_histo[index] ++;
				
				int to_state = L + to;	
				
				//int from_state = count + c;
				//packed_table_ptr[ from_state ] = check_value_cast<U16>( to_state );

				*packed_table_ptr++ = check_value_cast<U16>( to_state );
			}
		}
	}
	
	RR_ASSERT_ALWAYS( cumulative_count == (SINTa)L );
}

void rrTANS_Decoder_Fill( rrTANS_Decoder * tables,
	const U32 * normalized_counts, int alphabet )
{
	RR_ASSERT_ALWAYS( alphabet <= tables->max_alphabet );
	RR_ASSERT( sum(normalized_counts,normalized_counts+alphabet) == (U32)tables->L );	

	//===================================================

	while( alphabet > 0 && normalized_counts[alphabet-1] == 0 )
		alphabet--;

	RR_ASSERT_ALWAYS( alphabet > 0 );

	tables->alphabet = alphabet;
	
	bool isU16 = tables->max_alphabet > 256;
	
	int L = tables->L;
	int L_bits = tables->L_bits;
	
	if ( normalized_counts[alphabet-1] == (U32) L )
	{
		// degenerate; only one symbol
		// make fake table with state->state
		
		int sym = alphabet-1;
		
		if ( isU16 )
		{
			U16 sym16 = check_value_cast<U16>( sym );
			
			tans_decode_entry_U16 * detable = tables->decode_table_U16;
			RR_ASSERT_ALWAYS( detable != NULL );
			
			for LOOP(i,L)
			{
				detable[ 0 ].sym = sym16;
				detable[ 0 ].num_bits = 0;
				detable[ 0 ].next_state = check_value_cast<U16>( i+L );
				detable++;
			}
		}
		else
		{
			U8 sym8 = check_value_cast<U8>( sym );
			
			rrtans_decode_entry_U8 * detable = tables->decode_table_U8;
			RR_ASSERT_ALWAYS( detable != NULL );
			
			for LOOP(i,L)
			{
				detable[ 0 ].sym = sym8;
				detable[ 0 ].num_bits = 0;
				detable[ 0 ].next_state = check_value_cast<U16>( i+L );
				detable++;
			}
		}
		
		return;
	}
	
	//=====================================
	
	int radix_bits = RADIX_BITS(L_bits);
	int radix_size = 1<<radix_bits;
	
	U32 radix_numer = ((radix_size-1)<<radix_fpbits);	
	//U32 radix_numer = (radix_size<<radix_fpbits)-1;	

	RR_STACK_ARRAY(radix_histo,U32,radix_size);
	memset(radix_histo,0,sizeof(U32)*radix_size);

	// count ranks to histo :
	for LOOP(sym,alphabet)
	{
		U32 count = normalized_counts[sym];
		if ( count == 0 ) continue;
		
		U32 invp = radix_numer / count;
		RR_ASSERT_ALWAYS( invp > 0 || count == (U32)L );

		U32 rank = invp;
		
		for LOOP(c,(int)count)
		{			
			radix_histo[(rank>>radix_fpbits)] ++;
			rank += invp;
		}
	}
	
	// sum radix histo to cumulative prob :
	int radix_cum = 0;
	for LOOP(r,radix_size)
	{
		U32 cur = radix_histo[r];
		radix_histo[r] = radix_cum;
		radix_cum += cur;
	}
	
	RR_ASSERT_ALWAYS( radix_cum == L );
	
	// radix_histo[r] now tells you where that radix goes in the array

	// NOTEZ : decode table loop is duped for U16 and U8 tables :
	if ( isU16 )
	{
		tans_decode_entry_U16 * detable = tables->decode_table_U16;
		RR_ASSERT_ALWAYS( detable != NULL );
		
		// count ranks to histo :
		for LOOP(sym,alphabet)
		{
			U32 count = normalized_counts[sym];
			if ( count == 0 )
			{
				// nop
			}
			else if ( count == 1 ) // helps speed a little to special case 1
			{
				U32 rank = radix_numer;
				
				U32 index = rank>>radix_fpbits;
				U32 to = radix_histo[index];
				radix_histo[index] ++;
				
				//int from_state = 1;
				
				// decoder numbits :
				RR_ASSERT( bsr(1) == 0 );
				//int num_bits = L_bits - bsr(from_state);
				
				//RR_ASSERT( detable[ to ].next_state == 0 );
				detable[ to ].next_state = check_value_cast<U16>( L );
				detable[ to ].num_bits = check_value_cast<U8>( L_bits );
				detable[ to ].sym = check_value_cast<U16>( sym );
			}
			else
			{		
				U32 invp = radix_numer / count;
				U32 rank = invp;
						
				// simpler, faster !
				
				for LOOP(c,(int)count)
				{
					U32 index = rank>>radix_fpbits;
					U32 to = radix_histo[index];
					radix_histo[index] ++;
					
					int from_state = count+c;
					
					// decoder numbits :
					int num_bits = L_bits - bsr(from_state);
					
					//RR_ASSERT( detable[ to ].next_state == 0 );
					detable[ to ].sym = check_value_cast<U16>( sym );
					detable[ to ].num_bits = check_value_cast<U8>( num_bits );
					detable[ to ].next_state = check_value_cast<U16>( from_state << num_bits );
				
					rank += invp;
				}
			}
		}
	}
	else // U8 loop
	{
		rrtans_decode_entry_U8 * detable = tables->decode_table_U8;
		RR_ASSERT_ALWAYS( detable != NULL );
	
		// count ranks to histo :
		for LOOP(sym,alphabet)
		{
			U32 count = normalized_counts[sym];
			if ( count == 0 )
			{
				// nop
			}
			else if ( count == 1 ) // helps speed a little to special case 1
			{
				U32 rank = radix_numer;
				
				U32 index = rank>>radix_fpbits;
				U32 to = radix_histo[index];
				radix_histo[index] ++;
				
				//int from_state = 1;
				
				// decoder numbits :
				RR_ASSERT( bsr(1) == 0 );
				//int num_bits = L_bits - bsr(from_state);
				
				//RR_ASSERT( detable[ to ].next_state == 0 );
				detable[ to ].next_state = check_value_cast<U16>( L );
				detable[ to ].num_bits = check_value_cast<U8>( L_bits );
				detable[ to ].sym = check_value_cast<U8>( sym );
			}
			else
			{		
				U32 invp = radix_numer / count;
				U32 rank = invp;
						
				// simpler, faster !
				
				for LOOP(c,(int)count)
				{
					U32 index = rank>>radix_fpbits;
					U32 to = radix_histo[index];
					radix_histo[index] ++;
					
					int from_state = count+c;
					
					// decoder numbits :
					int num_bits = L_bits - bsr(from_state);
					
					//RR_ASSERT( detable[ to ].next_state == 0 );
					detable[ to ].sym = check_value_cast<U8>( sym );
					detable[ to ].num_bits = check_value_cast<U8>( num_bits );
					detable[ to ].next_state = check_value_cast<U16>( from_state << num_bits );
				
					rank += invp;
				}
			}
		}
	} // U8 or U16
}

//===================================================================================

/**

03/18/2014 todos

rrTANS_PackCounts4
	instead of the max_delta_bits
	probably better to just send delta using ExpGolomb
	with r = L_bits/2
	or something like that

rrTANS_PackCounts8
	1. currently not using the known sum to L
		should be able to skip sending the mps's count
		or something like that
	2. codeLen is actually known to be in [1,L_bits]
		I'm not using that when I do the pred delta and foldupnegatives
		eg. if pred is 1 then no negative delta is possible


**/


/**

PackCodeLens4

intended for when very few symbols have non-zero counts

sends a set of [sym:count] pairs

sorts by increasing count
then can send them as delta from previous
and the last count is implicit from the sum to L

**/

#define NUM_NON_ZERO_PC4	7

static const int c_num_non_zero_bits = rrGetBitLevel_C(NUM_NON_ZERO_PC4);
	
void RADFORCEINLINE rrTANS_PackCounts4(S32 L_bits, S32 max_alphabet,
						const U32 * counts, S32 alphabet,
						S32 num_non_zero, rrVarBits_FuncArgs(vbl) ,
						bool normalized)
{
	RR_ASSERT( num_non_zero <= NUM_NON_ZERO_PC4 );
	
    int log2maxalphabet = rrIlog2ceil(max_alphabet);

	U32 L = 1U<<L_bits;

	rrVarBits_Output(vbl);
    rrVarBits_Put(vbl,num_non_zero,c_num_non_zero_bits);
    
    if ( num_non_zero == 0 )
    {
        return;
    }
    else if ( num_non_zero == 1 )
    {
		rrVarBits_Output(vbl);
		RR_ASSERT_ALWAYS( counts[alphabet-1] != 0 );
        rrVarBits_Put(vbl,(alphabet-1),log2maxalphabet);
	
		if ( normalized )
		{
			RR_ASSERT_ALWAYS( counts[alphabet-1] == L );
		}
		else
		{
	        rrVarBits_Put(vbl,counts[alphabet-1],L_bits);
		}
        return;
    }
    
    // alphabet should be reduced :
	RR_ASSERT_ALWAYS( counts[alphabet-1] != 0 );
	
	// find the non-zero count symbols :
    U32 got_counts[NUM_NON_ZERO_PC4];
    S32 got_syms[NUM_NON_ZERO_PC4];
    int got = 0;
    
    for(int sym=0;sym<alphabet;sym++)
    {
		if ( counts[sym] == 0 )
			continue;
		
		got_counts[got] = counts[sym];
		got_syms[got] = sym;
		got++;
    }
    
    RR_ASSERT_ALWAYS( got == num_non_zero );

	// sort:
	for(int i=0;i<num_non_zero;i++)
	{
		for(int j=i+1;j<num_non_zero;j++)
		{
			if ( got_counts[j] < got_counts[i] )
			{
				swap(got_syms[i],got_syms[j]);
				swap(got_counts[i],got_counts[j]);
			}
		}
	}
	   
	// counts increase :
	RR_ASSERT( got_counts[1] >= got_counts[0] );
    
    S32 max_delta_bits = 0;
    
    int icount = num_non_zero;
    if ( normalized )
		icount--; // don't include last
    
    // find the maximum delta count :
    U32 prev = 0;
	for(int i=0;i<icount;i++)
	{
		U32 cur = got_counts[i];
		RR_ASSERT( cur >= prev );
		S32 delta = cur - prev;
		prev = cur;
		S32 delta_bits = rrGetBitLevel_V(delta);
		max_delta_bits = RR_MAX(max_delta_bits,delta_bits);
	}
	RR_ASSERT( max_delta_bits > 0 );
	if ( max_delta_bits == 0 ) max_delta_bits = 1;
	RR_ASSERT_ALWAYS( max_delta_bits <= L_bits );
    
    // max_delta_bits_bits is usually 4
    U32 max_delta_bits_bits = rrGetBitLevel_V(L_bits);
    rrVarBits_Put(vbl,max_delta_bits,max_delta_bits_bits);
    
    // send the deltas :
	prev = 0;
	for(int i=0;i<icount;i++)
	{
		U32 cur = got_counts[i];
		RR_ASSERT( cur >= prev );
		U32 delta = cur - prev;
		prev = cur;
		
		rrVarBits_Output(vbl);
        rrVarBits_Put(vbl,got_syms[i],log2maxalphabet);
        rrVarBits_Put(vbl,delta,max_delta_bits);
	}
	
	rrVarBits_Output(vbl);
		
    if ( normalized )
    {
		// put last sym id
		rrVarBits_Put(vbl,got_syms[(num_non_zero-1)],log2maxalphabet);
		// don't put last count, it's implicit
	}
}

rrbool RADFORCEINLINE rrTANS_UnPackCounts4(S32 L_bits, S32 max_alphabet,
						U32 * counts,
						rrVarBits_FuncArgs(vbl) ,
						bool normalized)
{
	//zero counts :
	memset(counts,0,sizeof(U32)*max_alphabet);
	
    int log2maxalphabet = rrIlog2ceil(max_alphabet);

	U32 L = 1U<<L_bits;

	rrVarBits_Temps();
	rrVarBits_Refill_Safe(vbl);
    S32 num_non_zero = (S32) rrVarBits_Get_C(vbl,c_num_non_zero_bits);
        
    if ( num_non_zero == 0 )
    {
        return true;
    }
    else if ( num_non_zero == 1 )
    {
		rrVarBits_Refill_Safe(vbl);
		
		RR_VARBITSTYPE sym = rrVarBits_Get_V(vbl,log2maxalphabet);
		
        if ( sym >= (RR_VARBITSTYPE)max_alphabet )
        {
            ooLogError(" sym >= (RR_VARBITSTYPE)max_alphabet\n");
            return false;
        }
        
		if ( normalized )
		{
	        counts[sym] = L;
		}
		else
		{
	        counts[sym] = (U32) rrVarBits_Get_V(vbl,L_bits);
		}
		
        return true;
    }
    
    // max_delta_bits_bits is usually 4
    U32 max_delta_bits_bits = rrGetBitLevel_V(L_bits);
    S32 max_delta_bits = (S32) rrVarBits_Get_V(vbl,max_delta_bits_bits);
    RR_ASSERT( max_delta_bits > 0 );
    if ( max_delta_bits > L_bits )
    {
        ooLogError("max_delta_bits > L_bits\n");
        return false;
    }
    
    U32 prev = 0;
    U32 sum = 0;
    for(int i=0;i<(num_non_zero-1);i++) 
    {
	    rrVarBits_Refill_Safe(vbl);

        int sym = (int) rrVarBits_Get_V(vbl,log2maxalphabet);
        
        U32 delta = (U32) rrVarBits_Get_V(vbl,max_delta_bits);
        
        U32 cur = prev + delta;
        prev = cur;

		if ( cur >= L )
		{
	       ooLogError("cur >= L\n");
		   return false;
		}

		counts[sym] = cur;
		sum += cur;
    }
    
	
    // get last :
    rrVarBits_Refill_Safe(vbl);
    int mps = (int) rrVarBits_Get_V(vbl,log2maxalphabet);
    
    if ( normalized )
    {
		if ( sum >= L )
		{
		   ooLogError("sum >= L\n");
		   return false;
		}
    
		U32 last = L - sum;
	    
		if ( last < prev )
		{
		   ooLogError("last < prev\n");
		   return false;
		}
	    
		counts[mps] = last;
    }
    else
    {
        U32 delta = (U32) rrVarBits_Get_V(vbl,max_delta_bits);
        
        U32 cur = prev + delta;
		counts[mps] = cur;
    }
    
    return true;
}


// you can get a decent win from fucking around tweak these predictors
//	but it's very different for each file, so dangerous to over-tweak on one file

/*
#define PRED_INIT(log2)			(log2)*3
//#define PRED_GET(state)			(((state) + 2)>>2)
#define PRED_GET(state)			(((state) + 1)>>2)	// +1 seems to win on granny
//#define PRED_UPDATE(state,val)	(((state)*3 +2)>>2) + (val)
#define PRED_UPDATE(state,val)	(((state)*3 + 2)>>2) + (val)
/**/

#define PRED_INIT(log2)			(log2)*4
#define PRED_GET(state)			(((state) + 2)>>2)
#define PRED_UPDATE(state,val)	(((state)*3 + 2)>>2) + (val)
//#define PRED_UPDATE(state,val)	(((state)+(state)+(state) + 2)>>2) + (val)

/*
#define PRED_INIT(log2)			(log2)*2
#define PRED_GET(state)			(((state) + 1)>>1)
#define PRED_UPDATE(state,val)	(((state) +1)>>1) + (val)
/**/

#define RUNLEN_EXPGOLOMB_BITS	(1)

void RADFORCEINLINE rrTANS_PackCounts8(S32 L_bits, S32 max_alphabet,
						const U32 * counts, S32 alphabet,
						S32 num_non_zero, rrVarBits_FuncArgs(vbl) ,
						bool normalized)
{
	// use PackCounts4 for tiny alphabets
	RR_ASSERT( num_non_zero > NUM_NON_ZERO_PC4 );

    int log2maxalphabet = rrIlog2ceil(max_alphabet);

	// alphabet should be reduced :
	RR_ASSERT( counts[alphabet-1] != 0 );
	RR_ASSERT( alphabet == max_alphabet || counts[alphabet] == 0 );

	RR_STACK_ARRAY(codeLenTable,U8,alphabet);
	
	// make codeLenTable :
	for(int i=0;i<alphabet;i++)
	{
		if ( counts[i] == 0 )
		{
			codeLenTable[i] = 0;
		}
		else
		{
			// rrGetBitLevel_V is one *higher* than the position of the top bit
			codeLenTable[i] = (U8) rrGetBitLevel_V( counts[i] );
			RR_ASSERT( codeLenTable[i] > 0 );
		}
	}

    // make histo for deltas :  
    int NumDeltaFromPrevious[32] = { 0 };
    int prevCodeLen4 = PRED_INIT(log2maxalphabet);
    for(int i=0;i<alphabet;i++)
    {
		int codeLen = codeLenTable[i];
        if ( codeLen > 0 )
        {
            int pred = PRED_GET(prevCodeLen4);
            int delta = codeLen - pred;
            delta = rrFoldUpNegatives(delta);
            RR_ASSERT( delta < 32 );
            NumDeltaFromPrevious[delta] ++;
            prevCodeLen4 = PRED_UPDATE(prevCodeLen4,codeLen);
        }
    }

	rrVarBits_Output(vbl);
		
    // riceBits is usually best at 1, but on 'book1' it's much better at 2
    int riceBits = 1;

    // check if entropy is better if I bump meanCodeLen up or down
    {
        S32 bestH = RR_S32_MAX;
        for(int cur=0;cur<4;cur++)
        {
            S32 H = EntropyOfCountsRice(NumDeltaFromPrevious,32,cur);
            if ( H < bestH )
            {
                bestH = H;
                riceBits = cur;
            }
        }
        
        rrVarBits_Put(vbl,riceBits,2);
    }
    
    bool firstIsOn =  codeLenTable[0] != 0;
    rrVarBits_Put(vbl, firstIsOn?1:0 ,1);
       
    prevCodeLen4 = PRED_INIT(log2maxalphabet);
    
    int i =0;
    for(;;)
    {
        // send a run of 0 lens
        int RunLen = 0;

		if ( firstIsOn && i == 0 )
		{
			// no initial zero run
		}
		else
		{
			/*
			if ( i > HI->topSym )
			{
				rrVarBits_WriteExpGolomb( rrVarBits_PassArgs(vbl) ,0,RUNLEN_EXPGOLOMB_BITS);
				break;
			}
			*/
			while ( i < alphabet && codeLenTable[i] == 0 )
			{
				RunLen++;
				i++;
			}

			RR_ASSERT( RunLen > 0 );
			RunLen--;
			rrVarBits_WriteExpGolomb( rrVarBits_PassArgs(vbl) ,RunLen,RUNLEN_EXPGOLOMB_BITS);
		}
		
		if ( i >= alphabet )
			break;
					
        // now send a run of non-zero lens
        //int runStartI = i;
        RunLen = 0;
        while ( (i+RunLen) < alphabet && codeLenTable[i+RunLen] != 0 )
        {
            RunLen++;
        }
        
        RR_ASSERT( RunLen > 0 );
        //rrVarBits_WriteRice( rrVarBits_PassArgs(vbl),RunLen-1,RUNLEN_RICE_BITS);
        rrVarBits_WriteExpGolomb( rrVarBits_PassArgs(vbl) ,RunLen-1,RUNLEN_EXPGOLOMB_BITS);

        for(int j=0;j<RunLen;j++)
        {
            // transmit the difference from meanCodeLen
            int pred = PRED_GET(prevCodeLen4);
            int cl = codeLenTable[i];
            int send = rrFoldUpNegatives(cl - pred);
            
			rrVarBits_Output(vbl);
    
            //RR_ASSERT( send < 24 );
            rrVarBits_WriteRice( rrVarBits_PassArgs(vbl) ,send,riceBits);
            
            // send count below top bit :
            int topbit = cl-1;
            RR_ASSERT( topbit >= 0 );
            RR_ASSERT( counts[i] >= (1U<<topbit) && counts[i] < (2U<<topbit) );
            rrVarBits_Put( vbl, rrMaskInBits32_V( counts[i] , topbit), topbit );
            
            //prevCodeLen = codeLenTable[i];
            prevCodeLen4 = PRED_UPDATE(prevCodeLen4,cl);
            i++;
        }
        
        if ( i >= alphabet )
			break;
    }
    
    // send final zero run :
    if ( alphabet < max_alphabet )
    {
		int RunLen = max_alphabet - alphabet;
		RunLen--;
		rrVarBits_WriteExpGolomb( rrVarBits_PassArgs(vbl) ,RunLen,RUNLEN_EXPGOLOMB_BITS);
    }    
}

// must be OOINLINE for rrVarBits_FuncArgs
RADFORCEINLINE 
rrbool rrTANS_UnPackCounts8(S32 L_bits, S32 max_alphabet,
						U32 * counts,
						rrVarBits_FuncArgs(vbl) ,
						bool normalized)
{
	rrVarBits_Temps();
	
    const int log2maxalphabet = rrIlog2ceil(max_alphabet);

	//rrVarBits_Refill_Safe(vbl);
	
    const int riceBits = (int) rrVarBits_Get_C(vbl,2);
    
	rrVarBits_Refill_Safe(vbl);
		
    int prevCodeLen4 = PRED_INIT(log2maxalphabet);

	int i = 0;
	U32 count_sum = 0;
	U32 L = 1U<<L_bits;
	RR_UNUSED_VARIABLE(L);
	
	const int firstIsOn = (int) rrVarBits_Get1(vbl);
	if ( firstIsOn )
		goto rrTANS_UnPackCounts8_GetCodes; // ****** GOTO ************

    for(;;)
    {
		{
			// read RunLen of zeros :
			//int RunLen = rrVarBits_ReadRice(rrVarBits_PassArgs(vbl),RUNLEN_RICE_BITS);
			//int RunLen = rrVarBits_ReadExpGolomb( rrVarBits_PassArgs(vbl) ,RUNLEN_EXPGOLOMB_BITS);
			int zRunLen = 1 + rrVarBits_ReadExpGolomb_Small( rrVarBits_PassArgs(vbl) ,RUNLEN_EXPGOLOMB_BITS);
			/*
			if ( zRunLen == 0 )
			{
				break;
			}
			*/
			
			#if RR_TANS_DO_CHECK_CORRUPTION
			// use funny U32 compare like this to catch zRunLen < 0 or too big :
			if ( (U32)zRunLen > (U32)(max_alphabet - i) )
			{
				ooLogError("i + RunLen > packNumSymbols \n");
				return false;
			}
			#endif 
	        
			//RR_ASSERT_ALWAYS( i >= 0 && (i+zRunLen) <= numSymbols && zRunLen >= 0 );
			//RR_ASSERT_ALWAYS( numSymbols == HI->numSymbols && codeLenTable == HI->codeLenTable );
			memset(counts+i,0,sizeof(U32)*zRunLen);
			i += zRunLen;
	        
			if ( RAD_UNLIKELY( i >= max_alphabet ) )
			{
				break;
			}
			        
			rrVarBits_Refill_Safe(vbl);
		}
		
		// can skip the zero run reader to get here :
		rrTANS_UnPackCounts8_GetCodes: // **** GOTO *****
		
		{
			// read RunLen of non-zeros :
			//RunLen = 1 + rrVarBits_ReadRice(rrVarBits_PassArgs(vbl),RUNLEN_RICE_BITS);
			//RunLen = 1 + rrVarBits_ReadExpGolomb( rrVarBits_PassArgs(vbl) ,RUNLEN_EXPGOLOMB_BITS);
			int nzRunLen = 1 + rrVarBits_ReadExpGolomb_Small( rrVarBits_PassArgs(vbl) ,RUNLEN_EXPGOLOMB_BITS);
	        
			#if RR_TANS_DO_CHECK_CORRUPTION
			if ( (i + nzRunLen) > max_alphabet )
			{
				ooLogError("i + RunLen > packNumSymbols \n");
				return false;
			}       
			#endif
	        
			for(int j=0;j<nzRunLen;j++)
			{	                
				rrVarBits_Refill_Safe(vbl);
				
				//U32 code = rrVarBits_ReadRice(rrVarBits_PassArgs(vbl),riceBits);
				U32 code = rrVarBits_ReadRice_Small(rrVarBits_PassArgs(vbl),riceBits);
				S32 delta = rrUnfoldNegatives(code);
	                
				rrVarBits_Refill_Safe(vbl);
			
				int pred = PRED_GET(prevCodeLen4);
				S32 curCodeLen = delta + pred;
				//prevCodeLen = curCodeLen;
				#if RR_TANS_DO_CHECK_CORRUPTION
				// use U32 compare to catch curCodeLen < 0 or too big :
				if ( (U32)curCodeLen >= 32 )
				{
					ooLogError("curCodeLen < 0 || curCodeLen >= 32 ) \n");
					return false;
				}
				#endif
	            
				prevCodeLen4 = PRED_UPDATE(prevCodeLen4,curCodeLen);
	                
				RR_ASSERT( curCodeLen > 0 );
				int topbit = curCodeLen-1;
				
				// get bottom bits :
				int low = 0;
				if ( topbit > 0 )
					low = (int) rrVarBits_Get_V( vbl, topbit );
	            
	            counts[i] = (1U<<topbit) + low;	   
	            count_sum += counts[i]; 
	                    
				i++;
			}
	
			if ( RAD_UNLIKELY( i >= max_alphabet ) )
			{
				break;
			}
		}
    }

	if ( normalized )
	{    
		RR_ASSERT( count_sum == L );
		if ( count_sum != L )
		{
			ooLogError(" count_sum != L \n");
		}
	}
	    
    return true;
}

void rrTANS_PackCounts(rrVarBits * vb,
						S32 L_bits, S32 max_alphabet,
						const U32 * counts, S32 alphabet,
						S32 num_non_zero,
						bool normalized)
{
	rrVarBits_Locals(vbl);
    rrVarBits_Copy(vbl,vb->m);
    rrVarBits_Output(vbl);
    
	if ( normalized )
	{
		RR_ASSERT( alphabet != 0 );
		RR_ASSERT( sum(counts,counts+alphabet) == (1U<<L_bits) );    
	}
	else
	{
		RR_ASSERT( alphabet == 0 || array_max(counts,counts+alphabet) < (1U<<L_bits) );    
	}
	
    if ( num_non_zero <= NUM_NON_ZERO_PC4 )
    {
        // I put the oneChar support in PackCounts4 to just not have to deal with it elsewhere
    
        rrVarBits_Puta0(vbl);
        rrTANS_PackCounts4(L_bits,max_alphabet,counts,alphabet,num_non_zero, rrVarBits_PassArgs(vbl) ,normalized);
    }
    else
    {
        rrVarBits_Puta1(vbl);
	    rrTANS_PackCounts8(L_bits,max_alphabet,counts,alphabet,num_non_zero,rrVarBits_PassArgs(vbl) ,normalized);
    }
    
	rrVarBits_Output(vbl);
    rrVarBits_Copy(vb->m,vbl);
}

rrbool rrTANS_UnPackCounts(rrVarBits * vb,
						S32 L_bits, S32 max_alphabet,
						U32 * counts,
						bool normalized)
{
	rrVarBits_Temps();
	rrVarBits_Locals(vbl);
    rrVarBits_Copy(vbl,vb->m);
    rrVarBits_Refill_Safe(vbl);
        
    rrbool ret = false;
    if ( rrVarBits_Get1(vbl) )
    {
	    ret = rrTANS_UnPackCounts8(L_bits,max_alphabet,counts, rrVarBits_PassArgs(vbl) ,normalized);
    }
    else
    {
        ret = rrTANS_UnPackCounts4(L_bits,max_alphabet,counts, rrVarBits_PassArgs(vbl) ,normalized);
    }
    
    rrVarBits_Copy(vb->m,vbl);
    
    return ret;
}

//===================================================================================

OODLE_NS_END
