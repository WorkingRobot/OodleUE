// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "newlz_shared.h"
#include "newlz_speedfit.h"
#include "histogram.h"
#include "rrvarbitcodes.h"

#include "log2table.h"
#include "rrcompressutil.h"

#include "templates/rralgorithm.h"
#include "rrlogutil.h"

//#include "rrsimpleprof.h"

OODLE_NS_START

// add on a flat probability model and scale up the ones that were present
enum { count_add = 1 }; // count_add / histo_scale , so 1 means 1/histo_scale
enum { histo_scale = 4 }; // <- 4 or 8 seems to be best
	
	
// log2 fractional bit costs
//	since array might be TANS or split or RLE or whatever

void histo_to_codecost(const U32 * histo, S32 * codecost,int alphabet,int add_cost,
		int cost_entropy_threshold)
{
	U32 histo_sum = sum(histo,histo+alphabet);

	histo_sum *= histo_scale;
	histo_sum += alphabet*count_add;

	// note this is actually the negative log2
	S32 log2_sum = log2tabled_bk_32(histo_sum);

	S32 total_cost = 0;

	for(int i=0;i<alphabet;i++)
	{	
		U32 count = histo[i]*histo_scale + count_add;
		
		S32 log2_count = log2tabled_bk_32(count) - log2_sum;
				
		S32 cost = ( log2_count * COST_ONE_BIT ) >> RR_LOG2TABLE_ONE_SHIFT;
		
		total_cost += cost * count;
		
		codecost[i] = cost + add_cost;
	}
	
	#if 1 // @@ - this is important for Mermaid
	
	// histo_to_codecost high entropy fallback to flat :
	
	// this is really just for the Mermaid U16 offsets
	//	it could be turned off in all the other cases
	
	RR_ASSERT( cost_entropy_threshold <= COST_ONE_BYTE );
	RR_ASSERT( cost_entropy_threshold >= 7*COST_ONE_BIT );
	
	// total_cost is in bits (COST_ONE_BIT)
	RR_ASSERT( alphabet == 256 ); // only 256 for now
	// 7.5 bits :
	//const int cost_entropy_threshold = (75 * COST_ONE_BIT)/10;
	int threshold_bits = cost_entropy_threshold * histo_sum;
	
	if ( total_cost > threshold_bits )
	{
		for(int i=0;i<alphabet;i++)
		{	
			codecost[i] = COST_ONE_BYTE + add_cost;
		}		
	}
	#endif
}


U32 newlz_array_estimate_complen_bits(const U32 * histo,int alphabet, U32 sumCounts)
{
	RR_ASSERT( sumCounts == rrSumOfHistogram(histo,alphabet) );
		
	U32 newlz_array_header_size_bits = 5*8;
		
    if ( sumCounts <= 1 )
		return newlz_array_header_size_bits;
	
	// counts are 17 bit max in newlz
	RR_ASSERT( sumCounts <= 128*1024 );
	
	S32 invSumI = (1<<30) / sumCounts;
	
	U32 header_bits_counts = 0;
	U32 header_bits_zrl = 0;
    U64 cl_sum = 0;
    U32 zero_runlen = 0;
    int num_non_zero = 0;
    for(int i=0;i<alphabet;i++)
    {
		U32 c = histo[i];
        if ( c == 0 )
		{
			zero_runlen++;
			continue;
		}
		
		num_non_zero++;
		
		// send zero_runlen :
		if ( zero_runlen == 0 )
		{
			// common case for speed
			header_bits_zrl += 1;
		}
		else
		{
			//	(when zero_runlen is 0 this is 1 bit)
			header_bits_zrl += rrVarBits_CountBits_EliasGamma(zero_runlen);
			zero_runlen = 0;
		}
		
		// send the count c :
		//	obviously in huffman you only need to send cl , not c
		//	but, since cl here is much more accurate you are generally saving bits overall
		header_bits_counts += rrVarBits_CountBits_EliasGamma(c-1);
		
		// the EliasGamma and the log2tabled are both forms of log2(c)
		//	could merge them and just do it once, though it's a bit funny
		//	@@ I'm sort of doing the log2 of c twice
		U32 cl = log2tabled<30>( c * invSumI );
			
		//H += c * cl; // 32 bit mul - could this overflow? it's close
			// count can be 17 bits
			//	codelen starts at 13 bits (RR_LOG2TABLE_ONE_SHIFT)
			//	but when count is high, codelen is low
			//	so codelen is never large when count is large, maybe it's okay?
		cl_sum += (U64)c * cl; // 64 bit mul
    }
    
    if ( num_non_zero == 1 )
    {
		// pure singleton
		U32 bits = 8; // send the symbol id
		
		bits += newlz_array_header_size_bits;

		return bits;
    }
    
    // does final zero runlen count against us or not?
    //	(not if you transmit alphabet/topsym)
	header_bits_zrl += rrVarBits_CountBits_EliasGamma(zero_runlen);
		
	// alternate transmission (for low num_non_zero)
	// 8 bits per non-zero symbol :
	U32 alt_header_size = num_non_zero * 8;
    
    U32 header_sum = header_bits_counts + RR_MIN(alt_header_size,header_bits_zrl);
    
    // header_sum is in bits
    // cl_sum is in bits scaled up by RR_LOG2TABLE_ONE_SHIFT
    cl_sum >>= RR_LOG2TABLE_ONE_SHIFT;

    U32 bits = header_sum + (U32)cl_sum;

	bits += newlz_array_header_size_bits;

	return bits;
}


F32 newlz_array_estimate_huff_J(const U32 * histo,int alphabet,SINTa sumCounts,F32 lambda,const OodleSpeedFit * speedfit)
{
	U32 bits = newlz_array_estimate_complen_bits(histo,alphabet,(U32)sumCounts);
	// J is in bytes
	F32 J = (bits/8.f) + lambda * speedfit_estimate_entropy_array_time(speedfit,sumCounts);
	return J;
}


//=================================================
// newlz_guess_should_hash_length_be_over_four_by_hash_collisions :
//	does the data compress better with mml 4 or mml 6
//	this correlates very well to whether the data has lots of len 4 hash collisions
//	so just measure that
//
// this is attractive because it is measuring the actual property of the data
//	that makes us want to go above mml 4 (too many len 4 hash collisions)
// in practice on the actual data we have this is almost always text files
//
// this was built by measuring the actual compressed size each way
//	then turning that into a boolean class
// and then seeing what measurement of the data had good correlation to that class

// from ctmf.h :
static RADFORCEINLINE UINTa hash_4_bytes(const U8 * ptr,int bits)
{
	// can use 32-bit mul
	U32 x = RR_GET32_NATIVE(ptr) * 0x9E3779B1U;
	return (UINTa)(x >> (32 - bits));
}

static rrbool newlz_guess_should_hash_length_be_over_four_by_hash_collisions(const void * inBuf,SINTa inSize)
{
	#define test_hash_collisions_hash_bits	13
	#define test_hash_collisions_hash_size	(1<<test_hash_collisions_hash_bits)
	
	if ( inSize < test_hash_collisions_hash_size*2 )
	{
		// tiny file - hash length 4
		return false;
	}

	//SIMPLEPROFILE_SCOPE(guess_hash_length);

	U8 hash_table[test_hash_collisions_hash_size]; // @@ 8k on stack ? arena?
	
	memcpy(hash_table,inBuf,RR_MIN(test_hash_collisions_hash_size,inSize));

	inSize -= 4; // room for U32 grab

	UINTr sparse2_collisions = 0;
	UINTr sparse2_count = 0;

	SINTa sparse2_step = 256*1024; // @@ 128k ? or maybe not quite on quantum boundary?

	// dense section then a big step :
	// with de-serialized hash table (hash four then lookup four rather than interleave as you should)
	// -> yes works fine
	for(SINTa i=0; i < inSize; i += sparse2_step)
	{
		// sample 8k bytes :
		SINTa cur = RR_MIN(test_hash_collisions_hash_size,(inSize-i));
		const U8 * ptr = (const U8 *)inBuf + i;
		SINTa cur4 = cur/4;
		sparse2_count += cur4*4;
		while(cur4--)
		{
			UINTa h1 = hash_4_bytes(ptr+0,test_hash_collisions_hash_bits);
			UINTa h2 = hash_4_bytes(ptr+1,test_hash_collisions_hash_bits);
			UINTa h3 = hash_4_bytes(ptr+2,test_hash_collisions_hash_bits);
			UINTa h4 = hash_4_bytes(ptr+3,test_hash_collisions_hash_bits);
			sparse2_collisions += ( hash_table[h1] == ptr[0] );
			sparse2_collisions += ( hash_table[h2] == ptr[1] );
			sparse2_collisions += ( hash_table[h3] == ptr[2] );
			sparse2_collisions += ( hash_table[h4] == ptr[3] );
			hash_table[h1] = ptr[0];			
			hash_table[h2] = ptr[1];			
			hash_table[h3] = ptr[2];			
			hash_table[h4] = ptr[3];	
			ptr += 4;		
		}
	}
	
	F32 collision_percent = (sparse2_collisions*100.0f)/sparse2_count;
	
	// somewhere in  58-61%
	//rrprintfvar(collision_percent);
	
	rrbool ret = (collision_percent > 60.0);

	return ret;
}

//=================================================
// newlz_guess_should_hash_length_be_over_four_by_utf8_detector :
//	explicitly look for utf8 text
//	use mml 6 there

static inline bool is_cont_byte(U8 code)
{
	return code >= 0x80 && code <= 0xbf;
}

static bool all_cont_bytes(const U8 * begin, const U8 * end)
{
	const U8 * p = begin;
	while ( p < end )
	{
		if ( !is_cont_byte(*p++) )
			return false;
	}
	return true;
}

static bool might_be_utf8(const U8 * begin, const U8 * end)
{
	// skip over initial continuation bytes
	const U8 * p = begin;
	while ( p < end && is_cont_byte(*p) )
		p++;

	// if we skipped over more than 3 continuation bytes -> can't be legal
	if ( p - begin > 3 )
		return false;

	// we're now at (what should be) the start of a new character
	while ( p < end )
	{
		U8 byte = *p++;
		if ( byte >= 0x09 && byte <= 0x7e ) // printable ASCII (generous: allowing various control chars)
			continue;
		else if ( byte >= 0xc2 && byte <= 0xf4 ) // start of multi-byte sequence? (0xc2 because 0xc0 / 0xc1 are illegal over-long encodings of ASCII chars)
		{
			if ( byte <= 0xdf ) // 1 cont byte
			{
				if ( end - p < 1 ) // we're at the end!
					return true;
				else if ( !is_cont_byte(p[0]) )
					return false;
				p += 1;
			}
			else if ( byte <= 0xef ) // 2 cont bytes
			{
				if ( end - p < 2 ) // about to hit the end
					return all_cont_bytes(p, end);
				else if ( !is_cont_byte(p[0]) || !is_cont_byte(p[1]) )
					return false;
				p += 2;
			}
			else // 3 cont bytes
			{
				if ( end - p < 3 ) // about to hit the end
					return all_cont_bytes(p, end);
				else if ( !is_cont_byte(p[0]) || !is_cont_byte(p[1]) || !is_cont_byte(p[2]) )
					return false;
				p += 3;
			}
		}
		else
			return false; // anything else not permissible here
	}

	// we made it to the end, congratulations, it might be UTF-8
	return true;
}

static rrbool newlz_guess_should_hash_length_be_over_four_by_utf8_detector(const void * inBuf,SINTa inSize)
{
	const U8 * inU8 = (const U8 *)inBuf;
	int utf8_score = 0;

	// take a few samples
	SINTa nsmp = 32;
	SINTa nbytes = 32;
	SINTa step = inSize / nsmp;

	if ( step < nbytes ) return false;

	for(SINTa smp = 0; smp < nsmp; smp++)
	{
		SINTa pos = smp*step;
		utf8_score += might_be_utf8(inU8 + pos, inU8 + RR_MIN(pos + nbytes,inSize));
	}

	// 4/16 and 6/16 are the thresholds for wad7 and osdb
	//	which want to be len 6
	//	but that's sort of cheating
	// true binary vs text is somewhere in 2-8 /16

	int utf8_score_threshold = 14; // 14/32
	
	return utf8_score >= utf8_score_threshold;
}


// used by Kraken & Mermaid levels 1-3
rrbool newlz_guess_should_hash_length_be_over_four(const void * inBuf,SINTa inSize)
{
	// @@@@ which to use ?
	//	maybe switch to utf8 detector on smaller input sizes? -> nah
	// the time to evaluate the decision here is not as important as getting the decision right
	//	(because getting the decision right has a big effect on the following parse time)
	//	(in particular getting len 6 right when that actually helps saves a lot of parse time)
	
	// -> need a better/bigger test set to really make this decision
	//	on our current set of {silesia+seven} we're over-training on a very few files

// whole file :
 //  481,861 ->   192,865 =  3.202 bpb =  2.498 to 1  len 6
//   481,861 ->   209,413 =  3.477 bpb =  2.301 to 1  len 4
   
// -k16 : even at 64k still doesn't like len 6
//   481,861 ->   219,158 =  3.639 bpb =  2.199 to 1  len 6
//   481,861 ->   214,015 =  3.553 bpb =  2.252 to 1  len 4
   
// -k15 : marginal
//   768,771 ->   357,096 =  3.716 bpb =  2.153 to 1  len 6
//   768,771 ->   357,693 =  3.722 bpb =  2.149 to 1  len 4

//   481,861 ->   224,057 =  3.720 bpb =  2.151 to 1  len 6
//   481,861 ->   222,790 =  3.699 bpb =  2.163 to 1  len 4
   
// -k14 : definitely worse with hash len 6
//   768,771 ->   373,340 =  3.885 bpb =  2.059 to 1  len 6
//   768,771 ->   369,087 =  3.841 bpb =  2.083 to 1  len 4
   
//   481,861 ->   233,335 =  3.874 bpb =  2.065 to 1  len 6
//   481,861 ->   231,158 =  3.838 bpb =  2.085 to 1  len 4
   
/*   
	return false;
/*/

	// different on baby7, model7, mr & osdb

	//return newlz_guess_should_hash_length_be_over_four_by_hash_collisions(inBuf,inSize);
	return newlz_guess_should_hash_length_be_over_four_by_utf8_detector(inBuf,inSize);
/**/
}

OODLE_NS_END
