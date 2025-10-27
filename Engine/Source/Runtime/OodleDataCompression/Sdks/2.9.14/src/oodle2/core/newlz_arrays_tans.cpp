// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "newlz_arrays_tans.h"

#include "rrvarbits.h"

#include "newlz_tans.h"
#include "newlz_tans.inl"
#include "rrcompressutil.h"
#include "cbradutil.h"
#include "histogram.h"
#include "rrlogutil.h"
#include "rrstackarray.h"
#include "oodlemalloc.h"
#include "rrarenaallocator.h"
#include "cpux86.h"
#include "log2table.h"
#include "newlz_speedfit.h"
#include "newlz_arrays.inl"
#include "lzasserts.h"
#include "speedfitter.h"

//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"

OODLE_NS_START

// iOS build wonkiness: ARM64_TANS_ASM switch gets set even on non-ARM64 devices
#if defined(NEWLZ_ARM64_TANS_ASM) && !defined(__RADARM64__)
#undef NEWLZ_ARM64_TANS_ASM
#endif

/**

newlz TANS
5 states x 2 streams

decoder sees the streams as
forward LE + backward BE

streams are LSB-first in terms of bit packing
eg. next word is at bottom
(this removes the zero-bit problem, and allows you to precompute 1<<nb mask in decode table)

-------------

5 states = 5*11 = 55 bit minimum TANS flush
I reduce the pain of this slightly now by packing the last 5 bytes in the states
still wastes 3*5 = 15 bits

**/

/*
//	you mostly want L_bits <= 11
//	  because with 8 byte decode table entries, 2048*8 = 16k = fits in L1
*/

// these are in the bitstream and can't change
#define NEWLZ_TANS_L_BITS_MAX	11
#define NEWLZ_TANS_L_BITS_MIN	8 // has to be >=8 because of byte stuffing

#define NEWLZ_TANS_L_BITS_BITS	2	// 11-8 = 3 = fits in 2 bits


#define GetBits_LSB(into,vb,cnt) do { RR_ASSERT( NVB(vb,_bitcnt) >= cnt ); \
	(into) = NVB(vb,_bits) & RR_BIT_MASK_U32(cnt); \
	NVB(vb,_bits) >>= cnt; NVB(vb,_bitcnt) -= cnt; } while(0)

#define PutBits_LSB_Back(vb,val,nb) do { \
	RR_ASSERT( (NVB(vb,_bitcnt) + (nb)) < 64 ); \
	NVB(vb,_bits) <<= (nb); \
	NVB(vb,_bits) |= (val) & RR_BIT_MASK_U32(nb); \
	NVB(vb,_bitcnt) += (nb);	\
	} while(0)
	
	
#ifdef __RAD64REGS__

// refill vb :
#define Refill64_LSB_LE_Forward(vb) do { \
	RR_ASSERT( NVB(vb,_bitcnt) >= 0 && NVB(vb,_bitcnt) < 64 ); \
	NVB(vb,_bits) |= RR_GET64_LE(NVB(vb,_ptr)) << NVB(vb,_bitcnt); \
	NVB(vb,_ptr) += (63 - NVB(vb,_bitcnt)) >> 3; \
	NVB(vb,_bitcnt) |= 56; } while(0)
			
// note refill backwards : _ptr should have -8 pre-applied!			
#define Refill64_LSB_BE_Backward(vb) do { \
	RR_ASSERT( NVB(vb,_bitcnt) >= 0 && NVB(vb,_bitcnt) < 64 ); \
	NVB(vb,_bits) |= RR_GET64_BE(NVB(vb,_ptr)) << NVB(vb,_bitcnt); \
	NVB(vb,_ptr) -= (63 - NVB(vb,_bitcnt)) >> 3; \
	NVB(vb,_bitcnt) |= 56; } while(0)
			
// note output backwards : _ptr should NOT have -8 pre-applied!
#define OutputBits64_LSB_Back_LE(vb) do { \
	RR_ASSERT( NVB(vb,_bitcnt) >= 0 && NVB(vb,_bitcnt) < 64 ); \
	RR_PUT64_LE( NVB(vb,_ptr)-8, (NVB(vb,_bits) << (64 - NVB(vb,_bitcnt))) ); \
	int nbytes = NVB(vb,_bitcnt)>>3; \
	NVB(vb,_ptr) -= nbytes; \
	NVB(vb,_bitcnt) -= (nbytes<<3); \
	NVB(vb,_bits) &= RR_BIT_MASK_U32(NVB(vb,_bitcnt)); \
	} while(0)
		
#define OutputBits64_LSB_Forward_BE(vb) do { \
	RR_ASSERT( NVB(vb,_bitcnt) >= 0 && NVB(vb,_bitcnt) < 64 ); \
	RR_PUT64_BE( NVB(vb,_ptr), (NVB(vb,_bits) << (64 - NVB(vb,_bitcnt))) ); \
	int nbytes = NVB(vb,_bitcnt)>>3; \
	NVB(vb,_ptr) += nbytes; \
	NVB(vb,_bitcnt) -= (nbytes<<3); \
	NVB(vb,_bits) &= RR_BIT_MASK_U32(NVB(vb,_bitcnt)); \
	} while(0)
	
#define Refill_LSB_LE_Forward		Refill64_LSB_LE_Forward
#define Refill_LSB_BE_Backward		Refill64_LSB_BE_Backward
#define OutputBits_LSB_Back_LE		OutputBits64_LSB_Back_LE
#define OutputBits_LSB_Forward_BE	OutputBits64_LSB_Forward_BE
		 		
#else

// refill vb :
#define Refill32_LSB_LE_Forward(vb) do { \
	RR_ASSERT( NVB(vb,_bitcnt) >= 0 && NVB(vb,_bitcnt) < 32 ); \
	NVB(vb,_bits) |= RR_GET32_LE(NVB(vb,_ptr)) << NVB(vb,_bitcnt); \
	NVB(vb,_ptr) += (31 - NVB(vb,_bitcnt)) >> 3; \
	NVB(vb,_bitcnt) |= 24; } while(0)
			
// note refill backwards : _ptr should have -8 pre-applied!			
#define Refill32_LSB_BE_Backward(vb) do { \
	RR_ASSERT( NVB(vb,_bitcnt) >= 0 && NVB(vb,_bitcnt) < 32 ); \
	NVB(vb,_bits) |= RR_GET32_BE(NVB(vb,_ptr)) << NVB(vb,_bitcnt); \
	NVB(vb,_ptr) -= (31 - NVB(vb,_bitcnt)) >> 3; \
	NVB(vb,_bitcnt) |= 24; } while(0)
			
// note output backwards : _ptr should NOT have -8 pre-applied!
#define OutputBits32_LSB_Back_LE(vb) do { \
	RR_ASSERT( NVB(vb,_bitcnt) >= 0 && NVB(vb,_bitcnt) < 32 ); \
	RR_PUT32_LE( NVB(vb,_ptr)-4, (NVB(vb,_bits) << (32 - NVB(vb,_bitcnt))) ); \
	int nbytes = NVB(vb,_bitcnt)>>3; \
	NVB(vb,_ptr) -= nbytes; \
	NVB(vb,_bitcnt) -= (nbytes<<3); \
	NVB(vb,_bits) &= RR_BIT_MASK_U32(NVB(vb,_bitcnt)); \
	} while(0)
		
#define OutputBits32_LSB_Forward_BE(vb) do { \
	RR_ASSERT( NVB(vb,_bitcnt) >= 0 && NVB(vb,_bitcnt) < 32 ); \
	RR_PUT32_BE( NVB(vb,_ptr), (NVB(vb,_bits) << (32 - NVB(vb,_bitcnt))) ); \
	int nbytes = NVB(vb,_bitcnt)>>3; \
	NVB(vb,_ptr) += nbytes; \
	NVB(vb,_bitcnt) -= (nbytes<<3); \
	NVB(vb,_bits) &= RR_BIT_MASK_U32(NVB(vb,_bitcnt)); \
	} while(0)

#define Refill_LSB_LE_Forward		Refill32_LSB_LE_Forward
#define Refill_LSB_BE_Backward		Refill32_LSB_BE_Backward
#define OutputBits_LSB_Back_LE		OutputBits32_LSB_Back_LE
#define OutputBits_LSB_Forward_BE	OutputBits32_LSB_Forward_BE
		 	
#endif					

// #define RAD_REGBITS 64
// #define RAD_REGBYTES 8

SINTa newLZ_put_array_tans(U8 * const to, U8 * const to_end, const U8 * const from, SINTa from_len, 
									const U32 * histogram,
									F32 lambda, const OodleSpeedFit * speedfit, F32 *pJ,
									rrArenaAllocator * arena)
{
	SIMPLEPROFILE_SCOPE_N(put_array_tans,from_len);

	// normalize :
	// @@ could normalize counts to lower summers for reduced precision
	//		(mainly for small arrays)
	//		-> the counts_bits we normalize to doesn't need to be = L_bits
	// @@ could handle very rare symbols explicitly
	//		(prob < 1/L), force them to the tail of the table, or even use an escape code
	//		(as FSE does)
	
	// 5 minimum for tail stuffing
	// other overhead makes < 8 silly
	//	probably < 16 is pointless
	//	(NEWLZ_HUFF_ARRAY_MIN_SIZE applies and is 32)
	RR_COMPILER_ASSERT( NEWLZ_HUFF_ARRAY_MIN_SIZE >= 8 );
	if ( from_len < NEWLZ_HUFF_ARRAY_MIN_SIZE ) // should only happen if I'm called directly
	//if ( from_len <= NEWLZ_HUFF_ARRAY_MIN_SIZE+5 ) // make count >= 32 even after the -5'ing
		return -1;
	
	F32 prevJ = *pJ;
	// we need this to be a limit against expansion :
	RR_ASSERT( prevJ <= from_len+3 );
			
	// remove last 5 symbols from histogram :
	from_len -= 5;
	U32 * nonconst_histogram = const_cast<U32 *>(histogram);
	nonconst_histogram[ from[from_len+0] ] --;
	nonconst_histogram[ from[from_len+1] ] --;
	nonconst_histogram[ from[from_len+2] ] --;
	nonconst_histogram[ from[from_len+3] ] --;
	nonconst_histogram[ from[from_len+4] ] --;
	
	RR_ASSERT( rrSumOfHistogram(nonconst_histogram,256) == (U32)from_len );
	
	// heuristic to choose L
	//	could also consider "num_non_zero" (annoyingly don't know it yet)
	//	for now just go slightly under array size
	//	(it's silly to have L ever bigger than array size, you're then scaling *up* counts)
	// -2 looks slightly better than -1
	int L_bits = rrIlog2round((U32)from_len) - 2;
	L_bits = RR_CLAMP(L_bits,NEWLZ_TANS_L_BITS_MIN,NEWLZ_TANS_L_BITS_MAX);
	
	int normc_bits = L_bits;
	int normc_sum = 1<<normc_bits; // normc_sum = L
	
	int alphabet = 256;
	while ( alphabet > 0 && histogram[alphabet-1] == 0 ) alphabet--;
	RR_ASSERT( alphabet == 256 || histogram[alphabet] == 0 );
			
	U32 normc[256];
	S32 num_non_zero = normalize_counts_current(normc,normc_sum,histogram,(int)from_len,alphabet);

	// put back 5 removed counts :
	nonconst_histogram[ from[from_len+0] ] ++;
	nonconst_histogram[ from[from_len+1] ] ++;
	nonconst_histogram[ from[from_len+2] ] ++;
	nonconst_histogram[ from[from_len+3] ] ++;
	nonconst_histogram[ from[from_len+4] ] ++;
	
	if ( num_non_zero <= 1 )
	{	
		// don't handle the fully degenerate case here (only one symbol), let huff do it :
		
		//	little bit of a nasty rare case - if the entropy coded prefix (from_len-5) is degenerate
		//		but the last 5 bytes are not the same - that would be an ideal case for TANS
		//		and we bail on it; rare but nasty when it happens
		//	 -> actually RLE should handle that case well now, so okay for TANS to bail on it
	
		return -1;
	}
	
	// if num_non_zero == 2 ; special case binary coder? (rare)
	// if num_non_zero <= 16 could put pairs of symbols together as nibbles
	//	 -> could do that in huff as well
	
	
	F32 tans_J_comp_add = 5 + lambda * speedfit->tans(from_len , num_non_zero, normc_sum );
	SINTa comp_len_must_be_under = (SINTa)(prevJ - tans_J_comp_add);
	// we need this to be a limit against expansion :
	RR_ASSERT( comp_len_must_be_under <= from_len+3 );
	
	// early out when we can't possibly win :
	if ( comp_len_must_be_under < 4 )
	{		
		return -1;
	}	
		
	// zero the counts we didn't fill in normalize_counts
	//	@@ should be unnecessary if all code used alphabet instead of 256
	//	  TODO : fix me
	memset(normc+alphabet,0,sizeof(U32)*(256 - alphabet));
		
	// put counts :
	
	U8 header_buf[512];
	rrVarBits counts_vb;
	rrVarBits_PutOpen(counts_vb.m,header_buf);
	counts_vb.m_end = header_buf + sizeof(header_buf);

	// put a 0 bit for future flagging use :
	rrVarBits_Puta0(counts_vb.m);
	
	RR_VARBITSTYPE L_bits_put = L_bits - NEWLZ_TANS_L_BITS_MIN;
	RR_ASSERT( L_bits_put <= 3 );
	rrVarBits_Put(counts_vb.m,L_bits_put,NEWLZ_TANS_L_BITS_BITS);
	
	newlz_tans_PackCounts(&counts_vb,normc_bits,normc,alphabet,num_non_zero);
	
	rrVarBits_PutFlush8(counts_vb.m);
	SINTa tans_header_size = rrVarBits_PutSizeBytes(counts_vb.m,header_buf);
	
	RR_ASSERT( tans_header_size < (SINTa)sizeof(header_buf) - 32 ); // make sure we're not close to running out
	//rrprintfvar(counts_bytes);

	//===============================================================

	// @@ this is almost the same as newlz_estimate - we repeat this same work a lot
	//	clean that up!

	// quick check to see if we're anywhere near our target
	// we don't even want to bother trying to encode if we're not
	S64 H = 0;
	for (int i = 0; i < alphabet; i++)
	{
		U32 c = normc[i];
		// code cost from the normalized histo but frequencies from the original one
		if ( c > 0 )
		{
			RR_ASSERT( L_bits <= 13 );
			H += (S64)nonconst_histogram[i] * log2tabled<13>(c << (13 - L_bits));
		}
	}
	H >>= RR_LOG2TABLE_ONE_SHIFT + 3; // +3 to convert bits->bytes

	if ( tans_header_size + H >= comp_len_must_be_under )
	{
		return -1;
	}
		
	//===============================================================
	
	// make encoder :
	
	// @@ use "alphabet" instead of 256 here?
	SINTa encmemsize = newlz_tans_Encoder_Size(L_bits);
	
	//rrprintfvar(encmemsize); // 8k - okay on stack? or use arena?
	RR_SCOPE_ARENA_ARRAY(encmem,U8,encmemsize,arena);
	
	newlz_tans_Encoder * enc = newlz_tans_Encoder_Init(encmem,encmemsize,L_bits);
	
	newlz_tans_Encoder_Fill(enc,normc,alphabet);
	
	//===============================================================

	const tans_encode_entry * eet = enc->encode_sym_table;
	UINTr x1,x2,x3,x4,x5;
	
	const U8 * from_ptr_back;
	U8 sym;
	
	U32 wtf_winrt_its_not_implicit = 1U<<L_bits;
	UINTr L = (UINTr)wtf_winrt_its_not_implicit;
	
	//===============================================================
	
	// pass 1 : scan whole array and just count bits
	
	x1 = x2 = x3 = x4 = x5 = L;
	
	#if 1
	RR_ASSERT( L_bits >= 8 );
	x1 |= from[from_len+0];
	x2 |= from[from_len+1];
	x3 |= from[from_len+2];
	x4 |= from[from_len+3];
	x5 |= from[from_len+4];
	#endif
	
	from_ptr_back = from + from_len-1;
	
	// -> needed just for byte alignment
	// -> should also be used for making the code-or-not decision

	const U16 * encode_table_packed = enc->encode_table_packed;
	encode_table_packed; // @@ unused if not using packed_table_offset

// COUNT1 = do a TANS encode, but just count bits, don't put them	
#define COUNT1(eetable,state,bitcnt,sym) do { \
	const tans_encode_entry * ee = eetable+(sym); \
	int nb = ee->max_state_numbits + ( state >= ee->max_state_thresh ); \
	state = *tans_encode_table_ptr(encode_table_packed,ee,state>>nb); \
	bitcnt += nb; \
} while(0)
	
	int prevb1_bitcnt = 0;
	int prevb2_bitcnt = 0;
	
	int ntens = (int)from_len/10;
	int ntentail = (int)from_len%10;
	
	// do the tail that doesn't go in mod-4 first :
	switch ( ntentail )
	{
		//if ( ntentail > 9 ) { sym = *from_ptr_back--;	COUNT1(eet,x5,prevb2_bitcnt,sym); }
		case 9: sym = *from_ptr_back--;	COUNT1(eet,x4,prevb2_bitcnt,sym);
		case 8: sym = *from_ptr_back--;	COUNT1(eet,x3,prevb2_bitcnt,sym);
		case 7: sym = *from_ptr_back--;	COUNT1(eet,x2,prevb2_bitcnt,sym);
		case 6: sym = *from_ptr_back--;	COUNT1(eet,x1,prevb2_bitcnt,sym);
		
		case 5: sym = *from_ptr_back--;	COUNT1(eet,x5,prevb1_bitcnt,sym);
		case 4: sym = *from_ptr_back--;	COUNT1(eet,x4,prevb1_bitcnt,sym);
		case 3: sym = *from_ptr_back--;	COUNT1(eet,x3,prevb1_bitcnt,sym);
		case 2: sym = *from_ptr_back--;	COUNT1(eet,x2,prevb1_bitcnt,sym);
		case 1: sym = *from_ptr_back--;	COUNT1(eet,x1,prevb1_bitcnt,sym);
		case 0:
		break;
		
		RR_NO_DEFAULT_CASE
	}
			
	for LOOP(i,ntens)
	{
		sym = *from_ptr_back--;	COUNT1(eet,x5,prevb2_bitcnt,sym);
		sym = *from_ptr_back--;	COUNT1(eet,x4,prevb2_bitcnt,sym);
		sym = *from_ptr_back--;	COUNT1(eet,x3,prevb2_bitcnt,sym);
		sym = *from_ptr_back--;	COUNT1(eet,x2,prevb2_bitcnt,sym);
		sym = *from_ptr_back--;	COUNT1(eet,x1,prevb2_bitcnt,sym);
		
		sym = *from_ptr_back--;	COUNT1(eet,x5,prevb1_bitcnt,sym);
		sym = *from_ptr_back--;	COUNT1(eet,x4,prevb1_bitcnt,sym);
		sym = *from_ptr_back--;	COUNT1(eet,x3,prevb1_bitcnt,sym);
		sym = *from_ptr_back--;	COUNT1(eet,x2,prevb1_bitcnt,sym);
		sym = *from_ptr_back--;	COUNT1(eet,x1,prevb1_bitcnt,sym);
	}
	
	RR_ASSERT( from_ptr_back == from-1 );
	
	// count for flushing states :
	prevb1_bitcnt += 3*(L_bits);
	prevb2_bitcnt += 2*(L_bits);
	
	int prevb1_bittail = prevb1_bitcnt&7;
	int prevb1_bytecnt = (prevb1_bitcnt+7)/8;
	
	int prevb2_bittail = prevb2_bitcnt&7;
	int prevb2_bytecnt = (prevb2_bitcnt+7)/8;
	
	int precount_code_bytes = prevb1_bytecnt + prevb2_bytecnt;
	
	SINTa complen_estimate = tans_header_size + precount_code_bytes;	
	//rrprintfvar(complen_estimate);
	
	if ( complen_estimate >= comp_len_must_be_under )
	{
		// return of -1 means we did not modify *to
		return -1;
	}
	
 	F32 tans_J = complen_estimate + tans_J_comp_add;
 	// @@ this should be the same as the comp_len_must_be_under check right above
 	if ( tans_J >= prevJ )
 		return -1;
	
	// ! check there's enough room in the buff to write both tans streams
	//	 without them running into each other
	if ( complen_estimate+8 > rrPtrDiff(to_end - to) )
	{
		return -1;
	}
	
	//=========================================================
	
	// must have at least from_len worth of room :
	// RR_ASSERT( (to_end - to) > from_len ); // changed 03-28-2019 , no longer pre-checking room
	
	U8 * to_ptr = to;
	U8 * to_ptr_end = to_end;
	
	// to_end can be really far away
	//	does it help cache hotness in the encoder to keep it closer?
	//to_ptr_end = RR_MIN(to_ptr_end,to_ptr + from_len + 256);
	
	// now we know we have room and will definitely output data
	//	put header to to_ptr :
	memcpy(to_ptr,header_buf,tans_header_size);	
	to_ptr += tans_header_size;			
	
	//===============================================================

	// pass 2 : actual output
	
	from_ptr_back = from + from_len-1;

	x1 = x2 = x3 = x4 = x5 = L;
	
	#if 1
	RR_ASSERT( L_bits >= 8 );
	x1 |= from[from_len+0];
	x2 |= from[from_len+1];
	x3 |= from[from_len+2];
	x4 |= from[from_len+3];
	x5 |= from[from_len+4];
	#endif
	
	U8 * lvb1_ptr; UINTr lvb1_bits; int lvb1_bitcnt;
	U8 * lvb2_ptr; UINTr lvb2_bits; int lvb2_bitcnt;
	
	// lvb1 is forward in the decoder, backward in the encoder (LE in both)
	// lvb2 is backward in the decoder, forward in the encoder (BE in both)
	
	// lvb1 is written from the back of the output buf, then moved to the head
	// lvb2 is written at the head of the output buf, then moved to follow buf1

	// lvb1_ptr is the amount I have written into
	//		(even though it's backward, it does not have -8 pre-applied like the decoder does)
	// lvb1_bitcnt is the # of bits not in _ptr	
		
	lvb1_ptr = to_ptr_end;
	lvb1_bits = 0;
	lvb1_bitcnt = 0;
	
	lvb2_ptr = to_ptr;
	lvb2_bits = 0;
	lvb2_bitcnt = 0;
				
#define PUT1(eetable,state,vb,sym) do { \
		const tans_encode_entry * ee = eetable+(sym); \
		int nb = ee->max_state_numbits + ( state >= ee->max_state_thresh ); \
		PutBits_LSB_Back(vb, state, nb); \
		state = *tans_encode_table_ptr(encode_table_packed,ee,state>>nb); \
	} while(0)
	
	
	if ( prevb1_bittail != 0 )
	{
		PutBits_LSB_Back(lvb1,0,(8 - prevb1_bittail));
	}	
	if ( prevb2_bittail != 0 )
	{
		PutBits_LSB_Back(lvb2,0,(8 - prevb2_bittail));
	}	
	
	// writing backwards, so do the tail that doesn't go in mod-10 first :

	switch(ntentail)
	{
		case 9: sym = *from_ptr_back--;	PUT1(eet,x4,lvb2,sym);
		case 8: sym = *from_ptr_back--;	PUT1(eet,x3,lvb2,sym);
		
		#ifndef __RAD64REGS__
		// 7+11+11 = 29
		OutputBits32_LSB_Forward_BE(lvb2);	
		#endif
	
		case 7: sym = *from_ptr_back--;	PUT1(eet,x2,lvb2,sym);
		case 6: sym = *from_ptr_back--;	PUT1(eet,x1,lvb2,sym);

		
		case 5: sym = *from_ptr_back--;	PUT1(eet,x5,lvb1,sym);
		case 4: sym = *from_ptr_back--;	PUT1(eet,x4,lvb1,sym);
		
		#ifndef __RAD64REGS__
		// 7+11+11 = 29
		OutputBits_LSB_Back_LE(lvb1);	
		#endif
		
		case 3: sym = *from_ptr_back--;	PUT1(eet,x3,lvb1,sym);
		case 2: sym = *from_ptr_back--;	PUT1(eet,x2,lvb1,sym);
		
		#ifndef __RAD64REGS__
		// 2*11 = 22 , 3*11 = 33
		OutputBits_LSB_Back_LE(lvb1);	
		#endif
		
		case 1: sym = *from_ptr_back--;	PUT1(eet,x1,lvb1,sym);
		case 0:
		break;
		
		RR_NO_DEFAULT_CASE
	}
	
	// NOTE : in first Output, 
	//	could have 55 bits from codes + 7 bits from padding = 62 bits total
	//	still just barely fits in 64-bit bitbuff
	OutputBits_LSB_Forward_BE(lvb2);
	OutputBits_LSB_Back_LE(lvb1);		
				
	for LOOP(i,ntens)
	{
		sym = *from_ptr_back--;	PUT1(eet,x5,lvb2,sym);
		sym = *from_ptr_back--;	PUT1(eet,x4,lvb2,sym);
		
		#ifndef __RAD64REGS__
		OutputBits32_LSB_Forward_BE(lvb2);
		#endif
		
		sym = *from_ptr_back--;	PUT1(eet,x3,lvb2,sym);
		sym = *from_ptr_back--;	PUT1(eet,x2,lvb2,sym);
		
		#ifndef __RAD64REGS__
		OutputBits32_LSB_Forward_BE(lvb2);
		#endif
		
		sym = *from_ptr_back--;	PUT1(eet,x1,lvb2,sym);

		OutputBits_LSB_Forward_BE(lvb2);
		
		sym = *from_ptr_back--;	PUT1(eet,x5,lvb1,sym);
		sym = *from_ptr_back--;	PUT1(eet,x4,lvb1,sym);
		
		#ifndef __RAD64REGS__
		OutputBits32_LSB_Back_LE(lvb1);	
		#endif
	
		sym = *from_ptr_back--;	PUT1(eet,x3,lvb1,sym);
		sym = *from_ptr_back--;	PUT1(eet,x2,lvb1,sym);
		
		#ifndef __RAD64REGS__
		OutputBits32_LSB_Back_LE(lvb1);	
		#endif
	
		sym = *from_ptr_back--;	PUT1(eet,x1,lvb1,sym);
		
		OutputBits_LSB_Back_LE(lvb1);			
	}
	
	RR_ASSERT( from_ptr_back == from-1 );
	
	// put state in L+1 bits
	RR_ASSERT( x1 >= L && x1 < 2*L );
	RR_ASSERT( x2 >= L && x2 < 2*L );
	RR_ASSERT( x3 >= L && x3 < 2*L );
	RR_ASSERT( x4 >= L && x4 < 2*L );
	RR_ASSERT( x5 >= L && x5 < 2*L );
	
	PutBits_LSB_Back(lvb1, x5, L_bits);
	PutBits_LSB_Back(lvb2, x4, L_bits);
	PutBits_LSB_Back(lvb1, x3, L_bits);	
	PutBits_LSB_Back(lvb2, x2, L_bits);	
	
	#ifndef __RAD64REGS__
	OutputBits32_LSB_Back_LE(lvb1);	
	#endif
		
	PutBits_LSB_Back(lvb1, x1, L_bits);		

	OutputBits_LSB_Back_LE(lvb1);
	OutputBits_LSB_Forward_BE(lvb2);
	
	// should have been a full flush because we pre-aligned :
	RR_ASSERT( NVB(lvb1,_bitcnt) == 0 );
	RR_ASSERT( NVB(lvb2,_bitcnt) == 0 );
	
	// lvb points are at the end of where they wrote
	//	(lvb1 wrote backwards so it's at the beginning of the data in address range)
	// pointers should not have crossed :
	// ! AND make sure there's enough pad space that the bitbuf write-ahead didn't collide :
	RR_ASSERT( lvb1_ptr >= lvb2_ptr );
	RR_ASSERT( rrPtrDiff(lvb1_ptr - lvb2_ptr) >= 8 );
	
	// lvb1 wrote backward from end, will read forward from the beginning
	SINTa lvb1_compLen = rrPtrDiff(to_ptr_end - lvb1_ptr);
	
	// lvb2 wrote forward from beginning, will read backward from the end
	SINTa lvb2_compLen = rrPtrDiff(lvb2_ptr - to_ptr);
	
	RR_ASSERT( lvb1_compLen == prevb1_bytecnt );
	RR_ASSERT( lvb2_compLen == prevb2_bytecnt );
			
	SINTa tans_compLen = lvb1_compLen + lvb2_compLen;

	// currently in buffer as :
	//	[lvb2][...][lvb1]
	//		(lvb1 wrote backwards, lvb2 wrote forward)
	// change to :
	//  [lvb1][lvb2][...]
	//		(lvb1 reads forwards, lvb2 reads backwards)
	

	// equivalent test :
	//if ( rrPtrDiff(lvb1_ptr-lvb2_ptr) > lvb1_compLen )
	if ( lvb1_ptr > to_ptr+tans_compLen )
	{
		// simple case :
		memmove(to_ptr+lvb1_compLen,to_ptr,lvb2_compLen);
		memmove(to_ptr,lvb1_ptr,lvb1_compLen);
	}
	else
	{
		// not enough room for simple case; need to malloc temp space to swap buffers
		// @@ this does happen a decent amount!
		//
		// NOTE(fg): no need to use a scratch alloc if we use a custom copy loop
		// basic idea: every iter, read (say) 16B from lvb1, read 16B from lvb2, write both
		//   to their destination.
		//   lvb1 write overwrites precisely the lvb2 bytes we just read (no problem)
		//   lvb2 write may stomp over early source-lvb1 bytes but we already read those
		// loop for min(lvb1_size,lvb2_size), with last few bytes being done carefully.
		//
		// Not sure if worthwhile but it's an option.
		RR_SCOPE_ARENA_ARRAY(temp,U8,lvb1_compLen,arena);
		memcpy(temp,lvb1_ptr,lvb1_compLen);
		memmove(to_ptr+lvb1_compLen,to_ptr,lvb2_compLen);
		memcpy(to_ptr,temp,lvb1_compLen);
	}
	
	to_ptr += tans_compLen;
		
	SINTa compLen = rrPtrDiff(to_ptr - to);
	RR_ASSERT( compLen == complen_estimate );

	//rrprintfvar(compLen);
	// tans_J was computed from complen_estimate
	//	and we checked already that it is an improvement
	RR_ASSERT( tans_J < *pJ );
	RR_ASSERT( tans_J >= compLen+5 );
 	*pJ = tans_J;
 	
	return compLen;
}

//===============================================================================

// KrakenTansState is in newlz_huff_common.inc too - MUST MATCH
struct KrakenTansState
{
    tans_decode_entry_U8 * table;
    U8 *decodeptr;
    U8 *decodeend;
    const U8 *bitp[2];
    U32 bits[2];
    U32 bitc[2];
    U32 tans_state[5];
};

static bool tansx2_finish(const KrakenTansState *s)
{   
	SIMPLEPROFILE_SCOPE_N(tansx2_finish,rrPtrDiff(s->decodeend - s->decodeptr));
	
	static const SINTa kLookaheadBytes = 2*3; // two streams look ahead up to 3 bytes per stream

	// Each of the two streams has 3 refills per iter, each reading 4 bytes from the current position
	// then advancing the pointer by up to 3 bytes.
    static const SINTa kMaxReadPerIter = 2*3 + 4; // Third (last) refill per stream: reads 4 bytes, prev 2 refills have advanced by up to 3 bytes each.

	// Tail buffer size is determined as follows:
	// We switch to copying data to the tail buffer once we have in_left < kMaxReadPerIter.
	//
	// Our in0 and in1 pointers both look ahead by a few bytes; we cancel this look-ahead before
	// we finalize ("step back" below), so once we switch to the tail buffer, we want to make sure
	// to cancel any existing lookahead _at that time_ so that all the bytes we might later back
	// up to also end up in the tail buffer. This adds at most an extra kLookaheadBytes (which is
	// already accounting for the lookahead in both streams).
	//
	// We don't know how the remaining bytes divide up between the two streams at that point.
	// Therefore, just make two copies of all the remaining bytes, one for the forward stream
	// and one for the backward stream. Either half also acts as padding for the other.
	// The size required for this buffer is
	//
	//   kTailBufSize = 2*kMaxReadPerIter + 2*kLookaheadBytes
	//
	// for two copies of the original remaining bytes from the top of the loop, plus two copies
	// of the lookahead bytes.
	//
	// After this process, set in0 to point to the begining of the tail buffer, and in1 to the end.
	// At that stage, in1 - in0 = kTailBufSize.
	//
	// Before we started, we had a distance of "in_left < kMaxReadPerIter" bytes between the
	// two pointers, hence strictly less than kMaxReadPerIter + kLookaheadBytes left to
	// process after accounting for the look-ahead.
	//
	// After this whole switch-over, we now have
	//
	//   in1 - in0 = kTailBufSize = 2*kMaxReadPerIter + 2*kLookaheadBytes
	//
	// Now suppose in some later iteration of the loop, with new in0' and in1', we fail the
	// kMaxReadPerIter check again:
	//
	//   in1' - in0' < kMaxReadPerIter
	//
	// That implies that after setting up the tail buffer (which made in0 and in1 be
	// kTailBufSize bytes apart), we've advanced by more than
	//
	//    kTailBufSize - kMaxReadPerIter
	//
	// extra bytes, and hence actually consumed (accounting for lookahead again) more than
	//
	//    kTailBufSize - kMaxReadPerIter - kLookaheadBytes
	//
	// bytes, which works out to
	//
	//    kMaxReadPerIter + kLookaheadBytes
	//
	// bytes! But when we set up the tail buffer for the first time, there were _less_ than
	// this many bytes left in the stream (which is what made us do the setup in the first
	// place). Therefore, if we enter the tail buffer setup a second time, we know
	// immediately that the stream can't possibly be well-formed.
    static const SINTa kTailBufSize = 2*kMaxReadPerIter + 2*kLookaheadBytes;
    U8 tailbuf[kTailBufSize];
    SINTa final_nbytes = 0; // changed to the expected tail gap when we switch into tail buff

    const U8 *in0 = s->bitp[0]; // Forward stream, pointer is inclusive (next byte is at in0[0]).
    const U8 *in1 = s->bitp[1]; // Backward stream, pointer is exclusive (next byte is at in1[-1]).
    
    U32 bits0 = s->bits[0], bitc0 = s->bitc[0];
    U32 bits1 = s->bits[1], bitc1 = s->bitc[1];

	// We expect to have no lookahead at this point, just a partially consumed byte.
	// The fast TANS readers are required to ensure this.
	RR_ASSERT( bitc0 <= 7 && bitc1 <= 7 );

    // in0 == in1 is ok, but they should not have already crossed.
	// (Because we have no lookahead as we just asserted, if
	// in0 > in1, the current read pointers have actually
	// crossed.)
    REQUIRE_FUZZ_RETURN( in1 >= in0 , false );

    U8 *decodeptr = s->decodeptr;
    U8 *decodeend = s->decodeend;

	RR_ASSERT( decodeptr <= decodeend ); // should be true by construction
	
    U32 tans0 = s->tans_state[0];
    U32 tans1 = s->tans_state[1];
    U32 tans2 = s->tans_state[2];
    U32 tans3 = s->tans_state[3];
    U32 tans4 = s->tans_state[4];
	tans_decode_entry_U8 * table = s->table;

    // Final decode loop. Very careful about what memory we access.
    while (decodeptr < decodeend)
    {
        SINTa in_left = rrPtrDiff(in1 - in0);

		// As noted above, kMaxReadPerIter is a bound on how many bytes each stream reads per iteration.
		// Because the two pointers advance towards each other, as long as in_left >= kMaxReadPerIter
		// here, both the forward and backward stream refill are guaranteed to access memory that's
		// in bounds. Stream 0 can read bytes from in0[0..kMaxReadPerIter-1], stream 1 can read
		// bytes from in1[-kMaxReadPerIter..-1], and since we have
		//
		//   begin <= in0 (always), in1 <= end (also always)
		//
		// then in1 - in0 >= kMaxReadPerIter implies
		//
		//   in0 + kMaxReadPerIter <= in1 <= end   (i.e. in0 doesn't run past end)
		//   in1 - kMaxReadPerIter >= in0 >= begin (i.e. in1 doesn't run past begin)
		//
		// Note that *both* streams can advance by kMaxReadPerIter per iteration; therefore, it
		// is possible to "tunnel" from in_left >= kMaxReadPerIter in one iteration straight to
		// in_left < 0 in the next. (I.e. the pointers have crossed.)
        if (in_left < kMaxReadPerIter)
        {        
            // if we were in the tail buf already (signalled with final_nbytes), we read past the
            // end of the actual data; bail.
            REQUIRE_FUZZ_RETURN( final_nbytes == 0 , false );

            // Cancel lookahead for both streams
			in0 -= bitc0 >> 3; bitc0 &= 7;
			in1 += bitc1 >> 3; bitc1 &= 7;
			in_left = in1 - in0;
			REQUIRE_FUZZ_RETURN( in_left >= 0, false ); // if the pointers have actually crossed (in_left<0), stream is bad

			// in_left was < kMaxReadPerIter
			//	by cancelling the lookahead we added at most kLookaheadBytes
			RR_ASSERT( in_left < kMaxReadPerIter + kLookaheadBytes );
			// kTailBufSize can hold two copies of the remaining bytes, with space to spare:
			RR_ASSERT( in_left*2 < kTailBufSize );
    
			// Set up the two copies, "left" half for in0, "right" half for in1;
			// the middle is a "no man's land".
            memcpy(tailbuf, in0, in_left);
            memcpy(tailbuf + kTailBufSize - in_left, in1 - in_left, in_left);

            in0 = tailbuf;
            in1 = tailbuf + kTailBufSize;
			// in0 and in1 are now kTailBufSize apart

            // Compute final distance between the ptrs after an
            // extra in_left bytes have been consumed.
            final_nbytes = kTailBufSize - in_left;

            // final_nbytes != 0 is required just because we use that to flag that we are in the tail.
			// We ensured that above 0 <= in_left*2 < kTailBufSize,
			// therefore final_bytes = kTailBufSize - in_left > kTailBufSize - (kTailBufSize/2) = kTailBufSize/2 > 0.
            RR_ASSERT( final_nbytes != 0 );
        }

        U32 len;

        #define REFILL0() \
            bits0 |= RR_GET32_LE(in0) << bitc0; \
            in0 += (31 - bitc0) >> 3; \
            bitc0 |= 24

        #define REFILL1() \
            bits1 |= RR_GET32_BE(in1 - 4) << bitc1; \
            in1 -= (31 - bitc1) >> 3; \
            bitc1 |= 24

        #define DECONE(tans,bits,bitc) \
            *decodeptr++ = table[tans].sym; \
            len = table[tans].len; \
            tans = (bits & table[tans].mask) + table[tans].nextst; \
            bits >>= len; \
            bitc -= len

        // We can run out of symbols to check any time, so check after every decode.
        // (The last one is covered by the loop condition.)

        REFILL0();
        DECONE(tans0, bits0, bitc0);
        if (decodeptr >= decodeend) break;
        DECONE(tans1, bits0, bitc0);
        if (decodeptr >= decodeend) break;
        REFILL0();
        DECONE(tans2, bits0, bitc0);
        if (decodeptr >= decodeend) break;
        DECONE(tans3, bits0, bitc0);
        if (decodeptr >= decodeend) break;
        REFILL0();
        DECONE(tans4, bits0, bitc0);
        if (decodeptr >= decodeend) break;

        REFILL1();
        DECONE(tans0, bits1, bitc1);
        if (decodeptr >= decodeend) break;
        DECONE(tans1, bits1, bitc1);
        if (decodeptr >= decodeend) break;
        REFILL1();
        DECONE(tans2, bits1, bitc1);
        if (decodeptr >= decodeend) break;
        DECONE(tans3, bits1, bitc1);
        if (decodeptr >= decodeend) break;
        REFILL1();
        DECONE(tans4, bits1, bitc1);

        #undef REFILL0
        #undef REFILL1
        #undef DECONE
    }

	// decodeptr != decodeend does not need to be checked, it's not possible
	RR_ASSERT( decodeptr == decodeend );

	/*
	// test :
	// if we went into safe-tail path
	// then we never hit (in1-in0) < kMaxReadPerIter again
	if ( final_nbytes != 0 )
	{
		SINTa diff = rrPtrDiff(in1 - in0);
		static SINTa min_diff = 99;
		min_diff = RR_MIN(diff,min_diff);
		rrprintfvar(min_diff);
		// min_diff = 11
		// kMaxReadPerIter = 10
		// -> safe!
	}
	/**/

    // Step back by number of lookahead bytes still in the buffer.
    // At this point we should have exhausted both the input and output
    // buffers. If not, something went wrong.
    in0 -= bitc0 >> 3;
    in1 += bitc1 >> 3;

    // The "in" ptrs should be inside the tail buffer, at the correct distance
    //	from each other
    //	final_nbytes == 0 if we didn't go into the tail safety branch
    //	otherwise it's the separation created there
    
    REQUIRE_FUZZ_RETURN( rrPtrDiff(in1 - in0) == (SINTa)final_nbytes , false );
   
	// final states should all be < 256 in valid streams :
	//	this check is not necessary for fuzz safety
	//	but provides some extra checksumishing
	//	(@@ could put something more useful in these top bits than just zeros?)
	U32 tans_states_or = tans0|tans1|tans2|tans3|tans4;
	REQUIRE_FUZZ_RETURN( tans_states_or <= 255 , false );
		
	// final 5 states have 5 more bytes :
	decodeend[0] = (U8) tans0;
	decodeend[1] = (U8) tans1;
	decodeend[2] = (U8) tans2;
	decodeend[3] = (U8) tans3;
	decodeend[4] = (U8) tans4;
			
	return true;
}

#ifdef __RAD64REGS__

// 64b bit IO variant
static bool tansx2_64(KrakenTansState *s)
{
	SIMPLEPROFILE_SCOPE_N(tansx2_64,rrPtrDiff(s->decodeend - s->decodeptr));
	
    U8 *decodeptr = s->decodeptr;
    U8 *decodeend = s->decodeend;
    const U8 *in0 = s->bitp[0];
    const U8 *in1 = s->bitp[1];

    if (in1 - in0 >= 8 && decodeend - decodeptr >= 10)
    {
		U64 bits0 = s->bits[0], bitc0 = s->bitc[0];
		U64 bits1 = s->bits[1], bitc1 = s->bitc[1];
    
        UINTr tans0 = s->tans_state[0];
        UINTr tans1 = s->tans_state[1];
        UINTr tans2 = s->tans_state[2];
        UINTr tans3 = s->tans_state[3];
        UINTr tans4 = s->tans_state[4];
		tans_decode_entry_U8 * table = s->table;

        #define DECONE(tans,ind) \
            *decodeptr++ = table[tans].sym; \
            len = table[tans].len; \
            tans = (bits##ind & table[tans].mask) + table[tans].nextst; \
            bits##ind >>= len; \
            bitc##ind -= len

        // move our internal end pointers to the watermarks
        in1 -= 8;
        decodeend -= 10;
        
        // in0 <= in1 ensures no access violation - but I could over-read to crossing pointers
        // decodeptr == decodeend means I have 10 left and will read them all up to end
        while (in0 <= in1 && decodeptr <= decodeend)
        {
            U32 len;

			RR_ASSERT( bitc0 < 64 );
			RR_ASSERT( bitc1 < 64 );

            bits0 |= RR_GET64_LE(in0) << bitc0;
            in0 += (63 - bitc0) >> 3;
            bitc0 |= 56;

            bits1 |= RR_GET64_BE(in1) << bitc1;
            in1 -= (63 - bitc1) >> 3;
            bitc1 |= 56;

            DECONE(tans0, 0);
            DECONE(tans1, 0);
            DECONE(tans2, 0);
            DECONE(tans3, 0);
            DECONE(tans4, 0);

            DECONE(tans0, 1);
            DECONE(tans1, 1);
            DECONE(tans2, 1);
            DECONE(tans3, 1);
            DECONE(tans4, 1);
        }

        #undef DECONE
		
        in1 += 8;

        // Transition to careful loop
        s->decodeptr = decodeptr;
        RR_ASSERT( decodeptr <= s->decodeend );
        s->bitp[0] = in0 - (bitc0 >> 3);
        s->bitp[1] = in1 + (bitc1 >> 3);
        //RR_ASSERT( s->bitp[0] <= s->bitp[1] );
        // <- this could be false, we may have over-read
        //	will be checked at the start of tansx2_finish
        s->bits[0] = (U32) bits0;
        s->bits[1] = (U32) bits1;
        s->bitc[0] = (U32) (bitc0 & 7);
        s->bitc[1] = (U32) (bitc1 & 7);
        s->tans_state[0] = (U32) tans0;
        s->tans_state[1] = (U32) tans1;
        s->tans_state[2] = (U32) tans2;
        s->tans_state[3] = (U32) tans3;
        s->tans_state[4] = (U32) tans4;
    }
    
    return tansx2_finish(s);
}

#else // __RAD64REGS__

// 32b bit IO variant
static bool tansx2_32(KrakenTansState *s)
{
	SIMPLEPROFILE_SCOPE_N(tansx2_32,rrPtrDiff(s->decodeend - s->decodeptr));

	U8 * RADRESTRICT decodeptr = s->decodeptr;
	U8 *decodeend = s->decodeend;
	const U8 *in0 = s->bitp[0];
	const U8 *in1 = s->bitp[1];

	// We do up to 5 32-bit refills per group of 20 values decoded, so make sure
	// we have at least 20 bytes of input left. (This is overly pessimistic, since
	// we consume at most 11 bits per symbol decoded, but it'll do.)
	if (in1 - in0 >= 20 && decodeend - decodeptr >= 20)
	{
		U32 bits0 = s->bits[0], bitc0 = s->bitc[0];
		U32 bits1 = s->bits[1], bitc1 = s->bitc[1];

		U32 tans0 = s->tans_state[0];
		U32 tans1 = s->tans_state[1];
		U32 tans2 = s->tans_state[2];
		U32 tans3 = s->tans_state[3];
		U32 tans4 = s->tans_state[4];
		const tans_decode_entry_U8 * table = s->table;

		#define REFILL0() \
			bits0 |= RR_GET32_LE(in0) << bitc0; \
			in0 += (31 - bitc0) >> 3; \
			bitc0 |= 24

		// NOTE: in1 + 16 because we pre-decrement in1 by 20 below and do a 4-byte read
		#define REFILL1() \
			bits1 |= RR_GET32_BE(in1 + 16) << bitc1; \
			in1 -= (31 - bitc1) >> 3; \
			bitc1 |= 24

		#define DECONE(tans,ind,outidx) \
			decodeptr[outidx] = table[tans].sym; \
			len = table[tans].len; \
			tans = (bits##ind & table[tans].mask) + table[tans].nextst; \
			bits##ind >>= len; \
			bitc##ind -= len

		// move our internal end pointers to the watermarks
		in1 -= 20;
		decodeend -= 20;

		// in0 <= in1 ensures no access violation - but I could over-read to crossing pointers
		// decodeptr == decodeend means I have 20 left and will read them all up to end
		while (in0 <= in1 && decodeptr <= decodeend)
		{
			U32 len;

			RR_ASSERT( bitc0 < 32 );
			RR_ASSERT( bitc1 < 32 );

		#ifdef __RADARM__
			// Prefetch ahead on ARM since some cores don't like the forward/backward sequential mix
			RR_PREFETCHR_CL(in0 + 64);
			RR_PREFETCHR_CL(in1 - 64);
		#endif

			// Bursts of 5 decodes alternate between bit buffers, but since we need to refill every 2 decodes,
			// that leaves us with a half-wasted refill if we decode 10 per loop iter; so decode 4 bursts for
			// a total of 20 symbols, which uses all 10 refills perfectly and leaves us back in phase.

			REFILL0();
			DECONE(tans0, 0, 0);
			DECONE(tans1, 0, 1);
			REFILL0();
			DECONE(tans2, 0, 2);
			DECONE(tans3, 0, 3);
			REFILL0();
			DECONE(tans4, 0, 4);

			REFILL1();
			DECONE(tans0, 1, 5);
			DECONE(tans1, 1, 6);
			REFILL1();
			DECONE(tans2, 1, 7);
			DECONE(tans3, 1, 8);
			REFILL1();
			DECONE(tans4, 1, 9);

			DECONE(tans0, 0, 10);
			REFILL0();
			DECONE(tans1, 0, 11);
			DECONE(tans2, 0, 12);
			REFILL0();
			DECONE(tans3, 0, 13);
			DECONE(tans4, 0, 14);

			DECONE(tans0, 1, 15);
			REFILL1();
			DECONE(tans1, 1, 16);
			DECONE(tans2, 1, 17);
			REFILL1();
			DECONE(tans3, 1, 18);
			DECONE(tans4, 1, 19);

			decodeptr += 20;
		}

		#undef REFILL0
		#undef REFILL1
		#undef DECONE

		in1 += 20;

		// Transition to careful loop
		s->decodeptr = decodeptr;
		RR_ASSERT( decodeptr <= s->decodeend );
		s->bitp[0] = in0 - (bitc0 >> 3);
		s->bitp[1] = in1 + (bitc1 >> 3);
		//RR_ASSERT( s->bitp[0] <= s->bitp[1] );
		// <- this could be false, we may have over-read
		//	will be checked at the start of tansx2_finish
		s->bits[0] = bits0;
		s->bits[1] = bits1;
		s->bitc[0] = bitc0 & 7;
		s->bitc[1] = bitc1 & 7;
		s->tans_state[0] = tans0;
		s->tans_state[1] = tans1;
		s->tans_state[2] = tans2;
		s->tans_state[3] = tans3;
		s->tans_state[4] = tans4;
	}

	return tansx2_finish(s);
}

#endif

//===============================================================================

#if defined(NEWLZ_JAGUAR_TANS_ASM)

// ASM version
extern "C" bool oodle_newLZ_tans_x64_jaguar_kern(KrakenTansState *s);

static bool tansx2_x64_jag_asm(KrakenTansState *s)
{
	SIMPLEPROFILE_SCOPE_N(tansx2_x64_jag_asm,rrPtrDiff(s->decodeend - s->decodeptr));
		
    if (!oodle_newLZ_tans_x64_jaguar_kern(s)) return false;
    return tansx2_finish(s);
}

#elif defined(NEWLZ_X64GENERIC_TANS_ASM)

// ASM version
extern "C" bool oodle_newLZ_tans_x64_generic_kern(KrakenTansState *s);

static bool tansx2_x64_asm(KrakenTansState *s)
{
	SIMPLEPROFILE_SCOPE_N(tansx2_x64_asm,rrPtrDiff(s->decodeend - s->decodeptr));
	
    if (!oodle_newLZ_tans_x64_generic_kern(s)) return false;
    return tansx2_finish(s);
}

// ASM BMI2 version
extern "C" bool oodle_newLZ_tans_x64_bmi2_kern(KrakenTansState *s);

static bool tansx2_x64_bmi2_asm(KrakenTansState *s)
{
	SIMPLEPROFILE_SCOPE_N(tansx2_x64_bmi2_asm,rrPtrDiff(s->decodeend - s->decodeptr));
	
    if (!oodle_newLZ_tans_x64_bmi2_kern(s)) return false;
    return tansx2_finish(s);
}

// ASM BMI2 Raptor Lake version
extern "C" bool oodle_newLZ_tans_x64_bmi2_rpl_kern(KrakenTansState *s);

static bool tansx2_x64_bmi2_rpl_asm(KrakenTansState *s)
{
	SIMPLEPROFILE_SCOPE_N(tansx2_x64_bmi2_rpl_asm,rrPtrDiff(s->decodeend - s->decodeptr));

	if (!oodle_newLZ_tans_x64_bmi2_rpl_kern(s)) return false;
	return tansx2_finish(s);
}

#elif defined(NEWLZ_ARM64_TANS_ASM)

// ASM version
extern "C" bool oodle_newLZ_tans_a64_kern(KrakenTansState *s);

static bool tansx2_arm64_asm(KrakenTansState *s)
{
	SIMPLEPROFILE_SCOPE_N(tansx2_arm64_asm,rrPtrDiff(s->decodeend - s->decodeptr));

    if (!oodle_newLZ_tans_a64_kern(s)) return false;
    return tansx2_finish(s);
}

#endif

//===============================================================================

//#define DUMP_DECOMP_ARRAYS // FG TEST

#ifdef DUMP_DECOMP_ARRAYS
OODLE_NS_END
#include <stdio.h>
OODLE_NS_START
#endif
									
SINTa newlz_get_array_tans(const U8 * const comp, SINTa comp_len, U8 * const to, SINTa to_len, U8 * scratch_ptr, U8 * scratch_end)
{
	SIMPLEPROFILE_SCOPE_N(get_array_tans,to_len);
	
	//rrprintfvar(to_len);
	//rrprintfvar(comp_len);

	const U8 * comp_ptr = comp;
	const U8 * comp_end = comp + comp_len;
	
	// ensure initial U64 fill is okay :
	REQUIRE_FUZZ_RETURN( comp_len >= 8 , -1 );
	REQUIRE_FUZZ_RETURN( to_len >= 5, -1 ); // last 5 bytes are special
	
	rrVarBits_Temps();

	int L_bits;
	newlz_tans_UnpackedCounts unpacked_counts;
	
	{
	SIMPLEPROFILE_SCOPE(tans_unpack);
	
	rrVarBits header_vb;
	rrVarBits_GetOpen(header_vb.m,comp_ptr,comp_end);	
	
	// (this is actually just getting 3 bits and then checking that only the bottom 2 are set) 
	
	RR_VARBITSTYPE flag = rrVarBits_Get1(header_vb.m);
	// flag == 0 ; reserved for future use
	REQUIRE_FUZZ_RETURN( flag == 0, -1 );
	
	L_bits = (int) rrVarBits_Get_C(header_vb.m,NEWLZ_TANS_L_BITS_BITS);
	L_bits += NEWLZ_TANS_L_BITS_MIN;
	RR_ASSERT( L_bits <= NEWLZ_TANS_L_BITS_MAX );
	
	// @@ in theory counts_bits could be different than L_bits
	if ( !newlz_tans_UnPackCounts(&header_vb,L_bits,&unpacked_counts) )
	{
		NEWLZ_ARRAY_RETURN_FAILURE();
	}
	
	// note comp_ptr can be after comp_end here on corrupt data : (checked below via tans_compLen)
	comp_ptr = rrVarBits_GetEndPtr(header_vb.m);
	RR_ASSERT_IF_NOT_CORRUPT( comp_ptr <= comp_end );
	}

	#ifdef SPEEDFITTING
	g_speedfitter_tans_L = 1<<L_bits;
	g_speedfitter_huff_num_non_zero = unpacked_counts.num_singles + unpacked_counts.num_larger;
	#endif
	
	//SINTa tans_header_size = rrPtrDiff(comp_ptr - comp);
	//rrprintfvar(tans_header_size);
		
	// the actual TANS payload portion
	// ensure that an initial fill of U64 bitbuf is okay
	SINTa tans_compLen = rrPtrDiff( comp_end - comp_ptr );
	
	static const SINTa comp_scratch_landing_pad = 8;
	U8 comp_scratch[2*comp_scratch_landing_pad + 8]; // front/back landing pads plus 8B payload
	if ( tans_compLen < 8 )
	{
		// fuzz checks comp_ptr going past comp_end :
		REQUIRE_FUZZ_RETURN( tans_compLen > 0 , -1);
	
		memset(comp_scratch,0,sizeof(comp_scratch)); // make sure it's initialized
		memcpy(comp_scratch+comp_scratch_landing_pad,comp_ptr,tans_compLen);
		comp_ptr = comp_scratch+comp_scratch_landing_pad;
		comp_end = comp_ptr + tans_compLen;
	}
	
	// make the decoder :
	
	SINTa decmemsize = newlz_tans_Decoder_Size(L_bits);
	
	#define TANS_DECODE_SCRATCH_MEM_LIMIT	(17*1024) // 16k + a little more
	RR_ASSERT( decmemsize <= TANS_DECODE_SCRATCH_MEM_LIMIT );
	
	RR_COMPILER_ASSERT( TANS_DECODE_SCRATCH_MEM_LIMIT < NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH );
	
	if ( (scratch_end - scratch_ptr) < 16 ) NEWLZ_ARRAY_RETURN_FAILURE();
	scratch_ptr = rrAlignUpPointer(scratch_ptr,16);
	void * decmem = scratch_ptr;
	if ( (scratch_end - scratch_ptr) < decmemsize ) NEWLZ_ARRAY_RETURN_FAILURE();
	
	newlz_tans_Decoder * dec = newlz_tans_Decoder_Init(decmem,decmemsize,L_bits);
	
	newlz_tans_Decoder_Fill(dec,&unpacked_counts);
	

	{
		const U8 * lvb1_ptr;
		UINTr lvb1_bits;
		int lvb1_bitcnt;
		
		const U8 * lvb2_ptr;
		UINTr lvb2_bits;
		int lvb2_bitcnt;
		
		lvb1_ptr = comp_ptr;
		lvb1_bits = RR_GETR_LE(lvb1_ptr);
		lvb1_bitcnt = RAD_REGBITS;
		lvb1_ptr += RAD_REGBYTES;
		
		lvb2_ptr = comp_end - RAD_REGBYTES;
		lvb2_bits = RR_GETR_BE(lvb2_ptr);
		lvb2_bitcnt = RAD_REGBITS;
		lvb2_ptr -= RAD_REGBYTES;
		
		//rrVarBits_GetOpen_PartialHeader(lvb1,comp_ptr,comp_end);
		
		UINTr x1,x2,x3,x4,x5;
		
		// initial state = get L bits		
		GetBits_LSB(x1,lvb1,L_bits);
		GetBits_LSB(x2,lvb2,L_bits);
		GetBits_LSB(x3,lvb1,L_bits);
		GetBits_LSB(x4,lvb2,L_bits);

		#ifndef __RAD64REGS__
		Refill32_LSB_LE_Forward(lvb1);
		#endif		
		
		GetBits_LSB(x5,lvb1,L_bits);
		
		//-------------------------------------------
		
		to_len -= 5;
	
		U8 * to_ptr = to;
		U8 * to_ptr_end = to_ptr + to_len;
		
		//-------------------------------------------
		
		KrakenTansState s;
		s.decodeptr = to_ptr;
		s.decodeend = to_ptr_end;
		s.bitp[0] = lvb1_ptr - (lvb1_bitcnt>>3);
		s.bitp[1] = lvb2_ptr + RAD_REGBYTES + (lvb2_bitcnt>>3);
		s.bits[0] = (U32) lvb1_bits;
		s.bits[1] = (U32) lvb2_bits;
		s.bitc[0] = lvb1_bitcnt & 7;
		s.bitc[1] = lvb2_bitcnt & 7;
		s.table = dec->decode_table_U8;
		s.tans_state[0] = (U32) x1;
		s.tans_state[1] = (U32) x2;
		s.tans_state[2] = (U32) x3;
		s.tans_state[3] = (U32) x4;
		s.tans_state[4] = (U32) x5;
		
		bool ok;
		
		// these defines are set by cdep - not used in my msvc build
		#if defined(NEWLZ_JAGUAR_TANS_ASM)
		ok = tansx2_x64_jag_asm(&s);
		#elif defined(NEWLZ_X64GENERIC_TANS_ASM)
		
		if (rrCPUx86_feature_present(RRX86_CPU_BMI2))
		{
			if (rrCPUx86_feature_present(RRX86_CPU_RAPTOR_LAKE))
			{
				ok = tansx2_x64_bmi2_rpl_asm(&s);
			}
			else
			{
				ok = tansx2_x64_bmi2_asm(&s);
			}
		}
		else
		{
			ok = tansx2_x64_asm(&s);
		}
		#elif defined(NEWLZ_ARM64_TANS_ASM)
		ok = tansx2_arm64_asm(&s);
		#elif defined(__RAD64REGS__)
		ok = tansx2_64(&s);
		#else
		ok = tansx2_32(&s);
		//ok = tansx2_finish(&s); // fallback - always works
		#endif
		
		RR_ASSERT_IF_NOT_CORRUPT(ok);
		if ( ! ok )
			NEWLZ_ARRAY_RETURN_FAILURE();
	}

#ifdef DUMP_DECOMP_ARRAYS
	{
		static int ctr = 0;
		char buf[256];
		sprintf(buf, "tans%03d_comp%d.dat", ctr, (int)comp_len);
		if (ctr < 78)
		{
			FILE *f = fopen(buf, "wb");
			if (f)
			{
				fwrite(to, 1, to_len, f);
				fclose(f);
			}
		}
		++ctr;
	}
#endif

	//SINTa comp_len_check = rrPtrDiff(comp_ptr - comp);
	//rrprintfvar(comp_len_check);
		
	return comp_len;
}

OODLE_NS_END
