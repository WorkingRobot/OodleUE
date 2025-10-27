// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.


/***

NEWLZ HC (Leviathan) !!

***/
	
static RADNOINLINE bool newLZHC_decode_parse(
	const newLZHC_chunk_arrays * arrays,
	U8 * to_ptr, U8 * chunk_base, U8 * chunk_end, U8 * window_base )
{
	const S32 * offsets = arrays->offsets;
	const S32 * offsets_end = offsets + arrays->offsets_count;
	const U32 * excesses = arrays->excesses;
	SINTa excesses_count = arrays->excesses_count;
	
	
#if NEWLZHC_DECODE_MULTIPACKET
	SINTa packets_count = arrays->packets_count;
	const U8 * packet_ptrs[NEWLZHC_PACKET_POS_COUNT];
	//memcpy(packet_ptrs,arrays->packet_pos_ptrs,sizeof(packet_ptrs));;

	// subtract off chunk_base modulo in the copy
	// so that packet_ptrs[] is now indexed by ptr & MASK , not pos
	RR_COMPILER_ASSERT( NEWLZHC_PACKET_POS_COUNT == 8 );
	RR_UNROLL_I_8(0, packet_ptrs[i] = arrays->packet_pos_ptrs[ (i - (UINTa)chunk_base) & NEWLZHC_PACKET_POS_MASK ] );
	
#else
	const U8 * packets = arrays->packets;
	const U8 * packets_end = packets + arrays->packets_count;
#endif

	const S32 * offsets_ptr = offsets;
	const U32 * excesses_ptr = excesses;
	const U32 * excesses_end_ptr = excesses + excesses_count;
	
#if defined(NEWLZ_LITERALS_TYPE_SUBAND3) && NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_SUBAND3
	#ifdef NEWLZHC_SUBAND3_VEC_OFFS

	RAD_ALIGN(S32, literals_offs[8], 16);
	const U8 * literals_base = arrays->literals_ptrs[0];

	RR_UNROLL_I_N(4,0, literals_offs[i] = rrPtrDiff32( arrays->literals_ptrs[ (i - (UINTa)chunk_base) & 3 ] - literals_base ) );

	SubAnd3LitOffs v_literals_offs = newlzhc_init_suband3_lit_offs(literals_offs);

	#else

	const U8 * literals_ptrs[4];
	RR_UNROLL_I_N(4,0, literals_ptrs[i] = arrays->literals_ptrs[ (i - (UINTa)chunk_base) & 3 ] );

	#endif
#elif ( defined(NEWLZ_LITERALS_TYPE_O1) && NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_O1 ) 
	const U8 * literals_ptrs_o1[ NEWLZ_O1_CONTEXT_COUNT ];
	memcpy(literals_ptrs_o1,arrays->literals_ptrs,NEWLZ_O1_CONTEXT_COUNT*sizeof(void *));
	U8 next_literals_o1[ NEWLZ_O1_CONTEXT_COUNT ];
	RR_UNROLL_I_N(16,0, next_literals_o1[i] = *literals_ptrs_o1[i]; literals_ptrs_o1[i]++; );
#elif ( defined(NEWLZ_LITERALS_TYPE_O2) && NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_O2 ) 
	const U8 * literals_ptrs[ NEWLZ_O2_CONTEXT_COUNT ];
	memcpy(literals_ptrs,arrays->literals_ptrs,NEWLZ_O2_CONTEXT_COUNT*sizeof(void *));
#elif defined(NEWLZ_LITERALS_TYPE_SUBANDF) && NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_SUBANDF
	const U8 * literals_ptrs[ 16 ];
	//memcpy(literals_ptrs,arrays->literals_ptrs,16*sizeof(void *));
	
	RR_UNROLL_I_N(16,0, literals_ptrs[i] = arrays->literals_ptrs[ (i - (UINTa)chunk_base) & 0xF ] );

#else

	const U8 * literals_ptr = arrays->literals_ptrs[0];
//	SINTa literals_count = arrays->literals_counts[0];

#if NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_LAMSUB
	const U8 * literals_ptr_lam = arrays->literals_ptrs[1];
#endif

#endif // NEWLZHC_DECODE_LITERALS_TYPE

	//=========================================================================
	// verify that scratch fuzz requirements are met
	//	(this has already been checked)
	// the encoder ensures that valid data it makes will satisfy the requirements
	//	but remember we could have got a fuzzy phase1
	//	so array sizes might not make sense for a valid parse
	//	(we check each array size against its valid maximum
	//	 eg. offsets_count is limited to chunk_len/3
	//	 but they can add up to more than any valid data should,
	//	 eg. offsets_count could be chunk_len/3 and excesses_count could be chunk_len/3
	//	  which should never occur together)

	/**
	
	Leviathan decoder scratch arrays layout :

	offsets u32
	excesses u32
	literals
	packets
	[chunk_len padding after packets, can overlap u8 arrays]
	[u8 arrays no longer active in phase2]

	_phase1 decode has checked that the retained arrays are <= 2*chunk_len
	which means there is at least chunk_len padding after packets

	**/

	RR_DURING_ASSERT( U8 * scratch_end = arrays->scratch_end );
	RR_DURING_ASSERT( SINTa chunk_len = rrPtrDiff( chunk_end - chunk_base ) );
	RR_DURING_ASSERT( U8 * scratch_arrays_end = scratch_end - chunk_len ); // chunk_len padding for fuzz
	RR_DURING_ASSERT( scratch_arrays_end; );
	
	// with non-corrupt data, no array should go past "scratch_arrays_end"
	// even with corrupt data, they should not go past "scratch_end"
	
	RR_ASSERT( (U8 *)excesses_ptr >= (U8 *)offsets_ptr );
	
	// you could grab an offset at every packet and not go past scratch :
	RR_ASSERT( (U8 *)(offsets_ptr + arrays->packets_count) < scratch_end );
	// you could grab an excess (lrl,forward) at every packet and not go past scratch :
	//	(ml excesses are grabbed backwards and bounds checked)
	RR_ASSERT( (U8 *)(excesses_ptr + arrays->packets_count) < scratch_end );
				
#if ! NEWLZHC_DECODE_MULTIPACKET
	// packets is the last thing in scratch
	// the scratch space after packets_end should be > chunk_len
	//	eg. packets_end should be <= scratch_arrays_end
	RR_ASSERT( packets >= (U8 *)excesses_end_ptr );
	RR_ASSERT( packets_end < scratch_arrays_end );
#endif
	
	//=========================================================================
	
	// make sure it doesn't cross cache lines:	
	RAD_ALIGN(newLZHC_dec_LOs,lastoffsets,64);
	//newLZHC_dec_LOs lastoffsets;
	newLZHC_dec_LOs_Reset_Neg(lastoffsets);
	SINTa neg_offset = - NEWLZ_MIN_OFFSET;

	{
	SIMPLEPROFILE_SCOPE_N(newLZHC_decode_parse,rrPtrDiff32(chunk_end - to_ptr));

		/**
		
		newlzhc fuzz :
		
		newlzhc fuzz relies on the output buffer check to gaurantee most of the other checks
		
		all the "arrays" from phase1 are forced to be in scratch
		they fit in the first 2*chunk_len of scratch
		scratch mem is actually 3*chunk_len + more
		so scratch has >= chunk_len padding after the arrays
		
		this means that as long as the output buffer is checked (we put <= chunk_len)
		then the arrays cannot read past the end of scratch
		as long as they read <= 1 byte per output byte written
		
		so literals & packets cannot overrun
		
		excesses do 2 front/back pointers
		the forward pointer (LRL excess) cannot overrun
		the backward pointer (ML excess) can in theory, so it needs extra fuzzing
		
		the offsets pointer can advance by 4 for every 3 output
		so the automatic fuzzing doesn't work, it needs an extra check
		
		---
		
		the output checking works via fast/careful loops like Kraken
		
		in the fast loop :
		
		packets on the fast path can proceed with minimal checking
			(lrl <= 24 && ml <= 16)
		groups of 8 packets are tested to have enough room for fast path
		
		when one of them takes a slow path (lrl > 24 || ml > 16)
			then lrl/ml is validated to not overrun
			and the checkpoint group is restarted
		
		---
		
		NOTE : 
		excesses and offsets can overrun their arrays on corrupt data
		they are guaranteed not to overrun so far as to access violate
		but can read garbage
		(excess ML is an exception, it does fine-grain checks for overrunning backwards)
		this is a particular issue in 32-bit builds where the U32 fetch is put into an SINTa (S32)
		that means the sign bit can be garbage
		so "neg_offset" can actually be positive
		and excess lrl can be negative
		
		therefore the code that handles these must be hardened to catch those cases correctly
		
		-> this is currently being prevented for LRL's in 32-bit with VALIDATE_EXCESS_LRL
		so even corrupt lrls are >= 0
		
		the reason it's a little tricky for lrl is that the lrl check is also used for to_ptr bounds checking.
		This :
		
		REQUIRE_FUZZ_RETURN( (match_zone_end - to_ptr) >= (lrl) , false ); \
		
		is assuming lrl >= 0
		and trying to check to_ptr < match_zone_end
		
		**/

		//static const SINTa kPacketsPerCheckpoint = 8;
		static const SINTa kBigLRL = 24; // any LRL longer than this needs to do bounds checks
		static const SINTa kBigML = 16; // any ML longer than this needs to do bounds checks
		//static const SINTa kOutputPerCheckpoint = (kBigLRL + kBigML) * kPacketsPerCheckpoint; // number of out bytes written/checkpoint if there are no big LRLs or MLs
		// kOutputPerCheckpoint = 320

		const U8 * match_zone_end = ptr_sub_saturate(chunk_end, NEWLZHC_CHUNK_NO_MATCH_ZONE, chunk_base);
		const U8 * match_end = chunk_end - NEWLZHC_MATCH_END_PAD; // matches have to end by here
		//const U8 * output_safe_end = ptr_sub_saturate(chunk_end, kOutputPerCheckpoint + NEWLZHC_CHUNK_NO_MATCH_ZONE, chunk_base);
		
		
		// main loop that runs while we're far enough away from the end
		
		{
			// inner internally loops on checkpoint groups
		
			#define NEWLZHC_PARSE_CAREFUL_OUTPUT 0
			#include "newlzhc_decode_parse_inner.inl"
		}

		{
			// careful inner does not internally loop			
			#define NEWLZHC_PARSE_CAREFUL_OUTPUT 1
			#include "newlzhc_decode_parse_inner.inl"
		}
		
		// can do an only-literals block by having zero packets and everything in the final lrl :
		// do final lrl :
		//	be careful of overrun
		if ( to_ptr < chunk_end )
		{
			SINTa lrl = rrPtrDiff( chunk_end - to_ptr );
			RR_ASSERT( lrl > 0 );
			
			// usually 4 or 8 , but sometimes very long :
			// final lrl : 12144
			//rrprintf("final lrl : %d\n",lrl);

#if defined(NEWLZ_LITERALS_TYPE_O1) && NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_O1
			{
				if ( lrl > 0 )
				{					
					newlz_copyliterals_o1(to_ptr,lrl,next_literals_o1,literals_ptrs_o1);
					to_ptr += lrl;
				}
			}
#elif defined(NEWLZ_LITERALS_TYPE_O2) && NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_O2
			{
				if ( lrl > 0 )
				{					
					newlz_copyliterals_o2(to_ptr,to_ptr+neg_offset,lrl,literals_ptrs);
					to_ptr += lrl;
				}
			}
#elif defined(NEWLZ_LITERALS_TYPE_SUBANDF) && NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_SUBANDF
			{
				if ( lrl > 0 )
				{
					newlz_copyliterals_subandF(to_ptr,to_ptr+neg_offset,lrl,literals_ptrs);
					to_ptr += lrl;
				}
			}
#elif defined(NEWLZ_LITERALS_TYPE_SUBAND3) && NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_SUBAND3
			{
				#if 0
				// suband3 reference decoder - SLOW!
							
				while(lrl--)
				{
					SINTa pos = to_ptr - chunk_base;
					U8 lit = *literals_ptrs[pos&3];
					literals_ptrs[pos&3]++;
					*to_ptr = (lit + *(to_ptr+neg_offset));
					to_ptr++;
				}
				
				#else
				
				SINTa pos3 = (UINTa)to_ptr;
				#ifdef NEWLZHC_SUBAND3_VEC_OFFS

				const U8 * lptr0 = literals_base + literals_offs[pos3&3]; RR_DURING_ASSERT( literals_offs[pos3&3] += (S32) ((lrl + 3) >> 2) ); pos3++;
				const U8 * lptr1 = literals_base + literals_offs[pos3&3]; RR_DURING_ASSERT( literals_offs[pos3&3] += (S32) ((lrl + 2) >> 2) ); pos3++;
				const U8 * lptr2 = literals_base + literals_offs[pos3&3]; RR_DURING_ASSERT( literals_offs[pos3&3] += (S32) ((lrl + 1) >> 2) ); pos3++;
				const U8 * lptr3 = literals_base + literals_offs[pos3&3]; RR_DURING_ASSERT( literals_offs[pos3&3] += (S32) ((lrl    ) >> 2) );

				#else

				const U8 * lptr0 = literals_ptrs[pos3&3]; RR_DURING_ASSERT( literals_ptrs[pos3&3] = lptr0 + ((lrl + 3) >> 2) ); pos3++;
				const U8 * lptr1 = literals_ptrs[pos3&3]; RR_DURING_ASSERT( literals_ptrs[pos3&3] = lptr1 + ((lrl + 2) >> 2) ); pos3++;
				const U8 * lptr2 = literals_ptrs[pos3&3]; RR_DURING_ASSERT( literals_ptrs[pos3&3] = lptr2 + ((lrl + 1) >> 2) ); pos3++;
				const U8 * lptr3 = literals_ptrs[pos3&3]; RR_DURING_ASSERT( literals_ptrs[pos3&3] = lptr3 + ((lrl    ) >> 2) );

				#endif
				
				// @@ faster ? who cares ?
				while( lrl >= 4 )
				{
					to_ptr[0] = (U8)( (*lptr0++) + to_ptr[0+neg_offset] );
					to_ptr[1] = (U8)( (*lptr1++) + to_ptr[1+neg_offset] );
					to_ptr[2] = (U8)( (*lptr2++) + to_ptr[2+neg_offset] );
					to_ptr[3] = (U8)( (*lptr3++) + to_ptr[3+neg_offset] );
					to_ptr += 4; lrl -= 4;
				}
				
				// 0-3 more
				if ( lrl > 0 )
				{
					to_ptr[0] = (U8)( (*lptr0++) + to_ptr[0+neg_offset] );
					to_ptr++; lrl--;
				}
				if ( lrl > 0 )
				{
					to_ptr[0] = (U8)( (*lptr1++) + to_ptr[0+neg_offset] );
					to_ptr++; lrl--;
				}
				if ( lrl > 0 )
				{
					to_ptr[0] = (U8)( (*lptr2++) + to_ptr[0+neg_offset] );
					to_ptr++; lrl--;
				}
				
				#endif
			}
#else // RAW, SUB, LAMSUB
			{
				RR_ASSERT_IF_NOT_CORRUPT( newlz_literals_type_offset1_ok(NEWLZHC_DECODE_LITERALS_TYPE) || -neg_offset >= NEWLZ_MIN_OFFSET );
									
				#if NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_RAW

				RR_ASSERT_IF_NOT_CORRUPT( literals_ptr + lrl == arrays->literals_ptrs[0] + arrays->literals_counts[0] );
					
				#if 1
				memcpy(to_ptr,literals_ptr,lrl);
				to_ptr += lrl;
				literals_ptr += lrl;
				#else
				while( lrl >= 64 )
				{
					memcpy(to_ptr,literals_ptr,64);
					to_ptr += 64;
					literals_ptr += 64;
					lrl -= 64;
				}
				while( lrl >= 8 )
				{
					lz_copy8(to_ptr,literals_ptr);
					to_ptr += 8;
					literals_ptr += 8;
					lrl -= 8;
				}
				while( lrl--)
				{
					*to_ptr++ = *literals_ptr++;
				}
				#endif

				/**/
				#else // SUB AND LAMSUB

				#if NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_LAMSUB
				{
					U8 sub = *literals_ptr_lam++;
					*to_ptr = (U8)(sub + to_ptr[neg_offset]);
					to_ptr++;
					lrl--;
				}		
				#endif
				
				RR_ASSERT_IF_NOT_CORRUPT( literals_ptr + lrl == arrays->literals_ptrs[0] + arrays->literals_counts[0] );
				
				while( lrl >= 8 )
				{
					newlz_copyadd8(to_ptr,to_ptr+neg_offset,literals_ptr);
					
					CHECK( RR_ASSERT( memcmp(to_ptr,check_ptr,RR_MIN(8,lrl)) == 0 ); RR_DURING_ASSERT( check_ptr += RR_MIN(8,lrl) ) );
					
					to_ptr += 8;
					literals_ptr += 8;
					lrl -= 8;
				}

				while( lrl--)
				{
					*to_ptr = (U8)(*literals_ptr++ + to_ptr[neg_offset]);			
					to_ptr++;
				}

				#endif // RAW vs SUB
			}
#endif // NEWLZHC_DECODE_LITERALS_TYPE
		} // final LRL copy
	
	} // profile scope ; parse loop	

	// check that we hit output size :
	REQUIRE_FUZZ_RETURN( to_ptr == chunk_end , false);
	
	//-----------------------------------------------------------
	// just debug checking that we consumed all arrays :

	// @@!! fuzz - could make this runtime checking for a stronger corruption test
	//	 but that is optional, garbage in - garbage out is okay.
	
	// check whether we consumed all our input
	REQUIRE_FUZZ_RETURN( offsets_ptr == offsets_end , false);
	REQUIRE_FUZZ_RETURN( excesses_ptr == excesses_end_ptr , false);
	
#ifdef RR_DO_ASSERTS
			
#if NEWLZHC_DECODE_MULTIPACKET
	for LOOP(i,NEWLZHC_PACKET_POS_COUNT)
	{
		RR_ASSERT_IF_NOT_CORRUPT( packet_ptrs[i] == arrays->packet_pos_ends[(i - (UINTa)chunk_base)&NEWLZHC_PACKET_POS_MASK] );
	}
#else
	RR_ASSERT_IF_NOT_CORRUPT( packets == arrays->packets + arrays->packets_count );
#endif
	
#if defined(NEWLZ_LITERALS_TYPE_SUBAND3) && ( NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_SUBAND3 )
	{
		for(int i=0;i<4;i++)
		{
			UINTa src = (i - (UINTa)chunk_base)&3; src;
			#ifdef NEWLZHC_SUBAND3_VEC_OFFS
			RR_ASSERT_IF_NOT_CORRUPT( literals_base + literals_offs[i] == arrays->literals_ptrs[src] + arrays->literals_counts[src] );
			#else
			RR_ASSERT_IF_NOT_CORRUPT( literals_ptrs[i] == arrays->literals_ptrs[src] + arrays->literals_counts[src] );
			#endif
		}
	}
#endif
#if defined(NEWLZ_LITERALS_TYPE_SUBANDF) && ( NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_SUBANDF )
	{
		for(int i=0;i<16;i++)
		{
			UINTa src = (i - (UINTa)chunk_base)&0xF; src;
			RR_ASSERT_IF_NOT_CORRUPT( literals_ptrs[i] == arrays->literals_ptrs[src] + arrays->literals_counts[src] );
		}
	}
#endif
#if ( defined(NEWLZ_O1_CONTEXT_COUNT) && ( NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_O1 ) )
	{
		for(int i=0;i<NEWLZ_O1_CONTEXT_COUNT;i++)
		{
			RR_ASSERT_IF_NOT_CORRUPT( literals_ptrs_o1[i] == 1 + arrays->literals_ptrs[i] + arrays->literals_counts[i] );
		}
	}
#endif
#if ( defined(NEWLZ_O2_CONTEXT_COUNT) && ( NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_O2 ) )
	{
		for(int i=0;i<NEWLZ_O2_CONTEXT_COUNT;i++)
		{
			RR_ASSERT_IF_NOT_CORRUPT( literals_ptrs[i] == arrays->literals_ptrs[i] + arrays->literals_counts[i] );
		}
	}
#endif
#if defined(NEWLZ_LITERALS_TYPE_LAMSUB) && ( NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_LAMSUB )
	RR_ASSERT_IF_NOT_CORRUPT( literals_ptr_lam == arrays->literals_ptrs[1] + arrays->literals_counts[1] );
	RR_ASSERT_IF_NOT_CORRUPT( literals_ptr == arrays->literals_ptrs[0] + arrays->literals_counts[0] );
#endif

#if ( NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_RAW || NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_SUB )
	RR_ASSERT_IF_NOT_CORRUPT( literals_ptr == arrays->literals_ptrs[0] + arrays->literals_counts[0] );
#endif

#endif // RR_DO_ASSERTS

	//-----------------------------------------------------------
		
	return true;
}
