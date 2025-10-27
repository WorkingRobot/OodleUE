// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.


RADNOINLINE const U8 * newLZF_decode_parse(
	U8 * chunk_ptr, SINTa chunk_len, U8 * overrun_chunk_end,
	U8 * window_base,
	const U8 * comp_end,
	newLZF_chunk_arrays * arrays,
	S32 * p_neg_offset
	)
{
	//RR_SIMPLEPROF_SCOPE(newLZF_decode_parse);
	//RR_DO_MACRO( SIMPLEPROFILE_SCOPE , newLZF_decode_parse );

	const U8 * literals_ptr = arrays->literals_ptr; 
	const U8 * literals_end = arrays->literals_end;
			
	S32 neg_offset = *p_neg_offset;

	SINTa start_pos = NEWLZF_DECODE_FIRST_CHUNK ? NEWLZF_MIN_OFFSET : 0;

	U8 * to_ptr = chunk_ptr + start_pos;
		
	// packets_count can be zero
	const U8 * packets_ptr = arrays->packets_ptr;
	const U8 * packets_end = arrays->packets_end;
	const U8 * packets_end16 = packets_end-15;
	
	// chunk_ptr is the 64k parse chunk base
	// chunk_len is parse chunk len
	// makes this the *parse* chunk end pointer
	U8 * parse_chunk_end = chunk_ptr + chunk_len;
	
	const U8 * literals_end_save = literals_end;
	const U8 * off16_ptr = arrays->off16_ptr;
	const U8 * off16_end = arrays->off16_end;
	const U8 * off16_end_save = off16_end;
	
	const U8 * excesses_ptr = arrays->excesses_ptr;
	const U8 * excesses_end = arrays->excesses_end;
	
	#if !NEWLZF_DECODE_FIRST_CHUNK
	const U32 * escape_offset_ptr = arrays->escape_offsets_ptr;
	const U32 * escape_offset_end = arrays->escape_offsets_end;
	#endif
	
	#if 0
	// prefetch the first escape_offset
	//	if there are none, escape_offset_ptr is zero
	// -> seems to be meh or slightly negative
	//	just let the first few miss cache instead of doing this?
	for(int i=0;i<=NEWLZF_ESCAPE_PREFETCH_INDEX;i++)
	{
		RR_ASSERT( i < escape_offset_count || escape_offset_ptr[i] == 0 );
		const U8 *  mp = chunk_ptr - escape_offset_ptr[i];		
		RR_PREFETCHR_CL(mp);
	}
	#endif
	
	#ifdef __RADSSE2__
	__m128i all_23 = _mm_set1_epi8(23);
	#endif
	
	/**
	
	newlzf fuzz safety :
	
	fast loop does (up to) 16 packets at a time
	
	the checkpoint ensures that we can do 16 simple packets without testing
	simple packets can :
	  consume 2 bytes from comp_ptr for offset
	  consume 7 literals from literals_ptr
	  output 22 bytes to to_ptr
	
	comp_ptr and literals_ptr reads are protected by switching to padded scratch space
		near the end of their buffers
	to_ptr is checked for enough room to do 16 packets, and when it runs out
		we bail out to a safe loop
		
	the to_ptr end check is vs "overrun_chunk_end" which is the end of the BLOCK (256k)
		parse_chunk_end is the end of the current 64k parse chunk
		so the first 3 parse chunks in a block will not hit the to_ptr safety condition
	
	the "safe" loop uses the same scratch mechanism for comp_ptr and literals_ptr reads
	it checks to_ptr for overrun on every packet
	
	packets_ptr reads are protected by doing 16 at a time while there are >= 16 remaining
	then switching to the safe loop which is 1 at a time
	
	long escapes (packet <= 2) always do full range checks within their handler
	  and force a checkpoint when they are done
	
	There are two possibilities for short escapes (packet in 3-23, ml in 10-30) :
	
	1. They could not cause a checkpoint, in which case kMaxBytesOutPerCheck should be 30*16
		you only need to checkpoint after 16 packets (or a long escape)
		
	2. They could just go ahead and checkpoint after any escape
		so you checkpoint after 16 simple packets or any escape
		
	#2 = less fuzz checks
	#1 = works better with the current loop logic, which bails after any escape packet
	currently #1 is better but this might merit revisitation
	
	offset validation is done thusly :
	
	In the first 64k parse chunk :
		all 16-bit offsets are range checked
		long offsets should not occur, because matches before chunk_start don't exist
	In later chunks :
		16-bit offsets don't need to be checked
		long offsets are checked in the offset unpacker
		escape packet range checks the offset index when it fetches
	
	
	**/

	enum { kPacketsPerCheck = 16 };
	enum { kMaxBytesOutPerUncheckedPacket = 30 }; // short escapes
	enum { kMaxBytesOutPerSimplePacket = 22 }; //7+15 = 22
	// if short escapes don't trigger checkpoint :
	//enum { kMaxBytesOutPerCheck = kPacketsPerCheck*kMaxBytesOutPerPacket };
	// if any escape triggers checkpoint :
	enum { kMaxBytesOutPerCheck = (kPacketsPerCheck-1)*kMaxBytesOutPerSimplePacket + kMaxBytesOutPerUncheckedPacket };
	enum { kMaxLiteralsPerCheck = 7*(kPacketsPerCheck-1)+8 }; // we copy lits 8B at a time, so account for a full 8B read on final packet
	enum { kMaxOff16BytesPerCheck = 2*kPacketsPerCheck }; // +3 for escape getv

	//const U8 * packets_checkpoint = RR_MIN( packets_ptr + kPacketsPerCheck, packets_end16 );

	U8 literal_scratch[kMaxLiteralsPerCheck*2];
	U8 off16_scratch[kMaxOff16BytesPerCheck*2];

	//if ( packets_count > 0 )
	{
		//===============================================
		const U8 * literals_check;
		const U8 * off16_check;

		RR_ASSERT( literals_ptr <= literals_end ); // @@@@ ?
		
		{
		//SIMPLEPROFILE_SCOPE(P2_prep);	

		// copy the last few literals over (might as well do this now)
		{
			SINTa literals_count = rrPtrDiff( literals_end - literals_ptr );
			SINTa num_copy = RR_MIN(kMaxLiteralsPerCheck, literals_count);
			RR_ASSERT( num_copy >= 0 );
			literals_check = literals_end - num_copy;
			memcpy(literal_scratch + kMaxLiteralsPerCheck - num_copy, literals_check, num_copy);
			memset(literal_scratch + kMaxLiteralsPerCheck, 0, kMaxLiteralsPerCheck); // clear second half (for brownie points)
		}

		RR_ASSERT( off16_ptr <= off16_end ); // @@@@ ?

		{
			SINTa off16_count = rrPtrDiff( off16_end - off16_ptr );
			SINTa num_copy = RR_MIN(kMaxOff16BytesPerCheck, off16_count);
			RR_ASSERT( num_copy >= 0 );
			off16_check = off16_end - num_copy;
			memcpy(off16_scratch + kMaxOff16BytesPerCheck - num_copy, off16_check, num_copy);
			memset(off16_scratch + kMaxOff16BytesPerCheck, 0, kMaxOff16BytesPerCheck);
		}
			
		#define CHECK_LITERALS_SAFE() do { \
		if ( literals_ptr >= literals_check ) { \
			REQUIRE_FUZZ_RETURN( ( literals_ptr <= literals_end ) , NULL ); \
			literals_ptr = (literal_scratch + kMaxLiteralsPerCheck) - (literals_end - literals_ptr); \
			literals_end = literals_check = literal_scratch + kMaxLiteralsPerCheck; \
			} \
		} while (0)
		
		#define CHECK_OFF16_SAFE() do { \
		if ( off16_ptr >= off16_check ) { \
			REQUIRE_FUZZ_RETURN( ( off16_ptr <= off16_end ) , NULL ); \
			off16_ptr = (off16_scratch + kMaxOff16BytesPerCheck) - (off16_end - off16_ptr); \
			off16_end = off16_check = off16_scratch + kMaxOff16BytesPerCheck; \
			} \
		} while (0)
		
		#define NEWLZF_TOPTR_OVERRUN_SAFE	0
		// in this loop to_ptr overrun is protected by the kMaxBytesOutPerCheck pad
		//	so no check per-packet is needed (except in the long escapes)
		
		}

		{
		//SIMPLEPROFILE_SCOPE(P2_core);		

		// loop while there are >= 16 packets
		for(;;)
		{
			// very unpredictable branch against checkpoint
			// better to just do it every time
			// -> this hurts on webster (lots of short escapes)
			// -> helps on lzt99
			//if ( packets_ptr >= packets_checkpoint )
			if ( 1 )
			{				
				if ( packets_ptr >= packets_end16 )
					break;
				
				// set next checkpoint :
				//packets_checkpoint = RR_MIN( packets_ptr + kPacketsPerCheck, packets_end16 );
				
				// do fuzz checks
				
				// output pointer :
				if ( to_ptr+kMaxBytesOutPerCheck > overrun_chunk_end )
					break;			
					
				// literals_ptr
				// comp_ptr
				CHECK_LITERALS_SAFE();
				CHECK_OFF16_SAFE();
			}
		
			RR_ASSERT( packets_ptr + 16 <= packets_end );
			
			#ifdef __RADSSE2__ // SSE4 is here
	
			// load 16 packets
			//	and compare them all against the escape threshold
			
			__m128i packets_16 = _mm_loadu_si128((__m128i *)packets_ptr);
			// compare < 24 :
			__m128i cmp_16 = _mm_cmpeq_epi8(all_23, _mm_max_epu8(all_23,packets_16) );
			U32 cmp_mask = _mm_movemask_epi8(cmp_16);
			
			if ( cmp_mask == 0 )
			{
				// do 16 !
				NEWLZF_SIXTEEN_SIMPLE_PACKETS();
			}
			else
			{
				// at least one escape
				// count the # of simple packets, do them, then the 1 escape
				int num_simple = rrCtz32(cmp_mask);
				RR_ASSERT( num_simple >= 0 && num_simple < 16 );
				
				while(num_simple >= 4) // @@ if ?
				{
					num_simple -= 4;
					
					NEWLZF_FOUR_SIMPLE_PACKETS();
				}
								
				while(num_simple--)
				{
					int packet = *packets_ptr++;
					NEWLZF_ONE_SIMPLE_PACKET(packet);
				}
								
				//-------------------------------
				// now one escape packet :
				int packet = *packets_ptr++;				
				RR_ASSERT( packet < 24 );

				#include "newlzf_escape_packet.inl"
			}
			
			#elif defined(__RADARM64__)

			// scalar-SIMD using U64's
			// this is a win on x64 (over non-simd-scalar) , but of course SSE is better there

			// -> big win on the Cortex-A57, so enabled there
			// solid win on Linux ARM RockPro64 (A72) as well
			// and also solid win on current (as of 2020/2021) Apple devices

			/**
			
			scalar-SIMD loop
			uses scalar SIMPLE_PACKET copies
			uses scalar to count simple packets up to the next escape
			
			**/
				
			// scalar simd :
			
			// have >= 16 packets, 
			// can do two loops of 8

			// load 8 packets
			//	and compare them all against the escape threshold
			
			// escape is < 24

			// Going from LSB to MSB:
			// As long as we have a run of bytes >=24, the subtract did nothing
			// interesting. The first byte <24 generates a borrow that ends up setting
			// the MSB in that byte. The results for higher-numbered bytes above that
			// are wrong, but we don't care about those. (This is also why we need to
			// use a LE read here: since we care not just about whether there is an
			// escape byte but also how long the run of non-escapes leading up to it
			// is, we want the carry chain to go in increasing byte order.)
			//
			// So now we have a bunch of bytes where the MSB got set if the original
			// value was <24. However, the MSB will also be set if the original value
			// was >=128+24, so also >=128 (>= 24, so such bytes can never be an escape).
			//
			// Therefore, we now AND with & ~packets_8 to exclude packets that already
			// had their MSB set before we started. After that, we just mask with
			// 0x8080...8080. The resulting uint64 lets us infer if there are any
			// escape packets, and if so, how long a run of non-escapes we get until
			// we hit it.
			//
			// (I've exhaustively tested the 32-bit version of this logic, including
			// counting the length of the resulting runs, to make sure this is right.)

			U64 packets_8a = RR_GET64_LE(packets_ptr);
			U64 cmp_mask = packets_8a - 0x1818181818181818ULL; // - splat_byte(24)
			
			cmp_mask &= ~packets_8a; // exclude packets >=128 (see notes above)
			cmp_mask &= 0x8080808080808080ULL; // just the MSBs

			// maybe rewrite to get the two packets at once (for LDP)
			// then do a joint test?

			if ( cmp_mask == 0 )
			{
				U64 packets_8b = RR_GET64_LE(packets_ptr+8);

				// check 8 more :
				cmp_mask = packets_8b - 0x1818181818181818ULL; // - splat_byte(24)
				
				cmp_mask &= ~packets_8b; // exclude packets >=128 (see notes above)
				cmp_mask &= 0x8080808080808080ULL; // just the MSBs
			
				// do 8 !
				NEWLZF_EIGHT_SIMPLE_PACKETS_SCALAR_PRELOADED(packets_8a);

				if ( cmp_mask == 0 )
				{
					// do 8 !
					NEWLZF_EIGHT_SIMPLE_PACKETS_SCALAR_PRELOADED(packets_8b);

					// did 16, now checkpoint
					continue;
				}
			}

			// at least one escape in this 8

			/*
			// count the # of simple packets, do them, then the 1 escape
			
			int num_simple = rrCtzBytes64(cmp_mask);
			RR_ASSERT( num_simple >= 0 && num_simple < 8 );
			
			#if 0
			// optional check for 4
			if (num_simple >= 4)
			{
				num_simple -= 4;
				
				NEWLZF_FOUR_SIMPLE_PACKETS_SCALAR();
			}
			#endif
							
			while(num_simple--)
			{
				int packet = *packets_ptr++;
				NEWLZF_ONE_SIMPLE_PACKET(packet);
			}
					
			//-------------------------------
			// now one escape packet :
			int packet = *packets_ptr++;				
			RR_ASSERT( packet < 24 );

			#include "newlzf_escape_packet.inl"
			*/

			// may as well just do the branch on packet
			//   using cmp_mask does nothing beneficial
			int packet = *packets_ptr++;		
			while( packet >= 24 )
			{
				NEWLZF_ONE_SIMPLE_PACKET(packet);
				packet = *packets_ptr++;
			}

			#include "newlzf_escape_packet.inl"
			
			// end scalar simd
			#else
		
			// fallback (not SSE, not scalar simd)
			// used on non-x86 32b targets
		
			// do 16 packets
			for(int packet_i=0;packet_i<16;packet_i++)
			{
				int packet = *packets_ptr++;
			
				if ( packet >= 24 )
				{
				#if 1
					NEWLZF_ONE_SIMPLE_PACKET(packet);
				#else 
					// debug mode :
				
					RR_ASSERT( packet >= 24 ); 
					int lrl = packet & 0x7; 
					int ml = (packet >> 3) & 0xF; 
					S32 next_offset = -(S32) RR_GET16_LE(off16_ptr); 
					S32 offset_mask = (S32)(packet>>7)-1; 
					newlz_literals_copy8<NEWLZF_DECODE_LITERALS_TYPE>(to_ptr,to_ptr+neg_offset,literals_ptr); 
					neg_offset ^= (next_offset ^ neg_offset) & offset_mask; 
					off16_ptr += offset_mask&2; 
					
					//rrprintf("DEC : %4d : %3d , %3d , %d+%7d\n",
					//	rrPtrDiff32(to_ptr - window_base),lrl,ml,packet>>7,-neg_offset);
					
					to_ptr += lrl; literals_ptr += lrl; 
					const U8 * mp = to_ptr + neg_offset; 
					NEWLZF_FIRST_CHUNK_FUZZ( mp >= window_base ); 
					newlzf_copy16(to_ptr,mp); 
					to_ptr += ml; 
		
				#endif
				}
				else
				{
					#include "newlzf_escape_packet.inl"
					
					// need to re-checkpoint after escape packets :
					break;
				}
			}
					
			#endif	
			
		}
		}

		//SIMPLEPROFILE_SCOPE(P2_tail);		

		// to_ptr was validated against overrun_chunk_end
		//	check against tighter parse_chunk_end now :
		REQUIRE_FUZZ_RETURN( (to_ptr <= parse_chunk_end), NULL );

		#undef NEWLZF_TOPTR_OVERRUN_SAFE
		#define NEWLZF_TOPTR_OVERRUN_SAFE	1
		// don't have kMaxBytesOutPerCheck padding here
		//	so need to check to_ptr overrun on every packet

		// @@ todo : quite often I have packets_count much > 16 here still
		//	it's usually the to_ptr out of room that makes me break out
		//	so I could still do the 16-at-a-time loop a few times
		//	but with NEWLZF_TOPTR_OVERRUN_SAFE enabled

		/*
		{
			int np = packets_end - packets_ptr;
			int bytes = parse_chunk_end - to_ptr;
			rrprintfvar(np);
			rrprintfvar(bytes);
		}
				
		// first three 64k parse chunks in a 256k block
		//	can get here with np < 16 because overrun_chunk_end is at the 256k boundary
		// last piece gets here with a pretty high packet count
		//  for example :
		np : 4
		bytes : 36
		np : 8
		bytes : 72
		np : 15
		bytes : 104
		np : 94
		*/

		// tail
		// need to_ptr end-safety
		
		// do a checkpoint right away :
		const U8 * packets_checkpoint;
		packets_checkpoint = packets_ptr + kPacketsPerCheck;		
		CHECK_LITERALS_SAFE();
		CHECK_OFF16_SAFE();
				
		while( packets_ptr < packets_end )
		{
			// still need packet checkpoints for literals & comp_ptr
			// but no longer checking packets & to_ptr
			if ( packets_ptr >= packets_checkpoint )
			{				
				// set next checkpoint :
				packets_checkpoint = packets_ptr + kPacketsPerCheck;
				
				// do fuzz checks				
				CHECK_LITERALS_SAFE();
				CHECK_OFF16_SAFE();
			}
			
			int packet = *packets_ptr++;
			
			if ( packet >= 24 )
			{
				NEWLZF_ONE_SIMPLE_PACKET_TO_PTR_SAFE(packet);
			}
			else
			{						
				#include "newlzf_escape_packet.inl"
				
				// @@ if I have packets_checkpoint updates in escape.inl then don't need this
				//packets_checkpoint = packets_ptr;
				// do checkpoint :
				packets_checkpoint = packets_ptr + kPacketsPerCheck;
				
				// do fuzz checks				
				CHECK_LITERALS_SAFE();
				CHECK_OFF16_SAFE();
			}
		}

		#undef NEWLZF_TOPTR_OVERRUN_SAFE
						
	} // packet_count == 0

	// can do an only-literals block by having zero packets and everything in the final lrl :
	// do final lrl :
	if ( to_ptr < parse_chunk_end )
	{
		SINTa lrl = rrPtrDiff( parse_chunk_end - to_ptr );
		
		REQUIRE_FUZZ_RETURN( (literals_ptr+lrl <= literals_end) , NULL );
		
		// final tail
		//	be careful of overrun
		
		#if NEWLZF_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_RAW
		{
			memcpy(to_ptr,literals_ptr,lrl);
			to_ptr += lrl;
			literals_ptr += lrl;
		}
		#elif NEWLZF_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_SUB
		{				
			while( lrl >= 8 )
			{
				newlz_literals_copy8<NEWLZF_DECODE_LITERALS_TYPE>(to_ptr,to_ptr+neg_offset,literals_ptr);
				
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
		}
		#else
		{
			//RR_CANT_GET_HERE();
			#error NEWLZF_DECODE_LITERALS_TYPE
		}
		#endif
	}
	else if ( to_ptr > parse_chunk_end )
	{
		return NULL;
	}
		
	RR_ASSERT( to_ptr == chunk_ptr + chunk_len );

	// these may have overrun inside the scratch
	//	if they do it's a fail :
	
	if ( off16_ptr > off16_end || literals_ptr > literals_end )
		return NULL;

	// on the first 64k chunk, off16 and literals will not be == end
	//	on the final 64k they should wind up exactly == end
	// -> this is now checked outside

	*p_neg_offset = neg_offset;

	// get the end pointers right even if we switched to a scratch buffer
	//	if did switch, then the _end pointer is also switched,
	//	so we can use that to count back from the original end
	arrays->literals_ptr = literals_end_save - (literals_end - literals_ptr);	
	arrays->off16_ptr = off16_end_save - (off16_end - off16_ptr);
	
	// newlzf_getv won't read from excesses_ptr if it passes end
	//	but it could have advanced there
	if ( excesses_ptr > excesses_end )
		return NULL;
	
	arrays->excesses_ptr = excesses_ptr;
	
	return comp_end;
}
