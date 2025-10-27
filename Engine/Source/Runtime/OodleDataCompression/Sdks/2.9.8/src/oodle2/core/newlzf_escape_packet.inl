// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.


{
	RR_ASSERT( packet < 24 );
	
	// special escape packet
	// hidden in the zone of "send an offset + ML < MML (3)"
	//  which is (0<<7) + (3<<3) == 24

	// if_unlikely does help on Jaguar here
	if_unlikely ( packet <= 2 ) // long LRL or long ML
	{
		if_likely ( packet == 0 ) // LRL escape
		{				
			// I get no win here
			//RR_PREFETCHR_CL(literals_ptr + RR_CACHE_LINE_SIZE);

			SINTr lrl = newlzf_getv(excesses_ptr,excesses_end);
			// I sent (lrl - NEWLZF_LRL_EXCESS) in raw bytes
			//	but PACKET_LRL_MAX was also sent in a normal packet lrl, so take that off
			lrl += NEWLZF_LRL_EXCESS;
			
			// fuzz check :
			
			// parse_chunk_end is the 64k sub-chunk
			//	 in the first parse chunk, LRL can go all the way to the end
			//	(NEWLZF_CHUNK_MATCH_SAFE_ZONE doesn't apply to first parse chunk)
			// overrun_chunk_end is the end of the 128k chunk
			//	 NEWLZF_CHUNK_MATCH_SAFE_ZONE does apply there
			
			// to_ptr+lrl <= parse_chunk_end should always be true
			// but you can overrun that because of the 16 byte copies
			//	(that's allowed on first parse chunk)
			// to_ptr+lrl+NEWLZF_CHUNK_MATCH_SAFE_ZONE <= overrun_chunk_end
			//	should also be true 
			
			// to_ptr has the guaranteed NEWLZF_CHUNK_MATCH_SAFE_ZONE
			//  so we could just check for that and bail
			// BUT literals does NOT
			//	we would have to check for that and handle it			
			// so instead use the end-aligned copy method
			
			REQUIRE_FUZZ_RETURN( (lrl <= rrPtrDiff(parse_chunk_end-to_ptr)) , NULL );
			REQUIRE_FUZZ_RETURN( (lrl <= rrPtrDiff(literals_end-literals_ptr)) , NULL );
			
			// NEWLZF_LRL_EXCESS is 64
			// so we know we're in long LRL
			
			// could do 64 up front -> meh
			// could unroll to take larger steps than 8 at a time -> meh
			
			CHECK( check_ptr += lrl );
			
			// NEWLZF_CHUNK_MATCH_SAFE_ZONE protects 16-byte copies here
			
			// we do these copies with the last quantum aligned to the end
			//	so the last may back up and overlaps a bit
			// this prevents overrunning output & input both
			
			#if NEWLZF_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_SUB
			{
				// sub literals, need to check offset if you want to do longer copies
						
				// do 8 :
				do
				{				
					newlz_literals_copy8<NEWLZF_DECODE_LITERALS_TYPE>(to_ptr,to_ptr+neg_offset,literals_ptr);
				
					to_ptr += 8;
					literals_ptr += 8;
					lrl -= 8;	
				}
				while ( lrl > 8 );
				
				// go to end :
				to_ptr += lrl;
				literals_ptr += lrl;
				
				// do last 8 :
				newlz_literals_copy8<NEWLZF_DECODE_LITERALS_TYPE>(to_ptr-8,to_ptr-8+neg_offset,literals_ptr-8);		
			}
			#else
			{
				// raw literals, no need to check offset
									
				// do 16 at a time :
				do
				{				
					lz_copy16(to_ptr,literals_ptr);
				
					to_ptr += 16;
					literals_ptr += 16;
					lrl -= 16;	
				}
				while ( lrl > 16 );
				
				// go to end :
				to_ptr += lrl;
				literals_ptr += lrl;
				
				// do last 16 :
				lz_copy16(to_ptr-16,literals_ptr-16);
			}
			#endif
			
							
			
			// force a checkpoint:
			//packets_checkpoint = packets_ptr;
		}
		else if ( packet == 1 )
		{
			// packet == 1
			// long ML , off 16
			
			SINTr ml = NEWLZF_ML_EXCESS + newlzf_getv(excesses_ptr,excesses_end);
						
			// fuzz check :
			// +8 for lz_copy8
			//	we actually are guaranteed 16 by NEWLZF_CHUNK_MATCH_SAFE_ZONE
			//	valid streams should never cross parse_chunk_end
			//	overruning parse_chunk_end is allowed (due to 8-byte copies)
			//	overruning overrun_chunk_end has the NEWLZF_CHUNK_MATCH_SAFE_ZONE
			// require :
			// to_ptr_end <= parse_chunk_end
			// to_ptr_end+8 <= overrun_chunk_end
			if ( ml+8 > rrPtrDiff(parse_chunk_end-to_ptr) )
			{
				REQUIRE_FUZZ_RETURN( ( ml <= rrPtrDiff(parse_chunk_end-to_ptr) ) , NULL );
				REQUIRE_FUZZ_RETURN( ( ml+8 <= rrPtrDiff(overrun_chunk_end-to_ptr) ) , NULL );
			}
			
			U8 * to_ptr_end = to_ptr + ml;
			
			//-----------
			// 16-bit offset
			
			S32 offset = RR_GET16_LE(off16_ptr);
			off16_ptr += 2;
						
			neg_offset = (S32) - offset;
			RR_ASSERT_IF_NOT_CORRUPT( neg_offset <= -NEWLZF_MIN_OFFSET );

			const U8 * mp = to_ptr + neg_offset;
			
			// 16-bit offset in first chunk - 
			NEWLZF_FIRST_CHUNK_FUZZ( mp >= window_base );
		
			RR_COMPILER_ASSERT( NEWLZF_ML_EXCESS > 64 );
			
			//do 64 :
			RR_UNROLL_I_8(0, lz_copy8(to_ptr+i*8,mp+i*8); );
			
			to_ptr += 64; mp += 64;
				
			lz_copy8steptoend(to_ptr,mp,to_ptr_end);
			
			to_ptr = to_ptr_end;
			
			// force a checkpoint:
			//packets_checkpoint = packets_ptr;
		}
		else
		{
			#if NEWLZF_DECODE_FIRST_CHUNK
			// long offset in first chunk, fuzz failure
			return NULL;
			#else
			// packet == 2
			// long ML , off 24
			
			SINTr ml = 21 + NEWLZF_OFF24_MML_DECODE + newlzf_getv(excesses_ptr,excesses_end);
						
			//-----------
			// some code dupe with the following off24 case :
			
			REQUIRE_FUZZ_RETURN( (escape_offset_ptr < escape_offset_end) , NULL );
			
			const U8 *  mp = chunk_ptr - escape_offset_ptr[0];
			escape_offset_ptr++;

			RR_ASSERT( mp >= window_base ); // should be assured by offset unpacker
		
			neg_offset = (S32)( mp - to_ptr );
			RR_ASSERT_IF_NOT_CORRUPT( neg_offset <= -NEWLZF_MIN_OFFSET );

			// fuzz check
			// similar to above, but +16 here instead of +8
			// (+16 = NEWLZF_CHUNK_MATCH_SAFE_ZONE is as far as we can push it
			// and still accept all legal bitstreams)
			if ( ml+16 > rrPtrDiff(parse_chunk_end-to_ptr) )
			{
				REQUIRE_FUZZ_RETURN( ( ml <= rrPtrDiff(parse_chunk_end-to_ptr) ) , NULL );
				REQUIRE_FUZZ_RETURN( ( ml+16 <= rrPtrDiff(overrun_chunk_end-to_ptr) ) , NULL );
			}
			
			U8 * to_ptr_end = to_ptr + ml;
					
			// ml >= 29 ; I can go 16 past that (because of +16 above)
			RR_COMPILER_ASSERT( 21 + NEWLZF_OFF24_MML_DECODE >= 29 );

			//do 40 :
			lz_copy8(to_ptr,mp);
			lz_copy8(to_ptr+8,mp+8);
			lz_copy8(to_ptr+16,mp+16);
			lz_copy8(to_ptr+24,mp+24);
			lz_copy8(to_ptr+32,mp+32);
			//to_ptr += ml;
			
			if ( ml > 40 )
			{
				to_ptr += 40; mp += 40;
				
				lz_copy8steptoend(to_ptr,mp,to_ptr_end);
			}
			to_ptr = to_ptr_end;
			
#ifdef NEWLZF_ESCAPE_PREFETCH_INDEX
			// prefetch next :
			mp = chunk_ptr - escape_offset_ptr[NEWLZF_ESCAPE_PREFETCH_INDEX];
			RR_PREFETCHR_CL(mp);
#endif
			
			// force a checkpoint:
			//packets_checkpoint = packets_ptr;
			#endif
		}
	}
	else
	{
		#if NEWLZF_DECODE_FIRST_CHUNK
		// long offset in first chunk, fuzz failure
		return NULL;
		#else
		// 3 byte offset , long match, but not unlimited length or EXCESS ML case
		RR_ASSERT( packet >= 3 && packet <= 23 );
		
		SINTr ml = packet - 3 + NEWLZF_OFF24_MML_DECODE;

		#define NEWLZF_OFF24MEDIUM_MAX_ML (23 + NEWLZF_OFF24_MML_DECODE - 3)
		
		// short escape case
		//	I do not check to_ptr+ml vs. end , so length must be <= kMaxBytesOutPerUncheckedPacket
		RR_COMPILER_ASSERT( NEWLZF_OFF24MEDIUM_MAX_ML <= kMaxBytesOutPerUncheckedPacket );
		RR_COMPILER_ASSERT( kMaxBytesOutPerUncheckedPacket <= 32 );
		RR_ASSERT( ml <= kMaxBytesOutPerUncheckedPacket );
		
		
		//-----------
		// some code dupe with the preceding off24 case :
			
		REQUIRE_FUZZ_RETURN( (escape_offset_ptr < escape_offset_end) , NULL );
				
		const U8 *  mp = chunk_ptr - escape_offset_ptr[0];
		escape_offset_ptr++;

		RR_ASSERT( mp >= window_base ); // should be assured by offset unpacker

		neg_offset = (S32)( mp - to_ptr );
		RR_ASSERT_IF_NOT_CORRUPT( neg_offset <= -NEWLZF_MIN_OFFSET );
		
		#if NEWLZF_TOPTR_OVERRUN_SAFE
		// Coarse pre-check: does max ML fit, with up to 8 bytes slack from writing past the end?
		if ( (NEWLZF_OFF24MEDIUM_MAX_ML + 8) > rrPtrDiff(parse_chunk_end - to_ptr) )
		{
			// If it doesn't, need to be careful:
			// 1. Check that match ends inside this parse chunk
			REQUIRE_FUZZ_RETURN( ml <= rrPtrDiff(parse_chunk_end - to_ptr) , NULL );
			// 2. Check that we have 16 bytes of overrun space (=NEWLZF_CHUNK_MATCH_SAFE_ZONE)
			// past the end of this match.
			// This means we can copy up to 16 bytes past the end of the match, used below.
			REQUIRE_FUZZ_RETURN( ml+16 <= rrPtrDiff(overrun_chunk_end - to_ptr) , NULL );
		}
		#endif
		
		U8 * to_ptr_end = to_ptr + ml;

		// NEWLZF_CHUNK_MATCH_SAFE_ZONE is 16
		// NEWLZF_OFF24_MML_DECODE >= 8 means I can always do 24 here unconditionally
		RR_COMPILER_ASSERT( NEWLZF_OFF24_MML_DECODE >= 8 );
		
		lz_copy8(to_ptr,mp);
		lz_copy8(to_ptr+8,mp+8);		
		lz_copy8(to_ptr+16,mp+16);
		
		// tiny bit of speed advantage to not check this and just always copy 32
		// but for overrun safety that requires an extra "off24 safe zone" bigger than
		//	the normal match safe zone
		// not a big enough speed difference to bother with
		if ( ml > 24 )
		{
			lz_copy8(to_ptr+24,mp+24);
		}
		to_ptr = to_ptr_end;
		
#ifdef NEWLZF_ESCAPE_PREFETCH_INDEX
		// prefetch next :
		// escape_offset_ptr[] is padded with some zero offsets, so we can read past the end
		mp = chunk_ptr - escape_offset_ptr[NEWLZF_ESCAPE_PREFETCH_INDEX];
		RR_PREFETCHR_CL(mp);
#endif
		
		// no forced checkpoint, simple escapes can keep going
		#undef NEWLZF_OFF24MEDIUM_MAX_ML
		#endif
	}
}

