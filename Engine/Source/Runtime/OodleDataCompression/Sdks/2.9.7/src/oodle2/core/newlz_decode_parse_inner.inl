// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.


/***

NEWLZ !!

***/

#ifndef NEWLZ_PARSE_CAREFUL_OUTPUT
#error Set NEWLZ_PARSE_CAREFUL_OUTPUT to 0 or 1!
#endif

	// are we starting to run out of literals?
	if ( literals_ptr >= literals_check )
	{
		if ( literals_ptr > literals_end )
			return false; // bad stream

		SWITCH_TO_LITERAL_SCRATCH_BUF();
	}

	// no loop in NEWLZ_PARSE_CAREFUL_OUTPUT, done outside
#if ! NEWLZ_PARSE_CAREFUL_OUTPUT
	// determine next checkpoint
	const U8 * packets_check = packets + kPacketsPerCheckpoint;
	if ( packets_end - packets < kPacketsPerCheckpoint )
		packets_check = packets_end;

	while (packets < packets_check)
#endif
	
	{
		RR_ASSERT_IF_NOT_CORRUPT( to_ptr <= match_zone_end ); // packet cannot start within the match-start exclusion zone
		// -> this will be checked below in the case of NEWLZ_PARSE_CAREFUL_OUTPUT
		#if ! NEWLZ_PARSE_CAREFUL_OUTPUT
		RR_ASSERT( to_ptr <= match_zone_end ); 
		#endif

		UINTa packet = *packets++;
				
		// unpacket the packet :
		SINTa lrl = packet & 0x3;
		SINTa packet_ml = (packet>>2) & 0xF;
		UINTa packet_offset = packet >> 6;
		
		//// len-2 normal match exclude zone :
		//RR_ASSERT( (packet>>2) != (3<<4) );
		
		// stuff *offsets_ptr into lastsoffsets early :
		newLZ_dec_LOs_Add(lastoffsets,*offsets_ptr);
						
		// do literal copy
		//	with sub using neg_offset			
		{
			RR_ASSERT( neg_offset == lastoffsets.LastOffset() );
			// alt offsets can go down to 1 :
			RR_ASSERT( -neg_offset >= 1 );
			RR_ASSERT_IF_NOT_CORRUPT( -neg_offset >= NEWLZ_MIN_OFFSET );

			// NOTE(fg):
			// branchless version of "packet_lrl + ((packet_lrl == LRL_MAX) ? *excessses++ : 0)"
			// BUT get_offsets_excesses already adds LRL_MAX to the excesses before storing them (when subtracting out exp-golomb bias)
			// so we don't actually have to add packet_lrl at all!
			RR_ASSERT( excesses_ptr < excesses_end + kExcessesMaxOverrun );
			SINTa lrli = *excesses_ptr;
			const U32 *eptri = excesses_ptr + 1;
			excesses_ptr = (lrl == NEWLZ_PACKET_LRL_MAX) ? eptri : excesses_ptr;
			lrl = (lrl == NEWLZ_PACKET_LRL_MAX) ? lrli : lrl;

			// excesses can read past end
			//	when they do, they read in zeroed space; must not read garbage that makes lrl negatives
			RR_ASSERT( lrl >= 0 );

		#if NEWLZ_PARSE_CAREFUL_OUTPUT
			// LRLs in main loop may not put us into the no-match-zone (since a match follows)
			if_unlikely ( (match_zone_end - to_ptr) < lrl )
				return false;
		#endif

			// In careful mode, we've now fully validated LRL.
			// in non-careful output mode, our rules are weaker; we're guaranteed
			// that lrls <= kBigLRL are fine, but longer LRLs still need to be
			// validated (which the big LRL case below handles).
			RR_ASSERT( (match_zone_end - to_ptr) >= RR_MIN(lrl,kBigLRL) );
		
			newlz_literals_copy8<NEWLZ_DECODE_LITERALS_TYPE>(to_ptr,to_ptr+neg_offset,literals_ptr);
			CHECK( RR_ASSERT( memcmp(to_ptr,check_ptr,RR_MIN(8,lrl)) == 0 ); RR_DURING_ASSERT( check_ptr += RR_MIN(8,lrl) ) );

			if ( lrl > 8 )
			{
				// lrl can be huge (20k) but this handles it just fine
				// lrl >= 9 , 8 already done, so do 8 more, then loop on steps of 8

				newlz_literals_copy8<NEWLZ_DECODE_LITERALS_TYPE>(to_ptr+8,(to_ptr+8)+neg_offset,literals_ptr+8);
				CHECK( RR_ASSERT( memcmp(to_ptr+8,check_ptr,RR_MIN(8,lrl-8)) == 0 ); RR_DURING_ASSERT( check_ptr += RR_MIN(8,lrl-8) ) );

				// long lrl case
				if ( lrl > 16 )
				{
					newlz_literals_copy8<NEWLZ_DECODE_LITERALS_TYPE>(to_ptr+16,(to_ptr+16)+neg_offset,literals_ptr+16);
					CHECK( RR_ASSERT( memcmp(to_ptr+16,check_ptr,RR_MIN(8,lrl-16)) == 0 ); RR_DURING_ASSERT( check_ptr += RR_MIN(8,lrl-16) ) );

					if_unlikely ( lrl > 24 ) // @@ this threshold must match kBigLRL!!!
					{
						RR_COMPILER_ASSERT( kBigLRL == 24 );

						// validate LRL
						if_unlikely ( (literals_check - literals_ptr) <= lrl )
						{
							#if ! NEWLZ_PARSE_CAREFUL_OUTPUT
							// if this LRL gets us past the literal mark, we need to checkpoint after this packet.
							//  (this is a way of making us break out of the packets loop after the match copy)
							packets_check = packets;
							#endif

							// if it outright extends past the end of the literal buffer, the stream is bad.
							if_unlikely ( (literals_end - literals_ptr) < lrl )
								return false;
						}

						#if !NEWLZ_PARSE_CAREFUL_OUTPUT // careful output mode checks ML for every match and runs when we're past the safe area end.
						if_unlikely ( (output_safe_end - to_ptr) < lrl )
						{
							#if ! NEWLZ_PARSE_CAREFUL_OUTPUT
							// if this LRL gets us outside the safe output area, we need to checkpoint after this packet.
							packets_check = packets;
							#endif

							// if it gets us outside the match zone, that's outright forbidden, since we're always
							// followed by a match.
							if_unlikely ( (match_zone_end - to_ptr) < lrl )
								return false;
						}
						#endif

						// At this point, this really needs to hold.
						// If not, we screwed something up.
						RR_ASSERT( lrl <= (match_zone_end - to_ptr) );

						to_ptr += 24;
						literals_ptr += 24;
						lrl -= 24;

						for (;;)
						{
							// do a copy-16 loop here? tried it, no big wins; long lrl is already fast
							newlz_literals_copy8<NEWLZ_DECODE_LITERALS_TYPE>(to_ptr,to_ptr+neg_offset,literals_ptr);

							CHECK( RR_ASSERT( memcmp(to_ptr,check_ptr,RR_MIN(8,lrl)) == 0 ); RR_DURING_ASSERT( check_ptr += RR_MIN(8,lrl) ) );

							to_ptr += 8;
							literals_ptr += 8;
							lrl -= 8;
							if ( lrl <= 0 )
								break;
						}
					}
				}
			}
		}

		// small LRLs didn't update pointers at all; long LRLs
		// did but overshot (and now have negative LRL). Take
		// care of the final advance for both.
		to_ptr += lrl;
		literals_ptr += lrl;

		// decode offset :
					
		// get LO or normal offset :
		neg_offset = newLZ_dec_LOs_MTF(lastoffsets,packet_offset);
				
		// advance offsets_ptr if it was used :
		//offsets_ptr += ( packet_offset == 3 );
		SINTa four_if_offset = (packet_offset + 1) & 4;
		offsets_ptr = (S32*)( (char *)offsets_ptr + four_if_offset );
									
		RR_ASSERT_IF_NOT_CORRUPT( -neg_offset >= NEWLZ_MIN_OFFSET && -neg_offset < NEWLZ_MAX_OFFSET );
		//rrprintf("offset: %d ml: %d mp=%d\n",offset,ml,(to_ptr - chunk_ptr));
		
		//     0    <        offset <= (to_ptr - window_base)
		// <=> 0    >       -offset >= window_base - to_ptr
		// <=> 2^64 > 2^64 - offset >= 2^64 + (window_base - to_ptr)
		// long story short: do the calcs unsigned and it all works.
		if ( (UINTa)neg_offset < (UINTa) (window_base - to_ptr) )
			return false; // bad stream

		SINTa ml = packet_ml + NEWLZ_LOMML;
		const U8 * mp = to_ptr + ((SINTa)neg_offset);
		CHECK( RR_ASSERT( memcmp(check_ptr,mp, RR_MIN(ml,(int)-neg_offset)) == 0 ) );
		
		RR_ASSERT(to_ptr<=match_zone_end); // NOTE(fg): at this point (unlike pre-LRL), this bound is tight
		// also note that with to_ptr<=match_zone_end now, we don't need to worry about bounds-checking ML
		// in the ML<16 case, not even in "careful output" mode.
		// (illegal ML<16 matches can end up extending past match_end, i.e. the end of the official match
		// zone, but they can never end up writing past chunk_end, which is our actual concern)

		/**
		
		Match copy and check for excess/long-match case
		
		packet_ml = 0xF is excess, that's ml == 17
		
		**/

		RR_COMPILER_ASSERT( NEWLZ_LOMML == 2 );	
		if_unlikely ( packet_ml == NEWLZ_PACKET_ML_MAX )
		{
			RR_ASSERT( ml == 17 );
						
			// get excess ml
			// NOTE(fg): conceptually, this does "ml += *excesses_ptr++".
			// But we know that ml = ML_MAX + LOMML, and furthermore the values stored at *excesses_ptr are
			// biased by LRL_MAX to make the (more common) LRL-excess path cheaper.
			RR_ASSERT( excesses_ptr < excesses_end + kExcessesMaxOverrun );
			ml = (NEWLZ_PACKET_ML_MAX + NEWLZ_LOMML) - NEWLZ_PACKET_LRL_MAX + *excesses_ptr++;
			// when excesses_ptr is past end, it reads NEWLZ_PACKET_LRL_MAX;
			// we set that up earlier (in decode_parse_outer) so this condition holds:
			RR_ASSERT( ml > 16 ); // this is required for the copy below to be legal (and not write past match safe zone)

			CHECK( RR_ASSERT( memcmp(check_ptr,mp, RR_MIN(ml,(int)-neg_offset)) == 0 ); check_ptr += ml );

			// validate ML
			if ( (match_end - to_ptr) < ml )
				return false; // bad stream
		
			RR_ASSERT_IF_NOT_CORRUPT( -neg_offset >= 8 );
						
			// ml >= 17 , so I can copy 24 to start :
			U8 * to_ptr_end = to_ptr+ml;			
			lz_copy8(to_ptr,mp);
			lz_copy8(to_ptr+8,mp+8);
			lz_copy8(to_ptr+16,mp+16);
			to_ptr += 24; mp += 24;
			
			if ( to_ptr < to_ptr_end )
			{
				// ML > 24			
				// basic loop does 8-at-a-time copies
				//	with sloppy tail
				// have tried rolling up to bigger copy steps here, but no win, long matches are already fast		
				
				do { lz_copy8(to_ptr,mp); to_ptr += 8; mp += 8; }
				while( to_ptr < to_ptr_end );
			}
			to_ptr = to_ptr_end;

			#if !NEWLZ_PARSE_CAREFUL_OUTPUT
			// if we're starting to poke outside the safe output area, need to break
			// into safe loop (will try to checkpoint again but see it's impossible)
			// this is the same as doing packets_check = packets;
			if_unlikely ( to_ptr >= output_safe_end )
				break;
			#endif
		}
		else
		{
			// short match path (no excess)
			// copy 16 unconditionally :
			// definitely faster than checking ml > 8
			
			RR_COMPILER_ASSERT( NEWLZ_CHUNK_NO_MATCH_ZONE >= 16 );
			RR_ASSERT(chunk_end - to_ptr >= 16);

			RR_ASSERT( ml <= 16 );
			lz_copy8(to_ptr,mp);
			lz_copy8(to_ptr+8,mp+8);

			CHECK( check_ptr += ml );				
			to_ptr += ml;
		}
	}

	// excesses can overrun by 2*kPacketsPerCheckpoint = 16 = 64 bytes
	// NEWLZ_EXTRA_SCRATCH_MEM_FOR_FUZZ = 64

	// sloppy (conservative) check here; we do a more precise check later.
	//	(offsets in excesses must be in same scratch and excesses after offsets)
	if ( (U8 *)offsets_ptr > (U8 *)excesses_end || excesses_ptr > excesses_end )
		return false; // bad stream

#undef NEWLZ_PARSE_CAREFUL_OUTPUT
