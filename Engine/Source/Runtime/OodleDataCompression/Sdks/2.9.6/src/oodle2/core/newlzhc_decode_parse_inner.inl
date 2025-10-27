// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.


/***

NEWLZ HC !!

***/

#ifndef NEWLZHC_PARSE_CAREFUL_OUTPUT
#error Set NEWLZHC_PARSE_CAREFUL_OUTPUT to 0 or 1!
#endif

	#ifndef VALIDATE_EXCESS_LRL
	#ifndef __RAD64__
	// extra work in 32-bit because fuzzy lrls can read past end and be garbage
	// just have to make sure the sign bit is off to enforce >= 0
	RR_COMPILER_ASSERT( sizeof(SINTa) == 4 );
	#define VALIDATE_EXCESS_LRL(lrl)	do { lrl &= 0xFFFFFF; RR_ASSERT( lrl >= 0 ); } while(0)
	#else
	// not needed in 64 bit
	RR_COMPILER_ASSERT( sizeof(SINTa) == 8 );
	#define VALIDATE_EXCESS_LRL(lrl)	RR_ASSERT( lrl >= 0 )
	#endif
	#endif

	// in non-multipacket mode, better to use a packets_ptr for checkpoint
	//	and don't maintain count
	// multi-packet counts down packets

	#ifdef DO_CHECKPOINT_LRL
	#undef DO_CHECKPOINT_LRL
	#endif
	#ifdef DO_CHECKPOINT_PACKET
	#undef DO_CHECKPOINT_PACKET
	#endif

	#if NEWLZHC_DECODE_MULTIPACKET
	UINTa packet_ptr_index = ( (UINTa)to_ptr ) & NEWLZHC_PACKET_POS_MASK;
	const U8 * packet_ptr = packet_ptrs[packet_ptr_index];
	#endif
	
	#if NEWLZHC_PARSE_CAREFUL_OUTPUT
	
	// careful just does all remaining packets :
	//	each packet is checked for to_ptr overrun
	
	#if NEWLZHC_DECODE_MULTIPACKET
	while ( packets_count > 0 )
	#else
	while ( packets < packets_end )
	#endif
	
	#else // NEWLZHC_PARSE_CAREFUL_OUTPUT
	
	// non-careful phase ("fast" phase)
	
	/*********
	
	offsets, literals & packets are all guaranteed to stay in scratch
	excesses forward pointer (LRL) is guaranteed, backward pointer (ML) is checked every time
	
	we need to ensure we won't overrun to_ptr
	
	fast-path packet writes at most (kBigLRL+kBigML) bytes = 40
	
	compute how many fast path packets I have room for in to_ptr buff
	(match_zone_end - to_ptr)/40
	
	round down with >>6 (/64) instead
	
	DO_CHECKPOINT_LRL is used once I have lrl, but before I put literals
		it ensures there is room for the literals first
		
	literals should at most go up to match_zone_end (last possible start location of a packet)
	
	DO_CHECKPOINT_PACKET is used initially, and then after a long match
		after the final match, to_ptr can be > match_zone_end
		which may make packets_count_fast negative
		that should be okay - no more packets will be done in that case
		@@ be a little wary of this
	
	Only a single loop is needed
		(contrast to newlz which uses a double loop)
		
	the target of the loop is constantly moving
	
	**********/	
	
	RR_COMPILER_ASSERT( (kBigLRL+kBigML) <= 64 ); // using >> 6
	
	#if NEWLZHC_DECODE_MULTIPACKET
	
	SINTa packets_count_fast;
	SINTa packets_count_check;
	
	// determine next checkpoint
	#define DO_CHECKPOINT_PACKET() do { \
		packets_count_fast = rrPtrDiff(match_zone_end - to_ptr) >> 6; \
		packets_count_check = RR_MAX(packets_count - packets_count_fast,0); \
		} while(0)
		
	#define DO_CHECKPOINT_LRL(lrl) do { \
		REQUIRE_FUZZ_RETURN( (match_zone_end - to_ptr) >= (lrl) , false ); \
		packets_count_fast = rrPtrDiff(match_zone_end - to_ptr - lrl) >> 6; \
		packets_count_check = RR_MAX(packets_count - packets_count_fast,0); \
		} while(0)
	
	DO_CHECKPOINT_PACKET();
			
	while( packets_count > packets_count_check )
	
	#else // NEWLZHC_DECODE_MULTIPACKET
	
	SINTa packets_count_fast;
	const U8 * packets_check;
	
	// determine next checkpoint
	#define DO_CHECKPOINT_PACKET() do { \
		packets_count_fast = rrPtrDiff(match_zone_end - to_ptr) >> 6; \
		if ( packets_count_fast > (packets_end - packets) ) \
			packets_check = packets_end; \
		else \
			packets_check = packets + packets_count_fast; \
		} while(0)
		
	#define DO_CHECKPOINT_LRL(lrl) do { \
		REQUIRE_FUZZ_RETURN( (match_zone_end - to_ptr) >= (lrl) , false ); \
		packets_count_fast = rrPtrDiff(match_zone_end - to_ptr - lrl) >> 6; \
		if ( packets_count_fast > (packets_end - packets) ) \
			packets_check = packets_end; \
		else \
			packets_check = packets + packets_count_fast; \
		} while(0)
		
	DO_CHECKPOINT_PACKET();
	
	while (packets < packets_check)
	
	#endif
	
	#endif // NEWLZHC_PARSE_CAREFUL_OUTPUT
	
	{
		//---------------------------------------------------
		// fetch a packet :
		
		#if NEWLZHC_DECODE_MULTIPACKET
		
		RR_ASSERT( packet_ptr_index == (( (UINTa)to_ptr ) & NEWLZHC_PACKET_POS_MASK ) );
		RR_ASSERT( packet_ptr == packet_ptrs[packet_ptr_index] );
	
		packets_count--;
				
		RR_ASSERT_IF_NOT_CORRUPT( packet_ptr < arrays->packet_pos_ends[(to_ptr - chunk_base)& NEWLZHC_PACKET_POS_MASK] ); // true on non-corrupt data
		RR_ASSERT( packet_ptr < scratch_end ); // should always be true
		
		UINTa packet = *packet_ptr++;
		packet_ptrs[packet_ptr_index] = packet_ptr;
			
		#else
				
		UINTa packet = *packets++;
		
		#endif // NEWLZHC_DECODE_MULTIPACKET
	
		//---------------------------------------------------
		// unpack the packet :
		
		UINTa lrl_part = packet & (NEWLZHC_PACKET_LRL_MAX << NEWLZHC_PACKET_ML_BITS);
		
		
		UINTa packet_offset = packet >> NEWLZHC_PACKET_OFFSET_SHIFT;	
		UINTa ml = (packet & NEWLZHC_PACKET_ML_MAX) + NEWLZHC_LOMML;
		
		RR_ASSERT_IF_NOT_CORRUPT( (packet - lrl_part) != NEWLZHC_PACKET_NORMAL_MATCH_MIN );
		
		//---------------------------------------------------
		
		/*
		SINTa pos = (to_ptr - chunk_base);
		if ( pos >= 24400 )
			RR_BREAK();
		/**/
		
		RR_ASSERT_IF_NOT_CORRUPT( to_ptr <= match_zone_end ); // packet cannot start within the match-start exclusion zone
		// at this point we don't know this condition on to_ptr to be true
		//	it will be checked after we get the lrl
		//	as to_ptr+lrl <= match_zone_end 
		// -> note must be checked even when lrl == 0 if you are in the CAREFUL phase

		// if not corrupt, pointers should stay in "scratch_arrays"
		//	even if corrupt, should not get out of scratch :
		RR_ASSERT_IF_NOT_CORRUPT( (U8 *)offsets_ptr < scratch_arrays_end );
		RR_ASSERT_IF_NOT_CORRUPT( (U8 *)excesses_ptr < scratch_arrays_end );
		RR_ASSERT_IF_NOT_CORRUPT( excesses_ptr <= excesses_end_ptr );
		RR_ASSERT( (U8 *)offsets_ptr < scratch_end );
		RR_ASSERT( (U8 *)excesses_ptr < scratch_end );
		
		// stuff *offsets_ptr into lastsoffsets early :
		newLZHC_dec_LOs_Add(lastoffsets,*offsets_ptr);
				
		RR_ASSERT( neg_offset == lastoffsets.LastOffset() );
		// alt offsets can go down to 1 :
		RR_ASSERT( -neg_offset >= 1 );
		RR_ASSERT_IF_NOT_CORRUPT( newlz_literals_type_offset1_ok(NEWLZHC_DECODE_LITERALS_TYPE) || -neg_offset >= NEWLZ_MIN_OFFSET );
		
		// after LRL, to_ptr+lrl <= match_zone_end
		// this is guaranteed without checking for short LRL's in the checkpoint loop
		
		//=====================================================================================
		// 
		// LRL copier :

		/**
		
		in NEWLZHC_PARSE_CAREFUL_OUTPUT loop :
		
		check every lrl vs match_zone_end (fail)
		
		in non-careful loop :
		
		only need to check when lrl > kBigLRL (24)
		
		if lrl takes us into output_safe_end , need to reset the checkpoint
		if lrl takes us into match_zone_end , fail
				
		**/
				
		#if defined(NEWLZ_LITERALS_TYPE_O1) && NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_O1
		
		{
			if ( lrl_part == (NEWLZHC_PACKET_LRL_MAX<<NEWLZHC_PACKET_ML_BITS) )
			{
				SINTa lrl = *excesses_ptr++;
				
				// *excesses_ptr is not trusted, need to check
				REQUIRE_FUZZ_RETURN( lrl > 0 , false );
				
				#if NEWLZHC_PARSE_CAREFUL_OUTPUT
				REQUIRE_FUZZ_RETURN( (match_zone_end - to_ptr) >= lrl , false );
				#endif
			
				#if !NEWLZHC_PARSE_CAREFUL_OUTPUT
				if_unlikely ( lrl > kBigLRL )
				{
					DO_CHECKPOINT_LRL(lrl);
				}
				#endif
			
				// could do a separate copier for lrl > 24 and <= 24
				// but o1 is just so slow and serial that unrolls don't help much
				
				newlz_copyliterals_o1(to_ptr,lrl,next_literals_o1,literals_ptrs_o1);
				// literals_ptrs advanced inside
				to_ptr += lrl;
			}
			else if ( lrl_part )
			{
				SINTa lrl  = lrl_part >> NEWLZHC_PACKET_ML_BITS;
				
				RR_ASSERT( lrl == 1 || lrl == 2 );
				
				#if NEWLZHC_PARSE_CAREFUL_OUTPUT
				REQUIRE_FUZZ_RETURN( (match_zone_end - to_ptr) >= lrl , false );
				#endif
				
				UINTr val = to_ptr[-1];
				UINTr c = newlz_o1_context( val );
				const U8 * from_ptr = literals_ptrs_o1[c];
				val = next_literals_o1[c];
				next_literals_o1[c] = *from_ptr++;
				*to_ptr++ = (U8)val;
				literals_ptrs_o1[c] = from_ptr;
	
				if ( lrl == 2 )
				{
					c = newlz_o1_context( val );
					from_ptr = literals_ptrs_o1[c];
					val = next_literals_o1[c];
					next_literals_o1[c] = *from_ptr++;
					*to_ptr++ = (U8)val;
					literals_ptrs_o1[c] = from_ptr;
				}
			}
			else
			{
				#if NEWLZHC_PARSE_CAREFUL_OUTPUT
				REQUIRE_FUZZ_RETURN( (match_zone_end - to_ptr) >= 0 , false );
				#endif
			}
		}
		
		#elif defined(NEWLZ_LITERALS_TYPE_O2) && NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_O2
			
		if ( lrl_part )
		{
			SINTa lrl;

			if ( lrl_part == (NEWLZHC_PACKET_LRL_MAX<<NEWLZHC_PACKET_ML_BITS) )
			{
				lrl = *excesses_ptr++;
				VALIDATE_EXCESS_LRL(lrl);
			}
			else
			{
				lrl  = lrl_part >> NEWLZHC_PACKET_ML_BITS;
			}
			
			#if NEWLZHC_PARSE_CAREFUL_OUTPUT
			REQUIRE_FUZZ_RETURN( (match_zone_end - to_ptr) >= lrl , false );
			#endif
			
			#if !NEWLZHC_PARSE_CAREFUL_OUTPUT
			if_unlikely ( lrl > kBigLRL )
			{
				DO_CHECKPOINT_LRL(lrl);
			}
			#endif
			
			newlz_copyliterals_o2(to_ptr,to_ptr+neg_offset,lrl,literals_ptrs);
			// literals_ptrs advanced inside
			to_ptr += lrl;
		}
		else
		{
			#if NEWLZHC_PARSE_CAREFUL_OUTPUT
			REQUIRE_FUZZ_RETURN( (match_zone_end - to_ptr) >= 0 , false );
			#endif
		}

		#elif defined(NEWLZ_LITERALS_TYPE_SUBANDF) && NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_SUBANDF
		
		{
			if ( lrl_part == (NEWLZHC_PACKET_LRL_MAX<<NEWLZHC_PACKET_ML_BITS) )
			{
				SINTa lrl = *excesses_ptr++;
				
				VALIDATE_EXCESS_LRL(lrl);
				
				#if NEWLZHC_PARSE_CAREFUL_OUTPUT
				REQUIRE_FUZZ_RETURN( (match_zone_end - to_ptr) >= lrl , false );
				#endif
			
				#if !NEWLZHC_PARSE_CAREFUL_OUTPUT
				if_unlikely ( lrl > kBigLRL )
				{
					DO_CHECKPOINT_LRL(lrl);
					
					#if 1
					
					// could do a separate copier for lrl > 24 and <= 24
					// subandf long lrl copier could roll up to alignment, then copying 16 is much neater
				
					// jaguar texture.bc1 :
					//	107.93 -> 108.65 MB/s
					// pretty pointless
				
					while( (((UINTa)to_ptr)&0xF) != 0 )
					{
						newlz_copyliteral_subandF(to_ptr,to_ptr+neg_offset,literals_ptrs);
						to_ptr++;
						lrl--;
					}
					
					SINTa num16s = lrl>>4;
					lrl = lrl & 0xF;
					
					while(num16s--)
					{
						RR_UNROLL_I_16(0, \
							to_ptr[i] = *(literals_ptrs[i]) + to_ptr[i+neg_offset]; \
							literals_ptrs[i] += 1; );
						to_ptr += 16;
						// pos doesn't need to be updated because the mod stays zero
					}
					
					// lrl tail drops through and is done below
					
					#endif
				}
				#endif
						
				newlz_copyliterals_subandF(to_ptr,to_ptr+neg_offset,lrl,literals_ptrs);
				// literals_ptrs advanced inside
				to_ptr += lrl;
			}
			else if ( lrl_part )
			{
				// note : there is the option to not branch on lrl_part here
				//	always output 2 literals and branchlessly increment 0-2
				//	seems to be about the same speed, so meh
			
				SINTa lrl  = lrl_part >> NEWLZHC_PACKET_ML_BITS;
				
				RR_ASSERT( lrl == 1 || lrl == 2 );
				
				#if NEWLZHC_PARSE_CAREFUL_OUTPUT
				REQUIRE_FUZZ_RETURN( (match_zone_end - to_ptr) >= lrl , false );
				#endif
				
				newlz_copyliteral_subandF(to_ptr,to_ptr+neg_offset,literals_ptrs);
				to_ptr++;
				
				// second literal branchless?
				// pretty meh
				
				/*
				if ( lrl == 2 )
				{
					pos++;
					newlz_copyliteral_subandF(to_ptr,pos,to_ptr+neg_offset,literals_ptrs);
					to_ptr++;
				}
				*/
				
				int inc2 = (lrl == 2);
				SINTa index = ((UINTa)to_ptr)&0xF;
				
				const U8 * from_ptr = literals_ptrs[index];
				*to_ptr = (*from_ptr) + to_ptr[neg_offset];
				to_ptr += inc2; from_ptr += inc2;
				literals_ptrs[index] = from_ptr;
			}
			else
			{
				#if NEWLZHC_PARSE_CAREFUL_OUTPUT
				REQUIRE_FUZZ_RETURN( (match_zone_end - to_ptr) >= 0 , false );
				#endif
			}
		}
			
		#elif defined(NEWLZ_LITERALS_TYPE_SUBAND3) && NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_SUBAND3
		
		/**
		
		tried alternatives here :
		
		branch on if (lrl_part) -> worse
		
		no branch on lrl excess ; branchless excess fetch
		do still branch on lrl > 4
		use the tail copier to handle lrl 0-4
		-> worse
		
		**/
		
		{
			SINTa lrl;

			UINTa pos3 = (UINTa)to_ptr;
				
			if ( lrl_part == (NEWLZHC_PACKET_LRL_MAX<<NEWLZHC_PACKET_ML_BITS) )
			{
				lrl = *excesses_ptr++;
				
				VALIDATE_EXCESS_LRL(lrl);
				
				#if NEWLZHC_PARSE_CAREFUL_OUTPUT
				REQUIRE_FUZZ_RETURN( (match_zone_end - to_ptr) >= lrl , false );
				#endif
			
				// lrl >= 3
				
				// use 8-at-a-time path
				 
				#ifdef NEWLZHC_SUBAND3_VEC_OFFS

				const U8 * lptr0 = literals_base + literals_offs[(pos3&3)    ];
				const U8 * lptr1 = literals_base + literals_offs[(pos3&3) + 1];
				const U8 * lptr2 = literals_base + literals_offs[(pos3&3) + 2];
				const U8 * lptr3 = literals_base + literals_offs[(pos3&3) + 3];

				v_literals_offs = newlzhc_advance_suband3_lit_offs(v_literals_offs, literals_offs, pos3, lrl);

				#else

				const U8 * lptr0 = literals_ptrs[pos3&3]; literals_ptrs[pos3&3] = lptr0 + ((lrl + 3) >> 2); pos3++;
				const U8 * lptr1 = literals_ptrs[pos3&3]; literals_ptrs[pos3&3] = lptr1 + ((lrl + 2) >> 2); pos3++;
				const U8 * lptr2 = literals_ptrs[pos3&3]; literals_ptrs[pos3&3] = lptr2 + ((lrl + 1) >> 2); pos3++;
				const U8 * lptr3 = literals_ptrs[pos3&3]; literals_ptrs[pos3&3] = lptr3 + ((lrl    ) >> 2);

				#endif
				
				// now the first literal is just at lptr0
				
				// do 8 :
				NEWLZ_COPY8SUBAND3_RET first8_ret = newlz_literals_copy8suband3(to_ptr,to_ptr+neg_offset,lptr0,lptr1,lptr2,lptr3);
				
				CHECK( RR_ASSERT( memcmp(to_ptr,check_ptr,RR_MIN(8,lrl)) == 0 ); RR_DURING_ASSERT( check_ptr += RR_MIN(8,lrl) ) );

				if ( lrl > 8 )
				{
					// lrl can be huge (20k) but this handles it just fine					
					// lrl >= 9 , 8 already done, so do 8 more, then loop on steps of 8
					SINTa loffs = 2;
					newlz_literals_copy8suband3_second(to_ptr+8,to_ptr+8+neg_offset,lptr0+loffs,lptr1+loffs,lptr2+loffs,lptr3+loffs,first8_ret);

					CHECK( RR_ASSERT( memcmp(to_ptr+8,check_ptr,RR_MIN(8,lrl-8)) == 0 ); RR_DURING_ASSERT( check_ptr += RR_MIN(8,lrl-8) ) );
	
					// long lrl case
					if ( lrl > 16 )
					{
						loffs = 2+2;
						first8_ret = newlz_literals_copy8suband3(to_ptr+16,to_ptr+16+neg_offset,lptr0+loffs,lptr1+loffs,lptr2+loffs,lptr3+loffs);

						CHECK( RR_ASSERT( memcmp(to_ptr+16,check_ptr,RR_MIN(8,lrl-16)) == 0 ); RR_DURING_ASSERT( check_ptr += RR_MIN(8,lrl-16) ) );
	
						if_unlikely ( lrl > 24 ) // this threshold must match kBigLRL!!!
						{
							RR_COMPILER_ASSERT( kBigLRL == 24 );

							#if !NEWLZHC_PARSE_CAREFUL_OUTPUT // careful output mode checks LRL for every match and runs when we're past the safe area end.
							DO_CHECKPOINT_LRL(lrl);
							#endif

							to_ptr += 24;
							lrl -= 24;
							// at this point we've done 24 bytes of copying, and
							//	to_ptr and lrl are both adjusted
							//  but litp is behind by one step
							
							for (;;)
							{
								loffs += 2;
								newlz_literals_copy8suband3_second(to_ptr,to_ptr+neg_offset,lptr0+loffs,lptr1+loffs,lptr2+loffs,lptr3+loffs,first8_ret);

								CHECK( RR_ASSERT( memcmp(to_ptr,check_ptr,RR_MIN(8,lrl)) == 0 ); RR_DURING_ASSERT( check_ptr += RR_MIN(8,lrl) ) );

								to_ptr += 8;
								lrl -= 8;
								if ( lrl <= 0 )
									break;

								loffs += 2;
								first8_ret = newlz_literals_copy8suband3(to_ptr,to_ptr+neg_offset,lptr0+loffs,lptr1+loffs,lptr2+loffs,lptr3+loffs);

								CHECK( RR_ASSERT( memcmp(to_ptr,check_ptr,RR_MIN(8,lrl)) == 0 ); RR_DURING_ASSERT( check_ptr += RR_MIN(8,lrl) ) );

								to_ptr += 8;
								lrl -= 8;
								if ( lrl <= 0 )
									break;
							}
							RR_ASSERT( lrl <= 0 && lrl >= -7 );
						}
					}
				}

				to_ptr += lrl;
			}
			else
			{
				lrl  = lrl_part >> NEWLZHC_PACKET_ML_BITS;
				
				RR_ASSERT( lrl == 0 || lrl == 1 || lrl == 2 );
				
				#if NEWLZHC_PARSE_CAREFUL_OUTPUT
				REQUIRE_FUZZ_RETURN( (match_zone_end - to_ptr) >= lrl , false );
				#endif
			
				// output 2
				// advance 0-2 :

				#ifdef NEWLZHC_SUBAND3_VEC_OFFS
			
				const U8 * lptr0 = literals_base + literals_offs[(pos3&3)    ];
				const U8 * lptr1 = literals_base + literals_offs[(pos3&3) + 1];
				v_literals_offs = newlzhc_advance_suband3_lit_offs(v_literals_offs, literals_offs, pos3, lrl);

				#else

				const U8 * lptr0 = literals_ptrs[pos3&3]; literals_ptrs[pos3&3] = lptr0 + ((lrl + 3) >> 2); pos3++;
				const U8 * lptr1 = literals_ptrs[pos3&3]; literals_ptrs[pos3&3] = lptr1 + ((lrl + 2) >> 2); pos3++;

				#endif

				to_ptr[0] = (U8)( *lptr0 + to_ptr[0+neg_offset] );
				to_ptr[1] = (U8)( *lptr1 + to_ptr[1+neg_offset] );
				to_ptr += lrl;
			}
				
		// suband3
		}
		#else // RAW, SUB, LAMSUB
		{
		
			#if NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_LAMSUB
			
			if ( lrl_part ) // check lrl > 0
			{
						
			#elif 0
			}
			#else
			
			{
			
			#endif
			
				// branchless lrl excess fetch :
				
				SINTa lrl  = lrl_part >> NEWLZHC_PACKET_ML_BITS;
				SINTa lrli = *excesses_ptr;
				VALIDATE_EXCESS_LRL(lrli);
				const U32 *eptri = excesses_ptr + 1;
				excesses_ptr = (lrl == NEWLZHC_PACKET_LRL_MAX) ? eptri : excesses_ptr;
				lrl = (lrl == NEWLZHC_PACKET_LRL_MAX) ? lrli : lrl;

				// In 64-bit, no problem (64-bit signed lrl initialized from 32-bit uint)
				// In 32-bit, VALIDATE_EXCESS_LRL ensures this.
				//
				// We need this to be true for the match_zone_end check that follows.
				RR_ASSERT( lrl >= 0 );
		
				#if NEWLZHC_PARSE_CAREFUL_OUTPUT
				REQUIRE_FUZZ_RETURN( (match_zone_end - to_ptr) >= lrl , false );
				#endif

				#if NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_LAMSUB
				{
					// First byte is LAM byte and not part of the regular LRL
					lrl--;
					// fuzz - doing lrl-- means I do one more literal than I think
					//	see branch on lrl >= 24 below

					// This check is necessary: we checked that lrl_part != 0
					// earlier, but *excesses_ptr is untrusted and could be
					// uninitialized garbage, since the decoder can run off
					// the end of the initialzied excesses
					//
					// (this is detected and flagged as an error eventually,
					// but this loop needs to handle garbage)
					REQUIRE_FUZZ_RETURN( lrl >= 0 , false );

					U8 sub = *literals_ptr_lam++;
					*to_ptr = (U8)(sub + to_ptr[neg_offset]); to_ptr++;
				}
				// then proceeds like sub literals
				#endif
				
				RR_ASSERT_IF_NOT_CORRUPT( literals_ptr+lrl <= arrays->literals_ptrs[0] + arrays->literals_counts[0] );
				RR_ASSERT_IF_NOT_CORRUPT( literals_ptr+lrl <= scratch_arrays_end );

				// LRLs below kBigLRL should be fine; long LRLs need to do extra validation
				// see below
				RR_ASSERT( RR_MIN(lrl,kBigLRL) < (scratch_end - literals_ptr) );
				
				//#pragma RR_PRAGMA_MESSAGE("NEWLZHC_DECODE_LITERALS_TYPE = " RR_STRINGIZE_DELAY( NEWLZHC_DECODE_LITERALS_TYPE ) )
				newlz_literals_copy8<NEWLZHC_DECODE_LITERALS_TYPE>(to_ptr,to_ptr+neg_offset,literals_ptr);
				
				CHECK( RR_ASSERT( memcmp(to_ptr,check_ptr,RR_MIN(8,lrl)) == 0 ); RR_DURING_ASSERT( check_ptr += RR_MIN(8,lrl) ) );

				if ( lrl > 8 )
				{								
					// lrl can be huge (20k) but this handles it just fine					
					// lrl >= 9 , 8 already done, so do 8 more, then loop on steps of 8
					
					newlz_literals_copy8<NEWLZHC_DECODE_LITERALS_TYPE>(to_ptr+8,(to_ptr+8)+neg_offset,literals_ptr+8);
					CHECK( RR_ASSERT( memcmp(to_ptr+8,check_ptr,RR_MIN(8,lrl-8)) == 0 ); RR_DURING_ASSERT( check_ptr += RR_MIN(8,lrl-8) ) );
	
					// long lrl case
					if ( lrl > 16 )
					{
						newlz_literals_copy8<NEWLZHC_DECODE_LITERALS_TYPE>(to_ptr+16,(to_ptr+16)+neg_offset,literals_ptr+16);
						CHECK( RR_ASSERT( memcmp(to_ptr+16,check_ptr,RR_MIN(8,lrl-16)) == 0 ); RR_DURING_ASSERT( check_ptr += RR_MIN(8,lrl-16) ) );
	
						#if NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_LAMSUB
						// fuzz - for LAMSUB ; we already did an lrl--
						//	so to keep kBigLRL at 24 , must go in here at lrl == 24
						
						if_unlikely ( lrl >= 24 )
						#else
						if_unlikely ( lrl > 24 )
						#endif
						{
							RR_COMPILER_ASSERT( kBigLRL == 24 );

							#if !NEWLZHC_PARSE_CAREFUL_OUTPUT // careful output mode checks LRL for every match and runs when we're past the safe area end.
							DO_CHECKPOINT_LRL(lrl);
							#endif

							// At this point, we know LRL is sane, so this really should hold.
							RR_ASSERT( lrl < (scratch_end-literals_ptr) );

							to_ptr += 24;
							literals_ptr += 24;
							lrl -= 24;

							for (;;)
							{
								newlz_literals_copy8<NEWLZHC_DECODE_LITERALS_TYPE>(to_ptr,to_ptr+neg_offset,literals_ptr);
								
								CHECK( RR_ASSERT( memcmp(to_ptr,check_ptr,RR_MIN(8,lrl)) == 0 ); RR_DURING_ASSERT( check_ptr += RR_MIN(8,lrl) ) );
								
								to_ptr += 8;
								literals_ptr += 8;
								lrl -= 8;
								if ( lrl <= 0 )
									break;
							}
						}
					}

					// lrl is negative cuz we went too far; back up:
					to_ptr += lrl;
					literals_ptr += lrl;
				}
				else
				{				
					to_ptr += lrl;
					literals_ptr += lrl;
				}
			}
			#if NEWLZHC_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_LAMSUB			
			else // if ( lrl_part )
			{
				RR_ASSERT( lrl_part == 0 );
			
				#if NEWLZHC_PARSE_CAREFUL_OUTPUT
				REQUIRE_FUZZ_RETURN( (match_zone_end - to_ptr) >= 0 , false );
				#endif
			}			
			#endif
		}
		#endif // NEWLZHC_DECODE_LITERALS_TYPE
		
		/**
		
		get offset :
		
		**/
					
		// get LO or normal offset :
		neg_offset = newLZHC_dec_LOs_MTF(lastoffsets,packet_offset);
								
		// advance offsets_ptr if it was used :
		offsets_ptr += ( packet_offset == NEWLZHC_NUM_LAST_OFFSETS );
								
		RR_ASSERT_IF_NOT_CORRUPT( neg_offset < 0 ); // fuzz : in corruption, "neg_offset" can be positive; this is caught by "long story short" below									
		RR_ASSERT_IF_NOT_CORRUPT( -neg_offset < NEWLZ_MAX_OFFSET );
		RR_ASSERT_IF_NOT_CORRUPT( newlz_literals_type_offset1_ok(NEWLZHC_DECODE_LITERALS_TYPE) || -neg_offset >= NEWLZ_MIN_OFFSET );
		//rrprintf("offset: %d ml: %d mp=%d\n",offset,ml,(to_ptr - chunk_ptr));
		
		//     0    <        offset <= (to_ptr - window_base)
		// <=> 0    >       -offset >= window_base - to_ptr
		// <=> 2^64 > 2^64 - offset >= 2^64 + (window_base - to_ptr)
		// long story short: do the calcs unsigned and it all works.
		REQUIRE_FUZZ_RETURN( (UINTa)neg_offset >= (UINTa) (window_base - to_ptr) , false );
		
		RR_ASSERT( neg_offset < 0 );
		RR_ASSERT( to_ptr + neg_offset >= window_base );

		const U8 * mp = to_ptr + ((SINTa)neg_offset);
		CHECK( RR_ASSERT( memcmp(check_ptr,mp, RR_MIN(ml,(int)-neg_offset)) == 0 ) );
		
		// NOTE(fg): at this point (unlike pre-LRL), this bound is tight
		// also note that with to_ptr<=match_zone_end now, we don't need to worry about bounds-checking ML
		// in the ML<16 case, not even in "careful output" mode.
		
		RR_ASSERT( to_ptr <= match_zone_end );		

		/**
		
		Match copy
		
		**/

		RR_COMPILER_ASSERT( NEWLZHC_LOMML == 2 );
		RR_COMPILER_ASSERT( NEWLZHC_PACKET_ML_MAX == 7 );
		
		if_unlikely ( ml == (NEWLZHC_PACKET_ML_MAX+NEWLZHC_LOMML) )
		{
			RR_ASSERT( -neg_offset >= 1 );
			RR_ASSERT_IF_NOT_CORRUPT( -neg_offset >= NEWLZ_MIN_OFFSET );
		
			// ML excesses at excesses_end_ptr backward :

			// @@!! fuzz - just check backward excesses_end_ptr every time for now
			//	in theory it might be able to read back to before the start of scratch
			//	-> could use a copy-to-scratch check in the outer loop to avoid this
			//		but whatevs I suspect
			REQUIRE_FUZZ_RETURN( excesses_end_ptr > excesses_ptr , false );
			
			excesses_end_ptr--;
			ml = (NEWLZHC_PACKET_ML_MAX + NEWLZHC_LOMML) - NEWLZHC_PACKET_LRL_MAX + *excesses_end_ptr
			// fuzz : *excesses can be garbage (even negative)
			//  in that case ml will just be large, and the kBigML REQUIRE_FUZZ_RETURN will catch it

			CHECK( RR_ASSERT( memcmp(check_ptr,mp, RR_MIN(ml,(int)-neg_offset)) == 0 ); check_ptr += ml );

			// @@!! fuzz - note - no validation needed here for ml <= 16
			//	because to_ptr <= match_zone_end				
			RR_COMPILER_ASSERT( NEWLZHC_CHUNK_NO_MATCH_ZONE >= 16 );
			
			// ml >= 9 , so I can copy 16 :
			U8 * to_ptr_end = to_ptr+ml;			
			lz_copy8(to_ptr,mp);
			lz_copy8(to_ptr+8,mp+8);
			
			#if NEWLZHC_DECODE_MULTIPACKET
			packet_ptr_index = ( (UINTa)to_ptr_end ) & NEWLZHC_PACKET_POS_MASK;
			packet_ptr = packet_ptrs[packet_ptr_index];
			#endif
	
			// ml <= 16 can skip validation
			if ( ml > 16 )
			{
				RR_COMPILER_ASSERT( kBigML == 16 );
			
				// validate ML
				RR_ASSERT( match_end > to_ptr ); // gauranteed even in fuzz
				REQUIRE_FUZZ_RETURN( (UINTa)(match_end - to_ptr) >= ml , false );
					
				// do another 8 , so 24 is done :
				lz_copy8(to_ptr+16,mp+16);
				to_ptr += 24; mp += 24;
										
				if ( to_ptr < to_ptr_end )
				{					
					do { lz_copy8(to_ptr,mp); to_ptr += 8; mp += 8; }
					while( to_ptr < to_ptr_end );
				}
				
				to_ptr = to_ptr_end;

				#if !NEWLZHC_PARSE_CAREFUL_OUTPUT
				DO_CHECKPOINT_PACKET();
				#endif
			}
			else
			{
				to_ptr = to_ptr_end;
			}
		}
		else
		{
			// copy <= 8 :
		
			RR_COMPILER_ASSERT( NEWLZHC_CHUNK_NO_MATCH_ZONE >= 8 );
			RR_ASSERT(chunk_end - to_ptr >= 8);

			RR_ASSERT( ml <= 8 );
			lz_copy8(to_ptr,mp);
						
			CHECK( check_ptr += ml );				
			to_ptr += ml;
			
			#if NEWLZHC_DECODE_MULTIPACKET
			packet_ptr_index = ( (UINTa)to_ptr ) & NEWLZHC_PACKET_POS_MASK;
			packet_ptr = packet_ptrs[packet_ptr_index];
			#endif
		}
			
	}

#undef NEWLZHC_PARSE_CAREFUL_OUTPUT
