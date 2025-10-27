// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

	
		//RR_ASSERT( rp+ml <= rpEnd ); // no, ml may be bogus
						
		ml_control = (control>>4);
		
		RR_ASSERT( cp+2 <= comp_end );

//rrprintf("ml_control : %d\n",(int)ml_control);
				
		FUZZ_MUNGE(*((U8 *)cp));
				
		// speculatively grab offset but don't advance cp yet
		off = RR_GET16_LE_UNALIGNED(cp);
			
		if ( ml_control <= 8 )
		{
			#if DO_OFFSET_CHECK
			if_unlikely ( off > (UINTr)(rp - dictionaryBase) )
			{
				LZB_DECODE_ERROR_RETURN("LZB offset out of bounds\n");
			}
			#endif
			
			cp += 2; // consume offset
			const U8 * match = rp - off;

			SINTr ml = ml_control + LZB_MML;
		
			RR_ASSERT( ml <= 12 );

			#if DO_FAST_PATH
			#if defined(IS_LAST) && ! IS_LAST && ! IS_LONG_LRL_CASE
			// don't need to check it
			// FUZZ : we ensured that raw buf has enough room to do these
			//	short match copies with no check
			#else
			if_unlikely ( ml >= rpSafeEnd-rp )
			{
				if ( ml >= rpEnd-rp ) // match _touching_ rpEnd is not allowed (exclusion zone)
				{
					LZB_DECODE_ERROR_RETURN("LZB match past end\n");
				}

				copy_overlap8_nooverrun(rp,match,ml);
				rp += ml;
				break; // drop to careful loop
			}
			#endif

			RR_ASSERT( rp+12 <= rpEnd );
			
			lz_copy8(rp,match);
			lz_copy4(rp+8,match+8);
			#else // DO_FAST_PATH
			if_unlikely ( ml >= rpEnd-rp ) // match _touching_ rpEnd is not allowed (exclusion zone)
			{
				LZB_DECODE_ERROR_RETURN("LZB match past end\n");
			}

			copy_overlap8_nooverrun(rp,match,ml);
			#endif
			
			rp += ml;
		}
		else
		{			
			if_likely( ml_control < LZB_MLCONTROL_ESCAPE ) // short match
			{
				#if DO_OFFSET_CHECK
				if_unlikely ( off > (UINTr)(rp - dictionaryBase) )
				{
					LZB_DECODE_ERROR_RETURN("LZB offset out of bounds\n");
				}
				#endif
			
				cp += 2; // consume offset
				const U8 * match = rp - off;
					
				SINTr ml = ml_control + LZB_MML;
				// LZB_MML = 4 , LZB_MLCONTROL_ESCAPE = 15
				// ml < (LZB_MML+LZB_MLCONTROL_ESCAPE)
				// ml < 19
		
				RR_ASSERT_IF_NOT_CORRUPT( off >= 8 || (UINTr)ml <= off );
				RR_ASSERT( ml <= 18 );
				
				#if DO_FAST_PATH
				if_unlikely ( ml >= rpSafeEnd-rp )
				{
					if ( ml >= rpEnd-rp ) // match _touching_ rpEnd is not allowed (exclusion zone)
					{
						LZB_DECODE_ERROR_RETURN("LZB match past end\n");
					}

					copy_overlap8_nooverrun(rp,match,ml);
					rp += ml;
					break; // drop to careful loop
				}
			
				//copy_match_short_nooverlap(rp,match,ml);
			
				RR_ASSERT( rp+18 <= rpEnd );
			
				lz_copy8(rp,match);
				lz_copy8(rp+8,match+8);
				lz_copy2(rp+16,match+16);
				#else // DO_FAST_PATH
				if ( ml >= rpEnd-rp ) // match _touching_ rpEnd is not allowed (exclusion zone)
				{
					LZB_DECODE_ERROR_RETURN("LZB match past end\n");
				}
				copy_overlap8_nooverrun(rp,match,ml);
				#endif
				
				rp += ml;
			}
			else
			{
				RR_ASSERT( cp < comp_end );
				
				// get 1-byte excess code
				//UINTr excess1 = *cp++;
				//UINTr excess1 = off&0xFF;
				UINTr excesslow = off&127;
				cp++; // consume 1
			
				FUZZ_MUNGE(excesslow);
		
				//if ( excess1 >= 128 )
				if ( off & 128 )
				{				
					//ml_control = (excess1>>3) & 0xF;
					ml_control = excesslow >> 3;
				
					SINTr ml = ml_control + LZB_MML;
				
					if_unlikely ( ml_control == 0xF )
					{					
						FUZZ_MUNGE(*((U8 *)cp));
							
						RR_ASSERT( cp+3 <= comp_end );
				
						// get more ml
						LZB_AddExcessML( cp, ml );
					}	
			
					UINTr myoff = off & 7;
				
					#if DO_OFFSET_CHECK
					if_unlikely ( myoff > (UINTr)(rp - dictionaryBase) )
					{
						LZB_DECODE_ERROR_RETURN("LZB offset out of bounds\n");
					}
					#endif
				
					#if DO_FAST_PATH
					if_unlikely ( ml >= rpSafeEnd-rp )
					{
						if ( ml > rpEnd-rp ) // match _touching_ rpEnd is not allowed (exclusion zone)
						{
							LZB_DECODE_ERROR_RETURN("LZB match past end\n");
						}

						// if it gets close to the end, just be careful about copying.
						copy_match_simple(rp,rp-myoff,ml);
						rp += ml;
						break; // drop to careful loop after
					}
				
					RR_ASSERT( rp+ml <= rpEnd );
				
					// NOTE: this writes up to 8 bytes past end, but we just
					// verified that the match ends by rpMatchEnd, which guarantees
					// we can write at least 8 bytes past that point.

					// low offset, can't do 8-byte grabs on initial few bytes
					U8 *from = rp - myoff;
					U8 *to = rp;
					to[0] = from[0];
					to[1] = from[1];
					to[2] = from[2];
					to[3] = from[3];
					from += c_overlap_offset_step1add[myoff];
					lz_copy4(to+4,from);
					// not actually "no overlap" but overlap distance is >=8 at this point
					// so we can copy 8 bytes at a time.
					if (ml > 8)
					{
						copy_no_overlap_long(to+8, from+8-c_overlap_offset_step2sub[myoff], ml - 8);
					}
					#else // DO_FAST_PATH
					if_unlikely ( ml >= rpEnd-rp ) // match _touching_ rpEnd is not allowed (exclusion zone)
					{
						LZB_DECODE_ERROR_RETURN("LZB match past end\n");
					}

					RR_ASSERT( rp+ml <= rpEnd );

					copy_match_simple(rp,rp-myoff,ml);
					#endif

					rp += ml;
				}
				else
				{
					UINTr myoff = RR_GET16_LE_UNALIGNED(cp); cp += 2;
					
					#if DO_OFFSET_CHECK
					if_unlikely ( myoff > (UINTr)(rp - dictionaryBase) )
					{
						LZB_DECODE_ERROR_RETURN("LZB offset out of bounds\n");
					}
					#endif
					
					const U8 * match = rp - myoff;
		
					SINTr ml = ml_control + LZB_MML;
					ml += excesslow;
							
					if_unlikely ( excesslow == 127 )
					{
						FUZZ_MUNGE(*((U8 *)cp));
						
						RR_ASSERT( cp+3 <= comp_end );
						
						// get more ml
						LZB_AddExcessML( cp, ml );
					}
				
					#if DO_FAST_PATH
					if_unlikely ( ml >= rpSafeEnd-rp )
					{
						if ( ml >= rpEnd-rp ) // match _touching_ rpEnd is not allowed (exclusion zone)
						{
							LZB_DECODE_ERROR_RETURN("LZB match past end\n");
						}

						copy_overlap8_nooverrun(rp,match,ml);
						rp += ml;
						break; // drop to careful loop
					}
					
					RR_ASSERT( rp+ml <= rpEnd );
					
					// 8-byte copier :
					copy_no_overlap_long(rp,match,ml);
					rp += ml;
					#else // DO_FAST_PATH
					if_unlikely ( ml >= rpEnd - rp ) // match _touching_ rpEnd is not allowed (exclusion zone)
					{
						LZB_DECODE_ERROR_RETURN("LZB match past end\n");
					}

					copy_overlap8_nooverrun(rp,match,ml);
					rp += ml;
					#endif
				}
			}		

			#if ! IS_LONG_LRL_CASE
			// FUZZ : continue skips the SET

			continue;		
			#endif
		}
		
