// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

	{
		RR_ASSERT( cp < comp_end );
		FUZZ_MUNGE(*((U8 *)cp));
		control = *cp++;
		lrl = control & 0xF;

//#if DO_FAST_PATH
//		rrprintf("FP control=0x%02x raw_to_end=%zd\n",control,rpEnd-rp);
//#else
//		rrprintf("SP control=0x%02x raw_to_end=%zd\n",control,rpEnd-rp);
//#endif
						
		// if lrl was <= 8 we did it, else need this :
		if_unlikely ( lrl > 8 )
		{
			// FUZZ : if we got long lrl, jump to last if we're doing a set
			
			if_unlikely ( lrl >= LZB_LRL_ESCAPE )
			{
				RR_ASSERT( cp+3 <= comp_end );
				FUZZ_MUNGE(*((U8 *)cp));
				#if DO_FAST_PATH
				const U8 * pre_excess_cp = cp;
				#endif
		
				LZB_AddExcessLRL( cp, lrl );
			
				// hide the EOF check here ?
				// has to be after the GetExcess
				#if DO_FAST_PATH
				if_unlikely(cp + lrl > comp_end_back)
				{
					// this LRL puts us outside the safe zone; roll back
					// this packet and retry it on the safe path.
					cp = pre_excess_cp - 1;
					break;
				}

				if_unlikely ( lrl >= rpSafeEnd-rp )
				{
					// this LRL puts us outside the safe zone; roll back
					// this packet and retry it on the safe path.
					cp = pre_excess_cp - 1;
					break;
				}

				RR_ASSERT( rp+lrl <= rpEnd );
				RR_ASSERT( cp+lrl <= comp_end );
				
				copy_no_overlap_long(rp,cp,lrl);
				
				rp += lrl;
				cp += lrl;
				
				LZB_COMP_PTR_FUZZ_CHECK(cp);
				#else
				// Safe path: need to validate and use safe copies
				if ( lrl > comp_end - cp || lrl > rpEnd - rp )
				{
					LZB_DECODE_ERROR_RETURN("LZB decomp past end\n");
				}

				RR_ASSERT( rp+lrl <= rpEnd );
				RR_ASSERT( cp+lrl <= comp_end );
	
				copy_no_overlap_nooverrun(rp,cp,lrl);
			
				rp += lrl;
				cp += lrl;

				if ( rp == rpEnd )
					break;
				LZB_COMP_PTR_FUZZ_CHECK(cp);
				#endif
			}
			else // > 8 but not 0xF
			{
				#if DO_FAST_PATH
				// no need to validate cp; the <16 bytes we process here
				// are covered by the FASTPATH+8
				if_unlikely ( lrl >= rpSafeEnd-rp )
				{
					// if this LRL puts us outside the safe zone, just roll back
					// this packet and retry it in the safe path.
					cp--;
					break;
				}

				RR_ASSERT( rp+16 <= rpEnd );
				RR_ASSERT( cp+16 <= comp_end );
				
				lz_copy16(rp,cp);
				
				rp += lrl;
				cp += lrl;
				#else
				// NOTE(fg): This case needs to clamp; the encoder intentionally produces
				// over-long lrls here sometimes.
				if_unlikely ( lrl > rpEnd-rp )
				{
					lrl = rrPtrDiff32( rpEnd - rp );
				}
				if_unlikely ( lrl > comp_end-cp )
				{
					LZB_DECODE_ERROR_RETURN("LZB decomp past end\n");
				}

				copy_no_overlap_nooverrun(rp,cp,lrl);
			
				rp += lrl;
				cp += lrl;
				if ( rp == rpEnd )
					break;
				#endif
			}
			
			#define IS_LONG_LRL_CASE 1
			
			#include "lzb_decode_inner2.inl"

			#undef IS_LONG_LRL_CASE

			// FUZZ : continue skips the SET
			continue;	
		}
		else
		{
			// FUZZ :
			//	copy 8 literals unconditional
			//	this can over-read comp & over-write raw per- QUANTUM , but not per BLOCK
			// but I don't have good information about that here
			// it's okay to always allow this
			//	because comp is protected by cp_safe
			//	and raw is protected by us staying within the safe zone
		
			#if DO_FAST_PATH
			// no need to validaet cp: the <8 bytes we process here
			// are covered by the FASTPATH
			RR_ASSERT( rp+8 <= rpEnd );
			RR_ASSERT( cp+8 <= comp_end );
					
			lz_copy8(rp,cp);
					
			rp += lrl;
			cp += lrl;
			#else
			if_unlikely ( lrl > rpEnd-rp || lrl > comp_end-cp )
			{
				LZB_DECODE_ERROR_RETURN("LZB decomp past end\n");
			}

			copy_no_overlap_nooverrun(rp,cp,lrl);
		
			rp += lrl;
			cp += lrl;
			if ( rp == rpEnd )
				break;
			#endif
				
			#define IS_LONG_LRL_CASE 0
			
			#include "lzb_decode_inner2.inl"

			#undef IS_LONG_LRL_CASE

			// no continue, SET drops through
		}
	}

	
