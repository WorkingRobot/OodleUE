// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.


/***

NEWLZ (Kraken) !!

***/
	
// RADFORCEINLINE			
static RADNOINLINE bool newLZ_decode_parse(
	const newLZ_chunk_arrays * arrays,
	U8 * to_ptr, U8 * chunk_base, U8 * chunk_end, U8 * window_base )
{
	// Encoder never produces chunks shorter than NEWLZ_MIN_CHUNK_LEN
	// but we need to test this for fuzz safety.
	if ( chunk_end - chunk_base < NEWLZ_CHUNK_NO_MATCH_ZONE )
		return false;

	const S32 * offsets = arrays->offsets;
	SINTa offsets_count = arrays->offsets_count;
	const U32 * excesses = arrays->excesses;
	SINTa excesses_count = arrays->excesses_count;
	const U8 * packets = arrays->packets;
	SINTa packets_count = arrays->packets_count;

	const S32 * offsets_ptr = offsets;
	const U32 * excesses_ptr = excesses;
	const U8 * literals_ptr = arrays->literals_ptr;
	SINTa literals_count = arrays->literals_count;

	//RAD_ALIGN(newLZ_dec_LOs,lastoffsets,16);
	newLZ_dec_LOs lastoffsets;
	newLZ_dec_LOs_Reset_Neg(lastoffsets);
	SINTa neg_offset = - NEWLZ_MIN_OFFSET;

	{
	SIMPLEPROFILE_SCOPE_N(newLZ_decode_parse,rrPtrDiff32(chunk_end - chunk_base));

		// Fuzz-safety high concept:
		// - We process limited-size groups of packets, one "checkpoint" at a time.
		// - We try to make the common case (neither LRL nor ML large) fast.
		// - In that common case (LRL <= kBigLRL and ML <= bigML) for all packets, we know how many output bytes we
		//   can produce and how many literals etc. we can consume, so we can do most of the periodic bounds checking
		//   per checkpoint instead of per packet.
		// - Match offsets, however, always need to be checked.
		// - Long LRLs or MLs also need to do extra checks.
		//
		// So here's what we actually do:
		// - Offsets can be consumed at a rate of at most 1/packet, excesses at 2/packet. We just make sure there's
		//   padding after them in our scratch mem and let them run over. If we catch them going past the end, we
		//   report an error.
		// - For literals, we can't assume padding, since we permit chunks with uncompressed literals where literals_ptr
		//   points straight into the compressed data. But for the common case (no long LRLs), we can bound how many
		//   literals are read per checkpoint. So we switch to a scratch buffer that has extra padding bytes
		//   (literal_scratch) if we're in risk of hitting the end.
		// - For the output buffer, we likewise have a bound on how many bytes can be generated per checkpoint if
		//   there's no big LRLs/MLs. We run a sloppy loop until we're within that many bytes from the end, and then
		//   a careful loop that checks all lens after.
		// - Long LRLs need to check:
		//   1. does this LRL move us past the literal checkpoint (close to end of literal buffer); if so, do a
		//      checkpoint after current packet. (To switch over to literal scratch buffer if necessary).
		//   2. does this LRL move us past the end of the literal buffer altogether? This means the stream is
		//      invalid. Since literals_check <= literals_end, for this to be true, 1. must also be true, so we can
		//      hide one branch in the other.
		//   3. does this LRL move the output pointer into the "no match" zone? Not allowed, since packet LRLs are always
		//      followed by matches.
		// - Long MLs need to check:
		//   1. does the match len extend past the end of the chunk? Never allowed.
		//   2. when in sloppy loop: did this match put us outside the output safe zone? If so, do a checkpoint after
		//      the current packet.

		static const SINTa kPacketsPerCheckpoint = 8;
		static const SINTa kBigLRL = 24; // any LRL longer than this needs to do bounds checks
		static const SINTa kBigML = 16; // any ML longer than this needs to do bounds checks
		static const SINTa kLitOverread = 8; // how many bytes we can over-read as part of literal copying
		static const SINTa kLitsPerCheckpoint = kBigLRL * kPacketsPerCheckpoint + kLitOverread; // number of literals consumed/checkpoint if there were no long LRLs
		static const SINTa kOutputPerCheckpoint = (kBigLRL + kBigML) * kPacketsPerCheckpoint; // number of out bytes written/checkpoint if there are no big LRLs or MLs

		// NOTE(fg): make sure we have enough padding space for our sloppy offset/excess checking
		// to not run off the end of our actual scratch mem.
		// Can read 2 excesses (excess LRL, excess ML) per packet.
		static const SINTa kExcessesMaxOverrun = kPacketsPerCheckpoint*2;
		RR_COMPILER_ASSERT(RR_MAX(kExcessesMaxOverrun * sizeof(*excesses), kPacketsPerCheckpoint * sizeof(*offsets)) <= NEWLZ_EXTRA_SCRATCH_MEM_FOR_FUZZ);

		const U8 * packets_end = packets + packets_count;
		const U8 * match_zone_end = ptr_sub_saturate(chunk_end, NEWLZ_CHUNK_NO_MATCH_ZONE, chunk_base); // last location where matches may begin
		const U8 * match_end = chunk_end - NEWLZ_MATCH_END_PAD; // matches have to end by here
		const U8 * output_safe_end = ptr_sub_saturate(chunk_end, kOutputPerCheckpoint + NEWLZ_CHUNK_NO_MATCH_ZONE, chunk_base);
		const U8 * literals_end = literals_ptr + literals_count;
		const U8 * literals_check = literals_end;
		const U32 * excesses_end = excesses + excesses_count; // NOTE(fg): assumes excesses allocated after offsets from the same chunk!
		RR_ASSERT( (UINTa)excesses >= (UINTa)offsets && ((UINTa)excesses - (UINTa)offsets) < (256*1024) );

		// Set the extra padding excesses to NEWLZ_PACKET_LRL_MAX.
		// The decoder expects the excesses to be in range and correctly biased;
		// the padding values should be as well, or we get in trouble with the
		// match lengths.
		for (SINTa i = 0; i < kExcessesMaxOverrun; i++)
			const_cast<U32*>(excesses_end)[i] = NEWLZ_PACKET_LRL_MAX;

		// literal scratch buffer
		// bytes are placed so the last literal from the input ends up at
		// position kLitsPerCheckpoint-1 (with kLitsPerCheckpoint bytes padding after).
		U8 literal_scratch[kLitsPerCheckpoint*2];

		// copy the last few literals over (might as well do this now)
		{
			SINTa num_copy = RR_MIN(kLitsPerCheckpoint, literals_count);
			literals_check = literals_end - num_copy;
			memcpy(literal_scratch + kLitsPerCheckpoint - num_copy, literals_check, num_copy);
			// this is unnecessary except to make valgrind happy :
			memset(literal_scratch + kLitsPerCheckpoint, 0, kLitsPerCheckpoint); // clear second half (for brownie points)
		}

		#define SWITCH_TO_LITERAL_SCRATCH_BUF() do { \
			literals_ptr = (literal_scratch + kLitsPerCheckpoint) - (literals_end - literals_ptr); \
			literals_end = literals_check = literal_scratch + kLitsPerCheckpoint; \
		} while (0)
		
		// main loop that runs while we're far enough away from the end
		while (packets < packets_end && to_ptr < output_safe_end)
		{
			// inner internally loops on checkpoint groups
		
			#define NEWLZ_PARSE_CAREFUL_OUTPUT 0
			#include "newlz_decode_parse_inner.inl"
		}

		// careful loop that runs when we're close to the end of the output buffer
		// && to_ptr <= match_zone_end)
		//	 -> not needed, checked inside via (to_ptr+lrl <= match_zone_end)
		while (packets < packets_end )
		{
			// inner does not internally loop
			
			#define NEWLZ_PARSE_CAREFUL_OUTPUT 1
			#include "newlz_decode_parse_inner.inl"
		}

		RR_ASSERT( packets == packets_end );
		//if ( packets != packets_end ) return false;
		
		#undef SWITCH_TO_LITERAL_SCRATCH_BUF

		// check whether we consumed all our input
		if ( offsets_ptr != offsets + offsets_count || excesses_ptr != excesses_end )
			return false;

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

			{
				RR_ASSERT_IF_NOT_CORRUPT( -neg_offset >= NEWLZ_MIN_OFFSET );
									
				// the final literal run better consume all the remaining lits!
				if ( literals_end - literals_ptr != lrl )
					return false;
			
				#if NEWLZ_DECODE_LITERALS_TYPE == NEWLZ_LITERALS_TYPE_RAW
				
				memcpy(to_ptr,literals_ptr,lrl);
				to_ptr += lrl;
				literals_ptr += lrl;

				#else // SUB

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

				#endif // RAW or SUB
			}
		}
	
	} // profile scope ; parse loop	

	RR_ASSERT( to_ptr == chunk_end );
	if ( to_ptr != chunk_end )
		return false;
	
	//-----------------------------------------------------------
		
	return true;
}

