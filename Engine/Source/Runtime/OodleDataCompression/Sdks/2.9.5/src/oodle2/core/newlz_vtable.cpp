// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "newlz_vtable.h"
#include "ctmf.h"
#include "rrlzh_lzhlw_shared.h"
#include "newlz_arrays.h"
#include "newlz_arrays.inl"
#include "threadprofiler.h"
#include "newlz_speedfit.h"
#include "newlz_shared.h"
#include "rrlogutil.h"
#include "oodlejob.h"
#include "matchfinder.h"

//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"

//#include "../ext/rrsimplemalloc.h" // for OodleXMalloc_LogMemUse
#define OodleXMalloc_LogMemUse(x)

#if OODLE_PLATFORM_HAS_ADVANCED_MATCHERS

#include "longrangematcher.h"
#include "oodleconfigvalues.h"
#include "suffixtrie.h"

#endif

OODLE_NS_START

//===================================================

#if 0
// g_newlz_do_compress_to_temp is set by test_framework when guard pages are enabled
extern bool g_newlz_do_compress_to_temp;
bool g_newlz_do_compress_to_temp = false;
#endif

//===================================================

newlz_vtable::newlz_vtable()
{
	RR_ZERO(*this); // zero is good
	// zero is okay for newlz_scratchblock too
	carried_encoder_state_chunktype = -1;
	// NOTE: leaves speedfit at 0, derived codecs need to set in Fill_VTable!
}

newlz_vtable::~newlz_vtable()
{
	// carried_encoder_state has a destructor

	if ( fp_free_matcher )
	{
		(*fp_free_matcher)(matcher);
		fp_free_matcher = NULL;
	}
}

//===================================================

SINTa newlz_enc_mem_bound(newlz_vtable * vtable,SINTa wholeRawLen)
{
	// Hydra just punts for now and does not call to here
	RR_ASSERT( vtable->compressor != OodleLZ_Compressor_Hydra );

	// ctmf mem size was set by newlz_vtable_setup_ctmf
	//	this is the majority of mem use
	SINTa ret = vtable->ctmf_mem_size;
	RR_ASSERT( ret != 0 ); // should be filled out
	
	const int newlz_chunk_len = vtable->chunk_len;
	SINTa chunk_len = RR_MIN( wholeRawLen, newlz_chunk_len );
	
	// Selkie :
	ret += 4*1024;
	ret += chunk_len*3;
	
	// there is no entropy_flags for ALLOW_HUFF
	// it's implicit if not Selkie
	if ( vtable->compressor != OodleLZ_Compressor_Selkie )
	{
		//Mermaid & higher
		// Mermaid is very much 5*chunk_len + a little
	
		ret += 10*1024;
		ret += chunk_len*2;
	}
	
	if ( vtable->compressor == OodleLZ_Compressor_Kraken || 
		vtable->compressor == OodleLZ_Compressor_Leviathan )
	{
		// Kraken base usage is 6*chunk_len + a little
		// alt offsets uses some, could check NEWLZ_BITSTREAM_FLAG_ALT_OFFSETS
		ret += 2*1024 + chunk_len;
	}
			
	if ( (vtable->entropy_flags & NEWLZ_ARRAY_FLAG_ALLOW_TANS) ||
		(vtable->entropy_flags & NEWLZ_ARRAY_FLAG_ALLOW_RLEHUFF) )
	{
		// entropy modes that need a temp array
		ret += 8*1024 + chunk_len;
	}
	
	if ( vtable->entropy_flags & NEWLZ_ARRAY_FLAG_ALLOW_SPLIT )
	{
		// split makes a histo by chunk
		//	num chunks is scaled by chunk_len
		//	to try to make total histo mem use ~ chunk_len
	
		// Kraken Normal
		//ret += 64*1024 + chunk_len;
		ret += 16*1024 + RR_MIN(chunk_len,16*1024) + chunk_len;
	}
		
	if ( vtable->entropy_flags & NEWLZ_ARRAY_FLAG_ALLOW_SPLIT_INDEXED )
	{
		// only used by Leviathan
		//	(Kraken only uses indexed split in optimals which don't go through here)
		// checking for SPLIT_INDEXED separately from == Leviathan
		//	 is a bit bogus with the way things are set up now
		// but we can stress it by making Kraken Normal use SPLIT_INDEXED
		
		// highest mem use at -zs0
		
		// multiarrays extra memory use
		// varies with level
		// encoder_max_num_arrays(level) * sizeoF(Histo)
		//	histos are 1k each , up to 32 of them (*2)
		// indexed interval is 6 bytes per input byte
		// intervals live at the same time as output scratch
	
		// this is enough to cover level 4
		//	higher levels don't go through here
		//  lower levels could use less memory
	
		//ret += 256*1024 + chunk_len*8;
		//ret += chunk_len*10;
		ret += 128*1024 + chunk_len*12;
	}
	
	if ( vtable->compressor == OodleLZ_Compressor_Leviathan )
	{
		if ( vtable->entropy_flags & NEWLZ_ARRAY_FLAG_ALLOW_SPLIT_INDEXED )
		{
			ret -= chunk_len*2;
		}
	
		// needs room for big newlzhc_histo ,
		//	some extra arrays for pos-packets and such that are proportional to chunk_len
		// do not include indexed split mem use here, that's below
		ret += 128*1024 + chunk_len*6;
	}
	
	return ret;
}

//===================================================

// put one OODLELZ_BLOCK (256k) using newlz chunks (128k)
static SINTa newLZ_encode_block_vtable(
	const newlz_vtable * vtable,
	newlz_encoder_scratch * scratch,
	const U8 * dictionaryBase,
	const U8 * block_ptr,int block_len,
	U8 * comp,U8 * comp_end,
	SINTa block_pos,
	LZQuantumHeader * pLZQH,
	F32 * pJ,
	rrArenaAllocator * arena,
	UnpackedMatchPair * matches
	)
{
	if ( block_len < NEWLZ_MIN_CHUNK_LEN )
		return block_len+1;

	SIMPLEPROFILE_SCOPE_N(newLZ_encode_block_vt,block_len);
		
	// LZQH no flags set -> chunk len = 128k
	U32 entropy_flags = vtable->entropy_flags;
	const int newlz_chunk_len = vtable->chunk_len;
	F32 lambda = vtable->lambda;
	const OodleSpeedFit * speedfit = vtable->speedfit;

	const U8 * block_end = block_ptr + block_len;
	const U8 * chunk_ptr = block_ptr;
	
	U8 * comp_ptr = comp;
	
	// at this point, the block header & the large quantum header have been written
	// I still need to write LZ chunk header 
	RR_ASSERT( rrPtrDiff(comp_end - comp) >= (block_len+NEWLZ_CHUNK_HEADER_SIZE) );
	
	F32 block_total_J = 0;
	OodleKrakenChunkDeadlines deadlines;
	
	// this typically loops twice : (two 128k chunks in 256k block)
	while ( chunk_ptr < block_end )
	{
		// point at correct half of pre-decoded match pairs
		if ( matches )
			scratch->match_pairs = matches + (chunk_ptr - block_ptr) * vtable->find_all_matches_num_pairs;

		int chunk_len = RR_MIN( rrPtrDiff32( block_end - chunk_ptr ) , newlz_chunk_len );
		
		// require at least chunk_len+3 to be able to write uncomp header
		RR_COMPILER_ASSERT( NEWLZ_CHUNK_HEADER_SIZE == 3 ); // 24 bit
		if ( rrPtrDiff(comp_end-comp_ptr) < chunk_len+NEWLZ_CHUNK_HEADER_SIZE )
		{
			ooLogError("newLZ_encode_block_vtable: not enough room for uncomp chunk.\n");
			
			// fail!
			return block_len+1;
		}
		
		// *does* count time to memcpy :
		F32 raw_J = NEWLZ_CHUNK_HEADER_SIZE + speedfit_memcpy_array_J(lambda,speedfit,chunk_len);
		speedfit->deadlines_Kraken(&deadlines, chunk_len);
				
		// NEWLZ_HUFF_ARRAY_MIN_SIZE is < LZ chunk min len (32 < 128)
		if ( chunk_len < NEWLZ_HUFF_ARRAY_MIN_SIZE )
		{
			// send uncompressed :
			
			U32 header = (U32)chunk_len;
			header |= 1<<23;
			//header |= 0 << 19; // LZ chunk_type
		
			RR_PUT24_BE_NOOVERRUN(comp_ptr,header);
			comp_ptr += 3; // NEWLZ_CHUNK_HEADER_SIZE
			
			memcpy(comp_ptr,chunk_ptr,chunk_len);
			comp_ptr += chunk_len;
						
			block_total_J += raw_J;
			chunk_ptr += chunk_len;
			continue;
		}
	
		// memset test is on the *chunk* level :
		if ( rrIsMemset(chunk_ptr,chunk_len) )
		{
			// degenerate!
			// send as a huff-chunk ; newlz_array has a memset decoder for degenerate cases
			// -> this is a nop, this will get picked up anyway even if we don't do this test explicitly
			//	but wtf just make sure
			// -> note that doing this means the string matcher isn't updated on this chunk, which is fine
			rrPrintf_v2("Memset chunk\n");
			
			// note : even if try_huff_chunks is off, we will use them for memset chunks
			
			F32 memset_J = LAGRANGE_COST_INVALID;
			SINTa chunk_comp_len = newLZ_put_array_memset(comp_ptr,comp_end,chunk_ptr,chunk_len,entropy_flags,lambda,speedfit,&memset_J,arena);
			
			// need to check expansion?
			//	(can occur if chunk_len is tiny)
			// -> no:
			RR_ASSERT( chunk_len >= NEWLZ_HUFF_ARRAY_MIN_SIZE );
			
			RR_ASSERT( memset_J != LAGRANGE_COST_INVALID );
			RR_ASSERT( chunk_comp_len > 0 );
			
			block_total_J += memset_J;
			chunk_ptr += chunk_len;
			comp_ptr += chunk_comp_len;
			continue;
		}
		
		SINTa chunk_pos = block_pos + rrPtrDiff(chunk_ptr - block_ptr);
		
		// skip 3 bytes to save space for the header I will put :
		U8 * chunk_comp_len_ptr = comp_ptr;
		comp_ptr += NEWLZ_CHUNK_HEADER_SIZE;
		
		int chunk_type = -1;
		SINTa chunk_comp_len = RR_S32_MAX;
		F32 chunk_comp_J = LAGRANGE_COST_INVALID;

		//=======================================
		// LZ CHUNK COMPRESS CALL :
	
		chunk_comp_len = (*vtable->fp_encode_chunk)(vtable,scratch,dictionaryBase,chunk_ptr,chunk_len,comp_ptr,comp_end,chunk_pos,&chunk_type,&chunk_comp_J,&deadlines);
	
		RR_ASSERT( chunk_type != -1 || chunk_comp_len >= chunk_len );
		
		// note if chunk_comp_len >= chunk_len , signalling expansion or failure
		//	then J and chunk_type are not filled out
		RR_ASSERT( chunk_comp_len < chunk_len || chunk_comp_J == LAGRANGE_COST_INVALID );

		// note chunk_comp_len & chunk_comp_J are the pure LZ sizes at this point
		// add my header to J for proper compares :
		//	(but not on chunk_comp_len)
		chunk_comp_J += NEWLZ_CHUNK_HEADER_SIZE;

		SINTa huff_comp_len = chunk_len;
		F32 huff_J = LAGRANGE_COST_INVALID;
		U8 * huff_comp_ptr = NULL;
		if ( vtable->try_huff_chunks )
		{
			SIMPLEPROFILE_SCOPE_N(whole_huff_chunk,chunk_len);
	
			RR_ASSERT( vtable->compressor != OodleLZ_Compressor_Selkie );
	
			// this trying huff-only chunks is pretty expensive
			
			huff_J = RR_MIN(chunk_comp_J,raw_J); // initialize with chunk_comp_J for early out in newLZ_put_array_huff
		
			// chunk_comp_len == RR_S32_MAX is the memset quantum case
			
			// @@ wait, what? why? newLZ_put_array should be defined so that putting over is okay
			// huff_comp_ptr does NOT output on top of earlier compressed data
			// find scratch space for all-huff compressed output :
			U8 * huff_comp_ptr_end;
			
			if ( scratch->newlzf_arrays_space.get() != NULL )
			{
				huff_comp_ptr = scratch->newlzf_arrays_space.getU8();
				huff_comp_ptr_end = huff_comp_ptr + scratch->newlzf_arrays_space.m_size;
			}
			else
			{			
				// newlz_literal_space_reserve_size must always be the same reserve size
				scratch->literals_space.extend( newlz_literal_space_reserve_size(chunk_len) ,arena);
				huff_comp_ptr = scratch->literals_space.getU8();
				huff_comp_ptr_end = huff_comp_ptr + scratch->literals_space.m_size;
			}			
	
			rrPrintf_v2("NEWLZ put whole huff chunk :");

			huff_comp_len = newLZ_put_array(huff_comp_ptr,huff_comp_ptr_end,chunk_ptr,chunk_len,entropy_flags,lambda,speedfit,&huff_J,deadlines.literals_only,arena,vtable->level,NULL);
			
			rrPrintf_v2("%d\n",huff_comp_len);
			
			if ( huff_comp_len <= 0 || huff_comp_len >= chunk_len )
			{
				// not compressed, make sure we don't pick this
				huff_J = LAGRANGE_COST_INVALID;
				
				// (if newLZ array chose uncompressed, its J does NOT include memcpy time,
				//	it assumes you can point at, which I can't here,
				//	so its J is too low; use raw_J instead)
			}
		}
		
		if ( chunk_comp_J < raw_J && chunk_comp_J <= huff_J && chunk_comp_len < chunk_len )
		{
			// send compressed
			
			rrPrintf_v2("Chunk %9d : R %8.1f  H %8.1f  [LZ %8.1f]\n",chunk_pos,
				(raw_J == LAGRANGE_COST_INVALID) ? -1 : raw_J,
				(huff_J == LAGRANGE_COST_INVALID) ? -1 : huff_J,
				(chunk_comp_J == LAGRANGE_COST_INVALID) ? -1 : chunk_comp_J);
		
			//rrPrintf_v2("CHUNK : %d : %d\n",chunk_len,chunk_comp_len);
			
			// this must be compatible with the newLZ_put_array header
			//	newLZ array has a header at <<21
			//	values 0 and 1 are newLZ array types
			//	so take value 7 for Kraken data
		
			U32 header = (U32)chunk_comp_len;
			header |= 1<<23;
			header |= chunk_type << 19;
					
			RR_PUT24_BE_NOOVERRUN(chunk_comp_len_ptr,header);
			
			block_total_J += chunk_comp_J;
		}
		else if ( huff_J < raw_J )
		{			
			RR_ASSERT( huff_comp_len < chunk_len );
		
			// NOTE : checking raw vs huff is actually redundant
			//	since huff newlz_array does that internally as well
			//	so in that case huff_J always == raw_J
			//	and the huff data is the same as the raw data we make
			
			rrPrintf_v2("Chunk %9d : R %8.1f  [H %8.1f]  LZ %8.1f\n",chunk_pos,
				(raw_J == LAGRANGE_COST_INVALID) ? -1 : raw_J,
				(huff_J == LAGRANGE_COST_INVALID) ? -1 : huff_J,
				(chunk_comp_J == LAGRANGE_COST_INVALID) ? -1 : chunk_comp_J);
			
			//rrPrintf_v2("HCHUNK : %d : %d\n",chunk_len,huff_comp_len);
			
			// put the huff array at the comp len :
			comp_ptr = chunk_comp_len_ptr;
			memcpy(comp_ptr,huff_comp_ptr,huff_comp_len);
			chunk_comp_len = huff_comp_len;
			
			block_total_J += huff_J;
		}
		else
		{
			// expansion :
			chunk_type = 0;
			chunk_comp_len = chunk_len;
			memcpy(comp_ptr,chunk_ptr,chunk_len);
			
			rrPrintf_v2("Chunk %9d : [R %8.1f]  H %8.1f  LZ %8.1f\n",chunk_pos,
							(raw_J == LAGRANGE_COST_INVALID) ? -1 : raw_J,
							(huff_J == LAGRANGE_COST_INVALID) ? -1 : huff_J,
							(chunk_comp_J == LAGRANGE_COST_INVALID) ? -1 : chunk_comp_J);
			
			//rrPrintf_v2("RAWCHUNK : %d : %d\n",chunk_len,chunk_comp_len);
		
			// this must be compatible with the newLZ_put_array header
			//	newLZ array has a header at <<21
			//	values 0 and 1 are newLZ array types
			//	so take value 7 for Kraken data
			
			// @@ for some reason I'm sending raw data through the "newlz data" path
			//	(not the HCHUNK newlz_array path)
			//  either one can do it in exactly the same way
		
			//*
			// send raw chunks through the newlz compressed data path :
			U32 header = (U32)chunk_comp_len;
			header |= 1<<23;
			header |= chunk_type << 19;
			/*/
			// send raw chunks through the newlz_array HUFF CHUNK path :
			U32 header = (U32)chunk_len;
			/**/
		
			RR_PUT24_BE_NOOVERRUN(chunk_comp_len_ptr,header);
			
			block_total_J += raw_J;
		}
		
		chunk_ptr += chunk_len;
		comp_ptr += chunk_comp_len;
	}
	
	*pJ = block_total_J;
	
	SINTa block_total_complen = rrPtrDiff( comp_ptr - comp );
	
	RR_ASSERT( block_total_J >= block_total_complen );
	
	#if 0
	F32 block_total_cycles = (block_total_J - block_total_complen)/lambda;
	
	/*
	rrprintfvar(block_len);
	rrprintfvar(block_total_complen);
	rrprintfvar(block_total_J);
	rrprintfvar(block_total_cycles);
	*/
	
	static F32 s_total_cycles = 0;
	s_total_cycles += block_total_cycles;
	rrprintfvar(s_total_cycles);
	#endif
	
	return block_total_complen;
}

static SINTa newlz_compress_vtable_block_outer(
	newlz_vtable * vtable,
	newlz_encoder_scratch * scratch,
	U8 * compStart,
	U8 * compEnd,
	const U8 * rawPtr,
	SINTa block_pos,
	int block_len,
	rrbool blockIsReset,
	const U8 * dictionaryBase,
	rrArenaAllocator * arena,
	UnpackedMatchPair * matches)
{
	const OodleLZ_CompressOptions * pOptions = vtable->pOptions;
	U8 * compPtr = compStart;

	// should have room for this block :
	RR_ASSERT( rrPtrDiff(compEnd - compStart) >= OodleLZ_GetCompressedBufferSizeNeeded(vtable->compressor,block_len) );

	//-----------------------------------------------------
	// write LZHeader for block :

	U8 * blockHeaderPtr = compPtr;
	rrbool sendQuantumCRCs = pOptions->sendQuantumCRCs;

	// put block header :
	if ( 1 )
	{
		LZBlockHeader header = { 0 };
		header.version = RAD_LZ_HEADER_VERSION;
		header.decodeType = vtable->decodeType;
		header.chunkIsReset = blockIsReset;
		header.chunkHasQuantumCRCs = sendQuantumCRCs;
		compPtr = LZBlockHeader_Put(header,compPtr);
	}

	//-----------------------------------------------------
	// check for memset/wholematch special quanta :

	U8 * quantumHeaderPtr = compPtr;

	LZQuantumHeader LZQH = { 0 };
	LZQH.compLen = block_len-1;

	// detect memset quantum (large quantum = whole block) :
	//	 (this is also done at the chunk level, a bit redundant, could remove)
	rrbool quantumIsMemset = MemsetQuantum_Test(rawPtr,block_len);
	if ( quantumIsMemset )
	{
		LZQH.crc = *rawPtr;
		LZQH.compLen = 0;

		// whole block memset can be signalled in the LargeQuantum header :
		int packedQHLen = LZLargeQuantumHeader_Put(quantumHeaderPtr,&LZQH,sendQuantumCRCs,block_len);
		compPtr += packedQHLen;
		
		// done with block
		return rrPtrDiff(compPtr - blockHeaderPtr);
	}

	/*
	// could use CheckWholeMatchQuantum_LRM here
	// -> not much point; 2 GB dictionary deduping is not really our usage case
	// minimum len for wholeMatch :
	// detect whole-match quantum :
	//	(this would be exceedingly rare for entire 256k blocks)
	//	(was more common for 16k blocks)
	// -> NOTE these are illegal in "Reduced" profile
	if Profile != Reduced
	if ( CheckWholeMatchQuantum(&LZQH,rawBuf,rrPtrDiff(quantumPtr-rawBuf),quantumLen32,tmf,lastWholeMatchOffset) )
	{
		lastWholeMatchOffset = LZQH.wholeMatchOffset;

		int packedQHLen = LZLargeQuantumHeader_Put(quantumHeaderPtr,&LZQH,sendQuantumCRCs,quantumLen32);
		compPtr += packedQHLen;

		quantumPtr += quantumLen;
		continue;
	}
	*/

	//-------------------------------
	// normal quantum :
	// Large Quantum = whole block

	// put QH initially to reserve space :
	int packedQHLen = LZLargeQuantumHeader_Put(quantumHeaderPtr,&LZQH,sendQuantumCRCs,block_len);
	compPtr += packedQHLen; // packedQHLen is usually 2

	U8 * compPtrPostQH = compPtr;

	//-------------------------------------

	SINTa block_comp_len;
	F32 block_comp_J = LAGRANGE_COST_INVALID;
	block_comp_len = newLZ_encode_block_vtable(vtable,scratch,dictionaryBase,rawPtr,block_len,compPtr,compEnd,block_pos,&LZQH,&block_comp_J,arena,matches);

	// *does* count time to memcpy :
	F32 block_raw_J = NEWLZ_CHUNK_HEADER_SIZE + speedfit_memcpy_array_J(vtable->lambda,vtable->speedfit,block_len);

	if ( block_comp_len >= block_len || block_comp_J >= block_raw_J )
	{
		// block expanded ; put a "memcpy" header :
		// write memcpy block header at blockHeaderPtr

		LZBlockHeader header = { 0 };
		header.version = RAD_LZ_HEADER_VERSION;
		header.decodeType = vtable->decodeType;
		header.chunkIsMemcpy = true;
		header.chunkIsReset = blockIsReset;
		header.chunkHasQuantumCRCs = false;
		compPtr = LZBlockHeader_Put(header,blockHeaderPtr);

		memcpy(compPtr,rawPtr,block_len);

		// advance to next quantum :
		compPtr += block_len;

		// done with block
		return rrPtrDiff(compPtr - blockHeaderPtr);
	}

	// only tries encoder2 if encoder1 didn't expand :

	newlz_vtable * pvtable2 = vtable->pvtable2;
	while( pvtable2 )
	{
		RR_ASSERT( vtable->compressor == OodleLZ_Compressor_Hydra ); // was Kraken
		RR_ASSERT( pvtable2->compressor == OodleLZ_Compressor_Mermaid ||
					pvtable2->compressor == OodleLZ_Compressor_Leviathan );

		// Hydra
		// try second compressor :
		
		// compress into some scratch mem
		scratch->comp2_space.extend( OodleLZ_GetCompressedBufferSizeNeeded(pvtable2->compressor,block_len), arena );
		U8 * compPtr2 = scratch->comp2_space.getU8();
		U8 * compEnd2 = scratch->comp2_space.endU8();

		LZQuantumHeader LZQH2 = { 0 };

		F32 block_comp_J2 = LAGRANGE_COST_INVALID;
		SINTa block_comp_len2 = newLZ_encode_block_vtable(pvtable2,scratch,dictionaryBase,rawPtr,block_len,compPtr2,compEnd2,block_pos,&LZQH2,&block_comp_J2,arena,matches);

		if ( block_comp_J2 < block_comp_J )
		{
			// okay, take it !

			rrPrintf_v2("Hydra WINNER : %s : %1.f < %.1f : %d vs %d\n",
				OodleLZ_Compressor_GetName(pvtable2->compressor),
				block_comp_J2,block_comp_J,
				block_comp_len2,block_comp_len);

			// memcpy scratch comp Ptr
			memcpy(compPtr,compPtr2,block_comp_len2);
			block_comp_len = block_comp_len2;

			// fix the blockHeaderPtr
			LZBlockHeader header = { 0 };
			header.version = RAD_LZ_HEADER_VERSION;
			header.decodeType = pvtable2->decodeType; // <- pvtable2
			header.chunkIsReset = blockIsReset;
			header.chunkHasQuantumCRCs = sendQuantumCRCs;
			U8 * after_blockHeaderPtr = LZBlockHeader_Put(header,blockHeaderPtr);

			RR_ASSERT_ALWAYS( after_blockHeaderPtr + packedQHLen == compPtr );

			LZQH = LZQH2;
		}
		else
		{
			rrPrintf_v2("Hydra LOSER : %s : %1.f >= %.1f : %d vs %d\n",
				OodleLZ_Compressor_GetName(pvtable2->compressor),
				block_comp_J2,block_comp_J,
				block_comp_len2,block_comp_len);
		}

		pvtable2 = pvtable2->pvtable2;
	}

	// re-put the quantum header now that I know compLen

	LZQH.compLen = S32_checkA(block_comp_len);
	if ( sendQuantumCRCs )
		LZQH.crc = LZQuantumHeader_ComputeCRC(compPtrPostQH,block_comp_len);

	rrPrintf_v2("EBLOCK : %d : %d : %d : %d\n",block_pos,blockIsReset,block_len,block_comp_len);

	// make sure we put the same amount because we reserved space earlier :
	int packedQHLen2 = LZLargeQuantumHeader_Put(quantumHeaderPtr,&LZQH,sendQuantumCRCs,block_len);
	RR_UNUSED_VARIABLE(packedQHLen2);
	RR_ASSERT( packedQHLen == packedQHLen2 );

	// advance to next quantum :
	compPtr += block_comp_len;

	// check the header :
	RR_ASSERT( rrPtrDiff32( compPtrPostQH - quantumHeaderPtr ) == packedQHLen );
	RR_ASSERT( rrPtrDiff32( compPtr - compPtrPostQH ) == LZQH.compLen );

	return rrPtrDiff(compPtr - blockHeaderPtr);
}

// These are the block parse jobs we hand out
struct BlockParseJob
{
	// input: "globals"
	newlz_vtable * vtable;
	newlz_encoder_scratch * scratch;
	rrArenaAllocator * arena;
	UnpackedMatchPair * matches;
	const U8 * dictionaryBase;
	U8 * overall_comp_end;

	// input: variable per block
	U8 * * pcompPtr; // pointer to compPtr (indirect because at time of job launch, it's not yet known)
	const U8 * rawPtr;
	SINTa block_pos;
	int block_len;
	rrbool blockIsReset;

	// output
	U8 * compPtrOut; // points just past the end of this block's output

	// tracking
	OodleJob job; // the active background job
};

static void OODLE_CALLBACK newlz_compress_block_parse(void * job_data)
{
	BlockParseJob * job = static_cast<BlockParseJob*>(job_data);

	U8 * compPtr = *job->pcompPtr;

	SINTa block_comp_space = OodleLZ_GetCompressedBufferSizeNeeded(job->vtable->compressor,job->block_len);
	RR_ASSERT( rrPtrDiff(job->overall_comp_end - compPtr) >= block_comp_space );
	U8 * compEnd = compPtr + block_comp_space;

	SINTa blockTotCompLen = newlz_compress_vtable_block_outer(job->vtable,job->scratch, compPtr, compEnd,
		job->rawPtr, job->block_pos, job->block_len, job->blockIsReset, job->dictionaryBase,
		job->arena, job->matches);

	job->compPtrOut = compPtr + blockTotCompLen;
}

static SINTa newlz_compress_vtable_sub(
	newlz_vtable * vtable,
	newlz_encoder_scratch * scratch,
	const U8 * raw,U8 * comp,SINTa raw_len,
	const U8 * dictionaryBase,
	const U8 * zeroPosPtr,
	IncrementalMatchFinder * mf,
	rrArenaAllocator * arena)
{
	THREADPROFILESCOPE("newlz_compress_vts");

/*
#ifdef OODLE_BUILDING_DATA
#ifndef OODLE_BUILDING_DLL
	OodleXMalloc_LogMemUse("newlz_compress_vtable_sub start: ");
#endif
#endif
*/

	const U8 * rawPtr = U8_void(raw);
	const U8 * rawEnd = rawPtr + raw_len;

	U8 * comp_ptr = U8_void(comp);
	U8 * comp_end = comp_ptr + OodleLZ_GetCompressedBufferSizeNeeded(vtable->compressor,raw_len);

	//OodleLZ_CompressionLevel level = vtable->level;
	const OodleLZ_CompressOptions * pOptions = vtable->pOptions;

	static const UINTa MAX_PARSE_JOBS = 4;
	BlockParseJob parse_job[MAX_PARSE_JOBS];
	UINTa next_submit_job = 0;
	UINTa next_retire_job = 0;

	// Set parser job count from compression options
	UINTa parse_job_count = 1; // default to no threading

	// if we have no match finder, no point in threading
	if ( mf )
	{
		bool has_jobs = Oodle_IsJobSystemSet();
	
		if ( pOptions->jobify >= OodleLZ_Jobify_Normal ||
			 ( pOptions->jobify == OodleLZ_Jobify_Default && has_jobs ) )
		{
			// 3 buffers give smooth operation usually, 2 is a bit jerky
			if ( pOptions->jobify == OodleLZ_Jobify_Aggressive )
				parse_job_count = 3;
			else
				parse_job_count = 2;
		}
		
		// no point in using more jobs than we have blocks
		parse_job_count = RR_MIN(parse_job_count, (UINTa)(raw_len + OODLELZ_BLOCK_LEN-1) / OODLELZ_BLOCK_LEN);
		parse_job_count = RR_MAX(parse_job_count, 1);

		RR_ASSERT_ALWAYS( parse_job_count >= 1 && parse_job_count <= MAX_PARSE_JOBS );
		
		rrPrintf_v2("parse_job_count=%d , jobify opt=%d=%s , has_jobs=%d\n",
			parse_job_count,(int)pOptions->jobify,OodleLZ_Jobify_GetName(pOptions->jobify),(int)has_jobs);
	}

	// ALLocate match scratch area if we have a match finder
	UnpackedMatchPair * match_base = NULL;

	if ( mf )
	{
		RR_ASSERT( vtable->find_all_matches_num_pairs > 0);

		// sharing this only works if all vtables agree on how many matches they want
		for ( newlz_vtable * pvtable2 = vtable->pvtable2; pvtable2; pvtable2 = pvtable2->pvtable2 )
		{
			RR_ASSERT( pvtable2->find_all_matches_num_pairs == vtable->find_all_matches_num_pairs );
		}

		// Allocate match pairs area enough for match_job_count blocks or the entire data, whichever is shorter
		int max_match_buf = (int) RR_MIN( OODLELZ_BLOCK_LEN * (int)parse_job_count, raw_len );
		SINTa match_space_needed = sizeof(UnpackedMatchPair) * max_match_buf * vtable->find_all_matches_num_pairs;

		#if 0 // log :
		rrprintfvar(match_job_count);
		rrprintfvar(match_space_needed);
		#endif

		scratch->matches_space.extend( match_space_needed, arena);
		match_base = (UnpackedMatchPair *)scratch->matches_space.m_ptr;
	}

	// Set up the parse jobs
	for (int i = 0; i < (int)parse_job_count; i++)
	{
		BlockParseJob * pj = &parse_job[i];

		pj->vtable = vtable;
		pj->scratch = scratch;
		pj->arena = arena;
		pj->matches = match_base + i*OODLELZ_BLOCK_LEN*vtable->find_all_matches_num_pairs;
		pj->dictionaryBase = dictionaryBase;
		pj->overall_comp_end = comp_end;

		// compPtr for each block is compPtrOut of previous block
		pj->pcompPtr = &parse_job[(i > 0) ? i - 1 : parse_job_count - 1].compPtrOut;
		pj->rawPtr = NULL; // set later
		pj->block_pos = 0; // set later
		pj->block_len = 0; // set later
		pj->blockIsReset = 0; // set later

		pj->compPtrOut = comp_ptr; // initially, all "end" pointers are at start of compressed buf
	}

	//-----------------------------------------------------
	// the rest is generic :

	RR_ASSERT( zeroPosPtr != NULL );

	// step OodleLZ blocks (256k)
	while(rawPtr<rawEnd)
	{
		int block_len = (int) RR_MIN( OODLELZ_BLOCK_LEN , (rawEnd - rawPtr) );

		SIMPLEPROFILE_SCOPE_N(compress_vt_sub_block,block_len);

		//-----------------------------------------------------

		// Retire most recent parse job using the current matches buffer
		if ( next_submit_job - next_retire_job == parse_job_count )
		{
			BlockParseJob * pj = &parse_job[next_retire_job % parse_job_count];
			pj->job.wait(pOptions->jobifyUserPtr);
			++next_retire_job;
		}

		// Prepare the next job
		BlockParseJob * pj = &parse_job[next_submit_job % parse_job_count];

		// If there's a match finder, run it on this thread
		if ( mf )
		{
			THREADPROFILESCOPE("mf_ProcessChunk");
			SIMPLEPROFILE_SCOPE_N(optimal_mf, block_len);
			mf->ProcessChunk(block_len, pj->matches, vtable->find_all_matches_num_pairs);
		}

		pj->rawPtr = rawPtr;
		pj->block_pos = rrPtrDiff( rawPtr - zeroPosPtr ); // this is "pos since reset" ; must match decoder
		pj->block_len = block_len;
		pj->blockIsReset = LZBlockHeader_IsReset(rrPtrDiff( rawPtr - dictionaryBase ), pOptions);
		RR_ASSERT_ALWAYS( !pj->blockIsReset || rawPtr == raw ); // reset chunk splitting is supposed to happen outside now

		if ( parse_job_count == 1 )
		{
			newlz_compress_block_parse(pj);
		}
		else
		{
			// Kick off the job
			const BlockParseJob * prev_job = (next_submit_job > 0) ? &parse_job[(next_submit_job - 1) % parse_job_count] : 0;

			pj->job.run(newlz_compress_block_parse, pj, pOptions->jobifyUserPtr,
				parse_job_count > 1, // run async if we actually have multiple workers
				prev_job ? prev_job->job.get_handle() : 0,  // dependencies
				0);
		}

		++next_submit_job;

		// Advance
		rawPtr += block_len;
	}

	// Retire pending parse jobs in order
	while ( next_submit_job != next_retire_job )
	{
		BlockParseJob * pj = &parse_job[next_retire_job % parse_job_count];
		pj->job.wait(pOptions->jobifyUserPtr);
		comp_ptr = pj->compPtrOut; // final output ptr
		++next_retire_job;
	}

/*
#ifdef OODLE_BUILDING_DATA
#ifndef OODLE_BUILDING_DLL
	OodleXMalloc_LogMemUse("newlz_compress_vtable_sub end: ");
#endif
#endif
*/

	SINTa tot_comp_len = comp_ptr - U8_void(comp);

	return tot_comp_len;
}

#if ! OODLE_PLATFORM_HAS_ADVANCED_MATCHERS

SINTa newlz_compress_vtable(newlz_vtable * vtable,
	const U8 * wholeRawBuf,U8 * compBuf,SINTa wholeRawLen,
	const U8 * dictionaryBase,
	const LRMCascade * lrm_casc,
	rrArenaAllocator * arena)
{
	// force level down to normal :
	if ( vtable->level >= OodleLZ_CompressionLevel_Optimal1 )
		vtable->level = OodleLZ_CompressionLevel_Normal;

	RR_ASSERT_ALWAYS( vtable->fp_create_match_finder == NULL );
	RR_ASSERT( lrm_casc == NULL ); // @@ true?

	// save and restore arena state around the scratch object
	rrArenaAllocatorStateSaver arena_saver(arena);
	newlz_encoder_scratch scratch;
	scratch.arena = arena;

	return newlz_compress_vtable_sub(vtable,&scratch,wholeRawBuf,compBuf,wholeRawLen,dictionaryBase,dictionaryBase,NULL,arena);
}

#else // OODLE_PLATFORM_HAS_ADVANCED_MATCHERS

struct CreateMatchFinderJob
{
	const U8 * raw_ptr;				// Pointer to raw data for this job
	SINTa raw_len;					// Length of raw data for this job

	const U8 * pointerToPos0;		// Where position 0 in the data is
	const U8 * dictionaryBase;		// Overall dictionary base
	const U8 * dictionaryStartPtr;	// Base pointer for dictionary on this match finder
	SINTa dictionary_inset;			// How many extra bytes of context to include

	const LRMCascade * lrmcasc;		// LRM cascade to use
	LRMCascade * lrmcasc_incr;		// Incremental LRM cascade (non-const)
	LRMSet lrmset_mem;				// Storage for LRMSet goes here

	OodleLZ_Jobify jobify;			// Internal jobification mode
	void * jobifyUserPtr;
	t_create_match_finder * create_fp;

	IncrementalMatchFinder * mf;	// The result goes here
	OodleJob job;					// The job itself
};

static void OODLE_CALLBACK newlz_compress_create_match_finder(void *job_data)
{

/*
#ifdef OODLE_BUILDING_DATA
#ifndef OODLE_BUILDING_DLL
	OodleXMalloc_LogMemUse("newlz_compress_create_match_finder start: ");
#endif
#endif
*/

	CreateMatchFinderJob *job = static_cast<CreateMatchFinderJob *>(job_data);
	THREADPROFILESCOPE("CreateMatchFinder");
	//SIMPLEPROFILE_SCOPE_N(create_matchfind, job->raw_len);

	LRMSet * lrmset = NULL;

	if ( job->lrmcasc && job->dictionaryStartPtr > job->dictionaryBase )
	{
		RR_ASSERT( job->lrmcasc_incr == NULL || job->lrmcasc_incr == job->lrmcasc );

		lrmset = &job->lrmset_mem;
		lrmset->lrms.clear();
		if ( job->lrmcasc_incr )
			LRM_CascadeGetSet_Align_UpdateIncremental(job->lrmcasc_incr,lrmset,job->dictionaryStartPtr,job->raw_ptr);
		else
			LRM_CascadeGetSet_Align(job->lrmcasc,lrmset,job->dictionaryStartPtr,job->raw_ptr);

		rrPrintf_v2("LRM covers to " RR_UINTa_FMT " ; dicBackup starts at " RR_UINTa_FMT "\n",
			rrPtrDiff(LRMSet_GetEndPtr(lrmset) - job->dictionaryBase),rrPtrDiff(job->dictionaryStartPtr - job->dictionaryBase));
	}

	SINTa dicPlusRaw = job->dictionary_inset + job->raw_len;
	job->mf = job->create_fp(job->dictionaryStartPtr, dicPlusRaw, job->dictionary_inset, lrmset, job->jobify, job->jobifyUserPtr);

/*
#ifdef OODLE_BUILDING_DATA
#ifndef OODLE_BUILDING_DLL
	OodleXMalloc_LogMemUse("newlz_compress_create_match_finder end: ");
#endif
#endif
*/
}
		
SINTa newlz_compress_vtable(newlz_vtable * vtable,
	const U8 * wholeRawBuf,U8 * compBuf,SINTa wholeRawLen,
	const U8 * dictionaryBase,
	const LRMCascade * lrm_casc,
	rrArenaAllocator * arena)
{
	// early out for tiny buffers already done at OodleLZ level
		
	OodleLZ_CompressionLevel level = vtable->level;
	const OodleLZ_CompressOptions * pOptions = vtable->pOptions;
	
	// save and restore arena state around the scratch object
	rrArenaAllocatorStateSaver arena_saver(arena); //,0);
	newlz_encoder_scratch scratch;
	scratch.arena = arena;
	
	if ( dictionaryBase == NULL )
	{
		dictionaryBase = wholeRawBuf;
		RR_ASSERT( lrm_casc == 0 );
	}
	
	// early-out just for debugging clarity
	// you would get to this same call if you dropped through
	if ( vtable->fp_create_match_finder == NULL )
	{
		return newlz_compress_vtable_sub(vtable,&scratch,wholeRawBuf,compBuf,wholeRawLen,dictionaryBase,dictionaryBase,NULL,arena);
	}
	
	if ( vtable->pvtable2 )
	{
		// Must be the same! (we only do it once)
		RR_ASSERT( vtable->fp_create_match_finder == vtable->pvtable2->fp_create_match_finder );
	}
	
	SINTa rawLenPlusBackup = wholeRawLen + rrPtrDiff(wholeRawBuf - dictionaryBase);
	SINTa maxLocalDictionarySize =	pOptions->maxLocalDictionarySize;
	RR_ASSERT( maxLocalDictionarySize >= 2*OODLELZ_BLOCK_LEN );
	
	// for opt1/CTMF don't do splits :
	if ( ! vtable->wants_dic_limit_splits && lrm_casc == NULL )
	{
		// CTMF_FindAll doesn't even fucking USE the LRM so this is just silly waste
		//	(not true any more; it does)
		// note that if this does lead to an LRM being made, it makes very large chunks
		// @@ the limit here is just a shitty hack to prevent the huge chunk out of memory
		//	 lame, fix me better
		maxLocalDictionarySize = RR_MAX(maxLocalDictionarySize,(1<<26));
	}
	
	// log :
	rrPrintf_v2("compress_vtable : pos : %d , size : %d\n",
		rrPtrDiff32(wholeRawBuf - dictionaryBase), (int)wholeRawLen );

	// Outer threading logic
	static const UINTa MAX_CMF_JOBS = 2; // little point in ever doing more than 2
	CreateMatchFinderJob cmf_jobs[MAX_CMF_JOBS];
	UINTa next_submit_job = 0;
	UINTa next_retire_job = 0;
	UINTa cmf_job_count = (pOptions->jobify == OodleLZ_Jobify_Aggressive) ? 2 : 1;

	RR_ASSERT_ALWAYS( cmf_job_count >= 1 && cmf_job_count <= MAX_CMF_JOBS );

	const U8 * pointerToPos0 = dictionaryBase;

	// this math is all really foogly

	// chunking to limit SuffixTrie memory use :
	//	controlled by maxLocalDictionarySize parameter

	const U8 * curRawPtr = wholeRawBuf;
	SINTa rawLenRemaining = (SINTa)wholeRawLen;

	const LRMCascade * casc = lrm_casc; // may be NULL
	LRMCascade * casc_local = NULL; // locally-created cascade, if any
		
	// don't do chunks larger than maatchMaxOffset;
	SINTa maxSubSize;
	
	if ( pOptions->seekChunkReset )
	{
		maxSubSize = pOptions->seekChunkLen;
		RR_ASSERT( casc == NULL ); // should not have LRM
		
		// NOTE : maxLocalDictionarySize is ignored
		//	makeLRM is ignored
		// seek Chunk overrides all and acts as a local dictionary
	}
	else
	{
		if ( maxLocalDictionarySize >= rawLenPlusBackup )
		{
			// if I can fit the whole thing, just do the whole thing :
		
			maxSubSize = rawLenPlusBackup;
			
			casc = NULL; // don't use LRM
		}
		else
		{
			maxSubSize = maxLocalDictionarySize/2; // half in the chunk, half in the backup
			RR_ASSERT( maxSubSize >= OODLELZ_BLOCK_LEN );
			RR_ASSERT( (maxSubSize % OODLELZ_BLOCK_LEN) == 0 );

			if ( pOptions->makeLongRangeMatcher )
			{
			
				// LRM is not used at levels below Optimal1
				//	so don't waste time making one!!
				if ( casc == NULL && level >= OodleLZ_CompressionLevel_Optimal1 )
				{
					// make an LRM for my internal sliding
				
					SINTa numChunks = (rawLenPlusBackup + maxSubSize-1)/maxSubSize;
					SINTa last_chunk_dic_start = numChunks*maxSubSize - maxLocalDictionarySize;
					
					// last_chunk_dic_start needs to bea chunk size boundary for LRM to work right
					
					// make LRM cascade :
					//	we make LRM for the region that the last chunk will not see in its dic backup :
					//casc_local = LRM_CreateCascade(dictionaryBase,last_chunk_dic_start,g_OodleLZ_LW_LRM_step,g_OodleLZ_LW_LRM_jumpbits,0,maxSubSize,g_OodleLZ_LW_LRM_hashLength);
					casc_local = LRM_CreateCascadeIncremental(dictionaryBase,last_chunk_dic_start,g_OodleLZ_LW_LRM_step,g_OodleLZ_LW_LRM_jumpbits,0,maxSubSize,g_OodleLZ_LW_LRM_hashLength);

					casc = casc_local;
				}
			}
		}
	}

	//RR_ASSERT_ALWAYS( casc == NULL || LRM_GetCascadeChunkSize(casc) == (maxLocalDictionarySize/2) );
	if ( casc )
	{
		// LRM chunk size is half 	maxLocalDictionarySize (or less)
		//	LRM chunk size evenly divides maxLocalDictionarySize
		//	so we can line up with a boundary
		RR_DURING_ASSERT( SINTa casc_chunk = LRM_GetCascadeChunkSize(casc) );
		RR_ASSERT( casc_chunk == maxSubSize );
		RR_ASSERT( casc_chunk <= (maxLocalDictionarySize/2) );
		RR_ASSERT( ((maxLocalDictionarySize/casc_chunk)*casc_chunk) == maxLocalDictionarySize );
	}

	SINTa totCompLen = 0;
	for (;;)
	{
		// Loop to create new outstanding match finder jobs
		while( rawLenRemaining > 0 && next_submit_job - next_retire_job < cmf_job_count )
		{
			SINTa dicPos = rrPtrDiff( curRawPtr - dictionaryBase );
			SINTa curLen = RR_MIN(rawLenRemaining,maxSubSize);;
			SINTa dicBackup;

			if ( pOptions->seekChunkReset )
			{
				RR_ASSERT( maxSubSize == pOptions->seekChunkLen );
				dicBackup = 0;
				pointerToPos0 = curRawPtr;
			}
			else
			{
				// first chunk can be whole maxLocalDictionarySize
				// otherwise it's maxSubSize in chunk + maxSubSize in backup
				//	maxLocalDictionarySize = maxSubSize*2
				if ( dicPos == 0 )
					curLen = RR_MIN(rawLenRemaining,maxLocalDictionarySize);

				// absorb shitlet end piece into last chunk
				//	@@ this must match the logic in OodleLZ_MakeParallelChunking !
				if ( rawLenRemaining <= (maxSubSize*5)/4 )
					curLen = rawLenRemaining;

				if ( curLen < maxLocalDictionarySize )
					dicBackup = RR_MIN3( dicPos , maxSubSize, (maxLocalDictionarySize - curLen) );
				else
					dicBackup = 0;

				if ( pOptions->dictionarySize > 0 )
				{
					// no need to backup more than dictionarySize
					dicBackup = RR_MIN(dicBackup, pOptions->dictionarySize);
				}

				RR_ASSERT( dicBackup >= 0 );
			}

			const U8 * dictionaryStartPtr = curRawPtr - dicBackup;
			CreateMatchFinderJob * subjob = &cmf_jobs[next_submit_job % cmf_job_count];

			// log the chunking :
			rrPrintf_v2("Optimal Chunk %d : [%d + %d]\n",(int)dicPos,(int)curLen,(int)dicBackup);

			RR_ASSERT( dictionaryStartPtr != NULL );

			// Store all the parameters in the job
			subjob->raw_ptr = curRawPtr;
			subjob->raw_len = curLen;
			subjob->pointerToPos0 = pointerToPos0;
			subjob->dictionaryBase = dictionaryBase;
			subjob->dictionaryStartPtr = dictionaryStartPtr;
			subjob->dictionary_inset = rrPtrDiff( curRawPtr - dictionaryStartPtr );
			subjob->lrmcasc = casc;
			subjob->lrmcasc_incr = casc_local;
			subjob->jobify = pOptions->jobify;
			subjob->jobifyUserPtr = pOptions->jobifyUserPtr;
			subjob->create_fp = vtable->fp_create_match_finder;

			RR_ASSERT_ALWAYS( wholeRawLen > 0 && subjob->dictionary_inset >= 0 );

			// Run the job
			const CreateMatchFinderJob * prev_job = (next_submit_job > 0) ? &cmf_jobs[(next_submit_job - 1) % cmf_job_count] : 0;

			subjob->job.run(newlz_compress_create_match_finder, subjob, pOptions->jobifyUserPtr,
				(cmf_job_count != 1 && next_submit_job != 0), // run async when we're not about to wait for it immediately
				prev_job ? prev_job->job.get_handle() : 0); // depend on previous match finder create job, because the LRM Fill increments need to run in order

			rawLenRemaining -= curLen;
			curRawPtr += curLen;
			++next_submit_job;
		}

		//-------------------------------------------

		// Retire next job, if any
		if ( next_retire_job == next_submit_job )
			break;

		CreateMatchFinderJob * retjob = &cmf_jobs[next_retire_job % cmf_job_count];

		// Wait for the job to finish if we did it async
		retjob->job.wait(pOptions->jobifyUserPtr);
		
		SINTa curCompLen = (SINTa) newlz_compress_vtable_sub(vtable,&scratch,retjob->raw_ptr,compBuf,retjob->raw_len,
			retjob->dictionaryStartPtr,retjob->pointerToPos0,retjob->mf,arena);

		if ( retjob->mf )
		{
			THREADPROFILESCOPE("DeleteMatchFinder");
			OodleDeleteVirtual(retjob->mf);
		}

		++next_retire_job;
		compBuf += curCompLen;
		totCompLen += curCompLen;
	}

	// if I made a local cascade, free it :
	if ( casc_local )
	{
		LRM_DestroyCascade(casc_local);
	}
	
	return totCompLen;
}

#endif // ADVANCED_MATCHERS

//===============================================

OODLE_NS_END
