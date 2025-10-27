// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "cbradutil.h"
#include "oodlelzcompressors.h"
#include <stdlib.h>

#include "newlz.h"

#include "rrlz_getmatchlen.inl"
#include "rrprefetch.h"
#include "rrlzh_lzhlw_shared.h"

#include "rrvarbits.h"
#include "templates/rrvector_a.h"

#define RRVBC_INLINE
#include "rrvarbitcodes.h"
#include "rrvarbitcodes.cpp"

#include "ctmf.h"
#include "newlz_arrays.h"
#include "newlz_subliterals.h"
#include "newlz_vtable.h"
#include "newlz_arrays.inl"
#include "newlz_multiarrays.h"

#include "histogram.h"
#include "rrlogutil.h"
#include "rrarenaallocator.h"
#include "cpux86.h"

#include "oodleconfigvalues.h"
#include "threadprofiler.h"

#include "newlz_speedfit.h"
#include "newlz_shared.h"
#include "newlz_offsets.h"
#include "newlz_decoder.h"
#include "speedfitter.h"
#include "matchfinder.h"

#ifdef _MSC_VER
#pragma warning(disable : 4702) // unreachable
#endif

// SimpleProf (this is still a NOP unless explicitly turned on in CDep via -DOODLE_SIMPLEPROF_BUILD)
/*
#include "rrsimpleprof.h"
/*/
#include "rrsimpleprofstub.h"
/**/

#ifndef OODLE_BUILDING_DLL
// dump: used to prepare input files for standalone kernel tests
//#define DO_NEWLZ_DUMP
#endif

#ifdef DO_NEWLZ_DUMP
#include <stdio.h>
#endif

OODLE_NS_START

#if 0 && !defined(OODLE_BUILDING_DLL)
// for comptime lambda optimization :
extern double g_comptime_param;

#define NEWLZ_LAMBDA_KRAKEN	g_comptime_param
#else
#define NEWLZ_LAMBDA_KRAKEN	0.01f
#endif

// Set up parse flags; the Jaguars are slightly slower with the SSE4 parse, so even though they're
// known to always have SSE4, make them use the original path.
#ifdef DO_SSE4_ALWAYS
#define DO_SSE4_PARSE_ALWAYS
#endif

#ifdef DO_SSE4_TEST
#define DO_SSE4_PARSE_TEST
#endif

#ifdef __RADJAGUAR__
#undef DO_SSE4_PARSE_ALWAYS
#undef DO_SSE4_PARSE_TEST
#endif

/****

newLZ summary 02/24/2016 :

sub literals
MML 4
minimum offset of 8 (no overlap check in decoder)
great on binary, less great on text

Action is by "packet" (control word)
packet contains an LRL and ML
strict alternating LRL-ML-LRL-ML sequence
LRL can be 0, ML is >= MML

packet contains rep-match index
so short reps are entirely specified in packet

packet models the LRL0-LO0 exclusion

LO MML is 2 ; LO0 len1 can be sent with sub literal 0
(though that doesn't capture the exclusion or any state patterns)

Literals & packets are sent as separate arrays with Huffman
(sent with raw bytes if huffman entropy is near 8 bpb)

Offsets are sent with TANS
 bottom bits (5)
 log2 + raw bits
 offset is unlimited (OodleLZ 30 bit limit)

ML's and LRL's that don't fit in packet are sent as excesses
 they're unlimited in the parser
 unlimited LRL is required by the format (because there's no ML 0)
 but ML could be limited and long matches sent as multiple packets

excesses are sent with varbits exp-golomb
  (fixed # of bits + log2 + remainder)

Offsets & excesses are sent out of band in separate arrays

Decoder flow :

get huff array of literals
get huff array of packets
get TANS offsets
get varbits excesses

then you have all your variables, you do the decoder parse of the packets
to blast out the output :

for each packet
  get lrl from packet + excesses
    add literals only lastoffset match (undo sub literals)
    
  get offset type (rep or not)
  do rep-offset MTF update
  
  get ml from packet + excesses
  copy the match


There is no bit input in the parse
It's all just interleaving data from the various pre-decoded buffers


currently packet is :
2 bits LRL
2 bits offset (3 LO's + normal)
4 bits ML


nice things about this packet :
LO matches are entirely in packet
Good number of ML linear values, which is good for modeling & most matches stay in packet
enough LRL values to model all patterns wanted (really only need 0,1,2+ for that)

bad thing about this packet :
LRL >= 3 is a spill to excess, which is a frequent branch (20-25%)
only 3 LO's fit (4 would be nice)

=========================================================================

newlz - encoder could do special parsing for memsets
	no change to decoder
	just try to encode them as well as possible
	when you find a long memset, first send 8 bytes as a normal match (if possible)
	then send the remainder as an offset=8 match

newlz - chunk size?
	currently locked at 128k
	maybe expose options of 64k and 256k ?
	could flag them using the flags in the Quantum header

newlz - can try encoding with MML 3

newlz - could even send len 2 normal matches when they lead to good literals or later LO
	(LZ-Sub style)

newlz - LO0-LRL0 space at the bottom of packet is wasted

newLZ - tiny huff arrays could be decoded with a different technique
	(not LSB, not dual, not fastdecode; just use the old huff with non-fast-decode-table)

newLZ - micro-threaded decoder would in fact work
	entropy decode for next chunk can run at the same time as parse of current chunk
	you need 2 threads running in lockstep together and never going to sleep

newlz - wasting lots of single bytes here and there
	lots of varbits starts & ends
	-> meh

newlz - tweak NEWLZ_LOML_GOOD_ENOUGH

-------------------

newLZ - fast sub&3 decoder ?
	
****/
		
//==============================================================================


		
/*

always use LOMML (2) for packet formation
the normal offset packets with ML 2 and 3 just don't get made
so they get entropy-coded out
but you lose a tiny bit of match len range

The decoder totally independent of NEWLZ_MML (decoder knows NEWLZ_LOMML)

NEWLZ_MML is 4 for greedy parses, sometimes 3 for optimal

*/

#define NEWLZ_MML_NORMAL	4	// <- this is just an encoder parameter, doesn't change code stream


			
/**

packet :

{3 LO's + 1 general offset}=4
*4 LRL's
*16 ML's
= 256

a bunch of packet values are not used
LO0LRL0
normal matches of len 2 & 3

**/

// can not define NEWLZ_LRL_FOR_LONGER_MML to turn it off :
// @@@@ could use more thorough tweak ; 64 looks okay
//	decode_parse_inner checks for LRL >= 24 then unrolls big steps
#define NEWLZ_LRL_FOR_LONGER_MML	(24+32)


//=============================================================================

//#define NEWLZ_OFF4M_MML		8
//#define NEWLZ_OFF4M_MML		10

static RADINLINE bool newLZ_IsAllowedNormalMatch_Optimal(int ml,int off,const OodleLZ_CompressOptions *pOptions)
{
	// -> no rules in optimal!
	//	just use the guided statistics from heuristic ?
	//  this is totally fine for compression ratio, but can cause decode speed dips
	//return true;
		
	// DO still enforce some rules :
	// -> these are for decode speed , mainly on files like "webster"
	//		and mainly on slow-memory platforms like Jaguar & ARM
	// -> at typical cache sizes, require larger ML to go out of cache

	if ( pOptions->farMatchOffsetLog2 > 0 && off >= (1<<pOptions->farMatchOffsetLog2) )
	{
		if ( ml < pOptions->farMatchMinLen )
		{
			return false;
		}
	}
	
	if ( off >= (1<<20) )// 1M
	{
		/*
		idea : like Mermaid ?
			another MML level for ESCAPE_OFFSET_MIN ?
			it's around 8M so it's not cache related
			it's just to avoid the 0xF0 offset escapes

		try to improve decode speed on "webster" and such

		maybe MML 10 ?
	
		-> doesn't seem to work
		hurts compression a tiny bit and doesn't help decode speed
		
		if ( off >= ESCAPE_OFFSET_MIN ) // 8M
		{
			return ml >= 10;
		}
		else 
		*/
		
		if ( off >= (1<<22) ) // 4M
		{
			return ml >= 8;
		}
		else if ( off >= (1<<21) ) // 2M
		{
			return ml >= 6; // @@@@ not sure about this one
		}
		else
		{		
			return ml >= 5;
		}
	}
	
	return true;
}

static RADINLINE bool newLZ_IsAllowedNormalMatch(int ml,int off,const OodleLZ_CompressOptions *pOptions)
{
	// no low offsets at all :
	RR_ASSERT( off >= NEWLZ_MIN_OFFSET );
	RR_ASSERT( off < NEWLZ_MAX_OFFSET );
	RR_ASSERT( ml >= 3 );
	
	// strict rules :
	if ( ! newLZ_IsAllowedNormalMatch_Optimal(ml,off,pOptions) )
		return false;
	
	// guiding rules :
	//  (these are strict in Normal parse
	//	 in optimal parse they are used for the pre-parse stats
	//	 but then not enforced in the final parse)

	// the idea here is :
	// for low match lens, require that offset is also low
	// don't take short matches with high offsets, we'd rather do literals

	// the exact tradeoff depends on the file
	// on highly compressible binary, these offsets are best off very low
	//	  (helps compression AND decode speed)
	// on text, they want to be higher (helps compression)
	
	// game testset can stand a pretty low ML4 offset max
	// hurts text-like (to lower the ML4 offset)
	// it *does* help speed (to lower this)

	// -> I could do better on text by raising these

	// higher offset threshold for MML3 = slower decode
	//	because more ML3 matches can be taken
	// low offset threshold is kind of bad for offset entropy coding
	//	because it makes the statistics of ML3 and 4+ offsets quite different
	//	and they are merged together

	// ML3 max should definitely be 16k - 32k :
	// the -> array is after the "No IsAllowed in Optimal" change
	//   NEWLZ_ML3_MAX_OFFSET=1<<14: 27,312,187 -> 27,279,804
	//   NEWLZ_ML3_MAX_OFFSET=1<<15: 27,304,525  -> 27,274,892
	#define NEWLZ_ML3_MAX_OFFSET	(1<<14) // 14 or 15
	#define NEWLZ_ML4_MAX_OFFSET	(1<<17) // 17 or 18
	#define NEWLZ_ML5_MAX_OFFSET	(1<<20) // 20 or 21


/*****************************************************************************************/

	if ( ml > 5 )
		return true;

	static const int max_offset[6] = { 0,0,0, NEWLZ_ML3_MAX_OFFSET, NEWLZ_ML4_MAX_OFFSET, NEWLZ_ML5_MAX_OFFSET };
	return off < max_offset[ml];
}

static RADINLINE bool newLZ_IsNormalMatchBetter(int ml,int off, int bestml, int bestoff)
{
	if ( ml < bestml ) return false;
	if ( ml == bestml ) return off < bestoff;
	if ( ml > bestml+1 ) return true;
	
	RR_ASSERT( ml == bestml+1 );
	
	return ( (off>>7) <= bestoff );
}

static RADINLINE bool newLZ_IsLOMatchBetter(int repLen,int mainLen,int mainDist)
{
	if (repLen >= 2 && (
		(repLen + 1 >= mainLen) ||
		(repLen + 2 >= mainLen && mainDist >= (1 << 10)) ||
		(repLen + 3 >= mainLen && mainDist >= (1 << 16))))
	{
		return true;
	}
	else
	{
		return false;
	}
}

// 3,348,789 
// if newLZ_LazyMatchDelta > 0 , do lazy1 , if > NEWLZ_LAZYBETTER_LAZY2 , do lazy2
#define NEWLZ_LAZYBETTER_LAZY2 3
static RADINLINE int newLZ_LazyMatchDelta(int newMatchLen,U32 newOffset,bool newIsLast,
                int oldMatchLen,U32 oldOffset,bool oldIsLast)
{
	// this has to also ensure that the new match is allowed
	//if ( newMatchLen < 2+(newIsLast?0:1) ) return 0;
	if ( ! newMatchLen ) return 0;

	// warning : these parameters can be very easily over-trained to specific files
	//	they are parse-guiding , very approximate code cost

	// estimate of bits to send offset :
	U32 newOffsetLog2 = newIsLast ? 0 : 2+rrGetBitLevel_V(newOffset);
	U32 oldOffsetLog2 = oldIsLast ? 0 : 2+rrGetBitLevel_V(oldOffset);
	
	// was *4
	// 3,339,983
	// *6 = 10,370,844
	// lzt99 : 10,324,637
	// *3 = 10,299,507
	// 3,353,900
	int newGain,oldGain;
	
	if ( 0 ) // newIsLast ) // lzt99 likes *3 better
	{
		newGain = newMatchLen*3 - newOffsetLog2;
		oldGain = (oldMatchLen+1)*3 - oldOffsetLog2;
	}
	else // dickens likes *4 ; it's sort of like bits per symbol
	{
		newGain = newMatchLen*4 - newOffsetLog2;
		oldGain = (oldMatchLen+1)*4 - oldOffsetLog2;
	}
	
//3,344,832	
//	if ( newIsLast ) newGain += 3;
//	if ( oldIsLast ) oldGain += 3;
	
	return newGain - oldGain;
}

//=======================================================================================
/**

newLZ_LOs contain one extra LO for the decoder
it puts the normal match offset in there
so it can be chosen branchlessly

decoder also carries *negative* offsets through the LO set

**/

RR_COMPILER_ASSERT( NEWLZ_NUM_LAST_OFFSETS == 3 );

struct newLZ_LOs_NoPad
{
	S32 lasts[3];
	
	RADFORCEINLINE void Reset()
	{
		for(int i=0;i<NEWLZ_NUM_LAST_OFFSETS;i++)
			lasts[i] = NEWLZ_MIN_OFFSET;
	}
	
	RADFORCEINLINE void SetMTF(const newLZ_LOs_NoPad & from,SINTr index)
	{
		RR_ASSERT( index < NEWLZ_NUM_LAST_OFFSETS );
		
		// if index == 0 , do nothing
		if ( index == 0 )
		{
			lasts[0] = from.lasts[0];
			lasts[1] = from.lasts[1];
			lasts[2] = from.lasts[2];
		}
		else if ( index == 1 )
		{
			lasts[0] = from.lasts[1];
			lasts[1] = from.lasts[0];
			lasts[2] = from.lasts[2];
		}
		else // if ( index == 2 )
		{
			lasts[0] = from.lasts[2];
			lasts[1] = from.lasts[0];
			lasts[2] = from.lasts[1];
		}
	}
	
	RADFORCEINLINE void SetAdd(const newLZ_LOs_NoPad & from,S32 offset)
	{
		RR_ASSERT( from.Find(offset) == -1 );
				
		lasts[0] = offset;
		lasts[1] = from.lasts[0];
		lasts[2] = from.lasts[1];
	}
	
	RADFORCEINLINE void MTF(SINTr index)
	{
		RR_ASSERT( index < NEWLZ_NUM_LAST_OFFSETS );
		
		// if index == 0 , do nothing
		if ( index == 1 )
		{
			swap(lasts[0],lasts[1]);
		}
		else if ( index == 2 )
		{
			S32 top = lasts[2];
			lasts[2] = lasts[1];
			lasts[1] = lasts[0];
			lasts[0] = top;
		}
	}
	
	RADFORCEINLINE void Add(S32 offset)
	{
		RR_ASSERT( Find(offset) == -1 );
		
		// alternate :
		//lasts[3] = offset;
		//MTF(3);
		
		S32 m0 = lasts[0];
		S32 m1 = lasts[1];
		lasts[0] = offset;
		lasts[1] = m0;
		lasts[2] = m1;
	}
	
	RADFORCEINLINE int Find(S32 offset) const
	{
		RR_UNROLL_I_3(0, if ( offset == (S32) lasts[i] ) { return i; } );
		return -1;
	}
	
	RADFORCEINLINE bool IsLastOff(S32 offset) const
	{
		return Find(offset) >= 0;
	}

	RADFORCEINLINE int Find_Update(S32 offset)
	{
		RR_UNROLL_I_N(NEWLZ_NUM_LAST_OFFSETS,0, if ( offset == (S32) lasts[i] ) { MTF(i); return i; } );
		Add(offset);
		return -1;
	}

	S32 LastOffset() const { return lasts[0]; }
};		

struct newLZ_PaddedLO_Storage
{
	// padding in front for slide-down and at end to simplify insert
	S32 contents[4 + 4];

	RADFORCEINLINE const S32& operator[](SINTa i) const { return contents[i + 4]; }
	RADFORCEINLINE S32& operator[](SINTa i) { return contents[i + 4]; }
};

// NOTE(fg): LO array crossing cache lines is a regular source of perf
// regressions; align it generously. (4 entries + 3 padding = 7 32-bit words,
// so align to 32B, which means that on pow2 cache line sizes >=32B, we're good)
struct alignas(32) newLZ_LOs
{
	newLZ_PaddedLO_Storage lasts;
		
	RADFORCEINLINE void Reset()
	{
		for(int i=0;i<NEWLZ_NUM_LAST_OFFSETS;i++)
			lasts[i] = NEWLZ_MIN_OFFSET;
	}
	
	RADFORCEINLINE void Reset_Neg()
	{
		for(int i=0;i<NEWLZ_NUM_LAST_OFFSETS;i++)
			lasts[i] = - NEWLZ_MIN_OFFSET;
	}
	
	RADFORCEINLINE bool Equals(const newLZ_LOs & rhs) const
	{
		for(int i=0;i<NEWLZ_NUM_LAST_OFFSETS;i++)
		{
			if ( lasts[i] != rhs.lasts[i] )
				return false;
		}
		
		return true;	
	}
	
	RADFORCEINLINE S32 MTF3(SINTr index)
	{
		RR_ASSERT( index < NEWLZ_NUM_LAST_OFFSETS );

		S32 top = lasts[index];
		S32 m1 = lasts[index-1];
		S32 m2 = lasts[index-2];
		lasts[index  ] = m1;
		lasts[index-1] = m2;
		lasts[0] = top;
		return top;
	}
	
	RADFORCEINLINE S32 MTF4(SINTr index)
	{
		// index can be 1 above last set for normal offset
		RR_ASSERT( index <= NEWLZ_NUM_LAST_OFFSETS );
		
		// do the MTF :
		S32 top = lasts[index];
		S32 m1 = lasts[index-1];
		S32 m2 = lasts[index-2];
		S32 m3 = lasts[index-3];
		lasts[index  ] = m1;
		lasts[index-1] = m2;
		lasts[index-2] = m3;
		
		lasts[0] = top;
		return top;
	}
	
	RADFORCEINLINE S32 MTFA(SINTr index,S32 above)
	{		
		// put the normal offset above lasts
		lasts[NEWLZ_NUM_LAST_OFFSETS] = above;
		
		return MTF4(index);
	}
	
	RADFORCEINLINE void Add(S32 offset)
	{
		RR_ASSERT( Find(offset) == -1 );
		
		// alternate :
		//lasts[3] = offset;
		//MTF(3);
		
		S32 m0 = lasts[0];
		S32 m1 = lasts[1];
		lasts[0] = offset;
		lasts[1] = m0;
		lasts[2] = m1;
	}

	RADFORCEINLINE void AddRaw(S32 offset)
	{
		// for use in decoder, doesn't do extra checking
		lz_copy8(&lasts[1], &lasts[0]);
		lasts[0] = offset;
	}
	
	RADFORCEINLINE int Find(S32 offset) const
	{
		RR_UNROLL_I_3(0, if ( offset == (S32) lasts[i] ) { return i; } );
		return -1;
	}
	
	RADFORCEINLINE bool IsLastOff(S32 offset) const
	{
		return Find(offset) >= 0;
	}

	RADFORCEINLINE int Find_Update(S32 offset)
	{
		RR_UNROLL_I_3(0, if ( offset == (S32) lasts[i] ) { MTF3(i); return i; } );
		Add(offset);
		return -1;
	}

	const newLZ_LOs_NoPad & NoPad() const { return *((newLZ_LOs_NoPad *)&lasts[0]); }

	S32 LastOffset() const { return lasts[0]; }
};

#define newLZ_dec_LOs							newLZ_LOs
#define newLZ_dec_LOs_Reset_Neg(lasts)			lasts.Reset_Neg()
#define newLZ_dec_LOs_MTF(lasts,index)			lasts.MTF4(index)
#define newLZ_dec_LOs_Add(lastoffsets,above)	lastoffsets.lasts[3] = above

//=============================================================================

// newlz_encoder_parse = 4*dword
//	@@ could skip storing "lastoffset" here
struct newlz_encoder_parse
{
	S32 lastoffset;
	S32 lrl;
	S32 ml;
	S32 offset; // negative for LO
	bool IsLO() const { return offset <= 0; }
};

#define NEWLZ_PACKET_NORMAL_MATCH_MIN ((NEWLZ_PACKET_OFFSETS_COUNT-1) * NEWLZ_PACKET_LRL_COUNT * NEWLZ_PACKET_ML_COUNT)
		
#ifdef NEWLZ_O1_CONTEXT_COUNT
#define NEWLZ_O1_ARRAY_COUNT	(NEWLZ_O1_CONTEXT_COUNT+1)
#define NEWLZ_LITERAL_ARRAY_COUNT_MAX	RR_MAX(4,NEWLZ_O1_ARRAY_COUNT)
#else
#define NEWLZ_LITERAL_ARRAY_COUNT_MAX	4
#endif

struct newLZ_encoder_arrays
{
	U8 * literals_space_raw;
	U8 * literals_ptr_raw;

	U8 * literals_space_sub;
	U8 * literals_ptr_sub;

	U8 * packets_space;
	U8 * packets_ptr;

	U8 * offsets_u8_space;
	U8 * offsets_u8_ptr;

	U32 * offsets_space;
	U32 * offsets_ptr;

	U8 * excess_u8_space;
	U8 * excess_u8_ptr;

	U32 * excess_u32_space;
	U32 * excess_u32_ptr;

	U8 * excess_ptr_end;

	int chunk_len;
	const U8 * chunk_ptr;

	S32 lastoffset;
	U32 bitstream_flags;
};

void newLZ_encoder_arrays_point_to_scratch(
	newLZ_encoder_arrays * encarrays,
	newlz_encoder_scratch * scratch,
	int chunk_len,
	const U8 * chunk_ptr,
	U32 bitstream_flags
	)
{
	rrArenaAllocator * arena = scratch->arena;

	/**

	literals    : chunk_len
	packets     : chunk_len/2
	    the max # of packets occurs with all lrl0 + len2 LO matches
		(always alternating between 2 of the last offsets)
	offsets U8  : chunk_len/3  (every packet lrl0 + new offset len3 match)
	offsets U32 : (chunk_len/3)*4
	excess U8   : chunk_len/5
		lrl3 (=excess 0) + len2 LO every packet
		gives 1 excess U8 every 5 bytes
	excess U32  : (chunk_len/256)*4
		U32 excess means either LRL or ML (or both!) are coding >=256 bytes (a bit more actually, but it hardly matters)
	**/

	encarrays->chunk_ptr = chunk_ptr;
	encarrays->chunk_len = chunk_len;

	int scratch_space_literals = chunk_len + 8; // +8 for sloppy copy
	int scratch_space_packets = chunk_len/2 + 8;
	int scratch_space_offsets8 = chunk_len/3;
	int scratch_space_offsets32 = (chunk_len/3)*4;

	int scratch_space_excess8 = chunk_len/5;
	int scratch_space_excess32 = (chunk_len/256)*4;
	int scratch_slack = 256;
	int scratch_space_needed = 2*scratch_space_literals + scratch_space_packets +
		scratch_space_offsets8 + scratch_space_offsets32 +
		scratch_space_excess8 + scratch_space_excess32 +
		scratch_slack;

	scratch->newlz_arrays_space.extend(scratch_space_needed,arena);
	U8 * scratch_space = scratch->newlz_arrays_space.getU8();

	U8 * scratch_ptr = scratch_space;

	encarrays->literals_space_raw = scratch_ptr;
	encarrays->literals_ptr_raw = scratch_ptr;
	scratch_ptr += scratch_space_literals;

	encarrays->literals_space_sub = scratch_ptr;
	encarrays->literals_ptr_sub = scratch_ptr;
	scratch_ptr += scratch_space_literals;

	encarrays->packets_space = scratch_ptr;
	encarrays->packets_ptr = scratch_ptr;
	scratch_ptr += scratch_space_packets;

	encarrays->offsets_u8_space = scratch_ptr;
	encarrays->offsets_u8_ptr = scratch_ptr;
	scratch_ptr += scratch_space_offsets8;

	scratch_ptr = rrAlignUpPointer(scratch_ptr,4);
	
	encarrays->offsets_space = (U32 *) scratch_ptr;
	encarrays->offsets_ptr = (U32 *) scratch_ptr;
	scratch_ptr += scratch_space_offsets32;

	encarrays->excess_u8_space = scratch_ptr;
	encarrays->excess_u8_ptr = scratch_ptr;
	scratch_ptr += scratch_space_excess8;

	scratch_ptr = rrAlignUpPointer(scratch_ptr,4);

	encarrays->excess_u32_space = (U32 *) scratch_ptr;
	encarrays->excess_u32_ptr = (U32 *) scratch_ptr;
	scratch_ptr += scratch_space_excess32;

	encarrays->excess_ptr_end = scratch_ptr;

	RR_ASSERT( scratch_ptr-scratch_space <= scratch_space_needed );
	RR_ASSERT( scratch_ptr-scratch_space >= scratch_space_needed-scratch_slack );

	encarrays->lastoffset = NEWLZ_MIN_OFFSET;
	encarrays->bitstream_flags = bitstream_flags;
}

static void newLZ_encoder_arrays_put_excess(
	newLZ_encoder_arrays * encarrays,
	int val)
{
	if ( val < 255 )
	{
		*encarrays->excess_u8_ptr++ = U8_check(val);
	}
	else
	{
		*encarrays->excess_u8_ptr++ = 255;
		*encarrays->excess_u32_ptr++ = val - 255;
	}
}

template<int t_enable_sub_lits>
static RADNOINLINE void newLZ_encoder_arrays_put_long_lrl(
	newLZ_encoder_arrays * encarrays,
	const U8 * literals_start,
	int lrl)
{
	if ( t_enable_sub_lits )
	{
		put_sub_literals_sloppy(encarrays->literals_ptr_sub,literals_start,lrl,encarrays->lastoffset);
		encarrays->literals_ptr_sub += lrl;
	}

	U8 *literals_ptr_raw = encarrays->literals_ptr_raw;
	// NOTE: modifies both literals_ptr_raw and literals_start
	lz_copywordsteptoend_overrunok(literals_ptr_raw,literals_start,lrl);
	encarrays->literals_ptr_raw = literals_ptr_raw;

	newLZ_encoder_arrays_put_excess(encarrays,lrl - NEWLZ_PACKET_LRL_MAX);
}

static RADNOINLINE void newLZ_encoder_arrays_put_long_offset(
	newLZ_encoder_arrays * encarrays,
	S32 off)
{	
	*encarrays->offsets_u8_ptr++ = newLZ_offset44_pack_high_u8(off);
	*encarrays->offsets_ptr++ = off;
}

template<int t_lo0_only,int t_try_sub_lits>
static RADFORCEINLINE void newLZ_encoder_arrays_put_packet(
	newLZ_encoder_arrays * encarrays,
	newLZ_LOs * plastoffsets,
	int lrl, int ml, int offset,
	const U8 * literals_start)
{
	int packet;

	// special-casing lrl==0 seems to be advantageous
	if ( lrl == 0 )
		packet = 0;
	else if ( lrl <= 8 ) // fast path for up to 8 literals
	{
		int lrl_minus_escape = lrl - NEWLZ_PACKET_LRL_MAX;

		// inline excess path
		U8 * excess_ptr = encarrays->excess_u8_ptr;
		U8 * excess_ptr_inc = excess_ptr + 1;
		*excess_ptr = (U8)lrl_minus_escape;
		encarrays->excess_u8_ptr = (lrl_minus_escape >= 0) ? excess_ptr_inc : excess_ptr;
		packet = (lrl_minus_escape >= 0) ? NEWLZ_PACKET_LRL_MAX : lrl;

		lz_copy8(encarrays->literals_ptr_raw,literals_start);
		encarrays->literals_ptr_raw += lrl;

		if ( t_try_sub_lits )
		{
			put_sub_literals8(encarrays->literals_ptr_sub,literals_start,lrl,encarrays->lastoffset);
			encarrays->literals_ptr_sub += lrl;
		}
	}
	else
	{
		packet = NEWLZ_PACKET_LRL_MAX;
		newLZ_encoder_arrays_put_long_lrl<t_try_sub_lits>(encarrays,literals_start,lrl);
	}

	int ml_to_send = ml - NEWLZ_LOMML;
	int ml_for_packet = ml_to_send;

	if ( ml_to_send >= NEWLZ_PACKET_ML_MAX )
	{
		ml_for_packet = NEWLZ_PACKET_ML_MAX;
		newLZ_encoder_arrays_put_excess(encarrays,ml_to_send - NEWLZ_PACKET_ML_MAX);
	}

	packet += ml_for_packet<<2;
	RR_ASSERT(offset <= 0 || ml_for_packet > 0); // not an escape packet

	if ( offset <= 0 )
	{
		if ( t_lo0_only )
		{
			RR_ASSERT( offset == 0 );
		}
		else
		{
			// LO
			int loi = -offset;
			RR_ASSERT( loi >= 0 && loi < NEWLZ_NUM_LAST_OFFSETS );
			packet += loi << 6;

			encarrays->lastoffset = plastoffsets->MTF3(loi);
		}
	}
	else
	{
		S32 off = offset;
		RR_ASSERT( off >= NEWLZ_MIN_OFFSET && off < NEWLZ_MAX_OFFSET );
		RR_ASSERT( ml >= 3 );

		packet += 3 << 6;
		if ( !t_lo0_only )
			plastoffsets->Add(off);
		encarrays->lastoffset = off;

		//encarrays->offsets_blend += newLZ_offset_blend_score(off);

		if ( off < ESCAPE_OFFSET_MIN )
		{
			*encarrays->offsets_u8_ptr++ = newLZ_offset44_pack_low_u8(off);
			*encarrays->offsets_ptr++ = off;
		}
		else
		{
			// newLZ_encoder_arrays_put_long_offset is NOINLINE
			newLZ_encoder_arrays_put_long_offset(encarrays,off);
		}
	}

	*encarrays->packets_ptr++ = U8_check(packet);
}

template<int t_enable_sub_lits>
static RADNOINLINE void newLZ_encoder_arrays_finish_chunk(
	newLZ_encoder_arrays * encarrays,
	const U8 * literals_start)
{
	SINTa final_lrl = encarrays->chunk_ptr + encarrays->chunk_len - literals_start;
	RR_ASSERT( final_lrl >= 0 && final_lrl <= encarrays->chunk_len );
	if ( final_lrl > 0 )
	{
		// put the literals :
		// careful tail
		memcpy(encarrays->literals_ptr_raw,literals_start,final_lrl);
		encarrays->literals_ptr_raw += final_lrl;

		if ( t_enable_sub_lits )
		{
			put_sub_literals(encarrays->literals_ptr_sub,literals_start,final_lrl,encarrays->lastoffset);
			encarrays->literals_ptr_sub += final_lrl;
		}
	}
}

//=======================================================================================

// my CTMF depths are shockingly low
//	that's okay on binary, hurts a lot on text

//#define SECOND_HASH_LEN	1	// 28,752,580
//#define SECOND_HASH_LEN	6	// 28,752,235
//#define SECOND_HASH_LEN	7	// 28,724,647
#define SECOND_HASH_LEN	8	// 28,643,159

//typedef CTMF<4,1,NEWLZ_MML_NORMAL,0>	newLZ_CTMF_Normal;
typedef CTMF<U32,2,SECOND_HASH_LEN,NEWLZ_MML_NORMAL>	newLZ_CTMF_Normal;

// newLZ_CTMF_SuperFast is no longer used, except for test_hash_collisions experiment
//typedef CTMF<U32,0,0,NEWLZ_MML_NORMAL>	newLZ_CTMF_SuperFast;
//typedef CTMF<U32,0,0,6>	newLZ_CTMF_SuperFast;

typedef FastCTMF<U32> newLZ_CTMF_HyperFast32;
typedef FastCTMF<U16> newLZ_CTMF_HyperFast16;

//=======================================================================================

template<typename newLZ_CTMF>
static RADFORCEINLINE bool newLZ_find_lo0_match_ahead(match *pmatch,
	newLZ_CTMF * ctmf, const newLZ_LOs & lastoffsets, const U8 * ptr, const U8 * ptr_matchend )
{
	S32 lastoffset = lastoffsets.lasts[0];

	if ( RR_GET32_NATIVE(ptr+1) != RR_GET32_NATIVE(ptr+1-lastoffset) )
		return false;

	S32 lo_ml = getmatchlen_after4(ptr+1,ptr+1-lastoffset,ptr_matchend);

	pmatch->ml = lo_ml;
	pmatch->off = 0;

	// update hash :
	RR_ASSERT( ptr == ctmf->m_next_ptr );
	U32 * row = ctmf->m_next_row;
	U32 hash = ctmf->m_next_hash;
	U32 * row2 = ctmf->m_next_row_second;
	//U32 hash2 = ctmf->next_hash_second;

	// where to prefetch? sets m_next which will be used in the partial insert
	//ctmf->prefetch_next(ptr+1+lo_ml); // end - no
	ctmf->prefetch_next(ptr+2); // inside?
	//ctmf->prefetch_next(ptr+1); // about the same as ptr+2

	U32 cur_absolute_pos = (U32)rrPtrDiff(ptr - ctmf->m_base_ptr);

	// now update the hash rows :
	ctmf->insert(row,cur_absolute_pos,hash);
	if ( newLZ_CTMF::c_do_second_hash )
		ctmf->insert(row2,cur_absolute_pos,hash);

	return true;
}

// NOTE: inline version intended to be used in the hot match finding loop for fast encoders, has "lo0_allowed" flag.
template<typename newLZ_CTMF>
static 
RADFORCEINLINE 
bool newLZ_get_match_heuristic_inline(match *pmatch,
	newLZ_CTMF * ctmf, U32 step, const newLZ_LOs & lastoffsets, const U8 * ptr, const U8 * ptr_matchend, int mml, const U8 * literals_start, U32 dictionarySize,
	bool lo0_allowed,const OodleLZ_CompressOptions *pOptions)
{
	U32 ptr32 = RR_GET32_NATIVE_UNALIGNED(ptr);

	RR_ASSERT( ptr == ctmf->m_next_ptr );
	U32 * row = ctmf->m_next_row;
	U32 hash = ctmf->m_next_hash;
	U32 * row2 = ctmf->m_next_row_second;
	//U32 hash2 = ctmf->next_hash_second;
	
	U32 cur_absolute_pos = (U32)rrPtrDiff(ptr - ctmf->m_base_ptr);
	
	ctmf->prefetch_next(ptr+step);
	
	// check LOs
	// only take higher LO index if the ML is greater
	int lo_index = -1;
	S32 lo_ml=0;
		
	// pretty aggressively just take LO if it's non-trivial
	//	don't even look for non-LO match
	#define NEWLZ_LOML_GOOD_ENOUGH	4
	
	RR_UNROLL_I_N(NEWLZ_NUM_LAST_OFFSETS,0, \
	{ \
		if ( i != 0 || lo0_allowed ) { \
			U32 offset = lastoffsets.lasts[i]; \
			int ml; \
			if ( havematch_mml2_one32(ptr32,ptr,ptr-offset,ptr_matchend,&ml) ) { \
				if ( i == 0 || ml > lo_ml ) { \
					lo_ml = ml; \
					lo_index = i; \
				} \
			} \
		} \
	} );
	
	if ( lo_ml >= NEWLZ_LOML_GOOD_ENOUGH )
	{
		// just take the LO match with no normal search :
		pmatch->ml = lo_ml;
		pmatch->off = -lo_index;
		
		// now update the hash rows :
		ctmf->insert(row,cur_absolute_pos,hash);
		if ( newLZ_CTMF::c_do_second_hash )
			ctmf->insert(row2,cur_absolute_pos,hash);
	
		return true;
	}

	//---------------------------------------

	#ifdef NEWLZ_LRL_FOR_LONGER_MML
	if ( ptr - literals_start >= NEWLZ_LRL_FOR_LONGER_MML )
	{
		// try heuristic - if in long LRL bump up LO MML to 3 -> meh
		if ( lo_ml <= 2 ) lo_ml = 0;
		// try heuristic - if in long LRL bump up MML to 5 -> no good?
		if ( mml < 5 ) mml = 5;
	}
	#endif
			
	S32 bestml = 0;
	U32 bestoff = 0;

	// Find match candidates that pass tag bits check (first pass)
	RAD_ALIGN(U32, candidates[newLZ_CTMF::c_table_depth * 2], 16);
	U32 candidate_mask = ctmf->find_candidates(candidates, row, row2, cur_absolute_pos, hash);

	// Verify match candidates (second pass)
	SINTa prevml = mml - 1; // this forces any matches we find to be >=mml
	while (candidate_mask)
	{
		SINTa i = rrCtz32(candidate_mask);
		candidate_mask &= candidate_mask - 1;

		// could do -NEWLZ_MIN_OFFSET here, and then +NEWLZ_MIN_OFFSET after
		// checking NEWLZ_MIN_OFFSET anyway
		U32 offset = candidates[i];
		if ( offset >= dictionarySize )
			continue;

		// force all offsets >= 8
		if ( offset < NEWLZ_MIN_OFFSET ) offset = NEWLZ_MIN_OFFSET;

		const U8 * vs_ptr = ptr - offset;

		SINTa len;
		if ( ! havematch_better(ptr,vs_ptr,ptr_matchend,prevml,&len) )
			continue;

		RR_ASSERT( len > prevml && len >= NEWLZ_MML_NORMAL );

		// NOTE(fg): behavioral change here 2022-11-21.
		// We used to set prevml=0 in between the first and second hash lookup results,
		// not forcing the second hash matches to be longer than the best result with the first hash.
		// We could do this here by setting prevml=0 when new candidate_mask has the lower half all clear
		// (that is, before proceeding to the second hash lookup results), but there doesn't seem to be much
		// reason to in practice?
		prevml = len;

		const S32 len32 = (S32)len;
		if ( newLZ_IsAllowedNormalMatch(len32,offset,pOptions) &&
			 ( ( newLZ_CTMF::c_table_depth == 1 && !newLZ_CTMF::c_do_second_hash ) || newLZ_IsNormalMatchBetter(len32,offset,bestml,bestoff) ) )
		{
			bestml = len32;
			bestoff = offset;
		}
	}
	
	// now update the hash rows :
	ctmf->insert(row,cur_absolute_pos,hash);
	if ( newLZ_CTMF::c_do_second_hash )
		ctmf->insert(row2,cur_absolute_pos,hash);

	// If !lo0_allowed, we may end up finding LO0 as a regular match
	if ( !lo0_allowed && bestoff == (U32)lastoffsets.lasts[0] )
	{
		// NOTE: this assumes that any lo0 match found through the regular match
		// find is long enough to just use automatically.
		RR_COMPILER_ASSERT( NEWLZ_MML_NORMAL >= NEWLZ_LOML_GOOD_ENOUGH );

		pmatch->ml = bestml;
		pmatch->off = -0;
		return true;
	}

	if ( bestoff == 0 )
	{
		// no normal match
		if ( lo_ml >= NEWLZ_LOMML )
		{
			pmatch->ml = lo_ml;
			pmatch->off = -lo_index;
			return true;
		}
		else
		{
			return false;
		}
	}
	else
	{
		// normal match + maybe rep as well
		//	rep is only len <= 3 here, len >= 4 earlied out

		if ( newLZ_IsLOMatchBetter(lo_ml,bestml,bestoff) )
		{
			pmatch->ml = lo_ml;
			pmatch->off = -lo_index;
			return true;
		}
		else
		{
			pmatch->ml = bestml;
			pmatch->off = bestoff;
			return true;
		}
	}
}

// NOTE: use for lazy parsers etc. to reduce code bloat
template<typename newLZ_CTMF>
static RADNOINLINE
bool newLZ_get_match_heuristic(match *pmatch,
	newLZ_CTMF * ctmf, const newLZ_LOs & lastoffsets, const U8 * ptr, const U8 * ptr_matchend, int mml, const U8 * literals_start, U32 dictionarySize,
	const OodleLZ_CompressOptions *pOptions)
{
	return newLZ_get_match_heuristic_inline(pmatch,ctmf,1,lastoffsets,ptr,ptr_matchend,mml,literals_start,dictionarySize,true,pOptions);
}

struct newlz_passinfo
{
	U32	literal_histo_raw[256];
	U32	literal_histo_sub[256];
	U32	packet_histo[256];
	U32	excess_histo[256];
	int offset_alt_modulo; // == 0 for offsets44
	U32	offset_histo1[256];
	U32	offset_histo2[256]; // filled of modulo > 1
};
	
static SINTa newLZ_put_parse(
	newlz_encoder_scratch * scratch,
	F32 * pJ,
	int * pchunktype,
	const U8 * chunk_ptr,int chunk_len,U8 * comp,U8 * comp_end,
	SINTa chunk_pos,const newlz_vtable * vtable,
	const vector_a<newlz_encoder_parse> & parsevec,
	newlz_passinfo * passinfo,
	const OodleKrakenChunkDeadlines * deadline
	);

CHECK(static const U8 * check_buf);


static SINTa newLZ_encoder_arrays_output(
	// filled :
	F32 * pJ,
	int * pchunktype,
	newlz_passinfo * passinfo,
	U8 * comp, U8 * comp_end,

	// read :
	const newlz_vtable * vtable,
	newlz_encoder_scratch * scratch,
	newLZ_encoder_arrays * encarrays,
	SINTa chunk_pos,
	const OodleKrakenChunkDeadlines * deadline
	)
{

	SIMPLEPROFILE_SCOPE_N(arrays_output,encarrays->chunk_len);

	int chunk_len = encarrays->chunk_len;
	const U8 * chunk_ptr = encarrays->chunk_ptr;
	rrArenaAllocator * arena = scratch->arena;
	U32 entropy_flags = vtable->entropy_flags;
	OodleLZ_CompressionLevel level = vtable->level;
	F32 lambda = vtable->lambda;
	const OodleSpeedFit * speedfit = vtable->speedfit;
	*pJ = LAGRANGE_COST_INVALID;

	// check for collisions :
	RR_ASSERT( encarrays->literals_ptr_raw < encarrays->literals_space_sub );
	RR_ASSERT( encarrays->literals_ptr_sub < encarrays->packets_space );
	RR_ASSERT( encarrays->packets_ptr <= encarrays->offsets_u8_space );
	RR_ASSERT( encarrays->offsets_u8_ptr <= (U8 *)encarrays->offsets_space );
	RR_ASSERT( encarrays->excess_u8_ptr <= (U8 *)encarrays->excess_u32_space );
	RR_ASSERT( (U8 *)encarrays->excess_u32_ptr <= encarrays->excess_ptr_end );

	SINTa packet_count = encarrays->packets_ptr - encarrays->packets_space;
	// forbid edge case of zero packets :
	//	(allow for version >= 6)
	if ( packet_count == 0 && g_OodleLZ_BackwardsCompatible_MajorVersion < 6 )
	{
		// just let the outer all-huff case do this
		return chunk_len;
	}

	// init histograms to 0
	if ( passinfo )
	{
		RR_ZERO( *passinfo );
	}

	//===========================================================

	U8 * comp_ptr = comp;

	// stuff first bytes raw :
	SINTa start_pos = ( chunk_pos == 0 ) ? NEWLZ_MIN_OFFSET : 0;
	for(SINTa i=0;i<start_pos;i++)
	{
		*comp_ptr++ = chunk_ptr[i];
	}

	//===========================================================

	// put new-style separate excess stream header first, when present
	// also prepares excess stream (we need to to know the size) and
	// stashes it at comp_end until we move it to its final location

	SINTa excess_stream_bytes = 0;
	int excesses_u32_hdr_size = 0;

	if ( vtable->bitstream_flags & NEWLZ_BITSTREAM_FLAG_SEND_EXCESS_SIZE )
	{
		// subtract u32 pointers = count
		int excesses_u32_count = (int)( encarrays->excess_u32_ptr - encarrays->excess_u32_space );

		excess_stream_bytes = newLZ_put_excesses_u32_separate(comp_ptr,comp_end,encarrays->excess_u32_space,excesses_u32_count);

		if ( excess_stream_bytes < 0 )
			return chunk_len;

		// move comp_end to reflect the storage taken up for the excesses_u32 stream
		comp_end -= excess_stream_bytes;

		if ( comp_end - comp_ptr < 2 ) // 2 bytes = max excesses_u32 header size
			return chunk_len;

		// layout:
		// bit  7    = "new format" flag
		// bit  6    = reserved (must be 0)
		// bits[5:0] = excess_u32_byte_count encoding
		//
		// encoded value: 0-31 means that value
		// 32-63 means an extra byte follows just before the offsets stream,
		// stream size is value + (extra_byte << 5)
		//
		// max of that encoding is 63 + (255<<5) = 8223 bytes, which is overprovisioned:
		//   excess_u32 implies excess>=255 so we can only get one every 256 bytes (a bit less really)
		//   giving <512 excess_u32s per 128k chunk.
		//
		//   if each of those were the maximum 35 bits noted for VARBITS_GET_EXPGOLOMB, we'd get
		//   512 * 35 bits = 17920 bits = 2.2k (although that's actually a big overestimate since
		//   we have EXCESS_EXPGOLOMB_SHIFT=6 and LRLs/MLs are limited to 128k (17 bits), making
		//   the max per code more like 6 + (17-6)*2 + 1 = 25 bits, and furthermore any code this
		//   large means there can't be many more excess_u32s!
		//
		//   the actual worst case is probably each EXCESS_EG code minimum length (6 + 1 = 7 bits)
		//   or close to, giving 512 * 7bits = 3584 bits = 448 bytes.

		if ( excess_stream_bytes < 32 )
		{
			*comp_ptr++ = 0x80 + (U8)excess_stream_bytes;
			excesses_u32_hdr_size = 1;
		}
		else
		{
			RR_ASSERT(excess_stream_bytes < 64 + (255<<5));
			*comp_ptr++ = 0x80 + (32 + (U8) (excess_stream_bytes & 31));
			*comp_ptr++ = (U8) ((excess_stream_bytes - 32) >> 5);
			excesses_u32_hdr_size = 2;
		}
	}

	//===========================================================

	// put literals

	SINTa literal_count = encarrays->literals_ptr_raw - encarrays->literals_space_raw;
	bool have_sub_lits = (encarrays->literals_ptr_sub != encarrays->literals_space_sub); // some modes don't even try to emit sub lits, so check
	RR_ASSERT( !have_sub_lits || literal_count == encarrays->literals_ptr_sub - encarrays->literals_space_sub );

	F32 literals_J = LAGRANGE_COST_INVALID;
	F32 literals_uncompressed_J = 3 + (F32)literal_count;
	SINTa literal_comp_len = -1;

	if ( literal_count < NEWLZ_HUFF_ARRAY_MIN_SIZE || level <= OodleLZ_CompressionLevel_HyperFast4 /* HF4 doesn't bother with Huffman lits */ )
	{
		// literal_space is empty or tiny ; put a NULL or raw array :
		*pchunktype = NEWLZ_LITERALS_TYPE_RAW;
		//SINTa literal_comp_len = newLZ_put_array(comp_ptr,comp_end,encarrays->literals_space_raw,literal_count,entropy_flags,lambda,&literals_J,arena,level,
		//	NULL );
		literal_comp_len = newLZ_put_array_uncompressed(comp_ptr,comp_end,encarrays->literals_space_raw,literal_count);
		if ( literal_comp_len < 0 )
			return chunk_len;
		comp_ptr += literal_comp_len;

		literals_J = literals_uncompressed_J;
	}
	else
	{
		U32 histo_raw[256];
		U32 histo_sub[256];

		CountHistoArrayU8(encarrays->literals_space_raw,literal_count,histo_raw,256);
		if ( have_sub_lits )
			CountHistoArrayU8(encarrays->literals_space_sub,literal_count,histo_sub,256);

		if ( passinfo )
		{
			memcpy(passinfo->literal_histo_raw,histo_raw,sizeof(histo_raw));
			if ( have_sub_lits )
				memcpy(passinfo->literal_histo_sub,histo_sub,sizeof(histo_sub));
		}

		// @@?? in Optimal levels don't try to estimate the cost of raw/sub , just put both :
		bool do_both_ways = level >= OodleLZ_CompressionLevel_Optimal2;
		bool do_put_sub = do_both_ways;
		F32 sub_literal_time_J = 0.0f;

		if ( !have_sub_lits )
		{
			do_both_ways = false;
			do_put_sub = false;
		}
		else
		{
			sub_literal_time_J = lambda * speedfit->parse_Kraken_subliterals(literal_count);

			if ( ! do_both_ways )
			{
				S32 bits_raw = newlz_array_estimate_complen_bits(histo_raw,256,(U32)literal_count);
				S32 bits_sub = newlz_array_estimate_complen_bits(histo_sub,256,(U32)literal_count);

				F32 J_sub = bits_sub/8.f + sub_literal_time_J;
				F32 J_raw = bits_raw/8.f;

				do_put_sub = J_sub < J_raw;
			}
		}

		#ifdef SPEEDFITTING
		RR_ASSERT_ALWAYS( have_sub_lits ); // don't try speedfitting this in hyperfast modes that don't emit sub lits
		if ( g_speedfitter_stage == 3 )
			do_put_sub = ( g_stage3_literal_mode == NEWLZ_LITERALS_TYPE_SUB );
		#endif
				
		// @@ TODO : if estimate J is > literals_count just send it uncompressed without trying huff at all?

		if ( do_put_sub )
		{
			// try sub
			*pchunktype = NEWLZ_LITERALS_TYPE_SUB;

			literal_comp_len = newLZ_put_array_histo(comp_ptr,comp_end,encarrays->literals_space_sub,literal_count,
				histo_sub,entropy_flags,lambda,speedfit,&literals_J,deadline->literal,arena,level);

			// sub_literal_time is now on literals_J ; not in the parse_time
			literals_J += sub_literal_time_J;
		}

		bool do_put_raw = do_both_ways;
	
		#ifdef SPEEDFITTING
		if ( g_speedfitter_stage == 3 )
			do_put_raw = ( g_stage3_literal_mode == NEWLZ_LITERALS_TYPE_RAW );
		#endif
			
		// if we didn't put sub, or sub expanded, put raw :
		if ( literal_comp_len < 0 || literal_comp_len >= literal_count ||
			literals_J > literals_uncompressed_J )
		{
			literals_J = LAGRANGE_COST_INVALID;	// make sure we re-put
			do_put_raw = true;
		}
		
		if ( do_put_raw )
		{					
			// when do_both_ways is on, literals_J is left in from sub,
			//	and this only writes to comp if it improves
	
			SINTa literal_comp_len_raw = newLZ_put_array_histo(comp_ptr,comp_end,encarrays->literals_space_raw,literal_count,
				histo_raw,entropy_flags,lambda,speedfit,&literals_J,deadline->literal,arena,level);
				
			if ( literal_comp_len_raw > 0 )
			{
				literal_comp_len = literal_comp_len_raw;
				*pchunktype = NEWLZ_LITERALS_TYPE_RAW;
			}
		}
		
		if ( literal_comp_len < 0 )
			return chunk_len;

		RR_ASSERT( literal_comp_len >= 0 );
		comp_ptr += literal_comp_len;
		
		RR_ASSERT( literals_J >= literal_comp_len );
		RR_ASSERT( lambda != 0.f || literals_J == literal_comp_len );
	}

	RR_ASSERT( literals_J <= literals_uncompressed_J );

	//===========================================================

	// put packets

	F32 packet_J = LAGRANGE_COST_INVALID;

	SINTa packet_comp_len = newLZ_put_array(comp_ptr,comp_end,encarrays->packets_space,packet_count,
		entropy_flags,lambda,speedfit,&packet_J,deadline->packet,arena,level,
		passinfo ? passinfo->packet_histo : NULL
		);

	if ( packet_comp_len < 0 )
		return chunk_len;

	comp_ptr += packet_comp_len;

	//rrprintfvar(packet_count);
	//rrprintfvar(packet_comp_len);

	//===========================================================
	// put offsets U8

	int offsets_count = rrPtrDiff32( encarrays->offsets_u8_ptr - encarrays->offsets_u8_space );
	RR_ASSERT( offsets_count == (int)( encarrays->offsets_ptr - encarrays->offsets_space ) );
	
	int offset_alt_modulo = 0; // modulo = 0 means 44 offsets
	rrbool try_alt_offsets = ( vtable->bitstream_flags & NEWLZ_BITSTREAM_FLAG_ALT_OFFSETS );
		
	F32 offsets_J = LAGRANGE_COST_INVALID;
	// offsets_J gets the offset newlz_array J's
	//	 and the offset getbits time
	//	 BUT NOT the offset raw bits time

	SINTa offsets_comp_len = newLZ_put_offset_44_or_alt_arrays(comp_ptr,comp_end,
		encarrays->offsets_u8_space,encarrays->offsets_space,offsets_count,
		entropy_flags,lambda,speedfit,&offsets_J,deadline->offsets,
		eNewLZ_MinOffset_8,try_alt_offsets,&offset_alt_modulo,
		arena,level,
		passinfo ? passinfo->offset_histo1 : NULL,
		passinfo ? passinfo->offset_histo2 : NULL
		);
	
	if ( offsets_comp_len < 0 )
		return -1;
	
	comp_ptr += offsets_comp_len;
	
	RR_ASSERT( offsets_J >= offsets_comp_len );
	RR_ASSERT( lambda > 0.f || offsets_J == offsets_comp_len );
	
	if ( passinfo )
		passinfo->offset_alt_modulo = offset_alt_modulo;
	
	//===========================================================

	// put excesses U8

	F32 excesses_u8_J = LAGRANGE_COST_INVALID;
	int excesses_u8_count = rrPtrDiff32( encarrays->excess_u8_ptr - encarrays->excess_u8_space );

	SINTa excesses_u8_comp_len = newLZ_put_array(comp_ptr,comp_end,encarrays->excess_u8_space,excesses_u8_count,
		entropy_flags,lambda,speedfit,&excesses_u8_J,deadline->excesses_u8,arena,level,
		passinfo ? passinfo->excess_histo : NULL
		);

	if ( excesses_u8_comp_len < 0 )
		return chunk_len;

	comp_ptr += excesses_u8_comp_len;

	//rrprintfvar(excesses_u8_count);

	//===========================================================

	// verify that decoder will have enough scratch space :
	// -> this must be an over-estimate so that decoder will never fail to have space unexpectedly

	// space for the retained arrays :
	SINTa decoder_scratch_space_needed_arrays =
		literal_count + packet_count +
		offsets_count * 6 +  // *6 for alt offsets, two U8's + U32
		excesses_u8_count * 5;  // *5 to have a U8 and U32 of each

	//	and double the size of the largest entropy array to account for indexed scratch
	// find the largest working set needed :
	SINTa phase1_scratch_needed = decoder_scratch_space_needed_arrays + RR_MAX(offsets_count,excesses_u8_count);
	phase1_scratch_needed = RR_MAX(phase1_scratch_needed, literal_count*2 );
	phase1_scratch_needed = RR_MAX(phase1_scratch_needed, literal_count + packet_count*2 );
	phase1_scratch_needed += NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH; 
	phase1_scratch_needed += OODLELZ_SCRATCH_ALIGNMENT_PAD; // + 16 for excess alignment
	
	SINTa decoder_scratch_space_needed = phase1_scratch_needed + 
											OODLELZ_MAX_SCRATCH_FOR_PHASE_HEADERS_AND_FUZZ;

	SINTa min_scratch_avail = OodleLZ_Compressor_ScratchMemSize(vtable->compressor,chunk_len);

	// decoder_scratch_space_needed this need to be always bigger than
	//	what the decoder actually uses on this data (decoder_scratch_space_used)
	//rrprintfvar(decoder_scratch_space_needed);

	RR_ASSERT( decoder_scratch_space_needed <= min_scratch_avail ); // <- I've never seen this
	if ( decoder_scratch_space_needed > min_scratch_avail )
	{
		// bail out, return expansion
		ooLogError("newLZ chunk needs too much scratch for decoder!\n");
		return chunk_len;
	}

	//=============================================================
	// put offsets and excesses varbits

	// subtract u32 pointers = count
	int excesses_u32_count = (int)( encarrays->excess_u32_ptr - encarrays->excess_u32_space );

	SINTa varbits_comp_len = newLZ_put_offsets_excesses(comp_ptr,comp_end,
		encarrays->offsets_u8_space,
		encarrays->offsets_space,
		offsets_count,
		offset_alt_modulo,
		encarrays->excess_u32_space,
		excesses_u32_count,
		excesses_u32_hdr_size,excess_stream_bytes);
		
	if ( varbits_comp_len < 0 )
		return chunk_len;
	
	//rrprintfvar(varbits_comp_len);
	
	comp_ptr += varbits_comp_len;

	SINTa total_comp = rrPtrDiff( comp_ptr - comp );

	// if we expand then don't need to calc J
	if ( total_comp >= chunk_len )
		return chunk_len;

	F32 parse_time = speedfit->parse_Kraken(chunk_len,packet_count,excesses_u8_count);
	// if parse time misses our deadline, bail
	if ( parse_time > deadline->parse )
		return chunk_len;

	SINTa parse_complen = varbits_comp_len + start_pos + excesses_u32_hdr_size;
	F32 parse_J = parse_complen + lambda * parse_time;

	// literals_J has sub literal time
	// offsets_J has get_offsets time

	F32 total_J = literals_J + packet_J + offsets_J + excesses_u8_J + parse_J;

	// add get_excesses time :
	total_J += lambda * speedfit->get_excesses(excesses_u8_count,excesses_u32_count);

	// speedfitters can supply an alternative J calc
	if ( speedfit->recompute_Kraken_J )
	{
		total_J = speedfit->recompute_Kraken_J(
			total_comp, lambda,
			literal_comp_len, literals_J,
			packet_comp_len, packet_J,
			offsets_comp_len, offsets_J,
			excesses_u8_comp_len, excesses_u8_J,
			parse_complen, parse_time
		);
	}

	// to estimate the scale difference between HW and SW cycles for a given file
	#if 0
	rrprintf("RATIO:%d,%8.1f,%6.1f\n", (int)(chunk_pos >> 17), total_J, (total_J - total_comp)/lambda);
	#endif

	*pJ = total_J;

	RR_ASSERT( total_J >= total_comp );
	RR_ASSERT( lambda > 0.f || total_J == total_comp );

	return total_comp;
}

	
//=============================================================================
	
// t_do_lazy_parse can be 1 or 2
template<typename newLZ_CTMF,int t_do_lazy_parse,int t_do_match_backup,int t_step_literals_shift>
static SINTa newLZ_encode_chunk(const newlz_vtable * vtable,
	newlz_encoder_scratch * scratch,
	const U8 * dictionaryBase,
	const U8 * chunk_ptr,int chunk_len,
	U8 * comp,U8 * comp_end,
	SINTa chunk_pos,
	int * pchunktype,
	F32 * pJ,
	const OodleKrakenChunkDeadlines * deadline)
{
	*pchunktype = -1;
	//rrprintf("newLZ_encode_chunk : %d\n",chunk_len);
	
	if ( chunk_len <= NEWLZ_MIN_CHUNK_LEN )
	{
		return chunk_len;
	}

	SIMPLEPROFILE_SCOPE_N(encode_chunk,chunk_len);
	
	CHECK( check_buf = chunk_ptr );

	const OodleLZ_CompressOptions * pOptions = vtable->pOptions;
	U32 dictionarySize = ( pOptions->dictionarySize > 0 ) ?
		RR_MIN(NEWLZ_MAX_OFFSET,pOptions->dictionarySize)
		: NEWLZ_MAX_OFFSET;

	int mml = RR_MAX(pOptions->minMatchLen,NEWLZ_MML_NORMAL);
		
	newLZ_CTMF * ctmf = (newLZ_CTMF *)vtable->matcher;

	newLZ_encoder_arrays encarrays;
	newLZ_encoder_arrays_point_to_scratch(&encarrays,scratch,chunk_len,chunk_ptr,vtable->bitstream_flags);

	// ptr_matchend is the match *end* limit
	//	need -8 cuz we do blind U64 match copies :
	const U8 * ptr_matchend = chunk_ptr + chunk_len - NEWLZ_MATCH_END_PAD;

	// parse_end_pos is the match *start* pos limit
	// NOTE the last match may not start after parse_end_pos (it may start _at_
	// parse_end_pos), and it may not extend into the last NEWLZ_MATCH_END_PAD
	// bytes. Since END_PAD=8 and NO_MATCH_ZONE=16, that means we have a
	// fairly generous amount of padding between parse_end_pos and the actual
	// end of the buffer, so there's no concern about +1 byte padding for lazy
	// matches or matchers grabbing 8 source bytes at a time or that sort of
	// thing.
	int parse_end_pos = chunk_len - NEWLZ_CHUNK_NO_MATCH_ZONE;
	
	newLZ_LOs lastoffsets;
	lastoffsets.Reset();

	// start at pos 1 :
	int start_pos = 0;
	
	if ( chunk_pos == 0 )
	{
		// start literals at NEWLZ_MIN_OFFSET so sub doesn't have to check
		start_pos = NEWLZ_MIN_OFFSET;
	}

	const U8 * start_ptr = chunk_ptr + start_pos;
	const U8 * literals_start = start_ptr;
	
	// ctmf->set_next is safe here because of min chunk len check
	RR_ASSERT( start_pos < parse_end_pos );
	
	// start ctmf :
	ctmf->set_next(start_ptr);

	const U8 * parse_end_ptr = chunk_ptr + parse_end_pos;
	
	{
	SIMPLEPROFILE_SCOPE_N(heuristic_parse,chunk_len);
		
	for(const U8 * ptr = start_ptr; ;)
	{
		RR_ASSERT( ptr < parse_end_ptr );

		// literal loop: seek the next match
		match chosen;
		U32 scaled_skip_dist = 1 << t_step_literals_shift;

		// if t_step_literals_shift is on, t_do_lazy_parse must be off :
		RR_COMPILER_ASSERT( t_do_lazy_parse==0 || t_step_literals_shift==0 ); // not compatible

		for(;;)
		{
			RR_ASSERT( ctmf->m_next_ptr == ptr );

			if ( !t_do_lazy_parse )
			{
				if ( newLZ_find_lo0_match_ahead(&chosen,ctmf,lastoffsets,ptr,ptr_matchend) )
				{
					ptr++;
					break;
				}
			}

			U32 step = t_step_literals_shift ? (scaled_skip_dist >> t_step_literals_shift) : 1;

			// if skipping that far would put us past the parse end, bail.
			// have to check this before get match because it does prefetch next
			if ( rrPtrDiff(parse_end_ptr - ptr) <= (SINTa)step )
				goto parse_chunk_done;

			// once we find a match, we can stop!
			if ( newLZ_get_match_heuristic_inline(&chosen,ctmf,step,lastoffsets,ptr,ptr_matchend,mml,literals_start,dictionarySize,true,pOptions) )
				break;

			if ( t_step_literals_shift )
			{
				scaled_skip_dist++;
				// NOTE(fg): limit step size - old code did this.
				// scaled_skip_dist = RR_MIN(scaled_skip_dist, 12<<t_step_literals_shift);
				// CB - no need to do this any more
				// kind of meh whether to do it or not; I'm removing it just to make newlz match newlzf
			}

			ptr += step;
		}
		
		// idea : don't do lazy if I have an LO0 match, cuz it puts a 0 lit
		// -> pretty meh and just random whether it helps or not
		//  // && chosen.off != 0 )
		// 10,305,575 -> 10,304,294
		// 3,345,935  ->  3,345,421
		// 57,602,299 -> 57,610,708
		if ( t_do_lazy_parse )
		{
			//SIMPLEPROFILE_SCOPE(lazy);
			
			// dickens -z4
			// lazy repeat : (while loop to look again)
			//  3,445,700
			//  3,420,080
			//  -> very small benefit
			// lazy2 : (look at +1 and +2)
			// 3,367,284 
			//  -> big benefit!
			
			while(ptr+1<parse_end_ptr)
			{
				// lazy parse
				
				match lazy;
				// what should prefetch step be for lazy parse?
				//	I guess 1 is good because we'll do an insert there when we advance
				if ( newLZ_get_match_heuristic(&lazy,ctmf,lastoffsets,ptr+1,ptr_matchend,mml,literals_start,dictionarySize,pOptions) &&
					 newLZ_LazyMatchDelta(lazy.ml,lazy.off,lazy.IsLO(),chosen.ml,chosen.off,chosen.IsLO()) > 0 )
				{
					// insert a literal :
					ptr++;
					// take lazy match :
					chosen = lazy;
				}
				else if ( t_do_lazy_parse >= 2 ) // lazy2 check <- @@ DON'T DO THIS ON 64k chunks <- this is Kraken Normal speed getting worse
				{
					// lazy2
					if ( ptr+2 >= parse_end_ptr ) break;
					// don't do this if we might take an ml==2 step !
					if ( chosen.ml == 2 ) break;
				
					// IsLazy2Better : check newLZ_LazyMatchDelta > 3 :
					// 0: 3,348,789 
					// 3: 3,344,832
					if ( newLZ_get_match_heuristic(&lazy,ctmf,lastoffsets,ptr+2,ptr_matchend,mml,literals_start,dictionarySize,pOptions) &&
						 newLZ_LazyMatchDelta(lazy.ml,lazy.off,lazy.IsLO(),chosen.ml,chosen.off,chosen.IsLO()) > NEWLZ_LAZYBETTER_LAZY2 )
					{
						// insert a literal :
						ptr+=2;
						// take lazy match :
						chosen = lazy;
					}
					else
					{				
						break;
					}
				}
				else
				{
					break;
				}
			}
		}
		// normal match is < parse_end_ptr ; lazy can be == parse_end_ptr
		RR_ASSERT( ptr <= parse_end_ptr );
		
		// have a match
		RR_ASSERT( chosen.ml >= NEWLZ_MML_NORMAL || chosen.IsLO() );
		
		S32 offset = chosen.IsLO() ? lastoffsets.lasts[-chosen.off] : chosen.off;
		
		// verify the match is valid :	
		RR_ASSERT( memcmp(ptr,ptr-offset,chosen.ml) == 0 );

		if ( t_do_match_backup )
		{
			//SIMPLEPROFILE_SCOPE(match_backup);
			
			// try match start backup :
			//  @@ ideally this would be done per-match in the CTMF, not after selecting one
			while( ptr > literals_start )
			{
				if ( rrPtrDiff(ptr - ctmf->m_base_ptr) <= offset )
					break;
				if ( ptr[-1] == ptr[-1-(SINTa)offset] )
				{
					ptr--;
					chosen.ml++;
				}
				else
				{
					break;
				}
			}
		}
				
		// force a pure LO0-LRL0 exclusion
		// the only time this should happen is at the very start of the file
		// when pos == start_pos , we could match from the initial lastoffsets
		// in that case, just bump index from 0 to 1
		if ( chosen.off == 0 && ptr == literals_start )
		{
			RR_ASSERT( lastoffsets.lasts[0] == NEWLZ_MIN_OFFSET );
			RR_ASSERT( lastoffsets.lasts[1] == lastoffsets.lasts[0] );
			chosen.off = -1;
		}

		// make sure match backup didn't violate LO0LRL0 exclusion :
		RR_ASSERT( ptr > literals_start || chosen.off != 0 );
		int lrl = rrPtrDiff32( ptr - literals_start );
				
		newLZ_encoder_arrays_put_packet<0,1>(&encarrays,&lastoffsets,lrl,chosen.ml,chosen.off,literals_start);
		
		ptr += chosen.ml;
		literals_start = ptr;
		if ( ptr >= parse_end_ptr )
			break;

		// should only check this on the *last* chunk :
		// this isn't needed cuz chunk overrun zone gives plenty of padding already
		ctmf->step_and_insert(ptr-chosen.ml,chosen.ml);
	}

parse_chunk_done:;
	} // profiler scope

	// output!
	newLZ_encoder_arrays_finish_chunk<1>(&encarrays,literals_start);

	SINTa complen = newLZ_encoder_arrays_output(pJ,pchunktype,NULL,comp,comp_end,vtable,scratch,&encarrays,chunk_pos,deadline);
	return complen;
}

static RADFORCEINLINE U32 different_bytes1to3le(U32 a, U32 b)
{
	return (a ^ b) >> 8;
}

template<typename newLZ_CTMF,int t_step_literals_shift,int t_do_match_backup,int t_quadratic_skip,int t_try_sub_lits,int t_lo12_allowed,int t_prefetch_hash>
static SINTa newLZ_encode_chunk_fast_mode(const newlz_vtable * vtable,
	newlz_encoder_scratch * scratch,
	const U8 * dictionaryBase,
	const U8 * chunk_ptr,int chunk_len,
	U8 * comp,U8 * comp_end,
	SINTa chunk_pos,
	int * pchunktype,
	F32 * pJ,
	const OodleKrakenChunkDeadlines * deadline)
{
	typedef typename newLZ_CTMF::hash_type HashType;
	*pchunktype = -1;

	if ( chunk_len <= NEWLZ_MIN_CHUNK_LEN )
		return chunk_len;

	SIMPLEPROFILE_SCOPE_N(encode_chunk_fastmode,chunk_len);

	CHECK( check_buf = chunk_ptr );

	const OodleLZ_CompressOptions * pOptions = vtable->pOptions;
	U32 dictionarySize = ( pOptions->dictionarySize > 0 ) ?
		RR_MIN(NEWLZ_MAX_OFFSET,pOptions->dictionarySize)
		: NEWLZ_MAX_OFFSET;

	U32 dictionary_span = dictionarySize - NEWLZ_MIN_OFFSET;

	newLZ_CTMF * ctmf = (newLZ_CTMF *)vtable->matcher;

	newLZ_encoder_arrays encarrays;
	newLZ_encoder_arrays_point_to_scratch(&encarrays,scratch,chunk_len,chunk_ptr,vtable->bitstream_flags);

	newLZ_LOs lastoffsets;
	lastoffsets.Reset();

	// ptr_matchend is the match *end* limit
	//	need -8 cuz we do blind U64 match copies :
	const U8 * ptr_matchend = chunk_ptr + chunk_len - NEWLZ_MATCH_END_PAD;

	// parse_end_pos is the match *start* pos limit
	// NOTE the last match may not start after parse_end_pos (it may start _at_
	// parse_end_pos), and it may not extend into the last NEWLZ_MATCH_END_PAD
	// bytes. Since END_PAD=8 and NO_MATCH_ZONE=16, that means we have a
	// fairly generous amount of padding between parse_end_pos and the actual
	// end of the buffer, so there's no concern about +1 byte padding for lazy
	// matches or matchers grabbing 8 source bytes at a time or that sort of
	// thing.
	int parse_end_pos = chunk_len - NEWLZ_CHUNK_NO_MATCH_ZONE;

	int start_pos = (chunk_pos == 0) ? NEWLZ_MIN_OFFSET : 0;
	SINTa neg_lo0 = -NEWLZ_MIN_OFFSET;
	const U8 * start_ptr = chunk_ptr + start_pos;
	const U8 * parse_end_ptr = chunk_ptr + parse_end_pos;
	const U8 * literals_start = start_ptr;

	const U8 * hash_base = ctmf->m_base_ptr;
	HashType * hash_table = ctmf->m_hash_table.data();
	U64 hash_mul = ctmf->m_hash_mul;
	int hash_shift = ctmf->m_hash_shift;

	// HIGH CONCEPT:
	// - Single-way cache table, no hash conflict bits
	// - Only use rep offset 0, and only the "lazy LO0" kind
	//   (means we can also use a simplified packet emission func)
	// - No hash prefetch (assumption being the hash table should be L1-sized for hyperfast modes)
	// - Keep it straight-line so it's easy to see exactly what the code is doing

	{
	SIMPLEPROFILE_SCOPE_N(fast_parse,chunk_len);

	for(const U8 * ptr = start_ptr; ;)
	{
		RR_ASSERT( ptr < parse_end_ptr );

		// literal loop: seek the next match
		const U8 * match_start; // where the match begins in the parse
		SINTa neg_offs;
		S32 code_offs;
		SINTa scaled_skip_dist = 1 << t_step_literals_shift;

		for(;;)
		{
			// This is the address we will be looking at if we don't find a match at the current
			// location.
			//
			// Testing this here is a bit early, but we need to test this here for match
			// prefetches, and it's preferable to do this here (and not in the prefetch code
			// itself) so that the decision between prefetching or not doesn't affect the
			// output bitstream.
			SINTa step = scaled_skip_dist >> t_step_literals_shift;
			if ( rrPtrDiff(parse_end_ptr - ptr) <= step )
				goto parse_chunk_done;

			// prefetch next hash lookup if desired
			if ( t_prefetch_hash )
			{
				UINTa hash_next = fast_ctmf_hash(RR_GET64_LE_UNALIGNED(ptr+step), hash_mul, hash_shift);
				RR_PREFETCHRW_CL(&hash_table[hash_next]);
			}

			U64 ptr64le = RR_GET64_LE_UNALIGNED(ptr);
			U32 ptr32le = (U32)ptr64le;

			// hash lookup and insert
			SINTa pos = rrPtrDiff(ptr - hash_base);
			HashType hashpos = fast_ctmf_insert(hash_table, ptr64le, hash_mul, hash_shift, pos);

			// if we're tracking other last offsets, try to make them happen (with LOMML=2)
			if ( t_lo12_allowed )
			{
				U16 ptr16le = (U16)ptr64le;
				if ( ptr16le == RR_GET16_LE_UNALIGNED(ptr - lastoffsets.lasts[1]) )
				{
					code_offs = -1;
					match_start = ptr;
					neg_offs = -lastoffsets.lasts[1];

					// no backup after LO1/2 match
					ptr += 2;
					goto got_match;
				}
				else if ( ptr16le == RR_GET16_LE_UNALIGNED(ptr - lastoffsets.lasts[2]) )
				{
					code_offs = -2;
					match_start = ptr;
					neg_offs = -lastoffsets.lasts[2];

					// no backup after LO1/2 match
					ptr += 2;
					goto got_match;
				}
			}

			// check for rep0 match *at next byte* (pseudo-lazy), MMl 3
			if ( !different_bytes1to3le(ptr32le, RR_GET32_LE_UNALIGNED(ptr+neg_lo0)) )
			{
				ptr++;
				neg_offs = neg_lo0;
				code_offs = 0;

				// We already inserted at the previous position (that we just skipped over).
				// My testing says that this second insert is almost zero net cost, even at
				// HyperFast3 (0.02 cycles/byte more), but it's a tidy ratio win.
				fast_ctmf_insert(hash_table, ptr, hash_mul, hash_shift, pos + 1);

				// no backup after MMl3 LO0 match
				match_start = ptr;
				ptr += 3;
				goto got_match;
			}

			// do we have a len >=4 valid match?
			code_offs = (S32) (HashType)(pos - hashpos);
			U32 match32le = RR_GET32_LE_UNALIGNED(ptr - code_offs);
			if ( match32le == ptr32le )
			{
				// We can find matches with offs <NEWLZ_MIN_OFFSET here, which we want to handle,
				// because they denote runs.
				// Do it in this branchy way, which tests for any out-of-range offset whether
				// there's a match at MIN_OFFSET. This is a bit wonky, but helps with runs and,
				// just as importantly, does not seem to be a big perf loss in the typical case.
				if ( (U32)(code_offs - NEWLZ_MIN_OFFSET) < dictionary_span )
				{
					neg_offs = -code_offs;
					// NOTE: not checking for the regular normal parse rules here
					// (enforced larger MML for higher offsets)
					//
					// Could test this here, but considering we're doing a single-way, small cache
					// table, it seems like such matches would be really rare anyway.
					//
					// Since this is purely a decoder perf issue, we should be fine anyway.
					break;
				}
				else if ( ptr32le == RR_GET32_LE_UNALIGNED(ptr - NEWLZ_MIN_OFFSET) )
				{
					code_offs = NEWLZ_MIN_OFFSET;
					neg_offs = -NEWLZ_MIN_OFFSET;
					break;
				}
			}

			// compute step update
			if ( t_quadratic_skip )
			{
				// NOTE(fg): quadratic skip is _really_ aggressive, but might be worth it for really fast levels?
				// let's give a try.
				scaled_skip_dist += rrPtrDiff(ptr - literals_start)>>t_quadratic_skip;
				// Clamp it so we don't get completely ridiculous...
				// I changed this to 37 (prime) from 32 somewhat superstitiously:
				// I'm worried about clamping the skip distance at a power of 2
				// because pow2 record sizes are common and we might just get really
				// unlucky. A prime seems less likely to fall into that trap.
				scaled_skip_dist = RR_MIN(scaled_skip_dist,37 << t_step_literals_shift);
			}
			else
				scaled_skip_dist++;

			ptr += step;
		}

		// do match backup if requested
		if ( t_do_match_backup )
		{
			// try match start backup :
			while( ptr > literals_start )
			{
				if ( rrPtrDiff(hash_base - ptr) >= neg_offs ||
					 ptr[-1] != ptr[neg_offs-1] )
					break;

				ptr--;
			}

			// force a pure LO0-LRL0 exclusion
			// the only time this should happen is at the very start of the file
			// when pos == start_pos, we could match from the initial lastoffsets
			//
			// we never generate LO0s at the first position (since we only check at ptr+1
			// above), but we might back up into it. Therefore, we need to check if we
			// do match backup.
			//
			// unlike regular heuristic encoder, we don't support lo1 or higher on this path
			// so screw it, just make the offset explicit
			if ( ptr == literals_start && code_offs == 0 )
			{
				RR_ASSERT( neg_lo0 == -NEWLZ_MIN_OFFSET );
				code_offs = (S32)-neg_lo0;
			}
		}

		// determine match len
		match_start = ptr;
		ptr += 4; // we already checked that the first 4 bytes match (regular match)

	got_match:
		while (ptr < ptr_matchend)
		{
			UINTr big1 = *((UINTr *)ptr);
			UINTr big2 = *((UINTr *)(ptr + neg_offs));
			ptr += sizeof(UINTr);
			if ( big1 != big2 )
			{
				ptr -= sizeof(UINTr); // we didn't actually match, back up!
				ptr += GetNumBytesZeroNeverAllR(big1 ^ big2);
				break;
			}
		}
		ptr = RR_MIN(ptr,ptr_matchend); // we might have overshot

		// make sure match backup didn't violate LO0LRL0 exclusion :
		RR_ASSERT( match_start > literals_start || code_offs != 0 );
		int lrl = rrPtrDiff32( match_start - literals_start );
		int ml = rrPtrDiff32( ptr - match_start );

		newLZ_encoder_arrays_put_packet<t_lo12_allowed == 0,t_try_sub_lits>(&encarrays,&lastoffsets,lrl,ml,code_offs,literals_start);

		literals_start = ptr;
		neg_lo0 = neg_offs;
		if ( ptr >= parse_end_ptr )
			break;

		// only do partial inserts in the (slower) specializations where t_lo12_allowed != 0
		if ( t_lo12_allowed )
		{
			SINTa base_pos = rrPtrDiff(match_start - hash_base);
			for ( SINTa i = 1; i < ml; i += i )
				fast_ctmf_insert(hash_table, match_start + i, hash_mul, hash_shift, base_pos + i);
		}
	}

parse_chunk_done:;
	} // profile scope

	newLZ_encoder_arrays_finish_chunk<t_try_sub_lits>(&encarrays,literals_start);

	SINTa complen = newLZ_encoder_arrays_output(pJ,pchunktype,NULL,comp,comp_end,vtable,scratch,&encarrays,chunk_pos,deadline);
	return complen;
}

/*
// histo gathering for huff baking here :
U32 g_offsets_u8_histo[256] = { 0 };
U32 g_excesses_u8_histo[256] = { 0 };
U32 g_packet_histo[256] = { 0 };
*/

static SINTa newLZ_put_parse(
	newlz_encoder_scratch * scratch,
	F32 * pJ,
	int * pchunktype,
	const U8 * chunk_ptr,int chunk_len,U8 * comp,U8 * comp_end,
	SINTa chunk_pos,const newlz_vtable * vtable,
	const vector_a<newlz_encoder_parse> & parsevec,
	newlz_passinfo * passinfo,
	const OodleKrakenChunkDeadlines * deadline)
{
	// dump the parsevec into encoder_arrays, then output

	SIMPLEPROFILE_SCOPE(put_parse);

	*pJ = LAGRANGE_COST_INVALID;
	// forbid edge case of zero packets :
	SINTa parse_size = parsevec.size();
	if ( parse_size == 0 )
	{
		// just let the outer all-huff case do this
		return chunk_len;	
	}
	
	newLZ_encoder_arrays encarrays;
	newLZ_encoder_arrays_point_to_scratch(&encarrays,scratch,chunk_len,chunk_ptr,vtable->bitstream_flags);

	newLZ_LOs dummy_los;
	dummy_los.Reset();

	SINTa start_pos = (chunk_pos == 0) ? NEWLZ_MIN_OFFSET : 0;
	const U8 * literals_start = chunk_ptr + start_pos;

	for (SINTa s=0;s<parse_size;s++)
	{
		const newlz_encoder_parse & parse = parsevec[s];

		newLZ_encoder_arrays_put_packet<0,1>(&encarrays,&dummy_los,parse.lrl,parse.ml,parse.offset,literals_start);
		literals_start += parse.lrl + parse.ml;
	}

	newLZ_encoder_arrays_finish_chunk<1>(&encarrays,literals_start);

	SINTa complen = newLZ_encoder_arrays_output(pJ,pchunktype,passinfo,comp,comp_end,vtable,scratch,&encarrays,chunk_pos,deadline);
	return complen;
}

/*****************************************************************/

// for 128k chunks
// LRL or ML can be 128k ; so need 17 bits for log2 *2 = 34+1 = 35
//	quite a bit in other words; pretty much always have to refill after this

//================================================		

#ifndef DO_SSE4_PARSE_ALWAYS

#define NEWLZ_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_RAW
#define newLZ_decode_parse	newLZ_decode_parse_raw
#include "newlz_decode_parse_outer.inl"
#undef newLZ_decode_parse
#undef NEWLZ_DECODE_LITERALS_TYPE

#define NEWLZ_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_SUB
#define newLZ_decode_parse	newLZ_decode_parse_sub
#include "newlz_decode_parse_outer.inl"
#undef newLZ_decode_parse
#undef NEWLZ_DECODE_LITERALS_TYPE

static const newLZ_decode_parse_func_set newLZ_decode_parse_funcs_regular =
{
	newLZ_decode_parse_raw,
	newLZ_decode_parse_sub,
};

#endif // !DO_SSE4_PARSE_ALWAYS

//================================================		

// newLZ_decode_chunk_phase1 returns bool, but as 1/-1		
static SINTa newLZ_decode_chunk_phase1(
	int chunk_type,
	const U8 * comp,const U8 * chunk_comp_end,
	U8 * chunk_ptr,SINTa chunk_len, SINTa chunk_pos, 
	U8 * const scratch_space,
	U8 * const scratch_end,
	newLZ_chunk_arrays * arrays)
{
	//SIMPLEPROFILE_SCOPE_N(newLZ_dec_phase1,chunk_len);
	
	if ( chunk_type > 1 )
		return -1;
	
	RR_ASSERT( chunk_type == NEWLZ_LITERALS_TYPE_RAW || chunk_type == NEWLZ_LITERALS_TYPE_SUB );
	
	/*
	#ifndef __RADFUZZ__
	memset(scratch_space,-1,rrPtrDiff(scratch_end - scratch_space));
	#endif
	/**/
	
	newlz_array_get_printf("newlz chunk type %d = %s [%d->%d]\n",chunk_type,newlz_literals_type_name[chunk_type],chunk_len,rrPtrDiff32(chunk_comp_end-comp));
		
	// check for in-place decode comp-raw overlap :	
	bool inplace_comp_raw_overlap;
	//inplace_comp_raw_overlap = RR_MIN(comp_end,chunk_ptr+chunk_len) >= RR_MAX(comp,chunk_ptr);
	inplace_comp_raw_overlap = comp <= (chunk_ptr+chunk_len) && chunk_ptr <= chunk_comp_end;
	
	arrays->chunk_ptr = chunk_ptr;
	arrays->scratch_ptr = scratch_space;
	
	//rrprintf("newLZ_decode_chunk : %d\n",chunk_len);

	const U8 * comp_ptr = comp;

	// 4 array headers * 3 byte length = 12 bytes minimum
	//	+ offsets varbits 1 byte minimum = 13
	// (this also protects the first-8 raw byte read)
	if ( rrPtrDiff(chunk_comp_end-comp_ptr) < 13 )
		return -1;
			
	//---------------------------------------------
	
	U8 * to_ptr = chunk_ptr;
	//U8 * chunk_end = to_ptr + chunk_len;
	
	//int start_pos = 0;
	// on first chunk, start at NEWLZ_MIN_OFFSET
	if ( chunk_pos == 0 )
	{
		//start_pos = NEWLZ_MIN_OFFSET;
		
		// in-place issue ; to_ptr should never overlap comp here because of in-place padding
		
		RR_COMPILER_ASSERT( NEWLZ_MIN_OFFSET == 8 );
		lz_copy8(to_ptr,comp_ptr);
		to_ptr += 8;
		comp_ptr += 8;
	}
	
	//---------------------------------------------
	// excess extra header comes first

	U8 excess_hdr_byte = 0;
	S32 excess_stream_size = 0;
	if ( comp_ptr < chunk_comp_end && *comp_ptr >= 0x80 )
	{
		// !$!
		// Oodle 2.6.0 new format: excesses U32 stream size signaled explicitly.
		excess_hdr_byte = *comp_ptr++;
		if ( ( excess_hdr_byte & 0xc0 ) != 0x80 )
		{
			ooLogError("corruption : excess reserved flag bit nonzero\n");
			return -1;
		}

		excess_stream_size = excess_hdr_byte & 0x3f;
		if ( excess_stream_size >= 32 )
		{
			if ( comp_ptr == chunk_comp_end )
			{
				ooLogError("corruption : truncated excess byte count\n");
				return -1;
			}

			excess_stream_size += *comp_ptr++ << 5;
		}
	}

	//---------------------------------------------
	// unpack the huff arrays of literals & packets :
	
	U8 * scratch_ptr = U8_void(scratch_space);
		
	{
	//SIMPLEPROFILE_SCOPE(get_array_literals);

		newlz_array_get_printf("literals : ");

		// in-place issue :
		//	force_copy_uncompressed is passed as inplace_comp_raw_overlap
		//	so that comp data is always copied out to scratch

		{
			U8 * literals = scratch_ptr; SINTa literals_count;
			SINTa literals_comp_len = newLZ_get_array(&literals,comp_ptr,chunk_comp_end,&literals_count,
										RR_MIN(chunk_len,rrPtrDiff(scratch_end-scratch_ptr)),
										inplace_comp_raw_overlap,
										scratch_ptr,scratch_end);
			if ( literals_comp_len < 0 )
			{
				ooLogError("corruption : bad literals\n");
				return -1;
			}

			comp_ptr += literals_comp_len;
				
			// scratch is advanced whether we used it or not :
			scratch_ptr += literals_count;
			
			arrays->literals_ptr = literals;
			arrays->literals_count = literals_count;
		}
		
		newlz_array_get_printf("\n");
	}

	//rrprintfvar(literals_count);
	//rrprintfvar(literals_comp_len);
	
	newlz_array_get_printf("packets : ");
	
	U8 * packets = scratch_ptr;
	SINTa packets_count = 0;
	{
	//SIMPLEPROFILE_SCOPE(get_array_packets);
	SINTa packets_comp_len = newLZ_get_array(&packets,comp_ptr,chunk_comp_end,&packets_count,
		RR_MIN(chunk_len,rrPtrDiff(scratch_end-scratch_ptr)),
		inplace_comp_raw_overlap,
		scratch_ptr,scratch_end);
	if ( packets_comp_len < 0 )
	{
		ooLogError("corruption : bad packets\n");
		return -1;
	}
	
	comp_ptr += packets_comp_len;
	}
	
	newlz_array_get_printf("\n");
				
	scratch_ptr += packets_count;
	
	// scratch_ptr has so far advanced by literals_count + packets_count
	// sum of packets & literals should be <= chunk_len
	RR_ASSERT_IF_NOT_CORRUPT( scratch_ptr <= U8_void(scratch_space) + chunk_len );
	
	//rrprintfvar(packets_count);
	//rrprintfvar(packets_comp_len);
		
	//g_stat_chunks ++;
		
	// now forbidden	
	// packets can be 0 for an all-literals chunk
	//RR_ASSERT_IF_NOT_CORRUPT( packets_count > 0 ); // @@!!
	// @@!! is there anything wrong with packet count == 0 ?	

	// in-place issue :
	//	force_copy_uncompressed is not needed for offsets or excesses huffs
	//	because we decode them into the offsets/excesses u32 arrays, which live in scratch
		
	//-----------------------------------------------------
	// get huff array of offsets_u8 :
	U8 * offsets_u8 = scratch_ptr;
	U8 * offsets_u8_2 = NULL;
	SINTa offsets_count = 0;
	U32 offset_alt_modulo = 0;
	
	if ( rrPtrDiff(chunk_comp_end-comp_ptr) < 3 )
		return -1;
	
	// alt offset flag in top bit of offset_u8 array :
	if ( *comp_ptr >= 0x80 )
	{
		U8 offset_alt_header_byte = *comp_ptr++;
		offset_alt_modulo = offset_alt_header_byte - 0x80 + 1;
		
		newlz_array_get_printf("offsets alt 1 : ");
		
		SINTa offsets_u8_comp_len = newLZ_get_array(&offsets_u8,comp_ptr,chunk_comp_end,&offsets_count,
			RR_MIN(packets_count,rrPtrDiff(scratch_end-scratch_ptr)),false,
			scratch_ptr,scratch_end);
		if ( offsets_u8_comp_len < 0 )
		{
			ooLogError("corruption : bad offsets alt1\n");
			return -1;
		}
		
		newlz_array_get_printf("\n");
		
		comp_ptr += offsets_u8_comp_len;
		scratch_ptr += offsets_count;
		
		if ( offset_alt_modulo != 1 )
		{
			offsets_u8_2 = scratch_ptr;
			
			newlz_array_get_printf("offsets alt 2 : ");
		
			SINTa offsets_count_2 = 0;
			SINTa offsets_u8_comp_len_2 = newLZ_get_array(&offsets_u8_2,comp_ptr,chunk_comp_end,&offsets_count_2,
				RR_MIN(offsets_count,rrPtrDiff(scratch_end-scratch_ptr)),false,
				scratch_ptr,scratch_end);
			if ( offsets_u8_comp_len_2 < 0 )
			{
				ooLogError("corruption : bad offsets alt2\n");
				return -1;
			}
			
			newlz_array_get_printf("\n");
		
			comp_ptr += offsets_u8_comp_len_2;
			scratch_ptr += offsets_count;
			
			REQUIRE_FUZZ_RETURN( offsets_count == offsets_count_2 , -1);
		}		
	}
	else
	{
		// offsets 44
	
		newlz_array_get_printf("offsets 44 : ");
		
		//SIMPLEPROFILE_SCOPE(get_array_offsetu8);
		SINTa offsets_u8_comp_len = newLZ_get_array(&offsets_u8,comp_ptr,chunk_comp_end,&offsets_count,
			RR_MIN(packets_count,rrPtrDiff(scratch_end-scratch_ptr)),false,
			scratch_ptr,scratch_end);
		if ( offsets_u8_comp_len < 0 )
		{
			ooLogError("corruption : bad offsets44\n");
			return -1;
		}
		
		newlz_array_get_printf("\n");
			
		comp_ptr += offsets_u8_comp_len;
		scratch_ptr += offsets_count;
	}
	

	// get huff array of excesses_u8 :
	U8 * excesses_u8 = scratch_ptr;
	SINTa excesses_count = 0;

	newlz_array_get_printf("excesses u8 : ");
		
	{
	//SIMPLEPROFILE_SCOPE(get_array_excessu8);
	SINTa excesses_u8_comp_len = newLZ_get_array(&excesses_u8,comp_ptr,chunk_comp_end,&excesses_count,
		RR_MIN(chunk_len/4,rrPtrDiff(scratch_end-scratch_ptr)),false,
		scratch_ptr,scratch_end);
	if ( excesses_u8_comp_len < 0 )
	{
		ooLogError("corruption : bad excesses\n");
		return -1;
	}
	
	comp_ptr += excesses_u8_comp_len;
	//rrprintfvar(excesses_u8_comp_len);
	}
	
	newlz_array_get_printf("\n");
	
	scratch_ptr += excesses_count;
	
	//-----------------------------------------------------
	// get offset rawbits & excesses :

	// !!NOTE decode_parse assumes excesses are last (for fuzz safety checking)
	// if you move things around, you need to update scratch_end.

	//rrprintfvar(offsets_count);

	// 16 byte alignment for SIMD :
	U32 * u32_space = (U32 *)rrAlignUpPointer(scratch_ptr,16);
	S32 * offsets = (S32 *)u32_space;
	U32 * excesses = u32_space + offsets_count;
	// 16 byte alignment for SIMD :
	excesses = rrAlignUpPointer(excesses,16);
	
	SINTa scratch_remaining = scratch_end - (U8 *)excesses;
	
	// must be room for excesses and NEWLZ_EXTRA_SCRATCH_MEM_FOR_FUZZ :
	if ( scratch_remaining < excesses_count*sizeofA(U32) + NEWLZ_EXTRA_SCRATCH_MEM_FOR_FUZZ )
	{
		ooLogError("newlz scratch overflow\n");
		return -1;
	}
		
	U8 * scratch_used_ptr = (U8 *)(excesses + excesses_count);
	
	/*
	// compare decoder_scratch_space_used to decoder_scratch_space_needed estimated by the encoder
	//	decoder_scratch_space_used must be <= decoder_scratch_space_needed
	{
		SINTa decoder_scratch_space_used = scratch_used_ptr - scratch_space;
		rrprintfvar(decoder_scratch_space_used);
	}
	*/
	
	// zero the excesses overrun area so we can fetch from it safely :
	memset(scratch_used_ptr,0,NEWLZ_EXTRA_SCRATCH_MEM_FOR_FUZZ);


	if ( newLZ_get_offsets_excesses(comp_ptr,chunk_comp_end,
		offsets_u8,offsets_u8_2,offsets_count,offset_alt_modulo,
		excesses_u8,excesses_count,
		offsets,excesses,
		chunk_pos+chunk_len,
		excess_hdr_byte,excess_stream_size) < 0 )
	{
		// get_offsets_excesses already calls ooLogError on corruption
		rrPrintf_v2("newlz newLZ_get_offsets_excesses error\n");
		return -1; // error	
	}
		
	arrays->offsets = offsets;
	arrays->offsets_count = offsets_count;
	arrays->excesses = excesses;
	arrays->excesses_count = excesses_count;
	arrays->packets = packets;
	arrays->packets_count = packets_count;
	
	return 1;			
}

static RADINLINE SINTa newLZ_decode_chunk_phase2(
	int chunk_type,
	U8 * chunk_ptr,SINTa chunk_len, SINTa chunk_pos, 
	const newLZ_chunk_arrays * arrays)
{
	//SIMPLEPROFILE_SCOPE_N(newLZ_dec_phase2,chunk_len);

	#ifdef SPEEDFITTING
	U64 t1 = speedfitter_ticks_start();
	#endif

	if ( chunk_type > 1 ) // already checked in phase1
		return -1;

	int start_pos = 0;
	// on first chunk, start at NEWLZ_MIN_OFFSET
	if ( chunk_pos == 0 ) start_pos = NEWLZ_MIN_OFFSET;

	U8 * to_ptr = chunk_ptr + start_pos;
	U8 * chunk_end = chunk_ptr + chunk_len;	

#ifdef DO_NEWLZ_DUMP
	static const int dump_pos = 0*1024;
	if ( chunk_pos == dump_pos )
	{
		if ( FILE *f = fopen("dump.dat", "wb") )
		{
			U32 counts[4];

			// Cover initial seed data
			int initial_histo_size = (dump_pos == 0) ? 8 : dump_pos;

			// history
			fwrite(chunk_ptr-chunk_pos, 1, initial_histo_size, f);

			// counts
			counts[0] = (U32) arrays->offsets_count;
			counts[1] = (U32) arrays->excesses_count;
			counts[2] = (U32) arrays->packets_count;
			counts[3] = (U32) arrays->literals_count;
			fwrite(counts, 1, sizeof(counts), f);

			rrprintf("noffs=%u nexc=%u npkt=%u nlit=%u cktype=%d\n", counts[0], counts[1], counts[2], counts[3], chunk_type);

			// the data itself
			fwrite(arrays->offsets, sizeof(S32), counts[0], f);
			fwrite(arrays->excesses, sizeof(U32), counts[1], f);
			fwrite(arrays->packets, 1, counts[2], f);
			fwrite(arrays->literals_ptr, 1, counts[3], f);

			fclose(f);
		}
	}
#endif

	//=====================================================================
	// now parse !

	#if defined(DO_SSE4_PARSE_ALWAYS)
	const newLZ_decode_parse_func_set * parse = &newLZ_decode_parse_funcs_sse4;
	#elif defined(DO_SSE4_PARSE_TEST)
	const newLZ_decode_parse_func_set * parse = rrsimd_has_sse4() ? &newLZ_decode_parse_funcs_sse4 : &newLZ_decode_parse_funcs_regular;
	#else
	const newLZ_decode_parse_func_set * parse = &newLZ_decode_parse_funcs_regular;
	#endif

	if ( chunk_type == NEWLZ_LITERALS_TYPE_RAW )
	{
		bool res;

		res = parse->raw(
			arrays,
			to_ptr,chunk_ptr,chunk_end,chunk_ptr-chunk_pos);

		if (!res)
		{
			ooLogError("corruption : inconsistency detected during phase 2\n");
			return -1;
		}
	}
	else if ( chunk_type == NEWLZ_LITERALS_TYPE_SUB )
	{
		bool res;

		res = parse->sub(
			arrays,
			to_ptr,chunk_ptr,chunk_end,chunk_ptr-chunk_pos);

		if (!res)
		{
			ooLogError("corruption : inconsistency detected during phase 2\n");
			return -1;
		}
	}
	else
	{
		RR_ASSERT_FAILURE_ALWAYS("bad chunk_type");
	}

	#ifdef SPEEDFITTING
	
	if ( g_speedfitter_stage == 3 )
	{	
		U64 t2 = speedfitter_ticks_end();
		
		/*
		// gather stats:
		
		int offset_blend_score = 0;
		
		for LOOP(offi,arrays->offsets_count)
		{
			S32 neg_off = arrays->offsets[offi];
			RR_ASSERT( neg_off <= -8 );
			offset_blend_score += 1 + newLZ_offset_blend_score(-neg_off);
		}
		*/

		speedfitter_stage3_collect_parse_newlz(chunk_ptr,(t2-t1),chunk_type,
			chunk_len,
			arrays->literals_count,
			arrays->packets_count,
			arrays->excesses_count);
			//offset_blend_score);
	}

	#endif
	
	return 1;
}


/**

Thread phase1 : all independent per-block work
Kraken entropy decode
also uncompressed blocks & huff-only blocks

Thread phase2 : all dependent-on-previous-blocks work
also depends on the block's phase1, passed through the scratch mem
does the Kraken final parse

Unthreaded just does phase1+phase2

**/		
S32 Kraken_DecodeOneQuantum(U8 * decomp,U8 * decomp_end,const U8 * comp,S32 quantumCompLen,const U8 * compBufEnd,SINTa pos_since_reset,
	void * scratch,SINTa scratch_size,OodleLZ_Decode_ThreadPhase threadPhase)
{
	SINTa decomp_len = rrPtrDiff( decomp_end - decomp );
	RR_ASSERT( decomp_len > 0 && decomp_len <= OODLELZ_BLOCK_LEN );
	decomp_len; // unused

	rrPrintf_v2("DBLOCK : %d : %d : %d\n",pos_since_reset,decomp_len,quantumCompLen);

	//SIMPLEPROFILE_SCOPE_N(newLZ_decode,decomp_len);
	
	U8 * scratch_ptr = U8_void(scratch);
	U8 * scratch_end = scratch_ptr + scratch_size;
	
	U8 * rawPtr = decomp;
	U8 * rawEnd = decomp_end;
	const U8 * compPtr = U8_void(comp);
	const U8 * compEnd = compPtr + quantumCompLen;
	RR_ASSERT( compEnd <= compBufEnd );
	//const U8 * checkPtr = U8_void(checkbuf);

	// LZQH no flags set -> block len = 128k
	const int newlz_chunk_len = NEWLZ_CHUNK_LEN;
	
	while(rawPtr<rawEnd)
	{
		SINTa chunk_len = RR_MIN( newlz_chunk_len , (rawEnd - rawPtr) );
		SINTa chunk_pos = rrPtrDiff( rawPtr - U8_void(decomp) ) + pos_since_reset;
		
		RR_ASSERT( chunk_len >= 1 && chunk_len <= NEWLZ_CHUNK_LEN );
		
		// minimum length of a chunk is 4 bytes
		//	3 byte comp len header + 1 byte payload
		if ( 4 > rrPtrDiff( compEnd - compPtr ) )
			return -1;
		
		SINTa chunk_comp_len = RR_GET24_BE_OVERRUNOK(compPtr);
		
		if ( chunk_comp_len >= (1<<23) )
		{
			int chunk_type = (int)( (chunk_comp_len>>19)&0xF );
		
			chunk_comp_len &= (1<<19)-1;
			//RR_ASSERT_ALWAYS( chunk_comp_len <= chunk_len );
			
			compPtr += 3;

			rrPrintf_v2("CHUNK : %d : %d\n",chunk_len,chunk_comp_len);

			if ( chunk_comp_len > compEnd - compPtr )
				return -1;

			const U8 * chunk_comp_end = compPtr + chunk_comp_len;
		
			if ( chunk_comp_len >= chunk_len )
			{			
				//raw 
				if ( chunk_comp_len > chunk_len ) return -1;
				if ( chunk_type != 0 ) return -1;
				
				if ( threadPhase & OodleLZ_Decode_ThreadPhase1 )
				{
					// has to be memmove for in-place decoding
					//memcpy(rawPtr,compPtr,chunk_len);
					memmove(rawPtr,compPtr,chunk_len);
				}
			}
			else
			{
				RR_ASSERT_IF_NOT_CORRUPT( chunk_len >= NEWLZ_MIN_CHUNK_LEN );

				// Encoder will only ever produce Kraken chunks above this
				// min size, don't accept anything less
				REQUIRE_FUZZ_RETURN( chunk_len >= NEWLZ_MIN_CHUNK_LEN , -1 );
			
				// check scratch size meets decoder needs
				
				SINTa scratch_expected = OodleLZ_Compressor_ScratchMemSize(OodleLZ_Compressor_Kraken,chunk_len);
						
				if ( rrPtrDiff(scratch_end - scratch_ptr) < scratch_expected )
				{
					ooLogError("decoder scratch too small\n");
					return -1;
				}		
						
				U8 * chunk_scratch_end = scratch_ptr + scratch_expected;
				
				newLZ_chunk_arrays * arrays = (newLZ_chunk_arrays *) scratch_ptr;
				scratch_ptr += sizeof(newLZ_chunk_arrays);
						
				/**
				
				Kraken in-place decoding :
				
				force phase1 to put all its arrays in scratch
					(uncompressed arrays can't just point at comp)
				
				phase2 always fills raw buff from scratch
					Kraken does not read comp during phase2 at all
				
				**/

				if ( threadPhase & OodleLZ_Decode_ThreadPhase1 )
				{
					RR_ZERO(*arrays);
				
					if ( newLZ_decode_chunk_phase1(chunk_type,compPtr,chunk_comp_end,rawPtr,chunk_len,chunk_pos,scratch_ptr,chunk_scratch_end,arrays) < 0)
					{
						// wipe out the newLZ_chunk_arrays in scratch :
						arrays->chunk_ptr = NULL;
						return -1;
					}
				}
							
				if ( threadPhase & OodleLZ_Decode_ThreadPhase2 )
				{
					RR_ASSERT( arrays->chunk_ptr == rawPtr );
					if ( arrays->chunk_ptr != rawPtr )
					{
						RR_ASSERT( arrays->chunk_ptr == NULL );
						// means Phase1 failed
						return -1;
					}
					RR_ASSERT( arrays->scratch_ptr == scratch_ptr );
									
					if ( newLZ_decode_chunk_phase2(chunk_type,rawPtr,chunk_len,chunk_pos,arrays) < 0)
					{
						return -1;
					}
				}
					
				if ( threadPhase != OodleLZ_Decode_ThreadPhaseAll )
				{
					// if threaded, advance scratch
					scratch_ptr = chunk_scratch_end;
				}
				else
				{
					// undo scratch advance for arrays
					scratch_ptr = U8_void(scratch);
				}
			}
		}
		else
		{
			SINTa literals_len;
			
			// huff-only chunk
			// uncompressed chunks do NOT go through here	
						
			if ( threadPhase & OodleLZ_Decode_ThreadPhase1 )
			{									
				// in-place issue : on huff-only chunks, can't decode directly into target if raw,comp overlap
				// check for in-place decode comp-raw overlap :
				bool inplace_comp_raw_overlap;
				//inplace_comp_raw_overlap = RR_MIN(comp_end,chunk_ptr+chunk_len) >= RR_MAX(comp,chunk_ptr);
				inplace_comp_raw_overlap = compPtr <= (rawPtr+chunk_len) && rawPtr <= compEnd;
	
				newlz_array_get_printf("whole huff chunk: ");
	
				if ( inplace_comp_raw_overlap )
				{
					// decode into scratch and then copy
					U8 * literals = scratch_ptr;
					chunk_comp_len = newLZ_get_array(&literals,compPtr,compEnd,&literals_len,chunk_len,false,
										scratch_ptr,scratch_end);	
					if ( literals != scratch_ptr ) // got uncompressed data
						return -1;
					memcpy(rawPtr,scratch_ptr,chunk_len);
				}
				else
				{	
					U8 * literals = rawPtr;
					chunk_comp_len = newLZ_get_array(&literals,compPtr,compEnd,&literals_len,chunk_len,false,
										scratch_ptr,scratch_end);	
					if ( literals != rawPtr ) // got uncompressed data
						return -1;
				}		
				
				newlz_array_get_printf("\n");		
			}
			else
			{
				chunk_comp_len = newLZ_get_arraylens(compPtr,compEnd,&literals_len,chunk_len);
			}
			
			if ( chunk_comp_len < 0 || chunk_comp_len >= chunk_len )
				return -1;
			
			if ( literals_len != chunk_len )
				return -1;
					
			RR_ASSERT( chunk_comp_len < chunk_len );
				
			rrPrintf_v2("HCHUNK : %d : %d\n",chunk_len,chunk_comp_len);
		}
		
		compPtr += chunk_comp_len;
		rawPtr += chunk_len;
	}
	
	// return comp len :
	
	return rrPtrDiff32( compPtr - comp );
}

// Headerless decode for customers who don't use Oodle container
SINTa Kraken_Decode_Headerless(U8 * decomp, SINTa decomp_size, const U8 * comp, SINTa comp_size,
	UINTa bytes_since_reset, rrbool isMemcpy, rrbool lzEnable, rrbool subLiteralEnable,
	void * decoderMemory, SINTa decoderMemorySize)
{
	// public entry point OodleKraken_Decode_Headerless

	if ( decomp_size > NEWLZ_CHUNK_LEN )
	{
		ooLogError("Kraken_Decode_Headerless: decomp_size too large, must be <=128KiB.\n");
		return 0;
	}

	if ( isMemcpy )
	{
		if ( comp_size != decomp_size )
		{
			ooLogError("Kraken_Decode_Headerless: memcpy set but decomp_size != comp_size.\n");
			return 0;
		}

		memmove(decomp, comp, decomp_size);
		return decomp_size;
	}

	const U8 * comp_end = comp + comp_size;
	U8 * scratch_ptr = (U8 *)decoderMemory;
	U8 * scratch_end = scratch_ptr + decoderMemorySize;

	if ( !lzEnable ) // whole-huff chunk
	{
		U8 * literals = scratch_ptr;
		SINTa literals_len;

		SINTa chunk_comp_len = newLZ_get_array(&literals, comp,comp_end, &literals_len,decomp_size, false,
			scratch_ptr, scratch_end);

		if ( chunk_comp_len < 0 )
		{
			ooLogError("Kraken_Decoder_Headerless: !lzEnable literal stream decode failed.\n");
			return 0;
		}

		// we allow uncompressed streams here (unlike regular Oodle.)
		memcpy(decomp, literals, literals_len);

		if ( chunk_comp_len > comp_size )
		{
			ooLogError("Kraken_Decode_Headerless: !lz internal error - chunk_comp_len > comp_size.\n");
			return 0;
		}
		else if ( chunk_comp_len < comp_size )
		{
			ooLogError("Kraken_Decode_Headerless: !lz WARNING - given comp_size=%lld but literal stream only consumed %lld bytes.\n",
				(long long)comp_size, (long long)literals_len);
		}

		if ( literals_len != decomp_size )
		{
			ooLogError("Kraken_Decode_Headerless: !lz error - specified decomp_size was %lld but literal stream len was %lld.\n",
				(long long)decomp_size, (long long)literals_len);
			return 0;
		}

		return decomp_size;
	}

	// If we get here, it's a regular LZ stream (LZ_raw or LZ_sub)
	newLZ_chunk_arrays * arrays = (newLZ_chunk_arrays *) scratch_ptr;
	scratch_ptr += sizeof(newLZ_chunk_arrays);

	if ( scratch_ptr >= scratch_end )
	{
		ooLogError("Kraken_Decode_Headerless: lz internal error - scratch?!?\n");
		return 0;
	}

	RR_ZERO(*arrays);
	int chunk_type = subLiteralEnable ? NEWLZ_LITERALS_TYPE_SUB : NEWLZ_LITERALS_TYPE_RAW;
	if ( newLZ_decode_chunk_phase1( chunk_type, comp, comp_end, decomp, decomp_size, bytes_since_reset,
		scratch_ptr, scratch_end, arrays) < 0 )
	{
		ooLogError("Kraken_Decode_Headerless: phase1 (FE) decode failed!\n");
		return 0;
	}

	if ( newLZ_decode_chunk_phase2( chunk_type, decomp, decomp_size, bytes_since_reset, arrays) < 0 )
	{
		ooLogError("Kraken_Decode_Headerless: phase2 (BE) decode failed!\n");
		return 0;
	}

	return decomp_size;
}

/***=================================================

OPTIMAL PARSE

****/

#define NEWLZ_MATCH_NUM_PAIRS	4

#define COST_PENALTY_SCALE	6
		
// penalties to bias parse towards faster decodes
//  @@ -> not really tweaked, not finding a great win here
#define COST_PENALTY_PACKET			(3*COST_PENALTY_SCALE)  // penalty per packet; fewer packets = less mode switching
#define COST_PENALTY_EXCESS			(2*COST_PENALTY_SCALE)	// excesses take time
#define COST_PENALTY_NORMAL_MATCH	(6*COST_PENALTY_SCALE)  // normal matches are slow (offset decoding)

//#define COST_PENALTY_NORMAL_CACHE	(COST_PENALTY_SCALE)	// penalty for offsets that go out of cache
//#define COST_PENALTY_NORMAL_CACHE	(COST_PENALTY_SCALE*2)
//#define COST_PENALTY_NORMAL_CACHE	(COST_PENALTY_SCALE*6)

struct newLZ_MatchParseRecord
{
	UnpackedMatchPair	pairs[NEWLZ_MATCH_NUM_PAIRS];	
};

/***

newLZ_get_match_heuristic(newLZ_MatchParseRecord) -
code dupe from newLZ_get_match_heuristic(CTMF)

simple heuristic greedy parse to seed the optimal parse

-> NOTE : the heuristics here are what establishes our statistics baseline
->   this strongly guides the optimal parse

***/
static match newLZ_get_match_heuristic(const newLZ_MatchParseRecord & matches, const newLZ_LOs & lastoffsets, const U8 * ptr, const U8 * ptr_matchend, int mml, int lrl,
const U8 * dictionaryBase, U32 dictionarySize,const OodleLZ_CompressOptions *pOptions)
{
	U32 ptr32 = RR_GET32_NATIVE_UNALIGNED(ptr);
	
	// check LOs
	// only take higher LO index if the ML is greater
	int lo_index = -1;
	S32 lo_ml=0;
	RR_UNROLL_I_N(NEWLZ_NUM_LAST_OFFSETS,0, \
	{ \
		U32 offset = lastoffsets.lasts[i]; \
		int ml; \
		if ( havematch_mml2_one32(ptr32,ptr,ptr-offset,ptr_matchend,&ml) ) { \
			if ( i == 0 || ml > lo_ml ) { \
				lo_ml = ml; \
				lo_index = i; \
			} \
		} \
	} );
	
	//U32 cur_absolute_pos = (U32)rrPtrDiff(ptr - ctmf->m_base_ptr);
	
	// pretty aggressively just take LO if it's non-trivial
	//	don't even look for non-LO match
	//#define NEWLZ_LOML_GOOD_ENOUGH	4	// or 5 ?
	//  NOTE : this strongly biases the optimal parse statistics!
	if ( lo_ml >= NEWLZ_LOML_GOOD_ENOUGH )
	{
		// just take the LO match with no normal search :
		match m = { lo_ml, -lo_index };
		
		return m;
	}
		
	#ifdef NEWLZ_LRL_FOR_LONGER_MML
	if ( lrl >= NEWLZ_LRL_FOR_LONGER_MML )
	{
		if ( lo_ml <= 2 ) lo_ml = 0;
	
		// normal mml up one :	
		mml ++;
	}
	#endif
	
	S32 bestml = 0;
	U32 bestoff = 0;
	
	for(int m=0;m<NEWLZ_MATCH_NUM_PAIRS;m++)
	{
		S32 len = matches.pairs[m].length;
		
		// matches are in descending length
		// so when we hit a short one, get out
		if ( len < mml )
			break;
		
		if ( len > ptr_matchend - ptr )
		{
			len = rrPtrDiff32(ptr_matchend - ptr);
			if ( len < mml )
				break;
		}
		
		U32 offset = matches.pairs[m].offset;
		
		// verify match :
		RR_ASSERT( memcmp(ptr,ptr-(SINTa)offset,len) == 0 );
		
		if ( offset >= dictionarySize )
			continue;
			
		// force all offsets >= 8
		if ( offset < NEWLZ_MIN_OFFSET )
		{
			offset = newLZ_offset44_round_up_tiny(offset);
			if ( (SINTa)offset > rrPtrDiff(ptr - dictionaryBase) )
				continue;

			const U8 * vs_ptr = ptr - offset;
			
			len = getmatchlen_mml3_one32(ptr32,ptr,vs_ptr,ptr_matchend);
						
			if ( len < mml )
				continue;
		}
		
		// to get rep0 exclusion I need matchlens to be maximal :
		RR_ASSERT( len >= ptr_matchend - ptr || ptr[len] != ptr[len-(SINTa)offset] );
		
		if ( newLZ_IsAllowedNormalMatch(len,offset,pOptions) && newLZ_IsNormalMatchBetter(len,offset,bestml,bestoff) )
		{
			bestml = len;
			bestoff = offset;
		}		
	}
	
	if ( newLZ_IsLOMatchBetter(lo_ml,bestml,bestoff) )
	{
		// take the LO match :
		match m = { lo_ml, -lo_index };
		return m;
	}
	else
	{
		// normal match :
		match m = { bestml, (S32)(bestoff) };
		return m;
	}
}

struct newlz_optimal_arrival_tll
{
	// NOTE: cost now in newlz_optimal_arrivals_tll
	newLZ_LOs_NoPad los; // this is the los *after* my match, eg. the set for going forward when you start here
		// los[0] is the offset used in the packet for this arrival
	S32 ml,lrl; // prev pos is at -ml-lrl
		
	union
	{
	U32 is_twostep;
	struct {
		U32 lrl : 8;
		U32 ml : 24;
	} twostep;
	};
	
	S32 prev_index; // how to find your previous arrival (this is a tll index)	
		// prev_index could also be used for multi-parse
	
	
	#ifdef RR_DO_ASSERTS
	S32 check_offset;
		// this is the coding offset, equal to how parse.offset is store
		// check_offset <= 0 for LO ; if it's an lo index it's not relative to my los, it's in the *previous* los
		// offset is redundant, it's always equal to los[0]

	S32 check_arrival_pos; // @@ should be removable, just for asserting, can be computed
	// check_arrival_pos is the arrival pos of the full packet that made this TLL arrival
	//	eg. if tll == 0 , then check_arrival_pos = this pos
	//	but at higher tll , check_arrival_pos is behind by tll
	//	and at tll == last , it's behind by a variable amount (in the last_tll_len array)
	#endif

	// Initialize a last-offset match arrival
	RADFORCEINLINE void set_lo(int in_prev_index,
		S32 in_pos,S32 in_lrl,S32 in_ml,S32 in_loi,
		const newLZ_LOs_NoPad & in_los)
	{
		ml = in_ml;
		lrl = in_lrl;
		los.SetMTF(in_los,in_loi);
		is_twostep = 0;
		prev_index = in_prev_index;
		RR_DURING_ASSERT( check_offset = -in_loi );
		RR_DURING_ASSERT( check_arrival_pos = in_pos+in_ml );
	}

	// Initialize to a regular match arrival
	RADFORCEINLINE void set_match(int in_prev_index,
		S32 in_pos,S32 in_lrl,S32 in_ml,S32 in_offset,
		const newLZ_LOs_NoPad & in_los)
	{
		ml = in_ml;
		lrl = in_lrl;
		los.SetAdd(in_los,in_offset);
		is_twostep = 0;
		prev_index = in_prev_index;
		RR_DURING_ASSERT( check_offset = in_offset );
		RR_DURING_ASSERT( check_arrival_pos = in_pos+in_ml );
	}
};

class newlz_optimal_arrivals_tll
{
	S32 * costs; // cost[i]
	newlz_optimal_arrival_tll * payloads; // payload[i]
	UINTa count;

public:
	static SINTa size(SINTa count)
	{
		return count * (sizeof(*costs) + sizeof(*payloads));
	}

	// storage should point to 32-bit aligned memory with size(init_count) bytes
	void init(void * storage, SINTa init_count)
	{
		RR_ASSERT(init_count >= 0);

		costs = (S32*)storage;
		payloads = (newlz_optimal_arrival_tll *)(costs + init_count);
		count = init_count;
	}

	S32& cost(SINTa index)
	{
		RR_ASSERT((UINTa)index < count);
		return costs[index];
	}

	newlz_optimal_arrival_tll& operator[](SINTa index)
	{
		RR_ASSERT((UINTa)index < count);
		return payloads[index];
	}
};


enum {
	NEWLZ_SHORTCOST_COUNT = 32,
	NEWLZ_SHORT_ENDML = NEWLZ_LOMML + NEWLZ_SHORTCOST_COUNT,
};

struct newlz_codecosts
{
	int chunktype;
	U32 subliteralmask;
	S32 codecost_literal[256];
	S32 codecost_packet[256];
	
	int offset_alt_modulo;
	S32 codecost_offset1[256];
	S32 codecost_offset2[256];
	S32 codecost_excess[256];

	// pre-transposed costs for short(-ish) matches
	S32 cost_packet_short[4][4][NEWLZ_SHORTCOST_COUNT]; // [packet_lrl][loi][ml - NEWLZ_LOMML]
};

static RADINLINE S32 cost_literals(
	const U8 * chunk,SINTa pos,int lrl,SINTa lo,
	const newlz_codecosts & codecosts)
{
	if ( lrl == 0 ) return 0;
	
	RR_ASSERT( lo >= NEWLZ_MIN_OFFSET );
	
	// Kraken
	// uses subliteralmask to support RAW and SUB

	S32 ret = 0;
	while(lrl--)
	{
		int sub_literal = (U8)(chunk[pos] - (chunk[pos - lo] & codecosts.subliteralmask));
		ret += codecosts.codecost_literal[sub_literal];
		pos++;
	}
	return ret;	
}

static RADINLINE S32 cost_add_literal(
	const U8 * chunk,SINTa pos,SINTa lo,
	const newlz_codecosts & codecosts)
{
	RR_ASSERT( lo >= NEWLZ_MIN_OFFSET );
	
	// single literal -> uses subliteralmask to support RAW and SUB

	int sub_literal = (U8)(chunk[pos] - (chunk[pos - lo] & codecosts.subliteralmask));
	return codecosts.codecost_literal[sub_literal];
}

// very cold path, force into separate fn so it's not inlined everywhere
static RADNOINLINE S32 cost_excess_huge(int excess,
	const newlz_codecosts & codecosts)
{
	S32 ret = codecosts.codecost_excess[255];
	excess -= 255;
	int bits = rrVarBits_CountBits_ExpGolomb(excess,EXCESS_EXPGOLOMB_SHIFT);
	ret += COST_ONE_BIT * bits;
	//ret += COST_PENALTY_EXCESS*2; // extra penalty for excess of excess ?
	return ret;
}

static RADINLINE S32 cost_excess(int excess,
	const newlz_codecosts & codecosts)
{
	if ( excess < 255 )
	{
		return codecosts.codecost_excess[excess];
	}
	else
		return cost_excess_huge(excess,codecosts);
}

static RADINLINE S32 cost_lo_match(int lrl_for_packet,int ml,int loi,
		const newlz_codecosts & codecosts)
{
	RR_ASSERT( loi >= 0 && loi < NEWLZ_NUM_LAST_OFFSETS );
	int ml_excess = ml - (NEWLZ_LOMML + NEWLZ_PACKET_ML_MAX);
	if ( ml_excess < 0 )
	{
		int packet = lrl_for_packet + (ml<<2) - (NEWLZ_LOMML<<2) + (loi<<6);
		return codecosts.codecost_packet[packet];
	}
	else
	{
		int packet = lrl_for_packet + (NEWLZ_PACKET_ML_MAX<<2) + (loi<<6);
		return codecosts.codecost_packet[packet] + cost_excess(ml_excess,codecosts);
	}
}

static RADINLINE S32 cost_offset(int offset,const newlz_codecosts & codecosts)
{
	RR_ASSERT( offset >= NEWLZ_MIN_OFFSET && offset <= NEWLZ_MAX_OFFSET );

	S32 cost = 0;
	
	if ( codecosts.offset_alt_modulo == 0 )
	{
		U8 offset_u8 = newLZ_offset44_pack_u8(offset);
		U32 num_raw_bits = newLZ_offset44_unpack_numrawbits(offset_u8);
		
		cost += codecosts.codecost_offset1[offset_u8];
		cost += num_raw_bits * COST_ONE_BIT;
		
		if ( offset >= ESCAPE_OFFSET_MIN )
		{
			// cost speed penalty
			cost += COST_PENALTY_EXCESS;
		}
	}
	else if ( codecosts.offset_alt_modulo == 1 )
	{
		U8 offset_u8 = newLZ_alt_offset_pack_u8_modulo1(offset);
		U32 num_raw_bits = newLZ_alt_offset_unpack_numrawbits(offset_u8);
		
		cost += codecosts.codecost_offset1[offset_u8];
		cost += num_raw_bits * COST_ONE_BIT;
	}
	else
	{
		U8 top,bot;
		newLZ_alt_offset_pack_u8s(offset,&top,&bot,codecosts.offset_alt_modulo);
		U32 num_raw_bits = newLZ_alt_offset_unpack_numrawbits(top);
		
		cost += codecosts.codecost_offset1[top];
		cost += codecosts.codecost_offset2[bot];
		cost += num_raw_bits * COST_ONE_BIT;
	}	
	
	
	//===================================================================
	// offset cache miss penalties for space-speed
	//	-> so far as I can tell this does absolutely nothing measurable
	//		I tried COST_PENALTY_NORMAL_CACHE as large as (6 * COST_PENALTY_SCALE)
	
	#ifdef COST_PENALTY_NORMAL_CACHE
	cost += newLZ_offset_blend_score(offset) * COST_PENALTY_NORMAL_CACHE;
	#endif
	
	//===================================================================
	
	return cost;
}

// cost_offset not included
static RADINLINE S32 cost_normal_match(int lrl_for_packet,int ml,
	const newlz_codecosts & codecosts)
{
	// offset & lrl cost will be added on

	int ml_excess = ml - (NEWLZ_LOMML + NEWLZ_PACKET_ML_MAX);
	if ( ml_excess < 0 )
	{
		int packet = lrl_for_packet + (ml<<2) - (NEWLZ_LOMML<<2) + (3<<6);
		return codecosts.codecost_packet[packet];
	}
	else
	{
		int packet = lrl_for_packet + (NEWLZ_PACKET_ML_MAX<<2) + (3<<6);
		return codecosts.codecost_packet[packet] + cost_excess(ml_excess,codecosts);
	}
}

// these are only used in asserts :
#define tll_index_arrival_pos(index,num_tlls)	( ( (index)/(num_tlls) ) - ( (index)%(num_tlls) ) )
#define tll_index_is_last(index,num_tlls)	( ( (index)%(num_tlls) ) == (num_tlls)-1 )
#define tll_check_index_pos(pos,index,num_tlls)	( ((pos) == tll_index_arrival_pos(index,num_tlls)) || tll_index_is_last(index,num_tlls) )

// filling the arrival at [pos+ml] , from [pos-lrl]
static RADFORCEINLINE S32 try_lo_arrival_tll(
	newlz_optimal_arrivals_tll & tll_arrivals,int num_tlls,
	int prev_index, const U8 * chunk_ptr,
	int * last_tll_len,
	
	S32 pos,S32 base_cost,S32 lrl,S32 lrl_for_packet,S32 ml,S32 loi,
	const newLZ_LOs_NoPad & los,
	const newlz_codecosts & codecosts)
{
	// cost it
	S32 lo_cost = base_cost + cost_lo_match(lrl_for_packet,ml,loi,codecosts);				
	
	if ( num_tlls == 1 )
	{
		S32 & arrival_cost = tll_arrivals.cost(pos + ml);
	
		if ( lo_cost < arrival_cost )
		{
			arrival_cost = lo_cost;
			tll_arrivals[pos + ml].set_lo(prev_index, pos,lrl,ml,loi,los);
		}
	}
	else
	{		
		RR_ASSERT( tll_check_index_pos(pos-lrl,prev_index,num_tlls) );
		
		// do t=0 (packet arrival)
		{
			S32 & arrival_cost = tll_arrivals.cost((pos + ml)*num_tlls);
		
			if ( lo_cost < arrival_cost )
			{
				arrival_cost = lo_cost;
				tll_arrivals[(pos + ml)*num_tlls].set_lo(prev_index, pos,lrl,ml,loi,los);
			}
		}
		
		S32 offset = los.lasts[loi]; // silly way to get this
		
		// try all tailing counts :
		int t = 1;
		S32 cost = lo_cost;
		do
		{
			cost += cost_add_literal(chunk_ptr,pos+ml+t-1,offset,codecosts);
			
			SINTa arrival_pos = pos + ml + t;
			SINTa arrival_index = arrival_pos*num_tlls + t;
			S32 & arrival_cost = tll_arrivals.cost(arrival_index);
			if ( cost < arrival_cost )
			{
				arrival_cost = cost;
				tll_arrivals[arrival_index].set_lo(prev_index, pos,lrl,ml,loi,los);
				
				if ( t == num_tlls-1 )
				{
					// fill last_tll_len !!
					last_tll_len[arrival_pos] = t;
				}
			}
		}
		while( ++t < num_tlls );
	}
	
	return lo_cost;
}

// filling the arrival at [pos+ml] , from [pos-lrl]
static RADFORCEINLINE S32 try_match_arrival_tll(
	newlz_optimal_arrivals_tll & tll_arrivals,int num_tlls,
	int prev_index, const U8 * chunk_ptr,
	int * last_tll_len,
	
	S32 pos,S32 base_cost,S32 lrl,S32 lrl_for_packet,S32 ml,S32 offset,
	const newLZ_LOs_NoPad & los,
	const newlz_codecosts & codecosts)
{			
	S32 match_cost = base_cost + cost_normal_match(lrl_for_packet,ml,codecosts);
	
	if ( num_tlls == 1 )
	{
		S32 & arrival_cost = tll_arrivals.cost(pos + ml);
	
		if ( match_cost < arrival_cost )
		{
			arrival_cost = match_cost;
			tll_arrivals[pos + ml].set_match(prev_index, pos,lrl,ml,offset,los);
		}
	}
	else
	{		
		RR_ASSERT( tll_check_index_pos(pos-lrl,prev_index,num_tlls) );
		
		// do t == 0 (true packet arrival)
		{
			S32 & arrival_cost = tll_arrivals.cost((pos+ml)*num_tlls);
			if ( match_cost < arrival_cost )
			{
				arrival_cost = match_cost;
				tll_arrivals[(pos+ml)*num_tlls].set_match(prev_index, pos,lrl,ml,offset,los);
			}
		}
	
		// try all tailing counts :
		int t = 1;
		S32 cost = match_cost;
		do
		{
			cost += cost_add_literal(chunk_ptr,pos+ml+t-1,offset,codecosts);

			S32 & arrival_cost = tll_arrivals.cost((pos+ml+t)*num_tlls + t);
			if ( cost < arrival_cost )
			{
				arrival_cost = cost;
				tll_arrivals[(pos+ml+t)*num_tlls + t].set_match(prev_index, pos,lrl,ml,offset,los);
				
				if ( t == num_tlls-1 )
				{
					// fill last_tll_len !!
					last_tll_len[(pos+ml+t)] = num_tlls-1;
				}
			}
		}
		while( ++t < num_tlls );
	}
	
	return match_cost;
}

// don't define TWO_STEP_MAX_LRL to disable two step
// TWO_STEP_MAX_LRL = 2 means no excess
#define TWO_STEP_MAX_LRL	3

#ifdef TWO_STEP_MAX_LRL

// two-step arrival where first step (first match) ends at pos
static RADFORCEINLINE void try_two_step_arrival_tll(
	newlz_optimal_arrivals_tll & tll_arrivals,int num_tlls,
	int prev_index, const U8 * chunk_ptr, const U8 * ptr_matchend, S32 * pparse_chunk_end,

	S32 pos,S32 first_step_cost,S32 initial_ml,S32 initial_lrl,S32 initial_loi,S32 offset,
	const newLZ_LOs_NoPad & los,
	const newlz_codecosts & codecosts)
{
	// Two-step matches are: original match, then a gap of non-matching chars,
	// then a LO0 match resuming our original match.

	// Try finding match len at TWO_STEP_MAX_LRL, then attempt to extend
	// that match len backwards to lower LRL while we can.
	//
	// For larger TWO_STEP_MAX_LRL (>=4), this misses gap matches that end
	// earlier than 4 bytes after pos+ml! So you don't want to push it too far.
	const U8 * twostep_ptr = chunk_ptr+pos+TWO_STEP_MAX_LRL;
	int twostep_ml = getmatchlen_mml1(twostep_ptr,twostep_ptr-offset,ptr_matchend);
	int twostep_lrl = TWO_STEP_MAX_LRL;

	#if TWO_STEP_MAX_LRL == 2
	int adjust = (twostep_ptr[-1] == twostep_ptr[-offset-1]) ? 1 : 0;
	twostep_lrl -= adjust;
	twostep_ml += adjust;
	#elif TWO_STEP_MAX_LRL == 3
	int adjust = (twostep_ptr[-2] == twostep_ptr[-offset-2]) ? 2 : 1;
	adjust = (twostep_ptr[-1] == twostep_ptr[-offset-1]) ? adjust : 0;
	twostep_lrl -= adjust;
	twostep_ml += adjust;
	#else
	while (twostep_lrl > 1 && twostep_ptr[-1] == twostep_ptr[-offset-1])
	{
		twostep_lrl--;
		twostep_ml++;
		twostep_ptr--;
	}
	#endif

	if ( twostep_ml >= NEWLZ_LOMML )
	{
		// cost the literals in the gap lrl :
		S32 twostep_literals_cost = cost_literals(chunk_ptr,pos,twostep_lrl,offset,codecosts);

		// cost it
		S32 twostep_cost = first_step_cost + twostep_literals_cost;
		int twostep_lrl_forpacket = twostep_lrl;
		#if TWO_STEP_MAX_LRL >= NEWLZ_PACKET_LRL_MAX
		if ( twostep_lrl >= NEWLZ_PACKET_LRL_MAX )
		{
			twostep_lrl_forpacket = NEWLZ_PACKET_LRL_MAX;
			twostep_cost += cost_excess(twostep_lrl-NEWLZ_PACKET_LRL_MAX,codecosts);
		}
		#endif
		twostep_cost += cost_lo_match(twostep_lrl_forpacket,twostep_ml,0,codecosts);

		int twostep_arrival_pos = pos+twostep_lrl+twostep_ml;

		*pparse_chunk_end = RR_MAX(*pparse_chunk_end,twostep_arrival_pos);

		// don't bother :
		//for(int t=0;t<(num_tlls/2);t++)
		{
			S32 & arrival_cost = tll_arrivals.cost((twostep_arrival_pos)*num_tlls);
			if ( twostep_cost < arrival_cost )
			{
				arrival_cost = twostep_cost;

				//newlz_optimal_arrival_tll & arrival = tll_arrivals[(twostep_arrival_pos+t)*num_tlls + t];
				newlz_optimal_arrival_tll & arrival = tll_arrivals[(twostep_arrival_pos)*num_tlls ];

				arrival.ml = initial_ml;
				arrival.lrl = initial_lrl;
				if (initial_loi >= 0)
					arrival.los.SetMTF(los,initial_loi);
				else
					arrival.los.SetAdd(los,offset);
				arrival.twostep.lrl = twostep_lrl;
				arrival.twostep.ml = twostep_ml;
				arrival.prev_index = prev_index;
				RR_DURING_ASSERT( arrival.check_offset = (initial_loi >= 0) ? -initial_loi : offset );
				RR_DURING_ASSERT( arrival.check_arrival_pos = twostep_arrival_pos );
			}

			//twostep_cost += cost_add_literal(chunk_ptr,twostep_arrival_pos+t,offset,codecost_literal,subliteralmask);
		}
	}
}

#endif

/*************

newLZ_encode_chunk_optimal -
a lot of code dupe from newLZ_encode_chunk

first do a greedy parse just like the Normal heuristic parse
using the PMP matches

*************/

static SINTa newLZ_encode_chunk_optimal_greedy(
		// ** OUTPUT :
		F32 * pJ,
		int * pchunktype,
		newlz_passinfo * const pcodelens,
		U8 * comp,U8 * comp_end,
		// ** INPUT :
		int mml,
		const newlz_vtable * vtable,
		const newLZ_MatchParseRecord * matches,
		const U8 * chunk_ptr,int chunk_len,
		SINTa chunk_pos,
		const U8 * dictionaryBase,
		const OodleKrakenChunkDeadlines * deadline,
		// ** SCRATCH :
		newlz_encoder_scratch * scratch,
		U8 * const literal_space,U8 * const literal_space_end)
{
	SIMPLEPROFILE_SCOPE_N(optimal_greedy,chunk_len);
	
	newLZ_encoder_arrays encarrays;
	newLZ_encoder_arrays_point_to_scratch(&encarrays,scratch,chunk_len,chunk_ptr,vtable->bitstream_flags);

	newLZ_LOs lastoffsets;
	lastoffsets.Reset();
	
	int start_pos = 0;
	if ( chunk_pos == 0 ) start_pos = NEWLZ_MIN_OFFSET;
	
	const OodleLZ_CompressOptions *pOptions = vtable->pOptions;
	
	U32 dictionarySize = ( pOptions->dictionarySize > 0 ) ?
		RR_MIN(NEWLZ_MAX_OFFSET,pOptions->dictionarySize)
		: NEWLZ_MAX_OFFSET;
	
	// parse_end_pos is the match *start* pos limit
	int parse_end_pos = chunk_len - NEWLZ_CHUNK_NO_MATCH_ZONE;
	// ptr_matchend is the match *end* limit
	const U8 * ptr_matchend = chunk_ptr + chunk_len - NEWLZ_MATCH_END_PAD;
	
	int literals_start = start_pos;
	
	{
	SIMPLEPROFILE_SCOPE_N(optimal_greedy_parse,chunk_len);
		
	for(int pos=start_pos;pos<parse_end_pos;)
	{
		const U8 * ptr = chunk_ptr + pos;
				
		match chosen = newLZ_get_match_heuristic(matches[pos],lastoffsets,ptr,ptr_matchend,mml,pos-literals_start,dictionaryBase,dictionarySize,pOptions);
			
		if ( chosen.ml == 0 )
		{
			pos++;
			continue;
		}
		
		// lazy parse
		while(pos+1<parse_end_pos)
		{
			// lazy parse
			
			match lazy;
			// what should prefetch step be for lazy parse?
			//	I guess 1 is good because we'll do an insert there when we advance
			lazy = newLZ_get_match_heuristic(matches[pos+1],lastoffsets,ptr+1,ptr_matchend,mml,pos+1-literals_start,dictionaryBase,dictionarySize,pOptions);
			
			if ( newLZ_LazyMatchDelta(lazy.ml,lazy.off,lazy.IsLO(),chosen.ml,chosen.off,chosen.IsLO()) > 0 )
			{
				// insert a literal :
				ptr++;
				pos++;
				// take lazy match :
				chosen = lazy;
			}
			else // if ( t_do_lazy_parse >= 2 ) // lazy2 check
			{
				// lazy2
				if ( pos+2 >= parse_end_pos ) break;
			
				lazy = newLZ_get_match_heuristic(matches[pos+2],lastoffsets,ptr+2,ptr_matchend,mml,pos+2-literals_start,dictionaryBase,dictionarySize,pOptions);
				
				// IsLazy2Better : check newLZ_LazyMatchDelta > 3 :
				if ( newLZ_LazyMatchDelta(lazy.ml,lazy.off,lazy.IsLO(),chosen.ml,chosen.off,chosen.IsLO()) > NEWLZ_LAZYBETTER_LAZY2 )
				{
					// insert a literal :
					ptr+=2;
					pos+=2;
					// take lazy match :
					chosen = lazy;
				}
				else
				{				
					break;
				}
			}
		}
		
		RR_ASSERT( ptr == chunk_ptr + pos );
		
		// have a match
		RR_ASSERT( chosen.ml >= mml || chosen.IsLO() );

		// force a pure LO0-LRL0 exclusion
		// the only time this should happen is at the very start of the file
		// when pos == start_pos , we could match from the initial lastoffsets
		// in that case, just bump index from 0 to 1
		if ( chosen.off == 0 && pos == literals_start )
		{
			RR_ASSERT( lastoffsets.lasts[0] == NEWLZ_MIN_OFFSET );
			RR_ASSERT( lastoffsets.lasts[1] == lastoffsets.lasts[0] );
			if ( lastoffsets.lasts[1] == lastoffsets.lasts[0] )
			{
				chosen.off = -1;
			}
			else
			{
				chosen.off = lastoffsets.lasts[0];
			}
		}
		
		// verify match :
		RR_DURING_ASSERT( S32 offset = chosen.IsLO() ? lastoffsets.lasts[-chosen.off] : chosen.off );
		RR_ASSERT( memcmp(chunk_ptr+pos,chunk_ptr+pos-offset,chosen.ml) == 0 );

		// make sure match backup didn't violate LO0LRL0 exclusion :
		RR_ASSERT( pos > literals_start || chosen.off != 0 );
		
		int lrl = pos - literals_start;
		RR_ASSERT( lrl == pos - literals_start );
				
		newLZ_encoder_arrays_put_packet<0,1>(&encarrays,&lastoffsets,lrl,chosen.ml,chosen.off,chunk_ptr+literals_start);
		
		pos += chosen.ml;
		literals_start = pos;
	}
	
	}
			
	// output!
	newLZ_encoder_arrays_finish_chunk<1>(&encarrays,chunk_ptr+literals_start);

	// Turn off split for greedy-for-optimal output; too slow!
	//	(part of the slowness being that this is done 3 times at MML 3,4,&8)
	
	// @@ -> does this ever hurt final compression ratio?
	// mostly the greedy-for-optimal is just used to seed the optimal parse
	// but it can be used by itself in the greedy revert case
	//   when we do that, we'd like to go back and consider split arrays here

	/*
	
	yes disabling it does hurt a tiny bit :
	
	Kraken, Optimal3, lzt99 : 

	before:
	24,700,820 -> 9,499,686 =  3.077 bpb =  2.600 to 1 

	after:
	24,700,820 -> 9,515,426 =  3.082 bpb =  2.596 to 1 
	
	0.005 bpb is actually big in my world
	
	@@@@ TODO : win this back, without a big encode speed hit
	
	*/

	// greedy-for-optimal
	OodleLZ_CompressionLevel save_level = vtable->level;
	U32 save_entropy_flags = vtable->entropy_flags;
	const_cast<newlz_vtable *>(vtable)->level = OodleLZ_CompressionLevel_Normal;
	const_cast<newlz_vtable *>(vtable)->entropy_flags &= ~ NEWLZ_ARRAY_FLAG_ALLOW_SPLIT;
	// @@ allow normal splits, just not N-indexed ?

	// @@ NOTE : vtable->bitstream flags not changed !
	//		-> that makes optimal-greedy different than normal greedy
	//		in particular ALT OFFSETS ARE *ON*	

	SINTa greedy_complen = newLZ_encoder_arrays_output(pJ,pchunktype,pcodelens,comp,comp_end,vtable,scratch,&encarrays,chunk_pos,deadline);
	
	const_cast<newlz_vtable *>(vtable)->level = save_level;
	const_cast<newlz_vtable *>(vtable)->entropy_flags = save_entropy_flags;
	
	return greedy_complen;
}

/**

optimal_update_codelens :

scan through parsevec (the incremental piece we are adding on)
histogram the literals/packets/offsets/excesses it makes
add them to previous histo
update codecosts

**/
static void optimal_update_passinfo(
		const vector_a<newlz_encoder_parse> & parsevec,
		int parse_start_pos,
		newlz_passinfo & passinfo,
		const U8 * chunk)
{
	SIMPLEPROFILE_SCOPE(optimal_update_passinfo);
			
	// passinfo contains running histos

	// @@!! tweaky
	//enum { optimal_update_codelens_histo_inc = 16 };
	//enum { optimal_update_codelens_histo_inc = 4 };
	enum { optimal_update_codelens_histo_inc = 2 };

	for LOOPVEC(parseveci,parsevec)
	{
		const newlz_encoder_parse & parse = parsevec[parseveci];

		int lrl = parse.lrl;
		int pos = parse_start_pos;
		SINTa lastoffset = parse.lastoffset;

		parse_start_pos += lrl + parse.ml; // update for next now
		
		for(SINTa i=0;i<lrl;i++)
		{
			U8 lit = chunk[pos];

			passinfo.literal_histo_raw[lit] += optimal_update_codelens_histo_inc;		

			U8 ref = chunk[pos - (SINTa)lastoffset];
			U8 sub = (U8)(lit - ref);
			passinfo.literal_histo_sub[sub] += optimal_update_codelens_histo_inc;
			
			pos++;
		}
	
		bool islo = parse.offset <= 0;
		
		int lrl_for_packet = (lrl > NEWLZ_PACKET_LRL_MAX) ? NEWLZ_PACKET_LRL_MAX : lrl;
		int ml_to_send = parse.ml - NEWLZ_LOMML;
		int ml_for_packet = (ml_to_send > NEWLZ_PACKET_ML_MAX) ? NEWLZ_PACKET_ML_MAX : ml_to_send;
	
		int packet = lrl_for_packet + (ml_for_packet<<2);
	
		if ( islo )
		{
			// LO
			int loi = -parse.offset;
			RR_ASSERT( loi >= 0 && loi < NEWLZ_NUM_LAST_OFFSETS );
			packet += loi << 6;			
		}
		else
		{	
			packet += 3 << 6;
						
			S32 off = parse.offset;
			RR_ASSERT( off >= NEWLZ_MIN_OFFSET && off < NEWLZ_MAX_OFFSET );
			
			if ( passinfo.offset_alt_modulo == 0 )
			{			
				U8 offset_u8 = newLZ_offset44_pack_u8(off);

				passinfo.offset_histo1[ offset_u8 ] += optimal_update_codelens_histo_inc;
			}
			else if ( passinfo.offset_alt_modulo == 1 )
			{
				U8 offset_u8 = newLZ_alt_offset_pack_u8_modulo1(off);

				passinfo.offset_histo1[ offset_u8 ] += optimal_update_codelens_histo_inc;
			}
			else
			{
				U8 top,bot;
				newLZ_alt_offset_pack_u8s(off,&top,&bot,passinfo.offset_alt_modulo);

				passinfo.offset_histo1[ top ] += optimal_update_codelens_histo_inc;
				passinfo.offset_histo2[ bot ] += optimal_update_codelens_histo_inc;
			}
		}
	
		passinfo.packet_histo[packet] += optimal_update_codelens_histo_inc;

		if ( lrl_for_packet == NEWLZ_PACKET_LRL_MAX )
		{
			int excess = lrl - NEWLZ_PACKET_LRL_MAX;
			passinfo.excess_histo[ RR_MIN(excess,255) ] += optimal_update_codelens_histo_inc;
		}
		
		if ( ml_for_packet == NEWLZ_PACKET_ML_MAX )
		{
			int excess = ml_to_send - NEWLZ_PACKET_ML_MAX;
			passinfo.excess_histo[ RR_MIN(excess,255) ] += optimal_update_codelens_histo_inc;
		}
	}
}


static void optimal_passinfo_to_codecost(
		const newlz_passinfo & passinfo,
		newlz_codecosts & codecosts,
		int mml)
{
	SIMPLEPROFILE_SCOPE(optimal_passinfo_to_codecost);
	
	// cost_entropy_threshold triggers flattening codecost (important for Mermaid, not Kraken)
	const int cost_entropy_threshold = COST_ONE_BYTE - 1; // just barely under
	
	// must be the same or copying the histos doesn't make any sense :
	RR_ASSERT( passinfo.offset_alt_modulo == codecosts.offset_alt_modulo );
	
	histo_to_codecost(passinfo.offset_histo1,codecosts.codecost_offset1,256,COST_PENALTY_NORMAL_MATCH,cost_entropy_threshold);
	if ( passinfo.offset_alt_modulo > 1 )
		histo_to_codecost(passinfo.offset_histo2,codecosts.codecost_offset2,256,0,cost_entropy_threshold);

	histo_to_codecost(passinfo.packet_histo,codecosts.codecost_packet,256,COST_PENALTY_PACKET,cost_entropy_threshold);
	histo_to_codecost(passinfo.excess_histo,codecosts.codecost_excess,256,COST_PENALTY_EXCESS,cost_entropy_threshold);
	
	if ( codecosts.chunktype == NEWLZ_LITERALS_TYPE_RAW )
	{
		histo_to_codecost(passinfo.literal_histo_raw,codecosts.codecost_literal,256,0,cost_entropy_threshold);
	}
	else
	{
		histo_to_codecost(passinfo.literal_histo_sub,codecosts.codecost_literal,256,0,cost_entropy_threshold);
	}

	for LOOPINT(packet_lrl,4)
	{
		for LOOPINT(loi,4)
		{
			int ml_base = (loi == 3) ? (mml - NEWLZ_LOMML) : 0;

			for LOOPINT(slot,NEWLZ_SHORTCOST_COUNT)
			{
				int rel_ml = slot + ml_base;
				int packet_ml = RR_MIN(rel_ml, NEWLZ_PACKET_ML_MAX);
				int packet = packet_lrl + (packet_ml << 2) + (loi << 6);
				int cost = codecosts.codecost_packet[packet];
				if (rel_ml >= NEWLZ_PACKET_ML_MAX)
					cost += codecosts.codecost_excess[rel_ml - NEWLZ_PACKET_ML_MAX];

				codecosts.cost_packet_short[packet_lrl][loi][slot] = cost;
			}
		}
	}
}

static void optimal_rescale_histo(
		U32	histo[256],
		int add)
{
	RR_UNUSED_VARIABLE(add);

	// @@!! tweaky
	//	skip this completely if optimal_update_codelens_histo_inc is high?
	//	-> no
	//	even when inc is high this seems to help because it flattens out histo
	//	 adds on a constant distribution
	for LOOP(i,256)
	{
		// meh basically a nop :
		//histo[i] = add + (histo[i]>>4);
		histo[i] = 1 + (histo[i]>>4);
		//histo[i] = 1 + (histo[i]>>5);
		//histo[i] = 1 + (histo[i]>>2);
		//histo[i] = (histo[i]>>4);
		//histo[i] = (histo[i]>>2);
	}
}

static void optimal_rescale_histo2(
		U32	histo1[256],
		const U32	histo2[256])
{
	for LOOP(i,256)
	{
		histo1[i] = 1 + ((histo1[i] + histo2[i])>>5);
	}
}

static void optimal_rescale_passinfo(
		newlz_passinfo & passinfo) // <- read/write argument
{
	optimal_rescale_histo(passinfo.literal_histo_raw,2);
	optimal_rescale_histo(passinfo.literal_histo_sub,2);
	optimal_rescale_histo(passinfo.offset_histo1,1);
	if ( passinfo.offset_alt_modulo > 1 )
		optimal_rescale_histo(passinfo.offset_histo2,1);
	optimal_rescale_histo(passinfo.packet_histo,1);
	optimal_rescale_histo(passinfo.excess_histo,2);
}

// passinfo1 = current chunk's greedy
// passinfo2 = last chunk's optimal
static void optimal_rescale_passinfo2(
		newlz_passinfo & passinfo1, // <- read/write argument
		const newlz_passinfo & passinfo2,
		bool literals_same)
{
	// since I track histos of both, you can carry stats even if mode changes
	// meh for compression whether to check literals_same or not
	// if ( 1 )
	if ( literals_same )
	{
		optimal_rescale_histo2(passinfo1.literal_histo_raw,passinfo2.literal_histo_raw);
		optimal_rescale_histo2(passinfo1.literal_histo_sub,passinfo2.literal_histo_sub);
	}
	else
	{
		optimal_rescale_histo(passinfo1.literal_histo_raw,2);
		optimal_rescale_histo(passinfo1.literal_histo_sub,2);
	}
	
	optimal_rescale_histo2(passinfo1.packet_histo,passinfo2.packet_histo);
	optimal_rescale_histo2(passinfo1.excess_histo,passinfo2.excess_histo);
	
	if ( passinfo1.offset_alt_modulo == passinfo2.offset_alt_modulo )
	{
		optimal_rescale_histo2(passinfo1.offset_histo1,passinfo2.offset_histo1);
		if ( passinfo1.offset_alt_modulo > 1 )
			optimal_rescale_histo2(passinfo1.offset_histo2,passinfo2.offset_histo2);
	}
	else
	{
		// offset_alt_modulo changed
		
		// passinfo1 comes from greedy , which doesn't do choose_offset_modulo
		//	so it is always 0 or 1 :
		// -> not true; normal greedy does do choose now
		//RR_ASSERT( passinfo1.offset_alt_modulo == 0 || passinfo1.offset_alt_modulo == 1 );
		
		// -> there's no real reason to take pass1 or pass2?
		//	-> pass1 is fine, could skip this memcpy
		
		//*
		// keep passinfo2 only 
		memcpy(passinfo1.offset_histo1,passinfo2.offset_histo1,sizeof(passinfo2.offset_histo1));
		memcpy(passinfo1.offset_histo2,passinfo2.offset_histo2,sizeof(passinfo2.offset_histo2));
		passinfo1.offset_alt_modulo = passinfo2.offset_alt_modulo;
		/**/
		
		optimal_rescale_histo(passinfo1.offset_histo1,1);
		if ( passinfo1.offset_alt_modulo > 1 )
			optimal_rescale_histo(passinfo1.offset_histo2,1);
	}
}

//===================================

struct NewLZParseCoreGeneric
{
	template<typename Tfunc>
	static RADFORCEINLINE void try_cached_match_truncations(S32 * arrival_costs, S32 base_cost, const S32 * cached_costs, S32 iter_limit, Tfunc use_candidate)
	{
		for (int i = 0; i <= iter_limit; i++)
		{
			const S32 candidate_cost = base_cost + cached_costs[i];
			if ( candidate_cost < arrival_costs[i] )
			{
				arrival_costs[i] = candidate_cost;
				use_candidate(i);
			}
		}
	}
};

#ifdef __RADSSE2__

struct NewLZParseCoreSSE2
{
	template<typename Tfunc>
	static RADFORCEINLINE void try_cached_match_truncations(S32 * arrival_costs, S32 base_cost, const S32 * cached_costs, S32 iter_limit, Tfunc use_candidate)
	{
		__m128i vbase = _mm_set1_epi32(base_cost);
		S32 basei = 0;

		for (;;)
		{
			__m128i vcost0 = _mm_add_epi32(vbase, _mm_loadu_si128((const __m128i *)&cached_costs[basei + 0]));
			__m128i vcost1 = _mm_add_epi32(vbase, _mm_loadu_si128((const __m128i *)&cached_costs[basei + 4]));

			__m128i vsmaller0 = _mm_cmpgt_epi32(_mm_loadu_si128((const __m128i *)&arrival_costs[basei + 0]), vcost0);
			__m128i vsmaller1 = _mm_cmpgt_epi32(_mm_loadu_si128((const __m128i *)&arrival_costs[basei + 4]), vcost1);
			U32 msmaller0 = _mm_movemask_ps(_mm_castsi128_ps(vsmaller0));
			U32 msmaller1 = _mm_movemask_ps(_mm_castsi128_ps(vsmaller1));
			U32 msmaller = msmaller0 + (msmaller1 << 4);

			while (msmaller)
			{
				const int i = basei + rrCtz32(msmaller);
				msmaller = rrClearLowestSetBit32(msmaller);

				// if past actual match len, stop
				if ( i > iter_limit)
					return;

				S32 candidate_cost = base_cost + cached_costs[i];
				RR_ASSERT(candidate_cost < arrival_costs[i]); // what we just established in the SIMD code
				arrival_costs[i] = candidate_cost;
				use_candidate(i);
			}

			basei += 8;
			if (basei > iter_limit)
				break;
		}
	}
};

#endif

/***********************************

TLL parse :

arrivals is a 2d array conceptually
tll_arrivals[pos][tll]

tll = "trailing literal len"

it's a number of literals *after* the current packet
(constrast with lrl = # of literals leading the current packet)

arrival[pos][tll]
is a way to get to pos with tll literals carried
so it's a packet arrival to (pos-tll) + tll literals after

arrival[pos][0] is a true packet arrival

tll becomes the start of the next packet

arrival[pos][num_tlls-1] is special, it can store a tll len that gets high
stored in separate array last_tll_len
this lets cheap LRL stretches get carried forward

----------

parse at pos p :
find matches

consider forming a packet that starts at (p - lrl) and arrives at (p + ml)

the lrl for packet leads are the tlls in arrivals[p][tll]

when you form the arrival at (p+ml) , also do tll's at (p+ml+1) etc.

-----------------------

the whole point of the TLL parse is that

arrival A to pos P might be the cheapest way to get to P
but arrival B + 1 literal to get to pos P+1 might be cheaper than A + lit to P+1

TLL parse is only interesting for sub literals (no advantage for raw)

TLL parse is about 2X slower than normal optimal parse

-------------------------

In some ways the TLL parse is cleaner conceptually.  It doesn't need the "cheap_preceding_arrival" thing at all,
it just propagates that through the [last_tll] slot.  It doesn't do explicit LRL scan-back, it just uses all the
tll arrivals at the current pos.

***********************************/

/**

TLL and non-TLL parse are now both in here

if num_tlls = 1 : non-TLL parse
	"index" of tll_arrivals[] is just "pos"
	cheap_preceding_arrival_pos is used to find long LRL steps
	
if num_tlls > 1 : TLL-parse
	index is (pos*num_tlls+t)
	last_tll_len[] is used

**/
template<int t_num_tlls, typename TParseCore>
static RADNOINLINE void newLZ_encode_chunk_optimal_parse_tll_core(
	const U8 * chunk_ptr, int chunk_len, int start_pos,
	int parse_chunk_len, int parse_chunk_len_max,

	const U8 * dictionaryBase, U32 dictionarySize,
	const newLZ_MatchParseRecord * matches, int mml, int max_search_lrl,

	newlz_optimal_arrivals_tll & tll_arrivals,
	int last_tll, int * last_tll_len,

	const newlz_vtable * vtable,
	newlz_passinfo & passinfo,
	newlz_codecosts & codecosts,
	vector_a<newlz_encoder_parse> & parsevec,
	vector_a<newlz_encoder_parse> & chunk_parsevec,
	const OodleLZ_CompressOptions * pOptions
)
{
	const int num_tlls = t_num_tlls;
	SIMPLEPROFILE_SCOPE_N(optimal_parse_tll,chunk_len);
	
	// In LRL loop, we keep lrl_iterator <= max_search_lrl
	// and want to keep track of match candidates
	constexpr int MAX_MATCH_CANDIDATES = 8 + 1;
	RR_ASSERT(max_search_lrl + 1 <= MAX_MATCH_CANDIDATES);

	// General parse parameters (not tuneables)

	// parse_end_pos is the match *start* pos limit
	int parse_end_pos = chunk_len - NEWLZ_CHUNK_NO_MATCH_ZONE;
	// ptr_matchend is the match *end* limit
	const U8 * ptr_matchend = chunk_ptr + chunk_len - NEWLZ_MATCH_END_PAD;
	
	// Several tunable knobs here for what types of parse moves to consider:
	const int optimal_skip_len = 1 << RR_MIN((int)(vtable->level),8);

	// Match length ranges where we consider length reductions; currently: whenever
	// applicable, as long as match len is below optimal skip len.
	const HalfOpenS32Interval ml_range_for_lomatch_red(NEWLZ_LOMML + 1, optimal_skip_len);
	const HalfOpenS32Interval ml_range_for_match_red(mml + 1, optimal_skip_len);

	// You can set these to zero to disable trying two-step matches of the
	// given type.
	const int twostep_end_pos = parse_end_pos - (TWO_STEP_MAX_LRL + NEWLZ_LOMML);
	const int twostep_lo_end_pos = twostep_end_pos;

	// Rate-limits updating of the parse cost vector. This is a noticeable fraction
	// of encode time on some files.
	const int cost_update_interval = (vtable->level >= OodleLZ_CompressionLevel_Optimal2) ? 1 : 1024;
	
	// parse in chunks :
	int parse_chunk_start = start_pos;
	int last_cost_update_pos = start_pos;
	parsevec.clear();

	for(;;)
	{
		RR_ASSERT( parse_chunk_start < chunk_len );
		if ( parse_chunk_start >= parse_end_pos )
		{
			RR_ASSERT(false); // should not happen
			break;
		}
						
		int parse_chunk_end = parse_chunk_start + parse_chunk_len;
		parse_chunk_end = RR_MIN(parse_chunk_end,parse_end_pos);
		
		int parse_chunk_end_max = parse_chunk_start + parse_chunk_len_max;
		parse_chunk_end_max = RR_MIN(parse_chunk_end_max,parse_end_pos);
		
		// avoid tiny tails :
		if ( parse_chunk_end_max >= parse_end_pos-16 ) parse_chunk_end_max = parse_end_pos;
		if ( parse_chunk_end >= parse_end_pos-16 ) parse_chunk_end = parse_end_pos;
		
		// cheap_preceding_arrival_pos is the last arrival that's <= pos
		//	(there are arrivals filled past that spot)
		// this chunk chain must start from parse_chunk_start
		int cheap_preceding_arrival_pos = parse_chunk_start;
		S32 cheap_preceding_arrival_literal_cost = 0;
		// -> cheap_preceding_arrival_pos is only used in non-TLL mode (num_tlls=1)
		
		// I should have a true packet arrival at parse_chunk_start :
		RR_ASSERT( tll_arrivals.cost( parse_chunk_start*num_tlls ) != RR_S32_MAX );
		// but there should be nothing past :
		//RR_ASSERT( tll_arrivals.cost( (parse_chunk_start+1)*num_tlls ) == RR_S32_MAX );
		
		//-------------------------
		
		{
		SIMPLEPROFILE_SCOPE(tll_parse_inner);
		
		if ( num_tlls > 1 )
		{
			// wipe TLL arrival creep-over :
					
			for(int p=1;p<num_tlls;p++)
			{
				// should be no true packet arrivals :
				//RR_ASSERT( tll_arrivals[(parse_chunk_start+p)*num_tlls].cost == RR_S32_MAX );
				
				for(int t=0;t<num_tlls;t++)
				{
					tll_arrivals.cost((parse_chunk_start+p)*num_tlls + t) = RR_S32_MAX;
				}
			}
			
			for(int t=1;t<num_tlls;t++)
			{
				tll_arrivals.cost(parse_chunk_start*num_tlls + t) = RR_S32_MAX;
			}
			
			// anything beyond should be unfilled :
			RR_ASSERT( tll_arrivals.cost((parse_chunk_start + num_tlls)*num_tlls) == RR_S32_MAX );
			RR_ASSERT( tll_arrivals.cost((parse_chunk_start + num_tlls)*num_tlls + 1) == RR_S32_MAX );
			
			// just skip tiny tail :
			
			if ( (parse_chunk_end-parse_chunk_start) <= num_tlls ) 
			{
				RR_ASSERT( parse_chunk_end == parse_end_pos );
				parse_chunk_end = parse_end_pos = parse_chunk_start;
				goto parse_chunk_break_out;
			}
			
			//-------------------------
			
			// fill initial tlls from parse_chunk_start arrival :
			//  this stops any shitlet tails we left from last time
			for(int t=1;t<num_tlls;t++)
			{
				const SINTa dest_index = (parse_chunk_start+t)*num_tlls + t;
				const SINTa src_index = (parse_chunk_start + t-1)*num_tlls + (t-1);
				tll_arrivals[dest_index] = tll_arrivals[src_index];
				
				S32 lo = tll_arrivals[parse_chunk_start*num_tlls].los.LastOffset();
				tll_arrivals.cost(dest_index) = tll_arrivals.cost(src_index) + cost_add_literal(chunk_ptr,parse_chunk_start+t-1,lo,codecosts);
			}
			last_tll_len[parse_chunk_start+last_tll] = last_tll;
		}
		
		// iterate up to parse_chunk_end
		// but have to stop where there's a valid arrival
		//	(I only have arrivals from full packets, not bare literals)
		
		// NOTE : in TLL, parse_chunk_end is the last true packet arrival
		//	eg. TLL steps go *past* parse_chunk_end	
		
		for(int pos=parse_chunk_start; 
			parse_chunk_end <= parse_chunk_end_max;
			pos++)
		{
			// if we reach parse_end_pos, we're done :
			if ( pos == parse_end_pos )
			{
				parse_chunk_end = parse_end_pos;
				break;
			}
		
			const U8 * ptr = chunk_ptr + pos;
			U32 ptr32 = RR_GET32_NATIVE_UNALIGNED(ptr);

			// my_arrivals [num_tlls]
			newlz_optimal_arrival_tll * my_arrivals = &tll_arrivals[pos * num_tlls];
			S32 * my_costs = &tll_arrivals.cost(pos * num_tlls);
			
			{
			//SIMPLEPROFILE_SCOPE(inner1);
			
			//---------------------------------------------------------------
			
			if ( num_tlls == 1 )
			{
			
				if ( pos != cheap_preceding_arrival_pos )  // this is true every time except the first iteration at parse_chunk_start
				{			
					// extend cheap_preceding_arrival_literal_cost from earlier cheap_preceding_arrival_pos
					S32 lo = tll_arrivals[cheap_preceding_arrival_pos].los.LastOffset();
					cheap_preceding_arrival_literal_cost += cost_add_literal(chunk_ptr,pos-1,lo,codecosts);
					
					NEWLZ_ASSERT_HEAVY( cost_literals(chunktype,chunk_ptr,cheap_preceding_arrival_pos,lrl,lo,codecost_literal,subliteralmask) == cheap_preceding_arrival_literal_cost );
				
					if ( tll_arrivals.cost(pos) != RR_S32_MAX )
					{
						RR_ASSERT( parse_chunk_end >= pos );
						
						// I have a valid arrival
						// see if I should update cheap_preceding_arrival_pos to current pos
									
						// fix broken ass optimal parse
						//	do not give up the current "cheap_preceding_arrival_pos"
						//	if the cost to arrive there + literals to current pos
						//   is cheaper than the current arrival
						//	eg. don't always go to the *closest* arrival
						//	  go back to the *cheapest* arrival , duh
						// in non-TLL mode, this is how we find literal runs as an alternative to packets
						int lrl = pos - cheap_preceding_arrival_pos;

						S32 prev_cost = tll_arrivals.cost(cheap_preceding_arrival_pos);

						// we're considering alternative of keeping this lrl
						//	vs. sending a packet with a 0 lrl
						S32 cost_lrl = 0;
						//int lrl_for_packet = lrl;
						if ( lrl >= NEWLZ_PACKET_LRL_MAX )
						{
							//lrl_for_packet = NEWLZ_PACKET_LRL_MAX;
							int excess = lrl - NEWLZ_PACKET_LRL_MAX;
							cost_lrl = cost_excess(excess,codecosts);
						}				
					
						//RR_ASSERT( lrl_for_packet <= NEWLZ_PACKET_LRL_MAX );
						//cost_lrl += lrl_for_packet; // hacky bullshit
						
						S32 cost = prev_cost + cost_lrl + cheap_preceding_arrival_literal_cost;
						
						if ( cost > tll_arrivals.cost(pos) )
						{
							// I have a valid arrival AND I like it better than ignoring it and staying LRL
												
							cheap_preceding_arrival_pos = pos;
							cheap_preceding_arrival_literal_cost = 0;
							
							// if I reach parse_chunk_end it means there are no further arrivals
							//	go ahead and end this parse chunk now
							// I can only terminate a parse chunk at an arrival
							//	 (if there are no matches, parse chunks can get very long)
							if ( pos >= parse_chunk_end )
							{
								parse_chunk_end = pos;
								goto parse_chunk_break_out;
							}
						}
					}
				}
				
				NEWLZ_ASSERT_HEAVY( cost_literals(chunktype,chunk_ptr,cheap_preceding_arrival_pos,(pos - cheap_preceding_arrival_pos),arrivals[cheap_preceding_arrival_pos].los.LastOffset(),codecost_literal,subliteralmask) == cheap_preceding_arrival_literal_cost );
			}
			else // num_tlls > 1
			{
				// if I reach parse_chunk_end it means there are no further arrivals
				//	go ahead and end this parse chunk now
				// @@@@?? only do this if I do have a valid arrival
				if ( pos >= parse_chunk_end )
				{
					// if I don't have an arrival at [pos]
					//	set parse_chunk_end to the closest arrival before me
					// -> not the closet
					//	find the cheapest at [pos][tll] and go back to there
					S32 lowest_cost = RR_S32_MAX;
					S32 lowest_arrival_pos = 0;
					for(int t=0;t<num_tlls;t++)
					{
						if ( my_costs[t] < lowest_cost )
						{
							lowest_cost = my_costs[t];
							// where's the true packet arrival that landed in this TLL :
							S32 arrival_pos = pos;
							if ( t == last_tll ) arrival_pos -= last_tll_len[pos];
							else arrival_pos -= t;
							
							RR_ASSERT( tll_arrivals.cost(arrival_pos*num_tlls) != RR_S32_MAX );
							RR_ASSERT( tll_arrivals.cost(arrival_pos*num_tlls) <= lowest_cost );
							RR_ASSERT( arrival_pos == my_arrivals[t].check_arrival_pos );
							
							lowest_arrival_pos = arrival_pos;
						}
					}
					RR_ASSERT( lowest_cost != RR_S32_MAX );
					//RR_ASSERT( lowest_arrival_pos > parse_chunk_start ); 
					if ( lowest_arrival_pos >= parse_chunk_end )
					{		
						parse_chunk_end = lowest_arrival_pos;
						goto parse_chunk_break_out;
					}
				}		
			
				//---------------------------------------------------------------
			
				// @@@@?? try carrying forward from my_arrivals[0] as well
				//		-> that shouldn't help, make sure it doesn't
			
				if ( my_costs[last_tll] != RR_S32_MAX )
				{
					// continue long tll arrival forward :
					// note that a normal last_tll arrival at pos+1 would already be filled from
					//	the packet tail of (pos+1-last_tll)
					
					S32 lo = my_arrivals[last_tll].los.LastOffset();
					S32 cost = my_costs[last_tll] + cost_add_literal(chunk_ptr,pos,lo,codecosts);
					
					// + num_tlls steps you to pos+1
					S32 & dest_arrival_cost = my_costs[last_tll + num_tlls];
					if ( cost < dest_arrival_cost )
					{
						newlz_optimal_arrival_tll & dest_arrival = my_arrivals[last_tll + num_tlls];
						// does not change parse_chunk_end
						dest_arrival = my_arrivals[last_tll];
						dest_arrival_cost = cost;
						last_tll_len[pos+1] = last_tll_len[pos] + 1;
					}
				}
			}				
			
			}
			//---------------------------------------------------------------
			
			const UnpackedMatchPair * pairs = matches[pos].pairs;
			
			int longestml = 0;
			
			// try LRL 0-6 :
			//	OR if cheap_preceding_arrival_pos is > 6 away, just try that (always LRL excess)
			
			S32 lowest_base_cost = RR_S32_MAX;
			
			S32 match_lrl[MAX_MATCH_CANDIDATES];
			S32 match_base_cost[MAX_MATCH_CANDIDATES];
			S32 match_prev_index[MAX_MATCH_CANDIDATES];
			S32 match_longestlo[MAX_MATCH_CANDIDATES];
			SINTa match_lrl_candidate_count = 0;

			{
			//SIMPLEPROFILE_SCOPE(inner3);
			
			int cheap_preceding_arrival_lrl = pos - cheap_preceding_arrival_pos;
			
			// lrl_iterator<=max_search_lrl
			// AND lrl <= max_search_lrl BUT always do lrl == cheap_preceding_arrival_lrl
			for(int lrl_iterator=0;;lrl_iterator++)
			{
				S32 base_cost;
				int lrl;
				S32 prev_index;
				
				if ( num_tlls == 1 )
				{				
					// 12-19-2017 : changed LRL iteration
					//	if cheap_preceding was > max_search ,
					//	 I used to just do one LRL = cheap_preceding
					// based on the archaic idea that cheap_preceding = closest
					// but it's not, there can be arrivals closer
					// so instead :
					//   scan back all low LRL's normally
					//  at the longest LRL,
					//	 if cheap_preceding is further back, do it instead
				
					if ( lrl_iterator > max_search_lrl ) break;
						
					lrl = lrl_iterator;
					
					if ( lrl == max_search_lrl && cheap_preceding_arrival_lrl > max_search_lrl )
					{
						lrl = cheap_preceding_arrival_lrl;
					}
					
					prev_index = pos-lrl;
					if ( prev_index < parse_chunk_start )
						break;
					
					S32 arrival_cost = tll_arrivals.cost(prev_index);
					if ( arrival_cost == RR_S32_MAX )
						continue;
					
					const newLZ_LOs_NoPad & los = tll_arrivals[prev_index].los;
					
					// cost the sub literals :
					base_cost = arrival_cost;
					
					S32 lo = los.LastOffset();
					RR_ASSERT( lo >= NEWLZ_MIN_OFFSET );
							
					if ( lrl == cheap_preceding_arrival_lrl )
					{
						NEWLZ_ASSERT_HEAVY( cheap_preceding_arrival_literal_cost == cost_literals(chunktype,chunk_ptr,pos-lrl,lrl,lo,codecost_literal,subliteralmask) );
						base_cost += cheap_preceding_arrival_literal_cost;
					}
					else
					{			
						// we only do long lrl spans at cheap_preceding_arrival_lrl
						RR_ASSERT( lrl <= max_search_lrl );	
						base_cost += cost_literals(chunk_ptr,pos-lrl,lrl,lo,codecosts);
					}					
				}
				else // num_tlls > 1 
				{				
					if ( lrl_iterator < num_tlls )
					{
						// arrival at pos via tll
						// this is an arrival at [pos-t]
						//	stored in [pos][t]
						//	carried lrl t will go in packet
					
						S32 arrival_cost = my_costs[lrl_iterator];
						if ( arrival_cost == RR_S32_MAX )
							continue;
					
						lrl = lrl_iterator;
						if ( lrl_iterator == last_tll )
							lrl = last_tll_len[pos];
						
						// pos-lrl is where this packet starts
						RR_ASSERT( (pos - lrl) >= parse_chunk_start );
						
						RR_ASSERT( (pos-lrl) == my_arrivals[lrl_iterator].check_arrival_pos );
						
						prev_index = pos*num_tlls + lrl_iterator; // not my prev, this is my index, the prev that I am filling
						RR_ASSERT( tll_check_index_pos((pos-lrl),prev_index,num_tlls) );
							
						base_cost = arrival_cost;
					}
					else // TLL when lrl_iterator >= num_tlls
					{
						// coming from the packet arrivals ?
						//	shouldn't it be cheaper to come from the TLL arrivals ?
						// -> tried doing arrivals from TLL here, did not help
					
						// search lrl beyond the tll range
						// just take an arrival and cost the literals
					
						if ( lrl_iterator > max_search_lrl ) break;
				
						// -1 to make sure we check (or re-check) last_tll
						lrl = lrl_iterator-1;
						
						if ( (pos - lrl) < parse_chunk_start )
							break;
					
						prev_index = (pos - lrl)*num_tlls; // true packet arrival
						S32 arrival_cost = tll_arrivals.cost(prev_index);
						if ( arrival_cost == RR_S32_MAX )
							continue;
							
						base_cost = arrival_cost + 
							cost_literals(chunk_ptr,pos-lrl,lrl,tll_arrivals[prev_index].los.LastOffset(),codecosts);
					}
				}
				
				int lrl_for_packet = lrl;
				// base_cost includes lrl :
				if ( lrl >= NEWLZ_PACKET_LRL_MAX )
				{
					lrl_for_packet = NEWLZ_PACKET_LRL_MAX;
					int excess = lrl - NEWLZ_PACKET_LRL_MAX;
					base_cost += cost_excess(excess,codecosts);
				}	
					
				// arrival lands at (pos - lrl)
				// arrival_cost includes the literals in lrl
				// but does not include the cost of the lrl itself
				// base_cost has the cost of excess LRL added, the in-packet portion depends on ML so done later
				
				const newLZ_LOs_NoPad & los = tll_arrivals[prev_index].los;
				
				// find LO matches :
				// have to do this each time because LO set may be different
		
				int longestlo = 0;

				{
					//SIMPLEPROFILE_SCOPE(inner3_lo); // <- all the optimal parse time is here
					// this is the big slow meat of the optimal parse
					//	just doing getmatchlen_mml2 + try_lo_arrival over and over
					// @@ can we speed this up?

					for(int loi=0;loi<NEWLZ_NUM_LAST_OFFSETS;loi++)
					{
						S32 offset = los.lasts[loi];
						int ml;
						if ( ! havematch_mml2_one32(ptr32,ptr,ptr-offset,ptr_matchend, &ml ) )
							continue;

						// force higher LO index only for longer matches :
						if ( ml <= longestlo )
							continue;
						
						RR_ASSERT( ml >= NEWLZ_LOMML );
						longestlo = ml; // NOTE: ml > longestlo established above
						parse_chunk_end = RR_MAX(parse_chunk_end,pos+ml);

						S32 first_step_cost;

						if ( num_tlls != 1 || ml >= NEWLZ_SHORT_ENDML )
						{
							// cost it
							first_step_cost = try_lo_arrival_tll(tll_arrivals,num_tlls,prev_index,chunk_ptr,last_tll_len,
														pos,base_cost,lrl,lrl_for_packet,ml,loi,los,codecosts);

							// also try ml reduction
							// NOTE : these break lit-after-match exclusion and LRL0-LO0 exclusion
							//	but seem to help anyway

							if ( ml_range_for_lomatch_red.contains(ml) )
							{
								// -> try all lens
								for(int l=NEWLZ_LOMML;l<ml;l++)
								{
									try_lo_arrival_tll(tll_arrivals,num_tlls,prev_index,chunk_ptr,last_tll_len,
														pos,base_cost,lrl,lrl_for_packet,l,loi,los,codecosts);
								}
							}
						}
						else
						{
							const S32 * costs = codecosts.cost_packet_short[lrl_for_packet][loi];

							// Not too long; try all lens.
							first_step_cost = base_cost + costs[ml - NEWLZ_LOMML];
							TParseCore::try_cached_match_truncations(&tll_arrivals.cost(pos + NEWLZ_LOMML), base_cost, costs, ml - NEWLZ_LOMML,
								[prev_index,pos,lrl,loi,&los,&tll_arrivals](S32 i)
								{
									const S32 len = i + NEWLZ_LOMML;
									tll_arrivals[pos + len].set_lo(prev_index, pos,lrl,len,loi,los);
								}
							);
						}
						
						#ifdef TWO_STEP_MAX_LRL
						// look for two-step :
						if ( pos+ml < twostep_lo_end_pos )
						{
							try_two_step_arrival_tll(tll_arrivals,num_tlls,prev_index,chunk_ptr,ptr_matchend,&parse_chunk_end,
								pos+ml,first_step_cost,ml,lrl,loi,offset,los,codecosts);
						}
						#endif
					}
				}
				
				longestml = RR_MAX(longestlo,longestml);
				
				// early out to avoid repeating long getmatchlen's in degenerate cases :
				if ( longestml >= optimal_skip_len )
				{
					// this goto is the same as "break"
					goto long_ml_skip;
				}
			
				// Now try which normal matches improve on the base cost and are worth trying later.
				// We keep track of all candidates because LRL-ML correlation in the packet can
				// make the cost landscape somewhat complicated.
				if ( base_cost < lowest_base_cost )
				{
					RR_ASSERT( match_lrl_candidate_count < MAX_MATCH_CANDIDATES );

					lowest_base_cost = base_cost;
					match_lrl[match_lrl_candidate_count] = lrl;
					match_base_cost[match_lrl_candidate_count] = base_cost;
					match_prev_index[match_lrl_candidate_count] = prev_index;
					match_longestlo[match_lrl_candidate_count] = longestlo;
					match_lrl_candidate_count++;
				}
			} // lrl_iterator loop
			
			} // profile scope

			RR_ASSERT( lowest_base_cost != RR_S32_MAX ); // I should have had some arrival

			for(SINTa m=0;m<NEWLZ_MATCH_NUM_PAIRS;m++)
			{
				// Figure out what match offset and length to use
				S32 ml = pairs[m].length;
				
				// matches are in descending length
				// so when we hit a short one, get out
				if ( ml < mml ) // ml == 0 is the end-of-data marker
					break;
				
				U32 offset = pairs[m].offset;
				
				ml = RR_MIN(ml, rrPtrDiff32(ptr_matchend - ptr));
				
				// verify match :
				RR_ASSERT( memcmp(ptr,ptr-(SINTa)offset,ml) == 0 );
				
				if ( offset >= dictionarySize )
					continue;
					
				if ( offset < NEWLZ_MIN_OFFSET )
				{
					offset = newLZ_offset44_round_up_tiny(offset);

					if ( (SINTa)offset > rrPtrDiff(ptr - dictionaryBase) )
						continue;
					
					// mml4 ? should be mml3 ?
					//  -> changing to mml3 saves a microscopic few bytes
					//	-> just leave it alone
					ml = getmatchlen_mml4_one32(ptr32,ptr,ptr-offset,ptr_matchend);

					// If this offset did not get snapped to the min offset, also test at min offset
					// and take whichever is best; in case of ties, prefer MIN_OFFSET (which is also
					// what we're going to use for most runs so it's a likely common offset).
					if ( offset != NEWLZ_MIN_OFFSET )
					{
						S32 minml = getmatchlen_mml4_one32(ptr32,ptr,ptr-NEWLZ_MIN_OFFSET,ptr_matchend);
						if ( minml >= ml )
						{
							offset = NEWLZ_MIN_OFFSET;
							ml = minml;
						}
					}
					
					if ( ml < mml )
						continue;
				}
							
				RR_ASSERT( ml >= mml );
				
				// don't check isAllowed in optimal parse
				//	it's just for guiding the offset statistics
				//  instead allow arbitrariy offsets, and let the cost-chooser
				//	 make its own decision
				// -> yes! cool
				if ( ! newLZ_IsAllowedNormalMatch_Optimal(ml,offset,pOptions) )
					continue;
					
				// Of all candidate LRLs, check which one gives us the lowest combined cost of arrival,
				// literals, and packet byte. This accounts for LRL-ML correlation in the packet byte.
				S32 best_cost = RR_S32_MAX;
				SINTa best_i = -1;

				for (SINTa i = 0; i < match_lrl_candidate_count; ++i)
				{
					const S32 my_lrl = match_lrl[i];
					const S32 my_packet_lrl = RR_MIN(my_lrl, NEWLZ_PACKET_LRL_MAX);
					const S32 my_base_cost = match_base_cost[i];
					const S32 my_cost = my_base_cost + cost_normal_match(my_packet_lrl, ml, codecosts);
					if ( my_cost < best_cost )
					{
						best_cost = my_cost;
						best_i = i;
					}
				}

				if ( best_i < 0 )
					continue;

				const S32 base_cost = match_base_cost[best_i];
				const S32 prev_index = match_prev_index[best_i];
				const S32 lrl = match_lrl[best_i];
				const S32 longestlo = match_longestlo[best_i];

				const S32 lrl_for_packet = RR_MIN(lrl, NEWLZ_PACKET_LRL_MAX);
				const newLZ_LOs_NoPad & los = tll_arrivals[prev_index].los;

				RR_ASSERT( ml >= mml );
				RR_ASSERT( ptr+ml <= ptr_matchend );
				RR_ASSERT( memcmp(ptr,ptr-(SINTa)offset,ml) == 0 );
				RR_ASSERT( offset <= NEWLZ_MAX_OFFSET );
				RR_ASSERT( offset >= NEWLZ_MIN_OFFSET );
				//RR_ASSERT( newLZ_IsAllowedNormalMatch(ml,offset) );

				// enforce normal > LO :
				if ( ml <= longestlo )
					break;

				// no LO matches!
				// this should not happen because ml > longestlo
				RR_ASSERT( ml > longestlo );
				RR_ASSERT( los.Find(offset) == -1 );

				// okay, it's allowed - cost it :

				longestml = RR_MAX(ml,longestml);

				parse_chunk_end = RR_MAX(parse_chunk_end,pos+ml);

				// start with cost including lrl & offset
				S32 cost = base_cost + cost_offset(offset,codecosts);
				S32 first_step_cost;

				if ( num_tlls != 1 || ml >= NEWLZ_SHORT_ENDML )
				{
					first_step_cost = try_match_arrival_tll(tll_arrivals,num_tlls,prev_index,chunk_ptr,last_tll_len,
												pos,cost,lrl,lrl_for_packet,ml,offset,los,codecosts);

					if ( ml_range_for_match_red.contains(ml) )
					{
						// try len reductions
						// all lens
						for(int l=mml;l<ml;l++)
						{
							try_match_arrival_tll(tll_arrivals,num_tlls,prev_index,chunk_ptr,last_tll_len,
												pos,cost,lrl,lrl_for_packet,l,offset,los,codecosts);
						}
					}
				}
				else
				{
					const S32 * costs = codecosts.cost_packet_short[lrl_for_packet][3];

					// Not too long; try all lens.
					first_step_cost = cost + costs[ml - mml];
					TParseCore::try_cached_match_truncations(&tll_arrivals.cost(pos + mml), cost, costs, ml - mml,
						[mml,prev_index,pos,lrl,offset,&los,&tll_arrivals](S32 i)
						{
							const S32 len = i + mml;
							tll_arrivals[pos + len].set_match(prev_index, pos,lrl,len,offset,los);
						}
					);
				}

				#ifdef TWO_STEP_MAX_LRL
				if ( pos+ml < twostep_end_pos )
				{
					try_two_step_arrival_tll(tll_arrivals,num_tlls,prev_index,chunk_ptr,ptr_matchend,&parse_chunk_end,
						pos+ml,first_step_cost,ml,lrl,-1,offset,los,codecosts);
				}
				#endif
			}

			// if long ml - jump ahead !
			if ( longestml >= optimal_skip_len )
			{
				long_ml_skip:
				
				RR_ASSERT( longestml >= optimal_skip_len );
				
				int longestml_arrival_pos = pos+longestml;
				
				// my arrival should be filled, and should have pushed parse_chunk_end >= longestml_arrival_pos
				RR_ASSERT( longestml_arrival_pos <= parse_chunk_end );
				RR_ASSERT( tll_arrivals.cost(longestml_arrival_pos*num_tlls) != RR_S32_MAX );

				if ( longestml_arrival_pos == parse_chunk_end )
				{
					// longestml took us to chunk end
					// just jump to the end and terminate parse chunk
					parse_chunk_end = cheap_preceding_arrival_pos = longestml_arrival_pos;
					goto parse_chunk_break_out;
				}
			
				pos = longestml_arrival_pos-1; // (-1 because of pos++ in loop)
				
				if ( num_tlls == 1 )
				{
					// invalidate carried state :
					cheap_preceding_arrival_pos = pos+1;
					cheap_preceding_arrival_literal_cost = 0;
				}
				else
				{
					// I need to fill trailing literals from here
					//	 (at pos+1)				
					S32 lo = tll_arrivals[longestml_arrival_pos*num_tlls].los.LastOffset();	
					for(int t=1;t<num_tlls;t++)
					{
						SINTa index = (longestml_arrival_pos+t)*num_tlls + t;
						SINTa src_index = (longestml_arrival_pos + t-1)*num_tlls + (t-1);
						tll_arrivals[index] = tll_arrivals[src_index];
						tll_arrivals.cost(index) = tll_arrivals.cost(src_index) + cost_add_literal(chunk_ptr,longestml_arrival_pos+t-1,lo,codecosts);
					}
					last_tll_len[longestml_arrival_pos+last_tll] = last_tll;
				}
			}
		} // pos++ loop
		parse_chunk_break_out:
		
		;
		} // profile scope
		
		//rrprintf("adapt chunk len : %d\n",(parse_chunk_end - parse_chunk_start));
		
		// I should now have an arrival at parse_chunk_end that traces back to parse_chunk_start
		//	unless parse_chunk_end == parse_end_pos in which case I might not have an arrival
		
		// not true ; parse_end_pos is the last allowed match start location
		//	parse_chunk_end is the last found packet arrival pos
		//RR_ASSERT( parse_chunk_end <= parse_end_pos );
		RR_ASSERT( tll_arrivals.cost(parse_chunk_end*num_tlls) != RR_S32_MAX || parse_chunk_end == parse_end_pos );
		RR_ASSERT( num_tlls > 1 || cheap_preceding_arrival_pos >= parse_chunk_start );
		
		S32 parse_chunk_end_index = parse_chunk_end * num_tlls;
		
		//bool is_final_arrival = ( parse_chunk_end >= parse_end_pos );
		bool is_final_arrival = ( parse_chunk_end >= parse_end_pos-2 );
	
		if ( is_final_arrival )
		{
			// will set final_arrival_pos
		
			//SIMPLEPROFILE_SCOPE(tll_parse_eof);
			
			// we're at EOF
			//	I may have a final arrival at [parse_chunk_end]
			//	there may be a cheaper arrival before that at cheap_preceding_arrival_pos
			
			int final_arrival_pos = -1;
			S32 final_arrival_cost = RR_S32_MAX;
				
			#define FINAL_EXTRA_BACKUP 8
				
			if ( num_tlls == 1 )
			{
				RR_ASSERT( cheap_preceding_arrival_pos >= parse_chunk_start );
				RR_ASSERT( cheap_preceding_arrival_pos <= parse_chunk_end );
				
				// @@ in rare cases (chunk_end - cheap_preceding_arrival_pos) can be nearly 128k
				//	 in which case doing cost_literals 8 times is a bit expensive
				int backup_start_pos = RR_MAX(cheap_preceding_arrival_pos-FINAL_EXTRA_BACKUP,parse_chunk_start);
				// doing some unnecessary iterations here but whatevs
				//	can't be any arrival within SAFE ZONE of chunk_len
				for(int pos=backup_start_pos;pos<chunk_len;pos++)
				{
					S32 cost = tll_arrivals.cost(pos);
					if ( cost == RR_S32_MAX )
						continue;
				
					//const U8 * ptr = chunk_ptr + pos;
					S32 lo = tll_arrivals[pos].los.LastOffset();
						
					cost += cost_literals(chunk_ptr,pos,(chunk_len - pos),lo,codecosts);
					
					if ( cost < final_arrival_cost )
					{
						final_arrival_cost = cost;
						final_arrival_pos = pos;
					}
				}
				
				parse_chunk_end = final_arrival_pos;
				parse_chunk_end_index = final_arrival_pos;
			}
			else
			{				
				int backup_start_pos = RR_MAX(parse_chunk_end-FINAL_EXTRA_BACKUP,parse_chunk_start);
				
				for(int pos=backup_start_pos;pos<chunk_len;pos++)
				{				
					for(int t=0;t<num_tlls;t++)
					{
						S32 index = pos*num_tlls + t;
						S32 cost = tll_arrivals.cost(index);
						if ( cost == RR_S32_MAX )
							continue;
					
						// I already have the literal cost from arrival pos up to parse_chunk_end
						//	add on the literals from there to the end 
						// no LRL cost in the tail
					
						S32 lo = tll_arrivals[index].los.LastOffset();
							
						int lrl = t;
						if ( t == last_tll ) lrl = last_tll_len[pos];
						int arrival_pos = pos - lrl;
						
						RR_ASSERT( tll_arrivals[index].check_arrival_pos == arrival_pos );
						if ( arrival_pos < parse_chunk_start )
							continue;
						
						// TLL arrival cost already includes the literals from arrival_pos -> pos
						//	add on the cost from pos -> chunk_len
						
						cost += cost_literals(chunk_ptr,pos,(chunk_len - pos),lo,codecosts);
						
						if ( cost < final_arrival_cost )
						{							
							final_arrival_cost = cost;
							final_arrival_pos = arrival_pos;
							
							parse_chunk_end = final_arrival_pos;
							parse_chunk_end_index = index;
						}
					}
				}
			}
			
			RR_ASSERT( final_arrival_cost != RR_S32_MAX ); 
			RR_ASSERT( final_arrival_pos >= 0 );
			RR_ASSERT( final_arrival_pos >= parse_chunk_start );

			/*
			// make sure my code costs match reality
			//	(will be off a little due to penalties and huffman re-cost feedback)
			rrprintf("final_arrival_cost : %d\n",final_arrival_cost);
			rrprintf("final_arrival_cost bits : %f\n",final_arrival_cost/(float)COST_ONE_BIT);
			rrprintf("final_arrival_cost bytes : %f\n",(final_arrival_cost/(8.f*COST_ONE_BIT)));
			rrprintf("greedy_complen : %d\n",greedy_complen);
			*/
			
			RR_ASSERT( tll_check_index_pos(final_arrival_pos,parse_chunk_end_index,num_tlls) );
		}
	
		{
		//SIMPLEPROFILE_SCOPE(tll_parse_reverse);		
		
		// reverse the parse & output it
		// fill parsevec by tracing back through arrivals :
		chunk_parsevec.clear();
		int rpos = parse_chunk_end;
		int rindex = parse_chunk_end_index;
		for(;;)
		{
			newlz_optimal_arrival_tll & arrival = tll_arrivals[rindex];
			RR_ASSERT( tll_check_index_pos(rpos,rindex,num_tlls) );
			RR_ASSERT( arrival.check_arrival_pos == rpos );

			RR_ASSERT( rpos >= parse_chunk_start );
			if ( rpos == parse_chunk_start )
			{
				break;
			}
			
			if ( arrival.is_twostep )
			{
				int twostep_prev_pos = rpos - arrival.twostep.ml - arrival.twostep.lrl;
				RR_ASSERT( twostep_prev_pos < rpos );
				RR_ASSERT( twostep_prev_pos >= parse_chunk_start );
		
				// first the twostep LO match :		
				chunk_parsevec.push_back();
				newlz_encoder_parse & parse = chunk_parsevec.back();
				parse.lastoffset = arrival.los.LastOffset(); // LO0 after the
				parse.offset = 0; // LO0
				parse.ml = arrival.twostep.ml;
				parse.lrl = arrival.twostep.lrl;
				RR_ASSERT( parse.IsLO() );
			
				// verify the match :
				RR_DURING_ASSERT( const U8 * twostep_ptr = chunk_ptr + rpos - arrival.twostep.ml );
				RR_ASSERT( memcmp(twostep_ptr,twostep_ptr - parse.lastoffset,arrival.twostep.ml) == 0 );
			
				rpos = twostep_prev_pos;
			}
			
			int prev_pos = rpos - arrival.ml - arrival.lrl;
			RR_ASSERT( prev_pos < rpos );
			RR_ASSERT( prev_pos >= parse_chunk_start );
			
			int prev_index = arrival.prev_index;
			RR_ASSERT( tll_check_index_pos(prev_pos,prev_index,num_tlls) );
			
			const newlz_optimal_arrival_tll & prev_arrival = tll_arrivals[prev_index];
			
			RR_ASSERT( prev_arrival.check_arrival_pos == prev_pos );
			RR_ASSERT( prev_pos >= parse_chunk_start );
			
			const newLZ_LOs_NoPad & prev_los = prev_arrival.los;
			S32 offset = arrival.los.LastOffset();
			S32 loi = prev_los.Find(offset);
			if ( loi >= 0 ) offset = -loi;
				
			RR_ASSERT( offset == arrival.check_offset );
				
			#ifdef RR_DO_ASSERTS
			{
				newLZ_LOs_NoPad los = prev_los;
				if ( offset > 0 ) los.Add(offset);
				else los.MTF(-offset);
				RR_ASSERT( memcmp(&los,&arrival.los,sizeof(los)) == 0 );
			}
			#endif
				
			chunk_parsevec.push_back();
			newlz_encoder_parse & parse = chunk_parsevec.back();

			// I need the LO to apply to my literals; that's the *previous* LO
			parse.lastoffset = prev_arrival.los.LastOffset();
			parse.lrl = arrival.lrl;
			parse.ml = arrival.ml;
			parse.offset = offset;

			rpos = prev_pos;
			rindex = prev_index;
		}
		
		// should have traced back to packet start
		RR_ASSERT( rpos == parse_chunk_start );
		RR_ASSERT( tll_check_index_pos(parse_chunk_start,rindex,num_tlls) );
		RR_ASSERT( tll_arrivals[rindex].check_arrival_pos == parse_chunk_start );
				
		// chunk_parsevec is back-to-front, reverse it :
		reverse(chunk_parsevec.begin(),chunk_parsevec.end());

		parsevec.appendv(chunk_parsevec);

		} // profile scope
				
		//RR_ASSERT( parse_chunk_end < parse_end_pos ); // <- not true, parse_chunk_end can be at the end of the last match
		RR_ASSERT( parse_chunk_end <= (chunk_len - NEWLZ_MATCH_END_PAD) );
		
		if ( is_final_arrival )
		{
			// this is the normal end of parsing
			break;
		}
			
		// tried scale-down of histos here - don't work
		//optimal_rescale_passinfo_incremental(passinfo);
	
		optimal_update_passinfo(
			chunk_parsevec,
			parse_chunk_start,
			passinfo,
			chunk_ptr);

		if ( parse_chunk_end - last_cost_update_pos >= cost_update_interval )
		{
			last_cost_update_pos = parse_chunk_end;
			optimal_passinfo_to_codecost(passinfo,codecosts,mml);
		}
		
		parse_chunk_start = parse_chunk_end;
	
	} // parse chunks loop
}

static SINTa newLZ_encode_chunk_optimal_tll(const newlz_vtable * vtable,
		newlz_encoder_scratch * scratch,
		const U8 * dictionaryBase,
		const U8 * chunk_ptr,int chunk_len,
		U8 * comp,U8 * comp_end,
		SINTa chunk_pos,
		int * pchunktype,
		F32 * pJ,
		const OodleKrakenChunkDeadlines * deadline)
{
	//rrprintf("newLZ_encode_chunk : %d\n",chunk_len);
	SIMPLEPROFILE_SCOPE_N(encode_optimal_tll,chunk_len);
	THREADPROFILESCOPE("kraken_chunk_optimal");

	*pchunktype = 0;
	
	if ( chunk_len <= NEWLZ_MIN_CHUNK_LEN )
	{
		return chunk_len;
	}
	
	// desired update interval : (this is sensitive/tweaky)
	int parse_chunk_len = 256;
	
	// forced updates
	int parse_chunk_len_max = 4096;
			
	int num_tlls;
	int max_search_lrl;
	
	if ( vtable->level >= OodleLZ_CompressionLevel_Optimal4 ) // Optimal4 is TLL
	{
		num_tlls = 2; // more than 2 doesn't help
		max_search_lrl = 8;
	}
	else
	{
		num_tlls = 1;
		
		if ( vtable->level >= OodleLZ_CompressionLevel_Optimal2 ) max_search_lrl = 3;
		else max_search_lrl = 1; // Optimal1
			
		// max_search_lrl = 4 means all the packet LRLs (0-3), PLUS one more for cheapest_preceding instead3		// max_search_lrl = 4 
		// max_search_lrl = 1 means just LRL 0 + cheapest_preceding (or LRL =1)
		// max_search_lrl = 3 means the last packet LRL (3) which triggers excess uses cheapest_preceding (or 3)

		// Optimal1 is CTMF
		// Optimal2 is ST
		// Optimal3 is MML 3 selection
		// Optimal4 is TLL
	}
	
	//=============================================
	
	rrArenaAllocator * arena = scratch->arena;
	
	const OodleLZ_CompressOptions * pOptions = vtable->pOptions;
	U32 dictionarySize = ( pOptions->dictionarySize > 0 ) ?
		RR_MIN(NEWLZ_MAX_OFFSET,pOptions->dictionarySize)
		: NEWLZ_MAX_OFFSET;

	RR_ASSERT( vtable->find_all_matches_num_pairs == NEWLZ_MATCH_NUM_PAIRS );
	newLZ_MatchParseRecord * matches = (newLZ_MatchParseRecord *) scratch->match_pairs;
	
	/*
	
	@@ 
	newlz chunk_parsevec is unnecessary, kill it
		just append right onto parsevec

	-> or only keep chunk_parsevec
	and just fill it per adaptation chunk
	then output to "arrays" after each step
	no overall parsevec

	*/
	
	vector_a<newlz_encoder_parse> parsevec;
	vector_a<newlz_encoder_parse> chunk_parsevec;
	SINTa parsevec_bytes = sizeof(newlz_encoder_parse)*chunk_len/2;
	SINTa chunk_parsevec_bytes = sizeof(newlz_encoder_parse)*parse_chunk_len_max;
	
	scratch->parsevec_space.extend(parsevec_bytes+chunk_parsevec_bytes,arena);
	parsevec.provide_arena(scratch->parsevec_space.m_ptr,parsevec_bytes);
	chunk_parsevec.provide_arena((char *)scratch->parsevec_space.m_ptr+parsevec_bytes,chunk_parsevec_bytes);

	int literals_plus_packets_limit = newlz_literal_space_reserve_size(chunk_len);

	scratch->literals_space.extend(literals_plus_packets_limit,arena);
	U8 * literal_space = scratch->literals_space.getU8();
	U8 * literal_space_end = literal_space + literals_plus_packets_limit;
		
	int start_pos = 0;
	if ( chunk_pos == 0 ) start_pos = NEWLZ_MIN_OFFSET;
		
	newlz_passinfo passinfo;
			
	const newLZ_MatchParseRecord * pmatches = matches;
	
	SINTa arrivals_count = num_tlls*(chunk_len+1);
	scratch->arrivals_space.extend(newlz_optimal_arrivals_tll::size(arrivals_count),arena);
	newlz_optimal_arrivals_tll tll_arrivals;
	tll_arrivals.init(scratch->arrivals_space.m_ptr, arrivals_count);
	
	//=====================================
	
	// mml >= 4 for the greedy seed pass :
	int mml = RR_MAX(4,pOptions->minMatchLen);
	F32 greedy_J = LAGRANGE_COST_INVALID;
	
	SINTa greedy_complen = newLZ_encode_chunk_optimal_greedy(
						&greedy_J,
						pchunktype,&passinfo,
						comp,comp_end,
						mml,
						vtable,pmatches,chunk_ptr,chunk_len,chunk_pos,dictionaryBase,deadline,
						scratch,literal_space,literal_space_end);
	
	// expanded :
	if ( greedy_complen >= chunk_len )
		return chunk_len;

	// If we have a whole-chunk match, there's no point trying to do anything else, just using
	// the basic greedy parse which uses that match is definitely optimal.
	if ( chunk_pos != 0 // Can't have whole-chunk match at a reset point
		 && pmatches[0].pairs[0].length >= chunk_len - NEWLZ_CHUNK_NO_MATCH_ZONE )
	{
		*pJ = greedy_J;
		rrPrintf_v2("Full-chunk match fast path greedy_complen=%d", (int)greedy_complen);
		return greedy_complen;
	}
	
	// output into the arrivals buffer :
	U8 * optimal_comp = (U8 *)scratch->arrivals_space.m_ptr;
	U8 * optimal_comp_end = optimal_comp + scratch->arrivals_space.m_size;
	
	newlz_passinfo passinfo_mml_alt;
		
	// MML3 testing on -z7 for Kraken , on all levels for Hydra
	if ( ( vtable->level >= OodleLZ_CompressionLevel_Optimal3
		|| vtable->compressor == OodleLZ_Compressor_Hydra ) &&
		pOptions->minMatchLen <= 3 )
	{
		int chunktype_mml3;
		F32 greedy_J_mml3 = LAGRANGE_COST_INVALID;
			
		SINTa greedy_complen_mml3 = newLZ_encode_chunk_optimal_greedy(
						&greedy_J_mml3,
						&chunktype_mml3,&passinfo_mml_alt,
						optimal_comp,optimal_comp_end,
						3,
						vtable,pmatches,chunk_ptr,chunk_len,chunk_pos,dictionaryBase,deadline,
						scratch,literal_space,literal_space_end);

		if ( greedy_J_mml3 < greedy_J &&
			greedy_complen_mml3 < chunk_len )
		{
			*pchunktype = chunktype_mml3;
			memcpy(comp,optimal_comp,greedy_complen_mml3);
		
			greedy_J = greedy_J_mml3;
			greedy_complen = greedy_complen_mml3;
		
			mml = 3;	
			passinfo = passinfo_mml_alt;
		}
	}
	
	// MML 8 testing at all levels ?
	// no need at pOptions->minMatchLen == 8 because I did that in the first greedy pass
	// -> this helps a decent amount on GameTestSet
	if ( 8 > pOptions->minMatchLen )
	{
		// 8 is better than 6
		int chunktype_mml8;
		F32 greedy_J_mml8 = LAGRANGE_COST_INVALID;
				
		SINTa greedy_complen_mml8 = newLZ_encode_chunk_optimal_greedy(
						&greedy_J_mml8,
						&chunktype_mml8,&passinfo_mml_alt,
						optimal_comp,optimal_comp_end,
						8,
						vtable,pmatches,chunk_ptr,chunk_len,chunk_pos,dictionaryBase,deadline,
						scratch,literal_space,literal_space_end);

		if ( greedy_J_mml8 < greedy_J &&
			greedy_complen_mml8 < chunk_len )
		{
			*pchunktype = chunktype_mml8;
			memcpy(comp,optimal_comp,greedy_complen_mml8);
		
			greedy_J = greedy_J_mml8;
			greedy_complen = greedy_complen_mml8;
			
			/*
			// change optimal MML here? or leave it?
			// -> pretty meh
			// -> this is stomped to 3 anyway
			mml = 8;
			/**/

			// do change passinfo but not mml :
			passinfo = passinfo_mml_alt;
		}
	}
		
	
	// set mml to 3	regardless of the greedy choice !
	if ( ( vtable->level >= OodleLZ_CompressionLevel_Optimal3
		|| vtable->compressor == OodleLZ_Compressor_Hydra ) &&
		pOptions->minMatchLen <= 3 )
	{
		mml = 3;
	}
	
	//-----------------------------------------------------------------------------
	
	// tll_arrivals[0] is a true packet arrival
	// tll_arrivals[t] has t trailing literals
	// tll_arrivals[last_tll] has trailing len last_tll_len[pos]
	
	int last_tll = chunk_len; // make sure I'm not used
	vector_a<int> last_tll_lenv;
	int * last_tll_len = NULL;
	if ( num_tlls > 1 )
	{
		SINTa last_tll_len_space_bytes = (chunk_len+1)*sizeof(int);
		scratch->last_tll_len_space.extend(last_tll_len_space_bytes,arena);
		last_tll_lenv.provide_arena(scratch->last_tll_len_space.m_ptr,last_tll_len_space_bytes);
	
		last_tll = num_tlls-1;
		last_tll_lenv.resize(chunk_len+1,0);
		last_tll_len = last_tll_lenv.data();
	}
	
	//-----------------------------------------------------------------------------
	// optimal parse!
	
	// do_optimal_iter will repeat the parse for statistics refinement
	//	-> now done only if literal type changes
	//  -> in Kraken this is pretty rare, so the net effect on compression & encodetime are both small
	bool do_optimal_iter = ( vtable->level >= OodleLZ_CompressionLevel_Optimal4 );

	for (int optimal_iter=0;;optimal_iter++)
	{
	
	newlz_codecosts codecosts;
	
	// take chunktype from the greedy parse :
	codecosts.chunktype = *pchunktype;
	codecosts.subliteralmask = (codecosts.chunktype == NEWLZ_LITERALS_TYPE_RAW) ? 0 : (U32)-1;
	
	//-----------------------------------------

	// @@!! options for seeding histo :
	//	from greedy parse
	//	from last chunk's optimal
	//	from flat model
	//	blend?
	//	can use it for only the first chunk
	//	or scale it down over time
	//	or just add it in as a base
	// wipe out the histos :
	//rrMemSet32_Aligned(&passinfo,1,sizeof(passinfo));
	
	
	// passinfo currently contains the greedy pass
		
	// only carry if chunktype is the same it was
	//	 even carrying both types of literals, it's important to only do this if it matches
	// carried_encoder_state_chunktype = -1 on first chunk
	if ( vtable->carried_encoder_state_chunktype >= 0 )
	{
		// blend the previous optimal + the current greedy
		//	maybe very marginally better (than just taking previous), not big
		
		newlz_scratchblock & carried_encoder_state = const_cast<newlz_scratchblock &>(vtable->carried_encoder_state);
		RR_ASSERT( carried_encoder_state.size() == sizeof(newlz_passinfo) );
		const newlz_passinfo * p_prev_passinfo = (const newlz_passinfo *) carried_encoder_state.get();		
		
		// what if offset_alt_modulo is different? -> optimal_rescale can change passinfo.offset_alt_modulo
			
		bool literals_same = ( vtable->carried_encoder_state_chunktype == codecosts.chunktype );

		optimal_rescale_passinfo2(passinfo,*p_prev_passinfo,literals_same);
	}
	else
	{
		// @@ could still use previous optimal chunk passinfo for non-literal histos
	
		// else it comes from the greedy pass
		optimal_rescale_passinfo(passinfo);
	}
	
 	// NOTE : do after optimal_rescale_passinfo2 , which can change this!
	codecosts.offset_alt_modulo = passinfo.offset_alt_modulo;
	
	optimal_passinfo_to_codecost(passinfo,codecosts,mml);

	// wipe all the arrivals :
	for(int i=0;i<=chunk_len*num_tlls;i++)
	{
		tll_arrivals.cost(i) = RR_S32_MAX;
	}

	{
		S32 & arrival_cost = tll_arrivals.cost(start_pos*num_tlls + 0);
		newlz_optimal_arrival_tll & arrival = tll_arrivals[start_pos*num_tlls + 0];
		arrival_cost = 0;
		arrival.los.Reset();
		arrival.ml = 0;
		arrival.lrl = 0;
		arrival.prev_index = 0;
		arrival.is_twostep = 0;
		RR_DURING_ASSERT( arrival.check_offset = 0 );
		RR_DURING_ASSERT( arrival.check_arrival_pos = start_pos );
	}
	
#ifdef __RADSSE2__
	using ParseCore = NewLZParseCoreSSE2;
#else
	using ParseCore = NewLZParseCoreGeneric;
#endif

	if ( num_tlls == 1 )
	{
		newLZ_encode_chunk_optimal_parse_tll_core<1, ParseCore>(
			chunk_ptr, chunk_len, start_pos,
			parse_chunk_len, parse_chunk_len_max,

			dictionaryBase, dictionarySize,
			matches, mml, max_search_lrl,

			tll_arrivals,
			last_tll, last_tll_len,

			vtable,
			passinfo,
			codecosts,
			parsevec,
			chunk_parsevec,
			pOptions
		);
	}
	else if ( num_tlls == 2 )
	{
		newLZ_encode_chunk_optimal_parse_tll_core<2, ParseCore>(
			chunk_ptr, chunk_len, start_pos,
			parse_chunk_len, parse_chunk_len_max,

			dictionaryBase, dictionarySize,
			matches, mml, max_search_lrl,

			tll_arrivals,
			last_tll, last_tll_len,

			vtable,
			passinfo,
			codecosts,
			parsevec,
			chunk_parsevec,
			pOptions
		);
	}
	else
	{
		RR_BREAK();
	}
	
	// output the parsevec :
	
	int optimal_chunktype;
	F32 optimal_J = LAGRANGE_COST_INVALID;
	// fills passinfo for carried_state :
	SINTa optimal_complen = newLZ_put_parse(scratch,&optimal_J,&optimal_chunktype,chunk_ptr,chunk_len,optimal_comp,optimal_comp_end,chunk_pos,vtable,parsevec,&passinfo,deadline);
			
	/*
	if ( greedy_J < optimal_J )
	{
		rrprintf("%8d : greedy better  : [%.3f] < %.3f : [%d] vs %d\n",chunk_pos,greedy_J,optimal_J,greedy_complen,optimal_complen);
	}
	else
	{
		rrprintf("%8d : optimal better : %.3f >= [%.3f] : %d vs [%d]\n",chunk_pos,greedy_J,optimal_J,greedy_complen,optimal_complen);
	}
	/**/
	
	//rrprintf("optimal_complen : %d\n",optimal_complen);
	// optimal_complen can be worse than greedy sometimes ; revert?
	if ( optimal_J < greedy_J )
	{
		RR_ASSERT( optimal_complen < chunk_len );
		*pchunktype = optimal_chunktype;
		*pJ = optimal_J;
		memcpy(comp,optimal_comp,optimal_complen);
		
		// if I'm iteratable && literal type changed :
		// this gets hit on lzt03 ; you save 100-1000 bytes (per chunk) by iterating
		if ( do_optimal_iter && optimal_iter == 0 && codecosts.chunktype != optimal_chunktype )
		{
			// *pchunktype is used for what's stored in [comp] AND
			//	also for how should I parse the current chunk
			
			//rrprintf("literal type changed, iterating : %d -> %d\n",codecosts.chunktype,optimal_chunktype);
	
			// newLZ_put_parse filled "passinfo"
			
			// invalidate carried state so we will work off "passinfo" :
			const_cast<newlz_vtable *>(vtable)->carried_encoder_state_chunktype = -1;
			
			// our fallback is now the first pass of optimal :
			//	(must match *pchunktype)
			greedy_J = optimal_J;
			greedy_complen = optimal_complen;
			
			continue;
		}
	
		newlz_scratchblock & carried_encoder_state = const_cast<newlz_scratchblock &>(vtable->carried_encoder_state);
	
		carried_encoder_state.extend( sizeof(newlz_passinfo) , arena);
		memcpy(	carried_encoder_state.get(),&passinfo,sizeof(passinfo));
		
		const_cast<newlz_vtable *>(vtable)->carried_encoder_state_chunktype = optimal_chunktype;
	
		return optimal_complen;
	}
	else
	{
		*pJ = greedy_J;
		rrPrintf_v2("Greedy reverted : %d -> %d : %d == %.3f%%\n",optimal_complen,greedy_complen,
				(optimal_complen - greedy_complen),(optimal_complen - greedy_complen)*100.0/chunk_len);
		return greedy_complen;
	}
	
	// never loops
	
	} // optimal_iter

}

//===================================

#if OODLE_PLATFORM_HAS_ADVANCED_MATCHERS

OODLE_NS_END
#include "suffixtrie.h"
OODLE_NS_START

#endif // OODLE_PLATFORM_HAS_ADVANCED_MATCHERS


void Kraken_FillVTable(
	newlz_vtable * pvtable,
	OodleLZ_Compressor compressor,
	SINTa raw_len,
	OodleLZ_CompressionLevel level,
	const OodleLZ_CompressOptions * pOptions,
	const U8 * dictionaryBase,	
	const U8 * raw,
    rrArenaAllocator * arena)	
{
	newlz_vtable & vtable = *pvtable;
	
	int table_bits = GetLZMatchTableBits(raw_len,
						RR_MAX(level,OodleLZ_CompressionLevel_VeryFast), // avoid SuperFast
						pOptions,16,20,17,24);

// was , with 2 dwords per entry :
	//					pOptions,17,21,18,24);
								
	// spaceSpeedTradeoffBytes default is 256		
	//int array_min_size_gain_base = pOptions->spaceSpeedTradeoffBytes / 2;
	// with default options this is always 128 :
	//RR_ASSERT( array_min_size_gain_base == 128 );
	
	// compressor can be Hydra or Kraken, decodeType is always Kraken
	vtable.compressor = compressor;
	vtable.decodeType = RAD_LZ_DECODE_KRAKEN;
	vtable.level = level;
	
	vtable.speedfit = speedfit_get_default();
	S32 version = g_OodleLZ_BackwardsCompatible_MajorVersion;
#ifndef OODLE_CUSTOM_REDUCED_PROFILE
	version = 2; // default reduced profile is just v2 bitstream, Kraken only
#endif
	
	vtable.lambda = NEWLZ_LAMBDA_KRAKEN;
	// scale lambda by spaceSpeedTradeoffBytes  :
	vtable.lambda *= newlz_spaceSpeedTradeoffBytes_for_lambda(pOptions) * (1.f / OODLELZ_DEFAULT_SSTB);
	
	vtable.chunk_len = NEWLZ_CHUNK_LEN; // @@ paramter? and have to transmit it?
	vtable.pOptions = pOptions;
	vtable.try_huff_chunks = (level >= OodleLZ_CompressionLevel_Fast);
	vtable.entropy_flags = 0;
	vtable.bitstream_flags = 0;
	if ( version >= 3 ) // Oodle 2.3.0 introduces Huff6
		vtable.entropy_flags |= NEWLZ_ARRAY_FLAG_ALLOW_HUFF6;
	
	if ( version >= 6 ) // Oodle 2.6.0 introduces TANS/RLE
	{
		vtable.entropy_flags |= NEWLZ_ARRAY_FLAG_ALLOW_TANS | NEWLZ_ARRAY_FLAG_ALLOW_RLE | NEWLZ_ARRAY_FLAG_ALLOW_RLEHUFF;
		vtable.entropy_flags |= NEWLZ_ARRAY_FLAG_ALLOW_SPLIT;
		vtable.entropy_flags |= NEWLZ_ARRAY_FLAG_ALLOW_HUFFLENS2;
		vtable.entropy_flags |= NEWLZ_ARRAY_FLAG_ALLOW_RLE_MEMSET;
	
		// @@ indexed splits at optimal level 2 or 3 ?
		//	 (depends on encode time impact mainly)
		if ( level >= OodleLZ_CompressionLevel_Optimal3 )
		{
			vtable.entropy_flags |= NEWLZ_ARRAY_FLAG_ALLOW_SPLIT_INDEXED;
		}

		// for encode speed , alt offsets only at higher levels :
		if ( level >= OodleLZ_CompressionLevel_Optimal1 )
		{
			vtable.bitstream_flags |= NEWLZ_BITSTREAM_FLAG_ALT_OFFSETS;
		}
	}
	
	if ( pOptions->profile == OodleLZ_Profile_Reduced )
	{
#ifdef OODLE_CUSTOM_REDUCED_PROFILE
		extern void kraken_custom_reduced_profile(newlz_vtable * pvtable);
		kraken_custom_reduced_profile(&vtable);
#endif
	}
	
	//-----------------------------------------------------
	
	int hash_len = 4;
	
	// all levels below Normal test for hash length ?
	//	@@ ?? turn off at HyperFast2 ?
	if ( level < OodleLZ_CompressionLevel_Normal && level >= OodleLZ_CompressionLevel_HyperFast2 )
	{
		if ( pOptions->minMatchLen > 4 )
		{
			// minMatchLen over 4 is specified in options, just use that as hash len
			// @@ todo : also for Normal? (and don't second hash?)
			hash_len = RR_MIN(8,pOptions->minMatchLen);
		}
		else if ( raw_len > 64*1024 && raw != NULL )
		{
			// below 64k , len 4 is often better, even on text files
	
			if ( newlz_guess_should_hash_length_be_over_four(raw,raw_len) )
			{
				hash_len = 6;
			}
		}
	}

	//-----------------------------------------------------
	// set up newLZ "vtable" :

	if ( level <= OodleLZ_CompressionLevel_HyperFast1 )
	{
		switch ( level )
		{
		case OodleLZ_CompressionLevel_HyperFast1:
			if ( pOptions->matchTableSizeLog2 <= 0 ) table_bits = RR_MIN(table_bits,16);
			newlz_vtable_setup_ctmf<newLZ_CTMF_HyperFast32>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len); // U32 entries
			vtable.fp_encode_chunk = newLZ_encode_chunk_fast_mode<newLZ_CTMF_HyperFast32,4,1,0,1,0,0>; // backup, no quadratic skip, sub-lits allowed
			break;

		case OodleLZ_CompressionLevel_HyperFast2:
			if ( pOptions->matchTableSizeLog2 <= 0 ) table_bits = RR_MIN(table_bits,14);
			newlz_vtable_setup_ctmf<newLZ_CTMF_HyperFast16>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len); // U16 entries
			vtable.fp_encode_chunk = newLZ_encode_chunk_fast_mode<newLZ_CTMF_HyperFast16,4,1,0,0,0,0>; // backup, no quadratic skips, no sub-lits
			break;

		case OodleLZ_CompressionLevel_HyperFast3:
		case OodleLZ_CompressionLevel_HyperFast4:
		default:
			if ( pOptions->matchTableSizeLog2 <= 0 ) table_bits = RR_MIN(table_bits,12);
			
			// HF4 uses same parser as HF3, but takes a different path in arrays_output (no literal entropy)
			 
			RR_ASSERT( hash_len == 4 ); // longer hashes should not be active here
			
			newlz_vtable_setup_ctmf<newLZ_CTMF_HyperFast16>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len); // U16 entries
			vtable.fp_encode_chunk = newLZ_encode_chunk_fast_mode<newLZ_CTMF_HyperFast16,3,0,1,0,0,0>; // no backup, aggressive quadratic skip, no sub-lits
			break;
		}

		// Disallow the less common entropy types in fast modes
		vtable.entropy_flags &= ~(NEWLZ_ARRAY_FLAG_ALLOW_TANS | NEWLZ_ARRAY_FLAG_ALLOW_SPLIT | NEWLZ_ARRAY_FLAG_ALLOW_RLE);
	}
	else 
	if ( level <= OodleLZ_CompressionLevel_SuperFast )
	{
		// if not explicitly given, limit table to 19 bits :
		if ( pOptions->matchTableSizeLog2 <= 0 ) table_bits = RR_MIN(table_bits,19);
		
		newlz_vtable_setup_ctmf<newLZ_CTMF_HyperFast32>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len);
		vtable.fp_encode_chunk = newLZ_encode_chunk_fast_mode<newLZ_CTMF_HyperFast32,5,1,0,1,1,1>;
		
		// Disallow the less common entropy types in fast modes
		// RLE used to be borderline when the encoder was slower, now that's it faster
		// it's not worth it.
		vtable.entropy_flags &= ~(NEWLZ_ARRAY_FLAG_ALLOW_TANS | NEWLZ_ARRAY_FLAG_ALLOW_SPLIT | NEWLZ_ARRAY_FLAG_ALLOW_RLE);
	}
	else if ( level <= OodleLZ_CompressionLevel_VeryFast )
	{
		// VeryFast : do_lazy OFF
		// do lazy yes or no is a big step here
		typedef CTMF<U32,1,0,4>	newLZ_CTMF_VeryFast;
	
		newlz_vtable_setup_ctmf<newLZ_CTMF_VeryFast>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len);
		vtable.fp_encode_chunk = newLZ_encode_chunk<newLZ_CTMF_VeryFast,0,1,8>;
		
		// Disallow the less common entropy types in fast modes (RLE is now on)
		//	-> could allow RLE but not RLEHUFF ?
		vtable.entropy_flags &= ~(NEWLZ_ARRAY_FLAG_ALLOW_TANS | NEWLZ_ARRAY_FLAG_ALLOW_SPLIT);
	}
	else if ( level == OodleLZ_CompressionLevel_Fast )
	{
		// Fast : do_lazy ON
		//	note : no literal step-ahead, incompatible with lazy
		typedef CTMF<U32,2,0,4>	newLZ_CTMF_Fast;
	
		newlz_vtable_setup_ctmf<newLZ_CTMF_Fast>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len);
		vtable.fp_encode_chunk = newLZ_encode_chunk<newLZ_CTMF_Fast,1,1,0>;
		
		// Disallow the less common entropy types in fast modes
		vtable.entropy_flags &= ~(NEWLZ_ARRAY_FLAG_ALLOW_TANS | NEWLZ_ARRAY_FLAG_ALLOW_SPLIT);
	}
	#if OODLE_PLATFORM_HAS_ADVANCED_MATCHERS
	else if ( level >= OodleLZ_CompressionLevel_Optimal2 )
	{
		vtable.carried_encoder_state.reserve(sizeof(newlz_passinfo), arena);
		vtable.fp_encode_chunk = newLZ_encode_chunk_optimal_tll;
		vtable.fp_create_match_finder = SuffixTrie_CreateMatchFinder;
		vtable.find_all_matches_num_pairs = NEWLZ_MATCH_NUM_PAIRS;
		vtable.wants_dic_limit_splits = true;
	}
	else if ( level == OodleLZ_CompressionLevel_Optimal1 )
	{
		vtable.carried_encoder_state.reserve(sizeof(newlz_passinfo), arena);
		vtable.fp_encode_chunk = newLZ_encode_chunk_optimal_tll;
		vtable.fp_create_match_finder = CTMF_CreateMatchFinder;
		vtable.find_all_matches_num_pairs = NEWLZ_MATCH_NUM_PAIRS;
		vtable.wants_dic_limit_splits = false;
	}
	#endif
	else 
	{
		// Normal + Optimals on platforms that aren't HAS_ADVANCED_MATCHERS
	
		RR_ASSERT( hash_len == 4 ); // should not be active here
			
		newlz_vtable_setup_ctmf<newLZ_CTMF_Normal>(&vtable,dictionaryBase,raw,table_bits,arena);
		
		vtable.fp_encode_chunk = newLZ_encode_chunk<newLZ_CTMF_Normal,2,1,0>;
		
		if ( level == OodleLZ_CompressionLevel_Normal )
		{
			// no SPLIT in Normal mode for encode speed ?

			// turn off indexed splits :
			//vtable.entropy_flags &= ~(NEWLZ_ARRAY_FLAG_ALLOW_SPLIT_INDEXED);
			// it's already off :
			RR_ASSERT( (vtable.entropy_flags & NEWLZ_ARRAY_FLAG_ALLOW_SPLIT_INDEXED) == 0 );

			// YES do non-indexed splits :
			//	NEWLZ_ARRAY_FLAG_ALLOW_SPLIT is on
			//	@@ this appears to be a slight win by "Enc Weissman N" rating
			//	 -> but that needs more careful study , the Normal Enc Weissman range is not well tweaked
			
			// @@ probably no TANS either (need to measure enc time vs size benefit)
			vtable.entropy_flags &= ~NEWLZ_ARRAY_FLAG_ALLOW_TANS;
			
			//vtable.entropy_flags |= (NEWLZ_ARRAY_FLAG_ALLOW_SPLIT_INDEXED);
		}
	}

}


SINTa Kraken_Compress(
	OodleLZ_Compressor compressor,
	const U8 * raw,U8 * comp,SINTa raw_len,
	OodleLZ_CompressionLevel level,
	const OodleLZ_CompressOptions * pOptions,
	const U8 * dictionaryBase,
	const LRMCascade * lrm_casc,
    rrArenaAllocator * arena)
{
	SIMPLEPROFILE_SCOPE(kraken_compress);
	
	rrArenaAllocatorStateSaver saver(arena); //,newlz_arena_alloc_size_if_none);
	newlz_vtable vtable;
	
	Kraken_FillVTable(&vtable,compressor,raw_len,level,pOptions,dictionaryBase,raw,arena);
	
	// raw == NULL is for OodleLZ_GetCompressScratchMemBound
	if ( raw == NULL )
		return newlz_enc_mem_bound(&vtable,raw_len);
	
	SINTa ret = newlz_compress_vtable(&vtable,
		raw,comp,raw_len,
		dictionaryBase,
		lrm_casc,arena);

	return ret;
}

//=================================================================

OODLE_NS_END
#include "newlzf.h"
#include "newlzhc.h"
OODLE_NS_START

/**

Hydra :

run Kraken & Mermaid, take best
currently doesn't exactly consider Selkie
 but can do Mermaid with entropy coding disabled
 
NOTE on matchers :

at levels >= Optimal1
	find_all_matches is run *once* for both Kraken & Mermaid
	matches go into PMP array which both can use
	
at level < Optimal1
	each vtable gets its own matcher (CTMF variant)
	they both advance it
	

At levels < Optimal, the time to run Hydra is roughly just the sum of the two encoders
at levels >= Optimal, Hydra is a bit faster than just running both
	because the matching work can be shared

**/

SINTa Hydra_Compress(
	OodleLZ_Compressor compressor,
	const U8 * raw,U8 * comp,SINTa raw_len,
	OodleLZ_CompressionLevel level,
	const OodleLZ_CompressOptions * pOptions,
	const U8 * dictionaryBase,
	const LRMCascade * lrm_casc,
    rrArenaAllocator * arena)
{

	if ( raw == NULL )
	{
		// from OodleLZ_GetCompressScratchMemBound
		// punt for now
		// @@ fix me? meh?
		return OODLELZ_SCRATCH_MEM_NO_BOUND;
	}
	
	rrArenaAllocatorStateSaver saver(arena); //,newlz_arena_alloc_size_if_none);
	newlz_vtable vtable1;
	newlz_vtable vtable2;
	newlz_vtable vtable3;
	
	Kraken_FillVTable(&vtable1,OodleLZ_Compressor_Hydra,raw_len,level,pOptions,dictionaryBase,raw,arena);
	Mermaid_FillVTable(&vtable2,OodleLZ_Compressor_Mermaid,raw_len,level,pOptions,dictionaryBase,raw,arena);
	
	// make lambda the same; use Kraken scale factor
	
	vtable1.pvtable2 = &vtable2;
	vtable2.lambda = vtable1.lambda;
	// only try huff chunks once :
	vtable1.try_huff_chunks = true;
	vtable2.try_huff_chunks = false;
	
	// only do Leviathan in Hydra if version >= 6 :
	if ( g_OodleLZ_BackwardsCompatible_MajorVersion >= 6 )
	{
		Leviathan_FillVTable(&vtable3,OodleLZ_Compressor_Leviathan,raw_len,level,pOptions,dictionaryBase,raw,arena);
	
		vtable2.pvtable2 = &vtable3;
		vtable3.lambda = vtable1.lambda;
		vtable3.try_huff_chunks = false;
	}
	
	SINTa ret = newlz_compress_vtable(&vtable1,
		raw,comp,raw_len,
		dictionaryBase,
		lrm_casc,
		arena);
		
	return ret;

}

//=================================================================

// verify OODLELZ_MAX_SCRATCH_FOR_PHASE_HEADERS_AND_FUZZ is big enough :
//	the sizeof changes in different builds, so OODLELZ_MAX_SCRATCH_FOR_PHASE_HEADERS_AND_FUZZ should be an overestimate
RR_COMPILER_ASSERT( (NEWLZ_EXTRA_SCRATCH_MEM_FOR_FUZZ + sizeof(newLZ_chunk_arrays)) <= OODLELZ_MAX_SCRATCH_FOR_PHASE_HEADERS_AND_FUZZ );


const char * newlz_literals_type_name[] =
{
	"sub","raw","lamsub","suband3","o1","subandf"
};

RR_COMPILER_ASSERT( RR_ARRAY_SIZE(newlz_literals_type_name) == NEWLZ_LITERALS_TYPE_MAX+1 );

OODLE_NS_END
