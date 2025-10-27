// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "oodlelzcompressors.h"
#include "cbradutil.h"
#include <stdlib.h>

#include "rrlz_getmatchlen.inl"
#include "rrprefetch.h"
#include "rrlzh_lzhlw_shared.h"
#include "newlzf.h"

#include "templates/rrvector_a.h"

/*
#include "rrsimpleprof.h"
/*/
#include "rrsimpleprofstub.h"
/**/

#include "ctmf.h"
#include "newlz_arrays.h"
#include "newlz_subliterals.h"
#include "newlz_vtable.h"
#include "newlz_speedfit.h"

#include "histogram.h"
#include "rrlogutil.h"
#include "threadprofiler.h"
#include "oodleconfigvalues.h"

#include "rrbits.h"

#include "newlzf_decoder.h"
#include "newlz_shared.h"
#include "newlz_arrays.inl"
#include "speedfitter.h"
#include "matchfinder.h"

#ifndef OODLE_BUILDING_DLL
// dump: used to prepare input files for standalone selkie_test
//#define DO_NEWLZF_DUMP
#endif

#ifdef DO_NEWLZF_DUMP
#include <stdio.h>
#endif

OODLE_NS_START

//#undef __RADSSE2__	// for debug decoder

#if 0 && !defined(OODLE_BUILDING_DLL)
// for comptime lambda optimization :
extern double g_comptime_param;

#define NEWLZF_LAMBDA_MERMAID	g_comptime_param
#define NEWLZF_LAMBDA_SELKIE	g_comptime_param
#else

#define NEWLZF_LAMBDA_MERMAID	0.050f
#define NEWLZF_LAMBDA_SELKIE	0.140f
#endif

/*
#ifdef __RADARM__
#pragma message "ARM"
#endif
#ifdef __RADARM64__
#pragma message "ARM64"
#endif
#ifdef __RAD64REGS__
#pragma message "RAD64REGS"
#endif
#ifdef __RADNEON__
#pragma message "NEON"
#endif
*/

/**

newlzf = Mermaid & Selkie

Selkie = Mermaid-NoEntropy

Selkie data shows up as Mermaid in the decoder


==============

**/


// Huff adaptation is good for compression :
//	but of course hurts decode speed somewhat
// smaller chunk also allows finer grain switching to uncompressed chunks
#define NEWLZF_CHUNK_LEN	(128*1024)

// short chunks have no hope of meeting the space-speed switch :
#define NEWLZF_MIN_CHUNK_LEN			NEWLZ_MIN_CHUNK_LEN

// Mermaid : 9-14 or 10-14 ?
// Selkie : 12-20 or 14-22 ?

// Weissman score testing suggests 10-14 and 14-22

#define OFF24_MML_MERMAID		10
#define OFF24_MML_SELKIE		14

RR_COMPILER_ASSERT( OFF24_MML_MERMAID >= NEWLZF_OFF24_MML_DECODE );
RR_COMPILER_ASSERT( OFF24_MML_SELKIE  >= NEWLZF_OFF24_MML_DECODE );

// MML 3 is allowed in the format but only used at level >= Optimal3 (z7)
#define NEWLZF_MML_DECODE	3
#define NEWLZF_MML_NORMAL	4

// LOMML is 2 in the normal parse, 1's only come in to reduce packet count
#define NEWLZF_LOMML_NORMAL	2
//#define NEWLZF_LOMML_NORMAL	4

#define NEWLZF_MAX_OFFSET		OODLELZ_MAX_OFFSET
//#define NEWLZF_MAX_OFFSET		(0xFFFFFF)	// 3 byte offset
//#define NEWLZF_MAX_OFFSET		(0xFFFFFF - (1<<16))	// back 64k cuz of end of pointer relative


#define NEWLZF_CHUNK_MATCH_SAFE_ZONE 16

/**

NEWLZF_CHUNK_MATCH_SAFE_ZONE

the constraint comes from match copy doing a 16-byte splat

so you can't *start* a match within 16 of end

longer matches can get broken, and none of those pieces can start within 16 of end either

easier just to make the whole 16-bytes-at-end region be forbidden
	
**/

//=======================================

/**

Mermaid game test set ; literals choice 04/20/2016 4:53 PM :

sub/raw :
average : 118,706,201 ->48,561,455 =  3.273 bpb =  2.444 to 1 
total   : 118,706,201 ->58,274,326 =  3.927 bpb =  2.037 to 1 

sub only :
average : 118,706,201 ->48,810,081 =  3.289 bpb =  2.432 to 1 
total   : 118,706,201 ->58,368,391 =  3.934 bpb =  2.034 to 1 

**/
		
#if 0 //def _DEBUG
#define DO_CHECK	1
#else
#define DO_CHECK	0
#endif

#if DO_CHECK
#define CHECK(x)	x
#else
#define CHECK(x)
#endif

/***

newLZF = newLZ-Fast

summary 02/24/2016

newLZF is based on newLZ but fast

like newLZ , newLZF uses :

sub literals
separate huffman-coded arrays for literals & packet
packet of combined LRL & ML
minimum offset of 8 (no overlap check in decoder)

the big differences are :

non-entropy-coded offset
  just 2 or 3 bytes raw
non-entropy-coded excess lens
packet does not check excess
only one lastoffset

The big difference in the packet parse is :

packet is :
	3 bits LRL
	1 bit offset (LO or not)
	4 bits ML
	
LRL and ML can both be zero - no min
LRL and ML don't check excess in the normal packet
	(eg. LRL = 7 and ML = 15 do NOT cause you to fetch more)
	
Long LRL's can be written as a sequence of :
	{LRL=7 + ML=0}
	{LRL=7 + ML=0}
	{last LRL + following ML}
Similarly long ML's can be written as
	{LRL=initial LRL + ML=15}
	{LRL=0 + ML=15}
	{LRL=0 + last ML}
	

So the decoder basic action is :

loop on packets
	{
	get packet
	add 8 literals (LRL <= 7)
	copy 16 byte match (ML <= 15)
	}
		
all branchless, unconditional

Because newLZF unconditionally does a match after every LRL
it doesn't hurt speed to do short matches
eg. LOMML 1 and MML 3 are totally fine
(LOMML 1 is a bit funny; useful only when it reduces threshold LRL's to save a packet)


NewLZF now also does do excesses for LRL and ML
long LRL's and ML's can be sent as a sequence of normal packets
but sometimes they are very long and it's just faster to have a special flag for that
(this is the LRL_EXCESS and ML_EXCESS parameters)
these are large (currently 64)
so that this branch stays rare in the decoder
-> the excess path helps both space and speed


3-byte offsets are now sent in the escape path
with the long matches
3-byte offsets have a large MML (8) so they stay rare
-> the idea is to make these rare for the branch being rare in the decoder
-> also to account for the cost of going out of L2
	newLZF is designed with slow-memory architectures like Jaguar & ARM in mind
	so we try to stay in the 64k normal offset case
	and only go to large offsets when we get a really big win (long match)

NewLZF is very wacky to parse
lots of weird cases of LRL/ML spilling to more packets
the optimal parse is going to be "interesting"

=============================

Stats on escape packets :

g_stat_num_packets : 5789734
g_stat_num_packets_escape : 181891
g_stat_num_quartets : 1447202
g_stat_num_quartets_escape : 143933
escapes : 3.14 %
quartet escapes : 9.95 %

***/

/****

newLZF = newLZ-Fast

newLZF + Huff = Mermaid
newLZF + byte = Selkie

Selkie is a pretty damn good LZB !!!

=================

newlzf - retweak the escape ML and LRL

newlzf - could hide a special memset code in the escape case

newlzf - could make offset unlimited
	3-byte offset is already hidden in the rare escape case
	could make it fully variable
	2-3-4 , or just 3-4 ?
	flag it in the escape code or in the offset?

newlzf - get17/put17 for excesses
	haven't really tweaked this
	-> need to handle 18 bit if I allow 256k chunks

newlzf - LOMML 1 ? questionable
	-> it seems to be a win
	but like LZNIB I bet there are lots of cases where it's not good
	certainly if you have LRL on either side of you, probably not
		except if by splitting that LRL you keep them in packet size limits

newlzf - CTMF - secondary 8-byte hash ?
	to find those off-24 len 8 matches?
	-> the jump up to len-8 for long matches means I could use a specialized match finder
	short matches in first 64k (16 bits), then 8-byte matches at off-24
	-> could be faster & better

newlzf- IsLazy
	if lrl == 7 don't do lazy
	-> lots of crazy fucking edge cases in newlzf like this

newlzf - use MML4 CTMF + extra hash for len3
	biases towards being better at finding long matches
	like LZNIB

newlzf - ML_EXCESS path
	could flag 2/3 byte offset
	saves a byte sometimes
	I have 24 values there, don't really need them all for ML
	could use one as a flag bit
	(there are also sometimes long lastoffset matches; those don't need to send offset at all)
	(so it could be a trinary flag)

	could use packet = 21,22,23 for the ML EXCESS path
		(offset = LO, 2 byte, 3-byte)
	use packet = 1-20 for the 24-bit offset path


newlzF - being very sloppy with buffer overrun

newLZF_IsLazyMatchBetter
	-> all the parse heuristics are not really right for newlzf weirdness

newlzf - LOMML 1 seesm to be the win, but beware evil tricky parse decisions with that
	(seems to hurt on enwik8, help on most others)

****/

/*
S64 g_raw_offset_bits = 0;
S64 g_raw_lrl_bits = 0;
S64 g_raw_ml_bits = 0;
*/

/*
S64 g_stat_chunks = 0;
S64 g_stat_literals = 0;
S64 g_stat_num_packets = 0;
S64 g_stat_num_packets_lrl_excess = 0;
S64 g_stat_num_packets_ml_excess = 0;
S64 g_stat_num_packets_either_excess = 0;
*/

/*
S64 g_stat_num_packets = 0;
S64 g_stat_num_packets_escape = 0;
S64 g_stat_num_quartets = 0;
S64 g_stat_num_quartets_escape = 0;
*/
			
//=============================================================================

// Initializes table of maximum offsets given the ML. ML>=32 is always allowed all offsets.
// offset.
// off24mml is a constant for Mermaid/Selkie
// normal_mml can be 3 or 4 for Optimal/Normal parses, or overridden by user
static void newLZF_InitOffsLimitTab(S32 offsLimitTab[32], int normal_mml, int off24mml, const OodleLZ_CompressOptions * pOptions)
{
	// off24mml is the threshold to go over 64k
	// off24mml = 10 Mermaid , 14 Selkie

	// (2*off24mml-6) = 14 Mermaid , 22 Selkie
	int off24mml_1M = 2*off24mml-6;
	
	// ?? only super-long matches out of the 4 MB L3 ?
	//  -> yes, this is the big speed difference on webster
	//	@@ use (off24mml_1M+10) instead? (lower for Mermaid?)
	int off24mml_4M = 32;
	
	// @@@@ what about a 2M threshold? test again on targets with 2MB shared L2
	//	maybe (off24mml_1M + off24mml_4M)/2
	
	RR_ASSERT( normal_mml >= 3 );

	for (int i = 0; i < 32; ++i)
	{
		if ( i < normal_mml )
			offsLimitTab[i] = 0;
		else if ( i < off24mml )
			offsLimitTab[i] = 1<<16; // 64k
		else if ( i < off24mml_1M )
			offsLimitTab[i] = 1<<20; // 1MB
		else if ( i < off24mml_4M )
			offsLimitTab[i] = 1<<22; // 4MB
		else
			offsLimitTab[i] = OODLELZ_MAX_OFFSET;
	}

	// NOTE: farMatch options can only make the constraint *stronger* (smaller offs limit) than normal
	if ( pOptions->farMatchOffsetLog2 > 0 && pOptions->farMatchOffsetLog2 <= 30 )
	{
		S32 farOffsLimit = 1 << pOptions->farMatchOffsetLog2;

		for (int i = RR_MAX(pOptions->farMatchMinLen, 0); i < 32; ++i)
			offsLimitTab[i] = RR_MIN(offsLimitTab[i], farOffsLimit);
	}
}

static RADINLINE bool newLZF_IsAllowedNormalMatch(int ml,int off, const S32 offsLimitTab[32])
{
	RR_ASSERT( off >= NEWLZF_MIN_OFFSET );
	return ml >= 32 || off < offsLimitTab[ml];
}

#define NEWLZF_NORMAL_16BIT_OFFSET_ML_STEP	5
// @@ <- this something that could be a text/binary mode tweak
//		note that tweaking it also needs decode speed care (check Weissmans)

static RADINLINE bool newLZF_IsNormalMatchBetter(int ml,int off, int bestml, int bestoff)
{
	// typically off > bestoff
	//	but it goes backwards for hash2 and the explicit 8 check and such

	// if lens the same, just take lower offset :
	if ( ml == bestml )
		return off < bestoff;

	if ( (off | bestoff) <= 0xFFFF )
	{
		// both in 16 bit : just take the longer
		return ml > bestml;
	}
	else
	{
		bool best16 = bestoff <= 0xFFFF;
		bool off16 = off <= 0xFFFF;

		rrbool diff16 = best16 ^ off16;
	
		if ( diff16 )
		{
			if ( best16 )
			{
				return ml > bestml + NEWLZF_NORMAL_16BIT_OFFSET_ML_STEP;
			}
			else
			{
				// -> this is not hit normally because CTMF goes in order of increasing offset
				// -> for encode speed could remove this
				return ml >= bestml - NEWLZF_NORMAL_16BIT_OFFSET_ML_STEP;
			}
		}
		else
		{
			// offsets same class : just take the longer
			return ml > bestml;
		}
	}
}

static RADINLINE bool newLZF_IsLOMatchBetter(int repLen,int mainLen,int mainDist)
{
	// @@ NEEDS WORK
	// if offset is over 0xFFFF
	//	 then strongly favor rep
	// NEWLZF_LOMML_NORMAL = 2
	// @@ this is probably something that could be different for text/binary with data detection
	if ( repLen >= NEWLZF_LOMML_NORMAL && (
		(repLen + 1 >= mainLen) ||
		(repLen + 4 >= mainLen && mainDist > 0xFFFF))) // <- can skip this in the 64k matchers
	{
		return true;
	}
	else
	{
		return false;
	}
}

static RADINLINE U32 newLZF_FixSmallOffset(U32 offset)
{
	RR_ASSERT( offset < NEWLZF_MIN_OFFSET );
	// go up in multiples of offset : (smallest multiple of offset >=NEWLZF_MIN_OFFSET)
	static_assert(NEWLZF_MIN_OFFSET == 8, "min offset doesn't match adjusted offset table");
	static const U32 adjusted_offset[8] = { 0,8,8,9, 8,10,12,14 };

	return adjusted_offset[offset];
}

// Selkie likes 4 , Mermaid likes 5 , pretty consistently 
//	it's pretty small fiddly shit though
// -> this is reflects trying to put prices on
#define NEWLZF_LAZYBETTER_LAZY2 4	
//#define NEWLZF_LAZYBETTER_LAZY2 5

/*

NEWLZF_LAZYBETTER_LAZY2 = 4

Mermaid :

SUM:average : 143,579,361 ->63,124,656 =  3.517 bpb =  2.275 to 1 
SUM:total   : 143,579,361 ->63,933,558 =  3.562 bpb =  2.246 to 1 
SUM:encode           : 9.755 seconds, 117.48 c/b, rate= 14.72 mb/s
SUM:decode           : 95.702 millis, 1.15 c/b, rate= 1500.27 mb/s
SUM:encode+decode    : 9.851 seconds, 118.63 c/b, rate= 14.58 mb/s
SUM:Weissman 1-256 : [5.093] 40-800 : [2.211] 100-inf : [1.988]

Selkie :

SUM:average : 143,579,361 ->73,928,199 =  4.119 bpb =  1.942 to 1 
SUM:total   : 143,579,361 ->72,428,633 =  4.036 bpb =  1.982 to 1 
SUM:encode           : 8.858 seconds, 106.67 c/b, rate= 16.21 mb/s
SUM:decode           : 65.670 millis, 0.79 c/b, rate= 2186.38 mb/s
SUM:encode+decode    : 8.923 seconds, 107.46 c/b, rate= 16.09 mb/s
SUM:Weissman 1-256 : [4.595] 40-800 : [2.140] 100-inf : [2.141]

NEWLZF_LAZYBETTER_LAZY2 = 5

Mermaid :

SUM:average : 143,579,361 ->63,075,538 =  3.514 bpb =  2.276 to 1 
SUM:total   : 143,579,361 ->63,890,458 =  3.560 bpb =  2.247 to 1 
SUM:encode           : 9.686 seconds, 116.64 c/b, rate= 14.82 mb/s
SUM:decode           : 95.425 millis, 1.15 c/b, rate= 1504.63 mb/s
SUM:encode+decode    : 9.781 seconds, 117.79 c/b, rate= 14.68 mb/s
SUM:Weissman 1-256 : [5.097] 40-800 : [2.213] 100-inf : [1.992]

Selkie :

SUM:average : 143,579,361 ->73,950,206 =  4.120 bpb =  1.942 to 1 
SUM:total   : 143,579,361 ->72,437,007 =  4.036 bpb =  1.982 to 1 
SUM:encode           : 8.808 seconds, 106.08 c/b, rate= 16.30 mb/s
SUM:decode           : 65.668 millis, 0.79 c/b, rate= 2186.46 mb/s
SUM:encode+decode    : 8.874 seconds, 106.87 c/b, rate= 16.18 mb/s
SUM:Weissman 1-256 : [4.595] 40-800 : [2.140] 100-inf : [2.141]

*/

static RADINLINE int newLZF_LazyMatchDelta(int newMatchLen,U32 newOffset,bool newIsLast,
                int oldMatchLen,U32 oldOffset,bool oldIsLast)
                // int lrl)
{
	int newOffsetBits = 0;
	int oldOffsetBits = 0;
	if ( ! newIsLast ) { newOffsetBits = newOffset > 0xFFFF ? 32 : 16; }
	if ( ! oldIsLast ) { oldOffsetBits = oldOffset > 0xFFFF ? 32 : 16; }
	
	int newGain = newMatchLen*5 - newOffsetBits;
	int oldGain = (oldMatchLen+1)*5 - oldOffsetBits;
	
	// 3*5 is just under 16 , there's an important near-edge case there
	// eg. rep of len 2 vs. normal of len 5
	
	// this issue is just so microscopic :
	//if ( lrl == 7 || lrl == 14 ) newGain -= 4;
	
	return newGain - oldGain;
}

//=============================================================================

static RADINLINE bool is_escape_match( SINTa codeoffset, int ml )
{
	// codeoffset == 0 for LO , so high offset LO's are NOT included in here
	return ( codeoffset > 0xFFFF || ml >= NEWLZF_ML_EXCESS );
}

// newlzf_encoder_parse is now only used for optimal parse
//	the other coders output directly to encoder_arrays
struct newlzf_encoder_parse
{
	S32 lrl;
	S32 ml;
	S32 offset; // this is a "code offset" : zero for LO
	//S32 lastoffset; // <- no longer storing this
	
	bool IsLO() const { return offset == 0; }
	SINTa GetMatchOffset(SINTa lastoffset) const { return offset == 0 ? lastoffset : offset; }
	bool IsEscape() const { return is_escape_match(offset,ml); }
};



static RADINLINE S32 getmatchlen_0to8(U32 ptr32,const U8 * ptr,const U8 * vs_ptr)
{
	S32 len;
	
	#ifdef __RAD64REGS__
	U64 me_64 = RR_GET64_NATIVE(ptr);
	U64 vs_64 = RR_GET64_NATIVE(vs_ptr);
	U64 x = me_64 ^ vs_64;
	if ( x == 0 )
	{
		len = 8;
	}
	else
	{
		len = GetNumBytesZeroNeverAll64(x);
	}
	#else
	U32 vs32 = RR_GET32_NATIVE(vs_ptr);
	if ( ptr32 == vs32 )
	{
		U32 vs32_2 = RR_GET32_NATIVE(vs_ptr+4);
		U32 ptr32_2 = RR_GET32_NATIVE(ptr+4);
	
		U32 x = ptr32_2 ^ vs32_2;
		if ( x == 0 )
		{
			len = 8;
		}
		else
		{
			len = 4 + GetNumBytesZeroNeverAll32(x);
		}		
	}
	else
	{
		len = GetNumBytesZeroNeverAll32(ptr32^vs32);
	}
	#endif
	
	RR_ASSERT( len >= 0 && len <= 8 );
	
	return len;
}
		
//=======================================================================================


#if 1

// @@@@ experimental :
//	longer MML if you're in an EXCESS LRL run
//	to try to stay in it
//	-> this helps decode speed a lot if I go to 4,6
//	-> but then also hurts compression
//	-> at 3,5 it seems to have little effect on either
//	-> this is only in the non-optimal parse right now

//#define NEWLZF_IN_EXCESS_LRL_LOMML	4
//#define NEWLZF_IN_EXCESS_LRL_MML	6
	
//#define NEWLZF_IN_EXCESS_LRL_LOMML	3
//#define NEWLZF_IN_EXCESS_LRL_MML	5

#define NEWLZF_IN_EXCESS_LRL_LOMML	3
#define NEWLZF_IN_EXCESS_LRL_MML	5

#endif

/**

Going to 6/4 is a big speed effect on x-ray :
also decent compression loss
effect on most files is small
(effect on webster is none)

Jaguar :

6/4 :
x-ray :  8,474,240 -> 7,582,843 =  7.158 bpb =  1.118 to 1
encode only      : 3.791 seconds, 713.05 c/b, rate= 2.24 mb/s
decode only      : 8.913 millis, 1.68 c/b, rate= 950.77 mb/s

5/3 :
x-ray :  8,474,240 -> 7,278,490 =  6.871 bpb =  1.164 to 1
encode only      : 3.858 seconds, 725.49 c/b, rate= 2.20 mb/s
decode only      : 12.103 millis, 2.28 c/b, rate= 700.18 mb/s

old : (4/2)
x-ray :  8,474,240 -> 7,277,553 =  6.870 bpb =  1.164 to 1
encode only      : 3.742 seconds, 703.78 c/b, rate= 2.26 mb/s
decode only      : 12.224 millis, 2.30 c/b, rate= 693.25 mb/s

**/


// !!!!!!!!!!!!!!!!!!!!!!! OLD CTMF !!!!!!!!!!!!!!!!!!!!!!!

// newLZF_find_lo_match_ahead :
//	look for 4-byte LO match at (ptr+1)

template <typename t_hashtype,int t_table_depth_bits, int t_second_hash_len, int t_mml>
static RADFORCEINLINE bool newLZF_find_lo_match_ahead(match *pmatch,
	CTMF<t_hashtype,t_table_depth_bits,t_second_hash_len,t_mml> * ctmf,
	S32 lastoffset, const U8 * ptr, const U8 * ptr_matchend )
{
	typedef CTMF<t_hashtype,t_table_depth_bits,t_second_hash_len,t_mml> newLZF_CTMF;

	if ( RR_GET32_NATIVE(ptr+1) != RR_GET32_NATIVE(ptr+1-lastoffset) )
		return false;

	S32 lo_ml = getmatchlen_after4(ptr+1,ptr+1-lastoffset,ptr_matchend);

	pmatch->ml = lo_ml;
	pmatch->off = 0;

	// update hash :
	RR_ASSERT( ptr == ctmf->m_next_ptr );
	t_hashtype * row = ctmf->m_next_row;
	U32 hash = ctmf->m_next_hash;
	t_hashtype * row2 = ctmf->m_next_row_second;
	//U32 hash2 = ctmf->next_hash_second;

	// where to prefetch? sets m_next which will be used in the partial insert
	//ctmf->prefetch_next(ptr+1+lo_ml); // end - no
	ctmf->prefetch_next(ptr+2); // inside?
	//ctmf->prefetch_next(ptr+1); // about the same as ptr+2

	U32 cur_absolute_pos = (U32)rrPtrDiff(ptr - ctmf->m_base_ptr);

	// now update the hash rows :
	ctmf->insert(row,cur_absolute_pos,hash);
	if ( newLZF_CTMF::c_do_second_hash )
		ctmf->insert(row2,cur_absolute_pos,hash);

	return true;
}

template <typename t_hashtype,int t_table_depth_bits, int t_second_hash_len, int t_mml>
static RADFORCEINLINE bool newLZF_get_match_heuristic(match *pmatch,
	CTMF<t_hashtype,t_table_depth_bits,t_second_hash_len,t_mml> * ctmf,
	SINTa step, S32 lastoffset, const U8 * ptr, const U8 * ptr_matchend, int mml, const S32 offsLimitTab[32],
	S32 dictionarySize, const U8 * literals_start )
{
	typedef CTMF<t_hashtype,t_table_depth_bits,t_second_hash_len,t_mml> newLZF_CTMF;

	U32 ptr32 = RR_GET32_NATIVE_UNALIGNED(ptr);

	RR_ASSERT( ptr == ctmf->m_next_ptr );
	t_hashtype * row = ctmf->m_next_row;
	U32 hash = ctmf->m_next_hash;
	t_hashtype * row2 = ctmf->m_next_row_second;
	//U32 hash2 = ctmf->next_hash_second;
	
	ctmf->prefetch_next(ptr+step);
	
	U32 cur_absolute_pos = (U32)rrPtrDiff(ptr - ctmf->m_base_ptr);
	
	// check LOs
	// LO MML is 1 but I only look for mml2 matches :
	//	mml1 matches will be found in the LRL when I make packets

	U32 lo32 = RR_GET32_NATIVE(ptr-lastoffset);
	if ( ptr32 == lo32 ) // len 4
	{
		// NOTE : this is *never* hit with newLZF_find_lo_match_ahead
		//	(it *is* used in lazy parse mode to find long lo matches at current pos)
		
		// just take the LO match with no normal search :
		
		S32 lo_ml = getmatchlen_after4(ptr,ptr-lastoffset,ptr_matchend);
	
		// now update the hash rows :
		ctmf->insert(row,cur_absolute_pos,hash);
		if ( newLZF_CTMF::c_do_second_hash )
			ctmf->insert(row2,cur_absolute_pos,hash);
	
		pmatch->ml = lo_ml;
		pmatch->off = 0;
		return true;
	}
	
	S32 lo_ml = GetNumBytesZeroNeverAll32(ptr32^lo32);
	
	#if 1
	if ( ptr - literals_start >= NEWLZF_LRL_EXCESS )
	{
		#ifdef NEWLZF_IN_EXCESS_LRL_LOMML
		// longer LO MML if we're in an excess
		if ( lo_ml < NEWLZF_IN_EXCESS_LRL_LOMML ) lo_ml = 0;
		#endif

		#ifdef NEWLZF_IN_EXCESS_LRL_MML
		// longer MML if we're in an excess
		mml ++;
		#endif
	}
	#endif

	S32 bestml = mml-1;
	S32 bestoff = 0;
	const SINTa neg_offs_limit = -SINTa(dictionarySize);
	
	if ( sizeof(t_hashtype) == 2 )
	{
		// U16 row
		//	for newLZF_CTMF_Fast_64k special raw_len <= 64k mode
		//	skip IsAllowed and so on
		// -> surprisingly little win here ; less than 5% speed gain
		// -> not really worth it at the moment
		
		RR_ASSERT( ! newLZF_CTMF::c_do_second_hash );
		
		for(int d=0;d<newLZF_CTMF::c_table_depth;d++)
		{
			// no CTMF_CHECK_MASK in 16-bit row[]
			SINTa neg_offset = SINTa(row[d] - cur_absolute_pos) | ~0xffff; // see CTMF resolve_neg_offset for rationale
			if ( neg_offset <= neg_offs_limit ) // if user doesn't limit dictionarySize, this is never hit
				continue;
			
			// force all offsets >= 8
			neg_offset = RR_MIN(neg_offset,-NEWLZF_MIN_OFFSET);
			
			const U8 * vs_ptr = ptr + neg_offset;
			
			U32 vs32 = RR_GET32_NATIVE(vs_ptr);
			if ( ptr32 != vs32 )
				continue;
			
			S32 len = getmatchlen_after4(ptr,vs_ptr,ptr_matchend);													
			if ( len <= bestml )
				continue;
						
			RR_ASSERT( newLZF_IsAllowedNormalMatch(len,(int)-neg_offset,offsLimitTab) );
			
			bestml = len;
			bestoff = (S32)(-neg_offset);
		}
		
		// no explicit MIN_OFFSET check
		//	-> meh, pretty irrelevant whether to use the "skip" or "force" option
	}
	else
	{
		t_hashtype * cur_row = row;
		for(;;) // two hash rows
		{
			// not resetting prevml here is "wrong" ; 
			//  when you change to second hash, offset can get lower
			//	so offset monotonic assumption is broken
			//	and therefore a lower length match might be preferred
			// BUT seems to make no difference in practice
			//S32 prevml = mml-1; // so we beat it and thus have ml >= mml
		
			for(int d=0;d<newLZF_CTMF::c_table_depth;d++)
			{
				t_hashtype hash_entry = cur_row[d];

				// hash check helps speed a lot
				if ( (hash_entry ^ hash) & CTMF_CHECK_MASK )
					continue;
				
				SINTa neg_offset = newLZF_CTMF::resolve_neg_offset(hash_entry, cur_absolute_pos);

				// skip offsets <= 8 because we catch them after
				if ( neg_offset >= -NEWLZF_MIN_OFFSET ) continue;
				// in normal use this check is never taken :
				if ( neg_offset <= neg_offs_limit )
					continue;
									
				const U8 * vs_ptr = ptr + neg_offset;
				
				//S32 len = getmatchlen_mml4_one32(ptr32,ptr,vs_ptr,ptr_matchend);
				U32 vs32 = RR_GET32_NATIVE(vs_ptr);
				if ( ptr32 != vs32 )
					continue;
				
				S32 len = getmatchlen_after4(ptr,vs_ptr,ptr_matchend);													
				if ( len <= bestml )
					continue;

				S32 offset = S32(-neg_offset);
								
				// @@ completely removing newLZF_IsNormalMatchBetter here is not a major penalty
				//	(BTW be careful of checking its effect on decode speed too)
				if ( newLZF_IsAllowedNormalMatch(len,offset,offsLimitTab) &&
					 ( newLZF_CTMF::c_table_depth == 1 || newLZF_IsNormalMatchBetter(len,offset,bestml,bestoff) ) )
				{
					bestml = len;
					bestoff = offset;
				}
			}
			
			if ( ! newLZF_CTMF::c_do_second_hash )
				break;
				
			if ( cur_row == row2 )
				break;
			
			cur_row = row2;
		}
		
		// explicit always check at MIN_OFFSET :
		#if 1
		//if ( bestml == 0 )
		{
			const U8 * vs_ptr = ptr - NEWLZF_MIN_OFFSET;
			
			//S32 len = getmatchlen_mml3_one32(ptr32,ptr,vs_ptr,ptr_matchend);
			S32 len = getmatchlen_mml4_one32(ptr32,ptr,vs_ptr,ptr_matchend);
			// "mml" var is 4 here anyway, was just a leftover mistake to use mml3 matcher
									
			if ( len >= bestml && len >= mml )
			// idea : require len > bestml to make it slightly less likely to take this match :
			//	must beat one we already found, even if it's a large offset
			// -> no this hurts compression a decent amount
			//if ( len > bestml && len >= mml )
			{
				bestml = len;
				bestoff = NEWLZF_MIN_OFFSET;
				//bestm.pos = pos;
			}
		}
		#endif
	}	
	
	RR_ASSERT( bestml < mml || newLZF_IsAllowedNormalMatch(bestml,bestoff,offsLimitTab) );
	
	// now update the hash rows :
	ctmf->insert(row,cur_absolute_pos,hash);
	if ( newLZF_CTMF::c_do_second_hash )
		ctmf->insert(row2,cur_absolute_pos,hash);
	
	// the LO match :	
		
	if ( bestoff == 0 )
	{
		// no normal match
	
		if ( lo_ml >= NEWLZF_LOMML_NORMAL )
		{
			// take the LO match :
			pmatch->ml = lo_ml;
			pmatch->off = 0;
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
	
		RR_ASSERT( bestml >= mml );
		RR_ASSERT( lo_ml <= 3 );
	
		if ( newLZF_IsLOMatchBetter(lo_ml,bestml,bestoff) )
		{
			// take the LO match :
			pmatch->ml = lo_ml;
			pmatch->off = 0;
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

// !!!!!!!!!!!!!!!!!!!!!!! NEW CTMF !!!!!!!!!!!!!!!!!!!!!!!

template <typename t_hash1_type,int t_mml1,int t_mml2,int t_chain_steps_max>
static RADFORCEINLINE bool newLZF_find_lo_match_ahead(match *pmatch,
	CTMF2<t_hash1_type,t_mml1,t_mml2,t_chain_steps_max> * ctmf,
	S32 lastoffset, const U8 * ptr, const U8 * ptr_matchend )
{
	typedef CTMF2<t_hash1_type,t_mml1,t_mml2,t_chain_steps_max> newLZF_CTMF;

	if ( RR_GET32_NATIVE(ptr+1) != RR_GET32_NATIVE(ptr+1-lastoffset) )
		return false;

	S32 lo_ml = getmatchlen_after4(ptr+1,ptr+1-lastoffset,ptr_matchend);

	pmatch->ml = lo_ml;
	pmatch->off = 0;

	// update hash :

	UINTa h1 = ctmf->hash1(ptr);
	ctmf->insert1(ptr,h1);

	return true;
}

template <typename t_hash1_type,int t_mml1,int t_mml2,int t_chain_steps_max>
static RADFORCEINLINE bool newLZF_get_match_heuristic(match *pmatch,
	CTMF2<t_hash1_type,t_mml1,t_mml2,t_chain_steps_max> * ctmf,
	SINTa step, S32 lastoffset, const U8 * ptr, const U8 * ptr_matchend, int mml, const S32 offsLimitTab[32],
	S32 dictionarySize, const U8 * literals_start )
{
	typedef CTMF2<t_hash1_type,t_mml1,t_mml2,t_chain_steps_max> newLZF_CTMF;
	const bool c_is_64k = sizeof(t_hash1_type) == 2;
	
	U32 ptr32 = RR_GET32_NATIVE_UNALIGNED(ptr);

	//RR_ASSERT( ptr == ctmf->m_next_ptr );
	RR_ASSERT( ptr >= ctmf->m_next_ptr );
	UINTa h1 = ctmf->hash1(ptr);

	U32 * h2ptr;
	U32 h2check;
	if ( newLZF_CTMF::c_do_hash2 )
	{
		UINTa h2 = ctmf->hash2(ptr,h2check);	
		h2ptr = ctmf->m_hash2.data() + h2;

		// prefetch at h2 ? definitely helps
		// NOTE(fg): now doing the ptr+step prefetch instead
		//RR_PREFETCHR_CL(h2ptr);
	}

	// prefetch ahead hash1 and 2 at ptr+step
	// -> Jaguar says yes
	{
		UINTa h1n = ctmf->hash1(ptr + step);

		RR_PREFETCHRW_CL(ctmf->m_hash1.data() + h1n);
		if ( newLZF_CTMF::c_do_hash2 )
		{
			U32 h2checkn;
			UINTa h2n = ctmf->hash2(ptr + step, h2checkn);
			RR_PREFETCHR_CL(ctmf->m_hash2.data() + h2n);
		}
	}

	UINTa cur_absolute_pos = rrPtrDiff(ptr - ctmf->m_base_ptr);
	
	// check LOs
	// LO MML is 1 but I only look for mml2 matches :
	//	mml1 matches will be found in the LRL when I make packets

	U32 lo32 = RR_GET32_NATIVE(ptr-lastoffset);
	if ( ptr32 == lo32 ) // len 4
	{
		// just take the LO match with no normal search :
		
		S32 lo_ml = getmatchlen_after4(ptr,ptr-lastoffset,ptr_matchend);
	
		// just take the LO match with no normal search :
		pmatch->ml = lo_ml;
		pmatch->off = 0;
		
		// now update the hash rows :
		ctmf->insert1(ptr,h1);
		//ctmf->insert2(ptr);
	
		return true;
	}
	
	S32 lo_ml = GetNumBytesZeroNeverAll32(ptr32^lo32);
	
	if ( ptr - literals_start >= NEWLZF_LRL_EXCESS )
	{
		#ifdef NEWLZF_IN_EXCESS_LRL_LOMML
		// longer MML if we're in an excess
		if ( lo_ml < NEWLZF_IN_EXCESS_LRL_LOMML ) lo_ml = 0;
		#endif

		#ifdef NEWLZF_IN_EXCESS_LRL_MML
		// longer MML if we're in an excess
		mml ++;
		#endif
	}
	
	//-----------
	// hash chain walk
	
	int bestml = 0; // @@ mml-1 ?
	int bestoff = 0;
	
	U32 hpos = ctmf->m_hash1[ h1 ];
	// previous version had m_hash1 as a U32
	//	which allowed you to get a position outside the 64k window
	
	// first offset check :
	// if m_hash1 is U32 (not U16) then hpos may be beyond the chain
	//	-> this makes a huge difference on "records7"
	if ( ! newLZF_CTMF::c_do_chain ||
		( ! c_is_64k &&
		(U32)(cur_absolute_pos - hpos) > 0xFFFF ) )
	{
		S32 offset = (t_hash1_type)(cur_absolute_pos - hpos);
		
		if ( ! newLZF_CTMF::c_do_chain && offset < NEWLZF_MIN_OFFSET )
			offset = NEWLZF_MIN_OFFSET;
					
		const U8 * vs_ptr = ptr - offset;
		RR_ASSERT( vs_ptr >= ctmf->m_base_ptr );
		
		if ( offset < dictionarySize )
		{		
			// off24 zone
			// off24mml is 10 for Mermaid
				
			S32 len = getmatchlen_mml4_one32(ptr32,ptr,vs_ptr,ptr_matchend);
				
			//if ( len >= off24mml )
			//if ( len >= 8 ) // we might make up some len with backup
			if ( len >= mml )
			{
				// earlier version did backup here
							
				if ( c_is_64k )
				{
					// always allowed
					RR_ASSERT( offset < 0xFFFF );
					RR_ASSERT( newLZF_IsAllowedNormalMatch(len,offset,offsLimitTab) );
					
					bestml = len;
					bestoff = offset;
				}
				else
				{			
					if ( newLZF_IsAllowedNormalMatch(len,offset,offsLimitTab) )
					{
						bestml = len;
						bestoff = offset;
					}
				}
			}	
		}			
	}
	else
	{
		// c_chain_steps_max passed through template param
		//	just as a hacky way of getting it through for different levels
		int chain_steps_max = newLZF_CTMF::c_chain_steps_max;
		S32 prevoffset = 0;
		RR_ASSERT( ctmf->m_chain_mask == 0xFFFF );
		
		for(;;)
		{
			S32 offset = (U16)(cur_absolute_pos - hpos);
			// offset is U16 ; it's always < 64k
			//	if dictionarySize is >= 64k the check of offset vs dictionarySize is always true
			
			// we should be stepping in increasing offset order :
			//	if we go backwards it means we looped the chain
			if ( offset <= prevoffset || offset >= dictionarySize )
				break;
			prevoffset = offset;

			// only match valid offsets, but step over low ones		
			//	(don't check offset == NEWLZF_MIN_OFFSET , we do that outside)
			if ( offset > NEWLZF_MIN_OFFSET )
			{
				const U8 * vs_ptr = ptr - offset;
				RR_ASSERT( vs_ptr >= ctmf->m_base_ptr );
				
				// offset should be strictly increasing
				// so require length increasing too
				// offset is <= 0xFFFF through use of U16
									
				S32 len = getmatchlen_mml4_one32_better(ptr32,ptr,vs_ptr,ptr_matchend,bestml);
					
				if ( len >= mml )
				{
					RR_ASSERT( len > bestml ); // cuz of _better matcher
				
					// should be true :
					RR_ASSERT( newLZF_IsAllowedNormalMatch(len,offset,offsLimitTab) );
					RR_ASSERT( newLZF_IsNormalMatchBetter(len,offset,bestml,bestoff) );

					bestml = len;
					bestoff = offset;
				}
			
				// only dec chain_steps_max when we actually test a match	
				chain_steps_max--;
				if ( chain_steps_max <= 0 )
					break;
			}
			
			// step to next :
			hpos = ctmf->m_chain[ (U16)hpos ];
		}
	}
	
	//-----------
	// now check hash 2
	
	if ( newLZF_CTMF::c_do_hash2 )
	{
		hpos = *h2ptr;
	
		// optional check for uninit hash entry :
		//if ( hpos != 0 )
		// hash check instead (also catches uninit probabilistically)
		//if ( (U8)hpos == h2check )
		if ( ((hpos^h2check)&CTMF2_HASH_CHECK_MASK) == 0 )
		{	
			hpos >>= CTMF2_HASH_CHECK_BITS;
			S32 offset = (U32)(cur_absolute_pos - hpos);

			// only match valid offsets, but step over low ones			
			if ( offset >= NEWLZF_MIN_OFFSET && offset < dictionarySize )
			{
				const U8 * vs_ptr = ptr - offset;
				RR_ASSERT( vs_ptr >= ctmf->m_base_ptr );
				
				// off24 zone
				S32 len = getmatchlen_mml4_one32(ptr32,ptr,vs_ptr,ptr_matchend);
				
				if ( len )
				{
					if ( newLZF_IsAllowedNormalMatch(len,offset,offsLimitTab) &&
						 newLZF_IsNormalMatchBetter(len,offset,bestml,bestoff) )
					{
						bestml = len;
						bestoff = offset;
					}
				}
			}
		}
	}
	
	// when do_chain is on, offsets < MIN are skipped, so we do an extra check here
	// when do_chain is off, offset < MIN are pushed up to MIN and checked above
	if ( newLZF_CTMF::c_do_chain )
	{
		// explicit always check at MIN_OFFSET :
		const U8 * vs_ptr = ptr - NEWLZF_MIN_OFFSET;
		
		//S32 len = getmatchlen_mml3_one32(ptr32,ptr,vs_ptr,ptr_matchend);
		S32 len = getmatchlen_mml4_one32(ptr32,ptr,vs_ptr,ptr_matchend);
		// "mml" var is 4 here anyway, was just a leftover mistake to use mml3 matcher
								
		if ( len >= bestml && len >= mml )
		{
			bestml = len;
			bestoff = NEWLZF_MIN_OFFSET;
			//bestm.pos = pos;
		}
	}
	
	RR_ASSERT( bestml == 0 || newLZF_IsAllowedNormalMatch(bestml,bestoff,offsLimitTab) );
	
	// now update the hash rows :
	ctmf->insert1(ptr,h1);
	//ctmf->insert2(ptr); // @@@@!! ?? no insert2 ?
	
	// the LO match :	
		
	if ( newLZF_IsLOMatchBetter(lo_ml,bestml,bestoff) )
	{
		// take the LO match :
		pmatch->ml = lo_ml;
		pmatch->off = 0;
		return true;
	}
	else
	{
		if ( bestml == 0 )
			return false;
			
		// normal match :
		pmatch->ml = bestml;
		pmatch->off = bestoff;
		return true;
	}
}

//=================================================================

static RADFORCEINLINE U8 * newlzf_putv(U8 * to, U32 val)
{
	RR_ASSERT( val <= (1<<18) );
	if( val <= 251 )
	{
		*to++ = U8_check( val );
	}
	else
	{
		// 252 == 11111100 == 256 - 4
		U32 lo = 252 + (val&3);
		RR_ASSERT( lo == (U8)(252|val) );
		U32 hi = (val - lo)>>2;
		RR_ASSERT( val == lo + (hi<<2) );
		*to++ = (U8)lo;
		RR_PUT16_LE(to,U16_check(hi));
		to += 2;
	}
	return to;
}

static RADFORCEINLINE S32 newlzf_countbytesv(U32 val)
{
	RR_ASSERT( val <= (1<<18) );
	if( val <= 251 )
	{
		return 1;
	}
	else
	{
		return 3;
	}
}


struct newLZF_passinfo
{
	U32	literal_histo[256];
	U32	packet_histo[256];
	U32	offsetL_histo[256];
	U32	offsetH_histo[256];
};


struct newLZF_encoder_arrays
{
	U8 * scratch_base;

	U8 * literals_space_raw;
	U8 * literals_ptr_raw;
	
	U8 * literals_space_sub; // can be NULL
	U8 * literals_ptr_sub;
	
	U8 * packets_space;
	U8 * packets_ptr;
	
	U8 * offsets16_space;
	U8 * offsets16_ptr;
	
	U8 * offsets24_space;
	U8 * offsets24_ptr;
	
	U8 * excesses_space;
	U8 * excesses_ptr;
	U8 * excesses_ptr_end;
	
	int speedstat_escapes;
	//int speedstat_offset_blend;
	int off24_chunk_count;
	
	
	int whole_chunk_len;
	const U8 * whole_chunk_ptr;
	
	int parse_chunk_base_pos; // 0 or 64k for current parse chunk	
	int parse_chunk_len;
	const U8 * parse_chunk_ptr;
	int parse_chunk_start_pos;
	
	int parse_chunk_len1;
	int parse_chunk_len2;
	
	int packet_count1;
	int off24_ptr_chunk_count1;
	int off24_ptr_chunk_count2;
	
};

static void newLZF_encoder_arrays_point_to_scratch(
	newLZF_encoder_arrays * encarrays,
	newlz_encoder_scratch * scratch,
	int whole_chunk_len,
	const U8 * whole_chunk_ptr,
	bool do_sub
	)
{
	rrArenaAllocator * arena = scratch->arena;
	
	/**
	
	literals    : chunk_len
	packets     : (chunk_len/2)
		the max # of packets occurs when every packet is
		{lrl1 rep len 1}
		(think 1 block of random data with no zeros, then a copy of that block
		with every 2nd byte after the first few bytes zeroed)
	offsets U16 : (chunk_len/4)*2
	excesses    : very few ?
	
	**/
	
	encarrays->speedstat_escapes = 0;
//	encarrays->speedstat_offset_blend = 0;
	encarrays->off24_chunk_count = 0;
	
	encarrays->whole_chunk_ptr = whole_chunk_ptr;
	encarrays->whole_chunk_len = whole_chunk_len;
	
	// reserve the maximum possible array size for each stream :
	
	int scratch_space_literals = whole_chunk_len + 8; // + 8 for sloppy copy
	int scratch_space_packets  = whole_chunk_len/2 + 8; // {rep1, lrl1} = packet per two bytes worst case
	int scratch_space_offsets16 = (whole_chunk_len/3)*2; // MML 3 , 2 bytes each
	int scratch_space_offsets24 = (whole_chunk_len/8)*4; // MML 8 , 4 bytes each
	int scratch_space_excesses = whole_chunk_len/29; // the most excesses case comes from off24 with ml just out of the in-packet range; see escape_match_transmission
	
	// add a bit of slack just in case there's a bug and we overrun
	//	(this is checked with asserts)
	#define SCRATCH_SLACK	256
	int scratch_space_needed = scratch_space_literals + scratch_space_packets + scratch_space_offsets16 + scratch_space_offsets24 + scratch_space_excesses + SCRATCH_SLACK;

	if ( do_sub )
		scratch_space_needed += scratch_space_literals;

	scratch->newlzf_arrays_space.extend(scratch_space_needed,arena);
	U8 * scratch_space = scratch->newlzf_arrays_space.getU8();
	
	encarrays->scratch_base = scratch_space;
	
	U8 * scratch_ptr = scratch_space;
	
	// @@ selkie reserves scratch space for literals then doesn't use it
	
	encarrays->literals_space_raw = scratch_ptr;
	encarrays->literals_ptr_raw = scratch_ptr;
	scratch_ptr += scratch_space_literals;
	
	if ( do_sub )
	{
		encarrays->literals_space_sub = scratch_ptr;
		encarrays->literals_ptr_sub = scratch_ptr;
		scratch_ptr += scratch_space_literals;
	}
	else
	{
		encarrays->literals_space_sub = NULL;
		encarrays->literals_ptr_sub = NULL;
	}
	
	encarrays->packets_space = scratch_ptr;
	encarrays->packets_ptr = scratch_ptr;
	scratch_ptr += scratch_space_packets;
	
	encarrays->offsets16_space = scratch_ptr;
	encarrays->offsets16_ptr = scratch_ptr;
	scratch_ptr += scratch_space_offsets16;
	
	encarrays->offsets24_space = scratch_ptr;
	encarrays->offsets24_ptr = scratch_ptr;
	scratch_ptr += scratch_space_offsets24;
	
	encarrays->excesses_space = scratch_ptr;
	encarrays->excesses_ptr = scratch_ptr;
	scratch_ptr += scratch_space_excesses;

	encarrays->excesses_ptr_end = scratch_ptr;
	
	scratch_ptr += SCRATCH_SLACK;
	
	RR_ASSERT( scratch_ptr == scratch_space + scratch_space_needed );	
	
	encarrays->parse_chunk_len1 = RR_MIN(whole_chunk_len,(1<<16));
	encarrays->parse_chunk_len2 = whole_chunk_len - encarrays->parse_chunk_len1;
	
	encarrays->packet_count1 = 0;
	encarrays->off24_ptr_chunk_count1 = 0;
	encarrays->off24_ptr_chunk_count2 = 0;
}

// returns false on the second chunk if its null
static bool newLZF_encoder_arrays_start_parse_chunk(
	newLZF_encoder_arrays * encarrays,
	int which,
	int whole_start_pos,
	U8cptr & ptr_matchend_ref)	
{
	if ( which==0 )
	{
		RR_ASSERT( whole_start_pos== 0 || whole_start_pos == 8);
	
		encarrays->parse_chunk_start_pos = whole_start_pos;
		encarrays->parse_chunk_len = encarrays->parse_chunk_len1;
		encarrays->parse_chunk_ptr = encarrays->whole_chunk_ptr;
		encarrays->parse_chunk_base_pos = 0;
	}
	else
	{
		encarrays->packet_count1 = rrPtrDiff32( encarrays->packets_ptr - encarrays->packets_space );
		
		if ( encarrays->parse_chunk_len2 == 0 ) return false;
		
		encarrays->parse_chunk_start_pos = encarrays->parse_chunk_len1;
		encarrays->parse_chunk_len = encarrays->parse_chunk_len2;
		encarrays->parse_chunk_ptr = encarrays->whole_chunk_ptr + encarrays->parse_chunk_len1;
		encarrays->parse_chunk_base_pos = encarrays->parse_chunk_len1;	
	}

	encarrays->off24_chunk_count = 0;
	
	// ptr_matchend is the match *end* limit
	U8cptr ptr_matchend = encarrays->whole_chunk_ptr + encarrays->whole_chunk_len - NEWLZF_CHUNK_MATCH_SAFE_ZONE;
	ptr_matchend = RR_MIN(ptr_matchend,encarrays->parse_chunk_ptr + encarrays->parse_chunk_len);
		
	ptr_matchend_ref = ptr_matchend;
		
	return true;
}

static void newLZF_encoder_arrays_finish_parse_chunk(
	newLZF_encoder_arrays * encarrays,
	int which,
	const U8 * literals_start,SINTa lastoffset)
{
	// write final sequence
	// trailing LRL :
	SINTa final_lrl = encarrays->parse_chunk_ptr + encarrays->parse_chunk_len - literals_start;
	RR_ASSERT( final_lrl >= 0 && final_lrl <= encarrays->parse_chunk_len );

	// check : always have a final LRL of NEWLZF_CHUNK_MATCH_SAFE_ZONE at the tail :
	RR_ASSERT( literals_start + NEWLZF_CHUNK_MATCH_SAFE_ZONE <= encarrays->whole_chunk_ptr + encarrays->whole_chunk_len || encarrays->parse_chunk_len < NEWLZF_CHUNK_MATCH_SAFE_ZONE );
	RR_ASSERT( final_lrl >= NEWLZF_CHUNK_MATCH_SAFE_ZONE || encarrays->parse_chunk_base_pos + encarrays->parse_chunk_len != encarrays->whole_chunk_len || encarrays->parse_chunk_len < NEWLZF_CHUNK_MATCH_SAFE_ZONE );

	if ( final_lrl > 0 )
	{
		// put the literals :
		// careful tail
		memcpy(encarrays->literals_ptr_raw,literals_start,final_lrl);
		encarrays->literals_ptr_raw += final_lrl;
		
		if ( encarrays->literals_ptr_sub )
		{
			put_sub_literals(encarrays->literals_ptr_sub,literals_start,final_lrl,lastoffset);
			encarrays->literals_ptr_sub += final_lrl;
		}
	}
			
	if ( which == 0 )
		encarrays->off24_ptr_chunk_count1 = encarrays->off24_chunk_count;
	else
		encarrays->off24_ptr_chunk_count2 = encarrays->off24_chunk_count;
}

static RADFORCEINLINE void escape_match_transmission(int offset,int ml,
	int * p_packet,
	int * p_escape_ml)
{
	if ( offset <= 0xFFFF )
	{
		RR_ASSERT( ml >= NEWLZF_ML_EXCESS );
		*p_packet = 1;
		*p_escape_ml = ml - NEWLZF_ML_EXCESS;
	}
	else
	{
		RR_ASSERT( ml >= NEWLZF_OFF24_MML_DECODE );
		
		int packet = ml - NEWLZF_OFF24_MML_DECODE + 3;
		if ( packet > 23 )
		{
			*p_packet = 2;
			*p_escape_ml = packet-24;
		}
		else
		{
			*p_packet = packet;
			*p_escape_ml = -1;
		}
	}
}

static void newLZF_encoder_arrays_put_packet_complex(
	newLZF_encoder_arrays * encarrays,
	int ml, int lrl, SINTa codeoffset, SINTa lastoffset,
	const U8 * literals_start)
{
	// complex

	// need to save this before we modify lrl :
	const U8 * match_ptr = literals_start + lrl;
	
	// put the literals :
	if ( encarrays->literals_ptr_sub )
	{
		put_sub_literals_sloppy(encarrays->literals_ptr_sub,literals_start,lrl,lastoffset);
		encarrays->literals_ptr_sub += lrl;
	}
	
	// (note : literals_start modified by this too)
	lz_copywordsteptoend_overrunok(encarrays->literals_ptr_raw,literals_start,lrl);
	// literals_ptr_raw advanced by lrl
		
	if ( lrl >= NEWLZF_LRL_EXCESS )
	{
		int lrl_excess = lrl - NEWLZF_LRL_EXCESS;
		encarrays->excesses_ptr = newlzf_putv(encarrays->excesses_ptr,lrl_excess);
		RR_ASSERT( encarrays->excesses_ptr < encarrays->excesses_ptr_end );
		
		encarrays->speedstat_escapes++;
		
		// send escape 0 packet :
		*encarrays->packets_ptr++ = 0;

		// send ml in next packet :
		lrl = 0;
		// optimal can make zero-ml packets :
		//RR_ASSERT( ml != 0 );
		if ( ml == 0 )
			return;
	}
	else
	{
		while( lrl > NEWLZF_PACKET_LRL_MAX )
		{
			*encarrays->packets_ptr++ = NEWLZF_PACKET_LRL_MAX | NEWLZF_PACKET_LO_FLAG;
		
			lrl -= NEWLZF_PACKET_LRL_MAX;
		}
	}
	
	// lrl can now fit in match packet :
	RR_ASSERT( lrl <= NEWLZF_PACKET_LRL_MAX );

	bool islo = (codeoffset == 0 );
	bool isescapematch = is_escape_match(codeoffset,ml);
		
	if ( isescapematch )
	{
		encarrays->speedstat_escapes++;
		
		// if I have any LRL, first send an LO packet with it
		if ( lrl > 0 )
		{
			int packet = lrl | NEWLZF_PACKET_LO_FLAG;
			
			*encarrays->packets_ptr++ = U8_check(packet);
		}
		
		// I send the true offset, even if it was a rep
		//	there's no long-rep escape (bit of a waste)
		SINTa offset = (codeoffset == 0) ? lastoffset : codeoffset;
		
		// now send the special packet :
		
		int packet,escape_ml;
		escape_match_transmission((int)offset,ml,&packet,&escape_ml);
				
		*encarrays->packets_ptr++ = U8_check(packet);
			
		if ( escape_ml >= 0 )
		{
			encarrays->excesses_ptr = newlzf_putv(encarrays->excesses_ptr,escape_ml);
			RR_ASSERT( encarrays->excesses_ptr < encarrays->excesses_ptr_end );
		}
						
		RR_ASSERT( offset >= NEWLZF_MIN_OFFSET && offset <= NEWLZF_MAX_OFFSET );
		
		if ( offset <= 0xFFFF )
		{
			RR_PUT16_LE(encarrays->offsets16_ptr,U16_checkA(offset));
			encarrays->offsets16_ptr += 2;
		}
		else
		{				
			// old/wrong :
			//encarrays->speedstat_offset_blend += newLZ_offset_blend_score(offset);
		
			// cur_pos is match start pos (relative to whole chunk base)
			SINTa cur_pos = rrPtrDiff( match_ptr - encarrays->whole_chunk_ptr );
			SINTa match_from_pos = cur_pos - offset;
			
			// store offset relative to parse chunk base :
			// (parse_chunk_base_pos is relative to whole chunk base)
			//	alternative : (parse_chunk_ptr - match_from_ptr)
			SINTa store_offset = encarrays->parse_chunk_base_pos - match_from_pos;
			RR_ASSERT( store_offset >= 0 && store_offset < OODLELZ_MAX_OFFSET );
			
			// match what decoder/speedfitter does - run offset_blend_score on the stored offset
			//encarrays->speedstat_offset_blend += newLZ_offset_blend_score(store_offset) + 1;
			
			if ( store_offset < NEWLZF_OFFSET_FOURBYTE_THRESHOLD )
			{					
				RR_PUT24_LE_NOOVERRUN(encarrays->offsets24_ptr,(U32)store_offset);
				encarrays->offsets24_ptr += 3;
			}
			else
			{
				U32 lo = NEWLZF_OFFSET_FOURBYTE_THRESHOLD | (U32)store_offset;
				lo &= 0xFFFFFF;
				RR_ASSERT( lo == (NEWLZF_OFFSET_FOURBYTE_THRESHOLD + ((U32)store_offset & NEWLZF_OFFSET_FOURBYTE_MASK)) );
				RR_PUT24_LE_NOOVERRUN(encarrays->offsets24_ptr,(U32)lo);
				encarrays->offsets24_ptr += 3;
				RR_ASSERT( NEWLZF_OFFSET_FOURBYTE_SHIFT == 22 );
				U32 hi = ((U32)store_offset - lo) >> NEWLZF_OFFSET_FOURBYTE_SHIFT;
				RR_ASSERT( hi < 256 );
				*(encarrays->offsets24_ptr)++ = (U8) hi;
			}
			encarrays->off24_chunk_count++;
		}
		
	}
	else // not escapematch
	{
		RR_ASSERT( ml > 0 || lrl > 0 );
	
		int ml_for_packet = (ml > NEWLZF_PACKET_ML_MAX) ? NEWLZF_PACKET_ML_MAX : ml;
		int packet = (ml_for_packet * NEWLZF_PACKET_LRL_COUNT) + lrl;
		
		if ( islo )
		{
			// LO
			packet += (NEWLZF_PACKET_LO_FLAG);
		}
		else
		{
			RR_PUT16_LE(encarrays->offsets16_ptr,U16_checkA(codeoffset));
			encarrays->offsets16_ptr += 2;
		
			RR_ASSERT( ml >= 3 );
		}

		RR_ASSERT( packet >= 24 );
		
		*encarrays->packets_ptr++ = U8_check(packet);
			
		ml -= ml_for_packet;
			
		// more ML :
		while( ml > 0 )
		{
			// more ML :
			ml_for_packet = (ml > NEWLZF_PACKET_ML_MAX) ? NEWLZF_PACKET_ML_MAX : ml;
			ml -= ml_for_packet;
			packet = (ml_for_packet * NEWLZF_PACKET_LRL_COUNT) + NEWLZF_PACKET_LO_FLAG;
			*encarrays->packets_ptr++ = U8_check(packet);
		}
	}	
}
	
		
// @@ only the simple case needs to be FORCEINLINE, maybe factor out complex and make it non inline?
static RADFORCEINLINE void newLZF_encoder_arrays_put_packet(
	newLZF_encoder_arrays * encarrays,
	int ml, int lrl, SINTa codeoffset, SINTa lastoffset,
	const U8 * literals_start)
{
	// codeoffset == 0 for LO
	
	bool islo = (codeoffset == 0 );
	bool is_high_offset = codeoffset > 0xFFFF; // offset == 0 is LO , so LO high offsets are not in here
	RR_ASSERT( islo || memcmp(literals_start+lrl,literals_start+lrl-codeoffset,ml) == 0 );
	RR_ASSERT( !islo || memcmp(literals_start+lrl,literals_start+lrl-lastoffset,ml) == 0 );

	if ( lrl <= NEWLZF_PACKET_LRL_MAX && ml <= NEWLZF_PACKET_ML_MAX && !is_high_offset )
	{
		// simple
		lz_copy8(encarrays->literals_ptr_raw,literals_start);
		encarrays->literals_ptr_raw += lrl;
		
		if ( encarrays->literals_ptr_sub )
		{
			put_sub_literals8(encarrays->literals_ptr_sub,literals_start,lrl,lastoffset);
			encarrays->literals_ptr_sub += lrl;
		}
		
		int packet = ml * NEWLZF_PACKET_LRL_COUNT + lrl;
		//if ( islo ) packet += NEWLZF_PACKET_LO_FLAG;
		packet |= (int)islo << 7;
		
		*encarrays->packets_ptr++ = U8_check(packet);
		
		if ( ! islo )
		{
			RR_ASSERT( ml >= 3 );
			
			RR_PUT16_LE(encarrays->offsets16_ptr,U16_checkA(codeoffset));
			encarrays->offsets16_ptr += 2;
		}
	}
	else
	{		
		newLZF_encoder_arrays_put_packet_complex(encarrays,ml,lrl,codeoffset,lastoffset,literals_start);
	}
}


/**

do_lomml1_for_lrl

called with a "parse" for a packet that's an LRL -> ML
 (ML >= 2)
 
do_lomml1_for_lrl scans over the literals in the LRL looking for rep0 len1 matches

it then tries to output the LRL advantageously as {LRL,rep0len1} packets where it can

the goal :
#1. output the minimum number of packets
#2. for that packet count, output the minimum number of bytes

any rep0 packets made are added to parsevec & histo here
the match packet (p_cur_parse) is modified to shorten its lrl
p_cur_parse is not added here, it will be done outside

if the match packet (p_cur_parse) is an escape match,
then it can't contain any LRL
so I want to end in a rep0 if possible
if it's not an escape match, it can take LRL
so ending in a rep is usually not advantageous

***/

static RADINLINE int lrl_remainder(int lrl)
{
// alternative form
// avoids loop that's could be bad for long lrl
// (lrl is called at NEWLZF_LRL_EXCESS now, so maybe the loop version is better?) meh

#if 1
	if ( lrl <= 7 ) return lrl;
	return ((lrl-1)%7) + 1;
#else
	while ( lrl > 7 ) lrl -= 7;
	return lrl;
#endif

}

/*
static int lrl_over7_packet_count(int lrl)
{
	int ret = 0;
	while ( lrl > 7 ){ ret ++; lrl -= 7; }
	return ret;
}
*/

/*
do_lomml1_for_lrl : call instead of newLZF_encoder_arrays_put_packet
*/
static void do_lomml1_for_lrl(
	newLZF_encoder_arrays * encarrays,
	int ml, int whole_lrl, SINTa codeoffset, SINTa lo,
	const U8 * litptr)
{
	RR_ASSERT( whole_lrl < NEWLZF_LRL_EXCESS );
	RR_ASSERT( whole_lrl >= 8 && whole_lrl < NEWLZF_LRL_EXCESS );
		
	// the +1 at the end here is unnecessary I believe but be safe :
	#define LRL_TO_REP_SIZE	(((NEWLZF_LRL_EXCESS+1)/2)+1)
	int lrl_to_rep[LRL_TO_REP_SIZE];
	int lrl_to_rep_count = 0;
	
	int i_lrl_start = 0;
	
	#ifdef __RADSSE2__
	// this is a speed NOP but WTF do it anyway
	//encode (x5)      : 810.220 millis, 56.72 c/b, rate= 30.49 mb/s
	//encode (x5)      : 812.238 millis, 56.86 c/b, rate= 30.41 mb/s

	// could do chunks of 8 or 16 simd style to find LO matches here
	//  whole_lrl is >= 8 so could at least do an early-out check for any LO matches at all
	// NEWLZF_CHUNK_MATCH_SAFE_ZONE is 16 so I can safely grab 16 off end here
	
	{
		for(int i = 1;i<whole_lrl;)
		{
			__m128i cur = _mm_loadu_si128((__m128i *)(litptr+i));
			__m128i vs  = _mm_loadu_si128((__m128i *)(litptr+i-lo));
			__m128i cmp = _mm_cmpeq_epi8(cur,vs);
			U32 cmpmsk = _mm_movemask_epi8(cmp);
			if ( cmpmsk == 0 )
			{
				i += 16;
				continue;
			}
			U32 step = rrCtz32(cmpmsk);
			i += step;
			RR_ASSERT( litptr[i] == litptr[i-lo] );
			// step could have taken us past end
			if ( i >= whole_lrl )
				break;
			int cur_lrl = i - i_lrl_start;
			if ( cur_lrl == 0 )
			{
				i++;
				continue;
			}
			
			RR_ASSERT( lrl_to_rep_count < LRL_TO_REP_SIZE );

			lrl_to_rep[lrl_to_rep_count] = cur_lrl;
			lrl_to_rep_count++;

			i++;			
			i_lrl_start = i;
		}
	}
	
	#else
	
	// start i at 1 , don't look for rep at head (should never be there)
	for(int i=1;i<whole_lrl;i++)
	{
		const U8 * ptr = litptr + i;
		if ( *ptr == ptr[-lo] )
		{
			int cur_lrl = i - i_lrl_start;
			
			// cur_lrl == 0 generally shouldn't happen
			// because no two adjacent bytes can be rep0 matches
			//	(I would have had an mml2 rep match)
			// and I can't have a rep0 right after a match
			//	(the match would have just been longer)
			// the exception is at the start of the chunk
			// @@ this assert is true in greedy parse, but not true in optimal parse :
			//	(since optimal can decide not to take a rep0 match due to cost)
			//RR_ASSERT( cur_lrl != 0 || p_cur_parse->literals_start == 0 );
			// @@ allow lrl 0 ? (if you do, lrl_to_rep[] array has to be bigger)
			if ( cur_lrl == 0 ) continue;

			RR_ASSERT( lrl_to_rep_count < LRL_TO_REP_SIZE );

			lrl_to_rep[lrl_to_rep_count] = cur_lrl;
			lrl_to_rep_count++;
			// start after me :
			i_lrl_start = i + 1;
		}
	}
	
	#endif
	
	if ( lrl_to_rep_count == 0 )
	{
		// nothing to do, just put the original packet :
	
		newLZF_encoder_arrays_put_packet(encarrays,ml,whole_lrl,codeoffset,lo,litptr);
	
		return;
	}

	RR_ASSERT( lrl_to_rep_count < LRL_TO_REP_SIZE );
			
	// put the tail lrl in for use as next_lrl, but don't inc lrl_to_rep_count :
	lrl_to_rep[lrl_to_rep_count] = whole_lrl - i_lrl_start;
	//lrl_to_rep_count++;
	
	// lrl_to_rep_count is the # of rep0's found
	// lrl_to_rep[] is the lrl before each one
	// there's a tail after last
				
	const U8 * ptr = litptr;
	const U8 * end = litptr + whole_lrl;

	// okay, output some reps
	for(int lrl_to_rep_i = 0;lrl_to_rep_i<lrl_to_rep_count;lrl_to_rep_i++)
	{
		// if current lrl + rep + next lrl fit in 7
		int cur_lrl = lrl_to_rep[lrl_to_rep_i];
		int next_lrl = lrl_to_rep[lrl_to_rep_i+1];

		RR_ASSERT( cur_lrl > 0 ); // && next_lrl > 0 );

		//* 
  		int cur_lrl_remainder  = lrl_remainder(cur_lrl);
  		int next_lrl_remainder = lrl_remainder(next_lrl);
  
		//z1:average : 143,579,361 ->66,244,501 =  3.691 bpb =  2.167 to 1 
		//z4:average : 143,579,361 ->63,139,903 =  3.518 bpb =  2.274 to 1 

		//lzt99 : 24,700,820 ->11,301,489 =  3.660 bpb =  2.186 to 1
		//decode only      : 18.143 millis, 1.27 c/b, rate= 1361.42 mb/s

  		if ( (cur_lrl_remainder+1+next_lrl_remainder) <= 7 )
  		/*/
  		
  		// this is wrong but seems to help?
  		// helps compression a bit, hurts decode speed
  		//	because it makes more packets
		//z1:average : 143,579,361 ->66,236,466 =  3.691 bpb =  2.168 to 1 
		//z4:average : 143,579,361 ->63,129,676 =  3.517 bpb =  2.274 to 1 

		//lzt99 : 24,700,820 ->11,305,582 =  3.662 bpb =  2.185 to 1 
		//decode only      : 18.755 millis, 1.31 c/b, rate= 1317.02 mb/s

		if ( (cur_lrl+1+next_lrl) <= 7 )
		/**/
		{

			#if 0

			// with the SSE2 lrl check enabled, this is an ICE in lappy VC 2005 :
			// v:\devel\projects\oodle2\core\newlzf.cpp(2118) : fatal error C1001: An internal error has occurred in the compiler.
			// compiler file 'F:\SP\vctools\compiler\utc\src\P2\main.c[0x00000000:0x00000000]', line 182)


			// last packet leading into escape - may as well make the rep
			//	because escape can't take any lrl
			// pretty meh
			// 11,507,960
			// 11,508,421
  			if ( lrl_to_rep_i == lrl_to_rep_count-1 && next_lrl == 0 && is_escape_match(codeoffset,ml) )
  			{
  				// DO make the packet
  			}
  			else
  			#endif
 			{	

				// don't output this packet
				// stick it on the next
				// "ptr" is the base of the lrl, it doesn't move
						
				lrl_to_rep[lrl_to_rep_i+1] += cur_lrl + 1;
				continue;
			}
		}

		// yes do a rep packet	
		
		newLZF_encoder_arrays_put_packet(encarrays,1,cur_lrl,0,lo,ptr);
	
		ptr  += cur_lrl + 1;
	}
	
	// modify the last parse that follows :
	int lrl = rrPtrDiff32(end - ptr);

	newLZF_encoder_arrays_put_packet(encarrays,ml,lrl,codeoffset,lo,ptr);
}

/**

newLZF_encoder_arrays_output:

take newLZF_encoder_arrays
write to [comp]

returns complen, fills pJ

**/

static SINTa newLZF_encoder_arrays_output(
	// filled :
	F32 * pJ,
	int * pchunktype,
	newLZF_passinfo * passinfo,
	U8 * comp,U8 * comp_end,
	
	// read :
	const newlz_vtable * vtable,
	newlz_encoder_scratch * scratch,
	newLZF_encoder_arrays * encarrays,
	SINTa whole_chunk_pos
	)
{
	int whole_chunk_len = encarrays->whole_chunk_len;
	const U8 * whole_chunk_ptr = encarrays->whole_chunk_ptr;
	bool selkie_comp = vtable->compressor == OodleLZ_Compressor_Selkie;
	bool selkie_lits = selkie_comp || vtable->level <= OodleLZ_CompressionLevel_HyperFast4;
	rrArenaAllocator * arena = scratch->arena;

	//rrprintf("newLZF_encoder_arrays_output : %d\n", whole_chunk_pos >> 17 );

	SIMPLEPROFILE_SCOPE_N(enc_arrays_put,whole_chunk_len);
		
	// check for collisions :
	RR_ASSERT( selkie_lits || encarrays->literals_ptr_raw < encarrays->packets_space );
	RR_ASSERT( !selkie_lits || ( encarrays->literals_ptr_raw > comp && encarrays->literals_ptr_raw < comp_end ) );
	RR_ASSERT( encarrays->packets_ptr < encarrays->offsets16_space );
	RR_ASSERT( encarrays->offsets16_ptr < encarrays->offsets24_space );
	RR_ASSERT( encarrays->offsets24_ptr < encarrays->excesses_space );
	RR_ASSERT( encarrays->excesses_ptr < encarrays->excesses_ptr_end );
	
	// Mermaid scratch does not have room for Indexed , make sure it is off :
	// OodleLZ_Compressor_ScratchMemSize
	RR_ASSERT( (vtable->entropy_flags & NEWLZ_ARRAY_FLAG_ALLOW_SPLIT_INDEXED) == 0 );
	
	SINTa packet_count = encarrays->packets_ptr - encarrays->packets_space;
	// is packet_count1 == 0 okay ? (yes)
	if ( packet_count == 0 )
	{
		// as of v6 packet_count == 0 is allowed
		if ( selkie_lits || encarrays->literals_space_sub == NULL ||
			g_OodleLZ_BackwardsCompatible_MajorVersion < 6 )
		{
			// no packets; just forbid this very rare case
			// let it be handled by outer huff-only array
			return whole_chunk_len;
		}
	}
	
	U8 * comp_ptr = comp;
	
	// start inset for sub literals :
	int whole_start_pos = 0;
	if ( whole_chunk_pos == 0 )
	{
		whole_start_pos = NEWLZF_MIN_OFFSET;
		
		// stuff first bytes raw :
		RR_COMPILER_ASSERT( NEWLZF_MIN_OFFSET == 8 );
		lz_copy8(comp_ptr,whole_chunk_ptr);
		comp_ptr += 8;
	}
	
	//===========================================================

	// put literals
	
	F32 lambda = vtable->lambda;
	U32 entropy_flags = vtable->entropy_flags;
	OodleLZ_CompressionLevel level = vtable->level;
	const OodleSpeedFit * speedfit = vtable->speedfit;
	
	F32 literals_J = LAGRANGE_COST_INVALID;	
	SINTa literal_count = encarrays->literals_ptr_raw - encarrays->literals_space_raw;
	SINTa literal_count_sub = encarrays->literals_ptr_sub - encarrays->literals_space_sub; // these can be null
	SINTa literal_comp_len = -1;

	F32 literals_uncompressed_J = (F32)(3 + literal_count);

	rrScopeArenaAlloc two_histos_alloc;
	U32 * histo_raw = NULL;
	U32 * histo_sub = NULL;
	if ( ! selkie_lits )
	{
		U32 * two_histos = (U32 *) two_histos_alloc.Alloc( 2*256*sizeof(U32), arena );
		histo_raw = two_histos;
		histo_sub = two_histos+256;
	}
		
	if ( literal_count == 0 && literal_count_sub > 0 )
	{
		// special case for all-sub-chunks test ; no raw literals
		RR_ASSERT( ! selkie_lits );
		
		#ifdef SPEEDFITTING
		if ( g_speedfitter_stage == 3 && g_stage3_literal_mode == NEWLZ_LITERALS_TYPE_RAW )
			return whole_chunk_len+1;
		#endif
		
		literal_count = literal_count_sub;
		literals_uncompressed_J = (F32)(3 + literal_count);
		
		*pchunktype = NEWLZ_LITERALS_TYPE_SUB;
					
		CountHistoArrayU8(encarrays->literals_space_sub,literal_count,histo_sub,256);
				
		literal_comp_len = newLZ_put_array_histo(comp_ptr,comp_end,
								encarrays->literals_space_sub,literal_count,
								histo_sub,entropy_flags,lambda,speedfit,&literals_J,ARRAY_DEADLINE_HUGE,arena,level);
								
		// if we didn't put sub, or sub expanded, put raw :
		if ( literal_comp_len < 0 || literal_comp_len > literal_count )
		{
			return whole_chunk_len+1;
		}
		
		if ( passinfo )
		{
			memcpy(passinfo->literal_histo,histo_sub,256*sizeof(histo_sub[0]));
		}
	}
	else
	{
	
		RR_ASSERT( encarrays->literals_ptr_sub == NULL || literal_count_sub == literal_count );
		if ( selkie_lits || literal_count < NEWLZ_HUFF_ARRAY_MIN_SIZE )
		{
			literal_comp_len = 3 + literal_count;
			literals_J = literals_uncompressed_J;
	
			*pchunktype = NEWLZ_LITERALS_TYPE_RAW;
			
			if ( selkie_lits )
			{
				// already done, put directly
				RR_ASSERT( encarrays->literals_space_raw == comp_ptr+3 );
			
				newLZ_put_array_uncomp_header(comp_ptr,literal_count);
			}
			else
			{
				newLZ_put_array_uncompressed(comp_ptr,comp_end,encarrays->literals_space_raw,literal_count);
			}
		}
		else
		{	
			CountHistoArrayU8(encarrays->literals_space_raw,literal_count,histo_raw,256);

			// @@?? in Optimal levels don' try to estimate the cost of raw/sub , just put both :
			bool do_both_ways = level >= OodleLZ_CompressionLevel_Optimal2;
				
			if ( encarrays->literals_space_sub )
			{
				// histo both, estimate cost, choose
				// copy histo to passinfo
				CountHistoArrayU8(encarrays->literals_space_sub,literal_count,histo_sub,256);
				
				bool do_put_sub = do_both_ways;
				
				F32 sub_literal_time_J = lambda * speedfit->parse_Mermaid_subliterals(literal_count);
		
				if ( ! do_both_ways )
				{				
					S32 bits_raw = newlz_array_estimate_complen_bits(histo_raw,256,(U32)literal_count);
					S32 bits_sub = newlz_array_estimate_complen_bits(histo_sub,256,(U32)literal_count);
					F32 J_sub = bits_sub/8.f + sub_literal_time_J;
					F32 J_raw = bits_raw/8.f;
					
					do_put_sub = ( J_sub < J_raw );
				}
				
				#ifdef SPEEDFITTING
				if ( g_speedfitter_stage == 3 )
					do_put_sub = ( g_stage3_literal_mode == NEWLZ_LITERALS_TYPE_SUB );
				#endif
		
				if ( do_put_sub )
				{
					*pchunktype = NEWLZ_LITERALS_TYPE_SUB;
								
					literal_comp_len = newLZ_put_array_histo(comp_ptr,comp_end,
											encarrays->literals_space_sub,literal_count,
											histo_sub,entropy_flags,lambda,speedfit,&literals_J,ARRAY_DEADLINE_HUGE,arena,level);
					
					// sub_literal_time is now on literals_J ; not in the parse_time
					literals_J += sub_literal_time_J;
				}
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
		
				SINTa literal_comp_len_raw = newLZ_put_array_histo(
										comp_ptr,comp_end,encarrays->literals_space_raw,literal_count,
										histo_raw,entropy_flags,lambda,speedfit,&literals_J,ARRAY_DEADLINE_HUGE,arena,level);
			
				if ( literal_comp_len_raw > 0 )
				{
					literal_comp_len = literal_comp_len_raw;
					*pchunktype = NEWLZ_LITERALS_TYPE_RAW;
				}
			}
			
			if ( passinfo )
			{
				if ( *pchunktype == NEWLZ_LITERALS_TYPE_RAW ) memcpy(passinfo->literal_histo,histo_raw,256*sizeof(histo_sub[0]));
				else memcpy(passinfo->literal_histo,histo_sub,256*sizeof(histo_sub[0]));
			}
		}
	}
	
	if ( literal_comp_len < 0 )
		return whole_chunk_len;
	comp_ptr += literal_comp_len;
	
	RR_ASSERT( literals_J <= literals_uncompressed_J );
	RR_ASSERT( literals_J >= literal_comp_len );
	RR_ASSERT( lambda != 0.f || literals_J == literal_comp_len );
	
	//===========================================================
	// put packets :
		
	SINTa packet_comp_len;
	F32 packets_J = LAGRANGE_COST_INVALID;
	
	if ( selkie_comp )
	{
		packets_J = 3 + (F32)packet_count;
		
		packet_comp_len = newLZ_put_array_uncompressed(comp_ptr,comp_end,encarrays->packets_space,packet_count);
	}
	else
	{
		packet_comp_len = newLZ_put_array(comp_ptr,comp_end,encarrays->packets_space,packet_count,entropy_flags,lambda,speedfit,&packets_J,ARRAY_DEADLINE_HUGE,
			arena,level,passinfo ? passinfo->packet_histo : NULL);
	}
	
	if ( packet_comp_len < 0 )
		return whole_chunk_len;
			
	comp_ptr += packet_comp_len;
	
	//rrprintfvar(packet_count);
	//rrprintfvar(packet_comp_len);
	
	//===========================================================
	
	// comp_ptr_parse_start = after packets & literals
	U8 * comp_ptr_parse_start = comp_ptr;
	
	// early check space for minimum of remaining headers :
	if ( 16 >= (comp_end-comp_ptr) ) return whole_chunk_len;
	
	// put packet_count1 after packets :
	if ( whole_chunk_len > (1<<16) )
	{
		// put packet_count1 :
		// fits in 16 bits :
		RR_ASSERT( encarrays->packet_count1 < (1<<16) );
		
		RR_PUT16_LE_UNALIGNED(comp_ptr,(U16)encarrays->packet_count1);
		comp_ptr += 2;
	}
	else
	{
		RR_ASSERT( encarrays->packet_count1 == packet_count );
	}
	
	//===========================================================
		
	SINTa off16_comp_len = rrPtrDiff(encarrays->offsets16_ptr - encarrays->offsets16_space);
	RR_ASSERT( (off16_comp_len&1) == 0 );
	SINTa num_off16s = off16_comp_len/2;
	
	bool do_off16s_raw = true;
	
	F32 off16_J = (F32)off16_comp_len; // uncompressed offsets just point at, no memcpy time

	// Mermaid HyperFast4 - try entropy offsets or not? not sure.
	bool selkie_offs = selkie_comp; // Mermaid HF4 offset huff on
	//bool selkie_offs = selkie_lits; // Mermaid HF4 offset huff OFF

	// try entropy coding offsets :
	// Mermaid+ entropy coded offsets start in 2.4.0
	if ( ! selkie_offs && num_off16s >= NEWLZ_HUFF_ARRAY_MIN_SIZE && (g_OodleLZ_BackwardsCompatible_MajorVersion >= 4) )
	{
		// put to scratch_space (overlaps literals) - they're already output
		RR_ASSERT( encarrays->scratch_base == encarrays->literals_space_raw || selkie_lits );
	
		U8 * off16s_hi = encarrays->scratch_base;
		U8 * off16s_lo = off16s_hi + num_off16s;	

		const U8 * off16s_ptr = encarrays->offsets16_space;

		// deinterleave off16s' - @@ could be simd of course
		for(SINTa i=0;i<num_off16s;i++)
		{
			U16 off = RR_GET16_LE_UNALIGNED(off16s_ptr); off16s_ptr += 2;
			off16s_lo[i] = (U8)(off);
			off16s_hi[i] = (U8)(off>>8);
		}
		
		F32 offhi_J = LAGRANGE_COST_INVALID;
		F32 offlo_J = LAGRANGE_COST_INVALID;

		// put huff in the packet space (already output)
		U8 * off_huff_comp_ptr = encarrays->scratch_base + num_off16s*2;
		U8 * off_huff_comp_end = encarrays->offsets16_space;

		// @@ is this guaranteed to be true?  (the LHS is chunk_len) -> I think so
		RR_ASSERT( rrPtrDiff(off_huff_comp_end - off_huff_comp_ptr) > (num_off16s + 5)*2 );

		SINTa offhi_complen = newLZ_put_array(off_huff_comp_ptr,off_huff_comp_end,off16s_hi,num_off16s,entropy_flags,lambda,speedfit,&offhi_J,ARRAY_DEADLINE_HUGE,
			arena,level,passinfo ? passinfo->offsetH_histo : NULL);
											
		SINTa offlo_complen = newLZ_put_array(off_huff_comp_ptr+offhi_complen,off_huff_comp_end,off16s_lo,num_off16s,entropy_flags,lambda,speedfit,&offlo_J,ARRAY_DEADLINE_HUGE,
			arena,level,passinfo ? passinfo->offsetL_histo : NULL);

		RR_ASSERT( offhi_complen > 0 && offlo_complen > 0 );
			
		// off16_comp_len = num_off16s * 2
		
		// is it worth it space-speed ?

		// time to do the lo+hi merge :
		F32 merge_time = speedfit->simd_interleave_8x2(num_off16s);
		F32 off16_huff_J = offhi_J + offlo_J + lambda * merge_time;

		if ( off16_huff_J < off16_J )
		{
			// if we did any compression :
			SINTa off_huff_complen = offhi_complen + offlo_complen;
			RR_ASSERT( off_huff_complen < off16_comp_len );
			
			if ( (2+off_huff_complen) >= (comp_end-comp_ptr) ) return whole_chunk_len;
		
			// put special header :		
			RR_PUT16_LE_UNALIGNED( comp_ptr, 0xFFFF );
			comp_ptr += 2;

			memcpy(comp_ptr,off_huff_comp_ptr,off_huff_complen);
			comp_ptr += off_huff_complen;
			
			//rrprintfvar(off_huff_complen);

			off16_J = off16_huff_J;
			off16_comp_len = off_huff_complen;

			do_off16s_raw = false;
		}
	}

	if ( do_off16s_raw )
	{	
		if ( (2+off16_comp_len) >= (comp_end-comp_ptr) ) return whole_chunk_len;
	
		// off16 header :
		RR_PUT16_LE_UNALIGNED( comp_ptr, U16_check((S32)num_off16s) );
		comp_ptr += 2;
		
		memcpy(comp_ptr,encarrays->offsets16_space,off16_comp_len);
		comp_ptr += off16_comp_len;
			
		//rrprintfvar(off16_comp_len);
	}
	
	// off16_J is the comp payload size (off16_comp_len) (not including the 2 byte header) + time
	// the 2 byte header will be picked up by the "parse" bytes (starting at comp_ptr_parse_start)
	RR_ASSERT( off16_J >= off16_comp_len );
	RR_ASSERT( lambda > 0.f || off16_J == (F32)off16_comp_len );
	
	//===========================================================
	
	SINTa off24_total_count = encarrays->off24_ptr_chunk_count1 + encarrays->off24_ptr_chunk_count2;
	
	// check that decoder will have enough scratch space :
	SINTa decoder_scratch_space_needed =
		literal_count + packet_count + 4 +
		4*num_off16s + sizeof(U32)*(off24_total_count +
		2 * NEWLZF_ESCAPE_OFFSET_PAD_ZERO_COUNT );
			
	decoder_scratch_space_needed += OODLELZ_MAX_SCRATCH_FOR_PHASE_HEADERS_AND_FUZZ;
	
	decoder_scratch_space_needed += NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH;
	
	// no indexed in newlzf :	
	//	and double the size of the largest entropy array to account for indexed scratch
	//decoder_scratch_space_needed += RR_MAX4(literal_count,packet_count,offsets_count,excesses_u8_count);
		
	SINTa decoder_scratch_size = OodleLZ_Compressor_ScratchMemSize(vtable->compressor,whole_chunk_len);		
		
	// should never hit this :
	RR_ASSERT( decoder_scratch_space_needed <= decoder_scratch_size );
	if ( decoder_scratch_space_needed > decoder_scratch_size )
	{
		ooLogError("newLZF chunk needs too much scratch!\n");
		return whole_chunk_len; // return expansion
	}
	
	//===========================================================
	
	if ( 7 >= (comp_end-comp_ptr) ) return whole_chunk_len;
	
	// put the off24 header :
	U32 off24_header = (RR_MIN(encarrays->off24_ptr_chunk_count1,0xFFF)<<12) | 
						RR_MIN(encarrays->off24_ptr_chunk_count2,0xFFF);
	RR_PUT24_LE_NOOVERRUN(comp_ptr,off24_header);
	comp_ptr += 3;
	
	if ( encarrays->off24_ptr_chunk_count1 >= 0xFFF )
	{
		RR_PUT16_LE_UNALIGNED(comp_ptr,(U16)encarrays->off24_ptr_chunk_count1);
		comp_ptr += 2;
	}
	if ( encarrays->off24_ptr_chunk_count2 >= 0xFFF )
	{
		RR_PUT16_LE_UNALIGNED(comp_ptr,(U16)encarrays->off24_ptr_chunk_count2);
		comp_ptr += 2;
	}
	
	// put the off24 data :
					
	SINTa off24_data_len = rrPtrDiff(encarrays->offsets24_ptr - encarrays->offsets24_space);
	RR_ASSERT( off24_data_len >= 3*(encarrays->off24_ptr_chunk_count1 + encarrays->off24_ptr_chunk_count2) );

	if ( off24_data_len >= (comp_end-comp_ptr) ) return whole_chunk_len;
	
	memcpy(comp_ptr,encarrays->offsets24_space,off24_data_len);
	comp_ptr += off24_data_len;
	
	//rrprintfvar(off24_data_len);
	
	// put the excesses :
	
	SINTa excesses_data_len = rrPtrDiff(encarrays->excesses_ptr - encarrays->excesses_space);
	
	if ( excesses_data_len >= (comp_end-comp_ptr) ) return whole_chunk_len;
	
	memcpy(comp_ptr,encarrays->excesses_space,excesses_data_len);
	comp_ptr += excesses_data_len;
			
	SINTa total_complen = rrPtrDiff( comp_ptr - comp );
	
	if ( total_complen >= whole_chunk_len )
	{
		// expansion, bail out
		// -> this is what you hit a lot on Fez
		//	that causes either speed or compression to be very wobbly
		return whole_chunk_len;
	}
		
	SINTa parse_complen = rrPtrDiff( comp_ptr - comp_ptr_parse_start );
	// don't include off16 size in the parse, we'll get that from off16_J
	parse_complen -= off16_comp_len;
	
	F32 parse_time;
	
	if ( vtable->compressor == OodleLZ_Compressor_Mermaid )
	{
		parse_time = speedfit->parse_Mermaid(
			whole_chunk_len,
			packet_count,
			encarrays->speedstat_escapes);
	}
	else
	{
		RR_ASSERT( vtable->compressor == OodleLZ_Compressor_Selkie );
	
		parse_time = speedfit->parse_Selkie(
			whole_chunk_len,
			packet_count,
			encarrays->speedstat_escapes,
			literal_count);
	}
	
	// literals_J has sub literal time
	// off16_J has offset merge time
	
	F32 parse_J = parse_complen + lambda * parse_time;
	F32 total_J = literals_J + packets_J + parse_J + off16_J + whole_start_pos;

	total_J += lambda * speedfit->newlzf_unpack_escape_offsets(off24_total_count);

	//rrprintfvar(total_J);
	//rrprintfvar(total_complen);

	// reverting to uncompressed is checked on the outside
	
	// when lambda is 0, we're just optimizing complen, so it should be exact :
	RR_ASSERT( total_J >= total_complen );
	RR_ASSERT( lambda > 0.f || total_J == total_complen );
	
	*pJ = total_J;
	return total_complen;
}
	
//=========================================================================

static SINTa newLZF_put_parse(
	F32 * pJ,
	int * pchunktype,
	newLZF_passinfo * passinfo,
	U8 * comp,U8 * comp_end,
	
	const newlz_vtable * vtable,
	newlz_encoder_scratch * scratch,
	const U8 * whole_chunk_ptr,int whole_chunk_len,
	SINTa whole_chunk_pos,
	const vector_a<newlzf_encoder_parse> & parsevec,
	SINTa parsevec_size1
	)
{
	/**
	
	put_parse :
	
	just put the parsevec to an encoder_arrays
	then output the encoder_arrays
	
	-> this is only used by the optimal parse now
		sub-optimal uses put_arrays
	
	**/

	bool selkie = vtable->compressor == OodleLZ_Compressor_Selkie;
	bool do_sub = ! selkie;
	newLZF_encoder_arrays encarrays;
	newLZF_encoder_arrays_point_to_scratch(&encarrays,scratch,whole_chunk_len,whole_chunk_ptr,do_sub);
	
	int whole_start_pos = 0;
	if ( whole_chunk_pos == 0 )
		whole_start_pos = NEWLZF_MIN_OFFSET;
	
	if ( selkie )
	{
		encarrays.literals_space_raw = comp + 3 + whole_start_pos;
		encarrays.literals_ptr_raw = encarrays.literals_space_raw;
	}
	
	SINTa lastoffset = NEWLZF_MIN_OFFSET;
	
	// two parse chunks per huff chunk :
	for(int twice=0;twice<2;twice++)
	{
		U8cptr ptr_matchend;
		if ( ! newLZF_encoder_arrays_start_parse_chunk(&encarrays,twice,whole_start_pos,ptr_matchend) )
			break;
	
		SINTa parsevec_start;
		SINTa parsevec_end;
		
		if ( twice == 0 )
		{
			parsevec_start = 0;
			parsevec_end = parsevec_size1;
		}
		else
		{
			parsevec_start = parsevec_size1;
			parsevec_end = parsevec.size();		
		}
		
		SINTa parse_cur_pos = encarrays.parse_chunk_start_pos;
		
		for(SINTa s=parsevec_start;s<parsevec_end;s++)
		{
			const newlzf_encoder_parse & parse = parsevec[s];

			RR_DURING_ASSERT( int match_start_pos = (int)parse_cur_pos + parse.lrl );
			RR_ASSERT( match_start_pos + NEWLZF_CHUNK_MATCH_SAFE_ZONE <= whole_chunk_len );
			RR_ASSERT( match_start_pos + parse.ml + NEWLZF_CHUNK_MATCH_SAFE_ZONE -1 <= whole_chunk_len );
			
			const U8 * literals_start = whole_chunk_ptr + parse_cur_pos;
			
			#if 0
			// Selkie optimal parse does mml1 LO matching and makes its own choices about this
			if ( parse.lrl >= 8 && parse.lrl < NEWLZF_LRL_EXCESS )
			{ 
				do_lomml1_for_lrl(&encarrays,parse.ml,parse.lrl,parse.offset,lastoffset,literals_start);
			}
			else
			#endif
			{
				newLZF_encoder_arrays_put_packet(&encarrays,parse.ml,parse.lrl,parse.offset,lastoffset,literals_start);
			}
			
			lastoffset = parse.IsLO() ? lastoffset : parse.offset;
			
			parse_cur_pos += parse.ml + parse.lrl;
		}
	
		const U8 * literals_start = whole_chunk_ptr + parse_cur_pos;
			
		newLZF_encoder_arrays_finish_parse_chunk(&encarrays,twice,literals_start,lastoffset);
	}	

	SINTa compLen = newLZF_encoder_arrays_output(pJ,pchunktype,passinfo,comp,comp_end,vtable,scratch,&encarrays,whole_chunk_pos);
	return compLen;
}	

//=========================================================================

template<typename newLZ_CTMF,int t_do_lazy_parse,int t_do_match_backup,int t_step_literals_shift,int t_do_lomml1>
static SINTa newLZF_encode_chunk(const newlz_vtable * vtable,
	newlz_encoder_scratch * scratch,
	const U8 * dictionaryBase,
	const U8 * whole_chunk_ptr,int whole_chunk_len,
	U8 * comp,U8 * comp_end,
	SINTa chunk_pos,
	int * pchunktype,
	F32 * pJ,
	const OodleKrakenChunkDeadlines *)
{
	SIMPLEPROFILE_SCOPE_N(encode_chunk,whole_chunk_len);
	
	*pchunktype = -1;
	*pJ = LAGRANGE_COST_INVALID;
	//rrprintf("newLZF_encode_chunk : %d\n",chunk_len);
	
	if ( whole_chunk_len <= NEWLZF_MIN_CHUNK_LEN )
	{
		return whole_chunk_len;
	}
				
	newLZ_CTMF * ctmf = (newLZ_CTMF *) vtable->matcher;
	
	bool selkie  = vtable->compressor == OodleLZ_Compressor_Selkie;
	bool do_sub = ! selkie;
	
	newLZF_encoder_arrays encarrays;
	newLZF_encoder_arrays_point_to_scratch(&encarrays,scratch,whole_chunk_len,whole_chunk_ptr,do_sub);
			
	const OodleLZ_CompressOptions * pOptions = vtable->pOptions;
	int mml = RR_MAX(pOptions->minMatchLen,NEWLZF_MML_NORMAL);
	
	U32 dictionarySize = ( pOptions->dictionarySize > 0 ) ?
		RR_MIN(NEWLZF_MAX_OFFSET,pOptions->dictionarySize)
		: NEWLZF_MAX_OFFSET;	

	S32 offsLimitTab[32];
	newLZF_InitOffsLimitTab(offsLimitTab, mml, selkie ? OFF24_MML_SELKIE : OFF24_MML_MERMAID, pOptions);
	
	// @@ carry los from last chunk ?
	S32 lastoffset = NEWLZF_MIN_OFFSET;

	// start inset for sub literals :
	int whole_start_pos = 0;
	if ( chunk_pos == 0 )
	{
		whole_start_pos = NEWLZF_MIN_OFFSET;
	}
	
	if ( selkie )
	{
		// selkie direct literal output :
		encarrays.literals_space_raw = comp + whole_start_pos + 3;
		encarrays.literals_ptr_raw = encarrays.literals_space_raw;
	}
		
	// two 64k chunks
	// lastoffset is carried
	for(int twice=0;twice<2;twice++)
	{
	
	// ptr_matchend is the match *end* limit
	const U8 * ptr_matchend;
	
	if ( ! newLZF_encoder_arrays_start_parse_chunk(&encarrays,twice,whole_start_pos,ptr_matchend) )
		break;

	const U8 * start_ptr = whole_chunk_ptr + encarrays.parse_chunk_start_pos;
	const U8 * literals_start = start_ptr;

	// parse_end_pos is the match *start* pos limit
	// -5 from : -1 for lazy, -4 to make room for a U32 grab before ptr_matchend
	int parse_end_pos = rrPtrDiff32(ptr_matchend - whole_chunk_ptr) - 5;
	
	// don't start the ctmf unless we have room to grab a U32 :
	//  (NEWLZF_MIN_CHUNK_LEN does not guarantee this because of the two-64k chunk business)
	//	(eg. if the whole buffer is 64k+1 bytes, the second chunk is tiny)
	if ( encarrays.parse_chunk_start_pos < parse_end_pos )
	{
	
	// start ctmf :
	ctmf->set_next(start_ptr);

	const U8 * parse_end_ptr = whole_chunk_ptr + parse_end_pos;

	for(const U8 * ptr = start_ptr; ;)
	{
		RR_ASSERT( ptr < parse_end_ptr );
	
		//if ( (ptr - whole_chunk_ptr) >= 69270 ) RR_BREAK();

		// literal loop: seek the next match
		match chosen;
		SINTa scaled_skip_dist = 1 << t_step_literals_shift;

		// if t_step_literals_shift is on, t_do_lazy_parse must be off :
		RR_COMPILER_ASSERT( t_do_lazy_parse==0 || t_step_literals_shift==0 ); // not compatible

		for(;;)
		{
			RR_ASSERT( ctmf->m_next_ptr == ptr );

			if ( ! t_do_lazy_parse )
			{
				// look for lo match at ptr+1 !
				// parse_end_ptr has the -1 for lazy so this is safe
				if ( newLZF_find_lo_match_ahead(&chosen,ctmf,lastoffset,ptr,ptr_matchend) )
				{
					ptr++;
					break;
				}
			}

			SINTa step = t_step_literals_shift ? (scaled_skip_dist>>t_step_literals_shift) : 1;

			// if skipping that far would put us past the parse end, bail.
			// have to check this before get match because it does prefetch next
			if ( rrPtrDiff(parse_end_ptr - ptr) <= step )
				goto parse_chunk_done;

			// once we find a match, we can stop!
			if ( newLZF_get_match_heuristic(&chosen,ctmf,step,lastoffset,ptr,ptr_matchend,mml,offsLimitTab,dictionarySize,literals_start) )
				break;

			if ( t_step_literals_shift )
			{
				scaled_skip_dist++;
			}
				
			ptr += step;

			RR_ASSERT( ctmf->m_next_ptr == ptr );
		}
		
		// in new scheme LOML==1 is not found by the matcher
		// those are considered part of the literal run
		// then we find them with do_lomml1_for_lrl
		RR_ASSERT( chosen.ml > 1 );
		
		// update pos in case there was a match backup
		//ptr = whole_chunk_ptr + chosen.pos;
					
		//if ( 1 )
		// if chosen ML is one, go ahead and emit it, we'll get the lazy next time
		if ( t_do_lazy_parse )
		{
			RR_ASSERT( t_step_literals_shift == 0 ); // not compatible
			// lazy parse
				
			while(ptr+1<parse_end_ptr)
			{
				// lazy parse
				
				// if we didn't step ahead (eg. because we took a match backup)
				//	then we can't search for matches again,
				//	because the hashes have already been updated
				RR_ASSERT( ptr+1 == ctmf->m_next_ptr );
				//if ( ptr+1 != ctmf->m_next_ptr )
				//	break;
				
				match lazy;
				if (
					newLZF_get_match_heuristic(&lazy,ctmf,1,lastoffset,ptr+1,ptr_matchend,mml,offsLimitTab,dictionarySize,literals_start)
					&& newLZF_LazyMatchDelta(lazy.ml,lazy.off,lazy.IsLO(),chosen.ml,chosen.off,chosen.IsLO()) > 0 )
				{
					chosen = lazy;
					//ptr = whole_chunk_ptr + chosen.pos;
					ptr++;
				}
				else if ( t_do_lazy_parse >= 2 ) // lazy2 check
				{
					// lazy2
					if ( ptr+2 > parse_end_ptr ) break;
					// don't do this if we might take an ml==2 step !
					if ( chosen.ml == 2 ) break;
				
					// lazy2 helps a lot on text (dickens)
					//	it actually mostly *hurts* on binary
					//	(baby7, records7, etc)
				
					if ( newLZF_get_match_heuristic(&lazy,ctmf,1,lastoffset,ptr+2,ptr_matchend,mml,offsLimitTab,dictionarySize,literals_start)
						&& newLZF_LazyMatchDelta(lazy.ml,lazy.off,lazy.IsLO(),chosen.ml,chosen.off,chosen.IsLO()) > NEWLZF_LAZYBETTER_LAZY2 )
					{
						chosen = lazy;
						//ptr = whole_chunk_ptr + chosen.pos;
						ptr+=2;
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
		RR_ASSERT( chosen.ml >= NEWLZF_MML_NORMAL || chosen.IsLO() );
		
		SINTa offset = chosen.IsLO() ? lastoffset : chosen.off;
		
  		if ( t_do_match_backup )
  		{
  			// try match start backup :
  			//   ( I tried doing per-match in the CTMF, not after selecting one, was no big win )
  			while( ptr > literals_start )
  			{
				if ( rrPtrDiff(ptr - ctmf->m_base_ptr) <= offset )
  					break;
  					
				if ( ptr[-1] == ptr[-1-offset] )
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
		
		RR_ASSERT( memcmp(ptr,ptr-offset,chosen.ml) == 0 );
		
		int lrl = rrPtrDiff32( ptr - literals_start );
		RR_ASSERT( lrl >= 0 );
		
		if ( t_do_lomml1 && lrl >= 8 && lrl < NEWLZF_LRL_EXCESS )
		{
			do_lomml1_for_lrl(&encarrays,chosen.ml,lrl,chosen.off,lastoffset,literals_start);
		}
		else
		{
			newLZF_encoder_arrays_put_packet(&encarrays,chosen.ml,lrl,chosen.off,lastoffset,literals_start);
		}
			
		// update lastoffsets :
		lastoffset = (S32)offset;
		
		// update hashes inside the match
		// but don't do it if we might run into end-of-buffer
		// @@ should only check this on the *last* chunk :
		//	or check vs buffer_end_pos NOT parse_end_pos
		//  (we miss some match finder updates between chunks this way)
		
		ptr += chosen.ml;
		literals_start = ptr;
		if ( ptr >= parse_end_ptr )
			break;
		
		// in SuperFast - don't insert inside matches?
		//	yes, I think this is good
		if ( t_do_lomml1 ) // hacky way to detect !SuperFast
		{
			ctmf->step_and_insert(ptr-chosen.ml,chosen.ml);
		}
		else
		{
			// or just set_next ? (skips all inserts)
			//  no, partial is much better
			ctmf->step_and_insert_partial(ptr-chosen.ml,chosen.ml);
		}
	}

parse_chunk_done:;
	
	/*
	#if NEWLZF_DO_CTMF2
	RR_ASSERT( ctmf->m_next_ptr <= ptr_matchend );
	#endif
	*/
	
	} // if any bytes to parse

	newLZF_encoder_arrays_finish_parse_chunk(&encarrays,twice,literals_start,lastoffset);
			
	} // twice
		
	//===========================================================

	return newLZF_encoder_arrays_output(pJ,pchunktype,NULL,comp,comp_end,
		vtable,scratch,
		&encarrays,chunk_pos);
}

//=========================================================================

static RADFORCEINLINE const U8 * find_match_end(const U8 * ptr,SINTa neg_offset,const U8 * ptr_matchend)
{
	while (ptr < ptr_matchend)
	{
		UINTr big1 = *((UINTr *)ptr);
		UINTr big2 = *((UINTr *)(ptr + neg_offset));
		ptr += sizeof(UINTr);
		if ( big1 != big2 )
		{
			ptr -= sizeof(UINTr); // we didn't actually match, back up!
			ptr += GetNumBytesZeroNeverAllR(big1 ^ big2);
			break;
		}
	}
	ptr = RR_MIN(ptr,ptr_matchend); // we might have overshot
	return ptr;
}

template<typename newLZ_CTMF,int t_step_literals_shift,int t_do_match_backup,int t_quadratic_skip,int t_allow_sub_lits,int t_do_partial_inserts>
static SINTa newLZF_encode_chunk_fast_mode(const newlz_vtable * vtable,
	newlz_encoder_scratch * scratch,
	const U8 * dictionaryBase,
	const U8 * whole_chunk_ptr,int whole_chunk_len,
	U8 * comp,U8 * comp_end,
	SINTa chunk_pos,
	int * pchunktype,
	F32 * pJ,
	const OodleKrakenChunkDeadlines *)
{
	SIMPLEPROFILE_SCOPE_N(encode_chunk_fast,whole_chunk_len);
	
	typedef typename newLZ_CTMF::hash_type HashType;
	*pchunktype = -1;
	*pJ = LAGRANGE_COST_INVALID;

	if ( whole_chunk_len <= NEWLZF_MIN_CHUNK_LEN )
	{
		return whole_chunk_len;
	}

	const OodleLZ_CompressOptions * pOptions = vtable->pOptions;
	int mml = RR_MAX(pOptions->minMatchLen,NEWLZF_MML_NORMAL);
	bool selkie_comp = vtable->compressor == OodleLZ_Compressor_Selkie;
	bool selkie_lits = selkie_comp || vtable->level <= OodleLZ_CompressionLevel_HyperFast4;
	bool do_sub = !selkie_lits && t_allow_sub_lits;

	U32 dictionarySize = ( pOptions->dictionarySize > 0 ) ?
		RR_MIN(NEWLZF_MAX_OFFSET,pOptions->dictionarySize)
		: NEWLZF_MAX_OFFSET;

	U32 dictionary_span = dictionarySize - NEWLZF_MIN_OFFSET;

	newLZ_CTMF * ctmf = (newLZ_CTMF *) vtable->matcher;

	newLZF_encoder_arrays encarrays;
	newLZF_encoder_arrays_point_to_scratch(&encarrays,scratch,whole_chunk_len,whole_chunk_ptr,do_sub);

	S32 offsLimitTab[32];
	newLZF_InitOffsLimitTab(offsLimitTab, mml, selkie_comp ? OFF24_MML_SELKIE : OFF24_MML_MERMAID, pOptions);

	SINTa neg_lo0 = -NEWLZF_MIN_OFFSET;

	// start inset for sub literals :
	int whole_start_pos = 0;
	if ( chunk_pos == 0 )
	{
		whole_start_pos = NEWLZF_MIN_OFFSET;
	}

	if ( selkie_lits )
	{
		// selkie direct literal output :
		encarrays.literals_space_raw = comp + whole_start_pos + 3;
		encarrays.literals_ptr_raw = encarrays.literals_space_raw;
	}

	// two 64k chunks
	// lastoffset is carried
	for(int twice=0;twice<2;twice++)
	{
		// ptr_matchend is the match *end* limit
		const U8 * ptr_matchend;

		if ( ! newLZF_encoder_arrays_start_parse_chunk(&encarrays,twice,whole_start_pos,ptr_matchend) )
			break;

		const U8 * start_ptr = whole_chunk_ptr + encarrays.parse_chunk_start_pos;
		const U8 * literals_start = start_ptr;

		// parse_end_pos is the match *start* pos limit
		// -5 from : -1 for lazy, -4 to make room for a U32 grab before ptr_matchend
		int parse_end_pos = rrPtrDiff32(ptr_matchend - whole_chunk_ptr) - 5;

		const U8 * hash_base = ctmf->m_base_ptr;
		HashType * hash_table = ctmf->m_hash_table.data();
		U64 hash_mul = ctmf->m_hash_mul;
		int hash_shift = ctmf->m_hash_shift;

		// don't start the ctmf unless we have room to grab a U32 :
		//  (NEWLZF_MIN_CHUNK_LEN does not guarantee this because of the two-64k chunk business)
		//	(eg. if the whole buffer is 64k+1 bytes, the second chunk is tiny)
		if ( encarrays.parse_chunk_start_pos < parse_end_pos )
		{
			const U8 * parse_end_ptr = whole_chunk_ptr + parse_end_pos;

			for(const U8 * ptr = start_ptr; ;)
			{
				RR_ASSERT( ptr < parse_end_ptr );

				// literal loop: seek the next match
				const U8 * match_end;
				const U8 * match_ptr; // position we're matching to
				SINTa match_offs; // or 0 for LO
				SINTa neg_offset;
				SINTa scaled_skip_dist = 1 << t_step_literals_shift;

				for(;;)
				{
					U64 ptr64le = RR_GET64_LE_UNALIGNED(ptr);
					U32 ptr32le = (U32)ptr64le;

					// hash lookup and insert
					SINTa pos = rrPtrDiff(ptr - hash_base);
					HashType hashpos = fast_ctmf_insert(hash_table, ptr64le, hash_mul, hash_shift, pos);

					// check for rep0 match *at next byte* (pseudo-lazy), MML 3
					U32 lodiff = ptr32le ^ RR_GET32_LE_UNALIGNED(ptr+neg_lo0);
					if ( lodiff <= 255 ) // this checks that bytes 1-3 (zero-based) at ptr and lo32 agree (i.e. ptr+1 has a len >=3 LO0 match)
					{
						ptr++;
						match_ptr = ptr + neg_lo0;
						match_offs = 0;
						neg_offset = neg_lo0;

						// We already inserted at the previous position (that we just skipped over).
						// My testing says that this second insert is almost zero net cost, even at
						// HyperFast3, but it's a tidy ratio win.
						fast_ctmf_insert(hash_table, ptr, hash_mul, hash_shift, pos + 1);

						match_end = find_match_end(ptr + 3,neg_lo0,ptr_matchend);
						break;
					}

					// if ptr32==lo32 (lodiff==0), we take the above branch which breaks out of the loop,
					// guaranteeing we don't pass 0 here
					S32 lo_ml = rrCtzBytes32(lodiff);

					// is this a len >=4 valid match?
					match_offs = (SINTa) (HashType)(pos - hashpos);
					match_ptr = ptr - match_offs;
					U32 match32le = RR_GET32_LE_UNALIGNED(match_ptr);
					if ( match32le == ptr32le )
					{
						// We can find matches with offs <NEWLZ_MIN_OFFSET here, which we want to handle,
						// because they denote runs.
						// Do it in this branchy way, which tests for any out-of-range offset whether
						// there's a match at MIN_OFFSET. This is a bit wonky, but helps with runs and,
						// just as importantly, does not seem to be a big perf loss in the typical case.
						if ( (U32)(match_offs - NEWLZF_MIN_OFFSET) < dictionary_span )
						{
							neg_offset = match_ptr - ptr;
							match_end = find_match_end(ptr + 4,neg_offset,ptr_matchend);

							S32 ml = rrPtrDiff32(match_end - ptr);
							if (newLZF_IsAllowedNormalMatch(ml,(int)match_offs,offsLimitTab))
								break;
						}
						else if ( ptr32le == RR_GET32_LE_UNALIGNED(ptr - NEWLZF_MIN_OFFSET) )
						{
							match_offs = NEWLZF_MIN_OFFSET;
							neg_offset = -NEWLZF_MIN_OFFSET;
							match_end = find_match_end(ptr + 4,neg_offset,ptr_matchend);
							// ml>=4 and we're at min offset - always legal, no need to check
							break;
						}
					}

					// if we didn't find a good match, check for a short LO match
					// this is relatively expensive, so only do it on the "slower" levels
					if ( t_step_literals_shift >= 5 && lo_ml >= NEWLZF_LOMML_NORMAL )
					{
						match_ptr = ptr + neg_lo0;
						match_end = ptr + lo_ml;
						match_offs = 0;
						neg_offset = neg_lo0;
						break;
					}

					// no match found, skip ahead
					// check if we can step as far as we want to
					SINTa step = scaled_skip_dist >> t_step_literals_shift;

					if ( rrPtrDiff(parse_end_ptr - ptr) <= step )
						goto parse_chunk_done;

					if (t_quadratic_skip)
					{
						// NOTE(fg): quadratic skip is _really_ aggressive, but might be worth it for really fast levels?
						// let's give a try.
						scaled_skip_dist += rrPtrDiff(ptr - literals_start)>>t_quadratic_skip;
						// clamp it so we don't get completely ridiculous...
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

				// normal match is < parse_end_ptr; lo0 can be == parse_end_ptr
				RR_ASSERT( ptr <= parse_end_ptr );

				if ( t_do_match_backup )
				{
					// try match start backup :
					while( ptr > literals_start )
					{
						if ( rrPtrDiff(hash_base - ptr) >= neg_offset ||
							 ptr[-1] != ptr[neg_offset-1] )
							break;

						ptr--;
					}
				}

				// encode the match
				int lrl = rrPtrDiff32( ptr - literals_start );
				int ml = rrPtrDiff32( match_end - ptr );
				newLZF_encoder_arrays_put_packet(&encarrays,ml,lrl,(S32)match_offs,(S32)-neg_lo0,literals_start);

				// update lastoffset
				neg_lo0 = neg_offset;

				ptr = match_end;
				literals_start = ptr;
				if ( ptr >= parse_end_ptr )
					break;

				// only do partial inserts if requested
				if ( t_do_partial_inserts )
				{
					const U8 * match_start = ptr - ml;
					SINTa base_pos = rrPtrDiff(match_start - hash_base);
					for ( SINTa i = 1; i < ml; i += i )
						fast_ctmf_insert(hash_table, match_start + i, hash_mul, hash_shift, base_pos + i);
				}
			}

parse_chunk_done:;
		} // if any bytes to parse

		newLZF_encoder_arrays_finish_parse_chunk(&encarrays,twice,literals_start,(S32)-neg_lo0);

	} // twice

	//===========================================================

	return newLZF_encoder_arrays_output(pJ,pchunktype,NULL,comp,comp_end,
		vtable,scratch,
		&encarrays,chunk_pos);
}

//=========================================================================

static SINTa newlzf_unpack_escape_offsets(const U8 * comp_base, const U8 * comp_end,
	U32 * offsets, SINTa offset_count , SINTa max_offset)
{
	if ( offset_count == 0 ) return 0;

	// offset == max_offset is ok, > max_offset is a fail
	SINTa fail_offset = max_offset+1;
	// off24 >= fail_offset means corruption
	// off24 >= NEWLZF_OFFSET_FOURBYTE_THRESHOLD means get another byte

	const U8 * comp_ptr = comp_base;
	
	if ( fail_offset < NEWLZF_OFFSET_FOURBYTE_THRESHOLD )
	{
		// no valid 4-byte offset can occur
		//	high offsets are fuzz failures, just detect them and return
	
		int i =0;
		for(;i<=offset_count-4;)
		{
			// check I can read 4*3 + overrun 1
			if ( comp_ptr + 13 > comp_end ) break;
		
			U32 off24;
			
			RR_UNROLL_4( \
				off24 = RR_GET24_LE_OVERRUNOK(comp_ptr); comp_ptr += 3;	\
				REQUIRE_FUZZ_RETURN( (SINTa)off24 <= max_offset , -1 ); \
				offsets[i] = off24; i++; \
			);
		}
	
		for(;i<offset_count;i++)
		{
			REQUIRE_FUZZ_RETURN( (comp_ptr+3 <= comp_end), -1 );
			
			U32 off24 = RR_GET24_LE_NOOVERRUN(comp_ptr);
			comp_ptr += 3;
			
			REQUIRE_FUZZ_RETURN( (SINTa)off24 <= max_offset , -1 );
			
			offsets[i] = off24;
		}
	}
	else
	{
		// all 3-byte offsets are fuzz-okay
		// only need to check for invalid high offsets in the 4-byte case
	
		int i =0;
		for(;i<=offset_count-4;)
		{
			// check I can read 4*4
			if ( comp_ptr + 16 > comp_end ) break;
		
			U32 off24;
			RR_UNROLL_4( \
			off24 = RR_GET24_LE_OVERRUNOK(comp_ptr); comp_ptr += 3; \
			if ( off24 >= NEWLZF_OFFSET_FOURBYTE_THRESHOLD ) { \
				U32 hi = *comp_ptr++; \
				off24 += (hi << NEWLZF_OFFSET_FOURBYTE_SHIFT); \
				REQUIRE_FUZZ_RETURN( (SINTa)off24 <= max_offset , -1 ); \
			} \
			offsets[i] = off24; i++; \
			);
		}
		
		for(;i<offset_count;i++)
		{
			REQUIRE_FUZZ_RETURN( (comp_ptr+3 <= comp_end), -1 );
			
			U32 off24 = RR_GET24_LE_NOOVERRUN(comp_ptr);
			comp_ptr += 3;
			if_likely ( off24 < NEWLZF_OFFSET_FOURBYTE_THRESHOLD )
			{
				RR_ASSERT( (SINTa)off24 <= max_offset );
				offsets[i] = off24;
			}
			else
			{
				REQUIRE_FUZZ_RETURN( comp_ptr < comp_end, -1 );
			
				U32 hi = *comp_ptr++;
				off24 += (hi << NEWLZF_OFFSET_FOURBYTE_SHIFT);
				REQUIRE_FUZZ_RETURN( (SINTa)off24 <= max_offset , -1 );
				offsets[i] = off24;
			}		
		}
	}
	
	return rrPtrDiff( comp_ptr - comp_base );
}


//=========================================================================

// scalar version of do_four_packets

/**

Macro version same as the above

Almost identical speed (692 vs 694 mb/s)
MSVC is good with the inline
so use it for now 

**/

#ifdef __RADARM64__

/**

A64 fiddled version

the idea is to take advantage of ARM conditional instructions for the offset selection

I want offset_flag to just be a bit TST , set a conditional, then do the offset updates from that

850 mb/s -> 875 mb/s

but this is pretty sensitive to random code-gen fiddling
so quite possible that's just heisenoptimization

 offset_flag = 0 means fetch offset (and ptr += 2)
 offset_flag = 1 means use last (rep)

**/

#define NEWLZF_BEXTR32(x,start,len) (((x) >> (start)) & ((1u << (len)) - 1))

#define NEWLZF_FOUR_SIMPLE_PACKETS_COPY_I(i) { \
		newlz_literals_copy8<NEWLZF_DECODE_LITERALS_TYPE>(to_ptr,to_ptr+neg_offset,literals_ptr); \
		U32 lrl = (U32)(several_packets & 7); \
		U32 offset_flag = (U32)(several_packets & 0x80); \
		U32 ml = (U32)((several_packets >> 3) & 0xf); \
		several_packets >>= 8; \
		to_ptr += lrl; literals_ptr += lrl; \
		S32 next_offset = -(S32) RR_GET16_LE_UNALIGNED(off16_ptr); \
		neg_offset = offset_flag ? neg_offset : next_offset; \
		off16_ptr += offset_flag ? 0 : 2; \
		const U8 * mp = to_ptr + neg_offset; \
		NEWLZF_FIRST_CHUNK_FUZZ( mp >= window_base ); \
		newlzf_copy16(to_ptr,mp); \
		to_ptr += ml; } \

#define NEWLZF_FOUR_SIMPLE_PACKETS_SCALAR() do { \
	U32 several_packets = RR_GET32_LE_UNALIGNED(packets_ptr); packets_ptr += 4; \
	RR_UNROLL_I_4(0, NEWLZF_FOUR_SIMPLE_PACKETS_COPY_I(i) ); \
} while(0)

#define NEWLZF_EIGHT_SIMPLE_PACKETS_SCALAR_PRELOADED(eight_packets) do { \
	U64 several_packets = eight_packets; packets_ptr += 8; \
	RR_UNROLL_I_8(0, NEWLZF_FOUR_SIMPLE_PACKETS_COPY_I(i) ); \
} while(0)

#else

/**

x86/x64 four packets

use partial register to grab bottom byte of dwords

turn the rep bit into a -1 all-bits mask to do branchless selects without cmov

**/

#define NEWLZF_FOUR_SIMPLE_PACKETS_COPY { \
		newlz_literals_copy8<NEWLZF_DECODE_LITERALS_TYPE>(to_ptr,to_ptr+neg_offset,literals_ptr); \
		U8 lrl = (U8)four_lrls; four_lrls >>= 8; \
		to_ptr += lrl; literals_ptr += lrl; \
		S32 next_offset = -(S32) RR_GET16_LE_UNALIGNED(off16_ptr); \
		S32 offset_mask = (S32)((U8)four_offsets) -1; four_offsets >>= 8; \
		neg_offset ^= (next_offset ^ neg_offset) & offset_mask; \
		off16_ptr += offset_mask&2; \
		const U8 * mp = to_ptr + neg_offset; \
		NEWLZF_FIRST_CHUNK_FUZZ( mp >= window_base ); \
		newlzf_copy16(to_ptr,mp); \
		U8 ml = (U8)four_mls; four_mls >>= 8; \
		to_ptr += ml; } \
		
#define NEWLZF_FOUR_SIMPLE_PACKETS_SCALAR() do { \
	U32 four_packets = RR_GET32_LE_UNALIGNED(packets_ptr); packets_ptr += 4; \
	U32 four_lrls = four_packets & 0x07070707UL; \
	U32 four_mls = (four_packets>>3) & 0x0F0F0F0FUL; \
	U32 four_offsets = (four_packets & 0x80808080UL)>>7; \
	RR_UNROLL_4( NEWLZF_FOUR_SIMPLE_PACKETS_COPY ); \
} while(0)

#endif

// four packets SCALAR
	
//===========================================================

/*

use the PSHUFB Mermaid decoder on Intel chips that have it

PSHUFB is actually SSSE3	
slightly weaker
but just test SSE41
(cuz I use insert_epi32 ; I could use unpack_epi64 instead)

The space of chips that have SSSE3 but not SSE41 is pretty tiny

*/

//===========================================================

#define NEWLZF_SIXTEEN_SIMPLE_PACKETS()	RR_UNROLL_4( NEWLZF_FOUR_SIMPLE_PACKETS_SCALAR() )
#define NEWLZF_FOUR_SIMPLE_PACKETS		NEWLZF_FOUR_SIMPLE_PACKETS_SCALAR

//===========================================================

// the non-SSE4 decoders :
#if ! defined(DO_SSE4_ALWAYS)

#define NEWLZF_DECODE_FIRST_CHUNK 0
#define NEWLZF_FIRST_CHUNK_FUZZ(expr)

#define NEWLZF_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_SUB
#define newLZF_decode_parse newLZF_decode_parse_sub

#include "newlzf_decode_parse_outer.inl"
		
#undef NEWLZF_DECODE_LITERALS_TYPE
#undef newLZF_decode_parse

#define NEWLZF_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_RAW
#define newLZF_decode_parse newLZF_decode_parse_raw

#include "newlzf_decode_parse_outer.inl"
		
#undef NEWLZF_DECODE_LITERALS_TYPE
#undef newLZF_decode_parse

#undef NEWLZF_DECODE_FIRST_CHUNK
#undef NEWLZF_FIRST_CHUNK_FUZZ
#define NEWLZF_DECODE_FIRST_CHUNK 1
#define NEWLZF_FIRST_CHUNK_FUZZ(expr)	REQUIRE_FUZZ_RETURN( expr , NULL )

#define NEWLZF_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_SUB
#define newLZF_decode_parse newLZF_decode_parse_sub_first

#include "newlzf_decode_parse_outer.inl"
		
#undef NEWLZF_DECODE_LITERALS_TYPE
#undef newLZF_decode_parse

#define NEWLZF_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_RAW
#define newLZF_decode_parse newLZF_decode_parse_raw_first

#include "newlzf_decode_parse_outer.inl"
		
#undef NEWLZF_DECODE_LITERALS_TYPE
#undef newLZF_decode_parse

#undef NEWLZF_DECODE_FIRST_CHUNK
#undef NEWLZF_FIRST_CHUNK_FUZZ


#endif // DO_SSE4_ALWAYS
		
static SINTa newLZF_decode_chunk_phase1(int chunk_type,const U8 * comp,const U8 * comp_end,U8 * chunk_ptr,SINTa chunk_len, SINTa chunk_pos, 
	U8 * scratch_space,U8 * scratch_end,
	newLZF_chunk_arrays * arrays)
{
	CHECK( const U8 * check_buf = NULL ); 

	SIMPLEPROFILE_SCOPE_N(newLZF_decode_chunkP1,chunk_len);
	
	if ( chunk_type > 1 ) return -1;
	
	RR_ASSERT( chunk_type == NEWLZ_LITERALS_TYPE_RAW || chunk_type == NEWLZ_LITERALS_TYPE_SUB );

	/*
	#ifndef __RADFUZZ__
	memset(scratch_space,-1,rrPtrDiff(scratch_end - scratch_space));
	#endif
	/**/
	
	newlz_array_get_printf("newlzf chunk type %d = %s [%d->%d]\n",chunk_type,newlz_literals_type_name[chunk_type],chunk_len,rrPtrDiff32(comp_end-comp));

	// check for in-place decode comp-raw overlap :	
	bool inplace_comp_raw_overlap;
	//inplace_comp_raw_overlap = RR_MIN(comp_end,chunk_ptr+chunk_len) >= RR_MAX(comp,chunk_ptr);
	inplace_comp_raw_overlap = comp <= (chunk_ptr+chunk_len) && chunk_ptr <= comp_end;
	
	//rrprintf("newLZF_decode_chunk : %d\n",chunk_len);

	const U8 * comp_ptr = comp;

	// 2 array headers * 3 byte length = 6
	// 3 byte off24 header
	// + at least 1 byte of payload
	// = 10
	// (this also protects the first-8 raw byte read)
	REQUIRE_FUZZ_RETURN( 10 <= rrPtrDiff(comp_end - comp_ptr), - 1 );

	CHECK( const U8 * check_ptr = check_buf );
	
	U8 * to_ptr = chunk_ptr;
	//U8 * chunk_end = to_ptr + chunk_len;
	
	//int start_pos = 0;
	if ( chunk_pos == 0 )
	{
		//start_pos = NEWLZF_MIN_OFFSET;
		
		// in-place issue ; to_ptr should never overlap comp here because of in-place padding
		
		RR_COMPILER_ASSERT( NEWLZF_MIN_OFFSET == 8 );
		lz_copy8(to_ptr,comp_ptr);
		to_ptr += 8;
		comp_ptr += 8;
	}
	
	//---------------------------------------------------
	// get literals :
		
	U8 * scratch_ptr = U8_void(scratch_space);
		
	U8 * literals = scratch_ptr;
						
	SINTa literals_count;
	{
		newlz_array_get_printf("literals : ");
						
		SIMPLEPROFILE_SCOPE(get_literals);
		SINTa literals_comp_len = newLZ_get_array(&literals,comp_ptr,comp_end,&literals_count,
										RR_MIN(chunk_len,rrPtrDiff(scratch_end-scratch_ptr)),
										inplace_comp_raw_overlap,
										scratch_ptr,scratch_end);
		if ( literals_comp_len < 0 )
			return -1;
		comp_ptr += literals_comp_len;
		SIMPLEPROFILE_SCOPE_SETCOUNT(get_literals,literals_count);
		
		newlz_array_get_printf("\n");
	}
		
	scratch_ptr += literals_count;
	
	//---------------------------------------------------
	// get packets :

	newlz_array_get_printf("packets : ");
							
	U8 * packets_base = scratch_ptr;
	SINTa packets_count;
	{
		SIMPLEPROFILE_SCOPE(get_packets);
		SINTa packets_comp_len = newLZ_get_array(&packets_base,comp_ptr,comp_end,&packets_count,
										RR_MIN(chunk_len,rrPtrDiff(scratch_end-scratch_ptr)),
										inplace_comp_raw_overlap,
										scratch_ptr,scratch_end);
		if ( packets_comp_len < 0 )
			return -1;
		comp_ptr += packets_comp_len;
		SIMPLEPROFILE_SCOPE_SETCOUNT(get_packets,packets_count);
	}
		
	newlz_array_get_printf("\n");
		
	scratch_ptr += packets_count;
	arrays->packets_count = packets_count;
	
	//---------------------------------------------------
	// get 64k parse chunk packet count split :
	
	if ( chunk_len > (1<<16) )
	{
		// @@ could skip this when packets_count == 0
		REQUIRE_FUZZ_RETURN( comp_ptr+2 <= comp_end, -1 );
		arrays->packets_count1 = RR_GET16_LE_UNALIGNED(comp_ptr);
		comp_ptr += 2;
		
		RR_ASSERT_IF_NOT_CORRUPT( arrays->packets_count1 >= 0 && arrays->packets_count1 <= packets_count );
	}
	else
	{
		arrays->packets_count1 = packets_count;
	}

	// packets_count == 0 and packets_count1 == 0 ARE allowed
	// but negative packet counts in either half are not
	if ( arrays->packets_count1 > packets_count )
		return -1;

	//---------------------------------------------------
	// get off16's :

	// get offset count headers :
	REQUIRE_FUZZ_RETURN( 2 <= rrPtrDiff(comp_end - comp_ptr), -1 );
				
	U32 num_off16s = RR_GET16_LE_UNALIGNED(comp_ptr);
	comp_ptr += 2;
	
	if ( num_off16s == 0xFFFF )
	{
		SIMPLEPROFILE_SCOPE_N(newLZF_get_off16s,1);
	
		// get huffman off16's
		
		newlz_array_get_printf("off16s_hi : ");
	
		U8 * off16s_hi = scratch_ptr;
		SINTa off16s_hi_count;
		SINTa off16s_hi_comp_len = newLZ_get_array(&off16s_hi,comp_ptr,comp_end,&off16s_hi_count,
										RR_MIN(chunk_len/2,rrPtrDiff(scratch_end-scratch_ptr)),false,
										scratch_ptr,scratch_end);
		if ( off16s_hi_comp_len < 0 )
			return -1;
		comp_ptr += off16s_hi_comp_len;
		scratch_ptr += off16s_hi_count;
		
		newlz_array_get_printf("\n");
		
		newlz_array_get_printf("off16s_lo : ");
		
		U8 * off16s_lo = scratch_ptr;
		SINTa off16s_lo_count;
		SINTa off16s_lo_comp_len = newLZ_get_array(&off16s_lo,comp_ptr,comp_end,&off16s_lo_count,
										RR_MIN(chunk_len/2,rrPtrDiff(scratch_end-scratch_ptr)),false,
										scratch_ptr,scratch_end);
		if ( off16s_lo_comp_len < 0 )
			return -1;
		comp_ptr += off16s_lo_comp_len;
		scratch_ptr += off16s_lo_count;
		
		newlz_array_get_printf("\n");
		
		REQUIRE_FUZZ_RETURN(off16s_hi_count == off16s_lo_count, -1);
		
		num_off16s = (U32)off16s_hi_count;
		
		SIMPLEPROFILE_SCOPE_SETCOUNT(newLZF_get_off16s,num_off16s);
		
		//	I need 4 bytes per offset, offsets could occur every 4 bytes,
		//	so I need a total of "chunk_len"
		//	-> in theory I could exceed the scratch size here
		//	-> encoder needs to ensure that I will fit
		//	-> yes done now (decoder_scratch_space_needed)
		
		// off16's go in scratch :
		// align up :
		if ( (UINTa)scratch_ptr&1 ) scratch_ptr++;
		// check for room :
		REQUIRE_FUZZ_RETURN( 2*(SINTa)num_off16s <= rrPtrDiff( scratch_end - scratch_ptr ), -1 );
		arrays->off16_ptr = scratch_ptr;
		scratch_ptr += 2 * num_off16s;
		
		U16 * off16s = (U16 *) arrays->off16_ptr;

		// merge off16s hi and lo :

		// off16s are grabbed from this array LE (not native) so make sure I put them there that way

		simd_interleave_8x2(off16s,num_off16s,off16s_lo,off16s_hi);	
	}
	else
	{
		REQUIRE_FUZZ_RETURN( 2*(SINTa)num_off16s <= rrPtrDiff( comp_end - comp_ptr ), -1 );
		
		if ( inplace_comp_raw_overlap )
		{
			// must copy out offsets for in-place decode :
			if ( rrPtrDiff(scratch_end-scratch_ptr) < (SINTa)( 2 * num_off16s ) )
				return -1;
		
			memcpy(scratch_ptr,comp_ptr,2 * num_off16s);
			arrays->off16_ptr = scratch_ptr;
			scratch_ptr += 2 * num_off16s;
		}
		else
		{
			arrays->off16_ptr = comp_ptr;
		}
		
		comp_ptr += 2 * num_off16s;
	}
	
	arrays->off16_end = arrays->off16_ptr + 2 * num_off16s;
	
	//---------------------------------------------------
	// get off24's :
		
	REQUIRE_FUZZ_RETURN( 3 <= rrPtrDiff( comp_end - comp_ptr ), -1 );
	
	U32 off24_header = RR_GET24_LE_NOOVERRUN(comp_ptr);
	comp_ptr += 3;	
	
	if ( off24_header == 0 )
	{
		arrays->escape_offsets_count1 = 0;
		arrays->escape_offsets_count2 = 0;
		
		// give them a pointer full of zeros anyway :
		// can both point at the same bunch of zeros :
		scratch_ptr = rrAlignUpPointer(scratch_ptr,4);
		arrays->escape_offsets1 = (U32 *)scratch_ptr;
		arrays->escape_offsets2 = arrays->escape_offsets1;
		
		SINTa scratch_bytes_needed = sizeof(U32)*NEWLZF_ESCAPE_OFFSET_PAD_ZERO_COUNT;
		
		REQUIRE_FUZZ_RETURN( scratch_bytes_needed <= rrPtrDiff( scratch_end - scratch_ptr ), -1 );
		
		memset(scratch_ptr,0,scratch_bytes_needed);
		scratch_ptr += scratch_bytes_needed;
	}
	else
	{
		U32 off24_ptr_chunk_count1 = off24_header>>12;
		U32 off24_ptr_chunk_count2 = off24_header & 0xFFF;
		
		// extremely unlikely excess case :
		if ( off24_ptr_chunk_count1 == 0xFFF )
		{
			REQUIRE_FUZZ_RETURN( 2 <= rrPtrDiff( comp_end - comp_ptr ), -1 );
			off24_ptr_chunk_count1 = RR_GET16_LE_UNALIGNED(comp_ptr);
			comp_ptr += 2;
		}
		if ( off24_ptr_chunk_count2 == 0xFFF )
		{
			REQUIRE_FUZZ_RETURN( 2 <= rrPtrDiff( comp_end - comp_ptr ), -1 );
			off24_ptr_chunk_count2 = RR_GET16_LE_UNALIGNED(comp_ptr);
			comp_ptr += 2;
		}
		
		// @@ this time is near zero; remove it :
		//SIMPLEPROFILE_SCOPE_N(newLZF_get_off24s,(off24_ptr_chunk_count1+off24_ptr_chunk_count2));
		
		arrays->escape_offsets_count1 = off24_ptr_chunk_count1;
		arrays->escape_offsets_count2 = off24_ptr_chunk_count2;

		// reserve some space in scratch for the offsets :
		// check for room available first
		SINTa scratch_bytes_needed = sizeof(U32)*(off24_ptr_chunk_count1 + off24_ptr_chunk_count2 +
			2 * NEWLZF_ESCAPE_OFFSET_PAD_ZERO_COUNT );
		
		REQUIRE_FUZZ_RETURN( scratch_bytes_needed <= rrPtrDiff( scratch_end - scratch_ptr ), -1 );
		
		scratch_ptr = rrAlignUpPointer(scratch_ptr,4);
		arrays->escape_offsets1 = (U32 *)scratch_ptr;
		scratch_ptr += sizeof(U32)*off24_ptr_chunk_count1;
		// pad some zeros so that inner loop can prefetch offsets past the end :
		memset(scratch_ptr,0,sizeof(U32)*NEWLZF_ESCAPE_OFFSET_PAD_ZERO_COUNT);
		scratch_ptr += sizeof(U32)*NEWLZF_ESCAPE_OFFSET_PAD_ZERO_COUNT;		

		arrays->escape_offsets2 = (U32 *)scratch_ptr;
		scratch_ptr += sizeof(U32)*off24_ptr_chunk_count2;
		memset(scratch_ptr,0,sizeof(U32)*NEWLZF_ESCAPE_OFFSET_PAD_ZERO_COUNT);
		scratch_ptr += sizeof(U32)*NEWLZF_ESCAPE_OFFSET_PAD_ZERO_COUNT;	
		
		#ifdef SPEEDFITTING

		const U8 * comp_ptr_save = comp_ptr;

		for(int speedfit_iter=0;;speedfit_iter++)
		{
		comp_ptr = comp_ptr_save;
		U64 t1 = speedfitter_ticks_start();
		
		#endif
	
		// this needs to be two calls to newlzf_unpack_escape_offsets
		//	because they have different parse chunk base positions for fuzz checks

		#ifdef DO_BUILD_SSE4
		if ( rrsimd_has_sse4() )
		{
			SINTa off24_data_len1 = newlzf_unpack_escape_offsets_sse4(comp_ptr,comp_end,arrays->escape_offsets1,off24_ptr_chunk_count1,chunk_pos);
			if ( off24_data_len1 < 0 ) return -1;		
			comp_ptr += off24_data_len1;
			
			SINTa off24_data_len2 = newlzf_unpack_escape_offsets_sse4(comp_ptr,comp_end,arrays->escape_offsets2,off24_ptr_chunk_count2,chunk_pos+(1<<16));
			if ( off24_data_len2 < 0 ) return -1;	
			comp_ptr += off24_data_len2;
		}
		else
		#endif
		{			
			SINTa off24_data_len1 = newlzf_unpack_escape_offsets(comp_ptr,comp_end,arrays->escape_offsets1,off24_ptr_chunk_count1,chunk_pos);
			if ( off24_data_len1 < 0 ) return -1;	
			comp_ptr += off24_data_len1;
			
			SINTa off24_data_len2 = newlzf_unpack_escape_offsets(comp_ptr,comp_end,arrays->escape_offsets2,off24_ptr_chunk_count2,chunk_pos+(1<<16));
			if ( off24_data_len2 < 0 ) return -1;	
			comp_ptr += off24_data_len2;
		}
		
		#ifdef SPEEDFITTING
		
		if ( g_speedfitter_stage != 2 )
			break;

		U64 t2 = speedfitter_ticks_end();
		
		speedfitter_stage2_collect_newlzf_unpack_escape_offsets(speedfit_iter,(int)(off24_ptr_chunk_count1+off24_ptr_chunk_count2),t2-t1);
		
		if ( speedfit_iter > c_speedfit_stage2_num_repeats )
			break;

		}
		
		#endif
	}
	
	//---------------------------------------------------

	// comp_ptr is now where the excess-len bytes start (coded with newlzf_putv)

	if ( inplace_comp_raw_overlap )
	{
		// have to copy excesses to scratch :
		//	(actually I think this is unnecessary for the excesses, it was the off16's that were the problem)
		//	but do it for consistency
		
		SINTa excesses_len = rrPtrDiff( comp_end - comp_ptr );
		if ( (scratch_end - scratch_ptr) < excesses_len )
		{
			return -1;
		}
		
		memcpy(scratch_ptr, comp_ptr, excesses_len);
		
		arrays->excesses_ptr = scratch_ptr;
		scratch_ptr += excesses_len;
		arrays->excesses_end = scratch_ptr;
	}
	else
	{
		arrays->excesses_ptr = comp_ptr;
		arrays->excesses_end = comp_end;
	}
	
	// this is guaranteed :
	RR_ASSERT( scratch_ptr <= scratch_end );
				
	arrays->chunk_ptr = chunk_ptr;
	arrays->scratch_ptr = scratch_space;
	arrays->packets_ptr = packets_base;
	arrays->packets_end = packets_base + packets_count;
	arrays->literals_ptr = literals;
	arrays->literals_end = literals + literals_count;
	
	return 1;
}
		
static RADINLINE SINTa newLZF_decode_chunk_phase2(int chunk_type,const U8 * comp_base,const U8 * comp_end,
	U8 * whole_chunk_ptr,SINTa whole_chunk_len, SINTa chunk_pos,
	U8 * block_end,
	const newLZF_chunk_arrays * const_arrays)
{
	//U8 * whole_chunk_end = whole_chunk_ptr + whole_chunk_len;
	RR_ASSERT( block_end >= whole_chunk_ptr + whole_chunk_len );
	SIMPLEPROFILE_SCOPE_N(newLZF_decode_chunkP2,whole_chunk_len);
	
	#ifdef SPEEDFITTING
	U64 t1 = speedfitter_ticks_start();
	#endif
	
	newLZF_chunk_arrays local_arrays = *const_arrays;
	newLZF_chunk_arrays * arrays = &local_arrays;

	SINTa chunk_len1 = RR_MIN((1<<16),whole_chunk_len);
	SINTa chunk_len2 = whole_chunk_len - chunk_len1;
	
	SINTa packets_count1 = arrays->packets_count1;	
	SINTa packets_count2 = arrays->packets_count - packets_count1;
	
	U8 * window_base = whole_chunk_ptr - chunk_pos;
	
	// excesses no longer always point in comp :
	//RR_ASSERT( arrays->excesses_ptr <= comp_end );
		
	S32 neg_offset = - NEWLZF_MIN_OFFSET;

	const U8 * comp_ptr = NULL; // really just a bool return value now

	for(int twice=0;twice<2;twice++)
	{
		U8 * chunk_ptr = whole_chunk_ptr;
		SINTa chunk_len = chunk_len1;
		
		if ( twice == 0 )
		{
			chunk_ptr = whole_chunk_ptr;
			chunk_len = chunk_len1;
		
			arrays->escape_offsets_ptr = arrays->escape_offsets1;	
			arrays->escape_offsets_end = arrays->escape_offsets1 + arrays->escape_offsets_count1;
			
			arrays->packets_end = arrays->packets_ptr + packets_count1;
		}
		else
		{
			// only one chunk :
			if ( chunk_len2 == 0 )
				break;
			
			chunk_ptr = whole_chunk_ptr + chunk_len1;
			chunk_len = chunk_len2;
			
			arrays->escape_offsets_ptr = arrays->escape_offsets2;	
			arrays->escape_offsets_end = arrays->escape_offsets2 + arrays->escape_offsets_count2;

			arrays->packets_ptr += packets_count1;
			arrays->packets_end = arrays->packets_ptr + packets_count2;
		}

#ifdef DO_NEWLZF_DUMP
		// dump decoder state after first 128k to file (for stand-alone tests)
		//static const int dump_pos = 128*1024;
		static const int dump_pos = 0*1024;
		if ( chunk_pos == dump_pos && twice == 0 )
		{
			if ( FILE *f = fopen("dump.dat", "wb") )
			{
				U32 counts[6];

				// Cover initial seed data
				int initial_histo_size = (dump_pos == 0) ? 8 : dump_pos;

				// history
				fwrite(window_base, 1, initial_histo_size, f);

				// counts
				counts[0] = 0-neg_offset;
				counts[1] = (U32) (UINTa) (arrays->escape_offsets_end - arrays->escape_offsets_ptr);
				counts[2] = (U32) (UINTa) (arrays->excesses_end - arrays->excesses_ptr);
				counts[3] = (U32) (UINTa) (arrays->packets_end - arrays->packets_ptr);
				counts[4] = (U32) (UINTa) (arrays->literals_end - arrays->literals_ptr);
				counts[5] = (U32) (UINTa) (arrays->off16_end - arrays->off16_ptr);
				fwrite(counts, 1, sizeof(counts), f);

				rrprintf("initoffs=%u nesco=%u nexc=%u npak=%u nlit=%u nofs=%u cktype=%d\n",
						 counts[0], counts[1], counts[2], counts[3], counts[4], counts[5], chunk_type);

				// the data itself
				fwrite(arrays->escape_offsets_ptr, sizeof(U32), counts[1], f);
				fwrite(arrays->excesses_ptr, 1, counts[2], f);
				fwrite(arrays->packets_ptr, 1, counts[3], f);
				fwrite(arrays->literals_ptr, 1, counts[4], f);
				fwrite(arrays->off16_ptr, 1, counts[5], f);

				fclose(f);
			}
		}
#endif
		
		// these are advanced by the decoder :
		//	I don't explicitly count the # in chunk1 & chunk2
		//arrays->literals_ptr
		//arrays->off16_ptr
		//arrays->excesses_ptr
	
		if ( chunk_pos == 0 && twice == 0 )
		{
			RR_ASSERT_IF_NOT_CORRUPT( arrays->escape_offsets_count1 == 0 );
		
			#if defined(DO_SSE4_TEST) || defined(DO_SSE4_ALWAYS)
			if ( rrsimd_has_sse4() )
			{
				if ( chunk_type == NEWLZ_LITERALS_TYPE_RAW )
				{
					comp_ptr = newLZF_decode_parse_raw_sse4_first(chunk_ptr,chunk_len,
										block_end,window_base,
										comp_end,
										arrays,&neg_offset
										);	
				}
				else
				{	
					comp_ptr = newLZF_decode_parse_sub_sse4_first(chunk_ptr,chunk_len,
										block_end,window_base,
										comp_end,
										arrays,&neg_offset
										);	
				}	
			}
			else
			#endif
			#ifdef DO_SSE4_ALWAYS
			{
				// no fallback
			}
			#else
			{
				// non-sse4 path :
				if ( chunk_type == NEWLZ_LITERALS_TYPE_RAW )
				{
					comp_ptr = newLZF_decode_parse_raw_first(chunk_ptr,chunk_len,
										block_end,window_base,
										comp_end,
										arrays,&neg_offset
										);	
				}
				else
				{	
					comp_ptr = newLZF_decode_parse_sub_first(chunk_ptr,chunk_len,
										block_end,window_base,
										comp_end,
										arrays,&neg_offset
										);	
				}
			}
			#endif
		}
		else
		{
			#if defined(DO_SSE4_TEST) || defined(DO_SSE4_ALWAYS)
			if ( rrsimd_has_sse4() )
			{
				if ( chunk_type == NEWLZ_LITERALS_TYPE_RAW )
				{
					comp_ptr = newLZF_decode_parse_raw_sse4(chunk_ptr,chunk_len,
										block_end,window_base,
										comp_end,
										arrays,&neg_offset
										);	
				}
				else
				{	
					comp_ptr = newLZF_decode_parse_sub_sse4(chunk_ptr,chunk_len,
										block_end,window_base,
										comp_end,
										arrays,&neg_offset
										);	
				}
			}
			else
			#endif
			#ifdef DO_SSE4_ALWAYS
			{
				// no fallback
			}
			#else
			{
				// non-sse4 path :
				if ( chunk_type == NEWLZ_LITERALS_TYPE_RAW )
				{
					comp_ptr = newLZF_decode_parse_raw(chunk_ptr,chunk_len,
										block_end,window_base,
										comp_end,
										arrays,&neg_offset
										);	
				}
				else
				{	
					comp_ptr = newLZF_decode_parse_sub(chunk_ptr,chunk_len,
										block_end,window_base,
										comp_end,
										arrays,&neg_offset
										);	
				}
			}
			#endif
		
		}
		
		// error in parse :	
		if ( comp_ptr == NULL )
			return -1;
				
		RR_ASSERT( comp_ptr <= comp_end );
		
	} // twice
	
	// verify complen :
	REQUIRE_FUZZ_RETURN( comp_ptr == comp_end , -1 );

	// verify that all arrays were fully consumed :
	REQUIRE_FUZZ_RETURN( arrays->off16_ptr == arrays->off16_end , -1 );
	REQUIRE_FUZZ_RETURN( arrays->literals_ptr == arrays->literals_end , -1 );
	REQUIRE_FUZZ_RETURN( arrays->excesses_ptr == arrays->excesses_end , -1 );
	
	
	#ifdef SPEEDFITTING
	
	if ( g_speedfitter_stage == 3 )
	{	
		U64 t2 = speedfitter_ticks_end();
		
		// gather stats:
		
		int num_escapes = 0;
		
		const U8 * p = const_arrays->packets_ptr;
		const U8 * p_end = p + const_arrays->packets_count;
		while(p<p_end)
		{		
			int packet = *p++;				
			if ( packet < 24 )
				num_escapes++;
		}
		
		//int num_off16s = (off16_end - off16_ptr)/2;
		//int num_escape_offsets = escape_offsets_count1 + escape_offsets_count2;
		
		SINTa num_literals = const_arrays->literals_end - const_arrays->literals_ptr;

		/*		
		int offset_blend_score = 0;
		
		SINTa num_escape_offsets = 
			const_arrays->escape_offsets_count1 +
			const_arrays->escape_offsets_count2;
		
		if ( num_escape_offsets )
		{		
			U32 * poff; SINTa offc;
				
			poff = const_arrays->escape_offsets1;
			offc = const_arrays->escape_offsets_count1;
			while(offc--)
			{
				offset_blend_score += newLZ_offset_blend_score(*poff++) + 1;
			}
			poff = const_arrays->escape_offsets2;
			offc = const_arrays->escape_offsets_count2;
			while(offc--)
			{
				offset_blend_score += newLZ_offset_blend_score(*poff++) + 1;
			}
		}

		/**/
		
		speedfitter_stage3_collect_parse_newlzf(whole_chunk_ptr,(t2-t1),chunk_type,
			whole_chunk_len,
			num_literals,const_arrays->packets_count,
			num_escapes);
			//offset_blend_score);
			//num_escape_offsets);
	}

	#endif
						 
	return 1;
}

S32 Mermaid_DecodeOneQuantum(U8 * decomp,U8 * decomp_end,const U8 * comp,S32 quantumCompLen,const U8 * compBufEnd,SINTa pos_since_reset,
	void * scratch,SINTa scratch_size,OodleLZ_Decode_ThreadPhase threadPhase)
{
	SINTa decomp_len = rrPtrDiff( decomp_end - decomp );
	RR_ASSERT( decomp_len > 0 && decomp_len <= OODLELZ_BLOCK_LEN );
	decomp_len; // unused

	rrPrintf_v2("DBLOCK : %d : %d : %d\n",pos_since_reset,decomp_len,quantumCompLen);
		
	SIMPLEPROFILE_SCOPE_N(newLZF_decode,decomp_len);
		
	U8 * scratch_ptr = U8_void(scratch);
	U8 * scratch_end = scratch_ptr + scratch_size;

	U8 * rawPtr = decomp;
	U8 * rawEnd = decomp_end;
	const U8 * compPtr = U8_void(comp);
	const U8 * compEnd = compPtr + quantumCompLen;
	RR_ASSERT( compEnd <= compBufEnd );
	//const U8 * checkPtr = U8_void(checkbuf);

	// LZQH no flags set -> block len = 128k
	const int newlz_chunk_len = NEWLZF_CHUNK_LEN;
	
	while(rawPtr<rawEnd)
	{
		SINTa chunk_len = RR_MIN( newlz_chunk_len , (rawEnd - rawPtr) );
		SINTa chunk_pos = rrPtrDiff( rawPtr - U8_void(decomp) ) + pos_since_reset;
		
		if ( 4 > rrPtrDiff( compEnd - compPtr ) )
			return -1;
		
		SINTa chunk_comp_len = RR_GET24_BE_OVERRUNOK(compPtr);
		
		if ( chunk_comp_len >= (1<<23) )
		{
			SINTa chunk_type = (chunk_comp_len>>19)&0xF;
		
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
				
				//raw 
				if ( threadPhase & OodleLZ_Decode_ThreadPhase1 )
				{
					// has to be memmove for in-place decoding
					//memcpy(rawPtr,compPtr,chunk_len);
					memmove(rawPtr,compPtr,chunk_len);
				}
			}
			else
			{
				RR_ASSERT_IF_NOT_CORRUPT( chunk_len >= NEWLZF_MIN_CHUNK_LEN );

				// Encoder will only ever produce Mermaid chunks above this
				// min size, don't accept anything less
				REQUIRE_FUZZ_RETURN( chunk_len >= NEWLZF_MIN_CHUNK_LEN , -1 );
				
				// check scratch size meets decoder needs
				
				SINTa scratch_expected = OodleLZ_Compressor_ScratchMemSize(OodleLZ_Compressor_Mermaid,chunk_len);
						
				if ( rrPtrDiff(scratch_end - scratch_ptr) < scratch_expected )
				{
					ooLogError("decoder scratch too small\n");
					return -1;
				}		
						
				U8 * chunk_scratch_end = scratch_ptr + scratch_expected;
				
				newLZF_chunk_arrays * arrays = (newLZF_chunk_arrays *) scratch_ptr;
				scratch_ptr += sizeof(newLZF_chunk_arrays);
				
				/**
				
				Mermaid/Selkie in-place decoding :
				
				force phase1 to put all its arrays in scratch
					(uncompressed arrays can't just point at comp)
				
				phase2 always fills raw buff from scratch
				
				phase2 also reads "excesses" bytes from the end of comp
				raw output can never run ahead and run into the excesses
				because one excesses byte corresponds to many raw bytes
				
				**/
				
				if ( threadPhase & OodleLZ_Decode_ThreadPhase1 )
				{
					if ( newLZF_decode_chunk_phase1((int)chunk_type,compPtr,chunk_comp_end,rawPtr,chunk_len,chunk_pos,scratch_ptr,chunk_scratch_end,arrays) < 0 )
					{
						// wipe out the newLZ_chunk_arrays in scratch :
						RR_ZERO(*arrays);
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
				
					if ( newLZF_decode_chunk_phase2((int)chunk_type,compPtr,chunk_comp_end,rawPtr,chunk_len,chunk_pos,decomp_end,arrays) < 0 )
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
			
			// huff chunks go through here
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


//=================================================================================

// small win from doing 6 instead of 4
// hurts (encode) speed & memory use a tiny bit

// NEWLZ_MATCH_NUM_PAIRS must match NEWLZF_MATCH_NUM_PAIRS for Hydra

#define NEWLZF_MATCH_NUM_PAIRS	4 // average : 223,542,223 ->98,120,807 =  3.511 bpb =  2.278 to 1 
//#define NEWLZF_MATCH_NUM_PAIRS	6	// average : 223,542,223 ->98,065,847 =  3.510 bpb =  2.280 to 1
//#define NEWLZF_MATCH_NUM_PAIRS	32
// newlzf ST vs. LzFind speed on webster :
// tried bumping this up
// the idea that with a low "num_pairs" you get longest first
//	so you might not even get to the low offset choices
// -> nope
// -> more num_pairs gives more compression
//	-> but doesn't help speed or stop the off > 1M choices

//=================================================
/***

OPTIMAL PARSE

****/

// 64*8*128*1024
// dangerously close to 32-bit
// could go over with penalties

// COST_ONE_BYTE = 256

#define COST_PENALTY_SCALE	8

#define COST_SELKIE_PACKET	(COST_ONE_BYTE + COST_PENALTY_PACKET)

//*

// penalties to bias parse towards faster decodes
//  @@ -> not really tweaked, not finding a great win here
#define COST_PENALTY_HUFF			0	// penalty per huff literal
#define COST_PENALTY_PACKET			(2*COST_PENALTY_SCALE+1)  // penalty per packet; fewer packets = less mode switching
#define COST_PENALTY_EXCESS			(3*COST_PENALTY_SCALE)	// excesses take time (newlzf_getv)
#define COST_PENALTY_ESCAPE			(8*COST_PENALTY_SCALE)	// escapes
#define COST_PENALTY_OFF24			(16*COST_PENALTY_SCALE)	// on top of escape penalty!
#define COST_PENALTY_OFF24_1M		(16*COST_PENALTY_SCALE)	// on top of escape penalty and COST_PENALTY_OFF24

#define COST_PENALTY_OFF24_4M		(64*COST_PENALTY_SCALE)	// on top of escape penalty and COST_PENALTY_OFF24

#define COST_PENALTY_NORMAL_MATCH	(0)   // <- yes something here


/*/

//  disabled penalties
//	to verify code costs
//	-> yes they seem to be right

#define COST_PENALTY_HUFF			0	// penalty per huff literal
#define COST_PENALTY_PACKET			0  // penalty per packet; fewer packets = less mode switching
#define COST_PENALTY_EXCESS			0	// excesses take time (newlzf_getv)
#define COST_PENALTY_ESCAPE			0	// escapes
#define COST_PENALTY_NORMAL_MATCH	0   // normal offsets don't take any time
#define COST_PENALTY_NORMAL_L1		0	// penalty for offsets that go out of cache
#define COST_PENALTY_OFF24			0
#define COST_PENALTY_OFF24_1M		0
#define COST_PENALTY_OFF24_4M		0
#define COST_PENALTY_ZEROML_PACKET	0

/**/

struct newLZF_MatchParseRecord
{
	UnpackedMatchPair	pairs[NEWLZF_MATCH_NUM_PAIRS];	
};

/***

newLZ_get_match_heuristic(newLZF_MatchParseRecord) -
code dupe from newLZ_get_match_heuristic(CTMF)

simple heuristic greedy parse to seed the optimal parse

-> NOTE : the heuristics here are what establishes our statistics baseline
->   this strongly guides the optimal parse

***/
static RADINLINE bool newLZF_get_match_heuristic(
	match * pmatch,
	const newLZF_MatchParseRecord & matches, 
	S32 lastoffset, const U8 * ptr, const U8 * ptr_matchend, 
	int lrl, int mml, const S32 offsLimitTab[32],
	const U8 * dictionaryBase, // dictionaryBase : match ptr >= this
	U32 dictionarySize // dictionarySize : (optional) match offset limit
	)
{
	//int lomml = mml/2; // 4 -> 2 , 8 -> 4
	int lomml = 2;
	
	// longer MML if we're in an excess
	if ( lrl >= NEWLZF_LRL_EXCESS )
	{
		// +1 ? or more ?
		//lomml += lomml/2;
		lomml = mml-1;
		mml += mml/2; // /4 ?
	}
		
	U32 ptr32 = RR_GET32_NATIVE_UNALIGNED(ptr);
	
	// check LOs
	// check MML2 ; rep0len1 will be found in  do_lomml1_for_lrl
	S32 lo_ml = getmatchlen_mml2_one32(ptr32,ptr,ptr-lastoffset,ptr_matchend);
	
	if ( lo_ml < lomml ) lo_ml = 0;
	
	//		lo_off = offset;

	//U32 cur_absolute_pos = (U32)rrPtrDiff(ptr - dictionaryBase);
	
	// pretty aggressively just take LO if it's non-trivial
	//	don't even look for non-LO match
	#define NEWLZF_LOML_GOOD_ENOUGH	4
	//  NOTE : this strongly biases the optimal parse statistics!
	if ( lo_ml >= NEWLZF_LOML_GOOD_ENOUGH )
	{
		// just take the LO match with no normal search :
		*pmatch = match { lo_ml, 0 };
		return true;
	}
	
	S32 bestml = 0;
	U32 bestoff = 0;
				
	for(int m=0;m<NEWLZF_MATCH_NUM_PAIRS;m++)
	{
		S32 len = matches.pairs[m].length;
		
		// matches are in descending length
		// so when we hit a short one, get out
		if ( len < mml )
			break;
		
		if ( ptr+len > ptr_matchend )
		{
			len = rrPtrDiff32(ptr_matchend - ptr);
			if ( len < mml )
				break;
		}
		
		U32 offset = matches.pairs[m].offset;
		
		// verify match :
		NEWLZF_ASSERT_HEAVY( memcmp(ptr,ptr-(SINTa)offset,len) == 0 );
		
		if ( offset >= dictionarySize )
			continue;
			
		// force all offsets >= 8
		if ( offset < NEWLZF_MIN_OFFSET )
		{
			offset = newLZF_FixSmallOffset(offset);
			if ( (SINTa)offset > rrPtrDiff(ptr - dictionaryBase) )
				continue;
				
			const U8 * vs_ptr = ptr - offset;
			
			len = getmatchlen_mml4_one32(ptr32,ptr,vs_ptr,ptr_matchend);
							
			if ( len < mml )
				continue;
		}
				
		if ( newLZF_IsAllowedNormalMatch(len,offset,offsLimitTab) &&
			 newLZF_IsNormalMatchBetter(len,offset,bestml,bestoff) )
		{
			bestml = len;
			bestoff = offset;
		}		
	}
	
	if ( newLZF_IsLOMatchBetter(lo_ml,bestml,bestoff) )
	{
		// take the LO match :
		*pmatch = match { lo_ml, 0 };
		return true;
	}
	else
	{
		// normal match :
		if ( bestoff != 0 )
		{
			*pmatch = match { bestml, (S32)bestoff };
			return true;
		}
		else
		{
			return false;
		}
	}
}

struct newlzf_optimal_arrival
{
	S32	cost;
	S32 ml,lrl; // prev pos is at -ml-lrl
	S32 offset; // this is the match offset (not = 0 for LO) ; 
	// check offset == last to tell if its an LO arrival

	union
	{
	U32 is_twostep;
	struct {
		U32 lrl : 8;
		U32 ml : 24;
	} twostep;
	};
};

struct newlzf_codecosts
{
	U32 subliteralmask;

	S32 literal[256 + 256]; // 256 real, 256 all-0 for conditional disables
	S32 packet[256];
	S32 offsetL[256];
	S32 offsetH[256];
};

static RADINLINE S32 cost_literals(const U8 * ptr,int lrl,S32 lo,const newlzf_codecosts & codecosts)
{
	RR_ASSERT( lo >= NEWLZF_MIN_OFFSET );
	S32 ret = 0;
	const U8 * ptr_lo = ptr - lo;
	for(int i=0;i<lrl;i++)
	{
		int sub_literal = (U8)(ptr[i] - (ptr_lo[i] & codecosts.subliteralmask));
		ret += codecosts.literal[sub_literal];
	}
	return ret;
}

// Used to score the literals in two-step matches. These are special in two ways:
// 1. LRL is known to always be in [0,7] because we never do two-step matches where the LRL
//    does not fit into a single packet.
// 2. Said LRL is at the start of a packet, and since packets may only start with at least 16 bytes
//    of data left in the source (per NEWLZF_CHUNK_MATCH_SAFE_ZONE) we can read 8 bytes unconditionally
//    with no problem.
//
// This lets us do the process branchlessly which matters because cost determination for two-step literals
// is among the least predictable branches in the encoder.
static RADINLINE S32 cost_twostep_literals(const U8 * ptr,int lrl,S32 lo,const newlzf_codecosts & codecosts)
{
	RR_ASSERT( lo >= NEWLZF_MIN_OFFSET );
	RR_ASSERT( 0 <= lrl && lrl <= 7 );

#if defined(__RADSSE2__)
	static RAD_ALIGN(const U8, disable_from_lrl[16], 16) =
	{
		0,0,0,0,0,0,0,0,
		1,1,1,1,1,1,1,1,
	};

	__m128i lit_bytes = _mm_loadl_epi64((const __m128i *)ptr);
	__m128i sub_bytes = _mm_loadl_epi64((const __m128i *)(ptr - lo));
	__m128i disable_byte = _mm_loadl_epi64((const __m128i *)&disable_from_lrl[8 - lrl]);
	sub_bytes = _mm_and_si128(sub_bytes, _mm_set1_epi32(codecosts.subliteralmask));

	__m128i codes8 = _mm_sub_epi8(lit_bytes, sub_bytes);
	__m128i codes16 = _mm_unpacklo_epi8(codes8, disable_byte);

	S32 sum = 0;
	sum += codecosts.literal[_mm_extract_epi16(codes16, 0)];
	sum += codecosts.literal[_mm_extract_epi16(codes16, 1)];
	sum += codecosts.literal[_mm_extract_epi16(codes16, 2)];
	sum += codecosts.literal[_mm_extract_epi16(codes16, 3)];
	sum += codecosts.literal[_mm_extract_epi16(codes16, 4)];
	sum += codecosts.literal[_mm_extract_epi16(codes16, 5)];
	sum += codecosts.literal[_mm_extract_epi16(codes16, 6)];

	return sum;
#elif defined(DO_BUILD_NEON64)
	static RAD_ALIGN(const U8, disable_from_lrl[16], 16) =
	{
		0,0,0,0,0,0,0,0,
		1,1,1,1,1,1,1,1,
	};

	uint8x8_t lit_bytes = vld1_u8(ptr);
	uint8x8_t sub_bytes = vld1_u8(ptr - lo);
	uint8x8_t disable_byte = vld1_u8(&disable_from_lrl[8 - lrl]);
	sub_bytes = vand_u8(sub_bytes, vdup_n_u8((U8)codecosts.subliteralmask));

	uint8x8_t codes8 = vsub_u8(lit_bytes, sub_bytes);
	uint8x16_t codes16 = vzip1q_u8(vcombine_u8(codes8, vdup_n_u8(0)), vcombine_u8(disable_byte, vdup_n_u8(0)));

	U64 packed0 = vgetq_lane_u64(vreinterpretq_u64_u8(codes16), 0);
	U64 packed1 = vgetq_lane_u64(vreinterpretq_u64_u8(codes16), 1);

	S32 sum = 0;
	sum += codecosts.literal[(packed0 >>  0) & 0xffff];
	sum += codecosts.literal[(packed0 >> 16) & 0xffff];
	sum += codecosts.literal[(packed0 >> 32) & 0xffff];
	sum += codecosts.literal[(packed0 >> 48) & 0xffff];
	sum += codecosts.literal[(packed1 >>  0) & 0xffff];
	sum += codecosts.literal[(packed1 >> 16) & 0xffff];
	sum += codecosts.literal[(packed1 >> 32) & 0xffff];

	return sum;
#else
	return cost_literals(ptr,lrl,lo,codecosts);
#endif
}

static RADINLINE S32 cost_lo_match(int lrl_for_packet,int ml,const newlzf_codecosts & codecosts)
{
	RR_ASSERT( ml >= 1 && ml <= 15 );
	RR_ASSERT( lrl_for_packet >= 0 && lrl_for_packet <= 7 );
	
	int packet = lrl_for_packet + (ml<<3) + 128;
	
	S32 cost = codecosts.packet[packet];
		
	return cost;
}

// cost_offset not included
static RADINLINE S32 cost_normal_match(int lrl_for_packet,int ml,const newlzf_codecosts & codecosts)
{
	RR_ASSERT( ml >= 1 && ml <= 15 );
	RR_ASSERT( lrl_for_packet >= 0 && lrl_for_packet <= 7 );
	
	int packet = lrl_for_packet + (ml<<3);
	
	S32 cost = codecosts.packet[packet];
		
	return cost;
}

static RADINLINE S32 cost_long_lrl(int lrl,const newlzf_codecosts & codecosts,int * plrl_for_packet)
{
	RR_ASSERT( lrl > 7 );
	
	if ( lrl >= NEWLZF_LRL_EXCESS )
	{
		// 0 escape packet :
		S32 cost = codecosts.packet[0];

		int lrl_excess = lrl - NEWLZF_LRL_EXCESS;
		cost += newlzf_countbytesv(lrl_excess) * COST_ONE_BYTE;
		
		*plrl_for_packet = 0;
		
		cost += COST_PENALTY_EXCESS;
		cost += COST_PENALTY_ESCAPE;
		
		return cost;
	}
	else
	{
		S32 lrl_continue_cost = codecosts.packet[128 + 7];
		S32 cost = 0;
		// @@ could be a divide by 7 instead
		while( lrl > 7 )
		{
			lrl -= 7;
			cost += lrl_continue_cost;
		}
		
		// remainder goes in packet :
		RR_ASSERT( lrl <= 7 );
		*plrl_for_packet = lrl;
		return cost;
	}
}			

static RADINLINE void try_arrival(newlzf_optimal_arrival * arrivals,
	S32 pos,S32 cost,S32 lrl,S32 ml,S32 offset,
	int & parse_chunk_end)
{
	RR_ASSERT( cost > 0 );
	// < : don't replace equal cost existing arrivals
	//	this means that in a multi-step set of packets
	//	the one with the longest last step will get in first
	//	since it's filled soonest
	//	eg. [R14][R11] will be chosen instead of [R15][R10]
	// <= : prefer the shortest final step
	//  it appears that "<=" is slightly better in Mermaid and "<" is slightly better in Selkie
	//	but it's small
	newlzf_optimal_arrival & arrival = arrivals[pos+ml];
	if ( cost < arrival.cost )
	{
		arrival.cost = cost;
		arrival.ml = ml;
		arrival.lrl = lrl;
		arrival.offset = offset;
		arrival.is_twostep = 0;
		parse_chunk_end = RR_MAX(parse_chunk_end, pos+ml);
		//return true;
	}
	//return false;
}
	
static RADINLINE S32 try_lo_arrival(newlzf_optimal_arrival * arrivals,
	S32 pos,S32 base_cost,S32 lrl,S32 lrl_for_packet,S32 ml,S32 offset,const newlzf_codecosts & codecosts,
	int & parse_chunk_end)
{
	// cost it
	S32 cost = base_cost + cost_lo_match(lrl_for_packet,ml,codecosts);

	try_arrival(arrivals,pos,cost,lrl,ml,offset,parse_chunk_end);
	
	return cost;
}
				
static RADINLINE S32 try_match_arrival(newlzf_optimal_arrival * arrivals,
	S32 pos,S32 base_cost,S32 lrl,S32 lrl_for_packet,S32 ml,S32 offset,const newlzf_codecosts & codecosts,
	int & parse_chunk_end)
{
	S32 cost = base_cost + cost_normal_match(lrl_for_packet,ml,codecosts);
	
	cost += COST_PENALTY_NORMAL_MATCH;
	#ifdef COST_PENALTY_NORMAL_L1
	if ( offset > 8192 ) // @@@@ tweak? or don't do this
		cost += COST_PENALTY_NORMAL_L1;
	#endif
	if ( offset > 256 ) // @@@@ tie breaker to low offsets ?
		cost ++;
	
	try_arrival(arrivals,pos,cost,lrl,ml,offset,parse_chunk_end);
	
	return cost;
}
			
			
static void newLZF_encode_chunk_optimal_sub_Mermaid(
	// out :	
	vector_a<newlzf_encoder_parse> & parsevec,
	newlzf_optimal_arrival * arrivals,
	int * p_lastoffset,
	// in : 
	const newlz_vtable * vtable,
	newlz_encoder_scratch * scratch,	
	newLZF_passinfo & passinfo,
	int greedy_chunktype,
	int start_pos, int parse_end_pos,
	const U8 * chunk_ptr, int chunk_base, int chunk_len,
	const newLZF_MatchParseRecord * matches,
	const U8 * ptr_matchend,
	int mml,
	int off24mml,
	const U8 * dictionaryBase
	);
	
static void newLZF_encode_chunk_optimal_sub_Selkie(
	// out :	
	newlzf_optimal_arrival * arrivals,
	int * p_lastoffset,
	int * p_final_arrival_pos,
	// in : 
	const newlz_vtable * vtable,
	int start_pos, int parse_end_pos,
	const U8 * chunk_ptr, int chunk_base, int chunk_len,
	const newLZF_MatchParseRecord * matches,
	const U8 * ptr_matchend,
	int mml,
	int off24mml,
	const U8 * dictionaryBase
	);

	
static SINTa newLZF_greedy_for_optimal(
	//out :
	F32 *pJ,
	int * pchunktype,
	newLZF_passinfo * passinfo,
	U8 * comp,U8 * comp_end,
	// in:
	const newlz_vtable * vtable,
	newlz_encoder_scratch * scratch,
	const U8 * whole_chunk_ptr,int whole_chunk_len, SINTa chunk_pos,
	int whole_start_pos,
	const newLZF_MatchParseRecord * matches,
	int mml,
	int off24mml,
	const U8 * dictionaryBase, // dictionaryBase : match ptr >= this
	U32 dictionarySize // dictionarySize : (optional) match offset limit
	)
{			
	
	S32 lastoffset = NEWLZF_MIN_OFFSET;
	
	// start ctmf :
	//ctmf->set_next(chunk_ptr+start_pos);
	
	bool selkie  = vtable->compressor == OodleLZ_Compressor_Selkie;
	//int off24mml = selkie ? OFF24_MML_SELKIE : OFF24_MML_MERMAID;
	
	S32 offsLimitTab[32];
	newLZF_InitOffsLimitTab(offsLimitTab, mml, off24mml, vtable->pOptions);

	bool do_sub = true;

	newLZF_encoder_arrays encarrays;
	newLZF_encoder_arrays_point_to_scratch(&encarrays,scratch,whole_chunk_len,whole_chunk_ptr,do_sub);
	
	if ( selkie )
	{
		encarrays.literals_space_raw = comp + 3 + whole_start_pos;
		encarrays.literals_ptr_raw = encarrays.literals_space_raw;
	}
	
	// two 64k chunks
	// lastoffset is carried
	for(int twice=0;twice<2;twice++)
	{
	// ptr_matchend is the match *end* limit
	const U8 * ptr_matchend;
	
	if ( ! newLZF_encoder_arrays_start_parse_chunk(&encarrays,twice,whole_start_pos,ptr_matchend) )
		break;
		
	int literals_start = encarrays.parse_chunk_start_pos;
	
	// parse_end_pos is the match *start* pos limit
	// -5 from : -1 for lazy, -4 to make room for a U32 grab before ptr_matchend
	int parse_end_pos = rrPtrDiff32(ptr_matchend - whole_chunk_ptr) - 5;
	
	for(int pos=literals_start;pos<parse_end_pos;)
	{
		const U8 * ptr = whole_chunk_ptr + pos;

		match chosen;
		if ( ! newLZF_get_match_heuristic(&chosen,matches[pos],lastoffset,ptr,ptr_matchend,pos-literals_start,mml,offsLimitTab,dictionaryBase,dictionarySize) )
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
			
			if (
				newLZF_get_match_heuristic(&lazy,matches[pos+1],lastoffset,ptr+1,ptr_matchend,pos+1-literals_start,mml,offsLimitTab,dictionaryBase,dictionarySize)
				&& newLZF_LazyMatchDelta(lazy.ml,lazy.off,lazy.IsLO(),chosen.ml,chosen.off,chosen.IsLO()) > 0 )
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
			
				// IsLazy2Better : check newLZ_LazyMatchDelta > 3 :
				if (
					newLZF_get_match_heuristic(&lazy,matches[pos+2],lastoffset,ptr+2,ptr_matchend,pos+2-literals_start,mml,offsLimitTab,dictionaryBase,dictionarySize)
					&& newLZF_LazyMatchDelta(lazy.ml,lazy.off,lazy.IsLO(),chosen.ml,chosen.off,chosen.IsLO()) > NEWLZF_LAZYBETTER_LAZY2 )
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
		
		RR_ASSERT( ptr == whole_chunk_ptr + pos );
		
		// have a match
		RR_ASSERT( chosen.ml >= mml || chosen.IsLO() );
						
		// verify match :
		S32 offset = chosen.IsLO() ? lastoffset : chosen.off;
		NEWLZF_ASSERT_HEAVY( memcmp(whole_chunk_ptr+pos,whole_chunk_ptr+pos-offset,chosen.ml) == 0 );

		// make sure match backup didn't violate LO0LRL0 exclusion :
		//RR_ASSERT( pos > literals_start || chosen.off != 0 );
		
		int lrl = pos - literals_start;
		
		#if 1
		// avoid the lens that produce a trailing shitlet of len 1
		// -> this is pretty meh for compression and speed
		// ideally we'd decide whether to shift forward or back
		if ( chosen.ml == 16 || chosen.ml == 31 )
		{
			chosen.ml --;
			if ( chosen.off <= 0xFFFF && lrl_remainder(lrl) < 7 )
			{
				// insert a literal :
				ptr++;
				pos++;
				lrl++;			
			}
		}
		#endif
		
		const U8 * litptr = whole_chunk_ptr + literals_start;
		
		// greedy for optimal : t_do_lomml1 is on
		if ( lrl >= 8 && lrl < NEWLZF_LRL_EXCESS )
		{
			do_lomml1_for_lrl(&encarrays,chosen.ml,lrl,chosen.off,lastoffset,litptr);
		}
		else
		{
			newLZF_encoder_arrays_put_packet(&encarrays,chosen.ml,lrl,chosen.off,lastoffset,litptr);
		}
				
		lastoffset = offset;
				
		pos += chosen.ml;
		literals_start = pos;
	}
	
	const U8 * litptr = whole_chunk_ptr + literals_start;
		
	newLZF_encoder_arrays_finish_parse_chunk(&encarrays,twice,litptr,lastoffset);
		
	} // twice

	// greedy-for-optimal
	//	this can be done multiple times for MML finding, and is usually just discarded
	//	so don't bother with the slowest modes
	OodleLZ_CompressionLevel save_level = vtable->level;
	U32 save_entropy_flags = vtable->entropy_flags;
	const_cast<newlz_vtable *>(vtable)->level = OodleLZ_CompressionLevel_Normal;
	const_cast<newlz_vtable *>(vtable)->entropy_flags &= ~NEWLZ_ARRAY_FLAG_ALLOW_SPLIT;
	
	SINTa compLen = newLZF_encoder_arrays_output(pJ,pchunktype,passinfo,comp,comp_end,vtable,scratch,&encarrays,chunk_pos);

	const_cast<newlz_vtable *>(vtable)->level = save_level;
	const_cast<newlz_vtable *>(vtable)->entropy_flags = save_entropy_flags;

	return compLen;
}

static SINTa newLZF_encode_all_sub_chunk(
	//out :
	F32 *pJ,
	int * pchunktype,
	newLZF_passinfo * passinfo,
	U8 * comp,U8 * comp_end,
	// in:
	const newlz_vtable * vtable,
	newlz_encoder_scratch * scratch,
	const U8 * whole_chunk_ptr,int whole_chunk_len, SINTa chunk_pos,
	int whole_start_pos
	)
{	
	S32 lastoffset = NEWLZF_MIN_OFFSET;
	
	// start ctmf :
	//ctmf->set_next(chunk_ptr+start_pos);
	
	bool selkie  = vtable->compressor == OodleLZ_Compressor_Selkie;
	//int off24mml = selkie ? OFF24_MML_SELKIE : OFF24_MML_MERMAID;
	
	if ( selkie )
		return whole_chunk_len+1;
	
	bool do_sub = true;

	newLZF_encoder_arrays encarrays;
	newLZF_encoder_arrays_point_to_scratch(&encarrays,scratch,whole_chunk_len,whole_chunk_ptr,do_sub);
				
	int literals_start = whole_start_pos;
	
	const U8 * literals_start_ptr = whole_chunk_ptr + literals_start;
		
	int final_lrl = whole_chunk_len - literals_start;

	RR_ASSERT( encarrays.literals_ptr_sub != NULL );

	// @@ could do a faster version of put_sub_literals for this obviously special cased to long runs and offset=8
	put_sub_literals(encarrays.literals_ptr_sub,literals_start_ptr,final_lrl,lastoffset);
	
	encarrays.literals_ptr_sub += final_lrl;
	
	SINTa compLen = newLZF_encoder_arrays_output(pJ,pchunktype,passinfo,comp,comp_end,vtable,scratch,&encarrays,chunk_pos);

	return compLen;
}

static void newLZF_append_arrivals_to_parsevec(
	vector_a<newlzf_encoder_parse> & parsevec,
	const newlzf_optimal_arrival * arrivals,
	int start_pos, int final_arrival_pos,
	const U8 * chunk_ptr)	
{
	// reverse the arrivals & output it to parsevec :
	// fill parsevec by tracing back through arrivals :
	
	int parsevec_start = parsevec.size32();
	
	for(int pos=final_arrival_pos;pos>start_pos;)
	{
		const newlzf_optimal_arrival & arrival = arrivals[pos];
				
		if ( arrival.is_twostep )
		{
			int twostep_prev_pos = pos - arrival.twostep.ml - arrival.twostep.lrl;
			RR_ASSERT( twostep_prev_pos < pos );
			RR_ASSERT( twostep_prev_pos >= start_pos );
	
			// first the twostep LO match :		
			parsevec.push_back();
			newlzf_encoder_parse & parse = parsevec.back();
			//parse.lastoffset = arrival.offset;
			parse.offset = 0; //arrival.offset; // LO0
			parse.ml = arrival.twostep.ml;
			parse.lrl = arrival.twostep.lrl;
			RR_ASSERT( parse.IsLO() );
		
			// verify the match :
			RR_DURING_ASSERT( const U8 * twostep_ptr = chunk_ptr + pos - arrival.twostep.ml );
			RR_ASSERT( memcmp(twostep_ptr,twostep_ptr - arrival.offset,arrival.twostep.ml) == 0 );
		
			pos = twostep_prev_pos;
		}
		
		int prev_pos = pos - arrival.ml - arrival.lrl;
		RR_ASSERT( prev_pos < pos );
		RR_ASSERT( prev_pos >= start_pos );
		const newlzf_optimal_arrival & prev_arrival = arrivals[prev_pos];
		RR_ASSERT( prev_arrival.cost != RR_S32_MAX );
		RR_ASSERT( prev_arrival.offset >= NEWLZF_MIN_OFFSET );
		
		RR_ASSERT( arrival.ml > 0 || arrival.lrl > 0 );
		RR_ASSERT( arrival.ml >= 3 || arrival.offset == prev_arrival.offset );
				
		parsevec.push_back();
		newlzf_encoder_parse & parse = parsevec.back();

		// I need the LO to apply to my literals; that's the *previous* offset
		//parse.lastoffset = prev_arrival.offset;
		parse.lrl = arrival.lrl;
		parse.ml = arrival.ml;
		parse.offset = ( arrival.offset == prev_arrival.offset ) ? 0 : arrival.offset;
		pos = prev_pos;
	}
	
	int parsevec_end = parsevec.size32();
	
	if ( parsevec_end > parsevec_start )
	{
		// we put to parsevec backwards, now reverse :
		reverse(parsevec.data() + parsevec_start,parsevec.data() + parsevec_end);
	}
}
		
/*************

newLZ_encode_chunk_optimal -
a lot of code dupe from newLZ_encode_chunk

first do a greedy parse just like the Normal heuristic parse
using the PMP matches

*************/
SINTa newLZF_encode_chunk_optimal(const newlz_vtable * vtable,
		newlz_encoder_scratch * scratch,
		const U8 * dictionaryBase,
		const U8 * chunk_ptr,int whole_chunk_len,
		U8 * comp,U8 * comp_end,
		SINTa chunk_pos,
		int * pchunktype,
		F32 * pJ,
		const OodleKrakenChunkDeadlines *)
{
	//rrprintf("newLZ_encode_chunk : %d\n",chunk_len);
	SIMPLEPROFILE_SCOPE_N(encode_optimal,whole_chunk_len);
	THREADPROFILESCOPE("newlzf_chunk_optimal");
	
	*pchunktype = 0;
	*pJ = LAGRANGE_COST_INVALID;
	
	if ( whole_chunk_len <= NEWLZF_MIN_CHUNK_LEN )
	{
		return whole_chunk_len;
	}

	rrArenaAllocator * arena = scratch->arena;

	RR_ASSERT( vtable->find_all_matches_num_pairs == NEWLZF_MATCH_NUM_PAIRS );
	newLZF_MatchParseRecord * matches = (newLZF_MatchParseRecord *) scratch->match_pairs;

	vector_a<newlzf_encoder_parse> parsevec;	
	scratch->parsevec_space.extend(sizeof(newlzf_encoder_parse)*whole_chunk_len/2,arena);
	parsevec.provide_arena(scratch->parsevec_space.m_ptr,scratch->parsevec_space.m_size);	

	bool selkie = (vtable->compressor == OodleLZ_Compressor_Selkie);
	int off24mml = selkie ? OFF24_MML_SELKIE : OFF24_MML_MERMAID;

	// start at pos 1 :
	int start_pos = 0;
	
	if ( chunk_pos == 0 )
	{
		// start literals at NEWLZ_MIN_OFFSET so sub doesn't have to check
		start_pos = NEWLZF_MIN_OFFSET;
	}
		
	//------------------
		
	scratch->arrivals_space.extend(sizeof(newlzf_optimal_arrival)*(whole_chunk_len+1),arena);
	newlzf_optimal_arrival * arrivals = (newlzf_optimal_arrival *) scratch->arrivals_space.m_ptr;
	
	// output into the arrivals buffer :
	U8 * optimal_comp = (U8 *)arrivals;
	U8 * optimal_comp_end = optimal_comp + scratch->arrivals_space.m_size;
	
	//------------------
	
	const OodleLZ_CompressOptions * pOptions = vtable->pOptions;
	
	U32 dictionarySize = ( pOptions->dictionarySize > 0 ) ?
		RR_MIN(NEWLZF_MAX_OFFSET,pOptions->dictionarySize)
		: NEWLZF_MAX_OFFSET;
		
	newLZF_passinfo greedy_passinfo;
	RR_ZERO(greedy_passinfo);
	F32 greedy_J = LAGRANGE_COST_INVALID;
	int greedy_chunktype = 0;
	int greedy_mml = RR_MAX(pOptions->minMatchLen,NEWLZF_MML_NORMAL);
	
	SINTa greedy_complen = newLZF_greedy_for_optimal(
		&greedy_J,&greedy_chunktype,
		&greedy_passinfo,
		comp,comp_end,

		vtable,scratch,
		chunk_ptr,whole_chunk_len,chunk_pos,
		start_pos,
		matches,
		greedy_mml,
		off24mml,
		dictionaryBase,
		dictionarySize
		);
			
	// expanded :
	if ( greedy_complen >= whole_chunk_len )
	{
		return whole_chunk_len;
	}

	*pchunktype = greedy_chunktype;
	
	// If we have a whole-chunk match, there's no point trying to do anything else, just
	// using the basic greedy parse which uses that match is definitely optimal.
	if ( chunk_pos != 0 && // Can't have whole-chunk match at a reset point
		 matches[0].pairs[0].length >= whole_chunk_len - NEWLZF_CHUNK_MATCH_SAFE_ZONE )
	{
		*pJ = greedy_J;
		rrPrintf_v2("Full-chunk match fast path greedy_complen=%d\n", (int)greedy_complen);
		return greedy_complen;
	}

	//-----------------------------------------------------------------------------
	
	// try high MML
	//	the idea is that this provides a space-speed point between the normal parse
	//	 and just abandoning the chunk
	// -> pretty meh at the moment
	// -> the benefit of this is not what I expected (I expected faster to decode, less compression)
	// -> instead it's finding better parses on chunks that like lots of literals (in Mermaid)
	//	(stuff like "nci")
	// -> a nasty one to watch are the Fez chunks (like fez_chunk_655360) that want to be pure sub literals
	#if 1
	if ( ! selkie )
	if ( vtable->level >= OodleLZ_CompressionLevel_Optimal2 )
	if ( pOptions->minMatchLen < 8 )
	{
		newLZF_passinfo greedy2_codelens;
		RR_ZERO(greedy2_codelens);
		F32 greedy2_J = LAGRANGE_COST_INVALID;
		int greedy2_chunktype = 0;
		
		// DISABLED
		#if 0
		int greedy2_mml = 8; // 8 is better
		
		//*
		int greedy2_off24mml = off24mml + off24mml/2; // @@ higher off24mml ??
		
		// hammer dictionarysize :
		// idea is to make this more different
		// less matches, more literals
		// -> doing this seems to be very slightly better than not doing it
		U32 greedy2_dictionarySize = dictionarySize;
		if ( vtable->compressor != OodleLZ_Compressor_Selkie )
			greedy2_dictionarySize = RR_MIN(dictionarySize,64*1024);
		/*/
		
		// don't mess around :
		int greedy2_off24mml = off24mml;
		U32 greedy2_dictionarySize = dictionarySize;
	
		/**/
		
		SINTa greedy2_complen = newLZF_greedy_for_optimal(
			&greedy2_J,&greedy2_chunktype,
			&greedy2_codelens,
			optimal_comp,optimal_comp_end,

			vtable,scratch,
			chunk_ptr,whole_chunk_len,chunk_pos,
			start_pos,
			matches,
			greedy2_mml,
			greedy2_off24mml,
			dictionaryBase,
			greedy2_dictionarySize
			);
				
		// expanded :
		if ( greedy2_complen < whole_chunk_len &&
			greedy2_J < greedy_J )
		{
			greedy_J = greedy2_J;
			*pchunktype = greedy2_chunktype;
			memcpy(comp,optimal_comp,greedy2_complen);			
			
			greedy_complen= greedy2_complen;
			
			// greedy_mml will be used by the following optimal parse
			// set MML for the optimal parse? or just leave it?
			// -> doing this or not is all quite meh
			//greedy_mml = greedy2_mml;
			//dictionarySize = greedy2_dictionarySize;
			//rrprintf("greedy_mml = %d, greedy_complen = %d\n",greedy_mml,greedy_complen);
			
			// change code costs used for optimal parse guiding ?
			// greedy_chunktype needs to go with passinfo :
			// -> currently not doing this, was it good or bad?
			//	-> seems to hurt a little
			//greedy_passinfo = greedy2_codelens;
			//greedy_chunktype = greedy2_chunktype;
			
			// note :
			// *pchunktype contains the type that goes with the comp data
			// greedy_chunktype is the type that goes with "passinfo"
			// they might differ !!
		}
		#endif	
		
		#if 1
		if ( ! selkie )
		{
			// @@@@!! try a pure-sub-literal chunk
			// write into the greedy2 space again,
			//	if we wanted it we memcpy'd it out already
			
			SINTa greedy2_complen = newLZF_encode_all_sub_chunk(
				&greedy2_J,&greedy2_chunktype,
				&greedy2_codelens,
				optimal_comp,optimal_comp_end,

				vtable,scratch,
				chunk_ptr,whole_chunk_len,chunk_pos,
				start_pos
				);
					
			// expanded :
			if ( greedy2_complen < whole_chunk_len &&
				greedy2_J < greedy_J )
			{
				// happens a lot on Fez
				//	a few other places
				//	horse.vipm , d.dds
				//rrprintf("TOOK ALL SUB CHUNK : %d < %d\n",greedy2_complen,greedy_complen);
			
				greedy_J = greedy2_J;
				*pchunktype = greedy2_chunktype;
				memcpy(comp,optimal_comp,greedy2_complen);			
				
				greedy_complen= greedy2_complen;
			}
		}
		#endif
	}
	#endif	
		
	//-----------------------------------------------------------------------------
	// optimal parse!

	// wipe out arrival costs :	
	for(int i=0;i<=whole_chunk_len;i++)
	{
		arrivals[i].cost = RR_S32_MAX;
	}
	
	// wipe out parse :
	parsevec.clear();
	
	// two 64k chunks :
	
	int parse_chunk_len1 = RR_MIN(whole_chunk_len,(1<<16));
	int parse_chunk_len2 = whole_chunk_len - parse_chunk_len1;
	
	SINTa parsevec_size1 = 0;
		
	S32 lastoffset = NEWLZF_MIN_OFFSET;
	
	// two 64k chunks in each 128k chunk
	// lastoffset is carried across the 64k chunks
	//	there's a literal tail per 64k chunk
	for(int twice=0;twice<2;twice++)
	{
		int parse_chunk_len;
		int parse_chunk_start_pos;

		if ( twice==0 )
		{
			parse_chunk_len = parse_chunk_len1;
			parse_chunk_start_pos = 0;
		}
		else
		{
			parsevec_size1 = parsevec.size();

			if ( parse_chunk_len2 == 0 ) break;

			start_pos = parse_chunk_len1;
			parse_chunk_len = parse_chunk_len2;
			parse_chunk_start_pos = parse_chunk_len1;
		}

		const U8 * parse_chunk_ptr = chunk_ptr + parse_chunk_start_pos;

		// ptr_matchend is the match *end* limit
		const U8 * ptr_matchend = chunk_ptr + whole_chunk_len - NEWLZF_CHUNK_MATCH_SAFE_ZONE;
		ptr_matchend = RR_MIN(ptr_matchend,parse_chunk_ptr + parse_chunk_len);

		// parse_end_pos is the match *start* pos limit
		// -5 from : -1 for lazy, -4 to make room for a U32 grab before ptr_matchend
		int parse_end_pos = rrPtrDiff32(ptr_matchend - chunk_ptr) - 5;

		int parse_chunk_end_pos = parse_chunk_start_pos + parse_chunk_len;

		// initialize first arrival : (this seeds lastoffset for second parse chunk)
		arrivals[start_pos].cost = 0;
		arrivals[start_pos].ml = 0;
		arrivals[start_pos].lrl = 0;
		arrivals[start_pos].offset = lastoffset;
		arrivals[start_pos].is_twostep = 0;

		// optimal parser fills the arrivals[] :

		if ( ! selkie )
		{
			// newLZF_encode_chunk_optimal_sub_Mermaid also works for Selkie

			newLZF_encode_chunk_optimal_sub_Mermaid(
				// out :
				parsevec,
				arrivals,
				&lastoffset,
				// in :
				vtable,
				scratch,
				greedy_passinfo,
				greedy_chunktype,
				start_pos,parse_end_pos,
				chunk_ptr,parse_chunk_start_pos,parse_chunk_end_pos,
				matches,ptr_matchend,
				greedy_mml,
				off24mml,
				dictionaryBase
			);
		}
		else
		{
			RR_ASSERT( greedy_chunktype == NEWLZ_LITERALS_TYPE_RAW );

			int final_arrival_pos = start_pos;

			newLZF_encode_chunk_optimal_sub_Selkie(
				// out :
				arrivals,
				&lastoffset,
				&final_arrival_pos,
				// in :
				vtable,
				start_pos,parse_end_pos,
				chunk_ptr,parse_chunk_start_pos,parse_chunk_end_pos,
				matches,ptr_matchend,
				greedy_mml,
				off24mml,
				dictionaryBase
			);

			newLZF_append_arrivals_to_parsevec(
				parsevec,
				arrivals,
				start_pos,final_arrival_pos,
				chunk_ptr);
		}
	} // end of "twice" loop, two 64k chunks in 128k chunk

	newLZF_passinfo * p_optimal_passinfo = NULL;
	if (  ! selkie )
	{
		// overwrite greedy_passinfo
		p_optimal_passinfo = &greedy_passinfo;
		RR_ZERO(*p_optimal_passinfo);
	}

	int optimal_chunktype = 0;
	SINTa optimal_complen = whole_chunk_len + 1;
	F32 optimal_J = LAGRANGE_COST_INVALID;
	
	optimal_complen = newLZF_put_parse(&optimal_J,&optimal_chunktype,p_optimal_passinfo,
		optimal_comp,optimal_comp_end,

		vtable,scratch,
		chunk_ptr,whole_chunk_len,chunk_pos,
		parsevec,
		parsevec_size1);
	
	// optimal_complen can be worse than greedy sometimes ; revert?
	// select space-speed :
	if ( optimal_J < greedy_J )
	{
		if ( p_optimal_passinfo )
		{
			// carried optimal encoder state :
			newlz_scratchblock & carried_encoder_state = const_cast<newlz_scratchblock &>(vtable->carried_encoder_state);
			
			carried_encoder_state.extend( sizeof(newLZF_passinfo) , arena);
			memcpy(	carried_encoder_state.get(),p_optimal_passinfo,sizeof(newLZF_passinfo));
		
			const_cast<newlz_vtable *>(vtable)->carried_encoder_state_chunktype = optimal_chunktype;
		}
	
		memcpy(comp,optimal_comp,optimal_complen);
		*pchunktype = optimal_chunktype;
		*pJ = optimal_J;
		return optimal_complen;
	}
	else
	{
		rrPrintf_v2("Greedy reverted : %d -> %d : %d == %.3f%%\n",optimal_complen,greedy_complen,(optimal_complen - greedy_complen),(optimal_complen - greedy_complen)*100.0/whole_chunk_len);
	
		//*pchunktype already filled
		*pJ = greedy_J;
			
		return greedy_complen;
	}
}

static RADFORCEINLINE bool find_twostep_match_generic(const U8 * ptr, SINTa offset, int pos, int parse_end_pos, const U8 * ptr_matchend, S32 * plrl, S32 * pml)
{
	// fill LO search forward :
	U64 me = RR_GET64_LE(ptr);
	U64 vs = RR_GET64_LE(ptr - offset);
	// any bytes match ?
	U64 x = me ^ vs;

#define RR_U64_HAS_ZERO_BYTE(V)       (((V) - 0x0101010101010101ULL) & ~(V) & 0x8080808080808080ULL)

	x = RR_U64_HAS_ZERO_BYTE(x);
	if ( ! x )
		return false;

	S32 lrl = rrCtzBytes64(x);
	RR_ASSERT( ptr[lrl] == ptr[(SINTa)lrl-offset] );
	RR_ASSERT( lrl == 0 || ptr[lrl-1] != ptr[lrl-offset-1] );
	// using a U64 means lrl must be <= 7 which is exactly what I want :
	RR_ASSERT( lrl <= 7 );

	if ( pos + lrl >= parse_end_pos )
		return false;

	const U8 * ptr_lrl = ptr + lrl;
	*plrl = lrl;
	*pml = getmatchlen_mml1(ptr_lrl,ptr_lrl - offset,ptr_matchend);
	return true;
}

#ifdef __RADSSE2__

static RADFORCEINLINE bool find_twostep_match_sse2(const U8 * ptr, SINTa offset, int pos, int parse_end_pos, const U8 * ptr_matchend, S32 * plrl, S32 * pml)
{
	// NOTE: safe to read 16 bytes here since ptr + ml is the end of the
	// previous packet, and packets always end at least 16 bytes before the
	// end of the input buffer (enforced via ptr_matchend).
	__m128i cur_bytes = _mm_loadu_si128((const __m128i*) ptr);
	__m128i lo_bytes = _mm_loadu_si128((const __m128i*) (ptr - offset));
	__m128i equal_bytes = _mm_cmpeq_epi8(cur_bytes, lo_bytes);
	U32 equal_mask = _mm_movemask_epi8(equal_bytes);

	// If there are 8 or more non-matching bytes, that makes our initial LRL >= 8, which we don't allow.
	if ( ( equal_mask & 0xff ) == 0 )
		return false;

	// equal_mask != 0 by the above
	SINTa twostep_lrl = rrCtz32(equal_mask);
	if ( pos + twostep_lrl >= parse_end_pos )
		return false;

	// Isolate mask for lowest set bit (first byte of match) to find start of match
	const U32 initial_match_mask = rrLowestSetBitMask32(equal_mask);

	// Add to match mask, which carries through a contiguous run of set match bits
	// and makes a new lowest set bit at the position of the first non-matching byte
	const U32 match_end_mask = equal_mask + initial_match_mask;
	SINTa twostep_ml;

	// If we didn't carry past 16 bits, the match ends within the 16 bytes we loaded;
	// work out the actual match end.
	if ( match_end_mask <= 0xffff )
	{
		// Match end is within the 16 bytes we loaded
		SINTa match_end = rrCtz32(match_end_mask);

		// Need to clamp so we don't go past ptr_matchend
		match_end = RR_MIN(match_end, ptr_matchend - ptr);

		twostep_ml = match_end - twostep_lrl;
	}
	else
	{
		// Still need to be careful not to go past ptr_matchend. Just re-match the first
		// few bytes after LRL again, they're guaranteed to be before ptr_matchend.
		twostep_ml = getmatchlen_mml1(ptr + twostep_lrl, ptr + twostep_lrl - offset, ptr_matchend);
	}

	*plrl = (S32)twostep_lrl;
	*pml = (S32)twostep_ml;
	return true;
}

#endif

// Find a two-step match candidate with an initial LRL not exceeeding 7.
// May read up to 16 bytes starting from ptr, caller must ensure that is legal.
//
// Returns true on success.
//
// If true, plrl and pml are filled with a description of the two-step match.
//
// On success:
//   *plrl will be in [1,7]
//   *pml > 0
static RADFORCEINLINE bool find_twostep_match(const U8 * ptr, SINTa offset, int pos, int parse_end_pos, const U8 * ptr_matchend, S32 * plrl, S32 * pml)
{
#if defined(__RADSSE2__)
	return find_twostep_match_sse2(ptr, offset, pos, parse_end_pos, ptr_matchend, plrl, pml);
#else
	// NOTE: NEON64 tested (in save/) but tied with generic so just stick with that.
	return find_twostep_match_generic(ptr, offset, pos, parse_end_pos, ptr_matchend, plrl, pml);
#endif
}

static RADFORCEINLINE void mermaid_try_twostep(
	newlzf_optimal_arrival * arrivals,
	int pos, int parse_end_pos,
	int ml,int lrl,int offset,S32 cost,
	const U8 * ptr, const U8 * ptr_matchend,
	const newlzf_codecosts & codecosts,
	int & parse_chunk_end)
{
	// pos/ptr are for the original match, but we care about what happens after
	pos += ml;
	ptr += ml;

	S32 twostep_lrl, twostep_ml;
	if ( ! find_twostep_match(ptr, offset, pos, parse_end_pos, ptr_matchend, &twostep_lrl, &twostep_ml) )
		return;

	RR_ASSERT( twostep_lrl > 0 && twostep_lrl <= 7 );
	RR_ASSERT( twostep_ml > 0 );

/*
	
@@@@!! > 0 or > 1 ?
this appears marginal/close

//if ( twostep_ml > 1 )
	
lzt99 : 24,700,820 ->10,515,795 =  3.406 bpb =  2.349 to 1
decode only      : 26.632 millis, 1.86 c/b, rate= 927.50 mb/s
wad7 : 10,000,000 -> 3,169,656 =  2.536 bpb =  3.155 to 1
decode only      : 10.749 millis, 1.86 c/b, rate= 930.35 mb/s
normals.bc1 :    524,316 ->   266,515 =  4.066 bpb =  1.967 to 1
decode only      : 0.890 millis, 2.94 c/b, rate= 589.06 mb/s

if ( twostep_ml > 0 )
	
lzt99 : 24,700,820 ->10,504,538 =  3.402 bpb =  2.351 to 1
decode only      : 26.715 millis, 1.87 c/b, rate= 924.62 mb/s
wad7 : 10,000,000 -> 3,168,410 =  2.535 bpb =  3.156 to 1
decode only      : 10.814 millis, 1.87 c/b, rate= 924.75 mb/s
normals.bc1 :    524,316 ->   266,256 =  4.063 bpb =  1.969 to 1
decode only      : 0.895 millis, 2.95 c/b, rate= 585.55 mb/s

- allowing lo mml1 in twostep
seems to be a small compression gain
maybe also a small speed penalty?

gain 0.004 bpb
lost 0.01 c/b

0.01 c/b = 0.0036 bpb
very close to break even J

*/

	//if ( twostep_ml <= 1 ) return;

	// @@ just clamp to 15 for now ? :
	twostep_ml = RR_MIN(twostep_ml,15);
	
	cost += cost_twostep_literals(ptr,twostep_lrl,offset,codecosts);
	cost += cost_lo_match(twostep_lrl,twostep_ml,codecosts);
	
	int twostep_arrival_pos = pos+twostep_lrl+twostep_ml;
	newlzf_optimal_arrival & arrival = arrivals[twostep_arrival_pos];
	
	if ( cost < arrival.cost )
	{
		arrival.cost = cost;
		arrival.ml = ml;
		arrival.lrl = lrl;
		arrival.offset = offset;
		arrival.twostep.lrl = twostep_lrl;
		arrival.twostep.ml = twostep_ml;
		parse_chunk_end = RR_MAX(parse_chunk_end,twostep_arrival_pos);
	}
}

static void newLZF_passinfo_to_codecost(
	const newLZF_passinfo & passinfo,
	newlzf_codecosts & codecosts)
{
	const int cost_entropy_threshold = (75 * COST_ONE_BIT)/10; // @@ <- tweaky
	// <- this really should be based on lambda

	histo_to_codecost(passinfo.literal_histo,codecosts.literal,256,COST_PENALTY_HUFF,cost_entropy_threshold);
	histo_to_codecost(passinfo.packet_histo,codecosts.packet,256,COST_PENALTY_PACKET,cost_entropy_threshold);
	
	/*********
	
	some files in Silesia really don't like offset codecosting (eg. "reymont")
	some files in GTS really *do* like offset codecosting

	so I now use entropy to try to tell the difference :
	
	reymont :  6,627,202 -> 1,554,582 =  1.877 bpb =  4.263 to 1 
	reymont :  6,627,202 -> 1,603,168 =  1.935 bpb =  4.134 to 1 

	-> interestingly, reymont greedy prepass uses the mml 8 path a lot
		mml 8 path also sets max offset to 64k
		it's like a totally different parse
		-> but I'm not using that for optimal parse guiding currently
		-> and it doesn't seem to help to do so
	
	the problem on reymont seems to be that if you use the histos,
	codecost_offsetH[0] is around 140 - around half a byte
	(COST_ONE_BYTE = 256)
	but in the actually put_parse it will send those uncompressed
	so the parse is not matching reality, it's favoring low offsets too much
	
	***********/
	
	histo_to_codecost(passinfo.offsetL_histo,codecosts.offsetL,256,0,cost_entropy_threshold);
	histo_to_codecost(passinfo.offsetH_histo,codecosts.offsetH,256,0,cost_entropy_threshold);

	#if 0 // @@@@!!??
		
	F32 HL = rrEntropyOfHistogram(passinfo.offsetL_histo,256);
	//rrprintfvar(HL);
	if ( HL > 7.5 )
	{
		for LOOP(i,256) codecosts.offsetL[i] = COST_ONE_BYTE;
	}
	
	F32 HH = rrEntropyOfHistogram(passinfo.offsetH_histo,256);
	//rrprintfvar(HH);
	if ( HH > 7.5 )
	{
		for LOOP(i,256) codecosts.offsetH[i] = COST_ONE_BYTE;
	}
	
	#endif
}

static void newLZF_update_passinfo(
		int chunktype,
		const vector_a<newlzf_encoder_parse> & parsevec, int parsevec_start,
		int parse_start_pos,
		newLZF_passinfo & passinfo,
		const U8 * chunk_ptr,
		SINTa lastoffset)
{
	// passinfo contains running histos

	// @@!! tweaky
	//enum { optimal_update_codelens_histo_inc = 16 };
	//enum { optimal_update_codelens_histo_inc = 4 };
	enum { optimal_update_codelens_histo_inc = 2 };
	
	for(int parsevec_i = parsevec_start;parsevec_i< parsevec.size32();parsevec_i++)
	{
		const newlzf_encoder_parse & parse = parsevec[parsevec_i];
		
		// big copy from add_to_histo
		
		const U8 * litptr = chunk_ptr + parse_start_pos;
		
		int lrl = parse.lrl;
		int ml = parse.ml;
		
		parse_start_pos += lrl + ml; // update for next now
	
		if ( chunktype == NEWLZ_LITERALS_TYPE_RAW )
		{	
			for(SINTa i=0;i<lrl;i++)
			{
				U8 lit = litptr[i];
				passinfo.literal_histo[lit] += optimal_update_codelens_histo_inc;
			}
		}
		else
		{			
			for(SINTa i=0;i<lrl;i++)
			{
				U8 lit = litptr[i];
				U8 ref = litptr[i - lastoffset];
				U8 sub = (U8)(lit - ref);
				passinfo.literal_histo[sub] += optimal_update_codelens_histo_inc;
			}
		}
		
		RR_DURING_ASSERT( const U8 * matchptr = litptr + lrl; );
		RR_DURING_ASSERT( const U8 * vsptr = matchptr - parse.GetMatchOffset(lastoffset) );
		RR_ASSERT( memcmp(matchptr,vsptr,ml) == 0 );
					
		if ( lrl >= NEWLZF_LRL_EXCESS )
		{
			// send escape 0 packet :
			passinfo.packet_histo[0] += optimal_update_codelens_histo_inc;

			// will send what I can of lrl in the next normal packet :
			// @@ this isn't quite right ; long LRL's may leave lrl_for_packet
			lrl = 0;
			// it's an escape-only parse :
			if ( ml == 0 )
				continue;
			
			// I currently never make long-LRL + match parse entries from the optimal parse
			// long LRL's always come from cheap_preceding and don't include a match
			// matches always come from the 0-7 LRL search
			// -> tried an experiment 12-19 to change this, no benefit
		}
		
		// update lastoffset
		SINTa offset = parse.GetMatchOffset(lastoffset);
		lastoffset = offset;
		
		//int ml = parse.ml;
		
		while( lrl > NEWLZF_PACKET_LRL_MAX )
		{
			int packet = NEWLZF_PACKET_LRL_MAX;
			lrl -= NEWLZF_PACKET_LRL_MAX;
			
			// LO
			packet += (NEWLZF_PACKET_LO_FLAG);
			
			passinfo.packet_histo[packet] += optimal_update_codelens_histo_inc;
		}

		// send match as escape
		bool isescapematch = parse.IsEscape();
		bool islo = parse.IsLO();
				
		if ( ! islo && parse.offset < (1<<16) )
		{
			// histo offset bytes :
				
			passinfo.offsetL_histo[(U8)parse.offset] += optimal_update_codelens_histo_inc;
			passinfo.offsetH_histo[U8_check(parse.offset>>8)] += optimal_update_codelens_histo_inc;
		}
		
		if ( isescapematch )
		{
			// if I have any LRL, first send an LO packet with it
			if ( lrl > 0 )
			{
				int packet = lrl;
				
				// LO
				packet += (NEWLZF_PACKET_LO_FLAG);
				
				passinfo.packet_histo[packet] += optimal_update_codelens_histo_inc;
			}
			
			// now send the special packet :
					
			// @@ escape_match_transmission
			
			if ( offset <= 0xFFFF )
			{
				RR_ASSERT( ml >= NEWLZF_ML_EXCESS );
				
				int packet = 1;
				passinfo.packet_histo[packet] += optimal_update_codelens_histo_inc;
			}
			else
			{
				RR_ASSERT( ml >= NEWLZF_OFF24_MML_DECODE );
				int packet = ml - NEWLZF_OFF24_MML_DECODE + 3;
				if ( packet > 23 ) packet = 2;
				
				passinfo.packet_histo[packet] += optimal_update_codelens_histo_inc;
			}
		
		// put further ml_escape later
		}
		else
		{
			if ( ml == 0 && lrl == 0 )
			{
				// don't send packet at all
				RR_ASSERT( false );
			}
			else
			{			
				int ml_for_packet = (ml > NEWLZF_PACKET_ML_MAX) ? NEWLZF_PACKET_ML_MAX : ml;
				int packet = (ml_for_packet * NEWLZF_PACKET_LRL_COUNT) + lrl;
				
				if ( islo )
				{
					// LO
					packet += (NEWLZF_PACKET_LO_FLAG);
				}
				else
				{
					RR_ASSERT( ml >= 3 );
				}

				RR_ASSERT( packet >= 24 );
				
				passinfo.packet_histo[packet] += optimal_update_codelens_histo_inc;
					
				ml -= ml_for_packet;
			}
			
			// more ML :
			while( ml > 0 )
			{
				// more ML :
				int ml_for_packet = (ml > NEWLZF_PACKET_ML_MAX) ? NEWLZF_PACKET_ML_MAX : ml;
				ml -= ml_for_packet;
				int packet = (ml_for_packet * NEWLZF_PACKET_LRL_COUNT);
				// LO
				packet += (NEWLZF_PACKET_LO_FLAG);
				passinfo.packet_histo[packet] += optimal_update_codelens_histo_inc;
			}
		}
	}
}

static void optimal_rescale_histo(
		U32	histo[256],
		int add)
{
	RR_UNUSED_VARIABLE(add);

	// @@ tweaky
	for LOOP(i,256)
	{
		histo[i] = add + (histo[i]>>4);
	}
}

static void optimal_rescale_histo2(
		U32	histo1[256],
		const U32	histo2[256],
		int add)
{
	// @@ tweaky
	for LOOP(i,256)
	{
		histo1[i] = add + ((histo1[i] + histo2[i])>>5);
	}
}

static void optimal_rescale_passinfo(
		newLZF_passinfo & passinfo) // <- read/write argument
{
	// @@ tweaky - these add values are not at all well justified
	optimal_rescale_histo(passinfo.literal_histo,1);
	optimal_rescale_histo(passinfo.packet_histo,1);
	optimal_rescale_histo(passinfo.offsetL_histo,2);
	optimal_rescale_histo(passinfo.offsetH_histo,2);
}

static void optimal_rescale_passinfo2(
		newLZF_passinfo & passinfo1, // <- read/write argument
		const newLZF_passinfo & passinfo2)
{
	// @@ tweaky - these add values are not at all well justified
	optimal_rescale_histo2(passinfo1.literal_histo,passinfo2.literal_histo,1);
	optimal_rescale_histo2(passinfo1.packet_histo,passinfo2.packet_histo,1);
	optimal_rescale_histo2(passinfo1.offsetL_histo,passinfo2.offsetL_histo,2);
	optimal_rescale_histo2(passinfo1.offsetH_histo,passinfo2.offsetH_histo,2);
}

static S32 newlzf_match_escape_costs(S32 offset, S32 pos, S32 chunk_base, int escape_ml)
{
	S32 cost;

	// cost offset :
	// store_offset is from base of parse chunk
	RR_ASSERT( pos >= chunk_base );
	SINTa store_offset = offset - (pos - chunk_base);
	RR_ASSERT( store_offset >= 0 );

	if ( store_offset >= NEWLZF_OFFSET_FOURBYTE_THRESHOLD )
		cost = COST_ONE_BYTE * 4; // @@ + more penalty ?
	else
		cost = COST_ONE_BYTE * 3;
		
	// extra penalties for distant matches
	cost += COST_PENALTY_OFF24;
	if ( offset > (1<<20) )
	{
		cost += COST_PENALTY_OFF24_1M;
		if ( offset > (1<<22) )
		{
			cost += COST_PENALTY_OFF24_4M;
		}
	}

	// escape match length penalties
	if ( escape_ml >= 0 )
	{
		cost += COST_ONE_BYTE * newlzf_countbytesv(escape_ml);
		cost += COST_PENALTY_EXCESS;
	}

	return cost;
}

// NOTE :
//	newlzf does not have a separate parameter for "skip len" (aka "fast bytes" aka "good enough len"
// it uses ML_EXCESS for the long escape len as the threshold
//	does goto continue_parse_loop
// currently NEWLZF_ML_EXCESS	= 91
// compression on "nci" could be improved by bumping that up to 256 or 512 at high levels
//  but fuggit

static void newLZF_encode_chunk_optimal_sub_Mermaid(
	// out :	
	vector_a<newlzf_encoder_parse> & parsevec,
	newlzf_optimal_arrival * arrivals,
	int * p_lastoffset,
	// in : 
	const newlz_vtable * vtable,
	newlz_encoder_scratch * scratch,
	newLZF_passinfo & passinfo,
	int greedy_chunktype,
	int optimal_parse_start_pos, int parse_end_pos,
	const U8 * chunk_ptr, int chunk_base, int chunk_len,
	const newLZF_MatchParseRecord * matches,
	const U8 * ptr_matchend,
	int mml,
	int off24mml,
	const U8 * dictionaryBase
	)
{
	// Selkie has its own function now that skips the codecosting :
	RR_ASSERT( vtable->compressor != OodleLZ_Compressor_Selkie );

	// desired update interval :
	//  change 12-19-2017 :
	//	this is now hit; we do try chunk adaptation at this interval
	int parse_chunk_len = 1024;
	
	// forced updates
	int parse_chunk_len_max = 4096;

	newlzf_codecosts codecosts;
	codecosts.subliteralmask = ( greedy_chunktype == NEWLZ_LITERALS_TYPE_RAW ) ? 0 : ~0u;

	// clear high literal costs (=slots for disabled literals)
	for (int i = 0; i < 256; i++)
		codecosts.literal[256 + i] = 0;
	
	// optimal_parse_start_pos is 0,8, or 64k
	if ( optimal_parse_start_pos < (1<<16) ) // only on the first 64k chunk
	{
		// passinfo currently contains the greedy pass
		// passinfo is modified here and will be carried to the next 64k chunk
			
		// only carry if chunktype is the same it was
		//	 even carrying both types of literals, it's important to only do this if it matches
		if ( vtable->carried_encoder_state_chunktype == greedy_chunktype )
		{
			// blend the previous optimal + the current greedy
			//	maybe very marginally better, not big
			
			newlz_scratchblock & carried_encoder_state = const_cast<newlz_scratchblock &>(vtable->carried_encoder_state);
			
			RR_ASSERT( carried_encoder_state.size() == sizeof(newLZF_passinfo) );
			const newLZF_passinfo * p_prev_passinfo = (const newLZF_passinfo *) carried_encoder_state.get();		
			
			optimal_rescale_passinfo2(passinfo,*p_prev_passinfo);
		}
		else
		{
			// wipe it :
			//rrMemSet32_Aligned(&passinfo,1,sizeof(passinfo));
			// else it comes from the greedy pass
			optimal_rescale_passinfo(passinfo);
		}
	}
	
	newLZF_passinfo_to_codecost(passinfo,codecosts);
		
	const OodleLZ_CompressOptions * pOptions = vtable->pOptions;
	S32 base_cost_disable_mask = 0;
	
	// @@@@ -z7 MML down to 3 ?
	//	Only in Mermaid, doesn't make sense in Selkie (?)
	// the way this works is
	//	the greedy-for-optimal pass used MML 4
	//	so the MML 3 packets were impossible, hence max codecost (11 bits)
	// By setting MML 3 they are now allowed here, and chosen if codecost likes them
	if ( vtable->level >= OodleLZ_CompressionLevel_Optimal3 )
	{
		mml = RR_MAX(3,pOptions->minMatchLen);

		// Disable LO base cost optimization at z7+
		// this is slightly more accurate but noticeably slower
		base_cost_disable_mask = RR_S32_MAX;
	}
	
	S32 offsLimitTab[32];
	newLZF_InitOffsLimitTab(offsLimitTab, mml, off24mml, pOptions);

	U32 dictionarySize = ( pOptions->dictionarySize > 0 ) ?
		RR_MIN(NEWLZF_MAX_OFFSET,pOptions->dictionarySize)
		: NEWLZF_MAX_OFFSET;
	
	int final_arrival_pos = optimal_parse_start_pos;
	
	{
	SIMPLEPROFILE_SCOPE_N(optimal_parse,chunk_len);
		
	RR_ASSERT( parse_end_pos < chunk_len - NEWLZF_CHUNK_MATCH_SAFE_ZONE || chunk_len == (1<<16) );
	             
	// parse in chunks :
	int parse_chunk_start = optimal_parse_start_pos;
	
	for(;;)
	{
		RR_ASSERT( parse_chunk_start < chunk_len );
		if ( parse_chunk_start >= parse_end_pos )
			break;
						
		int parse_chunk_end = parse_chunk_start + parse_chunk_len;
		parse_chunk_end = RR_MIN(parse_chunk_end,parse_end_pos);
		
		int parse_chunk_end_max = parse_chunk_start + parse_chunk_len_max;
		parse_chunk_end_max = RR_MIN(parse_chunk_end_max,parse_end_pos);
		
	
		// cheap_preceding_arrival_pos is a good arrival <= pos
		// cheap_preceding_arrival_literal_cost = running cost of literals between cheap_preceding_arrival_pos & pos
		int cheap_preceding_arrival_pos = parse_chunk_start;
		S32 cheap_preceding_arrival_literal_cost = 0;

		// cost for literal runs of len 0 is always 0
		// others, we cache in advance
		S32 lrl_costs[8][8] = {}; // [lrl][arrive_pos & 7]
		for (int lrl = 1; lrl <= 7; ++lrl)
		{
			for (int i = 0; i < 8; i++)
				lrl_costs[lrl][i] = RR_S32_MAX;
		}

		int parse_backup_limit = parse_chunk_start; // don't walk back before this

		for(int pos=parse_chunk_start; 
			parse_chunk_end <= parse_chunk_end_max;
			pos++)
		{
			if ( 0 )
			{
				// goto for long match jump-ahead :
				continue_parse_loop:
				if ( pos >= parse_chunk_end_max )
				{
					parse_chunk_end = pos;
					break;
				}

				parse_backup_limit = pos;

				// wipe out the literal arrival state :
				cheap_preceding_arrival_pos = pos;
				cheap_preceding_arrival_literal_cost = 0;

				// wipe cached LRL costs on long arrivals
				// need to take LRL=0 next
				for (int lrl = 1; lrl <= 7; ++lrl)
				{
					for (int i = 0; i < 8; i++)
						lrl_costs[lrl][i] = RR_S32_MAX;
				}
			}
			
			// if we reach parse_end_pos, we're done :
			if ( pos == parse_end_pos )
			{
				parse_chunk_end = parse_end_pos;
				break;
			}
						
			const U8 * ptr = chunk_ptr + pos;
			U32 ptr32 = RR_GET32_NATIVE_UNALIGNED(ptr);

			// I always have an arrival :
			//	because at [pos-7] I did a 7-lrl step
			//  *except* after a long jump
			//	in which case pos-7 wasn't visited
			// I could fill a bunch of dummy entries when I do a long jump (and also near start_pos)
			//	or I can just check for un-filled slots
			//	and know they are rare
			//RR_ASSERT( arrivals[pos].cost != RR_S32_MAX );
			
			// Mermaid !
			#if 1
			// cheap_preceding_arrival_pos maintenance		
			// -> optional!
			// -> this is just for finding LRL_EXCESS runs
			if ( (pos - cheap_preceding_arrival_pos) > 0 )
			{
				// extend cheap_preceding_arrival_literal_cost
				S32 lo = arrivals[cheap_preceding_arrival_pos].offset;
				U8 sub_literal = (U8)(ptr[-1] - (ptr[-1-lo]&codecosts.subliteralmask));
				cheap_preceding_arrival_literal_cost += codecosts.literal[sub_literal];
			
				// @@ N^2 assert can make debug very slow :
				NEWLZF_ASSERT_HEAVY( cost_literals(ptr - (pos - cheap_preceding_arrival_pos),(pos - cheap_preceding_arrival_pos),arrivals[cheap_preceding_arrival_pos].offset,codecosts,subliteralmask) == cheap_preceding_arrival_literal_cost );
				
				if ( arrivals[pos].cost != RR_S32_MAX )
				{
					int lrl = (pos - cheap_preceding_arrival_pos);
					if ( lrl >= NEWLZF_LRL_EXCESS )
					{
						// consider replacing current arrival with an LRL_EXCESS
						// @@ this can happen over and over in a long lrl span
						
						// 0 escape packet :
						S32 cost = codecosts.packet[0];

						int lrl_excess = lrl - NEWLZF_LRL_EXCESS;
						cost += newlzf_countbytesv(lrl_excess) * COST_ONE_BYTE;
						cost += cheap_preceding_arrival_literal_cost;
						cost += arrivals[cheap_preceding_arrival_pos].cost;
						
						if ( cost < arrivals[pos].cost )
						{
							arrivals[pos].cost = cost;
							arrivals[pos].lrl = lrl;
							arrivals[pos].ml = 0;
							arrivals[pos].offset = lo;
							arrivals[pos].is_twostep = 0;
						}				
					}
			
					// consider updating cheap_preceding_arrival_pos to be the current pos
					//	but only if it's an arrival that's not trivial
					if ( arrivals[pos].ml > 1 )
					{
						S32 prev_cost = arrivals[cheap_preceding_arrival_pos].cost;

						S32 cost_lrl = 0;
						int lrl_for_packet = lrl;
						if ( lrl > 7 )
							cost_lrl = cost_long_lrl(lrl,codecosts,&lrl_for_packet);

						// lrl_for_packet not included
						
						S32 cost = prev_cost + cost_lrl + cheap_preceding_arrival_literal_cost;
						
						if ( cost >= arrivals[pos].cost )
						{
							cheap_preceding_arrival_pos = pos;
							cheap_preceding_arrival_literal_cost = 0;
						}
					}
				}
			}
			#endif // cheap_preceding_arrival_pos maintenance

			//---------------------------------------------------------------
						
			#if 1 // -> yes good
			
			// trying to get parse_chunk_len chunk adaptation
			//
			// if pos is near chunk end, and arrivals past here are trivial
			//	terminate the chunk and wipe them out
			//
			// (you can never actually hit pos == parse_chunk_end
			//	 because I'm always filling literal (and replen1) arrivals at pos+7)

			if ( pos+8 >= parse_chunk_end && 
				arrivals[pos].cost != RR_S32_MAX &&
				arrivals[pos].ml > 1 &&
				(pos - parse_chunk_start) >= parse_chunk_len
				)
			//	@@ pos == cheap_preceding_arrival_pos 
			{
				bool any_non_trivial = false;
				for (int p= pos+1; p <= parse_chunk_end ; p ++)
				{
					if ( arrivals[p].cost != RR_S32_MAX && 
						arrivals[p].ml > 1 )
					{
						any_non_trivial = true;
						break;
					}
				}
				
				if ( ! any_non_trivial )
				{
					// wipe them :
					for (int p= pos+1; p <= parse_chunk_end ; p ++)
					{
						arrivals[p].cost = RR_S32_MAX;
						arrivals[p].ml = 0;
					}
					
					parse_chunk_end = pos;
					break;
				}
			}
			
			#endif			
			
			//---------------------------------------------------------------
			
			if ( arrivals[pos].cost != RR_S32_MAX )
			{
				const S32 lo = arrivals[pos].offset;
				const U8 * ptr_lo = ptr - lo;

				// fill out LRL run cost for future arrivals from here
				S32 cost_sum = 0;
				S32 lrl_bound = RR_MIN(parse_end_pos - pos, 8);

				for (int lrl = 1; lrl < lrl_bound; ++lrl)
				{
					const int i = lrl - 1;
					int sub_literal = (U8)(ptr[i] - (ptr_lo[i] & codecosts.subliteralmask));
					cost_sum += codecosts.literal[sub_literal];
					lrl_costs[lrl][(pos + lrl) & 7] = cost_sum;
				}

				if ( pos+7 < parse_end_pos)
				{
					// fill a 7-LRL 0-ML arrival from here
					const S32 base_cost = arrivals[pos].cost;
					
					//7+0 only :
					//SUM:average : 10,580,573 -> 5,841,885 =  4.417 bpb =  1.811 to 1 
					//SUM:total   : 10,580,573 -> 5,577,163 =  4.217 bpb =  1.897 to 1 

					//6+1 test :
					//SUM:average : 10,580,573 -> 5,841,701 =  4.417 bpb =  1.811 to 1 
					//SUM:total   : 10,580,573 -> 5,577,049 =  4.217 bpb =  1.897 to 1 

					// do 6+1 instead of 7+0 if possible :
					// this is important for Selkie, much less so for Mermaid.
					if ( ptr[6] == ptr_lo[6] )
					{
						const S32 base_lrl6 = base_cost + lrl_costs[6][(pos + 6) & 7];
						S32 cost = base_lrl6 + codecosts.packet[NEWLZF_PACKET_LO_FLAG + 6 + (1<<3)];
						try_arrival(arrivals,pos+6,cost,6,1,lo,parse_chunk_end);
					}
					else
					{
						const S32 base_lrl7 = base_cost + lrl_costs[7][(pos + 7) & 7];
						S32 cost = base_lrl7 + codecosts.packet[NEWLZF_PACKET_LO_FLAG + 7];
						try_arrival(arrivals,pos+7,cost,7,0,lo,parse_chunk_end);
					
						// tack a rep on the end too if we can :
						
						// this does help a tiny bit, not sure why exactly
						// shouldn't it be found anyway when we get to pos+7 ?
						if ( ptr[7] == ptr_lo[7] )
						{	
							cost = base_lrl7 + codecosts.packet[NEWLZF_PACKET_LO_FLAG + 7 + (1<<3)];
							try_arrival(arrivals,pos+7,cost,7,1,lo,parse_chunk_end);
						}
					}
				}
			}
			
			//---------------------------------------------------------------
			
			UnpackedMatchPair near_matches[NEWLZF_MATCH_NUM_PAIRS];
			UnpackedMatchPair far_matches[NEWLZF_MATCH_NUM_PAIRS];
			int near_matches_count = 0;
			int far_matches_count = 0;
			
			const UnpackedMatchPair * pairs = matches[pos].pairs;
			
			S32 longestml = 0;
			S32 longestoff = 0;
			bool any_escapes = false;
			
			// for all normal matches :
			for(int m=0;m<NEWLZF_MATCH_NUM_PAIRS;m++)
			{
				S32 ml = pairs[m].length;
				
				// matches are in descending length
				// so when we hit a short one, get out
				if ( ml < mml ) // ml == 0 is the end-of-data marker
					break;
				
				U32 offset = pairs[m].offset;
				const SINTa len_limit = ptr_matchend - ptr;
				
				if ( ml > len_limit )
					ml = (S32)len_limit;
				
				// verify match :
				NEWLZF_ASSERT_HEAVY( memcmp(ptr,ptr-(SINTa)offset,ml) == 0 );
				
				if ( offset >= dictionarySize )
					continue;
					
				if ( offset < NEWLZF_MIN_OFFSET )
				{
					offset = newLZF_FixSmallOffset(offset);
					if ( (SINTa)offset > rrPtrDiff(ptr - dictionaryBase) )
						continue;
					
					ml = getmatchlen_mml4_one32(ptr32,ptr,ptr-offset,ptr_matchend);

					// If this offset did not get snapped to the min offset, also test at min offset
					// and take whichever is best; in case of ties, prefer MIN_OFFSET (which is also
					// what we're going to use for most runs so it's a likely common offset).
					if ( offset != NEWLZF_MIN_OFFSET )
					{
						S32 minml = getmatchlen_mml4_one32(ptr32,ptr,ptr-NEWLZF_MIN_OFFSET,ptr_matchend);
						if ( minml >= ml )
						{
							offset = NEWLZF_MIN_OFFSET;
							ml = minml;
						}
					}
					
					if ( ml < mml )
						continue;
				}
									
				RR_ASSERT( ml >= 3 && ml >= mml );
									
				if ( ! newLZF_IsAllowedNormalMatch(ml,offset,offsLimitTab) )
					continue;

				if ( offset <= 0xFFFF )
				{
					near_matches[near_matches_count].length = ml;
					near_matches[near_matches_count].offset = offset;
					near_matches_count++;
				}
				else
				{
					far_matches[far_matches_count].length = ml;
					far_matches[far_matches_count].offset = offset;
					far_matches_count++;
					any_escapes = true;
				}

				if ( ml > longestml )
				{
					longestml = ml;
					longestoff = offset;
					if ( ml >= NEWLZF_ML_EXCESS ) any_escapes = true;
				}
			}
			
			const int max_lrl = RR_MIN((pos-parse_backup_limit),7);
			
			// very long matches or high offsets :
			if ( any_escapes )
			{
				// if I have a long ML ; just take it unconditionally
				// find the cheapest way to get to our long match :

				// note this path does not consider LRL 0-7 possibility * # match possibilities
				//	it adds them, doesn't multiply

				int cheapest_lrl = 0;
				S32 cheapest_base_cost = arrivals[pos].cost;
				for(int lrl=1;lrl<=max_lrl;lrl++)
				{			
					S32 arrival_cost = arrivals[pos-lrl].cost;				
					if ( arrival_cost == RR_S32_MAX )
						continue;
						
					RR_DURING_ASSERT( const S32 lo = arrivals[pos-lrl].offset );

					RR_ASSERT( lrl > 0 && lrl <= 7 );
					
					S32 lrl_base_cost = lrl_costs[lrl][pos & 7];
					RR_ASSERT( lrl_base_cost != RR_S32_MAX );
					NEWLZF_ASSERT_HEAVY( lrl_base_cost == cost_literals(ptr-lrl,lrl,lo,codecosts));
					
					// send an LRL with no match packet :
					lrl_base_cost += codecosts.packet[128 + lrl];

					S32 base_cost = arrival_cost + lrl_base_cost;
						
					if ( base_cost < cheapest_base_cost )
					{
						cheapest_base_cost = base_cost;
						cheapest_lrl = lrl;
					}
				}
				
				RR_ASSERT( cheapest_base_cost != RR_S32_MAX );
				
				S32 base_cost = cheapest_base_cost + COST_PENALTY_ESCAPE;
				int lrl = cheapest_lrl;

				if ( longestml >= NEWLZF_ML_EXCESS )
				{
					// ML_EXCESS are taken unconditionally
				
					S32 ml = longestml;
					S32 offset = longestoff;
					
					// cost here is actually irrelevant because it's a forced skip

					S32 cost = base_cost + COST_ONE_BYTE;
					try_arrival(arrivals,pos,cost,lrl,ml,offset,parse_chunk_end);
					
					// skip ahead :
					pos += ml;
					goto continue_parse_loop;
				}
				else
				{
					const S32 lo = arrivals[pos-lrl].offset;
									
					// cost the 3-byte offsets here :
					for(int m=0;m<far_matches_count;m++)
					{
						S32 ml = far_matches[m].length;
						S32 offset = far_matches[m].offset;
						
						// doesn't qualify for 16-bit offset escape case :
						RR_ASSERT( ml < NEWLZF_ML_EXCESS );
						RR_ASSERT( offset > 0xFFFF ); // only excesses here
						
						// don't do LO here , it will be costed wrong
						// doing it here is a total NOP for compression
						// the normal LO path will do it and get a lower cost and replace our work
						if ( offset == lo )
							continue;
						
						int packet,escape_ml;
						escape_match_transmission(offset,ml,&packet,&escape_ml);
						
						S32 cost = base_cost + codecosts.packet[packet]; // COST_PENALTY_ESCAPE is in base_cost
						cost += newlzf_match_escape_costs(offset,pos,chunk_base,escape_ml);
					
						try_arrival(arrivals,pos,cost,lrl,ml,offset,parse_chunk_end);
						
						mermaid_try_twostep(arrivals,pos,parse_end_pos,ml,lrl,offset,cost,
							ptr,ptr_matchend,codecosts,parse_chunk_end);
							
						// ?? try ml-1 too
						//	meh, tiny benefit?
						try_arrival(arrivals,pos,cost,lrl,ml-1,offset,parse_chunk_end);
						//try_arrival(arrivals,pos,cost,lrl,ml-2,offset,parse_chunk_end);
					}					
				}
			}
					
			// try LRL 0-7 + LO or match :
					
			// lowest_base_cost is just a heuristic to cost the normal matches less often
			S32 lowest_base_cost = RR_S32_MAX;
			const U8 match_zero_mask = ( near_matches_count == 0 ) ? 0xff : 0x00;
			S32 prev_lo = -1;
			S32 prev_base_cost = RR_S32_MAX;

			S32 match_lrl[8];
			S32 match_base_cost[8];
			SINTa match_lrl_candidate_count = 0;

			for(int lrl=0;lrl<=max_lrl;lrl++)		
			{
				S32 arrival_cost = arrivals[pos-lrl].cost;				
				if ( arrival_cost == RR_S32_MAX )
				{
					continue;
				}
						
				const S32 lo = arrivals[pos-lrl].offset;
				RR_ASSERT( lo >= NEWLZF_MIN_OFFSET );

				const U8 lo_byte0_xor = *ptr ^ ptr[-lo];
				// This tests if ptr != ptr[-lo] && cur_matches_count == 0,
				// i.e. no legal matches (either LO or regular) at current position.
				// In that case, no need to score the literals (which is not cheap)
				if ( lo_byte0_xor & match_zero_mask )
					continue;

				S32 lrl_base_cost = lrl_costs[lrl][pos & 7];
				RR_ASSERT( lrl_base_cost != RR_S32_MAX ); // parse_backup_limit should ensure we never hit this

				NEWLZF_ASSERT_HEAVY( lrl_base_cost == cost_literals(ptr-lrl,lrl,lo,codecosts) );
				
				// lrl is always in packet :
				int lrl_for_packet = lrl;
												
				// cost base is arrival and literals :
				S32 base_cost = arrival_cost + lrl_base_cost;

				// keep track of candidates that improve on best cost for match eval below
				if ( base_cost < lowest_base_cost )
				{
					lowest_base_cost = base_cost;
					match_lrl[match_lrl_candidate_count] = lrl;
					match_base_cost[match_lrl_candidate_count] = base_cost;
					match_lrl_candidate_count++;
				}

				// find LO matches :
				// have to do this each time because LO set may be different
				if ( lo_byte0_xor == 0 &&
					 ( lo != prev_lo || base_cost < prev_base_cost ) )
				{
					prev_lo = lo;
					
					// base_cost_disable_mask = 0 usually (nop)
					// can set it to RR_S32_MAX to disable base cost updates entirely,
					// which scores all LO options always, even the ones unlikely to
					// be profitable.
					prev_base_cost = base_cost | base_cost_disable_mask;

					// mml 1 or 2 ?
					//	 you can do mml2 and use do_lomml1 to find these
					//	or just use mml1 here and let the optimal parse decide
					int loml = getmatchlen_mml1(ptr,ptr-lo,ptr_matchend);
					//int loml = getmatchlen_mml2(ptr,ptr-lo,ptr_matchend);

					RR_ASSERT( loml > 0 ); // since lo_byte0_xor == 0
				
					if ( loml >= NEWLZF_ML_EXCESS )
					{
						// @@ ??
						// normal match finder missed it, but LO found it
						// -> goto the excess handler ?
						// -> does this ever happen ?
						// @@ not a real cost cuz it's a forced skip :
						S32 cost = base_cost + COST_ONE_BYTE;
						
						try_arrival(arrivals,pos,cost,lrl,loml,lo,parse_chunk_end);
						
						// skip ahead :
						pos += loml; 
											
						// breaks out of lrl
						goto continue_parse_loop;
					}

					// Limit to max ML within a single packet; we're an optimal parser,
					// trust future positions to resume the match if it was longer.
					const S32 origml = loml;
					loml = RR_MIN(loml,15);

					S32 cost = try_lo_arrival(arrivals,pos,base_cost,lrl,lrl_for_packet,loml,lo,codecosts,parse_chunk_end);

					// don't try two-step if we truncated the match
					if ( loml == origml )
					{
						mermaid_try_twostep(arrivals,pos,parse_end_pos,loml,lrl,lo,cost,
							ptr,ptr_matchend,codecosts,parse_chunk_end);
					}

					// try len reductions down all the way to 1
					for(int tryml=1;tryml<loml;tryml++)
					{
						try_lo_arrival(arrivals,pos,base_cost,lrl,lrl_for_packet,tryml,lo,codecosts,parse_chunk_end);
					}
				}
			} // LRL loop

			// This used to check just lowest_base_cost; but with match_zero_mask, we might
			// legitimately never even get to the normal match part for all LRLs if
			// near_matches_count was 0.
			RR_ASSERT( ( lowest_base_cost != RR_S32_MAX && match_lrl_candidate_count != 0 ) || near_matches_count == 0 );

			for(int m = 0; m < near_matches_count; ++m)
			{
				S32 ml = near_matches[m].length;
				S32 offset = near_matches[m].offset;
				RR_ASSERT( offset <= 0xFFFF ); // no excesses here

				const S32 packet_ml = RR_MIN(ml,15);
				S32 lrl = 0;
				S32 base_cost = 0;
				S32 best_cost = RR_S32_MAX;

				// Of all candidate LRLs, check which one gives us the lowest combined cost of arrival,
				// literals, and packet byte. This accounts for LRL-ML correlation in the packet byte.
				for (SINTa i = 0; i < match_lrl_candidate_count; ++i)
				{
					const S32 my_lrl = match_lrl[i];
					const S32 my_base_cost = match_base_cost[i];
					const S32 my_cost = my_base_cost + cost_normal_match(my_lrl, packet_ml, codecosts);
					if ( my_cost < best_cost )
					{
						best_cost = my_cost;
						base_cost = my_base_cost;
						lrl = my_lrl;
					}
				}

				const S32 lrl_for_packet = lrl;
				const S32 lo = arrivals[pos - lrl].offset;

				RR_ASSERT( ml >= 3 && ml >= mml );
				RR_ASSERT( ptr+ml <= ptr_matchend );
				RR_ASSERT( memcmp(ptr,ptr-(SINTa)offset,ml) == 0 );
				RR_ASSERT( offset <= NEWLZF_MAX_OFFSET );
				RR_ASSERT( offset >= NEWLZF_MIN_OFFSET );
				RR_ASSERT( newLZF_IsAllowedNormalMatch(ml,offset,offsLimitTab) );

				// skip offset matching LO, we evaluated all such candidates above and
				// they are encoded differently.
				if ( offset == lo )
					continue;

				// okay, it's allowed - cost it :

				// start with cost including lrl & offset
				S32 cost = base_cost;
				// cost += 2 * COST_ONE_BYTE;
				cost += codecosts.offsetL[offset&0xFF] + codecosts.offsetH[offset>>8];

				// try multi-packet steps for longer ml ?
				//	-> yes this is important
				if ( ml >= 15 )
				{
					S32 match_cost = try_match_arrival(arrivals,pos,cost,lrl,lrl_for_packet,15,offset,codecosts,parse_chunk_end);

					int ml_done = 15;
					for(;;)
					{
						int next_ml = RR_MIN(ml-ml_done,15);
						if ( next_ml <= 1 ) break; // stop at 1 remaining, don't do final 1 packet

						// Implied match continuation packet is LO LRL=0 ML=next_ml
						match_cost += cost_lo_match(0,next_ml,codecosts);
						ml_done += next_ml;

						try_arrival(arrivals,pos,match_cost,lrl,ml_done,offset,parse_chunk_end);
					}
				}
				else
				{
					S32 match_cost = try_match_arrival(arrivals,pos,cost,lrl,lrl_for_packet,ml,offset,codecosts,parse_chunk_end);

					mermaid_try_twostep(arrivals,pos,parse_end_pos,ml,lrl,offset,match_cost,
						ptr,ptr_matchend,codecosts,parse_chunk_end);

					// for comparison : just try all ml's ?
					for(int tryml=mml;tryml<ml;tryml++)
					{
						try_match_arrival(arrivals,pos,cost,lrl,lrl_for_packet,tryml,offset,codecosts,parse_chunk_end);
					}
				}
			}
		} // parse loop for one chunk
	
		RR_ASSERT( arrivals[parse_chunk_end].cost != RR_S32_MAX || parse_chunk_end == parse_end_pos );
		RR_ASSERT( cheap_preceding_arrival_pos >= parse_chunk_start );
		
		if ( parse_chunk_end >= parse_end_pos )
		{			
			// we're at EOF
			//	I may have a final arrival at [parse_chunk_end]
			//	there may be a cheaper arrival before that at cheap_preceding_arrival_pos
		
			RR_ASSERT( cheap_preceding_arrival_pos >= parse_chunk_start );
			RR_ASSERT( cheap_preceding_arrival_pos <= parse_chunk_end );
				
				
			// last_arrival_pos can be after parse_end_pos from a match :
			int last_arrival_pos = chunk_len; //-1;
			while( arrivals[last_arrival_pos].cost == RR_S32_MAX )
				last_arrival_pos --;
			
			RR_ASSERT( last_arrival_pos == parse_chunk_end || parse_chunk_end == parse_end_pos );
		
			// choose my final LRL span, which finishes the parse
			// -> non trivial because LO affects the sub literals cost
			//  cheap_preceding_arrival_pos is a valid arrival near the end
			//	there may be more after that due to matches found
			// -> backing up 8 from cheap_preceding_arrival_pos to consider a few more
			#define FINAL_EXTRA_BACKUP	16
			final_arrival_pos = RR_MAX(last_arrival_pos-FINAL_EXTRA_BACKUP,parse_chunk_start);
			S32 final_arrival_cost = RR_S32_MAX;
			for(int pos=final_arrival_pos;pos<=last_arrival_pos;pos++)
			{
				S32 cost = arrivals[pos].cost;
				if ( cost == RR_S32_MAX )
					continue;
			
				const U8 * ptr = chunk_ptr + pos;
				S32 lo = arrivals[pos].offset;
					
				cost += cost_literals(ptr,(chunk_len - pos),lo,codecosts);
				
				if ( cost < final_arrival_cost )
				{
					final_arrival_cost = cost;
					final_arrival_pos = pos;
				}
			}
			
			RR_ASSERT( final_arrival_cost != RR_S32_MAX );
			
			// also consider cheap_preceding_arrival ?
			if ( cheap_preceding_arrival_pos < (last_arrival_pos-FINAL_EXTRA_BACKUP) )
			{
				S32 pos = cheap_preceding_arrival_pos;
				S32 cost = arrivals[pos].cost;
				RR_ASSERT( cost != RR_S32_MAX );
			
				const U8 * ptr = chunk_ptr + pos;
				S32 lo = arrivals[pos].offset;
					
				cost += cost_literals(ptr,(chunk_len - pos),lo,codecosts);
				
				if ( cost < final_arrival_cost )
				{
					final_arrival_cost = cost;
					final_arrival_pos = pos;
				}		
			}
			
			RR_ASSERT( final_arrival_cost != RR_S32_MAX ); 
			
			parse_chunk_end = final_arrival_pos;
		}
		else
		{
			// non-terminal chunk
		}
		
		S32 parsevec_size_before = parsevec.size32();
		
		newLZF_append_arrivals_to_parsevec(
			parsevec,
			arrivals,
			parse_chunk_start,parse_chunk_end,
			chunk_ptr);
		
		SINTa lastoffset = arrivals[parse_chunk_start].offset;
		
		// update histos for chunk
		
		newLZF_update_passinfo(
			greedy_chunktype,
			parsevec,parsevec_size_before,
			parse_chunk_start,
			passinfo,
			chunk_ptr,
			lastoffset);
		
		newLZF_passinfo_to_codecost(
			passinfo,
			codecosts);
		
		if ( parse_chunk_end == final_arrival_pos )
			break;
			
		RR_ASSERT( parse_chunk_end < parse_end_pos );
		
		parse_chunk_start = parse_chunk_end;
	

	} // chunks
		
	} // profile scope

	//*p_final_arrival_pos = final_arrival_pos;
	*p_lastoffset = arrivals[final_arrival_pos].offset;
}	

#if 1

static RADINLINE void selkie_try_twostep(
	newlzf_optimal_arrival * arrivals,
	int pos, int parse_end_pos,
	int ml,int lrl,int offset,S32 cost,
	const U8 * ptr, const U8 * ptr_matchend,
	int & parse_chunk_end)
{
	// pos/ptr are for the original match, but we care about what happens after
	pos += ml;
	ptr += ml;

	S32 twostep_lrl, twostep_ml;
	if ( ! find_twostep_match(ptr, offset, pos, parse_end_pos, ptr_matchend, &twostep_lrl, &twostep_ml) )
		return;

	// twostep_lrl == 0 is possible if we are called after a non-full-length match
	RR_ASSERT( twostep_lrl > 0 && twostep_lrl <= 7 );
	RR_ASSERT( twostep_ml > 0 );
	
	// is twostep ml == 1 interesting?
	//	-> yes appears to help a little

	// 7,105,158 -> 3,332,053 =  3.752 bpb =  2.132 to 1 
	//decode (x20)     : 3.549 millis, 0.86 c/b, rate= 2002.29 mb/s
	//if ( twostep_ml > 1 )
	// 7,105,158 -> 3,328,099 =  3.747 bpb =  2.135 to 1 
	//decode (x20)     : 3.482 millis, 0.85 c/b, rate= 2040.43 mb/s
	//if ( twostep_ml <= 1 ) return;

	cost += twostep_lrl * COST_ONE_BYTE;
	cost += COST_SELKIE_PACKET;
	// @@ just clamp to 15 for now ? :
	twostep_ml = RR_MIN(twostep_ml,15);
	
	int twostep_arrival_pos = pos+twostep_lrl+twostep_ml;
	newlzf_optimal_arrival & arrival = arrivals[twostep_arrival_pos];
	
	if ( cost < arrival.cost )
	{
		arrival.cost = cost;
		arrival.ml = ml;
		arrival.lrl = lrl;
		arrival.offset = offset;
		arrival.twostep.lrl = twostep_lrl;
		arrival.twostep.ml = twostep_ml;
	}
}

#else

static RADINLINE void selkie_try_twostep(
	newlzf_optimal_arrival * arrivals,
	int pos, int parse_end_pos,
	int ml,int lrl,int offset,S32 cost,
	const U8 * ptr, const U8 * ptr_matchend)
{
}

#endif

/**

ALL_LEN_REDUCTIONS is pretty cheap in Selkie because the cost doesn't change

BUT it's also nearly no gain in compression

**/
				
//#define SELKIE_TRY_ALL_LEN_REDUCTIONS	1
//   768,771 ->   351,512 =  3.658 bpb =  2.187 to 1 
// 7,105,158 -> 3,331,571 =  3.751 bpb =  2.133 to 1 
//10,000,000 -> 3,929,219 =  3.143 bpb =  2.545 to 1 

#define SELKIE_TRY_ALL_LEN_REDUCTIONS	0
//   768,771 ->   351,581 =  3.659 bpb =  2.187 to 1 
// 7,105,158 -> 3,332,536 =  3.752 bpb =  2.132 to 1 
//10,000,000 -> 3,931,780 =  3.145 bpb =  2.543 to 1 


static void newLZF_encode_chunk_optimal_sub_Selkie(
	// out :	
	newlzf_optimal_arrival * arrivals,
	int * p_lastoffset,
	int * p_final_arrival_pos,
	// in : 
	const newlz_vtable * vtable,
	int start_pos, int parse_end_pos,
	const U8 * chunk_ptr, int chunk_base, int chunk_len,
	const newLZF_MatchParseRecord * matches,
	const U8 * ptr_matchend,
	int mml,
	int off24mml,
	const U8 * dictionaryBase
	)
{
	// cheap_preceding_arrival_pos is the last arrival that's <= pos
	//	(there are arrivals filled past that spot)
	int cheap_preceding_arrival_pos = start_pos;

	const OodleLZ_CompressOptions * pOptions = vtable->pOptions;
	
	U32 dictionarySize = ( pOptions->dictionarySize > 0 ) ?
		RR_MIN(NEWLZF_MAX_OFFSET,pOptions->dictionarySize)
		: NEWLZF_MAX_OFFSET;
		
	int offsLimitTab[32];
	newLZF_InitOffsLimitTab(offsLimitTab, mml, off24mml, pOptions);

	{
	SIMPLEPROFILE_SCOPE_N(optimal_parse,chunk_len);
	
	/*
	for(int lrl=1;lrl<=7;lrl++)
	{
		RR_ASSERT( start_pos+lrl < parse_end_pos );
		
		// fill a pure-lrl arrival :
				
		arrivals[start_pos+lrl].cost = codecost_packet[128 + lrl] + cost_literals(chunk_ptr + start_pos,lrl,NEWLZF_MIN_OFFSET,codecost_literal,subliteralmask);
		arrivals[start_pos+lrl].ml = 0;
		arrivals[start_pos+lrl].lrl = lrl;
		arrivals[start_pos+lrl].offset = NEWLZF_MIN_OFFSET;
	}
	*/
	
	int parse_chunk_end = start_pos; // @@ NOT USED, but try_arrivals shared with Mermaid updates it
	
	for(int pos=start_pos;;pos++)
	{
		if ( 0 )
		{
			// target for long match jump-ahead
			continue_parse_loop:

			// wipe out the literal arrival state
			cheap_preceding_arrival_pos = pos;
		}
		
		if ( pos >= parse_end_pos )
			break;

		const U8 * ptr = chunk_ptr + pos;
		U32 ptr32 = RR_GET32_NATIVE_UNALIGNED(ptr);
		
		// Selkie !!
		#if 1
		// cheap_preceding_arrival_pos maintenance		
		//	this is used for finding LRL_EXCESS steps
		//	also now used to find the cheapest lrl arrival without searching
		// for LRL_EXCESS purposes this should really be on a delay
		//	but then I can't use it the other way
		if ( (pos - cheap_preceding_arrival_pos) > 0 && arrivals[pos].cost != RR_S32_MAX )
		{
			int lrl = (pos - cheap_preceding_arrival_pos);
	
			S32 prev_cost = arrivals[cheap_preceding_arrival_pos].cost
				+ lrl * COST_ONE_BYTE;
			
			if ( prev_cost > arrivals[pos].cost )
			{
				// don't update cheap_preceding_arrival_pos for trivial arrivals
				if ( arrivals[pos].ml > 1 )
				{
					cheap_preceding_arrival_pos = pos;
				}
			}
			else
			{			
				if ( lrl >= NEWLZF_LRL_EXCESS )
				{
					// consider replacing current arrival with an LRL_EXCESS
					// @@ this can happen over and over in a long lrl span
					
					// 0 escape packet :
					S32 cost = COST_SELKIE_PACKET;

					//int lrl_excess = lrl - NEWLZF_LRL_EXCESS;
					//cost += newlzf_countbytesv(lrl_excess) * COST_ONE_BYTE;
					// just don't count it if it's huge
					cost += COST_ONE_BYTE;
					cost += prev_cost;
					
					if ( cost < arrivals[pos].cost )
					{
						S32 lo = arrivals[cheap_preceding_arrival_pos].offset;
						arrivals[pos].cost = cost;
						arrivals[pos].lrl = lrl;
						arrivals[pos].ml = 0;
						arrivals[pos].offset = lo;
						arrivals[pos].is_twostep = 0;
					}				
				}
			}
		}
		#endif // cheap_preceding_arrival_pos maintenance

		//---------------------------------------------------------------
		
		if ( arrivals[pos].cost != RR_S32_MAX )
		{
			if ( pos+7 <= parse_end_pos )
			{
				// lzt99 : 24,700,820 ->12,684,890
				// lzt99 : 24,700,820 ->12,683,296
				S32 lo = arrivals[pos].offset;
				S32 cost = arrivals[pos].cost;
				const U8 * ptr_lo = ptr - lo;

				// don't fill a 7-lit if I can do a 6+1 instead :
				if ( ptr[6] == ptr_lo[6] )
				{
					cost += 7 * COST_ONE_BYTE + COST_PENALTY_PACKET;
					
					try_arrival(arrivals,pos+6,cost,6,1,lo,parse_chunk_end);
				}
				else
				{
					// fill a 7-LRL 0-ML arrival from here
					// 7 literals + 1 packet
					cost += 8 * COST_ONE_BYTE + COST_PENALTY_PACKET;
					try_arrival(arrivals,pos+7,cost,7,0,lo,parse_chunk_end);
				
					// this does help a tiny bit, not sure why exactly
					// shouldn't it be found anyway when we get to pos+7 ?
					if ( ptr[7] == ptr_lo[7] )
					{
						// cost is the same
						try_arrival(arrivals,pos+7,cost,7,1,lo,parse_chunk_end);
					}
				}
			}
		}
		
		//---------------------------------------------------------------
		
		int max_lrl = RR_MIN((pos-start_pos),7);
		
		// this is just finding the MIN of (arrival.cost - pos) in the last 8
		//  can use cheap_preceding
		int cheapest_lrl = 0;
		int cheapest_base_cost = arrivals[pos].cost;
						
		int cheap_preceding_arrival_lrl = pos - cheap_preceding_arrival_pos;
		if ( cheap_preceding_arrival_lrl > 0 )
		{
			if ( cheap_preceding_arrival_lrl <= 7 )
			{
				S32 arrival_cost = arrivals[cheap_preceding_arrival_pos].cost;
				cheapest_lrl = cheap_preceding_arrival_lrl;
				cheapest_base_cost = arrival_cost + cheapest_lrl * COST_ONE_BYTE;
			}
			else
			{
				for(int lrl=1;lrl<=max_lrl;lrl++)
				{
					S32 arrival_cost = arrivals[pos-lrl].cost;				
					if ( arrival_cost == RR_S32_MAX )
					{
						continue;
					}
						
					RR_ASSERT( lrl > 0 && lrl <= 7 );
					
					S32 base_cost = arrival_cost + lrl * COST_ONE_BYTE;
					
					if ( base_cost < cheapest_base_cost )
					{
						cheapest_base_cost = base_cost;
						cheapest_lrl = lrl;
					}
				}
			}
		}
		
		RR_ASSERT( cheapest_base_cost != RR_S32_MAX );
		
		// try LRL 0-7 :
		// in Selkie the only point of the LRL loop here is to get more LO's
		// I only want the cheapest LRL
		//	and any LRL which gives me a different LO
				
		for(int lrl=0;lrl<=max_lrl;lrl++)		
		{
			S32 arrival_cost = arrivals[pos-lrl].cost;			
			if ( arrival_cost == RR_S32_MAX )
			{
				// should only happen after a long ml jump
				continue;
			}
					
			const S32 lo = arrivals[pos-lrl].offset;
			RR_ASSERT( lo >= NEWLZF_MIN_OFFSET );
			
			// early-out if loml==0, we need at least 1 byte for this to be worth scoring
			if ( *ptr != *(ptr - lo) )
				continue;

			if ( lrl != cheapest_lrl && lo == arrivals[pos-cheapest_lrl].offset )
			{
				// only try cheapest LRL and different LO's
				continue;
			}
			
			S32 base_cost = arrival_cost + lrl * COST_ONE_BYTE;
			
			// find LO matches :
			// have to do this each time because LO set may be different
	
			// lo mml 1 or 2 ?
			//	can do 2 and then use do_lomml1 to output the parse
			//	better is to use mml1 here and let the optimal parser decide
			int loml = getmatchlen_mml1(ptr,ptr-lo,ptr_matchend);
			//int loml = getmatchlen_mml2(ptr,ptr-lo,ptr_matchend);
			
			RR_ASSERT( loml > 0 ); // ensured by early out above
				
			if ( loml >= NEWLZF_ML_EXCESS )
			{
				// @@ ??
				// normal match finder missed it, but LO found it
				// -> goto the excess handler ?
				// -> does this ever happen ?
				
				// don't bother with computing the real cost cuz it's a forced skip :
				S32 cost = base_cost + COST_SELKIE_PACKET;
				
				try_arrival(arrivals,pos,cost,lrl,loml,lo,parse_chunk_end);
				
				// skip ahead :
				pos += loml; 
									
				// breaks out of lrl
				goto continue_parse_loop;
			}
		
			S32 lo_cost = base_cost + COST_SELKIE_PACKET;
		
			if ( loml > 15 )
			{
				// try long LO arrival :
				// the only way this could help is if
				//	the arrival I write at [pos+15]
				//	gets replaced with something cheaper
				//	so that I don't get to re-find when I get there
				// it's a way of saving this parse option so it doesn't get stomped
				// -> seems to not help in practice
				
				int remainingml = loml;
				
				S32 long_lo_cost = lo_cost;
				remainingml -= 15;
				while( remainingml > 1 )
				{
					int curml= RR_MIN(remainingml,15);
					long_lo_cost += COST_SELKIE_PACKET;
					remainingml -= curml;
				}

				RR_ASSERT( remainingml == 0 || remainingml == 1 );
				try_arrival(arrivals,pos,long_lo_cost,lrl,loml - remainingml,lo,parse_chunk_end);
				try_arrival(arrivals,pos,lo_cost,lrl,15,lo,parse_chunk_end);
				try_arrival(arrivals,pos,lo_cost,lrl,14,lo,parse_chunk_end);

				// 7,105,158 -> 3,332,433 =  3.752 bpb =  2.132 to 1 
				// 7,105,158 -> 3,332,358 =  3.752 bpb =  2.132 to 1 
				// helps a few bytes to try a twostep here
				//if ( remainingml == 0 )
				//{
				//	selkie_try_twostep(arrivals,pos,parse_end_pos,loml - remainingml,lrl,lo,long_lo_cost,ptr,ptr_matchend,parse_chunk_end);
				//}
			}
			else
			{
				try_arrival(arrivals,pos,lo_cost,lrl,loml,lo,parse_chunk_end);

				selkie_try_twostep(arrivals,pos,parse_end_pos,loml,lrl,lo,lo_cost,ptr,ptr_matchend,parse_chunk_end);
				
				#if SELKIE_TRY_ALL_LEN_REDUCTIONS
				
				while ( loml >= 2 )
				{
					loml--;
					try_arrival(arrivals,pos,lo_cost,lrl,loml,lo,parse_chunk_end);
				}
				
				#else
				
				// allowing == 2 (reduces to 1) would be okay too
				// 7,105,158 -> 3,332,536 =  3.752 bpb =  2.132 to 1 
				// 7,105,158 -> 3,332,445 =  3.752 bpb =  2.132 to 1 

				if ( loml > 2 )
				{
					try_arrival(arrivals,pos,lo_cost,lrl,loml-1,lo,parse_chunk_end);
				}
				
				#endif
			}
			
			// helps a teeny tiny bit; not big like it was in the push-forward rep variant
			//try_arrival(arrivals,pos,lo_cost,lrl,1,lo);
		} // lrl loop
		
		//---------------------------------------------------------------
		
		const UnpackedMatchPair * pairs = matches[pos].pairs;

		// no normal match, continue :
		if ( pairs[0].length < mml )
			continue;
		
		UnpackedMatchPair cur_matches[NEWLZF_MATCH_NUM_PAIRS];
		int cur_matches_count = 0;
				
		S32 longestml = 0;
		S32 longestoff = 0;
		bool any_escapes = false;
		
		// for all normal matches :
		for(int m=0;m<NEWLZF_MATCH_NUM_PAIRS;m++)
		{
			S32 ml = pairs[m].length;
			
			// matches are in descending length
			// so when we hit a short one, get out
			if ( ml < mml ) // ml == 0 is the end-of-data marker
				break;
			
			U32 offset = pairs[m].offset;
			const SINTa len_limit = rrPtrDiff(ptr_matchend - ptr);

			if ( ml > len_limit )
				ml = (S32)len_limit;
			
			// verify match :
			NEWLZF_ASSERT_HEAVY( memcmp(ptr,ptr-(SINTa)offset,ml) == 0 );
			
			if ( offset >= dictionarySize )
				continue;
				
			if ( offset < NEWLZF_MIN_OFFSET )
			{
				offset = newLZF_FixSmallOffset(offset);
				if ( (SINTa)offset > rrPtrDiff(ptr - dictionaryBase) )
					continue;

				ml = getmatchlen_mml4_one32(ptr32,ptr,ptr-offset,ptr_matchend);

				// If this offset did not get snapped to the min offset, also test at min offset
				// and take whichever is best; in case of ties, prefer MIN_OFFSET. Unlike Mermaid
				// where offsets get entropy-coded so it makes sense to bias towards common
				// offsets, in Selkie there is no strong reason to prefer either offset, but also
				// no reason not to.
				if ( offset != NEWLZF_MIN_OFFSET )
				{
					S32 minml = getmatchlen_mml4_one32(ptr32,ptr,ptr-NEWLZF_MIN_OFFSET,ptr_matchend);
					if ( minml > ml )
					{
						offset = NEWLZF_MIN_OFFSET;
						ml = minml;
					}
				}
				
				if ( ml < mml )
					continue;
			}
									
			RR_ASSERT( ml >= mml );
								
			if ( ! newLZF_IsAllowedNormalMatch(ml,offset,offsLimitTab) )
				continue;
				
			cur_matches[cur_matches_count].length = ml;
			cur_matches[cur_matches_count].offset = offset;
			cur_matches_count++;
			
			if ( offset > 0xFFFF ) any_escapes = true;
			
			if ( ml > longestml )
			{
				longestml = ml;
				longestoff = offset;
				if ( ml >= NEWLZF_ML_EXCESS ) any_escapes = true;
			}
		}
		
		if ( cur_matches_count == 0 )
			continue;
		
		if ( any_escapes )
		{
			// have a long ML ; just take it unconditionally
			// find the cheapest way to get to our long match :
					
			int escape_cheapest_lrl = 0;
			S32 escape_cheapest_base_cost = arrivals[pos].cost;	

			// if cheapest LRL arrival isn't LRL=0, try that too
			if ( cheapest_lrl != 0 )
			{
				S32 base_cost = cheapest_base_cost + COST_SELKIE_PACKET;

				if ( base_cost < escape_cheapest_base_cost )
				{
					escape_cheapest_base_cost = base_cost;
					escape_cheapest_lrl = cheapest_lrl;
				}
			}
			
			RR_ASSERT( escape_cheapest_base_cost != RR_S32_MAX );
			
			S32 base_cost = escape_cheapest_base_cost;
			int lrl = escape_cheapest_lrl;
			
			base_cost += COST_PENALTY_ESCAPE + COST_SELKIE_PACKET;
		
			if ( longestml >= NEWLZF_ML_EXCESS )
			{
				S32 ml = longestml;
				S32 offset = longestoff;
				
				// cost here is actually irrelevant because it's a forced skip
				S32 cost = base_cost;
				try_arrival(arrivals,pos,cost,lrl,ml,offset,parse_chunk_end);
				
				// skip ahead :
				pos += longestml;
				
				goto continue_parse_loop;
			}
			else
			{
				const S32 lo = arrivals[pos-lrl].offset;
								
				// cost the 3-byte offsets here :
				for(int m=0;m<cur_matches_count;m++)
				{
					S32 ml = cur_matches[m].length;
					S32 offset = cur_matches[m].offset;
					
					// no off-16 escapes here :
					RR_ASSERT( ml < NEWLZF_ML_EXCESS );
					if ( offset <= 0xFFFF )
						continue;
						
					// don't do LO here , it will be costed wrong
					// doing it here is a total NOP for compression
					// the normal LO path will do it and get a lower cost and replace our work
					if ( offset == lo )
						continue;
					
					if ( ml < off24mml )
						break; // ml's in order

					int packet,escape_ml;
					escape_match_transmission(offset,ml,&packet,&escape_ml);
					
					S32 cost = base_cost; // COST_SELKIE_PACKET and COST_PENALTY_ESCAPE already added
					cost += newlzf_match_escape_costs(offset,pos,chunk_base,escape_ml);
				
					try_arrival(arrivals,pos,cost,lrl,ml,offset,parse_chunk_end);
					
					selkie_try_twostep(arrivals,pos,parse_end_pos,ml,lrl,offset,cost,ptr,ptr_matchend,parse_chunk_end);
					
					if ( ml > off24mml )
					{
						try_arrival(arrivals,pos,cost,lrl,ml-1,offset,parse_chunk_end);
					}
				}
			}
		}
									
		// now try normal matches
		// in Selkie there's no need to consider various LRL's here
		//	since packet is not entropy coded
		//	just start with the cheapest
		
		{
			S32 lrl = cheapest_lrl;
			S32 base_cost = cheapest_base_cost;
			
			// start with cost including lrl & offset
			base_cost += 2 * COST_ONE_BYTE;
			base_cost += COST_SELKIE_PACKET;
			base_cost += COST_PENALTY_NORMAL_MATCH;
				
			for(int m=0;m<cur_matches_count;m++)
			{
				S32 ml = cur_matches[m].length;
				S32 offset = cur_matches[m].offset;
				
				if ( offset > 0xFFFF ) // no excesses here
					continue;
				
				RR_ASSERT( ml >= NEWLZF_MML_DECODE );
				RR_ASSERT( ptr+ml <= ptr_matchend );
				RR_ASSERT( memcmp(ptr,ptr-(SINTa)offset,ml) == 0 );
				RR_ASSERT( offset >= NEWLZF_MIN_OFFSET );
				RR_ASSERT( newLZF_IsAllowedNormalMatch(ml,offset,offsLimitTab) );
							
				S32 cost = base_cost;
								
				#ifdef COST_PENALTY_NORMAL_L1
				if ( offset > 8192 )
					cost += COST_PENALTY_NORMAL_L1;
				#endif
				
				if ( ml > 15 )
				{		
					// try multi-packet steps for longer ml ?
					//	-> yes this is important
				
					try_arrival(arrivals,pos,cost,lrl,15,offset,parse_chunk_end);
	
					int ml_done = 15;
					S32 cur_cost = cost;
					for(;;)
					{
						int next_ml = RR_MIN(ml-ml_done,15);
						if ( next_ml <= 1 ) break;

						cur_cost += COST_SELKIE_PACKET;
	
						ml_done += next_ml;
					
						try_arrival(arrivals,pos,cur_cost,lrl,ml_done,offset,parse_chunk_end);
					}

					RR_ASSERT( ml_done == ml || ml_done == ml-1 );

					if ( ml == ml_done )
					{
						// 7,105,158 -> 3,332,433 =  3.752 bpb =  2.132 to 1 
						// 7,105,158 -> 3,332,053 =  3.752 bpb =  2.132 to 1 
						// twostep here helps a micro bit
						selkie_try_twostep(arrivals,pos,parse_end_pos,ml_done,lrl,offset,cur_cost,ptr,ptr_matchend,parse_chunk_end);
					}
				}
				else
				{
					try_arrival(arrivals,pos,cost,lrl,ml,offset,parse_chunk_end);
					
					selkie_try_twostep(arrivals,pos,parse_end_pos,ml,lrl,offset,cost,ptr,ptr_matchend,parse_chunk_end);
					
					#if SELKIE_TRY_ALL_LEN_REDUCTIONS
					
					while ( ml > mml )
					{
						ml--;
						try_arrival(arrivals,pos,cost,lrl,ml,offset,parse_chunk_end);
					}
					
					#else
					
					if ( ml > mml )
					{
						try_arrival(arrivals,pos,cost,lrl,ml-1,offset,parse_chunk_end);
					
						if ( ml > mml+1 )
						{
							try_arrival(arrivals,pos,cost,lrl,ml-2,offset,parse_chunk_end);
						
						}
					}
					
					#endif
				}				
			}
		} // match
		
	} // parse loop
	
	} // profile scope
	
	// find final arrival pos :
	//	there can be matches that end past parse_end_pos , so find them
	// -> also back up a bit from parse_end_pos
	//	if the last packets are just all-literals packets, we want to consume them into the final lrl
	//	 (though cheap_preceding_arrival_pos also does this)
	int final_arrival_pos = 0;
	int final_arrival_cost = RR_S32_MAX;
	for(int pos=RR_MAX(start_pos,parse_end_pos-16);pos<=chunk_len;pos++)
	{
		if ( arrivals[pos].cost == RR_S32_MAX )
			continue;
		
		int cost = arrivals[pos].cost + COST_ONE_BYTE * (chunk_len - pos );
		if ( cost < final_arrival_cost )
		{
			final_arrival_pos = pos;
			final_arrival_cost = cost;
		}
	}
	
	RR_ASSERT( final_arrival_pos != 0 );
	RR_ASSERT( final_arrival_cost != RR_S32_MAX );
	
	// use final_arrival_pos or cheap_preceding_arrival_pos
	if ( cheap_preceding_arrival_pos < parse_end_pos )
	{
		S32 closest_to_end_cost = arrivals[cheap_preceding_arrival_pos].cost +
			(chunk_len - cheap_preceding_arrival_pos) * COST_ONE_BYTE;
		if ( closest_to_end_cost < final_arrival_cost )
		{
			final_arrival_pos = cheap_preceding_arrival_pos;
		}
	}
	
	*p_final_arrival_pos = final_arrival_pos;
	*p_lastoffset = arrivals[final_arrival_pos].offset;
}


#if OODLE_PLATFORM_HAS_ADVANCED_MATCHERS

OODLE_NS_END
#include "suffixtrie.h"
//#include "lzfind.h" 
OODLE_NS_START

#endif // OODLE_PLATFORM_HAS_ADVANCED_MATCHERS

//=================================================================================================

RR_COMPILER_ASSERT( NEWLZF_MML_NORMAL == 4 );

// template CTMF2<typename t_hash1_type,int t_mml1,int t_mml2,int t_chain_steps_max>
typedef CTMF2<U32,4,8,8>	newLZF_CTMF_Normal; // 12 chain steps seems to be roughly parity with CTMF1 Normal

#define NEWLZF_FAST_HASH_LEN	4
// len 5 of 6 hash is a big win on text (eg. see "dickens")
// it hurts a lot on binary (eg. see "pd3d")
// this appears to be a no-win scenario where you have to choose to target text or binary
// -> we now auto-detect and change hash in the fastest modes
//#define NEWLZF_FAST_HASH_LEN	5
//#define NEWLZF_FAST_HASH_LEN	6

//typedef CTMF<U32,2,0,NEWLZF_FAST_HASH_LEN>	newLZF_CTMF_Fast; // <- old baseline
//typedef CTMF2<U32,4,8,2>	newLZF_CTMF_Fast; // <- solid Encode Weissman win on Desktop, dubious on targets with slow mem
//typedef CTMF<U32,1,8,4>	newLZF_CTMF_Fast; // <- worse
typedef CTMF<U32,1,0,NEWLZF_FAST_HASH_LEN>	newLZF_CTMF_Fast; // <- new baseline

//===========================================================
// special <= 64k matchers :
//	can use U16 for offsets
//	also don't bother with 2nd hash
//	in general less work match-finding is okay

// CTMF2<mml1,mml2,max_chain_steps>
//typedef CTMF2<U16,4,0,8>	newLZF_CTMF_Normal_64k; 
typedef CTMF2<U16,4,0,4>	newLZF_CTMF_Normal_64k; // <- fewer chain steps = more compression !? on "seven" anyway

//typedef CTMF2<U16,4,0,2>	newLZF_CTMF_Fast_64k; 
//typedef CTMF<U16,2,0,4>	newLZF_CTMF_Fast_64k; // <- old default
typedef CTMF<U16,1,0,4>	newLZF_CTMF_Fast_64k; // <- new
// Fast 64k with CTMF or CTMF2 ? Seems marginal
//	-> in both cases not really much faster than Normal
//	-> prefer CTMF so at least it's maybe different than Normal so an interesting option

//typedef CTMF<U16,1,0,4>	newLZF_CTMF_VeryFast_64k; // <- old default
typedef CTMF<U16,0,0,4>	newLZF_CTMF_VeryFast_64k; // <- new

// 64k special case helps a lot on Normal and VeryFast
//	doesn't seem to help much in Fast or SuperFast
//	(a bit weird that VeryFast is helped so much)

//===========================================================
// HyperFast matchers

typedef FastCTMF<U32>	newLZF_CTMF_HyperFast32;
typedef FastCTMF<U16>	newLZF_CTMF_HyperFast16;

//===========================================================
	
void Mermaid_FillVTable(
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
	
	bool is_64k = ( raw_len <= 65536 && dictionaryBase == raw );
	if ( pOptions->dictionarySize > 0 && pOptions->dictionarySize <= 65536 )
		is_64k = true;
	// note : in <= 64k scenario, the table_bits min is in play
	//	so in Fast & Normal, table_bits = 17 always
	//	and in SuperFast/VeryFast , table_bits = 16

	int table_bits = GetLZMatchTableBits(raw_len,
						RR_MAX(level,OodleLZ_CompressionLevel_VeryFast), // avoid SuperFast
						pOptions,16,20,17,24);

// was , with 2 dwords per entry :
//						pOptions,17,21,18,24);
	
	vtable.decodeType = RAD_LZ_DECODE_MERMAID;	
	
	bool mermaid = compressor == OodleLZ_Compressor_Mermaid;	

	if ( mermaid )
		vtable.lambda = NEWLZF_LAMBDA_MERMAID;
	else
		vtable.lambda = NEWLZF_LAMBDA_SELKIE;

	// scale lambda by spaceSpeedTradeoffBytes  :
	vtable.lambda *= newlz_spaceSpeedTradeoffBytes_for_lambda(pOptions) * (1.f / OODLELZ_DEFAULT_SSTB);
		
	vtable.chunk_len = NEWLZF_CHUNK_LEN;
	vtable.compressor = compressor;
	vtable.level = level;
	vtable.pOptions = pOptions;

	vtable.speedfit = speedfit_get_default();

	// try_huff_chunks is off for Selkie : (& Mermaid super fast levels)
	// @@@@!!!  should Mermaid Fast do Huff chunks or not?
	//  -> make this an option the client can toggle?
	vtable.try_huff_chunks = mermaid && (level >= OodleLZ_CompressionLevel_Normal);

	vtable.entropy_flags = 0;
	vtable.bitstream_flags = 0;
	
	// note : Selkie doesn't actually check entropy_flags , it manually puts arrays uncompressed
	//  but lets try to set them up right anyway

	if ( mermaid )
	{
		vtable.entropy_flags = NEWLZ_ARRAY_FLAG_ALLOW_HUFF6; // NOTE always OK, mermaid shipped post-huff6

		if ( g_OodleLZ_BackwardsCompatible_MajorVersion >= 6 ) // Oodle 2.6.0 introduces TANS/RLE
		{
			vtable.entropy_flags |= NEWLZ_ARRAY_FLAG_ALLOW_RLE | NEWLZ_ARRAY_FLAG_ALLOW_RLEHUFF | NEWLZ_ARRAY_FLAG_ALLOW_HUFFLENS2;
			vtable.entropy_flags |= NEWLZ_ARRAY_FLAG_ALLOW_RLE_MEMSET;
	
			// NEWLZ_ARRAY_FLAG_ALLOW_SPLIT - nah, not in Mermaid <- WRONG	
			// yes do split :
			// split helps quite a lot sometimes!
			if ( level >= OodleLZ_CompressionLevel_Optimal1 )
			{
				vtable.entropy_flags |= NEWLZ_ARRAY_FLAG_ALLOW_SPLIT;
			
				// NOTE : YES _SPLIT - NO _INDEXED_SPLIT
				//	- OodleLZ_Compressor_ScratchMemSize assumes no indexed splits in Mermaid
				//	  INDEXED_SPLIT is rarely used in the Mermaid Lambda range
				//	  it would kick in at -zs32 but you should be switching to Kraken then
			
				// @@@@ TANS will almost never help in the Mermaid lambda range, don't bother ?
				//	 -> evaluate this
				vtable.entropy_flags |= NEWLZ_ARRAY_FLAG_ALLOW_TANS;
			}
		}

		// Mermaid levels go down to HyperFast3, but not 4
		// if you want a speed-viable point for HF4, disable Huffman output at HyperFast4 in arrays_output
		// but ratio does a nosedive around there and it just seems bad
		//level = RR_MAX(level, OodleLZ_CompressionLevel_HyperFast3);
	}
	else
	{
		// Selkie
		
		// Selkie can still use RLE_MEMSET :
		if ( g_OodleLZ_BackwardsCompatible_MajorVersion >= 6 )
		{
			vtable.entropy_flags |= NEWLZ_ARRAY_FLAG_ALLOW_RLE_MEMSET;
		}	
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
		else if ( raw_len > 16*1024 && raw != NULL ) // @@ not well tweaked; at some small size, don't bother
		{
			// below 64k , len 4 is often better, even on text files
	
			if ( is_64k && level == OodleLZ_CompressionLevel_Fast )
			{
				// don't
			}
			else
			if ( newlz_guess_should_hash_length_be_over_four(raw,raw_len) )
			{
				hash_len = 6;
			}
		}
	}

	//-----------------------------------------------------
	// set up newLZ "vtable" :

//template<typename newLZ_CTMF,int t_do_lazy_parse,int t_do_match_backup,int t_step_literals_shift,int t_do_lomml1>
//static SINTa newLZF_encode_chunk(const newlz_vtable * vtable,

	if ( level <= OodleLZ_CompressionLevel_VeryFast )
	{
		switch ( level )
		{
		case OodleLZ_CompressionLevel_VeryFast:
			// if not explicitly given, limit table to 19 bits : (fits in 2M L2)
			if ( pOptions->matchTableSizeLog2 <= 0 ) table_bits = RR_MIN(table_bits,19);
			// t_do_lomml1 is on
			// do lazy yes or no is a big step here
			if ( is_64k )
			{
				newlz_vtable_setup_ctmf<newLZF_CTMF_VeryFast_64k>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len);
				vtable.fp_encode_chunk = newLZF_encode_chunk<newLZF_CTMF_VeryFast_64k,1,1,0,1>;
			}
			else
			{
				newlz_vtable_setup_ctmf<newLZF_CTMF_HyperFast32>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len);
				vtable.fp_encode_chunk = newLZF_encode_chunk_fast_mode<newLZF_CTMF_HyperFast32,5,1,0,1,1>;
			}
			break;

		case OodleLZ_CompressionLevel_SuperFast:
			if ( pOptions->matchTableSizeLog2 <= 0 ) table_bits = RR_MIN(table_bits,17);
			if ( is_64k )
			{
				newlz_vtable_setup_ctmf<newLZF_CTMF_HyperFast16>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len);
				vtable.fp_encode_chunk = newLZF_encode_chunk_fast_mode<newLZF_CTMF_HyperFast16,5,1,0,1,1>;
			}
			else
			{
				newlz_vtable_setup_ctmf<newLZF_CTMF_HyperFast32>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len);
				vtable.fp_encode_chunk = newLZF_encode_chunk_fast_mode<newLZF_CTMF_HyperFast32,4,1,0,1,0>; // backup, no quadratic skip, sub-lits allowed (in principle), no partial insert
			}
			break;

		case OodleLZ_CompressionLevel_HyperFast1:
			if ( pOptions->matchTableSizeLog2 <= 0 ) table_bits = RR_MIN(table_bits,16);
			if ( is_64k )
			{
				newlz_vtable_setup_ctmf<newLZF_CTMF_HyperFast16>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len); // U16 entries
				vtable.fp_encode_chunk = newLZF_encode_chunk_fast_mode<newLZF_CTMF_HyperFast16,4,1,0,1,0>; // backup, no quadratic skip, sub-lits allowed, no partial insert
			}
			else
			{
				newlz_vtable_setup_ctmf<newLZF_CTMF_HyperFast32>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len); // U32 entries
				vtable.fp_encode_chunk = newLZF_encode_chunk_fast_mode<newLZF_CTMF_HyperFast32,4,1,0,1,0>; // backup, no quadratic skip, sub-lits allowed (in principle), no partial insert
			}
			break;

		case OodleLZ_CompressionLevel_HyperFast2:
			if ( pOptions->matchTableSizeLog2 <= 0 ) table_bits = RR_MIN(table_bits,14);
			if ( is_64k && !mermaid ) // Selkie doesn't see any effect from turning off sub lits, so do something different here
			{
				newlz_vtable_setup_ctmf<newLZF_CTMF_HyperFast16>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len); // U16 entries
				vtable.fp_encode_chunk = newLZF_encode_chunk_fast_mode<newLZF_CTMF_HyperFast16,3,1,2,0,0>; // backup, less aggresive quadratic skip, no sub-lits, no partial insert
			}
			else
			{
				newlz_vtable_setup_ctmf<newLZF_CTMF_HyperFast16>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len); // U16 entries
				vtable.fp_encode_chunk = newLZF_encode_chunk_fast_mode<newLZF_CTMF_HyperFast16,4,1,0,0,0>; // backup, no quadratic skip, no sub-lits, no partial insert
			}
			break;

		case OodleLZ_CompressionLevel_HyperFast3:
			if ( pOptions->matchTableSizeLog2 <= 0 ) table_bits = RR_MIN(table_bits,13);
			newlz_vtable_setup_ctmf<newLZF_CTMF_HyperFast16>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len); // U16 entries
			vtable.fp_encode_chunk = newLZF_encode_chunk_fast_mode<newLZF_CTMF_HyperFast16,3,1,1,0,0>; // backup, aggressive quadratic skip, no sub-lits, no partial insert
			break;

		case OodleLZ_CompressionLevel_HyperFast4:
		default:
			if ( pOptions->matchTableSizeLog2 <= 0 ) table_bits = RR_MIN(table_bits,12);
			newlz_vtable_setup_ctmf<newLZF_CTMF_HyperFast16>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len); // U16 entries
			vtable.fp_encode_chunk = newLZF_encode_chunk_fast_mode<newLZF_CTMF_HyperFast16,3,0,1,0,0>; // no backup, aggressive quadratic skip, no sub-lits, no partial insert
			break;
		}

		// Disallow the less common entropy types in fast modes
		vtable.entropy_flags &= ~( NEWLZ_ARRAY_FLAG_ALLOW_RLE | NEWLZ_ARRAY_FLAG_ALLOW_RLEHUFF);
	}
	else if ( level == OodleLZ_CompressionLevel_Fast )
	{
		if ( pOptions->matchTableSizeLog2 <= 0 ) table_bits = RR_MIN(table_bits,20);
		if ( is_64k )
		{
			newlz_vtable_setup_ctmf<newLZF_CTMF_Fast_64k>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len);
			vtable.fp_encode_chunk = newLZF_encode_chunk<newLZF_CTMF_Fast_64k,1,1,0,1>;
		}
		else
		{
			newlz_vtable_setup_ctmf<newLZF_CTMF_Fast>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len);
			vtable.fp_encode_chunk = newLZF_encode_chunk<newLZF_CTMF_Fast,1,1,0,1>;
		}

		// Fast does RLE , not TANS
	}
	#if OODLE_PLATFORM_HAS_ADVANCED_MATCHERS
	else if ( level == OodleLZ_CompressionLevel_Optimal1 )
	{
		vtable.carried_encoder_state.reserve(sizeof(newLZF_passinfo), arena);
		vtable.fp_encode_chunk = newLZF_encode_chunk_optimal;
		vtable.fp_create_match_finder = CTMF_CreateMatchFinder;
		vtable.find_all_matches_num_pairs = NEWLZF_MATCH_NUM_PAIRS;
		vtable.wants_dic_limit_splits = false;
		
		/*
		// load parsebin :
		if ( 1 )
		{
			vtable.fp_find_all_matches = NULL;
			vtable.fp_encode_chunk = newLZF_encode_chunk_load_parsebin;
		}
		*/
	}
	else if ( level >= OodleLZ_CompressionLevel_Optimal2 )
	{
		vtable.carried_encoder_state.reserve(sizeof(newLZF_passinfo), arena);
		vtable.fp_encode_chunk = newLZF_encode_chunk_optimal;
		vtable.find_all_matches_num_pairs = NEWLZF_MATCH_NUM_PAIRS;
		
		//*
		vtable.fp_create_match_finder = SuffixTrie_CreateMatchFinder;
		vtable.wants_dic_limit_splits = true;
		/*/
		// LzFind doesn't need dic splits
		//	it has a window_size constant
		vtable.fp_find_all_matches = LzFind_FindAllMatches;
		vtable.wants_dic_limit_splits = false;
		/**/
	}
	#endif
	else 
	//if ( level == OodleLZ_CompressionLevel_Normal )
	{
		// Normal + Optimals on platforms that aren't HAS_ADVANCED_MATCHERS
	
		RR_ASSERT( hash_len == 4 );
	
		if ( is_64k )
		{
			newlz_vtable_setup_ctmf<newLZF_CTMF_Normal_64k>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len);
			//vtable.fp_encode_chunk = newLZF_encode_chunk<newLZF_CTMF_Normal_64k,2,1,0,1>;
			// lazy2 doesn't seem to help at all in the 64k case, so disable it for speed
			vtable.fp_encode_chunk = newLZF_encode_chunk<newLZF_CTMF_Normal_64k,1,1,0,1>;
		}
		else
		{
			newlz_vtable_setup_ctmf<newLZF_CTMF_Normal>(&vtable,dictionaryBase,raw,table_bits,arena,hash_len);
			vtable.fp_encode_chunk = newLZF_encode_chunk<newLZF_CTMF_Normal,2,1,0,1>;
		}
	}
}

SINTa Mermaid_Compress(
	OodleLZ_Compressor compressor,
	const U8 * raw,U8 * comp,SINTa raw_len,
	OodleLZ_CompressionLevel level,
	const OodleLZ_CompressOptions * pOptions,
	const U8 * dictionaryBase,
	const LRMCascade * lrm_casc,
    rrArenaAllocator * arena)
{
	SIMPLEPROFILE_SCOPE_N(Mermaid_Compress,raw_len);
	
	rrArenaAllocatorStateSaver saver(arena); //,newlz_arena_alloc_size_if_none);
	newlz_vtable vtable;
	
	Mermaid_FillVTable(&vtable,compressor,raw_len,level,pOptions,dictionaryBase,raw,arena);
	
	// raw == NULL is for OodleLZ_GetCompressScratchMemBound
	if ( raw == NULL )
		return newlz_enc_mem_bound(&vtable,raw_len);
		
	SINTa ret = newlz_compress_vtable(&vtable,
		raw,comp,raw_len,
		dictionaryBase,
		lrm_casc,arena);

	return ret;
}

//=================================================================================================

// NEWLZF_EXTRA_SCRATCH_MEM_FOR_FUZZ ? none ?
#define NEWLZF_EXTRA_SCRATCH_MEM_FOR_FUZZ 0

// verify OODLELZ_MAX_SCRATCH_FOR_PHASE_HEADERS_AND_FUZZ is big enough :
//	the sizeof changes in different builds, so OODLELZ_MAX_SCRATCH_FOR_PHASE_HEADERS_AND_FUZZ should be an overestimate
RR_COMPILER_ASSERT( (NEWLZF_EXTRA_SCRATCH_MEM_FOR_FUZZ + sizeof(newLZF_chunk_arrays)) <= OODLELZ_MAX_SCRATCH_FOR_PHASE_HEADERS_AND_FUZZ );


OODLE_NS_END
