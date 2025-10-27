// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrbase.h"
#include "rrvarbits.h"
#include "lzasserts.h"

OODLE_NS_START

struct OodleSpeedFit;

// in theory chunk_len is variable as vtable->chunk_len
//	in practice it's always 128k
#define NEWLZ_CHUNK_LEN		128*1024

#define NEWLZ_CHUNK_HEADER_SIZE	3		// LZ or memcpy 128k chunk

#define COST_ONE_BIT		32
#define COST_ONE_BYTE	(COST_ONE_BIT*8)

// cost_entropy_threshold is in terms of COST_ONE_BIT
// codecost[] is in COST_ONE_BIT units
extern void histo_to_codecost(const U32 * histo, S32 * codecost,int alphabet,int add_cost,int cost_entropy_threshold);

// scale is integer bits
U32 newlz_array_estimate_complen_bits(const U32 * histo,int alphabet, U32 sumCounts);

static RADINLINE U32 newlz_array_estimate_complen_bytes(const U32 * histo,int alphabet, U32 sumCounts)
{
    U32 bits = newlz_array_estimate_complen_bits(histo,alphabet,sumCounts);
	return (bits+7)/8;
}

// J is always in bytes
F32 newlz_array_estimate_huff_J(const U32 * histo,int alphabet,SINTa sumCounts,F32 lambda,const OodleSpeedFit * speedfit);

rrbool newlz_guess_should_hash_length_be_over_four(const void * inBuf,SINTa inSize);

//===============================================

struct match
{
	S32 ml;
	S32 off; // negative lo_index for LO
	bool IsLO() { return off <= 0; }
};

//===============================================

/***************

Front-Back pointer standard fuzz safety via memcpy to scratch buffer.

Each iteration can read "FRONTBACK_HALF_BYTES_PER_LOOP" from Front or Back
As long as they are not crossing, they are definitely safe

The idea is that this is preferred vs checking buffer start/end because front/back are hot vars
(checking start/end has the advantage that it could be a branch that's only taken when you touch the
edges of compressed data, which would make it very rare; front/back always touch at the end of every
iteration).

There are two primary methods :

1. Single memcpy

When Front-Back touches, copy to scratch buffer.

If B-F is small, you could read up to FRONTBACK_HALF_BYTES_PER_LOOP preceding "front"
and up to FRONTBACK_HALF_BYTES_PER_LOOP following "back"

So expand the memcpy range by FRONTBACK_HALF_BYTES_PER_LOOP on each side
(total ~FRONTBACK_HALF_BYTES_PER_LOOP*4)

The distance B-F is not changed, the pointer and data are just relocated to a place with
known padding.

This means that after the copy, the fuzz check condition (B-F small) will be true every time.

With this method, B & F bit pointers (not the byte pointers) should wind up exactly touching
at the end of valid streams.


2. Double memcpy

When B-F is small,

copy F + FRONTBACK_HALF_BYTES_PER_LOOP following to the start of a scratch bufer
copy B - FRONTBACK_HALF_BYTES_PER_LOOP preceding to the end of a scratch buffer

the distance between them is now pushed out to FRONTBACK_HALF_BYTES_PER_LOOP*2 (+16)

They should not touch again, further touches are corruption.

This method requires two memcpies, but the fuzz condition should only be hit once

With this method, B & F bit pointers do NOT wind up touching, they have been spread apart.

=========================

Standard usage is with a front/back varbits

be aware of the initial refill in _GetOpen
suggest using _GetOpen_NoRefill
then MAKE_FRONTBACK_SAFE(vbfront_cur,vbback_cur);
then do the initial refills

=========================

Note that in the end, back and front bitstreams should win up exactly touching
(on valid streams, maybe not on corrupt streams)
but even on valid streams, the pointers may be ahead by 8 due to U64 bitbuf read-ahead
(eg. front & back bitbufs may contain unconsumed bits)
so you must account for the f&b read pointers crossing.


@@ Possible optimization :

measure how much padding compressed data we have around our bitbuf ( min of head & tail )
allow (B-F) to cross by that amount before doing the scratch safety copy

in most cases this should prevent the scratch-safety case from ever being hit
it makes that branch very rare
and avoids the "hitting it all the time in the tail" case

=========================

The over-read pad amount needs to be (almost) 16
The actual bits you need may end at byte 0
but the bitbuf is filled ahead another 8 bytes, full of unconsumed bits
the varbits Read pointer is advanced to the end of those bits
the branchless refill will then read 8 more bytes from that pos
total = 16

****************/


#define FRONTBACK_HALF_BYTES_PER_LOOP	(-1)  // client should set this
#define FRONTBACK_MAX_BYTES_PER_LOOP	(FRONTBACK_HALF_BYTES_PER_LOOP*2)
#define FRONTBACK_OVERREAD_PAD_SPACE	(FRONTBACK_HALF_BYTES_PER_LOOP+16) // + 16 for U64 peek ahead

// safe_space not initialized?
// in fuzz case we may read those values
//	that should be fine whatever they are, we are hardened against any values there
// but it makes valgrind unhappy
// do the memset right before the memcpy()

#define FRONTBACK_SAFE_VARS() \
	RR_COMPILER_ASSERT( FRONTBACK_HALF_BYTES_PER_LOOP > 0 ); \
	U8 safe_space[FRONTBACK_MAX_BYTES_PER_LOOP+2*FRONTBACK_OVERREAD_PAD_SPACE]; \
	const U8 * safe_buf_start; const U8 * safe_buf_end; \
	bool safe_done;

// RESET_FRONTBACK_SAFE_VARS with buffer start/end
#define RESET_FRONTBACK_SAFE_VARS(s,e) do { \
	safe_buf_start = s; \
	safe_buf_end = e; \
	safe_done = false; \
} while(0)
		
#define MAKE_FRONTBACK_SAFE(ptrf,ptrb) do { \
	U8 * b = (U8 *)(ptrb); U8 * f = (U8 *)(ptrf); \
	if ( b <= f + FRONTBACK_MAX_BYTES_PER_LOOP ) { \
	if ( ! safe_done ) { \
		safe_done = true; \
		REQUIRE_FUZZ_RETURN( b >= f , -1); \
		SINTa fb_size = b - f; \
		RR_ASSERT( f >= safe_buf_start ); \
		RR_ASSERT( safe_buf_end >= b ); \
		SINTa head_pad = RR_MIN(f - safe_buf_start,FRONTBACK_OVERREAD_PAD_SPACE); \
		SINTa tail_pad = RR_MIN(safe_buf_end - b,FRONTBACK_OVERREAD_PAD_SPACE); \
		U8 * newf = safe_space+FRONTBACK_OVERREAD_PAD_SPACE; \
		memset(safe_space,0,sizeof(safe_space)); /* just for valgrind */ \
		memcpy(newf-head_pad,f-head_pad,fb_size+head_pad+tail_pad); \
		ptrf = newf; \
		ptrb = newf + fb_size; \
	} else { \
		REQUIRE_FUZZ_RETURN( b >= f-16 , -1); \
		REQUIRE_FUZZ_RETURN( b >= safe_space+FRONTBACK_HALF_BYTES_PER_LOOP , -1); \
		REQUIRE_FUZZ_RETURN( f <= safe_space + sizeof(safe_space) -FRONTBACK_HALF_BYTES_PER_LOOP , -1); \
	} } \
} while(0)
	
//=============================
//
// VARBITS_GET_PLUS_TOP :
//	get N bits
//	+ turn on the bit above N

// VARBITS_GET_PLUS_TOP : num >= 1
// VARBITS_GET_PLUS_TOP_0OK : num >= 0

#ifdef __RADPPC__

	// the rotate way was causing a bug with some compilers
	// deprecated platform, just cover it up by using the non-rotate way

	// straightforward way :
	//	Get num bits + flag the bit above that
	
	#define VARBITS_GET_PLUS_TOP(into,vb,num)		into = (1U<<(num)) | (U32) rrVarBits_Get_V(vb,num)
	#define VARBITS_GET_PLUS_TOP_0OK(into,vb,num)  into = (1U<<(num)) | (U32) rrVarBits_Get_0Ok(vb,num)
	
#else
			
	// no clear-for-fill in varbits Refill
	// need to mask out the bits I consume when using the rotate method
	
	#if defined(__RADARM__)

	// no need to avoid variable shifts on ARM, just do it the simple way :
	#define VARBITS_GET_PLUS_TOP(into,vb,num)		into = (1U<<(num)) | (U32) rrVarBits_Get_V(vb,num)
	#define VARBITS_GET_PLUS_TOP_0OK(into,vb,num)  into = (1U<<(num)) | (U32) rrVarBits_Get_0Ok(vb,num)
	
	#elif defined(__RADJAGUAR__) || defined(__RADZEN2__) // cheaper to compute the mask than load it

	// NOTE(fg): The mask computation here uses:
	// (1 << num) - 1 == ~((-1) << num)
	// and ((-1) << (num + 1)) == ((-1 << 1) << num)
	// this works out faster on the Jaguars
	#define VARBITS_GET_PLUS_TOP(into,vb,num) do { \
		NVB(vb,_bits) |= 1; \
		rrVarBits_RotL(vb,num); \
		NVB(vb,_inv_bitlen) += num; \
		RR_VARBITSTYPE mask = ((RR_VARBITSTYPE)-2 << (num)); \
		into = (U32) ( NVB(vb,_bits) & ~mask ); \
		NVB(vb,_bits) &= mask; \
		} while(0)

	// rotate way is okay with 0 :
	#define VARBITS_GET_PLUS_TOP_0OK	VARBITS_GET_PLUS_TOP
	
	#else

	// set bottom bit
	// then rotate around to get num bits + 1 above
	// then turn off consumed bits in bitbuf

	#define VARBITS_GET_PLUS_TOP(into,vb,num) do { \
		NVB(vb,_bits) |= 1; \
		rrVarBits_RotL(vb,num); \
		NVB(vb,_inv_bitlen) += num; \
		RR_VARBITSTYPE mask = c_rrBitMaskVB[(num)+1]; \
		into = (U32) ( NVB(vb,_bits) & mask ); \
		NVB(vb,_bits) &= ~mask; \
		} while(0)

	// rotate way is okay with 0 :
	#define VARBITS_GET_PLUS_TOP_0OK	VARBITS_GET_PLUS_TOP
	
	#endif // architecture

#endif
	
//=========================================

#define VARBITS_PUT_EXPGOLOMB(vb,val,rshift,output) do { \
    U32 head = ((val)>>(rshift)) +1; \
    U32 topbit = rrGetBitLevel_V_NonZero(head) - 1;\
    RR_ASSERT( head >= (1UL<<topbit) && head < (2UL<<topbit) );\
    rrVarBits_Put(vb,1,topbit+1); output(vb);\
    if ( topbit > 0 ) { head -= (1UL<<topbit); rrVarBits_Put(vb,head,topbit); output(vb); }\
    U32 tail = (val) & ((1UL<<(rshift))-1);\
    rrVarBits_Put(vb,tail,rshift); output(vb);\
    } while(0)
	    
//=========================================

// rrVarBits_CountLeadingZeros returns undefined if rrVarBits_Bits(vb) == 0
//	(only occurs with corruption)
// so instead, check the contents of the bit buffer first to see if they
// have too many leading zeros

#if RR_MINBITSAVAILABLE >= 48

#define VARBITS_GET_EXPGOLOMB(vb,rshift,out,refill,bias) do { \
	/* minval is the smallest integer with (18-rshift) leading zeros */ \
	RR_VARBITSTYPE minval = ((RR_VARBITSTYPE)1 << (RR_VARBITSTYPELEN - 1 - (18 - (rshift)))); \
	if ( rrVarBits_Bits(vb) < minval ) return -1; \
	RR_BITLENTYPE cntLZ = rrVarBits_CountLeadingZeros(vb); \
	RR_BITLENTYPE nBits = 2*cntLZ+1 + rshift; \
	RR_ASSERT( rrVarBits_BitLen(vb) >= nBits ); \
	RR_VARBITSTYPE val = rrVarBits_Get_V(vb,nBits); \
    out = val - ((RR_VARBITSTYPE)1<<rshift) + (bias); \
    } while(0)

#else

// eg. for 32-bit varbits ; only 24 available in state
// get LZcnt, then refill, then payload bits

#define VARBITS_GET_EXPGOLOMB(vb,rshift,out,refill,bias) do { \
	/* minval is the smallest integer with (18-rshift) leading zeros */ \
	RR_VARBITSTYPE minval = ((RR_VARBITSTYPE)1 << (RR_VARBITSTYPELEN - 1 - (18 - (rshift)))); \
	if ( rrVarBits_Bits(vb) < minval ) return -1; \
	RR_BITLENTYPE cntLZ = rrVarBits_CountLeadingZeros(vb); \
	RR_ASSERT( rrVarBits_BitLen(vb) >= (cntLZ+1) ); \
	rrVarBits_Use(vb,cntLZ); \
	refill(vb); \
	RR_BITLENTYPE nBits = cntLZ + rshift+1; \
	RR_ASSERT( rrVarBits_BitLen(vb) >= nBits ); \
	RR_VARBITSTYPE val = rrVarBits_Get_V(vb,nBits); \
    out = val - ((RR_VARBITSTYPE)1<<rshift) + (bias); \
    } while(0)

#endif    

//=========================================

OODLE_NS_END
