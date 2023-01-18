// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(parent,OodleAPI_Network)
//idoc(end)

#include "oodlecore.h"
#include "oodlenetworkversion.h"

#include "oodlestaticlzp.h"

#include "lzasserts.h"

#define UDP_VERIFY_CONSTNESS 0

#define RR_BINARY_MODEL_TOT_SHIFT   (11)
#define RR_BINARY_MODEL_UPD_SHIFT   (4)

#include "templates/rrstl.h"
#include "templates/rrvector.h"
#include "templates/rralgorithm.h"
//#include "templates/rrHashtable.h"
#include "rrarithcoder.h"
#include "rrarithcoder.inl"
#include "rrcompressutil.h"
#include "cbradutil.h"
#include "rrhashes.h"
#include "rrrand.h"
#include "rrvarbits.h"
#include "rrtans.h"
#define RRVBC_INLINE
#include "rrvarbitcodes.h"
#include "rrvarbitcodes.cpp"

#include "rrprefetch.h"
#include "rrstackarray.h"

#include "rrlogutil.h"

#include "rrsimpleprofstub.h"

#include "suffixtrie.h"
#include "suffixtriematcher.h"

#include "rrlz_getmatchlen.inl"
#include "rrbits.h"
#include "histogram.h"

#include <math.h>

#ifdef __RADSSE2__
#include <emmintrin.h>
#endif

#include "arith_o0.inl"
#include "arith_o1.inl"
#include "arith_nosbvaluecoder.inl"
#include "arith_o1symbolranker.inl"

#ifndef OODLE_BUILDING_NETWORK
#error OODLE_BUILDING_NETWORK should be set
#endif

#if defined(__RADNT__) || defined(__RADMAC__) || defined(__RADLINUX__)

#define OODLE_PLATFORM_HAS_SELECTDICTIONARYANDTRAIN	1

#else

#define OODLE_PLATFORM_HAS_SELECTDICTIONARYANDTRAIN	0

#endif // platforms

//#pragma message("OODLE_NS = " RR_STRINGIZE_DELAY(OODLE_NS) )

OODLE_NS_START

//===================================================================================

// no match pointers start within this distance of end :
//	(they can end in there)
#define LZP_DICTIONARY_END_PAD	256


S32 normalize_counts(int oodle_major_version,U32 * to, int to_sum_desired, const U32 * from, int from_sum, int alphabet)
{
	if ( oodle_major_version <= 5 )
		return normalize_counts_v5(to,to_sum_desired,from,from_sum,alphabet);
	else
		return normalize_counts_v6(to,to_sum_desired,from,from_sum,alphabet);
}

//===================================================================================

#if 0
static RADINLINE SINTr lzp_getmatchlen(
	const U8 * p1, const U8 * p1end,
	const U8 * p2, const U8 * p2end)
{
	// shitty slow implementation for now
	SINTr len = 1;
	while ( 
		p1+len < p1end &&
		p2+len < p2end &&
		p1[len] == p2[len] )
	{
		len++;
	}

	return len;
}
#else
static RADINLINE SINTr lzp_getmatchlen(
	const U8 * p1, const U8 * p1end,
	const U8 * p2, const U8 * p2end)
{
	// match pointer is not too close to the end :
	RR_ASSERT( (p2end - p2) >= LZP_DICTIONARY_END_PAD );
	
	if ( p1end > p1+16 )
	{
		// can do 16 with no end check :
	
		#ifdef __RAD64REGS__
	
		RR_COMPILER_ASSERT( sizeof(UINTr) == 8 );		
	
		UINTr big1 = *((UINTr *)(p1));
		UINTr big2 = *((UINTr *)(p2));
    
		if ( big1 != big2 )
		{
			int len = GetNumBytesZeroNeverAllR(big1^big2);  
			
			return len;    
		}
		
		big1 = *((UINTr *)(p1+8));
		big2 = *((UINTr *)(p2+8));
    
		if ( big1 != big2 )
		{
			int len = 8 + GetNumBytesZeroNeverAllR(big1^big2);  
			
			return len;    
		}
		
		SINTr len = 16;
			
		#else
		
		RR_COMPILER_ASSERT( sizeof(UINTr) == 4 );
		
		UINTr big1 = *((UINTr *)(p1));
		UINTr big2 = *((UINTr *)(p2));
    
		if ( big1 != big2 )
		{
			int len = GetNumBytesZeroNeverAllR(big1^big2);  
			
			return len;    
		}
		
		big1 = *((UINTr *)(p1+4));
		big2 = *((UINTr *)(p2+4));
    
		if ( big1 != big2 )
		{
			int len = 4 + GetNumBytesZeroNeverAllR(big1^big2);  
			
			return len;    
		}
		
		SINTr len = 8;
			
		#endif
		
		SINTa maxml = RR_MIN(p1end-p1,p2end-p2);
		while( len < maxml &&
			p1[len] == p2[len] )
		{
			len++;
		}
		return len;
	}
	else
	{
		SINTa maxml = RR_MIN(p1end-p1,p2end-p2);
		SINTr len = 1;
		while( len < maxml &&
			p1[len] == p2[len] )
		{
			len++;
		}
		return len;
	}	
}
#endif

static RADFORCEINLINE void lzp_copymatch(U8 * to, const U8 * from, SINTr len)
{
	RR_ASSERT( len >= 1 );

	to[0] = from[0];
	if ( len == 1 ) return;
	to[1] = from[1];
	if ( len == 2 ) return;
	to[2] = from[2];
	if ( len == 3 ) return;
	to[3] = from[3];
	
	// this is meh , none of the time is in copymatch
	
	// len >= 4
	len -= 4; to += 4; from += 4;
	RR_ASSERT( len >= 0 );
	
	for(SINTr i=0;i<(len/4);i++)
	{
		RR_PUT32_NATIVE( to, RR_GET32_NATIVE( from ) );
		to += 4;
		from += 4;
	}
	
	for(SINTr i=0;i<(len%4);i++)
	{
		*to++ = *from++;
	}
}

static RADFORCEINLINE void lzp_copymatch_basic(U8 * to, const U8 * from, SINTr len)
{
	for(SINTr i=0;i<len;i++)
	{
		to[i] = from[i];
	}
}
					
//===================================================================================

// there is no min size necessary, but tiny packets tend to expand often
//	there's also a lot of time wasted per byte on them
// (this is all a bit silly due to IP min packet size)
//#define OODLENETWORK1_MIN_RAWLEN  0	// 84.4
//#define OODLENETWORK1_MIN_RAWLEN	4	// 84.4
#define OODLENETWORK1_MIN_RAWLEN	6
//#define OODLENETWORK1_MIN_RAWLEN	8	// 84.4
//#define OODLENETWORK1_MIN_RAWLEN	32  // 84.5

// make sure if we're sending matchlens we can send something nontrivial; nope not helpful
#define OODLENETWORK1_MATCHEND_BACKUP	1

#define OODLENETWORK1_ML_NUMUNARY	3	// 84104 bytes	 138.9 average
//#define OODLENETWORK1_ML_NUMUNARY	2	// 83080 bytes   139.0 average
//#define OODLENETWORK1_ML_NUMUNARY	1	// 82056 bytes   139.2 average

//literal-after-match ; helps a little (this is only in TCP mode)
#define OODLENETWORK1_LAM		1	// 86.8
//#define OODLENETWORK1_LAM		0	// 87.1

//#define ON1_HASHENTRY_MP0 0
#define ON1_HASHENTRY_MP0 1	// yes helps speed

struct ON1_HashEntry
{
	/*
	U32	index;
	#if ON1_HASHENTRY_MP0
	U32 context : 24;
	U32 mp0 : 8;	
	#else
	U32 context;
	#endif
	*/
	
	
	#if ON1_HASHENTRY_MP0
	// 24 bit dictionary index :
	RR_COMPILER_ASSERT( OODLENETWORK1_MAX_DICTIONARY_SIZE <= (1<<24) );
	U32	index : 24; // OODLENETWORK1_MAX_DICTIONARY_SIZE
	U32 mp0 : 8;	
	U32 context;
	#else
	U32	index;
	U32 context;
	#endif
	
	
	// also could store first 4 bytes of pointer in the hash
	// would reduce cache misses in some cases
	// or even just the single first char, packed in with index or context
	// could store 8 bytes
	// then any match of len <= 8 doesn't need another cache fetch
};

RR_COMPILER_ASSERT( sizeof(ON1_HashEntry) == 8 );

/*

static RADINLINE U32 ContextBack3(const U8 * ptr)
{
	// overrun always okay because I'm encoding *ptr
    return RR_GET24_LE_OVERRUNOK(ptr-3);
}

static RADINLINE U32 ContextSlide3(U32 ctx,U32 o1)
{
	return (ctx>>8) | (o1<<16);
}

/**/

//*

// OodleNetwork1UDP encode : 1.771 seconds, 8.68 b/kc, rate= 13.82 mb/s
// 429.3 -> 167.6 average = 2.562:1 = 60.96% reduction

static RADINLINE U32 ContextBack3(const U8 * ptr)
{
	// overrun always okay because I'm encoding *ptr
    //return RR_GET24_BE_OVERRUNOK(ptr-3) << 8;
    return RR_GET32_BE_UNALIGNED(ptr-3) & 0xFFFFFF00;
    //return RR_GET32_BE_UNALIGNED(ptr-4) << 8;
}

static RADINLINE U32 ContextSlide3(U32 ctx,U32 o1)
{
	RR_ASSERT( (ctx & 0xFF) == 0 );
	return (ctx | o1) << 8;
}

/**/

/*

//OodleNetwork1UDP encode : 1.876 seconds, 0.00 b/kc, rate= 13.04 mb/s
//429.3 -> 167.2 average = 2.568:1 = 61.06% reduction

static RADINLINE U32 ContextBack3(const U8 * ptr)
{
	// overrun always okay because I'm encoding *ptr
    return RR_GET24_BE_OVERRUNOK(ptr-3);
}

static RADINLINE U32 ContextSlide3(U32 ctx,U32 o1)
{
	ctx = ctx & 0xFFFF;
	return (ctx<<8) | o1;
}

/**/

//19 :
//r:\packet_test_small.bin :66,187,074 ->15,242,491 =  1.842 bpb =  4.342 to 1 
//178376 packets; 371.1 -> 85.5 average
//20 :
//r:\packet_test_small.bin :66,187,074 ->13,916,756 =  1.682 bpb =  4.756 to 1 
//178376 packets; 371.1 -> 78.0 average

static RADINLINE U32 OodleNetwork1_HashBack3(U32 ctx)
{
	/*
    U32 h = ctx * 2654435761u;
    h ^= (h>>13); // 13,334,084
    return h;
    */
    //return ctx >> 5; // for 19
    //ctx = RR_BSWAP32(ctx);
    U32 h = ctx * 2654435761u;
    return RR_BSWAP32(h);
}


//cmp2
// 13,254,487 bits=10
// 13,075,989 bits=11 ; 2.4M per channel
// 12,924,608 bits=12

//#define LZP_MC_BITS	10	// 88.9 with lazy , 88.8 without
//#define LZP_MC_BITS	11 // 87.2 with lazy ; SR-8 = 80.1
#define LZP_MC_BITS	12	// SR-4 = 79.8
//#define LZP_MC_BITS	13	// SR-2 = 80.5
#define LZP_MC_SIZE	(1<<LZP_MC_BITS)
#define LZP_MC_MASK	(LZP_MC_SIZE-1)

//#define O1_TOP_BITS(o1,num)	( (o1) >> ( 8 - (num) ) )

#define LZP_MLC_BITS	9	// 139.1  84104 bytes per channel
//#define LZP_MLC_BITS	10	// 138.9  103560 bytes per channel
#define LZP_MLC_SIZE	(1<<LZP_MLC_BITS)
#define LZP_MLC_MASK	(LZP_MLC_SIZE-1)

// toggle here :
//*

// OodleNetwork1 encode : 0.728 seconds, 19.46 b/kc, rate= 33.61 mb/s
// 429.3 -> 173.6 average = 2.474:1 = 59.57% reduction

static RADINLINE int OodleNetwork1_MakeMatchContext( int o1, int ex )
{
	int both = (((int)o1)<<8) | ex;
    U32 h = both * 2654435761u;
   // h ^= (h>>13);
    h = RR_BSWAP32(h);
    return h & LZP_MC_MASK;
}

static RADINLINE int OodleNetwork1_MakeMLC( int mc )
{
    return mc & LZP_MLC_MASK;
}

/*/

// use the LitMatchContext for MatchContext
// same hash computation = better speed
// but does hurt compression a little (in one case, not in another)

// 429.3 -> 174.0 average = 2.467:1 = 59.47% reduction
// OodleNetwork1 encode : 0.683 seconds, 20.72 b/kc, rate= 35.81 mb/s

#define OodleNetwork1_MakeMatchContext	OodleNetwork1_MakeLitMatchContext

static RADINLINE int OodleNetwork1_MakeMLC( int mc )
{
    //U32 h = mc * 2654435761u;
    //h ^= (h>>13);
    //U32 h = mc ^ (mc>>5);
    //return h & LZP_MLC_MASK;
    //return mc & LZP_MLC_MASK;
    // totally doesn't seem to matter at all
    // this way gives you o1 and the top bits of ex, so do this :
    return (mc >> (LZP_MC_BITS - LZP_MLC_BITS)) & LZP_MLC_MASK;
	//return 0;
}

/**/

static RADINLINE int OodleNetwork1_MakeLitMatchContext( int o1, int ex )
{
	/*
	// fast and good :
	// 429.3 -> 174.5 average
	return OodleNetwork1_MakeMatchContext(o1,ex);
	/**/
	/*
	// 429.3 -> 175.8 average = 2.442:1 = 59.05% reduction
	int h = (O1_TOP_BITS(o1,(LZP_MC_BITS-8))<<8) | ex;
    return h;
    /**/
    /*
    // 429.3 -> 174.9 average = 2.454:1 = 59.26% reduction
    U32 h = o1 * 2654435761u;
    h ^= (h>>13);
    h &= (1<<(LZP_MC_BITS-8))-1;
    return (h<<8) | ex;
    */
    /*
    // 429.3 -> 174.9 average = 2.454:1 = 59.26% reduction
    U32 h = o1 * 2654435761u;
    h ^= (h>>13);
    h &= (1<<(LZP_MC_BITS-8))-1;
    return (h<<8) | ex;
    */
    // 429.3 -> 175.0 average = 2.454:1 = 59.24% reduction 
    //U32 h = o1 * 41;
    //U32 h = o1 * 21;
    //*
    // bottom 8 bits must be the exclude
    // -> best compression
    // 429.3 -> 174.3 average = 2.463:1 = 59.41% reduction
    U32 h = o1 + (o1>>3);
    h = (h<<8) | ex;
    return h & LZP_MC_MASK;
    /**/
}


struct OodleNetwork1_Shared
{
	const U8 * m_lzp_window;
	const U8 * m_lzp_window_end;
	U32 m_lzp_hash_mask;
	U32 m_pad_for_alignment;
	ON1_HashEntry m_lzp_hash[1];
};

// O1BinaryArithCoder was 80.8 - 
//	O1SRCoder with 8 symbols - 81.0
//	yes success!

//===============================================

	//-----------------------------------
	
	//#define ON1_Hash2(h)	( (h) + ((h)>>28) + 1 )
	#define ON1_Hash2(h)	( (h) + 1 ) // faster, small compression penalty
	
	static RADFORCEINLINE void ON1_InsertHash(U32 cb3,
		S32 index,
		ON1_HashEntry * lzp_hash,
		const U32 lzp_hash_mask,
		const U8 * ptr)
	{
		U32 h = OodleNetwork1_HashBack3(cb3);
		U32 h1 = h & lzp_hash_mask;
		U32 h2 = ON1_Hash2(h) & lzp_hash_mask;
		
		#if ON1_HASHENTRY_MP0
		U32 mp0 = *ptr;
		#endif
		
		/*
		
		// always overwrite
		// hash winds up with the *last* occurance of each hash
		
		U32 hi;
		if ( lzp_hash[h1].index == 0 ) hi = h1;
		else if ( lzp_hash[h2].index == 0 ) hi = h2;
		else if ( lzp_hash[h1].context == cb3 ) hi = h1;
		else if ( lzp_hash[h2].context == cb3 ) hi = h2;
		else hi = ( rrRandBool() ) ? h1 : h2;
		
		lzp_hash[hi].index = index;
		lzp_hash[hi].context = cb3;
		/*/
		
		// never overwrite
		// hash winds up with the *first* occurance of each hash
		
		if ( lzp_hash[h1].index == 0 )
		{
			lzp_hash[h1].index = index;
			lzp_hash[h1].context = cb3;
			#if ON1_HASHENTRY_MP0
			lzp_hash[h1].mp0 = mp0;
			#endif
		}
		else if ( lzp_hash[h2].index == 0 )
		{
			lzp_hash[h2].index = index;
			lzp_hash[h2].context = cb3;
			#if ON1_HASHENTRY_MP0
			lzp_hash[h2].mp0 = mp0;
			#endif
		}
		// else no insert !
		/**/
	}
	
	static RADFORCEINLINE const U8 * ON1_LookupHash(U32 cb3,
		const ON1_HashEntry * lzp_hash,
		const U32 lzp_hash_mask,
		const U8 * lzp_window)
	{
		U32 h = OodleNetwork1_HashBack3(cb3);
		U32 h1 = h & lzp_hash_mask;
		U32 h2 = ON1_Hash2(h) & lzp_hash_mask;
		
		if ( lzp_hash[h1].context == cb3 &&
			lzp_hash[h1].index != 0 )
		{
			return lzp_window + lzp_hash[h1].index;				
		}
		else if ( lzp_hash[h2].context == cb3 &&
			lzp_hash[h2].index != 0 )
		{
			return lzp_window + lzp_hash[h2].index;				
		}
		else
		{		
			return NULL;
		}
	}

	//*
	
	// 429.3 -> 168.5
	
	static RADFORCEINLINE const ON1_HashEntry * ON1UDP_LookupHash(U32 cb3,
		const ON1_HashEntry * lzp_hash,
		const U32 lzp_hash_mask)
	{
		U32 h = OodleNetwork1_HashBack3(cb3);
		U32 h1 = h & lzp_hash_mask;
		U32 h2 = ON1_Hash2(h) & lzp_hash_mask;
		
		if ( lzp_hash[h1].context == cb3 )
		{
			return lzp_hash+h1;
		}
		else if ( lzp_hash[h2].context == cb3 )
		{
			return lzp_hash+h2;
		}
		else
		{		
			return NULL;
		}
	}
	
	/**/
	
	/*
	
	// 429.3 -> 179.0 average ; way worse
	
	static RADFORCEINLINE const ON1_HashEntry * ON1UDP_LookupHash(U32 cb3,
		const ON1_HashEntry * lzp_hash,
		const U32 lzp_hash_mask)
	{
		U32 h = OodleNetwork1_HashBack3(cb3);
		U32 h1 = h & lzp_hash_mask;
		return lzp_hash+h1;
	}
	
	/**/

	/*

	// 429.3 -> 170.5 average = 2.518:1 = 60.28% reduction

	static RADFORCEINLINE const ON1_HashEntry * ON1UDP_LookupHash(U32 cb3,
		const ON1_HashEntry * lzp_hash,
		const U32 lzp_hash_mask)
	{
		U32 h = OodleNetwork1_HashBack3(cb3);
		U32 h1 = h & lzp_hash_mask;
		U32 h2 = ON1_Hash2(h) & lzp_hash_mask;
		
		if ( lzp_hash[h1].context == cb3 )
		{
			return lzp_hash+h1;
		}
		else if ( lzp_hash[h2].context == cb3 )
		{
			return lzp_hash+h2;
		}
		else
		{		
			return lzp_hash;
		}
	}
	
	/**/
	
//===============================================

struct OodleNetwork1TCP_State
{
	O1SRCoder<8,8,256,U8> m_o1ac_lit_nomatch;
	O1SRCoder4U8<LZP_MC_BITS> m_o1ac_lit_match;
	O1BinaryArithCoder<LZP_MC_BITS,1> m_o1ac_mf;
	O1ACNOSBValueCoder<LZP_MLC_BITS,OODLENETWORK1_ML_NUMUNARY> m_o1ac_ml_vc;
	
	void Reset()
	{		
		m_o1ac_lit_nomatch.init();
		m_o1ac_lit_match.init();
		m_o1ac_mf.init();		
		m_o1ac_ml_vc.init();
	}
	
	//-----------------------------------
	
	SINTa Encode(OodleNetwork1_Shared const * shared,const U8 * raw, SINTa rawlen, U8 * comp)
	{
		SIMPLEPROFILE_SCOPE_N(Encode,rawlen);
	
		//RR_ASSERT_ALWAYS( comp_buf_size >= rawlen + 8 );
	
		if ( rawlen <= OODLENETWORK1_MIN_RAWLEN )
		{
			memcpy(comp,raw,rawlen);
			return rawlen;
		}
		
		rrArithEncoder ac;
		rrArithEncodeInit(&ac,comp);
		
		U8 * ac_expanded_ptr = comp + rawlen;
		bool expanded = false;
		
		const ON1_HashEntry * lzp_hash = shared->m_lzp_hash;
		const U8 * lzp_window = shared->m_lzp_window;
		const U8 * lzp_window_end = shared->m_lzp_window_end;
		const U32 lzp_hash_mask = shared->m_lzp_hash_mask;
		
		// encode first 3 :
		m_o1ac_lit_nomatch.encode(&ac,raw[0],0);
		m_o1ac_lit_nomatch.encode(&ac,raw[1],raw[0]);
		m_o1ac_lit_nomatch.encode(&ac,raw[2],raw[1]);
		
		const U8 * ptr = raw+3;
		const U8 * rawend = raw+rawlen;
		const U8 * matchend = rawend - OODLENETWORK1_MATCHEND_BACKUP;
		
		U32 cb3;
		int o1;
		
		if ( ptr >= matchend )
			goto reached_matchend;	
				
		cb3 = ContextBack3(ptr);		
		o1 = ptr[-1];
			
		for(;;)
		{
			const U8 * mp;
			
			if ( ac.ptr >= ac_expanded_ptr )
			{
				// expanded !
				// adaptive; must finish coding
				expanded = true;
				// put the pointer back to the start to avoid overrun
				// be careful to avoid propagating carry!
				*comp = 0;
				ac.ptr = comp+1;
			}
			
			RR_ASSERT( cb3 == ContextBack3(ptr) );
			mp = ON1_LookupHash(cb3,lzp_hash,lzp_hash_mask,lzp_window);
		
			int cur = ptr[0];
			RR_ASSERT( o1 == ptr[-1] );
				
			if ( mp == NULL ) // no match pointer
			{
				m_o1ac_lit_nomatch.encode(&ac,cur,o1);
				ptr++;
				
				if ( ptr >= matchend )
					goto reached_matchend;	
				
				o1 = cur;	
				cb3 = ContextSlide3(cb3,cur);
				continue;
			}
			
			int mp0 = mp[0];
			int mc = OodleNetwork1_MakeMatchContext(o1,mp0);
			
			if ( cur != mp0 ) // no match
			{
				m_o1ac_mf.encode(&ac,0,mc);
				int lmc = OodleNetwork1_MakeLitMatchContext(o1,mp0);
				m_o1ac_lit_match.encode_withexclude(&ac,cur,lmc,mp0);
				
				ptr++;
			
				if ( ptr >= matchend )
					goto reached_matchend;	
				
				o1 = cur;	
				cb3 = ContextSlide3(cb3,cur);
				continue;
			}
			
			 // match
			
			{
				SINTr len;
				
				// profiler scope
				{
				RR_ASSERT( cur == mp0 );
				len = lzp_getmatchlen(ptr,rawend,mp,lzp_window_end);

				#if defined(OODLE_BUILD_CONFIG_SIMPLEPROFILER) && OODLE_BUILD_CONFIG_SIMPLEPROFILER
				// fix profiler count cuz we didnt know it at scope start
				_profile_scoper.m_count = (int) len;
				#endif

				RR_ASSERT( ptr+len <= rawend );
									
				RR_ASSERT( ptr+len <= rawend );
				RR_ASSERT( memcmp(ptr,mp,(size_t)len) == 0 );
				RR_ASSERT( ptr[len] != mp[len] || ptr+len == rawend || mp+len == lzp_window_end );
				RR_ASSERT( len >= 1 );
				
				m_o1ac_mf.encode(&ac,1,mc);
				
				//int mlc = OodleNetwork1_MakeMLC(o1,mp0);
				int mlc = OodleNetwork1_MakeMLC(mc);
				m_o1ac_ml_vc.encode(&ac,(int)len-1,mlc);
				
				ptr += len;
				}
				
				#if OODLENETWORK1_LAM
				if ( ptr >= rawend )
					goto reached_matchend;
				
				const U8 * mpend = mp+len;
				if ( mpend < lzp_window_end )
				{
					U8 after_match = mpend[0];
					RR_ASSERT( ptr[0] != after_match );
												
					int lmc = OodleNetwork1_MakeLitMatchContext(ptr[-1],after_match);
					m_o1ac_lit_match.encode_withexclude(&ac,ptr[0],lmc,after_match);

					ptr++;					
				}
				#endif
			}
			
			if ( ptr >= matchend )
				goto reached_matchend;	
		
			// reload :
			cb3 = ContextBack3(ptr);		
			o1 = ptr[-1];
		}
		
		reached_matchend:
		
		if ( ! expanded )
		{
			while( ptr < rawend )
			{
				m_o1ac_lit_nomatch.encode(&ac,ptr[0],ptr[-1]);
				ptr++;
			}

			RR_ASSERT( ptr == rawend );
		}
		
		SINTa compLen = rrArithEncodeFlush(&ac);
		
		if ( expanded || compLen >= rawlen )
		{
			memcpy(comp,raw,rawlen);
			return rawlen;
		}
		else
		{		
			return compLen;
		}
	}
	
	rrbool Decode(OodleNetwork1_Shared const * shared,const U8 * comp, SINTa compLen, U8 * raw, SINTa rawlen)
	{
		if ( rawlen <= OODLENETWORK1_MIN_RAWLEN )
		{
			memcpy(raw,comp,rawlen);
			return true;
		}
		if ( compLen >= rawlen )
		{
			if ( compLen > rawlen )
				return false;
				
			// expanded packet
			
			memcpy(raw,comp,rawlen);

			{
				// have to encode the data to get the same statistics adaptation that encoder got
				// BEWARE OVERRUN !
				// we're in this case because the encoder expanded
				// @@@@ this is ugly as hell and can be a large stack usage in theory
				//	 in practice, this is the *TCP* case which nobody uses, so meh
				SINTa shit_size = rawlen + 4096;
				RR_STACK_ARRAY(shit,U8,shit_size);
				shit[shit_size-1] = 0xEC;
				Encode(shared,raw,rawlen,shit); //,shit_size);
				RR_ASSERT( shit[shit_size-1] == 0xEC );
			}
			
			return true;
		}
		
		RR_ASSERT( compLen < rawlen );
	
		rrArithDecoder ac;
		rrArithDecodeInit(&ac,comp);
		
		const ON1_HashEntry * lzp_hash = shared->m_lzp_hash;
		const U8 * lzp_window = shared->m_lzp_window;
		const U8 * lzp_window_end = shared->m_lzp_window_end;
		const U32 lzp_hash_mask = shared->m_lzp_hash_mask;
		
		//RR_UNUSED_VARIABLE(lzp_window_end);
		
		// decode first 3 :
		raw[0] = check_value_cast<U8>( m_o1ac_lit_nomatch.decode(&ac,0) );
		raw[1] = check_value_cast<U8>( m_o1ac_lit_nomatch.decode(&ac,raw[0]) );
		raw[2] = check_value_cast<U8>( m_o1ac_lit_nomatch.decode(&ac,raw[1]) );
		
		U8 * ptr = raw+3;
		U8 * rawend = raw+rawlen;
		const U8 * matchend = rawend - OODLENETWORK1_MATCHEND_BACKUP;
		while( ptr < matchend )
		{
			U32 cb3 = ContextBack3(ptr);
			const U8 * mp = ON1_LookupHash(cb3,lzp_hash,lzp_hash_mask,lzp_window);
			
			if ( mp == NULL )
			{
				int dec = m_o1ac_lit_nomatch.decode(&ac,ptr[-1]);
				*ptr++ = check_value_cast<U8>( dec );
			}
			else
			{
				int mc = OodleNetwork1_MakeMatchContext(ptr[-1],mp[0]);
				int mf = m_o1ac_mf.decode(&ac,mc);
				if ( mf )
				{
					//int mlc = OodleNetwork1_MakeMLC(ptr[-1],mp[0]);
					int mlc = OodleNetwork1_MakeMLC(mc);
					SINTa len = 1 + m_o1ac_ml_vc.decode(&ac,mlc);

					RR_ASSERT( len >= 1 );
									
					RR_ASSERT( ptr+len <= rawend &&
						mp+len <= lzp_window_end );

					lzp_copymatch(ptr,mp,len);
					ptr += len;
					
					#if OODLENETWORK1_LAM
					if ( ptr < rawend && mp+len < lzp_window_end )
					{
						U8 after_match = mp[len];
						
						int lmc = OodleNetwork1_MakeLitMatchContext(ptr[-1],after_match);
						int dec = m_o1ac_lit_match.decode_withexclude(&ac,lmc,after_match);
						
						*ptr++ = check_value_cast<U8>( dec );
					}
					#endif
				}
				else // no match
				{		
					//int dec = m_o1ac_lit_match.decode(&ac,mc);
					int lmc = OodleNetwork1_MakeLitMatchContext(ptr[-1],mp[0]);
					int dec = m_o1ac_lit_match.decode_withexclude(&ac,lmc,mp[0]);
					
					*ptr++ = check_value_cast<U8>( dec );
				}
			}
		}
		
		while( ptr < rawend )
		{
			ptr[0] = check_value_cast<U8>( m_o1ac_lit_nomatch.decode(&ac,ptr[-1]) );
			ptr++;
		}
		
		RR_ASSERT( ptr == rawend );
		
		//return rrArithDecode(&ac);
		return true;
	}
};

//======================================================================================

OOFUNC1 void OOFUNC2 OodleNetwork1TCP_State_Reset(
		OodleNetwork1TCP_State * state)
{
	state->Reset();
}		
		
OOFUNC1 void OOFUNC2 OodleNetwork1TCP_State_InitAsCopy(
		OodleNetwork1TCP_State * state, const OodleNetwork1TCP_State * from )
{
	*state = *from;
	//memcpy(state,from,sizeof(OodleNetwork1TCP_State));
}
		
OOFUNC1 SINTa OOFUNC2 OodleNetwork1_Shared_Size(S32 htbits)
{
	return (SINTa) ( sizeof(OodleNetwork1_Shared) + ((size_t)1<<htbits)*sizeof(ON1_HashEntry) );
}

OOFUNC1 SINTa OOFUNC2 OodleNetwork1TCP_State_Size()
{
	return (SINTa) sizeof(OodleNetwork1TCP_State);
}

OOFUNC1 void OOFUNC2 OodleNetwork1_Shared_SetWindow( OodleNetwork1_Shared * data,
	S32 htbits,
	const void * windowv, S32 window_size )
{
	OOFUNCSTART
	
	if ( window_size > OODLENETWORK1_MAX_DICTIONARY_SIZE )
	{
		ooLogError("window_size larger than OODLENETWORK1_MAX_DICTIONARY_SIZE ; clamping!\n");
		window_size = OODLENETWORK1_MAX_DICTIONARY_SIZE;
	}
	
	const U8 * window = U8_void(windowv);
	data->m_lzp_window = window;
	data->m_lzp_window_end = window+window_size;
	data->m_lzp_hash_mask = (1<<htbits)-1;
	const U32 lzp_hash_mask = data->m_lzp_hash_mask;
	
	ON1_HashEntry * hashtable = data->m_lzp_hash;
	
	rrMemSetZero(hashtable,((size_t)1<<htbits)*sizeof(ON1_HashEntry));
	
	// don't get too close to the end, cuz no match len is possible there
	for(int i=4;i<window_size-LZP_DICTIONARY_END_PAD;i++)
	{
		const U8 * ptr = window+i;
		U32 c = ContextBack3(ptr);
		ON1_InsertHash(c,i,hashtable,lzp_hash_mask,ptr);
	}
}

OOFUNC1 void OOFUNC2 OodleNetwork1TCP_Train(
		OodleNetwork1TCP_State * state,
		const OodleNetwork1_Shared * shared,
		const void ** training_packet_pointers, 
		const S32 * training_packet_sizes,
		S32 num_training_packets)
{
	OOFUNCSTART

	// train State directly

	// LZP [4|18] : 429.3 -> 164.9 average = 2.604:1 = 61.60% reduction
	// 57002 packets; 24471721 -> 9398340 total bytes = 38.40% of original

	state->Reset();
				
	vector<U8> compv; compv.resize(65536);
	
	for LOOP(p,num_training_packets)
	{
		const void * cur_ptr = training_packet_pointers[p];
		SINTa cur_size = training_packet_sizes[p];
	
		if ( cur_size+4096 >= (SINTa)compv.size() )
		{
			compv.resize( cur_size + 4096 );
		}
	
		state->Encode(shared,U8_void(cur_ptr),cur_size,compv.data());
	}
}

OOFUNC1 SINTa OOFUNC2 OodleNetwork1TCP_Encode(
		OodleNetwork1TCP_State * state,
		const OodleNetwork1_Shared * shared,
		const void * raw, SINTa rawLen,
		void * comp )
{
	OOFUNCSTART
	return state->Encode(shared,U8_void(raw),rawLen,U8_void(comp));
}		

OOFUNC1 rrbool OOFUNC2 OodleNetwork1TCP_Decode(
		OodleNetwork1TCP_State * state,
		const OodleNetwork1_Shared * shared,
		const void * comp,  SINTa compLen,
		void * raw, SINTa rawLen )
{
	OOFUNCSTART
	return state->Decode(shared,U8_void(comp),compLen,U8_void(raw),rawLen);
}		

//-----------------------------------------------------

#define ON1_COMPBUF_OVERRUN_ALLOWED	8

OOFUNC1 SINTa OOFUNC2 OodleNetwork1_CompressedBufferSizeNeeded(SINTa rawLen)
{
	return rawLen + ON1_COMPBUF_OVERRUN_ALLOWED;
}

//===================================================================

static SINTa OodleNetwork1UDP_MeasureTotalCompressedSize(
		const OodleNetwork1UDP_State * state,
		const OodleNetwork1_Shared * shared,
		const void ** training_packet_pointers, 
		const S32 * training_packet_sizes,
		S32 num_training_packets);
		
//===================================================================

#if OODLE_PLATFORM_HAS_SELECTDICTIONARYANDTRAIN

#if ! OODLE_PLATFORM_HAS_ADVANCED_MATCHERS
#error OODLE_PLATFORM_HAS_SELECTDICTIONARYANDTRAIN but not OODLE_PLATFORM_HAS_ADVANCED_MATCHERS
#endif

// OODLE_PLATFORM_HAS_ADVANCED_MATCHERS == OODLE_PLATFORM_LARGEMEMORY
//	OODLE_PLATFORM_LARGEMEMORY is host PC's + also large memory consoles
//	OODLE_PLATFORM_HAS_SELECTDICTIONARYANDTRAIN is slightly more restrictive to just host PC's

struct rated_packet
{
	double	rating;
	double	base_rating;
	const void *	packet_ptr;
	S32		packet_size;
	
	// backward compare ; highest first -
	bool operator < (const rated_packet & rhs) const { return rating > rhs.rating; }
};


static void FillDictionary(void * dictionary_to_fill,S32 dictionary_size,
	const vector<rated_packet> & rated_packets)
{
	U8 * dictionary_to_fill_ptr = (U8 *) dictionary_to_fill;
	U8 * dictionary_end_ptr = dictionary_to_fill_ptr + dictionary_size;
	for LOOPVEC(rp,rated_packets)
	{
		const void * ptr = rated_packets[rp].packet_ptr;
		S32 size = rated_packets[rp].packet_size;
		
		S32 size_left = rrPtrDiff32( dictionary_end_ptr - dictionary_to_fill_ptr);
		size = RR_MIN(size,size_left);
		
		#if 0
		// put best packet at end
		// for ON1_InsertHash with always-replace
		
		U8 * to = dictionary_end_ptr - size;
		memcpy(to,ptr,size);
		
		dictionary_end_ptr = to;
		if ( dictionary_end_ptr <= dictionary_to_fill_ptr )
			break;
		#else
		// put best packet at head
		// for ON1_InsertHash with never-replace
		
		memcpy(dictionary_to_fill_ptr,ptr,size);
		dictionary_to_fill_ptr += size;
		if ( dictionary_to_fill_ptr >= dictionary_end_ptr )
			break;
		#endif
	}
	
	if ( dictionary_end_ptr > dictionary_to_fill_ptr )
	{
		rrPrintf_v0("WARNING : OodleNetwork_SelectDictionaryFromPackets - not enough packets to fill dictionary!\n");
		//memset(dictionary_to_fill,0,rrPtrDiff(dictionary_end_ptr - dictionary_to_fill_ptr));
		memset(dictionary_to_fill_ptr,0,rrPtrDiff(dictionary_end_ptr - dictionary_to_fill_ptr));
	}
}
	
OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1_SelectDictionaryFromPackets_Trials(
		void * dictionary_to_fill,
		S32 dictionary_size,
		S32 htbits,
		const void ** dictionary_packet_pointers, 
		const S32 * dictionary_packet_sizes,
		S32 num_dictionary_packets,
		const void ** test_packet_pointers, 
		const S32 * test_packet_sizes,
		S32 num_test_packets,
		S32 num_trials,
		double randomness,
		S32 num_generations
		)
{
	// @@@@ CAN SHIP
//	OodleNetwork2_Test(dictionary_packet_pointers,dictionary_packet_sizes,num_dictionary_packets,
//		test_packet_pointers,test_packet_sizes,num_test_packets);
		

	if ( dictionary_size > OODLENETWORK1_MAX_DICTIONARY_SIZE )
	{
		ooLogError("dictionary_size larger than OODLENETWORK1_MAX_DICTIONARY_SIZE ; clamping!\n");
		dictionary_size = OODLENETWORK1_MAX_DICTIONARY_SIZE;
	}

	num_trials = RR_CLAMP(num_trials,0,100);
	num_generations = RR_CLAMP(num_generations,0,100);

	//=============================================================
	#if 0
	typedef hash_table_ops_mask31hash<U32>	hash4_ops;
	typedef hashtableentry_hkd<U32,U32,hash4_ops> hash4_entry;
	hashtable< hash4_entry > hash4;

	typedef hash_table_ops_mask31hash<U64>	hash8_ops;
	typedef hashtableentry_hkd<U64,U32,hash8_ops> hash8_entry;
	hashtable< hash8_entry > hash8;

	// add test packets to hash4 :
	
	rrprintf("SDFP : building hash...\n");
	
	hash4.reserve_initial_size((1<<24));
	hash8.reserve_initial_size((1<<24));
	
	for LOOP(p,num_test_packets)
	{
		S32 cur_size = test_packet_sizes[p];
		const U8 * packet_ptr = U8_void( test_packet_pointers[p] );
		
		for(int i=0;i<cur_size-4;i++)
		{
			U32 v4 = RR_GET32_NATIVE(packet_ptr+i);
			U32 h4 = hash4_ops::hash_key(v4);
			const hash4_entry * entry4 = hash4.find_or_insert(h4,v4,0);
			const_cast<hash4_entry *>(entry4)->m_data += 1;
		}
		
		for(int i=0;i<cur_size-8;i++)
		{
			//U64 v8 = RR_GET64_NATIVE(packet_ptr+i);
			U64 v8 = RR_GET32_NATIVE(packet_ptr+i) | ((U64)packet_ptr[i+4]<<32);
			//v8 |= ((U64)packet_ptr[i+5]<<40);
			U32 h8 = hash8_ops::hash_key(v8);
			const hash8_entry * entry8 = hash8.find_or_insert(h8,v8,0);
			const_cast<hash8_entry *>(entry8)->m_data += 1;
		}		
	}
	
	#endif
	//=============================================================
	#if 1
	
	// suffix trie is used for *test* packets
	//	(not train or dictionary building)
	
	// combine all test packets :	
	// put a random u32 between packets
	//	to prevent matching across packets
	// @@ better : find the shortest N-byte sequence that does not occur in the source packets
	//	  will usually find a 2 or 3 byte break token rather than 4 bytes
	//	  which can save a lot of work in the suffix trie
	
	rrRandState rand_state = RR_RANDSTATE_INIT_VALUE;
	
	S64 test_packet_total_size64 = 0;
	S32 largest_packet_size = 0;
	for LOOP(p,num_test_packets)
	{
		S32 cur_size = test_packet_sizes[p];
		largest_packet_size = RR_MAX(cur_size,largest_packet_size);
		test_packet_total_size64 += cur_size;
	}
	
	S64 test_block_size64 = test_packet_total_size64 + num_test_packets * 4;
	
	rrprintf("SelectDictionaryFromPackets; test_packet_total_size = " RR_S64_FMT "\n",test_packet_total_size64);
	
	if ( largest_packet_size > 128*1024 )
	{
		rrprintf("WARNING: largest_packet_size = %d\n",largest_packet_size);
		rrprintf("consider excluding huge packets and compress them with OodleLZ\n");
	}
	
	// note : this doesn't happen if using "example_packet"
	//	because it has its own randomized limit to 64 MB :
	// 64M limit for dictest to keep speed reasonable
	//while ( dictest_tot_raw_len > 64*1024*1024 )

	if ( test_block_size64 > SuffixTrie2_MaxSize )
	{
		ooLogError("training data over 1 GB; SelectDictionaryFromPackets is not 64-bit safe.  Clamping...\n");
		test_block_size64 = SuffixTrie2_MaxSize;
	}

	#ifndef __RAD64__
	if ( test_block_size64 > (16<<20) )
	{
		rrprintf("WARNING : large training dictionary in 32-bit, this will probably run out of memory...\n");
	}
	#endif
	
	SINTa test_block_size = oo64toA(test_block_size64);

	U8 * test_block = (U8 *) OodleMalloc( test_block_size );
	
	U8 * test_block_ptr = test_block;
	U8 * test_block_end = test_block + test_block_size;
	
	if ( test_block_size64 == SuffixTrie2_MaxSize )
	{
		// hit clamp
		// do randomized packet selection rather than just the first N

		for(;;)
		{
			int p = rrRandStateMod(&rand_state,num_test_packets);
			
			S32 cur_size = test_packet_sizes[p];

			if ( test_block_ptr + cur_size + 4 > test_block_end )
			{
				// hit premature end due to clamp
				memset(test_block_ptr,0,(test_block_end-test_block_ptr));
				test_block_ptr = test_block_end;
				break;
			}

			memcpy(test_block_ptr,test_packet_pointers[p],cur_size);
			test_block_ptr += cur_size;
			// insert random 4 bytes between packets to prevent match in suffix trie : (lame)
			U32 r32 = rrRandState32(&rand_state);
			RR_PUT32_NATIVE_UNALIGNED(test_block_ptr,r32);
			test_block_ptr += 4;
		}
	}
	else
	{
		for LOOP(p,num_test_packets)
		{
			S32 cur_size = test_packet_sizes[p];
			memcpy(test_block_ptr,test_packet_pointers[p],cur_size);
			test_block_ptr += cur_size;
			// insert random 4 bytes between packets to prevent match in suffix trie : (lame)
			U32 r32 = rrRandState32(&rand_state);
			RR_PUT32_NATIVE_UNALIGNED(test_block_ptr,r32);
			test_block_ptr += 4;
		}
	}
	
	RR_ASSERT_ALWAYS( test_block_ptr == test_block_end );
	
	rrprintf("SDFP : building Suffix Trie...\n");
	// build a SuffixArray on the test_block :
		
	SuffixTrieMatcher * test_stm = SuffixTrieMatcher_Create(test_block,test_block_size);

	#endif
	//=============================================================
	// make a lzp hash table :
	//	hash table stores the packet index that it was filled from
	
	S32 htsize = 1<<htbits;
	S32 htmask = htsize -1;	
	
	vector<S32> hash_table;
	hash_table.resize(htsize,-1);
	
	//=============================================================
	// scan all the dictionary packets and rate them :
	
	rrprintf("SDFP : rating packets...\n");
	
	vector<rated_packet> rated_packets;
	rated_packets.reserve(num_dictionary_packets);
	
	vector<const U8 *> unique_hashed_pointers;
	unique_hashed_pointers.reserve(16384);
	
	vector<SuffixTrieMatch> matches;
	
	double total_rating = 0;
	
	for LOOP(p,num_dictionary_packets)
	{
		const U8 * packet_ptr = (U8 *) dictionary_packet_pointers[p]; 
		S32 packet_size = dictionary_packet_sizes[p];
	
		#if 0
		
		// all packets, no rating :
	
		double rating = 1;
	
		rated_packets.push_back();
		rated_packet & rp = rated_packets.back();				
		rp.packet_ptr = packet_ptr;
		rp.packet_size = packet_size;
		rp.rating = rating;
		rp.base_rating = rating;
		
		#else
		
		// leave out tiny packets?
		//	hard to match strings and rate
		if ( packet_size < 8 )
			continue;
	
		const U8 * packet_end_ptr = packet_ptr + packet_size;
	
		double rating = 0;
		
		// scan packet backwards
		// add string if slot in hash table is not taken
		unique_hashed_pointers.clear();
		//for(U8 * ptr = packet_end_ptr - 2; ptr >= packet_ptr+3; ptr--)
		//for(U8 * ptr = packet_ptr + 3; ptr < packet_end_ptr-2; ptr++)
		for(const U8 * ptr = packet_ptr + 3; ptr < packet_end_ptr-8; ptr++)
		{
			U32 context = ContextBack3(ptr);
			U32 hash = OodleNetwork1_HashBack3(context) & htmask;
			if ( hash_table[hash] == p )
				continue; // hash slot already taken
			hash_table[hash] = p;
			
			unique_hashed_pointers.push_back(ptr);
		}
		
		if ( unique_hashed_pointers.empty() )
			continue;
		
		for LOOPVEC(pi,unique_hashed_pointers)
		{
			const U8 * ptr = unique_hashed_pointers[pi];
			const U8 * ctxptr = ptr-3;
			
			#if 0
			// 176.6 on its own
			
			U32 v4 = RR_GET32_NATIVE(ctxptr);
			U32 h4 = hash4_ops::hash_key(v4);
			const hash4_entry * entry4 = hash4.find(h4,v4);
			if ( entry4 != NULL )
			{
				rating += entry4->m_data;
			}
			
			U64 v8 = RR_GET32_NATIVE(ctxptr) | ((U64)ctxptr[4]<<32);
			//v8 |= ((U64)ctxptr[5]<<40);;
			//U64 v8 = RR_GET64_NATIVE(ctxptr);
			U32 h8 = hash8_ops::hash_key(v8);
			const hash8_entry * entry8 = hash8.find(h8,v8);
			if ( entry8 != NULL )
			{
				rating += entry8->m_data * 4.0;
				//rating += entry8->m_data * 6.0;
			}
			
			#endif
			
			#if 1
			// 175.5 on its own
			
			S32 maxml = rrPtrDiff32(packet_end_ptr - ctxptr);
			RR_ASSERT( maxml <= packet_size );
			
			// look up ptr in suffixarray
			S32 num_matches = SuffixTrieMatcher_LookupMatches(test_stm,ctxptr,maxml,&matches);
			
			for(int m=0;m<num_matches;m++)
			{
				if ( matches[m].length < 4 ) break;
				
				S32 ml = matches[m].length;
				S32 count = matches[m].count;
				
				//RR_ASSERT( memcmp(ctxptr,matches[m].ptr,ml) == 0 );
				
				//rating += count;
				//rating += rrlog2_F64((F64)ml)*count;
				rating += rrlog2_U32_approx(ml)*count;
				//rating += log((double)ml)*count;
				//rating += sqrt((double)ml)*count;
				//rating += c_rr_log2_table[RR_LOG2TABLE_SIZE/ml]*count;
				//rating += ml*count;
				//rating += ml*(double)count*count;
				//rating += ml*ml*count;
			}
			#endif
		}
		
		// scale rating?
		// like divide by size so it's a quality per byte?
		// nope everything seems to hurt here
		//rating *= rating;
		//rating /= unique_hashed_pointers.size();
		//rating /= packet_size;
					
		rated_packets.push_back();
		rated_packet & rp = rated_packets.back();				
		rp.packet_ptr = packet_ptr;
		rp.packet_size = packet_size;
		rp.rating = rating;
		rp.base_rating = rating;
		
		#endif

		total_rating += rating;
	}
	
	double average_rating = total_rating / num_dictionary_packets;
	//rrprintfvar(average_rating);
	
	//=================================================================

	rrprintf("SDFP : building dictionary...\n");
	
	SuffixTrieMatcher_Destroy(test_stm); test_stm = 0;
	OodleFree(test_block); test_block = 0;
	
	if ( num_trials <= 1 )
	{
		//=================================================================
		// sort by rating :
		
		stdsort(rated_packets.begin(),rated_packets.end());
		// best rating first :
		RR_ASSERT( rated_packets.size() < 2 || rated_packets[0].rating >= rated_packets[1].rating );

		//=================================================================	
		// build the dictionary from the end, best rating first :
		
		FillDictionary(dictionary_to_fill,dictionary_size,rated_packets);
			
		//=============================================================
	}
	else
	{
		void * trial_dictionary = OodleMalloc( dictionary_size );
				
		SINTa shared_size = OodleNetwork1_Shared_Size(htbits);
		SINTa lzpudp_state_size = OodleNetwork1UDP_State_Size();
	
		OodleNetwork1_Shared * lzp_shared = (OodleNetwork1_Shared *) OodleMalloc( shared_size );
		RR_ASSERT_ALWAYS( lzp_shared != NULL );
		
		OodleNetwork1UDP_State * lzpudp_state = (OodleNetwork1UDP_State *) OodleMalloc( lzpudp_state_size );
		RR_ASSERT_ALWAYS( lzpudp_state != NULL );

		/**
		
		generations :
		
		keep the best set from the last generation as the seed for the next
		not really working great
		should really keep the best N and cross them or some shit
		
		**/

		vector<rated_packet> best_rated_packets;
		if ( num_generations > 0 )
			best_rated_packets = rated_packets;
	
		SINTa best_comp_len = RR_S32_MAX;
			
		for LOOP(g,num_generations)
		{
			rrprintf("Generation %d :\n",g);

			if ( g > 0 )
				rated_packets = best_rated_packets;

			// randomness is a percent of average rating
			double random_multiplier = randomness * average_rating / 100.0;

			// randomness goes down with generations :
			random_multiplier *= (num_generations - g)/(double)num_generations;

			for LOOP(t,num_trials)
			{
				//if ( t > 0 ) // t == 0 gets no randomness
				{
					for LOOPVEC(rp,rated_packets)		
					{
						rated_packets[rp].rating = rated_packets[rp].base_rating + rrRandStateUnitFloat(&rand_state) * random_multiplier;
					}
				}
				
				stdsort(rated_packets.begin(),rated_packets.end());
			
				FillDictionary(trial_dictionary,dictionary_size,rated_packets);

				OodleNetwork1_Shared_SetWindow(lzp_shared,htbits,trial_dictionary,dictionary_size);
				
				//SINTa trial_comp_len = 
				OodleNetwork1UDP_Train(
					lzpudp_state,
					lzp_shared,
					test_packet_pointers, 
					test_packet_sizes,
					num_test_packets);
			
				SINTa trial_comp_len = OodleNetwork1UDP_MeasureTotalCompressedSize(
					lzpudp_state,
					lzp_shared,
					test_packet_pointers, 
					test_packet_sizes,
					num_test_packets);
			
				if ( trial_comp_len < best_comp_len )
				{
					best_comp_len = trial_comp_len;
					memcpy(dictionary_to_fill,trial_dictionary,dictionary_size);
					
					if ( num_generations > 0 )
					{
						// save the best ratings as the base for the next generation :
						for LOOPVEC(rp,rated_packets)		
						{
							best_rated_packets[rp] = rated_packets[rp];
							//double base_t = 0.25; // 
							double base_t = 0.5; // 173.1
							//double base_t = 0.75;
							best_rated_packets[rp].base_rating = 
								 (1.0 - base_t) * rated_packets[rp].rating
								 + base_t * rated_packets[rp].base_rating;
						}
					}
				}
				
				rrprintf("Trial %3d : %9d : %9d\n",t,trial_comp_len,best_comp_len);
			}
		}

		OodleFree(trial_dictionary);
		OodleFree(lzp_shared);
		OodleFree(lzpudp_state);
	}

	return true;
}

OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1_SelectDictionaryFromPackets(
		void * dictionary_to_fill,
		S32 dictionary_size,
		S32 htbits,
		const void ** dictionary_packet_pointers, 
		const S32 * dictionary_packet_sizes,
		S32 num_dictionary_packets,
		const void ** test_packet_pointers, 
		const S32 * test_packet_sizes,
		S32 num_test_packets
		)
{
	return OodleNetwork1_SelectDictionaryFromPackets_Trials(dictionary_to_fill,dictionary_size,htbits,
		dictionary_packet_pointers, 
		dictionary_packet_sizes,
		num_dictionary_packets,
		test_packet_pointers, 
		test_packet_sizes,
		num_test_packets,
		0,0.0,0);
}

OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1_SelectDictionarySupported(void)
{
	return true;
}

#else // OODLE_PLATFORM_HAS_SELECTDICTIONARYANDTRAIN

OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1_SelectDictionarySupported(void)
{
	return false;
}

OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1_SelectDictionaryFromPackets_Trials(
		void * dictionary_to_fill,
		S32 dictionary_size,
		S32 htbits,
		const void ** dictionary_packet_pointers, 
		const S32 * dictionary_packet_sizes,
		S32 num_dictionary_packets,
		const void ** test_packet_pointers, 
		const S32 * test_packet_sizes,
		S32 num_test_packets,
		S32 num_trials,
		double randomness,
		S32 num_generations
		)
{
	ooLogError("OodleNetwork1_SelectDictionaryFromPackets_Trials not supported on this platform!\n");
	return false;
}

OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1_SelectDictionaryFromPackets(
		void * dictionary_to_fill,
		S32 dictionary_size,
		S32 htbits,
		const void ** dictionary_packet_pointers, 
		const S32 * dictionary_packet_sizes,
		S32 num_dictionary_packets,
		const void ** test_packet_pointers, 
		const S32 * test_packet_sizes,
		S32 num_test_packets
		)
{
	ooLogError("OodleNetwork1_SelectDictionaryFromPackets not supported on this platform!\n");
	return false;
}

#endif // OODLE_PLATFORM_HAS_SELECTDICTIONARYANDTRAIN

//===================================================================

//#define OODLENETWORK1UDP_MAXML	128	// 177.2

#define ON1UDP_CONTEXT_COUNT	LZP_MC_SIZE

RADFORCEINLINE int OodleNetwork1_MakeContext(int o1, int mp0)
{
	//return OodleNetwork1_MakeLitMatchContext(o1,mp0); // 177.2
	return OodleNetwork1_MakeMatchContext(o1,mp0); // 171.0
}

//===================================================================

#define ON1UDP_LEN_NUM_LINEAR			12 // 168.3
//#define ON1UDP_LEN_NUM_LINEAR			8  // 168.5
//#define ON1UDP_LEN_NUM_LINEAR			4  // 169.0

#define ON1UDP_LEN_FIRST_WITH_EXTRABITS			(2+ON1UDP_LEN_NUM_LINEAR)
#define ON1UDP_LEN_FIRST_WITH_EXTRABITS_CODE		(254 + ON1UDP_LEN_FIRST_WITH_EXTRABITS)

#define ON1UDP_LEN_EXTRABIT_CODE_COUNT		13

#define OODLENETWORK1UDP_PACKET_SPLIT_SIZE		(2<<ON1UDP_LEN_EXTRABIT_CODE_COUNT) // 16384

#define ON1UDP_LEN_TOTAL_CODE_COUNT	(ON1UDP_LEN_NUM_LINEAR + ON1UDP_LEN_EXTRABIT_CODE_COUNT)

#if 1 // compiled-in table

static const S32 c_on2_len_decode_table[] = 
{
        14,      16,      20,      28,      44,      76,     140,     268,
       524,    1036,    2060,    4108,    8204,   16396
};

RR_COMPILER_ASSERT( RR_ARRAY_SIZE(c_on2_len_decode_table) == ON1UDP_LEN_EXTRABIT_CODE_COUNT+1 );

static void on2_make_len_code_tables()
{
}

#else // make the table

// indexed by (code - ON1UDP_LEN_FIRST_WITH_EXTRABITS_CODE)
static S32 c_on2_len_decode_table[ON1UDP_LEN_EXTRABIT_CODE_COUNT+1];

//static int c_maxml;

static void on2_make_len_code_tables()
{
	// fill c_on2_len_code_decode_table :
	S32 base = ON1UDP_LEN_FIRST_WITH_EXTRABITS;
	for LOOP(i,ON1UDP_LEN_EXTRABIT_CODE_COUNT)
	{
		c_on2_len_decode_table[i] = base;
		S32 extraBits = i+1;
		S32 count = 1 << extraBits;
		base += count;
	}

	// one past end	
	c_on2_len_decode_table[ON1UDP_LEN_EXTRABIT_CODE_COUNT] = base;

	rrPrintfS32Array(c_on2_len_decode_table,RR_ARRAY_SIZE(c_on2_len_decode_table),"c_on2_len_decode_table",
		8,8);

	S32 maxml = base-1;
	//rrprintfvar(c_maxml);
	
	RR_ASSERT_ALWAYS( maxml >= OODLENETWORK1UDP_PACKET_SPLIT_SIZE );	
}

#endif

//===================================================================

#define ON1UDP_TANS_L_BITS		12	// 167.6

/*

example_packet Jaguar perf counters : (arith; cumprobtotshift = 10 instead of 12)
decoder is a tiny bit faster (12.66 mb/s vs 10.26 mb/s)

OodleNetwork1 UDP [4|19] :
OodleNetwork1UDP encode : 5.571 seconds, 309.87 b/kc, rate= 17.57 mb/s
OodleNetwork1UDP decode : 7.732 seconds, 0.00 b/kc, rate= 12.66 mb/s
OodleNetwork1UDP both : 13.303 seconds, 0.00 b/kc, rate= 14.72 mb/s
ooNet1Decode perf timings:
  insts_retired=1950.0M  cycles_not_halted=5189.6M
  IPC=0.38  I$miss=0.2%  D$miss=8.7%  br_mispred=9.7%
  cycles wasted from...  br_mispred=6.7%  D$miss=59.4%  I$miss=0.8%
429.3 -> 175.3 average = 2.449:1 = 59.16% reduction

#define ON1UDP_TANS_L_BITS		10 // 169.4

*/

//#define ON1UDP_TANS_L_BITS		10 // 169.4
//#define ON1UDP_TANS_L_BITS		11	//
//#define ON1UDP_TANS_L_BITS		12	// 167.6
//#define ON1UDP_TANS_L_BITS		13 // 167.5
//#define ON1UDP_TANS_L_BITS		14 // 167.4
#define ON1UDP_TANS_L			(1<<ON1UDP_TANS_L_BITS)
#define ON1UDP_ALPHABET		(256+ON1UDP_LEN_TOTAL_CODE_COUNT)

/**

OodleNetwork1UDP_CountingState - big code dupe from encoder!

**/

struct OodleNetwork1UDP_CountingState
{
	struct Histo_match { U32 counts[ON1UDP_ALPHABET]; };
	struct Histo_nomatch { U32 counts[256]; };
	Histo_match	m_histos_match[ON1UDP_CONTEXT_COUNT];
	Histo_nomatch	m_histos_nomatch[256];
	
	U32	m_total_match[ON1UDP_CONTEXT_COUNT];
	U32	m_total_nomatch[256];

	void Init( )
	{
		// set all counts to 1 so none are zero
		for LOOP(c,ON1UDP_CONTEXT_COUNT)
		{
			Histo_match & h = m_histos_match[c];
			for LOOP(a,ON1UDP_ALPHABET)
			{
				h.counts[a] = 1;
			}
		}
		
		for LOOP(c,256)
		{
			Histo_nomatch & h = m_histos_nomatch[c];
			for LOOP(a,256)
			{
				h.counts[a] = 1;
			}
		}
	}
	
	void Normalize()
	{
		for LOOP(c,ON1UDP_CONTEXT_COUNT)
		{
			Histo_match normc;
			Histo_match & h = m_histos_match[c];
			
			m_total_match[c] = sum(h.counts,h.counts+ON1UDP_ALPHABET);
			
			// normal from "h" to "normc"
			normalize_counts_current(normc.counts,ON1UDP_TANS_L,h.counts,m_total_match[c],ON1UDP_ALPHABET);
			// then copy over "h" :
			h = normc;
		}
		
		for LOOP(c,256)
		{
			Histo_nomatch normc;
			Histo_nomatch & h = m_histos_nomatch[c];

			m_total_nomatch[c] = sum(h.counts,h.counts+256);

			// normal from "h" to "normc"
			normalize_counts_current(normc.counts,ON1UDP_TANS_L,h.counts,m_total_nomatch[c],256);
			// then copy over "h" :
			h = normc;
		}
	}
	
	void count_match(int sym,int ctx)
	{
		m_histos_match[ctx].counts[sym] += 2;
	}
	
	void count_nomatch(int sym,int o1)
	{
		m_histos_nomatch[o1].counts[sym] += 2;
	}
	
	//-----------------------------------
	
	void Count(OodleNetwork1_Shared const * shared,const U8 * raw, SINTa rawlen)
	{
		if ( rawlen <= OODLENETWORK1_MIN_RAWLEN )
		{
			//memcpy(comp,raw,rawlen);
			return;
		}
		else if ( rawlen > OODLENETWORK1UDP_PACKET_SPLIT_SIZE )
		{
			Count(shared,raw,OODLENETWORK1UDP_PACKET_SPLIT_SIZE);
			raw += OODLENETWORK1UDP_PACKET_SPLIT_SIZE;
			rawlen -= OODLENETWORK1UDP_PACKET_SPLIT_SIZE;
			// call again to check rawlen again
			Count(shared,raw,rawlen);
			return;
		}

		//---------------------------------------------------------
		
		const ON1_HashEntry * lzp_hash = shared->m_lzp_hash;
		const U8 * lzp_window = shared->m_lzp_window;
		const U8 * lzp_window_end = shared->m_lzp_window_end;
		const U32 lzp_hash_mask = shared->m_lzp_hash_mask;
				
		const U8 * ptr = raw;
		const U8 * rawend = raw+rawlen;
		const U8 * matchend = rawend - OODLENETWORK1_MATCHEND_BACKUP;
				
		int o1 = 0;
		
		// encode first 3 :
		for(int i=0;i<3;i++)
		{
			int cur = ptr[0];
				
			count_nomatch(cur,o1);

			// slide :		
			o1 = cur;	
			ptr++;
		}

		RR_ASSERT( ptr == raw+3 );
			
		RR_ASSERT( ptr < matchend );
				
		U32 cb3 = ContextBack3(ptr);
		RR_ASSERT( o1 == ptr[-1] );
			
		for(;;)
		{
			int cur = ptr[0];
			RR_ASSERT( o1 == ptr[-1] );
			
			RR_ASSERT( cb3 == ContextBack3(ptr) );
			const ON1_HashEntry * he = ON1UDP_LookupHash(cb3,lzp_hash,lzp_hash_mask);
			if ( ! he ) 
			{
				count_nomatch(cur,o1);
				
				ptr++;
			
				if ( ptr >= matchend )
					break;
				
				o1 = cur;	
				cb3 = ContextSlide3(cb3,cur);				
			
				continue;
			}
			
			const U8 * mp = lzp_window + he->index;
						
			int mp0 = mp[0];
			int lmc = OodleNetwork1_MakeContext(o1,mp0);
			
			if ( cur != mp0 ) // no match
			{
				count_match(cur,lmc);
				
				ptr++;
			
				if ( ptr >= matchend )
					break;
				
				o1 = cur;	
				cb3 = ContextSlide3(cb3,cur);	
			}
			else
			{
				 // match

				RR_ASSERT( cur == mp0 );
				SINTr len = lzp_getmatchlen(ptr,rawend,mp,lzp_window_end);
				
				if ( len == 1 )
				{
					// len 1 match, just encode it as a literal
					count_match(cur,lmc);
					
					ptr++;
				
					if ( ptr >= matchend )
						break;
					
					o1 = cur;	
					cb3 = ContextSlide3(cb3,cur);	
				}
				else
				{				
					RR_ASSERT( len >= 2 );
					RR_ASSERT( ptr+len <= rawend );
					
					RR_ASSERT( memcmp(ptr,mp,(size_t)len) == 0 );
					
					if ( len < ON1UDP_LEN_FIRST_WITH_EXTRABITS )
					{	
						// len linear :				
						count_match(254+(int)len,lmc);
					}
					else
					{
						// true NOSB, just use BSR :
						U32 len_above = (U32)len - ON1UDP_LEN_FIRST_WITH_EXTRABITS + 2;
						U32 remainderBits = rrGetBitLevel_V_NonZero(len_above) - 1;
						//U32 remainderBits = 31 - __builtin_clz(len_above); 
						S32 code = ON1UDP_LEN_FIRST_WITH_EXTRABITS_CODE -1 + remainderBits;
						//S32 remainder = len_above - (1<<remainderBits);
						count_match(code,lmc);
					}
					
					ptr += len;
				
					if ( ptr >= matchend )
						break;
				
					// reload :
					cb3 = ContextBack3(ptr);		
					o1 = ptr[-1];
				}
			}
		}
		
		{
			while( ptr < rawend )
			{	
				o1 = ptr[-1];
			
				int cur = ptr[0];
					
				count_nomatch(cur,o1);
			
				ptr++;
			}

			RR_ASSERT( ptr == rawend );
		}		
	}
};

// I know I never write more than 16 bits in an op (max of 2 bytes out), so I can use a simple renorm check :
static RADFORCEINLINE void myArithEncodeRenorm2( rrArithEncoder* ac )
{
    if ( ac->range < RR_ARITHCODER_MINRANGE )
    {
		if ( ac->range < (1<<16) )
		{
			RR_PUT32_BE(ac->ptr,ac->low);
			ac->ptr += 2;
			ac->low <<= 16;
			ac->range <<= 16;
		}
		else
		{
			*ac->ptr++ = (U8) (ac->low>>24);
			ac->low <<= 8;
			ac->range <<= 8;
			
			RR_ASSERT( ac->range >= RR_ARITHCODER_MINRANGE );
		}
    }
}

#define rrArithEncodeRenorm myArithEncodeRenorm2
// 14.87
// 14.95

//#define rrArithEncodeRenorm ArithEncodeRenormBranchless

// same as rrArithDecodeRenorm , but checks ac.ptr vs check_ptr to prevent comp over-read
#define myArithDecodeRenormAndCheckReturn0(arithDecoder,check_ptr) do{ \
    while( (arithDecoder)->range < RR_ARITHCODER_MINRANGE ) { \
        (arithDecoder)->code <<= 8; \
        (arithDecoder)->range <<= 8; \
        if ( (arithDecoder)->ptr >= check_ptr ) \
			return 0; \
        (arithDecoder)->code |= *(arithDecoder)->ptr++; \
    } \
} while(0)


struct OodleNetwork1UDP_State
{
	//*
	// decoder uses binary search of cumprob
	
	// OodleNetwork1 UDP models take : 10847776 bytes shared (2459136 + 4194336 + 4194304)
	
	//OodleNetwork1UDP encode : 0.414 seconds, 34.12 b/kc, rate= 59.06 mb/s
	//OodleNetwork1UDP decode : 1.253 seconds, 11.29 b/kc, rate= 19.53 mb/s
	//typedef O0ArithCoder_Const<U16,ON1UDP_ALPHABET,ON1UDP_TANS_L_BITS> t_o0coder_match;
	//typedef O0ArithCoder_Const<U16,256,ON1UDP_TANS_L_BITS> t_o0coder_nomatch;
	
	typedef O0ArithCoderJump_Const<U16,U16,ON1UDP_ALPHABET,ON1UDP_TANS_L_BITS> t_o0coder_match;
	typedef O0ArithCoderJump_Const<U16,U8,256,ON1UDP_TANS_L_BITS> t_o0coder_nomatch;
	
	/*/

	// !! MUCH HIGHER MEMORY USE
	// OodleNetwork1 UDP models take : 45450784 bytes shared (37062144 + 4194336 + 4194304)

	//OodleNetwork1UDP encode : 0.473 seconds, 29.90 b/kc, rate= 51.77 mb/s
	//OodleNetwork1UDP decode : 0.809 seconds, 17.46 b/kc, rate= 30.23 mb/s
	typedef O0ArithCoder_Const<U16,ON1UDP_ALPHABET,ON1UDP_TANS_L_BITS> t_o0encoder_match;
	typedef O0ArithCoder_Const<U16,256,ON1UDP_TANS_L_BITS> t_o0encoder_nomatch;
	typedef O0ArithTableDecoder_Const<U16,U16,ON1UDP_ALPHABET,ON1UDP_TANS_L_BITS> t_o0decoder_match;
	typedef O0ArithTableDecoder_Const<U16,U8,256,ON1UDP_TANS_L_BITS> t_o0decoder_nomatch;
	/**/

	t_o0coder_match		m_coders_match[ON1UDP_CONTEXT_COUNT];
	t_o0coder_nomatch	m_coders_nomatch[256];
	
	U32	m_total_match[ON1UDP_CONTEXT_COUNT];
	U32	m_total_nomatch[256];
	
	void SetFromCounting(const OodleNetwork1UDP_CountingState * cs)
	{
		memcpy(m_total_match,cs->m_total_match,sizeof(U32)*ON1UDP_CONTEXT_COUNT);
		memcpy(m_total_nomatch,cs->m_total_nomatch,sizeof(U32)*256);
	
		for LOOP(c,ON1UDP_CONTEXT_COUNT)
		{
			m_coders_match[c].set_from_histo_normalized(cs->m_histos_match[c].counts);
		}
		for LOOP(c,256)
		{
			m_coders_nomatch[c].set_from_histo_normalized(cs->m_histos_nomatch[c].counts);
		}
	}
	
	//-----------------------------------
	
	SINTa Encode(OodleNetwork1_Shared const * shared,const U8 * raw, SINTa rawlen, U8 * comp) const
	{
		SIMPLEPROFILE_SCOPE_N(Encode,rawlen);
	
		//RR_ASSERT_ALWAYS( comp_buf_size >= rawlen + 8 );
	
		if ( rawlen <= OODLENETWORK1_MIN_RAWLEN )
		{
			memcpy(comp,raw,rawlen);
			return rawlen;
		}
		else if ( rawlen > OODLENETWORK1UDP_PACKET_SPLIT_SIZE )
		{
			U8 * first_comp_size_ptr = comp;
			comp += 2;
			SINTa first = Encode(shared,raw,OODLENETWORK1UDP_PACKET_SPLIT_SIZE,comp);
			RR_PUT16_LE(first_comp_size_ptr,(U16)(first));
			comp += first;
			SINTa second = Encode(shared,raw + OODLENETWORK1UDP_PACKET_SPLIT_SIZE,rawlen - OODLENETWORK1UDP_PACKET_SPLIT_SIZE,comp);
			SINTa total_ret = 2 + first + second;
			if ( total_ret >= rawlen )
			{
				// expanded!
				memcpy(first_comp_size_ptr,raw,rawlen);
				return rawlen;
			}
			return total_ret;
		}
				
		//=====================================================================
		
		rrArithEncoder ac;
		rrArithEncodeInit(&ac,comp);
		
		U8 * compExpandPtr = comp + rawlen - 2;
		SINTa compLen;
		
		t_o0coder_match const * const encoders_match = m_coders_match;
		t_o0coder_nomatch const * const encoders_nomatch = m_coders_nomatch;
		
		const ON1_HashEntry * lzp_hash = shared->m_lzp_hash;
		const U8 * lzp_window = shared->m_lzp_window;
		const U8 * lzp_window_end = shared->m_lzp_window_end;
		const U32 lzp_hash_mask = shared->m_lzp_hash_mask;
				
		const U8 * ptr = raw;
		const U8 * rawend = raw+rawlen;
		const U8 * matchend = rawend - OODLENETWORK1_MATCHEND_BACKUP;
		
		#if 1
		// initial prefestch :
		for(int i=4;i<5;i++)
		{
			U32 t_cb3 = ContextBack3(ptr+i);	
			U32 h = OodleNetwork1_HashBack3(t_cb3);
			U32 h1 = h & lzp_hash_mask;
			const ON1_HashEntry * t_he = lzp_hash+h1;
			RR_PREFETCHR_32B(t_he);
		}
		#endif
			
		int o1 = 0;
				
		// encode first 3 :
		for(int i=0;i<3;i++)
		{
			int cur = *ptr++;

			//rrprintf("ENC : %3d | %3d | %4d\n",cur,o1,encoders_nomatch[o1]->count(cur));
		
			encoders_nomatch[o1].encode_noadapt(&ac,cur);

			o1 = cur;	
		}

		RR_ASSERT( ptr == raw+3 );
			
		RR_ASSERT( ptr < matchend );
				
		U32 cb3 = ContextBack3(ptr);
		RR_ASSERT( o1 == ptr[-1] );
		
		const ON1_HashEntry * he = ON1UDP_LookupHash(cb3,lzp_hash,lzp_hash_mask);
			
		while( ptr < matchend )			
		{
			// cb3 and [he] should have been looked up in the previous iteration
			// it's done as early as possible for a small speed win
		
			if ( ac.ptr >= compExpandPtr )
			{
				goto expanded;
			}

			#if 1 // prefetch ahead
			if ( ptr+2 < rawend ) // fix bug, was reading 1 byte off end
			{
				// ptr+3 or ptr+2 ? -> measurement indicates +2 is marginally better
				//	be careful of measuring this, in my synethic tests hash table can easily be in cache
				//	in real use packet encoding is done with the whole hash table out of cache
				// of course if you get a match at the current pos this prefetch might not be used at all
				//	but that's not the case we need to speed up anyway
				U32 t_cb3 = ContextBack3(ptr+2);
				U32 h = OodleNetwork1_HashBack3(t_cb3);
				U32 h1 = h & lzp_hash_mask;
				const ON1_HashEntry * t_he = lzp_hash+h1;
				RR_PREFETCHR_32B(t_he);
			}
			#endif
			
			RR_ASSERT( ptr < matchend );
			int cur = ptr[0];
		
			RR_ASSERT( cb3 == ContextBack3(ptr) );
			RR_ASSERT( he == ON1UDP_LookupHash(cb3,lzp_hash,lzp_hash_mask) );
			RR_ASSERT( o1 == ptr[-1] );
							
			if ( ! he )
			{				
				ptr++;			
				cb3 = ContextSlide3(cb3,cur);	
				
				rrArithEncodeRenorm(&ac);
			
				he = ON1UDP_LookupHash(cb3,lzp_hash,lzp_hash_mask);
				
				encoders_nomatch[o1].encode_noadapt_norenorm(&ac,cur);
						
				o1 = cur;
				continue;
			}
									
			const U8 * mp = lzp_window + he->index;

			int mp0;
			#if ON1_HASHENTRY_MP0
			mp0 = he->mp0;
			RR_ASSERT( mp0 == mp[0] );
			#else
			mp0 = mp[0];
			#endif
			
			int lmc = OodleNetwork1_MakeContext(o1,mp0);
			
			rrArithEncodeRenorm(&ac);
			
			RR_ASSERT( (rawend - ptr) >= 2 );
			RR_ASSERT( (lzp_window_end - mp) >= LZP_DICTIONARY_END_PAD );
			
			// could just be a U16 compare
			if ( cur != mp0 || ptr[1] != mp[1] ) // no match
			{	
				ptr++;				
				o1 = cur;	
				cb3 = ContextSlide3(cb3,cur);	
				
				he = ON1UDP_LookupHash(cb3,lzp_hash,lzp_hash_mask);
				// no, don't ; already prefetched at ptr+3
				//RR_PREFETCHR_32B(he);
				
				encoders_match[lmc].encode_noadapt_norenorm(&ac,cur);
				continue;
			}
			else
			{
				// encode match len :

				SINTr len;
				len = lzp_getmatchlen(ptr,rawend,mp,lzp_window_end);
			
				RR_ASSERT( len >= 2 );
				RR_ASSERT( ptr+len <= rawend );
				
				RR_ASSERT( memcmp(ptr,mp,(size_t)len) == 0 );
							
				ptr += len;
							
				// reload early for the next iteration :
				if ( ptr < rawend ) // this if is redundant with ptr < matchend that we're about to check on the continue , but whatever
				{
					cb3 = ContextBack3(ptr);		
					o1 = ptr[-1];
					
					he = ON1UDP_LookupHash(cb3,lzp_hash,lzp_hash_mask);
				}
				
				if ( len < ON1UDP_LEN_FIRST_WITH_EXTRABITS )
				{
					// len linear :		
					S32 code = 254+(int)len;
					encoders_match[lmc].encode_noadapt_norenorm(&ac,code);		
				}
				else
				{
					// true NOSB, just use BSR :
					U32 len_above = (U32)len - ON1UDP_LEN_FIRST_WITH_EXTRABITS + 2;
					U32 remainderBits = rrGetBitLevel_V_NonZero(len_above) - 1;
					//U32 remainderBits = 31 - __builtin_clz(len_above); 
					S32 code = ON1UDP_LEN_FIRST_WITH_EXTRABITS_CODE -1 + remainderBits;
					S32 remainder = len_above - (1<<remainderBits);
											
					encoders_match[lmc].encode_noadapt_norenorm(&ac,code);		
					rrArithEncodeRenorm(&ac);
					rrArithPutBits(&ac,remainder,remainderBits);
				}
			}
		}
					
		rrArithEncodeRenorm(&ac);
		
		while( ptr < rawend )
		{
			o1 = ptr[-1];
		
			int cur = *ptr++;
				
			//rrprintf("ENC : %3d | %3d | %4d\n",cur,o1,encoders_nomatch[o1]->count(cur));
			
			encoders_nomatch[o1].encode_noadapt(&ac,cur);
		}

		RR_ASSERT( ptr == rawend );
		
		compLen = rrArithEncodeFlush(&ac);
		
		if ( compLen >= rawlen )
		{
			expanded:

			SIMPLEPROFILE_SCOPE(expanded);		
			
			memcpy(comp,raw,rawlen);
			return rawlen;
		}
		else
		{
			return compLen;
		}
	}
	
	SINTa Decode(OodleNetwork1_Shared const * shared,const U8 * comp, SINTa compLen, U8 * raw, SINTa rawlen) const
	{
		RR_ASSERT( compLen <= rawlen );
		if ( rawlen <= OODLENETWORK1_MIN_RAWLEN || compLen >= rawlen )
		{
			if ( compLen != rawlen )
				return -1;
			// tiny or expanded packet
			memcpy(raw,comp,rawlen);
			return rawlen;
		}
		else if ( rawlen > OODLENETWORK1UDP_PACKET_SPLIT_SIZE )
		{
			// added compLen 2 check for the GET16 on 08-26-2019
			// should not have been a crash risk due to OODLENETWORK1_DECOMP_BUF_OVERREAD_LEN
			RR_ASSERT( compLen > 2 );
			if ( compLen <= 2 )
				return -1;
			
			SINTa first_compLen = RR_GET16_LE(comp);
			comp += 2; compLen -= 2;
			
			RR_ASSERT( first_compLen < compLen );
			if ( first_compLen >= compLen )
				return -1;
			
			SINTa first_ret = Decode(shared,comp,first_compLen,raw,OODLENETWORK1UDP_PACKET_SPLIT_SIZE);
			RR_ASSERT( first_ret == first_compLen );
			if ( first_ret != first_compLen )
				return -1;
				
			comp += first_compLen;
			compLen -= first_compLen;
			RR_ASSERT( compLen > 0 );
			
			raw += OODLENETWORK1UDP_PACKET_SPLIT_SIZE;
			rawlen -= OODLENETWORK1UDP_PACKET_SPLIT_SIZE;
			
			SINTa second_ret = Decode(shared,comp,compLen,raw,rawlen);
			SINTa total_ret = 2 + first_ret + second_ret;
			
			return total_ret;
		}
		
		RR_ASSERT( compLen < rawlen );
	
		// can read past by 4 normally due to AC refill :
		const U8 * compOverrunCheck = comp + compLen + OODLENETWORK1_DECOMP_BUF_OVERREAD_LEN;
		
		rrArithDecoder ac;
		rrArithDecodeInit(&ac,comp);
	
		t_o0coder_match const * const decoders_match = m_coders_match;
		t_o0coder_nomatch const * const decoders_nomatch = m_coders_nomatch;
		
		// note : I tried the renorm-factor-out thing like encode
		//	it doesn't help here
		
		const ON1_HashEntry * lzp_hash = shared->m_lzp_hash;
		const U8 * lzp_window = shared->m_lzp_window;
		const U32 lzp_hash_mask = shared->m_lzp_hash_mask;
		
		const U8 * lzp_window_end = shared->m_lzp_window_end;
			
		U8 * ptr = raw;
		U8 * rawend = raw+rawlen;
		const U8 * matchend = rawend - OODLENETWORK1_MATCHEND_BACKUP;
		
		int o1 = 0;
				
		// decode first 3 :
		for(int i=0;i<3;i++)
		{
			myArithDecodeRenormAndCheckReturn0(&ac,compOverrunCheck);
			
			int cur = decoders_nomatch[o1].decode_noadapt_norenorm(&ac);
			
			//rrprintf("DEC : %3d | %3d | %4d\n",cur,o1,decoders_nomatch[o1]->count(cur));

			RR_ASSERT( cur < 256 );
			if ( cur >= 256 ) return -1;

			// slide :	
			*ptr++ = (U8) cur;	
			o1 = cur;	
		}
				
			
		RR_ASSERT( ptr == raw+3 );
		U32 cb3 = ContextBack3(ptr);
		
		for(;;)
		{
			RR_ASSERT( ptr < matchend );
			RR_ASSERT( cb3 == ContextBack3(ptr) );
			RR_ASSERT( o1 == ptr[-1] );
			
			myArithDecodeRenormAndCheckReturn0(&ac,compOverrunCheck);
			
			const ON1_HashEntry * he = ON1UDP_LookupHash(cb3,lzp_hash,lzp_hash_mask);
			
			// NOTE : this check for NULL he should be merged with the LookupHash checks
			//	 in VC2005 it isn't, but manually inlining LookupHash to remove the check didn't help
			if ( ! he )
			{
				int dec = decoders_nomatch[o1].decode_noadapt_norenorm(&ac);
			
				*ptr++ = (U8)( dec );
				
				if ( ptr >= matchend )
					break;
				
				o1 = dec;
				cb3 = ContextSlide3(cb3,dec);	
				
				continue;
			}
			
			RR_COMPILER_ASSERT( ON1_HASHENTRY_MP0 );
			int mp0 = he->mp0;
			
			int lmc = OodleNetwork1_MakeContext(o1,mp0);
			int dec = decoders_match[lmc].decode_noadapt_norenorm(&ac);

			if ( dec < 256 )
			{
				*ptr++ = (U8)( dec );
				
				if ( ptr >= matchend )
					break;
				
				o1 = dec;
				cb3 = ContextSlide3(cb3,dec);	
			}
			else
			{
				const U8 * mp = lzp_window + he->index;
			
				RR_ASSERT( mp + LZP_DICTIONARY_END_PAD <= lzp_window_end );
				RR_ASSERT( mp0 == mp[0] );

				if ( dec < ON1UDP_LEN_FIRST_WITH_EXTRABITS_CODE )
				{
					int len = dec - 254;
					RR_ASSERT( len >= 2 && len < ON1UDP_LEN_FIRST_WITH_EXTRABITS );
									
					RR_ASSERT_IF_NOT_CORRUPT( ptr+len <= rawend ); // mp+len part commented out below

					RR_COMPILER_ASSERT( LZP_DICTIONARY_END_PAD > ON1UDP_LEN_FIRST_WITH_EXTRABITS );

					// checking lzp_window_end is NOT necessary here
					// because LZP_DICTIONARY_END_PAD is greater than
					//	maximum len in this branch
					
					if ( ptr+len > rawend ) // || mp+len > lzp_window_end )
					{
						// overrun !
						return 0;
					}
					
					lzp_copymatch_basic(ptr,mp,len);
					ptr += len;
					
					if ( ptr >= matchend )
						break;
						
					cb3 = ContextBack3(ptr);
					o1 = ptr[-1];
				}
				else
				{
					int len_index = dec - ON1UDP_LEN_FIRST_WITH_EXTRABITS_CODE;
					SINTr len = c_on2_len_decode_table[len_index];
					int extraBits = 1+len_index;

					myArithDecodeRenormAndCheckReturn0(&ac,compOverrunCheck);
					
					RR_VARBITSTYPE remainder = rrArithGetBits_NoRenorm(&ac,extraBits);
					len += (SINTr)remainder;
					
					RR_ASSERT( len >= ON1UDP_LEN_FIRST_WITH_EXTRABITS );

					RR_ASSERT_IF_NOT_CORRUPT( ptr+len <= rawend && mp+len <= lzp_window_end );

					if ( ptr+len > rawend || mp+len > lzp_window_end )
					{
						// overrun !
						return 0;
					}

					lzp_copymatch_basic(ptr,mp,len);
					ptr += len;
					
					if ( ptr >= matchend )
						break;
						
					cb3 = ContextBack3(ptr);
					o1 = ptr[-1];				
				}
			}
		}
			
		while( ptr < rawend )
		{
			myArithDecodeRenormAndCheckReturn0(&ac,compOverrunCheck);
			
			o1 = ptr[-1];
		
			int dec = decoders_nomatch[o1].decode_noadapt_norenorm(&ac);

			//rrprintf("DEC : %3d | %3d | %4d \n",dec,o1,decoders_nomatch[o1]->count(dec));
			
			RR_ASSERT( dec < 256 );
			if ( dec >= 256 ) return -1;
			*ptr++ = (U8) dec;
		}
		
		RR_ASSERT( ptr == rawend );
		
		// no renorm here to avoid reading bytes past end of compbuf that we don't actually need
		// TellPos works in unnormalized state
		//rrArithDecodeRenorm(&ac);
			
		SINTa ret = rrArithDecodeTellPos(&ac);
		return ret;
	}
};


OOFUNC1 SINTa OOFUNC2 OodleNetwork1UDP_State_Size()
{
	return sizeof(OodleNetwork1UDP_State);
}

//=======================================================================================

static void OodleNetwork1UDP_Train_State_Counting(
		OodleNetwork1UDP_CountingState * state,
		const OodleNetwork1_Shared * shared,
		const void ** training_packet_pointers, 
		const S32 * training_packet_sizes,
		S32 num_training_packets)
{
	state->Init();
	
	// ->Count() uses a U32 histo which can overflow on huge input

	S64 total_size_64 = 0;
	for LOOP(p,num_training_packets)
	{
		total_size_64 += training_packet_sizes[p];
	}

	// counts go up by +2 so leave some safety :
	const S64 max_bytes_to_count = (1ULL<<30); // 1B
	if ( total_size_64 > max_bytes_to_count )
	{
		rrPrintf_v1("OodleNetwork1UDP_Train_State_Counting: huge input (" RR_S64_FMT "), sampling...\n",total_size_64);

		// randomly choose packets until we hit max bytes
		rrRandState rand_state;
		rrRandStateSeed(&rand_state, (U32)total_size_64 );
		
		S64 bytes_counted = 0;
		while(bytes_counted < max_bytes_to_count)
		{
			SINTa p = rrRandStateMod(&rand_state, num_training_packets);

			const void * cur_ptr = training_packet_pointers[p];
			SINTa cur_size = training_packet_sizes[p];
	
			state->Count(shared,U8_void(cur_ptr),cur_size);

			bytes_counted += cur_size;
		}
	}
	else
	{
		for LOOP(p,num_training_packets)
		{
			const void * cur_ptr = training_packet_pointers[p];
			SINTa cur_size = training_packet_sizes[p];
	
			state->Count(shared,U8_void(cur_ptr),cur_size);
		}
	}

	state->Normalize();
}



OOFUNC1 void OOFUNC2 OodleNetwork1UDP_Train(
		OodleNetwork1UDP_State * state,
		const OodleNetwork1_Shared * shared,
		const void ** training_packet_pointers, 
		const S32 * training_packet_sizes,
		S32 num_training_packets)
{
	// make a OodleNetwork1_State_Counting
	//	train it
	// then copy

	//RR_DO_ONCE( on2_make_len_code_tables() );

	OodleNetwork1UDP_CountingState * cs = OodleNew(OodleNetwork1UDP_CountingState);
	
	// OodleNetwork1_Train_State_Counting doesn't make good complen
	//SINTa complen = 
	OodleNetwork1UDP_Train_State_Counting(cs,shared,training_packet_pointers,training_packet_sizes,num_training_packets);

	if ( state )
		state->SetFromCounting(cs);

	OodleDelete(cs);
	
	//=================================
}

OOFUNC1 SINTa OOFUNC2 OodleNetwork1UDP_Encode(
		const OodleNetwork1UDP_State * state,
		const OodleNetwork1_Shared * shared,
		const void * raw, SINTa rawLen,
		void * comp )
{
	OOFUNCSTART
	
	SINTa ret = state->Encode(shared,U8_void(raw),rawLen,U8_void(comp));
	
	return ret;
}

OOFUNC1 rrbool OOFUNC2 OodleNetwork1UDP_Decode(
		const OodleNetwork1UDP_State * state,
		const OodleNetwork1_Shared * shared,
		const void * comp,  SINTa compLen,
		void * raw, SINTa rawLen )
{
	OOFUNCSTART
	
	RR_ASSERT_IF_NOT_CORRUPT( rawLen > 0 && compLen > 0 );
	RR_ASSERT_IF_NOT_CORRUPT( compLen <= rawLen );
	if ( rawLen <= 0 || compLen <= 0 || compLen > rawLen )
		return false;
	
	SINTa decLen = state->Decode(shared,U8_void(comp),compLen,U8_void(raw),rawLen);
	RR_ASSERT_IF_NOT_CORRUPT( decLen == compLen );
	rrbool ret = (decLen == compLen);
		
	return ret;
}	

static SINTa OodleNetwork1UDP_MeasureTotalCompressedSize(
		const OodleNetwork1UDP_State * state,
		const OodleNetwork1_Shared * shared,
		const void ** training_packet_pointers, 
		const S32 * training_packet_sizes,
		S32 num_training_packets)
{
	//=================================
	// compute a complen for the state
	
	SINTa tot_comp_len = 0;
	
	vector<U8> compv; compv.resize(65536);
	
	for LOOP(p,num_training_packets)
	{
		const void * cur_ptr = training_packet_pointers[p];
		SINTa cur_size = training_packet_sizes[p];
	
		if ( cur_size+4096 >= (SINTa)compv.size() )
		{
			compv.resize( cur_size + 4096 );
		}
	
		tot_comp_len += state->Encode(shared,U8_void(cur_ptr),cur_size,compv.data());
		//,compv.size());
	}
	
	return tot_comp_len;
}

//=====================================================================

struct OodleNetwork1UDP_StateCompacted
{
	U32 bufSize;
	U8 buffer[1];
};

OOFUNC1 SINTa OOFUNC2 OodleNetwork1UDP_StateCompacted_MaxSize()
{
	return OodleNetwork1UDP_State_Size();
}

//=========================================================================

//const float dz_scale = 1.f; // 0.5 - 1

#define Q_MIN		2
#define Q_SCALE		2
const int sendq_bits = 5;
//const int sendq_bits = 4;

// quantval also has DCT scaling in it
// dz_scale is in [0.5 , 1.0 ]
static RADFORCEINLINE int dequantize_dz( int coef, int quantizer ) //, float dz_scale )
{
	if ( coef == 0 )
	{
		return 0;
	}
	else
	{
		coef++;
		int mag = (coef>>1);
		int sign = coef&1;
		
		// 1412719
		//int val = (int) ( (mag - 0.5f + 1) * quantizer );
		int val = (mag+1)*quantizer - (quantizer>>1);
		
		return sign ? -val : val;
	}
}

static RADFORCEINLINE int quantize_dzf( int coef, float inv_quantizer ) //, float dz_scale)
{
	int signbit = 0;
	if ( coef < 0 )
	{
		signbit = 1;
		coef = -coef;
	}
	
	// invquantval also has DCT scaling in it
	float scaled = coef * inv_quantizer - 1;
	if ( scaled <= 0.f ) // <- @@ THIS IS A TERRIBLE FLOAT CHECK , 7 * (1/7) vs 1
		return 0; // inside DZ
	
	int qi = 1 + (int)( scaled ); // truncate
	
	int ret = (qi<<1) + signbit - 1;	
	
	return ret;
}

static RADFORCEINLINE int quantize_dzi( int coef, int quantizer ) //, float dz_scale)
{
	int signbit = 0;
	if ( coef < 0 )
	{
		signbit = 1;
		coef = -coef;
	}
	
	// quantize_dzf was trying to send 0 when coef == quantizer
	//	that's a little funny, but okay, preserve it
	//if ( coef < quantizer )
	if ( coef <= quantizer )
		return 0; // inside DZ

	// invquantval also has DCT scaling in it
	// truncate
	int qi = (coef / quantizer);
	RR_ASSERT( qi >= 1 );
	
	int ret = (qi<<1) + signbit - 1;	
	
	return ret;
}

// adaptive_quantizer :
//	give more bits to histos on contexts that are used more often 
//	because we send *normalized* histos
//
// use of floats and math that might differ per-platform
//	that's okay because this isn't use in Uncompact
//	it's baked into "sendq" and transmitted
static float adaptive_quantizer(U32 cur,U32 total,U32 context_count)
{
	RR_ASSERT( cur > 0 && cur < total );
	double l2 = rrlog2_F64( (double) total / ( cur * context_count ) );
	// larger quantizer = worse runtime compression
	//	but smaller state file after Compact
	//l2 *= 8.0; // 168.1   1475289
	l2 *= 12.0; // 168.2 1412981
	//l2 *= 16.0; // 168.4  1377420
	//l2 *= 20.0; // 168.5  1354299
	//l2 *= 24.0; // 168.6  1339057
	// OodleNetwork1UDP state file size : 1336252
	return (float) l2;
}

OOFUNC1 SINTa OOFUNC2 OodleNetwork1UDP_State_Compact( OodleNetwork1UDP_StateCompacted * to, const OodleNetwork1UDP_State * from)
{
	return OodleNetwork1UDP_State_Compact_ForVersion(to,from,OODLE2NET_VERSION_MAJOR);
}

OOFUNC1 SINTa OOFUNC2 OodleNetwork1UDP_State_Compact_ForVersion( OodleNetwork1UDP_StateCompacted * to, const OodleNetwork1UDP_State * from, S32 for_oodle_major_version)
{
	U8 * tobuf = to->buffer;

	rrVarBits vb;
	rrVarBits_PutOpen(vb.m,tobuf);
	
	//=============================================================
	
	U32 o0_histo_accum[ON1UDP_ALPHABET] = { 0 };
	
	// accumulate o0_histo :
	for(int c=0;c<ON1UDP_CONTEXT_COUNT;c++)
	{
		for(int a=0;a<ON1UDP_ALPHABET;a++)
		{
			o0_histo_accum[a] += from->m_coders_match[c].count(a);
		}
	}
	
	U32 o0_histo_norm[ON1UDP_ALPHABET];
	normalize_counts(for_oodle_major_version,o0_histo_norm,ON1UDP_TANS_L,o0_histo_accum,rrSumOfHistogram(o0_histo_accum,ON1UDP_ALPHABET),ON1UDP_ALPHABET);
	
	rrTANS_PackCounts(&vb,ON1UDP_TANS_L_BITS,ON1UDP_ALPHABET,o0_histo_norm,ON1UDP_ALPHABET,ON1UDP_ALPHABET,true);
		
	
	{
		U32 sum_total_match = sum(from->m_total_match,from->m_total_match+ON1UDP_CONTEXT_COUNT);
				
		for(int c=0;c<ON1UDP_CONTEXT_COUNT;c++)
		{
			U32 cur_total = from->m_total_match[c];
			float quantizerf = adaptive_quantizer(cur_total,sum_total_match,ON1UDP_CONTEXT_COUNT);
			//int sendq = (int)((quantizer - 1.0) / Q_SCALE);
			int sendq = (int)(0.5 + (quantizerf - Q_MIN) / Q_SCALE );
			sendq = RR_CLAMP(sendq,0,(1<<sendq_bits)-1);
			rrVarBits_Put(vb.m,sendq,sendq_bits);
			int quantizer = Q_MIN + sendq * Q_SCALE;
			//float inv_quantizer = 1.f/quantizer;
		
			//rrprintf("%d : (%d/%d) : %f\n",c,cur_total,sum_total_match,quantizer);
		
			U32 delta_histo[ON1UDP_ALPHABET];
			U32 nnz = 0;
			for(int a=0;a<ON1UDP_ALPHABET;a++)
			{
				S32 count = from->m_coders_match[c].count(a);
				S32 delta = count - o0_histo_norm[a];
				
				//delta_histo[a] = quantize_dzf( delta, inv_quantizer );
				delta_histo[a] = quantize_dzi( delta, quantizer );
				if ( delta_histo[a] )
					nnz ++;
			}

			int alphabet = ON1UDP_ALPHABET;
			while( alphabet > 0 && delta_histo[alphabet-1] == 0 )
				alphabet--;			
			rrTANS_PackCounts(&vb,ON1UDP_TANS_L_BITS,ON1UDP_ALPHABET,delta_histo,alphabet,nnz,false);
		}
	}
	
	// renormalize the first 256 :
	memcpy(o0_histo_accum,o0_histo_norm,256*sizeof(U32));
	normalize_counts(for_oodle_major_version,o0_histo_norm,ON1UDP_TANS_L,o0_histo_accum,rrSumOfHistogram(o0_histo_accum,256),256);
	
	{	
		U32 sum_total_nomatch = sum(from->m_total_nomatch,from->m_total_nomatch+256);
		
		for(int c=0;c<256;c++)
		{
			U32 cur_total = from->m_total_nomatch[c];
			float quantizerf = adaptive_quantizer(cur_total,sum_total_nomatch,256);
			int sendq = (int)(0.5 + (quantizerf - Q_MIN) / Q_SCALE);
			sendq = RR_CLAMP(sendq,0,(1<<sendq_bits)-1);
			rrVarBits_Put(vb.m,sendq,sendq_bits);
			int quantizer = Q_MIN + sendq * Q_SCALE;
			//float inv_quantizer = 1.f/quantizer;

			//rrprintf("%d : (%d/%d) : %f\n",c,cur_total,sum_total_nomatch,quantizer);

			U32 delta_histo[256];
			U32 nnz = 0;
			for(int a=0;a<256;a++)
			{
				S32 count = from->m_coders_nomatch[c].count(a);
				S32 delta = count - o0_histo_norm[a];
				
				//delta_histo[a] = quantize_dzf( delta, inv_quantizer );
				delta_histo[a] = quantize_dzi( delta, quantizer );
				if ( delta_histo[a] )
					nnz ++;
			}

			int alphabet = 256;
			while( alphabet > 0 && delta_histo[alphabet-1] == 0 )
				alphabet--;			
			rrTANS_PackCounts(&vb,ON1UDP_TANS_L_BITS,256,delta_histo,alphabet,nnz,false);
		}
	}
	
	rrVarBits_PutFlush8(vb.m);
	U8 * endPtr = rrVarBits_PutEndPtr( vb.m );
	SINTa bufSize = rrPtrDiff( endPtr - tobuf );
	to->bufSize = check_value_cast<U32>( bufSize );
	return sizeof(U32) + bufSize;
}

OOFUNC1 rrbool OOFUNC2 OodleNetwork1UDP_State_Uncompact( OodleNetwork1UDP_State * to, const OodleNetwork1UDP_StateCompacted * from )
{
	return OodleNetwork1UDP_State_Uncompact_ForVersion(to,from,OODLE2NET_VERSION_MAJOR);
}

OOFUNC1 rrbool OOFUNC2 OodleNetwork1UDP_State_Uncompact_ForVersion( OodleNetwork1UDP_State * to, const OodleNetwork1UDP_StateCompacted * from, S32 for_oodle_major_version)
{
	U32 bufSize =  from->bufSize;

	rrVarBits_Temps();
	rrVarBits vb;
	rrVarBits_GetOpen(vb.m,from->buffer,from->buffer+bufSize);
	
	U32 o0_histo_norm[ON1UDP_ALPHABET];
	if ( ! rrTANS_UnPackCounts(&vb,ON1UDP_TANS_L_BITS,ON1UDP_ALPHABET,o0_histo_norm,true) )
		return false;
				
	{			
		for(int ctx=0;ctx<ON1UDP_CONTEXT_COUNT;ctx++)
		{
			RR_VARBITSTYPE sendq = rrVarBits_Get_V(vb.m,sendq_bits);
			int quantizer = Q_MIN + (int)sendq * Q_SCALE;
			
			U32 histo[ON1UDP_ALPHABET];
			if ( ! rrTANS_UnPackCounts(&vb,ON1UDP_TANS_L_BITS,ON1UDP_ALPHABET,histo,false) )
				return false;
				
			for(int a=0;a<ON1UDP_ALPHABET;a++)
			{
				int delta = dequantize_dz( histo[a], quantizer );

				S32 count = (S32)( o0_histo_norm[a] + delta );
				if ( count < 1 ) count = 1;

				histo[a] = count;
			}			
			
			to->m_coders_match[ctx].set_from_histo(histo,for_oodle_major_version);
		}
	
	}
	
	// renormalize the first 256 :
	// copy first 256 to temp because normalize_counts can't read/write in place :
	U32 o0_histo_temp[256];
	memcpy(o0_histo_temp,o0_histo_norm,256*sizeof(U32));
	U32 sum_of_first_256 = rrSumOfHistogram(o0_histo_temp,256);
	
	//@@
	//rrprintfvar(sum_of_first_256);
	//rrprintf("Uncompact: o0_histo_temp = %08X\n",rrFNVHash((const U8 *)o0_histo_temp,sizeof(o0_histo_temp)) );
	
	// renormalize first 256 of o0_histo_norm to sum to ON1UDP_TANS_L
	normalize_counts(for_oodle_major_version,o0_histo_norm,ON1UDP_TANS_L,o0_histo_temp,sum_of_first_256,256);
	
	//@@
	//rrprintf("Uncompact: o0_histo_norm = %08X\n",rrFNVHash((const U8 *)o0_histo_norm,256*sizeof(U32)) );


	{
		for(int ctx=0;ctx<256;ctx++)
		{
			RR_VARBITSTYPE sendq = rrVarBits_Get_V(vb.m,sendq_bits);
			int quantizer = Q_MIN + (int)sendq * Q_SCALE;
			
			U32 histo[256];
			if ( ! rrTANS_UnPackCounts(&vb,ON1UDP_TANS_L_BITS,256,histo,false) )
				return false;
				
			for(int a=0;a<256;a++)
			{
				int delta = dequantize_dz( histo[a], quantizer );

				S32 count = (S32)( o0_histo_norm[a] + delta );
				if ( count < 1 ) count = 1;

				histo[a] = count;
			}			
			
			to->m_coders_nomatch[ctx].set_from_histo(histo,for_oodle_major_version);			
		}
	}
	
	SINTa getSize = rrVarBits_GetSizeBytes(vb.m,from->buffer);
	RR_ASSERT( getSize == (SINTa)from->bufSize );
	if ( getSize != (SINTa)from->bufSize )
		return false;
	
	return true;
}

//=======================================================================

OODLE_NS_END

//=====================================================
				
//idoc(begin)
//idoc(parent,OodleAPI_Network)
//idoc(page,OodleAPI_OodleNetwork1)
/*

	OodleNetwork1 compressor for shared-dictionary network channel compression.

	$^TableOfContents
	
*/

//idoc(parent,OodleAPI_OodleNetwork1)
//idoc(page,OodleNetwork1_About,About OodleNetwork1)
//idoc(autolink,on)
//idoc(markdown,on)
/*

OodleNetwork1 is a specialized compressor designed for compression of network packet transmission.
It's designed to reduce game bandwidth use and improve player experience.  

-------------------------

## Quick Intro to OodleNetwork features

$* OodleNetwork encodes packet by packet with zero latency.  The runtime encode and decode are simple buffer->buffer
calls.  Just build your packet as usual and then compress it, and send the compressed packet.

$* OodleNetwork runtime integration is extremely simple, just call Encode() in the server and Decode() in the client
(for one-way compression).  The complexity is all in the offline training process.

$* OodleNetwork does its heavy work in the offline training phase, so the runtime can be quite fast.

$* OodleNetwork does no allocations at runtime (you provide the memory for the model).  OodleNetwork for UDP uses zero memory per channel.  OodleNetwork for TCP uses around 100k per channel, which may be provided by the client code as a linear block.

$* OodleNetwork is about the same speed as "zlib -5" or "LZ4 HC" while achieving much more compression.

$* OodleNetwork is able to compress packets even if they have already been bit-packed.

$* OodleNetwork will never expand a packet.

$* OodleNetwork uses a dictionary size of your choosing.  Even a small 1 MB dictionary can provide good compression.
A typical size is 4 MB.  Larger dictionaries generally give more compression, but you are free to choose the tradeoff
that best suits your needs.

$* OodleNetwork is in Oodle2 Core and the runtime available on all platforms.  (model training can only be done on desktop platforms)

$* OodleNetwork is fully thread-safe and re-entrant with no mutexes or blocking.  The shared model data is read-only
after initialization.

-------------------------

## Compression of UDP network packets

With UDP networking, packets may be dropped or arrive in different orders.  This means there is
not a consistent history that the encoder and decoder see, so you cannot use any compressor which
is based on a per-channel history (zlib, OodleLZ, OodleNetwork1-TCP).

OodleNetwork1UDP does compression of packets without any per-channel state.

The $OodleNetwork1UDP_State is global and const; you make it offline in training, then distribute it as a const block of data with your game.  It can be used by all encoder and decoder channels.

Sometimes it may be advantageous to have separate models for upstream or downstream traffic, or to compress only one direction and not the other.  You can easily try these options and choose what works best for you.

Compression is done on each packet independently, so they can be lost or out of order and still decompress correctly.

\<PRE>
OodleNetwork1 [dictionary MB|hash bits]

OodleNetwork1 for TCP with per-channel history :

OodleNetwork1 [8|19] : 595654217 -> 123101634 = 4.839:1
1605378 packets; 371.0 -> 76.7 average

OodleNetwork1 for UDP with no per-channel state :

OodleNetwork1UDP [8|19] : 595654217 -> 150022411 = 3.970:1
1605378 packets; 371.0 -> 93.4 average

\</PRE>

-------------------------

## Compression of TCP network packets

NOTE : OodleNetwork1 and OodleLZ are for TCP network compression, that is when
you have a reliable per-channel history.  For UDP, use OodleNetwork1UDP (see below).

OodleNetwork1 for TCP uses a shared static dictionary, plus a dynamic state per channel.

Usage of OodleNetwork1 is demonstrated in $example_packet.

For more normal data distribution needs, and large downloads or join packets, use Oodle Data Compression.  See $OodleLZ_About.  

OodleNetwork1 is "zero latency".  That is, it don't add any buffering or delay of packets.  It produces compressed bytes immediately for each raw byte processed. 

OodleNetwork1 uses a static dictionary and hash table which is const and shared by all network channels. The size is set by the user.  The bigger the static dictionary, the more compression you will get. There is an adaptive per-channel arithmetic coder so that the match length and literal statistics can adapt to the channel a bit (this was a big win vs. using any kind of static models).

OodleNetwork1 has only 104k of per-channel state.  (compare to zlib which uses 400k per encoder)

On the server, a large static dictionary is no problem. They're running 16GB servers with 10,000 connections, they really don't care if the static dictionary is 64MB. However, that same static dictionary also has to be on the client, so the limit on how big a static dictionary you can use really comes from the client side. I suspect that something in the 8MB - 16MB range is reasonable. (and of course you can compress the static dictionary; it's only something like 2-4 MB that you have to distribute and load).  (for loading the compressed static dictionary off disk, see $OodleLZ_About)

(BTW you don't necessarily need an adaptive compression state for every open channel. If some channels tend to go idle, you could drop their state. When the channel starts up again, grab a fresh state (and send a reset message to the client so it wipes its adaptive state). You could do something like have a few thousand compression states which you cycle in an LRU for an unbounded number of open channels. Of course the problem with that is if you actually get a higher number of simultaneous active connections you would be recycling states all the time, which is just the standard cache over-commit problem that causes nasty thrashing, so YMMV etc.)

Here are some real world results :

\<PRE>
OodleNetwork1 TCP [dictionary MB|hash bits]

OodleNetwork1 [4|18] : 595654217 -> 131935361 = 4.515:1
1605378 packets; 371.0 -> 82.2 average
OodleNetwork1 [8|19] : 595654217 -> 123101634 = 4.839:1
1605378 packets; 371.0 -> 76.7 average
OodleNetwork1 [16|20] : 595654217 -> 110427772 = 5.394:1
1605378 packets; 371.0 -> 68.8 average
OodleNetwork1 [32|21] : 595654217 -> 93276137 = 6.386:1
1605378 packets; 371.0 -> 58.1 average
\</PRE>

-------------

See $OodleAPI_OodleNetwork1 , also $OodleNetwork_About_CapturingTrainingData and $OodleNetwork_About_FormingPacketsForMaximumCompression

*/


//idoc(parent,OodleAPI_Network)
//idoc(page,OodleNetwork_About,About Oodle Network Compression)
//idoc(autolink,on)
//idoc(markdown,on)
/*

Oodle Network Compression provides realtime compression & decompression of game
packets.

The primary compressor which you probably want is OodleNetwork1.  See
$OodleNetwork1_About .

If you are evaluating Oodle Network, the main thing you need to do is to capture a large
data set for testing and training.  See $OodleNetwork_About_CapturingTrainingData

Note that network packet compression is separate from compression of static packages.  eg. for sending
game updates or content, you would compress the data offline and just store it
compressed on your server.  For that, you should use the normal Oodle LZ compressors.
See $OodleLZ_About.

$^TableOfContents

$* $OodleNetwork1_About
$* $OodleAPI_OodleNetwork1

*/

//idoc(parent,OodleNetwork_About)
//idoc(page,OodleNetwork_About_CapturingTrainingData, Capturing Training data for OodleNetwork )
/*

To evaluate Oodle Network fairly, you will need to capture network packets from a real play session.

Once you are using Oodle Network in production, you will need to make a large capture to train the
compressor that you ship.

If possible, capture from a real game session (perhaps from your QA department
playing the game), not a simulation using bots.  Simulated captures can have patterns that don't
reflect real play.

The captured packets should be without any encryption or other compression
algorithms applied.  Any already compressed data (such as zlib or jpeg or voice data)
should be excluded from the packet capture.  Very large packets that you will compress with OodleLZ should also be excluded.

You should typically continue to use your heuristic bit-packing or delta scheme.

Try to capture at least 100 MB of packet data for the evalution.  For final game training you should
capture more.  Note that just capturing a very long single session is not generally helpful; you want to take a
broad sampling of many sessions over time to ensure that the capture is reflective of the whole spectrum of
packets that the game sends.

The better the training packets match the packets seen in the final game, the more compression there will be.
Mismatches are not a disaster, they simply mean that part of the dictionary is not useful for compression.

The $example_packet shipped with Oodle reads this file format :

\<PRE>
packet.bin :
U32 [LE] : numbers of channels (num_channels)
repeatedly :
{
	U32 [LE] : channel index in [0,num_channels-1]
	U32 [LE] : number of bytes of data in this packet (num_bytes)
	U8 * num_bytes : payload of this packet
}
\</PRE>

though you may always change example_packet to read a different format.

For UDP, write num_channels = 1 and the channel index of all packets as 0.

The cleanest way to capture packets is to add code to your server to log them out immediately before sending.
If you can't change your server code, then something like tcpdump can be used, but you will have to strip
the protocol headers.

For your final shipping capture, you should try to capture packets from a wide variety of play sessions in
different levels, with different numbers of players, to get a broad sampling of what your network traffic
looks like.  Then combine random portions of those captures to make the packet file that you use for Oodle
Network training.  This ensures that you don't train on a non-representative set of data.

NOTE : the packets that you hold out for testing/training/dictionary should be a random selection of
packets, not linear chunks.  You want each group of packets to be an independent random sampling of the
network traffic.  Each group should span the range of different types of data you send.  ($example_packet
includes one way of doing this)

NOTE : if you are compressing both upstream and downstream, those should generally have different dictionaries
and different trained states.  Typically the nature of the network traffic up and down is very different, so
they should not be mixed together in a single capture.  If you have very distinct network traffic phases
(such as, for example, a lobby or match-making phase and then a match play phase) then it may be advantageous
to separate those types of traffic for compression.

Contact oodle@radgametools.com with any questions

*/


//idoc(parent,OodleNetwork_About)
//idoc(page,OodleNetwork_About_FormingPacketsForMaximumCompression, Forming Packets for Maximum Compression )
/*

The way you create your network packets can have a large effect on their compressability.
These are a few tips for people who are interested in experimenting to get more compression.

What works on different games varies, some of these may not work for your game.

$* Try not to include pre-compressed data.  Don't use zlib or any other compressor before giving the data to
Oodle.  If you do need to send pre-compressed data to the client, don't run those packets through OodleNetwork,
and don't include them in the training set.

$* Getting rid of floats is always good.  Floating point numbers usually have a lot of useless precision in
the low bits that the game doesn't actually care about.  These show up as a nearly random byte in the low
part of the word.  It's best to figure out how much precision you actually need and covert all floats to
fixed point integers.

$* Do do reductions that are mathematical in nature, which the compressor will never be able to figure out.
For example, send matrices as Euler angles, send only 2 components of normalized vectors, etc.

$* Don't worry too much about bit packing or entropy reduction that is purely statistical.  This is what the
Oodle Network compressor is good at.

$* Consider turning off bit packing entirely.  In some cases it's better to just do byte-aligned packet
construction and let the compressor handle all the bit packing.  Even though the uncompressed packet will be
much bigger, it can result in a smaller compressed packet.  It also may save a lot of CPU time by avoiding the
bit packing.  (this varies a lot from game to game)

$* If you do bit-pack, try to byte-align in strategic places.  At the very least at the start of each game entity
you should flush to byte alignment.  Possibly in other places, such as between the property index and the property
value.

$* Try to use absolute indices instead of relative indices when refering to properties or other entities.
This makes the identifier stable and consistent, so that the compressor can identify repeated uses of the same
identifier.

$* If possible, try to include a hint about the property or entity in its first bytes.  Generally don't do this
if requires adding a byte you weren't already sending.  For example if you send an entity index, use the top bits
to indicate the type of entity.  That way the compressor can see the first bytes and use them to expect what's
coming next.

$* Very large and rare packets should usually be compressed with Oodle Data Compression and not piped through the
Oodle Network system.  For example you might send a huge initial join packet that's 100 kB , then normal game play
packets are around 200 bytes.  Those are very different things and should be compressed separately.

$* Split packets for MTU after Oodle network, not before.

See also :

http://cbloomrants.blogspot.com/2012/10/10-16-12-thoughts-on-bit-packing.html

*/

