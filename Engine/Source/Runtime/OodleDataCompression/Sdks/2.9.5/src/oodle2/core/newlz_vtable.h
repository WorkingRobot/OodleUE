// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlebase.h"
#include "oodlelzcompressors.h"
#include "rrmemutil.h"
#include "rrarenaallocator.h"
#include "rrlzh_lzhlw_shared.h"


OODLE_NS_START

struct UnpackedMatchPair;
struct LRMSet;
struct newlz_vtable;
struct LRMCascade;
class IncrementalMatchFinder;
struct OodleSpeedFit;
struct OodleKrakenChunkDeadlines;

#define NEWLZ_MIN_CHUNK_LEN			128

// 64k ?
// huff packagemerge uses around ~20k
//	I use arena from scratch mem in various other spots in encoders
//	so just get 64k up front
//#define newlz_arena_alloc_size_if_none	(64*1024)
// -> NO
// this is done at the OodleLZ_Compress() level

struct newlz_encoder_scratch;

typedef
SINTa (t_newLZ_encode_chunk)(const newlz_vtable * vtable,
	newlz_encoder_scratch * scratch,
	const U8 * dictionaryBase,
	const U8 * chunk_ptr,int chunk_len,
	U8 * comp,U8 * comp_end,
	SINTa chunk_pos,
	int * pchunk_type,
	F32 * pJ,
	const OodleKrakenChunkDeadlines * deadline);
	
typedef
IncrementalMatchFinder *(t_create_match_finder)(const U8 * ubuf,
		SINTa size,
		SINTa startRecordingPos,
		LRMSet * lrms,
		OodleLZ_Jobify jobify,
		void * jobifyUserPtr);

typedef
void (t_free_matcher)(void * matcher);

// void_deletor
//	func ptr helper to "delete" an object by type (destructor + free)
template <typename T>
void void_deletor(void * what)
{
	T * thing = (T *)what;
	OodleDelete(thing);
}

// void_destructor
//	like void_deletor but only does destruct, NO FREE
template <typename T>
void void_destructor(void * what)
{
	T * thing = (T *)what;
	destruct(thing);
}
		
/**

newlz_scratchblock is intended to just hold an allocation in scope
it's intended for places where I would use Array<> or vector<> just to scope an alloc

**/

struct newlz_scratchblock
{
public:
	void *	m_ptr;
	SINTa	m_size;
	rrbool  m_freeptr;

	newlz_scratchblock() : m_ptr(NULL), m_size(0), m_freeptr(0) { }
	~newlz_scratchblock() { release(); }
	
	/*
	explicit newlz_scratchblock(SINTa size)
	{
		m_ptr = OodleMalloc(size);
		m_size = size;
	}
	*/
	
	void * get() const { return m_ptr; }
	SINTa size() const { return m_size; }
	
	U8 * getU8() const { return (U8 *)m_ptr; }
	U8 * endU8() const { return (U8 *)m_ptr + m_size; }
	
	void release()
	{
		if ( m_ptr && m_freeptr )
		{
			OodleFree(m_ptr);
		}
		m_freeptr = false;
		m_ptr = NULL;
		m_size = 0;
	}
	
	// extend = allocate if bigger than previous
	/*
	void extend(SINTa size)
	{
		if ( size <= m_size ) return;
		release();
		
		m_ptr = OodleMalloc(size);
		m_size = size;
	}
	*/
	
	void extend(SINTa size, rrArenaAllocator * arena)
	{
		if ( m_size > 0 )
		{
			// first extend should be the biggest
			RR_ASSERT_ALWAYS( size <= m_size );
			return;
		}
		
		// this is basically the same as rrArenaAllocLeakyAligned
		if ( arena && arena->GetCurAvail() >= size )
		{
			m_ptr = arena->Alloc(size);
			m_freeptr = false;
		}
		else
		{
			m_ptr = OodleMalloc(size);
			m_freeptr = true;
		}
		
		m_size = size;
	}
	
	void memset_zero() { if ( m_ptr ) rrMemSetZero(m_ptr,m_size); }
	
private:

	// no copy
	newlz_scratchblock(const newlz_scratchblock & rhs) { }
	void operator = (const newlz_scratchblock & rhs) { }
};


// opt-in bitstream features (->vtable bitstream_flags)
#define NEWLZ_BITSTREAM_FLAG_SEND_EXCESS_SIZE	(1<<0)	// Oodle >= 2.6.0
#define NEWLZ_BITSTREAM_FLAG_PREFETCHABLE_MATCH	(1<<1)	// Oodle >= 2.6.0
#define NEWLZ_BITSTREAM_FLAG_ALT_OFFSETS		(1<<2)	// Oodle >= 2.6.0

struct newlz_vtable
{
	OodleLZ_Compressor compressor;
	OodleLZ_CompressionLevel level;
	const OodleSpeedFit * speedfit;
	const OodleLZ_CompressOptions * pOptions;
	int chunk_len;
	void * matcher;
	SINTa ctmf_mem_size;
	t_free_matcher * fp_free_matcher;
	t_create_match_finder * fp_create_match_finder;
	int find_all_matches_num_pairs;
	F32 lambda;
	U32 entropy_flags;
	U32 bitstream_flags;
	bool wants_dic_limit_splits;
	bool try_huff_chunks;

	int decodeType;
	t_newLZ_encode_chunk * fp_encode_chunk;
	
	// optimal parse carried state :
	// in Hydra this needs to be separate per coder
	//	Hydra shares the newlz_encoder_scratch between all coders
	//	but they get their own vtable, so I'm sticking it here
	newlz_scratchblock	carried_encoder_state;
	int					carried_encoder_state_chunktype;	
	
	// for Hydra :
	newlz_vtable * pvtable2; // this is a linked list
	
	newlz_vtable();
	~newlz_vtable();
};

// size of literals_space reserve
//	should fit literals + packets
//	-> you CAN get good compression even when this is > chunk_len
//	-> I just want to never hit this
static int newlz_literal_space_reserve_size( int chunk_len )
{
	int limit = chunk_len + (chunk_len>>4) + 256;
	
	return limit;
}

struct newlz_encoder_scratch
{
	rrArenaAllocator *	arena;

	newlz_scratchblock	newlz_arrays_space;
	newlz_scratchblock	newlzf_arrays_space;
	newlz_scratchblock	literals_space; // literals and packets in parse; also used for huff chunks
										// this is sized by newlz_literal_space_reserve_size
	newlz_scratchblock	parsevec_space; // encoder parse scratch room
	newlz_scratchblock	newlzhc_parsevec_space; // encoder parse scratch room
	newlz_scratchblock	matches_space; // for optimal parse
	newlz_scratchblock	arrivals_space; // for optimal parse
	newlz_scratchblock	last_tll_len_space;
	newlz_scratchblock	newlzhc_arrivals_space; // for optimal parse
	newlz_scratchblock	comp2_space; // for hydra comp buf
	
	newlz_scratchblock  newlzhc_passinfo_space;
	newlz_scratchblock  newlzhc_codecosts_space;
	newlz_scratchblock  newlzhc_last_tll_len_space;
	
	UnpackedMatchPair * match_pairs; // for optimal matchers (points into matches_space if used)

	newlz_encoder_scratch() : arena(NULL), match_pairs(NULL) { }
};


template <typename t_ctmf_type>
void newlz_vtable_setup_ctmf(
	newlz_vtable * pvtable,
	const U8 * dictionaryBase,	
	const U8 * raw,
	int table_bits,
    rrArenaAllocator * arena,
	int hash_len_override = 0)
{

	pvtable->ctmf_mem_size = t_ctmf_type::get_mem_size(table_bits,table_bits,16);
	if ( raw == NULL ) // for OodleLZ_GetCompressScratchMemBound
		return;

	t_ctmf_type * ctmf;
	
	ctmf = rrArenaNewIfAvail<t_ctmf_type>(arena);
	if ( ctmf )
	{
		pvtable->fp_free_matcher = void_destructor<t_ctmf_type>;
	}
	else
	{
		ctmf = OodleNew(t_ctmf_type);
		pvtable->fp_free_matcher = void_deletor<t_ctmf_type>;
	}
	
	ctmf->allocate(table_bits,table_bits,16,arena,hash_len_override);
	
	if ( raw != dictionaryBase )
	{		
		SINTa maxPreload = rrPtrDiff(raw - dictionaryBase);
		
		// maxLocalDictionarySize limits preload distance :
		// if I have an LRM anyway, no need to preload more :
		
		RR_ASSERT( pvtable->pOptions != NULL );
		const OodleLZ_CompressOptions * pOptions = pvtable->pOptions;
		
		if ( pvtable->level >= OodleLZ_CompressionLevel_Optimal1 && pOptions->makeLongRangeMatcher )
			maxPreload = RR_MIN(maxPreload,pOptions->maxLocalDictionarySize);
		// NOTE : in non-LRM modes, maxLocalDictionarySize does NOT limit preload
		
		// also no need to preload more than dictionarySize :
		if ( pOptions->dictionarySize > 0 )
			maxPreload = RR_MIN(maxPreload,pOptions->dictionarySize);
		
		// no point preloading more than CTMF_POS_BITS :
		maxPreload = RR_MIN(maxPreload,(1<<t_ctmf_type::c_max_preload_pos_bits));
		
		// check for resets :
		RR_DURING_ASSERT( SINTa posInDic = raw - dictionaryBase );
		// OodleLZ_Compress ensures this now :
		// if decodeStartOffset is not a multiple of OODLELZ_BLOCK_LEN it's an almost gauranteed failure
		RR_ASSERT( (posInDic & (OODLELZ_BLOCK_LEN-1)) == 0 );
		// OodleLZ_Compress has fixed posInDic :
		RR_ASSERT( ! pOptions->seekChunkReset || posInDic < pOptions->seekChunkLen );
		
		ctmf->set_base_and_preload(dictionaryBase,raw,maxPreload);
	}
	else
	{
		ctmf->set_base(raw);
	}
	
	pvtable->matcher = ctmf;	
}


SINTa newlz_compress_vtable(newlz_vtable * vtable,
	const U8 * wholeRawBuf,U8 * compBuf,SINTa wholeRawLen,
	const U8 * dictionaryBase,
	const LRMCascade * lrm_casc,
	rrArenaAllocator * arena);

SINTa newlz_enc_mem_bound(newlz_vtable * vtable,SINTa wholeRawLen);


// this is just a deref (pOptions->spaceSpeedTradeoffBytes) + validation
// all newlz uses of spaceSpeedTradeoffBytes should call this
//	(it's just used to fill vtable->lambda)
static RADINLINE F32 newlz_spaceSpeedTradeoffBytes_for_lambda(const OodleLZ_CompressOptions * pOptions)
{
	S32 sstb = pOptions->spaceSpeedTradeoffBytes;
	if ( sstb == 0 ) // zero means default
		return (F32) OODLELZ_DEFAULT_SSTB;
	else if ( sstb < 0 ) // negative to get zero
		return 0.f;
	else
		return (F32) sstb;
}

OODLE_NS_END
