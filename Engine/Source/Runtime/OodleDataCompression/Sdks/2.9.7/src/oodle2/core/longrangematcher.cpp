// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "longrangematcher.h"
#include "oodlebase.h"
#include "threadprofiler.h"
#include "templates/rrvector.h"
#include "templates/rralgorithm.h"
#include "rrlz_getmatchlen.inl"
#include "cbradutil.h"
#include "oodleconfigvalues.h"
#include "rrlog.h"
#include "rrprefetch.h"

#include "oodlehandle.h"

#if 0
#include "../ext/rrsimplemalloc.h" // for OodleXMalloc_LogMemUse
#else
#define OodleXMalloc_LogMemUse(x)
#endif

OODLE_NS_START

#ifndef OODLE_BUILDING_DATA
#undef THREADPROFILEFUNC
#define THREADPROFILEFUNC()
#endif


extern const LRM_hash_t c_hashMul;

// SREP uses 153191
//	

#if 0
RR_COMPILER_ASSERT( sizeof(LRM_hash_t) == 8 );
const LRM_hash_t c_hashMul = 44485709377909ULL;
#define LRM_HASH_BITS	64
#else
// 32 bit hash - use less memory, more better
RR_COMPILER_ASSERT( sizeof(LRM_hash_t) == 4 );
// this seems to have pretty bad generacy on english text :
const LRM_hash_t c_hashMul = 741103597UL;
#define LRM_HASH_BITS	32

const LRM_hash_t c_hashMul4 = 1360715025UL; //(LRM_hash_t)(c_hashMul*c_hashMul*c_hashMul*c_hashMul);
#endif

OODLE_NS_END
#include "lrminternal.h"
OODLE_NS_START

LRM_hash_t LRM_InitialHash( const U8 * ptr , int length)
{
	LRM_hash_t hash = 0;
	
	#ifdef LRM_DO_FORCE_HASH_LEN8
	
	RR_ASSERT( length == 8 );
	
	hash = LRM_Hash8(ptr);
	
	#else
	
	RR_ASSERT( length >= LRM_MIN_MATCH_LEN );
	
	if ( length == 8 )
	{
		#if 0
		
		RR_UNROLL_I_N(8,0,	hash *= c_hashMul; hash += ptr[i]; );
		
		#else
		
		// alternative way : do 4 at a time then merge
		
		LRM_hash_t h4_1 = LRM_InitialHash4(ptr);
		LRM_hash_t h4_2 = LRM_InitialHash4(ptr+4);
		hash = h4_1 * c_hashMul4 + h4_2;
		
		#endif
	}
	else
	{	
		for(int i=0;i<length;i++)
		{
			hash *= c_hashMul;
			hash += ptr[i];
		}
	}
	
	#endif
	
	return hash;
}

static LRM_hash_t intpow( LRM_hash_t val, int pow )
{
	LRM_hash_t ret = 1;
	while( pow > 0 )
	{
		pow--;
		ret *= val;
	}
	return ret;
}

static LRM_hash_t MakeMulEnd(int length)
{
	return intpow( c_hashMul, length-1 );
}


int LRM_GetHashLength(const LRM * lrm)
{
	return lrm->hash_length;
}

LRM_hash_t LRM_GetHashMulEnd(const LRM * lrm)
{
	return lrm->hash_mul_end;
}

LRM_hash_t LRM_InitialHash( const LRM * lrm, const U8 * ptr )
{
	return LRM_InitialHash(ptr, lrm->hash_length );
}

LRM_hash_t LRM_RollHashPtr( const LRM * lrm, LRM_hash_t hash, const U8 * ptr )
{
	return LRM_RollHashPtr(hash,lrm->hash_mul_end,ptr,lrm->hash_length);
}

#define USE_BLOOM

//#define PRINT_BLOOM_STATS // not thread safe!
#define BLOOM_COL_SHIFT	(9)	// "columns" of 512 bits (64 bytes)
#define BLOOM_COL_SIZE	(1 << BLOOM_COL_SHIFT) // COL_SIZE is in bits
#define BLOOM_COL_MASK 	(BLOOM_COL_SIZE - 1)
#define BLOOM_COL_WORDS	(BLOOM_COL_SIZE / 32)
#define BLOOM_NHASH		(3) // at the 4-8 bits/entry we aim at, this is appropriate

// Blocked Bloom filter a la "Cache-, Hash- and Space-Efficient Bloom Filters"

static inline void bloom_insert(U32 * bloom_filter, int bloom_row_shift, LRM_hash_t hash)
{
	U32 * bloom_row = bloom_filter + (hash >> bloom_row_shift) * BLOOM_COL_WORDS;

	// "Cache-, Hash- and Space-Efficient Bloom Filters" cites another paper according to
	// which you don't need k independent hash functions; k linear combinations of two
	// hash functions is fine.
	//
	// So... can we get away with just using different bits of our hash value (we scramble
	// it once more forming "mixed" so we don't have trivial aliasing with the row select
	// bits for large filters) and then using the Very Much Linear Combinations
	//
	//   h0 + 0*h1
	//   h0 + 1*h1
	//   h0 + 2*h2
	//
	// etc.? It sure looks like we can!
	U32 mixed = hash * 3677199193u;
	U32 counter = mixed;
	U32 col_step = mixed >> (32 - BLOOM_COL_SHIFT);

	for (int i = 0; i < BLOOM_NHASH; i++)
	{
		U32 column = counter & BLOOM_COL_MASK;
		U32 mask = 1u << (column & 31);
		bloom_row[column >> 5] |= mask;

		counter += col_step;
	}
}

static inline bool bloom_maybe_contains(const U32 * bloom_filter, int bloom_row_shift, LRM_hash_t hash)
{
	const U32 * bloom_row = bloom_filter + (hash >> bloom_row_shift) * BLOOM_COL_WORDS;
	U32 mixed = hash * 3677199193u;
	U32 counter = mixed;
	U32 col_step = mixed >> (32 - BLOOM_COL_SHIFT);

	// all accesses are to the same cache line, so avoid extraneous branches
	U32 maybe = 1;
	for (int i = 0; i < BLOOM_NHASH; i++)
	{
		U32 column = counter & BLOOM_COL_MASK;
		maybe &= bloom_row[column >> 5] >> (column & 31);
		counter += col_step;
	}

	return maybe != 0;
}

void LRM::FreeBloom()
{
	if (bloom_filter)
	{
		OODLE_FREE_ARRAY(bloom_filter, (1 << (32 - bloom_row_shift)) * BLOOM_COL_WORDS);
		bloom_filter = 0;
	}
}

LRM::~LRM()
{
	FreeBloom();
}

static void LRM_MakeJump( LRM * lrm , int jumpBits )
{
	LRMEntry * entries = lrm->entries.data();
	S32 numEntries = lrm->entries.size32() - 1;

	// jumpBits is not stored, jumpInShift is used to infer it
			
	if ( jumpBits > 0 && numEntries > 4 )
	{	
		int jumpInShift = LRM_HASH_BITS - jumpBits;
		lrm->jumpInShift = jumpInShift;
		
		int jumpSize = 1<<jumpBits;
		lrm->jumpIn.resize(jumpSize + 1);
		S32 * jumpIn = lrm->jumpIn.data();
		
		// jumpIn[j] points at the first entry whole hash is >= my top bits
		
		S32 hash_index = 0;
		for(int j=0;j<jumpSize;j++)
		{
			LRM_hash_t jump_hash = ((LRM_hash_t)j)<<jumpInShift;
			
			while( hash_index < numEntries && entries[hash_index].hash < jump_hash )
			{
				hash_index++;
			}
			
			jumpIn[j] = hash_index;
		}
		lrm->jumpIn[jumpSize] = numEntries;
	}
	else
	{
		//jumpBits = 0; // force to zero if numEntries was tiny
		//int jumpSize = 1<<jumpBits; // jumpSize = 1
		lrm->jumpInShift = LRM_HASH_BITS;
		lrm->jumpIn.resize(2); // 2 == jumpSize+1
		lrm->jumpIn[0] = 0;
		lrm->jumpIn[1] = numEntries; // [jumpSize]
	}

#ifdef USE_BLOOM
	lrm->FreeBloom();
	
	// is numEntries big enough to care about bloom filter?
	// if the whole LRM fits in cache, there's no point
	if ( numEntries > 4*1024 )
	{
		// aim for around 4-8 bits per entry
		int bloom_size_log2 = (31 + 3/*target 8 bits/entry*/ - BLOOM_COL_SHIFT/*compensate for bits per column*/) - rrClz32(numEntries);
		RR_ASSERT_ALWAYS( bloom_size_log2 > 0 );
		U32 bloom_nrows = 1u << bloom_size_log2;
		int bloom_shift = 32 - bloom_size_log2;

		//rrprintf("bloom bits per entry: %.2f\n", 1.0 * bloom_nrows * BLOOM_COL_SIZE / numEntries);

		lrm->bloom_filter = OODLE_MALLOC_ARRAY_CACHEALIGNED(U32, bloom_nrows * BLOOM_COL_WORDS);
		lrm->bloom_row_shift = bloom_shift;

		memset(lrm->bloom_filter, 0, bloom_nrows * BLOOM_COL_WORDS * sizeof(U32));
		for (S32 i = 0; i < numEntries; i++)
			bloom_insert(lrm->bloom_filter, bloom_shift, entries[i].hash);
	}
#endif
}

static void LRM_DeDupe(LRM * lrm)
{
	#ifdef LRM_NUM_DUPES
	LRMEntry * entries = lrm->entries.data();
	int numEntries = lrm->entries.size32();
	
	//if ( numEntries > 2 )
	if ( numEntries > LRM_NUM_DUPES*2 )
	{
		// unique

		// skip prefix where all are unique :
		int srci= 1;
		while( srci<numEntries && entries[srci].hash != entries[srci-1].hash )
		{
			srci++;
		}
		
		if ( srci != numEntries )
		{
			// srci and srci-1 match
		
			int dsti=srci-1;
			// entries[dsti] has this hash
			for(;;)
			{			
				// starting a block of dupes
				RR_ASSERT( srci != dsti );
				LRM_hash_t cur_hash = entries[dsti].hash;
				RR_ASSERT( cur_hash == entries[srci].hash );
				
				
				#if LRM_NUM_DUPES > 0
				int first_dupe = srci;	
				SINTa last_dupe = RR_MIN( numEntries, first_dupe + LRM_NUM_DUPES );
				while ( srci < last_dupe && entries[srci].hash == cur_hash )
				{
					dsti++;
					entries[dsti] =	entries[srci];
					srci++;
				}
				#else
				++srci;
				#endif
								
				// take highest pos :
				//entries[dsti].pos = RR_MAX(entries[dsti].pos,entries[srci].pos);
						
				while ( srci < numEntries && entries[srci].hash == cur_hash )
				{
					// take highest pos :
					//entries[dsti].pos = RR_MAX(entries[dsti].pos,entries[srci].pos);
					srci++;
				}
				
				// flag that this one has a next level :
				//entries[dsti].pos |= U32_TOP_BIT;
								
				// now entries[srci] is a unique one
				if ( srci == numEntries )
					break;
					
				dsti++;
				entries[dsti] =	entries[srci];
				srci++;
				while ( srci < numEntries && entries[srci].hash != entries[dsti].hash )
				{
					dsti++;
					entries[dsti] =	entries[srci];
					srci++;
				}
				
				// hash[srci] == hash[dsti]
				
				if ( srci == numEntries )
					break;
			}
			
			int newCount = dsti+1;
			RR_ASSERT( newCount <= lrm->entries.size32() );
			lrm->entries.resize(newCount);
			lrm->entries.tighten();
		}
	}
	#endif
}
	
LRM * LRM_Fill(LRM * lrm, const U8 * buffer, SINTa bufSize, int step, int jumpBits, int hash_length)
{
	THREADPROFILEFUNC();
	
	RR_ASSERT_ALWAYS( bufSize >= hash_length );
	RR_ASSERT_ALWAYS( hash_length >= LRM_MIN_MATCH_LEN );
	// not strictly required, don't assert :
	// generally LRM is intended for smaller chunks :
	//RR_ASSERT_ALWAYS( bufSize <= 32*1024*1024 );
	RR_ASSERT_ALWAYS( bufSize <= (1<<30) ); // most fit in 32 bit
	
	SINTa numEntries = 1 + (bufSize-hash_length)/step;

	lrm->chunkBasePtr = buffer;
	lrm->matchBasePtr = buffer;
	lrm->bufSize = bufSize;
	lrm->hash_length = hash_length;
	lrm->hash_mul_end = MakeMulEnd(hash_length);
	
	lrm->entries.resize(numEntries+1);
	LRMEntry * entries = lrm->entries.data();
	int ei = 0;
	
	LRM_hash_t last = (LRM_hash_t)-1;
	for(SINTa pos=0; pos <= (bufSize-hash_length); pos += step)
	{
		LRM_hash_t hash = LRM_InitialHash(buffer+pos,hash_length);
		
		// don't add a bunch of the same hash in a degenerate area ?
		if ( hash == last )
			continue;
		last = hash;
		
		entries[ei].hash = hash;
		//entries[ei].firstfour = RR_GET32_NATIVE(buffer+pos);
		entries[ei].pos32 = check_value_cast<U32>( pos );
		ei++;		
	}
	
	//RR_ASSERT( ei == numEntries );
	RR_ASSERT( ei <= numEntries );
	numEntries = ei;
	// size is 1 more than numEntries for sentry
	lrm->entries.resize(numEntries+1);
	RR_ASSERT( entries == lrm->entries.data() );

	//std::sort(entries,entries+numEntries);
	stdsort(entries,entries+numEntries);
		
	// entries sorted by hash
	
	if ( 0 ) // get some stats
	{		
		int num_hashes_equal = 0;
		int num_hash_collisions = 0;
		
		for(int i=1;i<numEntries;i++)
		{
			if ( entries[i].hash == entries[i-1].hash )
			{
				num_hashes_equal++;
				if ( memcmp(buffer+entries[i].pos32,buffer+entries[i-1].pos32,hash_length) != 0 )
				{
					num_hash_collisions++;
				}
			}
		}
		
		rrPrintf_v0("LRM : entries : %d, hashes equal : %d (%.2f%%), collisions : %d (%.2f%%)\n",
			numEntries,
			num_hashes_equal,(num_hashes_equal*100.f)/numEntries,
			num_hash_collisions,(num_hash_collisions*100.f)/numEntries);
			
		/*
		
		lzt99 :
		
		LRM : entries : 209715, hashes equal : 31 (0.01%), collisions : 6 (0.00%)
		LRM : entries : 209703, hashes equal : 26383 (12.58%), collisions : 5 (0.00%)
		LRM : entries : 202381, hashes equal : 53464 (26.42%), collisions : 1 (0.00%)
		LRM : entries : 196491, hashes equal : 14829 (7.55%), collisions : 6 (0.00%)
		LRM : entries : 209711, hashes equal : 21497 (10.25%), collisions : 2 (0.00%)
		LRM : entries : 209710, hashes equal : 7967 (3.80%), collisions : 2 (0.00%)
		LRM : entries : 201199, hashes equal : 32935 (16.37%), collisions : 4 (0.00%)
		LRM : entries : 207139, hashes equal : 6946 (3.35%), collisions : 5 (0.00%)
		LRM : entries : 195717, hashes equal : 37073 (18.94%), collisions : 1 (0.00%)
		LRM : entries : 192550, hashes equal : 26586 (13.81%), collisions : 2 (0.00%)

		enwik8 :

		LRM : entries : 838803, hashes equal : 115068 (13.72%), collisions : 70 (0.01%)
		LRM : entries : 838709, hashes equal : 116270 (13.86%), collisions : 65 (0.01%)
		LRM : entries : 838638, hashes equal : 114989 (13.71%), collisions : 65 (0.01%)
		LRM : entries : 838464, hashes equal : 116835 (13.93%), collisions : 56 (0.01%)
		LRM : entries : 838813, hashes equal : 116574 (13.90%), collisions : 61 (0.01%)
		LRM : entries : 838821, hashes equal : 115585 (13.78%), collisions : 50 (0.01%)
		LRM : entries : 838743, hashes equal : 112040 (13.36%), collisions : 71 (0.01%)
		LRM : entries : 838808, hashes equal : 116818 (13.93%), collisions : 63 (0.01%)
		LRM : entries : 838803, hashes equal : 118611 (14.14%), collisions : 59 (0.01%)
		LRM : entries : 1258241, hashes equal : 189038 (15.02%), collisions : 127 (0.01%)

		*/
	}
		
	// copy pos of last for sentry :
	LRMEntry sentry = entries[numEntries - 1];
	sentry.hash = (LRM_hash_t)-1;
	entries[numEntries] = sentry;
	
	LRM_DeDupe(lrm);
		
	LRM_MakeJump(lrm,jumpBits);

	// this is a race :	
	//lrm->creation = 0;
	
	return lrm;
}

LRM * LRM_FillSeveral(LRM ** lrmArray,
						const U8 * bufStart, const U8 * bufEnd, SINTa chunkSize,
						int step, int jumpBits, int hash_length)
{
	const U8 * bufPtr = bufStart;
	
	for(int c=0;bufPtr < bufEnd;c++)
	{
		if ( bufPtr + chunkSize > bufEnd )
		{
			chunkSize = rrPtrDiff(bufEnd - bufPtr);
		}
		
		LRM_Fill(lrmArray[c],bufPtr,chunkSize,step,jumpBits,hash_length);
		bufPtr += chunkSize;
	}

	return lrmArray[0];
}

LRM * LRM_Create(const U8 * buffer, SINTa bufSize, int step, int jumpBits, int hash_length)
{
	LRM * lrm = OodleNew(LRM);
	
	LRM_Fill(lrm,buffer,bufSize,step,jumpBits,hash_length);
	
	return lrm;
}

void LRM_Destroy(LRM * lrm)
{
	OodleDelete(lrm);
}

//===========================================

S32 LRM_FindMatch(const LRM * lrm,LRM_hash_t hash, const U8 *ptr, const U8 *ptrend,
					SINTa * pMatchOffset, SINTa matchOffsetLimit)
{

	RR_ASSERT( LRM_InitialHash(lrm,ptr) == hash );

	if ( rrPtrDiff(ptrend - ptr) < LRM_MIN_MATCH_LEN )
		return 0;

#ifdef USE_BLOOM
	if ( lrm->bloom_filter )
	{
		if ( !bloom_maybe_contains(lrm->bloom_filter, lrm->bloom_row_shift, hash) )
			return 0;
	}
#endif
		
	// @@ another option is just to skip the jumpIn[]
	//	and go straight to interpolation
	//	that would be great except for degeneracies
	//	though if I remove dupe hashes that might be fine

	// look for hash :
	
	RR_ASSERT( lrm->jumpIn.size() > 0 );
	
	//U32 firstfour = RR_GET32_NATIVE(ptr);
	
	// @@ CB 09-06-2019 : FIX CRASH
	// jumpInShift can be 32
	// hash is 32 bit
	// must promote to 64-bit to get a proper shift
	
	//LRM_hash_t ji = hash >> (lrm->jumpInShift);
	U64 ji64 = ( (U64)hash ) >> (lrm->jumpInShift);
	SINTa ji = (SINTa)ji64;
	S32 jump1 = lrm->jumpIn[ji];
	
	const LRMEntry * entries = lrm->entries.data();
	S32 numEntries = lrm->entries.size32();
	
	RR_ASSERT( jump1 < numEntries );
	
	LRM_hash_t hash1 = entries[jump1].hash;
	if ( hash1 > hash ) return 0;
	
	RR_ASSERT( jump1 < numEntries-1 );
	RR_ASSERT( entries[jump1+1].hash >= hash1 );
	
	const U8 * matchBasePtr = lrm->matchBasePtr;
		
	S32 best_ml = 0;
	SINTa best_off = matchOffsetLimit;
	
	/*
	// @@
	// this interpolation multiple could be precomputed and stored in the hash table :
	
	S32 jump2 = lrm->jumpIn[ji+1];
	LRM_hash_t hash2 = entries[jump2].hash;
	RR_ASSERT( hash2 >= hash1 && hash2 >= hash );
		
	// interpolation :
	S32 i = ((jump2 - jump1)*(hash - hash1))/ (hash2 - hash1);
	S32 i =  (hash - hash1) * ((jump2 - jump1) * 2^32 )/ (hash2 - hash1) / 2^32;
	U32 scale = (jump2 - jump1) * 2^32 )/ (hash2 - hash1);
	S32 i = (hash - hash1) * scale / 2^32;
	S32 i = rrMul32High( (hash - hash1) , scale )
	*/

	/*
	// linear search forward :
	int i = jump1;
	while(i<numEntries)
	{
		if ( entries[i].hash > hash )
		{
			break;
		}
		else if ( entries[i].hash == hash )
		{
			// found it 
			const U8 * vs = buffer + entries[i].pos;
			S32 ml = getmatchlen_mml8(ptr,vs,ptrend);
			if ( ml > best_ml )
			{
				best_ml = ml;
				best_off = rrPtrDiff32(ptr - vs);
			}
		}
		
		i++;		
	}
	/**/
	
	//*
	// binary search in the jump interval :
	
	numEntries;
	
	S32 jump2 = lrm->jumpIn[ji+1];
	RR_DURING_ASSERT( LRM_hash_t hash2 = entries[jump2].hash );
	RR_ASSERT( (hash2 > hash1 && hash2 > hash) || (jump2 == lrm->jumpIn.size32()-1 && (hash+1) == 0 ) );

#if 1
	const LRMEntry * it = entries + jump1;
	size_t nleft = jump2 - jump1;

	if (nleft > 0)
	{
		// The design of this type of search is elaborated on in
		//  https://arxiv.org/abs/1509.05053

		// Careful branch-free binary search. Invariants:
		// 1. everything in [begin, it) is <hash
		// 2. everything in [it + nleft, end) is >=hash
		//
		// This code is carefully written so the only thing depending on the hash comparison can
		// be expressed as a conditional move, and the computation for "half" etc. has no data
		// dependencies.
		while (size_t half = nleft >> 1) // Loop while >1 elements properly inside [it, it + nleft)
		{
			const LRMEntry * mid = &it[half];

			// Prefetch both possible paths for the next step
			RR_PREFETCHR_CL(&it[half >> 1]);
			RR_PREFETCHR_CL(&mid[half >> 1]);

			// This reduction guarantees the two invariants above. It doesn't shrink nleft quite
			// as quickly as it could but it has the advantage of being a very simple update.
			//
			// Specifically, if (mid->hash < hash), we _could_ set it = mid + 1 and subtract
			// (half + 1) from nleft, but keeping "mid" itself inside the interval makes the
			// update rule slightly cheaper, even though it's a bit sloppy.
			it = (mid->hash < hash) ? mid : it; // conditional move
			nleft -= half; // nleft = (nleft >> 1) + ((nleft + 1) >> 1), so nleft - half = (nleft + 1) >> 1
		}

		// The above loop shrank nleft down to 1. Do the final step.
		RR_ASSERT(nleft == 1);
		it += (it->hash < hash);

	#ifdef PRINT_BLOOM_STATS
		static int nsearch = 0;
		static int nfound = 0;
		nfound += (it->hash == hash);
		if ((++nsearch & 65535) == 0)
			rrprintf("nsearch=%6d found=%.2f%%\n", nsearch, 100.0f*nfound/nsearch);
	#endif
	}
#else
	const LRMEntry * it = lower_bound(entries+jump1,entries+jump2,hash,LRMEntry_Hash_Less());
#endif
	RR_ASSERT( it->hash >= hash );
	RR_ASSERT( it == entries || it[-1].hash < hash );
	// then linear search forward :
	while( it->hash == hash )
	{
		//if ( it->firstfour == firstfour )
		{
			// found it 
			const U8 * vs = matchBasePtr + it->pos32;
			RR_ASSERT( vs < ptr );
			//RR_ASSERT( LRM_InitialHash(ptr) == hash );
			//@@ todo : check best_ml first
			S32 ml = getmatchlen_lrm(ptr,vs,ptrend);
			if ( ml >= best_ml )
			{
				RR_ASSERT( memcmp(ptr,vs,ml) == 0 );
			
				SINTa offset = rrPtrDiff(ptr - vs );
				RR_ASSERT( offset >= 1 );
				if ( offset < best_off )
				{		
					best_ml = ml;
					best_off = offset;
					
					if ( best_ml >= LRM_GOOD_ENOUGH_LEN )
					{
						*pMatchOffset = best_off;
						return best_ml;
					}
				}
			}
		}
		
		++it;
	}
	/**/
		
	*pMatchOffset = best_off;
	return best_ml;
}

// tot : 88353265

LRM * LRM_FillMerge(LRM * lrm,const LRM * lhs, const LRM * rhs,int jumpBits)
{
	THREADPROFILEFUNC();
	
	lrm->hash_length = lhs->hash_length;
	RR_ASSERT( rhs->hash_length == lhs->hash_length );
	lrm->hash_mul_end = lhs->hash_mul_end;

	RR_ASSERT( lhs->chunkBasePtr < rhs->chunkBasePtr );
	RR_ASSERT( lhs->chunkBasePtr + lhs->bufSize == rhs->chunkBasePtr );
	SINTa rhs_add = rrPtrDiff( rhs->chunkBasePtr - lhs->chunkBasePtr );

	lrm->chunkBasePtr = lhs->chunkBasePtr;
	lrm->bufSize = rhs_add + rhs->bufSize;

	// positions relative to matchBasePtr must fit in U32
	#ifdef __RAD64__
	if ( lrm->bufSize >= (((SINTa)1)<<31) )
	{
		// range is too big to put both in a U32
		
		// as of 01-03-2013 , I believe this never happens
		//	because base_chunkSize is 4M
		//	and num levels is 8
		//	the biggest bufSize is 4*256 = 1 G
	
		// just copy in the RHS entries and do not get any LHS
		lrm->matchBasePtr = rhs->matchBasePtr;
		lrm->entries.assignv( rhs->entries );
		LRM_MakeJump(lrm,jumpBits);
		return lrm;	
	}
	else
	#endif
	{
		lrm->matchBasePtr = lrm->chunkBasePtr;
	}
	
	U32 rhs_add32 = check_value_cast<U32>(rhs_add);
	
	const LRMEntry * lhs_entries = lhs->entries.data();	
	S32 lhs_numEntries = lhs->entries.size32() - 1; // one dummy
	
	RR_ASSERT( lhs_entries[lhs_numEntries].hash+1 == 0 );
	
	const LRMEntry * rhs_entries = rhs->entries.data();	
	S32 rhs_numEntries = rhs->entries.size32() - 1; // one dummy

	RR_ASSERT( rhs_entries[rhs_numEntries].hash+1 == 0 );
	
	S32 tot_numEntries = lhs_numEntries + rhs_numEntries;
	lrm->entries.resize( tot_numEntries + 1 ); // one dummy
	
	LRMEntry * entries = lrm->entries.data();	
	
	int lhsi = 0;
	int rhsi = 0;

	// merge :
	// (I bet this could be faster)

	// funny end of loop condition handles degenerate sentry case
	int desti = 0;
	for(;;)
	{
		// no need to check for end of array cuz of sentries
		// == numEntries is the sentry
		RR_ASSERT( lhsi <= lhs_numEntries );
		RR_ASSERT( rhsi <= rhs_numEntries );
		
		#if LRM_DO_DEDUPE_IN_MERGE
		// de-dupe in merge; if hash is same in both, just take one
		if ( lhs_entries[lhsi].hash == rhs_entries[rhsi].hash )
		{
			//entries[desti] = lhs_entries[lhsi];
			entries[desti].hash = rhs_entries[rhsi].hash;
			entries[desti].pos  = rhs_entries[rhsi].pos + rhs_add;
			desti++;
			lhsi++; rhsi++;
			if ( lhsi == lhs_numEntries )
				break;
			if ( rhsi >= rhs_numEntries )
				break;
		}
		else if ( lhs_entries[lhsi].hash < rhs_entries[rhsi].hash )
		#else
		if ( lhs_entries[lhsi].hash <= rhs_entries[rhsi].hash )
		#endif
		{
			entries[desti] = lhs_entries[lhsi];
			lhsi++; desti++;
			if ( lhsi >= lhs_numEntries )
				break;
		}
		else
		{
			entries[desti].hash = rhs_entries[rhsi].hash;
			//entries[desti].firstfour = rhs_entries[rhsi].firstfour;
			entries[desti].pos32  = rhs_entries[rhsi].pos32 + rhs_add32;
			rhsi++; desti++;
			if ( rhsi >= rhs_numEntries )
				break;
		}
	}
	while(rhsi<rhs_numEntries)
	{
		entries[desti].hash = rhs_entries[rhsi].hash;
		//entries[desti].firstfour = rhs_entries[rhsi].firstfour;
		entries[desti].pos32  = rhs_entries[rhsi].pos32 + rhs_add32;
		desti++;
		rhsi++;
	}
	while(lhsi<lhs_numEntries)
	{
		entries[desti] = lhs_entries[lhsi];
		lhsi++; desti++;
	}
	
	lrm->entries.resize(desti);
	
	// sentry :
	lrm->entries.push_back( lhs_entries[lhs_numEntries] );

	#if ! LRM_DO_DEDUPE_IN_MERGE
	// if LRM_DO_DEDUPE_IN_MERGE is off
	//	still do basic DeDupe
	LRM_DeDupe(lrm);
	#endif

	LRM_MakeJump(lrm,jumpBits);
	
	// this is a race :
	//lrm->creation = 0;

	return lrm;
}

LRM * LRM_FillMergeSeveral(LRM ** lrms,LRM ** prev_lrms, SINTa clo, SINTa cnum, int jumpBits)
{
	THREADPROFILEFUNC();

	for(SINTa c=clo;c<(clo+cnum);c++)
	{
		const LRM * lhs = prev_lrms[ 2*c + 0 ];
		const LRM * rhs = prev_lrms[ 2*c + 1 ];
		
		LRM_FillMerge(lrms[c], lhs,rhs, jumpBits);
		
		if ( 1 )
		{
			// I can free prev_lrms[2*c+1] now
			// it's a Fenwickish binary tree thing
				
			// risk of a race here ?
			LRM_Destroy( prev_lrms[ 2*c + 1 ] );
			prev_lrms[ 2*c + 1 ] = NULL;
		}
	}
	
	return lrms[clo];
}
				
LRM * LRM_CreateMerge(const LRM * lhs, const LRM * rhs,int jumpBits)
{
	
	LRM * lrm = OodleNew(LRM);

	LRM_FillMerge(lrm,lhs,rhs,jumpBits);

	return lrm;
}
//=========================================================

/*

LRMSet is just a linear list of LRM's

the first in the set is the closest (lowest offset)
so we search from front to back and use non-equal compare

*/

S32 LRMSet_FindMatch(const LRMSet * set,LRM_hash_t hash, const U8 *ptr, const U8 *ptrend,
					SINTa * pMatchOffset, SINTa matchOffsetLimit)
{
	S32 best_len = 0;
	SINTa best_off = 0;

	// LRM off completely : 4.10 mb/s

	// @@ speed test : just return 0 ; 3.93 mb/s
	//return 0;
	
	// @@ speed test : just do first LRM - does help, but not a ton (2.74 mb/s vs 2.5 mb/s)
	//int i = 0;
	//if ( ! set->lrms.empty() )
	
	// lookup all : 2.5 mb/s
	for LOOPVEC(i,set->lrms)
	{
		S32 len;
		SINTa off;
		len = LRM_FindMatch(set->lrms[i],hash,ptr,ptrend,&off,matchOffsetLimit);
		if ( len > best_len )
		{
			best_len = len;
			best_off = off;
		}
	}
	
	*pMatchOffset = best_off;
	return best_len;
}

const U8 * LRMSet_GetBasePtr(const LRMSet * set)
{
	if ( set->lrms.empty() )
		return NULL;
	LRM * lrm = set->lrms.back();
	return lrm->chunkBasePtr;
}

const U8 * LRMSet_GetEndPtr(const LRMSet * set)
{
	if ( set->lrms.empty() )
		return NULL;
	LRM * lrm = set->lrms[0];
	return lrm->chunkBasePtr + lrm->bufSize;
}
					
					
//=============================================================
/*

LRMCascade is a binary tree of LRM's like :

	[Y][Y][Y][N]
    [ Y  ][  N ]
    
*/


SINTa LRM_GetCascadeChunkSize(const LRMCascade * casc)
{
	return casc->chunkSize;
}

LRMCascade * LRM_AllocCascade()
{
	LRMCascade * casc = OodleNew(LRMCascade);
	return casc;
}

static void LRM_FillCascadeSetup(LRMCascade * casc,const U8 * buffer, SINTa bufSize, int step, int jumpBits_0, int jumpBits_inc,
						SINTa base_chunkSizeA, int hash_length)
{
	casc->buffer = buffer;
	casc->bufSize = bufSize;
	
	S32 base_chunkSize = S32_checkA(base_chunkSizeA);
	casc->chunkSize = base_chunkSize;

	rrPrintf_v2("LRM base_chunkSize : %d ; bufSize = %d\n",(int)base_chunkSize,(int)bufSize);

	// bufSize should be a multiple of chunkSize :
	RR_ASSERT( ((bufSize/base_chunkSize)*base_chunkSize) == bufSize );
	
	casc->numChunks = bufSize / base_chunkSize;
	casc->nextChunk = 0;

	// Reserve space for the cascade chunk pointers at every level
	for (int level = 0; level < LRM_CASCADE_MAX_LEVELS; level++)
	{
		casc->lrms[level].clear();
		casc->lrms[level].resize(casc->numChunks >> level, 0);
	}

	casc->step = step;
	casc->jumpBits_0 = jumpBits_0;
	casc->jumpBits_inc = jumpBits_inc;
	casc->hash_length = hash_length;
}

static bool LRM_FillCascadeIncrement(LRMCascade * casc)
{
	if ( casc->nextChunk >= casc->numChunks )
		return false;

	// Make the level-0 LRM for this chunk
	SINTa chunk = casc->nextChunk++;
	SINTa chunkStart = chunk * casc->chunkSize;

	LRM * cur_lrm = OodleNew(LRM);
	LRM_Fill(cur_lrm, casc->buffer + chunkStart, casc->chunkSize, casc->step, casc->jumpBits_0, casc->hash_length);

	// Merge upwards while we can
	// every trailing 1 bit in the chunk index corresponds to one level to merge.
	int curLevel = 0;
	while (((chunk >> curLevel) & 1) == 1 && curLevel + 1 < LRM_CASCADE_MAX_LEVELS)
	{
		// we can merge, so go ahead!
		LRM * merged_lrm = OodleNew(LRM);
		const LRM * lhs = casc->lrms[curLevel][(chunk >> curLevel) - 1]; // previous LRM at this level

		int jumpBits = casc->jumpBits_0 + curLevel * casc->jumpBits_inc;
		LRM_FillMerge( merged_lrm, lhs, cur_lrm, jumpBits );

		// can free RHS lrm now, we will only ever use the combined version
		LRM_Destroy( cur_lrm );

		// the new (merged) LRM is our new current
		cur_lrm = merged_lrm;
		curLevel++;
	}

	// Store the cur lrm at whatever level we ended up with
	casc->lrms[curLevel][chunk >> curLevel] = cur_lrm;

	return true;
}

void LRM_FillCascade(LRMCascade * casc,const U8 * buffer, SINTa bufSize, int step, int jumpBits_0, int jumpBits_inc,
						SINTa base_chunkSizeA, int hash_length)
{
	THREADPROFILEFUNC();

/*
#ifdef OODLE_BUILDING_DATA
#ifndef OODLE_BUILDING_DLL
	OodleXMalloc_LogMemUse("LRM_FillCascade start: ");
#endif
#endif
*/

	LRM_FillCascadeSetup(casc,buffer,bufSize,step,jumpBits_0,jumpBits_inc,base_chunkSizeA,hash_length);

	while ( LRM_FillCascadeIncrement(casc) )
	{
	}

// log mem use here
	#if 0
	// Oodle2 Core can't call rrLogGetVerboseLevel
	//if ( rrLogGetVerboseLevel() >= 2 )
	if ( 1 )
	{
		rrprintf("\n");
		// log memory use :
		for(int level = 0;level<LRM_CASCADE_MAX_LEVELS;level++)
		{
			LRM ** lrms = casc->lrms[level].data();
			S32 numChunks = casc->lrms[level].size32();
			rrprintf("l%2d : ",level);
			rrprintf("%5d chunks :",numChunks);
			SINTa count = 0,nz=0;
			for(int c=0;c<numChunks;c++)
			{
				if ( lrms[c] )
				{
					nz++;
					count += lrms[c]->entries.size();
				}
			}
			rrprintf("%5d nz :",nz);
			rrprintf(RR_S64_FMT , (S64)(count * sizeof(LRMEntry)));
			rrprintf(" bytes\n");
		}
	}
	#endif

/*
#ifdef OODLE_BUILDING_DATA
#ifndef OODLE_BUILDING_DLL
	OodleXMalloc_LogMemUse("LRM_FillCascade end: ");
#endif
#endif
*/
}


LRMCascade * LRM_CreateCascade(const U8 * buffer, SINTa bufSize, int step, int jumpBits_0, int jumpBits_inc,
						SINTa base_chunkSizeA, int hash_length)
{
	LRMCascade * casc = LRM_AllocCascade();
	
	LRM_FillCascade(casc,buffer,bufSize,step,jumpBits_0,jumpBits_inc,base_chunkSizeA,hash_length);
	
	return casc;
}

LRMCascade * LRM_CreateCascadeIncremental(const U8 * buffer, SINTa bufSize, int step, int jumpBits_0, int jumpBits_inc,
						SINTa base_chunkSizeA, int hash_length)
{
	LRMCascade * casc = LRM_AllocCascade();

	LRM_FillCascadeSetup(casc,buffer,bufSize,step,jumpBits_0,jumpBits_inc,base_chunkSizeA,hash_length);

	return casc;
}

void LRM_DestroyCascade(LRMCascade * casc)
{
	for(int level = 0;level<LRM_CASCADE_MAX_LEVELS;level++)
	{
		for LOOPVEC(i,casc->lrms[level])
		{
			if ( casc->lrms[level][i] )
				LRM_Destroy(casc->lrms[level][i]);
		}
	}	
	
	OodleDelete(casc);
}

static void LRM_CascadeGetSetByChunkIndex(const LRMCascade * casc, LRMSet * set, int chunkIndex)
{
	RR_ASSERT( set->lrms.empty() );
	
	if ( chunkIndex == 0 )
	{
		return;
	}

	// Increments must be caught up to this chunk before we try to retrieve it
	// if you're hitting this, you should be using the _UpdateIncremental versions of CascadeGetSet!
	RR_ASSERT( chunkIndex <= casc->nextChunk );

	int ci = chunkIndex;
	RR_ASSERT_ALWAYS( ! casc->lrms[0].empty() );

	RR_DURING_ASSERT( const U8 * bufBase = casc->buffer );
	RR_DURING_ASSERT( SINTa start = chunkIndex*casc->chunkSize );

	rrPrintf_v2("SET : %d : ",ci);

	for(int level = 0;level<LRM_CASCADE_MAX_LEVELS && ci != 0;level++)
	{
//		RR_DURING_ASSERT( const LRM * lrml0 = casc->lrms[level][0] );

		if ( ci & 1 )
		{
			LRM * cur = casc->lrms[level][ci-1];

			rrPrintf_v2("[%d,%d] ",level,ci-1);
						
			RR_ASSERT( set->lrms.empty() || (cur->chunkBasePtr + cur->bufSize) == set->lrms.back()->chunkBasePtr );
			RR_ASSERT( ! set->lrms.empty() || (cur->chunkBasePtr + cur->bufSize + casc->chunkSize) >= bufBase + start );

			set->lrms.push_back( cur );
		}
		ci >>= 1;
	}
	if ( ci != 0 )
	{
		int i = 2*ci - 2 - 1;
		int level = LRM_CASCADE_MAX_LEVELS-1;
		while ( i >= 0 )
		{
			LRM * cur = casc->lrms[level][i];

			rrPrintf_v2("[%d,%d] ",level,i);

			RR_ASSERT( set->lrms.empty() || (cur->chunkBasePtr + cur->bufSize) == set->lrms.back()->chunkBasePtr );
			RR_ASSERT( ! set->lrms.empty() || (cur->chunkBasePtr + cur->bufSize + casc->chunkSize) >= bufBase + start );
			
			set->lrms.push_back( cur );
			
			i--;
		}
	}

	rrPrintf_v2("\n");
}

static void LRM_CascadeFillUntil(LRMCascade * casc, int chunkIndex)
{
	// For incremental LRMs, run extra increments until we're caught up to
	// the point we want to read
	while ( chunkIndex > casc->nextChunk && casc->nextChunk < casc->numChunks )
		LRM_FillCascadeIncrement(casc);
}

static int LRM_CascadeGetChunkIndex_Align(const LRMCascade * casc, const U8 * startPtr, const U8 * parsePtr)
{
	RR_ASSERT( startPtr <= parsePtr );

	SINTa start = rrPtrDiff( startPtr - casc->buffer );
	if ( start <= 0 )
		return 0;

	// try align up :

	/*

	@@ NOTE : aligning up means that LRM can overlap the local dictionary!
	that should be okay but it's a bit ugly

	this occurs on the final plus-size chunk

	*/


	SINTa chunkUp = (start + casc->chunkSize-1 ) / casc->chunkSize;
	SINTa startUp = chunkUp * casc->chunkSize;

	// use startUp if it works
	if ( startUp <= rrPtrDiff( parsePtr - casc->buffer ) && startUp <= casc->bufSize )
		return S32_checkA(chunkUp);

	// else round down
	RR_ASSERT( start <= casc->bufSize );
	start = RR_MIN(start,casc->bufSize);
	SINTa chunkDown = start / casc->chunkSize;
	return S32_checkA( chunkDown );
}

static int LRM_CascadeGetChunkIndex(const LRMCascade * casc, const U8 * startPtr)
{
	SINTa start = rrPtrDiff( startPtr - casc->buffer );
	if ( start <= 0 )
		return 0;

	RR_ASSERT( start <= casc->bufSize );
	start = RR_MIN(start,casc->bufSize);

	int chunkIndex = S32_checkA(start / casc->chunkSize);
	RR_ASSERT( chunkIndex*casc->chunkSize == start );

	return chunkIndex;
}

void LRM_CascadeGetSet(const LRMCascade * casc, LRMSet * set, const U8 * startPtr)
{
	int chunkIndex = LRM_CascadeGetChunkIndex(casc,startPtr);
	LRM_CascadeGetSetByChunkIndex(casc,set,chunkIndex);
}

void LRM_CascadeGetSet_UpdateIncremental(LRMCascade * casc, LRMSet * set, const U8 * startPtr)
{
	int chunkIndex = LRM_CascadeGetChunkIndex(casc,startPtr);
	LRM_CascadeFillUntil(casc, chunkIndex);
	LRM_CascadeGetSetByChunkIndex(casc,set,chunkIndex);
}

void LRM_CascadeGetSet_Align(const LRMCascade * casc, LRMSet * set, const U8 * startPtr, const U8 * parsePtr)
{
	int chunkIndex = LRM_CascadeGetChunkIndex_Align(casc,startPtr,parsePtr);
	LRM_CascadeGetSetByChunkIndex(casc,set,chunkIndex);
}

void LRM_CascadeGetSet_Align_UpdateIncremental(LRMCascade * casc, LRMSet * set, const U8 * startPtr, const U8 * parsePtr)
{
	int chunkIndex = LRM_CascadeGetChunkIndex_Align(casc,startPtr,parsePtr);
	LRM_CascadeFillUntil(casc, chunkIndex);
	LRM_CascadeGetSetByChunkIndex(casc,set,chunkIndex);
}


/*
void LRMSet_PrintNoLF(const LRMSet * set)
{
	rrPrintf_v0("[set : 
}
*/

void LRMScanner_Init(LRMScanner * scanner, const LRMSet * set, const U8 * startPtr, const U8 * endPtr , SINTa matchOffsetLimit)
{
	RR_ZERO(*scanner);
	
	if ( set && ! set->lrms.empty()  )
	{
		scanner->lrmset = set;
		LRM * lrmfirst = set->lrms[0];
		RR_ASSERT_ALWAYS( lrmfirst != NULL );
		scanner->lrm_hashMulEnd = LRM_GetHashMulEnd(lrmfirst);
		scanner->lrm_hashLength = LRM_GetHashLength(lrmfirst);
		
		scanner->lrm_end_ptr = endPtr - scanner->lrm_hashLength;
		
		scanner->matchOffsetLimit = matchOffsetLimit;
		
		if ( startPtr < scanner->lrm_end_ptr )
		{
			scanner->lrm_hash = LRM_InitialHash(startPtr,scanner->lrm_hashLength);
		}
		else
		{
			// set end pointer so that checks against it will make us not use the LRM :
			scanner->lrm_end_ptr = NULL;
		}
		// 0 is fine :
		//scanner->lrm_last_ml_plus_ptr = 0;
		//scanner->lrm_last_off = 0;
	}
	else
	{
		// set end pointer so that checks against it will make us not use the LRM :
		scanner->lrm_end_ptr = NULL;
	}
}

S32 LRMScanner_FindMatchAndRoll(LRMScanner * scanner, const U8 *ptr, const U8 *ptrend,
					SINTa * pMatchOffset)
{
	if ( ptr <= scanner->lrm_end_ptr )
	{
		RR_ASSERT( scanner->lrmset != NULL );

		// try to carry forward the last LRM match :
		SINTa lrm_len;
		if ( scanner->lrm_last_ml_plus_ptr >= ptr + LRM_GOOD_ENOUGH_LEN )
		{
			// carry-forward was "good enough" , don't do a new search
		
			lrm_len = rrPtrDiff(scanner->lrm_last_ml_plus_ptr - ptr);
			RR_ASSERT( lrm_len >= LRM_GOOD_ENOUGH_LEN );

			*pMatchOffset = scanner->lrm_last_off;
		}
		else
		{
			lrm_len = LRMSet_FindMatch(scanner->lrmset,scanner->lrm_hash,ptr,ptrend,pMatchOffset,scanner->matchOffsetLimit);
		
			// see if carry-forward was better :
			
			if ( scanner->lrm_last_ml_plus_ptr > ptr + lrm_len )
			{
				lrm_len = rrPtrDiff(scanner->lrm_last_ml_plus_ptr - ptr);
				*pMatchOffset = scanner->lrm_last_off;
			}
		
			// store for carry-forward :
			scanner->lrm_last_ml_plus_ptr = lrm_len + ptr;
			scanner->lrm_last_off = *pMatchOffset;
		}
		
		if ( ptr < scanner->lrm_end_ptr )
		{
			LRMScanner_Roll(scanner,ptr);
		}
				
		return S32_checkA(lrm_len);
	}
	else
	{
		return 0;
	}
}

S32 LRMScanner_FindMatch(LRMScanner * scanner, const U8 *ptr, const U8 *ptrend,
					SINTa * pMatchOffset)
{
	if ( ptr <= scanner->lrm_end_ptr )
	{
		RR_ASSERT( scanner->lrmset != NULL );

		// try to carry forward the last LRM match :
		SINTa lrm_len;
		if ( scanner->lrm_last_ml_plus_ptr >= ptr + LRM_GOOD_ENOUGH_LEN )
		{
			lrm_len = rrPtrDiff(scanner->lrm_last_ml_plus_ptr - ptr);
			RR_ASSERT( lrm_len >= LRM_GOOD_ENOUGH_LEN );
			
			*pMatchOffset = scanner->lrm_last_off;
		}
		else
		{
			lrm_len = LRMSet_FindMatch(scanner->lrmset,scanner->lrm_hash,ptr,ptrend,pMatchOffset,scanner->matchOffsetLimit);
		
			// branch here is optional, you could just unconditionally store it
			//	but that causes more LHS
			if ( lrm_len > 0 )
			{
				scanner->lrm_last_ml_plus_ptr = lrm_len + ptr;
				scanner->lrm_last_off = *pMatchOffset;
			}
		}
				
		return S32_checkA(lrm_len);
	}
	else
	{
		return 0;
	}
}

//===========================================

// set can be NULL and makes a scanner which will return no matches
void LRMScannerWindowed_Init(LRMScannerWindowed * scanner,const LRMSet * set, const U8 * startPtr, const U8 * endPtr, SINTa matchOffsetLimit )
{
	LRMScanner_Init(scanner,set,startPtr,endPtr,matchOffsetLimit);
	
	scanner->lrm_base_ptr = startPtr;
	scanner->lrm_last_ptr = startPtr;

	if ( set )
	{		
		scanner->lrm_window_base = LRMSet_GetBasePtr(set);
	}
	else
	{
		scanner->lrm_window_base = NULL;
	}
	
	// do initial window fill ;
	for(int i=0;i<LRMSCANNER_WINDOW_SIZE;i++)
	{
		const U8 * ptr = startPtr + i;
		scanner->window_off[i] = 0;
		scanner->window_len[i] = LRMScanner_FindMatchAndRoll(scanner,ptr,endPtr,&scanner->window_off[i]);
	}
}

S32 LRMScannerWindowed_FindMatchAndRoll(LRMScannerWindowed * scanner, const U8 *ptr, const U8 *ptrend,
					SINTa * pMatchOffset)
{
	// check sequential scan :
	RR_ASSERT( ptr == scanner->lrm_last_ptr );
	scanner->lrm_last_ptr++;
	
	int window_i = (int)( rrPtrDiff(ptr - scanner->lrm_base_ptr) & (LRMSCANNER_WINDOW_SIZE-1) );
	
	// just return what's in the window :
	S32 & window_len = scanner->window_len[window_i];
	SINTa & window_off = scanner->window_off[window_i];
	
	// check it :
	//RR_ASSERT( window_len == 0 || getmatchlen_debug(ptr,ptr - window_off,ptrend) >= window_len );
		
	// copy out the return before we replace it :
	S32 ret = window_len;
	*pMatchOffset = window_off;
	
	// find at the end of the window :
	//	replace current :
	const U8 * window_ptr = ptr+LRMSCANNER_WINDOW_SIZE;
	window_len = LRMScanner_FindMatchAndRoll(scanner,window_ptr,ptrend,&window_off);
	
	if ( window_len > 0 )
	{
		const U8 * vs_ptr = window_ptr - window_off;
		//RR_ASSERT( getmatchlen_debug(window_ptr,vs_ptr,ptrend) >= window_len );
		
		const U8 * lrm_dic_start_ptr = scanner->lrm_window_base;
		
		// try to extend back the match :
		for(int back=1;back<LRMSCANNER_WINDOW_SIZE;back++)
		{
			RR_ASSERT( vs_ptr < window_ptr );
			// don't access violate by looking before the start of the buffer :
			if ( vs_ptr-back < lrm_dic_start_ptr )
				break;
			if ( window_ptr[-back] != vs_ptr[-back] )
				break;
		
			// yes they match back :
			int back_i = (window_i - back + LRMSCANNER_WINDOW_SIZE)&(LRMSCANNER_WINDOW_SIZE-1);
			if ( window_len + back > scanner->window_len[ back_i ] )
			{
				scanner->window_len[ back_i ] = window_len + back;
				scanner->window_off[ back_i ] = window_off;
			}
		}
	}
	
	return ret;
}	
					
SINTa LRMSet_CheckWholeMatchQuantum(const LRMSet * lrmset,const U8 * raw,SINTa rawPos,S32 quantumLen)
{
	//LRM * lrmfirst = tmf->m_lrmfirst;
	
	// LRMScannerWindowed pushes matches forward and backward
	//	try to put the middle of the window on "curPtr"
	//	then scan up to it
		
	const U8 * curPtr = raw + rawPos;
	
	const U8* lrmset_end = LRMSet_GetEndPtr(lrmset);
	
	RR_ASSERT_ALWAYS( curPtr >= lrmset_end );
	
	const U8 * startPtr = curPtr - (LRMSCANNER_WINDOW_SIZE/2);
	startPtr = RR_MAX(startPtr,raw);
	startPtr = RR_MAX(startPtr,lrmset_end);
	const U8 * endPtr = curPtr + quantumLen;
	
	LRMScannerWindowed scanner;
	LRMScannerWindowed_Init(&scanner,lrmset,startPtr,endPtr,RR_S32_MAX);

	// advance the scanner to curPtr :
	const U8 * scanPtr = startPtr;
	while ( scanPtr < curPtr )
	{
		SINTa off;
		LRMScannerWindowed_FindMatchAndRoll(&scanner,scanPtr,endPtr,&off);
		scanPtr++;
	}
	
	// get the match there :
	SINTa off;
	S32 len = LRMScannerWindowed_FindMatchAndRoll(&scanner,curPtr,endPtr,&off);
				
	if ( len > 0 )
	{
		// see if we can extend match :
		while( len < quantumLen && curPtr[len] == curPtr[len-off] )
			len++;
		
		if ( len >= quantumLen )
		{
			return off;
		}
	}

	return 0;
}

OODLE_NS_END
