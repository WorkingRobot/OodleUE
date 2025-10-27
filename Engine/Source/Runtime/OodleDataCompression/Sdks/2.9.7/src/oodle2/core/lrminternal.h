// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.


OODLE_NS_START
	
/*

TABLES OF LINEAR CONGRUENTIAL GENERATORS
Ecuyer

741103597, 887987685
1597334677, 851723965
747796405, 204209821

1181783497276652981, 4292484099903637661
7664345821815920749, 1865811235122147685
2685821657736338717, 1803442709493370165

*/

// dupe hashes help, but very little :
// with LRM_DO_DEDUPE_IN_MERGE = 1 :
//#define LRM_NUM_DUPES	0	// 31,290,099 enwik8 LRM_DO_DEDUPE_IN_MERGE
//#define LRM_NUM_DUPES	4	// 31,288,945 enwik8 LRM_DO_DEDUPE_IN_MERGE
//#define LRM_NUM_DUPES	8	// 31,287,020 enwik8 LRM_DO_DEDUPE_IN_MERGE
//#define LRM_NUM_DUPES	32	// 31,283,941 enwik8 LRM_DO_DEDUPE_IN_MERGE
//#define LRM_NUM_DUPES	256 // 31,280,503 enwik8 LRM_DO_DEDUPE_IN_MERGE
//#define LRM_NUM_DUPES	9999999  // 31,280,428 enwik8 LRM_DO_DEDUPE_IN_MERGE
// no LRM_NUM_DUPES		31,258,034
// the difference is the dedupe in Merge

#define LRM_DO_DEDUPE_IN_MERGE	0
// LRM_DO_DEDUPE_IN_MERGE toggles deduping in the merge process
//	if it is off, I still dedupe *after* the merge
//	using the normal LRM_NUM_DUPES

// 12/09/2015 : cranking down NUM_DUPES doesn't really help speed much either
// with LRM_DO_DEDUPE_IN_MERGE = 0 :
//#define LRM_NUM_DUPES	0	// 
//#define LRM_NUM_DUPES	4	// 
//#define LRM_NUM_DUPES	8	// 
#define LRM_NUM_DUPES	16
//#define LRM_NUM_DUPES	32	// 31,268,849
//#define LRM_NUM_DUPES	256 // 31,261,631 enwik8
// no LRM_NUM_DUPES		31,258,034


//#define LRM_GOOD_ENOUGH_LEN	1024
//#define LRM_GOOD_ENOUGH_LEN	512
#define LRM_GOOD_ENOUGH_LEN	256

#define LRM_MIN_MATCH_LEN	8

#if LRM_MIN_MATCH_LEN >= 8
#define getmatchlen_lrm	getmatchlen_mml8
#else
RR_COMPILER_ASSERT( LRM_MIN_MATCH_LEN >= 4 );
#define getmatchlen_lrm	getmatchlen_mml4
#endif

#ifndef LRM_DO_FORCE_HASH_LEN8

extern const LRM_hash_t c_hashMul;

static RADFORCEINLINE LRM_hash_t LRM_InitialHash4( const U8 * ptr )
{
	LRM_hash_t ret = ptr[3] + c_hashMul*(ptr[2] + c_hashMul*(ptr[1] + c_hashMul*(ptr[0])));
	return ret;
}

#endif // LRM_DO_FORCE_HASH_LEN8

struct LRMEntry
{
	LRM_hash_t hash;
	//U32 firstfour;
	//SINTa pos;
	U32 pos32; // pos32 is relative to matchBasePtr
	
	bool operator < (const LRMEntry & rhs) const
	{
		return hash < rhs.hash;
	}
	
	/*
	void operator = (const LRMEntry & rhs)
	{
		hash = rhs.hash;
		pos = rhs.pos;
	}
	*/
};

RR_COMPILER_ASSERT( sizeof(LRMEntry) == 8 );

struct LRMEntry_Hash_Less
{
	bool operator () (const LRMEntry & lhs,const LRMEntry rhs) const
	{
		return lhs.hash < rhs.hash;
	}
	
	bool operator () (const LRMEntry & lhs,const LRM_hash_t rhs) const
	{
		return lhs.hash < rhs;
	}
	
	bool operator () (const LRM_hash_t lhs,const LRMEntry & rhs) const
	{
		return lhs < rhs.hash;
	}
};

struct LRMEntry_Hash_Equal
{
	bool operator () (const LRMEntry & lhs,const LRMEntry rhs) const
	{
		return lhs.hash == rhs.hash;
	}
	
	bool operator () (const LRMEntry & lhs,const LRM_hash_t rhs) const
	{
		return lhs.hash == rhs;
	}
	
	bool operator () (const LRM_hash_t lhs,const LRMEntry & rhs) const
	{
		return lhs == rhs.hash;
	}
};
	
struct LRM
{
	OodleXHandle	creation;

	vector<LRMEntry>	entries;
	int bloom_row_shift;
	U32 * bloom_filter;

	int hash_length;
	LRM_hash_t hash_mul_end;

	// this LRM covers [chunkBasePtr,chunkBasePtr+bufSize]
	//  matchBasePtr is somewhere in that range
	//	entries are relative to matchBasePtr
	const U8 * chunkBasePtr;
	const U8 * matchBasePtr;
	SINTa bufSize;

	vector<S32>		jumpIn;
	int	jumpInShift;

	LRM() : creation(0), bloom_row_shift(0), bloom_filter(0), hash_length(0), hash_mul_end(0), chunkBasePtr(0), matchBasePtr(0), bufSize(0), jumpInShift(0) { }
	~LRM();

	void FreeBloom();
};



//#define LRM_CASCADE_MAX_LEVELS	6
#define LRM_CASCADE_MAX_LEVELS	8

struct LRMCascade
{
	vector<LRM *>	lrms[LRM_CASCADE_MAX_LEVELS];
	SINTa chunkSize;
	
	const U8 * buffer;
	SINTa bufSize;

	// LRM build parameters
	int step;
	int jumpBits_0;
	int jumpBits_inc;
	int hash_length;

	// For incremental processing
	SINTa numChunks;
	SINTa nextChunk;
};


LRM * LRM_FillSeveral(LRM ** lrmArray,
						const U8 * bufStart, const U8 * bufEnd, SINTa chunkSize,
						int step, int jumpBits, int hash_length);
						
						
LRM * LRM_FillMergeSeveral(LRM ** lrms,LRM ** prev_lrms, SINTa clo, SINTa cnum, int jumpBits);

OODLE_NS_END
