// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlebase.h"
#include "templates/rrvector.h"

OODLE_NS_START

/*********

LRM = Long Range Matcher

uses rolling hash and stepped insertion

key parameters are "step" and "hash length"

memory use is 8*(file size)/step
so step = 8 is 1 byte per raw byte

---------------

LRM = hash on one whole buffer

LRM Cascade = binary tree of LRM's
	whole buffer is cut into chunks, LRM on each chunk
	then make a tree by merging base chunk pairs
	
	[0][1][2][3]
	[0+1 ][2+3 ]
	[0 - 3     ]
	
LRM Set = selection from an LRM Cascade

	eg. to get the LRM that starts at 3 you would have
	[0+1 ][2]
	
	as your LRM Set
	


***********/

//#define LRM_DO_FORCE_HASH_LEN8

typedef U32	LRM_hash_t;

LRM_hash_t LRM_InitialHash( const U8 * ptr , int length );

#ifdef LRM_DO_FORCE_HASH_LEN8

static RADFORCEINLINE LRM_hash_t LRM_Hash8( const U8 * ptr )
{
	U64 x = RR_GET64_NATIVE(ptr);
	U64 h = x * 0xCF1BBCDCB7A56463ULL;
	h += (h>>17);
	return (U32)(h>>32);
}

RADFORCEINLINE LRM_hash_t LRM_RollHashPtr( LRM_hash_t hash, LRM_hash_t hashMulEnd, const U8 * ptr, int length )
{
	RR_ASSERT( length == 8 );
	return LRM_Hash8(ptr+1);
}

#else

extern const LRM_hash_t c_hashMul;

RADFORCEINLINE LRM_hash_t LRM_RollHash( LRM_hash_t hash, LRM_hash_t hashMulEnd, U8 add, U8 remove )
{
	// it can be 0 when there's no real LRM
	//RR_ASSERT( hashMulEnd != 0 );
	hash -= remove * hashMulEnd;
	hash *= c_hashMul;
	hash += add;
	return hash;
}

RADFORCEINLINE LRM_hash_t LRM_RollHashPtr( LRM_hash_t hash, LRM_hash_t hashMulEnd, const U8 * ptr, int length )
{
	return LRM_RollHash(hash,hashMulEnd,ptr[length],ptr[0]);
}

#endif // LRM_DO_FORCE_HASH_LEN8

struct LRM;

LRM * LRM_Create(const U8 * buffer, SINTa bufSize, int step, int jumpBits, int hash_length);
LRM * LRM_CreateMerge(const LRM * lhs, const LRM * rhs,int jumpBits);
void LRM_Destroy(LRM * lrm);

int LRM_GetHashLength(const LRM * lrm);
LRM_hash_t LRM_GetHashMulEnd(const LRM * lrm);


LRM_hash_t LRM_InitialHash( const LRM * lrm, const U8 * ptr );
LRM_hash_t LRM_RollHashPtr( const LRM * lrm, LRM_hash_t hash, const U8 * ptr );

// only offsets < matchOffsetLimit are considered
//	so eg if you need offset in an S32, then pass RR_S32_MAX
S32 LRM_FindMatch(const LRM * lrm,LRM_hash_t hash, const U8 *ptr, const U8 *ptrend,
					SINTa * pMatchOffset, SINTa matchOffsetLimit);
		
struct LRMCascade;			
LRMCascade * LRM_CreateCascade(const U8 * buffer, SINTa bufSize, int step, int jumpBits_0, int jumpBits_inc,
						SINTa base_chunkSize, int hash_length);
LRMCascade * LRM_CreateCascadeIncremental(const U8 * buffer, SINTa bufSize, int step, int jumpBits_0, int jumpBits_inc,
						SINTa base_chunkSize, int hash_length);
void LRM_DestroyCascade(LRMCascade * casc);

LRMCascade * LRM_AllocCascade();
// All at once
void LRM_FillCascade(LRMCascade * casc,const U8 * buffer, SINTa bufSize, int step, int jumpBits_0, int jumpBits_inc,
						SINTa base_chunkSize, int hash_length);

SINTa LRM_GetCascadeChunkSize(const LRMCascade * casc);

struct LRMSet
{
	vector<LRM *>	lrms;
};

S32 LRMSet_FindMatch(const LRMSet * set,LRM_hash_t hash, const U8 *ptr, const U8 *ptrend,
					SINTa * pMatchOffset, SINTa matchOffsetLimit);

const U8 * LRMSet_GetBasePtr(const LRMSet * set);
const U8 * LRMSet_GetEndPtr(const LRMSet * set);
					
RADINLINE int LRMSet_GetHashLength(const LRMSet * set)
{
	return LRM_GetHashLength(set->lrms[0]);
}
RADINLINE LRM_hash_t LRMSet_GetHashMulEnd(const LRMSet * set)
{
	return LRM_GetHashMulEnd(set->lrms[0]);
}

// start must be aligned properly;
// use _UpdateIncremental version for incremental LRM cascades, which runs increments if required
void LRM_CascadeGetSet(const LRMCascade * casc, LRMSet * set, const U8 * start);
void LRM_CascadeGetSet_UpdateIncremental(LRMCascade * casc, LRMSet * set, const U8 * start);

// if startPtr is not necessary at correct alignment, use this :
//	will align up or down but stay before parsePtr
void LRM_CascadeGetSet_Align(const LRMCascade * casc, LRMSet * set, const U8 * startPtr, const U8 * parsePtr);
void LRM_CascadeGetSet_Align_UpdateIncremental(LRMCascade * casc, LRMSet * set, const U8 * startPtr, const U8 * parsePtr);

// returns offset if found
// reutrn 0 if none
SINTa LRMSet_CheckWholeMatchQuantum(const LRMSet * lrmset,const U8 * raw,SINTa rawPos,S32 quantumLen);

//=======================================================================

struct LRMScanner
{
	const U8 * lrm_last_ml_plus_ptr;
	LRM_hash_t lrm_hash;
	LRM_hash_t lrm_hashMulEnd;
	SINTa lrm_last_off;
	S32 lrm_hashLength;
	const U8 * lrm_end_ptr; // == buffer end - lrm hash length
	const LRMSet * lrmset;
	SINTa matchOffsetLimit;
};

// set can be NULL and makes a scanner which will return no matches
void LRMScanner_Init(LRMScanner * scanner,const LRMSet * set, const U8 * startPtr, const U8 * endPtr, SINTa matchOffsetLimit);

RADINLINE bool LRMScanner_IsActive(LRMScanner * scanner) { return scanner->lrm_end_ptr != NULL; }

RADFORCEINLINE void LRMScanner_Roll(LRMScanner * scanner, const U8 * ptr)
{
	RR_ASSERT( ptr <= scanner->lrm_end_ptr || scanner->lrm_end_ptr == NULL );
	scanner->lrm_hash = LRM_RollHashPtr( scanner->lrm_hash, scanner->lrm_hashMulEnd, ptr, scanner->lrm_hashLength );
}

RADFORCEINLINE void LRMScanner_ResetHash(LRMScanner * scanner, const U8 * ptr)
{
	// this is hit regularly at the end of the buffer :
	//RR_ASSERT( ptr <= scanner->lrm_end_ptr );
	RR_ASSERT( ptr <= scanner->lrm_end_ptr+16 );
	
	if ( ptr <= scanner->lrm_end_ptr ) // protect
	{
		scanner->lrm_hash = LRM_InitialHash( ptr, scanner->lrm_hashLength );
	}
	
	// wipe the stored match too :
	scanner->lrm_last_ml_plus_ptr = 0;
	scanner->lrm_last_off = 0;
}

S32 LRMScanner_FindMatch(LRMScanner * scanner, const U8 *ptr, const U8 *ptrend,
					SINTa * pMatchOffset);
					
S32 LRMScanner_FindMatchAndRoll(LRMScanner * scanner, const U8 *ptr, const U8 *ptrend,
					SINTa * pMatchOffset);
					
//=======================================================================
/*

LRMScannerWindowed rolls a window of matches

it finds better matches than LRMScanner because when it finds a match,
it also fills that backward to missed slots due to the lossy hashing

*/

// needs to be pow2 and should be at least as large as the LRM step :
#define LRMSCANNER_WINDOW_SIZE		32
//#define LRMSCANNER_WINDOW_SIZE		16

struct LRMScannerWindowed : public LRMScanner
{
	const U8 * lrm_base_ptr;
	const U8 * lrm_last_ptr;
	const U8 * lrm_window_base;
	S32 window_len[LRMSCANNER_WINDOW_SIZE];
	SINTa window_off[LRMSCANNER_WINDOW_SIZE];
};

// set can be NULL and makes a scanner which will return no matches
void LRMScannerWindowed_Init(LRMScannerWindowed * scanner,const LRMSet * set, const U8 * startPtr, const U8 * endPtr , SINTa matchOffsetLimit);

S32 LRMScannerWindowed_FindMatchAndRoll(LRMScannerWindowed * scanner, const U8 *ptr, const U8 *ptrend,
					SINTa * pMatchOffset);
										
OODLE_NS_END

