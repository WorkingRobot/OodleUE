// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrhuffman.h"
#include "rrmemutil.h"
#include "oodlemalloc.h"
#include "rrlogutil.h"
#include "rrmath.h"
#include "rrvarbitcodes.h"
#include "histogram.h"
#include "rrhuffman.inl"
#include "rrarenaallocator.h"

#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "rrcppstd.h"
#include "rrstackarray.h"

#include "templates/rralgorithm.h" // need a sort
#include "cbradutil.h"

/*
#include "rrsimpleprof.h"
/*/
#include "rrsimpleprofstub.h"
/**/

OODLE_NS_START

//-----------------------------------------------------------

#define DO_MOFFAT_INPLACE
// zero measurable speed difference between Moffat & my "branchy"
//	("branchy" is 5-10% faster it's just not significant overall)

// DO_BUILD_CODE_LEN_NO_ALLOC makes the counts fit in U16's
//	that can hurt compression a tiny bit

#define DO_BUILD_CODE_LEN_NO_ALLOC
	// -> is required for radix sort
	//	because I do two byte radixes on the U16 char count

//=================================================

#define memclear(ptr,size)  memset(ptr,0,size)

//=================================================

// rrHuffman_FinishCodeLens : 
//	codeLenTable is done
//	make numCodesOfLen and minCodeLen,maxCodeLen
static void rrHuffman_FinishCodeLens(rrHuffman * HI)
{
    // fill codeLenTable and numCodesOfLen :
    S32 * numCodesOfLen = HI->numCodesOfLen;
    memclear(numCodesOfLen,sizeof(HI->numCodesOfLen));
    
    RR_DURING_ASSERT( U32 kraft = 0; const U32 kraft_one = (1UL<<31) );
    //U32 estimatedSize = 0;

	const U8 * codeLenTable = HI->codeLenTable;        
        
    S32 topSymbol = HI->topSym;
    RR_ASSERT( topSymbol <= HI->numSymbols-1 );
    RR_ASSERT( codeLenTable[topSymbol] != 0 );
    RR_ASSERT( topSymbol == HI->numSymbols-1 || codeLenTable[topSymbol+1] == 0 );
    
    for(int i=0;i<=topSymbol;i++)
    {
		int cl = codeLenTable[ i ];
		if ( cl == 0 ) continue;	

		RR_DURING_ASSERT( kraft += kraft_one>>cl );
        numCodesOfLen[ cl ] ++;
	        
        //estimatedSize += cl * charCounts[i];
    }

	//rrprintf("estimatedSize : %d\n",estimatedSize);

	// check validity of prefix code :    
    RR_ASSERT( kraft == kraft_one );
     
    S32 minCodeLen = 1;
    while ( numCodesOfLen[minCodeLen] == 0 ) minCodeLen++;
    S32 maxCodeLen = 31;
    while ( numCodesOfLen[maxCodeLen] == 0 ) maxCodeLen--;
	HI->minCodeLen = minCodeLen;
	HI->maxCodeLen = maxCodeLen;
}

// codeLens - numSymbols of them - 0 means symbol does not occur
rrbool rrHuffman_ProvideCodeLens(rrHuffman *HI,const U8 * givenCodeLens)
{
    HI->minCodeLen = HI->maxCodeLen = 0;
	HI->gotNumSymbols = 0;
	HI->oneChar = (U16)(-1);
	HI->topSym = 0;
	
    // fill codeLenTable :
    
    for(int i=0;i<HI->numSymbols;i++)
    {
        HI->codeLenTable[ i ] = (U8) givenCodeLens[i];
        
        if ( givenCodeLens[i] != 0 )
        {
			HI->topSym = i;
			HI->gotNumSymbols ++;
			HI->oneChar = i; // record a valid char in case it winds up being the only one 
        }
    }
    
    if ( HI->gotNumSymbols <= 1 )
    {
		// one symbol is a special case, because we want to decode it, but it has no code len
		return false;
    }

	rrHuffman_FinishCodeLens(HI);

	return true;
}


//=================================================

#ifdef DO_BUILD_CODE_LEN_NO_ALLOC // <- this is always on

// fit rrHuffman_CharAndCount in 32 bits so it can overlap the encodeTable and needs no alloc

#define PACKEDSYM_TYPE U16

#define CHAR_COUNT_TYPE	U16
static const int MAX_CHAR_COUNT	= (CHAR_COUNT_TYPE)(~0);

struct rrHuffman_CharAndCount
{
	PACKEDSYM_TYPE symbol;
	CHAR_COUNT_TYPE count;
};
// we reuse the memory so make sure it fits :
RR_COMPILER_ASSERT( sizeof(U32) == sizeof(rrHuffman_CharAndCount) );

#else // DO_BUILD_CODE_LEN_NO_ALLOC

// let rrHuffman_CharAndCount be big and do an alloc :

#define PACKEDSYM_TYPE S32

#define CHAR_COUNT_TYPE	S32
#define MAX_CHAR_COUNT	RR_S32_MAX

struct rrHuffman_CharAndCount
{
	PACKEDSYM_TYPE symbol;
	CHAR_COUNT_TYPE count;
};

#endif // DO_BUILD_CODE_LEN_NO_ALLOC

/*

MAX_TOT_COUNT must fit in CHAR_COUNT_TYPE as well,
because I use the moffat inplace tree; sums of counts replace counts
the root of the tree gets the sum of all and so must fit in that type as well

*/
static const int MAX_TOT_COUNT = MAX_CHAR_COUNT;


// rrHuffman_CharAndCountCompareCount is used in the stdsort
//	which is used intead of radix when sym count is low
struct rrHuffman_CharAndCountCompareCount
{
    bool operator() (const rrHuffman_CharAndCount & lhs,const rrHuffman_CharAndCount & rhs) const
    {
	// @@ CB this works fine but not sure there's any advantage
	#if 0 && defined(__RADLITTLEENDIAN__) && defined(DO_BUILD_CODE_LEN_NO_ALLOC)
		// count is in the high bits of the 32-bit word
		//  so just sort the whole 32-bit word
		RR_COMPILER_ASSERT( sizeof(rrHuffman_CharAndCount) == 4 );
		return RR_GET32_LE(&lhs) < RR_GET32_LE(&rhs);
	#else
        return lhs.count < rhs.count;
	#endif
    }
};

/*
 * Scale Counts to [0,MaxVal] 
 *  (brackets indicate the range is "inclusive")
 *
 * a count which was > 0 will not be scaled to 0
 *
 */
static const U32 * rrHuffman_ScaleCounts(rrHuffman *HI,
							const U32 * charCounts,
							U32 * destCounts,
							U32 allowedCharCount,U32 allowedTotCount)
{
    S32 numSymbols = HI->numSymbols;

    RR_ASSERT( allowedTotCount >= (U32)(numSymbols*2) );
    RR_ASSERT( allowedCharCount >= 2 );
    
    //get maxcharcount
    U32 totCharCount = 0;
    U32 maxCharCount = 0;
    S32 maxi = 0;
    for(S32 i=0;i<numSymbols;i++)
    {
		U32 cc = charCounts[i];
		// @@ I am *NOT* robust to overflowing int
		//	if someone gives me really enormous char counts
		RR_ASSERT( cc >= 0 && cc < (RR_ONE_U32<<30) );
		// check for overflow :
		RR_ASSERT( (S64)((U32)(totCharCount+cc)) == ((S64)totCharCount+cc) );
        totCharCount += cc;
        if ( cc > maxCharCount )
        {
			maxCharCount = cc;
			maxi = i;
        }
    }
    
    // allowedCharCount never bigger than 16 bits :
    //  we multiply char by tot so the product must fit in 32 bits
    // -> not true any more, I use floats
    RR_ASSERT( allowedCharCount <= (U32)MAX_CHAR_COUNT );
    //allowedCharCount = RR_MIN(allowedCharCount,MAX_CHAR_COUNT);
    //allowedTotCount  = RR_MIN(allowedTotCount ,MAX_CHAR_COUNT);

	if ( maxCharCount > allowedCharCount || 
		 totCharCount > allowedTotCount )
	{
		// we must change charCounts
		// if mutating is okay with the user, then they pass destCounts == charCounts
		// if not, they pass NULL and I delay the malloc as late as possible
		//	 so that it's only done when needed
		/*
		if ( destCounts == NULL )
		{
			destCounts = OODLE_MALLOC_ARRAY(U32,numSymbols);
		}
		*/
	
		// could use an integer fixed point multiply
		//	but charCounts can be bigger than 16 bits, so I don't have a lot of room
		//	might have to do a variable shift based on the pow2 of the largest charCount
		// screw it just use floats
              
		F32 scale = RR_MIN( ((F32)allowedCharCount/maxCharCount) , ((F32)allowedTotCount/totCharCount) );
    
		U32 destTotal = 0;
		for(int i=0;i<numSymbols;i++)
		{
			if ( charCounts[i] == 0 )
			{
				destCounts[i] = 0;
				continue;
			}
			
			// scale and round :
			U32 OutCharCount = (U32) rr_ftoi_round_positive( charCounts[i] * scale );
	        
	        // make sure it doesn't slip to zero :
	        OutCharCount = RR_CLAMP(OutCharCount,1,allowedCharCount);
	        
	        // destCounts may == charCounts
			destCounts[i] = OutCharCount;
			destTotal += OutCharCount;
		}
		
		// apply correction for destTotal exceeding allowed on the symbol with max count
		if ( destTotal > allowedTotCount )
		{
			destCounts[maxi] += allowedTotCount - destTotal;
			RR_ASSERT_ALWAYS( destCounts[maxi] > 0 );
		}
		
		return destCounts;
    }
    else
    {
		// destCounts not filled
		
		return charCounts;
    }
}
 
/***********

rrHuffman_BuildCodeLens_Limited

Package-Merge implementation

See "A Fast and Space-Economical Algorithm for Length-Limited Coding"

Explicity what we are trying to do is solve :

Cost = Sum[ L_i * C_i ]

C_i = count of ith symbol
find L_i to minimize Cost
contrained such that L_i <= L_max
and 
Sum[ 2 ^ - L_i ] = 1

(Kraft prefix constraint)

This is solved by construction of the "coin collector problem"

The Cost that we minimize is the real (numismatic) value of the coins that the collector pays out
C_i is the numismatic value of the ith coin
L_i is the number of times the collector uses it
so Cost = Sum[ L_i * C_i ] is his total cost.

For each value C_i, the coins have face value 2^-1, 2^-2, 2^-4, ...
If the coin collector pays out total face value of (n-1) , then he creates a Kraft correct prefix code

The coin collector problem is simple & obvious ; you just want to pay out from your 2^-1 value items ;
an item is either a 2^-1 value coin, or a pair of 2^-2 value items ; pick the one with lower numismatic value

The fact that this creates a prefix code is a bit more obscure
But you can prove it by the kraft property

If you start with all lenghts = 0 , then K = sum[2^-L] = N
Now add an item from the 2^-1 list
if it's a leaf, L changes from 0 to 1, so K does -= 1/2
if it's a node, then it will bring in two nodes at a lower level
	equivalent to to leaves at that level, so L changes from 1 to 2 twice, so K does -= 1/2 then too
	
so if the last list has length (2N-2) , you get K -= 1/2 * (2N-2) , or K -= N-1 , hence K = 1 afterward

-----------------------------------
BTW you can do this in a dynamic programming sort of way where only the active front is needed; has same
run time but less storage requirements.

You start at the 2^-1 (final) list.  You ask : what's the next node of this list?  It's either a symbol or
  made from the first two nodes of the prior list.  So you get the first two nodes of the prior list.
When you select a node into the final list, that is committed, and all its children in the earlier lists
  become final; they can now just do their increments onto CodeLen and be deleted.
If you select a symbol into the final list, then the nodes that you looked at earlier stick around so you
  can look at them next time.

*****************/

// chars sorted to lowest count first :
static void rrHuffman_Sort( rrHuffman_CharAndCount * symTable,int symCount,rrArenaAllocator * arena)
{
	SIMPLEPROFILE_SCOPE(rrHuffman_Sort);

	if ( symCount <= 1 )
	{
		return;
	}
	// not very many symbols, don't do a big radix, just stdsort :
	if ( symCount <= 32 )
	{
	    stdsort(symTable,symTable+symCount,rrHuffman_CharAndCountCompareCount());
	    return;
	}
	
	#if 0
	
	// old way - RR sort :
	
    stdsort(symTable,symTable+symCount,rrHuffman_CharAndCountCompareCount());

	#else
	
	// Radix sort
	// U16 counts so we just radix on U8 twice -> scan over [256] twice
	//	most helpful when symCount is large
	// uses 3k of stack + 4*symCount = 4k for 256 sym alphabet
		
	RR_COMPILER_ASSERT( sizeof(rrHuffman_CharAndCount) == 4 );
	RR_COMPILER_ASSERT( sizeof(CHAR_COUNT_TYPE) == 2 );

	/*
	RR_STACK_ARRAY(table2,rrHuffman_CharAndCount,symCount);
	U32 hist[512] = { 0 }; // 2048 bytes
	U32 offsets[256]; // 1024 bytes
	*/
	SINTa scratch_size_needed = 256*3*sizeof(U32) + sizeof(rrHuffman_CharAndCount)*symCount;
	RR_SCOPE_ARENA_ARRAY(scratch,U8,scratch_size_needed,arena);
	U8 * scratch_ptr = scratch;
	RR_DURING_ASSERT( U8 * scratch_end = scratch + scratch_size_needed );
	
	U32 * hist = (U32 *)scratch_ptr; scratch_ptr += 2048;
	memset(hist,0,2048);
	
	U32 * offsets = (U32 *)scratch_ptr; scratch_ptr += 1024;
	rrHuffman_CharAndCount * table2 = (rrHuffman_CharAndCount *)scratch_ptr; scratch_ptr += sizeof(rrHuffman_CharAndCount)*symCount;
	RR_ASSERT( scratch_ptr == scratch_end );
	
	rrHuffman_CharAndCount* p = symTable;
	rrHuffman_CharAndCount* q = symTable + (symCount >> 1) * 2;

	for ( ; p != q; p += 2)
	{
		const U32 freq0 = p[0].count;
		const U32 freq1 = p[1].count;

		hist[        freq0         & 0xFF]++;
		hist[256 + ((freq0 >>  8) & 0xFF)]++;

		hist[        freq1        & 0xFF]++;
		hist[256 + ((freq1 >>  8) & 0xFF)]++;
	}

	if (symCount & 1)
	{
		const U32 freq = p->count;

		hist[        freq        & 0xFF]++;
		hist[256 + ((freq >>  8) & 0xFF)]++;
	}

	// for first pass :
	rrHuffman_CharAndCount* pCur_syms = symTable;
	rrHuffman_CharAndCount* pNew_syms = table2;
			
	for (int pass = 0; pass < 2; pass++)
	{
		const U32* pHist = &hist[pass << 8];

		int cur_offset = 0;
		
		//for (U32 i = 0; i < 256; i += 2)
		// much shorter loop typically in pass 1 :
		for (int i = 0; cur_offset < symCount; i += 2)
		{
			offsets[i]   = cur_offset;	cur_offset += pHist[i];
			offsets[i+1] = cur_offset;	cur_offset += pHist[i+1];
		}
		
		RR_ASSERT( cur_offset == symCount );
		
		if ( pass == 0 )
		{
			// bottom bits are super random, so just do the trivial way :
			//	(exception if total count is very low)
		
			/*
			for(int i=0;i<symCount;i++)
			{
				U32 c = pCur_syms[i].count & 0xFF;
				U32 dst_offset = offsets[c]++;
				pNew_syms[dst_offset] = pCur_syms[i];
			}
			*/
			
			int sym=0;
			for(;sym<=(symCount-4);sym+=4)
			{
				U32 c; U32 dst_offset;
				
				RR_UNROLL_I_4(sym, \
					c = pCur_syms[i].count & 0xFF; \
					dst_offset = offsets[c]++; \
					RR_ASSERT( dst_offset < (U32)symCount ); \
					pNew_syms[dst_offset] = pCur_syms[i]; \
				);
			}
			for(;sym<symCount;sym++)
			{
				U32 c; U32 dst_offset;
				
				c = pCur_syms[sym].count & 0xFF;
				dst_offset = offsets[c]++;
				RR_ASSERT( dst_offset < (U32)symCount );
				pNew_syms[dst_offset] = pCur_syms[sym];
			}
		}
		else
		{
			// top bits are often all the same
			//	do big block copies
			//	avoids LHS in offsets[] table too
		
			if ( pHist[0] == (U32)symCount )
			{
				// can just memcpy the whole thing
				memcpy(pNew_syms,pCur_syms,sizeof(pCur_syms[0])*symCount);
				return;
			}
		
			#define RADIX(i)	( (pCur_syms[i].count) >> 8 )
		
			U32 prev = RADIX(0);
			int previ = 0;
			//U32 prev_offset = offsets[prev];
			//pNew_syms[prev_offset] = pCur_syms[0];
			//prev_offset++;
			
			for(int i=1;;)
			{
			
				U32 cur = RADIX(i);				
				while ( cur == prev )
				{			
					i++;
					if ( i == symCount )
						break;
					cur = RADIX(i);
				}
						
				int count = i - previ;	
				
				U32 prev_offset = offsets[prev];
				memcpy(pNew_syms+prev_offset,pCur_syms+previ,sizeof(pCur_syms[0])*count);
				offsets[prev] += count;
				
				// i may already be at symcount, that's okay
				
				prev = cur;
				previ = i;
				i++;
				if ( i >= symCount )
				{
					if ( i == symCount )
					{
						// ended with a unique one, need to output it :
						pNew_syms[ offsets[prev] ] = pCur_syms[previ];
					}
				
					break;
				}
			}
			
			#undef RADIX
		}
				
		// for second pass :
		pCur_syms = table2;
		pNew_syms = symTable;
	}
	
	#endif
}

/***

rrHuffman_BuildCodeLens_Limited_PackageMerge

does not require you to have previously built code lens

just allocates the max possible space and fills it all out

****/
static void rrHuffman_BuildCodeLens_Limited_PackageMerge(rrHuffman *HI,const U32 *charCounts,int maxCodeLen,
					rrArenaAllocator * arena)
{
	SIMPLEPROFILE_SCOPE(PackageMerge);

	// first row is the symbols :	
    rrHuffman_CharAndCount * symTable = NULL;
    // use the encodeTable as work space
    symTable = (rrHuffman_CharAndCount *) HI->encodeTable;
	int symCount = 0;
		
	for(int i=0;i<HI->numSymbols;i++)
	{
		if ( charCounts[i] == 0 )
			continue;
			
		symTable[symCount].symbol = check_value_cast<PACKEDSYM_TYPE>( i );
		symTable[symCount].count  = check_value_cast<CHAR_COUNT_TYPE>( charCounts[i] );
		symCount++;
	}
	HI->gotNumSymbols = symCount;
	
	rrHuffman_Sort(symTable,symCount,arena);
    
    int log2NumSyms = rrIlog2ceil(HI->gotNumSymbols);
    if ( log2NumSyms > maxCodeLen )
    {
		RR_ASSERT_FAILURE_ALWAYS("Impossible short code limit requested!\n");
    }
        
	// chars sorted to lowest count first :
	
	// add a fence:
	symTable[symCount].count = MAX_CHAR_COUNT;
	
	//-------------------------------------------------------------

	int maxNodesPerLevel = 2 * symCount - 2;
	
	int allocPackagesCount = maxNodesPerLevel*(maxCodeLen-1);
	rrHuffman_CharAndCount * pPackages = NULL;
	
	// allocPackagesCount = 510 * 10 = 5100
	// sizeof(rrHuffman_CharAndCount) == 4
	
	// ~ 20k alloc for Kraken
	//SINTa allocSize = sizeof(rrHuffman_CharAndCount) * allocPackagesCount;
	//rrHuffman_CharAndCount * allocPackages = NULL;
	
	RR_SCOPE_ARENA_ARRAY(allocPackages,rrHuffman_CharAndCount,allocPackagesCount,arena);
	
	pPackages = allocPackages;
			
	rrHuffman_CharAndCount * packages[32] = { 0 };
	packages[0] = symTable;
	for(int i=1;i<maxCodeLen;i++)
	{
		packages[i] = pPackages;
		pPackages += maxNodesPerLevel;
	}
	
	int packageCount[32] = { 0 };
    packageCount[0] = symCount;
    
	//-------------------------------------------------------------
	#define TREE_FLAG	(1<<15)

	// make all the lists :
	for(int l=1; l <maxCodeLen;l++)
	{
		// make l from (l-1) and l0
		//	make packages from (l-1) and merge in sorted order with raw leaves
		int i0 = 0;
		int i1 = 0;
		
		int i;
		for(i=0;i<maxNodesPerLevel;i++)
		{
			// add a node to cur level either from i0 or i1
			RR_ASSERT( i0 <= symCount );
			int c0 = symTable[i0].count;
			
			if ( i1+1 < packageCount[l-1] )
			{
				int c1 = packages[l-1][i1].count + packages[l-1][i1+1].count;
			
				if ( c1 <= c0 )
				{
					packages[l][i].count = check_value_cast<CHAR_COUNT_TYPE>( c1 );
					packages[l][i].symbol = check_value_cast<PACKEDSYM_TYPE>( i1 | TREE_FLAG );
					i1 += 2;
					continue;
				}
			}
			
			if ( c0 == MAX_CHAR_COUNT )
				break;
						
			packages[l][i].count = check_value_cast<CHAR_COUNT_TYPE>( c0 );
			packages[l][i].symbol = symTable[i0].symbol;
			i0 ++;
		}
		
	    packageCount[l] = i;
	}

	// clear codeLenTable , we will accumulate into it :
    memset( HI->codeLenTable, 0, sizeof(HI->codeLenTable[0])*HI->numSymbols );

	// start at the last list;
	// the last list is the active set
	//	and anything it refers to is also active
	//	each symbol in the active set gets its code len bumped :
	int curNumNodes = packageCount[maxCodeLen-1];
	for(int l=maxCodeLen-1;l>=0;l--)
	{
		// track lastTree that we use from this list :
		int lastTree = 0;
		for(int i=0;i<curNumNodes;i++)
		{
			if ( packages[l][i].symbol < TREE_FLAG )
			{
				// leaf :
				HI->codeLenTable[ packages[l][i].symbol ] += 1;
			}
			else
			{
				// tree :
				lastTree = (packages[l][i].symbol - TREE_FLAG) + 2;
			}			
		}
		
		// next list active size is lastTree :
		curNumNodes = lastTree;
	}

	//-------------------------------------------------------------
	
	rrHuffman_FinishCodeLens(HI);
	
	RR_ASSERT( HI->maxCodeLen <= maxCodeLen );
	
	//-------------------------------------------------------------
	
	// scoped
	//if ( allocPackages )
	//	OodleFree(allocPackages);
	
	return;
}

struct SymHeapNode
{
	int sym;
	S32 score;
};

// normal operator < will order the heap with largest first
bool operator < (const SymHeapNode & lhs, const SymHeapNode & rhs)
{
	return lhs.score < rhs.score;
}

/**

rrHuffman_CharAndCount * table
	has codelens
	longest to shortest

 **/
static void rrHuffman_BuildCodeLens_Limited_Heuristic(rrHuffman *HI,const U32 *charCounts,int limitCodeLen,rrHuffman_CharAndCount * table,rrArenaAllocator * arena)
{
	SIMPLEPROFILE_SCOPE(Limited_Heuristic);
	
    // table[] is in order from lowest symbol count to highest
    //	eg. highest codelen to lowest
    // .count now contains codelen (changed by huff-inplace)
    
    //RR_ASSERT( table[0].count > limitCodeLen );
    
	// 15 for new heuristic so that (count<<codelen) fits in S32
	//	(count is U16 so that takes 31 bits)
	RR_ASSERT( limitCodeLen <= RR_HUFFMAN_ENCODE_CODELEN_LIMIT );
	RR_COMPILER_ASSERT( RR_HUFFMAN_ENCODE_CODELEN_LIMIT == 15 );

	// @@ lookup table of shifts is silly
    U32 kraft_one_shifted[17];
    for(int i=0;i<=limitCodeLen+1;i++) kraft_one_shifted[i] = 1U<<(32 - i);
    //kraft_one_shifted[0] is invalid, don't use

	S32 gotNumSymbols = HI->gotNumSymbols;
	gotNumSymbols;
	RR_ASSERT( table[gotNumSymbols-1].count > 0 );
	
	// cl_start[] tracks where each range of codelens is in the sorted list
	// they are in order from highest codelen to lowest
    int cl_start[17];
	int last_cl = limitCodeLen+1;
	RR_ASSERT( last_cl < 17 );
    cl_start[last_cl] = 0;
    
	// kraft target is 1.0 in fixed point, at pos 32
	U64 kraft = 0;
	U64 kraft_one = ((U64)1)<<32;
	
	// truncate codelens to limit and compute kraft :
	//  can make this faster from numCodesOfLen[] which we have

	#if 0
	// old way :
	for LOOP(i,gotNumSymbols)
	{
		int cl = table[i].count;
		if ( cl > limitCodeLen )
		{
			cl = limitCodeLen;
			table[i].count = (CHAR_COUNT_TYPE)cl; 
		}
		kraft += kraft_one_shifted[cl];
				
		if ( cl != last_cl )
		{
			RR_ASSERT( cl < last_cl );
			while( last_cl > cl )
			{
				last_cl--;
				cl_start[last_cl] = i;
			}			
		}
	}
	while(last_cl>0)
	{
		last_cl--;
		cl_start[last_cl] = gotNumSymbols;
	}
	#else
	{
	const S32 * numCodesOfLen = HI->numCodesOfLen;

	// can make this faster from numCodesOfLen[] which we have
	//	  from Moffat tree builder
	// table[] codelens should have already been limited
	int next = 0;
	for (int cl=limitCodeLen;cl>=1;cl--)
	{
		int n = numCodesOfLen[cl];

		RR_ASSERT( n == 0 || table[next].count == cl );
		RR_ASSERT( n == 0 || table[next+n-1].count == cl );
		
		kraft += n * kraft_one_shifted[cl];
		
		cl_start[cl] = next;
		next += n;
	}
	cl_start[0] = next;
	RR_ASSERT( next == gotNumSymbols );

	}
	#endif
	
	RR_ASSERT( (cl_start[7-1] - cl_start[7]) == HI->numCodesOfLen[7] );

	// cl_start[cl] - cl_start[cl-1] is now the interval for symbols of that codelen
	// charCounts are in increasing order (lowest counts first)
	// codelens are in decreasing order (limitCodeLen comes first)
	
    // should only be called when truncation is needed :
	RR_ASSERT( kraft > kraft_one );
    RR_ASSERT( (kraft>>32) == 1 ); // kraft is a binary fraction with 1 at bit pos 32
        
    /**
    
    find symbols to do cl++ on , to decrease our kraft, give us more room in the codespace
    
    the cost of doing cl++ is adding 1 bit for each occurance of that symbol
    so the cost is charCounts[s]
    
    we want to gain as much kraft space as possible per bit we spend, so 
    
    score = (F64)kraft_delta / (F64)charCounts[s];
    
    I think of this score as this ratio, but I compute it more simply :
    
    score = (kraft_one >> cl) / count
    score = kraft_one / (count<<cl)
    score = 1.0 / (count<<cl)
    
    since I only care about ordering, instead of 1/x use -x
    score = -(count<<cl)
    
    now can be done in ints
    
    =========================
    
    this is a very rough first step towards a proper package-merge solution
    the true package-merge would use a priority queue like this
    but would be able to do combined steps
    you need a priority queue per codelen
    they can be lazy evaluated dynamic programming style
    etc.
    
    **/
    
    while( kraft != kraft_one )
    {
		// converge by taking positive or negative steps
		//	delta to kraft_one is strictly decreasing
		// but we can oscillate around it
		//  forcing strictly decreasing delta prevents infinite loops here		
    
		if ( kraft > kraft_one )
		{
			// find best cl to ++
			
			U32 kraft_abs_delta = check_value_cast<U32>( kraft - kraft_one );
			
			S32 best_score = RR_S32_MIN;
			int best_i = -1;
			
			// could clz on kraft_abs_delta to find the first cl
			//	rather than iterating up from 1
			int kraft_clz = rrClz32(kraft_abs_delta);
			RR_ASSERT( kraft_clz > 0 );
			int start_cl = kraft_clz;
			// if kraft_abs_delta is an exact power of 2, you do not do this branch,
			//	if it is not an exact power of 2, you do need start_cl--
			if ( start_cl > 1 && kraft_one_shifted[start_cl] < kraft_abs_delta*2 )
			{
				RR_ASSERT( ! rrIsPow2(kraft_abs_delta) );
				start_cl--;
			}
			else
			{
				RR_ASSERT( rrIsPow2(kraft_abs_delta) );
			}

			#ifdef RR_DO_ASSERTS
			// check it :
			{
				// start_cl step should be allowed :
				U32 kraft_delta = kraft_one_shifted[start_cl+1];
				RR_ASSERT( kraft_delta < kraft_abs_delta*2 );
				
				if ( start_cl > 1 )
				{
					// start_cl-1 step should not be allowed :
					U32 kraft_delta_below = kraft_one_shifted[start_cl];
					RR_ASSERT( kraft_delta_below >= kraft_abs_delta*2 );
				}
			}
			#endif
			
			//for(int cl=1;cl<limitCodeLen;cl++)
			for(int cl=start_cl;cl<limitCodeLen;cl++)
			{
				// best one to increase will be the lowest charcount at each CL
				int cl_first = cl_start[cl];
				int cl_len = cl_start[cl-1] - cl_first;
				if ( cl_len == 0 ) continue;
				
				RR_ASSERT( cl_first < gotNumSymbols );
				RR_ASSERT( table[cl_first].count == cl );
				
				// don't overshoot by more than we started with
				//U32 kraft_delta = kraft_one_shifted[cl+1];
				//if ( kraft_delta >= kraft_abs_delta*2 ) continue;
				RR_ASSERT( kraft_one_shifted[cl+1] < kraft_abs_delta*2 );
				
				int sym = table[cl_first].symbol;
				S32 score = -(S32)(charCounts[sym] << cl);
				if ( score > best_score )
				{
					best_score = score;
					best_i = cl_first;
				}
			}
			
			// this can fail to find any allowed step
			// if kraft_delta is small (eg. only a max codelen could be incremented)
			//	but we don't have any high codelens
			// then any increment of a lower codelen would make us overshoot too far
			//	bail out to failure handler
			//RR_ASSERT( best_i != -1 );
			if ( best_i < 0 )
			{
				break;
			}
				
			// do cl++ :
			int cl = table[best_i].count;
			RR_ASSERT( cl != 0 && cl < limitCodeLen );
			
			// correct kraft , remove old and add new :
			//kraft -= kraft_one >> cl;
			//kraft += kraft_one >> (cl+1);
			
			U32 kraft_delta = kraft_one_shifted[cl+1];
			kraft -= kraft_delta;
			
			// error should have gone down: 
			RR_ASSERT( RR_ABS((S64)kraft - (S64)kraft_one) < kraft_abs_delta );
			
			cl_start[cl] ++;
			
			cl ++;
			table[best_i].count = check_value_cast<CHAR_COUNT_TYPE>( cl );
		}
	    
		// we may have gone too far, see if we can get some back :
		
		if ( kraft < kraft_one )
		{    
			/**
			
			find symbols to do cl-- on :
			
			we have some kraft budget to spend
			we want to gain as many bits as possible
			
			score is just the inverse of the previous heap
			F64 score = (F64)charCounts[s] / (F64)kraft_delta;
			different is you want to do it on the highest count symbol at each codelen,
			  rather than the lowest

			unlike cl++ it's always possible to make a move here
			because you can always do cl-- on the longest codelen
			  without overshooting K = 1
			  since that's the number of bits of precision in K

			**/
	    
			U32 kraft_abs_delta = check_value_cast<U32>( kraft_one - kraft );
			
			S32 best_score = RR_S32_MIN;
			int best_i = -1;
			
			// could clz on kraft_abs_delta to find the first cl
			//	rather than iterating up from 1
			int kraft_clz = rrClz32(kraft_abs_delta);
			RR_ASSERT( kraft_clz > 0 );
			int start_cl = kraft_clz+1;
			// if kraft_abs_delta is an exact power of 2, you do not do this branch,
			//	if it is not an exact power of 2, you do need start_cl--
			if ( start_cl > 2 && kraft_one_shifted[start_cl-1] < kraft_abs_delta*2 )
			{
				RR_ASSERT( ! rrIsPow2(kraft_abs_delta) );
				start_cl--;
			}
			else
			{
				RR_ASSERT( rrIsPow2(kraft_abs_delta) );
			}

			#ifdef RR_DO_ASSERTS
			// check it :
			{
				// start_cl step should be allowed :
				U32 kraft_delta = kraft_one_shifted[start_cl];
				RR_ASSERT( kraft_delta < kraft_abs_delta*2 );
				
				if ( start_cl > 1 )
				{
					// start_cl-1 step should not be allowed :
					U32 kraft_delta_below = kraft_one_shifted[start_cl-1];
					RR_ASSERT( kraft_delta_below >= kraft_abs_delta*2 );
				}
			}
			#endif
			
			//for(int cl=2;cl<=limitCodeLen;cl++)
			for(int cl=start_cl;cl<=limitCodeLen;cl++)
			{
				// best one to increase will be the lowest charcount at each CL
				int cl_first = cl_start[cl];
				int cl_len = cl_start[cl-1] - cl_first;
				if ( cl_len == 0 ) continue;
				
				RR_ASSERT( cl_first < gotNumSymbols );
				RR_ASSERT( table[cl_first].count == cl );
				
				//U32 kraft_delta = kraft_one_shifted[cl];
				//if ( kraft_delta >= kraft_abs_delta*2 ) continue; // don't overshoot by more than we started with
				RR_ASSERT( kraft_one_shifted[cl] < kraft_abs_delta*2 );
				
				// highest count symbol in my interval is the last one
				int i = cl_first + cl_len - 1;
				
				int sym = table[i].symbol;
				S32 score = (S32)(charCounts[sym] << cl);
				if ( score > best_score )
				{
					best_score = score;
					best_i = i;
				}
			}
			
			RR_ASSERT( best_i != -1 ); // there's always a valid move
				
			// do cl-- :
			int cl = table[best_i].count;
			RR_ASSERT( cl >= 2 && cl <= limitCodeLen );
			
			// correct kraft , remove old and add new :
			//kraft -= kraft_one >> cl;
			//kraft += kraft_one >> (cl-1);
			
			U32 kraft_delta = kraft_one_shifted[cl];
			kraft += kraft_delta;
			
			// error should have gone down: 
			RR_ASSERT( RR_ABS((S64)kraft - (S64)kraft_one) < kraft_abs_delta );
					
			cl --;
			cl_start[cl] --;
			table[best_i].count = check_value_cast<CHAR_COUNT_TYPE>( cl );
		}
    }
    
    
    if ( kraft != kraft_one )
    {
		//SIMPLEPROFILE_SCOPE(Limited_fallback); // near zero time

		// handle failed cases
		
		/**
		
		this is the old heuristic :
		
		first increase codelens to satisfy kraft requirement
			overshooting allowed
			take best score step each time
		
		then decrease codelens as possible if we overshot
			do not go below kraft
		
		always converges to kraft == kraft_one
		
		**/
		
		
		while ( kraft > kraft_one )
		{
			// find best cl to ++
			
			//U32 kraft_abs_delta = check_value_cast<U32>( kraft - kraft_one );
			
			S32 best_score = RR_S32_MIN;
			int best_i = -1;
			
			for(int cl=1;cl<limitCodeLen;cl++)
			{
				// best one to increase will be the lowest charcount at each CL
				int cl_first = cl_start[cl];
				int cl_len = cl_start[cl-1] - cl_first;
				if ( cl_len == 0 ) continue;
				
				RR_ASSERT( cl_first < gotNumSymbols );
				RR_ASSERT( table[cl_first].count == cl );
				
				// overshooting allowed here in fallback
				//U32 kraft_delta = kraft_one_shifted[cl+1];
				//if ( kraft_delta >= kraft_abs_delta*2 ) continue; // don't overshoot by more than we started with
				
				int sym = table[cl_first].symbol;
				S32 score = -(S32)(charCounts[sym] << cl);
				if ( score > best_score )
				{
					best_score = score;
					best_i = cl_first;
				}
			}
			
			RR_ASSERT( best_i != -1 );
				
			// do cl++ :
			int cl = table[best_i].count;
			RR_ASSERT( cl != 0 && cl < limitCodeLen );
			
			// correct kraft , remove old and add new :
			//kraft -= kraft_one >> cl;
			//kraft += kraft_one >> (cl+1);
			
			U32 kraft_delta = kraft_one_shifted[cl+1];
			kraft -= kraft_delta;
			
			cl_start[cl] ++;
			
			cl ++;
			table[best_i].count = check_value_cast<CHAR_COUNT_TYPE>( cl );
		}
	    
	    
		// we may have gone too far, see if we can get some back :
		
		while ( kraft < kraft_one )
		{    
			/**
			
			find symbols to do cl-- on :
			
			we have some kraft budget to spend
			we want to gain as many bits as possible
			
			score is just the inverse of the previous heap
			F64 score = (F64)charCounts[s] / (F64)kraft_delta;
			
			**/
	    
			//U32 kraft_abs_delta = check_value_cast<U32>( kraft_one - kraft );
			
			S32 best_score = RR_S32_MIN;
			int best_i = -1;
			
			for(int cl=2;cl<=limitCodeLen;cl++)
			{
				// best one to increase will be the lowest charcount at each CL
				int cl_first = cl_start[cl];
				int cl_len = cl_start[cl-1] - cl_first;
				if ( cl_len == 0 ) continue;
				
				RR_ASSERT( cl_first < gotNumSymbols );
				RR_ASSERT( table[cl_first].count == cl );
				
				U32 kraft_delta = kraft_one_shifted[cl];
				if ( kraft + kraft_delta > kraft_one ) continue; // don't overshoot
				
				// highest count symbol in my interval is the last one
				int i = cl_first + cl_len - 1;
				
				int sym = table[i].symbol;
				S32 score = (S32)(charCounts[sym] << cl);
				if ( score > best_score )
				{
					best_score = score;
					best_i = i;
				}
			}
			
			RR_ASSERT( best_i != -1 );
				
			// do cl-- :
			int cl = table[best_i].count;
			RR_ASSERT( cl >= 2 && cl <= limitCodeLen );
			
			// correct kraft , remove old and add new :
			//kraft -= kraft_one >> cl;
			//kraft += kraft_one >> (cl-1);
			
			U32 kraft_delta = kraft_one_shifted[cl];
			kraft += kraft_delta;
			
			cl --;
			cl_start[cl] --;
			table[best_i].count = check_value_cast<CHAR_COUNT_TYPE>( cl );
		}
		
    } // failed case handler
    
	RR_ASSERT( kraft == kraft_one );

	// read codelens out of table[]

    S32 minCodeLen = table[HI->gotNumSymbols-1].count;
    S32 maxCodeLen = table[0].count;    
    
	RR_ASSERT( maxCodeLen <= limitCodeLen );
	RR_ASSERT( minCodeLen > 0 );

    U8 * codeLenTable = HI->codeLenTable;
    S32 * numCodesOfLen = HI->numCodesOfLen;
    
    RR_DURING_ASSERT( U64 Kraft_recheck = 0; );
    RR_DURING_ASSERT( U64 Kraft_recheck2 = 0; );
    
	//memset(numCodesOfLen,0,sizeof(HI->numCodesOfLen));
	for(int cl=1;cl<=limitCodeLen;cl++)
	{
		numCodesOfLen[cl] = cl_start[cl-1] - cl_start[cl];
		
		RR_DURING_ASSERT( Kraft_recheck2 += numCodesOfLen[cl] * kraft_one_shifted[cl] );
	}

	for(int i=0;i<HI->gotNumSymbols;i++)
	{
		int cl = table[i].count; // codelen
		
		RR_DURING_ASSERT( Kraft_recheck += kraft_one_shifted[cl] );
		
		RR_ASSERT( cl >= minCodeLen && cl <= maxCodeLen );
		
		codeLenTable[ table[i].symbol ] = (U8) cl;
		//  could make numCodesOfLen[] from the cl_starts instead
		//numCodesOfLen[ cl ] ++;
	}
    
    // always hits it :
    RR_ASSERT( Kraft_recheck == kraft_one );
    RR_ASSERT( Kraft_recheck2 == kraft_one );
    
	HI->maxCodeLen = maxCodeLen;
	HI->minCodeLen = minCodeLen;
	
    return;
}

void rrHuffman_BuildCodeLens_Moffat_InPlace(rrHuffman *HI,S32 limitCodeLen,rrHuffman_CharAndCount * table)
{
	SIMPLEPROFILE_SCOPE(BuildCore_Moffat);

    // fill codeLenTable and numCodesOfLen :
    U8 * codeLenTable = HI->codeLenTable;
    S32 * numCodesOfLen = HI->numCodesOfLen;
    bool hit_codelen_limit = false;

    // make code lens :
    //  code from "moffat_huffman_inplace.c"
	// + small CB edits
	// see main_huffman_builder.cpp
	//	you can in fact do a slightly faster huff build than this
	//		(branchy vs branchless seem to be about the same speed)
	//	if you are willing to go non-in-place
	//	use [2N] mem for all leaves & nodes
	//	Moffat only uses [N] by making the leaves implicit

    rrHuffman_CharAndCount * Atable = table;
    int n = HI->gotNumSymbols;
        
    #define AVAL(n) Atable[n].count
    
    int root;                  /* next root node to be used */
    int leaf;                  /* next leaf to be used */
    int next;                  /* next value to be assigned */

    /* check for pathological cases */
    //if (n==0) { return false; }
    //if (n==1) { AVAL(0) = 0; return false; }
	RR_ASSERT( n >= 2 );

	// sentry : to eliminate leaf < n check :
	// (encodeTableBytes uses numSymbols+1 so this is safe)
	// tot is <= MAX_CHAR_COUNT
	// therefore there can be no symbol == MAX_CHAR_COUNT
	// because if there was it would be the only symbol
	RR_ASSERT( AVAL(n-1) < MAX_CHAR_COUNT );
	AVAL(n) = MAX_CHAR_COUNT;
		
    /* first pass, left to right, setting parent pointers */
	// CB : there's 3 ranges in the tree here
	//	[leaf -> n) = input char counts
	//  [root -> next) = counts of nodes we made
	//  [0 -> root) = output of this loop, contains parent
	//	"root" = next_node
	//	"next" = node_end
	// it starts with the whole array full of leaf counts
	// you consume pairs of leaves to make nodes (node = sum of 2 counts)
	// as you consume leaves, you replace them with nodes
	// when you consume nodes, you replace them with their parent index
	//	and that's the output
	// eventually all leaves must be turned into nodes to get into the tree
	//   so the leaf range is replaced with node range
	//  and all nodes must be consumed to become a single root node
	// at that point the whole range is "consumed nodes" which is the parent-index tree
	// because we only have N entries
	// we only make the parent-index tree for the internal nodes
	// the leaves are implicit
	// the number of leaves at each level can be figured out from the # of nodes
				
    AVAL(0) = check_value_cast<CHAR_COUNT_TYPE>( AVAL(0) + AVAL(1) ); 
    root = 0; 
    leaf = 2;
	next = 1;
    //for (; next < n-1; ) 
    while(leaf < n) // until all leaves are in the tree
	{
        /* select first item for a pairing */
		// there must be at least one node to consume here
		//	so no root<next check is needed

		int sum;
        if ( AVAL(root)<AVAL(leaf)) 
        {
			// consume a node (count) and write a parent index to the output range
			sum = AVAL(root); 
			AVAL(root++) = check_value_cast<CHAR_COUNT_TYPE>( next );
        }
        else
        {
			sum = AVAL(leaf++);
		}
			
        /* add on the second item */
		// could be out of nodes here

        if ( (root<next && AVAL(root)<AVAL(leaf))) 
        {
			// consume a node (count) and write a parent index to the output range
			sum += AVAL(root);
			AVAL(root++) = check_value_cast<CHAR_COUNT_TYPE>( next );
        }
		else
		{
			sum += AVAL(leaf++);
		}

		// make a new node, write sum of counts :
		AVAL(next++) = check_value_cast<CHAR_COUNT_TYPE>( sum );
    }

	// only nodes left :
	//  simpler loop to finish adding all nodes to the tree
	while( next < n-1)
	{
		// no need to actually sum the counts any more :
		//	fill parent indexes :
		AVAL(root++) = check_value_cast<CHAR_COUNT_TYPE>( next );
		AVAL(root++) = check_value_cast<CHAR_COUNT_TYPE>( next );
		next++;
	}
        
    /* second pass, right to left, setting internal depths */
    AVAL(n-2) = 0;
    for (next=n-3; next>=0; next--)
    {
		int parent = AVAL(next);
		AVAL(next) = AVAL(parent)+1;
	}

	// you can get max depth before reading out codelens :
	int max_depth = AVAL(0) + 1;
	hit_codelen_limit = max_depth > limitCodeLen;
	//lprintfvar(max_depth);
		
	// the Moffat in-place doesn't explicitly set depths for leaves
	//	it only makes node parents, thus using only [N] not [2N]
	// so we must make the leaf depths implicitly
	//  that's extra work that non-in-place doesn't have to do
	// but it also gives us numCodesOfLen[]
	//	so it's not terrible wasted work

	// already done :
	//memset(numCodesOfLen,0,sizeof(numCodesOfLen[0])*32);

    /* third pass, right to left, setting leaf depths */
    int avbl = 1; 
    int dpth = 0; 
    root = n-2; // node index we scan back
    next = n;   // start of next leaf symbol range to fill
	for(;;)
    {
		int used = 0; // used = # of nodes at this depth
        while (root>=0 && AVAL(root)==dpth) 
        {
			root--;
			used++;
        }

		int count = avbl - used; // # of leaves at this depth
		if ( count > 0 )
		{
			// fill leaf depths = symbol codelens
			// this is just memsets of codelen
			// starting from lowest codelen (highest counts are last)
			next -= count;

			/*
			// @@ can just one or the other
			//	(beware minCodeLen below)
			//
			// table[] is in sorted-by-count order
			// codeLenTable[] is in original symbol order
			// if we hit codelen limit, we need to fill table[]
			//	 as it's read by rrHuffman_BuildCodeLens_Limited_Heuristic
			//	 (PackageMerge starts over from scratch)
			// if we did not hit codelen limit,
			/	  then table[] will not be used anymore
			//	 and codeLenTable[] is our final output
			if ( hit_codelen_limit )
			{
				for LOOP(i,count)
				{
					AVAL(next+i) = check_value_cast<CHAR_COUNT_TYPE>( dpth );
				}
			}
			else
			{
				for LOOP(i,count)
				{
					codeLenTable[ table[next+i].symbol ] = (U8) dpth;
				}
			}
			*/
				
			for LOOP(i,count)
			{
				AVAL(next+i) = check_value_cast<CHAR_COUNT_TYPE>( dpth );
				codeLenTable[ table[next+i].symbol ] = (U8) dpth;
			}

			numCodesOfLen[dpth] = count;

			if ( next == 0 ) break; // filled all symbols
		}

		RR_ASSERT( used > 0 );

		// each node has 2 kids at next depth :
        avbl = 2*used; 
        dpth++; 

		if ( dpth == limitCodeLen )
		{
			// apply codelen limit
			// all remaining leaves must be at this len
			// note this does not mean hit_codelen_limit
			//	if limitCodeLen is never exceeded
				
			count = next;
			next = 0;
			for LOOP(i,count)
			{
				AVAL(i) = check_value_cast<CHAR_COUNT_TYPE>( dpth );
					
				// @@ can skip filling codeLenTable[] if hit_codelen_limit
				codeLenTable[ table[i].symbol ] = (U8) dpth;
			}

			numCodesOfLen[dpth] = count;
			break;
		}
    }

	//RR_ASSERT( max_depth == AVAL(0) ); // no, AVAL is clamped to limitCodeLen
		
	HI->minCodeLen = table[HI->gotNumSymbols-1].count;
	HI->maxCodeLen = max_depth; // non-clamped
    
	RR_ASSERT( HI->maxCodeLen < 31 );
	RR_ASSERT( HI->minCodeLen > 0 );
		
	/*
	for(int i=0;i<HI->gotNumSymbols;i++)
	{
		int cl = table[i].count; // codelen
			
		codeLenTable[ table[i].symbol ] = (U8) cl;
		numCodesOfLen[ cl ] ++;
	}
	*/
}


// rrHuffman_BuildCodeLens_Branchy needs 2N storage in table[]
//	in-place in table[]
//	changes counts to lens
void rrHuffman_BuildCodeLens_Branchy(rrHuffman *HI,S32 limitCodeLen,rrHuffman_CharAndCount * table)
{
	SIMPLEPROFILE_SCOPE(BuildCore_Branchy);

	// leaf_count is actual number of symbols; leaf_count <= alphabet
	int leaf_count = HI->gotNumSymbols;
	
    rrHuffman_CharAndCount * Atable = table;
	// lowest first :
	RR_ASSERT( AVAL(0) <= AVAL(1) );
	
	// sentry at end :
	// large count so it's never chosen
	const CHAR_COUNT_TYPE c_sentry_count = MAX_CHAR_COUNT;
	AVAL(leaf_count) = c_sentry_count;
	
	// nodes are just past leaves :
	int node_base = leaf_count + 1;
	
	int next_leaf = 0;
	int next_node = node_base;
	int node_end = node_base;
	
	/**
	
	leaves & nodes are sorted by count; lowest at the head of each queue
	pop the lowest two from the two queues
	keep going until all symbols are in the tree
	
	in the active range (unconsumed leaves & nodes) count is the count
	once they get into the tree, count is no longer needed
	the field is reused to be "parent"
	
	consuming off the end of each list is prevented with a sentry of large count
	
	**/
	
	{
		// make first node :
		// so that in the loop body we always have >= 1 node
		int parent_count = AVAL(0) + AVAL(1);
		AVAL(0) = AVAL(1) = check_value_cast<CHAR_COUNT_TYPE>( node_end );
		next_leaf = 2;
		AVAL(node_end) = check_value_cast<CHAR_COUNT_TYPE>( parent_count );
		node_end++;
	}
	
	while( next_leaf < leaf_count )
	{
		// we always have at least one node & one leaf to pop :
		RR_ASSERT( next_node < node_end ); 
		
		// take a leaf or a node, whichever is cheaper (twice)
		int next_leaf_count = AVAL(next_leaf);
		int next_node_count = AVAL(next_node);
		int parent_count;
		
		// does it matter how I break the tie?
		// (it doesn't for entropy, but maybe for max code len?)
		// -> when counts are equal, prefer taking leaves first
		
		if ( next_leaf_count <= next_node_count )
		{
			parent_count = next_leaf_count;
			AVAL(next_leaf++) = check_value_cast<CHAR_COUNT_TYPE>( node_end ); // count is now parent
			next_leaf_count = AVAL(next_leaf);
		}
		else
		{
			parent_count = next_node_count;
			AVAL(next_node++) = check_value_cast<CHAR_COUNT_TYPE>( node_end ); // count is now parent
		
			#if 1
			// next_node == node_end is possible now
			// but it's very rare; highly predictable branch
			// not needed if you do the sentry fill option above
			if ( next_node == node_end )
			{
				// must pop a leaf :
				parent_count += next_leaf_count;
				AVAL(next_leaf++) = check_value_cast<CHAR_COUNT_TYPE>( node_end ); // count is now parent
				
				AVAL(node_end) = check_value_cast<CHAR_COUNT_TYPE>( parent_count );
				node_end++;
				continue;
			}
			#endif
		
			next_node_count = AVAL(next_node);
		}
		
		//parent_count += MIN(next_leaf_count,next_node_count);
		if ( next_leaf_count <= next_node_count )
		{
			parent_count += next_leaf_count;
			AVAL(next_leaf++) = check_value_cast<CHAR_COUNT_TYPE>( node_end ); // count is now parent
		}
		else
		{
			parent_count += next_node_count;
			AVAL(next_node++) = check_value_cast<CHAR_COUNT_TYPE>( node_end ); // count is now parent
		}
				
		// if we consumed sentry, count would go negative
		RR_ASSERT( parent_count >= 2 );
		
		AVAL(node_end) = check_value_cast<CHAR_COUNT_TYPE>( parent_count );
		node_end++;
		
		//AVAL(node_end) = c_sentry_count;
	}
	
	// all symbols are now in the tree
	// there should be at least one available node (the root)
	// if there are more nodes than that, pair them up
	//	(this phase could be removed; it could be done implicitly)
	//	(this could also just be handled by the previous loop)
	RR_ASSERT( next_node < node_end );
	int final_node_end = node_base + leaf_count-1;
	
	while( node_end < final_node_end )
	{		
		// AVAL() is now parent
		AVAL(next_node  ) = check_value_cast<CHAR_COUNT_TYPE>( node_end );
		AVAL(next_node+1) = check_value_cast<CHAR_COUNT_TYPE>( node_end );
		next_node += 2;
		node_end++;
	}
		
	// read out depths :
	// (replace count with depth)

	// read count = parent, write count = depth
	// set depth of root node to 0 :	
	AVAL(node_end-1) = 0;
	AVAL(leaf_count) = 0; // make the sentry not access violate
	for(int i = node_end-2; i >= 0;i--)
	{
		int parent = AVAL(i); // read parent
		RR_ASSERT( parent > i || i == leaf_count ); // AVAL( > i ) are depth, not parent
		int depth  = AVAL(parent) + 1; // AVAL(parent) has already been converted to depth
		AVAL(i) = check_value_cast<CHAR_COUNT_TYPE>( depth ); // store depth
	}
	
    // fill codeLenTable and numCodesOfLen :
    U8 * codeLenTable = HI->codeLenTable;
    S32 * numCodesOfLen = HI->numCodesOfLen;
	
	HI->minCodeLen = table[HI->gotNumSymbols-1].count;
	HI->maxCodeLen = table[0].count; // non-clamped
    
	RR_ASSERT( HI->maxCodeLen < 31 );
	RR_ASSERT( HI->minCodeLen > 0 );

	// already done :
	//memset(numCodesOfLen,0,sizeof(numCodesOfLen[0])*32);
	// @@ they're in sorted order, probably faster to scan while == than to histogram naively :
	#if 0
	for LOOP(i,leaf_count)
	{
		int cl = AVAL(i);
		if ( cl >= limitCodeLen )
		{
			AVAL(i) = check_value_cast<CHAR_COUNT_TYPE>( limitCodeLen ); // Heuristic will read this
			codeLenTable[ table[i].symbol ] = (U8) limitCodeLen; // <- NOT needed
			numCodesOfLen[ limitCodeLen ] ++;
		}
		else
		{
			codeLenTable[ table[i].symbol ] = (U8) cl;
			numCodesOfLen[ cl ] ++;
		}
	}
	#endif

	int sorti = 0;
	AVAL(leaf_count) = 0; // sentry
	while( AVAL(sorti) >= limitCodeLen )
	{
		AVAL(sorti) = check_value_cast<CHAR_COUNT_TYPE>( limitCodeLen );
		codeLenTable[ table[sorti].symbol ] = (U8) limitCodeLen;
		sorti++;
	}
	numCodesOfLen[ limitCodeLen ] = sorti;
	for(;;)
	{
		int cl = AVAL(sorti);
		if ( cl == 0 ) break;
		int starti = sorti;
		
		do
		{
			codeLenTable[ table[sorti].symbol ] = (U8) cl;
			sorti++;
		}
		while( AVAL(sorti) == cl );

		numCodesOfLen[ cl ] = sorti - starti;
	}
}
		
/**

BuildCodeLens uses Moffat's in-place code so it does no allocations

// charCounts may be scaled by BuildCodeLens but is otherwise untouched

sets :

    gotNumSymbols
    numCodesOfLen
    minCodeLen,maxCodeLen,
    codeLenTable    

**/
void rrHuffman_BuildCodeLens(rrHuffman *HI,const U32 *charCounts,U32 charCountSum,S32 limitCodeLen,
	rrArenaAllocator * arena,
	rrbool do_package_merge)
{
	SIMPLEPROFILE_SCOPE(BuildCodeLens);
	
    int numSymbols = HI->numSymbols;
    HI->minCodeLen = HI->maxCodeLen = 0;
    
    memclear(HI->numCodesOfLen,sizeof(HI->numCodesOfLen));
	memclear(HI->codeLenTable,sizeof(HI->codeLenTable[0])*numSymbols);

    RR_ASSERT( limitCodeLen <= RR_HUFFMAN_ENCODE_CODELEN_LIMIT );
    RR_COMPILER_ASSERT( RR_HUFFMAN_ENCODE_CODELEN_LIMIT <= RR_HUFFMAN_MAX_CODELEN_LIMIT );

    // limitCodeLen must be at most RR_HUFFMAN_MAX_CODELEN_LIMIT
    //	because NewVarbits on 32 bit only guarantees 25 bits available
	if ( limitCodeLen <= 0 || limitCodeLen > RR_HUFFMAN_ENCODE_CODELEN_LIMIT )
		limitCodeLen = RR_HUFFMAN_ENCODE_CODELEN_LIMIT;
	RR_COMPILER_ASSERT( RR_MINBITSAVAILABLE >= RR_HUFFMAN_MAX_CODELEN_LIMIT );
	
    
    #ifdef DO_BUILD_CODE_LEN_NO_ALLOC
    
    RR_ASSERT( charCountSum == rrSumOfHistogram(charCounts,HI->numSymbols) );
    
    rrScopeArenaAlloc scaled_counts_space;
    
    if ( charCountSum > (U32)MAX_TOT_COUNT )
    {   
		SIMPLEPROFILE_SCOPE(rrHuffman_ScaleCounts);

		// I put counts in a U16
		//	so have to Scale to make sure they fit :
		//  the *total* must fit in a word because they get accumulated in place to make the tree
		
		U32 * dest = (U32 *) scaled_counts_space.Alloc( sizeof(U32) * HI->numSymbols , arena );
		RR_ASSERT_ALWAYS( dest != NULL );
		// charCounts is changed to pointing at dest if scaling was done :
		charCounts = rrHuffman_ScaleCounts(HI,charCounts,dest,MAX_CHAR_COUNT,MAX_TOT_COUNT);
	}
    
    #endif
    
	#ifdef DO_MOFFAT_INPLACE
    // use the encodeTable as work space
	rrHuffman_CharAndCount * table = (rrHuffman_CharAndCount *) HI->encodeTable;
    #else
	// for Branchy :
	// @@ only do this if numSymbols <= 256 (eg. newlz but not LZH)
	//	fack back to Moffat for larger alphabets
    RR_SCOPE_ARENA_ARRAY(table,rrHuffman_CharAndCount,HI->numSymbols*2,arena);
    #endif

	{
	SIMPLEPROFILE_SCOPE(Fill);

	// "Fill" is just reading charCounts[] , [256] for newlz
	//	and filling a dense-packed {sym,count} array [gotNumSymbols] of them
	// for newlz [gotNumSymbols] is often quite small (for packets & offsets)
	//	-> this is stupid slow
	//	-> just scanning the zero
	// before optimizing this was 1/3 of the BuildCodeLens time
	//	roughly 1/3 in Sort, 1/3 in Core (Moffat), and 1/3 here!
		
	#ifdef __RADLITTLEENDIAN__
	rrHuffman_CharAndCount * pt = table;
	RR_COMPILER_ASSERT( sizeof(rrHuffman_CharAndCount) == 4 );
		
	int symi = 0;
    for(;symi<(numSymbols&~7);symi += 8)
    {
		// branchless : MUCH FASTER
		
		#if 1 // <- pretty marginal whether this helps overall or not
		// are all eight zero?
		//	common case :
		//	newlzf offset upper byte
		//	if you're in a small chunk, say a 4k chunk
		//	the upper byte only ever has the bottom values non-zero
		//	then the whole top part of the alphabet is zero
		// -> quick early out runs of zeros
		U64 * pc = (U64 *)(charCounts+symi);
		U64 test = pc[0] | pc[1] | pc[2] | pc[3];
		if ( test == 0 ) continue;
		#endif

		RR_UNROLL_I_8(symi, \
			RR_PUT32_LE(pt,i + (charCounts[i]<<16)); \
			pt += (charCounts[i] > 0); \
		);
    }
    for(;symi<numSymbols;symi++) // never used for newlz, numSymbols is always a multiple of 8
    {
		U32 cc = charCounts[symi];
		RR_PUT32_LE(pt,symi + (cc<<16));
		pt += (cc > 0);
    }
	int ti = (int)( pt - table );
	#else
    int ti = 0;
    for(int i=0;i<numSymbols;i++)
    {
		if ( charCounts[i] == 0 )
			continue;
		
        table[ti].symbol = check_value_cast<PACKEDSYM_TYPE>( i );
        table[ti].count  = check_value_cast<CHAR_COUNT_TYPE>( charCounts[i] );
	    ti++;
    }
	#endif

    HI->topSym = table[ti-1].symbol;
    HI->gotNumSymbols = ti;
	}
    		
    if ( HI->gotNumSymbols < 2 )
    {    
		// one symbol is a special case, because we want to decode it, but it has no code len
		// this handles gotNumSymbols == 0 also , though "oneChar" is garbage in that case
        HI->oneChar = table[0].symbol;
            
	    #ifndef DO_BUILD_CODE_LEN_NO_ALLOC
	    OodleFree(table);
		#endif
		//if ( allocatedCharCounts )
		//	OodleFree(allocatedCharCounts);
        return;
    }
    
    // sort syms by count - lowest count first :
    rrHuffman_Sort(table,HI->gotNumSymbols,arena);
        
    RR_ASSERT( table[1].count >= table[0].count );
    // set oneChar to the MPS :
    HI->oneChar = table[HI->gotNumSymbols-1].symbol;
    
	// table[] starts with char counts in sorted order (lowest count to highest)
	// table[] comes out with codelens in sorted order (highest codelen to lowest)
	// HI->codeLenTable[] gets codelens in alphabet order, with zero in gaps
	
	#ifdef DO_MOFFAT_INPLACE
	rrHuffman_BuildCodeLens_Moffat_InPlace(HI,limitCodeLen,table);
	#else
	rrHuffman_BuildCodeLens_Branchy(HI,limitCodeLen,table);
	#endif

	bool hit_codelen_limit = HI->maxCodeLen > limitCodeLen;

	if ( hit_codelen_limit )
	{
		SIMPLEPROFILE_SCOPE(Limit);

		if ( do_package_merge )
		{
			// PackageMerge just starts over from scratch & does everything
			rrHuffman_BuildCodeLens_Limited_PackageMerge(HI,charCounts,limitCodeLen,arena);		
		}
		else
		{
			// Heuristic reads from table[] and numCodesOfLen[]
			//	it now assumes you have already done the clamp to limitCodeLen
			rrHuffman_BuildCodeLens_Limited_Heuristic(HI,charCounts,limitCodeLen,table,arena);
			
			/*
			// log the difference between Heuristic and Package Merge :
			if ( 1 )
			{
				U32 cl_heuristic = CodeLenOfHistogram(charCounts,HI->codeLenTable,256);
			
				rrHuffman_BuildCodeLens_Limited_PackageMerge(HI,charCounts,limitCodeLen,arena);
				
				U32 cl_pm = CodeLenOfHistogram(charCounts,HI->codeLenTable,256);
			
				rrprintf("pain %d : %d - %d = %d = %.2f %%\n",pain,cl_heuristic,cl_pm,cl_heuristic-cl_pm,(cl_heuristic-cl_pm)*100.0/cl_pm);
			}
			/**/
		}		
	}
	
	RR_ASSERT_ALWAYS( HI->maxCodeLen <= limitCodeLen );
	
    #ifndef DO_BUILD_CODE_LEN_NO_ALLOC
    OodleFree(table);
	#endif
	
	//if ( allocatedCharCounts )
	//	OodleFree(allocatedCharCounts);
	
    return;
}

/*
 * at this point codeLenTable & numCodesOfLen are filled out
 *
 */
rrbool rrHuffman_BuildEncodeTable(rrHuffman *HI)
{
    if ( HI->gotNumSymbols < 2 )
    {
		if ( HI->gotNumSymbols == 0 )
		{
			HI->oneChar = 0;
		}
	    
	    RR_ASSERT( HI->codeLenTable[HI->oneChar] == 0 );
		HI->encodeTable[HI->oneChar] = 0;
        
        return true;
    }
    
    if ( HI->maxCodeLen == 0 )
        return false;
    if ( HI->maxCodeLen > 30 )
        return false;

    int numSymbols = HI->numSymbols;
    
    RR_VARBITSTYPE * codePrefixByLen = HI->codePrefixByLen;
    U8 * codeLenTable = HI->codeLenTable;
    S32 * numCodesOfLen = HI->numCodesOfLen;

    // codePrefixByLen is the starting code at each codelen
    //  (same loop in BuildDecodeTable)
    U32 LastCodePrefix = 0;
	codePrefixByLen[0] = 0; // put junk in for codelen == 0
    codePrefixByLen[HI->minCodeLen] = 0;
    for(int i=(HI->minCodeLen);i<(HI->maxCodeLen);)
    {
        LastCodePrefix = (LastCodePrefix + numCodesOfLen[i]) << 1;
        i++;
        codePrefixByLen[i] = LastCodePrefix;
    }
	
	// fill encodeTable
    U32 * charToCodeTable = HI->encodeTable;

    // read codes out of codePrefixByLen
    //  and increment it as we take up codes
    for(int i=0;i<numSymbols;i++)
    {
        int curCodeLen = codeLenTable[i];
        //if ( curCodeLen > 0 ) // @@ this branch is optional
        {
            charToCodeTable[i] =  check_value_cast<U32>( codePrefixByLen[curCodeLen] );
            codePrefixByLen[curCodeLen] += 1;
        }
    }
    
    // codePrefixByLen is now junk and not used any more

    return true;
}

void rrHuffman_PrintCodes(rrHuffman * HI)
{
    for(int i=0;i<HI->numSymbols;i++)
    {
        if ( HI->codeLenTable[i] > 0 )
        {
            rrprintf("%d : %d : ",i,HI->codeLenTable[i]);
            if ( HI->encodeTable )
                rrPrintfBinary(HI->encodeTable[i],HI->codeLenTable[i]);
            rrprintf("\n");
        }
    }
}

void rrHuffman_PrintCodeLens(rrHuffman * HI)
{
    rrprintf("rrHuffman : gotNumSymbols : %d , log2 : %f\n",
        HI->gotNumSymbols,
        rrlog2_F64((float)HI->gotNumSymbols));
    
    double sum = 0.0,sumSqr = 0.0;
        
    for(int i=HI->minCodeLen;i<=HI->maxCodeLen;i++)
    {
        int num = HI->numCodesOfLen[i];
        if ( num > 0 )
        {
            float P = 100.f*num / HI->gotNumSymbols;
            rrprintf("%d : %d , %f%%\n",i,num,P);
            
            sum += num * i;
            sumSqr += num * i * i;
        }
    }
    sum /= HI->gotNumSymbols;
    sumSqr /= HI->gotNumSymbols;
    sumSqr -= sum*sum;
    rrprintf(" mean : %f , sdev : %f\n",sum,sqrt(sumSqr));
    
    // stuff that's useful for studying the compression of the code lens for transmission :
    #if 0
    
    rrprintf(" entropy : %f\n",rrEntropyOfHistogram((U32 *)HI->numCodesOfLen,32));
    
    int imean = (int)(sum + 0.5f);
    
    rrprintf("NumDiffFromMean : \n");
    U32 NumDiffFromMean[32] = { 0 };
    
    for(int i=HI->minCodeLen;i<=HI->maxCodeLen;i++)
    {
        int num = HI->numCodesOfLen[i];
        int d = i - imean;
        d = rrFoldUpNegatives(d);
        NumDiffFromMean[d] += num;
    }
    for(int d=0;d<32;d++)
    {
        int num = NumDiffFromMean[d];
        if ( num > 0 )
        {
            float P = 100.f*num / HI->gotNumSymbols;
            rrprintf("%d : %d , %f%%\n",d,num,P);
        }
    }
    rrprintf(" entropy : %f\n",rrEntropyOfHistogram(NumDiffFromMean,32));
    
    rrprintf("NumDeltaFromPrevious : \n");
    U32 NumDeltaFromPrevious[32] = { 0 };
    int prev = imean;
    for(int i=0;i<HI->numSymbols;i++)
    {
        if ( HI->codeLenTable[i] > 0 )
        {
            int delta = (int)HI->codeLenTable[i] - prev;
            delta = rrFoldUpNegatives(delta);
            NumDeltaFromPrevious[delta] ++;
            prev = HI->codeLenTable[i];
        }
    }
    for(int d=0;d<32;d++)
    {
        int num = NumDeltaFromPrevious[d];
        if ( num > 0 )
        {
            float P = 100.f*num / HI->gotNumSymbols;
            rrprintf("%d : %d , %f%%\n",d,num,P);
        }
    }
    rrprintf(" entropy : %f\n",rrEntropyOfHistogram(NumDeltaFromPrevious,32));
    
    #endif
}


void rrHuffman_PrintEntropies(rrHuffman * HI,U32 * charCounts)
{
    double TrueEntropy = rrEntropyOfHistogram(charCounts,HI->numSymbols);

    U32 tot = 0;
    for(int i=0;i<HI->numSymbols;i++)
    {
        tot += charCounts[i];
    }
    double rrHuffman_Entropy = 0;
    for(int i=0;i<HI->numSymbols;i++)
    {
        if ( charCounts[i] > 0 )
        {
            rrHuffman_Entropy += charCounts[i] * HI->codeLenTable[i];
        }
    }
    rrHuffman_Entropy /= tot;
    
    rrprintf("True entropy : %6.3f , Huffman : %6.3f\n",TrueEntropy,rrHuffman_Entropy);
    
}

OODLE_NS_END
