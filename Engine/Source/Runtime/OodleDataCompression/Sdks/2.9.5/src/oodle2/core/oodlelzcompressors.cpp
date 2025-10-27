// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "oodlelzcompressors.h"
#include "oodlecoreinit.h"

#include "rrlzh_lzhlw_shared.h"
#include "rrvarbits.h"
#include "oodlemalloc.h"
#include "oodlejob.h" // for Oodle_IsJobSystemSet
#include "rrsimpleprofstub.h"
#include "cbradutil.h"
//#include "rrVarBitCodes.h"
#include "rrhuffman.h"
#include "threadprofiler.h"

#include "rrlogutil.h"
#include "oodleconfigvalues.h"
#include "rrstackarray.h"
#include "rrarenaallocator.h"

#include "cpux86.h"

#include "newlz.h"
#include "newlzhc.h"
#include "newlzf.h"
#include "lzb.h"

#include "newlz_arrays.h" // for NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH
#include "newlz_shared.h" // for NEWLZ_CHUNK_LEN

#include "oodlelzlegacyvtable.h"

//====================================

// @@ STACK USE DEBUGGING TOGGLE HERE
#if 0
#ifdef __RADNT__

#include <windows.h>
#include <winternl.h>

OODLE_NS_START
void log_stack_info()
{
	U32 x;
	U64 cur = (U64)&x;
	rrprintf("cur : %016I64X\n",cur);
	
	_TEB * teb = NtCurrentTeb();
	// TIB is right at the head of the TEB :
	NT_TIB* tib = (NT_TIB*) teb;

	// 32-bit :
	//NT_TIB* tib = (NT_TIB*)__readfsdword(0x18);
    // 64-bit :
    //NT_TIB* tib = (NT_TIB*)__readgsqword(0x30);
	U64 stackBottom = (U64) tib->StackLimit;
	U64 stackTop = (U64) tib->StackBase;

	rrprintf("bottom : %016I64X\n",stackBottom);
	rrprintf("top    : %016I64X\n",stackTop);
	rrprintf("stack size : %d\n",(int)(stackTop-stackBottom));
	rrprintf("used : %d\n",(int)(stackTop-cur));
	rrprintf("remaining : %d\n",(int)(cur-stackBottom));
	
	MEMORY_BASIC_INFORMATION membi;
	VirtualQuery((LPVOID)cur,&membi,sizeof(membi));
	
	U64 stackBase = (U64)membi.AllocationBase;
	
	rrprintf("mem base : %016I64X\n",stackBase);
	rrprintf("reserve size : %d\n",(int)(stackTop - stackBase);
	rrprintf("reserve remain : %d\n",(int)(cur - stackBase);
}
OODLE_NS_END

#endif
#endif

OODLE_NS_START

//====================================

// not pub ?

RADINLINE rrbool OodleLZ_Compressor_IsLegacy(OodleLZ_Compressor compressor)
{
	#ifdef OODLE_ALLOW_DEPRECATED_COMPRESSORS
	const U32 set = 
		OODLELZ_COMPRESSOR_MASK(OodleLZ_Compressor_BitKnit) |
		// OODLELZ_COMPRESSOR_MASK(OodleLZ_Compressor_LZB16) | // not LZB16
		OODLELZ_COMPRESSOR_MASK(OodleLZ_Compressor_LZNA) |
		OODLELZ_COMPRESSOR_MASK(OodleLZ_Compressor_LZH) |
		OODLELZ_COMPRESSOR_MASK(OodleLZ_Compressor_LZHLW) |
		OODLELZ_COMPRESSOR_MASK(OodleLZ_Compressor_LZNIB) |
		OODLELZ_COMPRESSOR_MASK(OodleLZ_Compressor_LZBLW) |
		OODLELZ_COMPRESSOR_MASK(OodleLZ_Compressor_LZA) |
		0;
	return OODLELZ_COMPRESSOR_BOOLBIT(set,compressor);
	#else
	return false;
	#endif
}

//====================================

RR_COMPILER_ASSERT( LZH_LZHLW_HUFFMAN_FAST_DECODE_BITS == 10 );

RR_COMPILER_ASSERT( OODLELZ_BLOCK_MAXIMUM_EXPANSION == OODLELZ_BLOCK_HEADER_BYTES_MAX );

/*

12-28-2015 : x64 :

compressor 0 : LZH , decoder memory : 6152
compressor 1 : LZHLW , decoder memory : 14328
compressor 2 : LZNIB , decoder memory : 168
compressor 3 : None , decoder memory : 168
compressor 4 : LZB16 , decoder memory : 168
compressor 5 : LZBLW , decoder memory : 168
compressor 6 : LZA , decoder memory : 19904
compressor 7 : LZNA , decoder memory : 11652
compressor 10 : BitKnit , decoder memory : 16208

*/
		
static S32 OodleLZ_Compressor_to_DecodeType(OodleLZ_Compressor compressor);
static OodleLZ_Compressor OodleLZ_DecodeType_to_Compressor(S32 decodeType);

//===========================================================================

// 3 * chunklen + NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH + headers/alignment pad
#define OODLELZ_COMPRESSOR_MAX_SCRATCH_MEM_SIZE		(3*128*1024 + NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH + OODLELZ_SCRATCH_ALIGNMENT_PAD + OODLELZ_MAX_SCRATCH_FOR_PHASE_HEADERS_AND_FUZZ)

SINTa OodleLZ_Compressor_ScratchMemSize(OodleLZ_Compressor compressor,SINTa rawLen)
{
	// max scratch needed for K/M/S for chunks that don't expand

	// "Invalid" was changed to "LZA"
	//	and that should act like Leviathan
	//if ( compressor == COMPRESSOR_THAT_NEEDS_MOST_MEMORY )
	//	compressor = OodleLZ_Compressor_Invalid;
	// -> in this function you should check == K/M/S and if those don't match, act like it's Leviathan

	#if 0
	// 07-01-2019 :
	//	fixing this more properly now
	//	use GetAllChunksCompressor() to detect Hydra
	
	// 04-01-2019 :
	// bug : was using whole rawLen, not limited to chunkLen
	//	that meant OODLELZ_COMPRESSOR_MAX_SCRATCH_MEM_SIZE was always being used on large files
	//	even for Mermaid/Selkie where it wasn't necessary
	// that actually was a good thing because it hid a bug with Hydra

	if ( rawLen > OODLELZ_BLOCK_LEN )
	{
		// if rawLen is more than one block, it might be Hydra
		// I'm asking for ScratchMemSize based on "compressor" from the first block
		// next block might not be the same
	
		// Hydra issue :
		//	if we have multiple blocks
		//	we must allocate enough for Kraken/Leviathan
		//	even if "compressor" is Mermaid/Selkie
	
		return OODLELZ_COMPRESSOR_MAX_SCRATCH_MEM_SIZE;
	}
	#endif
	
	if ( rawLen <= 0 )
	{
		// rawLen unknown or infinite
		rawLen = NEWLZ_CHUNK_LEN;
	}
	else
	{
		// limit to one 128k chunk :
		rawLen = RR_MIN(rawLen,NEWLZ_CHUNK_LEN);
	}
	
	SINTa maxScratchNeeded = rawLen*2 + OODLELZ_SCRATCH_ALIGNMENT_PAD;
	
	
	// Mermaid & Selkie need 2* len
	// Kraken & Leviathan need 3 * len
	// mainly due to indexed splits (which need their full array size as scratch)
	// but also for alt offsets extra space
	
	if ( compressor == OodleLZ_Compressor_Mermaid || compressor == OodleLZ_Compressor_Selkie )
	{
		// no Indexed arrays allowed
		//	-> assumption is Mermaid doesn't do Indexed so it can have a bit less
	}
	else
	{
		// Kraken, Leviathan, Hydra, Invalid
		// compressor actually == LZA for the "any" case (COMPRESSOR_THAT_NEEDS_MOST_MEMORY)
		maxScratchNeeded += rawLen;
	}
	
	// @@ could leave this out for Selkie
	//	(Selkie only uses uncompressed newlz_arrays)
	//	but it's dangerous to introduce differences between Mermaid & Selkie, so leave it
	// @@ when rawLen is small (32k or less), this is a significant part of decoder size
	//	-> actually this is impossible to omit for Selkie
	//	  because the decoder doesn't see Selkie data, it sees Mermaid
	//	  currently we verify the decoder size using the compressor flagged in the stream (which is Mermaid)
	maxScratchNeeded += NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH;
	
	// the actual sizeof(newLZ_chunk_arrays) changes in 32/64-bit
	//maxScratchNeeded += sizeof(newLZ_chunk_arrays); // 128 , more in Leviathan
	maxScratchNeeded += OODLELZ_MAX_SCRATCH_FOR_PHASE_HEADERS_AND_FUZZ;
	
	// never more than OODLELZ_COMPRESSOR_MAX_SCRATCH_MEM_SIZE
	//return RR_MIN(maxScratchNeeded,OODLELZ_COMPRESSOR_MAX_SCRATCH_MEM_SIZE);
	RR_ASSERT( maxScratchNeeded <= OODLELZ_COMPRESSOR_MAX_SCRATCH_MEM_SIZE );
	
	return maxScratchNeeded;
}

//===============================================================

OOFUNC1 S32 OOFUNC2 OodleLZ_ThreadPhased_BlockDecoderMemorySizeNeeded()
{
	// @@@@ -> this could actually be lower
	//	OODLELZ_COMPRESSOR_MAX_SCRATCH_MEM_SIZE is the amount needed to decode one chunk
	//		(around 3*128k because 128k extra live mem is needed during decode)
	//	the amount that needs to be retained for phase2 is only 256k
	// but don't bother for now, nobody cares about the memory use overhead of threadphasing

	// (assumes 2 chunks per block)
	//return 2 * OODLELZ_COMPRESSOR_MAX_SCRATCH_MEM_SIZE;
	
	// need one full decoder + one extra scratch
	//	(slightly larger than two scratches becaucse of the OodleLZDecoder object)
	S32 one_decoder = OodleLZDecoder_MemorySizeNeeded(OodleLZ_Compressor_Leviathan,0);
	return one_decoder + OODLELZ_COMPRESSOR_MAX_SCRATCH_MEM_SIZE;
}

//===========================================================================

#undef RADDEFAULT
#define RADDEFAULT(x)

struct _OodleLZDecoder // must be aligned 16
{
    S64         decPos;
    S64         decLen;
    S64			gotHeaderPos;
    S64			resetPos;
    U64			check;
    S32			callsWithoutProgress;
    rrbool      ownsmem;
    LZBlockHeader    header;

	S32			decoderSize; // size of this + following legacy states
	S32			memorySize; // decoderSize + scratch size

	void *		scratch;
	SINTa		scratch_size;
	
	// legacy_OodleLZDecoder follows
	U8		legacy[LEGACY_DECODER_SIZE];

	// scratch mem for newlz follows
	// or legacy objects (huffs/LZA state/etc)
};


const U64 c_checkVal = 0xABADF00DC0CAC01AULL;

/**

OodleLZDecoder_MemorySizeNeeded_NoPad_Compute is the raw decoder object

**/

static S32 OodleLZDecoder_MemorySizeNeeded_NoPad_Compute(OodleLZ_Compressor compressor)
{
	RR_ASSERT( compressor > OodleLZ_Compressor_Invalid && compressor < OodleLZ_Compressor_Count );
	    
	S32 ret = rrAlignUp((S32)sizeof(OodleLZDecoder),16) + sizeof(U64);

    if ( OodleLZ_Compressor_IsLegacy(compressor) )
    {
		if ( g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_MemorySizeNeeded_NoPad_Compute == NULL )
		{
			// should not get here
			RR_ASSERT(false);
			return -1;
		}

		ret += g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_MemorySizeNeeded_NoPad_Compute(compressor);
	}	
	
	return ret;
}

/**

OodleLZDecoder_MemorySizeNeeded_NoPad
is the decoder object PLUS scratch mem if needed

07-29-2019 :
OodleLZDecoder_MemorySizeNeeded:
 0: LZH       : 6168 bytes
 1: LZHLW     : 14344 bytes
 2: LZNIB     : 184 bytes
 3: None      : 184 bytes
 4: LZB16     : 184 bytes
 5: LZBLW     : 184 bytes
 6: LZA       : 19920 bytes
 7: LZNA      : 11668 bytes
 8: Kraken    : 446680 bytes
 9: Mermaid   : 315608 bytes
10: BitKnit   : 16224 bytes
11: Selkie    : 315608 bytes
12: Hydra     : 446680 bytes
13: Leviathan : 446680 bytes
-1: any       : 466416 bytes

**/
static S32 OodleLZDecoder_MemorySizeNeeded_NoPad(OodleLZ_Compressor compressor,S64 rawLen, S32 * pDecSize)
{
	bool dec_any = false;
	
	if ( compressor == OodleLZ_Compressor_Invalid || compressor >= OodleLZ_Compressor_Count )
	{
		// if you pass Invalid you get scratch mem too
		dec_any = true;
		
		if ( OodleLZLegacyVTable_IsInstalled() )
		{

			// @@ LEGACY GOES AWAY

			// make it the one that takes the most memory :
			// 
			#define COMPRESSOR_THAT_NEEDS_MOST_MEMORY	OodleLZ_Compressor_LZA
			// LZA is the largest "decoder" object (19920 bytes) (*pDecSize)
			//	but Leviathan is the largest total decoder mem use (including "scratch")
			// when people pass Invalid they get a weird slightly larger amount
			//	because they get an LZA "decoder" and enough "scratch" for Leviathan
			// that's sort of fucked, but whatever
			//  466416 for Invalid but 446680 would do
			compressor = COMPRESSOR_THAT_NEEDS_MOST_MEMORY;
			
			RR_ASSERT( OodleLZDecoder_MemorySizeNeeded_NoPad_Compute(compressor)
				>= g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_MemorySizeNeeded_NoPad_Compute(OodleLZ_Compressor_LZA) );
			
			RR_ASSERT( OodleLZDecoder_MemorySizeNeeded_NoPad_Compute(compressor)
				>= g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_MemorySizeNeeded_NoPad_Compute(OodleLZ_Compressor_LZNA) );
			
			RR_ASSERT( OodleLZDecoder_MemorySizeNeeded_NoPad_Compute(compressor)
				>= g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_MemorySizeNeeded_NoPad_Compute(OodleLZ_Compressor_LZHLW) );
			
			RR_ASSERT( OodleLZDecoder_MemorySizeNeeded_NoPad_Compute(compressor)
				>= g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_MemorySizeNeeded_NoPad_Compute(OodleLZ_Compressor_BitKnit) );
		}
		else
		{
			compressor = OodleLZ_Compressor_Hydra;			
		}
	}

	S32 decSize = OodleLZDecoder_MemorySizeNeeded_NoPad_Compute(compressor);
	
	if ( pDecSize )
		*pDecSize = decSize;
	
	if ( dec_any || OodleLZ_Compressor_NeedsScratchMem(compressor) )
	{
		SINTa scratchMemSize;
		scratchMemSize = OodleLZ_Compressor_ScratchMemSize(compressor,oo64toA(rawLen));
		decSize += (S32) scratchMemSize;
	}
	
	return decSize;
}

//===============================================================

// 16 larger than needed so we can align within the user-provided memory :
#define OODLELZ_DECODER_MEMORY_SIZE_PAD	16

OOFUNC1 S32 OOFUNC2 OodleLZDecoder_MemorySizeNeeded(OodleLZ_Compressor compressor,SINTa rawLen)
{
	OOFUNCSTART

	if ( OodleLZ_Compressor_IsLegacy(compressor) && ! OodleLZLegacyVTable_IsInstalled() )
	{
		ooLogError("OodleLZ_Decompress: asked to decode Legacy compressor; not supported!\n");
		return 0;		
	}

	return OodleLZDecoder_MemorySizeNeeded_NoPad(compressor,rawLen,NULL) + OODLELZ_DECODER_MEMORY_SIZE_PAD;
}


static void OodleLZDecoder_Check(const OodleLZDecoder * lzhD)
{
#ifdef RR_DO_ASSERTS
	RR_ASSERT( lzhD->check == c_checkVal );
#endif
}

static legacy_OodleLZDecoder * legacy_OodleLZDecoder_Get(OodleLZDecoder * lzd)
{
	return (legacy_OodleLZDecoder *)(lzd->legacy);
}

static OodleLZDecoder * OodleLZDecoder_Create_Sub(OodleLZ_Compressor compressor,S64 rawLen,void * memory,SINTa memorySize,
	S32 memSize,S32 decSize)
{
	// you must always do an OodleLZDecoder_Create before any decoding path
	// so catch one-time inits here :
	OodleCore_Enter();
	
    OodleLZDecoder * lzhD = (OodleLZDecoder *) memory;
    if ( lzhD )
    {
        memorySize -= 16; // discount padding
		if ( memorySize < memSize )
		{
			// gave me memory but it's not big enough
			return NULL;
		}
	
		// OodleLZDecoder_MemorySizeNeeded is padded so I can do this :
        lzhD = (OodleLZDecoder *) rrAlignUpPointer(memory,16);
        RR_ZERO(*lzhD);
    }
    else
    {
		memorySize = memSize;
        lzhD = (OodleLZDecoder *) OodleMallocAligned( memSize , 16);
        RR_ZERO(*lzhD);
        lzhD->ownsmem = true;
    }
    
    #define LZHD_DECLEN_INFINITE (RR_S64_MAX>>1)
    
    //lzhD->compressor = compressor;
    if ( rawLen < 0 )
		lzhD->decLen = LZHD_DECLEN_INFINITE; // infinite? // @@ S64 max ?
	else
		lzhD->decLen = rawLen;
    lzhD->decPos = 0;
    
    // CB note 02-15-2018 :
    //	I was setting resetPos to -1
    //   then failing if a reset was not encountered
    //  that is fine with current data
    //	 (LZ always ensures a reset)
    //  but not with some older data
    //	 there were versions that incorrectly didn't set the reset flag
    //		on the first block of no-reset compresses
    // to load those we must act like there's an implicit reset at 0
    
    //lzhD->resetPos = -1; // make sure a reset is seen
    lzhD->resetPos = 0; // initialize to 0 if not set
    lzhD->gotHeaderPos = -1;
    lzhD->callsWithoutProgress = 0;

	lzhD->check = c_checkVal;
    lzhD->decoderSize = decSize;
    lzhD->memorySize = (S32)memorySize;
    
	// scratch OR legacy decoder follows after the lzhD object
	// newlz's use scratch

    lzhD->scratch = lzhD + 1;
    lzhD->scratch_size = memorySize - decSize;
    
	// @@ NOT change 03-07-2021 : this was NOT done before - they were UNINIT
	// -> no it WAS being done by the RR_ZERO above
	//	-> bitknit relied on it
	//	RR_ZERO above is fine for this now
	//legacy_OodleLZDecoder_Reset(legacy_OodleLZDecoder_Get(lzhD));

    OodleLZDecoder_Check(lzhD);
    
    return lzhD;
}

// "memory" can be NULL and I'll alloc, or pass in something of size at least OodleLZDecoder_MemorySizeNeeded
OOFUNC1 OodleLZDecoder * OOFUNC2 OodleLZDecoder_Create(OodleLZ_Compressor compressor,S64 rawLen,void * memory,SINTa memorySize)
{
	OOFUNCSTART
		
	// memSize includes scratch, decSize does not
	S32 decSize;
	S32 memSize = OodleLZDecoder_MemorySizeNeeded_NoPad(compressor,rawLen,&decSize);

	return OodleLZDecoder_Create_Sub(compressor,rawLen,memory,memorySize,memSize,decSize);
}

OOFUNC1 rrbool OOFUNC2 OodleLZDecoder_Reset(OodleLZDecoder * lzhD, SINTa decPos,SINTa decLen RADDEFAULTX(0))
{
	if ( decLen == 0 )
		decLen = oo64toA( lzhD->decLen );

	if ( (decPos & (OODLELZ_BLOCK_LEN-1)) != 0 )
	{
		ooLogError("Can't reset off block boundary : " RR_SINTa_FMT "\n",decPos);
		return false;
	}
	
	if ( decPos < 0 || decPos > decLen )
	{
		ooLogError("Can't reset out of bounds : " RR_SINTa_FMT " in [ " RR_SINTa_FMT " , " RR_S64_FMT " ] \n",decPos,0,decLen);
		return false;
	}
	
    lzhD->decPos = decPos;
    lzhD->decLen = decLen;
    //lzhD->resetPos = -1; // make sure a reset is seen
    lzhD->resetPos = 0; // assume there was a Reset at zero
    lzhD->gotHeaderPos = -1;
    lzhD->callsWithoutProgress = 0;
    
	if ( g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_Reset )
		g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_Reset(legacy_OodleLZDecoder_Get(lzhD));
	    
    return true;
}

OOFUNC1 void OOFUNC2 OodleLZDecoder_Destroy(OodleLZDecoder * decoder)
{
	OOFUNCSTART
    OodleLZDecoder_Check(decoder);

    if ( decoder->ownsmem )
    {
        OodleFree(decoder);
    }
}

//AT_STARTUP( RR_ASSERT( IsPow2(OODLELZ_BLOCK_LEN) ) );


#if defined(OODLE_ENABLE_X64_WC_PROBE)

// reads 1 byte at [ptr] :
extern "C" U64 oodle_x64_wc_probe(const void *ptr);

static void UsageWarning_WriteCombiningCheck(const void *ptr,SINTa len)
{
	RR_ASSERT( ! g_Oodle_UsageWarningsDisabled );

	// need len >= 1 for oodle_x64_wc_probe
	// but also don't do it on tiny buffers
	//	where the time overhead is not negligible
	if ( len < 1024 )
	{
		return;
	}

	static const int NUM_RUNS = 3; // number of consecutive slow runs we require to complain
	static const U64 SLOW_RUN_THRESHOLD = 3200; // in cycles; this is quite conservative. (a regular run with a main memory miss is ~300-500 cycles)

	// Try multiple runs of just trying to read 1 byte from the buffer;
	// if any one of them is faster than the "slow run" threshold,
	// assume the buffer is OK.
	for (int i = 0; i < NUM_RUNS; i++)
	{
		if ( oodle_x64_wc_probe( ptr) < SLOW_RUN_THRESHOLD )
			return;
	}

	// All runs were slow -> looks like somebody gave us a pointer to
	// write-combined memory!
	ooLogUsageWarning2("Destination buffer appears to be in uncached\n" \
		"or write-combined memory! This is extremely slow. See \"How do I\n" \
		"decompress to graphics memory quickly?\" in the Oodle FAQ.\n",0);
}

#else

static void UsageWarning_WriteCombiningCheck(const void *ptr,SINTa len)
{
	// nop
}

#endif

// returns false if corruption detected
OOFUNC1 rrbool OOFUNC2 OodleLZDecoder_DecodeSome(OodleLZDecoder * decoder,
										OodleLZ_DecodeSome_Out * pOut,

                                        // the decode sliding window : we output here & read from this for matches
                                        void * v_decBuf,
                                        SINTa decBufPos,
                                        SINTa decBufferSize,  // decBufferSize should be the result of OodleLZDecoder_MakeDecodeBufferSize()
                                        SINTa decBufAvail, // usually Size - Pos, but maybe less if you have pending IO flushes

                                        // compressed data :
                                        const void * v_compBuf,
                                        SINTa compBufAvail,
		
										OodleLZ_FuzzSafe fuzzSafe,
										OodleLZ_CheckCRC checkCRC,
										OodleLZ_Verbosity verbose,
										OodleLZ_Decode_ThreadPhase threadPhase

                                        )
{
	OOFUNCSTART
	const U8 * checkBuf = NULL;
	
	U8 * decBuf = U8_void(v_decBuf);
	const U8 * compBuf = U8_void(v_compBuf);
 
	SIMPLEPROFILE_SCOPE_N(DecodeSome,OODLELZ_QUANTUM_LEN);
	
    //LZHLW_InitStatics();
    //LZH_InitStatics();

	// pOut should not be NULL    
    RR_ASSERT( pOut );
    if ( ! pOut ) return false;
    
    // make sure we have zeros for invalid returns :
    RR_ZERO(*pOut);
    
	if_unlikely ( ++(decoder->callsWithoutProgress) >= 1000 )
	{
		// when headers are corrupted, we can spin calling DecodeSome
		//	infinitely and never making progress
		ooLogError("1000 calls without progress!\n");
        return false;   
	}


	// if decLen == LZHD_DECLEN_INFINITE , this will always be true :
	rrbool isSlidingWindow = ( decBufferSize < decoder->decLen );

	if ( isSlidingWindow )
	{
		rrPrintf_v2("slide 1\n");
	}
            
	RR_ASSERT( rrIsPow2(OODLELZ_BLOCK_LEN) );
	RR_ASSERT( rrIsPow2(OODLELZ_QUANTUM_LEN) );
   
    if_unlikely ( checkBuf != NULL )
    {
        checkBuf += decoder->decPos;
	}
	
    S64 chunkPos;
    
    // use at most what's available :
    // never more than a block :
    // NOTE : SINTa to S32 conversion here; safe because of BLOCK_LEN limit
    S32 rawBytesToGo = (S32) RR_MIN3( OODLELZ_BLOCK_LEN, (decBufferSize - decBufPos), decBufAvail );
        
    if ( decoder->decLen == LZHD_DECLEN_INFINITE )
    {
		chunkPos = decoder->decPos; // no blocks!
    }
    else
    {
		chunkPos = decoder->decPos & (OODLELZ_BLOCK_LEN-1);

		// I should be starting a quantum :
	    RR_ASSERT( ( chunkPos & (OODLELZ_QUANTUM_LEN - 1) ) == 0 );
    
		S64 rawLenLeft = decoder->decLen - decoder->decPos;
		RR_ASSERT( rawLenLeft > 0 ); // why are you calling me ?
    
		// only decode up to the end of the buffer or the end of the current huffman chunk
		rawBytesToGo = (S32) RR_MIN( rawBytesToGo , (OODLELZ_BLOCK_LEN - chunkPos) );
	    
		// don't decode past the end of the file :
		rawBytesToGo = (S32) RR_MIN(rawBytesToGo, rawLenLeft );
	}

    if_unlikely ( rawBytesToGo == 0 )
    {
	    // nothing to do :
        return true;
    }
    
    if_unlikely ( compBufAvail == 0 )
	{
        // you gave me no compressed bytes!
        return true;
	}

    const U8 * compPtr = compBuf;
    const U8 * compPtrEnd = compBuf + compBufAvail;
    
	FUZZ_MUNGE( *((U8 *)compPtr) );
	
	LZBlockHeader & header = decoder->header;
	
    // start of a new chunk, read huffmans :
    // we have to check gotHeaderPos because it's possible for chunkPos to sit at 0 for several calls
    //	 if user is not giving us enough compBufBytes to actually decode a byte
    if ( chunkPos == 0 && decoder->gotHeaderPos != decoder->decPos )
    {
		//	there are cases where I can read the headers and then decode 0 bytes
		//		which would cause me to read them again
		//  gotHeaderPos should stop that

		// can never do anything with < 2 bytes
		if_unlikely ( compBufAvail < OODLELZ_BLOCK_HEADER_BYTES_MAX )
		{
            //rrprintcorruption("no bytes for block header..\n");
            return true;
		}
    
		decoder->gotHeaderPos = decoder->decPos;
    
		FUZZ_MUNGE( *((U8 *)compPtr) );

		// this is checked above :
		RR_ASSERT( compPtr + OODLELZ_BLOCK_HEADER_BYTES_MAX <= compPtrEnd );
        compPtr = LZBlockHeader_Get(&header,compPtr);
        if ( compPtr == NULL )
        {
            rrprintcorruption("invalid header..\n");
            return false;
        }
                
		OodleLZ_Compressor compressor = OodleLZ_DecodeType_to_Compressor(header.decodeType);
		if ( fuzzSafe == OodleLZ_FuzzSafe_Yes
			&& ! OodleLZ_Compressor_CanDecodeFuzzSafe(compressor) )
		{
			ooLogError("Requested Fuzz Safety, saw a compressor that can't decode Safe\n");
			return false;
		}
		
		// if compressor of this block requires more memory than I have :
		S32 decMemSizeNeeded = OodleLZDecoder_MemorySizeNeeded_NoPad(compressor,rawBytesToGo,NULL);
		//RR_ASSERT( decoder->memorySize >= decMemSizeNeeded );
		if ( decoder->memorySize < decMemSizeNeeded )
		{
			ooLogError("Heterogenous compressor file with insufficient decoder memory size!\n");
			return false;
		}

		bool is_legacy = OodleLZ_Compressor_IsLegacy(compressor);

		if ( is_legacy )
		{
			if ( ! g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_Block_Start )
			{
				ooLogError("Legacy LZ VTable not installed");
				return false;
			}
			g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_Block_Start( legacy_OodleLZDecoder_Get(decoder) );
		}

        if ( header.chunkIsReset )
        {
			RR_ASSERT( decoder->decPos >= decoder->resetPos );
			decoder->resetPos = decoder->decPos;
	
			rrPrintf_v2("RESET : %d\n",(int)decoder->resetPos);
	
			if ( is_legacy )
			{
				// @@ 12-15-2015 : added this to make it automatic :
				//	should be a NOP - you should always have reset flags for this case		
				
				if ( ! g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_Block_Reset )
				{
					ooLogError("Legacy LZ VTable not installed");
					return false;
				}
				g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_Block_Reset( legacy_OodleLZDecoder_Get(decoder) );
			}
		}
		
        if ( verbose >= OodleLZ_Verbosity_Lots )
        {
			rrprintf("CHUNK header @ " RR_S64_FMT "  , compressor %d=%s %s %s\n",
				decoder->gotHeaderPos,
				(int)compressor,
				OodleLZ_Compressor_GetName(compressor),
				header.chunkIsReset ? "reset" : "",
				header.chunkIsMemcpy ? "memcpy" : "");
        }
	}
	
    S32 decodeType = header.decodeType;
           
	OodleLZ_Compressor compressor = OodleLZ_DecodeType_to_Compressor(header.decodeType);
	bool is_legacy = OodleLZ_Compressor_IsLegacy(compressor);

    if_unlikely ( decoder->gotHeaderPos == 0 && ! header.chunkIsReset )
    {
    	rrPrintf_v2("slide 2\n");

		// @@ WTF was this for ?

		// note decBufferSize >= decLen is possible here
		//	when decLen takes you to the end of the stream
		// in the middle of the stream this should only occur with
		//	decBufferSize being a multiple of OODLELZ_BLOCK_LEN
		
		// note we intentionally are NOT doing the buffer size checks in this case
		
		// @@ CB 02-27-2018 :
		//	was previously always setting isSlidingWindow = true here
		//	it would then fail immediately if not pow2 below
		//  now only set it if pow2 is true
		// this covers the old no reset data
		//	(very old LZH streams have no reset in first chunk)
		if ( rrIsPow2(S32_checkA(decBufferSize)) )
		{
			isSlidingWindow = true;
		}			
    }
        
    if_unlikely ( isSlidingWindow )
    {
		//rrprintf("slide 1\n");
    
		// verify buffer size is okay for sliding window :
    
		//RR_ASSERT_ALWAYS( rrIsPow2(S32_checkA(decBufferSize)) );
		if ( ! rrIsPow2(S32_checkA(decBufferSize)) )
		{
			ooLogError("sliding window must be pow2!\n");
			return false;
		}
		
		if ( compressor == OodleLZ_Compressor_Invalid )
	    {
			ooLogError("invalid decodeType!\n");
			return false;
		}
		
		if ( OodleLZ_Compressor_IsNewLZFamily(compressor) )
		{
			ooLogError("only legacy compressors do sliding window!\n");
			return false;
		}
		else if ( compressor == OodleLZ_Compressor_LZB16 )
		{
			if ( decBufferSize < LZB_SLIDING_WINDOW_SIZE )
			{
				ooLogError("you didn't give me enough window!\n");
				return false;
			}
		}
		else
		{
			if ( ! g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_CheckSlidingWindow )
			{
				ooLogError("Legacy LZ VTable not installed");
				return false;
			}
			if ( ! g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_CheckSlidingWindow(compressor,decBufferSize) )
			{
				// logs its own error
				return false;
			}
		}
    }
        
    // decPtr goes from decPtrStart to decPtrEnd
    //  that's the range of the decode buffer we're allowed to write to
    U8 * decPtrStart = decBuf + decBufPos;
    U8 * decPtr = decPtrStart;
	
    if ( header.chunkIsMemcpy )
    {
		// just copy bytes, but support all the right buffer ending stuff
		
		// we'll copy out one quantum at a time :
		// we could do the full block, but this makes it consistent with the normal decoder
		//  so we're always decoding a quantum per call
		RR_ASSERT( rawBytesToGo <= OODLELZ_BLOCK_LEN );    
    
		pOut->curQuantumRawLen  = rawBytesToGo;
		pOut->curQuantumCompLen = rawBytesToGo;

		SINTa compAvail = rrPtrDiff(compBuf + compBufAvail - compPtr);

		// wait for a full quantum
		if_unlikely ( (SINTa)rawBytesToGo > compAvail )
		{
			// not enough compressed bytes, wait for more
			//  this can cause an infinite loop if we're corrupted
			//	so we check # of calls without progress
			
			pOut->decodedCount = 0;
			pOut->compBufUsed = rrPtrDiff32(compPtr - compBuf);
			
			return true;
		}
		int canCopy = rawBytesToGo;
		
        if_unlikely ( verbose >= OodleLZ_Verbosity_Lots )
        {
			rrprintf(" decoder memcpy %d\n",canCopy);
        }
        
        // whole chunk is a memcpy
        // just call memcpy :
		if ( threadPhase & OodleLZ_Decode_ThreadPhase1 )
		{
			lzhd_memmov(decPtr,compPtr,canCopy);
        }
        decPtr  += canCopy;
        compPtr += canCopy;
        
        S32 bytesDecoded = rrPtrDiff32(decPtr - decPtrStart);
        decoder->decPos += bytesDecoded;
        
        /*
        // this is not possible
        if ( decoder->decPos > decoder->decLen )
        {
            rrprintcorruption("memcpy overrun!\n");
            return false;
        }
        /**/
        
        // made progress :
        decoder->callsWithoutProgress = 0;
        
        pOut->decodedCount = bytesDecoded;
        pOut->compBufUsed = rrPtrDiff32(compPtr - compBuf);
        
        // memcpy chunk
		if ( g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_Block_Reset )
			g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_Block_Reset( legacy_OodleLZDecoder_Get(decoder) );
        
        return true;
    }
    else
    {   	    
        RR_ASSERT( decodeType == RAD_LZ_DECODE_LZHLW ||
			decodeType == RAD_LZ_DECODE_LZA ||
			decodeType == RAD_LZ_DECODE_LZNA ||
			decodeType == RAD_LZ_DECODE_BITKNIT ||
			decodeType == RAD_LZ_DECODE_LZH ||
			decodeType == RAD_LZ_DECODE_LZB16 ||
			decodeType == RAD_LZ_DECODE_LZBLW ||
			decodeType == RAD_LZ_DECODE_LZNIB ||
			decodeType == RAD_LZ_DECODE_KRAKEN ||
			decodeType == RAD_LZ_DECODE_LEVIATHAN ||
			decodeType == RAD_LZ_DECODE_MERMAID );
        
        //if ( decoder->decPos == 884736 )
		//	RR_BREAK();
        
		if ( ! LZ_DecodeType_IsLargeQuantum(decodeType) )
		{
			rawBytesToGo = (S32) RR_MIN(rawBytesToGo, OODLELZ_QUANTUM_LEN ); // 16k
		}
    
	    U8 * decPtrEnd = decPtrStart + rawBytesToGo;
	    
		const U8 * qStartCompPtr = compPtr;
        //compPtr = qStartCompPtr;
    
		FUZZ_MUNGE( ((U8 *)compPtr)[0] );
		FUZZ_MUNGE( ((U8 *)compPtr)[1] );

		S32 qh_rawLen = rawBytesToGo;
		// (S32) RR_MIN( rawLenLeft, OODLELZ_QUANTUM_LEN );  		

        LZQuantumHeader qh;
		int lzqhLen;
        
		if ( LZ_DecodeType_IsLargeQuantum(decodeType) )
		{
			RR_ASSERT( rawBytesToGo <= OODLELZ_BLOCK_LEN );
	    
			lzqhLen = LZLargeQuantumHeader_Get(compPtr,compPtrEnd,&qh,header.chunkHasQuantumCRCs,qh_rawLen);
		}
		else
		{
			RR_ASSERT( rawBytesToGo <= OODLELZ_QUANTUM_LEN );
	    
			lzqhLen = LZQuantumHeader_Get(compPtr,compPtrEnd,&qh,header.chunkHasQuantumCRCs,qh_rawLen);
		}
		
		if ( lzqhLen <= 0 )
		{
			// either corrupt, or just not enough comp bytes
			// @@ using OODLELZ_QUANTUM_HEADER_MAX_SIZE to tell the difference is not great
			//	 but I think it works out in the end
			//	 see "SEMI-BUG : sloppy use of OODLELZ_QUANTUM_HEADER_MAX_SIZE"
			
			if ( rrPtrDiff(compPtrEnd - compPtr) < OODLELZ_QUANTUM_HEADER_MAX_SIZE )
			{
				// not enough compressed bytes, wait for more
				
				pOut->decodedCount = 0;
				pOut->compBufUsed = rrPtrDiff32(compPtr - compBuf);
				return true;
			}
			else
			{
				// corrupt
				return false;
			}
		}
		
		compPtr += lzqhLen;

        int qhLen = rrPtrDiff32(compPtr - qStartCompPtr);
		  
		//int qh_rawLen = qh.rawLen;
				
		pOut->curQuantumRawLen = qh_rawLen;
		pOut->curQuantumCompLen = qh.compLen + qhLen;
    
        if_unlikely ( verbose >= OodleLZ_Verbosity_Lots )
		{
			rrprintf("QH : %d , %d , %08X\n",qh_rawLen,qh.compLen,qh.crc);
		}
		
        // this is how rawLen is set :
        RR_ASSERT( qh_rawLen <= rawBytesToGo );
		/*
        if_unlikely ( qh_rawLen > OODLELZ_QUANTUM_LEN )
        {
            rrprintcorruption("qh_rawLen > OODLELZ_QUANTUM_LEN!\n");
            return false;			
        }
        */
        
        // use unsigned comp to get negatives too :
        if_unlikely ( (U32)qh.compLen > (U32)qh_rawLen )
        {
            rrprintcorruption("qh.compLen > qh_rawLen!\n");
            return false;			
        }
        RR_ASSERT( qh.compLen <= qh_rawLen );
        
		if_unlikely ( qh.compLen > rrPtrDiff( compPtrEnd - compPtr ) )
		{
			// not enough compressed bytes to decode
			
			#if 0
			// force it to take truncated data :
			//  (for fuzz testing of the underlying compressor)
			
			compPtrQuantumEnd = compPtrEnd;	
			qh.compLen = (S32)(compPtrQuantumEnd - compPtr);
			
			#else
			
			pOut->decodedCount = 0;
			pOut->compBufUsed = rrPtrDiff32(qStartCompPtr - compBuf);
	        
	        // return qh.compLen so caller knows how many bytes I need
	        //	this is in pOut->curQuantumCompLen now
	        
			return true;
			#endif
		}
        
		const U8 * compPtrQuantumEnd = compPtr + qh.compLen;

		if_unlikely ( checkCRC && qh.compLen > 0 && header.chunkHasQuantumCRCs )
		{
			SIMPLEPROFILE_SCOPE_N(crc,qh.compLen);
			
			// check compressed CRC
			U32 myCRC = LZQuantumHeader_ComputeCRC(compPtr,qh.compLen);
			if ( myCRC != qh.crc )
			{
                rrprintcorruption("crc check fail.\n");
                // corrupted!
                return false;
			}
		}
                
        S32 gotHuffCompLen = 0;
        
		if_unlikely ( qh.huffFlag )
		{
			//SIMPLEPROFILE_SCOPE(DecodeBuildHuffman);
			
			if ( is_legacy )
			{
				if ( ! g_OodleLZLegacyVTable.fp_legacy_GotHuffFlag )
				{
					ooLogError("Legacy LZ Vtable not installed");
					return false;
				}
				if ( ! g_OodleLZLegacyVTable.fp_legacy_GotHuffFlag( decodeType,legacy_OodleLZDecoder_Get(decoder),compPtr,compPtrEnd,&gotHuffCompLen) )
				{
					return false;
				}
			}
			else
			{
				// @@ is this an error?
			}
						
			compPtr += gotHuffCompLen;
		}
		
		RR_ASSERT( qh.compLen >= gotHuffCompLen );
		
        if_unlikely ( qh.compLen == qh_rawLen )
        {
			// quantum is a memcpy type
			
			SIMPLEPROFILE_SCOPE_N(DecodeOneQ_memcpy,qh_rawLen);
			
			int decLen =  rrPtrDiff32( decPtrEnd - decPtr );
			if_unlikely ( decLen != qh_rawLen )
			{
				rrprintcorruption("memcpy invalid size!\n");
				return false;
			}
			
			if ( threadPhase & OodleLZ_Decode_ThreadPhase1 )
			{
				lzhd_memmov(decPtr,compPtr,decLen);
			}
			decPtr += decLen;
			compPtr += decLen;
			
			/*
			int bytesDecoded = rrPtrDiff32(decPtr - decPtrStart);
			RR_ASSERT( bytesDecoded >= 0 && bytesDecoded <= rawBytesToGo );
			RR_ASSERT( bytesDecoded == decLen );
			*/
			
			decoder->decPos += decLen;
	        
			if_unlikely ( decoder->decPos > decoder->decLen )
			{
				rrprintcorruption("overrun!\n");
				return false;
			}
	        
		    // made progress :
	        decoder->callsWithoutProgress = 0;
        
			pOut->decodedCount = qh_rawLen;
	        pOut->compBufUsed = rrPtrDiff32( compPtr - compBuf );
	        
			return true;
        }
        //*
        else
        if_unlikely ( qh.compLen == gotHuffCompLen )
        {
			// this is handled automatically by the normal LZ decode if you just let it drop through
			SIMPLEPROFILE_SCOPE_N(DecodeOneQ_memset,qh_rawLen);
			
			int decLen =  rrPtrDiff32( decPtrEnd - decPtr );
			RR_ASSERT( decLen == qh_rawLen ); // this is guaranteed by the way decLen is made
			
			if_unlikely ( (decoder->decPos + decLen) > decoder->decLen )
			{
				rrprintcorruption("overrun!\n");
				return false;
			}
			
			if_unlikely ( qh.wholeMatchFlag )
			{
			
				if ( threadPhase & OodleLZ_Decode_ThreadPhase2 )
				{
					SINTa matchOff = qh.wholeMatchOffset;
					SINTa matchLen = decLen;
					
					// encoder won't make these unless quantum len >= 3 :
					RR_ASSERT_IF_NOT_CORRUPT( decLen >= 3 );
					
					REQUIRE_FUZZ_RETURN( matchOff > 1 , false ); // offset 1 would use memset path
					
					U8 * toPtr = decPtr;
					
					#if 0				
					lz_copygeneric(toPtr,matchOff,matchLen);
					#else
					const U8 * fmPtr = toPtr - matchOff;
					
					SINTa pos_since_reset = oo64toA( decoder->decPos - decoder->resetPos );
					if_unlikely( matchOff > pos_since_reset )
					{
						rrprintcorruption("wholematch offset too large!\n");
						return false;
					}
					
					if_unlikely( matchOff < matchLen )
					{
						// overlap
						// surprisingly this does occur with non-trivial data
						//  (eg. not memsets)
						// but with 10-byte patterns
						// weird!

						lz_copygeneric(toPtr,matchOff,matchLen);
					}
					else
					{
						memcpy(toPtr,fmPtr,matchLen);
					}
					#endif
				}
			}
			else
			{			
				int memsetVal;
				
				if ( qh.compLen == 0 )
				{
					// special memset mode sent by the encoder :

					memsetVal = qh.crc;
				}
				else
				{
					// normal quantum that happens to be all only has one symbol
					//	just do a memset

					if ( is_legacy )
					{
						if ( ! g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_GetDegenerateMemsetVal )
						{
							ooLogError("Legacy LZ Vtable not installed");
							return false;
						}
						memsetVal = g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_GetDegenerateMemsetVal( legacy_OodleLZDecoder_Get(decoder) );
						if ( memsetVal < 0 )
							return false;
					}
					else
					{
						rrprintcorruption("huffman->gotNumSymbols == 1 path is legacy only!\n");
						return false;
					}
				}
							
				if ( threadPhase & OodleLZ_Decode_ThreadPhase1 )
				{
					rrMemSetLarge(decPtr,memsetVal,decLen);
				}
			}
			
			decPtr += decLen;
			decoder->decPos += decLen;
	        	        
			// made progress :
			decoder->callsWithoutProgress = 0;
        
			pOut->decodedCount = qh_rawLen;
	        pOut->compBufUsed = rrPtrDiff32( compPtr - compBuf );
	        
			return true;
        }
        /**/
        else // normal LZ
        {
			SIMPLEPROFILE_SCOPE_N(DecodeOneQ_lz,qh_rawLen);
			
			// @@ no - not true - can happen if the whole quantum is a match
			/*
			// should have gone through memset case :
			RR_ASSERT( decoder->huff_combined != NULL && decoder->huff_combined->gotNumSymbols > 1 );
			if_unlikely ( ! decoder->huff_combined || decoder->huff_combined->gotNumSymbols < 2 )
			{
				rrprintcorruption("huffman->gotNumSymbols < 2!\n");
				return false;
			}
			*/
			
	        // for debugging :
	        //lzhlw_decPosBase = (int) decoder->decPos;
	        
	        // we must have gotten a reset chunk to set our base pos :
			RR_ASSERT( decoder->resetPos >= 0 );
			//REQUIRE_FUZZ_RETURN( decoder->resetPos >= 0 , false );
			RR_ASSERT( decoder->resetPos <= decoder->decPos );
		
			SINTa pos_since_reset = oo64toA( decoder->decPos - decoder->resetPos );
				
	        S32 compBufUsed = rrPtrDiff32( compPtr - compBuf );
	        
			S32 gotCodeCompLen;
			
			if ( decodeType == RAD_LZ_DECODE_MERMAID ) // does Selkie too
			{	
				gotCodeCompLen = Mermaid_DecodeOneQuantum(decPtr,decPtrEnd,compPtr,qh.compLen,compPtrEnd,pos_since_reset,decoder->scratch,decoder->scratch_size,threadPhase);
			}
			else if ( decodeType == RAD_LZ_DECODE_KRAKEN )
			{
				gotCodeCompLen = Kraken_DecodeOneQuantum(decPtr,decPtrEnd,compPtr,qh.compLen,compPtrEnd,pos_since_reset,decoder->scratch,decoder->scratch_size,threadPhase);
			}
			else if ( decodeType == RAD_LZ_DECODE_LEVIATHAN )
			{
				gotCodeCompLen = Leviathan_DecodeOneQuantum(decPtr,decPtrEnd,compPtr,qh.compLen,compPtrEnd,pos_since_reset,decoder->scratch,decoder->scratch_size,threadPhase);
			}
			else if ( decodeType == RAD_LZ_DECODE_LZB16 )
			{
				const U8 * dictionaryBase = decPtr - pos_since_reset;
				gotCodeCompLen = LZB_DecodeOneQuantum(decPtr,decPtrEnd,compPtr,compPtrEnd,qh,decBuf,(SINTr)decBufferSize,isSlidingWindow,dictionaryBase);
			}
			else if ( is_legacy )
			{
				if ( ! g_OodleLZLegacyVTable.fp_legacy_OodleLZ_DecodeOneQ )
				{
					ooLogError("Legacy LZ Vtable not installed");
					return false;
				}
				gotCodeCompLen = g_OodleLZLegacyVTable.fp_legacy_OodleLZ_DecodeOneQ(decodeType,legacy_OodleLZDecoder_Get(decoder),
					decPtr,decPtrEnd,
					compPtr,compPtrQuantumEnd,compPtrEnd,
					header,qh,
					decBuf,decBufferSize,
					isSlidingWindow,
					pos_since_reset,
					checkBuf);
			}
			else
			{
				rrprintcorruption("DecodeOneQ invalid decodeType!\n");
				return false;
			}
			     
			if_unlikely ( gotCodeCompLen < 0 )
			{
				rrprintcorruption("DecodeOneQuantum fail!\n");
				return false;
			}
	        
			S32 bytesDecoded = rrPtrDiff32( decPtrEnd - decPtr );
			RR_ASSERT( bytesDecoded >= 0 && bytesDecoded <= rawBytesToGo );
			//if ( bytesDecoded < 0 )
			//  bytesDecoded += decBufferSize;
			decoder->decPos += bytesDecoded;
	        
			RR_ASSERT( bytesDecoded == qh_rawLen );
	        
			if_unlikely ( decoder->decPos > decoder->decLen )
			{
				rrprintcorruption("overrun!\n");
				return false;
			}
	        
			S32 gotCompLen = gotCodeCompLen + gotHuffCompLen;
			if_unlikely ( gotCompLen != qh.compLen )
			{
				rrprintcorruption("not complen!\n");
				return false;
			}
	        
			compBufUsed += gotCodeCompLen;
	        
			// made progress :
			decoder->callsWithoutProgress = 0;
        
			pOut->decodedCount = bytesDecoded;
			pOut->compBufUsed = compBufUsed;
	        
			return true;
		}
    }
}

OOFUNC1 SINTa OOFUNC2 OodleLZ_Decompress(const void * v_compBuf,SINTa compBufferSize,void * v_decodeTo,SINTa decodePartLen,
											OodleLZ_FuzzSafe fuzzSafe RADDEFAULT(OodleLZ_FuzzSafe_No),
											OodleLZ_CheckCRC checkCRC RADDEFAULT(OodleLZ_CheckCRC_No),
											OodleLZ_Verbosity verbose RADDEFAULT(OodleLZ_Verbosity_Minimal),
											void * v_decBufBase RADDEFAULT(NULL),
											SINTa decBufSize RADDEFAULT(0),
											OodleDecompressCallback * fpCallback RADDEFAULT(NULL),
											void * callbackUserData RADDEFAULT(NULL),
											void * decoderMemory RADDEFAULT(NULL),
											SINTa decoderMemorySize RADDEFAULT(0),
											OodleLZ_Decode_ThreadPhase threadPhase)
{
	OOFUNCSTART
	//const U8 * checkBuf = NULL;
	THREADPROFILEFUNC();
	
	U8 * decodeTo = U8_void(v_decodeTo);
	U8 * decBufBase = U8_void(v_decBufBase);
	const U8 * compBuf = U8_void(v_compBuf);
 
	PARAMETER_CHECK( decodeTo != NULL , 0);
	PARAMETER_CHECK( compBuf != NULL ,0);
	
	if ( decodePartLen == 0 )
	{
		ooLogError("OodleLZ_Decompress: asked for len 0\n");
		return 0; // 0 == OODLELZ_FAILED
	}
	
	if ( ! g_Oodle_UsageWarningsDisabled )
	{                  
		// Check whether we were handed a pointer to write-combined memory
		UsageWarning_WriteCombiningCheck( decodeTo, decodePartLen );
	}
	
	SINTa decodeStartOffset = 0;
	if ( decBufBase == NULL )
	{
		decBufBase = decodeTo;
	}
	else
    {
	    decodeStartOffset = rrPtrDiff( decodeTo - decBufBase );
    
	    // @@ if decodeStartOffset is not a multiple of OODLELZ_BLOCK_LEN it's an almost gauranteed failure
	    //	this is not fundamental, just a consequence of the silly way I do decPos maintenance
	    //	I start it at decodeStartOffset, and I need to get a block header to start the stream
	    //	hence the assuming it's on block boundary
		if ( (decodeStartOffset & (OODLELZ_BLOCK_LEN-1)) != 0 )
		{
			ooLogError("OodleLZ_Decompress: dictionary backup distance must be a multiple of OODLELZ_BLOCK_LEN\n");
			return OODLELZ_FAILED;
		}
    }
    
    SINTa rawDecoded = decodeStartOffset;
    SINTa rawLen = decodeStartOffset + decodePartLen; // <- decode len PLUS backup
    SINTa compUsed = 0;

	if ( decBufSize <= 0 ) decBufSize = rawLen; // <- decode len PLUS backup
	RR_ASSERT( decBufSize >= rawLen );
	
	// get compressor to allocate the decoder :
	//	this assumes the same compressor for all blocks,
	//	or at least the same family (newlz hybrid is okay)
	// -> if you want mixed compressor data, provide your own Decoder object that's of max size
	//OodleLZ_Compressor compressor = OodleLZ_GetChunkCompressor(compBuf,compBufferSize,NULL);
	OodleLZ_Compressor compressor = OodleLZ_GetAllChunksCompressor(compBuf,compBufferSize,decodePartLen);

	if ( compressor == OodleLZ_Compressor_Invalid ) return OODLELZ_FAILED;

	if ( fuzzSafe == OodleLZ_FuzzSafe_Yes
		&& ! OodleLZ_Compressor_CanDecodeFuzzSafe(compressor) )
	{
		ooLogError("OodleLZ_Decompress: Requested Fuzz Safety, saw a compressor that can't decode Safe\n");
		return OODLELZ_FAILED;
	}

	// check on threadPhase sanity :
	if ( threadPhase != OodleLZ_Decode_Unthreaded )
	{
		if ( threadPhase == 0 )
		{
			threadPhase = OodleLZ_Decode_Unthreaded;
			// fix it?
			rrPrintf_v2("OodleLZ_Decompress: threadPhase 0! fixing it to OodleLZ_Decode_Unthreaded\n");
		}
		else
		{
			RR_ASSERT_ALWAYS( threadPhase == OodleLZ_Decode_ThreadPhase1 ||  threadPhase == OodleLZ_Decode_ThreadPhase2 );
				
			if ( ! OodleLZ_Compressor_CanDecodeThreadPhased(compressor) )
			{
				ooLogError("OodleLZ_Decompress: asked for threaded decode but !OodleLZ_Compressor_CanDecodeThreadPhased\n");
				return OODLELZ_FAILED;				
			}
			
			if ( decoderMemory == NULL || decoderMemorySize < OodleLZ_ThreadPhased_BlockDecoderMemorySizeNeeded() )
			{
				ooLogError("OodleLZ_Decompress: asked for threaded decode but decoderMemory < OodleLZ_ThreadPhased_BlockDecoderMemorySizeNeeded\n");
				return OODLELZ_FAILED;		
			}
		}
	}
	
	if ( OodleLZ_Compressor_IsLegacy(compressor) && ! OodleLZLegacyVTable_IsInstalled() )
	{
		ooLogError("OodleLZ_Decompress: asked to decode Legacy compressor; not supported!\n");
		return OODLELZ_FAILED;		
	}

	// memSize includes scratch, decSize does not
	S32 dec_decSize;
	S32 dec_memSize = OodleLZDecoder_MemorySizeNeeded_NoPad(compressor,rawLen,&dec_decSize);
	
	if ( decoderMemory == NULL )
	{
		if ( dec_memSize < g_OodleLZ_Decoder_Max_Stack_Size )
		{
			decoderMemorySize = dec_memSize + OODLELZ_DECODER_MEMORY_SIZE_PAD;
			decoderMemory = RAD_ALLOCA( decoderMemorySize );
		}
	}
	else
	{	
		// check provided memory size :
		if ( decoderMemorySize < (dec_memSize+OODLELZ_DECODER_MEMORY_SIZE_PAD) )
		{
			ooLogError("OodleLZ_Decompress : decoder memory given, but too small!\n");
			return OODLELZ_FAILED;
		}
	}
			
    OodleLZDecoder * lzhd = OodleLZDecoder_Create_Sub(compressor,rawLen,decoderMemory,decoderMemorySize,dec_memSize,dec_decSize);
    
    lzhd->decPos = decodeStartOffset;
    
    if ( decodeStartOffset > 0 )
    {
		RR_ASSERT( lzhd->resetPos == 0 );
		// if we're resuming a decode with an overlap
		//  we must have had a reset at the start of decBufBase
		lzhd->resetPos = 0;
    }
    
    // we check rawDecoded == rawLen to know that all calls to DecodeSome succeeded
    RR_ASSERT( rawDecoded != rawLen );
    
    while( rawDecoded < rawLen )
    {
        if ( compUsed >= compBufferSize )
        {
			rrprintcorruption("OodleLZ_Decompress not enough comp buf\n");
	        break;                
        }
        
		if ( verbose >= OodleLZ_Verbosity_Lots )
		{
			rrPrintf("OodleLZ_Decompress about to DecodeSome, starting at comp %d -> raw %d\n",(int)compUsed,(int)rawDecoded);
		}
		
		OodleLZ_DecodeSome_Out out;
		
        if ( ! OodleLZDecoder_DecodeSome(lzhd,&out,
												decBufBase,rawDecoded,decBufSize,decBufSize-rawDecoded,
												compBuf+compUsed,compBufferSize-compUsed,
												fuzzSafe,checkCRC,verbose,threadPhase
												) )
        {
	        break;                
        }
        
        S32 decoded = out.decodedCount;
        S32 comped = out.compBufUsed;
        
        if ( decoded == 0 )
        {
			rrprintcorruption("OodleLZ_Decompress : DecodeSome wanted more comp data\n");
	        break;                
        }
        
        rawDecoded += decoded;
        compUsed += comped;
        // no :
        //if ( checkBuf )
		//	checkBuf += decoded;
		
		if ( fpCallback )
		{
			OodleDecompressCallbackRet e = (*fpCallback)(callbackUserData,decodeTo,(decodePartLen),compBuf,(compBufferSize),rawDecoded-decodeStartOffset,compUsed);
			if ( e == OodleDecompressCallbackRet_Cancel )
				break;
		}
    }
    
    OodleLZDecoder_Destroy(lzhd);
    
    if ( rawDecoded != rawLen )
    {
		// may have successfully got some, but not the amount asked for
		// return failure
        rrprintcorruption("OodleLZ_Decompress failed (%d != %d)\n",(int)rawDecoded,(int)rawLen);
                        
        return OODLELZ_FAILED;
    }
    else
    {
        return rawDecoded; // - decodeStartOffset;
    }
}

// RAD_LZ_DECODE_MEMCPY
// hacky : need to assign a pretend compressor for the decodetype
//	 because memcpy is now a flag added to a base compressor
//	(this is used for Compressor_None, not for memcpy blocks on other compressors, they stay themselves)
// does the choice of compressor here affect anything?
//	-> yes it does
//	decoder mem size, fuzz safety flag, etc.
//	why not use something like LZB16?
// old :
//#define RAD_LZ_DECODE_MEMCPY RAD_LZ_DECODE_LZH
// new :
#define RAD_LZ_DECODE_MEMCPY RAD_LZ_DECODE_LZB16  // <- fuzz safe and tiny Decoder object

static const S32 s_OodleLZ_Compressor_to_DecodeType_table[] = 
{
	//OodleLZ_Compressor_LZH = 0,   // DEPRECATED : LZH , 128k sliding window ; generally use LZHLW instead
	RAD_LZ_DECODE_LZH,
	
	//OodleLZ_Compressor_LZHLW = 1, // DEPRECATED : LZH-LargeWindow ; fast to decode, good compression
	RAD_LZ_DECODE_LZHLW,
	
	//odleLZ_Compressor_LZNIB = 2, // DEPRECATED : LZ-Nibbled ; fast to decompress + medium compression
	RAD_LZ_DECODE_LZNIB,
	
	//OodleLZ_Compressor_None = 3,  // None = memcpy, pass through uncompressed bytes
	RAD_LZ_DECODE_MEMCPY,
	
	//OodleLZ_Compressor_LZB16 = 4, // DEPRECATED : LZB16 = LZ-Bytewise ; 64k window ; fast, low compression.  Generally prefer Selkie unless you need the 64k window
	RAD_LZ_DECODE_LZB16,
	
	//OodleLZ_Compressor_LZBLW = 5, // DO NOT USE : LZBLW ; use Selkie instead
	RAD_LZ_DECODE_LZBLW,
	
	//OodleLZ_Compressor_LZA = 6,   // DO NOT USE : LZA ; use LZNA instead
	RAD_LZ_DECODE_LZA,
	
	//OodleLZ_Compressor_LZNA = 7,  // DEPRECATED : LZNA : the highest compression option in Oodle, comparable to LZMA (7zip) but much faster to decode
	RAD_LZ_DECODE_LZNA,
	
	//OodleLZ_Compressor_Kraken = 8,    // Fast decodes, high compression, amazing! NOTE : LARGE QUANTUM
	RAD_LZ_DECODE_KRAKEN,

	//OodleLZ_Compressor_Mermaid = 9,   // Mermaid is between Kraken & Selkie - crazy fast, still decent compression. NOTE : LARGE QUANTUM
	RAD_LZ_DECODE_MERMAID,
	
	//OodleLZ_Compressor_BitKnit = 10, // DEPRECATED : BitKnit ; usually close to LZNA compression levels but faster.  Particularly great on some types of structured binary, such as structs of DWORD/float.:
	RAD_LZ_DECODE_BITKNIT,

	//OodleLZ_Compressor_Selkie = 11,   // Selkie is a super-fast relative of Mermaid.  Faster than LZB16/LZ4 but more compression.  NOTE : LARGE QUANTUM ; NOTE : Selkie will show up as Mermaid data in the decoder
	RAD_LZ_DECODE_MERMAID,
	
	//OodleLZ_Compressor_Hydra = 12,    // Hydra, the many-headed beast = Leviathan, Kraken, Mermaid, or Selkie (see $OodleLZ_About_Hydra)
	RAD_LZ_DECODE_KRAKEN,

	//OodleLZ_Compressor_Leviathan = 13,// Leviathan = Kraken's big brother with higher compression. NOTE : LARGE QUANTUM
	RAD_LZ_DECODE_LEVIATHAN
};
	
RR_COMPILER_ASSERT( RR_ARRAY_SIZE(s_OodleLZ_Compressor_to_DecodeType_table) == OodleLZ_Compressor_Count );

static S32 OodleLZ_Compressor_to_DecodeType(OodleLZ_Compressor compressor)
{
	RR_ASSERT( s_OodleLZ_Compressor_to_DecodeType_table[OodleLZ_Compressor_Kraken] == RAD_LZ_DECODE_KRAKEN );
	RR_ASSERT( s_OodleLZ_Compressor_to_DecodeType_table[OodleLZ_Compressor_Leviathan] == RAD_LZ_DECODE_LEVIATHAN );
	RR_ASSERT( s_OodleLZ_Compressor_to_DecodeType_table[OodleLZ_Compressor_None] == RAD_LZ_DECODE_MEMCPY );

	UINTa ic = (UINTa)compressor;
	if ( ic >= OodleLZ_Compressor_Count )
	{
		RR_ASSERT_FAILURE_ALWAYS("Invalid compressor\n");
		return -1;
	}

	return s_OodleLZ_Compressor_to_DecodeType_table[ic];
}

// indexed by RAD_LZ_DECODE_* :
RR_COMPILER_ASSERT( RAD_LZ_DECODE_LZHLW == 0 );
RR_COMPILER_ASSERT( RAD_LZ_DECODE_LZNIB == 1 );
static const OodleLZ_Compressor s_OodleLZ_DecodeType_to_Compressor_table[] = 
{
	// RAD_LZ_DECODE_LZHLW, etc.
	OodleLZ_Compressor_LZHLW,
	OodleLZ_Compressor_LZNIB,
	OodleLZ_Compressor_LZB16,
	OodleLZ_Compressor_LZBLW,
	OodleLZ_Compressor_LZA,
	OodleLZ_Compressor_LZNA,
	OodleLZ_Compressor_Kraken,
	OodleLZ_Compressor_LZH,
	OodleLZ_Compressor_LZH,
	OodleLZ_Compressor_LZH,
	OodleLZ_Compressor_Mermaid,
	OodleLZ_Compressor_BitKnit,
	OodleLZ_Compressor_Leviathan
};

RR_COMPILER_ASSERT( RR_ARRAY_SIZE(s_OodleLZ_DecodeType_to_Compressor_table) == RAD_LZ_DECODE_COUNT );

static OodleLZ_Compressor OodleLZ_DecodeType_to_Compressor(S32 decodeType)
{
	RR_ASSERT( s_OodleLZ_DecodeType_to_Compressor_table[RAD_LZ_DECODE_LEVIATHAN] == OodleLZ_Compressor_Leviathan );
	RR_ASSERT( s_OodleLZ_DecodeType_to_Compressor_table[RAD_LZ_DECODE_MERMAID] == OodleLZ_Compressor_Mermaid );
	RR_ASSERT( s_OodleLZ_DecodeType_to_Compressor_table[RAD_LZ_DECODE_LZNA] == OodleLZ_Compressor_LZNA );

	// LZBlockHeader_Get has already validated decodeType
	RR_ASSERT( decodeType >= 0 && decodeType < RAD_LZ_DECODE_COUNT );
	return s_OodleLZ_DecodeType_to_Compressor_table[decodeType];
}

OOFUNC1 OodleLZ_Compressor OOFUNC2 OodleLZ_GetFirstChunkCompressor(const void * compBuf,
									SINTa compBufAvail,
									rrbool * pIndependent)
{
	OOFUNCSTART
	
	// must have at least OODLELZ_BLOCK_HEADER_BYTES_MAX
	RR_COMPILER_ASSERT( OODLELZ_BLOCK_HEADER_BYTES_MAX == 2 );
	// it is possible to have a 1 byte block header
	//	but without at least 1 byte of payload it won't go anywhere anyway
	if ( compBufAvail < OODLELZ_BLOCK_HEADER_BYTES_MAX ) 
		return OodleLZ_Compressor_Invalid;
		
	LZBlockHeader header;
	const U8 * compPtr = (U8 *)compBuf;
    compPtr = LZBlockHeader_Get(&header,compPtr);
    if ( compPtr == NULL )
    {
        rrprintcorruption("invalid header..\n");
        return OodleLZ_Compressor_Invalid;
    }
    
    if ( pIndependent )
    {		
		// header.chunkIsIndependent should always be set right now
		*pIndependent = header.chunkIsReset;
    }
    
	return OodleLZ_DecodeType_to_Compressor(header.decodeType);
}

// @@@@ DEPRECATED : REMOVE ME IN OODLE 2.9.0 !
OOFUNC1 OodleLZ_Compressor OOFUNC2 OodleLZ_GetChunkCompressor(const void * compChunkPtr,
									SINTa compBufAvail,
									rrbool * pIndependent)
{
	return OodleLZ_GetFirstChunkCompressor(compChunkPtr,compBufAvail,pIndependent);
}
									
OOFUNC1 OodleLZ_Compressor OOFUNC2 OodleLZ_GetAllChunksCompressor(const void * compBuf,
									SINTa compBufSize,
									SINTa rawLen)
{
	RR_ASSERT( compBufSize > 0 );
	RR_ASSERT( rawLen > 0 );
	
	OodleLZ_Compressor compressor = OodleLZ_GetFirstChunkCompressor(compBuf,compBufSize,NULL);
	
	if ( compressor == OodleLZ_Compressor_Invalid )
	{
		ooLogError("Invalid chunk compressor in OodleLZ_GetAllChunksCompressor\n");
		return OodleLZ_Compressor_Invalid;
	}
	
	// optimize common case :
	//	compressor can only change at BLOCK granularity
	//	so anything smaller will just have FirstChunkCompressor 
	if ( rawLen <= OODLELZ_BLOCK_LEN )
		return compressor;
	
	const U8 * compPtr = U8_void(compBuf);
	SINTa compAvail = compBufSize;
	SINTa rawLenLeft = rawLen;
	
	while( compAvail > 0 )
	{
		//if ( rawLen <= 0 ) rawLenLeft = OODLELZ_BLOCK_LEN;
		SINTa rawStep = RR_MIN(rawLenLeft,OODLELZ_BLOCK_LEN);
		SINTa gotRawStep = 0;
		SINTa compStep = OodleLZ_GetCompressedStepForRawStep(compPtr,compAvail,0,rawStep,&gotRawStep,NULL);
		
		if ( compStep == compAvail ) break; // nice end of data
		if ( gotRawStep != rawStep ) break; // couldn't step a whole block
		
		if ( compStep <= 0 || compStep >= compAvail )
		{
			rrprintcorruption("insufficient comp data in OodleLZ_GetAllChunksCompressor\n");
			break;
		}
		
		compPtr += compStep;
		compAvail -= compStep;
		rawLenLeft -= rawStep;
		
		if ( rawLenLeft == 0 )
			break; // raw target reached
		
		RR_ASSERT( rawLenLeft > 0 );
		
		OodleLZ_Compressor cur_compressor = OodleLZ_GetFirstChunkCompressor(compPtr,compAvail,NULL);
		
		RR_ASSERT( cur_compressor != OodleLZ_Compressor_Invalid );
		if ( cur_compressor == OodleLZ_Compressor_Invalid )
		{
			ooLogError("Invalid chunk compressor in OodleLZ_GetAllChunksCompressor\n");
			return OodleLZ_Compressor_Invalid;
		}
		
		if ( compressor != cur_compressor )
		{
			if ( OodleLZ_Compressor_IsNewLZFamily(cur_compressor) )
			{
				if ( OodleLZ_Compressor_IsNewLZFamily(compressor) )
				{
					// mix of newlz's = Hydra
					compressor = OodleLZ_Compressor_Hydra;
					// can't return early, have to keep scanning in case any non-newlz was tacked on :(
				}
				else
				{
					// mix of newlz & non-newlz , just return Count = "Any"
					return OodleLZ_Compressor_Count;
				}
			}
			else
			{
				// mix of different non-newlz's , just return Count = "Any"
				return OodleLZ_Compressor_Count;
			}
		}
	}
	
	return compressor;
}


// decoder is allowed to overrun a tiny bit :
OOFUNC1 SINTa OOFUNC2 OodleLZ_GetDecodeBufferSize(OodleLZ_Compressor compressor,SINTa rawSize,rrbool corruptionPossible)
{
	if ( OodleLZ_Compressor_IsNewLZFamily(compressor) )
	{
		// no padding needed
		// it is assumed you will be using FuzzSafe_Yes
		return rawSize;
	}
	else
	{
		// LZB16 still needs padding for SW decode
		//	so this isn't entirely deprecated in 2.9.0

		if ( corruptionPossible )
			return rawSize + 256;
		else
			return rawSize + 16;
	}
}


// scan compressed LZ stream to make the seek table :
OOFUNC1 SINTa OOFUNC2 OodleLZ_GetInPlaceDecodeBufferSize(OodleLZ_Compressor compressor,SINTa compLen, SINTa rawLen)
{
	OOFUNCSTART

	RR_ASSERT( compLen <= OodleLZ_GetCompressedBufferSizeNeeded(compressor,rawLen) );
	
	if ( compLen <= 0 )
	{
		// if compLen is unknown, make room for it to be the maximum possible :
		compLen = OodleLZ_GetCompressedBufferSizeNeeded(compressor,rawLen);
	}
	
	if ( OodleLZ_Compressor_IsNewLZFamily(compressor) )
	{
		// @@@@ can I just use OodleLZ_GetCompressedBufferSizeNeeded ?
		//	that does simplify things somewhat
		// it is almost the same
	
		SINTa inPlaceSize = rawLen;
		
		SINTa numChunks = (rawLen + OODLELZ_BLOCK_LEN-1)/OODLELZ_BLOCK_LEN;
		RR_ASSERT( numChunks >= 1 );
		// note numChunks-1 : don't need current chunk expansion at all
		//SINTa maxChunkExpansion = (numChunks - 1) * OODLELZ_BLOCK_HEADER_BYTES_MAX;
		//SINTa maxChunkExpansion = OODLELZ_BLOCK_HEADER_BYTES_MAX;
		
		// Older Oodles (220) mistakenly wrote memcpy blocks using the LQH
		//	so they need room for each block to expand via the LQH not the block header
		//  see compresstest lzt:/lzt58 r:\lzt58.kraken.220.comp
		SINTa maxChunkExpansion = OODLELZ_QUANTUM_MAXIMUM_EXPANSION + OODLELZ_BLOCK_HEADER_BYTES_MAX;
		
		inPlaceSize += numChunks * maxChunkExpansion;
	
		/*
		// in theory we only need OODLELZ_QUANTUM_MAXIMUM_EXPANSION once
		
		// Large quantum header can expand 
		if ( rawLen > NEWLZ_CHUNK_LEN )
		{
			SINTa maxQuantumExpansion = OODLELZ_QUANTUM_MAXIMUM_EXPANSION;

			inPlaceSize += maxQuantumExpansion;
		}
		*/
		
		// maximum newlz chunk expansion :
		inPlaceSize += NEWLZ_CHUNK_HEADER_SIZE;
		
		// in quantum expansion is not a problem because we decode into scratch first
		//	(compressors that point to comp, like Selkie, are forced to memcpy out to scratch
		//	 when doing in place decode)
		
		// never need more than raw+comp+pads :
		SINTa inPlaceSizeLimit = rawLen + compLen;
		
		inPlaceSize = RR_MIN(inPlaceSize,inPlaceSizeLimit);

		// of course has to be at least compLen :
		inPlaceSize = RR_MAX(inPlaceSize,compLen);

		return inPlaceSize;
	}
	else // old codecs
	{
		// start with padding for dcbz and string-copy-run-aheads :
		SINTa inPlaceSize = rawLen + 256 + 16;
		
		// never need more than raw+comp+pads : (this includes the run-ahead padd)
		SINTa inPlaceSizeLimit = inPlaceSize + compLen;
		
		RR_ASSERT( inPlaceSize >= OodleLZ_GetDecodeBufferSize(compressor,rawLen,true) );
		
		SINTa numChunks = (rawLen + OODLELZ_BLOCK_LEN-1)/OODLELZ_BLOCK_LEN;
		SINTa maxChunkExpansion = OODLELZ_QUANTUM_MAXIMUM_EXPANSION + OODLELZ_BLOCK_HEADER_BYTES_MAX;

		inPlaceSize += numChunks * maxChunkExpansion;
		
		inPlaceSize += maxChunkExpansion;
		
		// 15 of the 16 quanta can expand :
		SINTa maxQuantumExpansion = 15 * OODLELZ_QUANTUM_MAXIMUM_EXPANSION;

		inPlaceSize += maxQuantumExpansion;
		
		// maxInQuantumExpansion is how far can the compressed data run ahead of the raw data
		//	at any point inside a quantum, for a quantum that is still overall compressible
		//	eg. a quantum that breaks down like :
		//		{15000 compressed bytes for 8192 raw bytes}{1383 compressed bytes for 8192 raw bytes}
		 // LZNIB seems to need a lot ?
		// @@ - I think that the new compressors don't need this!
		SINTa maxInQuantumExpansion = OODLELZ_QUANTUM_LEN/2;

		// this is the old value that caused failure :
		// lznib failed at -z4 level on lzt83
		// can't repro now even with low maxInQuantumExpansion
		//SINTa maxInQuantumExpansion = OODLELZ_QUANTUM_LEN / 6;

		inPlaceSize += maxInQuantumExpansion;

		inPlaceSize = RR_MIN(inPlaceSize,inPlaceSizeLimit);

		// of course has to be at least compLen :
		inPlaceSize = RR_MAX(inPlaceSize,compLen);

		// this should be >= the inplacesize for newlz

		return inPlaceSize;
	}
}


SINTa OodleLZ_CompressMemcpy_DecodeType(int decodeType,const U8 * rawBuf,SINTa rawLen,U8 * compBuf,const U8 * dicBase,const OodleLZ_CompressOptions * pOptions)
{
    U8 * compPtr = compBuf;
	const U8 * rawPtr = rawBuf;

	if ( dicBase == NULL ) dicBase = rawBuf;

	// put the one byte header specifying a memcpy chunk :
	LZBlockHeader header = { 0 };
	header.version = RAD_LZ_HEADER_VERSION;
	header.decodeType = decodeType;
	header.offsetShift = 0;
	header.chunkIsMemcpy = true;
	header.chunkHasQuantumCRCs = false;
				
	while( rawPtr < (rawBuf + rawLen) )
	{
		SINTa rawLenLeft = rrPtrDiff(rawBuf + rawLen - rawPtr );
		SINTa curRawLen = RR_MIN( rawLenLeft, OODLELZ_BLOCK_LEN ); 

		SINTa rawPos = rrPtrDiff(rawPtr - dicBase);
		header.chunkIsReset = LZBlockHeader_IsReset(rawPos,pOptions);

		compPtr = LZBlockHeader_Put(header,compPtr);
	    
		// blit it in :
		memcpy(compPtr,rawPtr,(size_t)curRawLen);
		compPtr += curRawLen;
		rawPtr += curRawLen;	    
    }
    
    return rrPtrDiff( compPtr - compBuf );
}

SINTa OodleLZ_CompressMemcpy_Compressor(OodleLZ_Compressor compressor,const U8 * rawBuf,SINTa rawLen,U8 * compBuf,const U8 * dicBase,const OodleLZ_CompressOptions * pOptions)
{
	return OodleLZ_CompressMemcpy_DecodeType( OodleLZ_Compressor_to_DecodeType(compressor), rawBuf, rawLen,  compBuf, dicBase, pOptions );
}					


/**

OodleLZ_GetCompressedStepForRawStep lets you step through the pakced stream without decoding
tell it how many raw bytes to skip (multiples of OODLELZ_QUANTUM_LEN) and it tells you how many packed bytes that is

note you do have to decode them anyway because it's data needed for the match window
but can be useful for breaking a decode into multiple steps

returns 0 for valid not-enough-data case
returns -1 for error

NOTE : *can* return comp step > comp avail!

**/
OOFUNC1 SINTa OOFUNC2 OodleLZ_GetCompressedStepForRawStep(const void * v_compPtrBase, SINTa compAvail, 
					SINTa startRawPos, SINTa rawSeekBytes, 
					SINTa * pEndRawPos RADDEFAULT(NULL),
					rrbool * pIndependent RADDEFAULT(NULL) )
{
	OOFUNCSTART

	const U8 * compPtrBase = U8_void(v_compPtrBase);

	// start raw pos must be on a quantum :
	RR_ASSERT( (startRawPos & (OODLELZ_QUANTUM_LEN-1)) == 0 );
	if ( (startRawPos & (OODLELZ_QUANTUM_LEN-1)) != 0 )
	{
		ooLogError("GetCompressedStepForRawStep : start pos not a quantum!\n");
		return -1;
	}
	
	/*
	// NO : more than that, it must be on a Seek Chunk so I can get my chunk type
	//	otherwise I would have to remember if I was in a MemCpy or LZH chunk
	RR_ASSERT( (startRawPos & (OODLELZ_BLOCK_LEN-1)) == 0 );
	if ( (startRawPos & (OODLELZ_BLOCK_LEN-1)) != 0 )
	{
		rrprintf("SeekInPackedStream : invalid start pos\n");
		return 0;
	}
	*/
	
	RR_ASSERT( compAvail >= 2 );
	if ( compAvail < OODLELZ_BLOCK_HEADER_BYTES_MAX ) // OODLELZ_BLOCK_HEADER_BYTES_MAX is 2
	{
		return 0;
	}

	// rawSeekBytes must be a quantum : // not true at the end of the fule
	// RR_ASSERT_ALWAYS( (rawSeekBytes & (OODLELZ_QUANTUM_LEN-1)) == 0 );
	
	SINTa rawPos = startRawPos;
	SINTa endRawPos = startRawPos + rawSeekBytes;
	const U8 * compPtr = compPtrBase;
	const U8 * compEnd = compPtrBase + compAvail;
	
	// start with true and then set it to false if any non-independent situations come up
	if ( pIndependent )
		*pIndependent = true;
	
	FUZZ_MUNGE( ((U8 *)compPtr)[0] );
	
	LZBlockHeader chunkHeader = { 0 };
	SINTa start_of_block_comp_pos = 0;
			
	while( rawPos < endRawPos )
	{
		RR_ASSERT( compPtr <= compEnd );
			
		SINTa nextBlockPos = rrAlignUpA(rawPos+1,OODLELZ_BLOCK_LEN);
		nextBlockPos = RR_MIN(nextBlockPos,endRawPos);
		S32 chunkLen = (S32) (nextBlockPos - rawPos);
		RR_ASSERT( chunkLen <= OODLELZ_BLOCK_LEN );
	
		//if ( logQuanta )	rrprintf("[");
	
		bool seekQuanta = false;
	
		if ( (rawPos & (OODLELZ_BLOCK_LEN -1) ) == 0 )
		{
			// at start of a seek chunk, get header :
			
			if ( rrPtrDiff(compEnd - compPtr) < OODLELZ_BLOCK_HEADER_BYTES_MAX )
				return 0;
		
			start_of_block_comp_pos = rrPtrDiff(compPtr - compPtrBase);
			
			compPtr = LZBlockHeader_Get(&chunkHeader,compPtr);

			REQUIRE_FUZZ_RETURN(compPtr != NULL,0);

			if ( chunkHeader.chunkIsMemcpy )
			{			
				//REQUIRE_FUZZ_RETURN( rrPtrDiff(compEnd - compPtr) >= chunkLen , 0 );
				if ( rrPtrDiff(compEnd - compPtr) < chunkLen )
				{
					// not enough comp data for memcpy chunk step
					
					if ( pEndRawPos )
						*pEndRawPos = rawPos;

					return start_of_block_comp_pos;
				}
			
				rawPos  += chunkLen;
				compPtr += chunkLen;	
				seekQuanta = false;	
				
				//if ( pIndependent )
				//	*pIndependent = true;
						
				//if ( logQuanta ) rrprintf("MEMCPY");
					
			}
			else if ( chunkHeader.decodeType == RAD_LZ_DECODE_MERMAID ||
				chunkHeader.decodeType == RAD_LZ_DECODE_KRAKEN ||
				chunkHeader.decodeType == RAD_LZ_DECODE_LEVIATHAN )
			{
				if ( pIndependent && ! chunkHeader.chunkIsReset )
					*pIndependent = false;
								
				seekQuanta = false;
				
				LZQuantumHeader qh;
				int lzqhlen = LZLargeQuantumHeader_Get(compPtr,compEnd,&qh,chunkHeader.chunkHasQuantumCRCs,chunkLen);
				
				// @@ LAME : using OODLELZ_QUANTUM_HEADER_MAX_SIZE to make a guess about
				//	 whether it's a data starvation or corruption
				//	we return either way, but it changes the return from 0 to -1
				//	(we're failing to return corruption in many cases when compAvail is < OODLELZ_QUANTUM_HEADER_MAX_SIZE)
				if ( lzqhlen <= 0 )
				{
					if ( rrPtrDiff(compEnd-compPtr) < OODLELZ_QUANTUM_HEADER_MAX_SIZE )
					{
						// not enough comp data to get header
						
						if ( pEndRawPos )
							*pEndRawPos = rawPos;

						return start_of_block_comp_pos;
					}
					else
					{
						// plenty of data but failed anyway, corrupt -
						REQUIRE_FUZZ_RETURN( lzqhlen > 0 , -1 );
					}
				}
				
				REQUIRE_FUZZ_RETURN( rrPtrDiff(compEnd - compPtr) >= lzqhlen , 0 );
				
				compPtr += lzqhlen;
        		
				REQUIRE_FUZZ_RETURN( qh.compLen >= 0 , -1 );
				REQUIRE_FUZZ_RETURN( qh.compLen <= OODLELZ_BLOCK_LEN , -1 );

				REQUIRE_FUZZ_RETURN( rrPtrDiff(compEnd - compPtr) >= qh.compLen , 0 );
				
				compPtr += qh.compLen;
				rawPos  += chunkLen;
			}
			else if ( chunkHeader.decodeType == RAD_LZ_DECODE_LZH ||
				chunkHeader.decodeType == RAD_LZ_DECODE_LZHLW ||
				chunkHeader.decodeType == RAD_LZ_DECODE_BITKNIT ||
				chunkHeader.decodeType == RAD_LZ_DECODE_LZA ||
				chunkHeader.decodeType == RAD_LZ_DECODE_LZNA ||
				chunkHeader.decodeType == RAD_LZ_DECODE_LZNIB ||
				chunkHeader.decodeType == RAD_LZ_DECODE_LZBLW ||
				chunkHeader.decodeType == RAD_LZ_DECODE_LZB16 )
			{
				if ( pIndependent && ! chunkHeader.chunkIsReset )
					*pIndependent = false;
					
				seekQuanta = true;	
			}
			else
			{
				ooLogError("corruption : unknown LZ code type");
				return -1;
			}
		}
		else
		{
			// didn't start at a chunk - must be in LZ quanta
		
			// no information about this chunk, assume not independent :
			if ( pIndependent )
				*pIndependent = false;
					
			seekQuanta = true;
		}

		if ( seekQuanta )
		{
			S32 chunkGot = 0;
			while(chunkGot < chunkLen)
			{
				FUZZ_MUNGE( ((U8 *)compPtr)[0] );
	
				S32 quantumRawLen = RR_MIN( (chunkLen - chunkGot), OODLELZ_QUANTUM_LEN );
				
				LZQuantumHeader qh;
				int lzqhlen = LZQuantumHeader_Get(compPtr,compEnd,&qh,chunkHeader.chunkHasQuantumCRCs,quantumRawLen);
				
				if ( lzqhlen <= 0 ||
					 ( rrPtrDiff(compEnd - compPtr) < lzqhlen + qh.compLen &&
					 rawPos > startRawPos ) )
				{
					// not enough comp data to get header
					// OR not enough comp data to get next chunk AND we've made one step already
								
					if ( pEndRawPos )
						*pEndRawPos = rawPos;

					return rrPtrDiff( compPtr - compPtrBase );
				}
				else if ( rrPtrDiff(compEnd - compPtr) < lzqhlen + qh.compLen )
				{
					return 0;
				}
				
				REQUIRE_FUZZ_RETURN( lzqhlen > 0 , -1 );
				compPtr += lzqhlen;
        		
				REQUIRE_FUZZ_RETURN( qh.compLen >= 0 , -1 );
				REQUIRE_FUZZ_RETURN( qh.compLen <= OODLELZ_QUANTUM_LEN , -1 );
						
				compPtr += qh.compLen;
				chunkGot += quantumRawLen;
				rawPos  += quantumRawLen;
				
				if ( rawPos >= endRawPos && chunkGot < chunkLen )
				{
					// got to endRawPos, but it didn't make a full chunk
					// this is a valid full return at end of buffer
					
					RR_ASSERT( rawPos == endRawPos );
								
					if ( pEndRawPos )
						*pEndRawPos = rawPos;

					return rrPtrDiff( compPtr - compPtrBase );
				}
			}

			RR_ASSERT( compPtr <= compEnd ); // should have been checked already
			REQUIRE_FUZZ_RETURN( compPtr <= compEnd , -1 );		
			REQUIRE_FUZZ_RETURN( chunkGot == chunkLen , -1 );
		}
	}
	
				
	if ( pEndRawPos )
		*pEndRawPos = rawPos;

	RR_ASSERT( compPtr <= compEnd ); // should have been checked already
											
	return rrPtrDiff( compPtr - compPtrBase );
}

//===================================================

#if 0

// OodleLZ_GetRawSizeFromCompressedBuf
// this does NOT work
//	because the last quantum no longer contains rawLen
//	so I can only find RawSize up to the next quantum size

OOFUNC1 SINTa OOFUNC2 OodleLZ_GetRawSizeFromCompressedBuf(const void * v_compPtrBase, SINTa compSize)
{
	const U8 * compPtrBase = U8_void(v_compPtrBase);
	const U8 * compPtrEnd = compPtrBase + compSize;
	const U8 * compPtr = compPtrBase;
	
	SINTa tot_raw_len = 0;
	
	while( compPtr < compPtrEnd )
	{
		SINTa gotRaw;
		SINTa compUsed = OodleLZ_GetCompressedStepForRawStep(compPtr,rrPtrDiff(compPtrEnd - compPtr),0,OODLELZ_BLOCK_LEN,&gotRaw,NULL);
		
		if ( compUsed == 0 )
		{
			ooLogError("Bad buffer in OodleLZ_GetRawSizeFromCompressedBuf\n");
			return OODLELZ_FAILED;
		}
		
		RR_ASSERT_ALWAYS( gotRaw > 0 && gotRaw <= OODLELZ_BLOCK_LEN );
		
		compPtr += compUsed;
		tot_raw_len += gotRaw;
		
		if ( gotRaw < OODLELZ_BLOCK_LEN )
		{
			RR_ASSERT( compPtr == compPtrEnd );
			
			if ( compPtr != compPtrEnd )
			{
				ooLogError("Bad buffer in OodleLZ_GetRawSizeFromCompressedBuf\n");
				return OODLELZ_FAILED;
			}
		}
	}
	
	return tot_raw_len;
}

#endif

//===================================================

/**

seekChunkLen
strictly only needs to be a multiple of OODLELZ_BLOCK_LEN
I'm making it be a power of 2
for ease of interaction with the other chunks (parallel chunk, LRM chunk)

**/

OOFUNC1 S32 OOFUNC2 OodleLZ_MakeSeekChunkLen(S64 rawLen, S32 desiredSeekPointCount)
{
	if ( rawLen < OODLELZ_SEEKCHUNKLEN_MIN )
		return OODLELZ_SEEKCHUNKLEN_MIN;
		
	if ( desiredSeekPointCount <= 1 )
	{
		if ( rawLen >= (S64)OODLELZ_SEEKCHUNKLEN_MAX )
			return OODLELZ_SEEKCHUNKLEN_MAX;
		else
			return rrNextPow2(check_value_cast<U32>(rawLen));
	}

	// must be a multiple of OODLELZ_BLOCK_LEN - no must be pow2
	// try to get close to desiredSeekPointCount of them
	S64 divLen = rawLen / desiredSeekPointCount;
			
	if ( divLen >= ((S64)OODLELZ_SEEKCHUNKLEN_MAX) )
		return OODLELZ_SEEKCHUNKLEN_MAX;
	
	U32 divLen32 = check_value_cast<U32>(divLen);
	
	if ( divLen32 <= (S64)OODLELZ_SEEKCHUNKLEN_MIN )
		return OODLELZ_SEEKCHUNKLEN_MIN;
			
	U32 l = rrIlog2round(divLen32);
	S32 ret = 1<<l;
	
	RR_ASSERT( ret >= OODLELZ_SEEKCHUNKLEN_MIN && ret <= OODLELZ_SEEKCHUNKLEN_MAX );
	
	return ret;
}

#define SELECT_SEEK_POINT_COUNT	(16)

OOFUNC1 S32 OOFUNC2 OodleLZ_GetNumSeekChunks(S64 rawLen, S32 seekChunkLen)
{
	if ( seekChunkLen <= 0 )
	{
		seekChunkLen = OodleLZ_MakeSeekChunkLen(rawLen,SELECT_SEEK_POINT_COUNT);
	}
	
	S64 numSeeks = (rawLen + seekChunkLen-1)/seekChunkLen;
	return S32_check(numSeeks);
}

OOFUNC1 SINTa OOFUNC2 OodleLZ_GetSeekTableMemorySizeNeeded(S32 numSeekChunks,OodleLZSeekTable_Flags flags)
{
	SINTa needed = sizeof(OodleLZ_SeekTable) + sizeof(U32)*numSeekChunks;
	if ( flags & OodleLZSeekTable_Flags_MakeRawCRCs )
		needed += sizeof(U32)*numSeekChunks;
	return needed;
}

OOFUNC1 OodleLZ_SeekTable * OOFUNC2 OodleLZ_CreateSeekTable(OodleLZSeekTable_Flags flags,S32 seekChunkLen,const void * rawBuf, SINTa rawLen,const void * compBuf,SINTa compLen)
{
	PARAMETER_CHECK( compBuf != NULL , NULL );
	PARAMETER_CHECK( compLen > 0 , NULL );

	if ( seekChunkLen <= 0 )
	{
		seekChunkLen = OodleLZ_MakeSeekChunkLen(rawLen,SELECT_SEEK_POINT_COUNT);
	}

	S32 numSeekChunks = OodleLZ_GetNumSeekChunks(rawLen,seekChunkLen);
	SINTa memSize = OodleLZ_GetSeekTableMemorySizeNeeded(numSeekChunks,flags);
	void * mem = OodleMalloc(memSize);
	OodleLZ_SeekTable * table = (OodleLZ_SeekTable *) mem;
	if ( ! OodleLZ_FillSeekTable(table,flags,seekChunkLen,rawBuf,rawLen,compBuf,compLen) )
	{
		OodleFree(mem);
		return NULL;
	}
	return table;
}

OOFUNC1 void OOFUNC2 OodleLZ_FreeSeekTable(OodleLZ_SeekTable * pTable)
{
	OodleFree(pTable);
}

OOFUNC1 S32 OOFUNC2 OodleLZ_FindSeekEntry( S64 rawPos, const OodleLZ_SeekTable * seekTable)
{
	S64 seekI = rawPos / seekTable->seekChunkLen;
	return S32_check(seekI);
}

OOFUNC1 S64 OOFUNC2 OodleLZ_GetSeekEntryPackedPos( S32 seekI , const OodleLZ_SeekTable * seekTable )
{
	// @@ have to accumulate compLens
	//	sucky ; perhaps should cache
	//	space/speed tradeoff yo
	S64 compPos = 0;
	for(int i=0;i<seekI;i++)
	{
		compPos += seekTable->seekChunkCompLens[i];
	}
	return compPos;
}

OOFUNC1 rrbool OOFUNC2 OodleLZ_CheckSeekTableCRCs(const void * rawBuf,SINTa rawLen, const OodleLZ_SeekTable * seekTable)
{
	S64 rawFileLen = seekTable->totalRawLen;
	if ( rawLen != rawFileLen )
		return false;
	
	// no CRC's to check; is that a success or fail ?
	if ( seekTable->rawCRCs == NULL )
		return true;
	
	SINTa rawPos = 0;
	int curSeekTableI = 0;
	S32 seekChunkLen = seekTable->seekChunkLen;
	
	while( rawPos < rawLen )
	{
		int chunkLen = (int) RR_MIN( seekChunkLen , (rawLen - rawPos) );
		
		U32 crc = rrLZH_CRC_Block((const U8 *)rawBuf + rawPos,chunkLen);
		
		// start a chunk :
		if ( crc != seekTable->rawCRCs[curSeekTableI] )
			return false;
		
		rawPos += chunkLen;
		curSeekTableI++;
	}
	
	return true;
}


void OodleLZ_InitSeekTable(OodleLZ_SeekTable * pTable,OodleLZSeekTable_Flags flags,S32 numSeekChunks)
{
	RR_ZERO(*pTable);

	// arrays are at the end of pTable
	U8 * pMem = (U8 *)pTable;
	U8 * pEnd = pMem + sizeof(OodleLZ_SeekTable);
	pTable->seekChunkCompLens = (U32 *) pEnd;
	if ( flags & OodleLZSeekTable_Flags_MakeRawCRCs )
	{
		pTable->rawCRCs = pTable->seekChunkCompLens + numSeekChunks;
	}
	else
	{
		pTable->rawCRCs = NULL;
	}
}

OOFUNC1 rrbool OOFUNC2 OodleLZ_FillSeekTable(OodleLZ_SeekTable * pTable,OodleLZSeekTable_Flags flags,S32 seekChunkLen,const void * rawBuf, SINTa rawLen,const void * compBuf,SINTa compLen)
{
	//PARAMETER_CHECK( rawBuf != NULL  ,false);
	PARAMETER_CHECK( pTable != NULL ,false);
	PARAMETER_CHECK( compBuf != NULL ,false);
	PARAMETER_CHECK( compLen > 0 ,false);

	if ( seekChunkLen <= 0 )
	{
		seekChunkLen = OodleLZ_MakeSeekChunkLen(rawLen,SELECT_SEEK_POINT_COUNT);
	}
	else
	{
		PARAMETER_CHECK( seekChunkLen >= OODLELZ_BLOCK_LEN , false );
		PARAMETER_CHECK( (seekChunkLen % OODLELZ_BLOCK_LEN) == 0 , false );
	}
	
	S32 numSeekChunks = OodleLZ_GetNumSeekChunks(rawLen,seekChunkLen);
	
	OodleLZ_InitSeekTable(pTable,flags,numSeekChunks);
	
	pTable->numSeekChunks = numSeekChunks;
	pTable->seekChunkLen = seekChunkLen;
	pTable->totalRawLen = rawLen;
	pTable->totalCompLen = compLen;
	
	// arrays are at the end of pTable
	U8 * pMem = (U8 *)pTable;
	U8 * pEnd = pMem + sizeof(OodleLZ_SeekTable);
	pTable->seekChunkCompLens = (U32 *) pEnd;
	if ( flags & OodleLZSeekTable_Flags_MakeRawCRCs )
	{
		pTable->rawCRCs = pTable->seekChunkCompLens + numSeekChunks;
	}
	else
	{
		pTable->rawCRCs = NULL;
	}

	// fill it out
	OodleLZ_Compressor compressor = OodleLZ_GetAllChunksCompressor(compBuf,compLen,rawLen);
	pTable->compressor = compressor;
	
	// start with true and set to false if we see any non-indy
	pTable->seekChunksIndependent = true;
	
	const U8 * compPtr = U8_void(compBuf);
	SINTa rawPos = 0;
	
	int curSeekTableI = 0;
	
	while( rawPos < rawLen )
	{
		S32 chunkLen = (S32) RR_MIN( seekChunkLen , (rawLen - rawPos) );
		SINTa compAvail = rrPtrDiff(U8_void(compBuf) + compLen - compPtr);
		
		rrbool cur_indy = false;
		OodleLZ_Compressor cur_compressor = OodleLZ_GetFirstChunkCompressor(compPtr,compAvail,&cur_indy);
		cur_compressor;
		if ( ! cur_indy )
			pTable->seekChunksIndependent = false;
		// compressor should be uniform ! <- no longer true in Hydra
		//RR_ASSERT_ALWAYS( cur_compressor == compressor );
	
		SINTa compStep = OodleLZ_GetCompressedStepForRawStep(compPtr,compAvail, rawPos, chunkLen );
		
		// start a chunk :
		pTable->seekChunkCompLens[curSeekTableI] = S32_checkA(compStep);

		if ( rawBuf && pTable->rawCRCs )
		{
			pTable->rawCRCs[curSeekTableI]  = rrLZH_CRC_Block(U8_void(rawBuf) + rawPos,chunkLen);
		}

		compPtr += compStep;
		rawPos += chunkLen;
		curSeekTableI++;
	}
	
	RR_ASSERT( curSeekTableI == numSeekChunks );
	
	return true;
}


void OodleLZ_LogBlocks(const void * compBuf,SINTa compLen, SINTa rawLen)
{
	const U8 * compPtr = U8_void(compBuf);
	const U8 * compEnd = compPtr + compLen;
	SINTa rawPos = 0;
	int chunkI = 0;
	
	while( compPtr < compEnd )
	{
		S32 chunkLen = (S32) RR_MIN( OODLELZ_BLOCK_LEN , (rawLen - rawPos) );
		SINTa compAvail = rrPtrDiff(compEnd - compPtr);
		
		rrbool cur_indy = false;
		OodleLZ_Compressor cur_compressor = OodleLZ_GetFirstChunkCompressor(compPtr,compAvail,&cur_indy);
	
		SINTa compStep = OodleLZ_GetCompressedStepForRawStep(compPtr,compAvail, rawPos, chunkLen );
		
		S32 compStep32 = S32_checkA(compStep);
		
		rrprintf("%3d : %7d -> %7d : %s %s\n",
			chunkI,
			chunkLen,compStep32,
			OodleLZ_Compressor_GetName(cur_compressor),
			cur_indy ? "independent" : "");

		compPtr += compStep;
		rawPos += chunkLen;
		chunkI++;
	}
}

static const char * c_OodleLZ_CompressionLevel_Name[] =
{
	"HyperFast4",
	"HyperFast3",
	"HyperFast2",
	"HyperFast1",
	"None",
	"SuperFast",
	"VeryFast",
	"Fast",
	"Normal",
	"Optimal1",
	"Optimal2",
	"Optimal3",
	"Optimal4",
	"Optimal5",
	"Too High"
};

OOFUNC1 const char * OOFUNC2 OodleLZ_CompressionLevel_GetName(OodleLZ_CompressionLevel level)
{
	OOFUNCSTART

	return c_OodleLZ_CompressionLevel_Name[ RR_CLAMP((int)level-OodleLZ_CompressionLevel_HyperFast4,0,(int)RR_ARRAY_SIZE(c_OodleLZ_CompressionLevel_Name)-1) ];
}

// get maximum expanded size for compBuf alloc :
// get maximum expanded size for compBuf alloc :
//	(note this is actually larger than the maximum compressed stream, it includes trash padding)
OOFUNC1 SINTa OOFUNC2 OodleLZ_GetCompressedBufferSizeNeeded(OodleLZ_Compressor compressor,SINTa rawSize)
{
	OOFUNCSTART
	
	// make it big enough to handle parallel compression of the chunks
	// each chunk needs enough space to expand
	
	// note the maximum compressed size is much smaller than this
	//	because expanded blocks would just write an uncomp block header
	//	but during compression of that block each individual quantum might expand
	// also note that this could be smaller for newLZ
	//	the small-quantum expansion is for old LZ
	
	// note OodleLZ_GetCompressedBufferSizeNeeded must be big enough for parallel encode
	// that means it must be >= OodleLZ_GetCompressedBufferSizeNeeded on each BLOCK
	// eg. you only actually need OODLELZ_QUANTUM_HEADER_MAX_SIZE padding once per encode
	//	 not for every block , but I add it for each block just for parallel
	// that means padding must done as paddingPerSeekChunk only
	//	so that it divides evenly for parallel encode
	
	S32 paddingPerSeekChunk;

	// if compressor is Invalid == All, we use the non-newlz branch, which is bigger
	if ( OodleLZ_Compressor_IsNewLZFamily(compressor) )
	{
		//	all chunks need the ability to expand by OODLELZ_BLOCK_HEADER_BYTES_MAX
		// Older Oodles (220) mistakenly wrote memcpy blocks using the LQH
		//	so they need room for each block to expand via the LQH not the block header
		//  see compresstest lzt:/lzt58 r:\lzt58.kraken.220.comp
		paddingPerSeekChunk = OODLELZ_BLOCK_HEADER_BYTES_MAX + OODLELZ_QUANTUM_HEADER_MAX_SIZE;
		
		// each newlz "chunk" needs 3 bytes for lz header during work
		//	(@@ like the newlz_arrays, this could be removed if the pre-check was removed)
		paddingPerSeekChunk += NEWLZ_CHUNK_HEADER_SIZE*2;
				
		// ensure there's enough for each newlz_array to expand (+5)
		// this is needed because newlz_arrays checks for room to expand
		//	  (put uncompressed) even before it tries compressing
		// if we only had comp_end checks as needed this wouldn't be necessary
		//	  because then we would only fail to compress when it was a real expansion case
		// alternative : remove this if the pre-compression room space checks are removed
		//	-> DONE pre-compression room space check now removed
		// 5 * 4 arrays
		// + 3 lz chunk header
		// + some header signalling bytes
		// just call it 32 ?
		//paddingPerSeekChunk += 32;
		
		// as noted above, this is not actually needed per seek chunk, but just once per call
		// but we have to do it per seek chunk because this might be used in the parallel encode
	}
	else
	{
		S32 numQuantaPerChunk = OODLELZ_BLOCK_LEN / OODLELZ_QUANTUM_LEN;	
		paddingPerSeekChunk = OODLELZ_BLOCK_HEADER_BYTES_MAX + numQuantaPerChunk * OODLELZ_QUANTUM_HEADER_MAX_SIZE;
		
		// 16 is the per-chunk overrun I can write during compression
		paddingPerSeekChunk += 16;

		// make sure it's a minimum of 256 per seek chunk :
		//	@@ why? was this for dcbz?
		RR_ASSERT( paddingPerSeekChunk >= 256 );

		// 12-16-2018 :
		// paddingPerSeekChunk	274	const int
		
		// this should be >= the newlz case
		//	because it's also used for "unknown/all" compressors
	}
	
	SINTa numSeekChunks = (rawSize + OODLELZ_BLOCK_LEN-1) / OODLELZ_BLOCK_LEN;

	SINTa ret = rawSize + numSeekChunks * paddingPerSeekChunk;

	return ret;
}



static const char * c_OodleLZ_Compressor_Names[] =
{
	"LZH","LZHLW","LZNIB","None","LZB16","LZBLW","LZA","LZNA","Kraken","Mermaid","BitKnit","Selkie","Hydra","Leviathan"
};
	
RR_COMPILER_ASSERT( RR_ARRAY_SIZE(c_OodleLZ_Compressor_Names) == OodleLZ_Compressor_Count );
	
OOFUNC1 const char * OOFUNC2 OodleLZ_Compressor_GetName(OodleLZ_Compressor compressor)
{
	if ( (int)compressor < 0 || (int)compressor >= (int)OodleLZ_Compressor_Count )
		return "invalid";
	
	return c_OodleLZ_Compressor_Names[compressor];
}


static const char * c_OodleLZ_Jobify_Names[] =
{
	"default","disable","normal","aggressive"
};
	
RR_COMPILER_ASSERT( RR_ARRAY_SIZE(c_OodleLZ_Jobify_Names) == OodleLZ_Jobify_Count );

OOFUNC1 const char * OOFUNC2 OodleLZ_Jobify_GetName(OodleLZ_Jobify jobify)
{
	if ( (int)jobify < 0 || (int)jobify >= (int)OodleLZ_Jobify_Count )
		return "invalid";
	
	return c_OodleLZ_Jobify_Names[jobify];
}
		                                

//----------------------------------------------
// OodleLZ_Compress							                               

#define VU8	U8_void

static SINTa OodleLZ_Compress_Sub(OodleLZ_Compressor compressor,
	const U8 * rawBuf,SINTa rawLen,
	U8 * compBuf,
	OodleLZ_CompressionLevel level,
	const OodleLZ_CompressOptions * pOptions,
	const U8 * dictionaryBase,
    const LRMCascade * lrm,
    rrArenaAllocator * arena)
{
	SINTa compLen;

	rrPrintf_v2("OodleLZ_Compress_Sub : " RR_SINTa_FMT " + " RR_SINTa_FMT "\n",
		rawLen, (rawBuf - dictionaryBase) );

	// HyperFast clamped to SuperFast :
	OodleLZ_CompressionLevel clampedLevel = (level < OodleLZ_CompressionLevel_None) ? OodleLZ_CompressionLevel_SuperFast : level;
	
	switch(compressor)
	{
	case OodleLZ_Compressor_Kraken:
		compLen = Kraken_Compress(compressor,(rawBuf),(compBuf),rawLen,level,pOptions,(dictionaryBase),lrm,arena);
		break;
	case OodleLZ_Compressor_Mermaid:
		compLen = Mermaid_Compress(compressor,(rawBuf),(compBuf),rawLen,level,pOptions,(dictionaryBase),lrm,arena);
		break;	
	case OodleLZ_Compressor_Selkie:
		compLen = Mermaid_Compress(compressor,(rawBuf),(compBuf),rawLen,level,pOptions,(dictionaryBase),lrm,arena);
		break;
	case OodleLZ_Compressor_Hydra:
		compLen = Hydra_Compress(compressor,(rawBuf),(compBuf),rawLen,clampedLevel,pOptions,(dictionaryBase),lrm,arena);
		break;
	case OodleLZ_Compressor_Leviathan:		
		compLen = Leviathan_Compress(compressor,(rawBuf),(compBuf),rawLen,clampedLevel,pOptions,(dictionaryBase),lrm,arena);
		break;
	case OodleLZ_Compressor_None:	
		compLen = OodleLZ_CompressMemcpy_Compressor(compressor,(rawBuf),rawLen,(compBuf),(dictionaryBase),pOptions);
		break;
	case OodleLZ_Compressor_LZB16:
		compLen = LZB_Compress((rawBuf),(compBuf),rawLen,clampedLevel,pOptions,(dictionaryBase),lrm);
		break;

	case OodleLZ_Compressor_LZH:
	case OodleLZ_Compressor_LZHLW:
	case OodleLZ_Compressor_LZNIB:
	case OodleLZ_Compressor_LZBLW:
	case OodleLZ_Compressor_LZA:
	case OodleLZ_Compressor_LZNA:
	case OodleLZ_Compressor_BitKnit:

		if ( ! g_OodleLZLegacyVTable.fp_legacy_OodleLZ_Compress_Sub )
		{
			ooLogError("Legacy LZ Vtable not installed");
			return -1;
		}
		compLen = g_OodleLZLegacyVTable.fp_legacy_OodleLZ_Compress_Sub(compressor,rawBuf,rawLen,compBuf,level,pOptions,dictionaryBase,lrm,arena);
		break;	
	default:
		ooLogError("invalid compressor : %d\n",(int)compressor);
		return -1;
	}

	return compLen;
}

#ifdef _MSC_VER // need __try/__except

// NOINLINE so that if we do get an exception in these,
//	it's easy to see why just from the func name in the call stack

static RADNOINLINE bool check_comp_buf_size(OodleLZ_Compressor compressor,void * compBuf,SINTa rawLen)
{
	__try
	{ 
		// check that compBuf is big enough :
		SINTa compBufSize = OodleLZ_GetCompressedBufferSizeNeeded(compressor,rawLen);
		U8 * compBuf8 = U8_void(compBuf);
		volatile U8 t;
		t = compBuf8[0];
		t = compBuf8[compBufSize-1];
	}
	__except(1) // EXCEPTION_EXECUTE_HANDLER)
	{
		return false;
	}

	return true;
}

static RADNOINLINE bool check_raw_buf_size(const void * buf,SINTa len)
{
	__try
	{
		const U8 * buf8 = U8_void(buf);
		volatile U8 t;
		t = buf8[0];
		t = buf8[len-1];
	}
	__except(1) // EXCEPTION_EXECUTE_HANDLER)
	{
		return false;
	}

	return true;	
}

#else

static bool check_comp_buf_size(OodleLZ_Compressor compressor,void * compBuf,SINTa rawLen)
{
	return true;
}

static bool check_raw_buf_size(const void * buf,SINTa len)
{
	return true;
}

#endif // _MSC_VER

OOFUNC1 SINTa OOFUNC2 OodleLZ_Compress(OodleLZ_Compressor compressor,
	const void * rawBufV,SINTa rawLen,
	void * compBufV, /* compBufEnd */
	OodleLZ_CompressionLevel level,
	const OodleLZ_CompressOptions * pOptions,
	const void * dictionaryBaseV,
    const void * lrmv RADDEFAULT(NULL),
    void * scratchMem RADDEFAULT(NULL),
    SINTa scratchSize RADDEFAULT(0) )
{
	OOFUNCSTART
	THREADPROFILEFUNC();
	
	OodleCore_Enter();

	// @@ STACK USE DEBUGGING TOGGLE HERE
	//log_stack_info();

	if ( ! g_Oodle_UsageWarningsDisabled )
	{
		if ( ! OodleLZ_Compressor_IsNewLZFamily(compressor) && compressor != OodleLZ_Compressor_None ) // None is not NewLZ, but also not deprecated
		{
			static bool s_once1 = true;
			if ( s_once1 )
			{	
				s_once1 = false;
				const char * name = OodleLZ_Compressor_GetName(compressor);
				ooLogUsageWarning2("Encoding with archaic compressor (%s) is deprecated and will be removed in a future version.\n  Use one of the modern Oodle codecs (Kraken, Mermaid, Leviathan, Selkie, Hydra).\n  Check OodleLZ_Compressor_IsNewLZFamily.\n  This message is only shown once per run.\n",name);
			}
		
			if( pOptions && pOptions->dictionarySize != 0 && ! OodleLZ_Compressor_RespectsDictionarySize(compressor) )
			{			
				static bool s_once2 = true;
				if ( s_once2 )
				{	
					s_once2 = false;
					const char * name = OodleLZ_Compressor_GetName(compressor);
					ooLogUsageWarning2("Requested dictionarySize limit but compressor (%s) does not support it.\n  Use one of the modern Oodle codecs (Kraken, Mermaid, Leviathan, Selkie, Hydra).\n  Check OodleLZ_Compressor_RespectsDictionarySize.\n  This message is only shown once per run.\n",name);
				}
			}
		}

		if ( pOptions && pOptions->jobify >= OodleLZ_Jobify_Normal && ! Oodle_IsJobSystemSet() )
		{
			static bool s_once3 = true;
			if ( s_once3 )
			{
				s_once3 = false;
				ooLogUsageWarning2("'jobify' is enabled, but no job system was installed!\n  Call OodleCore_Plugins_SetJobSystem to set up a job system first.\n  This message is only shown once per run.\n", 0);
			}
		}
		
		// @@@@ ! BAD API ! Fix OodleLZ_Compress to take a compBufEnd pointer (or compBufSize)
		//	currently assumes compBuf is at least OodleLZ_GetCompressedBufferSizeNeeded
		//	but doesn't have any way to verify if client obeyed that
		//	so you get random access violation in the compressor if too-small buffer is given
		if ( ! check_comp_buf_size(compressor,compBufV,rawLen) )
		{
			// not ooLogUsageWarning2
			ooLogError("comp buf size too small!\n");
			return OODLELZ_FAILED;
		}
		if ( ! check_raw_buf_size(rawBufV,rawLen) )
		{
			// not ooLogUsageWarning2
			ooLogError("raw buf mem invalid!\n");
			return OODLELZ_FAILED;
		}
	}
		
	struct OodleLZ_CompressOptions local_options_copy;
	pOptions = OodleLZ_CompressOptions_GetDefault_Or_Copy_And_Validate(pOptions,&local_options_copy);
    
	const U8 * rawBuf = VU8(rawBufV);
	U8 * compBuf = VU8(compBufV);
	const U8 * dictionaryBase = VU8(dictionaryBaseV);
	
    // early out, don't compress tiny buffers :		 
    // new bigger threshold for any compression - check spaceSpeedTradeoffBytes too
    // RR_LZH_MIN_RAW_LEN == 24 currently
	// spaceSpeedTradeoffBytes = 256 by default
	// @@ in the new paradigm where spaceSpeedTradeoffBytes is newlz lambda control
	//	 (not a literal number of bytes to save) this is a little bit dirty
    if ( rawLen <= RR_LZH_MIN_RAW_LEN ||
		 rawLen <= pOptions->spaceSpeedTradeoffBytes ||
		 level == OodleLZ_CompressionLevel_None ) // added 10-24-2017 - just check Level_None early
    {
		if ( rawLen <= 0 )
			return OODLELZ_FAILED;
			
		return OodleLZ_CompressMemcpy_Compressor(compressor,rawBuf,rawLen,compBuf,dictionaryBase,pOptions);
	}
	
	#if 0
	// CB 03-07-2021 :
	// stop doing this small buffers change compressor nonsense
	if ( pOptions->profile == OodleLZ_Profile_Main ) // Don't even consider fallback outside Main profile
	{
		// g_OodleLZ_Small_Buffer_LZ_Fallback_Size = 0 by default now
		if_unlikely ( rawLen < g_OodleLZ_Small_Buffer_LZ_Fallback_Size )
		{
			if ( OodleLZ_Compressor_IsEntropyCoded(compressor) ) // eg. not Selkie
			{
				if ( OodleLZ_Compressor_IsNewLZFamily(compressor) )
				{
					// NewLZ compressors are not great on tiny buffers
					//	lots of startup overhead time
					//	also the SSTB bail outs will usually just make them revert to uncompressed
					// the old simple streaming type compressors are better here

					// LZNIB is way better for small buffers
					//	but it does give you unexpected properties in kind of a nasty way
					//	(change quantum size, threadphasing, etc.)
					//	-> worst problem with LZNIB is that it's not fuzz safe
					// Selkie might be okay
					//compressor = OodleLZ_Compressor_Selkie;
					// @@@@ LZNIB is better for small buffers
					//		-> LZNIB is nice cuz of 12 bit offset option
					//		-> but needs to be fuzz safe
					//	use LZB16 for now because it's fuzz safe
					// @@@@ -> I fucking hate this, stop doing it
					//	it changes from a NewLZ family compressor to one that's not
					//	can cause very unexpected behavior if the LZ stream you make
					//	  isn't the compressor you thought it would be
					compressor = OodleLZ_Compressor_LZB16;
				}
			}
		}
	}
	else
	#endif
	
	if ( pOptions->profile == OodleLZ_Profile_Reduced )
	{
		if ( compressor != OodleLZ_Compressor_Kraken )
		{
			const char * name = OodleLZ_Compressor_GetName(compressor);
			ooLogError("Reduced profile only supports Kraken, failing compress.\n",name);
			
			return OODLELZ_FAILED;
		}
	}
			    
	SINTa halfG = ((SINTa)1<<29);
	SINTa oneG = ((SINTa)1<<30);
	
	// no dic backup, set it to raw base :
	if_likely ( dictionaryBase == NULL )
	{
		dictionaryBase = rawBuf;
	}
	else
	{
		SINTa posInDic = rrPtrDiff( rawBuf - dictionaryBase );
	
		// if decodeStartOffset is not a multiple of OODLELZ_BLOCK_LEN it's an almost gauranteed failure (in the decoder)
		if ( (posInDic & (OODLELZ_BLOCK_LEN-1)) != 0 )
		{
			ooLogError("dictionary offset must be a multiple of OODLELZ_BLOCK_LEN\n");
			
			return OODLELZ_FAILED;
		}
    
		// is seekChunkReset offset relative to dictionary start, or rawBuf ?
		//	possible ambiguity = nasty
		//	-> it's to *dictionary* start
		
		if ( pOptions->seekChunkReset )
		{
			// step ahead by any integer number of seek chunks :
			SINTa posInDic_seekChunks = (posInDic / pOptions->seekChunkLen) * pOptions->seekChunkLen;
			dictionaryBase += posInDic_seekChunks;
			// dic backup now is strictly < seekChunkLen
			// if dic backup was aligned to seekchunklen, then we step past all of it here
		}
		else if ( posInDic > halfG )
		{
			// NOT seek chunking, large backup, limit to halfG
			rrPrintf_v2("OodleLZ_Compress limited dicBackup to halfG\n");
			dictionaryBase = rawBuf - halfG;
		}		
	}
	
	SIMPLEPROFILE_SCOPE_N(OodleLZ_Compress,rawLen);
	
	// if no scratch mem given, alloc a chunk now :
	//	(if you don't do this, it will call to malloc for everything needed, which is okay too)
	//	this is not so much for optimization as because stupid clients hate to see lots of malloc calls
	void * arena_alloc = NULL;
	if ( scratchMem == NULL || scratchSize <= 0 )
	{
		// this does not cover the big hash table allocs for CTMF
		// I want to cover all the little crap ones
		// Mermaid uses ~750k , Kraken ~350k 
		// @@@@ just use OodleLZ_GetCompressScratchMemBound ? (that does cover CTMF too)
		SINTa scratch_bound = OodleLZ_GetCompressScratchMemBound(compressor,level,rawLen,pOptions);
		SINTa arena_allocSizeIfNoneGiven = scratch_bound;
		// if we can't bound (old codec or optimal level)
		//	go aheand alloc some scratch anyway
		//	(this is not used by the old codecs!)
		if ( scratch_bound == OODLELZ_SCRATCH_MEM_NO_BOUND )
			arena_allocSizeIfNoneGiven = RR_MIN( rawLen*8 + 16384 , (1<<20) );
		rrPrintf_v2("arena_allocSizeIfNoneGiven : %d\n",(int)arena_allocSizeIfNoneGiven);
		arena_alloc = OodleMalloc(arena_allocSizeIfNoneGiven);
		scratchMem  = arena_alloc;
		scratchSize = arena_allocSizeIfNoneGiven;
		// this is totally wasted for the old compressors, but whatevs
	}
	
	RR_ASSERT( scratchSize > 0 );
	// stuff a byte at the end of scratch to check overrun :
	scratchSize--;
	U8 * scratchLastPtr = U8_void(scratchMem) + scratchSize;
	const U8 scratchLastStuffByte = 0x17;
	*scratchLastPtr = scratchLastStuffByte;
	
	rrArenaAllocator arena(scratchMem,scratchSize,true);
	const LRMCascade * lrmc = (const LRMCascade *)lrmv;
			
	SINTa totCompLen;
	
	if ( pOptions->seekChunkReset )
	{
		// new 10-27-2018 Oodle 2.7.5 :
		//	just do seek chunk splitting in the outer caller
		//	to make sure I don't screw it up
		// this adds a little overhead having to go through OodleLZ_Compress_Sub each time
		//	but worth it for the sanity
	
		//CRUCIAL :
		//seek chunks are relative to *dictionaryBase* not to *rawBuf*

		// split the calls
		const U8 * rawPtr = rawBuf;
		const U8 * rawEnd = rawBuf+rawLen;
		U8 * compPtr = compBuf;
		
		rrPrintf_v2("OodleLZ_Compress splitting for seekChunkReset : rawLen=" RR_SINTa_FMT ", seekChunkLen = %d, posInDic=" RR_SINTa_FMT "\n",
			rawLen, pOptions->seekChunkLen, rrPtrDiff(rawBuf - dictionaryBase) );
				
		// first chunk may have dic backup :
		if ( dictionaryBase != rawBuf )
		{
			// seek chunk starts at dictionaryBase:
			SINTa posInDic = rawPtr - dictionaryBase;
			RR_ASSERT( posInDic < pOptions->seekChunkLen );
			
			// remainder of the seekchunklen :
			SINTa curLen = pOptions->seekChunkLen - posInDic;
			curLen = RR_MIN(curLen,rawLen);
			
			RR_ASSERT( curLen > 0 );
			
			SINTa compLen = OodleLZ_Compress_Sub(compressor,rawPtr,curLen,compPtr,level,pOptions,dictionaryBase,NULL,&arena);
			if ( compLen <= 0 )
			{
				if ( arena_alloc ) OodleFree(arena_alloc);
				return OODLELZ_FAILED;
			}
			compPtr += compLen;
			rawPtr += curLen;
		}
		
		// later chunks are just seekchunklens with no backup :
		while ( rawPtr < rawEnd )
		{
			SINTa rawLeft = rawEnd - rawPtr;
			SINTa curLen = RR_MIN( rawLeft, pOptions->seekChunkLen );
												
			SINTa compLen = OodleLZ_Compress_Sub(compressor,rawPtr,curLen,compPtr,level,pOptions,rawPtr,NULL,&arena);
			if ( compLen <= 0 )
			{
				if ( arena_alloc ) OodleFree(arena_alloc);
				return OODLELZ_FAILED;
			}
			compPtr += compLen;
			rawPtr += curLen;
		}
		
		totCompLen = compPtr - compBuf;
	}
	else
	{
		// not seek chunk reset
	
		if_unlikely ( rawLen >= oneG ||
		 rrPtrDiff(rawBuf+rawLen - dictionaryBase) >= (oneG+halfG) )
		{
			// very large buf, split it
			//	so internal compressors only see S32-safe sizes
			rrPrintf_v2("OodleLZ_Compress splitting : rawLen=" RR_SINTa_FMT ", posInDic=" RR_SINTa_FMT "\n",
				rawLen, rrPtrDiff(rawBuf - dictionaryBase) );
		
			// this splitter is kind of fucked with seek resets
			//	what is the half G splitting doesn't match the seek chunk split points?
						
			// split the calls
			const U8 * rawPtr = rawBuf;
			const U8 * rawEnd = rawBuf+rawLen;
			U8 * compPtr = compBuf;
			while ( rawPtr < rawEnd )
			{
				SINTa rawLeft = rawEnd - rawPtr;
				SINTa curLen;
				if ( rawLeft < (600<<20) ) // avoid small final shitlet
					curLen = rawLeft;
				else
					curLen = RR_MIN( halfG, rawLeft );
					
				SINTa posInDic = rawPtr - dictionaryBase;
				SINTa dicBackup = RR_MIN( halfG, posInDic );
								
				rrPrintf_v2("OodleLZ_Compress split : posInDic= " RR_SINTa_FMT " curLen=" RR_SINTa_FMT " dicBackup=" RR_SINTa_FMT "\n",
					posInDic, curLen, dicBackup );
						
				SINTa compLen = OodleLZ_Compress_Sub(compressor,rawPtr,curLen,compPtr,level,pOptions,rawPtr - dicBackup,lrmc,&arena);
				if ( compLen <= 0 )
				{
					if ( arena_alloc ) OodleFree(arena_alloc);
					return OODLELZ_FAILED;
				}
				compPtr += compLen;
				rawPtr += curLen;
			}
			
			totCompLen = compPtr - compBuf;
		}
		else
		{
			// -> normal path is here
		
			totCompLen =  OodleLZ_Compress_Sub(compressor,rawBuf,rawLen,compBuf,level,pOptions,dictionaryBase,lrmc,&arena);
		}
	}
	
	RR_ASSERT( *scratchLastPtr == scratchLastStuffByte );
	
	#ifdef _DEBUG
	// log arena high water mark of scratch mem used :
	// Log Verbose level 2 compiles out in cdep release build
	//if ( scratchMem != NULL ) // && rrLogGetVerboseLevel() >= 2 )
	{
		RR_ASSERT( arena.m_cur == arena.m_base );
		
		SINTa arena_max_allocated = arena.m_cur_max - arena.m_base;
		SINTa room = arena.m_size - arena_max_allocated;
		static SINTa arena_max_allocated_max = 0;
		arena_max_allocated_max = RR_MAX(arena_max_allocated_max,arena_max_allocated);
		static SINTa arena_room_min = 1<<29;
		arena_room_min = RR_MIN(arena_room_min,room);
		
		// for OodleLZ_GetCompressScratchMemBound checking :
		rrPrintf_v2
			("OodleLZ_Compress arena_max_allocated : %d KB, room=%d KB (max %d KB min room %d KB)\n",
			(int)(arena_max_allocated+1023)/1024,
			(int)room/1024, // round down room
			(int)(arena_max_allocated_max+1023)/1024,
			(int)arena_room_min/1024 // round down room
			);
	}
	#endif
	
	if ( arena_alloc ) OodleFree(arena_alloc);
	
	if ( totCompLen < 0 ) return totCompLen;

	// check GetCompressedStepForRawStep :
	RR_DURING_ASSERT( SINTa compStep = OodleLZ_GetCompressedStepForRawStep(compBuf,totCompLen,0,rawLen) );
	RR_ASSERT( compStep == totCompLen );
	
	return totCompLen;
}

OOFUNC1 SINTa OOFUNC2 OodleLZ_GetCompressScratchMemBound(
	OodleLZ_Compressor compressor,
	OodleLZ_CompressionLevel level,
	SINTa rawLen,
	const OodleLZ_CompressOptions * pOptions RADDEFAULT(NULL)
	)
{
	// first exclude stuff we just don't handle yet
	// newlz's only , non-Optimals only
	if ( ! OodleLZ_Compressor_IsNewLZFamily(compressor) )
	{
		return OODLELZ_SCRATCH_MEM_NO_BOUND;
	}
	if ( level >= OodleLZ_CompressionLevel_Optimal1 )
	{
		return OODLELZ_SCRATCH_MEM_NO_BOUND;
	}
	
	struct OodleLZ_CompressOptions local_options_copy;
	pOptions = OodleLZ_CompressOptions_GetDefault_Or_Copy_And_Validate(pOptions,&local_options_copy);
    
    // limit rawLen to seekChunkLen :
    if ( pOptions->seekChunkReset )
		rawLen = RR_MIN(rawLen,pOptions->seekChunkLen);
    
    // unbounded/unknown rawLen :
    if ( rawLen <= 0 )
		rawLen = OODLELZ_SEEKCHUNKLEN_MAX;
    
    // we should not allocate anything, give it a dummy arena :
    //	(false means can't fall back to system malloc either)
    U8 scratchMem[1];
	rrArenaAllocator arena(scratchMem,sizeof(scratchMem),false);
	
	// now just call Compress as if we were really doing it
	//	pass NULL for raw array to indicate we want mem bound
	// this avoid duping all the setup code for all the compressors
	
    SINTa bound = OodleLZ_Compress_Sub(compressor,NULL,rawLen,NULL,level,pOptions,NULL,NULL,&arena);

	return bound;
}	

OOFUNC1 S32 OOFUNC2 OodleLZDecoder_MakeValidCircularWindowSize(OodleLZ_Compressor compressor,S32 minWindowSize)
{
	OOFUNCSTART

	static bool s_once1 = true;
	if ( s_once1 )
	{
		s_once1 = false;
		ooLogUsageWarning2("Circular windows are deprecated and will be removed in a future version of Oodle.\n",0);
	}
	
	if ( compressor == OodleLZ_Compressor_LZB16 )
	{
		S32 comp_window_size = LZB_SLIDING_WINDOW_SIZE;
	
		if ( minWindowSize < (S32)comp_window_size )
			return comp_window_size;
	    
		// half of desired window size should be aligned on pages
		S32 windowSize = 2 * rrAlignUp(minWindowSize/2,4096);
	    
		return windowSize;		
	}
	else if ( OodleLZ_Compressor_IsLegacy(compressor) )
	{
		if ( ! g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_MakeValidCircularWindowSize )
		{
			ooLogError("Legacy LZ Vtable not installed");
			return -1;
		}
		return g_OodleLZLegacyVTable.fp_legacy_OodleLZDecoder_MakeValidCircularWindowSize(compressor,minWindowSize);
	}
	else
	{
		ooLogError("only legacy codecs support circular window\n");
		return -1;
	}
}

/**

GetLZMatchTableBits

if user provides a size in Options (matchTableSizeLog2)
	then just use their size
	do NOT clamp to the lo/hi ranges passed in
	but DO use the log2 of the file size as a maximum
	so the table scales to file size

if user does not provide a size
	then I make a reasonable one
	using the log2 and the provided clamp ranges

**/
S32 GetLZMatchTableBits( SINTa rawLen, 
	OodleLZ_CompressionLevel level,
	const OodleLZ_CompressOptions * pOptions,
	S32 vf_lo, S32 vf_hi,
	S32 lo, S32 hi )
{
	//if ( level == OodleLZ_CompressionLevel_SuperFast ) return OODLELZ_SUPERFAST_HASH_BITS;

	S32 rawLen32 = S32_clampA(rawLen);
	S32 sizeForTable = rawLen32;
	if ( pOptions->seekChunkReset && pOptions->seekChunkLen < rawLen32 )
		sizeForTable = pOptions->seekChunkLen;
		
	//S32 log2 = rrIlog2ceil(sizeForTable);
	S32 rawLenLog2 = rrIlog2round(sizeForTable);
	
	S32 tableBits;
	if ( level <= OodleLZ_CompressionLevel_VeryFast )
	{
		// SuperFast, VeryFast
		tableBits = RR_CLAMP((rawLenLog2-1),vf_lo,vf_hi);
	}
	else
	{				
		// Fast, Normal
		tableBits = RR_CLAMP(rawLenLog2,lo,hi);
	}
		
	if ( pOptions->matchTableSizeLog2 > 0 ) // non-default has been provided
	{
		// matchTableSizeLog2 was provided
		// @@ this fucking around here is a bit weird and annoying
		//	maybe just pass matchTableSizeLog2 through untouched ?
		//	(I don't think anyone actually uses this)
		//	(I just use it for perf testing to see the effect of hash size on speed)
		
		if ( pOptions->matchTableSizeLog2 > 100 )
		{
			// hacky > 100 mode to allow unmodifed setting of hash size
			//	eg "112" = use 12
			tableBits = pOptions->matchTableSizeLog2 - 100;
		
			// no clamp, no nada?
			tableBits = RR_CLAMP(tableBits,8,28);
		}
		else
		{	
			// matchTableSizeLog2 can only take us *down* from default, not up :
			tableBits = RR_MIN(tableBits,pOptions->matchTableSizeLog2);

			// ensure a reasonable range :
			tableBits = RR_CLAMP(tableBits,12,26);
		}		
	}
	
	/*
	rrPrintf_v2("GetLZMatchTableBits : %d for %d (%d), mts=%d, lvl=%d\n",
		tableBits,
		rawLen32,log2,
		pOptions->matchTableSizeLog2,
		(int)level);		
	*/
	
	// note "tableBits" is typically used to make a # of U32's
	//	actual memory use is 4X higher
	// 4 MB L3 = 1 M dwords = 20 bit tableBits max
	
	#if OODLE_PLATFORM_SMALLMEMORY
	// on old consoles & phones and such - limit to 22 bits :
	//	= 4 M entries = 16 M allocation
	tableBits = RR_MIN(tableBits,22);
	#endif
	
	return tableBits;
}	
	

//=====================================================================================

//* // toggle here to make all platforms the same
// the limitation is memory use and speed
#if OODLE_PLATFORM_LARGEMEMORY
#ifdef __RAD64__
	#include "suffixtrie.h"

	// stays in SuffixTrie1 :	
    //#define DEFAULT_MAXLOCALDICTIONARY (SuffixTrie1_MaxSize) //(8<<20)
    // uses SuffixTrie2 :
    #define DEFAULT_MAXLOCALDICTIONARY (SuffixTrie1_MaxSize*2) // (1<<24)

#else
    #define DEFAULT_MAXLOCALDICTIONARY (4<<20)
#endif
#else // OODLE_PLATFORM_LARGEMEMORY
	// consoles
    #define DEFAULT_MAXLOCALDICTIONARY (2<<20) 
#endif

#define DEFAULT_MINMATCHLEN	0

// 04-08-2018 , Oodle 2.8.0 :
//	no more different defaultOptions per level
//	just set everything to "default" / zero
	
static OodleLZ_CompressOptions s_defaultOptions =
{
	0, //unused_was_verbosity,
	DEFAULT_MINMATCHLEN, // minMatchLen
	false, // seekChunkReset
	OODLELZ_BLOCK_LEN,	// seekChunkLen
	OodleLZ_Profile_Main, // profile
	0, // dictionarySize
	OODLELZ_DEFAULT_SSTB, // spaceSpeedTradeoffBytes;
	0,   // deprecated_maxHuffmansPerChunk;
	false, // sendQuantumCRCs
	DEFAULT_MAXLOCALDICTIONARY, // maxLocalDictionarySize
	true, // makeLongRangeMatcher @@ DEFAULT OF 0 doesn't work here
	0, // matchTableSizeLog2
	OodleLZ_Jobify_Default, // jobify
	NULL, // jobifyUserPtr
	0, //farMatchMinLen
	0, //farMatchOffsetLog2
	
	// reserved
	// more zeros
};

// OodleLZ_CompressOptions_GetDefault - get options for compress LEvel
OOFUNC1 const OodleLZ_CompressOptions * OOFUNC2 OodleLZ_CompressOptions_GetDefault(
													OodleLZ_Compressor compressor,
													OodleLZ_CompressionLevel lzLevel RADDEFAULT(OodleLZ_CompressionLevel_Normal))
{
	OOFUNCSTART

	// compressor & level no longer used :
	compressor;
	lzLevel;

	return &s_defaultOptions;
}

const OodleLZ_CompressOptions * OodleLZ_CompressOptions_GetDefault_Or_Copy_And_Validate(
	const OodleLZ_CompressOptions * pOptions,
	OodleLZ_CompressOptions * p_scratch_options)
{
	if ( pOptions == NULL )
	{
		return OodleLZ_CompressOptions_GetDefault();
	}
	else
	{
		// call _Validate on passed in options (copy and work in scratch so source isn't changed) :
		*p_scratch_options = *pOptions;
		OodleLZ_CompressOptions_Validate(p_scratch_options);
		return p_scratch_options;
	}
}

//=====================

OOFUNC1 void OOFUNC2 OodleLZ_CompressOptions_Validate(OodleLZ_CompressOptions * pOptions)
{
	OOFUNCSTART
	
	// the idea here is that the user can set OodleLZ_CompressOptions = { } (all zero)
	//	and that gives good defaults
	// so for all values 0 should mean "use default"
	// in _Validate() we change 0 to default
	//	(for fields where the compressors don't do it themselves)
	
	// @@ 
	//  One field currently not working for that : makeLongRangeMatcher
	//	-> we want default = true
	// the way to do it properly is a Trinary bool (like jobify)
	//	 0 = not set, use default
	//	 1 = set, OFF
	//	 2 = set, ON
	// but makeLongRangeMatcher is a legacy field so we can't change the meaning of 0

	pOptions->unused_was_verbosity = 0;
	pOptions->unused_was_maxHuffmansPerChunk = 0;
	
	// changed 4-08-2018 for Oodle 2.8.0 :
	// spaceSpeedTradeoffBytes ZERO now means default
	//   to get zero (max compress) pass negative
	// this is done by newlz at deref time in newlz_spaceSpeedTradeoffBytes_for_lambda
	if ( pOptions->spaceSpeedTradeoffBytes == 0 )
	{
		pOptions->spaceSpeedTradeoffBytes = OODLELZ_DEFAULT_SSTB;
	}
	/*
	// negatives let you get to zero
	// NO don't do this here
	//	must pass negatives through
	// if you do this here then multiple Validates change -1 -> 0 -> 256
	// you can easily get multiple Validates by accident
	//	say if the user does it, then we do it again
	//  so multiple Validates should always be a nop
	else if ( pOptions->spaceSpeedTradeoffBytes < 0 )
	{
		pOptions->spaceSpeedTradeoffBytes = 0;
	}
	*/
	else
	{
		//pOptions->spaceSpeedTradeoffBytes = RR_CLAMP(pOptions->spaceSpeedTradeoffBytes,0,4096);
		// pass negatives through :
		//pOptions->spaceSpeedTradeoffBytes = RR_MIN(pOptions->spaceSpeedTradeoffBytes,4096);
		// pass negatives through with a clamp to -1 :
		pOptions->spaceSpeedTradeoffBytes = RR_CLAMP(pOptions->spaceSpeedTradeoffBytes,-1,4096);
	}
	
	// seekChunkLen must be a multiple of OODLELZ_BLOCK_LEN :
	// actually STRICTER it must be power of 2 !
	//pOptions->seekChunkLen = rrAlignUp( pOptions->seekChunkLen, OODLELZ_BLOCK_LEN );
	if ( pOptions->seekChunkLen == 0 )
	{
		pOptions->seekChunkLen = OODLELZ_BLOCK_LEN;
	}
	else
	{
		pOptions->seekChunkLen = RR_CLAMP(pOptions->seekChunkLen, OODLELZ_SEEKCHUNKLEN_MIN, OODLELZ_SEEKCHUNKLEN_MAX );
		pOptions->seekChunkLen = rrNextPow2( pOptions->seekChunkLen );
	}

	// validate maxLocalDictionarySize :
	if ( pOptions->maxLocalDictionarySize <= 0 )
	{
		pOptions->maxLocalDictionarySize = DEFAULT_MAXLOCALDICTIONARY;
	}
	else if ( pOptions->maxLocalDictionarySize != DEFAULT_MAXLOCALDICTIONARY ) // skip work for common case
	{
		// maxLocalDictionarySize must be power of 2 :
		pOptions->maxLocalDictionarySize = rrPrevPow2(pOptions->maxLocalDictionarySize);
		// must be at least 2 * OODLELZ_BLOCK_LEN :
		//	because half of maxLocalDictionarySize is the LRM chunk size and LRM chunk must be >= OODLELZ_BLOCK_LEN
		//	also applies to Optimal parse chunking so yes enforce >= 2*BLOCK strictly
		
		// err.. making lots of small suffix tries is probably not ideal for perf
		//	make it a little bigger
		// @@ 2* is the actual requirement
		//	forcing up to 4* is for perf?
		//#define MIN_maxLocalDictionarySize	(2 * OODLELZ_BLOCK_LEN)
		#define MIN_maxLocalDictionarySize	(4 * OODLELZ_BLOCK_LEN) // 1 MB

		if ( pOptions->maxLocalDictionarySize <= MIN_maxLocalDictionarySize )
		{
			pOptions->maxLocalDictionarySize = MIN_maxLocalDictionarySize;
			
			// 08-04-2020 : was turning off LRM in this case, but that's wrong
			//	you can have a tiny LD and then LRM if you want
			//pOptions->makeLongRangeMatcher = false;
		}
		else
		{
			// must be a multiple of 2*OODLELZ_BLOCK_LEN
			//	(happens automatically because it's power of 2)
			RR_ASSERT( (pOptions->maxLocalDictionarySize % (2*OODLELZ_BLOCK_LEN)) == 0 );
			
			// not over 2^29
			pOptions->maxLocalDictionarySize = RR_MIN(pOptions->maxLocalDictionarySize,OODLELZ_MAX_DIC_BACKUP);
		}
	}
	
	// if maxOffset fits in maxLocalDictionarySize , don't make long range matcher
	if ( pOptions->dictionarySize > 0 )
	{
		pOptions->dictionarySize = RR_MIN(pOptions->dictionarySize,OODLELZ_MAX_OFFSET);

		if ( pOptions->dictionarySize <= pOptions->maxLocalDictionarySize )
		{
			// -> only relevant to Optimals

			#if 1
			// can optionally reduce maxLocalDictionarySize :
			// -> this makes you do lots of small suffix tries, instead of fewer big ones
			// -> I guess that's better? definitely lower memory use
			// -> saves a lot of work finding matches that wind up being ruled out
			// -> but does do more work on all the overlaps, refilling the ST
			// -> basically ST is not great for sliding window
			// maxLocalDictionarySize must be dicSize*2 , because maxLocalDictionarySize covers the backup + current chunk
			// -> @@@@ NOT SURE IF GOOD
			S32 smaller_maxLocalDictionarySize = 2 * rrNextPow2(pOptions->dictionarySize);
			// only if this makes maxLocalDictionarySize go down :
			pOptions->maxLocalDictionarySize = RR_MIN(smaller_maxLocalDictionarySize,pOptions->maxLocalDictionarySize);

			pOptions->maxLocalDictionarySize = RR_CLAMP(pOptions->maxLocalDictionarySize,MIN_maxLocalDictionarySize,OODLELZ_MAX_DIC_BACKUP);
			#endif

			// offset limit is inside maxLocalDictionarySize
			//	so no point in any LRM :

			pOptions->makeLongRangeMatcher = false;
		}
	}
	
	// minMatchLen <= 0 means "use default"
	// the old compressors expect something >= 2 here
	// I think it's all fixed
	// but WTF just protect it here
	pOptions->minMatchLen = RR_MAX(pOptions->minMatchLen,2);

	pOptions->jobify = (OodleLZ_Jobify) RR_CLAMP( (int)pOptions->jobify, 0, OodleLZ_Jobify_Count-1 );

	// clamp farMatchMinLen ?

	pOptions->farMatchOffsetLog2 = RR_MIN(pOptions->farMatchOffsetLog2,29);

	// zero all reserved fields :
	RR_ZERO(pOptions->reserved);

}

//=====================

OOFUNC1 SINTa OOFUNC2 OodleKraken_Decode_Headerless(
	U8 * decomp, SINTa decomp_size,
	const U8 * comp, SINTa comp_size,
	UINTa bytes_since_reset, rrbool isMemcpy, rrbool lzEnable, rrbool subLiteralEnable,
	void * decoderMemory, SINTa decoderMemorySize
	)
{
	OOFUNCSTART
	
	// This is a public entry point so it needs to do this
	OodleCore_Enter();

	SINTa scratch_size = OodleLZ_Compressor_ScratchMemSize(OodleLZ_Compressor_Kraken,decomp_size);
	U8 * scratch_mem = (U8 *)decoderMemory;

	if ( decoderMemory && decoderMemorySize < scratch_size )
	{
		ooLogError("OodleKraken_Decode_Headerless : decoder memory given, but too small!\n");
		return OODLELZ_FAILED;
	}

	if ( ! decoderMemory )
	{
		scratch_mem = (U8 *)OodleMalloc( scratch_size );
		if ( ! scratch_mem )
		{
			ooLogError("OodleKraken_Decode_Headerless : decoder memory allocation failed!\n");
			return OODLELZ_FAILED;
		}
	}

	SINTa ret = Kraken_Decode_Headerless(decomp, decomp_size, comp, comp_size, bytes_since_reset, isMemcpy, lzEnable, subLiteralEnable, scratch_mem, scratch_size);

	if ( ! decoderMemory )
		OodleFree(scratch_mem);

	return ret;
}

OODLE_NS_END
