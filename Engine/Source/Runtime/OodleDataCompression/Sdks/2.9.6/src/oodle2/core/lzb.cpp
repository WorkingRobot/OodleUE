// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "threadprofiler.h"
#include "rrbytepacking.h"
#include "templates/rrvector.h"
#include "rrlogutil.h"
#include "cbradutil.h"
#include "rrsimpleprofstub.h"
#include "rrlz_getmatchlen.inl"
#include "rrlzh_lzhlw_shared.h"
#include "lzb.h"
#include "rrfuzztesthelper.h"

OODLE_NS_START

static RADINLINE U32 hmf_hash4_32(U32 ptr32)
{
    U32 h = ( ptr32 * 2654435761u );
    h ^= (h>>13);
    return h;
}

#ifdef _MSC_VER
#pragma warning(disable : 4456) // declaration hides previous local declaration
#endif

/***************************

02/27/2015 :

"Normal" is a cache table (Normal_New using _fast_normal.inl)
	(can't use _fast.inl because it picks the first match found, not longest)
	
"Optimal1" is hash-link (Normal_Old)

"Optimal2" is true optimal (SuffixArray + backwards parse)

04/27/2017 : >= Optimal is gone

*****************************/

#define LZB_LRL_BITS	4
#define LZB_LRL_ESCAPE	15

#define LZB_ML_BITS		4
#define LZB_MLCONTROL_ESCAPE	15

//#define LZB_MAX_OFFSET		(LZB_SLIDING_WINDOW_SIZE-1)
// can't make the last few offsets
//	because of the 8-byte blasting decoder in the circular window case
#define LZB_MAX_OFFSET		(LZB_SLIDING_WINDOW_SIZE-1 - 8)
// @@ is LZB_SLIDING_WINDOW_SIZE-8 okay ?

/**

One of the little ways that "lzb_simple" is faster to encode than LZB16 (in Oodle)
is because it allows the full range of U16 offsets, while LZB16 has a
limit of (0xFFFF-8) = 65527

Because of this, lzb_simple can use a simpler form of offset check
(it just puts offset in a U16, and then checks offset == 0 after the 4-byte equality check)

LZB16 has to check offset < MAX_OFFSET

the reason why LZB16 has the lower max offset is because it might be decoded in a sliding window
and the decoder wants to be able to do blind 8-byte splat copies
When the decoder does that it can be stomping on the very begining of the window (high offsets)
so matches in that domain are not allowed

**/

#define LZB_MML		4		// should be 3 if I had LO

#define LZB_MATCHLEN_ESCAPE		(LZB_MLCONTROL_ESCAPE+4)

//===============================================

#define LZB_END_WITH_LITERALS			1
#define LZB_END_OF_BLOCK_NO_MATCH_ZONE	8

/**

NOTE ABOUT LZB_END_OF_BLOCK_NO_MATCH_ZONE

The limitation in LZB does not actually come from the 8-at-a-time match copier

it comes from the unconditional 8-byte LRL copy

that means the last 8 bytes of every block *must* be literals

(note that's *block* not quantum)

The constraint due to matches is actually weaker
(match len rounded up to next multiple of 8 must not go past block end)

**/

// disable assert that trips on fuzz :
#define LZB_ASSERT	RR_ASSERT
//#define LZB_ASSERT(x)

#define LZB_FORCELASTLRL9	1
//#define LZB_FORCELASTLRL9	0

//=================================================

#define LZB_CHUNK_LEN	OODLELZ_QUANTUM_LEN

#define DO_HEAVY_ASSERT	0

//#define RR_ASSERT_LZB		RR_ASSERT_ALWAYS
#define RR_ASSERT_LZB		RR_ASSERT

#if DO_HEAVY_ASSERT
//#define HEAVY_ASSERT	RR_ASSERT
#define HEAVY_ASSERT	RR_ASSERT_ALWAYS
#else
#define HEAVY_ASSERT(exp)	// nada
#endif

#define ASSERT RR_ASSERT
#define ASSERT_RELEASE RR_ASSERT_ALWAYS
#define MIN RR_MIN

#define DO_CHECK	0

#if DO_CHECK
#define CHECK(exp)	do { exp; } while(0)
#else
#define CHECK(exp)
#endif

#define	LZB_getmatchlen	getmatchlen_mml4

typedef
SINTa (fp_Encode_OneChunk)(const U8 * raw, SINTa rawLen, U8 * comp, void * matcher, const U8 * dictionaryBase, const U8 * blockMatchLimitPtr, const U8 * bufferEndPtr, LZQuantumHeader * pQH);
		
static SINTa Encode_LZB_None(const U8 * raw, SINTa rawLen, U8 * comp, void * searchers , const U8 * dictionaryBase,  OodleLZ_CompressionLevel level, const OodleLZ_CompressOptions * pOptions);
static SINTa Encode_LZB_Fast(const U8 * raw, SINTa rawLen, U8 * comp, void * searchers , const U8 * dictionaryBase,  OodleLZ_CompressionLevel level, const OodleLZ_CompressOptions * pOptions);
static SINTa Encode_LZB_VeryFast(const U8 * raw, SINTa rawLen, U8 * comp, void * searchers , const U8 * dictionaryBase,  OodleLZ_CompressionLevel level, const OodleLZ_CompressOptions * pOptions);

static SINTa Encode_LZB_RLE(const U8 * rawBuf, SINTa rawLen, U8 * compBuf, const U8 * dictionaryBase, OodleLZ_CompressionLevel level, const OodleLZ_CompressOptions * pOptions);

static SINTa Encode_LZB_Normal_New(const U8 * raw, SINTa rawLen, U8 * comp, void * searchers,  const U8 * dictionaryBase, OodleLZ_CompressionLevel level, const OodleLZ_CompressOptions * pOptions);

static OodleLZB_CompressFast_Context * Encode_LZB_Fast_Alloc(OodleLZ_CompressionLevel level,const U8 * raw, SINTa rawSize, SINTa dicBackup, int tableBits);

static void Encode_LZB_OneChunk_Normal_New_Preload(
		SINTa * hashTable,
		int tableBits,
		const U8 * rawBufStart,
		SINTa dictionaryBackup );
		
static void Encode_LZB_OneChunk_Fast_NoSlide_Preload(
		SINTa * hashTable,
		int tableBits,
		const U8 * rawBufStart,
		SINTa dictionaryBackup );
		
static void Encode_LZB_OneChunk_VeryFast_NoSlide_Preload(
		SINTa * hashTable,
		int tableBits,
		const U8 * rawBufStart,
		SINTa dictionaryBackup );
		
//========================================================

// @@ these needs work :
//	no overlap and no overrun
#define LZB_memcpy(to,from,len)	lzhd_memmov(to,from,(size_t)(len))

//========================================================

/*
//decode only      : 0.014 seconds, 98.60 b/hc, rate= 1704.89 mb/s
//lzt99 : 24,700,820 ->14,757,966 =  4.780 bpb =  1.674 to 1

#define LZB_PutExcess255(cp,val)	do { while( val >= 0xFF ) { *cp++ = 0xFF; val -= 0xFF; } *cp++ = (U8) val; } while(0)

#define LZB_AddExcess255(cp,val)	do { U32 b = *cp++; val += b; if ( b != 0xFF ) break; } while(1)

#define LZB_PutExcessLRL(cp,val) LZB_PutExcess255(cp,val)
#define LZB_PutExcessML(cp,val)  LZB_PutExcess255(cp,val)

#define LZB_AddExcessLRL(cp,val) LZB_AddExcess255(cp,val)
#define LZB_AddExcessML(cp,val)  LZB_AddExcess255(cp,val)

/*/
// use the fact that due to quanta
// len can be at most 16k
// so just do byte-then-word

// very slightly more compression
// undetectable speed difference

//decode only      : 0.014 seconds, 99.81 b/hc, rate= 1725.77 mb/s
//lzt99 : 24,700,820 ->14,753,128 =  4.778 bpb =  1.674 to 1

#define LZB_PutExcessBW(cp,val)	do { RR_ASSERT( val < 16384 ); \
	if ( val < 192 ) *cp++ = (U8) val; \
	else { *cp++ = 192 + (U8) ( val>>8); *cp++ = (U8) val; } \
	} while(0)

#define LZB_AddExcessBW(cp,val)	do { U32 b = *cp++; \
	if ( b < 192 ) val += b; \
	else { val += (b-192)<<8; val += *cp++; } } while(0)

//#define LZB_AddExcessBW(cp,val)	do { U32 b = *cp++; val += b; \
//	if ( b >= 192 ) { val -= b; val += ((b-192)<<8); val += *cp++; } } while(0)

#define LZB_PutExcessLRL(cp,val) LZB_PutExcessBW(cp,val)
#define LZB_PutExcessML(cp,val)  LZB_PutExcessBW(cp,val)

#define LZB_AddExcessLRL(cp,val) LZB_AddExcessBW(cp,val)
#define LZB_AddExcessML(cp,val)  LZB_AddExcessBW(cp,val)

/**/

//========================================================

static RADINLINE rrbool LZB_IsLazyMatchBetter(int newMatchLen,int oldMatchLen,int cur_lrl)
{
	// @@ needs work
	
    // is lazy match better even though we code an extra literal ?
    	
    int diff = newMatchLen-oldMatchLen;
        
    //if ( diff <= 1 ) return false;
    //if ( diff >= 4 ) return true;
	
	{
		//if ( cur_lrl == 0 ) return diff >= 4;

		return diff >= 2
		/* 
			|| ( diff >= k6 && 
				(int)newOffset < config_offset_threshold_3 &&
			  (int)oldOffset > config_offset_threshold_3 )
		/**/
			;
	}
}

static RADFORCEINLINE U8 * LZB_Output(U8 * cp, S32 lrl, const U8 * literals,  S32 matchlen ,  S32 mo )
{
	RR_ASSERT( lrl >= 0 );
	RR_ASSERT( matchlen >= LZB_MML );
	RR_ASSERT( mo > 0 && mo <= LZB_MAX_OFFSET );
	
	//rrprintf("[%3d][%3d][%7d]\n",lrl,ml,mo);
		
	S32 sendml = matchlen - LZB_MML;
	
	U32 ml_in_control  = RR_MIN(sendml,LZB_MLCONTROL_ESCAPE);
	
	if ( mo >= 8 ) // no overlap	
	{
		if ( lrl < LZB_LRL_ESCAPE )
		{
			U32 control = lrl | (ml_in_control<<4);

			*cp++ = (U8) control;
			
			RR_PUT64_NATIVE(cp, RR_GET64_NATIVE(literals));
			if ( lrl > 8 )
			{
				RR_PUT64_NATIVE(cp+8, RR_GET64_NATIVE(literals+8));
			}
			cp += lrl;
		}
		else
		{
			U32 control = LZB_LRL_ESCAPE | (ml_in_control<<4);

			*cp++ = (U8) control;
			
			U32 lrl_excess = lrl - LZB_LRL_ESCAPE;
			LZB_PutExcessLRL(cp,lrl_excess);

			// @@ ? is this okay for overrun ?
			//LZB_memcpy(cp,literals,lrl);
			//memcpy(cp,literals,lrl);
			lz_copysteptoend_overrunok(cp,literals,lrl);
			//cp += lrl;
		}
		
		if ( ml_in_control < LZB_MLCONTROL_ESCAPE )
		{
			RR_ASSERT( (U16)(mo) == mo );
			RR_PUT16_LE_UNALIGNED(cp,(U16)(mo));
			cp += 2;
		}
		else
		{
			U32 ml_excess = sendml - LZB_MLCONTROL_ESCAPE;
			
			// put special first byte, then offset, then remainder
			if ( ml_excess < 127 )
			{
				*cp++ = (U8)ml_excess;
			
				RR_ASSERT( (U16)(mo) == mo );
				RR_PUT16_LE_UNALIGNED(cp,(U16)(mo));
				cp += 2;
			}
			else
			{
				*cp++ = (U8)127;
			
				RR_ASSERT( (U16)(mo) == mo );
				RR_PUT16_LE_UNALIGNED(cp,(U16)(mo));
				cp += 2;
			
				ml_excess -= 127;
				LZB_PutExcessML(cp,ml_excess);
			}
		}
	}
	else
	{
		U32 lrl_in_control = RR_MIN(lrl,LZB_LRL_ESCAPE);
	
		 // overlap case
		U32 control = (lrl_in_control) | (LZB_MLCONTROL_ESCAPE<<4);
		
		*cp++ = (U8) control;
		
		if ( lrl_in_control == LZB_LRL_ESCAPE )
		{
			U32 lrl_excess = lrl - LZB_LRL_ESCAPE;
			LZB_PutExcessLRL(cp,lrl_excess);
		}
		
		lz_copysteptoend_overrunok(cp,literals,lrl);
		//cp += lrl;
		
		// special excess1 :
		UINTr excess1 = 128 + (ml_in_control<<3) + mo;
		RR_ASSERT( excess1 < 256 );
		
		*cp++ = (U8)excess1;
		
		if ( ml_in_control == LZB_MLCONTROL_ESCAPE )
		{
			U32 ml_excess = sendml - LZB_MLCONTROL_ESCAPE;
			LZB_PutExcessML(cp,ml_excess);
		}		
	}
	
	return cp;
}

#if LZB_FORCELASTLRL9

static RADINLINE U8 * LZB_OutputLast(U8 * cp, S32 lrl, const U8 * literals )
{
	RR_ASSERT( lrl >= 0 );
	
	//U32 ml = 0;
	//U32 mo = 0;

	U32 lrl_in_control = RR_MIN(lrl,LZB_LRL_ESCAPE);
	
	#if LZB_END_WITH_LITERALS
	// lrl_in_control must be at least 9
	lrl_in_control = RR_MAX(lrl_in_control,9);
	#endif
	
	U32 control = lrl_in_control;

	*cp++ = (U8) control;
	
	if ( lrl_in_control == LZB_LRL_ESCAPE )
	{
		U32 lrl_excess = lrl - LZB_LRL_ESCAPE;
		LZB_PutExcessLRL(cp,lrl_excess);
	}
	
	lzhd_memmov(cp,literals,lrl);
	cp += lrl;
	
	return cp;
}

#else

static RADINLINE U8 * LZB_OutputLast(U8 * cp, S32 lrl, const U8 * literals )
{
	cp = LZB_Output(cp,lrl,literals,LZB_MML,1);
	
	// remove the offset we put :
	cp -= 2;
	
	return cp;
}

#endif


static SINTa Encode_LZB_OneChunk_RLE(const U8 * raw, SINTa rawLen, U8 * comp, void * matcher, const U8 * dictionaryBase, 
									const U8 * blockMatchLimitPtr, const U8 * bufferEndPtr,
									LZQuantumHeader * pQH)
{
	//THREADPROFILEFUNC();
	
	U8 * compExpandedPtr = comp + rawLen;
	U8 * cp = comp;
	
	const U8 * rp = raw;
	const U8 * rpEnd = raw+rawLen;

	// we can match up to rpMatchEnd
	//	but matches can't start past rpLastMatchStart

	#if LZB_END_WITH_LITERALS
	const U8 * rpMatchEnd = rpEnd - 1;
	#else
	const U8 * rpMatchEnd = rpEnd;
	#endif
	// blockMatchLimitPtr is blockEnd - 8
	rpMatchEnd = RR_MIN(rpMatchEnd, blockMatchLimitPtr);
	
	// no matches can start within MML of MatchEnd
	const U8 * rpLastMatchStart = rpMatchEnd - LZB_MML;
	
	const U8 * literals_start = rp;

	U32 last32 = RR_GET32_NATIVE_UNALIGNED(rp);
	rp++;

	// while buffer :
	for(;;)
	{
		// while literals :
		for(;;)
		{
			if ( rp > rpLastMatchStart )
				goto end_of_buffer;
			
			RR_ASSERT( last32 == RR_GET32_NATIVE_UNALIGNED(rp-1) );
			
			// just grab 32 :
			U32 rp32 = RR_GET32_NATIVE_UNALIGNED(rp);
			if ( rp32 == last32 )
				break;

			// no match possible; add to literal run length

			// just step 1 :
			last32 = rp32;
			rp++;
			
			continue;
		}
		
		S32 cur_lrl = rrPtrDiff32(rp - literals_start);
		
		// catch expansion while writing :
		if ( cp+cur_lrl+3 >= compExpandedPtr )
		{
			return rawLen+1;
		}
		
		// find more match len :
		rp += 4;
		while( rp <= rpLastMatchStart )
		{
			U32 rp32 = RR_GET32_NATIVE_UNALIGNED(rp);
			if ( rp32 != last32 )
			{
				rp += GetNumBytesZeroNeverAll32(rp32^last32);
				break;
			}
			rp += 4;
		}
		
		S32 match_len = rrPtrDiff32(rp - literals_start) - cur_lrl;
									
		RR_ASSERT( rp <= rpMatchEnd );
		RR_ASSERT( match_len >= LZB_MML );
		
		#if 0
		
		// break long matches into a len=8 then offset=8
		// so we can use the non-overlap copier for most of it
		if ( match_len >= 16 )
		{
			cp = LZB_Output(cp,
						cur_lrl,literals_start,
						8,1);
						
			cp = LZB_Output(cp,
						0,literals_start,
						match_len-8,8);
		}
		else
		{
			cp = LZB_Output(cp,
						cur_lrl,literals_start,
						match_len,1);
		}
		
		#else
							
		cp = LZB_Output(cp,
					cur_lrl,literals_start,
					match_len,1);
		
		#endif
		
		last32 = RR_GET32_NATIVE_UNALIGNED(rp-1);		
		literals_start = rp;		
	}
	
	end_of_buffer:
	
	ASSERT_RELEASE( rp <= rpEnd );
			
	int cur_lrl = rrPtrDiff32(rpEnd - literals_start);
	#if LZB_END_WITH_LITERALS
	ASSERT_RELEASE(cur_lrl > 0 );
	#endif
	if ( cur_lrl > 0 )
	{
		cp = LZB_OutputLast(cp,
					cur_lrl,literals_start);
	}
		
	return rrPtrDiff( cp - comp );
}

//=================================================

#define LZB_FAST_TABLEBITS_DEFAULT	(18)

struct OodleLZB_CompressFast_Context
{
public:
	SINTa * m_tablePos;  // @@ should really be an S32 but then I have to be more careful blah
	int m_tableSizeBits;
	int m_tableSize;

	const U8 * m_windowBase;
	U8 * m_windowAlloc;
	SINTa m_windowSize;
	SINTa m_windowMask;

	bool IsSlidingWindow() const { return m_windowMask != -1; }

	// number of bytes seen this reset :
	SINTa m_posThisReset;
	SINTa m_maxOffset;
	const U8 * m_zeroPosPtr;

	const U8 * m_dicBase;
	SINTa m_dicSize;

	// for normal compressor :
	OodleLZB_CompressFast_Context(const U8 * raw, SINTa rawSize, SINTa dicBackup, int tableBits)
	{
	
		m_dicBase = raw - dicBackup;
		m_dicSize = rawSize + dicBackup;
		
		RR_ASSERT( tableBits >= 8 && tableBits <= 24 );
		m_tableSizeBits = tableBits;
		m_tableSize = 1<<tableBits;
		m_tablePos = OODLE_MALLOC_ARRAY_CACHEALIGNED(SINTa,m_tableSize);
		RR_ASSERT_ALWAYS( m_tablePos != NULL );
		
		m_windowBase = raw;
		m_windowAlloc = NULL;
		m_windowSize = rawSize + dicBackup;
		m_windowMask = -1;
		
		m_posThisReset = 0;
		m_maxOffset = ((SINTa)OODLELZ_MAX_OFFSET);
		m_zeroPosPtr = raw;
	}
	
	// for WithContext :
	OodleLZB_CompressFast_Context(U8 * window, bool slidingWindow, S32 tableBits)
	{
		if ( tableBits <= 0 )
			tableBits = LZB_FAST_TABLEBITS_DEFAULT;
			
		m_dicBase = NULL;
		m_dicSize = 0;
		
		RR_ASSERT( tableBits >= 8 && tableBits <= 24 );
		m_tableSizeBits = tableBits;
		m_tableSize = 1<<tableBits;
		m_tablePos = OODLE_MALLOC_ARRAY_CACHEALIGNED(SINTa,m_tableSize);
		RR_ASSERT_ALWAYS( m_tablePos != NULL );
		
		m_windowAlloc = NULL;
		
		if ( slidingWindow )
		{
			m_windowSize = LZB_SLIDING_WINDOW_SIZE;
			m_windowMask = m_windowSize-1;
			
			if ( window == NULL )
			{
				m_windowAlloc = U8_void( OodleMalloc(m_windowSize + LZB_END_OF_BLOCK_NO_MATCH_ZONE) );
				window = m_windowAlloc;
			}
		}
		else
		{
			m_windowSize = 0;
			m_windowMask = -1;
		}
		
		m_windowBase = window;
		
		m_posThisReset = 0;
		m_maxOffset = ((SINTa)OODLELZ_MAX_OFFSET);
		m_zeroPosPtr = window;
	}
	
	
	~OodleLZB_CompressFast_Context()
	{
		RR_ASSERT( m_tablePos != NULL );
		OODLE_FREE_ARRAY(m_tablePos,m_tableSize);
		
		if ( m_windowAlloc )
		{
			OodleFree(m_windowAlloc);
		}
	}
	
};

	
static void LZB_Fast_Hash_Reset(OodleLZB_CompressFast_Context * fh,SINTa pos)
{
	SINTa tableBytes = (fh->m_tableSize)*sizeof(fh->m_tablePos[0]);
	
	SIMPLEPROFILE_SCOPE_N(LZB_Fast_Hash_Reset,tableBytes);
	
	rrMemSetA_Aligned(fh->m_tablePos,pos,tableBytes);
	
	fh->m_posThisReset = 0;
}
		
static OodleLZB_CompressFast_Context * Encode_LZB_Fast_Alloc(OodleLZ_CompressionLevel level,const U8 * raw, SINTa rawSize, SINTa dicBackup, int tableBits)
{
	OodleLZB_CompressFast_Context * fh;
	fh = OodleNewT(OodleLZB_CompressFast_Context)(raw,rawSize,dicBackup,tableBits);
	
	LZB_Fast_Hash_Reset(fh,- dicBackup);
	
	if ( dicBackup > 0 )
	{
		THREADPROFILESCOPE("LZB Overlap Preload");
	
		if ( level <= OodleLZ_CompressionLevel_VeryFast )
			Encode_LZB_OneChunk_VeryFast_NoSlide_Preload(fh->m_tablePos,fh->m_tableSizeBits,raw,dicBackup);
		else if ( level == OodleLZ_CompressionLevel_Fast )
			Encode_LZB_OneChunk_Fast_NoSlide_Preload(fh->m_tablePos,fh->m_tableSizeBits,raw,dicBackup);
		else if ( level == OodleLZ_CompressionLevel_Normal )
			Encode_LZB_OneChunk_Normal_New_Preload(fh->m_tablePos,fh->m_tableSizeBits,raw,dicBackup);
		else
			RR_CANT_GET_HERE();
	}
	
	return fh;
}


//=================================================================================

//=================================================================================

#define LZB_Hash4	hmf_hash4_32

static RADINLINE U32 LZB_SecondHash4(U32 be4)
{
	const U32 m = 0x5bd1e995;

	U32 h = be4 * m;
	//h += h >> 16;
	//h *= m;

	h += (h>>11);
	
	return h;
}

static bool RADFORCEINLINE LZB_IsAllowedMatch(S32 matchLen, S32 matchOff)
{
	//RR_ASSERT( matchLen >= LZB_MML );
	//RR_ASSERT( matchOff >= 1 );
	return ( matchOff <= LZB_MAX_OFFSET );
}

// put LZB_FastCheckMatch in the .inl with this if it has the #if 0 block 
// #undef LZB_FastCheckMatch
// #define LZB_FastCheckMatch RR_STRING_JOIN(FAST_NAME,_CheckMatch)

static RADFORCEINLINE S32 LZB_FastCheckMatch(U32 ptr32,const U8 * ptr,const U8 * inBufEnd,const U8 * vs,
	const U8 * windowBase,
	S32 prevml,
	S32 * pcurMatchOff)
{
    //int matchLen = getmatchlen_mml3_one32(ptr32,ptr,vs,inBufEnd);
    int matchLen = getmatchlen_mml4_one32_better(ptr32,ptr,vs,inBufEnd,prevml);
    if ( matchLen == 0 ) return prevml;
    #if 0
    // can't do this here, cuz we need to reduce LRL
    while( vs > windowBase && ptr[-1] == vs[-1] )
    {
		ptr--; vs--; matchLen++;
    }
    #endif
    if ( matchLen > prevml )
    {
		RR_ASSERT( matchLen >= LZB_MML );
        S32 offset = rrPtrDiff32( ptr - vs );
        RR_ASSERT( offset > 0 );

		if ( offset <= LZB_MAX_OFFSET )
		{
	        *pcurMatchOff = offset;
		    return matchLen;
		}
    }

	return prevml;
}

static RADINLINE S32 LZB_MatchLen_Windowed(const U8 * ptr,const U8 * inBufEnd,
	SINTa vsPos, const U8 * windowBase, SINTa windowMask)
{
	RR_COMPILER_ASSERT( LZB_MML == 4 );
	// MML 4
	if ( ptr[0] != windowBase[ (vsPos + 0) & windowMask ]
	  || ptr[1] != windowBase[ (vsPos + 1) & windowMask ]
	  || ptr[2] != windowBase[ (vsPos + 2) & windowMask ]
	  || ptr[3] != windowBase[ (vsPos + 3) & windowMask ] )
		return 0;

	// can't do "vs32" thing because vs could be at end of the window

	// @@ Windowed matcher currently not doing "better" at all

	SINTa matchLen = 4;
	while( ptr+matchLen < inBufEnd &&
		ptr[matchLen] == windowBase[ (vsPos + matchLen) & windowMask ] )
	{
		matchLen++;
	}
	
	return S32_checkA(matchLen);
}

//=================================================================================

#define FAST_MAX_DICTIONARYBACKUP_VARIABLESTEP	1
#define FAST_MAX_DICTIONARYBACKUP				LZB_MAX_OFFSET



  //   lzt99,  24700820,  15367903,  15925047
//encode only      : 1.226 seconds, 1.17 b/hc, rate= 20.15 mb/s

/**

It looks like depth 0 + 2nd hash is better than depth 1 + no 2nd

depth == 0 + 2nd hash :
15662044, 1.577, 112.94
 LZB16, Fez_Essentials.pak,  17784477,  16335578, 1.089, 872.08, 2147.06, 15961983, 1.114, 214.13, 1877.40,
 LZB16,     enwik8, 100000000,  55523315, 1.801, 196.58, 1190.73, 49520124, 2.019, 78.26, 1229.94,

depth == 1 + no 2nd :
15710742, 1.572, 111.30
 LZB16, Fez_Essentials.pak,  17784477,  16335578, 1.089, 873.55, 2131.52, 15978308, 1.113, 166.90, 1906.00,
 LZB16,     enwik8, 100000000,  55523315, 1.801, 195.84, 1183.34, 49601704, 2.016, 67.63, 1227.64,

very slightly faster & very slightly more compression on every test

**/

/*     
#define FAST_HASH_DEPTH_SHIFT   (1) // more depth = more & more compression,
#define DO_FAST_2ND_HASH    //  rate= 30.69 mb/s , 15451369 <- turning this off is the best way to get more speed and less compression
/*/
#define FAST_HASH_DEPTH_SHIFT   (0)
#define DO_FAST_2ND_HASH
/**/

//     lzt99,  24700820,  15475520,  16677179
//encode only      : 0.880 seconds, 1.62 b/hc, rate= 28.08 mb/s

//#define FAST_HASH_DEPTH_SHIFT   (1) // more depth = more & more compression, but slower

/*

tradeoff of variable steps for inside-match hash update :

#define DO_FAST_UPDATE_MATCH_HASHES 1
lzt99 :
24,700,820 ->15,662,257 =  5.073 bpb =  1.577 to 1
encode           : 0.200 seconds, 71.58 b/kc, rate= 123.76 mb/s
decode           : 0.015 seconds, 953.30 b/kc, rate= 1648.36 mb/s
lzt99 , 24700820, 15662257, 199586, 14985

#define DO_FAST_UPDATE_MATCH_HASHES 2
lzt99 :
24,700,820 ->15,747,081 =  5.100 bpb =  1.569 to 1
encode           : 0.173 seconds, 82.42 b/kc, rate= 142.51 mb/s
decode           : 0.015 seconds, 958.63 b/kc, rate= 1657.60 mb/s
lzt99 , 24700820, 15747081, 173327, 14901

#define DO_FAST_UPDATE_MATCH_HASHES 4
24,700,820 ->15,823,701 =  5.125 bpb =  1.561 to 1
encode           : 0.161 seconds, 88.48 b/kc, rate= 152.99 mb/s
decode           : 0.015 seconds, 960.20 b/kc, rate= 1660.30 mb/s
lzt99 , 24700820, 15823701, 161458, 14877

-------------------

#define DO_FAST_UPDATE_MATCH_HASHES 1
c4z3: 1.554 1.532 1.489 1.409 1.274 1.069 0.808 0.543
#define DO_FAST_UPDATE_MATCH_HASHES 2
c4z3: 1.548 1.529 1.491 1.421 1.299 1.108 0.857 0.589

-------------------

pareto curves :

c2z3: 1.755 1.674 1.531 1.309 1.014 0.699 0.431 0.244
c2z2: 1.697 1.667 1.612 1.511 1.342 1.098 0.805 0.525
c4z4: 1.575 1.536 1.464 1.338 1.143 0.884 0.608 0.375
c4z3: 1.554 1.532 1.489 1.409 1.274 1.069 0.808 0.543
c4z3: 1.548 1.529 1.491 1.421 1.299 1.108 0.857 0.589
c4z2: 1.472 1.464 1.448 1.416 1.357 1.252 1.084 0.855

-------------------

*/

//#define DO_FAST_UPDATE_MATCH_HASHES 1 // helps compression a lot , like 0.30
#define DO_FAST_UPDATE_MATCH_HASHES 2
#define DO_FAST_LAZY_MATCH  // also helps a lot , like 0.15
#define DO_FAST_HASH_DWORD		1

#define FAST_MULTISTEP_LITERALS_SHIFT	(5)



#define FAST_SLIDING_WINDOW	1
#define FAST_NAME	Encode_LZB_OneChunk_Fast_Slide
#include "lzb_fast_generic.inl"
#undef FAST_NAME
#undef FAST_SLIDING_WINDOW
#define FAST_NAME	Encode_LZB_OneChunk_Fast_NoSlide
#define FAST_SLIDING_WINDOW	0
#include "lzb_fast.inl"

#undef FAST_SLIDING_WINDOW
#undef FAST_NAME
#undef FAST_HASH_DEPTH_SHIFT

#undef DO_FAST_UPDATE_MATCH_HASHES
#undef DO_FAST_LAZY_MATCH
#undef DO_FAST_2ND_HASH  

#define FAST_HASH_DEPTH_SHIFT	(0)

#undef FAST_MULTISTEP_LITERALS_SHIFT
#define FAST_MULTISTEP_LITERALS_SHIFT	(4)

#define FAST_SLIDING_WINDOW	1
#define FAST_NAME	Encode_LZB_OneChunk_VeryFast_Slide
//#include "lzb_vfast.inl"
#include "lzb_fast_generic.inl"
#undef FAST_NAME
#undef FAST_SLIDING_WINDOW
#define FAST_NAME	Encode_LZB_OneChunk_VeryFast_NoSlide
#define FAST_SLIDING_WINDOW	0
#include "lzb_vfast.inl"

#undef FAST_SLIDING_WINDOW
#undef FAST_NAME
#undef FAST_HASH_DEPTH_SHIFT

#undef DO_FAST_UPDATE_MATCH_HASHES
#undef DO_FAST_LAZY_MATCH
#undef DO_FAST_2ND_HASH  

//=================================================================

#undef FAST_MULTISTEP_LITERALS_SHIFT

//#define FAST_HASH_DEPTH_SHIFT   (3)
#define FAST_HASH_DEPTH_SHIFT   (2)
//#define DO_FAST_2ND_HASH

#define DO_FAST_UPDATE_MATCH_HASHES 1 // helps compression a lot , like 0.30
//#define DO_FAST_UPDATE_MATCH_HASHES 2 // helps compression a lot , like 0.30
#define DO_FAST_LAZY_MATCH  // also helps a lot , like 0.15
#define DO_FAST_HASH_DWORD		1

// it definitely helps to have FAST_MULTISTEP_LITERALS_SHIFT
//	even if the shift is quite high
//	it just lets you skip ahead in the uncompressible sections
//#define FAST_MULTISTEP_LITERALS_SHIFT	(6)
#define FAST_MULTISTEP_LITERALS_SHIFT	(7)


#undef FAST_SLIDING_WINDOW
#define FAST_NAME	Encode_LZB_OneChunk_Normal_New
#define FAST_SLIDING_WINDOW	0
#include "lzb_fast_normal.inl"

#undef FAST_SLIDING_WINDOW
#undef FAST_NAME
#undef FAST_HASH_DEPTH_SHIFT

#undef DO_FAST_UPDATE_MATCH_HASHES
#undef DO_FAST_LAZY_MATCH
#undef DO_FAST_2ND_HASH  
	
	
//=================================================================


void  OodleLZB_CompressFast_ResetContext(OodleLZB_CompressFast_Context * fh)
{
	if ( fh->IsSlidingWindow() )
	{
		LZB_Fast_Hash_Reset(fh,-RR_S32_MAX);
	
		// @@ could change window pointer here
	}
	else
	{
		LZB_Fast_Hash_Reset(fh,0);
		
		fh->m_windowBase = NULL;
		RR_ASSERT( fh->m_windowAlloc == NULL );
	}

	RR_ASSERT( fh->m_posThisReset == 0 );
}

 OodleLZB_CompressFast_Context *  OodleLZB_CompressFast_AllocContext(S32 hashTableBits,rrbool slidingWindow,U8 * window RADDEFAULTX(NULL))
{	
	OodleLZB_CompressFast_Context * ret = OodleNewT(OodleLZB_CompressFast_Context)(window,!!slidingWindow,hashTableBits);
	
	OodleLZB_CompressFast_ResetContext(ret);
	
	return ret;
}


 void  OodleLZB_CompressFast_FreeContext(OodleLZB_CompressFast_Context * context)
{
	OodleDelete(context);
}

void  OodleLZB_CompressFast_ResetContext(OodleLZB_CompressFast_Context * fh, 
												void * window,
												S32 slidingWindowSize,
												rrbool isSlidingWindow)
{
	RR_ASSERT_ALWAYS( window != NULL );
	RR_ASSERT_ALWAYS( fh->m_windowAlloc == NULL );
	fh->m_windowBase = U8_void(window);
	
	if ( isSlidingWindow )
	{
		RR_ASSERT_ALWAYS( rrIsPow2(slidingWindowSize) );
		fh->m_windowSize = slidingWindowSize;
		fh->m_windowMask = slidingWindowSize - 1;
		RR_ASSERT( fh->IsSlidingWindow() );
	}
	else
	{
		fh->m_windowSize = 0;
		fh->m_windowMask = -1;
		RR_ASSERT( ! fh->IsSlidingWindow() );
	}

	OodleLZB_CompressFast_ResetContext(fh);
}
 					
typedef SINTa (fp_Encode_Fast) (const U8 * raw, SINTa rawLen, U8 * comp, 
									const OodleLZB_CompressFast_Context * fh,
									const U8 * blockMatchLimitPtr, LZQuantumHeader * pQH);
													
 SINTa  OodleLZB_CompressFast_WithContext (OodleLZB_CompressFast_Context * context,
										const void * rawBuf,SINTa rawLen,void * compBuf,
										OodleLZ_CompressionLevel level_fast_or_veryfast,
										const OodleLZ_CompressOptions * pOptions RADDEFAULTX(NULL),
										OodleLZ_EncoderHeaders headers RADDEFAULTX(OodleLZ_EncoderHeaders_Default))
{	
	OodleLZB_CompressFast_Context * fh = context;

	RR_ASSERT( level_fast_or_veryfast <= OodleLZ_CompressionLevel_Fast );

	bool isSliding = fh->IsSlidingWindow();

	fp_Encode_Fast * enc;
	if ( level_fast_or_veryfast == OodleLZ_CompressionLevel_Fast )
	{
		if ( isSliding )
			enc = Encode_LZB_OneChunk_Fast_Slide_Sub;
		else
			enc = Encode_LZB_OneChunk_Fast_NoSlide_Sub;
	}
	else if ( level_fast_or_veryfast <= OodleLZ_CompressionLevel_VeryFast )
	{
		if ( isSliding )
			enc	= Encode_LZB_OneChunk_VeryFast_Slide_Sub;
		else
			enc	= Encode_LZB_OneChunk_VeryFast_NoSlide_Sub;
	}
	else
	{
		RR_ASSERT_FAILURE_ALWAYS("bad level_fast_or_veryfast");
		return -1;
	}

	if ( ! isSliding )
	{
		// raw must be continuing previously done buffer
		RR_ASSERT_ALWAYS( fh->m_posThisReset == 0 || rawBuf == (fh->m_zeroPosPtr + fh->m_posThisReset) );
	
		// first call since a reset, set windowBase :	
		if ( fh->m_windowBase == NULL )
		{
			RR_ASSERT( fh->m_posThisReset == 0 );
			fh->m_zeroPosPtr = U8_void(rawBuf);
			fh->m_dicBase = U8_void(rawBuf);
			fh->m_windowBase = U8_void(rawBuf);
		}
	}

	const U8 * rawPtr = U8_void(rawBuf);
	U8 * cp = U8_void(compBuf);
	const U8 * rawPtrEnd = rawPtr + rawLen;

	rrbool sendQuantumCRCs = pOptions->sendQuantumCRCs;
	rrbool seekChunkReset = pOptions->seekChunkReset;

	if ( 
	// headers == OodleLZ_EncoderHeaders_ForceBlockHeader ||
		(headers == OodleLZ_EncoderHeaders_OnlyFirstBlockHeader &&
		fh->m_posThisReset == 0
		) )
	{
		RR_ASSERT_ALWAYS( ! seekChunkReset );
		// can't be more than one block per call : ; sure there could
		//RR_ASSERT_ALWAYS( rawLen <= OODLELZ_BLOCK_LEN );
			
		// put block header
		LZBlockHeader header = { 0 };
		header.version = RAD_LZ_HEADER_VERSION;
		header.decodeType = RAD_LZ_DECODE_LZB16;
		header.chunkHasQuantumCRCs = sendQuantumCRCs;
		header.chunkIsReset = true;
		cp = LZBlockHeader_Put(header,cp);
	}
				
	while( rawPtr < rawPtrEnd )
	{
		if ( seekChunkReset && fh->m_posThisReset >= pOptions->seekChunkLen )
		{
			RR_ASSERT_ALWAYS( fh->m_posThisReset == pOptions->seekChunkLen );
			OodleLZB_CompressFast_ResetContext(context);
			RR_ASSERT( fh->m_posThisReset == 0 );
			
			if ( ! isSliding )
			{
				RR_ASSERT( fh->m_windowBase == NULL );
				fh->m_windowBase = rawPtr;
				fh->m_zeroPosPtr = rawPtr;
			}
		}
		
		if ( headers == OodleLZ_EncoderHeaders_Default )
		{
			// must be a quantum boundary :
			RR_ASSERT( ( fh->m_posThisReset & ( OODLELZ_QUANTUM_LEN-1) ) == 0 );
			
			if ( ( fh->m_posThisReset & (OODLELZ_BLOCK_LEN-1) ) == 0 )
			{
				// put block header
				LZBlockHeader header = { 0 };
				header.version = RAD_LZ_HEADER_VERSION;
				header.decodeType = RAD_LZ_DECODE_LZB16;
				//if ( (fh->m_posThisReset == 0 && seekChunkReset) || rawPtr == fh->m_dicBase )
				if ( fh->m_posThisReset == 0 )
				{
					header.chunkIsReset = true;
				}
				header.chunkHasQuantumCRCs = sendQuantumCRCs;
				cp = LZBlockHeader_Put(header,cp);
			}
		}
		
		SINTa curLen = RR_MIN(rrPtrDiff(rawPtrEnd-rawPtr),OODLELZ_QUANTUM_LEN);
		
		const U8 * curPtr;
		SINTa maxOffset;
		
		const U8 * blockMatchLimitPtr;
		
		RR_ASSERT( isSliding == fh->IsSlidingWindow() );
		
		// sliding window ?
		if ( isSliding )
		{
			// memcpy from raw into sliding
			SINTa curWindowPos = (fh->m_posThisReset & fh->m_windowMask);
			
			// can only go to end of window :
			curLen = RR_MIN( fh->m_windowSize - curWindowPos , curLen );
			
			curPtr = fh->m_windowBase + curWindowPos;
			
			memcpy(const_cast<U8 *>(curPtr),rawPtr,curLen);	
			
			maxOffset = fh->m_windowSize - curLen;
			// @@ optional :
			//	I think if I do this I can skip doing the actual reset
			maxOffset = RR_MIN(maxOffset,fh->m_posThisReset);
			
			// allow matches all the way to the end :
			// @@ NOTE : this is more strict than necessary when doing SW encode with OodleLZ_EncoderHeaders_Default headers
			blockMatchLimitPtr = curPtr+curLen;
				
			if ( fh->m_windowAlloc == NULL )
			{
				const U8 * windowEnd = fh->m_windowBase + fh->m_windowSize;
				blockMatchLimitPtr = RR_MIN(blockMatchLimitPtr, windowEnd - LZB_END_OF_BLOCK_NO_MATCH_ZONE);
			}
		}
		else
		{			
			curPtr = rawPtr;
			maxOffset = fh->m_posThisReset;
		
			const U8 * blockEnd = rawPtrEnd;
			const U8 * blockStart = fh->m_zeroPosPtr + (fh->m_posThisReset & (~(OODLELZ_BLOCK_LEN-1)));
			RR_ASSERT( blockStart >= fh->m_windowBase );
			blockEnd = blockStart + OODLELZ_BLOCK_LEN;	
			
			blockEnd = RR_MIN(blockEnd,rawPtrEnd);
	
			blockMatchLimitPtr = blockEnd - LZB_END_OF_BLOCK_NO_MATCH_ZONE;
		}
				
		fh->m_maxOffset = maxOffset;
		// set the zeropos pointer
		fh->m_zeroPosPtr = curPtr - fh->m_posThisReset;
			
		// compress one chunk
		// handle possible expansion

		#if 0
		if ( headers == OodleLZ_EncoderHeaders_NoHeaders )
		{
			// can't be more than one quantum :
			RR_ASSERT_ALWAYS( rawLen <= OODLELZ_QUANTUM_LEN );
			RR_ASSERT_ALWAYS( curLen == rawLen );
			RR_ASSERT_ALWAYS( ! sendQuantumCRCs );

			const U8 * quantumPtr = curPtr;
			SINTa quantumLen = curLen;
				
			LZQuantumHeader LZQH = { 0 };
				
			SINTa quantumCompLen = (*enc)(quantumPtr,quantumLen,cp,fh,blockMatchLimitPtr,&LZQH);
					
			if ( quantumCompLen >= quantumLen )
			{
				// expanded ; send uncompressed :
				// client must use compLen to detect memcpy block, I don't send any header
				
				memcpy(cp,quantumPtr,quantumLen);
				quantumCompLen = quantumLen;
			}

			cp += quantumCompLen;	
		}
		else
		#endif
		{
			
			do
			{
				const U8 * quantumPtr = curPtr;
				SINTa quantumLen = curLen;
				S32 quantumLen32 = S32_checkA(quantumLen);
		
				U8 * quantumHeaderPtr = cp;
				
				LZQuantumHeader LZQH = { 0 };
				LZQH.compLen = quantumLen32-1;
			
				// detect memset quantum :
				rrbool quantumIsMemset = MemsetQuantum_Test(quantumPtr,quantumLen32);
				if ( quantumIsMemset )
				{
					LZQH.crc = *quantumPtr;
					LZQH.compLen = 0;
					
					int packedQHLen = LZQuantumHeader_Put(quantumHeaderPtr,&LZQH,sendQuantumCRCs,quantumLen32);
					cp += packedQHLen;
					
					break;
				}
			
				#if 0 // disabled for now since Fast TMF doesn't support wholematch anyway
				// minimum len for wholeMatch :
				if ( quantumLen >= 3 )
				{
					// detect whole-match quantum :
					if ( CheckWholeMatchQuantum(&LZQH,rawBuf,rrPtrDiff(quantumPtr-rawBuf),quantumLen32,tmf,lastWholeMatchOffset) )
					{
						lastWholeMatchOffset = LZQH.wholeMatchOffset;
					
						int packedQHLen = LZQuantumHeader_Put(quantumHeaderPtr,&LZQH,sendQuantumCRCs,quantumLen32);
						cp += packedQHLen;
						
						quantumPtr += quantumLen;
						break;
					}
				}
				#endif
			
				//-------------------------------
				// normal quantum :
				
				// put QH initially to reserve space :		
				int packedQHLen = LZQuantumHeader_Put(quantumHeaderPtr,&LZQH,sendQuantumCRCs,quantumLen32);
				cp += packedQHLen; // packedQHLen is usually 2
			
				U8 * compPtrStart = cp; // compPtrStart is after the QH
		
				//-----------------------------------
		
				SINTa quantumCompLen = (*enc)(quantumPtr,quantumLen,cp,fh,blockMatchLimitPtr,&LZQH);
							
				if ( quantumCompLen >= quantumLen )
				{
					// expanded ; memcpy header :
					
					memcpy(compPtrStart,quantumPtr,quantumLen);
					quantumCompLen = quantumLen;
					
					LZQH.huffFlag = false; // clear flag in case we set it
				}
				
				// re-put the header now that I know compLen
				
				LZQH.compLen = S32_checkA(quantumCompLen);
				if ( sendQuantumCRCs )
					LZQH.crc = LZQuantumHeader_ComputeCRC(compPtrStart,quantumCompLen);
				
				// make sure we put the same amount because we reserved space earlier :		
				int packedQHLen2 = LZQuantumHeader_Put(quantumHeaderPtr,&LZQH,sendQuantumCRCs,quantumLen32);
				RR_UNUSED_VARIABLE(packedQHLen2);
				RR_ASSERT( packedQHLen == packedQHLen2 );
				
				cp += quantumCompLen;		

				// check the header :
				RR_ASSERT( rrPtrDiff32( compPtrStart - quantumHeaderPtr ) == packedQHLen );
				RR_ASSERT( rrPtrDiff32( cp - compPtrStart ) == LZQH.compLen );
			} while(0);
		}
		
		rawPtr += curLen;
		fh->m_posThisReset += curLen;
	}
	
	RR_ASSERT( rawPtr == rawPtrEnd );
	
	return rrPtrDiff(cp - U8_void(compBuf));
}
										
//=============================================================================
// match copies :

// used for LRL :
OOINLINE void copy_no_overlap_long(U8 * to, const U8 * from, SINTr length)
{
	// the exact copy here changes performance a lot on some targets
	lz_copysteptoend_overrunok(to,from,length);
}

OOINLINE void copy_match_simple_sw(U8 * to, SINTr fmPos, const U8 * window, SINTr windowMask, SINTr length)
{
	RR_ASSERT( length > 0 && length < windowMask );
	RR_ASSERT( fmPos >= 0 );
	RR_ASSERT( to >= window && to <= window+windowMask );
	RR_ASSERT( to != window + (fmPos & windowMask) );

	while(length--)
	{
		*to++ = window[ fmPos & windowMask ]; fmPos++;
	}
	
}

OOINLINE void copy_match_simple(U8 * to, const U8 * from, SINTr length)
{
	while(RAD_UNLIKELY(length--))
		*to++ = *from++;
}

OOINLINE void copy_no_overlap_nooverrun(U8 * to, const U8 * from, SINTr length)
{
	// used for final LRL of every block
	//  must not overrun
	lzhd_memmov(to,from,(size_t)length);
}

RADFORCEINLINE void copy_overlap8_nooverrun(U8 * to, const U8 * from, SINTr length)
{
	// used near end of buffer
	// needs to be careful not to overrun
	// from - to >= 8 is guaranteed on valid data (and if we write garbage on
	// invalid data, that's OK as long as we stay inside buffer bounds)
	if ( length < 8 )
		copy_no_overlap_nooverrun(to, from, length);
	else
	{
		// copy first 8 bytes
		lz_copy8(to,from);

		// copy remaining bytes so our final write is flush with the end of the copy
		SINTr advance = length & 7;
		copy_no_overlap_long(to+advance,from+advance,length & ~7);
	}
}

RR_COMPILER_ASSERT( LZB_MLCONTROL_ESCAPE == 15 );
RR_COMPILER_ASSERT( LZB_MATCHLEN_ESCAPE == 19 );
	
OOINLINE void copy_match_short_overlap(U8 * to, const U8 * from, SINTr ml)
{
	RR_ASSERT( ml >= LZB_MML && ml < LZB_MATCHLEN_ESCAPE );

	// overlap
	// @@ err not awesome
	to[0] = from[0];
	to[1] = from[1];
	to[2] = from[2];
	to[3] = from[3];
	to[4] = from[4];
	to[5] = from[5];
	to[6] = from[6];
	to[7] = from[7];
	if ( ml > 8 )
	{
		to += 8; from += 8; ml -= 8;
		// max of 10 more
		while(ml--)
		{
			*to++ = *from++;
		}
	}
}
			
OOINLINE void copy_match_short_nooverlap(U8 * to, const U8 * from, SINTr ml)
{
	RR_ASSERT( ml >= LZB_MML && ml < LZB_MATCHLEN_ESCAPE );

	lz_copy8(to,from);
	
	if ( ml > 8 )
	{
		// possibly 10 more
		lz_copy4(to+8,from+8);
		if ( ml > 12 )
		{
			lz_copy4(to+12,from+12);
			if ( ml > 16 )
			{
				lz_copy2(to+16,from+16);
			}
		}
	}
}

OOINLINE void copy_match_memset(U8 * to, int c, SINTr ml)
{
	RR_ASSERT( ml >= 4 );
	//U32 four = c | (c<<8);
	//four |= four<<16;
	U32 four = c | (c<<8) | (c<<16) | (c<<24);
	U8 * end = to + ml;
	RR_PUT32_NATIVE(to,four); to += 4;
	while(to<end)
	{
		RR_PUT32_NATIVE(to,four); to += 4;
	}
}

OOINLINE void copy_match_wrap(U8 * rp, const U8 * mp, SINTr ml, const U8 * window, SINTr windowSize)
{
	const U8 * windowEnd = window+windowSize;
	
	RR_ASSERT( mp >= window && mp < windowEnd );
	
	// shit but whatever, very rare
	for(SINTr i=0;i<ml;i++)
	{
		*rp++ = *mp++;						
		if ( mp == windowEnd )
			mp = window;
	}
}
								
//=============================================================================

static SINTa Decode_LZB_OneChunk(const U8 * comp,const U8 * comp_end, U8 * raw, SINTa rawLen, const U8 * dictionaryBase)
{
	//THREADPROFILEFUNC();
			

//rrprintf("Decode_LZB_OneChunk\n");

	U8 * rp = raw;
	U8 * rpEnd = raw+rawLen;

	const U8 *	cp = comp;
	
	//#define CHECK_VAL	0xABCDE
	//U32 check1 = CHECK_VAL;
	U8 cp_safe[128]; // must be at least LZB_C_BYTES_SET*2 + LZB_FINAL_SKID
	//U32 check2 = CHECK_VAL;

/*
rrprintf("rp : %08X\n",(U32)(UINTa)rp);
rrprintf("rpEnd : %08X\n",(U32)(UINTa)rpEnd);
rrprintf("cp : %08X\n",(U32)(UINTa)cp);
rrprintf("cp_safe : %08X\n",(U32)(UINTa)cp_safe);
*/

	UINTr control;
	SINTr lrl;
	UINTr ml_control;
	UINTr off;

	#define LZB_R_BYTES_FASTPATH	20
	#define LZB_C_BYTES_FASTPATH	11
	//#define LZB_C_BYTES_SET	(LZB_C_BYTES_FASTPATH*4)
	#define LZB_C_BYTES_SET	(LZB_C_BYTES_FASTPATH*4 + 8 + 2) // = 54
	#define LZB_R_BYTES_SET	(LZB_R_BYTES_FASTPATH*4)
	#define LZB_FINAL_SKID 16	// amount of sloppiness/over-read in our reads from cp

	RR_COMPILER_ASSERT( sizeof(cp_safe) > 2*LZB_C_BYTES_SET );

	U8 * rpSafeEnd = rpEnd - RR_MIN(LZB_R_BYTES_FASTPATH,rawLen);

//rrprintf("LZB_C_BYTES_SET : %d\n",LZB_C_BYTES_SET);

/**

LZB16 fuzz scheme :

offsets are checked for bounds if we are within 64k of start of the buffer
  two loops, one with offset checking one without

each iteration of "lzb_decode_inner.inl"
is allowed to read LZB_C_BYTES_FASTPATH
without checking for comp overrun

LZB_C_BYTES_FASTPATH = 11
= 1 control + 2 offset + 8 literal

to ensure that's okay, if I'm too close to the end of the comp buf,
I copy into the cp_safe buf on the stack

The "fast path" is LRL <= 8 and ml_control <= 8 (hence ml <= 12)
that means RAW can advance 20 on the fast path

The decoder can do 4 fast paths per set
before a set, enough raw room is checked

***/

	const U8 * comp_end_back = comp_end - (LZB_C_BYTES_SET+8);

/*
#define LZB_DECODE_ERROR_RETURN(str) do { \
	rrprintcorruption(str); \
	RR_ASSERT_ALWAYS( check1 == CHECK_VAL ); \
	RR_ASSERT_ALWAYS( check2 == CHECK_VAL );  \
	return -1; } while(0)
/*/
#define LZB_DECODE_ERROR_RETURN(str) do { \
	rrprintcorruption(str); \
	return -1; } while(0)
/**/

	// ASAN doesn't like the way I fool around with comp/comp_end here
	//	it looks like they point at cp_safe but are out of bounds
	//	I do it to keep the relative pointer math correct when I switch arrays
	//	technically pointing off the ends of arrays is undefined

	#define LZB_COMP_PTR_FUZZ_CHECK(cp) do { \
			if_unlikely ( cp >= comp_end_back ) \
			{ \
				if ( cp >= comp_end ) \
				{ \
					LZB_DECODE_ERROR_RETURN("LZB comp over-read\n"); \
				} \
				if ( comp_end == cp_safe + sizeof(cp_safe) ) \
				{ \
					LZB_DECODE_ERROR_RETURN("LZB comp safe twice\n"); \
				} \
				size_t copy_len = (comp_end-cp); \
				RR_ASSERT( copy_len < sizeof(cp_safe) - LZB_FINAL_SKID ); \
				memmove(cp_safe,cp,copy_len); \
				comp = cp_safe - (cp - comp); \
				comp_end = cp_safe + sizeof(cp_safe) - LZB_FINAL_SKID; \
				comp_end_back = comp_end - (LZB_C_BYTES_SET+8); \
				cp = cp_safe; \
			} } while(0)
			
	if ( (rp - dictionaryBase) < 65536 )
	{
		#define DO_OFFSET_CHECK 1
		#define DO_FAST_PATH 1
	
		if ( rp < rpSafeEnd )
		{
			for(;;)
			{
				LZB_ASSERT( rp < rpSafeEnd );

				LZB_COMP_PTR_FUZZ_CHECK(cp);

				// FUZZ : no rp vs rpEnd check here
				//	it will be caught when we try to do an lrl or ml

				#include "lzb_decode_inner.inl"
			}
		}

		#undef DO_FAST_PATH
		#define DO_FAST_PATH 0

		// Use careful loop near the end
		if ( rp < rpEnd )
		{
			for(;;)
			{
				LZB_ASSERT( rp < rpEnd );

				LZB_COMP_PTR_FUZZ_CHECK(cp);

				// FUZZ : no rp vs rpEnd check here
				//	it will be caught when we try to do an lrl or ml

				#include "lzb_decode_inner.inl"
			}
		}
		
		#undef DO_FAST_PATH
		#undef DO_OFFSET_CHECK
	}
	else
	{
		#define DO_OFFSET_CHECK 0
		#define DO_FAST_PATH 1
	
		if ( rp < rpSafeEnd )
		{
			for(;;)
			{
				LZB_ASSERT( rp < rpSafeEnd );

				LZB_COMP_PTR_FUZZ_CHECK(cp);

				// this is where the SET should help
				//	and it definitely hurts with some compilers
				// -> helps enormously on MSVC , haven't tested other platforms
				// -> bad in clang 3.5 , fixed in clang 3.6
				#if 1

				if_unlikely ( (SINTa)(rpEnd-rp) < LZB_R_BYTES_SET )
				{
					if_unlikely( rp >= rpEnd )
					{
						LZB_DECODE_ERROR_RETURN("LZB raw went too far\n");
					}
					goto last2;
				}

				#define last_label1	last_label3
				#define last_label2 last_label4
				#define IS_LAST 0
				#include "lzb_decode_inner.inl"
				#include "lzb_decode_inner.inl"
				#include "lzb_decode_inner.inl"
				#undef IS_LAST
				#define IS_LAST 1
				last2:
				#include "lzb_decode_inner.inl"
				#undef IS_LAST

				#else

				// FUZZ : no rp vs rpEnd check here
				//	it will be caught when we try to do an lrl or ml

				#include "lzb_decode_inner.inl"

				#endif
			}
		}

		#undef DO_FAST_PATH
		#define DO_FAST_PATH 0

		if ( rp < rpEnd )
		{
			for(;;)
			{
				LZB_ASSERT( rp < rpEnd );

				LZB_COMP_PTR_FUZZ_CHECK(cp);
				// FUZZ : no rp vs rpEnd check here
				//	it will be caught when we try to do an lrl or ml

				#include "lzb_decode_inner.inl"
			}
		}
		
		#undef DO_FAST_PATH
		#undef DO_OFFSET_CHECK
	}

//rrprintf("finishing...\n");
		
	if_unlikely ( rp != rpEnd )
	{
		LZB_DECODE_ERROR_RETURN("LZB didn't stop at end\n");
	}
	
	//RR_ASSERT_ALWAYS( check1 == CHECK_VAL );
	//RR_ASSERT_ALWAYS( check2 == CHECK_VAL );
	
	SINTa used = rrPtrDiff( cp - comp );
	return used;
}

//=====================================================================

//static SINTa g_total_pos = 0; // for debugging

static SINTa Decode_LZB_OneChunk_SW(const U8 * comp,const U8 * comp_end, U8 * raw, SINTa rawLen,
												const U8 * window, SINTr windowSize)
{
	//THREADPROFILEFUNC();

	// raw should be in the window :
	RR_ASSERT( raw >= window && (raw+rawLen) <= window+windowSize );
			
	U8 * rp = raw;
	U8 * rpEnd = raw+rawLen;

	const U8 *	cp = comp;
	
	U8 cp_safe[32];
	
	for(;;)
	{
		RR_ASSERT( rp < rpEnd );
	
		#if 1
		if ( cp >= (comp_end-16) )
		{
			memcpy(cp_safe,cp,(comp_end-cp));
			comp = cp_safe - (cp - comp); // fix so decomp size is right
			comp_end = cp_safe + 1024; // so it's not hit again
			cp = cp_safe;
		}
		#endif
		
		/*
		// NO don't check this
		// we CAN over-write by 8
		// fuzz safety check :
		if_unlikely ( rp+8 > rpEnd )
		{
			rrprintcorruption("LZB decomp past end\n");
			return -1;
		}
		*/
			
		UINTr control = *cp++;
		
		UINTr lrl = control & 0xF;
		UINTr ml_control = (control>>4);
		UINTr ml = ml_control + LZB_MML;
		// note this may not be the real ml
		// 
									
		// copy 8 literals :
		//	NOTEZ : this effectively makes my usable window 8 bytes smaller!
		RR_PUT64_NATIVE( rp , RR_GET64_NATIVE(cp) );


		#if 1 // helps less as the other case gets better, but at the moment still helps a little
		// try to catch the fastest & most common case
		if ( (control & 0x88) == 0 )
		{
			RR_ASSERT( lrl <= 8 && ml_control < 8 );
			
			rp += lrl;
			cp += lrl;
			
			if_unlikely ( rp+ml > rpEnd )
			{
				rrprintcorruption("LZB decomp past end\n");
				return -1;
			}
		
			UINTr off = RR_GET16_LE_UNALIGNED(cp);
			cp += 2;
			
			const U8 * match = rp - off;
			
			RR_ASSERT( off >= 8 || ml <= off );
			RR_ASSERT( ml <= 11 );
			
			if ( match < window )
			{
				match += windowSize;
				copy_match_wrap(rp,match,ml,window,windowSize);
			}
			else
			{			
				RR_PUT64_NATIVE( rp ,  RR_GET64_NATIVE(match) );
				RR_PUT32_NATIVE( (rp+8) ,  RR_GET32_NATIVE((match+8)) );
			}
				
			rp += ml;
			
			continue;
		}
		#endif

		// if lrl was <= 8 we did it, else need this :
		if_unlikely ( lrl > 8 )
		{
			if_unlikely ( lrl >= LZB_LRL_ESCAPE )
			{
				LZB_AddExcessLRL( cp, lrl );
			
				// hide the EOF check here ?
				// has to be after the GetExcess
				if_unlikely ( rp+lrl >= rpEnd )
				{	
					if ( rp+lrl > rpEnd )
					{
						rrprintcorruption("LZB decomp past end\n");
						return -1;
					}
					
					copy_no_overlap_nooverrun(rp,cp,lrl);
				
					rp += lrl;
					cp += lrl;
					break;
				}
				else
				{
					// total undo of the previous copy	
					copy_no_overlap_long(rp,cp,lrl);
				}
			}
			else // > 8 but not 0xF
			{
				// hide the EOF check here ?
				if_unlikely ( rp+lrl >= rpEnd )
				{	
					// may be a false 9
					// just always fix it :
					lrl = rrPtrDiff32( rpEnd - rp );
					
					copy_no_overlap_nooverrun(rp,cp,lrl);
				
					rp += lrl;
					cp += lrl;
					break;						
				}
				else
				{
					// put 8 more :
					RR_PUT64_NATIVE( (rp+8) , RR_GET64_NATIVE((cp+8)) );
				}
			}
		}
				
		rp += lrl;
		cp += lrl;
				
		//RR_ASSERT( rp+ml <= rpEnd ); // no true cuz ml may be bogus
					
		// speculatively grab offset but don't advance cp yet
		UINTr off = RR_GET16_LE_UNALIGNED(cp);
		
		if ( ml_control <= 8 )
		{
			cp += 2; // consume offset
			const U8 * match = rp - off;
			
			RR_ASSERT( ml <= 12 );

			if_unlikely ( rp+ml > rpEnd )
			{
				rrprintcorruption("LZB match past end\n");
				return -1;
			}
			
			if ( match < window )
			{
				match += windowSize;
				copy_match_wrap(rp,match,ml,window,windowSize);
			}
			else
			{
				RR_PUT64_NATIVE( rp , RR_GET64_NATIVE(match) );
				RR_PUT32_NATIVE( rp+8 , RR_GET32_NATIVE(match+8) );
			}
				
			rp += ml;
			continue;
		}
		else
		{		
			if_likely( ml_control < LZB_MLCONTROL_ESCAPE ) // short match
			{
				cp += 2; // consume offset
				const U8 * match = rp - off;
			
				RR_ASSERT( off >= 8 || ml <= off );
				
				if_unlikely ( rp+ml > rpEnd )
				{
					rrprintcorruption("LZB match past end\n");
					return -1;
				}
				
				if ( match < window )
				{
					match += windowSize;
					copy_match_wrap(rp,match,ml,window,windowSize);
				}
				else
				{
					RR_PUT64_NATIVE( rp , RR_GET64_NATIVE(match) );
					RR_PUT64_NATIVE( rp+8 , RR_GET64_NATIVE(match+8) );

					if ( ml > 16 )
					{
						*((U16 *)(rp+16)) = *((U16 *)(match+16));
					}
				}
			}
			else
			{
				// get 1-byte excess code
				UINTr excesslow = off&127;
				cp++; // consume 1
				
				if ( off & 128 ) // special low offset case
				{				
					//ml_control = (off>>3) & 0xF;
					ml_control = excesslow >> 3;
					ml = ml_control + LZB_MML;
					if ( ml_control == 0xF )
					{
						// get more ml
						LZB_AddExcessML( cp, ml );
					}	
				
					if_unlikely ( rp+ml > rpEnd )
					{
						rrprintcorruption("LZB match past end\n");
						return -1;
					}
				
					UINTr myoff = off & 7;
					
					// low offset, can't do 8-byte grabs
					if ( myoff == 1 )
					{
						int c = (rp == window) ? (window[windowSize-1]) : rp[-1];
						copy_match_memset(rp,c,ml);
					}
					else
					{
						const U8 * match = rp - myoff;
						if ( match < window ) match += windowSize;
						
						// just use copy_match_wrap cuz it does byte-at-a-time
						copy_match_wrap(rp,match,ml,window,windowSize);
					}
				}
				else
				{
					SINTr myoff = RR_GET16_LE_UNALIGNED(cp); cp += 2;
					const U8 * match = rp - myoff;
			
					ml += excesslow;
			
					if_unlikely ( excesslow == 127 )
					{
						// get more ml
						LZB_AddExcessML( cp, ml );
					}
					
					// fuzz check :
					if_unlikely ( rp+ml > rpEnd )
					{
						rrprintcorruption("LZB match past end\n");
						return -1;			
					}
					
					if ( match < window )
					{
						match += windowSize;
						
						if ( match+ml+8 <= window )
						{
							// this is gauranted by LZB_MAX_OFFSET
							RR_ASSERT( match+windowSize-8 > rp );
							// same as :
							RR_ASSERT( windowSize-8 > myoff );
							
							goto long_match;
						}
						else
						{
							copy_match_wrap(rp,match,ml,window,windowSize);
						}
					}
					else
					{
						long_match:
						
						// 8-byte copier :
						copy_no_overlap_long(rp,match,ml);
					}
				}
			}
			
			rp += ml;
		}
	}
		
	if_unlikely ( rp != rpEnd )
	{
		rrprintcorruption("LZB didn't stop at end\n");
		return -1;
	}

	SINTa used = rrPtrDiff( cp - comp );
	return used;
}

//===============================================================

#include "oodleconfigvalues.h"
//#include "LongRangeMatcher.h"

#undef RADDEFAULT
#define RADDEFAULT(x)

static SINTa LZB_Compress_Sub(const U8 * wholeRawBuf,U8 * compBuf,SINTa wholeRawLen,OodleLZ_CompressionLevel level,
                            const OodleLZ_CompressOptions * pOptions,
                            const U8 * dictionaryBase);
                            
SINTa LZB_Compress(const U8 * wholeRawBuf,U8 * compBuf,SINTa wholeRawLen,OodleLZ_CompressionLevel level,
                            const OodleLZ_CompressOptions * pOptions RADDEFAULT(NULL),
                            const U8 * dictionaryBase RADDEFAULT(NULL),
                            const LRMCascade * lrm_casc RADDEFAULT(NULL))
{
	if ( pOptions == NULL )
		pOptions = OodleLZ_CompressOptions_GetDefault(OodleLZ_Compressor_LZB16,level);

	//-----------------------------------------------------
	// much is copy-paste from LZHLW CompressOptimal :

	const U8 * curRawPtr = wholeRawBuf;
	SINTa rawLenRemaining = (SINTa)wholeRawLen;

	if ( pOptions->seekChunkReset )
	{
		// @@ lazy implementation - causes reallocation of matchers and setup for each chunk
		// just compress each seek chunk as a separate Sub call :
		// no LRM, assuming seek chunks are small
	
		S32 seekChunkLen = pOptions->seekChunkLen;
		
		SINTa totCompLen = 0;
		while( rawLenRemaining > 0 )
		{
			SINTa curLen = RR_MIN(rawLenRemaining,seekChunkLen);
			
			// log the chunking :
			rrPrintf_v2("LZB : chunk : [%d]\n",(int)curLen);
						
			SINTa curCompLen = LZB_Compress_Sub(curRawPtr,compBuf,curLen,level,pOptions,curRawPtr);

			rawLenRemaining -= curLen;
			curRawPtr += curLen;
			compBuf += curCompLen;
			totCompLen += curCompLen;
		}
		
		return totCompLen;
	}
	else
	{
		if ( dictionaryBase == NULL )
			dictionaryBase = wholeRawBuf;

		SINTa dicBackup = rrPtrDiff(wholeRawBuf - dictionaryBase);
		//SINTa rawLenPlusBackup = wholeRawLen + dicBackup;
		
		// log :
		rrPrintf_v2("LZB : pos : " RR_SINTa_FMT " , size : %d\n",
			dicBackup, (int)wholeRawLen );
		
		dicBackup = RR_MIN(dicBackup,LZB_MAX_OFFSET);
		
		const U8 * dictionaryStartPtr = curRawPtr - dicBackup;
			
		SINTa curCompLen = LZB_Compress_Sub(curRawPtr,compBuf,rawLenRemaining,level,pOptions,dictionaryStartPtr);
	
		return curCompLen;
	}
}

static SINTa LZB_Compress_Sub(const U8 * rawBuf,U8 * compBuf,SINTa rawLen,OodleLZ_CompressionLevel level,
                            const OodleLZ_CompressOptions * pOptions,
                            const U8 * dictionaryBase)
{	
	//SIMPLEPROFILE_SCOPE_N(LZB_Compress_Sub,rawLen);
	
	if ( rawLen < RR_LZH_MIN_RAW_LEN )
	{
		// only memcpy compression :
		level = OodleLZ_CompressionLevel_None;
	}

	if ( level < OodleLZ_CompressionLevel_SuperFast )
	{
		return OodleLZ_CompressMemcpy_DecodeType(RAD_LZ_DECODE_LZB16,rawBuf,rawLen,compBuf,dictionaryBase,pOptions);
	}
	else
	{
		//RR_ASSERT( level == OodleLZ_CompressionLevel_VeryFast || level == OodleLZ_CompressionLevel_Fast );

		SINTa dicBackup = rrPtrDiff( rawBuf - dictionaryBase );
		
		// LZB VeryFast speed is best with HT bits in 12-14
		//	starts going down at 15 but still tolerable
		//	see also s_defaultOptions_VeryFast_LZB16
		S32 tableBits;
		if ( level >= OodleLZ_CompressionLevel_Normal )
			tableBits = GetLZMatchTableBits(rawLen,level,pOptions,15,20,15,20);
		else if ( level == OodleLZ_CompressionLevel_SuperFast )
			tableBits = 13; // OODLELZ_SUPERFAST_HASH_BITS
		else
			tableBits = GetLZMatchTableBits(rawLen,level,pOptions,12,17,14,19);
				
		OodleLZB_CompressFast_Context * data = Encode_LZB_Fast_Alloc(level,rawBuf,rawLen,dicBackup,tableBits);
		if ( ! data )
			return -1;

		SINTa compLen;

		if ( level <= OodleLZ_CompressionLevel_VeryFast ) // SuperFast too
			compLen = Encode_LZB_VeryFast(rawBuf,rawLen,compBuf,data,dictionaryBase,level,pOptions);
		else if ( level == OodleLZ_CompressionLevel_Fast )
			compLen = Encode_LZB_Fast(rawBuf,rawLen,compBuf,data,dictionaryBase,level,pOptions);
		else
			compLen = Encode_LZB_Normal_New(rawBuf,rawLen,compBuf,data,dictionaryBase,level,pOptions);		

		OodleDelete(data);
		
		return compLen;
	}
}


S32 LZB_DecodeOneQuantum(U8 * decPtr,U8 * decPtrEnd,const U8 * cp,const U8 * compPtrEnd,const LZQuantumHeader & LZQH,
							const U8 * decBuf, SINTr decBufferSize, 
							rrbool isSlidingWindow,
							const U8 * dictionaryBase)
{
	SINTa decLen = rrPtrDiff( decPtrEnd - decPtr );
	
	RR_ASSERT( decLen <= LZB_CHUNK_LEN );

	if ( isSlidingWindow )
	{
		SINTa chunkCompLen = Decode_LZB_OneChunk_SW(cp,compPtrEnd,decPtr,decLen,decBuf,decBufferSize);
		
		return S32_checkA(chunkCompLen);
	}
	else
	{	
		SINTa chunkCompLen = Decode_LZB_OneChunk(cp,compPtrEnd,decPtr,decLen,dictionaryBase);
			
		return S32_checkA(chunkCompLen);
	}
}



//=============================================

static SINTa Encode_LZB_Chunker(fp_Encode_OneChunk * enc,const U8 * rawBuf, SINTa rawLen, U8 * compBuf, void * tmf,
const U8 * dictionaryBase,
OodleLZ_CompressionLevel level, const OodleLZ_CompressOptions * pOptions)
{
	SIMPLEPROFILE_SCOPE_N(lzb_chunker,rawLen);
	THREADPROFILEFUNC();
	
	//RR_ASSERT( tmf != NULL );
	RR_ASSERT( pOptions != NULL );
	//RR_ASSERT( ! pOptions->seekChunkReset || tmf->m_dicBackup == 0 );
	RR_ASSERT( ! pOptions->seekChunkReset || rawLen <= pOptions->seekChunkLen );
	
	const U8 * rawPtr = rawBuf;
	U8 * cp = compBuf;
	SINTa rawLenLeft = rawLen;
	
//	SINTa lastWholeMatchOffset = 0;
	
	// for each LZ Block :		
	while ( rawLenLeft > 0 )
	{
		const U8 * blockPtr = rawPtr;
		U8 * blockCompPtr = cp;
		SINTa blockLen = RR_MIN(rawLenLeft,OODLELZ_BLOCK_LEN); 
		
		//-----------------------------------------------------
		// write LZHeader for chunk :
	    
		rrbool sendQuantumCRCs = pOptions->sendQuantumCRCs;
	    
		// only the first chunk can be independent, because our outer LZB_Compress call
		//	breaks the call to _Sub on seekchunk boundaries
		bool chunkIsReset = ( ( rawPtr == rawBuf && pOptions->seekChunkReset ) ||
			rawPtr == dictionaryBase ); // @@ first call ?
				
		if ( 1 )   
		{
			LZBlockHeader header = { 0 };
			header.version = RAD_LZ_HEADER_VERSION;
			header.decodeType = RAD_LZ_DECODE_LZB16;
			header.chunkIsReset = chunkIsReset;
			header.chunkHasQuantumCRCs = sendQuantumCRCs;
			cp = LZBlockHeader_Put(header,cp);
		}
    
		//-----------------------------------------------------
		
		// write quanta in block :
		const U8 * blockEnd = blockPtr + blockLen;
		
		const U8 * blockMatchLimitPtr = blockEnd - LZB_END_OF_BLOCK_NO_MATCH_ZONE;
		
		const U8 * quantumPtr = blockPtr;
				
		// for each Quantum in block :
		while( quantumPtr < blockEnd )
		{
			//SINTa quantumPos = rrPtrDiff( quantumPtr - rawBuf );
			SINTa quantumLen = RR_MIN( rrPtrDiff(blockEnd - quantumPtr) ,OODLELZ_QUANTUM_LEN);
			S32 quantumLen32 = S32_checkA(quantumLen);
	
			U8 * quantumHeaderPtr = cp;
			
			LZQuantumHeader LZQH = { 0 };
			LZQH.compLen = quantumLen32-1;
		
			// detect memset quantum :
			rrbool quantumIsMemset = MemsetQuantum_Test(quantumPtr,quantumLen32);
			if ( quantumIsMemset )
			{
				LZQH.crc = *quantumPtr;
				LZQH.compLen = 0;
				
				int packedQHLen = LZQuantumHeader_Put(quantumHeaderPtr,&LZQH,sendQuantumCRCs,quantumLen32);
				cp += packedQHLen;
				
				quantumPtr += quantumLen;
				continue;
			}
		
			#if 0
			// minimum len for wholeMatch :
			if ( quantumLen >= 3 )
			{
				// detect whole-match quantum :
				if ( CheckWholeMatchQuantum(&LZQH,rawBuf,rrPtrDiff(quantumPtr-rawBuf),quantumLen32,tmf,lastWholeMatchOffset) )
				{
					lastWholeMatchOffset = LZQH.wholeMatchOffset;
				
					int packedQHLen = LZQuantumHeader_Put(quantumHeaderPtr,&LZQH,sendQuantumCRCs,quantumLen32);
					cp += packedQHLen;
					
					quantumPtr += quantumLen;
					continue;
				}
			}
			#endif
			
			//-------------------------------
			// normal quantum :
			
			// put QH initially to reserve space :		
			int packedQHLen = LZQuantumHeader_Put(quantumHeaderPtr,&LZQH,sendQuantumCRCs,quantumLen32);
			cp += packedQHLen; // packedQHLen is usually 2
		
			U8 * compPtrStart = cp; // compPtrStart is after the QH
	
			//-----------------------------------
			// Encode_LZB_OneChunk_Normal , etc. :
	
			SINTa quantumCompLen;
			
			if ( quantumPtr+LZB_MML >= blockMatchLimitPtr )
			{
				// send uncompressed
				quantumCompLen = quantumLen+1;
			}
			else
			{
				quantumCompLen = (*enc)(quantumPtr,quantumLen,cp,tmf,dictionaryBase,blockMatchLimitPtr,rawBuf+rawLen,&LZQH);
			}
					
			//-------------------------------------------------------------
			// done compressing quantum, do the header
		
			#if 0
			// check if we only got a tiny bit of compression and make it a memcpy instead
			// -> no point in doing this in LZB because decode is so fast
			//def DO_MEMCPY_NEARLY_INCOMPRESSIBLE_QUANTA_SSTBDIV
			if ( quantumCompLen <  quantumLen &&
				(quantumLen - quantumCompLen)*DO_MEMCPY_NEARLY_INCOMPRESSIBLE_QUANTA_SSTBDIV <= (pOptions->spaceSpeedTradeoffBytes) &&
				quantumLen >= (OODLELZ_QUANTUM_LEN/2) )
			{
				// compLen is within spaceSpeedTradeoffBytes of rawlen
				//	and rawLen is reasonably big
				// -> bail back to memcpy
				// fake expansion :
				quantumCompLen = quantumLen+1;
			}
			#endif
			
			if ( quantumCompLen >= quantumLen )
			{
				// expanded ; memcpy header :
				
				memcpy(compPtrStart,quantumPtr,quantumLen);
				quantumCompLen = quantumLen;
				
				LZQH.huffFlag = false; // clear flag in case we set it
			}
			
			// re-put the header now that I know compLen
			
			LZQH.compLen = S32_checkA(quantumCompLen);
			if ( sendQuantumCRCs )
				LZQH.crc = LZQuantumHeader_ComputeCRC(compPtrStart,quantumCompLen);
			
			// make sure we put the same amount because we reserved space earlier :		
			int packedQHLen2 = LZQuantumHeader_Put(quantumHeaderPtr,&LZQH,sendQuantumCRCs,quantumLen32);
			RR_UNUSED_VARIABLE(packedQHLen2);
			RR_ASSERT( packedQHLen == packedQHLen2 );
			
			// advance to next quantum :
			quantumPtr += quantumLen;
			cp += quantumCompLen;		

			// check the header :
			RR_ASSERT( rrPtrDiff32( compPtrStart - quantumHeaderPtr ) == packedQHLen );
			RR_ASSERT( rrPtrDiff32( cp - compPtrStart ) == LZQH.compLen );
				
		} // iterate quanta
		
		SINTa blockCompLen = rrPtrDiff( cp - blockCompPtr );
		// blockCompLen includes the 1 byte block header
		if ( blockCompLen >= blockLen )
		{
			// block expanded, put a memcpy block instead

			// put the one byte header specifying a memcpy chunk :
			LZBlockHeader header = { 0 };
			header.version = RAD_LZ_HEADER_VERSION;
			header.decodeType = RAD_LZ_DECODE_LZB16;
			header.offsetShift = 0;
			header.chunkIsMemcpy = true;
			header.chunkIsReset = chunkIsReset;
			header.chunkHasQuantumCRCs = false;
				
			cp = blockCompPtr;
			cp = LZBlockHeader_Put(header,cp);
			
			memcpy(cp,blockPtr,blockLen);
			cp += blockLen;
		}
		
		// step blocks :
		rawPtr = blockEnd;
		rawLenLeft -= blockLen;
		// cp already advanced
	}
	
	SINTa compLen = rrPtrDiff( cp - compBuf );
	return compLen;
}

// for testing the special quantum types only (memset,wholematch)
static SINTa Encode_LZB_OneChunk_None(const U8 * raw, SINTa rawLen, U8 * comp, void * tmf, const U8 * dictionaryBase,
									const U8 * blockMatchLimitPtr, const U8 * bufferEndPtr, LZQuantumHeader * pQH)
{

	// expand :
	return rawLen+1;
}

static SINTa Encode_LZB_None(const U8 * rawBuf, SINTa rawLen, U8 * compBuf, void * tmf, const U8 * dictionaryBase, OodleLZ_CompressionLevel level, const OodleLZ_CompressOptions * pOptions)
{
	return Encode_LZB_Chunker(Encode_LZB_OneChunk_None,rawBuf,rawLen,compBuf,tmf,dictionaryBase,level,pOptions);
}

static SINTa Encode_LZB_Fast(const U8 * rawBuf, SINTa rawLen, U8 * compBuf, void * tmf, const U8 * dictionaryBase, OodleLZ_CompressionLevel level, const OodleLZ_CompressOptions * pOptions)
{
	return Encode_LZB_Chunker(Encode_LZB_OneChunk_Fast_NoSlide,rawBuf,rawLen,compBuf,tmf,dictionaryBase,level,pOptions);
}

static SINTa Encode_LZB_VeryFast(const U8 * rawBuf, SINTa rawLen, U8 * compBuf, void * tmf, const U8 * dictionaryBase, OodleLZ_CompressionLevel level, const OodleLZ_CompressOptions * pOptions)
{
	return Encode_LZB_Chunker(Encode_LZB_OneChunk_VeryFast_NoSlide,rawBuf,rawLen,compBuf,tmf,dictionaryBase,level,pOptions);
}

static SINTa Encode_LZB_RLE(const U8 * rawBuf, SINTa rawLen, U8 * compBuf, const U8 * dictionaryBase, OodleLZ_CompressionLevel level, const OodleLZ_CompressOptions * pOptions)
{
	return Encode_LZB_Chunker(Encode_LZB_OneChunk_RLE,rawBuf,rawLen,compBuf,NULL,dictionaryBase,level,pOptions);
}
		
		
static SINTa Encode_LZB_Normal_New(const U8 * rawBuf, SINTa rawLen, U8 * compBuf, void * tmf, const U8 * dictionaryBase, OodleLZ_CompressionLevel level, const OodleLZ_CompressOptions * pOptions)
{
	return Encode_LZB_Chunker(Encode_LZB_OneChunk_Normal_New,rawBuf,rawLen,compBuf,tmf,dictionaryBase,level,pOptions);
}
		
OODLE_NS_END
