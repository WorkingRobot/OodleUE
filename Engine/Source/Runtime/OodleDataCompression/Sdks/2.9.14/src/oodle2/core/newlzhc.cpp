// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "cbradutil.h"
#include "oodlelzcompressors.h"
#include <stdlib.h>

#include "newlzhc.h"
#include "newlz.h"

#include "rrlz_getmatchlen.inl"
#include "rrprefetch.h"
#include "rrlzh_lzhlw_shared.h"

#include "rrvarbits.h"
#include "templates/rrvector_a.h"

#define RRVBC_INLINE
#include "rrvarbitcodes.h"
#include "rrvarbitcodes.cpp"

#include "ctmf.h"
#include "newlz_arrays.h"
#include "newlz_multiarrays.h"
#include "newlz_subliterals.h"
#include "newlz_complexliterals.h"
#include "newlz_vtable.h"
#include "newlz_arrays.inl"
#include "newlz_offsets.h"
#include "newlzhc_decoder.h"

#include "histogram.h"
#include "rrlogutil.h"
#include "rrarenaallocator.h"

#include "oodleconfigvalues.h"
#include "threadprofiler.h"

#include "newlz_speedfit.h"
#include "newlz_shared.h"
#include "log2table.h"
#include "speedfitter.h"
#include "matchfinder.h"

// SimpleProf (this is still a NOP unless explicitly turned on in CDep via -DOODLE_SIMPLEPROF_BUILD)
/*
#include "rrsimpleprof.h"
/*/
#include "rrsimpleprofstub.h"
/**/

#ifdef _MSC_VER
#pragma warning(disable : 4702) // unreachable
#endif

OODLE_NS_START

#if 0 && !defined(OODLE_BUILDING_DLL)
// for comptime lambda optimization :
extern double g_comptime_param;

#define NEWLZHC_LAMBDA_LEVIATHAN	g_comptime_param
#else
#define NEWLZHC_LAMBDA_LEVIATHAN	0.0025f
#endif

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


/****

newLZ-HC	- LEVIATHAN

****/
		
//=============================================================================

// can not define NEWLZHC_LRL_FOR_LONGER_MML to turn it off :
// @@@@ could use more thorough tweak ; 64 looks okay
//	decode_parse_inner checks for LRL >= 24 then unrolls big steps
//#define NEWLZHC_LRL_FOR_LONGER_MML	(24+32)

//=============================================================================

//#define NEWLZHC_OFF4M_MML		8
//#define NEWLZHC_OFF4M_MML		10

static RADINLINE bool newLZHC_IsAllowedNormalMatch_Optimal(int ml,int off,const OodleLZ_CompressOptions *pOptions)
{
	// -> no rules in optimal!
	//	just use the guided statistics from heuristic ?
	//  this is totally fine for compression ratio, but can cause decode speed dips
	//return true;
		
	if ( pOptions->farMatchOffsetLog2 > 0 && off >= (1<<pOptions->farMatchOffsetLog2) )
	{
		if ( ml < pOptions->farMatchMinLen )
		{
			return false;
		}
	}
	
	// DO still enforce some rules :
	// -> these are for decode speed , mainly on files like "webster"
	//		and mainly on slow-memory platforms like Jaguar & ARM
	// -> at typical cache sizes, require larger ML to go out of cache
	
	if ( off >= (1<<20) )// 1M
	{
		/*
		idea : like Mermaid ?
			another MML level for ESCAPE_OFFSET_MIN ?
			it's around 8M so it's not cache related
			it's just to avoid the 0xF0 offset escapes

		try to improve decode speed on "webster" and such

		maybe MML 10 ?
	
		// hurts compression a tiny bit and doesn't help decode speed
		if ( off >= ESCAPE_OFFSET_MIN ) // 8M
		{
			return ml >= 10;
		}
		else 
		*/
		
		if ( off >= (1<<22) ) // 4M
		{
			return ml >= 8;
		}
		else if ( off >= (1<<21) ) // 2M
		{
			return ml >= 6; // @@@@ not sure about this one
		}
		else
		{		
			return ml >= 5;
		}
	}
	
	return true;
}

static RADINLINE bool newLZHC_IsAllowedNormalMatch(int ml,int off,const OodleLZ_CompressOptions *pOptions)
{
	// no low offsets at all :
	//RR_ASSERT( off >= NEWLZ_MIN_OFFSET );
	RR_ASSERT( off >= 1 );
	RR_ASSERT( off < NEWLZ_MAX_OFFSET );
	RR_ASSERT( ml >= 3 );
	
	// strict rules :
	if ( ! newLZHC_IsAllowedNormalMatch_Optimal(ml,off,pOptions) )
		return false;
	
	// guiding rules :
	//  (these are strict in Normal parse
	//	 in optimal parse they are used for the pre-parse stats
	//	 but then not enforced in the final parse)

	// the idea here is :
	// for low match lens, require that offset is also low
	// don't take short matches with high offsets, we'd rather do literals

	// the exact tradeoff depends on the file
	// on highly compressible binary, these offsets are best off very low
	//	  (helps compression AND decode speed)
	// on text, they want to be higher (helps compression)
	
	// game testset can stand a pretty low ML4 offset max
	// hurts text-like (to lower the ML4 offset)
	// it *does* help speed (to lower this)

	// -> I could do better on text by raising these

	#define NEWLZHC_ML3_MAX_OFFSET	(1<<14) // 14 or 15
	#define NEWLZHC_ML4_MAX_OFFSET	(1<<17) // 17 or 18
	#define NEWLZHC_ML5_MAX_OFFSET	(1<<20) // 20 or 21


/*****************************************************************************************/

	if ( ml > 5 )
		return true;

	static const int max_offset[6] = { 0,0,0, NEWLZHC_ML3_MAX_OFFSET, NEWLZHC_ML4_MAX_OFFSET, NEWLZHC_ML5_MAX_OFFSET };
	return off < max_offset[ml];
}

static RADINLINE bool newLZHC_IsNormalMatchBetter(int ml,int off, int bestml, int bestoff)
{
	if ( ml < bestml ) return false;
	if ( ml == bestml ) return off < bestoff;
	if ( ml > bestml+1 ) return true;
	
	RR_ASSERT( ml == bestml+1 );
	
	return ( (off>>7) <= bestoff );
}

static RADINLINE bool newLZHC_IsLOMatchBetter(int repLen,int mainLen,int mainDist)
{
	if (repLen >= 2 && (
		(repLen + 1 >= mainLen) ||
		(repLen + 2 >= mainLen && mainDist >= (1 << 10)) ||
		(repLen + 3 >= mainLen && mainDist >= (1 << 16))))
	{
		return true;
	}
	else
	{
		return false;
	}
}

// 3,348,789 
// if newLZHC_LazyMatchDelta > 0 , do lazy1 , if > NEWLZHC_LAZYBETTER_LAZY2 , do lazy2
#define NEWLZHC_LAZYBETTER_LAZY2 3
static RADINLINE int newLZHC_LazyMatchDelta(int newMatchLen,U32 newOffset,bool newIsLast,
                int oldMatchLen,U32 oldOffset,bool oldIsLast)
{
	// this has to also ensure that the new match is allowed
	//if ( newMatchLen < 2+(newIsLast?0:1) ) return 0;
	if ( ! newMatchLen ) return 0;

	// warning : these parameters can be very easily over-trained to specific files
	//	they are parse-guiding , very approximate code cost

	// estimate of bits to send offset :
	U32 newOffsetLog2 = newIsLast ? 0 : 2+rrGetBitLevel_V(newOffset);
	U32 oldOffsetLog2 = oldIsLast ? 0 : 2+rrGetBitLevel_V(oldOffset);
	
	// was *4
	// 3,339,983
	// *6 = 10,370,844
	// lzt99 : 10,324,637
	// *3 = 10,299,507
	// 3,353,900
	int newGain,oldGain;
	
	if ( 0 ) // newIsLast ) // lzt99 likes *3 better
	{
		newGain = newMatchLen*3 - newOffsetLog2;
		oldGain = (oldMatchLen+1)*3 - oldOffsetLog2;
	}
	else // dickens likes *4 ; it's sort of like bits per symbol
	{
		newGain = newMatchLen*4 - newOffsetLog2;
		oldGain = (oldMatchLen+1)*4 - oldOffsetLog2;
	}
	
	return newGain - oldGain;
}

//=======================================================================================
/**

newLZHC_LOs contain one extra LO for the decoder
it puts the normal match offset in there
so it can be chosen branchlessly

decoder also carries *negative* offsets through the LO set

**/

struct newLZHC_PaddedLO_Storage
{
	RR_COMPILER_ASSERT( NEWLZHC_NUM_LAST_OFFSETS == 7 );
	// padding in front for slide-down and at end to simplify insert
	S32 contents[8 + 8];

	RADFORCEINLINE const S32& operator[](SINTa i) const { return contents[i + 8]; }
	RADFORCEINLINE S32& operator[](SINTa i) { return contents[i + 8]; }
};

// NOTE(fg): LO array crossing cache lines is a regular source of perf
// regressions, so just flag it as 64B aligned which is the size of padded
// storage, so this is enough to guarantee we always occupy the minimum
// number of cache lines
struct alignas(64) newLZHC_LOs
{
	newLZHC_PaddedLO_Storage lasts;
	
	RADFORCEINLINE void Reset()
	{
		for(int i=0;i<NEWLZHC_NUM_LAST_OFFSETS;i++)
			lasts[i] = NEWLZ_MIN_OFFSET;
	}
	
	RADFORCEINLINE void Reset_Neg()
	{
		for(int i=0;i<NEWLZHC_NUM_LAST_OFFSETS;i++)
			lasts[i] = - NEWLZ_MIN_OFFSET;
	}
	
	RADFORCEINLINE bool Equals(const newLZHC_LOs & rhs) const
	{
		for(int i=0;i<NEWLZHC_NUM_LAST_OFFSETS;i++)
		{
			if ( lasts[i] != rhs.lasts[i] )
				return false;
		}
		
		return true;	
	}
			
	RADFORCEINLINE S32 MTF(SINTr index)
	{
		// index can be 1 above last set for normal offset
		RR_ASSERT( index <= NEWLZHC_NUM_LAST_OFFSETS );
		
		// do the MTF :
		// NOTE(fg): alas, this is super-tweaky. be very careful in here!
		S32 * top_loc = lasts.contents + 8 + index;
		S32 top = *top_loc;
		S32 * from = top_loc - NEWLZHC_NUM_LAST_OFFSETS;

		// instead of 7 U32's moved from -> from+1
		// slide 8 U32's (from-1) -> from
		//  = 4 U64's or 2 vec128's :

		#if defined(__RADSSE2__)
		
		// meh, micro-faster than U64 load/stores
		// this is not really a "simd vector rep set"
		// by which I mean they stay in vector, and mtf via shuffle
		
		__m128i t1 = _mm_loadu_si128((__m128i *)(from-1));
		__m128i t2 = _mm_loadu_si128((__m128i *)(from+3));
		_mm_storeu_si128((__m128i *)(from),t1);
		_mm_storeu_si128((__m128i *)(from+4),t2);

		#elif defined(__RADNEON__)

		int32x4_t t1 = vld1q_s32(from - 1);
		int32x4_t t2 = vld1q_s32(from + 3);
		vst1q_s32(from, t1);
		vst1q_s32(from + 4, t2);
		
		#else
		
		U64 t1 = RR_GET64_NATIVE(from-1);
		U64 t2 = RR_GET64_NATIVE(from+1);
		U64 t3 = RR_GET64_NATIVE(from+3);
		U64 t4 = RR_GET64_NATIVE(from+5);
		RR_PUT64_NATIVE(from+0,t1);
		RR_PUT64_NATIVE(from+2,t2);
		RR_PUT64_NATIVE(from+4,t3);
		RR_PUT64_NATIVE(from+6,t4);
		
		#endif
		
		lasts.contents[8] = top;
		return top;
	}

	RADFORCEINLINE void Add(S32 offset)
	{
		RR_ASSERT( Find(offset) == -1 );
		
		// alternate :
		lasts[NEWLZHC_NUM_LAST_OFFSETS] = offset;
		MTF(NEWLZHC_NUM_LAST_OFFSETS);
	}
	
	RADFORCEINLINE int Find(S32 offset) const
	{
		RR_UNROLL_I_N(NEWLZHC_NUM_LAST_OFFSETS,0, if ( offset == (S32) lasts[i] ) { return i; } );
		return -1;
	}
	
	RADFORCEINLINE bool IsLastOff(S32 offset) const
	{
		return Find(offset) >= 0;
	}

	RADFORCEINLINE int Find_Update(S32 offset)
	{
		RR_UNROLL_I_N(NEWLZHC_NUM_LAST_OFFSETS, 0, if ( offset == (S32) lasts[i] ) { MTF(i); return i; } );
		Add(offset);
		return -1;
	}

	S32 LastOffset() const { return lasts[0]; }
};

struct newLZHC_LOs_NoPad
{
	S32 lasts[NEWLZHC_NUM_LAST_OFFSETS];
	
	S32 LastOffset() const { return lasts[0]; }
	
	void Reset()
	{
		for(int i=0;i<NEWLZHC_NUM_LAST_OFFSETS;i++)
			lasts[i] = NEWLZ_MIN_OFFSET;
	}
	
	int Find(S32 offset) const
	{
		RR_UNROLL_I_N(NEWLZHC_NUM_LAST_OFFSETS,0, if ( offset == (S32) lasts[i] ) { return i; } );
		return -1;
	}
		
	void Set(const newLZHC_LOs &rhs)
	{
		RR_UNROLL_I_N(NEWLZHC_NUM_LAST_OFFSETS, 0, lasts[i] = rhs.lasts[i] );
	}
	
	void CopyTo(newLZHC_LOs * pTo) const
	{
		RR_UNROLL_I_N(NEWLZHC_NUM_LAST_OFFSETS, 0, pTo->lasts[i] = lasts[i] );		
	}
	
	void SetMTF(const newLZHC_LOs_NoPad &rhs, int loi)
	{
		// @@ shitty
		newLZHC_LOs los;
		rhs.CopyTo(&los);
		los.MTF(loi);
		Set(los);
	}
	
	void SetAdd(const newLZHC_LOs_NoPad & rhs, S32 offset)
	{
		lasts[0] = offset;
		lasts[1] = rhs.lasts[0];
		lasts[2] = rhs.lasts[1];
		lasts[3] = rhs.lasts[2];
		lasts[4] = rhs.lasts[3];
		lasts[5] = rhs.lasts[4];
		lasts[6] = rhs.lasts[5];
		RR_COMPILER_ASSERT( NEWLZHC_NUM_LAST_OFFSETS == 7 );
	}
	
	bool Equals(const newLZHC_LOs_NoPad & rhs) const
	{
		RR_UNROLL_I_N(NEWLZHC_NUM_LAST_OFFSETS, 0, if ( lasts[i] != rhs.lasts[i] ) return false; );
		return true;
	}
	
};

#if 0 // NOTE(fg): nope, this works but is slower (on my machine anyway)

// This does the decode side only
// BitKnit-style
struct newLZHC_LOs_Indir
{
	RR_COMPILER_ASSERT( NEWLZHC_NUM_LAST_OFFSETS == 7 );
	S32 offsets[NEWLZHC_NUM_LAST_OFFSETS+1];
	U32 mtf_state;

	RADFORCEINLINE void Reset_Neg()
	{
		mtf_state = 0x76543210;
		for(int i=0;i<NEWLZHC_NUM_LAST_OFFSETS;i++)
			offsets[i] = - NEWLZ_MIN_OFFSET;
	}

	RADFORCEINLINE S32 MTF(SINTr index)
	{
		// index can be 1 above last set for normal offset
		RR_ASSERT( index <= NEWLZHC_NUM_LAST_OFFSETS );

		U32 index4 = 4 * static_cast<U32>(index);
		U32 slot_id = (mtf_state >> index4) & 7;
		S32 val = offsets[slot_id];

		// MTF
		U32 mask = (0u - 16) << index4;
		mtf_state = (mtf_state & mask) | (((mtf_state << 4) + slot_id) & ~mask);

		return val;
	}
	
	S32 LastOffset() const { return offsets[mtf_state & 7]; }
};

#define newLZHC_dec_LOs							newLZHC_LOs_Indir
#define newLZHC_dec_LOs_Reset_Neg(lasts)		lasts.Reset_Neg()
#define newLZHC_dec_LOs_MTF(lasts,index)		lasts.MTF(index)
#define newLZHC_dec_LOs_Add(lastoffsets,above)	lastoffsets.offsets[lastoffsets.mtf_state >> (NEWLZHC_NUM_LAST_OFFSETS*4)] = above

#else

#define newLZHC_dec_LOs							newLZHC_LOs
#define newLZHC_dec_LOs_Reset_Neg(lasts)		lasts.Reset_Neg()
#define newLZHC_dec_LOs_MTF(lasts,index)		lasts.MTF(index)
#define newLZHC_dec_LOs_Add(lastoffsets,above)	lastoffsets.lasts[NEWLZHC_NUM_LAST_OFFSETS] = above

#endif


//=============================================================================

struct newlzhc_encoder_parse
{
	S32 lastoffset;
	S32 lrl;
	S32 ml;
	S32 offset; // negative for LO
	bool IsLO() const { return offset <= 0; }
};

struct newlzhc_histo
{
	U32 total;
	U32 literals_raw[256];
	
	// no histo for literals_sub
	//	it's made by adding sub_lam + sub_later
	U32 literals_sub_lam[256];
	U32 literals_sub_later[256];

	U32 literals_o1[NEWLZ_O1_CONTEXT_COUNT][256];

	// no separate stats for literals_sub_and3 anymore
	// these are implied by sub_andF
	U32 literals_sub_andF[16][256]; // 16k bytes!
};

struct newlzhc_passinfo
{
	newlzhc_histo	literals;
	U32 literals_sub[256];
	/*
	int literal_mode_simple;
	int literal_mode_complex;
	int literal_mode_best;
	*/

	U32	packet_histo[256];
	U32	packet_pos_histo[NEWLZHC_PACKET_POS_COUNT][256];
	bool did_packet_pos;
	
	U32	excess_ml_histo[256];
	U32	excess_lrl_histo[256];
	
	int offset_alt_modulo; // == 0 for offsets44
	U32	offset_histo1[256];
	U32	offset_histo2[256]; // filled if modulo > 1
};


static void newLZHC_histo_literals( newlzhc_histo * histo, const U8 * chunk, SINTa pos, SINTa lrl, S32 lastoffset , S32 increment = 1)
{
	if ( lrl == 0 ) return;

	histo->total += (U32)lrl; // @@ * increment ?
		
	// i == 0
	{
		U8 lit = chunk[pos];
		U8 lam = chunk[pos - (SINTa)lastoffset];
		U8 sub = (U8)(lit - lam);
		histo->literals_raw[lit] += increment;
		histo->literals_sub_lam[sub] += increment;

		histo->literals_sub_andF[pos&0xF][sub] += increment;
		
		#ifdef NEWLZ_O1_CONTEXT_COUNT
		int o1cntx = newlz_o1_context( chunk[pos-1] );
		histo->literals_o1[ o1cntx ][lit] += increment;
		#endif
		
		pos++;
	}

	for(SINTa i=1;i<lrl;i++)
	{
		U8 lit = chunk[pos];
		histo->literals_raw[lit] += increment;	
		
		U8 lam = chunk[pos - (SINTa)lastoffset];
		U8 sub = (U8)(lit - lam);	
		histo->literals_sub_later[sub] += increment;

		histo->literals_sub_andF[pos&0xF][sub] += increment;
		
		#ifdef NEWLZ_O1_CONTEXT_COUNT
		int o1cntx = newlz_o1_context( chunk[pos-1] );
		histo->literals_o1[ o1cntx ][lit] += increment;
		#endif

		pos++;
	}
}

template <int t_literal_type>
static void newLZHC_parse_put_literals(
		const U8 * chunk,  SINTa chunk_len, SINTa start_pos,
		U8 * literals_ptr, SINTa literals_count,
		const vector_a<newlzhc_encoder_parse> & parsevec,
		S32 final_lo)
{
	RR_DURING_ASSERT( U8 * literals_end = literals_ptr + literals_count );
	SINTa cur_pos = start_pos;

	for (int i=0;i<parsevec.size32();i++)
	{
		const newlzhc_encoder_parse & parse = parsevec[i];
		
		int lrl = parse.lrl;
		if ( lrl != 0 )
		{
			const U8 * litptr = chunk + cur_pos;

			if ( t_literal_type == NEWLZ_LITERALS_TYPE_RAW )
			{
				memcpy(literals_ptr,litptr,lrl);
			}
			else if ( t_literal_type == NEWLZ_LITERALS_TYPE_SUB )
			{
				put_sub_literals(literals_ptr,litptr,lrl,parse.lastoffset);
			}
			else
			{
				RR_ASSERT( false );
			}

			literals_ptr += lrl;
		}
		cur_pos += lrl + parse.ml;
	}
	
	SINTa parse_end_pos = cur_pos;
	
	if ( ! parsevec.empty() )
	{	
		// weak final_lo validation :
		RR_DURING_ASSERT( const newlzhc_encoder_parse & last_parse = parsevec.back() );
		RR_ASSERT( final_lo == last_parse.offset || last_parse.IsLO() );
	}
	else
	{
		RR_ASSERT( final_lo == 8 );
	}
	
	SINTa final_lrl = chunk_len - parse_end_pos;
	RR_ASSERT( literals_ptr + final_lrl == literals_end );
	
	if ( final_lrl > 0 )
	{
		int lrl = (int)final_lrl;
		const U8 * litptr = chunk + parse_end_pos;
		
		if ( t_literal_type == NEWLZ_LITERALS_TYPE_RAW )
		{
			memcpy(literals_ptr,litptr,lrl);
		}
		else if ( t_literal_type == NEWLZ_LITERALS_TYPE_SUB )
		{
			put_sub_literals(literals_ptr,litptr,lrl,final_lo);
		}
		else
		{
			RR_ASSERT( false );
		}
		
		literals_ptr += lrl;		
	}
}

#if NEWLZ_LITERALS_TYPE_MAX > 1

template <int t_literal_type>
static void newLZHC_put_literals_multi(int lrl,U8 ** literals_ptr,SINTa lo,SINTa pos,const U8 * chunk)
{
#ifdef NEWLZ_LITERALS_TYPE_SUBAND3
	if ( t_literal_type == NEWLZ_LITERALS_TYPE_SUBAND3 )
	{
		while(lrl--)
		{
			*literals_ptr[pos&3] = (U8)( chunk[pos] - chunk[pos - lo] );
			literals_ptr[pos&3] ++;
			pos++;
		}
	}
	else 
#endif	
#ifdef NEWLZ_LITERALS_TYPE_SUBANDF
	if ( t_literal_type == NEWLZ_LITERALS_TYPE_SUBANDF )
	{
		while(lrl--)
		{
			*literals_ptr[pos&0xF] = (U8)( chunk[pos] - chunk[pos - lo] );
			literals_ptr[pos&0xF] ++;
			pos++;
		}
	}
	else 
#endif	
#ifdef NEWLZ_LITERALS_TYPE_LAMSUB
	if ( t_literal_type == NEWLZ_LITERALS_TYPE_LAMSUB )
	{
		// LAMSUB : array 1 is LAM, array 0 is later
		*literals_ptr[1] = (U8)( chunk[pos] - chunk[pos - lo] );
		literals_ptr[1] ++;
		pos++;
		lrl--;
		
		while(lrl--)
		{
			*literals_ptr[0] = (U8)( chunk[pos] - chunk[pos - lo] );
			literals_ptr[0] ++;
			pos++;
		}
	}
	else
#endif
#ifdef NEWLZ_LITERALS_TYPE_O1
	if ( t_literal_type == NEWLZ_LITERALS_TYPE_O1 )
	{		
		while(lrl--)
		{
			int c = newlz_o1_context( chunk[pos-1] );
			*literals_ptr[c] = chunk[pos];
			literals_ptr[c] ++;
			pos++;
		}
	}
	else
#endif
	{
		RR_CANT_GET_HERE();
	}
}

template <int t_literal_type>
static void newLZHC_parse_put_literals_multi( 
		const U8 * chunk,  SINTa chunk_len, SINTa start_pos,
		U8 ** literals_ptr, SINTa literals_count,
		const vector_a<newlzhc_encoder_parse> & parsevec,
		S32 final_lo )
{
	SINTa cur_pos = start_pos;
	for (int i=0;i<parsevec.size32();i++)
	{
		const newlzhc_encoder_parse & parse = parsevec[i];
		
		int lrl = parse.lrl;
		if ( lrl != 0 )
		{
			SINTa lo = parse.lastoffset;

			newLZHC_put_literals_multi<t_literal_type>(lrl,literals_ptr,lo,cur_pos,chunk);
		}

		cur_pos += lrl + parse.ml;
	}
	
	SINTa parse_end_pos = cur_pos;
	
	if ( ! parsevec.empty() )
	{	
		// weak final_lo validation :
		RR_DURING_ASSERT( const newlzhc_encoder_parse & last_parse = parsevec.back() );
		RR_ASSERT( final_lo == last_parse.offset || last_parse.IsLO() );
	}
	else
	{
		RR_ASSERT( final_lo == 8 );
	}
	
	SINTa final_lrl = chunk_len - parse_end_pos;
	//RR_ASSERT_ALWAYS( literals_ptr + final_lrl == literals_end );
	
	if ( final_lrl > 0 )
	{
		int lrl = (int)final_lrl;
		SINTa pos = parse_end_pos;
		SINTa lo = final_lo;
		
		newLZHC_put_literals_multi<t_literal_type>(lrl,literals_ptr,lo,pos,chunk);
	}
}

#endif // NEWLZ_LITERALS_TYPE_MAX

static int parse_to_packet(const newlzhc_encoder_parse & parse)
{
	bool islo = parse.offset <= 0;
	
	int lrl = parse.lrl;
	int lrl_for_packet = (lrl > NEWLZHC_PACKET_LRL_MAX) ? NEWLZHC_PACKET_LRL_MAX : lrl;
	
	int ml_to_send = parse.ml - NEWLZHC_LOMML;
	int ml_for_packet = (ml_to_send > NEWLZHC_PACKET_ML_MAX) ? NEWLZHC_PACKET_ML_MAX : ml_to_send;
	
	int packet = (lrl_for_packet<<NEWLZHC_PACKET_ML_BITS) + ml_for_packet;
		
	if ( islo )
	{
		// LO
		int loi = -parse.offset;
		RR_ASSERT( loi >= 0 && loi < NEWLZHC_NUM_LAST_OFFSETS );

		packet += loi << NEWLZHC_PACKET_OFFSET_SHIFT;
	}
	else
	{
		RR_ASSERT( parse.ml >= 3 );
	
		packet += NEWLZHC_NUM_LAST_OFFSETS << NEWLZHC_PACKET_OFFSET_SHIFT;
	}
	
	return packet;
}

//=======================================================================================

// my CTMF depths are shockingly low
//	that's okay on binary, hurts a lot on text

//#define SECOND_HASH_LEN	1	// 28,752,580
//#define SECOND_HASH_LEN	6	// 28,752,235
//#define SECOND_HASH_LEN	7	// 28,724,647
#define SECOND_HASH_LEN	8	// 28,643,159

//typedef CTMF<4,1,NEWLZHC_MML_NORMAL,0>	newLZHC_CTMF_Normal;
typedef CTMF<U32,2,SECOND_HASH_LEN,NEWLZHC_MML_NORMAL>	newLZHC_CTMF_Normal;

// lower depth + 2nd hash , or more depth?
//  2nd hash way is slower, more compression
//typedef CTMF<1,SECOND_HASH_LEN,NEWLZHC_MML_NORMAL,0>	newLZHC_CTMF_Fast;
typedef CTMF<U32,3,0,NEWLZHC_MML_NORMAL>	newLZHC_CTMF_Fast;

typedef CTMF<U32,1,0,NEWLZHC_MML_NORMAL>	newLZHC_CTMF_VeryFast;
typedef CTMF<U32,0,0,NEWLZHC_MML_NORMAL>	newLZHC_CTMF_SuperFast;

//=======================================================================================

template<typename newLZHC_CTMF>
static RADFORCEINLINE bool newLZHC_find_lo0_match_ahead(match *pmatch,
	newLZHC_CTMF * ctmf, const newLZHC_LOs & lastoffsets, const U8 * ptr, const U8 * ptr_matchend )
{
	S32 lastoffset = lastoffsets.lasts[0];

	if ( RR_GET32_NATIVE(ptr+1) != RR_GET32_NATIVE(ptr+1-lastoffset) )
		return false;

	S32 lo_ml = getmatchlen_after4(ptr+1,ptr+1-lastoffset,ptr_matchend);

	pmatch->ml = lo_ml;
	pmatch->off = 0;

	// update hash :
	RR_ASSERT( ptr == ctmf->m_next_ptr );
	U32 * row = ctmf->m_next_row;
	U32 hash = ctmf->m_next_hash;
	U32 * row2 = ctmf->m_next_row_second;
	//U32 hash2 = ctmf->next_hash_second;

	// where to prefetch? sets m_next which will be used in the partial insert
	//ctmf->prefetch_next(ptr+1+lo_ml); // end - no
	ctmf->prefetch_next(ptr+2); // inside?
	//ctmf->prefetch_next(ptr+1); // about the same as ptr+2

	U32 cur_absolute_pos = (U32)rrPtrDiff(ptr - ctmf->m_base_ptr);

	// now update the hash rows :
	ctmf->insert(row,cur_absolute_pos,hash);
	if ( newLZHC_CTMF::c_do_second_hash )
		ctmf->insert(row2,cur_absolute_pos,hash);

	return true;
}

template<typename newLZHC_CTMF>
static 
// a lot of function call overhead here; could forceinline?
RADFORCEINLINE 
bool newLZHC_get_match_heuristic_inline(match * pmatch,
	newLZHC_CTMF * ctmf, int step, const newLZHC_LOs & lastoffsets, const U8 * ptr, const U8 * ptr_matchend, int mml, const U8 * literals_start, U32 dictionarySize,
	const OodleLZ_CompressOptions *pOptions)
{
	U32 ptr32 = RR_GET32_NATIVE_UNALIGNED(ptr);

	RR_ASSERT( ptr == ctmf->m_next_ptr );
	U32 * row = ctmf->m_next_row;
	U32 hash = ctmf->m_next_hash;
	U32 * row2 = ctmf->m_next_row_second;
	//U32 hash2 = ctmf->next_hash_second;
	
	U32 cur_absolute_pos = (U32)rrPtrDiff(ptr - ctmf->m_base_ptr);
	
	ctmf->prefetch_next(ptr+step);
	
	// check LOs
	// only take higher LO index if the ML is greater
	// Single MAX reduction over (ml*8 + 6-index)
	// so that larger MLs win and tied MLs resolve
	// in favor of smaller LO indices
	RR_COMPILER_ASSERT(NEWLZHC_NUM_LAST_OFFSETS <= 7); // we use 7 to mark "none found"

	int lo_packed = 7; // lo_index=-1
	RR_UNROLL_I_N(NEWLZHC_NUM_LAST_OFFSETS,0, \
	{ \
		U32 offset = lastoffsets.lasts[i]; \
		int ml; \
		if ( havematch_mml2_one32(ptr32,ptr,ptr-offset,ptr_matchend,&ml) ) { \
			int packed = (ml << 3) + (NEWLZHC_NUM_LAST_OFFSETS - 1 - i); \
			lo_packed = RR_MAX(lo_packed, packed); \
		} \
	} );
	int lo_index = (NEWLZHC_NUM_LAST_OFFSETS - 1) - (lo_packed & 7);
	S32 lo_ml = lo_packed >> 3;

	// pretty aggressively just take LO if it's non-trivial
	//	don't even look for non-LO match
	#define NEWLZHC_LOML_GOOD_ENOUGH	4
	
	if ( lo_ml >= NEWLZHC_LOML_GOOD_ENOUGH )
	{
		// just take the LO match with no normal search :
		pmatch->ml = lo_ml;
		pmatch->off = -lo_index;
		
		// now update the hash rows :
		ctmf->insert(row,cur_absolute_pos,hash);
		if ( newLZHC_CTMF::c_do_second_hash )
			ctmf->insert(row2,cur_absolute_pos,hash);
	
		return true;
	}

	//---------------------------------------
					
	#ifdef NEWLZHC_LRL_FOR_LONGER_MML
	if ( ptr - literals_start >= NEWLZHC_LRL_FOR_LONGER_MML )
	{
		// try heuristic - if in long LRL bump up LO MML to 3 -> meh
		if ( lo_ml <= 2 ) lo_ml = 0;
		// try heuristic - if in long LRL bump up MML to 5 -> no good?
		if ( mml < 5 ) mml = 5;
	}
	#endif
			
	S32 bestml = 0;
	U32 bestoff = 0;
	
	// Find match candidates that pass tag bits check (first pass)
	RAD_ALIGN(U32, candidates[newLZHC_CTMF::c_table_depth * 2], 16);
	U32 candidate_mask = ctmf->find_candidates(candidates, row, row2, cur_absolute_pos, hash);

	// Verify match candidates (second pass)
	SINTa prevml = mml - 1; // this forces any matches we find to be >=mml
	while (candidate_mask)
	{
		SINTa i = rrCtz32(candidate_mask);
		candidate_mask &= candidate_mask - 1;

		// could do -NEWLZ_MIN_OFFSET here, and then +NEWLZ_MIN_OFFSET after
		// checking NEWLZ_MIN_OFFSET anyway
		U32 offset = candidates[i];
		if ( offset >= dictionarySize )
			continue;
		
		// force all offsets >= 8
		offset = RR_MAX(offset, NEWLZ_MIN_OFFSET);
		
		const U8 * vs_ptr = ptr - offset;
		
		SINTa len;
		if ( ! havematch_better(ptr,vs_ptr,ptr_matchend,prevml,&len) )
			continue;

		RR_ASSERT( len > prevml && len >= NEWLZHC_MML_NORMAL );
		
		prevml = len;
		
		const S32 len32 = (S32)len;
		if ( newLZHC_IsAllowedNormalMatch(len32,offset,pOptions) &&
			 ( ( newLZHC_CTMF::c_table_depth == 1 && !newLZHC_CTMF::c_do_second_hash ) || newLZHC_IsNormalMatchBetter(len32,offset,bestml,bestoff) ) )
		{
			bestml = len32;
			bestoff = offset;
		}
	}
	
	// now update the hash rows :
	ctmf->insert(row,cur_absolute_pos,hash);
	if ( newLZHC_CTMF::c_do_second_hash )
		ctmf->insert(row2,cur_absolute_pos,hash);

	if ( bestoff == 0 )
	{
		// no normal match
		if ( lo_ml >= NEWLZHC_LOMML )
		{
			pmatch->ml = lo_ml;
			pmatch->off = -lo_index;
			return true;
		}
		else
			return false;
	}
	else
	{
		// normal match + maybe rep match as well
		if ( newLZHC_IsLOMatchBetter(lo_ml,bestml,bestoff) )
		{
			// take the LO match :
			pmatch->ml = lo_ml;
			pmatch->off = -lo_index;
			return true;
		}
		else
		{
			RR_ASSERT( bestml == 0 || bestml >= NEWLZHC_MML_NORMAL );
			pmatch->ml = bestml;
			pmatch->off = bestoff;
			return true;
		}
	}
}

// NOTE: used in cases where inlining isn't worthwhile
template<typename newLZHC_CTMF>
static RADNOINLINE
bool newLZHC_get_match_heuristic(match * pmatch,
	newLZHC_CTMF * ctmf, const newLZHC_LOs & lastoffsets, const U8 * ptr, const U8 * ptr_matchend, int mml, const U8 * literals_start, U32 dictionarySize,
	const OodleLZ_CompressOptions *pOptions)
{
	return newLZHC_get_match_heuristic_inline(pmatch,ctmf,1,lastoffsets,ptr,ptr_matchend,mml,literals_start,dictionarySize,pOptions);
}

#define NEWLZHC_EXTRA_SCRATCH_MEM_FOR_FUZZ	64

static SINTa newLZHC_put_parse(
	newlz_encoder_scratch * scratch,
	F32 * pJ,
	newlzhc_histo * histo, int * pchunktype,
	const U8 * chunk_ptr,int chunk_len,U8 * comp,U8 * comp_end,
	SINTa chunk_pos,const newlz_vtable * vtable,
	S32 final_lo,
	const vector_a<newlzhc_encoder_parse> & parsevec,
	int start_pos,
	newlzhc_passinfo * passinfo,
	ENewLZ_MinOffset min_offset
	);

CHECK(static const U8 * check_buf);
	
	
//=============================================================================

// t_do_lazy_parse can be 1 or 2
template<typename newLZHC_CTMF,int t_do_lazy_parse,int t_do_match_backup,int t_step_literals_shift>
static SINTa newLZHC_encode_chunk(const newlz_vtable * vtable,
	newlz_encoder_scratch * scratch,
	const U8 * dictionaryBase,
	const U8 * chunk_ptr,int chunk_len,
	U8 * comp,U8 * comp_end,
	SINTa chunk_pos,
	int * pchunktype,
	F32 * pJ,
	const OodleKrakenChunkDeadlines *)
{
	*pchunktype = -1;
	
	if ( chunk_len <= NEWLZ_MIN_CHUNK_LEN )
	{
		return chunk_len;
	}

	SIMPLEPROFILE_SCOPE_N(encode_chunk,chunk_len);
	
	CHECK( check_buf = chunk_ptr );

	rrArenaAllocator * arena = scratch->arena;
	
	const OodleLZ_CompressOptions * pOptions = vtable->pOptions;
	U32 dictionarySize = ( pOptions->dictionarySize > 0 ) ?
		RR_MIN(NEWLZ_MAX_OFFSET,pOptions->dictionarySize)
		: NEWLZ_MAX_OFFSET;

	int mml = RR_MAX(pOptions->minMatchLen,NEWLZHC_MML_NORMAL);
		
	newLZHC_CTMF * ctmf = (newLZHC_CTMF *)vtable->matcher;

	vector_a<newlzhc_encoder_parse> parsevec;
	scratch->newlzhc_parsevec_space.extend(sizeof(newlzhc_encoder_parse)*chunk_len/2,arena);
	parsevec.provide_arena(scratch->newlzhc_parsevec_space.m_ptr,scratch->newlzhc_parsevec_space.m_size);
		
	int literals_plus_packets_limit = newlz_literal_space_reserve_size(chunk_len);
	scratch->literals_space.extend(literals_plus_packets_limit,arena);
	
	// ptr_matchend is the match *end* limit
	//	need -8 cuz we do blind U64 match copies :
	const U8 * ptr_matchend = chunk_ptr + chunk_len - NEWLZHC_MATCH_END_PAD;

	// parse_end_pos is the match *start* pos limit
	int parse_end_pos = chunk_len - NEWLZHC_CHUNK_NO_MATCH_ZONE;
	// @@@@ ?? is pos == parse_end_pos okay?
	
	newLZHC_LOs lastoffsets;
	lastoffsets.Reset();

	// start at pos 1 :
	int start_pos = 0;
	
	if ( chunk_pos == 0 )
	{
		// start literals at NEWLZ_MIN_OFFSET so sub doesn't have to check
		start_pos = NEWLZ_MIN_OFFSET;
	}

	const U8 * start_ptr = chunk_ptr + start_pos;
	const U8 * literals_start = start_ptr;
	
	// ctmf->set_next is safe here because of min chunk len check
	RR_ASSERT( start_pos < parse_end_pos );
	
	// start ctmf :
	ctmf->set_next(chunk_ptr+start_pos);

	const U8 * parse_end_ptr = chunk_ptr + parse_end_pos;
		
	// newlzhc_histo is quite big, don't put it on the stack
	//newlzhc_histo histo = { }; // zero init histo
	RR_SCOPE_ARENA(phisto,newlzhc_histo,arena);
	RR_ZERO(*phisto);

	{
	SIMPLEPROFILE_SCOPE_N(heuristic_parse,chunk_len);
		
	for(const U8 * ptr = start_ptr;;)
	{
		RR_ASSERT( ptr < parse_end_ptr );
		
		// literal loop: seek the next match
		match chosen;
		U32 scaled_skip_dist = 1 << t_step_literals_shift;

		// if t_step_literals_shift is on, t_do_lazy_parse must be off :
		RR_COMPILER_ASSERT( t_do_lazy_parse==0 || t_step_literals_shift==0 ); // not compatible

		for(;;)
		{
			RR_ASSERT( ctmf->m_next_ptr == ptr );

			if ( !t_do_lazy_parse )
			{
				if ( newLZHC_find_lo0_match_ahead(&chosen,ctmf,lastoffsets,ptr,ptr_matchend) )
				{
					ptr++;
					break;
				}
			}

			U32 step = t_step_literals_shift ? (scaled_skip_dist >> t_step_literals_shift) : 1;

			// if skipping that far would put us past the parse end, bail.
			// have to check this before get match because it does prefetch next
			if ( rrPtrDiff(parse_end_ptr - ptr) <= (SINTa)step )
				goto parse_chunk_done;

			if ( newLZHC_get_match_heuristic_inline(&chosen,ctmf,step,lastoffsets,ptr,ptr_matchend,mml,literals_start,dictionarySize,pOptions) )
				break;

			if ( t_step_literals_shift )
			{
				scaled_skip_dist++;
			}

			ptr += step;
		}

		// idea : don't do lazy if I have an LO0 match, cuz it puts a 0 lit
		// -> pretty meh and just random whether it helps or not
		//  // && chosen.off != 0 )
		// 10,305,575 -> 10,304,294
		// 3,345,935  ->  3,345,421
		// 57,602,299 -> 57,610,708
		if ( t_do_lazy_parse )
		{
			//SIMPLEPROFILE_SCOPE(lazy);
			
			// dickens -z4
			// lazy repeat : (while loop to look again)
			//  3,445,700
			//  3,420,080
			//  -> very small benefit
			// lazy2 : (look at +1 and +2)
			// 3,367,284 
			//  -> big benefit!
			
			while(ptr+1<parse_end_ptr)
			{
				// lazy parse
				
				match lazy;
				// what should prefetch step be for lazy parse?
				//	I guess 1 is good because we'll do an insert there when we advance
				if ( newLZHC_get_match_heuristic(&lazy,ctmf,lastoffsets,ptr+1,ptr_matchend,mml,literals_start,dictionarySize,pOptions) &&
					 newLZHC_LazyMatchDelta(lazy.ml,lazy.off,lazy.IsLO(),chosen.ml,chosen.off,chosen.IsLO()) > 0 )
				{
					// insert a literal :
					ptr++;
					// take lazy match :
					chosen = lazy;
				}
				else if ( t_do_lazy_parse >= 2 ) // lazy2 check
				{
					// lazy2
					if ( ptr+2 >= parse_end_ptr ) break;
					// don't do this if we might take an ml==2 step !
					if ( chosen.ml == 2 ) break;
				
					// IsLazy2Better : check newLZHC_LazyMatchDelta > 3 :
					// 0: 3,348,789 
					// 3: 3,344,832
					if ( newLZHC_get_match_heuristic(&lazy,ctmf,lastoffsets,ptr+2,ptr_matchend,mml,literals_start,dictionarySize,pOptions) &&
						 newLZHC_LazyMatchDelta(lazy.ml,lazy.off,lazy.IsLO(),chosen.ml,chosen.off,chosen.IsLO()) > NEWLZHC_LAZYBETTER_LAZY2 )
					{
						// insert a literal :
						ptr+=2;
						// take lazy match :
						chosen = lazy;
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
		// normal match is <parse_end_ptr ; lazy can be == parse_end_ptr
		RR_ASSERT( ptr <= parse_end_ptr );
		
		// have a match
		RR_ASSERT( chosen.ml >= NEWLZHC_MML_NORMAL || chosen.IsLO() );
		
		S32 offset = chosen.IsLO() ? lastoffsets.lasts[-chosen.off] : chosen.off;
		
		// verify the match is valid :	
		RR_ASSERT( memcmp(ptr,ptr-offset,chosen.ml) == 0 );

		if ( t_do_match_backup )
		{
			//SIMPLEPROFILE_SCOPE(match_backup);
			
			// try match start backup :
			//  @@ ideally this would be done per-match in the CTMF, not after selecting one
			while( ptr > literals_start )
			{
				if ( rrPtrDiff(ptr - ctmf->m_base_ptr) <= offset )
					break;
				if ( ptr[-1] == ptr[-1-(SINTa)offset] )
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
				
		// force a pure LO0-LRL0 exclusion
		// the only time this should happen is at the very start of the file
		// when pos == start_pos , we could match from the initial lastoffsets
		// in that case, just bump index from 0 to 1
		if ( chosen.off == 0 && ptr == literals_start )
		{
			RR_ASSERT( lastoffsets.lasts[0] == NEWLZ_MIN_OFFSET );
			RR_ASSERT( lastoffsets.lasts[1] == lastoffsets.lasts[0] );
			chosen.off = -1;
		}

		// make sure match backup didn't violate LO0LRL0 exclusion :
		RR_ASSERT( ptr > literals_start || chosen.off != 0 );
		
		int lrl = rrPtrDiff32( ptr - literals_start );
		
		newlzhc_encoder_parse cur_parse;
		cur_parse.lrl = lrl;
		cur_parse.ml = chosen.ml;
		cur_parse.offset = chosen.off;
		cur_parse.lastoffset = lastoffsets.LastOffset();
						
		parsevec.push_back(cur_parse);
		newLZHC_histo_literals(phisto, chunk_ptr, rrPtrDiff32(literals_start - chunk_ptr), lrl, lastoffsets.LastOffset() );
		
		// update lastoffsets :
		if ( chosen.IsLO() )
			lastoffsets.MTF(-chosen.off);
		else
			lastoffsets.Add(chosen.off);		
		
		// should only check this on the *last* chunk :
		ctmf->step_and_insert(ptr,chosen.ml);

		ptr += chosen.ml;
		literals_start = ptr;
		if ( ptr >= parse_end_ptr )
			break;
	}
	
parse_chunk_done:;
	} // profiler scope
	
	// write final sequence
	// trailing LRL :
	SINTa final_lrl = chunk_len - rrPtrDiff(literals_start - chunk_ptr);
	S32 final_lo = lastoffsets.LastOffset();
		
	if ( final_lrl > 0 )
	{
		newLZHC_histo_literals(phisto,chunk_ptr,rrPtrDiff32(literals_start - chunk_ptr),final_lrl,final_lo);
	}
		
	SINTa compLen = newLZHC_put_parse(scratch,pJ,phisto,pchunktype,chunk_ptr,chunk_len,comp,comp_end,chunk_pos,vtable,final_lo,parsevec,start_pos,NULL,eNewLZ_MinOffset_8);

	return compLen;
}


/*

newlzhc_estimate_array_complen is used to decide the literal mode
 (raw/sub/o1/lamsub/suband3)
 
this decision is made before we've decided to do huff or tans
or how to split/merge the entropy contexts

so it's not entirely accurate

to make a perfect decision, we'd have to try all the ways and do full optimization of each, then choose
which sucks
  (@@ but maybe this should be the way it's done at -z8)
	(just to see how much it helps)

so the idea is just use a log2(c) ; treat the coding as perfect, not bit-rounding like huffman
use a simple approximation of codelen transmission
this should be more accurate for TANS
also faster

*/

static U32 newlzhc_estimate_array_complen(const U32 * histo,int alphabet, U32 sumCounts)
{
	return newlz_array_estimate_complen_bytes(histo,alphabet,sumCounts);
}

static U32 newlzhc_estimate_array_complen(const U32 * histo,int alphabet)
{
	U32 sumCounts = rrSumOfHistogram(histo,alphabet); 
	
	return newlzhc_estimate_array_complen(histo,alphabet,sumCounts);
}

//=============================		

static F32 newlzhc_estimate_array_J(const U32 * histo,int alphabet,F32 lambda,const OodleSpeedFit * speedfit)
{
	U32 count = rrSumOfHistogram(histo,alphabet); 
	
	U32 len = newlzhc_estimate_array_complen(histo,alphabet,count);
	
	if ( count <= 1 ) return (F32)len;
	
	F32 time = speedfit_estimate_entropy_array_time(speedfit,count);
	F32 J = len + lambda * time;
	return J;
}

//=============================		

static SINTa newLZHC_put_parse(
	newlz_encoder_scratch * scratch,
	F32 * pJ,
	newlzhc_histo * histo, int * pchunktype,
	const U8 * chunk_ptr,int chunk_len,U8 * comp,U8 * comp_end,
	SINTa chunk_pos,const newlz_vtable * vtable,
	S32 final_lo,
	const vector_a<newlzhc_encoder_parse> & parsevec,
	int start_pos,
	newlzhc_passinfo * passinfo,
	ENewLZ_MinOffset min_offset)
{
	SIMPLEPROFILE_SCOPE_N(lzhc_put_parse,chunk_len);
                              
	F32 lambda = vtable->lambda;            	 
	U32 entropy_flags = vtable->entropy_flags;	
	OodleLZ_CompressionLevel level = vtable->level;
	const OodleSpeedFit * speedfit = vtable->speedfit;
	*pJ = LAGRANGE_COST_INVALID;
	
	/*
	// forbid edge case of zero packets :
	if ( parsevec.empty() )
	{
		// just let the outer all-huff case do this
		return chunk_len;	
	}
	*/
		
	SINTa literal_count = histo->total;
	
	int parse_size = parsevec.size32();
	int packet_count = parsevec.size32(); // same same
	
	//RR_ASSERT( parse_size > 0 );
		
	RR_ASSERT( rrSumOfHistogram(histo->literals_raw,256) == (U32)(histo->total) );
		
	int literals_plus_packets_limit = newlz_literal_space_reserve_size(chunk_len);

	rrArenaAllocator * arena = scratch->arena;
	
	scratch->literals_space.extend(literals_plus_packets_limit,arena);
	U8 * literal_space = scratch->literals_space.getU8();
	U8 * literal_space_end = literal_space + literals_plus_packets_limit;
		
	rrPrintf_v2("newLZHC_put_parse : %d lits\n",literal_count);
	
	// passinfo should have been zero'ed :
	// except passinfo->literals histo which can be == histo
	RR_ASSERT( passinfo == NULL || passinfo->offset_alt_modulo == 0 );
	
	//===========================================================
	
	U8 * comp_ptr = comp;
	
	// stuff first bytes raw :
	for(int i=0;i<start_pos;i++)
	{
		*comp_ptr++ = chunk_ptr[i];
	}
	
	//===========================================================
	
	F32 offsets_J = LAGRANGE_COST_INVALID;
	F32 excesses_J = LAGRANGE_COST_INVALID;	
	int offsets_count = 0;
	SINTa excesses_u8_count = 0;
	
	//	need space for offsets alt :
	//		3 offset arrays could all fit in literal_space
	//		max num offsets is (chunk_len/3) at MML 3
	SINTa max_num_offsets  = RR_MIN(parse_size,chunk_len/3);
	SINTa max_num_excesses = RR_MIN(parse_size*2,chunk_len/5);
	
	// *5 for U8 array and a U32 array
	//	alts done later
	SINTa scratch_space_needed =
		max_num_offsets * 5 +
		max_num_excesses * 5 + 16;
	
	// could reuse literal_space , but be simple , just alloc :
	RR_SCOPE_ARENA_ARRAY(scratch_space,U8,scratch_space_needed,arena);
	
	U8 * offsets_u8_space;
	U32 * offsets_u32_space;
	U32 * excesses_u32_space;
	SINTa excesses_u32_count = 0;
		
	int offset_alt_modulo = 0; // modulo = 0 means 44 offsets
	// always true in Leviathan :
	rrbool try_alt_offsets = true; //( vtable->bitstream_flags & NEWLZ_BITSTREAM_FLAG_ALT_OFFSETS );
	
			
	{
		SIMPLEPROFILE_SCOPE(lzhc_pp_offsets);
	
		//===========================================================
		// put offsets u8 with huff :
						
		RR_DURING_ASSERT( U8 * scratch_space_end = scratch_space + scratch_space_needed );
			
		
		offsets_u8_space = scratch_space;
		
		U8 * excesses_u8_space = scratch_space + max_num_offsets;
		U8 * excesses_u8_space_end = excesses_u8_space + max_num_excesses;
		
		// head and tail pointers for excesses :
		U8 * excesses_u8_ptr = excesses_u8_space;
		U8 * excesses_u8_back_ptr = excesses_u8_space_end;
		
		U8 * scratch_u32_ptr = excesses_u8_space_end;
		scratch_u32_ptr = rrAlignUpPointer(scratch_u32_ptr,4);
		offsets_u32_space = (U32 *) scratch_u32_ptr;
		excesses_u32_space = offsets_u32_space + max_num_offsets;
		U32 * excesses_u32_space_end = excesses_u32_space + max_num_excesses;
		U32 * excesses_u32_ptr = excesses_u32_space;
		U32 * excesses_u32_back_ptr = excesses_u32_space_end;
		 
		RR_ASSERT( (U8 *)excesses_u32_back_ptr < scratch_space_end );
		
		for(int s=0;s<parse_size;s++)
		{
			const newlzhc_encoder_parse & parse = parsevec[s];
				
			bool islo = parse.IsLO();
			
			if ( ! islo )
			{
				S32 off = parse.offset;
				RR_ASSERT( off >= min_offset && off < NEWLZ_MAX_OFFSET );
				RR_ASSERT( off >= 8 || parse.ml <= off );
				
				offsets_u32_space[offsets_count] = off;
				
				if ( min_offset != eNewLZ_MinOffset_1 )
				{
					offsets_u8_space[offsets_count] = newLZ_offset44_pack_u8(off);
				}
							
				offsets_count++;
			}				
			
			//===================================================
			// excesses :
			
			int lrl_excess = parse.lrl - NEWLZHC_PACKET_LRL_MAX;
			int ml_excess = parse.ml - NEWLZHC_LOMML - NEWLZHC_PACKET_ML_MAX;
			
			if ( lrl_excess >= 0 )
			{
				// LRL excesses at head :
				U8 x_u8 = U8_check( RR_MIN(lrl_excess,255) );
				*excesses_u8_ptr++ = x_u8;
				if ( lrl_excess >= 255 )
				{
					*excesses_u32_ptr++ = lrl_excess - 255;
				}
			}
			
			if ( ml_excess >= 0 )
			{
				// ML excess at tail :
				--excesses_u8_back_ptr;
				U8 x_u8 = U8_check( RR_MIN(ml_excess,255) );
				*excesses_u8_back_ptr = x_u8;
				if ( ml_excess >= 255 )
				{
					--excesses_u32_back_ptr;
					*excesses_u32_back_ptr = ml_excess - 255;
				}
			}
		}
		
		RR_ASSERT( excesses_u8_ptr <= excesses_u8_back_ptr );
		
		// condense excesses :
		SINTa num_excesses_lrl = excesses_u8_ptr - excesses_u8_space;
		SINTa num_excesses_ml = excesses_u8_space_end - excesses_u8_back_ptr;

		memmove(excesses_u8_ptr,excesses_u8_back_ptr,num_excesses_ml);	
				
		excesses_u8_count = num_excesses_lrl + num_excesses_ml;
		
		// lrl excesses at excesses_u8_space
		// ml excesses at excesses_u8_ptr
		
		RR_ASSERT( excesses_u32_ptr <= excesses_u32_back_ptr );
		
		// condense excesses :
		// subtract u32 pointers = count
		SINTa num_excesses_u32_lrl = excesses_u32_ptr - excesses_u32_space;
		SINTa num_excesses_u32_ml = excesses_u32_space_end - excesses_u32_back_ptr;

		memmove(excesses_u32_ptr,excesses_u32_back_ptr,num_excesses_u32_ml*sizeof(U32));	
				
		excesses_u32_count = num_excesses_u32_lrl + num_excesses_u32_ml;
		
		//=======================================================
		
		if ( passinfo )
		{
			CountHistoArrayU8(excesses_u8_space,num_excesses_lrl,passinfo->excess_lrl_histo,256,false);
			CountHistoArrayU8(excesses_u8_ptr,num_excesses_ml,passinfo->excess_ml_histo,256,false);
		}
		
		//=======================================================
		// put offsets U8

		SINTa offsets_comp_len = newLZ_put_offset_44_or_alt_arrays(comp_ptr,comp_end,
			offsets_u8_space,offsets_u32_space,offsets_count,
			entropy_flags,lambda,speedfit,&offsets_J,ARRAY_DEADLINE_HUGE,
			min_offset,try_alt_offsets,&offset_alt_modulo,
			arena,level,
			passinfo ? passinfo->offset_histo1 : NULL,
			passinfo ? passinfo->offset_histo2 : NULL
			);
		
		if ( offsets_comp_len < 0 )
			return -1;
		
		comp_ptr += offsets_comp_len;
		
		// offsets_J contains get_offsets time
		
		RR_ASSERT( offsets_J >= offsets_comp_len );
		RR_ASSERT( lambda > 0.f || offsets_J == offsets_comp_len );
		
		if ( passinfo )
			passinfo->offset_alt_modulo = offset_alt_modulo;
		
		rrPrintf_v2("offsets_count : %d, offsets_comp_len : %d , offset_alt_modulo = %d\n",
			offsets_count,offsets_comp_len,offset_alt_modulo);
		
		//=============================================================
		// excesses :
		
		SINTa excesses_u8_comp_len = newLZ_put_array_split2_hinted(
			comp_ptr,comp_end,excesses_u8_space,excesses_u8_count,
			entropy_flags,lambda,speedfit,&excesses_J,
			arena,level,NULL,
			num_excesses_lrl
			);

		if ( excesses_u8_comp_len < 0 )
			return chunk_len;
				
		comp_ptr += excesses_u8_comp_len;

		rrPrintf_v2("excesses_u8_comp_len : %d \n",excesses_u8_comp_len);
		
		excesses_J += lambda * speedfit->get_excesses(excesses_u8_count,excesses_u32_count);
		
		//=============================================================
		// verify that decoder will have enough scratch space :
		// -> this must be an over-estimate so that decoder will never fail to have space unexpectedly
			
		// space used during decoding :
		
		/**
		
		The maximum scratch use comes from
		
		all len 3 matches
		+ lrl 3 (causes an excess)
		
		packet_count = chunk_len/6
		literal_count = chunk_len/2
		offsets_count = packet_count
		excesses_u8_count = packet_count
		
		---------------
		
		during phase1 the space needed is
		
		literal_count + packet_count + offsets_count*6 + excesses_count*5
		+ literal_count again for indexed split
		+ NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH
		
		= l + 1/6 + 1 + 5/6 = 3*chunk_len + NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH
		
		= 432k = 442368
		
		**/
		
		// space for the retained arrays :
		SINTa retained_arrays_space_needed =
			literal_count + packet_count + 
			offsets_count*4 +
			excesses_u8_count*4 +
			OODLELZ_SCRATCH_ALIGNMENT_PAD;
		
		// retained arrays size maximum is 2*chunk_len
		RR_ASSERT( retained_arrays_space_needed <= 2*chunk_len + OODLELZ_SCRATCH_ALIGNMENT_PAD );
		
		// during phase1 we need the u8 arrays also :
		SINTa decoder_scratch_space_needed_arrays_phase1 =
			retained_arrays_space_needed +
			offsets_count*2 +
			excesses_u8_count;
		
		// (note that even though only the U32's of offsets & excesses are retained,
		//	we advanced the scratch pointer past the U8's , so we need that room)
		
		// up to NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH is used during array unpacking
		//	and double the size of the largest entropy array to account for indexed scratch
		//	-> the actual requirement here is weaker than this
		//		you only need 2* your array size at the point in scratch where you do your decode
		//		but F it this is simpler
		
		// find the largest working set needed :
		SINTa phase1_scratch_needed = decoder_scratch_space_needed_arrays_phase1 + RR_MAX4(offsets_count,excesses_u8_count,literal_count,packet_count);
		
		// the maximum of phase1 arrays + scratch space for indexed array = 3 * chunk_len
		RR_ASSERT( phase1_scratch_needed <= 3*chunk_len );
		
		phase1_scratch_needed += NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH; 
		
		// add chunk_len for fuzz padding during phase2 :
		SINTa phase2_scratch_needed = retained_arrays_space_needed + chunk_len;
		
		SINTa decoder_scratch_space_needed = RR_MAX(phase1_scratch_needed,phase2_scratch_needed);
		
		// some extra slack : (and newLZHC_chunk_arrays) (compiler asserted later)
		decoder_scratch_space_needed += OODLELZ_MAX_SCRATCH_FOR_PHASE_HEADERS_AND_FUZZ;
		
		SINTa min_scratch_avail = OodleLZ_Compressor_ScratchMemSize(vtable->compressor,chunk_len);

		RR_ASSERT( decoder_scratch_space_needed <= min_scratch_avail );
		if ( decoder_scratch_space_needed > min_scratch_avail )
		{
			// bail out, return expansion
			ooLogError("newLZ chunk needs too much scratch for decoder!\n");
			return chunk_len;
		}
		
		/*
		// find and log the maximum of decoder_scratch_space_needed :
		static SINTa max_decoder_scratch_space_needed = 0;
		max_decoder_scratch_space_needed = RR_MAX(max_decoder_scratch_space_needed,decoder_scratch_space_needed);
		rrprintfvar(max_decoder_scratch_space_needed);
		//rrprintfvar(min_scratch_avail);
		*/
		
		/**
				
		// lzt92 :
		
		decoder_scratch_space_needed	445616	__int64
		min_scratch_avail	442368	__int64
		
		literal_count	62802	__int64
		packet_count	22298	int
		offsets_count	22181	int
		excesses_u8_count	18446	__int64

		(float)(chunk_len-literal_count)/packet_count	3.0617095703650552	double

		this is almost entirely len 3 matches
		there's an offset AND an excess for almost every packet
		
		phase1_scratch_needed	422402	__int64
		phase2_scratch_needed	441520	__int64
		
		**/

	}
	
	//===================================================================
	
	// choose literal type :	
	F32 literals_J = LAGRANGE_COST_INVALID;
	F32 literals_uncompressed_J = 3 + (F32)literal_count; // no memcpy time
	// literals_uncompressed_J = uncompressed literal J
	//	confusing double-use of the term "raw"
	//	to mean non-sub huffman'ed, or just straight U8 uncompressed
	
	SINTa literal_comp_len = -1;
		
	if ( literal_count < NEWLZ_HUFF_ARRAY_MIN_SIZE )
	{
		// literal_space is empty or tiny ; put a NULL or raw array :
		
		*pchunktype = NEWLZ_LITERALS_TYPE_RAW;
	
		if ( literal_count > 0 )
		{
			newLZHC_parse_put_literals<NEWLZ_LITERALS_TYPE_RAW>(chunk_ptr,chunk_len,start_pos,
				literal_space,literal_count,parsevec,final_lo);
		}
			
		// @@ call newLZ_put_array_uncompressed here ?
		literal_comp_len = newLZ_put_array(comp_ptr,comp_end,literal_space,literal_count,entropy_flags,lambda,speedfit,&literals_J,ARRAY_DEADLINE_HUGE,arena,level,NULL);
		if ( literal_comp_len < 0 )
			return chunk_len;
		comp_ptr += literal_comp_len;
		
		literals_J = literals_uncompressed_J;
		//RR_ASSERT( literals_J == 3 );
		
		// passinfo for literals stays zero
	}
	else
	{
		SIMPLEPROFILE_SCOPE_N(lzhc_pp_literals,literal_count);
	
		// estimate J of all the literal types
		// pick the best one of "simple" {raw,sub}
		// pick the best one of "complex" {lamsub,o1,suband3,subandF}
		// put the best complex first
		// put the best simple on top of that (replaces if better)
		// compensate for parse time effect of different literal types
		//
		// the reason for doing both simple & complex is that sometimes
		//	the simple raw or sub comes out winning in the real put
		//	(due to array splits and so on)
		//	eg the real costs don't match the simple estimates
		// doing just the two classes simple&complex is a good approximation
		//	of the exact answer of just putting all types (which is slow)
				
		// make histo_literals_sub from sub_lam and sub_later	
		U32 histo_literals_sub[256];
		for(int i=0;i<256;i++)
		{
			histo_literals_sub[i] = histo->literals_sub_lam[i] + histo->literals_sub_later[i];
		}
		RR_ASSERT( rrSumOfHistogram(histo_literals_sub,256) == (U32)(histo->total) );
		
		if ( passinfo )
		{
			// saves histos for this pass
			// ("histo" object contains all classes except sub)
			// copy in the newlzhc_histo : (currently always ==)
			if ( histo != &(passinfo->literals) )
				memcpy(&(passinfo->literals) , histo, sizeof(*histo) );
			memcpy( passinfo->literals_sub , histo_literals_sub , sizeof(histo_literals_sub) );
		}
		
				
		U32 bytes_raw = newlzhc_estimate_array_complen(histo->literals_raw,256);
		U32 bytes_sub = newlzhc_estimate_array_complen(histo_literals_sub,256);
		
		// these J's are not the real J's, they only compare to eachother :
		
		// estimate of huff array overhead time (see newLZ_speedfit_huff_time)
		//	a whole entropy coding of the array time is assumed to be in all the J's
		//	so this is cost for subdivision into more arrays.
		//	eg. the per-sym cost is not changed, but the constant time is added
		//	so we pass len 0 to newLZ_speedfit_huff_time
		F32 J_entropy_array_time = lambda * speedfit_estimate_entropy_array_time(speedfit,0);
		// J_entropy_array_time = 13.7 bytes currently
		
		F32 J_sub = bytes_sub + J_entropy_array_time;
		F32 J_raw = bytes_raw + J_entropy_array_time;
				
		U32 bytes_lamsub_first = newlzhc_estimate_array_complen(histo->literals_sub_lam,256);
		U32 bytes_lamsub_later = newlzhc_estimate_array_complen(histo->literals_sub_later,256);
		
		U32 bytes_lamsub_total = bytes_lamsub_first + bytes_lamsub_later;
		
		F32 J_lamsub = bytes_lamsub_total + 2 * J_entropy_array_time;
		
		U32 bytes_suband3 = 0;
		for(int i=0;i<4;i++)
		{
			U32 histo_temp[256];
			const U32 * src0 = histo->literals_sub_andF[i];
			for (int j = 4; j < 16; j += 4)
			{
				simd_add_u32_256(histo_temp, src0, histo->literals_sub_andF[i + j]);
				src0 = histo_temp;
			}

			bytes_suband3 += newlzhc_estimate_array_complen(histo_temp,256);
		}
				
		F32 J_suband3 = bytes_suband3 + 4*J_entropy_array_time;
		
		U32 bytes_subandF = 0;
		for(int i=0;i<16;i++)
		{
			bytes_subandF += newlzhc_estimate_array_complen(histo->literals_sub_andF[i],256);
		}
				
		F32 J_subandF = bytes_subandF + 16*J_entropy_array_time;

		U32 bytes_o1 = 0;
		for(int i=0;i<NEWLZ_O1_CONTEXT_COUNT;i++)
		{
			bytes_o1 += newlzhc_estimate_array_complen(histo->literals_o1[i],256);
		}
				
		F32 J_o1 = bytes_o1 + NEWLZ_O1_CONTEXT_COUNT*J_entropy_array_time;
				
		//===============================
		
		// add parse times :
		
		J_sub += lambda * speedfit->parse_Leviathan_literals(literal_count,NEWLZ_LITERALS_TYPE_SUB);
		J_lamsub += lambda * speedfit->parse_Leviathan_literals(literal_count,NEWLZ_LITERALS_TYPE_LAMSUB);
		J_suband3 += lambda * speedfit->parse_Leviathan_literals(literal_count,NEWLZ_LITERALS_TYPE_SUBAND3);
		J_subandF += lambda * speedfit->parse_Leviathan_literals(literal_count,NEWLZ_LITERALS_TYPE_SUBANDF);
		J_o1 += lambda * speedfit->parse_Leviathan_literals(literal_count,NEWLZ_LITERALS_TYPE_O1);
		
		//===============================
		// try the best "complex" mode (multi-array)
		//	and the best "simple" mode
		// two ideas here :
		// 1. the complex mode may have seemed unattractive due to J overhead of many arrays
		//		but after merging it might be good
		// 2. if the complex mode merges a lot, may as well try the simpler mode instead
				
		F32 J_best_simple = RR_MIN(J_raw,J_sub);
		F32 J_best_complex = RR_MIN4(J_lamsub,J_suband3,J_subandF,J_o1);
						
		//===============================
		
		#ifdef SPEEDFITTING
		if ( g_speedfitter_stage == 3 )
		{
			J_best_simple = J_best_complex = RR_F32_MAX;
		
			switch(g_stage3_literal_mode)
			{
				case NEWLZ_LITERALS_TYPE_SUB: J_best_simple = J_sub; break;
				case NEWLZ_LITERALS_TYPE_RAW: J_best_simple = J_raw; break;
				case NEWLZ_LITERALS_TYPE_LAMSUB: J_best_complex = J_lamsub; break;
				case NEWLZ_LITERALS_TYPE_SUBAND3: J_best_complex = J_suband3; break;
				case NEWLZ_LITERALS_TYPE_O1: J_best_complex = J_o1; break;
				case NEWLZ_LITERALS_TYPE_SUBANDF: J_best_complex = J_subandF; break;
				case SPEEDFITTER_RAW_LITERALS_WITH_PACKETPOS: J_best_simple = J_raw; break;
				RR_NO_DEFAULT_CASE
			}
		}	
		#endif
				
		//===============================
		
		if ( min_offset == eNewLZ_MinOffset_1 )
		{
			// only raw & o1 allowed
			J_best_simple = J_raw;
			J_best_complex = J_o1;
			
			J_sub = J_lamsub = J_suband3 = J_subandF = LAGRANGE_COST_INVALID;
			
			// @@@@ TEMP HACKY - did a lot of work above to compute J's that I don't need
			//	but this way is the minimal code change & avoids duping for now
		}
		
		//===============================
		
		// 03-28-2019 :
		// only try complex modes if they seem likely to get any compression at all
		//	the goal is to skip work on incompressible files
		//	but not lose any compression when it is possible
		if ( J_best_complex < literals_uncompressed_J )
		{

		if ( J_best_complex == J_o1 )
		{
			SINTa num_literals_o1[NEWLZHC_O1_ARRAY_COUNT];

			for(int i=0;i<NEWLZ_O1_CONTEXT_COUNT;i++)
			{
				num_literals_o1[i] = rrSumOfHistogram(histo->literals_o1[i],256);
			}
						
			*pchunktype = NEWLZ_LITERALS_TYPE_O1;
			
			RR_ASSERT( sum(num_literals_o1,num_literals_o1+NEWLZHC_O1_ARRAY_COUNT,(SINTa)0) == (SINTa)literal_count );
	
			U8 * literals_arrays[NEWLZHC_O1_ARRAY_COUNT];
			U8 * literals_ptrs[NEWLZHC_O1_ARRAY_COUNT];
			U8 * literal_ptr = literal_space;
			for(int i=0;i<NEWLZHC_O1_ARRAY_COUNT;i++)
			{
				literals_arrays[i] = literal_ptr;
				literals_ptrs[i] = literal_ptr;
				literal_ptr += num_literals_o1[i];
			}
			RR_ASSERT( literal_ptr == literal_space + literal_count );

			rrPrintf_v2("NEWLZHC put lits o1 :");

			// NOTE : literals_ptr advanced !
			newLZHC_parse_put_literals_multi<NEWLZ_LITERALS_TYPE_O1>(chunk_ptr,chunk_len,start_pos,
				literals_ptrs,literal_count,parsevec,final_lo);

			literal_comp_len = newLZ_put_multiarray_indexed(comp_ptr,comp_end,literals_arrays,num_literals_o1,NEWLZHC_O1_ARRAY_COUNT,entropy_flags,
					lambda,speedfit,&literals_J,arena,level);
		}
		else if ( J_best_complex == J_suband3 )
		{
			SINTa num_literals_and3[4];
			for(int i=0;i<4;i++)
			{
				U32 sum = 0;
				for (int j = 0; j < 16; j += 4)
					sum += rrSumOfHistogram(histo->literals_sub_andF[i + j],256);


				num_literals_and3[i] = sum;
			}

			// do literals and3 :
			*pchunktype = NEWLZ_LITERALS_TYPE_SUBAND3;

			U8 * literals_arrays[4];
			U8 * literals_ptrs[4];
			U8 * literal_ptr = literal_space;
			for(int i=0;i<4;i++)
			{
				literals_arrays[i] = literal_ptr;
				literals_ptrs[i] = literal_ptr;
				literal_ptr += num_literals_and3[i];
			}
			RR_ASSERT( literal_ptr == literal_space + literal_count );

			rrPrintf_v2("NEWLZHC put lits suband3 :");
			
			// NOTE : literals_ptr advanced !
			newLZHC_parse_put_literals_multi<NEWLZ_LITERALS_TYPE_SUBAND3>(chunk_ptr,chunk_len,start_pos,
				literals_ptrs,literal_count,parsevec,final_lo);
		
			literal_comp_len = newLZ_put_multiarray_indexed(comp_ptr,comp_end,literals_arrays,num_literals_and3,4,
					entropy_flags,lambda,speedfit,&literals_J,arena,level);
		}
		else if ( J_best_complex == J_subandF )
		{
			SINTa num_literals_andF[16];
			for(int i=0;i<16;i++)
			{
				num_literals_andF[i] = rrSumOfHistogram(histo->literals_sub_andF[i],256);
			}

			// do literals andF :
			*pchunktype = NEWLZ_LITERALS_TYPE_SUBANDF;

			U8 * literals_arrays[16];
			U8 * literals_ptrs[16];
			U8 * literal_ptr = literal_space;
			for(int i=0;i<16;i++)
			{
				literals_arrays[i] = literal_ptr;
				literals_ptrs[i] = literal_ptr;
				literal_ptr += num_literals_andF[i];
			}
			RR_ASSERT( literal_ptr == literal_space + literal_count );

			rrPrintf_v2("NEWLZHC put lits subandF :");
			
			// NOTE : literals_ptr advanced !
			newLZHC_parse_put_literals_multi<NEWLZ_LITERALS_TYPE_SUBANDF>(chunk_ptr,chunk_len,start_pos,
				literals_ptrs,literal_count,parsevec,final_lo);
		
			literal_comp_len = newLZ_put_multiarray_indexed(comp_ptr,comp_end,literals_arrays,num_literals_andF,16,
				entropy_flags,lambda,speedfit,&literals_J,arena,level);
		}
		else if ( J_best_complex == J_lamsub )
		{
			// LAMSUB : array 1 is LAM, array 0 is later

			SINTa literal_count_lam = rrSumOfHistogram(histo->literals_sub_lam,256);
											
			*pchunktype = NEWLZ_LITERALS_TYPE_LAMSUB;

			SINTa literal_count_later = literal_count - literal_count_lam;
			RR_ASSERT( (U32)literal_count_later == rrSumOfHistogram(histo->literals_sub_later,256) );

			SINTa num_literals[2] = { literal_count_later, literal_count_lam };
			//U32 * histos[2] = { histo->literals_sub_later, histo->literals_sub_lam };
		
			U8 * literals_arrays[2];
			U8 * literals_ptrs[2];
			literals_ptrs[0] = literals_arrays[0] = literal_space;
			literals_ptrs[1] = literals_arrays[1] = literal_space + literal_count_later;

			rrPrintf_v2("NEWLZHC put lits lamsub :");
			
			// NOTE : literals_ptr advanced !
			newLZHC_parse_put_literals_multi<NEWLZ_LITERALS_TYPE_LAMSUB>(chunk_ptr,chunk_len,start_pos,
				literals_ptrs,literal_count,parsevec,final_lo);
		
			literal_comp_len = newLZ_put_multiarray_indexed(comp_ptr,comp_end,literals_arrays,num_literals,2,
				entropy_flags,lambda,speedfit,&literals_J,arena,level);
		}
		else
		{
			#ifndef SPEEDFITTING
			RR_ASSERT_FAILURE("wtf");
			#endif
		}
		
		rrPrintf_v2("%d\n",literal_comp_len);
			
		//===============
		// we have put the literals to comp_ptr in the best complex mode
		//	try again with the best simple mode
		// we can reuse literal_space here because the complex literals are already put
		//	output will write to comp only if J is better
		
		#ifdef SPEEDFITTING
		if ( literals_J == LAGRANGE_COST_INVALID )
		{
		}
		else
		#endif
		{
			// can be false if we hit comp_end due to expansion
			RR_ASSERT( literals_J != LAGRANGE_COST_INVALID || literal_comp_len == -1 );
			
			// add parse time :
			literals_J += lambda * speedfit->parse_Leviathan_literals(literal_count,*pchunktype);
		}

		} // did literals complex

		// literals_J includes parse time				
		RR_ASSERT( literals_J >= literal_comp_len );
		RR_DURING_ASSERT( F32 literals_J_complex = literals_J ); // @@ save J for debug
		
		if ( J_best_simple == J_sub )
		{
			rrPrintf_v2("NEWLZHC put lits sub :");
			
			newLZHC_parse_put_literals<NEWLZ_LITERALS_TYPE_SUB>(chunk_ptr,chunk_len,start_pos,
				literal_space,literal_count,parsevec,final_lo);
		
			F32 sub_literals_parse_time_J = lambda * speedfit->parse_Leviathan_literals(literal_count,NEWLZ_LITERALS_TYPE_SUB);
		
			// subtract off sub_literals_parse_time_J first
			//	so that we compare to previous giving it this edge
			literals_J -= sub_literals_parse_time_J;
		
			SINTa cur_literal_comp_len = newLZ_put_array_histo(comp_ptr,comp_end,literal_space,literal_count,
				const_cast<U32 *>(histo_literals_sub),entropy_flags,lambda,speedfit,&literals_J,ARRAY_DEADLINE_HUGE,arena,level
				);
			
			// add back on sub_literals_parse_time_J whether we changed it or not
			literals_J += sub_literals_parse_time_J;
			
			rrPrintf_v2("%d\n",cur_literal_comp_len);
		
			if ( cur_literal_comp_len > 0 )
			{
				RR_ASSERT( literals_J <= literals_J_complex );
			
				literal_comp_len = cur_literal_comp_len;
				*pchunktype = NEWLZ_LITERALS_TYPE_SUB;
				
				//if ( J_best_complex < J_best_simple ) rrprintf("reverted complex to simple\n");
			}
		}
		else if ( J_best_simple == J_raw )
		{
			rrPrintf_v2("NEWLZHC put lits raw :");
			
			newLZHC_parse_put_literals<NEWLZ_LITERALS_TYPE_RAW>(chunk_ptr,chunk_len,start_pos,
				literal_space,literal_count,parsevec,final_lo);
		
			SINTa cur_literal_comp_len = newLZ_put_array_histo(comp_ptr,comp_end,literal_space,literal_count,
				const_cast<U32 *>(histo->literals_raw),entropy_flags,lambda,speedfit,&literals_J,ARRAY_DEADLINE_HUGE,arena,level
				);
			
			// literals_J parse time for raw literals is zero
				
			rrPrintf_v2("%d\n",cur_literal_comp_len);
			
			if ( cur_literal_comp_len > 0 )
			{
				RR_ASSERT( literals_J <= literals_J_complex );
				
				literal_comp_len = cur_literal_comp_len;
				*pchunktype = NEWLZ_LITERALS_TYPE_RAW;
				
				//if ( J_best_complex < J_best_simple ) rrprintf("reverted complex to simple\n");
			}
		}
		else
		{
			// this else is used when speedfitting
			#ifndef SPEEDFITTING
			RR_ASSERT_FAILURE("wtf");
			#endif
		}
		
		// should have chosen something
		// if what we thought was best bailed out, try something else?
		// can be false if we expanded and hit comp_end, unable to put any literals
		// 
		//RR_ASSERT( literals_J < LAGRANGE_COST_INVALID );
		//RR_ASSERT( literal_comp_len > 0 );
						
		if ( ( literal_comp_len >= literal_count ||
			literals_J >= literals_uncompressed_J ) &&
			( *pchunktype != NEWLZ_LITERALS_TYPE_RAW
			 || literals_J != literals_uncompressed_J
			 || literal_comp_len != literal_count+3 ) )
		{
			// expanded
			// change to uncompressed raw (non-sub) literals for decode speed
			// no need to try huffman, put uncompressed
			//
			// note that newlz_put_array also has a bail to uncompressed internally
			// but we may have done something like put SUB literals
			//	 they expanded so were put uncompressed
			// that will have the exact same J and comp_len as we have here
			//	BUT we are switching to RAW (non-sub)
			//	which makes the decode faster
			
			// note it's possible that we got some compression before
			//	so literal_comp_len is currently == literal_count or literal_count+1 or +2
			// we will just throw that away here and kick it up to literal_count+3
			//	(this happens at -zs0)
			
			*pchunktype = NEWLZ_LITERALS_TYPE_RAW;
	
			newLZHC_parse_put_literals<NEWLZ_LITERALS_TYPE_RAW>(chunk_ptr,chunk_len,start_pos,
				literal_space,literal_count,parsevec,final_lo);
			
			literal_comp_len = newLZ_put_array_uncompressed(comp_ptr,comp_end,literal_space,literal_count );
		
			RR_ASSERT( literal_comp_len == literal_count+3 );
			literals_J = literals_uncompressed_J;
		}
		else
		{
			// chose something good, should be valid :
		
			RR_ASSERT( literals_J < LAGRANGE_COST_INVALID );
			RR_ASSERT( literal_comp_len > 0 );
		}
										
		if ( literal_comp_len < 0 ) // error ; can happen if we're expanding
			return chunk_len;
					
		comp_ptr += literal_comp_len;
		
		
		//rrprintfvar(literal_count);
		//rrprintfvar(literal_comp_len);
	}
	
	RR_ASSERT( literals_J <= literals_uncompressed_J );
	RR_ASSERT( literals_J >= literal_comp_len );
	RR_ASSERT( lambda > 0.f || literals_J == literal_comp_len );
	
	// literals_J contains parse time
	RR_ASSERT( literals_J >= literal_comp_len + lambda * speedfit->parse_Leviathan_literals(literal_count,*pchunktype) );
		
	//===========================================================
		
	//rrprintfvar(literal_count);
	//rrprintfvar(literal_comp_len);
	
	// currently required by encoder ; not required by decoder in theory :
	//RR_ASSERT( parse_size > 0 );
	//RR_ASSERT( packet_count > 0 );
	
	// put packets :
	F32 packet_J = LAGRANGE_COST_INVALID;
	
	{ // packet scope
	SIMPLEPROFILE_SCOPE_N(lzhc_pp_packets,packet_count);
	
	// put normal packets in literals space :
	U8 * packets_array = literal_space;
	if ( literal_space_end - literal_space < packet_count )
		return -1;
	
	// @@ need some scratch space for packets_pos
	SINTa space_per_packet_pos_array = (chunk_len+NEWLZHC_PACKET_POS_COUNT-1)/NEWLZHC_PACKET_POS_COUNT;
	//scratch->newlzhc_arrivals_space.extend(space_per_packet_pos_array*NEWLZHC_PACKET_POS_COUNT,arena);
	//U8 * packet_pos_space_ptr = scratch->newlzhc_arrivals_space.getU8();
	RR_SCOPE_ARENA_ARRAY(packet_pos_space,U8,(space_per_packet_pos_array*NEWLZHC_PACKET_POS_COUNT),arena);
	
	// could loop on NEWLZHC_PACKET_POS_BITS here, try 1 to 4
	//	though array merging sort of does that for me
	
	U8 * packet_pos_space_ptr = packet_pos_space;
	U8 * packet_pos_arrays[NEWLZHC_PACKET_POS_COUNT];
	U8 * packet_pos_ptrs[NEWLZHC_PACKET_POS_COUNT];
	
	for(int i=0;i<NEWLZHC_PACKET_POS_COUNT;i++)
	{
		packet_pos_arrays[i] = packet_pos_space_ptr; packet_pos_space_ptr += space_per_packet_pos_array;
		packet_pos_ptrs[i] = packet_pos_arrays[i];
	}	
	
	for(int cur_pos=start_pos, s=0;s<parse_size;s++)
	{
		const newlzhc_encoder_parse & parse = parsevec[s];
		
		int packet = parse_to_packet(parse);
		int packet_pos = cur_pos & NEWLZHC_PACKET_POS_MASK;
		
		packets_array[s] = U8_check(packet);
		
		U8 * ptr = packet_pos_ptrs[packet_pos];
		*ptr++ = U8_check(packet);
		packet_pos_ptrs[packet_pos] = ptr;

		cur_pos += parse.lrl + parse.ml;
	}
	
	SINTa packet_pos_comp_len = -1;

	bool try_packet_pos = true;	
	bool try_simple_packets = true;

	#ifdef SPEEDFITTING
	if ( g_speedfitter_stage == 3 )
	{
		try_packet_pos = (g_stage3_literal_mode == SPEEDFITTER_RAW_LITERALS_WITH_PACKETPOS);
		try_simple_packets = ! try_packet_pos;
	}
	#endif

	if ( try_packet_pos )
	{	
		SINTa packet_pos_lens[NEWLZHC_PACKET_POS_COUNT];
		
		for(int i=0;i<NEWLZHC_PACKET_POS_COUNT;i++)
		{
			packet_pos_lens[i] = rrPtrDiff( packet_pos_ptrs[i] - packet_pos_arrays[i] );
			
			if ( passinfo )
			{
				// could skip doing this until we set did_packet_pos = 1
				CountHistoArrayU8( packet_pos_arrays[i], packet_pos_lens[i],
					passinfo->packet_pos_histo[i], 256, false );
			}
		}	
		
		rrPrintf_v2("NEWLZHC put packets %d POS_BITS :",NEWLZHC_PACKET_POS_BITS);
				
		// flag byte : (@@ can I avoid this?)
		*comp_ptr = 0x80 | NEWLZHC_PACKET_POS_BITS;
		packet_pos_comp_len = newLZ_put_multiarray_indexed(
			comp_ptr+1,comp_end,packet_pos_arrays,packet_pos_lens,NEWLZHC_PACKET_POS_COUNT,
			entropy_flags,lambda,speedfit,&packet_J,arena,level);
			
		// packet_pos_comp_len of -1 can be returned if all were merged
		if ( packet_pos_comp_len == -1 )
		{
			RR_ASSERT( packet_J == LAGRANGE_COST_INVALID );
		}
		else
		{
			packet_J += lambda * speedfit->parse_Leviathan_packetpos(packet_count);
		}
		
		rrPrintf_v2("%d\n",packet_pos_comp_len);
		
		packet_pos_comp_len += 1; packet_J += 1; // for the flag byte
	}
	
	// packet_J contains packetpos parse-time
	
	SINTa packet_comp_len = -1;
	
	if ( try_simple_packets )
	{
		// put simple array on top : (will only write if it can beat J) :
		
		rrPrintf_v2("NEWLZHC put packets : ");
			
		packet_comp_len = newLZ_put_array(comp_ptr,comp_end,packets_array,packet_count,
			entropy_flags,lambda,speedfit,&packet_J,ARRAY_DEADLINE_HUGE,arena,level,NULL);
		
		rrPrintf_v2("%d -> %d\n",packet_count,packet_comp_len);
	}
	
	if ( passinfo )
	{
		RR_ASSERT( passinfo->packet_histo[77] == 0 );
	
		CountHistoArrayU8( packets_array,packet_count,
			passinfo->packet_histo, 256, false );
			
		passinfo->did_packet_pos = false;
	}
	
	if ( packet_comp_len < 0 )
	{
		// did not overwrite, take complen from multiarray :
		packet_comp_len = packet_pos_comp_len;
		
		if ( passinfo ) passinfo->did_packet_pos = true;
		
		if ( packet_pos_comp_len < 0 )
		{
			return chunk_len;
		}					
	}
	
	comp_ptr += packet_comp_len;	
	
	RR_ASSERT( packet_J != LAGRANGE_COST_INVALID );
	RR_ASSERT( packet_J >= packet_comp_len );
	RR_ASSERT( lambda > 0.f || packet_J == packet_comp_len );
	
	//rrprintfvar(packet_count);
	//rrprintfvar(packet_comp_len);
	
	}
	
	
	//=============================================================
	// put offsets and excesses varbits :
				
	SINTa varbits_comp_len = 0;
	
	U32 excesses_u32_hdr_size = 0;
	U32 excess_stream_bytes = 0;
	// @@ no separate excesses

	varbits_comp_len = newLZ_put_offsets_excesses(comp_ptr,comp_end,
		offsets_u8_space,
		offsets_u32_space,
		offsets_count,
		offset_alt_modulo,
		excesses_u32_space,
		(int)excesses_u32_count,
		excesses_u32_hdr_size,excess_stream_bytes);
		
	if ( varbits_comp_len < 0 )
		return chunk_len;
	
	//rrprintfvar(varbits_comp_len);
	
	comp_ptr += varbits_comp_len;
	
	rrPrintf_v2("varbits_comp_len : %d \n",varbits_comp_len);
		
	//===================================================================
	
	SINTa total_comp = rrPtrDiff( comp_ptr - comp );
	
	// if we expand then don't set J :
	if ( total_comp >= chunk_len )
		return chunk_len;
	
	F32 parse_time = speedfit->parse_Leviathan(chunk_len,packet_count,excesses_u8_count);
	
	SINTa parse_complen = varbits_comp_len + start_pos;
	
	F32 parse_J = parse_complen + lambda * parse_time;
	
	*pJ = literals_J + packet_J + offsets_J + excesses_J + parse_J;

	// excesses_J contains get_excesses time
	// offsets_J contains get_offsets time
	// literals_J contains literals parse time
	// packet_J contains packet-pos parse time

	RR_ASSERT( *pJ >= total_comp );
	RR_ASSERT( lambda > 0.f || *pJ == total_comp );

	rrPrintf_v2("total_comp : %d ; J = %.2f\n",total_comp,*pJ);
		
	return total_comp;
}

//================================================

#ifndef DO_SSE4_ALWAYS

#define NEWLZHC_DECODE_MULTIPACKET	0


#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_RAW
#define newLZHC_decode_parse	newLZHC_decode_parse_raw
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse	
#undef NEWLZHC_DECODE_LITERALS_TYPE			

#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_SUB
#define newLZHC_decode_parse	newLZHC_decode_parse_sub
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse	
#undef NEWLZHC_DECODE_LITERALS_TYPE			

#ifdef NEWLZ_LITERALS_TYPE_LAMSUB
#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_LAMSUB
#define newLZHC_decode_parse	newLZHC_decode_parse_lamsub
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse	
#undef NEWLZHC_DECODE_LITERALS_TYPE			
#endif // NEWLZ_LITERALS_TYPE_LAMSUB


#ifdef NEWLZ_LITERALS_TYPE_SUBAND3
#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_SUBAND3
#define newLZHC_decode_parse	newLZHC_decode_parse_suband3
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse	
#undef NEWLZHC_DECODE_LITERALS_TYPE			
#endif // NEWLZ_LITERALS_TYPE_SUBAND3

#ifdef NEWLZ_LITERALS_TYPE_O1
#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_O1
#define newLZHC_decode_parse	newLZHC_decode_parse_o1
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse	
#undef NEWLZHC_DECODE_LITERALS_TYPE			
#endif // NEWLZ_LITERALS_TYPE_O1

#ifdef NEWLZ_LITERALS_TYPE_SUBANDF
#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_SUBANDF
#define newLZHC_decode_parse	newLZHC_decode_parse_subandF
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse	
#undef NEWLZHC_DECODE_LITERALS_TYPE			
#endif // NEWLZ_LITERALS_TYPE_SUBANDF


//================================================	

#undef NEWLZHC_DECODE_MULTIPACKET
#define NEWLZHC_DECODE_MULTIPACKET	1


#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_RAW
#define newLZHC_decode_parse	newLZHC_decode_parse_multipacket_raw
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse	
#undef NEWLZHC_DECODE_LITERALS_TYPE			

#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_SUB
#define newLZHC_decode_parse	newLZHC_decode_parse_multipacket_sub
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse	
#undef NEWLZHC_DECODE_LITERALS_TYPE			

#ifdef NEWLZ_LITERALS_TYPE_LAMSUB
#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_LAMSUB
#define newLZHC_decode_parse	newLZHC_decode_parse_multipacket_lamsub
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse	
#undef NEWLZHC_DECODE_LITERALS_TYPE			
#endif // NEWLZ_LITERALS_TYPE_LAMSUB


#ifdef NEWLZ_LITERALS_TYPE_SUBAND3
#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_SUBAND3
#define newLZHC_decode_parse	newLZHC_decode_parse_multipacket_suband3
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse	
#undef NEWLZHC_DECODE_LITERALS_TYPE			
#endif // NEWLZ_LITERALS_TYPE_SUBAND3

#ifdef NEWLZ_LITERALS_TYPE_O1
#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_O1
#define newLZHC_decode_parse	newLZHC_decode_parse_multipacket_o1
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse	
#undef NEWLZHC_DECODE_LITERALS_TYPE			
#endif // NEWLZ_LITERALS_TYPE_O1

#ifdef NEWLZ_LITERALS_TYPE_SUBANDF
#define NEWLZHC_DECODE_LITERALS_TYPE	NEWLZ_LITERALS_TYPE_SUBANDF
#define newLZHC_decode_parse	newLZHC_decode_parse_multipacket_subandF
#include "newlzhc_decode_parse_outer.inl"
#undef newLZHC_decode_parse	
#undef NEWLZHC_DECODE_LITERALS_TYPE			
#endif // NEWLZ_LITERALS_TYPE_SUBANDF

static const newLZHC_decode_parse_func_set newLZHC_decode_parse_funcs_regular =
{
	newLZHC_decode_parse_raw,
	newLZHC_decode_parse_sub,
#ifdef NEWLZ_LITERALS_TYPE_LAMSUB
	newLZHC_decode_parse_lamsub,
#else
	NULL,
#endif
#ifdef NEWLZ_LITERALS_TYPE_SUBAND3
	newLZHC_decode_parse_suband3,
#else
	NULL,
#endif
#ifdef NEWLZ_LITERALS_TYPE_O1
	newLZHC_decode_parse_o1,
#else
	NULL,
#endif
#ifdef NEWLZ_LITERALS_TYPE_SUBANDF
	newLZHC_decode_parse_subandF,
#else
	NULL,
#endif

	newLZHC_decode_parse_multipacket_raw,
	newLZHC_decode_parse_multipacket_sub,
#ifdef NEWLZ_LITERALS_TYPE_LAMSUB
	newLZHC_decode_parse_multipacket_lamsub,
#else
	NULL,
#endif
#ifdef NEWLZ_LITERALS_TYPE_SUBAND3
	newLZHC_decode_parse_multipacket_suband3,
#else
	NULL,
#endif
#ifdef NEWLZ_LITERALS_TYPE_O1
	newLZHC_decode_parse_multipacket_o1,
#else
	NULL,
#endif
#ifdef NEWLZ_LITERALS_TYPE_SUBANDF
	newLZHC_decode_parse_multipacket_subandF,
#else
	NULL,
#endif
};

#endif // !DO_SSE4_ALWAYS

//================================================		

static RADINLINE int num_literal_arrays_from_type(int chunk_type )
{
	if ( chunk_type <= NEWLZ_LITERALS_TYPE_RAW ) // RAW AND SUB
	{
		// normal Kraken
		return 1;
	}
#ifdef NEWLZ_LITERALS_TYPE_LAMSUB
	else if ( chunk_type == NEWLZ_LITERALS_TYPE_LAMSUB )
	{
		return 2;
	}
#endif
#ifdef NEWLZ_LITERALS_TYPE_SUBAND3	
	else if ( chunk_type == NEWLZ_LITERALS_TYPE_SUBAND3 )
	{
		return 4;
	}
#endif
#ifdef NEWLZ_LITERALS_TYPE_SUBANDF	
	else if ( chunk_type == NEWLZ_LITERALS_TYPE_SUBANDF )
	{
		return 16;
	}
#endif
#ifdef NEWLZ_LITERALS_TYPE_O1
	else if ( chunk_type == NEWLZ_LITERALS_TYPE_O1 )
	{
		return NEWLZHC_O1_ARRAY_COUNT;
	}
#endif
	else
	{
		RR_CANT_GET_HERE();
		return 0;
	}
}

// newLZHC_decode_chunk_phase1 returns bool, but as 1/-1		
static SINTa newLZHC_decode_chunk_phase1(
	int chunk_type,
	const U8 * comp,const U8 * chunk_comp_end,
	U8 * chunk_ptr,SINTa chunk_len, SINTa chunk_pos, 
	U8 * const scratch_space,
	U8 * const scratch_space_end,
	newLZHC_chunk_arrays * arrays)
{
	//SIMPLEPROFILE_SCOPE_N(newLZHC_dec_phase1,chunk_len);
	
	if ( chunk_type > NEWLZ_LITERALS_TYPE_MAX )
		return -1;

	/*
	#ifndef __RADFUZZ__
	memset(scratch_space,-1,rrPtrDiff(scratch_end - scratch_space));
	#endif
	/**/
	
	newlz_array_get_printf("newlzhc chunk type %d = %s [%d->%d]\n",chunk_type,newlz_literals_type_name[chunk_type],chunk_len,rrPtrDiff32(chunk_comp_end-comp));
	
	// newlzhc always forces copy uncompressed
	//	so that all arrays go into scratch for fuzz :
	const bool force_copy_uncompressed = true;
	
	arrays->chunk_ptr = chunk_ptr;
	arrays->scratch_ptr = scratch_space;
	arrays->scratch_end = scratch_space_end; 
	U8 * scratch_ptr = scratch_space;
	U8 * scratch_end = scratch_space_end;
	
	//rrprintf("newLZHC_decode_chunk : %d\n",chunk_len);

	const U8 * comp_ptr = comp;
	
	// 4 array headers * 3 byte length = 12 bytes minimum
	//	+ offsets varbits 1 byte minimum = 13
	// (this also protects the first-8 raw byte read)
	if ( rrPtrDiff(chunk_comp_end - comp_ptr) < 13)
		return -1;
			
	//---------------------------------------------
	
	U8 * to_ptr = chunk_ptr;
	//U8 * chunk_end = to_ptr + chunk_len;
	
	//int start_pos = 0;
	// on first chunk, start at NEWLZ_MIN_OFFSET
	if ( chunk_pos == 0 )
	{
		//start_pos = NEWLZ_MIN_OFFSET;
		
		// in-place issue ; to_ptr should never overlap comp here because of in-place padding
		
		RR_COMPILER_ASSERT( NEWLZ_MIN_OFFSET == 8 );
		lz_copy8(to_ptr,comp_ptr);
		to_ptr += 8;
		comp_ptr += 8;
	}
	
	//-----------------------------------------------------
	
	// offsets & excesses u32's will be at the head of scratch :
	// literals & packets will follow after u32_space
	// 
	// the actual offset/excess varbits is decoded last of all
	//	because its complen is implicit (goes to end of chunk)

	/*
	Leviathan decoder scratch arrays layout :
	02-27-2019 reorg

	{
	offsets u32
	excesses u32
	literals
	packets
	[chunk_len padding after packets, can overlap u8 arrays]
	excesses u8 [only during phase1]
	offsets u8 [only during phase1]
	}
	*/

	// in-place issue :
	//	force_copy_uncompressed is not needed for offsets or excesses huffs
	//	because we decode them into the offsets/excesses u32 arrays, which live in scratch
	
	SINTa max_num_offsets = chunk_len/3;
	SINTa max_num_excesses = chunk_len/5;
	
	// we have checked OodleLZ_Compressor_ScratchMemSize
	// so we know scratch can hold the u8 offsets & excesses :
	//if ( (scratch_end - scratch_ptr) < (max_num_offsets*2 + max_num_excesses) ) return -1;
	//if ( (scratch_end - scratch_ptr) < chunk_len ) return -1;
	RR_ASSERT( rrPtrDiff(scratch_end - scratch_ptr) >= chunk_len );
		
	// get huff array of offsets_u8 :
	U8 * offsets_u8 = NULL;
	U8 * offsets_u8_2 = NULL;
	SINTa offsets_count = 0;
	S32 offset_alt_modulo = 0;
	{
	//SIMPLEPROFILE_SCOPE(get_array_offsetu8);
	
	// we use get_arraylens first to put offsets u8 at end of scratch :
	// @@ newLZ_get_arraylens dupes work with newLZ_get_array and isn't inline
	
	REQUIRE_FUZZ_RETURN( comp_ptr < chunk_comp_end , -1 );
	
	if ( *comp_ptr >= 0x80 ) // alt offset flag in top bit of offset entropy header
	{
		offset_alt_modulo = *comp_ptr - 0x80 + 1;
		comp_ptr++;
		
		if ( newLZ_get_arraylens(comp_ptr,chunk_comp_end,&offsets_count,max_num_offsets) < 0 ) return -1;

		RR_ASSERT( rrPtrDiff(scratch_end - scratch_ptr) >= offsets_count );
	
		offsets_u8 = scratch_end - offsets_count;
		scratch_end -= offsets_count;
		
		newlz_array_get_printf("offset alt modulo = %d\n",offset_alt_modulo);
		newlz_array_get_printf("offsets alt top : ");
		
		SINTa offsets_count1;
		SINTa offsets_u8_comp_len1 = newLZ_get_array(&offsets_u8,comp_ptr,chunk_comp_end,&offsets_count1,
			offsets_count,false,
			scratch_ptr,scratch_end);
		if ( offsets_u8_comp_len1 < 0 ) return -1;
		
		RR_ASSERT( offsets_count1 == offsets_count );
		
		newlz_array_get_printf("\n");
				
		comp_ptr += offsets_u8_comp_len1;
		
		if ( offset_alt_modulo > 1 )
		{
			RR_ASSERT( rrPtrDiff(scratch_end - scratch_ptr) >= offsets_count );
		
			offsets_u8_2 = scratch_end - offsets_count;
			scratch_end -= offsets_count;
		
			newlz_array_get_printf("offsets alt bot : ");
		
			SINTa offsets_count2 = 0;
			SINTa offsets_u8_comp_len2 = newLZ_get_array(&offsets_u8_2,comp_ptr,chunk_comp_end,&offsets_count2,
				offsets_count,false,
				scratch_ptr,scratch_end);
			if ( offsets_u8_comp_len2 < 0 ) return -1;
			
			newlz_array_get_printf("\n");
		
			if ( offsets_count != offsets_count2 ) return -1;
			
			comp_ptr += offsets_u8_comp_len2;
		}
	}
	else
	{
		newlz_array_get_printf("offsets 44 : ");
		
		if ( newLZ_get_arraylens(comp_ptr,chunk_comp_end,&offsets_count,max_num_offsets) < 0 ) return -1;

		RR_ASSERT( rrPtrDiff(scratch_end - scratch_ptr) >= offsets_count );
		
		offsets_u8 = scratch_end - offsets_count;
		scratch_end -= offsets_count;
		
		SINTa offsets_count1;
		SINTa offsets_u8_comp_len = newLZ_get_array(&offsets_u8,comp_ptr,chunk_comp_end,&offsets_count1,
			offsets_count,false,
			scratch_ptr,scratch_end);
		if ( offsets_u8_comp_len < 0 ) return -1;
		
		RR_ASSERT( offsets_count1 == offsets_count );
		
		newlz_array_get_printf("\n");
			
		comp_ptr += offsets_u8_comp_len;
	}
	
	}
	
	
	

	newlz_array_get_printf("excesses u8 : ");
			
	// get huff array of excesses_u8 :
	U8 * excesses_u8 = NULL;
	SINTa excesses_count = 0;
	{	
	if ( newLZ_get_arraylens(comp_ptr,chunk_comp_end,&excesses_count,max_num_excesses) < 0 ) return -1;

	RR_ASSERT( rrPtrDiff(scratch_end - scratch_ptr) >= excesses_count );
		
	excesses_u8 = scratch_end - excesses_count;
	scratch_end -= excesses_count;
	
	SINTa excesses_count1;
	SINTa excesses_u8_comp_len = newLZ_get_array(&excesses_u8,comp_ptr,chunk_comp_end,&excesses_count1,
		excesses_count,false,
		scratch_ptr,scratch_end);
	if ( excesses_u8_comp_len < 0 ) return -1;
	
	RR_ASSERT( excesses_count == excesses_count1 );
	
	comp_ptr += excesses_u8_comp_len;
	}
	
	newlz_array_get_printf("\n");

	// u8 offsets & excesses are now at the end of scratch
	//	(scratch_end pointer backed up so they can't be overwritten)
		
	//-----------------------------------------------------
	// reserve space in scratch for u32 offsets & excesses
	//	they go at the head of scratch

	// 16 byte alignment for SIMD :
	scratch_ptr = rrAlignUpPointer(scratch_ptr,16);
	
	S32 * offsets = (S32 *)scratch_ptr;
	U32 * excesses = (U32 *)(offsets + offsets_count);
	// 16 byte alignment for SIMD :
	excesses = rrAlignUpPointer(excesses,16);
	U32 * u32_space_end = excesses + excesses_count;
	// further scratch is after u32 space :
	scratch_ptr = (U8 *) u32_space_end;
		
	arrays->offsets = offsets;
	arrays->offsets_count = offsets_count;
	arrays->excesses = excesses;
	arrays->excesses_count = excesses_count;
		
	//---------------------------------------------
	// unpack the huff arrays of literals & packets :
	
	SINTa tot_literals_count;
		
	{
	//SIMPLEPROFILE_SCOPE(get_array_literals);

		// @@ num_literals_arrays is currently always 1 in Kraken
		//	multi-array types are disabled
		int num_literals_arrays = num_literal_arrays_from_type(chunk_type);				

		// in-place issue :
		//	force_copy_uncompressed is passed as inplace_comp_raw_overlap
		//	so that comp data is always copied out to scratch

		newlz_array_get_printf("literals : ");

		if ( num_literals_arrays == 1 )
		{
			U8 * literals = scratch_ptr; SINTa literals_count;
			SINTa literals_comp_len = newLZ_get_array(&literals,comp_ptr,chunk_comp_end,&literals_count,
										RR_MIN(chunk_len,rrPtrDiff(scratch_end-scratch_ptr)),
										force_copy_uncompressed,
										scratch_ptr,scratch_end);
			if ( literals_comp_len < 0 ) return -1;		
						
			comp_ptr += literals_comp_len;
				
			// scratch is advanced whether we used it or not :
			scratch_ptr += literals_count;
			
			arrays->literals_ptrs[0] = literals;
			arrays->literals_counts[0] = literals_count;
			tot_literals_count = literals_count;
		}
		else
		{
			SINTa literals_comp_len = newLZ_get_multiarray(
				comp_ptr,chunk_comp_end,
				scratch_ptr,scratch_end,
				arrays->literals_ptrs,
				arrays->literals_counts,
				num_literals_arrays,
				&tot_literals_count,
				force_copy_uncompressed,
				scratch_ptr,scratch_end);

			if ( literals_comp_len < 0 ) return -1;						
			if ( tot_literals_count > chunk_len ) return -1;
			
			comp_ptr += literals_comp_len;
				
			// scratch is advanced whether we used it or not :
			scratch_ptr += tot_literals_count;
		}
		
		newlz_array_get_printf("\n");
	}

	arrays->tot_literals_count = tot_literals_count;
	//rrprintfvar(literals_count);
	//rrprintfvar(literals_comp_len);
	
	newlz_array_get_printf("packets : ");
	
	SINTa tot_packets_count = 0;
	SINTa max_packets_count = chunk_len/2;
	
	REQUIRE_FUZZ_RETURN( comp_ptr < chunk_comp_end , -1 );
		
	if ( *comp_ptr >= 0x80 )
	{
		U8 header = *comp_ptr++;
		int packet_pos_bits = header & 0x7F;
		int num_packets_arrays = 1<<packet_pos_bits;
		// currently not actually variable :
		REQUIRE_FUZZ_RETURN( num_packets_arrays == NEWLZHC_PACKET_POS_COUNT , -1);
	
		SINTa packet_pos_counts[NEWLZHC_PACKET_POS_COUNT];
	
		SINTa packets_comp_len = newLZ_get_multiarray(
			comp_ptr,chunk_comp_end,
			scratch_ptr,scratch_end,
			arrays->packet_pos_ptrs,
			packet_pos_counts,
			num_packets_arrays,
			&tot_packets_count,
			force_copy_uncompressed,
			scratch_ptr,scratch_end);

		if ( packets_comp_len < 0 ) return -1;	
		if ( tot_packets_count > max_packets_count ) return -1;					

		// arrays->packet_pos_ptrs set
		for LOOP(i,NEWLZHC_PACKET_POS_COUNT)
		{
			arrays->packet_pos_ends[i] = arrays->packet_pos_ptrs[i] + packet_pos_counts[i];
		}

		comp_ptr += packets_comp_len;
			
		// scratch is advanced whether we used it or not :
		scratch_ptr += tot_packets_count;
	
		arrays->packets = NULL;
		arrays->packets_count = tot_packets_count;
	}
	else
	{
		U8 * packets = scratch_ptr;

		SINTa packets_comp_len = newLZ_get_array(&packets,comp_ptr,chunk_comp_end,&tot_packets_count,
			RR_MIN(max_packets_count,rrPtrDiff(scratch_end-scratch_ptr)),
			force_copy_uncompressed,
			scratch_ptr,scratch_end);
			
		if ( packets_comp_len < 0 ) return -1;
		
		comp_ptr += packets_comp_len;
							
		scratch_ptr += tot_packets_count;
					
		arrays->packets = packets;
		arrays->packets_count = tot_packets_count;
	}
	
	newlz_array_get_printf("\n");

	//=======================================================

	// "scratch_ptr" is now at the end of the retained arrays
	//	we must have at least chunk_len padding after that for phase2 fuzz
	// "scratch_end" has been backed up and contains the u8 offsets/exceses

	/*
	// verify that decoder_scratch_space_needed_arrays computation
	//	that the encoder did is correct (must be a strict upper bound)
	
	SINTa decoder_scratch_space_needed_arrays =
		tot_literals_count + tot_packets_count + 
		offsets_count*6 + 
		excesses_count*5 + 
		32; // + 32 for offset & excess alignment
	
	decoder_scratch_space_needed_arrays;
	// encoder ensured this for valid streams :
	RR_ASSERT_IF_NOT_CORRUPT( rrPtrDiff(scratch_ptr - scratch_space) <= decoder_scratch_space_needed_arrays );
	*/
	
	// on non-corrupt data the retained arrays are <= 2*chunk_len
	RR_ASSERT_IF_NOT_CORRUPT( rrPtrDiff( scratch_ptr - scratch_space ) <= 2*chunk_len );
	REQUIRE_FUZZ_RETURN( rrPtrDiff( scratch_ptr - scratch_space ) <= 2*chunk_len , -1 );
	
	// should be >= chunk_len of padding past end of the used scratch :
	// this is the scratch padding invariant required for fuzz safety
	//	the 2*chunk_len check above gaurantees this
	RR_ASSERT( rrPtrDiff( scratch_space_end - scratch_ptr ) >= chunk_len );
		
	//=======================================================
	
	// no separate excesses
	U8 excess_hdr_byte = 0;
	S32 excess_stream_bytes = 0;
	
	if ( newLZ_get_offsets_excesses(comp_ptr,chunk_comp_end,
		offsets_u8,offsets_u8_2,
		offsets_count,
		offset_alt_modulo,
		excesses_u8,excesses_count,
		offsets,excesses,
		chunk_pos+chunk_len,
		excess_hdr_byte,excess_stream_bytes) < 0 )
	{
		ooLogError("newLZ_get_offsets_excesses failed\n");
		return -1; // error	
	}
	
	//=======================================================		
	
	return 1;			
}

static RADINLINE SINTa newLZHC_decode_chunk_phase2(
	int chunk_type,
	U8 * chunk_ptr,SINTa chunk_len, SINTa chunk_pos, 
	const newLZHC_chunk_arrays * arrays)
{
	//SIMPLEPROFILE_SCOPE_N(newLZHC_dec_phase2,chunk_len);
	
	#ifdef SPEEDFITTING
	U64 t1 = speedfitter_ticks_start();
	#endif
	
	if ( chunk_type > NEWLZ_LITERALS_TYPE_MAX )
		return -1;
	
	int start_pos = 0;
	// on first chunk, start at NEWLZ_MIN_OFFSET
	if ( chunk_pos == 0 ) start_pos = NEWLZ_MIN_OFFSET;
	
	U8 * to_ptr = chunk_ptr + start_pos;
	U8 * chunk_end = chunk_ptr + chunk_len;	
		
	//=====================================================================
	// now parse !

	#if defined(DO_SSE4_ALWAYS)
	const newLZHC_decode_parse_func_set * parse = &newLZHC_decode_parse_funcs_sse4;
	#elif defined(DO_SSE4_TEST)
	const newLZHC_decode_parse_func_set * parse = rrsimd_has_sse4() ? &newLZHC_decode_parse_funcs_sse4 : &newLZHC_decode_parse_funcs_regular;
	#else
	const newLZHC_decode_parse_func_set * parse = &newLZHC_decode_parse_funcs_regular;
	#endif

	if ( arrays->packets != NULL )
	{

		if ( chunk_type == NEWLZ_LITERALS_TYPE_RAW )
		{
			bool res = parse->raw(
				arrays,
				to_ptr,chunk_ptr,chunk_end,chunk_ptr-chunk_pos);
			if (!res)
				return -1;
		}
		else if ( chunk_type == NEWLZ_LITERALS_TYPE_SUB )
		{
			bool res = parse->sub(
				arrays,
				to_ptr,chunk_ptr,chunk_end,chunk_ptr-chunk_pos);
			if (!res)
				return -1;
		}
	#ifdef NEWLZ_LITERALS_TYPE_LAMSUB
		else if ( chunk_type == NEWLZ_LITERALS_TYPE_LAMSUB )
		{
			bool res = parse->lamsub(
				arrays,
				to_ptr,chunk_ptr,chunk_end,chunk_ptr-chunk_pos);
			if (!res)
				return -1;
		}
	#endif	
	#ifdef NEWLZ_LITERALS_TYPE_SUBAND3	
		else if ( chunk_type == NEWLZ_LITERALS_TYPE_SUBAND3 )
		{
			bool res = parse->suband3(
				arrays,
				to_ptr,chunk_ptr,chunk_end,chunk_ptr-chunk_pos);
			if (!res)
				return -1;
		}
	#endif
	#ifdef NEWLZ_LITERALS_TYPE_SUBANDF
		else if ( chunk_type == NEWLZ_LITERALS_TYPE_SUBANDF )
		{
			bool res = parse->subandF(
				arrays,
				to_ptr,chunk_ptr,chunk_end,chunk_ptr-chunk_pos);
			if (!res)
				return -1;
		}
	#endif
	#ifdef NEWLZ_LITERALS_TYPE_O1	
		else if ( chunk_type == NEWLZ_LITERALS_TYPE_O1 )
		{
			bool res = parse->o1(
				arrays,
				to_ptr,chunk_ptr,chunk_end,chunk_ptr-chunk_pos);
			if (!res)
				return -1;
		}
	#endif	
		else
		{
			RR_ASSERT_FAILURE_ALWAYS("bad chunk_type");
		}

	}
	else
	{
	
		if ( chunk_type == NEWLZ_LITERALS_TYPE_RAW )
		{
			bool res = parse->multipacket_raw(
				arrays,
				to_ptr,chunk_ptr,chunk_end,chunk_ptr-chunk_pos);
			if (!res)
				return -1;
		}
		else if ( chunk_type == NEWLZ_LITERALS_TYPE_SUB )
		{
			bool res = parse->multipacket_sub(
				arrays,
				to_ptr,chunk_ptr,chunk_end,chunk_ptr-chunk_pos);
			if (!res)
				return -1;
		}
	#ifdef NEWLZ_LITERALS_TYPE_LAMSUB
		else if ( chunk_type == NEWLZ_LITERALS_TYPE_LAMSUB )
		{
			bool res = parse->multipacket_lamsub(
				arrays,
				to_ptr,chunk_ptr,chunk_end,chunk_ptr-chunk_pos);
			if (!res)
				return -1;
		}
	#endif	
	#ifdef NEWLZ_LITERALS_TYPE_SUBAND3	
		else if ( chunk_type == NEWLZ_LITERALS_TYPE_SUBAND3 )
		{
			bool res = parse->multipacket_suband3(
				arrays,
				to_ptr,chunk_ptr,chunk_end,chunk_ptr-chunk_pos);
			if (!res)
				return -1;
		}
	#endif
	#ifdef NEWLZ_LITERALS_TYPE_SUBANDF
		else if ( chunk_type == NEWLZ_LITERALS_TYPE_SUBANDF )
		{
			bool res = parse->multipacket_subandF(
				arrays,
				to_ptr,chunk_ptr,chunk_end,chunk_ptr-chunk_pos);
			if (!res)
				return -1;
		}
	#endif
	#ifdef NEWLZ_LITERALS_TYPE_O1	
		else if ( chunk_type == NEWLZ_LITERALS_TYPE_O1 )
		{
			bool res = parse->multipacket_o1(
				arrays,
				to_ptr,chunk_ptr,chunk_end,chunk_ptr-chunk_pos);
			if (!res)
				return -1;
		}
	#endif	
		else
		{
			RR_ASSERT_FAILURE_ALWAYS("bad chunk_type");
		}
	
	}

	#ifdef SPEEDFITTING
	
	if ( g_speedfitter_stage == 3 )
	{	
		U64 t2 = speedfitter_ticks_end();
		
		/*
		// gather stats:
		
		int offset_blend_score = 0;
		
		for LOOP(offi,arrays->offsets_count)
		{
			S32 neg_off = arrays->offsets[offi];
			RR_ASSERT( neg_off <= -8 );
			offset_blend_score += 1 + newLZ_offset_blend_score(-neg_off);
		}
		*/

		bool is_multipacket = ( arrays->packets == NULL );
		if ( is_multipacket )
		{
			RR_ASSERT( chunk_type == NEWLZ_LITERALS_TYPE_RAW );
			chunk_type = SPEEDFITTER_RAW_LITERALS_WITH_PACKETPOS;
		}

		speedfitter_stage3_collect_parse_newlzhc(chunk_ptr,(t2-t1),chunk_type,
			chunk_len,
			arrays->tot_literals_count,
			arrays->packets_count,
			arrays->excesses_count);
			//offset_blend_score);
	}

	#endif

	return 1;
}
				
/**

Thread phase1 : all independent per-block work
Kraken entropy decode
also uncompressed blocks & huff-only blocks

Thread phase2 : all dependent-on-previous-blocks work
also depends on the block's phase1, passed through the scratch mem
does the Kraken final parse

Unthreaded just does phase1+phase2

**/		
S32 Leviathan_DecodeOneQuantum(U8 * decomp,U8 * decomp_end,const U8 * comp,S32 quantumCompLen,const U8 * compBufEnd,SINTa pos_since_reset,
	void * scratch,SINTa scratch_size,OodleLZ_Decode_ThreadPhase threadPhase)
{
	SINTa decomp_len = rrPtrDiff( decomp_end - decomp );
	RR_ASSERT( decomp_len > 0 && decomp_len <= OODLELZ_BLOCK_LEN );
	decomp_len; // unused

	rrPrintf_v2("DBLOCK : %d : %d : %d\n",pos_since_reset,decomp_len,quantumCompLen);

	//SIMPLEPROFILE_SCOPE_N(newLZHC_decode,decomp_len);
	
	U8 * scratch_ptr = U8_void(scratch);
	U8 * scratch_end = scratch_ptr + scratch_size;
	
	U8 * rawPtr = decomp;
	U8 * rawEnd = decomp_end;
	const U8 * compPtr = U8_void(comp);
	const U8 * compEnd = compPtr + quantumCompLen;
	RR_ASSERT( compEnd <= compBufEnd );
	//const U8 * checkPtr = U8_void(checkbuf);

	// LZQH no flags set -> block len = 128k
	const int newlz_chunk_len = NEWLZHC_CHUNK_LEN;
	
	while(rawPtr<rawEnd)
	{
		SINTa chunk_len = RR_MIN( newlz_chunk_len , (rawEnd - rawPtr) );
		SINTa chunk_pos = rrPtrDiff( rawPtr - U8_void(decomp) ) + pos_since_reset;
		
		RR_ASSERT( chunk_len >= 1 && chunk_len <= NEWLZHC_CHUNK_LEN );
		
		// minimum length of a chunk is 4 bytes
		//	3 byte comp len header + 1 byte payload
		if ( 4 > rrPtrDiff( compEnd - compPtr ) )
			return -1;
		
		SINTa chunk_comp_len = RR_GET24_BE_OVERRUNOK(compPtr);
		
		if ( chunk_comp_len >= (1<<23) )
		{
			int chunk_type = (int)( (chunk_comp_len>>19)&0xF );
		
			chunk_comp_len &= (1<<19)-1; 
				// @@ <- this is one more bit than I actually need
				//  <- I could get antoehr bit for chunk_type here by using <<18
			//RR_ASSERT_ALWAYS( chunk_comp_len <= chunk_len );
			
			compPtr += 3;
			
			rrPrintf_v2("CHUNK : %d : %d\n",chunk_len,chunk_comp_len);

			if ( chunk_comp_len > compEnd - compPtr )
			{
				#if 0
				
				// force it to take truncated data :
				//  (for fuzz testing of the underlying compressor)
				
				chunk_comp_end = compEnd;
				chunk_comp_len = compEnd - compPtr;
				if ( chunk_comp_len <= 0 )
					return -1;
				
				#else
			
				return -1;
				
				#endif
			}

			const U8 * chunk_comp_end = compPtr + chunk_comp_len;

			if ( chunk_comp_len >= chunk_len )
			{			
				//raw 
				if ( chunk_comp_len > chunk_len ) return -1;
				if ( chunk_type != 0 ) return -1;
				
				if ( threadPhase & OodleLZ_Decode_ThreadPhase1 )
				{
					// has to be memmove for in-place decoding
					//memcpy(rawPtr,compPtr,chunk_len);
					memmove(rawPtr,compPtr,chunk_len);
				}
			}
			else
			{
				RR_ASSERT_IF_NOT_CORRUPT( chunk_len >= NEWLZ_MIN_CHUNK_LEN );

				// if this chunk is below the size where the encoder should've
				// bailed, don't accept it
				REQUIRE_FUZZ_RETURN( chunk_len >= NEWLZ_MIN_CHUNK_LEN, -1 );
			
				/*
				// changed 03-01-2019 : we DO now check scratch size
				
				// we do NOT at this point check if scratch_ptr is at least OodleLZ_Compressor_ScratchMemSize
				// -> should we ?				
				U8 * chunk_scratch_end = scratch_ptr + OodleLZ_Compressor_ScratchMemSize(OodleLZ_Compressor_Leviathan,chunk_len);
				chunk_scratch_end = RR_MIN(chunk_scratch_end,scratch_end); // @@ or test and fail ?
				
				newLZHC_chunk_arrays * arrays = (newLZHC_chunk_arrays *) scratch_ptr;
				scratch_ptr += sizeof(newLZHC_chunk_arrays);
			
				if ( scratch_ptr >= chunk_scratch_end ) return -1;
				*/
				
				// check scratch size meets decoder needs
				
				SINTa scratch_expected = OodleLZ_Compressor_ScratchMemSize(OodleLZ_Compressor_Leviathan,chunk_len);
						
				if ( rrPtrDiff(scratch_end - scratch_ptr) < scratch_expected )
				{
					ooLogError("Leviathan decoder scratch too small\n");
					return -1;
				}		
						
				U8 * chunk_scratch_end = scratch_ptr + scratch_expected;
				
				// arrays at head of scratch :
				newLZHC_chunk_arrays * arrays = (newLZHC_chunk_arrays *) scratch_ptr;
				scratch_ptr += sizeof(newLZHC_chunk_arrays);
			
				/**
				
				Kraken in-place decoding :
				
				force phase1 to put all its arrays in scratch
					(uncompressed arrays can't just point at comp)
				
				phase2 always fills raw buff from scratch
					Kraken does not read comp during phase2 at all
				
				**/

				if ( threadPhase & OodleLZ_Decode_ThreadPhase1 )
				{
					RR_ZERO(*arrays);
				
					if ( newLZHC_decode_chunk_phase1(chunk_type,compPtr,chunk_comp_end,rawPtr,chunk_len,chunk_pos,scratch_ptr,chunk_scratch_end,arrays) < 0)
					{
						// wipe out the newLZHC_chunk_arrays in scratch :
						arrays->chunk_ptr = NULL;
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
					RR_ASSERT( arrays->scratch_end == chunk_scratch_end );
					
 					if ( newLZHC_decode_chunk_phase2(chunk_type,rawPtr,chunk_len,chunk_pos,arrays) < 0)
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
					// put it back to base :
					scratch_ptr = U8_void(scratch);
				}
			}
		}
		else
		{
			SINTa literals_len;
			
			// huff-only chunk
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


/***=================================================

OPTIMAL PARSE

****/

#define NEWLZHC_MATCH_NUM_PAIRS	4	// @@@@ more for newlzhc ?

struct newLZHC_MatchParseRecord
{
	UnpackedMatchPair	pairs[NEWLZHC_MATCH_NUM_PAIRS];	
};

//=================================

//#define COST_PENALTY_SCALE	6
#define COST_PENALTY_SCALE	2	// lower for Leviathan ? (since lambda is lower)
// 1 vs 2 seems to be roughly even, 3 is slightly worse compression
//	(decode speed effect not measured)


// penalties to bias parse towards faster decodes
//  @@ -> not really tweaked, not finding a great win here
#define COST_PENALTY_PACKET			(3*COST_PENALTY_SCALE)  // penalty per packet; fewer packets = less mode switching
//#define COST_PENALTY_EXCESS		(2*COST_PENALTY_SCALE)	// excesses take time
#define COST_PENALTY_EXCESS_LRL		(1*COST_PENALTY_SCALE)	// at lrl >= 3
#define COST_PENALTY_EXCESS_ML		(3*COST_PENALTY_SCALE)	// at ml >= 9
#define COST_PENALTY_NORMAL_MATCH	(6*COST_PENALTY_SCALE)  // normal matches are slow (offset decoding)

//#define COST_PENALTY_NORMAL_CACHE	(COST_PENALTY_SCALE)	// penalty for offsets that go out of cache
//#define COST_PENALTY_NORMAL_CACHE	(COST_PENALTY_SCALE*2)
//#define COST_PENALTY_NORMAL_CACHE	(COST_PENALTY_SCALE*6)


/***

newLZHC_get_match_heuristic(newLZHC_MatchParseRecord) -
code dupe from newLZHC_get_match_heuristic(CTMF)

simple heuristic greedy parse to seed the optimal parse

-> NOTE : the heuristics here are what establishes our statistics baseline
->   this strongly guides the optimal parse

***/
static match newLZHC_get_match_heuristic(const newLZHC_MatchParseRecord & matches, const newLZHC_LOs & lastoffsets, const U8 * ptr, const U8 * ptr_matchend, int mml, int lrl,
	const U8 * dictionaryBase, U32 dictionarySize, ENewLZ_MinOffset min_offset,
	const OodleLZ_CompressOptions *pOptions)
{
	U32 ptr32 = RR_GET32_NATIVE_UNALIGNED(ptr);
	
	// check LOs
	// only take higher LO index if the ML is greater
	// Single MAX reduction over (ml*8 + 6-index)
	// so that larger MLs win and tied MLs resolve
	// in favor of smaller LO indices
	RR_COMPILER_ASSERT(NEWLZHC_NUM_LAST_OFFSETS <= 7);
	int lo_packed = 7; // lo_index = -1
	RR_UNROLL_I_N(NEWLZHC_NUM_LAST_OFFSETS,0, \
	{ \
		U32 offset = lastoffsets.lasts[i]; \
		int ml; \
		if ( havematch_mml2_one32(ptr32,ptr,ptr-offset,ptr_matchend,&ml) ) { \
			if ( offset < 8 ) ml = RR_MIN(ml,(int)offset); \
			int packed = (ml << 3) + (NEWLZHC_NUM_LAST_OFFSETS - 1 - i); \
			lo_packed = RR_MAX(lo_packed, packed); \
		} \
	} );
	int lo_index = (NEWLZHC_NUM_LAST_OFFSETS - 1) - (lo_packed & 7);
	S32 lo_ml = lo_packed >> 3;
	
	//  NOTE : this strongly biases the optimal parse statistics!
	if ( lo_ml >= NEWLZHC_LOML_GOOD_ENOUGH )
	{
		// just take the LO match with no normal search :
		match m = { lo_ml, -lo_index };
		
		return m;
	}
		
	#if 0 //def NEWLZHC_LRL_FOR_LONGER_MML
	if ( lrl >= NEWLZHC_LRL_FOR_LONGER_MML )
	{
		if ( lo_ml <= 2 ) lo_ml = 0;
	
		// normal mml up one :	
		mml ++;
	}
	#endif
	
	S32 bestml = 0;
	U32 bestoff = 0;
	
	for(int m=0;m<NEWLZHC_MATCH_NUM_PAIRS;m++)
	{
		S32 len = matches.pairs[m].length;
		
		// matches are in descending length
		// so when we hit a short one, get out
		if ( len < mml )
			break;
		
		if ( len > ptr_matchend - ptr )
		{
			len = rrPtrDiff32(ptr_matchend - ptr);
			if ( len < mml )
				break;
		}
		
		U32 offset = matches.pairs[m].offset;
		
		// verify match :
		RR_ASSERT( offset != 0 );
		RR_ASSERT( memcmp(ptr,ptr-(SINTa)offset,len) == 0 );
		
		if ( offset >= dictionarySize )
			continue;
				
		// force all offsets >= 8
		if ( offset < NEWLZ_MIN_OFFSET )
		{
			if ( min_offset == 1 )
			{
				// can do offset as is but must clamp len for no overlap
				
				len = RR_MIN(len,(S32)offset);
				
				if ( len >= mml && 
					newLZHC_IsAllowedNormalMatch(len,offset,pOptions) &&
				 newLZHC_IsNormalMatchBetter(len,offset,bestml,bestoff) )
				{
					bestml = len;
					bestoff = offset;
				}
				
				// drop through and also try striding up for overlap matches
			}
		
			offset = newLZ_offset44_round_up_tiny(offset);
			if ( (SINTa)offset > rrPtrDiff(ptr - dictionaryBase) )
				continue;

			const U8 * vs_ptr = ptr - offset;
			
			len = getmatchlen_mml3_one32(ptr32,ptr,vs_ptr,ptr_matchend);
						
			if ( len < mml )
				continue;
		}
		
		// to get rep0 exclusion I need matchlens to be maximal :
		RR_ASSERT( len >= ptr_matchend - ptr || ptr[len] != ptr[len-(SINTa)offset] );
		
		if ( newLZHC_IsAllowedNormalMatch(len,offset,pOptions) &&
		 newLZHC_IsNormalMatchBetter(len,offset,bestml,bestoff) )
		{
			bestml = len;
			bestoff = offset;
		}		
	}
	
	if ( newLZHC_IsLOMatchBetter(lo_ml,bestml,bestoff) )
	{
		// take the LO match :
		match m = { lo_ml, -lo_index };
		return m;
	}
	else
	{
		// normal match :
		match m = { bestml, (S32)(bestoff) };
		return m;
	}
}

struct newlzhc_optimal_arrival_tll
{
	S32	cost;
	newLZHC_LOs_NoPad los; // this is the los *after* my match, eg. the set for going forward when you start here
		// los[0] is the offset used in the packet for this arrival
	S32 ml,lrl; // prev pos is at -ml-lrl
		
	union
	{
	U32 is_twostep;
	struct {
		U32 lrl : 8;
		U32 ml : 24;
	} twostep;
	};
	
	S32 prev_index; // how to find your previous arrival (this is a tll index)	
		// prev_index could also be used for multi-parse
	
	
	#ifdef RR_DO_ASSERTS
	S32 check_offset;
		// this is the coding offset, equal to how parse.offset is store
		// check_offset <= 0 for LO ; if it's an lo index it's not relative to my los, it's in the *previous* los
		// offset is redundant, it's always equal to los[0]

	S32 check_arrival_pos; // @@ should be removable, just for asserting, can be computed
	// check_arrival_pos is the arrival pos of the full packet that made this TLL arrival
	//	eg. if tll == 0 , then check_arrival_pos = this pos
	//	but at higher tll , check_arrival_pos is behind by tll
	//	and at tll == last , it's behind by a variable amount (in the last_tll_len array)
	#endif
};

struct newlzhc_codecosts
{
	int chunktype;
	RR_COMPILER_ASSERT( NEWLZ_O1_CONTEXT_COUNT <= 16 );
	S32 codecost_literal[16][256];
	
	S32 codecost_packet[NEWLZHC_PACKET_POS_COUNT][256];
	U32 packet_pos_mask; // == 0 of packet pos was not used
	
	int offset_alt_modulo;
	S32 codecost_offset1[256];
	S32 codecost_offset2[256];
	
	S32 codecost_excess_ml[256];
	S32 codecost_excess_lrl[256];
};

static RADINLINE S32 newlzhc_cost_literals(
	const U8 * chunk,SINTa pos,int lrl,SINTa lo,
	const newlzhc_codecosts & codecosts,
	int prev_lrl)
{
	if ( lrl == 0 ) return 0;
	
	RR_ASSERT( lo >= 1 );
	RR_ASSERT( lo >= NEWLZ_MIN_OFFSET || newlz_literals_type_offset1_ok(codecosts.chunktype) );
	
	S32 ret = 0;
	
	switch(codecosts.chunktype)
	{
	case NEWLZ_LITERALS_TYPE_SUB:
		while(lrl--)
		{
			int sub_literal = (U8)(chunk[pos] - chunk[pos - lo]);
			ret += codecosts.codecost_literal[0][sub_literal];
			pos++;
		}
		break;
	
	case NEWLZ_LITERALS_TYPE_RAW:
		while(lrl--)
		{
			ret += codecosts.codecost_literal[0][chunk[pos]];
			pos++;
		}
		break;
	
	case NEWLZ_LITERALS_TYPE_LAMSUB:
		{
		
		RR_ASSERT( lrl >= 1 );
		
		if ( prev_lrl == 0 )
		{
			lrl--;
			int sub_literal = (U8)(chunk[pos] - chunk[pos - lo]);
			ret += codecosts.codecost_literal[0][sub_literal];
			pos++;
		}
		
		while(lrl--)
		{
			int sub_literal = (U8)(chunk[pos] - chunk[pos - lo]);
			ret += codecosts.codecost_literal[1][sub_literal];
			pos++;
		}
		
		}
		break;
	
	case NEWLZ_LITERALS_TYPE_SUBAND3:
		while(lrl--)
		{
			int sub_literal = (U8)(chunk[pos] - chunk[pos - lo]);
			ret += codecosts.codecost_literal[pos&3][sub_literal];
			pos++;
		}
		break;
	
	case NEWLZ_LITERALS_TYPE_O1:
		while(lrl--)
		{
			int c = newlz_o1_context( chunk[pos-1] );
			ret += codecosts.codecost_literal[c][chunk[pos]];
			pos++;
		}
		break;
	
	case NEWLZ_LITERALS_TYPE_SUBANDF:
		while(lrl--)
		{
			int sub_literal = (U8)(chunk[pos] - chunk[pos - lo]);
			ret += codecosts.codecost_literal[pos&0xF][sub_literal];
			pos++;
		}
		break;
	}	
	return ret;	
}

static RADINLINE S32 cost_add_literal(
	const U8 * chunk,SINTa pos,SINTa lo,
	const newlzhc_codecosts & codecosts,
	int prev_lrl)
{
	RR_ASSERT( lo >= 1 );
	RR_ASSERT( lo >= NEWLZ_MIN_OFFSET || newlz_literals_type_offset1_ok(codecosts.chunktype) );
	
	// single literal
	
	S32 ret = 0;
	
	switch(codecosts.chunktype)
	{
	case NEWLZ_LITERALS_TYPE_SUB:
		{
			int sub_literal = (U8)(chunk[pos] - chunk[pos - lo]);
			ret = codecosts.codecost_literal[0][sub_literal];
		}
		break;
	
	case NEWLZ_LITERALS_TYPE_RAW:
		{
			ret = codecosts.codecost_literal[0][chunk[pos]];
		}
		break;
	
	case NEWLZ_LITERALS_TYPE_LAMSUB:
		{
		
		int sub_literal = (U8)(chunk[pos] - chunk[pos - lo]);
			
		int index = ( prev_lrl == 0 ) ? 0 : 1;		
		ret = codecosts.codecost_literal[index][sub_literal];
		
		}
		break;
	
	case NEWLZ_LITERALS_TYPE_SUBAND3:
		{
			int sub_literal = (U8)(chunk[pos] - chunk[pos - lo]);
			ret = codecosts.codecost_literal[pos&3][sub_literal];
		}
		break;
	
	case NEWLZ_LITERALS_TYPE_O1:
		{
			int c = newlz_o1_context( chunk[pos-1] );
			ret = codecosts.codecost_literal[c][chunk[pos]];
		}
		break;
	
	case NEWLZ_LITERALS_TYPE_SUBANDF:
		{
			int sub_literal = (U8)(chunk[pos] - chunk[pos - lo]);
			ret = codecosts.codecost_literal[pos&0xF][sub_literal];
		}
		break;
	}	
	
	return ret;	
}

// very cold path, force into separate fn so it's not inlined everywhere
static RADNOINLINE S32 cost_excess_huge(int excess, const S32 * codecosts)
{
	RR_ASSERT(excess >= 255);
	S32 ret = codecosts[255]; // excess code
	int bits = rrVarBits_CountBits_ExpGolomb(excess - 255,EXCESS_EXPGOLOMB_SHIFT);
	ret += COST_ONE_BIT * bits;
	//ret += COST_PENALTY_EXCESS*2; // extra penalty for excess of excess ?
	return ret;
}

static RADFORCEINLINE S32 cost_excess_lrl(int excess,
	const newlzhc_codecosts & codecosts)
{
	if ( excess < 255 )
	{
		return codecosts.codecost_excess_lrl[excess];
	}
	else
		return cost_excess_huge(excess, codecosts.codecost_excess_lrl);
}
	
static RADFORCEINLINE S32 cost_excess_ml(int excess,
	const newlzhc_codecosts & codecosts)
{
	if ( excess < 255 )
	{
		return codecosts.codecost_excess_ml[excess];
	}
	else
		return cost_excess_huge(excess, codecosts.codecost_excess_ml);
}

static RADINLINE S32 cost_offset(int offset,const newlzhc_codecosts & codecosts)
{
	RR_ASSERT( offset >= 1 && offset <= NEWLZ_MAX_OFFSET );

	S32 cost = 0;
	
	if ( codecosts.offset_alt_modulo == 0 )
	{
		RR_ASSERT( offset >= NEWLZ_MIN_OFFSET );
	
		U8 offset_u8 = newLZ_offset44_pack_u8(offset);
		U32 num_raw_bits = newLZ_offset44_unpack_numrawbits(offset_u8);
		
		cost += codecosts.codecost_offset1[offset_u8];
		cost += num_raw_bits * COST_ONE_BIT;
		
		if ( offset >= ESCAPE_OFFSET_MIN )
		{
			// cost speed penalty
			//cost += COST_PENALTY_EXCESS;
			cost += COST_PENALTY_NORMAL_MATCH;
		}
	}
	else if ( codecosts.offset_alt_modulo == 1 )
	{
		U8 offset_u8 = newLZ_alt_offset_pack_u8_modulo1(offset);
		U32 num_raw_bits = newLZ_alt_offset_unpack_numrawbits(offset_u8);
		
		cost += codecosts.codecost_offset1[offset_u8];
		cost += num_raw_bits * COST_ONE_BIT;
	}
	else
	{
		U8 top,bot;
		newLZ_alt_offset_pack_u8s(offset,&top,&bot,codecosts.offset_alt_modulo);
		U32 num_raw_bits = newLZ_alt_offset_unpack_numrawbits(top);
		
		cost += codecosts.codecost_offset1[top];
		cost += codecosts.codecost_offset2[bot];
		cost += num_raw_bits * COST_ONE_BIT;
	}	
	
	
	//===================================================================
	// offset cache miss penalties for space-speed
	//	-> so far as I can tell this does absolutely nothing measurable
	//		I tried COST_PENALTY_NORMAL_CACHE as large as (6 * COST_PENALTY_SCALE)
	
	#ifdef COST_PENALTY_NORMAL_CACHE
	cost += newLZ_offset_blend_score(offset) * COST_PENALTY_NORMAL_CACHE;
	#endif
	
	#if 0
	
	// old way (shipped pre 2.6)

	// penalty for matches that go out of L1 : 
	//  ? what threshold here ?
	//  not sure if this is good at all
	//  just use "offset_blend_score" here ?
	if ( offset > 32*1024 )
	{
		cost += COST_PENALTY_NORMAL_CACHE;
		if ( offset > 256*1024 )
		{
			// 512k step?
			cost += COST_PENALTY_NORMAL_CACHE;
			if ( offset > 1024*1024 )
			{
				cost += COST_PENALTY_NORMAL_CACHE;
				// 2M step ?
				if ( offset > 4*1024*1024 )
				{
					cost += COST_PENALTY_NORMAL_CACHE;
					cost += COST_PENALTY_NORMAL_CACHE;
				}
			}
		}
	}
	
	// tiny bias to favor low offsets ? (this is super meh)
	//if ( offset > 256 )
	//	cost ++;
	
	#endif
	//===================================================================
	
	return cost;
}

// note : this "pos" should be the start of the packet
//	which is (pos-lrl) in the optimal parser

static RADFORCEINLINE S32 cost_lo_match_rep0len1(int lrl_for_packet,
		const newlzhc_codecosts & codecosts,int packet_pos)
{
	int packet = (lrl_for_packet<<NEWLZHC_PACKET_ML_BITS);
	packet += NEWLZHC_PACKET_NORMAL_MATCH_MIN;

	return codecosts.codecost_packet[packet_pos&codecosts.packet_pos_mask][packet];
}

static RADFORCEINLINE S32 cost_lo_match(int lrl_for_packet,int ml,int loi,
		const newlzhc_codecosts & codecosts,int packet_pos)
{
	RR_ASSERT( loi >= 0 && loi < NEWLZHC_NUM_LAST_OFFSETS );
	
	RR_ASSERT( ml >= NEWLZHC_LOMML );
	int ml_excess = ml - (NEWLZHC_LOMML + NEWLZHC_PACKET_ML_MAX);
	if ( ml_excess < 0 )
	{
		int packet = (lrl_for_packet<<NEWLZHC_PACKET_ML_BITS) + (ml - NEWLZHC_LOMML) + (loi<<NEWLZHC_PACKET_OFFSET_SHIFT);
		return codecosts.codecost_packet[packet_pos&codecosts.packet_pos_mask][packet];
	}
	else
	{
		int packet = (lrl_for_packet<<NEWLZHC_PACKET_ML_BITS) + (NEWLZHC_PACKET_ML_MAX) + (loi<<NEWLZHC_PACKET_OFFSET_SHIFT);
		return codecosts.codecost_packet[packet_pos&codecosts.packet_pos_mask][packet] + cost_excess_ml(ml_excess,codecosts);
	}
}

// cost_offset not included
static RADFORCEINLINE S32 cost_normal_match(int lrl_for_packet,int ml,
	const newlzhc_codecosts & codecosts,int packet_pos)
{
	// offset & lrl cost will be added on

	int ml_excess = ml - (NEWLZHC_LOMML + NEWLZHC_PACKET_ML_MAX);
	if ( ml_excess < 0 )
	{
		int packet = (lrl_for_packet<<NEWLZHC_PACKET_ML_BITS) + (ml - NEWLZHC_LOMML) + NEWLZHC_PACKET_NORMAL_MATCH_MIN;
		return codecosts.codecost_packet[packet_pos&codecosts.packet_pos_mask][packet];
	}
	else
	{
		int packet = (lrl_for_packet<<NEWLZHC_PACKET_ML_BITS) + (NEWLZHC_PACKET_ML_MAX) + NEWLZHC_PACKET_NORMAL_MATCH_MIN;
		return codecosts.codecost_packet[packet_pos&codecosts.packet_pos_mask][packet] + cost_excess_ml(ml_excess,codecosts);
	}
}

// these are only used in asserts :
#define tll_index_arrival_pos(index,num_tlls)	( ( (index)/(num_tlls) ) - ( (index)%(num_tlls) ) )
#define tll_index_is_last(index,num_tlls)	( ( (index)%(num_tlls) ) == (num_tlls)-1 )
#define tll_check_index_pos(pos,index,num_tlls)	( ((pos) == tll_index_arrival_pos(index,num_tlls)) || tll_index_is_last(index,num_tlls) )

// filling the arrival at [pos+ml] , from [pos-lrl]
static RADFORCEINLINE S32 try_lo_arrival_tll(
	newlzhc_optimal_arrival_tll * tll_arrivals,int num_tlls,
	int prev_index, const U8 * chunk_ptr,
	int * last_tll_len,
	
	S32 pos,S32 base_cost,S32 lrl,S32 lrl_for_packet,S32 ml,S32 loi,
	const newLZHC_LOs_NoPad & los,
	const newlzhc_codecosts & codecosts)
{
	// cost it
	S32 lo_cost = base_cost + cost_lo_match(lrl_for_packet,ml,loi,codecosts,pos-lrl);				
	
	if ( num_tlls == 1 )
	{
		newlzhc_optimal_arrival_tll & arrival = tll_arrivals[ (pos+ml) ];
	
		if ( lo_cost < arrival.cost )
		{
			arrival.cost = lo_cost;
			arrival.ml = ml;
			arrival.lrl = lrl;
			arrival.los.SetMTF(los,loi);
			arrival.is_twostep = 0;
			arrival.prev_index = prev_index;
			RR_DURING_ASSERT( arrival.check_offset = -loi );
			RR_DURING_ASSERT( arrival.check_arrival_pos = pos+ml );
		}
	}
	else
	{		
		RR_ASSERT( tll_check_index_pos(pos-lrl,prev_index,num_tlls) );
		
		// do t=0 (packet arrival)
		{
			newlzhc_optimal_arrival_tll & arrival = tll_arrivals[ (pos+ml)*num_tlls ];
		
			if ( lo_cost < arrival.cost )
			{
				arrival.cost = lo_cost;
				arrival.ml = ml;
				arrival.lrl = lrl;
				arrival.los.SetMTF(los,loi);
				arrival.is_twostep = 0;
				arrival.prev_index = prev_index;
				RR_DURING_ASSERT( arrival.check_offset = -loi );
				RR_DURING_ASSERT( arrival.check_arrival_pos = pos+ml );
			}
		}
		
		S32 offset = los.lasts[loi]; // silly way to get this
		
		// try all tailing counts :
		int t = 1;
		S32 cost = lo_cost;
		do
		{
			cost += cost_add_literal(chunk_ptr,pos+ml+t-1,offset,codecosts,t-1);
			
			newlzhc_optimal_arrival_tll & arrival = tll_arrivals[ (pos+ml+t)*num_tlls + t ];
		
			if ( cost < arrival.cost )
			{
				arrival.cost = cost;
				arrival.ml = ml;
				arrival.lrl = lrl;
				arrival.los.SetMTF(los,loi);
				arrival.is_twostep = 0;
				arrival.prev_index = prev_index;
				RR_DURING_ASSERT( arrival.check_offset = -loi );
				RR_DURING_ASSERT( arrival.check_arrival_pos = pos+ml );
				
				if ( t == num_tlls-1 )
				{
					// fill last_tll_len !!
					last_tll_len[(pos+ml+t)] = t;
				}
			}
		}
		while( ++t < num_tlls );
	}
	
	return lo_cost;
}

// filling the arrival at [pos+ml] , from [pos-lrl]
static RADFORCEINLINE S32 try_match_arrival_tll(
	newlzhc_optimal_arrival_tll * tll_arrivals,int num_tlls,
	int prev_index, const U8 * chunk_ptr,
	int * last_tll_len,
	
	S32 pos,S32 base_cost,S32 lrl,S32 lrl_for_packet,S32 ml,S32 offset,
	const newLZHC_LOs_NoPad & los,
	const newlzhc_codecosts & codecosts)
{			
	S32 match_cost = base_cost + cost_normal_match(lrl_for_packet,ml,codecosts,pos-lrl);
	
	if ( num_tlls == 1 )
	{
		newlzhc_optimal_arrival_tll & arrival = tll_arrivals[ (pos+ml) ];
	
		if ( match_cost < arrival.cost )
		{
			arrival.cost = match_cost;
			arrival.ml = ml;
			arrival.lrl = lrl;
			arrival.los.SetAdd(los,offset);
			arrival.is_twostep = 0;
			arrival.prev_index = prev_index;
			RR_DURING_ASSERT( arrival.check_offset = offset );
			RR_DURING_ASSERT( arrival.check_arrival_pos = pos+ml );
		}
	}
	else
	{		
		RR_ASSERT( tll_check_index_pos(pos-lrl,prev_index,num_tlls) );
		
		// do t == 0 (true packet arrival)
		{
			newlzhc_optimal_arrival_tll & arrival = tll_arrivals[ (pos+ml)*num_tlls ];
		
			if ( match_cost < arrival.cost )
			{
				arrival.cost = match_cost;
				arrival.ml = ml;
				arrival.lrl = lrl;
				arrival.los.SetAdd(los,offset);
				arrival.is_twostep = 0;
				arrival.prev_index = prev_index;
				RR_DURING_ASSERT( arrival.check_offset = offset );
				RR_DURING_ASSERT( arrival.check_arrival_pos = pos+ml );
			}
		}
	
		// try all tailing counts :
		int t = 1;
		S32 cost = match_cost;
		do
		{
			cost += cost_add_literal(chunk_ptr,pos+ml+t-1,offset,codecosts,t-1);
			
			newlzhc_optimal_arrival_tll & arrival = tll_arrivals[ (pos+ml+t)*num_tlls + t ];
		
			if ( cost < arrival.cost )
			{
				arrival.cost = cost;
				arrival.ml = ml;
				arrival.lrl = lrl;
				arrival.los.SetAdd(los,offset);
				arrival.is_twostep = 0;
				arrival.prev_index = prev_index;
				RR_DURING_ASSERT( arrival.check_offset = offset );
				RR_DURING_ASSERT( arrival.check_arrival_pos = pos+ml );
				
				if ( t == num_tlls-1 )
				{
					// fill last_tll_len !!
					last_tll_len[(pos+ml+t)] = num_tlls-1;
				}
			}
		}
		while( ++t < num_tlls );
	}
	
	return match_cost;
}



// don't define TWO_STEP_MAX_LRL to disable two step
// TWO_STEP_MAX_LRL = 2 means no excess
#define TWO_STEP_MAX_LRL	3
#define TWO_STEP_MML 2

#ifdef TWO_STEP_MAX_LRL

// two-step arrival where first step (first match) ends at pos
static RADFORCEINLINE void try_two_step_arrival_tll(
	newlzhc_optimal_arrival_tll * tll_arrivals,int num_tlls,
	int prev_index, const U8 * chunk_ptr, const U8 * ptr_matchend, S32 * pparse_chunk_end,

	S32 pos,S32 first_step_cost,S32 initial_ml,S32 initial_lrl,S32 initial_loi,S32 offset,
	const newLZHC_LOs_NoPad & los,
	const newlzhc_codecosts & codecosts)
{
	// Two-step matches are: original match, then a gap of non-matching chars,
	// then a LO0 match resuming our original match.

	// Try finding match len at TWO_STEP_MAX_LRL, then attempt to extend
	// that match len backwards to lower LRL while we can.
	//
	// For larger TWO_STEP_MAX_LRL (>=4), this misses gap matches that end
	// earlier than 4 bytes after pos+ml! So you don't want to push it too far.
	const U8 * twostep_ptr = chunk_ptr+pos+TWO_STEP_MAX_LRL;
	int twostep_ml = getmatchlen_mml1(twostep_ptr,twostep_ptr-offset,ptr_matchend);
	int twostep_lrl = TWO_STEP_MAX_LRL;

	#if TWO_STEP_MAX_LRL == 2
	int adjust = (twostep_ptr[-1] == twostep_ptr[-offset-1]) ? 1 : 0;
	twostep_lrl -= adjust;
	twostep_ml += adjust;
	#elif TWO_STEP_MAX_LRL == 3
	int adjust = (twostep_ptr[-2] == twostep_ptr[-offset-2]) ? 2 : 1;
	adjust = (twostep_ptr[-1] == twostep_ptr[-offset-1]) ? adjust : 0;
	twostep_lrl -= adjust;
	twostep_ml += adjust;
	#else
	while (twostep_lrl > 1 && twostep_ptr[-1] == twostep_ptr[-offset-1])
	{
		twostep_lrl--;
		twostep_ml++;
		twostep_ptr--;
	}
	#endif

	if ( offset < 8 )
		twostep_ml = RR_MIN(twostep_ml,(int)offset);

	if ( twostep_ml >= TWO_STEP_MML )
	{
		// cost the literals in the gap lrl :
		S32 twostep_literals_cost = newlzhc_cost_literals(chunk_ptr,pos,twostep_lrl,offset,codecosts,0);

		// cost it
		S32 twostep_cost = first_step_cost + twostep_literals_cost;
		int twostep_lrl_forpacket = twostep_lrl;
		#if TWO_STEP_MAX_LRL >= NEWLZHC_PACKET_LRL_MAX
		if ( twostep_lrl >= NEWLZHC_PACKET_LRL_MAX )
		{
			twostep_lrl_forpacket = NEWLZHC_PACKET_LRL_MAX;
			twostep_cost += cost_excess_lrl(twostep_lrl-NEWLZHC_PACKET_LRL_MAX,codecosts);
		}
		#endif
		twostep_cost += cost_lo_match(twostep_lrl_forpacket,twostep_ml,0,codecosts,pos);

		int twostep_arrival_pos = pos+twostep_lrl+twostep_ml;

		*pparse_chunk_end = RR_MAX(*pparse_chunk_end,twostep_arrival_pos);

		// don't bother :
		//for(int t=0;t<(num_tlls/2);t++)
		{
			//newlzhc_optimal_arrival_tll & arrival = tll_arrivals[(twostep_arrival_pos+t)*num_tlls + t];
			newlzhc_optimal_arrival_tll & arrival = tll_arrivals[(twostep_arrival_pos)*num_tlls ];

			if ( twostep_cost < arrival.cost )
			{
				arrival.cost = twostep_cost;
				arrival.ml = initial_ml;
				arrival.lrl = initial_lrl;
				if (initial_loi >= 0)
					arrival.los.SetMTF(los,initial_loi);
				else
					arrival.los.SetAdd(los,offset);
				arrival.twostep.lrl = twostep_lrl;
				arrival.twostep.ml = twostep_ml;
				arrival.prev_index = prev_index;
				RR_DURING_ASSERT( arrival.check_offset = (initial_loi >= 0) ? -initial_loi : offset );
				RR_DURING_ASSERT( arrival.check_arrival_pos = twostep_arrival_pos );
			}

			//twostep_cost += cost_add_literal(chunk_ptr,twostep_arrival_pos+t,offset,codecost_literal,subliteralmask);
		}
	}
}

#endif

/*************

newLZ_encode_chunk_optimal -
a lot of code dupe from newLZ_encode_chunk

first do a greedy parse just like the Normal heuristic parse
using the PMP matches

*************/

static SINTa newLZHC_encode_chunk_optimal_greedy(
		// ** OUTPUT :
		F32 * pJ,
		int * pchunktype,
		newlzhc_passinfo * const passinfo,
		U8 * comp,U8 * comp_end,
		// ** INPUT :
		int mml,
		ENewLZ_MinOffset min_offset,
		const newlz_vtable * vtable,
		const newLZHC_MatchParseRecord * matches,
		const U8 * chunk_ptr,int chunk_len,
		SINTa chunk_pos,
		const U8 * dictionaryBase,
		// ** SCRATCH :
		newlz_encoder_scratch * scratch,
		U8 * const literal_space,U8 * const literal_space_end,
		vector_a<newlzhc_encoder_parse> & parsevec)
{
	SIMPLEPROFILE_SCOPE_N(optimal_greedy,chunk_len);
	
	newLZHC_LOs lastoffsets;
	lastoffsets.Reset();
	
	parsevec.clear();

	RR_ASSERT( chunk_len >= NEWLZ_MIN_CHUNK_LEN );

	int start_pos = 0;
	if ( chunk_pos == 0 ) start_pos = NEWLZ_MIN_OFFSET;
	
	const OodleLZ_CompressOptions *pOptions = vtable->pOptions;
	
	U32 dictionarySize = ( pOptions->dictionarySize > 0 ) ?
		RR_MIN(NEWLZ_MAX_OFFSET,pOptions->dictionarySize)
		: NEWLZ_MAX_OFFSET;
	
	// parse_end_pos is the match *start* pos limit
	int parse_end_pos = chunk_len - NEWLZHC_CHUNK_NO_MATCH_ZONE;
	// ptr_matchend is the match *end* limit
	const U8 * ptr_matchend = chunk_ptr + chunk_len - NEWLZHC_MATCH_END_PAD;
	
	int literals_start = start_pos;
	
	//rrArenaAllocator * arena = scratch->arena;
	// newlzhc_histo is quite big, don't put it on the stack
	//newlzhc_histo histo = { }; // zero init histo
	//RR_SCOPE_ARENA(phisto,newlzhc_histo,arena);
	newlzhc_histo * phisto = &(passinfo->literals);
	RR_ZERO(*passinfo);
	
	
	{
	SIMPLEPROFILE_SCOPE_N(optimal_greedy_parse,chunk_len);
		
	for(int pos=start_pos;pos<parse_end_pos;)
	{
		const U8 * ptr = chunk_ptr + pos;
				
		match chosen = newLZHC_get_match_heuristic(matches[pos],lastoffsets,ptr,ptr_matchend,mml,pos-literals_start,dictionaryBase,dictionarySize,min_offset,pOptions);
					
		if ( chosen.ml == 0 )
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
			lazy = newLZHC_get_match_heuristic(matches[pos+1],lastoffsets,ptr+1,ptr_matchend,mml,pos+1-literals_start,dictionaryBase,dictionarySize,min_offset,pOptions);
			
			if ( newLZHC_LazyMatchDelta(lazy.ml,lazy.off,lazy.IsLO(),chosen.ml,chosen.off,chosen.IsLO()) > 0 )
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
			
				lazy = newLZHC_get_match_heuristic(matches[pos+2],lastoffsets,ptr+2,ptr_matchend,mml,pos+2-literals_start,dictionaryBase,dictionarySize,min_offset,pOptions);
				
				// IsLazy2Better : check newLZ_LazyMatchDelta > 3 :
				if ( newLZHC_LazyMatchDelta(lazy.ml,lazy.off,lazy.IsLO(),chosen.ml,chosen.off,chosen.IsLO()) > NEWLZHC_LAZYBETTER_LAZY2 )
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
		
		RR_ASSERT( ptr == chunk_ptr + pos );
		
		// have a match
		RR_ASSERT( chosen.ml >= mml || chosen.IsLO() );
				
		// force a pure LO0-LRL0 exclusion
		// the only time this should happen is at the very start of the file
		// when pos == start_pos , we could match from the initial lastoffsets
		// in that case, just bump index from 0 to 1
		// @@ with min_offset 1 this is not so easy
		//	rep0 exclusion is not enforced in low offset matches
		//	because they are len-clamped to prevent overlap
		if ( pos == literals_start && chosen.off == 0 && ( lastoffsets.lasts[0] == lastoffsets.lasts[1] ) )
		{
			RR_ASSERT( lastoffsets.lasts[0] == NEWLZ_MIN_OFFSET && lastoffsets.lasts[1] == NEWLZ_MIN_OFFSET );
			RR_ASSERT( chosen.ml >= 2 );
			chosen.off = -1;
		}
		
		// verify match :
		RR_DURING_ASSERT( S32 offset = chosen.IsLO() ? lastoffsets.lasts[-chosen.off] : chosen.off );
		RR_ASSERT( memcmp(ptr,ptr-offset,chosen.ml) == 0 );
		RR_ASSERT( (ptr+chosen.ml) >= ptr_matchend || ptr[chosen.ml] != (ptr-offset)[chosen.ml] || offset < 8 );

		// make sure match backup didn't violate LO0LRL0 exclusion :
		RR_ASSERT( pos > literals_start || chosen.off != 0 || chosen.ml == 1 || min_offset == 1 );
		
		int lrl = pos - literals_start;
				
		newlzhc_encoder_parse cur_parse;
		cur_parse.lrl = lrl;
		cur_parse.ml = chosen.ml;
		cur_parse.offset = chosen.off;
		cur_parse.lastoffset = lastoffsets.LastOffset();
						
		{
		//SIMPLEPROFILE_SCOPE(newLZHC_add_to_histo);
		parsevec.push_back(cur_parse);
		
		newLZHC_histo_literals(phisto, chunk_ptr, literals_start, lrl, lastoffsets.LastOffset() );
		}
		
		// update lastoffsets :
		if ( chosen.IsLO() )
			lastoffsets.MTF(-chosen.off);
		else
			lastoffsets.Add(chosen.off);	
		
		pos += chosen.ml;
		literals_start = pos;
	}
	
	}

	/*
	if ( parsevec.empty() )
	{
		// no packets; just forbid this very rare case
		// let it be handled by outer huff-only array
		return chunk_len;
	}
	*/
	
	// write final sequence
	// trailing LRL :
	SINTa final_lrl = chunk_len - literals_start;	
	S32 final_lo = lastoffsets.LastOffset();
		
	if ( final_lrl > 0 )
	{
		newLZHC_histo_literals(phisto,chunk_ptr,literals_start,final_lrl,final_lo);
	}

	// Turn off split for greedy-for-optimal output; too slow!
	//	(part of the slowness being that this is done 3 times at MML 3,4,&8)
	
	// @@ -> does this ever hurt final compression ratio?
	// mostly the greedy-for-optimal is just used to seed the optimal parse
	// but it can be used by itself in the greedy revert case
	//   when we do that, we'd like to go back and consider split arrays here

	/*
	
	yes disabling it does hurt a tiny bit :
	
	Kraken, Optimal3, lzt99 : 

	before:
	24,700,820 -> 9,499,686 =  3.077 bpb =  2.600 to 1 

	after:
	24,700,820 -> 9,515,426 =  3.082 bpb =  2.596 to 1 
	
	0.005 bpb is actually big in my world
	
	@@@@ TODO : win this back, without a big encode speed hit
	
	*/

	// greedy-for-optimal
	OodleLZ_CompressionLevel save_level = vtable->level;
	U32 save_entropy_flags = vtable->entropy_flags;
	const_cast<newlz_vtable *>(vtable)->level = OodleLZ_CompressionLevel_Normal;
	const_cast<newlz_vtable *>(vtable)->entropy_flags &= ~ NEWLZ_ARRAY_FLAG_ALLOW_SPLIT;
	// @@ allow normal splits, just not N-indexed ?

	// @@ NOTE : vtable->bitstream flags not changed !
	//		-> that makes optimal-greedy different than normal greedy
	//		in particular ALT OFFSETS ARE *ON*	

	SINTa greedy_complen = newLZHC_put_parse(scratch,pJ,phisto,pchunktype,chunk_ptr,chunk_len,comp,comp_end,chunk_pos,vtable,final_lo,parsevec,start_pos,passinfo,min_offset);

	const_cast<newlz_vtable *>(vtable)->level = save_level;
	const_cast<newlz_vtable *>(vtable)->entropy_flags = save_entropy_flags;
	
	return greedy_complen;
}

//===================================================================

static S32 OPTIMAL_RESCALE_ADD = 0;
static S32 OPTIMAL_RESCALE_SHIFT = 3;
static S32 OPTIMAL_RESCALE_BIAS = 5;
static S32 INCREMENTAL_RESCALE_BIAS = 1;

static S32 c_histo_rescale_incremental_total = 9000;
//static S32 c_histo_rescale_incremental_total = 0; // 0 to disable
	
// desired update interval : (this is sensitive/tweaky)
static S32 parse_chunk_len = 384;
	
//static S32 optimal_update_codelens_histo_inc = 4;
static S32 optimal_update_codelens_histo_inc = 5;

#if 0
OODLE_NS_END
#include "optvars.h"
OODLE_NS_START

//OPTVAR_REGISTER(OPTIMAL_RESCALE_SHIFT,1,4,1);
//OPTVAR_REGISTER(OPTIMAL_RESCALE_BIAS,0,7,1);
OPTVAR_REGISTER(OPTIMAL_RESCALE_ADD,0,1,1);
OPTVAR_REGISTER(INCREMENTAL_RESCALE_BIAS,0,1,1);
OPTVAR_REGISTER(c_histo_rescale_incremental_total,7000,10000,1000);
//OPTVAR_REGISTER(c_histo_rescale_incremental_total,0,8000,2000);
//OPTVAR_REGISTER(parse_chunk_len,128,512,128);
OPTVAR_REGISTER(optimal_update_codelens_histo_inc,4,5,1);
#endif

// forced update interval
static const int parse_chunk_len_max = 4096;

//===================================================================
	
/**

optimal_update_codelens :

scan through parsevec (the incremental piece we are adding on)
histogram the literals/packets/offsets/excesses it makes
add them to previous histo
update codecosts

**/
static void optimal_update_passinfo(
		const vector_a<newlzhc_encoder_parse> & parsevec,
		int parse_start_pos,
		newlzhc_passinfo & passinfo,
		const U8 * chunk)
{
	SIMPLEPROFILE_SCOPE(optimal_update_passinfo);
			
	// passinfo contains running histos

	for LOOPVEC(parseveci,parsevec)
	{
		const newlzhc_encoder_parse & parse = parsevec[parseveci];

		int lrl = parse.lrl;
		int packet_pos = parse_start_pos;
		S32 lastoffset = parse.lastoffset;

		parse_start_pos += lrl + parse.ml; // update for next now
		
		// @@ I actually only need to do the literal type of my chunktype ; this does all
		newLZHC_histo_literals(&passinfo.literals,chunk,packet_pos,lrl,lastoffset,optimal_update_codelens_histo_inc);
		
		// newLZHC_histo_literals doesn't do sub :
		for(int i=0;i<lrl;i++)
		{
			int pos = packet_pos + i;
			U8 sub = (U8)(chunk[pos] - chunk[pos - (SINTa)lastoffset]);
			passinfo.literals_sub[sub] += optimal_update_codelens_histo_inc;
		}
	
		// parse_to_packet
	
		bool islo = parse.offset <= 0;
		
		int lrl_for_packet = (lrl > NEWLZHC_PACKET_LRL_MAX) ? NEWLZHC_PACKET_LRL_MAX : lrl;
		
		if ( lrl_for_packet == NEWLZHC_PACKET_LRL_MAX )
		{
			int excess = lrl - NEWLZHC_PACKET_LRL_MAX;
			passinfo.excess_lrl_histo[ RR_MIN(excess,255) ] += optimal_update_codelens_histo_inc;
		}
		
		int packet = (lrl_for_packet<<NEWLZHC_PACKET_ML_BITS);
			
		if ( parse.ml == 1 )
		{
			RR_ASSERT_FAILURE_ALWAYS("ml == 1");
		}
		else
		{		
			int ml_to_send = parse.ml - NEWLZHC_LOMML;
			int ml_for_packet = (ml_to_send > NEWLZHC_PACKET_ML_MAX) ? NEWLZHC_PACKET_ML_MAX : ml_to_send;
						
			packet += ml_for_packet;
		
			if ( islo )
			{
				// LO
				int loi = -parse.offset;
				RR_ASSERT( loi >= 0 && loi < NEWLZHC_NUM_LAST_OFFSETS );
				packet += loi << NEWLZHC_PACKET_OFFSET_SHIFT;
			}
			else
			{
				RR_ASSERT( parse.ml >= 3 );
			
				packet += NEWLZHC_PACKET_NORMAL_MATCH_MIN;
							
				S32 off = parse.offset;
				RR_ASSERT( off >= 1 && off < NEWLZ_MAX_OFFSET );
				
				if ( passinfo.offset_alt_modulo == 0 )
				{
					RR_ASSERT( off >= NEWLZ_MIN_OFFSET );
				
					U8 offset_u8 = newLZ_offset44_pack_u8(off);

					passinfo.offset_histo1[ offset_u8 ] += optimal_update_codelens_histo_inc;
				}
				else if ( passinfo.offset_alt_modulo == 1 )
				{
					U8 offset_u8 = newLZ_alt_offset_pack_u8_modulo1(off);

					passinfo.offset_histo1[ offset_u8 ] += optimal_update_codelens_histo_inc;
				}
				else
				{
					U8 top,bot;
					newLZ_alt_offset_pack_u8s(off,&top,&bot,passinfo.offset_alt_modulo);

					passinfo.offset_histo1[ top ] += optimal_update_codelens_histo_inc;
					passinfo.offset_histo2[ bot ] += optimal_update_codelens_histo_inc;
				}
			}	
			
			if ( ml_for_packet == NEWLZHC_PACKET_ML_MAX )
			{
				int excess = ml_to_send - NEWLZHC_PACKET_ML_MAX;
				passinfo.excess_ml_histo[ RR_MIN(excess,255) ] += optimal_update_codelens_histo_inc;
			}
		}
	
		passinfo.packet_histo[packet] += optimal_update_codelens_histo_inc;

		passinfo.packet_pos_histo[packet_pos&NEWLZHC_PACKET_POS_MASK][packet] += optimal_update_codelens_histo_inc;	
	}
}


static void optimal_passinfo_to_codecost(
		const newlzhc_passinfo & passinfo,
		newlzhc_codecosts & codecosts)
{
	SIMPLEPROFILE_SCOPE(optimal_passinfo_to_codecost);
	
	// cost_entropy_threshold triggers flattening codecost (important for Mermaid, not Kraken)
	const int cost_entropy_threshold = COST_ONE_BYTE - 1; // just barely under
	
	// must be the same or copying the histos doesn't make any sense :
	RR_ASSERT( passinfo.offset_alt_modulo == codecosts.offset_alt_modulo );
	
	histo_to_codecost(passinfo.offset_histo1,codecosts.codecost_offset1,256,COST_PENALTY_NORMAL_MATCH,cost_entropy_threshold);
	if ( passinfo.offset_alt_modulo > 1 )
		histo_to_codecost(passinfo.offset_histo2,codecosts.codecost_offset2,256,0,cost_entropy_threshold);

	histo_to_codecost(passinfo.excess_ml_histo,codecosts.codecost_excess_ml,256,COST_PENALTY_EXCESS_ML,cost_entropy_threshold);
	histo_to_codecost(passinfo.excess_lrl_histo,codecosts.codecost_excess_lrl,256,COST_PENALTY_EXCESS_LRL,cost_entropy_threshold);

	if ( passinfo.did_packet_pos )
	{
		codecosts.packet_pos_mask = NEWLZHC_PACKET_POS_MASK;

		for LOOP(i,NEWLZHC_PACKET_POS_COUNT)
		{
			histo_to_codecost(passinfo.packet_pos_histo[i],codecosts.codecost_packet[i],256,COST_PENALTY_PACKET,cost_entropy_threshold);
		}	
	}
	else
	{
		codecosts.packet_pos_mask = 0;
		histo_to_codecost(passinfo.packet_histo,codecosts.codecost_packet[0],256,COST_PENALTY_PACKET,cost_entropy_threshold);
	}
	
	switch(codecosts.chunktype)
	{
	case NEWLZ_LITERALS_TYPE_SUB:
		histo_to_codecost(passinfo.literals_sub,codecosts.codecost_literal[0],256,0,cost_entropy_threshold);
		break;
		
	case NEWLZ_LITERALS_TYPE_RAW:
		histo_to_codecost(passinfo.literals.literals_raw,codecosts.codecost_literal[0],256,0,cost_entropy_threshold);
		break;
	
	case NEWLZ_LITERALS_TYPE_LAMSUB:
		histo_to_codecost(passinfo.literals.literals_sub_lam  ,codecosts.codecost_literal[0],256,0,cost_entropy_threshold);
		histo_to_codecost(passinfo.literals.literals_sub_later,codecosts.codecost_literal[1],256,0,cost_entropy_threshold);
		break;
		
	case NEWLZ_LITERALS_TYPE_SUBAND3:
		for LOOP(i,4)
		{
			U32 histo_temp[256];
			const U32 * src0 = passinfo.literals.literals_sub_andF[i];
			for (int j = 4; j < 16; j += 4)
			{
				simd_add_u32_256(histo_temp,src0,passinfo.literals.literals_sub_andF[i + j]);
				src0 = histo_temp;
			}

			histo_to_codecost(histo_temp,codecosts.codecost_literal[i],256,0,cost_entropy_threshold);
		}
		break;
		
	case NEWLZ_LITERALS_TYPE_O1:
		for LOOP(i,NEWLZ_O1_CONTEXT_COUNT)
		{
			histo_to_codecost(passinfo.literals.literals_o1[i],codecosts.codecost_literal[i],256,0,cost_entropy_threshold);
		}
		break;
		
	case NEWLZ_LITERALS_TYPE_SUBANDF:
		for LOOP(i,16)
		{
			histo_to_codecost(passinfo.literals.literals_sub_andF[i],codecosts.codecost_literal[i],256,0,cost_entropy_threshold);
		}
		break;
	
	// no default
	}
}

static void optimal_rescale_histo(
		U32	histo[256],
		int add)
{
	RR_UNUSED_VARIABLE(add);

	// @@!! tweaky
	//	skip this completely if optimal_update_codelens_histo_inc is high?
	//	-> no
	//	even when inc is high this seems to help because it flattens out histo
	//	 adds on a constant distribution
	for LOOP(i,256)
	{
		// meh basically a nop :
		//histo[i] = add + (histo[i]>>4);
		// @@@@ + rounding bias inside the () ?
		histo[i] = OPTIMAL_RESCALE_ADD + ((histo[i] + OPTIMAL_RESCALE_BIAS)>>OPTIMAL_RESCALE_SHIFT);
	}
}

static void optimal_rescale_histo2(
		U32	histo1[256],
		const U32	histo2[256])
{
	for LOOP(i,256)
	{
		// @@@@ + rounding bias inside the () ?
		histo1[i] = OPTIMAL_RESCALE_ADD + ((histo1[i] + histo2[i] + OPTIMAL_RESCALE_BIAS*2+1)>>(OPTIMAL_RESCALE_SHIFT+1));
	}
}

static void optimal_rescale_histo_incremental(
		U32	histo[256],
		int add)
{
	if ( c_histo_rescale_incremental_total == 0 )
		return;

	U32 sum = rrSumOfHistogram(histo,256);
	if ( sum < (U32)c_histo_rescale_incremental_total )
		return;
	
	for LOOP(i,256)
	{
		//histo[i] = OPTIMAL_RESCALE_ADD + ((histo[i]+1)>>1);
		histo[i] = OPTIMAL_RESCALE_ADD + ((histo[i] + INCREMENTAL_RESCALE_BIAS)>>1);
	}	
}

static void optimal_rescale_passinfo_incremental(
		newlzhc_passinfo & passinfo) // <- read/write argument
{

	optimal_rescale_histo_incremental(passinfo.literals.literals_raw,2);
	optimal_rescale_histo_incremental(passinfo.literals.literals_sub_lam,2);
	optimal_rescale_histo_incremental(passinfo.literals.literals_sub_later,2);
	for LOOP(i,NEWLZ_O1_CONTEXT_COUNT)
		optimal_rescale_histo_incremental(passinfo.literals.literals_o1[i],2);
	for LOOP(i,16)
		optimal_rescale_histo_incremental(passinfo.literals.literals_sub_andF[i],2);
	
	optimal_rescale_histo_incremental(passinfo.literals_sub,2);
	
	optimal_rescale_histo_incremental(passinfo.offset_histo1,1);
	if ( passinfo.offset_alt_modulo > 1 )
		optimal_rescale_histo_incremental(passinfo.offset_histo2,1);
	
	optimal_rescale_histo_incremental(passinfo.packet_histo,1);
	for LOOP(i,4)
	{
		optimal_rescale_histo_incremental(passinfo.packet_pos_histo[i],1);
	}
	
	optimal_rescale_histo_incremental(passinfo.excess_ml_histo,2);
	optimal_rescale_histo_incremental(passinfo.excess_lrl_histo,2);
}		

static void optimal_rescale_passinfo(
		newlzhc_passinfo & passinfo) // <- read/write argument
{
	// @@ don't really need to be doing ALL these literal modes
	//	can do just the one for chunktype, since we never switch
	optimal_rescale_histo(passinfo.literals.literals_raw,2);
	optimal_rescale_histo(passinfo.literals.literals_sub_lam,2);
	optimal_rescale_histo(passinfo.literals.literals_sub_later,2);
	for LOOP(i,NEWLZ_O1_CONTEXT_COUNT)
		optimal_rescale_histo(passinfo.literals.literals_o1[i],2);
	for LOOP(i,16)
		optimal_rescale_histo(passinfo.literals.literals_sub_andF[i],2);
	
	optimal_rescale_histo(passinfo.literals_sub,2);
	
	optimal_rescale_histo(passinfo.offset_histo1,1);
	if ( passinfo.offset_alt_modulo > 1 )
		optimal_rescale_histo(passinfo.offset_histo2,1);
	
	optimal_rescale_histo(passinfo.packet_histo,1);
	for LOOP(i,4)
	{
		optimal_rescale_histo(passinfo.packet_pos_histo[i],1);
	}
	
	optimal_rescale_histo(passinfo.excess_ml_histo,2);
	optimal_rescale_histo(passinfo.excess_lrl_histo,2);
}

// passinfo1 = current chunk's greedy
// passinfo2 = last chunk's optimal
static void optimal_rescale_passinfo2(
		newlzhc_passinfo & passinfo1, // <- read/write argument
		const newlzhc_passinfo & passinfo2,
		bool literals_same)
{
	// since I track histos of both, you can carry stats even if mode changes
	// meh for compression whether to check literals_same or not
	
	// @@ don't really need to be doing ALL these literal modes
	//	can do just the one for chunktype, since we never switch
	// @@@@ switch on chunktype!
	optimal_rescale_histo2(passinfo1.literals.literals_raw,passinfo2.literals.literals_raw);
	optimal_rescale_histo2(passinfo1.literals.literals_sub_lam  ,passinfo2.literals.literals_sub_lam);
	optimal_rescale_histo2(passinfo1.literals.literals_sub_later,passinfo2.literals.literals_sub_later);
	for LOOP(i,NEWLZ_O1_CONTEXT_COUNT)
		optimal_rescale_histo2(passinfo1.literals.literals_o1[i],passinfo2.literals.literals_o1[i]);
	for LOOP(i,16)
		optimal_rescale_histo2(passinfo1.literals.literals_sub_andF[i],passinfo2.literals.literals_sub_andF[i]);
	
	optimal_rescale_histo2(passinfo1.literals_sub,passinfo2.literals_sub);
	
	optimal_rescale_histo2(passinfo1.packet_histo,passinfo2.packet_histo);
	for LOOP(i,4)
	{
		optimal_rescale_histo2(passinfo1.packet_pos_histo[i],passinfo2.packet_pos_histo[i]);
	}
	
	optimal_rescale_histo2(passinfo1.excess_ml_histo,passinfo2.excess_ml_histo);
	optimal_rescale_histo2(passinfo1.excess_lrl_histo,passinfo2.excess_lrl_histo);
	
	if ( passinfo1.offset_alt_modulo == passinfo2.offset_alt_modulo )
	{
		optimal_rescale_histo2(passinfo1.offset_histo1,passinfo2.offset_histo1);
		if ( passinfo1.offset_alt_modulo > 1 )
			optimal_rescale_histo2(passinfo1.offset_histo2,passinfo2.offset_histo2);
	}
	else
	{
		// offset_alt_modulo changed
		
		// passinfo1 comes from greedy , which doesn't do choose_offset_modulo
		//	so it is always 0 or 1 :
		// -> not true; normal greedy does do choose now
		//RR_ASSERT( passinfo1.offset_alt_modulo == 0 || passinfo1.offset_alt_modulo == 1 );
		
		// -> there's no real reason to take pass1 or pass2?
		//	-> pass1 is fine, could skip this memcpy
		
		/*
		// keep passinfo2 only 
		memcpy(passinfo1.offset_histo1,passinfo2.offset_histo1,sizeof(passinfo2.offset_histo1));
		memcpy(passinfo1.offset_histo2,passinfo2.offset_histo2,sizeof(passinfo2.offset_histo2));
		passinfo1.offset_alt_modulo = passinfo2.offset_alt_modulo;
		/**/
		
		optimal_rescale_histo(passinfo1.offset_histo1,1);
		if ( passinfo1.offset_alt_modulo > 1 )
			optimal_rescale_histo(passinfo1.offset_histo2,1);
	}
}

//===================================

/***********************************

TLL parse :

arrivals is a 2d array conceptually
tll_arrivals[pos][tll]

tll = "trailing literal len"

it's a number of literals *after* the current packet
(constrast with lrl = # of literals leading the current packet)

arrival[pos][tll]
is a way to get to pos with tll literals carried
so it's a packet arrival to (pos-tll) + tll literals after

arrival[pos][0] is a true packet arrival

tll becomes the start of the next packet

arrival[pos][num_tlls-1] is special, it can store a tll len that gets high
stored in separate array last_tll_len
this lets cheap LRL stretches get carried forward

----------

parse at pos p :
find matches

consider forming a packet that starts at (p - lrl) and arrives at (p + ml)

the lrl for packet leads are the tlls in arrivals[p][tll]

when you form the arrival at (p+ml) , also do tll's at (p+ml+1) etc.

-----------------------

the whole point of the TLL parse is that

arrival A to pos P might be the cheapest way to get to P
but arrival B + 1 literal to get to pos P+1 might be cheaper than A + lit to P+1

TLL parse is only interesting for sub literals (no advantage for raw)

TLL parse is about 2X slower than normal optimal parse

-------------------------

In some ways the TLL parse is cleaner conceptually.  It doesn't need the "cheap_preceding_arrival" thing at all,
it just propagates that through the [last_tll] slot.  It doesn't do explicit LRL scan-back, it just uses all the
tll arrivals at the current pos.

***********************************/

/**

TLL and non-TLL parse are now both in here

if num_tlls = 1 : non-TLL parse
	"index" of tll_arrivals[] is just "pos"
	cheap_preceding_arrival_pos is used to find long LRL steps
	
if num_tlls > 1 : TLL-parse
	index is (pos*num_tlls+t)
	last_tll_len[] is used

**/

struct newlzhc_carried_encoder_state
{
	int chunktypes[2];
	newlzhc_passinfo passinfos[2];
};

static SINTa newLZHC_encode_chunk_optimal_tll(const newlz_vtable * vtable,
		newlz_encoder_scratch * scratch,
		const U8 * dictionaryBase,
		const U8 * chunk_ptr,int chunk_len,
		U8 * comp,U8 * comp_end,
		SINTa chunk_pos,
		int * pchunktype,
		F32 * pJ,
		const OodleKrakenChunkDeadlines *)
{
	SIMPLEPROFILE_SCOPE_N(encode_optimal_tll,chunk_len);
	THREADPROFILESCOPE("leviathan_chunk_optimal");

	*pchunktype = 0;
	
	rrPrintf_v2("---------------------------------------------------------------------\n");
	rrPrintf_v2("%8d : %d\n",chunk_pos,chunk_len);
	
	if ( chunk_len <= NEWLZ_MIN_CHUNK_LEN )
	{
		return chunk_len;
	}
			
	int num_tlls;
	int max_search_lrl;
	
	if ( vtable->level >= OodleLZ_CompressionLevel_Optimal4 ) // Optimal4 is TLL
	{
		num_tlls = 2; // more than 2 doesn't help
		max_search_lrl = 8;
	}
	else
	{
		num_tlls = 1;

		// 02-25-2019
		// was max_search_lrl = 8, 4
		// see repots zzz_max_search_lrl before/after		

		if ( vtable->level >= OodleLZ_CompressionLevel_Optimal2 ) max_search_lrl = 3;
		else max_search_lrl = 1; // Optimal1
		
		// Optimal1 is CTMF
		// Optimal2 is ST
		// Optimal3 is MML 3 selection
		// Optimal4 is TLL
	}
	
	//=============================================
	
	rrArenaAllocator * arena = scratch->arena;
	
	const OodleLZ_CompressOptions * pOptions = vtable->pOptions;
	U32 dictionarySize = ( pOptions->dictionarySize > 0 ) ?
		RR_MIN(NEWLZ_MAX_OFFSET,pOptions->dictionarySize)
		: NEWLZ_MAX_OFFSET;

	RR_ASSERT( vtable->find_all_matches_num_pairs == NEWLZHC_MATCH_NUM_PAIRS );
	newLZHC_MatchParseRecord * matches = (newLZHC_MatchParseRecord *) scratch->match_pairs;
	
	/*
	
	@@ 
	newlz chunk_parsevec is unnecessary, kill it
		just append right onto parsevec

	-> or only keep chunk_parsevec
	and just fill it per adaptation chunk
	then output to "arrays" after each step
	no overall parsevec

	*/
	
	vector_a<newlzhc_encoder_parse> parsevec;
	vector_a<newlzhc_encoder_parse> chunk_parsevec;
	SINTa parsevec_bytes = sizeof(newlzhc_encoder_parse)*(chunk_len/2+8);
	SINTa chunk_parsevec_bytes = sizeof(newlzhc_encoder_parse)*(parse_chunk_len_max+8);
	
	scratch->newlzhc_parsevec_space.extend(parsevec_bytes+chunk_parsevec_bytes,arena);
	parsevec.provide_arena(scratch->newlzhc_parsevec_space.m_ptr,parsevec_bytes);
	chunk_parsevec.provide_arena((char *)scratch->newlzhc_parsevec_space.m_ptr+parsevec_bytes,chunk_parsevec_bytes);

	int literals_plus_packets_limit = newlz_literal_space_reserve_size(chunk_len);

	scratch->literals_space.extend(literals_plus_packets_limit,arena);
	U8 * literal_space = scratch->literals_space.getU8();
	U8 * literal_space_end = literal_space + literals_plus_packets_limit;
		
	int start_pos = 0;
	if ( chunk_pos == 0 ) start_pos = NEWLZ_MIN_OFFSET;
		
	// parse_end_pos is the match *start* pos limit
	int parse_end_pos = chunk_len - NEWLZHC_CHUNK_NO_MATCH_ZONE;
	// ptr_matchend is the match *end* limit
	const U8 * ptr_matchend = chunk_ptr + chunk_len - NEWLZHC_MATCH_END_PAD;
	
	newlzhc_passinfo * ppassinfo;
	scratch->newlzhc_passinfo_space.extend( sizeof(newlzhc_passinfo)*2, arena);
	ppassinfo = (newlzhc_passinfo *) scratch->newlzhc_passinfo_space.get();
	newlzhc_passinfo & passinfo = ppassinfo[0];
	newlzhc_passinfo & passinfo_alt = ppassinfo[1];
			
	const newLZHC_MatchParseRecord * pmatches = matches;
	
	scratch->newlzhc_arrivals_space.extend(sizeof(newlzhc_optimal_arrival_tll)*num_tlls*(chunk_len+1),arena);
	newlzhc_optimal_arrival_tll * tll_arrivals = (newlzhc_optimal_arrival_tll *) scratch->newlzhc_arrivals_space.m_ptr;
	
	//=====================================
	
	// mml >= 4 for the greedy seed pass :
	int mml = RR_MAX(4,pOptions->minMatchLen);
	F32 greedy_J_initial = LAGRANGE_COST_INVALID;
	int passinfo_chunktype = 0;
	
	SINTa comp_complen = RR_S32_MAX;
	
	// [comp] is of len [comp_complen] , [*pchunktype] and [*pJ]
	// those 4 variables store the current best found encoding
	//
	// the current pass is seeded by [passinfo] and [passinfo_chunktype]
	// which doesn't necessarily match what's in [comp]

	// output trials into the arrivals buffer :
	//	then copy to "comp" only if it beats greedy_complen
	U8 * optimal_comp = (U8 *)tll_arrivals;
	U8 * optimal_comp_end = optimal_comp + scratch->newlzhc_arrivals_space.m_size;
		
	// tll_arrivals[0] is a true packet arrival
	// tll_arrivals[t] has t trailing literals
	// tll_arrivals[last_tll] has trailing len last_tll_len[pos]
	
	int last_tll = chunk_len; // make sure I'm not used
	int * last_tll_len = NULL;
	if ( num_tlls > 1 )
	{
		scratch->newlzhc_last_tll_len_space.extend(sizeof(int)*(chunk_len+1),arena);
		last_tll_len = (int *)scratch->newlzhc_last_tll_len_space.get();
	}
	
	int optimal_skip_len = 1 << RR_MIN((int)(vtable->level),8);
	const HalfOpenS32Interval ml_range_for_lomatch_red(NEWLZHC_LOMML + 1, optimal_skip_len);
	const HalfOpenS32Interval ml_range_for_match_red(mml + 1, optimal_skip_len);
	int twostep_end_pos = parse_end_pos - (TWO_STEP_MAX_LRL + 2);
	
	ENewLZ_MinOffset min_offset = eNewLZ_MinOffset_8;
	
	//=======================================
	
	for (int optimal_iter=0;optimal_iter<2;optimal_iter++)
	{
		
	if ( optimal_iter )
	{
		if ( vtable->level < OodleLZ_CompressionLevel_Optimal5 )
			break;
			
		min_offset = eNewLZ_MinOffset_1;
		
		int chunktype_mo1;
		F32 greedy_J_mo1 = LAGRANGE_COST_INVALID;
		
		mml = 3; // @@ min offset 1 always mml 3 ?
		
		SINTa greedy_complen_mo1 = newLZHC_encode_chunk_optimal_greedy(
						&greedy_J_mo1,
						&chunktype_mo1,&passinfo,
						optimal_comp,optimal_comp_end,
						mml,eNewLZ_MinOffset_1,
						vtable,pmatches,chunk_ptr,chunk_len,chunk_pos,dictionaryBase,
						scratch,literal_space,literal_space_end,parsevec);

		rrPrintf_v2("greedy mo1 seed : complen=%d\n",greedy_complen_mo1);

		if ( greedy_complen_mo1 >= chunk_len )
		{
			// expanded - just abort this iter
			continue;
		}

		// does this encoding beat what's in [comp] ?
		if ( greedy_J_mo1 < *pJ )
		{
			rrPrintf_v2("greedy mo1 passinfo alt!\n");
		
			*pchunktype = chunktype_mo1;
			*pJ = greedy_J_mo1;
			memcpy(comp,optimal_comp,greedy_complen_mo1);
			comp_complen = greedy_complen_mo1;
		}
		
		// even if we don't beat *pJ
		// we do set the [passinfo] and chunktype for this pass
		
		passinfo_chunktype = chunktype_mo1;
			
		// alt offsets :
		RR_ASSERT( passinfo.offset_alt_modulo > 0 );
		// compatible literals :
		RR_ASSERT( passinfo_chunktype == NEWLZ_LITERALS_TYPE_RAW || passinfo_chunktype == NEWLZ_LITERALS_TYPE_O1 );
	}
	else
	{	
		comp_complen = newLZHC_encode_chunk_optimal_greedy(
							&greedy_J_initial,
							&passinfo_chunktype,&passinfo,
							comp,comp_end,
							mml,eNewLZ_MinOffset_8,
							vtable,pmatches,chunk_ptr,chunk_len,chunk_pos,dictionaryBase,
							scratch,literal_space,literal_space_end,parsevec);
			
		if ( comp_complen >= chunk_len )
		{
			// expanded - just abort this iter
			continue;
		}
				
		*pchunktype = passinfo_chunktype;
		*pJ = greedy_J_initial;

	
		#if 0 // @@ try mml3 passinfo seeding ?
				
		// MML3 testing on -z7 for Kraken , on all levels for Hydra
		if ( ( vtable->level >= OodleLZ_CompressionLevel_Optimal3
			|| vtable->compressor == OodleLZ_Compressor_Hydra ) &&
			pOptions->minMatchLen <= 3 )
		{
			int chunktype_mml3;
			F32 greedy_J_mml3 = LAGRANGE_COST_INVALID;
				
			SINTa greedy_complen_mml3 = newLZHC_encode_chunk_optimal_greedy(
							&greedy_J_mml3,
							&chunktype_mml3,&passinfo_alt,
							optimal_comp,optimal_comp_end,
							3,min_offset,
							vtable,pmatches,chunk_ptr,chunk_len,chunk_pos,dictionaryBase,
							scratch,literal_space,literal_space_end,parsevec);

			if ( greedy_J_mml3 < *pJ &&
				greedy_complen_mml3 < chunk_len )
			{
				rrPrintf_v2("greedy mml3 passinfo alt!\n");
			
				*pchunktype = chunktype_mml3;
				*pJ = greedy_J_mml3;
				memcpy(comp,optimal_comp,greedy_complen_mml3);
				comp_complen = greedy_complen_mml3;
			
				// -> we always change mml to 3 anyway
				// this is just for the passinfo
				mml = 3;	
				
				passinfo = passinfo_alt;
				greedy_chunktype = chunktype_mml3;
			}
		}
		
		#endif
		
		#if 1 // try mml8 passinfo seeding ?
		
		// MML 8 testing at all levels ?
		// no need at pOptions->minMatchLen == 8 because I did that in the first greedy pass
		// -> this helps a decent amount on GameTestSet
		if ( 8 > pOptions->minMatchLen )
		{
			// 8 is better than 6
			int chunktype_mml8;
			F32 greedy_J_mml8 = LAGRANGE_COST_INVALID;
					
			SINTa greedy_complen_mml8 = newLZHC_encode_chunk_optimal_greedy(
							&greedy_J_mml8,
							&chunktype_mml8,&passinfo_alt,
							optimal_comp,optimal_comp_end,
							8,min_offset,
							vtable,pmatches,chunk_ptr,chunk_len,chunk_pos,dictionaryBase,
							scratch,literal_space,literal_space_end,parsevec);

			if ( greedy_J_mml8 < *pJ &&
				greedy_complen_mml8 < chunk_len )
			{
				rrPrintf_v2("greedy mml8 passinfo alt!\n");
				
				*pchunktype = chunktype_mml8;
				*pJ = greedy_J_mml8;
				memcpy(comp,optimal_comp,greedy_complen_mml8);
				comp_complen = greedy_complen_mml8;
				
				/*
				// change optimal MML here? or leave it?
				// -> pretty meh
				// -> this is stomped to 3 anyway
				mml = 8;
				/**/

				// do change passinfo but not mml :
				passinfo = passinfo_alt;
				passinfo_chunktype = chunktype_mml8;
			}
		}
	
		#endif	
	
		// set mml to 3	regardless of the greedy choice !
		// @@@@ -> just do this always in Leviathan ?
		if ( ( vtable->level >= OodleLZ_CompressionLevel_Optimal3
			|| vtable->compressor == OodleLZ_Compressor_Hydra ) &&
			pOptions->minMatchLen <= 3 )
		{
			mml = 3;
		}
	}
	
	rrPrintf_v2("greedy complen : %d chunktype : %d = %s ; mml %d ; did_packet_pos = %d ; offset_alt_modulo = %d ; min_offset = %d \n",
		comp_complen,passinfo_chunktype,newlz_literals_type_name[passinfo_chunktype],mml,
		(int)passinfo.did_packet_pos,passinfo.offset_alt_modulo,min_offset);
	
	//-----------------------------------------------------------------------------
	
	if ( num_tlls > 1 )
	{
		last_tll = num_tlls-1;
		memset(last_tll_len,0,sizeof(int)*(chunk_len+1));
	}
	
	//-----------------------------------------------------------------------------
	// optimal parse!
	
	/*
	// do_optimal_iter will repeat the parse for statistics refinement
	//	-> now done only if literal type changes
	//  -> this is much more common in Leviathan than in Kraken, much bigger net effect on ratio & encode time
	bool do_optimal_iter = ( vtable->level >= OodleLZ_CompressionLevel_Optimal4 );
	*/
	
	
	newlzhc_codecosts * pcodecosts;
	scratch->newlzhc_codecosts_space.extend( sizeof(newlzhc_codecosts), arena);
	pcodecosts = (newlzhc_codecosts *) scratch->newlzhc_codecosts_space.get();
	newlzhc_codecosts & codecosts = *pcodecosts;
				
	// take chunktype from the greedy parse :
	codecosts.chunktype = passinfo_chunktype;
	
	//-----------------------------------------

	// @@!! options for seeding histo :
	//	from greedy parse
	//	from last chunk's optimal
	//	from flat model
	//	blend?
	//	can use it for only the first chunk
	//	or scale it down over time
	//	or just add it in as a base
	// wipe out the histos :
	//rrMemSet32_Aligned(&passinfo,1,sizeof(passinfo));
	
	// In LRL loop, we keep lrl_iterator <= max_search_lrl
	// and want to keep track of match candidates
	constexpr int MAX_MATCH_CANDIDATES = 8 + 1;
	RR_ASSERT(max_search_lrl + 1 <= MAX_MATCH_CANDIDATES);
	
	// passinfo currently contains the greedy pass
		
	// only carry if chunktype is the same it was
	//	 even carrying both types of literals, it's important to only do this if it matches
	// carried_encoder_state_chunktype = -1 on first chunk
	if ( vtable->carried_encoder_state.size() > 0 )
	{
		// blend the previous optimal + the current greedy
		//	maybe very marginally better (than just taking previous), not big
		
		newlz_scratchblock & carried_encoder_state = const_cast<newlz_scratchblock &>(vtable->carried_encoder_state);
		RR_ASSERT( carried_encoder_state.size() == sizeof(newlzhc_carried_encoder_state) );
		const newlzhc_carried_encoder_state * p_prev_state = (const newlzhc_carried_encoder_state *) carried_encoder_state.get();		
		
		// what if offset_alt_modulo is different? -> optimal_rescale can change passinfo.offset_alt_modulo
		
		// when optimal_iter == 1 , carried_state has been filled from iter 0
		if ( p_prev_state->chunktypes[optimal_iter] != -1 )
		{			
			bool literals_same = ( p_prev_state->chunktypes[optimal_iter] == codecosts.chunktype );

			optimal_rescale_passinfo2(passinfo,p_prev_state->passinfos[optimal_iter],literals_same);
		}
		else
		{
			// else it comes from the greedy pass
			optimal_rescale_passinfo(passinfo);
		}
	}
	else
	{
		// else it comes from the greedy pass
		optimal_rescale_passinfo(passinfo);
	}
	
 	// NOTE : do after optimal_rescale_passinfo2 , which can change this!
	codecosts.offset_alt_modulo = passinfo.offset_alt_modulo;
	
	optimal_passinfo_to_codecost(passinfo,codecosts);

	if ( min_offset == eNewLZ_MinOffset_1 )
	{
		// cannot cost offsets with 44 offsets when doing min_offset 1 parse :	
		RR_ASSERT( codecosts.offset_alt_modulo != 0 );
		RR_ASSERT( codecosts.chunktype == NEWLZ_LITERALS_TYPE_RAW || codecosts.chunktype == NEWLZ_LITERALS_TYPE_O1 );
	}
	
	// wipe all the arrivals :
	for(int i=0;i<=chunk_len*num_tlls;i++)
	{
		tll_arrivals[i].cost = RR_S32_MAX;
	}
	
	tll_arrivals[start_pos*num_tlls+0].cost = 0;
	tll_arrivals[start_pos*num_tlls+0].los.Reset();
	tll_arrivals[start_pos*num_tlls+0].ml = 0;
	tll_arrivals[start_pos*num_tlls+0].lrl = 0;
	tll_arrivals[start_pos*num_tlls+0].prev_index = 0;
	tll_arrivals[start_pos*num_tlls+0].is_twostep = 0;
	RR_DURING_ASSERT( tll_arrivals[start_pos*num_tlls+0].check_offset = 0 );
	RR_DURING_ASSERT( tll_arrivals[start_pos*num_tlls+0].check_arrival_pos = start_pos );
	
	int final_arrival_pos = -1;
	S32 final_arrival_cost = RR_S32_MAX;
	S32 final_lo = 0;

	{
	SIMPLEPROFILE_SCOPE_N(optimal_parse_tll,chunk_len);
	
	// parse in chunks :
	int parse_chunk_start = start_pos;
	parsevec.clear();	        
                      
	for(;;)
	{
		RR_ASSERT( parse_chunk_start < chunk_len );
		if ( parse_chunk_start >= parse_end_pos )
		{
			RR_ASSERT( false ); // should not happen
			break;
		}
						
		int parse_chunk_end = parse_chunk_start + parse_chunk_len;
		parse_chunk_end = RR_MIN(parse_chunk_end,parse_end_pos);
		
		int parse_chunk_end_max = parse_chunk_start + parse_chunk_len_max;
		parse_chunk_end_max = RR_MIN(parse_chunk_end_max,parse_end_pos);
		
		// avoid tiny tails :
		if ( parse_chunk_end_max >= parse_end_pos-16 ) parse_chunk_end_max = parse_end_pos;
		if ( parse_chunk_end >= parse_end_pos-16 ) parse_chunk_end = parse_end_pos;
		
		// cheap_preceding_arrival_pos is the last arrival that's <= pos
		//	(there are arrivals filled past that spot)
		// this chunk chain must start from parse_chunk_start
		int cheap_preceding_arrival_pos = parse_chunk_start;
		S32 cheap_preceding_arrival_literal_cost = 0;
		// -> cheap_preceding_arrival_pos is only used in non-TLL mode (num_tlls=1)
		
		// I should have a true packet arrival at parse_chunk_start :
		RR_ASSERT( tll_arrivals[ parse_chunk_start*num_tlls ].cost != RR_S32_MAX );
		// but there should be nothing past :
		//RR_ASSERT( tll_arrivals[ (parse_chunk_start+1)*num_tlls ].cost == RR_S32_MAX );
		
		//-------------------------
		
		{
		SIMPLEPROFILE_SCOPE(tll_parse_inner);
		
		if ( num_tlls > 1 )
		{
			// wipe TLL arrival creep-over :
					
			for(int p=1;p<num_tlls;p++)
			{
				// should be no true packet arrivals :
				//RR_ASSERT( tll_arrivals[(parse_chunk_start+p)*num_tlls].cost == RR_S32_MAX );
				
				for(int t=0;t<num_tlls;t++)
				{
					tll_arrivals[(parse_chunk_start+p)*num_tlls + t].cost = RR_S32_MAX;
				}
			}
			
			for(int t=1;t<num_tlls;t++)
			{
				tll_arrivals[parse_chunk_start*num_tlls + t].cost = RR_S32_MAX;
			}
			
			// anything beyond should be unfilled :
			RR_ASSERT( tll_arrivals[(parse_chunk_start + num_tlls)*num_tlls].cost == RR_S32_MAX );
			RR_ASSERT( tll_arrivals[(parse_chunk_start + num_tlls)*num_tlls + 1].cost == RR_S32_MAX );
			
			//-------------------------
			
			if ( (parse_chunk_end-parse_chunk_start) <= num_tlls ) // just skip tiny tail
			{
				RR_ASSERT( parse_chunk_end == parse_end_pos );
				parse_chunk_end = parse_end_pos = parse_chunk_start;
				goto parse_chunk_break_out;
			}
			
			//-------------------------
			
			// fill initial tlls from parse_chunk_start arrival :
			//  this stops any shitlet tails we left from last time
			for(int t=1;t<num_tlls;t++)
			{
				tll_arrivals[(parse_chunk_start+t)*num_tlls + t] = tll_arrivals[(parse_chunk_start + t-1)*num_tlls + (t-1)];
				
				S32 lo = tll_arrivals[parse_chunk_start*num_tlls].los.LastOffset();
				tll_arrivals[(parse_chunk_start+t)*num_tlls + t].cost += cost_add_literal(chunk_ptr,parse_chunk_start+t-1,lo,codecosts,t-1);
			}
			last_tll_len[parse_chunk_start+last_tll] = last_tll;
		}
		
		// iterate up to parse_chunk_end
		// but have to stop where there's a valid arrival
		//	(I only have arrivals from full packets, not bare literals)
		
		// NOTE : in TLL, parse_chunk_end is the last true packet arrival
		//	eg. TLL steps go *past* parse_chunk_end	
		
		for(int pos=parse_chunk_start; 
			parse_chunk_end <= parse_chunk_end_max;
			pos++)
		{
			// if we reach parse_end_pos, we're done :
			if ( pos == parse_end_pos )
			{
				parse_chunk_end = parse_end_pos;
				break;
			}
		
			const U8 * ptr = chunk_ptr + pos;
			U32 ptr32 = RR_GET32_NATIVE_UNALIGNED(ptr);

			newlzhc_optimal_arrival_tll * my_arrivals = tll_arrivals + pos * num_tlls;
			// my_arrivals [num_tlls]
			
			{
			//SIMPLEPROFILE_SCOPE(inner1);
			
			//---------------------------------------------------------------
			
			if ( num_tlls == 1 )
			{
			
				if ( pos != cheap_preceding_arrival_pos )  // this is true every time except the first iteration at parse_chunk_start
				{			
					// extend cheap_preceding_arrival_literal_cost from earlier cheap_preceding_arrival_pos
					S32 lo = tll_arrivals[cheap_preceding_arrival_pos].los.LastOffset();
					int lrl = pos - cheap_preceding_arrival_pos;
					RR_ASSERT( lrl >= 1 );
					cheap_preceding_arrival_literal_cost += cost_add_literal(chunk_ptr,pos-1,lo,codecosts,lrl-1);
					
					NEWLZHC_ASSERT_HEAVY( newlzhc_cost_literals(chunk_ptr,cheap_preceding_arrival_pos,lrl,lo,codecosts,0) == cheap_preceding_arrival_literal_cost );
				
					if ( tll_arrivals[pos].cost != RR_S32_MAX )
					{
						RR_ASSERT( parse_chunk_end >= pos );
						
						// I have a valid arrival
						// see if I should update cheap_preceding_arrival_pos to current pos
									
						// fix broken ass optimal parse
						//	do not give up the current "cheap_preceding_arrival_pos"
						//	if the cost to arrive there + literals to current pos
						//   is cheaper than the current arrival
						//	eg. don't always go to the *closest* arrival
						//	  go back to the *cheapest* arrival , duh
						// in non-TLL mode, this is how we find literal runs as an alternative to packets

						S32 prev_cost = tll_arrivals[cheap_preceding_arrival_pos].cost;

						// we're considering alternative of keeping this lrl
						//	vs. sending a packet with a 0 lrl
						S32 cost_lrl = 0;
						//int lrl_for_packet = lrl;
						if ( lrl >= NEWLZHC_PACKET_LRL_MAX )
						{
							//lrl_for_packet = NEWLZ_PACKET_LRL_MAX;
							int excess = lrl - NEWLZHC_PACKET_LRL_MAX;
							cost_lrl = cost_excess_lrl(excess,codecosts);
						}				
					
						//RR_ASSERT( lrl_for_packet <= NEWLZ_PACKET_LRL_MAX );
						//cost_lrl += lrl_for_packet; // hacky bullshit
						
						S32 cost = prev_cost + cost_lrl + cheap_preceding_arrival_literal_cost;
						
						if ( cost > tll_arrivals[pos].cost )
						{
							// I have a valid arrival AND I like it better than ignoring it and staying LRL
												
							cheap_preceding_arrival_pos = pos;
							cheap_preceding_arrival_literal_cost = 0;
							
							// if I reach parse_chunk_end it means there are no further arrivals
							//	go ahead and end this parse chunk now
							// I can only terminate a parse chunk at an arrival
							//	 (if there are no matches, parse chunks can get very long)
							if ( pos >= parse_chunk_end )
							{
								parse_chunk_end = pos;
								goto parse_chunk_break_out;
							}
						}
					}
				}
				
				NEWLZHC_ASSERT_HEAVY( newlzhc_cost_literals(chunk_ptr,cheap_preceding_arrival_pos,(pos - cheap_preceding_arrival_pos),tll_arrivals[cheap_preceding_arrival_pos].los.LastOffset(),codecosts,0) == cheap_preceding_arrival_literal_cost );
			}
			else // num_tlls > 1
			{
				// if I reach parse_chunk_end it means there are no further arrivals
				//	go ahead and end this parse chunk now
				// @@@@?? only do this if I do have a valid arrival
				if ( pos >= parse_chunk_end )
				{
					// if I don't have an arrival at [pos]
					//	set parse_chunk_end to the closest arrival before me
					// -> not the closet
					//	find the cheapest at [pos][tll] and go back to there
					S32 lowest_cost = RR_S32_MAX;
					S32 lowest_arrival_pos = 0;
					for(int t=0;t<num_tlls;t++)
					{
						if ( my_arrivals[t].cost < lowest_cost )
						{
							lowest_cost = my_arrivals[t].cost;
							// where's the true packet arrival that landed in this TLL :
							S32 arrival_pos = pos;
							if ( t == last_tll ) arrival_pos -= last_tll_len[pos];
							else arrival_pos -= t;
							
							RR_ASSERT( tll_arrivals[arrival_pos*num_tlls].cost != RR_S32_MAX );
							RR_ASSERT( tll_arrivals[arrival_pos*num_tlls].cost <= lowest_cost );
							RR_ASSERT( arrival_pos == my_arrivals[t].check_arrival_pos );
							
							lowest_arrival_pos = arrival_pos;
						}
					}
					RR_ASSERT( lowest_cost != RR_S32_MAX );
					//RR_ASSERT( lowest_arrival_pos > parse_chunk_start ); 
					if ( lowest_arrival_pos >= parse_chunk_end )
					{		
						parse_chunk_end = lowest_arrival_pos;
						goto parse_chunk_break_out;
					}
				}		
			
				//---------------------------------------------------------------
			
				// @@@@?? try carrying forward from my_arrivals[0] as well
				//		-> that shouldn't help, make sure it doesn't
			
				if ( my_arrivals[last_tll].cost != RR_S32_MAX )
				{
					// continue long tll arrival forward :
					// note that a normal last_tll arrival at pos+1 would already be filled from
					//	the packet tail of (pos+1-last_tll)
					
					S32 lo = my_arrivals[last_tll].los.LastOffset();
					
					S32 cost = my_arrivals[last_tll].cost + cost_add_literal(chunk_ptr,pos,lo,codecosts,last_tll_len[pos]);
					
					// + num_tlls steps you to pos+1
					newlzhc_optimal_arrival_tll & dest_arrival = my_arrivals[last_tll + num_tlls];
					if ( cost < dest_arrival.cost )
					{
						// does not change parse_chunk_end
						dest_arrival = my_arrivals[last_tll];
						dest_arrival.cost = cost;
						last_tll_len[pos+1] = last_tll_len[pos] + 1;
					}
				}
			}				
			
			}
			//---------------------------------------------------------------
			
			int cur_matches_count = 0;
			UnpackedMatchPair cur_matches[NEWLZHC_MATCH_NUM_PAIRS*2+1];
			
			const UnpackedMatchPair * pairs = matches[pos].pairs;
			
			{
			//SIMPLEPROFILE_SCOPE(inner2);
			
			// for all normal matches :
			for(int m=0;m<NEWLZHC_MATCH_NUM_PAIRS;m++)
			{
				S32 ml = pairs[m].length;
				
				// matches are in descending length
				// so when we hit a short one, get out
				if ( ml < mml ) // ml == 0 is the end-of-data marker
					break;
				
				U32 offset = pairs[m].offset;

				ml = RR_MIN(ml, rrPtrDiff32(ptr_matchend - ptr));
				
				// verify match :
				RR_ASSERT( memcmp(ptr,ptr-(SINTa)offset,ml) == 0 );
				
				if ( offset >= dictionarySize )
					continue;
												
				if ( offset < NEWLZ_MIN_OFFSET )
				{
					RR_ASSERT( offset != 0 );
					RR_ASSERT( min_offset == 1 || min_offset == 8 );
					if ( min_offset == 1 )
					{
						// can do offset as is but must clamp len for no overlap
						
						ml = RR_MIN(ml,(S32)offset);
						
						if ( ml >= mml )
						{
							cur_matches[cur_matches_count].length = ml;
							cur_matches[cur_matches_count].offset = offset;
							cur_matches_count++;
						}
						
						// drop through and try striding too
					}
				
					offset = newLZ_offset44_round_up_tiny(offset);
					if ( (SINTa)offset > rrPtrDiff(ptr - dictionaryBase) )
						continue;
					
					// mml4 ? should be mml3 ?
					//  -> changing to mml3 saves a microscopic few bytes
					//	-> just leave it alone
					ml = getmatchlen_mml4_one32(ptr32,ptr,ptr-offset,ptr_matchend);
					//ml = getmatchlen_mml3_one32(ptr32,ptr,ptr-offset,ptr_matchend);
					if ( ml < mml )
						continue;
				}
							
				RR_ASSERT( ml >= mml );
				
				// don't check isAllowed in optimal parse
				//	it's just for guiding the offset statistics
				//  instead allow arbitrariy offsets, and let the cost-chooser
				//	 make its own decision
				// -> yes! cool
				if ( ! newLZHC_IsAllowedNormalMatch_Optimal(ml,offset,pOptions) )
					continue;
					
				cur_matches[cur_matches_count].length = ml;
				cur_matches[cur_matches_count].offset = offset;
				cur_matches_count++;
			}
			
			// also always check offset 8 :
			RR_ASSERT( min_offset == 1 || min_offset == 8 );
			if ( min_offset == NEWLZ_MIN_OFFSET )
			{
				S32 ml,offset;
				offset = NEWLZ_MIN_OFFSET;
				
				// @@ mml4 here to discourage over-use ?
				ml = getmatchlen_mml3_one32(ptr32,ptr,ptr-offset,ptr_matchend);
				
				if ( ml >= mml )
				{
					cur_matches[cur_matches_count].length = ml;
					cur_matches[cur_matches_count].offset = offset;
					cur_matches_count++;
				}
			}
			
			}	
									
			int longestml = 0;
			
			// try LRL 0-6 :
			//	OR if cheap_preceding_arrival_pos is > 6 away, just try that (always LRL excess)
			
			S32 lowest_base_cost = RR_S32_MAX;
			
			S32 match_lrl[MAX_MATCH_CANDIDATES];
			S32 match_base_cost[MAX_MATCH_CANDIDATES];
			S32 match_prev_index[MAX_MATCH_CANDIDATES];
			S32 match_longestlo[MAX_MATCH_CANDIDATES];
			SINTa match_lrl_candidate_count = 0;
			
			{
			//SIMPLEPROFILE_SCOPE(inner3);
			
			int cheap_preceding_arrival_lrl = pos - cheap_preceding_arrival_pos;
			
			// lrl_iterator<=max_search_lrl
			// AND lrl <= max_search_lrl BUT always do lrl == cheap_preceding_arrival_lrl
			for(int lrl_iterator=0;;lrl_iterator++)
			{
				S32 base_cost;
				int lrl;
				S32 prev_index;
				
				if ( num_tlls == 1 )
				{				
					// 12-19-2017 : changed LRL iteration
					//	if cheap_preceding was > max_search ,
					//	 I used to just do one LRL = cheap_preceding
					// based on the archaic idea that cheap_preceding = closest
					// but it's not, there can be arrivals closer
					// so instead :
					//   scan back all low LRL's normally
					//  at the longest LRL,
					//	 if chea_preceding is further back, do it instead
				
					if ( lrl_iterator > max_search_lrl ) break;
						
					lrl = lrl_iterator;
					
					if ( lrl == max_search_lrl && cheap_preceding_arrival_lrl > max_search_lrl )
					{
						lrl = cheap_preceding_arrival_lrl;
					}
					
					prev_index = pos-lrl;
					if ( prev_index < parse_chunk_start )
						break;
					
					S32 arrival_cost = tll_arrivals[prev_index].cost;
					if ( arrival_cost == RR_S32_MAX )
						continue;
					
					const newLZHC_LOs_NoPad & los = tll_arrivals[prev_index].los;
					
					// cost the sub literals :
					base_cost = arrival_cost;
					
					S32 lo = los.LastOffset();
					RR_ASSERT( lo >= min_offset );
							
					if ( lrl == cheap_preceding_arrival_lrl )
					{
						NEWLZHC_ASSERT_HEAVY( cheap_preceding_arrival_literal_cost == newlzhc_cost_literals(chunk_ptr,pos-lrl,lrl,lo,codecosts,0) );
						base_cost += cheap_preceding_arrival_literal_cost;
					}
					else
					{			
						// we only do long lrl spans at cheap_preceding_arrival_lrl
						RR_ASSERT( lrl <= max_search_lrl );	
						base_cost += newlzhc_cost_literals(chunk_ptr,pos-lrl,lrl,lo,codecosts,0);
					}					
				}
				else // num_tlls > 1 
				{				
					if ( lrl_iterator < num_tlls )
					{
						// arrival at pos via tll
						// this is an arrival at [pos-t]
						//	stored in [pos][t]
						//	carried lrl t will go in packet
					
						S32 arrival_cost = my_arrivals[lrl_iterator].cost;
						if ( arrival_cost == RR_S32_MAX )
							continue;
					
						lrl = lrl_iterator;
						if ( lrl_iterator == last_tll )
							lrl = last_tll_len[pos];
						
						// pos-lrl is where this packet starts
						RR_ASSERT( (pos - lrl) >= parse_chunk_start );
						
						RR_ASSERT( (pos-lrl) == my_arrivals[lrl_iterator].check_arrival_pos );
						
						prev_index = pos*num_tlls + lrl_iterator; // not my prev, this is my index, the prev that I am filling
						RR_ASSERT( tll_check_index_pos((pos-lrl),prev_index,num_tlls) );
							
						base_cost = arrival_cost;
					}
					else // lrl_iterator >= num_tlls
					{
						// search lrl beyond the tll range
						// just take an arrival and cost the literals
					
						if ( lrl_iterator > max_search_lrl ) break;
				
						// -1 to make sure we check last_tll
						lrl = lrl_iterator-1;
						
						if ( (pos - lrl) < parse_chunk_start )
							break;
										
						prev_index = (pos - lrl)*num_tlls; // true packet arrival
						S32 arrival_cost = tll_arrivals[prev_index].cost;
						if ( arrival_cost == RR_S32_MAX )
							continue;
							
						base_cost = arrival_cost + 
							newlzhc_cost_literals(chunk_ptr,pos-lrl,lrl,tll_arrivals[prev_index].los.LastOffset(),codecosts,0);
					}
				}
				
				int lrl_for_packet = lrl;
				// base_cost includes lrl :
				if ( lrl >= NEWLZHC_PACKET_LRL_MAX )
				{
					lrl_for_packet = NEWLZHC_PACKET_LRL_MAX;
					int excess = lrl - NEWLZHC_PACKET_LRL_MAX;
					base_cost += cost_excess_lrl(excess,codecosts);
				}	
					
				// arrival lands at (pos - lrl)
				// arrival_cost includes the literals in lrl
				// but does not include the cost of the lrl itself
				
				const newLZHC_LOs_NoPad & los = tll_arrivals[prev_index].los;
				
				// find LO matches :
				// have to do this each time because LO set may be different
		
				// note longestlo is per-lrl, while longestml is overall
				int longestlo = 0;

				{
				//SIMPLEPROFILE_SCOPE(inner3_lo); // <- all the optimal parse time is here
				// this is the big slow meat of the optimal parse
				//	just doing getmatchlen_mml2 + try_lo_arrival over and over
				// @@ can we speed this up?

				for(int loi=0;loi<NEWLZHC_NUM_LAST_OFFSETS;loi++)
				{
					S32 offset = los.lasts[loi];
					int ml;
					if ( ! havematch_mml2_one32(ptr32,ptr,ptr-offset,ptr_matchend, &ml ) )
						continue;
			
					if ( offset < 8 ) ml = RR_MIN(ml,(int)offset);
		
					// force higher LO index only for longer matches :
					if ( ml <= longestlo )
						continue;
					
					longestlo = ml; // NOTE: ml > longestlo established above
					parse_chunk_end = RR_MAX(parse_chunk_end,pos+ml);
					
					// cost it
					S32 first_step_cost = try_lo_arrival_tll(tll_arrivals,num_tlls,prev_index,chunk_ptr,last_tll_len,
												pos,base_cost,lrl,lrl_for_packet,ml,loi,los,codecosts);
					
					// also try ml reduction
					// NOTE : these break lit-after-match exclusion and LRL0-LO0 exclusion
					//	but seem to help anyway
					
					if ( ml_range_for_lomatch_red.contains(ml) )
					{
						// -> try all lens
						for(int l=NEWLZHC_LOMML;l<ml;l++)
						{
							try_lo_arrival_tll(tll_arrivals,num_tlls,prev_index,chunk_ptr,last_tll_len,
												pos,base_cost,lrl,lrl_for_packet,l,loi,los,codecosts);
						}
					}
					
					#ifdef TWO_STEP_MAX_LRL
					// look for two-step :				
					if ( pos+ml < twostep_end_pos )
					{
						try_two_step_arrival_tll(tll_arrivals,num_tlls,prev_index,chunk_ptr,ptr_matchend,&parse_chunk_end,
							pos+ml,first_step_cost,ml,lrl,loi,offset,los,codecosts);
					}
					#endif
				}
				}
				
				longestml = RR_MAX(longestlo,longestml);
				
				// early out to avoid repeating long getmatchlen's in degenerate cases :
				if ( longestml >= optimal_skip_len )
				{
					// this goto is the same as "break"
					goto long_ml_skip;
				}
			
				// Now try which normal matches improve on the base cost and are worth trying later.
				// We keep track of all candidates because LRL-ML correlation in the packet can
				// make the cost landscape somewhat complicated.
				if ( base_cost < lowest_base_cost )
				{
					RR_ASSERT( match_lrl_candidate_count < MAX_MATCH_CANDIDATES );

					lowest_base_cost = base_cost;
					match_lrl[match_lrl_candidate_count] = lrl;
					match_base_cost[match_lrl_candidate_count] = base_cost;
					match_prev_index[match_lrl_candidate_count] = prev_index;
					match_longestlo[match_lrl_candidate_count] = longestlo;
					match_lrl_candidate_count++;
				}
			} // lrl_iterator loop
			
			} // profile scope
			
			RR_ASSERT( lowest_base_cost != RR_S32_MAX ); // I should have had some arrival

			for(int m=0;m<cur_matches_count;m++)
			{
				S32 ml = cur_matches[m].length;
				S32 offset = cur_matches[m].offset;

				S32 best_cost = RR_S32_MAX;
				SINTa best_i = -1;
				
				// Of all candidate LRLs, check which one gives us the lowest combined cost of arrival,
				// literals, and packet byte. This accounts for LRL-ML correlation in the packet byte.
				for (SINTa i = 0; i < match_lrl_candidate_count; ++i)
				{
					const S32 my_lrl = match_lrl[i];
					const S32 my_packet_lrl = RR_MIN(my_lrl, NEWLZHC_PACKET_LRL_MAX);
					const S32 my_base_cost = match_base_cost[i];
					const S32 my_cost = my_base_cost + cost_normal_match(my_packet_lrl, ml, codecosts, pos - my_lrl);
					if ( my_cost < best_cost )
					{
						best_cost = my_cost;
						best_i = i;
					}
				}

				if ( best_i < 0 )
					continue;

				const S32 base_cost = match_base_cost[best_i];
				const S32 prev_index = match_prev_index[best_i];
				const S32 lrl = match_lrl[best_i];
				const S32 longestlo = match_longestlo[best_i];

				const S32 lrl_for_packet = RR_MIN(lrl, NEWLZHC_PACKET_LRL_MAX);
				const newLZHC_LOs_NoPad & los = tll_arrivals[prev_index].los;

				RR_ASSERT( ml >= mml );
				RR_ASSERT( ptr+ml <= ptr_matchend );
				RR_ASSERT( memcmp(ptr,ptr-(SINTa)offset,ml) == 0 );
				RR_ASSERT( offset <= NEWLZ_MAX_OFFSET );
				RR_ASSERT( offset >= min_offset );
				//RR_ASSERT( newLZ_IsAllowedNormalMatch(ml,offset) );
				
				// enforce normal > LO :
				if ( ml <= longestlo )
					break;				
				
				// no LO matches!
				// this should not happen because ml > longestlo 
				RR_ASSERT( ml > longestlo );
				RR_ASSERT( los.Find(offset) == -1 );
				
				// okay, it's allowed - cost it :
				
				longestml = RR_MAX(ml,longestml);
				
				parse_chunk_end = RR_MAX(parse_chunk_end,pos+ml);
				
				// start with cost including lrl & offset
				S32 cost = base_cost + cost_offset(offset,codecosts);
				
				S32 first_step_cost = try_match_arrival_tll(tll_arrivals,num_tlls,prev_index,chunk_ptr,last_tll_len,
											pos,cost,lrl,lrl_for_packet,ml,offset,los,codecosts);
				
				/*
				if ( ml >= optimal_skip_len )
				{
					goto long_ml_skip;
					//continue;
				}
				*/		
				
				if ( ml_range_for_match_red.contains(ml) )
				{
					// try len reductions
										
					// all lens
					for(int l=mml;l<ml;l++)
					{
						try_match_arrival_tll(tll_arrivals,num_tlls,prev_index,chunk_ptr,last_tll_len,
											pos,cost,lrl,lrl_for_packet,l,offset,los,codecosts);
					}
				}
				
				#ifdef TWO_STEP_MAX_LRL
				if ( pos+ml < twostep_end_pos )
				{
					try_two_step_arrival_tll(tll_arrivals,num_tlls,prev_index,chunk_ptr,ptr_matchend,&parse_chunk_end,
						pos+ml,first_step_cost,ml,lrl,-1,offset,los,codecosts);
				}
				#endif
			}
			
			// if long ml - jump ahead !
			if ( longestml >= optimal_skip_len )
			{
				long_ml_skip:
				
				RR_ASSERT( longestml >= optimal_skip_len );
				
				int longestml_arrival_pos = pos+longestml;
				
				// my arrival should be filled, and should have pushed parse_chunk_end >= longestml_arrival_pos
				RR_ASSERT( longestml_arrival_pos <= parse_chunk_end );
				RR_ASSERT( tll_arrivals[longestml_arrival_pos*num_tlls].cost != RR_S32_MAX );

				if ( longestml_arrival_pos == parse_chunk_end )
				{
					// longestml took us to chunk end
					// just jump to the end and terminate parse chunk
					parse_chunk_end = cheap_preceding_arrival_pos = longestml_arrival_pos;
					goto parse_chunk_break_out;
				}
			
				pos = longestml_arrival_pos-1; // (-1 because of pos++ in loop)
				
				if ( num_tlls == 1 )
				{
					// invalidate carried state :
					cheap_preceding_arrival_pos = pos+1;
					cheap_preceding_arrival_literal_cost = 0;
				}
				else
				{
					// I need to fill trailing literals from here
					//	 (at pos+1)				
					S32 lo = tll_arrivals[longestml_arrival_pos*num_tlls].los.LastOffset();	
					for(int t=1;t<num_tlls;t++)
					{
						tll_arrivals[(longestml_arrival_pos+t)*num_tlls + t] = tll_arrivals[(longestml_arrival_pos + t-1)*num_tlls + (t-1)];
						
						tll_arrivals[(longestml_arrival_pos+t)*num_tlls + t].cost += cost_add_literal(chunk_ptr,longestml_arrival_pos+t-1,lo,codecosts,t-1);
					}
					last_tll_len[longestml_arrival_pos+last_tll] = last_tll;
				}
			}
		} // pos++ loop
		parse_chunk_break_out:
		
		;
		} // profile scope
		
		//rrPrintf_v2("adapt chunk len : %d\n",(parse_chunk_end - parse_chunk_start));
		
		// I should now have an arrival at parse_chunk_end that traces back to parse_chunk_start
		//	unless parse_chunk_end == parse_end_pos in which case I might not have an arrival
		
		// not true ; parse_end_pos is the last allowed match start location
		//	parse_chunk_end is the last found packet arrival pos
		//RR_ASSERT( parse_chunk_end <= parse_end_pos );
		RR_ASSERT( tll_arrivals[parse_chunk_end*num_tlls].cost != RR_S32_MAX || parse_chunk_end == parse_end_pos );
		RR_ASSERT( num_tlls > 1 || cheap_preceding_arrival_pos >= parse_chunk_start );
		
		S32 parse_chunk_end_index = parse_chunk_end * num_tlls;
		
		RR_ASSERT( final_arrival_pos == -1 ); // eof has not been hit yet
		// if parse_chunk_end is close to parse_end_pos , just finish
		//	 tiny tail won't do any good
		if ( parse_chunk_end >= parse_end_pos-2 )
		{
			//SIMPLEPROFILE_SCOPE(tll_parse_eof);
			
			RR_ASSERT( final_arrival_cost == RR_S32_MAX );
			
			// we're at EOF
			//	I may have a final arrival at [parse_chunk_end]
			//	there may be a cheaper arrival before that at cheap_preceding_arrival_pos
							
			#define FINAL_EXTRA_BACKUP 8
				
			if ( num_tlls == 1 )
			{
				RR_ASSERT( cheap_preceding_arrival_pos >= parse_chunk_start );
				RR_ASSERT( cheap_preceding_arrival_pos <= parse_chunk_end );
				
				int backup_start_pos = RR_MAX(cheap_preceding_arrival_pos-FINAL_EXTRA_BACKUP,parse_chunk_start);

				for(int pos=backup_start_pos;pos<chunk_len;pos++)
				{
					S32 cost = tll_arrivals[pos].cost;
					if ( cost == RR_S32_MAX )
						continue;
				
					//const U8 * ptr = chunk_ptr + pos;
					S32 lo = tll_arrivals[pos].los.LastOffset();
						
					cost += newlzhc_cost_literals(chunk_ptr,pos,(chunk_len - pos),lo,codecosts,0);
					
					if ( cost < final_arrival_cost )
					{
						final_arrival_cost = cost;
						final_arrival_pos = pos;
					}
				}
								
				parse_chunk_end_index = final_arrival_pos;
			}
			else
			{				
				int backup_start_pos = RR_MAX(parse_chunk_end-FINAL_EXTRA_BACKUP,parse_chunk_start);

				for(int pos=backup_start_pos;pos<chunk_len;pos++)
				{
					for(int t=0;t<num_tlls;t++)
					{
						S32 index = pos*num_tlls + t;
						S32 cost = tll_arrivals[index].cost;
						if ( cost == RR_S32_MAX )
							continue;
					
						// I already have the literal cost from arrival_pos up to pos
						//	add on the literals from there to the end 
						// no LRL cost in the tail
					
						S32 lo = tll_arrivals[index].los.LastOffset();
										
						int lrl = t;
						if ( t == last_tll ) lrl = last_tll_len[pos];
						int arrival_pos = pos - lrl;
						
						RR_ASSERT( tll_arrivals[index].check_arrival_pos == arrival_pos );
						if ( arrival_pos < parse_chunk_start )
							continue;
						
						// arrivals at parse_chunk_start can only be tll 0 :
						//  OR it must just be the literal continuation of the same arrival
						RR_ASSERT( arrival_pos > parse_chunk_start || t == 0 || \
							( tll_arrivals[index].prev_index == tll_arrivals[parse_chunk_start*num_tlls].prev_index && \
							tll_arrivals[index].los.Equals( tll_arrivals[parse_chunk_start*num_tlls].los ) ) );
						
						// TLL arrival cost already includes the literals from arrival_pos -> pos
						//	add on the cost from pos -> chunk_len
						
						cost += newlzhc_cost_literals(chunk_ptr,pos,(chunk_len - pos),lo,codecosts,lrl);
						
						if ( cost < final_arrival_cost )
						{						
							final_arrival_cost = cost;
							final_arrival_pos = arrival_pos;
							
							parse_chunk_end_index = index;
						}
					}
				}
			}
			
			RR_ASSERT( final_arrival_cost != RR_S32_MAX ); 
			RR_ASSERT( final_arrival_pos >= 0 );
			RR_ASSERT( final_arrival_pos >= parse_chunk_start );
		
			parse_chunk_end = final_arrival_pos;
			
			RR_ASSERT( tll_check_index_pos(final_arrival_pos,parse_chunk_end_index,num_tlls) );
			final_lo = tll_arrivals[parse_chunk_end_index].los.LastOffset();
		}
	
		{
		//SIMPLEPROFILE_SCOPE(tll_parse_reverse);		
		
		// reverse the parse & output it
		// fill parsevec by tracing back through arrivals :
		chunk_parsevec.clear();
		int rpos = parse_chunk_end;
		int rindex = parse_chunk_end_index;
		for(;;)
		{
			newlzhc_optimal_arrival_tll & arrival = tll_arrivals[rindex];
			RR_ASSERT( tll_check_index_pos(rpos,rindex,num_tlls) );
			RR_ASSERT( arrival.check_arrival_pos == rpos );

			RR_ASSERT( rpos >= parse_chunk_start );
			if ( rpos == parse_chunk_start )
			{
				break;
			}
			
			if ( arrival.is_twostep )
			{
				int twostep_prev_pos = rpos - arrival.twostep.ml - arrival.twostep.lrl;
				RR_ASSERT( twostep_prev_pos < rpos );
				RR_ASSERT( twostep_prev_pos >= parse_chunk_start );
		
				// first the twostep LO match :		
				chunk_parsevec.push_back();
				newlzhc_encoder_parse & parse = chunk_parsevec.back();
				parse.lastoffset = arrival.los.LastOffset(); // LO0 after the
				parse.offset = 0; // LO0
				parse.ml = arrival.twostep.ml;
				parse.lrl = arrival.twostep.lrl;
				RR_ASSERT( parse.IsLO() );
			
				// verify the match :
				RR_DURING_ASSERT( const U8 * twostep_ptr = chunk_ptr + rpos - arrival.twostep.ml );
				RR_ASSERT( memcmp(twostep_ptr,twostep_ptr - parse.lastoffset,arrival.twostep.ml) == 0 );
			
				rpos = twostep_prev_pos;
			}
			
			int prev_pos = rpos - arrival.ml - arrival.lrl;
			RR_ASSERT( prev_pos < rpos );
			RR_ASSERT( prev_pos >= parse_chunk_start );
			
			int prev_index = arrival.prev_index;
			RR_ASSERT( tll_check_index_pos(prev_pos,prev_index,num_tlls) );
			
			const newlzhc_optimal_arrival_tll & prev_arrival = tll_arrivals[prev_index];
			
			RR_ASSERT( prev_arrival.check_arrival_pos == prev_pos );
			RR_ASSERT( prev_pos >= parse_chunk_start );
			
			const newLZHC_LOs_NoPad & prev_los = prev_arrival.los;
			S32 offset = arrival.los.LastOffset();
			S32 loi = prev_los.Find(offset);
			if ( loi >= 0 ) offset = -loi;
				
			RR_ASSERT( offset == arrival.check_offset );
				
			#ifdef RR_DO_ASSERTS
			{
				newLZHC_LOs_NoPad los;
				if ( offset > 0 ) los.SetAdd(prev_los,offset);
				else los.SetMTF(prev_los,-offset);
				RR_ASSERT( memcmp(&los,&arrival.los,sizeof(los)) == 0 );
			}
			#endif
				
			chunk_parsevec.push_back();
			newlzhc_encoder_parse & parse = chunk_parsevec.back();

			// I need the LO to apply to my literals; that's the *previous* LO
			parse.lastoffset = prev_arrival.los.LastOffset();
			parse.lrl = arrival.lrl;
			parse.ml = arrival.ml;
			parse.offset = offset;

			rpos = prev_pos;
			rindex = prev_index;
		}
		
		// should have traced back to packet start
		RR_ASSERT( rpos == parse_chunk_start );
		RR_ASSERT( tll_check_index_pos(parse_chunk_start,rindex,num_tlls) );
		RR_ASSERT( tll_arrivals[rindex].check_arrival_pos == parse_chunk_start );
		
		// final rindex should always be at parse_chunk_start, TLL 0 :
		//	rindex == parse_chunk_start*num_tlls
		// -> not exactly, it can be the trivial literal TLL continuation from there
		//  instead check that they are the same :
		RR_ASSERT( tll_arrivals[rindex].prev_index == tll_arrivals[parse_chunk_start*num_tlls].prev_index );
		RR_ASSERT( tll_arrivals[rindex].los.Equals( tll_arrivals[parse_chunk_start*num_tlls].los ) );
				
		// chunk_parsevec is back-to-front, reverse it :
		reverse(chunk_parsevec.begin(),chunk_parsevec.end());

		parsevec.appendv(chunk_parsevec);

		} // profile scope
		
		//RR_ASSERT( parse_chunk_end < parse_end_pos ); // <- not true, parse_chunk_end can be at the end of the last match
		RR_ASSERT( parse_chunk_end <= (chunk_len - NEWLZHC_MATCH_END_PAD) );
		
		RR_ASSERT( parse_chunk_end <= final_arrival_pos || final_arrival_pos < 0 );
		if ( parse_chunk_end == final_arrival_pos )
		{
			// this is the normal end of parsing
			break;
		}
			
		optimal_rescale_passinfo_incremental(passinfo);
	
		optimal_update_passinfo(
			chunk_parsevec,
			parse_chunk_start,
			passinfo,
			chunk_ptr);
				
		optimal_passinfo_to_codecost(
			passinfo,codecosts);
		
		parse_chunk_start = parse_chunk_end;
	
	} // parse chunks loop
	
	} // timing scope
	
	//=============================================================================

	if ( parsevec.empty() )
	{
		rrPrintf_v2("parsevec empty!\n");
	
		return comp_complen;		
	}
	
	RR_ASSERT( final_arrival_cost != RR_S32_MAX );
	RR_ASSERT( final_arrival_pos >= 0 );
	RR_ASSERT( final_lo != 0 );

	rrPrintf_v2("final_arrival_cost : %d\n",final_arrival_cost/(COST_ONE_BIT*8));

	//=============================================================================
	
	// fill histo :
	
	// can reuse passinfo :
	RR_ZERO(passinfo);
	newlzhc_histo & histo = passinfo.literals;

	{
		int cur_pos = start_pos;
		for LOOPVEC(parseveci,parsevec)
		{
			const newlzhc_encoder_parse & cur_parse = parsevec[parseveci];

			newLZHC_histo_literals(&histo, chunk_ptr, cur_pos, cur_parse.lrl, cur_parse.lastoffset );
			cur_pos += cur_parse.lrl + cur_parse.ml;
		}
	}
	
	// write final sequence
	// trailing LRL :
	SINTa final_lrl = chunk_len - final_arrival_pos;	
		
	if ( final_lrl > 0 )
	{
		newLZHC_histo_literals(&histo,chunk_ptr,final_arrival_pos,final_lrl,final_lo);
	}
	
	//=============================================================================
	
	// output the parsevec :
		
	int optimal_chunktype;
	F32 optimal_J = LAGRANGE_COST_INVALID;
	// fills passinfo for carried_state :
	SINTa optimal_complen = newLZHC_put_parse(scratch,&optimal_J,&histo,&optimal_chunktype,chunk_ptr,chunk_len,optimal_comp,optimal_comp_end,chunk_pos,vtable,final_lo,parsevec,start_pos,&passinfo,min_offset);
	
	rrPrintf_v2("optimal complen : %d chunktype : %d = %s ; did_packet_pos = %d ; offset_alt_modulo = %d\n",
		optimal_complen,optimal_chunktype,newlz_literals_type_name[optimal_chunktype],
		(int)passinfo.did_packet_pos,passinfo.offset_alt_modulo);

	if ( *pJ < optimal_J )
	{
		rrPrintf_v2("%8d : previous better  : [%.3f] < %.3f : [%d] vs %d\n",chunk_pos,*pJ,optimal_J,comp_complen,optimal_complen);
	}
	else
	{
		rrPrintf_v2("%8d : current better : %.3f >= [%.3f] : %d vs [%d]\n",chunk_pos,*pJ,optimal_J,comp_complen,optimal_complen);
	}
	
	if ( optimal_complen < chunk_len )
	{
		// @@ do this even if optimal_J is worse than *pJ ?
	
		newlz_scratchblock & carried_encoder_state = const_cast<newlz_scratchblock &>(vtable->carried_encoder_state);
	
		if ( carried_encoder_state.size() != sizeof(newlzhc_carried_encoder_state) )
		{
			carried_encoder_state.extend( sizeof(newlzhc_carried_encoder_state) , arena);
			newlzhc_carried_encoder_state * p_carried_state_init = (newlzhc_carried_encoder_state *) carried_encoder_state.get();
			p_carried_state_init->chunktypes[0] = -1;
			p_carried_state_init->chunktypes[1] = -1;
		}
		newlzhc_carried_encoder_state * p_carried_state = (newlzhc_carried_encoder_state *) carried_encoder_state.get();
		p_carried_state->passinfos[optimal_iter] = passinfo;
		p_carried_state->chunktypes[optimal_iter] = optimal_chunktype;
	}	

	// optimal_complen can be worse than greedy sometimes ; revert?
	if ( optimal_J < *pJ )
	{
		if ( min_offset == 1 )
		{
			rrPrintf_v2("chose optimal min offset 1\n");
		}
	
		RR_ASSERT( optimal_complen < chunk_len );
		*pchunktype = optimal_chunktype;
		*pJ = optimal_J;
		memcpy(comp,optimal_comp,optimal_complen);
		comp_complen = optimal_complen;
		
		/*
		// if I'm iteratable && literal type changed :
		// this gets hit on lzt03 ; you save 100-1000 bytes (per chunk) by iterating
		if ( do_optimal_iter && optimal_iter == 0 && codecosts.chunktype != optimal_chunktype )
		{
			// *pchunktype is used for what's stored in [comp] AND
			//	also for how should I parse the current chunk
			
			rrPrintf_v2("literal type changed, iterating : %d -> %d\n",codecosts.chunktype,optimal_chunktype);
	
			// newLZ_put_parse filled "passinfo"
			
			// invalidate carried state so we will work off "passinfo" :
			const_cast<newlz_vtable *>(vtable)->carried_encoder_state_chunktype = -1;
			
			// our fallback is now the first pass of optimal :
			//	(must match *pchunktype)
			greedy_J = optimal_J;
			greedy_complen = optimal_complen;
			
			continue;
		}
		*/
	
	}
	else
	{
		/*
		if ( optimal_iter == 0 )
		{
			rrPrintf_v2("Greedy reverted : %d -> %d : %d == %.3f%%\n",optimal_complen,greedy_complen,
				(optimal_complen - greedy_complen),(optimal_complen - greedy_complen)*100.0/chunk_len);
		}
		*/
	}
	
	} // optimal_iter

	// note comp_complen = chunk_len is possible here
	//	if both greedy passes expanded

	// comp_complen = len of [comp]
	return comp_complen;
}

//=======================================================================================

#if OODLE_PLATFORM_HAS_ADVANCED_MATCHERS

OODLE_NS_END
#include "suffixtrie.h"
OODLE_NS_START

#endif // OODLE_PLATFORM_HAS_ADVANCED_MATCHERS


void Leviathan_FillVTable(
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
		
	int table_bits = GetLZMatchTableBits(raw_len,
						RR_MAX(level,OodleLZ_CompressionLevel_VeryFast), // avoid SuperFast
						pOptions,16,20,17,24);
	
	// compressor can be Hydra or Kraken, decodeType is always Kraken
	vtable.compressor = compressor;
	vtable.decodeType = RAD_LZ_DECODE_LEVIATHAN;
	vtable.level = level;

	vtable.lambda = NEWLZHC_LAMBDA_LEVIATHAN;
	// scale lambda by spaceSpeedTradeoffBytes  :
	vtable.lambda *= newlz_spaceSpeedTradeoffBytes_for_lambda(pOptions) * (1.f / OODLELZ_DEFAULT_SSTB);
	
	vtable.chunk_len = NEWLZHC_CHUNK_LEN; // @@ parameter? and have to transmit it?
	vtable.pOptions = pOptions;
	vtable.try_huff_chunks = true;

	vtable.speedfit = speedfit_get_default();
		
	// Leviathan always >= version 2.6.0 so TANS/RLE allowed
	vtable.entropy_flags = NEWLZ_ARRAY_FLAG_ALLOW_HUFF6;
	vtable.entropy_flags |= NEWLZ_ARRAY_FLAG_ALLOW_TANS | NEWLZ_ARRAY_FLAG_ALLOW_RLE | NEWLZ_ARRAY_FLAG_ALLOW_RLEHUFF;
	vtable.entropy_flags |= NEWLZ_ARRAY_FLAG_ALLOW_SPLIT | NEWLZ_ARRAY_FLAG_ALLOW_SPLIT_INDEXED;
	vtable.entropy_flags |= NEWLZ_ARRAY_FLAG_ALLOW_HUFFLENS2;
	vtable.entropy_flags |= NEWLZ_ARRAY_FLAG_ALLOW_RLE_MEMSET;
		
	// newlzhc doesn't test this, but set it anyway :
	vtable.bitstream_flags = NEWLZ_BITSTREAM_FLAG_ALT_OFFSETS;
	
	//-----------------------------------------------------
	
	// @@ totally randomly turn off some of the slower array modes at lower compress level
	//	(this needs tweak!)
	// at Normal level , everything is on
	// @@ ?? turn off indexed splits in Normal ?
	if ( level <= OodleLZ_CompressionLevel_Fast )
		vtable.entropy_flags &= ~NEWLZ_ARRAY_FLAG_ALLOW_SPLIT_INDEXED;
	if ( level <= OodleLZ_CompressionLevel_VeryFast )
		vtable.entropy_flags &= ~NEWLZ_ARRAY_FLAG_ALLOW_SPLIT;
	if ( level <= OodleLZ_CompressionLevel_SuperFast )
		vtable.entropy_flags &= ~NEWLZ_ARRAY_FLAG_ALLOW_TANS;
		
	//-----------------------------------------------------
	// set up newLZ "vtable" :

	if ( level <= OodleLZ_CompressionLevel_SuperFast )
	{
		// if not explicitly given, limit table to 19 bits :
		if ( pOptions->matchTableSizeLog2 <= 0 ) table_bits = RR_MIN(table_bits,19);
	
		newlz_vtable_setup_ctmf<newLZHC_CTMF_SuperFast>(&vtable,dictionaryBase,raw,table_bits,arena);
		
		vtable.fp_encode_chunk = newLZHC_encode_chunk<newLZHC_CTMF_SuperFast,0,1,5>;
	}
	else if ( level <= OodleLZ_CompressionLevel_VeryFast )
	{
		newlz_vtable_setup_ctmf<newLZHC_CTMF_VeryFast>(&vtable,dictionaryBase,raw,table_bits,arena);

		vtable.fp_encode_chunk = newLZHC_encode_chunk<newLZHC_CTMF_VeryFast,0,1,0>;
	}
	else if ( level == OodleLZ_CompressionLevel_Fast )
	{
		if ( pOptions->matchTableSizeLog2 <= 0 ) table_bits = RR_MIN(table_bits,21);

		newlz_vtable_setup_ctmf<newLZHC_CTMF_Fast>(&vtable,dictionaryBase,raw,table_bits,arena);

		vtable.fp_encode_chunk = newLZHC_encode_chunk<newLZHC_CTMF_Fast,1,1,0>;
	}
	#if OODLE_PLATFORM_HAS_ADVANCED_MATCHERS
	else if ( level >= OodleLZ_CompressionLevel_Optimal2 )
	{
		vtable.carried_encoder_state.reserve(sizeof(newlzhc_carried_encoder_state), arena);
		vtable.fp_encode_chunk = newLZHC_encode_chunk_optimal_tll;
		vtable.fp_create_match_finder = SuffixTrie_CreateMatchFinder;
		vtable.find_all_matches_num_pairs = NEWLZHC_MATCH_NUM_PAIRS;
		vtable.wants_dic_limit_splits = true;
	}
	else if ( level == OodleLZ_CompressionLevel_Optimal1 )
	{
		vtable.carried_encoder_state.reserve(sizeof(newlzhc_carried_encoder_state), arena);
		vtable.fp_encode_chunk = newLZHC_encode_chunk_optimal_tll;
		vtable.fp_create_match_finder = CTMF_CreateMatchFinder;
		vtable.find_all_matches_num_pairs = NEWLZHC_MATCH_NUM_PAIRS;
		vtable.wants_dic_limit_splits = false;
	}
	#endif
	else 
	//if ( level == OodleLZ_CompressionLevel_Normal )
	{
		newlz_vtable_setup_ctmf<newLZHC_CTMF_Normal>(&vtable,dictionaryBase,raw,table_bits,arena);

		vtable.fp_encode_chunk = newLZHC_encode_chunk<newLZHC_CTMF_Normal,2,1,0>;
	}
}


SINTa Leviathan_Compress(
	OodleLZ_Compressor compressor,
	const U8 * raw,U8 * comp,SINTa raw_len,
	OodleLZ_CompressionLevel level,
	const OodleLZ_CompressOptions * pOptions,
	const U8 * dictionaryBase,
	const LRMCascade * lrm_casc,
    rrArenaAllocator * arena)
{
	SIMPLEPROFILE_SCOPE(Leviathan_Compress);
	
	rrArenaAllocatorStateSaver saver(arena); //,newlz_arena_alloc_size_if_none);
	newlz_vtable vtable;
	
	Leviathan_FillVTable(&vtable,compressor,raw_len,level,pOptions,dictionaryBase,raw,arena);
	
	// raw == NULL is for OodleLZ_GetCompressScratchMemBound
	if ( raw == NULL )
		return newlz_enc_mem_bound(&vtable,raw_len);
	
	SINTa ret = newlz_compress_vtable(&vtable,
		raw,comp,raw_len,
		dictionaryBase,
		lrm_casc,
		arena);

	return ret;
}

//=================================================================

// verify OODLELZ_MAX_SCRATCH_FOR_PHASE_HEADERS_AND_FUZZ is big enough :
//	the sizeof changes in different builds, so OODLELZ_MAX_SCRATCH_FOR_PHASE_HEADERS_AND_FUZZ should be an overestimate
RR_COMPILER_ASSERT( (NEWLZHC_EXTRA_SCRATCH_MEM_FOR_FUZZ + sizeof(newLZHC_chunk_arrays)) <= OODLELZ_MAX_SCRATCH_FOR_PHASE_HEADERS_AND_FUZZ );
	
OODLE_NS_END
