// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrbytepacking.h"
#include "rrmath.h"
#include "templates/rrstl.h"

OODLE_NS_START

//PackToInt is [lo,hi) - inclusive on lo, exclusive on hi
int rrFloatPackToInt(F32 val, F32 lo, F32 hi, int bits)
{
	RR_ASSERT( val >= lo && val <= hi );

	const int steps = (1<<bits);
	const F32 stepSizeInv = ((F32)steps) / (hi - lo);
	int i = rr_froundint( (val - lo) * stepSizeInv );
	i = RR_CLAMP(i,0,steps-1);
	//i = RR_MIN((U32)i,(U32)(steps-1));
	
	// note : this only reaches to (hi - stepSize) - 
	
	return i;
}

float rrFloatUnpackInt(int i, F32 lo, F32 hi, const int bits)
{
	const int steps = (1<<bits);
	const F32 scale = 1.f / ((F32)steps);
	F32 stepSize = (hi - lo) * scale;
	return i * stepSize;
}

//=====================================================

U8 * rrPutVariableModPow2(U8 * to, U32 val, int bits)
{
	RR_ASSERT( bits >= 0 && bits < 8 );
	const U32 mod = (1U<<bits);
	const U32 upper = 256 - mod;
	for(;;)
	{		
		if ( val < upper )
		{
			*to++ = (U8)(mod + val);
			return to;
		}
		else
		{
			// val >= upper
			val -= upper;
			int lower = val & (mod-1);
			*to++ = (U8)lower;
			val >>= bits;
		}
	}
}

const U8 * rrGetVariableModPow2(const U8 * from, const U8 * end, U32 * pVal, int bits)
{
	RR_ASSERT( bits >= 0 && bits < 8 );
	const int mod = (1<<bits);
	int shift = 0;
	int val = 0;
	for(;;)
	{
		if ( from >= end ) return NULL;
		int byte = *from++;
		byte -= mod;
		if ( byte >= 0 )
		{
			val += byte<<shift;
			*pVal = val;
			return from;
		}
		else
		{
			val += (byte+256)<<shift;
			shift += bits;
		}
	}
}

U8 * rrPutVariableModPow2Series2(U8 * to, U32 val, int bits1, int bits2)
{
	RR_ASSERT( bits1 >= 0 && bits1 < 8 );
	const U32 mod = (1U<<bits1);
	const U32 upper = 256 - mod;
		
	if ( val < upper )
	{
		*to++ = (U8)(mod + val);
		return to;
	}
	else
	{
		// val >= upper
		val -= upper;
		int lower = val & (mod-1);
		*to++ = (U8)lower;
		val >>= bits1;
		
		return rrPutVariableModPow2(to,val,bits2);
	}
}

const U8 * rrGetVariableModPow2Series2(const U8 * from, const U8 * end, U32 * pVal, int bits1, int bits2)
{
	RR_ASSERT( bits1 >= 0 && bits1 < 8 );
	const int mod = (1<<bits1);

	if ( from >= end ) return NULL;
	int byte = *from++;
	byte -= mod;
	if ( byte >= 0 )
	{
		*pVal = byte;
		return from;
	}
	else
	{
		U32 top = 0; // unnecessary initialize for warning
		from = rrGetVariableModPow2(from,end,&top,bits2);
		*pVal = (top<<bits1) + (byte + 256);
		return from;
	}
}

U8 * rrPutVariableModPow2Series3(U8 * to, U32 val, int bits1, int bits2, int bits3)
{
	RR_ASSERT( bits1 >= 0 && bits1 < 8 );
	const U32 mod = (1U<<bits1);
	const U32 upper = 256 - mod;
		
	if ( val < upper )
	{
		*to++ = (U8)(mod + val);
		return to;
	}
	else
	{
		// val >= upper
		val -= upper;
		int lower = val & (mod-1);
		*to++ = (U8)lower;
		val >>= bits1;
		
		return rrPutVariableModPow2Series2(to,val,bits2,bits3);
	}
}

const U8 * rrGetVariableModPow2Series3(const U8 * from, const U8 * end, U32 * pVal, int bits1, int bits2, int bits3)
{
	RR_ASSERT( bits1 >= 0 && bits1 < 8 );
	const int mod = (1<<bits1);

	if ( from >= end ) return NULL;
	int byte = *from++;
	byte -= mod;
	if ( byte >= 0 )
	{
		*pVal = byte;
		return from;
	}
	else
	{
		U32 top;
		from = rrGetVariableModPow2Series2(from,end,&top,bits2,bits3);
		*pVal = (top<<bits1) + (byte + 256);
		return from;
	}
}


U8 * rrPutVariableModPow2SeriesWB(U8 * to, U32 val, int bits1, int bits2)
{
	RR_ASSERT( bits1 >= 0 && bits1 < 16 );
	const U32 mod = (1U<<bits1);
	const U32 upper = (1<<16) - mod;
		
	if ( val < upper )
	{
		return rrPutBytes_U16(to, (U16)(mod+val) );
	}
	else
	{
		// val >= upper
		val -= upper;
		int lower = val & (mod-1);
		to = rrPutBytes_U16(to, (U16)lower );
		val >>= bits1;
		
		return rrPutVariableModPow2(to,val,bits2);
	}
}

const U8 * rrGetVariableModPow2SeriesWB(const U8 * from, const U8 * end, U32 * pVal, int bits1, int bits2)
{
	RR_ASSERT( bits1 >= 0 && bits1 < 16 );
	const int mod = (1<<bits1);
	const int upper = (1<<16) - mod;

	if ( from >= end-1 ) return NULL;
	U16 word;
	from = rrGetBytes_U16(from,&word);
	if ( word >= mod )
	{
		*pVal = word - mod;
		return from;
	}
	else
	{
		U32 top = 0; // unnecessary initialize for warning
		from = rrGetVariableModPow2(from,end,&top,bits2);
		*pVal = (top<<bits1) + (word + upper);
		return from;
	}
}


U8 * rrPut64VariableModPow2(U8 * to, U64 val, int bits)
{
	RR_ASSERT( bits >= 0 && bits < 8 );
	const U64 mod = ((U64)1<<bits);
	const U64 upper = 256 - mod;
	for(;;)
	{		
		if ( val < upper )
		{
			*to++ = (U8)(mod + val);
			return to;
		}
		else
		{
			// val >= upper
			val -= upper;
			U64 lower = val & (mod-1);
			*to++ = (U8)lower;
			val >>= bits;
		}
	}
}

const U8 * rrGet64VariableModPow2(const U8 * from, const U8 * end, U64 * pVal, int bits)
{
	RR_ASSERT( bits >= 0 && bits < 8 );
	const U64 mod = ((U64)1<<bits);
	int shift = 0;
	U64 val = 0;
	for(;;)
	{
		if ( from >= end ) return NULL;
		S64 byte = *from++;
		byte -= mod;
		if ( byte >= 0 )
		{
			val += byte<<shift;
			*pVal = val;
			return from;
		}
		else
		{
			val += (byte+256)<<shift;
			shift += bits;
		}
	}
}

U8 * rrPut64VariableModPow2SeriesWB(U8 * to, U64 val, int bits1, int bits2)
{
	RR_ASSERT( bits1 >= 0 && bits1 < 16 );
	const U64 mod = ((U64)1<<bits1);
	const U64 upper = (1<<16) - mod;
		
	if ( val < upper )
	{
		return rrPutBytes_U16(to, (U16)(mod+val) );
	}
	else
	{
		// val >= upper
		val -= upper;
		U64 lower = val & (mod-1);
		to = rrPutBytes_U16(to, (U16)lower );
		val >>= bits1;
		
		return rrPut64VariableModPow2(to,val,bits2);
	}
}

const U8 * rrGet64VariableModPow2SeriesWB(const U8 * from, const U8 * end, U64 * pVal, int bits1, int bits2)
{
	RR_ASSERT( bits1 >= 0 && bits1 < 16 );
	const U64 mod = ((U64)1<<bits1);
	const U64 upper = (1<<16) - mod;

	if ( from >= end-1 ) return NULL;
	U16 word;
	from = rrGetBytes_U16(from,&word);
	if ( word >= mod )
	{
		*pVal = word - mod;
		return from;
	}
	else
	{
		U64 top = 0; // unnecessary initialize for warning
		from = rrGet64VariableModPow2(from,end,&top,bits2);
		*pVal = (top<<bits1) + (word + upper);
		return from;
	}
}

U8 *		rrPutVariableModPow2_FileSize(U8 * to, S64 val)
{
	RR_ASSERT( val >= 0 );
	return rrPut64VariableModPow2SeriesWB(to,val,13,4);
}

const U8 *	rrGetVariableModPow2_FileSize(const U8 * from, const U8 * end, S64 * pVal)
{
	return rrGet64VariableModPow2SeriesWB(from,end,(U64 *)pVal,13,4);
}

U8 *		rrPutVariableModPow2SeriesWBA(U8 * to, SINTa val, int bits1, int bits2)
{
	#ifdef __RAD64__
	U64 v = check_value_cast<U64>(val);
	return rrPut64VariableModPow2SeriesWB(to,v,bits1,bits2);
	#else
	U32 v = check_value_cast<U32>(val);
	return rrPutVariableModPow2SeriesWB(to,v,bits1,bits2);
	#endif
}

const U8 *	rrGetVariableModPow2SeriesWBA(const U8 * from, const U8 * end, SINTa * pVal, int bits1, int bits2)
{
	#ifdef __RAD64__
	U64 v;
	const U8 * ret = rrGet64VariableModPow2SeriesWB(from,end,&v,bits1,bits2);
	#else
	U32 v;
	const U8 * ret = rrGetVariableModPow2SeriesWB(from,end,&v,bits1,bits2);
	#endif
	
	*pVal = check_value_cast<SINTa>(v);
	return ret;
}

OODLE_NS_END
