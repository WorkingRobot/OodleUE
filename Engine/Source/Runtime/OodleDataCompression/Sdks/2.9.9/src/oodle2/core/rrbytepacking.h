// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_BYTEPACKING_H__
#define __RADRR_BYTEPACKING_H__

#include "rrbase.h"
#include "oodlecore.h"

OODLE_NS_START

//===============================================================
/*

BytePacking routines

WARNING : LOTS OF CODE DUPE WITH OODLEFILE

should all take a U8 * and return a U8 * , usage is like :

U8 * ptr = buffer;

ptr = rrPutBytes_U16(ptr,val1);
ptr = rrPutBytes_U24(ptr,val2);

@@ maybe a little faster to use RR_GET/PUT32 macros

--

see also clamps/checks in cbradutil

*/
//===============================================================

RADFORCEINLINE U8 * rrPutBytes_U8(U8 * ptr,U8 val)
{
	*ptr++ = val;
	return ptr;
}

RADFORCEINLINE U8 * rrPutBytes_U16(U8 * ptr,U16 val)
{
	*ptr++ = (U8)(val>>8);
	*ptr++ = (U8)(val);
	return ptr;
}

RADFORCEINLINE U8 * rrPutBytes_U24(U8 * ptr,U32 val)
{
	RR_ASSERT( val < (1<<24) );
	*ptr++ = (U8)(val>>16);
	*ptr++ = (U8)(val>>8);
	*ptr++ = (U8)(val);
	return ptr;
}

RADFORCEINLINE U8 * rrPutBytes_U32(U8 * ptr,U32 val)
{
	*ptr++ = (U8)(val>>24);
	*ptr++ = (U8)(val>>16);
	*ptr++ = (U8)(val>>8);
	*ptr++ = (U8)(val);
	return ptr;
}

RADFORCEINLINE U8 * rrPutBytes_U64(U8 * ptr,U64 val)
{
	ptr = rrPutBytes_U32(ptr,(U32)(val>>32));
	ptr = rrPutBytes_U32(ptr,(U32)(val));
	return ptr;
}

RADFORCEINLINE const U8 * rrGetBytes_U8(const U8 * ptr,U8 *pVal)
{
	*pVal = *ptr++;
	return ptr;
}

RADFORCEINLINE const U8 * rrGetBytes_U16(const U8 * ptr,U16 *pVal)
{
	*pVal  = ((U16) *ptr++) << 8;
	*pVal |= ((U16) *ptr++);
	return ptr;
}

RADFORCEINLINE const U8 * rrGetBytes_U24(const U8 * ptr,U32 *pVal)
{
	*pVal  = ((U32) *ptr++) << 16;
	*pVal |= ((U32) *ptr++) << 8;
	*pVal |= ((U32) *ptr++);
	return ptr;
}

RADFORCEINLINE const U8 * rrGetBytes_U32(const U8 * ptr,U32 *pVal)
{
	*pVal  = ((U32) *ptr++) << 24;
	*pVal |= ((U32) *ptr++) << 16;
	*pVal |= ((U32) *ptr++) << 8;
	*pVal |= ((U32) *ptr++);
	return ptr;
}

RADFORCEINLINE const U8 * rrGetBytes_U64(const U8 * ptr,U64 *pVal)
{
	U32 lo,hi;
	ptr = rrGetBytes_U32(ptr,&hi);
	ptr = rrGetBytes_U32(ptr,&lo);
	*pVal = ((U64)hi<<32) | lo;
	return ptr;
}

//===============================================================
// packing floats to ints/bytes :

int   rrFloatPackToInt(float val, float lo, float hi, int bits);
float rrFloatUnpackInt(int i, float lo, float hi, int bits);

OOINLINE U8 rrFloatPackTo8(float val, float low, float hi)
{
	return (U8) rrFloatPackToInt(val,low,hi,8);
}

OOINLINE U16 rrFloatPackTo16(float val, float low, float hi)
{
	return (U16) rrFloatPackToInt(val,low,hi,16);
}

OOINLINE float rrFloatUnpack8(U8 byte, float low, float hi)
{
	return rrFloatUnpackInt(byte,low,hi,8);
}

OOINLINE float rrFloatUnpack16(U16 w, float low, float hi)
{
	return rrFloatUnpackInt(w,low,hi,16);
}

//===============================================================

RADFORCEINLINE U8 * rrPut8_rarely_16(U8 * ptr,U16 val)
{
	if ( val < 0xFF )
	{
		ptr = rrPutBytes_U8(ptr,(U8)val);
	}
	else
	{
		ptr = rrPutBytes_U8(ptr,0xFF);
		ptr = rrPutBytes_U16(ptr,val);
	}
	return ptr;
}


RADFORCEINLINE const U8 * rrGet8_rarely_16(const U8 * ptr,U16 *pVal)
{
	U8 v8;
	ptr = rrGetBytes_U8(ptr,&v8);
	if ( v8 < 0xFF )
		*pVal = v8;
	else
		ptr = rrGetBytes_U16(ptr,pVal);
	return ptr;
}

// 16_to_64 is useful for file sizes
//	val can be up to 62 bits long
//	writes 2 bytes or 4 bytes when possible
//		(this is not really ideal for file sizes;
//		in particular we should do 3 bytes sometimes;
//		but it's pretty irrelevant)

RADFORCEINLINE U8 * rrPut16_to_64(U8 * ptr,U64 val)
{
	if ( val < (1<<15) )
	{
		U16 v16 = (U16) val;
		ptr = rrPutBytes_U16(ptr,v16);
	}
	else if ( val < (1UL<<30) )
	{
		U32 v32 = (U32) val;
		v32 |= (1UL<<31); // top bit flagged
		//OodleFile_Put16(oof,(U16)(v32>>16));
		//OodleFile_Put16(oof,(U16)(v32&0xFFFF));
		ptr = rrPutBytes_U32(ptr,v32);
	}
	else
	{
		// top two bits must be off :
		RR_ASSERT( val < (1ULL<<62) );

		U64 v64 = val | (3ULL<<62); // top two bits flagged
		ptr = rrPutBytes_U64(ptr,v64);
	}
	return ptr;
}


RADFORCEINLINE const U8 * rrGet16_to_64(const U8 * ptr,U64 *pVal)
{
	U16 v16;
	ptr = rrGetBytes_U16(ptr,&v16);
	if ( (v16 & (1<<15)) == 0 )
	{
		*pVal = v16;
		return ptr;
	}
	v16 ^= (1<<15);
	
	U32 v32 = ((U32)v16)<<16;
	ptr = rrGetBytes_U16(ptr,&v16);
	v32 |= v16;
	
	if ( (v32 & (1<<30)) == 0 )
	{
		*pVal = v32;
		return ptr;
	}
	
	v32 ^= (1<<30);
	
	U64 v64 = ((U64)v32)<<32;
	ptr = rrGetBytes_U32(ptr,&v32);
	v64 |= v32;
	*pVal = v64;
	return ptr;
}

// OodleFile_Put7Variable
//	writes 1 byte if it fits in 7 bits
//	2 bytes if it fits in 14 bits, etc.
RADFORCEINLINE U8 * rrPut7Variable(U8 * ptr,U64 orig)
{
	U64 val = orig;
	for(;;)
	{
		U8 cur = (U8)(val&0x7F);
		if ( val == cur )
		{
			ptr = rrPutBytes_U8(ptr,cur);
			return ptr;
		}
		else
		{
			cur |= 0x80;
			ptr = rrPutBytes_U8(ptr,cur);
			val >>= 7;
		}
	}
}

RADFORCEINLINE const U8 * rrGet7Variable(const U8 * ptr,U64 *pVal)
{
	U64 ret = 0;
	int shift = 0;
	for(;;)
	{
		U8 cur = *ptr++;
		RR_ASSERT( (ret>>shift) == 0 );
		ret |= ((U64)(cur & 0x7F)<<shift);
		if ( cur < 0x80 )
		{
			*pVal = ret;
			return ptr;
		}
		shift += 7;
	}
}

//===============================================================

// EncodeMod :
//	with pow2 mod
//	bits=0 is mod=1 is flag-value 255
//	bits=7 is mod=128 is the same as rrPut7Variable
U8 *		rrPutVariableModPow2(U8 * to, U32 val, int bits);
const U8 *	rrGetVariableModPow2(const U8 * from, const U8 * end, U32 * pVal, int bits);

// Series : put one byte with bits1 , then remaining bytes with bits2
U8 *		rrPutVariableModPow2Series2(U8 * to, U32 val, int bits1, int bits2);
const U8 *	rrGetVariableModPow2Series2(const U8 * from, const U8 * end, U32 * pVal, int bits1, int bits2);

U8 *		rrPutVariableModPow2Series3(U8 * to, U32 val, int bits1, int bits2, int bits3);
const U8 *	rrGetVariableModPow2Series3(const U8 * from, const U8 * end, U32 * pVal, int bits1, int bits2, int bits3);

// SeriesWB : put one *WORD* with bits, then remaining *BYTES* with bits2
U8 *		rrPutVariableModPow2SeriesWB(U8 * to, U32 val, int bits1, int bits2);
const U8 *	rrGetVariableModPow2SeriesWB(const U8 * from, const U8 * end, U32 * pVal, int bits1, int bits2);

U8 * rrPut64VariableModPow2(U8 * to, U64 val, int bits);
const U8 * rrGet64VariableModPow2(const U8 * from, const U8 * end, U64 * pVal, int bits);

U8 * rrPut64VariableModPow2SeriesWB(U8 * to, U64 val, int bits1, int bits2);
const U8 * rrGet64VariableModPow2SeriesWB(const U8 * from, const U8 * end, U64 * pVal, int bits1, int bits2);

U8 *		rrPutVariableModPow2_FileSize(U8 * to, S64 val);
const U8 *	rrGetVariableModPow2_FileSize(const U8 * from, const U8 * end, S64 * pVal);

//===============================================================

U8 *		rrPutVariableModPow2SeriesWBA(U8 * to, SINTa val, int bits1, int bits2);
const U8 *	rrGetVariableModPow2SeriesWBA(const U8 * from, const U8 * end, SINTa * pVal, int bits1, int bits2);

OODLE_NS_END

#endif // __RADRR_BYTEPACKING_H__

