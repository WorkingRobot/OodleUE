// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once
#ifndef __RR_ARITHCODERH__
#define __RR_ARITHCODERH__

#include "oodlecore.h"

OODLE_NS_START

/**

Arithmetic Coder

**/

struct rrArithEncoder
{
    U32 low;
    U32 range;
    U8 * ptr;
    U8 * start;
};

struct rrArithDecoder
{
    U32 code;
    U32 range;
    const U8 * ptr;
    const U8 * start;
};

// Encoder & Decoder are identical, but they're nominally two different types to make sure
//	you call the right thing in the right place

/**

WARNING :

unlike VarBits and the old ArithBits you can *NOT* save your state by just copying the rrArithEncoder struct,
because the carries are propagated back into the actual bits!  If you do some encoding, the carries might get
propagated into the code stream, then when you restore the struct, they are still there!

see rrArithCoderNCP

**/

#define RR_ARITHCODER_CUMPROBMAX_SHIFT  (24)
#define RR_ARITHCODER_CUMPROBMAX        (1<<RR_ARITHCODER_CUMPROBMAX_SHIFT)

//---------------------------------------------------------------------------------------

void rrArithEncodeInit( rrArithEncoder* ac, void * ptr );
void rrArithDecodeInit( rrArithDecoder* ac, void const * ptr );

// Flush returns number of output bytes
SINTa rrArithEncodeFlush( rrArithEncoder* ac );

SINTa rrArithEncodeTellPos( const rrArithEncoder* ac );
SINTa rrArithDecodeTellPos( const rrArithDecoder* ac );

// information for optimizations :
//	gets # of bits actually written plus bits pending in the range :
F32 rrArithEncodeGetBitsInState( const rrArithEncoder* ac );
F32 rrArithDecodeGetBitsInState( const rrArithDecoder* ac );

//---------------------------------------------------------------------------------------
// Basic range encoder :
//  pass in cumulative frequency counts

void rrArithEncodeRange( rrArithEncoder* ac , U32 cumfreqLow, U32 freq, U32 cumfreqTot );

// Peek is mutating ; call it only once, always call Peek/Remove in a pair
//  Peek returns a target in [0,cumfreqTot)  (not inclusive on top)
U32 rrArithDecodePeek( rrArithDecoder* ac , U32 cumfreqTot );

// Find the symbol such that target >= cumfreqLow && target < (cumfreqLow+ freq) , then call Remove()
void rrArithDecodeRemove( rrArithDecoder* ac , U32 cumfreqLow, U32 freq, U32 cumfreqTot );

// PeekScaled avoids a divide and gives you back (target * scale)
//  target is in the range [0,cumfreqTot*scale] (note inclusive on top of range)
U32 rrArithDecodePeekScaled( rrArithDecoder* ac , U32 cumfreqTot , U32 * pScale);

//---------------------------------------------------------------------------------------
// "Pow2" versions of range encoder avoid the divide when cumfreqTot = 1 << cumfreqTotShift

void rrArithEncodeRangePow2( rrArithEncoder* ac , U32 cumfreqLow, U32 freq, U32 cumfreqTotShift );

// WARNING : PeekPow2 is a little different than Peek() in that it returns a target in [0,1<<cumfreqTotShift] (inclusive on top)
U32  rrArithDecodePeekPow2( rrArithDecoder* ac , U32 cumfreqTotShift );
void rrArithDecodeRemovePow2( rrArithDecoder* ac , U32 cumfreqLow, U32 freq, U32 cumfreqTotShift );
U32  rrArithDecodePeekScaledPow2( rrArithDecoder* ac , U32 cumfreqTotShift , U32 * pScale);

//---------------------------------------------------------------------------------------

// binary coder with division :
void rrArithEncodeBinary( rrArithEncoder* ac , rrbool bit, U32 p0, U32 ptot );
// decode returns 0 or 1 (peek & remove is done)
U32  rrArithDecodeBinary( rrArithDecoder* ac , U32 p0, U32 ptot );

void rrArithEncodeBinaryPow2( rrArithEncoder* ac , rrbool bit, U32 p0, U32 ptotShift );
U32  rrArithDecodeBinaryPow2( rrArithDecoder* ac , U32 p0, U32 ptotShift );

//---------------------------------------------------------------------------------------
// raw bit IO :

void rrArithPutBit(  rrArithEncoder* ac , rrbool bit );
U32  rrArithGetBit(  rrArithDecoder* ac );

void rrArithPutBits(  rrArithEncoder* ac , U32 val, U32 numBits );
U32  rrArithGetBits(  rrArithDecoder* ac , U32 numBits );

//---------------------------------------------------------------------------------------

#if 0
typedef U32 rrArithBinaryModel;
#define rrArithBinaryModel_Check	U32_check
#else
typedef U16 rrArithBinaryModel;
#define rrArithBinaryModel_Check	U16_check
#endif

// binary coder with shifting and probability update :
//  CB note : this is really more of an example than a suggestion that you use it as-is
//		see the .inl for details
//  if you actually want to do fast binary coding & modeling, you might want to use rung/ladder
static void rrArithBinaryModelInit( rrArithBinaryModel * p0 ); // or just use = RR_BINARY_MODEL_INIT_VAL
static void rrArithBinaryModelEncode( rrArithEncoder* ac , rrbool bit, rrArithBinaryModel * p0 );
static rrbool rrArithBinaryModelDecode( rrArithDecoder* ac , rrArithBinaryModel * p0 );
static F32  rrArithBinaryModelGetBitsToCode( rrbool bit , rrArithBinaryModel p0 );
static void rrArithBinaryModelRenorm( rrArithBinaryModel * p0 );

//---------------------------------------------------------------------------------------

OODLE_NS_END


#endif // __RR_ARITHCODERH__

