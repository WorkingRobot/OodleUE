// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_VARBITCODESH__
#define __RADRR_VARBITCODESH__

#include "rrbase.h"
#include "rrvarbits.h"
#include "rrmath.h"

OODLE_NS_START

/**

rrVarBits codes

all these code integers in the range [0,inf)

**/

//-------------------------------------------------------
// toggle VarBits as inlines or funcs :
//  @@ these require a varbits named "vb" , they don't take an argument
#ifdef RRVBC_INLINE
#define RRVBC_FUNCTYPE	static RADFORCEINLINE
#define RRVBC_ARGTYPE	rrVarBits_FuncArgs(vb)
#define RRVBC_VB		vb
#define RRVBC_PASSTYPE	rrVarBits_PassArgs(vb)
#else
#define RRVBC_FUNCTYPE	
#define RRVBC_ARGTYPE	rrVarBits & vb
#define RRVBC_VB		vb.m
#define RRVBC_PASSTYPE	vb
#endif
//-------------------------------------------------------

// Elias Gamma code = count # of bits in val; write # of bits with unary; write val in # of bits
static RADINLINE int rrVarBits_CountBits_EliasGamma(U32 val)
{
    val++;
    U32 topbit = rrGetBitLevel_V_NonZero(val) - 1;
    RR_ASSERT( val >= (1U<<topbit) && val < (2U<<topbit) );
    int ret = 1 + 2*topbit;
    return ret;
}

// Exp Golomb = Elias Gamma but right shift by 'rshift' first and write those bits always
static RADINLINE int rrVarBits_CountBits_ExpGolomb(U32 val,U32 rshift)
{
    int ret = rrVarBits_CountBits_EliasGamma(val>>rshift);
    ret += rshift;
    return ret;
}

static RADINLINE int rrVarBits_CountBits_Flat(U32 symbol,U32 numSymbols)
{
	if ( numSymbols <= 1 )
		return 0;
	
	// obviously slow to call log2ceil here
	//	should precompute it when you know numsymbols in advance
    U32 bits = rrIlog2ceil(numSymbols);
    U32 threshold = (1U<<bits) - numSymbols;
    RR_ASSERT( threshold <= (1U<<(bits-1)) );
    if ( symbol < threshold )
    {
		return bits - 1;
    }
    else
    {
		return bits;
    }
}

//-------------------------------------------------------

/**

these Read/Writes do Output & Refill :

**/

// Unary is the optimal code for the probabilities {1/2,1/4,1/8,...}
RRVBC_FUNCTYPE void rrVarBits_WriteUnaryTrue(RRVBC_ARGTYPE,U32 val);
RRVBC_FUNCTYPE U32 rrVarBits_ReadUnaryTrue(RRVBC_ARGTYPE);

// Golomb uses a divide ; mean > 0 !!
//  Golomb is optimal for a laplacian dist with this mean

// Rice = Golomb with mean = 2^bits
RRVBC_FUNCTYPE void rrVarBits_WriteRice(RRVBC_ARGTYPE,U32 val,U32 bits);
RRVBC_FUNCTYPE U32 rrVarBits_ReadRice(RRVBC_ARGTYPE,U32 bits);

// Elias Gamma code = count # of bits in val; write # of bits with unary; write val in # of bits
RRVBC_FUNCTYPE void rrVarBits_WriteEliasGamma(RRVBC_ARGTYPE,U32 val);
RRVBC_FUNCTYPE U32 rrVarBits_ReadEliasGamma(RRVBC_ARGTYPE);

// Exp Golomb = Elias Gamma but right shift by 'rshift' first and write those bits always
RRVBC_FUNCTYPE void rrVarBits_WriteExpGolomb(RRVBC_ARGTYPE,U32 val,U32 rshift);
RRVBC_FUNCTYPE U32 rrVarBits_ReadExpGolomb(RRVBC_ARGTYPE,U32 rshift);

// try to write in bits; if too big write escape and bits += bitincr
RRVBC_FUNCTYPE void rrVarBits_WriteEscaping(RRVBC_ARGTYPE,U32 val,U32 bits,U32 bitincr);
RRVBC_FUNCTYPE U32 rrVarBits_ReadEscaping(RRVBC_ARGTYPE,U32 bits,U32 bitincr);

// write an arbitrary size alphabet without wasting code space
//  symbol is >=0 and < numSymbols
// uses log2(numSymbols) bits , floor or ceil
RRVBC_FUNCTYPE void rrVarBits_WriteFlat(RRVBC_ARGTYPE,U32 symbol,U32 numSymbols);
RRVBC_FUNCTYPE U32 rrVarBits_ReadFlat(RRVBC_ARGTYPE,U32 numSymbols);
RRVBC_FUNCTYPE U32 rrVarBits_ReadFlat_NoRefill(RRVBC_ARGTYPE,U32 numSymbols);

// True Golomb = silly expensive ; just here for reference
RRVBC_FUNCTYPE void rrVarBits_WriteTrueGolomb(RRVBC_ARGTYPE,U32 val,U32 divisor);
RRVBC_FUNCTYPE U32 rrVarBits_ReadTrueGolomb(RRVBC_ARGTYPE,U32 divisor);

//-------------------------------------------------------

/**

VarBitCodes "Small" variants
for reading small values fast

do not do Refill - you must do it outside
do not ever loop to fit large values - values coded must fit in one varbit word

**/

RRVBC_FUNCTYPE U32 rrVarBits_ReadEliasGamma_Small(RRVBC_ARGTYPE);
RRVBC_FUNCTYPE U32 rrVarBits_ReadExpGolomb_Small(RRVBC_ARGTYPE,U32 rshift);
RRVBC_FUNCTYPE U32 rrVarBits_ReadRice_Small(RRVBC_ARGTYPE,U32 riceBits);
RRVBC_FUNCTYPE U32 rrVarBits_ReadUnary_Small(RRVBC_ARGTYPE);

//-------------------------------------------------------

OODLE_NS_END

#endif // __RADRR_VARBITCODESH__
