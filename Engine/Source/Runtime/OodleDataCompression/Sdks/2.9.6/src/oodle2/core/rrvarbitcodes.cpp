// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrvarbitcodes.h"

#include "rrmath.h"
#include "rrmem.h"
#include "rrlog.h"
#include "rrbits.h"
#include "oodlebase.h"

OODLE_NS_START

enum { c_maxBitsPerStep = RR_MIN(RR_MINBITSAVAILABLE,31) };
		
// Unary is the optimal code for the probabilities {1/2,1/4,1/8,...}
RRVBC_FUNCTYPE void rrVarBits_WriteUnary(RRVBC_ARGTYPE,U32 val)
{    
    while( val >= c_maxBitsPerStep)
    {
        rrVarBits_Put(RRVBC_VB,0,c_maxBitsPerStep);
        rrVarBits_Output(RRVBC_VB);	
        val -= c_maxBitsPerStep;	
    }
    
    rrVarBits_Put(RRVBC_VB,1,(val+1));
    rrVarBits_Output(RRVBC_VB);	
}

RRVBC_FUNCTYPE void rrVarBits_WriteUnaryBack(RRVBC_ARGTYPE,U32 val)
{    
    while( val >= c_maxBitsPerStep)
    {
        rrVarBits_Put(RRVBC_VB,0,c_maxBitsPerStep);
        rrVarBits_OutputBack(RRVBC_VB);	
        val -= c_maxBitsPerStep;	
    }
    
    rrVarBits_Put(RRVBC_VB,1,(val+1));
    rrVarBits_OutputBack(RRVBC_VB);	
}

#define RR_DO_MACRO2( M,X,Y )       M(X,Y)

RRVBC_FUNCTYPE U32 rrVarBits_ReadUnary(RRVBC_ARGTYPE)
{
	U32 base = 0;

	for(;;)
	{
		rrVarBits_Refill_Safe(RRVBC_VB);
		
		#if 1
		// need to check for all zeros because ctlz can't do it
		// @@ use BMI lzcnt and remove this !
		if ( rrVarBits_Bits(RRVBC_VB) == 0 )
		{
			// all bits are zero
			// consume all bits
			RR_BITLENTYPE cntLZ = rrVarBits_BitLen(RRVBC_VB);
			rrVarBits_Use_V(RRVBC_VB,cntLZ);
			base += (U32)cntLZ;
			if ( RR_DO_MACRO2(NVB,RRVBC_VB,_cur) >= RR_DO_MACRO2(NVB,RRVBC_VB,_end) )
			{
				// bad!
				ooLogError("corruption : ReadUnary past end\n");
				return 0;
			}
			continue;
		}
		#endif		
				
		// can find top bit with BSR :
		U32 cntLZ = rrVarBits_CountLeadingZeros(RRVBC_VB);
		if ( cntLZ < c_maxBitsPerStep )
		{
			rrVarBits_Use_V(RRVBC_VB,cntLZ+1);
			return base + cntLZ;
		}
		rrVarBits_Use_C(RRVBC_VB,c_maxBitsPerStep);
		
		base += c_maxBitsPerStep;
	}
}

RRVBC_FUNCTYPE U32 rrVarBits_ReadUnaryBack(RRVBC_ARGTYPE)
{
	U32 base = 0;

	// @@@@ NOT FUZZ SAFE

	for(;;)
	{
		rrVarBits_RefillBack_Unsafe(RRVBC_VB);
		
		#if 1
		// need to check for all zeros because ctlz can't do it
		// @@ use BMI lzcnt and remove this !
		if ( rrVarBits_Bits(RRVBC_VB) == 0 )
		{
			// all bits are zero
			// consume all bits
			RR_BITLENTYPE cntLZ = rrVarBits_BitLen(RRVBC_VB);
			rrVarBits_Use_V(RRVBC_VB,cntLZ);
			base += (U32)cntLZ;
			/*
			if ( RR_DO_MACRO2(NVB,RRVBC_VB,_cur) >= RR_DO_MACRO2(NVB,RRVBC_VB,_end) )
			{
				// bad!
				RR_ASSERT_FAILURE("ReadUnaryTrue past end");
				return 0;
			}
			*/
			continue;
		}
		#endif		
				
		// can find top bit with BSR :
		U32 cntLZ = rrVarBits_CountLeadingZeros(RRVBC_VB);
		if ( cntLZ < c_maxBitsPerStep )
		{
			rrVarBits_Use_V(RRVBC_VB,cntLZ+1);
			return base + cntLZ;
		}
		rrVarBits_Use_C(RRVBC_VB,c_maxBitsPerStep);
		
		base += c_maxBitsPerStep;
	}
}

// Rice = Golomb with mean = 2^bits
RRVBC_FUNCTYPE void rrVarBits_WriteRice(RRVBC_ARGTYPE,U32 val,U32 bits)
{
    // bits can be zero !!
    rrVarBits_WriteUnary(RRVBC_PASSTYPE,val>>bits);
    int tail = val & ((1U<<bits)-1);
    rrVarBits_Put(RRVBC_VB,tail,bits);
    rrVarBits_Output(RRVBC_VB);
}

RRVBC_FUNCTYPE U32 rrVarBits_ReadRice(RRVBC_ARGTYPE,U32 bits)
{
	rrVarBits_Temps();
    U32 top = rrVarBits_ReadUnary(RRVBC_PASSTYPE);
    rrVarBits_Refill_Safe(RRVBC_VB);
    U32 tail = (U32) rrVarBits_Get_0Ok(RRVBC_VB,bits);
	return (top<<bits) + tail;
}

// Elias Gamma code = count # of bits in val; write # of bits with unary; write val in # of bits
RRVBC_FUNCTYPE void rrVarBits_WriteEliasGamma(RRVBC_ARGTYPE,U32 val)
{
    val++;
    U32 topbit = rrGetBitLevel_V_NonZero(val) - 1;
    RR_ASSERT( val >= (1U<<topbit) && val < (2U<<topbit) );
    rrVarBits_WriteUnary(RRVBC_PASSTYPE,topbit);
    if ( topbit > 0 )
    {
        val -= (1U<<topbit);
        rrVarBits_Put(RRVBC_VB,val,topbit);
        rrVarBits_Output(RRVBC_VB);
    }
}

RRVBC_FUNCTYPE U32 rrVarBits_ReadEliasGamma(RRVBC_ARGTYPE)
{
	rrVarBits_Temps();
    U32 bits = rrVarBits_ReadUnary(RRVBC_PASSTYPE);
    if ( bits == 0 )
        return 0;
    rrVarBits_Refill_Safe(RRVBC_VB);
    U32 val = (1U<<bits) + (U32) ( rrVarBits_Get_V(RRVBC_VB,bits) );
    return val - 1;
}

// Exp Golomb = Elias Gamma but right shift by 'rshift' first and write those bits always
RRVBC_FUNCTYPE void rrVarBits_WriteExpGolomb(RRVBC_ARGTYPE,U32 val,U32 rshift)
{
    rrVarBits_WriteEliasGamma(RRVBC_PASSTYPE,val>>rshift);
    U32 tail = val & ((1U<<rshift)-1);
    rrVarBits_Put(RRVBC_VB,tail,rshift);   
    rrVarBits_Output(RRVBC_VB);    
}


RRVBC_FUNCTYPE void rrVarBits_WriteEliasGammaBack(RRVBC_ARGTYPE,U32 val)
{
    val++;
    U32 topbit = rrGetBitLevel_V_NonZero(val) - 1;
    RR_ASSERT( val >= (1U<<topbit) && val < (2U<<topbit) );
    rrVarBits_WriteUnaryBack(RRVBC_PASSTYPE,topbit);
    if ( topbit > 0 )
    {
        val -= (1U<<topbit);
        rrVarBits_Put(RRVBC_VB,val,topbit);
        rrVarBits_OutputBack(RRVBC_VB);
    }
}

RRVBC_FUNCTYPE U32 rrVarBits_ReadEliasGammaBack(RRVBC_ARGTYPE)
{
	rrVarBits_Temps();
    U32 bits = rrVarBits_ReadUnaryBack(RRVBC_PASSTYPE);
    if ( bits == 0 )
        return 0;
    // @@ UNSAFE
    rrVarBits_RefillBack_Unsafe(RRVBC_VB);
    U32 val = (1U<<bits) + (U32) ( rrVarBits_Get_V(RRVBC_VB,bits) );
    return val - 1;
}

RRVBC_FUNCTYPE void rrVarBits_WriteExpGolombBack(RRVBC_ARGTYPE,U32 val,U32 rshift)
{
    rrVarBits_WriteEliasGammaBack(RRVBC_PASSTYPE,val>>rshift);
    U32 tail = val & ((1U<<rshift)-1);
    rrVarBits_Put(RRVBC_VB,tail,rshift);
    rrVarBits_OutputBack(RRVBC_VB);    
}

RRVBC_FUNCTYPE U32 rrVarBits_ReadExpGolomb(RRVBC_ARGTYPE,U32 rshift)
{
	rrVarBits_Temps();
    U32 head = rrVarBits_ReadEliasGamma(RRVBC_PASSTYPE);
    rrVarBits_Refill_Safe(RRVBC_VB);
    // rrVarBits_Get_V doesn't allow rshift == 0
    // could use rrVarBits_Get_0k
    //U32 tail = (U32) rrVarBits_Get_V(RRVBC_VB,rshift);
    U32 tail = (U32) rrVarBits_Get_0Ok(RRVBC_VB,rshift);
    return (head<<rshift) + tail;
}

RRVBC_FUNCTYPE U32 rrVarBits_ReadExpGolombBack(RRVBC_ARGTYPE,U32 rshift)
{
	rrVarBits_Temps();
    U32 head = rrVarBits_ReadEliasGammaBack(RRVBC_PASSTYPE);
    // @@@@ UNSAFE :
    rrVarBits_RefillBack_Unsafe(RRVBC_VB);
    // rrVarBits_Get_V doesn't allow rshift == 0
    // could use rrVarBits_Get_0k
    //U32 tail = (U32) rrVarBits_Get_V(RRVBC_VB,rshift);
    U32 tail = (U32) rrVarBits_Get_0Ok(RRVBC_VB,rshift);
    return (head<<rshift) + tail;
}

// try to write in bits; if too big write escape and bits += bitincr
RRVBC_FUNCTYPE void rrVarBits_WriteEscaping(RRVBC_ARGTYPE,U32 val,U32 bits,U32 bitincr)
{
	RR_ASSERT( rrVarBits_BitsWriteable(RRVBC_VB) >= (int)bits );
	
    for(;;)
    {
		RR_ASSERT( bits <= RR_MINBITSAVAILABLE );
        U32 maxval = (1U<<bits) - 1;
        if ( val < maxval )
        {
            rrVarBits_Put(RRVBC_VB,val,bits);
			rrVarBits_Output(RRVBC_VB);
            return;
        }
        
        rrVarBits_Put(RRVBC_VB,maxval,bits);
		rrVarBits_Output(RRVBC_VB);
        val -= maxval;
        bits += bitincr;
        bits = RR_MIN(bits,RR_MINBITSAVAILABLE);
    }
}

RRVBC_FUNCTYPE U32 rrVarBits_ReadEscaping(RRVBC_ARGTYPE,U32 bits,U32 bitincr)
{
    U32 base = 0;

	rrVarBits_Temps();
    
    for(;;)
    {
        U32 maxval = (1U<<bits) - 1;
        
	    rrVarBits_Refill_Safe(RRVBC_VB);
        U32 cur = (U32) rrVarBits_Get_V(RRVBC_VB,bits);
        if ( cur < maxval )
        {
            return base + cur;
        }
        
        base += maxval;
        bits += bitincr;
        bits = RR_MIN(bits,31);
        
        // hacky : make sure we don't infinite loop in the reader :
        if ( bits == 31 )
		{
			rrPrintf("Escaping overflow : corruption!?\n");
            return 0xFFFFFFFF;
        }
    }
}

// write an arbitrary size alphabet without wasting code space
//  symbol is >=0 and < numSymbols
// uses log2(numSymbols) bits , floor or ceil
RRVBC_FUNCTYPE void rrVarBits_WriteFlat(RRVBC_ARGTYPE,U32 symbol,U32 numSymbols)
{
	RR_ASSERT( symbol < numSymbols );
	
	if ( numSymbols <= 1 )
		return;
	
	// obviously slow to call log2ceil here
	//	should precompute it when you know numsymbols in advance
    U32 bits = rrIlog2ceil(numSymbols);
    U32 threshold = (1U<<bits) - numSymbols;
    RR_ASSERT( threshold <= (1U<<(bits-1)) );
    if ( symbol < threshold )
    {
        rrVarBits_Put(RRVBC_VB,symbol,(bits-1));
    }
    else
    {
        symbol += threshold;
        // symbol is >= 2*threshold , so the top (bits-1) part is >= threshold
        rrVarBits_Put(RRVBC_VB,symbol,bits);
    }
    rrVarBits_Output(RRVBC_VB);
}

RRVBC_FUNCTYPE void rrVarBits_WriteFlatBack(RRVBC_ARGTYPE,U32 symbol,U32 numSymbols)
{
	RR_ASSERT( symbol < numSymbols );
	
	if ( numSymbols <= 1 )
		return;
	
	// obviously slow to call log2ceil here
	//	should precompute it when you know numsymbols in advance
    U32 bits = rrIlog2ceil(numSymbols);
    U32 threshold = (1U<<bits) - numSymbols;
    RR_ASSERT( threshold <= (1U<<(bits-1)) );
    if ( symbol < threshold )
    {
        rrVarBits_Put(RRVBC_VB,symbol,(bits-1));
    }
    else
    {
        symbol += threshold;
        // symbol is >= 2*threshold , so the top (bits-1) part is >= threshold
        rrVarBits_Put(RRVBC_VB,symbol,bits);
    }
    rrVarBits_OutputBack(RRVBC_VB);
}

RRVBC_FUNCTYPE U32 rrVarBits_ReadFlat_NoRefill(RRVBC_ARGTYPE,U32 numSymbols)
{
	rrVarBits_Temps();
	
	if ( numSymbols <= 2 )
	{
		// when numSymbols == 2 ,
		//	can't use the below because it would do Get_V(0) then Get1()
		//	and getting 0 bits is forbidden now
		RR_ASSERT( numSymbols >= 1 );
		if ( numSymbols == 1 )
		{
			return 0;
		}
		else
		{
			return (U32) rrVarBits_Get1(RRVBC_VB);
		}
	}
		
    U32 bits = rrIlog2ceil(numSymbols);
    U32 threshold = (1U<<bits) - numSymbols;
    RR_ASSERT( threshold <= (1U<<(bits-1)) );
    
    // probably faster to Peek (bits) and then
    //	either consume (bits-1) or (bits)
    // but whatever
    
    /*
    UINTr symbol = rrVarBits_Get_V(RRVBC_VB,bits-1);
    if ( symbol >= threshold )
    {
        symbol += symbol - threshold + rrVarBits_Get1(RRVBC_VB);   
    }
    /*/
    
    UINTr symbol = rrVarBits_Peek(RRVBC_VB,bits);
    if ( symbol >= threshold*2 )
    {
		rrVarBits_Use(RRVBC_VB,bits);
		symbol -= threshold;
    }
    else
    {
		rrVarBits_Use(RRVBC_VB,bits-1);
		symbol >>= 1;
    }
    /**/
    
    RR_ASSERT( symbol < numSymbols );
    
    return (U32) symbol;
}

RRVBC_FUNCTYPE U32 rrVarBits_ReadFlat(RRVBC_ARGTYPE,U32 numSymbols)
{
    rrVarBits_Refill_Safe(RRVBC_VB);
    
    return rrVarBits_ReadFlat_NoRefill(RRVBC_PASSTYPE,numSymbols);
}

// a Golomb code is the correct prefix code for a geometric distribution
//	with divisor = ceil( log2(mean) )

// True Golomb = silly expensive ; just here for reference
RRVBC_FUNCTYPE void rrVarBits_WriteTrueGolomb(RRVBC_ARGTYPE,U32 val,U32 divisor)
{
	RR_ASSERT( divisor > 0 );
	U32 quotient = (val / divisor);
	U32 remainder = val - (quotient * divisor);
	
    rrVarBits_WriteUnary(RRVBC_PASSTYPE,quotient);

    rrVarBits_WriteFlat(RRVBC_PASSTYPE,remainder,divisor);
	
}

RRVBC_FUNCTYPE U32 rrVarBits_ReadTrueGolomb(RRVBC_ARGTYPE,U32 divisor)
{
	U32 quotient = rrVarBits_ReadUnary(RRVBC_PASSTYPE);
	U32 remainder = rrVarBits_ReadFlat(RRVBC_PASSTYPE,divisor);
	
	return quotient * divisor + remainder;
}


/**

VarBitCodes "Small" variants
for reading small values fast

do not do Refill - you must do it outside
do not ever loop to fit large values - values coded must fit in one varbit word

**/

// rrVarBits_CountLeadingZerosMayBeZero just needs to return a large number if bits is zero
//	to trigger the use of the overflow case
//  (this is just for corruption handling)
#define rrVarBits_CountLeadingZerosMayBeZero(vb)	( ( rrVarBits_Bits(vb) == 0 ) ? RR_VARBITSTYPELEN : rrVarBits_CountLeadingZeros(vb) )

/*
#ifdef RR_VB_64
#define rrVB_CountLZMayBeZero rrCountLeadingZeros64
#else
#define rrVB_CountLZMayBeZero rrCountLeadingZeros32
#endif

#define rrVarBits_CountLeadingZerosMayBeZero(vb)	rrVB_CountLZMayBeZero( rrVarBits_Bits(vb) )
*/

RRVBC_FUNCTYPE U32 rrVarBits_ReadUnary_Small(RRVBC_ARGTYPE)
{
	// can find top bit with BSR :
	int cntLZ = rrVarBits_CountLeadingZerosMayBeZero(RRVBC_VB);
	//RR_ASSERT( cntLZ < 32 && cntLZ <= RR_MINBITSAVAILABLE );
	if ( RAD_UNLIKELY( cntLZ >= c_maxBitsPerStep ) )
	{
		return rrVarBits_ReadUnary(RRVBC_PASSTYPE);
	}
	rrVarBits_Use_V(RRVBC_VB,cntLZ+1);
	return cntLZ;
}

RRVBC_FUNCTYPE U32 rrVarBits_ReadRice_Small(RRVBC_ARGTYPE,U32 riceBits)
{	
	rrVarBits_Temps();

	// can find top bit with BSR :
	U32 cntLZ = rrVarBits_CountLeadingZerosMayBeZero(RRVBC_VB);
	U32 nBits = cntLZ+1 + riceBits;
	if ( RAD_UNLIKELY( nBits >= c_maxBitsPerStep ) )
	{
		return rrVarBits_ReadRice(RRVBC_PASSTYPE,riceBits);
	}
	
	U32 tail = (U32) rrVarBits_Get_V(RRVBC_VB,nBits);
	// when cntLZ is 0 this does weird stuff with wrapping around the FFF
	//	but it's still correct
	// just make sure all the variables are U32
	return ((cntLZ-1)<<riceBits) + tail;
}

RRVBC_FUNCTYPE U32 rrVarBits_ReadEliasGamma_Small(RRVBC_ARGTYPE)
{	
	rrVarBits_Temps();
	/*
    U32 bits = rrVarBits_ReadUnary(RRVBC_PASSTYPE);
    if ( bits == 0 )
        return 0;
    U32 val = (1UL<<bits) + (U32) ( rrVarBits_Get_V(RRVBC_VB,bits) );
    return val - 1;
    /*/
    
	int cntLZ = rrVarBits_CountLeadingZerosMayBeZero(RRVBC_VB);
	int nBits = 2*cntLZ+1;
	if ( RAD_UNLIKELY( nBits >= c_maxBitsPerStep ) )
	{
		return rrVarBits_ReadEliasGamma(RRVBC_PASSTYPE);
	}
	//RR_ASSERT( nBits < 32 && nBits <= RR_MINBITSAVAILABLE );
	RR_VARBITSTYPE val = rrVarBits_Get_V(RRVBC_VB,nBits);
    return (U32)val - 1;
    
    /**/
}

/*
static RADFORCEINLINE U32 rrVarBits_ReadExpGolomb_Small(RRVBC_ARGTYPE,U32 rshift)
{    
    U32 head = rrVarBits_ReadEliasGamma_Small(RRVBC_PASSTYPE);
    U32 tail = (U32) rrVarBits_Get_V(RRVBC_VB,rshift);
    return (head<<rshift) + tail;
}
/*/

RRVBC_FUNCTYPE U32 rrVarBits_ReadExpGolomb_Small(RRVBC_ARGTYPE,U32 rshift)
{    	
	rrVarBits_Temps();
	int cntLZ = rrVarBits_CountLeadingZerosMayBeZero(RRVBC_VB);
	int nBits = 2*cntLZ+1 + rshift;
	if ( RAD_UNLIKELY( nBits >= c_maxBitsPerStep ) )
	{
		return rrVarBits_ReadExpGolomb(RRVBC_PASSTYPE,rshift);
	}
	//RR_ASSERT( nBits < 32 && nBits <= RR_MINBITSAVAILABLE );
	RR_VARBITSTYPE val = rrVarBits_Get_V(RRVBC_VB,nBits);
    return (U32)val - (1<<rshift);
}
/**/

OODLE_NS_END
