// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrarithcoder.h"
#include "rrarithcoder.inl"

#include "rrmath.h"

//=====================================================================================================================

OODLE_NS_START

void rrArithEncodeInit( rrArithEncoder* ac, void * ptr )
{
    ac->start = ac->ptr = (U8 * ) ptr;
    ac->low = 0;
    ac->range = ~((U32)0);
}

void rrArithDecodeInit( rrArithDecoder* ac, void const * ptr )
{
    ac->start = ac->ptr = (const U8 * ) ptr;
    ac->range = ~((U32)0);
    
    ac->code = *ac->ptr++;
    ac->code <<= 8;
    ac->code |= *ac->ptr++;
    ac->code <<= 8;
    ac->code |= *ac->ptr++;
    ac->code <<= 8;
    ac->code |= *ac->ptr++;
}

/*
// Minimal only works if the decoder inputs 0 bytes after the end of the array
U32 rrArithEncodeFlushMinimal( rrArithCoder* ac )
{
    // put code to top 
    
    U32 code = ac->low + ac->range - 1;

    if ( code < ac->low ) rrArithEncodePropagateCarry(ac);
    
    RR_ASSERT( ac->range >= (1<<24) );
    
    // output only top byte of code and assume 0's come in for low
    
    *ac->ptr++ = code>>24;
    
    // boom we're done

    return rrPtrDiff( ac->ptr - ac->start );
}
*/

// Flush returns number of output bytes
SINTa rrArithEncodeFlush( rrArithEncoder* ac )
{
    // output minimal number of bytes such that if you read in random bytes
    //  it will still output the same
    // 
    // first bump up "code" (1<<24) so that reading in 0's is okay
    // range is always big enough that reading 0xFF's is okay
	//
	// range is somewhere in (1<<24) to ((1<<32)-1)
	// I can output anything in [code,code+range] and decode to the same thing
	
    if ( ac->range > (1<<25) )
    {
        // just one byte needed :
        U32 code = ac->low + (1<<24);
        if ( code < ac->low ) rrArithEncodePropagateCarry(ac);
        *ac->ptr++ = (U8)(code>>24);
    }
    else
    {
        // two bytes needed : ; this is very rare
        U32 code = ac->low + (1<<16);
        if ( code < ac->low ) rrArithEncodePropagateCarry(ac); // should never happen actually
        *ac->ptr++ = (U8)(code>>24);
        code <<= 8;
        *ac->ptr++ = (U8)(code>>24);
    }
    
    return rrPtrDiff( ac->ptr - ac->start );
}

SINTa rrArithEncodeTellPos( const rrArithEncoder* ac )
{
    SINTa pos = rrPtrDiff( ac->ptr - ac->start );

	// if we're called in a standard normalized state; range is >= (1<<24)

    if ( ac->range > (1<<25) )
    {
        pos ++;
    }
    else if ( ac->range > (1<<17) )
    {
		pos += 2;
    }
    else if ( ac->range > (1<<9) )
    {
		pos += 3;
    }
    else
    {
		pos += 4;
    }    
    
	return pos;
}

SINTa rrArithDecodeTellPos( const rrArithDecoder* ac )
{
    SINTa pos = rrPtrDiff( ac->ptr - ac->start );
    RR_ASSERT( pos >= 4 );
    pos -= 4;

	// if we're called in a standard normalized state; range is >= (1<<24)
	
    if ( ac->range > (1<<25) )
    {
        pos ++;
    }
    else if ( ac->range > (1<<17) )
    {
		pos += 2;
    }
    else if ( ac->range > (1<<9) )
    {
		pos += 3;
    }
    else
    {
		pos += 4;
    }    
    
	return pos;
}

// information for optimizations :
//	gets # of bits actually written plus bits pending in the range :
F32 rrArithEncodeGetBitsInState( const rrArithEncoder* ac )
{
	// bits already flushed :
	SINTa bytes = rrPtrDiff( ac->ptr - ac->start );
    F32 bits = 8.0f * bytes;
	
	// bits pending :
	// each time "range" is halved under 1<<32 , that's a bit :
		
	//F32 rangeBits = 32.f - rrlog2f( (F32) ac->range );
	F32 rangeBits = rrlog2neg_bk<32>( ac->range );
		
	RR_ASSERT( rangeBits >= 0 && rangeBits <= 8 );
	
	bits += rangeBits;
	
	return bits;
}

F32 rrArithDecodeGetBitsInState( const rrArithDecoder* ac )
{
	// decoder bit count is just the same as encoder
	//	except - 4 bytes because we read ahead
	// immediately after init rrArithDecodeGetBitsInState == 0
	
	// CODE DUPE OF ABOVE except bytes -= 4

	// bits already flushed :
	SINTa bytes = rrPtrDiff( ac->ptr - ac->start );
	RR_ASSERT( bytes >= 4 );
	bytes -= 4;
    F32 bits = 8.0f * bytes;
	
	// bits pending :
	// each time "range" is halved under 1<<32 , that's a bit :
		
	//F32 rangeBits = 32.f - rrlog2f( (F32) ac->range );
	F32 rangeBits = rrlog2neg_bk<32>( ac->range );
		
	RR_ASSERT( rangeBits >= 0 && rangeBits <= 8 );
	
	bits += rangeBits;
	
	return bits;
}

//=====================================================================================================================

OODLE_NS_END
