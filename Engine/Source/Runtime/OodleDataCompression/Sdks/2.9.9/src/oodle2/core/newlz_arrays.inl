// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.


#include "rrmem.h"
#include "lzasserts.h"
#include "speedfitter.h"

OODLE_NS_START

// NEWLZ_ARRAY_RETURN_FAILURE is used to return error from inside newlz_get_array
// toggle here for debugging, to get a BREAK where the newlz_array decode fails :
#define NEWLZ_ARRAY_RETURN_FAILURE()	do { RR_ASSERT_IF_NOT_CORRUPT(false); return -1; } while(0)
//#define NEWLZ_ARRAY_RETURN_FAILURE()	return -1
//#define NEWLZ_ARRAY_RETURN_FAILURE()	do { RR_ASSERT_FAILURE_ALWAYS("newlz_array failure"); return -1; } while(0)


// toggle Leviathan array get logging :
//#define newlz_array_get_printf	rrPrintf_v2 // NOzSHIP do not ship with this! too revealing
#define newlz_array_get_printf	rrPrintfKill
//#define newlz_array_get_printf	rrprintf

/**

newLZ_get_array :

scratch is for get_array to use internally for its temp memory needs
	(indexed, RLE & TANS)
	
[to] should not overlap [scratch]
*except* for the specific case of to == scratch
(which is very very common)
in which case get_array will internally advance scratch_ptr by to_len

**/
				
// newLZ_get_array : returns comp len ; fills *pto_len
//		passed "from_len" should be the array size
static RADINLINE SINTa newLZ_get_array(U8ptr * ptr_to, const U8 * from, const U8 * from_end, SINTa * pto_len, SINTa to_len_max,
								bool force_copy_uncompressed,
								U8 * scratch_ptr, U8 * scratch_end)
{
	RR_ASSERT( ptr_to != NULL );

	const U8 * from_ptr = from;

	// ptr_to == NULL us used by get_arraylens to share code

	// can I get 2 bytes? :
	REQUIRE_FUZZ_RETURN( 2 <= rrPtrDiff( from_end - from_ptr ), -1 );
	
	U8 first_byte = *from_ptr;
	U32 array_type = first_byte >> 4;
	
	RR_COMPILER_ASSERT( NEWLZ_ARRAY_TYPE_UNCOMPRESSED == 0 );
	
	if ( first_byte >= 0x80 )
	{
		// small array header
		array_type &= 7;
		
		if ( array_type == 0 )
		{
			// uncompressed, two byte header :
			
			U32 header = RR_GET16_BE(from_ptr);
			from_ptr += 2;
			
			SINTa to_len = (SINTa)(header & 0xFFF);
			
			if ( to_len > to_len_max ) NEWLZ_ARRAY_RETURN_FAILURE();
			if ( from_ptr+to_len > from_end ) NEWLZ_ARRAY_RETURN_FAILURE();
			
			//SIMPLEPROFILE_SCOPE_N(newLZ_get_array_raw,to_len);
			
			if ( force_copy_uncompressed )
			{
				// must be memmove for comp-raw overlap
				memmove(*ptr_to,from_ptr,to_len);
			}
			else
			{
				// no need to memcpy, just point at it :
				*ptr_to = const_cast<U8 *>( from_ptr );
			}
			
			newlz_array_get_printf("[uncomp %d]",(int)to_len);
			
			#ifdef SPEEDFITTING
			if ( g_speedfitter_stage == 2 )
			{
				speedfitter_stage2_add_array(*ptr_to,to_len);
			}
			#endif
			
			from_ptr += to_len;
			*pto_len = to_len;
			return rrPtrDiff( from_ptr - from );
		}
		
		// drops down to newLZ_get_array_comp
	}
	else
	{	
		// can I get 4 bytes? :
		if ( 4 > rrPtrDiff( from_end - from_ptr ) )
		{
			// could be a 3-byte header at end of stream with zero payload
			// -> this cannot occur currently in valid Kraken/Mermaid/Selkie data
			//	 they always have >= 1 byte after the "arrays" data
			//	 but you could hit it on corrupt/truncated data, or in some future codec
			if ( 3 == rrPtrDiff( from_end - from_ptr ) )
			{		
				// get 3-byte header :
				U32 header = RR_GET24_BE_NOOVERRUN(from_ptr);
				// array type UNCOMPRESSED = 0 , and 0 bytes of data = whole header zero
				REQUIRE_FUZZ_RETURN( header == 0 , -1 );
				
				*ptr_to = NULL;
				*pto_len = 0;
				
				return 3;
			}
			
			NEWLZ_ARRAY_RETURN_FAILURE();
		}
				
		if ( array_type == NEWLZ_ARRAY_TYPE_UNCOMPRESSED )
		{	
			// get 3-byte header :
			U32 header = RR_GET24_BE_OVERRUNOK(from_ptr);
		
			from_ptr += 3;
			SINTa to_len = (SINTa)(header & NEWLZ_ARRAY_SIZE_MASK );
			
			// I have spare bits here that're always zero :
			//	array_type is 4 bytes, size is 18 bits = 22 used, 2 spare
			if ( (header>>NEWLZ_ARRAY_SIZE_BITS) != 0 ) NEWLZ_ARRAY_RETURN_FAILURE();
			
			if ( to_len > to_len_max ) NEWLZ_ARRAY_RETURN_FAILURE();
			if ( to_len > rrPtrDiff( from_end - from_ptr ) ) NEWLZ_ARRAY_RETURN_FAILURE();
			
			if ( force_copy_uncompressed )
			{
				// must be memmove for comp-raw overlap
				memmove(*ptr_to,from_ptr,to_len);
			}
			else
			{
				// no need to memcpy, just point at it :
				*ptr_to = const_cast<U8 *>( from_ptr );
			}
			
			newlz_array_get_printf("[uncomp %d]",(int)to_len);
			
			#ifdef SPEEDFITTING
			if ( g_speedfitter_stage == 2 )
			{
				speedfitter_stage2_add_array(*ptr_to,to_len);
			}
			#endif
	
			from_ptr += to_len;
			*pto_len = to_len;
			return rrPtrDiff( from_ptr - from );
		}
	}
	
	return newLZ_get_array_comp(array_type, ptr_to, from, from_end, pto_len, to_len_max, scratch_ptr, scratch_end);
}

static RADINLINE U8 * newLZ_put_array_uncomp_header(U8 * to_ptr, SINTa from_len)
{
	// 3 byte header
	RR_ASSERT( from_len >= 0 );
	RR_ASSERT( from_len <= NEWLZ_ARRAY_SIZE_MASK );
	
	U32 header = (U32)(from_len);
	// header implicitly is flagged with NEWLZ_ARRAY_TYPE_UNCOMPRESSED
	//  which is value 0 in the top bits
	RR_PUT24_BE_NOOVERRUN(to_ptr,header);
	to_ptr += 3;
	
	return to_ptr;
}
		
static RADINLINE U8 * newLZ_put_array_comp_header(U8 * to_ptr,int array_type, SINTa from_len, SINTa comp_len)
{
	// 5 byte header
	//	raw len, comp len, huff flag :
	RR_ASSERT( from_len > 0 );
	RR_ASSERT( (from_len-1) <= NEWLZ_ARRAY_SIZE_MASK );
	RR_ASSERT( comp_len >= 0 );
	RR_ASSERT( comp_len <= NEWLZ_ARRAY_SIZE_MASK );
	
	U64 header = ((U64)(array_type)<<36) + ((U64)(from_len-1)<<NEWLZ_ARRAY_SIZE_BITS) + (U64)(comp_len);
	
	*to_ptr++ = (U8)(header>>32);
	RR_PUT32_BE_UNALIGNED(to_ptr,(U32)header);
	to_ptr += 4;
	to_ptr += comp_len;
	
	return to_ptr;
}

static RADINLINE U8 * newLZ_put_array_huff3_memset_payload(U8 * to_ptr, U8 value)
{
	// Huffman bitstream (MSB-first):
	// 00 (small alphabet) 00000001 (numSymbols=1) <memsetByte>
	//
	// (3 bytes total): 0x00, 0x40 | (memsetByte >> 2), (memsetByte & 3) << 6
	to_ptr[0] = 0x00;
	to_ptr[1] = 0x40 | (value >> 2);
	to_ptr[2] = (value & 3) << 6;
	return to_ptr + 3;
}

static RADINLINE U8 * newLZ_put_array_huff3_memset(U8 * to_ptr, SINTa from_len, U8 value)
{
	// 5 bytes header + 3 bytes payload
	newLZ_put_array_huff3_memset_payload(to_ptr + 5, value);
	return newLZ_put_array_comp_header(to_ptr, NEWLZ_ARRAY_TYPE_HUFF, from_len, 3);
}

OODLE_NS_END
