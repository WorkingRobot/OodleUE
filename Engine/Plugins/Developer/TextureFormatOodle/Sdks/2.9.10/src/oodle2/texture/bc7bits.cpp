// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "bc7bits.h"
#include "bc7compress.h"
#include "bc7compress_internal.h"
//#include "rrlogutil.h"

OODLE_NS_START

const int c_bc7_total_bits_header[8] = { 
	5, 8, 9, 10, 8, 8, 7, 14
};

const int c_bc7_total_bits_endpoints[8] = { 
	72, 72, 90, 84, 42, 58, 56, 80
};

const int c_bc7_total_bits_pbits[8] = { 
	6, 2, 0, 4, 0, 0, 2, 4
};

const int c_bc7_total_bits_indices[8] = { 
	45, 46, 29, 30, 78, 62, 63, 30
};

const bc7bitrange c_bc7bitrange_header[8] = {
	{0,5}, {0,8}, {0,9}, {0,10}, {0,8}, {0,8}, {0,7}, {0,14}
};
const bc7bitsqw c_bc7bitrange_header_mask[8] = {
	{{0x000000000000001FULL,0x0000000000000000ULL}}, {{0x00000000000000FFULL,0x0000000000000000ULL}}, {{0x00000000000001FFULL,0x0000000000000000ULL}}, {{0x00000000000003FFULL,0x0000000000000000ULL}}, {{0x00000000000000FFULL,0x0000000000000000ULL}}, {{0x00000000000000FFULL,0x0000000000000000ULL}}, {{0x000000000000007FULL,0x0000000000000000ULL}}, {{0x0000000000003FFFULL,0x0000000000000000ULL}}
};
const bc7bitrange c_bc7bitrange_endpoints[8] = {
	{5,72}, {8,72}, {9,90}, {10,84}, {8,42}, {8,58}, {7,56}, {14,80}
};
const bc7bitsqw c_bc7bitrange_endpoints_mask[8] = {
	{{0xFFFFFFFFFFFFFFE0ULL,0x0000000000001FFFULL}}, {{0xFFFFFFFFFFFFFF00ULL,0x000000000000FFFFULL}}, {{0xFFFFFFFFFFFFFE00ULL,0x00000007FFFFFFFFULL}}, {{0xFFFFFFFFFFFFFC00ULL,0x000000003FFFFFFFULL}}, {{0x0003FFFFFFFFFF00ULL,0x0000000000000000ULL}}, {{0xFFFFFFFFFFFFFF00ULL,0x0000000000000003ULL}}, {{0x7FFFFFFFFFFFFF80ULL,0x0000000000000000ULL}}, {{0xFFFFFFFFFFFFC000ULL,0x000000003FFFFFFFULL}}
};
const bc7bitrange c_bc7bitrange_pbits[8] = {
	{77,6}, {80,2}, {99,0}, {94,4}, {50,0}, {66,0}, {63,2}, {94,4}
};
const bc7bitsqw c_bc7bitrange_pbits_mask[8] = {
	{{0x0000000000000000ULL,0x000000000007E000ULL}}, {{0x0000000000000000ULL,0x0000000000030000ULL}}, {{0x0000000000000000ULL,0x0000000000000000ULL}}, {{0x0000000000000000ULL,0x00000003C0000000ULL}}, {{0x0000000000000000ULL,0x0000000000000000ULL}}, {{0x0000000000000000ULL,0x0000000000000000ULL}}, {{0x8000000000000000ULL,0x0000000000000001ULL}}, {{0x0000000000000000ULL,0x00000003C0000000ULL}}
};
const bc7bitrange c_bc7bitrange_indices[8] = {
	{83,45}, {82,46}, {99,29}, {98,30}, {50,78}, {66,62}, {65,63}, {98,30}
};
const bc7bitsqw c_bc7bitrange_indices_mask[8] = {
	{{0x0000000000000000ULL,0xFFFFFFFFFFF80000ULL}}, {{0x0000000000000000ULL,0xFFFFFFFFFFFC0000ULL}}, {{0x0000000000000000ULL,0xFFFFFFF800000000ULL}}, {{0x0000000000000000ULL,0xFFFFFFFC00000000ULL}}, {{0xFFFC000000000000ULL,0xFFFFFFFFFFFFFFFFULL}}, {{0x0000000000000000ULL,0xFFFFFFFFFFFFFFFCULL}}, {{0x0000000000000000ULL,0xFFFFFFFFFFFFFFFEULL}}, {{0x0000000000000000ULL,0xFFFFFFFC00000000ULL}}
};
const bc7bitrange c_bc7bitrange_header_without_mode[8] = {
	{1,4}, {2,6}, {3,6}, {4,6}, {5,3}, {6,2}, {7,0}, {8,6}
};
const bc7bitsqw c_bc7bitrange_header_without_mode_mask[8] = {
	{{0x000000000000001EULL,0x0000000000000000ULL}}, {{0x00000000000000FCULL,0x0000000000000000ULL}}, {{0x00000000000001F8ULL,0x0000000000000000ULL}}, {{0x00000000000003F0ULL,0x0000000000000000ULL}}, {{0x00000000000000E0ULL,0x0000000000000000ULL}}, {{0x00000000000000C0ULL,0x0000000000000000ULL}}, {{0x0000000000000000ULL,0x0000000000000000ULL}}, {{0x0000000000003F00ULL,0x0000000000000000ULL}}
};
const bc7bitrange c_bc7bitrange_endpoints_with_pbits[8] = {
	{5,78}, {8,74}, {9,90}, {10,88}, {8,42}, {8,58}, {7,58}, {14,84}
};
const bc7bitsqw c_bc7bitrange_endpoints_with_pbits_mask[8] = {
	{{0xFFFFFFFFFFFFFFE0ULL,0x000000000007FFFFULL}}, {{0xFFFFFFFFFFFFFF00ULL,0x000000000003FFFFULL}}, {{0xFFFFFFFFFFFFFE00ULL,0x00000007FFFFFFFFULL}}, {{0xFFFFFFFFFFFFFC00ULL,0x00000003FFFFFFFFULL}}, {{0x0003FFFFFFFFFF00ULL,0x0000000000000000ULL}}, {{0xFFFFFFFFFFFFFF00ULL,0x0000000000000003ULL}}, {{0xFFFFFFFFFFFFFF80ULL,0x0000000000000001ULL}}, {{0xFFFFFFFFFFFFC000ULL,0x00000003FFFFFFFFULL}}
};
const bc7bitrange c_bc7bitrange_indices_with_pbits[8] = {
	{77,51}, {80,48}, {99,29}, {94,34}, {50,78}, {66,62}, {63,65}, {94,34}
};
const bc7bitsqw c_bc7bitrange_indices_with_pbits_mask[8] = {
	{{0x0000000000000000ULL,0xFFFFFFFFFFFFE000ULL}}, {{0x0000000000000000ULL,0xFFFFFFFFFFFF0000ULL}}, {{0x0000000000000000ULL,0xFFFFFFF800000000ULL}}, {{0x0000000000000000ULL,0xFFFFFFFFC0000000ULL}}, {{0xFFFC000000000000ULL,0xFFFFFFFFFFFFFFFFULL}}, {{0x0000000000000000ULL,0xFFFFFFFFFFFFFFFCULL}}, {{0x8000000000000000ULL,0xFFFFFFFFFFFFFFFFULL}}, {{0x0000000000000000ULL,0xFFFFFFFFC0000000ULL}}
};

#if 0

static void log_bc7bitrange_array( bc7bitrange (*fp_bc7bitrange)(int mode) , const char * name )
{
	rrprintf("const bc7bitrange c_%s[8] = {\n\t",name);
	
	for LOOP(m,BC7_NUM_MODES)
	{
		bc7bitrange range = (*fp_bc7bitrange)(m);

		if ( m != 0 ) rrprintf(", ");
		
		rrprintf("{%d,%d}",range.pos,range.count);
	}

	rrprintf("\n};\n");
	
	rrprintf("const bc7bits c_%s_mask[8] = {\n\t",name);
	
	for LOOP(m,BC7_NUM_MODES)
	{
		bc7bitrange range = (*fp_bc7bitrange)(m);

		bc7bits mask = bc7bitrange_makemask(range);

		if ( m != 0 ) rrprintf(", ");
		
		rrprintf("{0x" RR_64_FMT_16HEX "ULL,0x" RR_64_FMT_16HEX "ULL}",mask.qw[0],mask.qw[1]);
	}

	rrprintf("\n};\n");
}

void bc7bits_log_tables()
{
	int c_bc7_total_bits_header[BC7_NUM_MODES];
	int c_bc7_total_bits_endpoints[BC7_NUM_MODES];
	int c_bc7_total_bits_pbits[BC7_NUM_MODES];
	int c_bc7_total_bits_indices[BC7_NUM_MODES];
	
	for LOOP(m,BC7_NUM_MODES)
	{
		c_bc7_total_bits_header[m] = bc7_total_bits_header(m);
		c_bc7_total_bits_endpoints[m] = bc7_total_bits_endpoints(m);
		c_bc7_total_bits_pbits[m] = bc7_total_bits_pbits(m);
		c_bc7_total_bits_indices[m] = bc7_total_bits_indices(m);
	}
	
	RR_PRINTF_ARRAY_INT(c_bc7_total_bits_header);
	RR_PRINTF_ARRAY_INT(c_bc7_total_bits_endpoints);
	RR_PRINTF_ARRAY_INT(c_bc7_total_bits_pbits);
	RR_PRINTF_ARRAY_INT(c_bc7_total_bits_indices);
	
	//log_bc7bitrange_array( bc7bitrange_header ); 
	#define XX(func) log_bc7bitrange_array( func , RR_STRINGIZE(func) )
	
	XX(bc7bitrange_header);
	XX(bc7bitrange_endpoints);
	XX(bc7bitrange_pbits);
	XX(bc7bitrange_indices);
	XX(bc7bitrange_header_without_mode);
	XX(bc7bitrange_endpoints_with_pbits);
	XX(bc7bitrange_indices_with_pbits);

}
	

/**

total bits , sum = 128
broken into 4 categories

{ header, endpoints, pbits, indices }

**/

int bc7_total_bits_header(int mode)
{
	const BC7ModeDesc & desc = bc7_modes[mode];
	int unary_mode_bits = mode+1;
	int bits = unary_mode_bits + desc.pb + desc.rb + desc.isb;
	return bits;
}

int bc7_total_bits_endpoints(int mode)
{
	const BC7ModeDesc & desc = bc7_modes[mode];
	int rgba = 3 * desc.cb + desc.ab;
	int bits = rgba * 2 * desc.ns;
	return bits;
}

int bc7_total_bits_pbits(int mode)
{
	const BC7ModeDesc & desc = bc7_modes[mode];
	int pbits = ( desc.epb * 2 + desc.spb ) * desc.ns;
	return pbits;
}

int bc7_total_bits_indices(int mode)
{
	const BC7ModeDesc & desc = bc7_modes[mode];
	int total_index_bits = desc.ib*16 - desc.ns;
	if ( desc.ib2 )
	{
		total_index_bits += desc.ib2*16 - desc.ns;
	}
	return total_index_bits;
}
#endif

int bc7bits_get_mode(const bc7bits & block)
{
	/*
	// mode is unary in the bottom byte
	const U8 * ptr = bc7bits_U8ptr(&block);
	U8 mbyte = ptr[0];
	if ( mbyte == 0 )
	{
		// invalid
		RR_ASSERT_FAILURE_ALWAYS("invalid zero mode byte");
		return -1;
	}
	
	int mode = rrCtz32(mbyte);
	*/
	
	int mode = rrCtz64( bc7bits_qw0(block) );
	RR_ASSERT( mode >= 0 && mode < 8 );
	
	return mode;
}

int bc7bits_get_part(const bc7bits & block,int mode)
{
	int mbits = mode+1;
	// part is right after mode :
	U64 partbits = bc7bits_qw0(block) >> mbits;
	// @@ part bit mask could be premade
	// (this is rare at the moment)
	int pb = bc7_modes[mode].pb; // number of part bits in this mode
	U64 part = rrMask64(pb) & partbits;
	return (int)part;
}

#if 0
bc7bitrange bc7bitrange_header(int mode)
{
	bc7bitrange ret;
	ret.pos = 0;
	ret.count = bc7_total_bits_header(mode);
	return ret;
}

bc7bitrange bc7bitrange_header_without_mode(int mode)
{
	int unary_mode_bits = mode+1;
	bc7bitrange ret;
	ret.pos = unary_mode_bits;
	ret.count = bc7_total_bits_header(mode) - unary_mode_bits;
	return ret;
}

bc7bitrange bc7bitrange_endpoints(int mode)
{
	bc7bitrange ret;
	ret.pos = bc7_total_bits_header(mode);
	ret.count = bc7_total_bits_endpoints(mode);
	return ret;
}

bc7bitrange bc7bitrange_pbits(int mode)
{
	bc7bitrange ret;
	ret.pos = bc7_total_bits_header(mode) + bc7_total_bits_endpoints(mode);
	ret.count = bc7_total_bits_pbits(mode);
	return ret;
}

bc7bitrange bc7bitrange_indices(int mode)
{
	bc7bitrange ret;
	ret.pos = bc7_total_bits_header(mode) + bc7_total_bits_endpoints(mode) + bc7_total_bits_pbits(mode);
	ret.count = bc7_total_bits_indices(mode);
	RR_ASSERT( ret.pos + ret.count == 128 );
	return ret;
}

bc7bitrange bc7bitrange_endpoints_with_pbits(int mode)
{
	bc7bitrange ret;
	ret.pos = bc7_total_bits_header(mode);
	ret.count = bc7_total_bits_endpoints(mode) + bc7_total_bits_pbits(mode);
	return ret;
}

bc7bitrange bc7bitrange_indices_with_pbits(int mode)
{
	bc7bitrange ret;
	ret.pos = bc7_total_bits_header(mode) + bc7_total_bits_endpoints(mode);
	ret.count = bc7_total_bits_indices(mode) + bc7_total_bits_pbits(mode);
	RR_ASSERT( ret.pos + ret.count == 128 );
	return ret;
}
#endif


bc7bits bc7bits_extract_indices(const bc7bits &block, bool with_pbits)
{
	int mode = bc7bits_get_mode(block);
	bc7bits range;
	if ( with_pbits ) range = c_bc7bitrange_indices_with_pbits_mask[mode];
	else range = c_bc7bitrange_indices_mask[mode];
	bc7bits ret = bc7bits_and(block,range);
	return ret; 
}

bc7bits bc7bits_extract_endpoints(const bc7bits &block, bool with_pbits)
{
	int mode = bc7bits_get_mode(block);
	bc7bits range;
	if ( with_pbits ) range = c_bc7bitrange_endpoints_with_pbits_mask[mode];
	else range = c_bc7bitrange_endpoints_mask[mode];
	bc7bits ret = bc7bits_and(block,range);
	return ret; 
}

bc7bits bc7bits_zero_indices(const bc7bits &block, bool also_zero_pbits)
{
	bc7bits extracted = bc7bits_extract_indices(block,also_zero_pbits);
	bc7bits ret = bc7bits_xor_assert_on(block,extracted);
	return ret; 
}

bc7bits bc7bits_zero_endpoints(const bc7bits &block, bool also_zero_pbits)
{
	bc7bits extracted = bc7bits_extract_endpoints(block,also_zero_pbits);
	bc7bits ret = bc7bits_xor_assert_on(block,extracted);
	return ret; 
}

#if 0
//makemask is only used to generate the tables
bc7bits bc7bitrange_makemask(const bc7bitrange & br)
{
	RR_ASSERT( br.pos >= 0 && br.pos + br.count <= 128 );

	if ( br.count <= 64 )
	{
		bc7bits ret = { }; // zero
		
		if ( br.count == 0 ) return ret;
		
		//U64 mask = (((U64)1) << br.count) - 1;
		// count == 64 is not okay, 1<<64 is 1, not 0 like you want
		RR_ASSERT( br.count > 0 );
		// handle count == 64 here
		// shift one less, then another one
		// so count == 64 makes a mask of -1
		U64 mask = ((((U64)1) << (br.count-1))<<1) - 1;
		// @@ could use c_rrBitMask64 from rrvarbits.h
		int w0 = br.pos / 64;
		int p0 = br.pos & 63;
		ret.qw[w0] = mask << p0;
		if ( (p0 + br.count) > 64 )
		{
			RR_ASSERT( w0 == 0 );
			ret.qw[1] = mask >> (64 - p0);
		}
		RR_ASSERT( bc7bits_popcnt(ret) == br.count );
		return ret;
	}
	/*
	else if ( count == 64 && br.pos == 64 )
	{
		// problematic case not handled well by either branch
		bc7bits ret;
		ret.qw[0] = 0;
		ret.qw[1] = (~(U64)0);
		return ret;
	}
	*/
	else // count >= 64
	{
		bc7bits ret; // does not need zeros
		// at least 1 bit on each qw
		RR_ASSERT( br.pos < 64 );
		int p0 = br.pos;
		ret.qw[0] = (~(U64)0) << p0;
		int nhi = br.count + br.pos - 64;
		RR_ASSERT( nhi >= 1 );
		//U64 mask = (((U64)1) << nhi) - 1;
		// doesn't work for nhi == 64
		U64 mask = ((((U64)1) << (nhi-1))<<1) - 1;
		// @@ could use c_rrBitMask64 from rrvarbits.h		
		ret.qw[1] = mask;
		
		RR_ASSERT( bc7bits_popcnt(ret) == br.count );
		return ret;
	}
}
#endif

OODLE_NS_END
