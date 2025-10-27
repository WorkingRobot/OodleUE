// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "texbase.h"
#include "rrmath.h"
#include "rrsimd.h"

OODLE_NS_START

//========================================

/*
void bc7bits_log_tables();

// total_bits sum to 128
// bits in order {header, endpoints, pbits, indices}
int bc7_total_bits_header(int mode);
int bc7_total_bits_endpoints(int mode);
int bc7_total_bits_pbits(int mode);
int bc7_total_bits_indices(int mode);
*/

//========================================

struct bc7bitsqw
{
	U64 qw[2];
};

struct bc7bitrange
{
	int pos,count;
};

//=====================================

extern const int c_bc7_total_bits_header[8]; 
extern const int c_bc7_total_bits_endpoints[8]; 
extern const int c_bc7_total_bits_pbits[8]; 
extern const int c_bc7_total_bits_indices[8]; 

extern const bc7bitrange c_bc7bitrange_header[8];
extern const bc7bitsqw c_bc7bitrange_header_mask[8];
extern const bc7bitrange c_bc7bitrange_endpoints[8];
extern const bc7bitsqw c_bc7bitrange_endpoints_mask[8];
extern const bc7bitrange c_bc7bitrange_pbits[8];
extern const bc7bitsqw c_bc7bitrange_pbits_mask[8];
extern const bc7bitrange c_bc7bitrange_indices[8];
extern const bc7bitsqw c_bc7bitrange_indices_mask[8];
extern const bc7bitrange c_bc7bitrange_header_without_mode[8];
extern const bc7bitsqw c_bc7bitrange_header_without_mode_mask[8];
extern const bc7bitrange c_bc7bitrange_endpoints_with_pbits[8];
extern const bc7bitsqw c_bc7bitrange_endpoints_with_pbits_mask[8];
extern const bc7bitrange c_bc7bitrange_indices_with_pbits[8];
extern const bc7bitsqw c_bc7bitrange_indices_with_pbits_mask[8];

//=====================================

#ifdef __RADSSE2__

struct bc7bits
{
	__m128i	vec;
	
	bc7bits() { }
	explicit bc7bits(__m128i v) : vec(v) { }
	explicit bc7bits(const bc7bitsqw & qw) { vec = _mm_loadu_si128((const __m128i *)&qw); }
	
	void operator = (__m128i v) { vec = v; }
	void operator = (const bc7bitsqw & qw) { vec = _mm_loadu_si128((const __m128i *)&qw); }
};

static RADINLINE void bc7bits_set_zero(bc7bits * pbits)
{
	pbits->vec = _mm_setzero_si128();
}

static RADINLINE bool operator == (const bc7bits &lhs, const bc7bits &rhs)
{
	// ptest ; xor then _mm_test_all_zeros
	int mask = _mm_movemask_epi8( _mm_cmpeq_epi8(lhs.vec,rhs.vec) );
	return mask == 0xffff;
}
static RADINLINE bool operator != (const bc7bits &lhs, const bc7bits &rhs)
{
	int mask = _mm_movemask_epi8( _mm_cmpeq_epi8(lhs.vec,rhs.vec) );
	return mask != 0xffff;
}

static RADINLINE bool operator < (const bc7bits &lhs, const bc7bits &rhs)
{
	// interesting way to get a total order :
	//  make the mask of all the byte compares
	//	and then go through the bits of the mask from top to bottom
	//	  if "less" bit is on -> return true
	//	  if "greater" bit is on -> return false
	//	  else if neither bit is on, its' equal, go to next bit
	//  that's just the same as comparing the masks as integers
	//	  (or subtracting the masks and comparing to zero)

	// SSE has no native lt/gt epu8 , we make some that are a bit slow :
    //__m128i less_vec = _mm_cmplt_epu8(lhs.vec,rhs.vec);
    //__m128i gt_vec = _mm_cmpgt_epu8(lhs.vec,rhs.vec);
    
    /*
    // bias both and use the native lt/gt on signed bytes :
	const __m128i bias = _mm_set1_epi8(-0x80);
	__m128i lhs_biased = _mm_add_epi8(lhs.vec, bias);
	__m128i rhs_biased = _mm_add_epi8(rhs.vec, bias);

	__m128i less_vec = _mm_cmplt_epi8(lhs_biased,rhs_biased);
	__m128i gt_vec   = _mm_cmpgt_epi8(lhs_biased,rhs_biased);
   
    const int less = _mm_movemask_epi8( less_vec );
    const int greater = _mm_movemask_epi8( gt_vec );

    bool ret = less > greater;
    /**/
    
    // we want to do LT > GT
    // use the fact that mask LT = ~ mask GE
    
    // this is the same as the below :
	//__m128i le_vec = _mm_cmple_epu8(lhs.vec,rhs.vec);
	//__m128i ge_vec = _mm_cmpge_epu8(lhs.vec,rhs.vec);
   
	__m128i max_vec = _mm_max_epu8(lhs.vec,rhs.vec);
	__m128i le_vec = _mm_cmpeq_epi8(rhs.vec, max_vec); // lhs <= rhs
	__m128i ge_vec = _mm_cmpeq_epi8(lhs.vec, max_vec); // lhs >= rhs
   
    const int le = _mm_movemask_epi8( le_vec );
    const int ge = _mm_movemask_epi8( ge_vec );

	// we want the ~ of the masks
	// use negative and reverse the compare :
	//-x = ~x + 1

    bool ret = ge < le;
    
    
    /*
    // check that it's equal to scalar way :
	U64 lhs_qw[2];
	U64 rhs_qw[2];
	_mm_storeu_si128((__m128i *)lhs_qw,lhs.vec);
	_mm_storeu_si128((__m128i *)rhs_qw,rhs.vec);
	bool check_ret;
	if ( lhs_qw[1] == rhs_qw[1] )
		check_ret = lhs_qw[0] < rhs_qw[0];
	else
		check_ret = lhs_qw[1] < rhs_qw[1];

	RR_ASSERT( ret == check_ret );
	*/

	return ret;		
}

static RADINLINE U8 * bc7bits_U8ptr(bc7bits * block)
{
	return (U8 *)block;
}
static RADINLINE const U8 * bc7bits_U8ptr(const bc7bits * block)
{
	return (const U8 *)block;
}

static RADINLINE bc7bits bc7bits_load(const U8 * ptr)
{
	return bc7bits( _mm_loadu_si128( (const __m128i *)ptr ) );
}

static RADINLINE void bc7bits_store(U8 * ptr,const bc7bits & lhs)
{
	_mm_storeu_si128( (__m128i *)ptr , lhs.vec );
}

static RADINLINE bc7bits bc7bits_and(const bc7bits &lhs, const bc7bits &rhs)
{
	return bc7bits( _mm_and_si128( lhs.vec, rhs.vec ) );
}
static RADINLINE bc7bits bc7bits_or(const bc7bits &lhs, const bc7bits &rhs)
{
	return bc7bits( _mm_or_si128( lhs.vec, rhs.vec ) );
}
static RADINLINE bc7bits bc7bits_xor(const bc7bits &lhs, const bc7bits &rhs)
{
	return bc7bits( _mm_xor_si128( lhs.vec, rhs.vec ) );
}
static RADINLINE bc7bits bc7bits_not(const bc7bits &rhs)
{
	return bc7bits( _mm_andnot_si128( rhs.vec , _mm_set1_epi16( (short)-1 ) ) );
}
static RADINLINE bc7bits bc7bits_andnot(const bc7bits &lhs, const bc7bits &rhs_will_be_notted)
{
	// arg order change :
	return bc7bits( _mm_andnot_si128( rhs_will_be_notted.vec , lhs.vec ) );
}

static RADINLINE U64 bc7bits_qw1(const bc7bits &rhs)
{
    #if defined(_MSC_VER) && (_MSC_VER > 1400) && defined(__RAD64REGS__)
	__m128i n = rhs.vec;
    const __m128i n_hi = _mm_unpackhi_epi64(n, n);
    return _mm_cvtsi128_si64(n_hi);
    #else
	U64 qw[2];
	_mm_storeu_si128((__m128i *)qw,rhs.vec);
	return qw[1];
    #endif
}

static RADINLINE U64 bc7bits_qw0(const bc7bits &rhs)
{
    #if defined(_MSC_VER) && (_MSC_VER > 1400) && defined(__RAD64REGS__)
    return _mm_cvtsi128_si64(rhs.vec);
    #else
	U64 qw[2];
	_mm_storeu_si128((__m128i *)qw,rhs.vec);
	return qw[0];
    #endif
}

#else

#define bc7bits bc7bitsqw

static RADINLINE void bc7bits_set_zero(bc7bits * pbits)
{
	pbits->qw[0] = pbits->qw[1] = 0;
}

static RADINLINE bool operator == (const bc7bits &lhs, const bc7bits &rhs)
{
	return lhs.qw[0] == rhs.qw[0] && lhs.qw[1] == rhs.qw[1];
}
static RADINLINE bool operator != (const bc7bits &lhs, const bc7bits &rhs)
{
	return lhs.qw[0] != rhs.qw[0] || lhs.qw[1] != rhs.qw[1];
}
static RADINLINE bool operator < (const bc7bits &lhs, const bc7bits &rhs)
{
	if ( lhs.qw[1] == rhs.qw[1] )
		return lhs.qw[0] < rhs.qw[0];
	else
		return lhs.qw[1] < rhs.qw[1];
}

static RADINLINE U8 * bc7bits_U8ptr(bc7bits * block)
{
	return (U8 *)block;
}
static RADINLINE const U8 * bc7bits_U8ptr(const bc7bits * block)
{
	return (const U8 *)block;
}

static RADINLINE bc7bits bc7bits_load(const U8 * ptr)
{
	bc7bits ret;
	memcpy(&ret,ptr,16);
	return ret;
}

static RADINLINE void bc7bits_store(U8 * ptr,const bc7bits & lhs)
{
	memcpy(ptr,&lhs,16);
}

static RADINLINE bc7bits bc7bits_and(const bc7bits &lhs, const bc7bits &rhs)
{
	bc7bits ret;
	ret.qw[0] = lhs.qw[0] & rhs.qw[0];
	ret.qw[1] = lhs.qw[1] & rhs.qw[1];
	return ret;
}
static RADINLINE bc7bits bc7bits_or(const bc7bits &lhs, const bc7bits &rhs)
{
	bc7bits ret;
	ret.qw[0] = lhs.qw[0] | rhs.qw[0];
	ret.qw[1] = lhs.qw[1] | rhs.qw[1];
	return ret;
}
static RADINLINE bc7bits bc7bits_xor(const bc7bits &lhs, const bc7bits &rhs)
{
	bc7bits ret;
	ret.qw[0] = lhs.qw[0] ^ rhs.qw[0];
	ret.qw[1] = lhs.qw[1] ^ rhs.qw[1];
	return ret;
}
static RADINLINE bc7bits bc7bits_not(const bc7bits &rhs)
{
	bc7bits ret;
	ret.qw[0] = ~ rhs.qw[0];
	ret.qw[1] = ~ rhs.qw[1];
	return ret;
}
static RADINLINE bc7bits bc7bits_andnot(const bc7bits &lhs, const bc7bits &rhs_will_be_notted)
{
	return bc7bits_and( lhs, bc7bits_not(rhs_will_be_notted) );
}

static RADINLINE U64 bc7bits_qw0(const bc7bits &rhs)
{
	return rhs.qw[0];
}

static RADINLINE U64 bc7bits_qw1(const bc7bits &rhs)
{
	return rhs.qw[1];
}

#endif

static RADINLINE bool bc7bits_iszero(const bc7bits &rhs)
{
	bc7bits lhs;
	bc7bits_set_zero(&lhs);
	return lhs == rhs;
}

// lhs has rhs bits on ; assert they are on and turn them off
static RADINLINE bc7bits bc7bits_xor_assert_on(const bc7bits &lhs, const bc7bits &rhs)
{
	RR_ASSERT( bc7bits_and(lhs,rhs) == rhs );
	return bc7bits_xor(lhs,rhs);
}

// or together {lhs|rhs} , assert they don't overlap
static RADINLINE bc7bits bc7bits_or_assert_exclusive(const bc7bits &lhs, const bc7bits &rhs)
{
	RR_ASSERT( bc7bits_iszero( bc7bits_and(lhs,rhs) ) );
	return bc7bits_or(lhs,rhs);
}

int bc7bits_get_mode(const bc7bits & block);
int bc7bits_get_part(const bc7bits & block,int mode);

bc7bits bc7bits_zero_indices(const bc7bits &block, bool also_zero_pbits);
bc7bits bc7bits_zero_endpoints(const bc7bits &block, bool also_zero_pbits);

bc7bits bc7bits_extract_indices(const bc7bits &block, bool with_pbits);
bc7bits bc7bits_extract_endpoints(const bc7bits &block, bool with_pbits);

#if 0

bc7bits bc7bitrange_makemask(const bc7bitrange & br);

static RADINLINE bc7bits bc7bits_extract(const bc7bits &block, const bc7bitrange &range)
{
	bc7bits mask = bc7bitrange_makemask(range);
	return bc7bits_and(block,mask);
}
static RADINLINE bc7bits bc7bits_extract_not(const bc7bits &block, const bc7bitrange &range)
{
	// or bc7bits_extract then xor
	bc7bits mask = bc7bitrange_makemask(range);
	mask = bc7bits_not(mask);
	return bc7bits_and(block,mask);
}

bc7bitrange bc7bitrange_header(int mode);
bc7bitrange bc7bitrange_endpoints(int mode);
bc7bitrange bc7bitrange_pbits(int mode);
bc7bitrange bc7bitrange_indices(int mode);
bc7bitrange bc7bitrange_header_without_mode(int mode);
bc7bitrange bc7bitrange_endpoints_with_pbits(int mode);
bc7bitrange bc7bitrange_indices_with_pbits(int mode);
#endif

//=====================================

OODLE_NS_END
