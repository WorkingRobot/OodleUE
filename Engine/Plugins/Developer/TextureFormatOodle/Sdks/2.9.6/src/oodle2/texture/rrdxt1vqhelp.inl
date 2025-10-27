// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrdxtcblock.h"
#include "vec128.inl"

#ifdef DO_BUILD_SSE4
#include <smmintrin.h>
#endif

RR_NAMESPACE_START

// 4 channel diff :
static RADINLINE U32 ColorBlock4x4_ComputeSAD_RGBA(const rrColorBlock4x4 & lhs,const rrColorBlock4x4 & rhs);
static RADINLINE U32 ColorBlock4x4_ComputeSSD_RGBA(const rrColorBlock4x4 & lhs,const rrColorBlock4x4 & rhs);

// previous block endpoints and original colors
//	trying new indices
// compute SSD
//	essentially decode BC1 then color diffs
	// 4 channel RGBA diff, but A's should be 255 before coming in here
static RADFORCEINLINE U32 BC1_Palette_SSD_RGBA(const rrColorBlock4x4 * colors,const rrColor32BGRA * palette,const U32 in_indices);
static RADFORCEINLINE U32 BC1_Palette_SAD_RGBA(const rrColorBlock4x4 * colors,const rrColor32BGRA * palette,const U32 in_indices);


// BC1 only needs RGB diff, but we do all four channels anyway
//	it's free in SIMD unless you wanted to tight pack to 3 RGB vectors
static RADINLINE U32 ColorBlock4x4_ComputeSAD_RGBA(const rrColorBlock4x4 & lhs,const rrColorBlock4x4 & rhs)
{
	#ifdef __RADSSE2__

	// rrColorBlock4x4 = 64 bytes = four 16x8 vecs :

	__m128i lhs0 = _mm_loadu_si128( ((__m128i *)lhs.colors) + 0 );
	__m128i lhs1 = _mm_loadu_si128( ((__m128i *)lhs.colors) + 1 );
	__m128i lhs2 = _mm_loadu_si128( ((__m128i *)lhs.colors) + 2 );
	__m128i lhs3 = _mm_loadu_si128( ((__m128i *)lhs.colors) + 3 );

	__m128i rhs0 = _mm_loadu_si128( ((__m128i *)rhs.colors) + 0 );
	__m128i rhs1 = _mm_loadu_si128( ((__m128i *)rhs.colors) + 1 );
	__m128i rhs2 = _mm_loadu_si128( ((__m128i *)rhs.colors) + 2 );
	__m128i rhs3 = _mm_loadu_si128( ((__m128i *)rhs.colors) + 3 );
	
	// @@ this add reduction could be done with one less add I think
	//	we have four sad results that are 2x64
	//	we need to sum those 8
	//	could do that in 3 adds not 4 ?
	
	#if 0 // def DO_BUILD_SSE4
	
	// slower :
	
	__m128i h1 = _mm_packus_epi32(
		_mm_sad_epu8(lhs0,rhs0),
		_mm_sad_epu8(lhs1,rhs1) );
	__m128i h2 = _mm_packus_epi32(
		_mm_sad_epu8(lhs2,rhs2),
		_mm_sad_epu8(lhs3,rhs3) );
	__m128i h = _mm_add_epi32(h1,h2);
	
	U32 ret = hsum_epi32_sse2(h);
	
	#else
	
	// individual sub-SADs are at most 255*8 = 2040 so they comfortably fit in 16 bits

	// sums of two are <= 2*2040 = 4080
	__m128i sad01_2x64 = _mm_add_epi16(
		_mm_sad_epu8(lhs0,rhs0),
		_mm_sad_epu8(lhs1,rhs1) );
	
	__m128i sad23_2x64 = _mm_add_epi16(
		_mm_sad_epu8(lhs2,rhs2),
		_mm_sad_epu8(lhs3,rhs3) );
	
	// sum of four is <= 4*2040 = 8160
	__m128i sad_2x64 = _mm_add_epi16( sad01_2x64, sad23_2x64 );

	// can grab the individual elements slightly faster than with a final reduction
	U32 ret = _mm_cvtsi128_si32(sad_2x64) + _mm_extract_epi16(sad_2x64, 4);
    
    #endif
    
    /*
    {
		U32 err = 0;
		
		for(int i=0;i<16;i++)
		{
			err += rrColor32BGRA_DeltaSADRGBA( lhs.colors[i] , rhs.colors[i] );
		}

		RR_ASSERT( err == ret );	
    }
    /**/
    
    return ret;
    	
	#else

	U32 err = 0;
	
	for(int i=0;i<16;i++)
	{
		err += rrColor32BGRA_DeltaSADRGBA( lhs.colors[i] , rhs.colors[i] );
	}
	
	return err;
	
	#endif
}

#ifdef __RADSSE2__

static RADFORCEINLINE __m128i sse_ssd_u8x16_to_u32x4( __m128i v1, __m128i v2 )
{
	#ifdef DO_BUILD_SSE4
	
	// alternative : (SSSE3)
	 //__m128i plus_minus = _mm_setr_epi8( 1,-1,1,-1, 1,-1,1,-1, 1,-1,1,-1, 1,-1,1,-1 );
	__m128i plus_minus = _mm_set1_epi16(0x1FF);
	__m128i sub16_1 = _mm_maddubs_epi16(_mm_unpacklo_epi8(v1, v2), plus_minus);
	__m128i sub16_2 = _mm_maddubs_epi16(_mm_unpackhi_epi8(v1, v2), plus_minus);
	
	#else
	
	__m128i sub8 = _mm_or_si128( _mm_subs_epu8(v1,v2), _mm_subs_epu8(v2,v1) );
	__m128i sub16_1 = _mm_unpacklo_epi8(sub8, _mm_setzero_si128() );
	__m128i sub16_2 = _mm_unpackhi_epi8(sub8, _mm_setzero_si128() );
	
	#endif
	
	__m128i squares32_1 = _mm_madd_epi16(sub16_1,sub16_1);
	__m128i squares32_2 = _mm_madd_epi16(sub16_2,sub16_2);
	
	return _mm_add_epi32(squares32_1,squares32_2);
}

#endif

static RADINLINE U32 ColorBlock4x4_ComputeSSD_RGBA(const rrColorBlock4x4 & lhs,const rrColorBlock4x4 & rhs)
{
	#ifdef __RADSSE2__

	// rrColorBlock4x4 = 64 bytes = four 16x8 vecs :

	__m128i lhs0 = _mm_loadu_si128( ((__m128i *)lhs.colors) + 0 );
	__m128i lhs1 = _mm_loadu_si128( ((__m128i *)lhs.colors) + 1 );
	__m128i lhs2 = _mm_loadu_si128( ((__m128i *)lhs.colors) + 2 );
	__m128i lhs3 = _mm_loadu_si128( ((__m128i *)lhs.colors) + 3 );

	__m128i rhs0 = _mm_loadu_si128( ((__m128i *)rhs.colors) + 0 );
	__m128i rhs1 = _mm_loadu_si128( ((__m128i *)rhs.colors) + 1 );
	__m128i rhs2 = _mm_loadu_si128( ((__m128i *)rhs.colors) + 2 );
	__m128i rhs3 = _mm_loadu_si128( ((__m128i *)rhs.colors) + 3 );
	
	__m128i squares32 = sse_ssd_u8x16_to_u32x4(lhs0,rhs0);
	squares32 = _mm_add_epi32(squares32, sse_ssd_u8x16_to_u32x4(lhs1,rhs1) );
	squares32 = _mm_add_epi32(squares32, sse_ssd_u8x16_to_u32x4(lhs2,rhs2) );
	squares32 = _mm_add_epi32(squares32, sse_ssd_u8x16_to_u32x4(lhs3,rhs3) );
	
	U32 ret = reduce_add_u32(squares32);
	
    /*
    {
		U32 err = 0;
		
		for(int i=0;i<16;i++)
		{
			err += rrColor32BGRA_DeltaSqrRGBA( lhs.colors[i] , rhs.colors[i] );
		}

		RR_ASSERT( err == ret );	
    }
    /**/
    
    return ret;
    	
	#else

	U32 err = 0;
	
	for(int i=0;i<16;i++)
	{
		err += rrColor32BGRA_DeltaSqrRGBA( lhs.colors[i] , rhs.colors[i] );
	}
	
	return err;
	
	#endif
}


static RADFORCEINLINE U32 BC1_Palette_SAD_RGBA(const rrColorBlock4x4 * colors,const rrColor32BGRA * palette,const U32 in_indices)
{
	// 4 channel RGBA diff, but A's should be 255 before coming in here
	
	U32 indices = in_indices;
	
	#ifndef __RADSSE2__
	
	U32 err = 0;
	
	for(int i=0;i<16;i++)
	{
		err += rrColor32BGRA_DeltaSADRGBA( colors->colors[i] , palette[indices&3] );
		indices >>= 2;
	}
	
	//indices = in_indices;
	return err;
	
	#else
	
	__m128i sad2x64_accum = _mm_setzero_si128();
	
	#ifdef DO_BUILD_SSE4

	// load palette vec and then select from it
	__m128i pal_vec = _mm_loadu_si128((const __m128i *)palette);

	// index_vec has indices pre-shifted 2 to make room for RGBA byte index in the bottom						
	__m128i index_vec = _mm_setr_epi32(	(indices<<2),(indices   ),(indices>>2),(indices>>4) );
	
	index_vec = _mm_and_si128( index_vec , _mm_set1_epi32(0x0C0C0C0C) );
	
	#endif
	
	for(int r=0;r<4;r++)
	{
		const rrColor32BGRA * row = colors->colors+r*4;
		
		__m128i v1 = _mm_loadu_si128((const __m128i *)row);
		
		/*
		RAD_ALIGN(rrColor32BGRA,rowpal,16) [4];
		rowpal[0] = palette[(indices   )&3];
		rowpal[1] = palette[(indices>>2)&3];
		rowpal[2] = palette[(indices>>4)&3];
		rowpal[3] = palette[(indices>>6)&3];
		__m128i v2ref = _mm_loadu_si128((const __m128i *)rowpal);
		/**/
		
		#ifdef DO_BUILD_SSE4
		
		// make 4 copies of each index byte
		__m128i index_broadcast = _mm_shuffle_epi8(index_vec, 
			_mm_setr_epi8(0,0,0,0, 4,4,4,4, 8,8,8,8, 12,12,12,12));

		// or in RGBA byte index :
		__m128i pal_index = _mm_or_si128(index_broadcast, _mm_set1_epi32(0x03020100));

		__m128i v2 = _mm_shuffle_epi8(pal_vec,pal_index);

		// indexes >>= 8 for next iter
		index_vec = _mm_srli_epi32(index_vec,8);
		
		#else
				
		__m128i v2 = _mm_set_epi32( palette[(indices>>6)&3].dw,
									palette[(indices>>4)&3].dw,
									palette[(indices>>2)&3].dw,
									palette[(indices>>0)&3].dw);
		
		indices >>= 8;
		
		#endif

		__m128i sad2x64 = _mm_sad_epu8(v1,v2);
		sad2x64_accum = _mm_add_epi64( sad2x64_accum, sad2x64 );
	}
	
	// add two 16-bit partial sums in the 64-bit halves :
    U32 sad_sse = _mm_cvtsi128_si32(_mm_add_epi64(sad2x64_accum, _mm_srli_si128(sad2x64_accum, 8)));		
		
	//RR_ASSERT_ALWAYS( err == sad_sse );
		
	return sad_sse;
	
	#endif
}

// previous block endpoints and original colors
//	trying new indices
// compute SSD
//	essentially decode BC1 then color diffs
static RADFORCEINLINE U32 BC1_Palette_SSD_RGBA(const rrColorBlock4x4 * colors,const rrColor32BGRA * palette,const U32 in_indices)
{
	// 4 channel RGBA diff, but A's should be 0/255 before coming in here
	// NOTE: this is actual RGBA SSD , no A bias
	
	U32 indices = in_indices;
	
	#ifndef __RADSSE2__
	
	U32 ssd_ref = 0;
	for(int i=0;i<16;i++)
	{
		ssd_ref += rrColor32BGRA_DeltaSqrRGBA( colors->colors[i] , palette[indices&3] );
		indices >>= 2;
	}
	//indices = in_indices;

	return ssd_ref;

	#else
	
	// SSE2 or 4
	
	#ifdef DO_BUILD_SSE4

	// load palette vec and then select from it
	__m128i pal_vec = _mm_loadu_si128((const __m128i *)palette);

	// index_vec has indices pre-shifted 2 to make room for RGBA byte index in the bottom						
	__m128i index_vec = _mm_setr_epi32(	(indices<<2),(indices   ),(indices>>2),(indices>>4) );
	
	index_vec = _mm_and_si128( index_vec , _mm_set1_epi32(0x0C0C0C0C) );
	
	#endif
	
	__m128i squares32 = _mm_setzero_si128();
		
	for(int r=0;r<4;r++)
	{
		const rrColor32BGRA * row = colors->colors+r*4;
				
		__m128i v1 = _mm_loadu_si128((const __m128i *)row);
		
		#ifdef DO_BUILD_SSE4
		
		// make 4 copies of each index byte
		__m128i index_broadcast = _mm_shuffle_epi8(index_vec, 
			_mm_setr_epi8(0,0,0,0, 4,4,4,4, 8,8,8,8, 12,12,12,12));

		// or in RGBA byte index :
		__m128i pal_index = _mm_or_si128(index_broadcast, _mm_set1_epi32(0x03020100));

		__m128i v2 = _mm_shuffle_epi8(pal_vec,pal_index);

		// indexes >>= 8 for next iter
		index_vec = _mm_srli_epi32(index_vec,8);
		
		#else
				
		__m128i v2 = _mm_set_epi32( palette[(indices>>6)&3].dw,
									palette[(indices>>4)&3].dw,
									palette[(indices>>2)&3].dw,
									palette[(indices>>0)&3].dw);
		
		indices >>= 8;
		
		#endif
		
		// note : does Alpha diffs too
		//	but all A's should be 255 here so should be the same
			
		squares32 = _mm_add_epi32(squares32, sse_ssd_u8x16_to_u32x4(v1,v2) );
	}
	
	U32 ssd = reduce_add_u32(squares32);
	
	//RR_ASSERT_ALWAYS( ssd == ssd_ref );
	
	return ssd;
	
	#endif
}


//=============================================================

// WARNING : reject_endpoints_color_bbox much code dupe with endpoints_color_bbox_dsqr !!
//	

#ifdef DO_BUILD_SSE4

// reject returns true if too far away		
static RADINLINE bool reject_endpoints_color_bbox_sse4(const rrColor32BGRA * endpoints, const rrColor32BGRA * color_bbox_lohi, U32 min_d)
{
	__m128i ep = load64u( endpoints );
	// ep0 and ep1 in ep[0] and ep[1]

	// avg(x,x) == x, so we can do some shuffles and then average to give us
	//   ep0, ep1, avg(ep0,ep1), avg(ep0,ep1)
	ep = _mm_avg_epu8(shuffle32<0,1,0,1>(ep), shuffle32<0,1,1,0>(ep));

	// unpack bbox
	__m128i bbox_lohi = load64u( color_bbox_lohi );
	__m128i bbox_lo = shuffle32<0,0,0,0>(bbox_lohi);
	__m128i bbox_hi = shuffle32<1,1,1,1>(bbox_lohi);

	// Originally we clamped ep to inside box, and then computed squared distance between
	// original ep and ep_in_box, but we can get that more directly with saturating math:
	//
	//    x - min(max(x, lo), hi) = max(lo - x, 0) + max(x - hi, 0)
	//
	// (Proof: since lo <= hi, consider the three cases 1. x < lo 2. lo <= x <= hi 3. hi < x.
	// In the second case, the clamp does nothing in that component, so we compute ep - ep = 0
	// in the original calc. In case 1, lo - x > 0 so the first summand is the axial
	// distance and the second summand is certainly zero, since x < lo <= hi. Case 3 is
	// symmetric.)
	__m128i sub8 = _mm_add_epi8(_mm_subs_epu8(bbox_lo, ep), _mm_subs_epu8(ep, bbox_hi));
	__m128i sub16_1 = _mm_and_si128(sub8, _mm_set1_epi16(0xff)); // dists in B and R
	__m128i sub16_2 = _mm_srli_epi16(sub8, 8); // dists in G and A

	// sub16_1 has the even-channel distances (16b/channel)
	// sub16_2 has the odd-chanel distances
	//
	// now compute the dot products for the squared distances
	// giving us 32b lanes per endpoint:
	//
	//   dots_1 = db*db + dr*dr
	//   dots_2 = dg*dg + da*da
	Vec128 dots_1 = _mm_madd_epi16(sub16_1,sub16_1);
	Vec128 dots_2 = _mm_madd_epi16(sub16_2,sub16_2);

	// Sum the dot products to get the squared distances considering all channels
	Vec128 distances = _mm_add_epi32(dots_1, dots_2);

	// We want to reject the point if the minimum of all 3 distances is > min_d
	// which happens iff all 3 distances are. We set up the final lane (3) to
	// also contain the endpoint midpoint so it's always the same as lane 2
	// and doesn't change the result.
	Vec128 above_min = _mm_cmpgt_epi32(distances, _mm_set1_epi32(min_d));

	// PTEST(a,b) sets ZF if all bits of (a & b) == 0,
	// and CF if all bits of (~a & b) == 0, which we use this way to test if
	// all our compares flagged as greater:
	return _mm_testc_si128(above_min, _mm_set1_epi32(-1));
}

#endif
	
// reject returns true if too far away		
static bool reject_endpoints_color_bbox_scalar(const rrColor32BGRA * endpoints, const rrColor32BGRA * color_bbox_lohi, U32 min_d)
{
	// find distance of candidate vq endpoints to the color bbox of this block
	//	one or the other vq endpoint must be close to the color bbox
	rrColor32BGRA ep0 = endpoints[0];
	rrColor32BGRA ep1 = endpoints[1];
	rrColor32BGRA color_bbox_lo = color_bbox_lohi[0];
	rrColor32BGRA color_bbox_hi = color_bbox_lohi[1];
	
	// RGB part only :
	RR_ASSERT( color_bbox_lo.u.a == 255 );
	RR_ASSERT( color_bbox_hi.u.a == 255 );
	RR_ASSERT( ep0.u.a == 255 );
	RR_ASSERT( ep1.u.a == 255 );

	rrColor32BGRA ep0_in_box;
	rrColor32BGRA ep1_in_box;
	
	ep0_in_box.u.b = RR_CLAMP(ep0.u.b,color_bbox_lo.u.b,color_bbox_hi.u.b);
	ep0_in_box.u.g = RR_CLAMP(ep0.u.g,color_bbox_lo.u.g,color_bbox_hi.u.g);
	ep0_in_box.u.r = RR_CLAMP(ep0.u.r,color_bbox_lo.u.r,color_bbox_hi.u.r);
	
	ep1_in_box.u.b = RR_CLAMP(ep1.u.b,color_bbox_lo.u.b,color_bbox_hi.u.b);
	ep1_in_box.u.g = RR_CLAMP(ep1.u.g,color_bbox_lo.u.g,color_bbox_hi.u.g);
	ep1_in_box.u.r = RR_CLAMP(ep1.u.r,color_bbox_lo.u.r,color_bbox_hi.u.r);
	
	U32 d0 = rrColor32BGRA_DeltaSqrRGB(ep0,ep0_in_box);
	U32 d1 = rrColor32BGRA_DeltaSqrRGB(ep1,ep1_in_box);
	
	// vq endpoints one or the other must be close to bbox
	U32 d = RR_MIN(d0,d1);
	
	if ( d <= min_d )
		return false;
				
	// @@ check midpoint of the endpoints also, and then use a tighter distance?
	//	how is it that it's important to consider encodings where neither endpoint
	//	 is close to the color bbox !?
	// with midpoint a dsqr of 200 is nearly a quality nop
	//	that's a step of 14 in one component
	//	in 565 endpoints the step is 8 or 4
	
	#if 1
	{
		rrColor32BGRA midpoint,midpoint_in_box;
		// +1 to match _mm_avg_epu8
		midpoint.u.b = (ep0.u.b + ep1.u.b +1)>>1;
		midpoint.u.g = (ep0.u.g + ep1.u.g +1)>>1;
		midpoint.u.r = (ep0.u.r + ep1.u.r +1)>>1;
		
		midpoint_in_box.u.b = RR_CLAMP(midpoint.u.b,color_bbox_lo.u.b,color_bbox_hi.u.b);
		midpoint_in_box.u.g = RR_CLAMP(midpoint.u.g,color_bbox_lo.u.g,color_bbox_hi.u.g);
		midpoint_in_box.u.r = RR_CLAMP(midpoint.u.r,color_bbox_lo.u.r,color_bbox_hi.u.r);
		
		U32 midpoint_d = rrColor32BGRA_DeltaSqrRGB(midpoint,midpoint_in_box);
		if ( midpoint_d > min_d )
		{
			return true;
		}
	}
	#else
	// don't check midpoint, use larger min_d
			return true;
	#endif
	
	// some d <= min_d
	return false;
}

#ifdef DO_BUILD_SSE4
#define reject_endpoints_color_bbox reject_endpoints_color_bbox_sse4
#else
#define reject_endpoints_color_bbox reject_endpoints_color_bbox_scalar
#endif

#define FIND_NEXT_BELOW_PADDING 8

// Finds next i >= i0 where vals[i] < x; such an i _must_ exist! Use a sentinel
// to ensure. Furthermore, there must be at least FIND_NEXT_BELOW_PADDING
// sentinel values after the final real value, to enable SIMD versions.
//
// Used for hot "find next block with SAD below threshold" loops.
static RADINLINE int find_next_below_scalar(const int * vals, int i0, int x)
{
	int i = i0;
	while (vals[i] >= x)
		++i;

	return i;
}

// Finds next i >= i0 where vals[i] < x; such an i _must_ exist! Use a sentinel
// to ensure. Furthermore, there must be at least FIND_NEXT_BELOW_PADDING
// sentinel values after the final real value, to enable SIMD versions.
//
// Used for hot "find next block with VQD below threshold" loops.
static RADINLINE int find_next_below_scalar(const F32 * vals, int i0, F32 x)
{
	int i = i0;
	// For floats, !(a < b) is not the same as (a >= b) so be careful here
	while ( ! ( vals[i] < x ) )
		++i;

	return i;
}


#ifdef __RADSSE2__

static RADINLINE int find_next_below_sse2(const int * vals, int i0, int x)
{
	Vec128 vx = _mm_set1_epi32(x);
	int mask = 0;
	int i = i0 - 8; // pre-decrement by 8 to set up for loop below

	do
	{
		// find positions i where vx > vals[i]
		i += 8;
		Vec128 cmp0 = _mm_cmpgt_epi32(vx, load128u(vals + i + 0));
		Vec128 cmp1 = _mm_cmpgt_epi32(vx, load128u(vals + i + 4));
		Vec128 cmp01 = _mm_packs_epi32(cmp0, cmp1);
		mask = _mm_movemask_epi8(cmp01);
	} while (!mask);

	i += rrCtz32(mask) >> 1; // mask has two bits for every 32-bit lane since it's byte-wise and we only packed once
	return i;
}

static RADINLINE int find_next_below_sse2(const F32 * vals, int i0, F32 x)
{
	VecF32x4 vx(x);
	int mask = 0;
	int i = i0 - 8; // pre-decrement by 8 to set up for loop below

	do
	{
		// find positions i where vx > vals[i]
		i += 8;
		VecF32x4 cmp0 = vx.cmp_gt(VecF32x4::loadu(vals + i + 0));
		VecF32x4 cmp1 = vx.cmp_gt(VecF32x4::loadu(vals + i + 4));
		Vec128 cmp01 = _mm_packs_epi32(_mm_castps_si128(cmp0), _mm_castps_si128(cmp1));
		mask = _mm_movemask_epi8(cmp01);
	} while (!mask);

	i += rrCtz32(mask) >> 1; // mask has two bits for every 32-bit lane since it's byte-wise and we only packed once
	return i;
}

#endif

#ifdef __RADSSE2__
#define find_next_below find_next_below_sse2
#else
#define find_next_below find_next_below_scalar
#endif

static RADFORCEINLINE U32 unpacked_16x8_diff( const U8 * p1, const U8 * p2 )
{
	// @@ temp not aligned
	//RR_ASSERT( rrIsAlignedPointer(p1,16) );
	//RR_ASSERT( rrIsAlignedPointer(p2,16) );

	// SAD :

	#ifdef __RADSSE2__

	Vec128 v1 = load128u(p1);
	Vec128 v2 = load128u(p2);
	// SAD instruction is so nice :
	Vec128 sad2x64 = _mm_sad_epu8(v1,v2);
	// add two 16-bit partial sums in the 64-bit halves :
    return _mm_cvtsi128_si32(_mm_add_epi32(sad2x64, shuffle32<2,2,2,2>(sad2x64)));

    #else

    // experiment with a more correct diff for 3c :
	// -> this doesn't help

    // can do this with xor & blend in simd
    // xor v1^v2
    // if the == black bit is not equal, top bits will be on
    // if top bits are on, instead of SAD change to 12

    U32 ret =0;
    for LOOP(i,16)
    {
		int t = p1[i] - p2[i];
		t = RR_ABS(t);

		/*
		// putting the black as value 13 on the same scale as other indices is wrong
		// there should be no difference between 0->13 vs 6->13
		// any diff >= 7 should be the same
		if ( t >= 7 )
			t = 12;
		*/

		ret += t;
	}
	return ret;

    #endif
}

#ifdef DO_BUILD_SSE4

// Vec128 lanes correspond to element ids base_id + 0, base_id + 1, base_id + 2, base_id + 3
// (base_id should have the same value in all four lanes). Count lanes where active_mask has
// the MSB set, store the associated IDs for those lanes to dest (sloppily writing up to 3
// extra slots), and return the number of lanes that were active.
//
// This sounds very specific, and it is, but it's a useful building block for "filter" style
// operations. (If you have AVX512, you can just use VCOMPRESS for this, but alas, we can't
// assume that.)
static RADFORCEINLINE SINTa store_active_lane_ids( int * dest, const Vec128 & base_id, const Vec128 & active_mask )
{
	static RAD_ALIGN(const S32, indices[16][4], 16) =
	{
		{ 0,0,0,0 }, // 0000
		{ 0,0,0,0 }, // 0001
		{ 1,0,0,0 }, // 0010
		{ 0,1,0,0 }, // 0011
		{ 2,0,0,0 }, // 0100
		{ 0,2,0,0 }, // 0101
		{ 1,2,0,0 }, // 0110
		{ 0,1,2,0 }, // 0111
		{ 3,0,0,0 }, // 1000
		{ 0,3,0,0 }, // 1001
		{ 1,3,0,0 }, // 1010
		{ 0,1,3,0 }, // 1011
		{ 2,3,0,0 }, // 1100
		{ 0,2,3,0 }, // 1101
		{ 1,2,3,0 }, // 1110
		{ 0,1,2,3 }, // 1111
	};
	static const U8 popcount[16] =
	{
		0,1,1,2, 1,2,2,3, 1,2,2,3, 2,3,3,4
	};

	SINTa active_mask_int = (U32)_mm_movemask_ps(_mm_castsi128_ps(active_mask)); // U32 cast to prevent pointless sign extend

	store128u(dest, _mm_add_epi32(load128u(indices[active_mask_int]), base_id));
	return popcount[active_mask_int];
}

static RADFORCEINLINE SINTa store_active_values( int * dest, const Vec128 & values, const Vec128 & active_mask )
{
	static RAD_ALIGN(const S8, shuffles[16][16], 16) =
	{
	#define A 0,1,2,3
	#define B 4,5,6,7
	#define C 8,9,10,11
	#define D 12,13,14,15
	#define _ -1,-1,-1,-1

		{ _,_,_,_ }, // 0000
		{ A,_,_,_ }, // 0001
		{ B,_,_,_ }, // 0010
		{ A,B,_,_ }, // 0011
		{ C,_,_,_ }, // 0100
		{ A,C,_,_ }, // 0101
		{ B,C,_,_ }, // 0110
		{ A,B,C,_ }, // 0111
		{ D,_,_,_ }, // 1000
		{ A,D,_,_ }, // 1001
		{ B,D,_,_ }, // 1010
		{ A,B,D,_ }, // 1011
		{ C,D,_,_ }, // 1100
		{ A,C,D,_ }, // 1101
		{ B,C,D,_ }, // 1110
		{ A,B,C,D }, // 1111

	#undef A
	#undef B
	#undef C
	#undef D
	#undef _
	};
	static const U8 popcount[16] =
	{
		0,1,1,2, 1,2,2,3, 1,2,2,3, 2,3,3,4
	};

	SINTa active_mask_int = (U32)_mm_movemask_ps(_mm_castsi128_ps(active_mask)); // U32 cast to prevent pointless sign extend

	store128u(dest, _mm_shuffle_epi8(values, load128a(shuffles[active_mask_int])));
	return popcount[active_mask_int];
}

#endif // DO_BUILD_SSE4

//=============================================================

static U32 AnyIndexD_lookup(const AnyIndexD * aid, U32 in_indices)
{
	U32 indices = in_indices;

#ifdef DO_BUILD_SSE4
	// Set up two index vectors, one for the low and one for the high bit in every index
	// Start by braodcasting every byte of the index 4 times:
	const Vec128 c_replicate4x = _mm_setr_epi8(0,0,0,0, 1,1,1,1, 2,2,2,2, 3,3,3,3);
	const Vec128 v_inds = _mm_shuffle_epi8(_mm_cvtsi32_si128(indices), c_replicate4x);

	// Determine positions where bit 0 and 1 of the index is set
	// this gives us a 16-lane byte vector with a byte that's either 0 or 0xff
	// depending on whether the corresponding index bit is clear or set.
	const Vec128 c_bit0mask = _mm_setr_epi8(1<<0,1<<2,1<<4,1<<6, 1<<0,1<<2,1<<4,1<<6, 1<<0,1<<2,1<<4,1<<6, 1<<0,1<<2,1<<4,1<<6);
	const Vec128 c_bit1mask = _mm_setr_epi8(1<<1,1<<3,1<<5,-(1<<7), 1<<1,1<<3,1<<5,-(1<<7), 1<<1,1<<3,1<<5,-(1<<7), 1<<1,1<<3,1<<5,-(1<<7));

	Vec128 v_ind0 = _mm_cmpeq_epi8(_mm_and_si128(v_inds, c_bit0mask), c_bit0mask);
	Vec128 v_ind1 = _mm_cmpeq_epi8(_mm_and_si128(v_inds, c_bit1mask), c_bit1mask);
	Vec128 v_shuf = c_replicate4x;
	Vec128 v_ssd = _mm_setzero_si128();

	for (int c = 0; c < 16; c += 4)
	{
		// Get blend mask for low bit
		Vec128 v_mask_ind0 = _mm_shuffle_epi8(v_ind0, v_shuf);
		// Select between the 4 possible error values depending on the chosen low index bit
		Vec128 v_vals01 = _mm_blendv_epi8(load128u(&aid->ssd[0][c]), load128u(&aid->ssd[1][c]), v_mask_ind0);
		Vec128 v_vals23 = _mm_blendv_epi8(load128u(&aid->ssd[2][c]), load128u(&aid->ssd[3][c]), v_mask_ind0);

		// Blend based on high bit
		Vec128 v_mask_ind1 = _mm_shuffle_epi8(v_ind1, v_shuf);
		Vec128 v_vals = _mm_blendv_epi8(v_vals01, v_vals23, v_mask_ind1);

		// Add into total
		v_ssd = _mm_add_epi32(v_ssd, v_vals);

		// Advance shuffle
		v_shuf = _mm_add_epi8(v_shuf, _mm_set1_epi8(4));
	}

	// Horizontal sum at the end
	U32 ssd = reduce_add_u32(v_ssd);
#else
	U32 ssd = 0;
	for LOOP(c,16)
	{
		U32 cur = indices&3; indices>>=2;
		ssd += aid->ssd[cur][c];
	}
#endif

	U32 D = 2 * ssd;

	return D;
}

static S64 AnyIndex8D64_lookup(const AnyIndex8D64 * aid, const U8 *indices8_scaled)
{
	S64 ssd = 0;
	for LOOP(c,16)
	{
		U32 cur = indices8_scaled[c] >> 1;
		ssd += aid->ssdx[c][cur];
	}
	return ssd;
}

//=============================================================

// The design of this type of search is elaborated on in
//   "Array layouts for comparison-based sorting"
//   https://arxiv.org/abs/1509.05053

// Similar to std::lower_bound; for sorted "values", returns the
// smallest i such that
//   !(values[i] < x)  (think values[i] >= x)
// and if there is none, returns count.
template<typename T>
static SINTa branchless_lower_bound_idx(const T * values, SINTa count, const T & x)
{
	const T * base = values;
	SINTa nleft = count;

	// Careful branch-free binary search. Invariants:
	// 1. everything in [begin, base) is <x
	// 2. everything in [base + nleft, end) is >=x
	//
	// This code is carefully written so the only thing depending on the comparison with x can
	// be expressed as a conditional move, and the computation for "half" etc. has no data
	// dependencies.
	for (;;)
	{
		// This loop reduces nleft to be <= 1.
		SINTa half = nleft >> 1;
		if ( half <= 0 )
			break;

		// This reduction guarantees the two invariants above. It doesn't shrink nleft quite
		// as quickly as it could but it has the advantage of being a very simple update.
		//
		// Specifically, if (*mid < x), we _could_ set it = mid + 1 and subtract
		// (half + 1) from nleft, but keeping "mid" itself inside the interval makes the
		// updating cheaper and means that the sequence of "nleft" is the same no matter
		// which path we take, making the actual branches in this loop entirely predictable.
		const T * mid = base + half;
		base = (*mid < x) ? mid : base;
		nleft -= half;
	}

	// Now nleft <= 1; if it's ==1, do a final test to get us to the right spot
	if (nleft == 1)
		base += (*base < x) ? 1 : 0;

	//RR_ASSERT(values <= base && base <= values + count);
	//RR_ASSERT(base == values + count || !(*base < x));
	//RR_ASSERT(base == values || (base[-1] < x));

	return base - values;
}

// Similar to std::lower_bound; for sorted "values", returns the
// smallest i such that
//   !cmp_less(values[i], x)    (think values[i] >= x)
// and if there is none, returns count.
template<typename T, typename U, typename Functor>
static SINTa branchless_lower_bound_idx(const T * values, SINTa count, const U & x, const Functor & cmp_less)
{
	const T * base = values;
	SINTa nleft = count;

	// Careful branch-free binary search. Invariants:
	// 1. everything in [begin, base) is <x
	// 2. everything in [base + nleft, end) is >=x
	//
	// This code is carefully written so the only thing depending on the comparison with x can
	// be expressed as a conditional move, and the computation for "half" etc. has no data
	// dependencies.
	for (;;)
	{
		// This loop reduces nleft to be <= 1.
		SINTa half = nleft >> 1;
		if ( half <= 0 )
			break;

		// This reduction guarantees the two invariants above. It doesn't shrink nleft quite
		// as quickly as it could but it has the advantage of being a very simple update.
		//
		// Specifically, if (*mid < x), we _could_ set it = mid + 1 and subtract
		// (half + 1) from nleft, but keeping "mid" itself inside the interval makes the
		// updating cheaper and means that the sequence of "nleft" is the same no matter
		// which path we take, making the actual branches in this loop entirely predictable.
		const T * mid = base + half;
		base = cmp_less(*mid, x) ? mid : base;
		nleft -= half;
	}

	// Now nleft <= 1; if it's ==1, do a final test to get us to the right spot
	if (nleft == 1)
		base += cmp_less(*base, x) ? 1 : 0;

	//RR_ASSERT(values <= base && base <= values + count);
	//RR_ASSERT(base == values + count || !cmp_less(*base, x));
	//RR_ASSERT(base == values || cmp_less(base[-1], x));

	return base - values;
}

//=============================================================

#ifdef DO_BUILD_SSE4

static RADFORCEINLINE SINTa final_matrix_search_simd_step(const U16 * keys, Vec128 vx)
{
    // Do 16 compares of keys[i] < vx
	// NOTE these are actually signed not unsigned
    Vec128 lt0 = _mm_cmpgt_epi16(vx, load128u(keys + 0));
    Vec128 lt1 = _mm_cmpgt_epi16(vx, load128u(keys + 8));

    // Pack results
    Vec128 lt_packed = _mm_packs_epi16(lt0, lt1);

    // Get the bit mask
    int lt_mask = _mm_movemask_epi8(lt_packed);

    // Negate; this also ensures that bit 16 is set so if nothing was found,
    // we return 16, which is what we want
    int nlt_mask = ~lt_mask;

    // Find first set bit (the above guarantees there is one, since the mask
    // is 16 bits and we complemented 32 bits)
    //
    // This is the position where we go from being <x to >=x
	return rrCtz32(nlt_mask);
}

#endif

inline int FinalMatrixL1Index::lookup(const rrColorBlock4x4 & target, U16 const * * pOutInds, int must_beat_dist)
{
	if ( must_beat_dist <= 0 )
		return 0;

#ifdef DO_BUILD_SSE4
	// Compute reference norm
	Vec128 zero = _mm_setzero_si128();
	Vec128 ref_norm;
	ref_norm = _mm_sad_epu8(load128u(target.colors + 0), zero);
	ref_norm = _mm_add_epi32(ref_norm, _mm_sad_epu8(load128u(target.colors + 4), zero));
	ref_norm = _mm_add_epi32(ref_norm, _mm_sad_epu8(load128u(target.colors + 8), zero));
	ref_norm = _mm_add_epi32(ref_norm, _mm_sad_epu8(load128u(target.colors + 12), zero));

	// Sum the two halves
	// we now have two copies of ref_norm, one in lane 0 and one in lane 2
	ref_norm = _mm_add_epi32(ref_norm, shuffle32<2,3,0,1>(ref_norm));

	// Set up -must_beat_dist + 1 in lane 0 and must_beat_dist in lane 2:
	Vec128 v_distances = shuffle32<0,1,0,1>(_mm_cvtsi32_si128(must_beat_dist));
	v_distances = _mm_add_epi32(_mm_xor_si128(v_distances, _mm_setr_epi32(-1,0,0,0)), _mm_setr_epi32(2,0,0,0)); // 1-x = -x + 1 = (~x + 1) + 1 = ~x + 2

	// Sum to get target search range:
	Vec128 search_lo_hi32 = _mm_add_epi32(ref_norm, v_distances);
	Vec128 search_lo_hi16 = _mm_packs_epi32(search_lo_hi32, search_lo_hi32); // saturated to 16-bit signed
	/// search_lo_hi16 as 16-bit lanes is now (lo,0,hi,0, lo,0,hi,0)

	// Now we can materialize the search lower/upper bounds:
	Vec128 search_lo = _mm_shuffle_epi8(search_lo_hi16, _mm_set1_epi16(0x0100));
	Vec128 search_hi = _mm_shuffle_epi8(search_lo_hi16, _mm_set1_epi16(0x0504));

	// Do the actual searches!
	RR_COMPILER_ASSERT( CHUNK_SIZE == 256 ); // this code needs to change when CHUNK_SIZE does

    // Search on the subsampled keys
    // this gives us the index of the group of 16 keys where we go from
    // < to >=
	SINTa l = final_matrix_search_simd_step(sorted_norms_sub, search_lo) * 16;
	SINTa r = final_matrix_search_simd_step(sorted_norms_sub, search_hi) * 16;

    // Refine on the full list of keys once we know the group of 16 where
    // the transition happens
	l += final_matrix_search_simd_step(sorted_norms + l, search_lo);
	r += final_matrix_search_simd_step(sorted_norms + r, search_hi);

	RR_DURING_ASSERT( int ref_norm_int = _mm_cvtsi128_si32(ref_norm) );
	RR_DURING_ASSERT( int search_l = RR_MAX(ref_norm_int - must_beat_dist + 1, -32768) );
	RR_DURING_ASSERT( int search_r = RR_MIN(ref_norm_int + must_beat_dist, 32767) );
	RR_ASSERT( 0 <= l && l <= r && r <= count );
	RR_ASSERT( l == 0 || sorted_norms[l - 1] < search_l );
	RR_ASSERT( r == count || sorted_norms[r] >= search_r );

	*pOutInds = sorted_inds + l;
	return (int)(r - l);
#else
	// Compute reference norm
	rrColorBlock4x4 zero_colors = {}; // zero-init
	const int ref_norm = ColorBlock4x4_ComputeSAD_RGBA(target,zero_colors);

	// Two binary searches to narrow down the range. NOTE: it's pretty important
	// for both of these to use the same range (i.e. to not have the second search
	// start at "l") to keep the branches based on the item count nice and predictable.
	const SINTa l = branchless_lower_bound_idx(sorted_norms, count, static_cast<U16>(RR_MAX(ref_norm - must_beat_dist + 1, 0)));
	const SINTa r = branchless_lower_bound_idx(sorted_norms, count, static_cast<U16>(RR_MIN(ref_norm + must_beat_dist, 65535))); // NOTE exclusive!

	*pOutInds = sorted_inds + l;
	return (int)(r - l);
#endif
}

//=============================================================

RR_NAMESPACE_END
