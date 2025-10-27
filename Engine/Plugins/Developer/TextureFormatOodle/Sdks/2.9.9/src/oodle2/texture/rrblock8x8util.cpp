// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrblock8x8util.h"

RR_NAMESPACE_START

U32 rrSingleChannelBlock8x8_SAD(const rrSingleChannelBlock8x8 * a,const rrSingleChannelBlock8x8 *b)
{
	U32 ret = 0;
	const U8 * pa = a->values;
	const U8 * pb = b->values;
	for(int i=0;i<64;i++)
	{
		ret += RR_ABS((S32)pa[i] - (S32)pb[i]);
	}
	return ret;
}

//=============================
// this is unnormalized hadamard
//	normalized does *= (1/8)
// swizzled puts the coefficients in frequency order like DCT

#define HADAMARD8_1(r1,s1,r2,s2) \
	r2[0*s2] = r1[0*s1] + r1[1*s1]; \
	r2[1*s2] = r1[2*s1] + r1[3*s1]; \
	r2[2*s2] = r1[4*s1] + r1[5*s1]; \
	r2[3*s2] = r1[6*s1] + r1[7*s1]; \
	r2[4*s2] = r1[0*s1] - r1[1*s1]; \
	r2[5*s2] = r1[2*s1] - r1[3*s1]; \
	r2[6*s2] = r1[4*s1] - r1[5*s1]; \
	r2[7*s2] = r1[6*s1] - r1[7*s1];

#define HADAMARD8_1_SWIZZLED(r1,s1,r2,s2) \
	r2[0*s2] = r1[0*s1] + r1[1*s1]; \
	r2[7*s2] = r1[2*s1] + r1[3*s1]; \
	r2[3*s2] = r1[4*s1] + r1[5*s1]; \
	r2[4*s2] = r1[6*s1] + r1[7*s1]; \
	r2[1*s2] = r1[0*s1] - r1[1*s1]; \
	r2[6*s2] = r1[2*s1] - r1[3*s1]; \
	r2[2*s2] = r1[4*s1] - r1[5*s1]; \
	r2[5*s2] = r1[6*s1] - r1[7*s1];

// Hadamard8 on self is okay
	
template <typename t_fm,typename t_to>
RADFORCEINLINE void Hadamard8(const t_fm * fm,t_to * to,int step)
{
	t_to a[8],b[8];
	HADAMARD8_1(fm,step,a,1);
	HADAMARD8_1(a,1,b,1);
	HADAMARD8_1(b,1,to,step);
}

template <typename t_fm,typename t_to>
RADFORCEINLINE void Hadamard8_Swizzled(const t_fm * fm,t_to * to,int step)
{
	t_to a[8],b[8];
	HADAMARD8_1(fm,step,a,1);
	HADAMARD8_1(a,1,b,1);
	HADAMARD8_1_SWIZZLED(b,1,to,step);
}

static void Hadamard8x8_U8_S16(const U8 * fm,S16 * to)
{
	// rows :
	for(int y=0;y<8;y++)
	{
		const U8 * pfm = fm + y*8;
		S16 * pto = to + y*8;
		Hadamard8(pfm,pto,1);
	}
	
	// columns :
	for(int x=0;x<8;x++)
	{
		S16 * pto = to + x;
		Hadamard8(pto,pto,8);
	}
}

template <typename t_type,typename t_ret>
t_ret SumL1(const t_type * data,int n,t_ret ret)
{
	for(int i=0;i<n;i++)
	{
		ret += RR_ABS(data[i]);
	}
	return ret;
}

// SATD = abs delta of AC_L1
U32 rrSingleChannelBlock8x8_Hadamard_AC_L1(const rrSingleChannelBlock8x8 * a)
{
	S16 temp[64];
	Hadamard8x8_U8_S16(a->values,temp);
	
	//temp[0] = 0; // kill DC
	// could kill AC01 and AC10 as well, or a better way of removing a simple linear slope
	
	U32 ret = 0;
	ret = SumL1(temp+1,63,ret);
	
	return ret;
}

//=================================================
//
// the idea of Laplacian_AC_L1 is to detect "noisyness"
//	but to have smooth ramps return zero
//	(unlike Hadamard AC L1 where a smooth ramp and a step have similar AC10)
//
// @@ idea :
//	ideally you'd like to also rule out hard edge steps here as well
//	(for variance masking purposes, though not for edge-preservation purposes)
//	those should cou

template <int t_step>
U32 Row8_Laplacian_AC_L1(const U8 * values)
{
	#define V(n)	values[(n)*t_step]
	
	U32 ret = 0;
	ret += RR_ABS( (S32)V(1) - (V(0) + V(2))/2 );
	ret += RR_ABS( (S32)V(2) - (V(1) + V(3))/2 );
	ret += RR_ABS( (S32)V(3) - (V(2) + V(4))/2 );
	ret += RR_ABS( (S32)V(4) - (V(3) + V(5))/2 );
	ret += RR_ABS( (S32)V(5) - (V(4) + V(6))/2 );
	ret += RR_ABS( (S32)V(6) - (V(5) + V(7))/2 );
	
	#undef V
	
	return ret;
}

U32 rrSingleChannelBlock8x8_Laplacian_AC_L1(const rrSingleChannelBlock8x8 * a)
{
	U32 ret = 0;
	
	// rows
	for(int y=0;y<8;y++)
	{
		ret += Row8_Laplacian_AC_L1<1>(a->values + y*8);	
	}
	
	// columns
	for(int x=0;x<8;x++)
	{
		ret += Row8_Laplacian_AC_L1<8>(a->values + x);	
	}
	
	return ret;
}

RR_NAMESPACE_END
