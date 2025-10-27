// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_VEC_C_UTIL_H__
#define __RADRR_VEC_C_UTIL_H__

#include "rrbase.h"
#include "rrmath.h"
#include "rrvecc.h"
#include "oodlecore.h"

// C++ obv

RR_NAMESPACE_START

OOINLINE const rrVec3f rrVec3f_ProjectPerp(const rrVec3f & v,const rrVec3f & normal)
{
	float dot = (float) ( v * normal );
	return (v - dot * normal);
}

OOINLINE F64 rrVec3f_HorizontalSum(const rrVec3f & v)
{
	return (F64) v.x + v.y + v.z;
}

OOINLINE F64 rrVec3f_TripleProduct(const rrVec3f &a,const rrVec3f &b,const rrVec3f &c)
{
	// return Epsilon_ijk A_i B_j C_k
	const F64 t =
		(F64)
		a.x * (b.y * c.z - b.z * c.y) +
		a.y * (b.z * c.x - b.x * c.z) +
		a.z * (b.x * c.y - b.y * c.x);
	return t;
}
	
RR_NAMESPACE_END



#endif // __RADRR_VEC_C_UTIL_H__
