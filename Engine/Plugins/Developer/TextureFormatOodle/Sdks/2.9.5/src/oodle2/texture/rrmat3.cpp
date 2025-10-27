// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrmat3.h"
#include "rrveccutil.h"
#include "templates/rrstl.h"
#include "rrmemutil.h"
#include <math.h>

#ifdef _MSC_VER
#pragma warning(disable : 4244)
#endif

RR_NAMESPACE_START

void rrMat3_SetZero(rrMat3 * mat)
{
	RR_ZERO(*mat);
}

void rrMat3_SetIdentity(rrMat3 * mat)
{
	RR_ZERO(*mat);
	mat->m_data[0][0] = 1.f;
	mat->m_data[1][1] = 1.f;
	mat->m_data[2][2] = 1.f;
}

F64 rrMat3_GetDeterminant(const rrMat3 * pMat)
{
	return rrVec3f_TripleProduct((*pMat)[0],(*pMat)[1],(*pMat)[2]);
}

/*! get the inverse; can work on self
	NOTEZ : calling this on matrices which have
		no inverse will result in a divide by zero !!
	first check if the Determinant is != 0 if you
	want to be vareful
*/
void rrMat3_GetInverse(const rrMat3 & m,rrMat3 * pInv)
{
	RR_ASSERT( pInv );
	
	// if my determinant is zero, I have no inverse :
	float det = (float) rrMat3_GetDeterminant(&m);
	RR_ASSERT( fabsf(det) > 0.f );

	float invDet = 1.f / det;

	// get the columns :

	const rrVec3f u = rrMat3_GetColumnX(&m);
	const rrVec3f v = rrMat3_GetColumnY(&m);
	const rrVec3f w = rrMat3_GetColumnZ(&m);

	// RowU = (V x W) / ( U * (V x W)) , and so on

	// NOTEZ : calling this on matrices which have
	//	no inverse will result in a divide by zero !!
	//	(RR_ASSERT above on Determinant should catch that)

	const rrVec3f vw = Cross(v , w);
	(*pInv)[0] = vw * invDet ;
	
	const rrVec3f uw = Cross(w , u);
	(*pInv)[1] = uw * invDet ;

	const rrVec3f uv = Cross(u , v);
	(*pInv)[2] = uv * invDet ;

	/*
	#ifdef DO_ASSERTS // make sure it really is the inverse!
	{
	rrMat3 product;
	product.SetProduct(m,*pInv);
	RR_ASSERT(product.IsIdentity(0.005f)); // within EPSILON
	}
	#endif
	*/
}
	
//! float == test with slop
OOINLINE bool fequal(const float f1,const float f2,const float tolerance = RR_EPSILON)
{
	const float diff = fabsf(f1 - f2);
	return diff <= tolerance;
}
OOINLINE bool fisone(const float f1,const float tolerance = RR_EPSILON)
{
	return fequal(f1,1.f,tolerance);
}
OOINLINE bool fiszero(const float f1,const float tolerance = RR_EPSILON)
{
	return fequal(f1,0.f,tolerance);
}

rrbool rrMat3_IsOrthogonal(const rrMat3 & m,const float tolerance /*= RR_EPSILON*/)
{
	// IsOrthogonal is weaker than IsOrthonormal
	return
		fiszero(m[0] * m[1],tolerance) &&
		fiszero(m[1] * m[2],tolerance) &&
		fiszero(m[0] * m[2],tolerance);
}

rrbool rrMat3_IsOrthonormal(const rrMat3 & m,const float tolerance /*= RR_EPSILON*/)
{
 	const float det = rrMat3_GetDeterminant(&m);
	return fisone(det,tolerance) && 
		rrMat3_IsOrthogonal(m,tolerance) && 
		fisone(LengthSqr(m[0]), tolerance/*tolerance*/) && 
		fisone(LengthSqr(m[1]), tolerance/*tolerance*/) &&
		fisone(LengthSqr(m[2]), tolerance/*tolerance*/);
}

//! fuzzy equality test
rrbool rrMat3_Equals(const rrMat3 &a,const rrMat3 &b,const float tolerance /*= EPSILON*/)
{
	return 
		Equals(a[0],b[0],tolerance) && 
		Equals(a[1],b[1],tolerance) && 
		Equals(a[2],b[2],tolerance);
}

//=====================================================================================

void rrMat3_SetProduct(rrMat3 * pm,const rrMat3 &m1,const rrMat3 &m2)
{
	// rrMat3 multiplication :
	// multiply to temps so pm can == m1 or m2

	rrVec3f mx =	
			m2[0] * m1[0].x +
			m2[1] * m1[0].y +
			m2[2] * m1[0].z;

	rrVec3f my =
			m2[0] * m1[1].x +
			m2[1] * m1[1].y +
			m2[2] * m1[1].z;

	rrVec3f mz =
			m2[0] * m1[2].x +
			m2[1] * m1[2].y +
			m2[2] * m1[2].z;

	pm->RowX() = mx;
	pm->RowY() = my;
	pm->RowZ() = mz;
}

void rrMat3_LeftMultiply(rrMat3 * pm,const rrMat3 &m)
{
	rrMat3_SetProduct(pm,m,*pm);
}

void rrMat3_RightMultiply(rrMat3 * pm,const rrMat3 &m)
{
	rrMat3_SetProduct(pm,*pm,m);
}

//=====================================================================================

RR_NAMESPACE_END
