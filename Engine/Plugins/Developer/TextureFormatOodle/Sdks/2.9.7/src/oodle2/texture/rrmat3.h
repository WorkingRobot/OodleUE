// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_MAT3_H__
#define __RADRR_MAT3_H__

#include "rrbase.h"
#include "rrmath.h"
#include "rrvecc.h"
#include "oodlecore.h"

// C++ obv

RR_NAMESPACE_START

class rrMat3
{
public:
	// rrVec3f rows[3];
	float m_data[3][3]; // [row][col]
	
	
	const rrVec3f & operator [](const int r) const	{ RR_ASSERT( r >= 0 && r <= 2 );  return *(const rrVec3f *)&(m_data[r][0]); }
	rrVec3f & operator [](const int r)			{ RR_ASSERT( r >= 0 && r <= 2 );  return *(rrVec3f *)&(m_data[r][0]); }
	
	const rrVec3f & RowX() const { return *(const rrVec3f *)&(m_data[0][0]); }
	const rrVec3f & RowY() const { return *(const rrVec3f *)&(m_data[1][0]); }
	const rrVec3f & RowZ() const { return *(const rrVec3f *)&(m_data[2][0]); }
	
	rrVec3f & RowX() { return *(rrVec3f *)&(m_data[0][0]); }
	rrVec3f & RowY() { return *(rrVec3f *)&(m_data[1][0]); }
	rrVec3f & RowZ() { return *(rrVec3f *)&(m_data[2][0]); }
};

//-----------------------------------------------------

void rrMat3_SetZero(rrMat3 * mat);
void rrMat3_SetIdentity(rrMat3 * mat);
void rrMat3_GetInverse(const rrMat3 & m,rrMat3 * pInv);
F64 rrMat3_GetDeterminant(const rrMat3 * pMat);

rrbool rrMat3_IsOrthogonal( const rrMat3 & m,const float tolerance = RR_EPSILON);
rrbool rrMat3_IsOrthonormal(const rrMat3 & m,const float tolerance = RR_EPSILON);
rrbool rrMat3_Equals(const rrMat3 &a,const rrMat3 &b,const float tolerance = RR_EPSILON);

// pm == m1 or m2 is okay
void rrMat3_SetProduct(rrMat3 * pm,const rrMat3 &m1,const rrMat3 &m2);
void rrMat3_LeftMultiply(rrMat3 * pm,const rrMat3 &m);
void rrMat3_RightMultiply(rrMat3 * pm,const rrMat3 &m);

//-----------------------------------------------------

OOINLINE const rrVec3f rrMat3_GetColumnX(const rrMat3 * pMat)
{
	return rrVec3f( pMat->m_data[0][0] , pMat->m_data[1][0] , pMat->m_data[2][0] );
}
OOINLINE const rrVec3f rrMat3_GetColumnY(const rrMat3 * pMat)
{
	return rrVec3f( pMat->m_data[0][1] , pMat->m_data[1][1] , pMat->m_data[2][1] );
}
OOINLINE const rrVec3f rrMat3_GetColumnZ(const rrMat3 * pMat)
{
	return rrVec3f( pMat->m_data[0][2] , pMat->m_data[1][2] , pMat->m_data[2][2] );
}
	
//-----------------------------------------------------

OOINLINE const rrVec3f operator * (const rrMat3 & lhs, const rrVec3f & rhs)
{
	return rrVec3f( (F32) (lhs.RowX() * rhs) , 
					(F32) (lhs.RowY() * rhs) , 
					(F32) (lhs.RowZ() * rhs) ); 
}
	

//-----------------------------------------------------

RR_NAMESPACE_END

#endif // __RADRR_MAT3_H__

