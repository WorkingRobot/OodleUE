// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_VEC_C_H__
#define __RADRR_VEC_C_H__

#include "rrbase.h"
#include "rrmath.h"

// C++ obv

#define RR_EPSILON (0.0005f) 

RR_NAMESPACE_START

/*********

rrVec-N-i/f

2d-4f int & float vectors

Class design :

try to make as much like dumb structs as possible, but with the advantages of C++

As few member functions as possible

Use non-member overloaded functions with the same name.



***********/

//-----------------------------------------------------

class rrVec2f
{
public:
	F32 x,y;
	
	rrVec2f() { }
	rrVec2f(F32 _x,F32 _y) : x(_x), y(_y) { }

	void operator *= (const F32 rhs) { x *= rhs; y *= rhs; }
	void operator += (const rrVec2f & rhs) { x += rhs.x; y += rhs.y; }
	void operator -= (const rrVec2f & rhs) { x -= rhs.x; y -= rhs.y; }
	
	F32   operator [](const int c) const	{ RR_ASSERT( c >= 0 && c < 2 );  return (&x)[c]; }
	F32 & operator [](const int c)			{ RR_ASSERT( c >= 0 && c < 2 );  return (&x)[c]; }
};

class rrVec2i
{
public:
	S32 x,y;
	
	rrVec2i() { }
	rrVec2i(S32 _x,S32 _y) : x(_x), y(_y) { }
	
	
	void operator *= (const S32 rhs) { x *= rhs; y *= rhs; }
	void operator += (const rrVec2i & rhs) { x += rhs.x; y += rhs.y; }
	void operator -= (const rrVec2i & rhs) { x -= rhs.x; y -= rhs.y; }
	
	S32   operator [](const int c) const	{ RR_ASSERT( c >= 0 && c < 2 );  return (&x)[c]; }
	S32 & operator [](const int c)			{ RR_ASSERT( c >= 0 && c < 2 );  return (&x)[c]; }
};

class rrVec3f
{
public:
	F32 x,y,z;
	
	rrVec3f() { }
	rrVec3f(F32 _x,F32 _y,F32 _z) : x(_x), y(_y), z(_z) { }

	void operator *= (const F32 rhs) { x *= rhs; y *= rhs; z *= rhs; }
	void operator += (const rrVec3f & rhs) { x += rhs.x; y += rhs.y; z += rhs.z; }
	void operator -= (const rrVec3f & rhs) { x -= rhs.x; y -= rhs.y; z -= rhs.z; }
	
	F32   operator [](const int c) const	{ RR_ASSERT( c >= 0 && c < 3 );  return (&x)[c]; }
	F32 & operator [](const int c)			{ RR_ASSERT( c >= 0 && c < 3 );  return (&x)[c]; }
};

class rrVec3i
{
public:
	S32 x,y,z;
	
	rrVec3i() { }
	rrVec3i(S32 _x,S32 _y,S32 _z) : x(_x), y(_y), z(_z) { }
	
	
	void operator *= (const S32 rhs) { x *= rhs; y *= rhs; z *= rhs; }
	void operator += (const rrVec3i & rhs) { x += rhs.x; y += rhs.y; z += rhs.z; }
	void operator -= (const rrVec3i & rhs) { x -= rhs.x; y -= rhs.y; z -= rhs.z; }
	
	S32   operator [](const int c) const	{ RR_ASSERT( c >= 0 && c < 3 );  return (&x)[c]; }
	S32 & operator [](const int c)			{ RR_ASSERT( c >= 0 && c < 3 );  return (&x)[c]; }
};

class rrVec4f
{
public:
	F32 x,y,z,w;
	
	rrVec4f() { }
	rrVec4f(F32 _x,F32 _y,F32 _z,F32 _w) : x(_x), y(_y), z(_z), w(_w) { }
	
	void operator *= (const F32 rhs) { x *= rhs; y *= rhs; z *= rhs; w *= rhs; }
	void operator += (const rrVec4f & rhs) { x += rhs.x; y += rhs.y; z += rhs.z; w += rhs.w; }
	void operator -= (const rrVec4f & rhs) { x -= rhs.x; y -= rhs.y; z -= rhs.z; w -= rhs.w; }
	
	F32   operator [](const int c) const	{ RR_ASSERT( c >= 0 && c < 4 );  return (&x)[c]; }
	F32 & operator [](const int c)			{ RR_ASSERT( c >= 0 && c < 4 );  return (&x)[c]; }
};

class rrVec4i
{
public:
	S32 x,y,z,w;
	
	rrVec4i() { }
	rrVec4i(S32 _x,S32 _y,S32 _z,S32 _w) : x(_x), y(_y), z(_z), w(_w) { }
	
	void operator *= (const S32 rhs) { x *= rhs; y *= rhs; z *= rhs; w *= rhs; }
	void operator += (const rrVec4i & rhs) { x += rhs.x; y += rhs.y; z += rhs.z; w += rhs.w; }
	void operator -= (const rrVec4i & rhs) { x -= rhs.x; y -= rhs.y; z -= rhs.z; w -= rhs.w; }
	
	S32   operator [](const int c) const	{ RR_ASSERT( c >= 0 && c < 4 );  return (&x)[c]; }
	S32 & operator [](const int c)			{ RR_ASSERT( c >= 0 && c < 4 );  return (&x)[c]; }
};

//-----------------------------------------------------

typedef F64	VEC_DOT_F;
typedef S32	VEC_DOT_I;

#ifndef __GNUC__
#define RR_VECC_RET_PRE RADINLINE const
#else
#define RR_VECC_RET_PRE RADINLINE
#endif

RR_VECC_RET_PRE VEC_DOT_I Square( const S32 x ) { return (VEC_DOT_I) x*x; }
RR_VECC_RET_PRE VEC_DOT_F Square( const F32 x ) { return (VEC_DOT_F) x*x; }

//-----------------------------------------------------

static RADINLINE void SetZero( F32 * p ) { *p = 0.f; }
static RADINLINE void SetZero( rrVec2f * p ) { p->x = p->y = 0.f; }
static RADINLINE void SetZero( rrVec3f * p ) { p->x = p->y = p->z = 0.f; }
static RADINLINE void SetZero( rrVec4f * p ) { p->x = p->y = p->z = p->w = 0.f; }

static RADINLINE void SetZero( S32 * p ) { *p = 0; }
static RADINLINE void SetZero( rrVec2i * p ) { p->x = p->y = 0; }
static RADINLINE void SetZero( rrVec3i * p ) { p->x = p->y = p->z = 0; }
static RADINLINE void SetZero( rrVec4i * p ) { p->x = p->y = p->z = p->w = 0; }

//-----------------------------------------------------

RR_VECC_RET_PRE rrVec2f operator + (const rrVec2f & lhs,const rrVec2f & rhs) { return rrVec2f(lhs.x + rhs.x,lhs.y + rhs.y); }

RR_VECC_RET_PRE rrVec2i operator + (const rrVec2i & lhs,const rrVec2i & rhs) { return rrVec2i(lhs.x + rhs.x,lhs.y + rhs.y); }

RR_VECC_RET_PRE rrVec3f operator + (const rrVec3f & lhs,const rrVec3f & rhs) { return rrVec3f(lhs.x + rhs.x,lhs.y + rhs.y,lhs.z + rhs.z); }

RR_VECC_RET_PRE rrVec3i operator + (const rrVec3i & lhs,const rrVec3i & rhs) { return rrVec3i(lhs.x + rhs.x,lhs.y + rhs.y,lhs.z + rhs.z); }

RR_VECC_RET_PRE rrVec4f operator + (const rrVec4f & lhs,const rrVec4f & rhs) { return rrVec4f(lhs.x + rhs.x,lhs.y + rhs.y,lhs.z + rhs.z,lhs.w + rhs.w); }

RR_VECC_RET_PRE rrVec4i operator + (const rrVec4i & lhs,const rrVec4i & rhs) { return rrVec4i(lhs.x + rhs.x,lhs.y + rhs.y,lhs.z + rhs.z,lhs.w + rhs.w); }

//-----------------------------------------------------

RR_VECC_RET_PRE rrVec2f operator - (const rrVec2f & lhs,const rrVec2f & rhs) { return rrVec2f(lhs.x - rhs.x,lhs.y - rhs.y); }

RR_VECC_RET_PRE rrVec2i operator - (const rrVec2i & lhs,const rrVec2i & rhs) { return rrVec2i(lhs.x - rhs.x,lhs.y - rhs.y); }

RR_VECC_RET_PRE rrVec3f operator - (const rrVec3f & lhs,const rrVec3f & rhs) { return rrVec3f(lhs.x - rhs.x,lhs.y - rhs.y,lhs.z - rhs.z); }

RR_VECC_RET_PRE rrVec3i operator - (const rrVec3i & lhs,const rrVec3i & rhs) { return rrVec3i(lhs.x - rhs.x,lhs.y - rhs.y,lhs.z - rhs.z); }

RR_VECC_RET_PRE rrVec4f operator - (const rrVec4f & lhs,const rrVec4f & rhs) { return rrVec4f(lhs.x - rhs.x,lhs.y - rhs.y,lhs.z - rhs.z,lhs.w - rhs.w); }

RR_VECC_RET_PRE rrVec4i operator - (const rrVec4i & lhs,const rrVec4i & rhs) { return rrVec4i(lhs.x - rhs.x,lhs.y - rhs.y,lhs.z - rhs.z,lhs.w - rhs.w); }

//-----------------------------------------------------

RR_VECC_RET_PRE VEC_DOT_F operator * (const rrVec2f & lhs , const rrVec2f & rhs) { return (VEC_DOT_F)lhs.x * rhs.x + lhs.y * rhs.y; }

RR_VECC_RET_PRE VEC_DOT_I operator * (const rrVec2i & lhs , const rrVec2i & rhs) { return (VEC_DOT_I)lhs.x * rhs.x + lhs.y * rhs.y; }

RR_VECC_RET_PRE VEC_DOT_F operator * (const rrVec3f & lhs , const rrVec3f & rhs) { return (VEC_DOT_F)lhs.x * rhs.x + lhs.y * rhs.y + lhs.z * rhs.z; }

RR_VECC_RET_PRE VEC_DOT_I operator * (const rrVec3i & lhs , const rrVec3i & rhs) { return (VEC_DOT_I)lhs.x * rhs.x + lhs.y * rhs.y + lhs.z * rhs.z; }

RR_VECC_RET_PRE VEC_DOT_F operator * (const rrVec4f & lhs , const rrVec4f & rhs) { return (VEC_DOT_F)lhs.x * rhs.x + lhs.y * rhs.y + lhs.z * rhs.z + lhs.w * rhs.w; }

RR_VECC_RET_PRE VEC_DOT_I operator * (const rrVec4i & lhs , const rrVec4i & rhs) { return (VEC_DOT_I)lhs.x * rhs.x + lhs.y * rhs.y + lhs.z * rhs.z + lhs.w * rhs.w; }

//-----------------------------------------------------

RR_VECC_RET_PRE rrVec2f operator * (const rrVec2f & lhs,const F32 rhs) { return rrVec2f(lhs.x * rhs,lhs.y * rhs); }

RR_VECC_RET_PRE rrVec2i operator * (const rrVec2i & lhs,const S32 rhs) { return rrVec2i(lhs.x * rhs,lhs.y * rhs); }

RR_VECC_RET_PRE rrVec3f operator * (const rrVec3f & lhs,const F32 rhs) { return rrVec3f(lhs.x * rhs,lhs.y * rhs,lhs.z * rhs); }

RR_VECC_RET_PRE rrVec3i operator * (const rrVec3i & lhs,const S32 rhs) { return rrVec3i(lhs.x * rhs,lhs.y * rhs,lhs.z * rhs); }

RR_VECC_RET_PRE rrVec4f operator * (const rrVec4f & lhs,const F32 rhs) { return rrVec4f(lhs.x * rhs,lhs.y * rhs,lhs.z * rhs,lhs.w * rhs); }

RR_VECC_RET_PRE rrVec4i operator * (const rrVec4i & lhs,const S32 rhs) { return rrVec4i(lhs.x * rhs,lhs.y * rhs,lhs.z * rhs,lhs.w * rhs); }

RR_VECC_RET_PRE rrVec2f operator * (const F32 rhs,const rrVec2f & lhs) { return rrVec2f(lhs.x * rhs,lhs.y * rhs); }

RR_VECC_RET_PRE rrVec2i operator * (const S32 rhs,const rrVec2i & lhs) { return rrVec2i(lhs.x * rhs,lhs.y * rhs); }

RR_VECC_RET_PRE rrVec3f operator * (const F32 rhs,const rrVec3f & lhs) { return rrVec3f(lhs.x * rhs,lhs.y * rhs,lhs.z * rhs); }

RR_VECC_RET_PRE rrVec3i operator * (const S32 rhs,const rrVec3i & lhs) { return rrVec3i(lhs.x * rhs,lhs.y * rhs,lhs.z * rhs); }

RR_VECC_RET_PRE rrVec4f operator * (const F32 rhs,const rrVec4f & lhs) { return rrVec4f(lhs.x * rhs,lhs.y * rhs,lhs.z * rhs,lhs.w * rhs); }

RR_VECC_RET_PRE rrVec4i operator * (const S32 rhs,const rrVec4i & lhs) { return rrVec4i(lhs.x * rhs,lhs.y * rhs,lhs.z * rhs,lhs.w * rhs); }

//-----------------------------------------------------

RR_VECC_RET_PRE rrVec2f operator - (const rrVec2f & lhs) { return rrVec2f(-lhs.x,-lhs.y); }

RR_VECC_RET_PRE rrVec2i operator - (const rrVec2i & lhs) { return rrVec2i(-lhs.x,-lhs.y); }

RR_VECC_RET_PRE rrVec3f operator - (const rrVec3f & lhs) { return rrVec3f(-lhs.x,-lhs.y,-lhs.z); }

RR_VECC_RET_PRE rrVec3i operator - (const rrVec3i & lhs) { return rrVec3i(-lhs.x,-lhs.y,-lhs.z); }

RR_VECC_RET_PRE rrVec4f operator - (const rrVec4f & lhs) { return rrVec4f(-lhs.x,-lhs.y,-lhs.z,-lhs.w); }

RR_VECC_RET_PRE rrVec4i operator - (const rrVec4i & lhs) { return rrVec4i(-lhs.x,-lhs.y,-lhs.z,-lhs.w); }

//-----------------------------------------------------

RR_VECC_RET_PRE VEC_DOT_F LengthSqr (const rrVec2f & lhs ) { return ( lhs * lhs ); }

RR_VECC_RET_PRE VEC_DOT_I LengthSqr (const rrVec2i & lhs ) { return ( lhs * lhs ); }

RR_VECC_RET_PRE VEC_DOT_F LengthSqr (const rrVec3f & lhs ) { return ( lhs * lhs ); }

RR_VECC_RET_PRE VEC_DOT_I LengthSqr (const rrVec3i & lhs ) { return ( lhs * lhs ); }

RR_VECC_RET_PRE VEC_DOT_F LengthSqr (const rrVec4f & lhs ) { return ( lhs * lhs ); }

RR_VECC_RET_PRE VEC_DOT_I LengthSqr (const rrVec4i & lhs ) { return ( lhs * lhs ); }

//-----------------------------------------------------

RR_VECC_RET_PRE VEC_DOT_F DeltaSqr (F32 lhs, F32 rhs) { return Square(lhs - rhs); }

RR_VECC_RET_PRE VEC_DOT_F DeltaSqr (const rrVec2f & lhs , const rrVec2f & rhs) { return Square(lhs.x - rhs.x) + Square(lhs.y - rhs.y); }

RR_VECC_RET_PRE VEC_DOT_I DeltaSqr (const rrVec2i & lhs , const rrVec2i & rhs) { return Square(lhs.x - rhs.x) + Square(lhs.y - rhs.y); }

RR_VECC_RET_PRE VEC_DOT_F DeltaSqr (const rrVec3f & lhs , const rrVec3f & rhs) { return Square(lhs.x - rhs.x) + Square(lhs.y - rhs.y) + Square(lhs.z - rhs.z); }

RR_VECC_RET_PRE VEC_DOT_I DeltaSqr (const rrVec3i & lhs , const rrVec3i & rhs) { return Square(lhs.x - rhs.x) + Square(lhs.y - rhs.y) + Square(lhs.z - rhs.z); }

RR_VECC_RET_PRE VEC_DOT_F DeltaSqr (const rrVec4f & lhs , const rrVec4f & rhs) { return Square(lhs.x - rhs.x) + Square(lhs.y - rhs.y) + Square(lhs.z - rhs.z) + Square(lhs.w - rhs.w); }

RR_VECC_RET_PRE VEC_DOT_I DeltaSqr (const rrVec4i & lhs , const rrVec4i & rhs) { return Square(lhs.x - rhs.x) + Square(lhs.y - rhs.y) + Square(lhs.z - rhs.z) + Square(lhs.w - rhs.w); }

//-----------------------------------------------------

RR_VECC_RET_PRE rrbool Equals (const rrVec2f & lhs , const rrVec2f & rhs, const float tolerance = RR_EPSILON) { return DeltaSqr(lhs,rhs) <= tolerance; }

RR_VECC_RET_PRE rrbool Equals (const rrVec3f & lhs , const rrVec3f & rhs, const float tolerance = RR_EPSILON) { return DeltaSqr(lhs,rhs) <= tolerance; }

RR_VECC_RET_PRE rrbool Equals (const rrVec4f & lhs , const rrVec4f & rhs, const float tolerance = RR_EPSILON) { return DeltaSqr(lhs,rhs) <= tolerance; }

RR_VECC_RET_PRE rrbool Equals (const rrVec2i & lhs , const rrVec2i & rhs) { return lhs.x == rhs.x && lhs.y == rhs.y ; }

RR_VECC_RET_PRE rrbool Equals (const rrVec3i & lhs , const rrVec3i & rhs) { return lhs.x == rhs.x && lhs.y == rhs.y && lhs.z == rhs.z; }

RR_VECC_RET_PRE rrbool Equals (const rrVec4i & lhs , const rrVec4i & rhs) { return lhs.x == rhs.x && lhs.y == rhs.y && lhs.z == rhs.z && lhs.w == rhs.w; }

//-----------------------------------------------------

F32 rrRecipSqrt(F32 x); // just 1/sqrtf() , deprecated

RR_VECC_RET_PRE rrVec2f Normalize(const rrVec2f & v)
{
	VEC_DOT_F lsqr = LengthSqr(v);
	return v * rrRecipSqrt((F32)lsqr);
}

RR_VECC_RET_PRE rrVec3f Normalize(const rrVec3f & v)
{
	VEC_DOT_F lsqr = LengthSqr(v);
	return v * rrRecipSqrt((F32)lsqr);
}

RR_VECC_RET_PRE rrVec4f Normalize(const rrVec4f & v)
{
	VEC_DOT_F lsqr = LengthSqr(v);
	return v * rrRecipSqrt((F32)lsqr);
}

RR_VECC_RET_PRE bool NormalizeSafe(rrVec2f * pv)
{
	VEC_DOT_F lsqr = LengthSqr(*pv);
	if ( lsqr < RR_EPSILON ) return false;
	*pv *= rrRecipSqrt((F32)lsqr);
	return true;
}

RR_VECC_RET_PRE bool NormalizeSafe(rrVec3f * pv)
{
	VEC_DOT_F lsqr = LengthSqr(*pv);
	if ( lsqr < RR_EPSILON ) return false;
	*pv *= rrRecipSqrt((F32)lsqr);
	return true;
}

RR_VECC_RET_PRE bool NormalizeSafe(rrVec4f * pv)
{
	VEC_DOT_F lsqr = LengthSqr(*pv);
	if ( lsqr < RR_EPSILON ) return false;
	*pv *= rrRecipSqrt((F32)lsqr);
	return true;
}

//-----------------------------------------------------


// Cross in 2d is a scalar.
//	this is well-defined as the hodge-dual of the wedge-products :
//	result = *( u /\ v)
//	this is also the determinant of the Mat3 made from u and v as rows
RR_VECC_RET_PRE F32 Cross(const rrVec2f & u,const rrVec2f & v) // cross
{
	return ( u.x * v.y - u.y * v.x );
}
	
//Cross product.
RR_VECC_RET_PRE rrVec3f Cross (const rrVec3f & u,const rrVec3f & v)
{
	return rrVec3f( 
		u.y * v.z - u.z * v.y,
		u.z * v.x - u.x * v.z,
		u.x * v.y - u.y * v.x );
}

//-----------------------------------------------------

RR_NAMESPACE_END

#endif // __RADRR_VEC_C_H__

