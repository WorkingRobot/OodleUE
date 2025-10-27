// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRRBITMAP_COLOR_VECC_H__
#define __RADRRBITMAP_COLOR_VECC_H__

#include "rrbase.h"
#include "rrcolor.h"
#include "rrvecc.h"
#include <math.h>

RR_NAMESPACE_START

inline const rrColor32BGRA U32_to_Color(U32 w)
{
	rrColor32BGRA u;
	u.dw = w;
	return u;
}

inline const rrColor32BGRA Average(const rrColor32BGRA &c1,const rrColor32BGRA & c2)
{
	rrColor32BGRA ret;
	ret.u.r = (c1.u.r + c2.u.r)>>1;
	ret.u.g = (c1.u.g + c2.u.g)>>1;
	ret.u.b = (c1.u.b + c2.u.b)>>1;
	ret.u.a = 0xFF;
	return ret;
}

inline const rrVec3i ColorToVec3i(rrColor32BGRA color)
{
	return rrVec3i(color.u.b,color.u.g,color.u.r);
}

inline const rrVec3f ColorToVec3f(rrColor32BGRA color)
{
	return rrVec3f(color.u.b,color.u.g,color.u.r);
}

inline const rrColor32BGRA Vec3iToColor(const rrVec3i & vec)
{
	rrColor32BGRA c;
	c.u.b = (U8) vec.x;
	c.u.g = (U8) vec.y;
	c.u.r = (U8) vec.z;
	c.u.a = 0xFF;
	return c;
}
inline const rrColor32BGRA Vec3iToColorClamp(const rrVec3i & vec)
{
	rrColor32BGRA c;
	c.u.b = RR_CLAMP_U8( vec.x );
	c.u.g = RR_CLAMP_U8( vec.y );
	c.u.r = RR_CLAMP_U8( vec.z );
	c.u.a = 0xFF;
	return c;
}

inline const rrVec3f Vec3i_to_Vec3f(const rrVec3i & vi)
{
	return rrVec3f((float)vi.x,(float)vi.y,(float)vi.z);
}

inline const rrColor32BGRA Vec3fToColor(const rrVec3f & vec)
{
	rrColor32BGRA c;
	c.u.b = U8_check( rr_froundint(vec.x) );
	c.u.g = U8_check( rr_froundint(vec.y) );
	c.u.r = U8_check( rr_froundint(vec.z) );
	c.u.a = 0xFF;
	return c;
	
}
inline const rrColor32BGRA Vec3fToColorClamp(const rrVec3f & vec)
{
	rrColor32BGRA c;
	c.u.b = RR_CLAMP_U8( rr_froundint(vec.x) );
	c.u.g = RR_CLAMP_U8( rr_froundint(vec.y) );
	c.u.r = RR_CLAMP_U8( rr_froundint(vec.z) );
	c.u.a = 0xFF;
	return c;
}

RR_NAMESPACE_END


#endif // __RADRRBITMAP_COLOR_VECC_H__
