// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRRBITMAP_COLOR_H__
#define __RADRRBITMAP_COLOR_H__

#include "rrbase.h"
#include "rrmath.h"
#include "rrfixedfloat.h"
#include "cbradutil.h"
#include "vec128.inl"

RR_NAMESPACE_START

/***************

WARNING : by current convention rrColor4F is stored with white = 255.f
DOUBLE WARNING : NOT TRUE ANYMORE
	we now mostly use "Normalized" GPU standard white = 1.f
	but there are some exceptions (imdiff)
	be careful of context

OLD :

that's different than many equations and my old cblib code (white = 1.f)

The advantage of ColorF using white = 255 is that I can write code like :

if ( color.r < 32 )

and it works the same for int or float colors.

---------

Note that in this scheme the correct way to do conversion is :

Int->Float is :
	F = I;
	
	F in [0,255] but okay to drift to [-0.5,255.5]
		
Float->Int is :
	I = round( F );
		
*******************/


struct rrColor4F
{
	F32 r,g,b,a;
};

struct rrColor4I
{
	S32	r,g,b,a;
};


// rrColor32BGRA : color byte order is the same as D3D :
//  BGRA in memory, ARGB in a U32 word shifted (on Intel)
union rrColor32BGRA
{
	U32		dw;
	struct
	{
		U8	b,g,r,a;
	} u;
};

// rrColor32RGBA : RGBA in bytes like OpenGl and Mac : 
union rrColor32RGBA
{
	U32		dw;
	struct
	{
		U8	r,g,b,a;
	} u;
};

RR_COMPILER_ASSERT( sizeof(rrColor32BGRA) == 4 );
RR_COMPILER_ASSERT( sizeof(rrColor32RGBA) == 4 );

// BGRA and RGBA both have A as the top 8 bits
#define RR_COLOR32_A_MASK	(0xFF000000)
#define RR_COLOR32_RGB_MASK	(0x00FFFFFF)

// 565 compatible with BC1 :

// NOTE FOR ENDIAN :
// the U16 in here should not be load/stored to bits directly
// it is always treated as LITTLE ENDIAN for compatibility with DXTC
// use GET/PUT U16 macros :
//
// NOTE(fg): That won't actually work, BE vs. LE platforms normally
// switch bit packing order too...

union rrColor565Bits
{
	U16	w;
	struct
	{
		U16 b : 5;
		U16 g : 6;
		U16 r : 5;
	} u;
};

union rrColor1555Bits
{
	U16	w;
	struct
	{
		U16 a : 1;
		U16 b : 5;
		U16 g : 5;
		U16 r : 5;
	} u;
};

RR_COMPILER_ASSERT( sizeof(rrColor565Bits) == 2 );
RR_COMPILER_ASSERT( sizeof(rrColor1555Bits) == 2 );

// RGB565 unpacked to 8-bit fields in the (low->high) B, G, R field order used by
// BC1. This is the working representation in the BC1 encoder.

struct rrColorUnpacked565
{
	// Not using unions here to avoid some annoying codegen issues.
	U32 dw;		// b | (g << 8) | (r << 8) with b, r in [0,31] and g in [0,63]; other bits clear.

	rrColorUnpacked565()													{}
	explicit rrColorUnpacked565(rrColor565Bits x) : dw(x.u.b | (x.u.g << 8) | (x.u.r << 16)) {}
	rrColorUnpacked565(U8 r, U8 g, U8 b) : dw(b | (g << 8) | (r << 16))		{ RR_ASSERT(r <= 31 && g <= 63 && b <= 31); }

	// Accessors
	U8 b() const															{ return (U8)dw; }
	U8 g() const															{ return (U8)(dw >> 8); }
	U8 r() const															{ return (U8)(dw >> 16); }

	static rrColorUnpacked565 unpack(const rrColor565Bits x)				{ return rrColorUnpacked565(x); }
	rrColor565Bits pack() const												{ rrColor565Bits x; x.u.b = b(); x.u.g = g(); x.u.r = r(); return x; }

	static inline rrColorUnpacked565 quantize(rrColor32BGRA x);
	inline rrColor32BGRA dequantize() const;

	// Compares the same way the underlying 565 color does
	bool operator ==(const rrColorUnpacked565 x) const						{ return dw == x.dw; }
	bool operator !=(const rrColorUnpacked565 x) const						{ return dw != x.dw; }
	bool operator < (const rrColorUnpacked565 x) const						{ return dw <  x.dw; }
	bool operator <=(const rrColorUnpacked565 x) const						{ return dw <= x.dw; }
	bool operator > (const rrColorUnpacked565 x) const						{ return dw >  x.dw; }
	bool operator >=(const rrColorUnpacked565 x) const						{ return dw >= x.dw; }
};

//-------------------------------------------------------------

RADINLINE bool rrColor32BGRA_EqualsRGBA(const rrColor32BGRA & c1,const rrColor32BGRA & c2)
{
	return c1.dw == c2.dw;
}

RADINLINE bool rrColor32BGRA_EqualsRGB(const rrColor32BGRA & c1,const rrColor32BGRA & c2)
{
	return ((c1.dw ^ c2.dw) & RR_COLOR32_RGB_MASK) == 0;
}

RADINLINE bool rrColor32RGBA_EqualsRGBA(const rrColor32RGBA & c1,const rrColor32RGBA & c2)
{
	return c1.dw == c2.dw;
}

RADINLINE bool rrColor32RGBA_EqualsRGB(const rrColor32RGBA & c1,const rrColor32RGBA & c2)
{
	return ((c1.dw ^ c2.dw) & RR_COLOR32_RGB_MASK) == 0;
}

RADINLINE U32 rrColor32BGRA_DeltaSqrRGB(const rrColor32BGRA & c1,const rrColor32BGRA & c2)
{
	return S32_Square((S32)c1.u.r - c2.u.r) + S32_Square((S32)c1.u.g - c2.u.g) + S32_Square((S32)c1.u.b - c2.u.b);
}

RADINLINE U32 rrColor32BGRA_DeltaSqrRGBA(const rrColor32BGRA & c1,const rrColor32BGRA & c2)
{
	return S32_Square((S32)c1.u.r - c2.u.r) + S32_Square((S32)c1.u.g - c2.u.g) + S32_Square((S32)c1.u.b - c2.u.b) + S32_Square((S32)c1.u.a - c2.u.a);
}

RADINLINE U32 rrColor32BGRA_DeltaSADRGB(const rrColor32BGRA & c1,const rrColor32BGRA & c2)
{
	return RR_ABS((S32)c1.u.r - c2.u.r) + RR_ABS((S32)c1.u.g - c2.u.g) + RR_ABS((S32)c1.u.b - c2.u.b);
}

RADINLINE U32 rrColor32BGRA_DeltaSADRGBA(const rrColor32BGRA & c1,const rrColor32BGRA & c2)
{
	return RR_ABS((S32)c1.u.r - c2.u.r) + RR_ABS((S32)c1.u.g - c2.u.g) + RR_ABS((S32)c1.u.b - c2.u.b) + RR_ABS((S32)c1.u.a - c2.u.a);
}

RADINLINE U32 rrColor4I_DeltaSqrRGB(const rrColor4I & c1,const rrColor4I & c2)
{
	// warning : could overflow U32
	return S32_Square(c1.r - c2.r) + S32_Square(c1.g - c2.g) + S32_Square(c1.b - c2.b);
}

RADINLINE U32 rrColor4I_DeltaSqrRGBA(const rrColor4I & c1,const rrColor4I & c2)
{
	// warning : could overflow U32
	return S32_Square(c1.r - c2.r) + S32_Square(c1.g - c2.g) + S32_Square(c1.b - c2.b) + S32_Square(c1.a - c2.a);
}

RADINLINE F32 rrColor4F_DeltaSqrRGB(const rrColor4F & c1,const rrColor4F & c2)
{
#ifdef __RADSSE2__
	VecF32x4 c1v = VecF32x4::loadu(&c1.r);
	VecF32x4 c2v = VecF32x4::loadu(&c2.r);
	VecF32x4 diff = c1v - c2v;
	VecF32x4 diff_masked = diff & _mm_castsi128_ps(_mm_setr_epi32(-1,-1,-1,0)); // mask off A
	VecF32x4 diff_sq = diff_masked * diff_masked;

	// NOTE: not using sum_across here to have a defined order of summation matching
	// scalar variant
	VecF32x4 sum = diff_sq + diff_sq.yxwz(); // sum with 1 over
	sum = sum + sum.zwxy(); // sum with 2 over

	return sum.scalar_x();
#else
	// NOTE: note parenthesization (to match vector ver)
	return (F32_Square(c1.r - c2.r) + F32_Square(c1.g - c2.g)) + F32_Square(c1.b - c2.b);
#endif
}

RADINLINE F32 rrColor4F_DeltaSqrRGBA(const rrColor4F & c1,const rrColor4F & c2)
{
#ifdef __RADSSE2__
	VecF32x4 c1v = VecF32x4::loadu(&c1.r);
	VecF32x4 c2v = VecF32x4::loadu(&c2.r);
	VecF32x4 diff = c1v - c2v;
	VecF32x4 diff_sq = diff * diff;

	// NOTE: not using sum_across here to have a defined order of summation matching
	// scalar variant
	VecF32x4 sum = diff_sq + diff_sq.yxwz(); // sum with 1 over
	sum = sum + sum.zwxy(); // sum with 2 over

	return sum.scalar_x();
#else
	// NOTE: note parenthesization (to match vector ver)
	return (F32_Square(c1.r - c2.r) + F32_Square(c1.g - c2.g)) + (F32_Square(c1.b - c2.b) + F32_Square(c1.a - c2.a));
#endif
}

RADINLINE F32 rrColor4F_DeltaL1RGB(const rrColor4F & c1,const rrColor4F & c2)
{
	return RR_ABS(c1.r - c2.r) + RR_ABS(c1.g - c2.g) + RR_ABS(c1.b - c2.b);
}

RADINLINE F32 rrColor4F_DeltaL1RGBA(const rrColor4F & c1,const rrColor4F & c2)
{
	return RR_ABS(c1.r - c2.r) + RR_ABS(c1.g - c2.g) + RR_ABS(c1.b - c2.b) + RR_ABS(c1.a - c2.a);
}

//-------------------------------------------------------------

// Mul8bit takes a,b in [0,255] and makes 255 act like 1.0
//	and does proper rounding , it's equivalent to :
//	rrMul8Bit = froundint( a*b/255.f );
RADINLINE int rrMul8Bit(int a,int b)
{
	int t = a*b;
	
	RR_ASSERT( t>= 0 );
	/*
	if ( t < 0 )
	{
		t = -t;
		t += 128;
		return - ((t + (t >> 8)) >> 8);
	}
	else
	*/
	{
		t += 128;
		return (t + (t >> 8)) >> 8;
	}
}

// f in [0,255] lerps from v0 to v1
RADINLINE int rrLerp8bit(int v0,int v1, int f)
{
	return v0 + rrMul8Bit(v1 - v0,f);
}

RADINLINE rrColor32BGRA rrLerpColor8bit(rrColor32BGRA c0,rrColor32BGRA c1,int f)
{
	rrColor32BGRA ret;
	ret.u.r = (U8) rrLerp8bit(c0.u.r,c1.u.r,f);
	ret.u.g = (U8) rrLerp8bit(c0.u.g,c1.u.g,f);
	ret.u.b = (U8) rrLerp8bit(c0.u.b,c1.u.b,f);
	ret.u.a = 0xFF;
	
	return ret;
}

//-------------------------------------------------------------

RADINLINE rrColor32BGRA rrColor565Bits_UnQuantize(rrColor565Bits c)
{
	rrColor32BGRA dw;
	// bit replication :
	dw.u.r =(U8) ( (c.u.r << 3) | (c.u.r>>2) ); // or *33 >> 2
	dw.u.g =(U8) ( (c.u.g << 2) | (c.u.g>>4) ); // *65 >> 4
	dw.u.b =(U8) ( (c.u.b << 3) | (c.u.b>>2) );
	dw.u.a = 0xFF;
	
	return dw;
}

RADINLINE rrColor32BGRA rrColor1555Bits_UnQuantize(rrColor1555Bits c)
{
	rrColor32BGRA dw;
	// bit replication :
	dw.u.r =(U8) ( (c.u.r << 3) | (c.u.r>>2) );
	dw.u.g =(U8) ( (c.u.g << 3) | (c.u.g>>2) );
	dw.u.b =(U8) ( (c.u.b << 3) | (c.u.b>>2) );
	//dw.u.a = c.u.a * 0xFF; // 0 or 0xFF
	dw.u.a =(U8) ( (c.u.a << 8) - c.u.a );
	
	return dw;
}

RADINLINE rrColor565Bits rrColor565Bits_Quantize(rrColor32BGRA dw)
{
	rrColor565Bits c;
	
	// proper quantizer for bit replication dequantizer :
	c.u.r = rrMul8Bit(dw.u.r,31);
	c.u.g = rrMul8Bit(dw.u.g,63);
	c.u.b = rrMul8Bit(dw.u.b,31);
	
	return c;
}

inline rrColorUnpacked565 rrColorUnpacked565::quantize(rrColor32BGRA x)
{
#ifdef __RADLITTLEENDIAN__
	rrColorUnpacked565 ret;

	// This is the same as the below, but we can do B and R at the same time:
	// (two Mul8Bit at once)
	U32 t = (x.dw & 0xff00ff) * 31 + 0x800080;
	t = t + ((t >> 8) & 0xff00ff);
	ret.dw = (t >> 8) & 0xff00ff;

	// G on its own
	ret.dw += rrMul8Bit(x.u.g,63)<<8;
	return ret;
#else
	// proper quantizer for bit replication dequantizer :
	return rrColorUnpacked565(
		(U8)rrMul8Bit(x.u.r,31),
		(U8)rrMul8Bit(x.u.g,63),
		(U8)rrMul8Bit(x.u.b,31)
	);
#endif
}

inline rrColor32BGRA rrColorUnpacked565::dequantize() const
{
	RR_ASSERT( r() < 32 );
	RR_ASSERT( g() < 64 );
	RR_ASSERT( b() < 32 );

	rrColor32BGRA ret;

#ifdef __RADLITTLEENDIAN__
	// expand blue and red
	U32 br = (((dw & 0x1f001f) * 33) >> 2) & 0xff00ff;

	// expand green (keeping it in place)
	U32 g = ((dw & 0x3f00) << 2) | ((dw & 0x3000) >> 4);

	ret.dw = br | g | 0xff000000u; // also sets alpha=255
#else
	ret.u.b = (b() * 33) >> 2;
	ret.u.g = (g() * 65) >> 4;
	ret.u.r = (r() * 33) >> 2;
	ret.u.a = 255;
#endif
	return ret;
}

RADINLINE rrColor1555Bits rrColor1555Bits_Quantize(rrColor32BGRA dw)
{
	rrColor1555Bits c;
	
	c.u.a = (dw.u.a>>7);
	c.u.r = rrMul8Bit(dw.u.r,31);
	c.u.g = rrMul8Bit(dw.u.g,31);
	c.u.b = rrMul8Bit(dw.u.b,31);
	
	return c;
}

//-------------------------------------------------------------

RADINLINE void rrColor4_I_to_F_Linear(rrColor4F * to, const rrColor4I * from)
{
	to->r =	(F32) from->r;
	to->g =	(F32) from->g;
	to->b =	(F32) from->b;
	to->a =	(F32) from->a;
}

RADINLINE void rrColor4_F_to_I_Linear(rrColor4I * to, const rrColor4F * from)
{
	to->r =	rr_froundint( from->r );
	to->g =	rr_froundint( from->g );
	to->b =	rr_froundint( from->b );
	to->a =	rr_froundint( from->a );
}

//-------------------------------------------------------------

RADINLINE const rrColor32BGRA rrColor32BGRA_Make_Clamp( S32 r, S32 g, S32 b, S32 a = 255 )
{
	rrColor32BGRA ret;
	ret.u.r = RR_CLAMP_U8( r );
	ret.u.g = RR_CLAMP_U8( g );
	ret.u.b = RR_CLAMP_U8( b );
	ret.u.a = RR_CLAMP_U8( a );
	return ret;
}

RADINLINE const rrColor32BGRA rrColor32BGRA_Make_NoClamp( S32 r, S32 g, S32 b, S32 a = 255 )
{
	rrColor32BGRA ret;
	ret.u.r = (U8)( r );
	ret.u.g = (U8)( g );
	ret.u.b = (U8)( b );
	ret.u.a = (U8)( a );
	return ret;
}

RADINLINE void rrColor32BGRA_Set(rrColor32BGRA * to, S32 r, S32 g, S32 b, S32 a = 255 )
{
	to->u.r = RR_CLAMP_U8( r );
	to->u.g = RR_CLAMP_U8( g );
	to->u.b = RR_CLAMP_U8( b );
	to->u.a = RR_CLAMP_U8( a );
}

RADINLINE void rrColor32BGRA_SetColorI(rrColor32BGRA * to, const rrColor4I * color )
{
	to->u.r = RR_CLAMP_U8( color->r );
	to->u.g = RR_CLAMP_U8( color->g );
	to->u.b = RR_CLAMP_U8( color->b );
	to->u.a = RR_CLAMP_U8( color->a );
}

RADINLINE void rrColor32BGRA_SetColorF(rrColor32BGRA * to, const rrColor4F * from )
{
	int r =	rr_froundint_positive( from->r );
	int g =	rr_froundint_positive( from->g );
	int b =	rr_froundint_positive( from->b );
	int a =	rr_froundint_positive( from->a );
	to->u.r = RR_CLAMP_U8( r );
	to->u.g = RR_CLAMP_U8( g );
	to->u.b = RR_CLAMP_U8( b );
	to->u.a = RR_CLAMP_U8( a );
}

RADINLINE void rrColor4I_Set(rrColor4I * to, S32 r, S32 g, S32 b, S32 a = 255 )
{
	to->r = r;
	to->g = g;
	to->b = b;
	to->a = a;
}

RADINLINE void rrColor4I_SetF(rrColor4I * to, F32 r, F32 g, F32 b, F32 a = 255.f )
{
	to->r = rr_froundint( r );
	to->g = rr_froundint( g );
	to->b = rr_froundint( b );
	to->a = rr_froundint( a );
}

RADINLINE void rrColor4I_Add(rrColor4I * to, const rrColor32BGRA & c)
{
	to->r += c.u.r;
	to->g += c.u.g;
	to->b += c.u.b;
	to->a += c.u.a;
}

//-------------------------------------------------------------

/**

rgbe is in bytes like [r,g,b,e]

Greg Ward's RGBE HDR

**/

void rrColor4F_From_RGBE_8888(rrColor4F * to, const U8 * from);
void rrColor4F_To_RGBE_8888(U8 * to, const rrColor4F * from);

/**

D3D has a 9995 RGBE :

**/

void rrColor4F_From_RGBE_9995(rrColor4F * to, const U32 from);
void rrColor4F_To_RGBE_9995(U32 * to, const rrColor4F * from);

//---------------------------------------------------------------------

// SRGB byte <-> linear UNIT float
//	fast conversions

// follows ColorITOF convention ; byte [0,255] -> float [0.0,1.0]

extern const float c_srgb_byte_to_linear_float[256];

static inline float srgb_byte_to_linear_unit_float(U8 b)
{
	return c_srgb_byte_to_linear_float[b];
}

// linear_float_to_srgb_byte also does clamp-to-unit on input f
U8 linear_unit_float_to_srgb_byte(float f);

void rrColor_LinearToSRGB(const rrColor4F* src, rrColor32BGRA* dst, int count);
void rrColor_SRGBToLinear(const rrColor32RGBA* src, rrColor4F* dst, int count);

//======================================================

RR_NAMESPACE_END

#endif // __RADRRBITMAP_COLOR_H__
