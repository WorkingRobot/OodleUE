// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRRBITMAP_FIXEDFLOAT_H__
#define __RADRRBITMAP_FIXEDFLOAT_H__


#include "rrbase.h"
#include "rrmath.h"
#include "cbradutil.h"

RR_NAMESPACE_START
//RADDEFSTART

/*********

CB : class design notes :

this could done much more elegantly with Templates - all these functions could easily be generic and
	work on floats of arbitrary sizes

	to do that you would want GetParts/SetParts/GetBits to be generic overloaded functions

instead I have a compromise where everything is named the same so you can make the FP24 by just
  copy-pasting from FP16 and doing find-replace

***********/

// values over RR_FP16_MAX cannot be represented in FP16
// they may become "Inf"
#define RR_FP16_MAX	(65504)

// smallest > 0 half float (subnormal) :
#define RR_FP16_TINIEST_SUBNORMAL	(0.000000059604645)
#define RR_FP16_TINIEST_NORMAL		(0.00006103515625)

// http://www.mrob.com/pub/math/floatformats.html

// http://en.wikipedia.org/wiki/Half_precision
struct rrFP16
{
	enum { c_size_bits = 16 };
	enum { c_mantissa_bits = 10 };
	enum { c_exponent_bits = 5 };
	enum { c_exponent_bias = 15 };
	
	U16 bits;
};

// http://en.wikipedia.org/wiki/Single_precision
struct rrFP32
{
	enum { c_size_bits = 32 };
	enum { c_mantissa_bits = 23 };
	enum { c_exponent_bits = 8 };
	enum { c_exponent_bias = 127 };
	
	U32 bits;
};

// Half <-> Float correct from OpenEXR :
// http://www.koders.com/cpp/fidF0DD0510FAAED03817A956D251787609BEB5989E.aspx

/*

Some SSE Half <->Float :

http://ompf.org/forum/viewtopic.php?f=11&t=836
http://ompf.org/forum/viewtopic.php?p=5916&sid=286260f501881f2429c87f3f31f639d7#p5916
http://www.devmaster.net/forums/showthread.php?t=10924

*/

//-----------------------------------------------------------------

RADINLINE U16 rrFP16_GetBits(const rrFP16 * fp)
{
	return fp->bits;
}

RADINLINE U32 rrFP32_GetBits(const rrFP32 * fp)
{
	return fp->bits;
}

RADINLINE U32 rrF32_GetBits(const F32 * fp)
{
	rrFloatAnd32 f32;
	f32.f = *fp;
	return f32.i;
}

//-----------------------------------------------------------------

RADINLINE void rrFP16_SetBits(rrFP16 * fp,U32 bits)
{
	fp->bits = (U16)bits;;
}

RADINLINE void rrFP32_SetBits(rrFP32 * fp,U32 bits)
{
	fp->bits = bits;
}

RADINLINE void rrF32_SetBits(F32 * fp,U32 bits)
{
	rrFloatAnd32 f32;
	f32.i = bits;
	*fp = f32.f;
}

//-----------------------------------------------------------------

RADINLINE void rrFP16_GetParts(const rrFP16 * fp,U32 * mantissa,U32 *exponent,U32 * sign)
{
	U32 fpbits = rrFP16_GetBits(fp);
	*mantissa = fpbits & RR_BIT_MASK_U32( rrFP16::c_mantissa_bits );
	*exponent = ( (fpbits >> rrFP16::c_mantissa_bits) & RR_BIT_MASK_U32( rrFP16::c_exponent_bits ) );
	*sign = fpbits >> (rrFP16::c_size_bits - 1);
}

RADINLINE void rrFP32_GetParts(const rrFP32 * fp,U32 * mantissa,U32 *exponent,U32 * sign)
{
	U32 fpbits = rrFP32_GetBits(fp);
	*mantissa = fpbits & RR_BIT_MASK_U32( rrFP32::c_mantissa_bits );
	*exponent = ( (fpbits >> rrFP32::c_mantissa_bits) & RR_BIT_MASK_U32( rrFP32::c_exponent_bits ) );
	*sign = fpbits >> (rrFP32::c_size_bits - 1);
}

//-----------------------------------------------------------------

RADINLINE void rrFP16_SetParts(rrFP16 * fp,U32 mantissa,U32 exponent,U32 sign)
{
	RR_ASSERT( sign <= 1 );
	RR_ASSERT( mantissa <= RR_BIT_MASK_U32( rrFP16::c_mantissa_bits ) );
	RR_ASSERT( exponent <= RR_BIT_MASK_U32( rrFP16::c_exponent_bits ) );
	
	U32 fpbits = (sign << (rrFP16::c_size_bits -1) ) | (exponent << (rrFP16::c_mantissa_bits)) | mantissa;
	rrFP16_SetBits(fp,fpbits);
}

RADINLINE void rrFP32_SetParts(rrFP32 * fp,U32 mantissa,U32 exponent,U32 sign)
{
	RR_ASSERT( sign <= 1 );
	RR_ASSERT( mantissa <= RR_BIT_MASK_U32( rrFP32::c_mantissa_bits ) );
	RR_ASSERT( exponent <= RR_BIT_MASK_U32( rrFP32::c_exponent_bits ) );
	
	U32 fpbits = (sign << (rrFP32::c_size_bits -1) ) | (exponent << (rrFP32::c_mantissa_bits)) | mantissa;
	rrFP32_SetBits(fp,fpbits);
}

//-----------------------------------------------------------------

/**

// from ryg :

static FP32 half_to_float_fast5(FP16 h)
{
    static const FP32 magic = { (127 + (127 - 15)) << 23 };
    static const FP32 was_infnan = { (127 + 16) << 23 };
    FP32 o;

    o.u = (h.u & 0x7fff) << 13;     // exponent/mantissa bits
    o.f *= magic.f;                 // exponent adjust
    if (o.f >= was_infnan.f)        // make sure Inf/NaN survive
        o.u |= 255 << 23;
    o.u |= (h.u & 0x8000) << 16;    // sign bit
    return o;
}

static const FP32 magic = { (127 + (127 - 15)) << 23 };

static FP32 half_to_float_fast5(FP16 h)
{
    FP32 o;
    o.u = (h.u & 0x7fff) << 13;     // exponent/mantissa bits
    o.f *= magic.f;                 // exponent adjust
    o.u |= (h.u & 0x8000) << 16;    // sign bit
    return o;
}

static FP32 half_to_float_fast2(FP16 h)
{
    static const FP32 magic = { 126 << 23 };
    FP32 o;

    if (h.Exponent == 0) // Zero / Denormal
    {
        o.u = magic.u + h.Mantissa;
        o.f -= magic.f;
    }
    else
    {
        o.Mantissa = h.Mantissa << 13;
        if (h.Exponent == 0x1f) // Inf/NaN
            o.Exponent = 255;
        else
            o.Exponent = 127 - 15 + h.Exponent;
    }

    o.Sign = h.Sign;
    return o;
}
static FP32 half_to_float_fast2(FP16 h)
{
    static const FP32 magic = { 126 << 23 };
    FP32 o;

    if (h.Exponent == 0) // Zero / Denormal
    {
        o.u = magic.u + h.Mantissa;
        o.f -= magic.f;
    }
    else
    {
        o.Mantissa = h.Mantissa << 13;
        o.Exponent = 127 - 15 + h.Exponent;
    }

    o.Sign = h.Sign;
    return o;
}

**/

RADINLINE F32 rrFP16_GetFloat(const rrFP16 * fp)
{
	rrFloatAnd32 f32;
	
	U32 mantissa,exponent,sign;
	rrFP16_GetParts(fp,&mantissa,&exponent,&sign);

	U32 o_exponent = 0;
	U32 o_mantissa = mantissa;

	if (exponent == 0) // Denormal or zero (will convert to normalized)
	{
		if (o_mantissa) // nothing to do if 0
		{
			// Adjust mantissa so it's normalized (and keep track of exp adjust)
			o_exponent = 127 - 15 + 1;
			do
			{
				--o_exponent;
				o_mantissa <<= 1;
			} while (o_mantissa < 0x400);
			o_mantissa -= 0x400; // hidden 1 bit
		}
	}
	else if (exponent == 0x1f) // Inf/NaN (NOTE: safe to treat both with the same code path)
	{
		o_exponent = 255;
		// quiet NaNs to match x86 VCVTPH2PS behavior exactly
		if (o_mantissa) o_mantissa |= 0x200;
	}
	else // Normalized number
		o_exponent = 127 - 15 + exponent;

	f32.i = (o_mantissa << 13) | (o_exponent << 23) | (sign << 31);
	return f32.f;
}

RADINLINE F32 rrFP32_GetFloat(const rrFP32 * fp)
{
	rrFloatAnd32 f32;
	f32.i = fp->bits;
	return f32.f;
}

//-----------------------------------------------------------------

RADINLINE void rrFP16_SetFloat(rrFP16 * fp,F32 val)
{
	rrFloatAnd32 f32;
	f32.f = val;
	
	U32 f_sign = f32.i>>31;
	U32 f_exponent = (f32.i>>23) & 0xff;
	U32 f_mantissa = f32.i & 0x007fffff;

	U32 o_exp_mant = 0;

	if (f_exponent == 0) // Signed zero/denormal (which will underflow)
		o_exp_mant = 0;
	else if (f_exponent == 255) // Inf or NaN (all exponent bits set)
	{
		// NOTE: preserve NaN payload bits to match x86 VCVTPS2PH behavior
		o_exp_mant = (31 << 10) | (f_mantissa ? 0x200 : 0) | (f_mantissa >> 13); // NaN->qNaN and Inf->Inf
	}
	else // Normalized number
	{
		// Exponent unbias the single, then bias the halfp
		int newexp = f_exponent - 127 + 15;
		if (newexp >= 31) // Overflow, return signed infinity
			o_exp_mant = 31 << 10;
		else if (newexp <= 0) // Underflow
		{
			int shift = 14 - newexp;
			if (shift <= 24) // Mantissa might be non-zero
			{
				int mant = f_mantissa | 0x800000; // Hidden 1 bit
				o_exp_mant = mant >> shift;

				U32 mask = 1u << shift;
				U32 lowmant = mant & (mask - 1);

				if (lowmant + (o_exp_mant & 1) > (mask >> 1)) // if above halfway point or right on halfway point with an odd unrounded result
				{
					// Round; if overflow, increment exponent
					++o_exp_mant;
				}
			}
		}
		else
		{
			o_exp_mant = (newexp << 10) | (f_mantissa >> 13);
			if ((f_mantissa & 0x1fff) + (o_exp_mant & 1) > 0x1000) // if above halfway point or right on halfway point with an odd unrounded result
			{
				// Round; if overflow, increment exponent
				++o_exp_mant;
			}
		}
	}
	rrFP16_SetBits(fp, o_exp_mant | (f_sign << 15));
}

RADINLINE void rrFP32_SetFloat(rrFP32 * fp,F32 val)
{
	rrFloatAnd32 f32;
	f32.f = val;
	fp->bits = f32.i;
}

//=========================================================

RADINLINE bool rrFP16_IsZero(rrFP16 * fp)
{
	// stupid fucking negative zero :
	U32 bits = rrFP16_GetBits(fp);
	return ( (bits & RR_BIT_MASK_U32( rrFP16::c_size_bits-1 ) ) == 0 );
}

RADINLINE bool rrFP16_Equal(rrFP16 * lhs,rrFP16 *rhs)
{
	U32 b1 = rrFP16_GetBits(lhs);
	U32 b2 = rrFP16_GetBits(rhs);
	
	if ( b1 == b2 ) return true;
	else if ( rrFP16_IsZero(lhs) && rrFP16_IsZero(rhs) ) return true;
	else return false;
}

RADINLINE bool operator == (rrFP16 & lhs, rrFP16 & rhs)
{
	return rrFP16_Equal(&lhs,&rhs);
}

//=========================================================

//RADDEFEND
RR_NAMESPACE_END

#endif // __RADRRBITMAP_FIXEDFLOAT_H__
