// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_MATHH__
#define __RADRR_MATHH__

#include "rrbase.h"
#include "rrbits.h"
   
RR_NAMESPACE_START

/**

CB : math helpers

for little single float & int math routines

**/

#define RR_LN2      (0.6931471805599453)
#define RR_PI       (3.141592653589793)
#define RR_LN2f     (0.6931471805599453f)
#define RR_PIf      (3.141592653589793f)

#define RR_F32_EPSILON     (1.192092896e-07F)        /* smallest such that 1.0+FLT_EPSILON != 1.0 */
#define RR_F32_MAX         (3.402823466e+38F)        /* max value */

#define RR_S16_MAX			(32767)
#define RR_S16_MIN			(-32768)
#define RR_U16_MAX			(~(U32)0)

#define RR_S32_MAX          ((S32)2147483647)
#define RR_S32_MIN          (-RR_S32_MAX-1)
#define RR_U32_MAX          (~(U32)0)

#define RR_U64_MAX			(~(U64)0)
#define RR_S64_MAX			((S64)(RR_U64_MAX>>1))
#define RR_S64_MIN			(-RR_S64_MAX-1)

#define RR_ONE_U32				((U32)1)
#define RR_ONE_S32				((S32)1)
#define RR_ONE_U64				((U64)1)
#define RR_ONE_S64				((S64)1)
#define RR_ONE_UA				((UINTa)1)
#define RR_ONE_SA				((SINTa)1)
#define RR_ONE_UR				((UINTr)1)
#define RR_ONE_SR				((SINTr)1)

#ifdef __RAD64__
RR_COMPILER_ASSERT( sizeof(SINTa) == 8 );

#define RR_UINTA_MAX	RR_U64_MAX
#define RR_SINTA_MAX	RR_S64_MAX
#define RR_SINTA_MIN	RR_S64_MIN

#else

RR_COMPILER_ASSERT( sizeof(SINTa) == 4 );

#define RR_UINTA_MAX	RR_U32_MAX
#define RR_SINTA_MAX	RR_S32_MAX
#define RR_SINTA_MIN	RR_S32_MIN

#endif

#ifdef __RAD64REGS__
RR_COMPILER_ASSERT( sizeof(SINTr) == 8 );

#define RR_UINTR_MAX	RR_U64_MAX
#define RR_SINTR_MAX	RR_S64_MAX
#define RR_SINTR_MIN	RR_S64_MIN

#else

RR_COMPILER_ASSERT( sizeof(SINTr) == 4 );

#define RR_UINTR_MAX	RR_U32_MAX
#define RR_SINTR_MAX	RR_S32_MAX
#define RR_SINTR_MIN	RR_S32_MIN

#endif

F64 rrlog2_F64( F64 X );
// uses log2table :
F32 rrlog2_U32_approx( U32 x );
F32 rrlog2_U64_approx( U64 x );

U32 rrIlog2roundf(F32 val);

// inlines below :
//U32 rrIlog2floor(U32 val);
//U32 rrIlog2ceil (U32 val);
//U32 rrIlog2round(U32 val);

rrbool rr_isnan(F64 val);
rrbool rr_isnan_f(F32 val);
rrbool rr_isnaninf(F64 val);
rrbool rr_isnaninf_f(F32 val);

// see also rrGetBitMask32_Not32 in rrvarbits.h
static RADINLINE U32 rrMask32(int nb)
{
	RR_ASSERT( nb >= 0 && nb < 32 );
	U32 bit = RR_ONE_U32 << nb;
	U32 mask = bit-1;
	return mask;
}
static RADINLINE U64 rrMask64(int nb)
{
	RR_ASSERT( nb >= 0 && nb < 64 );
	U64 bit = RR_ONE_U64 << nb;
	U64 mask = bit-1;
	return mask;
}

static RADINLINE rrbool rrIsPow2(const U32 x)
{
	RR_ASSERT( x != 0 );
    return ! (x & ~(-(S32)x));
}

#ifdef __RAD64REGS__
static RADINLINE rrbool rrIsPow2_64(const U64 x)
{
	RR_ASSERT( x != 0 );
    return ! (x & ~(-(S64)x));
}
#endif

// rrNextPow2 : if x already is Pow2, returns x ; else returns the next higher power of 2 ; rrNextPow2 returns 1 for x <= 0
U32 rrNextPow2(const U32 x);

// rrPrevPow2 : if x already is Pow2, returns x ; else returns the next lower power of 2 ; rrPrevPow2 returns 0 for x <= 0
U32 rrPrevPow2(const U32 x);

#ifdef __RAD64REGS__
U64 rrNextPow2_64(const U64 x);
U64 rrPrevPow2_64(const U64 x);
#endif

UINTa rrNextPow2_A(const UINTa x);
UINTa rrPrevPow2_A(const UINTa x);
rrbool rrIsPow2_A(const UINTa x);

U32 rrClosestPow2(const U32 x);

// Aligns v up, alignment must be a power of two.
static RADINLINE U32 rrAlignUp32(const U32 v, const S32 alignment)
{
    RR_ASSERT(rrIsPow2(alignment));
    //RR_ASSERT( v >= 0 );
    return (v+(alignment-1)) & ~(alignment-1);
}

// Aligns v down, alignment must be a power of two.
static RADINLINE U32 rrAlignDown32(const U32 v, const S32 alignment)
{
    RR_ASSERT(rrIsPow2(alignment));
    //RR_ASSERT( v >= 0 );
    return v & ~(alignment-1);
}

// Aligns v up, alignment must be a power of two.
static RADINLINE U64 rrAlignUp64(const U64 v, const S32 alignment)
{
    RR_ASSERT(rrIsPow2(alignment));
    //RR_ASSERT( v >= 0 );
    return (v+(alignment-1)) & ~(alignment-1);
}

// Aligns v down, alignment must be a power of two.
static RADINLINE U64 rrAlignDown64(const U64 v, const S32 alignment)
{
    RR_ASSERT(rrIsPow2(alignment));
    //RR_ASSERT( v >= 0 );
    return v & ~(alignment-1);
}

#ifdef __cplusplus
// 64 bit overloads for the normal names
// Aligns v up, alignment must be a power of two.
static RADINLINE S64 rrAlignUp(const S64 v, const S32 alignment)
{
    return rrAlignUp64(v,alignment);
}

// Aligns v down, alignment must be a power of two.
static RADINLINE S64 rrAlignDown(const S64 v, const S32 alignment)
{
    return rrAlignDown64(v,alignment);
}

static RADINLINE S32 rrAlignUp(const S32 v, const S32 alignment)
{
    return rrAlignUp32((U32)v,alignment);
}

// Aligns v down, alignment must be a power of two.
static RADINLINE S32 rrAlignDown(const S32 v, const S32 alignment)
{
    return rrAlignDown32((U32)v,alignment);
}

static RADINLINE U32 rrAlignUp(const U32 v, const S32 alignment)
{
    return rrAlignUp32((U32)v,alignment);
}

// Aligns v down, alignment must be a power of two.
static RADINLINE U32 rrAlignDown(const U32 v, const S32 alignment)
{
    return rrAlignDown32((U32)v,alignment);
}

static RADINLINE U64 rrAlignUp(const U64 v, const S32 alignment)
{
    return rrAlignUp64(v,alignment);
}

// Aligns v down, alignment must be a power of two.
static RADINLINE U64 rrAlignDown(const U64 v, const S32 alignment)
{
    return rrAlignDown64(v,alignment);
}

static RADINLINE SINTa rrAlignUpA(const SINTa v, const S32 alignment)
{
    return RR_STRING_JOIN(rrAlignUp,RAD_PTRBITS)((RR_STRING_JOIN(S,RAD_PTRBITS))v,alignment);
}

// Aligns v down, alignment must be a power of two.
static RADINLINE SINTa rrAlignDownA(const SINTa v, const S32 alignment)
{
    return RR_STRING_JOIN(rrAlignDown,RAD_PTRBITS)((RR_STRING_JOIN(S,RAD_PTRBITS))v,alignment);
}

static RADINLINE UINTa rrAlignUpA(const UINTa v, const S32 alignment)
{
    return RR_STRING_JOIN(rrAlignUp,RAD_PTRBITS)((RR_STRING_JOIN(U,RAD_PTRBITS))v,alignment);
}

// Aligns v down, alignment must be a power of two.
static RADINLINE UINTa rrAlignDownA(const UINTa v, const S32 alignment)
{
    return RR_STRING_JOIN(rrAlignDown,RAD_PTRBITS)((RR_STRING_JOIN(U,RAD_PTRBITS))v,alignment);
}

template <typename T>
static RADINLINE T * rrAlignUpPointer(T * v, const S32 alignment)
{
    return (T *) rrAlignUpA((UINTa)v,alignment);
}

template <typename T>
static RADINLINE T * rrAlignDownPointer(T * v, const S32 alignment)
{
    return (T *) rrAlignDownA((UINTa)v,alignment);
}

template <typename T>
static RADINLINE bool rrIsAligned(T v, const S32 alignment)
{
    return rrAlignDown(v,alignment) == v;
}

template <>
RADINLINE bool rrIsAligned<SINTa>(SINTa v, const S32 alignment)
{
    return rrAlignDownA(v,alignment) == v;
}

template <>
RADINLINE bool rrIsAligned<UINTa>(UINTa v, const S32 alignment)
{
    return rrAlignDownA(v,alignment) == v;
}

template <typename T>
static RADINLINE bool rrIsAlignedPointer(T * v, const S32 alignment)
{
    return rrIsAligned((UINTa)v,alignment);
}

#endif

//=============================================

// return a*b 
static RADINLINE U32 rrMul32High(U32 a,U32 b)
{
    // this appears to do the right thing :
    return (U32)( ((U64) a * b) >>32);
}

//=============================================

// unions for treating floats as their bits :

typedef union union_rrFloatAnd32
{
    U32 i;
    F32 f;
} rrFloatAnd32;

typedef union union_rrDoubleAnd64
{
    U64 i;
    F64 f;
} rrDoubleAnd64;

static RADFORCEINLINE U32 rrF32AsInt(F32 val)
{
    rrFloatAnd32 ff;
    ff.f = val;
    return ff.i;
}

static RADFORCEINLINE U64 rrF64AsInt(F64 val)
{
    rrDoubleAnd64 ff;
    ff.f = val;
    return ff.i;
}

static RADFORCEINLINE F32 rrF32FromInt(U32 val)
{
    rrFloatAnd32 ff;
    ff.i = val;
    return ff.f;
}

static RADFORCEINLINE F64 rrF64FromInt(U64 val)
{
    rrDoubleAnd64 ff;
    ff.i = val;
    return ff.f;
}

//=============================================

/*

ftoi : truncates ; eg. fractions -> 0

*/
static RADINLINE S32 rr_ftoi_trunc(const F32 f)
{
    /*
    #ifdef __RADNT__
    // SSE single scalar cvtt to avoid rounding mode issues ?
    return _mm_cvtt_ss2si( _mm_set_ss( f ) );
    #endif
    */

    // just C cast :
    return (S32) f;
}

static RADINLINE S32 rr_ftoi_round(const F32 val)
{
    return ( val >= 0.f ) ? rr_ftoi_trunc( val + 0.5f ) : rr_ftoi_trunc( val - 0.5f );
}

static RADINLINE S32 rr_ftoi_round_positive(const F32 val)
{
    // only correct for val >= 0 ; doesn't assert though, lets you pass negatives, they are just wrong
    return rr_ftoi_trunc( val + 0.5f );
}

// @@ alias ?
#define rr_ftoi         rr_ftoi_trunc
#define rr_froundint    rr_ftoi_round
#define rr_froundint_positive    rr_ftoi_round_positive

// convert double/float to integer with fastest path, don't care about rounding
#define rr_dtoi_fastest(x)  ((S32) (x))
#define rr_ftoi_fastest(x)  ((S32) (x))

//=======================================================

// F32 min/max and select
#define RR_FSEL_F32(x,a,b) (((x) >= 0) ? (a) : (b))
#define RR_MIN_F32(a,b) RR_MIN(a,b)
#define RR_MAX_F32(a,b) RR_MAX(a,b)

//=======================================================
// CB : getbitlevel used to be in VarBits , but doesn't really belong there;
//  doesn't really belong here either though

// rrGetBitLevel :
//    getbitlevel(n) is the number of bits that n uses for its on bits
//    eg. n < (1<<getbitlevel(n)) , n >= (1<<(getbitlevel(n)-1))
//    getbitlevel(n)-1 is the bit position of the leftmost 1 bit in 'n'
// NOTE : getbitlevel(0) = 0
//  getbitlevel(n) = ilog2ceil except on powers of two (on powers of two, GetBitLevel is one larger)
//
// that is :
//	 (1<<getbitlevel(n))-1 
//  is a mask which covers all the 1 bits of n
//---------------------------------------
//   topbit = (getbitlevel(n)-1); 
//  is the position of the top bit of n
//	eg. n >= (1<<topbit) && n < (2<<topbit)
//  eg. for sending "NOSB" / EliasGamma encoding

// WARNING : this getbitlevel only works on *constant* U16 values ! (up to 65535)
//   it will fail to compile with strange errors if you use a variable
#define rrGetBitLevel_C(level)        \
(                                      \
  (((level)<    1)?0:                  \
  (((level)<    2)?1:                  \
  (((level)<    4)?2:                  \
  (((level)<    8)?3:                  \
  (((level)<   16)?4:                  \
  (((level)<   32)?5:                  \
  (((level)<   64)?6:                  \
  (((level)<  128)?7:                  \
  (((level)<  256)?8:                  \
  (((level)<  512)?9:                  \
  (((level)< 1024)?10:                 \
  (((level)< 2048)?11:                 \
  (((level)< 4096)?12:                 \
  (((level)< 8192)?13:                 \
  (((level)<16384)?14:                 \
  (((level)<32768)?15:                 \
  (((level)<65536)?16:sizeof(char[65535-level]) \
  )))))))))))))))))                    \
)

//=======================================================
// rrGetBitLevel is just Clz from rrbits.h now :
	
static RADFORCEINLINE U32 rrGetBitLevel_V( U32 val )
{
    int b = rrClz32(val);
    return (val) ? (32 - b) : 0;
}

static RADFORCEINLINE U32 rrGetBitLevel64_V( U64 val )
{
    int b = rrClz64(val);
    return (val) ? (64 - b) : 0;
}

static RADFORCEINLINE U32 rrGetBitLevel_V_NonZero( U32 val )
{
	RR_ASSERT( val > 0 );
    return 32 - rrClz32(val);
}

static RADFORCEINLINE U32 rrGetBitLevel64_V_NonZero( U64 val )
{
	RR_ASSERT( val > 0 );
    return 64 - rrClz64(val);
}
    
//---------------------------------------------------------------

#ifdef __RAD64__
#define rrGetBitLevelA_V	rrGetBitLevel64_V
#else
#define rrGetBitLevelA_V	rrGetBitLevel_V
#endif

#ifdef __RAD64REGS__
#define rrGetBitLevelR_V	rrGetBitLevel64_V
#else
#define rrGetBitLevelR_V	rrGetBitLevel_V
#endif

//=======================================================

static RADINLINE U32 rrIlog2ceil(U32 val)
{
	RR_ASSERT( val != 0 );
	RADASSUME( val != 0 );

	if ( val <= 1 ) return 0;
	else
	{
        // clz :
		U32 ret = rrGetBitLevel_V_NonZero( val-1 );
	
		RR_ASSERT( (1U<<ret) >= val );
		RR_ASSERT( ret == 0 || (1U<<(ret-1)) < val );
	
		return ret;
	}
}

static RADINLINE U32 rrIlog2floor(U32 val)
{
	U32 ret = rrIlog2ceil(val);
	//if ( ! rrIsPow2(val) ) ret--;
	ret -= !rrIsPow2(val);
	return ret;
}

static RADINLINE U32 rrIlog2round(U32 val)
{
    return rrIlog2roundf( (F32)val );
}

//=======================================================

// replace libc expf(x) and exp(x) with our own versions for e^x
// this isn't for speed it's to avoid libc/libm dependencies
// do not use the CRT in Oodle when avoidable

// minimize SSD err :
//#define RR_APPROX_EXP_HUMP_C	(-0.34363413890962335)
// minimize relative err :
#define RR_APPROX_EXP_HUMP_C	(-0.34168466148744669)

// max relative error of 0.295 %

static inline double rr_exp_approx(const double x)
{
	// change to power of 2 :
	double t = x * (1.0/RR_LN2);
	
	// do pow of 2^t :
	
	RR_ASSERT( t >= -1000 && t <= 1000 );

	// bias before int-float so that negatives floor to -infinity instead of zero
	U64 ti = (U64)(1023 + t);
	double tf = t - ti + 1023;
	
	// construct the exact integer part of 2^t :
	double y = rrF64FromInt( ti << 52 );
	
	// linear fractional part could be done in the int mantissa instead
	// linear is exact at endpoint, correct with a hump in the middle
	y *= 1.0 + tf + RR_APPROX_EXP_HUMP_C * tf * (1.0 - tf);

    return y;
}

static inline float rr_expf_approx(const float x)
{
	// change to power of 2 :
	float t = x * (float)(1.0/RR_LN2);
	
	// do pow of 2^t :

	RR_ASSERT( t >= -120 && t <= 120 );

	// bias before int-float so that negatives floor to -infinity instead of zero
	U32 ti = (U32)(127 + t);
	float tf = t - ti + 127;
	
	// construct the exact integer part of 2^t :
	float y = rrF32FromInt( ti << 23 );
	
	// linear fractional part could be done in the int mantissa instead
	// linear is exact at endpoint, correct with a hump in the middle
	y *= 1.f + tf + float(RR_APPROX_EXP_HUMP_C) * tf * (1.f - tf);

    return y;
}

// replacement for libc log(x) ; log-base-e (natural log)
static inline double rr_loge( const double x )
{
	return RR_LN2 * rrlog2_F64(x);
}

//=======================================================

RR_NAMESPACE_END


#endif //__RADRR_MATHH__
