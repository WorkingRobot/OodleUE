// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrmath.h"
#include "rrbase.h"
#include "log2table.h"
#include "cbradutil.h"
#include <math.h>

RR_NAMESPACE_START

// uses log2table :
F32 rrlog2_U32_approx( U32 x )
{
	return rrlog2_bk(x);
}

F32 rrlog2_U64_approx( U64 x )
{
	RR_DURING_ASSERT( F64 check = rrlog2_F64( (F64) x ) );
	
	F32 ret = rrlog2_bk_U64(x);
	
	RR_ASSERT( fequal(ret,(F32)check,0.1f) );
	
	return ret;
}

F64 rrlog2_F64( const F64 X )
{
    RR_ASSERT( X > 0.0 );

	// originally did this to avoid calling log() on Linux
	// but just always use our approximation on all platforms for consistency
	// and avoid CRT dependence on transcendentals

    //F64 simple =  log( X ) * (1.0 / RR_LN2);
    //return simple;

    ///-------------
    // approximate log2 by getting the exponent from the float
    //  and then using the mantissa to do a taylor series
    //  this works but is by no means fast

    U32 X_as_int = rrF32AsInt((F32)X);
    int iLogFloor = (X_as_int >> 23) - 127;

    rrFloatAnd32 fi;
    fi.i = (X_as_int & ( (1<<23)-1 ) ) | (127<<23);
    F32 frac = fi.f;
    RR_ASSERT( frac >= 1.0 && frac < 2.0 );

	double k5;
	
    if ( frac > 4.0/3.0 )
    {
        // (frac/2) is closer to 2.0 than frac is to 1.0
        //  push the iLog up and our correction will now be negative
        // the branch here sucks but this is necessary for accuracy
        //  when frac is near 2.0, t is near 1.0 and the Taylor is totally invalid
        frac /= 2.0;
        iLogFloor++;
        
		k5 = (1/5.0 + 11.0/(18.0*12.0));
	}
    else
    {    
		k5 = (1/5.0 - 11.0/(18.0*12.0));
	}

    // X = 2^iLogFloor * frac
    F64 t = frac - 1.0;

	// simple Taylor series assuming t is small
	//	-> would be better to minimize error over range of t
	//	see main_log2_approx
	//	but whatevs
    F64 lnFrac = t - t*t*0.5 + (t*t*t)*( (1.0/3.0) - t*(1.0/4.0) + t*t*k5 );
	
    F64 approx = (F64)iLogFloor + lnFrac * (1.0 / RR_LN2);

    RR_DURING_ASSERT( F64 exact =  log( X ) * (1.0 / RR_LN2) );
    RR_ASSERT( fabs(exact - approx) <= 0.0002 );

    return approx;
}

U32 rrIlog2roundf(F32 val)
{
    return ((rrF32AsInt(val) + 0x257D86) >> 23) - 127;
}

// don't require CRT
// if you don't care, testing for both nan or infinity is faster
rrbool rr_isnaninf_f(F32 val)
{
   union { F32 f; U32 u; } num;
   num.f = val;
   // nan or inf if exponent is 255
   return (num.u & 0x7f800000) == 0x7f800000;
}

rrbool rr_isnaninf(F64 val)
{
   union { F64 f; U64 u; } num;
   num.f = val;
   // nan or inf if exponent is 2^11-1
   #if defined(_MSC_VER) && _MSC_VER < 1300
   return (num.u & 0x7ff0000000000000Ui64) == 0x7ff0000000000000Ui64;
   #else
   return (num.u & 0x7ff0000000000000ULL) == 0x7ff0000000000000ULL;
   #endif
}

rrbool rr_isnan_f(F32 val)
{
   union { F32 f; U32 u; } num;
   num.f = val;
   // nan or inf if exponent is 255
   return (num.u & 0x7f800000) == 0x7f800000
      // nan if non-zero mantissa
      &&  (num.u & 0x007fffff);
}

rrbool rr_isnan(F64 val)
{
   union { F64 f; U64 u; } num;
   num.f = val;
   // nan or inf if exponent is 2^11-1
   #if defined(_MSC_VER) && _MSC_VER < 1300
   return (num.u & 0x7ff0000000000000Ui64) == 0x7ff0000000000000Ui64
      // nan if non-zero mantissa
      &&  (num.u & 0x000fffffffffffffUi64);
   #else
   return (num.u & 0x7ff0000000000000ULL) == 0x7ff0000000000000ULL
      // nan if non-zero mantissa
      &&  (num.u & 0x000fffffffffffffULL);
   #endif
}

U32 rrNextPow2(const U32 x)
{
	if ( x <= 1 ) return 1;
	
	RADASSUME( x > 1 );
	
    U32 bl = rrGetBitLevel_V( (x-1) );
    
    U32 up = 1 << bl;
    
    RR_ASSERT( up >= x );
    RR_ASSERT( up < 2*x );
    
    return up;
}

U32 rrPrevPow2(const U32 x)
{
	if ( x == 0 ) return 0;

    U32 bl = rrGetBitLevel_V( x );
    RR_ASSERT( bl != 0 );
    
    U32 down = 1 << (bl-1);
    
    RR_ASSERT( down <= x );
    RR_ASSERT( down*2 > x );
    
    return down;
}

U32 rrClosestPow2(const U32 x)
{
	if ( x <= 2 ) return x;
	
	U32 log2 = rrIlog2round(x);
	U32 ret = (1U<<log2);
	return ret;
}


#ifdef __RAD64REGS__
U64 rrNextPow2_64(const U64 x)
{
	if ( x <= 0 ) return 1;
	
    U64 bl = rrGetBitLevel64_V( (x-1) );
    
    U64 up = (U64)1 << bl;
    
    RR_ASSERT( up >= x );
    RR_ASSERT( up < 2*x );
    
    return up;
}

U64 rrPrevPow2_64(const U64 x)
{
	if ( x == 0 ) return 0;

    U64 bl = rrGetBitLevel64_V( x );
    RR_ASSERT( bl != 0 );
    
    U64 down = (U64)1 << (bl-1);
    
    RR_ASSERT( down <= x );
    RR_ASSERT( down*2 > x );
    
    return down;
}
#endif


UINTa rrNextPow2_A(const UINTa x)
{
#ifdef __RAD64__
	return rrNextPow2_64(x);
#else
	return rrNextPow2(x);
#endif
}

UINTa rrPrevPow2_A(const UINTa x)
{
#ifdef __RAD64__
	return rrPrevPow2_64(x);
#else
	return rrPrevPow2(x);
#endif
}

rrbool rrIsPow2_A(const UINTa x)
{
#ifdef __RAD64__
	return rrIsPow2_64(x);
#else
	return rrIsPow2(x);
#endif
}

RR_NAMESPACE_END

