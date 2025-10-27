// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_RANDH__
#define __RADRR_RANDH__

#include "rrbase.h"

/********************

CB random number helpers

rrRand32 is the main function, basic 32 bit random

The goal is to be moderately fast without giant tables like Mersenne Twister,
 and not have any terrible non-random bits like the clib rand()

These functions are not intended to be super-duper fast, nor are they intended to be
 good cryptographically correct random bits.

In general they err to the side of correctness, not the side of speed.

---------------------

The Rand functions primarily act on a struct rrRandState that you pass in
 for example you could put an rrRandState in your TLS and thus have a Rand per thread

There are variants that act on a global shared state for convenience
 but this should NOT be used in production code!

*********************/

RR_NAMESPACE_START

//------------------------------------

// stateless munge -> munge :

// "Munge" takes some seed input and gives you a "random" U32 out
// thorough munge :
U32 rrRand32Munge(U32 seed);

// simple munge :
RADINLINE U32 rrRand32MungeSimple(U32 from)
{
    // shuffle non-random bits to the middle, and xor to decorrelate with seed
    U32 result = 0x31415926 ^ ((from >> 13) + (from << 19));
    //U32 result = 0x31415926 ^ RR_ROTL32(from,19);
    result = result * 2147001325 + 715136305;
    return result;
}

// Lemire's splitmix64_stateless
RADINLINE U64 rrRand64MungeSimple(U64 from)
{
  U64 z = (from + (0x9E3779B97F4A7C15ULL));
  z = (z ^ (z >> 30)) * (0xBF58476D1CE4E5B9ULL);
  z = (z ^ (z >> 27)) * (0x94D049BB133111EBULL);
  return z ^ (z >> 31);
}

// data-based seed :
U64 rrRand64Simple_SeedFromU64Array(const U64 * pvals,int nvals);

// splitmix64 :
// trivial LCG followed by a hash munge ; supposedly has good properties
//	can initialize state = 0
// this is fast if your critical path is in the rand state variable
//	 (eg. benchmarks that call rand over and over)
// but this is not as fast in our typical use case where you are stalling on the value coming out of rand

RADINLINE U64 rrRand64Simple(U64 * pState)
{
	U64 z = *pState + 0x9E3779B97F4A7C15ULL;
	*pState = z;
	// now "munge" :
	z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
	z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
	return z ^ (z >> 31);
}

// rrRand64Simple convention :
//  call rrRand64Simple to generate a U64
//  then you can pull multiple random values from that
//  as you consume bits of the rand, left shift
//	best bits are at the top

RADINLINE U64 rrRand64Simple_NBits(U64 r,U32 count)
{
	RR_ASSERT( count > 0 && count < 64 );
    return r >> (64 - count);
}

RADINLINE bool rrRand64Simple_Bool(U64 r)
{
	return ((S64)r) < 0;
}

// returns in [0,count-1] aka [0,count)
// use top bits of r
RADINLINE U32 rrRand64Simple_Mod(U64 r,U32 count)
{
    return (U32) ( ((r>>32) * count) >> 32 );
}

// rand in range [lo,hi] inclusive
//  note : rrRandCount(count) = rrRandInRange(0,count-1)
// uses top bits of r
RADINLINE S32 rrRand64Simple_InRange(U64 r,S32 lo,S32 hi)
{
    RR_ASSERT( hi >= lo );
    return lo + rrRand64Simple_Mod(r,hi-lo+1);
}

//------------------------------------
// functions that work on your own rrRandState struct :
//	(this is what you should do to be thread safe)

struct rrRandState
{
	U32 x,y,z,c;
};

#define RR_RANDSTATE_INIT_VALUE	{ 123456789, 362436000, 521288629, 7654321 }

void rrRandState_Init(rrRandState * rrs);

void rrRandState_Log(rrRandState * rrs);

// the main function :
U32 rrRandState32(rrRandState * rrs);

// seed with a value for reproduction :
/// (seed will be munged, so it's okay to just use seed=0,1,2,...)
void rrRandStateSeed(rrRandState * rrs, U32 seed );

// seed with time for randomness :
void rrRandStateSeedWithTime(rrRandState * rrs, U64 tsc, U64 qpc);

// unit float is in [0,1) (never exactly equal to 1.0)
RADINLINE F32 rrRandStateUnitFloat(rrRandState * rrs)
{
    // BTW you could also do the Iniqo Quilez way of putting random bits in the IEEE float manually
    return rrRandState32(rrs) * (1.f/4294967296.f);
}

RADINLINE F32 rrRandStateFloat(rrRandState * rrs,F32 lo,F32 hi)
{
    return lo + rrRandStateUnitFloat(rrs) * (hi - lo);
}

RADINLINE U64 rrRandState64(rrRandState * rrs)
{
    U32 r1 = rrRandState32(rrs);
    U32 r2 = rrRandState32(rrs);
    return ((U64)r1<<32) | r2;
}

RADINLINE U16 rrRandState16(rrRandState * rrs)
{
    U32 r = rrRandState32(rrs);
    return (U16)(r + (r>>16));
}

RADINLINE U8 rrRandState8(rrRandState * rrs)
{
    U32 r = rrRandState32(rrs);
    return (U8)(r + (r>>11));
}

RADINLINE U32 rrRandStateMod(rrRandState * rrs,U32 count) // returns in [0,count-1] aka [0,count)
{
	// I want the 32*32 mulhi
    // see also UInt32x32To64
    U32 rand32 = rrRandState32(rrs);
    return (U32) ( ((U64)rand32 * count) >> 32 );
}

#ifdef __RAD64REGS__
U64 rrRandStateMod64(rrRandState * rrs,U64 count); // returns in [0,count-1] aka [0,count)
#endif

// rand in range [lo,hi] inclusive
//  note : rrRandCount(count) = rrRandInRange(0,count-1)
RADINLINE S32 rrRandStateInRange(rrRandState * rrs,S32 lo,S32 hi)
{
    RR_ASSERT( hi >= lo );
    return lo + rrRandStateMod(rrs,hi-lo+1);
}

RADINLINE rrbool rrRandStateBool(rrRandState * rrs)
{
    // not sure what the best way to get one good bit out is ...
    U32 u32 = rrRandState32(rrs);
    return (rrbool) ( (u32 + (u32>>7)) & 1 );
}

//------------------------------------
// functions that work on global state :
// WARNING : NOT thread safe

extern rrRandState g_randGlobalState;

RADINLINE void rrRand_Log_Global() { rrRandState_Log(&g_randGlobalState); }

RADINLINE void rrRandSeed_Global( U32 seed ) { rrRandStateSeed(&g_randGlobalState,seed); }

RADINLINE void rrRandSeedWithTime_Global(U64 tsc, U64 qpc) { rrRandStateSeedWithTime(&g_randGlobalState,tsc,qpc); }

RADINLINE F32 rrRandUnitFloat_Global() { return rrRandStateUnitFloat(&g_randGlobalState); }

RADINLINE F32 rrRandFloat_Global(F32 lo,F32 hi) { return rrRandStateFloat(&g_randGlobalState,lo,hi); }

RADINLINE U64 rrRand64_Global() { return rrRandState64(&g_randGlobalState); }

RADINLINE U32 rrRand32_Global() { return rrRandState32(&g_randGlobalState); }

RADINLINE U16 rrRand16_Global() { return rrRandState16(&g_randGlobalState); }

RADINLINE U8 rrRand8_Global() { return rrRandState8(&g_randGlobalState); }

RADINLINE U32 rrRandMod_Global(U32 count) { return rrRandStateMod(&g_randGlobalState,count); }

#ifdef __RAD64REGS__
RADINLINE U64 rrRandMod64_Global(U64 count) { return rrRandStateMod64(&g_randGlobalState,count); }
#endif

RADINLINE S32 rrRandInRange_Global(S32 lo,S32 hi) { return rrRandStateInRange(&g_randGlobalState,lo,hi); }

RADINLINE rrbool rrRandBool_Global() { return rrRandStateBool(&g_randGlobalState); }

//------------------------------------

RR_NAMESPACE_END

#endif // __RADRR_RANDH__
