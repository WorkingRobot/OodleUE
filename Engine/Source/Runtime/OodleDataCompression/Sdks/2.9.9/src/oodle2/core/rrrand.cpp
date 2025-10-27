// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrrand.h"
#include "rrhashes.h"
#include "rrmath.h"
#include "rrlog.h"

RR_NAMESPACE_START

void rrRandState_Init(rrRandState * rrs)
{
	static const rrRandState c_init = RR_RANDSTATE_INIT_VALUE;
	*rrs = c_init;
}

rrRandState g_randGlobalState = RR_RANDSTATE_INIT_VALUE;

void rrRandState_Log(rrRandState * rrs)
{
	rrprintf("rrRandState rrs = { 0x%08X, 0x%08X, 0x%08X, 0x%08X };\n",rrs->x,rrs->y,rrs->z,rrs->c);
}

U32 rrRandState32(rrRandState * rrs)
{
    // Marsaglia's KISS99
    //  basically just a sum of 3 very simple random generators
    //  supposedly has very good randomness in every bit
    
    U32 s_kiss99_x = rrs->x;
    U32 s_kiss99_y = rrs->y;
    U32 s_kiss99_z = rrs->z;
    U32 s_kiss99_c = rrs->c;
        
    s_kiss99_x = 69069*s_kiss99_x+12345;  
    
    s_kiss99_y ^= (s_kiss99_y<<13);
    s_kiss99_y ^= (s_kiss99_y>>17); 
    s_kiss99_y ^= (s_kiss99_y<<5);  
    
    //static const uint64 a = 698769069ULL;  
    //uint64 t = a*z+c; 
    U64 t = (698769069ULL*s_kiss99_z);
    t += s_kiss99_c;
    s_kiss99_c = (U32)(t>>32); 
    s_kiss99_z = (U32)t;
    
    U32 ret = (s_kiss99_x+s_kiss99_y+s_kiss99_z);

    rrs->x = s_kiss99_x;
    rrs->y = s_kiss99_y;
    rrs->z = s_kiss99_z;
    rrs->c = s_kiss99_c;
    
    ret += (ret>>15);
    return ret;
}

// seeds

// seed with a value for reproduction :
void rrRandStateSeed(rrRandState * rrs, U32 seed )
{
    seed = rrRand32MungeSimple(seed);
    
    // need to get 4 seeds from one :
    //  probably a better way to do this :
    
    rrs->x = 123456789 ^ seed;
    rrs->y = 362436000 ^ seed;
    rrs->z = 521288629 ^ seed;
    rrs->c = 7654321 ^ seed;

    // burn off some shitty initial rands :
    rrRandState32(rrs);
    rrRandState32(rrs);
    rrRandState32(rrs);
    rrRandState32(rrs);
}

// seed with time for randomness :
void rrRandStateSeedWithTime(rrRandState * rrs, U64 tsc, U64 qpc)
{
    // try to get some very "random" timers -
    //  eg. that have very different bits even if you run twice in a row
    //  clock() is very bad , but tsc is good :

	// these seeds suck because they are zero-rebased !!
	// rrGetTicks is not the raw tsc, which is what I want
	//	-> fixed with OodleGetTicksRaw

	// @@ BETTER , but moved to ext :(
    //U64 tsc = OodleGetTicksRaw();
    //U64 tsc = rrGetTicks();
    //U64 qpc = rrGetTime();
    
    U32 t1 = (U32) (tsc>>32) ^ (U32) (qpc>>32);
    U32 t2 = (U32) tsc;
    U32 t3 = (U32) qpc;
    
    t1 = rrRand32MungeSimple(t1);
    t2 = rrRand32MungeSimple(t2);
    t3 = rrRand32MungeSimple(t3);
    
    rrMix3(t1,t2,t3);
    
    rrs->x = 123456789 ^ t1;
    rrs->y = 362436000 ^ t2;
    rrs->z = 521288629 ^ t3;
    rrs->c = 7654321 ^ (t1<<16) ^ (t2>>16) ^ t3;
    
    // burn off some shitty initial rands :
    rrRandState32(rrs);
    rrRandState32(rrs);
    rrRandState32(rrs);
    rrRandState32(rrs);
}

/*
// "Munge" takes some seed input and gives you a "random" U32 out
U32 rrRand32Munge(U32 seed)
{
    // this is basically just a 32->32 hash
    // CRC might be better  

    U32 h = rrRand32MungeSimple(seed);
    
    const U32 c_murmur2_mul = 0x5bd1e995;
    
    // a Murmur2 step :
    U32 k = seed;

    k *= c_murmur2_mul; 
    k ^= k >> 24; 
    k *= c_murmur2_mul; 
    
    h *= c_murmur2_mul; 
    h ^= k;

    return h;
}
*/

// Cessu's version :
U32 rrRand32Munge(U32 seed)
{
    U64 r = (2857720171ULL * ((U32) seed)) ^ 0x1EF57D8A7B344E7BULL;
    r ^= r >> 29;
    r += r << 16;
    r ^= r >> 21;
    r += r >> 32;
    r = (2857720171ULL * ((U32) (seed ^ r))) ^ (0xD9EA571C8AF880B6ULL);
    r ^= r >> 29;
    r += r << 16;
    r ^= r >> 21;
    return (U32)r + (U32)(r >> 32);
}

#ifdef __RAD64REGS__
U64 rrRandStateMod64(rrRandState * rrs,U64 count)
{
	#ifdef _MSC_VER
	// x64 has a 64*64 mul hi
	U64 r = rrRandState64(rrs);
	U64 ret = __umulh(r,count);
	return ret;
	#else	
	if ( count < RR_S32_MAX )
		return rrRandStateMod(rrs,(U32)count);

	// find the next pow2 above count and get that many bits :
	U32 bl = rrGetBitLevel64_V(count);
	U64 mask = (1ULL<<bl)-1 ;
	RR_ASSERT( mask >= count && (mask>>1) < count );
	for(;;)
	{
		U64 r = rrRandState64(rrs);
		r &= mask;
		// should return >= 50% of the time :
		if ( r < count )
			return r;
	}
	#endif
}
#endif

U64 rrRand64Simple_SeedFromU64Array(const U64 * pvals,int nvals)
{
	// we just want to run a hash on the colors to get a pseudorand seed :
	U64 rand_state = 0x9E3779B97F4A7C15ULL;
	for(int i=0;i<nvals;i++)
	{
		U64 v = pvals[i];
		v *= 0xBF58476D1CE4E5B9ULL;
		v ^= RR_ROTR64(rand_state,43);
		
		rand_state *= 0x94D049BB133111EBULL;
		rand_state ^= v;
	}
	rand_state = rrRand64MungeSimple(rand_state);
	return rand_state;
}

RR_NAMESPACE_END
