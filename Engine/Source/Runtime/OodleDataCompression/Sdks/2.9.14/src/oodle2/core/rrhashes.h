// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_HASHESH__
#define __RADRR_HASHESH__

#include "rrbase.h"

/**

CB : various hashes

**/

RR_NAMESPACE_START

//=============================================================================

RADINLINE U32 rrBitShuffle(U32 state)
{
    // Bob Jenkins's Mix
    state += (state << 12);
    state ^= (state >> 22);
    state += (state << 4);
    state ^= (state >> 9);
    state += (state << 10);
    state ^= (state >> 2);
    state += (state << 7);
    state ^= (state >> 12);
    return state;
}

// reversible mixer from BurtleBurtle :
//  a,b,c should typically be U32's
#define rrMix3(a,b,c) do { \
  a -= c;  a ^= RR_ROTL32(c, 4);  c += b; \
  b -= a;  b ^= RR_ROTL32(a, 6);  a += c; \
  c -= b;  c ^= RR_ROTL32(b, 8);  b += a; \
  a -= c;  a ^= RR_ROTL32(c,16);  c += b; \
  b -= a;  b ^= RR_ROTL32(a,19);  a += c; \
  c -= b;  c ^= RR_ROTL32(b, 4);  b += a; \
} while(0)

// after FinalMix3 "c" has the best value
#define rrFinalMix3(a,b,c) { \
  c ^= b; c -= RR_ROTL32(b,14); \
  a ^= c; a -= RR_ROTL32(c,11); \
  b ^= a; b -= RR_ROTL32(a,25); \
  c ^= b; c -= RR_ROTL32(b,16); \
  a ^= c; a -= RR_ROTL32(c,4);  \
  b ^= a; b -= RR_ROTL32(a,14); \
  c ^= b; c -= RR_ROTL32(b,24); \
}

// RR_U32_HAS_ZERO_BYTE :
//  does this 4-char dword have any zero byte ?
#define RR_U32_HAS_ZERO_BYTE(V)       (((V) - 0x01010101UL) & ~(V) & 0x80808080UL)
#define RR_U64_HAS_ZERO_BYTE(V)       (((V) - 0x0101010101010101ULL) & ~(V) & 0x8080808080808080ULL)

//=============================================================================

//---------------------------------------------------
//
// FNV and Murmur are good 32 bit hashes
//  note : to use in smaller tables make sure you bit-fold down !

RADINLINE U32 rrFNVHash (const U8 * data, SINTa len) 
{
    U32 hash = 2166136261UL;
    for (SINTa i=0; i < len; i++) 
    {
        hash = ((U32)(16777619) * hash) ^ data[i];
    }
    return hash;
}

RADINLINE U32 rrFNVHashStr(const char * str)
{
    U32 hash = 2166136261UL;
    while (*str)
    {
        hash = ((U32)(16777619) * hash) ^ (*str++);
    }
    return hash;
}


// unsafe_tolower works on alphabetic chars and also leaves number digits alone
//      RR_UNSAFE_TOLOWER is also safe on '.' and '/' but not '\\' or '_'
#define RR_UNSAFE_TOLOWER(c)    ( (c) | 0x20 )
#define RR_MACRO_TOLOWER(c)     ( ( (c) >= 'A' && (c) <= 'Z' ) ? ( (c) | 0x20 ) : (c) )

// rrHashInsensitiveChar turns a path character into something that is not case or path-style sensitive :
//  !! you must use rrHashInsensitiveChar when hashing paths if you want it to compare right
RADINLINE U8 rrHashInsensitiveChar(char c)
{
    // '/' = 0x2F
    // '\\' = 0x5c
    // '/' is not affected by unsafe_tolower , but '\\' is

    return ( c == '\\' ) ? '/' : RR_MACRO_TOLOWER(c);
}

RADINLINE U32 rrFNVHashStrInsensitive(const char * str)
{
    U32 hash = 2166136261UL; 
    while (*str)
    {
        char c = *str++;
        hash = ((U32)(16777619) * hash) ^ rrHashInsensitiveChar(c);
    }
    return hash;
}

// Murmur is slightly stronger and more generic than FNV, but slower
U32 rrMurmurHash2( const U8 * bytes, SINTa len, U32 seed RADDEFAULT( 0x12345678 ) );

//---------------------------------------------------
// CB :
//  simple hashes for 32->32 and 64->32
//  see http://www.concentric.net/~Ttwang/tech/inthash.htm

// Bob's non-multiplying version :
RADINLINE U32 rrHash32( U32 a)
{
    a -= (a<<6);
    a ^= (a>>17);
    a -= (a<<9);
    a ^= (a<<4);
    a -= (a<<3);
    a ^= (a<<10);
    a ^= (a>>15);
    return a;
}

// if you can do multiplies, this is a little faster :
RADINLINE U32 rrHash32_mul( U32 a)
{
    a = (a ^ 61) ^ (a >> 16);
    a = a + (a << 3);
    a = a ^ (a >> 4);
    a = a * 0x27d4eb2d;
    a = a ^ (a >> 15);
    return a;
}

// based on Murmur :
RADINLINE U32 rrHash32_murmur(U32 v)
{
    const U32 m = 0x5bd1e995;
    
    U32 h = 0x12345678;

    // Mix 4 bytes at a time into the hash
    v *= m; 
    v ^= v >> 24; 
    v *= m; 
    
    h *= m; 
    h ^= v;
        
    // Do a few final mixes of the hash to ensure the last few
    // bytes are well-incorporated.

    h ^= h >> 13;
    h *= m;
    h ^= h >> 15;

    return h;
}

#if 1

//use this ?
// faster if you have 64 bit registers
// Thomas Wang's :

RADINLINE U32 rrHash64(U64 key)
{
	key = (~key) + (key << 18); // key = (key << 18) - key - 1;
	key = key ^ (key >> 31);
	key = key * 21; // key = (key + (key << 2)) + (key << 4);
	key = key ^ (key >> 11);
	key = key + (key << 6);
	key = key ^ (key >> 22);
	return (U32) key;
}

RADINLINE U32 rrHashTwo32(U32 v0,U32 v1)
{
	return rrHash64( ((U64)v0<<32) | v1 );
}

#else

// faster if you don't have 64 bit registers

RADINLINE U32 rrHashTwo32(U32 v0,U32 v1)
{
	// based on Murmur :

    const U32 m = 0x5bd1e995;
    
    U32 h = 0x12345678;

    // Mix 4 bytes at a time into the hash
    v0 *= m; 
    v0 ^= v0 >> 24; 
    v0 *= m; 
    
    h *= m; 
    h ^= v0;
    
    v1 *= m; 
    v1 ^= v1 >> 24; 
    v1 *= m; 
    
    h *= m; 
    h ^= v1;
    
    // Do a few final mixes of the hash to ensure the last few
    // bytes are well-incorporated.

    h ^= h >> 13;
    h *= m;
    h ^= h >> 15;

    return h;
}

RADINLINE U32 rrHash64(U64 v)
{
    U32 v0 = (U32)(v>>32);
    U32 v1 = (U32)(v);

    return rrHashTwo32(v0,v1);  
}

#endif

//---------------------------------------------------

RADINLINE U32 rrHashA(UINTa v)
{
	#ifdef __RAD64__
	return rrHash64((U64)v);
	#else
	return rrHash32((U32)v);
	#endif
}

RR_NAMESPACE_END

#endif // __RADRR_HASHESH__
