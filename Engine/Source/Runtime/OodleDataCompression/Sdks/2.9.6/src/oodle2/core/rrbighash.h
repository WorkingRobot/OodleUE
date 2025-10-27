// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_BIGHASH_H__
#define __RADRR_BIGHASH_H__

#include "oodlecore.h"
#include "rrhashes.h"

/**

hashes for big array

**/

OODLE_NS_START

//=============================================================================

// Fast SIMD File Hash for large buffers :
U64 rrBigHash64_SIMD( const void * bytes, SINTa size);

// 32-bit hash is just the bottom bits of 64-bit
// the bottom 32 bit of Lookup2 are good quality :
static RADINLINE U32 rrBigHash32_SIMD(const void * bytes, SINTa size)
{
	return (U32) rrBigHash64_SIMD(bytes,size);
}

// always use the "SIMD" hash even when it runs scalar for value consistency
#define	rrBigHash32	rrBigHash32_SIMD
#define	rrBigHash64	rrBigHash64_SIMD

//=============================================================================

U64 rrBigHash64_Combine(U64 a, U64 b);

//=============================================================================

OODLE_NS_END

#endif // __RADRR_BIGHASH_H__
