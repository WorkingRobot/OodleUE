// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRRBITMAP_DXTC_BLOCK8x8UTIL_H__
#define __RADRRBITMAP_DXTC_BLOCK8x8UTIL_H__

#include "rrcolor.h"

RR_NAMESPACE_START

struct rrSingleChannelBlock8x8
{
	U8		values[64];
};

RR_COMPILER_ASSERT( sizeof(rrSingleChannelBlock8x8) == 64 );


U32 rrSingleChannelBlock8x8_SAD(const rrSingleChannelBlock8x8 * a,const rrSingleChannelBlock8x8 *b);

// SATD = abs delta of AC_L1
U32 rrSingleChannelBlock8x8_Hadamard_AC_L1(const rrSingleChannelBlock8x8 * a);

U32 rrSingleChannelBlock8x8_Laplacian_AC_L1(const rrSingleChannelBlock8x8 * a);

RR_NAMESPACE_END

#endif // __RADRRBITMAP_DXTC_BLOCK8x8UTIL_H__
