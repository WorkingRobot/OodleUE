// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrhashes.h"
#include "rrmem.h"

RR_NAMESPACE_START

//===============================================================

U32 rrMurmurHash2( const U8 * key, SINTa len, U32 seed )
{
    // 'm' and 'r' are mixing constants generated offline.
    // They're not really 'magic', they just happen to work well.

    const U32 m = 0x5bd1e995;
    //const int r = 24;

    // Initialize the hash to a 'random' value

    U32 h = seed ^ ((U32)len);

    // Mix 4 bytes at a time into the hash

    const U8 * data = (const U8 *)key;

    while(len >= 4)
    {
        U32 k = RR_GET32_BE(data);
        data += 4;
        len -= 4;

        k *= m; 
        k ^= k >> 24; 
        k *= m; 
        
        h *= m; 
        h ^= k;
    }
    
    // Handle the last few bytes of the input array

    switch(len)
    {
    case 3: h ^= data[2] << 16;
    case 2: h ^= data[1] << 8;
    case 1: h ^= data[0];
            h *= m;
    case 0: break;
    }

    // Do a few final mixes of the hash to ensure the last few
    // bytes are well-incorporated.

    h ^= h >> 13;
    h *= m;
    h ^= h >> 15;

    return h;
} 

RR_NAMESPACE_END
