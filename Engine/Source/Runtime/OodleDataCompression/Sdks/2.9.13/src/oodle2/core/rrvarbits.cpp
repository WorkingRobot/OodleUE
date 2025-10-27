// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrvarbits.h"

OODLE_NS_START

#ifdef RR_VARBITS_INLINES
//#pragma RR_PRAGMA_MESSAGE("RR_VARBITS_INLINES")
#endif

const RAD_ALIGN(U32, c_rrBitMask32[ 33 ], 32 ) =
{
           0,         1,           3,          7,
         0xf,      0x1f,        0x3f,       0x7f,
        0xff,     0x1ff,       0x3ff,      0x7ff,
       0xfff,    0x1fff,      0x3fff,     0x7fff,
      0xffff,    0x1ffff,    0x3ffff,    0x7ffff,
     0xfffff,   0x1fffff,   0x3fffff,   0x7fffff,
    0xffffff,  0x1ffffff,  0x3ffffff,  0x7ffffff,
   0xfffffff, 0x1fffffff, 0x3fffffff, 0x7fffffff,
  0xffffffff
};

const RAD_ALIGN(U64, c_rrBitMask64[ 65 ], 32 ) =
{
           0,         1,           3,          7,
         0xf,      0x1f,        0x3f,       0x7f,
        0xff,     0x1ff,       0x3ff,      0x7ff,
       0xfff,    0x1fff,      0x3fff,     0x7fff,
      0xffff,    0x1ffff,    0x3ffff,    0x7ffff,
     0xfffff,   0x1fffff,   0x3fffff,   0x7fffff,
    0xffffff,  0x1ffffff,  0x3ffffff,  0x7ffffff,
   0xfffffffULL, 0x1fffffffULL, 0x3fffffffULL, 0x7fffffffULL,
          0xffffffffULL,    0x1ffffffffULL,         0x3ffffffffULL,        0x7ffffffffULL,
         0xfffffffffULL,      0x1fffffffffULL,        0x3fffffffffULL,       0x7fffffffffULL,
        0xffffffffffULL,     0x1ffffffffffULL,       0x3ffffffffffULL,      0x7ffffffffffULL,
       0xfffffffffffULL,    0x1fffffffffffULL,      0x3fffffffffffULL,     0x7fffffffffffULL,
      0xffffffffffffULL,    0x1ffffffffffffULL,    0x3ffffffffffffULL,    0x7ffffffffffffULL,
     0xfffffffffffffULL,   0x1fffffffffffffULL,   0x3fffffffffffffULL,   0x7fffffffffffffULL,
    0xffffffffffffffULL,  0x1ffffffffffffffULL,  0x3ffffffffffffffULL,  0x7ffffffffffffffULL,
   0xfffffffffffffffULL, 0x1fffffffffffffffULL, 0x3fffffffffffffffULL, 0x7fffffffffffffffULL,
  0xffffffffffffffffULL
};

OODLE_NS_END
