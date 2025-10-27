// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_MEMH__
#define __RADRR_MEMH__

#include "rrbase.h"
#include <string.h> // need memcpy

//===================================================================================

#if defined(__RADWINRTAPI__) && defined(__RADARM__)

#include <intrin.h>

#endif

//=======================================================

RR_NAMESPACE_START

//===========================================================
//
// Get/Put from memory in little or big endian :
//
// val = RR_GET32_BE(ptr)
// RR_PUT32_BE(ptr,val)
//
//  available here :
//		RR_[GET/PUT][16/32/64]_[BE/LE]
//
// CB new : Jan 2017 : these all work on unaligned pointers!

// native-endian version of get/put  :

#if 0 // raw value cast way for comparison

#define RR_GET16_NATIVE(ptr)     (*((const U16 * )(ptr)))
#define RR_PUT16_NATIVE(ptr,val) *((U16 * )(ptr)) = (val)

#define RR_GET32_NATIVE(ptr)     (*((const U32 * )(ptr)))
#define RR_PUT32_NATIVE(ptr,val) *((U32 * )(ptr)) = (val)

#define RR_GET64_NATIVE(ptr)     (*((const U64 * )(ptr)))
#define RR_PUT64_NATIVE(ptr,val) *((U64 * )(ptr)) = (val)

#else

// packed structs have alignof()==1 so can be used unaligned
RR_PACKED_STRUCT_START(packed_U16) { U16 m; } RR_PACKED_STRUCT_END;
RR_PACKED_STRUCT_START(packed_U32) { U32 m; } RR_PACKED_STRUCT_END;
RR_PACKED_STRUCT_START(packed_U64) { U64 m; } RR_PACKED_STRUCT_END;

#ifdef _DEBUG
// in debug, avoid memcpy ; note that the puts intentionally don't cast val

#define RR_GET16_NATIVE(ptr)     (((const packed_U16 * )(ptr))->m)
#define RR_PUT16_NATIVE(ptr,val) ((packed_U16 * )(ptr))->m = (val)

#define RR_GET32_NATIVE(ptr)     (((const packed_U32 * )(ptr))->m)
#define RR_PUT32_NATIVE(ptr,val) ((packed_U32 * )(ptr))->m = (val)

#define RR_GET64_NATIVE(ptr)     (((const packed_U64 * )(ptr))->m)
#define RR_PUT64_NATIVE(ptr,val) ((packed_U64 * )(ptr))->m = (val)

#else
// in release, use memcpy

static RADFORCEINLINE U16 RR_GET16_NATIVE(const void * ptr) { U16 ret; memcpy(&ret,ptr,sizeof(ret)); return ret; }
static RADFORCEINLINE U32 RR_GET32_NATIVE(const void * ptr) { U32 ret; memcpy(&ret,ptr,sizeof(ret)); return ret; }
static RADFORCEINLINE U64 RR_GET64_NATIVE(const void * ptr) { U64 ret; memcpy(&ret,ptr,sizeof(ret)); return ret; }

static RADFORCEINLINE void RR_PUT16_NATIVE(void *ptr,U16 val) { memcpy(ptr,&val,sizeof(val)); }
static RADFORCEINLINE void RR_PUT32_NATIVE(void *ptr,U32 val) { memcpy(ptr,&val,sizeof(val)); }
static RADFORCEINLINE void RR_PUT64_NATIVE(void *ptr,U64 val) { memcpy(ptr,&val,sizeof(val)); }

#endif

#endif

//---------------------------------------------------
// map NATIVE to either LE or BE :

#ifdef __RADLITTLEENDIAN__

#define RR_GET16_LE     RR_GET16_NATIVE
#define RR_PUT16_LE     RR_PUT16_NATIVE

#define RR_GET32_LE     RR_GET32_NATIVE
#define RR_PUT32_LE     RR_PUT32_NATIVE

#define RR_GET64_LE     RR_GET64_NATIVE
#define RR_PUT64_LE     RR_PUT64_NATIVE

#else

#define RR_GET16_BE     RR_GET16_NATIVE
#define RR_PUT16_BE     RR_PUT16_NATIVE

#define RR_GET32_BE     RR_GET32_NATIVE
#define RR_PUT32_BE     RR_PUT32_NATIVE

#define RR_GET64_BE     RR_GET64_NATIVE
#define RR_PUT64_BE     RR_PUT64_NATIVE

#endif

//=============================================================================
// helper to offset pointers :

#define RR_PTR_OFFSET(ptr,offset) ((void *)((char *)(ptr) + (offset)))

//=============================================================================
// get BSWAP macros :

// fall back :
#define RR_BSWAP16_FALLBACK(u16) ( (U16) ( ((u16) >> 8) | ((u16) << 8) ) )
#define RR_BSWAP32_FALLBACK(u32) ( (U32) ( ((u32) >> 24) | (((u32)<<8) & 0x00FF0000) | (((u32)>>8) & 0x0000FF00) | ((u32) << 24) ) )
#define RR_BSWAP64_FALLBACK(u64) ( ( (U64) RR_BSWAP32_FALLBACK((U32)(u64)) << 32 ) | RR_BSWAP32_FALLBACK((U32)((u64)>>32)) )

//=============================================================================

#if defined(_MSC_VER)

RR_NAMESPACE_END

RADDEFFUNC unsigned short __cdecl _byteswap_ushort (unsigned short _Short);
RADDEFFUNC unsigned long  __cdecl _byteswap_ulong  (unsigned long  _Long);
RADDEFFUNC unsigned __int64 __cdecl _byteswap_uint64 (unsigned __int64 val);
#pragma intrinsic(_byteswap_ushort, _byteswap_ulong)
#pragma intrinsic(_byteswap_uint64)

RR_NAMESPACE_START

#define RR_BSWAP16  _byteswap_ushort
#define RR_BSWAP32  _byteswap_ulong
#define RR_BSWAP64  _byteswap_uint64

#elif defined(__GNUC__) && (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 3))

// bswap16 starts in GCC 4.8 :
#if (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 8))
#define RR_BSWAP16  __builtin_bswap16
#else
#define RR_BSWAP16 RR_BSWAP16_FALLBACK
#endif

#define RR_BSWAP32  __builtin_bswap32
#define RR_BSWAP64  __builtin_bswap64

#elif defined(__clang__)

// GCC has __builtin_bswap16, but Clang only seems to have added it recently.
// We use __builtin_bswap32/64 but 16 just uses the macro version. (No big
// deal if that turns into shifts anyway)
#define RR_BSWAP16 RR_BSWAP16_FALLBACK
#define RR_BSWAP32  __builtin_bswap32
#define RR_BSWAP64  __builtin_bswap64

#else

#define RR_BSWAP16  RR_BSWAP16_FALLBACK
#define RR_BSWAP32  RR_BSWAP32_FALLBACK
#define RR_BSWAP64  RR_BSWAP64_FALLBACK

#endif

//===============================================
// Standard implementation of non-native-endian Get/Put via bswap :

#if defined(__RADLITTLEENDIAN__)
// implementation for X86 using bswaps :

#define RR_GET16_BE(ptr)        RR_BSWAP16( RR_GET16_NATIVE(ptr) )
#define RR_PUT16_BE(ptr,val)    RR_PUT16_NATIVE(ptr,RR_BSWAP16(val))

#define RR_GET32_BE(ptr)        RR_BSWAP32( RR_GET32_NATIVE(ptr) )
#define RR_PUT32_BE(ptr,val)    RR_PUT32_NATIVE(ptr,RR_BSWAP32(val))

#define RR_GET64_BE(ptr)        RR_BSWAP64( RR_GET64_NATIVE(ptr) )
#define RR_PUT64_BE(ptr,val)    RR_PUT64_NATIVE(ptr,RR_BSWAP64(val))

#else

#define RR_GET16_LE(ptr)        RR_BSWAP16( RR_GET16_NATIVE(ptr) )
#define RR_PUT16_LE(ptr,val)    RR_PUT16_NATIVE(ptr,RR_BSWAP16(val))

#define RR_GET32_LE(ptr)        RR_BSWAP32( RR_GET32_NATIVE(ptr) )
#define RR_PUT32_LE(ptr,val)    RR_PUT32_NATIVE(ptr,RR_BSWAP32(val))

#define RR_GET64_LE(ptr)        RR_BSWAP64( RR_GET64_NATIVE(ptr) )
#define RR_PUT64_LE(ptr,val)    RR_PUT64_NATIVE(ptr,RR_BSWAP64(val))

#endif

//===================================================================

// OLD UNALIGNED VERSIONS :
// just map them for now :

#define RR_GET64_BE_UNALIGNED(ptr)                 RR_GET64_BE(ptr)
#define RR_GET64_LE_UNALIGNED(ptr)                 RR_GET64_LE(ptr)
#define RR_PUT64_BE_UNALIGNED(ptr,val)             RR_PUT64_BE(ptr,val)
#define RR_PUT64_LE_UNALIGNED(ptr,val)             RR_PUT64_LE(ptr,val)

#define RR_GET32_BE_UNALIGNED(ptr)                 RR_GET32_BE(ptr)
#define RR_GET32_LE_UNALIGNED(ptr)                 RR_GET32_LE(ptr)
#define RR_PUT32_BE_UNALIGNED(ptr,val)             RR_PUT32_BE(ptr,val)
#define RR_PUT32_LE_UNALIGNED(ptr,val)             RR_PUT32_LE(ptr,val)

#define RR_GET16_LE_UNALIGNED(ptr)                 RR_GET16_LE(ptr)
#define RR_GET16_BE_UNALIGNED(ptr)                 RR_GET16_BE(ptr)
#define RR_PUT16_BE_UNALIGNED(ptr,val)             RR_PUT16_BE(ptr,val)
#define RR_PUT16_LE_UNALIGNED(ptr,val)             RR_PUT16_LE(ptr,val)

#define RR_GET64_NATIVE_UNALIGNED(ptr)                 RR_GET64_NATIVE(ptr)
#define RR_PUT64_NATIVE_UNALIGNED(ptr,val)             RR_PUT64_NATIVE(ptr,val)

#define RR_GET32_NATIVE_UNALIGNED(ptr)                 RR_GET32_NATIVE(ptr)
#define RR_PUT32_NATIVE_UNALIGNED(ptr,val)             RR_PUT32_NATIVE(ptr,val)

#define RR_GET16_NATIVE_UNALIGNED(ptr)                 RR_GET16_NATIVE(ptr)
#define RR_PUT16_NATIVE_UNALIGNED(ptr,val)             RR_PUT16_NATIVE(ptr,val)

//===================================================================

// you generally want the GET24 OVERRUNOK variants unless you specifically can't overrun
#define RR_GET24_BE_NOOVERRUN(ptr) ( \
    ( (U32)(((const U8 * )(ptr)))[0] << 16 ) | \
    ( (U32)(((const U8 * )(ptr)))[1] << 8  ) | \
    ( (U32)(((const U8 * )(ptr)))[2] << 0  ) )

#define RR_GET24_LE_NOOVERRUN(ptr) ( \
    ( (U32)(((const U8 * )(ptr)))[2] << 16 ) | \
    ( (U32)(((const U8 * )(ptr)))[1] << 8  ) | \
    ( (U32)(((const U8 * )(ptr)))[0] << 0  ) )

// if over-reading is okay, we can just grab a 32 and adjust it :
#define RR_GET24_BE_OVERRUNOK(ptr)		( RR_GET32_BE(ptr) >> 8 )
#define RR_GET24_LE_OVERRUNOK(ptr)		( RR_GET32_LE(ptr) & 0xFFFFFF )

#define RR_PUT24_LE_NOOVERRUN(to,val) do { ((U8 *)(to))[0] = (U8)(val); ((U8 *)(to))[1] = (U8)((val)>>8); ((U8 *)(to))[2] = (U8)((val)>>16); } while(0)
#define RR_PUT24_BE_NOOVERRUN(to,val) do { ((U8 *)(to))[0] = (U8)((val)>>16); ((U8 *)(to))[1] = (U8)((val)>>8); ((U8 *)(to))[2] = (U8)(val); } while(0)

#define RR_PUT24_LE_OVERRUNOK(ptr,val)	RR_PUT32_LE(ptr,val)
#define RR_PUT24_BE_OVERRUNOK(ptr,val)	RR_PUT32_BE(ptr,((val)<<8))

//=====================================================================
// GETR / PUTR = Get reg size ; UINTr  (not pointer size)

#ifdef __RAD64REGS__

#define RR_GETR_LE	RR_GET64_LE
#define RR_GETR_BE	RR_GET64_BE
#define RR_GETR_NATIVE	RR_GET64_NATIVE

#define RR_PUTR_LE	RR_PUT64_LE
#define RR_PUTR_BE	RR_PUT64_BE
#define RR_PUTR_NATIVE	RR_PUT64_NATIVE

#else

#define RR_GETR_LE	RR_GET32_LE
#define RR_GETR_BE	RR_GET32_BE
#define RR_GETR_NATIVE	RR_GET32_NATIVE

#define RR_PUTR_LE	RR_PUT32_LE
#define RR_PUTR_BE	RR_PUT32_BE
#define RR_PUTR_NATIVE	RR_PUT32_NATIVE

#endif // __RAD64REGS__

//=====================================================================

RR_NAMESPACE_END

//===================================================================================


#endif // __RADRR_MEMH__
