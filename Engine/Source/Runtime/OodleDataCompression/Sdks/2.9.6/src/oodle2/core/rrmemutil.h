// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrbase.h"
#include "rrmem.h"
#include "oodlecore.h"
#include <string.h>

OODLE_NS_START

// Memset and Memcpy the fastest way you can, don't worry about rollup/down, buf is large :
//	no alignment requirement
void rrMemSetLarge(void * ptr, int val, SINTa size);
void rrMemCpyLarge(void * RADRESTRICT to,const void * RADRESTRICT from, SINTa size);

// special memset to zero :
void rrMemSetZero(void * ptr, SINTa size);

// 32 bit or 64 bit memsets :
void rrMemSet32_Aligned(void * ptr, U32 val, SINTa bytes);
void rrMemSet64_Aligned(void * ptr, U64 val, SINTa bytes);

// rrIsMemset tells you the block is all one char :
rrbool rrIsMemset(const U8 * rawBuf,SINTa rawLen);

#ifdef __RAD64__
#define rrMemSetA_Aligned rrMemSet64_Aligned
#else
#define rrMemSetA_Aligned rrMemSet32_Aligned
#endif

#ifdef __GNUC__
#define rrMemSetSmall	__builtin_memset
#define rrMemCpySmall	__builtin_memcpy
#else
//#define rrMemSetSmall	memset
//#define rrMemCpySmall	memcpy

static RADFORCEINLINE void rrMemSetSmall(void * ptr, int val, SINTa size)
{
	U8 * pto = (U8 *) ptr;
	SINTa i;
	for(i=0;i<size;i++)
		pto[i] = (U8)val;
}

//#pragma RR_PRAGMA_MESSAGE("rrMemCpySmall")

static RADFORCEINLINE void rrMemCpySmall(void * RADRESTRICT to,const void * RADRESTRICT from, SINTa size)
{
	U8 * RADRESTRICT pto = (U8 * RADRESTRICT) to;
	const U8 * RADRESTRICT pfm = (const U8 * RADRESTRICT) from;
	SINTa i;
	for(i=0;i<size;i++)
		pto[i] = pfm[i];
}

#endif

// RR_ZERO is better :
#define RR_ZERO(obj)	do { rrMemSetZero(&(obj),sizeof(obj)); } while(0)

OODLE_NS_END

