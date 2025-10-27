// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "vec256.inl"

OODLE_NS_START

#ifdef DO_BUILD_AVX512

typedef __m512i Vec512;
typedef __m512i const & Vec512r;

// No load/store wrappers; the official intrinsics (at long last) take void* now,
// and there are more variants (due to masking etc.) which would increase the API
// surface a lot, now to not actually do anything, since the purpose of the original
// loadN/storeN funcs was just to handle the type casting.

static inline Vec256 lo_half(Vec512 v)								{ return _mm512_castsi512_si256(v); }
static inline Vec256 hi_half(Vec512 v)								{ return _mm512_extracti32x8_epi32(v, 1); }
static inline Vec512 combine(Vec256 a, Vec256 b)					{ return _mm512_inserti32x8(_mm512_castsi256_si512(a), b, 1); }

static inline __m256 lo_half(__m512 v)								{ return _mm512_castps512_ps256(v); }
static inline __m256 hi_half(__m512 v)								{ return _mm512_extractf32x8_ps(v, 1); }
static inline __m512 combine(__m256 a, __m256 b)					{ return _mm512_insertf32x8(_mm512_castps256_ps512(a), b, 1); }

template<int x,int y,int z,int w>
static inline Vec512 shuffle32in128(Vec512 v)						{ return _mm512_shuffle_epi32(v, _MM_SHUFFLE(w,z,y,x)); }

static inline Vec512 broadcast128_512(Vec128 v)						{ return _mm512_broadcast_i32x4(v); }
static inline Vec512 broadcast256_512(Vec256 v)						{ return _mm512_broadcast_i32x8(v); }
template<int i>
static inline Vec512 broadcast128in512(Vec512 v)					{ return _mm512_shuffle_i32x4(v, v, _MM_SHUFFLE(i,i,i,i)); }

// Ternary logic constants
// do bit math between these to get the constant for the corresponding func
// i.e. (TERNLOG_A | TERNLOG_B) ^ TERNLOG_C selects bitwise op (a | b) ^ c
#define TERNLOG_A 0xF0
#define TERNLOG_B 0xCC
#define TERNLOG_C 0xAA

#endif

OODLE_NS_END

