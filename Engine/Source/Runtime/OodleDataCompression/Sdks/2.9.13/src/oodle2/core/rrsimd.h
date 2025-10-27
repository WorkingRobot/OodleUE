// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrbase.h"
#include "cpux86.h"

//=========

#if defined(__RADSSE2__)

#if defined(__RADJAGUAR__) || defined(__RADZEN2__)

// Always enable SSE 4.1
#ifndef __SSE4_1__
#define __SSE4_1__
#endif

#endif

#if defined(_MSC_VER)
#pragma warning(disable : 4324) // structure was padded due to __declspec(align())
#endif

// don't build AVX2 when we explicitly disabled in build
#if !defined(__RADNOAVX2__)
#define DO_BUILD_AVX2
#endif

// Only build AVX512 if enabled and AVX2 is not disabled
#if !defined(__RADNOAVX512__) && !defined(__RADNOAVX2__)
#define DO_BUILD_AVX512
#endif

#if defined(_MSC_VER) || defined(__SSSE3__)
#define DO_BUILD_SSSE3
#endif

#if defined(_MSC_VER) || defined(__SSE4_1__)
#define DO_BUILD_SSE4
#endif

#if defined(DO_BUILD_AVX2)
#include <immintrin.h> // AVX/AVX2/FMA
#elif defined(DO_BUILD_SSE4)
#include <smmintrin.h> // SSE4.1
#elif defined(DO_BUILD_SSSE3)
#include <tmmintrin.h>
#else
#include <emmintrin.h> // SSE2
#endif

#elif defined(__RADNEON__)

#include <arm_neon.h>

#define DO_BUILD_NEON

#ifdef __RAD64__
#define DO_BUILD_NEON64
#endif

#if !defined(__RADNOARMDP__)
#define DO_BUILD_ARM_DOTPROD
#endif

#elif defined(__RADWASM_SIMD128__)

#include <wasm_simd128.h>

#else

// other vector ISAs?

#endif

//=========

OODLE_NS_START

//=========

#if (defined(__RADNT__) || defined(__RADMAC__) || defined(__RADLINUX__) || defined(__RADWINRT__))

#ifdef DO_BUILD_SSE4

// platforms where we test for SSE4 and fallback if not found

#define DO_SSE4_TEST

//#pragma message("DO_SSE4_TEST")

static rrbool rrsimd_has_sse4()
{
    return rrCPUx86_feature_present(RRX86_CPU_SSE41);
}

#else

// Linux and Mac ARM

static rrbool rrsimd_has_sse4()
{
    return false;
}

#endif // DO_BUILD_SSE4

#elif defined(__RADJAGUAR__) || defined(__RADZEN2__)

// platforms where we know we have a speciifc CPU and don't need a fallback

#define DO_SSE4_ALWAYS

static rrbool rrsimd_has_sse4()
{
    return true;
}

#else

static rrbool rrsimd_has_sse4()
{
    return false;
}

#endif

// Update this part with AVX2 feature detection once we're on compilers
// that actually support it for our Windows/Linux/Mac builds, but right
// now we only have this on console targets.
//
// Detecting this and using AVX2 automatically is a problem because of
// Intel's downclocking shenanigans; don't want to use AVX2 in a loop
// that runs for a sub-millisecond and force the whole package into
// a lower-frequency power state as a result when the app is running.
//
// Our test workloads have hit that - be very very careful here.
// (It's a lot less problematic with Zens which don't have any
// drastic clock rate changes from AVX2 usage.)

// -> DO_AVX2_ALWAYS + DO_AVX2_TEST

#if defined(__RADZEN2__)

#define DO_AVX2_ALWAYS

static rrbool rrsimd_has_avx2()
{
    return true;
}

#else

static rrbool rrsimd_has_avx2()
{
    return false;
}

#endif

OODLE_NS_END
