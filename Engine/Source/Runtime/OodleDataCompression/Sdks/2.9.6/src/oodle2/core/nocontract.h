// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// Use this header file to disable FP contraction, which allows compilers to
// automatically turn multiplies followed by additions into fused
// multiply-adds where available. This results in different results when
// FMAs are available (e.g. when compiling on x86 with AVX2 support) than
// when they are not, which breaks our compatibility guarantees.
//
// If you want FMAs in Oodle code, you need to write them directly. (And be
// very careful to not introduce mismatches between build targets with
// and without FMA support.)

#if defined(_MSC_VER) && !defined(__clang__)
#pragma fp_contract(off)
#else
#pragma STDC FP_CONTRACT OFF
#endif

