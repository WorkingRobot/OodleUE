// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrbase.h"
#include "rrdxtcenums.h"

RR_NAMESPACE_START

// CPU dispatch flags for BCn encoder kernels. We just need a handful of variants
// currently, and we want to keep it compact so we can pass it around by value
// cheaply.
struct CpuDispatchFlags
{
	U32 flags;

	// This is the intended way to construct this, not using a regular ctor
	// to avoid anything implicit happening.
	static CpuDispatchFlags init(const rrDXTCOptions* pOpts = 0);

	CpuDispatchFlags() : flags(0) {}

	// Accessors
	bool AVX2() const			{ return (flags & FLAG_AVX2) != 0; }
	bool AVX256() const			{ return (flags & FLAG_AVX256) != 0; } // AVX512 ISA, only use 256-bit vectors
	bool AVX512() const			{ return (flags & FLAG_AVX512) != 0; }
	bool ARM_DotProd() const	{ return (flags & FLAG_ARM_DOTPROD) != 0; } // ARMv8.2 UDOT/SDOT

private:
	enum
	{
		FLAG_AVX2 = 1<<0,
		FLAG_AVX256 = 1<<1,
		FLAG_AVX512 = 1<<2,
		FLAG_ARM_DOTPROD = 1<<3,
	};

	CpuDispatchFlags(U32 value) : flags(value) {}
};

RR_NAMESPACE_END

