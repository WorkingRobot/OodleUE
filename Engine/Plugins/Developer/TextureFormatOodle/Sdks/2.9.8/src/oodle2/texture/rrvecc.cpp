// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrvecc.h"
#include <math.h>

EXPORT_SOME_CRAP(rrVecC);

RR_NAMESPACE_START

//WAS rsqrtss + Newton Raphson
// 11-04-2020 : changed for 2.8.13
//	this caused a difference between Intel & AMD encodings of BC1 RDO
//	the fixed version is different than previous on either processor
F32 rrRecipSqrt(F32 x)
{
    RR_ASSERT( x > 0.f );

	// NOTE(fg): may not use rsqrt on any x86, because the instructions are underspecified.
	// Namely, implementations are only required to satisfy a given error bound and they
	// are _not_ the same between different chips, although mostly Intel and AMD have been
	// staying with their respective approximations of choice for a while now.
	//
	// Either way, we can't use this and still satisfy our binary reproducibility guarantees
	// for Oodle Texture. It would have to be either our own (known) approximation or we
	// just do the obvious thing, which is 1 / sqrt(x) - both fully IEEE specified ops.

    return 1.f / sqrtf(x);
}

RR_NAMESPACE_END
