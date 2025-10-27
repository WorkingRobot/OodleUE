// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "newlz_histo.h"
#include "newlz_speedfit.h"
#include "newlz_shared.h"

OODLE_NS_START

// newlz_array_estimate_complen_bits includes an estimate of codelen transmission cost
// note this is in actual bits
//	unlike the "codelen" stuff above that is scaled up by RR_LOG2TABLE_ONE_SHIFT
U32 histo_estimate_complen_bits(const Histo256 & histo,SINTa sumCounts)
{
	// -> this is the cost func for the N^2 merger in the trellis iteration
	//SIMPLEPROFILE_SCOPE(histo_estimate_complen_bits);

	// newlz_array_estimate_complen_bits includes header bytes
	//	(eg. 3 or 5 byte newlz_array header)
	// that will make you strongly favor merges
	return newlz_array_estimate_complen_bits(histo.counts,256,(U32)sumCounts);
}

F32 histo_estimate_J(const Histo256 & histo,SINTa sumCounts,F32 lambda,const OodleSpeedFit * speedfit)
{
	U32 bits = newlz_array_estimate_complen_bits(histo.counts,256,(U32)sumCounts);
	// J is in bytes
	F32 J = (bits/8.f) + lambda * speedfit_estimate_entropy_array_time(speedfit,sumCounts);
	return J;
}

OODLE_NS_END

