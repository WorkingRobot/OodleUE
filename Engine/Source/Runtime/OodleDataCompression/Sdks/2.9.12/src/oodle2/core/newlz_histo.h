// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "histogram.h"

OODLE_NS_START

struct OodleSpeedFit;

// newlz_array_estimate_complen_bits includes an estimate of codelen transmission cost
// note this is in actual bits
//	unlike the "codelen" stuff above that is scaled up by RR_LOG2TABLE_ONE_SHIFT
U32 histo_estimate_complen_bits(const Histo256 & histo,SINTa sumCounts);
F32 histo_estimate_J(const Histo256 & histo,SINTa sumCounts,F32 lambda,const OodleSpeedFit * speedfit);

OODLE_NS_END
