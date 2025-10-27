// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlebase.h"
#include "cpudispatch.h"

OODLE_NS_START

enum class BC7SubsetChan
{
	OverallErr = 0,		// Overall squared error for replacing block by its mean
	PcaErr,				// Squared error accounted for by principal axis
	PcaR,				// PCA axis split into R, G, B, A components
	PcaG,
	PcaB,
	PcaA,

	Count
};

class BC7SubsetAnalysis
{
	static const SINTa num_subsets = 248; // max number of subsets we ever have 
	static const SINTa stride = num_subsets + 8; // extra padding (for 8-wide vectors) so we can write sloppily

	F32 storage[static_cast<SINTa>(BC7SubsetChan::Count) * stride];

public:
	// subset_count <= stride required!
	void compute(const U8 pixels_rgba[], const U32 subset_masks[], const F32 subset_scale[], SINTa subset_count, bool is_rgba, CpuDispatchFlags dispatch);

	// Accessor for results. analysis[channel][i]
	const F32 * operator[](BC7SubsetChan channel) const		{ return &storage[static_cast<SINTa>(channel) * stride]; }
	F32 * operator[](BC7SubsetChan channel)					{ return &storage[static_cast<SINTa>(channel) * stride]; }
};

// do not call directly!
namespace internal {
	void subset_prepare_avx2(F32 * results, const U8 pixels_rgba[], const U32 subset_masks[], const F32 subset_scale[], SINTa subset_count, SINTa stride, bool is_rgba);
}

OODLE_NS_END

