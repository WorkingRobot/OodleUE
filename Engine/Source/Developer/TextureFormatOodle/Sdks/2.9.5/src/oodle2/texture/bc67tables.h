// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlebase.h"

OODLE_NS_START

struct BC7OptimalEndpoint
{
	U16 err;
	U8 lo, hi;
};

extern const U8 radtex_lerp_factor[5][16];
extern const U8 radtex_anchor_2_2[64];
extern const U8 radtex_anchor_3_2[64];
extern const U8 radtex_anchor_3_3[64];

// radtex_anchor_eqv: equivalence class of anchor index positions
extern const U8 radtex_anchor_eqv[3][64]; // [ns-1][partition]

// radtex_subset_anchors : anchor pos in dense linear subset, packed into 4 bits * 2
extern const U8 radtex_subset_anchors[3][64]; // [ns-1][partition]

//extern const F32 radtex_lerp_factor_4x[5][16][4]; // [ib][ind]
extern RAD_ALIGN(const F32, radtex_lerp_factor_4x[5][16][4], 16);

extern const U8 radtex_num_pixels_per_subset[4][64][3]; // [ns][partition][subset]

// radtex_partition_order for 2 and 3 subset modes
//	partition ids in order from most useful to least
extern const U8 radtex_partition_order[2][64]; // [ns-2][index]

// radtex_section_tbl : tells you subset of each pel (2 bits * 16 in U32)
extern const U32 radtex_section_tbl[4][64]; // [ns][partition]
// radtex_subset_to_inds : index of pels in subset (4 bits * 16 in U64)
extern const U64 radtex_subset_to_inds[3][64]; // [ns-1][partition]

extern const BC7OptimalEndpoint radtex_bc7_optimal_endpoints_new[5][4][3][256]; // [mode][p_bits][index-1][256]

extern const BC7OptimalEndpoint bc1_optimal_3c[2][256]; // [chan][256] (chan: 0=red/blue, 1=green)
extern const BC7OptimalEndpoint bc1_optimal_4c[2][256]; // [chan][256] (chan: 0=red/blue, 1=green)

OODLE_NS_END

