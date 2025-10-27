// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

//#include "log2table.h"

OODLE_NS_START

//======================================================================

#define RR_LOG2TABLE_SIZE_SHIFT		(13) // 8k entries = 32 KB
//#define RR_LOG2TABLE_SIZE_SHIFT		(11) // 2k entries = 8 KB
// smaller RR_LOG2TABLE_SIZE_SHIFT does hurt compression a little
//	for Leviathan & LZNA both
// I'm not seeing much speed win for a smaller table

#define RR_LOG2TABLE_SIZE		(1<<RR_LOG2TABLE_SIZE_SHIFT)

extern const S32 c_rr_log2_table[RR_LOG2TABLE_SIZE+1]; // scaled by LZA_COST_ONE_BIT

//#include <math.h>

template <int t_one_shift>
S32 log2tabled_big(U32 x)
{
	// no interpolation, no rounding :
	//	no clz to put x in range
	RR_ASSERT( x >= 0 && x <= (1<<t_one_shift) );
	if ( t_one_shift == RR_LOG2TABLE_SIZE_SHIFT )
		return c_rr_log2_table[x];
	else if ( t_one_shift > RR_LOG2TABLE_SIZE_SHIFT )
		return c_rr_log2_table[x >> RR_MAX((t_one_shift - RR_LOG2TABLE_SIZE_SHIFT),0)];
	else
		return c_rr_log2_table[x << RR_MAX((RR_LOG2TABLE_SIZE_SHIFT - t_one_shift),0)];
}

//======================================================================

OODLE_NS_END
