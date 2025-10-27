// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "bc67tables.h"

OODLE_NS_START

// NOTE: these are the hand-written tables; most of the tables
// are auto-generated and live in bc67tables_generated.cpp

RAD_ALIGN(const U8, radtex_lerp_factor[5][16], 16) =
{
	{},{},// 0,1 bits -- never used. undefined
	{0, 21, 43, 64},
	{0, 9, 18, 27, 37, 46, 55, 64},
	{0, 4, 9, 13, 17, 21, 26, 30, 34, 38, 43, 47, 51, 55, 60, 64}
};

const U8 radtex_anchor_2_2[64] =
{
	15,15,15,15,15,15,15,15,
	15,15,15,15,15,15,15,15,
	15, 2, 8, 2, 2, 8, 8,15,
	 2, 8, 2, 2, 8, 8, 2, 2,
	15,15, 6, 8, 2, 8,15,15,
	 2, 8, 2, 2, 2,15,15, 6,
	 6, 2, 6, 8,15,15, 2, 2,
	15,15,15,15,15, 2, 2,15,
};

const U8 radtex_anchor_3_2[64] =
{
	 3, 3,15,15, 8, 3,15,15,
	 8, 8, 6, 6, 6, 5, 3, 3,
	 3, 3, 8,15, 3, 3, 6,10,
	 5, 8, 8, 6, 8, 5,15,15,
	 8,15, 3, 5, 6,10, 8,15,
	15, 3,15, 5,15,15,15,15,
	 3,15, 5, 5, 5, 8, 5,10,
	 5,10, 8,13,15,12, 3, 3,
};

const U8 radtex_anchor_3_3[64] =
{
	15, 8, 8, 3,15,15, 3, 8,
	15,15,15,15,15,15,15, 8,
	15, 8,15, 3,15, 8,15, 8,
	 3,15, 6,10,15,15,10, 8,
	15, 3,15,10,10, 8, 9,10,
	 6,15, 8,15, 3, 6, 6, 8,
	15, 3,15,15,15,15,15,15,
	15,15,15,15, 3,15,15, 8,
};

OODLE_NS_END

