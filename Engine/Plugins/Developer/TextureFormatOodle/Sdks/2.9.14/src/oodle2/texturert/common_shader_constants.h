// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#define BC7PREP_NUM_MODES					10
#define BC7PREP_INITIAL_COUNT_THREADS		64
#define BC7PREP_SORT_BLOCK_SIZE				512

// Flags for BC7Prep decode global_flags
#define BC7PREP_DECODE_SWITCH_COLORSPACE	(1<<0)
#define BC7PREP_DECODE_DEBUG_SORT			(1<<1)

