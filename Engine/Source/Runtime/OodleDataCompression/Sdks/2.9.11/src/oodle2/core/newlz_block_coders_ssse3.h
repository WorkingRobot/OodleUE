// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlebase.h"

OODLE_NS_START

struct BlockBitReader;

bool newLZ_decode_alphabet_shape_run_pairs_ssse3(U16 * runLens, BlockBitReader * bbr, const U8 * run_prefix, U32 num_run_pairs);

extern bool g_has_newlz_decode_alphabet_shape_run_pairs_ssse3;

OODLE_NS_END
