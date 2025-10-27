// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrbase.h"
#include "rrdxtcenums.h"

RR_NAMESPACE_START

struct BlockSurface;

bool BC6_RD(BlockSurface * to_blocks,
	const BlockSurface * from_blocks,
	const BlockSurface * baseline_blocks,
	const BlockSurface * activity_blocks,
	int lambda,
	rrDXTCOptions options,
	const rrDXTCRD_Options & rdopts);

RR_NAMESPACE_END
