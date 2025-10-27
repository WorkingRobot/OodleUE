// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlebase.h"

OODLE_NS_START

void OodleNetwork2_Test(
		const void ** train_packet_pointers, 
		const S32 * train_packet_sizes,
		S32 num_train_packets,
		const void ** test_packet_pointers, 
		const S32 * test_packet_sizes,
		S32 num_test_packets
		);

OODLE_NS_END

		