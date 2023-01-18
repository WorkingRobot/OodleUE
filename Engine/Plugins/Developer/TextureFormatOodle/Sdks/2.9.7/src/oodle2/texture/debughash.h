// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlebase.h"

OODLE_NS_START

// Very simple hash fn intended for easy debugging. There are better hashes
// for when we actually want to hash data in the encoder but this fits the
// bill for an OK, tiny hash function for when you want to do a quick sanity
// check that data matches.
static inline U64 simple_debug_hash(const void * data, SINTa size_bytes)
{
	// FNV-1a with default primes
	const U8* bytes = (const U8 *)data;
	const U64 fnv_prime = 0x100000001b3ull;
	U64 hash = 0xcbf29ce484222325ull;

	for (SINTa i = 0; i < size_bytes; ++i)
		hash = (hash ^ bytes[i]) * fnv_prime;

	return hash;
}

OODLE_NS_END

