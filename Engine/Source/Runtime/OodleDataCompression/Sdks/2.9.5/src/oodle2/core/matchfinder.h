// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrbase.h"
#include "oodlecore.h"

OODLE_NS_START

struct UnpackedMatchPair
{
	S32	length,offset;
};

struct UnpackedMatchPair_Sort_HighLength_then_LowOffset
{
	bool operator () (const UnpackedMatchPair & lhs,const UnpackedMatchPair & rhs) const
	{
		if ( lhs.length != rhs.length )
		{
			return ( lhs.length > rhs.length );
		}
		return ( lhs.offset < rhs.offset );
	}
};

struct UnpackedMatchPair_Equals
{
	bool operator () (const UnpackedMatchPair & lhs,const UnpackedMatchPair & rhs) const
	{
		return lhs.offset == rhs.offset && lhs.length == rhs.length;
	}
};

struct UnpackedMatchPair_EqualLength
{
	bool operator () (const UnpackedMatchPair & lhs,const UnpackedMatchPair & rhs) const
	{
		return lhs.length == rhs.length;
	}
};

// Incremental match finder interface
class IncrementalMatchFinder
{
public:
	virtual ~IncrementalMatchFinder() { };

	virtual int ProcessChunk(int chunkSize, UnpackedMatchPair * matches, int maxPairs) = 0;
};

OODLE_NS_END
