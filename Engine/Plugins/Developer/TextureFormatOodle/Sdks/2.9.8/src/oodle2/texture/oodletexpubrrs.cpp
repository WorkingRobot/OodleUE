// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "oodletexpubrrs.h"
#include "cbstrutil.h"

// implementation is in oodletexpub

OODLE_NS_START

struct OodleTex_BC_Name
{
	const char * str;
	OodleTex_BC bcn;
};

// this is only for internal utilities, not for public use

static OodleTex_BC_Name bc_names[] = {
	{ "bc1", OodleTex_BC1 },
	{ "bc1a", OodleTex_BC1_WithTransparency },
	{ "bc1t", OodleTex_BC1_WithTransparency },
	{ "bc2", OodleTex_BC2 },
	{ "bc3", OodleTex_BC3 },
	{ "bc4", OodleTex_BC4U },
	{ "bc4u", OodleTex_BC4U },
	{ "bc4s", OodleTex_BC4S },
	{ "bc5", OodleTex_BC5U },
	{ "bc5u", OodleTex_BC5U },
	{ "bc5s", OodleTex_BC5S },
	{ "bc6", OodleTex_BC6U },
	{ "bc6u", OodleTex_BC6U },
	{ "bc6s", OodleTex_BC6S },
	{ "bc7", OodleTex_BC7RGBA },
	{ "bc7rgba", OodleTex_BC7RGBA },
	{ "bc7rgb", OodleTex_BC7RGB }
};

static int bc_names_count = RR_ARRAY_SIZE(bc_names);

OodleTex_BC string_to_bc(const char * str)
{
	for LOOP(i,bc_names_count)
	{
		if ( strisame(str,bc_names[i].str) )
			return bc_names[i].bcn;
		// +2 to skip "bc"
		if ( strisame(str,bc_names[i].str+2) )
			return bc_names[i].bcn;
	}
	return OodleTex_BC_Invalid;
}

OODLE_NS_END
