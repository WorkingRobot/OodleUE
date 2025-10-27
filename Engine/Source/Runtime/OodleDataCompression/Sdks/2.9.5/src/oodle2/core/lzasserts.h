// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlebase.h"

/*******


In the past there have been asserts around the LZ code that would fire on corrupt data.

They were asserting things that were only true on valid data.

The idea was, when doing fuzz tests, run in Release so you don't hit that.  In debug you would only run on valid data, no fuzz tests.

I'm now trying to change that so I can run fuzz tests in Debug.

That means removing asserts that rely on valid data and replacing them with :

RR_ASSERT_IF_NOT_CORRUPT

if they should just be a debug assert, not a runtime check, or :

REQUIRE_FUZZ_RETURN

if they should be a runtime check.


By default RR_ASSERT_IF_NOT_CORRUPT is just a NOP because the data might be corrupt.

The idea then is that if I do hit a decode bug on *valid* data, I can then go and toggle RR_ASSERT_IF_NOT_CORRUPT to be a real assert to help in finding bugs when the decode should not be failing.

******/


// RR_ASSERT_IF_NOT_CORRUPT(exp) if exp should be true on data that's not corrupt
//	eg. on valid data, assert (exp) should be true, but on corrupt data it might not be
//	so it's not impossible to see (exp) false, but it's not good data
// g_do_lz_assert_on_corrupt_data is ON by default
// toggle g_do_lz_assert_on_corrupt_data to off when testing corrupt data
extern bool g_do_lz_assert_on_corrupt_data;
#define RR_ASSERT_IF_NOT_CORRUPT(exp)	if ( ! g_do_lz_assert_on_corrupt_data ) { } else RR_ASSERT(exp)


// REQUIRE_FUZZ_RETURN is like an assert enforced at runtime, it's an expression that must be TRUE
//	it's not like an "if" that if it's true you return,
//	eg. REQUIRE_FUZZ_RETURN( ptr <= end , NULL );   
			
#define REQUIRE_FUZZ_RETURN(expr,ret) do { RR_ASSERT_IF_NOT_CORRUPT( expr ); \
		if_unlikely ( ! (expr) ) { ooLogError("corruption : " RR_STRINGIZE(expr) "\n" ); return (ret); } \
		} while(0)
		
#define PARAMETER_CHECK(expr,ret) do { if_unlikely ( ! (expr) ) { \
		ooLogError("bad parameter : " RR_STRINGIZE(expr) "\n" ); return (ret); } \
		} while(0)
			
// break on corruption :
#define rrprintcorruption(...)	ooLogError( "LZ corruption : "  __VA_ARGS__ )
//#define rrprintcorruption	RR_ASSERT(false); rrprintf("corruption : "); rrprintf
//#define rrprintcorruption	rrPrintfKill
