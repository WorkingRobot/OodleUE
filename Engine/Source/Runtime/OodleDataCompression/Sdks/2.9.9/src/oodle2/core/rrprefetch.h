// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_PREFETCHH__
#define __RADRR_PREFETCHH__

#include "rrbase.h"

// RR_PREFETCHR_CL prefetches a cache line for reading.
// RR_PREFETCHRW_CL prefetches a cache line for read/write access.
//
// Cache lines are different sizes, so avoid using those directly.
//
// RR_PREFETCHR_nB(ptr) prefetches at least n bytes starting from ptr for reading.
// RR_PREFETCHRW_nB(ptr) prefetches at least n bytes starting from ptr for read/write access.
//
// Prefer these.

#if defined(__RADX86__)

	#if defined(__GNUC__) // Clang also sets this and gets the intrinsics right so we don't care

		#define RR_PREFETCHR_CL(ptr) __builtin_prefetch((ptr), 0)
		#define RR_PREFETCHRW_CL(ptr) __builtin_prefetch((ptr), 1)

	#else
	
		// x86/x64 have _mm_prefetch
		#include <emmintrin.h>

		#ifndef _MM_HINT_T0
		#error no HINT_T0
		#endif

		#define RR_PREFETCHR_CL(ptr) _mm_prefetch((char *)(ptr), _MM_HINT_T0)
		#define RR_PREFETCHRW_CL(ptr) _mm_prefetch((char *)(ptr), _MM_HINT_T0)

	#endif

#elif defined(__RADARM__)

	#if defined(__clang__) || (defined(__GNUC__) && !defined(__RADARM64__))

		#define RR_PREFETCHR_CL(ptr) __builtin_prefetch((ptr), 0)
		#define RR_PREFETCHRW_CL(ptr) __builtin_prefetch((ptr), 1)

	#elif defined(__GNUC__) && defined(__RADARM64__)

		// Several GCCs in the 4.x series just ignore __builtin_prefetch (!!)
		// so have to use inline ASM.
		#define RR_PREFETCHR_CL(ptr) __asm__("prfm PLDL1KEEP, [%0]" : : "r"(ptr))
		#define RR_PREFETCHRW_CL(ptr) __asm__("prfm PSTL1KEEP, [%0]" : : "r"(ptr))

	#elif defined(_MSC_VER) && defined(__RADNEON__) // not actually a NEON thing, but ARMv7 arch

		#include <arm_neon.h>

		#define RR_PREFETCHR_CL(ptr) __prefetch((ptr))
		#define RR_PREFETCHRW_CL(ptr) __prefetch((ptr)) // VC++ doesn't expose PLDW

	#elif defined(_MSC_VER) && !defined(__RADNEON__)

		// No useful prefetch available
		#define RR_PREFETCHR_CL(ptr) /*nop*/
		#define RR_PREFETCHRW_CL(ptr) /*nop*/

	#else
		
		#error Unknown ARM target

	#endif

#elif defined(__RADEMSCRIPTEN__)

	// No useful prefetch available
	#define RR_PREFETCHR_CL(ptr) /*nop*/
	#define RR_PREFETCHRW_CL(ptr) /*nop*/

#else

	#error Unsupported target ISA!

#endif

#if RR_CACHE_LINE_SIZE == 32

	#define RR_PREFETCHR_32B(ptr) RR_PREFETCHR_CL(ptr)
	#define RR_PREFETCHR_64B(ptr) RAD_STATEMENT_WRAPPER( RR_PREFETCHR_CL(ptr); RR_PREFETCHR_CL((char*)(ptr) + 32); )
	#define RR_PREFETCHR_96B(ptr) RAD_STATEMENT_WRAPPER( RR_PREFETCHR_CL(ptr); RR_PREFETCHR_CL((char*)(ptr) + 32); RR_PREFETCHR_CL((char *)(ptr) + 64); )
	#define RR_PREFETCHR_128B(ptr) RAD_STATEMENT_WRAPPER( RR_PREFETCHR_CL(ptr); RR_PREFETCHR_CL((char*)(ptr) + 32); RR_PREFETCHR_CL((char *)(ptr) + 64); RR_PREFETCHR_CL((char *)(ptr) + 96); )

	#define RR_PREFETCHRW_32B(ptr) RR_PREFETCHRW_CL(ptr)
	#define RR_PREFETCHRW_64B(ptr) RAD_STATEMENT_WRAPPER( RR_PREFETCHRW_CL(ptr); RR_PREFETCHRW_CL((char*)(ptr) + 32); )
	#define RR_PREFETCHRW_96B(ptr) RAD_STATEMENT_WRAPPER( RR_PREFETCHRW_CL(ptr); RR_PREFETCHRW_CL((char*)(ptr) + 32); RR_PREFETCHRW_CL((char *)(ptr) + 64); )
	#define RR_PREFETCHRW_128B(ptr) RAD_STATEMENT_WRAPPER( RR_PREFETCHRW_CL(ptr); RR_PREFETCHRW_CL((char*)(ptr) + 32); RR_PREFETCHRW_CL((char *)(ptr) + 64); RR_PREFETCHRW_CL((char *)(ptr) + 96); )

#elif RR_CACHE_LINE_SIZE == 64

	#define RR_PREFETCHR_32B(ptr) RR_PREFETCHR_CL(ptr)
	#define RR_PREFETCHR_64B(ptr) RR_PREFETCHR_CL(ptr)
	#define RR_PREFETCHR_96B(ptr) RAD_STATEMENT_WRAPPER( RR_PREFETCHR_CL(ptr); RR_PREFETCHR_CL((char*)(ptr) + 64); )
	#define RR_PREFETCHR_128B(ptr) RAD_STATEMENT_WRAPPER( RR_PREFETCHR_CL(ptr); RR_PREFETCHR_CL((char*)(ptr) + 64); )

	#define RR_PREFETCHRW_32B(ptr) RR_PREFETCHRW_CL(ptr)
	#define RR_PREFETCHRW_64B(ptr) RR_PREFETCHRW_CL(ptr)
	#define RR_PREFETCHRW_96B(ptr) RAD_STATEMENT_WRAPPER( RR_PREFETCHRW_CL(ptr); RR_PREFETCHRW_CL((char*)(ptr) + 64); )
	#define RR_PREFETCHRW_128B(ptr) RAD_STATEMENT_WRAPPER( RR_PREFETCHRW_CL(ptr); RR_PREFETCHRW_CL((char*)(ptr) + 64); )

#elif RR_CACHE_LINE_SIZE == 128

	#define RR_PREFETCHR_32B(ptr) RR_PREFETCHR_CL(ptr)
	#define RR_PREFETCHR_64B(ptr) RR_PREFETCHR_CL(ptr)
	#define RR_PREFETCHR_96B(ptr) RR_PREFETCHR_CL(ptr)
	#define RR_PREFETCHR_128B(ptr) RR_PREFETCHR_CL(ptr)

	#define RR_PREFETCHRW_32B(ptr) RR_PREFETCHRW_CL(ptr)
	#define RR_PREFETCHRW_64B(ptr) RR_PREFETCHRW_CL(ptr)
	#define RR_PREFETCHRW_96B(ptr) RR_PREFETCHRW_CL(ptr)
	#define RR_PREFETCHRW_128B(ptr) RR_PREFETCHRW_CL(ptr)

#else

	#error unsupported cache line size

#endif

#endif // __RADRR_TANSH__
