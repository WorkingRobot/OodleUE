#include "cpudispatch.h"
#include "rrsimd.h"
#include "cpux86.h"
#include "rrdxtcenums.h"

RR_NAMESPACE_START

CpuDispatchFlags CpuDispatchFlags::init(const rrDXTCOptions* pOpts)
{
	U32 flags = 0;

#ifdef DO_BUILD_AVX2
	if (rrCPUx86_feature_present(RRX86_CPU_AVX2))	flags |= FLAG_AVX2;
#endif

#ifdef DO_BUILD_AVX512
	if (rrCPUx86_feature_present(RRX86_CPU_AVX512))
	{
		// If AVX-512 is present, can always use 256-bit vector variants safely
		flags |= FLAG_AVX256;

		// Use 512-bit vectors when our rules say it's preferable
		if (!(*pOpts & rrDXTCOptions_AvoidWideVectors))
		{
			if ((*pOpts & rrDXTCOptions_PreferWideVectors) || rrCPUx86_feature_present(RRX86_CPU_PREFER512))
			{
				flags |= FLAG_AVX512;
			}
		}
	}
#endif

	return CpuDispatchFlags(flags);
}

RR_NAMESPACE_END

