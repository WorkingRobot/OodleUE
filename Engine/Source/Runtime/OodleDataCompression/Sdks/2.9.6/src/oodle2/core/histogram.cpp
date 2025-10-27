// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "histogram.h"
#include "oodlecore.h"
#include "rrmem.h"
#include "log2table.h"
#include "newlz_simd.h"
#include "cbradutil.h"

#ifdef OODLE_BUILDING_DATA
//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"
#include "histogram_asm.h"
#else
// oo2net never has simpleprof
#include "rrsimpleprofstub.h"
#endif

#if defined(OODLE_HISTO_A64_ASM) && !defined(__RADARM64__)
// on iOS, we have the same command line switches for ARM32
// and 64, but we can't use the A64 ASM on 64-bit.
#undef OODLE_HISTO_A64_ASM
#endif

OODLE_NS_START

U64 CodeLenOfArrayU8(const U8 * rawArray,SINTa rawLen)
{
	U32 histo[256];
	CountHistoArrayU8(rawArray,rawLen,histo,256);
	return rrCodeLenOfHistogramT(histo,256,(U32)rawLen);
}

void CountHistoArrayU8_Simple
	(const U8 * rawArray,SINTa rawLen,
	U32 * counts,int numSymbols,bool addOn)
{
	RR_ASSERT( numSymbols <= 256 ); // it better be for U8!
	
	if ( ! addOn )
	{
		// zero counts
		memset(counts,0,sizeof(U32)*numSymbols);
	}
	
	const U8 * rawPtr = rawArray;
	const U8 * rawEnd = rawArray+rawLen;
	
	#if 0
	
	while(rawPtr < rawEnd)
	{
		counts[ *rawPtr++ ] ++;
	}
	
	#else
	
	// do fours with U32 load, then tail
	
	const U8 * rawEndMul4 = rawArray+(rawLen&~3);
	
	while(rawPtr < rawEndMul4)
	{
		U32 x = RR_GET32_NATIVE_UNALIGNED(rawPtr);
		counts[ x & 0xff ] ++; x >>= 8;
		counts[ x & 0xff ] ++; x >>= 8;
		counts[ x & 0xff ] ++; x >>= 8;
		counts[ x ] ++;
		rawPtr += 4;
	}
	
	while(rawPtr < rawEnd)
	{
		counts[ *rawPtr++ ] ++;
	}

	#endif		
}
	
	
void CountHistoArrayU8
	(const U8 * rawArray,SINTa rawLen,
	U32 * counts,int numSymbols,bool addOn)
{
	RR_ASSERT( numSymbols <= 256 ); // it better be for U8!

	if ( rawLen < 192 ) // NOTE(fg): exact crossover point depends, but this is in the right ballpark
	{
		CountHistoArrayU8_Simple(rawArray,rawLen,counts,numSymbols,addOn);
		return;
	}

#if defined(OODLE_HISTO_X64GENERIC_ASM)
	SIMPLEPROFILE_SCOPE_N(CountHistoU8_x64,rawLen);
	RAD_ALIGN(OodleHistogramContext,ctx,16); // 4KB of stack (plus change).

	ctx.inptr = rawArray;
	ctx.inlen = rawLen;
	ctx.counts = counts;
	ctx.nsyms_flags = numSymbols | (addOn ? (1<<16) : 0);

	oodle_histo_x64_generic(&ctx);
#else
	SIMPLEPROFILE_SCOPE_N(CountHistoU8,rawLen);

	// Use 4 separate histo arrays internally to reduce store forwarding stalls.
	// We used to do 8 for in-order PPCs with crazy LHS penalties but luckily
	// those days are behind us!
	
	RAD_ALIGN(U32,countsArray[4][256],16); // 4KB of stack.
	memset(countsArray,0,sizeof(countsArray));
	
	const U8 * rawPtr = rawArray;
	const U8 * rawEnd = rawArray+rawLen;

#if defined(OODLE_HISTO_A64_ASM)
	if (rawLen >= 32)
	{
		// get to 8B alignment
		while ((UINTa)rawPtr & 7)
			countsArray[0][ *rawPtr++ ]++;

		rawPtr = oodle_histo_a64_kern(rawPtr, rawEnd - rawPtr, countsArray[0]);
	}
#else
	const U8 * rawEndMul4 = rawArray+(rawLen&~3);

	while(rawPtr < rawEndMul4)
	{
		U32 x = RR_GET32_NATIVE_UNALIGNED(rawPtr);
		RR_UNROLL_I_4(0, countsArray[i][ x & 0xff ] ++; x >>= 8; );
		rawPtr += 4;
	}
#endif
	
	// finish the last few bytes (just throw them into array 0, doesn't matter)
	while(rawPtr < rawEnd)
		countsArray[0][ *rawPtr++ ] ++;
	
	if ( addOn )
	{
		// sum the countsarrays together
		int k=0;
	#if defined(__RADSSE2__)
		for(;k<(numSymbols & ~3);k+=4)
		{
			__m128i sum = _mm_loadu_si128((const __m128i *) &counts[k]);
			sum = _mm_add_epi32(sum, *(const __m128i *) &countsArray[0][k]);
			sum = _mm_add_epi32(sum, *(const __m128i *) &countsArray[1][k]);
			sum = _mm_add_epi32(sum, *(const __m128i *) &countsArray[2][k]);
			sum = _mm_add_epi32(sum, *(const __m128i *) &countsArray[3][k]);
			_mm_storeu_si128((__m128i *)&counts[k], sum);
		}
	#elif defined(__RADNEON__)
		for(;k<(numSymbols & ~3);k+=4)
		{
			uint32x4_t sum = vld1q_u32(&counts[k]);
			sum = vaddq_u32(sum, vld1q_u32(&countsArray[0][k]));
			sum = vaddq_u32(sum, vld1q_u32(&countsArray[1][k]));
			sum = vaddq_u32(sum, vld1q_u32(&countsArray[2][k]));
			sum = vaddq_u32(sum, vld1q_u32(&countsArray[3][k]));
			vst1q_u32(&counts[k], sum);
		}
	#endif
		for(;k<numSymbols;k++)
		{
			U32 out = counts[k];
			RR_UNROLL_I_4(0, out += countsArray[i][k] );
			counts[k] = out;
		}
	}
	else
	{	
		// sum the countsarrays together
		int k=0;
	#if defined(__RADSSE2__)
		for(;k<(numSymbols & ~3);k+=4)
		{
			__m128i sum = _mm_load_si128((const __m128i *) &countsArray[0][k]);
			sum = _mm_add_epi32(sum, *(const __m128i *) &countsArray[1][k]);
			sum = _mm_add_epi32(sum, *(const __m128i *) &countsArray[2][k]);
			sum = _mm_add_epi32(sum, *(const __m128i *) &countsArray[3][k]);
			_mm_storeu_si128((__m128i *)&counts[k], sum);
		}
	#elif defined(__RADNEON__)
		for(;k<(numSymbols & ~3);k+=4)
		{
			uint32x4_t sum = vld1q_u32(&countsArray[0][k]);
			sum = vaddq_u32(sum, vld1q_u32(&countsArray[1][k]));
			sum = vaddq_u32(sum, vld1q_u32(&countsArray[2][k]));
			sum = vaddq_u32(sum, vld1q_u32(&countsArray[3][k]));
			vst1q_u32(&counts[k], sum);
		}
	#endif
		for(;k<numSymbols;k++)
		{
			U32 out = 0;
			RR_UNROLL_I_4(0, out += countsArray[i][k] );
			counts[k] = out;
		}
	}
#endif
}

// rrCodeLenOfHistogramT :
//	simple entropy estimate
//	in bits
//	("codelen" means total # of bits to send count symbols; eg. its proportional to SumCounts
//	 whereas with "entropy" I mean bits per symbol) 
U64 rrCodeLenOfHistogramT(const U32 * counts,int numCounts,U32 sumCounts)
{
    if ( sumCounts == 0 )
		return 0;
		
	S32 invSumI = (1<<30) / sumCounts;
	
    U64 H = 0;
    for(int i=0;i<numCounts;i++)
    {
		U32 c = counts[i];
        if ( c > 0 )
        {
            //H += c * rrlog2((F64)sumCounts/c);
            
            // rrlog2neg_bk does the /= RR_LOG2TABLE_ONE
			//H += c * rrlog2neg_bk<30>( c * invSumI );
			
			// log2tabled is 13 bits (RR_LOG2TABLE_ONE_SHIFT)
			// 32-bit mul is okay for c up to 19 bits
			// not safe unless I check that sumCounts is small enough
			U32 cl = log2tabled<30>( c * invSumI );
			//H += c * cl; // 32 bit mul
			H += (U64)c * cl; // 64 bit mul
        }
    }
    H >>= RR_LOG2TABLE_ONE_SHIFT;
    return H;
}
 
U64 rrCodeLenOfHistogramPow2External(const U32 * counts,int numCounts,const U32 * counts_for_codelen,U32 counts_for_codelen_shift)
{
	//S32 invSumI = (1<<(30-counts_for_codelen_shift));
	S32 invSumI_shift = (30-counts_for_codelen_shift);
	
    U64 H = 0;
    for(int i=0;i<numCounts;i++)
    {
		U32 c = counts[i];
        if ( c > 0 )
        {
			// log2tabled is 13 bits (RR_LOG2TABLE_ONE_SHIFT)
			// 32-bit mul is okay for c up to 19 bits
			// not safe unless I check that sumCounts is small enough
			U32 cl = log2tabled<30>( counts_for_codelen[i] << invSumI_shift );
			//H += c * cl; // 32 bit mul
			H += (U64)c * cl; // 64 bit mul
        }
    }
    H >>= RR_LOG2TABLE_ONE_SHIFT;
    return H;
}
   
F32 rrEntropyOfHistogramT(const U32 * counts,int numCounts,U32 sumCounts)
{
    if ( sumCounts == 0 )
		return 0;
	U64 H = rrCodeLenOfHistogramT(counts,numCounts,sumCounts);
    return (F32)H / sumCounts;
}

F32 rrEntropyOfHistogram(const U32 * counts,int numCounts)
{
    U32 tot = rrSumOfHistogram(counts,numCounts);
    return rrEntropyOfHistogramT(counts,numCounts,tot);
}


U32 rrSumOfHistogram(const U32 * counts,int numCounts)
{
    U32 tot = simd_horizontal_sum_s32((const S32 *)counts,numCounts);
    return tot;
}

#if 0
U32 CodeLenOfHistogram(const U32 * histogram, const U8 * codeLenTable, int alphabet)
{
	// @@ SIMD  ; mul_accum_u32_u8()
	U32 comp_len_estimate = 0;
	for (int i=0;i<alphabet;i++)
	{
		// ensure codeLen is never 0 when histogram is non-zero so this is encodeable!
		RR_ASSERT( histogram[i] ==0 || codeLenTable[i] != 0 );
		comp_len_estimate += codeLenTable[i] * histogram[i];
	}
	return comp_len_estimate;
}

#endif

U32 CodeLenOfHistogram256(const U32 * histogram, S32 histoSum, const U8 * codeLenTable,int alphabet)
{
	bool histo_fits_inS16 = (histoSum < 32768);
	return simd_dotproduct_s32_s8_256((const S32 *)histogram,histo_fits_inS16,(const S8 *)codeLenTable,alphabet);
}

#ifdef RR_DO_ASSERTS

bool verify_histo(const U8 * rawArray,SINTa rawLen,const U32 * counts,int numSymbols)
{
	U32 check_histo[256];
	RR_ASSERT( numSymbols <= 256 );
	CountHistoArrayU8(rawArray,rawLen,check_histo,numSymbols);
	bool ok = memcmp(check_histo,counts,sizeof(U32)*numSymbols) == 0;
	return ok;
}

bool histo_check(const U8 * ptr, SINTa len,const Histo256 & histo)
{
	return verify_histo(ptr,len,histo.counts,256);
}

#endif

// histo_slide : move [ptr,len] from histo "from" to "to"
void histo_slide(const U8 * ptr, SINTa len,
	Histo256 & from,
	Histo256 & to)
{
	for LOOP(i,len)
	{
		int s = ptr[i];
		
		RR_ASSERT( from.counts[s] > 0 );
		from.counts[s] --;
		to.counts[s] ++;
	}
}
			
void histo_add(const U8 * ptr, SINTa len,
	Histo256 & histo)
{
	for LOOP(i,len)
	{
		int s = ptr[i];
		
		histo.counts[s] ++;
	}
}

void histo_remove(const U8 * ptr, SINTa len,
	Histo256 & histo)
{
	for LOOP(i,len)
	{
		int s = ptr[i];
		
		RR_ASSERT( histo.counts[s] > 0 );
		histo.counts[s] --;
	}
}

void histo_count(Histo256 * pTo,const U8 * ptr, SINTa len)
{
	CountHistoArrayU8(ptr,len,pTo->counts,256);
}

U32 histo_sum(Histo256 & histo)
{
	U32 ret = rrSumOfHistogram(histo.counts,256);
	return ret;
}

// to = lhs + rhs
void histo_add(Histo256 * pTo, const Histo256 & lhs, const Histo256 & rhs)
{
	simd_add_u32_256(pTo->counts,lhs.counts,rhs.counts);
}
	
// to = lhs - rhs
void histo_sub(Histo256 * pTo, const Histo256 & lhs, const Histo256 & rhs)
{
	simd_sub_u32_256(pTo->counts,lhs.counts,rhs.counts);
}

//=======================================================

OODLE_NS_END
