// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "newlz_arrays_huff.h"

#include "rrvarbits.h"
#include "histogram.h"
#include "cbradutil.h"
#include "rrlog.h"
#include "rrcompressutil.h"
#include "rrprefetch.h"

#include "newlz_arrays.inl"
#include "newlz_block_coders.h"
#include "newlz_block_coders.inl"
#include "newlz_speedfit.h"
#include "newlz_simd.h"
#include "cpux86.h"
#include "oodleconfigvalues.h"

#include "rrarenaallocator.h"
#include "lzasserts.h"
#include "speedfitter.h"

/*
#include "rrsimpleprof.h"
/*/
#include "rrsimpleprofstub.h"
/**/

// NOTE: MAKE SURE TO KEEP THIS IN SYNC WITH newlz_huff_common.inc! (for ASM vers)
//

/**

CB NOTE :

The MSVC build doesn't normally include the huffx asm (it's in cdep)

To debug it, define this, and drag in the .obj from cdepbuild

//#define NEWLZ_X64GENERIC_HUFF_ASM

**/

#include "rrhuffman.h"
#include "rrhuffman.inl"
#include "sloppymemset.inl"

#if defined(__RADARM__)
#include "rrbits.h"
#ifdef __RAD64REGS__
#define NEWLZ_ARM64
#else
#define NEWLZ_ARM32
#endif
#endif

#ifdef __RADX86__
#include <emmintrin.h>
#endif

#ifdef __RADNEON__
#include <arm_neon.h>
#endif

#ifdef __RADWASM__
// Use the C++ version of the ARM decoders
#include "rrbits.h"
#define NEWLZ_ARM64
#endif

#ifndef OODLE_BUILDING_DLL
// dump: used to prepare input files for standalone kernel tests
//#define DO_NEWLZ_HUFF_DUMP
#endif

#ifdef DO_NEWLZ_HUFF_DUMP
#include <stdio.h>
#endif

OODLE_NS_START

#define NEWLZ_HUFF_DECODE_TABLE_MASK	(NEWLZ_HUFF_DECODE_TABLE_SIZE-1)


/***

newlz_arrays_huff

HUFF TYPES:
* regular huff ("huff3") splits the data into three streams that are laid out as follows in
  the compressed bitstream:
      strm0-> | strm2-> | <-strm1

  The streams labeled "->" are decoded in regular forward order; the final compressed stream
  ("strm1") is stored byte-reversed. Hence the first byte processed by the strm1 decoder is the
  *last* byte in the actual bytestream, and strm1 proceeds consuming bytes backwards.

  Logically, these three streams are decoded separately and then 3-way interleaved as
  follows to form the output bytestream

      strm0[0], strm1[0], strm2[0], strm0[1], strm1[1], strm2[1], strm0[2], ...

  Of course the real decoders process these 3 streams at once (that's the whole point, to gain
  more latent ILP), but that is essentially an implementation detail (albeit a very important
  one). 3 streams happen to be a sweet spot (better than either 2 or 4 streams) on most of our
  key target archs; 2 streams is not enough to avoid frequent stalls even on relatively narrow
  machines like the AMD Jaguar cores, 4 streams has problematic levels of register pressure on
  most 16-register architectures.

  The compressed bitstream layout allows the three stream pointers to act as (conservative) end
  markers for each other:
  - strm0_cursor (increasing) < strm0_end = strm2_begin <= strm2_cursor (increasing)
  - strm2_cursor (increasing) < strm2_end = strm1_end <= strm1_cursor (decreasing)

  In their inner loops, the actual decoders merely ensure that:
    strm0_cursor <= strm2_cursor <= strm1_cursor

  which is sufficient to guarantee that all memory dereferences are in-bounds, and requires
  no "end" pointers to be lugged around or kept in registers at all. Checking against the actual
  stream end positions can be safely deferred until the main decoder loop is complete. In
  particular, strm0 may have decoded some garbage past its end, but this is always data from
  within the bounds of the compressed bit steam, and always detected before data is passed back
  to the caller.

* "huff6" adds even more potential ILP by using a full 6 streams. This is useful on wide-issue
  out of order machines, but not on narrower designs. Therefore, the 6-stream layout is set up
  so it can be decoded in two passes through the 3-stream decoder when a direct 6-stream decoder
  is not available.

  huff6 breaks the uncompressed data in half at the midpoint and then has 6 streams, each of
  which code one half of the data:
      strm0-> | strm2-> | <-strm1    encodes the first half
      strm3-> | strm5-> | <-strm4    encodes the second half

  This can be decoded in one pass by a direct 6-stream decoder, or in two passes through a regular
  3-stream decoder.

****/

#if defined(NEWLZ_ARM64_HUFF_ASM) && !defined(__RADARM64__)
// iOS compiles both ARM32 and 64 with the same build switches,
// but only the 64-bit ARM version can actually use 64-bit ARM
// loops.
#undef NEWLZ_ARM64_HUFF_ASM
#endif

#if defined(NEWLZ_ARM32) && ! defined(_MSC_VER)

// Assuming GCC-style compilers on ARM for now.
static RADINLINE U32 newlz_huff_bitreverse(U32 x)
{
#ifdef __clang__
	return __builtin_arm_rbit(x) >> (32 - NEWLZ_HUFF_CODELEN_LIMIT);
#else
	U32 y;
	__asm__("rbit %0,%1" : "=r"(y) : "r"(x));
	return y >> (32 - NEWLZ_HUFF_CODELEN_LIMIT);
#endif
}

#elif defined(NEWLZ_ARM64) && ! (defined(_MSC_VER) || defined(__RADWASM__))

static RADINLINE U32 newlz_huff_bitreverse(U32 x)
{
#ifdef __clang__
	return __builtin_arm_rbit(x) >> (32 - NEWLZ_HUFF_CODELEN_LIMIT);
#else
	U32 y;
	__asm__("rbit %w0,%w1" : "=r"(y) : "r"(x));
	return y >> (32 - NEWLZ_HUFF_CODELEN_LIMIT);
#endif
}

#else

static RADINLINE U32 newlz_huff_bitreverse(U32 x)
{
	// precomputed bit reverse table for 6 bits
	static const U8 tab[64] =
	{
		0x00, 0x20, 0x10, 0x30, 0x08, 0x28, 0x18, 0x38,
		0x04, 0x24, 0x14, 0x34, 0x0c, 0x2c, 0x1c, 0x3c,
		0x02, 0x22, 0x12, 0x32, 0x0a, 0x2a, 0x1a, 0x3a,
		0x06, 0x26, 0x16, 0x36, 0x0e, 0x2e, 0x1e, 0x3e,
		0x01, 0x21, 0x11, 0x31, 0x09, 0x29, 0x19, 0x39,
		0x05, 0x25, 0x15, 0x35, 0x0d, 0x2d, 0x1d, 0x3d,
		0x03, 0x23, 0x13, 0x33, 0x0b, 0x2b, 0x1b, 0x3b,
		0x07, 0x27, 0x17, 0x37, 0x0f, 0x2f, 0x1f, 0x3f,
	};

	// puzzle it together with 2 table lookups
	RR_COMPILER_ASSERT( NEWLZ_HUFF_CODELEN_LIMIT == 11 );
	return (tab[x & 0x3f] << 5) | tab[x >> 5];
}

#endif

struct KrakenHuffEncState
{
	const U8 * inptr;
	const U8 * inend;
	const RR_VARBITSTYPE * codeptr;
	const U8 * lenptr;
	U8 * outp[3];
	RR_VARBITSTYPE outb[3];
	U32 outc[3];
};

struct KrakenHuffEncStateCombined
{
	const U8 * inptr;
	const U8 * inend;
	const U32 * codelenptr;
	U8 * outp[3];
	RR_VARBITSTYPE outb[3];
	U32 outc[3];
};

#if defined(NEWLZ_X64GENERIC_HUFF_ASM)
extern "C" void oodle_enchuff3_x64_generic_kern(KrakenHuffEncState * st);
extern "C" void oodle_enchuff3_x64_bmi2_kern(KrakenHuffEncState * st);
#elif defined(NEWLZ_JAGUAR_HUFF_ASM)
extern "C" void oodle_enchuff3_x64_jaguar_kern(KrakenHuffEncState * st);
#elif defined(NEWLZ_ZEN2_HUFF_ASM)
extern "C" void oodle_enchuff3_x64_bmi2_kern(KrakenHuffEncState * st);
#elif defined(NEWLZ_ARM64_HUFF_ASM)
extern "C" void oodle_enchuff3c_a64_kern(KrakenHuffEncStateCombined * st);
#endif

// write a pack of 3 streams but no header
// returns number of bytes written
static SINTa newLZ_put_array_huff3streams( U8 * const to, U8 * const to_ptr_end, U8 * const to_end, const U8 * const from, SINTa from_len, 
					const U8 * RADRESTRICT codelens, const RR_VARBITSTYPE * RADRESTRICT le_codes ,
					rrArenaAllocator * arena)
{
	SIMPLEPROFILE_SCOPE_N(put_huff3streams,from_len);
	
	U8 * to_ptr = to;
	RR_ASSERT( rrPtrDiff(to_ptr_end - to_ptr) >= 6 ); // 2 byte header + 1 byte per stream = 5 min size

	// save 2 bytes for complen1
	U8 * to_ptr_complen1 = to_ptr;
	to_ptr += 2;
	
	SINTa comp_len1 = 0;
	SINTa comp_len2 = 0;
	SINTa comp_len3 = 0;
	
	#ifdef __RAD64REGS__
	#define BITSTYPE	U64
	#define HALFTYPE	U32
	#define HALFCOUNT	32
	//#define RR_BSWAPHALF	RR_BSWAP32
	#define RR_PUTHALF_LE	RR_PUT32_LE
	#define RR_PUTHALF_BE	RR_PUT32_BE
	#else
	#define BITSTYPE	U32
	#define HALFTYPE	U16
	#define HALFCOUNT	16
	//#define RR_BSWAPHALF	RR_BSWAP16
	#define RR_PUTHALF_LE	RR_PUT16_LE
	#define RR_PUTHALF_BE	RR_PUT16_BE
	#endif
		
	/**
	
	[stream1-->][stream3-->][<--stream2]
	
	There's funny stuff here because of trying to avoid using scratch space.  I'm trying to just put the
	huff data to the output array, then slide it around to the right place afterward.
	
	To do that 2 trials are done :
	
	trial 1, the streams are put at the 1/3 points of the buffer.
	
	If they collide in trial 1, then the true sizes of each stream are known, and those are used to set the
	start point for trial 2.
	
	
	The bug fixed in Oodle 2.5.5 :
	
	there was a bug introduced in Oodle 2.5.0 with the 64-bit fast output path.
	
	The 64-bit outputter, unlike the other outputter, writes bits past the end of the true bitbuf.
	Those over-writes could crash into other streams.  (most commonly, the end of stream1 writing over
	the head of stream3).  This would then not be detected as a collision because the bitbufs fit,
	and it would return true but with garbage data that doesn't decode correctly.	
	
	**/
	
	// we only call this when we're getting compression
	// BUT it's called on a portion of the array for 6-stream
	// so you could be getting compression overall, but expand this portion
	
	// each segment is (from_len/3)
	//	each byte can expand to 11 bits
	// what's the maximum size of each stream ?
	
	SINTa max_stream_comp_len = ( from_len * 11 + 23 ) / ( 8 * 3 ) + 1;
	max_stream_comp_len += 8; // + 8 for U64 sloppy overwrite
	
	RR_ASSERT( from_len <= 128*1024 );
	RR_ASSERT( max_stream_comp_len < 0xFFFF ); // actually 60075 , fits in 16 bits
	
	rrScopeArenaAlloc huff_comp_scratch_alloc;
	SINTa huff_comp_scratch_alloc_size = max_stream_comp_len;
	U8 * huff_comp_scratch = (U8 *) huff_comp_scratch_alloc.Alloc(huff_comp_scratch_alloc_size,arena);
	
	// comp1 & comp2 in [to,end]
	//	comp3 in scratch

	// [1--> <--2] + [3-->]
	U8 * comp1_start = to_ptr;
	U8 * comp2_end   = to_ptr_end;
	U8 * comp3_start = huff_comp_scratch;
	RR_DURING_ASSERT( U8 * comp3_end = huff_comp_scratch + huff_comp_scratch_alloc_size );


	{			
		BITSTYPE encode1_bits = 0;
		int encode1_bitcount = 0;
		HALFTYPE * encode1_out = (HALFTYPE *)comp1_start;
				
		BITSTYPE encode2_bits = 0;
		int encode2_bitcount = 0;
		HALFTYPE * encode2_out = (HALFTYPE *)comp2_end;
		
		BITSTYPE encode3_bits = 0;
		int encode3_bitcount = 0;
		HALFTYPE * encode3_out = (HALFTYPE *)comp3_start;
				
		const U8 * fileptr = from;
		const U8 * fileptrend = from + from_len;
		
		#define LOADSYM(i) fileptr[i]

		#define PUTSYM(i,encode_bits,encode_bitcount) do { \
			SINTa sym = LOADSYM(i); \
			encode_bits |= le_codes[sym] << encode_bitcount; \
			encode_bitcount += codelens[sym]; \
			} while(0)

		#if defined(NEWLZ_ARM64_HUFF_ASM)
		
		// Loops using the combined code/len table layout
		// since we do some extra setup for the combined_code_len table, bump up the limit a bit
		// (the actual min the loops support is 16)
		if ( fileptrend - fileptr >= 64 )
		{
			KrakenHuffEncStateCombined es = {};
			U32 combined_code_len[256];
			es.inptr = fileptr;
			es.inend = fileptrend;
			es.codelenptr = combined_code_len;
			es.outp[0] = (U8 *)encode1_out;
			es.outp[1] = (U8 *)encode2_out;
			es.outp[2] = (U8 *)encode3_out;

			for (int i = 0; i < 256; i++)
				combined_code_len[i] = (((U32) le_codes[i] + (1 << codelens[i]) - 1) << 8) + codelens[i];

			oodle_enchuff3c_a64_kern(&es);

			// check for sloppy collision :
			// [1--> <--2] + [3-->]
			RR_ASSERT( (es.outp[2]+8) < comp3_end );
			// stream 1 & 2 are in [to,end] , make sure they didn't collide :
			RR_ASSERT(  rrPtrDiff(es.outp[1] - es.outp[0]) >= 8 );

			fileptr = es.inptr;
			encode1_out = (HALFTYPE *)es.outp[0];
			encode2_out = (HALFTYPE *)es.outp[1];
			encode3_out = (HALFTYPE *)es.outp[2];
			encode1_bits = es.outb[0];
			encode2_bits = es.outb[1];
			encode3_bits = es.outb[2];
			encode1_bitcount = es.outc[0];
			encode2_bitcount = es.outc[1];
			encode3_bitcount = es.outc[2];
		}

		#elif defined(NEWLZ_X64GENERIC_HUFF_ASM) || defined(NEWLZ_JAGUAR_HUFF_ASM) || defined(NEWLZ_ZEN2_HUFF_ASM)

		if ( fileptrend - fileptr >= 16 )
		{
			KrakenHuffEncState es = {};
			es.inptr = fileptr;
			es.inend = fileptrend;
			es.codeptr = le_codes;
			es.lenptr = codelens;
			es.outp[0] = (U8 *)encode1_out;
			es.outp[1] = (U8 *)encode2_out;
			es.outp[2] = (U8 *)encode3_out;

		#if defined(NEWLZ_JAGUAR_HUFF_ASM)
			oodle_enchuff3_x64_jaguar_kern(&es);
		#elif defined(NEWLZ_ZEN2_HUFF_ASM) && !defined(NEWLZ_X64GENERIC_HUFF_ASM)
			oodle_enchuff3_x64_bmi2_kern(&es);
		#else
			if (rrCPUx86_feature_present(RRX86_CPU_BMI2))
				oodle_enchuff3_x64_bmi2_kern(&es);
			else
				oodle_enchuff3_x64_generic_kern(&es);
		#endif

			// check for sloppy collision :
			// [1--> <--2] + [3-->]
			RR_ASSERT( (es.outp[2]+8) < comp3_end );
			// stream 1 & 2 are in [to,end] , make sure they didn't collide :
			RR_ASSERT(  rrPtrDiff(es.outp[1] - es.outp[0]) >= 8 );

			fileptr = es.inptr;
			encode1_out = (HALFTYPE *)es.outp[0];
			encode2_out = (HALFTYPE *)es.outp[1];
			encode3_out = (HALFTYPE *)es.outp[2];
			encode1_bits = es.outb[0];
			encode2_bits = es.outb[1];
			encode3_bits = es.outb[2];
			encode1_bitcount = es.outc[0];
			encode2_bitcount = es.outc[1];
			encode3_bitcount = es.outc[2];
		}

		#elif defined(__RAD64REGS__)
		
		// codelen limit is 11
		// bit buffs have >= 57 bits available
		// so I can unconditionally put 5 symbols to each stream
		// 3 streams = 15 symbols per loop
		
		U8 * outptr1 = (U8 *)encode1_out;
		U8 * outptr2 = (U8 *)encode2_out;
		U8 * outptr3 = (U8 *)encode3_out;
		
		// WARNING : this putter style is different than the others in here
		//	the others only put bits when they are used
		//	this one puts bits past the end of the bitbuf
		//	up to 7 bytes past the end of needed bits!
		
		while(fileptrend-fileptr >= 16) // 16 not 15 because we do 8-byte reads on 64-bit ARM
		{
			RR_ASSERT( encode1_bitcount <= 7 );
			RR_ASSERT( encode2_bitcount <= 7 );
			RR_ASSERT( encode3_bitcount <= 7 );

			// do larger loads, otherwise this code has a fairly poor
			// loads-to-arithmetic ratio (hurting us on the Cortex-A57)
			// could be NATIVE_UNALIGNED with modified LOADSYM if we care about BE
		#ifdef __RADARM__
			// For 64-bit ARM, the shift-and-mask operations here are cheap (1 instruction each)
			// so pool the loads!
			U64 syms;
			#undef LOADSYM
			#define LOADSYM(i) ((syms >> ((i&7) * 8)) & 0xff)
			#define BLOCKLOAD(i) syms = RR_GET64_LE_UNALIGNED(fileptr + (i))
		#else
			#define BLOCKLOAD(i)
		#endif

			// 123,123 for fileptr fetch order
			BLOCKLOAD(0);
			PUTSYM( 0,encode1_bits,encode1_bitcount);
			PUTSYM( 1,encode2_bits,encode2_bitcount);
			PUTSYM( 2,encode3_bits,encode3_bitcount);

			PUTSYM( 3,encode1_bits,encode1_bitcount);
			PUTSYM( 4,encode2_bits,encode2_bitcount);
			PUTSYM( 5,encode3_bits,encode3_bitcount);

			PUTSYM( 6,encode1_bits,encode1_bitcount);
			PUTSYM( 7,encode2_bits,encode2_bitcount);
			BLOCKLOAD(8);
			PUTSYM( 8,encode3_bits,encode3_bitcount);

			PUTSYM( 9,encode1_bits,encode1_bitcount);
			PUTSYM(10,encode2_bits,encode2_bitcount);
			PUTSYM(11,encode3_bits,encode3_bitcount);

			PUTSYM(12,encode1_bits,encode1_bitcount);
			PUTSYM(13,encode2_bits,encode2_bitcount);
			PUTSYM(14,encode3_bits,encode3_bitcount);
			fileptr += 15;
			
			RR_ASSERT( encode1_bitcount <= 64 );
			RR_ASSERT( encode2_bitcount <= 64 );
			RR_ASSERT( encode3_bitcount <= 64 );
						
			int outbytes1 = encode1_bitcount>>3;
			RR_PUT64_LE(outptr1,encode1_bits);
			outptr1 += outbytes1;
			int outbits1 = encode1_bitcount&~7;
			encode1_bits >>= outbits1;
			encode1_bitcount -= outbits1;
			
			int outbytes2 = encode2_bitcount>>3;
			RR_PUT64_BE(outptr2-8,encode2_bits);
			outptr2 -= outbytes2;
			int outbits2 = encode2_bitcount&~7;
			encode2_bits >>= outbits2;
			encode2_bitcount -= outbits2;
						
			int outbytes3 = encode3_bitcount>>3;
			RR_PUT64_LE(outptr3,encode3_bits);
			outptr3 += outbytes3;
			int outbits3 = encode3_bitcount&~7;
			encode3_bits >>= outbits3;
			encode3_bitcount -= outbits3;		
			
			#undef LOADSYM
			#undef BLOCKLOAD
			#define LOADSYM(i) fileptr[i]

			RR_ASSERT( (outptr1+8) < outptr2 );	
		}
		
		// check for sloppy collision :
		// [1--> <--2] + [3-->]
		RR_ASSERT( (outptr3+8) < comp3_end );
		// stream 1 & 2 are in [to,end] , make sure they didn't collide :
		RR_ASSERT(  rrPtrDiff(outptr2 - outptr1) >= 8 );

		encode1_out = (HALFTYPE *)outptr1;
		encode2_out = (HALFTYPE *)outptr2;
		encode3_out = (HALFTYPE *)outptr3;
		
		#endif // __RAD64REGS__
		
		// sets of 3 :
		// this is the tail loop for 64-bit and the main loop for 32-bit :
		
		while(fileptr+3<=fileptrend)
		{
			PUTSYM(0,encode1_bits,encode1_bitcount);
			PUTSYM(1,encode2_bits,encode2_bitcount);
			PUTSYM(2,encode3_bits,encode3_bitcount);
			fileptr += 3;
			
			// NOTE output checks code duplicated :
			
			if ( encode1_bitcount >= HALFCOUNT )
			{
				//*encode1_out++ = (HALFTYPE)(encode1_bits);
				RR_PUTHALF_LE(encode1_out,(HALFTYPE)encode1_bits); encode1_out++;
				encode1_bits >>= HALFCOUNT;
				encode1_bitcount -= HALFCOUNT;
			}
			
			if ( encode2_bitcount >= HALFCOUNT )
			{
				encode2_out--;
				//*encode2_out = RR_BSWAPHALF( (HALFTYPE)(encode2_bits) );
				RR_PUTHALF_BE(encode2_out,(HALFTYPE)encode2_bits);
				encode2_bits >>= HALFCOUNT;
				encode2_bitcount -= HALFCOUNT;
			}
			
			if ( encode3_bitcount >= HALFCOUNT )
			{
				//*encode3_out++ = (HALFTYPE)(encode3_bits);
				RR_PUTHALF_LE(encode3_out,(HALFTYPE)encode3_bits); encode3_out++;
				encode3_bits >>= HALFCOUNT;
				encode3_bitcount -= HALFCOUNT;
			}
		}
		
		// final 1 or 2 that doesn't fit in mod3 :
		
		if ( fileptr < fileptrend )
		{
			int sym1 = *fileptr++;
			int cl1 = codelens[sym1];
			BITSTYPE code1 = le_codes[sym1];
			
			encode1_bits |= code1 << encode1_bitcount;
			encode1_bitcount += cl1;
			if ( encode1_bitcount >= HALFCOUNT )
			{
				RR_PUTHALF_LE(encode1_out,(HALFTYPE)encode1_bits); encode1_out++;
				encode1_bits >>= HALFCOUNT;
				encode1_bitcount -= HALFCOUNT;
			}
		}
		if ( fileptr < fileptrend )
		{
			int sym2 = *fileptr++;
			int cl2 = codelens[sym2];
			BITSTYPE code2 = le_codes[sym2];
			
			encode2_bits |= code2 << encode2_bitcount;
			encode2_bitcount += cl2;
			if ( encode2_bitcount >= HALFCOUNT )
			{
				encode2_out--;
				RR_PUTHALF_BE(encode2_out,(HALFTYPE)encode2_bits);
				encode2_bits >>= HALFCOUNT;
				encode2_bitcount -= HALFCOUNT;
			}
		}
		RR_ASSERT( fileptr == fileptrend );
		
		// count the length of the HALFTYPE streams :
				
		comp_len1 = ((char *)encode1_out) - ((char *)comp1_start);
		comp_len2 = ((char *)comp2_end) - ((char *)encode2_out);
		comp_len3 = ((char *)encode3_out) - ((char *)comp3_start);
		
		// flush out single bytes :
						
		while ( encode1_bitcount > 0 )
		{
			comp1_start[comp_len1++] = (U8)(encode1_bits);
			encode1_bits >>= 8;
			encode1_bitcount -= 8;
		}
		while ( encode2_bitcount > 0 )
		{
			comp_len2++;
			comp2_end[-comp_len2] = (U8)(encode2_bits);
			encode2_bits >>= 8;
			encode2_bitcount -= 8;
		}
		while ( encode3_bitcount > 0 )
		{
			comp3_start[comp_len3++] = (U8)(encode3_bits);
			encode3_bits >>= 8;
			encode3_bitcount -= 8;
		}
		
		/*			
		rrPrintf("streams : %d + %d + %d = %d / %d\n",
			comp_len1,comp_len2,comp_len3,
			comp_len1+comp_len2+comp_len3,from_len);
		*/
		
		RR_ASSERT( comp1_start + comp_len1 + comp_len2 < comp2_end );
		RR_ASSERT( comp3_start + comp_len3 < comp3_end );
	}
	
	RR_ASSERT( comp_len1 < max_stream_comp_len );
	RR_ASSERT( comp_len2 < max_stream_comp_len );
	RR_ASSERT( comp_len3 < max_stream_comp_len );
	
	SINTa tot_comp_len = comp_len1+comp_len2+comp_len3+2;
	// there's no need to check fitting in to_ptr_end :
	// this was done on the outside with the huff codelen < must_be_under check
	RR_ASSERT( to + tot_comp_len < to_ptr_end );
	
	// stream1 len must fit in 16 bits :
	// this is guaranteed because (128k/3)*(11/8) < 64k  (max_stream_comp_len)
	RR_ASSERT( comp_len1 < 0xFFFF );
	
	// put length of stream 1 :
	RR_PUT16_LE_UNALIGNED(to_ptr_complen1,U16_checkA(comp_len1));
	
	//rrprintf("E comp_lens : %d + %d + %d = %d\n",comp_len1,comp_len2,comp_len3,comp_len1+comp_len2+comp_len3);
		
	RR_ASSERT( to_ptr == to_ptr_complen1+2 );
	
	// step past comp1 :
	RR_ASSERT( to_ptr == comp1_start );
	to_ptr += comp_len1;
	
	// comp3 in the middle :
	memcpy(to_ptr,comp3_start,comp_len3);
	to_ptr += comp_len3;
	
	// comp2 at the tail :
	memmove(to_ptr,comp2_end - comp_len2,comp_len2);
	to_ptr += comp_len2;
	
	RR_DURING_ASSERT( SINTa check_tot_comp_len = rrPtrDiff( to_ptr - to ) );
	RR_ASSERT( check_tot_comp_len == tot_comp_len );

	return tot_comp_len; // <- could be expanded (3-stream portion of 6-stream), that's okay
}

// Runs: zeros, nonzeros, zeros, nonzeros...
// The initial zero run can have len 0; after that it's always nonzero, zero pairs
// with minimum length of 1.
//
// runLens must have space for numSymbols + 1 elements for the worst case.
//
// numSymbols max of 256, but run lens are strictly < 256
//	degenerate
//
// returns number of runs
static int newlz_identify_alphabet_runs_scalar(rrHuffman * HI, t_alphabet_runlen_type * runLens, int numSymbols)
{
	RR_ASSERT( numSymbols <= 256 );

	// Runs: zeros, nonzeros, zeros, nonzeros...
	// The initial zero run can have len 0; after that it's always nonzero, zero pairs.
	// Then the last pair will be elided because we don't care about zeros past the last nonzero,
	// and the last nonzero count is implied by gotNumSyms (which we send).
	int sym = 0;
	while (sym < numSymbols && HI->codeLenTable[sym] == 0)
		sym++;

	RR_ASSERT(sym < numSymbols);
	runLens[0] = ( sym );
	int runCount = 1;

	while (sym < numSymbols)
	{
		// nonzero run
		RR_ASSERT(HI->codeLenTable[sym] != 0);

		int runBase = sym;
		while (sym < numSymbols && HI->codeLenTable[sym] > 0)
			sym++;

		runLens[runCount++] = ( sym - runBase );

		// zero run
		runBase = sym;
		while (sym < numSymbols && HI->codeLenTable[sym] == 0)
			sym++;
		runLens[runCount++] = ( sym - runBase );
	}

	return runCount;
}

#ifdef __RADSSE2__
static int newlz_identify_alphabet_runs_sse2(rrHuffman * HI, t_alphabet_runlen_type * runLens, int numSymbols)
{
	RR_ASSERT( numSymbols <= 256 );

	// WARNING : reads off end of codeLenTable
	//	that's okay because encodeTable comes after codeLenTable
	const U8 * codeLens = HI->codeLenTable;

	// Runs: zeros, nonzeros, zeros, nonzeros...
	// The initial zero run can have len 0; after that it's always nonzero, zero pairs.
	// Then the last pair will be elided because we don't care about zeros past the last nonzero,
	// and the last nonzero count is implied by gotNumSyms (which we send).
	int sym = 0;
	for(;;)
	{
		RR_ASSERT(sym < numSymbols); // should not reach numSymbols in first zero run

		__m128i lens = _mm_loadu_si128((const __m128i *)(codeLens+sym));
		__m128i lenisnonzero = _mm_cmpgt_epi8(lens, _mm_setzero_si128());
		int nonzeromask = _mm_movemask_epi8(lenisnonzero);
		if ( nonzeromask == 0 )
		{
			sym += 16;
		}
		else
		{
			sym += rrCtz32(nonzeromask);
			break;
		}
	}
	runLens[0] = ( sym );
	int runCount = 1;

	while (sym < numSymbols)
	{
		// nonzero run
		RR_ASSERT(codeLens[sym] != 0);

		int runBase = sym;
		while (sym < numSymbols )
		{
			__m128i lens = _mm_loadu_si128((const __m128i *)(codeLens+sym));
			__m128i leniszero = _mm_cmpeq_epi8(lens, _mm_setzero_si128());
			int zeromask = _mm_movemask_epi8(leniszero);
			if ( zeromask == 0 )
			{
				sym += 16;
			}
			else
			{
				sym += rrCtz32(zeromask);
				break;
			}
		}
		sym = RR_MIN(sym,numSymbols);

		runLens[runCount++] = ( sym - runBase );
		
		// nonzero run
		RR_ASSERT( sym == numSymbols || codeLens[sym] == 0);

		// zero run
		runBase = sym;
		while (sym < numSymbols )
		{
			__m128i lens = _mm_loadu_si128((const __m128i *)(codeLens+sym));
			__m128i lenisnonzero = _mm_cmpgt_epi8(lens, _mm_setzero_si128());
			int nonzeromask = _mm_movemask_epi8(lenisnonzero);
			if ( nonzeromask == 0 )
			{
				sym += 16;
			}
			else
			{
				sym += rrCtz32(nonzeromask);
				break;
			}
		}
		sym = RR_MIN(sym,numSymbols);
		runLens[runCount++] = ( sym - runBase );
	}

	return runCount;
}

#define newlz_identify_alphabet_runs newlz_identify_alphabet_runs_sse2
#else // __RADSSE2__

#define newlz_identify_alphabet_runs newlz_identify_alphabet_runs_scalar

#endif // __RADSSE2__

// code length predictor
#define PRED_INIT(log2)			(log2)*4
#define PRED_GET(state)			(((state) + 2)>>2)
#define PRED_UPDATE(state,val)	(((state)*3 + 2)>>2) + (val)

static void newlz_encode_hufflens2(rrHuffman * HI, rrVarBits * vb, const t_alphabet_runlen_type * runLens, int runCount)
{
	SIMPLEPROFILE_SCOPE(encode_hufflens2);

	// Prepare array and histo of codelen deltas; the runs we already have.
	const U32 maxCodelenDeltaFolded = (NEWLZ_HUFF_CODELEN_LIMIT - 1) * 2;
    int NumDeltaFromPrevious[maxCodelenDeltaFolded + 1] = { 0 };
	U8 codelenDelta[256];
	int prevCodeLen4 = PRED_INIT(8);
	int gotNumSyms = 0;

	RR_ASSERT(HI->numSymbols == 256);

	// The last run pair will be elided because we don't care about zeros past the last nonzero,
	// and the last nonzero count is implied by gotNumSyms (which we send).
	RR_ASSERT(runCount >= 3);
	RR_ASSERT((runCount & 1) == 1);

	int sym = runLens[0];
	for (int runPairBase = 1; runPairBase < runCount; runPairBase += 2)
	{
		int symRunEnd = sym + runLens[runPairBase];
		while (sym < symRunEnd)
		{
			int codeLen = HI->codeLenTable[sym];

			int pred = (prevCodeLen4 + 2) >> 2;
			int delta = codeLen - pred;
			int foldedDelta = rrFoldUpNegatives(delta);
			RR_ASSERT( foldedDelta <= (int)maxCodelenDeltaFolded );
			NumDeltaFromPrevious[foldedDelta] ++;
			prevCodeLen4 += delta;

			codelenDelta[gotNumSyms] = (U8)foldedDelta;
			gotNumSyms++;
			sym++;
		}

		sym += runLens[runPairBase + 1];
	}

	rrVarBits_Output(vb->m);

    // riceBits is usually best at 1, but on 'book1' it's much better at 2
    int riceBits = 1;

    // check if entropy is better if I bump meanCodeLen up or down
    {
        double bestH = 99999999999.f;
        for(int cur=0;cur<4;cur++)
        {
            double H = EntropyOfCountsRice(NumDeltaFromPrevious,maxCodelenDeltaFolded + 1,cur);
            if ( H < bestH )
            {
                bestH = H;
                riceBits = cur;
            }
        }
    }
	rrVarBits_Put(vb->m, riceBits, 2);

	// Determine Exp-Golomb run lengths for alphabet shape
	U8 runlen_top[256], runlen_bot[256], runlen_bot_nbits[256];
	int num_eg = newLZ_encode_alphabet_shape_runlens_split(runlen_top, runlen_bot, runlen_bot_nbits, gotNumSyms, runLens, runCount);

	RR_ASSERT(gotNumSyms >= 1 && gotNumSyms <= 256);
	rrVarBits_Put(vb->m, gotNumSyms-1, 8);
	newLZ_encode_alphabet_shape_num_EG(vb, num_eg, gotNumSyms);

	rrVarBits_Output(vb->m);

	// Codelen deltas
	U8 codelen_top[256], codelen_bot[256];
	newLZ_encode_rice_U8_split(codelen_top, codelen_bot, codelenDelta, gotNumSyms, riceBits);

	// Send all the unary prefixes of the Rice and Exp-Golomb codes
	newLZ_encode_unary_block(vb, codelen_top, gotNumSyms);
	newLZ_encode_unary_block(vb, runlen_top, num_eg);

	// Send the Rice bottom bits
	newLZ_encode_uniform_U8_block(vb, codelen_bot, gotNumSyms, riceBits);

	// Send the EG bottom bits
	newLZ_encode_variable_U8_block(vb, runlen_bot, runlen_bot_nbits, num_eg);
}

// kind of nasty two different failure return values ;
//	either -1 or from_len+1 for failure ; WTF me
// -> this is now important to get right
//  < 0 means failure but *to was not modified, so previous contents are still valid
//	> from_len means failure but *to was changed!
SINTa newLZ_put_array_huff(U8 * const to, U8 * const to_end, const U8 * const from, SINTa from_len, 
									const U32 * histogram, 
									F32 lambda, const OodleSpeedFit * speedfit, F32 * pJ, F32 deadline_t,
									U32 * p_huff_type, U32 entropy_flags,
									rrArenaAllocator * arena,
									int compression_level)
{
	SIMPLEPROFILE_SCOPE_N(put_array_huff,from_len);
			
	RAD_ALIGN(U8,huffMem,16)[2048];
	RR_ASSERT( rrHuffman_MemorySizeNeeded(256,0) <= (int)sizeof(huffMem) );
	
	rrHuffman * H = rrHuffman_Create(256,0,huffMem);
	
	//RR_ASSERT( verify_histo(from,from_len,histogram,256) );
	
	{
		SIMPLEPROFILE_SCOPE(put_array_huff_build);

		// new Heuristic is very good ; package merge doesn't do much anymore
		rrbool do_package_merge = ( compression_level >= 6 ); // 6 is Optimal2
		rrHuffman_BuildCodeLens(H,histogram,(U32)from_len,NEWLZ_HUFF_CODELEN_LIMIT,arena,do_package_merge);
	}
			
	RR_ASSERT( H->numSymbols == 256 );
	int nonzero_alphabet = H->topSym + 1;
	RR_ASSERT( nonzero_alphabet <= 256 );
	
	int num_non_zero = H->gotNumSymbols;

	// Handle memset special case here
	if ( num_non_zero < 2 )
	{
		// We're not going to get here from cases that funnel into newLZ_put_array_memset, which is most of them.
		// That used to not be true when RLE_MEMSET was disabled, but now even with RLE_MEMSET off, we still
		// emit a Huff memset chunk directly from there.
		//
		// But we still have a couple direct calls to put_array_huff from the RLE encoder and also in tests,
		// so make sure this is handled.

		RR_ASSERT( num_non_zero == 1 );

		// if not enough space, too bad.
		// return -1, we haven't modified "to" yet.
		if ( to_end - to < 3 )
			return -1;

		// check lagrange J, bail if not a win
		F32 memset_time = speedfit->memcpy(from_len);
		F32 huff_J = 5/*header*/ + 3/*payload*/ + lambda * memset_time;
		if ( huff_J >= *pJ )
			return -1;

		// okay, this is happening!
		U8 * to_ptr = newLZ_put_array_huff3_memset_payload(to, from[0]);
		*p_huff_type = NEWLZ_ARRAY_TYPE_HUFF;
		*pJ = huff_J;

		SINTa comp_len = rrPtrDiff( to_ptr - to );
		RR_ASSERT( comp_len == 3 );
		return comp_len;
	}
	
	RR_VARBITSTYPE le_codes[256+1];
	// runLens is 1k stack ; le_codes is 2k
	// runLens not used at same time as le_codes so share space on stack :
	t_alphabet_runlen_type * runLens = (t_alphabet_runlen_type *) le_codes;

	// Some parameters used in speedfit
	int runCount = 0;
	int num_alphabet_runs = 0;
	if ( speedfit->uses_huff_alphabet_runs )
	{
		runCount = newlz_identify_alphabet_runs(H, runLens, 256);
		num_alphabet_runs = (runCount - 1) / 2;
	}

	SINTa huff_comp_len_estimate_bits = CodeLenOfHistogram256(histogram,(U32)from_len,H->codeLenTable,nonzero_alphabet);
	F32 expected_bpb = (F32)huff_comp_len_estimate_bits / (F32)from_len;

	// huff6 extra cost: (see below)
	// - 5 extra bytes of metadata for stream sizes
	// - 3 extra bitstream flushes at an expected 3.5 bits/stream cost
	static const F32 HUFF6_EXTRA_BYTES = 5.0f + 3*3.5f/8.0f;

	U32 huff_type = NEWLZ_ARRAY_TYPE_HUFF;
	if ( entropy_flags & NEWLZ_ARRAY_FLAG_ALLOW_HUFF6 )
	{
		F32 huff6_minus_huff3_time =
			speedfit->huff6(from_len,num_non_zero,num_alphabet_runs,expected_bpb) -
			speedfit->huff3(from_len,num_non_zero,num_alphabet_runs,expected_bpb);
		// huff6_minus_huff3_time is > 0 for small len , < 0 for large len
	
		F32 huff6_delta_J = HUFF6_EXTRA_BYTES + lambda * huff6_minus_huff3_time;

		if ( huff6_delta_J < 0.0f )
		{
			huff_type = NEWLZ_ARRAY_TYPE_HUFF6;
		}
	}
	*p_huff_type = huff_type;
	
	F32 huff_time = speedfit_huff_time(speedfit,huff_type,from_len,num_non_zero,num_alphabet_runs,expected_bpb);
	F32 huff_J_add = 5 + lambda * huff_time;

	// is it possible to meet our deadline? if not, bail
	if ( huff_time > deadline_t )
	{
		//rrprintf("!! huff candidate misses deadline: huff_time=%.1f > deadline=%.1f\n", huff_time, deadline_t);
		// return of -1 means we did not modify *to
		return -1;
	}
	
	// count output size now to detect incompressibility now that I have codelens
	//	 also compare against passed in J for alternative encoding
	//	 if I can't beat that J, early out now
	// -> this is a speed optimization for the whole-huff chunk test ; if we can't beat LZ encoding
	//	we early out and don't actually make the huffman output
	// huff_comp_len_estimate is in bits, make it bytes :
	SINTa huff_comp_len_estimate = (huff_comp_len_estimate_bits+7)/8;
	// -> this rounding up bits to bytes doesn't account for the 3-6 bitstream flushes
	//		(we actually round up bits to bytes 6 times)
	// + header+metadata
	//	13 bytes for huff6
	huff_comp_len_estimate += 13;
	// for the baked case this must be our bail-out so we leave the previous unbaked alone
	//  that is, huff_comp_len_estimate must be a strict over-estimate
	// (huff_comp_len_estimate does not yet include any codelen packing header size,
	//	 I will add it on below)
	//rrprintf("  est1 J=%7.1f time=%5.f Jadd=%6.1f\n", huff_comp_len_estimate + huff_J_add, huff_time, huff_J_add);

	#if 1
	// @@@@ at this point huff_comp_len_estimate has no estimate of the codelen transmission AT ALL
	//	we could put in a lower bound here
	//	something like 2 bits per non-zero symbol?
	//	(once you get runCount you can add that too)
	SINTa min_header_size = (num_non_zero*2 + 8 + 7)/8;
	#else
	SINTa min_header_size = 0;
	#endif

	if ( huff_comp_len_estimate + huff_J_add + min_header_size >= *pJ )
	{
		// return of -1 means we did not modify *to
		return -1;
	}
		
	// Identify runs of present/missing symbols
	if ( ! speedfit->uses_huff_alphabet_runs )
	{
		// Speedfits can choose to do this earlier; if they haven't yet,
		// do it not.
		runCount = newlz_identify_alphabet_runs(H, runLens, 256);
	}
	
	// write huff header to scratch first :
	U8 huff_header_scratch[256];

	rrVarBits vb;
	rrVarBits_PutOpen(vb.m,huff_header_scratch);

	// hufflens2 in Oodle 2.6.0
	bool do_hufflens2 = ( H->gotNumSymbols > 4 ) && ( entropy_flags & NEWLZ_ARRAY_FLAG_ALLOW_HUFFLENS2 );
	// still use old PackCodeLens4 with hufflens2

    if ( ! do_hufflens2 ) 
	{
		rrVarBits_Puta0(vb.m);

		rrHuffman_PackCodeLens(H,&vb);
	}
	else
	{
		// >= Oodle 2.6.0 only
		RR_ASSERT( g_OodleLZ_BackwardsCompatible_MajorVersion >= 6 );

		rrVarBits_Puta1(vb.m);
		rrVarBits_Puta0(vb.m);
		// reserve 11 for the future
		
		newlz_encode_hufflens2(H,&vb,runLens,runCount);
	}
	
	rrVarBits_PutFlush8(vb.m);
	SINTa headerSize = rrVarBits_PutSizeBytes(vb.m,huff_header_scratch);
	RR_ASSERT( headerSize+16 < (SINTa)sizeof(huff_header_scratch) );

	// huff_comp_len_estimate is the huff codes
	huff_comp_len_estimate += headerSize;

	// check huff_comp_len_estimate
	//	this also ensures headerSize fits in [to,to_end]
	//rrprintf("  est2 J=%7.1f\n", huff_comp_len_estimate + huff_J_add);
	if ( huff_comp_len_estimate + huff_J_add >= *pJ )
	{
		// return of -1 means we did not modify *to
		return -1;
	}
	
	// must have at least from_len worth of room : // changed 03-28-2019 , no longer pre-checking room
	// RR_ASSERT( (to_end - to) >= from_len );
	
	// make sure we can put huff AND have 8 bytes for U64 output overwrite :
	if ( huff_comp_len_estimate+8 >= rrPtrDiff(to_end - to) )
	{
		return -1;
	}
	
	U8 * to_ptr = to;
	U8 * to_ptr_end = to_end;
	
	// to_end can be really far away
	//	@@ does it help cache hotness in the encoder to keep it closer?
	to_ptr_end = RR_MIN(to_ptr_end,to_ptr + from_len + 256);
	
	RR_ASSERT( (to_ptr + headerSize) <= to_ptr_end );
	
	// now we know we have room and will definitely output data
	//	put header to to_ptr :
	memcpy(to_ptr,huff_header_scratch,headerSize);	
	to_ptr += headerSize;		

	// we handle the degernate case with numSymbols==1 above, so there's going to
	// be an actual payload too!

	// build the codes ; MSB-first and then reverse them :
	rrHuffman_BuildEncodeTable(H);

	const U8 * RADRESTRICT codelens = H->codeLenTable;

	// there used to be hand-unrolled versions of this loop here, but... really?
	// there are better places to save cycles, so KISS.
	// (would be OK to SIMD if we wanted: basically PSHUFB to look up nibble-granular bit reverses)
	for LOOP(i,nonzero_alphabet)
	{
		int cl = codelens[i];
		// NOTE(fg): used to check for cl==0 here but that check predicts poorly and costs way more than it saves.
		// (quick test on lzt99 says ~5.5 cycles per loop iter with check vs. ~3 cycles per iter without.)
		U32 code = H->encodeTable[i];
		RR_ASSERT( code < (1<<NEWLZ_HUFF_CODELEN_LIMIT) );
		U32 rev = (newlz_huff_bitreverse(code)) >> (NEWLZ_HUFF_CODELEN_LIMIT - cl);
		le_codes[i] = rev;
	}

	//======================================
	// write huffman codes to 3 (or 6) streams :

	if ( huff_type == NEWLZ_ARRAY_TYPE_HUFF6 )
	{
		// 2+2+3 bytes of metadata + 6 bitstream flushes = 13 bytes overhead max
		//	= 7 + 6 = 13

		// reserve 3 bytes for size of first 3-stream complex
		U8 * to_first3_size = to_ptr;
		RR_ASSERT( to_end - to_first3_size >= 3 );
		to_ptr += 3;

		// put first half-stream; length needs to be >= second half-stream length
		SINTa first_half_len = (from_len + 1)>>1;
		SINTa size1 = newLZ_put_array_huff3streams(to_ptr,to_ptr_end,to_end,from,first_half_len,codelens,le_codes,arena);
		RR_ASSERT( size1 > 0 );
		to_ptr += size1;

		// store size
		U32 size24 = U32_checkA(size1);
		RR_PUT24_LE_NOOVERRUN(to_first3_size,size24); // @@24 bits for this seems overkill, but 16 isn't guaranteed to be enough.. argh.

		// put second half-stream
		SINTa second_half_len = from_len - first_half_len;
		SINTa size2 = newLZ_put_array_huff3streams(to_ptr,to_ptr_end,to_end,from+first_half_len,second_half_len,codelens,le_codes,arena);
		RR_ASSERT( size2 > 0 );
		to_ptr += size2;
	}
	else
	{
		// 2 bytes of metadata + 3 bitstream flushes = 5 bytes overhead max

		SINTa size = newLZ_put_array_huff3streams(to_ptr,to_ptr_end,to_end,from,from_len,codelens,le_codes,arena);
		RR_ASSERT( size > 0 );

		to_ptr += size;
	}

	SINTa tot_comp_len = rrPtrDiff( to_ptr - to );

	// huff_comp_len_estimate should have been a strict over-estimate :
	RR_ASSERT( tot_comp_len <= huff_comp_len_estimate );

	// NOTE : "comp_len" does NOT contain the 5 byte header size
	//	but J does !!
	F32 huff_J = tot_comp_len + huff_J_add;
	RR_ASSERT( huff_J <= *pJ );
	*pJ = huff_J;

	//rrprintf("  real J=%7.1f\n", huff_J);
	
	return tot_comp_len;
}

//===================================================================

// Un-bit-reversed huff table in MSB-first decode order, used in building the real KrakenHuffTab
struct KrakenMSBHuffTab
{
	U8 len[NEWLZ_HUFF_DECODE_TABLE_SIZE + 16]; // code lens; +16 for sloppy memset
	U8 sym[NEWLZ_HUFF_DECODE_TABLE_SIZE + 16]; // sym id; +16 for sloppy memset
};

#ifdef NEWLZ_X86SSE2_HUFF_ASM
#define NEWLZ_SSE2_LAYOUT
#endif

#if defined(NEWLZ_ARM32) || defined(NEWLZ_ARM64)
// On ARM, we want the hufftab to be specially aligned, so don't throw it into
// the regular huff state array
#define NEWLZ_NO_HUFFTAB_IN_STATE
#endif

// Define the table layout. The different decoder inner loops need
// slightly different formats.

#if defined NEWLZ_SSE2_LAYOUT

// NOTE: must match what the ASM inner loops expect (or disable them above)!
struct KrakenHuffTab
{
	U32 len[NEWLZ_HUFF_DECODE_TABLE_SIZE]; // needs to be U32 for MOVD
	U8 sym[NEWLZ_HUFF_DECODE_TABLE_SIZE];
};

#define NEWLZ_HUFF_LEN(tab,peek) ((tab)->len[(peek)])
#define NEWLZ_HUFF_SYM(tab,peek) ((tab)->sym[(peek)])
#define NEWLZ_HUFF_INIT(tab,ind,ilen,isym) ((tab)->len[(ind)] = (ilen), (tab)->sym[(ind)] = (isym))

#else

// NOTE: must match what the ASM inner loops expect (or disable them above)!
struct KrakenHuffElem
{
	U8 len;	// code len
	U8 sym; // symbol
};

struct KrakenHuffTab
{
	KrakenHuffElem e[NEWLZ_HUFF_DECODE_TABLE_SIZE];
};

#define NEWLZ_HUFF_LEN(tab,peek) ((tab)->e[(peek)].len)
#define NEWLZ_HUFF_SYM(tab,peek) ((tab)->e[(peek)].sym)
#define NEWLZ_HUFF_INIT(tab,ind,ilen,isym) ((tab)->e[(ind)].len = (ilen), (tab)->e[(ind)].sym = (isym))

#endif // NEWLZ_SSE2_LAYOUT

//=======================================================================

static void newLZ_bit_reverse_hufftab(KrakenHuffTab * to_table, const KrakenMSBHuffTab * msbHuff);

static RADFORCEINLINE const KrakenHuffTab * newLZ_prep_hufftab(KrakenHuffTab * to_table,const KrakenMSBHuffTab * msbHuff)
{
	newLZ_bit_reverse_hufftab(to_table, msbHuff);
	return to_table;
}
	
//=======================================================================

#if 1 && defined(__RADX86__)

#ifndef NEWLZ_SSE2_LAYOUT

// default layout
static void newLZ_bit_reverse_hufftab(KrakenHuffTab * to_table, const KrakenMSBHuffTab * msbHuff)
{
	SIMPLEPROFILE_SCOPE(huff_bitrev_default_sse2);

	RR_COMPILER_ASSERT( ( NEWLZ_HUFF_DECODE_TABLE_SIZE % 64 ) == 0 );
	UINTa e = NEWLZ_HUFF_DECODE_TABLE_SIZE/8;

	// NOTE(fg):
	// The idea here is to load a square 8x8 group of table entries, transpose
	// them (8x8 matrix transpose), and then store it back.
	//
	// Using bit-reversed indices on both the loads and stores makes the whole
	// process equivalent to a large index bit-reverse.

#define interleave_rows(a,b) t = a; a = _mm_unpacklo_epi16(a,b); b = _mm_unpackhi_epi16(t, b)

	for (UINTa s = 0; s < e; s += 8)
	{
		UINTa rs = newlz_huff_bitreverse(static_cast<U32>(s)); // could use a dedicated table for this but meh
		__m128i r0,r1,r2,r3,r4,r5,r6,r7,t;

		// syms: read rows (bit-reversed index order)
		r0 = _mm_loadl_epi64((__m128i const *) (msbHuff->sym + rs + e*0));
		r1 = _mm_loadl_epi64((__m128i const *) (msbHuff->sym + rs + e*4));
		r2 = _mm_loadl_epi64((__m128i const *) (msbHuff->sym + rs + e*2));
		r3 = _mm_loadl_epi64((__m128i const *) (msbHuff->sym + rs + e*6));
		r4 = _mm_loadl_epi64((__m128i const *) (msbHuff->sym + rs + e*1));
		r5 = _mm_loadl_epi64((__m128i const *) (msbHuff->sym + rs + e*5));
		r6 = _mm_loadl_epi64((__m128i const *) (msbHuff->sym + rs + e*3));
		r7 = _mm_loadl_epi64((__m128i const *) (msbHuff->sym + rs + e*7));

		// lens: read rows (bit-reversed index order), interleave with 8-bit packed syms
		r0 = _mm_unpacklo_epi8(_mm_loadl_epi64((__m128i const *) (msbHuff->len + rs + e*0)), r0);
		r1 = _mm_unpacklo_epi8(_mm_loadl_epi64((__m128i const *) (msbHuff->len + rs + e*4)), r1);
		r2 = _mm_unpacklo_epi8(_mm_loadl_epi64((__m128i const *) (msbHuff->len + rs + e*2)), r2);
		r3 = _mm_unpacklo_epi8(_mm_loadl_epi64((__m128i const *) (msbHuff->len + rs + e*6)), r3);
		r4 = _mm_unpacklo_epi8(_mm_loadl_epi64((__m128i const *) (msbHuff->len + rs + e*1)), r4);
		r5 = _mm_unpacklo_epi8(_mm_loadl_epi64((__m128i const *) (msbHuff->len + rs + e*5)), r5);
		r6 = _mm_unpacklo_epi8(_mm_loadl_epi64((__m128i const *) (msbHuff->len + rs + e*3)), r6);
		r7 = _mm_unpacklo_epi8(_mm_loadl_epi64((__m128i const *) (msbHuff->len + rs + e*7)), r7);

		// transpose pass 1
		interleave_rows(r0, r4);
		interleave_rows(r1, r5);
		interleave_rows(r2, r6);
		interleave_rows(r3, r7);

		// transpose pass 2
		interleave_rows(r0, r2);
		interleave_rows(r1, r3);
		interleave_rows(r4, r6);
		interleave_rows(r5, r7);

		// transpose pass 3
		interleave_rows(r0, r1);
		interleave_rows(r2, r3);
		interleave_rows(r4, r5);
		interleave_rows(r6, r7);

		#define STORE_ROW(dest, vals) _mm_store_si128((__m128i *) (dest), (vals))

		// store columns (bit-reversed index order)
		STORE_ROW(to_table->e + s + e*0, r0);
		STORE_ROW(to_table->e + s + e*4, r1);
		STORE_ROW(to_table->e + s + e*2, r2);
		STORE_ROW(to_table->e + s + e*6, r3);
		STORE_ROW(to_table->e + s + e*1, r4);
		STORE_ROW(to_table->e + s + e*5, r5);
		STORE_ROW(to_table->e + s + e*3, r6);
		STORE_ROW(to_table->e + s + e*7, r7);

		#undef STORE_ROW
	}

#undef interleave_rows
}

#else // #ifndef NEWLZ_SSE2_LAYOUT

// SSE2 layout
static void newLZ_bit_reverse_hufftab(KrakenHuffTab * to_table, const KrakenMSBHuffTab * msbHuff )
{
	SIMPLEPROFILE_SCOPE(huff_bitrev_wide_sse2);

	RR_COMPILER_ASSERT( ( NEWLZ_HUFF_DECODE_TABLE_SIZE % 64 ) == 0 );
	UINTa e = NEWLZ_HUFF_DECODE_TABLE_SIZE/8;

	// NOTE(fg):
	// The idea here is to load a square 8x8 group of table entries, transpose
	// them (8x8 matrix transpose), and then store it back.
	//
	// Using bit-reversed indices on both the loads and stores makes the whole
	// process equivalent to a large index bit-reverse.
	
	U32 * to_len = to_table->len;
	U8 * to_sym = to_table->sym;

#define interleave_rows(a,b) t = a; a = _mm_unpacklo_epi8(a,b); b = _mm_unpackhi_epi8(t, b)

	for (UINTa s = 0; s < e; s += 8)
	{
		UINTa rs = newlz_huff_bitreverse(static_cast<U32>(s)); // could use a dedicated table for this but meh
		__m128i r0,r1,r2,r3,r4,r5,r6,r7,t;

		// lens: read rows (bit-reversed index order)
		r0 = _mm_loadl_epi64((__m128i const *) (msbHuff->len + rs + e* 0));
		r1 = _mm_loadl_epi64((__m128i const *) (msbHuff->len + rs + e* 4));
		r2 = _mm_loadl_epi64((__m128i const *) (msbHuff->len + rs + e* 2));
		r3 = _mm_loadl_epi64((__m128i const *) (msbHuff->len + rs + e* 6));
		r4 = _mm_loadl_epi64((__m128i const *) (msbHuff->len + rs + e* 1));
		r5 = _mm_loadl_epi64((__m128i const *) (msbHuff->len + rs + e* 5));
		r6 = _mm_loadl_epi64((__m128i const *) (msbHuff->len + rs + e* 3));
		r7 = _mm_loadl_epi64((__m128i const *) (msbHuff->len + rs + e* 7));

		// transpose pass 1 (half-size input)
		r0 = _mm_unpacklo_epi8(r0, r4);
		r1 = _mm_unpacklo_epi8(r1, r5);
		r2 = _mm_unpacklo_epi8(r2, r6);
		r3 = _mm_unpacklo_epi8(r3, r7);

		// transpose pass 2
		interleave_rows(r0, r2);
		interleave_rows(r1, r3);

		// transpose pass 3
		interleave_rows(r0, r1);
		interleave_rows(r2, r3);

		// store columns (bit-reversed index order)
		{
			__m128i o0, o1, o2, o3;
			__m128i t0, t1;

			// Expand bytes to DWords (doing it this way is better than PSHUFB on Jaguar)
			#define EXPAND4(in) \
				t0 = _mm_unpacklo_epi8(in, _mm_setzero_si128()); \
				t1 = _mm_unpackhi_epi8(in, _mm_setzero_si128()); \
				o0 = _mm_unpacklo_epi8(t0, _mm_setzero_si128()); \
				o1 = _mm_unpackhi_epi8(t0, _mm_setzero_si128()); \
				o2 = _mm_unpacklo_epi8(t1, _mm_setzero_si128()); \
				o3 = _mm_unpackhi_epi8(t1, _mm_setzero_si128())

			EXPAND4(r0);
			_mm_store_si128((__m128i *) (to_len + s + e*0 + 0), o0);
			_mm_store_si128((__m128i *) (to_len + s + e*0 + 4), o1);
			_mm_store_si128((__m128i *) (to_len + s + e*4 + 0), o2);
			_mm_store_si128((__m128i *) (to_len + s + e*4 + 4), o3);

			EXPAND4(r1);
			_mm_store_si128((__m128i *) (to_len + s + e*2 + 0), o0);
			_mm_store_si128((__m128i *) (to_len + s + e*2 + 4), o1);
			_mm_store_si128((__m128i *) (to_len + s + e*6 + 0), o2);
			_mm_store_si128((__m128i *) (to_len + s + e*6 + 4), o3);

			EXPAND4(r2);
			_mm_store_si128((__m128i *) (to_len + s + e*1 + 0), o0);
			_mm_store_si128((__m128i *) (to_len + s + e*1 + 4), o1);
			_mm_store_si128((__m128i *) (to_len + s + e*5 + 0), o2);
			_mm_store_si128((__m128i *) (to_len + s + e*5 + 4), o3);

			EXPAND4(r3);
			_mm_store_si128((__m128i *) (to_len + s + e*3 + 0), o0);
			_mm_store_si128((__m128i *) (to_len + s + e*3 + 4), o1);
			_mm_store_si128((__m128i *) (to_len + s + e*7 + 0), o2);
			_mm_store_si128((__m128i *) (to_len + s + e*7 + 4), o3);

			#undef EXPAND4
		}

		// syms: read rows (bit-reversed index order)
		r0 = _mm_loadl_epi64((__m128i const *) (msbHuff->sym + rs + e* 0));
		r1 = _mm_loadl_epi64((__m128i const *) (msbHuff->sym + rs + e* 4));
		r2 = _mm_loadl_epi64((__m128i const *) (msbHuff->sym + rs + e* 2));
		r3 = _mm_loadl_epi64((__m128i const *) (msbHuff->sym + rs + e* 6));
		r4 = _mm_loadl_epi64((__m128i const *) (msbHuff->sym + rs + e* 1));
		r5 = _mm_loadl_epi64((__m128i const *) (msbHuff->sym + rs + e* 5));
		r6 = _mm_loadl_epi64((__m128i const *) (msbHuff->sym + rs + e* 3));
		r7 = _mm_loadl_epi64((__m128i const *) (msbHuff->sym + rs + e* 7));
		
		// pack and transpose pass 1
		r0 = _mm_unpacklo_epi8(r0, r4);
		r1 = _mm_unpacklo_epi8(r1, r5);
		r2 = _mm_unpacklo_epi8(r2, r6);
		r3 = _mm_unpacklo_epi8(r3, r7);

		// transpose pass 2
		interleave_rows(r0, r2);
		interleave_rows(r1, r3);

		// transpose pass 3
		interleave_rows(r0, r1);
		interleave_rows(r2, r3);

		// store columns (bit-reversed index order)
		_mm_storel_epi64((__m128i *) (to_sym + s + e* 0), r0);
		_mm_storeh_pi   ((__m64 *)   (to_sym + s + e* 4), _mm_castsi128_ps(r0));
		_mm_storel_epi64((__m128i *) (to_sym + s + e* 2), r1);
		_mm_storeh_pi   ((__m64 *)   (to_sym + s + e* 6), _mm_castsi128_ps(r1));
		_mm_storel_epi64((__m128i *) (to_sym + s + e* 1), r2);
		_mm_storeh_pi   ((__m64 *)   (to_sym + s + e* 5), _mm_castsi128_ps(r2));
		_mm_storel_epi64((__m128i *) (to_sym + s + e* 3), r3);
		_mm_storeh_pi   ((__m64 *)   (to_sym + s + e* 7), _mm_castsi128_ps(r3));
	}

#undef interleave_rows
}

#endif

#elif 1 && defined(__RADNEON__)

static void newLZ_bit_reverse_hufftab(KrakenHuffTab * to_table, const KrakenMSBHuffTab * msbHuff)
{
	SIMPLEPROFILE_SCOPE(huff_bitrev_neon);
	RR_COMPILER_ASSERT( ( NEWLZ_HUFF_DECODE_TABLE_SIZE % 64 ) == 0 );
	UINTa e = NEWLZ_HUFF_DECODE_TABLE_SIZE/8;

	// NOTE(fg):
	// The idea here is to load a square 8x8 group of table entries, transpose
	// them (8x8 matrix transpose), and then store it back.
	//
	// Using bit-reversed indices on both the loads and stores makes the whole
	// process equivalent to a large index bit-reverse.
	//
	// The NEON version is a bit weirder than the x86 version because we interleave
	// the stores from the previous iteration with the loads for the current
	// iteration, since having a large block of loads at the beginning of the block
	// and a large block of stores at the end is not an efficient grouping for
	// ARM cores with separate load/store units and moderate reordering queue
	// depths (like the Cortex-A57).

	// Init registers to dummy values for the first-iteration stores.
	uint16x8_t r0,r1,r2,r3,r4,r5,r6,r7;
	KrakenHuffElem *outp = to_table->e;

	r0 = r1 = r2 = r3 = r4 = r5 = r6 = r7 = vdupq_n_u16(0);

#ifdef __RADCORTEXA57__

	// This version is optimized for the Cortex-A57's particular brand of weirdness.
	// In particular, it likes loads and stores grouped reasonably close to each other,
	// because the load/store instruction queues aren't that long. It also doesn't have
	// real 128-bit stores, they're internally double-pumped 64-bit stores, which means
	// that it's advantageous to use a transpose algorithm that leaves the interchanging
	// of 64-bit groups for last, since that can just be folded into the stores at
	// essentially no extra cost.
	//
	// In fact, it turns out that leaning even harder on the store interleaving (using NEON
	// 4-way interleaving stores) turns out to be beneficial. This gets rid of two of the
	// three transpose passes.
	//
	// This *might* be advantageous on other cores too but need to test. For now, only
	// do it when we know we run on a Cortex-A57.

    uint32x4x4_t to;

#define xpose2x2_rows(a,b)   t = vtrnq_u16(a, b), a = t.val[0], b = t.val[1]
#define store_prep(a,b,c,d)  to.val[0] = vreinterpretq_u32_u16(a), to.val[1] = vreinterpretq_u32_u16(b), to.val[2] = vreinterpretq_u32_u16(c), to.val[3] = vreinterpretq_u32_u16(d)
#define store_row(dst,i)     vst4q_lane_u32((uint32_t *) (dst), to, i)

#define store_then_load(offs0,offs1,offs2,offs3,r0,r1,r2,r3) \
    { \
        store_prep(r0, r2, r1, r3); \
        uint8x8_t len = vld1_u8(msbHuff->len + rs + (offs0)); \
        uint8x8_t sym = vld1_u8(msbHuff->sym + rs + (offs0)); \
        store_row(outp + offs0, 0); \
        r0 = vreinterpretq_u16_u8(vzipq_u8(vcombine_u8(len, vdup_n_u8(0)), vcombine_u8(sym, vdup_n_u8(0))).val[0]); \
        len = vld1_u8(msbHuff->len + rs + (offs1)); \
        sym = vld1_u8(msbHuff->sym + rs + (offs1)); \
        store_row(outp + offs1, 2); \
        r1 = vreinterpretq_u16_u8(vzipq_u8(vcombine_u8(len, vdup_n_u8(0)), vcombine_u8(sym, vdup_n_u8(0))).val[0]); \
        len = vld1_u8(msbHuff->len + rs + (offs2)); \
        sym = vld1_u8(msbHuff->sym + rs + (offs2)); \
        store_row(outp + offs2, 1); \
        r2 = vreinterpretq_u16_u8(vzipq_u8(vcombine_u8(len, vdup_n_u8(0)), vcombine_u8(sym, vdup_n_u8(0))).val[0]); \
        len = vld1_u8(msbHuff->len + rs + (offs3)); \
        sym = vld1_u8(msbHuff->sym + rs + (offs3)); \
        store_row(outp + offs3, 3); \
        r3 = vreinterpretq_u16_u8(vzipq_u8(vcombine_u8(len, vdup_n_u8(0)), vcombine_u8(sym, vdup_n_u8(0))).val[0]); \
    }

	for (UINTa s = 0; s < e; s += 8)
	{
        UINTa rs = newlz_huff_bitreverse(static_cast<U32>(s));
		uint16x8x2_t t;
        uint32x4x2_t tx;

        // store: store previous iter columns (bit-reversed index order)
		// lens: read rows (bit-reversed index order), interleave with syms
		// syms: read rows (bit-reversed index order)
        store_then_load(e*0, e*1, e*2, e*3, r0, r4, r2, r6);
        store_then_load(e*4, e*5, e*6, e*7, r1, r5, r3, r7);

        // swap antidiagonal elements within 2x2 blocks
        xpose2x2_rows(r0, r1);
        xpose2x2_rows(r2, r3);
        xpose2x2_rows(r4, r5);
        xpose2x2_rows(r6, r7);

		// the "swaps 2x2s inside 4x4" and "swap 4x4s inside 8x8" passes are elided,
		// or more precisely, they're performed by the 4-way interleaving store.

        // prepare store in next iter
        outp = to_table->e + s;
	}

    // store final columns (bit-reversed index order)
    store_prep(r0, r2, r4, r6);
    store_row(outp + e*0, 0);
    store_row(outp + e*1, 2);
    store_row(outp + e*2, 1);
    store_row(outp + e*3, 3);
    store_prep(r1, r3, r5, r7);
    store_row(outp + e*4, 0);
    store_row(outp + e*5, 2);
    store_row(outp + e*6, 1);
    store_row(outp + e*7, 3);

#undef xpose2x2_rows
#undef store_prep
#undef store_row
#undef store_then_load

#else

#define interleave_rows(a,b) t = vzipq_u16(a, b), a = t.val[0], b = t.val[1]
#define load_rows(r,a,b)     r = vreinterpretq_u16_u8(vzipq_u8(vcombine_u8(a, vdup_n_u8(0)), vcombine_u8(b, vdup_n_u8(0))).val[0])
#define store_row(dest, v)	 vst1q_u16((U16 *) (dest), (v))

	for (UINTa s = 0; s < e; s += 8)
	{
		UINTa rs = newlz_huff_bitreverse(static_cast<U32>(s));
		uint16x8x2_t t;

		// store: store results from previous iter (bit-reversed index order)
		// lens: read rows (bit-reversed index order), interleave with syms
		// syms: read rows (bit-reversed index order)
		store_row(outp + e*0, r0);
		load_rows(r0, vld1_u8(msbHuff->len + rs + e*0), vld1_u8(msbHuff->sym + rs + e*0));
		store_row(outp + e*1, r4);
		load_rows(r4, vld1_u8(msbHuff->len + rs + e*1), vld1_u8(msbHuff->sym + rs + e*1));
		store_row(outp + e*2, r2);
		load_rows(r2, vld1_u8(msbHuff->len + rs + e*2), vld1_u8(msbHuff->sym + rs + e*2));
		store_row(outp + e*3, r6);
		load_rows(r6, vld1_u8(msbHuff->len + rs + e*3), vld1_u8(msbHuff->sym + rs + e*3));
		store_row(outp + e*4, r1);
		load_rows(r1, vld1_u8(msbHuff->len + rs + e*4), vld1_u8(msbHuff->sym + rs + e*4));
		store_row(outp + e*5, r5);
		load_rows(r5, vld1_u8(msbHuff->len + rs + e*5), vld1_u8(msbHuff->sym + rs + e*5));
		store_row(outp + e*6, r3);
		load_rows(r3, vld1_u8(msbHuff->len + rs + e*6), vld1_u8(msbHuff->sym + rs + e*6));
		store_row(outp + e*7, r7);
		load_rows(r7, vld1_u8(msbHuff->len + rs + e*7), vld1_u8(msbHuff->sym + rs + e*7));

		// transpose pass 1
		interleave_rows(r0, r4);
		interleave_rows(r2, r6);
		interleave_rows(r1, r5);
		interleave_rows(r3, r7);

		// transpose pass 2
		interleave_rows(r0, r2);
		interleave_rows(r4, r6);
		interleave_rows(r1, r3);
		interleave_rows(r5, r7);

		// transpose pass 3
		interleave_rows(r0, r1);
		interleave_rows(r2, r3);
		interleave_rows(r4, r5);
		interleave_rows(r6, r7);

		// prepare store in next iter
		outp = to_table->e + s;
	}

	// final stores
	store_row(outp + e*0, r0);
	store_row(outp + e*1, r4);
	store_row(outp + e*2, r2);
	store_row(outp + e*3, r6);
	store_row(outp + e*4, r1);
	store_row(outp + e*5, r5);
	store_row(outp + e*6, r3);
	store_row(outp + e*7, r7);

#undef interleave_rows
#undef load_rows
#undef store_row

#endif
}

#elif defined(__RADWASM_SIMD128__)

// default layout
static void newLZ_bit_reverse_hufftab(KrakenHuffTab * to_table, const KrakenMSBHuffTab * msbHuff)
{
	SIMPLEPROFILE_SCOPE(huff_bitrev_default_wasm);

	RR_COMPILER_ASSERT( ( NEWLZ_HUFF_DECODE_TABLE_SIZE % 64 ) == 0 );
	UINTa e = NEWLZ_HUFF_DECODE_TABLE_SIZE/8;

	// NOTE(fg):
	// The idea here is to load a square 8x8 group of table entries, transpose
	// them (8x8 matrix transpose), and then store it back.
	//
	// Using bit-reversed indices on both the loads and stores makes the whole
	// process equivalent to a large index bit-reverse.


#define wasm_unpackhi_i16x8(a,b) wasm_i16x8_shuffle(a, b, 4, 12, 5, 13, 6, 14, 7, 15)
#define wasm_unpacklo_i16x8(a,b) wasm_i16x8_shuffle(a, b, 0, 8, 1, 9, 2, 10, 3, 11)
#define wasm_unpacklo_i8x16(a,b) wasm_i8x16_shuffle(a, b, 0, 16, 1, 17, 2, 18, 3, 19, 4, 20, 5, 21, 6, 22, 7, 23)
#define interleave_rows(a,b) t = a; a = wasm_unpacklo_i16x8(a,b); b = wasm_unpackhi_i16x8(t, b)

	for (UINTa s = 0; s < e; s += 8)
	{
		UINTa rs = newlz_huff_bitreverse(static_cast<U32>(s)); // could use a dedicated table for this but meh
		v128_t r0,r1,r2,r3,r4,r5,r6,r7,t;

		// syms: read rows (bit-reversed index order)
		r0 = wasm_v128_load64_zero(msbHuff->sym + rs + e*0);
		r1 = wasm_v128_load64_zero(msbHuff->sym + rs + e*4);
		r2 = wasm_v128_load64_zero(msbHuff->sym + rs + e*2);
		r3 = wasm_v128_load64_zero(msbHuff->sym + rs + e*6);
		r4 = wasm_v128_load64_zero(msbHuff->sym + rs + e*1);
		r5 = wasm_v128_load64_zero(msbHuff->sym + rs + e*5);
		r6 = wasm_v128_load64_zero(msbHuff->sym + rs + e*3);
		r7 = wasm_v128_load64_zero(msbHuff->sym + rs + e*7);

		// lens: read rows (bit-reversed index order), interleave with 8-bit packed syms
		r0 = wasm_unpacklo_i8x16(wasm_v128_load64_zero(msbHuff->len + rs + e*0), r0);
		r1 = wasm_unpacklo_i8x16(wasm_v128_load64_zero(msbHuff->len + rs + e*4), r1);
		r2 = wasm_unpacklo_i8x16(wasm_v128_load64_zero(msbHuff->len + rs + e*2), r2);
		r3 = wasm_unpacklo_i8x16(wasm_v128_load64_zero(msbHuff->len + rs + e*6), r3);
		r4 = wasm_unpacklo_i8x16(wasm_v128_load64_zero(msbHuff->len + rs + e*1), r4);
		r5 = wasm_unpacklo_i8x16(wasm_v128_load64_zero(msbHuff->len + rs + e*5), r5);
		r6 = wasm_unpacklo_i8x16(wasm_v128_load64_zero(msbHuff->len + rs + e*3), r6);
		r7 = wasm_unpacklo_i8x16(wasm_v128_load64_zero(msbHuff->len + rs + e*7), r7);

		// transpose pass 1
		interleave_rows(r0, r4);
		interleave_rows(r1, r5);
		interleave_rows(r2, r6);
		interleave_rows(r3, r7);

		// transpose pass 2
		interleave_rows(r0, r2);
		interleave_rows(r1, r3);
		interleave_rows(r4, r6);
		interleave_rows(r5, r7);

		// transpose pass 3
		interleave_rows(r0, r1);
		interleave_rows(r2, r3);
		interleave_rows(r4, r5);
		interleave_rows(r6, r7);

		#define STORE_ROW(dest, vals) wasm_v128_store((dest), (vals))

		// store columns (bit-reversed index order)
		STORE_ROW(to_table->e + s + e*0, r0);
		STORE_ROW(to_table->e + s + e*4, r1);
		STORE_ROW(to_table->e + s + e*2, r2);
		STORE_ROW(to_table->e + s + e*6, r3);
		STORE_ROW(to_table->e + s + e*1, r4);
		STORE_ROW(to_table->e + s + e*5, r5);
		STORE_ROW(to_table->e + s + e*3, r6);
		STORE_ROW(to_table->e + s + e*7, r7);

		#undef STORE_ROW
	}

#undef interleave_rows
#undef wasm_unpacklo_i8x16
#undef wasm_unpacklo_i16x8
#undef wasm_unpackhi_i16x8
}

#else // not x86 or ARM with NEON, use generic fallback

static void newLZ_bit_reverse_hufftab(KrakenHuffTab * to_table, const KrakenMSBHuffTab * msbHuff )
{
	SIMPLEPROFILE_SCOPE(huff_bitrev_fallback);
	for LOOP(s,NEWLZ_HUFF_DECODE_TABLE_SIZE)
	{	
		int rs = newlz_huff_bitreverse(s);

		// write linear order, read shuffle order :
		NEWLZ_HUFF_INIT(to_table, s, msbHuff->len[rs], msbHuff->sym[rs]);
	}
}

#endif

// NOTE: keep this in sync with ASM struct defn in newlz_huff_common.inc!
struct KrakenHuffState
{
	U8 *decodeptr[2];		// Current write cursor for the two stream triples
	U8 *decodeend[2];		// End of decoded bytes buffer for the two stream triples

	const U8 *strm0_end[2];	// End of stream pointer for streams 0/3

	const U8 *bitp[6];		// Next byte to be read for the streams
	U32 bits[6];			// Current contents of bit buffer
	U32 bitc[6];			// Current number of valid bits in bit buffer

#ifndef NEWLZ_NO_HUFFTAB_IN_STATE
	// align should generate no padding here; it's mostly docs
	RAD_ALIGN(KrakenHuffTab, table, 16);
#endif
};

// When we don't have a dedicated 6-stream decoder, we decode the two 3-stream halves
// seperately. Modify the KrakenHuffState to move the pointers and state for the second
// half into the state fields for the first half, allowing us to use a regular unmodified
// 3-stream decoder for both halves.
static void newlz_huff_switch_to_second_half(KrakenHuffState * s)
{
	RR_ASSERT(s->decodeend[0] != s->decodeend[1]); // catch someone calling this twice

	s->decodeptr[0] = s->decodeptr[1];
	s->decodeend[0] = s->decodeend[1];
	s->strm0_end[0] = s->strm0_end[1];

	memmove(&s->bitp[0], &s->bitp[3], 3 * sizeof(s->bitp[0]));
	memmove(&s->bits[0], &s->bits[3], 3 * sizeof(s->bits[0]));
	memmove(&s->bitc[0], &s->bitc[3], 3 * sizeof(s->bitc[0]));
}

static bool newlz_huff_precise_finish(KrakenHuffState * s, const KrakenHuffTab * htab, bool is_huff6)
{
	for (;;)
	{
		const U8 * in0 = s->bitp[0];
		const U8 * in1 = s->bitp[1];
		const U8 * in2 = s->bitp[2];

		U32 bits0 = s->bits[0], bitc0 = s->bitc[0];
		U32 bits1 = s->bits[1], bitc1 = s->bitc[1];
		U32 bits2 = s->bits[2], bitc2 = s->bitc[2];

		if (in0 > in2)
			return false;

		U8 *decodeptr = s->decodeptr[0];
		U8 *decodeend = s->decodeend[0];
		SIMPLEPROFILE_SCOPE_N(huff_careful_loop,(decodeend - decodeptr));

		#define DECONE(strm) \
			peek = bits##strm & NEWLZ_HUFF_DECODE_TABLE_MASK; \
			cl = NEWLZ_HUFF_LEN(htab, peek); \
			sym = NEWLZ_HUFF_SYM(htab, peek); \
			bits##strm >>= cl; bitc##strm -= cl; \
			*decodeptr++ = (U8) sym

		#define DECTHREE() \
			DECONE(0); \
			DECONE(1); \
			DECONE(2)

		RR_COMPILER_ASSERT( NEWLZ_HUFF_CODELEN_LIMIT <= 12 );	
		#define N_DECS_PER_REFILL		2
		#define TRIPLE_DECS_PER_REFILL	(3*N_DECS_PER_REFILL)

		// bulk loop to get within 4B of end
		if (in1 - in2 >= 4 && decodeend - decodeptr >= TRIPLE_DECS_PER_REFILL)
		{
			in1 -= 4;
			decodeend -= TRIPLE_DECS_PER_REFILL-1;

			while (decodeptr < decodeend)
			{
				// non-crossing invariant: in0 <= in2 && in2 <= in1
				if (in0 > in2 || in2 > in1)
					break;

				// non-crossing and 4B access size guarantee that the
				// following reads are safe; the decodeend decrement before the
				// loop guarantees that we don't write out of bounds.

				// refill :
				bits0 |= RR_GET32_LE(in0) << bitc0;
				in0 += (31 - bitc0)>>3; // bytes_consumed
				bitc0 |= 24; // same as += bytes_consumed<<3 here!

				bits1 |= RR_GET32_BE(in1) << bitc1;
				in1 -= (31 - bitc1)>>3; // bytes_consumed
				bitc1 |= 24; // same as += bytes_consumed<<3 here!

				bits2 |= RR_GET32_LE(in2) << bitc2;
				in2 += (31 - bitc2)>>3; // bytes_consumed
				bitc2 |= 24; // same as += bytes_consumed<<3 here!

				U32 peek; int cl; int sym;
				
				RR_COMPILER_ASSERT( N_DECS_PER_REFILL == 2 );
				DECTHREE();
				DECTHREE();
			}

			decodeend += TRIPLE_DECS_PER_REFILL-1;
			in1 += 4;

			// transition to final loop
			in0 -= (bitc0 >> 3); bitc0 &= 7;
			in1 += (bitc1 >> 3); bitc1 &= 7;
			in2 -= (bitc2 >> 3); bitc2 &= 7;
		}
			
		// Final loop. This is really careful about the bytes it accesses.
		while (decodeptr < decodeend)
		{
			U32 peek, cl, sym;

			// refill to >=16b in bit0 buf
			if (in2 - in0 > 1)
				bits0 |= RR_GET16_LE(in0) << bitc0;
			else if (in2 - in0 == 1)
				bits0 |= in0[0] << bitc0;

			DECONE(0);
			in0 += (7 - bitc0) >> 3;
			bitc0 &= 7;

			if (decodeptr >= decodeend)
				break;

			// refill to >=16b left in bit1, bit2 bufs
			if (in1 - in2 > 1)
			{
				bits1 |= RR_GET16_BE(in1 - 2) << bitc1;
				bits2 |= RR_GET16_LE(in2) << bitc2;
			}
			else if (in1 - in2 == 1)
			{
				// accessing the same byte!
				bits1 |= in2[0] << bitc1;
				bits2 |= in2[0] << bitc2;
			}

			DECONE(1);
			in1 -= (7 - bitc1) >> 3;
			bitc1 &= 7;

			if (decodeptr >= decodeend)
				break;

			DECONE(2);
			in2 += (7 - bitc2) >> 3;
			bitc2 &= 7;

			if (in0 > in2 || in2 > in1) // corruption check
				return false;
		}

		if (decodeptr != decodeend)
			return false;

		#undef DECONE
		#undef DECTHREE
		#undef N_DECS_PER_REFILL
		#undef TRIPLE_DECS_PER_REFILL

		// Final bounds check:
		// in0, in1, in2 all should point at their respective
		// stream end marks now!
		if (in0 != s->strm0_end[0] || in1 != in2)
			return false;

		// huff3 -> we're done; else prep for second pass.
		if (!is_huff6)
			break;

		newlz_huff_switch_to_second_half(s);
		is_huff6 = false;
	}

	return true;
}

// NOTE(fg): just funneling into the final loop directly was actually faster than using a specialized
// loop a la newlz_huff64 above. Still slower than the original specialized loop from before that I
// copied the code from though - compiler SNAFU.
//
// Since precise_finish actually has a competent 32-bit-at-a-time impl (just with bounds checking
// every iter), this is not as wasteful as it might sound.
static bool newlz_huff32(KrakenHuffState * s, const KrakenMSBHuffTab * huff,bool is_huff6)
{
#ifdef NEWLZ_NO_HUFFTAB_IN_STATE
	RAD_ALIGN(KrakenHuffTab, hufftabdata, 16);
#else
	KrakenHuffTab &hufftabdata = s->table;
#endif

	const KrakenHuffTab * phufftab = newLZ_prep_hufftab(&hufftabdata,huff);

	// just use the final loop, it works well enough here!
	return newlz_huff_precise_finish(s, phufftab, is_huff6);
}

static bool newlz_huff64(KrakenHuffState * s, const KrakenMSBHuffTab * huff,bool is_huff6)
{
#ifdef NEWLZ_NO_HUFFTAB_IN_STATE
	RAD_ALIGN(KrakenHuffTab, hufftabdata, 16);
#else
	KrakenHuffTab &hufftabdata = s->table;
#endif
	const KrakenHuffTab * phufftab = newLZ_prep_hufftab(&hufftabdata,huff);

	for (;;)
	{
		// Layout: strm0-> | strm2-> | <-strm1
		const U8 * in0 = s->bitp[0];
		const U8 * in1 = s->bitp[1];
		const U8 * in2 = s->bitp[2];

		U8 * decodeptr = s->decodeptr[0];
		U8 * decodeend = s->decodeend[0];

		// NEWLZ_HUFF_CODELEN_LIMIT == 11 , could actually do 5 per refill = 10 per loop
		#if (56/NEWLZ_HUFF_CODELEN_LIMIT) >= 5
		#define N_DECS_PER_REFILL		5
		#elif (56/NEWLZ_HUFF_CODELEN_LIMIT) >= 4
		#define N_DECS_PER_REFILL		4
		#else
		#define N_DECS_PER_REFILL		3
		#endif
		#define TRIPLE_DECS_PER_REFILL	(3*N_DECS_PER_REFILL)
			
		// bulk loop
		if (decodeend - decodeptr > TRIPLE_DECS_PER_REFILL-1)
		{
			SIMPLEPROFILE_SCOPE_N(huff64_loop,(decodeend - decodeptr));

			// offset the end marker so we only run with full groups left
			decodeend -= TRIPLE_DECS_PER_REFILL-1;

			// typedef U64 LENTYPE;
			// typedef SINTr LENTYPE;
			typedef int LENTYPE;

			// bit buffers start empty
			U64 bits0=0,bits1=0,bits2=0;
			LENTYPE bitcount0=0,bitcount1=0,bitcount2=0;		
			
			#define DECONE(strm) \
				peek = bits##strm & NEWLZ_HUFF_DECODE_TABLE_MASK; \
				cl = NEWLZ_HUFF_LEN(phufftab, peek); \
				sym = NEWLZ_HUFF_SYM(phufftab, peek); \
				bits##strm >>= cl; bitcount##strm -= cl; \
				*decodeptr++ = (U8) sym

			#define DECTHREE() \
				DECONE(0); \
				DECONE(1); \
				DECONE(2)

			static const SINTa nIterCheck = 8; // Number of iterations of main decode loop to run between checks
			static const SINTa nBytesCheck = nIterCheck * TRIPLE_DECS_PER_REFILL; // Number of bytes written between checks
			static const SINTa nBytesDecOverRead = 7; // Number of bytes we can over-read past read pointer. A 8-byte access gives 7B.
			static const SINTa nBytesDecMax = (nBytesCheck * NEWLZ_HUFF_CODELEN_LIMIT + 7) / 8 + nBytesDecOverRead; // Max bytes accessed in one run of decoder loop

			for (;;)
			{
				SINTa nBytesDecLeft = decodeend - decodeptr;
				if (nBytesDecLeft <= 0)
					break;
				
				// Break out of main decoder loop one decodeptr >= decodemark
				// Which is either nIterCheck iters of main loop or the end of the output
				// buffer, whichever is earlier.
				U8 *decodemark = decodeptr + nBytesCheck;
				if (nBytesDecLeft < nBytesCheck)
					decodemark = decodeend;

				// Go to careful loop once stream pointers crossed or are at risk of
				// crossing within one outer iteration.
				if_unlikely (in0 > in2 || (in1 - in2) < nBytesDecMax)
					break;

				// Main loop
				while (decodeptr < decodemark)
				{
					// refill :
					U64 next0 = RR_GET64_LE(in0);
					bits0 |= next0 << bitcount0;
					in0 += (63 - bitcount0)>>3; // bytes_consumed
					bitcount0 |= 56; // same as += bytes_consumed<<3 here!

					U64 next1 = RR_GET64_BE(in1 - 8);
					bits1 |= next1 << bitcount1;
					in1 -= (63 - bitcount1)>>3; // bytes_consumed
					bitcount1 |= 56; // same as += bytes_consumed<<3 here!

					U64 next2 = RR_GET64_LE(in2);
					bits2 |= next2 << bitcount2;
					in2 += (63 - bitcount2)>>3; // bytes_consumed
					bitcount2 |= 56; // same as += bytes_consumed<<3 here!

					// variable type of "sym" doesn't seem to matter; int/U64/U8 all the same
					U64 peek; LENTYPE cl; U8 sym;
					
					RR_COMPILER_ASSERT( N_DECS_PER_REFILL >= 3 && N_DECS_PER_REFILL <= 5 );
					DECTHREE();
					DECTHREE();
					DECTHREE();
					#if N_DECS_PER_REFILL > 3
					DECTHREE();
					#endif
					#if N_DECS_PER_REFILL > 4
					DECTHREE();
					#endif
				}
			}

			#undef DECONE
			#undef DECTHREE

			// transition to careful loop
			s->decodeptr[0] = decodeptr;
			s->bitp[0] = in0 - (bitcount0 >> 3); s->bits[0] = (U32) (bits0 & 0xff); s->bitc[0] = bitcount0 & 7;
			s->bitp[1] = in1 + (bitcount1 >> 3); s->bits[1] = (U32) (bits1 & 0xff); s->bitc[1] = bitcount1 & 7;
			s->bitp[2] = in2 - (bitcount2 >> 3); s->bits[2] = (U32) (bits2 & 0xff); s->bitc[2] = bitcount2 & 7;
		}

		#undef N_DECS_PER_REFILL
		#undef TRIPLE_DECS_PER_REFILL

		if (!newlz_huff_precise_finish(s, phufftab, false))
			return false;

		// huff3 -> we're done; else prep for second pass.
		if (!is_huff6)
			break;

		newlz_huff_switch_to_second_half(s);
		is_huff6 = false;
	}

	return true;
}

#ifdef NEWLZ_ARM32

static RADNOINLINE bool newlz_huff32_arm(KrakenHuffState * s, const KrakenMSBHuffTab * huff, bool is_huff6)
{
	RAD_ALIGN(KrakenHuffTab, hufftabdata, 16);
	const KrakenHuffTab * phufftab = newLZ_prep_hufftab(&hufftabdata,huff);

	for (;;)
	{
		// Layout: strm0-> | strm2-> | <-strm1
		const U8 * in0 = s->bitp[0];
		const U8 * in1 = s->bitp[1];
		const U8 * in2 = s->bitp[2];

		U8 * RADRESTRICT decodeptr = s->decodeptr[0];
		U8 * decodeend = s->decodeend[0];

		#define N_DECS_PER_REFILL		2
		#define TRIPLE_DECS_PER_REFILL	(3*N_DECS_PER_REFILL)

		// bulk loop
		if (decodeend - decodeptr > TRIPLE_DECS_PER_REFILL-1 && in1 - in2 > 4)
		{
			SIMPLEPROFILE_SCOPE_N(huff32_arm_loop,(decodeend - decodeptr));

			// offset the end marker so we only run with full groups left
			// NOTE we decrement by 1 more than the usual TRIPLE_DECS_PER_REFILL-1 here,
			// because the loop condition in this variant is "<= decodeend" not "< decodeend".
			decodeend -= TRIPLE_DECS_PER_REFILL;
			in1 -= 4;

			// bit buffers start empty
			U32 bits0 = 0, bits1 = 0, bits2 = 0;

			// check initial non-crossing (NOTE: in1 > in2 we already established below, but just be explicit here)
			if (in0 <= in2 && in2 <= in1)
			{
			#if 1 && (defined(__GNUC__) || defined(__clang__))
				U32 tmp0, tmp1, tmp2;

				RR_COMPILER_ASSERT(sizeof(KrakenHuffElem) == 2); // this is the lsl #1 shift
				// NOTE: this is scheduled, see newlz_arrays_huff_arm.txt for unscheduled version

				// Sketchy produces this schedule
				// NOTE this also uses the trick of moving the final CLZ up and adding in the final len later
				__asm__(
					"sub		%[decptr], %[decptr], #1\n"				// pre-decrement since we use pre-index addressing
					"1:\n"
					"ldr		%[tmp1], [%[in1]]\n"					// next1
					"and		%[bits0], %[bits0], #7\n"				// leftover0
					"ldr		%[tmp0], [%[in0]]\n"					// next0
					"and		%[bits1], %[bits1], #7\n"				// leftover1
					"ldr		%[tmp2], [%[in2]]\n"					// next2
					"and		%[bits2], %[bits2], #7\n"				// leftover2
					"rev		%[tmp1], %[tmp1]\n"						// next1 BE->LE
					"orr		%[tmp0], %[tmp0], #0x80000000\n"		// bits0 marker
					"orr		%[tmp1], %[tmp1], #0x80000000\n"		// bits1 marker
					"orr		%[tmp2], %[tmp2], #0x80000000\n"		// bits2 marker
					"lsr		%[bits1], %[tmp1], %[bits1]\n"			// consume leftover1
					"pld		[%[in0], #64]\n"						// prefetch in0
					"lsr		%[bits0], %[tmp0], %[bits0]\n"			// consume leftover0
					"and		%[tmp1], %[mask], %[bits1], lsl #1\n"	// BYTE 1 table index
					"lsr		%[bits2], %[tmp2], %[bits2]\n"			// consume leftover2
					"and		%[tmp0], %[mask], %[bits0], lsl #1\n"	// BYTE 0 table index
					"and		%[tmp2], %[mask], %[bits2], lsl #1\n"	// BYTE 2 table index
					"pld		[%[in1], #-64]\n"						// prefetch in1
					"ldrh		%[tmp1], [%[phuff], %[tmp1]]\n"			// BYTE 1 table grab
					"ldrh		%[tmp0], [%[phuff], %[tmp0]]\n"			// BYTE 0 table grab
					"ldrh		%[tmp2], [%[phuff], %[tmp2]]\n"			// BYTE 2 table grab
					"lsr		%[bits1], %[bits1], %[tmp1]\n"			// BYTE 1 consume
					"pld		[%[in2], #64]\n"						// prefetch in2
					"lsr		%[bits0], %[bits0], %[tmp0]\n"			// BYTE 0 consume
					"rev16		%[tmp1], %[tmp1]\n"						// BYTE 1 swap sym into low byte
					"lsr		%[bits2], %[bits2], %[tmp2]\n"			// BYTE 2 consume
					"rev16		%[tmp0], %[tmp0]\n"						// BYTE 0 swap sym into low byte
					"strb		%[tmp0], [%[decptr], #1]\n"				// BYTE 0 emit
					"rev16		%[tmp2], %[tmp2]\n"						// BYTE 2 swap sym into low byte
					"strb		%[tmp1], [%[decptr], #2]\n"				// BYTE 1 emit
					"and		%[tmp0], %[mask], %[bits0], lsl #1\n"	// BYTE 3 table index
					"strb		%[tmp2], [%[decptr], #3]\n"				// BYTE 2 emit
					"and		%[tmp1], %[mask], %[bits1], lsl #1\n"	// BYTE 4 table index
					"and		%[tmp2], %[mask], %[bits2], lsl #1\n"	// BYTE 5 table index
					"clz		%[bits0], %[bits0]\n"					// ADV  0 lz count
					"ldrh		%[tmp0], [%[phuff], %[tmp0]]\n"			// BYTE 3 table grab
					"clz		%[bits1], %[bits1]\n"					// ADV  1 lz count
					"ldrh		%[tmp1], [%[phuff], %[tmp1]]\n"			// BYTE 4 table grab
					"clz		%[bits2], %[bits2]\n"					// ADV  2 lz count
					"ldrh		%[tmp2], [%[phuff], %[tmp2]]\n"			// BYTE 5 table grab
					"rev16		%[tmp0], %[tmp0]\n"						// BYTE 3 swap sym/len bytes
					"strb		%[tmp0], [%[decptr], #4]\n"				// BYTE 3 emit
					"rev16		%[tmp1], %[tmp1]\n"						// BYTE 4 swap sym/len bytes
					"strb		%[tmp1], [%[decptr], #5]\n"				// BYTE 4 emit
					"rev16		%[tmp2], %[tmp2]\n"						// BYTE 5 swap sym/len bytes
					"add		%[bits0], %[bits0], %[tmp0], lsr #8\n"	// ADV  0 final lz count
					"add		%[bits1], %[bits1], %[tmp1], lsr #8\n"	// ADV  1 final lz count
					"strb		%[tmp2], [%[decptr], #6]!\n"			// BYTE 5 emit and decode ptr advance
					"add		%[bits2], %[bits2], %[tmp2], lsr #8\n"	// ADV  2 final lz count
					"add		%[in0], %[in0], %[bits0], lsr #3\n"		// ADV  0 advance
					"cmp  		%[decptr], %[decend]\n"					// decodeptr <= decodeend
					"sub		%[in1], %[in1], %[bits1], lsr #3\n"		// ADV  1 advance
					"add		%[in2], %[in2], %[bits2], lsr #3\n"		// ADV  2 advance

					// loop
					"it 		ls\n"
					"cmpls 		%[in2], %[in1]\n"						// && in2 <= in1
					"it			ls\n"
					"cmpls		%[in0], %[in2]\n"						// && in0 <= in2
					"bls		1b\n"

					"add		%[decptr], %[decptr], #1\n"				// undo pre-decrement
					:	[bits0]"+r"(bits0),	[tmp0]"=&r"(tmp0), [in0]"+r"(in0),
						[bits1]"+r"(bits1),	[tmp1]"=&r"(tmp1), [in1]"+r"(in1),
						[bits2]"+r"(bits2),	[tmp2]"=&r"(tmp2), [in2]"+l"(in2), /* +l here to allocate in reg# 0-7, so the final two cmpls can encode as 16-bit forms in thumb */
						[decptr]"+r"(decodeptr)
					:	[decend]"r"(decodeend - 1 /*-1 for decodeptr predecrement*/), [phuff]"r"(phufftab), [mask]"r"(NEWLZ_HUFF_DECODE_TABLE_MASK << 1)
				);

				bits0 &= 7;
				bits1 &= 7;
				bits2 &= 7;
			#else
				#define DECONE(strm) \
					pelem = &phufftab->e[bits##strm & NEWLZ_HUFF_DECODE_TABLE_MASK]; \
					bits##strm >>= pelem->len; \
					*decodeptr++ = pelem->sym

				#define DECTHREE() \
					DECONE(0); \
					DECONE(1); \
					DECONE(2)

				do
				{
					const KrakenHuffElem * pelem;

					bits0 &= 7;
					bits1 &= 7;
					bits2 &= 7;

					// prefetch ahead (the forward/backward mix seems to _really_ confuse
					// several of the ARM cores)
					RR_PREFETCHR_CL(in0 + 64);
					RR_PREFETCHR_CL(in1 - 64);
					RR_PREFETCHR_CL(in2 + 64);

					// refill :
					bits0 = (RR_GET32_LE(in0) | 0x80000000u) >> bits0;
					bits1 = (RR_GET32_BE(in1) | 0x80000000u) >> bits1;
					bits2 = (RR_GET32_LE(in2) | 0x80000000u) >> bits2;

					RR_COMPILER_ASSERT( N_DECS_PER_REFILL == 2 );
					DECTHREE();
					DECTHREE();

					// advance
					bits0 = rrClz32(bits0); in0 += bits0 >> 3;
					bits1 = rrClz32(bits1); in1 -= bits1 >> 3;
					bits2 = rrClz32(bits2); in2 += bits2 >> 3;
				} while (decodeptr <= decodeend && in0 <= in2 && in2 <= in1); // while not done and not crossed

				bits0 &= 7;
				bits1 &= 7;
				bits2 &= 7;

				#undef DECONE
				#undef DECTHREE
			#endif
			}

			in1 += 4;

			// transition to careful loop
			s->decodeptr[0] = decodeptr;

			// if pointers actually crossed, it's definitely bad
			if (in0 > in2 || in2 > in1)
				return false;

			if (in2 == in1) // this can work, but only if both streams have no bits left
			{
				if (bits1 || bits2)
					return false;
			}

			#define FINISHSTREAM(strm,refillexpr) \
				if (bits##strm) { \
					s->bits[strm] = (refillexpr) >> bits##strm; \
					s->bitc[strm] = 8 - bits##strm; \
				} \
				s->bitp[strm] = in##strm

			FINISHSTREAM(0, *in0++);
			FINISHSTREAM(1, *--in1);
			FINISHSTREAM(2, *in2++);

			#undef FINISHSTREAM
		}

		#undef N_DECS_PER_REFILL
		#undef TRIPLE_DECS_PER_REFILL

		if (!newlz_huff_precise_finish(s, phufftab, false))
			return false;

		// huff3 -> we're done; else prep for second pass.
		if (!is_huff6)
			break;

		newlz_huff_switch_to_second_half(s);
		is_huff6 = false;
	}

	return true;
}

#endif

#ifdef NEWLZ_ARM64

extern "C" bool oodle_newLZ_huff3_a64_kern(KrakenHuffState *s, const KrakenHuffTab *htab);
extern "C" bool oodle_newLZ_huff6_a64_kern(KrakenHuffState *s, const KrakenHuffTab *htab);

static bool newlz_huff64_arm(KrakenHuffState * s, const KrakenMSBHuffTab * huff, bool is_huff6)
{
	RAD_ALIGN(KrakenHuffTab, hufftabdata, 16);

	const KrakenHuffTab * phufftab = newLZ_prep_hufftab(&hufftabdata,huff);

	if (is_huff6)
	{
	#ifdef NEWLZ_ARM64_HUFF_ASM
		SIMPLEPROFILE_SCOPE_N(huff6_arm64_asm,(s->decodeend[1] - s->decodeptr[0]));
		if (!oodle_newLZ_huff6_a64_kern(s, phufftab))
			return false;
	#else
		// Layout: strm0-> | strm2-> | <-strm1
		//         strm3-> | strm5-> | <-strm4
		const U8 * in0 = s->bitp[0];
		const U8 * in1 = s->bitp[1];
		const U8 * in2 = s->bitp[2];
		const U8 * in3 = s->bitp[3];
		const U8 * in4 = s->bitp[4];
		const U8 * in5 = s->bitp[5];

		U8 * decodeptr0 = s->decodeptr[0];
		U8 * decodeptr1 = s->decodeptr[1];
		U8 * decodeend1 = s->decodeend[1];

		// Extra invariant: first stream is the longer one
		RR_ASSERT( s->decodeend[0] - decodeptr0 >= decodeend1 - decodeptr1 );

		// NEWLZ_HUFF_CODELEN_LIMIT == 11 , could actually do 5 per refill = 10 per loop
		#if (56/NEWLZ_HUFF_CODELEN_LIMIT) >= 5
		#define N_DECS_PER_REFILL		5
		#elif (56/NEWLZ_HUFF_CODELEN_LIMIT) >= 4
		#define N_DECS_PER_REFILL		4
		#else
		#define N_DECS_PER_REFILL		3
		#endif
		#define TRIPLE_DECS_PER_REFILL	(3*N_DECS_PER_REFILL)

		// bulk loop
		if (decodeend1 - decodeptr1 > TRIPLE_DECS_PER_REFILL-1 && in1 - in2 > 8 && in4 - in5 > 8 )
		{
			SIMPLEPROFILE_SCOPE_N(huff6_arm64_loop,(decodeend1 - decodeptr0));

			// offset the end marker so we only run with full groups left
			decodeend1 -= TRIPLE_DECS_PER_REFILL-1;
			in1 -= 8;
			in4 -= 8;

			// bit buffers start empty
			U64 bits0=0,bits1=0,bits2=0;
			U64 bits3=0,bits4=0,bits5=0;
			const U8 *hufftab_base = &phufftab->e[0].len;

			#define DECONE(strm, outp) \
				/* NOTE(fg): This address calc is a single UBFIZ */ \
				tabv = (bits##strm & NEWLZ_HUFF_DECODE_TABLE_MASK) * sizeof(KrakenHuffElem); \
				tabv = RR_GET16_LE((const U16 *) (hufftab_base + tabv)); \
				/* NOTE(fg): ARM64 shift implicitly does & 63 on shift count. */ \
				/* Would love to write that explicitly (avoiding UB), but that */ \
				/* results in extra code :/ */ \
				bits##strm >>= tabv; \
				*decodeptr##outp++ = (U8) (tabv >> 8)

			#define DECSIX() \
				DECONE(0,0); \
				DECONE(1,0); \
				DECONE(2,0); \
				DECONE(3,1); \
				DECONE(4,1); \
				DECONE(5,1)

			do
			{
				// non-crossing invariant: in0 <= in2 && in2 <= in1
				// (and same for second triple)
				if_unlikely (in0 > in2)
					break;
				if_unlikely (in2 > in1)
					break;
				if_unlikely (in3 > in5)
					break;
				if_unlikely (in5 > in4)
					break;

				// refill :
				bits0 = (RR_GET64_LE(in0) | (1ull << 63)) >> (bits0 & 7);
				bits1 = (RR_GET64_BE(in1) | (1ull << 63)) >> (bits1 & 7);
				bits2 = (RR_GET64_LE(in2) | (1ull << 63)) >> (bits2 & 7);
				bits3 = (RR_GET64_LE(in3) | (1ull << 63)) >> (bits3 & 7);
				bits4 = (RR_GET64_BE(in4) | (1ull << 63)) >> (bits4 & 7);
				bits5 = (RR_GET64_LE(in5) | (1ull << 63)) >> (bits5 & 7);

				U32 tabv;
				
				RR_COMPILER_ASSERT( N_DECS_PER_REFILL >= 3 && N_DECS_PER_REFILL <= 5 );
				DECSIX();
				DECSIX();
				DECSIX();
				#if N_DECS_PER_REFILL > 3
				DECSIX();
				#endif
				#if N_DECS_PER_REFILL > 4
				DECSIX();
				#endif

				// advance
				bits0 = rrClz64(bits0); in0 += bits0 >> 3;
				bits1 = rrClz64(bits1); in1 -= bits1 >> 3;
				bits2 = rrClz64(bits2); in2 += bits2 >> 3;
				bits3 = rrClz64(bits3); in3 += bits3 >> 3;
				bits4 = rrClz64(bits4); in4 -= bits4 >> 3;
				bits5 = rrClz64(bits5); in5 += bits5 >> 3;
			} while (decodeptr1 < decodeend1);

			#undef DECONE
			#undef DECSIX

			in1 += 8;
			in4 += 8;

			// transition to careful loop
			s->decodeptr[0] = decodeptr0;
			s->decodeptr[1] = decodeptr1;
			bits0 &= 7;
			bits1 &= 7;
			bits2 &= 7;
			bits3 &= 7;
			bits4 &= 7;
			bits5 &= 7;

			// if pointers actually crossed, it's definitely bad
			if (in0 > in2 || in2 > in1 || in3 > in5 || in5 > in4)
				return false;

			if (in2 == in1) // this can work, but only if both streams have no bits left
			{
				if (bits1 || bits2)
					return false;
			}

			if (in5 == in4) // this can work, but only if both streams have no bits left
			{
				if (bits4 || bits5)
					return false;
			}

			#define FINISHSTREAM(strm,refillexpr) \
				if (bits##strm) { \
					s->bits[strm] = (refillexpr) >> bits##strm; \
					s->bitc[strm] = (U32)(8 - bits##strm); \
				} \
				s->bitp[strm] = in##strm

			FINISHSTREAM(0, *in0++);
			FINISHSTREAM(1, *--in1);
			FINISHSTREAM(2, *in2++);
			FINISHSTREAM(3, *in3++);
			FINISHSTREAM(4, *--in4);
			FINISHSTREAM(5, *in5++);

			#undef FINISHSTREAM
		}

		#undef N_DECS_PER_REFILL
		#undef TRIPLE_DECS_PER_REFILL
	#endif
	}
	else
	{
	#ifdef NEWLZ_ARM64_HUFF_ASM
		SIMPLEPROFILE_SCOPE_N(huff3_arm64_asm,(s->decodeend[0] - s->decodeptr[0]));
		if (!oodle_newLZ_huff3_a64_kern(s, phufftab))
			return false;
	#else
		// Layout: strm0-> | strm2-> | <-strm1
		const U8 * in0 = s->bitp[0];
		const U8 * in1 = s->bitp[1];
		const U8 * in2 = s->bitp[2];

		U8 * decodeptr = s->decodeptr[0];
		U8 * decodeend = s->decodeend[0];

		// NEWLZ_HUFF_CODELEN_LIMIT == 11 , could actually do 5 per refill = 10 per loop
		#if (56/NEWLZ_HUFF_CODELEN_LIMIT) >= 5
		#define N_DECS_PER_REFILL		5
		#elif (56/NEWLZ_HUFF_CODELEN_LIMIT) >= 4
		#define N_DECS_PER_REFILL		4
		#else
		#define N_DECS_PER_REFILL		3
		#endif
		#define TRIPLE_DECS_PER_REFILL	(3*N_DECS_PER_REFILL)

		// bulk loop
		if (decodeend - decodeptr > TRIPLE_DECS_PER_REFILL-1 && in1 - in2 > 8)
		{
			SIMPLEPROFILE_SCOPE_N(huff3_arm64_loop,(decodeend - decodeptr));

			// offset the end marker so we only run with full groups left
			decodeend -= TRIPLE_DECS_PER_REFILL-1;
			in1 -= 8;

			// bit buffers start empty
			U64 bits0=0,bits1=0,bits2=0;
			U64 bitcount0=0,bitcount1=0,bitcount2=0;
			const U8 *hufftab_base = &phufftab->e[0].len;

			#define DECONE(strm) \
				/* NOTE(fg): This address calc is a single UBFIZ */ \
				tabv = (bits##strm & NEWLZ_HUFF_DECODE_TABLE_MASK) * sizeof(KrakenHuffElem); \
				tabv = RR_GET16_LE((const U16 *) (hufftab_base + tabv)); \
				bits##strm >>= tabv; bitcount##strm -= tabv; \
				*decodeptr++ = (U8) (tabv >> 8)

			#define DECTHREE() \
				DECONE(0); \
				DECONE(1); \
				DECONE(2)

			while (decodeptr < decodeend)
			{
				// non-crossing invariant: in0 <= in2 && in2 <= in1
				if_unlikely (in0 > in2)
					break;
				if_unlikely (in2 > in1)
					break;

				// refill :
				U64 next0 = RR_GET64_LE(in0);
				bits0 |= next0 << bitcount0;
				in0 += (63 - bitcount0)>>3; // bytes_consumed
				bitcount0 |= 56; // same as += bytes_consumed<<3 here!

				U64 next1 = RR_GET64_BE(in1);
				bits1 |= next1 << bitcount1;
				in1 -= (63 - bitcount1)>>3; // bytes_consumed
				bitcount1 |= 56; // same as += bytes_consumed<<3 here!

				U64 next2 = RR_GET64_LE(in2);
				bits2 |= next2 << bitcount2;
				in2 += (63 - bitcount2)>>3; // bytes_consumed
				bitcount2 |= 56; // same as += bytes_consumed<<3 here!

				U32 tabv;
				
				RR_COMPILER_ASSERT( N_DECS_PER_REFILL >= 3 && N_DECS_PER_REFILL <= 5 );
				DECTHREE();
				DECTHREE();
				DECTHREE();
				#if N_DECS_PER_REFILL > 3
				DECTHREE();
				#endif
				#if N_DECS_PER_REFILL > 4
				DECTHREE();
				#endif

				// our decode process puts some crap in the top bits; clear them
				bitcount0 &= 63;
				bitcount1 &= 63;
				bitcount2 &= 63;
			}

			#undef DECONE
			#undef DECTHREE

			in1 += 8;

			// transition to careful loop
			s->decodeptr[0] = decodeptr;
			s->bitp[0] = in0 - (bitcount0 >> 3); s->bits[0] = (U32) (bits0 & 0xff); s->bitc[0] = bitcount0 & 7;
			s->bitp[1] = in1 + (bitcount1 >> 3); s->bits[1] = (U32) (bits1 & 0xff); s->bitc[1] = bitcount1 & 7;
			s->bitp[2] = in2 - (bitcount2 >> 3); s->bits[2] = (U32) (bits2 & 0xff); s->bitc[2] = bitcount2 & 7;
		}

		#undef N_DECS_PER_REFILL
		#undef TRIPLE_DECS_PER_REFILL
	#endif
	}

	return newlz_huff_precise_finish(s, phufftab, is_huff6);
}

#endif

#if defined NEWLZ_X86SSE2_HUFF_ASM

extern "C" bool oodle_newLZ_huffx86_sse2_kern(KrakenHuffState *s);
extern "C" bool oodle_newLZ_huff6_x86_sse2_kern(KrakenHuffState *s);

static bool newlz_huff_x86sse2(KrakenHuffState * s, const KrakenMSBHuffTab * huff, bool is_huff6)
{
	const KrakenHuffTab * phufftab = newLZ_prep_hufftab(&s->table,huff);

	if ( is_huff6 )
	{
		SIMPLEPROFILE_SCOPE_N(huff6_x86sse2_loop,(s->decodeend[1] - s->decodeptr[0]));
		if (!oodle_newLZ_huff6_x86_sse2_kern(s))
			return false;
	}
	else
	{
		SIMPLEPROFILE_SCOPE_N(huff_x86sse2_loop,(s->decodeend[0] - s->decodeptr[0]));
		if (!oodle_newLZ_huffx86_sse2_kern(s))
			return false;
	}

	return newlz_huff_precise_finish(s, phufftab, is_huff6);
}

#endif

#if defined NEWLZ_X64GENERIC_HUFF_ASM

extern "C" bool oodle_newLZ_huff_x64_generic_kern(KrakenHuffState *s);
extern "C" bool oodle_newLZ_huff_x64_bmi2_kern(KrakenHuffState *s);
extern "C" bool oodle_newLZ_huff_x64_zen2_kern(KrakenHuffState *s);
extern "C" bool oodle_newLZ_huff6_x64_generic_kern(KrakenHuffState *s);
extern "C" bool oodle_newLZ_huff6_x64_bmi2_kern(KrakenHuffState *s);
extern "C" bool oodle_newLZ_huff6_x64_zen2_kern(KrakenHuffState *s);

static bool newlz_huff_x64generic(KrakenHuffState * s, const KrakenMSBHuffTab * huff, bool is_huff6)
{
	const KrakenHuffTab * phufftab = newLZ_prep_hufftab(&s->table,huff);

	if ( is_huff6 )
	{
		if (rrCPUx86_feature_present(RRX86_CPU_AMD_ZEN))
		{
			SIMPLEPROFILE_SCOPE_N(huff6_x64zen2_loop,(s->decodeend[1] - s->decodeptr[0]));
			if (!oodle_newLZ_huff6_x64_zen2_kern(s))
				return false;
		}
		else if (rrCPUx86_feature_present(RRX86_CPU_BMI2))
		{
			SIMPLEPROFILE_SCOPE_N(huff6_x64bmi2_loop,(s->decodeend[1] - s->decodeptr[0]));
			if (!oodle_newLZ_huff6_x64_bmi2_kern(s))
				return false;
		}
		else
		{
			SIMPLEPROFILE_SCOPE_N(huff6_x64generic_loop,(s->decodeend[1] - s->decodeptr[0]));
			if (!oodle_newLZ_huff6_x64_generic_kern(s))
				return false;
		}
	}
	else
	{
		if (rrCPUx86_feature_present(RRX86_CPU_AMD_ZEN))
		{
			SIMPLEPROFILE_SCOPE_N(huff_x64zen2_loop,(s->decodeend[0] - s->decodeptr[0]));
			if (!oodle_newLZ_huff_x64_zen2_kern(s))
				return false;
		}
		else if (rrCPUx86_feature_present(RRX86_CPU_BMI2))
		{
			SIMPLEPROFILE_SCOPE_N(huff_x64bmi2_loop,(s->decodeend[0] - s->decodeptr[0]));
			if (!oodle_newLZ_huff_x64_bmi2_kern(s))
				return false;
		}
		else
		{
			SIMPLEPROFILE_SCOPE_N(huff_x64generic_loop,(s->decodeend[0] - s->decodeptr[0]));
			if (!oodle_newLZ_huff_x64_generic_kern(s))
				return false;
		}
	}

	return newlz_huff_precise_finish(s, phufftab, is_huff6);
}

#endif

#if defined NEWLZ_JAGUAR_HUFF_ASM

extern "C" bool oodle_newLZ_huffx64_jaguar_kern(KrakenHuffState *s);

static bool newlz_huff_jaguar(KrakenHuffState * s, const KrakenMSBHuffTab * huff, bool is_huff6)
{
	const KrakenHuffTab * phufftab = newLZ_prep_hufftab(&s->table,huff);

	for (;;)
	{
		{
			SIMPLEPROFILE_SCOPE_N(huff_x64jaguar_loop,(s->decodeend[0] - s->decodeptr[0]));
			if (!oodle_newLZ_huffx64_jaguar_kern(s))
				return false;
		}

		if (!newlz_huff_precise_finish(s, phufftab, false))
			return false;

		// huff3 -> we're done; else prep for second pass.
		if (!is_huff6)
			break;

		newlz_huff_switch_to_second_half(s);
		is_huff6 = false;
	}

	return true;
}

#endif

#if defined NEWLZ_ZEN2_HUFF_ASM

extern "C" bool oodle_newLZ_huff_x64_zen2_kern(KrakenHuffState *s);
extern "C" bool oodle_newLZ_huff6_x64_zen2_kern(KrakenHuffState *s);

static bool newlz_huff_zen2(KrakenHuffState * s, const KrakenMSBHuffTab * huff, bool is_huff6)
{
	const KrakenHuffTab * phufftab = newLZ_prep_hufftab(&s->table,huff);

	if ( is_huff6 )
	{
		SIMPLEPROFILE_SCOPE_N(huff6_x64zen2_loop,(s->decodeend[1] - s->decodeptr[0]));
		if (!oodle_newLZ_huff6_x64_zen2_kern(s))
			return false;
	}
	else
	{
		SIMPLEPROFILE_SCOPE_N(huff_x64zen2_loop,(s->decodeend[0] - s->decodeptr[0]));
		if (!oodle_newLZ_huff_x64_zen2_kern(s))
			return false;
	}

	return newlz_huff_precise_finish(s, phufftab, is_huff6);
}

#endif

// this is the same as rrHuffman_BuildFastDecodeTable
// but needs nextSameLen instead of using the results of rrHuffman_BuildDecodeTable (radix sort)
static bool newlz_build_msbfirst_table(const U32 * firstSymOfLen, const U32 * lastSymOfLen, KrakenMSBHuffTab * msbHuff, U8 * const symLists )
{
	SIMPLEPROFILE_SCOPE(huff_build_table);

	// We expect degenerate cases to be handled outside.
	U8 * RADRESTRICT lens = msbHuff->len;
	U8 * RADRESTRICT syms = msbHuff->sym;

	// Build fast-decode table
	U32 curCode = 0;
	for (U32 codeLen = 1; codeLen < NEWLZ_HUFF_CODELEN_LIMIT; ++codeLen)
	{
		U32 num = lastSymOfLen[codeLen] - firstSymOfLen[codeLen];
		if (!num)
			continue;

		// Check that Kraft inequality isn't violated
		U32 numEntriesShift = NEWLZ_HUFF_CODELEN_LIMIT - codeLen;
		U32 numEntriesPerSym = 1 << numEntriesShift;
		U32 numTableEntriesThisLen = num << numEntriesShift;
		if (curCode + numTableEntriesThisLen > NEWLZ_HUFF_DECODE_TABLE_SIZE)
			return false;

		SloppyMemset_U8(lens + curCode, (U8)codeLen, numTableEntriesThisLen);

		const U8 * symList = symLists + firstSymOfLen[codeLen];
		for (U32 i = 0; i < num; i++)
		{
			SloppyMemset_U8(syms + curCode, symList[i], numEntriesPerSym);
			curCode += numEntriesPerSym;
		}
	}

	// last len
	if (U32 num = lastSymOfLen[NEWLZ_HUFF_CODELEN_LIMIT] - firstSymOfLen[NEWLZ_HUFF_CODELEN_LIMIT])
	{
		// Check that Kraft inequality isn't violated
		if (curCode + num > NEWLZ_HUFF_DECODE_TABLE_SIZE)
			return false;

		SloppyMemset_U8(lens + curCode, NEWLZ_HUFF_CODELEN_LIMIT, num);
		memcpy(syms + curCode, symLists + firstSymOfLen[NEWLZ_HUFF_CODELEN_LIMIT], num);
		curCode += num;
	}

	// For the code table to be valid, we need to have used the entire code space.
	return curCode == NEWLZ_HUFF_DECODE_TABLE_SIZE;
}

static RADFORCEINLINE U32 newlz_decode_packhuff8_runlen(rrVarBits_FuncArgs(vb))
{
	// PackHuff8 run lengths are Exp-Golomb codes with k=1.
	// For our 256-symbol alphabet, the largest valid run length we can ever have
	// is 256, which is sent using a 16-bit codeword. This means we can have at
	// most 7 leading zeros for a valid code.
	rrVarBits_Temps();

	if (rrVarBits_Peek(vb,8) == 0) // >=8 leading zeros -> illegal code
		return 511; // just return one larger than largest possible "real" code.

	U32 clz = rrVarBits_CountLeadingZeros(vb); // <=7 (we just checked)
	U32 nbits = clz*2 + 1 + 1; // 2clz + 1 bits for Exp-Golomb prefix, + 1 extra bit
	RR_ASSERT(nbits <= 16);
	return (U32)rrVarBits_Get_V(vb, nbits) - 1; // smallest legal code is "10" and corresponds to value 1
}

// Must match format of rrHuffman_PackCodeLens
// returns number of symbols in alphabet
static int newlz_decode_hufflens(rrVarBits * vb, U8 * symLists, U32 * lastSymOfLen)
{
	SIMPLEPROFILE_SCOPE(huff_get_code_lens);

	rrVarBits_Temps();
	rrVarBits_Locals(vbl);
	rrVarBits_Copy(vbl,vb->m);
	int gotNumSyms = -1;

	if (rrVarBits_Get1(vbl))
	{
		newlz_array_get_printf("[large huff]");
	
		// Unpack8
		const U32 numSymbols = 256;
		const int riceBits = (int) rrVarBits_Get_C(vbl,2);

		// Legal codelens are in [1,NEWLZ_HUFF_CODELEN_LIMIT] = [1,11].
		// Therefore, legal codelen deltas are in [-10,10].
		// After rrFoldNegatives, this means the coded values are in [0,20], which
		// we send using Rice codes with k given by riceBits above. We get the
		// longest max codelens for k=0 since that's just an unary code.
		const U32 maxCodelenDeltaFolded = (NEWLZ_HUFF_CODELEN_LIMIT - 1) * 2;

		// minLegalRice is the smallest value where the unary prefix (i.e. the
		// number of leading zeros) isn't above the stated max.
		//
		// Any codeword numerically below that has more leading zeros and is
		// thus out of range and illegal.
		const U32 maxUnaryPrefixVal = maxCodelenDeltaFolded >> riceBits;
		const RR_VARBITSTYPE minLegalRice = (RR_VARBITSTYPE)1 << (RR_VARBITSTYPELEN - 1 - maxUnaryPrefixVal);

		RR_ASSERT(maxUnaryPrefixVal + 1 + riceBits <= RR_MINBITSAVAILABLE);

		// Used in Rice decoding
		const U32 riceLenBias = riceBits + 1;

		int prevCodeLen4 = PRED_INIT(8);
		U32 i = 0;

		rrVarBits_Refill_Safe(vbl);
		gotNumSyms = 0;

		// First-is-on flag. If not set, start with a zero run.
		if (!rrVarBits_Get1(vbl))
			i = newlz_decode_packhuff8_runlen(rrVarBits_PassArgs(vbl));

		while (i < numSymbols)
		{
			rrVarBits_Refill_Safe(vbl);

			// read RunLen of non-zeros :
			U32 nzRunLen = newlz_decode_packhuff8_runlen(rrVarBits_PassArgs(vbl));
			if ( i + nzRunLen > numSymbols ) // can't overflow; i < numSymbols (=256) and nzRunLen < 512.
				NEWLZ_ARRAY_RETURN_FAILURE();

			rrVarBits_Refill_Safe(vbl);

			gotNumSyms += nzRunLen;
			do
			{
				// This part corresponds to rrVarBits_ReadRice_Small, but is specialized to our
				// application.
				if (rrVarBits_Bits(vbl) < minLegalRice)
					NEWLZ_ARRAY_RETURN_FAILURE();

				U32 clz = rrVarBits_CountLeadingZeros(vbl); // now guaranteed to be in range
				RR_ASSERT(clz <= maxUnaryPrefixVal);
				U32 nbits = clz + riceLenBias;
				U32 riceTail = (U32) rrVarBits_Get_V(vbl,nbits);
				U32 code = ((clz-1) << riceBits) + riceTail;

				// Unfold the negative values to get our signed deltas
				S32 delta = rrUnfoldNegatives(code);

				int pred = PRED_GET(prevCodeLen4);
				S32 curCodeLen = delta + pred;
				if ( curCodeLen < 1 || curCodeLen > NEWLZ_HUFF_CODELEN_LIMIT )
					NEWLZ_ARRAY_RETURN_FAILURE();

				prevCodeLen4 = PRED_UPDATE(prevCodeLen4,curCodeLen);

				U32 count = lastSymOfLen[curCodeLen];
				rrVarBits_Refill_Safe(vbl);

				symLists[count] = static_cast<U8>(i);
				lastSymOfLen[curCodeLen] = count + 1;

				i++;
			} while (--nzRunLen);

			if_unlikely ( i >= numSymbols )
				break;

			// read next zero runlen
			U32 zRunLen = newlz_decode_packhuff8_runlen(rrVarBits_PassArgs(vbl));
			i += zRunLen; // can't overflow: i < numSymbols (=256) and zRunLen < 512.
		}

		// require that all symbols have been specified
		// and don't allow single-char alphabets on this path
		if (i != numSymbols || gotNumSyms < 2)
			gotNumSyms = -1;
	}
	else
	{		
		// Unpack4
		gotNumSyms = (int) rrVarBits_Get_C(vbl,8);
		if (gotNumSyms == 0) // not actually allowed in Kraken
			NEWLZ_ARRAY_RETURN_FAILURE();
		else if (gotNumSyms == 1)
		{
			newlz_array_get_printf("[memset huff]");
		
			U8 sym = (U8) rrVarBits_Get_C(vbl,8);
			symLists[0] = sym;
		}
		else
		{
			newlz_array_get_printf("[small huff]");
		
			int log2CodeLen = (int) rrVarBits_Get_C(vbl,3);
			if ( log2CodeLen > 4 ) // NEWLZ_HUFF_CODELEN_LIMIT comfortably fits in 4 bits
				NEWLZ_ARRAY_RETURN_FAILURE();

			for (int i = 0; i < gotNumSyms; i++)
			{
				rrVarBits_Refill_Safe(vbl);

				int sym = (int) rrVarBits_Get_V(vbl,8);
				int len = (int) rrVarBits_Get_0Ok(vbl,log2CodeLen);
				int curCodeLen = len+1;
				if ( curCodeLen > NEWLZ_HUFF_CODELEN_LIMIT )
					NEWLZ_ARRAY_RETURN_FAILURE();

				U32 count = lastSymOfLen[curCodeLen];
				symLists[count] = static_cast<U8>(sym);
				lastSymOfLen[curCodeLen] = count + 1;
			}
		}
	}

	rrVarBits_Copy(vb->m,vbl);
	return gotNumSyms;
}

// New format written above
static int newlz_decode_hufflens2(rrVarBits * vb, U8 * symLists, U32 * lastSymOfLen)
{
	SIMPLEPROFILE_SCOPE(huff_get_code_lens2);

	rrVarBits_Temps();
	rrVarBits_Locals(vbl);
	rrVarBits_Copy(vbl,vb->m);

	// Read number of Rice extra bits
	const int riceBits = (int) rrVarBits_Get_C(vbl,2); // 2b read
	U32 gotNumSyms = (int)rrVarBits_Get_C(vbl, 8) + 1; // 10b read
	if (gotNumSyms < 2) // don't allow single-symbol alphabets on this path
		NEWLZ_ARRAY_RETURN_FAILURE();

	U32 numEG = newLZ_decode_alphabet_shape_num_EG(gotNumSyms, rrVarBits_PassArgs(vbl)); // <=18b read

	// Read the unary codes all at once, as raw bytes
	BlockBitReader bbr;
	rrVarBits_To_BlockBitReader(vbl, &bbr);

	U32 numUnary = gotNumSyms + numEG;
	U8 unary[256/*numSymbols*/ + 255/*max numEG*/ + 16/*slop to simplify edge cases*/];
	if (newLZ_decode_unary_block(unary, numUnary, &bbr) != (SINTa)numUnary)
		NEWLZ_ARRAY_RETURN_FAILURE();

	// Initialize our padding slots past the end to 0 (avoid reading uninitialized mem)
	RR_PUT64_NATIVE_UNALIGNED(unary + numUnary + 0, 0);
	RR_PUT64_NATIVE_UNALIGNED(unary + numUnary + 8, 0);

	// Rice code has uniform bits following the unary code
	if (newLZ_decode_rice_U8_bottom_block(unary, gotNumSyms, riceBits, &bbr) != (SINTa)gotNumSyms)
		NEWLZ_ARRAY_RETURN_FAILURE();

	rrVarBits_From_BlockBitReader(vbl, &bbr);
	rrVarBits_Copy(vb->m,vbl);

	// Apply codelen prediction
	int prevCodeLen4 = 8*4 + 2; // +2 is rounding bias; we can add it here once
#if defined(__RADSSE2__)
	{
		// The SSE2 loop wants to work 16 items at a time. Instead of bothering with a tail loop, we just
		// clear the next array elements after the end to 0 and let the predictor run.

		// Save the original contents of "unary" after gotNumSyms.
		// We have sufficient padding (the "slop" above) that this never reads out of bounds.
		__m128i orig_after_array = _mm_loadu_si128((const __m128i *)&unary[gotNumSyms]);

		// Set everything past the regular end of the array to 0.
		// This makes the predictor produce the same codelen as the last real codelen; i.e. it never
		// introduces out-of-range code lengths where there weren't any in the input.
		_mm_storeu_si128((__m128i *)&unary[gotNumSyms], _mm_setzero_si128());

		// Prediction loop works 16 at a time
		// actually predict codelen-1 (which makes everything 0-based so range checking is easier)
		__m128i prev_codelen4 = _mm_set1_epi8((char)(prevCodeLen4 - 4));
		__m128i max_valid_codelen_minus_one = _mm_set1_epi8(NEWLZ_HUFF_CODELEN_LIMIT - 1);
		__m128i max_len_minus_one = max_valid_codelen_minus_one;

		for (U32 i = 0; i < gotNumSyms; i += 16)
		{
			__m128i vals = _mm_loadu_si128((const __m128i *)&unary[i]);
			// unfold negatives
			__m128i vals_shift1 = _mm_and_si128(_mm_srli_epi16(vals, 1), _mm_set1_epi8(0x7f)); // vals >> 1
			__m128i vals_sign = _mm_sub_epi8(_mm_setzero_si128(), _mm_and_si128(vals, _mm_set1_epi8(1)));
			__m128i delta = _mm_xor_si128(vals_shift1, vals_sign);
			// inclusive prefix sum of deltas
			__m128i pfx = simd_prefix_sum_u8(delta);
			// broadcast sum from end of previous iter
			prev_codelen4 = _mm_unpackhi_epi8(prev_codelen4, prev_codelen4);
			prev_codelen4 = _mm_unpackhi_epi8(prev_codelen4, prev_codelen4);
			prev_codelen4 = _mm_shuffle_epi32(prev_codelen4, 0xff);
			// form cur_codelen
			__m128i cur_codelen = _mm_add_epi8(_mm_slli_si128(pfx, 1), prev_codelen4);
			// update running total for next iter (only care about last slot)
			prev_codelen4 = _mm_add_epi8(prev_codelen4, pfx);
			// final codelen calc
			cur_codelen = _mm_and_si128(_mm_srli_epi16(cur_codelen, 2), _mm_set1_epi8(0x3f));
			cur_codelen = _mm_add_epi8(cur_codelen, delta);
			// keep track of max codelen
			max_len_minus_one = _mm_max_epu8(max_len_minus_one, cur_codelen);
			// codelens we've formed so far are 0-based, add 1.
			cur_codelen = _mm_add_epi8(cur_codelen, _mm_set1_epi8(1));
			// store codelen
			_mm_storeu_si128((__m128i *)&unary[i], cur_codelen);
		}

		// If there were any out-of-range lens, the stream is bad.
		if (_mm_movemask_epi8(_mm_cmpeq_epi8(max_len_minus_one, max_valid_codelen_minus_one)) != 0xffff)
			NEWLZ_ARRAY_RETURN_FAILURE();

		// Restore saved code lens
		_mm_storeu_si128((__m128i *)&unary[gotNumSyms], orig_after_array);
	}
#elif defined(__RADNEON__)
	{
		// This works the same way as the SSE2 code above, including saving the original values
		// before and restoring them after.
		uint8x16_t orig_after_array = vld1q_u8(&unary[gotNumSyms]);

		// Set everything past the regular end of the array to 0.
		vst1q_u8(&unary[gotNumSyms], vdupq_n_u8(0));

		// Prediction loop works 16 at a time, and predicts codelen-1 (to make range checking easier)
		uint8x16_t prev_codelen4 = vdupq_n_u8((U8) (prevCodeLen4 - 4));
		uint8x16_t max_valid_codelen_minus_one = vdupq_n_u8(NEWLZ_HUFF_CODELEN_LIMIT - 1);
		uint8x16_t max_len_minus_one = max_valid_codelen_minus_one;

		for (U32 i = 0; i < gotNumSyms; i += 16)
		{
			uint8x16_t vals = vld1q_u8(&unary[i]);
			// unfold negatives
			uint8x16_t vals_shift1 = vshrq_n_u8(vals, 1);
			uint8x16_t vals_sign = vsubq_u8(vdupq_n_u8(0), vandq_u8(vals, vdupq_n_u8(1)));
			uint8x16_t delta = veorq_u8(vals_shift1, vals_sign);
			// inclusive prefix sum of deltas
			uint8x16_t pfx = simd_prefix_sum_u8(delta);
			// broadcast sum from end of previous iter
			uint8x16_t prev_codelen4_last = vdupq_lane_u8(vget_high_u8(prev_codelen4), 7);
			// add to prefix sum of deltas, updating running total for next iter
			prev_codelen4 = vaddq_u8(prev_codelen4_last, pfx);
			// form cur_codelen (which is lagging behind by 1 element)
			uint8x16_t cur_codelen = vextq_u8(prev_codelen4_last, prev_codelen4, 15);
			// final codelen calc
			cur_codelen = vsraq_n_u8(delta, cur_codelen, 2);
			// keep track of max codelen
			max_len_minus_one = vmaxq_u8(max_len_minus_one, cur_codelen);
			// codelens we've formed so far are 0-based, add 1.
			cur_codelen = vaddq_u8(cur_codelen, vdupq_n_u8(1));
			// store codelen
			vst1q_u8(&unary[i], cur_codelen);
		}

		// If there were any out-of-range lens, the stream is bad.
		if (!check_all_set_u8_neon(vceqq_u8(max_len_minus_one, max_valid_codelen_minus_one)))
			NEWLZ_ARRAY_RETURN_FAILURE();

		// Restore saved code lens
		vst1q_u8(&unary[gotNumSyms], orig_after_array);
	}
#else
	for (U32 i = 0; i < gotNumSyms; i++)
	{
		S32 delta = rrUnfoldNegatives(unary[i]);
		U32 curCodeLen = (prevCodeLen4 >> 2) + delta;
		if ( curCodeLen == 0 || curCodeLen > NEWLZ_HUFF_CODELEN_LIMIT )
			NEWLZ_ARRAY_RETURN_FAILURE();

		prevCodeLen4 += delta;
		unary[i] = (U8)curCodeLen;
	}
#endif

	// Decode run lens
	U16 runLens[128*2 + 8 + 1];
	int numRunPairs = newLZ_decode_alphabet_shape_runlens(runLens, gotNumSyms, numEG, unary + gotNumSyms, vb);
	if (numRunPairs < 0)
		NEWLZ_ARRAY_RETURN_FAILURE();

	// Process runs
	const U8 *len = unary;
	for (SINTa pair = 0; pair < numRunPairs; pair++)
	{
		U32 curSymbol = runLens[pair*2 + 0];
		U32 runLen = runLens[pair*2 + 1];

		do
		{
			U32 curCodeLen = *(len++);

			// should have been fuzz checked in unary decode :
			RR_ASSERT( curCodeLen <= NEWLZ_HUFF_CODELEN_LIMIT );

			U32 count = lastSymOfLen[curCodeLen];

			symLists[count] = static_cast<U8>(curSymbol);
			lastSymOfLen[curCodeLen] = count + 1;
			curSymbol++;
		}
		while (--runLen);
	}

	return (int)gotNumSyms;
}


// ---- driver func

SINTa newlz_get_array_huff(const U8 * const comp, SINTa comp_len, U8 * const to, SINTa to_len, bool is_huff6)
{
	SIMPLEPROFILE_SCOPE_N(get_array_huff,to_len);
	
	// KrakenMSBHuffTab on the stack :
	//	~ 4K
	// @@ could put this in Scratch instead
	const U8 * comp_end = comp + comp_len;
	RAD_ALIGN(KrakenMSBHuffTab, msbHuff, 16);
	
	rrVarBits vb;
	rrVarBits_GetOpen(vb.m,comp,comp_end);
	rrVarBits_Temps();
	
	{
		U8 symListsBuf[256/*lens 1-7*/ + 256*(NEWLZ_HUFF_CODELEN_LIMIT-7) /* lens 8+*/];
		U32 firstSymOfLen[NEWLZ_HUFF_CODELEN_LIMIT + 1];
		U32 lastSymOfLen[NEWLZ_HUFF_CODELEN_LIMIT + 1];

		// illegal codelens still need valid pointers, but it's OK for them to clash
		// with storage for valid codelens (because if they are used, we will reject
		// the array anyway)
		firstSymOfLen[0] = lastSymOfLen[0] = 0;

		// code lens 1..7 have at most 1<<len symbols (if code stream is valid)
		U32 cur = 0;
		for (U32 i = 1; i <= 7; i++)
		{
			firstSymOfLen[i] = lastSymOfLen[i] = cur;
			cur += 1u << i;
		}

		// remaining lens have at most 256 symbols (if code stream is valid)
		for (U32 i = 8; i <= NEWLZ_HUFF_CODELEN_LIMIT; i++)
		{
			firstSymOfLen[i] = lastSymOfLen[i] = cur;
			cur += 256;
		}
	
		int gotNumSymbols;
		
		RR_VARBITSTYPE huff_type_flag1 = rrVarBits_Get1(vb.m);
		if ( huff_type_flag1 )
		{
			RR_VARBITSTYPE huff_type_flag2 = rrVarBits_Get1(vb.m);
			huff_type_flag2;
		
			//NEWLZ_ARRAY_RETURN_FAILURE();
			//gotNumSymbols = newlz_decode_hufflens_tans(&vb,numCodesOfLen,symListsByLen);
			// hufflens2 is on 10
			if ( huff_type_flag2 == 0 )
			{
				gotNumSymbols = newlz_decode_hufflens2(&vb,symListsBuf,lastSymOfLen);
			}
			else
			{
				rrPrintf_v2("corruption : huff_type_flag\n");
				NEWLZ_ARRAY_RETURN_FAILURE();
			}
		}
		else
		{
			gotNumSymbols = newlz_decode_hufflens(&vb,symListsBuf,lastSymOfLen);
		}
		
		#ifdef SPEEDFITTING
		g_speedfitter_huff_num_non_zero = gotNumSymbols;
		#endif
		
		if (gotNumSymbols < 1) // bad stream
			NEWLZ_ARRAY_RETURN_FAILURE();
		else if ( gotNumSymbols == 1 )
		{
			// degenerate case, get done
			//rrprintf("huff degenerate : %d\n",to_len);
			memset(to,symListsBuf[0],to_len);
			
			SINTa header_size = rrVarBits_GetSizeBytes(vb.m,comp);
			return header_size;
		}
		
		// this can return failure if the code doesn't obey Kraft
		// inequality (indicates data corruption)
		if ( !newlz_build_msbfirst_table(firstSymOfLen, lastSymOfLen, &msbHuff, symListsBuf) )
			NEWLZ_ARRAY_RETURN_FAILURE();
	}
	
	SINTa header_size = rrVarBits_GetSizeBytes(vb.m,comp);
		
	const U8 * comp_start = comp+header_size;
	KrakenHuffState s;

#ifdef DO_NEWLZ_HUFF_DUMP
	rrprintf("huff6=%d count=%d\n", is_huff6, (int)to_len);
	bool dumped = false;

	if (to_len >= 16384)
	{
		dumped = true;

		FILE *f = fopen("dump.dat", "wb");
		U32 counts[3];

		counts[0] = rrPtrDiff32( comp_end - comp_start );
		counts[1] = (U32)to_len;
		counts[2] = is_huff6;
		fwrite(counts, 1, sizeof(counts), f);

		const KrakenHuffTab * phufftab = newLZ_prep_hufftab(&s.table,&msbHuff);
		fwrite(phufftab, 1, sizeof(*phufftab), f);
		fwrite(comp_start, 1, counts[0], f);

		fclose(f);
	}
#endif

	if ( !is_huff6 )
	{
		if ( comp_end - comp_start < 3 )
			NEWLZ_ARRAY_RETURN_FAILURE();
		
		// get complen1 :
		int comp_len1 = RR_GET16_LE_UNALIGNED(comp_start);
		comp_start += 2;
		
		//rrprintf("comp_len1 : %d\n",comp_len1);
		
		// other 2 streams have at least 1 byte each
		//	so minimum comp len is comp_len1+2
		REQUIRE_FUZZ_RETURN( (comp_end-comp_start) >= comp_len1 + 2 , -1 );

		// set up the core decode loop
		s.decodeptr[0] = to;
		s.decodeend[0] = to + to_len;

		s.strm0_end[0] = comp_start + comp_len1;

		// Layout: strm0-> | strm2-> | <-strm1
		s.bitp[0] = comp_start;	  	s.bits[0] = s.bitc[0] = 0;
		s.bitp[1] = comp_end;	  	s.bits[1] = s.bitc[1] = 0;
		s.bitp[2] = s.strm0_end[0]; s.bits[2] = s.bitc[2] = 0;
	}
	else
	{
		if ( comp_end - comp_start < 6 )
			NEWLZ_ARRAY_RETURN_FAILURE();

		SINTa first_half_to_len = (to_len + 1)>>1;

		// get second half pos
		U32 first_half_len = RR_GET24_LE_OVERRUNOK(comp_start);
		comp_start += 3;

		// make sure it's valid
		if ( comp_end - comp_start < (SINTa)first_half_len )
			NEWLZ_ARRAY_RETURN_FAILURE();

		const U8 * first_half_end = comp_start + first_half_len;

		// get str0len for first half
		U32 comp_str0len = RR_GET16_LE_UNALIGNED(comp_start);
		comp_start += 2;

		// other 2 streams in first half have at least 1 byte each
		if ( first_half_end - comp_start < (SINTa)comp_str0len + 2 )
			NEWLZ_ARRAY_RETURN_FAILURE();

		// get complen1 for second half
		const U8 * comp_mid = first_half_end;
		if ( comp_end - comp_mid < 3 )
			NEWLZ_ARRAY_RETURN_FAILURE();

		// get str3len
		U32 comp_str3len = RR_GET16_LE_UNALIGNED(comp_mid);
		comp_mid += 2;

		// again, other 2 streams in first half have at least 1 byte each
		if ( comp_end - comp_mid < (SINTa)comp_str3len + 2 )
			NEWLZ_ARRAY_RETURN_FAILURE();

		// layout:
		//   strm0-> | strm2-> | <-strm1
		//   strm3-> | strm5-> | <-strm4
		s.decodeptr[0] = to;
		s.decodeend[0] = to + first_half_to_len;
		s.decodeptr[1] = to + first_half_to_len;
		s.decodeend[1] = to + to_len;
		s.strm0_end[0] = comp_start + comp_str0len;
		s.strm0_end[1] = comp_mid + comp_str3len;

		s.bitp[0] = comp_start;	  	s.bits[0] = s.bitc[0] = 0;
		s.bitp[1] = first_half_end; s.bits[1] = s.bitc[1] = 0;
		s.bitp[2] = s.strm0_end[0]; s.bits[2] = s.bitc[2] = 0;
		s.bitp[3] = comp_mid;		s.bits[3] = s.bitc[3] = 0;
		s.bitp[4] = comp_end;       s.bits[4] = s.bitc[4] = 0;
		s.bitp[5] = s.strm0_end[1]; s.bits[5] = s.bitc[5] = 0;
	}

	// convention: ordered from more specific to more general
	SINTa ret;

#if defined NEWLZ_JAGUAR_HUFF_ASM

	ret = newlz_huff_jaguar(&s, &msbHuff, is_huff6) ? comp_len : -1;

#elif defined NEWLZ_ZEN2_HUFF_ASM

	ret = newlz_huff_zen2(&s, &msbHuff, is_huff6) ? comp_len : -1;

#elif defined NEWLZ_X64GENERIC_HUFF_ASM

	ret = newlz_huff_x64generic(&s, &msbHuff, is_huff6) ? comp_len : -1;

#elif defined NEWLZ_X86SSE2_HUFF_ASM

	ret = newlz_huff_x86sse2(&s, &msbHuff, is_huff6) ? comp_len : -1;

#elif defined NEWLZ_ARM64

	ret = newlz_huff64_arm(&s, &msbHuff, is_huff6) ? comp_len : -1;

#elif defined NEWLZ_ARM32

	ret = newlz_huff32_arm(&s, &msbHuff, is_huff6) ? comp_len : -1;

#elif defined __RAD64REGS__

	ret = newlz_huff64(&s, &msbHuff, is_huff6) ? comp_len : -1;

#else

	ret = newlz_huff32(&s, &msbHuff, is_huff6) ? comp_len : -1;

#endif

#ifdef DO_NEWLZ_HUFF_DUMP
	if (dumped)
	{
		FILE *f = fopen("ref_results.dat", "wb");
		fwrite(to, to_len, 1, f);
		fclose(f);
	}
#endif

	return ret;
}

OODLE_NS_END
