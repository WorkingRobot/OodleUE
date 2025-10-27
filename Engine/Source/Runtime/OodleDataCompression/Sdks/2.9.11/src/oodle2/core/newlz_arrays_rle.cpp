// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "newlz_arrays_rle.h"
#include "rrbits.h"
#include "rrmem.h"
#include "rrlog.h"

#include "rrarenaallocator.h"
#include "histogram.h"
#include "newlz_speedfit.h"
#include "newlz_arrays.h"
#include "newlz_arrays.inl"
#include "newlz_arrays_huff.h"
#include "newlz_simd.h"

#include "rrsimpleprofstub.h"
//#include "rrsimpleprof.h"

OODLE_NS_START

// HACK_DECODE_ENTROPY_BUF_SIZE <- this acts as a scratch mem use limit, checked by the encoder
//static const SINTa HACK_DECODE_ENTROPY_BUF_SIZE = 24*1024;
// limited now by OODLELZ_NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH

// There are two streams in a forward/backward pair:
//
//	 literals-> | <-packets
//
// The literals are just that, the packets tell the decoder what to do.
// Runs are of a given run value that's kept as state. (There's a command
// to change what value runs have.)
//
// Basic packet is a single byte:
//	 packet[7:4] = run_len (rl)
//	 packet[3:0] = 15 - literal_run_len (lrl)
//
//	 both of these are actual run lens, nothing subtracted
//
// To get longer literals, we can use rl=0 lrl=15 packets (val=0x00)
//	 (or use escapes)
// To get longer runs, we can use rl=15 lrl=0 packets (val=0xff)
//	 (or use escapes)
//
// Now forbid packets with rl<3, we get escape codes for
//	 0x01 <= val < 0x30
//
// Finally, carve out escape codes:
//	 val=1: change run value (val in literal stream)
//	   new_run_val = *lits++
//	 0x02 <= val <= 0x08: long LRL (+1 extra byte)
//	   lrl = ((val-2)*256 + *--packets + 1) << 6
//	 0x09 <= val <= 0x0f: long RL (+1 extra byte)
//	   rl = ((val-9)*256 + *--packets + 1) << 7
//	 0x10 <= val <= 0x2f: (+1 extra byte) 6b LRL + 7b RL
//	   code = (val-16)*256 + *--packets
//	   lrl = code & 63
//	   rl = code >> 6

// Subtract, saturating at 0
static UINTa subtract_sat(UINTa a, UINTa b)
{
	return (a < b) ? 0 : a - b;
}

// ---- Memory access primitives (encoder/decoder) and fast run searching (encoder)

#if defined(__RADSSE2__)

OODLE_NS_END
#include <emmintrin.h>
OODLE_NS_START

typedef __m128i Vec128;

static Vec128 splatz_128()					{ return _mm_setzero_si128(); }
static Vec128 splatb_128(const U8 *p)		{ return _mm_set1_epi8(*p); }
static Vec128 read128(const U8 *p)			{ return _mm_loadu_si128((const __m128i *)p); }
static void write128(U8 *dst, Vec128 v)		{ _mm_storeu_si128((__m128i *)dst, v); }
static void write128a(U8 *dst, Vec128 v)	{ _mm_store_si128((__m128i *)dst, v); }

#define HAVE_SIMD_RLE7_SEARCHES

// skip ahead to next run of 3+ identical bytes
static RADFORCEINLINE const U8 *rle7_skip_to_next_run3(const U8 *in_ptr, const U8 *in_safe_end)
{
	RR_ASSERT(in_ptr < in_safe_end);
	do
	{
		Vec128 bytes0 = read128(in_ptr);
		Vec128 bytes1 = read128(in_ptr + 1);
		Vec128 bytes2 = read128(in_ptr + 2);
		Vec128 bytes_run3 = _mm_and_si128(_mm_cmpeq_epi8(bytes1, bytes0), _mm_cmpeq_epi8(bytes2, bytes0));
		int mask = _mm_movemask_epi8(bytes_run3);
		if (mask != 0)
			return in_ptr + rrCtz32(mask);

		in_ptr += 16;
	} while (in_ptr < in_safe_end);

	return in_ptr;
}

// determine end of run starting at run_ptr - 1, but stop when we get past in_safe_end
// it's OK for this to underestimate (we follow up with a scalar loop)
static RADFORCEINLINE const U8 *rle7_find_run_end(const U8 *run_ptr, const U8 *in_safe_end, const U8 *in_end)
{
	// Run = sequence of bytes where bytes[i] == bytes[i+1].
	// We can check for that!
	
	// insured by caller :
	RR_ASSERT(run_ptr < in_safe_end);
	
	// kRunSafetyMargin is 18
	// in_safe_end is kRunSafetyMargin back from in_end
	RR_ASSERT( (in_end - in_safe_end) >= 18 );

	U64 first = RR_GET64_NATIVE_UNALIGNED(run_ptr);
	U64 diff = RR_GET64_NATIVE_UNALIGNED(run_ptr + 1) ^ first;
	if (diff)
		return run_ptr + 1 + rrCtzBytes64(diff);
	else
	{
		// If run is >= 9 bytes, spin up the SSE2 loop
		// Why can we keep "bytes_first" fixed?
		// In first iter, we're comparing each byte against its immediate predecessor; if we find a mismatch,
		// we stop. Thus, if we get to a second iter, we've proven that "first" is a run of identical bytes.

		Vec128 bytes_first;
		// CB fix : to read 16 bytes at +8 we need 24 to end, which is not guaranteed
		if ( (in_end-run_ptr) >= 24 )
		{
			bytes_first = read128(run_ptr + 8);
			run_ptr += 9;
		}
		else
		{
			bytes_first = read128(run_ptr);
			run_ptr ++;
		}

		for (;;)
		{
			// If we're near the end, be careful
			if (run_ptr >= in_safe_end)
			{
				U8 v = (U8)first; // & 0xff;
				while (run_ptr < in_end && *run_ptr == v)
					++run_ptr;
				return run_ptr;
			}

			Vec128 bytes = read128(run_ptr);
			Vec128 bytes_eq = _mm_cmpeq_epi8(bytes, bytes_first);
			int mask = _mm_movemask_epi8(bytes_eq);
			if (mask != 0xffff)
				return run_ptr + rrCtz32(~mask);
			run_ptr += 16;
		}
	}
}

#elif defined(__RADNEON__)

OODLE_NS_END
#include <arm_neon.h>
OODLE_NS_START

typedef uint8x16_t Vec128;

static Vec128 splatz_128()					{ return vdupq_n_u8(0); }
static Vec128 splatb_128(const U8 *p)		{ return vld1q_dup_u8(p); }
static Vec128 read128(const U8 *p)			{ return vld1q_u8(p); }
static void write128(U8 *dst, Vec128 v)		{ vst1q_u8(dst, v); }
static void write128a(U8 *dst, Vec128 v)	{ vst1q_u8(dst, v); }

// NEON doesn't have a PMOVMSKB equivalent, so we don't define any custom RLE searches
// and instead use the scalar fallback code.

#else // scalar fallback

struct Vec128 { U64 a, b; };

static Vec128 splatz_128()					{ Vec128 v = { 0, 0 }; return v; }
static Vec128 splatb_128(const U8 *p)		{ Vec128 v; v.a = v.b = *p * 0x0101010101010101ull; return v; }
static Vec128 read128(const U8 *p)			{ Vec128 v; v.a = RR_GET64_NATIVE_UNALIGNED(p + 0); v.b = RR_GET64_NATIVE_UNALIGNED(p + 8); return v; }
static void write128(U8 *dst, Vec128 v)		{ RR_PUT64_NATIVE_UNALIGNED(dst + 0, v.a); RR_PUT64_NATIVE_UNALIGNED(dst + 8, v.b); }
static void write128a(U8 *dst, Vec128 v)	{ RR_PUT64_NATIVE(dst + 0, v.a); RR_PUT64_NATIVE(dst + 8, v.b); }

#endif

#ifndef HAVE_SIMD_RLE7_SEARCHES

// Find next run of 3+ identical bytes (but stop at in_safe_end)
static RADFORCEINLINE const U8 *rle7_skip_to_next_run3(const U8 *in_ptr, const U8 *in_safe_end)
{
	//while (in_ptr < in_safe_end && (in_ptr[0] != in_ptr[1] || in_ptr[0] != in_ptr[2]))
	//	++in_ptr;

	RR_ASSERT(in_ptr < in_safe_end);
	do
	{
		U64 bytes0 = RR_GET64_NATIVE_UNALIGNED(in_ptr + 0);
		U64 bytes1 = RR_GET64_NATIVE_UNALIGNED(in_ptr + 1);
		U64 bytes2 = RR_GET64_NATIVE_UNALIGNED(in_ptr + 2);
		U64 run3 = (bytes0 ^ bytes1) | (bytes0 ^ bytes2); // ==0x00 for a byte if and only if the next 2 bytes match

		// The trick we use to locate the first zero byte effectively relies on little-endian
		// byte order, but everything we did so far is byte-order invariant. So we only need to
		// do a single byte swap.
#if defined(__RADBIGENDIAN__)
		run3 = RR_BSWAP64(run3);
#elif !defined(__RADLITTLEENDIAN__)
#error Endian?
#endif

		// Now find the first zero byte (if any) in it.
		static const U64 splat8 = 0x0101010101010101ull;

		// (run3-splat8) turns the least significant zero byte (if any) into
		// 0xff (rest ends up mangled).
		// Then AND with 0x80 in every byte -> resulting byte is 0x80 iff
		// either:
		// a) byte was ==0x00 in run3 or
		// b) byte was >=0x81 in run3
		// Finally AND with ~run3 to rule out case b).
		//
		// The resulting value is 0x80 in the least significant byte that was
		// equal to 0x00 in run3; we can then use "count trailing zeros" to
		// figure out what byte posiiton that was.
		U64 haszero = (run3 - splat8) & (splat8 * 0x80) & ~run3;
		if (haszero != 0)
			return in_ptr + rrCtzBytes64(haszero);

		in_ptr += 8;
	}
	while (in_ptr < in_safe_end);

	return in_ptr;
}

static RADFORCEINLINE const U8 *rle7_find_run_end(const U8 *run_ptr, const U8 *in_safe_end, const U8 *in_end)
{
	RR_ASSERT(run_ptr < in_safe_end);

	// Run = sequence of bytes where bytes[i] == bytes[i+1].
	// We can check for that!

	U64 first = RR_GET64_NATIVE_UNALIGNED(run_ptr);
	++run_ptr; // every "run" is at least 1 byte long

	for (;;)
	{
		// At this point, run_ptr < in_safe_end + 1 (it was <in_safe_end at entry of func, and then we incremented;
		// alternatively, it was <in_safe_end when we decided to loop again).
		//
		// in_safe_end guarantees we can read at least 18 bytes; we consume 1 byte for the increment above
		// then read 8.
		U64 cur = RR_GET64_NATIVE_UNALIGNED(run_ptr);
		U64 diff = cur ^ first;

		// Why can we keep "first" fixed?
		// In first iter, we're comparing each byte against its immediate predecessor; if we find a mismatch,
		// we stop. Thus, if we get to a second iter, we've proven that "first" is a run of identical bytes.

		if (diff)
		{
#if defined(__RADLITTLEENDIAN__)
			run_ptr += rrCtzBytes64(diff);
#elif defined(__RADBIGENDIAN__)
			run_ptr += rrClzBytes64(diff);
#else
#error Endian?
#endif
			break;
		}

		run_ptr += 8;

		// If we're near the end, be careful
		if (run_ptr >= in_safe_end)
		{
			U8 v = first & 0xff;
			while (run_ptr < in_end && *run_ptr == v)
				++run_ptr;
			break;
		}
	}

	return run_ptr;
}

#endif

// Emits one or two simple packets. You need to check whether that fits before you call!
static RADFORCEINLINE U8 *rle7_emit_short_packets(U8 *pktp, U8 *litp, UINTa lrl, UINTa rl)
{
	RR_ASSERT(pktp - litp >= 2); // this function will emit at most 2 bytes.
	RR_ASSERT(rl >= 3);
	(void)litp; // shut up warnings

	if (lrl <= 15)
	{
		if (rl <= 15)
			*--pktp = static_cast<U8>((rl << 4) | (15 - lrl));
		else
		{
			RR_ASSERT(rl <= 30);

			// Neither packet may have a run length below 3, so just split the run in half.
			UINTa lo_rl = rl >> 1;
			UINTa hi_rl = rl - lo_rl;

			*--pktp = static_cast<U8>((lo_rl << 4) | (15 - lrl));
			*--pktp = static_cast<U8>((hi_rl << 4) | 15);
		}
	}
	else
	{
		RR_ASSERT(lrl <= 30 && rl <= 15);
		*--pktp = 0x00; // rl=0, lrl=15
		*--pktp = static_cast<U8>((rl << 4) | (15 - (lrl - 15)));
	}

	return pktp;
}

// Returns NULL if we run out of output space.
static RADFORCEINLINE U8 *rle7_emit_long_packet(U8 *pktp, U8 *litp, UINTa lrl, UINTa rl)
{
	// Required safety margin for the common cases, long LRL/RL can check every iter (they're rare)
	static const SINTa kMiddleMaxBytes = 2; // max bytes for middle part (medium packet)
	RR_ASSERT(pktp - litp >= 1 + kMiddleMaxBytes); // simple packet in front (next if)

	// If we're just slightly above 63, it's better to use a simple packet to get us below than to send a long LRL packet
	if (lrl >= 64 && lrl < 79)
	{
		// NOTE this packet priced into safety margin above
		*--pktp = 0x00; // rl=0, lrl=15
		lrl -= 15;
	}

	// long LRL
	while (lrl >= 64)
	{
		static const UINTa kMaxValLRL = 7*256;
		U32 lrl_this_run = static_cast<U32>(RR_MIN(lrl >> 6, kMaxValLRL));
		RR_ASSERT(lrl_this_run >= 1);

		// long LRL not priced into safety margin above, need to check whether this packet puts us below
		// the required number of bytes for the middle packet.
		if (pktp - litp < kMiddleMaxBytes + 2)
			return NULL;

		U32 v = lrl_this_run - 1;
		*--pktp = static_cast<U8>(2 + (v >> 8));
		*--pktp = (U8)v; // & 0xff;

		lrl -= lrl_this_run << 6;
	}

	// Middle part
	RR_ASSERT(pktp - litp >= kMiddleMaxBytes);
	UINTa rl_rem = rl & 0x7f;
	if (rl_rem < 3 || lrl > 30 || (rl_rem > 15 && lrl > 15) || rl_rem > 30) // NOTE(fg): more than two simple packets for the remainder
	//if (rl_rem < 3 || lrl > 15 || rl_rem > 15) // NOTE(fg): more than one simple packet for the remainder
	{
		// Don't emit middle packet if LRL and RL are both 0!
		// This is an optimization but it's important when a stream ends with a multiple of 64 literals;
		// the extra lrl=0 rl=0 medium-length packet occurs after all bytes have been decoded, which is
		// not allowed.
		if ((lrl | rl_rem) != 0)
		{
			// medium packet
			U32 v = static_cast<U32>(lrl | (rl_rem << 6));
			*--pktp = static_cast<U8>((v >> 8) + 0x10);
			*--pktp = (U8)v; // & 0xff;
		}
	}
	else
		pktp = rle7_emit_short_packets(pktp, litp, lrl, rl_rem);

	// Long RL
	while (rl >= 128)
	{
		static const UINTa kMaxValRL = 7*256;
		U32 rl_this_run = static_cast<U32>(RR_MIN(rl >> 7, kMaxValRL));
		RR_ASSERT(rl_this_run >= 1);

		// Long RL not considered in safety margin, need to check on every packet (but that's fine)
		if (pktp - litp < 2)
			return NULL;

		U32 v = rl_this_run - 1;
		*--pktp = static_cast<U8>(9 + (v >> 8));
		*--pktp = (U8)v; // & 0xff;

		rl -= rl_this_run << 7;
	}

	return pktp;
}

// Returns value >in_size if encode fails (not enough output space)
SINTa newLZ_put_array_rle(U8 * const out_buf, U8 * const out_end, const U8 *in_buf, SINTa in_len_signed, 
	F32 lambda, const OodleSpeedFit * speedfit, F32 *pJ, rrArenaAllocator * arena, U32 entropy_flags, int compression_level)
{
	SIMPLEPROFILE_SCOPE_N(put_array_rle,in_len_signed);
	
	// Min run len for us to consider sending a new run value
	static const UINTa RLE7_MRL_NEWV = 8;

	// Run length detection safety margin
	static const UINTa kRunSafetyMargin = 16 + 2; // 16-byte SSE2 loads; we load ptr, ptr+1, ptr+2 to identify 3-byte runs.

	// Paranoia.
	if (in_len_signed <= 0 || (out_end - out_buf) < 5)
		return in_len_signed + 1;

	//SIMPLEPROFILE_SCOPE_N(rle_encode, in_len_signed);

	UINTa in_size = in_len_signed; // we just checked it was positive.
	U8 *litp = out_buf;
	U8 *pktp = out_end;

	const U8 *in_ptr = in_buf;
	const U8 *lit_start = in_buf;
	const U8 *in_safe_end = in_buf + subtract_sat(in_size, kRunSafetyMargin); // for SSE2 len-3-run detection
	const U8 *in_end = in_buf + in_size;
	U8 last_v = 0;

	*litp++ = 0; // "uncompressed" header flag (note we checked earlier that out_end - out_buf >= 5, so we're good here)
	U8 *first_lit = litp; // first real literal

	while (in_ptr < in_safe_end)
	{
		// Skip over run-less regions quickly
		in_ptr = rle7_skip_to_next_run3(in_ptr, in_safe_end);

		// Don't even bother trying to find runs starting within the last few bytes of
		// the input buffer; the benefit is small and not having to deal with it makes
		// writing a fast encoder simpler.
		if (in_ptr >= in_safe_end)
			break;

		// Determine run length starting at current byte.
		// This returns the end of the run.
		const U8 *run_end = rle7_find_run_end(in_ptr, in_safe_end, in_end);

		// If long enough run, emit it!
		U8 v = *in_ptr;
		UINTa lrl = in_ptr - lit_start;
		UINTa run_len = run_end - in_ptr;
		in_ptr = run_end; // advance ptr whether we send the run or not

		// We started by skipping ahead to the next len-3 run, so this run better be
		// at least 3 bytes.
		RR_ASSERT(run_len >= 3);

		// We check for a little more output space than required. This allows us to do the
		// literal copying sloppily, but it also accounts for the number of packet bytes we
		// need to send later (see checks in emit_short_packets and emit_long_packet).
		static const UINTa kSafetyMargin = 2 + 16; // 2 bytes for "change run value" packet, up to 16 slop for copy. (Which also guarantees min packet space.)
		UINTa bytes_left = pktp - litp;
		if (bytes_left < lrl + kSafetyMargin)
			return in_len_signed + 1;

		// If we change run value, make sure our run is long enough, and send the
		// "change run value" packet.
		if (v != last_v)
		{
			if (run_len < RLE7_MRL_NEWV)
				continue;
			*--pktp = 1; // "new run symbol" code
			*litp++ = v;
			last_v = v;
		}

		// Copy literals over
		// NOTE kSafetyMargin above guarantees we can do this sloppily, as long
		// as we're far away enough from the end of the input, which our in_ptr < in_safe_end
		// checks above guarantee.
		//
		// Slop here is 16 not 15 because we can have lrl=0 (after changing run value)
		// but we always copy at least 16 bytes.
		RR_ASSERT(in_end - (lit_start + lrl) >= 16);

		// memcpy(litp, lit_start, lrl); litp += lrl; // but sloppy
		U8 *litp_end = litp + lrl;
		do
		{
			write128(litp, read128(lit_start));
			litp += 16;
			lit_start += 16;
		}
		while (litp < litp_end);
		litp = litp_end;
		lit_start = in_ptr;

		if (lrl > 30 || (run_len > 15 && lrl > 15) || run_len > 30) // NOTE(fg): more than two simple packets
		//if (run_len > 15 || lrl > 15) // NOTE(fg): more than one simple packet
		{
			pktp = rle7_emit_long_packet(pktp, litp, lrl, run_len);
			if (!pktp) // ran out of output space
				return in_len_signed + 1;
		}
		else
			pktp = rle7_emit_short_packets(pktp, litp, lrl, run_len);
	}

	if (UINTa lrl = in_end - lit_start)
	{
		// Use 16 bytes safety margin here, just for consistency with the main loop above.
		// This part always uses memcpy, so it doesn't need slop on the copy; but we still
		// need to reserve a few bytes of packet space (4 required at the time of writing)
		// and it seems easiest just to keep the slop amounts the same between the two loops.
		static const UINTa kSafetyMargin = 16;
		UINTa bytes_left = pktp - litp;
		if (bytes_left < lrl + kSafetyMargin)
			return in_len_signed + 1;

		// Copy literals over
		memcpy(litp, lit_start, lrl);
		litp += lrl;

		// Final packet has run len=0 so must use long packet form
		pktp = rle7_emit_long_packet(pktp, litp, lrl, 0);
		if (!pktp) // ran out of output space
			return in_len_signed + 1;
	}

	RR_ASSERT(litp <= pktp);

	// Should we compress the literals?
	SINTa lit_bytes = litp - first_lit;
	SINTa pkt_bytes = out_end - pktp;
	F32 J_raw = (F32)(1 + lit_bytes); // pkt_bytes ignored here, since they don't get compressed
	F32 J_comp = LAGRANGE_COST_INVALID;
	U32 array_type = NEWLZ_ARRAY_TYPE_UNCOMPRESSED;
	U8 * comp_buf = NULL;
	SINTa comp_len = 0;
	SINTa final_size = 0;

	// Is it worth trying to use huff on the rest?
	if ( ( entropy_flags & NEWLZ_ARRAY_FLAG_ALLOW_RLEHUFF ) &&
		 lit_bytes >= NEWLZ_HUFF_ARRAY_MIN_SIZE &&
		 ( lit_bytes + pkt_bytes <= NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH ) )
	{
		comp_buf = (U8 *)arena->Alloc( lit_bytes );

		// reproduces part of newLZ_put_array()
		// we could just call newLZ_put_array here
		// the difference is that if we fall back to uncompressed here,
		//	we use a special case 1 byte header, not the normal uncompressed header
		// @@ -> could just call put_array with *pJ seeded to J_raw
		//    for the sake of code un-duping

		U32 histo[256];
		CountHistoArrayU8(first_lit,lit_bytes,histo,256);
	
		U32 highest_histo_count = simd_find_max_U32_256(histo);
		if ( highest_histo_count == (U32)lit_bytes )
		{
			// VOICE_MARTH_MAP_1.ckb
			// has an RLE-Huff where the lit_bytes are a memset (lit_bytes = 95, lit_bytes = 178)
			// could detect that and put it as a memset RLE

			F32 J_memset = 6 + lambda * speedfit->memcpy(lit_bytes);
			array_type = NEWLZ_ARRAY_TYPE_RLE;
			comp_len = 1;
			J_comp = J_memset;
			comp_buf[0] = first_lit[0];
		}
		else
		{
			F32 huff_J = J_raw;
			U32 huff_type = 0;
			SINTa huff_comp_len = newLZ_put_array_huff(comp_buf,comp_buf + lit_bytes,first_lit,lit_bytes,histo,lambda,speedfit,&huff_J,ARRAY_DEADLINE_HUGE,&huff_type,entropy_flags,arena,compression_level);
			
			if ( huff_comp_len >= 0 && huff_comp_len < lit_bytes )
			{
				RR_ASSERT( huff_J <= J_raw );
				array_type = huff_type;
				comp_len = huff_comp_len;
				J_comp = huff_J; // huff_J has the +5 for header
			}
			// else use raw

			// NOTE(fg): I tried TANS here, but it's *super*-marginal. (Even more than TANS already is.)
			// NOTE(cb): in the current scheme you cannot use TANS or RLE for this sub-array
			//		because it could potentially violate the decoder scratch limit
			//		(more careful counting of the total scratch used by nested calls would be needed)
		}
	}

	// raw data only needs a single header byte (0) we already wrote
	if ( J_raw <= J_comp )
	{
		// All we need to do is sandwich the two halves together
		memmove(litp, pktp, pkt_bytes);
		final_size = 1 + lit_bytes + pkt_bytes;
		*pJ = J_raw + pkt_bytes;
	}
	else
	{
		// compressed data needs a 5-byte header
		//	raw len, comp len, huff flag :
		RR_ASSERT( lit_bytes > 0 );
		RR_ASSERT( (lit_bytes-1) <= NEWLZ_ARRAY_SIZE_MASK );
		RR_ASSERT( comp_len >= 0 );
		RR_ASSERT( comp_len <= NEWLZ_ARRAY_SIZE_MASK );
		RR_ASSERT( comp_len + 5 <= lit_bytes + 1 );

		newLZ_put_array_comp_header(out_buf,array_type,lit_bytes,comp_len);
		
		/*
		U64 header = ((U64)(array_type)<<36) + ((U64)(lit_bytes-1)<<NEWLZ_ARRAY_SIZE_BITS) + (U64)(comp_len);
		out_buf[0] = (U8)(header>>32);
		RR_PUT32_BE_UNALIGNED(out_buf + 1,(U32)header);
		*/
		memcpy(out_buf + 5, comp_buf, comp_len);

		SINTa huff_comp_len_plus_header = comp_len + 5;
		huff_comp_len_plus_header = newLZ_reput_array_small(out_buf,huff_comp_len_plus_header,&J_comp);

		memmove(out_buf + huff_comp_len_plus_header, pktp, pkt_bytes);
		final_size = huff_comp_len_plus_header + pkt_bytes;
		*pJ = J_comp + pkt_bytes;
	}

	*pJ += 5 + lambda * speedfit->rle(in_len_signed );

	if ( comp_buf != NULL )
		arena->Free( comp_buf, lit_bytes );

	return final_size;
}

// ---- Decoder

static RADFORCEINLINE bool rle7_is_simple(U32 packet)
{
	//return packet < 1 || packet >= 0x30;
	return packet - 1u >= 0x2fu; // this uses unsigned wraparound
}

SINTa newLZ_get_array_rle(const U8 * const comp, SINTa comp_len, U8 * const to, SINTa to_len, U8 * scratch_ptr, U8 * scratch_end)
{
	// Initial paranoia.
	if (comp_len <= 1 || to_len <= 0)
	{
		// special case : RLE type arrays with comp_len 1 are a memset :
		if ( comp_len == 1 )
		{
			// memset array :
			U8 val = comp[0];
			memset(to,val,to_len);
			return comp_len;
		}
		else
		{
			NEWLZ_ARRAY_RETURN_FAILURE();
		}
	}

#if 0
	{
		bool is_huff = comp[0] != 0;
		
		static int s_num_rle_arrays = 0;
		static int s_num_rle_arrays_huff = 0;
		static SINTa s_rle_tot_raw_size = 0;
		static SINTa s_rle_tot_comp_size = 0;
		s_num_rle_arrays ++;
		if ( is_huff ) s_num_rle_arrays_huff ++;
		s_rle_tot_raw_size += to_len;
		s_rle_tot_comp_size += comp_len;
		
		rrprintf("RLE: %d (%d) : %d -> %d : %.1f -> %.1f\n",s_num_rle_arrays,s_num_rle_arrays_huff,
			(int)s_rle_tot_raw_size,(int)s_rle_tot_comp_size,
			(double)s_rle_tot_raw_size/s_num_rle_arrays,(double)s_rle_tot_comp_size/s_num_rle_arrays);
	}
#endif

	// Fuzz safety idea: at every checkpoint, make sure we have enough input and output space to handle
	// kSimplePacketsPerCheck-1 simple packets followed by one medium escape (which implies we're also good
	// for kSimplePacketsPerCheck simple packets).
	static const SINTa kSimplePacketsPerCheck = 4; // NOTE: this is the "unroll" amount in the fast loop

	static const SINTa kMaxReadSimple = 1 + 15; // max number of bytes read by simple packet (1B packet, 15B literals)
	static const SINTa kMaxReadMedium = 2 + 63; // max number of bytes read by a medium escape packet (2B packet, 63B literals)
	static const SINTa kMaxReadFastCheck = (kSimplePacketsPerCheck - 1)*kMaxReadSimple + kMaxReadMedium; // fast loop that only checks every N simple packets
	static const SINTa kMaxReadSlowCheck = kMaxReadMedium; // slow path that checks after every packet

	static const SINTa kMaxWriteSimple = 15 + 15; // max number of bytes written by a simple packet (LRL=15, RL=15)
	static const SINTa kMaxWriteMedium = 63 + 128 + 15; // max number of bytes written by a medium escape packet (LRL=63, RL=127) with 16 byte slop for aligned 128B write
	static const SINTa kMaxWriteFastCheck = (kSimplePacketsPerCheck - 1)*kMaxWriteSimple + kMaxWriteMedium;
	static const SINTa kMaxWriteSlowCheck = kMaxWriteMedium;

	const U8 *lits = comp + 1;
	const U8 *pkts = comp + comp_len;

	if (comp[0] != 0)
	{
		SIMPLEPROFILE_SCOPE(rle_decode_array);
		U8 * temp_to = scratch_ptr;
		SINTa temp_to_len = 0;
		SINTa from_len = newLZ_get_array(&temp_to, comp, comp + comp_len, &temp_to_len, (scratch_end - scratch_ptr), true, scratch_ptr, scratch_end);
		if ( from_len <= 0 )
			NEWLZ_ARRAY_RETURN_FAILURE();

		RR_ASSERT( temp_to == scratch_ptr );
		SINTa rest_size = comp_len - from_len;
		
		// this is the condition guaranteed by the encoder :
		RR_ASSERT_IF_NOT_CORRUPT( (temp_to_len + rest_size) <= NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH );
		// the encoder should guarantee this for valid bitstreams,
		// but it's not necessarily true for corrupt ones.
		RR_ASSERT_IF_NOT_CORRUPT( rrPtrDiff(scratch_end - scratch_ptr) >= NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH );

		// could check NEWLZ_ARRAY_INTERNAL_MAX_SCRATCH here
		//	instead just check what we actually need, which is that scratch has enough room
		if ( temp_to_len + rest_size > rrPtrDiff(scratch_end - scratch_ptr) )
			NEWLZ_ARRAY_RETURN_FAILURE();

		memcpy(temp_to + temp_to_len, comp + from_len, rest_size);
		lits = temp_to;
		pkts = temp_to + temp_to_len + rest_size;
		
		scratch_ptr += temp_to_len + rest_size;
	}

	SIMPLEPROFILE_SCOPE_N(rle_decode, to_len);

	U8 *outp = to;
	U8 *out_check = outp + subtract_sat(to_len, kMaxWriteFastCheck-1);
	U8 *out_end = outp + to_len;
	Vec128 runv = splatz_128();

	#define RLE7_SIMPLE_PACKET(packet) \
		{ \
			UINTa lrl = (15 - packet) & 0xf; /* ==15 - (packet & 0xf) */ \
			UINTa rl = packet >> 4; \
			/* <16 literals */ \
			write128(outp, read128(lits)); \
			outp += lrl; \
			lits += lrl; \
			/* <16 run bytes */ \
			write128(outp, runv); \
			outp += rl; \
		}

	// Fast loop while we have sufficient input and output space left.
	// Loop structure is:
	// - Checkpoint at the top of the loop (Make sure we're good for up to 3 simple packets + 1 escape, or 4 simple packets)
	// - Each loop iteration handles up to 4 packets; if an escape occurs, we loop around (and hence checkpoint again)
	//
	// NOTE: out_check must use < here so that when the subtract_sat for output size clamped to 0, we don't attempt
	// to use the fast loop (we can't, since there's less than kMaxWriteFastCheck bytes available!).
	while (pkts - lits >= kMaxReadFastCheck && outp < out_check)
	{
		// NOTE couldn't find a win from doing 4-at-a-time check; not enough win from a super-fast
		// 4-simple-packet pathway (escapes aren't particularly rare)
		U8 packet = pkts[-1];
		if (rle7_is_simple(packet))
		{
			RLE7_SIMPLE_PACKET(packet);
			packet = pkts[-2];
			if (rle7_is_simple(packet))
			{
				RLE7_SIMPLE_PACKET(packet);
				packet = pkts[-3];
				if (rle7_is_simple(packet))
				{
					RLE7_SIMPLE_PACKET(packet);
					packet = pkts[-4];
					if (rle7_is_simple(packet))
					{
						RLE7_SIMPLE_PACKET(packet);
						pkts -= 4;
						continue;
					}
					else
						pkts -= 3;
				}
				else
					pkts -= 2;
			}
			else
				pkts -= 1;
		}

		#include "newlz_rle_escape_packet.inl"
	}

	// Tail input and output buffers
	RAD_ALIGN(U8, tail_in[kMaxReadSlowCheck * 2], 16);
	RAD_ALIGN(U8, tail_out[kMaxWriteSlowCheck * 2], 16);
	SINTa final_in_bytes = 0;
	SINTa final_out_bytes = 0;

	// Init tail input buffer just to make sure we always read defined data,
	// even when input is corrupted.
	// Not strictly necessary (this code is safe with any input) - this is to
	// not trip up ASan/UBSan/Valgrind etc.
	Vec128 vzero = splatz_128();
	for (UINTa i = 0; i < (sizeof(tail_in) / 16); ++i)
		write128a(tail_in + i*16, vzero);
	write128(tail_in + sizeof(tail_in) - 16, vzero);

	out_check = to + subtract_sat(to_len, kMaxWriteSlowCheck);

	// Careful loop that needs to check input/output pointers after every packet
	for (;;)
	{
		// Checkpoint input
		// Establishes invariant: pkts - lits >= kMaxReadSlowCheck
		SINTa in_left = pkts - lits;
		if (in_left < kMaxReadSlowCheck)
		{
			// If we already switched to tail input buffer earlier, that means we over-read:
			// we had <kMaxReadSlowCheck bytes left initially, and the refill adds
			// >=kMaxReadSlowCheck extra bytes of padding. If after that, pkts - lits gets
			// <kMaxReadSlowCheck again, that means we definitely over-read.
			if (final_in_bytes != 0)
				NEWLZ_ARRAY_RETURN_FAILURE();

			memcpy(tail_in, lits, in_left);
			memcpy(tail_in + sizeof(tail_in) - in_left, lits, in_left);
			lits = tail_in;
			pkts = tail_in + sizeof(tail_in);

			// Final distance between the ptrs once the extra in_left bytes have been consumed
			final_in_bytes = sizeof(tail_in) - in_left;
			RR_ASSERT(final_in_bytes >= kMaxReadSlowCheck);
		}
		RR_ASSERT(pkts - lits >= kMaxReadSlowCheck);

		// Checkpoint output
		// Establishes invariant: safe to write kMaxWriteSlowCheck bytes starting at "outp"
		if (outp >= out_check)
		{
			// If we hit the end exactly, we're good!
			if (outp == out_end)
				break;

			// If we already switched to tail output buffer earlier, that means we wrote past the end.
			// Same argument as with the input buffer above.
			if (final_out_bytes != 0)
				NEWLZ_ARRAY_RETURN_FAILURE();

			// First time we got close; switch to scratch buffer and log how many bytes are left to decode.
			// We decode to scratch mem so we can be sloppy, and then do a precise copy at the end.
			final_out_bytes = out_end - outp;
			RR_ASSERT(final_out_bytes <= kMaxWriteSlowCheck);

			// New destination is the scratch buffer, with both the checkpoint and end pointers set to
			// correspond to the end of the real output buffer.
			outp = tail_out;
			out_check = out_end = outp + final_out_bytes;
		}

		// Decode one packet
		//
		// NOTE: it's possible to unroll the simple-packet case up to 4x here, too, because the worst-case
		// bounds for 4 simple packets fit inside one medium packet. (The difference to the previous loop being
		// that in this case, we need to have a checkpoint right in front of every escape packet, so instead of
		// falling through into the escape packet, we use an if-else construction.)
		//
		// However, to pull this off means that we need to do one of three things:
		// 1. Check whether we've reached the end of the output buffer after every single packet
		//	  (somewhat defeating the purpose of unrolling, since that's one of the two checks we want to elide),
		// 2. Just let the simple packet decoder run, but in that case we might consume up to 3 dummy packets
		//	  past the end of the real stream. This is still fuzz safe, but it means we have to weaken our
		//	  "did we consume the right number of input bytes and write the right number of output bytes" check
		//	  below, which is ugly.
		// 3. Require that every valid stream end with an escape packet. Again, we're always fuzz-safe no matter
		//	  what, but the simple-packet case (without checks) might simpy not notice that we just decoded the
		//	  last packet and keep going. If we require every legal stream to end in an escape code, then by
		//	  definition the only streams where this happens are illegal anyway and are OK to be rejected by
		//	  the final check.
		//
		// I implemented variant 3; it was almost exactly the same speed as just making this tail loop process
		// packets one at a time, and it has a more complicated tail loop and adds constraints to the encoder,
		// so screw it. Not worth it.
		U8 packet = pkts[-1];
		if (rle7_is_simple(packet))
		{
			RLE7_SIMPLE_PACKET(packet);
			--pkts;
		}
		else
		{
			#include "newlz_rle_escape_packet.inl"
		}
	}

	// Check that we consumed all input bytes and generated the right output byte count.
	if (outp != out_end || (pkts - lits) != final_in_bytes)
		NEWLZ_ARRAY_RETURN_FAILURE();

	// Copy the last few bytes from the scratch buffer to the real destination
	if (final_out_bytes != 0)
		memcpy(to + to_len - final_out_bytes, tail_out, final_out_bytes);

	return comp_len;

	#undef RLE7_SIMPLE_PACKET
}

OODLE_NS_END
