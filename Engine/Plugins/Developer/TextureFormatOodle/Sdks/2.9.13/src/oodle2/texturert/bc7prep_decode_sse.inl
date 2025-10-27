// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "bc7prep_decode.h"
#include "vec128.inl"

#ifdef BC7PREP_AVX2
#include "vec256.inl"
#endif

OODLE_NS_START
namespace bc7prep {

template<typename Tsplit> void vec_load_single(Vec128 &first_half, Vec128 &second_half, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind);
template<typename Tsplit> void vec_load_pair(Vec128 &first_half, Vec128 &second_half, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind);
template<typename Tsplit> void vec_load_split6(Vec128 &first, Vec128 &second0, Vec128 &second1, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind);

template<>
inline void vec_load_pair<SplitAt8<false> >(Vec128 &first_half, Vec128 &second_half, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind)
{
	typedef SplitAt8<false> Tsplit;
	Vec128 a = load128u(first_ptr + block_ind*Tsplit::first_advance); // first block
	Vec128 b = load128u(first_ptr + block_ind*Tsplit::first_advance + 16); // second block

	first_half  = _mm_unpacklo_epi64(a, b);
	second_half = _mm_unpackhi_epi64(a, b);
}

template<>
inline void vec_load_pair<SplitAt8<true> >(Vec128 &first_half, Vec128 &second_half, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind)
{
	typedef SplitAt8<true> Tsplit;
	// Already split in halves!
	first_half  = load128u(first_ptr  + block_ind*Tsplit::first_advance);
	second_half = load128u(second_ptr + block_ind*Tsplit::second_advance);
}

// single SplitAt12 layout: used by modes 2+3
template<>
inline void vec_load_single<SplitAt12<false> >(Vec128 &first_half, Vec128 &second_half, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind)
{
	typedef SplitAt12<false> Tsplit;
	first_half = load128u(first_ptr + block_ind*Tsplit::first_advance);
	second_half = first_half;
}

template<>
inline void vec_load_single<SplitAt12<true> >(Vec128 &first_half, Vec128 &second_half, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind)
{
	typedef SplitAt12<true> Tsplit;

	// Appears to read past the end, but it's fine:
	// The idea is that for us to do any of these reads, we must have at least 16 bytes worth
	// of data for that particular mode.
	//
	// So any read in the first part has at least 4 bytes of second-part data after it,
	// and any read in the second part has at least 12 bytes of first-part data before it.
	first_half  = load128u(first_ptr + block_ind*Tsplit::first_advance);
	second_half = load128u(second_ptr + block_ind*Tsplit::second_advance - 12);
}

// pair SplitAt12 layout: used by mode 7
template<>
inline void vec_load_pair<SplitAt12<false> >(Vec128 &first_half, Vec128 &second_half, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind)
{
	typedef SplitAt12<false> Tsplit;
	Vec128 a = load128u(first_ptr + block_ind*Tsplit::first_advance); // first block
	Vec128 b = load128u(first_ptr + block_ind*Tsplit::first_advance + 16); // second block

	first_half = _mm_unpacklo_epi64(a, b);
	second_half = _mm_unpackhi_epi64(a, b);
}

template<>
inline void vec_load_pair<SplitAt12<true> >(Vec128 &first_half, Vec128 &second_half, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind)
{
	typedef SplitAt12<true> Tsplit;
	Vec128 lo_a = load128u(first_ptr + block_ind*Tsplit::first_advance); // 12B payload, 4B garbage
	Vec128 lo_b = load128u(first_ptr + block_ind*Tsplit::first_advance + Tsplit::first_advance); // 12B payload, 4B garbage
	Vec128 hi = load64u(second_ptr + block_ind*Tsplit::second_advance);

	Vec128 lohi = _mm_unpackhi_epi32(lo_a, lo_b); // (lo_a.u32[2], lo_b.u32[2], lo_a.u32[3], lo_b.u32[3])

	first_half = _mm_unpacklo_epi64(lo_a, lo_b);
	second_half = _mm_unpacklo_epi32(lohi, hi); // (lo_a.u32[2], hi.u32[0], lo_b.u32[2], hi.u32[1])
}

template<>
inline void vec_load_split6<SplitAt6<false> >(Vec128 &first, Vec128 &second0, Vec128 &second1, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind)
{
	typedef SplitAt6<false> Tsplit;
	Vec128 a = load128u(first_ptr + block_ind*Tsplit::first_advance); // first block
	Vec128 b = load128u(first_ptr + block_ind*Tsplit::first_advance + 16); // second block

	first   = _mm_unpacklo_epi64(a, b);
	second0 = first;
	second1 = _mm_unpackhi_epi64(a, b);
}

#ifdef BC7PREP_SSSE3

template<>
inline void vec_load_split6<SplitAt6<true> >(Vec128 &first, Vec128 &second0, Vec128 &second1, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind)
{
	typedef SplitAt6<true> Tsplit;

	// All these appear to read past the end, but it's fine:
	// The idea is that for us to do any of these reads, we must have at least 16 bytes worth
	// of data for that particular mode.
	//
	// So any read in the first part has at least 10 bytes of second-part data after it,
	// and any read in the second part has at least 6 bytes of first-part data before it.

	Vec128 lo   = load128u(first_ptr + block_ind*Tsplit::first_advance);
	Vec128 hi_a = load128u(second_ptr + block_ind*Tsplit::second_advance - 6);
	Vec128 hi_b = load128u(second_ptr + block_ind*Tsplit::second_advance + Tsplit::second_advance - 6);

	first = _mm_shuffle_epi8(lo, _mm_setr_epi8(0,1,2,3,4,5,-1,-1, 6,7,8,9,10,11,-1,-1));
	second0 = _mm_unpacklo_epi64(hi_a, hi_b);
	second1 = _mm_unpackhi_epi64(hi_a, hi_b);
}

#endif

#ifdef BC7PREP_AVX2

template<typename Tsplit> void vec_load_single(Vec256 &first_half, Vec256 &second_half, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind);
template<typename Tsplit> void vec_load_pair(Vec256 &first_half, Vec256 &second_half, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind);
template<typename Tsplit> void vec_load_split6(Vec256 &first, Vec256 &second0, Vec256 &second1, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind);

static RADFORCEINLINE Vec256 vec_load_2x128_strided(const U8 * ptr, SINTa stride)
{
	Vec128 lo = load128u(ptr);
	Vec128 hi = load128u(ptr + stride);
	return combine(lo, hi);
}

template<>
inline void vec_load_pair<SplitAt8<false> >(Vec256 &first_half, Vec256 &second_half, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind)
{
	typedef SplitAt8<false> Tsplit;
	Vec256 a = vec_load_2x128_strided(first_ptr + block_ind*Tsplit::first_advance, 32); // first block
	Vec256 b = vec_load_2x128_strided(first_ptr + block_ind*Tsplit::first_advance + 16, 32); // second block

	first_half  = _mm256_unpacklo_epi64(a, b);
	second_half = _mm256_unpackhi_epi64(a, b);
}

template<>
inline void vec_load_pair<SplitAt8<true> >(Vec256 &first_half, Vec256 &second_half, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind)
{
	typedef SplitAt8<true> Tsplit;
	// Already split in halves!
	first_half  = load256u(first_ptr  + block_ind*Tsplit::first_advance);
	second_half = load256u(second_ptr + block_ind*Tsplit::second_advance);
}

// single SplitAt12 layout: used by modes 2+3
template<>
inline void vec_load_single<SplitAt12<false> >(Vec256 &first_half, Vec256 &second_half, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind)
{
	typedef SplitAt12<false> Tsplit;
	first_half = load256u(first_ptr + block_ind*Tsplit::first_advance);
	second_half = first_half;
}

template<>
inline void vec_load_single<SplitAt12<true> >(Vec256 &first_half, Vec256 &second_half, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind)
{
	typedef SplitAt12<true> Tsplit;

	// Appears to read past the end, but it's fine:
	// The idea is that for us to do any of these reads, we must have at least 16 bytes worth
	// of data for that particular mode.
	//
	// So any read in the first part has at least 4 bytes of second-part data after it,
	// and any read in the second part has at least 12 bytes of first-part data before it.
	first_half  = vec_load_2x128_strided(first_ptr + block_ind*Tsplit::first_advance, Tsplit::first_advance);
	second_half = vec_load_2x128_strided(second_ptr + block_ind*Tsplit::second_advance - 12, Tsplit::second_advance);
}

// pair SplitAt12 layout: used by mode 7
template<>
inline void vec_load_pair<SplitAt12<false> >(Vec256 &first_half, Vec256 &second_half, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind)
{
	typedef SplitAt12<false> Tsplit;
	Vec256 a = vec_load_2x128_strided(first_ptr + block_ind*Tsplit::first_advance, 32); // first block
	Vec256 b = vec_load_2x128_strided(first_ptr + block_ind*Tsplit::first_advance + 16, 32); // second block

	first_half = _mm256_unpacklo_epi64(a, b);
	second_half = _mm256_unpackhi_epi64(a, b);
}

template<>
inline void vec_load_pair<SplitAt12<true> >(Vec256 &first_half, Vec256 &second_half, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind)
{
	typedef SplitAt12<true> Tsplit;
	Vec256 lo_a = vec_load_2x128_strided(first_ptr + block_ind*Tsplit::first_advance, 2*Tsplit::first_advance); // 12B payload, 4B garbage
	Vec256 lo_b = vec_load_2x128_strided(first_ptr + block_ind*Tsplit::first_advance + Tsplit::first_advance, 2*Tsplit::first_advance); // 12B payload, 4B garbage
	Vec128 hi128 = load128u(second_ptr + block_ind*Tsplit::second_advance);
	Vec256 hi = _mm256_permute4x64_epi64(_mm256_castsi128_si256(hi128), _MM_SHUFFLE(1,1,0,0)); // double both 64-bit words

	Vec256 lohi = _mm256_unpackhi_epi32(lo_a, lo_b); // (lo_a.u32[2], lo_b.u32[2], lo_a.u32[3], lo_b.u32[3])

	first_half = _mm256_unpacklo_epi64(lo_a, lo_b);
	second_half = _mm256_unpacklo_epi32(lohi, hi); // (lo_a.u32[2], hi.u32[0], lo_b.u32[2], hi.u32[1])
}

template<>
inline void vec_load_split6<SplitAt6<false> >(Vec256 &first, Vec256 &second0, Vec256 &second1, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind)
{
	typedef SplitAt6<false> Tsplit;

	Vec256 a = vec_load_2x128_strided(first_ptr + block_ind*Tsplit::first_advance, 32); // first block
	Vec256 b = vec_load_2x128_strided(first_ptr + block_ind*Tsplit::first_advance + 16, 32); // second block

	first   = _mm256_unpacklo_epi64(a, b);
	second0 = first;
	second1 = _mm256_unpackhi_epi64(a, b);
}

template<>
inline void vec_load_split6<SplitAt6<true> >(Vec256 &first, Vec256 &second0, Vec256 &second1, const U8 * first_ptr, const U8 * second_ptr, UINTa block_ind)
{
	typedef SplitAt6<true> Tsplit;

	// All these appear to read past the end, but it's fine:
	// The idea is that for us to do any of these reads, we must have at least 16 bytes worth
	// of data for that particular mode.
	//
	// So any read in the first part has at least 10 bytes of second-part data after it,
	// and any read in the second part has at least 6 bytes of first-part data before it.

	Vec256 lo   = vec_load_2x128_strided(first_ptr + block_ind*Tsplit::first_advance, 2*Tsplit::first_advance);
	Vec256 hi_a = vec_load_2x128_strided(second_ptr + block_ind*Tsplit::second_advance - 6, 2*Tsplit::second_advance);
	Vec256 hi_b = vec_load_2x128_strided(second_ptr + block_ind*Tsplit::second_advance + Tsplit::second_advance - 6, 2*Tsplit::second_advance);

	first = _mm256_shuffle_epi8(lo, broadcast128_256(_mm_setr_epi8(0,1,2,3,4,5,-1,-1, 6,7,8,9,10,11,-1,-1)));
	second0 = _mm256_unpacklo_epi64(hi_a, hi_b);
	second1 = _mm256_unpackhi_epi64(hi_a, hi_b);
}

#endif

} // namespace bc7prep
OODLE_NS_END

