// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodletexrtpub.h"
#include "bc7prep_format.h"
#include "vec128.inl"
#include "rrsimd.h"

OODLE_NS_START

// ---- Decoder internal API

OodleTexRT_Err bc7prep_read_header(const OodleTexRT_BC7PrepHeader * header, SINTa * out_num_blockks, SINTa * out_payload_size);

// Computes the required work memory size for the BC7Prep
// decoder. nblocks can be determined from a BC7PrepHeader
// using bc7prep_count_blocks.
SINTa bc7prep_min_decode_work_mem_size(SINTa nblocks);

// Decode flags for bc7prep_decode
enum
{
	BC7PREP_DECODE_DESTINATION_IS_CACHED	= 1<<0,		// set if target is known to be in cached memory
	BC7PREP_DECODE_AVOID_WIDE_VECTORS		= 1<<1,		// set to not use wide (>=256b) vectors even when available
};

// Decodes the given block of bc7prep data.
// Returns a negative value on error.
SINTa bc7prep_decode(
	U8 * output_buf, SINTa output_buf_size,
	const U8 * bc7prep_data, SINTa bc7prep_data_size,
	const OodleTexRT_BC7PrepHeader * header,
	U8 * work_mem, SINTa work_mem_size,
	U32 flags
);

// ---- Internal decoder stuff

// To target write-cobmined memory, we first write output data
// to (cached) work mem one chunk at a time, then memcpy from
// there to the destination. This turns out to also not be a
// bad idea for L2 hit rates even when write-back cached.
static const SINTa BC7PREP_CHUNK_BLOCK_COUNT = 2048; // 32k bytes

namespace bc7prep {

extern const SINTa decode_split_pos[BC7PREP_MODE_COUNT];
extern const SINTa mode_sizes[BC7PREP_MODE_COUNT];

template<bool t_split_on>
struct SplitAt8
{
	enum
	{
		is_split = 0,
		first_advance = 16,
		second_advance = 16,
		second_offs_const = 8,
		second_offs_linear = 0,
	};
};

template<>
struct SplitAt8<true>
{
	enum
	{
		is_split = 1,
		first_advance = 8,
		second_advance = 8,
		second_offs_const = 0,
		second_offs_linear = 8,
	};
};

template<bool t_split_on>
struct SplitAt12
{
	enum
	{
		is_split = 0,
		first_advance = 16,
		second_advance = 16,
		second_offs_const = 12,
		second_offs_linear = 0,
	};
};

template<>
struct SplitAt12<true>
{
	enum
	{
		is_split = 1,
		first_advance = 12,
		second_advance = 4,
		second_offs_const = 0,
		second_offs_linear = 12,
	};
};

template<bool t_split_on>
struct SplitAt6
{
	enum
	{
		is_split = 0,
		first_advance = 16,
		second_advance = 16,
		second_offs_const = 6,
		second_offs_linear = 0,
	};
};

template<>
struct SplitAt6<true>
{
	enum
	{
		is_split = 1,
		first_advance = 6,
		second_advance = 10,
		second_offs_const = 0,
		second_offs_linear = 6,
	};
};

template<typename Tsplit>
static const U8 *second_half(const U8 *coded_block, UINTa nblocks)
{
	return coded_block + Tsplit::second_offs_const + nblocks*Tsplit::second_offs_linear;
}

static inline SINTa target_offs(U32 index_and_mode)
{
	// NOTE: this uses the fact that the shift is 32-bit to nuke the high bits implicitly
	return SINTa(index_and_mode << 4);
}

// Decoder kernels
typedef UINTa OptimizedDecoderKernel(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa blocks, const U16 * target);
typedef void OptimizedSortKernel(U16 * mode_cur[16], const U8 * mode_nibbles, SINTa blocks_in_chunk);

struct OptimizedDecoderKernelSet
{
	OptimizedDecoderKernel * kernel[BC7PREP_MODE_COUNT][4];
	OptimizedSortKernel * sort;
};

#define UNMUNGE_DISPATCH_VARIANTS(mode,split) \
	{ mode<split<false>, false>, mode<split<false>, true>, mode<split<true>, false>, mode<split<true>, true> }

// Used for modes that don't do splits or colorspace switches
#define UNMUNGE_SINGLE_KERNEL(name) \
	{ name, name, name, name }

// Used for kernels that don't have optimized specializations
#define UNMUNGE_NO_KERNEL UNMUNGE_SINGLE_KERNEL(0)

#define DECLARE_UNMUNGE(name, split) \
	template<typename Tsplit, bool t_switch_colorspace> UINTa name(U8 * RADRESTRICT out_block, const U8 * first_ptr, const U8 * second_ptr, UINTa nblocks, const U16 * target)

DECLARE_UNMUNGE(un_munge_bc7_mode0_sse2, SplitAt8);
UINTa un_munge_bc7_mode8_sse2(U8 * RADRESTRICT out_block, const U8 * coded_block, const U8 *, UINTa nblocks, const U16 * target);

extern OptimizedDecoderKernelSet opt_kernels_none;
extern OptimizedDecoderKernelSet opt_kernels_sse2;

#ifdef DO_BUILD_SSE4
DECLARE_UNMUNGE(un_munge_bc7_mode1_ssse3, SplitAt8);
DECLARE_UNMUNGE(un_munge_bc7_mode2_ssse3, SplitAt12);
DECLARE_UNMUNGE(un_munge_bc7_mode3_ssse3, SplitAt12);
DECLARE_UNMUNGE(un_munge_bc7_mode4_ssse3, SplitAt6);
DECLARE_UNMUNGE(un_munge_bc7_mode5_ssse3, SplitAt8);
DECLARE_UNMUNGE(un_munge_bc7_mode6_ssse3, SplitAt8);
DECLARE_UNMUNGE(un_munge_bc7_mode7_ssse3, SplitAt12);

extern OptimizedDecoderKernelSet opt_kernels_ssse3;

extern U16 sort_compact_table[256][8];
extern void sort_pass_ssse3(U16 * mode_cur[16], const U8 * mode_nibbles, SINTa blocks_in_chunk);
#endif

#ifdef DO_BUILD_AVX2
// 256b wide kernels
DECLARE_UNMUNGE(un_munge_bc7_mode0_avx2, SplitAt6);
DECLARE_UNMUNGE(un_munge_bc7_mode1_avx2, SplitAt6);
DECLARE_UNMUNGE(un_munge_bc7_mode2_avx2, SplitAt6);
DECLARE_UNMUNGE(un_munge_bc7_mode3_avx2, SplitAt6);
DECLARE_UNMUNGE(un_munge_bc7_mode4_avx2, SplitAt6);
DECLARE_UNMUNGE(un_munge_bc7_mode5_avx2, SplitAt6);
DECLARE_UNMUNGE(un_munge_bc7_mode6_avx2, SplitAt6);
DECLARE_UNMUNGE(un_munge_bc7_mode7_avx2, SplitAt6);

// 128b wide but using AVX2 insns
DECLARE_UNMUNGE(un_munge_bc7_mode4_avx2_narrow, SplitAt6);

extern OptimizedDecoderKernelSet opt_kernels_avx2;
extern OptimizedDecoderKernelSet opt_kernels_avx2_narrow;
#endif

} // namespace bc7prep

OODLE_NS_END

