// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "texbase.h"
#include "templates/rrvector.h"
#include "rrdxtcblock.h"
#include "perceptualactivity.h" // SingleFloatBlock4x4
#include "bc7bits.h"

RR_NAMESPACE_START

struct dword_and_count
{
	U32 dw;
	U32 count;
};

struct dword_and_count_compare_dword
{
	bool operator ()(const dword_and_count & lhs,const dword_and_count & rhs)
	{
		if ( lhs.dw == rhs.dw )
		{
			// if values the same, sort count to highest first
			return lhs.count > rhs.count;
		}
		return lhs.dw < rhs.dw;
	}
};

struct dword_and_count_compare_count_highest_first
{
	bool operator ()(const dword_and_count & lhs,const dword_and_count & rhs)
	{
		if ( lhs.count == rhs.count )
		{
			return lhs.dw < rhs.dw;
		}
		return lhs.count > rhs.count;
	}
};

struct dword_and_count_compare_count_lowest_first
{
	bool operator ()(const dword_and_count & lhs,const dword_and_count & rhs)
	{
		if ( lhs.count == rhs.count )
		{
			return lhs.dw < rhs.dw;
		}
		return lhs.count < rhs.count;
	}
};

// sort_and_count_uniques sorts "vec_of_dwords"
//  and fills "pcounting" with all unique dwords & their occurance count
// it does NOT sort pcounting
void sort_and_count_uniques(vector<dword_and_count> * pcounting,vector<U32> & vec_of_dwords);

void sort_dword_and_count_compare_count_highest_first(vector<dword_and_count> * pcounting);

void sort_dword_and_count_compare_dword(vector<dword_and_count> * pcounting);

//================================================================
// sixteen bytes (128 bits) (BC7)

struct bc7bits_and_count
{
	bc7bits val;
	U32 count;
};

struct bc7bits_and_count_compare_bc7bits
{
	bool operator ()(const bc7bits_and_count & lhs,const bc7bits_and_count & rhs)
	{
		if ( lhs.val == rhs.val )
		{
			// if values the same, sort count to highest first
			return lhs.count > rhs.count;
		}
		return lhs.val < rhs.val;
	}
};

struct bc7bits_and_count_compare_count_highest_first
{
	bool operator ()(const bc7bits_and_count & lhs,const bc7bits_and_count & rhs)
	{
		/*
		if ( lhs.count == rhs.count )
		{
			return lhs.val < rhs.val;
		}
		*/
		return lhs.count > rhs.count;
	}
};

struct bc7bits_and_count_compare_count_lowest_first
{
	bool operator ()(const bc7bits_and_count & lhs,const bc7bits_and_count & rhs)
	{
		/*
		if ( lhs.count == rhs.count )
		{
			return lhs.val < rhs.val;
		}
		*/
		return lhs.count < rhs.count;
	}
};

// sort_and_count_uniques sorts "vec_of_bc7bitss"
//  and fills "pcounting" with all unique bc7bitss & their occurance count
// it does NOT sort pcounting
void sort_and_count_uniques(vector<bc7bits_and_count> * pcounting,vector<bc7bits> & vec_of_bc7bitss);

void sort_bc7bits_and_count_compare_count_highest_first(vector<bc7bits_and_count> * pcounting);
void sort_bc7bits_and_count_compare_count_lowest_first(vector<bc7bits_and_count> * pcounting);

void sort_bc7bits_and_count_compare_bc7bits(vector<bc7bits_and_count> * pcounting);

//================================================================

// Bitpacked positions of bits that are set in value i, one index per nibble.
// Used for "filter" type ops.
extern const U32 c_active_lane_table8[256];

//================================================================

// AnyIndexD :
//  store SSD over a set of blocks
//	for any index dword used
//
// when you do a merge of blocks, AnyIndexD is just additive

struct AnyIndexD // 256 bytes
{
	enum
	{
		MAX_PIXEL_ERR = (3 + 4) * 255 * 255,		// max squared error of 255^2 in 3 channels, plus A which is weighted 4x
		MAX_BLOCK_ERR = MAX_PIXEL_ERR * 16,			// max error that can occur in a block of 16 pixels
		MAX_BLOCK_ERR_SCALED = MAX_BLOCK_ERR,    	// AnyIndex_D lookup used to scale but now doesn't
		MAX_BLOCKS_FOR_S32 = 0x7fffffff / MAX_BLOCK_ERR_SCALED,	// max number of blocks summed before we risk running into int32 overflow
	};

	U32 ssd[4][16]; // [selector][pixel] since it's more convenient for AnyIndexD_add SIMD
};

struct AnyIndex8D64
{
	U64 ssdx[16][8]; // [pixel][selector] since it's more convenient for AnyIndexD_lookup AVX2, and there's no SSE4 version
	// ssdx must fit in 56 bits for the accumulation loop we use in the AVX2 version.
	// Not a significant limitation - the individual per-block scores are scaled to fit in
	// at most 30 bits, and our chunks of blocks are 64k bytes which are at most 8k blocks
	// (2^13), so 43 bits would be sufficient. Plenty of room.
};

// Returns lowest distortion found, for early-out
S32 AnyIndexD_batch_lookup(CpuDispatchFlags dispatch, S32 out_d[], const AnyIndexD * aid, const U32 in_indices[], int count);
void AnyIndex8D64_batch_lookup(CpuDispatchFlags dispatch, S64 out_d[], const AnyIndex8D64 * aid, const U8 *indices_interleaved, const U8 *indices_scaled, ptrdiff_t indices_step, int count);

//================================================================

// Final matrix L1 norm index
//
// Computes L1 norms of a given chunk of RGBA888 or BGRA8888 4x4
// pixel blocks and allows rapidly identifying candidate blocks
// within a specified range of a target L1 norm.
//
// This is used by the BC1 and BC7 RDO encoders in their
// respective "final matrix" phases, hence the name.
//
// Processing works in chunks of a limited size. This is for
// cache blocking; we prefer any given index to cover a range
// of blocks that fits comfortably in the L1D cache. This
// results in near perfect hit rates and the resulting loops
// being entirely compute bound, which is where we want to be.
struct FinalMatrixL1Index
{
	// Chunk size. The actual pixel blocks here are 64 bytes
	// each. 256 corresponds to 16k L1D footprint, which means
	// no bad thrashing even when two hardware threads on a
	// core with 32k L1D run this code simultaneously.
	static const int CHUNK_SIZE = 256;

	// Extra padding for sorted norms. Used to avoid
	// special cases in some implementations.
	static const int PADDING = 16;

	alignas(16) U16 sorted_norms[CHUNK_SIZE + PADDING]; // norms of blocks, sorted in increasing order
	alignas(16) U16 sorted_norms_sub[CHUNK_SIZE / 16]; // subsampled norms, for some lookup strategies
	U16 sorted_inds[CHUNK_SIZE]; // corresponding indices of blocks within the chunk

	int count; // Number of entries. 0 <= count <= CHUNK_SIZE.
	int _padding[3]; // unused

	// Build the index for the given blocks
	void build(const rrColorBlock4x4 * blocks, int nblocks);

	// Look up all candidate blocks in the index that are
	// potentially within a L1 norm distance of must_beat_dist
	// (exclusive) of target_block. Returns the number of
	// candidates found and a pointer to the corresponding indices
	// in pOutInds. (This points into sorted_inds and does not
	// require any memory management.)
	inline int lookup(const rrColorBlock4x4 & target_block, U16 const * * pOutInds, int must_beat_dist);
};

//================================================================

// Compute SADs of one block against a bunch of other blocks
// This can be done more efficiently when batched.

// For RGBA pixels.
// This is equivalent to
//
//     for (int i = 0; i < count; ++i)
//         chunk_sads[i] = ColorBlock4x4_ComputeSAD_RGBA(pair_colors[indices[i]],rColors);
//
// just a good deal faster.
void compute_chunk_sads_indirect(CpuDispatchFlags dispatch, int chunk_sads[], int count, const U16 indices[], const rrColorBlock4x4 * pair_colors, const rrColorBlock4x4 & rColors);

// For groups of 16 U8 indices, densely packed in index_batch[],
// compute an index SAD and record the positions of the spots where that SAD
// is below "threshold".
int filter_by_index_sad(CpuDispatchFlags dispatch, int remaining_positions[], int count, const U8 * index_batch, const U8 ref_inds[16], int threshold);

// For groups of 16 U8 indices, densely packed in index_batch[],
// compute an index SAD and record the positions of the spots where that SAD
// is below "threshold".
//
// Also filter the matching packed 32-bit indices from in_indices[] to out_indices[].
int filter_by_index_sad_with_inds(CpuDispatchFlags dispatch,int remaining_positions[], U32 out_indices[], int count, const U8 * index_batch, const U32 * in_indices, const U8 ref_inds[16], int threshold);

// given lists of minimum SSD costs for SAD in each pixel, compute the maximum SAD that can give an SSD below ssd_limit
// out_ssd_limit, if given, receives the smallest SSD value that would yield the same result
S32 compute_sad_bound_for_ssd_threshold(S32 delta_ssd_per_pixel_per_sad[16][8], S32 base_ssd, S32 ssd_limit, S32 * out_ssd_limit=0);

//================================================================

struct rrDXT1_VQ_Entry
{
	U32 dw;	// rrDXT1EndPoints endpoints;
	F32 codelen;

	int block_count;
	int block_link;

	// acceleration for endpoint vq :
	rrColor32BGRA palette[4];
};

// Compute pairs of VQDs for arrays of rrDXT1_VQ_Entries indirected through inds:
//
//   bc1inds = vqindices[inds[i]].dw
//   dest_vqd[i*2 + 0] = VQD(colors,first_palette,bc1inds,activity)
//   dest_vqd[i*2 + 1] = VQD(colors,second_palette,bc1inds,activity)
//
// This is the ranking computation for BC1RD index substitution.
void batch_compute_VQDs(
	CpuDispatchFlags dispatch,
	F32 * dest_vqd,
	const rrDXT1_VQ_Entry * vqindices, const int * inds, int count32,
	const rrColorBlock4x4 & colors,
	const rrColor32BGRA first_palette[4],
	const rrColor32BGRA second_palette[4],
	const SingleFloatBlock4x4 & activity);

// Compute best possible SSDs for arrays of rrDXT1_VQ_Entries indirected through inds:
//
//   idx = inds[i] & 0xffff
//   dest_ssd[i] = DXT1_FindErrors(colors, vqendpoints[idx].palette)
//
// This is the ranking computation for BC1RD endpoint substitution.
void batch_find_best_errors(
	CpuDispatchFlags dispatch,
	S32 * dest_ssd,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColorBlock4x4 & colors);

// Filter candidate endpoint pairs using the bounding box criterion,
// and writes out both the position and index value of the survivors,
// bit-packed into dest_inds[]. Also rejects blocks that are in 4-color
// mode when the block requires 3-color mode (require_3c).
//
// It then returns the number of candidates that passed the test.
//
// out_count = 0
// for 0 <= i < count32:
//     int idx = inds[i]
//     bool block_4c = is_4color(vqendpoints[idx].dw);
//     bool rejected = reject_endpoints_color_bbox(vqendpoints[idx].palette, bbox, bbox_min_d);
//     rejected |= block_4c & require_3c;
//     if (!rejected)
//         dest_inds[out_count++] = (i << 16) | idx
//
// return out_count
//
// Values of dest_inds past the returned out_count may end up with
// garbage. However, it will never write more than count32 entries
// to dest_inds.
int filter_endpoint_color_bbox(
	CpuDispatchFlags dispatch,
	int * dest_inds,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColor32BGRA bbox[2],
	U32 bbox_min_d, bool require_3c);

S32 bc4rd_single_block_change_distortion_explicit(const S16 *src_pixels, const S16 palette[8], const U8 * index8_scaled_ptr);

//================================================================

namespace internal {

// do not call directly!
S32 AnyIndexD_batch_lookup_AVX2(S32 out_d[], const AnyIndexD * aid, const U32 in_indices[], int count);
S32 AnyIndexD_batch_lookup_AVX512(S32 out_d[], const AnyIndexD * aid, const U32 in_indices[], int count);
void AnyIndex8D64_batch_lookup_AVX2(S64 out_d[], const AnyIndex8D64 * aid, const U8 *indices_interleaved, int count);
void compute_chunk_sads_indirect_AVX2(int chunk_sads[], int count, const U16 indices[], const rrColorBlock4x4 * pair_colors, const rrColorBlock4x4 & rColors);
void compute_chunk_sads_indirect_AVX512(int chunk_sads[], int count, const U16 indices[], const rrColorBlock4x4 * pair_colors, const rrColorBlock4x4 & rColors);
int filter_by_index_sad_AVX2(int remaining_positions[], int count32, const U8 * index_batch, const U8 ref_inds_ptr[16], int threshold);
int filter_by_index_sad_AVX512(int remaining_positions[], int count32, const U8 * index_batch, const U8 ref_inds_ptr[16], int threshold);
int filter_endpoint_color_bbox_ARM_DP(
	int * dest_inds,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColor32BGRA bbox[2],
	U32 bbox_min_d, bool require_3c);
int filter_endpoint_color_bbox_AVX2(
	int * dest_inds,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColor32BGRA bbox[2],
	U32 bbox_min_d, bool require_3c);
int filter_endpoint_color_bbox_AVX512(
	int * dest_inds,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColor32BGRA bbox[2],
	U32 bbox_min_d, bool require_3c);
void batch_compute_VQDs_AVX2(
	F32 * dest_vqd,
	const rrDXT1_VQ_Entry * vqindices, const int * inds, int count32,
	const rrColorBlock4x4 & colors,
	const rrColor32BGRA first_palette[4],
	const rrColor32BGRA second_palette[4],
	const SingleFloatBlock4x4 & activity);
void batch_compute_VQDs_AVX512(
	F32 * dest_vqd,
	const rrDXT1_VQ_Entry * vqindices, const int * inds, int count32,
	const rrColorBlock4x4 & colors,
	const rrColor32BGRA first_palette[4],
	const rrColor32BGRA second_palette[4],
	const SingleFloatBlock4x4 & activity);
void batch_find_best_errors_ARM_DP(
	S32 * dest_ssd,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColorBlock4x4 & colors);
void batch_find_best_errors_AVX2(
	S32 * dest_ssd,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColorBlock4x4 & colors);
void batch_find_best_errors_AVX512(
	S32 * dest_ssd,
	const rrDXT1_VQ_Entry * vqendpoints, const int * inds, int count32,
	const rrColorBlock4x4 & colors);
S32 bc4rd_single_block_change_distortion_AVX2(const S16 *src_pixels, const S16 palette[8], const U8 * index8_scaled_ptr);
void bc4rd_single_block_change_distortion_indirect_batch_AVX2(
	S32 *dest_ssd,
	const S16 *src_pixels, const S16 palette[8], const U8 * index8_scaled_ptr, const int * inds, int count);

}

RR_NAMESPACE_END
