// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "texbase.h"
#include "rrrand.h"
#include "bc7compress.h"
#include "bc7kernels.h"
#include "bc67format.h"
#include "cpudispatch.h"

OODLE_NS_START

typedef U32 BC7Error;
#define BC7_ERROR_MAX (~0u)

//#define BC7_TRACK_PARTITION_ERRORS // To set up the tweakables in the mode/partition decision

#ifdef BC7_TRACK_PARTITION_ERRORS

typedef void BC7LogPartitionErrorFunc(int mode, F32 estimated_overall_err, F32 estimated_pca_err, BC7Error actual_err);
typedef void BC7LogFinalErrorFunc(int mode, int p_or_config, BC7Error actual_err);

extern BC7LogPartitionErrorFunc * g_bc7_log_partition_error;
extern BC7LogFinalErrorFunc * g_bc7_log_final_error;

#endif

// BC7Flags :
enum
{
	BC7ENC_IGNORE_ALPHA			 = 0x00000001ll,
	BC7ENC_LSQ_FIT				 = 0x00000002ll,
	BC7ENC_PCA_STRETCH			 = 0x00000004ll,
	BC7ENC_JITTER_SELECTORS		 = 0x00000008ll,
	BC7ENC_ANNEAL_ENDPOINTS		 = 0x00000010ll,
	BC7ENC_PCA					 = 0x00000020ll,
	BC7ENC_NO_INITIAL_BBOX		 = 0x00000040ll, // don't try initial BBox fit, just the PCA fit
	BC7ENC_EXTRAPOLATE_SELECTORS = 0x00000080ll,
	BC7ENC_SCALE_SELECTORS		 = 0x00000100ll,
	BC7ENC_MODE4_ISB1_ONLY		 = 0x00000200ll, // don't try isb=0 with Mode 4
	// gap here
	BC7ENC_BRUTE_PBITS			 = 0x00000800ll,
	// gap here
	BC7ENC_IDXS_EXACT			 = 0x00002000ll,
	// gap here
	BC7ENC_PCA_TRY_HARD			 = 0x00010000ll, // try straddle fits for all PCA candidates
	BC7ENC_SLIDE_SELECTORS		 = 0x00020000ll,
	BC7ENC_BRUTE_PBITS_IDXS_EXACT= 0x00080000ll,
	BC7ENC_SEPARATE_ALPHA		 = 0x00100000ll, // alpha is special

	// gap here

	BC7ENC_PRESERVE_A0		     = 0x00400000ll, // preserve A=0 exactly (internal use)
	BC7ENC_PRESERVE_A255		 = 0x00800000ll, // preserve A=255 exactly (internal use)
	BC7ENC_PRESERVE_EXTREMES     = BC7ENC_PRESERVE_A0 | BC7ENC_PRESERVE_A255, // preserve extremes in alpha channel exactly

	BC7ENC_AEP0_LO			     = 0x01000000ll, // A endpoint 0 must be low (for index constraints)
	BC7ENC_AEP0_HI				 = 0x02000000ll, // A endpoint 0 must be high (for index constraints)
	BC7ENC_AEP0_CONSTRAINED		 = BC7ENC_AEP0_LO | BC7ENC_AEP0_HI,

	BC7ENC_DISABLE_MODE0123		 = 0x04000000ll,
	BC7ENC_DISABLE_MODE67		 = 0x08000000ll,

	BC7ENC_INDEX_MASK			 = BC7ENC_IDXS_EXACT,
	BC7ENC_RETAIN_MASK			 = BC7ENC_IGNORE_ALPHA | BC7ENC_INDEX_MASK | BC7ENC_SEPARATE_ALPHA | BC7ENC_PRESERVE_EXTREMES,
	BC7ENC_RANDOMIZED_MASK		 = BC7ENC_JITTER_SELECTORS | BC7ENC_ANNEAL_ENDPOINTS, // passes that use the random seed
	BC7ENC_ALL_PCA				 = BC7ENC_PCA | BC7ENC_PCA_STRETCH,
};

template<int mode>
struct BC7Mode
{
	enum
	{
		index = mode,
		ns  = (0x21112323 >> (mode*4)) & 0xf,
		pb  = (0x60006664 >> (mode*4)) & 0xf,
		rb  = (0x00220000 >> (mode*4)) & 0xf,
		isb = (0x00010000 >> (mode*4)) & 0xf,
		cb  = (0x57757564 >> (mode*4)) & 0xf,
		ab  = (0x57860000 >> (mode*4)) & 0xf,
		epb = (0x11001001 >> (mode*4)) & 0xf,
		spb = (0x00000010 >> (mode*4)) & 0xf,
		ib  = (0x24222233 >> (mode*4)) & 0xf,
		ib2 = (0x00230000 >> (mode*4)) & 0xf,

		has_pbits = epb + spb,
	};
};

// BC7Color == rrColor32RGBA
union BC7Color
{
	struct
	{
		U8 r, g, b, a;
	};
	U8 v[4];
	U32 dw; // as DWord

	void copy_rgb_from(const BC7Color &x)
	{
		U8 old_a = a;
		*this = x;
		a = old_a;
	}

	bool operator ==(const BC7Color &x) const { return dw == x.dw; }
	bool operator !=(const BC7Color &x) const { return dw != x.dw; }
};

// BC7PartitionInfo
//	precomputed information about partition
//	not mode dependent
struct BC7PartitionInfo
{
	BC7Color bbox[3][2]; // [subset][lohi]

	void calc(const U8 block[64], int p, int ns, BC7Flags flags);

	// are min/max in the given subset the same?
	bool all_same(int s) const
	{
		return bbox[s][0].dw == bbox[s][1].dw;
	}
};

// BC7PartitionChoice
//   just a mode/partition shape combination
struct BC7PartitionChoice
{
	U8 mode;
	U8 partition;
	U8 subsets[3];

#ifdef BC7_TRACK_PARTITION_ERRORS
	U8 _padding[3];
	F32 overall_error; // Estimated squared error against the mean
	F32 pca_error; // Portion of the squared error aligned with the PCA axis
#endif
};

// BC7PartitionRanking
//   list of candidate partitions for 2- and 3-subset in order of preference
struct BC7Prep;

struct BC7PartitionRanking
{
	static constexpr int NBEST = 8; // This is a maximum, we don't need to have that many.

	BC7PartitionChoice best_part2[NBEST]; // Best partitions with 2 subsets
	BC7PartitionChoice best_part3[NBEST]; // Best partitions with 3 subsets

	S32 num_part2 = 0; // Number of ranked 2-subset partitions we actually have
	S32 num_part3 = 0; // Number of ranked 3-subset partitions we actually have
	U8 pca_subset1[3]; // Base partition IDs for 1-subset partitions (always the same)

	void compute(BC7Prep * prep, const BC7EncOptions & opt);
};

// BC7Prep
//	precomputed information about a block
//	before any mode/coding decisions
//	const after init
struct BC7Prep
{
	BC7PartitionInfo info_1sub; // 1-subset partition info is computed once

	U8 local_block[64]; // @@ there may be some small benefit to making these color blocks 16-aligned
	bool single_color;
	bool has_alpha;
	U64 rand_seed;
	int max_ns;
	bool analysis_initialized = false;

	BC7PartitionRanking ranking;
	BC7SubsetAnalysis analysis;
};

const U8 * BC7Prep_init(BC7Prep * prep, const U8 in_block[64], const BC7EncOptions & opt, bool force_consider_all_modes);

// BC7BlockInput gathers the pixels from a block into linear order for each subset
//	  also does channel rotation
//	after init they are [S0..][S1][S2]
struct BC7BlockInput
{
	enum
	{
		NPAD = 8, // assumes we process pixels at most 8 at a time
	};
	RAD_ALIGN(U8, pixels[(16+NPAD)*4], 64); // 16 pixels worth of extra padding for over-read

	void init(const U8 * block, U64 inds, U8 rbit);
};

// Describes the input data for a BC7 subset
// BC7SubsetInput just points into a portion of a BC7BlockInput
struct BC7SubsetInput
{
	const U8 * pixels;
	const U8 * count_mask;
	int num_pixels;
	float rcp_num_pixels;

	// weights for the channels in same channel order as in pixels[]
	U16 channel_weights[4]; // must be in [0,128] individually, sum of channel weights must be <=256 to avoid overflows

	// "pixels" here usually come from a BC7BlockInput
	// they need to have sufficient padding!
	void init(const U8 * pixels, int num, int alpha_chan, int alpha_weight);
};

// BC7Input must be made per mode+part+rbits
struct BC7Input
{
	BC7BlockInput block;
	BC7SubsetInput subsets[3];
};

// This struct exists just because structs can be assigned directly
// whereas arrays can't.
struct BC7Inds
{
	U8 ind[16];
};

struct BC7SubsetEndpoints
{
	BC7Color endpoints[2]; // [lohi]
};

struct alignas(16) BC7SubsetState
{
	// idxs[] are in dense linear order for the subset, not block order
	// put them first so they're aligned since we often access them as vectors
	BC7Inds idxs[2]; // indexes for color/alpha aka vec/scalar

	// 32B so far

	// Two sets of endpoints, "endpoints" are our "desired" endpoints, solves write into these directly.
	// They're not necessarily representable in our chosen mode.
	//
	// "endpoints_q" are quantized endpoints with pbits applied, generally set by calc_endpoints_and_pbits.
	// These are actually representable in BC7 blocks, with one caveat: in "ignore alpha" mode, we clear the
	// A values in our input block to 255, and also make sure to set up endpoints_q so that the channel that
	// decodes to alpha has 255 for both endpoints, even if that value is not representable for the current
	// parameters (e.g. due to pbits in mode 7). This guarantees that all decoded A values are 255 and the
	// error in the alpha channel always comes out at 0.
	BC7Color endpoints[2]; // [lohi]
	BC7Color endpoints_q[2]; // [lohi]

	// 48B so far
	
	BC7Error err; // total error (vec + scalar_err)
	BC7Error scalar_err; // error in scalar channel (A in ib2 mode 4/5)

	U8 pbits; // bit 0 and 1. Always pretend we're doing epbits; if shared, both need to match.

	// These two are actually per-block; you can't change them for a single subset, all
	// the subsets must match.
	U8 isbit; // index switch bit: used in mode 4, otherwise 0.
	U8 rbit; // channel rotate bits: used in modes 4/5, otherwise 0.
	U8 pca_subset; // (not in format) which PCA subset this corresponds to (1-based); 0 if not set.

	U32 _padding; // unused
	
	// 64B total

	void init_empty(U8 isb, U8 rb);
	void init(const BC7Color &endpt0, const BC7Color &endpt1, U8 isb);
	void set_rbit(int new_rbit); // only allowed when rbit==0 right now
};

struct alignas(16) BC7BlockState
{
	// subset info first since it wants to be aligned
	BC7SubsetState subsets[3];
	BC7Error err;
	U8 mode, p; // block mode and partition
	U8 _padding[2 + 8]; // unused
};

// For every pixel in a block, sum of squared errors for each of the index options
// meant for accumulation
struct alignas(16) BC7IndexErrors
{
	BC7Error err[16][16]; // [pixel][index]

	void init(); // clears to zero
};

// Linear least-squares state for a subset, for multi-block endpoint solves
struct alignas(16) BC7SubsetLLSState
{
	F32 bsum[4];	// [chan]; sum of right-hand side values, per channel
	F32 ata[4];  	// {ata00,ata10,ata11,<padding>} A^T A for vector index bits
	F32 atb0[4]; 	// [chan]; first row of A^T B for vector index bits
	F32 atab_2[4];	// {ata00_2,ata10_2,ata11_2} A^T A for scalar index bits, A^T B in last lane
};

// Endpoint coefficient cache entry, used for single-block repeated LLS solves
// Similar to above, but higher-traffic and used with varying blocks so we can't cache
// bsum.
//
// A block needs three cache entries in general (either one per subset with three subsets,
// or two per subset with a single subset)
struct alignas(16) BC7EndpointCoeffCacheEntry
{
	F32 iata[4];	// {iata11,-iata10,iata00,<ignored>}
};

struct BC7EndpointCoeffCache
{
	BC7EndpointCoeffCacheEntry entry[3]; // Either 3 subsets * 1 entry, or 1 subset * 2 entries
};

struct BC7BlockAndInput
{
	BC7BlockState state;
	BC7Input input;
};

// Used for vector index finding kernels
struct alignas(16) BC7CalcVectorIndsDesc
{
	S8 lerp_factor_neg2x[16];
	F32 scale_factor[4]; // repeated 4x for SIMD
	S16 vec_channel_mask[8]; // repeated 2x for SIMD
	S8 scalar_lerp_factor_neg2x[16]; // lerp factors for scalar channel (if present)
	F32 scalar_scale_factor[4]; // repeated 4x for SIMD
	U8 scalar_inds_max[16]; // repeated 16x for SIMD
};

// Encapsulates storage for BC7MultiBlocks
class BC7MultiBlockStorage
{
	BC7BlockAndInput * storage;
	int capacity;

public:
	BC7MultiBlockStorage();
	~BC7MultiBlockStorage();

	// frees current storage
	void reset();

	// potentially destructive resize, called when the storage currently has no used slots
	BC7BlockAndInput * acquire(int count);

};

struct BC7MultiBlocks
{
	int count;
	BC7BlockAndInput * data;

	BC7MultiBlocks(BC7MultiBlockStorage * storage, int count);

	void set(int index, const U8 * block_bits, const U8 * pixels, BC7Flags flags);
};

typedef int BC7EncFindEndpointsModeFunc(BC7BlockState *st, const BC7BlockState *src_st, const BC7Input *input, BC7Flags flags, bool change_pbits, bool slow_anneal, const BC7EndpointCoeffCache *cache);
extern BC7EncFindEndpointsModeFunc * const g_bc7enc_find_endpoints_mode[8];

typedef int BC7EncReindexBlockModeFunc(BC7BlockState *st, const BC7BlockState *src_st, const BC7SubsetInput *input, BC7Flags flags);
extern BC7EncReindexBlockModeFunc * const g_bc7enc_reindex_block_mode[8];

typedef void BC7EncDecodeStateModeFunc(const BC7BlockState *in_state, U8 *block, BC7Flags flags);
extern BC7EncDecodeStateModeFunc * const g_bc7enc_decode_state_mode[8];

// ---- Internal API (used by BC7RD compressor etc.)

// Bring block into canonical form. This means swapping endpoints
// if necessary so the required-to-be-0 index bits actually are.
void bc7enc_canonicalize(BC7BlockState * st);

// Emit the encoded block bits. MUST be canonicalized first!
void bc7enc_emit(U8 * ouput_bc7, const BC7BlockState * st, BC7Flags flags);

// Turn an encoded block back into a BC7BlockState (block must
// be valid)
void bc7enc_state_from_bits(BC7BlockState * st, const U8 * input_bc7, BC7Flags flags);

void bc7_encode_single_color_block(U8 * output_bc7, const U8 rgba[4]);

// Checks whether the given block is an encoding of a BC7 solid-color block that we
// emitted
bool bc7_is_encoded_single_color_block(const U8 * bc7_block);

void BC7Input_Get(BC7Input * input,const U8 * block, int ns, int p, int rbit, int alpha_weight);
void BC7Input_Get(BC7Input * input,const U8 * block, const BC7BlockState * st, int alpha_weight=1);

BC7Error bc7enc_calc_error(const BC7BlockState *st, const U8 * block, BC7Flags flags);

bool bc7enc_check_endpoints_q(BC7BlockState *st, BC7Flags flags);

// Return value is the count of bits in the fields that were changed (endpoints + optionally pbits)
// Can pass optional coefficient cache, initialized with bc7enc_find_endpoints_init_cache below.
inline int bc7enc_find_endpoints(BC7BlockState *st, const BC7BlockState *src_st, const BC7Input *input, BC7Flags flags, bool change_pbits, bool slow_anneal, const BC7EndpointCoeffCache *cache=nullptr)
{
	RR_ASSERT(src_st->mode >= 0 && src_st->mode <= 7);
	return g_bc7enc_find_endpoints_mode[src_st->mode](st, src_st, input, flags, change_pbits, slow_anneal, cache);
}

// Initialize endpoint coefficient cache
void bc7enc_find_endpoints_init_cache(BC7EndpointCoeffCache *cache, const BC7BlockState *st, const BC7Input *input);

// return value is the count of bits in the fields that were changed (indices)
inline int bc7enc_reindex_block(BC7BlockState *st, const BC7BlockState *src_st, const BC7SubsetInput *input, BC7Flags flags)
{
	RR_ASSERT(src_st->mode >= 0 && src_st->mode <= 7);
	return g_bc7enc_reindex_block_mode[src_st->mode](st, src_st, input, flags);
}

// bc7enc_decode_state decode a state to 64-byte rgba
// flags can be ignore alpha
// flags = 0 to match output of bc7_decode_block
inline void bc7enc_decode_state(const BC7BlockState *st, U8 * block, BC7Flags flags)
{
#if 1
	RR_ASSERT(st->mode >= 0 && st->mode <= 7);
	g_bc7enc_decode_state_mode[st->mode](st, block, flags);
#else
	// just to make sure : yes decode_state_mode is a lot faster than this

	U8 out[16];
	bc7enc_emit(out,st);
	bc7_decode_block_fast(block,out);
#endif
}

BC7Flags BC7_ComputeEffectiveFlags(BC7Flags in_flags, const BC7Prep * in_prep);

bool BC7_CompressBlock_Sub(U8 * output_bc7, const U8 * block, const BC7EncOptions * in_opt, const BC7Prep * in_prep, const BC7_Mode_ErrBias * mpeb);

// accumulates the overall errors for every possible choice of index at every pixel
// either one or two index vectors depending on mode
//
// NOTE: requires endpoints in block state to be exactly representable in given mode
// (automatically true for result of bc7enc_decode_state)
//
// errs[0] is for idxs[0], errs[1] is for idxs[1]
void bc7enc_accumulate_index_errors(BC7IndexErrors errs[2], const BC7BlockState * st, const BC7Input * input, BC7Flags flags);

// Given accumulated IndexErrors for several blocks, pick whatever legal combination
// yields the minimum overall error
void bc7enc_reindex_for_min_error(BC7BlockState * st, const BC7IndexErrors errs[2]);

// Multi-block solve. Endpoints end up in st.
void bc7enc_find_endpoints_multi(BC7BlockState * st, const BC7MultiBlocks * multi, BC7Flags flags, bool change_pbits, bool slow_anneal);

// ---- Computational kernels used internally for dispatch

// do not call directly!
namespace internal {
	BC7Error avx2_calc_inds_linear(U8 out_inds[16], const BC7SubsetInput *in, const BC7CalcVectorIndsDesc *desc, const BC7Color endpoints_q[2]);
	BC7Error avx2_calc_inds_linear_twoindex(BC7Error *pscalar_err, U8 out_indv[16], U8 out_inds[16], const BC7SubsetInput *in, const BC7CalcVectorIndsDesc *desc, const BC7Color endpoints_q[2]);
	BC7Error avx2_calc_vector_err(const U8 inds[], const BC7SubsetInput *in, const BC7CalcVectorIndsDesc* desc, const BC7Color endpoints_q[2]);
}

OODLE_NS_END

