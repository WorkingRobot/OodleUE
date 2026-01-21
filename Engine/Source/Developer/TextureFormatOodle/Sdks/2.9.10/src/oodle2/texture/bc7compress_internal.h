// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "texbase.h"
#include "rrrand.h"
#include "bc7compress.h"
#include "bc67format.h"

OODLE_NS_START

typedef U64 BC7Flags;
typedef U32 BC7Error;
#define BC7_ERROR_MAX (~0u)

// BC7Flags :
enum
{
	BC7ENC_IGNORE_ALPHA			 = 0x00000001ll,
	BC7ENC_LSQ_FIT				 = 0x00000002ll,
	BC7ENC_PCA_STRETCH			 = 0x00000004ll,
	BC7ENC_JITTER_SELECTORS		 = 0x00000008ll,
	BC7ENC_ANNEAL_ENDPOINTS		 = 0x00000010ll,
	BC7ENC_PCA					 = 0x00000020ll,
	BC7ENC_EXTRAPOLATE_SELECTORS = 0x00000080ll,
	BC7ENC_SCALE_SELECTORS		 = 0x00000100ll,
	BC7ENC_REFINE_PARTITIONS	 = 0x00000200ll,
	BC7ENC_BRUTE_PBITS			 = 0x00000800ll,
	BC7ENC_IDXS_EXACT			 = 0x00002000ll,
	BC7ENC_PCA_PSEUDOSQUISH		 = 0x00008000ll,
	BC7ENC_PCA_TRY_HARD			 = 0x00010000ll, // try straddle fits for all PCA candidates
	BC7ENC_SLIDE_SELECTORS		 = 0x00020000ll,
	BC7ENC_BRUTE_PBITS_IDXS_EXACT= 0x00080000ll,
	BC7ENC_SEPARATE_ALPHA		 = 0x00100000ll, // alpha is special
	BC7ENC_MODE6_EARLY_LSQ_FIT	 = 0x00200000ll,

	BC7ENC_PRESERVE_A0		     = 0x00400000ll, // preserve A=0 exactly (internal use)
	BC7ENC_PRESERVE_A255		 = 0x00800000ll, // preserve A=255 exactly (internal use)
	BC7ENC_PRESERVE_EXTREMES     = BC7ENC_PRESERVE_A0 | BC7ENC_PRESERVE_A255, // preserve extremes in alpha channel exactly

	BC7ENC_AEP0_LO			     = 0x01000000ll, // A endpoint 0 must be low (for index constraints)
	BC7ENC_AEP0_HI				 = 0x02000000ll, // A endpoint 0 must be high (for index constraints)
	BC7ENC_AEP0_CONSTRAINED		 = BC7ENC_AEP0_LO | BC7ENC_AEP0_HI,

	BC7ENC_DISABLE_MODE0123		 = 0x04000000ll,
	BC7ENC_DISABLE_MODE67		 = 0x08000000ll,

	BC7ENC_DEFER_MODE13SEL		 = 0x10000000ll, // deferred mode 1/3 selection

	BC7ENC_INDEX_MASK			 = BC7ENC_IDXS_EXACT,
	BC7ENC_RETAIN_MASK			 = BC7ENC_IGNORE_ALPHA | BC7ENC_INDEX_MASK | BC7ENC_SEPARATE_ALPHA | BC7ENC_PRESERVE_EXTREMES,
	BC7ENC_RANDOMIZED_MASK		 = BC7ENC_JITTER_SELECTORS | BC7ENC_ANNEAL_ENDPOINTS, // passes that use the random seed
	BC7ENC_ALL_PCA				 = BC7ENC_PCA | BC7ENC_PCA_STRETCH | BC7ENC_PCA_PSEUDOSQUISH,
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
};

struct BC7EncOptions
{
	// number of partitions to search in trials phase :
	int max_part;
	bool share_partitions_mode;
	bool enable_mode[8];
	
	// used in trials phase by bc7enc_refine_subset_partition only if BC7ENC_REFINE_PARTITIONS :
	bool partition_pca;
	int partition_lsq_iters;
	
	// number of results to reduce to in the refinement phase :
	int narrow0;
	int narrow1;
	int narrow2;
	
	// intensity of search parameters :
	int max_lsq_iter;
	int max_pca_stretch_passes;
	int optiters_early[2]; // jitter & anneal optimization iterations
	int optiters_late[2]; // "early" is for narrow2 , "late" is final best one

	// alpha channel error weight
	int alpha_weight;
	int max_results_per_mode;

	BC7Flags flags;

	BC7EncOptions()
		: 
		max_part(64),
		share_partitions_mode(false),
		partition_pca(true),
		partition_lsq_iters(2),
		narrow0(32),
		narrow1(8),
		narrow2(4),
		max_lsq_iter(4),
		max_pca_stretch_passes(4),
		alpha_weight(1),
		max_results_per_mode(0),
		flags(0)
	{
		for LOOPARRAY(i,enable_mode) enable_mode[i] = true;
		optiters_early[0] = optiters_early[1] = optiters_late[0] = optiters_late[1] = 0;
	}
};

void bc7_enc_options_set(BC7EncOptions * popt,rrDXTCLevel level, rrDXTCOptions options);

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

// BC7Prep
//	precomputed information about a block
//	before any mode/coding decisions
//	const after init
struct BC7Prep
{
	enum {
		NINFO = 1 + 64 + 64,
	};

	BC7PartitionInfo infos[NINFO]; // 1 subset, 2 subsets, 3 subsets - indexed by real partition index in BC7 block.

	U8 local_block[64]; // @@ there may be some small benefit to making these color blocks 16-aligned
	bool single_color;
	bool has_alpha;
	U64 rand_seed;
	int max_ns;
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

struct BC7SubsetState
{
	BC7Error err; // total error (vec + scalar_err)
	BC7Error scalar_err; // error in scalar channel (A in ib2 mode 4/5)
	BC7Color endpoints[2]; // [lohi]
	BC7Color endpoints_q[2];
	
	BC7Inds idxs[2]; // indexes for color/alpha aka vec/scalar
	//	idxs[] are in dense linear order for the subset, not block order
	U8 pbits; // bit 0 and 1. Always pretend we're doing epbits; if shared, both need to match.

	// These two are actually per-block; you can't change them for a single subset, all
	// the subsets must match.
	U8 isbit; // index switch bit: used in mode 4, otherwise 0.
	U8 rbit; // channel rotate bits: used in modes 4/5, otherwise 0.

	void init_empty(U8 isb, U8 rb);
	void init(const BC7Color &endpt0, const BC7Color &endpt1, U8 isb);
	void set_rbit(int new_rbit); // only allowed when rbit==0 right now
};

struct BC7BlockState
{
	BC7Error err;
	U8 mode, p; // block mode and partition
	BC7SubsetState subsets[3];
};

// For every pixel in a block, sum of squared errors for each of the index options
// meant for accumulation
struct BC7IndexErrors
{
	RAD_ALIGN(BC7Error, err[16][16], 16); // [pixel][index]

	void init(); // clears to zero
};

// Linear least-squares state for a subset, for multi-block endpoint solves
struct BC7SubsetLLSState
{
	RAD_ALIGN(F32, bsum[4], 16);	// [chan]; sum of right-hand side values, per channel
	RAD_ALIGN(F32, ata[4], 16);  	// {ata00,ata10,ata11,<padding>} A^T A for vector index bits
	RAD_ALIGN(F32, atb0[4], 16); 	// [chan]; first row of A^T B for vector index bits
	RAD_ALIGN(F32, atab_2[4], 16);	// {ata00_2,ata10_2,ata11_2} A^T A for scalar index bits, A^T B in last lane
};

struct BC7BlockAndInput
{
	BC7BlockState state;
	BC7Input input;
};

struct BC7MultiBlocks
{
	// semantically this is like a vector_st<BC7BlockAndInput,MAX_INLINE>
	// with a bit of sugar in the "set" function, but I'm trying not to
	// make this internal header pull in heavy-weight templates
	enum { MAX_INLINE = 4 };

	int count;
	BC7BlockAndInput * data;

	explicit BC7MultiBlocks(int count);
	~BC7MultiBlocks();

	void set(int index, const U8 * block_bits, const U8 * pixels, BC7Flags flags);

private:
	BC7BlockAndInput inline_data[MAX_INLINE]; // used as storage when count is small
};

// ---- Internal API

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

// return value is the count of bits in the fields that were changed (endpoints + optionally pbits)
int bc7enc_find_endpoints(BC7BlockState *st,const U8 * block,  BC7Flags flags, bool change_pbits, bool slow_anneal);

// return value is the count of bits in the fields that were changed (indices + optionally pbits)
int bc7enc_reindex_block(BC7BlockState *st, const BC7SubsetInput *input, BC7Flags flags, bool change_pbits, bool slow_exact);

// bc7enc_decode_state decode a state to 64-byte rgba
// flags can be ignore alpha
// flags = 0 to match output of bc7_decode_block
void bc7enc_decode_state(const BC7BlockState *st, U8 * block, BC7Flags flags);

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

OODLE_NS_END

