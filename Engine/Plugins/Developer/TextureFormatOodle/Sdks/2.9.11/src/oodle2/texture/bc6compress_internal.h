// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "texbase.h"
#include "rrrand.h"
#include "bc6compress.h"
#include "bc67format.h"

OODLE_NS_START

typedef U32 BC6Error;
#define BC6_ERROR_MAX (~0u)

// BC6Flags
enum
{
	BC6ENC_LSQ_FIT					= 0x00000001ll,
	BC6ENC_PCA_STRETCH				= 0x00000002ll,
	BC6ENC_JITTER_SELECTORS			= 0x00000004ll,
	BC6ENC_ANNEAL_ENDPOINTS			= 0x00000008ll,
	BC6ENC_PCA						= 0x00000010ll,
	BC6ENC_SLIDE_SELECTORS			= 0x00000020ll,
	BC6ENC_EXTRAPOLATE_SELECTORS	= 0x00000040ll,
	BC6ENC_SIGNED					= 0x00000080ll,
	BC6ENC_AGGRESSIVE_MODE_PRUNE	= 0x00000100ll,
	BC6ENC_WEIGHTED_CHANNELS		= 0x00000200ll,

	BC6ENC_RETAIN_MASK				= 0,
	BC6ENC_RANDOMIZED_MASK			= BC6ENC_JITTER_SELECTORS | BC6ENC_ANNEAL_ENDPOINTS,
};

enum BC6Metric
{
	BC6METRIC_LOGSPACE, // definitely bad but nice to have as a sanity check sometimes
	BC6METRIC_MRSSE, // good, best yet when channel-weighted. use this one.
};

// MRSSE metric:
//
// We want something like a squared distance, but suitable to high dynamic range. The idea here is to start
// with (per pixel)
//
//   ||a - b||^2 / (||a||^2 + ||b||^2)
//
// so that we compare the size of the difference to the scale of the values. Then simplify this further by
// noting that if a is the original signal and b an approximation of a (as we have with color matching), then
// we expect ||a||^2 =~ ||b||^2, and thus the equation above can be approximated as
//
//   ||a - b||^2 / (2 ||a||^2)
//
// which has the massive advantage than in our case, a is constant for all trials within a single block,
// while b keeps changing every time. In our case, we have 16 pixels of RGB tuples for our block; let's
// number those 3-vectors a_1,...,a_16 and b_1,...,b_16. Then we can express the delta for a single pixel i
// as
//
//   ||a_i - b_i||^2 / (2 ||a_i||^2)
// = w_i ||a_i - b_i||^2
//
// where
//
//   w_i = 1 / (2 ||a_i||^2)
//
// is precomputed once per input block. This also allows us to handle annoying cases like ||a_i||=0 outside
// any hot loops. The main per-pixel computation is thus just an extra multiplication after a regular
// squared distance, very cheap.

template<int t_ns, int t_ib>
struct BC6IndexMode
{
	enum
	{
		ns = t_ns,
		ib = t_ib
	};
};

template<int t_ns, int t_is_signed>
struct BC6Mode
{
	enum
	{
		is_signed = t_is_signed,
		ns = t_ns,
		ib = (t_ns == 1) ? 4 : 3,
	};

	typedef BC6IndexMode<ns, ib> index_mode;
};

struct BC6ModeDesc
{
	U8 epb; // endpoint bits
	U8 db[3]; // delta bits per chan
	bool is_two_subset;
};

// 14 real modes plus two proxy modes for 1/2-subset
extern const BC6ModeDesc bc6_mode_desc[14 + 2];

struct BC6Input
{
	RAD_ALIGN(S32, blocki[3][16], 16); // [chan][index]

	RAD_ALIGN(F32, blockf32[3][16], 16); // [chan][index]
	RAD_ALIGN(F32, weightf32[16], 16); // the w_i for MRSSE (see comment above)

	// Not regular float values! These are floats for the float16 bit patterns!
	// I know it's weird.
	//
	// "alpha" channel = per-pixel LLS weights (initially 1.0)
	RAD_ALIGN(F32, blockf_lsq[64], 16);

	rrRandState rand_seed;

	BC6Metric active_metric;
	const F32 *channel_weights_sqrt;

	void init(const U16 blk_f16[64], const rrRandState &in_rand_seed, BC6Flags flags);

	// after setting up weightf32[] for LLS, run this to update the LLS weights
	void update_lls_weights();
};

struct BC6PartitionPrep
{
	float mean[2][3]; // [subset][chan]
	float pca[2][3]; // [subset][chan]

	void calc(const BC6Input &input, int p, int ns, bool is_signed, const BC6EncOptions *opt, S16 out_initial_endpoints[4][3]);
	void calc_subset_pca(int s, const float cov[6], const S16 ep[2][3]);
};

struct BC6Endpoints
{
	S16 raw[4][3]; // [subset*2+lohi][chan]
	S32 quant[4][3]; // [subset*2+lohi][chan]; S32 because these are 16 bits value + sign = 17 bits total

	template<typename Mode>
	inline void quantize(const BC6ModeDesc &desc);

	// NOTE: this is used to prune the modes to try, not whether a given encoding is valid or not;
	// we don't want to be too aggressive in ruling out candidates here!
	template<typename Mode>
	bool can_fit_mode(int index);
};

struct BC6PrepInfo
{
	BC6PartitionPrep prep;
	BC6Endpoints initial_ep;
};

// BC6Prep
//  caches precomputed PCAs for the partition types
//  const after init
struct BC6Prep
{
	BC6PrepInfo sub1; // one subset
	BC6PrepInfo sub2[32]; // two subsets

	void init(const BC6Input &in, const BC6EncOptions *opt);
};

struct BC6Inds
{
	U8 ind[16];
};

struct BC6BlockState
{
	BC6Inds idxs;
	BC6Error err;
	BC6Endpoints endpoints;
	U8 mode, p;

	BC6BlockState()
		: err(BC6_ERROR_MAX),
		mode(255),
		p(0)
	{
	}

	BC6BlockState(int in_mode, int in_p)
		: err(BC6_ERROR_MAX),
		mode(static_cast<U8>(in_mode)),
		p(static_cast<U8>(in_p))
	{
	}
};

// ---- Internal API

// Bring block into canonical form. This means swapping endpoints if necessary
// so the required-to-be-0 index bits actually are. This does _not_ change the
// deltas; everything must be in range (even after swaps happen) by the time
// you call this.
void bc6enc_canonicalize(BC6BlockState * st);

// Emit the encoded block bits. MUST be canonicalized first!
void bc6enc_emit(U8 * output_bc6h, const BC6BlockState * st);

// Make pixel values for input canonical.
// 16x RGBA F16 pixels (input A ignored, output A set to 1.0h)
void bc6enc_canonicalize_pixels(U16 block[64], const U16 in_block[64], bool is_signed);

// Encodes a single-color block for the given float16 RGBA values.
// (A ignored.)
void bc6enc_encode_single_color_block(U8 * output_bc6h, const U16 color[4], bool is_signed);

// Turns an encoded block back into a BC6BlockState (block must be valid)
void bc6enc_state_from_bits(BC6BlockState * st, const U8 * input_bc6h, bool is_signed);

// Evaluate error for current values in block state
BC6Error bc6enc_eval_error(const BC6BlockState * st, const BC6Input * input, bool is_signed);

// Calculate new indices for this input and return the resulting error (update st)
BC6Error bc6enc_reindex_block(BC6BlockState * st, const BC6Input * input, bool is_signed);

// Calculate new endpoints for this input, holding indices constant, and return resulting error.
BC6Error bc6enc_new_endpoints_with_indices_fixed(BC6BlockState * st, const BC6Input * input, bool is_signed);

OODLE_NS_END

