// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetsse4

#include "bc6compress_internal.h"
#include "bc67tables.h"
#include "templates/rralgorithm.h"
#include "vec128.inl"
#include <float.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "rrsimd.h"

//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"

#if defined(_MSC_VER) && _MSC_VER < 1500
#pragma warning(disable: 4244)
#endif

//#define LOGSPACE_LUMA_WEIGHT
#define MRSSE_LUMA_WEIGHT

#define PCA_POWER_ITERS 3

OODLE_NS_START

const BC6ModeDesc bc6_mode_desc[] =
{
	{ 10, { 5, 5, 5 }, true }, // mode 0
	{  7, { 6, 6, 6 }, true }, // mode 1
	{ 11, { 5, 4, 4 }, true }, // mode 2
	{ 11, { 4, 5, 4 }, true }, // mode 3
	{ 11, { 4, 4, 5 }, true }, // mode 4
	{  9, { 5, 5, 5 }, true }, // mode 5
	{  8, { 6, 5, 5 }, true }, // mode 6
	{  8, { 5, 6, 5 }, true }, // mode 7
	{  8, { 5, 5, 6 }, true }, // mode 8
	{  6, { 0, 0, 0 }, true }, // mode 9
	{ 10, { 0, 0, 0 }, false }, // mode 10
	{ 11, { 9, 9, 9 }, false }, // mode 11
	{ 12, { 8, 8, 8 }, false }, // mode 12
	{ 16, { 4, 4, 4 }, false }, // mode 13

	// ---- these are not real modes
	{ 12, { 0, 0, 0 }, false }, // "mode 14" (proxy for one-subset)
	{ 11, { 0, 0, 0 }, true },  // "mode 15" (proxy for two-subset)
};

// Determined by histogramming which partitions got chosen on a test set; it turns out
// the results came back perfectly classified by type of partition and with clearly
// visible "natural breaks", so instead of strictly ordering by popularity, I'm
// keeping the breaks between different partition types and just leaving them in
// numerical order within each group.
static const int bc6_partition_order[32] =
{
	0,13,				// 0-1: horiz/vert cut in half (cumulative total: ~21% coverage)
	1,2,14,15,			// 2-5: asymmetric horizontal/vertical splits (~42% coverage up to here)
	3,10,16,23,			// 6-9: more popular diagonal half-cuts (~57% coverage up to here)
	6,7,19,21,			// 10-13: less popular diagonal half-cuts (~69% coverage up to here)
	26,29,				// 14-15: horizontal and vertical stripe (~74% coverage up to here)
	5,11,12,17,18,25,	// 16-21: asymmetric diagonal cuts (~85% coverage up to here)
	4,8,9,20,22,24,		// 22-27: more asymmetric diagonal cuts (~96% coverage up to here)
	27,28,30,31,		// 28-31: rotated stripes
};

template<typename T> inline T radtex_sqr(T a) { return a*a; }
template<typename T> inline T radtex_min(T a, T b) { return a < b ? a : b; }
template<typename T> inline T radtex_max(T a, T b) { return a > b ? a : b; }
template<typename T> inline T radtex_clamp(T a, T b, T c) { return a > c ? c : a < b ? b : a; }

template<typename T> inline void radtex_swap(T &a, T &b)
{
	T tmp = a;
	a = b;
	b = tmp;
}

static RADFORCEINLINE U32 float2bits(F32 f)
{
	U32 u;
	RR_COMPILER_ASSERT(sizeof(u) == sizeof(f));
	memcpy(&u, &f, sizeof(f));
	return u;
}

static RADFORCEINLINE F32 bits2float(U32 u)
{
	F32 f;
	RR_COMPILER_ASSERT(sizeof(u) == sizeof(f));
	memcpy(&f, &u, sizeof(f));
	return f;
}

// bc6_quant goes from two's-complement-but-otherwise-half-float bit pattern "ep" to
// BC6 quantized values
template<int is_signed>
RADFORCEINLINE int bc6_quant(int epb, int ep)
{
	if (!is_signed)
	{
		// epb<16 gets a center reconstruction, epb==16 does not (see logic in bc6_dequant),
		// hence the asymmetry.
		if (epb != 16)
			return (ep << epb) / 0x7C00;
		else
			return (ep * 64 + 30) / 31;
	}
	else
	{
		// two remaps;
		//	 x |-> x * 32 / 31 (inverse of finish_unquantize)
		// then
		//	 x |-> x * pow(2, (epb-1) - 15)  (actual endpoint quant)
		// combine to
		//	 x |-> (x << (epb-1)) / (31 * (32768/32))
		if (epb != 16)
			return (ep << (epb - 1)) / 0x7C00;
		else
			return (ep * 32 + (ep >= 0 ? 30 : -30)) / 31;
	}
}

// bc6_dequant goes from BC6 quantized values to the internal 17-bit integer fixed point
// representation used for interpolation; it does _not_ go straight back to half-float,
// that happens _after_ interpolation! (via bc6_scaled_to_half)
template<int is_signed>
static int bc6_dequant(int epb, int v)
{
	if (!is_signed)
	{
		if (epb < 16)
		{
			int zext_mask = (1 << epb) - 1;
			v &= zext_mask;
			if (v == 0)
				return 0;
			else if (v == zext_mask)
				return 0xffff;

			return ((v << 16) + 0x8000) >> epb;
		}
		else
			return v & 0xffff;
	}
	else
	{
		int sign_bit = 1 << (epb - 1);
		int value_bits = sign_bit - 1;

		// sign extend from source width
		int x = (v & value_bits) - (v & sign_bit);
		if (epb >= 16)
			return x;

		// extract sign and absolute value
		int xsign = x >> 31;
		int xabs = (x ^ xsign) - xsign;

		// dequantize
		int deq;
		if (xabs == 0)
			deq = 0;
		else if (xabs >= value_bits)
			deq = 0x7fff;
		else
			deq = ((xabs << 16) + 0x8000) >> epb;

		// re-apply sign
		return (deq ^ xsign) - xsign;
	}
}

template<int is_signed>
RADFORCEINLINE int bc6_scaled_to_half(int x)
{
	// unsigned variant
	return (x * 31) >> 6;
}

template<>
RADFORCEINLINE int bc6_scaled_to_half<1>(int x)
{
	// signed variant
	return (x * 31) / 32;
}

static int remap_half_to_int(U16 half_flt)
{
	return (half_flt < 0x8000) ? half_flt : 0x8000 - half_flt;
}

static S16 round_endpoint(F32 val, int min_v)
{
	int rounded = static_cast<int>(val + (val >= 0 ? 0.5f : -0.5f));
	return static_cast<S16>(radtex_clamp(rounded, min_v, 0x7bff));
}

static F32 half_to_float32(U16 bits)
{
	rrFP16 fp16;
	rrFP16_SetBits(&fp16, bits);
	return rrFP16_GetFloat(&fp16);
}

// sqrt(weight) per channel
//
// Luma weights derived as follows:
// from linear RGB, CIE Y = [0.17697 0.81240 0.01063] [r g b]^T
// take the square roots of those weights so we don't go quite that drastic,
// and normalize the result so it sums to 1
// then take that and again take the square root because we want our metric to be
//
//    weight_r * diff_r^2 + weight_g * diff_g^2 + weight_b * diff_b^2
//
// but compute it as (to pull some arithmetic out of the innermost loops)
//
//    (weight_r' * diff_r)^2 + (weight_g' * diff_g)^2 + (weight_b' * diff_b)^2
//
// and to make that work, we need
//
//   weight_r' = sqrt(weight_r)
//   weight_g' = sqrt(weight_g)
//   weight_b' = sqrt(weight_b)
static const F32 weights_sqrt[2][3] =
{
	{ 1.00000f, 1.00000f, 1.00000f }, // no weighting
	{ 0.54331f, 0.79528f, 0.26897f }, // luma-weighted (see notes above)
};

void BC6Input::init(const U16 blk_f16[64], const rrRandState &in_rand_seed, BC6Flags flags)
{
	active_metric = BC6METRIC_MRSSE;
	channel_weights_sqrt = (flags & BC6ENC_WEIGHTED_CHANNELS) ? weights_sqrt[1] : weights_sqrt[0];

	for (int i = 0; i < 16; ++i)
	{
		blocki[0][i] = static_cast<S16>(remap_half_to_int(blk_f16[i*4 + 0]));
		blocki[1][i] = static_cast<S16>(remap_half_to_int(blk_f16[i*4 + 1]));
		blocki[2][i] = static_cast<S16>(remap_half_to_int(blk_f16[i*4 + 2]));

		blockf_lsq[i*4 + 0] = static_cast<F32>(blocki[0][i]);
		blockf_lsq[i*4 + 1] = static_cast<F32>(blocki[1][i]);
		blockf_lsq[i*4 + 2] = static_cast<F32>(blocki[2][i]);
		blockf_lsq[i*4 + 3] = 1.0f; // unit weights

		blockf32[0][i] = half_to_float32(blk_f16[i*4 + 0]) * channel_weights_sqrt[0];
		blockf32[1][i] = half_to_float32(blk_f16[i*4 + 1]) * channel_weights_sqrt[1];
		blockf32[2][i] = half_to_float32(blk_f16[i*4 + 2]) * channel_weights_sqrt[2];

		F32 sum_sq = 0.0f;
		sum_sq = radtex_sqr(blockf32[0][i]) + radtex_sqr(blockf32[1][i]) + radtex_sqr(blockf32[2][i]);
		sum_sq = RR_MAX(sum_sq, 1.42108547e-14f); // (2^(-23))^2 = smallest float16 subnormal, squared
		weightf32[i] = 0.5f / sum_sq; // 1 / (2 * sum_sq)
	}

	rand_seed = in_rand_seed;
}

void BC6Input::update_lls_weights()
{
	for (int i = 0; i < 16; ++i)
		blockf_lsq[i*4 + 3] = sqrtf(weightf32[i]);
}

void BC6PartitionPrep::calc(const BC6Input &input, int p, int ns, bool is_signed, const BC6EncOptions *opt, S16 out_initial_endpoints[4][3])
{
	int clampv = is_signed ? -0x7bff : 0;

	U64 inds = radtex_subset_to_inds[ns-1][p];
	for (int s = 0; s < ns; ++s)
	{
		int num_pixels = radtex_num_pixels_per_subset[ns][p][s];

	#if defined(DO_BUILD_SSE4) || defined(DO_BUILD_NEON64)
		VecF32x4 vmin(65504.0f);
		VecF32x4 vmax(-65504.0f);
		VecF32x4 vsum = VecF32x4::zero();

		// We store the covar matrix by diagonals, which looks funky but reduces the number
		// of shuffles we need to do:
		//
		// cov0 stores diagonal elements:      [0][0], [1][1], [2][2], and 0 (padding)
		// cov1 stores superdiagonal elements: [0][1], [1][2], [0][2], and 0 (padding)
		VecF32x4 cov0 = VecF32x4::zero();
		VecF32x4 cov1 = VecF32x4::zero();

		for (int j = 0; j < num_pixels; ++j)
		{
			int i = static_cast<int>(inds & 0xf);
			inds >>= 4;

			VecF32x4 pixel = VecF32x4::loadu(input.blockf_lsq + i*4);
			vmin = OODLE_NS::vmin(vmin, pixel);
			vmax = OODLE_NS::vmax(vmax, pixel);
			vsum += pixel;
			cov0 += pixel * pixel;
			cov1 += pixel * pixel.yzxw();
		}

		// See notes below in scalar version.
		VecF32x4 sf(1.0f / num_pixels);
		vsum *= sf;
		cov0 = cov0 * sf - vsum * vsum;
		cov1 = cov1 * sf - vsum * vsum.yzxw();

		// Format the output the way the remaining code expects it
		RAD_ALIGN(float, minf[4], 16);
		RAD_ALIGN(float, maxf[4], 16);
		RAD_ALIGN(float, sumf[4], 16);
		RAD_ALIGN(float, cov[7], 16);

		vmin.storea(minf);
		vmax.storea(maxf);
		vsum.storea(sumf);
		cov0.storea(cov);
		cov1.storeu(cov + 3);
	#else
		float minf[3] = {  65504.f,  65504.f,  65504.f };
		float maxf[3] = { -65504.f, -65504.f, -65504.f };
		float sumf[3] = { 0.0f, 0.0f, 0.0f };
		float cov[6]  = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };

		float sf = 1.0f / static_cast<float>(num_pixels);

		for (int j = 0; j < num_pixels; ++j)
		{
			int i = static_cast<int>(inds & 0xf);
			inds >>= 4;

			float r = input.blockf_lsq[i*4 + 0];
			float g = input.blockf_lsq[i*4 + 1];
			float b = input.blockf_lsq[i*4 + 2];

			minf[0] = radtex_min(minf[0], r);
			maxf[0] = radtex_max(maxf[0], r);
			sumf[0] += r;

			minf[1] = radtex_min(minf[1], g);
			maxf[1] = radtex_max(maxf[1], g);
			sumf[1] += g;

			minf[2] = radtex_min(minf[2], b);
			maxf[2] = radtex_max(maxf[2], b);
			sumf[2] += b;

			// Accumulate sums-of-squares
			cov[0] += r * r;
			cov[1] += g * g;
			cov[2] += b * b;
			cov[3] += r * g;
			cov[4] += g * b;
			cov[5] += b * r;
		}

		// Divide sum-of-squares matrix by count and subtract out mean*mean^T to get the actual covariance matrix
		// NOTE: our input is half-floats so working in floats here is essentially double precision relative to
		// the input, which should hopefully let us get away with this. In general the two-pass method (calculating
		// mean first then computing covar from block[i]-mean) is numerically preferable but in this case we
		// should be OK. (But keep an eye on this.)
		//
		// Reason this should be OK: the inputs have 10-bit mantissa so the squares have 20, and we sum at most 16
		// of them so this fits in 24 bits mantissa of a float. We should not be losing any significant bits purely
		// from the summation. If the values are of very differing scales then of course we get float rounding but
		// in that case one of these values really does matter much more for the covariance matrix so again we should
		// be good.
		//
		// We also do the sum-of-squares for BC7, but there we have 8-bit integer inputs: that means the squared values
		// fit in 16 bits and the sums over up to 16 pixels fit in 20 bits, still exact as floats, so there is
		// absolutely no concern in that instance.
		sumf[0] *= sf;
		sumf[1] *= sf;
		sumf[2] *= sf;
		cov[0] = cov[0]*sf - sumf[0]*sumf[0];
		cov[1] = cov[1]*sf - sumf[1]*sumf[1];
		cov[2] = cov[2]*sf - sumf[2]*sumf[2];
		cov[3] = cov[3]*sf - sumf[0]*sumf[1];
		cov[4] = cov[4]*sf - sumf[1]*sumf[2];
		cov[5] = cov[5]*sf - sumf[2]*sumf[0];
	#endif

		for (int c = 0; c < 3; ++c)
		{
			mean[s][c] = sumf[c];
			out_initial_endpoints[s*2 + 0][c] = round_endpoint(minf[c], clampv);
			out_initial_endpoints[s*2 + 1][c] = round_endpoint(maxf[c], clampv);
		}

		calc_subset_pca(s, cov, out_initial_endpoints + s*2);
	}
}

void BC6PartitionPrep::calc_subset_pca(int s, const float cov[6], const S16 ep[2][3])
{
	float local_pca[3];
	float len_sq = 0.0f;
	float norm_factor = 1.0f;
	for (int i = 0; i < 3; i++)
	{
		local_pca[i] = static_cast<float>(static_cast<int>(ep[1][i]) - static_cast<int>(ep[0][i]));
		len_sq += local_pca[i] * local_pca[i];
	}

	if (len_sq <= 2.0f*2.0f)
		memset(pca[s], 0, sizeof(pca[s]));
	else
	{
		for(int iter=0;iter<PCA_POWER_ITERS;iter++)
		{
			// cov stored by diagonals
			float x = cov[0]*local_pca[0] + cov[3]*local_pca[1] + cov[5]*local_pca[2];
			float y = cov[3]*local_pca[0] + cov[1]*local_pca[1] + cov[4]*local_pca[2];
			float z = cov[5]*local_pca[0] + cov[4]*local_pca[1] + cov[2]*local_pca[2];
			len_sq = x*x + y*y + z*z;
			if (!(len_sq > 0.0f)) // written to catch NaNs
				break;

			norm_factor = 1.0f / sqrtf(len_sq);
			local_pca[0] = x * norm_factor;
			local_pca[1] = y * norm_factor;
			local_pca[2] = z * norm_factor;
		}
		pca[s][0] = local_pca[0];
		pca[s][1] = local_pca[1];
		pca[s][2] = local_pca[2];
	}
}

template<typename Mode>
void BC6Endpoints::quantize(const BC6ModeDesc &desc)
{
	for (int i = 0; i < Mode::ns*2; ++i)
	{
		for (int c = 0; c < 3; ++c)
			quant[i][c] = bc6_quant<Mode::is_signed>(desc.epb, raw[i][c]);
	}
}

template<typename Mode>
bool BC6Endpoints::can_fit_mode(int index)
{
	// NOTE(fg): need a dedicated test to verify this actually works properly
	const int max_delta = 0x7c00 << Mode::is_signed; // supported half-float bit patterns are in [-0x7bff,0x7bff]
	const int too_large_delta = max_delta + (max_delta >> 2); // a bit of slack so we don't rule out modes that might be viable (albeit with some clamping)

	const BC6ModeDesc &desc = bc6_mode_desc[index];

	// With no delta, we can always fit!
	if (desc.db[0] == 0)
		return true;

	for (int c = 0; c < 3; ++c)
	{
		// What fraction of the range is covered by delta-ing:
		// 1 bit of the delta is the sign bit, the rest needs to cover the range
		int bit_loss = desc.epb - (desc.db[c] - 1);
		int too_large = too_large_delta >> bit_loss;

		for(int i = 1; i < 2*Mode::ns; ++i)
		{
			int diff = raw[i][c] - raw[0][c];
			if(abs(diff) >= too_large)
				return false;
		}
	}

	return true;
}

void BC6Prep::init(const BC6Input &in, const BC6EncOptions *opt)
{
	memset(this, 0, sizeof(*this));

	bool is_signed = (opt->flags & BC6ENC_SIGNED) != 0;

	sub1.prep.calc(in, 0, 1, is_signed, opt, sub1.initial_ep.raw);
	for (int i = 0; i < opt->max_part; ++i)
	{
		int p = bc6_partition_order[i];
		sub2[p].prep.calc(in, p, 2, is_signed, opt, sub2[p].initial_ep.raw);
	}
}

struct BC6Results
{
	enum { MAX_RESULTS = 64 };

	struct Result
	{
		BC6Error err;
		BC6BlockState *slot;

		bool operator <(const Result &x) const
		{
			return err < x.err;
		}
	};

	BC6Error best_err;
	int num; // capacity
	int used; // number of slots actually used
	Result heap[MAX_RESULTS];
	BC6BlockState pool[MAX_RESULTS];

	BC6Results()
		: best_err(BC6_ERROR_MAX),
		num(MAX_RESULTS),
		used(0),
		pool()
	{
		// initialize heap with all the free slots
		for (int i = 0; i < num; ++i)
		{
			heap[i].err = BC6_ERROR_MAX;
			heap[i].slot = &pool[i];
			pool[i].err = BC6_ERROR_MAX;
		}
	}

	void add_state(BC6BlockState *st)
	{
		// keep track of current best_err
		if(st->err < best_err)
			best_err = st->err;

		// if we're not full, we can just insert immediately
		if (used < num)
		{
			heap[used].err = st->err;
			*heap[used].slot = *st;

			if (++used == num)
				make_heap(heap, heap + used);

			return;
		}

		// we're full, need to check whether the new state is better
		// than our current worst
		RR_ASSERT(num > 0 && used == num);

		// if not an improvement over current worst, bail
		if (st->err >= heap[0].err)
			return;

		// new state replaces current worst
		heap[0].err = st->err;
		*heap[0].slot = *st;

		// now root may be in wrong place, fix heap
		adjust_heap(heap, 0, used, stdless<Result>());
	}

	void shrink(int new_num)
	{
		RR_ASSERT(new_num <= num);

		// not much to do if we're already below the threshold
		if (used <= new_num)
		{
			num = new_num;
			return;
		}

		// this is done after refinement steps which means the "err" values
		// in our heap are all out of date; update them!
		for (int i = 0; i < used; ++i)
			heap[i].err = heap[i].slot->err;

		// set up a heap of the target size and loop over existing items,
		// replacing whenever one beats the current worst
		make_heap(heap, heap + new_num);

		for (int i = new_num; i < used; ++i)
		{
			// if at least as bad as current worst, ignore
			if (heap[i].err >= heap[0].err)
				continue;

			// heap[i] beats current worst, keep it
			// swap so we don't "leak" slots
			swap(heap[0], heap[i]);

			// now root may be in wrong place, fix heap
			adjust_heap(heap, 0, new_num, stdless<Result>());
		}

		num = new_num;
		used = new_num;
	}

	bool has_mode(int mode)
	{
		for(int i = 0; i < used; ++i)
		{
			if(heap[i].slot->mode == mode)
				return true;
		}
		return false;
	}
};

// ---- index finding

struct BC6ColorSetLinearBits
{
	RAD_ALIGN(F32, dotdir[2][4], 16); // [subset][chan], chan 3 = bias
	RAD_ALIGN(S16, endpt[3][2][2], 16); // [chan][subset][lohi]

	template<typename Mode>
	void init(const BC6Endpoints &ep, const BC6ModeDesc &mode);
};

template<typename Mode>
void BC6ColorSetLinearBits::init(const BC6Endpoints &ep, const BC6ModeDesc &mode)
{
	const int ib_mask = (1 << Mode::ib) - 1;

	for (int s = 0; s < Mode::ns; ++s)
	{
		F32 mindot = 0.0f;
		F32 maxdot = 0.0f;

		for (int c = 0; c < 3; ++c)
		{
			int ep0 = bc6_dequant<Mode::is_signed>(mode.epb, ep.quant[s*2+0][c]);
			int ep1 = bc6_dequant<Mode::is_signed>(mode.epb, ep.quant[s*2+1][c]);

			ep0 = bc6_scaled_to_half<Mode::is_signed>(ep0);
			ep1 = bc6_scaled_to_half<Mode::is_signed>(ep1);

			endpt[c][s][0] = static_cast<S16>(ep0);
			endpt[c][s][1] = static_cast<S16>(ep1);

			dotdir[s][c] = F32(ep1 - ep0);
		#ifdef LOGSPACE_LUMA_WEIGHT
			static const float weight[3] = { 0.17697f, 0.81240f, 0.01063f };
			dotdir[s][c] *= weight[c];
		#endif
			mindot += ep0 * dotdir[s][c];
			maxdot += ep1 * dotdir[s][c];
		}

		F32 scalef = F32(ib_mask) / F32(maxdot - mindot);
		dotdir[s][0] *= scalef;
		dotdir[s][1] *= scalef;
		dotdir[s][2] *= scalef;
		dotdir[s][3] = -mindot * scalef;
	}
}

struct BC6ColorSetExactRel
{
	RAD_ALIGN(F32, col[2][3][16], 16); // [subset][chan][index]

	template<typename Mode>
	void init(const BC6Endpoints &ep, const BC6ModeDesc &mode, const F32 * channel_weights_sqrt);
};

#ifdef DO_BUILD_SSE4

// Half->float conversion, SSE2+
// input in 32-bit lanes
// positive values only
// no need to handle inf/nan here since we removed those earlier
// we do need to handle subnormals but those turn out to be easy the way we do it
static inline VecF32x4 pos_normal_F16_to_F32_4x(const Vec128 &h)
{
	const VecF32x4 magic_mult	= _mm_castsi128_ps(_mm_set1_epi32((254 - 15) << 23));

	Vec128 shifted		= _mm_slli_epi32(h, 13);
	VecF32x4 result		= _mm_mul_ps(_mm_castsi128_ps(shifted), magic_mult);

	return result;
}

#endif

#ifdef DO_BUILD_NEON64

// Half->float conversion, input in 32-bit lanes
// we have HW support for this so this is trivial
static inline VecF32x4 pos_normal_F16_to_F32_4x(const Vec128_S32 h)
{
	return vcvt_f32_f16(vreinterpret_f16_s16(vmovn_s32(h)));
}
#endif

template<typename Mode>
void BC6ColorSetExactRel::init(const BC6Endpoints &ep, const BC6ModeDesc &mode, const F32 * channel_weights_sqrt)
{
	const int num_lerp = 1 << Mode::ib;

	for (int s = 0; s < Mode::ns; ++s)
	{
		for (int c = 0; c < 3; ++c)
		{
			int ep0 = bc6_dequant<Mode::is_signed>(mode.epb, ep.quant[s*2+0][c]);
			int ep1 = bc6_dequant<Mode::is_signed>(mode.epb, ep.quant[s*2+1][c]);

#if defined(DO_BUILD_SSE4) || defined(DO_BUILD_NEON64)
			// NOTE: almost fits in 16-bit lanes but not quite:
			// in unsigned mode, we have a diff of two unsigned 16-bit values,
			// which takes 17 bits; in singed mode, diff of two signed 17-bit values,
			// which takes 18 bits.
			Vec128_S32 vbase(ep0 * 64 + 32);
			Vec128_S32 vdiff(ep1 - ep0); // yes, ep0-ep1
			VecF32x4 vweight(channel_weights_sqrt[c]);

			for (int i = 0; i < num_lerp; i += 4)
			{
				Vec128_S32 factors = Vec128_U8::loadu_lo32(&radtex_lerp_factor[Mode::ib][i]).to_s32_lo();
				Vec128_S32 scaled_a = vbase + vdiff * factors;

				// mask off the bits that would be shifed out during >>6
				Vec128_S32 masked_a = scaled_a & Vec128_S32(-64);
				Vec128_S32 abs_a = Mode::is_signed ? masked_a.abs() : masked_a;

				// abs(a) * 31 given (abs(a) << 6) is (abs(a) * 62) / 2 and the
				// (abs(a) * 62) part is just:
				Vec128_S32 abs_a_62 = abs_a - abs_a.srl<5>();
				Vec128_S32 half_float;

				if (Mode::is_signed)
				{
					// we need (abs(a) * 31) >> 5 = abs_a_62 >> 6
					half_float = abs_a_62.srl<6>();
				}
				else
				{
					// we need (abs(a) * 31) >> 6 = abs_a_62 >> 7
					half_float = abs_a_62.srl<7>();
				}

				VecF32x4 colors = pos_normal_F16_to_F32_4x(half_float) * vweight;
				if (Mode::is_signed) // put the sign back by grabbing the sign bit from scaled_a
					colors |= scaled_a.f32() & VecF32x4(-0.0f);

				colors.storeu(&col[s][c][i]);
			}
#else
			int base = ep0 * 64 + 32;
			int diff = ep1 - ep0;

			for (int i = 0; i < num_lerp; ++i)
			{
				// NOTE(fg): this is exact
				int a = (base + radtex_lerp_factor[Mode::ib][i] * diff) >> 6;
				rrFP16 fp16;

				if (Mode::is_signed)
				{
					int absa = (abs(a) * 31) >> 5;
					rrFP16_SetBits(&fp16, absa | (a < 0 ? 0x8000u : 0));
				}
				else
				{
					a = (a * 31) >> 6;
					rrFP16_SetBits(&fp16, a);
				}

				col[s][c][i] = rrFP16_GetFloat(&fp16) * channel_weights_sqrt[c];
			}
#endif
		}
	}
}

union BC6ColorSet
{
	BC6ColorSetExactRel exact_rel;
	BC6ColorSetLinearBits linear_bits;

	template<typename Mode>
	void init(const BC6Input &in, const BC6Endpoints &ep, const BC6ModeDesc &mode);
};

#if defined(DO_BUILD_SSE4) || defined(DO_BUILD_NEON64)
static RAD_ALIGN(const U32, g_identity_map[16], 16) =
{
	0, 1, 2, 3, 4, 5, 6, 7,
	8, 9, 10, 11, 12, 13, 14, 15
};
#endif

static BC6Error component_err(int diff)
{
	int x = abs(diff);
	x = RR_MIN(x, (1<<13)-1);
	return x*x; // SSD with clamped max err
}

#ifdef DO_BUILD_SSE4
static RADFORCEINLINE Vec128 component_err(const Vec128 &x_s32)
{
	Vec128 absx = _mm_sign_epi32(x_s32, x_s32);
	Vec128 clamped = _mm_min_epi32(absx, _mm_set1_epi32((1 << 13) - 1));

	// now 0 <= clamped < (1 << 13) in each 32-bit lane, so in particular,
	// the high halves of each 32-bit lane are zero, thus we can get the
	// 32-bit product of clamped with itself via PMADDWD
	return _mm_madd_epi16(clamped, clamped);
}
#endif

// Approximations in linear index finding:
// 1. Finding nearest color match using linearized dot product approximation
//    (adds about 0.1% error but is _massively_ faster)
// 2. Baking bc6_scaled_to_half into the endpoints before interpolation,
//    rather than interpolating then scaling.
template <typename Mode>
static BC6Error eval_indexes_linear_bits(const BC6Input &in, const BC6BlockState *st, const BC6ColorSetLinearBits &set)
{
	BC6Error tot_err = 0;

#ifdef DO_BUILD_SSE4
	RAD_ALIGN(S32, weight_pairs[16], 16);

	// Prepare weights
	Vec128 inds = load128u(st->idxs.ind);
	Vec128 weight_lut = load128u(radtex_lerp_factor[Mode::ib]);
	Vec128 weight1 = _mm_shuffle_epi8(weight_lut, inds);
	Vec128 weight0 = _mm_sub_epi8(_mm_set1_epi8(64), weight1);

	Vec128 interleaved0 = _mm_unpacklo_epi8(weight0, weight1);
	Vec128 interleaved1 = _mm_unpackhi_epi8(weight0, weight1);

	store128u(weight_pairs +  0, zext8to16_lo(interleaved0));
	store128u(weight_pairs +  4, zext8to16_hi(interleaved0));
	store128u(weight_pairs +  8, zext8to16_lo(interleaved1));
	store128u(weight_pairs + 12, zext8to16_hi(interleaved1));

	Vec128 endpt_r = load64u(set.endpt[0][0]);
	Vec128 endpt_g = load64u(set.endpt[1][0]);
	Vec128 endpt_b = load64u(set.endpt[2][0]);

	Vec128 subset_ind;
	if (Mode::ns == 1)
	{
		endpt_r = shuffle32<0,0,0,0>(endpt_r);
		endpt_g = shuffle32<0,0,0,0>(endpt_g);
		endpt_b = shuffle32<0,0,0,0>(endpt_b);
	}
	else
		subset_ind = _mm_set1_epi32(radtex_section_tbl[Mode::ns][st->p]);

	Vec128 sum_err = _mm_setzero_si128();
	for (int i = 0; i < 16; i += 4)
	{
		Vec128 end_r, end_g, end_b;

		if (Mode::ns == 1)
		{
			end_r = endpt_r;
			end_g = endpt_g;
			end_b = endpt_b;
		}
		else
		{
			const Vec128 subset_mask = _mm_setr_epi32(1, 4, 0x10, 0x40);
			Vec128 in_subset1 = _mm_cmpeq_epi32(_mm_and_si128(subset_ind, subset_mask), subset_mask);
			Vec128 shuffle = _mm_or_si128(_mm_and_si128(in_subset1, _mm_set1_epi8(4)), _mm_set1_epi32(0x03020100));

			end_r = _mm_shuffle_epi8(endpt_r, shuffle);
			end_g = _mm_shuffle_epi8(endpt_g, shuffle);
			end_b = _mm_shuffle_epi8(endpt_b, shuffle);
			subset_ind = _mm_srli_epi32(subset_ind, 8);
		}

		// Interpolate the colors
		// ((64-f)*ep0 + f*ep1 + 32) >> 6
		const Vec128 bias = _mm_set1_epi32(32);
		Vec128 weight_pair = load128u(weight_pairs + i);
		Vec128 colr = _mm_srai_epi32(_mm_add_epi32(_mm_madd_epi16(end_r, weight_pair), bias), 6);
		Vec128 colg = _mm_srai_epi32(_mm_add_epi32(_mm_madd_epi16(end_g, weight_pair), bias), 6);
		Vec128 colb = _mm_srai_epi32(_mm_add_epi32(_mm_madd_epi16(end_b, weight_pair), bias), 6);

		// Error calc
		sum_err = _mm_add_epi32(sum_err, component_err(_mm_sub_epi32(colr, load128u(&in.blocki[0][i]))));
		sum_err = _mm_add_epi32(sum_err, component_err(_mm_sub_epi32(colg, load128u(&in.blocki[1][i]))));
		sum_err = _mm_add_epi32(sum_err, component_err(_mm_sub_epi32(colb, load128u(&in.blocki[2][i]))));
	}

	tot_err = reduce_add_s32(sum_err);
#else
	U32 im = radtex_section_tbl[Mode::ns][st->p];
	for (int i = 0; i < 16; ++i)
	{
		int s = im & 3;
		im >>= 2;

		S32 w1 = radtex_lerp_factor[Mode::ib][st->idxs.ind[i]];
		S32 w0 = 64 - w1;

		S32 colr = (set.endpt[0][s][0] * w0 + set.endpt[0][s][1] * w1 + 32) >> 6;
		S32 colg = (set.endpt[1][s][0] * w0 + set.endpt[1][s][1] * w1 + 32) >> 6;
		S32 colb = (set.endpt[2][s][0] * w0 + set.endpt[2][s][1] * w1 + 32) >> 6;

		tot_err += component_err(in.blocki[0][i] - colr);
		tot_err += component_err(in.blocki[1][i] - colg);
		tot_err += component_err(in.blocki[2][i] - colb);
	}
#endif

	return tot_err;
}

#if defined(DO_BUILD_SSE4) || defined(DO_BUILD_NEON64)

static RADFORCEINLINE void transpose_4x4(VecF32x4 &x0, VecF32x4 &x1, VecF32x4 &x2, VecF32x4 &x3)
{
	// Transpose pass 1
	VecF32x4 t0 = x0.unpack_lo(x2);
	VecF32x4 t1 = x1.unpack_lo(x3);
	VecF32x4 t2 = x0.unpack_hi(x2);
	VecF32x4 t3 = x1.unpack_hi(x3);

	// Transpose pass 2
	x0 = t0.unpack_lo(t1);
	x1 = t0.unpack_hi(t1);
	x2 = t2.unpack_lo(t3);
	x3 = t2.unpack_hi(t3);
}

#endif

template <typename Mode>
static BC6Error calc_indexes_linear_bits(const BC6Input &in, BC6BlockState *st, const BC6ColorSetLinearBits &set)
{
	const int ib_mask = (1 << Mode::ib) - 1;

#ifdef DO_BUILD_SSE4
	VecF32x4 clamp_max = VecF32x4(F32(ib_mask));
	if (Mode::ns == 1)
	{
		VecF32x4 dir = VecF32x4::loada(set.dotdir[0]);
		for (int i = 0; i < 16; i += 4)
		{
			VecF32x4 dot0 = VecF32x4::loada(&in.blockf_lsq[i*4 +  0]) * dir;
			VecF32x4 dot1 = VecF32x4::loada(&in.blockf_lsq[i*4 +  4]) * dir;
			VecF32x4 dot2 = VecF32x4::loada(&in.blockf_lsq[i*4 +  8]) * dir;
			VecF32x4 dot3 = VecF32x4::loada(&in.blockf_lsq[i*4 + 12]) * dir;

			transpose_4x4(dot0, dot1, dot2, dot3);

			// Summation
			VecF32x4 sum = dot0 + dot1 + dot2 + dot3;

			// Clamp
			VecF32x4 clamped = vmin(vmax(sum, VecF32x4::zero()), clamp_max);

			// Convert the quantized index to int and store it
			Vec128 q32 = clamped.to_int32_round(); // NOTE rounding not truncating! important.
			store32u(st->idxs.ind + i, _mm_shuffle_epi8(q32, _mm_setr_epi8(0,4,8,12, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1)));
		}
	}
	else
	{
		U32 im = radtex_section_tbl[Mode::ns][st->p];
		for (int i = 0; i < 16; i += 4)
		{
			VecF32x4 dir0 = VecF32x4::loada(set.dotdir[im & 3]);
			VecF32x4 dir1 = VecF32x4::loada(set.dotdir[(im >> 2) & 3]);
			VecF32x4 dir2 = VecF32x4::loada(set.dotdir[(im >> 4) & 3]);
			VecF32x4 dir3 = VecF32x4::loada(set.dotdir[(im >> 6) & 3]);
			im >>= 8;

			VecF32x4 dot0 = VecF32x4::loada(&in.blockf_lsq[i*4 +  0]) * dir0;
			VecF32x4 dot1 = VecF32x4::loada(&in.blockf_lsq[i*4 +  4]) * dir1;
			VecF32x4 dot2 = VecF32x4::loada(&in.blockf_lsq[i*4 +  8]) * dir2;
			VecF32x4 dot3 = VecF32x4::loada(&in.blockf_lsq[i*4 + 12]) * dir3;

			transpose_4x4(dot0, dot1, dot2, dot3);

			// Summation
			VecF32x4 sum = dot0 + dot1 + dot2 + dot3;

			// Clamp
			VecF32x4 clamped = vmin(vmax(sum, VecF32x4::zero()), clamp_max);

			// Convert the quantized index to int and store it
			Vec128 q32 = clamped.to_int32_round(); // NOTE rounding not truncating! important.
			store32u(st->idxs.ind + i, _mm_shuffle_epi8(q32, _mm_setr_epi8(0,4,8,12, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1)));
		}
	}
#else
	U32 im = radtex_section_tbl[Mode::ns][st->p];
	for (int i = 0; i < 16; ++i)
	{
		int s = im & 3;
		im >>= 2;

		F32 dot;
		dot  = in.blockf_lsq[i*4+0] * set.dotdir[s][0];
		dot += in.blockf_lsq[i*4+1] * set.dotdir[s][1];
		dot += in.blockf_lsq[i*4+2] * set.dotdir[s][2];
		dot += set.dotdir[s][3];

		int q = static_cast<int>(dot + 0.5f);
		q = radtex_clamp(q, 0, ib_mask);
		st->idxs.ind[i] = static_cast<U8>(q);
	}
#endif

	return eval_indexes_linear_bits<Mode>(in, st, set);
}

template <typename Mode>
static BC6Error calc_indexes_exact_rel(const BC6Input &in, BC6BlockState *st, const BC6ColorSetExactRel &set)
{
	const int num_lerp = 1 << Mode::ib;

	U32 im = radtex_section_tbl[Mode::ns][st->p];
	F32 float_err = 0.0f;

#if defined(DO_BUILD_SSE4) || defined(DO_BUILD_NEON64)
	VecF32x4 best_arr[16];
	for (int i = 0; i < 16; ++i)
	{
		VecF32x4 pixr(in.blockf32[0][i]);
		VecF32x4 pixg(in.blockf32[1][i]);
		VecF32x4 pixb(in.blockf32[2][i]);
		const F32 * colbase = set.col[0][0];
		if (Mode::ns > 1)
		{
			colbase = set.col[im & 1][0];
			im >>= 2;
		}

		VecF32x4 best = VecF32x4::bitmask(255 << 23); // +inf (with index 0)
		VecF32x4 and_mask = VecF32x4::bitmask(0u - 16); // mask off low 4 bits
		for (int j = 0; j < num_lerp; j += 4)
		{
			// Load this batch of interpolated colors
			VecF32x4 colr = VecF32x4::loada(colbase + 0*16 + j);
			VecF32x4 colg = VecF32x4::loada(colbase + 1*16 + j);
			VecF32x4 colb = VecF32x4::loada(colbase + 2*16 + j);

			// Calculate the errors
			VecF32x4 e;
			e = (colr - pixr).square() + (colg - pixg).square() + (colb - pixb).square();

			// put the index into the low mantissa bits of the float
			e = (e & and_mask) | VecF32x4::loada((const float *) (g_identity_map + j));

			best = vmin(best, e);
		}

		best_arr[i] = best;
	}

	VecF32x4 sum_err = VecF32x4::zero();
	for (int i = 0; i < 16; i += 4)
	{
		// Complete min reduction: need to transpose first
		VecF32x4 in0 = best_arr[i + 0];
		VecF32x4 in1 = best_arr[i + 1];
		VecF32x4 in2 = best_arr[i + 2];
		VecF32x4 in3 = best_arr[i + 3];

		transpose_4x4(in0, in1, in2, in3);

		// Min reduction
		VecF32x4 best01 = vmin(in0, in1);
		VecF32x4 best23 = vmin(in2, in3);
		VecF32x4 best = vmin(best01, best23);

		// Emit best indices
		Vec128_U32 inds = best.u32() & Vec128_U32(0xf);
		inds.narrow32to8_mod().storeu_lo32(st->idxs.ind + i);

		// Weight and sum errors
		sum_err += best * VecF32x4::loada(in.weightf32 + i);
	}

	// Final error reduction
	float_err = sum_err.sum_across().scalar_x();
#else
	F32 err_sums[4] = { };

	for (int i = 0; i < 16; ++i)
	{
		int s = im & 3;
		im >>= 2;

		// index eval can just use straight SSD
		F32 best;
		best  = radtex_sqr(in.blockf32[0][i] - set.col[s][0][0]);
		best += radtex_sqr(in.blockf32[1][i] - set.col[s][1][0]);
		best += radtex_sqr(in.blockf32[2][i] - set.col[s][2][0]);
		best = bits2float(float2bits(best) & ~0xf); // put index in low mantissa bits
		
		for (int j = 1; j < num_lerp; ++j)
		{
			F32 e;
			e  = radtex_sqr(in.blockf32[0][i] - set.col[s][0][j]);
			e += radtex_sqr(in.blockf32[1][i] - set.col[s][1][j]);
			e += radtex_sqr(in.blockf32[2][i] - set.col[s][2][j]);
			e = bits2float((float2bits(e) & ~0xf) | j); // put index in low mantissa bits
			best = (e < best) ? e : best;
		}

		// error accum needs to perform weighting
		err_sums[i & 3] += best * in.weightf32[i];
		st->idxs.ind[i] = static_cast<U8>(float2bits(best) & 15);
	}

	// sum in same order as SIMD version
	float_err = (err_sums[0] + err_sums[2]) + (err_sums[1] + err_sums[3]);
#endif

	return (BC6Error) (S32)(float_err * 65536.0f);
}

template<typename Mode>
static BC6Error eval_indexes_exact_rel(const BC6Input &in, const BC6BlockState *st, const BC6ColorSetExactRel &set)
{
	U32 im = radtex_section_tbl[Mode::ns][st->p];
	F32 float_err = 0.0f;

	for (int i = 0; i < 16; ++i)
	{
		int s = im & 3;
		im >>= 2;

		UINTa ind = st->idxs.ind[i];
		F32 e;
		e  = radtex_sqr(in.blockf32[0][i] - set.col[s][0][ind]);
		e += radtex_sqr(in.blockf32[1][i] - set.col[s][1][ind]);
		e += radtex_sqr(in.blockf32[2][i] - set.col[s][2][ind]);
		float_err += e * in.weightf32[i];
	}

	return (BC6Error) (S32)(float_err * 65536.0f);
}

template<typename Mode>
void BC6ColorSet::init(const BC6Input &in, const BC6Endpoints &ep, const BC6ModeDesc &mode)
{
	switch (in.active_metric)
	{
	case BC6METRIC_LOGSPACE:	linear_bits.init<Mode>(ep, mode); break;
	case BC6METRIC_MRSSE:		exact_rel.init<Mode>(ep, mode, in.channel_weights_sqrt); break;
	}
}

template<typename Mode>
static BC6Error calc_indexes(const BC6Input &in, BC6BlockState *st, const BC6ColorSet &set)
{
	switch (in.active_metric)
	{
	case BC6METRIC_LOGSPACE:	return calc_indexes_linear_bits<typename Mode::index_mode>(in, st, set.linear_bits);
	case BC6METRIC_MRSSE:		return calc_indexes_exact_rel<typename Mode::index_mode>(in, st, set.exact_rel);
	}

	RR_BREAK();
	return 0;
}

template<typename Mode>
static BC6Error eval_indexes(const BC6Input &in, const BC6BlockState *st, const BC6ColorSet &set)
{
	switch (in.active_metric)
	{
	case BC6METRIC_LOGSPACE:	return eval_indexes_linear_bits<typename Mode::index_mode>(in, st, set.linear_bits);
	case BC6METRIC_MRSSE:		return eval_indexes_exact_rel<typename Mode::index_mode>(in, st, set.exact_rel);
	}

	RR_BREAK();
	return 0;
}

template<typename Mode>
static bool calc_endpoints(BC6Endpoints *ep, const BC6ModeDesc &desc)
{
	bool any_clamps = false;

	// Quantize the endpoints
	ep->quantize<Mode>(desc);

	if(desc.db[0] != 0)
	{
		for (int c = 0; c < 3; ++c)
		{
			int base = ep->quant[0][c];
			const int db = desc.db[c];
			int sign_bit = 1 << (db-1);

			// clamp endpoints_q to a range that can be represented via in-range deltas,
			// but don't actually delta them until we encode, since it's more convenient
			// for us to have them raw

			for (int i = 1; i < Mode::ns*2; ++i)
			{
				int clamped = radtex_clamp(ep->quant[i][c], base - sign_bit, base + sign_bit - 1);
				if (clamped != ep->quant[i][c])
				{
					any_clamps = true;
					ep->quant[i][c] = clamped;
				}
			}
		}
	}

	return any_clamps;
}

template<typename Mode>
static bool trivial_flip_possible(const BC6Endpoints &ep, const BC6ModeDesc &mode)
{
	RR_ASSERT(mode.db[0] != 0);

	for (int c = 0; c < 3; ++c)
	{
		int base = ep.quant[1][c]; // base after flip will be endpoint 1 not 0!
		const int db = mode.db[c];
		int sign_bit = 1 << (db-1);

		// we need to iterate over indices 0, 2, and 3, which
		// is easier to do if we go in the order 0, 3, 2.
		for (int j = 1; j < Mode::ns*2; ++j)
		{
			int i = j ^ 1;
			if (ep.quant[i][c] < base - sign_bit || ep.quant[i][c] >= base + sign_bit)
				return false;
		}
	}

	return true;
}

template<int which>
static void flip_subset_inds(BC6Inds *inds, U32 subset_mask, U8 flip_bits)
{
	for (int i = 0; i < 16; ++i)
	{
		U32 s = subset_mask & 3;
		if (s == which)
			inds->ind[i] ^= flip_bits;
		subset_mask >>= 2;
	}
}

template<typename Mode>
static void fix_delta_endpoint01(const BC6Input &in, BC6BlockState *st, BC6ColorSet *set, const BC6ModeDesc &mode, bool any_clamps)
{
	const U8 anchor_bit = 1 << (Mode::ib - 1);
	if (mode.db[0] == 0 || (st->idxs.ind[0] & anchor_bit) == 0)
		return;

	// With delta-ing, we have a problem: namely, swapping the endpoints (like we
	// would ordinarily do to fix the anchor bit issue) may make our existing
	// encoding invalid because the new deltas are out of range.
	//
	// But before we do anything drastic, check whether we have any large deltas
	// to begin with, because if we don't this is all straightforward.
	// Also, if any of our original deltas were clamped to fit in range, that
	// makes them somewhat suspect and we probably want to try it both ways
	// regardless.
	if (!any_clamps && trivial_flip_possible<Mode>(st->endpoints, mode))
	{
		flip_subset_inds<0>(&st->idxs, radtex_section_tbl[Mode::ns][st->p], (1 << Mode::ib) - 1);

		for (int c = 0; c < 3; ++c)
		{
			radtex_swap(st->endpoints.raw[0][c], st->endpoints.raw[1][c]);
			radtex_swap(st->endpoints.quant[0][c], st->endpoints.quant[1][c]);
		}

		return;
	}

	// Ok, we _do_ have large deltas. This is a bit of a production.
	// First, as a baseline, calculate the error we would get if we just
	// forced the anchor bit to 0.
	st->idxs.ind[0] = anchor_bit - 1; // largest index value with anchor0_bit not set -> smallest error
	st->err = eval_indexes<Mode>(in, st, *set); // NOTE we only ever change pixel 0, this could be done more cheaply.

	// Then, consider an alternative state where we actually swap the endpoints
	// (and thus change the origin for deltas)
	BC6BlockState nst = *st;
	for (int c = 0; c < 3; ++c)
		radtex_swap(nst.endpoints.raw[0][c], nst.endpoints.raw[1][c]);

	// Swapping these endpoints means we need to recalculate the quantized and range-limited
	// endpoints, which are now relative to a different base.
	calc_endpoints<Mode>(&nst.endpoints, mode);
	set->init<Mode>(in, nst.endpoints, mode);

	// This in turn invalidates the indices, so recalculate those too
	nst.err = calc_indexes<Mode>(in, &nst, *set);

	// And we can _still_ end up with a situation where the new encoding has that anchor bit
	// set, so we again need to try our fallback plan if that happens.
	// The reason this can happen is because the delta range isn't symmetric. Deltas are two's
	// complement, so we have more negative range than positive range; if any of the delta values
	// in either st or nst are the minimum value, that means we did not just swap the endpoints,
	// but rather got a different set.
	if (nst.idxs.ind[0] & anchor_bit)
	{
		nst.idxs.ind[0] = anchor_bit - 1;
		nst.err = eval_indexes<Mode>(in, &nst, *set); // NOTE we only ever change pixel 0, this could be done more cheaply.
	}

	// Finally, figure out which of the two variants (quick patch-up job
	// on the original indices or redoing with swapped endpoints) was
	// actually better.
	if (nst.err < st->err)
		*st = nst;
}

// index calculation under the constraint that anchor bits have to stay clear
template<typename Mode>
static BC6Error calc_indexes_anchor_zero(const BC6Input &in, BC6BlockState *st, const BC6ColorSet &set)
{
	BC6Error err = calc_indexes<Mode>(in, st, set);

	const U8 anchor_bit = 1 << (Mode::ib - 1);
	SINTa anchor2_pos = (Mode::ns == 1) ? 0 : radtex_anchor_2_2[st->p];

	// Is either anchor bit set?
	if ((st->idxs.ind[0] | st->idxs.ind[anchor2_pos]) & anchor_bit)
	{
		// best we can do is clamp both to acceptable range
		if (st->idxs.ind[0] & anchor_bit)
			st->idxs.ind[0] = anchor_bit - 1;

		if (st->idxs.ind[anchor2_pos] & anchor_bit)
			st->idxs.ind[anchor2_pos] = anchor_bit - 1;

		err = eval_indexes<Mode>(in, st, set);
	}

	return err;
}

typedef void BC6TrialFunc(const BC6Input &in, BC6BlockState *st, const BC6ModeDesc &mode);

template<typename Mode>
static void bc6enc_trial(const BC6Input &in, BC6BlockState *st, const BC6ModeDesc &mode)
{
	bool any_clamps = calc_endpoints<Mode>(&st->endpoints, mode);

	BC6ColorSet set;
	set.init<Mode>(in, st->endpoints, mode);

	st->err = calc_indexes<Mode>(in, st, set);
	fix_delta_endpoint01<Mode>(in, st, &set, mode, any_clamps);
}

typedef bool BC6TryAndCommitFunc(const BC6Input &in, BC6BlockState *cur_best, BC6BlockState *st, const BC6ModeDesc &mode);

template<typename Mode>
static bool bc6enc_try_and_commit(const BC6Input &in, BC6BlockState *cur_best, BC6BlockState *st, const BC6ModeDesc &mode)
{
	bc6enc_trial<Mode>(in, st, mode);
	if (st->err >= cur_best->err)
		return false;

	*cur_best = *st;
	return true;
}

// ---- Refinement helpers

// Linearly transforms the endpoints corresponding to a remapping of indices
// by adjust[0] quantization steps (in index space) for endpoint 0,
// and by adjust[1] quantization steps for endpoint 1.
static void linear_transform_endpoints(BC6Endpoints *transformed, const BC6Endpoints *initial, const float adjust[2], int min_v, int ib, int subset_base)
{
	const float old_minf = 0.0f;
	const float old_maxf = 1.0f;
	const float wiggle = radtex_lerp_factor_4x[ib][1][3];

	// Compute what to remap where
	const float new_minf = old_minf + wiggle * adjust[0];
	const float new_maxf = old_maxf + wiggle * adjust[1];

	// To map [new_minf,new_maxf] to [old_minf,old_maxf], first translate by
	// -new_minf, scale by (old_maxf - old_minf) / (new_maxf - new_minf),
	// then translate by old_minf.
	//
	// t0 and t1 are the images of 0 and 1 under that map.
	const float sc = (old_maxf - old_minf) / (new_maxf - new_minf);
	const float t0 = old_minf + (0.0f - new_minf) * sc;
	const float t1 = old_minf + (1.0f - new_minf) * sc;

	for (int c = 0; c < 3; ++c)
	{
		// endpoint parameteriztion: base + t*dir
		float base = static_cast<float>(initial->raw[subset_base + 0][c]);
		float dir = static_cast<float>(initial->raw[subset_base + 1][c]) - base;

		transformed->raw[subset_base + 0][c] = round_endpoint(base + t0 * dir, min_v);
		transformed->raw[subset_base + 1][c] = round_endpoint(base + t1 * dir, min_v);
	}
}

template<typename Mode>
static void try_linear_transforms(const BC6Input &in, BC6BlockState *st, const BC6Endpoints &initial_vals, const float adjust[][2], int num_adjust, const BC6ModeDesc &mode)
{
	const int min_v = Mode::is_signed ? -0x7bff : 0;
	BC6Endpoints initial = initial_vals;

	BC6BlockState nst = *st;
	for (int i = 0; i < num_adjust; ++i)
	{
		// Try the subsets separately for now
		for (int base = 0; base < Mode::ns*2; base += 2)
		{
			linear_transform_endpoints(&nst.endpoints, &initial, adjust[i], min_v, Mode::ib, base);
			bc6enc_try_and_commit<Mode>(in, st, &nst, mode);
		}
	}
}

// ---- refine

template<int t_ns, int t_ib>
static void bc6enc_refine_lsq_endpoints_core(const BC6Input &in, BC6BlockState *st, bool is_signed)
{
	const int comp = 3;

	// NOTE(fg): This is a really bizarre least-squares.
	//
	// Namely, blockf_lsq here are the _integer bit patterns_ of the input F16s, converted (as integers)
	// to F32. (With some fudging to make F16s corresponding to negative values actually show up as
	// negative values here and preserve the correct ordering.)
	//
	// The point being that that's the space the actual interpolation takes place in.
	//
	// I tried this earlier and it was worse than what was previously in here (which looked reasonable but
	// was fundamentally flawed in several ways upon closer inspection), but now it seems to be solidly
	// better. I guess my initial implementation was screwed up somehow?
	//
	// Either way, the underlying error metric here is L2 error on the semilog-scale F16 values, which
	// is a bit "eccentric". We already know that's not ideal. I want to experiment with weighted least
	// squares next, to put some of the relative weighting we have in the companding function back in.
	//
	// There's still a flawed assumption in here in that we use the same least-squares system for all three
	// channels, but really we probably want a weight per channel which would mean solving three LLS systems
	// instead of one. Let's see how regular weighting fares first before I try that.

	const float * factors = radtex_lerp_factor_4x[t_ib][0];

	int clampv = is_signed ? -0x7bff : 0;

	float z00[2] = { }, z10[2] = { }, z11[2] = { };
	float q00[2][3] = { }, t[2][3] = { };
	const F32 *blockf = in.blockf_lsq;
	U32 im = radtex_section_tbl[t_ns][st->p];
	for (int i = 0; i < 16; ++i)
	{
		int s = im & 3;
		im >>= 2;
		SINTa base = st->idxs.ind[i] * 4;
		F32 w = factors[base + 3];
		z00[s] += factors[base + 0];
		z10[s] += factors[base + 1];
		z11[s] += factors[base + 2];
		for(int c = 0; c < comp; ++c)
		{
			F32 value = blockf[i*4+c];
			q00[s][c] += w * value;
			t[s][c] += value;
		}
	}

	for(int s = 0; s < t_ns; ++s)
	{
		float det = z00[s] * z11[s] - z10[s] * z10[s];
		if (!det) continue;
		det = 1.0f/det;

		float iz00 = z11[s] * det;
		float iz10 = -z10[s] * det;
		float iz11 = z00[s] * det;

		for(int c = 0; c < comp; ++c)
		{
			float q0 = q00[s][c];
			float q1 = t[s][c] - q0;
			st->endpoints.raw[s*2+0][c] = round_endpoint(iz10 * q0 + iz11 * q1, clampv);
			st->endpoints.raw[s*2+1][c] = round_endpoint(iz00 * q0 + iz10 * q1, clampv);
		}
	}
}

template<typename Mode>
static void bc6enc_refine_lsq_endpoints(const BC6Input &in, BC6BlockState *st)
{
	bc6enc_refine_lsq_endpoints_core<Mode::ns, Mode::ib>(in, st, Mode::is_signed);
}

template<int t_ns, int t_ib>
static void bc6enc_refine_weighted_lsq_endpoints_core(const BC6Input &in, BC6BlockState *st, bool is_signed)
{
	const int comp = 3;

	const float * factors = radtex_lerp_factor_4x[t_ib][0];

	int clampv = is_signed ? -0x7bff : 0;

	float z00[2] = { }, z10[2] = { }, z11[2] = { };
	float q00[2][3] = { }, t[2][3] = { };
	const F32 *blockf = in.blockf_lsq;
	U32 im = radtex_section_tbl[t_ns][st->p];
	for (int i = 0; i < 16; ++i)
	{
		int s = im & 3;
		im >>= 2;
		SINTa base = st->idxs.ind[i] * 4;
		F32 w = factors[base + 3];
		F32 weight = blockf[i*4 + 3];
		z00[s] += weight * factors[base + 0];
		z10[s] += weight * factors[base + 1];
		z11[s] += weight * factors[base + 2];
		for(int c = 0; c < comp; ++c)
		{
			F32 weighted = weight * blockf[i*4+c];
			q00[s][c] += w * weighted;
			t[s][c] += weighted;
		}
	}

	for(int s = 0; s < t_ns; ++s)
	{
		float det = z00[s] * z11[s] - z10[s] * z10[s];
		if (!det) continue;
		det = 1.0f/det;

		float iz00 = z11[s] * det;
		float iz10 = -z10[s] * det;
		float iz11 = z00[s] * det;

		for(int c = 0; c < comp; ++c)
		{
			float q0 = q00[s][c];
			float q1 = t[s][c] - q0;
			st->endpoints.raw[s*2+0][c] = round_endpoint(iz10 * q0 + iz11 * q1, clampv);
			st->endpoints.raw[s*2+1][c] = round_endpoint(iz00 * q0 + iz10 * q1, clampv);
		}
	}
}

template<typename Mode>
static void bc6enc_refine_weighted_lsq_endpoints(const BC6Input &in, BC6BlockState *st)
{
	bc6enc_refine_weighted_lsq_endpoints_core<Mode::ns, Mode::ib>(in, st, Mode::is_signed);
}

typedef bool BC6RefineLsqAndTryFunc(const BC6Input &in, BC6BlockState *st, BC6BlockState *nst, const BC6ModeDesc &mode);

template<typename Mode>
static bool bc6enc_refine_lsq_endpoints_and_try(const BC6Input &in, BC6BlockState *st, BC6BlockState *nst, const BC6ModeDesc &mode)
{
	bc6enc_refine_lsq_endpoints<Mode>(in, nst);
	return bc6enc_try_and_commit<Mode>(in, st, nst, mode);
}

static void bc6enc_refine_lsq_endpoints_iterated_core(const BC6Input &in, BC6BlockState *st, int lsq_iters, BC6RefineLsqAndTryFunc *refine_and_try, const BC6ModeDesc &mode)
{
	BC6BlockState nst = *st;
	for (int lsq_iter = 0; lsq_iter < lsq_iters; ++lsq_iter)
	{
		if (!refine_and_try(in, st, &nst, mode))
			break;
	}
}

template<typename Mode>
static void bc6enc_refine_lsq_endpoints_iterated(const BC6Input &in, BC6BlockState *st, int lsq_iters, const BC6ModeDesc &mode)
{
	bc6enc_refine_lsq_endpoints_iterated_core(in, st, lsq_iters, bc6enc_refine_lsq_endpoints_and_try<Mode>, mode);
}

template<int is_signed>
static void bc6enc_refine_lsq_endpoints_pass(const BC6Input &in, BC6BlockState *st, int lsq_iters)
{
	const BC6ModeDesc &mode = bc6_mode_desc[st->mode];

	if (mode.is_two_subset)
		bc6enc_refine_lsq_endpoints_iterated<BC6Mode<2, is_signed> >(in, st, lsq_iters, mode);
	else
		bc6enc_refine_lsq_endpoints_iterated<BC6Mode<1, is_signed> >(in, st, lsq_iters, mode);
}

template<typename Mode>
static void bc6enc_refine_jitter_selectors(const BC6Input &in, BC6BlockState *st, int iters, const BC6ModeDesc &mode)
{
	const U64 ib_mask = (1 << Mode::ib) - 1;
	rrRandState rand_state = in.rand_seed;

	for (int iter = 0; iter < iters; ++iter)
	{
		BC6BlockState nst = *st;
		int ni = (rrRandState32(&rand_state) & 3)+1;

		for (int ii = 0; ii < ni; ++ii)
		{
			unsigned r = rrRandState32(&rand_state);
			int i = r & 15;
			int j = nst.idxs.ind[i];
			int push = ((U32) ( ((U64)r * 5) >> 32 )) - 2;
			nst.idxs.ind[i] = static_cast<U8>(radtex_clamp<int>(j + push, 0, ib_mask));
		}
		bc6enc_refine_lsq_endpoints_and_try<Mode>(in, st, &nst, mode);
	}
}

static void bc6enc_refine_anneal_endpoints_core(const BC6Input &in, BC6BlockState *st, int iters, int ns, bool is_signed, const BC6ModeDesc &mode, BC6TrialFunc *trial)
{
	if (!st->err || !iters)
		return;

	int clampv = is_signed ? -0x7bff : 0;
	int cb_shift = radtex_max(0, 14 + is_signed - mode.epb);
	BC6BlockState current = *st;
	static const int dirs[][2] = {
		{-2, -2}, {-1, -2}, {0, -2}, {1, -2}, {2, -2},
		{-2, -1}, {-1, -1}, {0, -1}, {1, -1}, {2, -1},
		{-2,  0}, {-1,  0},          {1,  0}, {2,  0},
		{-2,  1}, {-1,  1}, {0,  1}, {1,  1}, {2,  1},
		{-2,  2}, {-1,  2}, {0,  2}, {1,  2}, {2,  2},
	};
	rrRandState rand_state = in.rand_seed;

	// Super-basic exponential cooling schedule
	float temperature_flt = 1024.0f; // NOTE: this should almost certainly start higher, need to look at a bunch of test images
	float decay = (float)rr_exp_approx(-(rr_loge(temperature_flt) + 0.5) / (RR_MAX(iters, 2) - 1.0));

	for (int iter = 0; iter < iters; ++iter)
	{
		int temperature = (int)temperature_flt;
		temperature_flt *= decay;

		for (int s = 0; s < ns; ++s)
		{
			BC6BlockState part = current;
			int which = rrRandStateMod(&rand_state, 7) + 1;
			do
			{
				// Iterate over set bits
				int c = rrCtz32(which);
				which &= which - 1;

				int rnd = rrRandStateMod(&rand_state, RR_ARRAY_SIZE(dirs));
				part.endpoints.raw[s*2+0][c] = static_cast<S16>(radtex_clamp<int>(part.endpoints.raw[s*2+0][c] + (dirs[rnd][0] << cb_shift), clampv, 0x7bff));
				part.endpoints.raw[s*2+1][c] = static_cast<S16>(radtex_clamp<int>(part.endpoints.raw[s*2+1][c] + (dirs[rnd][1] << cb_shift), clampv, 0x7bff));
			}
			while (which);

			trial(in, &part, mode);

			if (part.err < current.err || (temperature > 0 && rrRandStateMod(&rand_state, temperature) >= current.err - part.err))
			{
				current = part;
				if (part.err < st->err)
				{
					*st = part;
					if (!st->err)
						return;
				}
			}
		}
	}
}

template<typename Mode>
static void bc6enc_refine_anneal_endpoints(const BC6Input &in, BC6BlockState *st, int iters, const BC6ModeDesc &mode)
{
	bc6enc_refine_anneal_endpoints_core(in, st, iters, Mode::ns, Mode::is_signed != 0, mode, bc6enc_trial<Mode>);
}

static void bc6enc_refine_pca_core(const BC6Input &in, BC6BlockState *st, BC6Flags flags, const BC6EncOptions *opt, int s, U64 inds, int num_pixels, BC6TryAndCommitFunc *try_and_commit, bool is_signed, const BC6ModeDesc &mode, const BC6PartitionPrep * part_prep)
{
	int clampv = is_signed ? -0x7bff : 0;

	const int comp = 3;
	float sum_sqr = 0.0f;
	float min_dot = FLT_MAX;
	float max_dot = FLT_MIN;
	const float *mean = part_prep->mean[s];
	const float *pca = part_prep->pca[s];
	const F32 *blockf = in.blockf_lsq;

	for (int j = 0; j < num_pixels; ++j)
	{
		int i = static_cast<int>(inds & 0xf);
		inds >>= 4;
		float dot = 0;
		for(int c = 0; c < comp; ++c)
			dot += (blockf[i*4+c] - mean[c]) * pca[c];
		sum_sqr += dot * dot;
		min_dot = radtex_min(min_dot, dot);
		max_dot = radtex_max(max_dot, dot);
	}

	// NOTE(fg): the idea about the shrink here is that min_dot and max_dot are at extremes,
	// and if we put the endpoints spot-on, we reduce the expected quantization error for those
	// pixels at the expense of everyone else. Instead, shrink the variation slightly which gives
	// us slightly better resolution inside and puts the extremal pixels closer to the edge of
	// their quantization bucket, rather than smack in the middle.
	float shrink = (max_dot - min_dot) * (1.0f / 64.0f);
	float smin_dot = min_dot + shrink;
	float smax_dot = max_dot - shrink;

	BC6BlockState nst = *st;
	for (int c = 0; c < comp; ++c)
	{
		nst.endpoints.raw[s*2+0][c] = round_endpoint(mean[c] + pca[c] * smin_dot, clampv);
		nst.endpoints.raw[s*2+1][c] = round_endpoint(mean[c] + pca[c] * smax_dot, clampv);
	}
	try_and_commit(in, st, &nst, mode);

	if (flags & BC6ENC_PCA_STRETCH)
	{
		float variance = sum_sqr / num_pixels;
		float sdev = sqrtf(variance)*2;
		const int num_passes = opt->max_pca_stretch_passes;

		float base = 0.75f * sdev;
		float step = 0.5f / (num_passes - 1.0f) * sdev;

		for (int pass = 0; pass < num_passes; pass++)
		{
			float spread = base + (float)pass * step;

			for (int c = 0; c < comp; ++c)
			{
				float delta = spread * pca[c];
				nst.endpoints.raw[s*2+0][c] = round_endpoint(mean[c] - delta, clampv);
				nst.endpoints.raw[s*2+1][c] = round_endpoint(mean[c] + delta, clampv);
			}

			try_and_commit(in, st, &nst, mode);
		}
	}
}

template<typename Mode>
static void bc6enc_refine_pca(const BC6Input &in, BC6BlockState *st, BC6Flags flags, const BC6EncOptions *opt, const BC6ModeDesc &mode, const BC6PartitionPrep *part_prep)
{
	U64 inds = radtex_subset_to_inds[Mode::ns-1][st->p];

	for (int s = 0; s < Mode::ns; ++s)
	{
		// Project pixels on the principal axis, take min and max projection as endpoints.
		const int num_pixels = radtex_num_pixels_per_subset[Mode::ns][st->p][s];
		bc6enc_refine_pca_core(in, st, flags, opt, s, inds, num_pixels, bc6enc_try_and_commit<Mode>, Mode::is_signed != 0, mode, part_prep);
		inds >>= num_pixels * 4;
	}
}

template<typename Mode>
static void bc6enc_refine_extrapolate_selectors(const BC6Input &in, BC6BlockState *st, const BC6ModeDesc &mode)
{
	static const float adjust[3][2] =
	{
		{ 1.0f,  0.0f },
		{ 0.0f, -1.0f },
		{ 1.0f, -1.0f },
	};

	try_linear_transforms<Mode>(in, st, st->endpoints, adjust, 3, mode);
}

template<typename Mode>
static void bc6enc_refine_slide_selectors(const BC6Input &in, BC6BlockState *st, const BC6ModeDesc &mode)
{
	// c1 are big wiggles in one particular direction
	// c2 are really mostly just about trying slightly different endpoint roundings
	// much like in BC7, I need to investigate how much of this is _literally_ just
	// about choosing the nearest other representable number
	const float c1 = 3.0f / 8.0f;
	const float c2 = 1.0f / 16.0f;
	static const float adjust[8][2] =
	{
		{ -c2,  -c2 }, { 0.0f, -c1 }, { c2,  -c2 },
		{ -c1, 0.0f },                { c1, 0.0f },
		{ -c2,   c2 }, { 0.0f,  c1 }, { c2,   c2 },
	};

	try_linear_transforms<Mode>(in, st, st->endpoints, adjust, 8, mode);
}

template<typename Mode>
static void bc6enc_refine(const BC6Input &in, BC6BlockState *st, BC6Flags flags, const int optiters[2], const BC6EncOptions *opt, const BC6ModeDesc &mode, const BC6PartitionPrep *part_prep)
{
	if (flags & (BC6ENC_PCA | BC6ENC_PCA_STRETCH))
		bc6enc_refine_pca<Mode>(in, st, flags, opt, mode, part_prep);

	if(flags & BC6ENC_LSQ_FIT) bc6enc_refine_lsq_endpoints_iterated<Mode>(in, st, opt->max_lsq_iter, mode);
	if(flags & BC6ENC_EXTRAPOLATE_SELECTORS) bc6enc_refine_extrapolate_selectors<Mode>(in, st, mode);
	if(flags & BC6ENC_SLIDE_SELECTORS) bc6enc_refine_slide_selectors<Mode>(in, st, mode);
	if(optiters[0] > 0 && (flags & BC6ENC_JITTER_SELECTORS)) bc6enc_refine_jitter_selectors<Mode>(in, st, optiters[0], mode);
	if(optiters[1] > 0 && (flags & BC6ENC_ANNEAL_ENDPOINTS)) bc6enc_refine_anneal_endpoints<Mode>(in, st, optiters[1], mode);
}

static const BC6PrepInfo * bc6enc_get_prep_info(const BC6Prep * prep, int mode, int p)
{
	const BC6ModeDesc &desc = bc6_mode_desc[mode];
	return desc.is_two_subset ? &prep->sub2[p] : &prep->sub1;
}

template<int is_signed>
static void bc6enc_refine(const BC6Input &in, BC6BlockState *st, BC6Flags flags, const int optiters[2], const BC6EncOptions *opt, const BC6Prep * prep)
{
	const BC6ModeDesc &mode = bc6_mode_desc[st->mode];
	const BC6PartitionPrep * part_prep = &bc6enc_get_prep_info(prep, st->mode, st->p)->prep;
	if (mode.is_two_subset)
		bc6enc_refine<BC6Mode<2, is_signed> >(in, st, flags, optiters, opt, mode, part_prep);
	else
		bc6enc_refine<BC6Mode<1, is_signed> >(in, st, flags, optiters, opt, mode, part_prep);
}

// ---- main encode

template <typename Mode>
static BC6Error bc6enc_partition(const BC6Input &in, const BC6Prep *prep, BC6Results *results, BC6Flags flags, const BC6EncOptions *opt, int mode, int p, const BC6Endpoints *endpoints=0)
{
	const BC6ModeDesc &desc = bc6_mode_desc[mode];
	const BC6PrepInfo *prep_nfo = bc6enc_get_prep_info(prep, mode, p);

	BC6BlockState st(mode, p);
	st.endpoints = endpoints ? *endpoints : prep_nfo->initial_ep;
	if (!st.endpoints.can_fit_mode<Mode>(mode))
		return BC6_ERROR_MAX;

	// First try the color bbox
	bc6enc_trial<Mode>(in, &st, desc);

	if (flags & (BC6ENC_PCA | BC6ENC_PCA_STRETCH))
		bc6enc_refine_pca<Mode>(in, &st, flags, opt, desc, &prep_nfo->prep);

	results->add_state(&st);
	return st.err;
}

template<typename Mode>
static int bc6enc_partitions(const BC6Input &in, const BC6Prep *prep, BC6Results *results, BC6Flags flags, const BC6EncOptions *opt, int mode, int force_partition=-1)
{
	bool is_two_subset = bc6_mode_desc[mode].is_two_subset;
	const int num_partitions = is_two_subset ? 32 : 1;
	int num_part = radtex_min<int>(num_partitions, opt->max_part);

	if(force_partition >= 0)
	{
		bc6enc_partition<Mode>(in, prep, results, flags, opt, mode, force_partition, 0);
		return force_partition;
	}

	int best_part = 0;
	BC6Error best_err = BC6_ERROR_MAX;

	for (int pp = 0; pp < num_part && results->best_err > 0; ++pp)
	{
		int p = (num_part > 1) ? bc6_partition_order[pp] : 0;

		BC6Error err = bc6enc_partition<Mode>(in, prep, results, flags, opt, mode, p, 0);
		if (err < best_err)
		{
			best_err = err;
			best_part = p;
		}
	}

	return best_part;
}

static U32 bitrev6(U32 x)
{
	// Precomputed bit reverse table for 6 bits
	static const U8 tab[64] = {
		0x00, 0x20, 0x10, 0x30, 0x08, 0x28, 0x18, 0x38,
		0x04, 0x24, 0x14, 0x34, 0x0c, 0x2c, 0x1c, 0x3c,
		0x02, 0x22, 0x12, 0x32, 0x0a, 0x2a, 0x1a, 0x3a,
		0x06, 0x26, 0x16, 0x36, 0x0e, 0x2e, 0x1e, 0x3e,
		0x01, 0x21, 0x11, 0x31, 0x09, 0x29, 0x19, 0x39,
		0x05, 0x25, 0x15, 0x35, 0x0d, 0x2d, 0x1d, 0x3d,
		0x03, 0x23, 0x13, 0x33, 0x0b, 0x2b, 0x1b, 0x3b,
		0x07, 0x27, 0x17, 0x37, 0x0f, 0x2f, 0x1f, 0x3f,
	};
	return tab[x];
}

static U64 remove_anchor_bit(U64 idxs, U64 anchor_bit)
{
	// anchor bit must have been forced to zero on entry
	RR_ASSERT((idxs & anchor_bit) == 0);
	U64 lo = anchor_bit - 1ull; // bits below anchor_bit
	return (idxs & lo) | ((idxs >> 1) & ~lo);
}

void bc6enc_canonicalize(BC6BlockState *st)
{
	const BC6ModeDesc &mode = bc6_mode_desc[st->mode];
	const int ns = mode.is_two_subset ? 2 : 1;
	const int ib = mode.is_two_subset ? 3 : 4;
	const bool has_delta = mode.db[0] != 0;

	const U8 flip_bits = (1 << ib) - 1;
	const U8 anchor_bit = 1 << (ib - 1);
	U32 subset_mask = radtex_section_tbl[ns][st->p];

	if (st->idxs.ind[0] & anchor_bit)
	{
		if (!has_delta)
		{
			// Without delta-ing, everything is straightforward.
			flip_subset_inds<0>(&st->idxs, subset_mask, flip_bits);

			for (int c = 0; c < 3; ++c)
			{
				radtex_swap(st->endpoints.raw[0][c], st->endpoints.raw[1][c]);
				radtex_swap(st->endpoints.quant[0][c], st->endpoints.quant[1][c]);
			}
		}
		else
		{
			// Should never get there!
			RR_ASSERT_ALWAYS(!"fix_delta_endpoint01 should have taken care of this!");
		}
	}

	// Check the anchor index for subset 1.
	// Luckily this one is actually straightforward since the base for delta-ing
	// never changes from swapping these endpoints.
	if (ns == 2)
	{
		SINTa pos = radtex_anchor_2_2[st->p];
		if (st->idxs.ind[pos] & anchor_bit)
		{
			flip_subset_inds<1>(&st->idxs, subset_mask, flip_bits);

			for (int c = 0; c < 3; ++c)
			{
				radtex_swap(st->endpoints.raw[2][c], st->endpoints.raw[3][c]);
				radtex_swap(st->endpoints.quant[2][c], st->endpoints.quant[3][c]);
			}
		}
	}
}

template<int t_index>
static void bc6enc_emit_impl(U8 * output_bc6h, const BC6BlockState * state)
{
	RR_ASSERT(state->mode == t_index);

	const BC6ModeDesc &mode = bc6_mode_desc[t_index];
	const int ns = mode.is_two_subset ? 2 : 1;
	const int ib = mode.is_two_subset ? 3 : 4;

	S32 endpoints[4][3];
	for (int i = 0; i < ns*2; ++i)
	{
		for (int c = 0; c < 3; ++c)
		{
			endpoints[i][c] = state->endpoints.quant[i][c];
			if (mode.db[0] != 0 && i != 0)
			{
				endpoints[i][c] -= endpoints[0][c]; // delta coding
				// verify that deltas are in range
				RR_DURING_ASSERT(int sign_bit = 1 << (mode.db[c] - 1));
				RR_ASSERT(endpoints[i][c] >= -sign_bit && endpoints[i][c] < sign_bit);
			}
		}
	}

	U64 idxs = 0;
	for (int i = 0; i < 16; ++i)
		idxs |= static_cast<U64>(state->idxs.ind[i]) << (i * ib);

	// remove the anchor bits
	const U64 anchor0_bit = 1ull << (ib - 1);
	U64 anchor1_bit = 1ull << (ib * (radtex_anchor_2_2[state->p] + 1) - 1);
	idxs = remove_anchor_bit(idxs, anchor0_bit);
	if(ns >= 2)
	{
		anchor1_bit >>= 1;
		idxs = remove_anchor_bit(idxs, anchor1_bit);
	}

	U64 lo,hi;
	if (t_index < 2)
		lo = t_index;
	else
		lo = ((t_index + 14) >> 3) + (((t_index - 2) & 7) << 2);
	lo |= (U64)(endpoints[0][0] & 0x3FF) << 5; // _R_~0~^0^ only 6 bits always
	lo |= (U64)(endpoints[0][1] & 0x3FF) << 15; // _G_~0~^0^ only 6 bits always
	lo |= (U64)(endpoints[0][2] & 0x3FF) << 25; // _B_~0~^0^ only 6 bits always
	if(ns == 2)
	{
		lo |= (U64)(endpoints[1][0] & 0x3F) << 35; // _R_~1~^0^ only 4 bits always
		lo |= (U64)(endpoints[2][1] & 0xF) << 41; // _G_~2~^0^ ...
		lo |= (U64)(endpoints[1][1] & 0x3F) << 45; // _G_~1~^0^ only 4 bits always
		lo |= (U64)(endpoints[3][1] & 0xF) << 51; // _G_~3~^0^ ...
		lo |= (U64)(endpoints[1][2] & 0x3F) << 55; // _B_~1~^0^ only 4 bits always
		lo |= (U64)(endpoints[2][2] & 0xF) << 61; // _B_~2~^0^ ...
		hi = (U64)((endpoints[2][2] >> 3) & 1) << 0;
		hi |= (U64)(endpoints[2][0] & 0x3F) << (65-64); // _R_~2~^0^ ... only 4 bits always
		hi |= (U64)(endpoints[3][0] & 0x3F) << (71-64); // _R_~3~^0^ ... only 4 bits always
		hi |= (U64)state->p << (77-64);
		hi |= (U64)idxs << (82-64);
	}
	else
	{
		lo |= (U64)(endpoints[1][0] & 0x3FF) << 35; // _R_~1~^0^ only 4 bits always
		lo |= (U64)(endpoints[1][1] & 0x3FF) << 45; // _G_~1~^0^ only 4 bits always
		lo |= (U64)endpoints[1][2] << 55; // _B_~1~^0^ only 4 bits always
		hi = (U64)(endpoints[1][2] >> 9) & 1;
		hi |= (U64)idxs << 1;
	}
	const int m = t_index;
	switch(m) {
	case 0:  lo &= 0xEFFBFEFFFFFFFFE3ull; hi &= 0xFFFFFFFFFFFFEFBFull; break;
	case 1:  lo &= 0xFFFFFFF8FE3F8FE3ull; break;
	case 2:  lo &= 0xE7F9FEFFFFFFFFFFull; hi &= 0xFFFFFFFFFFFFEFBFull; break;
	case 3:  lo &= 0xE7FBFE7FFFFFFFFFull; hi &= 0xFFFFFFFFFFFFE79Full; break;
	case 4:  lo &= 0xEFF9FE7FFFFFFFFFull; hi &= 0xFFFFFFFFFFFFE79Full; break;
	case 5:  lo &= 0xEFFBFEFBFEFFBFFFull; hi &= 0xFFFFFFFFFFFFEFBFull; break;
	case 6:  lo &= 0xEFFBFFF9FE7f9fFFull; break;
	case 7:  lo &= 0xEFFFFEF9FE7F9fFFull; hi &= 0xFFFFFFFFFFFFEFBFull; break;
	case 8:  lo &= 0xFFFBFEF9FE7F9FFFull; hi &= 0xFFFFFFFFFFFFEFBFull; break;
	case 9:  lo &= 0xFFFFFFF87E1F87FFull; break;
	case 10: break;
	case 11: lo &= 0xFFBFEFFFFFFFFFFFull; hi &= 0xFFFFFFFFFFFFFFFEull; break;
	case 12: lo &= 0x7F9FE7FFFFFFFFFFull; hi &= 0xFFFFFFFFFFFFFFFEull; break;
	case 13: lo &= 0x0781E07FFFFFFFFFull; hi &= 0xFFFFFFFFFFFFFFFEull; break;
	}

	// The rest of the mess...
	if(m == 1 || (m >= 5 && m <= 9)) {
		lo |= (U64)((endpoints[2][2] >> 4) & 1) << 14; // _B_~2~^4^
		lo |= (U64)((endpoints[2][1] >> 4) & 1) << 24; // _G_~2~^4^
		lo |= (U64)((endpoints[3][2] >> 4) & 1) << 34; // _B_~3~^4^
	}

	if(m == 1 || m == 6 || m == 9) {
		lo |= (U64)((endpoints[3][2] >> 2) & 1) << 23; // _B_~3~^2^
	}

	if(m == 3 || m == 4) {
		lo |= (U64)((endpoints[0][0] >> 10) & 1) << 39; // _R_~0~^10^
	}

	if(m == 0 || m == 3 || m == 5 || m == 7 || m == 8) {
		lo |= (U64)((endpoints[3][1] >> 4) & 1) << 40; // _G_~3~^4^
	} else if(m == 4) {
		lo |= (U64)((endpoints[2][2] >> 4) & 1) << 40; // _B_~2~^4^
	} else if(m == 2) {
		lo |= (U64)((endpoints[0][0] >> 10) & 1) << 40; // _R_~0~^10^
	}

	if(m == 2 || m == 4) {
		lo |= (U64)((endpoints[0][1] >> 10) & 1) << 49; // _G_~0~^10^
	}

	if(m == 0 || m == 2 || m == 4 || m == 5 || m == 6 || m == 8) {
		lo |= (U64)((endpoints[3][2] >> 0) & 1) << 50; // _B_~3~^0^
	}

	if(m == 2 || m == 3) {
		lo |= (U64)((endpoints[0][2] >> 10) & 1) << 59; // _B_~0~^10^
	}

	if(m == 0 || m == 2 || m == 3 || m == 5 || m == 6 || m == 7) {
		lo |= (U64)((endpoints[3][2] >> 1) & 1) << 60; // _B_~3~^1^
	} else if(m == 4) {
		lo |= (U64)((endpoints[0][2] >> 10) & 1) << 60; // _B_~0~^10^
	}

	if(m == 0 || m == 2 || m == 3 || m == 4 || m == 5 || m == 7 || m == 8) {
		hi |= (U64)((endpoints[3][2] >> 2) & 1) << (70-64); // _B_~3~^2^
		hi |= (U64)((endpoints[3][2] >> 3) & 1) << (76-64); // _B_~3~^3^
	}

	switch(m) {
	case 0:
		lo |= (U64)((endpoints[2][1] >> 4) & 1) << 2; // _G_~2~^4^
		lo |= (U64)((endpoints[2][2] >> 4) & 1) << 3; // _B_~2~^4^
		lo |= (U64)((endpoints[3][2] >> 4) & 1) << 4; // _B_~3~^4^
		break;
	case 1:
		lo |= (U64)((endpoints[2][1] >> 5) & 1) << 2; // _G_~2~^5^
		lo |= (U64)((endpoints[3][1] >> 4) & 3) << 3; // _G_~3~^4^, _G_~3~^5^
		lo |= (U64)((endpoints[3][2] >> 0) & 3) << 12; // _B_~3~^0^, _B_~3~^1^
		lo |= (U64)((endpoints[2][2] >> 5) & 1) << 22; // _B_~2~^5^
		lo |= (U64)((endpoints[3][2] >> 3) & 1) << 32; // _B_~3~^3^
		lo |= (U64)((endpoints[3][2] >> 5) & 1) << 33; // _B_~3~^5^
		break;
	case 2:
		// nothing to do! (keep this case to make Clang happy)
		break;
	case 3:
		lo |= (U64)((endpoints[0][1] >> 10) & 1) << 50; // _G_~0~^10^
		hi |= (U64)((endpoints[3][2] >> 0) & 1) << (69-64); // _B_~3~^0^
		hi |= (U64)((endpoints[2][1] >> 4) & 1) << (75-64); // _G_~2~^4^
		break;
	case 4:
		hi |= (U64)((endpoints[3][2] >> 1) & 1) << (69-64); // _B_~3~^1^
		hi |= (U64)((endpoints[3][2] >> 4) & 1) << (75-64); // _B_~3~^4^
		break;
	case 5:
		// nothing to do! (keep this case to make Clang happy)
		break;
	case 6:
		lo |= (U64)((endpoints[3][1] >> 4) & 1) << 13; // _G_~3~^4^
		lo |= (U64)((endpoints[3][2] >> 3) & 3) << 33; // _B_~3~^3^, _B_~3~^4^
		break;
	case 7:
		lo |= (U64)((endpoints[3][2] >> 0) & 1) << 13; // _B_~3~^0^
		lo |= (U64)((endpoints[2][1] >> 5) & 1) << 23; // _G_~2~^5^
		lo |= (U64)((endpoints[3][1] >> 5) & 1) << 33; // _G_~3~^5^
		break;
	case 8:
		lo |= (U64)((endpoints[3][2] >> 1) & 1) << 13; // _B_~3~^1^
		lo |= (U64)((endpoints[2][2] >> 5) & 1) << 23; // _B_~2~^5^
		lo |= (U64)((endpoints[3][2] >> 5) & 1) << 33; // _B_~3~^5^
		break;
	case 9:
		lo |= (U64)((endpoints[3][1] >> 4) & 1) << 11; // _G_~3~^4^
		lo |= (U64)((endpoints[3][2] >> 0) & 3) << 12; // _B_~3~^0^, _B_~3~^1^
		lo |= (U64)((endpoints[2][1] >> 5) & 1) << 21; // _G_~2~^5^
		lo |= (U64)((endpoints[2][2] >> 5) & 1) << 22; // _B_~2~^5^
		lo |= (U64)((endpoints[3][1] >> 5) & 1) << 31; // _G_~3~^5^
		lo |= (U64)((endpoints[3][2] >> 3) & 1) << 32; // _B_~3~^3^
		lo |= (U64)((endpoints[3][2] >> 5) & 1) << 33; // _B_~3~^5^
		break;
	case 10:
		// nothing to do! (keep this case to make Clang happy)
		break;
	case 12:
		lo |= (U64)((endpoints[0][0] >> 11) & 1) << 43; // _R_~0~^11^
		lo |= (U64)((endpoints[0][1] >> 11) & 1) << 53; // _G_~0~^11^
		lo |= (U64)((endpoints[0][2] >> 11) & 1) << 63; // _B_~0~^11^
		// intentional fall-through
	case 11:
		lo |= (U64)((endpoints[0][0] >> 10) & 1) << 44; // _R_~0~^10^
		lo |= (U64)((endpoints[0][1] >> 10) & 1) << 54; // _G_~0~^10^
		hi |= (U64)((endpoints[0][2] >> 10) & 1) << (64-64); // _B_~0~^10^
		break;
	case 13:
		lo |= (U64)bitrev6((endpoints[0][0] >> 10) & 0x3F) << 39;
		lo |= (U64)bitrev6((endpoints[0][1] >> 10) & 0x3F) << 49;
		lo |= (U64)bitrev6((endpoints[0][2] >> 10) & 0x3F) << 59;
		hi |= (U64)((endpoints[0][2] >> 10) & 1) << (64-64); // _B_~0~^10^
		break;
	}

	RR_PUT64_LE(output_bc6h + 0, lo);
	RR_PUT64_LE(output_bc6h + 8, hi);
}

void bc6enc_emit(U8 * output_bc6h, const BC6BlockState * st)
{
	switch (st->mode) {
	case 0: bc6enc_emit_impl<0>(output_bc6h, st); break;
	case 1: bc6enc_emit_impl<1>(output_bc6h, st); break;
	case 2: bc6enc_emit_impl<2>(output_bc6h, st); break;
	case 3: bc6enc_emit_impl<3>(output_bc6h, st); break;
	case 4: bc6enc_emit_impl<4>(output_bc6h, st); break;
	case 5: bc6enc_emit_impl<5>(output_bc6h, st); break;
	case 6: bc6enc_emit_impl<6>(output_bc6h, st); break;
	case 7: bc6enc_emit_impl<7>(output_bc6h, st); break;
	case 8: bc6enc_emit_impl<8>(output_bc6h, st); break;
	case 9: bc6enc_emit_impl<9>(output_bc6h, st); break;
	case 10: bc6enc_emit_impl<10>(output_bc6h, st); break;
	case 11: bc6enc_emit_impl<11>(output_bc6h, st); break;
	case 12: bc6enc_emit_impl<12>(output_bc6h, st); break;
	case 13: bc6enc_emit_impl<13>(output_bc6h, st); break;
	default: RR_BREAK(); // illegal!
	}
}

template<int is_signed>
static BC6BlockState * BC6H_EncodeCore(const BC6Input &in, const BC6EncOptions &opt)
{
	typedef BC6Mode<2, is_signed> TwoSubset;
	typedef BC6Mode<1, is_signed> OneSubset;

	// do the prep
	BC6Prep prep;
	{SIMPLEPROFILE_SCOPE(BC6Prep);
		prep.init(in, &opt);
	}

	BC6Results seed_results;
	seed_results.num = opt.narrow0;

	BC6Flags one_subset_seed_flags = opt.flags;
	BC6Flags two_subset_seed_flags = opt.flags & ~BC6ENC_PCA_STRETCH;

	if (opt.flags & BC6ENC_AGGRESSIVE_MODE_PRUNE)
	{
		one_subset_seed_flags &= ~(BC6ENC_PCA | BC6ENC_PCA_STRETCH);
		two_subset_seed_flags &= ~(BC6ENC_PCA | BC6ENC_PCA_STRETCH);
	}

	// Seed stage
	// try to find good partition and endpoint candidates
	// ignoring for now how well they quantize
	{SIMPLEPROFILE_SCOPE(BC6NewSeed);
		bc6enc_partitions<OneSubset>(in, &prep, &seed_results, one_subset_seed_flags, &opt, 14); // seed for one-subset
		bc6enc_partitions<TwoSubset>(in, &prep, &seed_results, two_subset_seed_flags, &opt, 15); // seed for two-subset
	}

	// Initial fit stage
	// given the seeds, check which modes we can fit them in
	BC6Results enc_results;
	enc_results.num = opt.narrow1 ? opt.narrow1 : opt.narrow0;

	BC6Flags initial_flags = opt.flags & ~(BC6ENC_PCA | BC6ENC_PCA_STRETCH);

	{SIMPLEPROFILE_SCOPE(BC6NewInitial);
		for (int i = 0; i < seed_results.used; ++i)
		{
			BC6BlockState *seed = seed_results.heap[i].slot;
			const BC6Endpoints *ep = &seed->endpoints;
			int p = seed->p;

			// Literally the only thing we get out of different modes is a larger maximum
			// allowed delta distance. So there is very little benefit to using a mode that
			// quantizes the endpoint bits more than necessary to hit that goal.

			// TODO be cleverer about pruning modes here

			if (seed->mode == 14)
			{
				if (opt.flags != one_subset_seed_flags)
					bc6enc_refine_pca<OneSubset>(in, seed, opt.flags, &opt, bc6_mode_desc[seed->mode], &bc6enc_get_prep_info(&prep, seed->mode, p)->prep);

				if (opt.flags & BC6ENC_AGGRESSIVE_MODE_PRUNE)
				{
					// take the first mode that will work
					for (int mode = 13; mode >= 10; --mode)
					{
						if (bc6enc_partition<OneSubset>(in, &prep, &enc_results, initial_flags, &opt, mode, p, ep) < BC6_ERROR_MAX)
							break;
					}
				}
				else
				{
					// try all one-subset modes
					for (int mode = 10; mode <= 13; ++mode)
						bc6enc_partition<OneSubset>(in, &prep, &enc_results, initial_flags, &opt, mode, p, ep);
				}
			}
			else
			{
				RR_ASSERT(seed->mode == 15);

				if (opt.flags != two_subset_seed_flags)
					bc6enc_refine_pca<TwoSubset>(in, seed, opt.flags, &opt, bc6_mode_desc[seed->mode], &bc6enc_get_prep_info(&prep, seed->mode, p)->prep);

				if (opt.flags & BC6ENC_AGGRESSIVE_MODE_PRUNE)
				{
					// modes sorted by decreasing endpoint precision
					// take the first that will work
					static const int order[] = { 2,3,4, 0, 5, 6,7,8, 1, 9 };
					for (int orderi = 0; orderi < 10; ++orderi)
					{
						int mode = order[orderi];
						if (bc6enc_partition<TwoSubset>(in, &prep, &enc_results, initial_flags, &opt, mode, p, ep) < BC6_ERROR_MAX)
							break;
					}
				}
				else
				{
					// try endpoints with all two-subset modes
					for (int mode = 0; mode <= 9; ++mode)
						bc6enc_partition<TwoSubset>(in, &prep, &enc_results, initial_flags, &opt, mode, p, ep);
				}
			}
		}
	}

	{SIMPLEPROFILE_SCOPE(BC6NewRefine);

	if(opt.narrow2)
	{
		// LSQ fit
		for (int i = 0; i < enc_results.used; ++i)
		{
			BC6BlockState *slot = enc_results.heap[i].slot;
			bc6enc_refine_lsq_endpoints_pass<is_signed>(in, slot, 3);
		}

		// Narrow it down
		enc_results.shrink(opt.narrow2);
	}

	// Refine the rest
	for (int i = 0; i < enc_results.used; ++i)
	{
		BC6BlockState *slot = enc_results.heap[i].slot;
		bc6enc_refine<is_signed>(in, slot, opt.flags, opt.optiters_early, &opt, &prep);
	}

	}
	// Which one was the best?
	BC6BlockState *best = enc_results.heap[0].slot;
	BC6Error best_err = best->err;
	for(int i = 1; i < enc_results.used; ++i)
	{
		BC6BlockState *slot = enc_results.heap[i].slot;
		if(slot->err < best_err)
		{
			best = slot;
			best_err = best->err;
		}
	}

	// Further refine the best block (can do this any number of times and will likely see small improvements)
	{
		SIMPLEPROFILE_SCOPE(BC6NewRefineFinal);

		BC6Flags filtered_flags = opt.flags & ~(BC6ENC_PCA | BC6ENC_PCA_STRETCH); // remove PCA, no benefit to doing this again.
		bc6enc_refine<is_signed>(in, best, filtered_flags, opt.optiters_late, &opt, &prep);
	}

	return best;
}

static bool all_one_color(const U16 * block)
{
	for ( int i = 4; i < 64; i += 4 )
	{
		if ( block[i+0] != block[0] || block[i+1] != block[1] || block[i+2] != block[2] )
			return false;
	}

	return true;
}

void bc6enc_encode_single_color_block(U8 * output_bc6h, const U16 in_color[4], bool is_signed)
{
	BC6BlockState st;

	// mode and indices
	st.mode = 13;
	for (int i = 0; i < 16; ++i)
		st.idxs.ind[i] = 0;

	// endpoints
	for (int c = 0; c < 3; ++c)
	{
		int remap = remap_half_to_int(in_color[c]);
		remap = radtex_clamp(remap, is_signed ? -0x7bff : 0, 0x7bff);
		st.endpoints.raw[0][c] = static_cast<S16>(remap);
		st.endpoints.raw[1][c] = static_cast<S16>(remap);
	}

	if (is_signed)
		st.endpoints.quantize<BC6Mode<1, 1> >(bc6_mode_desc[13]);
	else
		st.endpoints.quantize<BC6Mode<1, 0> >(bc6_mode_desc[13]);

	bc6enc_emit_impl<13>(output_bc6h, &st);
}

static U16 canonicalize_signed(U16 x)
{
	U16 non_sign = x & 0x7fff;
	if (non_sign < 0x7c00) // normal, just pass through
		return x;
	else if (non_sign == 0x7c00) // infinity, clamp to largest normal
		return (x & 0x8000) | 0x7bff;
	else // NaN, turn into 0
		return 0;
}

static U16 canonicalize_unsigned(U16 x)
{
	if ( x < 0x7c00 ) // normal and non-negative, just pass through
		return x;
	else if ( x == 0x7c00 ) // infinity turns into largest normal
		return 0x7bff;
	else // NaNs or anything negative turns into 0
		return 0;
}

void bc6enc_canonicalize_pixels(U16 block[64], const U16 in_block[64], bool is_signed)
{
	if ( is_signed )
	{
		for (int i = 0; i < 64; i += 4)
		{
			block[i + 0] = canonicalize_signed(in_block[i + 0]);
			block[i + 1] = canonicalize_signed(in_block[i + 1]);
			block[i + 2] = canonicalize_signed(in_block[i + 2]);
			block[i + 3] = 0x3c00; // 1.0h
		}
	}
	else
	{
		for (int i = 0; i < 64; i += 4)
		{
			block[i + 0] = canonicalize_unsigned(in_block[i + 0]);
			block[i + 1] = canonicalize_unsigned(in_block[i + 1]);
			block[i + 2] = canonicalize_unsigned(in_block[i + 2]);
			block[i + 3] = 0x3c00; // 1.0h
		}
	}
}

void bc6enc_options_init(BC6EncOptions * opt, rrDXTCLevel level, rrDXTCOptions options, bool is_signed)
{
	// good # partitions to try: (aligning with the breaks in the partition order table)
	//   2,6,10,14,16,22,28,32
	switch (level)
	{
	case rrDXTCLevel_VeryFast:
		// this corresponds to "fastest3" in rad_tex_tools
		opt->flags = BC6ENC_AGGRESSIVE_MODE_PRUNE | BC6ENC_PCA;
		opt->narrow0 = 3;
		opt->narrow1 = 1;
		opt->narrow2 = 1;
		opt->max_part = 6;
		break;

	case rrDXTCLevel_Fast:
		// this corresponds to "fastest" in rad_tex_tools
		opt->flags = BC6ENC_AGGRESSIVE_MODE_PRUNE | BC6ENC_PCA_STRETCH;
		opt->narrow0 = 5;
		opt->narrow1 = 0;
		opt->narrow2 = 1;
		opt->max_part = 10;
		break;

	default:
	case rrDXTCLevel_Slow:
		// this corresponds to "fast" in rad_tex_tools
		opt->flags = BC6ENC_PCA_STRETCH | BC6ENC_EXTRAPOLATE_SELECTORS | BC6ENC_SLIDE_SELECTORS | BC6ENC_JITTER_SELECTORS;
		opt->narrow0 = 5;
		opt->narrow1 = 3;
		opt->narrow2 = 1;
		opt->max_part = 16;
		opt->optiters_early[0] = 3;
		opt->optiters_late[0] = 9;
		break;

	case rrDXTCLevel_VerySlow:
		// this corresponds to "slow" in rad_tex_tools
		opt->flags = BC6ENC_PCA_STRETCH | BC6ENC_LSQ_FIT | BC6ENC_EXTRAPOLATE_SELECTORS | BC6ENC_SLIDE_SELECTORS | BC6ENC_JITTER_SELECTORS;
		opt->narrow0 = 10;
		opt->narrow1 = 4;
		opt->narrow2 = 2;
		opt->max_part = 32;
		opt->optiters_early[0] = 6;
		opt->optiters_late[0] = 30;
		break;

	case rrDXTCLevel_Reference:
		// this corresponds to "super" in rad_tex_tools
		// NOTE(fg): not anymore - a lot more stuff on, I want this to be reference for real
		opt->flags = BC6ENC_PCA_STRETCH | BC6ENC_LSQ_FIT | BC6ENC_EXTRAPOLATE_SELECTORS | BC6ENC_SLIDE_SELECTORS | BC6ENC_JITTER_SELECTORS | BC6ENC_ANNEAL_ENDPOINTS;
		opt->narrow0 = 12;
		opt->narrow1 = 10;
		opt->narrow2 = 8;
		opt->max_lsq_iter = 100;
		opt->optiters_early[0] = 16;
		opt->optiters_early[1] = 48;
		opt->optiters_late[0] = 64;
		opt->optiters_late[1] = 64;
		break;
	}

	if ( options & rrDXTCOptions_BC6_FavorLuminance )
		opt->flags |= BC6ENC_WEIGHTED_CHANNELS;

	if ( is_signed )
		opt->flags |= BC6ENC_SIGNED;
}

void bc6enc_compress_block(U8 * output_bc6h, const U16 * in_block, const BC6EncOptions & opt)
{
	bool is_signed = ( opt.flags & BC6ENC_SIGNED ) != 0;

	// Make block canonical
	U16 block[64];
	bc6enc_canonicalize_pixels(block, in_block, is_signed);

	// Check whether the block is all one solid color, which means we can
	// instantly emit an optimal encoding
	if ( all_one_color(block) )
	{
		bc6enc_encode_single_color_block(output_bc6h, block, is_signed);
		return;
	}

	rrRandState rand_state = RR_RANDSTATE_INIT_VALUE;
	if (opt.flags & BC6ENC_RANDOMIZED_MASK)
	{
		// NOTE(fg): this matches rad_tex_tools but skips every second DWord for U16 input in BC6H
		// might want to fix that eventually
		for (int i=0;i<16;i+=4)
		{
			rand_state.c ^= RR_GET32_LE(&block[i*4]);
			rand_state.x ^= RR_GET32_LE(&block[i*4+4]);
			rand_state.y ^= RR_GET32_LE(&block[i*4+8]);
			rand_state.z ^= RR_GET32_LE(&block[i*4+12]);
			// run a round after each 4 :
			rrRandState32(&rand_state);
		}
	}

	BC6Input in;
	BC6BlockState * best = 0;
	in.init(block, rand_state, opt.flags);

	in.active_metric = BC6METRIC_MRSSE;

	if (is_signed)
		best = BC6H_EncodeCore<1>(in, opt);
	else
		best = BC6H_EncodeCore<0>(in, opt);

	/*
	// FG TEST: try switching metric at the last minute and see if another
	// index finder with the same endpoints works
	// (to narrow in: is the problem with the indices or the endpoints?)
	if (!is_signed)
	{
		in.active_metric = BC6METRIC_MRSSE;
		const BC6ModeDesc &mode = bc6_mode_desc[best->mode];
		if (mode.is_two_subset)
			bc6enc_trial<BC6Mode<2, 0> >(in, best, mode);
		else
			bc6enc_trial<BC6Mode<1, 0> >(in, best, mode);
	}
	*/

	// Actually encode the BC6H block into the output data.
	bc6enc_canonicalize(best);
	bc6enc_emit(output_bc6h, best);
}

void bc6enc_state_from_bits(BC6BlockState * st, const U8 * input_bc6h, bool is_signed)
{
	// NOTE this could be optimized if necessary, but let's just lean on the
	// known-good reference decoder for as long as we can.
	BC6HBlockRawDesc desc;

	bool valid = bc6h_analyze_block_raw(&desc, is_signed, input_bc6h);
	RR_ASSERT(valid);

	if (!valid)
	{
		memset(st, 0, sizeof(*st));
		return;
	}

	const BC6HModeDesc * m = desc.mode;
	st->mode = m->linmode;
	st->p = desc.partition_id;
	st->err = 0; // not actually known, just make sure it's initialized

	for (int i = 0; i < 16; ++i)
		st->idxs.ind[i] = desc.inds[i];

	for (int i = 0; i < m->ns * 2; ++i)
	{
		for (int c = 0; c < 3; ++c)
		{
			// grab the coded values from the block
			S32 scaled = desc.endpoints_internal[c][i];
			st->endpoints.quant[i][c] = desc.endpoints_quant[c][i];

			// our "raw" endpoints are straight 16-bit half-floats
			if (is_signed)
			{
				// check that our quantization round-tripped; it should have since we started
				// with exactly representable values!
				RR_ASSERT(bc6_dequant<1>(m->epb, st->endpoints.quant[i][c]) == scaled);
				st->endpoints.raw[i][c] = static_cast<U16>(bc6_scaled_to_half<1>(scaled));
			}
			else
			{
				// check that our quantization round-tripped; it should have since we started
				// with exactly representable values!
				RR_ASSERT(bc6_dequant<0>(m->epb, st->endpoints.quant[i][c]) == scaled);
				st->endpoints.raw[i][c] = static_cast<U16>(bc6_scaled_to_half<0>(scaled));
			}
		}
	}
}

template<int t_is_signed>
static RADFORCEINLINE BC6Error bc6enc_eval_error_impl(const BC6BlockState * st, const BC6Input * input)
{
	BC6ColorSet set;
	typedef BC6Mode<2, t_is_signed> TwoSubset;
	typedef BC6Mode<1, t_is_signed> OneSubset;

	const BC6ModeDesc &mode = bc6_mode_desc[st->mode];
	if (mode.is_two_subset)
	{
		set.init<TwoSubset>(*input, st->endpoints, mode);
		return eval_indexes<TwoSubset>(*input, st, set);
	}
	else
	{
		set.init<OneSubset>(*input, st->endpoints, mode);
		return eval_indexes<OneSubset>(*input, st, set);
	}
}

BC6Error bc6enc_eval_error(const BC6BlockState * st, const BC6Input * input, bool is_signed)
{
	if (is_signed)
		return bc6enc_eval_error_impl<1>(st, input);
	else
		return bc6enc_eval_error_impl<0>(st, input);
}

template<int t_is_signed>
static RADFORCEINLINE BC6Error bc6enc_reindex_block_impl(BC6BlockState * st, const BC6Input * input)
{
	BC6ColorSet set;
	typedef BC6Mode<2, t_is_signed> TwoSubset;
	typedef BC6Mode<1, t_is_signed> OneSubset;

	const BC6ModeDesc &mode = bc6_mode_desc[st->mode];
	if (mode.is_two_subset)
	{
		set.init<TwoSubset>(*input, st->endpoints, mode);
		return calc_indexes_anchor_zero<TwoSubset>(*input, st, set);
	}
	else
	{
		set.init<OneSubset>(*input, st->endpoints, mode);
		return calc_indexes_anchor_zero<OneSubset>(*input, st, set);
	}
}

BC6Error bc6enc_reindex_block(BC6BlockState * st, const BC6Input * input, bool is_signed)
{
	if (is_signed)
		return bc6enc_reindex_block_impl<1>(st, input);
	else
		return bc6enc_reindex_block_impl<0>(st, input);
}

template<int t_is_signed>
static RADFORCEINLINE BC6Error bc6enc_new_endpoints_impl(BC6BlockState * st, const BC6Input * input)
{
	BC6ColorSet set;
	typedef BC6Mode<2, t_is_signed> TwoSubset;
	typedef BC6Mode<1, t_is_signed> OneSubset;

	const BC6ModeDesc &mode = bc6_mode_desc[st->mode];

	// @@ try changing the mode here (while preserving the subset count) to match the new endpoints.
	// might as well. I did an initial impl (one-subset modes only) and it helped MRSSE a lot but was
	// more mixed on VCDiff; still seems like the right thing to do, just need to spend some time
	// coming up with a good way to choose the ideal mode given endpoint values.

	// NOTE(fg): tried weighted here, no win.

	// Not being allowed to change the indices makes our job simpler because it restricts our
	// options greatly, but it also means we don't get to use the higher-level funcs which all
	// want to change indices along the way.
	if (mode.is_two_subset)
	{
		bc6enc_refine_lsq_endpoints<TwoSubset>(*input, st);
		//bc6enc_refine_weighted_lsq_endpoints<TwoSubset>(*input, st);
		calc_endpoints<TwoSubset>(&st->endpoints, mode);
		set.init<TwoSubset>(*input, st->endpoints, mode);
		st->err = eval_indexes<TwoSubset>(*input, st, set);
	}
	else
	{
		bc6enc_refine_lsq_endpoints<OneSubset>(*input, st);
		//bc6enc_refine_weighted_lsq_endpoints<OneSubset>(*input, st);
		calc_endpoints<OneSubset>(&st->endpoints, mode);
		set.init<OneSubset>(*input, st->endpoints, mode);
		st->err = eval_indexes<OneSubset>(*input, st, set);
	}

	return st->err;
}

BC6Error bc6enc_new_endpoints_with_indices_fixed(BC6BlockState * st, const BC6Input * input, bool is_signed)
{
	if (is_signed)
		return bc6enc_new_endpoints_impl<1>(st, input);
	else
		return bc6enc_new_endpoints_impl<0>(st, input);
}

OODLE_NS_END

