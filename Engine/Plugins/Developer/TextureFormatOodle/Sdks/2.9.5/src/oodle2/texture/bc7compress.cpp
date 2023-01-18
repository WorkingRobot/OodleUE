// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetsse4

#include "bc7compress.h"
#include "bc7compress_internal.h"
#include "bc67tables.h"
#include "bc67format.h"
#include "bc7bits.h"
#include "bc7decode_fast.h"
#include "templates/rralgorithm.h"
#include "templates/rrnew.h"
#include "rrrand.h"
#include "vec128.inl"
#include <float.h>
#include <string.h>
#include <math.h>
#include "rrsimd.h"

/*
#include "rrsimpleprof.h"
/*/
#include "rrsimpleprofstub.h"
/**/

/*******

CB documentation on 01-07-2020 :
bc7compress overall structure :

There are two primary phases on each block :
1. initial trials
2. refinement

A [BC7Results] holds the current active candidate set.

BC7_CompressBlock does :

initial trials phase :

for all modes (bc7enc_try_mode)
  for all partitions (bc7enc_try_partition) (*)
    for all rbits/isbit
      make an initial guess at the error
      put in [BC7Results]

(* only the most likely best max_part partitions are tried)      

The initial error guess just takes each subset
uses the color bbox min/max on that subset as endpoints
except in levels VerySlow & Reference where bc7enc_refine_subset_partition does pca

the initial trials phase gathers the best [narrow0] candidates in the [BC7Results]

we then go to the refinement phase (bc7_enc_try_refine)

in the refinement phase, the choices of mode/part/isbit/rbits is not changed
we just take candidates for those settings and try to find better endpoints for the subsets

refinement works in several stages of reducing the candidate set and doing more work each time

narrow0->narrow1 : pca
narrow1->narrow2 : lsq
narrow2->1 : bc7enc_refine with various flags
1 : bc7enc_refine again with BRUTE_PBITS & IDXS_EXACT (in Reference)


Most of the work is done on pixels in subset dense linear order.
BC7BlockInput/BC7SubsetInput takes a block and given mode/part/rbits
packs the pixels densely and does rbit rotation
so all the work on subset endpoint choosing just sees a dense linear array of RGBA

On output (canonicalize/emit) the subset linear order is scattered back out to block order

********/

/*
// !!!! force off sse4 to check for scalar-sse4 match :
#ifdef DO_BUILD_SSE4
#undef DO_BUILD_SSE4
#endif
/**/

//#define TRACE(...) rrprintf(__VA_ARGS__)
#define TRACE(...)

// CB : I'm using DO_BUILD_SSE4 from rrsimd.h
//	this tells whether you are allowed to compile it (eg. am I in VC 2005)
//	there could be a separate DO_BUILD_SSSE3 but keep things simple and just use one toggle
#ifdef DO_BUILD_SSE4
#include <smmintrin.h>
#endif

//#define SCALAR_PARANOIA // debug only: checks (some) SIMD kernels against scalar ones, checks for exact match
//#define PCA_REFACTOR

#if defined(SCALAR_PARANOIA) && defined(__RADRELEASE__)
#pragma warning("SCALAR_PARANOIA on in release build!")
#endif

#if defined(_MSC_VER) && _MSC_VER < 1500
#pragma warning(disable: 4244)
#endif

OODLE_NS_START

#define PCA_POWER_ITERS 3

struct BC7Prep;

// scalar path uses lrintf for float->int to match the SSE path
#if __STDC_VERSION__ >= 199901L
// c99 compilers use the C lib version of this :
// lrintf should be in math.h
// stdc lrintf returns "long"
#elif defined(__RADSSE2__)
static inline int lrintf(float const x) 
{
    return _mm_cvtss_si32(_mm_set_ss(x));
}
#else
// not a true lrint
static inline int lrintf(float const x) 
{
	return (int)(x + 0.5f);
}
#endif

template<typename T> inline T radtex_sqr(T a) { return a*a; }
template<typename T> inline T radtex_min(T a, T b) { return a < b ? a : b; }
template<typename T> inline T radtex_max(T a, T b) { return a > b ? a : b; }
template<typename T> inline T radtex_clamp(T a, T b, T c) { return a > c ? c : a < b ? b : a; }
inline U8 round_and_clamp_U8(float f) { return (U8) RR_CLAMP_U8( lrintf(f) ); }
inline int radtex_round(float f) { return (int) lrintf(f); }

template<typename T> inline void radtex_swap(T &a, T &b)
{
	T tmp = a;
	a = b;
	b = tmp;
}

#if defined(DO_BUILD_SSE4)

static void ssse3_calc_subset_summaries(BC7Color out_bbox[3][2], const U8 in_block[64], int ns, int p)
{
	const Vec128_U32 cur_subset_inc = Vec128_U32(1<<0, 1<<2, 1<<4, 1<<6);
	const Vec128_U32 mask_subset_ind = Vec128_U32(3<<0, 3<<2, 3<<4, 3<<6);
	const Vec128_U8 all_ones = Vec128_U8(~0);

	Vec128_U32 cur_subset = Vec128_U32::zero();
	Vec128_U32 packed_subset_inds = Vec128_U32(radtex_section_tbl[ns][p]);

	for(int s = 0; s < ns; ++s)
	{
		Vec128_U32 packed_subset_cur = packed_subset_inds;
		Vec128_U8 endpoints_min = Vec128_U8::zero();
		Vec128_U8 endpoints_max = Vec128_U8::zero();

		// accumulate max of x (actual max) and max of ~x (=min of x)

		for (int i = 0; i < 64; i += 16)
		{
			Vec128_U8 pixels = Vec128_U8::loadu(in_block + i); // 4x(r,g,b,a)

			// extract subset mask
			Vec128_U32 subset_inds = packed_subset_cur & mask_subset_ind;
			Vec128_U8 in_cur_subset = subset_inds.cmp_eq(cur_subset).u8();
			packed_subset_cur = packed_subset_cur.srl<8>();

			// max with 0 is idempotent -> do that for pixels outside active subset
			Vec128_U8 pixels_masked = in_cur_subset & pixels;
			Vec128_U8 not_pixels_masked = in_cur_subset.andnot(pixels);

			endpoints_min = vmax(endpoints_min, not_pixels_masked);
			endpoints_max = vmax(endpoints_max, pixels_masked);
		}

		// finish the min/max reductions on the endpoints
		endpoints_min.reduce_max_8away();
		endpoints_max.reduce_max_8away();
		endpoints_min.reduce_max_4away();
		endpoints_max.reduce_max_4away();

		// finally complement endpoints_min to get the actual mins
		// (so far we've been max-reducing ~x)
		endpoints_min ^= all_ones;

		// store all the endpoints and advance
		//store32u(&out_endpoints[0][s], endpoints_min);
		//store32u(&out_endpoints[1][s], endpoints_max);
		endpoints_min.storeu_lo32(&out_bbox[s][0]);
		endpoints_max.storeu_lo32(&out_bbox[s][1]);

		cur_subset += cur_subset_inc;
	}
	
	// make sure the endpoint arrays are fully initialized
	for(int s=ns;s<3;++s)
	{
		out_bbox[s][0].dw = 0;
		out_bbox[s][1].dw = 0;
	}
}

#endif

// calc fills endpoints[] with the color bbox of each subset
void BC7PartitionInfo::calc(const U8 block[64], int p, int ns, BC7Flags flags)
{
#if defined(DO_BUILD_SSE4)
	ssse3_calc_subset_summaries(bbox, block, ns, p);
#else
	{
	const int comp = 4;

	for LOOP(s,3)
	{
		memset(&bbox[s][0], 255, sizeof(bbox[s][0]));
		memset(&bbox[s][1], 0, sizeof(bbox[s][1]));
	}
	
	U32 im = radtex_section_tbl[ns][p];
	for (int i = 0; i < 64; i += 4) {
		int s = im & 3;	im >>= 2;
		for (int c = 0; c < comp; ++c) {
			bbox[s][0].v[c] = RR_MIN(bbox[s][0].v[c], block[i + c]);
			bbox[s][1].v[c] = RR_MAX(bbox[s][1].v[c], block[i + c]);
		}
	}
	}
#endif
}

// inds = radtex_subset_to_inds pixel indexes
void BC7BlockInput::init(const U8 * block, U64 inds, U8 rbit)
{
	RR_ASSERT(rbit <= 3);

#if defined(DO_BUILD_SSE4)
	static RAD_ALIGN(const U8, rbit_shuffles[4][8], 8) =
	{
		{ 0,1,2,3, 4,5,6,7 }, // rbit=0
		{ 3,1,2,0, 7,5,6,4 }, // rbit=1
		{ 0,3,2,1, 4,7,6,5 }, // rbit=2
		{ 0,1,3,2, 4,5,7,6 }, // rbit=3
	};
	Vec128_U8 rbit_shuffle = Vec128_U8::loadu_lo64(rbit_shuffles[rbit]);

	for (int i = 0; i < 16; i += 2)
	{
		Vec128_S32 va = Vec128_S32::loadu_lo32(block + ((inds << 2) & 0x3c)); // ((inds >> 0) & 0xf) << 2
		Vec128_S32 vb = Vec128_S32::loadu_lo32(block + ((inds >> 2) & 0x3c)); // ((inds >> 4) & 0xf) << 2

		va = va.unpack_lo(vb);
		va.u8().shuf(rbit_shuffle).storeu_lo64(&pixels[i * 4]);

		inds >>= 8;
	}
#else
	// unroll 2x to reduce overhead (somewhat)
	for (int i = 0; i < 16; i += 2)
	{
		U32 a = RR_GET32_NATIVE(block + ((inds << 2) & 0x3c)); // ((inds >> 0) & 0xf) << 2
		U32 b = RR_GET32_NATIVE(block + ((inds >> 2) & 0x3c)); // ((inds >> 4) & 0xf) << 2

		RR_PUT32_NATIVE(&pixels[i*4 + 0], a);
		RR_PUT32_NATIVE(&pixels[i*4 + 4], b);
		inds >>= 8;
	}

	if (rbit != 0)
	{
		// Apply channel rotation
		for (int i = 0; i < 64; i += 4)
			radtex_swap(pixels[i + 3], pixels[i + rbit - 1]);
	}
#endif

	// clear other half (just so it's initialized)
	memset(pixels + 16*4, 0, NPAD*4);
}

// BC7SubsetInput just points into a portion of a BC7BlockInput
void BC7SubsetInput::init(const U8 * in_pixels, int num, int alpha_chan, int alpha_weight)
{
	static const U8 count_masks[32] =
	{
		0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	};

	pixels = in_pixels;
	num_pixels = num;
	rcp_num_pixels = 1.0f / num;
	count_mask = count_masks + 16 - num;

	RR_ASSERT(0 <= alpha_weight && alpha_weight <= 128);
	for LOOP(i,4)
		channel_weights[i] = 1;
	channel_weights[alpha_chan] = static_cast<U16>(alpha_weight);
}

void BC7SubsetState::init_empty(U8 isb, U8 rb)
{
	// the initial trials unconditionally writes [err]
	// but the refinement phase try_and_commit only writes if err gets better
	// so initialize with large values
	err = BC7_ERROR_MAX;
	scalar_err = BC7_ERROR_MAX / 2; // so vec_err = err - scalar_err = BC7_ERROR_MAX/2; also

	pbits = 0;
	isbit = isb;
	rbit = rb;
}

void BC7SubsetState::init(const BC7Color &endpt0, const BC7Color &endpt1, U8 isb)
{
	init_empty(isb,0);
	
	endpoints[0] = endpt0;
	endpoints[1] = endpt1;
}

void BC7SubsetState::set_rbit(int new_rbit)
{
	RR_ASSERT(rbit == 0);
	if (!new_rbit)
		return;

	rbit = static_cast<U8>(new_rbit);
	radtex_swap(endpoints[0].a, endpoints[0].v[new_rbit - 1]);
	radtex_swap(endpoints[1].a, endpoints[1].v[new_rbit - 1]);
}

void BC7IndexErrors::init()
{
	memset(err, 0, sizeof(err));
}

struct BC7Results
{
	enum { MAX_RESULTS = 64 };

	struct Result
	{
		BC7Error heap_err; // slot->err is definitive, this is a copy ; use get_heap_err
		BC7BlockState *slot;
		// heap_err includes mpeb bias, slot->err does not
		// heap_err == get_heap_err(slot);

		bool operator <(const Result &x) const
		{
			return heap_err < x.heap_err;
		}
	};

	// heap[0] contains the current worst error (root of max heap)
	//	best_err tracks the current best (lowest) error
	BC7Error best_err;
	int heap_size; // capacity
	int used; // number of slots actually used
	Result heap[MAX_RESULTS]; // max-heap of current states
	BC7BlockState pool[MAX_RESULTS];
	const BC7_Mode_ErrBias * mpeb; // optional bias for get_heap_err

	BC7Results(const BC7_Mode_ErrBias * in_mpeb)
		: best_err(BC7_ERROR_MAX),
		heap_size(MAX_RESULTS),
		used(0),
		mpeb(in_mpeb)
	{
		// initialize heap with all the free slots
		for (int i = 0; i < heap_size; ++i)
		{
			heap[i].heap_err = BC7_ERROR_MAX;
			heap[i].slot = &pool[i];
			pool[i].err = BC7_ERROR_MAX;
		}
	}

	// BC7BlockState->err is the SSD
	//	"heap_err" used for scoring blocks adds ModePart_ErrBias
	BC7Error get_heap_err(const BC7BlockState *st)
	{
		BC7Error err = st->err;
		if ( mpeb )
		{
			RR_ASSERT( err != BC7_ERROR_MAX );
			err += mpeb->ssd_err_bias[st->mode]; //[st->p];
		}
		return err;
	}

	// NOTE only used during the first phase where you add new states
	// once you get into refinement, we don't keep the heap current
	void add_state(const BC7BlockState *st) // st is copied
	{
		TRACE("add_state(err=%d,mode=%d,p=%d,rbit=%d,isbit=%d)\n", st->err, st->mode, st->p, st->subsets[0].rbit, st->subsets[0].isbit);

		// keep track of current best_err
		//	only used for checking best_err == 0 to early out for perfect blocks
		if(st->err < best_err)
			best_err = st->err;

		BC7Error heap_err = get_heap_err(st);

		// if we're not full, we can just insert immediately
		if (used < heap_size)
		{
			heap[used].heap_err = heap_err;
			*heap[used].slot = *st;

			if (++used == heap_size)
				make_heap(heap, heap + used);

			return;
		}

		// we're full, need to check whether the new state is better
		// than our current worst
		RR_ASSERT(heap_size > 0 && used == heap_size);

		// if not an improvement over current worst, bail
		if (heap_err >= heap[0].heap_err)
			return;

		// new state replaces current worst
		heap[0].heap_err = heap_err;
		*heap[0].slot = *st;

		// now root may be in wrong place, fix heap
		adjust_heap(heap, 0, used, stdless<Result>());
	}

	void shrink(int new_num)
	{
		RR_ASSERT(new_num <= heap_size);

		// not much to do if we're already below the threshold
		if (used <= new_num)
		{
			heap_size = new_num;
			return;
		}

		// this is done after refinement steps which means the "err" values
		// in our heap are all out of date; update them!
		for (int i = 0; i < used; ++i)
			heap[i].heap_err = get_heap_err(heap[i].slot);

		// set up a heap of the target size and loop over existing items,
		// replacing whenever one beats the current worst
		make_heap(heap, heap + new_num);

		for (int i = new_num; i < used; ++i)
		{
			// if at least as bad as current worst, ignore
			if (heap[i].heap_err >= heap[0].heap_err)
				continue;

			// heap[i] beats current worst, keep it
			// swap so we don't "leak" slots
			swap(heap[0], heap[i]);

			// now root may be in wrong place, fix heap
			adjust_heap(heap, 0, new_num, stdless<Result>());
		}

		heap_size = new_num;
		used = new_num;
	}

	// has_mode : do ANY of the current top results in the heap use mode "mode" ?
	//	used as a heuristic to guess if that type of mode was useful
	bool has_mode(int mode) const
	{
		for(int i = 0; i < used; ++i)
			if(heap[i].slot->mode == mode)
				return true;
		return false;
	}
};

// ---- optimal single-color fit

struct BC7OptimalEndpoints
{
	BC7Error error;
	BC7Color endpoints[2];
	U8 ind; // index value to use
};

BC7OptimalEndpoints find_optimal_endpoints_for_constant_color(int mode, const U8 color[4], bool alpha_enabled)
{
	static const int max_constant_index[5] = { 3,3,1,1,1 };
	static const int num_pbit_values[5]    = { 4,2,1,4,4 };
	BC7OptimalEndpoints best = { BC7_ERROR_MAX };

	// only supported in 2-subset modes, 1-subset we do more directly
	RR_ASSERT((mode >= 0 && mode <= 3) || mode == 7);

	// remap modes to contiguous numbering (with excluded 4-6 removed)
	if (mode == 7)
		mode = 4;

	// search for the optimal combination of index and p bits
	for (int p = 0; p < num_pbit_values[mode]; ++p)
	{
		for (U8 i = 1; i <= max_constant_index[mode]; ++i)
		{
			// radtex_bc7_optimal_endpoints_new[mode][p_bits][index-1][256]
			const BC7OptimalEndpoint *ep = radtex_bc7_optimal_endpoints_new[mode][p][i-1];
			BC7Error error = 0;
			for (int c=0; c < 3; ++c)
				error += ep[color[c]].err;
			if (alpha_enabled)
				error += ep[color[3]].err;

			if (error < best.error)
			{
				best.error = error;
				for (int c=0; c < 4; ++c)
				{
					best.endpoints[0].v[c] = ep[color[c]].lo;
					best.endpoints[1].v[c] = ep[color[c]].hi;
				}
				best.ind = i;
			}
		}
	}

	return best;
}

// ---- endpoint quantization

template<int t_nbits>
static inline U8 quant_endpoint(int val)
{
	const U32 range = 1u << t_nbits;
	const U32 recip255 = 0x8081; // enough bits for our value range
	const int postscale = (0x10000 >> t_nbits) + (0x10000 >> (t_nbits*2));

	// The reconstruction of the quantized value to float is quant / prescale,
	// i.e. a uniform scalar quantizer; the optimal quantizer for that is
	//   round(flt_val * prescale) = floor(flt_val * prescale + 0.5)
	// so do that in fixed point.

	// uniform quantizer to narrower bit depth
	//U32 quant = (val * prescale + 128) / 255;
	U32 prescaled = val * ((range - 1) * recip255);
	U32 quant = (prescaled + 128*recip255) >> 23;

	// dequantize back to 8 bits
	return static_cast<U8>((quant * postscale) >> 8);
}

template<int t_nbits>
static inline void quant_endpoint_with_pbit(U8 *deq0, U8 *deq1, int val)
{
	const int expanded_nbits = t_nbits + 1;
	const U32 range = 1u << expanded_nbits;
	const U32 recip255 = 0x8081; // enough bits for our value range
	const int postscale = (0x10000 >> t_nbits) + (0x10000 >> (t_nbits*2 + 1));

	// The reconstruction here adds the pbit as the lowest bit and then reconstructs
	// it as a (nbits+1)-bit value to float, i.e. (quant*2 + pbit) / (range - 1).
	// Consider the two cases separately:
	//   pbit=0  reconstructs (quant*2) / (range-1) = quant / ((range-1) / 2)
	//   pbit=1  reconstructs (quant*2+1) / (range-1) = quant / ((range-1) / 2) + 1/(range-1)
	//
	// the former is a uniform quantizer with a step size of 0.5 / (range - 1)
	// -> quantize for that with the usual 1/2 rounding bias (see above in quant_endpoint).
	//
	// the latter is biased by 1/(range-1) which works out to needing a 0 rounding bias
	// (i.e. truncating).
	//
	// "quant" here is t_nbits wide; we then expand with the p-bit value in the
	// right place.

	// The math for quant here is
	//   quantP = (val * (range - 1) + (p == 0 ? 254 : 0)) / 510
	// except we use a sufficient-precision reciprocal (using that val*(range-1) + bias
	// fits in 16 bits). In this scalar version we fuse the mul by (range-1)*recip255
	// into one larger constant, in the SIMD version we keep them separate since two
	// 16x16 multiplies (one low half, one high half) are much cheaper than going to
	// 32 bits.
	U32 prescaled = val * ((range - 1) * recip255);
	U32 quant0 = (prescaled + 254*recip255) >> 24; // quant for pbit=0
	U32 quant1 = prescaled >> 24; // quant for pbit=1

	// dequantize back to 8 bits
	*deq0 = static_cast<U8>((quant0 * postscale) >> 8);
	*deq1 = static_cast<U8>(((quant1 * postscale) >> 8) | (128 >> t_nbits));
}

#if defined(DO_BUILD_SSE4)

static RAD_ALIGN(const S64, g_pbit_merge_table[4][4], 16) =
{
	{  0,  0 },
	{ -1,  0 },
	{  0, -1 },
	{ -1, -1 },
};

static RADFORCEINLINE Vec128 dot16x16_4x(Vec128_S16r v0_0, Vec128_S16r v0_1, Vec128_S16r v1_0, Vec128_S16r v1_1)
{
	Vec128_S32 dot0 = v0_0.madd(v1_0); // a.x*b.x + a.y*b.y, a.z*b.z + a.w*b.w (4x)
	Vec128_S32 dot1 = v0_1.madd(v1_1);
	Vec128_S32 even = shuffle_two<0,2,0,2>(dot0, dot1);
	Vec128_S32 odd = shuffle_two<1,3,1,3>(dot0, dot1);
	return even + odd;
}

// We previously cleared the (original input) alpha channel to 255 (during init);
// now force our endpoint interpretation to 255 which means we can hit
// these values exactly and incur 0 error from them. (Related: in ab==0 modes,
// our error calc relies on endpoints being all-255).
//
// Note that if we have rbits, the channel containing the original input alpha
// is not necessarily channel 3!
//
// Mode without rbits always have st->rbit = 0 which does the right thing.
static RADFORCEINLINE Vec128_U16 endpoint_ignore_alpha(Vec128_U16r endpoints16, int rbit)
{
	static RAD_ALIGN(const U16, alpha_ignore_table[4][8], 16) =
	{
		{ 0x00,0x00,0x00,0xff, 0x00,0x00,0x00,0xff }, // rbit==0
		{ 0xff,0x00,0x00,0x00, 0xff,0x00,0x00,0x00 }, // rbit==1
		{ 0x00,0xff,0x00,0x00, 0x00,0xff,0x00,0x00 }, // rbit==2
		{ 0x00,0x00,0xff,0x00, 0x00,0x00,0xff,0x00 }, // rbit==3
	};

	return endpoints16 | Vec128_U16::loada(alpha_ignore_table[rbit]);
}

#endif

// calc_endpoints_and_pbits
// read st->endpoints
// quantize to color/alpha bits 
// find best pbits (if not passed in, or -1)
// Returns new pbits
// does not store in [st]
template<typename Mode>
static RADFORCEINLINE U8 calc_endpoints_and_pbits(BC7Color endpoints_q[2], const BC7SubsetState *st, BC7Flags flags, int pbits)
{
	const int cb = Mode::cb;
	const int ab = Mode::ab;
	U8 result = 0;

	// CB note : 
	//  rbit and pbits are mutually exclusive
	// if you have rbit, you have ab, and ib2, and no pbits
	// if you have ib2, you never have pbits
	// you *can* have pbits and ab, but not with ib2 or rbit, just 4-component vector endpoints

#if defined(DO_BUILD_SSE4)
	const Vec128_U16 recip255 = Vec128_U16(0x8081); // ceil(2^16 * 128 / 255); accurate enough for a full 16 bits
	Vec128_U16 endpoints16 = Vec128_U8::loadu_lo64(st->endpoints).to_uint16_lo();

	if (Mode::epb || Mode::spb)
	{
		const int cpb = 256 >> (cb + 1);
		const int apb = 256 >> (ab + 1);

		// Follow the explanation in quant_endpoint_with_pbit above
		const U16 quant_cb = (2 << cb) - 1;
		const U16 quant_ab = (2 << ab) - 1;
		const U16 dequant_cb = ((0x10000 >> cb) + (0x10000 >> (2*cb + 1)));
		const U16 dequant_ab = (ab != 0) ? ((0x10000 >> ab) + (0x10000 >> (2*ab + 1))) : 0;

		const Vec128_U16 quant_scale   = Vec128_U16::repeat4(quant_cb,  quant_cb,  quant_cb,  quant_ab);
		const Vec128_U16 dequant_scale = Vec128_U16::repeat4(dequant_cb,dequant_cb,dequant_cb,dequant_ab);
		const Vec128_U16 pbit_value = Vec128_U16::repeat4(cpb,cpb,cpb,apb);
		const Vec128_U16 himask = Vec128_U16(0xff00);

		// Quantize two ways, once assuming pbit=0 and once assuming pbit=1
		Vec128_U16 endpoint1_prediv	= endpoints16 * quant_scale; // pbit=1 value has bias of 0
		Vec128_U16 endpoint0_prediv = endpoint1_prediv + Vec128_U16(254); // pbit=0 value has bias of 254
		Vec128_U16 quant0			= endpoint0_prediv.mulhi(recip255) & himask;
		Vec128_U16 quant1			= endpoint1_prediv.mulhi(recip255) & himask;
		// quantX is now 256 * (endpoiintX_prediv / 510)

		// Dequant, add the pbit in
		Vec128_U16 dequant0			= quant0.mulhi(dequant_scale);
		Vec128_U16 dequant1			= quant1.mulhi(dequant_scale) | pbit_value;

		if ((ab == 0) || (flags & BC7ENC_IGNORE_ALPHA))
		{
			dequant0 = endpoint_ignore_alpha(dequant0, st->rbit);
			dequant1 = endpoint_ignore_alpha(dequant1, st->rbit);
		}

		// Generate p-bits
		if (pbits == -1)
		{
			// Calculate squared errors for both options
			Vec128_S16 diff0 = (dequant0 - endpoints16).s16();
			Vec128_S16 diff1 = (dequant1 - endpoints16).s16();

			Vec128_S32 sqerrs = Vec128_S32(dot16x16_4x(diff0, diff1, diff0, diff1));

			// With spb, add the errors for the two endpoints together, so we make a joint
			// decision.
			if (Mode::spb)
				sqerrs += sqerrs.yxwz();

			Vec128_S32 pbit1_sqerr = sqerrs.shuf<2,3,2,3>();
			Vec128_S32 usepbit = sqerrs.cmp_gt(pbit1_sqerr); // NOTE: high two lanes compare sqerr.zw against themselves so always 0
			result = static_cast<U8>(usepbit.movemask());

			// We've picked the pbits to use, so select the final endpoints
			Vec128_S32 merge_mask = usepbit.unpack_lo(usepbit);
			endpoints16 = vblend(dequant0, dequant1, merge_mask);
		}
		else
		{
			// Just select the endpoints directly
			result = static_cast<U8>(pbits);
			endpoints16	= vblend(dequant0, dequant1, load128a(g_pbit_merge_table[pbits]));
		}
	}
	else
	{
		const U16 quant_cb = (1 << cb) - 1;
		const U16 quant_ab = (1 << ab) - 1;
		const U16 dequant_cb = ((0x20000 >> cb) + (0x20000 >> (2*cb)));
		const U16 dequant_ab = (ab != 0) ? ((0x20000 >> ab) + (0x20000 >> (2*ab))) : 0;
		const Vec128_U16 quant_scale   = Vec128_U16::repeat4(quant_cb,  quant_cb,  quant_cb,  quant_ab);
		const Vec128_U16 dequant_scale = Vec128_U16::repeat4(dequant_cb,dequant_cb,dequant_cb,dequant_ab);

		// Quantize
		Vec128_U16 endpoint_prediv  = endpoints16 * quant_scale + Vec128_U16(128);
		Vec128_U16 quant			= endpoint_prediv.mulhi(recip255) & Vec128_U16(0xff80);
		// quant is now 128 * (endpoint_prediv / 255)

		// Dequant
		endpoints16 				= quant.mulhi(dequant_scale);

		if ((ab == 0) || (flags & BC7ENC_IGNORE_ALPHA))
			endpoints16 = endpoint_ignore_alpha(endpoints16, st->rbit);
	}

	// Pack and write
	endpoints16.to_uint8_sat().storeu_lo64(endpoints_q);
#else
	if (Mode::epb || Mode::spb)
	{
		BC7Color candidates[2][2];
		int errs[2][2] = {};
		for (int e = 0; e < 2; ++e)
		{
			for (int c = 0; c < 3; ++c)
			{
				quant_endpoint_with_pbit<cb>(&candidates[0][e].v[c], &candidates[1][e].v[c], st->endpoints[e].v[c]);
				errs[0][e] += radtex_sqr(candidates[0][e].v[c] - st->endpoints[e].v[c]);
				errs[1][e] += radtex_sqr(candidates[1][e].v[c] - st->endpoints[e].v[c]);
			}

			if (ab != 0)
			{
				quant_endpoint_with_pbit<ab>(&candidates[0][e].a, &candidates[1][e].a, st->endpoints[e].a);
				errs[0][e] += radtex_sqr(candidates[0][e].a - st->endpoints[e].a);
				errs[1][e] += radtex_sqr(candidates[1][e].a - st->endpoints[e].a);
			}
			else
			{
				candidates[0][e].a = 255;
				candidates[1][e].a = 255;
				// not keeping track of the error here since it's the same for both options so
				// it doesn't influence our decision
			}
		}

		if (pbits == -1)
		{
			// epbits can choose separately, spbits must choose jointly
			if (Mode::epb)
			{
				pbits  = (errs[1][0] < errs[0][0]) ? 1 : 0;
				pbits |= (errs[1][1] < errs[0][1]) ? 2 : 0;
			}
			else
				pbits = (errs[1][0] + errs[1][1] < errs[0][0] + errs[0][1]) ? 3 : 0;
		}

		// update endpoints based on choice
		endpoints_q[0] = candidates[pbits & 1][0];
		endpoints_q[1] = candidates[pbits >> 1][1];
		result = static_cast<U8>(pbits);
	}
	else
	{
		for (int c = 0; c < 3; ++c)
		{
			endpoints_q[0].v[c] = quant_endpoint<cb>(st->endpoints[0].v[c]);
			endpoints_q[1].v[c] = quant_endpoint<cb>(st->endpoints[1].v[c]);
		}
		if (ab != 0)
		{
			endpoints_q[0].a = quant_endpoint<ab>(st->endpoints[0].a);
			endpoints_q[1].a = quant_endpoint<ab>(st->endpoints[1].a);
		}
		else
		{
			endpoints_q[0].a = 255;
			endpoints_q[1].a = 255;
		}
	}

	// if ab == 0 it should be 255 already
	if (flags & BC7ENC_IGNORE_ALPHA)
	{
		// We previously cleared the (original input) alpha channel to 255 (during init);
		// now force our endpoint interpretation to 255 which means we can hit
		// these values exactly and incur 0 error from them.
		//
		// Note that if we have rbits, the channel containing the original input alpha
		// is not necessarily channel 3!
		//
		// Mode without rbits always have st->rbit = 0 which does the right thing.
		int c = (st->rbit - 1) & 3;
		endpoints_q[0].v[c] = 255;
		endpoints_q[1].v[c] = 255;
	}
#endif

	return result;
}

// ---- index finding

template <typename Mode>
static void calc_lerp_bc7(int isbit, int colors[16][4], const BC7Color ep[2])
{
	int ib = Mode::ib;
	int ib2 = Mode::ib2;

	const int comp1 = ib2 ? 3 : 4;
	if(Mode::isb && isbit) radtex_swap(ib,ib2);

	for (int c = 0; c < comp1; ++c)
	{
		// NOTE(fg): This is exact!
		int base = ep[0].v[c] * 64 + 32;
		int diff = ep[1].v[c] - ep[0].v[c];
		for (int i = 0; i < (1<<ib); ++i)
			colors[i][c] = (base + radtex_lerp_factor[ib][i] * diff) >> 6;
	}

	if (ib2)
	{
		int base = ep[0].a * 64 + 32;
		int diff = ep[1].a - ep[0].a;
		for (int i = 0; i < (1<<ib2); ++i)
			colors[i][3] = (base + radtex_lerp_factor[ib2][i] * diff) >> 6;
	}
}

// Channel rotation swaps channels around, but our alpha channel weighting is applied
// to the actual alpha channel in the source data, so we need to figure out what
// channel that ends up in for the BC7SubsetInput.
static int alpha_chan_index(int rbit)
{
	return (3 + rbit) & 3;
}

static int alpha_chan_index(const BC7SubsetState *st)
{
	return alpha_chan_index(st->rbit);
}

template <typename Mode>
static void scalar_calc_indexes_exact(BC7SubsetState *st, const BC7SubsetInput *in, const BC7Color endpoints_q[2])
{
	const int ib = Mode::ib;
	const int ib2 = Mode::ib2;
	const int isb = Mode::isb;

	int colors[16][4];
	calc_lerp_bc7<Mode>(st->isbit, colors, endpoints_q);

	BC7Error vec_err = 0;
	BC7Error scalar_err = 0;

	unsigned ib_ = ib, ib2_ = ib2;
	if (isb && st->isbit) radtex_swap(ib_, ib2_);
	const int num_lerp = 1<<ib_;
	const int num_lerp_alpha = 1<<ib2_;

	U8 * idxs  = st->idxs[st->isbit].ind;
	U8 * idxs2 = st->idxs[st->isbit ^ 1].ind;

	for (int i = 0; i < in->num_pixels; ++i)
	{
		const U8 *rgba = in->pixels + i*4;
		
		{
			unsigned best = 0;
			BC7Error best_e;
			best_e  = radtex_sqr(rgba[0] - colors[0][0]) * in->channel_weights[0];
			best_e += radtex_sqr(rgba[1] - colors[0][1]) * in->channel_weights[1];
			best_e += radtex_sqr(rgba[2] - colors[0][2]) * in->channel_weights[2];
			if (ib2 == 0) best_e += radtex_sqr(rgba[3] - colors[0][3]) * in->channel_weights[3];
			for (int j = 1; j < num_lerp; ++j)
			{
				BC7Error e;
				e  = radtex_sqr(rgba[0] - colors[j][0]) * in->channel_weights[0];
				e += radtex_sqr(rgba[1] - colors[j][1]) * in->channel_weights[1];
				e += radtex_sqr(rgba[2] - colors[j][2]) * in->channel_weights[2];
				if (ib2 == 0) e += radtex_sqr(rgba[3] - colors[j][3]) * in->channel_weights[3];
				if (e < best_e)
				{
					best_e = e;
					best = j;
				}
			}
			vec_err += best_e;
			idxs[i] = static_cast<U8>(best);
		}
		if (ib2)
		{
			// NOTE(fg): we have a single channel here so we can apply weighting once at the end
			unsigned best = 0;
			BC7Error best_e = radtex_sqr(rgba[3] - colors[0][3]);
			for (int j = 1; j < num_lerp_alpha; ++j)
			{
				unsigned e = radtex_sqr(rgba[3] - colors[j][3]);
				if (e < best_e)
				{
					best_e = e;
					best = j;
				}
			}
			idxs2[i] = static_cast<U8>(best);
			scalar_err += best_e;
		}
	}

	// apply deferred weighting of scalar channel
	scalar_err *= in->channel_weights[3];

	st->err = vec_err + scalar_err;
	st->scalar_err = scalar_err;
}

#if defined(DO_BUILD_SSE4)

#define F(x) ((x)*-512)

static RAD_ALIGN(const S16, radtex_lerp_factor_neg512x[3][16], 4) = // [ib-2]
{
	{F(0), F(21), F(43), F(64)},
	{F(0), F( 9), F(18), F(27), F(37), F(46), F(55), F(64)},
	{F(0), F( 4), F( 9), F(13), F(17), F(21), F(26), F(30), F(34), F(38), F(43), F(47), F(51), F(55), F(60), F(64)},
};

#undef F

// NOTE: not the same as regular calc_lerp_bc7. We have a different result format
// and the way the IBs are plumbed in is also different.
//
// colors are [index][channel]
template <int t_ib, bool t_zero_alpha>
static void sse4_calc_lerpvec_bc7(S16 colors[16][4], const BC7Color endpoints_q[2])
{
	RR_ASSERT(t_ib >= 2 && t_ib <= 4);
	if ( t_ib < 2 ) return;

	Vec128_U8 endpoints8 = Vec128_U8::loadu_lo64(&endpoints_q[0]);
	if (t_zero_alpha)
		endpoints8 &= Vec128_U8::repeat4(0xff, 0xff, 0xff, 0);
	Vec128_S16 endpoints16 = endpoints8.to_sint16_lo();

	Vec128_S16 lo16 = endpoints16.dup_lo();
	Vec128_S16 hi16 = endpoints16.dup_hi();
	Vec128_S16 diff16 = lo16 - hi16; // yes, lo-hi not hi-lo!

	const S16 * factors = radtex_lerp_factor_neg512x[t_ib-2];
	const int num_lerp = 1 << t_ib;
	for (int i = 0; i < num_lerp; i += 2)
	{
		Vec128_S16 twofac = Vec128_S16::loadu_lo32(factors + i);
		// TODO(djg): This is twofac.xxxxyyyy().
		Vec128_S16 lerpf = Vec128_S16(_mm_shuffle_epi8(twofac, Vec128_U8(0,1, 0,1, 0,1, 0,1, 2,3, 2,3, 2,3, 2,3)));

		// interpolation:
		//   rounding_shift_right(lerpf * (lo16 - hi16), 15) + lo
		Vec128_S16 mult = lerpf.mulhrs(diff16);
		Vec128_S16 interp = mult + lo16;

		interp.storea(&colors[i][0]);
	}
}

template <int t_ib>
static inline Vec128 sse4_calc_lerpscalar_bc7(const BC7Color endpoints_q[2])
{
	RR_ASSERT(t_ib >= 2 && t_ib <= 3);
	if ( t_ib < 2 ) return Vec128();

	Vec128_S16 lo16 = Vec128_S16(endpoints_q[0].a);
	Vec128_S16 diff16 = Vec128_S16(endpoints_q[0].a - endpoints_q[1].a); // yes, lo-hi not hi-lo!

	Vec128_S16 lerpf = Vec128_S16::loadu(radtex_lerp_factor_neg512x[t_ib-2]);
	// interpolation:
	//   rounding_shift_right(lerpf * (lo16 - hi16), 15) + lo
	Vec128_S16 mult = lerpf.mulhrs(diff16);
	Vec128_S16 interp = mult + lo16;
	return interp;
}

template <int t_ib, bool t_zero_alpha>
static BC7Error sse4_calc_vector_inds_exact(BC7Inds *inds, const BC7SubsetInput *in, const BC7Color endpoints_q[2])
{
	const int num_lerp = 1 << t_ib;

	RAD_ALIGN(S16, colors[16][4], 16);
	sse4_calc_lerpvec_bc7<t_ib, t_zero_alpha>(colors, endpoints_q);

	Vec128_S32 sum_err = Vec128_S32::zero();
	Vec128_S32 last_err = Vec128_S32::zero();
	U8 * idxs = inds->ind;
	int i;

	Vec128_S16 chan_weight16 = Vec128_S16::loadu_lo64(in->channel_weights).dup_lo();
	if (t_zero_alpha) // make alpha weight zero if desired
		chan_weight16 &= Vec128_S16::repeat4(-1,-1,-1,0);

	for (i = 0; i < in->num_pixels; i += 4)
	{
		// load pixels and zext to 16 bits
		Vec128_U8 pix8 = Vec128_U8::loadu(in->pixels + i*4);

		Vec128_S16 pix16_01 = pix8.to_sint16_lo();
		Vec128_S16 pix16_23 = pix8.to_sint16_hi();
		Vec128_S32 j32 = Vec128_S32::zero();

		Vec128_S32 best = Vec128_S32(0x7ffffff0);
		for (int j = 0; j < num_lerp; ++j)
		{
			// Load color and broadcast it
			Vec128_S16 col = Vec128_S16::loadu_lo64(&colors[j][0]).dup_lo();

			// Calc the diffs
			Vec128_S16 diffs01 = col - pix16_01;
			Vec128_S16 diffs23 = col - pix16_23;

			// Apply weighting
			Vec128_S16 diffs01w = diffs01 * chan_weight16;
			Vec128_S16 diffs23w = diffs23 * chan_weight16;

			// dr*dr*wr + dg*dg*wg + db*db*wb + da*da*wa
			Vec128_S32 dots = Vec128_S32(dot16x16_4x(diffs01, diffs23, diffs01w, diffs23w));

			// then put the index in the low bits and reduce over it
			Vec128_S32 e = dots.shl<4>() + j32;
			best = vmin(best, e);
			j32 += Vec128_S32(1);
		}

		// Emit best indices
		Vec128_U8 vinds = (best & Vec128_S32(0xf)).gather_xyzw_lo8();
		vinds.storeu_lo32(idxs + i);

		// Sum errors
		last_err = best.srl<4>();
		sum_err += last_err;
	}

	// In the last iteration of the loop, we added some errors that we shouldn't have;
	// undo this now.
	Vec128_S32 wrong_errs = last_err.andnot(Vec128_S8::loadu_lo32(in->count_mask + i - 4).to_s32_lo());
	sum_err -= wrong_errs;

	// Final error reduction
	return reduce_add_s32(sum_err);
}

template <int t_ib>
static BC7Error sse4_calc_scalar_inds_exact(BC7Inds *inds, const BC7SubsetInput *in, const BC7Color endpoints_q[2])
{
	RR_ASSERT(t_ib <= 3); // not a compiler error because we still get instantiated inside dead ifs
	const int num_lerp = 1 << t_ib;

	Vec128_S16 colors = Vec128_S16(sse4_calc_lerpscalar_bc7<t_ib>(endpoints_q));

	// Make space for the index
	colors = colors.shl<4>();

	Vec128_U32 sum_err = Vec128_U32::zero();
	U8 * idxs = inds->ind;
	int i;

	for (i = 0; i < in->num_pixels; i += 8)
	{
		// Load 8 pixels worth of alpha and wrangle to 8 lanes of 16 bits
		Vec128_U8 pix8_0 = Vec128_U8::loadu(in->pixels + i*4 +  0);
		Vec128_U8 pix8_1 = Vec128_U8::loadu(in->pixels + i*4 + 16);
		Vec128_S32 pix8a_0 = pix8_0.s32().srl<24>();
		Vec128_S32 pix8a_1 = pix8_1.s32().srl<24>();

		Vec128_S16 pix16a = pix8a_0.to_s16_sat(pix8a_1);

		// Make space for the index
		pix16a = pix16a.shl<4>();

		Vec128_S16 best = Vec128_S16(0x100 << 4);
		Vec128_S16 j16 = Vec128_S16::zero();
		Vec128_U8 col_shuffle = Vec128_U8::repeat2(0,1);

		for (int j = 0; j < num_lerp; ++j)
		{
			// Broadcast the color
			Vec128_S16 col_bcast = colors.shuf(col_shuffle);

			// Calc absolute difference
			// (can use SAD not SSD here since they're equivalent for single channel)
			Vec128_S16 diff = col_bcast - pix16a;
			Vec128_S16 absdiff = diff.abs();

			// Stuff the index in and do the min reduction
			Vec128_S16 e = absdiff + j16;
			best = vmin(best, e);

			col_shuffle += Vec128_U8(2);
			j16 += Vec128_S16(1);
		}

		// Emit best indices
		Vec128_S16 vinds = best & Vec128_S16(0xf);
		vinds.to_uint8_sat().storeu_lo64(idxs + i);

		// Extract the errors, square and sum them
		Vec128_S16 errs = best.srl<4>();
		Vec128_S16 masked_errs = errs & Vec128_U8::loadu_lo64(in->count_mask + i).to_sint16_lo();
		sum_err += masked_errs.madd(masked_errs).u32();
	}

	// Final error reduction and weighting
	return reduce_add_s32(sum_err) * in->channel_weights[3];
}

#endif

template <typename Mode>
static void calc_indexes_exact(BC7SubsetState *st, const BC7SubsetInput *in, const BC7Color endpoints_q[2])
{
#if defined(DO_BUILD_SSE4)
	// NOTE(fg): this is supposed to match the results of the scalar version _exactly_
	// if it doesn't, that's a bug!
	if (Mode::ib2 == 0)
	{
		st->err = sse4_calc_vector_inds_exact<Mode::ib, false>(&st->idxs[0], in, endpoints_q);
		st->scalar_err = 0;
		return;
	}
	else
	{
		BC7Error vec_err, scalar_err;
		if (!Mode::isb || !st->isbit)
		{
			vec_err = sse4_calc_vector_inds_exact<Mode::ib, true>(&st->idxs[0], in, endpoints_q);
			scalar_err = sse4_calc_scalar_inds_exact<Mode::ib2>(&st->idxs[1], in, endpoints_q);
		}
		else
		{
			vec_err = sse4_calc_vector_inds_exact<Mode::ib2, true>(&st->idxs[1], in, endpoints_q);
			scalar_err = sse4_calc_scalar_inds_exact<Mode::ib>(&st->idxs[0], in, endpoints_q);
		}

		st->err = vec_err + scalar_err;
		st->scalar_err = scalar_err;
	}
#else
	scalar_calc_indexes_exact<Mode>(st, in, endpoints_q);
#endif
}

// Given a BC7SubsetState that was produced without constraints, make sure the anchor bit
// at anchor_pos is 0 without swapping the endpoints.
template <typename Mode>
static void force_anchor_bit_zero(BC7SubsetState *st, const BC7SubsetInput *in, const BC7Color endpoints_q[2], int anchor_pos)
{
	RR_ASSERT(anchor_pos >= 0);

	// When an index has the anchor bit set, the right move (in a MSE sense) _always_ is
	// to clamp that index to the largest value with the anchor bit not set. That is because
	// the entire half of the index space with the anchor bit set is closer to endpoint 1
	// than endpoint 0 (corresponding to a half-space in RGB as well), and the cloest we
	// can get to that half-space is by making the index as large as possible.

	const U8 * rgba = in->pixels + anchor_pos * 4;

	// NOTE: can optimize this later but for now, keep it simple.

	// Vector channels
	int ib_v = st->isbit ? Mode::ib2 : Mode::ib;
	U8 anchor_bit_v = 1 << (ib_v - 1);
	U8 * idx_v = st->idxs[st->isbit].ind;

	if (idx_v[anchor_pos] & anchor_bit_v)
	{
		const int comp = (Mode::ib2 != 0) ? 3 : 4;

		// Clamp the index to the maximum we can use
		int f_old = radtex_lerp_factor[ib_v][idx_v[anchor_pos]];
		int f_new = radtex_lerp_factor[ib_v][anchor_bit_v - 1];
		idx_v[anchor_pos] = anchor_bit_v - 1;

		// Update the error: subtract the old error and add the new one
		for (int c = 0; c < comp; ++c)
		{
			int base = 64 * endpoints_q[0].v[c] + 32;
			int diff = endpoints_q[1].v[c] - endpoints_q[0].v[c];
			int v_old = (base + f_old*diff) >> 6;
			int v_new = (base + f_new*diff) >> 6;

			st->err += (radtex_sqr(rgba[c] - v_new) - radtex_sqr(rgba[c] - v_old)) * in->channel_weights[c];
		}
	}

	if (Mode::ib2 != 0)
	{
		// Scalar channel
		int ib_s = st->isbit ? Mode::ib : Mode::ib2;
		U8 anchor_bit_s = 1 << (ib_s - 1);
		U8 * idx_s = st->idxs[st->isbit ^ 1].ind;

		if (idx_s[anchor_pos] & anchor_bit_s)
		{
			// Clamp the index to the maximum we can use
			int f_old = radtex_lerp_factor[ib_s][idx_s[anchor_pos]];
			int f_new = radtex_lerp_factor[ib_s][anchor_bit_s - 1];
			idx_s[anchor_pos] = anchor_bit_s - 1;

			// Update the error: subtract the old error and add the new one
			int base = 64 * endpoints_q[0].a + 32;
			int diff = endpoints_q[1].a - endpoints_q[0].a;
			int v_old = (base + f_old*diff) >> 6;
			int v_new = (base + f_new*diff) >> 6;

			BC7Error vec_err = st->err - st->scalar_err;
			st->scalar_err += (radtex_sqr(rgba[3] - v_new) - radtex_sqr(rgba[3] - v_old)) * in->channel_weights[3];
			st->err = vec_err + st->scalar_err;
		}
	}
}

#ifdef DO_BUILD_SSE4
template <typename Mode>
static void sse4_force_anchor_bit_zero(BC7SubsetState *st, const BC7SubsetInput *in, const BC7Color endpoints_q[2], int anchor_pos)
{
	RR_ASSERT(anchor_pos >= 0);

	#ifdef SCALAR_PARANOIA
	BC7SubsetState check_st = *st;
	force_anchor_bit_zero<Mode>(&check_st, in, endpoints_q, anchor_pos);
	#endif

	// When an index has the anchor bit set, the right move (in a MSE sense) _always_ is
	// to clamp that index to the largest value with the anchor bit not set. That is because
	// the entire half of the index space with the anchor bit set is closer to endpoint 1
	// than endpoint 0 (corresponding to a half-space in RGB as well), and the cloest we
	// can get to that half-space is by making the index as large as possible.

	Vec128_S32 chan_weight = Vec128_S16::loadu_lo64(in->channel_weights).to_sint32();
	if (Mode::ib2 != 0)
		chan_weight &= Vec128_S32(-1,-1,-1,0);

	Vec128_S32 rgba = Vec128_U8::loadu_lo32(in->pixels + anchor_pos * 4).to_sint32_lo();

	// NOTE: can optimize this later but for now, keep it simple.

	// Vector channels
	int ib_v = st->isbit ? Mode::ib2 : Mode::ib;
	U8 anchor_bit_v = 1 << (ib_v - 1);
	U8 * idx_v = st->idxs[st->isbit].ind;

	if (idx_v[anchor_pos] & anchor_bit_v)
	{
		// Clamp the index to the maximum we can use
		int f_old = radtex_lerp_factor[ib_v][idx_v[anchor_pos]];
		int f_new = radtex_lerp_factor[ib_v][anchor_bit_v - 1];
		idx_v[anchor_pos] = anchor_bit_v - 1;

		// Update the error: subtract the old error and add the new one
		Vec128_U8 endpoints8 = Vec128_U8::loadu_lo64(endpoints_q);
		// zext U8 to S32
		Vec128_S32 lo = Vec128_S32(endpoints8.shuf(Vec128_S32(-256 + 0, -256 + 1, -256 + 2, -256 + 3)));
		Vec128_S32 hi = Vec128_S32(endpoints8.shuf(Vec128_S32(-256 + 4, -256 + 5, -256 + 6, -256 + 7)));
		Vec128_S32 diff = hi - lo;

		Vec128_S32 base = lo.shl<6>() + Vec128_S32(32);

		Vec128_S32 v_old = (base + Vec128_S32(f_old) * diff).srl<6>();
		Vec128_S32 v_new = (base + Vec128_S32(f_new) * diff).srl<6>();

		st->err += reduce_add_s32((radtex_sqr(rgba - v_new) - radtex_sqr(rgba - v_old)) * chan_weight);
	}

	if (Mode::ib2 != 0)
	{
		// Scalar channel
		int ib_s = st->isbit ? Mode::ib : Mode::ib2;
		U8 anchor_bit_s = 1 << (ib_s - 1);
		U8 * idx_s = st->idxs[st->isbit ^ 1].ind;

		if (idx_s[anchor_pos] & anchor_bit_s)
		{
			// Clamp the index to the maximum we can use
			int f_old = radtex_lerp_factor[ib_s][idx_s[anchor_pos]];
			int f_new = radtex_lerp_factor[ib_s][anchor_bit_s - 1];
			idx_s[anchor_pos] = anchor_bit_s - 1;

			// Update the error: subtract the old error and add the new one
			int base = 64 * endpoints_q[0].a + 32;
			int diff = endpoints_q[1].a - endpoints_q[0].a;
			int v_old = (base + f_old*diff) >> 6;
			int v_new = (base + f_new*diff) >> 6;

			BC7Error vec_err = st->err - st->scalar_err;
			st->scalar_err += (radtex_sqr(rgba.extract<3>() - v_new) - radtex_sqr(rgba.extract<3>() - v_old)) * in->channel_weights[3];
			st->err = vec_err + st->scalar_err;
		}
	}

	#ifdef SCALAR_PARANOIA
	RR_ASSERT_ALWAYS(check_st.err == st->err);
	RR_ASSERT_ALWAYS(check_st.scalar_err == st->scalar_err);
	RR_ASSERT_ALWAYS(memcmp(&check_st.idxs[0], &st->idxs[0], 16) == 0);
	RR_ASSERT_ALWAYS(memcmp(&check_st.idxs[1], &st->idxs[1], 16) == 0);
	#endif
}
#endif

// linear approx, very fast.
template<typename Mode>
static void calc_indexes_linear_approx_scalar(BC7SubsetState *st, const BC7SubsetInput *in, const BC7Color endpoints_q[2])
{
	const int ib = Mode::ib;
	const int ib2 = Mode::ib2;
	const int isb = Mode::isb;

	int colors[16][4];
	calc_lerp_bc7<Mode>(st->isbit, colors, endpoints_q);

	const int ib1_comp = ib2 ? 3 : 4;

	unsigned ib_ = ib, ib2_ = ib2;
	if(isb && st->isbit) radtex_swap(ib_,ib2_);
	const int ib_mask = (1 << ib_) - 1;
	const int ib2_mask = (1 << ib2_) - 1;

	U8 * idxs  = st->idxs[st->isbit].ind;
	U8 * idxs2 = st->idxs[st->isbit ^ 1].ind;

	int bias[2], wdir[4];
	float scalef[2];
	int mindot = 0;
	int maxdot = 0;
	for (int c = 0; c < ib1_comp; ++c)
	{
		int lo = endpoints_q[0].v[c];
		int hi = endpoints_q[1].v[c];
		wdir[c] = (hi - lo) * in->channel_weights[c]; // apply weights to the dot product too
		mindot += lo * wdir[c];
		maxdot += hi * wdir[c];
	}
	scalef[0] = float(ib_mask) / float(maxdot - mindot);
	bias[0] = -mindot;

	if(ib2)
	{
		scalef[1] = float(ib2_mask) / float(endpoints_q[1].a - endpoints_q[0].a);
		if (endpoints_q[0].a == endpoints_q[1].a)
			scalef[1] = 0.0f;

		bias[1] = -endpoints_q[0].a;
	}

	BC7Error vec_err = 0;
	BC7Error scalar_err = 0;
	for (int i = 0; i < in->num_pixels; ++i)
	{
		const U8 *rgba = in->pixels + i*4;

		int dot = bias[0] + rgba[0]*wdir[0] + rgba[1]*wdir[1] + rgba[2]*wdir[2];
		if (ib2 == 0)
			dot += rgba[3]*wdir[3];

		// assume rounded index is best
		int q1 = radtex_round(dot * scalef[0]);
		q1 = radtex_clamp(q1, 0, ib_mask);
		idxs[i] = static_cast<U8>(q1);
		int *color = colors[q1];
		vec_err += radtex_sqr(rgba[0] - color[0]) * in->channel_weights[0];
		vec_err += radtex_sqr(rgba[1] - color[1]) * in->channel_weights[1];
		vec_err += radtex_sqr(rgba[2] - color[2]) * in->channel_weights[2];
		if(ib2 == 0)
			vec_err += radtex_sqr(rgba[3] - color[3]) * in->channel_weights[3];
		else
		{
			int q2 = radtex_round( (rgba[3] + bias[1]) * scalef[1] );
			q2 = radtex_clamp(q2, 0, ib2_mask);
			idxs2[i] = static_cast<U8>(q2);
			// NOTE: scalar values are single-channel, can defer weighting until the end
			scalar_err += radtex_sqr(rgba[3] - colors[q2][3]);
		}
	}

	scalar_err *= in->channel_weights[3];

	st->err = vec_err + scalar_err;
	st->scalar_err = scalar_err;
}

// calc_subset_error
//	does not change st->err
template<typename Mode>
static BC7Error calc_subset_error_scalar(const BC7SubsetState *st, const BC7SubsetInput *in, const BC7Color endpoints_q[2], BC7Error *out_scalar_err=0)
{
	int colors[16][4];
	calc_lerp_bc7<Mode>(st->isbit, colors, endpoints_q);

	BC7Error vec_err = 0;
	BC7Error scalar_err = 0;
	const U8 * idxs  = st->idxs[st->isbit].ind;
	const U8 * idxs2 = st->idxs[st->isbit ^ 1].ind;

	for (int i = 0; i < in->num_pixels; ++i)
	{
		const U8 *rgba = in->pixels + i*4;
		SINTa ind = idxs[i];
		vec_err += radtex_sqr(rgba[0] - colors[ind][0]) * in->channel_weights[0];
		vec_err += radtex_sqr(rgba[1] - colors[ind][1]) * in->channel_weights[1];
		vec_err += radtex_sqr(rgba[2] - colors[ind][2]) * in->channel_weights[2];
		if (Mode::ib2 == 0)
			vec_err += radtex_sqr(rgba[3] - colors[ind][3]) * in->channel_weights[3];
		else
		{
			// NOTE: scalar values are single-channel, can defer weighting until the end
			scalar_err += radtex_sqr(rgba[3] - colors[idxs2[i]][3]);
		}
	}

	scalar_err *= in->channel_weights[3];

	if (out_scalar_err)
		*out_scalar_err = scalar_err;

	return vec_err + scalar_err;
}

//RR_ASSERT( check_endpoints_q<Mode>(st,flags) );;
template<typename Mode>
static bool check_endpoints_q(const BC7SubsetState *st, BC7Flags flags)
{
	//BC7Color out_pixels_scalar[16];
	//decode_subset_scalar<Mode>(out_pixels_scalar,st,flags,num_pixels);
	
	BC7Color endpoints_q[2];
	U8 pbits = calc_endpoints_and_pbits<Mode>(endpoints_q, st, flags, st->pbits);
	RR_UNUSED_VARIABLE(pbits);
	RR_ASSERT(pbits == st->pbits);
	
	int cmp = memcmp(endpoints_q,st->endpoints_q,sizeof(endpoints_q));
	RR_ASSERT( cmp == 0 );
	
	return ( cmp == 0 );
}

// decodes _up to_ 16 pixels (actual count depends on num_pixels)
template<typename Mode>
static void decode_subset_scalar(BC7Color out_pixels[16], const BC7SubsetState *st, BC7Flags flags, int num_pixels)
{
	RR_ASSERT( check_endpoints_q<Mode>(st,flags) );
	
	int colors[16][4];
	calc_lerp_bc7<Mode>(st->isbit, colors, st->endpoints_q);

	const U8 * idxs1 = st->idxs[st->isbit].ind;
	const U8 * idxs2 = st->idxs[st->isbit ^ 1].ind;

	for (int i = 0; i < num_pixels; ++i)
	{
		SINTa ind = idxs1[i];
		out_pixels[i].r = U8_check( colors[ind][0] );
		out_pixels[i].g = U8_check( colors[ind][1] );
		out_pixels[i].b = U8_check( colors[ind][2] );

		if (Mode::ib2 == 0)
			out_pixels[i].a = U8_check( colors[ind][3] );
		else
			out_pixels[i].a = U8_check( colors[idxs2[i]][3] );
	}
}
	
// these match exactly between scalar and sse4
#if defined(DO_BUILD_SSE4)

static RAD_ALIGN(const F32, radtex_index_scale_table[3][4], 16) = // [ib-2], x4 for SIMD
{
	{  3.0f,  3.0f,  3.0f,  3.0f },
	{  7.0f,  7.0f,  7.0f,  7.0f },
	{ 15.0f, 15.0f, 15.0f, 15.0f },
};

static RAD_ALIGN(const S8, radtex_lerp_factor_neg2x[3][16], 16) = // [ib-2]
{
	{0, -42, -86, -128},
	{0, -18, -36, -54, -74, -92, -110, -128},
	{0, -8, -18, -26, -34, -42, -52, -60, -68, -76, -86, -94, -102, -110, -120, -128}
};

static BC7Error sse4_calc_scalar_inds_linear(U8 out_inds[16], const BC7SubsetInput *in, int ib, const BC7Color endpoints_q[2])
{
	// determine lo and hi endpoints
	const Vec128_S32 lo32 = Vec128_S32(endpoints_q[0].a);
	VecF32x4 index_scale = VecF32x4::loada(radtex_index_scale_table[ib-2]);
	VecF32x4 scalef(radtex_index_scale_table[ib-2][0] / (endpoints_q[1].a - endpoints_q[0].a));
	if (endpoints_q[0].a == endpoints_q[1].a)
		scalef = VecF32x4::zero();

	const Vec128_S16 diff16 = Vec128_S16(endpoints_q[0].a - endpoints_q[1].a);
	const Vec128_S8 weights_tab = Vec128_S8::loadu(radtex_lerp_factor_neg2x[ib-2]);
	Vec128_S32 total_err = Vec128_S32::zero();

	for (int i = 0; i < in->num_pixels; i += 8)
	{
		// grab the current set of pixels and extend to 32 bits
		// (using that "a" ends up in bits [31:24] of every 32-bit group)
		Vec128_S32 vals0 = Vec128_S32::loadu(in->pixels + i*4 + 0*4).srl<24>();
		Vec128_S32 vals1 = Vec128_S32::loadu(in->pixels + i*4 + 4*4).srl<24>();

		// compute un-lerp coeffs in 32-bit int
		Vec128_S32 rel0 = vals0 - lo32;
		Vec128_S32 rel1 = vals1 - lo32;

		// convert to float
		VecF32x4 rel0f = rel0.to_f32();
		VecF32x4 rel1f = rel1.to_f32();

		// determine the quantized index
		VecF32x4 q0f = vmin(vmax(rel0f * scalef, VecF32x4::zero()), index_scale);
		VecF32x4 q1f = vmin(vmax(rel1f * scalef, VecF32x4::zero()), index_scale);
		Vec128_S32 q0 = q0f.to_int32_round(); // NOTE rounding not truncating! important.
		Vec128_S32 q1 = q1f.to_int32_round();

		// pack into 16 bits
		Vec128_S16 rel16 = rel0.to_s16_sat(rel1);
		Vec128_S16 q16 = q0.to_s16_sat(q1);

		// store the indices
		q16.to_uint8_sat().storeu_lo64(out_inds + i);

		// look up the weights for q in the table
		Vec128_S16 weights_from_q = weights_tab.shuf(q16).s16();
		Vec128_S16 lerpfct = weights_from_q.shl<8>();

		// perform the interpolation to determine the error
		// we compute:
		//
		//   rounding_shift_right(lerpfct * (lo - hi), 15) - (pixel - lo)
		//
		// where lerpfct = -(bc7_weight[q] << 9)
		Vec128_S16 err16 = lerpfct.mulhrs(diff16) - rel16;

		// mask to take care of count
		err16 &= Vec128_S8::loadu_lo64(in->count_mask + i).to_s16_lo();

		// compute SSDs
		Vec128_S32 ssd = err16.madd(err16);
		total_err += ssd;
	}

	// reduce and apply weight!
	return reduce_add_s32(total_err) * in->channel_weights[3];
}

template<bool t_include_alpha>
static BC7Error sse4_calc_vector_inds_linear(U8 out_inds[16], const BC7SubsetInput *in, int ib, const BC7Color endpoints_q[2])
{
	const Vec128_S8 weights_tab = Vec128_S8::loadu(radtex_lerp_factor_neg2x[ib-2]);
	const VecF32x4 index_scale = VecF32x4::loada(radtex_index_scale_table[ib-2]);
	Vec128_U8 endpoints8 = Vec128_U8::loadu_lo64(&endpoints_q[0]);

	// Grab channel weights; if alpha ignored, set its weight to 0
	// (note this is "alpha" in rotated channel order for mode 4/5, which
	// does not necessarily line up with the actual image alpha channel!)
	Vec128_S16 chan_weight16 = Vec128_S16::loadu_lo64(in->channel_weights).dup_lo();
	if (!t_include_alpha) // make alpha weight zero if desired
		chan_weight16 &= Vec128_S16::repeat4(-1,-1,-1,0);

	// low/high endpoints
	VecF32x4 scalef;
	Vec128_S16 endpoints16 = endpoints8.to_sint16_lo();
	Vec128_S16 lo16 = endpoints16.dup_lo();
	Vec128_S16 hi16 = endpoints16.dup_hi();
	Vec128_S16 diff16 = lo16 - hi16;

	Vec128_S16 diff16_weighted = diff16 * chan_weight16;
	{
		Vec128_S32 dots = diff16.madd(diff16_weighted); // r+g, b+a, r+g, b+a dot products
		dots += dots.yxwz(); // 4x r+g+b+a
		scalef = -index_scale / dots.to_f32();
	}

	Vec128_S32 total_err = Vec128_S32::zero();
	Vec128_S32 last_err = Vec128_S32::zero();
	int i;
	for (i = 0; i < in->num_pixels; i += 4)
	{
		// load the pixels
		Vec128_U8 pixels = Vec128_U8::loadu(in->pixels + i*4);
		Vec128_S16 pixrel16_0 = pixels.to_sint16_lo() - lo16;
		Vec128_S16 pixrel16_1 = pixels.to_sint16_hi() - lo16;

		// compute the per-pixel dot products
		Vec128_S32 rgba_dots = Vec128_S32(dot16x16_4x(pixrel16_0, pixrel16_1, diff16_weighted, diff16_weighted));

		// determine the quantized index
		VecF32x4 qf = rgba_dots.to_f32() * scalef;
		qf = vmin(vmax(qf, VecF32x4::zero()), index_scale);
		Vec128_S32 q = Vec128_S32(qf.to_int32_round()); // NOTE rounding not truncating! important.

		// grab low 8 bits of each lane and store
		q.gather_xyzw_lo8().storeu_lo32(out_inds + i);

		// look up the weights for q in the table
		Vec128_S8 weights_from_q = weights_tab.shuf(q);

		// creates 4 copies of each of the weights as 16 bits, shifted by 8
		Vec128_S16 lerpf_0 = Vec128_S16(weights_from_q.shuf(Vec128_S8(-1,0,-1,0,-1,0,-1,0, -1,4,-1,4,-1,4,-1,4)));
		Vec128_S16 lerpf_1 = Vec128_S16(weights_from_q.shuf(Vec128_S8(-1,8,-1,8,-1,8,-1,8, -1,12,-1,12,-1,12,-1,12)));

		// perform the interpolation to determine the error
		// we compute:
		//
		//   rounding_shift_right(lerpf * (lo - hi), 15) - (pixel - lo)
		//
		// where lerpf = -(bc7_weight[q] << 9)
		Vec128_S16 err_0 = lerpf_0.mulhrs(diff16) - pixrel16_0;
		Vec128_S16 err_1 = lerpf_1.mulhrs( diff16) - pixrel16_1;

		// error calc with weighting
		Vec128_S16 errw_0 = err_0 * chan_weight16;
		Vec128_S16 errw_1 = err_1 * chan_weight16;

		last_err = Vec128_S32(dot16x16_4x(err_0, err_1, errw_0, errw_1));
		total_err += last_err;
	}

	// In the last iteration of the loop, we added some errors that we shouldn't have;
	// undo this now.
	Vec128_S32 wrong_errs = last_err.andnot(Vec128_S8::loadu_lo32(in->count_mask + i - 4).to_s32_lo());
	total_err -= wrong_errs;

	// reduce!
	return reduce_add_s32(total_err);
}

static BC7Error sse4_calc_scalar_err(const U8 inds[], const BC7SubsetInput *in, int ib, const BC7Color endpoints_q[2])
{
	// determine lo and hi endpoints
	const Vec128_S16 lo16 = Vec128_S16(endpoints_q[0].a);
	const Vec128_S16 diff16 = Vec128_S16(endpoints_q[0].a - endpoints_q[1].a);
	const Vec128_S8 weights_tab = Vec128_S8::loadu(radtex_lerp_factor_neg2x[ib-2]);
	Vec128_S32 total_err = Vec128_S32::zero();

	for (int i = 0; i < in->num_pixels; i += 8)
	{
		// grab the current set of pixels and extend to 32 bits
		// (using that "a" ends up in bits [31:24] of every 32-bit group)
		Vec128_S32 vals0 = Vec128_S32::loadu(in->pixels + i*4 + 0*4).srl<24>();
		Vec128_S32 vals1 = Vec128_S32::loadu(in->pixels + i*4 + 4*4).srl<24>();

		// Pack to 16 bits and determine values relative to lo
		Vec128_S16 rel16 = vals0.to_s16_sat(vals1) - lo16;

		// load the indices
		Vec128_U8 q8 = Vec128_U8::loadu_lo64(inds + i);

		// look up the weights for q in the table
		Vec128_S8 weights_from_q = weights_tab.shuf(q8);
		Vec128_S16 lerpfct = Vec128_S8::zero().unpack_lo(weights_from_q).s16();

		// perform the interpolation to determine the error
		// we compute:
		//
		//   rounding_shift_right(lerpfct * (lo - hi), 15) - (pixel - lo)
		//
		// where lerpfct = -(bc7_weight[q] << 9)
		Vec128_S16 err16 = lerpfct.mulhrs(diff16) - rel16;

		// mask to take care of count
		// err16 = _mm_and_si128(err16, _mm_cvtepi8_epi16(load64u(in->count_mask + i)));
		// NOTE(djg): This was a s8 -> s16 conversion, but in->count_mask is U8.
		err16 &= Vec128_S8::loadu_lo64(in->count_mask + i).to_s16_lo();

		// compute SSDs
		Vec128_S32 ssd = err16.madd(err16);
		total_err += ssd;
	}

	// reduce and apply weight!
	return reduce_add_s32(total_err) * in->channel_weights[3];
	// TODO(djg): This introduces an extra move and throws out register allocation. Why?
	// I guess because the optimizer can't determine that total_err is dead and allocates
	// a copy.
	//return total_err.reduce_add() * in->channel_weights[3];
}

template<bool t_include_alpha>
static BC7Error sse4_calc_vector_err(const U8 inds[], const BC7SubsetInput *in, int ib, const BC7Color endpoints_q[2])
{
	const Vec128_S8 weights_tab = Vec128_S8::loadu(radtex_lerp_factor_neg2x[ib-2]);

	// This makes us treat all endpoint alphas as 255
	Vec128_U16 chan_weight16 = Vec128_U16::loadu_lo64(in->channel_weights).dup_lo();
	if (!t_include_alpha) // make alpha weight zero if desired
		chan_weight16 &= Vec128_U16::repeat4(0xFFFF, 0xFFFF, 0xFFFF, 0);

	// low/high endpoints
	Vec128_S16 endpoints16 = Vec128_U8::loadu_lo64(&endpoints_q[0]).to_sint16_lo();
	Vec128_S16 lo16 = endpoints16.dup_lo();
	Vec128_S16 hi16 = endpoints16.dup_hi();
	Vec128_S16 diff16 = lo16 - hi16;

	Vec128_S32 total_err = Vec128_S32::zero();
	Vec128_S32 last_err = Vec128_S32::zero();
	int i;
	for (i = 0; i < in->num_pixels; i += 4)
	{
		// load the pixels
		Vec128_U8 pixels = Vec128_U8::loadu(in->pixels + i*4);
		// NOTE(djg): this could be pixels.lo64_to_uint16() - lo16
		Vec128_S16 pixrel16_0 = Vec128_S16(zext8to16_lo(pixels)) - lo16;
		Vec128_S16 pixrel16_1 = Vec128_S16(zext8to16_hi(pixels)) - lo16;

		// grab the indices
		Vec128_U8 q8 = Vec128_U8::loadu_lo64(inds + i);

		// look up the weights for q in the table
		Vec128_S8 weights_from_q = weights_tab.shuf(q8);

		// creates 4 copies of each of the weights as 16 bits, shifted by 8
		Vec128_S16 lerpf_0 = Vec128_S16(weights_from_q.shuf(Vec128_S8(-1, 0, -1, 0, -1, 0, -1, 0, -1, 1, -1, 1, -1, 1, -1, 1)));
		Vec128_S16 lerpf_1 = Vec128_S16(weights_from_q.shuf(Vec128_S8(-1, 2, -1, 2, -1, 2, -1, 2, -1, 3, -1, 3, -1, 3, -1, 3)));

		// perform the interpolation to determine the error
		// we compute:
		//
		//   rounding_shift_right(lerpf * (lo - hi), 15) - (pixel - lo)
		//
		// where lerpf = -(bc7_weight[q] << 9)
		Vec128_S16 err_0 = lerpf_0.mulhrs(diff16) - pixrel16_0;
		Vec128_S16 err_1 = lerpf_1.mulhrs(diff16) - pixrel16_1;

		// error calc with weighting
		Vec128_S16 errw_0 = err_0 * Vec128_S16(chan_weight16);
		Vec128_S16 errw_1 = err_1 * Vec128_S16(chan_weight16);

		last_err = Vec128_S32(dot16x16_4x(err_0, err_1, errw_0, errw_1));
		total_err += last_err;
	}

	// In the last iteration of the loop, we added some errors that we shouldn't have;
	// undo this now.
	Vec128_S32 wrong_errs = last_err.andnot(Vec128_S8::loadu_lo32(in->count_mask + i - 4).to_s32_lo());
	total_err -= wrong_errs;

	// reduce!
	return reduce_add_s32(total_err);
}

// *Updates* out_pixels to contain the scalar channel
// existing alpha channel values must be 0!
static void sse4_decode_subset_scalar(BC7Color out_pixels[16], int ib, const BC7Color endpoints_q[2], const U8 * inds)
{
	// determine lo and hi endpoints
	const Vec128_S16 lo16 = Vec128_S16(endpoints_q[0].a);
	const Vec128_S16 diff16 = Vec128_S16(endpoints_q[0].a - endpoints_q[1].a);
	const Vec128_S8  weights_tab = Vec128_S8::loadu(radtex_lerp_factor_neg2x[ib-2]);

	// Load the indices and translate to weights
	Vec128_U8 inds8 = Vec128_U8::loadu(inds);
	Vec128_S8 weights = weights_tab.shuf(inds8);

	// Expand to 16-bit lerp factors with a 8-bit left shift
	Vec128_S16 lerpf16_0 = Vec128_S8::zero().unpack_lo(weights).s16();
	Vec128_S16 lerpf16_1 = Vec128_S8::zero().unpack_hi(weights).s16();

	// Perform the interpolation:
	//   rounding_shift_right(lerpfct * (lo - hi), 15) + lo
	Vec128_S16 interp16_0 = lerpf16_0.mulhrs(diff16) + lo16;
	Vec128_S16 interp16_1 = lerpf16_1.mulhrs(diff16) + lo16;

	// Shift by 8 and then unpack to 32-bit values to place it in the alpha channel
	Vec128_S16 shifted16_0 = interp16_0.shl<8>();
	Vec128_S16 shifted16_1 = interp16_1.shl<8>();

	// Unpack from 16-bit values to 32-bit (with alpha in the top byte)
	// and update the output pixels to insert the new alpha.
	(Vec128_U8::loadu(out_pixels +  0) | Vec128_S16::zero().unpack_lo(shifted16_0).u8()).storeu(out_pixels +  0);
	(Vec128_U8::loadu(out_pixels +  4) | Vec128_S16::zero().unpack_hi(shifted16_0).u8()).storeu(out_pixels +  4);
	(Vec128_U8::loadu(out_pixels +  8) | Vec128_S16::zero().unpack_lo(shifted16_1).u8()).storeu(out_pixels +  8);
	(Vec128_U8::loadu(out_pixels + 12) | Vec128_S16::zero().unpack_hi(shifted16_1).u8()).storeu(out_pixels + 12);
}

// Overwrites out_pixels. When t_clear_alpha, the alpha channel is initialized to all-0, setting up
// for sse4_decode_subset_scalar.
template<bool t_clear_alpha>
static void sse4_decode_subset_vector(BC7Color out_pixels[16], int ib, const BC7Color endpoints_q[2], const U8 * inds, int num_pixels)
{
	const Vec128_S8 weights_tab = Vec128_S8::loadu(radtex_lerp_factor_neg2x[ib-2]);
	Vec128_U8 endpoints8 = Vec128_U8::loadu_lo64(&endpoints_q[0]);

	if (t_clear_alpha)
		endpoints8 &= Vec128_U8::repeat4(0xff, 0xff, 0xff, 0);

	// low/high endpoints
	Vec128_S16 endpoints16 = endpoints8.to_sint16_lo();
	Vec128_S16 lo16 = endpoints16.dup_lo();
	Vec128_S16 hi16 = endpoints16.dup_hi();
	Vec128_S16 diff16 = lo16 - hi16;

	for (int i = 0; i < num_pixels; i += 4)
	{
		// Grab 4 indices and turn into weights
		Vec128_U8 inds8 = Vec128_U8::loadu_lo32(inds + i);
		Vec128_S8 weights = weights_tab.shuf(inds8);

		// Expand to 16-bit lerp factors and replicate 4x each
		Vec128_S16 lerpf16_0 = weights.shuf(Vec128_S8(-1,0,-1,0,-1,0,-1,0, -1,1,-1,1,-1,1,-1,1)).s16();
		Vec128_S16 lerpf16_1 = weights.shuf(Vec128_S8(-1,2,-1,2,-1,2,-1,2, -1,3,-1,3,-1,3,-1,3)).s16();

		// Perform the interpolation:
		//   rounding_shift_right(lerpf * (lo - hi), 15) + lo
		Vec128_S16 interp16_0 = lerpf16_0.mulhrs(diff16) + lo16;
		Vec128_S16 interp16_1 = lerpf16_1.mulhrs(diff16) + lo16;

		// Pack down to 8-bit pixels
		interp16_0.to_uint8_sat(interp16_1).storeu(out_pixels + i);
	}
}

// linear approx, very fast.
template<typename Mode>
static void calc_indexes_linear_approx(BC7SubsetState *st, const BC7SubsetInput *in, const BC7Color endpoints_q[2])
{
	unsigned ib_ = Mode::ib, ib2_ = Mode::ib2;
	if(st->isbit) radtex_swap(ib_,ib2_);

	U8 * idxs  = st->idxs[st->isbit].ind;
	U8 * idxs2 = st->idxs[st->isbit ^ 1].ind;

	// NOTE(fg): not exactly the same as the scalar version because the SSSE3 version
	// works in floats and uses round to nearest when going to integer, instead of adding
	// 0.5 and truncating. Round to nearest is actually (very slightly) better, but it's
	// hard to get at in C++ code. (With a C99-level math lib, lrint should work.)
	// -> scalar uses lrint now so this does cause any difference

	BC7Error vec_err = sse4_calc_vector_inds_linear<Mode::ib2==0>(idxs, in, ib_, endpoints_q);
	BC7Error scalar_err = 0;
	if (Mode::ib2 != 0)
		scalar_err = sse4_calc_scalar_inds_linear(idxs2, in, ib2_, endpoints_q);

	st->err = vec_err + scalar_err;
	st->scalar_err = scalar_err;
	
	#ifdef SCALAR_PARANOIA
	// check vs scalar : yes matches
	BC7SubsetState check_st = *st;
	calc_indexes_linear_approx_scalar<Mode>(&check_st,in,endpoints_q);
	if ( st->err != check_st.err )
	{
		rrprintf("sse: %d scalar: %d\n",st->err,check_st.err);
	}
	RR_ASSERT_ALWAYS( st->err == check_st.err );
	#endif
}

template<typename Mode>
static BC7Error calc_subset_error(const BC7SubsetState *st, const BC7SubsetInput *in, const BC7Color endpoints_q[2], BC7Error *out_scalar_err=0)
{
	#ifdef SCALAR_PARANOIA
	BC7Error check_scalar_err;
	BC7Error check_subset_err = calc_subset_error_scalar<Mode>(st,in,endpoints_q,&check_scalar_err);
	#endif

	unsigned ib_ = Mode::ib, ib2_ = Mode::ib2;
	if(st->isbit) radtex_swap(ib_,ib2_);

	const U8 * idxs  = st->idxs[st->isbit].ind;
	const U8 * idxs2 = st->idxs[st->isbit ^ 1].ind;
	BC7Error vec_err = sse4_calc_vector_err<Mode::ib2==0>(idxs, in, ib_, endpoints_q);
	BC7Error scalar_err = 0;
	if (Mode::ib2 != 0)
		scalar_err = sse4_calc_scalar_err(idxs2, in, ib2_, endpoints_q);

	if (out_scalar_err)
		*out_scalar_err = scalar_err;

	BC7Error ret = vec_err + scalar_err;

	#ifdef SCALAR_PARANOIA
	// check vs scalar : yes matches
	// ok :
	RR_ASSERT_ALWAYS( check_subset_err == ret );
	RR_ASSERT_ALWAYS( check_scalar_err == scalar_err );
	#endif
	
	return ret;
}

// decodes _up to_ 16 pixels (actual count depends on num_pixels)
template<typename Mode>
static void decode_subset(BC7Color out_pixels[16], const BC7SubsetState *st, BC7Flags flags, int num_pixels)
{
	#ifdef SCALAR_PARANOIA
	BC7Color out_pixels_scalar[16];
	decode_subset_scalar<Mode>(out_pixels_scalar,st,flags,num_pixels);
	#endif
	
	RR_ASSERT( check_endpoints_q<Mode>(st,flags) );

	sse4_decode_subset_vector<Mode::ib2 != 0>(out_pixels, st->isbit ? Mode::ib2 : Mode::ib, st->endpoints_q, st->idxs[st->isbit].ind, num_pixels);
	if (Mode::ib2 != 0)
		sse4_decode_subset_scalar(out_pixels, st->isbit ? Mode::ib : Mode::ib2, st->endpoints_q, st->idxs[st->isbit ^ 1].ind);

	#ifdef SCALAR_PARANOIA
	RR_ASSERT_ALWAYS( 0 == memcmp(out_pixels,out_pixels_scalar,sizeof(BC7Color)*num_pixels) );
	#endif
}

#else // defined(DO_BUILD_SSE4)

// linear approx, very fast.
template<typename Mode>
static void calc_indexes_linear_approx(BC7SubsetState *st, const BC7SubsetInput *in, const BC7Color endpoints_q[2])
{
	calc_indexes_linear_approx_scalar<Mode>(st,in,endpoints_q);
}

// calc_subset_error
//	does not change st->err
template<typename Mode>
static BC7Error calc_subset_error(const BC7SubsetState *st, const BC7SubsetInput *in, const BC7Color endpoints_q[2], BC7Error *out_scalar_err=0)
{
	return calc_subset_error_scalar<Mode>(st,in,endpoints_q,out_scalar_err);
}

// decodes _up to_ 16 pixels (actual count depends on num_pixels)
template<typename Mode>
static void decode_subset(BC7Color out_pixels[16], const BC7SubsetState *st, BC7Flags flags, int num_pixels)
{
	decode_subset_scalar<Mode>(out_pixels,st,flags,num_pixels);
}

#endif

template<typename Mode>
static void bc7enc_update_subset_endpoints_q(BC7SubsetState *st, BC7Flags flags)
{
	calc_endpoints_and_pbits<Mode>(st->endpoints_q, st, flags, st->pbits);
}

// like calc_subset_error
// but writes st->err and scalar_err
// equivalent of calc_indexes but with no index change
template<typename Mode>
static void update_subset_error(BC7SubsetState *st, const BC7SubsetInput *in)
{
	//RR_ASSERT( check_endpoints_q<Mode>(st,flags) );
	
	st->err = calc_subset_error<Mode>(st, in, st->endpoints_q, &st->scalar_err);
}

template<typename Mode>
static BC7Error bc7enc_calc_subset_error(const BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags)
{
	RR_ASSERT( check_endpoints_q<Mode>(st,flags) );
	
	return calc_subset_error<Mode>(st, in, st->endpoints_q);
}

// unconditionally writes st->err (and scalar)
// does not change anything else in st
// gives you the err for the current state
// this is like bc7enc_trial with indices not allowed to be changed
template<typename Mode>
static void bc7enc_update_subset_error(BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags, int pbits)
{
	// endpoints changed; make new endpoints_q , but don't reindex
	// passed in pbits means use that, don't search ; pbits = -1 means search
	st->pbits = calc_endpoints_and_pbits<Mode>(st->endpoints_q, st, flags, pbits);
	update_subset_error<Mode>(st, in);
}

// calc_indexes
//  fills st->idxs[]
//	and st->err (and scalar_err)
// unconditionally writes to [st], doesn't check if it's an improvement
template<typename Mode>
static void calc_indexes(BC7SubsetState *st, const BC7SubsetInput *in, const BC7Color endpoints_q[2], BC7Flags flags)
{
	if (flags & BC7ENC_IDXS_EXACT)
		calc_indexes_exact<Mode>(st, in, endpoints_q);
	else
		calc_indexes_linear_approx<Mode>(st, in, endpoints_q);
}

typedef void BC7RegularTrialFunc(BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags, int pbits, int anchor_pos);

// bc7enc_trial_with_endpoints_q
template<typename Mode>
static RADFORCEINLINE void bc7enc_trial_with_endpoints_q(BC7SubsetState *st, const BC7SubsetInput *in, const BC7Color endpoints_q[2], BC7Flags flags, int anchor_pos)
{
	RR_ASSERT( st->pbits == 0 || (Mode::spb + Mode::epb) > 0 );

	calc_indexes<Mode>(st, in, endpoints_q, flags);
#ifdef DO_BUILD_SSE4
	if ( anchor_pos >= 0 )
		sse4_force_anchor_bit_zero<Mode>(st, in, endpoints_q, anchor_pos);
#else
	if ( anchor_pos >= 0 )
		force_anchor_bit_zero<Mode>(st, in, endpoints_q, anchor_pos);
#endif
}

// bc7enc_trial
//	take current st->endpoints candidate
// finds pbits & idxs
// fills st->err (unconditionally)
// optionally takes anchor_pos to force that bit off when endpoints can't be swapped
template<typename Mode>
static void bc7enc_trial(BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags, int pbits = -1, int anchor_pos = -1)
{
	// st->endpoints changed ; make new endpoints_q
	st->pbits = calc_endpoints_and_pbits<Mode>(st->endpoints_q, st, flags, pbits);

	bc7enc_trial_with_endpoints_q<Mode>(st, in, st->endpoints_q, flags, anchor_pos);
}

template<typename Mode>
static void bc7enc_trial_no_end_change(BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags, int anchor_pos)
{
	// endpoints did not change, but we want to find new indices (reindex)
	RR_ASSERT( check_endpoints_q<Mode>(st,flags) );

	bc7enc_trial_with_endpoints_q<Mode>(st, in, st->endpoints_q, flags, anchor_pos);
}

template<typename Mode>
static void bc7enc_refine_brute_pbits(BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags, int anchor_pos = -1);

// bc7enc_trial_brute alternative to bc7enc_trial for anneal or greedyoptimize
// not currently used
template<typename Mode>
static void bc7enc_trial_brute(BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags in_flags, int pbits = -1, int anchor_pos = -1)
{
	// same function signature as bc7enc_trial , but pbits should always be free (-1)
	RR_ASSERT( pbits == -1 );
	
	//BC7Flags flags = in_flags | BC7ENC_IDXS_EXACT; // ?
	BC7Flags flags = in_flags;
	
	if(!Mode::epb && !Mode::spb)
	{
		bc7enc_trial<Mode>(st,in,flags,pbits,anchor_pos);
	}
	else
	{
		// reset st err because refine_brute uses try_and_commit :
		st->err = BC7_ERROR_MAX;
		st->scalar_err = BC7_ERROR_MAX/2;
		bc7enc_refine_brute_pbits<Mode>(st,in,flags,anchor_pos);
		RR_ASSERT( st->err != BC7_ERROR_MAX );
	}
}

template<typename Mode>
static int bc7enc_reindex_block_mode(BC7BlockState *st, const BC7SubsetInput *input, BC7Flags flags, bool change_pbits, bool allow_swap_endpoints)
{
	// reindex :
	
	// these should not be used :
	RR_ASSERT( allow_swap_endpoints == false );
	
	int ns = Mode::ns;
	
	// radtex_subset_anchors[] contains two anchor positions
	//	<< 4 gives implicit third anchor = 0
	int anchors = radtex_subset_anchors[ns-1][st->p] << 4;
		
	BC7Error reindex_err = 0;
	for (int s = 0; s < ns; ++s)
	{
		BC7SubsetState *sst = &st->subsets[s];

		RR_ASSERT( check_endpoints_q<Mode>(sst,flags) );
	
		// extract 4 bit anchor_pos (first is always 0)
		//	anchor_pos is in dense linear subset position
		int anchor_pos = (anchors >> (s*4)) & 0xf;
		if ( allow_swap_endpoints )
			anchor_pos = -1;		
		
		if ( change_pbits && ( Mode::epb != 0 || Mode::spb != 0 ) )
		{
			// Determine existing pbits and endpoints rounded to representable values
			// Start with current guess
			BC7Color initial_endpoints_q[2];
			int initial_pbits = calc_endpoints_and_pbits<Mode>(initial_endpoints_q, sst, flags, sst->pbits);
			RR_ASSERT(initial_pbits == sst->pbits);

			// The pbits only appear once in the expanded endpoints for all modes;
			// that means to toggle a given pbit, all we have to do is toggle a single bit
			// in the quantized endpoints.
			const U8 color_pbit = 128 >> Mode::cb;
			const U8 alpha_pbit = (Mode::ab != 0) ? 128 >> Mode::ab : 0;

			// Try toggling the pbits to see if we find something better
			// spb tries 4 combinations (0,1,2,3)
			// epb only tries 2 (0 and 3)
			//
			// i tells us which bits we *toggle*, not what the actual
			// pbits are
			for (int i = 0; i < 4; i += (Mode::epb != 0 ? 1 : 3))
			{
				BC7SubsetState nst = *sst;
				nst.pbits = static_cast<U8>(initial_pbits ^ i);

				// compute the expanded endpoints
				for (int j = 0; j < 2; ++j)
				{
					nst.endpoints[j] = initial_endpoints_q[j];
					if (i & (1<<j))
					{
						nst.endpoints[j].r ^= color_pbit;
						nst.endpoints[j].g ^= color_pbit;
						nst.endpoints[j].b ^= color_pbit;
						nst.endpoints[j].a ^= alpha_pbit;
					}
				}

				// try it!
				bc7enc_trial_with_endpoints_q<Mode>(&nst, &input[s], nst.endpoints, flags, anchor_pos);
				if (nst.err < sst->err)
					*sst = nst;
			}
		}
		else
		{
			// retain pbits :
			bc7enc_trial_no_end_change<Mode>(sst,&input[s],flags,anchor_pos);
		}
		reindex_err += sst->err;
	}
	st->err = reindex_err;
	
	int changed_bits = c_bc7_total_bits_indices[Mode::index];
	if ( change_pbits )
		changed_bits += c_bc7_total_bits_pbits[Mode::index];
	
	return changed_bits;
}
		
// return value is the count of bits in the fields that were changed (indices + optionally pbits)
int bc7enc_reindex_block(BC7BlockState *st, const BC7SubsetInput *input, BC7Flags flags, bool change_pbits, bool allow_swap_endpoints, bool slow_exact)
{
	// these should not be used :
	RR_ASSERT( allow_swap_endpoints == false );
		
	if ( slow_exact )
		flags |= BC7ENC_IDXS_EXACT; // calc_indexes_exact
	else
		RR_ASSERT( (flags & BC7ENC_IDXS_EXACT) == 0 ); // calc_indexes_linear_approx
	
	switch(st->mode) {
	case 0: return bc7enc_reindex_block_mode< BC7Mode<0> >(st,input,flags,change_pbits,allow_swap_endpoints);
	case 1: return bc7enc_reindex_block_mode< BC7Mode<1> >(st,input,flags,change_pbits,allow_swap_endpoints);
	case 2: return bc7enc_reindex_block_mode< BC7Mode<2> >(st,input,flags,change_pbits,allow_swap_endpoints);
	case 3: return bc7enc_reindex_block_mode< BC7Mode<3> >(st,input,flags,change_pbits,allow_swap_endpoints);
	case 4: return bc7enc_reindex_block_mode< BC7Mode<4> >(st,input,flags,change_pbits,allow_swap_endpoints);
	case 5: return bc7enc_reindex_block_mode< BC7Mode<5> >(st,input,flags,change_pbits,allow_swap_endpoints);
	case 6: return bc7enc_reindex_block_mode< BC7Mode<6> >(st,input,flags,change_pbits,allow_swap_endpoints);
	case 7: return bc7enc_reindex_block_mode< BC7Mode<7> >(st,input,flags,change_pbits,allow_swap_endpoints);
	RR_NO_DEFAULT_CASE
	}
}

// takes this state if it improves on current best
// returns true if an improvement, false otherwise
template<typename Mode>
static bool bc7enc_try_and_commit(BC7SubsetState *cur_best, BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags, int pbits = -1, int anchor_pos = -1)
{
	bc7enc_trial<Mode>(st, in, flags, pbits, anchor_pos);
	if (st->err >= cur_best->err)
		return false;

	*cur_best = *st;
	return true;
}

// like the above, but treats vector and scalar state as separate
template<typename Mode>
static bool bc7enc_try_and_commit_partial(BC7SubsetState *cur_best, BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags, int pbits = -1, bool noreindex = false)
{

	if ( noreindex )
		bc7enc_update_subset_error<Mode>(st,in,flags, pbits);
	else
		bc7enc_trial<Mode>(st, in, flags, pbits);

	if (Mode::ib2 == 0)
	{
		// no separate scalar channel in this mode, we act just like bc7enc_try_and_commit
		if (st->err >= cur_best->err)
			return false;

		*cur_best = *st;
		return true;
	}
	else
	{
		// we can take either just scalar or just vector improvements or both
		
		// Modes with ib2 != 0 don't have pbits which makes our lives easier; we
		// can really treat scalar and vector channels as independent.
		bool any_improvement = false;
		UINTa isbit = st->isbit;
	
		BC7Error cur_best_vec_err = cur_best->err - cur_best->scalar_err;
		BC7Error st_vec_err = st->err - st->scalar_err;

		if (st_vec_err < cur_best_vec_err)
		{
			cur_best->endpoints[0].copy_rgb_from(st->endpoints[0]);
			cur_best->endpoints[1].copy_rgb_from(st->endpoints[1]);
			cur_best->endpoints_q[0].copy_rgb_from(st->endpoints_q[0]);
			cur_best->endpoints_q[1].copy_rgb_from(st->endpoints_q[1]);
			cur_best->idxs[isbit] = st->idxs[isbit];
			cur_best->err = st_vec_err + cur_best->scalar_err;
			cur_best_vec_err = st_vec_err;
			any_improvement = true;
		}

		if (st->scalar_err < cur_best->scalar_err)
		{
			cur_best->endpoints[0].a = st->endpoints[0].a;
			cur_best->endpoints[1].a = st->endpoints[1].a;
			cur_best->endpoints_q[0].a = st->endpoints_q[0].a;
			cur_best->endpoints_q[1].a = st->endpoints_q[1].a;
			cur_best->idxs[isbit ^ 1] = st->idxs[isbit ^ 1];
			cur_best->err = cur_best_vec_err + st->scalar_err;
			cur_best->scalar_err = st->scalar_err;
			any_improvement = true;
		}

		RR_ASSERT( cur_best->err <= st->err );

		return any_improvement;
	}
}

// ---- Whole-block error calculations

template<typename Mode>
static bool bc7enc_check_block_endpoints_q(BC7BlockState *st, BC7Flags flags)
{
	for (int s = 0; s < Mode::ns; ++s)
		RR_ASSERT( check_endpoints_q<Mode>(st->subsets + s, flags) );
		
	return true;
}

bool bc7enc_check_endpoints_q(BC7BlockState *st, BC7Flags flags)
{
	switch (st->mode) {
	case 0: return bc7enc_check_block_endpoints_q<BC7Mode<0> >(st, flags);
	case 1: return bc7enc_check_block_endpoints_q<BC7Mode<1> >(st, flags);
	case 2: return bc7enc_check_block_endpoints_q<BC7Mode<2> >(st, flags);
	case 3: return bc7enc_check_block_endpoints_q<BC7Mode<3> >(st, flags);
	case 4: return bc7enc_check_block_endpoints_q<BC7Mode<4> >(st, flags);
	case 5: return bc7enc_check_block_endpoints_q<BC7Mode<5> >(st, flags);
	case 6: return bc7enc_check_block_endpoints_q<BC7Mode<6> >(st, flags);
	case 7: return bc7enc_check_block_endpoints_q<BC7Mode<7> >(st, flags);
	default: RR_ASSERT_ALWAYS(!"bad mode"); return false;
	}
}


template<typename Mode>
static bool bc7enc_update_block_endpoints_q(BC7BlockState *st, BC7Flags flags)
{
	for (int s = 0; s < Mode::ns; ++s)
		bc7enc_update_subset_endpoints_q<Mode>(st->subsets + s, flags);
		
	return true;
}

bool bc7enc_update_endpoints_q(BC7BlockState *st, BC7Flags flags)
{
	switch (st->mode) {
	case 0: return bc7enc_update_block_endpoints_q<BC7Mode<0> >(st, flags);
	case 1: return bc7enc_update_block_endpoints_q<BC7Mode<1> >(st, flags);
	case 2: return bc7enc_update_block_endpoints_q<BC7Mode<2> >(st, flags);
	case 3: return bc7enc_update_block_endpoints_q<BC7Mode<3> >(st, flags);
	case 4: return bc7enc_update_block_endpoints_q<BC7Mode<4> >(st, flags);
	case 5: return bc7enc_update_block_endpoints_q<BC7Mode<5> >(st, flags);
	case 6: return bc7enc_update_block_endpoints_q<BC7Mode<6> >(st, flags);
	case 7: return bc7enc_update_block_endpoints_q<BC7Mode<7> >(st, flags);
	default: RR_ASSERT_ALWAYS(!"bad mode"); return false;
	}
}

template<typename Mode>
static BC7Error bc7enc_calc_block_error(const BC7BlockState *st, const BC7SubsetInput *in, BC7Flags flags)
{
	BC7Error sum = 0;
	for (int s = 0; s < Mode::ns; ++s)
		sum += bc7enc_calc_subset_error<Mode>(st->subsets + s, in + s, flags);

	return sum;
}

// bc7enc_calc_error : not currently used
// bc7enc_calc_error ; BC7SubsetInput should be given for all [ns]
// needs flags for ignore alpha
BC7Error bc7enc_calc_error(const BC7BlockState *st, const BC7SubsetInput *in, BC7Flags flags)
{
	switch (st->mode) {
	case 0: return bc7enc_calc_block_error<BC7Mode<0> >(st, in, flags);
	case 1: return bc7enc_calc_block_error<BC7Mode<1> >(st, in, flags);
	case 2: return bc7enc_calc_block_error<BC7Mode<2> >(st, in, flags);
	case 3: return bc7enc_calc_block_error<BC7Mode<3> >(st, in, flags);
	case 4: return bc7enc_calc_block_error<BC7Mode<4> >(st, in, flags);
	case 5: return bc7enc_calc_block_error<BC7Mode<5> >(st, in, flags);
	case 6: return bc7enc_calc_block_error<BC7Mode<6> >(st, in, flags);
	case 7: return bc7enc_calc_block_error<BC7Mode<7> >(st, in, flags);
	default: RR_ASSERT_ALWAYS(!"bad mode"); return 0;
	}
}

BC7Error bc7enc_calc_error(const BC7BlockState *st, const U8 * block, BC7Flags flags)
{
	BC7Input input;
	BC7Input_Get(&input,block,st);
	
	return bc7enc_calc_error(st,input.subsets,flags);
}


template<typename Mode>
static void bc7enc_decode_state_mode(const BC7BlockState *in_state, U8 * block, BC7Flags flags)
{
	//const int ns = Mode::ns;
	//const int ib = Mode::ib;
	//const int ib2 = Mode::ib2;

	// Grab data from subsets
	RR_ASSERT(in_state->mode == Mode::index);
	int p = in_state->p; // partition
	// rbit is actually per-block, not per subset, make sure they match :
	U8 rbit = in_state->subsets[0].rbit;
	
	U64 inds = radtex_subset_to_inds[Mode::ns-1][p];
	for (int s = 0; s < Mode::ns; ++s)
	{
		const BC7SubsetState *sst = in_state->subsets + s;

		RR_ASSERT( sst->pbits == 0 || (Mode::spb + Mode::epb) > 0 );
	
		int num_pixels = radtex_num_pixels_per_subset[Mode::ns][p][s];

		BC7Color pixels[16];
		decode_subset<Mode>(pixels, sst, flags, num_pixels);

		// pixels are in dense subset linear order,
		// scatter to pixel block order.
		BC7Color * out_rgba = (BC7Color *)block;
		for (int j = 0; j < num_pixels; ++j)
		{
			int i = static_cast<int>(inds & 0xf);
			inds >>= 4;
			out_rgba[i] = pixels[j];
		}
	}
	
	if ( Mode::rb != 0 && rbit != 0 )
	{
		for (int i = 0; i < 64; i += 4)
			radtex_swap(block[i + 3], block[i + rbit - 1]);
	}
}

// flags for ignore alpha
void bc7enc_decode_state(const BC7BlockState *st, U8 * block, BC7Flags flags)
{
	#if 1

	switch (st->mode) {
	case 0: return bc7enc_decode_state_mode<BC7Mode<0> >(st, block, flags);
	case 1: return bc7enc_decode_state_mode<BC7Mode<1> >(st, block, flags);
	case 2: return bc7enc_decode_state_mode<BC7Mode<2> >(st, block, flags);
	case 3: return bc7enc_decode_state_mode<BC7Mode<3> >(st, block, flags);
	case 4: return bc7enc_decode_state_mode<BC7Mode<4> >(st, block, flags);
	case 5: return bc7enc_decode_state_mode<BC7Mode<5> >(st, block, flags);
	case 6: return bc7enc_decode_state_mode<BC7Mode<6> >(st, block, flags);
	case 7: return bc7enc_decode_state_mode<BC7Mode<7> >(st, block, flags);
	default: RR_ASSERT_ALWAYS(!"bad mode"); return;
	}
	
	#else
	
	// just to make sure : yes decode_state_mode is a lot faster than this
	
	U8 out[16];
	
	//bc7enc_canonicalize(st);
	bc7enc_emit(out,st);
	bc7_decode_block_fast(block,out);
	
	#endif
}
	

// ---- Refinement helpers

#ifdef DO_BUILD_SSE4

static void store_u8_endpoints(BC7Color *result, const VecF32x4 &lo, const VecF32x4 &hi)
{
	Vec128_S16 vals_int16 = lo.to_int32_round().to_s16_sat(hi.to_int32_round());
	Vec128_U8  vals_int8 = vals_int16.to_uint8_sat(vals_int16);
	vals_int8.storeu_lo64(result);
}

#endif

// Linearly transforms the endpoints corresponding to a remapping of indices
// by adjust[0] quantization steps (in index space) for endpoint 0,
// and by adjust[1] quantization steps for endpoint 1.
static void linear_transform_endpoints(BC7Color *transformed, const BC7Color *initial, const float adjust[2], int ib)
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

#ifdef DO_BUILD_SSE4
	Vec128_S32 col0int = Vec128_U8::loadu_lo32(initial + 0).to_sint32_lo();
	Vec128_S32 col1int = Vec128_U8::loadu_lo32(initial + 1).to_sint32_lo();
	VecF32x4 base = col0int.to_f32();
	VecF32x4 dir = (col1int - col0int).to_f32();

	VecF32x4 new0 = base + VecF32x4(t0) * dir;
	VecF32x4 new1 = base + VecF32x4(t1) * dir;
	store_u8_endpoints(transformed, new0, new1);
#else
	for (int c = 0; c < 4; ++c)
	{
		// endpoint parameteriztion: base + t*dir
		float base = initial[0].v[c];
		float dir = initial[1].v[c] - base;

		transformed[0].v[c] = round_and_clamp_U8(base + t0 * dir);
		transformed[1].v[c] = round_and_clamp_U8(base + t1 * dir);
	}
#endif
}

template<typename Mode>
static void try_linear_transforms(BC7SubsetState *st, const BC7SubsetInput *in, const BC7Color &col0, const BC7Color &col1, BC7Flags flags, const float adjust[][2], int num_adjust)
{
	BC7SubsetEndpoints initial;
	initial.endpoints[0] = col0;
	initial.endpoints[1] = col1;

	BC7SubsetState nst = *st;
	if (Mode::ib2 == 0)
	{
		for (int i = 0; i < num_adjust; ++i)
		{
			linear_transform_endpoints(nst.endpoints, initial.endpoints, adjust[i], st->isbit ? Mode::ib2 : Mode::ib);
			bc7enc_try_and_commit<Mode>(st, &nst, in, flags);
		}
	}
	else
	{
		for (int i = 0; i < num_adjust; ++i)
		{
			BC7Color temp[2];
			linear_transform_endpoints(nst.endpoints, initial.endpoints, adjust[i], st->isbit ? Mode::ib2 : Mode::ib);
			linear_transform_endpoints(temp, initial.endpoints, adjust[i], st->isbit ? Mode::ib : Mode::ib2);
			nst.endpoints[0].a = temp[0].a;
			nst.endpoints[1].a = temp[1].a;

			bc7enc_try_and_commit_partial<Mode>(st, &nst, in, flags);
		}
	}
}

template<typename Mode>
static void try_extrapolated_fit(BC7SubsetState *st, const BC7SubsetInput *in, const BC7Color &col0, const BC7Color &col1, BC7Flags flags)
{
	// try endpoints that put the passed in col0/col1 at an internal interpolated point
	//	(expands endpoints)

	static const float adjust[3][3][2] = // [ib-2][index][ep]
	{
		// ib=2
		{
			{ 1.0f, -1.0f },
			{ 1.0f,  0.0f },
			{ 0.0f, -1.0f },
		},
		// ib=3
		{
			{ 2.0f, -2.0f },
			{ 2.0f,  0.0f },
			{ 0.0f, -2.0f },
		},
		// ib=4
		{
			{ 2.0f, -2.0f },
			{ 2.0f,  0.0f },
			{ 0.0f, -2.0f },
		},
	};

	const int ib = st->isbit ? Mode::ib2 : Mode::ib;
	try_linear_transforms<Mode>(st, in, col0, col1, flags, adjust[ib-2], 3);
}

// try_two_color_fit : try passed in endpoints col0/col1 , and step-offs
//	fills [st] if improvement is found
template<typename Mode>
static void try_two_color_fit(BC7SubsetState *st, const BC7SubsetInput *in, const BC7Color &col0, const BC7Color &col1, BC7Flags flags)
{
	// Put the endpoints at the ends
	BC7SubsetState nst = *st;
	nst.endpoints[0] = col0;
	nst.endpoints[1] = col1;
	bc7enc_try_and_commit_partial<Mode>(st, &nst, in, flags);

	try_extrapolated_fit<Mode>(st, in, col0, col1, flags);
}

// ---- refine


// Linear least squares. Normal equations to get a 2x2 system then solve using Cramer's rule.
// fills st->endpoints
//  doesn't change anything else
//
// The least-squares problem is
//
//   min ||AX - B||_2
//    X
//
// where A is the n-by-2 matrix of weights, X is the 2-by-4 matrix of endpoints we're solving
// for, and B is the n-by-4 matrix of color values. A has the form
//
//      |w_1  1-w_1|
//  A = |w_2  1-w_2|
//      |...   ... |
//      |w_n  1-w_n|
//
// because we're doing linear interpolation, which allows us to simplify the bookkeeping a bit more.
// Take the above (quadratic) problem and set the derivative equal to zero to minimize, which gives
// the linear system
//
//      2 (A^T A) X - 2 (A^T B) = 0
// <=>  (A^T A) X = A^T B
// <=>  X = (A^T A)^(-1) A^T B
//
// A^T A is a 2x2 symmetric matrix, therefore small and easy to handle. A^T B is 2x4.

template<bool t_has_ib2>
static void refine_lsq_endpoints_core_scalar(BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags, U32 ib, U32 ib2)
{
	const U8 * idxs  = st->idxs[st->isbit].ind;
	const U8 * idxs2 = st->idxs[st->isbit ^ 1].ind;

	const float * factors = radtex_lerp_factor_4x[ib][0];
	const float * factors2 = radtex_lerp_factor_4x[ib2][0];

	{
		const int ib1_comp = t_has_ib2 ? 3 : 4;
		// ataij = (A^T A) element at (i,j)
		float ata00 = 0.0f, ata10 = 0.0f, ata11 = 0.0f;
		// atb0[i] = (A^T B) element at (0,i) (first row)
		float atb0[4] = { };
		// bsum[i] = sum of the elements in the i'th column of B
		// lets us infer atb1[i] = bsum[i] - atb0[i] due to the structure of the weights
		float bsum[4] = { };

		for (int i = 0; i < in->num_pixels; ++i)
		{
			U32 base = idxs[i] * 4;

			// The products w_i^2, w_i*(1-w_i) and (1-w_i)^2 are always the same for a given
			// w_i and we just store them.
			ata00 += factors[base + 0];
			ata10 += factors[base + 1];
			ata11 += factors[base + 2];
			float w = factors[base + 3];
			for(int c = 0; c < ib1_comp; ++c)
			{
				float p = in->pixels[i*4+c];
				atb0[c] += w * p;
				bsum[c] += p;
			}
		}

		float det = ata00 * ata11 - ata10 * ata10;
		if (det)
		{
			float iata00 = ata11 / det;
			float iata10 = ata10 / det;
			float iata11 = ata00 / det;

			for(int c = 0; c < ib1_comp; ++c)
			{
				float atb0c = atb0[c];
				float atb1c = bsum[c] - atb0c;
				st->endpoints[0].v[c] = round_and_clamp_U8(iata11 * atb1c - iata10 * atb0c);
				st->endpoints[1].v[c] = round_and_clamp_U8(iata00 * atb0c - iata10 * atb1c);
			}
		}
	}

	if(t_has_ib2)
	{
		float ata00 = 0.0f, ata10 = 0.0f, ata11 = 0.0f;
		float atb0 = 0.0f, bsum = 0.0f;

		for (int i = 0; i < in->num_pixels; ++i)
		{
			U32 base = idxs2[i] * 4;

			ata00 += factors2[base + 0];
			ata10 += factors2[base + 1];
			ata11 += factors2[base + 2];
			float w = factors2[base + 3];
			float p = in->pixels[i*4+3];
			atb0 += w * p;
			bsum += p;
		}

		float det = ata00 * ata11 - ata10 * ata10;
		if (det)
		{
			float iata00 = ata11 / det;
			float iata10 = ata10 / det;
			float iata11 = ata00 / det;

			float atb1 = bsum - atb0;
			st->endpoints[0].a = round_and_clamp_U8(iata11 * atb1 - iata10 * atb0);
			st->endpoints[1].a = round_and_clamp_U8(iata00 * atb0 - iata10 * atb1);
		}
	}
}

#ifdef DO_BUILD_SSE4

static VecF32x4 sse4_lls_coeffs(const U8 * indices, const BC7SubsetInput * in, Vec128_U8 * out_lerpf, U32 ib)
{
	Vec128_U8 idx = Vec128_U8::loadu(indices);
	Vec128_U8 mask = Vec128_U8::loadu(in->count_mask);
	Vec128_U8 lerp_lut = Vec128_U8::loadu(radtex_lerp_factor[ib]);

	// Translate indices to lerp factors (weights) and mask
	Vec128_U8 lerpf = lerp_lut.shuf(idx) & mask;
	*out_lerpf = lerpf;

	// We need 3 coeffs for the normal equations:
	//
	// 1. wsqsum = sum_i w_i^2
	// 2. mixed = sum_i w_i * (64 - w_i) = 64 * (sum_i w_i) = 64 * (sum_i w_i) - wsqsum
	// 3. isqsum = sum_i (64 - w_i)^2 = sum_i (64^2 - 128 w_i + w_i^2) = 4096 * num_pixels - 128 * (sum_i w_i) + wsqsum
	//
	// in the tuple (wsqsum,-mixed,isqsum) which we can derive from wsqsum, wsum = (sum_i w_i), and num_pixels.

	// Sum of weights (wsum), we can get via PSADBW and then we need to sum the halves
	Vec128_S32 wsum1 = Vec128_U8::zero().sad(lerpf);
	Vec128_S32 wsum2 = wsum1 + wsum1.zwxy();
	Vec128_S32 wsum3 = wsum2.xxxx(); // broadcast the result 4x

	// Sum of squared weights (wsqsum), I'm currently computing but might be worth it to do table lookups
	// instead (one PSHUFB LUT for low byte, one for high, and then two PSADBW).
	Vec128_S16 wsqsum1 = lerpf.madd_sat(lerpf.s8()); // 8x8->16 dot products summed between pairs
	Vec128_S32 wsqsum2 = wsqsum1.madd(Vec128_S16(1)); // sum words to DWords
	Vec128_S32 wsqsum3 = wsqsum2.sum_across(); // finish reduction (leaves 32b result broadcast 4x)

	// Now puzzle together the result.
	Vec128_S16 wsum_and_count = wsum3.s16().insert<5>(in->num_pixels); // this lane is multiplied by 4096 below and gives us the 4096*num_pixels term
	Vec128_S32 const_plus_linear = wsum_and_count.madd(Vec128_S16(0,0, -64,0, -128,4096, 0,0));
	Vec128_S32 sum_int = const_plus_linear + wsqsum3;

	// Convert to float and scale by 1/64 (so overall scaling on the linear system
	// and the right-hand side is the same)
	// this should be exact, everything here is small enough to be a machine number
	VecF32x4 sum_flt = sum_int.to_f32();
	sum_flt *= VecF32x4(1.0f / 64.0f);

	return sum_flt;
}

#endif

template<bool t_has_ib2>
static void refine_lsq_endpoints_core(BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags, U32 ib, U32 ib2)
{
#ifdef DO_BUILD_SSE4
	const U8 * idxs  = st->idxs[st->isbit].ind;
	const U8 * idxs2 = st->idxs[st->isbit ^ 1].ind;

	Vec128_U8 lerpf, lerpf_2;
	VecF32x4 ata; // (ata00,-ata10,ata11,<ignored>)
	VecF32x4 ata_2; // ata for ib2
	Vec128_S32 bsumi = Vec128_S32::zero();
	Vec128_S32 atbi = Vec128_S32::zero();

	ata = sse4_lls_coeffs(idxs, in, &lerpf, ib);
	if (t_has_ib2)
		ata_2 = sse4_lls_coeffs(idxs2, in, &lerpf_2, ib2);

	Vec128_U8 lerp_shuf = Vec128_U8::repeat4(0,1,2,3);

	for (int i = 0; i < in->num_pixels; i += 4)
	{
		Vec128_U8 mask = Vec128_S8::loadu_lo32(in->count_mask + i).to_s32_lo().u8();
		Vec128_U8 pixels = Vec128_U8::loadu(in->pixels + i*4) & mask;

		// Deinterleave to R,R,R,R, G,G,G,G, B,B,B,B, A,A,A,A
		Vec128_U8 pixels_deint = pixels.shuf(Vec128_U8(0,4,8,12, 1,5,9,13, 2,6,10,14, 3,7,11,15));

		// Sum of pixels (times 64)
		Vec128_S16 bsum1 = pixels_deint.madd_sat(Vec128_S8(64)); // pairs summed into 16-bit values
		Vec128_S32 bsum2 = bsum1.madd(Vec128_S16(1)); // horizontal sum across all 4 values
		bsumi += bsum2; // 32-bit R, G, B, A sums

		// Determine lerp weights to use
		Vec128_U8 weights = lerpf.shuf(lerp_shuf);
		if (t_has_ib2)
		{
			// In 2-index mode, splice in second set of weights for A channel
			Vec128_U8 weights2 = lerpf_2.shuf(lerp_shuf);
			weights = Vec128_U8(_mm_blend_epi16(weights, weights2, 0xc0));
		}
		lerp_shuf += Vec128_U8(4);

		// Sum weighted pixels
		Vec128_S16 wsum1 = pixels_deint.madd_sat(weights.s8()); // pairs summed into 16-bit values
		Vec128_S32 wsum2 = wsum1.madd(Vec128_S16(1)); // horizontal sum across all 4 values
		atbi += wsum2;
	}

	// Convert results to float so we can resume with the original remainder of the computation
	VecF32x4 atb0 = atbi.to_f32();
	VecF32x4 atb1 = (bsumi - atbi).to_f32();

	// New endpoints
	VecF32x4 end0_flt = VecF32x4::zero();
	VecF32x4 end1_flt = VecF32x4::zero();
	Vec128_U8 merge_mask = Vec128_U8::zero();

	VecF32x4 det = ata.xxxx() * ata.zzzz() - ata.yyyy() * ata.yyyy();
	if (det.scalar_x())
	{
		VecF32x4 iata = ata / det; // (iata11,-iata10,iata00,<ignored>)
		end0_flt = iata.xxxx() * atb1 + iata.yyyy() * atb0;
		end1_flt = iata.zzzz() * atb0 + iata.yyyy() * atb1;

		if (t_has_ib2)
			merge_mask = Vec128_U8::repeat4(0xff, 0xff, 0xff, 0);
	}
	else if (!t_has_ib2)
		return;

	if (t_has_ib2)
	{
		det = ata_2.xxxx() * ata_2.zzzz() - ata_2.yyyy() * ata_2.yyyy();
		if (det.scalar_x())
		{
			VecF32x4 iata = ata_2 / det; // (iata11,-iata10,iata00,<ignored>)
			VecF32x4 ib2_end0 = iata.xxxx() * atb1 + iata.yyyy() * atb0;
			VecF32x4 ib2_end1 = iata.zzzz() * atb0 + iata.yyyy() * atb1;

			// merge into .a channel of endpoints
			end0_flt = vblend<8>(end0_flt, ib2_end0);
			end1_flt = vblend<8>(end1_flt, ib2_end1);

			// and also update the endpoint merge mask
			merge_mask |= Vec128_U8::repeat4(0, 0, 0, 0xff);
		}
	}

	// rounding and clamping happen as side-effects during conversion
	Vec128_S32 end0_int32 = Vec128_S32(end0_flt.to_int32_round());
	Vec128_S32 end1_int32 = Vec128_S32(end1_flt.to_int32_round());
	Vec128_S16 end01_int16 = end0_int32.to_s16_sat(end1_int32);
	Vec128_U8  end01_uint8 = end01_int16.to_uint8_sat(end01_int16);

	// merge with old values if necessary
	if (t_has_ib2)
	{
		Vec128_U8 end01_old = Vec128_U8::loadu_lo64(&st->endpoints[0]);
		end01_uint8 = vblend(end01_old, end01_uint8, merge_mask);
	}

	// write both lo and hi at once
	end01_uint8.storeu_lo64(&st->endpoints[0]);
#else
	refine_lsq_endpoints_core_scalar<t_has_ib2>(st,in,flags,ib,ib2);
#endif
}

template<typename Mode>
static bool bc7enc_refine_lsq_endpoints_and_try(BC7SubsetState *cur_best, BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags, bool no_reindex = false, bool keep_pbits = false)
{
	const bool has_ib2 = Mode::ib2 != 0;

	U32 ib = Mode::ib, ib2 = Mode::ib2;
	if(Mode::isb && st->isbit) radtex_swap(ib,ib2);

	#ifdef SCALAR_PARANOIA
	// grab original state before we change anything
	BC7SubsetState check_st = *st;
	#endif

	refine_lsq_endpoints_core<has_ib2>(st, in, flags, ib, ib2);

	#ifdef SCALAR_PARANOIA
	// check vs scalar: should match exactly!
	refine_lsq_endpoints_core_scalar<has_ib2>(&check_st,in,flags,ib,ib2);

	for LOOP(epi,2)
	{
		for LOOP(c,4)
		{
			int v_sse4 = st->endpoints[epi].v[c];
			int v_scalar = check_st.endpoints[epi].v[c];
			RR_ASSERT_ALWAYS( v_sse4 == v_scalar );
		}
	}
	#endif

	TRACE("  lls refine: (%02x,%02x,%02x,%02x) (%02x,%02x,%02x,%02x)\n",
		  st->endpoints[0].r, st->endpoints[0].g, st->endpoints[0].b, st->endpoints[0].a,
		  st->endpoints[1].r, st->endpoints[1].g, st->endpoints[1].b, st->endpoints[1].a);
	
	int pbits = -1;
	if ( keep_pbits ) pbits = st->pbits;
	
	return bc7enc_try_and_commit_partial<Mode>(cur_best, st, in, flags, pbits, no_reindex);
}

// lsq_iters = -1 for noreindex
// lsq_iters = -2 for noreindex + keep_pbits
template<typename Mode>
static void bc7enc_refine_lsq_endpoints_iterated(BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags, int lsq_iters)
{
	BC7SubsetState nst = *st;
	
	if ( lsq_iters < 0 )
	{
		// no reindex
		// no need to iterate since indices don't change
		bool keep_pbits = lsq_iters < -1;
		bc7enc_refine_lsq_endpoints_and_try<Mode>(st, &nst, in, flags, true, keep_pbits);
	}
	else
	{	
		// normal usage
		for (int iter = 0; iter < lsq_iters; ++iter)
			if (!bc7enc_refine_lsq_endpoints_and_try<Mode>(st, &nst, in, flags, false, false))
				break;
	}
}

template<typename Mode>
static void bc7enc_refine_lsq_endpoints_iterated(const U8 * block, BC7BlockState *st, BC7Flags flags, int alpha_weight, int lsq_iters)
{
	BC7Error total_err = 0;
	BC7BlockInput block_in;

	block_in.init(block, radtex_subset_to_inds[Mode::ns-1][st->p], st->subsets[0].rbit);
	int total_pixels = 0;

	for (int s = 0; s < Mode::ns; ++s)
	{
		BC7SubsetState *sst = st->subsets + s;
		const int num_pixels = radtex_num_pixels_per_subset[Mode::ns][st->p][s];

		RR_ASSERT( check_endpoints_q<Mode>(sst,flags) );
		
		BC7SubsetInput input;
		input.init(block_in.pixels + total_pixels * 4, num_pixels, alpha_chan_index(sst), alpha_weight);
		bc7enc_refine_lsq_endpoints_iterated<Mode>(sst, &input, flags, lsq_iters);

		RR_ASSERT( check_endpoints_q<Mode>(sst,flags) );
		
		total_err += sst->err;
		total_pixels += num_pixels;
	}

	st->err = total_err;
}

// lsq_iters = -1 for noreindex
static void bc7enc_refine_lsq_endpoints_pass(const U8 * block, BC7BlockState *st, BC7Flags flags, int alpha_weight, int lsq_iters)
{
	switch(st->mode) {
	case 0: bc7enc_refine_lsq_endpoints_iterated<BC7Mode<0> >(block, st, flags, alpha_weight, lsq_iters); break;
	case 1: bc7enc_refine_lsq_endpoints_iterated<BC7Mode<1> >(block, st, flags, alpha_weight, lsq_iters); break;
	case 2: bc7enc_refine_lsq_endpoints_iterated<BC7Mode<2> >(block, st, flags, alpha_weight, lsq_iters); break;
	case 3: bc7enc_refine_lsq_endpoints_iterated<BC7Mode<3> >(block, st, flags, alpha_weight, lsq_iters); break;
	case 4: bc7enc_refine_lsq_endpoints_iterated<BC7Mode<4> >(block, st, flags, alpha_weight, lsq_iters); break;
	case 5: bc7enc_refine_lsq_endpoints_iterated<BC7Mode<5> >(block, st, flags, alpha_weight, lsq_iters); break;
	case 6: bc7enc_refine_lsq_endpoints_iterated<BC7Mode<6> >(block, st, flags, alpha_weight, lsq_iters); break;
	case 7: bc7enc_refine_lsq_endpoints_iterated<BC7Mode<7> >(block, st, flags, alpha_weight, lsq_iters); break;
	}
}

static void bc7enc_refine_anneal_block(const U8 * block, BC7BlockState *st, BC7Flags flags, int iters, const U64 &rand_seed, bool no_reindex);

// bc7enc_find_endpoints unconditionally replaces contents
// return value is the number of bits changed
int bc7enc_find_endpoints(BC7BlockState *st,const U8 * block,  BC7Flags flags, bool change_pbits, bool slow_anneal)
{
	int alpha_weight = 1; // @@ TODO FIXME

	// mode, part, & indices held fixed
	//	find new endpoints
	// (are pbits free or not?)

	// need to wipe existing errs :
	st->err = BC7_ERROR_MAX;
	for LOOP(s,3)
	{
		st->subsets[s].err = BC7_ERROR_MAX;
		st->subsets[s].scalar_err = BC7_ERROR_MAX/2;
	}

	// @@ flags is not actually used by bc7enc_refine_lsq_endpoints_pass; remove it?
	// keep pbits option is lsq_iters -2
	int lsq_iters = change_pbits ? -1 : -2;
	bc7enc_refine_lsq_endpoints_pass(block,st,flags,alpha_weight,lsq_iters);
	
	RR_ASSERT( st->err != BC7_ERROR_MAX );
	
	if ( slow_anneal )
	{
		// could then greedy optimize endpoints to find further improvement
		
		/*
		int anneal_iters = 50; // @@ TWEAK ME as low as possible
		U64 rand_seed = rrRand64Simple_SeedFromU64Array((const U64 *)block,8); // 16*4 = 8 U64's
		bc7enc_refine_anneal_block(block,st,flags,anneal_iters,rand_seed,true);
		/*/
		// greedy only?
		bc7enc_refine_anneal_block(block,st,flags,-1,0,true);
		/**/
		
		// @@ compare anneal vs just doing greedy optimize? (for quality vs time)
		
		// maybe there's something even faster we could do here
		// for example just look at where the LSQ ideal endpoints were close to a bucket boundary
		//	and only try going +-1 across those boundaries
	}
	
	int bits = c_bc7_total_bits_endpoints[st->mode];
	if ( change_pbits )
		bits += c_bc7_total_bits_pbits[st->mode];
	
	return bits;
}

template<typename Mode>
static void bc7enc_refine_jitter_selectors(BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags, int iters, const U64 &rand_seed)
{
	const int num_inds = (Mode::ib2 != 0) ? 2 : 1;
	const int ib_masks[2] = { (1 << Mode::ib) - 1, (1 << Mode::ib2) - 1 };

	U64 rand_state = rand_seed;

	int num_pixels = in->num_pixels;
	for (int iter = 0; iter < iters; ++iter)
	{
		//int ni = rrRand64Simple_InRange(rrRand64Simple(&rand_state),1,4);
		int ni = (int)rrRand64Simple_NBits(rrRand64Simple(&rand_state),2) + 1;
		BC7SubsetState nst = *st;

		for (int n = 0; n < num_inds; ++n)
		{
			const int ib_mask = ib_masks[n];
			
			// ni is at most 4
			// so just rand outside the ni loop and pull from it 4 times
			U64 r = rrRand64Simple(&rand_state);
				
			for (int ii = 0; ii < ni; ++ii)
			{
				int i = rrRand64Simple_Mod(r,num_pixels); r <<= 7;
				int push = rrRand64Simple_InRange(r,-2,2); r <<= 5;
				
				int j = nst.idxs[n].ind[i];
				j = radtex_clamp<int>(j + push, 0, ib_mask);
				nst.idxs[n].ind[i] = static_cast<U8>(j);
			}
		}
		bc7enc_refine_lsq_endpoints_and_try<Mode>(st, &nst, in, flags);
	}
}

template<typename Mode>
static void bc7enc_refine_brute_pbits(BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags, int anchor_pos)
{
	if(!Mode::epb && !Mode::spb)
	{
		// ? do IDXS_EXACT ? just re-call _trial ?
		// pretty useless but whatever
	
		if ( flags & BC7ENC_BRUTE_PBITS_IDXS_EXACT )
		{
			bc7enc_trial<Mode>(st, in, flags | BC7ENC_IDXS_EXACT, 0, anchor_pos);			
		}
	
		return; // Nothing to brute force search
	}
	
	// bc7enc_refine_brute_pbits is the last step of refine_dispatch
	//	do exact indexes here :
	if ( flags & BC7ENC_BRUTE_PBITS_IDXS_EXACT )
		flags |= BC7ENC_IDXS_EXACT;

	BC7SubsetState nst = *st;

	// epb tries 4 combinations (0,1,2,3)
	// spb only tries 2 (0 and 3)
	for (int i = 0; i < 4; i += (Mode::epb ? 1 : 3))
		bc7enc_try_and_commit<Mode>(st, &nst, in, flags, i, anchor_pos);
}

typedef void BC7IterativeTrialFunc(void * user_ptr, BC7SubsetState * st, BC7Flags flags);

// take BC7IterativeTrialFunc as a func pointer to avoid making this a template,
// because the driver logic is the same for all modes and also both single-block
// and multi-block solves
static void bc7enc_refine_anneal_endpoints_core(int m, BC7SubsetState *st, BC7Flags flags, int iters, const U64 &rand_seed, BC7IterativeTrialFunc * trial, void * trial_user_ptr)
{
	if (!st->err || !iters)
		return;

	const BC7ModeDesc & mode = bc7_modes[m];	

	const int pbits = mode.epb + mode.spb; // epb and spb are 1 or 0 and sum is 1 or 0
	RR_ASSERT( pbits == 0 || pbits == 1 );
	const int cb_shift = radtex_max<int>(0, 8 - (mode.cb + pbits));
	const int ab_shift = radtex_max<int>(0, 8 - (mode.ab + pbits));
	const int max_step = 1 + pbits;
	// max_step<<cb_shift is a full step of a quantized endpoint
	//	if pbits != 0, then you also try half steps
	//	when you step in sub-endpoint pbit steps it may or may not happen depending on the other components
	//	also "trial" uses the heuristic pbit not brute force

	int shifts[4] = { cb_shift, cb_shift, cb_shift, ab_shift };

	BC7SubsetState current = *st;
	U64 rand_state = rand_seed;

	// 64 does seem to be a reasonable initial temperature
	const float start_temperature_flt = 64.f;
	float temperature_flt = start_temperature_flt;
	// Super-basic exponential cooling schedule
	//float decay = (float)rr_exp_approx(-(rr_loge(temperature_flt) + 0.5) / (RR_MAX(iters, 2) - 1.0));

	// number of color components :
	int nc = ((flags & BC7ENC_IGNORE_ALPHA) || mode.ab == 0) ? 3 : 4;
	//int comp_mask = (nc == 3) ? 7 : 15;
	
	for (int iter = 0; iter < iters; ++iter)
	{
		// linear cooling :
		float t = iter/(float)iters;
		/*
		// more than one cycle doesn't seem to help on rnd4k
		int ncycles = 2; // number of quenching cycles
		// simple sawtooth :
		t *= ncycles;
		t = t - (int)t; // fractional part
		*/
		temperature_flt = start_temperature_flt * (1.f - t);
		int temperature = (int)temperature_flt;
		// exponential cooling :
		//temperature_flt *= decay;

		BC7SubsetState part = current;

		U64 r = rrRand64Simple(&rand_state);

		// one random channel seems fine, no multi-channel steps necessary
		int c = rrRand64Simple_Mod(r, nc);
		r <<= 7;
		int start0 = part.endpoints[0].v[c];
		int start1 = part.endpoints[1].v[c];
		int e0,e1;
		for(;;)
		{
			// max_step is 1 with no pbits, 2 with pbits
			//	with pbits, shift is scaled to half steps, so a step of 2 is an endpoint step, 1 is a pbit step
			int d0 = rrRand64Simple_InRange(r,-max_step,max_step);
			r <<= 9;
			int d1 = rrRand64Simple_InRange(r,-max_step,max_step);
			r <<= 9;
			e0 = RR_CLAMP_255(start0 + (d0 << shifts[c]));
			e1 = RR_CLAMP_255(start1 + (d1 << shifts[c]));
			
			if ( e0 == start0 && e1 == start1 )
			{
				r = rrRand64Simple(&rand_state);				
				continue;
			}
			else
			{
				break;
			}
		}
		
		part.endpoints[0].v[c] = (U8)e0;
		part.endpoints[1].v[c] = (U8)e1;
		
		trial(trial_user_ptr, &part, flags);
		
		int derr = part.err - current.err;
		if ( derr >= temperature ) continue;
		/*
		// derr == 0 is very common
		// trace in here to make sure we actually made different endpoints
		//	(when pbits != 0 and the d steps are 1, you might not change endpoints, but if d is 2 you should)
		if ( derr == 0 )
		{
			trial(&current, in, flags);
			trial(&part, in, flags);
		}
		*/
		if (derr < 0 || (temperature > 0 && (int)rrRand64Simple_Mod(r,temperature) >= derr))
		{
			// [current] tracks where the anneal is wandering (including positive error steps)
			//	[st] stores the best found
			current = part;
			if (part.err < st->err)
			{
				*st = part;
				if (st->err == 0)
					return;
			}
		}
	}
}

static void bc7enc_refine_greedyoptimize_endpoints_core(int m, BC7SubsetState *st, BC7Flags flags, BC7IterativeTrialFunc * trial, void * trial_user_ptr)
{
	if (!st->err )
		return;

	const BC7ModeDesc & mode = bc7_modes[m];	
	
	// a step of 1<<shift should be a step in the bottom bit of a quantized endpoint
	const int cb_shift = radtex_max<int>(0, 8 - (mode.cb));
	const int ab_shift = radtex_max<int>(0, 8 - (mode.ab));
	int shifts[4] = { cb_shift, cb_shift, cb_shift, ab_shift };

	// number of color components :
	int nc = ((flags & BC7ENC_IGNORE_ALPHA) || mode.ab == 0) ? 3 : 4;
	
	for(;;)
	{
		BC7SubsetState cur = *st;
		bool did_anything = false;
		
		// find the best endpoint step from [cur] and do it
		// repeat while any improvement is found
		// (we don't just take the first found improvement)
		
		// one component axis at a time
		for LOOP(c,nc)
		{
			// if subset bbox is zero on this axis, skip it ?
						
			#if 0
			int start0 = cur.endpoints[0].v[c];
			int start1 = cur.endpoints[1].v[c];
			
			// wiggle each endpoint +1,-1,0 :
			//	this tries all the AND wiggles, 3*3-1 = 8 trials
			//	if you just tried the OR wiggles, 2+2 = 4
			// or you could do the AND wiggles but skip the ones where both ends move in the same direction = 6
			for LOOP(wiggle0,3)
			{
				int e0 = RR_CLAMP_255(start0 + ((wiggle0-1) << shifts[c]));
				cur.endpoints[0].v[c] = (U8) e0;
				for LOOP(wiggle1,3)
				{
					int e1 = RR_CLAMP_255(start1 + ((wiggle1-1) << shifts[c]));
					if ( e0 == start0 && e1 == start1 )
						continue;
					cur.endpoints[1].v[c] = (U8) e1;
					
					trial(&cur, in, flags);
					if ( cur.err < st->err )
					{
						*st = cur;
						if (st->err == 0)
							return;
						did_anything = true;
					}
				}
			}
						
			// put it back :
			cur.endpoints[0].v[c] = (U8) start0;
			cur.endpoints[1].v[c] = (U8) start1;
			#else
			
			// one end or the other, not both = 2+2 = 4 trials
			for LOOP(ei,2)
			{
				int start = cur.endpoints[ei].v[c];
				
				for (int delta=-1;delta<=1;delta+=2)
				{
					int wiggled = start + (delta << shifts[c]);
					if ( (U32)wiggled > 255 ) continue; // overflowed
					cur.endpoints[ei].v[c] = (U8) wiggled;
						
					trial(trial_user_ptr, &cur, flags);
					if ( cur.err < st->err )
					{
						*st = cur;
						if (st->err == 0)
							return;
						did_anything = true;
					}
				}
							
				// put it back :
				cur.endpoints[ei].v[c] = (U8) start;
			}
						
			#endif
		}

		if ( ! did_anything )
			break;
	}
}

template<typename Mode>
static void bc7enc_iterative_trial_for_anneal(void * user_ptr, BC7SubsetState *st, BC7Flags flags)
{
	const BC7SubsetInput * in = (const BC7SubsetInput *)user_ptr;
	bc7enc_trial<Mode>(st,in,flags,-1,-1);
}

template<typename Mode>
static void bc7enc_iterative_trial_noreindex(void * user_ptr, BC7SubsetState *st, BC7Flags flags)
{
	const BC7SubsetInput * in = (const BC7SubsetInput *)user_ptr;
	int pbits = st->pbits; // keeps them locked, -1 to change
	bc7enc_update_subset_error<Mode>(st,in,flags,pbits);
}

template<typename Mode>
static void bc7enc_refine_anneal_endpoints(BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags, int iters, const U64 &rand_seed, bool no_reindex)
{
	// anneal is only on in Reference

	void * trial_user_ptr = (void *)in;
	BC7IterativeTrialFunc * trial;
	if ( no_reindex )
		trial = bc7enc_iterative_trial_noreindex<Mode>;
	else
		trial = bc7enc_iterative_trial_for_anneal<Mode>;
 
	// iters negative means greedy only
 
	if ( iters > 0 )
	{
		bc7enc_refine_anneal_endpoints_core(Mode::index, st, flags, iters, rand_seed, trial, trial_user_ptr);
	}
	
	// high iters = anneal + greedy
	if ( iters >= 100 || iters < 0 )
	{
		// greedyoptimize only on the final single block (iters_late)
		bc7enc_refine_greedyoptimize_endpoints_core(Mode::index, st, flags, trial, trial_user_ptr);
	}
}


template<typename Mode>
static void bc7enc_refine_anneal_subset(const U8 * block, BC7BlockState *st, BC7Flags flags, int iters, const U64 &rand_seed, bool no_reindex)
{
	int alpha_weight = 1; // @@ TODO FIXME
	RR_ASSERT(st->mode == Mode::index);

	BC7BlockInput block_in;
	block_in.init(block, radtex_subset_to_inds[Mode::ns-1][st->p], st->subsets[0].rbit);

	BC7Error total_err = 0;
	int total_pixels = 0;
	for (int s = 0; s < Mode::ns; ++s)
	{
		BC7SubsetState *sst = st->subsets + s;
		const int num_pixels = radtex_num_pixels_per_subset[Mode::ns][st->p][s];

		BC7SubsetInput input;
		input.init(block_in.pixels + total_pixels*4, num_pixels, alpha_chan_index(sst), alpha_weight);

		if (sst->err)
		{
			bc7enc_refine_anneal_endpoints<Mode>(sst, &input, flags, iters, rand_seed, no_reindex);
		}

		total_err += sst->err;
		total_pixels += num_pixels;
	}
	RR_ASSERT( total_pixels == 16 );

	st->err = total_err;
}

static void bc7enc_refine_anneal_block(const U8 * block, BC7BlockState *st, BC7Flags flags, int iters, const U64 &rand_seed, bool no_reindex)
{
	switch(st->mode) {
	case 0: bc7enc_refine_anneal_subset<BC7Mode<0> >(block, st, flags, iters, rand_seed, no_reindex); break;
	case 1: bc7enc_refine_anneal_subset<BC7Mode<1> >(block, st, flags, iters, rand_seed, no_reindex); break;
	case 2: bc7enc_refine_anneal_subset<BC7Mode<2> >(block, st, flags, iters, rand_seed, no_reindex); break;
	case 3: bc7enc_refine_anneal_subset<BC7Mode<3> >(block, st, flags, iters, rand_seed, no_reindex); break;
	case 4: bc7enc_refine_anneal_subset<BC7Mode<4> >(block, st, flags, iters, rand_seed, no_reindex); break;
	case 5: bc7enc_refine_anneal_subset<BC7Mode<5> >(block, st, flags, iters, rand_seed, no_reindex); break;
	case 6: bc7enc_refine_anneal_subset<BC7Mode<6> >(block, st, flags, iters, rand_seed, no_reindex); break;
	case 7: bc7enc_refine_anneal_subset<BC7Mode<7> >(block, st, flags, iters, rand_seed, no_reindex); break;
	}
}

// https://codedeposit.blogspot.com/2018/03/bc7-endpoint-search-using-endpoint.html
template<typename Mode>
static void bc7enc_refine_extrapolate_selectors(BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags)
{
	static const float adjust[3][2] =
	{
		{ 1.0f,  0.0f },
		{ 0.0f, -1.0f },
		{ 1.0f, -1.0f },
	};

	try_linear_transforms<Mode>(st, in, st->endpoints[0], st->endpoints[1], flags, adjust, 3);
}

// Modified version of scale_selectors that just messes with the endpoints. It turns out
// that the key was to not do full-size steps but instead slide around more gradually,
// so really we're mostly probing different roundings of the endpoints here.
template<typename Mode>
static void bc7enc_refine_slide_selectors(BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags)
{
	static const float adjust[8][2] =
	{
		{ -0.15f, -0.15f }, {  0.00f, -0.50f }, {  0.15f, -0.15f },
		{ -0.50f,  0.00f },                     {  0.50f,  0.00f },
		{ -0.15f,  0.15f }, {  0.00f,  0.50f }, {  0.15f,  0.15f },
	};

	try_linear_transforms<Mode>(st, in, st->endpoints[0], st->endpoints[1], flags, adjust, 8);
}

template<typename Mode>
static void bc7enc_refine_scale_selectors(BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags)
{
	const int num_inds = (Mode::ib2 != 0) ? 2 : 1;
	const int ib_masks[2] = { (1 << Mode::ib) - 1, (1 << Mode::ib2) - 1 };

	int num_pixels = in->num_pixels;
	for(int ly = -1; ly <= 1; ++ly)
	{
		for(int hy = -1; hy <= 1; ++hy)
		{
			if (ly == 0 && hy == 0)
				continue;

			BC7SubsetState nst = *st;
			for (int n = 0; n < num_inds; ++n)
			{
				const int ib_mask = ib_masks[n];
				const int range = ib_mask + hy - ly;
				for (int i = 0; i < num_pixels; ++i)
				{
					int j = st->idxs[n].ind[i];
					j = radtex_clamp<int>((2 * ib_mask * (j - ly) / range + 1) / 2, 0, ib_mask);
					nst.idxs[n].ind[i] = static_cast<U8>(j);
				}
			}
			bc7enc_refine_lsq_endpoints_and_try<Mode>(st, &nst, in, flags);
		}
	}
}

static void scalar_calc_subset_pca3(float mean[4], float pca[4], const BC7SubsetInput *in)
{
	float m[4] = {};
	float cov[6] = {};

	// Determine the covariance matrix for this subset
	// Start with vanilla sum-of-squares
	for (int i = 0; i < in->num_pixels; ++i)
	{
		float a0 = in->pixels[i*4+0];
		float a1 = in->pixels[i*4+1];
		float a2 = in->pixels[i*4+2];
		float a3 = in->pixels[i*4+3];
		m[0] += a0;
		m[1] += a1;
		m[2] += a2;
		m[3] += a3;
		cov[0] += a0 * a0; // rr
		cov[1] += a0 * a1; // rg
		cov[2] += a0 * a2; // rb
		cov[3] += a1 * a1; // gg
		cov[4] += a1 * a2; // gb
		cov[5] += a2 * a2; // bb
	}

	// Finalize mean (divide by count)
	float sf = in->rcp_num_pixels;
	for (int i = 0; i < 4; ++i)
	{
		m[i] *= sf;
		mean[i] = m[i];
	}

	// Divide sum-of-squares matrix by count and subtract out mean*mean^T to get the actual covariance matrix
	cov[0] = cov[0]*sf - m[0]*m[0];
	cov[1] = cov[1]*sf - m[0]*m[1];
	cov[2] = cov[2]*sf - m[0]*m[2];
	cov[3] = cov[3]*sf - m[1]*m[1];
	cov[4] = cov[4]*sf - m[1]*m[2];
        cov[5] = cov[5]*sf - m[2]*m[2];

	float local_pca[3];
	for (int i = 0; i < 3; ++i)
		local_pca[i] = pca[i];

	// Power iteration to find the eigenvector corresponding to the largest absolute eigenvalue.
	//
	// NOTE(fg): with the parameters we use, iters <= 3, and we're using floats with a covariance
	// matrix computed from 8-bit numbers, so there's really no need to renormalize inside the
	// iteration at all, just normalizing once at the end is fine.
	for(int iter = 0; iter < PCA_POWER_ITERS; ++iter)
	{
		// NOTE(fg): the vector versions store the matrix by diagonals and thus sum in a slightly
		// unconventional order; match the order of summation here to get the same results
		float x = cov[0]*local_pca[0] + cov[1]*local_pca[1] + cov[2]*local_pca[2];
		float y = cov[3]*local_pca[1] + cov[4]*local_pca[2] + cov[1]*local_pca[0];
		float z = cov[5]*local_pca[2] + cov[2]*local_pca[0] + cov[4]*local_pca[1];
		local_pca[0] = x;
		local_pca[1] = y;
		local_pca[2] = z;
	}

	// NOTE(fg): our SIMD versions use sum_across which sums with 2-away, then with 1-away,
	// so calculation order must be this:
	float len_sq = (radtex_sqr(local_pca[0]) + radtex_sqr(local_pca[2])) + radtex_sqr(local_pca[1]);
	if (len_sq > 0.0f)
	{
		float len = sqrtf(len_sq); // NOTE(fg): divide not mul by recip so we match SSE version exactly
		for(int c = 0; c < 3; ++c)
			pca[c] = local_pca[c] / len;
	}
	else
	{
		for(int c = 0; c < 3; ++c)
			pca[c] = 0.0f;
	}
}

static void scalar_calc_subset_pca4(float mean[4], float pca[4], const BC7SubsetInput *in)
{
	float m[4] = {};
	float cov[10] = {};

	// Determine the covariance matrix for this subset
	// Start with vanilla sum-of-squares
	for (int i = 0; i < in->num_pixels; ++i)
	{
		float a0 = in->pixels[i*4+0];
		float a1 = in->pixels[i*4+1];
		float a2 = in->pixels[i*4+2];
		float a3 = in->pixels[i*4+3];
		m[0] += a0;
		m[1] += a1;
		m[2] += a2;
		m[3] += a3;
		cov[0] += a0 * a0; // rr
		cov[1] += a0 * a1; // rg
		cov[2] += a0 * a2; // rb
		cov[3] += a0 * a3; // ra
		cov[4] += a1 * a1; // gg
		cov[5] += a1 * a2; // gb
		cov[6] += a1 * a3; // ga
		cov[7] += a2 * a2; // bb
		cov[8] += a2 * a3; // ba
		cov[9] += a3 * a3; // aa
	}

	// Finalize mean (divide by count)
	float sf = in->rcp_num_pixels;
	for (int i = 0; i < 4; ++i)
	{
		m[i] *= sf;
		mean[i] = m[i];
	}

	// Divide sum-of-squares matrix by count and subtract out mean*mean^T to get the actual covariance matrix
	cov[0] = cov[0]*sf - m[0]*m[0];	// rr
	cov[1] = cov[1]*sf - m[0]*m[1];	// rg
	cov[2] = cov[2]*sf - m[0]*m[2];	// rb
	cov[3] = cov[3]*sf - m[0]*m[3];	// ra
	cov[4] = cov[4]*sf - m[1]*m[1];	// gg
	cov[5] = cov[5]*sf - m[1]*m[2];	// gb
	cov[6] = cov[6]*sf - m[1]*m[3];	// ga
	cov[7] = cov[7]*sf - m[2]*m[2];	// bb
	cov[8] = cov[8]*sf - m[2]*m[3];	// ba
	cov[9] = cov[9]*sf - m[3]*m[3];	// aa

	float local_pca[4];
	for (int i = 0; i < 4; ++i)
		local_pca[i] = pca[i];

	// Power iteration to find the eigenvector corresponding to the largest absolute eigenvalue.
	//
	// NOTE(fg): with the parameters we use, iters <= 3, and we're using floats with a covariance
	// matrix computed from 8-bit numbers, so there's really no need to renormalize inside the
	// iteration at all, just normalizing once at the end is fine.
	for(int iter = 0; iter < PCA_POWER_ITERS; ++iter)
	{
		// NOTE(fg): the vector versions store the matrix by diagonals and thus sum in a slightly
		// unconventional order; match the order of summation here to get the same results
		float x = cov[0]*local_pca[0] + cov[1]*local_pca[1] + cov[2]*local_pca[2] + cov[3]*local_pca[3];
		float y = cov[4]*local_pca[1] + cov[5]*local_pca[2] + cov[6]*local_pca[3] + cov[1]*local_pca[0];
		float z = cov[7]*local_pca[2] + cov[8]*local_pca[3] + cov[2]*local_pca[0] + cov[5]*local_pca[1];
		float w = cov[9]*local_pca[3] + cov[3]*local_pca[0] + cov[6]*local_pca[1] + cov[8]*local_pca[2];
		local_pca[0] = x;
		local_pca[1] = y;
		local_pca[2] = z;
		local_pca[3] = w;
	}

	// NOTE(fg): our SIMD versions use sum_across which sums with 2-away, then with 1-away,
	// so calculation order must be this:
	float len_sq = (radtex_sqr(local_pca[0]) + radtex_sqr(local_pca[2])) + (radtex_sqr(local_pca[1]) + radtex_sqr(local_pca[3]));
	if (len_sq > 0.0f)
	{
		float len = sqrtf(len_sq); // NOTE(fg): divide not mul by recip so we match SSE version exactly
		for(int c = 0; c < 4; ++c)
			pca[c] = local_pca[c] / len;
	}
	else
	{
		for(int c = 0; c < 4; ++c)
			pca[c] = 0.0f;
	}
}

#ifdef PCA_REFACTOR
// Swaps the channel c with alpha (index 3)
static inline void swap_channel(float pca[4], int c)
{
	radtex_swap(pca[c], pca[3]);
}

// Zeroes the correct channel based upon idx [0..4] where:
//	1 -> zero alpha
//	2 -> zero red
//	3 -> zero green
//	4 -> zero blue
// Otherwise, do nothing.
static void zero_channel(float pca[4], int idx)
{
	switch (idx)
	{
	case 1: pca[3] = 0.0f; break;
	case 2: pca[0] = 0.0f; break;
	case 3: pca[1] = 0.0f; break;
	case 4: pca[2] = 0.0f; break;
	default: break;
	}
}

// Prepare 5 vectors worth of pca from the 4-comp, seed. The result, pca,
// contains the 4-comp and 3-comp with component swaps:
//	pca[0] - 4-comp - rgba
//	pca[1] - 3-comp - rgb_
//	pca[2] - 3-comp - _gba
//	pca[3] - 3-comp - r_ba
//	pca[4] - 3-comp - rg_a
static void scalar_calc_combo_pca(float pca[5][4], const float seed[4], const float covar[12])
{
	float local_pca[5][4];
	for LOOP(i, 5)
	{
		memcpy(local_pca[i], seed, 4*sizeof(float));
		zero_channel(local_pca[i], i);
	}

	// Power iteration to find the eigenvector corresponding to the largest absolute eigenvalue.
	//
	// NOTE(fg): with the parameters we use, iters <= 3, and we're using floats with a covariance
	// matrix computed from 8-bit numbers, so there's really no need to renormalize inside the
	// iteration at all, just normalizing once at the end is fine.
	for(int iter = 0; iter < PCA_POWER_ITERS; ++iter)
	{
		for LOOP(i, 5)
		{
			// NOTE(fg): the vector versions store the matrix by diagonals and thus sum in a slightly
			// unconventional order; match the order of summation here to get the same results

			// covar[0..3] stores diagonal elements:      rr, gg, bb, aa
			// covar[4..7] stores superdiagonal elements: rg, gb, ba, ra
			// covar[8..9] is the rest:                   rb, ga
			float x = covar[0]*local_pca[i][0] + covar[4]*local_pca[i][1] + covar[8]*local_pca[i][2] + covar[7]*local_pca[i][3];
			float y = covar[1]*local_pca[i][1] + covar[5]*local_pca[i][2] + covar[9]*local_pca[i][3] + covar[4]*local_pca[i][0];
			float z = covar[2]*local_pca[i][2] + covar[6]*local_pca[i][3] + covar[8]*local_pca[i][0] + covar[5]*local_pca[i][1];
			float w = covar[3]*local_pca[i][3] + covar[7]*local_pca[i][0] + covar[9]*local_pca[i][1] + covar[6]*local_pca[i][2];
			local_pca[i][0] = x;
			local_pca[i][1] = y;
			local_pca[i][2] = z;
			local_pca[i][3] = w;

			zero_channel(local_pca[i], i);
		}
	}

	for LOOP(i, 5)
	{
		// NOTE(fg): our SIMD versions use sum_across which sums with 2-away, then with 1-away,
		// so calculation order must be this:
		float len_sq = (radtex_sqr(local_pca[i][0]) + radtex_sqr(local_pca[i][2])) + (radtex_sqr(local_pca[i][1]) + radtex_sqr(local_pca[i][3]));
		if (len_sq > 0.0f)
		{
			float len = sqrtf(len_sq); // NOTE(fg): divide not mul by recip so we match SSE version exactly
			for(int c = 0; c < 4; ++c)
				pca[i][c] = local_pca[i][c] / len;
		}
		else
		{
			for(int c = 0; c < 4; ++c)
				pca[i][c] = 0.0f;
		}
	}
}
#endif // PCA_REFACTOR

#ifdef DO_BUILD_SSE4

static void sse4_calc_subset_pca3(float mean[4], float pca[4], const BC7SubsetInput *in)
{
	// We store the covar matrix by diagonals, which looks funky but reduces the number
	// of shuffles we need to do:
	//
	// cov0 stores diagonal elements:      [0][0], [1][1], [2][2], and 0 (padding)
	// cov1 stores superdiagonal elements: [0][1], [1][2], [0][2], and 0 (padding)
	VecF32x4 m = VecF32x4::zero();
	VecF32x4 cov0 = VecF32x4::zero();
	VecF32x4 cov1 = VecF32x4::zero();

	// Determine the covariance matrix for this subset
	// Start with vanilla sum-of-squares
	for (int i = 0; i < in->num_pixels; ++i)
	{
		Vec128_S32 pixint = Vec128_U8::loadu_lo32(in->pixels + i*4).to_sint32_lo();
		VecF32x4 a = pixint.to_f32();

		m += a;
		cov0 += a * a;
		cov1 += a * a.yzxw();
	}

	// Finalize mean (divide by count)
	VecF32x4 sf(in->rcp_num_pixels);
	m *= sf;
	m.storeu(mean);

	// Divide sum-of-squares matrix by count and subtract out mean*mean^T to get the actual covariance matrix
	cov0 = cov0 * sf - m * m;
	cov1 = cov1 * sf - m * m.yzxw();

	// Set up for power iteration
	VecF32x4 local_pca = VecF32x4::loadu(pca);
	VecF32x4 cov2 = cov1.zxyw();

	// Power iteration to find the eigenvector corresponding to the largest absolute eigenvalue.
	//
	// NOTE(fg): with the parameters we use, iters <= 3, and we're using floats with a covariance
	// matrix computed from 8-bit numbers, so there's really no need to renormalize inside the
	// iteration at all, just normalizing once at the end is fine.
	for(int iter = 0; iter < PCA_POWER_ITERS; ++iter)
		local_pca = local_pca * cov0 + local_pca.yzxw() * cov1 + local_pca.zxyw() * cov2;

	// Normalize result
	VecF32x4 len_sq = (local_pca * local_pca).sum_across();
	VecF32x4 len_positive = len_sq.cmp_gt(VecF32x4::zero());
	VecF32x4 final_pca = (local_pca / VecF32x4r_sqrt(len_sq)) & len_positive;

	#ifdef SCALAR_PARANOIA
	// these should match exactly
	float ref_mean[4], ref_pca[4];
	memcpy(ref_pca, pca, 4 * sizeof(pca[0]));
	scalar_calc_subset_pca3(ref_mean, ref_pca, in);

	// NOTE(fg):
	// Don't test for bitwise equality in the final_pca w component since
	// the SIMD ver computes it and the ref variant doesn't, just check that
	// the vector w compares equal to 0, so we accept -0 too.
        RR_ASSERT(memcmp(ref_mean, mean, 4*sizeof(float)) == 0);
	RR_ASSERT(memcmp(ref_pca, &final_pca, 3*sizeof(float)) == 0 && final_pca.wwww().scalar_x() == 0.0f);
	#endif

	final_pca.storeu(pca);
}

static void sse4_calc_subset_pca4(float mean[4], float pca[4], const BC7SubsetInput *in)
{
	// We store the covar matrix by diagonals (with wrap-around), which looks funky
	// but reduces the number of shuffles we need to do:
	//
	// cov0 stores diagonal elements:      [0][0], [1][1], [2][2], [3][3]
	// cov1 stores superdiagonal elements: [0][1], [1][2], [2][3], [3][0]
	// cov2 is the rest:                   [0][2], [1][3], [2][0], [3][1]
	VecF32x4 m = VecF32x4::zero();
	VecF32x4 cov0 = VecF32x4::zero();
	VecF32x4 cov1 = VecF32x4::zero();
	VecF32x4 cov2 = VecF32x4::zero();

	// Determine the covariance matrix for this subset
	// Start with vanilla sum-of-squares
	for (int i = 0; i < in->num_pixels; ++i)
	{
		Vec128_S32 pixint = Vec128_U8::loadu_lo32(in->pixels + i*4).to_sint32_lo();
		VecF32x4 a = pixint.to_f32();

		m += a;
		cov0 += a * a;
		cov1 += a * a.yzwx();
		cov2 += a * a.zwxy();
	}

	// Finalize mean (divide by count)
	VecF32x4 sf(in->rcp_num_pixels);
	m *= sf;
	m.storeu(mean);

	// Divide sum-of-squares matrix by count and subtract out mean*mean^T to get the actual covariance matrix
	cov0 = cov0 * sf - m * m;
	cov1 = cov1 * sf - m * m.yzwx();
	cov2 = cov2 * sf - m * m.zwxy();

	// Set up for power iteration
	VecF32x4 local_pca = VecF32x4::loadu(pca);
	VecF32x4 cov3 = cov1.wxyz();

	// Power iteration to find the eigenvector corresponding to the largest absolute eigenvalue.
	//
	// NOTE(fg): with the parameters we use, iters <= 3, and we're using floats with a covariance
	// matrix computed from 8-bit numbers, so there's really no need to renormalize inside the
	// iteration at all, just normalizing once at the end is fine.
	for(int iter = 0; iter < PCA_POWER_ITERS; ++iter)
	{
		local_pca = local_pca * cov0 +
			local_pca.yzwx() * cov1 +
			local_pca.zwxy() * cov2 +
			local_pca.wxyz() * cov3;
	}

	// Normalize result
	VecF32x4 len_sq = (local_pca * local_pca).sum_across();
	VecF32x4 len_positive = len_sq.cmp_gt(VecF32x4::zero());
	VecF32x4 final_pca = (local_pca / VecF32x4r_sqrt(len_sq)) & len_positive;

	#ifdef SCALAR_PARANOIA
	// these should match exactly
	float ref_mean[4], ref_pca[4];
	memcpy(ref_pca, pca, 4 * sizeof(pca[0]));
	scalar_calc_subset_pca4(ref_mean, ref_pca, in);

        RR_ASSERT(memcmp(ref_mean, mean, 4*sizeof(float)) == 0);
	RR_ASSERT(memcmp(ref_pca, &final_pca, 4*sizeof(float)) == 0);
	#endif

	final_pca.storeu(pca);
}

#ifdef PCA_REFACTOR
static void sse4_calc_combo_pca(float pca[5][4], const float seed[4], const float covar[12])
{
	// Set up for power iteration
	VecF32x4 zero = VecF32x4::zero();
	VecF32x4 local_pca[5] = {
		VecF32x4::loadu(seed),
		_mm_blend_ps(VecF32x4::loadu(seed), zero, 0x8),
		_mm_blend_ps(VecF32x4::loadu(seed), zero, 0x1),
		_mm_blend_ps(VecF32x4::loadu(seed), zero, 0x2),
		_mm_blend_ps(VecF32x4::loadu(seed), zero, 0x4),
	};
	VecF32x4 cov0 = VecF32x4::loadu(&covar[0]);
	VecF32x4 cov1 = VecF32x4::loadu(&covar[4]);
	VecF32x4 cov2 = VecF32x4::loadu(&covar[8]);
	VecF32x4 cov3 = cov1.wxyz();

	// Power iteration to find the eigenvector corresponding to the largest absolute eigenvalue.
	//
	// NOTE(fg): with the parameters we use, iters <= 3, and we're using floats with a covariance
	// matrix computed from 8-bit numbers, so there's really no need to renormalize inside the
	// iteration at all, just normalizing once at the end is fine.
	for(int iter = 0; iter < PCA_POWER_ITERS; ++iter)
	{
#define MULT(local_pca) \
	local_pca = local_pca * cov0 + local_pca.yzwx() * cov1 + local_pca.zwxy() * cov2 + local_pca.wxyz() * cov3

		MULT(local_pca[0]);
		MULT(local_pca[1]);
		local_pca[1] = _mm_blend_ps(local_pca[1], zero, 0x8);
		MULT(local_pca[2]);
		local_pca[2] = _mm_blend_ps(local_pca[2], zero, 0x1);
		MULT(local_pca[3]);
		local_pca[3] = _mm_blend_ps(local_pca[3], zero, 0x2);
		MULT(local_pca[4]);
		local_pca[4] = _mm_blend_ps(local_pca[4], zero, 0x4);

#undef MULT
	}

	// Normalize result
	for LOOP(i, 5)
	{
		VecF32x4 len_sq = (local_pca[i] * local_pca[i]).sum_across();
		VecF32x4 len_positive = len_sq.cmp_gt(zero);
		VecF32x4 final_pca = (local_pca[i] / VecF32x4r_sqrt(len_sq)) & len_positive;
		final_pca.storeu(pca[i]);
	}
	#ifdef SCALAR_PARANOIA
	// these should match exactly
	float ref_pca[5][4];
	scalar_calc_combo_pca(ref_pca, seed, covar);

	for LOOP(i, 5)
		RR_ASSERT(memcmp(ref_pca[i], pca[i], 4*sizeof(float)) == 0);
	#endif
}
#endif // PCA_REFACTOR
#endif

static void calc_subset_pca3(float mean[4], float pca[4], const BC7SubsetInput *in)
{
#ifdef DO_BUILD_SSE4
	sse4_calc_subset_pca3(mean, pca, in);
#else
	scalar_calc_subset_pca3(mean, pca, in);
#endif
}

static void calc_subset_pca4(float mean[4], float pca[4], const BC7SubsetInput *in)
{
#ifdef DO_BUILD_SSE4
	sse4_calc_subset_pca4(mean, pca, in);
#else
	scalar_calc_subset_pca4(mean, pca, in);
#endif
}

#ifdef PCA_REFACTOR
static void calc_combo_pca(float pca[5][4], const float seed[4], const float covar[12])
{
#ifdef DO_BUILD_SSE4
	sse4_calc_combo_pca(pca, seed, covar);
#else
	scalar_calc_combo_pca(pca, seed, covar);
#endif
}
#endif

static int bc7enc_refine_pca_core(BC7SubsetEndpoints out_info[], int out_info_size, 
	const BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags,
	const BC7EncOptions *opt, const BC7Prep *prep, const int comp, int ns, bool has_ib2)
{
	RR_ASSERT(out_info_size > 0);

	float mean[4], pca[4];

	// seed PCA vector
	pca[3] = 0.0f; // make sure this is initialized even if comp==3
	for (int i = 0; i < comp; i++)
		pca[i] = static_cast<float>(st->endpoints[1].v[i] - st->endpoints[0].v[i]);

	if (comp == 3)
	{
		calc_subset_pca3(mean, pca, in); // this preserves pca[3]==0
	}
	else
	{
		calc_subset_pca4(mean, pca, in);
	}

	// Project pixels on the principal axis, take min and max projection as endpoints.
	static const int MAXDOTS = 16;
	float dots[MAXDOTS];
	RR_ASSERT(in->num_pixels <= MAXDOTS);

#ifdef DO_BUILD_SSE4
	VecF32x4 vsum_sqr = VecF32x4::zero();
	VecF32x4 vminmax_dot = VecF32x4(FLT_MAX); // accumulates min in x, -max in y
	VecF32x4 vmean = VecF32x4::loadu(mean);
	VecF32x4 vpca = VecF32x4::loadu(pca);
	// NOTE(fg): NOT GREAT
	// transpose me if possible (but ugh)
	for (int i = 0; i < in->num_pixels; ++i)
	{
		Vec128_S32 pixint = Vec128_U8::loadu_lo32(in->pixels + i*4).to_sint32_lo();
		VecF32x4 pixels = pixint.to_f32();
		VecF32x4 dot = ((pixels - vmean) * vpca).sum_across();
		_mm_store_ss(&dots[i], dot);
		vsum_sqr += dot * dot;
		vminmax_dot = vmin(vminmax_dot, dot ^ VecF32x4(0.0f,-0.0f,0.0f,0.0f));
	}
	float sum_sqr = vsum_sqr.scalar_x();
	float min_dot = vminmax_dot.scalar_x();
	float max_dot = -vminmax_dot.yyyy().scalar_x();
#else
	float sum_sqr = 0.0f;
	float min_dot = FLT_MAX;
	float max_dot = -FLT_MAX;
	for (int i = 0; i < in->num_pixels; ++i)
	{
		// Compute dot product with summation matching the sum_across above exactly
		float dot0 = (in->pixels[i*4+0] - mean[0]) * pca[0];
		float dot1 = (in->pixels[i*4+1] - mean[1]) * pca[1];
		float dot2 = (in->pixels[i*4+2] - mean[2]) * pca[2];
		float dot3 = (in->pixels[i*4+3] - mean[3]) * pca[3];
		float dot = (dot0 + dot2) + (dot1 + dot3);
		dots[i] = dot;
		sum_sqr += dot * dot;
		min_dot = radtex_min(min_dot, dot);
		max_dot = radtex_max(max_dot, dot);
	}
#endif

	TRACE("  pca sum_sqr=%.9e min=%.9e max=%.9e\n", sum_sqr, min_dot, max_dot);

	// NOTE(fg): not tweaked; this should probably depend on endpoint res
	const float min_diff = 4.0f;

	if (max_dot - min_dot <= min_diff)
	{
		F32 avg = 0.5f * (min_dot + max_dot);
		min_dot = avg - min_diff * 0.5f;
		max_dot = avg + min_diff * 0.5f;
	}

	// NOTE(fg): the idea about the shrink here is that min_dot and max_dot are at extremes,
	// and if we put the endpoints spot-on, we reduce the expected quantization error for those
	// pixels at the expense of everyone else. Instead, shrink the variation slightly which gives
	// us slightly better resolution inside and puts the extremal pixels closer to the edge of
	// their quantization bucket, rather than smack in the middle.
	const float shrink_factor = 1.0f / 64.0f;
	float shrink = (max_dot - min_dot) * shrink_factor;
	float smin_dot = min_dot + shrink;
	float smax_dot = max_dot - shrink;
	int count = 0;

#ifdef DO_BUILD_SSE4
	store_u8_endpoints(out_info[count].endpoints, vmean + vpca * VecF32x4(smin_dot), vmean + vpca * VecF32x4(smax_dot));
#else
	for (int c = 0; c < 4; ++c)
	{
		out_info[count].endpoints[0].v[c] = round_and_clamp_U8(mean[c] + pca[c] * smin_dot);
		out_info[count].endpoints[1].v[c] = round_and_clamp_U8(mean[c] + pca[c] * smax_dot);
	}
#endif

	// if we have a second set of index bits, the alpha channel is for all practical purposes its own thing,
	// so set that up as well.
	float sum_sqr_a = 0;
	float min_a = FLT_MAX;
	float max_a = -FLT_MAX;
	float mean_a = mean[3];

	if (has_ib2)
	{
		for (int i = 0; i < in->num_pixels; ++i)
		{
			float a = static_cast<float>(in->pixels[i*4+3]) - mean_a;
			min_a = radtex_min(min_a, a);
			max_a = radtex_max(max_a, a);
			sum_sqr_a += a * a;
		}

		if (max_a - min_a <= min_diff)
		{
			F32 avg = 0.5f * (min_a + max_a);
			min_a = avg - min_diff * 0.5f;
			max_a = avg + min_diff * 0.5f;
		}

		float shrink_a = (max_a - min_a) * shrink_factor;
		float smin_a = min_a + shrink_a;
		float smax_a = max_a - shrink_a;
		out_info[count].endpoints[0].a = round_and_clamp_U8(mean_a + smin_a);
		out_info[count].endpoints[1].a = round_and_clamp_U8(mean_a + smax_a);
	}

	count++;

	if (flags & BC7ENC_PCA_STRETCH)
	{
		// NOTE(fg): E[dot] = 0 because we subtracted out the subset mean during the dot product calc
		float variance = sum_sqr * in->rcp_num_pixels;
		float sdev = sqrtf(variance);
		const int num_passes = opt->max_pca_stretch_passes;

		float base = 1.50f * sdev;
		float step = 1.0f / (num_passes - 1.0f) * sdev;

		float sdev_a = 0.0f;
		float base_a = 0.0f;
		float step_a = 0.0f;
		if (has_ib2)
		{
			sdev_a = sqrtf(sum_sqr_a * in->rcp_num_pixels);
			base_a = 1.50f * sdev_a;
			step_a = 1.0f / (num_passes - 1.0f) * sdev_a;
		}

		for (int pass = 0; pass < num_passes; ++pass)
		{
			float spread = base + (float)pass * step;
#ifdef DO_BUILD_SSE4
			VecF32x4 delta = VecF32x4(spread) * vpca;
			store_u8_endpoints(out_info[count].endpoints, vmean - delta, vmean + delta);
#else
			for (int c = 0; c < 4; ++c)
			{
				float delta = spread * pca[c];
				out_info[count].endpoints[0].v[c] = round_and_clamp_U8(mean[c] - delta);
				out_info[count].endpoints[1].v[c] = round_and_clamp_U8(mean[c] + delta);
			}
#endif

			if (has_ib2)
			{
				float delta_a = base_a + (float)pass * step_a;
				out_info[count].endpoints[0].a = round_and_clamp_U8(mean_a - delta_a);
				out_info[count].endpoints[1].a = round_and_clamp_U8(mean_a + delta_a);
			}

			if (++count == out_info_size)
				return count;
		}
	}

	if (flags & BC7ENC_PCA_PSEUDOSQUISH)
	{
		U32 dot_and_index[MAXDOTS];
		int npix = in->num_pixels;
		for (int i = 0; i < npix; ++i)
		{
			F32 dot = dots[i];
			// maximum dot absolute value is 255*3
			U32 doti = (U32)(int)((dot + 1024.0f) * 16.0f);
			dot_and_index[i] = (doti << 8) | i;
		}

		// sort dot products
		stdsort(dot_and_index, dot_and_index + npix);

		// try all combinations of first endpoint in first half, second endpoint in second half
		// with the two reasonably far apart
		for (int i = 0; i < npix/2; ++i)
		{
			for (int j = i + npix/2 + 1; j < npix; ++j)
			{
				const int idx1 = dot_and_index[i] & 0xff;
				const int idx2 = dot_and_index[j] & 0xff;

				out_info[count].endpoints[0].dw = RR_GET32_NATIVE_UNALIGNED(in->pixels + idx1*4);
				out_info[count].endpoints[1].dw = RR_GET32_NATIVE_UNALIGNED(in->pixels + idx2*4);

				if (++count == out_info_size)
					return count;
			}
		}
	}

	return count;
}

template<typename Mode>
static void bc7enc_refine_pca(BC7SubsetState *st, const BC7SubsetInput *in, BC7Flags flags, const BC7EncOptions *opt, const BC7Prep *prep)
{
	const int comp = (Mode::ab != 0 && Mode::ib2 == 0 && !(flags & BC7ENC_IGNORE_ALPHA)) ? 4 : 3;
	BC7SubsetEndpoints results[80];

	// determine what to try
	int count = bc7enc_refine_pca_core(results, (int)RR_ARRAY_SIZE(results), st, in, flags, opt, prep, comp, Mode::ns, Mode::ib2 != 0);
	if (!count)
		return;

	/*
	for (int i = 0; i < count; i++)
	{
		const BC7Color &e0 = results[i].endpoints[0];
		const BC7Color &e1 = results[i].endpoints[1];
		rrprintf("  pcac[%d] (%02x,%02x,%02x,%02x) (%02x,%02x,%02x,%02x)\n", i,
			e0.r,e0.g,e0.b,e0.a, e1.r,e1.g,e1.b,e1.a);
	}
	*/

	// for BC7ENC_REFINE_PARTITIONS
	//	flags are always off, count is always 1
	//	eg. we just take the pca_core result and bc7enc_try_and_commit_partial

	// try them
	BC7SubsetState nst = *st;
	if (flags & BC7ENC_PCA_TRY_HARD)
	{
		for (int i = 0; i < count; ++i)
		{
			// Initial fit
			BC7SubsetState temp;
			temp.init_empty(st->isbit, st->rbit);
			temp.endpoints[0] = st->endpoints[0];
			temp.endpoints[1] = st->endpoints[1];
			try_two_color_fit<Mode>(&temp, in, results[i].endpoints[0], results[i].endpoints[1], flags);
			if (temp.err < st->err)
				*st = temp;

			// and then refine to hit those indices
			bc7enc_refine_lsq_endpoints_and_try<Mode>(st, &temp, in, flags);
		}
	}
	else if (count > 1)
	{
		// If doing complicated PCA variants, do the initial fit with two_color_fit,
		// but just do the cheap initial fits on the many other options
		try_two_color_fit<Mode>(st, in, results[0].endpoints[0], results[0].endpoints[1], flags);

		for (int i = 1; i < count; i++)
		{
			memcpy(&nst.endpoints, &results[i].endpoints, sizeof(nst.endpoints));
			bc7enc_try_and_commit_partial<Mode>(st, &nst, in, flags);
		}
	}
	else
	{
		memcpy(&nst.endpoints, &results[0].endpoints, sizeof(nst.endpoints));
		bc7enc_try_and_commit_partial<Mode>(st, &nst, in, flags);
	}
}

// ---- refine driver funcs

/****

"refine" phase takes a given choice for part/rbits/isbit and tried to find better endpoints for that

****/

// bc7enc_refine_subset_partition :
//	try some better endpoints (find indices & error), write to [st] if better
//  used by bc7enc_try_partition, the initial candidate trials
template<typename Mode>
static void bc7enc_refine_subset_partition(BC7SubsetState *st, const BC7SubsetInput *in, const BC7EncOptions *opt, const BC7Prep * prep, int s)
{
	int retain_flags = opt->flags & BC7ENC_RETAIN_MASK;
	
	// should have something on :
	RR_ASSERT( opt->partition_pca || opt->partition_lsq_iters );
	// what's actually used here is partition_pca = true , lsq_iters = 1 or 2
	
	TRACE("  pre-pca: (%02x,%02x,%02x,%02x) (%02x,%02x,%02x,%02x)\n",
		  st->endpoints[0].r, st->endpoints[0].g, st->endpoints[0].b, st->endpoints[0].a,
		  st->endpoints[1].r, st->endpoints[1].g, st->endpoints[1].b, st->endpoints[1].a);
	TRACE("  sub pre-refine err=%d\n", st->err);
	if (opt->partition_pca) bc7enc_refine_pca<Mode>(st, in, BC7ENC_PCA | retain_flags, opt, prep);
	TRACE("  refine pca err=%d\n", st->err);
	TRACE("  pre-lls: (%02x,%02x,%02x,%02x) (%02x,%02x,%02x,%02x)\n",
		  st->endpoints[0].r, st->endpoints[0].g, st->endpoints[0].b, st->endpoints[0].a,
		  st->endpoints[1].r, st->endpoints[1].g, st->endpoints[1].b, st->endpoints[1].a);

	if (opt->partition_lsq_iters) bc7enc_refine_lsq_endpoints_iterated<Mode>(st, in, retain_flags, opt->partition_lsq_iters);
	TRACE("  refine lsq err=%d\n", st->err);
}

// bc7enc_refine is the final heaviest duty refine
//	it's done on the most narrowed candidate set <= [narrow2]
template<typename Mode>
static RADNOINLINE void bc7enc_refine_mode(const U8 * block, BC7BlockState *st, BC7Flags flags, const int optiters[2], const BC7EncOptions *opt, const BC7Prep * prep)
{
	SIMPLEPROFILE_SCOPE_INDEXED(bc7enc_refine_mode,Mode::index,8,1);

	RR_ASSERT(st->mode == Mode::index);

	BC7BlockInput block_in;
	block_in.init(block, radtex_subset_to_inds[Mode::ns-1][st->p], st->subsets[0].rbit);

	BC7Error total_err = 0;
	int total_pixels = 0;
	for (int s = 0; s < Mode::ns; ++s)
	{
		BC7SubsetState *sst = st->subsets + s;
		const int num_pixels = radtex_num_pixels_per_subset[Mode::ns][st->p][s];

		RR_ASSERT( check_endpoints_q<Mode>(sst,flags) );
		
		BC7SubsetInput input;
		input.init(block_in.pixels + total_pixels*4, num_pixels, alpha_chan_index(sst), opt->alpha_weight);

		if (sst->err)
		{
			if (flags & (BC7ENC_ALL_PCA)) bc7enc_refine_pca<Mode>(sst, &input, flags, opt, prep);

			if (flags & BC7ENC_SLIDE_SELECTORS) bc7enc_refine_slide_selectors<Mode>(sst, &input, flags);
			if (flags & BC7ENC_EXTRAPOLATE_SELECTORS) bc7enc_refine_extrapolate_selectors<Mode>(sst, &input, flags);
			if (flags & BC7ENC_SCALE_SELECTORS) bc7enc_refine_scale_selectors<Mode>(sst, &input, flags);

			if (flags & BC7ENC_LSQ_FIT) bc7enc_refine_lsq_endpoints_iterated<Mode>(sst, &input, flags, opt->max_lsq_iter);

			if ( (flags & BC7ENC_JITTER_SELECTORS) && optiters[0] > 0) bc7enc_refine_jitter_selectors<Mode>(sst, &input, flags, optiters[0], prep->rand_seed + s * 0xBF58476D1CE4E5B9ULL);
			if ( (flags & BC7ENC_ANNEAL_ENDPOINTS) && optiters[1] > 0) bc7enc_refine_anneal_endpoints<Mode>(sst, &input, flags, optiters[1], prep->rand_seed + (s+7) * 0xBF58476D1CE4E5B9ULL, false);

			if (flags & BC7ENC_BRUTE_PBITS) bc7enc_refine_brute_pbits<Mode>(sst, &input, flags);
		}

		RR_ASSERT( check_endpoints_q<Mode>(sst,flags) );
		
		total_err += sst->err;
		total_pixels += num_pixels;
	}
	RR_ASSERT( total_pixels == 16 );

	st->err = total_err;
}

static void bc7enc_refine_dispatch(const U8 * block, BC7BlockState *st, BC7Flags flags, const int optiters[2], const BC7EncOptions *opt, const BC7Prep * prep)
{
	switch(st->mode) {
	case 0: bc7enc_refine_mode<BC7Mode<0> >(block, st, flags, optiters, opt, prep); break;
	case 1: bc7enc_refine_mode<BC7Mode<1> >(block, st, flags, optiters, opt, prep); break;
	case 2: bc7enc_refine_mode<BC7Mode<2> >(block, st, flags, optiters, opt, prep); break;
	case 3: bc7enc_refine_mode<BC7Mode<3> >(block, st, flags, optiters, opt, prep); break;
	case 4: bc7enc_refine_mode<BC7Mode<4> >(block, st, flags, optiters, opt, prep); break;
	case 5: bc7enc_refine_mode<BC7Mode<5> >(block, st, flags, optiters, opt, prep); break;
	case 6: bc7enc_refine_mode<BC7Mode<6> >(block, st, flags, optiters, opt, prep); break;
	case 7: bc7enc_refine_mode<BC7Mode<7> >(block, st, flags, optiters, opt, prep); break;
	}
}

// ---- main encode

// bc7enc_try_partition :
//	  initial candidate trail of a given mode/part
//	  tries all rbit/isbit
//   finds first candidate endpoints/indices and measures error
//	adds to [results] for later refinement
template <typename Mode>
static BC7Error bc7enc_try_partition(const U8 *block, const BC7Prep *prep, BC7Results *results, const BC7EncOptions *opt, int p)
{
	BC7Error best_err = BC7_ERROR_MAX;

	// seed_info has the color bbox of this subset
	const BC7PartitionInfo &seed_info = prep->infos[linear_partition_index(Mode::ns, p)];

	BC7BlockState st;
	st.mode = Mode::index;
	st.p = static_cast<U8>(p);

	// Optimal single-color fits for subsets. Only need to bother with this
	// for modes with multiple subsets; if there's a single subset and it's
	// all one color, that means the entire block is solid color, and that
	// is much easier (since we get to not just pick the endpoints but also
	// the mode.)
	BC7OptimalEndpoints opt_ep[3];
	bool have_opt[3] = { false, false, false };
	
	if (Mode::ns >= 2)
	{
		for (int s = 0; s < Mode::ns; ++s)
		{
			if (!seed_info.all_same(s))
				continue;

			const bool enable_alpha = Mode::ab > 0 && !(opt->flags & BC7ENC_IGNORE_ALPHA);
			opt_ep[s] = find_optimal_endpoints_for_constant_color(Mode::index, seed_info.bbox[s][0].v, enable_alpha);
			have_opt[s] = true;
		}
	}

	// Try the color bbox diagonal and all channel rotations of it
	// (in modes that have rotation bits)
	//
	// but in "separate alpha" mode, rotations are forbidden
	int num_rbit = (opt->flags & BC7ENC_SEPARATE_ALPHA) ? 1 : (1 << Mode::rb);

	for (int rbit = 0; rbit < num_rbit; ++rbit)
	{
		// get subsets in linear order :
		BC7BlockInput block_in;
		block_in.init(block, radtex_subset_to_inds[Mode::ns-1][p], static_cast<U8>(rbit));

		// make a BC7SubsetInput on each of the subsets in the BC7BlockInput
		BC7SubsetInput input[Mode::ns];
		int total_pixels = 0;
		for (int s = 0; s < Mode::ns; ++s)
		{
			int num_pixels = radtex_num_pixels_per_subset[Mode::ns][p][s];
			input[s].init(block_in.pixels + total_pixels*4, num_pixels, alpha_chan_index(rbit), opt->alpha_weight);
			total_pixels += num_pixels;
		}

		// isbit == 0 except in mode 4
		for (U8 isbit = 0; isbit < Mode::isb + 1; ++isbit)
		{
			// total_err over subsets
			BC7Error total_err = 0;
			for (int s = 0; s < Mode::ns; ++s)
			{
				const BC7SubsetInput *in = input + s;
				// fill [st] which will be copied to [results]
				BC7SubsetState *sst = st.subsets + s;

				sst->init(seed_info.bbox[s][0], seed_info.bbox[s][1], isbit);
				sst->set_rbit(rbit);

				if (have_opt[s])
				{
					// subset is single color, use optimal endpoints
				
					RR_ASSERT( Mode::ns > 1 ); // should not get here with single subset modes, as that must be a solid color block
					
					RR_ASSERT(rbit == 0 && isbit == 0); // these only exist in mode 4 and 5 which (should) never get here
					sst->endpoints[0] = opt_ep[s].endpoints[0];
					sst->endpoints[1] = opt_ep[s].endpoints[1];

					memset(sst->idxs[0].ind, opt_ep[s].ind, in->num_pixels);
					sst->err = in->num_pixels * opt_ep[s].error;

					sst->scalar_err = 0;
					RR_ASSERT( Mode::ib2 == 0 ); // never have ib2 or scalar_err here cuz that's ns == 1
					
					// Modes without alpha decode the alpha channel to 255; need to price that in, since it's
					// not included in the error above!
					if (Mode::ab == 0 && (opt->flags & BC7ENC_IGNORE_ALPHA) == 0)
					{
						int alpha_err = radtex_sqr(seed_info.bbox[s][0].a - 255);
						sst->err += in->num_pixels * alpha_err;
					}

					if ( (Mode::spb + Mode::epb) == 0 )
					{
						// CB : don't set pbits if we don't have any!
						sst->pbits = 0;
					}
					else
					{
						int pbit_mask = 128 >> Mode::cb;
						sst->pbits = ((opt_ep[s].endpoints[0].r & pbit_mask) ? 1 : 0) | ((opt_ep[s].endpoints[1].r & pbit_mask) ? 2 : 0);
					}
					
					calc_endpoints_and_pbits<Mode>(sst->endpoints_q, sst, opt->flags, sst->pbits);
	
					// yes it's right
					//BC7Error check_err = bc7enc_calc_subset_error<Mode>(sst,in,opt->flags);
					//RR_ASSERT_ALWAYS( check_err == sst->err );
				}
				else
				{
					// do a first trial to seed a guess
					//  sst->endpoints are the bbox min/max
					bc7enc_trial<Mode>(sst, in, opt->flags);
					
					RR_ASSERT( check_endpoints_q<Mode>(sst,opt->flags) );
		
					// do one refine on this subset to make this a better estimate :
					//	(this is only in VerySlow & Reference)
					if (opt->flags & BC7ENC_REFINE_PARTITIONS)
					{
						bc7enc_refine_subset_partition<Mode>(sst, in, opt, prep, s);
					}
					else if (Mode::index == 6 && (opt->flags & BC7ENC_MODE6_EARLY_LSQ_FIT))
					{
						// Run one round of LSQ for Mode 6. This is done to bias towards Mode
						// 6 blocks that already been BC encoded. eg. if BC1 encoded pixels
						// were converted to an image and recompressed with BC7.
						int retain_flags = opt->flags & BC7ENC_RETAIN_MASK;
						bc7enc_refine_lsq_endpoints_iterated<Mode>(sst, in, retain_flags, 1);
					}

					RR_ASSERT( check_endpoints_q<Mode>(sst,opt->flags) );
				}

				total_err += sst->err;
			}

			st.err = total_err;
			results->add_state(&st);
			best_err = RR_MIN(best_err, st.err);
		}
	}

	return best_err;
}

// "Mode" is a BC7Mode<mode_num>
template<typename Mode>
static int bc7enc_try_mode(const U8 *block, const BC7Prep *prep, BC7Results *results, const BC7EncOptions *opt)
{
	SIMPLEPROFILE_SCOPE_INDEXED(bc7enc_try_mode,Mode::index,8,1);

	if ( results->best_err == 0 )
		return 0;	

	const int num_partitions = 1 << Mode::pb;
	int num_part = RR_MIN(num_partitions, opt->max_part);

	// track best_err in this mode
	// [results] also tracks best error including previous modes
	int best_part = 0;
	BC7Error best_err = BC7_ERROR_MAX;

	if ( ! opt->max_results_per_mode )
	{
		for (int pp = 0; pp < num_part && results->best_err > 0; ++pp)
		{
			int p = (Mode::ns >= 2) ? radtex_partition_order[Mode::ns-2][pp] : 0;

			BC7Error err = bc7enc_try_partition<Mode>(block, prep, results, opt, p);
			if (err < best_err)
			{
				best_err = err;
				best_part = p;
			}
		}
	}
	else
	{
		// Output results for this mode to a temp BC7Results, then propagate
		// only the best few outwards
		BC7Results mode_results(results->mpeb);
		mode_results.heap_size = opt->max_results_per_mode;

		for (int pp = 0; pp < num_part && mode_results.best_err > 0; ++pp)
		{
			int p = (Mode::ns >= 2) ? radtex_partition_order[Mode::ns-2][pp] : 0;

			BC7Error err = bc7enc_try_partition<Mode>(block, prep, &mode_results, opt, p);
			if (err < best_err)
			{
				best_err = err;
				best_part = p;
			}
		}

		// submit the survivors into the main results
		for LOOP(i,mode_results.used)
			results->add_state(mode_results.heap[i].slot);
	}

	return best_part;
}

static inline U8 swap_pbits(U8 x)
{
	return ((x & 2) >> 1) | ((x & 1) << 1);
}

static void xor_indices(U8 * inds, U8 toggle)
{
#ifdef __RADX86__
	Vec128_U8 vinds = Vec128_U8::loadu(inds);
	vinds ^= Vec128_U8(toggle);
	vinds.storeu(inds);
#else
	for (int i = 0; i < 16; i++)
		inds[i] ^= toggle;
#endif
}

static void bc7enc_canonicalize_subset(BC7SubsetState *st, int anchor_pos, U8 ib1_msb, U8 ib2_msb)
{
	BC7Color ep[2];
	ep[0] = st->endpoints[0];
	ep[1] = st->endpoints[1];
	BC7Color epq[2];
	epq[0] = st->endpoints_q[0];
	epq[1] = st->endpoints_q[1];

	// idxs[] are dense linear order for each subset
	//	"msb" is the top bit of an index value

	U8 * vector_inds = st->idxs[st->isbit].ind;
	U8 * scalar_inds = st->idxs[st->isbit ^ 1].ind;

	U8 vector_msb = st->isbit ? ib2_msb : ib1_msb;
	U8 scalar_msb = st->isbit ? ib1_msb : ib2_msb;

	if (vector_inds[anchor_pos] & vector_msb)
	{
		// Vector channels need a swap
		st->endpoints[0] = ep[1];
		st->endpoints[1] = ep[0];
		st->endpoints_q[0] = epq[1];
		st->endpoints_q[1] = epq[0];
		st->pbits = swap_pbits(st->pbits);

		xor_indices(vector_inds, vector_msb * 2 - 1);
	}

	if (ib2_msb)
	{
		// restore to unswapped order in case vec swap above was done
		st->endpoints[0].a = ep[0].a;
		st->endpoints[1].a = ep[1].a;
		st->endpoints_q[0].a = epq[0].a;
		st->endpoints_q[1].a = epq[1].a;

		if (scalar_inds[anchor_pos] & scalar_msb)
		{
			// Scalar channels get a swap
			st->endpoints[0].a = ep[1].a;
			st->endpoints[1].a = ep[0].a;
			st->endpoints_q[0].a = epq[1].a;
			st->endpoints_q[1].a = epq[0].a;
			// pbits stay as they are

			xor_indices(scalar_inds, scalar_msb * 2 - 1);
		}
	}
}

// bc7enc_canonicalize :
//	swap endpoints of each subset to turn off anchor bits
void bc7enc_canonicalize(BC7BlockState *st)
{
	const BC7ModeDesc &mode = bc7_modes[st->mode];

	// These expressions do the right thing when ib==0 (msb will be = 0)
	U8 ib1_msb = static_cast<U8>((1 << mode.ib) >> 1);
	U8 ib2_msb = static_cast<U8>((1 << mode.ib2) >> 1);

	// radtex_subset_anchors[] contains two anchor positions
	//	<< 4 gives implicit third anchor = 0
	int anchors = radtex_subset_anchors[mode.ns-1][st->p] << 4;

	for (int s = 0; s < mode.ns; ++s)
	{
		// extract 4 bit anchor_pos (first is always 0)
		//	anchor_pos is in dense linear subset position
		int anchor_pos = (anchors >> (s*4)) & 0xf;
		bc7enc_canonicalize_subset(st->subsets + s, anchor_pos, ib1_msb, ib2_msb);
	}
}

static U64 remove_anchor_bit(U64 idxs, U64 anchor_bit)
{
	// anchor bit must have been forced to zero on entry
	RR_ASSERT((idxs & anchor_bit) == 0);
	U64 lo = anchor_bit - 1ull; // bits below anchor_bit
	return (idxs & lo) | ((idxs >> 1) & ~lo);
}

static RADFORCEINLINE int set_bits(U64 *base, int bit_idx, int num_bits, U64 in_val)
{
	RR_ASSERT(0 <= num_bits && num_bits <= 63);
	RR_ASSERT(bit_idx + num_bits <= 128);
	if(num_bits == 0)
		return bit_idx;

	int word = bit_idx / 64;
	int res = bit_idx & 63;
	U64 val = in_val & ((1ull << num_bits) - 1);

	base[word] |= val << res;
	if(num_bits > 64 - res)
	{
		RR_ASSERT( word == 0 );
		base[1] |= val >> (64 - res); // top part spans into next word
	}

	return bit_idx + num_bits;
}

template<typename Mode>
static void bc7enc_emit_bits(U8 * output_bc7, const BC7BlockState * in_state, BC7Flags flags)
{
	const int ns = Mode::ns;
	const int ib = Mode::ib;
	const int ib2 = Mode::ib2;

	// Grab data from subsets
	RR_ASSERT(in_state->mode == Mode::index);
	int p = in_state->p; // partition
	// rbit and isbit is actually per-block, not per subset, make sure they match :
	U8 isbit = in_state->subsets[0].isbit;
	U8 rbit = in_state->subsets[0].rbit;
	
	U64 idxs = 0;
	U64 idxs2 = 0;

	U64 inds = radtex_subset_to_inds[Mode::ns-1][p];
	for (int s = 0; s < Mode::ns; ++s)
	{
		const BC7SubsetState *sst = in_state->subsets + s;

		RR_ASSERT( sst->pbits == 0 || (Mode::spb + Mode::epb) > 0 );	
		RR_ASSERT( check_endpoints_q<Mode>(sst,flags) );
		
		// rbit and isbit is actually per-block, not per subset, make sure they match :
		RR_ASSERT(sst->isbit == isbit);
		RR_ASSERT(sst->rbit == rbit);

		// sst->idxs[] are in dense subset linear order
		//	scatter to pixel block order:
		int num_pixels = radtex_num_pixels_per_subset[Mode::ns][p][s];
		for (int j = 0; j < num_pixels; ++j)
		{
			int i = static_cast<int>(inds & 0xf);
			inds >>= 4;
			idxs  |= U64(sst->idxs[0].ind[j]) << (i * ib);
			idxs2 |= U64(sst->idxs[1].ind[j]) << (i * ib2);
		}
	}

	// find the location of the subset anchor bits
	// note the endpoint swap should have already been done to make these bits zero
	//	 (that's bc7enc_canonicalize)
	const U8 *anchor1_tbl = ns == 2 ? radtex_anchor_2_2 : radtex_anchor_3_2;
	U64 anchor1_bit = (ns >= 2) ? 1ull << (ib * (anchor1_tbl[p] + 1) - 1) : 0;
	U64 anchor2_bit = (ns >= 3) ? 1ull << (ib * (radtex_anchor_3_3[p] + 1) - 1) : 0;

	// remove the anchor bits, top to bottom
	// NOTE: remove_anchor_bit does nothing when anchor_bit==0, so this Just Works
	idxs = remove_anchor_bit(idxs, RR_MAX(anchor1_bit, anchor2_bit));
	idxs = remove_anchor_bit(idxs, RR_MIN(anchor1_bit, anchor2_bit));
	idxs = remove_anchor_bit(idxs, (1ull << ib) >> 1);
	idxs2 = remove_anchor_bit(idxs2, (1ull << ib2) >> 1);

	// Actually emit the bits.
	//
	// In my tests, bc7data and all of the manipulations of pos
	// completely disappear, we just get a bunch of straight-line
	// shifting and masking to registers.
	U64 bc7data[2] = { 0, 0 };
	int pos = 0;

	pos = set_bits(bc7data, pos, Mode::index+1, 1<<Mode::index);
	pos = set_bits(bc7data, pos, Mode::pb, p); // partition number
	pos = set_bits(bc7data, pos, Mode::rb, rbit); // rotation bits
	pos = set_bits(bc7data, pos, Mode::isb, isbit); // index selection bit

	// iterate over all subsets and then two endpoints within each subset,
	// running x with set=subset index and end=endpoint index within subset
#define PER_ENDPOINT(x) \
	do { \
		int set = 0, end = 0; \
		x; \
		end = 1; \
		x; \
		if (ns >= 2) { \
			set = 1; end = 0; \
			x; \
			end = 1; \
			x; \
		} \
		if (ns >= 3) { \
			set = 2; end = 0; \
			x; \
			end = 1; \
			x; \
		} \
	} while(0)

	// red/green/blue endpoints
	PER_ENDPOINT(pos = set_bits(bc7data, pos, Mode::cb, in_state->subsets[set].endpoints_q[end].r >> (8 - Mode::cb)));
	PER_ENDPOINT(pos = set_bits(bc7data, pos, Mode::cb, in_state->subsets[set].endpoints_q[end].g >> (8 - Mode::cb)));
	PER_ENDPOINT(pos = set_bits(bc7data, pos, Mode::cb, in_state->subsets[set].endpoints_q[end].b >> (8 - Mode::cb)));

	// alpha endpoints
	PER_ENDPOINT(pos = set_bits(bc7data, pos, Mode::ab, (int)in_state->subsets[set].endpoints_q[end].a >> (8 - Mode::ab)));

	// epbits/spbits
	const int num_pbits = Mode::epb*2 + Mode::spb;
	pos = set_bits(bc7data, pos, num_pbits, in_state->subsets[0].pbits);
	if (ns >= 2)
		pos = set_bits(bc7data, pos, num_pbits, in_state->subsets[1].pbits);
	if (ns >= 3)
		pos = set_bits(bc7data, pos, num_pbits, in_state->subsets[2].pbits);

	// indices
	pos = set_bits(bc7data, pos, ib*16-ns, idxs);
	if (ib2)
		pos = set_bits(bc7data, pos, ib2*16-ns, idxs2);

#undef PER_ENDPOINT

	RR_ASSERT(pos == 128);

	// Write the words out in target byte order
	RR_PUT64_LE(output_bc7 + 0, bc7data[0]);
	RR_PUT64_LE(output_bc7 + 8, bc7data[1]);
}

void bc7enc_emit(U8 * output_bc7, const BC7BlockState * state, BC7Flags flags)
{
	switch (state->mode)
	{
	case 0: bc7enc_emit_bits<BC7Mode<0> >(output_bc7, state, flags); break;
	case 1: bc7enc_emit_bits<BC7Mode<1> >(output_bc7, state, flags); break;
	case 2: bc7enc_emit_bits<BC7Mode<2> >(output_bc7, state, flags); break;
	case 3: bc7enc_emit_bits<BC7Mode<3> >(output_bc7, state, flags); break;
	case 4: bc7enc_emit_bits<BC7Mode<4> >(output_bc7, state, flags); break;
	case 5: bc7enc_emit_bits<BC7Mode<5> >(output_bc7, state, flags); break;
	case 6: bc7enc_emit_bits<BC7Mode<6> >(output_bc7, state, flags); break;
	case 7: bc7enc_emit_bits<BC7Mode<7> >(output_bc7, state, flags); break;
	}
}

void bc7_encode_single_color_block(U8 * output_bc7, const U8 rgba[4])
{
	U64 r = rgba[0];
	U64 g = rgba[1];
	U64 b = rgba[2];
	U64 a = rgba[3];

	const U64 bit6_mask = (0x40 << 8) | (0x40 << 22) | (0x40ull << 36);
	const U64 lo7_mask  = (0x7f << 8) | (0x7f << 22) | (0x7full << 36);
	U64 color_bits;

	// Color channels (see bc7solid/main.cpp)
	// this is the bit-hack version written to process 3 color chans at once
	color_bits = (r << 8) | (g << 22) | (b << 36);

	// The basic idea here is this: for every color channel, we get the right
	// results with
	//   e0 = val >> 1
	//   e1 = (val + (val < 128 ? 1 : -1)) >> 1
	// the bias in the e1 computation
	//   (val < 128 ? 1 : -1)
	// can be written as
	//   1 - ((val >> 6) & 2)
	// and the rest is just bit fiddling and accounting for the layout of mode 5.

	U64 t = color_bits << 6;
	color_bits = ((color_bits >> 1) & lo7_mask) - (color_bits & ~lo7_mask);
	color_bits += t + (t & bit6_mask);

	// alpha: just set endpoint 0 to target value then use all-0 inds
	color_bits |= a << 50;

	// insert mode bits
	color_bits |= 0x20; // mode 5, rotation=0

	RR_PUT64_LE_UNALIGNED(output_bc7 + 0, color_bits); // this covers 64 out of the 66 mode/color bits
	RR_PUT64_LE_UNALIGNED(output_bc7 + 8, 0xaaaaaaac); // last 2 color bits, color index bits, alpha index bits
}

bool bc7_is_encoded_single_color_block(const U8 * bc7_block)
{
	// Check for the types of blocks we emit above:
	if ( bc7_block[0] != 0x20 ) // not mode 5, or rotation!=0? not one of ours!
		return false;

	// Check whether the indices are all the same, and matching with the pattern we emit above
	// (color inds all 1, alpha inds all 0).
	// Any block with this index pattern is _definitely_ solid-color because all indices are the
	// same. It doesn't catch _all_ solid-color blocks but it doesn't have to.
	U64 all_inds = RR_GET64_LE_UNALIGNED(bc7_block + 8);
	if ( ( all_inds & ~3 ) == 0xaaaaaaac )
		return true;

	return false;
}

static BC7BlockState *bc7_enc_try_refine(const U8 * block, BC7Results &enc_results, const BC7EncOptions &opt, const BC7Prep * prep)
{
	SIMPLEPROFILE_SCOPE(bc7_enc_try_refine);

	// Exact index finding is expensive and very much wasted on the early attempts.
	BC7Flags full_flags = opt.flags;
	BC7Flags flags = (full_flags & ~BC7ENC_IDXS_EXACT);

	// enc_results has <= [narrow0] initial candidates

	RR_ASSERT( enc_results.used > 0 );
	RR_ASSERT( enc_results.best_err != BC7_ERROR_MAX );

	if(enc_results.best_err > 0)
	{
		// reduce active candidate count in each step
		// [narrow0] -> [narrow1]
		if(opt.narrow1)
		{
			// Do PCA on the best results
			//	(don't bother if bc7enc_refine_subset_partition already did it)
			for (int i = 0; i < enc_results.used; ++i)
			{
				BC7BlockState *slot = enc_results.heap[i].slot;
				bc7enc_refine_dispatch(block, slot, BC7ENC_PCA | (flags & BC7ENC_RETAIN_MASK), NULL, &opt, prep);
				if (slot->err == 0)
					return slot;
				
				// note heap[i].err not updated, but shrink() handles that
			}

			// Narrow it down
			enc_results.shrink(opt.narrow1);
		}

		if(opt.narrow2)
		{
			// LSQ fit
			for (int i = 0; i < enc_results.used; ++i)
			{
				BC7BlockState *slot = enc_results.heap[i].slot;
				bc7enc_refine_lsq_endpoints_pass(block, slot, flags, opt.alpha_weight, 3);
				if (slot->err == 0)
					return slot;
			}

			// Narrow it down
			enc_results.shrink(opt.narrow2);
		}

		// Refine the rest ( <= narrow2)
		//	final heavy duty refinement on the smallest set :
		for (int i = 0; i < enc_results.used; ++i)
		{
			BC7BlockState *slot = enc_results.heap[i].slot;
			bc7enc_refine_dispatch(block, slot, flags, opt.optiters_early, &opt, prep);
			if (slot->err == 0)
				return slot;
		}
	}

	// Which one was the best?
	// use shrink(1) so we get proper heap_err rescore :
	enc_results.shrink(1);
	BC7BlockState *best = enc_results.heap[0].slot;	
	RR_ASSERT( best->err != BC7_ERROR_MAX ); 

	// Further refine the best block (can do this any number of times and will likely see small improvements)
	//	this is the same ops done in the [narrow2] set but with IDXS_EXACT and BRUTE_PBITS
	BC7Flags filtered_flags = full_flags; // restore IDXS_EXACT
	filtered_flags &= ~BC7ENC_ALL_PCA; // remove PCA, no benefit to doing this again.
	filtered_flags |= BC7ENC_BRUTE_PBITS; // brute pbits forced on for all levels
	bc7enc_refine_dispatch(block, best, filtered_flags, opt.optiters_late, &opt, prep);

	return best;
}

// ---- driver func


void BC7Input_Get(BC7Input * input,const U8 * block, int ns, int p, int rbit, int alpha_weight)
{
	// get subsets in linear order :
	input->block.init(block, radtex_subset_to_inds[ns-1][p], static_cast<U8>(rbit));

	// make a BC7SubsetInput on each of the subsets in the BC7BlockInput
	int total_pixels = 0;
	for (int s = 0; s < ns; ++s)
	{
		int num_pixels = radtex_num_pixels_per_subset[ns][p][s];
		input->subsets[s].init(input->block.pixels + total_pixels*4, num_pixels, alpha_chan_index(rbit), alpha_weight);
		total_pixels += num_pixels;
	}
}

void BC7Input_Get(BC7Input * input,const U8 * block, const BC7BlockState * st, int alpha_weight)
{
	int ns = bc7_modes[st->mode].ns;
	int rbit = st->subsets[0].rbit;
	int p = st->p;
	
	BC7Input_Get(input,block,ns,p,rbit,alpha_weight);
}		

void bc7_enc_options_set(BC7EncOptions * popt,rrDXTCLevel level, rrDXTCOptions options)
{
	// !! assumes BC7EncOptions constructor was just run !!
	BC7EncOptions & opt = *popt;

	switch (level)
	{
	case rrDXTCLevel_VeryFast:
		// this corresponds to "fastest3" in rad_tex_tools
		opt.flags = BC7ENC_PCA;
		opt.max_part = 2;
		opt.narrow0 = 4;
		opt.narrow1 = 2;
		opt.narrow2 = 1;
		opt.share_partitions_mode = true;
		break;

	case rrDXTCLevel_Fast:
		// this corresponds to "fastest" in rad_tex_tools
		opt.flags = BC7ENC_PCA_STRETCH | BC7ENC_MODE6_EARLY_LSQ_FIT;
		opt.max_part = 9;
		opt.narrow0 = 12;
		opt.narrow1 = 6;
		opt.narrow2 = 1;
		break;

	default:
	case rrDXTCLevel_Slow:
		// this corresponds to "fast" in rad_tex_tools
		opt.flags = BC7ENC_EXTRAPOLATE_SELECTORS | BC7ENC_SLIDE_SELECTORS | BC7ENC_PCA_STRETCH | BC7ENC_JITTER_SELECTORS |
			BC7ENC_MODE6_EARLY_LSQ_FIT;
		opt.max_part = 28;
		opt.narrow0 = 36;
		opt.narrow1 = 12;
		opt.narrow2 = 3;
		opt.optiters_early[0] = 3;
		opt.optiters_late[0] = 9;
		break;

	case rrDXTCLevel_VerySlow:
		// this corresponds to "slow" in rad_tex_tools
		opt.flags = BC7ENC_PCA_STRETCH | BC7ENC_SLIDE_SELECTORS | BC7ENC_EXTRAPOLATE_SELECTORS | BC7ENC_REFINE_PARTITIONS |
			BC7ENC_SCALE_SELECTORS | BC7ENC_JITTER_SELECTORS | BC7ENC_BRUTE_PBITS_IDXS_EXACT;
		opt.max_part = 39;
		opt.partition_pca = true;
		opt.partition_lsq_iters = 1;
		//opt.max_results_per_mode = 3; // FG TEST - doesn't seem to have a big effect in either direction
		opt.narrow0 = 6; // our pre-selection is much better with REFINE_PARTITIONS, no need to have many options
		opt.narrow1 = 0; // don't bother with initial PCA refine (we did that for everything in refine_partitions)
		opt.narrow2 = 5;
		opt.optiters_early[0] = 6;
		opt.optiters_late[0] = 60;
		break;

	case rrDXTCLevel_Reference:
		// this corresponds to "super" in rad_tex_tools; not even bothering with the slower levels...
		// NOTE(fg): IDXS_EXACT is not in Jon's "super" preset but it definitely belongs here for the
		// "Reference" version, I don't really care whether it's pareto or not at that point
		opt.flags = BC7ENC_SLIDE_SELECTORS | BC7ENC_PCA_STRETCH | BC7ENC_EXTRAPOLATE_SELECTORS | BC7ENC_SCALE_SELECTORS |
			BC7ENC_REFINE_PARTITIONS | BC7ENC_JITTER_SELECTORS | BC7ENC_ANNEAL_ENDPOINTS | BC7ENC_BRUTE_PBITS | BC7ENC_BRUTE_PBITS_IDXS_EXACT;
			// BC7ENC_IDXS_EXACT
		// [0] = jitter , [1] = anneal
		opt.optiters_early[0] = 16;
		opt.optiters_early[1] = 40;
		opt.optiters_late[0] = 64;
		opt.optiters_late[1] = 128; // over 100 turns on greedy optimize
		RR_ASSERT( opt.partition_pca == true );
		RR_ASSERT( opt.partition_lsq_iters == 2 );
		RR_ASSERT( opt.max_part == 64 );
		RR_ASSERT( opt.narrow0 == 32 );
		opt.narrow1 = 0; // was 16, doesn't make much difference because REFINE_PARTITIONS is on
		opt.narrow2 = 12;
		opt.max_lsq_iter = 100;
		opt.max_pca_stretch_passes = 32;
		break;
	}
	
	if (options & rrDXTCOptions_BC7_IgnoreAlpha)
	{
		opt.flags |= BC7ENC_IGNORE_ALPHA;
	}

	//opt.alpha_weight = 128; // FG TEST
	//opt.flags |= BC7ENC_SEPARATE_ALPHA; // FG TEST
	//opt.alpha_weight = 64;
}

static int linear_partition_index(int ns, int p)
{
	static const int base[3] = { 0, 1, 1+64 }; // [ns-1]

	RR_ASSERT(ns >= 1 && ns <= 3);
	RR_ASSERT(p >= 0 && p < 64);
	return base[ns-1] + p;
}

static void BC7PartitionInfo_init(BC7PartitionInfo infos[BC7Prep::NINFO], const U8 in_block[64], BC7Flags flags, int max_ns, int max_part)
{
	// 1-subset partition
	infos[0].calc(in_block, 0, 1, flags);

	// 2- and 3-subset partitions
	for(int ns = 2; ns <= max_ns; ++ns)
	{
		for (int pp = 0; pp < max_part; ++pp)
		{
			const int p = radtex_partition_order[ns-2][pp];
			const int idx = linear_partition_index(ns, p);
			infos[idx].calc(in_block, p, ns, flags);
		}
	}
}

const U8 * BC7Prep_init(BC7Prep * prep, const U8 in_block[64], const BC7EncOptions & opt, bool force_consider_all_modes)
{
	// just always copy to local block
	memcpy(prep->local_block, in_block, 64);
	U8 * block = prep->local_block;

	prep->has_alpha = false;
	if (opt.flags & BC7ENC_IGNORE_ALPHA)
	{
		for (int i = 3; i < 64; i += 4)
			block[i] = 255;
	}
	else
	{
		// Check whether we have a non-255 alpha on any of the pixels
		for (int i = 3; i < 64; i += 4)
		{
			if (block[i] != 255)
			{
				prep->has_alpha = true;
				break;
			}
		}
	}

	prep->single_color = false;
	// Check whether the block is all one solid color, which means we can instantly
	// emit an optimal encoding
	{
		U32 pixel = RR_GET32_NATIVE_UNALIGNED(block);
		int i = 1;
		while (i < 16 && RR_GET32_NATIVE_UNALIGNED(block + i*4) == pixel)
			++i;

		// all the same, can emit a single-color block
		if (i == 16)
		{
			prep->single_color = true;
			// early return, doesn't fill out all of prep
			//return block;
			
			// meh, don't bother trying to find small speedups for this case
			// just initialize all the variables
		}
	}

#ifdef PCA_REFACTOR
	// Prepare 4 channel covariance matrix for use in later mode refine
	{
		BC7SubsetInput input;
		// NOTE(djg): values for alpha_chan and alpha_weight are unused by
		// calc_subset_mean_covar4, so pick some reasonable, but unused values.
		input.init(block, 16, 3, 1);
		calc_subset_mean_covar4(prep->mean, prep->covar, &input);
	}
#endif

	// Seed RNG if we need it
	if (opt.flags & BC7ENC_RANDOMIZED_MASK)
		prep->rand_seed = rrRand64Simple_SeedFromU64Array( (const U64 *)block, 8 );
	
	// NOTE(fg): not sold on completely ruling out 3-subset modes when non-255 alpha present.
	// A single pixel in the block with a=254 should not make us reject modes 0/2 outright.
	// This should probably at least be a threshold on the SSD between the actual alphas and
	// all-255 or something along those lines.

	prep->max_ns = 1;
	if((!prep->has_alpha || force_consider_all_modes) && (opt.enable_mode[0] || opt.enable_mode[2]))
		prep->max_ns = 3;
	else if(opt.enable_mode[1] || opt.enable_mode[3] || opt.enable_mode[7])
		prep->max_ns = 2;
		
	// init sets up color bboxes in "infos"
	BC7PartitionInfo_init(prep->infos, block, opt.flags, prep->max_ns, opt.max_part);

	return block;
}

//int g_bc7_force_mode = -1;  // CB to generate bc7_average_block_size_bits ; use bc7rd -d
	
//bool force_consider_all_modes = (options & 0x100) != 0; // FG DEBUG
static bool force_consider_all_modes = false;
	
void BC7_CompressBlock(U8 * output_bc7, const U8 * in_block, rrDXTCLevel level, rrDXTCOptions options, const BC7_Mode_ErrBias * mpeb)
{
	SIMPLEPROFILE_SCOPE(BC7_CompressBlock);

	BC7EncOptions opt;
	bc7_enc_options_set(&opt,level,options);

	BC7Prep prep;
	const U8 * block = BC7Prep_init(&prep,in_block,opt,force_consider_all_modes);

	bool ok = BC7_CompressBlock_Sub(output_bc7,block,&opt,&prep,mpeb);
	RR_ASSERT( ok );
	RR_UNUSED_VARIABLE( ok );
}	
	
bool BC7_CompressBlock_Sub(U8 * output_bc7, const U8 * block, const BC7EncOptions * in_opt, const BC7Prep * in_prep, const BC7_Mode_ErrBias * mpeb)
{
	const BC7Prep & prep = *in_prep;
	
	if ( prep.single_color )
	{
		bc7_encode_single_color_block(output_bc7, block);
		return true;
	}
	
	//----------------------------------------------------------------------------------
	
	BC7EncOptions opt = *in_opt;

	// FG TEMP: in "separate alpha" mode, turn off modes 6 and 7 which munge
	// alpha and color together unless alpha is constant.
	//
	// Modes 0-3 are already out of consideration unless alpha is all-255
	if ( opt.flags & BC7ENC_SEPARATE_ALPHA )
	{
		// Mode 6 has 7 bits + pbit for alpha; that means that as long as alpha is (nearly)
		// constant, we can hit it very close, which is good enough for us.
		const BC7PartitionInfo& nfo = in_prep->infos[0]; // 1-subset modes
		int alpha_diff = nfo.bbox[0][1].a - nfo.bbox[0][0].a;

		if ( alpha_diff > 1 ) // we allow a diff of 1 (so almost constant), but no larger
			opt.enable_mode[6] = false;

		// Mode 7 has 5-bit alpha endpoints + pbit which means we automatically have large
		// error in alpha unless the target alpha happens to be right near a quantization
		// boundary, which us further complicated by it depending on the choice of pbit
		// which affects other channels.
		//
		// The main problem with this is that with alpha != 255, we can't use modes 0-3,
		// meaning the only remaining mode with >1 subset is mode 7, which we disable
		// here. So this forces us entirely into 1-subset modes for non-opaque portions,
		// which is giving up a lot.
		//
		// I tried a more fine-grained thing which allows mode 7 if the alpha values
		// are close to ones we can hit exactly with some pbit value, but that gives
		// nasty "dither pattern" style artifacts in the alpha channel between blocks,
		// which is the kind of thing separate alpha mode tries to avoid.
		opt.enable_mode[7] = false;

		// FG TEST
		//if ( alpha_diff <= 1 )
		//	opt.enable_mode[7] = true;
		//opt.alpha_weight = 64;
	}

	int force_only_mode = -1; // off
	//int force_only_mode = g_bc7_force_mode; // CB to generate bc7_average_block_size_bits ; use bc7rd -d
	bool force_only_mode_on = force_only_mode >= 0;
	
	if ( force_only_mode_on )
	{	
		for LOOPARRAY(i,opt.enable_mode) opt.enable_mode[i] = false;
		opt.enable_mode[force_only_mode] = true;
	}
	
	if ( mpeb )
	{
		// RR_F32_MAX means disable mode
		for LOOP(m,BC7_NUM_MODES)
		{
			if ( mpeb->ssd_err_bias[m] == RR_U32_MAX )
				opt.enable_mode[m] = false;
		}
	}	
		
	//----------------------------------------------------------------------------------
	//opt.prep = &prep;

	// initial trial phase
	//	get [narrow0] candidates in enc_results from bc7enc_try_mode
	BC7Results enc_results(mpeb);
	enc_results.heap_size = opt.narrow0;

	// Keep the full flags for refinement, but score the initial
	// attempts with the linear approximation. Everything else is
	// just a waste of time.
	BC7Flags full_flags = opt.flags;
	opt.flags = opt.flags & ~BC7ENC_IDXS_EXACT;

	// First evaluate possible modes to get viable candidates
	{
		SIMPLEPROFILE_SCOPE(bc7enc_try_modes);

		// 4-7 are the alpha modes
		// 1 subset :
		// try mode 6 first; on simple blocks we can hit zero error with mode 6 and then do no more work
		if (opt.enable_mode[6]) bc7enc_try_mode<BC7Mode<6> >(block, &prep, &enc_results, &opt);
		if (opt.enable_mode[5]) bc7enc_try_mode<BC7Mode<5> >(block, &prep, &enc_results, &opt);
		// heuristic : only try mode 4 if has_mode(5) :
		if (opt.enable_mode[4] && (prep.has_alpha || enc_results.has_mode(5) || !opt.enable_mode[5] || force_only_mode_on)) bc7enc_try_mode<BC7Mode<4> >(block, &prep, &enc_results, &opt);
		// 2 subset:
		if (opt.enable_mode[7] && (prep.has_alpha || force_only_mode_on)) bc7enc_try_mode<BC7Mode<7> >(block, &prep, &enc_results, &opt);

		// 0-3 have no alpha + 2-3 subsets :
		if(!prep.has_alpha || force_consider_all_modes || force_only_mode_on)
		{
			int part_ns2 = -1; // partition chosen for 2-subset modes
			int part_ns3 = -1; // partition chosen for 3-subset modes
			if (opt.enable_mode[1]) part_ns2 = bc7enc_try_mode<BC7Mode<1> >(block, &prep, &enc_results, &opt);
			if (opt.share_partitions_mode && part_ns2 >= 0) {
				if (opt.enable_mode[3] && enc_results.has_mode(1)) bc7enc_try_partition<BC7Mode<3> >(block, &prep, &enc_results, &opt, part_ns2);
			} else {
				if (opt.enable_mode[3] && (enc_results.has_mode(1) || !opt.enable_mode[1])) bc7enc_try_mode<BC7Mode<3> >(block, &prep, &enc_results, &opt);
			}
			if (opt.enable_mode[0]) part_ns3 = bc7enc_try_mode<BC7Mode<0> >(block, &prep, &enc_results, &opt);
			if (opt.share_partitions_mode && part_ns3 >= 0) {
				if (opt.enable_mode[2] && enc_results.has_mode(0)) bc7enc_try_partition<BC7Mode<2> >(block, &prep, &enc_results, &opt, part_ns3);
			} else {
				if (opt.enable_mode[2] && (enc_results.has_mode(0) || !opt.enable_mode[0])) bc7enc_try_mode<BC7Mode<2> >(block, &prep, &enc_results, &opt);
			}
		}
	}

	if ( enc_results.used == 0 )
		return false;

	opt.flags = full_flags;
	BC7BlockState *best = bc7_enc_try_refine(block, enc_results, opt, &prep);
	
	// Actually encode the BC7 block into the output data.
	
	// canonicalize swaps endpoints to turn off output bits
	bc7enc_canonicalize(best);
	
	// emit just does bit packing, no changes
	bc7enc_emit(output_bc7, best, opt.flags);	
	
	return true;
}

// ---- decode blocks into block state

void bc7enc_state_from_bits(BC7BlockState * st, const U8 * input_bc7, BC7Flags flags)
{
	// NOTE this can be optimized if we need something faster, but
	// I wanted to get something simple and reliable first so I'm
	// just leaning on the known-good BC7 decoder as much as possible.
	BC7BlockRawDesc desc;

	bool valid = bc7_analyze_block_raw(&desc, input_bc7);
	RR_ASSERT(valid);

	if (!valid)
	{
		memset(st, 0, sizeof(*st));
		return;
	}

	const BC7ModeDesc * m = desc.mode;
	st->err = 0;
	st->mode = m->mode;
	st->p = desc.partition_id;

	// mask for where the pbit is
	int pbit_mask = (m->epb + m->spb) ? (128 >> m->cb) : 0;

	U64 inds = radtex_subset_to_inds[m->ns-1][st->p];
	for (int s = 0; s < m->ns; ++s)
	{
		BC7SubsetState * sst = st->subsets + s;
		sst->err = 0; // not actually known, just initialize it
		sst->scalar_err = 0;
		sst->isbit = desc.ind_sel;
		sst->rbit = desc.rot;

		const int num_pixels = radtex_num_pixels_per_subset[m->ns][st->p][s];
		for (int c = 0; c < 4; ++c)
		{
			sst->endpoints[0].v[c] = desc.endpoints_raw[c][s*2 + 0];
			sst->endpoints[1].v[c] = desc.endpoints_raw[c][s*2 + 1];
		}

		// recover pbits from endpoints (could easily do this more directly)
		sst->pbits  = (sst->endpoints[0].r & pbit_mask) ? 1 : 0;
		sst->pbits |= (sst->endpoints[1].r & pbit_mask) ? 2 : 0;

		for (int j = 0; j < num_pixels; ++j)
		{
			int i = static_cast<int>(inds & 15);
			inds >>= 4;

			sst->idxs[0].ind[j] = desc.inds[0][i];
			if (m->ib2)
				sst->idxs[1].ind[j] = desc.inds[1][i];
		}
	}
	
	// CB NOTE : modes without alpha get endpoints.a = 255
	//	encoder does not necessarily do the same?
	// FG NOTE: it does in calc_endpoints_and_pbits which is place that matters.
	// (that's what actually determines which values we use as input for the lerp)
	
	// update cached endpoints_q
	bc7enc_update_endpoints_q(st,flags);
	
	//RR_ASSERT( bc7enc_check_endpoints_q(st,flags) );
}

//======================================================

template<typename Mode>
static void bc7enc_accumulate_vector_errs(BC7IndexErrors * errs, const BC7BlockState * st, const BC7Input * in, BC7Flags flags)
{
	const int p = st->p;
	U64 inds = radtex_subset_to_inds[Mode::ns-1][p];

	const int ib = (Mode::isb != 0 && st->subsets[0].isbit) ? Mode::ib2 : Mode::ib;
	const int num_lerp = 1 << ib;

	for (int s = 0; s < Mode::ns; ++s)
	{
		const U8 * rgba_base = in->subsets[s].pixels;
		const int num_pixels = in->subsets[s].num_pixels;
		const BC7SubsetState *sst = st->subsets + s;

		int colors[16][4];
		calc_lerp_bc7<Mode>(sst->isbit, colors, sst->endpoints);

		// outer loop over pixels
		for (int i = 0; i < num_pixels; ++i)
		{
			const U8 * rgba = rgba_base + i*4;

			// accumulate error totals for pixels in original numbering
			BC7Error * cur_err = errs->err[inds & 0xf];
			inds >>= 4;

			// @@ easy to SIMD by adapting the kernels we already have

			// inner loop over index options
			for (int j = 0; j < num_lerp; ++j)
			{
				BC7Error e = cur_err[j];
				e += radtex_sqr(rgba[0] - colors[j][0]);
				e += radtex_sqr(rgba[1] - colors[j][1]);
				e += radtex_sqr(rgba[2] - colors[j][2]);
				if (Mode::ib2 == 0) // if no separate vector inds
					e += radtex_sqr(rgba[3] - colors[j][3]);
				cur_err[j] = e;
			}
		}
	}
}

template<typename Mode>
static void bc7enc_accumulate_scalar_errs(BC7IndexErrors * errs, const BC7BlockState * st, const BC7Input * in, BC7Flags flags)
{
	const int p = st->p;
	U64 inds = radtex_subset_to_inds[Mode::ns-1][p];

	const int ib = (Mode::isb != 0 && st->subsets[0].isbit) ? Mode::ib : Mode::ib2;
	const int num_lerp = 1 << ib;

	for (int s = 0; s < Mode::ns; ++s)
	{
		const U8 * rgba_base = in->subsets[s].pixels;
		const int num_pixels = in->subsets[s].num_pixels;
		const BC7SubsetState *sst = st->subsets + s;

		int colors[16][4];
		calc_lerp_bc7<Mode>(sst->isbit, colors, sst->endpoints);

		// outer loop over pixels
		for (int i = 0; i < num_pixels; ++i)
		{
			U8 a = rgba_base[i*4 + 3];

			// accumulate error totals for pixels in original numbering
			BC7Error * cur_err = errs->err[inds & 0xf];
			inds >>= 4;

			// @@ easy to SIMD by adapting the kernels we already have

			// inner loop over index options
			for (int j = 0; j < num_lerp; ++j)
				cur_err[j] += radtex_sqr(a - colors[j][3]);
		}
	}
}

void bc7enc_accumulate_index_errors(BC7IndexErrors errs[2], const BC7BlockState * st, const BC7Input * input, BC7Flags flags)
{
	switch (st->mode)
	{
	case 0: bc7enc_accumulate_vector_errs<BC7Mode<0> >(errs + 0, st, input, flags); break;
	case 1: bc7enc_accumulate_vector_errs<BC7Mode<1> >(errs + 0, st, input, flags); break;
	case 2: bc7enc_accumulate_vector_errs<BC7Mode<2> >(errs + 0, st, input, flags); break;
	case 3: bc7enc_accumulate_vector_errs<BC7Mode<3> >(errs + 0, st, input, flags); break;
	case 4:
		bc7enc_accumulate_vector_errs<BC7Mode<4> >(errs + st->subsets[0].isbit, st, input, flags);
		bc7enc_accumulate_scalar_errs<BC7Mode<4> >(errs + (st->subsets[0].isbit ^ 1), st, input, flags);
		break;
	case 5:
		bc7enc_accumulate_vector_errs<BC7Mode<5> >(errs + 0, st, input, flags);
		bc7enc_accumulate_scalar_errs<BC7Mode<5> >(errs + 1, st, input, flags);
	case 6: bc7enc_accumulate_vector_errs<BC7Mode<6> >(errs + 0, st, input, flags); break;
	case 7: bc7enc_accumulate_vector_errs<BC7Mode<7> >(errs + 0, st, input, flags); break;
	default: RR_BREAK();
	}
}

template<int t_ns, int t_ib>
static void bc7enc_pick_indices(BC7BlockState * st, int iset, const BC7IndexErrors * errs)
{
	const int p = st->p;
	const int num_lerp = 1 << t_ib;
	U64 inds = radtex_subset_to_inds[t_ns-1][p];

	// radtex_subset_anchors[] contains two anchor positions
	//	<< 4 gives implicit third anchor = 0
	int anchors = radtex_subset_anchors[t_ns-1][p] << 4;

	for (int s = 0; s < t_ns; ++s)
	{
		BC7Inds * out_inds = &st->subsets[s].idxs[iset];
		const int num_pixels = radtex_num_pixels_per_subset[t_ns][p][s];
		const int anchor = (anchors >> (s*4)) & 0xf;

		for (int i = 0; i < num_pixels; ++i)
		{
			const BC7Error * cur_err = errs->err[inds & 0xf];
			inds >>= 4;

			// anchors only get half as many options
			const int limit = num_lerp >> (i == anchor ? 1 : 0);
			int best = 0;
			BC7Error best_e = cur_err[0];

			for (int j = 1; j < limit; ++j)
			{
				if (cur_err[j] < best_e)
				{
					best_e = cur_err[j];
					best = j;
				}
			}

			out_inds->ind[i] = static_cast<U8>(best);
		}
	}
}

template<typename Mode>
static void bc7enc_reindex_for_min_error_mode(BC7BlockState * st, const BC7IndexErrors errs[2])
{
	bc7enc_pick_indices<Mode::ns, Mode::ib>(st, 0, errs + 0);
	if (Mode::ib2 != 0)
		bc7enc_pick_indices<Mode::ns, Mode::ib2>(st, 1, errs + 1);
}

void bc7enc_reindex_for_min_error(BC7BlockState * st, const BC7IndexErrors errs[2])
{
	switch (st->mode)
	{
	case 0: bc7enc_reindex_for_min_error_mode<BC7Mode<0> >(st, errs); break;
	case 1: bc7enc_reindex_for_min_error_mode<BC7Mode<1> >(st, errs); break;
	case 2: bc7enc_reindex_for_min_error_mode<BC7Mode<2> >(st, errs); break;
	case 3: bc7enc_reindex_for_min_error_mode<BC7Mode<3> >(st, errs); break;
	case 4: bc7enc_reindex_for_min_error_mode<BC7Mode<4> >(st, errs); break;
	case 5: bc7enc_reindex_for_min_error_mode<BC7Mode<5> >(st, errs); break;
	case 6: bc7enc_reindex_for_min_error_mode<BC7Mode<6> >(st, errs); break;
	case 7: bc7enc_reindex_for_min_error_mode<BC7Mode<7> >(st, errs); break;
	default: RR_BREAK();
	}
}

BC7MultiBlocks::BC7MultiBlocks(int count)
	: count(count)
{
	if (count <= MAX_INLINE)
		data = inline_data;
	else
		data = OodleNewArray(BC7BlockAndInput, count);
}

BC7MultiBlocks::~BC7MultiBlocks()
{
	if (count > MAX_INLINE)
		OodleDeleteArray(data, count);
}

void BC7MultiBlocks::set(int index, const U8 * block_bits, const U8 * pixels, BC7Flags flags)
{
	RR_ASSERT(index >= 0 && index < count);

	bc7enc_state_from_bits(&data[index].state, block_bits, flags);
	BC7Input_Get(&data[index].input, pixels, &data[index].state);
}

static void accumulate_lls(BC7SubsetLLSState lls[3], const BC7BlockAndInput *data)
{
	const BC7BlockState * st = &data->state;
	const BC7Input * input = &data->input;
	const BC7ModeDesc &mode = bc7_modes[st->mode];
	const U8 isbit = st->subsets[0].isbit;
	const F32 * factors_vector = radtex_lerp_factor_4x[isbit ? mode.ib2 : mode.ib][0];
	const F32 * factors_scalar = radtex_lerp_factor_4x[isbit ? mode.ib : mode.ib2][0];

	for (int s = 0; s < mode.ns; ++s)
	{
		const BC7SubsetState *sst = st->subsets + s;
		const BC7SubsetInput *in = input->subsets + s;
		BC7SubsetLLSState * acc = lls + s;

		// vector indices
		{
			const U8 * idxs = sst->idxs[isbit].ind;

			for (int i = 0; i < in->num_pixels; ++i)
			{
				const F32 * fac = factors_vector + idxs[i]*4;

				acc->ata[0] += fac[0];
				acc->ata[1] += fac[1];
				acc->ata[2] += fac[2];
				F32 w = fac[3];
				for(int c = 0; c < 4; ++c)
				{
					F32 p = in->pixels[i*4+c];
					acc->atb0[c] += w * p;
					acc->bsum[c] += p;
				}
			}
		}

		// scalar indices
		if (mode.ib2 != 0)
		{
			const U8 * idxs2 = sst->idxs[isbit ^ 1].ind;

			for (int i = 0; i < in->num_pixels; ++i)
			{
				const F32 * fac = factors_scalar + idxs2[i]*4;

				acc->atab_2[0] += fac[0];
				acc->atab_2[1] += fac[1];
				acc->atab_2[2] += fac[2];
				acc->atab_2[3] += fac[3] * in->pixels[i*4 + 3];
			}
		}
	}
}

struct BC7MultiblockIterContext
{
	const BC7MultiBlocks * multi;
	int subset;
	bool change_pbits;
};

template<typename Mode>
static void multiblock_subset_trial_noreindex(void * user_ptr, BC7SubsetState * sst, BC7Flags flags)
{
	BC7MultiblockIterContext * ctx = (BC7MultiblockIterContext *)user_ptr;
	const BC7MultiBlocks * multi = ctx->multi;
	int s = ctx->subset;

	sst->err = 0;
	sst->scalar_err = 0;

	// quantize endpoints once
	calc_endpoints_and_pbits<Mode>(sst->endpoints_q, sst, flags, ctx->change_pbits ? -1 : sst->pbits);

	for (int i = 0; i < multi->count; ++i)
	{
		const BC7BlockAndInput *bi = &multi->data[i];
		BC7Error local_err;
		BC7Error local_scalar_err;

		local_err = calc_subset_error<Mode>(&bi->state.subsets[s], &bi->input.subsets[s], sst->endpoints_q, &local_scalar_err);
		sst->err += local_err;
		sst->scalar_err += local_scalar_err;
	}

	// @@ for anneal, probably want to divide err by multi->count here so temperature is on same scale
	// for greedy it doesn't really matter though
}

void bc7enc_find_endpoints_multi(BC7BlockState * st, const BC7MultiBlocks * multi, BC7Flags flags, bool change_pbits, bool slow_anneal)
{
	if (!multi->count)
		return;

	// accumulation phase

	BC7SubsetLLSState lls[3];
	memset(lls,0,sizeof(lls));

	for (int i = 0; i < multi->count; ++i)
		accumulate_lls(lls, &multi->data[i]);

	typedef U8 CalcEndpoints(BC7Color endpoints_q[2], const BC7SubsetState *st, BC7Flags flags, int pbits);

	const BC7ModeDesc &mode = bc7_modes[st->mode];
	const int ib1_comp = (mode.ib2 == 0 && !(flags & BC7ENC_IGNORE_ALPHA)) ? 4 : 3;

	// reset actual error since we don't know it
	st->err = 0;

	CalcEndpoints *calc_endpoints;
	BC7IterativeTrialFunc *multi_trial;
	switch (st->mode)
	{
	case 0: calc_endpoints = calc_endpoints_and_pbits<BC7Mode<0> >; multi_trial = multiblock_subset_trial_noreindex<BC7Mode<0> >; break;
	case 1: calc_endpoints = calc_endpoints_and_pbits<BC7Mode<1> >; multi_trial = multiblock_subset_trial_noreindex<BC7Mode<1> >; break;
	case 2: calc_endpoints = calc_endpoints_and_pbits<BC7Mode<2> >; multi_trial = multiblock_subset_trial_noreindex<BC7Mode<2> >; break;
	case 3: calc_endpoints = calc_endpoints_and_pbits<BC7Mode<3> >; multi_trial = multiblock_subset_trial_noreindex<BC7Mode<3> >; break;
	case 4: calc_endpoints = calc_endpoints_and_pbits<BC7Mode<4> >; multi_trial = multiblock_subset_trial_noreindex<BC7Mode<4> >; break;
	case 5: calc_endpoints = calc_endpoints_and_pbits<BC7Mode<5> >; multi_trial = multiblock_subset_trial_noreindex<BC7Mode<5> >; break;
	case 6: calc_endpoints = calc_endpoints_and_pbits<BC7Mode<6> >; multi_trial = multiblock_subset_trial_noreindex<BC7Mode<6> >; break;
	case 7: calc_endpoints = calc_endpoints_and_pbits<BC7Mode<7> >; multi_trial = multiblock_subset_trial_noreindex<BC7Mode<7> >; break;
	default: RR_BREAK(); return;
	}

	BC7MultiblockIterContext ctx;
	ctx.multi = multi;
	ctx.change_pbits = change_pbits;

	for (int s = 0; s < mode.ns; ++s)
	{
		BC7SubsetState *sst = st->subsets + s;

		// reset err
		sst->err = BC7_ERROR_MAX;
		sst->scalar_err = BC7_ERROR_MAX/2;

		// LLS solve for vector components
		{
			const F32 ata00 = lls[s].ata[0];
			const F32 ata10 = lls[s].ata[1];
			const F32 ata11 = lls[s].ata[2];

			const F32 det = ata00 * ata11 - ata10 * ata10;
			if (det != 0.0f)
			{
				const F32 idet = 1.0f / det;
				F32 iata00 = ata11 * idet;
				F32 iata10 = ata10 * idet;
				F32 iata11 = ata00 * idet;

				for (int c = 0; c < ib1_comp; ++c)
				{
					F32 atb0c = lls[s].atb0[c];
					F32 atb1c = lls[s].bsum[c] - atb0c;
					sst->endpoints[0].v[c] = round_and_clamp_U8(iata11 * atb1c - iata10 * atb0c);
					sst->endpoints[1].v[c] = round_and_clamp_U8(iata00 * atb0c - iata10 * atb1c);
				}
			}
		}

		// LLS solve for scalar components
		if (mode.ib2 != 0)
		{
			const F32 ata00 = lls[s].atab_2[0];
			const F32 ata10 = lls[s].atab_2[1];
			const F32 ata11 = lls[s].atab_2[2];

			F32 det = ata00 * ata11 - ata10 * ata10;
			if (det != 0.0f)
			{
				const F32 idet = 1.0f / det;
				const F32 atb0 = lls[s].atab_2[3];
				const F32 atb1 = lls[s].bsum[3] - atb0;

				sst->endpoints[0].a = round_and_clamp_U8(idet * (ata00 * atb1 - ata10 * atb0));
				sst->endpoints[1].a = round_and_clamp_U8(idet * (ata11 * atb0 - ata10 * atb1));
			}
		}

		// quantize the endpoints to representable values
		BC7Color endpoints_q[2];
		sst->pbits = calc_endpoints(endpoints_q,sst,flags,change_pbits ? -1 : sst->pbits);

		sst->endpoints[0] = endpoints_q[0];
		sst->endpoints[1] = endpoints_q[1];

		// anneal on this subset if requested
		if ( slow_anneal )
		{
			ctx.subset = s;

			// greedyoptimize only on the final single block (iters_late)
			bc7enc_refine_greedyoptimize_endpoints_core(mode.mode, sst, flags, multi_trial, &ctx);
		}
	}
}

//======================================================

OODLE_NS_END


