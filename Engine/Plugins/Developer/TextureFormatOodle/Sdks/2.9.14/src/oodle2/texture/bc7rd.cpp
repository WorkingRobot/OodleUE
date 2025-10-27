// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetsse4

#include "bc7rd.h"
#include "rrsurface.h"
#include "blocksurface.h"
#include "rrdxtcrd.h"
#include "bc7compress.h"
#include "bc7compress_internal.h"
#include "bc7bits.h"
#include "bc67tables.h"
#include "bc67format.h"
#include "bc7decode_fast.h"
#include "perceptualactivity.h"
#include "perceptualactivity.inl"
#include "log2table.h"
#include "templates/rrvector.h"
#include "templates/rralgorithm.h"
#include "templates/rrhashtable.h"
#include "rrdxt1vqhelp.h"
#include "rrdxt1vqhelp.inl"
#include "rrlogutil.h"

//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"
#include "../include/oodle2texint.h"

//#define BC7RD_LAMBDA_SCALE 0.3125f // old
// 04-30-2020 : lambda normalization
// try using the same lambda scale as BC1
//	-> nope , that's too high (near 2.f)
//	-> but around 1.f looks okay
//#define BC7RD_LAMBDA_SCALE 1.f
//#define BC7RD_LAMBDA_SCALE 0.9f
#define BC7RD_LAMBDA_SCALE (0.8f * OO2TEX_GLOBAL_LAMBDA_SCALE)
// dial down to 0.8 just because the step from lambda 1->10 feels too big now
// J = D + lambda * R , VQD/bits

// bc7rd : 1F or 4F activity for e_make_activity_4F_R_G_B_A
//#define ActivityBlock4x4 FourFloatBlock4x4
//#define BlockSurface_SeekC_ActivityBlock4x4 BlockSurface_SeekC_FourFloatBlock4x4
#define ActivityBlock4x4 SingleFloatBlock4x4
#define BlockSurface_SeekC_ActivityBlock4x4 BlockSurface_SeekC_SingleFloatBlock4x4

// NOTE(fg): this is quite expensive, meant for debugging
//#define CHECK_EXTREMES_PRESERVED

#define BC7_BLOCK_ALIGN 32 // preferred alignment for decoded blocks.

RR_NAMESPACE_START

static const BC7_RD_Config c_config_levels[rrDXTCLevel_Count] =
{
	//rrDXTCLevel_VeryFast=0,	// == 1 secret level in OodleTex_ API ; too low quality, not currently well optimized for this quality-speed tradeoff
	{
		// block_encode_effort
		rrDXTCLevel_Slow,
		// core_fix_single_color_block
		true,
		// core_recompress_block_with_J_bias
		true,
		// core_disable_less_common_modes
		true,
		// core_endpoints_from_indices
		false,
		// MTF window size
		80, 2,
		// mtf_window_seed_most_common
		true,
		// do_ilm, ilm_hyperbola_max_index_product
		true, 80,
		// do_bottom_up_merge
		true,
		// do_final_matrix
		true,
		// fm_combined_size_limit_sqrt
		60,
		// do_final_optimize
		true,
	},

	//rrDXTCLevel_Fast=1,		// == OodleTex_EncodeEffortLevel_Low
	{
		// block_encode_effort
		rrDXTCLevel_Slow,
		// core_fix_single_color_block
		true,
		// core_recompress_block_with_J_bias
		true,
		// core_disable_less_common_modes
		true,
		// core_endpoints_from_indices
		false,
		// MTF window size
#ifdef _DEBUG
		80, 2,
#else
		(128+16+3), 2,
#endif
		// mtf_window_seed_most_common
		true,
		// do_ilm, ilm_hyperbola_max_index_product
		true, (128+16+3),
		// do_bottom_up_merge
		true,
		// do_final_matrix
		true,
		// fm_combined_size_limit_sqrt
		60,
		// do_final_optimize
		true,
	},

	//rrDXTCLevel_Slow=2,		// == OodleTex_EncodeEffortLevel_Normal
	{
		// block_encode_effort
		rrDXTCLevel_VerySlow,
		// core_fix_single_color_block
		true,
		// core_recompress_block_with_J_bias
		true,
		// core_disable_less_common_modes
		true,
		// core_endpoints_from_indices
		false,
		// MTF window size
#ifdef _DEBUG
		80, 2,
#else
		(256+64+16+3), 2,
#endif
		// mtf_window_seed_most_common
		true,
		// do_ilm, ilm_hyperbola_max_index_product
		true, 200,
		// do_bottom_up_merge
		true,
		// do_final_matrix,
		true,
		// fm_combined_size_limit_sqrt
		100,
		// do_final_optimize
		true,
	},

	//rrDXTCLevel_VerySlow=3,	// == OodleTex_EncodeEffortLevel_High == OodleTex_EncodeEffortLevel_Default
	{
		// block_encode_effort
		rrDXTCLevel_VerySlow,
		// core_fix_single_color_block
		true,
		// core_recompress_block_with_J_bias
		true,
		// core_disable_less_common_modes
		true,
		// core_endpoints_from_indices
		true,
		// MTF window size
#ifdef _DEBUG
		80, 64,
#else
		(256+64+16+3), (256+32+8+3),
#endif
		// mtf_window_seed_most_common
		true,
		// do_ilm, ilm_hyperbola_max_index_product
		true, 200,
		// do_bottom_up_merge
		true,
		// do_final_matrix,
		true,
		// fm_combined_size_limit_sqrt
		100,
		// do_final_optimize
		true,
	},

	//rrDXTCLevel_Reference=4	// == 99 secret level in OodleTex_ API ; too slow to be practical, not a good time-quality tradeoff; just a max quality reference
	{
		// block_encode_effort
		rrDXTCLevel_VerySlow,
		// core_fix_single_color_block
		true,
		// core_recompress_block_with_J_bias
		true,
		// core_disable_less_common_modes
		true,
		// core_endpoints_from_indices
		true,
		// MTF window size
#ifdef _DEBUG
		80, 64,
#else
		(256+64+16+3), (256+32+8+3),
#endif
		// mtf_window_seed_most_common
		true,
		// do_ilm, ilm_hyperbola_max_index_product
		true, 512,
		// do_bottom_up_merge
		true,
		// do_final_matrix,
		true,
		// fm_combined_size_limit_sqrt
		100,
		// do_final_optimize
		true,
	}
};

const BC7_RD_Config& bc7rd_get_config(rrDXTCLevel effort)
{
	//rrprintf("bc7rd_get_config: effort level=%d (%s)\n", effort, rrDXTCLevel_GetName(effort));

	RR_ASSERT( 0 <= (int)effort && (int)effort < RR_ARRAY_SIZE(c_config_levels) );
	return c_config_levels[effort];
}

// Tracks which pixels of a lbock are constrained to either 0 or 255 in preserve extremes mode
struct BC7Constraints
{
	// bit i+ 0 in mask is set if pixel i is constained to 0
	// bit i+16 in mask is set if pixel i is contrained to 255
	U32 mask;

	// The constraint set is empty if no constraints are active
	bool empty() const
	{
		return mask == 0;
	}

	// A set of constraints is feasible if it's not contradictory, i.e. no pixel
	// constrained to two different values
	bool feasible() const
	{
		// mask>>16 gives us an implicit & 0xffff for free
		return (mask & (mask >> 16)) == 0;
	}

	// Checks whether a given set of constraints is the superset of another
	bool superset_of(const BC7Constraints& x) const
	{
		return (mask & x.mask) == x.mask;
	}

	// Checks whether the intersection of two constraint sets is non-empty
	bool intersects_with(const BC7Constraints& x) const
	{
		return (mask & x.mask) != 0;
	}

	// Generates new constraints that flip 0<->255
	BC7Constraints flip() const
	{
		return BC7Constraints { (mask >> 16) | (mask << 16) };
	}

	static BC7Constraints none()
	{
		return BC7Constraints { 0 };
	}

	static BC7Constraints all_infeasible()
	{
		return BC7Constraints { ~0u };
	}

	static BC7Constraints from_pixels(const rrColorBlock4x4& b);

	static BC7Constraints from_pixels(const U8 pixels[64])
	{
		const rrColorBlock4x4* blk = (const rrColorBlock4x4*)pixels;
		return from_pixels(*blk);
	}

	static BC7Constraints from_indices(const BC7Inds& inds, int ib);

	BC7Constraints merge_with(const BC7Constraints &x) const
	{
		return BC7Constraints { mask | x.mask };
	}
};

BC7Constraints BC7Constraints::from_pixels(const rrColorBlock4x4& b)
{
	// NOTE(fg): trivial to optimize later if it's a hot spot
	U32 mask = 0;

	for (int i = 15; i >= 0; --i)
	{
		mask <<= 1;

		if (b.colors[i].u.a == 0) mask |= 1;
		if (b.colors[i].u.a == 255) mask |= 0x10000u;
	}

	return BC7Constraints { mask };
}

BC7Constraints BC7Constraints::from_indices(const BC7Inds& inds, int ib)
{
	// NOTE(fg): trivial to optimize later if it's a hot spot
	U8 hi_ind = (1 << ib) - 1;
	U32 mask = 0;

	for (int i = 15; i >= 0; --i)
	{
		mask <<= 1;

		if (inds.ind[i] == 0) mask |= 1;
		if (inds.ind[i] == hi_ind) mask |= 0x10000u;
	}

	return BC7Constraints { mask };
}

struct bc7rd_blockinfo
{
	BC7Flags flags;
	BC7Constraints constraints;
	F32 baseline_D;
	F32	J,D;
	rrColorBlock4x4 colors; // actually RGBA
};	
	
struct bc7rd_windowentry
{
	BC7BlockState st;
	bc7bits encoded;
};

struct BC7RDEndpointWindowExtra
{
	U8 mode; // block mode
	U8 mode_class; // mode class
	U8 pca_type; // used for mode 4-6 blocks to indicate what PCA vector to use, 0=disable early-out
	U8 _padding;
	F32 normalized_axis[4]; // used for mode 4-6 blocks only, rest set it to all zero

	void init(const BC7BlockState& st, const U8 *mode_class_map);

	F32 estimate_ssd(F32 overall_err, const F32 *scaled_pca) const
	{
#if defined(DO_BUILD_SSE4) || defined(DO_BUILD_NEON64)
		// We subtract the absolute dot product of the scaled pca vector and the normalized axis
		// vector from the overall PCA error estimate, which is our guess for the amount of error
		// remaining.
		VecF32x4 dot_muls = VecF32x4::loadu(scaled_pca) * VecF32x4::loadu(normalized_axis);
		return overall_err - dot_muls.sum_across_inner_outer().abs().scalar_x();
#else
		// Careful to match summation order with SIMD version:
		F32 dot = (scaled_pca[0] * normalized_axis[0] + scaled_pca[1] * normalized_axis[1])
			+ (scaled_pca[2] * normalized_axis[2] + scaled_pca[3] * normalized_axis[3]);

		return overall_err - fabsf(dot);
#endif
	}
};

void BC7RDEndpointWindowExtra::init(const BC7BlockState& st, const U8 *mode_class_map)
{
	mode = st.mode;
	mode_class = mode_class_map[mode];

	if (mode < 4 || mode > 6)
	{
		pca_type = 0;
		normalized_axis[0] = normalized_axis[1] = normalized_axis[2] = normalized_axis[3] = 0.0f;
		return;
	}

	const BC7SubsetState& subset = st.subsets[0];

	if (mode == 6)
		pca_type = 1;
	else // modes 4/5
		pca_type = subset.rbit + 2;

	F32 len_sq = 1e-15f;
	F32 diff[4];

	// mode 6 is 4-channel, modes 4/5 consider only 3 chans
	int n = (st.mode == 6) ? 4 : 3;
	diff[3] = 0.0f;

	for LOOP(c,n)
	{
		diff[c] = F32(subset.endpoints[1].v[c] - subset.endpoints[0].v[c]);
		len_sq += diff[c] * diff[c];
	}

	F32 scale = 1.0f / sqrtf(len_sq);
	for LOOP(c,4)
	{
		normalized_axis[c] = scale * diff[c];
	}
}

// warning on bc7_average_block_size_bits :
//	this is not super uniform per image, it can be quite different from one image to another
//	so just using some not-too-carefully gathered average is a bit suspect
static const F32 bc7_average_block_size_bits[8] = { 94.28325f,97.2345f,94.66392f,98.22575f,83.168f,84.9612f,79.677f,76.7255f };

static void bc7_decode_block4x4a(U8 * decomp_block, const void * bc7bits, BC7Flags flags)
{
	bc7_decode_block_fast(decomp_block,bc7bits,(flags & BC7ENC_IGNORE_ALPHA) != 0);
}

static bool check_extremes_preserved(const rrColorBlock4x4 & original, const rrColorBlock4x4 & decoded)
{
	const BC7Constraints orig_constraints = BC7Constraints::from_pixels(original);
	const BC7Constraints dec_constraints = BC7Constraints::from_pixels(decoded);
	return dec_constraints.superset_of(orig_constraints);
}

static F32 bc7rd_D(const U8 * decomp_block, const U8 * orig_block, BC7Flags flags, const ActivityBlock4x4 & activity)
{
	if ( flags & BC7ENC_IGNORE_ALPHA )
	{
		// VQD is RGBA ;  just make the A's equal before using it
		//		this was already done to orig_block in BC7Prep_init
		//		bc7enc_decode_state does it too
		//		so nothing is needed here
		
		for(int i=3;i<64;i+=4)
		{
			RR_ASSERT( orig_block[i] == 255 ); // done in BC7Prep_init
			RR_ASSERT( decomp_block[i] == 255 ); // this is done by bc7enc_decode_state (but not by bc7_decode_block)
		}
	}

	// RGBA vs BGRA order doesn't matter, it's 4-channel difference :
	RR_COMPILER_ASSERT( sizeof(rrColorBlock4x4) == 64 );
	const rrColorBlock4x4 * pc1 = (const rrColorBlock4x4 *)decomp_block;
	const rrColorBlock4x4 * pc2 = (const rrColorBlock4x4 *)orig_block;
	
#ifdef CHECK_EXTREMES_PRESERVED
	if ( flags & BC7ENC_PRESERVE_EXTREMES )
		RR_ASSERT_ALWAYS( check_extremes_preserved(*pc2, *pc1) );
#endif

	F32 D = VQD(*pc1,*pc2,activity);
	
	return D;
}

static F32 bc7rd_D(const BC7BlockState * st, const U8 * orig_block, BC7Flags flags, const ActivityBlock4x4 & activity)
{
	RAD_ALIGN(U8, decomp_block[64], BC7_BLOCK_ALIGN);
	bc7enc_decode_state(st,decomp_block,flags);
	
	return bc7rd_D(decomp_block,orig_block,flags,activity);
}

static F32 bc7rd_D(const bc7bits & bits, const U8 * orig_block, BC7Flags flags, const ActivityBlock4x4 & activity)
{
	RAD_ALIGN(U8, decomp_block[64], BC7_BLOCK_ALIGN);
	bc7_decode_block4x4a(decomp_block,bc7bits_U8ptr(&bits),flags);
	
	return bc7rd_D(decomp_block,orig_block,flags,activity);
}

static F32 bc7rd_D(const U8 * decomp_block, const rrColorBlock4x4 & orig_block, BC7Flags flags, const ActivityBlock4x4 & activity)
{
	return bc7rd_D(decomp_block,(const U8 *)&orig_block,flags,activity);
}

static F32 bc7rd_D(const bc7bits & bits, const rrColorBlock4x4 & orig_block, BC7Flags flags, const ActivityBlock4x4 & activity)
{
	return bc7rd_D(bits,(const U8 *)&orig_block,flags,activity);
}

static U32 bc7rd_SSD(const bc7bits & bits, const U8 * orig_block,BC7Flags flags)
{
	RAD_ALIGN(U8, decomp_block[64], BC7_BLOCK_ALIGN);
	bc7_decode_block4x4a(decomp_block,bc7bits_U8ptr(&bits),flags);
	
	const rrColorBlock4x4 * pc1 = (const rrColorBlock4x4 *)decomp_block;
	const rrColorBlock4x4 * pc2 = (const rrColorBlock4x4 *)orig_block;
	
	U32 ssd = ColorBlock4x4_ComputeSSD_RGBA(*pc1,*pc2);
	return ssd;
}

// uncompressed block is 128 bits
// R is just in bits
#define bc7rd_R_uncompressed	(128)

// bc7rd_matched_bits_to_lz_bits
//	take a number of shared bits
//	return an expected bit len of an lz match using only bytewise matches
//	result is <= matched_bits but >= 8*floor(matched_bits/8)
static F32 bc7rd_matched_bits_to_lz_bits( int matched_bits )
{
	RR_ASSERT( matched_bits >= 24 ); // mml 3
	
	// a 31 bit match = 
	//	3 bytes for sure
	//  + 50% chance of one more
	//	  (if that top bit is the right one to get you a 32 bit match)
	//  = 3.5 bytes on average
	//  = 28 bits
	// less than 31 but more than just truncating down to 24
	
	int matched_bytes = matched_bits / 8;
	int frac_bits = matched_bits - matched_bytes*8;
	RR_ASSERT( frac_bits >= 0 && frac_bits < 8 );
	F32 probability_of_another_byte = (256 >> (8 - frac_bits)) / 256.f;
	
	F32 mean_bytes = matched_bytes + probability_of_another_byte;
	F32 mean_bits = mean_bytes * 8.f;
	return mean_bits;
}

// bc7rd_lz_matched_bits
//	count the number of consecutive bits that are kept in a reindex/reendpoint

static int bc7rd_lz_matched_bits_reindex( int mode, bool change_pbits )
{
	// when you reindex, the preamble is all a match, {header,endpoints,pbits}
	int matched_bits = c_bc7_total_bits_header[mode] + c_bc7_total_bits_endpoints[mode];
	if ( ! change_pbits ) matched_bits += c_bc7_total_bits_pbits[mode];
	
	return matched_bits;
}

static int bc7rd_lz_matched_bits_reendpoint( int mode, bool change_pbits )
{
	// when you reendpoint, you lose the header match, not consecutive to indices
	int matched_bits = c_bc7_total_bits_indices[mode];
	if ( ! change_pbits ) matched_bits += c_bc7_total_bits_pbits[mode];
	
	return matched_bits;
}

static F32 bc7rd_matched_R_lz(int lzi)
{
	// cost of LZ match :
	//	note in BC7 match len and match offset are not constant / multiples of 4
	//	so they take more bits to send than in BC1
	
	// note this is the *difference* of sending a match vs. an LRL token
	//	so you could approximate by saying the tokens & lens cost roughly the same to send
	//	therefore we only need to count offset bits here
	
	#if 0
	// @@ constant match cost model :
	//	 note this tweak covers a lot of mis-estimations ; it's compensating for other parts of the code
	//F32 match_bits = 17;
	F32 match_bits = 16;
	
	return match_bits;
	
	#else
	// I don't have strong data to favor this
	// but I do like it theoretically for parse guiding
	// because it at least breaks near-ties in favor of recency
	
	// lzi is in blocks	
	// lzi starts at 1
	RR_ASSERT( lzi >= 1 );
	
	// @@ lzi only goes up to 256 so this could just be a table lookup
	
	// change lzi to bytes :
	//lzi *= 16;
	
	//F32 offset_bits = 2*(F32)rrlog2_bk(lzi+2) + 1;
	//F32 offset_bits = (F32)rrlog2_bk(lzi+7);
	//F32 match_bits = 10 + offset_bits;
	
	// adding larger constants to lzi decreases recency favoring

	// starts you off at 15 bits with lzi=1
	F32 offset_bits = (F32)rrlog2_bk(lzi+32);
	F32 match_bits = 10 + offset_bits;
	// could fiddle these constants more but it's over-training on one image
	
	return match_bits;
	
	#endif
}

static F32 bc7rd_matched_R( int matched_bits , int offset )
{
	// @@ mached_bits , align down to byte?
	//		this is meh at the moment @@ REVISIT ME
	//		note that when you change things like this you have to retweak the constant ("match_bits")
	F32 lz_matched_bits = bc7rd_matched_bits_to_lz_bits(matched_bits);
	//F32 lz_matched_bits = (F32) matched_bits;

	// portion sent raw unmatched :
	F32 raw_bits = 128 - lz_matched_bits;
	
	// the "matched_bits" are sent with an LZ match
	// cost of LZ match :
	F32 match_bits = bc7rd_matched_R_lz(offset);
	
	RR_ASSERT( lz_matched_bits >= match_bits ); // should be saving
	
	F32 bits = raw_bits + match_bits;
	
	RR_ASSERT( bits < 128.f );
	return bits;
}

static F32 bc7rd_J(F32 D, F32 R, F32 lambda)
{
	// D is VQD (~ 2*SSD)
	// R is just in bits (unlike vc1rd where it's 16*bits)
	// lambda is in steps of 10
	
	F32 J = D + lambda * R;
	
	return J;
}

static U32 bits_to_ssd_err_bias_J( F32 bits, F32 lambda )
{
	// * 0.5 because "err" in bc7comp is ssd while "D" in J is VQD
	// @@ this is the *2 SSD to VQD ratio
	//	 should have that on a #define so I can see the spots that use it
	//	 rather than hard-coded all over
	F32 ssd_J = 0.5f * ( lambda * bits );
	return (U32)( ssd_J + 0.5f ); // round to int
}

struct BC7_ModeHisto
{
	U32 count[BC7_NUM_MODES];
};

static void histogram_mode(BC7_ModeHisto * phisto,const BlockSurface * baseline_blocks)
{
	RR_ZERO(*phisto);
	
	for LOOP(bi,baseline_blocks->count)
	{
		const U8 * baseline_block_ptr = BlockSurface_SeekC(baseline_blocks,bi);

		// We don't want mode 5 (which is our mode of choice for solid-color blocks) to get
		// a boost just because there's many flat areas, since non-flat blocks are very
		// unlikely to benefit from this.
		if ( bc7_is_encoded_single_color_block( baseline_block_ptr ) )
			continue;

		bc7bits bits = bc7bits_load(baseline_block_ptr);
		int m = bc7bits_get_mode(bits);
		
		phisto->count[ m ] += 1;
	}
}

// In 2020 we tested histogramming both mode and partition shape index, but this is more complicated
// and didn't really improve results, so we're not doing it.
	
// ssd_err_bias_J == histo is okay
//	
static void histogram_to_ssd_err_bias_J(BC7_Mode_ErrBias * ssd_err_bias_J, const BC7_ModeHisto & histo, F32 lambda)
{
	// to handle the zero frequency / escape problem :
	//	scale up real counts by 8
	//	add +1 for novel symbols
	// @@ alternatives REVISIT
	const int count_scale = 8;
	const int count_bias = 1;
			
	U32 tot = 0;
	U32 mode_counts[8];
	for LOOP(m,8)
	{
		U32 c = histo.count[m] * count_scale + count_bias;
		mode_counts[m] = c;
		tot += c;
	}

	F32 log2_tot = rrlog2neg_bk_32(tot);
	
	for LOOP(m,8)
	{
		U32 c = mode_counts[m];
		F32 bits = rrlog2neg_bk_32(c) - log2_tot;
		ssd_err_bias_J->ssd_err_bias[m] = bits_to_ssd_err_bias_J(bits,lambda);
	}
}

static bool record_candidates_enabled = false;
static OodleTex_RecordEntry *record_entry = NULL;

OOFUNC1 void OOFUNC2 OodleTex_InstallVisualizerCallback(OodleTex_RecordEntry* callback, int width, int height)
{
	record_entry = callback;
	record_candidates_enabled=true;
	int arr[3] = { 0,width,height };
	record_entry(arr, sizeof(arr));
}

// @TODO: share this code with visualizer
struct ActivityItem
{
	S32 itemtype;
	float lambda;
	const void* ptr;
	float activity[16];
};

struct CandidateItem
{
	S32 itemtype;
	float lambda;
	const void* ptr;
	S32 phase;
	S32 mode;
	S32 bset;
	F32 D,R;
	U8 color_data[16][4];
	U32 padding; // due to 64-bit alignment
};

static RADNOINLINE void record_activity_block(F32 lambda, const BlockSurface* to_blocks, int bi, const ActivityBlock4x4& activity)
{
	if (!record_candidates_enabled) return;
	const void* q = (void*)BlockSurface_SeekC(to_blocks, bi); // we use this to identify blocks so we can reassemble image from multiple chunks
	ActivityItem item = { 1, lambda, q };
	memcpy(item.activity, activity.values, sizeof(item.activity));
	record_entry(&item, sizeof(item));
}

static RADNOINLINE void record_candidate_block(F32 lambda, int phase, const BlockSurface* to_blocks, int bi, const rrColorBlock4x4 & out_color, int mode, F32 D, F32 R, int bset=-1)
{
	if (!record_candidates_enabled) return;
	const void *q = (void*) BlockSurface_SeekC(to_blocks, bi); // we use this to identify blocks so we can reassemble image from multiple chunks
	CandidateItem item = { 2, lambda, q, phase, mode, bset, D, R };
	memcpy(item.color_data, &out_color.colors[0], sizeof(item.color_data));
	record_entry(&item, sizeof(item));
}

static RADNOINLINE void record_input_block_as_candidate(F32 lambda, int phase, const BlockSurface* to_blocks, const vector<bc7rd_blockinfo> &infos, int bi)
{
	if (!record_candidates_enabled) return;
	record_candidate_block(lambda, phase, to_blocks, bi, infos[bi].colors, 255, 0., 512.);
}

static RADNOINLINE void record_candidate_block(F32 lambda, int phase, const BlockSurface* to_blocks, int bi, const U8 out_block[16], F32 D, F32 R, BC7Flags flags)
{
	if (!record_candidates_enabled) return;
	RAD_ALIGN(U8, decomp_block[64], BC7_BLOCK_ALIGN);
	bc7_decode_block4x4a(decomp_block, out_block, flags);
	const rrColorBlock4x4* pc = (const rrColorBlock4x4*)decomp_block;
	record_candidate_block(lambda, phase, to_blocks, bi, *pc, bc7bits_get_mode(bc7bits_load(out_block)), D, R);
}

static RADNOINLINE void record_candidate_block(F32 lambda, int phase, const BlockSurface* to_blocks, int bi, BC7BlockState st, F32 D, F32 R, BC7Flags flags)
{
	RAD_ALIGN(U8, decomp_block[64], BC7_BLOCK_ALIGN);
	bc7enc_decode_state(&st, decomp_block, flags);
	const rrColorBlock4x4* pc = (const rrColorBlock4x4*)decomp_block;
	record_candidate_block(lambda, phase, to_blocks, bi, *pc, st.mode, D, R);
}

// this function takes the D and J values from infos[] and the colors from to_blocks;
static RADNOINLINE void record_candidate_block_from_infos(F32 lambda, int phase, const BlockSurface* to_blocks, const vector<bc7rd_blockinfo> &infos, int bi, BC7Flags flags)
{
	const void* out_block = (void*)BlockSurface_SeekC(to_blocks, bi);
	RAD_ALIGN(U8, decomp_block[64], BC7_BLOCK_ALIGN);
	bc7_decode_block4x4a(decomp_block, out_block, flags);
	const rrColorBlock4x4* pc = (const rrColorBlock4x4*)decomp_block;
	record_candidate_block(lambda, phase, to_blocks, bi, *pc, bc7bits_get_mode(bc7bits_load((const U8*)out_block)), infos[bi].D, (infos[bi].J - infos[bi].D) / lambda);
}

// this function recomputes D from the output BC7 block, and doesn't know what R is.
static RADNOINLINE void record_candidate_block_compute_D_unknown_R(F32 lambda, int phase, const BlockSurface* to_blocks, const vector<bc7rd_blockinfo> &infos, int bi, BC7Flags flags, const ActivityBlock4x4& activity, int set = -1)
{
	const rrColorBlock4x4& in_color = infos[bi].colors;
	const void* out_block = (void*)BlockSurface_SeekC(to_blocks, bi);
	RAD_ALIGN(U8, decomp_block[64], BC7_BLOCK_ALIGN);
	bc7_decode_block4x4a(decomp_block, out_block, flags);
	const rrColorBlock4x4* pc = (const rrColorBlock4x4*)decomp_block;
	F32 D = VQD(in_color, *pc, activity);
	record_candidate_block(lambda, phase, to_blocks, bi, *pc, bc7bits_get_mode(bc7bits_load((const U8*)out_block)), D, -1.0f, set);
}

struct BC7RDWindow // window of recent blocks with move-to-front semantics
{
	vector<bc7rd_windowentry> backing_store;
	vector<U32> mtf; // move-to-front list (inds into "backing_store" ordered from most to least recently used)
	int size = 0; // number of slots in MTF actually currently active. Element at [size] is special (used as "staging area")

	BC7RDWindow(int capacity);

	// Seed the window with most common blocks from baseline
	void seed(const BlockSurface * baseline_blocks, BC7Flags flags, vector<BC7RDEndpointWindowExtra> * window_endpoint_extra, const U8 * mode_class_map);

	// Update the window with a new state entry and an index of the slot where we found it if it
	// was a reused entry (pass negative value for lz_found_i for new entries)
	void update(const bc7rd_windowentry * new_state, int lz_found_i);

	bc7rd_windowentry& operator[](SINTa index)
	{
		RR_ASSERT(index <= size); // final slot is legal index (used as "staging area")
		return backing_store[mtf[index]];
	}

	const bc7rd_windowentry& operator[](SINTa index) const
	{
		RR_ASSERT(index <= size); // final slot is legal index (used as "staging area")
		return backing_store[mtf[index]];
	}
};

BC7RDWindow::BC7RDWindow(int capacity)
{
	// Allocate backing storage and MTF window pointers up front
	backing_store.resize(capacity);
	mtf.resize(capacity);

	// Set up the initial indices
	for LOOP(i, capacity)
	{
		mtf[i] = check_value_cast<U32>(i);
	}
}

void BC7RDWindow::seed(const BlockSurface * baseline_blocks, BC7Flags flags, vector<BC7RDEndpointWindowExtra> * window_endpoint_extra, const U8 * mode_class_map)
{
	// take baseline encoding
	//	find most common blocks (with indices zerod)
	//		so we find the common mode/part/endpoints
	
	vector<bc7bits> blocks;
	blocks.reserve(baseline_blocks->count);
	
	for LOOP(bi,baseline_blocks->count)
	{
		//@@ skip the first window_size blocks? (to avoid seeing self)
		// meh doesn't really make a difference either way
		//if ( bi < window_size ) continue;
	
		const U8 * baseline_block_ptr = BlockSurface_SeekC(baseline_blocks,bi);
	
		bc7bits bits = bc7bits_load(baseline_block_ptr);
		
		// do not zero pbits
		// when we try to reuse endpoints, we will try to reuse the pbits as well
		//	so leave them in now
		bc7bits no_indices;
		no_indices = bc7bits_zero_indices(bits,false);
		
		blocks.push_back(no_indices);
	}
	
	vector<bc7bits_and_count>  counting;
	sort_and_count_uniques(&counting,blocks);
	sort_bc7bits_and_count_compare_count_highest_first(&counting);
	
	counting.resize( RR_MIN(counting.size(),mtf.size() - 1) );
	
	// remove singletons :
	// maybe remove 2's also ?
	//	-> no that's really bad on "decanter" , otherwise meh, sometimes a slight win
	while( ! counting.empty() && counting.back().count == 1 )
	//while( ! counting.empty() && counting.back().count <= 2 )
		counting.pop_back();
		
	int num_seeds = counting.size32();
	
	// counting[s] has indices zeroed
	//	but that's okay because we're only seeding endpoints
	// read the encoded blocks back out to BC7BlockStates :

	// reset the MTF window size to the seed count
	// in MTF scheme, [0]th entry is the most favored
	//	 that's where we put the highest count, so order is good
	size = num_seeds;
	
	for LOOP(s,num_seeds)
	{
		bc7rd_windowentry& entry = (*this)[s];

		entry.encoded = counting[s].val;

		// clear preserve extremes bits from flags
		// we zeroed indices, the actual block contents are nonsense here
		bc7enc_state_from_bits(&entry.st,bc7bits_U8ptr(&counting[s].val), flags & ~BC7ENC_PRESERVE_EXTREMES);
		if (window_endpoint_extra)
			(*window_endpoint_extra)[mtf[s]].init(entry.st, mode_class_map);
	}
}

void BC7RDWindow::update(const bc7rd_windowentry * new_state, int lz_found_i)
{
	// < not <= since final slot is reserved
	RR_ASSERT( size < mtf.size32() );
	int move_to = 0;
	
	if ( lz_found_i < 0 )
	{
		// add it :
		// (final slot is reserved)
		if ( size + 1 < mtf.size32() )
			size++;
		
		lz_found_i = size - 1;

		// copy it in :
		(*this)[lz_found_i] = *new_state;
	}
	else
	{
		// lz_found_i was used
		// note that (*this)[lz_found_i] != *new_state
		// new_state is the actual encoding, with chosen indices for this block
		// they should have the same endpoints, so it shouldn't matter if you leave
		//	the old state in or stomp the new one
		
		// test this should be a nop : (confirmed)
		//(*this)[lz_found_i] = *new_state;
	}

	////NOTE(fg): this one's interesting
	//move_to = lz_found_i / 4;

	// MTF :
	RR_ASSERT_ALWAYS( move_to <= lz_found_i);
	U32 new_head = mtf[lz_found_i]; 
	memmove(&mtf[move_to+1],&mtf[move_to],(lz_found_i-move_to)*sizeof(mtf[0]));
	mtf[move_to] = new_head;
}

//===================================================================
// constraint validation for preserve extremes mode

// Cold part of are_endpoitns_permitted: the actual constraint validation
static RADNOINLINE bool are_endpoints_permitted_cold(const BC7BlockState & st, const bc7rd_blockinfo& info)
{
	const BC7Flags flags = info.flags;

	if (st.mode <= 3)
	{
		// Modes 0-3 (alpha always =255) only legal if we only have A=255 constrained pixels
		// since A=0 constraints can never be satisfied
		return (flags & BC7ENC_PRESERVE_A0) == 0;
	}
	else if (st.mode <= 5)
	{
		// Modes 4 and 5. Check whether min/max alpha obey our constraints.
		const BC7SubsetState& sst0 = st.subsets[0];

		// Constrained blocks must have alpha as the scalar channel
		if (sst0.rbit != 0)
			return false;

		const U8 a0 = sst0.endpoints[0].a;
		const U8 a1 = sst0.endpoints[1].a;

		if ((flags & BC7ENC_PRESERVE_A0) && RR_MIN(a0, a1) != 0)
			return false;
		if ((flags & BC7ENC_PRESERVE_A255) && RR_MAX(a0, a1) != 255)
			return false;

		// If the first pixel in block (=anchor pixel) is constrained, need to make sure
		// the endpoint ordering is such that the constraint is satisfied, since we can't
		// swap endpoints without losing the endpoint reuse that's the whole point
		if ( info.constraints.mask & 0x00010001u )
		{
			// implied index MSB at anchor pixel is always 0
			// -> if constrained lo, a0 needs to be 0;
			// if constrained hi, a0 needs to be 255
			return a0 == ((info.constraints.mask & 1) ? 0 : 255);
		}

		return true;
	}
	else
	{
		RR_ASSERT(st.mode <= 7);

		// Modes 6 and 7. We only allow this if both endpoint A values
		// are equal and match the constraint value.
		//
		// If both A=0 and A=255 are present, this is never legal
		if ((flags & BC7ENC_PRESERVE_EXTREMES) == BC7ENC_PRESERVE_EXTREMES)
			return false;

		const U8 need_a = (flags & BC7ENC_PRESERVE_A0) ? 0 : 255;

		// Check first subset always
		const BC7SubsetState& sst0 = st.subsets[0];
		if (sst0.endpoints[0].a != need_a || sst0.endpoints[1].a != need_a)
			return false;

		// Mode 7 also has a second subset that needs to be checked
		if (st.mode == 7)
		{
			const BC7SubsetState& sst1 = st.subsets[1];
			if (sst1.endpoints[0].a != need_a || sst1.endpoints[1].a != need_a)
				return false;
		}

		return true;
	}
}

// Hot path is just the "preserve extremes" check, which is usually off. This part should get inlined.
static RADFORCEINLINE bool are_endpoints_permitted(const BC7BlockState & st, const bc7rd_blockinfo& info)
{
	// If no preserve extremes, we don't have particular endpoint constraints
	if ((info.flags & BC7ENC_PRESERVE_EXTREMES) == 0)
		return true;

	return are_endpoints_permitted_cold(st, info);
}

// Can we do an index substitution? Only one caller, so might as well forceinline.
static RADFORCEINLINE bool are_indices_permitted(const BC7BlockState & st, const bc7rd_blockinfo& info, BC7Flags *pFlags)
{
	*pFlags = info.flags;
	BC7Flags flags = info.flags;

	// If no preserves extremes, we don't have particular index contraints
	if ((flags & BC7ENC_PRESERVE_EXTREMES) == 0)
		return true;

	if (st.mode <= 3)
	{
		// Modes 0-3 (alpha always =255) only legal if we only have A=255 constrained pixels
		// since A=0 constraints can never be satisfied, but no actual index constraints
		return (flags & BC7ENC_PRESERVE_A0) == 0;
	}
	else if (st.mode <= 5)
	{
		// Modes 4 and 5. In this case, we have index constraints.
		const BC7SubsetState& sst = st.subsets[0];

		// Constrained blocks must have alpha as the scalar channel
		if (sst.rbit != 0)
			return false;

		// Grab the alpha indices
		const BC7Inds& a_inds = sst.idxs[sst.isbit ^ 1];

		// Figure out extremal index values; we don't care which decode to A=0 or A=255,
		// since we determine new endpoints after, the two can switch places, so either
		// works.
		const int ib = (st.mode == 5 || sst.isbit == 1) ? 2 : 3; // Mode 4 ib2 is 3 bits, else 2 bits index.
		BC7Constraints satisfiable = BC7Constraints::from_indices(a_inds, ib);

		if (satisfiable.superset_of(info.constraints))
		{
			// Satisfiable with min_ind->0 and max_ind->255 ordering
			*pFlags = flags | BC7ENC_AEP0_LO;
			return true;
		}
		else if (satisfiable.flip().superset_of(info.constraints))
		{
			// Satisfiable with min_ind->255 and max_ind->0 ordering
			*pFlags = flags | BC7ENC_AEP0_HI;
			return true;
		}

		return false;
	}
	else
	{
		RR_ASSERT(st.mode <= 7);

		// Modes 6 and 7. We only use this for constrained blocks when A is constant.
		// We only use them for A=constant blocks and pick feasible endpoints, so
		// there are no actual index constraints here. If mode 6/7 were allowed, any
		// indices work. In mode 7, the pbits need to be compatible as well.
		if (flags & BC7ENC_DISABLE_MODE67)
			return false;

		if (st.mode != 7)
			return true;

		// In mode 7, the pbits for both subsets need to be all 0 when we're trying
		// to hit A=0 and all 1 when we're trying to hit A=255.
		U8 need_pbits = (flags & BC7ENC_PRESERVE_A255) ? 3 : 0;
		return st.subsets[0].pbits == need_pbits && st.subsets[1].pbits == need_pbits;
	}
}

//===================================================================
// final optimize

// get D over the block set
//	if we changes the bits in bits_mask to new_bits
static F32 bc7rd_final_optimize_blockset_D(
	const BlockSurface * bc7_blocks,
	const BlockSurface * activity_blocks,
	const vector<bc7rd_blockinfo> & infos,
	BC7Flags flags,
	const bc7bits_and_count * set_indexes,int set_size,
	const bc7bits & new_bits,const bc7bits & bits_mask)
{	
	// new_bits should be inside bits_mask
	// before we do anything, the blockset should have all the bits in bits_mask the same
	#ifdef RR_DO_ASSERTS
	{
		const U8 * block_ptr_first = BlockSurface_SeekC(bc7_blocks,set_indexes[0].count);
		bc7bits bits_before = bc7bits_and( bc7bits_load(block_ptr_first) , bits_mask );
		
		for LOOP(set_i,set_size)
		{
			int bi = set_indexes[set_i].count;
			const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);		
			bc7bits bits = bc7bits_load(block_ptr);
			bits = bc7bits_and(bits,bits_mask);
			RR_ASSERT( bits == bits_before );
		}
	}
	#endif
	
	F32 tot_D = 0;
	
	// get blocks and apply candidate new_bits :
	for LOOP(set_i,set_size)
	{
		int bi = set_indexes[set_i].count;
		const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);		
		bc7bits bits = bc7bits_load(block_ptr);
		bits = bc7bits_andnot(bits,bits_mask);
		bits = bc7bits_or(bits,new_bits);
		
		// measure D :

		// colors are RGBA :
		RAD_ALIGN(rrColorBlock4x4, colors, BC7_BLOCK_ALIGN);
		bc7_decode_block4x4a((U8 *)&colors,bc7bits_U8ptr(&bits),flags);

		const rrColorBlock4x4 & orig_colors = infos[bi].colors;		

		// ?? VQD or just SSD ? -> it looks like probably SSD
		//	(BC1RD is SSD)

		#if 0
		const ActivityBlock4x4 * pActivity = (const ActivityBlock4x4 *) BlockSurface_SeekC(activity_blocks,bi);
		tot_D += VQD(colors,orig_colors,*pActivity);
		#else		
		U32 ssd = ColorBlock4x4_ComputeSSD_RGBA(colors,orig_colors);
		tot_D += (F32)ssd;
		#endif
	}
	
	return tot_D;
}

static int bc7_read_partition_id(const U8 * block, int mode)
{
	// partition ID is always the first field after the mode id, which takes mode+1 bits
	RR_ASSERT(mode >= 0 && mode <= 7);

	// we always have the full partition id within the first 16 bits so that's all we need
	U32 bytes = block[0] | (block[1] << 8);
	U32 partition_bits = bytes >> (mode + 1);
	U32 partition_mask = (1 << bc7_modes[mode].pb) - 1;
	return partition_bits & partition_mask;
}

// Returns whether merging is feasible for this one block principle, updates the index
// constraints. The resulting index constraints after merging over multiple blocks
// might still be infeasible though!
static bool may_merge_indices(const U8* block_ptr, int mode, const bc7rd_blockinfo& info, BC7Constraints out_ind_constraints[2])
{
	// Nothing to do here for modes 0-3 since they decode to a fixed A=255 for all pixels. A block can either
	// legally use that (when the only constraints are of the A=255 for individual pixels type) or it can't;
	// within this mode, no coding decisions influence what A values get produced.
	if ((info.flags & BC7ENC_PRESERVE_EXTREMES) == 0 || mode < 4)
		return true;

	// Check whether we need to update our index constraints
	BC7Constraints block_constraints = info.constraints;

	BC7BlockState st;
	bc7enc_state_from_bits(&st,block_ptr,info.flags);
	RR_ASSERT(st.mode == mode);

	if (mode <= 5)
	{
		// Block has "preserve extremes" constraints and is mode 4/5, should have index
		// constraints
		RR_ASSERT(!block_constraints.empty());

		const BC7SubsetState& sst = st.subsets[0];
		if (sst.rbit != 0)
		{
			// We can have mode 4/5 blocks that have non-0 rbit (i.e. A is not the scalar channel),
			// which is one of the cases where thing happen to work out "by luck"; we can't
			// do a full final optimize here.
			return false;
		}

		// Modes 4/5 can have non-trivial index constraints.
		// Use the endpoints to figure out whether index 0 maps to A=0 or A=255.
		const int ainds = sst.isbit ^ 1; // index set containing A inds
		const int diff = sst.endpoints[0].a - sst.endpoints[1].a;

		if (diff < 0) // index 0 -> A=0 (our canonical ordering)
			out_ind_constraints[ainds] = out_ind_constraints[ainds].merge_with(block_constraints);
		else if (diff > 0) // index 0 -> A=255 (flipped ordering)
			out_ind_constraints[ainds] = out_ind_constraints[ainds].merge_with(block_constraints.flip());

		// If diff == 0, our index choices don't change decoded A so this block
		// has no actual index constraints.
		return true;
	}
	else
	{
		RR_ASSERT(6 <= mode && mode <= 7);
		const int ns = (mode == 6) ? 1 : 2;

		// For modes 6 and 7, we don't do index constraints, and our baseline encoding will only ever use these
		// for "preserve extremes" blocks with constant endpoint A (i.e. both endpoint pairs have the same A
		// value). In this particular case, there are no index constraints and we can just change indices
		// freely, because once again the choice of index bits makes no difference for decoded A values.
		//
		// However, some of the earlier BC7RD passes just splice bits from blocks together, so we have no
		// guarantee that blocks we see here are blocks that the baseline encoder would produce. We _do_
		// maintain the invariant that "preserve extremes" constraints are preserved throughout.
		//
		// Therefore, it is possible for us to end up for mode 6/7 blocks with an endpoint pair that is not
		// purely constant. If we ended up with that, just bail out of the final optimize for this set and
		// leave it alone. It's OK now, but tricky to preserve, don't mess with it.
		for LOOP(s,ns)
		{
			if (st.subsets[s].endpoints[0].a != st.subsets[s].endpoints[1].a)
				return false;
		}

		return true;
	}
}

static bc7bits bc7rd_final_optimize_blockset_indices(
	int mode,
	const BlockSurface * bc7_blocks,
	const BlockSurface * activity_blocks,
	const vector<bc7rd_blockinfo> & infos,
	BC7Flags flags,
	const bc7bits_and_count * set_indexes,int set_size,
	const bc7bits & bits_mask)
{
	RR_ASSERT(set_size >= 1);

	// blockset currently has all the same {indexes+pbits}
	//	but those may not be best for the set (keeping rest of the block the same)
	// find new candidate best {indexes+pbits}

	const int NUM_CLASSES = 14; // for 3-subset; 2-subset needs 4, 1-subset is 1
	const int ns = bc7_modes[mode].ns;
	int cls_counts[NUM_CLASSES] = {};
	int cur_max_cls = 0;
	int cur_max_count = 0;
	int cls_repr = 0;

	// We collect constraints on the indices here
	// unlike the block constraints which track A=0 and A=255 respectively,
	// these track index=0 (min) and index=max pixels in either index set.
	BC7Constraints index_constraints[2] = { { 0 }, { 0 } };

	for LOOP(i,set_size)
	{
		int bi = set_indexes[i].count;
		const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);

		int part = bc7_read_partition_id(block_ptr, mode);
		int cls = radtex_anchor_eqv[ns-1][part];
		cls_counts[cls]++;

		if (cls_counts[cls] > cur_max_count)
		{
			cur_max_count = cls_counts[cls];
			cur_max_cls = cls;
			cls_repr = bi; // just pick any i in class as representative
		}

		// Accumulate constraints and do other validity checks.
		//
		// Nothing to do here for modes 0-3 since they decode to a fixed A=255 for all pixels. A block can either
		// legally use that (when the only constraints are of the A=255 for individual pixels type) or it can't;
		// within this mode, no coding decisions influence what A values get produced.
		if (!may_merge_indices(block_ptr,mode,infos[bi],index_constraints))
		{
			return bc7bits_and(bc7bits_load(block_ptr), bits_mask);
		}
	}

	//if (cls_counts[cur_max_cls] != set_size)
	//	rrprintf("(%d,%d) covers %d/%d (%.1f%%)\n", mode, cur_max_cls, cls_counts[cur_max_cls], set_size, 100.0 * cls_counts[cur_max_cls] / set_size);

	// All block started out with the same index _bits_, but not necessarily
	// the same indices; if we have index constraints, we need to check whether
	// it's possible for us to satisfy them in our solve.
	if (!index_constraints[0].empty() || !index_constraints[1].empty())
	{
		// We can only have non-empty index constraints in modes 4 and 5 which are single-subset.
		// Therefore when we have non-trivial constraints, all blocks should be in the same
		// equivalence class.
		RR_ASSERT(mode == 4 || mode == 5);
		RR_ASSERT(cls_counts[cur_max_cls] == set_size);

		// We *can* have a constraint that pixel 0 be set to the max index value; since that's
		// an anchor pixel (MSB not sent, implied to be 0), this is never possible to satisfy.
		BC7Constraints impossible_constraints = { 0x10000u }; // index 0=max is impossible

		// We can also have constraints that are infeasible because some blocks want an index to be set
		// to its min value, and other blocks in the set want it to be at its max.
		//
		// If we have infeasible constraints, bail.
		if (!index_constraints[0].feasible() ||
			!index_constraints[1].feasible() ||
			index_constraints[0].intersects_with(impossible_constraints) ||
			index_constraints[1].intersects_with(impossible_constraints))
		{
			// Leave the existing bits alone
			const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,cls_repr);
			return bc7bits_and(bc7bits_load(block_ptr), bits_mask);
		}
	}

	// Loop over blocks in largest equivalence class and accumulate their errors
	BC7IndexErrors errs[2];
	errs[0].init();
	errs[1].init();

	for LOOP(i,set_size)
	{
		int bi = set_indexes[i].count;
		const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);

		// The rest of this processing, we only want to do for blocks belonging to the largest
		// equivalence class
		int part = bc7_read_partition_id(block_ptr, mode);
		int cls = radtex_anchor_eqv[ns-1][part];
		if (cls != cur_max_cls)
			continue;

		BC7BlockState st;
		bc7enc_state_from_bits(&st,block_ptr,infos[bi].flags);
		BC7Input input;
		BC7Input_Get(&input,(const U8 *)&(infos[bi].colors),&st);

		// NOTE: no support for changing pbits here; we've never done it here and it
		// wouldn't work with "preserve extremes" anyway
		bc7enc_accumulate_index_errors(errs,&st,&input,infos[bi].flags);
	}

	// Pick the indices for the class representative block
	const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,cls_repr);

	BC7BlockState st;
	bc7enc_state_from_bits(&st,block_ptr,infos[cls_repr].flags);

	// If we have non-trivial index constraints, enforce them now
	for (int index_bit_sel = 0; index_bit_sel < 2; ++index_bit_sel)
	{
		if (index_constraints[index_bit_sel].empty())
			continue;

		// Enforce constraints by changing the errors of constrained pixels so the
		// required value gets the lowest error
		BC7IndexErrors * cur_errs = &errs[index_bit_sel];
		int ib = index_bit_sel ? bc7_modes[mode].ib2 : bc7_modes[mode].ib;
		int max_ind = (1 << ib) - 1;

		const U32 constraint_mask = index_constraints[index_bit_sel].mask;
		U32 pixel_mask = (constraint_mask & 0xffff) | (constraint_mask >> 16); // mask of which pixels have active constraints

		while (pixel_mask)
		{
			int pixel = rrCtz32(pixel_mask);
			pixel_mask &= pixel_mask - 1; // clear lowest set bit

			// For constrained pixels, change the error for all the options other than
			// the required one to make them not win
			if (constraint_mask & (1u << pixel))
			{
				// constrained low (index must be 0)
				BC7Error target_err = cur_errs->err[pixel][0];
				for (int i = 1; i <= max_ind; ++i)
					cur_errs->err[pixel][i] = target_err + 1; // make all other inds worse
			}
			else
			{
				// Constrained high
				// we checked before that constraints are feasible
				RR_ASSERT(constraint_mask & (0x10000u << pixel));
				BC7Error target_err = cur_errs->err[pixel][max_ind];
				for (int i = 0; i < max_ind; ++i)
					cur_errs->err[pixel][i] = target_err + 1; // make all other inds worse
			}
		}
	}

	/*
	bc7bits old_bits = bc7bits_load(block_ptr);
	BC7BlockState check_st = st;
	/**/

	bc7enc_reindex_for_min_error(&st,errs);

	bc7bits output_bc7;
	bc7enc_emit(bc7bits_U8ptr(&output_bc7),&st, flags);

	/*// debug sanity check: if class has only one member, our
	// results should match regular exact reindex
	if (cls_counts[cur_max_cls] == 1)
	{
		BC7Error accum_err = bc7enc_calc_error(&st,(const U8 *)&infos[cls_repr].colors,flags);

		BC7BlockState reindex_st;
		bc7enc_state_from_bits(&reindex_st,block_ptr);

		BC7Input input;
		BC7Input_Get(&input,(const U8 *)&infos[cls_repr].colors,&st);
		bc7enc_reindex_block(&reindex_st,input.subsets,BC7ENC_IDXS_EXACT);

		BC7Error reindex_err = bc7enc_calc_error(&reindex_st,(const U8 *)&infos[cls_repr].colors,flags);
		RR_ASSERT(accum_err == reindex_err);
	}*/

	/*
	bc7bits check_bc7;
	bc7enc_emit(bc7bits_U8ptr(&check_bc7),&check_st,infos[cls_repr].flags);
	RR_ASSERT_ALWAYS( check_bc7 == old_bits );
		
	// should not have changed bits outside of bits_mask :
	//RR_ASSERT( bc7bits_andnot(old_bits,bits_mask) == bc7bits_andnot(output_bc7,bits_mask) );
	bc7bits old_other = bc7bits_andnot(old_bits,bits_mask);
	bc7bits out_other = bc7bits_andnot(output_bc7,bits_mask);
	bc7bits x = bc7bits_xor(old_other,out_other);
	RR_ASSERT_ALWAYS( old_other == out_other );
	/**/
	
	bc7bits new_bits = bc7bits_and(output_bc7,bits_mask);
	
	return new_bits;
}

static bc7bits bc7rd_final_optimize_blockset_endpoints(
	int mode,
	const BlockSurface * bc7_blocks,
	const BlockSurface * activity_blocks,
	const vector<bc7rd_blockinfo> & infos,
	BC7Flags flags,
	const bc7bits_and_count * set_indexes,int set_size,
	const bc7bits & bits_mask,
	BC7MultiBlockStorage * storage)
{
	RR_ASSERT(set_size >= 1);

	// blockset currently has all the same {endpoints+pbits}
	//	but those may not be best for the set (keeping rest of the block the same)
	// find new candidate best {endpoints+pbits}
	BC7BlockState st;

	BC7Flags combined_flags = flags & ~(BC7ENC_PRESERVE_EXTREMES | BC7ENC_AEP0_CONSTRAINED);
	BC7MultiBlocks multi(storage, set_size);

	for LOOPINT(i,set_size)
	{
		int bi = set_indexes[i].count;

		// Each block decodes with its own infos[bi].flags
		multi.set(i,BlockSurface_SeekC(bc7_blocks,bi),(const U8 *)&(infos[bi].colors),infos[bi].flags);

		// Keep track of which of the extremal values we need
		combined_flags |= infos[bi].flags & BC7ENC_PRESERVE_EXTREMES;

		// Track constraints we have to enforce on the endpoints and do other validity checks.
		//
		// Mode 0-3 have nothing to do here; they decode every pixel as A=255 no matter what we do.
		// If the original blocks are valid, so will be everything we produce. In the other modes,
		// we need to be more careful.
		if ((infos[bi].flags & BC7ENC_PRESERVE_EXTREMES) && mode >= 4)
		{
			const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);
			bool valid = true;

			bc7enc_state_from_bits(&st,block_ptr,infos[bi].flags);
			RR_ASSERT(st.mode == mode);

			if (!are_endpoints_permitted(st,infos[bi]))
				valid = false;

			if (mode <= 5) // we checked mode >= 4 above
			{
				const BC7SubsetState& sst = st.subsets[0];
				BC7Constraints block_constraints = infos[bi].constraints;

				// Figure out whether this block needs index 0 to map to the lower or higher A value.
				// If both endpoints are same, the indices could be anything; need to preserve that
				// property to be safe.
				//
				// If we disagree about the ordering within the set, then the solve is infeasible.
				const int diff = sst.endpoints[0].a - sst.endpoints[1].a;

				if (diff != 0)
				{
					combined_flags |= (diff < 0) ? BC7ENC_AEP0_LO : BC7ENC_AEP0_HI;

					const int ib = (st.mode == 5 || sst.isbit == 1) ? 2 : 3; // Mode 4 ib2 is 3 bits, else 2 bits index.
					BC7Constraints satisfiable = BC7Constraints::from_indices(sst.idxs[sst.isbit ^ 1], ib);

					if ((diff < 0) && !satisfiable.superset_of(block_constraints))
						valid = false;

					if ((diff > 0) && !satisfiable.flip().superset_of(block_constraints))
						valid = false;
				}
				else
				{
					// If endpoint values were the same and we have a "preserve extremes"
					// constraint on the block, there is exactly one A value that works, and that's
					// the one the block already has, so we can't touch endpoints at all in this set;
					// can bail immediately.
					valid = false;
				}
			}
			else
			{
				RR_ASSERT(6 <= mode && mode <= 7);

				// For constrained blocks in mode 6/7, the only case we even support is endpoint A values same,
				// which (just like in the diff==0 case above) means there is at most one solution that we
				// could find in this function, and the endpoint set either uses that solution already (in
				// which case there's no point in us trying to rediscover it) or has a random combination
				// of endpoints/indices that happens to satisfy the constraints that we wouldn't have found
				// systematically.
				//
				// In either case, there's nothing for us to do.
				valid = false;
			}

			if (!valid)
			{
				return bc7bits_and(bc7bits_load(block_ptr), bits_mask);
			}
		}
	}

	// Do we have "preserve endpoints" constraints?
	if (combined_flags & BC7ENC_PRESERVE_EXTREMES)
	{
		// If we have both blocks that want endpoint 0 A strictly low and blocks
		// that want it high, there is no feasible solution, just leave the blocks alone.
		if ((combined_flags & BC7ENC_AEP0_CONSTRAINED) == BC7ENC_AEP0_CONSTRAINED)
		{
			int bi = set_indexes[0].count; // random representative
			const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);
			return bc7bits_and(bc7bits_load(block_ptr),bits_mask);
		}
	}

	// Solve for the new endpoints
	int bi = set_indexes[set_size/2].count; // just use a random block to start with
	const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);
	bc7enc_state_from_bits(&st,block_ptr,infos[bi].flags); // need to use infos[bi].flags of that block when decoding to get correct endpoints_q

	bool change_pbits = false;
	bool slow_anneal = true; // <- helps nicely ; fast enough?
	bc7enc_find_endpoints_multi(&st,&multi,combined_flags,change_pbits,slow_anneal);

	bc7bits output_bc7;
	bc7enc_emit(bc7bits_U8ptr(&output_bc7),&st,combined_flags);

	/*
	bc7bits check_bc7;
	bc7enc_emit(bc7bits_U8ptr(&check_bc7),&check_st);	
	RR_ASSERT( check_bc7 == old_bits );
	
	// should not have changed bits outside of bits_mask :
	//RR_ASSERT( bc7bits_andnot(old_bits,bits_mask) == bc7bits_andnot(output_bc7,bits_mask) );
	bc7bits old_other = bc7bits_andnot(old_bits,bits_mask);
	bc7bits out_other = bc7bits_andnot(output_bc7,bits_mask);
	bc7bits x = bc7bits_xor(old_other,out_other);
	RR_ASSERT( old_other == out_other );
	/**/
	
	bc7bits new_bits = bc7bits_and(output_bc7,bits_mask);
	
	return new_bits;
}

static void bc7rd_final_optimize(BlockSurface * bc7_blocks,
	const BlockSurface * activity_blocks,
	const vector<bc7rd_blockinfo> & infos,
	BC7Flags flags,
	F32 lambda, // for OODLE_RECORD_CANDIDATE_BLOCKS
	bool do_indices)
{
	// find sets of blocks that have the same indices/endpoints
	RR_UNUSED_VARIABLE(lambda);
	
	// for each mode
	//	 segregate into modes
	//	you could get accidential matches across modes otherwise
	
	int tot_num_blocks = check_value_cast<int>(bc7_blocks->count);

	vector<bc7bits_and_count> bits_and_index;
	bits_and_index.reserve(tot_num_blocks);

	BC7MultiBlockStorage multi_storage;
	
	for LOOPINT(mode,8)
	{
		bits_and_index.clear();
		
		bc7bits bits_mask;
		// always without pbits; with pbits we can't preserve extremes, and it wasn't
		// much of a difference back when we supported it anyway.
		if ( do_indices ) bits_mask = c_bc7bitrange_indices_mask[mode];
		else bits_mask = c_bc7bitrange_endpoints_mask[mode];
		
		for LOOPINT(bi,tot_num_blocks)
		{
			const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);		
			bc7bits bits = bc7bits_load(block_ptr);
		
			int block_mode = bc7bits_get_mode(bits);
			if ( block_mode != mode )
				continue;

			// Mode 5 contains single-color blocks. These are excluded from
			// final optimization because there's absolutely no point.
			if ( mode == 5 && bc7_is_encoded_single_color_block(block_ptr) )
				continue;

			bits_and_index.push_back();
			bits_and_index.back().val = bc7bits_and(bits,bits_mask);
			bits_and_index.back().count = bi;
		}
		
		if ( bits_and_index.empty() ) // none of this mode
			continue;
		
		sort_bc7bits_and_count_compare_bc7bits(&bits_and_index);
		
		// find groups of same val :
		
		bc7bits zero_bits;
		bc7bits_set_zero(&zero_bits);
		
		for(int begin=0;begin<bits_and_index.size32();)
		{
			bc7bits bits_before = bits_and_index[begin].val;
			int end = begin+1; // end is 1 past the last
			while( end < bits_and_index.size32() && bits_and_index[end].val == bits_before )
				end++;
			
			// the range [begin,end) has the same indices+pbits
			// find best for that range
			
			// begin,end are not block indices
			//	they're indices in the bits_and_index array, where .count is the block index
			int set_size = end-begin;
			bc7bits_and_count * set_indexes = &bits_and_index[begin];

			bc7bits new_bits;
			
			if ( do_indices )
				new_bits = bc7rd_final_optimize_blockset_indices(mode,bc7_blocks,activity_blocks,infos,flags,set_indexes,set_size,bits_mask);
			else
				new_bits = bc7rd_final_optimize_blockset_endpoints(mode,bc7_blocks,activity_blocks,infos,flags,set_indexes,set_size,bits_mask,&multi_storage);
			
			if ( new_bits != bits_before )
			{			
				// get D before :
				F32 D_before = bc7rd_final_optimize_blockset_D(bc7_blocks,activity_blocks,infos,flags,set_indexes,set_size,
									zero_bits,zero_bits);
			
				// check if it's good :
				F32 D_after = bc7rd_final_optimize_blockset_D(bc7_blocks,activity_blocks,infos,flags,set_indexes,set_size,
									new_bits,bits_mask);
				
				// pretty common to have D_before == D_after due to degeneracies
				
				if ( D_after < D_before )
				{
					// yes it's good
					// apply it :
					for LOOP(set_i,set_size)
					{
						int bi = set_indexes[set_i].count;
						U8 * block_ptr = BlockSurface_Seek(bc7_blocks,bi);		
						bc7bits orig_bits = bc7bits_load(block_ptr);
						bc7bits bits = bc7bits_andnot(orig_bits,bits_mask);
						bits = bc7bits_or(bits,new_bits);
						bc7bits_store(block_ptr,bits);
						if (record_candidates_enabled)
							record_candidate_block_compute_D_unknown_R(lambda, do_indices ? 21 : 20, bc7_blocks, infos, bi, flags, *(const ActivityBlock4x4*) BlockSurface_SeekC(activity_blocks,bi), begin);

					#ifdef CHECK_EXTREMES_PRESERVED
						RAD_ALIGN(U8, decomp_block[64], BC7_BLOCK_ALIGN);
						bc7_decode_block4x4a(decomp_block,block_ptr,infos[bi].flags);

						BC7Constraints dec_constraints = BC7Constraints::from_pixels(decomp_block);
						if (!dec_constraints.superset_of(infos[bi].constraints))
						{
							BC7BlockState st, orig_st;

							bc7enc_state_from_bits(&st,block_ptr,infos[bi].flags); // need to use infos[bi].flags of that block when decoding to get correct endpoints_q
							bc7enc_state_from_bits(&orig_st,bc7bits_U8ptr(&orig_bits),infos[bi].flags); // need to use infos[bi].flags of that block when decoding to get correct endpoints_q
							U32 cmask = infos[bi].constraints.mask;

							rrprintf("final opt extremes mismatch! do_indices=%d mode=%d isb=%d rb=%d constraint mask=0x%08x\n", do_indices, st.mode, st.subsets[0].isbit, st.subsets[0].rbit, cmask);
							rrprintf("subset0 alpha: %02x-%02x orig: %02x-%02x\n",
								 st.subsets[0].endpoints[0].a, st.subsets[0].endpoints[1].a,
								 orig_st.subsets[0].endpoints[0].a, orig_st.subsets[0].endpoints[1].a);
							rrprintf("preserve: a0=%d a255=%d\n", (infos[bi].flags & BC7ENC_PRESERVE_A0) != 0, (infos[bi].flags & BC7ENC_PRESERVE_A255) != 0);

							const int which_ind = (st.mode < 4 || st.mode > 5 || st.subsets[0].isbit) ? 0 : 1;
							const BC7Inds& aind = st.subsets[0].idxs[which_ind];
							const BC7Inds& orig_aind = orig_st.subsets[0].idxs[which_ind];

							for LOOP(i,16)
							{
								rrprintf("[%2d] orig=%02x dec=%02x ind=%x orig_ind=%x %c%c\n", i, infos[bi].colors.colors[i].u.a, decomp_block[i*4+3], aind.ind[i],
									orig_aind.ind[i], " -"[(cmask >> i) & 1], " +"[(cmask >> (16 + i)) & 1]);
							}

							RR_ASSERT_ALWAYS(!"mismatch");
						}
					#endif
					}
				}
			}
			
			// step past the set :
			begin = end;
		}
	}
}

//===================================================================

template <>
inline hash_type hash_function<bc7bits>(const bc7bits & t)
{
	// @@ ? faster 128 bit hash function ?
	U64 qw0 = bc7bits_qw0(t);
	U64 qw1 = bc7bits_qw1(t);
	U64 h = rrRand64MungeSimple(qw0) + qw1;
	h = rrRand64MungeSimple( h );
	return (U32)(h>>32);
}

typedef hash_table<bc7bits,F32,hash_table_ops_mask31hash<bc7bits> > t_hash_bc7bits_to_F32;

struct bc7rd_static_rater_portion
{
	t_hash_bc7bits_to_F32 m_hash;
	F32 m_escape_code_len;
	
	F32 get_rate(const bc7bits & bits) const
	{
		t_hash_bc7bits_to_F32::entry_ptrc ep = m_hash.find(bits);
		if ( ep )
			return ep->data();
		else
			return m_escape_code_len;
	}
	
	void init(
		const vector<bc7bits_and_count> & vc, 
		int portion_bits, int mode_block_count );
};

struct bc7rd_static_rater_onemode
{
	int m_mode;

	bc7rd_static_rater_portion m_end_pb_rate;
	bc7rd_static_rater_portion m_ind_rate;
	
	bc7rd_static_rater_portion m_end_rate;
	bc7rd_static_rater_portion m_ind_pb_rate;
	
	F32 get_block_rate_sub(const bc7bits & bits,
		const bc7bits & mask_ind_part,
		const bc7rd_static_rater_portion & rater_ind_part,
		const bc7rd_static_rater_portion & rater_end_part) const
	{
		bc7bits bits_ind_part = bc7bits_and(   bits,mask_ind_part);
		bc7bits bits_end_part = bc7bits_andnot(bits,mask_ind_part);
		F32 ret;
		ret  = rater_ind_part.get_rate(bits_ind_part);
		ret += rater_end_part.get_rate(bits_end_part);
		return ret;
	}
	
	F32 get_block_rate(const bc7bits & bits) const
	{
		// EP+I or E+PI
		
		RR_ASSERT( m_mode == bc7bits_get_mode(bits) );
		
		bc7bits mask_ind(    c_bc7bitrange_indices_mask[m_mode] );
		bc7bits mask_ind_pb( c_bc7bitrange_indices_with_pbits_mask[m_mode] );
		
		F32 r_ep_i = 
			get_block_rate_sub( bits, mask_ind, 
				m_ind_rate, m_end_pb_rate );
				
		F32 r_e_pi = 
			get_block_rate_sub( bits, mask_ind_pb,
				m_ind_pb_rate, m_end_rate );
				
		// which one to use?
		// @@ ??
		// MIN ?
		
		F32 r = RR_MIN(r_ep_i,r_e_pi);
		return r;
	}
};

struct bc7rd_static_rater
{
	// first send a mode selection
	//	then the block within the mode
	F32	m_mode_rates[BC7_NUM_MODES];
	bc7rd_static_rater_onemode	m_raters[BC7_NUM_MODES];

	F32 get_block_rate(const bc7bits & bits) const
	{
		int mode = bc7bits_get_mode(bits);
		F32 ret = m_mode_rates[mode];		
		ret += m_raters[mode].get_block_rate(bits);
		return ret;
	}
	
	void init(const BlockSurface * bc7_blocks);
};

static int bc7bits_vectors_max_size(vector<bc7bits> * v_indices,int v_count)
{
	int max_size = 0;
	for LOOP(cat,v_count)
		max_size = RR_MAX(max_size, v_indices[cat].size32());
	return max_size;
}

void bc7rd_static_rater_portion::init(
	const vector<bc7bits_and_count> & vc, 
	int portion_bits, int mode_block_count )
{
	//m_bit_count = portion_bits;

	if ( mode_block_count == 0 )
	{
		// degenerate, no blocks
		//	always escape :
		m_escape_code_len = (F32)portion_bits;
		return;
	}
	
	// sum of all counts in vc should == mode_block_count
	// P = vc.count / mode_block_count
	// use singletons to estimate escape frequency
	
	int non_singleton_count = 0;
	while( non_singleton_count < vc.size32() && vc[non_singleton_count].count > 1 )
		non_singleton_count++; 
	
	if ( non_singleton_count == 0 )
	{
		// degenerate, all singletons
		//	always escape :
		m_escape_code_len = (F32)portion_bits;
		return;
	}
	
	int singleton_count = vc.size32() - non_singleton_count;
	
	int probability_denom = mode_block_count + 1;
	probability_denom += non_singleton_count;
	F32 log2_denom = rrlog2neg_bk_32(probability_denom);
	
	int non_singleton_tot = 0;
	
	for LOOP(vci,non_singleton_count)
	{
		int c = vc[vci].count;
		RR_ASSERT( c > 1 );
		
		//non_singleton_tot += c;
		non_singleton_tot += c+1;
		
		// use c+1 instead of c to measure rate *if* we added our block to this bucket
		F32 rate = rrlog2neg_bk_32(c + 1) - log2_denom;
		
		rate += portion_bits / (F32)c;
		
		bc7bits val = vc[vci].val;
		
		RR_ASSERT( m_hash.find(val) == NULL );
		m_hash.insert(val,rate); 
	}
			
	RR_ASSERT( non_singleton_tot < probability_denom );
	
	// @@ ??
	// BC1RD uses escape_count = 1 for escape rating
	//	   which grossly over-estimated escape rate
	//	  which encourages more quantization
	//	here I used the number of singletons
	
	int escape_count = probability_denom - non_singleton_tot;
	RR_ASSERT( escape_count == singleton_count + 1 );
	RR_UNUSED_VARIABLE( singleton_count );
	
	//escape_count = 1;
		
	F32 escape_rate = rrlog2neg_bk_32(escape_count) - log2_denom;
	escape_rate += portion_bits;
	//escape_rate += 8.f; // @@!! extra bit count for escapes to discourage them?
		// the idea is to encourage the J selector to prefer non-escape blocks
		//	doesn't seem to make a difference
	
	#if 1
	// I think this is never hit ?
	//	pretty sure it's impossible
	//	portion_bits has a min of 29 (IB in mode 2)
	RR_ASSERT( portion_bits >= 29 );
	// so the difference in payload is >= 14.5
	// if escape count is near 100%, the rate to signal an escape is near zero
	// # of bc7 blocks in a 256 KB chunk is 16k
	// due to slicing, mode_block_count has a max of 4k
	//	@@ slicing not working like I thought, currently 8k
	RR_ASSERT( mode_block_count <= 8192 );
	// therefore rate to select one of those is <= 12 bits
	// therefore payload bits difference always matters more than selection
	//F32 two_count_rate = rrlog2neg_bk_32(2+1) - log2_denom;
	// even more conservative without the +1 bias and still true :

	/*
	F32 two_count_rate = rrlog2neg_bk_32(2) - log2_denom;
	two_count_rate += portion_bits/2;
	RR_ASSERT( escape_rate >= two_count_rate + 0.1f );
	// escapes must be worse than two-counts !
	//escape_rate = RR_MAX(escape_rate,two_count_rate + 0.1f);
	*/
	#endif
	
	m_escape_code_len = escape_rate;
}

void bc7rd_static_rater::init(const BlockSurface * bc7_blocks)
{
	int num_blocks = check_value_cast<int>(bc7_blocks->count);
	RR_ASSERT_ALWAYS(num_blocks < (1u << 24));

	vector<U32> mode_and_bi; // (mode << 24) | bi
	int mode_count[BC7_NUM_MODES + 1] = {};
	int max_mode_block_count = 0;

	mode_and_bi.reserve(num_blocks);

	for LOOP(bi,num_blocks)
	{
		const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);
		int mode = rrCtz32(block_ptr[0] | 0x100); // invalid blocks count as mode 8 (which then gets ignored)
		mode_and_bi.push_back((mode << 24) | U32(bi));
		mode_count[mode]++;
	}

	// after sorting, we have the list of bi's for every mode
	stdsort(mode_and_bi.begin(), mode_and_bi.end());

	for LOOPINT(mode, BC7_NUM_MODES)
		max_mode_block_count = RR_MAX(max_mode_block_count, mode_count[mode]);

	vector<bc7bits> input_bits;
	vector<bc7bits_and_count> bits_and_counts;

	input_bits.reserve( max_mode_block_count );
	bits_and_counts.reserve( max_mode_block_count );

	const U32 * mode_and_bi_current = mode_and_bi.data();

	for LOOPINT(mode,BC7_NUM_MODES)
	{
		// size of all the v_ should be the # of blocks in that mode
		int mode_block_count = mode_count[mode];
		RR_ASSERT( mode_block_count <= max_mode_block_count );

		// zero frequency problem for mode? just +1 bias
		int mode_histo_count = mode_block_count + 1;
		int mode_histo_tot = num_blocks + BC7_NUM_MODES; // + BC7_NUM_MODES for the +1 on each mode
		
		m_mode_rates[mode] = rrlog2neg_bk_32( mode_histo_count) - rrlog2neg_bk_32( mode_histo_tot );
		
		m_raters[mode].m_mode = mode;
	
		// sort_and_count_uniques clears the vecs
		//vc_indices.clear();

		const bc7bits mask_ind(    c_bc7bitrange_indices_mask[mode] );
		const bc7bits mask_ind_pb( c_bc7bitrange_indices_with_pbits_mask[mode] );
	
		int ind_bits  = c_bc7_total_bits_indices[mode];
		int pbit_bits = c_bc7_total_bits_pbits[mode];
		int mode_bits = mode+1;
		RR_ASSERT( c_bc7_total_bits_header[mode] >= mode_bits );
		int end_bits = 128 - ind_bits - pbit_bits - mode_bits;
		// end_bits is end + header , but not counting mode
		//	because we already sent mode using histogram model
		RR_ASSERT( end_bits >= c_bc7_total_bits_endpoints[mode] );

		// 4 different ways to carve up indices/endpoints with and without pbits, run them all
		struct RaterDesc
		{
			bc7rd_static_rater_portion* portion;
			bc7bits mask;
			int portion_bits;
		}
		raters[4] =
		{
			{ &m_raters[mode].m_ind_rate,		mask_ind,					ind_bits },
			{ &m_raters[mode].m_ind_pb_rate,	mask_ind_pb,				ind_bits + pbit_bits },
			{ &m_raters[mode].m_end_rate,		bc7bits_not(mask_ind_pb),	end_bits },
			{ &m_raters[mode].m_end_pb_rate,	bc7bits_not(mask_ind),		end_bits + pbit_bits },
		};

		for (RaterDesc& rater : raters)
		{
			// extract the relevant bits from the source blocks using the mask
			input_bits.clear();
			const bc7bits mask = rater.mask;
			for (int i = 0; i < mode_block_count; ++i)
			{
				U32 bi = mode_and_bi_current[i] & 0xffffffu;
				const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);
				bc7bits bits = bc7bits_load(block_ptr);
				input_bits.push_back(bc7bits_and(bits, mask));
			}

			sort_and_count_uniques(&bits_and_counts,input_bits);
			sort_bc7bits_and_count_compare_count_highest_first(&bits_and_counts);
			rater.portion->init( bits_and_counts, rater.portion_bits, mode_block_count );
		}

		// advance to next mode
		mode_and_bi_current += mode_block_count;
	}
}

//===================================================================
// final matrix

// final matrix allow cross-part or not
//	as of 02-13-2020 doesn't seem to matter much
//	either for speed or quality, it's pretty meh
//	but slight win for yes do cross-part

//bc7rd_recompute_rates_static_model
//	changes infos[].J
static void bc7rd_recompute_rates_static_model(BlockSurface * bc7_blocks,
	vector<bc7rd_blockinfo> & infos,
	F32 lambda)
{
	SIMPLEPROFILE_SCOPE_N(bc7rd_recompute_rates,bc7_blocks->count);

	bc7rd_static_rater rater;
	rater.init(bc7_blocks);
	
	//F32 one_over_lambda = 1.f / lambda;
	int nb = check_value_cast<int>(bc7_blocks->count);
	
	for LOOP(bi,nb)
	{
		const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);
		bc7bits bits = bc7bits_load(block_ptr);
		
		// re-rate block :
		F32 new_R = rater.get_block_rate(bits);
		
		// verify D is right?
		//	(need activity)
		//F32 check_D = bc7rd_D(bits,infos[bi].colors,flags,*pActivity);
					
		F32 J = infos[bi].J;
		F32 D = infos[bi].D;
		RR_ASSERT( J >= D );
		//F32 old_R = ( J - D ) * one_over_lambda;
		// eyeballing old_R and new_R , they are often ballpack similar
		
		// ? trust old rate at all ? or just replace it?
		//	could hedge and average the two?		
		// just taking new R seems to be best, and it's the cleanest, so go with that
		//F32 R = RR_MIN(new_R,old_R);
		//F32 R = (new_R + old_R)*0.5f;
		F32 R = new_R;
		
		J = D + lambda * R;
		infos[bi].J = J;
	}
}
		
static U32 bc7bits_and_count_index_or_zero(const vector<bc7bits_and_count> & vec,int i)
{
	if ( vec.empty() ) return 0;
	i = RR_CLAMP(i,0,vec.size32()-1);
	return vec[i].count;
}
	
struct bc7_final_state
{
	F32 cur_J;
	F32 cur_D;
	U32 must_beat_sad;
	F32 max_rate;
	F32 best_possible_D;
};

template<typename T>
static T sqr(T x)
{
	return x * x;
}

static void bc7rd_final_matrix(
	CpuDispatchFlags dispatch,
	BlockSurface * bc7_blocks,
	const BlockSurface * activity_blocks,
	vector<bc7rd_blockinfo> & infos,
	F32 lambda,
	BC7Flags flags,
	const BC7_RD_Config & config)
{
	SIMPLEPROFILE_SCOPE_N(final_matrix, bc7_blocks->count);
		
	F32 one_over_lambda = 1.f / lambda;
	int nb = check_value_cast<int>(bc7_blocks->count);
	
	vector<bc7bits_and_count> combined;
	combined.reserve(nb);
	
	{
	//SIMPLEPROFILE_SCOPE(final_matrix_make);
	
	// you can do {I|EP} or {IP|E}
	// just doing one or the other does seem to be okay
	// and I|EP seems better than IP|E
	// mainly on red_blue
	// there's not a big difference
	
	vector<U32> mode_and_bi; // (mode << 24) | bi
	int mode_count[BC7_NUM_MODES + 1] = {};

	RR_ASSERT_ALWAYS(nb < (1 << 24));
	mode_and_bi.reserve(nb);

	for LOOP(bi,nb)
	{
		const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);	
		bc7bits bits = bc7bits_load(block_ptr);
		int mode = bc7bits_get_mode(bits);
		mode_and_bi.push_back(U32(mode << 24) | U32(bi));
		mode_count[mode]++;
	}

	// after sorting, we have the list of bi's for every mode
	stdsort(mode_and_bi.begin(), mode_and_bi.end());

	// find which mode has the max block count
	int max_mode_block_count = 0;
	//rrprintf("\n{ ");
	for LOOPINT(mode, BC7_NUM_MODES)
	{
		int count = mode_count[mode];
		//if ( count > 20 ) rrprintf("%d (%.1f%%),",count,100.0*count/nb);
		max_mode_block_count = RR_MAX(max_mode_block_count, count);
	}
	//rrprintf("}\n");

	vector<bc7bits> v_indices;
	vector<bc7bits> v_endpoints_pb;
	vector<bc7bits_and_count> vc_indices;
	vector<bc7bits_and_count> vc_endpoints_pb;
	
	v_indices.reserve( max_mode_block_count );
	v_endpoints_pb.reserve( max_mode_block_count );
	vc_indices.reserve( max_mode_block_count );
	vc_endpoints_pb.reserve( max_mode_block_count );

	for (SINTa i = 0; i < mode_and_bi.sizea(); )
	{
		// grab all blocks with the same mode
		int mode = mode_and_bi[i] >> 24;

		v_indices.clear();
		v_endpoints_pb.clear();

		while (i < mode_and_bi.sizea() && int(mode_and_bi[i] >> 24) == mode)
		{
			U32 bi = mode_and_bi[i] & ((1u << 24) - 1);
			const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);	
			bc7bits bits = bc7bits_load(block_ptr);

			v_indices.push_back( bc7bits_extract_indices(bits,false) );
			v_endpoints_pb.push_back( bc7bits_xor_assert_on(bits, v_indices.back() ) );
			++i;
		}

		// process them!
		sort_and_count_uniques(&vc_indices,v_indices);
		sort_bc7bits_and_count_compare_count_highest_first(&vc_indices);
		
		sort_and_count_uniques(&vc_endpoints_pb,v_endpoints_pb);
		sort_bc7bits_and_count_compare_count_highest_first(&vc_endpoints_pb);
		
		//make combined bits and sort by product count
		//that's the same as rating by codelen, -log2(c1)-log2(c2)

		// product count bias?
		// doesn't matter much
		//#define PRODUCT_COUNT(c1,c2)   c1*c2
		//#define PRODUCT_COUNT(c1,c2)   (c1+1)*(c2+1) // this is worst
		//#define PRODUCT_COUNT(c1,c2)   rr_froundint((c1+0.5f)*(c2+0.5f))
		#define PRODUCT_COUNT(c1,c2)   (2*c1+1)*(2*c2+1)
		//#define PRODUCT_COUNT(c1,c2)   (3*c1+1)*(3*c2+1)
		
		// guess a product count that we expect will limit us to the desired combined[] FINAL_MATRIX_SIZE_LIMIT
		//	so that we don't make combined[] too big leading into _make2
		// using FINAL_MATRIX_SIZE_LIMIT_SQRT would be right except that we do this 8* for the modes
		//#define CUTOFF_PRODUCT_INDEX	FINAL_MATRIX_SIZE_LIMIT_SQRT
		//#define CUTOFF_PRODUCT_INDEX	60 // this is a nop
		//#define CUTOFF_PRODUCT_INDEX	50 // this is a tiny penalty
		#define CUTOFF_PRODUCT_INDEX	55
		
		int cutoff_product = PRODUCT_COUNT( \
			bc7bits_and_count_index_or_zero(vc_indices,CUTOFF_PRODUCT_INDEX) , \
			bc7bits_and_count_index_or_zero(vc_endpoints_pb,CUTOFF_PRODUCT_INDEX) );
		
		//rrprintfvar(cutoff_product);
				
		RR_NOP();
						
		for LOOPVEC(vc_indices_i,vc_indices)
		{
			if ( vc_indices[vc_indices_i].count <= 1 ) break;
			
			for LOOPVEC(vc_endpoints_pb_i,vc_endpoints_pb)
			{
				if ( vc_endpoints_pb[vc_endpoints_pb_i].count <= 1 ) break;
				
				int c1 = vc_indices[vc_indices_i].count;
				int c2 = vc_endpoints_pb[vc_endpoints_pb_i].count;
				
				int product = PRODUCT_COUNT(c1,c2);
				if ( product <= cutoff_product ) break;

				bc7bits combined_bits = bc7bits_or_assert_exclusive( vc_indices[vc_indices_i].val , vc_endpoints_pb[vc_endpoints_pb_i].val );

				combined.emplace_back(bc7bits_and_count { combined_bits, (U32) product });
			}
		}
	}
	
	} // final_matrix_make
	
	if ( combined.empty() )
		return;
	
	{
	//SIMPLEPROFILE_SCOPE(final_matrix_make2);
	
	// at this point, combined is much bigger than target final matrix size
	//combined.size() : 606095  <- before CUTOFF_PRODUCT
	//rrprintfvar(combined.size());
	
	#if 0
	// no longer doing both; not necessary?
	
	// combined has redundant entries
	//		{EP}{I} and {E}{PI} were both added, so EPI is in twice
	//	need to remove those, but NOT sum the counts
	sort_bc7bits_and_count_compare_bc7bits(&combined);
	
	// condense to uniquify values, higher counts first :
	int toi=1;
	for LOOPVEC(fmi,combined)
	{
		if ( combined[fmi].val == combined[toi-1].val )
		{
			RR_ASSERT( combined[toi-1].count >= combined[fmi].count );
			continue;
		}
		
		combined[toi] = combined[fmi];
		toi++;
	}
	// not quite
	// every original block can be made two ways
	// but the new creations are singletons
	//RR_ASSERT( toi <= combined.size()/2 );
	
	rrprintfvar(toi);
	rrprintfvar(combined.size());
	
	combined.resize(toi);
	#endif
	
	// sort combined by count 
	sort_bc7bits_and_count_compare_count_highest_first(&combined);
	
	}
	
	// combined is now the matrix
	// limit its size :
		
	int matrix_size = RR_MIN( combined.size32() , sqr(config.fm_combined_size_limit_sqrt) );
	combined.resize( matrix_size );
	
	//rrprintfvar(matrix_size);
	// porsche640 three slices :
	// one slice gets almost no matrix at all
	//	the other two hit the limit
	//matrix_size : 41
	//matrix_size : 10000
	//matrix_size : 10000
	
	// decode everything in combined
	
	vector_aligned<rrColorBlock4x4, 64> matrix_decoded;
	matrix_decoded.resize( matrix_size );
	
	vector<F32> matrix_rate;
	matrix_rate.resize( matrix_size );
	
	//F32 log2_denom = rrlog2neg_bk_32(nb*nb);
	F32 log2_denom = rrlog2neg_bk_32( PRODUCT_COUNT(nb,nb) );
	
	//const F32 constant_rate = 31.f;
	
	{
	//SIMPLEPROFILE_SCOPE(final_matrix_decodes);
	
	for LOOPINT(matrix_i,matrix_size)
	{
		// bits should be unique and count in decreasing order :
		RR_ASSERT( matrix_i == 0 || combined[matrix_i].val != combined[matrix_i-1].val );
		RR_ASSERT( matrix_i == 0 || combined[matrix_i].count <= combined[matrix_i-1].count );
		
		bc7_decode_block4x4a((U8 *)matrix_decoded[matrix_i].colors,bc7bits_U8ptr(&combined[matrix_i].val),flags);
		
		// make Rate for final_matrix selection
		
		// @@ rate model here is counting the bits to *select* the entry
		//	but not the bits to send the base indices of the entry
		//	which should be (index bits)/N
		
		// rate of two matches in constant rate model currently is a flat 32 bits		
		// make this sightly better than the in-loop matrix rate
		//matrix_rate[matrix_i] = constant_rate;
		
		//*
		// or log2 of product count ?
		// NOTE this is affected by product count bias above
		U32 product_count = combined[matrix_i].count;
		F32 R = rrlog2neg_bk_32(product_count) - log2_denom;

		// @@??
		// hacky way to add on the cost of the payload
		//	(should be like 1/index_count + 1/endpoint_count)
		//	we don't have those anymore, just product count
		// -> seems to be a nop for quality
		//	 makes rates a bit higher which lets us early out better, but no measurable speed benefit either
		// -> meh
		R += (PRODUCT_COUNT(1,1)*128.f)/product_count;
		
		//rrprintfvar(R); // 20ish
		RR_ASSERT( R > 0.0 );

	    // @@??
		// truncate the matrix when we hit some rate?
		// at 28 it's nearly a nop
		//	at 24 it starts to hurt quality a bit
		//	 but we're not getting a good speedup here
		//	 in the slow cases, max rate is only 22 or so		
		// -> meh
		//if ( R >= 24.f && matrix_i > 1000 )
		if ( R >= 28.f && matrix_i > 1000 )
		{
			// truncate!
			matrix_size = matrix_i;
			combined.resize( matrix_size );
			matrix_decoded.resize( matrix_size );
			matrix_rate.resize( matrix_size );
			break;
		}
		
		// optimal rate bias seems to be zero :
		//matrix_rate[matrix_i] = (F32)(1 + R);
		matrix_rate[matrix_i] = (F32)R;
		/**/
		
		// rate should be strictly increasing :
		RR_ASSERT( matrix_i == 0 || matrix_rate[matrix_i] >= matrix_rate[matrix_i-1] );
	}
	
	} // final_matrix_decodes
	
	/*
	// quite often you get the full matrix size and all the rates are quite low
	//matrix_size : 20000
	//matrix_rate[0] : 14.117306
	//matrix_rate[matrix_size-1] : 21.358315

	rrprintfvar(matrix_size);
	rrprintfvar(matrix_rate[0]);
	rrprintfvar(matrix_rate[matrix_size-1]);
	/**/
	
	/*
	// force median matrix rate to constant_rate ? :
	//	slightly better without this
	F32 median_rate = matrix_rate[matrix_size/2];
	for LOOPVEC(matrix_i,matrix_rate)
	{
		F32 R = matrix_rate[matrix_i];
		
		R += (constant_rate - median_rate);
		// defo worse :
		//R = constant_rate + 0.5f * (R - median_rate);
		
		R = RR_CLAMP( R, 16.f, 48.f );
		//rrprintfvar(R);
		matrix_rate[matrix_i] = R;
	}
	/**/
		
	// for each block :
	{
	SIMPLEPROFILE_SCOPE_N(final_matrix_core,nb);

	vector<bc7_final_state> final_st;
	final_st.resize(nb);

	// initialize final_st for every block
	// (spilled state for the current best for every block)
	for LOOP(bi,nb)
	{
		// get J of what's currently encoded
		F32 cur_J = infos[bi].J;
		F32 cur_D = infos[bi].D;
		
		if ( cur_J == 0.f ) // signal to not try to improve me (single color blocks)
			continue;
		
		U8 * block_ptr = BlockSurface_Seek(bc7_blocks,bi);
	
		RR_DURING_ASSERT( const ActivityBlock4x4 * pActivity = BlockSurface_SeekC_ActivityBlock4x4(activity_blocks,bi) );
				
		// colors are RGBA :
		const rrColorBlock4x4 & colors = infos[bi].colors;
				
		// get SAD of what's currently encoded
		RAD_ALIGN(U8, cur_decoded[64], BC7_BLOCK_ALIGN);
		bc7_decode_block4x4a(cur_decoded,block_ptr,flags);
		rrColorBlock4x4 & cur_colors = *((rrColorBlock4x4 *)cur_decoded);
		
		RR_DURING_ASSERT( F32 check_D = VQD(cur_colors,colors,*pActivity) );
		RR_ASSERT( fequal(cur_D,check_D,0.001f) );
		
		U32 cur_sad = ColorBlock4x4_ComputeSAD_RGBA(colors,cur_colors);
		
		// make max_rate for early break like BC1
		//		(only if rate model is not flat)
		RR_ASSERT( cur_J >= cur_D );
		// this is of course not the best possible D, we don't know what that is
		//	we just assume this is the best we will be able to find here for early outing
		F32 best_possible_D = RR_MIN(cur_D,infos[bi].baseline_D);
		// D + lambda * max_rate must beat cur_J
		F32 max_rate = (cur_J - best_possible_D) * one_over_lambda;
		// this initial max_rate is not very helpful
		//	it's usually 80 bits or so, sometimes 128 bits
		//	it's typically just the rate that came out of the first pass
		//	which is much higher than typical rates here
		// but as we go we find lower J's and it gets tighter
		
		// not very sensitive :
		//	either 2 or 3 looks best
		//#define BC7_FINAL_MATRIX_SAD_INCREASE_TOLERANCE	3
		//#define BC7_FINAL_MATRIX_SAD_INCREASE_TOLERANCE	2
		#define BC7_FINAL_MATRIX_SAD_INCREASE_TOLERANCE	2.5f
			
		// start with a criterion that you must get SAD within some proximity of "cur"
		//	then after that it must be strictly decreasing
		U32 must_beat_sad = rr_froundint(cur_sad + lambda * BC7_FINAL_MATRIX_SAD_INCREASE_TOLERANCE);

		// 16*4*255 is the max any real SAD could ever be (16 pixels, 4 bytes/pixel).
		// clamp must_beat_sad there so we're staying well clear off any overflow thresholds
		// in the selection kernels below.
		must_beat_sad = RR_MIN(must_beat_sad, 16*4*255);

		final_st[bi].cur_J = cur_J;
		final_st[bi].cur_D = cur_D;
		final_st[bi].must_beat_sad = must_beat_sad;
		final_st[bi].max_rate = max_rate;
		final_st[bi].best_possible_D = best_possible_D;
	}

	// Process chunks of the full matrix that fit in L1 to minimize cache thrashing
	const int matrix_chunk_size = FinalMatrixL1Index::CHUNK_SIZE;
	FinalMatrixL1Index matrix_index;

	int chunk_sads[matrix_chunk_size + FIND_NEXT_BELOW_PADDING];
	for (int i = 0; i < matrix_chunk_size + FIND_NEXT_BELOW_PADDING; ++i)
		chunk_sads[i] = -1;

	rrColorBlock4x4 zero_colors = {}; // zero-init

	for (int matrix_chunk_begin = 0; matrix_chunk_begin < matrix_size; matrix_chunk_begin += matrix_chunk_size)
	{
		int matrix_chunk_end = RR_MIN(matrix_chunk_begin + matrix_chunk_size, matrix_size);
		int matrix_chunk_len = matrix_chunk_end - matrix_chunk_begin;

		matrix_index.build(&matrix_decoded[matrix_chunk_begin],matrix_chunk_len);

		for LOOP(bi,nb)
		{
			bc7_final_state * st = &final_st[bi];

			if ( st->cur_J == 0.f ) // signal to not try to improve me (single color blocks)
				continue;

			U8 * block_ptr = BlockSurface_Seek(bc7_blocks,bi);
			const ActivityBlock4x4 * pActivity = BlockSurface_SeekC_ActivityBlock4x4(activity_blocks,bi);

			// try everything in matrix
			// SAD first for early reject
			// then J from VQD

			// Candidates are ordered by ascending rate; check if everything in the chunk is above current
			// max in which case we can bail on it immediately.
			const F32 initial_max_rate = st->max_rate;
			if ( matrix_rate[matrix_chunk_begin] >= initial_max_rate )
				continue;

			// SAD(a,b) is 1-norm ||a-b||; because this is a norm, the triangle
			// inequality holds, and so does the reverse triangle inequality
			//
			//   abs(||a|| - ||b||) <= ||a - b||
			//
			// in short, to find blocks with SAD(a,b) <= t and for fixed b,
			// only need to look at blocks with L1 norms (=SAD against 0)
			// in [||b||-t, ||b||+t].
			//
			// In our case, must_beat_sad is exclusive (we test for < not <=),
			// so t = must_beat_sad + 1. When must_beat_sad is 0, we can just stop
			// immediately; we can't get errors lower than 0.
			const U16 * culled_inds;
			const int culled_len = matrix_index.lookup(infos[bi].colors,&culled_inds,st->must_beat_sad);
			RR_ASSERT( culled_len >= 0);
			if ( culled_len <= 0 )
				continue;

			if ( st->must_beat_sad == 0 )
				continue;

			// Determine constraint mask for target block
			// (only active in preserve extremes mode)
			BC7Constraints target_constraints = infos[bi].constraints;

			// Do one big dumb loop to compute all the SADs in this chunk before
			// we make any decisions
			compute_chunk_sads_indirect(dispatch, chunk_sads, culled_len, culled_inds, &matrix_decoded[matrix_chunk_begin], infos[bi].colors);

			// sentinel at the end for scan loop to guarantee we notice actual end; guaranteed to be less
			// than any must_beat_sad we might have (which is always going to be >=0)
			chunk_sads[culled_len] = -1;

			// copy into local var so compiler knows the loads can be hoisted
			// outside the inner loop
			RAD_ALIGN(const rrColorBlock4x4, colors, 16) = infos[bi].colors;

			for (int i = 0; ; i++) // yes, no loop condition!
			{
				// we allow i == culled_len here; we have the extra sentinels/padding at the end
				// to make sure we catch this.
				RR_ASSERT( i <= culled_len );
				i = find_next_below(chunk_sads, i, st->must_beat_sad + 1); // must_beat_sad + 1 because this loop scans for <=, not <
				if ( i >= culled_len )
					break;

				const int matrix_i = matrix_chunk_begin + culled_inds[i];

				U32 matrix_sad = chunk_sads[i];
				RR_ASSERT( matrix_sad <= st->must_beat_sad ); // that's what we scanned for before

				// If we're above the current max rate, no point in scoring.
				if ( matrix_rate[matrix_i] >= st->max_rate )
					continue;

				// If preserve extremes is active, need to check whether constraints are satisfied
				if ( !target_constraints.empty() )
				{
					BC7Constraints subst_constraints = BC7Constraints::from_pixels(matrix_decoded[matrix_i]);
					if ( !subst_constraints.superset_of(target_constraints) )
						continue;
				}

				st->must_beat_sad = matrix_sad;

				// SAD went down, evaluate this block :

				F32 matrix_D = VQD(colors, matrix_decoded[matrix_i], *pActivity);
				F32 matrix_J = bc7rd_J(matrix_D, matrix_rate[matrix_i], lambda);

				if ( matrix_J < st->cur_J )
				{
					st->cur_J = matrix_J;
					st->cur_D = matrix_D;
					// copy in the encoding :
					memcpy(block_ptr, &combined[matrix_i].val, sizeof(bc7bits));

					if ( matrix_sad == 0 )
						break;

					// as we find better solutions, max_rate comes down :
					st->max_rate = (st->cur_J - st->best_possible_D) * one_over_lambda;
				}
			}

			infos[bi].J = st->cur_J;
			infos[bi].D = st->cur_D;
		}
	}

	} // profile scope
}

//================================================================
// index merge

#define LARGE_D_F32	((F32)(1<<29))

struct bc7rd_index_vq_entry
{
	bc7bits indices;

	// This tracks the index constraints we had on the original blocks,
	// which are satisfied by this entry, else we wouldn't have kept it.
	//
	// For legal merges, we currently only consider merging entries when
	// the "to" entry has a superset of our active index constraints, with
	// the expectation that the main use case is merging unconstrained into
	// constrained clusters.
	//
	// BC4RD has more precise logic where we separately track the constraints
	// actually required by the source data and the constraints that happen to
	// be satisfied by the current indices, and except merges whenever the
	// union of pixel constraints is satisfiable by the index constraints.
	//
	// This is not hard to add here (I did an implementation that kept a second
	// constraint set "ind_satisfied[2]" to keep track of constraints that an
	// index block can satisfy, and then updated is_legal_merge and the cluster
	// merge logic) but needs a bit of extra memory and processing during
	// initialization, and while it does improve the results, the gains are _tiny_.
	// Therefore, we're doing the simple logic for now; I don't want to make this
	// any more complicated than it already is without evidence that doing so
	// actually has a tangible effect on the results.
	BC7Constraints ind_constraints[2]; // Index constraints

	F32 distortion_sum = 0;
	U32 count = 0;
	U32 count_log2_count = 0;
	S32 block_link = -1; // linked list of blocks with these indices, -1-terminated
	S32 merged_onto = -1; // negative if not merged

	bc7rd_index_vq_entry(const bc7bits& inds)
		: indices(inds)
	{
		ind_constraints[0] = BC7Constraints::none();
		ind_constraints[1] = BC7Constraints::none();
	}
};

// compare first highest count first :
static bool operator < (const bc7rd_index_vq_entry &lhs, const bc7rd_index_vq_entry &rhs)
{
	return lhs.count > rhs.count;
}

struct bc7rd_index_vq_heap_entry
{
	int fm,to;
	F32 dj; // positive dj = good
	int fm_count_save;
	F32 fm_distortion_onto;
};

// normal operator less will give a heap that pops largest first :
static bool operator < (const bc7rd_index_vq_heap_entry &lhs, const bc7rd_index_vq_heap_entry &rhs)
{
	return lhs.dj < rhs.dj;
}

struct bc7rd_index_vq_block
{
	bc7bits encoding_without_indices;
	const ActivityBlock4x4 * activity;
	int link;
};

#ifdef RR_DO_ASSERTS
static bool bc7rd_assert_block_link_check(const bc7rd_index_vq_entry * entry, 
	const vector<bc7rd_index_vq_block> & blocks )
{
	// verify links :
	int link = entry->block_link;
	U32 link_count = 0;
	while( link >= 0 )
	{
		link_count++;
		link = blocks[link].link;
	}
	RR_ASSERT( link_count == entry->count );
	return true;
}
#endif // RR_DO_ASSERTS


static F32 bc7rd_single_block_index_change_distortion_VQD(int bi,
	const vector<bc7rd_index_vq_block> & blocks,
	const vector<bc7rd_blockinfo> & infos,
	const bc7bits & new_indices,
	BC7Flags flags,
	BC7BlockDecoder * const decoder)
{
	bc7bits encoding = bc7bits_or_assert_exclusive( blocks[bi].encoding_without_indices, new_indices );
	alignas(BC7_BLOCK_ALIGN) U8 decoded[64];
	decoder(decoded, bc7bits_U8ptr(&encoding), (flags & BC7ENC_IGNORE_ALPHA) != 0);
	F32 D = bc7rd_D(decoded,infos[bi].colors,flags,*blocks[bi].activity);
	return D;
}

static F32 bc7rd_single_block_index_change_distortion_SSD(int bi,
	const vector<bc7rd_index_vq_block> & blocks,
	const vector<bc7rd_blockinfo> & infos,
	const bc7bits & new_indices,
	BC7Flags flags,
	BC7BlockDecoder * const decoder)
{
	bc7bits encoding = bc7bits_or_assert_exclusive( blocks[bi].encoding_without_indices, new_indices );
	alignas(BC7_BLOCK_ALIGN) U8 decoded[64];
	decoder(decoded, bc7bits_U8ptr(&encoding), (flags & BC7ENC_IGNORE_ALPHA) != 0);
	
	// RGBA vs BGRA order doesn't matter, it's 4-channel difference :
	RR_COMPILER_ASSERT( sizeof(rrColorBlock4x4) == 64 );
	const rrColorBlock4x4 * pc1 = (const rrColorBlock4x4 *)decoded;
	const rrColorBlock4x4 * pc2 = &(infos[bi].colors);
	
	U32 ssd = ColorBlock4x4_ComputeSSD_RGBA(*pc1,*pc2);
	
	// VQD ~ 2*SSD
	F32 D = ssd * 2.f;
	
	return D;
}

/*
#define bc7rd_single_block_index_change_distortion_is_vqd
#define bc7rd_single_block_index_change_distortion bc7rd_single_block_index_change_distortion_VQD
/*/
#define bc7rd_single_block_index_change_distortion bc7rd_single_block_index_change_distortion_SSD
/**/

// walk block link starting at bi
// change indices to "new_indices"
// measure total distortion
static F32 bc7rd_block_link_distortion(int bi,
	const vector<bc7rd_index_vq_block> & blocks,
	const vector<bc7rd_blockinfo> & infos,
	const bc7bits & new_indices,
	BC7Flags flags,
	int mode,
	F32 must_beat_D = LARGE_D_F32)
{
	F32 D_sum = 0.f;
	BC7BlockDecoder * const decoder = bc7_lookup_block_decoder(mode);
		
	while( bi >= 0 )
	{
		F32 D = bc7rd_single_block_index_change_distortion(bi,blocks,infos,new_indices,flags,decoder);
		D_sum += D;
		
		if ( D_sum > must_beat_D )
		{
			// early out when must_beat_D is hit 
			// this is hit 99% of the time
			// it's a great speedup for long block lists
			//	but doesn't help at all with singletons
			return LARGE_D_F32;
		}
		
		bi = blocks[bi].link;
	}
	
	return D_sum;
}

static F32 bc7rd_index_merge_dj( const bc7rd_index_vq_entry & from, const bc7rd_index_vq_entry & to, F32 D_diff , F32 lambda, int index_bits)
{
	// diff = distortions
	//	no need to multiply by count as we've already accumulated across the N blocks
	F32 D = D_diff;
	
	
	// bc1 32 bits -> bc7 "index_bits"
	// bc7rd rate is just bits, not "codelen"
	
	// each entry has to send 32 bit raw indices + count * selection of those indices	
	//	same rate model as vq_codelen_palette_count
	
	// when you do a merge, the main rate savings is the 32 bits for one less index value
	//	but you also save some because the selection gets cheaper
	
	#if 0 // @@!! 
	
	// massively wrong simplified rate model
	// just count the 32 bits of raw index data saved
	// does not favor higher count targets like it should (their selection is cheaper)
	// while this is wrong I like that it favors low-count merges :
	
	//	subtract size of sending a match (delta from sending an escape)
	//	  ?? only if from or to count is 1 ?
	//	@@ this depends on lambda
	//		at very low lambda, escapes are almost free (nearly 100%)
	//			so matches are much more expensive than escapes
	//		at high lambda they may be nearly equal in cost
	F32 R = (F32)index_bits;
	//R -= 16.f;
	//if ( from.count == 1 && to.count == 1 ) R -= 18.f;
	//else if ( from.count == 1 || to.count == 1 ) R -= 14.f;
	
	#else
	
	// more accurate rate using count probability model
	
	// @@ REVISIT ME
	//	in bc7rd so far I see near zero benefit to the more accurate rate model
	//	simple (index_bits) rate gives the same results
	//	-> this does help at high lambda to encourage more merges
	//		it finds rate reductions in difficult images
	//		  where the flat model stalls out and can't make much progress
	
	// log2tabled returns values scaled up by RR_LOG2TABLE_ONE
	// the denominator terms (nblocks) cancel out
	U32 merged_count = from.count + to.count;
	
	// count*log2count is cached in index_vq_entry :
	U32 log2one_from = from.count_log2_count;
	U32 log2one_to = to.count_log2_count;
	
	RR_ASSERT( log2one_from == from.count * log2tabled_bk_32(from.count) );
	RR_ASSERT( log2one_to   == to.count * log2tabled_bk_32(to.count) );
	
	U32 log2one_merged   = merged_count * log2tabled_bk_32(merged_count);
	U32 log2one_delta = log2one_from + log2one_to - log2one_merged;
	
	F32 rate_delta = log2one_delta * (1.f / (F32)RR_LOG2TABLE_ONE);
	
	rate_delta += (F32)(index_bits);
	
	F32 R  = rate_delta;
	
	#endif
	
	// R and D are positive here
	//	D is the distortion cost of the merge, R is the rate savings
	//	(D can sometimes by slightly negative when an index change actually helps a block)
	
	F32 dJ = - D + lambda * R;
	// dJ > 0 is a good merge
	//	the rate savings is enough to justify the distortion penalty
	return dJ;
}

// Only allow merging less into more constrained blocks for now
static bool is_legal_merge(const bc7rd_index_vq_entry& fm, const bc7rd_index_vq_entry& to)
{
	return to.ind_constraints[0].superset_of(fm.ind_constraints[0]) &&
		to.ind_constraints[1].superset_of(fm.ind_constraints[1]);
}

static void make_heap_bottom_up_merge_fm_singletons(
	vector<bc7rd_index_vq_heap_entry> & heap,
	const vector<bc7rd_index_vq_entry> & entries,
	const vector<bc7rd_index_vq_block> & blocks,
	const vector<bc7rd_blockinfo> & infos,
	int mode,
	int first_singleton,
	F32 lambda,
	int index_bits_this_mode,
	F32 max_distortion_increase,
	BC7Flags flags)
{
	const int entries_size = entries.size32();
	const bool ignore_alpha = (flags & BC7ENC_IGNORE_ALPHA) != 0;

	// VQD ~ 2*SSD
	const F32 vqd_from_ssd = 2.0f;
	const F32 ssd_from_vqd = 1.0f / vqd_from_ssd;

	BC7PredecodedEndpointsBlock predecoded;
	RR_DURING_ASSERT( BC7BlockDecoder * const decoder = bc7_lookup_block_decoder(mode) );

	// Initialize the index cache
	predecoded.init_indices(mode, (const U8 *)&entries[0].indices,sizeof(entries[0]),entries.size());

	for (int fm = first_singleton; fm < entries_size; fm++)
	{
		F32 fm_base_distortion = entries[fm].distortion_sum;

		int fm_bi = entries[fm].block_link;
		RR_DURING_ASSERT( F32 check_D = bc7rd_single_block_index_change_distortion(fm_bi, blocks, infos, entries[fm].indices,flags,decoder ) );
		RR_ASSERT( fequal(check_D, fm_base_distortion, 0.1f) );

		// initializing with max_distortion_increase or not is meh
		//F32 best_distortion = LARGE_D_F32;
		F32 best_distortion = (F32)(fm_base_distortion + max_distortion_increase);
		U32 best_distortion_ssd = (U32)ceil(best_distortion * ssd_from_vqd); // conservative

		predecoded.init_endpoints(bc7bits_U8ptr(&blocks[fm_bi].encoding_without_indices),(const U8 *)&infos[fm_bi].colors,ignore_alpha);

		// NOTE: we set things up so our early reject threshold is on the SSD values which are integers,
		// which saves a bit of work on the critical path.

		for (int to = 0; to < entries_size; to++)
		{
			if ( fm == to ) continue;

			U32 ssd;

			// try to skip ahead to next candidate block with SSD <= best_distortion_ssd
			SINTa max_count = (to < fm) ? (fm - to) : entries_size - to;
			SINTa skipped = predecoded.find_next_at_most(&predecoded,to,max_count,&ssd,best_distortion_ssd);
			to += (int)skipped;
			if (skipped == max_count) // didn't find anything better in this run? OK, skip eval.
				continue;

			// entries are sorted by count so I only have to look at J when D gets better
			if ( ssd <= best_distortion_ssd )
			{
				F32 new_index_D = vqd_from_ssd * ssd;

				RR_ASSERT( new_index_D < LARGE_D_F32 );
				best_distortion = new_index_D;
				best_distortion_ssd = ssd;

				if ( is_legal_merge(entries[fm],entries[to]) )
				{
					// make D the delta :
					F32 delta_D = new_index_D - fm_base_distortion;

					F32 dj = bc7rd_index_merge_dj( entries[fm], entries[to], delta_D, lambda, index_bits_this_mode);
					if ( dj > 0 )
					{
						// make a heap entry :
						heap.push_back();
						heap.back().fm = fm;
						RR_ASSERT( entries[fm].count == 1 );
						heap.back().fm_count_save = 1;
						heap.back().fm_distortion_onto = new_index_D;
						heap.back().to = to;
						heap.back().dj = dj;
					}
				}
			}
		}
	}
}

static U32 bc7rd_total_ssd(BlockSurface * bc7_blocks,
	vector<bc7rd_blockinfo> & infos, BC7Flags flags)
{
	int nb = check_value_cast<int>(bc7_blocks->count);

	U32 tot = 0;

	for LOOP(bi,nb)
	{
		const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);
		bc7bits bits = bc7bits_load(block_ptr);

		RAD_ALIGN(U8, decomp_block[64], BC7_BLOCK_ALIGN);
		bc7_decode_block4x4a(decomp_block,bc7bits_U8ptr(&bits),flags);
	
		const rrColorBlock4x4 * pc1 = &infos[bi].colors;
		const rrColorBlock4x4 * pc2 = (const rrColorBlock4x4 *) decomp_block;
		
		U32 ssd = ColorBlock4x4_ComputeSSD_RGBA(*pc1,*pc2);
		
		tot += ssd;
	}		
	
	return tot;
}

static void print_analyzed_block(const char* label, const U8 bits[16])
{
	BC7BlockRawDesc desc;
	bc7_analyze_block_raw(&desc, bits);

	rrprintf("%s:\n", label);
	if (desc.mode)
		rrprintf("  mode=%d\n", desc.mode->mode);
	else
	{
		rrprintf("  (invalid)\n");
		return;
	}

	rrprintf("  partition=%d rot=%d ind_sel=%d\n", desc.partition_id, desc.rot, desc.ind_sel);

	for LOOP(i,desc.mode->ns)
	{
		for LOOP(c,4)
			rrprintf("  [%d.%c] %02x-%02x\n", i, "rgba"[c], desc.endpoints_raw[c][i*2+0], desc.endpoints_raw[c][i*2+1]);
	}

	for LOOP(j,(desc.mode->ib2 ? 2 : 1))
	{
		rrprintf("  inds%d=", j);
		for LOOP(i,16)
			rrprintf("%x", desc.inds[j][i]);
		rrprintf("\n");
	}
}

static RADNOINLINE void actually_shrink_vq_heap(vector<bc7rd_index_vq_heap_entry>& heap, SINTa target_limit)
{
	RR_ASSERT(target_limit <= (SINTa)heap.size());

	// The target sizes here are relatively large, do a full sort with a fast sorting alg instead
	// of trying to do a partial sort that would need to use a slower alg like heapsort.
	//
	// Sort to put the largest-score elements first.
	stdsort(heap.data(), heap.data() + heap.size(), stdgreater_via_less<bc7rd_index_vq_heap_entry>());
	heap.resize(target_limit);
}

// if heap is at maximum size, shrink it to be half that size
static inline void test_shrink_vq_heap(vector<bc7rd_index_vq_heap_entry>& heap, SINTa limit)
{
	if((SINTa)heap.size() < limit)
		return;

	actually_shrink_vq_heap(heap, limit/2);
}

// bc7rd_bottom_up_merge_indices
//	bottom-up N^2 merge of indices within each mode group
//	 does only {i -> j} , not {i,j} -> {best single index for i+j}
//	does not try to change endpoints, considers them fixed
static void bc7rd_bottom_up_merge_indices(BlockSurface * bc7_blocks,
	const BlockSurface * activity_blocks,
	vector<bc7rd_blockinfo> & infos,
	F32 lambda,
	BC7Flags flags)
{
	SIMPLEPROFILE_SCOPE_N(bc7rd_index_bum, bc7_blocks->count);
		
	int nb = check_value_cast<int>(bc7_blocks->count);

	vector<bc7bits_and_count> v_indices_pb[BC7_NUM_MODES];
	
	vector<bc7rd_index_vq_block> blocks;
	blocks.resize(nb);
	
	for LOOPINT(bi,nb)
	{
		const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);
		bc7bits bits = bc7bits_load(block_ptr);
		
		int mode = bc7bits_get_mode(bits);
		
		v_indices_pb[mode].push_back();

		// merge {I} sets
		//	 so they are most likely to form good matches
		//
		// we used to do {I+P} here but this doesn't work with our predecode logic, nor with
		// preserve extremes, and the difference is very minor, so screw it.
		v_indices_pb[mode].back().val = bc7bits_extract_indices(bits,false);
		v_indices_pb[mode].back().count = bi;
		
		blocks[bi].link = -1;
		blocks[bi].encoding_without_indices = bc7bits_xor_assert_on( bits , v_indices_pb[mode].back().val );
		
		const ActivityBlock4x4 * pActivity = BlockSurface_SeekC_ActivityBlock4x4(activity_blocks,bi);
				
		blocks[bi].activity = pActivity;
	}
		
	vector<bc7rd_index_vq_entry> entries;
	entries.reserve(nb);

	// Merge candidate heap limit. We still score all prospective candidates, but maintain a merge candidate
	// heap below that size. If we hit this limit, we toss half the existing candidates before we keep adding
	// more. (It needs to be a constant ratio of the full size to not risk introducing quadratic blow-up.)
	//
	// Note that the initial candidate scoring is already a O(N*M)-type operation. So far, even with
	// adversarial inputs, it doesn't seem to ever blow up too badly, but worth keeping in mind.
	const SINTa vq_heap_limit = RR_MAX(nb,64) * 12;

	vector<bc7rd_index_vq_heap_entry> heap;
	heap.reserve(vq_heap_limit);
		
	for LOOPINT(mode,BC7_NUM_MODES)
	{
		if ( v_indices_pb[mode].size() <= 1 ) continue;

		BC7BlockDecoder * const decoder = bc7_lookup_block_decoder(mode);
		
		sort_bc7bits_and_count_compare_bc7bits(&v_indices_pb[mode]);
	
		// find runs of same val
		
		entries.clear();
		
		const int vcount = v_indices_pb[mode].size32();
		for (int vbase = 0; vbase < vcount; )
		{
			// Start new index VQ entry
			bc7bits indices = v_indices_pb[mode][vbase].val;

			entries.emplace_back(indices);
			bc7rd_index_vq_entry & entry = entries.back();

			// Go over all entries in this run
			int vi = vbase;

			for (; vi < vcount && indices == v_indices_pb[mode][vi].val; ++vi)
			{
				const int bi = v_indices_pb[mode][vi].count;
				RR_ASSERT( blocks[bi].link == -1 );
				if ( infos[bi].J == 0.f ) // single color block, don't touch
					continue;

				// Check whether this block is valid for index merging and update index constraints.
				// We only merge within the same mode, and currently only towards more constrained
				// blocks (i.e. merged entries' constraints need to be a superset of the original
				// entry).
				//
				// If this block is not eligible for index merging, just skip it here and
				// pretend it doesn't exist for the purposes of this pass.
				const U8* block_ptr = BlockSurface_SeekC(bc7_blocks,bi);
				if ( !may_merge_indices(block_ptr,mode,infos[bi],entry.ind_constraints) )
					continue;

				// Compute distortion
				F32 D;
				#if bc7rd_single_block_index_change_distortion_is_vqd
				D = infos[bi].D;
				RR_ASSERT( fequal(D,bc7rd_single_block_index_change_distortion(bi,blocks,infos,indices),0.1f) );
				#else
				D = bc7rd_single_block_index_change_distortion(bi,blocks,infos,indices,flags,decoder);
				#endif

				// Link the block in
				blocks[bi].link = entry.block_link;
				entry.block_link = bi;

				entry.count ++;
				entry.distortion_sum += D;
			}

			// If we created an empty run, pop it
			if (!entry.count)
				entries.pop_back();
			else
			{
				// We have processed all blocks in this run, but might have ended up with infeasible index
				// constraints. If so, remove the entry and leave the blocks alone.
				if (!entry.ind_constraints[0].empty() || !entry.ind_constraints[1].empty())
				{
					// We can only have non-empty index constraints in modes 4 and 5 which are single-subset.
					// Therefore when we have non-trivial constraints, all blocks should be in the same
					// equivalence class.
					RR_ASSERT(mode == 4 || mode == 5);

					// We also need to decode the index values to see whether they satisfy our
					// index constraints to begin with, because if the endpoints are close enough, we might
					// be able to hit extremal values without using extreme indices.
					//
					// This automatically takes care of infeasible indices.
					BC7BlockState st;
					bc7enc_state_from_bits(&st,BlockSurface_SeekC(bc7_blocks,entry.block_link),flags);

					const BC7SubsetState& sst = st.subsets[0];
					BC7Constraints satisfiable0 = BC7Constraints::from_indices(sst.idxs[0], (mode == 4) ? 3 : 2);
					BC7Constraints satisfiable1 = BC7Constraints::from_indices(sst.idxs[1], 2);

					if (!satisfiable0.superset_of(entry.ind_constraints[0]) ||
						!satisfiable1.superset_of(entry.ind_constraints[1]))
					{
						// Bad run, pop it again
						entries.pop_back();
					}
				}
			}

			vbase = vi;
		}
		
		int num_entries = entries.size32();
		if ( num_entries < 2 )
			continue;
		
		// confirmed okay :
		//U32 total_ssd_before = bc7rd_total_ssd(bc7_blocks,infos);
		//F32 total_D_delta = 0;
	
		// update cached fields in entries :
		for LOOPVEC(ei,entries)
		{
			entries[ei].count_log2_count = entries[ei].count * log2tabled_bk_32( entries[ei].count );
		}
		
		int index_bits_this_mode = c_bc7_total_bits_pbits[mode] + c_bc7_total_bits_indices[mode];
		
		// D threshold for dj > 0 :		
		F32 max_distortion_increase = index_bits_this_mode*lambda;
		
		// lambda is ~ 40 * BC7RD_LAMBDA_SCALE = 12
		//	
		//F32 max_bit_change_count = 0.5f + lambda * 2.f;
		
		// sort entries by count so we go in rate-increasing order
		stdsort(entries.begin(),entries.end());
		RR_ASSERT( entries[0].count >= entries[1].count );		

		heap.clear();
		
		//===========================================
		// make heap for non-singletons :
		
		{
		int entries_size = entries.size32();
		int first_singleton = entries_size;

		{
		SIMPLEPROFILE_SCOPE_N(bum_groups_make,first_singleton);

		vector<F32> best_distortions;
		best_distortions.resize(first_singleton);
		
		// process chunks of the entries array that fit in L1 to minimize cache thrashing
		const int entry_chunk_size = 256;
		RR_COMPILER_ASSERT(entry_chunk_size * sizeof(entries[0]) <= 16 * 1024);

		for (int to_chunk_begin = 0; to_chunk_begin < entries_size; to_chunk_begin += entry_chunk_size)
		{
			const int to_chunk_end = RR_MIN(to_chunk_begin + entry_chunk_size, entries_size);
			int fm = 0;

			for (;fm<entries_size;fm++)
			{
				// FIRST LOOP OF SIMILAR PAIR
				// HEAP FOR NON-SINGLETON FM's :
				if ( entries[fm].count <= 1 ) break; // count sorted order so when we hit a 1 we break

				F32 fm_base_distortion = entries[fm].distortion_sum;

				RR_DURING_ASSERT( F32 check_D = bc7rd_block_link_distortion( entries[fm].block_link , blocks, infos, entries[fm].indices, flags, mode ) );
				RR_ASSERT( fequal(check_D, fm_base_distortion, 0.1f) );

				// only bother looking if within max_distortion_increase
				// initializing with max_distortion_increase or not is meh
				//F32 best_distortion = LARGE_D_F32;
				F32 & best_distortion = best_distortions[fm];

				// initialize in first pass
				if ( to_chunk_begin == 0 )
					best_distortion = (F32)(fm_base_distortion + max_distortion_increase);

				// entries are sorted by count (increasing rate)
				//	best_distortion decreases as we go
				// -> decreasing best_distortion is a big time saver and not bad for quality

				for (int to = to_chunk_begin; to < to_chunk_end; to++)
				{
					if ( fm == to ) continue;

					RR_ASSERT( entries[fm].indices != entries[to].indices );

					// [fm] is multiple blocks, possibly of different partitions (all same mode)
					RR_ASSERT( entries[fm].count > 1 );
					// no xor indices early out for groups

					// try changing fm sets' indices to "to" :
					F32 new_index_D = bc7rd_block_link_distortion( entries[fm].block_link , blocks, infos, entries[to].indices,flags, mode, best_distortion );

					// entries are sorted by count so I only have to look at J when D gets better
					//  (this is an approximation, tests indicate it's okay)
					//		(it's exact for the first merge step, but not later as things get merged up)
					//	(to be exact you should go ahead and add everything with dj > 0 to the heap)
					//	(if we only did one merge this would be fine, it would not be an approximation
					//	 the issue is that later on, all your desired merge targets may be gone
					//	 so the best thing left may be one of the ones that we ruled out here)
					if ( new_index_D <= best_distortion )
					{
						RR_ASSERT( new_index_D < LARGE_D_F32 );
						best_distortion = new_index_D;

						// do the legality check after the cheap early outs but before we set up a heap entry
						if ( is_legal_merge(entries[fm],entries[to]) )
						{
							// make D the delta :
							F32 delta_D = new_index_D - fm_base_distortion;

							F32 dj = bc7rd_index_merge_dj( entries[fm], entries[to], delta_D , lambda, index_bits_this_mode); //,nblocks);
							if ( dj > 0 )
							{
								// make a heap entry :
								test_shrink_vq_heap(heap, vq_heap_limit);

								heap.push_back();
								heap.back().fm = fm;
								heap.back().fm_count_save = entries[fm].count;
								heap.back().fm_distortion_onto = new_index_D;
								heap.back().to = to;
								heap.back().dj = dj;
							}
						}
					}
				}
			}

			first_singleton = fm;
		}
		} // bum_groups_make
		
		{
		SIMPLEPROFILE_SCOPE_N(bum_singles_make,entries_size-first_singleton);
		
		// fm starts where we broke out of groups
		RR_ASSERT( first_singleton == entries_size || entries[first_singleton].count == 1 );
		
		// SECOND LOOP OF SIMILAR PAIR
		// SINGLETON FMS ONLY
		make_heap_bottom_up_merge_fm_singletons(heap,entries,blocks,infos,mode,first_singleton,lambda,index_bits_this_mode,max_distortion_increase,flags);

		} // bum_singles_make
				
		make_heap(heap.begin(),heap.end());
		} // making scope

		// now pop the heap and do the merges :
			
		{
		//SIMPLEPROFILE_SCOPE(bum_merge); // near zero
		// heap is sorted by dj, largest first
		while( ! heap.empty() )
		{
			bc7rd_index_vq_heap_entry heap_entry = heap[0];
			popped_heap(heap.begin(),heap.end());
			heap.pop_back();
			
			// if from entry is gone, ignore me
			int fm = heap_entry.fm;
			if ( entries[ fm ].merged_onto >= 0 )
				continue;
			
			// if [fm] entry changed (count changed) since I computed heap.dj
			//	 then it needs to recompute D & I should repush this heap entry
			// [to] entry changing entry doesn't affect us, unless it was merged onto someone else
			bool dirty = ( heap_entry.fm_count_save != (int)entries[ fm ].count );
			
			int to = heap_entry.to;
			if ( entries[ to ].merged_onto >= 0 )
			{
				// if my dest was merged, chase where he went
				do
				{
					to = entries[ to ].merged_onto;
				} while( entries[to].merged_onto >= 0 );
				if ( to == fm ) // I'm considering A->C , but C already did C->A or C->B->A
					continue;
					
				dirty = true;
			}
			
			if ( dirty )
			{
				// make a new candidate for me to merge onto merged_to

				F32 fm_base_distortion = entries[fm].distortion_sum;
				
				RR_DURING_ASSERT( F32 check_D = bc7rd_block_link_distortion( entries[fm].block_link , blocks, infos, entries[fm].indices,flags, mode ) );
				RR_ASSERT( fequal(check_D, fm_base_distortion, 0.1f) );
			
				F32 must_beat_D = (F32)(fm_base_distortion + max_distortion_increase);
			
				// try changing indices to "to" :
				F32 new_index_D = bc7rd_block_link_distortion( entries[fm].block_link , blocks, infos, entries[to].indices,flags, mode, must_beat_D );
				
				if ( new_index_D < LARGE_D_F32 )
				{
					if ( is_legal_merge(entries[fm],entries[to]) )
					{
						// make D the delta :
						F32 delta_D = new_index_D - fm_base_distortion;

						F32 dj = bc7rd_index_merge_dj( entries[fm], entries[to], delta_D , lambda, index_bits_this_mode); //,nblocks);

						if ( dj > 0 )
						{
							// make a heap entry :
							test_shrink_vq_heap(heap, vq_heap_limit);

							heap.push_back();
							heap.back().fm = fm;
							heap.back().fm_count_save = entries[fm].count;
							heap.back().fm_distortion_onto = new_index_D;
							heap.back().to = to;
							heap.back().dj = dj;
							push_heap(heap.begin(),heap.end());
						}
					}
				}
				
				continue;
			}
			
			// fm and to are both alive
			// do the merge
			RR_ASSERT( is_legal_merge(entries[fm],entries[to]) );
			
			RR_ASSERT( heap_entry.fm_count_save == (int)entries[ fm ].count );
			//RR_ASSERT( heap_entry.fm_distortion_onto == block_link_index_distortion(&entries[fm],blocks,entries[to].indices,LARGE_DIFF,palvec) );
			
			RR_ASSERT( bc7rd_assert_block_link_check(&entries[fm],blocks) );
			RR_ASSERT( bc7rd_assert_block_link_check(&entries[to],blocks) );
			
			//RR_ASSERT( entries[fm].distortion_sum == block_link_index_distortion(&entries[fm],blocks,entries[fm].indices,LARGE_DIFF,palvec) );
			//RR_ASSERT( entries[to].distortion_sum == block_link_index_distortion(&entries[to],blocks,entries[to].indices,LARGE_DIFF,palvec) );

			entries[fm].merged_onto = to;
			//entries[fm].count = 0; // optional, for debug tracking, keeps sum of all entries counts constant
			entries[to].count += entries[fm].count;
			
			entries[to].count_log2_count = entries[to].count * log2tabled_bk_32( entries[to].count );
			
			// merge the actual blocks :
			// all [fm] indices change to [to]

			// find the tail of the link to connect them
			int link = entries[fm].block_link;
			RR_DURING_ASSERT( U32 link_count = 1 );
			while( blocks[link].link >= 0 )
			{
				RR_DURING_ASSERT( link_count++ );
				RR_ASSERT( link_count <= entries[fm].count );
				
				link = blocks[link].link;
			}
			RR_ASSERT( link_count == entries[fm].count );
			
			// link the [to] chain onto it :
			blocks[link].link = entries[to].block_link;
			entries[to].block_link = entries[fm].block_link;
			
			//RR_ASSERT( assert_block_link_check(&entries[to],blocks) );
			
			// sum of all delta D's = net SSD change
			//F32 delta_D = heap_entry.fm_distortion_onto - entries[fm].distortion_sum;
			//total_D_delta += delta_D;
			
			// just add on the distortion of [fm] list adding on , which we already computed for heap J
			//	could store that in the heap record, then just check it here
			entries[to].distortion_sum += heap_entry.fm_distortion_onto;
			
			RR_DURING_ASSERT( F32 check_distortion_sum = bc7rd_block_link_distortion(entries[to].block_link,blocks,infos,entries[to].indices,flags,mode) );
			RR_ASSERT( fequal(entries[to].distortion_sum,check_distortion_sum,0.1f) );
		}
		}

		// scan out just the un-merged entries :
		// indices_out and total_count_out are accumulated from both the 3c and 4c loop
		for LOOPVEC(entry_i,entries)
		{
			if ( entries[entry_i].merged_onto >= 0 ) continue;
			
			RR_ASSERT( entries[entry_i].count > 0 );
			RR_ASSERT( bc7rd_assert_block_link_check(&entries[entry_i],blocks) );

			bc7bits new_indices = entries[entry_i].indices;

			// walk these blocks and commit the change
			int bi = entries[entry_i].block_link;
			while( bi >= 0 )
			{
				bc7bits bits = bc7bits_or_assert_exclusive(blocks[bi].encoding_without_indices , new_indices);
								
				U8 * block_ptr = BlockSurface_Seek(bc7_blocks,bi);
				bc7bits prev_bits = bc7bits_load(block_ptr);
				
				if ( bits != prev_bits )
				{				
					bc7bits_store(block_ptr,bits);

					//infos[bi].D & J need update
					// final matrix comes next, it needs corrected scores

					// we used SSD internally but the infos[].D is VQD so compute it :
					F32 new_D = bc7rd_D(bits,infos[bi].colors,flags,*blocks[bi].activity);
					F32 delta_D = new_D - infos[bi].D;
					infos[bi].D = new_D;
					// NOTE : fixing J for D change, but rate is not fixed for the index merges we did
					//	-> we will re-rate externally with bc7rd_recompute_rates_static_model
					infos[bi].J += delta_D;

					#ifdef CHECK_EXTREMES_PRESERVED
					if (infos[bi].flags & BC7ENC_PRESERVE_EXTREMES)
					{
						RAD_ALIGN(U8, decomp_block[64], BC7_BLOCK_ALIGN);
						bc7_decode_block4x4a(decomp_block,block_ptr,infos[bi].flags);

						BC7Constraints dec_constraints = BC7Constraints::from_pixels(decomp_block);
						if (!dec_constraints.superset_of(infos[bi].constraints))
						{
							BC7BlockState st, orig_st;

							bc7enc_state_from_bits(&st,block_ptr,infos[bi].flags); // need to use infos[bi].flags of that block when decoding to get correct endpoints_q
							bc7enc_state_from_bits(&orig_st,bc7bits_U8ptr(&prev_bits),infos[bi].flags); // need to use infos[bi].flags of that block when decoding to get correct endpoints_q
							U32 cmask = infos[bi].constraints.mask;

							rrprintf("index merge extremes mismatch! mode=%d isb=%d rb=%d constraint mask=0x%08x\n", st.mode, st.subsets[0].isbit, st.subsets[0].rbit, cmask);
							rrprintf("entry constraints=(0x%08x,0x%08x)\n",
								entries[entry_i].ind_constraints[0].mask, entries[entry_i].ind_constraints[1].mask);
							rrprintf("subset0 alpha: %02x-%02x orig: %02x-%02x\n",
								 st.subsets[0].endpoints[0].a, st.subsets[0].endpoints[1].a,
								 orig_st.subsets[0].endpoints[0].a, orig_st.subsets[0].endpoints[1].a);
							rrprintf("preserve: a0=%d a255=%d\n", (infos[bi].flags & BC7ENC_PRESERVE_A0) != 0, (infos[bi].flags & BC7ENC_PRESERVE_A255) != 0);

							const int which_ind = (st.mode < 4 || st.mode > 5 || st.subsets[0].isbit) ? 0 : 1;
							const BC7Inds& aind = st.subsets[0].idxs[which_ind];
							const BC7Inds& orig_aind = orig_st.subsets[0].idxs[which_ind];

							for LOOP(i,16)
							{
								rrprintf("[%2d] orig=%02x dec=%02x ind=%x orig_ind=%x %c%c\n", i, infos[bi].colors.colors[i].u.a, decomp_block[i*4+3], aind.ind[i],
									orig_aind.ind[i], " -"[(cmask >> i) & 1], " +"[(cmask >> (16 + i)) & 1]);
							}

							RR_ASSERT_ALWAYS(!"mismatch");
						}
					}
					#endif
				}
				
				bi = blocks[bi].link;
			}			
		}
		
		// yes confirmed okay :
		//  total_D_delta of all the heap merge steps = net SSD change
		/*
		U32 total_ssd_after = bc7rd_total_ssd(bc7_blocks,infos);
		U32 intended_ssd_delta = rr_froundint( total_D_delta / 2.f ); // D/SSD ratio
		RR_ASSERT( (total_ssd_after - total_ssd_before) == intended_ssd_delta );
		
		rrprintf("rmse before = %.3f\n", sqrt( total_ssd_before / (F64)(nb*16) ) );
		rrprintf("rmse after = %.3f\n", sqrt( total_ssd_after / (F64)(nb*16) ) );
		*/
	}
}

bool BC7_RD(BlockSurface * to_blocks,
	const BlockSurface * from_blocks,
	const BlockSurface * baseline_blocks,
	const BlockSurface * activity_blocks,
	int lambdai,rrDXTCOptions options,
	const rrDXTCRD_Options & rdoptions)
{
	//rrprintf("BC7_RD: effort level=%d (%s)\n", rdoptions.effort, rrDXTCLevel_GetName(rdoptions.effort));
	
	RR_ASSERT( to_blocks->pixelFormat == rrPixelFormat_BC7 );
	RR_ASSERT( baseline_blocks->pixelFormat == rrPixelFormat_BC7 );
	RR_ASSERT( from_blocks->pixelFormat == rrPixelFormat_R8G8B8A8 );
	RR_ASSERT( from_blocks->blockSizeBytes == 16*4 );
	RR_ASSERT( to_blocks->count == from_blocks->count );

	const F32 lambda = lambdai * BC7RD_LAMBDA_SCALE;
	
	const BC7_RD_Config & config = rdoptions.config_override
		? *(const BC7_RD_Config *) rdoptions.config_override
		: bc7rd_get_config(rdoptions.effort);

	// options can have IGNORE_ALPHA for BC7RGB
	//	 if so, orig image A was already set to 255 in TexPub
	BC7EncOptions opt;
	bc7_enc_options_set(&opt,config.block_encode_effort,options);

	CpuDispatchFlags dispatch = CpuDispatchFlags::init(&options);

	//U64 from_hash = BlockSurface_DebugHash(from_blocks); // for debug

	RR_ASSERT( from_blocks->count <= 16*1024 );
	const int nblocks = check_value_cast<int>(from_blocks->count);
	
	// 16K blocks per 256K LZ chunk
	// we further slice that to 4K blocks per slice
	//	so typically nblocks = 4K
	
	vector<bc7rd_blockinfo> infos;
	infos.resize(nblocks);

	// Precompute per-mode error bias for this lamba
	BC7_Mode_ErrBias mode_errbias;
	for LOOP(m,BC7_NUM_MODES)
	{
		mode_errbias.ssd_err_bias[m] = bits_to_ssd_err_bias_J( bc7_average_block_size_bits[m], lambda );
	}

	/*
	int stat_in_loop_matrix_hyperbola_count = 0;
	int stat_in_loop_matrix_decode_count = 0;
	int stat_in_loop_matrix_vqd_count = 0;
	/**/
	
	// 2 iters for improved seeding does have a small but consistent improvement (all images improve a little)
	// rdotestset1 :
	//	  total BPB saved at vcdiff +1.0 : [ 14.252 ]
	//	  prev BPB saved at vcdiff +1.0 : [ 14.134 ]
	// it is NOT a good use of CPU time
	//  much better to increase WINDOW_SIZE
	//for LOOP(iters,2)
	for LOOP(iters,1)
	{
		// mode_class limits possible merges: we can use endpoint/indices
		// from blocks that are in the same or a lower mode class, but not
		// a higher one.
		//
		// Currently mode_class = number of subsets.
		static const U8 mode_class[8] = { 3,2,3,2, 1,1,1,2 };
		//static const U8 mode_class[8] = { 0,0,0,0, 0,0,0,0 }; // nullifies mode class logic
	
		if ( iters > 0 )
		{
			// seed from first iter
			//  this is the whole output of the first iter
			// just used to seed the window and the histo to err bias
			baseline_blocks = to_blocks;
		}
	
		typedef hash_table<bc7bits,int,hash_table_ops_mask31hash<bc7bits> > BlockToIndexHash;
		BlockToIndexHash block_to_index;
		block_to_index.reserve(nblocks);

		BC7RDWindow window_endpoints(config.mtf_window_endpoints_size);
		BC7RDWindow window_indices(config.mtf_window_indices_size);
		vector<BC7RDEndpointWindowExtra> window_endpoint_extra;
		vector_aligned<BC7EndpointCoeffCache, 16> window_index_endpoint_cache;

		// Indexed with window_endpoints.mtf[i]
		// so the data is associated with the stable slot IDs, not the ever-changing MRU order
		window_endpoint_extra.resize(config.mtf_window_endpoints_size);

		// Indexed with window_indices.mtf[i]
		// so the data is associated with the stable slot IDs, not the ever-changing MRU order
		window_index_endpoint_cache.resize(config.mtf_window_indices_size);

		if (config.mtf_window_seed_most_common) // @@ ??
		{
			// seed block window with most common from baseline ?
			// -> currently pretty meh
			//  does smooth out a nasty wiggle on "decanter"
			//	definitely slightly worse on "wood_worn" and "frymire" , nop on test6 & test7
			window_endpoints.seed(baseline_blocks,opt.flags,&window_endpoint_extra,mode_class);
		}
		
		// get block usage in the baseline and favor using more common mode/part :
		BC7_ModeHisto histo_mode;
		histogram_mode(&histo_mode,baseline_blocks);
		BC7_Mode_ErrBias errbias;
		histogram_to_ssd_err_bias_J(&errbias,histo_mode,lambda);
		
		BC7_Mode_ErrBias expected_size_errbias;
		for LOOP(m,BC7_NUM_MODES)
		{
			// note bc7_average_block_size_bits does not include cost of sending mode
			//	because it's measured by forcing all modes same (therefore entropy of mode = 0)
			// so adding on histo cost of mode gives you a full block cost
			// "errbias" is a J in units of SSD
			expected_size_errbias.ssd_err_bias[m] = mode_errbias.ssd_err_bias[m] + errbias.ssd_err_bias[m];
		}
		
		dword_and_count mode_and_count_sorted[BC7_NUM_MODES];
		for LOOPINT(m,BC7_NUM_MODES)
		{
			mode_and_count_sorted[m].dw = m;
			mode_and_count_sorted[m].count = expected_size_errbias.ssd_err_bias[m];
		}
		stdsort(mode_and_count_sorted,mode_and_count_sorted+BC7_NUM_MODES,dword_and_count_compare_count_lowest_first());
		
		// @@!!
		// to rate blocks using static histo model, seeded from baseline blocks
		//	-> this is not great, because the seed blocks are way off from what we want to output
		//bc7rd_static_rater seed_rater;
		//seed_rater.init(baseline_blocks);

		{
		SIMPLEPROFILE_SCOPE_N(bc7rd_core,nblocks); // primary loops
	
		for LOOPINT(bi,nblocks)
		{
			const U8 * baseline_block_ptr = BlockSurface_SeekC(baseline_blocks,bi);
			U8 * output_bc7 = BlockSurface_Seek(to_blocks,bi);
			const U8 * fmPtr = BlockSurface_SeekC(from_blocks,bi);
				
			const ActivityBlock4x4 * pActivity = BlockSurface_SeekC_ActivityBlock4x4(activity_blocks,bi);
				
			// BC7Prep will change A to 255 in block if ignore alpha. Also copies pixels.
			BC7Prep prep;
			const U8 * block;
			{
				int baseline_mode = bc7bits_get_mode(bc7bits_load(baseline_block_ptr));
				int my_class = mode_class[baseline_mode];

				// Disable modes that we're never going to use due to mode class heuristic
				// In particular, if our block is already in a 1-subset mode, we don't
				// need to rank the 2/3-subset mode candidates at all.
			#if _MSC_VER >= 1938 // workaround for VC++ 2022 bug
				#pragma loop(no_vector)
			#endif
				for (int mode = 0; mode < 8; ++mode)
					opt.enable_mode[mode] = mode_class[mode] <= my_class;

				block = BC7Prep_init(&prep,fmPtr,opt,false);
			}
			
			const rrColorBlock4x4 & colors = *((const rrColorBlock4x4 *)block);
			infos[bi].flags = BC7_ComputeEffectiveFlags(opt.flags, &prep);
			infos[bi].constraints = (opt.flags & BC7ENC_PRESERVE_EXTREMES) ? BC7Constraints::from_pixels(colors) : BC7Constraints::none();
			infos[bi].colors = colors;
			if (record_candidates_enabled)
			{
				record_activity_block(lambda, to_blocks, bi, *pActivity);
				record_input_block_as_candidate(lambda, 0, to_blocks, infos, bi);
			}
			
			if ( config.core_fix_single_color_block && prep.single_color )
			{
				// RD optimized single color?
				// or just copy from baseline
				//bc7_encode_single_color_block(output_bc7, block);
				memcpy(output_bc7,baseline_block_ptr,16); // BC7 Block

				// just stuff zeros, then final matrix won't try to change this
				infos[bi].D = 0;
				infos[bi].J = 0;
				infos[bi].baseline_D = 0;
				if (record_candidates_enabled)
					record_candidate_block(lambda, 1, to_blocks, bi, output_bc7, 0., 0.0, infos[bi].flags); // we don't have a rate we can assign this

				continue;
			}

			// else, do it !
			
			U8 new_baseline_block[16]; // out of scope where I can point at it

			if (config.core_recompress_block_with_J_bias)
			{
				// recomp block with J bias - yes helps a lot
			
				// re-compress baseline with expected_size_errbias
				//	now only done for a few modes that have lower expected size bias than
				//	 the mode previously chosen by baseline
				
				SIMPLEPROFILE_SCOPE(bc7rd_recompress);
				
				bc7bits old_bits = bc7bits_load(baseline_block_ptr);
				int old_mode = bc7bits_get_mode(old_bits);
				
				// find old_mode in sorted list :
				int old_mode_sort_pos = 0;
				while( mode_and_count_sorted[old_mode_sort_pos].dw != (U32)old_mode )
					old_mode_sort_pos++;
				
				if ( old_mode_sort_pos > 0 )
				{
					// cur_errbias turns off modes we don't need to consider
					BC7_Mode_ErrBias cur_errbias;
					cur_errbias = expected_size_errbias;
					
					// turn off modes that are less common than old_mode :
					//	 and turn off old mode too		
						
					// truncating to 3 is nearly a NOP :
					old_mode_sort_pos = RR_MIN(old_mode_sort_pos,3); // leave a max of 3 modes on
					//old_mode_sort_pos = RR_MIN(old_mode_sort_pos,2); // leave a max of 2 modes on
					// hell even truncating to 1 is ok (it's not a NOP, better on some, worse on others)
					//old_mode_sort_pos = 1; // leave a max of 1 modes on
					for(int sort_pos=old_mode_sort_pos;sort_pos<8;sort_pos++)
					{
						int m = mode_and_count_sorted[sort_pos].dw;
						cur_errbias.ssd_err_bias[m] = RR_U32_MAX;
					}

					if (config.core_disable_less_common_modes)
					{
						// ALSO turn off modes that are less common than old_mode
						//	(this is very similar to just truncating the above to 1 or 2)
						// @@ not entirely sure about this, but seems to be okay
						for(int sort_pos=1;sort_pos<old_mode_sort_pos;sort_pos++)
						{
							int m = mode_and_count_sorted[sort_pos].dw;
							if ( histo_mode.count[m] <= histo_mode.count[old_mode] )
								cur_errbias.ssd_err_bias[m] = RR_U32_MAX;
						}
					}

					// use the "Prep" we already made; does save a tiny bit of time
					if ( BC7_CompressBlock_Sub(new_baseline_block,block,&opt,&prep,&cur_errbias) )
					{				
						// do a J test for the recomp baseline vs original baseline
						bc7bits new_bits = bc7bits_load(new_baseline_block);
						int new_mode = bc7bits_get_mode(new_bits);
						
						RR_ASSERT( new_mode != old_mode );
						RR_ASSERT( expected_size_errbias.ssd_err_bias[new_mode] <= expected_size_errbias.ssd_err_bias[old_mode] );
						
						// typically you should see that new_mode has lower ssd_err_bias and higher D
						//	 (since it wasn't chosen by original baseline encode which only cares about D)
											
						U32 old_D = bc7rd_SSD(old_bits,block,infos[bi].flags);
						U32 new_D = bc7rd_SSD(new_bits,block,infos[bi].flags);
							
						// J&D here is in units of SSD (not 2*SSD like VQD)
						U32 old_J = old_D + expected_size_errbias.ssd_err_bias[old_mode];
						U32 new_J = new_D + expected_size_errbias.ssd_err_bias[new_mode];
							
						if ( new_J < old_J )
						{
							baseline_block_ptr = new_baseline_block;
								
							// then everything recomputed below
						}
					}
				}							
			}
			
			BC7BlockState baseline_st;
			bc7enc_state_from_bits(&baseline_st,baseline_block_ptr,infos[bi].flags);
		
			/*
			// check that my SSD matches bc7comp :
			{
				bc7bits baseline_bits = bc7bits_load(baseline_block_ptr);
				U32 baseline_SSD = bc7rd_SSD(baseline_bits,block,infos[bi].flags);
				//RR_ASSERT( baseline_SSD == baseline_st.err );
				//baseline_st.err is NOT set!
				BC7Error baseline_st_err = bc7enc_calc_error(&baseline_st,block,infos[bi].flags);
				
				RR_ASSERT( baseline_SSD == baseline_st_err );
			}
			*/

			//U32 baseline_err = bc7enc_calc_error(&baseline_st,block,infos[bi].flags);
			F32 baseline_D = bc7rd_D(&baseline_st,block,infos[bi].flags,*pActivity);
			F32 baseline_R = bc7rd_R_uncompressed;
			if (record_candidates_enabled)
				record_candidate_block(lambda, 2, to_blocks, bi, baseline_st, baseline_D, baseline_R, infos[bi].flags);
	
			/*
			// investigating where A error comes from on opaque images
			//	  A should be 255 everywhere
			// on tech_panel_metal_001_g.bmp :
			// 100% of the A error comes from mode 6 pbits
			//	endpoint is 254 , pbit is 0 (if it was 1 you'd hit 255)
			//	decode comes out with A == 254 or 255 everywhere
			{
				U8 baseline_decoded[64];
				bc7enc_decode_state(&baseline_st,baseline_decoded,infos[bi].flags);
				U32 a_sum = 0;
				for(int i=3;i<64;i+=4)
					a_sum += baseline_decoded[i];
				RR_ASSERT( a_sum == 255*16 || ( a_sum >= 254*16 && baseline_st.mode == 6 ) );
			}			
			/**/
			
			#if 0
			// @@!!
			// rate baseline block from seed_rater
			//	rather than just assume it's 128 raw bits
			baseline_R = seed_rater.get_block_rate(baseline_block_bits);
			#endif
						
			F32 baseline_J = bc7rd_J(baseline_D,baseline_R,lambda);

			infos[bi].baseline_D = baseline_D;
					
			// start with baseline :
			F32 best_J = baseline_J;
			F32 best_D = baseline_D;
			BC7BlockState best_st = baseline_st;
			BC7Flags emit_flags = infos[bi].flags;
			int best_mtfwindow_endpoints_i = -1;
			int best_mtfwindow_indices_i = -1;
		
			if ( 1 )
			{
				// if we've seen the exact same baseline block before in this chunk, make sure we produce the same encoding
				// (if we can)
				bc7bits baseline_block_bits = bc7bits_load(baseline_block_ptr);
				bool seen_baseline_before = false;
				const BlockToIndexHash::entry_type * entry = block_to_index.find_or_insert(BlockToIndexHash::ops().hash_key(baseline_block_bits), baseline_block_bits, bi, &seen_baseline_before);
				if ( seen_baseline_before )
				{
					// use same encoding as we did last time so we do not break up duplicates
					int prev_bi = entry->data();
					const U8 * prev_block_ptr = BlockSurface_SeekC(to_blocks,prev_bi);

					memcpy(output_bc7, prev_block_ptr, 16);

					F32 repeat_R = bc7rd_matched_R(128,bi - prev_bi);

					// need to recompute block D even though we use the same block as prev_bi:
					// activity can be different
					RAD_ALIGN(U8, decoded[64], BC7_BLOCK_ALIGN);
					bc7_decode_block4x4a(decoded,prev_block_ptr,infos[bi].flags);

					// May only substitute if compatible with constraints
					BC7Constraints subst_constraints { 0 };
					if ( !infos[bi].constraints.empty() ) // if block is unconstrained, no need to compute which constraints we satisfy
						subst_constraints = BC7Constraints::from_pixels(decoded);

					if ( subst_constraints.superset_of(infos[bi].constraints) )
					{
						infos[bi].D = bc7rd_D(decoded,block,infos[bi].flags,*pActivity);
						infos[bi].J = bc7rd_J(infos[bi].D,repeat_R,lambda);

						// NOT SURE: insert into window?
						// easier said than done, would need to find where it actually is to get the right inds
						/*bc7rd_windowentry new_entry;
						bc7enc_state_from_bits(&new_entry.st,prev_block_ptr,infos[bi].flags);
						new_entry.encoded = bc7bits_load(prev_block_ptr);*/

						continue;
					}
				}
			}

			// ---- Take past blocks and try substituting endpoints or indices
			
			// 1-subset inputs are used by Mode 4, 5, 6.
			BC7Input input_1s[4];
			for LOOPINT(rbit,4)
				BC7Input_Get(&input_1s[rbit],block,/*ns*/1,/*p*/0,rbit,/*alpha_weight*/1);

			// The in-loop input that isn't compatible with input_rgba_1s.
			BC7Input mtf_input;

			// Ring buffer for "good" endpoint substitutions (that actually improve J). Keep track
			// of those during endpoint subst and make those our endpoint candidates during the
			// matrix phase.
			static const int GOOD_ENDPOINT_MAX = 16;
			int good_endpoints[GOOD_ENDPOINT_MAX];
			int good_endpoint_count = 0;

			if ( window_endpoints.size > 0 )
			{
				SIMPLEPROFILE_SCOPE(bc7rd_endpoints);
				// loop on window of past endpoints
				//	find new indexes
			
				// best_J_here is best_J from this loop, not overall
				F32 best_J_here = (F32)BC7_ERROR_MAX;
				F32 ssd_cutoff = (F32)BC7_ERROR_MAX;
				
				// mtfwindi_last will get copy of best and do slow refine
				//  I'm doing the copy-to-last just to avoid code dupe
				int mtfwindi_iters = window_endpoints.size+1;
				mtfwindi_iters = RR_MIN(mtfwindi_iters, config.mtf_window_endpoints_size);
				int mtfwindi_last = mtfwindi_iters-1;
				RR_DURING_ASSERT( window_endpoints[mtfwindi_last].st.err = BC7_ERROR_MAX );
				// disable last iter :
				mtfwindi_iters = mtfwindi_last;
						
				U32 best_err = BC7_ERROR_MAX;
		
				// Whole-block PCA axis for mode 4/5/6 early reject
				F32 mode456_overall_err[6]; // [variant] 0=disable, 1=mode 6, 2-5=rbit choices for mode 4 and 5
				alignas(16) F32 mode456_scaled_pca[6][4]; // [variant][chan]
				{
					// When early reject is disabled, we always produce an error estimate of 0, which means
					// we have to check the block.
					mode456_overall_err[0] = 0.0f;
					for (SINTa i = 0; i < 4; ++i)
						mode456_scaled_pca[0][i] = 0.0f;

					const BC7SubsetAnalysis& analysis = prep.analysis;
					for (SINTa i = 0; i < 5; ++i)
					{
						SINTa subset_id = prep.ranking.pca_subset1[0] + i;

						// Our assumption is that the best we can do with these modes is annihilate all the
						// error along the PCA axis
						mode456_overall_err[i + 1] = analysis[BC7SubsetChan::OverallErr][subset_id];
						const F32 mode456_pca_err = analysis[BC7SubsetChan::PcaErr][subset_id];

						// In our ssd_estimate, we ultimately want fabsf(dot) * mode456_pca_err
						// we can fold the scalar into the dot vector:
						mode456_scaled_pca[i + 1][0] = analysis[BC7SubsetChan::PcaR][subset_id] * mode456_pca_err;
						mode456_scaled_pca[i + 1][1] = analysis[BC7SubsetChan::PcaG][subset_id] * mode456_pca_err;
						mode456_scaled_pca[i + 1][2] = analysis[BC7SubsetChan::PcaB][subset_id] * mode456_pca_err;
						mode456_scaled_pca[i + 1][3] = analysis[BC7SubsetChan::PcaA][subset_id] * mode456_pca_err;
					}
				}

				for LOOPINT(mtfwindowi,mtfwindi_iters)
				{
					const BC7RDEndpointWindowExtra& window_extra = window_endpoint_extra[window_endpoints.mtf[mtfwindowi]];

					// Don't try substituting a higher-class mode
					if ( window_extra.mode_class > mode_class[best_st.mode] )
						continue;

					SINTa pca_type = window_extra.pca_type;

					// We have an early-out for the 1-subset modes 4-6 (our most common modes generally, especially 6)
					// to check if a block can work out at all. This could potentially be generalized to 2- and 3-subset
					// modes, but 1-subset are the most common and also the easiest.
					//
					// We have the expected squared error/variance for the whole block (mode456_overall_err)
					// and the portion accounted for by the PCA axis (mode456_pca_err). In the
					// continuous setting, the best we can do is zero out the error contribution along
					// the PCA axis. For modes 4 and 5, we only consider the error in the "vector" portion,
					// optimistically assuming that we can get the error in the scalar channel to zero.
					//
					// We estimate the error we would get with the window endpoints by looking at the dot product
					// between the normalized difference vector between the endpoints (window_entry.normalized_axis)
					// and the actual PCA axis. When the absolute value is close to 1 (endpoint direction closely
					// aligns), we consider this a viable candidate, and expect to be able to cancel out most of
					// the variance in the PCA direction. As the dot product gets closer to 0, we subtract less
					// and less of that PCA error contribution.
					//
					// This estimate is quite coarse in multiple ways:
					// - We only look at the endpoint difference, not the actual values. In particular we don't
					//   even try to check whether the block mean is far away from the endpoint candidate line segment
					//   or not. (This underestimates error.)
					// - We assume that any misalignment with the PCA axis _increases_ error, but don't account for any
					//   error _reduction_ that may occur at the same time. At the extreme, consider two dominant
					//   eigenvalues of the same magnitude. Any eigenvector in their eigenspace is actually equally
					//   good, and we have a second eigenvector perpendicular to the original one that nevertheless has
					//   the exact same error estimate. Therefore, this overestimates error.
					// - There's no accounting for discretization of endpoints, indices and interpolated colors, which
					//   can both under- and over-estimate error.
					//
					// All that said, this heuristic is cheap and appears to work well in my testing.
					//
					// NOTE: mode456_scaled_pca is normalize(pca_axis) * mode456_pca_err
					F32 ssd_estimate = window_extra.estimate_ssd(mode456_overall_err[pca_type], mode456_scaled_pca[pca_type]);
					if ( ssd_estimate > ssd_cutoff )
						continue;

					// Grab the window entry
					const bc7rd_windowentry & window_entry = window_endpoints[mtfwindowi];
					const BC7BlockState & window_st = window_entry.st;
					RR_ASSERT( window_st.err != BC7_ERROR_MAX );

					// Are we allowed to use this endpoint subst?
					if ( ! are_endpoints_permitted(window_st,infos[bi]) )
						continue;

					BC7BlockState st;

					BC7Input *input;
					if (window_st.mode >= 4 && window_st.mode <= 6)
						input = &input_1s[window_st.subsets[0].rbit];
					else
					{
						BC7Input_Get(&mtf_input,block,&window_st);
						input = &mtf_input;
					}

					// take past endpoints and find new indexes for this block : (and pbits?)
					// first run through, use calc_indexes_linear_approx
					//	then on the last/best redo with calc_indexes_exact
					bool slow_exact = (mtfwindowi == mtfwindi_last);
					BC7Flags reindex_flags = infos[bi].flags | (slow_exact ? BC7ENC_IDXS_EXACT : 0);
					int bits_changed = bc7enc_reindex_block(&st,&window_st,input->subsets,reindex_flags);
					int cur_matched_bits = 128 - bits_changed;
					RR_ASSERT( cur_matched_bits == bc7rd_lz_matched_bits_reindex(st.mode,false) );
					
					#if 0 // check that only indices changed
					{
						bc7bits out;
						bc7enc_emit(bc7bits_U8ptr(&out),&st);
						bc7bits old = window_endpoints[mtfwindowi].encoded;
						bc7bits x = bc7bits_xor(out,old);
						// x should only be on in index bits
						bc7bits mask( c_bc7bitrange_indices_mask[st.mode] );
						RR_ASSERT_ALWAYS( bc7bits_iszero( bc7bits_andnot(x,mask) ) );
					}
					#endif
					
					// use st->err (SSD) as early out?
					//	we want to favor lower mtfwindowi , so require SSD decreasing:
					if ( st.err > best_err ) continue;
					best_err = st.err;

					ssd_cutoff = (F32)st.err;

					#ifdef CHECK_EXTREMES_PRESERVED
					{
						RAD_ALIGN(U8, decomp_block[64], BC7_BLOCK_ALIGN);
						bc7enc_decode_state(&st, decomp_block, infos[bi].flags);

						BC7Constraints dec_constraints = BC7Constraints::from_pixels(decomp_block);
						if (!dec_constraints.superset_of(infos[bi].constraints))
						{
							BC7Flags flags = infos[bi].flags;
							U32 cmask = infos[bi].constraints.mask;
							const BC7SubsetState& sst0 = st.subsets[0];

							rrprintf("mtf endpoints extremes mismatch! matched mode=%d isb=%d rb=%d cmask=0x%08x slow_exact=%d\n", st.mode, st.subsets[0].isbit, st.subsets[0].rbit, cmask, slow_exact);
							rrprintf("subset0 alpha: desired=(%02x-%02x) q=(%02x-%02x)\n", sst0.endpoints[0].a, sst0.endpoints[1].a, sst0.endpoints_q[0].a, sst0.endpoints_q[1].a);
							rrprintf("preserve: a0=%d a255=%d\n", (flags & BC7ENC_PRESERVE_A0) != 0, (flags & BC7ENC_PRESERVE_A255) != 0);
							const BC7Inds& aind = st.subsets[0].idxs[(st.mode < 4 || st.mode > 5 || st.subsets[0].isbit) ? 0 : 1];

							for LOOP(i,16)
							{
								rrprintf("[%2d] orig=%02x dec=%02x ind=%x %c%c\n", i, block[i*4+3], decomp_block[i*4+3], aind.ind[i],
									" -"[(cmask >> i) & 1], " +"[(cmask >> (16 + i)) & 1]);
							}

							RR_ASSERT_ALWAYS(!"mismatch");
						}
					}
					#endif

					F32 cur_D = bc7rd_D(&st,block,infos[bi].flags,*pActivity);
					F32 cur_R = bc7rd_matched_R(cur_matched_bits,mtfwindowi+1);
					F32 cur_J = bc7rd_J(cur_D,cur_R,lambda);

					if ( cur_J < best_J )
					{
						best_J = cur_J;
						best_D = cur_D;
						best_st = st;
						best_mtfwindow_endpoints_i = mtfwindowi;
						best_mtfwindow_indices_i = -1;
						if (record_candidates_enabled)
							record_candidate_block(lambda, 3, to_blocks, bi, st, cur_D, cur_R, infos[bi].flags);

						good_endpoints[good_endpoint_count & (GOOD_ENDPOINT_MAX - 1)] = mtfwindowi;
						good_endpoint_count++;
					}
					
					if ( cur_J < best_J_here )
					{
						best_J_here = cur_J;
						// copy cur to last :
						window_endpoints[mtfwindi_last] = window_endpoints[mtfwindowi];
						// copy matching extra data
						window_endpoint_extra[window_endpoints.mtf[mtfwindi_last]] = window_endpoint_extra[window_endpoints.mtf[mtfwindowi]];
						// enable last iter :
						mtfwindi_iters = mtfwindi_last+1;
					}
				}
			} // endpoints loop
			
			if ( config.core_endpoints_from_indices && window_indices.size > 0 )
			{
				SIMPLEPROFILE_SCOPE(bc7rd_indices);
				
				// this (endpoints-from-indices loop) is not uniformly good
				// "frymire" gets much worse with this on
				// "good_record_only" get a bit worse
				//	most others get a bit better with this
				// "red_blue" desperately needs this and pushes the overall score in favor of ON


				// try past indices with lsqr to make new endpoints
				
				// best_J_here is best_J from this loop, not overall
				F32 best_J_here = (F32)BC7_ERROR_MAX;
				
				// mtfwindi_last will get copy of best and do slow refine
				//  I'm doing the copy-to-last just to avoid code dupe
				int mtfwindi_iters = window_indices.size+1;
				mtfwindi_iters = RR_MIN(mtfwindi_iters, config.mtf_window_indices_size);
				int mtfwindi_last = mtfwindi_iters-1;
				RR_DURING_ASSERT( window_indices[mtfwindi_last].st.err = BC7_ERROR_MAX );
				// disable last iter :
				mtfwindi_iters = mtfwindi_last;
				
				U32 best_err = BC7_ERROR_MAX;

				for LOOPINT(mtfwindowi,mtfwindi_iters)
				{	
					const BC7BlockState& window_st = window_indices[mtfwindowi].st;
					RR_ASSERT( window_st.err != BC7_ERROR_MAX );

					// Don't try substituting a higher-class mode
					if ( mode_class[window_st.mode] > mode_class[best_st.mode] )
						continue;

					// If it's a mode we're not allowed to use, skip candidate
					BC7Flags flags = infos[bi].flags;
					if ( ! are_indices_permitted(window_st,infos[bi],&flags) )
						continue;

					BC7Input *input;
					if (window_st.mode >= 4 && window_st.mode <= 6)
						input = &input_1s[window_st.subsets[0].rbit];
					else
					{
						BC7Input_Get(&mtf_input,block,&window_st);
						input = &mtf_input;
					}

					BC7BlockState st;

					// take past indexes and find new endpoints for this block : (and pbits?)
					bool change_pbits = false; // NOTE: for preserve extremes mode, pbits must be left alone. In other modes, could potentially change them.
					//bool change_pbits = true; // tiny bit worse, small difference
					// do slow_anneal on just the "last" which is a copy of the best so far
					// @@ if we get to last and best_J_here is too far off best_J , skip it?
					bool is_last = mtfwindowi == mtfwindi_last;
					bool slow_anneal = is_last;
					const BC7EndpointCoeffCache * cache = &window_index_endpoint_cache[window_indices.mtf[mtfwindowi]];

					int bits_changed = bc7enc_find_endpoints(&st,&window_st,input,flags,change_pbits,slow_anneal,cache);
					
					#if 0 // check that only endpoints were changed
					{
						bc7bits out;
						bc7enc_emit(bc7bits_U8ptr(&out),&st);
						bc7bits old = mtfwindow_indices[mtfwindowi].encoded;
						bc7bits x = bc7bits_xor(out,old);
						// x should only be on in index bits
						bc7bits mask( c_bc7bitrange_endpoints_mask[st.mode] );
						RR_ASSERT_ALWAYS( bc7bits_iszero( bc7bits_andnot(x,mask) ) );
					}
					#endif
		
					// use st->err (SSD) as early out?
					if ( st.err > best_err ) continue;
					best_err = st.err;		
						
					int cur_matched_bits_hi = 128 - bits_changed;
					int cur_matched_bits_lo = bc7rd_lz_matched_bits_reendpoint(st.mode,change_pbits);
					// cur_matched_bits_lo is just the index bits
					// cur_matched_bits_hi is index bits and header bits (all but endpoints)
					//	(cur_matched_bits_hi - cur_matched_bits_lo) == header bits
					
					// @@ I think cur_matched_bits_lo is better justified, but it hurts quite a bit at the moment
					//	cur_matched_bits_hi gives an overestimate of the value of a match (assumes all header bits match too)
					//	perhaps an issue is that we do frequently match the next header byte following indices
					//	 because the unary mode byte does tend to be clumpy, so we have a decent chance of getting that (?)
					//	@@ REVISIT ME
					RR_ASSERT( cur_matched_bits_hi > cur_matched_bits_lo );
											
					// average them is assuming there's a 50% chance of a header match :
					//int cur_matched_bits = (cur_matched_bits_lo + cur_matched_bits_hi + 1)/2;
					
					// guess we can match indices and the mode bits :
					// at high lambda, the full cur_matched_bits_hi seems to be best
					//	at lower lamda this is best 
					int cur_matched_bits = cur_matched_bits_lo + (st.mode+1);
					RR_UNUSED_VARIABLE( cur_matched_bits_hi );
					
					RR_ASSERT( cur_matched_bits <= cur_matched_bits_hi );

					#ifdef CHECK_EXTREMES_PRESERVED
					{
						RAD_ALIGN(U8, decomp_block[64], BC7_BLOCK_ALIGN);
						bc7enc_decode_state(&st, decomp_block, flags);

						BC7Constraints dec_constraints = BC7Constraints::from_pixels(decomp_block);
						if (!dec_constraints.superset_of(infos[bi].constraints))
						{
							U32 cmask = infos[bi].constraints.mask;

							rrprintf("mtf indices extremes mismatch! matched mode=%d isb=%d rb=%d constraint mask=0x%08x slow_anneal=%d\n", st.mode, st.subsets[0].isbit, st.subsets[0].rbit, cmask, slow_anneal);
							rrprintf("subset0 alpha: %02x-%02x\n", st.subsets[0].endpoints[0].a, st.subsets[0].endpoints[1].a);
							rrprintf("preserve: a0=%d a255=%d\n", (flags & BC7ENC_PRESERVE_A0) != 0, (flags & BC7ENC_PRESERVE_A255) != 0);

							const BC7Inds& aind = st.subsets[0].idxs[(st.mode < 4 || st.mode > 5 || st.subsets[0].isbit) ? 0 : 1];

							for LOOP(i,16)
							{
								rrprintf("[%2d] orig=%02x dec=%02x ind=%x %c%c\n", i, block[i*4+3], decomp_block[i*4+3], aind.ind[i],
									" -"[(cmask >> i) & 1], " +"[(cmask >> (16 + i)) & 1]);
							}

							RR_ASSERT_ALWAYS(!"mismatch");
						}
					}
					#endif

					// this hack gives same results as (128 - bits_changed)
					//cur_matched_bits = 8 + bc7rd_lz_matched_bits_reendpoint(st.mode,change_pbits);
					
					F32 cur_D = bc7rd_D(&st,block,infos[bi].flags,*pActivity);
					F32 cur_R = bc7rd_matched_R(cur_matched_bits,mtfwindowi+1);
					F32 cur_J = bc7rd_J(cur_D,cur_R,lambda);

					if ( cur_J < best_J )
					{
						best_J = cur_J;
						best_D = cur_D;
						best_st = st;
						best_mtfwindow_endpoints_i = -1;
						best_mtfwindow_indices_i = mtfwindowi;
						if (record_candidates_enabled)
							record_candidate_block(lambda, 4, to_blocks, bi, st, cur_D, cur_R, infos[bi].flags);
					}
					
					if ( cur_J < best_J_here )
					{
						best_J_here = cur_J;
						// copy cur to last :
						window_indices[mtfwindi_last] = window_indices[mtfwindowi];
						// copy matching endpoint cache data
						window_index_endpoint_cache[window_indices.mtf[mtfwindi_last]] = window_index_endpoint_cache[window_indices.mtf[mtfwindowi]];
						// enable last iter :
						mtfwindi_iters = mtfwindi_last+1;
					}
				}
			} // indices loop

			if (config.do_ilm && good_endpoint_count > 0)
			{
				SIMPLEPROFILE_SCOPE(bc7rd_matrix);

				const int good_endpoint_start = RR_MAX(good_endpoint_count - GOOD_ENDPOINT_MAX, 0);
				
				// in-loop matrix
				// N*M matrix search
				// take N last indices
				//   M last endpoints
				// make combined block from past {indices+endpoints}
				//	 (only possible when they are from the same mode, hence same bit counts)
				//	 (but partition can be different, which changes the meaning of indices)
				
				// note that something we accidentally get out of this is
				//	checks for whole block repeats
				//	as we'll consider {end A} + {ind A} from block A
				//	 those will of course have the same mode & part so be viable candidates
				
				int matrix_dim = RR_MIN3(config.mtf_window_endpoints_size,
										 config.mtf_window_indices_size,
										 config.ilm_hyperbola_max_index_product);
				int matrix_dim_ind = RR_MIN(matrix_dim,window_indices.size);
				
				// find SAD of current best, to set up must_beat_sad :
				U8 best_st_decoded[64];
				bc7enc_decode_state(&best_st,best_st_decoded,infos[bi].flags);
				U32 before_sad = ColorBlock4x4_ComputeSAD_RGBA(colors,*((const rrColorBlock4x4 *)best_st_decoded));

				// using final-matrix tweaked tolerance here to reduce tweak count
				U32 must_beat_sad = rr_froundint(before_sad + lambda * BC7_FINAL_MATRIX_SAD_INCREASE_TOLERANCE);				
						
						
				for LOOPINT(indi,matrix_dim_ind)
				{	
					const BC7BlockState & past_indices_st = window_indices[indi].st;
					const bc7bits & past_indices_encoded = window_indices[indi].encoded;
				
					int mode = past_indices_st.mode;

					//bc7bits mask_i ( c_bc7bitrange_indices_mask[mode] );
					bc7bits mask_ip( c_bc7bitrange_indices_with_pbits_mask[mode] );
													
					bc7bits past_indices_noe  = bc7bits_and(past_indices_encoded,mask_ip);
					//bc7bits past_indices_noep = bc7bits_and(past_indices_encoded,mask_i);

					for (int good_endpoint_iter = good_endpoint_start; good_endpoint_iter < good_endpoint_count; ++good_endpoint_iter)
					{
						int endpi = good_endpoints[good_endpoint_iter & (GOOD_ENDPOINT_MAX - 1)];

						// hyperbola limited search :
						// we want to mainly search where either indi or endpi are very low
						//	 like 100x4 and 4x100 , not the whole 100x100 square
						//int index_product = (indi+1)*(endpi+1);
						int index_product = (indi+1)*(good_endpoint_count - good_endpoint_iter); // "best" ep is the final one we took
						if ( index_product > config.ilm_hyperbola_max_index_product ) break;

						//stat_in_loop_matrix_hyperbola_count++;
						
						const BC7BlockState & past_endpoints_st = window_endpoints[endpi].st;
						
						// must be same mode
						if ( past_endpoints_st.mode != mode ) continue;

						// part must be the same :
						//	this is not strictly required but almost all useful pairs have same part
						if ( past_endpoints_st.p != past_indices_st.p ) continue;
						
						const bc7bits & past_endpoints_encoded = window_endpoints[endpi].encoded;

						// see if past_indices block & past_endpoints block are just the same block
						//	then we aren't really doing a pair matrix, 
						//	it's just a whole block match to a neighbor					
						// skipping here seems to help perceptual quality (and is meh for rmse)
						//	@@ this doesn't actually exclude all whole block matches
						//	  after you merge the matrix it could be equal to one or the other
						if ( past_indices_encoded == past_endpoints_encoded ) continue;
						
						// take header (part/etc) from endpoints section
						// pbits can come from either
											
						// take past_endpoints_bits and turn off its index bits, swap in past_indices_bits
						
						// you can do either {E|PI} or {EP|I}
						//	tests indicate you don't need to try both ways
						//	and the difference is meh random wiggles
						// we do "pair2" {E|PI}
						
						//bc7bits past_endpoints_noi  = bc7bits_andnot(past_endpoints_encoded, mask_i);
						bc7bits past_endpoints_noip = bc7bits_andnot(past_endpoints_encoded, mask_ip);
						bc7bits merged_bits = bc7bits_or_assert_exclusive(past_endpoints_noip, past_indices_noe);
						
						RAD_ALIGN(U8, decoded[64], BC7_BLOCK_ALIGN);
						//stat_in_loop_matrix_decode_count++;
						bc7_decode_block4x4a(decoded,bc7bits_U8ptr(&merged_bits),infos[bi].flags);
						
						// SAD early out is very strong
						//  almost nothing makes it past here	
						U32 sad = ColorBlock4x4_ComputeSAD_RGBA(colors,*((const rrColorBlock4x4 *)decoded));

						if ( sad <= must_beat_sad )
						{
							// Check for constraint violations
							// only need to compute this mask if original block was constrained
							BC7Constraints satisfied_constraints { 0 };
							if ( !infos[bi].constraints.empty() )
								satisfied_constraints = BC7Constraints::from_pixels(decoded);

							if ( satisfied_constraints.superset_of(infos[bi].constraints) )
							{
								//stat_in_loop_matrix_vqd_count++;
								F32 cur_D = bc7rd_D(decoded,block,infos[bi].flags,*pActivity);

								// J can't win if D doesn't , skip the rate computation :
								if ( cur_D < best_J )
								{
									// rate is two matches :
									// @@ better in-loop pair rate?
									//	-> maybe just a tweaked constant to add on here
									F32 cur_R = bc7rd_matched_R_lz(endpi+1) + bc7rd_matched_R_lz(indi+1);
									F32 cur_J = bc7rd_J(cur_D,cur_R,lambda);

									if ( cur_J < best_J )
									{
										// We have a bit-spliced block here. Normally our "preserve extremes" flags
										// are enforced and override endpoints whenever a blocks' endpoints change,
										// but in this case we explicitly checked that the constraints are satisfied
										// for the exact bit-spliced block we had. Therefore, for our state_from_bits,
										// we want the block to be read without overriding any endpoint values.
										emit_flags &= ~BC7ENC_PRESERVE_EXTREMES;

										// get state :
										// chould just do this once at the end
										//	but the average count of this is < 1 anyway so no harm
										bc7enc_state_from_bits(&best_st,bc7bits_U8ptr(&merged_bits),emit_flags);

										/*/
										// debug code
										if ( from_hash == 0x85618e236e49327b && bi == 2813 )
										{
											rrprintf("candidate! endpi=%d indi=%d satisfied=0x%08x required=0x%08x\n", endpi, indi, satisfied_constraints.mask, infos[bi].constraints.mask);
											rrprintf("preserve: a0=%d a255=%d\n", (block_flags & BC7ENC_PRESERVE_A0) != 0, (block_flags & BC7ENC_PRESERVE_A255) != 0);

											U8 encoded[16];
											bc7enc_emit(encoded,&best_st,emit_flags);

											rrprintf("pair vs re-enc\n");
											for LOOP(i,16)
												rrprintf("[%2d] pair=%02x reenc=%02x\n", i, bc7bits_U8ptr(&pair1)[i], encoded[i]);

											print_analyzed_block("pair", bc7bits_U8ptr(&pair1));
											print_analyzed_block("reenc", encoded);

											RR_ASSERT_ALWAYS(memcmp(encoded,&pair1,16) == 0);
										}
										//*/

										best_J = cur_J;
										best_D = cur_D;
										best_mtfwindow_endpoints_i = endpi;
										best_mtfwindow_indices_i = indi;
										if (record_candidates_enabled)
											record_candidate_block(lambda, 5, to_blocks, bi, *((const rrColorBlock4x4*)decoded), mode, cur_D, cur_R);
									}
								}
							}
						}
					}
				}
			} // in-loop matrix

			infos[bi].D = best_D;
			infos[bi].J = best_J;
			
			// no? should be in canonical form already?
			// bc7enc_canonicalize
			
			// emit_flags is usually infos[bi].flags, expect for spliced blocks in in-loop matrix above
			bc7enc_emit(output_bc7,&best_st, emit_flags);

			// make sure the endpoints reflect the quantized endpoints
			// this matters because preserve extremes can make endpoint_q very different from what's in endpoint
			for LOOP(si,3)
			{
				best_st.subsets[si].endpoints[0] = best_st.subsets[si].endpoints_q[0];
				best_st.subsets[si].endpoints[1] = best_st.subsets[si].endpoints_q[1];
			}
			
			bc7rd_windowentry new_entry;
			new_entry.st = best_st;
			new_entry.encoded = bc7bits_load(output_bc7);

			// add to window :
			window_endpoints.update(&new_entry, best_mtfwindow_endpoints_i);
			window_indices.update(&new_entry, best_mtfwindow_indices_i);

			// if we added a new endpoints entry, set up its extra data
			if ( best_mtfwindow_endpoints_i < 0 )
			{
				// post-update, new slot is what's at position 0 in MRU order
				window_endpoint_extra[window_endpoints.mtf[0]].init(new_entry.st, mode_class);
			}

			// if we added a new index entry, we need to populate the index endpoint cache entry
			// if we reused an existing cache entry, "update" just shuffled pointers around, so
			// no need to update any slots.
			if ( best_mtfwindow_indices_i < 0 )
			{
				// post-update, new slot is what's at position 0 in MRU order
				BC7Input_Get(&mtf_input, block, &best_st);
				bc7enc_find_endpoints_init_cache(&window_index_endpoint_cache[window_indices.mtf[0]], &best_st, &mtf_input);
			}

		#ifdef CHECK_EXTREMES_PRESERVED
			{
				RAD_ALIGN(U8, decomp_block[64], BC7_BLOCK_ALIGN);
				bc7_decode_block4x4a(decomp_block,output_bc7,infos[bi].flags);

				BC7Constraints dec_constraints = BC7Constraints::from_pixels(decomp_block);
				if (!dec_constraints.superset_of(infos[bi].constraints))
				{
					BC7BlockState st;

					bc7enc_state_from_bits(&st,output_bc7,infos[bi].flags); // need to use infos[bi].flags of that block when decoding to get correct endpoints_q
					U32 cmask = infos[bi].constraints.mask;

					rrprintf("subst extremes mismatch! mode=%d isb=%d rb=%d constraint mask=0x%08x\n", st.mode, st.subsets[0].isbit, st.subsets[0].rbit, cmask);
					//rrprintf("from_hash=0x%016llx\n", (long long)from_hash);
					rrprintf("bi=%d endpoint_i=%d indices_i=%d\n", bi, best_mtfwindow_endpoints_i, best_mtfwindow_indices_i);
					rrprintf("subset0 alpha: %02x-%02x\n",
						 st.subsets[0].endpoints[0].a, st.subsets[0].endpoints[1].a);
					rrprintf("preserve: a0=%d a255=%d\n", (infos[bi].flags & BC7ENC_PRESERVE_A0) != 0, (infos[bi].flags & BC7ENC_PRESERVE_A255) != 0);

					const int which_ind = (st.mode < 4 || st.mode > 5 || st.subsets[0].isbit) ? 0 : 1;
					const BC7Inds& aind = st.subsets[0].idxs[which_ind];

					for LOOP(i,16)
					{
						rrprintf("[%2d] orig=%02x dec=%02x ind=%x %c%c\n", i, infos[bi].colors.colors[i].u.a, decomp_block[i*4+3], aind.ind[i],
							" -"[(cmask >> i) & 1], " +"[(cmask >> (16 + i)) & 1]);
					}

					RR_ASSERT_ALWAYS(!"mismatch");
				}
			}
		#endif
			/*
			// log BEI block usage image :
			//  (on large images turn off threading)
			if ( (bi % 64) == 0 ) rrprintf("\n");
			if ( best_mtfwindow_endpoints_i >= 0 && best_mtfwindow_indices_i >= 0 ) rrprintf("M");
			else if ( best_mtfwindow_endpoints_i < 0 && best_mtfwindow_indices_i < 0 ) rrprintf("B");
			else if ( best_mtfwindow_indices_i < 0 ) rrprintf("E");
			else rrprintf("I");
			/**/
		}
		
		} // profile scope

		if (record_candidates_enabled)
		{
			for LOOPINT(bi, nblocks)
				record_candidate_block_from_infos(lambda, 6, to_blocks, infos, bi, infos[bi].flags); // validate 1
		}

		if (config.do_bottom_up_merge)
		{
			#if 1
			// @@!!
			// -> this is what causes the rmse jump on quaking lambda=1
			bc7rd_bottom_up_merge_indices(to_blocks,activity_blocks,infos,lambda,opt.flags);
			
			// rates were not updated by merge_indices
			//	fix that now by re-rating using "static" model (global VQ probability estimate)
			
			// @@ bc7rd_static_rater does a lot of the same stuff I do (in final matrix)
			//	the vc_indices construction is exactly the same, code dupe & redundant work
			bc7rd_recompute_rates_static_model(to_blocks,infos,lambda);

			if (record_candidates_enabled)
			{
				for LOOPINT(bi, nblocks)
					record_candidate_block_from_infos(lambda, 9, to_blocks, infos, bi, infos[bi].flags);
			}

			#else
			{
			// @@!!
			// attempt to fix bc7rd_bottom_up_merge_indices making a bad step
			//	on quaking alpha lambda=1
			
			// do bottom_up_merge to the side, save the blocks before
			// then do a J eval of the two options and take what's best
			
			// the better J too often goes with the non-merged
			//	but that seems to really hurt real world performance
			//	you have to apply a very large penalty to prefer the merged
			
			// this can fix the quaking alpha lambda=1 step
			
			BlockSurface merged_blocks = { };
			BlockSurface_AllocCopy(&merged_blocks,to_blocks);
			vector<bc7rd_blockinfo> merged_infos; // could skip this and have merge_indices not update infos
			merged_infos = infos;
			
			bc7rd_bottom_up_merge_indices(&merged_blocks,activity_blocks,merged_infos,lambda,opt.flags);

			bc7rd_static_rater rater;
			// using merged_blocks to build the rater strongly favors choosing them in the output
			//	but that's not strong enough, I need to add huge extra fudges to the rate!
			rater.init(&merged_blocks);
			
			//F32 one_over_lambda = 1.f / lambda;
			int nb = to_blocks->count;
			
			for LOOP(bi,nb)
			{
				U8 * to_blocks_ptr = BlockSurface_Seek(to_blocks,bi);
				bc7bits to_blocks_bits = bc7bits_load(to_blocks_ptr);
				
				
				// re-rate block :
				F32 to_blocks_R = rater.get_block_rate(to_blocks_bits);
				F32 to_blocks_D = infos[bi].D;
				F32 to_blocks_J = bc7rd_J(to_blocks_D,to_blocks_R,lambda);

				// update J for new rate :
				infos[bi].J = to_blocks_J;
				
				const U8 * merged_blocks_ptr = BlockSurface_SeekC(&merged_blocks,bi);
				bc7bits merged_blocks_bits = bc7bits_load(merged_blocks_ptr);
				
				if ( merged_blocks_bits != to_blocks_bits )
				{			
					// re-rate block :
					F32 merged_blocks_R = rater.get_block_rate(merged_blocks_bits);
					F32 merged_blocks_D = merged_infos[bi].D;
					F32 merged_blocks_J = bc7rd_J(merged_blocks_D,merged_blocks_R,lambda);
					
					//if ( merged_blocks_J < to_blocks_J ) // honest J compare : BAD!
					//if ( 1 ) // always take merged : GOOD! WTF
					// strong favor for taking merged blocks :
					//	if you don't strongly favor merged, results are way worse
					if ( merged_blocks_J < infos[bi].J + lambda * 512.f )
					{
						infos[bi].J = merged_blocks_J;
						infos[bi].D = merged_blocks_D;
						bc7bits_store(to_blocks_ptr,merged_blocks_bits);
						to_blocks_bits = merged_blocks_bits;
					}		
				}	
				
				// @@!! do original baseline too? (before recomp)
				//	at very low lambda this acts to make the step away from baseline smaller
				#if 1
				const U8 * baseline_block_ptr = BlockSurface_SeekC(baseline_blocks,bi);
				bc7bits baseline_block_bits = bc7bits_load(baseline_block_ptr);
				
				const ActivityBlock4x4 * pActivity = BlockSurface_SeekC_ActivityBlock4x4(activity_blocks,bi);
					
				if ( baseline_block_bits != to_blocks_bits )
				{			
					// re-rate block :
					F32 baseline_blocks_R = rater.get_block_rate(baseline_block_bits);
					F32 baseline_blocks_D = bc7rd_D(baseline_block_bits,infos[bi].colors,infos[bi].flags,*pActivity);
					// note this is *not* "baseline_D" in infos, that's after the recomp
					F32 baseline_blocks_J = bc7rd_J(baseline_blocks_D,baseline_blocks_R,lambda);
					
					// strong favor *against* taking baseline blocks :
					//at 128 (or less) quaking alpah lambda=1 keeps rmse at 0 exactly
					//if ( baseline_blocks_J < infos[bi].J - lambda * 128.f )
					//at 256 quaking alpah lambda=1 keeps rmse = small
					if ( baseline_blocks_J < infos[bi].J - lambda * 256.f )
					{
						infos[bi].J = baseline_blocks_J;
						infos[bi].D = baseline_blocks_D;
						bc7bits_store(to_blocks_ptr,baseline_block_bits);
					}		
				}	
				#endif			
			}			
			
			BlockSurface_Free(&merged_blocks);
			}
			#endif

		} // DO_BOTTOM_UP_MERGE
		else
		{
			// always need to do this
			bc7rd_recompute_rates_static_model(to_blocks,infos,lambda);
		}

		if (config.do_final_matrix)
		{
			bc7rd_final_matrix(dispatch,to_blocks,activity_blocks,infos,lambda,opt.flags,config);
			if (record_candidates_enabled)
			{
				for LOOPINT(bi, nblocks)
					record_candidate_block_from_infos(lambda, 11, to_blocks, infos, bi, infos[bi].flags);
			}
		}


		if (config.do_final_optimize)
		{
			SIMPLEPROFILE_SCOPE_N(final_optimize,nblocks);
	
			// @@ does order matter?
			//endpoints:
			bc7rd_final_optimize(to_blocks,activity_blocks,infos,opt.flags,lambda,false);

			//indices :
			bc7rd_final_optimize(to_blocks,activity_blocks,infos,opt.flags,lambda,true);

			// @@ then endpoints again? (that's what BC1RD does)
		}

		if (record_candidates_enabled)
		{
			for LOOPINT(bi, nblocks)
				// validate 2
				record_candidate_block_compute_D_unknown_R(lambda, 23, to_blocks, infos, bi, infos[bi].flags,*BlockSurface_SeekC_ActivityBlock4x4(activity_blocks,bi));
		}
	}

	/*
	F64 average_in_loop_matrix_hyperbola_count = (F64)stat_in_loop_matrix_hyperbola_count / nblocks;
	rrprintfvar(average_in_loop_matrix_hyperbola_count);
	F64 average_in_loop_matrix_decode_count = (F64)stat_in_loop_matrix_decode_count / nblocks;
	rrprintfvar(average_in_loop_matrix_decode_count);
	F64 average_in_loop_matrix_vqd_count = (F64)stat_in_loop_matrix_vqd_count / nblocks;
	rrprintfvar(average_in_loop_matrix_vqd_count);
	/**/

	#if 0
	// log post-RD mode histo :
	{
	BC7_ModeHisto histo;
	histogram_mode(&histo,to_blocks);
	rrprintf("mode histo = { %6.2f%%, %6.2f%%, %6.2f%%, %6.2f%%, %6.2f%%, %6.2f%%, %6.2f%%, %6.2f%% } (solid blocks excluded)\n",
		histo.count[0]*100.0/to_blocks->count,
		histo.count[1]*100.0/to_blocks->count,
		histo.count[2]*100.0/to_blocks->count,
		histo.count[3]*100.0/to_blocks->count,
		histo.count[4]*100.0/to_blocks->count,
		histo.count[5]*100.0/to_blocks->count,
		histo.count[6]*100.0/to_blocks->count,
		histo.count[7]*100.0/to_blocks->count);
	}
	#endif

	return true;
}

RR_NAMESPACE_END
