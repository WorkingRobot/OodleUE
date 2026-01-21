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

#include "rrsimpleprof.h"
//#include "rrsimpleprofstub.h"
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

// NOTE(fg): this assums no index+pbit grouping!!
#define USE_PREDECODE

#define BC7_BLOCK_ALIGN 32 // preferred alignment for decoded blocks.

#define FINAL_MATRIX_CATEGORIES_UNMIXED			(8*64)
#define FINAL_MATRIX_CATEGORIES_MIXED			(8)

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
		//#define HYPERBOLA_MAX_INDEX_PRODUCT	256
		//#define HYPERBOLA_MAX_INDEX_PRODUCT	512 // incremental gains and much slower
		true, 80,
		// do_bottom_up_merge
		true,
		// do_final_matrix, fm_mix_partitions
		true, true,
		// fm_combined_size_limit_sqrt
		// #define FINAL_MATRIX_SIZE_LIMIT_SQRT	100
		// #define FINAL_MATRIX_SIZE_LIMIT	(FINAL_MATRIX_SIZE_LIMIT_SQRT*FINAL_MATRIX_SIZE_LIMIT_SQRT)
		// #define FINAL_MATRIX_SIZE_LIMIT	20000
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
		//#define HYPERBOLA_MAX_INDEX_PRODUCT	256
		//#define HYPERBOLA_MAX_INDEX_PRODUCT	512 // incremental gains and much slower
		true, (128+16+3),
		// do_bottom_up_merge
		true,
		// do_final_matrix, fm_mix_partitions
		true, true,
		// fm_combined_size_limit_sqrt
		// #define FINAL_MATRIX_SIZE_LIMIT_SQRT	100
		// #define FINAL_MATRIX_SIZE_LIMIT	(FINAL_MATRIX_SIZE_LIMIT_SQRT*FINAL_MATRIX_SIZE_LIMIT_SQRT)
		// #define FINAL_MATRIX_SIZE_LIMIT	20000
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
		//#define HYPERBOLA_MAX_INDEX_PRODUCT	256
		//#define HYPERBOLA_MAX_INDEX_PRODUCT	512 // incremental gains and much slower
		true, 200,
		// do_bottom_up_merge
		true,
		// do_final_matrix, fm_mix_partitions
		true, true,
		// fm_combined_size_limit_sqrt
		// #define FINAL_MATRIX_SIZE_LIMIT_SQRT	100
		// #define FINAL_MATRIX_SIZE_LIMIT	(FINAL_MATRIX_SIZE_LIMIT_SQRT*FINAL_MATRIX_SIZE_LIMIT_SQRT)
		// #define FINAL_MATRIX_SIZE_LIMIT	20000
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
		//#define HYPERBOLA_MAX_INDEX_PRODUCT	256
		//#define HYPERBOLA_MAX_INDEX_PRODUCT	512 // incremental gains and much slower
		true, 200,
		// do_bottom_up_merge
		true,
		// do_final_matrix, fm_mix_partitions
		true, true,
		// fm_combined_size_limit_sqrt
		// #define FINAL_MATRIX_SIZE_LIMIT_SQRT	100
		// #define FINAL_MATRIX_SIZE_LIMIT	(FINAL_MATRIX_SIZE_LIMIT_SQRT*FINAL_MATRIX_SIZE_LIMIT_SQRT)
		// #define FINAL_MATRIX_SIZE_LIMIT	20000
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
		// do_final_matrix, fm_mix_partitions
		true, true,
		// fm_combined_size_limit_sqrt
		// #define FINAL_MATRIX_SIZE_LIMIT_SQRT	100
		// #define FINAL_MATRIX_SIZE_LIMIT	(FINAL_MATRIX_SIZE_LIMIT_SQRT*FINAL_MATRIX_SIZE_LIMIT_SQRT)
		// #define FINAL_MATRIX_SIZE_LIMIT	20000
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

struct bc7rd_blockinfo
{
	F32 baseline_D;
	F32	J,D;
	rrColorBlock4x4 colors; // actually RGBA
};	
	
struct bc7rd_windowentry
{
	BC7BlockState st;
	bc7bits encoded;
};

// warning on bc7_average_block_size_bits :
//	this is not super uniform per image, it can be quite different from one image to another
//	so just using some not-too-carefully gathered average is a bit suspect
static const F32 bc7_average_block_size_bits[8] = { 94.28325f,97.2345f,94.66392f,98.22575f,83.168f,84.9612f,79.677f,76.7255f };

static void bc7_decode_block4x4a(U8 * decomp_block, const void * bc7bits, BC7Flags flags)
{
	bc7_decode_block_fast(decomp_block,bc7bits,(flags & BC7ENC_IGNORE_ALPHA) != 0);
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

		// we don't want mode 5 (which is our mode of choice for solid-color blocks) to get
		// a boost just because there's many flat areas, since non-flat blocks are very
		// unlikely to benefit from this.
		//
		// NOTE(fg): tried this out while trying to diagnose a different issue; this
		// doesn't seem to be the cause, is often slightly better, but also ends up
		// noticeably worse on some textures. Not sure about this one, needs extra
		// investigation.
		//if ( bc7_is_encoded_single_color_block( baseline_block_ptr ) )
		//	continue;

		bc7bits bits = bc7bits_load(baseline_block_ptr);
		int m = bc7bits_get_mode(bits);
		
		phisto->count[ m ] += 1;
	}
}	

// test 04-07-2020 :
//	do we want to histo mode & part? or just mode?
//	(this is for the recomp bias)
// -> test indicates it's very MEH
//	 it's just a tiny random wiggle, you could go either way
#if 0

// histogram mode and part :
// (this will bias you towards using more of the most common partitions)
		
// fill ssd_err_bias with histogram
static void histogram_mode_part(BC7_ModePart_ErrBias * phisto,const BlockSurface * baseline_blocks)
{
	RR_ZERO(*phisto);
	
	for LOOP(bi,baseline_blocks->count)
	{
		const U8 * baseline_block_ptr = BlockSurface_SeekC(baseline_blocks,bi);
		
		bc7bits bits = bc7bits_load(baseline_block_ptr);
		int m = bc7bits_get_mode(bits);
		int p = bc7bits_get_part(bits,m);
		
		phisto->ssd_err_bias[ m ][ p ] += 1;
	}
}	
	
// ssd_err_bias_J == histo is okay
//	
static void histogram_to_ssd_err_bias_J(BC7_ModePart_ErrBias * ssd_err_bias_J, const BC7_ModePart_ErrBias & histo, F32 lambda)
{
	// to handle the zero frequency / escape problem :
	//	scale up real counts by 8
	//	add +1 for novel symbols
	// @@ alternatives REVISIT
	const int count_scale = 8;
	const int count_bias = 1;
			
	U32 tot = 0;
	for LOOP(m,8)
	{
		for LOOP(p,64)
		{
			U32 c = histo.ssd_err_bias[m][p] * count_scale + count_bias;
			tot += c;
		}
	}

	F32 log2_tot = rrlog2neg_bk_32(tot);
	
	for LOOP(m,8)
	{
		for LOOP(p,64)
		{
			U32 c = histo.ssd_err_bias[m][p] * count_scale + count_bias;
			F32 bits = rrlog2neg_bk_32(c) - log2_tot;
			ssd_err_bias_J->ssd_err_bias[m][p] = bits_to_ssd_err_bias_J(bits,lambda);
		}
	}
}

#else

	
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

#endif

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

static void record_activity_block(F32 lambda, const BlockSurface* to_blocks, int bi, const ActivityBlock4x4& activity)
{
	if (!record_candidates_enabled) return;
	const void* q = (void*)BlockSurface_SeekC(to_blocks, bi); // we use this to identify blocks so we can reassemble image from multiple chunks
	ActivityItem item = { 1, lambda, q };
	memcpy(item.activity, activity.values, sizeof(item.activity));
	record_entry(&item, sizeof(item));
}

static void record_candidate_block(F32 lambda, int phase, const BlockSurface* to_blocks, int bi, const rrColorBlock4x4 & out_color, int mode, F32 D, F32 R, int bset=-1)
{
	if (!record_candidates_enabled) return;
	const void *q = (void*) BlockSurface_SeekC(to_blocks, bi); // we use this to identify blocks so we can reassemble image from multiple chunks
	CandidateItem item = { 2, lambda, q, phase, mode, bset, D, R };
	memcpy(item.color_data, &out_color.colors[0], sizeof(item.color_data));
	record_entry(&item, sizeof(item));
}

static void record_input_block_as_candidate(F32 lambda, int phase, const BlockSurface* to_blocks, const vector<bc7rd_blockinfo> &infos, int bi)
{
	if (!record_candidates_enabled) return;
	record_candidate_block(lambda, phase, to_blocks, bi, infos[bi].colors, 255, 0., 512.);
}

static void record_candidate_block(F32 lambda, int phase, const BlockSurface* to_blocks, int bi, const U8 out_block[16], F32 D, F32 R, BC7Flags flags)
{
	if (!record_candidates_enabled) return;
	RAD_ALIGN(U8, decomp_block[64], BC7_BLOCK_ALIGN);
	bc7_decode_block4x4a(decomp_block, out_block, flags);
	const rrColorBlock4x4* pc = (const rrColorBlock4x4*)decomp_block;
	record_candidate_block(lambda, phase, to_blocks, bi, *pc, bc7bits_get_mode(bc7bits_load(out_block)), D, R);
}

static void record_candidate_block(F32 lambda, int phase, const BlockSurface* to_blocks, int bi, BC7BlockState st, F32 D, F32 R, BC7Flags flags)
{
	RAD_ALIGN(U8, decomp_block[64], BC7_BLOCK_ALIGN);
	bc7enc_decode_state(&st, decomp_block, flags);
	const rrColorBlock4x4* pc = (const rrColorBlock4x4*)decomp_block;
	record_candidate_block(lambda, phase, to_blocks, bi, *pc, st.mode, D, R);
}

// this function takes the D and J values from infos[] and the colors from to_blocks;
static void record_candidate_block_from_infos(F32 lambda, int phase, const BlockSurface* to_blocks, const vector<bc7rd_blockinfo> &infos, int bi, BC7Flags flags)
{
	const void* out_block = (void*)BlockSurface_SeekC(to_blocks, bi);
	RAD_ALIGN(U8, decomp_block[64], BC7_BLOCK_ALIGN);
	bc7_decode_block4x4a(decomp_block, out_block, flags);
	const rrColorBlock4x4* pc = (const rrColorBlock4x4*)decomp_block;
	record_candidate_block(lambda, phase, to_blocks, bi, *pc, bc7bits_get_mode(bc7bits_load((const U8*)out_block)), infos[bi].D, (infos[bi].J - infos[bi].D) / lambda);
}

// this function recomputes D from the output BC7 block, and doesn't know what R is.
static void record_candidate_block_compute_D_unknown_R(F32 lambda, int phase, const BlockSurface* to_blocks, const vector<bc7rd_blockinfo> &infos, int bi, BC7Flags flags, const ActivityBlock4x4& activity, int set = -1)
{
	const rrColorBlock4x4& in_color = infos[bi].colors;
	const void* out_block = (void*)BlockSurface_SeekC(to_blocks, bi);
	RAD_ALIGN(U8, decomp_block[64], BC7_BLOCK_ALIGN);
	bc7_decode_block4x4a(decomp_block, out_block, flags);
	const rrColorBlock4x4* pc = (const rrColorBlock4x4*)decomp_block;
	F32 D = VQD(in_color, *pc, activity);
	record_candidate_block(lambda, phase, to_blocks, bi, *pc, bc7bits_get_mode(bc7bits_load((const U8*)out_block)), D, -1.0f, set);
}

static int seed_past_blocks(vector<bc7rd_windowentry>& past_block_window,const BlockSurface * baseline_blocks,BC7Flags flags)
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
	
	counting.resize( RR_MIN(counting.size(),past_block_window.size()) );
	
	// remove singletons :
	// maybe remove 2's also ?
	//	-> no that's really bad on "decanter" , otherwise meh, sometimes a slight win
	while( ! counting.empty() && counting.back().count == 1 )
	//while( ! counting.empty() && counting.back().count <= 2 )
		counting.pop_back();
		
	int num_seeds = counting.size32();
	
	#if 0
	// the seeds we have now have zeroed indices
	// I don't really want that in my seeds
	// go back and find the last occurance of each
	//		(last to reduce the risk of early blocks seeing themselves)
	// silly slow way to do this, should carry it through
	// this is unnecessary now that I'm only seeding endpoints; yes it's okay to not do this
	
	// note logging will look messy when threaded, use --w1
	//rrprintfvar(num_seeds);
	for LOOP(s,num_seeds)
	{
		// log when count changes :
		//if ( s == 0 || counting[s].count != counting[s-1].count )
		//	rrprintf("%d : %d\n",s,counting[s].count);
	
		bool found = false;
		for LOOPBACK(bi,baseline_blocks->count)
		{
			const U8 * baseline_block_ptr = BlockSurface_SeekC(baseline_blocks,bi);
		
			BC7BlockState baseline_st;
			bc7enc_state_from_bits(&baseline_st,baseline_block_ptr);
		
			bc7bits encoded;
			bc7enc_emit_zero_indices(encoded.ptr(),&baseline_st);
			
			if ( encoded == counting[s].val )
			{
				found = true;
				past_block_window[s] = baseline_st;
				break;
			}
		}
		RR_ASSERT_ALWAYS( found );
	}
	#else
	
	// counting[s] has indices zeroed
	//	but that's okay because we're only seeding endpoints
	// read the encoded blocks back out to BC7BlockStates :
	
	for LOOP(s,num_seeds)
	{
		past_block_window[s].encoded = counting[s].val;
		
		bc7enc_state_from_bits(&past_block_window[s].st,bc7bits_U8ptr(&counting[s].val), flags);
	}
	#endif
	
	// in MTF scheme, [0]th entry is the most favored
	//	 that's where we put the highest count, so order is good
	
	return num_seeds;
}
	
static void bc7_update_mtf_window(const bc7rd_windowentry * new_state,int lz_found_i,vector<bc7rd_windowentry *>& lz_window, int & lz_window_size)
{
	RR_ASSERT( lz_window_size <= lz_window.size32() );
	int move_to = 0;
	
	if ( lz_found_i < 0 )
	{
		// add it :
		if ( lz_window_size < lz_window.size32() )
			lz_window_size++;
		
		lz_found_i = lz_window_size-1;
		
		// copy it in :
		*( lz_window[lz_found_i] ) = *new_state;
	}
	else
	{
		// lz_found_i was used
		// note that *lz_window[lz_found_i] != *new_state
		// new_state is the actual encoding, with chosen indices for this block
		// they should have the same endpoints, so it shouldn't matter if you leave
		//	the old state in or stomp the new one
		
		// test this should be a nop : (confirmed)
		//*( lz_window[lz_found_i] ) = *new_state;
	}

	////NOTE(fg): this one's interesting
	//move_to = lz_found_i / 4;

	// MTF :
	RR_ASSERT_ALWAYS( move_to <= lz_found_i);
	bc7rd_windowentry * new_head = lz_window[lz_found_i]; 
	memmove(&lz_window[move_to+1],&lz_window[move_to],(lz_found_i-move_to)*sizeof(lz_window[0]));
	lz_window[move_to] = new_head;
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

// pbits in the final optimize sets?
//	 (and if they are, then you can change them)
// -> seems to be just a small difference either way
#define FINAL_OPTIMIZE_CHANGE_PBITS_BOOL	0

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

static bc7bits bc7rd_final_optimize_blockset_indices(
	int mode,
	const BlockSurface * bc7_blocks,
	const BlockSurface * activity_blocks,
	const vector<bc7rd_blockinfo> & infos,
	BC7Flags flags,
	const bc7bits_and_count * set_indexes,int set_size,
	const bc7bits & bits_mask)
{
	// blockset currently has all the same {indexes+pbits}
	//	but those may not be best for the set (keeping rest of the block the same)
	// find new candidate best {indexes+pbits}

	const int NUM_CLASSES = 14; // for 3-subset; 2-subset needs 4, 1-subset is 1
	const int ns = bc7_modes[mode].ns;
	int cls_counts[NUM_CLASSES] = {};
	int cur_max_cls = 0;
	int cur_max_count = 0;
	int cls_repr = 0;

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
	}

	//if (cls_counts[cur_max_cls] != set_size)
	//	rrprintf("(%d,%d) covers %d/%d (%.1f%%)\n", mode, cur_max_cls, cls_counts[cur_max_cls], set_size, 100.0 * cls_counts[cur_max_cls] / set_size);

	// Loop over blocks in largest equivalence class and accumulate their errors
	BC7IndexErrors errs[2];
	errs[0].init();
	errs[1].init();

	for LOOP(i,set_size)
	{
		int bi = set_indexes[i].count;
		const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);

		int part = bc7_read_partition_id(block_ptr, mode);
		int cls = radtex_anchor_eqv[ns-1][part];
		if (cls != cur_max_cls)
			continue;

		BC7BlockState st;
		bc7enc_state_from_bits(&st,block_ptr,flags);

		BC7Input input;
		BC7Input_Get(&input,(const U8 *)&(infos[bi].colors),&st);

		// NOTE: no support for changing pbits here, could add this but it
		// was disabled before anyway

		bc7enc_accumulate_index_errors(errs,&st,&input,flags);
	}

	// Pick the indices for the class representative block
	const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,cls_repr);
	BC7BlockState st;
	bc7enc_state_from_bits(&st,block_ptr,flags);

	//bc7bits old_bits = bc7bits_load(block_ptr);
	//BC7BlockState check_st = st;

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
		bc7enc_reindex_block(&reindex_st,input.subsets,flags,false,false,true);

		BC7Error reindex_err = bc7enc_calc_error(&reindex_st,(const U8 *)&infos[cls_repr].colors,flags);
		RR_ASSERT(accum_err == reindex_err);
	}*/

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

static bc7bits bc7rd_final_optimize_blockset_endpoints(
	int mode,
	const BlockSurface * bc7_blocks,
	const BlockSurface * activity_blocks,
	const vector<bc7rd_blockinfo> & infos,
	BC7Flags flags,
	const bc7bits_and_count * set_indexes,int set_size,
	const bc7bits & bits_mask)
{
	// blockset currently has all the same {endpoints+pbits}
	//	but those may not be best for the set (keeping rest of the block the same)
	// find new candidate best {endpoints+pbits}
	BC7BlockState st;
	
	BC7MultiBlocks multi(set_size);
	for LOOP(i,set_size)
	{
		int bi = set_indexes[i].count;
		multi.set(i,BlockSurface_SeekC(bc7_blocks,bi),(const U8 *)&(infos[bi].colors),flags);
	}

	// Solve for the new endpoints
	int bi = set_indexes[set_size/2].count; // just use a random block to start with
	const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);
	bc7enc_state_from_bits(&st,block_ptr,flags);

	bool change_pbits = FINAL_OPTIMIZE_CHANGE_PBITS_BOOL;
	bool slow_anneal = true; // <- helps nicely ; fast enough?
	bc7enc_find_endpoints_multi(&st,&multi,flags,change_pbits,slow_anneal);

	bc7bits output_bc7;
	bc7enc_emit(bc7bits_U8ptr(&output_bc7),&st,flags);

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
	
	int tot_num_blocks = bc7_blocks->count;

	vector<bc7bits_and_count> bits_and_index;
	bits_and_index.reserve(tot_num_blocks);
	
	for LOOP(mode,8)
	{
	
		bits_and_index.clear();
		
		bc7bits bits_mask;
		// with pbits or not ?
		//	note that with bits on, the masks overlap
		//	so when you do the two different phases (ind,end) they can undo each others work
		//	if you don't include pbits they are orthogonal
		#if FINAL_OPTIMIZE_CHANGE_PBITS_BOOL
		if ( do_indices ) bits_mask = c_bc7bitrange_indices_with_pbits_mask[mode];
		else bits_mask = c_bc7bitrange_endpoints_with_pbits_mask[mode];
		#else
		if ( do_indices ) bits_mask = c_bc7bitrange_indices_mask[mode];
		else bits_mask = c_bc7bitrange_endpoints_mask[mode];
		#endif
		
		for LOOP(bi,tot_num_blocks)
		{
			const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);		
			bc7bits bits = bc7bits_load(block_ptr);
		
			int block_mode = bc7bits_get_mode(bits);
			if ( block_mode != mode )
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
				new_bits = bc7rd_final_optimize_blockset_endpoints(mode,bc7_blocks,activity_blocks,infos,flags,set_indexes,set_size,bits_mask);
			
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
						bc7bits bits = bc7bits_load(block_ptr);
						bits = bc7bits_andnot(bits,bits_mask);
						bits = bc7bits_or(bits,new_bits);
						bc7bits_store(block_ptr,bits);
						if (record_candidates_enabled)
							record_candidate_block_compute_D_unknown_R(lambda, do_indices ? 21 : 20, bc7_blocks, infos, bi, flags, *(const ActivityBlock4x4*) BlockSurface_SeekC(activity_blocks,bi), begin);
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
	F32 two_count_rate = rrlog2neg_bk_32(2) - log2_denom;
	two_count_rate += portion_bits/2;
	RR_ASSERT( escape_rate >= two_count_rate + 0.1f );
	// escapes must be worse than two-counts !
	//escape_rate = RR_MAX(escape_rate,two_count_rate + 0.1f);
	#endif
	
	m_escape_code_len = escape_rate;
}
	
void bc7rd_static_rater::init(const BlockSurface * bc7_blocks)
{
	int num_blocks = bc7_blocks->count;

	vector<bc7bits> v_indices[BC7_NUM_MODES];
	vector<bc7bits> v_indices_pb[BC7_NUM_MODES];
	vector<bc7bits> v_endpoints[BC7_NUM_MODES];
	vector<bc7bits> v_endpoints_pb[BC7_NUM_MODES];
	
	for LOOP(bi,num_blocks)
	{
		const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);
	
		bc7bits bits = bc7bits_load(block_ptr);
				
		int mode = bc7bits_get_mode(bits);
				
		const bc7bits mask_ind(    c_bc7bitrange_indices_mask[mode] );
		const bc7bits mask_ind_pb( c_bc7bitrange_indices_with_pbits_mask[mode] );

		v_indices[   mode].push_back( bc7bits_and(bits,mask_ind) );
		v_indices_pb[mode].push_back( bc7bits_and(bits,mask_ind_pb) );

		// header and endpoints :
		v_endpoints[   mode].push_back( bc7bits_andnot(bits, mask_ind_pb ) );
		v_endpoints_pb[mode].push_back( bc7bits_andnot(bits, mask_ind ) );
	}	
	
	vector<bc7bits_and_count>  vc_indices;
	vector<bc7bits_and_count>  vc_indices_pb;
	vector<bc7bits_and_count>  vc_endpoints;
	vector<bc7bits_and_count>  vc_endpoints_pb;
	
	int max_mode_block_count = bc7bits_vectors_max_size(v_indices,BC7_NUM_MODES);
	
	vc_indices.reserve( max_mode_block_count );
	vc_indices_pb.reserve( max_mode_block_count );
	vc_endpoints.reserve( max_mode_block_count );
	vc_endpoints_pb.reserve( max_mode_block_count );
		
	for LOOP(mode,BC7_NUM_MODES)
	{
		// size of all the v_ should be the # of blocks in that mode
		int mode_block_count = v_indices[   mode].size32();
		RR_ASSERT( mode_block_count <= max_mode_block_count );

		// zero frequency problem for mode? just +1 bias
		int mode_histo_count = mode_block_count + 1;
		int mode_histo_tot = num_blocks + BC7_NUM_MODES; // + BC7_NUM_MODES for the +1 on each mode
		
		m_mode_rates[mode] = rrlog2neg_bk_32( mode_histo_count) - rrlog2neg_bk_32( mode_histo_tot );
		
		m_raters[mode].m_mode = mode;
	
		// sort_and_count_uniques clears the vecs
		//vc_indices.clear();
	
		sort_and_count_uniques(&vc_indices,v_indices[mode]);
		sort_bc7bits_and_count_compare_count_highest_first(&vc_indices);
		
		sort_and_count_uniques(&vc_indices_pb,v_indices_pb[mode]);
		sort_bc7bits_and_count_compare_count_highest_first(&vc_indices_pb);
	
		sort_and_count_uniques(&vc_endpoints,v_endpoints[mode]);
		sort_bc7bits_and_count_compare_count_highest_first(&vc_endpoints);
		
		sort_and_count_uniques(&vc_endpoints_pb,v_endpoints_pb[mode]);
		sort_bc7bits_and_count_compare_count_highest_first(&vc_endpoints_pb);
		
		// if you sum all the counts in the vc's
		//	it should == mode_histo_count;
		
		int ind_bits  = c_bc7_total_bits_indices[mode];
		int pbit_bits = c_bc7_total_bits_pbits[mode];
		int mode_bits = mode+1;
		RR_ASSERT( c_bc7_total_bits_header[mode] >= mode_bits );
		int end_bits = 128 - ind_bits - pbit_bits - mode_bits;
		// end_bits is end + header , but not counting mode
		//	because we already sent mode using histogram model
		RR_ASSERT( end_bits >= c_bc7_total_bits_endpoints[mode] );

		m_raters[mode].m_ind_rate.init( vc_indices, ind_bits, mode_block_count );
		m_raters[mode].m_ind_pb_rate.init( vc_indices_pb, ind_bits + pbit_bits, mode_block_count );
		
		m_raters[mode].m_end_rate.init( vc_endpoints, end_bits, mode_block_count );
		m_raters[mode].m_end_pb_rate.init( vc_endpoints_pb, end_bits + pbit_bits, mode_block_count );
		
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
	SIMPLEPROFILE_SCOPE(bc7rd_recompute_rates);

	bc7rd_static_rater rater;
	rater.init(bc7_blocks);
	
	//F32 one_over_lambda = 1.f / lambda;
	int nb = bc7_blocks->count;
	
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
	SIMPLEPROFILE_SCOPE(final_matrix);
		
	F32 one_over_lambda = 1.f / lambda;
	int nb = bc7_blocks->count;
	
	vector<bc7bits_and_count> combined;
	combined.reserve(nb);
	
	{
	//SIMPLEPROFILE_SCOPE(final_matrix_make);
	
	// you can do {I|EP} or {IP|E}
	// just doing one or the other does seem to be okay
	// and I|EP seems better than IP|E
	// mainly on red_blue
	// there's not a big difference
	
	vector<vector<bc7bits> > v_indices;
	vector<vector<bc7bits> > v_endpoints_pb;

	int final_matrix_categories = config.fm_mix_partitions ?
		FINAL_MATRIX_CATEGORIES_MIXED :
		FINAL_MATRIX_CATEGORIES_UNMIXED;
	v_indices.resize(final_matrix_categories);
	v_endpoints_pb.resize(final_matrix_categories);

	for LOOP(bi,nb)
	{
		const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);	
		bc7bits bits = bc7bits_load(block_ptr);
		int mode = bc7bits_get_mode(bits);

		int category = config.fm_mix_partitions ?
			mode :
			mode + 8*bc7bits_get_part(bits,mode);

		RR_ASSERT( category < final_matrix_categories );

		v_indices[     category].push_back( bc7bits_extract_indices(bits,false) );
		v_endpoints_pb[category].push_back( bc7bits_xor_assert_on(bits, v_indices[category].back() ) );
	}

	// log of number of blocks in {mode,part} :
	//	very clumpy
	//	there are usually 2 or 3 {mode,part} categories that have almost all the blocks
	//{ 57 (0.7%),56 (0.7%),777 (9.5%),2677 (32.7%),3938 (48.1%),22 (0.3%),34 (0.4%),32 (0.4%),29 (0.4%),22 (0.3%),36 (0.4%),166 (2.0%),36 (0.4%),38 (0.5%),34 (0.4%),38 (0.5%),}

	vector<bc7bits_and_count>  vc_indices;
	vector<bc7bits_and_count>  vc_endpoints_pb;
	
	vc_indices.reserve( bc7bits_vectors_max_size(v_indices.data(),final_matrix_categories) );
	vc_endpoints_pb.reserve( bc7bits_vectors_max_size(v_endpoints_pb.data(),final_matrix_categories) );
		
	//rrprintf("\n{ ");
	for LOOP(cat,final_matrix_categories)
	{
		//int nofc = v_indices[cat].size32();
		//if ( nofc > 20 ) rrprintf("%d (%.1f%%),",nofc,100.0*nofc/nb);
	
		// sort_and_count_uniques clears the vecs
		//vc_indices.clear();
	
		sort_and_count_uniques(&vc_indices,v_indices[cat]);
		sort_bc7bits_and_count_compare_count_highest_first(&vc_indices);
		
		sort_and_count_uniques(&vc_endpoints_pb,v_endpoints_pb[cat]);
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
				
				combined.push_back();
				combined.back().count = product;
				combined.back().val = bc7bits_or_assert_exclusive( vc_indices[vc_indices_i].val , vc_endpoints_pb[vc_endpoints_pb_i].val );
			}
		}
	}
	
	} // final_matrix_make
	//rrprintf("}\n");
	
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
	
	for LOOP(matrix_i,matrix_size)
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
	SIMPLEPROFILE_SCOPE(final_matrix_core);

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

				st->must_beat_sad = matrix_sad;

				// SAD went down, evaluate this block :

				F32 matrix_D = VQD(colors, matrix_decoded[matrix_i],*pActivity);
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

	F32 distortion_sum;
	U32 count;
	U32 count_log2_count;
	S32 block_link; // linked list of blocks with these indices
	S32 merged_onto; // negative if not merged
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
	BC7Flags flags)
{
	bc7bits encoding = bc7bits_or_assert_exclusive( blocks[bi].encoding_without_indices, new_indices );
	RAD_ALIGN(U8, decoded[64], BC7_BLOCK_ALIGN);
	bc7_decode_block4x4a(decoded,&encoding,flags);
	F32 D = bc7rd_D(decoded,infos[bi].colors,flags,*blocks[bi].activity);
	return D;
}

static F32 bc7rd_single_block_index_change_distortion_SSD(int bi,
	const vector<bc7rd_index_vq_block> & blocks,
	const vector<bc7rd_blockinfo> & infos,
	const bc7bits & new_indices,
	BC7Flags flags)
{
	bc7bits encoding = bc7bits_or_assert_exclusive( blocks[bi].encoding_without_indices, new_indices );
	RAD_ALIGN(U8, decoded[64], BC7_BLOCK_ALIGN);
	bc7_decode_block4x4a(decoded,&encoding,flags);
	
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
	F32 must_beat_D = LARGE_D_F32)
{
	F32 D_sum = 0.f;
		
	while( bi >= 0 )
	{
		F32 D = bc7rd_single_block_index_change_distortion(bi,blocks,infos,new_indices,flags);
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

#ifdef USE_PREDECODE

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

	// Initialize the index cache
	predecoded.init_indices(mode, (const U8 *)&entries[0].indices,sizeof(entries[0]),entries.size());

	for (int fm = first_singleton; fm < entries_size; fm++)
	{
		F32 fm_base_distortion = entries[fm].distortion_sum;

		int fm_bi = entries[fm].block_link;
		RR_DURING_ASSERT( F32 check_D = bc7rd_single_block_index_change_distortion(fm_bi, blocks, infos, entries[fm].indices,flags ) );
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

#else

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

	// VQD ~ 2*SSD
	const F32 vqd_from_ssd = 2.0f;
	const F32 ssd_from_vqd = 1.0f / vqd_from_ssd;

	for (int fm = first_singleton; fm < entries_size; fm++)
	{
		F32 fm_base_distortion = entries[fm].distortion_sum;
		
		int fm_bi = entries[fm].block_link;
		RR_DURING_ASSERT( F32 check_D = bc7rd_single_block_index_change_distortion(fm_bi, blocks, infos, entries[fm].indices,flags ) );
		RR_ASSERT( fequal(check_D, fm_base_distortion, 0.1f) );
		
		// initializing with max_distortion_increase or not is meh
		//F32 best_distortion = LARGE_D_F32;
		F32 best_distortion = (F32)(fm_base_distortion + max_distortion_increase);
		U32 best_distortion_ssd = (U32)ceil(best_distortion * ssd_from_vqd); // conservative

		bc7bits fm_indices = entries[fm].indices;

		bc7bits fm_encoding_without_indices = blocks[fm_bi].encoding_without_indices;
		const rrColorBlock4x4 * fm_colors = &(infos[fm_bi].colors);

		RR_ASSERT( mode == bc7bits_get_mode(fm_encoding_without_indices) );
		int fm_part = bc7bits_get_part(fm_encoding_without_indices,mode);
		fm_part;
		
		// how to speed this up idea : 3 stage decode
		//
		// @@ decode endpoints here
		//	can look up part table and so on

		RR_NOP();

		// NOTE: we set things up so our early reject threshold is on the SSD values which are integers,
		// which saves a bit of work on the critical path.
		
		for LOOPVEC(to,entries)
		{
			if ( fm == to ) continue;
			
			// try changing fm sets' indices to "to" :
			
			// @@ unpack "to_indices" to U8x16
			//	do an early out check against fm_indices
			// -> this is the big advantage of the 3 stage decode
			//	that we could do an index-distance early out here
			
			bc7bits to_indices = entries[to].indices;
			RR_ASSERT( fm_indices != to_indices );

			bc7bits encoding = bc7bits_or_assert_exclusive(fm_encoding_without_indices, to_indices );
			
			// decode to_indices (with pbits) in the context of fm block mode & part
			// @@ use previously decoded fm endpoints & unpacked to_indices
			
			// should be the same as :
			//F32 new_index_D = bc7rd_single_block_index_change_distortion(fm_bi,blocks,infos,to_indices);

			RAD_ALIGN(U8, decoded[64], BC7_BLOCK_ALIGN);
			bc7_decode_block4x4a(decoded,&encoding,flags);
		
			// RGBA vs BGRA order doesn't matter, it's 4-channel difference :
			RR_COMPILER_ASSERT( sizeof(rrColorBlock4x4) == 64 );
			const rrColorBlock4x4 * pc = (const rrColorBlock4x4 *)decoded;
			
			U32 ssd = ColorBlock4x4_ComputeSSD_RGBA(*pc,*fm_colors);
		
			// entries are sorted by count so I only have to look at J when D gets better
			if ( ssd <= best_distortion_ssd )
			{
				F32 new_index_D = vqd_from_ssd * ssd;

				RR_ASSERT( new_index_D < LARGE_D_F32 );
				best_distortion = new_index_D;
				best_distortion_ssd = ssd;
				
				// make D the delta :
				F32 delta_D = new_index_D - fm_base_distortion;
				
				F32 dj = bc7rd_index_merge_dj( entries[fm], entries[to], delta_D , lambda, index_bits_this_mode); //,nblocks);
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

#endif
		
static U32 bc7rd_total_ssd(BlockSurface * bc7_blocks,
	vector<bc7rd_blockinfo> & infos, BC7Flags flags)
{
	int nb = bc7_blocks->count;

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
	SIMPLEPROFILE_SCOPE(bottom_up_merge);
		
	int nb = bc7_blocks->count;

	vector<bc7bits_and_count> v_indices_pb[BC7_NUM_MODES];
	
	vector<bc7rd_index_vq_block> blocks;
	blocks.resize(nb);
	
	for LOOP(bi,nb)
	{
		const U8 * block_ptr = BlockSurface_SeekC(bc7_blocks,bi);
		bc7bits bits = bc7bits_load(block_ptr);
		
		int mode = bc7bits_get_mode(bits);
		
		v_indices_pb[mode].push_back();

		// merge {I+P} sets
		//	 so they are most likely to form good matches
		// note this may break {E+P} matches that we previously made
		// in practice this is a lot better than just doing {I} here
		//bool with_pbits = true;
		// NOTE(fg): doesn't seem to make much of a difference with the current code, and {I+P} is a pain
		// in the ass for predecode, so let's do without for now
		const bool with_pbits = false;
#ifdef USE_PREDECODE
		RR_COMPILER_ASSERT(!with_pbits);
#endif
		v_indices_pb[mode].back().val = bc7bits_extract_indices(bits,with_pbits);
		v_indices_pb[mode].back().count = bi;
		
		blocks[bi].link = -1;
		blocks[bi].encoding_without_indices = bc7bits_xor_assert_on( bits , v_indices_pb[mode].back().val );
		
		const ActivityBlock4x4 * pActivity = BlockSurface_SeekC_ActivityBlock4x4(activity_blocks,bi);
				
		blocks[bi].activity = pActivity;
	}
		
	vector<bc7rd_index_vq_entry> entries;
	entries.reserve(nb);
	vector<bc7rd_index_vq_heap_entry> heap;
	heap.reserve(nb);
		
	for LOOP(mode,BC7_NUM_MODES)
	{
		if ( v_indices_pb[mode].size() <= 1 ) continue;
		
		sort_bc7bits_and_count_compare_bc7bits(&v_indices_pb[mode]);
	
		// find runs of same val
		
		entries.clear();
		
		for LOOPVEC(vi,v_indices_pb[mode])
		{
			int bi = v_indices_pb[mode][vi].count;
			RR_ASSERT( blocks[bi].link == -1 );
				
			if ( infos[bi].J == 0.f ) // single color block, don't touch
				continue;
			
			bc7bits indices = v_indices_pb[mode][vi].val;
			
			F32 D;
			#if bc7rd_single_block_index_change_distortion_is_vqd
			D = infos[bi].D;
			RR_ASSERT( fequal(D,bc7rd_single_block_index_change_distortion(bi,blocks,infos,indices),0.1f) );
			#else
			D = bc7rd_single_block_index_change_distortion(bi,blocks,infos,indices,flags);
			#endif
				
			if ( ! entries.empty() && entries.back().indices == v_indices_pb[mode][vi].val )
			{
				//add on
				bc7rd_index_vq_entry & entry = entries.back();
				blocks[bi].link = entry.block_link;
				entry.block_link = bi;
				
				entry.count ++;
				entry.distortion_sum += D;
			}
			else
			{
				// start new entry
				entries.push_back();
				bc7rd_index_vq_entry & entry = entries.back();
				entry.indices = indices;
				entry.block_link = bi;
				entry.count = 1;
				entry.distortion_sum = D;
				entry.merged_onto = -1;
			}
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
		const int entry_chunk_size = (16 * 1024) / sizeof(entries[0]);

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

				RR_DURING_ASSERT( F32 check_D = bc7rd_block_link_distortion( entries[fm].block_link , blocks, infos, entries[fm].indices, flags ) );
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
					F32 new_index_D = bc7rd_block_link_distortion( entries[fm].block_link , blocks, infos, entries[to].indices,flags, best_distortion );

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

						// make D the delta :
						F32 delta_D = new_index_D - fm_base_distortion;

						F32 dj = bc7rd_index_merge_dj( entries[fm], entries[to], delta_D , lambda, index_bits_this_mode); //,nblocks);
						if ( dj > 0 )
						{
							// make a heap entry :
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
				
				RR_DURING_ASSERT( F32 check_D = bc7rd_block_link_distortion( entries[fm].block_link , blocks, infos, entries[fm].indices,flags ) );
				RR_ASSERT( fequal(check_D, fm_base_distortion, 0.1f) );
			
				F32 must_beat_D = (F32)(fm_base_distortion + max_distortion_increase);
			
				// try changing indices to "to" :
				F32 new_index_D = bc7rd_block_link_distortion( entries[fm].block_link , blocks, infos, entries[to].indices,flags, must_beat_D );
				
				if ( new_index_D < LARGE_D_F32 )
				{				
					// make D the delta :
					F32 delta_D = new_index_D - fm_base_distortion;
					
					F32 dj = bc7rd_index_merge_dj( entries[fm], entries[to], delta_D , lambda, index_bits_this_mode); //,nblocks);

					if ( dj > 0 )
					{
						// make a heap entry :
						heap.push_back();
						heap.back().fm = fm;
						heap.back().fm_count_save = entries[fm].count;
						heap.back().fm_distortion_onto = new_index_D;
						heap.back().to = to;
						heap.back().dj = dj;
						push_heap(heap.begin(),heap.end());
					}
				}
				
				continue;
			}
			
			// fm and to are both alive
			// do the merge
			
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
			U32 link_count = 1;
			while( blocks[link].link >= 0 )
			{
				link_count++;
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
			
			RR_DURING_ASSERT( F32 check_distortion_sum = bc7rd_block_link_distortion(entries[to].block_link,blocks,infos,entries[to].indices,flags) );
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
	SIMPLEPROFILE_SCOPE(bc7rd);

	//rrprintf("BC7_RD: effort level=%d (%s)\n", rdoptions.effort, rrDXTCLevel_GetName(rdoptions.effort));
	
	RR_ASSERT( to_blocks->pixelFormat == rrPixelFormat_BC7 );
	RR_ASSERT( baseline_blocks->pixelFormat == rrPixelFormat_BC7 );
	RR_ASSERT( from_blocks->pixelFormat == rrPixelFormat_R8G8B8A8 );
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

	const int nblocks = from_blocks->count;
	
	RR_ASSERT( nblocks <= 16*1024 );
	// 16K blocks per 256K LZ chunk
	// we further slice that to 4K blocks per slice
	//	so typically nblocks = 4K
	
	vector<bc7rd_blockinfo> infos;
	infos.resize(nblocks);

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

		vector<bc7rd_windowentry> past_block_endpoints;
		vector<bc7rd_windowentry> past_block_indices;
		vector<bc7rd_windowentry *> mtfwindow_endpoints;
		vector<bc7rd_windowentry *> mtfwindow_indices;

		past_block_endpoints.resize(config.mtf_window_endpoints_size);
		past_block_indices.resize(config.mtf_window_indices_size);
		mtfwindow_endpoints.resize(config.mtf_window_endpoints_size);
		mtfwindow_indices.resize(config.mtf_window_indices_size);

		int mtfwindow_endpoints_size = 0;
		int mtfwindow_indices_size = 0;

		// mtf pointers instead of whole BC7BlockStates :
		for LOOPVEC(i,past_block_endpoints)
		{
			mtfwindow_endpoints[i] = & past_block_endpoints[i];
		}
		for LOOPVEC(i,past_block_indices)
		{
			mtfwindow_indices[i] = & past_block_indices[i];
		}
		
		if (config.mtf_window_seed_most_common) // @@ ??
		{
			// seed block window with most common from baseline ?
			// -> currently pretty meh
			//  does smooth out a nasty wiggle on "decanter"
			//	definitely slightly worse on "wood_worn" and "frymire" , nop on test6 & test7
			mtfwindow_endpoints_size = seed_past_blocks(past_block_endpoints,baseline_blocks,opt.flags);
		}
		
		// get block usage in the baseline and favor using more common mode/part :
		BC7_ModeHisto histo_mode;
		histogram_mode(&histo_mode,baseline_blocks);
		BC7_Mode_ErrBias errbias;
		histogram_to_ssd_err_bias_J(&errbias,histo_mode,lambda);
		
		BC7_Mode_ErrBias expected_size_errbias = { };
		for LOOP(m,BC7_NUM_MODES)
		{
			// note bc7_average_block_size_bits does not include cost of sending mode
			//	because it's measured by forcing all modes same (therefore entropy of mode = 0)
			// so adding on histo cost of mode gives you a full block cost
			// "errbias" is a J in units of SSD
			U32 mode_errb = bits_to_ssd_err_bias_J( bc7_average_block_size_bits[m], lambda );
			expected_size_errbias.ssd_err_bias[m] = mode_errb;
			expected_size_errbias.ssd_err_bias[m] += errbias.ssd_err_bias[m];
		}
		
		dword_and_count mode_and_count_sorted[BC7_NUM_MODES];
		for LOOP(m,BC7_NUM_MODES)
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
		SIMPLEPROFILE_SCOPE(bc7rd_core); // primary loops
	
		for LOOP(bi,nblocks)
		{
			const U8 * baseline_block_ptr = BlockSurface_SeekC(baseline_blocks,bi);
			U8 * output_bc7 = BlockSurface_Seek(to_blocks,bi);
			const U8 * fmPtr = BlockSurface_SeekC(from_blocks,bi);
				
			const ActivityBlock4x4 * pActivity = BlockSurface_SeekC_ActivityBlock4x4(activity_blocks,bi);
				
			// copy pixels : (RGBA)
			rrColorBlock4x4 in_colors;
			RR_ASSERT( from_blocks->blockSizeBytes == sizeof(in_colors) );
			RR_ASSERT( from_blocks->pixelFormat == rrPixelFormat_R8G8B8A8 );
			memcpy(&in_colors,fmPtr,sizeof(in_colors));			
			
			const U8 * in_block = (const U8 *)&in_colors;
						
			// BC7Prep will change A to 255 in block if ignore alpha
			//	but OodleTex_EncodeBCN_RDO has also already done that for BC7RGB format
			BC7Prep prep;
			const U8 * block = BC7Prep_init(&prep,in_block,opt,false);
			
			// in theory if we were BC7 (not RGB) with ignore alpha,
			//   then block was changed from in_block
			//	 but that should not have happened
			RR_ASSERT( memcmp(block,in_block,64) == 0 );
			
			const rrColorBlock4x4 & colors = *((const rrColorBlock4x4 *)block);
			infos[bi].colors = colors;
			if (record_candidates_enabled) {
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
					record_candidate_block(lambda, 1, to_blocks, bi, output_bc7, 0., 0.0, opt.flags); // we don't have a rate we can assign this

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
					//if ( BC7_CompressBlock(new_baseline_block,block,level,options,&cur_errbias) )
					if ( BC7_CompressBlock_Sub(new_baseline_block,block,&opt,&prep,&cur_errbias) )
					{				
						// do a J test for the recomp baseline vs original baseline
						bc7bits new_bits = bc7bits_load(new_baseline_block);
						int new_mode = bc7bits_get_mode(new_bits);
						
						RR_ASSERT( new_mode != old_mode );
						RR_ASSERT( expected_size_errbias.ssd_err_bias[new_mode] <= expected_size_errbias.ssd_err_bias[old_mode] );
						
						// typically you should see that new_mode has lower ssd_err_bias and higher D
						//	 (since it wasn't chosen by original baseline encode which only cares about D)
											
						U32 old_D = bc7rd_SSD(old_bits,block,opt.flags);
						U32 new_D = bc7rd_SSD(new_bits,block,opt.flags);
							
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
			bc7enc_state_from_bits(&baseline_st,baseline_block_ptr,opt.flags);
		
			/*
			// check that my SSD matches bc7comp :
			{
				bc7bits baseline_bits = bc7bits_load(baseline_block_ptr);
				U32 baseline_SSD = bc7rd_SSD(baseline_bits,block,opt.flags);
				//RR_ASSERT( baseline_SSD == baseline_st.err );
				//baseline_st.err is NOT set!
				BC7Error baseline_st_err = bc7enc_calc_error(&baseline_st,block,opt.flags);
				
				RR_ASSERT( baseline_SSD == baseline_st_err );
			}
			*/
		
			//U32 baseline_err = bc7enc_calc_error(&baseline_st,block,opt.flags);
			F32 baseline_D = bc7rd_D(&baseline_st,block,opt.flags,*pActivity);
			F32 baseline_R = bc7rd_R_uncompressed;
			if (record_candidates_enabled)
				record_candidate_block(lambda, 2, to_blocks, bi, baseline_st, baseline_D, baseline_R, opt.flags);
	
			/*
			// investigating where A error comes from on opaque images
			//	  A should be 255 everywhere
			// on tech_panel_metal_001_g.bmp :
			// 100% of the A error comes from mode 6 pbits
			//	endpoint is 254 , pbit is 0 (if it was 1 you'd hit 255)
			//	decode comes out with A == 254 or 255 everywhere
			{
				U8 baseline_decoded[64];
				bc7enc_decode_state(&baseline_st,baseline_decoded,opt.flags);
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
			int best_mtfwindow_endpoints_i = -1;
			int best_mtfwindow_indices_i = -1;
		
			if ( 1 )
			{
				// if we've seen the exact same baseline block before in this chunk, make sure we produce the same encoding
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

					// need to recompute block D even though we use the same block
					// as prev_bi because activity can be different
					RAD_ALIGN(U8, decoded[64], BC7_BLOCK_ALIGN);
					bc7_decode_block4x4a(decoded,prev_block_ptr,opt.flags);

					infos[bi].D = bc7rd_D(decoded,block,opt.flags,*pActivity);
					infos[bi].J = bc7rd_J(infos[bi].D,repeat_R,lambda);

					// NOT SURE: insert into window?
					// easier said than done, would need to find where it actually is to get the right inds
					/*bc7rd_windowentry new_entry;
					bc7enc_state_from_bits(&new_entry.st,prev_block_ptr,opt.flags);
					new_entry.encoded = bc7bits_load(prev_block_ptr);*/

					continue;
				}
			}

			// take past blocks and try substituting endpoints or indices
			
			if ( mtfwindow_endpoints_size > 0 )
			{
			SIMPLEPROFILE_SCOPE(bc7rd_endpoints);
			// loop on window of past endpoints
			//	find new indexes
		
			// best_J_here is best_J from this loop, not overall
			F32 best_J_here = (F32)BC7_ERROR_MAX;
			
			// mtfwindi_last will get copy of best and do slow refine
			//  I'm doing the copy-to-last just to avoid code dupe
			int mtfwindi_iters = mtfwindow_endpoints_size+1;
			mtfwindi_iters = RR_MIN(mtfwindi_iters, config.mtf_window_endpoints_size);
			int mtfwindi_last = mtfwindi_iters-1;
			RR_DURING_ASSERT( mtfwindow_endpoints[mtfwindi_last]->st.err = BC7_ERROR_MAX );
			// disable last iter :
			mtfwindi_iters = mtfwindi_last;
					
			U32 best_err = BC7_ERROR_MAX;
	
			/*
			// 2000 is a quality *improvement* on most images
			//	but "tech_panel_metal_001_g.bmp" really doesn't like it
			//#define ENDPOINTS_BBOX_COLOR_D		256 // <- near nop except on tech_panel
			#define ENDPOINTS_BBOX_COLOR_D		128 // <- starting to get real speed savings, bigger quality loss
			U32 color_bbox_reject_dsqr = (U32)( lambda * ENDPOINTS_BBOX_COLOR_D * ENDPOINTS_BBOX_COLOR_D );
			*/

			// 1-subset inputs are used by Mode 4, 5, 6.
			BC7Input input_1s[4];
			for LOOP(rbit,4)
				BC7Input_Get(&input_1s[rbit],block,/*ns*/1,/*p*/0,rbit,/*alpha_weight*/1);

			// The in-loop input that isn't compatible with input_rgba_1s.
			BC7Input mft_input;

			for LOOP(mtfwindowi,mtfwindi_iters)
			{
				const BC7BlockState & window_st = mtfwindow_endpoints[mtfwindowi]->st;
				RR_ASSERT( window_st.err != BC7_ERROR_MAX );

				#if 0
				// early reject subset endpoints vs current block color bbox
				// we already have all the color bboxes from the BC7Prep
				// -> 04/21/2020 :
				//	 I can't find a tweak here that works
				//	  if you tweak D down so it's saving time, it hurts quality
				//	 on many images it works great (porsche,mysoup)
				//	  but on some (tech_panel) it hurts a lot
				int ns = bc7_modes[ window_st.mode ].ns;
				const BC7PartitionInfo & prep_info = prep.infos[ns-1][window_st.p];
				const U8 * num_pixels_per_subset = radtex_num_pixels_per_subset[ns][window_st.p];
				U32 tot_dsqr = 0;
				for LOOP(s,ns)
				{
					const BC7Color * color_bbox_lohi = prep_info.bbox[s];
					const BC7Color * st_endpoints = window_st.subsets[s].endpoints; // @@ endpoints_q ?
					// color_bbox_lohi is from the current block
					// st_endpoints is from the state I want to reuse
					// both are arrays of [2] = 8 bytes

					// rather than reject per subset; sum the d's and reject once?
					//	(multiply by num pixels in subset)
					// -> yes I think this is better, but it still doesn't work on tech_panel

					U32 dsqr = endpoints_color_bbox_dsqr((const rrColor32BGRA *)st_endpoints,(const rrColor32BGRA *)color_bbox_lohi);
					tot_dsqr += dsqr * num_pixels_per_subset[s];
					
					/*
					if ( reject_endpoints_color_bbox((const rrColor32BGRA *)st_endpoints,(const rrColor32BGRA *)color_bbox_lohi,color_bbox_reject_d) )
  					{
  						goto rejected;
  					}
  					/**/
				}
				if ( tot_dsqr > color_bbox_reject_dsqr )
				{
					continue;
				}
				#endif
				
				BC7BlockState st = window_st;

				BC7Input *input;
				if (st.mode == 4 || st.mode == 5 || st.mode == 6)
					input = &input_1s[st.subsets[0].rbit];
				else
				{
					BC7Input_Get(&mft_input,block,&st);
					input = &mft_input;
				}

				// take past endpoints and find new indexes for this block : (and pbits?)
				// first run through, use calc_indexes_linear_approx
				//	then on the last/best redo with calc_indexes_exact
				bool slow_exact = (mtfwindowi == mtfwindi_last);
				bool change_pbits = false;
				//bool change_pbits = slow_exact; // do change pbits only on the last refine? -> meh
				int bits_changed = bc7enc_reindex_block(&st,input->subsets,opt.flags,change_pbits,false,slow_exact);
				int cur_matched_bits = 128 - bits_changed;
				RR_ASSERT( cur_matched_bits == bc7rd_lz_matched_bits_reindex(st.mode,change_pbits) );
				
				#if 0 // check that only indices changed
				{
					bc7bits out;
					bc7enc_emit(bc7bits_U8ptr(&out),&st);
					bc7bits old = mtfwindow_endpoints[mtfwindowi]->encoded;
					bc7bits x = bc7bits_xor(out,old);
					// x should only be on in index bits
					bc7bits mask( change_pbits ? c_bc7bitrange_indices_with_pbits_mask[st.mode] : c_bc7bitrange_indices_mask[st.mode] );
					RR_ASSERT_ALWAYS( bc7bits_popcnt( bc7bits_andnot(x,mask) ) == 0 );					
				}
				#endif
				
				// use st->err (SSD) as early out?
				//	we want to favor lower mtfwindowi , so require SSD decreasing:
				if ( st.err > best_err ) continue;
				best_err = st.err;				
								
				F32 cur_D = bc7rd_D(&st,block,opt.flags,*pActivity);
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
						record_candidate_block(lambda, 3, to_blocks, bi, st, cur_D, cur_R, opt.flags);
				}
				
				if ( cur_J < best_J_here )
				{
					best_J_here = cur_J;
					// copy cur to last :
					*( mtfwindow_endpoints[mtfwindi_last] ) = *( mtfwindow_endpoints[mtfwindowi] );
					// enable last iter :
					mtfwindi_iters = mtfwindi_last+1;
				}
			}
			
			} // profile scope
			
			if ( config.core_endpoints_from_indices && mtfwindow_indices_size > 0 )
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
			int mtfwindi_iters = mtfwindow_indices_size+1;
			mtfwindi_iters = RR_MIN(mtfwindi_iters, config.mtf_window_indices_size);
			int mtfwindi_last = mtfwindi_iters-1;
			RR_DURING_ASSERT( mtfwindow_indices[mtfwindi_last]->st.err = BC7_ERROR_MAX );
			// disable last iter :
			mtfwindi_iters = mtfwindi_last;
			
			U32 best_err = BC7_ERROR_MAX;
			
			for LOOP(mtfwindowi,mtfwindi_iters)
			{	
				BC7BlockState st = mtfwindow_indices[mtfwindowi]->st;
				RR_ASSERT( st.err != BC7_ERROR_MAX );

				// take past indexes and find new endpoints for this block : (and pbits?)
				bool change_pbits = false;
				//bool change_pbits = true; // tiny bit worse, small difference
				// do slow_anneal on just the "last" which is a copy of the best so far
				// @@ if we get to last and best_J_here is too far off best_J , skip it?
				bool slow_anneal = (mtfwindowi == mtfwindi_last);
				int bits_changed = bc7enc_find_endpoints(&st,block,opt.flags,change_pbits,slow_anneal);
				
				#if 0 // check that only endpoints where changed
				{
					bc7bits out;
					bc7enc_emit(bc7bits_U8ptr(&out),&st);
					bc7bits old = mtfwindow_indices[mtfwindowi]->encoded;
					bc7bits x = bc7bits_xor(out,old);
					// x should only be on in index bits
					bc7bits mask( c_bc7bitrange_endpoints_mask[st.mode] );
					RR_ASSERT_ALWAYS( bc7bits_popcnt( bc7bits_andnot(x,mask) ) == 0 );					
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
				
				// this hack gives same results as (128 - bits_changed)
				//cur_matched_bits = 8 + bc7rd_lz_matched_bits_reendpoint(st.mode,change_pbits);
				
				F32 cur_D = bc7rd_D(&st,block,opt.flags,*pActivity);
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
						record_candidate_block(lambda, 4, to_blocks, bi, st, cur_D, cur_R, opt.flags);
				}
				
				if ( cur_J < best_J_here )
				{
					best_J_here = cur_J;
					// copy cur to last :
					*( mtfwindow_indices[mtfwindi_last] ) = *( mtfwindow_indices[mtfwindowi] );
					// enable last iter :
					mtfwindi_iters = mtfwindi_last+1;
				}
			}
			
			} // profile scope

			if (config.do_ilm)
			{
			SIMPLEPROFILE_SCOPE(bc7rd_matrix);				
			
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
			int matrix_dim_ind = RR_MIN(matrix_dim,mtfwindow_indices_size);
			int matrix_dim_end = RR_MIN(matrix_dim,mtfwindow_endpoints_size);
			
			//const rrColorBlock4x4 & colors = *((const rrColorBlock4x4 *)block);
			// find SAD of current best, to set up must_beat_sad :
			U8 best_st_decoded[64];
			bc7enc_decode_state(&best_st,best_st_decoded,opt.flags);
			U32 before_sad = ColorBlock4x4_ComputeSAD_RGBA(colors,*((const rrColorBlock4x4 *)best_st_decoded));

			// using final-matrix tweaked tolerance here to reduce tweak count
			U32 must_beat_sad = rr_froundint(before_sad + lambda * BC7_FINAL_MATRIX_SAD_INCREASE_TOLERANCE);				
					
					
			for LOOP(indi,matrix_dim_ind)
			{	
				const BC7BlockState & past_indices_st = mtfwindow_indices[indi]->st;
				const bc7bits & past_indices_encoded = mtfwindow_indices[indi]->encoded;
			
				int mode = past_indices_st.mode;

				//bc7bits mask_i ( c_bc7bitrange_indices_mask[mode] );
				bc7bits mask_ip( c_bc7bitrange_indices_with_pbits_mask[mode] );
												
				bc7bits past_indices_noe  = bc7bits_and(past_indices_encoded,mask_ip);
				//bc7bits past_indices_noep = bc7bits_and(past_indices_encoded,mask_i);

				for LOOP(endpi,matrix_dim_end)
				{
					// hyperbola limited search :
					// we want to mainly search where either indi or endpi are very low
					//	 like 100x4 and 4x100 , not the whole 100x100 square
					int index_product = (indi+1)*(endpi+1);
					if ( index_product > config.ilm_hyperbola_max_index_product ) break;

					//stat_in_loop_matrix_hyperbola_count++;
					
					const BC7BlockState & past_endpoints_st = mtfwindow_endpoints[endpi]->st;
					
					// must be same mode
					if ( past_endpoints_st.mode != mode ) continue;

					// part must be the same :
					//	this is not strictly required but almost all useful pairs have same part
					if ( past_endpoints_st.p != past_indices_st.p ) continue;
					
					const bc7bits & past_endpoints_encoded = mtfwindow_endpoints[endpi]->encoded;

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
				
					//bc7bits pair1 = bc7bits_or_assert_exclusive(past_endpoints_noi,  past_indices_noep);
					//bc7bits pair2 = 
					bc7bits pair1 = bc7bits_or_assert_exclusive(past_endpoints_noip, past_indices_noe);
					
					// if the pbits are the same or in modes with no pbits, then pair1 == pair2
					// note pbits can make a big difference; max is at the 4th bit, so 16 per pixel in SSD = 16^3 overall
					
					for(;;) // loop on pair1 & pair2
					{								
						RAD_ALIGN(U8, decoded[64], BC7_BLOCK_ALIGN);
						//stat_in_loop_matrix_decode_count++;
						bc7_decode_block4x4a(decoded,bc7bits_U8ptr(&pair1),opt.flags);
						
						// SAD early out is very strong
						//  almost nothing makes it past here	
						U32 sad = ColorBlock4x4_ComputeSAD_RGBA(colors,*((const rrColorBlock4x4 *)decoded));

						if ( sad <= must_beat_sad )
						{
							//stat_in_loop_matrix_vqd_count++;				
							F32 cur_D = bc7rd_D(decoded,block,opt.flags,*pActivity);
											
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
									// get state : 
									// chould just do this once at the end
									//	but the average count of this is < 1 anyway so no harm
									bc7enc_state_from_bits(&best_st,bc7bits_U8ptr(&pair1),opt.flags);
									
									best_J = cur_J;
									best_D = cur_D;
									best_mtfwindow_endpoints_i = endpi;
									best_mtfwindow_indices_i = indi;
									if (record_candidates_enabled)
										record_candidate_block(lambda, 5, to_blocks, bi, *((const rrColorBlock4x4*)decoded), bc7bits_get_mode(pair1), cur_D, cur_R);
								}
							}
						}
						
						/*
						if ( pair1 == pair2 )
							break;
						pair1 = pair2;
						// after first iter, pair1 == pair2 will be true so second iter will break
						/*/
						// just one of the pairs
						break;
						/**/
					}
				}
			}
			
			} // profile scope

			infos[bi].D = best_D;
			infos[bi].J = best_J;
			
			// no? should be in canonical form already?
			// bc7enc_canonicalize
			
			bc7enc_emit(output_bc7,&best_st, opt.flags);
			
			bc7rd_windowentry new_entry;
			new_entry.st = best_st;
			new_entry.encoded = bc7bits_load(output_bc7);

			// add to window :
			bc7_update_mtf_window(&new_entry,best_mtfwindow_endpoints_i,mtfwindow_endpoints,mtfwindow_endpoints_size);
			bc7_update_mtf_window(&new_entry,best_mtfwindow_indices_i,mtfwindow_indices,mtfwindow_indices_size);

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
			for LOOP(bi, nblocks)
				record_candidate_block_from_infos(lambda, 6, to_blocks, infos, bi, opt.flags); // validate 1

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
				for LOOP(bi, nblocks)
					record_candidate_block_from_infos(lambda, 9, to_blocks, infos, bi, opt.flags);

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
					F32 baseline_blocks_D = bc7rd_D(baseline_block_bits,infos[bi].colors,opt.flags,*pActivity);
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

		if (config.do_final_matrix)
		{
			bc7rd_final_matrix(dispatch,to_blocks,activity_blocks,infos,lambda,opt.flags,config);
			if (record_candidates_enabled)
				for LOOP(bi, nblocks)
					record_candidate_block_from_infos(lambda, 11, to_blocks, infos, bi, opt.flags);
		}


		if (config.do_final_optimize)
		{
			SIMPLEPROFILE_SCOPE(final_optimize);
	
			// @@ does order matter?
			//endpoints:
			bc7rd_final_optimize(to_blocks,activity_blocks,infos,opt.flags,lambda,false);

			//indices :
			bc7rd_final_optimize(to_blocks,activity_blocks,infos,opt.flags,lambda,true);

			// @@ then endpoints again? (that's what BC1RD does)
		}

		if (record_candidates_enabled)
			for LOOP(bi, nblocks)
				// validate 2
				record_candidate_block_compute_D_unknown_R(lambda, 23, to_blocks, infos, bi, opt.flags,*BlockSurface_SeekC_ActivityBlock4x4(activity_blocks,bi));
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
	rrprintf("mode histo = { %5.2f%%, %5.2f%%, %5.2f%%, %5.2f%%, %5.2f%%, %5.2f%%, %5.2f%%, %5.2f%% }\n",
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
