// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetsse4


#include "rrdxt1vq.h"
#include "rrdxt1vqhelp.h"
#include "rrdxt1vqhelp.inl"
#include "rrdxtcblock.h"
#include "blocksurface.h"
#include "rrdxtccompress.h"
#include "rrdxtccompress.inl"
#include "rrsurfacedxtc.h"
#include "rrvecc.h"
#include "rrcolorvecc.h"
#include "templates/rrvector.h"
#include "templates/rrhashtable.h"
#include "templates/rralgorithm.h"
#include "rrlogutil.h"
#include "log2table.h"
#include "perceptualactivity.h"
#include "perceptualactivity.inl"
#include "rrsurfacerowcache.h"
#include "rrdxtcblock.inl"

#include "rrsimd.h"
#include "vec128.inl"

#ifdef DO_BUILD_SSE4
#include <smmintrin.h>
#endif

// 04-30-2020 : lambda normalization
//#define BC1RD_LAMBDA_SCALE (16.f/10.f) // before
// scale up by 5/4
//	to move lambda 40 to 50
//#define BC1RD_LAMBDA_SCALE 2.f
// too much
#define BC1RD_LAMBDA_SCALE (1.7f * OO2TEX_GLOBAL_LAMBDA_SCALE)
	
	
//#include "rrsurfaceblit.h"
//#include "../texutil/rrsurfacebyname.h"

#include "rrsimpleprof.h"
//#include "rrsimpleprofstub.h"

RR_NAMESPACE_START

//===========================================

// speed-quality tradeoffs

struct rrDXT1_VQ_Config
{
	// size of endpoint palette to search in endpoints pass :

	// note that endpoints vq palette gets the heuristic reduce ( < 1024 and often less)
	//	this needs to be much bigger than for indices
	//	because indices get a separate merger pass
	//	endpoints relies entirely on this for its merging
	int endpoints_vq_palette_search_limit;
	
	// size of "lz_window" :
	int max_num_added_entries_to_try_endpoints;
	
	// size of index palette to search in index pass :
	int index_vq_palette_search_limit;
	int max_num_added_entries_to_try_indices;
	
	// final matrix tweaks :
	// start with larger matrix dim than I want :
	// 40k @@ ?? initial candidate set for codelen doesn't have a huge effect on speed
	// these are a fine speed-quality tradeoff, needs careful tweak :
	int final_pass_matrix_dim_initial;	
	int final_pass_matrix_dim_search;
	
	// early reject distances :

	// !!WARNING!!
	// because these are lambda-scaled, they are a real nice speedup at low lambda
	//		not so much at high lambda
	//		(we do get other speedups at high lambda, due to smaller vq palette sizes)
	// also quality can be unchanged at high lambda, but affected at low lambda
	//	the standard "total BPB saved at vcdiff +3"
	//	  is at quite high lambda
	//	so you won't see the affect there
	// if you look at RD curves, you will see it at lower lambda!
	// -> add a report at vcdiff +1 to see these

	// EARLY_OUT_INDEX_DIFF
	//	 a full index step from ep0 to ep1 is "6" (0-2-4-6 or 0-3-6)
	//	 in 3c mode an index black is 13
	// scaled by lamda : (lambda=10 is identity scaling):
	
	int early_out_index_diff_times_lambda;
	int early_out_index_diff_plus_constant;
	
	// early out endpoints :
	// scaled by lamda :
	int early_out_color_endpoint_distance;
	int early_out_endpoints_ssd_max_increase_scaled_by_lambda;

	// index diff early out is a good speed-quality gain on most images
	//	but not all
	bool do_index_diff_early_out;

	// we need 2 iterations
	// 1st iteration is just for seeding
	//	  it takes us from baseline to a decent candidate VQ encoding
	// 2nd iteration basically throws away all the work of the 1st iteration
	//    but it needs the 1st iteration for initial seeds & codelens
	int num_vq_iterations;
};

static const rrDXT1_VQ_Config c_config_levels[rrDXTCLevel_Count] =
{
	// rrDXTCLevel_VeryFast=0 (== 1 secret level in OodleTex_ API ; too low quality, not currently well optimized for this quality-speed tradeoff)
	{ 
		// endpoints :
		256,64, 
		// indices:
		64,32, 
		// final matrix:
		60,30,
		// early_out_index_diff :
		9,55,
		// early out endpoints :
		20,3,
		// do_index_diff_early_out
		true,
		// num_vq_iterations
		1,
	},
	
	// rrDXTCLevel_Fast=1,		// == OodleTex_EncodeEffortLevel_Low
	{ 
		// lowering parameters a lot more here, but it's probably
		// better to just shrink cluster size (outside) instead.
		// but whatever, let's just try something.

		// endpoints :
		500,120,
		// indices:
		120,50,
		// final matrix:
		140,70,
		// early_out_index_diff :
		9,55,
		// early out endpoints :
		20,3,
		// do_index_diff_early_out
		true,
		// num_vq_iterations
		1,
	},

	// rrDXTCLevel_Slow=2,		// == OodleTex_EncodeEffortLevel_Normal
	{ 
		// slightly lower settings and turn off VQ iterations
		// they're expensive and don't help _that_ much

		// endpoints :
		800,200, 
		// indices:
		200,100, 
		// final matrix:
		160,80,
		// early_out_index_diff :
		9,55,
		// early out endpoints :
		20,3,
		// do_index_diff_early_out
		true,
		// num_vq_iterations
		1,
	},

	// DEFAULT :
	// rrDXTCLevel_VerySlow=3  == OodleTex_EncodeEffortLevel_High == OodleTex_EncodeEffortLevel_Default
	{ 
		// endpoints :
		1024,256, 
		// indices:
		256,128, 
		// final matrix:
		200,100,
		// early_out_index_diff :
		9,55,
		// early out endpoints :
		20,3,
		// do_index_diff_early_out
		true,
		// num_vq_iterations
		2,
	},
	
	// rrDXTCLevel_Reference=4,// == 99 secret level in OodleTex_ API ; too slow to be practical, not a good time-quality tradeoff; just a max quality reference
	{ 
		// endpoints :
		1024,256, 
		// indices:
		256,128, 
		// final matrix:
		200,100,
		// early_out_index_diff :
		9,55,
		// early out endpoints :
		20,3,
		// do_index_diff_early_out
		true,
		// num_vq_iterations
		2,
	}
};

const rrDXT1_VQ_Config & rrDXT1_VQ_GetConfig(rrDXTCLevel effort)
{
	//rrprintf("rrDXT1_VQ_GetConfig: effort level=%d (%s)\n", effort, rrDXTCLevel_GetName(effort));
	
	RR_ASSERT( (int)effort >= 0 && (int)effort < RR_ARRAY_SIZE(c_config_levels) );
	return c_config_levels[effort];
	//return c_config[0];
}

//===========================================

// Unit index map. We have some loops that always indirect through an index table, but part
// of the input is just from the front of the array; we just use an index table with idx[i]=i
// to handle both with the same kernels.

#define IDX4(i) (i)+0,(i)+1,(i)+2,(i)+3
#define IDX16(i) IDX4((i)+0),IDX4((i)+4),IDX4((i)+8),IDX4((i)+12)
#define IDX64(i) IDX16((i)+0),IDX16((i)+16),IDX16((i)+32),IDX16((i)+48)
#define IDX256(i) IDX64((i)+0),IDX64((i)+64),IDX64((i)+128),IDX64((i)+192)

static const int c_unit_index_map[1024] =
{
	IDX256(0),
	IDX256(256),
	IDX256(512),
	IDX256(768),
};

#undef IDX4
#undef IDX16
#undef IDX64
#undef IDX256

//=======================
// AnyIndexD

#if defined(DO_BUILD_SSE4)

//	basically the same thing FindIndices does without the MIN
static void AnyIndexD_add(AnyIndexD * aid,const rrColorBlock4x4 & block,rrColor32BGRA palette[4])
{
	using internal::DeltaSqrRGBA_SSE2;

	// Load all 16 pixels worth of data, and the palette
	// NOTE: in non-Alpha mode we canonicalize all the A's to 255,
	// so their delta is always zero, no need to special-case it
	Vec128 pal  = load128u(palette);
	Vec128 mask_lo = _mm_set1_epi16(0xff); // mask for low half of words

	// NOTE: see comments on in scalar AnyIndexD_Add for why there is no extra A scaling here.
	Vec128 pal_br16 = _mm_and_si128(pal, mask_lo);
	Vec128 pal_ga16 = _mm_srli_epi16(pal, 8);

	for (int c = 0; c < 16; c += 4)
	{
		// One row's worth of data
		Vec128 row = load128u(block.colors + c);
		Vec128 row_br16 = _mm_and_si128(row, mask_lo);
		Vec128 row_ga16 = _mm_srli_epi16(row, 8);

		Vec128 d0 = DeltaSqrRGBA_SSE2(row_br16, row_ga16, _mm_shuffle_epi32(pal_br16, 0x00), _mm_shuffle_epi32(pal_ga16, 0x00));
		Vec128 d1 = DeltaSqrRGBA_SSE2(row_br16, row_ga16, _mm_shuffle_epi32(pal_br16, 0x55), _mm_shuffle_epi32(pal_ga16, 0x55));
		Vec128 d2 = DeltaSqrRGBA_SSE2(row_br16, row_ga16, _mm_shuffle_epi32(pal_br16, 0xaa), _mm_shuffle_epi32(pal_ga16, 0xaa));
		Vec128 d3 = DeltaSqrRGBA_SSE2(row_br16, row_ga16, _mm_shuffle_epi32(pal_br16, 0xff), _mm_shuffle_epi32(pal_ga16, 0xff));

		// add on 4x4 matrix of ints :
		store128u( &(aid->ssd[0][c]), _mm_add_epi32( load128u(&(aid->ssd[0][c])), d0) );
		store128u( &(aid->ssd[1][c]), _mm_add_epi32( load128u(&(aid->ssd[1][c])), d1) );
		store128u( &(aid->ssd[2][c]), _mm_add_epi32( load128u(&(aid->ssd[2][c])), d2) );
		store128u( &(aid->ssd[3][c]), _mm_add_epi32( load128u(&(aid->ssd[3][c])), d3) );
	}
}

#elif defined(DO_BUILD_NEON64)

//	basically the same thing FindIndices does without the MIN
static void AnyIndexD_add(AnyIndexD * aid,const rrColorBlock4x4 & block,rrColor32BGRA palette[4])
{
	using internal::DeltaSqrRGBA_NEON;

	// Load all 16 pixels worth of data, and the palette
	// NOTE: in non-Alpha mode we canonicalize all the A's to 255,
	// so their delta is always zero, no need to special-case it
	const Vec128_U8 pal  = Vec128_U8::loadu(palette);

	// NOTE: see comments on in scalar AnyIndexD_Add for why there is no extra A scaling here.

	for (int c = 0; c < 16; c += 4)
	{
		// One row's worth of data
		const Vec128_U8 row = Vec128_U8::loadu(block.colors + c);

		const Vec128_U32 d0 = DeltaSqrRGBA_NEON<false>(row, pal.dup32<0>());
		const Vec128_U32 d1 = DeltaSqrRGBA_NEON<false>(row, pal.dup32<1>());
		const Vec128_U32 d2 = DeltaSqrRGBA_NEON<false>(row, pal.dup32<2>());
		const Vec128_U32 d3 = DeltaSqrRGBA_NEON<false>(row, pal.dup32<3>());

		// add on 4x4 matrix of ints :
		(Vec128_U32::loadu( &(aid->ssd[0][c]) ) + d0).storeu( &(aid->ssd[0][c]) );
		(Vec128_U32::loadu( &(aid->ssd[1][c]) ) + d1).storeu( &(aid->ssd[1][c]) );
		(Vec128_U32::loadu( &(aid->ssd[2][c]) ) + d2).storeu( &(aid->ssd[2][c]) );
		(Vec128_U32::loadu( &(aid->ssd[3][c]) ) + d3).storeu( &(aid->ssd[3][c]) );
	}
}

#else // no SSE2 and no NEON

static void AnyIndexD_add(AnyIndexD * aid,const rrColorBlock4x4 & colors,rrColor32BGRA palette[4])
{
	for LOOP(c,16)
	{
		// NOTE :
		// I only consider index merges where 1bt_mask is the same.
		//	note that AnyIndex can be filled with possible index changes that invalidate 1BT preservation
		//	but those values will never be looked up!
		// it can only look up indices where the A difference is 0
		//
		// Therefore, no need to bother with A weighting, and not DeltaSqrRGBA_1BT.
		const rrColor32BGRA & cur = colors.colors[c];
		for LOOP(p,4)
		{
			aid->ssd[p][c] += rrColor32BGRA_DeltaSqrRGBA(cur,palette[p]);
		}
	}
}

#endif

// For use by FindIndices_MultiBlock which can't ignore 1-bit transparency
static void AnyIndexD_add_with_1bt(AnyIndexD * aid,const rrColorBlock4x4 & colors,rrColor32BGRA palette[4])
{
	for LOOP(c,16)
	{
		const rrColor32BGRA & cur = colors.colors[c];
		for LOOP(p,4)
		{
			aid->ssd[p][c] += rrColor32BGRA_DeltaSqrRGBA_1BT(cur,palette[p]);
		}
	}
}


// for an AnyIndexD, find the indices that minimize SSD
//	and the D value
static U32 AnyIndexD_find_indices(const AnyIndexD * aid, U32 * pD)
{
	// not used in simd
	// see AnyIndexD_add_then_find_indices

	U32 indices = 0;
	U32 ssd = 0;
	for LOOP(c,16)
	{
		U32 d0 = (aid->ssd[0][c] << 2) + 0;
		U32 d1 = (aid->ssd[1][c] << 2) + 1;
		U32 d2 = (aid->ssd[2][c] << 2) + 2;
		U32 d3 = (aid->ssd[3][c] << 2) + 3;

		U32 md = RR_MIN4(d0,d1,d2,d3);
		//U32 cur_index = md & 3;
		//indices += (cur_index)<<c;

		ssd += (md>>2);

		indices >>= 2;
		indices |= (md<<30);
	}

	*pD = ssd;

	return indices;
}

static void AnyIndexD_add(AnyIndexD * to, const AnyIndexD & fm)
{
	// not used in simd
	// see AnyIndexD_add_then_find_indices

	U32 * pto = to->ssd[0];
	const U32 * pfm = fm.ssd[0];

	for LOOP(i,64) // 4*16
	{
		pto[i] += pfm[i];
	}
}

// return the D of the best indices for the pair {1,2}
//	same as adding aid1+aid2 then doing find_indices
static U32 AnyIndexD_best_index_pair_D(const AnyIndexD * aid1, const AnyIndexD * aid2)
{
	#if defined(DO_BUILD_SSE4) || defined(DO_BUILD_NEON64)

	Vec128_U32 error_sum { 0 };

	for (int c=0;c<16;c+=4)
	{
		const Vec128_U32 v0 = Vec128_U32::loadu( &aid1->ssd[0][c] ) + Vec128_U32::loadu( &aid2->ssd[0][c] );
		const Vec128_U32 v1 = Vec128_U32::loadu( &aid1->ssd[1][c] ) + Vec128_U32::loadu( &aid2->ssd[1][c] );
		const Vec128_U32 v2 = Vec128_U32::loadu( &aid1->ssd[2][c] ) + Vec128_U32::loadu( &aid2->ssd[2][c] );
		const Vec128_U32 v3 = Vec128_U32::loadu( &aid1->ssd[3][c] ) + Vec128_U32::loadu( &aid2->ssd[3][c] );

		error_sum += vmin( vmin(v0, v1), vmin(v2, v3) );
	}

	// Horizontal reduction for final error sum
	U32 ssd = reduce_add(error_sum);

	return ssd;

	#else // no SSE4 or NEON

	U32 ssd = 0;
	for LOOP(c,16)
	{
		U32 d0 = aid1->ssd[0][c] + aid2->ssd[0][c];
		U32 d1 = aid1->ssd[1][c] + aid2->ssd[1][c];
		U32 d2 = aid1->ssd[2][c] + aid2->ssd[2][c];
		U32 d3 = aid1->ssd[3][c] + aid2->ssd[3][c];

		U32 md = RR_MIN4(d0,d1,d2,d3);

		ssd += md;
	}

	return ssd;

	#endif
}

static U32 AnyIndexD_add_then_find_indices(AnyIndexD * to, const AnyIndexD & fm, U32 * pD)
{
	#if defined(DO_BUILD_SSE4) || defined(DO_BUILD_NEON64)

	/*
	AnyIndexD test = *to;
	AnyIndexD_add(&test,fm);
	U32 test_D;
	U32 test_indices = AnyIndexD_find_indices(&test,&test_D);
	*/

	// very similar to DXT1_FindIndices
	//	except we fetch two precomputed vecs of SSDs

	Vec128_U32 error_sum { 0u };
	Vec128_U32 bestr[4];

	for (int i=0;i<4;++i)
	{
		// Accumulate
		Vec128_U32 d0 = Vec128_U32::loadu(&to->ssd[0][i*4]) + Vec128_U32::loadu(&fm.ssd[0][i*4]);
		Vec128_U32 d1 = Vec128_U32::loadu(&to->ssd[1][i*4]) + Vec128_U32::loadu(&fm.ssd[1][i*4]);
		Vec128_U32 d2 = Vec128_U32::loadu(&to->ssd[2][i*4]) + Vec128_U32::loadu(&fm.ssd[2][i*4]);
		Vec128_U32 d3 = Vec128_U32::loadu(&to->ssd[3][i*4]) + Vec128_U32::loadu(&fm.ssd[3][i*4]);

		// Store the sums
		d0.storeu(&to->ssd[0][i*4]);
		d1.storeu(&to->ssd[1][i*4]);
		d2.storeu(&to->ssd[2][i*4]);
		d3.storeu(&to->ssd[3][i*4]);

		// Find the best error and index
		Vec128_U32 best01 = vmin(d0.shl<2>(), d1.shl<2>() | Vec128_U32(1));
		Vec128_U32 best23 = vmin(d2.shl<2>(), d3.shl<2>() | Vec128_U32(1));
		Vec128_U32 best = vmin(best01, best23 + Vec128_U32(2));

		// We now have 2b best color index in the low bits of every lane,
		// and square error in top 30 bits. Accumulate error first.
		error_sum += best.srl<2>();

		// Save best error values so we can later extract inds
		bestr[i] = best;
	}

	*pD = reduce_add(error_sum);

	// Extract indices
	U32 indices;

	#if defined(DO_BUILD_SSE4)
	indices  = internal::BC1_ExtractIndices_SSE4(bestr[0], bestr[1]);
	indices |= internal::BC1_ExtractIndices_SSE4(bestr[2], bestr[3]) << 16;
	#elif defined(DO_BUILD_NEON64)
	indices  = internal::BC1_ExtractIndices_NEON(bestr[0], bestr[1]);
	indices |= internal::BC1_ExtractIndices_NEON(bestr[2], bestr[3]) << 16;
	#else
	#error missing
	#endif

	//RR_ASSERT( *pD == test_D );
	RR_ASSERT( AnyIndexD_lookup(to,indices) == *pD );

	return indices;

	#else // no SSE4 or NEON

	AnyIndexD_add(to,fm);
	return AnyIndexD_find_indices(to,pD);

	#endif
}

//===========================================

/**

If image is larger than 256 KB in dxt1 (512 K pixels = 512x1024 crops)
then cut it into chunks and just do them independently
this keeps N^2 VQ issues from getting out of control
and also maps to OodleLZ chunking

256 KB dxt1 = 512K pixels = 32K blocks

VQ reduce at least 4:1 = 8K palette
pretty damn big
too big to brute force search all 8K palette entries to find closest

==============

cost to send entry with count C is not like a log2(C/N) probability thing
the more important aspect is the (32 bits/C) part
that is you must send the raw value (32 bits)
but you only have to send that once
so the cost per use is 1/C

consider C=1 vs C=2
the log2(C) difference is only 1 bit difference
but 32/C is 16 bits difference

**/

struct block_and_codelen
{
	rrDXT1Block	block;
	F32 codelen;
};

struct block_and_codelen_compare_codelen
{
	bool operator ()(const block_and_codelen & lhs,const block_and_codelen & rhs)
	{
		return lhs.codelen < rhs.codelen;
	}		
};

//===========================================

// hash U32 indices/endpoints -> F32 codelen
typedef RR_NAMESPACE::hash_table<U32,F32,hash_table_ops_mask31hash<U32> > t_hash_dw_to_codelen;
typedef RR_NAMESPACE::hash_table<U32,U32,hash_table_ops_mask31hash<U32> > t_hash_u32_to_u32;

struct rrDXT1_VQ_Block
{
	rrColorBlock4x4 colors;
	rrColor32BGRA color_bbox[2]; // lo/hi corners
	U8 block_has_any_transparency;
	U8 block_has_any_black3c;
	U8 block_is_degenerate_all_black_or_transparent;
	U8 pad;
	U32 block_1bt_mask;
	
	const SingleFloatBlock4x4 * pActivity;
	
	rrDXT1Block baseline;
	F32 baseline_vqd;
	
	rrDXT1Block cur;
	rrDXT1Block second_best;
	F32 cur_vqd;
	F32 cur_J;
	int vq_entry_index;
	int vq_entry_link;
};

// NOTE(fg): rrDXT1_VQ_Entry now in rrdxt1vqhelp.h because we want it for the AVX2
// batch VQD loop

struct vq_codelen_help
{
	F32 codelen_2count;
	//U32 codelen_singleton;
	F32 codelen_escape;
	
	//U32 sum_of_palette_counts;
	//U32 nblocks;
	//S32 nblocks_neglog2tabled;
	S32 codelen_denom;
};

#define DO_bc1_indices_vq_reduce_CHANGE_INDICES

static void bc1_indices_vq_reduce(CpuDispatchFlags dispatch,
	vector<dword_and_count> * indices_out, const vector<dword_and_count> & indices_in, 
	float lambda, int nblocks, rrDXT1_VQ_Block * blocks, rrDXT1PaletteMode pal_mode,
	const rrDXT1_VQ_Config & config);

static void Optimize_Endpoints_For_Assigned_Blocks(
	CpuDispatchFlags dispatch,
	vector<rrDXT1_VQ_Block> & blocks,
	vector<dword_and_count> & dcv,
	rrDXT1PaletteMode pal_mode);

static void Optimize_Indices_For_Assigned_Blocks(
	CpuDispatchFlags dispatch,
	vector<rrDXT1_VQ_Block> & blocks,
	vector<dword_and_count> & dcv,
	rrDXT1PaletteMode pal_mode);
	

static RADFORCEINLINE F32 VQD_BC1(CpuDispatchFlags dispatch,const rrColorBlock4x4 & colors,const rrDXT1Block & dxtb,const SingleFloatBlock4x4 & activity,rrDXT1PaletteMode pal_mode)
{
	rrColor32BGRA palette[4];
	DXT1_ComputePalette(dxtb.c0,dxtb.c1,palette,pal_mode);
	
	return VQD_BC1(dispatch,colors,palette,dxtb.indices,activity);
}

// @@ vq_codelens need work of course

static F32 vq_J( F32 D, F32 R, F32 lambda )
{
	/*
		
	LZ scheme J is
		D + lambda * R
	D = VQD
	D ~ 28 * SAD
	D ~ 2 * SSD
	R ~ 16 * bits
	*/
	
	F32 J = D + lambda * R;

	return J;	
}

static F32 vq_J_to_R_codelen(F32 J, F32 lambda)
{
	return J / lambda;
}

static F32 vq_codelen_lz(int lzi)
{
	// lzi is in blocks	
	// lzi starts at 0, don't take log of zero
	RR_ASSERT( lzi >= 0 );
	
	// @@ REVISIT ME
	//	add a constant to lzi ?
	//	to decrease the severity of recency favoring?
	
	F32 offset_bits = 2*(F32)rrlog2_bk(lzi+2) + 1;
//	F32 bits = 9 + offset_bits;
	F32 bits = 10 + offset_bits;
	// 9 or 10 about the same
	return bits;
}

static F32 vq_codelen_palette_count( U32 count, const vq_codelen_help *phelp )
{
	// P = (count/nblocks)
	//	 is the probability of using this palette index, out of all possible blocks
	// P = (count/sum_of_palette_counts)
	//	 is the (higher) probability of using this palette index, out of all palette indices
	// P = (sum_of_palette_counts/nblocks)
	//	 is the probability of using a vq palette index (1 - escape probability)
	
	// using 
	// P = (count/nblocks)
	// is like first sending a no-escape flag
	//	and then selecting this index from sum_of_palette_counts
	
	// bits to use me are
	//  log2(P) to code my index (and to send the non-escape flag)
	// PLUS (32/count) to send my endpoints (spread over each use)
	
	// use count+1 to encourage us to get another count :
	//F32 P = (count+1)/(F32)phelp->nblocks;
	//F32 bits = -rrlog2(P);
	
	S32 bits_log2one = log2tabled_bk_32(count+1) - phelp->codelen_denom;
	F32 bits = bits_log2one * (1.f / (F32)RR_LOG2TABLE_ONE);
		
	// @@ ??
	// scale up selector codelen by log2(e) because LZ is worse than indices :
	//bits *= 1.5; 
	
	// add cost of sending the endpoint/index bits :
	// @@ ?? endpoints are actually slightly cheaper than 32 with entropy coding
	bits += 32.0f/count;
		// use (count+1) here ?
		//		becase we want to know the cost not as is, but if I coded one more entry in this slot
		//  -> no definitely not count+1 is much worse
		//	@@ but it's hard to play with tweaking this right now because it's also used for
		//	   the "novel" (count=2) and "escape" estimate
		//	   so things are not decoupled for tweakage
	
	//U32 codelen = (U32)( bits );
	return bits;
}

static void vq_do_initial_palette_reduction(vector<dword_and_count> & counting,int counts_all_unique_endpoints_count,int nblocks)
{
	// counts_all_unique_endpoints_count includes singletons that have already been removed
	RR_ASSERT( nblocks >= counts_all_unique_endpoints_count );
	RR_ASSERT( counts_all_unique_endpoints_count >= counting.size32() );

	#if 0
	// count = 1's have already been removed
	// maybe remove count=2 too ?
	// -> hurts

	RR_ASSERT( counting.back().count >= 2 );
	while ( ! counting.empty() && counting.back().count == 2 )
	{
		counting.pop_back();
	}
	#endif
			
	// this is sort of hacky heuristic
	// we just want to reduce it below the eventual target
	// because we want to force more clumping & add some using the kmeans++ style incremental adder below
	
	// @@ this should be a factor of "quality" ?
	//	 is this how the levels are set?
	
	// just reduce 8:1	
	// limits palette to 4K so we don't get too many things to look up
	int max_palette_size = nblocks/8; // <- this does not apply much pressure
	// almost always initial palette size is already smaller than this
	
	// in the first pass counts_all_unique_endpoints_count/2 rarely applies much pressure
	//	 because there are lots of singles
	// on the second pass this can apply a lot of pressure at high lambda
	//	 because we don't add many singletons, then here we do a 1/2 again
	max_palette_size = RR_MIN(max_palette_size,counts_all_unique_endpoints_count/2); // @@
	
	// @@ just don't do this?
	//max_palette_size = RR_MAX(max_palette_size,512); // not less than 512
	// @@ on tiny images this 512 gets hit and we don't work
	//	should just go to like 7/8 of counts_all_unique_endpoints_count or something there
	
	#if 0
	if ( max_palette_size < 512 )
	{
		max_palette_size = nblocks/8;
		max_palette_size = RR_MIN(max_palette_size,counts_all_unique_endpoints_count*3/4);
	}
	#endif
	
	//rrprintfvar(max_palette_size);
	
	if ( counting.size32() > max_palette_size )
	{
		// just cut off the tail of "counting"
		// @@ these are not necessarily the best blocks to cut
		//	could be better about sorting them by how much they help D
		
		// we're often cutting somewhere in the long count=2 tail
		//	we pick an arbitrary spot in there to cut
		//  it would be better to sort within each count
		// @@ could sort by how much J gain it provided vs 2nd best choice
		//	 (we want to keep the entries that are furthest from earlier entries)
		
		// what's the frequency at the cut point :
		//rrprintfvar(counting.size());
		//rrprintfvar(max_palette_size);
		//rrprintfvar(counting[max_palette_size].count);
		
		// @@ ? don't cut off high counts ?
		//	typically we should be cutting in the count=2,3 region		
		// the question is, when the count is 4 or whatever
		//	can we take those blocks and map them onto some other vq bucket
		//	and is that a good or bad move for current lambda? (is it a J gain?)
		#if 0
		while( counting[max_palette_size].count > 4 && max_palette_size < counting.size32() )
		{
			// @@ should this VQ slot be left separate, or can it be mapped onto another slot?
			//  if the blocks assigned to this slot were better J on another slot, they would have been mapped there already
			//	but mapping it over now is more favorable
			//	because the target slot will add on my count
			// say my count is 4
			//	there was some other slot with count 6
			//	when it was count 6, my blocks didn't want to go there, they chose me for the best J
			//	but after I map over his count will be 10 and now his codelen will be even more favorable
			max_palette_size++;
		}
		#endif
		
		counting.resize(max_palette_size);
	}
}

static F32 find_hash_codelen_or_escape(U32 dw,const t_hash_dw_to_codelen & hash,const vq_codelen_help * pcodelens)
{
	t_hash_dw_to_codelen::entry_ptrc ep = hash.find(dw);
	if ( ep )
		return ep->data();
	else
		return pcodelens->codelen_escape;
}
 
static inline bool indices_are_flat(U32 indices)
{
	return indices == RR_ROTL32(indices,2);
}

static void hacky_clamp_counts(vector<dword_and_count> * pcounting,int nblocks)
{
	// 05-19-2020 :
	// do this?
	// I don't have any real strong (visual) evidence showing this is positive
	//	but I do like it theoretically
	//	and I don't see any evidence against it (nor for it particularly)
	// in terms of vcdiff , numerically this hurts a tiny bit
	// it does reduce the number of flat blocks in the output
	//	so it is doing what I think it is
	// see : "count number of flat blocks in the output" in textest_bc1
	
	#if 0
	// don't do it
	return;
	#else
		
	// clamp any large counts
	//	to prevent run-away favoritism feedback
	//	say a common option like flat AAA
	//	has count = 10% of all blocks
	//	before going into vq
	//	then it will be more desirable because it has a lower codelen
	//	so it will become even more popular after each pass
	//
	// the idea of clamping counts here is to pretend it's not as common as it really is
	//	to prevent its codelen from being too attractive (relative to other options)
	//	so it has to be more competitive in D before being chosen
	//
	// I visually see the over-flattening on "red_blue"
	//	but the only big numeric effect is on "good_record_only"
	
	if ( pcounting->size() < 2 ) // degenerate
		return;
	
	// the only thing I've ever seen go over 4% are the flats (AAA and FFF)
	//	they can go to 20%
	U32 max_count = (U32) ((nblocks * (UINTa) 50 + 999) / 1000); // 5.0%
	//U32 max_count = (nblocks * 40 + 999) / 1000; // 4.0%
	// more extreme clamps :
	//  -> in the end this hurts the numeric quality scores a tiny bit
	//	  and it's very hard to see the visual effect
	//		(does it reduce blocking?)
	//	  yes there's some effect per my eyeballs, is it better? hard to say
	//U32 max_count = (nblocks * 25 + 500) / 1000; // 2.5%
	//U32 max_count = (nblocks * 20 + 500) / 1000; // 2.0%
	
	if ( max_count < 5 ) return; // too small (same as having a min nblocks)
		
	vector<dword_and_count> & counting = *pcounting;
	
	RR_ASSERT( counting[0].dw != counting[1].dw );

	// high count first :
	RR_ASSERT( counting.front().count >= counting.back().count );
	
	for LOOPVEC(i,counting)
	{
		// sorted by count :
		if ( counting[i].count <= max_count )
			break;

		//textest bc1 -r30 R:\manyimages
		// log what gets clamped :
		// it's all AA's and FF's (1/3's and 2/3's)
		// AAAAAAAA : 3403 = 41.54%
		// AAAAAAAA : 1875 = 22.89%
		// FFFFFFFF : 499 = 6.09%
		//rrprintf("%08X : %d = %.2f%%\n",counting[i].dw,counting[i].count,counting[i].count*100.0/nblocks);

		counting[i].count = max_count;
	}
		
	#endif
}

enum EIndices
{
	eEndPoints = 0,
	eIndices = 1
};

enum EReduce
{
	eNoReduce=0,
	eReduce = 1
};
		
static void setup_palette_and_vq_codelen_help(vq_codelen_help * pcodelens,vector<dword_and_count> & counting,int nblocks,EIndices is_indices,EReduce do_initial_palette_reduction)
{
	// counting has the unique DWORDS & their count , but is not yet sorted by counted

	// sort counting by count :
	sort_dword_and_count_compare_count_highest_first(&counting);
	
	int counts_all_unique_endpoints_count = counting.size32();
	
	// should be all unique dws :
	RR_ASSERT( counting.size() < 2 || counting[0].dw != counting[1].dw );

	// high count first :
	RR_ASSERT( counting.front().count >= counting.back().count );
	
	// remove all the count=1 at the tail and call them escapes :
	RR_ASSERT( counting.back().count >= 1 );
	while ( ! counting.empty() && counting.back().count == 1 )
	{
		counting.pop_back();
	}
	
	int counts_num_singles_removed = counts_all_unique_endpoints_count - counting.size32();
	counts_num_singles_removed;
				
	if ( counting.size() > 2 )
	{
		if ( is_indices )
		{
			hacky_clamp_counts(&counting,nblocks);
		}
		
		if ( do_initial_palette_reduction )
		{
			// limit VQ palette size :
			RR_ASSERT( ! is_indices );
		
			vq_do_initial_palette_reduction(counting,counts_all_unique_endpoints_count,nblocks);
		}
	}
	
	/*
	// sum of counts that made it into the vq palette
	//	if nothing was removed (no singles) this would == nblocks
	U32 counts_sum_after = 0;
	for LOOPVEC(i,counting)
		counts_sum_after += counting[i].count;
	RR_ASSERT( counts_sum_after <= (U32)nblocks );
	/**/
		
	/*
	// look at the distribution :
	// it's very peaky
	// just a few with high count
	// then lots of 2s and 3s
	// (could make this a more compact log with a count of counts)
	rrprintf("setup_palette_and_vq_codelen_help (%s) (%s)\n",
		is_indices ? "indices" : "endpoints",
		do_initial_palette_reduction ? "reduce" : "no reduce" );
	for LOOPVEC(i,counting)
	{
		rrprintf("%08X : %d = %.2f%%\n",counting[i].dw,counting[i].count,counting[i].count*100.0/nblocks);
		if ( i == 20 ) break;
	}
	/**/
	
	//pcodelens->nblocks = nblocks;
	
	// what's the denominator for codelen in log2(count/denom) ?
	//	is it total # of blocks? or only the ones that made it into the VQ palette?
	// -> doesn't seem to matter much
	//	-> this is essentially just a constant that's added to all the J's
	//		so it really should completely factor out
	//		some exception to that since we compare to "lz" codelen
	U32 total_for_codelen_denom = nblocks;
	//U32 total_for_codelen_denom = nblocks - counts_num_singles_removed;
	//U32 total_for_codelen_denom = counts_sum_after;
	pcodelens->codelen_denom = log2tabled_bk_32(total_for_codelen_denom);


	// make codelens for rate estimate
	// pretend we are statistical coding the palette selection
	
	pcodelens->codelen_2count = vq_codelen_palette_count( 2,pcodelens );
	
	F32 codelen_singleton = vq_codelen_palette_count( 1,pcodelens );
		
	pcodelens->codelen_escape = codelen_singleton;	
}

struct block_final_state
{
	F32 cur_J;
	F32 min_vqd;
	F32 maximum_codelen;
	U32 must_beat_sad;
};

void bc1rd_final_matrix(
	CpuDispatchFlags dispatch,
	vector<rrDXT1_VQ_Block> & blocks,
	vector<dword_and_count> & endpoint_counting,
	vector<dword_and_count> & index_counting,
	F32 lambda, int lambdai,
	rrDXT1PaletteMode pal_mode,
	const rrDXT1_VQ_Config & config)
{
	int nblocks = blocks.size32();

	SIMPLEPROFILE_SCOPE_N(bc1rd_final_matrix,nblocks);

	/*

	N*M final pass
	index_counting & endpoint_counting are filled
	need codelen helpers
	cur_J and cur_vqd are up to date in blocks


	up to this point we have done no joint endpoint-index vq palette selection
	endpoint pass did not look at index palette (it finds new indices, but scores them by index codelen)
	index pass did not look at endpoint palette (it just keeps endpoints fixed)


	this final pass uniformly lowers quality & rate
	it finds alternative blocks that are quite low rate because they are in the joint vq palette
	at possibly lower quality than what we already have
	-> that's what I thought would happen, but it's wrong
	in fact this pass can find higher quality encodings

	---------

	this is just brute force over a ton of SADs
	8k blocks
	~10k candidates
	= 80 M SADs

	SADs are very fast to compute on x86 in particular thanks to PSADBW, so this is already not
	terrible.

	However, we are typically only interested in small distances, and SAD is the L1 norm difference
	of blocks ||a - b||_1. The reverse triangle inequality applies so we know that
	
	  abs(||a||_1 - ||b||_1) <= ||a - b||_1

	We search for candidates where the RHS is <= some threshold T. Clearly if the former is also
	above that threshold, we needn't even try. This is very coarse for larger T but when T is
	relatively small, just comparing the 1-norms of blocks like this rules out a lot of candidates.
	Furthermore, we can just keep a list of blocks sorted by norm and prune this list quickly with
	some binary searching.

	In summary:
	- compute L1 norms for blocks in advance (this an O(N) preprocess);
	- Sort (norm,index) pairs in advance.
	- use the reverse triangle inequality to batch-reject candidates that are way off,
	  using a binary search on the sorted list.
	- Use the actual SAD comparisons on block that pass this first test; because SADs are
	  directly supported, this is still very fast; typically only a tiny fraction (often
	  0.02% or less) of the initial blocks makes it past this test.
	- Finally, blocks that pass the actual SAD test are properly scored.

	As a final wrinkle, this loop is run in a blocked fashion to avoid thrashing the L1 and L2
	caches.

	*/

	vq_codelen_help endpoint_codelen_help;
	setup_palette_and_vq_codelen_help(&endpoint_codelen_help,endpoint_counting,nblocks,eEndPoints,eNoReduce);

	vq_codelen_help index_codelen_help;
	setup_palette_and_vq_codelen_help(&index_codelen_help,index_counting,nblocks,eIndices,eNoReduce);

	int num_endpoints = RR_MIN(endpoint_counting.size32(), config.final_pass_matrix_dim_initial);
	int num_indices = RR_MIN(index_counting.size32(), config.final_pass_matrix_dim_initial);

	//F32 codelen_endpoints[config.final_pass_matrix_dim_initial];
	//F32 codelen_indices[config.final_pass_matrix_dim_initial];
	vector<F32> codelen_endpoints(config.final_pass_matrix_dim_initial,0.f);
	vector<F32> codelen_indices(config.final_pass_matrix_dim_initial,0.f);

	for LOOP(epi,num_endpoints)
	{
		codelen_endpoints[epi] = vq_codelen_palette_count(endpoint_counting[epi].count,&endpoint_codelen_help);
	}

	for LOOP(indi,num_indices)
	{
		codelen_indices[indi] = vq_codelen_palette_count(index_counting[indi].count,&index_codelen_help);
	}

	int initial_matrix_dim = num_indices * num_endpoints;

	//	take the top N*M with the lowest codelen

	vector<block_and_codelen> pairs;
	pairs.resize(initial_matrix_dim);

	for LOOP(epi,num_endpoints)
	{
		for LOOP(indi,num_indices)
		{
			U32 cur_endpoints = endpoint_counting[epi].dw;
			U32 cur_indices = index_counting[indi].dw;
			F32 codelen = codelen_endpoints[epi] + codelen_indices[indi];
			int i = epi + indi * num_endpoints;
			pairs[i].block.endpoints = cur_endpoints;
			pairs[i].block.indices = cur_indices;
			pairs[i].codelen = codelen;
		}
	}

	// sort by codelen
	//  lowest first
	stdsort(pairs.begin(),pairs.end(),block_and_codelen_compare_codelen());

	// now reduce matrix size to smaller
	//	with just the top (lowest) codelens :

	int num_pairs = RR_MIN(config.final_pass_matrix_dim_search*config.final_pass_matrix_dim_search,pairs.size32());
	pairs.resize(num_pairs);

	// decompress them to colors
	// NOTE(fg): with the pair loop now chunked, could decompress a chunk at a time
	// and save some memory, but I'll keep changes down to a minimum right now.

	vector_aligned<rrColorBlock4x4, 64> pair_colors; // color blocks are 64B, we have a bunch, just leave them fully aligned
	pair_colors.resize(num_pairs);

	for LOOPVEC(i,pairs)
	{
		DXT1_Decompress(&pair_colors[i],pairs[i].block,pal_mode);
	}

	vector<block_final_state> final_st;
	final_st.resize(blocks.size());

	// initialize final_st for every block
	// (spilled state for the current totals for every block)
	for LOOPVEC(block_i,blocks)
	{
		rrDXT1_VQ_Block * pblock = &blocks[block_i];
		const rrColorBlock4x4 & colors = pblock->colors;

		// get the SAD of the current chosen encoding :
		rrColorBlock4x4 cur_colors;
		DXT1_Decompress(&cur_colors,pblock->cur,pal_mode);

		if ( pal_mode == rrDXT1PaletteMode_Alpha )
			RR_ASSERT( DXT1_OneBitTransparent_Mask_Same(pblock->block_1bt_mask,pblock->cur) );

		U32 cur_sad = ColorBlock4x4_ComputeSAD_RGBA(colors,cur_colors);
		F32 cur_J = pblock->cur_J;
		F32 cur_vqd = pblock->cur_vqd;

		// Optimize_Endpoints_For_Assigned_Blocks spoils this :
		//RR_DURING_ASSERT( F32 check_vqd = VQD_BC1(colors,pblock->cur,pblock->activity) );
		//RR_ASSERT( fequal(cur_vqd,check_vqd,0.001f) );
		cur_vqd = VQD(colors,cur_colors,*pblock->pActivity);

		// fix cur_J for vqd change :
		RR_ASSERT( cur_J > pblock->cur_vqd );
		cur_J -= pblock->cur_vqd; // J is now just the rate part
		cur_J += cur_vqd;

		// cur_J NOT = vq_J calculation because codelens have changed for this pass

		//recall J = D + (lambda*scale) * R;

		// if codelen is >= maximum_codelen then you can't beat cur_J even at D = 0
		// assume you can get to 0 D :
		//F32 maximum_codelen = cur_J  * scale / lambda;

		// maximum_codelen is a pure termination criterion, not an approximation
		//	if we assumed that we only ever do as well as "min_vqd"
		//	rather than thinking we can get down to D = 0

		// assume you could find baseline quality : (very conservative)
		// guess at what the minimum possible VQD is for this block in any BC1 encoding
		F32 min_vqd = RR_MIN(cur_vqd,pblock->baseline_vqd);
		F32 maximum_codelen = vq_J_to_R_codelen(cur_J - min_vqd, lambda);

		// assume you can only get to cur_vqd : // is this okay? -> no, hurts
		//F32 maximum_codelen = (cur_J - cur_vqd) * scale / lambda;

		RR_ASSERT( maximum_codelen > 0.f );

		// it appears the best thing is to allow only minimal SAD increase :
		//	(remember lambda goes in steps of 10 now so this isn't tiny)
		//#define FINAL_PASS_SAD_INCREASE_TOLERANCE	10 // @@ ??
		//#define FINAL_PASS_SAD_INCREASE_TOLERANCE	20 // no difference to 10 ?
		//#define FINAL_PASS_SAD_INCREASE_TOLERANCE	5 // no difference to 10 ?
		#define FINAL_PASS_SAD_INCREASE_TOLERANCE	1 // better !? (than 0 or 2)

		// start with a criterion that you must get SAD within some proximity of "cur"
		//	then after that it must be strictly decreasing
		U32 must_beat_sad = cur_sad + lambdai * FINAL_PASS_SAD_INCREASE_TOLERANCE;

		// must_beat_sad could be estimated from cur_J and a SAD-VQD ratio bound
		// VQD is ~ 22 * SAD
		// must_beat_sad ~ J / 22  (assume you find a zero rate encoding)
		// this seems okay but worse
		//U32 must_beat_sad = (U32) ( cur_J / 16 );

		final_st[block_i].cur_J = cur_J;
		final_st[block_i].min_vqd = min_vqd;
		final_st[block_i].maximum_codelen = maximum_codelen;
		final_st[block_i].must_beat_sad = must_beat_sad;
	}

	// process pairs in chunks of pair_colors that stay in L1 to minimize cache thrashing
	// this was a big speed-up!
	const int pair_chunk_size = FinalMatrixL1Index::CHUNK_SIZE;
	FinalMatrixL1Index chunk_index;

	// list of candidate chunk SADs; initialize fully to sentinel vals
	int chunk_sads[pair_chunk_size + FIND_NEXT_BELOW_PADDING];
	for (int i = 0; i < pair_chunk_size + FIND_NEXT_BELOW_PADDING; ++i)
		chunk_sads[i] = -1;

	for (int pair_chunk_begin = 0; pair_chunk_begin < num_pairs; pair_chunk_begin += pair_chunk_size)
	{
		int pair_chunk_end = RR_MIN(pair_chunk_begin + pair_chunk_size, num_pairs);
		int pair_chunk_len = pair_chunk_end - pair_chunk_begin;

		chunk_index.build(&pair_colors[pair_chunk_begin], pair_chunk_len);

		// for each block
		for LOOPVEC(block_i,blocks)
		{
			rrDXT1_VQ_Block * pblock = &blocks[block_i];

			F32 & cur_J = final_st[block_i].cur_J;
			F32 min_vqd = final_st[block_i].min_vqd;
			F32 & maximum_codelen = final_st[block_i].maximum_codelen;
			U32 & must_beat_sad = final_st[block_i].must_beat_sad;

			// Candidates are ordered by codelen ascending; first check if
			// everything in the chunk is above our current max, in which case
			// there's no reason to do anything else.
			const F32 initial_max_codelen = maximum_codelen;
			if ( pairs[pair_chunk_begin].codelen >= initial_max_codelen )
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
			const int culled_len = chunk_index.lookup(pblock->colors,&culled_inds,must_beat_sad);
			RR_ASSERT( culled_len >= 0);
			if ( culled_len <= 0 )
				continue;

			// After the big cull, do another batch loop to compute all the
			// SADs for non-rejected blocks in this chunk before we make
			// any decisions
			compute_chunk_sads_indirect(dispatch, chunk_sads, culled_len, culled_inds, &pair_colors[pair_chunk_begin], pblock->colors);

			// sentinel at the end for scan loop to guarantee we notice actual end; guaranteed to be less
			// than any must_beat_sad we might have (which is always going to be >=0)
			chunk_sads[culled_len] = -1;

			// go over the remaining candidates
			for (int i = 0; ; i++) // yes, no loop condition!
			{
				// we allow i == culled_len here; we have the extra sentinels/padding at the end
				// to make sure we catch this.
				RR_ASSERT( i <= culled_len );
				i = find_next_below(chunk_sads, i, must_beat_sad);
				if ( i >= culled_len )
					break;

				const int pair_i = pair_chunk_begin + culled_inds[i];

				// check if SAD got better, if not we ignore this block

				U32 this_sad = chunk_sads[i];
				RR_ASSERT(this_sad < must_beat_sad ); // that's what we scanned for before

				// SAD went down, evaluate this block :

				if ( pal_mode == rrDXT1PaletteMode_Alpha )
				{
					// reject if 1-bit A flag doesn't match :
					if ( ! DXT1_OneBitTransparent_Mask_Same(pblock->block_1bt_mask,pairs[pair_i].block) )
						continue;
				}

				// We already did the coarse reject with our initial maximum_codelen for the
				// whole chunk earlier; if we found more improvements within this chunk, then
				// maximum_codelen might've shrunk further. Check if we're below now.
				if ( pairs[pair_i].codelen >= maximum_codelen )
					continue;

				must_beat_sad = this_sad;

				F32 vqd = VQD(pblock->colors,pair_colors[pair_i],*pblock->pActivity);

				F32 J = vq_J(vqd,pairs[pair_i].codelen,lambda);

				// often the "cur" is re-found here
				//	but J is a bit different because the codelen estimates are different now

				if ( J < cur_J )
				{
					// take it !
					cur_J = J;
					//cur_vqd = vqd;
					pblock->cur = pairs[pair_i].block;

					//*
					// as we find lower J, we might be able to lower maximum_codelen :
					// this is pretty meh, doesn't affect speed or quality
					// the J gains we make here are just typically small
					//	therefore maximum_codelen doesn't change a ton
					maximum_codelen = vq_J_to_R_codelen(cur_J - min_vqd,lambda);
					/**/
				}
			}
		}
	}
}

static void update_mtf_window(int new_val,int * lz_window, int & lz_window_size, int max_lz_window_size)
{
	RR_ASSERT( lz_window_size <= max_lz_window_size );

	// is it already in lz_window ?
	int lz_found_i = -1;
	for LOOP(lz_i,lz_window_size)
	{
		if ( lz_window[lz_i] == new_val )
		{
			lz_found_i = lz_i;
			break;
		}
	}
	
	if ( lz_found_i < 0 )
	{
		// add it :
		if ( lz_window_size < max_lz_window_size )
			lz_window_size++;
		
		lz_found_i = lz_window_size-1;
	}

	// MTF :
	memmove(lz_window+1,lz_window,lz_found_i*sizeof(lz_window[0]));
	lz_window[0] = new_val;
}

static bool blocks_all_same(const BlockSurface * blocks)
{
	const int nblocks = blocks->count;
	if ( ! nblocks )
		return true;

	U64 first_block = RR_GET64_NATIVE_UNALIGNED(BlockSurface_SeekC(blocks,0));

	for (int bi = 1; bi < nblocks; ++bi)
	{
		const U8 * cur = BlockSurface_SeekC(blocks,bi);
		if ( RR_GET64_NATIVE_UNALIGNED(cur) != first_block )
			return false;
	}

	return true;
}

// Part of index substitution pass
// check whether candidates violate 1-bit transparency rules and if so,
// substitute their VQD errors with a huge value
//
// 2 VQD values per index
static void override_VQDs_on_transparency_mismatch(F32 * vqds_2per,
	const rrDXT1_VQ_Entry * vqindices, const int * inds, int count,
	U32 first_endpoints, U32 second_endpoints,
	U32 block_1bt_mask)
{
	// Mask of the even bits in 3-color mode, 0 in 4-color mode
	// (meaning no pixels are transparent)
	const U32 first_3c_mask = DXT1_Is4Color(first_endpoints, rrDXT1PaletteMode_Alpha) ? 0 :  0x55555555;
	const U32 second_3c_mask = DXT1_Is4Color(second_endpoints, rrDXT1PaletteMode_Alpha) ? 0 :  0x55555555;

	for LOOP(i,count)
	{
		const U32 vqp_indices = vqindices[inds[i]].dw;

		// Pixels where we use index 3 have one of the even bits set:
		const U32 idx3_mask = vqp_indices & (vqp_indices >> 1);

		// We can now AND idx3_mask with the 3c_mask for that set of endpoints to get the transparency mask
		if ( ( idx3_mask & first_3c_mask ) != block_1bt_mask )
			vqds_2per[i*2 + 0] = (F32)RR_DXTC_ERROR_BIG;
		if ( ( idx3_mask & second_3c_mask ) != block_1bt_mask )
			vqds_2per[i*2 + 1] = (F32)RR_DXTC_ERROR_BIG;
	}
}

bool BC1_RD(BlockSurface * to_blocks,
	const BlockSurface * from_blocks,
	const BlockSurface * baseline_blocks,
	const BlockSurface * activity_blocks,
	int lambdai,
	rrDXTCOptions options,
	const rrDXTCRD_Options & rdopts)
{
	SIMPLEPROFILE_SCOPE_N(bc1rd,to_blocks->count);

	const rrDXT1_VQ_Config & config = rrDXT1_VQ_GetConfig(rdopts.effort);
	const F32 lambdaf = lambdai * BC1RD_LAMBDA_SCALE;

	int nblocks = to_blocks->count;

	RR_ASSERT( to_blocks->pixelFormat == rrPixelFormat_BC1 || to_blocks->pixelFormat == rrPixelFormat_BC2 || to_blocks->pixelFormat == rrPixelFormat_BC3 );
	RR_ASSERT( baseline_blocks->pixelFormat == to_blocks->pixelFormat );

	// For BC1, we always ignore alpha
	// BC2 and BC3 are always in four-color mode
	rrDXT1PaletteMode pal_mode = ( to_blocks->pixelFormat == rrPixelFormat_BC1 ) ? rrDXT1PaletteMode_NoAlpha : rrDXT1PaletteMode_FourColor;
	
	if ( options & rrDXTCOptions_BC1_OneBitAlpha )
	{
		RR_ASSERT( to_blocks->pixelFormat == rrPixelFormat_BC1 );
		pal_mode = rrDXT1PaletteMode_Alpha;
	}

	// If baseline blocks are literally all the same (happens in large flat areas),
	// there's really no point trying anything regardless of lambda
	if ( blocks_all_same( baseline_blocks ) )
	{
		for LOOP(bi,nblocks)
		{
			const U8 * fromPtr = BlockSurface_SeekC(baseline_blocks,bi);
			U8 * toPtr = BlockSurface_Seek(to_blocks,bi);
			memcpy(toPtr,fromPtr,sizeof(U64));
		}
		return true;
	}

	CpuDispatchFlags dispatch = CpuDispatchFlags::init(&options);

	RR_ASSERT( nblocks <= 32*1024 );
	
	vector<rrDXT1_VQ_Block> blocks;
	vector<dword_and_count> endpoint_counting;
	vector<dword_and_count> index_counting;
	vector<dword_and_count> dwc_scratch;
	vector<U32> endpoints;
	vector<U32> indices;

	t_hash_dw_to_codelen dw_to_codelen_hash;
	t_hash_u32_to_u32 dw_to_index_hash;
	vector<rrDXT1_VQ_Entry> vqendpoints;
	vector<rrDXT1_VQ_Entry> vqindices;
	vector<F32> vq_codelen_lz_tab;
	vector<F32> index_vqd_results;
	vector<int> endpoint_filter_results;
	vector<S32> endpoint_ssds;

	const int max_either_palette_size = RR_MAX(config.index_vq_palette_search_limit, config.endpoints_vq_palette_search_limit);
	const int max_either_window_size = RR_MAX(config.max_num_added_entries_to_try_indices, config.max_num_added_entries_to_try_endpoints);

	{
		//SIMPLEPROFILE_SCOPE(bc1rd_upfront_alloc);
		blocks.resize(nblocks);
		endpoint_counting.reserve(nblocks);
		index_counting.reserve(nblocks);
		dwc_scratch.reserve(nblocks);
		endpoints.resize(nblocks);
		indices.resize(nblocks);

		dw_to_codelen_hash.reserve_initial_size(nblocks);
		dw_to_index_hash.reserve_initial_size(nblocks);
		vqendpoints.reserve( nblocks );
		vqindices.reserve( nblocks );
		vq_codelen_lz_tab.reserve(max_either_window_size);
		index_vqd_results.resize((config.index_vq_palette_search_limit + config.max_num_added_entries_to_try_indices)*2 + FIND_NEXT_BELOW_PADDING);
		endpoint_filter_results.resize(config.endpoints_vq_palette_search_limit + config.max_num_added_entries_to_try_endpoints);
		endpoint_ssds.resize(config.endpoints_vq_palette_search_limit + config.max_num_added_entries_to_try_endpoints + FIND_NEXT_BELOW_PADDING);
	}

	// precompute code lens for the VQ window ahead of time
	for LOOP(i,max_either_window_size)
		vq_codelen_lz_tab.push_back(vq_codelen_lz(i));

	// make sure index_vqd_results and endpoint_ssds are fully initialized since we can (and will) read past the parts we explicitly
	// populate later on as part of the scan loop
	memset(index_vqd_results.data(), 0, sizeof(index_vqd_results[0])*index_vqd_results.size());
	for LOOPVEC(i,endpoint_ssds)
		endpoint_ssds[i] = -1;

	RR_ASSERT_ALWAYS( max_either_palette_size <= RR_ARRAY_SIZE(c_unit_index_map) );

	// read baseline encoding to seed :
	{
		SIMPLEPROFILE_SCOPE_N(bc1rd_read_baseline,nblocks);

		for LOOP(bi,nblocks)
		{
			rrDXT1_VQ_Block * pblock = &blocks[bi];
		
			const U8 * fmPtr = BlockSurface_SeekC(from_blocks,bi);
			RR_ASSERT( from_blocks->pixelFormat == rrPixelFormat_R8G8B8A8 );
		
			rrColorBlock4x4 & colors = pblock->colors;
			memcpy(&colors,fmPtr,sizeof(rrColorBlock4x4));
			SwapRB(&colors);
			
			bool block_has_any_transparency = false;
			U32 block_1bt_mask = 0;

			if ( pal_mode == rrDXT1PaletteMode_Alpha )
			{
				// this is done by rrSurfaceDXTC_CompressBC1 in the non-RDO path too
				
				int num_transparent = 0;

				// Canonicalize, then you can just use RGBA deltas
				//	no need for the special alpha-aware error metric (still using that at the moment though)
				for LOOP(i,16)
				{
					rrColor32BGRA_MakeOneBitTransparentCanonical(&colors.colors[i]);

					if ( colors.colors[i].dw == 0 )
					{
						num_transparent++;
					}
				}
				
				block_1bt_mask = DXT1_OneBitTransparent_Mask(colors);

				block_has_any_transparency = num_transparent > 0;

				RR_ASSERT( block_has_any_transparency == (block_1bt_mask != 0) );

				if ( num_transparent == 16 )
				{
					// @@ degenerate
					// store all FF to block and do nothing else
					// -> that should have already been done in baseline
					// this will also trigger num_nonblack_colors == 0 below

					// at the moment we still need to process this block
					//	to get it counted, or we hit other degeneracies elsewhere
					// -> maybe fix that so we're better at earlying out on degenerate blocks
					//	such as solid color / all black
				}
			}
			else
			{
				// ensure all A's are 255 here :
				//	this lets us do RGBA SSD later and not worry about A :
				KillAlpha(colors);
			}
			
			RR_ASSERT( rrColorBlock4x4_IsBC1Canonical(colors,pal_mode) );

			{
				// find block color bbox :
				rrColor32BGRA loC;
				loC.u.b = loC.u.g = loC.u.r = 255;
				rrColor32BGRA hiC; 
				hiC.dw = 0;
				rrColor32BGRA hiC_all;
				hiC_all.dw = 0;

				int num_nonblack_colors = 0;

				for(int i=0;i<16;i++)
				{
					const rrColor32BGRA & c = colors.colors[i];
					//avg += ColorToVec3i( c );
					
					if ( c.u.r < 12 && c.u.g < 12 && c.u.b < 12 &&
						pal_mode != rrDXT1PaletteMode_FourColor ) // BLACKNESS_DISTANCE
					{
						// near black
						// transparent comes in here too
						
						hiC_all.u.b = RR_MAX(hiC_all.u.b,c.u.b);
						hiC_all.u.g = RR_MAX(hiC_all.u.g,c.u.g);
						hiC_all.u.r = RR_MAX(hiC_all.u.r,c.u.r);
					}
					else
					{
						// rgb bbox of the non-black/alpha colors
						RR_ASSERT( c.u.a == 255 );
						num_nonblack_colors++;

						hiC.u.b = RR_MAX(hiC.u.b,c.u.b);
						hiC.u.g = RR_MAX(hiC.u.g,c.u.g);
						hiC.u.r = RR_MAX(hiC.u.r,c.u.r);
						loC.u.b = RR_MIN(loC.u.b,c.u.b);
						loC.u.g = RR_MIN(loC.u.g,c.u.g);
						loC.u.r = RR_MIN(loC.u.r,c.u.r);
					}
				}
				
				if ( num_nonblack_colors == 0 )
				{
					// loC/hiC not set
					// use the black region bbox instead
					loC.dw = 0;
					hiC = hiC_all;					

					// if hiC == 0,0,0 , total degen, all black
				}
				else
				{
					hiC_all.u.b = RR_MAX(hiC.u.b,hiC_all.u.b);
					hiC_all.u.g = RR_MAX(hiC.u.g,hiC_all.u.g);
					hiC_all.u.r = RR_MAX(hiC.u.r,hiC_all.u.r);
				}

				// color_bbox is only the RGB part :
				loC.u.a = 255;
				hiC.u.a = 255;
				pblock->color_bbox[0] = loC;
				pblock->color_bbox[1] = hiC;
			
				// in FourColor mode, num_nonblack_colors is always 16
				pblock->block_has_any_transparency = block_has_any_transparency;
				pblock->block_has_any_black3c = num_nonblack_colors < 16;
				pblock->block_is_degenerate_all_black_or_transparent = ( hiC_all.dw == 0 );
				// block_is_degenerate_all_black_or_transparent can still have non-trivial coding if it's black/alpha
				//	-> r:\black_a_512.tga
				
				pblock->block_1bt_mask = block_1bt_mask;

				// if block_has_any_transparency, then block_has_any_black will be on too
				RR_ASSERT( pblock->block_has_any_black3c || ! block_has_any_transparency );
			}
			
			// read block from input surface :
			rrDXT1Block & block = pblock->baseline;
			const U8 * baselinePtr = BlockSurface_SeekC(baseline_blocks,bi);
			//rrDXT1_GetBlock(bc1RowPtr,&block);
			memcpy(&block,baselinePtr,sizeof(block));
			
			const U8 * activityPtr = BlockSurface_SeekC(activity_blocks,bi);
			RR_ASSERT( activity_blocks->pixelFormat == rrPixelFormat_1_F32 );
			pblock->pActivity =  (const SingleFloatBlock4x4*)activityPtr;
			
			pblock->baseline_vqd = VQD_BC1(dispatch,colors,block,*pblock->pActivity,pal_mode);
			//rrCompressDXT1_2( &block, &pblock->baseline_ssd, colors, rrDXTCOptions_None, pal_mode );
		
			pblock->cur = pblock->baseline;
			pblock->cur_vqd = pblock->baseline_vqd;
			pblock->cur_J = 0.f;
		
			pblock->second_best = pblock->cur;
			
			if ( pal_mode == rrDXT1PaletteMode_Alpha )
				RR_ASSERT( DXT1_OneBitTransparent_Mask_Same(pblock->block_1bt_mask,pblock->cur) );
			
			//bc1RowPtr += sizeof(rrDXT1Block);
		
			// ? swap endpoints to 4 color order? so initial seeds are only 4-color order?
			//	-> no much worse on stuff like frymire
			//	could also reject c0 == c1 degenerate endpoints
			//	-> baseline never has degenerate endpoints
			//	  it also never has indices = ep0 or ep1 (000 or 555)
			//	  but those can all arise after RD
		
			#if 0

			U32 ssd = DXT1_ComputeSSD_RGB(colors,block,pal_mode);
			U32 sad = DXT1_ComputeSAD_RGB(colors,block,pal_mode);

			cur_total_vqd += pblock->baseline_vqd;
			cur_total_ssd += ssd;
			cur_total_sad += sad;
			
			#endif				
		
			endpoints[bi] = rrDXT1Block_EndPoints_AsU32(block);
			indices[bi] = block.indices;
		}
		
		/*
		{
			F32 reference = 2 * cur_total_ssd;
			F32 cur_sad_scale = reference / cur_total_sad;
			F32 cur_vqd_scale = reference / cur_total_vqd;
			rrprintfvar(cur_sad_scale);
			rrprintfvar(cur_vqd_scale);
		}
		
		g_total_vqd += cur_total_vqd;
		g_total_ssd += cur_total_ssd;
		g_total_sad += cur_total_sad;
		log_total_vqd();
		*/
		
		sort_and_count_uniques(&endpoint_counting,endpoints);
		sort_and_count_uniques(&index_counting,indices);				
	}

	{
	SIMPLEPROFILE_SCOPE_N(bc1rd_core,nblocks);
	
	for LOOP(vq_iterations,config.num_vq_iterations)
	{
		//=================================================
		// first endpoint loop
		
		// endpoint_counting & index_counting is carried from last iteration
		
		// make index codelen hash table for endpoint loop :
		
		vq_codelen_help index_codelen_help;
		setup_palette_and_vq_codelen_help(&index_codelen_help,index_counting,nblocks,eIndices,eNoReduce);
		
		dw_to_codelen_hash.clear();
		dw_to_codelen_hash.reserve(index_counting.size32());
		
		RR_ASSERT( index_counting.size() < 2 || index_counting[0].count >= index_counting[1].count );
		
		//U32 cheapest_index_codelen = vq_codelen_palette_count( index_counting[0].count,&index_codelen_help );
			
		for LOOPVEC(i,index_counting)
		{
			F32 codelen = vq_codelen_palette_count( index_counting[i].count,&index_codelen_help );
			U32 dw = index_counting[i].dw;
			RR_ASSERT( dw_to_codelen_hash.find(dw) == NULL );
			dw_to_codelen_hash.insert( dw, codelen );
		}

		//=================================================
		// now make endpoint initial VQ Table :	
			
		vq_codelen_help endpoint_codelen_help;
		// @@ setup_initial forces reduction :
		setup_palette_and_vq_codelen_help(&endpoint_codelen_help,endpoint_counting,nblocks,eEndPoints,eReduce);
			
		// make VQ palette
		// @@ with accelerations to help evaluate block error quickly
		//	could pre-compute dxt1 palettes in simd vectors for example

		vqendpoints.clear();
		vqendpoints.resize( endpoint_counting.size() );
		
		dw_to_index_hash.clear();
		dw_to_index_hash.reserve( endpoint_counting.size32() );
		
		for LOOPVEC(i,endpoint_counting)
		{
			rrDXT1_VQ_Entry & vqp = vqendpoints[i];
			vqp.dw = endpoint_counting[i].dw;
			
			RR_ASSERT( dw_to_index_hash.find(vqp.dw) == NULL );
			dw_to_index_hash.insert( vqp.dw, i );
			
			rrDXT1EndPoints ep;
			ep.dw = vqp.dw;
			DXT1_ComputePalette(ep.u.c0,ep.u.c1,vqp.palette,pal_mode);
			vqp.codelen = vq_codelen_palette_count( endpoint_counting[i].count,&endpoint_codelen_help );
			
			vqp.block_link = -1;
			vqp.block_count = 0;
		}
		
		{
		SIMPLEPROFILE_SCOPE_N(bc1rd_endpoints,nblocks);
		
		int endpoint_vq_palette_size_before = vqendpoints.size32(); // == endpoint_counting.size()
		//rrprintfvar(endpoint_vq_palette_size_before);
		RR_ASSERT( endpoint_counting.size() <= 4*1024 );

		int endpoint_vq_palette_num_to_search = RR_MIN(endpoint_vq_palette_size_before,config.endpoints_vq_palette_search_limit);

		//int lz_window[config.max_num_added_entries_to_try_endpoints ];
		vector<int> lz_window;
		lz_window.resize(config.max_num_added_entries_to_try_endpoints );
		int lz_window_size = 0;

		const S32 ssd_max_increase_early_out = config.early_out_endpoints_ssd_max_increase_scaled_by_lambda*lambdai;
		
		for LOOPVEC(i,blocks)
		{
			// try block with something from VQ vqendpoints
			rrDXT1_VQ_Block * pblock = &blocks[i];
			const rrColorBlock4x4 & colors = pblock->colors;
			
			S32 best_ssd = RR_DXTC_ERROR_BIG;
			F32 best_vqd = (F32)RR_DXTC_ERROR_BIG;
			F32 best_J = (F32)RR_DXTC_ERROR_BIG;
			int best_p = -1;			
			rrDXT1Block best_block;
			rrDXT1Block best_block2; // save 2nd best choice for index loop
			// seed best_block with something valid :
			best_block2 = best_block = pblock->baseline;
			
			rrDXT1EndPoints baseline_ep;
			baseline_ep.dw = pblock->baseline.endpoints;
			rrColor32BGRA baseline_palette[4];
			DXT1_ComputePalette(baseline_ep.u.c0,baseline_ep.u.c1,baseline_palette,pal_mode);
								
			// this is the very slow N*M loop : (N blocks * M codebook entries)
			//		(which is N^2 because M is proportional to N)
			// this is basically just doing FindIndices & VQD over and over again
			
			int filtered_pal_count = 0;
			int filtered_wnd_count = 0;
			
			// Hoist bbox for early reject so compiler knows it's loop-invariant
			const U32 bbox_min_d = config.early_out_color_endpoint_distance*lambdai;
			const bool requires_3c = pblock->block_has_any_transparency && pal_mode == rrDXT1PaletteMode_Alpha;

			// if block_is_degenerate_all_black_or_transparent skip?
			// don't continue, still need to add it to block linked list
			// just leave the filtered pal/wnd counts at 0
			//	-> yes seems fine
			if ( ! pblock->block_is_degenerate_all_black_or_transparent )
			{
				filtered_pal_count = filter_endpoint_color_bbox(
					dispatch,
					endpoint_filter_results.data(),
					vqendpoints.data(), c_unit_index_map, endpoint_vq_palette_num_to_search,
					pblock->color_bbox, bbox_min_d, requires_3c);

				filtered_wnd_count = filter_endpoint_color_bbox(
					dispatch,
					endpoint_filter_results.data() + filtered_pal_count,
					vqendpoints.data(), lz_window.data(), lz_window_size,
					pblock->color_bbox, bbox_min_d, requires_3c);
			}

			// Batch-compute the SSDs for all candidates that pass the early reject
			int filtered_count = filtered_pal_count + filtered_wnd_count;

			batch_find_best_errors(
				dispatch,
				endpoint_ssds.data(),
				vqendpoints.data(), endpoint_filter_results.data(), filtered_count,
				colors);

			// set up sentinel
			endpoint_ssds[filtered_count] = -1;

			for (int filter_i = 0; ; filter_i++) // yes, no loop condition!
			{
				// Scan for next candidate SSD below threshold

				// we allow i == filtered_count here; we have the extra sentinels/padding at the end
				// to make sure we catch this.
				RR_ASSERT( filter_i <= filtered_count );
				filter_i = find_next_below(endpoint_ssds.data(), filter_i, best_ssd + ssd_max_increase_early_out);
				if ( filter_i >= filtered_count )
					break;

				S32 ssd = endpoint_ssds[filter_i];
				RR_ASSERT( ssd < best_ssd + ssd_max_increase_early_out );

				// Filter results store vqp
				const int filter_val = endpoint_filter_results[filter_i];
				const int vqp = filter_val & 0xffff;

				rrDXT1_VQ_Entry & vqendpoint = vqendpoints[vqp];

				// lower palette index = higher count = preferable
				// so favor lower palette indices at equal SSD
				//	with index_codelen you can't do this (and minimize J)
				//	endpoint codelen is strictly going up, but index codelen can go all over
				// we batch-computed SSD but threw away the indices (because that's faster)
				// if we decide to go further with them, which is rare, solve for the indices
				// again
				U32 solved_ssd;
				U32 new_indices = DXT1_FindIndices(dispatch,colors,vqendpoint.palette,&solved_ssd);

				RR_ASSERT( (S32)solved_ssd == ssd );

				rrDXT1Block block;
				block.indices = new_indices;
				block.endpoints = vqendpoint.dw;;

				F32 vqd = VQD_BC1(dispatch,colors,vqendpoint.palette,new_indices,*pblock->pActivity);

				// if in the lz_window region :
				//	endpoint codelen from lz offset instead of frequency ?
				F32 endpoint_codelen = vqendpoint.codelen;
				if ( filter_i >= filtered_pal_count )
				{
					const int lzi = filter_val >> 16;
					F32 lz_codelen = vq_codelen_lz_tab[lzi];
					endpoint_codelen = RR_MIN(lz_codelen,endpoint_codelen);
				}

				// only bother computing index_codelen (which is not cheap) when we might actually take this block
				// prelim_J is a conservative J estimate (always better than real J)
				F32 prelim_J = vq_J(vqd,endpoint_codelen,lambdaf);
				if ( prelim_J < best_J )
				{
					// in this loop we measure cost of indices
					//	so we will favor repeated indices if we chance upon them
					//	but we don't actively look for them
					F32 index_codelen = find_hash_codelen_or_escape(new_indices,dw_to_codelen_hash,&index_codelen_help );
					F32 J = vq_J(vqd,endpoint_codelen + index_codelen,lambdaf);
					RR_ASSERT( J >= prelim_J );

					// force strictly decreasing vqd ?
					//	-> quite good for red_blue and quite bad for good_record
					//	-> overall meh
					//if ( J < best_J && vqd < best_vqd )
					if ( J < best_J )
					{
						best_J = J;
						best_vqd = vqd;
						best_block2 = best_block;
						best_block = block;
						best_p = vqp;

						// ssd doesn't necessarily go down when J does, so MIN here :
						// we intentionally set best_ssd here only when we get a good J block
						// not any time we see a lower SSD
						// if we did that, we could make best_ssd too low for any block to qualify
						//	without ever getting a J we liked
						best_ssd = RR_MIN(best_ssd,ssd);
					}
				}

				if ( vq_iterations > 0 ) // second iter only
				{
					// see if cur indices from last iteration can be retained
					//
					// -> yes helps a little
					//	mainly on wood_worn and good_record_only
					//	near nop on the rest

					U32 cur_indices = pblock->cur.indices;

					if ( cur_indices != new_indices )
					{
						block.indices = cur_indices;
						//block.endpoints unchanged

						vqd = VQD_BC1(dispatch,colors,vqendpoint.palette,cur_indices,*pblock->pActivity);
						prelim_J = vq_J(vqd,endpoint_codelen,lambdaf);
						if ( prelim_J < best_J)
						{
							// compared to "new_indices" we should see a lower index_codelen and higher vqd
							F32 index_codelen = find_hash_codelen_or_escape(cur_indices,dw_to_codelen_hash,&index_codelen_help );
							F32 J = vq_J(vqd,endpoint_codelen + index_codelen,lambdaf);
							RR_ASSERT( J >= prelim_J );

							if ( J < best_J )
							{
								best_J = J;
								best_vqd = vqd;
								best_block2 = best_block;
								best_block = block;
								best_p = vqp;

								best_ssd = RR_MIN(best_ssd,ssd);
							}
						}
					}
				}
			}
			
			// consider sending an escape & baseline encoding instead
								
			rrDXT1Block candidates[2] = { pblock->baseline };
			F32 candidates_vqd[2] = { pblock->baseline_vqd };
			int ncandidates = 1;
			if ( pblock->cur != pblock->baseline )
			{
				// 1st iter, cur is == baseline and this is dupe work

				//RR_ASSERT( pblock->cur_vqd == VQD_BC1(pblock->colors,pblock->cur,pblock->activity) );
				// not up to date :
				pblock->cur_vqd = VQD_BC1(dispatch,pblock->colors,pblock->cur,*pblock->pActivity,pal_mode);

				candidates[ncandidates] = pblock->cur;
				candidates_vqd[ncandidates] = pblock->cur_vqd;		
				ncandidates++;
			}
			
			for LOOP(candidate_i,ncandidates)
			{
				rrDXT1Block candidate = candidates[candidate_i];
				F32 candidate_vqd = candidates_vqd[candidate_i];

				// big vcdiff gain to NOT check this :			
				//if ( candidate_vqd < best_vqd ) // almost always true (for baseline, but NOT for cur) remove me?
				{
					// assume baseline endpoints are an escape
					//	if they're not, then it should have been found in the vq loop above
					
					U32 cand_endpoints = candidate.endpoints;
					F32 endpoints_codelen = endpoint_codelen_help.codelen_escape;
				
					#if 1
					// check if baseline is in vqendpoints ?
					// see if cur is in palette to get a cheaper codelen, if it's not really an escape
					// -> this is almost a NOP for endpoints because we do full search of vqendpoints already
					//	(unlike indexes where it is a solid help)
					
					int endpoints_found_p = -1;
					
					/*
					for LOOPVEC(pp,vqendpoints)
					{
						if ( vqendpoints[pp].dw == endpoints )
						{
							endpoints_codelen = vqendpoints[pp].codelen;
							endpoints_found_p = pp;
							break;
						}
					}
					*/
					
					{
						t_hash_u32_to_u32::entry_ptrc ep = dw_to_index_hash.find(cand_endpoints);
						if ( ep != NULL )
						{
							endpoints_found_p = ep->data();
							endpoints_codelen = vqendpoints[endpoints_found_p].codelen;
							RR_ASSERT( vqendpoints[endpoints_found_p].dw == cand_endpoints );
						}
					}
					
					#endif
				
					// J eval :
					F32 index_codelen = find_hash_codelen_or_escape(candidate.indices,dw_to_codelen_hash,&index_codelen_help );
						
					F32 J = vq_J(candidate_vqd,endpoints_codelen + index_codelen,lambdaf);
					
					if ( J < best_J )
					{
						if ( endpoints_found_p < 0 )
						{
							// add to palette :
							vqendpoints.push_back();			
							best_p = vqendpoints.size32()-1;
							
							rrDXT1_VQ_Entry & vqp = vqendpoints.back();
							//vqp.endpoint_codelen = endpoint_codelen_singleton;
							vqp.codelen = endpoint_codelen_help.codelen_2count;
							// endpoint_codelen as if I have a 2 count
							//	since someone using me again would make my count at least 2
							//	this makes it more desirable to reuse me than to send another escape
							vqp.dw = cand_endpoints;
							DXT1_ComputePalette(cand_endpoints,vqp.palette,pal_mode);
							vqp.block_link = -1;
							vqp.block_count = 0;		
							
							RR_ASSERT( dw_to_index_hash.find(cand_endpoints) == NULL );
							dw_to_index_hash.insert(cand_endpoints,best_p);	
						}
						else
						{
							best_p = endpoints_found_p;
						}
						
						best_J = J;						
						best_block2 = best_block;
						best_block = candidate;
						best_vqd = candidate_vqd;
					}
				}
			}
			
			// @@ TODO :
			//	record the top N choices (maybe just N = 2)
			//	 rather than just one choice
			//  for use in the index VQ so it has endpoint options
			
			RR_ASSERT( best_block.endpoints == vqendpoints[best_p].dw );
			pblock->cur = best_block;
			pblock->second_best = best_block2;
			pblock->cur_vqd = best_vqd;
			pblock->cur_J = best_J;
			pblock->vq_entry_index = best_p;
			pblock->vq_entry_link = vqendpoints[best_p].block_link;
			vqendpoints[best_p].block_link = i;
			vqendpoints[best_p].block_count ++;
			
			#if 0
			// vqendpoint codelen comes from counts of previous pass
			//	as we go through this pass, update it?
			// -> not helping so far
			// update codelen :
			// block_count starts at 0 and counts up as it gets used
			//	  could use P = block_count / num_blocks_walked_so_far
			F32 new_codelen = vq_codelen_palette_count( vqendpoints[best_p].block_count,&endpoint_codelen_help );
			vqendpoints[best_p].codelen = RR_MIN(new_codelen,vqendpoints[best_p].codelen);
			#endif
			
			if ( pal_mode == rrDXT1PaletteMode_Alpha )
				RR_ASSERT( DXT1_OneBitTransparent_Mask_Same(pblock->block_1bt_mask,pblock->cur) );
			
			// update lz_window :
			
			if ( best_p >= endpoint_vq_palette_num_to_search )
			{
				update_mtf_window(best_p,lz_window.data(),lz_window_size,config.max_num_added_entries_to_try_endpoints );
			}
		}
		
		}
		
		//======================================================
		// VQ INDICES
		
		// prep by making endpoint codelens from the pass we just did
		//	(note endpoint_counting changes here)

		endpoint_counting.clear();
		//endpoint_counting.reserve( vqendpoints.size() );
		for LOOPVEC(i,vqendpoints)
		{
			U32 c = vqendpoints[i].block_count;
			if ( c == 0 ) continue;
			// count == 1 will be deleted too, but we save them for now just to do counts_singles
			endpoint_counting.push_back();
			endpoint_counting.back().dw = vqendpoints[i].dw;
			endpoint_counting.back().count = c;
		}

		// don't be reducing here, we just want to count them :
		setup_palette_and_vq_codelen_help(&endpoint_codelen_help,endpoint_counting,nblocks,eEndPoints,eNoReduce);
		
		dw_to_codelen_hash.clear();
		dw_to_codelen_hash.reserve(endpoint_counting.size32());
		
		for LOOPVEC(i,endpoint_counting)
		{
			F32 codelen = vq_codelen_palette_count( endpoint_counting[i].count,&endpoint_codelen_help );
			U32 dw = endpoint_counting[i].dw;
			RR_ASSERT( dw_to_codelen_hash.find(dw) == NULL );
			dw_to_codelen_hash.insert( dw, codelen );
		}		

		// refill index_counting from the previous endpoint pass

		// this has zero effect :
		//Optimize_Indices_For_Assigned_Blocks(blocks,dwc_scratch);
		
		for LOOPVEC(i,blocks)
		{
			rrDXT1_VQ_Block * pblock = &blocks[i];
			indices[i] = pblock->cur.indices;
		}
		
		sort_and_count_uniques(&index_counting,indices);		
	
		//rrprintfvar(index_counting.size());
		
		if ( lambdai > 0 )
		{
			// index_counting is made

			// before first VQ iter
			//	do direct reduction of the indices :
			
			sort_dword_and_count_compare_count_highest_first(&index_counting);
			
			hacky_clamp_counts(&index_counting,nblocks);
			
			// vq_reduce also changes blocks->cur
			vector<dword_and_count> index_counting_reduced;
			bc1_indices_vq_reduce(dispatch,&index_counting_reduced,index_counting,lambdaf,nblocks,blocks.data(),pal_mode,config);
			
			index_counting.swap(index_counting_reduced);
			
			//index_counting now has the unique indices & their count but is not yet sorted
		}
		
		//=================================================
		// now make index initial VQ Table :	
				
		//rrprintfvar(index_counting.size());
		
		setup_palette_and_vq_codelen_help(&index_codelen_help,index_counting,nblocks,eIndices,eNoReduce);

		vqindices.clear();
		vqindices.resize( index_counting.size() );
		
		dw_to_index_hash.clear();
		dw_to_index_hash.reserve( index_counting.size32() );
		
		for LOOPVEC(i,index_counting)
		{
			rrDXT1_VQ_Entry & vqp = vqindices[i];
			vqp.dw = index_counting[i].dw;
			
			RR_ASSERT( index_counting[i].count >= 2 );
			
			//DXT1_ComputePalette(vqp.endpoints.u.c0,vqp.endpoints.u.c1,vqp.palette,pal_mode);
			vqp.codelen = vq_codelen_palette_count( index_counting[i].count,&index_codelen_help);
			
			vqp.block_link = -1;
			vqp.block_count = 0;
			
			RR_ASSERT( dw_to_index_hash.find(vqp.dw) == NULL );
			dw_to_index_hash.insert(vqp.dw,i);
		}
		
		{
		SIMPLEPROFILE_SCOPE_N(bc1rd_indices,vqindices.size32());
		
		int index_vq_palette_size_before = vqindices.size32();
		//rrprintfvar(index_vq_palette_size_before);
		
		// it does help a decent amount still to re-score the VQ palette here
		//	I still need it to find merges to flat at the moment
		//		because the merger doesn't go to flat
		//	    (merger now does do flats, that's not it)
		//	the decision here is just slightly more accurate
		//		uses VQD instead of SSD
		//		uses codelen for J instead of very rough estimate

		int index_vq_palette_num_to_search = RR_MIN(index_vq_palette_size_before,config.index_vq_palette_search_limit);
		
		//int lz_window[config.max_num_added_entries_to_try_indices];
		vector<int> lz_window; lz_window.resize(config.max_num_added_entries_to_try_indices);
		int lz_window_size = 0;
		
		// for each block
		for LOOPVEC(block_i,blocks)
		{
			// try block with something from VQ vqindices
			rrDXT1_VQ_Block * pblock = &blocks[block_i];
			const rrColorBlock4x4 & colors = pblock->colors;
			
			// pblock->cur has the output of the endpoint vq stage
			// cur_ssd = the error of vq endpoint with free choice of indices
			
			if ( pal_mode == rrDXT1PaletteMode_Alpha )
				RR_ASSERT( DXT1_OneBitTransparent_Mask_Same(pblock->block_1bt_mask,pblock->cur) );

			// best_ssd = best error with vq of indices too
			F32 best_vqd = (F32)RR_DXTC_ERROR_BIG;		
			F32 best_J = (F32)RR_DXTC_ERROR_BIG;
			int best_p = -1;
			
			// set up candidate endpoints and their codelens and palettes
			// "cur" is the result of previous endpoint vq pass
			U32 try_ep[2];
			F32 try_ep_codelen[2];
			RAD_ALIGN(rrColor32BGRA, palettes[2][4], 16);

			try_ep[0] = rrDXT1Block_EndPoints_AsU32(pblock->cur);
			try_ep[1] = rrDXT1Block_EndPoints_AsU32(pblock->second_best);

			for LOOP(i,2)
			{
				try_ep_codelen[i] = find_hash_codelen_or_escape(try_ep[i],dw_to_codelen_hash,&endpoint_codelen_help );
				DXT1_ComputePalette(try_ep[i],palettes[i],pal_mode);
			}

			// remember if we preferred first or second endpoints :
			U32 best_endpoints = try_ep[0];

			// precompute VQDs for index palette entries and LZ window entries in this block
			// this is part of the N^2 loop and where most of the work is now
			batch_compute_VQDs(dispatch,
				index_vqd_results.data(),
				vqindices.data(),c_unit_index_map,index_vq_palette_num_to_search,
				colors,palettes[0],palettes[1],
				*pblock->pActivity);
			batch_compute_VQDs(dispatch,
				index_vqd_results.data() + index_vq_palette_num_to_search*2,
				vqindices.data(),lz_window.data(),lz_window_size,
				colors,palettes[0],palettes[1],
				*pblock->pActivity);

			if ( pal_mode == rrDXT1PaletteMode_Alpha )
			{
				// reject indices if they don't have alpha bits right
				// this used to be an early reject but that messes with the simple loop
				// structure; so right now, compute the VQDs anyway then substitute them
				// with huge errors.
				override_VQDs_on_transparency_mismatch(
					index_vqd_results.data(),
					vqindices.data(),c_unit_index_map,index_vq_palette_num_to_search,
					try_ep[0],try_ep[1],
					pblock->block_1bt_mask);

				override_VQDs_on_transparency_mismatch(
					index_vqd_results.data() + index_vq_palette_num_to_search*2,
					vqindices.data(),lz_window.data(),lz_window_size,
					try_ep[0],try_ep[1],
					pblock->block_1bt_mask);
			}

			// At this point, we have everything ranked, all that's left to do is
			// a simple reduction loop to pick the best candidate.
			const int candidate_count = index_vq_palette_num_to_search + lz_window_size;
			const int vqd_count = candidate_count * 2;

			index_vqd_results[vqd_count] = -1.0f; // set up sentinel for scan loop

			for (int scan_i = 0; ; scan_i++) // no loop condition, handled below!
			{
				// we allow scan_i == vqd_count here; we have extra padding at the
				// end so this is safe.
				scan_i = find_next_below(index_vqd_results.data(), scan_i, best_vqd);
				if ( scan_i >= vqd_count ) // found our sentinel, we're done
					break;

				const F32 vqd = index_vqd_results[scan_i];
				RR_ASSERT( vqd < best_vqd );

				// vqd < best_vqd, but is it actually better? Figure out the
				// rate so we can calc J!
				const int search_i = scan_i >> 1; // 2 VQD values per candidate
				const int ep_i = scan_i & 1;

				F32 vqp_indexes_codelen;
				int vqp;
				if ( search_i < index_vq_palette_num_to_search )
				{
					vqp = search_i;
					vqp_indexes_codelen = vqindices[vqp].codelen;
				}
				else
				{
					int lzi = search_i-index_vq_palette_num_to_search;
					RR_ASSERT( lzi < lz_window_size );
					vqp = lz_window[lzi];
					vqp_indexes_codelen = vqindices[vqp].codelen;
					
					#if 1
					// codelen model either by lz offset
					//	or by palette frequency
					// take the min?
					// @@ meh needs work
					// -> this is a small win on wood_worn which is the test that relies on LZ window the most
					//	on all others it's a NOP
					F32 lz_codelen = vq_codelen_lz_tab[lzi];
					vqp_indexes_codelen = RR_MIN(lz_codelen,vqp_indexes_codelen);
					#endif
				}

				// Now we can check whether J improved, not just VQD
				F32 J = vq_J(vqd,vqp_indexes_codelen + try_ep_codelen[ep_i],lambdaf);
				
				if ( J < best_J )
				{
					best_J = J;
					best_vqd = vqd;
					best_p = vqp;
					best_endpoints = try_ep[ep_i];
				}
			}

			// consider sending an escape & non-vq encoding instead
			// this compares the best vq-index ssd vs "cur" which is the result of vq_reduce
			
			// NOTE :
			//	 "cur" was changed by vq_reduce
			//	we could also have kept the "cur" that came out of vq endpoints and try against that
			//	-> TRIED IT, hurts a lot!
			//  we could also re-evaluate vs baseline (using new code cost estimate) -> NOPE			
			
			#ifdef DO_bc1_indices_vq_reduce_CHANGE_INDICES
			// cur.indices may have been changed by bc1_indices_vq_reduce
			pblock->cur_vqd = VQD_BC1(dispatch,pblock->colors,pblock->cur,*pblock->pActivity,pal_mode);
			#else
			RR_ASSERT( pblock->cur_vqd == VQD_BC1(dispatch,pblock->colors,pblock->cur,*pblock->pActivity,pal_mode) );
			#endif
			
			U32 cur_indices = pblock->cur.indices;
			
			// find cur.indices in vqindices
			// start with the assumption that cur indices are an escape (not in vqindices)
			F32 cur_indices_codelen = index_codelen_help.codelen_escape;
			int cur_indices_found_p = -1;
			
			// see if cur is in palette to get a cheaper codelen, if it's not really an escape
			// this was hurting because it prevented add-at-end
			// but with explicit lz_window it helps now
			// -> small but uniform benefit
			
			/*
			for LOOPVEC(pp,vqindices)
			{
				if ( vqindices[pp].dw == cur_indices )
				{
					cur_indices_codelen = vqindices[pp].codelen;
					cur_indices_found_p = pp;
					break;
				}
			}
			*/
			
			{
				t_hash_u32_to_u32::entry_ptrc ep = dw_to_index_hash.find(cur_indices);
				if ( ep != NULL )
				{
					cur_indices_found_p = ep->data();
					cur_indices_codelen = vqindices[cur_indices_found_p].codelen;
					RR_ASSERT( vqindices[cur_indices_found_p].dw == cur_indices );
				}
			}
					
			// checking this or not makes no difference :
			//only do escape if its lower error than best from palette :
			//if ( pblock->cur_vqd < best_vqd )
			{
				F32 J = vq_J(pblock->cur_vqd,cur_indices_codelen + try_ep_codelen[0],lambdaf);
				
				#if 1
				if ( try_ep[0] != try_ep[1] ) // equality is very rare
				{
					// can try second endpoints as well
					rrDXT1Block alt;
					alt.indices = cur_indices;
					alt.endpoints = try_ep[1];
					
					F32 alt_vqd = VQD_BC1(dispatch,pblock->colors,alt,*pblock->pActivity,pal_mode);
					
					F32 alt_J = vq_J(alt_vqd,cur_indices_codelen + try_ep_codelen[1],lambdaf);
				
					// this is much worse for vcdiff on wood_worn
					//	something non-local happening :(
					//if ( alt_J < J )
					// require vqd too improve too :
					if ( alt_J < J && alt_vqd < pblock->cur_vqd )
					{
						J = alt_J;
						// hacky slamming these :
						pblock->cur_vqd = alt_vqd;
						try_ep[0] = try_ep[1];
					}			
				}
				#endif
			
			
				if ( J < best_J )
				{
					best_J = J;			
					best_vqd = pblock->cur_vqd;
					best_endpoints = try_ep[0];
		
					// pblock->cur left alone
						
					if ( cur_indices_found_p < 0 )
					{
						// not found in vqindices[]
						best_p = vqindices.size32();
			
						// add to palette :
						vqindices.push_back();
						rrDXT1_VQ_Entry & vqp = vqindices.back();
						//vqp.index_codelen = index_codelen_singleton;
						vqp.codelen = index_codelen_help.codelen_2count;
						// index_codelen as if I have a 2 count
						//	since someone using me again would make my count at least 2
						//	this makes it more desirable to reuse me than to send another escape
						
						vqp.dw = cur_indices;
						vqp.block_link = -1;
						vqp.block_count = 0;
						
						RR_ASSERT( dw_to_index_hash.find(cur_indices) == NULL );
						dw_to_index_hash.insert(cur_indices,best_p);	
					}
					else
					{
						best_p = cur_indices_found_p;
					}
				}
			}
	
			// change pblock->cur indices to the select vq palette entry
			// if escape happened these are a benign nop		
			pblock->cur.endpoints = best_endpoints;
			pblock->cur.indices = vqindices[best_p].dw;
			pblock->cur_vqd = best_vqd;
			pblock->cur_J = best_J;
			
			if ( pal_mode == rrDXT1PaletteMode_Alpha )
				RR_ASSERT( DXT1_OneBitTransparent_Mask_Same(pblock->block_1bt_mask,pblock->cur) );

			// record that best_p entry was used for this block :
			pblock->vq_entry_index = best_p;
			pblock->vq_entry_link = vqindices[best_p].block_link;
			vqindices[best_p].block_link = block_i;
			vqindices[best_p].block_count ++;
			
			// update lz_window :
			
			if ( best_p >= index_vq_palette_num_to_search )
			{
				update_mtf_window(best_p,lz_window.data(),lz_window_size,config.max_num_added_entries_to_try_indices);
			}
			
		}
		
		} // profile scope
		
		// if iterating, scan out to "index_counting" to start again :
		
		index_counting.clear();
		index_counting.reserve( vqindices.size() );
		for LOOPVEC(i,vqindices)
		{
			U32 c = vqindices[i].block_count;
			if ( c == 0 ) continue;
			index_counting.push_back();
			index_counting.back().dw = vqindices[i].dw;
			index_counting.back().count = c;
		}

		// refill endpoint counting for next pass
		//	(it was changed by setup codelens & endpoints may have changed in the index vq loop)

		// doing this in-loop here helps a tiny bit :
		//	(the main benefit of it is in the "final_optimize"
		// note that doing this here breaks stored cur_vqd and cur_J
		Optimize_Endpoints_For_Assigned_Blocks(dispatch,blocks,dwc_scratch,pal_mode);
		
		for LOOPVEC(i,blocks)
		{
			// try block with something from VQ vqendpoints
			rrDXT1_VQ_Block * pblock = &blocks[i];
			endpoints[i] = pblock->cur.endpoints;
		}
		
		sort_and_count_uniques(&endpoint_counting,endpoints);
	} // for vq_iterations	
	
	} // profile scope vq_iters
	
	//=========================================================
	
	bc1rd_final_matrix(dispatch,
		blocks,
		endpoint_counting,index_counting,
		lambdaf,lambdai,pal_mode,config);
	
	//=========================================================
	
	#if 1
	{
		SIMPLEPROFILE_SCOPE(bc1rd_final_optimize);
	
		// now take the set of blocks assigned to each VQ entry
		//	and re-find the indexes/endpoints that optimize for that set of blocks
		//	assuming opposite endpoints/indexes fixed

		// was just done at end of iters :
		//Optimize_Endpoints_For_Assigned_Blocks(blocks,dwc_scratch);
	
		Optimize_Indices_For_Assigned_Blocks(dispatch,blocks,dwc_scratch,pal_mode);
		
		// and again : does help a tiny bit :
		Optimize_Endpoints_For_Assigned_Blocks(dispatch,blocks,dwc_scratch,pal_mode);
	}
	#endif
	
	//=========================================================
	// put to output surface :

	{
		SIMPLEPROFILE_SCOPE_N(bc1rd_emit_output,nblocks);
		for LOOP(bi,nblocks)
		{
			U8 * outPtr = BlockSurface_Seek(to_blocks,bi);

			const rrDXT1_VQ_Block * pblock = &blocks[bi];

			if ( pal_mode == rrDXT1PaletteMode_Alpha )
				RR_ASSERT( DXT1_OneBitTransparent_Mask_Same(pblock->block_1bt_mask,pblock->cur) );

			memcpy(outPtr,&(pblock->cur),sizeof(pblock->cur));
		}
	}

	return true;
}

// copy-paste town
// just the same as normal DXT1_OptimizeEndPointsFromIndices
//	but iterates over blocks
static bool DXT1_OptimizeEndPointsFromIndices_Inherit_MultiBlock(U32 * pEndPoints, bool fourc, 
	const vector<rrDXT1_VQ_Block> & blocks, 
	const vector<dword_and_count> & dcv, int dcv_i_start, int dcv_i_end,
	rrDXT1PaletteMode pal_mode)
{
	// find endpoints that minimize sum of errors across N blocks
	// that just means add up the terms from all the blocks
	BC1EndpointLLSSolver solver(fourc);
	
	// walk all blocks that use this vq entry :
	U32 any_index3_mask = 0; // positions where we use index 3 in any block
	for(int dcvi=dcv_i_start;dcvi<dcv_i_end;dcvi++)
	{
		int bi = dcv[dcvi].dw;
		const rrDXT1_VQ_Block * pblock = &blocks[bi];
		//RR_ASSERT( pblock->cur.endpoints == oldep );

		const U32 indices = pblock->cur.indices;

		// keep track of whether we have any uses of index 3
		// if so, we're not allowed to change modes
		any_index3_mask |= DXT1_OneBitTransparent_Mask_FromIndices( true, indices );

		solver.accumulate(pblock->colors,indices);
	} // for all blocks in link
	
	// solver now minimizes average error over all blocks
	rrColor565Bits qA, qB;
	if ( ! solver.solve_endpoints(&qA,&qB) )
		return false;

	// note in degenerate cases if qA == qB , that's always 3-index
	
	// switch endpoints to maintain four-color state :
	
	// NOTE : switching endpoints makes them no longer be the lsqr solve for these indices!
	//	they are now the lsqr solve for flip(indices)
	//	re-indexing fixes this but if you don't reindex, it may be much worse
	
	if ( qA.w < qB.w )
	{
		RR_NAMESPACE::swap(qA.w,qB.w);
	}
	
	if ( ! fourc )
		RR_NAMESPACE::swap(qA,qB);
	
	if ( pal_mode == rrDXT1PaletteMode_Alpha )
	{
		// If we care about alpha, and any of our blocks use index 3 anywhere,
		// then we can't change endpoint order from original because this would
		// change 1-bit transparency state.
		bool new_is_fourc = qA.w > qB.w;
		if ( any_index3_mask != 0 && new_is_fourc != fourc )
			return false;
	}

	rrDXT1EndPoints endpoints;
	endpoints.u.c0 = qA;
	endpoints.u.c1 = qB;
	*pEndPoints = endpoints.dw;

	return true;
}

/***

Optimize_Endpoints_For_Assigned_Blocks

done as very final pass

does not break any index or endpoint sharing
  (indices are not changed at all)
  
endpoints that were a set stay a set

so this should be close to rate-invariant

just look for Distortion gains that can be made without changing Rate

for all the blocks that are assigned to some endpoints oldep
	see if there is some endpoints newep
	that can be used on all of them (indices kept the same)
	and improve error
	
can just use SSD here (not VQD)
because this is not a rate allocation problem

***/

static void Optimize_Endpoints_For_Assigned_Blocks(
	CpuDispatchFlags dispatch,
	vector<rrDXT1_VQ_Block> & blocks,
	vector<dword_and_count> & dcv,
	rrDXT1PaletteMode pal_mode)
{
	// optimize palette entries from blocks mapped to them
	
	int nblocks = blocks.size32();
	
	// sort by endpoints
	//	with block index as payload
	//	to get runs of same endpoint
	dcv.clear();
	dcv.resize(nblocks);
	
	for LOOPVEC(b,blocks)
	{
		U32 endpoints = blocks[b].cur.endpoints;
		
		dcv[b].dw = b;
		dcv[b].count = endpoints;
	}
	
	// sort by "count" (that's endpoint value) :
	sort_dword_and_count_compare_count_highest_first(&dcv);
		
	// now for each vq palette entry :
	// look at blocks that mapped to that entry
	// choose new endpoints that optimize for those blocks
	// (endpoints-from-indices on the combined matrix)

	// (if there's only one block on the entry, AND its == baseline, can skip this)
	// (if there's only one block on the entry, and its not baseline, see if replacing it with baseline is better)
	
	int dcv_i_start = 0;
	while(dcv_i_start<nblocks)
	{
		int bi_start = dcv[dcv_i_start].dw;
		U32 endpoints = blocks[bi_start].cur.endpoints;
		int dcv_i_end = dcv_i_start+1;
		while( dcv_i_end < nblocks && blocks[ dcv[dcv_i_end].dw ].cur.endpoints == endpoints )
		{
			dcv_i_end++;
		}
		
		int count = dcv_i_end - dcv_i_start;
		RR_ASSERT( count > 0 );
		
		if ( count == 1 )
		{
			// get the one block that mapped to me :
			rrDXT1_VQ_Block * pblock = &blocks[bi_start];
			
			#if 0
			if ( pblock->baseline_vqd < pblock->cur_vqd )
			{
				// block thought it was doing VQ to a shared palette p
				//	but it wound up as the only one mapped there
				// so just use baseline
			
				// can't just do this
				//	because cur indices may have been chosen to be cheaper than baseline indices
				//  this was okay when we were doin this before hte indexes phase
			
				pblock->cur = pblock->baseline;
				pblock->cur_vqd = pblock->baseline_vqd;
			}
			#endif
			
			// cur == baseline is quite common at low lambda
			if ( pblock->cur != pblock->baseline )
			{			
				U32 err = DXT1_ComputeSSD_RGBA(pblock->colors,pblock->cur,pal_mode);
		
				// changes pblock->cur.endpoints if it finds a win
				// never switches between 3c and 4c mode so transparent pixels should be fine
				DXT1_OptimizeEndPointsFromIndices_Inherit_NoReindex(dispatch,&(pblock->cur),&err,pblock->colors,pal_mode);
				
				if ( pal_mode == rrDXT1PaletteMode_Alpha )
					RR_ASSERT( DXT1_OneBitTransparent_Mask_Same(pblock->block_1bt_mask,pblock->cur) );

				if ( pal_mode == rrDXT1PaletteMode_FourColor )
				{
					RR_ASSERT( rrDXT1Block_IsBC3Canonical(&pblock->cur) );
					// no need, should already be so because of Inherit :
					//rrDXT1Block_BC3_Canonicalize(&(pblock->cur));
				}
				
				#if 0
				// see if baseline endpoints are better
				// this can happen because the VerySlow encoder that made baseline
				//	is better than just doing OptimizeEndPointsFromIndices
				// -> this is hit non-zero times but it is very rare
				rrDXT1Block test;
				test.endpoints = pblock->baseline.endpoints;
				test.indices = pblock->cur.indices;
				U32 test_err = DXT1_ComputeSSD_RGBA(pblock->colors,test,pal_mode);
				if ( test_err < err )
				{
					pblock->cur = test;
				}
				#endif
			}
		}
		else // count >= 2
		{
			// multi-block endpoints from indices
			
			U32 oldep = endpoints;
			bool fourc = DXT1_Is4Color(oldep,pal_mode);
			
			U32 newep;
			if ( DXT1_OptimizeEndPointsFromIndices_Inherit_MultiBlock(&newep,fourc,blocks,dcv,dcv_i_start,dcv_i_end,pal_mode) )
			{
				if ( pal_mode == rrDXT1PaletteMode_FourColor )
				{
					RR_ASSERT( rrDXT1Block_IsBC3Canonical(newep) );
				}
			
				if ( newep != oldep )
				{
					// need to compute err before & after to see if it actually helped
				
					// if it did, then walk the blocks and assign the new ep :
					
					rrColor32BGRA oldpalette[4];
					DXT1_ComputePalette(oldep,oldpalette,pal_mode);
					
					rrColor32BGRA newpalette[4];
					DXT1_ComputePalette(newep,newpalette,pal_mode);

					U64 total_ssd_before = 0;
					U64 total_ssd_after = 0;

					// walk all blocks that use this vq entry :
					for(int dcvi=dcv_i_start;dcvi<dcv_i_end;dcvi++)
					{
						int bi = dcv[dcvi].dw;
						const rrDXT1_VQ_Block * pblock = &blocks[bi];
						RR_ASSERT( pblock->cur.endpoints == oldep );
						
						U32 indices = pblock->cur.indices;
						
						/*
						// do NOT reindex
						U32 err;
						U32 indices = DXT1_FindIndices(pblock->colors,palette,pal_mode,&err);
						*/
						
						U32 oldssd = BC1_Palette_SSD_RGBA(&pblock->colors,oldpalette,indices);
						U32 newssd = BC1_Palette_SSD_RGBA(&pblock->colors,newpalette,indices);
						
						total_ssd_before += oldssd;
						total_ssd_after += newssd;						
					}
					
					// semi-random , sometimes it goes up, sometimes down
					// (except if endpoints had to flip to maintain fourc state, then error is always big)
					
					if ( total_ssd_after < total_ssd_before )
					{
						//rrprintfvar(total_ssd_before);
						//rrprintfvar(total_ssd_after);
					
						// okay, do it :
						
						for(int dcvi=dcv_i_start;dcvi<dcv_i_end;dcvi++)
						{
							int bi = dcv[dcvi].dw;
							rrDXT1_VQ_Block * pblock = &blocks[bi];
							RR_ASSERT( pblock->cur.endpoints == oldep );

							pblock->cur.endpoints = newep;

							if ( pal_mode == rrDXT1PaletteMode_Alpha )
								RR_ASSERT( DXT1_OneBitTransparent_Mask_Same(pblock->block_1bt_mask,pblock->cur) );
						}
					}
				}
			}
		}
		
		dcv_i_start = dcv_i_end;
	}
}

static bool DXT1_FindIndices_MultiBlock(U32 *p_new_indices,U32 old_indices,
	const vector<rrDXT1_VQ_Block> & blocks, 
	const vector<dword_and_count> & dcv, int dcv_i_start, int dcv_i_end,
	rrDXT1PaletteMode pal_mode)
{
	// just a normal FindIndices
	//	for each of 16 colors, compute 4 distances to palette
	// we just sum all the distances across N blocks
	// before doing the min-of-4

	U32 old_err = 0;
	AnyIndexD aid = { };
	
	// walk all blocks that use this vq entry :
	for(int dcvi=dcv_i_start;dcvi<dcv_i_end;dcvi++)
	{
		int bi = dcv[dcvi].dw;
		const rrDXT1_VQ_Block * pblock = &blocks[bi];
		RR_ASSERT( pblock->cur.indices == old_indices );
	
		if ( pal_mode == rrDXT1PaletteMode_Alpha )
			RR_ASSERT( DXT1_OneBitTransparent_Mask_Same(pblock->block_1bt_mask,pblock->cur) );

		rrColor32BGRA palette[4];
		DXT1_ComputePalette(pblock->cur.endpoints,palette,pal_mode);
		
		const rrColorBlock4x4 & colors = pblock->colors;

		// get old SSD to compare to
		old_err += BC1_Palette_SSD_RGBA(&colors,palette,old_indices);

		// accumulate distances for palette entries
		if ( pal_mode != rrDXT1PaletteMode_Alpha )
		{
			AnyIndexD_add(&aid,colors,palette);
		}
		else
		{
			// regular AnyIndexD_add is not careful about alpha
			AnyIndexD_add_with_1bt(&aid,colors,palette);
		}
	}
	
	// all blocks added up, now find best index for each pixel pos :
	U32 new_err = 0;
	*p_new_indices = AnyIndexD_find_indices(&aid,&new_err);
	
	RR_ASSERT( new_err <= old_err );
	
	bool better = new_err < old_err;
	return better;
}

/**

Optimize_Indices_For_Assigned_Blocks

for each index vq palette entry
	look at the blocks actually assigned to it
	optimize the index values for those blocks

only changes indices, not endpoints

only changes whole groups of index value
	all occurances of I1 change to I2
so the # of unique indices & count of each doesn't change
so it should be roughly codelen invariant
only distortion changes

**/

static void Optimize_Indices_For_Assigned_Blocks(
	CpuDispatchFlags dispatch,
	vector<rrDXT1_VQ_Block> & blocks,
	vector<dword_and_count> & dcv,
	rrDXT1PaletteMode pal_mode)
{
	// optimize palette entries from blocks mapped to them
	
	int nblocks = blocks.size32();
	
	// sort by indices
	//	with block index as payload
	//	to get runs of same indices
	dcv.clear();
	dcv.resize(nblocks);
	
	for LOOPVEC(b,blocks)
	{
		dcv[b].dw = b;
		dcv[b].count = blocks[b].cur.indices;
	}
	
	// sort by "count" (that's indices value) :
	sort_dword_and_count_compare_count_highest_first(&dcv);
		
	// now for each vq palette entry :
	// look at blocks that mapped to that entry
	// choose new indices that optimize for those blocks

	// (if there's only one block on the entry, AND its == baseline, can skip this)
	// (if there's only one block on the entry, and its not baseline, see if replacing it with baseline is better)
	
	int dcv_i_start = 0;
	while(dcv_i_start<nblocks)
	{
		int bi_start = dcv[dcv_i_start].dw;
		U32 old_indices = blocks[bi_start].cur.indices;
		int dcv_i_end = dcv_i_start+1;
		while( dcv_i_end < nblocks && blocks[ dcv[dcv_i_end].dw ].cur.indices == old_indices )
		{
			dcv_i_end++;
		}
		
		int count = dcv_i_end - dcv_i_start;
		RR_ASSERT( count > 0 );
		
		if ( count == 1 )
		{
			// get the one block that mapped to me :
			rrDXT1_VQ_Block * pblock = &blocks[bi_start];
						
			// cur == baseline is quite common at low lambda
			if ( pblock->cur != pblock->baseline )
			{			
				U32 old_err = DXT1_ComputeSSD_RGBA(pblock->colors,pblock->cur,pal_mode);
		
				// changes pblock->cur.endpoints if it finds a win
				U32 new_err = RR_DXTC_INIT_ERROR;
				U32 new_indices = DXT1_FindIndices(dispatch,pblock->colors,pblock->cur.endpoints,pal_mode,&new_err);
				
				RR_ASSERT( new_err <= old_err );
	
				if ( new_err < old_err )
				{
					pblock->cur.indices = new_indices;
				}
			}
		}
		else // count >= 2
		{
			// multi-block best indices
			
			U32 new_indices = old_indices;
			if ( DXT1_FindIndices_MultiBlock(&new_indices,old_indices,blocks,dcv,dcv_i_start,dcv_i_end,pal_mode) )
			{
				// yes, commit the change :
			
				for(int dcvi=dcv_i_start;dcvi<dcv_i_end;dcvi++)
				{
					int bi = dcv[dcvi].dw;
					rrDXT1_VQ_Block * pblock = &blocks[bi];
					RR_ASSERT( pblock->cur.indices == old_indices );
					pblock->cur.indices = new_indices;
					
					if ( pal_mode == rrDXT1PaletteMode_Alpha )
						RR_ASSERT( DXT1_OneBitTransparent_Mask_Same(pblock->block_1bt_mask,pblock->cur) );
				}
			}
		}
		
		dcv_i_start = dcv_i_end;
	}
}
				
//===============================================================
// rrdxt1vqindices

// 4c indices -> 0,1,2,3
static const U32 c_bc1_indices_to_bytes_LE_4c[256] = 
{
  0x00000000,0x00000003,0x00000001,0x00000002,0x00000300,0x00000303,0x00000301,0x00000302,
  0x00000100,0x00000103,0x00000101,0x00000102,0x00000200,0x00000203,0x00000201,0x00000202,
  0x00030000,0x00030003,0x00030001,0x00030002,0x00030300,0x00030303,0x00030301,0x00030302,
  0x00030100,0x00030103,0x00030101,0x00030102,0x00030200,0x00030203,0x00030201,0x00030202,
  0x00010000,0x00010003,0x00010001,0x00010002,0x00010300,0x00010303,0x00010301,0x00010302,
  0x00010100,0x00010103,0x00010101,0x00010102,0x00010200,0x00010203,0x00010201,0x00010202,
  0x00020000,0x00020003,0x00020001,0x00020002,0x00020300,0x00020303,0x00020301,0x00020302,
  0x00020100,0x00020103,0x00020101,0x00020102,0x00020200,0x00020203,0x00020201,0x00020202,
  0x03000000,0x03000003,0x03000001,0x03000002,0x03000300,0x03000303,0x03000301,0x03000302,
  0x03000100,0x03000103,0x03000101,0x03000102,0x03000200,0x03000203,0x03000201,0x03000202,
  0x03030000,0x03030003,0x03030001,0x03030002,0x03030300,0x03030303,0x03030301,0x03030302,
  0x03030100,0x03030103,0x03030101,0x03030102,0x03030200,0x03030203,0x03030201,0x03030202,
  0x03010000,0x03010003,0x03010001,0x03010002,0x03010300,0x03010303,0x03010301,0x03010302,
  0x03010100,0x03010103,0x03010101,0x03010102,0x03010200,0x03010203,0x03010201,0x03010202,
  0x03020000,0x03020003,0x03020001,0x03020002,0x03020300,0x03020303,0x03020301,0x03020302,
  0x03020100,0x03020103,0x03020101,0x03020102,0x03020200,0x03020203,0x03020201,0x03020202,
  0x01000000,0x01000003,0x01000001,0x01000002,0x01000300,0x01000303,0x01000301,0x01000302,
  0x01000100,0x01000103,0x01000101,0x01000102,0x01000200,0x01000203,0x01000201,0x01000202,
  0x01030000,0x01030003,0x01030001,0x01030002,0x01030300,0x01030303,0x01030301,0x01030302,
  0x01030100,0x01030103,0x01030101,0x01030102,0x01030200,0x01030203,0x01030201,0x01030202,
  0x01010000,0x01010003,0x01010001,0x01010002,0x01010300,0x01010303,0x01010301,0x01010302,
  0x01010100,0x01010103,0x01010101,0x01010102,0x01010200,0x01010203,0x01010201,0x01010202,
  0x01020000,0x01020003,0x01020001,0x01020002,0x01020300,0x01020303,0x01020301,0x01020302,
  0x01020100,0x01020103,0x01020101,0x01020102,0x01020200,0x01020203,0x01020201,0x01020202,
  0x02000000,0x02000003,0x02000001,0x02000002,0x02000300,0x02000303,0x02000301,0x02000302,
  0x02000100,0x02000103,0x02000101,0x02000102,0x02000200,0x02000203,0x02000201,0x02000202,
  0x02030000,0x02030003,0x02030001,0x02030002,0x02030300,0x02030303,0x02030301,0x02030302,
  0x02030100,0x02030103,0x02030101,0x02030102,0x02030200,0x02030203,0x02030201,0x02030202,
  0x02010000,0x02010003,0x02010001,0x02010002,0x02010300,0x02010303,0x02010301,0x02010302,
  0x02010100,0x02010103,0x02010101,0x02010102,0x02010200,0x02010203,0x02010201,0x02010202,
  0x02020000,0x02020003,0x02020001,0x02020002,0x02020300,0x02020303,0x02020301,0x02020302,
  0x02020100,0x02020103,0x02020101,0x02020102,0x02020200,0x02020203,0x02020201,0x02020202
};

const int color3_index_map = 5;

// 3c indices -> 0,1,2,5
static const U32 c_bc1_indices_to_bytes_LE_3c[256] = 
{
  0x00000000,0x00000002,0x00000001,0x00000005,0x00000200,0x00000202,0x00000201,0x00000205,
  0x00000100,0x00000102,0x00000101,0x00000105,0x00000500,0x00000502,0x00000501,0x00000505,
  0x00020000,0x00020002,0x00020001,0x00020005,0x00020200,0x00020202,0x00020201,0x00020205,
  0x00020100,0x00020102,0x00020101,0x00020105,0x00020500,0x00020502,0x00020501,0x00020505,
  0x00010000,0x00010002,0x00010001,0x00010005,0x00010200,0x00010202,0x00010201,0x00010205,
  0x00010100,0x00010102,0x00010101,0x00010105,0x00010500,0x00010502,0x00010501,0x00010505,
  0x00050000,0x00050002,0x00050001,0x00050005,0x00050200,0x00050202,0x00050201,0x00050205,
  0x00050100,0x00050102,0x00050101,0x00050105,0x00050500,0x00050502,0x00050501,0x00050505,
  0x02000000,0x02000002,0x02000001,0x02000005,0x02000200,0x02000202,0x02000201,0x02000205,
  0x02000100,0x02000102,0x02000101,0x02000105,0x02000500,0x02000502,0x02000501,0x02000505,
  0x02020000,0x02020002,0x02020001,0x02020005,0x02020200,0x02020202,0x02020201,0x02020205,
  0x02020100,0x02020102,0x02020101,0x02020105,0x02020500,0x02020502,0x02020501,0x02020505,
  0x02010000,0x02010002,0x02010001,0x02010005,0x02010200,0x02010202,0x02010201,0x02010205,
  0x02010100,0x02010102,0x02010101,0x02010105,0x02010500,0x02010502,0x02010501,0x02010505,
  0x02050000,0x02050002,0x02050001,0x02050005,0x02050200,0x02050202,0x02050201,0x02050205,
  0x02050100,0x02050102,0x02050101,0x02050105,0x02050500,0x02050502,0x02050501,0x02050505,
  0x01000000,0x01000002,0x01000001,0x01000005,0x01000200,0x01000202,0x01000201,0x01000205,
  0x01000100,0x01000102,0x01000101,0x01000105,0x01000500,0x01000502,0x01000501,0x01000505,
  0x01020000,0x01020002,0x01020001,0x01020005,0x01020200,0x01020202,0x01020201,0x01020205,
  0x01020100,0x01020102,0x01020101,0x01020105,0x01020500,0x01020502,0x01020501,0x01020505,
  0x01010000,0x01010002,0x01010001,0x01010005,0x01010200,0x01010202,0x01010201,0x01010205,
  0x01010100,0x01010102,0x01010101,0x01010105,0x01010500,0x01010502,0x01010501,0x01010505,
  0x01050000,0x01050002,0x01050001,0x01050005,0x01050200,0x01050202,0x01050201,0x01050205,
  0x01050100,0x01050102,0x01050101,0x01050105,0x01050500,0x01050502,0x01050501,0x01050505,
  0x05000000,0x05000002,0x05000001,0x05000005,0x05000200,0x05000202,0x05000201,0x05000205,
  0x05000100,0x05000102,0x05000101,0x05000105,0x05000500,0x05000502,0x05000501,0x05000505,
  0x05020000,0x05020002,0x05020001,0x05020005,0x05020200,0x05020202,0x05020201,0x05020205,
  0x05020100,0x05020102,0x05020101,0x05020105,0x05020500,0x05020502,0x05020501,0x05020505,
  0x05010000,0x05010002,0x05010001,0x05010005,0x05010200,0x05010202,0x05010201,0x05010205,
  0x05010100,0x05010102,0x05010101,0x05010105,0x05010500,0x05010502,0x05010501,0x05010505,
  0x05050000,0x05050002,0x05050001,0x05050005,0x05050200,0x05050202,0x05050201,0x05050205,
  0x05050100,0x05050102,0x05050101,0x05050105,0x05050500,0x05050502,0x05050501,0x05050505
};

// unpack 2 bit indices -> 16x8 byte vector
//	 also remap to linear order 0->3
static void bc1_indices_4c_unpack_to_16x8(U8 * dest,U32 indices)
{
	// low bits of indices are first in 4x4 scan order
	RR_PUT32_LE(dest   , c_bc1_indices_to_bytes_LE_4c[ (indices    )&0xFF ] );
	RR_PUT32_LE(dest+4 , c_bc1_indices_to_bytes_LE_4c[ (indices>>8 )&0xFF ] );
	RR_PUT32_LE(dest+8 , c_bc1_indices_to_bytes_LE_4c[ (indices>>16)&0xFF ] );
	RR_PUT32_LE(dest+12, c_bc1_indices_to_bytes_LE_4c[ (indices>>24)&0xFF ] );
}

static void bc1_indices_3c_unpack_to_16x8(U8 * dest,U32 indices)
{
	// low bits of indices are first in 4x4 scan order
	RR_PUT32_LE(dest   , c_bc1_indices_to_bytes_LE_3c[ (indices    )&0xFF ] );
	RR_PUT32_LE(dest+4 , c_bc1_indices_to_bytes_LE_3c[ (indices>>8 )&0xFF ] );
	RR_PUT32_LE(dest+8 , c_bc1_indices_to_bytes_LE_3c[ (indices>>16)&0xFF ] );
	RR_PUT32_LE(dest+12, c_bc1_indices_to_bytes_LE_3c[ (indices>>24)&0xFF ] );
}

#ifdef __RADSSE2__
	
static inline U32 sse2_u8x16_ssd( const __m128i & v1, const __m128i & v2 )
{
	__m128i sub8 = _mm_or_si128( _mm_subs_epu8(v1,v2), _mm_subs_epu8(v2,v1) );
	__m128i sub16_1 = _mm_unpacklo_epi8(sub8, _mm_setzero_si128() );
	__m128i sub16_2 = _mm_unpackhi_epi8(sub8, _mm_setzero_si128() );
	
// alternative : (SSSE3)
//  __m128i plus_minus = _mm_setr_epi8( 1,-1,1,-1, 1,-1,1,-1, 1,-1,1,-1, 1,-1,1,-1 );
//	or plus_minus == _mm_set1_epi16(0x1FF);
//	__m128i sub16_1 = _mm_maddubs_epi16(_mm_unpacklo_epi8(v1, v2), plus_minus);
//	__m128i sub16_2 = _mm_maddubs_epi16(_mm_unpackhi_epi8(v1, v2), plus_minus);

	__m128i squares32_1 = _mm_madd_epi16(sub16_1,sub16_1);
	__m128i squares32_2 = _mm_madd_epi16(sub16_2,sub16_2);
	
	__m128i squares32 = _mm_add_epi32(squares32_1,squares32_2);
	
	U32 ssd = reduce_add_u32(squares32);
	
	return ssd;
}
#endif
	
struct index_vq_entry
{
	U8 unpacked_16x8[16]; // aligned 16 @@ temp not
	U32 indices;
	rrbool is3c;
	U32 distortion_sum;
	U32 count;
	U32 count_log2_count;
	S32 block_link; // linked list of blocks with these indices
	S32 merged_onto; // negative if not merged
	S32 best_distortion; // updated throughout loop
	S32 sad_bound_good_until; // as long as best_distortion >= this, sad_bound is current
	S32 sad_bound;
};

// meh , for experimenting with _load_ vs _loadu_ :
//RR_COMPILER_ASSERT( sizeof(index_vq_entry) == 16*3 );

struct index_vq_heap_entry
{
	int fm,to;
	F32 dj; // positive dj = good
	int pair_count_save;
};

// normal operator less will give a heap that pops largest first :
static bool operator < (const index_vq_heap_entry &lhs, const index_vq_heap_entry &rhs)
{
	return lhs.dj < rhs.dj;
}

/**

one way :

sort indices by count
only look for merges from higher index -> lower index
start at index 0, then only compute J when delta goes down
  at equal delta you would always prefer higher count
push all those to the heap

the heap is not full (it doesn't have all dests)
so now when you pop an item :
if "from" is already merged, skip it
if "to" is already merged, you can't skip it
  instead find what to was merged onto and make a merge candidate onto that

**/

static F32 index_merge_dj( const index_vq_entry & from, const index_vq_entry & to, S32 delta_D , F32 lambda) // int nblocks )
{
	/*
		
	LZ scheme J is
		D + lambda * R
	D ~ 28 * SAD
	D ~ 2 * SSD
	R ~ 16 * bits

	*/
	
	// diff = sum of SSD*2's
	//	no need to multiply by count as we've already accumulated across the N blocks
	F32 D = (F32)delta_D;
	
	
	// each entry has to send 32 bit raw indices + count * selection of those indices	
	//	same rate model as vq_codelen_palette_count
	
	// when you do a merge, the main rate savings is the 32 bits for one less index value
	//	but you also save some because the selection gets cheaper
	
	// @@ indexes actually take slightly less than 32 bits
	//	should tweak what that number is ; 30 ?
	
	// log2tabled returns values scaled up by RR_LOG2TABLE_ONE
	// the denominator terms cancel out
	U32 merged_count = from.count + to.count;
	
	// count*log2count is cached in index_vq_entry :
	U32 log2one_from = from.count_log2_count;
	U32 log2one_to = to.count_log2_count;
	
	RR_ASSERT( log2one_from == from.count * log2tabled_bk_32(from.count) );
	RR_ASSERT( log2one_to   == to.count * log2tabled_bk_32(to.count) );
	
	U32 log2one_merged   = merged_count * log2tabled_bk_32(merged_count);
	U32 log2one_delta = log2one_from + log2one_to - log2one_merged;
	
	// scale from log2 one to vq_codelen fixed point :
	// @@ round? or just truncate
	F32 rate_delta = log2one_delta * (1.f / (F32)RR_LOG2TABLE_ONE);
	
	rate_delta += 30.f; // 32 raw index bits - 2 bias
	RR_ASSERT( rate_delta >= 32.f );

	F32 R  = rate_delta;
	
//prev BPB saved at vcdiff +1.0 : [ 12.688 ]

	// R and D are positive here
	//	D is the distortion cost of the merge, R is the rate savings
	//	(D can sometimes by slightly negative when an index change actually helps a block)
	
	F32 dJ = vq_J(-D,R,lambda);
	// dJ > 0 is a good merge
	//	the rate savings is enough to justify the distortion penalty
	//return (int)(dJ + 0.5);
	return dJ;
}

static U32 block_link_to_AnyIndexD(const index_vq_entry * entry, const rrDXT1_VQ_Block * blocks, AnyIndexD * aid, rrDXT1PaletteMode pal_mode )
{
	int link = entry->block_link;
	U32 link_count = 0;
	
	// add SSD's into aid from each block
	RR_ZERO(*aid);
	
	while( link >= 0 )
	{
		#ifdef DO_bc1_indices_vq_reduce_CHANGE_INDICES
		RR_ASSERT( blocks[link].cur.indices == entry->indices );
		#endif

		const rrColorBlock4x4 & colors = blocks[link].colors;
		
		U32 ep = blocks[link].cur.endpoints;	
		
		if ( pal_mode == rrDXT1PaletteMode_Alpha )
			RR_ASSERT( DXT1_OneBitTransparent_Mask_Same(blocks[link].block_1bt_mask,ep,entry->indices) );

		rrColor32BGRA palette[4];
		DXT1_ComputePalette(ep,palette,pal_mode);
		
		AnyIndexD_add(aid,colors,palette);
				
		link_count++;
		link = blocks[link].vq_entry_link;
	}
	RR_ASSERT( link_count == entry->count );
	return entry->count;	
}

static S32 bc1rd_compute_index_sad_bound(const AnyIndexD &fm, S32 ssd_limit, bool is3c, const U8 *indices, S32 * out_ssd_limit)
{
	//SIMPLEPROFILE_SCOPE(compute_index_sad_bound);

	int ind_limit = is3c ? color3_index_map : 3;
	S32 ssd_scratch[8 + 8 + 8];
	S32 total_ssd=0;

	RR_ASSERT(ind_limit <= 8); // 8 is the padding we have

	// Put sentinels in for out-of-range index values
	for (int i = 0; i < 8; i++)
		ssd_scratch[i] = 0x7fffffff;
	for (int i = 8 + ind_limit + 1; i < 8 + 8 + 8; i++)
		ssd_scratch[i] = 0x7fffffff;

	// initialize ssd_for_index_delta to sentinel vals
	S32 ssd_for_index_delta[16][8];
	for (int i=0; i < 16; ++i)
		for (int j=0; j < 8; ++j)
			ssd_for_index_delta[i][j] = 0x7fffffff;

	for (int p=0; p < 16; ++p)
	{
		int ind = indices[p];
		int n = RR_MAX(ind, ind_limit - ind); // max useful change to ind

		// copy SSD values for p into ssd_scratch in linear order
		if (is3c)
		{
			// color 2 is in between 0 and 1, color 3 (transparent) is way off
			ssd_scratch[8 + 0] = fm.ssd[0][p];
			ssd_scratch[8 + 1] = fm.ssd[2][p];
			ssd_scratch[8 + 2] = fm.ssd[1][p];
			for (int i = 3; i <= color3_index_map; i++)
				ssd_scratch[8 + i] = fm.ssd[3][p];
		}
		else
		{
			// colors 0 and 1 are at either end in tight indexing,
			// color 2 is closer to 0, color 3 is closer to 1.
			ssd_scratch[8 + 0] = fm.ssd[0][p];
			ssd_scratch[8 + 1] = fm.ssd[2][p];
			ssd_scratch[8 + 2] = fm.ssd[3][p];
			ssd_scratch[8 + 3] = fm.ssd[1][p];
		}

		S32 prev_ssd = ssd_scratch[8 + ind];
		total_ssd += prev_ssd;

		for (int i=1; i <= n; ++i)
		{
			// ssd_scratch has the padding and sentinels already added
			// i <= n guarantees that at least one of these two is not a sentinel
			S32 ssd0 = ssd_scratch[8 + ind - i];
			S32 ssd1 = ssd_scratch[8 + ind + i];
			S32 cur_ssd = RR_MIN(ssd0,ssd1);

			// We artifically constrain this sequence to be monotonic, even when it's not.
			// This can happen if the original index wasn't optimal, for example due to an
			// index change in an earlier pass.
			S32 delta = RR_MAX(cur_ssd - prev_ssd, 0);
			ssd_for_index_delta[p][i-1] = delta;
			prev_ssd += delta;
		}
	}

	return compute_sad_bound_for_ssd_threshold(ssd_for_index_delta, total_ssd, ssd_limit, out_ssd_limit);
}

static void bc1_indices_vq_reduce(
	CpuDispatchFlags dispatch,
	vector<dword_and_count> * indices_out, const vector<dword_and_count> & total_indices_in, 
	F32 vqd_lambda, int nblocks_total ,
	rrDXT1_VQ_Block * blocks_in,
	rrDXT1PaletteMode pal_mode,
	const rrDXT1_VQ_Config & config)
{
	SIMPLEPROFILE_SCOPE_N(bc1rd_indices_reduce,total_indices_in.size32());
	
	// Index merge pass works on a slightly different lambda and also works in different units
	// this exact constant here is essentially a historical accident, it's somewhat fuzzy.
	//
	// Also, VQD ~ 2*SSD, and J = D + lambda*R
	//
	// therefore, to match the scale of values we get from switching from VQD (outside) to
	// SSD (inside), we need to scale lambda down by a factor of 2. We used to scale all
	// distortion measurements by 2, but this is simpler and cleaner.
	const F32 lambda = vqd_lambda * ( 1.2549f / 2.f );
	RR_ASSERT( lambda > 0 );

	const rrDXT1_VQ_Block * blocks = blocks_in;

	// input indices are uniqued already
	// indices_in should be sorted by count already :
	//RR_ASSERT( indices_in[0].count >= indices_in.back().count );
	int indices_in_count = total_indices_in.size32();

	// If number of unique indices is already tiny, there's no point trying anything here;
	// indices are already as redundant as they're going to get.
	if ( indices_in_count <= 4 )
	{
		*indices_out = total_indices_in;
		return;
	}
	
	vector<index_vq_entry> entries;
	entries.reserve( indices_in_count );
	
	indices_out->reserve(indices_in_count);
	indices_out->clear();
	U32 total_count_out = 0;
	
	vector<U32> indices_nc_raw;
	vector<dword_and_count> indices_nc_sorted;
	indices_nc_sorted.reserve(indices_in_count);
	
	entries.clear();
	int num_entries_per_mode[2] = { 0, 0 }; // [is3c]
	
	// build separate entries lists for 4c then for 3c
	// but then just stick them all in entries[]
	//	and do the N^2 merge letting them merge together
	//	this seems to be better than keeping them segregated
	for LOOP(loop_is3c,2)
	{
		indices_nc_raw.clear();
		indices_nc_sorted.clear();
		
		for LOOP(b,nblocks_total)
		{
			// filter for blocks that are in the desired 3c4c mode :
			bool cur_is3c = ! DXT1_Is4Color( blocks[b].cur, pal_mode );
			if ( (int)cur_is3c != loop_is3c )
				continue;

			// confirm block_1bt_mask matches :
			RR_ASSERT( pal_mode != rrDXT1PaletteMode_Alpha || DXT1_OneBitTransparent_Mask_Same(blocks[b].block_1bt_mask,blocks[b].cur) );
			
			U32 indices = blocks[b].cur.indices;
			indices_nc_raw.push_back(indices);
		}
		
		// number of blocks of this 3c4c mode :
		int nblocks_nc = indices_nc_raw.size32();
		RR_UNUSED_VARIABLE(nblocks_nc);

		//rrprintfvar(nblocks_nc);
		if ( indices_nc_raw.empty() ) // can happen with 3c
			continue;
		
		sort_and_count_uniques(&indices_nc_sorted,indices_nc_raw);
		sort_dword_and_count_compare_count_highest_first(&indices_nc_sorted);
		RR_ASSERT( indices_nc_sorted[0].count >= indices_nc_sorted.back().count );
		
		int cur_entries_count = indices_nc_sorted.size32();
		num_entries_per_mode[loop_is3c] = cur_entries_count;

		//rrprintfvar(cur_entries_count);
		int entries_base = entries.size32();
		entries.resize(entries_base+cur_entries_count);
		index_vq_entry * cur_entries = entries.data() + entries_base;
		
		U32 total_count_in = 0;
		for LOOPVEC(i,indices_nc_sorted)
		{
			U32 indices = indices_nc_sorted[i].dw;		
			int count = indices_nc_sorted[i].count;	
			
			cur_entries[i].is3c = loop_is3c;
			cur_entries[i].indices = indices;
			cur_entries[i].count = count;		
			cur_entries[i].count_log2_count = count * log2tabled_bk_32( count );

			if ( loop_is3c )
				bc1_indices_3c_unpack_to_16x8(cur_entries[i].unpacked_16x8,indices);
			else
				bc1_indices_4c_unpack_to_16x8(cur_entries[i].unpacked_16x8,indices);

			cur_entries[i].merged_onto = -1;
			cur_entries[i].block_link = -1;
			total_count_in += cur_entries[i].count;
		}
		RR_ASSERT( total_count_in == (U32)nblocks_nc );

		// make block linked lists :
		//	each index has list of all blocks that use it

		// we need to look up entry ID from an index, so do one more sort pass
		for LOOPVEC(i,indices_nc_sorted)
		{
			// .dw has the indices, set up .count to list the entry id
			indices_nc_sorted[i].count = entries_base + i;
		}
		stdsort(indices_nc_sorted.begin(),indices_nc_sorted.end(),dword_and_count_compare_dword());

		for LOOP(b,nblocks_total)
		{
			// filter for blocks that are in the desired 3c4c mode :
			bool cur_is3c = ! DXT1_Is4Color( blocks[b].cur, pal_mode );
			if ( (int)cur_is3c != loop_is3c )
				continue;
				
			U32 indices = blocks[b].cur.indices;
			
			// find in cur_entries via binary search :
			dword_and_count lookup = { indices, ~0u }; // our count compare < checks for count _greater_, so make our query count large
			const dword_and_count * entry_pos = lower_bound(indices_nc_sorted.begin(),indices_nc_sorted.end(),lookup,dword_and_count_compare_dword());
			RR_ASSERT( entry_pos != indices_nc_sorted.end() && entry_pos->dw == indices ); // we better find an exact match!
			int entry_i = entry_pos->count;

			// add to linked list :
			const_cast<rrDXT1_VQ_Block *>(blocks)[b].vq_entry_link = entries[entry_i].block_link;
			entries[entry_i].block_link = b;
		}
	
	} // 3c4c loop
	
	int num_entries = entries.size32();
	//rrprintfvar(num_entries);
	
	// use unpacked_16x8_diff as an early out for speed?
	// so a valid merge has (diff*count) <= 4*lambda
	// this should be higher than we think the limit is
	//	I want this to only rule out candidates that are almost certainly junk
	//	use the block distortion metric for real decisions, not index distance
	// @@ is this high enough? try it higher
	//	 when you bump it up it should not help rate much at all
	// -> this is a big speed savings
	//	 pretty decent way to trade quality for speed at the moment
	//	-> be careful and test this on a variety of images!

	// if distortion increases by this much, it won't be a J win
	// (32 bits = full index)
	F32 max_distortion_increase_f = vq_J(0,32.f,lambda);
	S32 max_distortion_increase = (S32)(max_distortion_increase_f + 0.5f);
	// the distortion increase isn't vq-cluster-size-dependent because the rate change isn't:
	// see comments in index_merge_dJ, but in summary, we assume that the cost of LZ references
	// to the unique index values does not change in the merge, hence the rate improvement is
	// independent of the number of blocks in the cluster
	
	vector<index_vq_heap_entry> heap;
	heap.reserve( num_entries*16 );

	// NOTE: keeping AIDs in a separate array since they're quite large and quite cold;
	// keeping them in the main index_vq_entry really tanks perf of vq_reduce pass
	vector_aligned<AnyIndexD, 64> aids;

	{
	SIMPLEPROFILE_SCOPE_N(bc1rd_indices_reduce_mkheap,num_entries); // all the time is here

	aids.resize(num_entries);
		
	for LOOP(fm,num_entries)
	{
		block_link_to_AnyIndexD(&entries[fm],blocks,&aids[fm],pal_mode);
		
		entries[fm].distortion_sum = AnyIndexD_lookup(&aids[fm],entries[fm].indices);

		// only bother looking if within max_distortion_increase
		entries[fm].best_distortion = entries[fm].distortion_sum + max_distortion_increase;
		entries[fm].sad_bound_good_until = 0x7fffffff;
		entries[fm].sad_bound = 0;
	}
	
	// process chunks of the entries that fit in L1 to minimize cache thrashing
	const int entry_chunk_size = (16 * 1024) / sizeof(entries[0]);

	// list of candidate index SADs
	int filtered_elems[entry_chunk_size];
	U32 filtered_entry_inds[entry_chunk_size]; // this one does not need slop (currently)
	S32 candidate_distortions[entry_chunk_size + FIND_NEXT_BELOW_PADDING];
	U32 chunk_1bt_masks[entry_chunk_size];
	U32 chunk_packed_inds[entry_chunk_size];
	RAD_ALIGN(U8, chunk_inds[entry_chunk_size * 16], 64);

	// find_next_below overshoots; just make sure it's all initialized to pacify ASan etc.
	for LOOP(i,entry_chunk_size + FIND_NEXT_BELOW_PADDING)
		candidate_distortions[i] = -1;

	for (int mode_is3c = 0; mode_is3c <= 1; ++mode_is3c)
	{
		// We have first all 4c blocks, then all 3c blocks, and we forbid cross-mode
		// index merges
		const int mode_begin = mode_is3c ? num_entries_per_mode[0] : 0;
		const int mode_end = mode_is3c ? num_entries : num_entries_per_mode[0];

		for (int to_chunk_begin = mode_begin; to_chunk_begin < mode_end; to_chunk_begin += entry_chunk_size)
		{
			const int to_chunk_end = RR_MIN(to_chunk_begin + entry_chunk_size, mode_end);
			const int to_chunk_len = to_chunk_end - to_chunk_begin;

			// copy indices into working buffer so we have them densely packed for the SAD funcs
			for LOOP(i,to_chunk_len)
				memcpy(&chunk_inds[i * 16], entries[to_chunk_begin + i].unpacked_16x8, 16);

			for LOOP(i,to_chunk_len)
				chunk_packed_inds[i] = entries[to_chunk_begin + i].indices;

			// Set up the masks of which pixels are transparent; while this is relatively cheap,
			// there's still no need to recompute it every time in the N^2 loop
			if ( pal_mode == rrDXT1PaletteMode_Alpha )
			{
				for LOOP(i,to_chunk_len)
				{
					int j = to_chunk_begin + i;
					chunk_1bt_masks[i] = DXT1_OneBitTransparent_Mask_FromIndices( entries[j].is3c, entries[j].indices );
				}
			}
			else
			{
				for LOOP(i,to_chunk_len)
					chunk_1bt_masks[i] = 0;
			}

			// this is N^2 on the number of unique indices
			//	(actually N*M with N = # of unique indices and M = # of blocks)
			for (int fm = mode_begin; fm < mode_end; ++fm)
			{
				// no need to consider a flat changing into non-flat
				if ( indices_are_flat(entries[fm].indices) )
					continue;

				const AnyIndexD & aid = aids[fm];
				U32 fm_base_distortion = entries[fm].distortion_sum;
				S32 best_distortion = entries[fm].best_distortion;

				U32 fm_1bt_mask = 0;
				if ( pal_mode == rrDXT1PaletteMode_Alpha )
					fm_1bt_mask = DXT1_OneBitTransparent_Mask_FromIndices( entries[fm].is3c, entries[fm].indices );

				// We disable the index early out (which is faster but reduces
				// quality somewhat) by setting its "should I skip" threshold
				// to one that's never taken.
				int adjusted_index_diff_limit;
				if ( ! config.do_index_diff_early_out )
					adjusted_index_diff_limit = 256*16; // larger than any possible SAD of 16 bytes
				else
				{
					// Original test was
					//   index_diff * entries[fm].count > conservative_index_diff_limit
					//   <=> index_diff > conservative_index_diff_limit / entries[fm].count
					//
					// LHS is always integer, RHS is not, but rounding down the division
					// does the right thing: say the RHS is 2.5, which we round down to 2,
					// but since index_diff is integer, "index_diff > 2" is the same as
					// "index_diff > 2.5".
					if ( best_distortion < entries[fm].sad_bound_good_until )
					{
						entries[fm].sad_bound = bc1rd_compute_index_sad_bound(aid, best_distortion, mode_is3c, entries[fm].unpacked_16x8, &entries[fm].sad_bound_good_until);
					}
					adjusted_index_diff_limit = entries[fm].sad_bound;
				}

				// When adjusted_index_diff_limit <= 0, the only thing we would accept is an exact
				// index match, which we're not going to find (since the index entries are unique),
				// so we can skip trying.
				if ( adjusted_index_diff_limit <= 0 )
					continue;

				// Filter candidates by SAD - this writes the (relative) indices of entries
				// where the index SAD is <= adjusted_index_diff_limit (done by testing
				// for < adjusted_index_diff_limit + 1).
				int filtered_count = filter_by_index_sad_with_inds(dispatch,filtered_elems,filtered_entry_inds,to_chunk_len,
					chunk_inds, chunk_packed_inds, entries[fm].unpacked_16x8,
					adjusted_index_diff_limit + 1);

				// Rank all the distortions
				S32 best_possible = AnyIndexD_batch_lookup(dispatch,candidate_distortions,&aid,filtered_entry_inds,filtered_count);

				// If there are no viable candidates, we can bail immediately
				if ( best_possible > best_distortion )
					continue;

				// Set up sentinel so we can use our faster search
				candidate_distortions[filtered_count] = -1;

				for (int i = 0; ; i++) // yes, no loop condition!
				{
					// entries are sorted by count so I only have to look at J when D gets better
					//  (this is an approximation, tests indicate it's okay)
					//		(it's exact for the first merge step, but not later as things get merged up)
					//	(to be exact you should go ahead and add everything with dj > 0 to the heap)
					//	(if we only did one merge this would be fine, it would not be an approximation
					//	 the issue is that later on, all your desired merge targets may be gone
					//	 so the best thing left may be one of the ones that we ruled out here)
					//
					// Scan for next entry with candidate_distortion <= best_distortion,
					// which is equvialent to candidate_distortion < best_distortion + 1.

					// We allow i == final_filtered_count here; we have the extra sentinels/padding at the end
					// to make sure we catch this.
					RR_ASSERT( i <= filtered_count );
					i = find_next_below(candidate_distortions, i, best_distortion + 1);
					if ( i >= filtered_count )
						break;

					S32 distortion = candidate_distortions[i];
					RR_ASSERT( distortion <= best_distortion );

					const int rel_index = filtered_elems[i];
					const int to = to_chunk_begin + rel_index;

					// Extra rejection checks that trigger rare enough that it's not worth doing them
					// as part of the early filter pass:

					// No merges that change 1-bit transparency mask
					if ( fm_1bt_mask != chunk_1bt_masks[rel_index] )
						continue;

					// this also catches fm == to :
					if ( entries[fm].indices == entries[to].indices )
					{
						RR_ASSERT( fm == to || entries[fm].is3c != entries[to].is3c );
						// indices equality can happen for 3c to 4c
						// error there would be zero, don't allow it
						continue;
					}

					// @@ exclude flat indices from merge-to candidates for now
					//  -> doesn't make much difference if you do or not
					//	   (assuming 2nd pass picks them up)
					if ( indices_are_flat(entries[to].indices) )
						continue;

					// OK, score it!

					best_distortion = distortion;

					S32 delta_D = distortion - fm_base_distortion;
					RR_ASSERT( delta_D <= max_distortion_increase ); // gauranteed by initial value of best_distortion

					// @@ now compute delta_D if we found the best indices over the set :
					U32 before_pair_D = fm_base_distortion + entries[to].distortion_sum;
					U32 after_D = AnyIndexD_best_index_pair_D(&aid,&aids[to]);
					S32 new_index_delta_D = after_D - before_pair_D;
					RR_ASSERT( new_index_delta_D <= delta_D );
					delta_D = new_index_delta_D;

					F32 dj = index_merge_dj( entries[fm], entries[to], delta_D, lambda); //,nblocks);
					if ( dj > 0 )
					{
						// make a heap entry :
						heap.push_back();
						heap.back().fm = fm;
						heap.back().pair_count_save = entries[fm].count + entries[to].count;
						heap.back().to = to;
						heap.back().dj = dj;
					}
				}

				entries[fm].best_distortion = best_distortion; // update state
			}
		}
	}

	make_heap(heap.begin(),heap.end());
	}
	
	{
	//it's weird how fast this part is :
	//SIMPLEPROFILE_SCOPE(indices_vq_reduce_pop_heap);
	
	// heap is sorted by dj, largest first
	while( ! heap.empty() )
	{
		index_vq_heap_entry heap_entry = heap[0];
		popped_heap(heap.begin(),heap.end());
		heap.pop_back();
		
		// if from entry is gone, ignore me
		int fm = heap_entry.fm;
		if ( entries[ fm ].merged_onto >= 0 )
			continue;

		bool dirty = false;
		
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
		
		// pair count changes makes the merge dirty
		//	block set must be the same we originally considered
		dirty = dirty || ( heap_entry.pair_count_save != (int)(entries[ fm ].count + entries[ to ].count) );
		
		if ( dirty )
		{
			// make a new candidate for me to merge onto merged_to

			// NOTE: do not need to re-check 1-bit transparency mask here
			// the only candidates we have preserve 1-bit transparency originally,
			// and two 3c indices having the same 1bt mask is transitive
			// -> if the original merges preserved transparency, so does their
			// transitive closure when following chains of merges

			U32 distortion_base = entries[fm].distortion_sum + entries[to].distortion_sum;
			U32 after_D = AnyIndexD_best_index_pair_D(&aids[fm],&aids[to]);
			S32 delta_D = after_D - distortion_base;
			//if ( delta_D <= max_distortion_increase ) // no, hurts
			{			
				F32 dj = index_merge_dj( entries[fm], entries[to], delta_D , lambda); //,nblocks);
				if ( dj > 0 )
				{
					// make a heap entry :
					heap.push_back();
					heap.back().fm = fm;
					heap.back().pair_count_save = entries[fm].count + entries[to].count;
					//heap.back().fm_distortion_onto = distortion;
					heap.back().to = to;
					heap.back().dj = dj;
					push_heap(heap.begin(),heap.end());
				}
			}
			
			continue;
		}
		
		// Assert that all applied merges preserve 1-bit transparency mask
		if ( pal_mode == rrDXT1PaletteMode_Alpha )
		{
			RR_ASSERT(DXT1_OneBitTransparent_Mask_FromIndices( entries[fm].is3c, entries[fm].indices ) == DXT1_OneBitTransparent_Mask_FromIndices( entries[to].is3c, entries[to].indices ) );
		}

		// fm and to are both alive
		// do the merge

		entries[fm].merged_onto = to;
		entries[to].count += entries[fm].count;
		
		entries[to].count_log2_count = entries[to].count * log2tabled_bk_32( entries[to].count );
		
		// merge the block linked list :
		// all [fm] indices change to [to]
		// find the tail of the link to connect them
		{
			int link = entries[fm].block_link;
			U32 link_count = 0;
			for(;;)
			{
				link_count++;
				RR_ASSERT( link_count <= entries[fm].count );
				if ( blocks[link].vq_entry_link < 0 )
				{
					// end of the chain
					// link the [to] chain onto it :
					const_cast<rrDXT1_VQ_Block *>(blocks)[link].vq_entry_link = entries[to].block_link;
					entries[to].block_link = entries[fm].block_link;
					break;
				}
				link = blocks[link].vq_entry_link;
			}
			RR_ASSERT( link_count == entries[fm].count );
		}
		
		U32 new_D;
		U32 new_indices = AnyIndexD_add_then_find_indices(&aids[to],aids[fm],&new_D);
		entries[to].distortion_sum = new_D;
		entries[to].indices = new_indices;
		// [to] is made dirty by to count changing
		
	}
	} //profile scope
	
	// scan out just the un-merged entries :
	// indices_out and total_count_out are accumulated from both the 3c and 4c loop
	for LOOPVEC(entry_i,entries)
	{
		if ( entries[entry_i].merged_onto >= 0 ) continue;
		
		RR_ASSERT( entries[entry_i].count > 0 );
		
		#ifdef DO_bc1_indices_vq_reduce_CHANGE_INDICES
		// change the actual block indices :
		int link = entries[entry_i].block_link;
		U32 link_count = 0;
		while( link >= 0 )
		{
			rrDXT1_VQ_Block * block = &const_cast<rrDXT1_VQ_Block *>(blocks)[link];
			link_count++;
			block->cur.indices = entries[entry_i].indices;
			RR_ASSERT( pal_mode != rrDXT1PaletteMode_Alpha || DXT1_OneBitTransparent_Mask_Same(block->block_1bt_mask,block->cur) );
			link = block->vq_entry_link;
		}
		RR_ASSERT( link_count == entries[entry_i].count );
		#endif

		indices_out->push_back();
		indices_out->back().count = entries[entry_i].count;
		indices_out->back().dw = entries[entry_i].indices;
		total_count_out += entries[entry_i].count;
	}
	
	RR_ASSERT( total_count_out == (U32)nblocks_total );
	
	// because of 3c/4c split indices_out can have dupe entries
	//	need to merge those again :
	
	sort_dword_and_count_compare_dword(indices_out);
	
	// condense :
	//	this is more than just the 3c/4c dupes
	//	because of the find-indexes-after-merge
	//	we can accidentally combine sets
	//	eg. {A,B}->C and {D,E}->C
	//	you wind up with two entries for {C}
	//	ideally we'd find and merge those as we go
	int toi = 1;
	S32 indices_out_size = indices_out->size32();
	for(int fmi=1;fmi<indices_out_size;fmi++)
	{
		if ( indices_out->at(fmi).dw == indices_out->at(toi-1).dw )
		{		
			indices_out->at(toi-1).count += indices_out->at(fmi).count;			
		}
		else
		{
			indices_out->at(toi) = indices_out->at(fmi);
			toi++;
		}
	}
	indices_out->resize(toi);	
	
	//indices_out now has the unique indices & their count but is not yet sorted (by count)
			
	//int bc1_indices_vq_reduce_num_out = indices_out->size32();
	//rrprintfvar(bc1_indices_vq_reduce_num_out);
}

RR_NAMESPACE_END
