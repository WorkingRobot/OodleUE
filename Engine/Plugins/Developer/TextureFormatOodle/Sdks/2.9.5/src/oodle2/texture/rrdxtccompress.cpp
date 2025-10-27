// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrdxtccompress.h"
#include "rrdxtcblock.h"
#include "rrcolorvecc.h"
#include "rrrand.h"
#include <float.h>
#include "rrdxtccompress.inl"
#include "rrmat3.h"
#include "rrlogutil.h"
#include "bc67tables.h" // single-color fit tables

#include "templates/rralgorithm.h"
#include "templates/rrvector_s.h"

//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"

//===============================================================================

RR_NAMESPACE_START

//===============================================================================

// Various AddEndPoints helpers!

// The SingleColor_Compact funcs use tables for optimum single-color fit that also
// constrain max distance between quantized endpoint values to avoid running
// afoul of allowed BC1 decoder tolerances.

rrDXT1UnpackedEndPoints * AddEndPoints_SingleColor_Compact_4C(rrDXT1UnpackedEndPoints * pEndPoints, const rrColor32BGRA & c)
{
	rrDXT1UnpackedEndPoints ep;

	const BC7OptimalEndpoint & opt_r = bc1_optimal_4c[0][c.u.r];
	const BC7OptimalEndpoint & opt_g = bc1_optimal_4c[1][c.u.g];
	const BC7OptimalEndpoint & opt_b = bc1_optimal_4c[0][c.u.b];

	ep.c[0] = rrColorUnpacked565(opt_r.lo, opt_g.lo, opt_b.lo);
	ep.c[1] = rrColorUnpacked565(opt_r.hi, opt_g.hi, opt_b.hi);

	// note : bco.c0.w == bco.c1.w is totally possible
	//	(happens for example when the color is 0 or 255)
	// that's a degenerate block that uses 3-color mode!
	// indices 0xAAA (= 1/2 interp) works fine for that too, so leave it

	// Put in 4-color order if possible
	if (ep.c[0] < ep.c[1])
		RR_NAMESPACE::swap(ep.c[0], ep.c[1]);

	*pEndPoints++ = ep;
	return pEndPoints;
}

rrDXT1UnpackedEndPoints * AddEndPoints_SingleColor_Compact_3C(rrDXT1UnpackedEndPoints * pEndPoints, const rrColor32BGRA & c)
{
	rrDXT1UnpackedEndPoints ep;

	const BC7OptimalEndpoint & opt_r = bc1_optimal_3c[0][c.u.r];
	const BC7OptimalEndpoint & opt_g = bc1_optimal_3c[1][c.u.g];
	const BC7OptimalEndpoint & opt_b = bc1_optimal_3c[0][c.u.b];

	ep.c[0] = rrColorUnpacked565(opt_r.lo, opt_g.lo, opt_b.lo);
	ep.c[1] = rrColorUnpacked565(opt_r.hi, opt_g.hi, opt_b.hi);

	// Put in 3-color order
	if (ep.c[0] > ep.c[1])
		RR_NAMESPACE::swap(ep.c[0].dw, ep.c[1].dw);

	*pEndPoints++ = ep;
	return pEndPoints;
}

rrDXT1UnpackedEndPoints * AddEndPoints_Default(rrDXT1UnpackedEndPoints * pEndPoints, rrDXT1PaletteMode mode,
							const rrColor32BGRA & end1,const rrColor32BGRA & end2)
{
	rrDXT1UnpackedEndPoints ep;
	ep.c[0] = rrColorUnpacked565::quantize(end1);
	ep.c[1] = rrColorUnpacked565::quantize(end2);

	// We check for actual solid colors during block init, but we can
	// still have endpoints close enough that they quantize to the same
	// 565 values:
	if ( ep.c[0] == ep.c[1] )
	{
		// We used to try various tricks here to separate the endpoints slightly,
		// but it was pretty hit and miss and basically nothing actually seems to care
		// much; at least at the higher levels (which are what Oodle Texture actually
		// uses) we also have the "Greedy Optimize" phase at the end that tries +-1
		// wiggles on the endpoint values anyway, which pushes us towards not really
		// caring here.
		//
		// The main texture in our test sets that gets worse from this is linear_ramp1,
		// and only at rrDXT levels 0 and 1 (0 isn't even exposed in Oodle Texture).
		return pEndPoints;
	}

	// Try both 3-color and 4-color orderings
	*pEndPoints++ = ep;

	// Add swapped order if not in four-color mode where both are equivalent
	if ( mode != rrDXT1PaletteMode_FourColor )
	{
		pEndPoints->c[0] = ep.c[1];
		pEndPoints->c[1] = ep.c[0];
		pEndPoints++;
	}

	return pEndPoints;
}

rrDXT1UnpackedEndPoints * AddEndPoints_Force3C(rrDXT1UnpackedEndPoints * pEndPoints,
							const rrColor32BGRA & end1,const rrColor32BGRA & end2)
{
	pEndPoints->c[0] = rrColorUnpacked565::quantize(end1);
	pEndPoints->c[1] = rrColorUnpacked565::quantize(end2);
	// force 3 color order
	if ( pEndPoints->c[0] > pEndPoints->c[1] )
		RR_NAMESPACE::swap(pEndPoints->c[0], pEndPoints->c[1]);
	pEndPoints++;

	return pEndPoints;
}

rrDXT1UnpackedEndPoints * AddEndPoints_BothWays(rrDXT1UnpackedEndPoints * pEndPoints,
							const rrColor32BGRA & end1,const rrColor32BGRA & end2)
{
	rrDXT1UnpackedEndPoints ep;
	ep.c[0] = rrColorUnpacked565::quantize(end1);
	ep.c[1] = rrColorUnpacked565::quantize(end2);
	if ( ep.c[0] == ep.c[1] ) // degenerate, just skip!
		return pEndPoints;
	*pEndPoints++ = ep;

	// Other-direction pair as well (swapped)
	pEndPoints->c[0] = ep.c[1];
	pEndPoints->c[1] = ep.c[0];
	pEndPoints++;

	return pEndPoints;
}

rrDXT1UnpackedEndPoints * AddEndPoints_TwoColorBest(rrDXT1UnpackedEndPoints * pEndPoints,
							const rrColor32BGRA & c1,const rrColor32BGRA & c2)
{
	rrDXT1UnpackedEndPoints * ptr = pEndPoints;

	// this is wasteful, fix to work directly on colors instead of going through vec3i :
	rrVec3i v1 = ColorToVec3i(c1);
	rrVec3i v2 = ColorToVec3i(c2);
	rrVec3i delta = v2 - v1;

	// try to hit two colors exactly by either
	//	using them as the ends or trying to hit them at the 1/3 or 2/3 points

	// I only actually try 4 ways, I should just unroll them :
	// 0 : c1 , 1 : c2
	// 0 : c1 , 2/3 : c2
	// 1/3 : c1 , 1 : c2
	// 1/3 : c1 , 2/3 : c2

	// these ways are actually not tried now :
	// 2/3 : c1 , 1 : c2
	// 0 : c1 , 1/3 : c2

	//0,3 : v1->v2
	//0,2 : v1->v2+delta2
	//1,2 : v1-delta->v2+delta
	//1,3 : v1-delta2->v2

	ptr = AddEndPoints_BothWays(ptr,c1,c2);

	// toggle just doing endpoints
	//	0.1 rmse win from this

	rrVec3i delta2;
	delta2.x = delta.x / 2;
	delta2.y = delta.y / 2;
	delta2.z = delta.z / 2;

	// tiny len, don't bother :
	if ( LengthSqr(delta2) < 6 )
		return ptr;

	{
		rrColor32BGRA cend1 = Vec3iToColorClamp(v1 - delta);
		rrColor32BGRA cend2 = Vec3iToColorClamp(v2 + delta);

		ptr = AddEndPoints_BothWays(ptr,cend1,cend2);
	}

	{
		rrColor32BGRA cend1 = Vec3iToColorClamp(v1 - delta2);
		rrColor32BGRA cend2 = Vec3iToColorClamp(v2 + delta2);

		ptr = AddEndPoints_BothWays(ptr,cend1,c2);
		ptr = AddEndPoints_BothWays(ptr,c1,cend2);

		#if 0
		ptr = AddEndPoints_BothWays(ptr,cend1,cend2);
		#endif
	}

	return ptr;
}

//===============================================================================

enum
{
	FilterEndpoints_Allow4c = 1 << 0,
	FilterEndpoints_Allow3c = 1 << 1,
	FilterEndpoints_AllowBoth = FilterEndpoints_Allow4c | FilterEndpoints_Allow3c,
};

// Filters endpoints [endpoints,endpoints_end) to only contain the given mode, in-place,
// and returns new end
static rrDXT1UnpackedEndPoints * FilterEndpoints(rrDXT1UnpackedEndPoints * endpoints, rrDXT1UnpackedEndPoints * endpoints_end, U32 allow_which)
{
	U32 mask = allow_which & FilterEndpoints_AllowBoth;

	// Handle trivial cases
	if ( mask == 0 )
		return endpoints; // none pass
	else if ( mask == FilterEndpoints_AllowBoth )
		return endpoints_end; // all pass

	bool want_4color = mask == FilterEndpoints_Allow4c;
	rrDXT1UnpackedEndPoints * new_end = endpoints;
	for ( rrDXT1UnpackedEndPoints * cur = endpoints; cur != endpoints_end; ++cur )
	{
		if ( DXT1_Is4Color(*cur,rrDXT1PaletteMode_Alpha) == want_4color )
			*new_end++ = *cur;
	}

	return new_end;
}

// Tries a number of endpoint pairs, given as endpoints[0..count-1].
// palette_scratch needs to have space for count*4 entries; doesn't
// need to be initialized, it just needs to be large enough.
static void TryBatchedEndpoints(rrDXT1Block * pBlock, U32 * pError, const rrColorBlock4x4 & colors, const rrDXT1PaletteMode mode,
	const rrDXT1UnpackedEndPoints * endpoints, int count, rrColor32BGRA palette_scratch[])
{
	RR_ASSERT(count < DXT1_FindErrorsContext::COUNT_LIMIT);

	DXT1_FindErrorsContext ctx;
	ctx.init(colors);

	// rank all the candidates
	DXT1_ComputePalette_Batched(endpoints, count, palette_scratch, mode);

	U32 best_err_and_i = ctx.find_best_palette(&ctx, palette_scratch, count);
	U32 best_err = best_err_and_i >> DXT1_FindErrorsContext::COUNT_SHIFT;

	if ( best_err < *pError ) // we found an improvement!
	{
		U32 best_i = best_err_and_i & (DXT1_FindErrorsContext::COUNT_LIMIT - 1);

		pBlock->c0 = endpoints[best_i].c[0].pack();
		pBlock->c1 = endpoints[best_i].c[1].pack();
		pBlock->indices = DXT1_FindIndices(colors,&palette_scratch[best_i * 4],pError);
		RR_ASSERT( *pError == best_err );
	}
}

//================================================

struct rrCompressDXT1_Startup_Data
{
	rrVec3i avg;
	rrVec3i diagonal;
	rrVec3i sum;
	//rrColor32BGRA avgC;
	rrColor32BGRA loC;
	rrColor32BGRA hiC;
	rrbool has_any_alpha; // has_any_alpha can only be true when mode == rrDXT1PaletteMode_Alpha
	// if has_any_alpha is on, you cannot use 4c mode, must use 3c mode
};

void DXT1_GreedyOptimizeBlock(const rrCompressDXT1_Startup_Data & data,rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXT1PaletteMode mode, bool do_joint_optimization);

void DXT1_AnnealBlock(const rrCompressDXT1_Startup_Data & data,rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, 
	rrDXT1PaletteMode mode);

// for palette mode,
//  is this color a 3rd-index special color? (transparent/black)
bool rrDXT1_IsTransparentOrBlack(rrDXT1PaletteMode mode,const rrColor32BGRA &color)
{
	if ( mode == rrDXT1PaletteMode_FourColor ) return false;
	else if ( mode == rrDXT1PaletteMode_Alpha ) return rrColor32BGRA_IsOneBitTransparent(color);
	else
	{
		// mode == NoAlpha ; is it black ?
		#define BLACKNESS_DISTANCE	12 // @@ blackness threshold
		return ( color.u.b < BLACKNESS_DISTANCE &&
			color.u.g < BLACKNESS_DISTANCE &&
			color.u.r < BLACKNESS_DISTANCE );
	}
}

bool Compress_TryAllPairs_Heavy(rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXT1PaletteMode mode)
{
	// Color565 is U16
	vector_s<Color565,16> uniques;
	uniques.resize(16);
	for(int i=0;i<16;i++)
	{
		uniques[i] = Quantize( colors.colors[i] ).w;
	}
	RR_NAMESPACE::stdsort(uniques.begin(),uniques.end());
	vector_s<Color565,16>::iterator it = RR_NAMESPACE::unique(uniques.begin(),uniques.end());
	uniques.erase( it, uniques.end()  );
	
	int count = uniques.size32();
	
	if ( count == 1 )
	{
		// @@ special case; single color
		return false;
	}
	
	bool ret = false;
	for(int i=0;i<count;i++)
	{
		for(int j=i+1;j<count;j++)
		{
			Color565 c0 = uniques[i];
			Color565 c1 = uniques[j];
			
			rrDXT1Block trial;
			
			trial.c0 = ToUnion(c0);
			trial.c1 = ToUnion(c1);	
			
			rrColor32BGRA palette[4];
			U32 err;

			{
				DXT1_ComputePalette(trial.c0,trial.c1,palette,mode);

				trial.indices = DXT1_FindIndices(colors,palette,&err);
				
				DXT1_OptimizeEndPointsFromIndicesIterative(&trial,&err,colors,mode);
				
				if ( err < *pError )
				{
					ret = true;
					*pError = err;
					*pBlock = trial;
				}
			}
			
			// reverse colors and try again :
			// no point trying this in force-four-color mode, it doesn't give us any new options
			if ( mode != rrDXT1PaletteMode_FourColor )
			{
				RR_NAMESPACE::swap( trial.c0, trial.c1 );

				DXT1_ComputePalette(trial.c0,trial.c1,palette,mode);

				trial.indices = DXT1_FindIndices(colors,palette,&err);
				
				DXT1_OptimizeEndPointsFromIndicesIterative(&trial,&err,colors,mode);
				
				if ( err < *pError )
				{
					ret = true;
					*pError = err;
					*pBlock = trial;
				}
			}
		}
	}
	
	return ret;
}

static bool rrCompressDXT1_Startup_Impl(rrCompressDXT1_Startup_Data * pData, const rrColorBlock4x4 & colors, rrDXT1PaletteMode mode, rrDXT1UnpackedEndPoints * & endptr)
{
	rrVec3i avg(0,0,0);
	
	rrColor32BGRA loC;
	loC.dw = 0xFFFFFFFF;
	loC.u.a = 0;
	rrColor32BGRA hiC;
	hiC.dw = 0;
	
	int num_colors = 0;
	
	rrColor32BGRA loC_colors;
	loC_colors.dw = 0xFFFFFFFF;
	loC_colors.u.a = 0;
	rrColor32BGRA hiC_colors;
	hiC_colors.dw = 0;
	
	int num_transparent = 0;

	for(int i=0;i<16;i++)
	{
		const rrColor32BGRA & c = colors.colors[i];

		RR_ASSERT( rrColor32BGRA_IsOneBitTransparentCanonical(c) );

		if ( c.dw == 0 )
		{
			num_transparent++;
		}

		avg += ColorToVec3i( c );
		
		hiC.u.b = RR_MAX(hiC.u.b,c.u.b);
		hiC.u.g = RR_MAX(hiC.u.g,c.u.g);
		hiC.u.r = RR_MAX(hiC.u.r,c.u.r);
		loC.u.b = RR_MIN(loC.u.b,c.u.b);
		loC.u.g = RR_MIN(loC.u.g,c.u.g);
		loC.u.r = RR_MIN(loC.u.r,c.u.r);
		
		if ( ! rrDXT1_IsTransparentOrBlack(mode,c) )
		{
			// if pal_mode == alpha,
			//	then blacks come in here and count as "colors"
			num_colors++;
			
			hiC_colors.u.b = RR_MAX(hiC_colors.u.b,c.u.b);
			hiC_colors.u.g = RR_MAX(hiC_colors.u.g,c.u.g);
			hiC_colors.u.r = RR_MAX(hiC_colors.u.r,c.u.r);
			loC_colors.u.b = RR_MIN(loC_colors.u.b,c.u.b);
			loC_colors.u.g = RR_MIN(loC_colors.u.g,c.u.g);
			loC_colors.u.r = RR_MIN(loC_colors.u.r,c.u.r);
		}
	}
	
	// loC/hiC alphas are all zero

	// hiC includes all colors, degen and non
	if ( hiC.dw == 0 )
	{
		// there can be a mix of opaque-black & transparent here
		// still need indices, but we can definitely use all-black colors

		endptr->c[0].dw = 0; // all-0 puts us in 3c mode, black
		endptr->c[1].dw = 0;
		++endptr;

		return false;
	}

	RR_ASSERT( num_transparent != 16 ); // should have been caught above
	// num_colors == 0 is possible here

	pData->has_any_alpha = num_transparent > 0;

	// "avg" includes all colors, including degens
	rrVec3i sum = avg;
	avg.x = (avg.x + 8)>>4;
	avg.y = (avg.y + 8)>>4;
	avg.z = (avg.z + 8)>>4;
	
	rrColor32BGRA avgC;
	avgC = Vec3iToColor(avg);
	
	if ( ! pData->has_any_alpha )
	{
		// try single color block to get started :
		endptr = AddEndPoints_SingleColor_Compact_4C(endptr,avgC);

		// Try the 3C mode too?
		// NOTE(fg): this helps linear_ramp1 for AMD/Intel but
		// increases error for NV; could pass a flag for us to try this
		// for high-quality modes, but since it seems very linear_ramp
		// specific, not sure?
		/*
		if ( mode != rrDXT1PaletteMode_FourColor )
			endptr = AddEndPoints_SingleColor_Compact_3C(endptr,avgC);
		//*/
	}

	if ( num_colors < 16 )
	{
		if ( num_colors == 0 )
		{
			// degenerate, no colors
			
			// we already checked hiC.dw == 0  above
			//  so it's not a true pure black degenerate (nor all transparent)

			// we still might have not quite true blacks that were classified as "black"
			//	eg. (4,4,4) would fall in the "blackness threshold"
			// we can do better by trying to code those
			// so don't just bail here
			
			// if we don't explicitly detect all-transparent
			//  the drop-through code might use the RGB values to code something funny
			//	(if input was not canonical, but it IS canonical, so that's not true)
			// -> because of canonicalization hiC.dw will be == 0 in either case
										
			// all were in "blackness threshold"
			//  but not true black
			// use the full color bbox						
			loC_colors = loC;
			hiC_colors = hiC;
		}
			
		// use the loC/hiC only of the non-transparent colors
		//	see rrDXT1_IsTransparentOrBlack

		rrColor32BGRA midC_colors = Average(loC_colors,hiC_colors);
		endptr = AddEndPoints_SingleColor_Compact_3C(endptr,midC_colors);

		if ( loC_colors.dw == hiC_colors.dw )
		{
			// degenerate, only one color (that's not transparent or black)
			//	 (this is a little fuzzy because of blackness threshold)
			//	 (there could be other shades of black that we just didn't count)
			// -> no don't return
			// helps to fall through here!
			//return false;
		}
		else
		{
			endptr = AddEndPoints_Force3C(endptr,loC_colors,hiC_colors);
		}
		
		/*
		rrVec3i diagonal_colors = ColorToVec3i(hiC_colors) - ColorToVec3i(loC_colors);
		S32 lensqr = LengthSqr(diagonal_colors);
		if ( lensqr <= 12 )
		{
			return false;
		}
		*/
		
		// @@ fill pData with full color info or reduced ?
		
		/*
		// this seems like a good idea but seems to hurt a bit in practice
		// bc1 -a1 -l3 --w1 r:\rdotestset2\p7_zm_zod_crab_cage_net_c_BC7_UNORM_sRGB_A.tga
		//per-pixel rmse : 5.1956
		//per-pixel rmse : 5.1977
		if ( mode == rrDXT1PaletteMode_Alpha )
		{
			// because of canonicalization
			// transparent pels are black
			// loC will always have dw == 0
			RR_ASSERT( loC.dw == 0 );
			loC = loC_colors;
			avg = ColorToVec3i(midC_colors);
		}
		/**/
		
	}
	
	if ( loC.dw == hiC.dw )
	{
		// degenerate, only one color
		//	already did SingleColor, get out
		return false;
	}	

	rrVec3i diagonal = ColorToVec3i(hiC) - ColorToVec3i(loC);

	if ( LengthSqr(diagonal) <= 12 )
	{
		// very tiny color bbox
		//endPtr = AddEndPoints_TwoColorBest(endPtr,loC,hiC);
		endptr = AddEndPoints_Default(endptr,mode,loC,hiC);
		return false;
	}
	
	// fill rrCompressDXT1_Startup_Data	
	pData->avg = avg;
	pData->diagonal = diagonal;
	pData->sum = sum;
	pData->loC = loC;
	pData->hiC = hiC;	
	
	return true;
}

// rrCompressDXT1_Startup returns false for degenerate blocks that should not continue
//	fills pData if true is returned
bool rrCompressDXT1_Startup(rrCompressDXT1_Startup_Data * pData, rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXT1PaletteMode mode)
{
	SIMPLEPROFILE_SCOPE(BC1_Startup);

	rrDXT1UnpackedEndPoints endpoints[16];
	rrDXT1UnpackedEndPoints * endptr = endpoints;

	bool result = rrCompressDXT1_Startup_Impl(pData,colors,mode,endptr);

	// Score the candidates we came up with
	SINTa count = endptr - endpoints;
	RR_ASSERT( count <= (SINTa)RR_ARRAY_SIZE(endpoints) );
	RR_ASSERT( count >= 1 );

	RAD_ALIGN(rrColor32BGRA, palettes[4 * RR_ARRAY_SIZE(endpoints)], 16);
	TryBatchedEndpoints(pBlock,pError,colors,mode,endpoints,(int)count,palettes);

	return result;
}

enum
{
	DXT1_4MEANS_PCA					= 1,	// enable PCA-based seeding (slower)
	DXT1_4MEANS_REDUCED_CANDIDATES	= 2,	// enable reduced candidate set (faster)
};

bool rrCompressDXT1_4Means(const rrCompressDXT1_Startup_Data & data,rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXT1PaletteMode mode, U32 flags)
{
	SIMPLEPROFILE_SCOPE(BC1_4Means);
		
	rrVec3i avg = data.avg;	
	rrVec3f pca;
	
	if ( flags & DXT1_4MEANS_PCA )
	{
		rrVec3f avgF = Vec3i_to_Vec3f(data.sum) * (1.f / 16.f);
		rrMat3 cov;
		rrMat3_SetZero(&cov);

		// Compute the covariance and also pick the longest diagonal between pixels in the block
		// and the average color; this is guaranteed to at least be something that makes sense for
		// the block (we handled single-color blocks during init) and is symmetric. In particular
		// it's not in the nullspace of the covariance matrix, because we already ruled out
		// the degenerate case (all pixels same) during startup; the longest len we find here
		// is thus nonzero, and so is the contribution from the corresponding pixel to the
		// covariance matrix, so we're good here.
		pca = rrVec3f(1.f,1.f,1.f);
		F32 longest_len2 = 0.0f;

		for(int i=0;i<16;i++)
		{
			rrVec3f d = ColorToVec3f( colors.colors[i] ) - avgF;
			cov[0].x += d.x * d.x;
			cov[0].y += d.x * d.y;
			cov[0].z += d.x * d.z;
			cov[1].y += d.y * d.y;
			cov[1].z += d.y * d.z;
			cov[2].z += d.z * d.z;

			F32 len2 = d.x * d.x + d.y * d.y + d.z * d.z;
			if (len2 > longest_len2)
			{
				pca = d;
				longest_len2 = len2;
			}
		}
		cov[1].x = cov[0].y;
		cov[2].x = cov[0].z;
		cov[2].y = cov[1].z;

		// The only way for this to happen is for all colors to be ==avgF exactly,
		// which is a degenerate case we should have caught in Startup().
		RR_ASSERT( longest_len2 > 0.0f );

		// The covariance matrix is the sum of outer products
		//
		//   C := sum_i (d_i) (d_i)^T
		//
		// It's symmetric positive semidefinite purely from this definition because
		// for any x
		//
		//   dot(x,C x)
		//   = x^T (sum_i (d_i) (d_i)^T)) x
		//   = sum_i (x^T) (d_i) (d_i)^T x
		//   = sum_i dot(x,d_i)^2
		//   >= 0
		//
		// because it's a sum of squares; furthermore our seed "pca" vector is
		// one of the d_i, the longest one. Suppose w.l.o.g. pca=d_1. Then the
		// sum above is >= dot(d_1,d_1) > 0 (see above for argument why pca is
		// not 0). In particular, C * pca != 0.
		//
		// C, being symmetric positive semidefinite, has a full set of real,
		// non-negative eigenvalues lambda_i with pairwise orthogonal eigenvectors
		// v_i. For any vector x written in this basis of eigenvectors
		//
		//   x = sum_i a_i v_i
		//
		// C multiplies the contributions by the corresponding lambda_i (that's
		// just what being diagonalizable means):
		//
		//   C x = sum_i (lambda_i * a_i) v_i
		//
		// For x = pca, since C * pca != 0, that means at least one of the
		// (lambda_i * a_i) is nonzero; since C^2, C^3, ..., C^k (k>=1) written
		// in the same basis are
		//
		//   C^k x = sum_i (lambda_i^k a_i) v_i
		//
		// and we already know that there is one i such that (lambda_i * a_i)
		// is nonzero, lambda_i^k a_i is as well for any k>=1. Therefore,
		// _none_ of the iterates are zero (in exact arithmetic).
		//
		// In practical terms, since C is constructed above from integer values
		// (or rather integer values - avg_F, but avg_F is itself an integer
		// times 1/16, still exact) it can't have tiny near-zero eigenvalues,
		// and our iterates grow very quickly. If it doesn't go to zero in the
		// first iteration, it won't.
		for(int iter=0;iter<4;iter++)
		{
			pca = cov * pca;
			pca = cov * pca;
			pca = Normalize(pca);
		}
	}
	else
	{
		// just diagonal	
		pca = Vec3i_to_Vec3f(data.diagonal);
		pca = Normalize(pca);
	}

	// dot the colors in the PCA linear fit direction & seed 4-means
	F32 minDot =  999999.f;
	F32 maxDot = -999999.f;
	for(int i=0;i<16;i++)
	{
		rrVec3i vec = ColorToVec3i( colors.colors[i] );
		rrVec3i delta = vec - avg;
		rrVec3f d = Vec3i_to_Vec3f(delta);
		F32 dot =(F32)( d * pca );
		minDot = RR_MIN(minDot,dot);
		maxDot = RR_MAX(maxDot,dot);
	}
	
	// NOTE: the use of 4-Means here has nothing in particular to do with the 4
	// "palette entries" in a BC1 block; we still make endpoint pairs from just
	// pairs of colors (we have to, it's not like BC1 lets us freely pick
	// in-between colors), but the 4-Means does a decent job at giving us
	// interesting options off the PCA line.

	// make initial 4 means :

	// means are indexed like 0,1,2,3 in order, not the DXT1 order of 0,2,3,1

	rrColor32BGRA means[4];
	
	{
	//SIMPLEPROFILE_SCOPE(BC1_4Means_FindMeans);
	
	// make 4 points staggered along the pca line :
	{
	rrVec3f meansf[4];
	meansf[0] = Vec3i_to_Vec3f(avg) + (0.75f * minDot) * pca;
	meansf[3] = Vec3i_to_Vec3f(avg) + (0.75f * maxDot) * pca;	
	meansf[1] = meansf[0] + (meansf[3] - meansf[0]) * (1.f/3.f);
	meansf[2] = meansf[0] + (meansf[3] - meansf[0]) * (2.f/3.f);
		
	for(int i=0;i<4;i++) means[i] = Vec3fToColorClamp(meansf[i]);
	}

	DXT1_SolveRGB4Means(means, colors);
	
	// We may have ended up taking means straight from colors.colors, including
	// the alpha value; the 4-means loop ignores alpha, but make sure to force
	// alpha to 255 for the following
	for(int i=0;i<4;i++)
		means[i].u.a = 255;

	}
	
	// add all endpoint pairs we want to try
	// Compress_TwoColorBest_AddEndPoints can add 8  (was 10)
	//	8*7 = 56
	
	static const int NUM_PAIRS = 10*7;
	rrDXT1UnpackedEndPoints endpoints[NUM_PAIRS];
	rrDXT1UnpackedEndPoints * endptr = endpoints;

	if ( flags & DXT1_4MEANS_REDUCED_CANDIDATES )
	{
		endptr = AddEndPoints_BothWays(endptr,means[0],means[1]);
		endptr = AddEndPoints_BothWays(endptr,means[0],means[2]);
		endptr = AddEndPoints_BothWays(endptr,means[0],means[3]);

		endptr = AddEndPoints_BothWays(endptr,means[1],means[2]);
		endptr = AddEndPoints_BothWays(endptr,means[1],means[3]);

		endptr = AddEndPoints_BothWays(endptr,means[2],means[3]);
	}
	else
	{
		endptr = AddEndPoints_TwoColorBest(endptr,means[0],means[3]);
		endptr = AddEndPoints_TwoColorBest(endptr,means[1],means[2]);
		endptr = AddEndPoints_TwoColorBest(endptr,means[0],means[2]);
		endptr = AddEndPoints_TwoColorBest(endptr,means[1],means[3]);

		endptr = AddEndPoints_TwoColorBest(endptr,means[0],means[1]);
		endptr = AddEndPoints_TwoColorBest(endptr,means[2],means[3]);
		endptr = AddEndPoints_TwoColorBest(endptr, Average(means[0],means[1]), Average(means[2],means[3]) );
	}
	
	RR_ASSERT( (SINTa)(endptr - endpoints) <= (SINTa)RR_ARRAY_SIZE(endpoints) );
	
	// If we have 1-bit alpha, endpoints that select 4-color mode do us no good, so we can
	// remove them all. Filter them out before the trial loop.
	//
	// Likewise, in reduced candidates mode, we only try 4-color unless we need 3-color
	// for alpha.
	U32 target_modes;
	if ( data.has_any_alpha )
		target_modes = FilterEndpoints_Allow3c;
	else
		target_modes = ( flags & DXT1_4MEANS_REDUCED_CANDIDATES ) ? FilterEndpoints_Allow4c : FilterEndpoints_AllowBoth;

	endptr = FilterEndpoints(endpoints,endptr,target_modes);
	int count = (int)(endptr - endpoints);

	// Try all the candidates
	RAD_ALIGN(rrColor32BGRA, palettes[4 * NUM_PAIRS], 16);
	TryBatchedEndpoints(pBlock,pError,colors,mode,endpoints,count,palettes);

	return true;
}

//===================================================================

// 0 = VeryFast
void rrCompressDXT1_0(rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXTCOptions options, rrDXT1PaletteMode mode)
{
	// @@ : note : this is "VeryFast"
	//	this is really a place-holder
	//	should replace with a good fast version
	//	using SSE2 and the simple divide method or whatever
	//	also doing block-at-a-time is obviously not ideal for VeryFast
	
	*pError = RR_DXTC_INIT_ERROR;

	rrCompressDXT1_Startup_Data data;
	if ( ! rrCompressDXT1_Startup(&data,pBlock,pError,colors,mode) )
		return;

	if ( ! rrCompressDXT1_4Means(data,pBlock,pError,colors,mode,DXT1_4MEANS_REDUCED_CANDIDATES) )
		return;
	
	// added 06-01-2019 :
	// @@ this should be skipped on flat blocks
	//	 and probably other cases where it's unlikely to help
	DXT1_OptimizeEndPointsFromIndices_Inherit_Reindex(pBlock,pError,colors,mode);
}

// 1 = Fast
void rrCompressDXT1_1(rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXTCOptions options, rrDXT1PaletteMode mode)
{
	*pError = RR_DXTC_INIT_ERROR;
	
	rrCompressDXT1_Startup_Data data;
	if ( ! rrCompressDXT1_Startup(&data,pBlock,pError,colors,mode) )
		return;

	if ( ! rrCompressDXT1_4Means(data,pBlock,pError,colors,mode,0) )
		return;
		
	DXT1_OptimizeEndPointsFromIndicesIterative(pBlock,pError,colors,mode);
	
}

// 2 = Slow
void rrCompressDXT1_2(rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXTCOptions options, rrDXT1PaletteMode mode)
{
	*pError = RR_DXTC_INIT_ERROR;
	
	rrCompressDXT1_Startup_Data data;
	if ( ! rrCompressDXT1_Startup(&data,pBlock,pError,colors,mode) )
		return;

	if ( ! rrCompressDXT1_4Means(data,pBlock,pError,colors,mode,0) )
		return;
		
	// 8 means here is not worth it, a lot slower and no big gains :
	//rrCompressDXT1_8Means(pBlock,pError,colors,mode);

	DXT1_OptimizeEndPointsFromIndicesIterative(pBlock,pError,colors,mode);
	
	//DXT1_AnnealBlock(pBlock,pError,colors,mode);

	DXT1_GreedyOptimizeBlock(data,pBlock,pError,colors,mode,false);
	
	/**/
	
	// verify *pError :
	RR_ASSERT( *pError == DXT1_ComputeSSD_OneBitTransparent(colors,*pBlock,mode) );
}

// 3 = VerySlow + Reference
void rrCompressDXT1_3(rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXTCOptions options, rrDXT1PaletteMode mode, rrDXTCLevel level)
{
	SIMPLEPROFILE_SCOPE(BC1_Level3);

	*pError = RR_DXTC_INIT_ERROR;
	
	rrCompressDXT1_Startup_Data data;
	if ( ! rrCompressDXT1_Startup(&data,pBlock,pError,colors,mode) )
	{
		// linear_ramp1.BMP still wants Optimize
		//	nobody else cares
		DXT1_GreedyOptimizeBlock(data,pBlock,pError,colors,mode,true);
		return;
	}

	// two approaches here, seem to come out quite similar
	//	in terms of overall rmse and run time
	// rmse is similar on all files
	// run time is NOT , some times one approach is much faster, but it's not monotonic
	// overall 8means+squish seems to be slightly better rmse & run time (than 4means + all pairs)

	#if 0
	
	// 4means + all pairs
	
	bool non_degenerate = rrCompressDXT1_4Means(data,pBlock,pError,colors,mode,0);

	if ( ! non_degenerate )
	{
		// linear_ramp1.BMP still wants Optimize
		//	nobody else cares
		DXT1_GreedyOptimizeBlock(pBlock,pError,colors,mode);
		return;
	}
	
	DXT1_OptimizeEndPointsFromIndicesIterative(pBlock,pError,colors,mode);
	
	if ( level >= rrDXTCLevel_Reference ) 
	{
		// Compress_TryAllPairs_Heavy does its own DXT1_OptimizeEndPointsFromIndicesIterative
		//  this is pretty slow and rarely helps much
		//	 it helps most on the rare weirdo images (frymire/serrano)	
		Compress_TryAllPairs_Heavy(pBlock,pError,colors,mode);
	}
	
	//rmse_total = 382.464

	#else

	// alternate approach : 4Means + PCA
	bool non_degenerate = rrCompressDXT1_4Means(data,pBlock,pError,colors,mode,DXT1_4MEANS_PCA);
	
	if ( ! non_degenerate )
	{
		// linear_ramp1.BMP still wants Optimize for degenerate blocks
		//	nobody else cares
		DXT1_GreedyOptimizeBlock(data,pBlock,pError,colors,mode,true);
		return;
	}

	DXT1_OptimizeEndPointsFromIndicesIterative(pBlock,pError,colors,mode);
	
	if ( level >= rrDXTCLevel_Reference )
	{
		// bc1difficult :
		// with neither :
		// rmse_total = 119.040
		// rmse_total = 118.205 without rrCompressDXT1_PCA_Squish_All_Clusters (yes Compress_TryAllPairs_Heavy)
		// rmse_total = 118.398 without Compress_TryAllPairs_Heavy (yes rrCompressDXT1_PCA_Squish_All_Clusters)
		// rmse_total = 118.166 with Compress_TryAllPairs_Heavy (and rrCompressDXT1_PCA_Squish_All_Clusters)
	
		// can try squish here too to see if it finds something different
		// @@ helps only a little and very slow -> I think this should go
		//	 leaving for now as "ground truth"
		// NOTE(fg): Squish-like removed 2021-09-22.
		// We have too many old experiments in here.
		//rrCompressDXT1_PCA_Squish_All_Clusters(data,pBlock,pError,colors,mode);
		
		// Compress_TryAllPairs_Heavy does its own DXT1_OptimizeEndPointsFromIndicesIterative
		//  this is pretty slow and rarely helps much
		//	 it helps most on the rare weirdo images (frymire/serrano)	
		Compress_TryAllPairs_Heavy(pBlock,pError,colors,mode);
	}
	
	#endif

	if ( *pError == 0 ) return; // pretty rare but may as well
	
	if ( 1 ) // level >= rrDXTCLevel_Reference )
	{
		// @@ alternative to Anneal that should be considered
		//	 is just to do a greedy optimize but with randomized larger steps
		//	(you would have to consider joint endpoint moves like dilations & contractions)

		/*
		// Anneal in VerySlow ?
		// yes I guess so
		
		rmse_total = 307.686 Slow
		rmse_total = 307.024 VerySlow
		rmse_total = 306.321 VerySlow with Anneal
		rmse_total = 305.705 Reference
		*/

		DXT1_AnnealBlock(data,pBlock,pError,colors,mode);
	}
	
	DXT1_GreedyOptimizeBlock(data,pBlock,pError,colors,mode,true);
}

//================================================


#define NUM_WIGGLES	(6)	// number of non-null wiggles

// rrColor32BGRA is ARGB in shifts
static const S32 c_wiggledw_delta[8] = { 1<<16,-(1<<16), 1<<8,-(1<<8), 1,-1, 0,0 };

static RADFORCEINLINE rrColorUnpacked565 Wiggle(const rrColorUnpacked565 & color,int how)
{
	U32 dw;
	rrColorUnpacked565 ret;
	
	dw = color.dw;
	RR_ASSERT( (dw & 0xFF1F3F1F) == dw );
	dw += (U32)c_wiggledw_delta[how];
	// if we went out of allowed range on this color,
	// some bits outside of 0x1F3F1F are on; instead
	// of clamping, we can just return the original
	// value (which works out to the same thing)
	ret.dw = (dw & (~0xFF1F3F1F)) ? color.dw : dw;
	
	return ret;
}

// This updates the endpoint and error, but not the indices, that's done outside
static void DXT1_GreedyOptimizeBlock_Inner(const rrCompressDXT1_Startup_Data & data,rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXT1PaletteMode mode, bool do_joint_optimization)
{
	SIMPLEPROFILE_SCOPE(BC1_GreedyOpt);
	
	// Greedy optimization - do after Annealing

	RR_ASSERT( *pError == DXT1_ComputeSSD_OneBitTransparent(colors,*pBlock,mode) );

	// these are unpacked to bytes but NOT unquantized :	
	rrColorUnpacked565 best0(pBlock->c0);
	rrColorUnpacked565 best1(pBlock->c1);
	DXT1_FindErrorsContext ctx;

	ctx.init(colors);
	
	for(;;)
	{
		rrColorUnpacked565 start0 = best0;
		rrColorUnpacked565 start1 = best1;

		const int MAX_TRIALS = 7*7;
		rrDXT1UnpackedEndPoints endpoints[MAX_TRIALS];
		int count = 0;
		
		// do_joint_optimization : 
		//	N*N pair wiggles, like end0 +1 in B and end1 -1 in R 
		//	or N+N independent endpoint wiggles (like BC7 does)
		
		// it's a pretty big speed difference
		//	but there is some decent quality available from do_joint_optimization
		// -> for now I'm turning off joint_optimization at level 2 (Slow)
		//	 leaving it on at level >= 3 (VerySlow)
		//	level 3 is annealing too so the speed impact of changing this isn't enormous
		
		if ( do_joint_optimization )
		{
			
			// try all wiggles :
			// 7*7 == 49 trials (actually 48, the both null is skipped)
			for(int w1=0;w1<=NUM_WIGGLES;w1++)
			{
				rrColorUnpacked565 c0 = Wiggle(start0,w1);
				
				for(int w2=0;w2<=NUM_WIGGLES;w2++)
				{
					rrColorUnpacked565 c1 = Wiggle(start1,w2);
					
					if ( c0 == start0 && c1 == start1 )
						continue;
									
					// if you have alpha, reject 4c mode :
					if ( data.has_any_alpha && c0 > c1 )
						continue;

					endpoints[count].c[0] = c0;
					endpoints[count].c[1] = c1;
					++count;
				}
			}

		}
		else
		{
			// N+N instead of N*N
			for(int w1=0;w1<NUM_WIGGLES;w1++)
			{
				rrColorUnpacked565 c0 = Wiggle(start0,w1);
				if ( c0 == start0 )
					continue;
						
				// if you have alpha, reject 4c mode :
				if ( data.has_any_alpha && c0 > start1 )
					continue;

				endpoints[count].c[0] = c0;
				endpoints[count].c[1] = start1;
				++count;
			}
			
			for(int w2=0;w2<NUM_WIGGLES;w2++)
			{
				rrColorUnpacked565 c1 = Wiggle(start1,w2);
				
				if ( c1 == start1 )
					continue;
								
				// if you have alpha, reject 4c mode :
				if ( data.has_any_alpha && start0 > c1 )
					continue;

				endpoints[count].c[0] = start0;
				endpoints[count].c[1] = c1;
				++count;
			}
		}

		// Score all of the current options
		RAD_ALIGN(rrColor32BGRA, palettes[4 * MAX_TRIALS], 16);

		DXT1_ComputePalette_Batched(endpoints,count,palettes,mode);

		U32 best_err_and_i = ctx.find_best_palette(&ctx, palettes, count);
		U32 best_err = best_err_and_i >> DXT1_FindErrorsContext::COUNT_SHIFT;
		if ( best_err < *pError )
		{
			U32 best_i = best_err_and_i & (DXT1_FindErrorsContext::COUNT_LIMIT - 1);

			*pError = best_err;
			best0 = endpoints[best_i].c[0];
			best1 = endpoints[best_i].c[1];
			pBlock->c0 = best0.pack();
			pBlock->c1 = best1.pack();
			if ( best_err == 0 )
				return;
		}
		else // no improvement found, we're done
			break;
	}
}

void DXT1_GreedyOptimizeBlock(const rrCompressDXT1_Startup_Data & data,rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXT1PaletteMode mode, bool do_joint_optimization)
{
	// If the error is already 0, nothing to do
	if ( *pError == 0 )
		return;

	U32 origError = *pError;

	DXT1_GreedyOptimizeBlock_Inner(data,pBlock,pError,colors,mode,do_joint_optimization);

	// If we found an improvement, we need to figure out what those indices are!
	if ( *pError != origError )
	{
		RAD_ALIGN(rrColor32BGRA, palette[4], 16);
		DXT1_ComputePalette(pBlock->c0,pBlock->c1,palette,mode);

		U32 solved_err;
		pBlock->indices = DXT1_FindIndices(colors,palette,&solved_err);
		RR_ASSERT( solved_err == *pError );
	}
}

void DXT1_AnnealBlock(const rrCompressDXT1_Startup_Data & data,rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, 
	rrDXT1PaletteMode mode)
{
	SIMPLEPROFILE_SCOPE(BC1_Anneal);

	rrColorUnpacked565 cur0(pBlock->c0);
	rrColorUnpacked565 cur1(pBlock->c1);
	U32 curError = *pError;
	U32 initialError = curError;
	
	RR_ASSERT( *pError == DXT1_ComputeSSD_OneBitTransparent(colors,*pBlock,mode) );

	rrDXT1UnpackedEndPoints best = { { cur0, cur1 } };
	U64 rand_state = rrRand64Simple_SeedFromU64Array((const U64 *)colors.colors,8);

	const int NITER = 256;
	RAD_ALIGN(U8, rng_bytes[NITER], 8);
	RR_COMPILER_ASSERT(NITER % 8 == 0);

	// Generate all the random bytes up front
	for(int i=0;i<NITER;i+=8)
	{
		U64 r = rrRand64Simple(&rand_state);
		RR_PUT64_LE(rng_bytes + i,r);
	}

	RAD_ALIGN(rrColor32BGRA, palette[4*4], 16);
	DXT1_FindErrorsContext ctx;
	ctx.init(colors);

	rrDXT1UnpackedEndPoints cur[4];
	U32 curErr[4];
	for(int i=0;i<4;i++)
	{
		cur[i].c[0] = cur0;
		cur[i].c[1] = cur1;
		curErr[i] = curError;
	}
	
	for(int time=0;time<64;time++)
	{
		// Set up 4 parallel trials
		rrDXT1UnpackedEndPoints ep[4];
		U32 err[4];

		for (int i=0;i<4;i++)
		{
			U8 r = rng_bytes[time*4 + i];

			rrColorUnpacked565 c0 = Wiggle(cur[i].c[0],r & 7);
			rrColorUnpacked565 c1 = Wiggle(cur[i].c[1],(r >> 3) & 7);

			if ( data.has_any_alpha && c0 > c1 )
				RR_NAMESPACE::swap(c0,c1);

			ep[i].c[0] = c0;
			ep[i].c[1] = c1;
		}

		DXT1_ComputePalette_Batched(ep,4,palette,mode);
		ctx.eval_palettes(&ctx,err,palette,4);

		const int thresh = 253 - time*4;
		for (int i=0;i<4;i++)
		{
			int diff = (int)err[i] - (int)curErr[i];
			if ( diff >= thresh )
				continue;

			if ( err[i] < *pError )
			{
				*pError = err[i];
				best = ep[i];
				if ( err[i] == 0 )
					goto done;
			}

			curErr[i] = err[i];
			cur[i] = ep[i];
		}
	}

done:
	// If we found an improvement in the annealing loop, we determined
	// the new error but not the indices; rectify this now.
	if ( *pError != initialError )
	{
		U32 solved_err;
		pBlock->c0 = best.c[0].pack();
		pBlock->c1 = best.c[1].pack();
		DXT1_ComputePalette(pBlock->c0,pBlock->c1,palette,mode);
		pBlock->indices = DXT1_FindIndices(colors,palette,&solved_err);

		RR_ASSERT( solved_err == *pError );
	}
	
	// after Anneal you want to further DXT1_GreedyOptimizeBlock
	//	to do greedy steps if any are available
	
}

//=========================================================================

// Quantize straight to packed RGB565; the previous approach where we
// first quantized results to 8-bit temps and from there to 565 rounds
// twice.
static inline rrColor565Bits Vec3fToQuantized565_RN(const rrVec3f & vec)
{
	const F32 scaleRB = 31.0f / 255.0f;
	const F32 scaleG = 63.0f / 255.0f;

	rrColor565Bits ret = {};
	ret.u.b = RR_CLAMP( rr_froundint( scaleRB * vec.x ), 0, 31 );
	ret.u.g = RR_CLAMP( rr_froundint( scaleG  * vec.y ), 0, 63 );
	ret.u.r = RR_CLAMP( rr_froundint( scaleRB * vec.z ), 0, 31 );
	return ret;
}

/*

OptimizeEndPointsFromIndices :

Simon's lsqr to optimize end points from indices

*/

bool DXT1_OptimizeEndPointsFromIndices_Raw(U32 * pEndPoints, const U32 indices, bool fourc, const rrColorBlock4x4 & colors)
{
	// can just scale up weights to make them ints
	// this is meh for optimization but it is nice to get "det" in an int to be able to check == 0 exactly
	static const int c_fourc_weights[8]  = {3,0,2,1,  0,3,1,2};
	static const int c_threec_weights[8] = {2,0,1,0,  0,2,1,0};
	
	const int * pWeights = fourc ? c_fourc_weights : c_threec_weights;
	
	int AA = 0;
	int AB = 0;
	int BB = 0;
	rrVec3f AX(0,0,0);
	rrVec3f BX(0,0,0);
	
	U32 tindices = indices;
	for(int i=0;i<16;i++)
	{
		int index = tindices&3;
		tindices >>= 2;
		
		const int A = pWeights[index];
		const int B = pWeights[index+4];
		
		AA += A*A;
		BB += B*B;
		AB += A*B;
		
		rrVec3f X = ColorToVec3f( colors.colors[i] );
		AX += float(A)*X;
		BX += float(B)*X;
	}
	
	int det = AA*BB - AB*AB;
	if ( det == 0 )
	{
		return false;
	}
	
	// have to multiply invDet by the normalization factor that was used on weights :
	float invDet = pWeights[0] / (float) det;
	
	rrVec3f vA = float(  BB * invDet) * AX + float(- AB * invDet ) * BX;
	rrVec3f vB = float(- AB * invDet) * AX + float(  AA * invDet ) * BX;
	
	rrColor565Bits qA = Vec3fToQuantized565_RN(vA);
	rrColor565Bits qB = Vec3fToQuantized565_RN(vB);

	// Try to swap into the desired order, if possible.
	// In case where both modes are possible, we can always hit 3c
	// (since it triggers on qA <= qB), but not always 4c (it's
	// not available to us when they're equal).

	// qA <= qB is 3c
	// qA >  qB is 4c
	if ( ( qA.w > qB.w ) != fourc )
		RR_NAMESPACE::swap(qA.w,qB.w);

	rrDXT1EndPoints endpoints;
	endpoints.u.c0 = qA;
	endpoints.u.c1 = qB;
	*pEndPoints = endpoints.dw;
	
	return true;
}

bool DXT1_OptimizeEndPointsFromIndices_Inherit_Reindex(rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXT1PaletteMode mode) //, rrDXTCOptions options)
{
	// keep previous fourc state :
	bool fourc = DXT1_Is4Color(*pBlock,mode);

	U32 endpoints = rrDXT1Block_EndPoints_AsU32(*pBlock);
	
	if ( ! DXT1_OptimizeEndPointsFromIndices_Raw(&endpoints,pBlock->indices,fourc,colors) )
	{
		return false;
	}

	// if endpoints didn't change, bail :
	if ( endpoints == rrDXT1Block_EndPoints_AsU32(*pBlock) )
	{
		return false;
	}
	
	// re-index for new endpoints :
	rrDXT1EndPoints ep; ep.dw = endpoints;
	rrColor32BGRA palette[4];
	DXT1_ComputePalette(ep.u.c0,ep.u.c1,palette,mode);
	
	U32 err;
	U32 indices = DXT1_FindIndices(colors,palette,&err);
	if ( err < *pError )
	{
		// it got better, take it
		*pError = err;
		rrDXT1Block_EndPoints_AsU32(*pBlock) = endpoints;
		pBlock->indices = indices;
		return true;
	}
	else
	{
		return false;
	}
}

bool DXT1_OptimizeEndPointsFromIndices_Inherit_NoReindex(rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXT1PaletteMode mode)
{
	// keep previous fourc state :
	bool fourc = DXT1_Is4Color(*pBlock,mode);

	U32 oldep = pBlock->endpoints;
	U32 endpoints = oldep;
	
	if ( ! DXT1_OptimizeEndPointsFromIndices_Raw(&endpoints,pBlock->indices,fourc,colors) )
	{
		return false;
	}

	// if endpoints didn't change, bail :
	if ( endpoints == oldep )
	{
		return false;
	}

	if ( mode == rrDXT1PaletteMode_Alpha )
	{
		// if we started out in four-color mode, new endpoints are in 3-color mode (probably
		// degenerate) and we have any pixels using index 3, we can't make the change;
		// we'd be not just turning that pixel black but also changing 1-bit transparency state.
		bool new_is_3c = ! DXT1_Is4Color(endpoints,mode);
		if ( fourc && DXT1_OneBitTransparent_Mask_FromIndices( new_is_3c, pBlock->indices) != 0 )
			return false;
	}
	
	// evaluate error with new endpoints :
	rrDXT1Block block;
	block.endpoints = endpoints;
	block.indices = pBlock->indices;
	
	// DXT1_OptimizeEndPointsFromIndices_Raw will try to preserve the fourc state
	//	except when it can't because endpoints were degenerate
	RR_ASSERT( DXT1_Is4Color(block,mode) == fourc || ( fourc && block.c0.w == block.c1.w ) );
	
	U32 err = DXT1_ComputeSSD_RGBA(colors,block,mode);
	if ( err < *pError )
	{
		// it got better, take it
		*pError = err;
		*pBlock = block;
		return true;
	}
	else
	{	
		return false;
	}
}

void DXT1_OptimizeEndPointsFromIndicesIterative(rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors,rrDXT1PaletteMode mode) //, rrDXTCOptions options)
{
	SIMPLEPROFILE_SCOPE(BC1_EndpointsFromIndsIter);
	
	for(;;)
	{
		U32 oldIndices = pBlock->indices;
		if ( ! DXT1_OptimizeEndPointsFromIndices_Inherit_Reindex(pBlock,pError,colors,mode) )
			break;
		if ( oldIndices == pBlock->indices )
			break;
		// else indices changed so do it again
		
		// this almost never actually repeats
		//	it helps quality a tiny bit and doesn't hurt speed much
		
		// @@ while iterating does help a tiny bit, is it worth it speed-wise ?
	}
}

//=============================================================================================

// main external entry points :
			
void rrCompressDXT1Block(rrDXT1Block * pBlock,const rrColorBlock4x4 & colors, rrDXTCLevel level, rrDXTCOptions options, bool isBC23ColorBlock)
{
//	SIMPLEPROFILE_SCOPE(rrCompressDXT1Block);
	
	rrDXT1PaletteMode mode = (options & rrDXTCOptions_BC1_OneBitAlpha) ? rrDXT1PaletteMode_Alpha : rrDXT1PaletteMode_NoAlpha;
	if ( isBC23ColorBlock ) // BC2/3 (and also DXT3/5) color blocks don't support the 3-color mode and ignore endpoint ordering
		mode = rrDXT1PaletteMode_FourColor;
	
	// rrSurfaceDXTC_CompressBC1 does the canonicalize :
	RR_ASSERT( rrColorBlock4x4_IsBC1Canonical(colors,mode) );
	
	U32 err = RR_DXTC_INIT_ERROR;
		
	if ( level >= rrDXTCLevel_VerySlow )
		rrCompressDXT1_3( pBlock, &err, colors, options, mode, level );
	else if ( level == rrDXTCLevel_Slow )
		rrCompressDXT1_2( pBlock, &err, colors, options, mode );
	else if ( level == rrDXTCLevel_Fast )
		rrCompressDXT1_1( pBlock, &err, colors, options, mode );
	else // VeryFast
		rrCompressDXT1_0( pBlock, &err, colors, options, mode );

	// In BC2/3, both endpoint orderings result in four-color mode. So we have some freedom
	// here and want to pick a canonical choice.
	if ( mode == rrDXT1PaletteMode_FourColor )
	{
		rrDXT1Block_BC3_Canonicalize(pBlock);
	}
	else if ( mode == rrDXT1PaletteMode_Alpha )
	{
		RR_ASSERT( DXT1_OneBitTransparent_Same(colors,pBlock->endpoints,pBlock->indices) );
	}
}

//=============================================================================================

RR_NAMESPACE_END

