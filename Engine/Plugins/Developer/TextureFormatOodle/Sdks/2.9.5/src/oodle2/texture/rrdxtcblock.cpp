// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetsse4

#include "rrdxtcblock.h"
#include "rrcolorvecc.h"
#include "templates/rrstl.h"
#include "rrdxtcblock.inl"
#include "rrdxt1vqhelp.h" // needed for the .inl below
#include "rrdxt1vqhelp.inl" // the faster SAD/SSD are here
#include "cpux86.h"

RR_NAMESPACE_START

// DXTC formats :
// http://msdn.microsoft.com/en-us/library/bb694531(VS.85).aspx

/**

BC1 is : 

4color when end0 > end1 (end0 == end1 is 3color)

4color : end0, end1, 1/3, 2/3
3color : end0, end1, 1/2, black+transparent

BC1 4color all 1s = 0x55555555  (end1)
BC1 4color all 2s = 0xAAAAAAAA  (1/3)

BC1 4color indices flip for endpoint exchange is ^= 0x55555555  (all 1s)
				
**/

//-----------------------------------------------------

// See notes/dxt1_palette_notes.txt on rationale behind the formulas used here
// (TL;DR it's a blended model targeting a good compromise between NV, AMD
// and Intel HW decoders.)

// Straightforward fixed-point approximation to 1/3 with rounding bias
// in the R/B channels:
static inline U8 Lerp13_RB(U8 fm, U8 to)
{
	return (U8) (fm + (((to - fm) * 85 + 128) >> 8));
}

// NV is quite off in the G channel; this shifts things so that our
// compromise model wants to use a different weight.
static inline U8 Lerp13_G(U8 fm, U8 to)
{
	return (U8) (fm + (((to - fm) * 83 + 128) >> 8));
}

void DXT1_ComputePalette(U32 endpoints, rrColor32BGRA palette[4], rrDXT1PaletteMode mode)
{
	rrDXT1EndPoints ep;
	ep.dw = endpoints;
	DXT1_ComputePalette(ep.u.c0,ep.u.c1,palette,mode);
}

// Reference/scalar implementation of palette calc
static RADFORCEINLINE void DXT1_ComputePalette_RefImpl(rrColor32BGRA c0b,rrColor32BGRA c1b, rrColor32BGRA palette[4], rrDXT1PaletteMode mode, bool e0_gt_e1)
{
	RR_ASSERT(c0b.u.a == 255 && c1b.u.a == 255); // we copy these straight to the output, they need to have alpha set correctly

	palette[0] = c0b;
	palette[1] = c1b;

	if ( mode == rrDXT1PaletteMode_FourColor || e0_gt_e1 )
	{
		rrColor32BGRA c2;
		rrColor32BGRA c3;

		c2.u.r = Lerp13_RB(c0b.u.r,c1b.u.r);
		c2.u.g = Lerp13_G (c0b.u.g,c1b.u.g);
		c2.u.b = Lerp13_RB(c0b.u.b,c1b.u.b);
		c2.u.a = 0xFF;

		c3.u.r = Lerp13_RB(c1b.u.r,c0b.u.r);
		c3.u.g = Lerp13_G (c1b.u.g,c0b.u.g);
		c3.u.b = Lerp13_RB(c1b.u.b,c0b.u.b);
		c3.u.a = 0xFF;

		palette[2] = c2;
		palette[3] = c3;
	}
	else
	{
		rrColor32BGRA c2;

		c2.u.r = (c0b.u.r + c1b.u.r + 1)>>1;
		c2.u.g = (c0b.u.g + c1b.u.g + 1)>>1;
		c2.u.b = (c0b.u.b + c1b.u.b + 1)>>1;
		c2.u.a = 0xFF;

		palette[2] = c2;

		if ( mode == rrDXT1PaletteMode_Alpha )
		{
			palette[3].dw = 0;
		}
		else
		{
			// must stuff A = 255 so we can do 4-channel RGBA diffs
			palette[3].u.r = 0;
			palette[3].u.g = 0;
			palette[3].u.b = 0;
			palette[3].u.a = 0xFF;
		}
	}
}

void DXT1_ComputePalette_Ref(rrColor565Bits c0,rrColor565Bits c1, rrColor32BGRA palette[4], rrDXT1PaletteMode mode)
{
	rrColor32BGRA c0b = rrColor565Bits_UnQuantize(c0);
	rrColor32BGRA c1b = rrColor565Bits_UnQuantize(c1);
	DXT1_ComputePalette_RefImpl(c0b,c1b,palette,mode,c0.w > c1.w);
}

#ifdef DO_BUILD_SSE4

static RADFORCEINLINE void DXT1_ComputePalette_SSE4(const Vec128_S16 & endpoints16, rrColor32BGRA palette[4], rrDXT1PaletteMode mode, bool e0_gt_e1)
{
	store128u(palette, internal::BC1_ComputePalette_SSE4(endpoints16, mode, e0_gt_e1));
}

static RADFORCEINLINE void DXT1_ComputePalette_Impl(rrColor32BGRA c0b,rrColor32BGRA c1b, rrColor32BGRA palette[4], rrDXT1PaletteMode mode, bool e0_gt_e1)
{
	RR_ASSERT(c0b.u.a == 255 && c1b.u.a == 255); // we copy these straight to the output, they need to have alpha set correctly

	// The first two palette entries are always the two endpoints, form those
	const Vec128_S16 endpoints16 = internal::BC1_PackEndpointColors_SSE4(c0b.dw, c1b.dw);

	DXT1_ComputePalette_SSE4(endpoints16, palette, mode, e0_gt_e1);
}

void DXT1_ComputePalette(rrColorUnpacked565 c0b,rrColorUnpacked565 c1b, rrColor32BGRA palette[4], rrDXT1PaletteMode mode)
{
	// The first two palette entries are always the two endpoints, form those
	const Vec128_S16 endpoints16_orig = internal::BC1_PackEndpointColors_SSE4(c0b.dw, c1b.dw);
	const Vec128_S16 endpoints16 = internal::BC1_DequantEndpointsU32_SSE4(endpoints16_orig);

	DXT1_ComputePalette_SSE4(endpoints16, palette, mode, c0b.dw > c1b.dw);
}

void DXT1_ComputePalette_Batched(const rrDXT1UnpackedEndPoints endpoints[],int count32,rrColor32BGRA palettes[],rrDXT1PaletteMode mode)
{
	const SINTa count = count32;

	for (SINTa i = 0; i < count; i++)
	{
		const rrDXT1UnpackedEndPoints & ep = endpoints[i];
		const Vec128_S16 endpoints16_quant = Vec128_U8::loadu_lo64(&ep).to_sint16_lo();
		const Vec128_S16 endpoints16 = internal::BC1_DequantEndpointsU32_SSE4(endpoints16_quant);
		DXT1_ComputePalette_SSE4(endpoints16, palettes + i*4, mode, ep.c[0].dw > ep.c[1].dw);
	}
}

#else

static RADFORCEINLINE void DXT1_ComputePalette_Impl(rrColor32BGRA c0b,rrColor32BGRA c1b, rrColor32BGRA palette[4], rrDXT1PaletteMode mode, bool e0_gt_e1)
{
	DXT1_ComputePalette_RefImpl(c0b,c1b,palette,mode,e0_gt_e1);
}

void DXT1_ComputePalette(rrColorUnpacked565 c0b,rrColorUnpacked565 c1b,rrColor32BGRA palette[4], rrDXT1PaletteMode mode)
{
	DXT1_ComputePalette_Impl(c0b.dequantize(), c1b.dequantize(), palette, mode, c0b.dw > c1b.dw);
}

void DXT1_ComputePalette_Batched(const rrDXT1UnpackedEndPoints endpoints[],int count,rrColor32BGRA palettes[],rrDXT1PaletteMode mode)
{
	for (int i = 0; i < count; i++)
	{
		const rrDXT1UnpackedEndPoints & ep = endpoints[i];
		DXT1_ComputePalette(ep.c[0],ep.c[1],palettes + i*4,mode);
	}
}

#endif

void DXT1_ComputePalette(rrColor565Bits c0,rrColor565Bits c1, rrColor32BGRA palette[4], rrDXT1PaletteMode mode)
{
	rrColor32BGRA c0b = rrColor565Bits_UnQuantize(c0);
	rrColor32BGRA c1b = rrColor565Bits_UnQuantize(c1);
	DXT1_ComputePalette_Impl(c0b,c1b,palette,mode,c0.w > c1.w);
}

void DXT1_Decompress(rrColorBlock4x4 * pTo,const rrDXT1Block & from, rrDXT1PaletteMode mode)
{
	rrColor32BGRA palette[4];
	DXT1_ComputePalette(from.c0,from.c1,palette,mode);
	
	U32 indices = from.indices;
	for(int i=0;i<16;i++)
	{
		pTo->colors[i] = palette[ indices&3 ];
		indices >>= 2;
	}
}

void BC2_CompressAlpha(U8 * pTo, const U8 from_bgra[64])
{
	// Encode pixels in pairs
	for(int i=0;i<8;i++)
	{
		// correct quantization for restoration by bit replication :
		int nib0 = rrMul8Bit(from_bgra[i*8 + 3], 15);
		int nib1 = rrMul8Bit(from_bgra[i*8 + 7], 15);
		pTo[i] = (U8) (nib0 | (nib1 << 4));
	}
}

void BC2_DecompressAlpha(U8 to_bgra[64], const U8 * pFrom)
{
	// Decode pixels in pairs
	for(int i=0;i<8;i++)
	{
		U8 x = pFrom[i];
		// Decode by bit replication: (x<<4)|x = x*17
		to_bgra[i*8 + 3] = (U8) ((x & 0xf) * 17);
		to_bgra[i*8 + 7] = (U8) ((x >> 4) * 17);
	}
}

// 1bit transparent mask :
//	turn a bit on, every 2nd bit, if the pel is transparent
// so all opaque -> mask = 0
// all transparent -> mask = 55555
U32 DXT1_OneBitTransparent_Mask(const rrColorBlock4x4 & block)
{
	// could be an SSE2 movmsk ?
	U32 out = 0;
	for LOOP(i,16)
	{
		if ( block.colors[i].dw == 0 )
			out |= 1U<<(i*2);
	}
	return out;
}

bool DXT1_OneBitTransparent_Mask_Same(U32 mask, U32 endpoints, U32 indices)
{
	if ( DXT1_Is4Color(endpoints,rrDXT1PaletteMode_Alpha) )
	{
		// endpoints are 4c
		// mask must be 0 (none transparent)
		return mask == 0;
	}
	else
	{
		// transparent pels must be at indices == 3
		U32 indices3c = indices & (indices>>1) & 0x55555555;
		return mask == indices3c;
	}
}

bool DXT1_OneBitTransparent_Same(const rrColorBlock4x4 & b1,const rrColorBlock4x4 & b2)
{
	for LOOP(i,16)
	{
		bool t1 = ( b1.colors[i].dw == 0 );
		bool t2 = ( b2.colors[i].dw == 0 );
		if ( t1 != t2 )
			return false;
	}
	return true;
}

bool DXT1_OneBitTransparent_Same(const rrColorBlock4x4 & block,U32 endpoints, U32 indices)
{
	U32 mask = DXT1_OneBitTransparent_Mask(block);
	return DXT1_OneBitTransparent_Mask_Same(mask,endpoints,indices);
}


#ifdef RR_DO_ASSERTS
// DXT1_ComputeSSD_OneBitTransparent
// returns same error as DXT1_FindIndices_Alpha
//	this absolutely forbids transparency changes (returns RR_DXTC_ERROR_BIG)
U32 DXT1_ComputeSSD_OneBitTransparent(const rrColorBlock4x4 & block,const rrDXT1Block & dxtb,rrDXT1PaletteMode mode)
{
	// this is now used only in asserts
	// differs from DXT1_ComputeSSD_RGBA if the 1bt mask changes
	//	1bt mask is not allowed to change in any valid coding
	//	so this should == DXT1_ComputeSSD_RGBA for all valid codings

	if ( mode != rrDXT1PaletteMode_Alpha )
	{
		// don't care about alpha, just do RGB SSD
		return DXT1_ComputeSSD_RGBA(block,dxtb,mode);
	}
	
	rrColor32BGRA palette[4];
	DXT1_ComputePalette(dxtb.c0,dxtb.c1,palette,rrDXT1PaletteMode_Alpha);
	
	U32 err = 0;
	U32 indices = dxtb.indices;
	
	RR_ASSERT( mode == rrDXT1PaletteMode_Alpha );
	bool block_has_transparent = rrColorBlock4x4_HasAnyOneBitTransparent(&block);
	bool palette_has_transparent = ( palette[3].u.a == 0 );
	
	if ( block_has_transparent != palette_has_transparent )
	{
		// mismatch!
		if ( block_has_transparent )
		{
			// block has transparent but I can't map it to any palette entry
			// I consider "transparent or not" to be a bit I'm not allowed to change
			// just big error
			return RR_DXTC_ERROR_BIG;
		}
		else
		{
			// palette has transparent but block does not
			// must not use index 3
			
			for(int i=0;i<16;i++)
			{
				int index = indices&3; indices >>= 2;
				if ( index == 3 )
					return RR_DXTC_ERROR_BIG;				
				err += rrColor32BGRA_DeltaSqrRGB( block.colors[i] , palette[index] );
			}
		}
	}
	else
	{
		if ( block_has_transparent )
		{
			// both have alpha
			
			for(int i=0;i<16;i++)
			{
				int index = indices&3; indices >>= 2;
				bool c_t = rrColor32BGRA_IsOneBitTransparent(block.colors[i]);
				bool p_t = (index == 3);
				if ( c_t != p_t )
				{
					// transparency bit	mismatch
					return RR_DXTC_ERROR_BIG;			
				}
				else
				{
					// transparent bit is the same		
					// if transparent, RGB does not contribute
					// (no point counting the RGB error when transparent because DXT1 transparent is always black)
					if ( ! c_t )
					{
						err += rrColor32BGRA_DeltaSqrRGB( block.colors[i] , palette[index] );
					}
				}
			}
		}
		else	
		{
			// neither have alpha
		
			for(int i=0;i<16;i++)
			{
				int index = indices&3; indices >>= 2;
				err += rrColor32BGRA_DeltaSqrRGB( block.colors[i] , palette[index] );
			}
		}
	}
		
	return err;
}
#endif // RR_DO_ASSERTS


bool rrColorBlock4x4_IsBC1Canonical(const rrColorBlock4x4 & colors,rrDXT1PaletteMode mode)
{
	// at this point colors should already be canonical :
	// either A = 255
	// or canonicalized 1-bit-transparent alpha :

	if ( mode == rrDXT1PaletteMode_Alpha )
	{
		for LOOP(i,16)
		{
			if ( ! rrColor32BGRA_IsOneBitTransparentCanonical(colors.colors[i]) )
				return false;
		}
	}
	else
	{
		for LOOP(i,16)
		{
			// all a's should be 255
			if ( colors.colors[i].u.a != 255 )
				return false;
		}
	}
	
	return true;
}


// NOTE: this is actual RGBA SSD , no A bias
U32 DXT1_ComputeSSD_RGBA(const rrColorBlock4x4 & color,const rrDXT1Block & dxtb, rrDXT1PaletteMode mode)
{
	RR_ASSERT( rrColorBlock4x4_IsBC1Canonical(color,mode) );

	rrColor32BGRA palette[4];
	DXT1_ComputePalette(dxtb.c0,dxtb.c1,palette,mode);
	
	// RGBA diff but we should be canonical
	U32 ret = BC1_Palette_SSD_RGBA(&color,palette,dxtb.indices);
		
	return ret;
}

void SwapRB(rrColorBlock4x4 * pBlock)
{
	for(int i=0;i<16;i++)
	{
		RR_NAMESPACE::swap( pBlock->colors[i].u.r, pBlock->colors[i].u.b );
	}
}

void KillAlpha(rrColorBlock4x4 & from)
{
	for(int i=0;i<16;i++)
	{
		from.colors[i].u.a = 255;
	}
}

//===================================================================================================

// returns (err << 2) | index
static inline U32 DXT1_FindIndexAndErr_Brute2_RGBA_1BT(rrColor32BGRA color, const rrColor32BGRA palette[4])
{
	// picks color indices by these rules, in priority order:
	// 1. must match target alpha
	// 2. if alpha matches, pick color index by minimum error
	// 3. on ties, pick lowest numerical index
	//
	// color need to be canonicalized for the alpha handling to work,
	// but it's the same for all palette modes.
	U32 d0 = rrColor32BGRA_DeltaSqrRGBA_1BT(color,palette[0]);
	U32 d1 = rrColor32BGRA_DeltaSqrRGBA_1BT(color,palette[1]);
	U32 d2 = rrColor32BGRA_DeltaSqrRGBA_1BT(color,palette[2]);
	U32 d3 = rrColor32BGRA_DeltaSqrRGBA_1BT(color,palette[3]);
	
	U32 best = d0*4;
	best = RR_MIN(best, d1*4 + 1);
	best = RR_MIN(best, d2*4 + 2);
	best = RR_MIN(best, d3*4 + 3);
	
	return best;
}

// just the error
static inline U32 DXT1_FindErr_Brute2_RGBA_1BT(rrColor32BGRA color, const rrColor32BGRA palette[4])
{
	U32 best;

	best = rrColor32BGRA_DeltaSqrRGBA_1BT(color,palette[0]);
	best = RR_MIN(best, rrColor32BGRA_DeltaSqrRGBA_1BT(color,palette[1]));
	best = RR_MIN(best, rrColor32BGRA_DeltaSqrRGBA_1BT(color,palette[2]));
	best = RR_MIN(best, rrColor32BGRA_DeltaSqrRGBA_1BT(color,palette[3]));

	return best;
}

//===============================================================================

U32 DXT1_FindIndices(const rrColorBlock4x4 & block, U32 endpoints,rrDXT1PaletteMode mode, U32 * pError)
{
	rrColor32BGRA palette[4];
	DXT1_ComputePalette(endpoints,palette,mode);
	return DXT1_FindIndices(block,palette,pError);
}


#ifdef DO_BUILD_SSE4 // FindIndices 

namespace internal {

static RADFORCEINLINE Vec128 FindIndicesInner(const Vec128 & row, const Vec128 & pal_br16, const Vec128 & pal_ga16)
{
	Vec128 row_br16, row_ga16;

	UnpackBGRAto16_Weighted_2x(row_br16, row_ga16, row);
	return BC1_FindBestIndices_SSE4(row_br16, row_ga16, pal_br16, pal_ga16);
}

// Just the errors. Doesn't need the extra 2x scaling on every component and has a cheaper reduction.
static RADFORCEINLINE Vec128 FindErrorsInner(const Vec128 & row, const Vec128 & pal_br16, const Vec128 & pal_ga16)
{
	Vec128 row_br16, row_ga16;

	UnpackBGRAto16_Weighted(row_br16, row_ga16, row);
	return BC1_FindBestErrors_SSE4(row_br16, row_ga16, pal_br16, pal_ga16);
}

static RADFORCEINLINE U32 DXT1_FindIndices_SSE4(const rrColorBlock4x4 & block, const rrColor32BGRA palette[4],U32 * pError)
{
	// There's a bunch of assumptions and simplifications in here, enough so that it's hard to all keep straight,
	// so here's the full rationale for everything in here:
	//
	// The basic goal is to select, per pixel, the palette entry that minimizes the SSD (sum of squared
	// differences) in the color channels, but with a few wrinkles.
	//
	// Inputs are expected to be canonicalized for our given palette mode (checked above). In particular, this
	// means that in 4c mode, all alpha values in "block" _must_ be 255, and in 3c mode they must be either
	// 0 or 255. Note that our generated palette in 3c mode depends on the palette mode! In when palette mode
	// has alpha enabled, index 3 has alpha=0 (which is how the GPU decodes it), but in no-alpha mode, we
	// actually consider index 3 to have alpha=255, and likewise canonicalize all our inputs to have alpha=255
	// for every pixel. Thus the computed error in the alpha channel is always 0 for every pixel in NoAlpha
	// mode.
	//
	// With transparency enabled, we require exact match of the 1-bit transparency flag. The way we accomplish
	// this is by scaling up alpha differences so that a mismatch in the A channel is considered a larger
	// error than the largest possible RGB error. As noted above, the A channel values in both our palette and
	// the reference block only ever contain 0 or 255. Therefore, the SSD component for the alpha channel is
	// at most 255^2 = 65,025. However, we have 3 other color chanels that can also have an SSD contribution of
	// up to 65,025 each. To make sure that we always exactly match in the A channel, we scale up the diffs
	// in A by a factor of 2, which scales the A contribution in the SSD by a factor of 4, enough to be larger
	// than any error contribution from the remaining 3 channels. This is enough to guarantee that we always
	// match A exactly, meaning the A contribution from the palette index we return is always 0. So somewhat
	// counter-intuitively, even though what we compute is a weighted SSD, our return value is always a
	// pure unweighted RGB SSD with exact match in the 1-bit alpha channel (subject to the NoAlpha rules above).
	//
	// SSD calculation happens in fixed point. The SSE2 instruction PMADDWD (_mm_madd_epi16) happens to be
	// very convenient for this, so it's what we use. This works on 16-bit signed integer values. Instead of
	// unpacking channels to 16-bit, it ends up cheaper to use masking, which means we have one PMADDWD dot
	// product for the B/R channels diffs (the even-numbered channels), and another for the odd-numbered
	// channels.
	//
	// After computing the SSDs for the 4 palette entries, we need to select the index that results in the
	// smallest SSD. Individual errors per pixel fit in 18 bits comfortably, and we have 16 pixels, so this
	// sum fits in 22 bits. That means that instead of doing a comparison tree or similar, we can simply
	// multiply the SSD by 4 and stuff the index value in the bottom 2 bits. Then we get both the index for
	// the palette entry with the smallest error, and what that error is, by computing the min over all 4
	// values of (SSD[i]*4 + i). (Ties are broken towards smaller indices.)
	//
	// Instead of multiplying the final SSDs by 4, it happens to be slightly cheaper to scale the values
	// in all color channels by 2 before we compute the differences. Since we independently also scale
	// the A channel by 2, that means the R,G,B channel values get scaled by 2, whereas A gets scaled by
	// a total of 4 prior to the error calculation.
	//
	// In terms of value range, that means the contributions of the R,G,B channels are at most 510^2 each
	// per pixel (fits in 18 bits), and the A contribution (pre-min reduction) is at most 1020^2 (fits in
	// 20 bits). As noted above, we never pick pixels with an effective A error that is nonzero after
	// calculation the mins, and we'd be good on range and free from overflows either way.

	// Load and unpack the palette
	Vec128 pal_br16, pal_ga16;

	UnpackBGRAto16_Weighted_2x(pal_br16, pal_ga16, load128u(palette));

	// Process all four rows of pixels
	const Vec128 best0 = FindIndicesInner(load128u(block.colors +  0), pal_br16, pal_ga16);
	const Vec128 best1 = FindIndicesInner(load128u(block.colors +  4), pal_br16, pal_ga16);
	const Vec128 best2 = FindIndicesInner(load128u(block.colors +  8), pal_br16, pal_ga16);
	const Vec128 best3 = FindIndicesInner(load128u(block.colors + 12), pal_br16, pal_ga16);

	// Extract index bits
	U32 indices;
	indices  = BC1_ExtractIndices_SSE4(best0, best1);
	indices |= BC1_ExtractIndices_SSE4(best2, best3) << 16;

	// Horizontal reduction for final error sum
	if (pError)
	{
		Vec128 error_sum01 = _mm_add_epi32(_mm_srli_epi32(best0, 2), _mm_srli_epi32(best1, 2));
		Vec128 error_sum23 = _mm_add_epi32(_mm_srli_epi32(best2, 2), _mm_srli_epi32(best3, 2));
		Vec128 error_sum = _mm_add_epi32(error_sum01, error_sum23);

		*pError = reduce_add_s32(error_sum);
	}
	
	return indices;
}

static RADFORCEINLINE U32 DXT1_FindErrors_SSE4(const rrColorBlock4x4 & block, const rrColor32BGRA palette[4])
{
	// Load and unpack the palette
	Vec128 pal_br16, pal_ga16;

	UnpackBGRAto16_Weighted(pal_br16, pal_ga16, load128u(palette));

	// Sum the errors
	Vec128 error_sum;
	error_sum = FindErrorsInner(load128u(block.colors), pal_br16, pal_ga16);
	error_sum = _mm_add_epi32(error_sum, FindErrorsInner(load128u(block.colors +  4), pal_br16, pal_ga16));
	error_sum = _mm_add_epi32(error_sum, FindErrorsInner(load128u(block.colors +  8), pal_br16, pal_ga16));
	error_sum = _mm_add_epi32(error_sum, FindErrorsInner(load128u(block.colors + 12), pal_br16, pal_ga16));

	// Finish horizontal reduction
	return reduce_add_s32(error_sum);
}

U32 DXT1_EvalPalette_SSE4(const DXT1_FindErrorsContext * ctx, const rrColor32BGRA palette[4])
{
	const Vec128 * pix_br = (const Vec128 *)&ctx->pixels.unpacked[0];
	const Vec128 * pix_ga = (const Vec128 *)&ctx->pixels.unpacked[32];

	// Load and unpack the palette
	Vec128 pal_br16, pal_ga16;
	UnpackBGRAto16_Weighted(pal_br16, pal_ga16, load128u(palette));

	// Sum the errors
	Vec128 error_sum;
	error_sum = BC1_FindBestErrors_SSE4(pix_br[0], pix_ga[0], pal_br16, pal_ga16);
	error_sum = _mm_add_epi32(error_sum, BC1_FindBestErrors_SSE4(pix_br[1], pix_ga[1], pal_br16, pal_ga16));
	error_sum = _mm_add_epi32(error_sum, BC1_FindBestErrors_SSE4(pix_br[2], pix_ga[2], pal_br16, pal_ga16));
	error_sum = _mm_add_epi32(error_sum, BC1_FindBestErrors_SSE4(pix_br[3], pix_ga[3], pal_br16, pal_ga16));

	return reduce_add_s32(error_sum);
}

void DXT1_EvalPalettes_SSE4(const DXT1_FindErrorsContext * ctx, U32 out_ssds[], const rrColor32BGRA palettes[], int count)
{
	const Vec128 * pix_br = (const Vec128 *)&ctx->pixels.unpacked[0];
	const Vec128 * pix_ga = (const Vec128 *)&ctx->pixels.unpacked[32];

	for (int i = 0; i < count; ++i)
	{
		// Load and unpack the palette
		Vec128 pal_br16, pal_ga16;
		UnpackBGRAto16_Weighted(pal_br16, pal_ga16, load128u(palettes + i*4));

		// Sum the errors
		Vec128 error_sum;
		error_sum = BC1_FindBestErrors_SSE4(pix_br[0], pix_ga[0], pal_br16, pal_ga16);
		error_sum = _mm_add_epi32(error_sum, BC1_FindBestErrors_SSE4(pix_br[1], pix_ga[1], pal_br16, pal_ga16));
		error_sum = _mm_add_epi32(error_sum, BC1_FindBestErrors_SSE4(pix_br[2], pix_ga[2], pal_br16, pal_ga16));
		error_sum = _mm_add_epi32(error_sum, BC1_FindBestErrors_SSE4(pix_br[3], pix_ga[3], pal_br16, pal_ga16));

		out_ssds[i] = reduce_add_u32(error_sum);
	}
}

U32 DXT1_FindBestPalette_SSE4(const DXT1_FindErrorsContext * ctx, const rrColor32BGRA palettes[], int count)
{
	RR_ASSERT(count <= DXT1_FindErrorsContext::COUNT_LIMIT);

	const Vec128 * pix_br = (const Vec128 *)&ctx->pixels.unpacked[0];
	const Vec128 * pix_ga = (const Vec128 *)&ctx->pixels.unpacked[32];

	Vec128 best_overall_vec = _mm_set1_epi32(-1);
	Vec128 index_vec = _mm_setzero_si128();

	for (int i = 0; i < count; ++i)
	{
		// Load and unpack the palette
		Vec128 pal_br16, pal_ga16;
		UnpackBGRAto16_Weighted(pal_br16, pal_ga16, load128u(palettes + i*4));

		// Sum the errors
		Vec128 error_sum;
		error_sum = BC1_FindBestErrors_SSE4(pix_br[0], pix_ga[0], pal_br16, pal_ga16);
		error_sum = _mm_add_epi32(error_sum, BC1_FindBestErrors_SSE4(pix_br[1], pix_ga[1], pal_br16, pal_ga16));
		error_sum = _mm_add_epi32(error_sum, BC1_FindBestErrors_SSE4(pix_br[2], pix_ga[2], pal_br16, pal_ga16));
		error_sum = _mm_add_epi32(error_sum, BC1_FindBestErrors_SSE4(pix_br[3], pix_ga[3], pal_br16, pal_ga16));

		// Horizontal reduce
		error_sum = reduce_add_s32_2away(error_sum);
		error_sum = reduce_add_s32_1away(error_sum);

		// Put in the index and keep track of overall best
		Vec128 score = _mm_or_si128(_mm_slli_epi32(error_sum, DXT1_FindErrorsContext::COUNT_SHIFT), index_vec);
		best_overall_vec = _mm_min_epu32(best_overall_vec, score);
		index_vec = _mm_add_epi32(index_vec, _mm_set1_epi32(1));
	}

	return _mm_cvtsi128_si32(best_overall_vec);
}

}

#endif // SSE4 work funcs

#ifdef DO_BUILD_SSE4 // put this in another ifdef just to make it easier to toggle the part that splices in the optimized funcs

U32 DXT1_FindIndices(const rrColorBlock4x4 & block, const rrColor32BGRA palette[4],U32 * pError)
{
#ifdef DO_BUILD_AVX2
	if ( rrCPUx86_feature_present(RRX86_CPU_AVX2) )
	{
		return internal::DXT1_FindIndices_AVX2(block,palette,pError);
	}
#endif

	return internal::DXT1_FindIndices_SSE4(block,palette,pError);
}

U32 DXT1_FindErrors(const rrColorBlock4x4 & block, const rrColor32BGRA palette[4])
{
#ifdef DO_BUILD_AVX2
	if ( rrCPUx86_feature_present(RRX86_CPU_AVX2) )
	{
		return internal::DXT1_FindErrors_AVX2(block,palette);
	}
#endif

	return internal::DXT1_FindErrors_SSE4(block,palette);
}

void DXT1_FindErrorsContext::init(const rrColorBlock4x4 & block)
{
	for(int i = 0; i < 4; i++)
	{
		Vec128 pix_br, pix_ga;
		internal::UnpackBGRAto16_Weighted(pix_br, pix_ga, load128u(block.colors + i*4));
		store128u(pixels.unpacked + i*8, pix_br);
		store128u(pixels.unpacked + i*8 + 32, pix_ga);
	}

	eval_palette = internal::DXT1_EvalPalette_SSE4;
	eval_palettes = internal::DXT1_EvalPalettes_SSE4;
	find_best_palette = internal::DXT1_FindBestPalette_SSE4;

#ifdef DO_BUILD_AVX2
	if ( rrCPUx86_feature_present(RRX86_CPU_AVX2) )
	{
		eval_palette = internal::DXT1_EvalPalette_AVX2;
		eval_palettes = internal::DXT1_EvalPalettes_AVX2;
		find_best_palette = internal::DXT1_FindBestPalette_AVX2;
	}
#endif
}

#else // not DO_BUILD_SSE4

//===============================================================================
// scalar fallbacks for FindIndices :

U32 DXT1_FindIndices(const rrColorBlock4x4 & block, const rrColor32BGRA palette[4],U32 * pError)
{
	U32 indices = 0;
	U32 err = 0;

	for(int i=0;i<16;i++)
	{
		U32 index_err = DXT1_FindIndexAndErr_Brute2_RGBA_1BT(block.colors[i],palette);
	
		indices >>= 2;
		indices |= index_err << 30; // ends up implicitly masking out just the bottom 2 bits, great!

		err += index_err >> 2;
	}

	if ( pError )
		*pError = err;

	return indices;
}

U32 DXT1_FindError(const rrColorBlock4x4 & block, const rrColor32BGRA palette[4])
{
	U32 err = 0;
	for(int i=0;i<16;i++)
		err += DXT1_FindErr_Brute2_RGBA_1BT(block.colors[i],palette);

	return err;
}

static U32 Generic_EvalPalette(const DXT1_FindErrorsContext * ctx, const rrColor32BGRA palette[4])
{
	return DXT1_FindError(ctx->pixels.packed, palette);
}

static void Generic_EvalPalettes(const DXT1_FindErrorsContext * ctx, U32 out_ssds[], const rrColor32BGRA palettes[], int count)
{
	for (int i = 0; i < count; ++i)
		out_ssds[i] = DXT1_FindError(ctx->pixels.packed, &palettes[i*4]);
}

static U32 Generic_FindBestPalette(const DXT1_FindErrorsContext * ctx, const rrColor32BGRA palettes[], int count)
{
	RR_ASSERT(count <= DXT1_FindErrorsContext::COUNT_LIMIT);
	U32 best_overall = ~0u;

	for (int i = 0; i < count; ++i)
	{
		U32 err = DXT1_FindError(ctx->pixels.packed, palettes + i*4);
		best_overall = RR_MIN(best_overall, (err << DXT1_FindErrorsContext::COUNT_SHIFT) + i);
	}

	return best_overall;
}

void DXT1_FindErrorsContext::init(const rrColorBlock4x4 & block)
{
	pixels.packed = block;
	eval_palette = Generic_EvalPalette;
	eval_palettes = Generic_EvalPalettes;
	find_best_palette = Generic_FindBestPalette;
}

#endif // processor for FindIndices

//===================================================================================================

void rrDXT1_PutBlock(U8 * outPtr,const rrDXT1Block & block)
{
	#ifdef __RADLITTLEENDIAN__
	*((rrDXT1Block *)outPtr) = block;
	#else
	#pragma error // need to put words as little endian
	#endif
}

void rrDXT1_GetBlock(const U8 * inPtr, rrDXT1Block * pBlock)
{
	#ifdef __RADLITTLEENDIAN__
	*pBlock = *((rrDXT1Block *)inPtr);
	#else
	#pragma error // need to put words as little endian
	#endif	
}

//===================================================================================================

bool rrColorBlock4x4_HasAnyOneBitTransparent(const rrColorBlock4x4 * pBlock)
{
	for(int i=0;i<16;i++)
	{
		if ( rrColor32BGRA_IsOneBitTransparent(pBlock->colors[i]) )
			return true;
	}
	
	return false;
}

//===================================================================================================

static inline const rrColor32BGRA Vec3iToColor_div_f_round(const rrVec3i & vec, int divider)
{
	rrColor32BGRA c;
	F32 recip = 1.f / divider;
	c.u.b = U8_check( rr_froundint( vec.x * recip ) );
	c.u.g = U8_check( rr_froundint( vec.y * recip ) );
	c.u.r = U8_check( rr_froundint( vec.z * recip ) );
	c.u.a = 0xFF;
	return c;
}

void DXT1_SolveRGB4Means_Scalar(rrColor32BGRA means[4], const rrColorBlock4x4 & colors)
{
	for(;;)
	{
		// refine 4-means :
		rrVec3i sums[4];
		memset(sums,0,4*sizeof(rrVec3i));
		int	counts[4] = { 0 };

		//rmse_total = 386.710

		U32 worst_d_i = 0; // (d << 4) | (15 - i); max reduction of these gives the lowest i at which the largest d occurs.

		// this is essentially FindIndices but with "means" as the palette
		for(int i=0;i<16;i++)
		{
			const rrColor32BGRA & c = colors.colors[i];
			U32 d0 = rrColor32BGRA_DeltaSqrRGB( c , means[0] );
			U32 d1 = rrColor32BGRA_DeltaSqrRGB( c , means[1] );
			U32 d2 = rrColor32BGRA_DeltaSqrRGB( c , means[2] );
			U32 d3 = rrColor32BGRA_DeltaSqrRGB( c , means[3] );

			// NOTE(fg): d values are <= 3 * 255 * 255 so they comfortably fit in 18 bits

			d0 <<= 2;
			d1 = (d1<<2) + 1;
			d2 = (d2<<2) + 2;
			d3 = (d3<<2) + 3;
			U32 min_d = RR_MIN4(d0,d1,d2,d3);
			int index = min_d & 3;

			// NOTE(fg): scaled min_d with index in bottom bits = 20 bits

			sums[index] += ColorToVec3i(c);
			counts[index] ++;

			// track which point is furthest from mean
			// we want to break ties in favor of earlier points since that's
			// what the original logic did, and we still include the index as
			// part of the error, even though that's wonky, for no other
			// reason than that's what the code originally did and I don't
			// want to change behavior here.
			U32 d_i = (min_d << 4) | (i ^ 15); // NOTE(fg): fits in 24 bits
			worst_d_i = RR_MAX(worst_d_i,d_i);
		}

		bool anyChanged = false;

		for(int i=0;i<4;i++)
		{
			// if count == 0 , nobody mapped there,
			int count = counts[i];
			if ( count == 0 )
			{
				//just leave means alone
				// rmse_total = 386.710
				//change this mean to the point that was furthest from any mean :
				// rmse_total = 385.078
				int worst_i = 15 ^ (worst_d_i & 15);
				means[i] = colors.colors[worst_i];
				continue;
			}

			rrColor32BGRA color = Vec3iToColor_div_f_round(sums[i],count);

			if ( color.dw != means[i].dw )
			{
				anyChanged = true;
				means[i] = color;
			}
		}

		if ( ! anyChanged )
			break;
	}
}

#ifdef DO_BUILD_SSE4

namespace internal {

static RADFORCEINLINE void Interleave32(Vec128_U8 & a, Vec128_U8 & b)
{
	Vec128_U8 t = a;
	a = Vec128_U8(_mm_unpacklo_epi32(a, b));
	b = Vec128_U8(_mm_unpackhi_epi32(t, b));
}

static RADFORCEINLINE void Transpose32_4x4(Vec128_U8 & r0, Vec128_U8 & r1, Vec128_U8 & r2, Vec128_U8 & r3)
{
	// The usual transpose out of perfect even-odd interleaves
	// First pass
	Interleave32(r0, r2);
	Interleave32(r1, r3);

	// Second pass
	Interleave32(r0, r1);
	Interleave32(r2, r3);
}

static RADFORCEINLINE void Unpack8to16_BR_G0_Scaled(Vec128_S16 & out_br, Vec128_S16 & out_g0, const Vec128_U8 & pixels)
{
	Vec128_U16 pixels_br = (pixels & Vec128_U8::repeat4(0xff, 0x00, 0xff, 0x00)).u16();
	Vec128_U16 pixels_g0 = (pixels & Vec128_U8::repeat4(0x00, 0xff, 0x00, 0x00)).u16();

	// Results are scaled by 2
	out_br = (pixels_br + pixels_br).s16();
	out_g0 = Vec128_S16(_mm_srli_epi16(pixels_g0, 7));
}

void DXT1_SolveRGB4Means_SSE4(rrColor32BGRA means[4], const rrColorBlock4x4 & colors)
{
	// Extract just the R, G and B channels of colors
	Vec128_U8 color_chan[4]; // we only care about R, G, B but A is sort of along for the ride

	// Group BBBB GGGG RRRR AAAA within each row of the 4x4 block:
	Vec128_S8 c_separation_shuf(0,4,8,12, 1,5,9,13, 2,6,10,14, 3,7,11,15);
	color_chan[0] = Vec128_U8::loadu(colors.colors +  0).shuf(c_separation_shuf);
	color_chan[1] = Vec128_U8::loadu(colors.colors +  4).shuf(c_separation_shuf);
	color_chan[2] = Vec128_U8::loadu(colors.colors +  8).shuf(c_separation_shuf);
	color_chan[3] = Vec128_U8::loadu(colors.colors + 12).shuf(c_separation_shuf);

	// 4x4 transpose and we have the channels fully separated!
	Transpose32_4x4(color_chan[0], color_chan[1], color_chan[2], color_chan[3]);

	// Also unpack the 8-bit colors to 16-bit B,R and 16-bit G,0 pairs, and scale them by 2
	Vec128_S16 colors_br[4], colors_g0[4];
	for (int i = 0; i < 4; ++i)
		Unpack8to16_BR_G0_Scaled(colors_br[i], colors_g0[i], Vec128_U8::loadu(colors.colors + i*4));

	// Now we're ready to go into the main loop
	for(;;)
	{
		// Unpack the means vector too
		Vec128_S16 means_br, means_g0;
		Unpack8to16_BR_G0_Scaled(means_br, means_g0, Vec128_U8::loadu(means));

		// Determine for each pixel what cluster center it's closest to,
		// working rows at a time
		Vec128_S32 inv_pixel_idx(15, 14, 13, 12);
		Vec128_U8 chosen_idx = Vec128_U8::zero();
		Vec128_S32 worst_d_i = Vec128_S32::zero();

		for (int i = 0; i < 4; ++i)
		{
			Vec128_S16 col_br = colors_br[i];
			Vec128_S16 col_g0 = colors_g0[i];

			// Compute the four distances
			// because of our pre-scaling by 2, they come out pre-multiplied by 4
			Vec128_S32 d0(DeltaSqrRGBA_SSE2(col_br, col_g0, shuffle32<0,0,0,0>(means_br), shuffle32<0,0,0,0>(means_g0)));
			Vec128_S32 d1(DeltaSqrRGBA_SSE2(col_br, col_g0, shuffle32<1,1,1,1>(means_br), shuffle32<1,1,1,1>(means_g0)));
			Vec128_S32 d2(DeltaSqrRGBA_SSE2(col_br, col_g0, shuffle32<2,2,2,2>(means_br), shuffle32<2,2,2,2>(means_g0)));
			Vec128_S32 d3(DeltaSqrRGBA_SSE2(col_br, col_g0, shuffle32<3,3,3,3>(means_br), shuffle32<3,3,3,3>(means_g0)));

			// Put the right values in the low bits then perform the min reduction
			// to find the best index
			Vec128_S32 best01(_mm_min_epi32(d0, d1 + Vec128_S32(1)));
			Vec128_S32 best23(_mm_min_epi32(d2, d3 + Vec128_S32(1)));
			Vec128_S32 best(_mm_min_epi32(best01, best23 + Vec128_S32(2)));

			// Extract the chosen inds and save them for later
			Vec128_U8 idx_this_row = Vec128_U8(best & Vec128_S32(3)).shuf(c_separation_shuf);
			chosen_idx = Vec128_U8(_mm_alignr_epi8(idx_this_row, chosen_idx, 4)); // shift into chosen_idx from the top

			// Keep track of the worst distortion/index combinations
			Vec128_S32 d_i = Vec128_S32(_mm_slli_epi32(best, 4)) | inv_pixel_idx;
			worst_d_i = Vec128_S32(_mm_max_epi32(worst_d_i, d_i));
			inv_pixel_idx -= Vec128_S32(4);
		}

		// Finish the worst_d_i reduction
		worst_d_i = Vec128_S32(_mm_max_epi32(worst_d_i, shuffle32<2,3,0,1>(worst_d_i))); // with 2 away
		worst_d_i = Vec128_S32(_mm_max_epi32(worst_d_i, shuffle32<1,0,3,2>(worst_d_i))); // with 1 away

		// Determine worst_i
		int worst_i = 15 ^ (_mm_cvtsi128_si32(worst_d_i) & 15);

		// We have now assigned each pixel to its closest mean; time to determine the new position
		// of the means!
		bool anyChanged = false;
		Vec128_U8 current_idx = Vec128_U8::zero();

		for (int i = 0; i < 4; ++i, current_idx += Vec128_U8(1))
		{
			Vec128_U8 in_cluster_mask = chosen_idx.cmp_eq(current_idx);

			// If this mean didn't get chosen, replace it with the point furthest from any of our means
			if (_mm_testz_si128(in_cluster_mask, in_cluster_mask))
			{
				means[i] = colors.colors[worst_i];
				continue;
			}

			// Sum B, G, R, 1 for all pixels assigned to this mean using SAD
			Vec128 zero = _mm_setzero_si128();

			Vec128 sum0 = _mm_sad_epu8(color_chan[0] & in_cluster_mask, zero);
			Vec128 sum1 = _mm_sad_epu8(color_chan[1] & in_cluster_mask, zero);
			Vec128 sum2 = _mm_sad_epu8(color_chan[2] & in_cluster_mask, zero);
			Vec128 count = _mm_sad_epu8(Vec128_U8(1) & in_cluster_mask, zero);

			// We now have 16-bit sums in 64-bit lanes; pack it down so we have densely packed
			// 16-bit values, and the two 64-bit sub-sums right next to each other
			Vec128 pack32_0 = _mm_packs_epi32(sum0, sum1);
			Vec128 pack32_1 = _mm_packs_epi32(sum2, count);
			Vec128_S16 pack16(_mm_packs_epi32(pack32_0, pack32_1));

			// Sum the two counts from the low and high halves of the original 128-bit vector
			// together and also convert the sums to 32-bit integer
			Vec128_S32 sums32 = pack16.madd(Vec128_S16(1));

			// Convert to float to finish our calc
			// Calculation must match Vec3iToColor_div_f_round!
			//
			// B,G,R are in first 3 lanes, count of pixels in cluster is in last lane
			VecF32x4 sums_flt = VecF32x4::from_int32(sums32);
			VecF32x4 recip = VecF32x4(1.0f) / sums_flt.wwww();

			// NOTE(fg): need to add 0.5 and then truncate because that's what rr_froundint
			// does in this case (sums are non-negative)
			//
			// RTNE would be nicer here but doesn't match our scalar code!
			Vec128_S32 new_mean((sums_flt * recip + VecF32x4(0.5f)).to_int32_trunc());

			// Pack down (just the first byte of every lane)
			Vec128_U8 new_mean_narrow = Vec128_U8(new_mean).shuf(c_separation_shuf);
			new_mean_narrow |= Vec128_U8::repeat4(0, 0, 0, 0xff); // force A to 255

			U32 old_mean = means[i].dw;
			store32u(means + i, new_mean_narrow);

			anyChanged |= means[i].dw != old_mean;
		}

		if ( !anyChanged )
			break;
	}
}

} // internal namespace

#endif

// means is in-out; it starts with the initial estimates and gets updated
// to contain the solved means.
//
// Only RGB channels matter, values in A channel (of both source and colors)
// are ignored and not guaranteed to be initialized to anything in particular.
void DXT1_SolveRGB4Means(rrColor32BGRA means[4], const rrColorBlock4x4 & colors)
{
#ifdef DO_BUILD_SSE4
	//rrColor32BGRA ref_means[4] = { means[0], means[1], means[2], means[3] };

	internal::DXT1_SolveRGB4Means_SSE4(means, colors);

	//DXT1_SolveRGB4Means_Scalar(ref_means, colors);
	//RR_ASSERT_ALWAYS( memcmp(ref_means, means, sizeof(ref_means)) == 0 );
#else
	DXT1_SolveRGB4Means_Scalar(means, colors);
#endif
}

RR_NAMESPACE_END
