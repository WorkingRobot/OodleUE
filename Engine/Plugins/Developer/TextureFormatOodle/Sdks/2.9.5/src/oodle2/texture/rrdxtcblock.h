// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRRBITMAP_DXTC_BLOCK_H__
#define __RADRRBITMAP_DXTC_BLOCK_H__

#include "rrcolor.h"

RR_NAMESPACE_START
// needs C++

//===============================================================================
// DXT1 block and color block :

struct rrColorBlock4x4 // BGRA by old convention
{
	rrColor32BGRA		colors[16];
};

RR_COMPILER_ASSERT( sizeof(rrColorBlock4x4) == 64 );

// BC1 == DXT1

union rrDXT1EndPoints
{
	struct
	{
		rrColor565Bits	c0,c1;
	} u;
	U32		dw;
};

// These have the b, g, r bitfield unpacked to bytes but not expanded to
// full 8-bit value range; b and r are still in [0,31] and g in [0,63].
struct rrDXT1UnpackedEndPoints
{
	rrColorUnpacked565 c[2];
};

#ifdef _MSC_VER
#pragma warning(disable: 4201) // warning C4201: nonstandard extension used : nameless struct/union
#endif

struct rrDXT1Block
{
	// was :
	//rrColor565Bits	c0,c1;	// c0 > c1 for normal blocks
	// alternative :
	//rrDXT1EndPoints endpoints;
	// nameless struct/union is now okay in C11 :
	union {
		U32		endpoints;
		struct
		{
			rrColor565Bits	c0,c1;
		};
	};
	U32				indices;	
};

RR_COMPILER_ASSERT( sizeof(rrDXT1Block) == 8 );

static RADINLINE bool operator == (const rrDXT1Block & lhs,const rrDXT1Block & rhs)
{
	return memcmp(&lhs,&rhs,sizeof(rrDXT1Block)) == 0;
}

static RADINLINE bool operator != (const rrDXT1Block & lhs,const rrDXT1Block & rhs)
{
	return memcmp(&lhs,&rhs,sizeof(rrDXT1Block)) != 0;
}

#define RR_DXTC_ERROR_BIG	((1U<<30)-1)

// NOTE: rrDXT1PaletteMode is *not* whether the palette has alpha
//	(eg. it's not 3 color vs 4 color for the palette)
// it is also *not* telling us if the image actually has alpha or not
// what it tells us is will the client use the image's alpha channel
// so should we encode alpha correctly (preserving transparent/not-transparent)
// or can we abuse the transparent-zero 3-color mode to make black
// this is really a "care about alpha" flag
// it matches the passed in DXT1Options_Alpha flag
// 
// when rrDXT1PaletteMode_Alpha is chosen, then the transparent/not-transparent state of a pixel
//	is considered immutable; changes to it give you RR_DXTC_ERROR_BIG
// in rrDXT1PaletteMode_NoAlpha/rrDXT1PaletteMode_FourColor only RGB delta is used
//
// this is like selecting error metric RGB vs RGBA
enum rrDXT1PaletteMode
{
	rrDXT1PaletteMode_NoAlpha = 0,		// DXT1 but alpha is ignored, so transparent 0 can be used for black
	rrDXT1PaletteMode_Alpha = 1,		// DXT1 but client cares about alpha, so we cannot abuse transparent zero
	rrDXT1PaletteMode_FourColor = 2,	// Force four-color mode (for DXT3/5 and BC2/3)
};

//===============================================================================
// the simple functions :

// Sets alpha channel to all-255 (opaque)
void KillAlpha(rrColorBlock4x4 & from);

void SwapRB(rrColorBlock4x4 * pBlock);

// Lots of variants of this because we have several endpoint formats in use
void DXT1_ComputePalette(rrColor565Bits c0,rrColor565Bits c1, rrColor32BGRA palette[4], rrDXT1PaletteMode mode);
void DXT1_ComputePalette(rrColorUnpacked565 ep0,rrColorUnpacked565 ep1, rrColor32BGRA palette[4], rrDXT1PaletteMode mode);
void DXT1_ComputePalette(U32 endpoints, rrColor32BGRA palette[4], rrDXT1PaletteMode mode);

// Reference impl of DXT1_ComputePalette for testing
void DXT1_ComputePalette_Ref(rrColor565Bits c0,rrColor565Bits c1, rrColor32BGRA palette[4], rrDXT1PaletteMode mode);

// Compute many palettes from unpacked endpoints at once: endpoints[count] in, palettes[4*count] out
void DXT1_ComputePalette_Batched(const rrDXT1UnpackedEndPoints endpoints[],int count,rrColor32BGRA palettes[],rrDXT1PaletteMode mode);

void DXT1_Decompress(rrColorBlock4x4 * pTo,const rrDXT1Block & from, rrDXT1PaletteMode mode);

void BC2_CompressAlpha(U8 * pTo, const U8 from_bgra[64]); // pTo=BC2 alpha block. Source pixels can be RGBA too (don't care, only reads A)
void BC2_DecompressAlpha(U8 to_bgra[64], const U8 * pFrom); // pFrom=BC2 alpha block. Dest pixels can be RGBA too (only A channel changes)

#ifdef RR_DO_ASSERTS
// Error is SSD
// DXT1_ComputeSSD_OneBitTransparent can be used for mode = alpha or not
// DXT1_ComputeSSD_OneBitTransparent must match the error return from FindIndices
U32 DXT1_ComputeSSD_OneBitTransparent(const rrColorBlock4x4 & color,const rrDXT1Block & dxtb, rrDXT1PaletteMode mode);
#endif

// RGBA diffs ; compute palette then call BC1_Palette_SSD_RGBA.
//	colors should be previously canonicalized
U32 DXT1_ComputeSSD_RGBA(const rrColorBlock4x4 & color,const rrDXT1Block & dxtb, rrDXT1PaletteMode mode);

inline bool DXT1_Is4Color(const rrDXT1Block & dxb, rrDXT1PaletteMode mode) { if ( mode == rrDXT1PaletteMode_FourColor ) return true; return dxb.c0.w > dxb.c1.w; }
inline bool DXT1_Is4Color(const rrDXT1EndPoints & dxb, rrDXT1PaletteMode mode) { if ( mode == rrDXT1PaletteMode_FourColor ) return true; return dxb.u.c0.w > dxb.u.c1.w; }
inline bool DXT1_Is4Color(const rrDXT1UnpackedEndPoints & ep, rrDXT1PaletteMode mode) { if ( mode == rrDXT1PaletteMode_FourColor ) return true; return ep.c[0].dw > ep.c[1].dw; }
inline bool DXT1_Is4Color(U32 dw, rrDXT1PaletteMode mode) { if ( mode == rrDXT1PaletteMode_FourColor ) return true; rrDXT1EndPoints dxb; dxb.dw = dw; return dxb.u.c0.w > dxb.u.c1.w; }

//inline U32 & rrDXT1Block_EndPoints_AsU32(rrDXT1Block & dxb) { return *((U32 *)&dxb); }
inline U32 & rrDXT1Block_EndPoints_AsU32(rrDXT1Block & dxb) { return dxb.endpoints; }
	
inline bool rrDXT1Block_IsBC3Canonical(rrDXT1Block * pBlock)
{
	return pBlock->c0.w >= pBlock->c1.w;
}
inline bool rrDXT1Block_IsBC3Canonical(const rrDXT1EndPoints & dxb)
{
	return dxb.u.c0.w >= dxb.u.c1.w;
}
inline bool rrDXT1Block_IsBC3Canonical(const U32 ep)
{
	rrDXT1EndPoints dxb; dxb.dw = ep;
	return dxb.u.c0.w >= dxb.u.c1.w;
}

inline void rrDXT1Block_BC3_Canonicalize(rrDXT1Block * pBlock)
{
	// We prefer the c0.w >= c1.w ordering, which is the ordering we would pick anyway
	// for four-color blocks in DXT1.
	if ( pBlock->c0.w < pBlock->c1.w ) // clearly better
	//if ( pBlock->indices & 1 ) // clearly worse
	{
		//RR_NAMESPACE::swap(pBlock->c0,pBlock->c1);
		U16 t = pBlock->c0.w;
		pBlock->c0.w = pBlock->c1.w;
		pBlock->c1.w = t; 

		// adjust indices: 0 swaps with 1 and 2 with 3 -> toggle LSB
		// of every 2-bit pair
		pBlock->indices ^= 0x55555555u;
	}
}

//===============================================================================

// Findices fills pError with SSD

U32 DXT1_FindIndices(const rrColorBlock4x4 & block, U32 endpoints,rrDXT1PaletteMode mode, U32 * pError);
U32 DXT1_FindIndices(const rrColorBlock4x4 & block, const rrColor32BGRA palette[4], U32 * pError);

// Evaluates just the best error without determining the corresponding indices
// this is faster, but you'll need to do a final FindIndices to determine those inds
U32 DXT1_FindErrors(const rrColorBlock4x4 & block, const rrColor32BGRA palette[4]);

struct DXT1_FindErrorsContext
{
	// Error for one channel is at most 255*255;
	// A channel has 4x weight so per pixel we get at most 7*255*255 per pixel,
	// so 16*7*255*255 per block, which is below 1<<(4+3+16) = 1<<23.
	// Therefore we can rank batches of 256 blocks and stay below 1<<31,
	// which means everything can be implemented with both S32 and U32 math.
	enum { COUNT_SHIFT = 8, COUNT_LIMIT = 1 << COUNT_SHIFT };

	// Internal data
	union
	{
		RAD_ALIGN(S16, unpacked[64], 32); // used for SSE4/AVX2 versions: first 32 are B/R, last 32 are G/A
		rrColorBlock4x4 packed; // used for general fallback
	} pixels;

	// Error evaluation function pointer. This is set up by init. Returns SSD.
	U32 (*eval_palette)(const DXT1_FindErrorsContext * ctx, const rrColor32BGRA palette[4]);

	// Same as above, but for multiple palettes at once; writes the results to an array.
	// palettes has 4*count entries.
	void (*eval_palettes)(const DXT1_FindErrorsContext * ctx, U32 out_ssds[], const rrColor32BGRA palettes[], int count);

	// Evaluate multiple palettes, eval SSDs for given collection of palettes (4*count
	// entries total) and return (best_error << COUNT_SHIFT) | best_index.
	U32 (*find_best_palette)(const DXT1_FindErrorsContext * ctx, const rrColor32BGRA palettes[], int count);

	// Initializes the error context for the given pixels.
	void init(const rrColorBlock4x4 & pixels);
};

//===============================================================================

// only use if rrDXT1PaletteMode_Alpha :
U32 DXT1_OneBitTransparent_Mask(const rrColorBlock4x4 & block);
bool DXT1_OneBitTransparent_Mask_Same(U32 mask, U32 endpoints, U32 indices);
bool DXT1_OneBitTransparent_Same(const rrColorBlock4x4 & b1,const rrColorBlock4x4 & b2);
bool DXT1_OneBitTransparent_Same(const rrColorBlock4x4 & block,U32 endpoints, U32 indices);

static RADINLINE U32 DXT1_OneBitTransparent_Mask_FromIndices(bool is3c,U32 indices)
{
	if ( ! is3c ) return 0;

	U32 indices3c = indices & (indices>>1) & 0x55555555;
	return indices3c;
}

static RADINLINE bool DXT1_OneBitTransparent_Mask_Same(U32 mask,const rrDXT1Block & dxtb)
{
	return DXT1_OneBitTransparent_Mask_Same(mask,dxtb.endpoints,dxtb.indices);
}

static RADINLINE bool DXT1_OneBitTransparent_Same(const rrColorBlock4x4 & block,const rrDXT1Block & dxtb)
{
	return DXT1_OneBitTransparent_Same(block,dxtb.endpoints,dxtb.indices);
}

// assert canonicalize has been done :
bool rrColorBlock4x4_IsBC1Canonical(const rrColorBlock4x4 & colors,rrDXT1PaletteMode mode);

// do any colors in the block have an alpha < 128 ?
bool rrColorBlock4x4_HasAnyOneBitTransparent(const rrColorBlock4x4 * pBlock);

/*
inline bool rrColor32BGRA_IsOneBitTransparent(const rrColor32BGRA & color) { return color.u.a < 128; }
inline bool rrColor32RGBA_IsOneBitTransparent(const rrColor32RGBA & color) { return color.u.a < 128; }
/*/
// one bit opaque = top bit of A , A is at the top of the dw
inline bool rrColor32BGRA_IsOneBitTransparent(const rrColor32BGRA & color) { return !(color.dw>>31); }
inline bool rrColor32RGBA_IsOneBitTransparent(const rrColor32RGBA & color) { return !(color.dw>>31); }
/**/

static void RADFORCEINLINE rrColor32BGRA_MakeOneBitTransparentCanonical(rrColor32BGRA * pColor)
{
	if ( rrColor32BGRA_IsOneBitTransparent(*pColor) )
	{
		pColor->dw = 0; // BC1 transparent = black & alpha = 0
	}
	else
	{
		pColor->u.a = 255; // force full alpha
	}
}

static bool RADFORCEINLINE rrColor32BGRA_IsOneBitTransparentCanonical(const rrColor32BGRA & color)
{
	if ( rrColor32BGRA_IsOneBitTransparent(color) )
	{
		return color.dw == 0;
	}
	else
	{
		return color.u.a == 255;
	}
}


static RADINLINE bool rrColor32BGRA_1BT_Same(const rrColor32BGRA & c1,const rrColor32BGRA & c2)
{
	bool t1 = rrColor32BGRA_IsOneBitTransparent(c1);
	bool t2 = rrColor32BGRA_IsOneBitTransparent(c2);
	return t1 == t2;
}

static RADINLINE U32 rrColor32BGRA_DeltaSqrRGBA_1BT(const rrColor32BGRA & c1,const rrColor32BGRA & c2)
{
	RR_ASSERT( c1.u.a == 0 || c1.u.a == 255 );
	RR_ASSERT( c2.u.a == 0 || c2.u.a == 255 );
	U32 rgb = S32_Square((S32)c1.u.r - c2.u.r) + S32_Square((S32)c1.u.g - c2.u.g) + S32_Square((S32)c1.u.b - c2.u.b);
	// squared A delta would be 0 or 255*255
	// instead make it much bigger if not 0
	// + S32_Square((S32)c1.u.a - c2.u.a);
	U32 adelta = (c1.u.a ^ c2.u.a) << 10;
	RR_ASSERT( adelta == 0 || adelta == 255*1024 ); // bigger than 3*255*255 , max of rgb
	U32 ret = rgb + adelta;
	return ret;
}

//=========================================================================

void rrDXT1_PutBlock(U8 * outPtr,const rrDXT1Block & block);
void rrDXT1_GetBlock(const U8 * inPtr, rrDXT1Block * pBlock);

//===============================================================================

// Solves a 4-means clustering problem in RGB colors for the 16 input pixels.
//
// means is in-out; it starts with the initial estimates and gets updated
// to contain the solved means.
//
// Only RGB channels matter, values in A channel (of both source and colors)
// are ignored and not guaranteed to be initialized to anything in particular.
void DXT1_SolveRGB4Means(rrColor32BGRA means[4], const rrColorBlock4x4 & colors);

//===============================================================================

// do not call directly
namespace internal {

U32 DXT1_FindIndices_AVX2(const rrColorBlock4x4 & block, const rrColor32BGRA palette[4],U32 * pError);
U32 DXT1_FindErrors_AVX2(const rrColorBlock4x4 & block, const rrColor32BGRA palette[4]);
U32 DXT1_EvalPalette_AVX2(const DXT1_FindErrorsContext * ctx, const rrColor32BGRA palette[4]);
void DXT1_EvalPalettes_AVX2(const DXT1_FindErrorsContext * ctx, U32 out_ssds[], const rrColor32BGRA palette[4], int count);
U32 DXT1_FindBestPalette_AVX2(const DXT1_FindErrorsContext * ctx, const rrColor32BGRA palettes[], int count);

}

//===============================================================================
RR_NAMESPACE_END

#endif // __RADRRBITMAP_DXTC_BLOCK_H__
