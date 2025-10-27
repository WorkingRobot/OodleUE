// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRRBITMAP_SURFACEUTIL_H__
#define __RADRRBITMAP_SURFACEUTIL_H__

#include "rrsurface.h"
#include "rrcolor.h" // just needed for scaler
#include <math.h> // sqrt

RR_NAMESPACE_START

// pad w/h up to pow2 if not already :
//rrbool rrSurface_PadUpToPow2(rrSurface * surf);

// Range remaps come in complementary pairs (->rrRangeRemap_Inverse)
enum rrRangeRemap
{
	rrRangeRemap_None = 0,
	rrRangeRemap_UnormToSnorm, // maps [0,1] to [-1,1] linearly
	rrRangeRemap_SnormToUnorm, // maps [-1,1] to [0,1] linearly
	rrRangeRemap_LinearToSRGB, // maps [0,1] to U8 sRGB, only for 4xF32 input
	rrRangeRemap_SRGBToLinear, // maps U8 sRGB to linear 3xF32 or 4xF32 output
	rrRangeRemap_Count
};

// Determines the inverse mapping for a given range remap, if any.
rrRangeRemap rrRangeRemap_Inverse(rrRangeRemap mapping);

// Format change between two surfaces that respects the UNORM/SNORM interpretation.
// * unsigned int formats are interpreted as UNORM, signed ones as SNORM
// * unsigned pixel values are canonically mapped to [0,1], signed ones to [-1,1]
// * these are also the values ranges used when going from or to floats
//   (i.e. if you go U8->F32 using this function, you will get pixels in [0,1] not [0,255] as with ChangeFormat)
// * you can optionally apply a range remap (rrRangeRemap above)
rrbool rrSurface_ChangeFormatNormalized(rrSurface * surf, rrPixelFormat newFormat, rrRangeRemap remap = rrRangeRemap_None);

// if you do U16 to S16 with Remap_None , values in [0,65535] go to [0,32767]
//  to shift and use the full range, use Remap_UnormToSnorm
//	that does a slight scale, takes [0,65535] to [-32767,32767] (no -32768)

// same as rrSurface_ChangeFormatNormalized
//	but also accepts BCN input and decompresses it
rrbool rrSurface_ChangeFormatNormalized_AndDecomp(rrSurface * surf, rrPixelFormat newFormat, rrRangeRemap remap = rrRangeRemap_None);

// ChangeFormatNormalized : automatically remaps SRGB based on format rrPixelFormat_DefaultSRGB (which is mostly "is it 8 bit")
//	also does decomp and depal like rrSurface_ChangeFormatNormalized_AndDecomp
rrbool rrSurface_ChangeFormatNormalized_AutoSRGB(rrSurface * surf,rrPixelFormat to_fmt);

// change format in place by copying out then swapping pointers
rrbool rrSurface_ChangeFormat_NonNormalized(rrSurface * surf, rrPixelFormat newFormat);


// Blit between two surfaces that respects the UNORM/SNORM interpretation.
rrbool rrSurface_BlitNormalized(rrSurface * to,const rrSurface * from,rrRangeRemap remap = rrRangeRemap_None);

// Subrect blit that handles normalized format conversions (and optional
// range remapping) along the way.
rrbool rrSurface_BlitRect_Normalized( rrSurface * to, const rrSurface * from, 
	SINTa from_x, SINTa from_y,
	SINTa to_x  , SINTa to_y  , 
	SINTa width,  SINTa height,
	rrRangeRemap remap = rrRangeRemap_None);


// MirrorExtend : useful for padding ; dupes last row/col
void rrSurface_MirrorExtend(rrSurface * pSurf,SINTa oldW,SINTa oldH);

void rrSurface_AllocCopy_MirrorExtended(rrSurface * to,const rrSurface *from,rrPixelFormat toFmt,SINTa toW,SINTa toH);
void rrSurface_AllocCopy_MirrorExtended_Pad4(rrSurface * to,const rrSurface *from,rrPixelFormat toFmt);

// Blit F32 to F16 with clamping to F16 range
rrbool rrSurface_BlitF32toF16_Clamped(rrSurface * to_surf,const rrSurface *fm_surf);

// alpha_mode = 0/1 = force alpha off/on
// alpha_mode = anything else (2) = detect alpha
// returns bool if has alpha
rrbool rrSurface_ForceOrDetectAlpha(rrSurface * surf, int alpha_mode, rrbool verbose_logs);

// keep surf in same scalar channel type, but change format to different channel count
// note this is a Blit so it doesn't just add zero'ed channels
//	eg when adding A it gets filled with Opaque
bool rrSurface_ChangeNumberOfChannels(rrSurface * surf,int nchan);

// check if surface is a false alpha : (Normalized)
bool rrSurface_Is4ChannelWithAllAlphasOpaque(const rrSurface * surf);

bool rrSurface_HasNonIdentityAlpha(const rrSurface * surf);

// if is 4 channel, set A = Opaque (Normalized)
bool rrSurface_ForceAllAlphasOpaqueIf4Channel(rrSurface * surf);

// Down/Up simple zoom :
//	note these do not gamma correct or whatever for you, you must do it :
// note : these do not allocate dest, you must do it
void rrSurface_BoxDownSample2X(rrSurface * to,const rrSurface * from);
void rrSurface_BoxUpSample2X(rrSurface * to,const rrSurface * from);

void rrSurface_BilinearUpSample2X(rrSurface * to,const rrSurface * from);

// SRGB standard Gamma with linear portion :
void rrSurface_Gamma_SRGB_UnitLinearFloat_To_255Int(rrSurface * to,const rrSurface * from);
void rrSurface_Gamma_SRGB_255Int_To_UnitLinearFloat(rrSurface * to,const rrSurface * from);

// Gamma correct from Linear float to SRGB scale, but stay in floats so you don't lose precision
//	for error metrics that want to go to SRGB space? or sanity checks
void rrSurface_Gamma_SRGB_UnitLinearFloat_To_UnitSRGBFloat_Slow_3F(rrSurface * to,const rrSurface * from);

// note, to == from is okay :
// this is a useful way to blit from N channel to 1 channel
//	MakeGray uses a simple luma kind of thing
void rrSurface_MakeGray(rrSurface * to,const rrSurface * from);

void rrSurface_FillSolidColorF(rrSurface * to,const rrColor4F & color);
void rrSurface_FillSolidColorI(rrSurface * to,const rrColor4I & color);

// FillChannel is often used to set A to 255
void rrSurface_FillChannelF(rrSurface * surf, rrSurface_EChannel channel, F32 value );
void rrSurface_FillChannelI(rrSurface * surf, rrSurface_EChannel channel, S32 value );

// rrSurface_SelectChannels :
//	specify the channel you want to read from, eg :
//	SelectChannels(R,R,R,R);
//	replicates R in input to all channels of output
// note, to == from is okay :
void rrSurface_SelectChannels(rrSurface * to,const rrSurface * from, 
									rrSurface_EChannel to_R,
									rrSurface_EChannel to_G,
									rrSurface_EChannel to_B,
									rrSurface_EChannel to_A );
									
void rrSurface_GetMoments(const rrSurface * surf, 
				rrSurface_EChannel channel,
				F64 * pMean,
				F64 * pVariance,
				F64 * pMin,
				F64 * pMax );

/**

MSE,SSD,SAD are computed over the MIN of channels and w/h between the pair of images

eg. if one is padded to 4x4, only the non-padded pixels are compared

BC1&7 is considered to be 4 channel even if the alpha channel will be ignored
to compare without the alpha channel, the other image should be 3 channel
that is, the source being RGB vs RGBA determines whether the A channel difference is added

**/

F64 rrSurface_GetSSD(
				const rrSurface * surf1, 
				const rrSurface * surf2,
				const F32 * channel_weights = nullptr
				);

F64 rrSurface_GetSAD(
				const rrSurface * surf1,
				const rrSurface * surf2
				);

// note MSE is per *pixel* not per sample
F64 rrSurface_GetMSE(
				const rrSurface * surf1,
				const rrSurface * surf2,
				const F32 * channel_weights = 0
				);
			
static RADINLINE F64 rrSurface_GetRMSE(
				const rrSurface * surf1, 
				const rrSurface * surf2,
				const F32 * channel_weights = 0
				)
{
	F64 mse = rrSurface_GetMSE(surf1,surf2,channel_weights);
	return sqrt(mse);
}

// Normalized SSD. Always computes SSDs in normalized float space.
// Requires surface sizes to match.
F64 rrSurface_GetSSD_Normalized(
	const rrSurface * surf1,
	const rrSurface * surf2,
	const F32 * channel_weights = nullptr,
	int force_num_channels = -1
);

F64 rrSurface_GetMSE_Normalized(
	const rrSurface * surf1,
	const rrSurface * surf2,
	const F32 * channel_weights = nullptr,
	int force_num_channels = -1
);

static RADINLINE F64 rrSurface_GetRMSE_Normalized(
	const rrSurface * surf1,
	const rrSurface * surf2,
	const F32 * channel_weights = nullptr,
	int force_num_channels = -1
)
{
	F64 mse = rrSurface_GetMSE_Normalized(surf1,surf2,channel_weights,force_num_channels);
	return sqrt(mse);
}

// Like GetMSE (MSE per *pixel* not per sample), but also normalized
// to be "8-bit equivalent", i.e. scaled to match a [0,255] value scale.
// NOTE: for F32, this assumes the canonical value range in [0,1] not
// [0,255]. (Matching what ChangeFormatNormalized does.)
// NOTE: rrSurface_GetMSE8BitEquivalent only works if surf1 & 2 formats are the same
//	prefer rrSurface_RMSE_Normalized_BCNAware if they are not
F64 rrSurface_GetMSE8BitEquivalent(
				const rrSurface * surf1,
				const rrSurface * surf2,
				const F32 * channel_weights = 0
				);

//F64 rrSurface_MSE_to_PSNR(F64 mse);

// rrSurface_DoLinLog_Exposure outputs surf in unit float scale and not yet gamma corrected
//	 feed to rrSurface_Gamma_SRGB_UnitLinearFloat_To_255Int
//	or pass and_gamma_correct to output gamma corrected directly from here
void rrSurface_DoLinLog_Exposure(rrSurface * to_surf, const rrSurface * fm_surf, F32 median, F32 exposure_multiplier, bool and_gamma_correct);

F32 rrSurface_DoLinLog_Exposure_GetMedian(const rrSurface * surf);

// rrSurface_Reinhard_Unreal_Tonemap
// output values are in U8 scale [0,255]
// and already in sRGB , do NOT gamma correct
// to_surf can be BGRA8
void rrSurface_Reinhard_Unreal_Tonemap(rrSurface * to_surf,const rrSurface * fm_surf, F32 median, F32 exposure_multiplier);

// For float images: mean relative sum of squares error
// THIS IS OKAY
F64 rrSurface_GetMRSSE(
				const rrSurface * surf1,
				const rrSurface * surf2
				);
				
F64 rrSurface_GetMRSSE_Neighborhood(const rrSurface * surf1, const rrSurface * surf2,int max_channels_to_diff=3);

// Same as above, but with per-channel weighting factors
F64 rrSurface_GetWeightedMRSSE(
				const rrSurface * surf1,
				const rrSurface * surf2,
				const F32 weights[4]
				);

void rrSurface_MultAdd(rrSurface * surf, const rrColor4F & multiplier, const rrColor4F & adder );
void rrSurface_MultAddF(rrSurface * surf, const float multiplier, const float adder );
void rrSurface_MultAddF(rrSurface * dest_surf, const rrSurface * src_surf, const float multiplier, const float adder );

void rrSurface_AddScaled(rrSurface * dest, 
							const rrColor4F & scale_A,
							const rrSurface * pSurfA,
							const rrColor4F & scale_B,
							const rrSurface * pSurfB);
							
//---------------------

void rrSurface_Clamp(rrSurface * surf, float lo, float hi);
void rrSurface_Clamp255(rrSurface * surf);
// SNORM formats have two redundant encodings for -1.0, e.g.
// SNORM8 allows both -128 and -127 to mean -1.0; it makes sense to
// make SNORM be defined for all integer values in the format but
// when working with it, we'd much rather avoid the "smallest ints"
// entirely, since they complicate diffing, SSD calcs etc.
void rrSurface_ClampSNORM(rrSurface * surf);

// Quantize a 16-bit surface type to 8 bits per channel
rrbool rrSurface_QuantizeTo8Bits(rrSurface * dest, const rrSurface * source);

// Check whether the extremes (0/255 for U8, -128/127 for S8 etc.) of the pixel
// format where preserved correctly. Only supports primitive pixel formats,
// returns false for everything else.
//
// If first_mismatch_at is non-null, it points to 2 ints that receive the coordinates
// of the top-left-most mismatching pixel.
rrbool rrSurface_CheckExtremesPreserved(const rrSurface * surf1, const rrSurface * surf2, rrPixelFormat bcn_pf, int * first_mismatch_at = nullptr);

void rrSurface_GetValueRange(const rrSurface * surf, rrColor4F * pLo , rrColor4F * pHi );

void rrSurface_ClampToRange(rrSurface * surf, const rrColor4F & inlo, const rrColor4F & inhi );

/**

rrSurface_Scale_To_255 scales whatever range is in the image to [0,255]
and remembers what it did so it can be undone

**/

struct rrSurface_Scaler
{
	rrColor4F lo,hi;
};

// if uniform is true, the same scale is applied to all components
//	 pScaler is filled out with the previous color range, and can be used to undo
void rrSurface_Scale_To_255(rrSurface * surf, rrSurface_Scaler * pScaler, bool uniform );

void rrSurface_Scale_To_255_Undo(rrSurface * surf, const rrSurface_Scaler * pScaler );

//----------------------------

RR_NAMESPACE_END

#endif
