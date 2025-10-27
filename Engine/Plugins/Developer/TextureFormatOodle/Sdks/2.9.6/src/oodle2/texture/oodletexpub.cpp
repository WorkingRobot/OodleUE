// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(parent,OodleAPI_Texture)
//idoc(end)
#include "oodletexpub.h"
#include "oodletexpubrrs.h"

#include "rrsurface.h"
#include "blocksurface.h"
#include "rrsurfaceutil.h"
#include "rrsurfacedxtc.h"
#include "rrsurfaceblit.h"
#include "bc7prep.h"
#include "oodlejob.h"
#include "oodletextureversion.h"
#include "rrdxtcrd.h"
#include "layout.h"
#include "templates/rrnew.h"

#include <math.h>

/********

OodleTexPub is a call layer from public API to internal calls

OodleTex internal functions are never directly public
	(unlike Oodle Core)
	
call OodleTex_Enter() on entry from the public API to set up one time initializations

public API entry points may do arg validation, verbose level logging

========

NOTE : this file should not be compiled with SSE4
	we check for required CPU caps here

========

NOTE : public APIs that return integers can return OodleTex_Err codes as negative numbers.

client should do something like :

SINTa ret = OodleTex_BC7Prep_Decode(args);
if ( ret < 0 )
{
	OodleTex_Err err = (OodleTex_Err) ret;
	const char * err_name = OodleTex_Err_GetName(err);
}

*********/

// removed :
	//OodleTex_RDO_ErrorMetric_Perceptual_RGB_A = 3,
	//OodleTex_RDO_ErrorMetric_Perceptual_R_G_B_A = 4,
	
OODLE_NS_START

static void init_rrs_from_texsurf(rrSurface * rrs, const OodleTex_Surface * input, rrPixelFormat input_pf)
{
	rrSurface_Init(rrs);
	rrs->data = (U8 *)input->pixels;
	rrs->pixelFormat = input_pf;
	rrs->freeData = false;
	rrs->width = input->width;
	rrs->height = input->height;
	rrs->stride = input->rowStrideBytes;
}

static OodleTex_Err OodleTex_Entry_ValidateSurfaces(const OodleTex_Surface * surfaces, SINTa num_surfaces)
{
	for (SINTa i = 0; i < num_surfaces; ++i)
	{
		if ( surfaces[i].width < 1 || surfaces[i].width > OODLETEX_MAX_SURFACE_DIMENSION ||
			 surfaces[i].height < 1 || surfaces[i].height > OODLETEX_MAX_SURFACE_DIMENSION )
		{
			rrprintf("ERROR: unsupported size on surface %zd!\n"
				"Surface passed to Oodle Texture is %dx%d pixels, dimensions must be between 1 and %d.\n",
				i,
				surfaces[i].width, surfaces[i].height,
				OODLETEX_MAX_SURFACE_DIMENSION);

			return OodleTex_Err_SurfaceTooLarge;
		}
	}

	return OodleTex_Err_OK;
}

static rrDXTCOptions dxtcoptions_from_bcnflags(OodleTex_BC to_bcn, OodleTex_BCNFlags flags)
{
	U32 dxtcoptions = 0;

	if ( to_bcn == OodleTex_BC1_WithTransparency )
		dxtcoptions |= rrDXTCOptions_BC1_OneBitAlpha;

	if ( to_bcn == OodleTex_BC7RGB )
		dxtcoptions |= rrDXTCOptions_BC7_IgnoreAlpha;

	if ( (flags & OodleTex_BCNFlag_PreserveExtremes_BC345) )
		dxtcoptions |= rrDXTCOptions_BC345_PreserveExtremes;

	//if ( (flags & OodleTex_BCNFlag_IgnoreAlpha_BC7) )
	//	dxtcoptions |= rrDXTCOptions_BC7_IgnoreAlpha;

	if ( to_bcn == OodleTex_BC6S || to_bcn == OodleTex_BC6U )
	{
		// FavorLuminance is on by default
		// use NonRGBData to turn it off
		if ( ! (flags & OodleTex_BCNFlag_BC6_NonRGBData) )
			dxtcoptions |= rrDXTCOptions_BC6_FavorLuminance;
	}

	if ( flags & OodleTex_BCNFlag_AvoidWideVectors )
		dxtcoptions |= rrDXTCOptions_AvoidWideVectors;
	else if ( flags & OodleTex_BCNFlag_PreferWideVectors )
		dxtcoptions |= rrDXTCOptions_PreferWideVectors;
	
	return (rrDXTCOptions) dxtcoptions;
}

static OodleTex_Err OodleTex_Entry_ValidateLayout(const OodleTex_Layout * layout, OodleTex_BC bcn_fmt, SINTa num_blocks, const OodleTex_Surface * surfaces, SINTa num_surfaces)
{
	// BCn block size matches?
	if ( OodleTex_BC_BytesPerBlock(bcn_fmt) != layout->m_block_size )
		return OodleTex_Err_LayoutFormatMismatch;

	// NOTE: when the OodleTex_Layout is created but has not had SetBlockLayout called,
	// its m_nblocks is 0, so this also catches partially-initialized layouts.
	if ( num_blocks != layout->m_nblocks )
		return OodleTex_Err_BlockCountMismatch;

	// Number of source surfaces matches expectation?
	if ( num_surfaces != check_value_cast<SINTa>(layout->m_surfaces.size()) )
		return OodleTex_Err_SurfaceCountMismatch;

	// Check the surface sizes match our expectation
	for (SINTa i = 0; i < num_surfaces; ++i)
	{
		if ( surfaces[i].width != layout->m_surfaces[i].width ||
			 surfaces[i].height != layout->m_surfaces[i].height )
		{
			rrprintf("ERROR: size mismatch on surface %zd!\n"
				"Surface passed to Oodle Texture is %dx%d pixels, corresponding surface in layout is %dx%d pixels.\n",
				i,
				surfaces[i].width, surfaces[i].height,
				layout->m_surfaces[i].width, layout->m_surfaces[i].height);

			return OodleTex_Err_SurfaceSizeMismatch;
		}
	}

	return OodleTex_Err_OK;
}

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_LogVersion()
{
	OodleTex_Err err = OodleTex_Enter();
	
	rrprintf("Oodle Texture %s %s\n",OodleTextureVersion,RADCOPYRIGHT);
	rrprintf("Oodle Texture %s built %s %s\n",OodleTextureVersion,__DATE__,__TIME__);
	
	return err;
}

#define CASE_TO_STRING(x) case x: return #x;

OOFUNC1 const char * OOFUNC2 OodleTex_Err_GetName(OodleTex_Err error)
{
	// NOTE: intentionally no OodleTex_Enter() and early-out here
	// this is the message you call on failure of OodleTex_Enter to
	// find out what's wrong!

	switch (error)
	{
		CASE_TO_STRING(OodleTex_Err_OK)
		CASE_TO_STRING(OodleTex_Err_UnsupportedCPU)
		CASE_TO_STRING(OodleTex_Err_BadBCnFormat)
		CASE_TO_STRING(OodleTex_Err_BadPixelFormat)
		CASE_TO_STRING(OodleTex_Err_SurfaceCountMismatch)
		CASE_TO_STRING(OodleTex_Err_BlockCountMismatch)
		CASE_TO_STRING(OodleTex_Err_LayoutFormatMismatch)
		CASE_TO_STRING(OodleTex_Err_NegativeLambda)
		CASE_TO_STRING(OodleTex_Err_Internal)

		CASE_TO_STRING(OodleTex_Err_BC7PrepHeaderCorrupt)
		CASE_TO_STRING(OodleTex_Err_BC7PrepOutputBufTooSmall)
		CASE_TO_STRING(OodleTex_Err_BC7PrepScratchBufTooSmall)
		CASE_TO_STRING(OodleTex_Err_BC7PrepPayloadCorrupt)
		CASE_TO_STRING(OodleTex_Err_BC7PrepNoHeader)
		CASE_TO_STRING(OodleTex_Err_BC7PrepIllegalBlockCount)

		CASE_TO_STRING(OodleTex_Err_InvalidSurfaceIndex)
		CASE_TO_STRING(OodleTex_Err_MalformedBlockIDs)
		CASE_TO_STRING(OodleTex_Err_BadMetric)
		CASE_TO_STRING(OodleTex_Err_BadEncodeEffortLevel)
		CASE_TO_STRING(OodleTex_Err_SurfaceSizeMismatch)
		CASE_TO_STRING(OodleTex_Err_NoLicense_Unused)
		CASE_TO_STRING(OodleTex_Err_BufferTooSmall)
		CASE_TO_STRING(OodleTex_Err_SurfaceTooLarge)
		CASE_TO_STRING(OodleTex_Err_BadUniversalTiling)
		CASE_TO_STRING(OodleTex_Err_LayoutAndUniversalTilingIncompatible)
	default:
		break;
	}

	return "unknown error code!";
}

OOFUNC1 const char * OOFUNC2 OodleTex_PixelFormat_GetName(OodleTex_PixelFormat pf)
{
	switch(pf)
	{
    	CASE_TO_STRING(OodleTex_PixelFormat_Invalid)
    	CASE_TO_STRING(OodleTex_PixelFormat_4_U8_RGBA)
    	CASE_TO_STRING(OodleTex_PixelFormat_4_U8_BGRA)
    	CASE_TO_STRING(OodleTex_PixelFormat_4_F32_RGBA)
    	CASE_TO_STRING(OodleTex_PixelFormat_4_F16_RGBA)
    	CASE_TO_STRING(OodleTex_PixelFormat_3_F32_RGB)
    	CASE_TO_STRING(OodleTex_PixelFormat_3_U8_RGB)
    	CASE_TO_STRING(OodleTex_PixelFormat_3_U8_BGR)
    	CASE_TO_STRING(OodleTex_PixelFormat_2_U16)
    	CASE_TO_STRING(OodleTex_PixelFormat_2_S16)
    	CASE_TO_STRING(OodleTex_PixelFormat_2_U8)
    	CASE_TO_STRING(OodleTex_PixelFormat_2_S8)
    	CASE_TO_STRING(OodleTex_PixelFormat_1_U16)
    	CASE_TO_STRING(OodleTex_PixelFormat_1_S16)
    	CASE_TO_STRING(OodleTex_PixelFormat_1_U8)
    	CASE_TO_STRING(OodleTex_PixelFormat_1_S8)
		CASE_TO_STRING(OodleTex_PixelFormat_4_U8_RGBx)
		CASE_TO_STRING(OodleTex_PixelFormat_4_U8_BGRx)
		CASE_TO_STRING(OodleTex_PixelFormat_4_U16)
		CASE_TO_STRING(OodleTex_PixelFormat_3_U16)
		CASE_TO_STRING(OodleTex_PixelFormat_2_F32)
		CASE_TO_STRING(OodleTex_PixelFormat_1_F32)
	default:
		break;
	}

	return "unknown PixelFormat!";
}

OOFUNC1 const char * OOFUNC2 OodleTex_BC_GetName(OodleTex_BC bcn)
{
	switch(bcn)
	{
		CASE_TO_STRING(OodleTex_BC_Invalid)
		CASE_TO_STRING(OodleTex_BC1)
		CASE_TO_STRING(OodleTex_BC1_WithTransparency)
		CASE_TO_STRING(OodleTex_BC2)
		CASE_TO_STRING(OodleTex_BC3)
		CASE_TO_STRING(OodleTex_BC4U)
		CASE_TO_STRING(OodleTex_BC4S)
		CASE_TO_STRING(OodleTex_BC5U)
		CASE_TO_STRING(OodleTex_BC5S)
		CASE_TO_STRING(OodleTex_BC6U)
		CASE_TO_STRING(OodleTex_BC6S)
		CASE_TO_STRING(OodleTex_BC7RGBA)
		CASE_TO_STRING(OodleTex_BC7RGB)
	default:
		break;
	}

	return "unknown OodleTex_BC!";
}

OOFUNC1 const char * OOFUNC2 OodleTex_RDO_UniversalTiling_GetName(OodleTex_RDO_UniversalTiling tiling)
{
	switch(tiling)
	{
		CASE_TO_STRING(OodleTex_RDO_UniversalTiling_Disable)
		CASE_TO_STRING(OodleTex_RDO_UniversalTiling_256KB)
		CASE_TO_STRING(OodleTex_RDO_UniversalTiling_64KB)
	default:
		break;
	}

	return "unknown OodleTex_RDO_UniversalTiling!";
}

OOFUNC1 S32 OOFUNC2 OodleTex_BC_BytesPerBlock(OodleTex_BC bcn)
{
	// @@ this could just be a switch returning constants

	rrPixelFormat pf = OodleTex_BC_to_rrPixelFormat(bcn);
	if ( pf == rrPixelFormat_Invalid )
		return OodleTex_Err_BadBCnFormat;

	const rrPixelFormatInfo * info = rrPixelFormat_GetInfo(pf);
	
	return info->bytesPerBlock;
}

OOFUNC1 S32 OOFUNC2 OodleTex_PixelFormat_BytesPerPixel(OodleTex_PixelFormat from_format)
{
	// @@ this could just be a switch returning constants

	rrPixelFormat pf = OodleTex_PixelFormat_to_rrPixelFormat(from_format);
	if ( pf == rrPixelFormat_Invalid )
		return OodleTex_Err_BadPixelFormat;

	const rrPixelFormatInfo * info = rrPixelFormat_GetInfo(pf);
	
	return info->bytesPerPixel;
}

// internal
rrPixelFormat OodleTex_PixelFormat_to_rrPixelFormat(OodleTex_PixelFormat pf)
{
	switch(pf)
	{
    case OodleTex_PixelFormat_4_U8_RGBA:	return rrPixelFormat_R8G8B8A8; // NOT rrPixelFormat_4_U8
    case OodleTex_PixelFormat_4_U8_BGRA:	return rrPixelFormat_B8G8R8A8;
    case OodleTex_PixelFormat_4_F32_RGBA:	return rrPixelFormat_4_F32; // rrPixelFormat_4_F32 is rrColor4F which is RGBA order
	case OodleTex_PixelFormat_4_F16_RGBA:	return rrPixelFormat_4_F16; 
    case OodleTex_PixelFormat_3_F32_RGB:	return rrPixelFormat_3_F32; 
    case OodleTex_PixelFormat_3_U8_RGB:		return rrPixelFormat_R8G8B8;
	case OodleTex_PixelFormat_3_U8_BGR:		return rrPixelFormat_B8G8R8;
    case OodleTex_PixelFormat_2_U16:		return rrPixelFormat_2_U16;
    case OodleTex_PixelFormat_2_S16:		return rrPixelFormat_2_S16;
    case OodleTex_PixelFormat_2_U8: 		return rrPixelFormat_2_U8;
    case OodleTex_PixelFormat_2_S8: 		return rrPixelFormat_2_S8;
    case OodleTex_PixelFormat_1_U16:		return rrPixelFormat_1_U16;
    case OodleTex_PixelFormat_1_S16:		return rrPixelFormat_1_S16;
    case OodleTex_PixelFormat_1_U8:			return rrPixelFormat_1_U8;
    case OodleTex_PixelFormat_1_S8:			return rrPixelFormat_1_S8;
	case OodleTex_PixelFormat_4_U8_RGBx:	return rrPixelFormat_R8G8B8x8;
	case OodleTex_PixelFormat_4_U8_BGRx:	return rrPixelFormat_B8G8R8x8;
	case OodleTex_PixelFormat_4_U16:		return rrPixelFormat_4_U16;
	case OodleTex_PixelFormat_3_U16:		return rrPixelFormat_3_U16;
	case OodleTex_PixelFormat_2_F32:		return rrPixelFormat_2_F32;
	case OodleTex_PixelFormat_1_F32:		return rrPixelFormat_1_F32;
    default:
		RR_ASSERT_FAILURE("OodleTex_PixelFormat Invalid");
		return rrPixelFormat_Invalid;
	}
}

// internal
rrPixelFormat OodleTex_BC_to_rrPixelFormat(OodleTex_BC pf)
{
	switch(pf)
	{
	case OodleTex_BC1:
	case OodleTex_BC1_WithTransparency:	return rrPixelFormat_BC1;
	case OodleTex_BC2:  return rrPixelFormat_BC2;
	case OodleTex_BC3:  return rrPixelFormat_BC3;
	case OodleTex_BC4U: return rrPixelFormat_BC4U;
	case OodleTex_BC4S: return rrPixelFormat_BC4S;
	case OodleTex_BC5U: return rrPixelFormat_BC5U;
	case OodleTex_BC5S: return rrPixelFormat_BC5S;
	case OodleTex_BC6U: return rrPixelFormat_BC6U;
	case OodleTex_BC6S: return rrPixelFormat_BC6S;
	case OodleTex_BC7RGBA:  return rrPixelFormat_BC7;
	case OodleTex_BC7RGB: return rrPixelFormat_BC7;
    default:
		RR_ASSERT_FAILURE("OodleTex_BC Invalid");
		return rrPixelFormat_Invalid;
	}
}

//invert OodleTex_PixelFormat_to_rrPixelFormat
//	NOTE doesn't match rrPixelFormat_4_U8
OodleTex_PixelFormat rrPixelFormat_to_OodleTex_PixelFormat(rrPixelFormat pf)
{
	// OodleTex_PixelFormat starts at 1
	for (int i=1;i<=OodleTex_PixelFormat_Max;i++)
	{
		OodleTex_PixelFormat otpf = (OodleTex_PixelFormat) i;
		rrPixelFormat rrpf = OodleTex_PixelFormat_to_rrPixelFormat(otpf);
		if ( rrpf == pf )
			return otpf;
	}
	return OodleTex_PixelFormat_Invalid;
}

OodleTex_BC rrPixelFormat_to_OodleTex_BC(rrPixelFormat pf)
{
	// OodleTex_PixelFormat starts at 1
	for (int i=1;i<=OodleTex_BC_Max;i++)
	{
		OodleTex_BC otpf = (OodleTex_BC) i;
		rrPixelFormat rrpf = OodleTex_BC_to_rrPixelFormat(otpf);
		if ( rrpf == pf )
			return otpf;
	}
	return OodleTex_BC_Invalid;
}

OOFUNC1 OodleTex_PixelFormat OOFUNC2 OodleTex_BC_GetNaturalDecodeFormat(OodleTex_BC bcn)
{
	// matches rrSurfaceDXTC_GetBCNDecompFormat
	//	just convert the enums

	rrPixelFormat to_pf = OodleTex_BC_to_rrPixelFormat(bcn);
	if ( to_pf == rrPixelFormat_Invalid ) return OodleTex_PixelFormat_Invalid;
	rrPixelFormat desired_fm_fmt = rrSurfaceDXTC_GetBCNDecompFormat(to_pf);
	OodleTex_PixelFormat ot_pf = rrPixelFormat_to_OodleTex_PixelFormat(desired_fm_fmt);
	RR_ASSERT( ot_pf != OodleTex_PixelFormat_Invalid );
	return ot_pf;
}

static const rrPixelFormatInfo * get_format_info(OodleTex_PixelFormat from_format)
{
	rrPixelFormat pf = OodleTex_PixelFormat_to_rrPixelFormat(from_format);
	if ( pf == rrPixelFormat_Invalid )
		return NULL;

	return rrPixelFormat_GetInfo(pf);
}

static bool is_signed_bc45(OodleTex_BC bcn)
{
	return (bcn == OodleTex_BC4S || bcn == OodleTex_BC5S);
}

static bool is_bc6(OodleTex_BC bcn)
{
	return (bcn == OodleTex_BC6U || bcn == OodleTex_BC6S);
}

static OodleTex_Err OodleTex_Entry_ValidateFormats(
	OodleTex_BC to_bcn,OodleTex_PixelFormat from_format)
{
	// check requested formats are allowed
	const rrPixelFormatInfo * info = get_format_info(from_format);

	if ( ! info )
		return OodleTex_Err_BadPixelFormat;
	
	if ( info->isFloat && ! is_bc6(to_bcn) )
	{
		// source image is float
		// requested BCN is not BC6
		// return error
		// require client to do the float->int
		// so it's not ambiguous for us
		
		return OodleTex_Err_BadPixelFormat;
	}

	// signed integer input formats are required for BC4S/BC5S input
	// and not allowed for anything else
	bool is_signed_int = info->isSigned && !info->isFloat;
	if ( is_signed_int != is_signed_bc45(to_bcn) )
	{
		return OodleTex_Err_BadPixelFormat;
	}

	return OodleTex_Err_OK;
}

static bool check_buf_size(void * buf, SINTa required_size)
{
	#ifdef _MSC_VER // need __try/__except
	__try
	{ 
		// check that compBuf is big enough :
		U8 * buf8 = U8_void(buf);
		volatile U8 t;
		t = buf8[0];
		t = buf8[required_size-1];
	}
	__except(1) // EXCEPTION_EXECUTE_HANDLER)
	{
		return false;
	}
	#endif

	return true;
}

static bool OodleTex_EncodeEffortLevel_to_rrDXTCLevel(rrDXTCLevel * pdxtc_level,OodleTex_EncodeEffortLevel effort_level,rrDXTCLevel default_level)
{
	rrDXTCLevel dxtc_level;

	switch((int)effort_level)
	{
	// secret levels :
    case 1:
		dxtc_level = rrDXTCLevel_VeryFast;
		break;
    case 99:
		dxtc_level = rrDXTCLevel_Reference;
		break;
    case OodleTex_EncodeEffortLevel_Low:
		dxtc_level = rrDXTCLevel_Fast;
		break;
    case OodleTex_EncodeEffortLevel_Normal:
		dxtc_level = rrDXTCLevel_Slow;
		break;
    case OodleTex_EncodeEffortLevel_High:
		dxtc_level = rrDXTCLevel_VerySlow;
		break;
    case OodleTex_EncodeEffortLevel_Default:
		dxtc_level = default_level;
		break;
	default:
		// bad value
		return false;
	}

	*pdxtc_level = dxtc_level;

	return true;
}

static int OodleTex_NumJobThreads_to_NumWorkers(int num_job_threads)
{
	if ( num_job_threads == OODLETEX_JOBS_DEFAULT )
		return OODLEJOB_DEFAULT;

	if ( num_job_threads == OODLETEX_JOBS_DISABLE )
		return OODLEJOB_DISABLE;

	return num_job_threads;
}
	
OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_EncodeBCN_Blocks(
	OodleTex_BC to_bcn,void * to_bcn_blocks,SINTa num_blocks,
	const void * from_pixel_blocks,OodleTex_PixelFormat from_format,
	OodleTex_EncodeEffortLevel effort_level,OodleTex_BCNFlags flags,
	int num_job_threads,void * jobify_user_ptr)
{
	OodleTex_Err err = OodleTex_Enter();
	if ( err != OodleTex_Err_OK )
		return err;

	err = OodleTex_Entry_ValidateFormats(to_bcn,from_format);
	if ( err != OodleTex_Err_OK )
		return err;

	rrPixelFormat to_pf = OodleTex_BC_to_rrPixelFormat(to_bcn);
	if ( to_pf == rrPixelFormat_Invalid )
		return OodleTex_Err_BadBCnFormat;
	rrPixelFormat fm_pf = OodleTex_PixelFormat_to_rrPixelFormat(from_format);
	if ( fm_pf == rrPixelFormat_Invalid )
		return OodleTex_Err_BadPixelFormat;

	// zero init of rrSurface struct has freeData = false; it's a point-at
	FPStateScope save_state;
	BlockSurface to_surf = { };
	BlockSurface_SetView(&to_surf,to_bcn_blocks,num_blocks,to_pf);

	BlockSurface fm_surf = { };
	BlockSurface_SetView(&fm_surf,(void *)from_pixel_blocks,num_blocks,fm_pf);
	
	if ( ! check_buf_size(to_surf.blocks,BlockSurface_GetDataSizeBytes(&to_surf)) )
		return OodleTex_Err_BufferTooSmall;
	if ( ! check_buf_size(fm_surf.blocks,BlockSurface_GetDataSizeBytes(&fm_surf)) )
		return OodleTex_Err_BufferTooSmall;

	if ( num_job_threads == OODLETEX_JOBS_DEFAULT )
	{
		// num_job_threads not given
		num_job_threads = OodlePlugins_GetJobTargetParallelism();
	}
	else
	{
		num_job_threads = OodleTex_NumJobThreads_to_NumWorkers(num_job_threads);
	}
	
	rrDXTCOptions dxtcoptions = dxtcoptions_from_bcnflags(to_bcn,flags);
	
	rrDXTCLevel dxtc_level;
	if ( ! OodleTex_EncodeEffortLevel_to_rrDXTCLevel(&dxtc_level,effort_level,rrDXTCLevel_VerySlow) )
	{
		return OodleTex_Err_BadEncodeEffortLevel;
	}
	
	rrbool ok = rrSurfaceDXTC_CompressBCN_Blocks(&to_surf,&fm_surf,dxtc_level,num_job_threads,dxtcoptions,jobify_user_ptr);
	if ( !ok )
		err = OodleTex_Err_Internal;

	// make sure nobody changed to surf :
	RR_ASSERT( to_surf.blocks == (U8 *) to_bcn_blocks );
	RR_ASSERT( fm_surf.blocks == (U8 *) from_pixel_blocks );
	
	return err;
}



OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_EncodeBCN_LinearSurfaces(
	OodleTex_BC to_bcn,void * to_bcn_blocks,SINTa num_blocks,
	const OodleTex_Surface * from_surfaces,SINTa num_from_surfaces,OodleTex_PixelFormat from_format,
	const OodleTex_Layout * layout,
	OodleTex_EncodeEffortLevel level,OodleTex_BCNFlags flags,
	int num_job_threads,void * jobify_user_ptr)
{
	OodleTex_Err err = OodleTex_Enter();
	if ( err != OodleTex_Err_OK )
		return err;

	err = OodleTex_Entry_ValidateFormats(to_bcn,from_format);
	if ( err != OodleTex_Err_OK )
		return err;
		
	err = OodleTex_Entry_ValidateSurfaces(from_surfaces,num_from_surfaces);
	if ( err != OodleTex_Err_OK )
		return err;

	rrPixelFormat fm_pf = OodleTex_PixelFormat_to_rrPixelFormat(from_format);
	if ( fm_pf == rrPixelFormat_Invalid )
		return OodleTex_Err_BadPixelFormat;

	FPStateScope save_state;
	BlockSurfaceObj from_blocks;
	
	if ( layout )
	{
		err = OodleTex_Entry_ValidateLayout(layout,to_bcn,num_blocks,from_surfaces,num_from_surfaces);
		if ( err != OodleTex_Err_OK )
			return err;
	}
	
	rrSurface from_rrs_one = { };
	rrSurface * from_rrs = &from_rrs_one;
	if ( num_from_surfaces > 1 )
		from_rrs = OODLE_MALLOC_ARRAY(rrSurface,num_from_surfaces);
	
	for (SINTa i = 0; i < num_from_surfaces; ++i)
		init_rrs_from_texsurf(from_rrs + i, from_surfaces + i, fm_pf);

	if ( layout == NULL )
	{
		int from_num_blocks = TotalBlockCount(from_rrs,(int)num_from_surfaces);

		if ( num_blocks != from_num_blocks )
			return OodleTex_Err_BlockCountMismatch;
	}

	BlockSurface_AllocCopy_from_RRS_Layout(&from_blocks,from_rrs,(int)num_from_surfaces,layout);

	if ( from_rrs != &from_rrs_one )
		OODLE_FREE_ARRAY(from_rrs,num_from_surfaces);

	err = OodleTex_EncodeBCN_Blocks(to_bcn,to_bcn_blocks,num_blocks,
		from_blocks.blocks,from_format,level,flags,num_job_threads,jobify_user_ptr);

	return err;
}

#define RDO_MIN_BLOCK_COUNT	16
// added 05/23/2020 :
// tiny images with blocks fewer than RDO_MIN_BLOCK_COUNT don't get RDO
//	 this is not because it's broken
//	 but just because it hasn't been well tested
//	and probably won't do anything helpful
// note that direct callers to rrdxtcrd (internal tests) do not get this exclusion,
//	they still RDO tiny images to make sure it doesn't fail
// 16 blocks = a 16x16 image

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_EncodeBCN_RDO_Ex(
	OodleTex_BC to_bcn,void * to_bcn_blocks,SINTa num_blocks,
	const OodleTex_Surface * from_surfaces,SINTa num_from_surfaces,OodleTex_PixelFormat from_format,
	const OodleTex_Layout * layout,
	int rdo_lagrange_lambda,
	const OodleTex_RDO_Options * options,
	int num_job_threads,void * jobify_user_ptr)
{
	OodleTex_Err err = OodleTex_Enter();
	if ( err != OodleTex_Err_OK )
		return err;

	static OodleTex_RDO_Options zero_options = { };
	if ( options == NULL ) options = &zero_options;

	const OodleTex_RDO_ErrorMetric rdo_metric = options->metric;
	// effort: handled below
	const OodleTex_BCNFlags flags = options->bcn_flags;
	// rdo_flags: nothing currently defined

	// set up rdopts
	rrDXTCRD_Options rdopts = { };
	rrDXTCRD_SetDefaults(&rdopts);
	
	rdopts.use_bc3_alpha_lambda = options->use_bc3_alpha_lambda;
	rdopts.bc3_alpha_lambda = options->bc3_alpha_lambda;

	// As of 2.9.1, default for RDO is Slow (=what we call Normal in the public API)
	// not VerySlow
	if ( ! OodleTex_EncodeEffortLevel_to_rrDXTCLevel(&rdopts.effort,options->effort,rrDXTCLevel_Slow) )
	{
		return OodleTex_Err_BadEncodeEffortLevel;
	}

	err = OodleTex_Entry_ValidateFormats(to_bcn,from_format);
	if ( err != OodleTex_Err_OK )
		return err;
		
	err = OodleTex_Entry_ValidateSurfaces(from_surfaces,num_from_surfaces);
	if ( err != OodleTex_Err_OK )
		return err;

	rrPixelFormat to_pf = OodleTex_BC_to_rrPixelFormat(to_bcn);
	if ( to_pf == rrPixelFormat_Invalid )
		return OodleTex_Err_BadBCnFormat;
	rrPixelFormat fm_pf = OodleTex_PixelFormat_to_rrPixelFormat(from_format);
	if ( fm_pf == rrPixelFormat_Invalid )
		return OodleTex_Err_BadPixelFormat;

	if ( rdo_lagrange_lambda < 0 )
		return OodleTex_Err_NegativeLambda;
		
	if ( num_job_threads == OODLETEX_JOBS_DEFAULT )
	{
		num_job_threads = OodlePlugins_GetJobTargetParallelism();
	}
	else
	{
		num_job_threads = OodleTex_NumJobThreads_to_NumWorkers(num_job_threads);
	}

	// Determine universal tiling parameters
	OodleTex_Layout universal_layout(rrPixelFormat_GetInfo(to_pf)->bytesPerBlock);
	const OodleTex_Layout * use_layout = layout;
	bool universal_tiling = false;

	if ( options->universal_tiling != OodleTex_RDO_UniversalTiling_Disable )
	{
		// Layout and universal tiling are mutually exclusive
		if ( layout )
			return OodleTex_Err_LayoutAndUniversalTilingIncompatible;

		err = universal_layout.InitUniversal(options->universal_tiling);
		if ( err != OodleTex_Err_OK )
			return err;

		// If all passed-in surfaces are small enough to fit inside a single tile, universal tiling is a NOP, so
		// turn it off
		bool all_small = true;
		for (SINTa i = 0; i < num_from_surfaces; i++)
		{
			if ( from_surfaces[i].width > universal_layout.m_tile_w ||
				 from_surfaces[i].height > universal_layout.m_tile_h )
			{
				all_small = false;
				break;
			}
		}

		if ( !all_small )
		{
			use_layout = &universal_layout;
			universal_tiling = true;
		}
	}
		
	// zero init of rrSurface struct has freeData = false; it's a point-at

	FPStateScope save_state;
	BlockSurface to_surf = { };
	BlockSurface_SetView(&to_surf,to_bcn_blocks,num_blocks,to_pf);

	if ( ! check_buf_size(to_surf.blocks,BlockSurface_GetDataSizeBytes(&to_surf)) )
		return OodleTex_Err_BufferTooSmall;

	// allocate & blit into from_blocks , either from a single linear surface or from a layout
	BlockSurfaceObj from_blocks;
	rrSurface from_rrs_local;
	rrSurface * from_rrs = &from_rrs_local;
	
	if ( num_from_surfaces != 1 )
		from_rrs = OODLE_MALLOC_ARRAY(rrSurface,num_from_surfaces);

	if ( layout )
	{
		err = OodleTex_Entry_ValidateLayout(layout,to_bcn,num_blocks,from_surfaces,num_from_surfaces);
		if ( err != OodleTex_Err_OK )
			return err;
	}

	SINTa from_num_blocks = 0;
	for (SINTa i = 0; i < num_from_surfaces; ++i)
	{
		init_rrs_from_texsurf(from_rrs + i, from_surfaces + i, fm_pf);
		from_num_blocks += OodleTex_Surface_NumBlocks(from_surfaces + i);
	}

	// layout can have more or fewer blocks than from set, that's up to them
	// null layout is tight packed N surfaces
	if ( !layout && num_blocks != from_num_blocks )
		return OodleTex_Err_BlockCountMismatch;

	BlockSurface_AllocCopy_from_RRS_Layout(&from_blocks,from_rrs,(int)num_from_surfaces,use_layout);

	// Target surface to encode to; usually to_surf, but we encode to temp when universal tiling is on,
	// because we need to un-tile the result
	BlockSurface encode_to_surf = { };
	if ( ! universal_tiling ) // no universal tiling -> straight to output
	{
		// this is checked and returned error code above, should be true now :
		RR_ASSERT( from_blocks.count == num_blocks );
		encode_to_surf = to_surf;
	}
	else
	{
		// NOTE(fg): _not_ checking the from_blocks.count for equality since in the universal
		// tiling case, we will add padding if the texture contains one or more partially
		// filled tiles, so instead check that it's large enough to contain all the source
		// blocks, and allocate our dest surface to match:
		RR_ASSERT( from_blocks.count >= num_blocks );
		BlockSurface_Alloc(&encode_to_surf,from_blocks.count,to_pf);

		// NOTE(fg): the code below uses num_blocks (actual number of valid blocks) to check
		// whether RDO makes sense (which is fine) but the actual work functions use the count
		// fields in the passed BlockSurfaces, which is the correct number to use for that
		// purpose.
	}
		
	rrDXTCOptions dxtcoptions = dxtcoptions_from_bcnflags(to_bcn,flags);

	if ( rdo_lagrange_lambda >= 1 &&
		num_blocks >= RDO_MIN_BLOCK_COUNT )
	{
		// yes do RDO
		
		EDXTCRD_Metric dxtc_metric = eDXTCRD_Metric_Perceptual_RGBA;
		switch(rdo_metric)
		{
		case OodleTex_RDO_ErrorMetric_Default: // default is RGBA
		case OodleTex_RDO_ErrorMetric_Perceptual_RGBA:
			dxtc_metric = eDXTCRD_Metric_Perceptual_RGBA;
			break;
		/*
		case OodleTex_RDO_ErrorMetric_Perceptual_RGB_A:
			dxtc_metric = eDXTCRD_Metric_Perceptual_RGB_A;
			break;
		case OodleTex_RDO_ErrorMetric_Perceptual_R_G_B_A:
			dxtc_metric = eDXTCRD_Metric_Perceptual_R_G_B_A;
			break;
		*/
		case OodleTex_RDO_ErrorMetric_RMSE_RGBA:
			dxtc_metric = eDXTCRD_Metric_RMSE_RGBA;
			break;
		default:
			err = OodleTex_Err_BadMetric; // don't return here, we have stuff that needs to get freed
			break;
		}

		if ( err == OodleTex_Err_OK &&
			! rrDXTCRD_Encode_RDO(&encode_to_surf,&from_blocks,from_rrs,(int)num_from_surfaces,use_layout,rdo_lagrange_lambda,dxtcoptions,jobify_user_ptr,num_job_threads,dxtc_metric,rdopts) )
		{
			ooLogError("rrDXTCRD_Encode_RDO failed!");
			err = OodleTex_Err_Internal;	
		}
	}
	else
	{
		// just run the non-RDO encoder
		// also run rdo_lagrange_lambda == 0 through here
		// and num_blocks < RDO_MIN_BLOCK_COUNT
		
		if ( ! rrSurfaceDXTC_CompressBCN_Blocks(&encode_to_surf,&from_blocks,rdopts.effort,num_job_threads,dxtcoptions,jobify_user_ptr) )
		{
			ooLogError("rrSurfaceDXTC_CompressBCN_Blocks failed!");
			err = OodleTex_Err_Internal;	
		}
	}

	// If we did universal tiling, we need to turn the results back into linear order
	if ( universal_tiling )
	{
		BlockSurface_Copy_Detile(&to_surf,&encode_to_surf,from_rrs,(int)num_from_surfaces,use_layout->m_tile_w,use_layout->m_tile_h);
		BlockSurface_Free(&encode_to_surf);
	}

	if ( from_rrs != &from_rrs_local )
		OODLE_FREE_ARRAY(from_rrs,num_from_surfaces);

	// make sure nobody changed to surf :
	RR_ASSERT( to_surf.blocks == (U8 *) to_bcn_blocks );
	
	return err;
}

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_EncodeBCN_RDO(
	OodleTex_BC to_bcn,void * to_bcn_blocks,SINTa num_blocks,
	const OodleTex_Surface * from_surfaces,SINTa num_from_surfaces,OodleTex_PixelFormat from_format,
	const OodleTex_Layout * layout,
	int rdo_lagrange_lambda,OodleTex_BCNFlags flags,
	OodleTex_RDO_ErrorMetric rdo_metric,
	int num_job_threads,void * jobify_user_ptr)
{
	// Call into _Ex with equivalent options
	OodleTex_RDO_Options opts = { };

	opts.metric = rdo_metric;
	opts.effort = OodleTex_EncodeEffortLevel_High; // matches previous default
	opts.bcn_flags = flags;
	opts.rdo_flags = OodleTex_RDO_Flags_None;

	return OodleTex_EncodeBCN_RDO_Ex(
		to_bcn,to_bcn_blocks,num_blocks,
		from_surfaces,num_from_surfaces,from_format,
		layout,
		rdo_lagrange_lambda,
		&opts,
		num_job_threads,jobify_user_ptr
	);
}

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_DecodeBCN_Blocks(
	void * to_pixel_blocks,OodleTex_PixelFormat to_format,SINTa num_blocks,
	OodleTex_BC from_bcn,const void * from_bcn_blocks
	)
{
	OodleTex_Err err = OodleTex_Enter();
	if ( err != OodleTex_Err_OK )
		return err;

	rrPixelFormat to_pf = OodleTex_PixelFormat_to_rrPixelFormat(to_format);
	if ( to_pf == rrPixelFormat_Invalid )
		return OodleTex_Err_BadPixelFormat;
	rrPixelFormat fm_pf = OodleTex_BC_to_rrPixelFormat(from_bcn);
	if ( fm_pf == rrPixelFormat_Invalid )
		return OodleTex_Err_BadBCnFormat;
		
	FPStateScope save_state;
	BlockSurface from_bs = { };
	BlockSurface_SetView(&from_bs,(void *)from_bcn_blocks,num_blocks,fm_pf);
	
	BlockSurface to_bs = { };
	BlockSurface_SetView(&to_bs,(void *)to_pixel_blocks,num_blocks,to_pf);
	
	BlockSurfaceObj to_bs_decomp;
	BlockSurface_SetView(&to_bs_decomp,&to_bs);
	
	// will alloc decomp :
	//   hinted to produce to_pf data. decoders will try but no guarantees.
	rrbool ok = BlockSurface_DecompressBCN(&to_bs_decomp,&from_bs,to_pf);
	if ( ! ok )
		return OodleTex_Err_Internal;
	
	if ( to_bs_decomp.blocks != to_bs.blocks )
	{
		// DecompressBCN allocated dest, didn't write where we wanted
		
		// must have been due to a pixel format difference :
		RR_ASSERT( to_bs.pixelFormat != to_bs_decomp.pixelFormat );
		
		// verify that to_bs.pixelFormat is not the natural dest format
		//	(we shouldn't be taking the conversion path in that case)
		// meh this can easily happen just for crap like RGBA vs BGRA
		// RR_ASSERT( to_format != OodleTex_BC_GetNaturalDecodeFormat(from_bcn) );
		
		// do a format changing blit :
		ok = BlockSurface_BlitNormalized(&to_bs,&to_bs_decomp);
		if ( ! ok )
			return OodleTex_Err_Internal;
	}

	return err;
}
	
OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_DecodeBCN_LinearSurfaces(
	OodleTex_Surface * to_surfaces,SINTa num_to_surfaces,OodleTex_PixelFormat to_format,
	OodleTex_BC from_bcn,const void * from_bcn_blocks,SINTa num_blocks,
	const OodleTex_Layout * layout
	)
{
	OodleTex_Err err = OodleTex_Enter();
	if ( err != OodleTex_Err_OK )
		return err;

	err = OodleTex_Entry_ValidateSurfaces(to_surfaces,num_to_surfaces);
	if ( err != OodleTex_Err_OK )
		return err;

	rrPixelFormat to_pf = OodleTex_PixelFormat_to_rrPixelFormat(to_format);
	if ( to_pf == rrPixelFormat_Invalid )
		return OodleTex_Err_BadPixelFormat;
	rrPixelFormat fm_pf = OodleTex_BC_to_rrPixelFormat(from_bcn);
	if ( fm_pf == rrPixelFormat_Invalid )
		return OodleTex_Err_BadBCnFormat;

	// Do input validation up front before we start decoding anything
	if ( layout )
	{
		err = OodleTex_Entry_ValidateLayout(layout,from_bcn,num_blocks,to_surfaces,num_to_surfaces);
		if ( err != OodleTex_Err_OK )
			return err;
	}
	
	if ( layout == NULL )
	{
		SINTa to_num_blocks = 0;
		for (SINTa i = 0; i < num_to_surfaces; ++i)
		{
			to_num_blocks += OodleTex_Surface_NumBlocks(to_surfaces+i);
		}

		if ( num_blocks != to_num_blocks )
			return OodleTex_Err_BlockCountMismatch;
	}

	FPStateScope save_state;
	BlockSurface from_bs = { };
	BlockSurface_SetView(&from_bs,(void *)from_bcn_blocks,num_blocks,fm_pf);

	// will alloc decomp :
	//   hinted to produce to_pf data. decoders will try but no guarantees.
	BlockSurfaceObj decomp;
	rrbool ok = BlockSurface_DecompressBCN(&decomp,&from_bs,to_pf);
	if ( ! ok )
		return OodleTex_Err_Internal;

	// change format if decomp.format != to->format
	BlockSurfaceObj decomp_to_format;
	BlockSurface_AllocCopyOrSetViewIfFormatMatches_Normalized(&decomp_to_format,&decomp,to_pf);

	rrSurface to_rrs_one = { };
	rrSurface * to_rrs = &to_rrs_one;
	if ( num_to_surfaces > 1 )
		to_rrs = OODLE_MALLOC_ARRAY(rrSurface,num_to_surfaces);

	for (SINTa i = 0; i < num_to_surfaces; ++i)
		init_rrs_from_texsurf(to_rrs + i, to_surfaces + i, to_pf);

	BlockSurface_Copy_to_RRS_SameFormat_Layout(to_rrs,(int)num_to_surfaces,&decomp_to_format,layout);
	
	if ( to_rrs != &to_rrs_one )
		OODLE_FREE_ARRAY(to_rrs,num_to_surfaces);

	return OodleTex_Err_OK;
}

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_Layout_CopyBCNToLinear(
	OodleTex_BC bcn,
	OodleTex_Surface * to_surfaces,SINTa num_to_surfaces,
	const void * from_bcn_blocks,SINTa num_blocks,
	const OodleTex_Layout * layout
	)
{
	// to_surfaces will also be of "bcn"
	//	width & height are pixels
	//	to_surfaces.rowStrideBytes is for a row of *blocks*

	OodleTex_Err err = OodleTex_Enter();
	if ( err != OodleTex_Err_OK )
		return err;

	err = OodleTex_Entry_ValidateSurfaces(to_surfaces,num_to_surfaces);
	if ( err != OodleTex_Err_OK )
		return err;

	rrPixelFormat bcn_pf = OodleTex_BC_to_rrPixelFormat(bcn);
	if ( bcn_pf == rrPixelFormat_Invalid )
		return OodleTex_Err_BadBCnFormat;

	// Do input validation up front before we start decoding anything
	err = OodleTex_Entry_ValidateLayout(layout,bcn,num_blocks,to_surfaces,num_to_surfaces);
	if ( err != OodleTex_Err_OK )
		return err;

	BlockSurface from_bs = { };
	BlockSurface_SetView(&from_bs,(void *)from_bcn_blocks,num_blocks,bcn_pf);

	rrSurface * to_rrs = OODLE_MALLOC_ARRAY(rrSurface,num_to_surfaces);
	for (SINTa i = 0; i < num_to_surfaces; ++i)
		init_rrs_from_texsurf(to_rrs + i, to_surfaces + i, bcn_pf);

	BlockSurface_Copy_to_RRS_SameFormat_Layout(to_rrs,(int)num_to_surfaces,&from_bs,layout);

	OODLE_FREE_ARRAY(to_rrs,num_to_surfaces);

	return OodleTex_Err_OK;
}

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_Layout_Create(
	OodleTex_BC bcn_format,
	const OodleTex_Surface * surfaces, int num_surfaces,
	OodleTex_Layout ** out_layout
	)
{
	OodleTex_Err err = OodleTex_Enter();
	if ( err != OodleTex_Err_OK )
		return err;

	err = OodleTex_Entry_ValidateSurfaces(surfaces,num_surfaces);
	if ( err != OodleTex_Err_OK )
		return err;

	SINTa bytes_per_block = OodleTex_BC_BytesPerBlock(bcn_format);
	if ( bytes_per_block == -1 )
		return OodleTex_Err_BadBCnFormat;

	OodleTex_Layout * layout = OodleNew1(OodleTex_Layout, bytes_per_block);
	if ( !layout )
		return OodleTex_Err_Internal;

	// NOTE(fg): only using OodleTex_Surface * here to make it easier
	// for customers that are creating one layout per texture
	layout->m_surfaces.reserve(num_surfaces);
	for (int i = 0; i < num_surfaces; ++i)
	{
		OodleTex_Layout::SurfaceInfo nfo;
		nfo.width = surfaces[i].width;
		nfo.height = surfaces[i].height;
		layout->m_surfaces.push_back(nfo);
	}

	if ( err == OodleTex_Err_OK )
		*out_layout = layout;

	return err;
}

// OodleTex_Layout_* funcs (except for Create) don't need
// OodleTex_Enter since having successfully created a OodleTex_Layout
// implies OodleTex_Enter has already been called

OOFUNC1 void OOFUNC2 OodleTex_Layout_Destroy(OodleTex_Layout * layout)
{
	if ( layout )
		OodleDelete(layout);
}

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_Layout_GenerateBlockIDs(
	const OodleTex_Layout * layout,
	int surface_index,
	void * block_ids, SINTa block_id_row_stride_bytes
	)
{
	return layout->GenerateBlockIDs(surface_index, block_ids, block_id_row_stride_bytes);
}

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_Layout_SetBlockLayout(
	OodleTex_Layout * layout,
	const void * reordered_block_ids, SINTa num_reordered_blocks
	)
{
	return layout->SetBlockLayout(reordered_block_ids, num_reordered_blocks);
}

OOFUNC1 SINTa OOFUNC2 OodleTex_BC7Prep_MinEncodeOutputSize(SINTa nblocks)
{
	return bc7prep_min_output_size(nblocks);
}

OOFUNC1 SINTa OOFUNC2 OodleTex_BC7Prep_MinEncodeScratchSize(SINTa nblocks)
{
	return bc7prep_min_scratch_size(nblocks);
}

OOFUNC1 SINTa OOFUNC2 OodleTex_BC7Prep_Encode(OodleTexRT_BC7PrepHeader * header, void * output_buf, SINTa output_size, const void * input_bc7, SINTa num_blocks, void * scratch_buf, SINTa scratch_size)
{
	OodleTex_Err err = OodleTex_Enter();
	if ( err != OodleTex_Err_OK )
		return err;

	FPStateScope save_state;
	BC7PrepSettings settings;
	BC7PrepSettings_InitDefault(&settings);
	
	/*
	settings.lambda = 0.01f; // default lambda
	settings.canonicalize_solid = true; // yes, canonicalize solid blocks
	settings.exhaustive_splits = false; // don't bother with these, they're slow and barely help
	*/
	
	SINTa result = bc7prep_encode(header, (U8 *)output_buf, output_size, (const U8 *)input_bc7, num_blocks, (U8 *)scratch_buf, scratch_size, settings);
	return result;
}

#define OodleTex_RMSE_Normalized_BCNAware_Error	(-1.f)

OOFUNC1 F32 OOFUNC2 OodleTex_RMSE_Normalized_BCNAware(
	OodleTex_BC was_bcn,
	const OodleTex_Surface * surf1,OodleTex_PixelFormat format1,
	const OodleTex_Surface * surf2,OodleTex_PixelFormat format2)
{
	OodleTex_Err err = OodleTex_Enter();
	if ( err != OodleTex_Err_OK )
		return OodleTex_RMSE_Normalized_BCNAware_Error;

	// ValidateSurface works on arrays, so copy them into one for validation!
	OodleTex_Surface surfs[2];
	surfs[0] = *surf1;
	surfs[1] = *surf2;
	err = OodleTex_Entry_ValidateSurfaces(surfs,2);
	if ( err != OodleTex_Err_OK )
		return OodleTex_RMSE_Normalized_BCNAware_Error;

	rrPixelFormat pf1 = OodleTex_PixelFormat_to_rrPixelFormat(format1);
	if ( pf1 == rrPixelFormat_Invalid )
		return OodleTex_RMSE_Normalized_BCNAware_Error;

	rrPixelFormat pf2 = OodleTex_PixelFormat_to_rrPixelFormat(format2);
	if ( pf2 == rrPixelFormat_Invalid )
		return OodleTex_RMSE_Normalized_BCNAware_Error;
		
	FPStateScope save_state;
	rrSurfaceObj rrs1;
	init_rrs_from_texsurf(&rrs1, surf1, pf1);
		
	rrSurfaceObj rrs2;
	init_rrs_from_texsurf(&rrs2, surf2, pf2);
	
	if ( was_bcn == OodleTex_BC1_WithTransparency )
	{
		// force A to 0 or 255
	
		// if it was 3 U8 we can NOT just leave it
		// because rrSurface_GetRMSE uses the MIN of channel count
		//	and we do not want to ignore A deltas here even if source as 3-channel
	
		rrSurface_ChangeFormatNormalized(&rrs1,rrPixelFormat_4_U8);
		rrSurfaceDXTC_MakeOneBitTransparentCanonical(&rrs1);
		
		rrSurface_ChangeFormatNormalized(&rrs2,rrPixelFormat_4_U8);
		rrSurfaceDXTC_MakeOneBitTransparentCanonical(&rrs2);

		// just go ahead and do the 8-bit RMSE immediately
		//	rather than dropping through to general path
		//	(should give same result)
		
		F64 ret = rrSurface_GetRMSE(&rrs1,&rrs2);
		return (F32)ret;
	}
	
	const rrPixelFormatInfo * info1 = rrPixelFormat_GetInfo(pf1);
	const rrPixelFormatInfo * info2 = rrPixelFormat_GetInfo(pf2);
		
	int channels_to_diff;
	bool do_hdr_diff = false;
	
	int min_channels = RR_MIN(info1->channels,info2->channels);
		
	switch(was_bcn)
	{
	case OodleTex_BC1:
	case OodleTex_BC7RGB:
		channels_to_diff = 3;
		break;
	//case OodleTex_BC1_WithTransparency: // handled earlier
	case OodleTex_BC2: 
	case OodleTex_BC3: 
	case OodleTex_BC7RGBA :
		channels_to_diff = 4;
		break;
	case OodleTex_BC4U:
	case OodleTex_BC4S:
		channels_to_diff = 1;
		break;
	case OodleTex_BC5U:
	case OodleTex_BC5S:
		channels_to_diff = 2;
		break;
	case OodleTex_BC6U:
	case OodleTex_BC6S:
		do_hdr_diff = true;
		channels_to_diff = 3;
		break;	
	case OodleTex_BC_Invalid:
		// can be used for unknown BCN, or arbitrary non-BCN image diff
		// assume HDR diff if both are float :
		do_hdr_diff = ( info1->isFloat && info2->isFloat );
		channels_to_diff = min_channels;
		break;
	default:
		return OodleTex_RMSE_Normalized_BCNAware_Error;		
	}
	
	RR_ASSERT( channels_to_diff >= 1 && channels_to_diff <= 4 );		
	
	if ( min_channels < channels_to_diff )
	{
		// if BC7RGBA was encoded, even if source was 3-channel RGB
		//	we need a 4-channel diff to measure how the 255 A was lost
		// do NOT go down to min channels here, make sure to use channels_to_diff

		rrPrintf_v2("Warning: min_channels (%d) < channels_to_diff (%d)\n",min_channels,channels_to_diff);
		
		#if 0
		// disabled 05-24-2021 :

		// say if I take a 1-channel image
		//	and encode it as BC7
		// then pass was_bcn of BC7RGB
		// it will have channels_to_diff = 3
		//  but min_channels = 1
		// if I diff the other 2 missing channels, what I will be diffing against
		//	is their implicit promotion to 3 channels
		// that stresses whether you do the implicit promotion the same in Encode and here
		// eg. gray scale 1 U8 needs to be promoted like X -> XXX
		rrPrintf_v2("  changing to %d\n",min_channels);
		channels_to_diff = min_channels;
		#endif
	}
	
	// force format to float of N channels :
	rrPixelFormat diff_format = RR_MAKE_PIXELFORMAT_SIMPLE(channels_to_diff,RR_PIXELFORMAT_TYPE_F32);
	
	rrSurface_ChangeFormatNormalized(&rrs1,diff_format);
	rrSurface_ChangeFormatNormalized(&rrs2,diff_format);
	
	if ( do_hdr_diff )
	{
		// BC6 doesn't do A; should never have the A channel here
		RR_ASSERT( channels_to_diff < 4 );

		// CB 05-04-2021 :
		//	change to MRSSEN
		F64 mrssen = rrSurface_GetMRSSE_Neighborhood(&rrs1,&rrs2);
		mrssen = sqrt(mrssen);

		F32 ret = (F32)mrssen; 
		
		// scale to be similar to other rmse's
		//  (in particular you need the +1 rmse target of textest_bc1rd to make sense)
		ret *= 200.f;

		return ret;
	}
	else
	{	
		// RMSE on normalized floats
		// then * 255 to go to U8 scale
		F64 rmse_normalized = rrSurface_GetRMSE(&rrs1,&rrs2);
		F32 ret = (F32)( rmse_normalized * 255.0 );
		return ret;
	}
}	

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_BlitNormalized(
	OodleTex_Surface * to_surf,OodleTex_PixelFormat to_format,
	const OodleTex_Surface * from_surf,OodleTex_PixelFormat from_format)
{
	OodleTex_Err err = OodleTex_Enter();
	if ( err != OodleTex_Err_OK )
		return err;
		
	err = OodleTex_Entry_ValidateSurfaces(from_surf,1);
	if ( err != OodleTex_Err_OK )
		return err;

	rrPixelFormat to_pf = OodleTex_PixelFormat_to_rrPixelFormat(to_format);
	if ( to_pf == rrPixelFormat_Invalid )
		return OodleTex_Err_BadPixelFormat;

	rrPixelFormat from_pf = OodleTex_PixelFormat_to_rrPixelFormat(from_format);
	if ( from_pf == rrPixelFormat_Invalid )
		return OodleTex_Err_BadPixelFormat;
		
	FPStateScope save_state;
	rrSurfaceObj to_rrs;
	init_rrs_from_texsurf(&to_rrs, to_surf, to_pf);
		
	rrSurfaceObj from_rrs;
	init_rrs_from_texsurf(&from_rrs, from_surf, from_pf);
	
	rrSurface_BlitNormalized(&to_rrs,&from_rrs);
	
	return OodleTex_Err_OK;
}	
		
OodleTex_PixelFormat OodleTex_Surface_from_RRS( OodleTex_Surface * input, const rrSurface * rrs )
{
	input->pixels = rrs->data;
	input->width = rrs->width;
	input->height = rrs->height;
	input->rowStrideBytes = rrs->stride;
	OodleTex_PixelFormat input_pf = rrPixelFormat_to_OodleTex_PixelFormat(rrs->pixelFormat);
	return input_pf;
}

// OodleTex_RMSE_Normalized_BCNAware
F32 rrSurface_RMSE_Normalized_BCNAware(
	rrPixelFormat bcn_pf,
	rrDXTCOptions options,
	const rrSurface * in_rrs1,
	const rrSurface * in_rrs2
	)
{
	// automatically pick up bcn_pf if one of the surfaces is bcn ?
	//	-> NO DO NOT
	// if we only have the PF we've lost the informat about whether it's BC7 or BC7RGB
	//	better to use the Invalid path in that case
	/*
	if ( bcn_pf == rrPixelFormat_Invalid )
	{
		if ( rrPixelFormat_IsBlockCompressed(in_rrs1->pixelFormat) )
			bcn_pf = in_rrs1->pixelFormat;
		else if ( rrPixelFormat_IsBlockCompressed(in_rrs2->pixelFormat) )
			bcn_pf = in_rrs2->pixelFormat;
	}
	*/

	OodleTex_BC bcn = rrPixelFormat_to_OodleTex_BC(bcn_pf);
	// bcn == Invalid is okay
	if ( options & rrDXTCOptions_BC7_IgnoreAlpha )
	{
		if ( bcn == OodleTex_BC7RGBA )
		{
			bcn = OodleTex_BC7RGB;
		}
		else
		{
			rrprintf("WARNING: rrSurface_RMSE_Normalized_BCNAware IgnoreAlpha without BC7 ?\n");
		}
	}
	else if ( options & rrDXTCOptions_BC1_OneBitAlpha )
	{
		if ( bcn == OodleTex_BC1 )
		{
			bcn = OodleTex_BC1_WithTransparency;
		}
		else
		{
			rrprintf("WARNING: rrSurface_RMSE_Normalized_BCNAware OneBitAlpha without BC1 ?\n");
		}		
	}
	// @@ could do the BC4/5 PreserveExtremes validation here too
	// @@ NOTE : FavorLuma ignored fix me

	FPStateScope save_state;
	rrSurfaceObj rrs1;
	rrSurfaceObj rrs2;
	rrSurface_SetView(&rrs1,in_rrs1);
	rrSurface_SetView(&rrs2,in_rrs2);
	
	rrSurfaceDXTC_DecompressIfBCNInPlace(&rrs1);
	rrSurface_Depalettize(&rrs1);
	
	rrSurfaceDXTC_DecompressIfBCNInPlace(&rrs2);
	rrSurface_Depalettize(&rrs2);
		
	// rrs1 & 2 should be planar signals now
	//	which is all OodleTex_PixelFormat can do
	OodleTex_Surface is1;
	OodleTex_Surface is2;
	OodleTex_PixelFormat pf1 = OodleTex_Surface_from_RRS(&is1,&rrs1);
	OodleTex_PixelFormat pf2 = OodleTex_Surface_from_RRS(&is2,&rrs2);
	
	RR_ASSERT_ALWAYS( pf1 != OodleTex_PixelFormat_Invalid );
	RR_ASSERT_ALWAYS( pf2 != OodleTex_PixelFormat_Invalid );
	
	F32 ret = OodleTex_RMSE_Normalized_BCNAware(bcn,&is1,pf1,&is2,pf2);
	
	return ret;
}
	
OODLE_NS_END


// OodleAPI_Texture is in index.idc

		
//idoc(begin)
//idoc(autolink,on)
//idoc(markdown,on)

//idoc(page,OodleAPI_Texture)
/*

	Oodle Texture library contains tool-side functions for encoding to BC1-7.

	Oodle Texture should not be built into your game runtime or redistributed
	(Oodle Texture RT is the runtime portion).
	
	Oodle Texture includes the encoders for the lossless transforms like BC7Prep.
	The decoders are in the RT lib.

	$^TableOfContents

*/

//idoc(parent,OodleAPI_Texture)
//idoc(page,OodleAPI_TextureBase,Texture base)
/*

	Oodle Texture base layer.

	$^TableOfContents

*/

//idoc(parent,OodleAPI_Texture)
//idoc(page,OodleAPI_TextureCoding,Texture encoding and decoding)
/*
	This is the portion of the Oodle Texture API dealing with its primary functionality, BCn texture
	encoding and decoding.

	$^TableOfContents

*/

//idoc(parent,OodleAPI_Texture)
//idoc(page,OodleAPI_TextureBC7Prep,Texture BC7Prep encoding)
/*
	The RDO encoders make the already-lossy BCn formats even more so to achieve higher
	compression ratios. But for some textures, increasing error any further is highly
	undesirable.

	BC1-5 can instead be encoded with our baseline encoders for maximum quality, and will
	in general still see a decent size reduction when stored on disk in a lossless format
	such as Oodle Kraken.  Unfortunately, the same cannot be said for BC6H or BC7, which
	usually see little to no size reduction with a typical lossless compressor.

	BC7Prep solves this problem for BC7. BC7Prep losslessly rewrites BC7 textures into
	a different format that is normally larger than the original BC7 data, but compresses
	to a significantly smaller size; results depend on the image, but lossless size
	reductions of 5% to 25% are common, which is more in line with the results you get
	when further compressing BC1-5 format data.

	The other Oodle Texture encoders produce BCn format data that can be immediately
	consumed on the GPU. The same is not true for BC7Prep; rather, BC7Prep requires
	decoding back to BC7. This can be done either on the CPU (always supported) or,
	on select platforms, by a GPU decoder using compute shaders.

*/

//idoc(parent,OodleAPI_Texture)
//idoc(page,OodleAPI_TextureLayout,Texture layouts)
/*
	Texture layouts are an advanced feature of Oodle Texture that is used to encode BCn texture data
	straight to internal GPU memory layouts. This is primarily intended for game consoles, which often have
	specified, fixed texture layout patterns and store texture data in that layout even on disk, which
	avoids extra CPU/GPU work on load.

	However, the Oodle Texture encoders assume their output is fed straight into the final on-disk
	compressor; reordering the BCn blocks afterwards breaks this assumption and will reduce compression
	ratio, often significantly.

	The solution is to make Oodle Texture aware of the final GPU memory layout, and make the encoders
	produce data in the target layout directly without requiring another reordering step afterwards
	that might spoil the results.

	This is accomplished through texture layouts.

	$^TableOfContents

*/

//idoc(parent,OodleAPI_Texture)
//idoc(page,OodleAPI_TextureUtils,Texture utility functions)
/*
	These functions are not required to use Oodle Texture and are provided for convenience.

	$^TableOfContents
*/
//idoc(parent,OodleAPI_Texture)
//idoc(page,OodleAPI_OodleTex_Plugins,Texture plugins)
/*
	Plugins allow the app to customize Oodle Texture's memory allocation, logging and asserts.
	They are also used to enable the job system.
*/
//idoc(parent,OodleAPI_OodleTex_Plugins)
//idoc(page,OodleAPI_Base,Oodle base)
/*

   Shared definitions between all Oodle libraries.

*/

