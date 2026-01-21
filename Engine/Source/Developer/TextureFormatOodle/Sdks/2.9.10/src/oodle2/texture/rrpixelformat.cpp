// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetsse4

#include "rrpixelformat.h"
#include "rrmath.h"
#include "rrcolor.h"
//#include "cbrad/cbradutil.h"
#include "rrbytepacking.h"
#include "vec128.inl"

RR_NAMESPACE_START

#if defined(DO_BUILD_SSE4) || defined(DO_BUILD_NEON64)
#define RRPIXEL_GENERIC_SIMD
#endif

/*

!! WARNING : I have not been careful about endian!  
Probably should be swapped in save/load, but I currently IO as raw data

rrPixelFormat currently treats I16 and I32 data in memory as being in native format
	eg. no endian swap on memory access

It either has to be swapped in IO or in the PixelFormat_Get operations

/**/

//---------------------------------------------------------------------------------

SINTa rrPixelFormat_GetSurfaceSizeBytes(rrPixelFormat format, SINTa stride, int height)
{
	//if ( (format & RR_PIXELFORMAT_CLASS_MASK) == RR_PIXELFORMAT_CLASS_BLOCK4 )
	if ( RR_PIXELFORMAT_GET_TYPE(format) == RR_PIXELFORMAT_TYPE_BLOCK4 )
	{
		RR_DURING_ASSERT( const rrPixelFormatInfo * info = rrPixelFormat_GetInfo(format) );
		RR_ASSERT( info->blockSize == 4 );
		
		height = rrAlignUp(height,4);
			
		SINTa by = height>>2;
		
		return  (SINTa)by * stride;
	}
	else
	{
		// simple , some number of bytes per pixel
		
		return  (SINTa) height * stride;
	}
}

int rrPixelFormat_Get4x4BlockSizeBytes(rrPixelFormat format)
{
	const rrPixelFormatInfo * info = rrPixelFormat_GetInfo(format);

	if ( RR_PIXELFORMAT_GET_TYPE(format) == RR_PIXELFORMAT_TYPE_BLOCK4 )
	{
		RR_ASSERT( info->blockSize == 4 );
		RR_ASSERT( info->bytesPerPixel == 0 );
		RR_ASSERT( info->bytesPerBlock != 0 );
		
		return info->bytesPerBlock;
	}
	else
	{
		RR_ASSERT( info->blockSize == 1 );
		RR_ASSERT( info->bytesPerPixel == info->bytesPerBlock );
		RR_ASSERT( info->bytesPerPixel != 0 );
		
		return info->bytesPerPixel * 16;
	}
}


U8 * rrPixelFormat_Seek(rrPixelFormat format, U8 * ptr, SINTa stride, int x,int y)
{
	const rrPixelFormatInfo * info = rrPixelFormat_GetInfo(format);
	
//	if ( (format & RR_PIXELFORMAT_CLASS_MASK) == RR_PIXELFORMAT_CLASS_BLOCK4 )
	if ( RR_PIXELFORMAT_GET_TYPE(format) == RR_PIXELFORMAT_TYPE_BLOCK4 )
	{
		if ( (x&3) || (y&3) )
		{
			RR_ASSERT_FAILURE("Block4 not aligned");
			return NULL;
		}
		
		RR_ASSERT( info->blockSize == 4 );
		int bx = x>>2;
		int by = y>>2;
		
		ptr += by * stride;
		
		ptr += bx * (SINTa) (info->bytesPerBlock);
		
		return ptr;
	}
	else
	{
		// simple , some number of bytes per pixel
		
		RR_ASSERT( info->bytesPerPixel != 0 );
		
		ptr += y * stride;
		ptr += x * (SINTa) (info->bytesPerPixel);
	
		return ptr;
	}
}

SINTa	rrPixelFormat_MakeStride_Minimum(rrPixelFormat format, int w)
{
	const rrPixelFormatInfo * info = rrPixelFormat_GetInfo(format);
	RR_ASSERT_ALWAYS( info != NULL );
	
	// in practice blockSize == 1 or 4
	RR_ASSERT( info->blockSize >= 1 && info->blockSize <= 8 );
	w = rrAlignUp32(w,info->blockSize);
	
	//RR_ASSERT( w == rrAlignUp( w, info->blockSize ) );
		
	int blocks = w / info->blockSize;
	SINTa bytes = blocks * (SINTa) info->bytesPerBlock;
	
	return bytes;
}

//---------------------------------------------------------------------------------

#define INTU_INFO(channels,bytesPerPixel)	{ channels, bytesPerPixel, 1, bytesPerPixel, false, false }
#define INTS_INFO(channels,bytesPerPixel)	{ channels, bytesPerPixel, 1, bytesPerPixel, false, true }
#define FLOAT_INFO(channels,bytesPerPixel)	{ channels, bytesPerPixel, 1, bytesPerPixel, true, true }
#define BLOCK4U_INFO(channels,bitsPerPixel)	{ channels, 0, 4, bitsPerPixel*16/8, false, false }
#define BLOCK4S_INFO(channels,bitsPerPixel)	{ channels, 0, 4, bitsPerPixel*16/8, false, true }

static const rrPixelFormatInfo rrPixelFormat_1_U8_Info = INTU_INFO(1,1);
static const rrPixelFormatInfo rrPixelFormat_2_U8_Info = INTU_INFO(2,2);
static const rrPixelFormatInfo rrPixelFormat_B8G8R8_Info = INTU_INFO(3,3); // 3_U8
static const rrPixelFormatInfo rrPixelFormat_B8G8R8A8_Info = INTU_INFO(4,4); // 4_U8
static const rrPixelFormatInfo rrPixelFormat_R8G8B8A8_Info = INTU_INFO(4,4);
static const rrPixelFormatInfo rrPixelFormat_R8G8B8_Info = INTU_INFO(3,3);
static const rrPixelFormatInfo rrPixelFormat_R8G8B8x8_Info = INTU_INFO(3,4);
static const rrPixelFormatInfo rrPixelFormat_B8G8R8x8_Info = INTU_INFO(3,4);

static const rrPixelFormatInfo rrPixelFormat_1_U16_Info = INTU_INFO(1,2);
static const rrPixelFormatInfo rrPixelFormat_2_U16_Info = INTU_INFO(2,4);
static const rrPixelFormatInfo rrPixelFormat_3_U16_Info = INTU_INFO(3,6);
static const rrPixelFormatInfo rrPixelFormat_4_U16_Info = INTU_INFO(4,8);

static const rrPixelFormatInfo rrPixelFormat_1_S32_Info = INTU_INFO(1,4);
static const rrPixelFormatInfo rrPixelFormat_2_S32_Info = INTU_INFO(2,8);
static const rrPixelFormatInfo rrPixelFormat_3_S32_Info = INTU_INFO(3,12);
static const rrPixelFormatInfo rrPixelFormat_4_S32_Info = INTU_INFO(4,16);

static const rrPixelFormatInfo rrPixelFormat_1_F16_Info = FLOAT_INFO(1,2*1);
static const rrPixelFormatInfo rrPixelFormat_2_F16_Info = FLOAT_INFO(2,2*2);
static const rrPixelFormatInfo rrPixelFormat_3_F16_Info = FLOAT_INFO(3,2*3);
static const rrPixelFormatInfo rrPixelFormat_4_F16_Info = FLOAT_INFO(4,2*4);

static const rrPixelFormatInfo rrPixelFormat_1_F32_Info = FLOAT_INFO(1,4);
static const rrPixelFormatInfo rrPixelFormat_2_F32_Info = FLOAT_INFO(2,8);
static const rrPixelFormatInfo rrPixelFormat_3_F32_Info = FLOAT_INFO(3,12);
static const rrPixelFormatInfo rrPixelFormat_4_F32_Info = FLOAT_INFO(4,16);

static const rrPixelFormatInfo rrPixelFormat_R5G6B5_Info   = INTU_INFO(3,2);
static const rrPixelFormatInfo rrPixelFormat_A1R5G5B5_Info = INTU_INFO(4,2);
static const rrPixelFormatInfo rrPixelFormat_Palette8_Info = INTU_INFO(1,1);

static const rrPixelFormatInfo rrPixelFormat_RGBE_8888_Info = FLOAT_INFO(3,4);
static const rrPixelFormatInfo rrPixelFormat_RGBE_9995_Info = FLOAT_INFO(3,4);

static const rrPixelFormatInfo rrPixelFormat_BC1_Info = BLOCK4U_INFO(4,4);
static const rrPixelFormatInfo rrPixelFormat_BC2_Info = BLOCK4U_INFO(4,8);
static const rrPixelFormatInfo rrPixelFormat_BC3_Info = BLOCK4U_INFO(4,8);
static const rrPixelFormatInfo rrPixelFormat_BC4U_Info = BLOCK4U_INFO(1,4);
static const rrPixelFormatInfo rrPixelFormat_BC4S_Info = BLOCK4S_INFO(1,4);
static const rrPixelFormatInfo rrPixelFormat_BC5U_Info = BLOCK4U_INFO(2,8);
static const rrPixelFormatInfo rrPixelFormat_BC5S_Info = BLOCK4S_INFO(2,8);
static const rrPixelFormatInfo rrPixelFormat_BC6U_Info = BLOCK4U_INFO(4,8);
static const rrPixelFormatInfo rrPixelFormat_BC6S_Info = BLOCK4S_INFO(4,8);
static const rrPixelFormatInfo rrPixelFormat_BC7_Info = BLOCK4U_INFO(4,8);

static const rrPixelFormatInfo rrPixelFormat_1_S8_Info = INTS_INFO(1,1);
static const rrPixelFormatInfo rrPixelFormat_2_S8_Info = INTS_INFO(2,2);
static const rrPixelFormatInfo rrPixelFormat_3_S8_Info = INTS_INFO(3,3);
static const rrPixelFormatInfo rrPixelFormat_4_S8_Info = INTS_INFO(4,4);

static const rrPixelFormatInfo rrPixelFormat_1_S16_Info = INTS_INFO(1,2);
static const rrPixelFormatInfo rrPixelFormat_2_S16_Info = INTS_INFO(2,4);
static const rrPixelFormatInfo rrPixelFormat_3_S16_Info = INTS_INFO(3,6);
static const rrPixelFormatInfo rrPixelFormat_4_S16_Info = INTS_INFO(4,8);

#define PIXELFORMAT_CASES()	\
	CASE(rrPixelFormat_1_U8)\
	CASE(rrPixelFormat_2_U8)\
	CASE(rrPixelFormat_B8G8R8)\
	CASE(rrPixelFormat_R8G8B8)\
	CASE(rrPixelFormat_B8G8R8A8)\
	CASE(rrPixelFormat_R8G8B8A8)\
	CASE(rrPixelFormat_B8G8R8x8)\
	CASE(rrPixelFormat_R8G8B8x8)\
	CASE(rrPixelFormat_1_U16)\
	CASE(rrPixelFormat_2_U16)\
	CASE(rrPixelFormat_3_U16)\
	CASE(rrPixelFormat_4_U16)\
	CASE(rrPixelFormat_1_S32)\
	CASE(rrPixelFormat_2_S32)\
	CASE(rrPixelFormat_3_S32)\
	CASE(rrPixelFormat_4_S32)\
	CASE(rrPixelFormat_1_F16)\
	CASE(rrPixelFormat_2_F16)\
	CASE(rrPixelFormat_3_F16)\
	CASE(rrPixelFormat_4_F16)\
	CASE(rrPixelFormat_1_F32)\
	CASE(rrPixelFormat_2_F32)\
	CASE(rrPixelFormat_3_F32)\
	CASE(rrPixelFormat_4_F32)\
	CASE(rrPixelFormat_R5G6B5)\
	CASE(rrPixelFormat_A1R5G5B5)\
	CASE(rrPixelFormat_Palette8)\
	CASE(rrPixelFormat_RGBE_8888)\
	CASE(rrPixelFormat_RGBE_9995)\
	CASE(rrPixelFormat_BC1)\
	CASE(rrPixelFormat_BC2)\
	CASE(rrPixelFormat_BC3)\
	CASE(rrPixelFormat_BC4U)\
	CASE(rrPixelFormat_BC4S)\
	CASE(rrPixelFormat_BC5U)\
	CASE(rrPixelFormat_BC5S)\
	CASE(rrPixelFormat_BC6U)\
	CASE(rrPixelFormat_BC6S)\
	CASE(rrPixelFormat_BC7)\
	CASE(rrPixelFormat_1_S8)\
	CASE(rrPixelFormat_2_S8)\
	CASE(rrPixelFormat_3_S8)\
	CASE(rrPixelFormat_4_S8)\
	CASE(rrPixelFormat_1_S16)\
	CASE(rrPixelFormat_2_S16)\
	CASE(rrPixelFormat_3_S16)\
	CASE(rrPixelFormat_4_S16)

// hack for LosslessFiltersHeuristicDDS :
#ifndef RR_SURFACEDDS_READHEADER_ONLY

const rrPixelFormat c_formats[] =
{
	#define CASE(fmt) fmt, 
	PIXELFORMAT_CASES()
	#undef CASE
	rrPixelFormat_ForceDWord
};

const int c_formats_count = RR_ARRAY_SIZE(c_formats) - 1;

RR_COMPILER_ASSERT( RR_ARRAY_SIZE(c_formats) - 1 == RR_PIXELFORMAT_COUNT );

int rrPixelFormat_GetIndex(rrPixelFormat format)
{
	RR_ASSERT( format != rrPixelFormat_Invalid );
	
	// @@ speed this up?
	//	this is used for every line of a row cache scan to blit each line
	
	for LOOPARRAY(i,c_formats)
	{
		if ( c_formats[i] == format ) return i;
	}
	
	RR_ASSERT_FAILURE("format not found");
	return -1;
}

#endif // RR_SURFACEDDS_READHEADER_ONLY
	
const rrPixelFormatInfo * rrPixelFormat_GetInfo(rrPixelFormat format)
{
	RR_ASSERT( format != rrPixelFormat_Invalid );

	switch(format)
	{
	#define CASE(fmt)  case fmt: return &(fmt##_Info);
	
	PIXELFORMAT_CASES();
	
	#undef CASE
	
	default:
		RR_ASSERT_FAILURE("no Info for format");
		return NULL;
	}
}

const char * rrPixelFormat_GetName(rrPixelFormat format)
{
	RR_ASSERT( format != rrPixelFormat_Invalid );
	
	const size_t c_skip  = strlen("rrPixelFormat_");
	switch(format)
	{
	
	#define CASE(fmt)  case fmt: { const char * full = RR_STRINGIZE(fmt); return &full[c_skip]; }
	
	PIXELFORMAT_CASES()
	
	#undef CASE
	
	default:
		return NULL;
	}
}

const char * rrPixelFormat_GetName_WithDXTCOptions(rrPixelFormat pf, rrDXTCOptions opts)
{	
	if ( pf == rrPixelFormat_BC6U && (opts & rrDXTCOptions_BC6_FavorLuminance) )
		return "BC6U-Luma";
	if ( pf == rrPixelFormat_BC6S && (opts & rrDXTCOptions_BC6_FavorLuminance) )
		return "BC6S-Luma";
	if ( pf == rrPixelFormat_BC7 && (opts & rrDXTCOptions_BC7_IgnoreAlpha ) )
		return "BC7-RGB";
	if ( pf == rrPixelFormat_BC1 && (opts & rrDXTCOptions_BC1_OneBitAlpha ) )
		return "BC1-A";

	if ( opts & rrDXTCOptions_BC3457_PreserveExtremes )
	{
		if ( pf == rrPixelFormat_BC3 ) return "BC3-PE";
		if ( pf == rrPixelFormat_BC4U ) return "BC4U-PE";
		if ( pf == rrPixelFormat_BC4S ) return "BC4S-PE";
		if ( pf == rrPixelFormat_BC5U ) return "BC5U-PE";
		if ( pf == rrPixelFormat_BC5S ) return "BC5S-PE";
		if ( pf == rrPixelFormat_BC7 ) return "BC7-PE";
	}

	return rrPixelFormat_GetName(pf);
}

int rrPixelFormat_GetBCNumber(rrPixelFormat pf)
{
	if ( ! rrPixelFormat_IsBlockCompressed(pf) ) return 0;

	switch(pf)
	{
	case rrPixelFormat_BC1: return 1;
	case rrPixelFormat_BC2: return 2;
	case rrPixelFormat_BC3: return 3;
	case rrPixelFormat_BC4U: 
	case rrPixelFormat_BC4S: return 4;
	case rrPixelFormat_BC5U: 
	case rrPixelFormat_BC5S: return 5;
	case rrPixelFormat_BC6U: 
	case rrPixelFormat_BC6S: return 6;
	case rrPixelFormat_BC7: return 7;
	RR_NO_DEFAULT_CASE
	}
}

rrPixelFormat rrPixelFormat_NumberToBCN_U(int bcn)
{
	switch(bcn)
	{
		case 1: return rrPixelFormat_BC1;
		case 2: return rrPixelFormat_BC2;
		case 3: return rrPixelFormat_BC3;
		case 4: return rrPixelFormat_BC4U;
		case 5: return rrPixelFormat_BC5U;
		case 6: return rrPixelFormat_BC6U;
		case 7: return rrPixelFormat_BC7;
		default:
			return rrPixelFormat_Invalid;
	}
}


// Scaling applied to pixels to yield normalized values
// this is what 1.0 encodes to when viewing the format as integer, essentially
int rrPixelFormat_GetNormalizedScale(rrPixelFormat fmt)
{
	RR_ASSERT( ! rrPixelFormat_IsBlockCompressed(fmt) );

	switch ( RR_PIXELFORMAT_GET_TYPE( fmt ) )
	{
	case RR_PIXELFORMAT_TYPE_U8:	return 255;
	case RR_PIXELFORMAT_TYPE_U16:	return 65535;
	case RR_PIXELFORMAT_TYPE_S32:	return 255; // our S32 is odd; it's not SNORM, it's 8-bit pixels with extra range
	case RR_PIXELFORMAT_TYPE_F16:	return 1;
	case RR_PIXELFORMAT_TYPE_F32:	return 1;
	case RR_PIXELFORMAT_TYPE_S8: 	return 127;
	case RR_PIXELFORMAT_TYPE_S16:	return 32767;
	default:						
		ooLogError("rrPixelFormat_GetNormalizedScale: not a scalar format");
		return 0;
	}
}

//---------------------------------------------------------------------------------

// NOTE: dest == src is used (despite type pun)
static void Colors4ItoF_Linear_Multi( rrColor4F * dest, const rrColor4I * src, S32 count )
{
#ifdef RRPIXEL_GENERIC_SIMD
	int i;

	// groups of 4
	for (i = 0; i < (count & ~3); i += 4)
	{
		Vec128_S32 col_int0 = Vec128_S32::loadu(&src[i + 0].r);
		Vec128_S32 col_int1 = Vec128_S32::loadu(&src[i + 1].r);
		Vec128_S32 col_int2 = Vec128_S32::loadu(&src[i + 2].r);
		Vec128_S32 col_int3 = Vec128_S32::loadu(&src[i + 3].r);
		VecF32x4::from_int32(col_int0).storeu(&dest[i + 0].r);
		VecF32x4::from_int32(col_int1).storeu(&dest[i + 1].r);
		VecF32x4::from_int32(col_int2).storeu(&dest[i + 2].r);
		VecF32x4::from_int32(col_int3).storeu(&dest[i + 3].r);
	}

	for (; i < count; ++i)
	{
		Vec128_S32 col_int = Vec128_S32::loadu(&src[i].r);
		VecF32x4::from_int32(col_int).storeu(&dest[i].r);
	}
#else
	for LOOP(i,count)
		rrColor4_I_to_F_Linear(&dest[i],&src[i]);
#endif
}

// NOTE: dest == src is used (despite type pun)
static void Colors4FtoI_Linear_Multi( rrColor4I * dest, const rrColor4F * src, S32 count )
{
#ifdef RRPIXEL_GENERIC_SIMD
	int i;

	// groups of 4
	for (i = 0; i < (count & ~3); i += 4)
	{
		VecF32x4 col_flt0 = VecF32x4::loadu(&src[i + 0].r);
		VecF32x4 col_flt1 = VecF32x4::loadu(&src[i + 1].r);
		VecF32x4 col_flt2 = VecF32x4::loadu(&src[i + 2].r);
		VecF32x4 col_flt3 = VecF32x4::loadu(&src[i + 3].r);
		col_flt0.to_int32_round().storeu(&dest[i + 0].r);
		col_flt1.to_int32_round().storeu(&dest[i + 1].r);
		col_flt2.to_int32_round().storeu(&dest[i + 2].r);
		col_flt3.to_int32_round().storeu(&dest[i + 3].r);
	}

	for (; i < count; ++i)
	{
		VecF32x4 col_flt = VecF32x4::loadu(&src[i].r);
		col_flt.to_int32_round().storeu(&dest[i]);
	}
#else
	for LOOP(i,count)
		rrColor4_F_to_I_Linear(&dest[i],&src[i]);
#endif
}

#ifdef RRPIXEL_GENERIC_SIMD

template<bool t_signed>
static inline void rrPixelFormat_GetColors4I_1I8(rrColor4I * color, S32 count32, const U8 * ptr)
{
	const U8 * from = ptr;
	const Vec128_U32 mask_8b { 0xff };
	const SINTa count = count32;
	SINTa i;

	// groups of 4
	for (i = 0; i < (count & ~3); i += 4)
	{
		Vec128_U32 v = Vec128_U32::loadu_lo32(from + i); // load 4 pixels worth

		if (!t_signed)
		{
			v = v.xxxw(); // U8 has grayscale expand: broadcast value to RGB (leave A at 0)
			v |= Vec128_U32 { 0, 0, 0, 0xffffffffu }; // fill alpha channel with 0xff

			// now each lane holds 4 pixels worth of data, store them all
			(v & mask_8b).storeu(color + i + 0);
			(v.srl<8>() & mask_8b).storeu(color + i + 1);
			(v.srl<16>() & mask_8b).storeu(color + i + 2);
			v.srl<24>().storeu(color + i + 3);
		}
		else
		{
			// S8 doesn't expand, just leave green and blue at 0
			v |= Vec128_U32 { 0, 0, 0, 0x7f7f7f7fu }; // fill alpha channel with 0x7f

			// now each lane holds 4 pixels worth of data, store them all
			(v.shl<24>()).sra<24>().storeu(color + i + 0);
			(v.shl<16>()).sra<24>().storeu(color + i + 1);
			(v.shl<8>()).sra<24>().storeu(color + i + 2);
			v.sra<24>().storeu(color + i + 3);
		}
	}

	// tail
	for (; i < count; ++i)
	{
		if (!t_signed)
		{
			Vec128_U32 v = Vec128_U8(from[i]).u32() & mask_8b; // broadcast value to RGB
			v |= Vec128_U32 { 0, 0, 0, 0xff };
			v.storeu(color + i);
		}
		else
		{
			Vec128_S32 v = Vec128_S32((S8)from[i]); // sign-extended value
			v = (v & Vec128_S32 { -1, 0, 0, 0 }) | Vec128_S32 { 0, 0, 0, 0x7f }; // put 0x7f in alpha
			v.storeu(color + i);
		}
	}
}

template<bool t_signed>
static inline void rrPixelFormat_GetColors4I_2I8(rrColor4I * color, S32 count32, const U8 * ptr)
{
	const U16 * from = (const U16 *)ptr;
	const SINTa count = count32;
	SINTa i;

	// groups of 2
	for (i = 0; i < (count & ~1); i += 2)
	{
		Vec128_U8 v8 = Vec128_U8::loadu_lo32(from + i); // load 2 pixels worth

		if (!t_signed)
		{
			const Vec128_U32 constant_a_2x { 0, 0xff, 0, 0xff };

			// unpack to 32-bit lanes
			Vec128_S32 v32 = v8.to_s32_lo();

			// store the halves with constant b/a inserted
			v32.u8().unpack_lo64(constant_a_2x.u8()).storeu(color + i + 0);
			v32.u8().unpack_hi64(constant_a_2x.u8()).storeu(color + i + 1);
		}
		else
		{
			const Vec128_S32 constant_a_2x { 0, 0x7f, 0, 0x7f };

			// unpack to 32-bit lanes
			Vec128_S32 v32 = v8.s8().to_s32_lo();

			// store the halves with constant b/a inserted
			v32.u8().unpack_lo64(constant_a_2x.u8()).storeu(color + i + 0);
			v32.u8().unpack_hi64(constant_a_2x.u8()).storeu(color + i + 1);
		}
	}

	// tail element if any
	if (i < count)
	{
		// load 16-bit value (two channels)
		U32 value =  RR_GET16_LE_UNALIGNED(from + i);
		Vec128_U8 v8 = Vec128_U32(value).u8();

		if (!t_signed)
		{
			// unpack to 32-bit lanes
			Vec128_S32 v32 = v8.to_s32_lo();

			// Insert constant A
			v32 |= Vec128_S32 { 0, 0, 0, 0xff };
			v32.storeu(color + i);
		}
		else
		{
			// unpack to 32-bit lanes
			Vec128_S32 v32 = v8.s8().to_s32_lo();

			// Insert constant A
			v32 |= Vec128_S32 { 0, 0, 0, 0x7f };
			v32.storeu(color + i);
		}
	}
}

template<bool t_signed>
static void rrPixelFormat_GetColors4I_3I8(rrColor4I * color, S32 count32, const U8 * from, bool is_rgb)
{
	const Vec128_S8 shuffle_rgb { 0,1,2,-1, 3,4,5,-1, 6,7,8,-1, 9,10,11,-1 };
	const Vec128_S8 shuffle_bgr { 2,1,0,-1, 5,4,3,-1, 8,7,6,-1, 11,10,9,-1 };
	const Vec128_S8 shuffle = is_rgb ? shuffle_rgb : shuffle_bgr;
	const Vec128_U8 constant_a = t_signed ? Vec128_U8::repeat4(0, 0, 0, 0x7f) : Vec128_U8::repeat4(0, 0, 0, 0xff);
	const SINTa count = count32;
	SINTa i;

	// groups of 4, but accounting for the fact that we're actually reading 16 bytes (=1.33 pixels worth)
	for (i = 0; i <= count - (4 + 2); i += 4) // 4 pixels processed, 2 pixels slop from over-read
	{
		Vec128_U8 v8 = Vec128_U8::loadu(from + i*3).shuf(shuffle) | constant_a;

		if (!t_signed)
		{
			// Unpack to 32-bit lanes
			Vec128_U16 v16_0 = v8.to_u16_lo();
			Vec128_U16 v16_1 = v8.to_u16_hi();

			v16_0.to_u32_lo().storeu(color + i + 0);
			v16_0.to_u32_hi().storeu(color + i + 1);
			v16_1.to_u32_lo().storeu(color + i + 2);
			v16_1.to_u32_hi().storeu(color + i + 3);
		}
		else
		{
			// Unpack to 32-bit lanes
			Vec128_S16 v16_0 = v8.unpack_lo(v8).s16();
			Vec128_S16 v16_1 = v8.unpack_hi(v8).s16();

			v16_0.unpack_lo(v16_0).s32().sra<24>().storeu(color + i + 0);
			v16_0.unpack_hi(v16_0).s32().sra<24>().storeu(color + i + 1);
			v16_1.unpack_lo(v16_1).s32().sra<24>().storeu(color + i + 2);
			v16_1.unpack_hi(v16_1).s32().sra<24>().storeu(color + i + 3);
		}
	}

	// Tail loop processes pixels one at a time to be safe
	for (; i < count; i++)
	{
		U32 value = RR_GET16_LE_UNALIGNED(from + i*3 + 0) | (from[i*3 + 2] << 16);
		Vec128_U8 v8 = Vec128_U32(value).u8().shuf(shuffle) | constant_a;

		if (!t_signed)
		{
			// Unpack to 32-bit lanes and store that one pixel
			v8.to_s32_lo().storeu(color + i);
		}
		else
		{
			// Unpack to 32-bit lanes and store that one pixel
			v8.s8().to_s32_lo().storeu(color + i);
		}
	}
}	

template<bool t_signed>
static void rrPixelFormat_GetColors4I_4I8(rrColor4I * color, S32 count32, const U8 * from, bool is_rgb, bool force_alpha_255)
{
	const Vec128_S8 shuffle_rgba { 0,1,2,3, 4,5,6,7, 8,9,10,11, 12,13,14,15 };
	const Vec128_S8 shuffle_bgra { 2,1,0,3, 6,5,4,7, 10,9,8,11, 14,13,12,15 };
	const Vec128_S8 shuffle = is_rgb ? shuffle_rgba : shuffle_bgra;
	const Vec128_U8 constant_a = force_alpha_255 ? Vec128_U8::repeat4(0, 0, 0, 0xff) : Vec128_U8(0);
	const SINTa count = count32;
	SINTa i;

	// Work in groups of 4
	for (i = 0; i <= count - 4; i += 4)
	{
		Vec128_U8 v8 = Vec128_U8::loadu(from + i*4).shuf(shuffle) | constant_a;

		if (!t_signed)
		{
			// Unpack to 32-bit lanes
			Vec128_U16 v16_0 = v8.to_u16_lo();
			Vec128_U16 v16_1 = v8.to_u16_hi();

			v16_0.to_u32_lo().storeu(color + i + 0);
			v16_0.to_u32_hi().storeu(color + i + 1);
			v16_1.to_u32_lo().storeu(color + i + 2);
			v16_1.to_u32_hi().storeu(color + i + 3);
		}
		else
		{
			// Unpack to 32-bit lanes
			Vec128_S16 v16_0 = v8.unpack_lo(v8).s16();
			Vec128_S16 v16_1 = v8.unpack_hi(v8).s16();

			v16_0.unpack_lo(v16_0).s32().sra<24>().storeu(color + i + 0);
			v16_0.unpack_hi(v16_0).s32().sra<24>().storeu(color + i + 1);
			v16_1.unpack_lo(v16_1).s32().sra<24>().storeu(color + i + 2);
			v16_1.unpack_hi(v16_1).s32().sra<24>().storeu(color + i + 3);
		}
	}

	// Tail loop for last few pixels
	for (; i < count; i++)
	{
		Vec128_U8 v8 = Vec128_U8::loadu_lo32(from + i*4).shuf(shuffle) | constant_a;
		if (!t_signed)
		{
			v8.to_s32_lo().storeu(color + i);
		}
		else
		{
			v8.s8().to_s32_lo().storeu(color + i);
		}
	}
}

template<bool t_signed>
static inline void rrPixelFormat_GetColors4I_1I16(rrColor4I * color, S32 count, const U8 * ptr)
{
	const U16 * from = (const U16 *)ptr;
	Vec128_U32 constant_a = t_signed ? Vec128_U32 { 0, 0, 0, 0x7fff} : Vec128_U32 { 0, 0, 0, 0xffff };
	int i;

	// NOTE: unlike U8, our standard conversion here does not broadcast r, just expands to (r,0,0,1)

	// groups of 2
	for (i = 0; i < (count & ~1); i += 2)
	{
		Vec128_U32 v = Vec128_U32::loadu_lo32(from + i); // load 2 pixels worth

		if (!t_signed)
		{
			// put the two result vectors in place using shifts+masking, then insert the constant a
			((v & Vec128_U32 { 0xffff, 0, 0, 0 }) | constant_a).storeu(color + i + 0);
			(v.srl<16>() | constant_a).storeu(color + i + 1);
		}
		else
		{
			// put the two result vectors in place using shifts, then insert the constant a
			(v.shl<16>().sra<16>() | constant_a).storeu(color + i + 0);
			(v.sra<16>() | constant_a).storeu(color + i + 1);
		}
	}

	// tail element if any
	if (i < count)
	{
		// don't actually want a broadcast here, just a single scalar value with rest 0, but we don't have
		// that initializer currently
		if (!t_signed)
		{
			Vec128_U32 v = Vec128_U32 { from[i] } & Vec128_U32 { ~0u, 0, 0, 0 };
			v |= constant_a;
			v.storeu(color + i);
		}
		else
		{
			Vec128_S32 v = Vec128_S32 { (S16)from[i] } & Vec128_S32 { -1, 0, 0, 0 };
			v |= constant_a.s32();
			v.storeu(color + i);
		}
	}
}

template<bool t_signed>
static inline void rrPixelFormat_GetColors4I_2I16(rrColor4I * color, S32 count, const U8 * ptr)
{
	const U32 * from = (const U32 *)ptr;
	Vec128_U32 constant_a = t_signed ? Vec128_U32 { 0, 0, 0, 0x7fff } : Vec128_U32 { 0, 0, 0, 0xffff };
	for LOOP(i,count)
	{
		// load 32-bit value (two channels)
		Vec128_U16 v16 = Vec128_U16::loadu_lo32(from + i);

		// unpack to 32-bit lanes
		Vec128_U32 v32 = t_signed ? v16.s16().to_s32_lo().u32() : v16.to_u32_lo();

		// insert constant a
		v32 |= constant_a;
		v32.storeu(color + i);
	}
}

template<bool t_signed>
static inline void rrPixelFormat_GetColors4I_4I16(rrColor4I * color, S32 count, const U8 * ptr)
{
	const U16 * from = (const U16 *)ptr;
	for LOOP(i,count)
	{
		// load 64-bit value (four channels)
		Vec128_U16 v16 = Vec128_U16::loadu_lo64(from + i*4);

		// unpack to 32-bit lanes
		Vec128_U32 v32 = t_signed ? v16.s16().to_s32_lo().u32() : v16.to_u32_lo();

		v32.storeu(color + i);
	}
}

#endif

void rrPixelFormat_GetColors4I( const U8 * ptr, rrPixelFormat format, rrColor4I * color, S32 count )
{
	#define STANDARD_COPY_LOOP(fromtype,outr,outg,outb,outa) { \
		const fromtype * from = (const fromtype *)ptr; \
		for LOOP(i,count) { \
			color[i].r = outr; \
			color[i].g = outg; \
			color[i].b = outb; \
			color[i].a = outa; \
		} \
	}

	switch(format)
	{
#ifdef RRPIXEL_GENERIC_SIMD
	case rrPixelFormat_1_U8:		rrPixelFormat_GetColors4I_1I8<false>(color, count, ptr); break;
	case rrPixelFormat_R8G8:		rrPixelFormat_GetColors4I_2I8<false>(color, count, ptr); break;
	case rrPixelFormat_B8G8R8:		rrPixelFormat_GetColors4I_3I8<false>(color, count, ptr, false); break;
	case rrPixelFormat_R8G8B8:		rrPixelFormat_GetColors4I_3I8<false>(color, count, ptr, true); break;
	case rrPixelFormat_B8G8R8x8:	rrPixelFormat_GetColors4I_4I8<false>(color, count, ptr, false, true); break;
	case rrPixelFormat_R8G8B8x8:	rrPixelFormat_GetColors4I_4I8<false>(color, count, ptr, true, true); break;
	case rrPixelFormat_B8G8R8A8:	rrPixelFormat_GetColors4I_4I8<false>(color, count, ptr, false, false); break;
	case rrPixelFormat_R8G8B8A8:	rrPixelFormat_GetColors4I_4I8<false>(color, count, ptr, true, false); break;

	case rrPixelFormat_1_U16:		rrPixelFormat_GetColors4I_1I16<false>(color, count, ptr); break;
	case rrPixelFormat_2_U16:		rrPixelFormat_GetColors4I_2I16<false>(color, count, ptr); break;
	case rrPixelFormat_4_U16:		rrPixelFormat_GetColors4I_4I16<false>(color, count, ptr); break;

	case rrPixelFormat_1_S8:		rrPixelFormat_GetColors4I_1I8<true>(color, count, ptr); break;
	case rrPixelFormat_2_S8:		rrPixelFormat_GetColors4I_2I8<true>(color, count, ptr); break;
	case rrPixelFormat_3_S8:		rrPixelFormat_GetColors4I_3I8<true>(color, count, ptr, true); break;
	case rrPixelFormat_4_S8:		rrPixelFormat_GetColors4I_4I8<true>(color, count, ptr, true, false); break;

	case rrPixelFormat_1_S16:		rrPixelFormat_GetColors4I_1I16<true>(color, count, ptr); break;
	case rrPixelFormat_2_S16:		rrPixelFormat_GetColors4I_2I16<true>(color, count, ptr); break;
	case rrPixelFormat_4_S16:		rrPixelFormat_GetColors4I_4I16<true>(color, count, ptr); break;
#else
	case rrPixelFormat_1_U8:		STANDARD_COPY_LOOP(U8, from[i], from[i], from[i], 0xFF); break;
	case rrPixelFormat_R8G8:		STANDARD_COPY_LOOP(U8, from[i*2+0], from[i*2+1], 0, 0xFF); break;
	case rrPixelFormat_B8G8R8:		STANDARD_COPY_LOOP(U8, from[i*3+2], from[i*3+1], from[i*3+0], 0xFF); break;
	case rrPixelFormat_R8G8B8:		STANDARD_COPY_LOOP(U8, from[i*3+0], from[i*3+1], from[i*3+2], 0xFF); break;
	case rrPixelFormat_B8G8R8x8:	STANDARD_COPY_LOOP(U8, from[i*4+2], from[i*4+1], from[i*4+0], 0xFF); break;
	case rrPixelFormat_R8G8B8x8:	STANDARD_COPY_LOOP(U8, from[i*4+0], from[i*4+1], from[i*4+2], 0xFF); break;
	case rrPixelFormat_B8G8R8A8:	STANDARD_COPY_LOOP(U8, from[i*4+2], from[i*4+1], from[i*4+0], from[i*4+3]); break;
	case rrPixelFormat_R8G8B8A8:	STANDARD_COPY_LOOP(U8, from[i*4+0], from[i*4+1], from[i*4+2], from[i*4+3]); break;

	case rrPixelFormat_1_U16:		STANDARD_COPY_LOOP(U16, from[i], 0, 0, 0xFFFF); break;
	case rrPixelFormat_2_U16:		STANDARD_COPY_LOOP(U16, from[i*2+0], from[i*2+1], 0, 0xFFFF); break;
	case rrPixelFormat_4_U16:		STANDARD_COPY_LOOP(U16, from[i*4+0], from[i*4+1], from[i*4+2], from[i*4+3]); break;

	case rrPixelFormat_1_S8:		STANDARD_COPY_LOOP(S8, from[i], 0, 0, 0x7F); break;
	case rrPixelFormat_2_S8:		STANDARD_COPY_LOOP(S8, from[i*2+0], from[i*2+1], 0, 0x7F); break;
	case rrPixelFormat_3_S8:		STANDARD_COPY_LOOP(S8, from[i*3+0], from[i*3+1], from[i*3+2], 0x7F); break;
	case rrPixelFormat_4_S8:		STANDARD_COPY_LOOP(S8, from[i*4+0], from[i*4+1], from[i*4+2], from[i*4+3]); break;

	case rrPixelFormat_1_S16:		STANDARD_COPY_LOOP(S16, from[i], 0, 0, 0x7FFF); break;
	case rrPixelFormat_2_S16:		STANDARD_COPY_LOOP(S16, from[i*2+0], from[i*2+1], 0, 0x7FFF); break;
	case rrPixelFormat_4_S16:		STANDARD_COPY_LOOP(S16, from[i*4+0], from[i*4+1], from[i*4+2], from[i*4+3]); break;
#endif

	case rrPixelFormat_3_U16:		STANDARD_COPY_LOOP(U16, from[i*3+0], from[i*3+1], from[i*3+2], 0xFFFF); break;

	case rrPixelFormat_1_S32:		STANDARD_COPY_LOOP(S32, from[i], 0, 0, 0xFF); break;
	case rrPixelFormat_2_S32:		STANDARD_COPY_LOOP(S32, from[i*2+0], from[i*2+1], 0, 0xFF); break;
	case rrPixelFormat_3_S32:		STANDARD_COPY_LOOP(S32, from[i*3+0], from[i*3+1], from[i*3+2], 0xFF); break;
	case rrPixelFormat_4_S32:		memcpy(color, ptr, (SINTa)count * sizeof(rrColor4I)); break;

	case rrPixelFormat_3_S16:		STANDARD_COPY_LOOP(S16, from[i*3+0], from[i*3+1], from[i*3+2], 0x7FFF); break;

	case rrPixelFormat_R5G6B5:
	{
		for LOOP(i,count)
		{
			rrColor565Bits c565;
			c565.w = RR_GET16_LE(ptr + i*2);
			rrColor32BGRA cdw = rrColor565Bits_UnQuantize(c565);
			color[i].r = cdw.u.r;
			color[i].g = cdw.u.g;
			color[i].b = cdw.u.b;
			color[i].a = cdw.u.a;
		}
		break;
	}
	
	case rrPixelFormat_A1R5G5B5:
	{
		for LOOP(i,count)
		{
			rrColor1555Bits cc;
			cc.w = RR_GET16_LE(ptr + i*2);
			rrColor32BGRA cdw = rrColor1555Bits_UnQuantize(cc);
			color[i].r = cdw.u.r;
			color[i].g = cdw.u.g;
			color[i].b = cdw.u.b;
			color[i].a = cdw.u.a;
		}
		break;
	}
	
	case rrPixelFormat_1_F16:
	case rrPixelFormat_2_F16:
	case rrPixelFormat_3_F16:
	case rrPixelFormat_4_F16:
	case rrPixelFormat_1_F32:
	case rrPixelFormat_2_F32:
	case rrPixelFormat_3_F32:
	case rrPixelFormat_4_F32:
	case rrPixelFormat_RGBE_8888:
	case rrPixelFormat_RGBE_9995:
	{
		rrPixelFormat_GetColors4F(ptr,format,(rrColor4F *)color,count);
		Colors4FtoI_Linear_Multi(color,(const rrColor4F *)color,count);
		break;
	}
	
	default:
		RR_ASSERT_FAILURE("rrPixelFormat_GetColors4I for format");
		break;
	}

#undef STANDARD_COPY_LOOP
}

#ifdef RRPIXEL_GENERIC_SIMD

static RADFORCEINLINE Vec128_U8 rrColor4I_to_4U8(const rrColor4I * color)
{
	const Vec128_S32 v32 = Vec128_S32::loadu(color);
	const Vec128_S16 v16 = v32.to_s16_sat(v32);
	const Vec128_U8 v8 = v16.to_u8_sat(v16);

	return v8;
}

static RADFORCEINLINE Vec128_U8 rrColor4I_to_4U8_4x(const rrColor4I * color)
{
	const Vec128_S32 v32_0 = Vec128_S32::loadu(color + 0);
	const Vec128_S32 v32_1 = Vec128_S32::loadu(color + 1);
	const Vec128_S32 v32_2 = Vec128_S32::loadu(color + 2);
	const Vec128_S32 v32_3 = Vec128_S32::loadu(color + 3);

	// Clamp down to 16 bits per component
	const Vec128_S16 v16_01 = v32_0.to_s16_sat(v32_1);
	const Vec128_S16 v16_23 = v32_2.to_s16_sat(v32_3);

	// Then down to 8
	const Vec128_U8 v8 = v16_01.to_u8_sat(v16_23);

	return v8;
}

static RADINLINE void rrPixelFormat_PutColors4I_1U8(U8 * ptr, S32 count32, const rrColor4I * color)
{
	const Vec128_S8 shuffle { 0,4,8,12, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1 };
	const SINTa count = count32;
	SINTa i;

	// Work in groups of 4
	for (i = 0; i <= count - 4; i += 4)
	{
		const Vec128_U8 v8 = rrColor4I_to_4U8_4x(color + i);
		v8.shuf(shuffle).storeu_lo32(ptr + i);
	}

	for (; i < count; i++)
	{
		U8 temp[4];

		const Vec128_U8 v8 = rrColor4I_to_4U8(color + i);
		v8.shuf(shuffle).storeu_lo32(temp);
		ptr[i] = temp[0];
	}
}

static RADINLINE void rrPixelFormat_PutColors4I_2U8(U8 * ptr, S32 count32, const rrColor4I * color)
{
	const Vec128_S8 shuffle { 0,1, 4,5, 8,9, 12,13, -1,-1,-1,-1, -1,-1,-1,-1 };
	const SINTa count = count32;
	SINTa i;

	// Work in groups of 4
	for (i = 0; i <= count - 4; i += 4)
	{
		const Vec128_U8 v8 = rrColor4I_to_4U8_4x(color + i);
		v8.shuf(shuffle).storeu_lo64(ptr + i*2);
	}

	for (; i < count; i++)
	{
		U8 temp[4];

		const Vec128_U8 v8 = rrColor4I_to_4U8(color + i);
		v8.shuf(shuffle).storeu_lo32(temp);
		memcpy(ptr + i*2, temp, 2);
	}
}

static void rrPixelFormat_PutColors4I_3U8(U8 * ptr, S32 count32, const rrColor4I * color, bool is_rgb)
{
	const Vec128_S8 shuffle_rgb { 0,1,2, 4,5,6, 8,9,10, 12,13,14, -1,-1,-1,-1 };
	const Vec128_S8 shuffle_bgr { 2,1,0, 6,5,4, 10,9,8, 14,13,12, -1,-1,-1,-1 };
	const Vec128_S8 shuffle = is_rgb ? shuffle_rgb : shuffle_bgr;
	const SINTa count = count32;
	SINTa i;

	// Work in groups of 4, but we write 16 not 12 bytes, so we need one pixel's worth of extra slop
	for (i = 0; i <= count - (4 + 1); i += 4)
	{
		const Vec128_U8 v8 = rrColor4I_to_4U8_4x(color + i);
		v8.shuf(shuffle).storeu(ptr + i*3);
	}

	for (; i < count; i++)
	{
		U8 temp[4];

		const Vec128_U8 v8 = rrColor4I_to_4U8(color + i);
		v8.shuf(shuffle).storeu_lo32(temp);
		memcpy(ptr + i*3, temp, 3);
	}
}

static void rrPixelFormat_PutColors4I_4U8(U8 * ptr, S32 count32, const rrColor4I * color, bool is_rgb, bool force_alpha_255)
{
	const Vec128_S8 shuffle_rgba { 0,1,2,3, 4,5,6,7, 8,9,10,11, 12,13,14,15 };
	const Vec128_S8 shuffle_bgra { 2,1,0,3, 6,5,4,7, 10,9,8,11, 14,13,12,15 };
	const Vec128_S8 shuffle = is_rgb ? shuffle_rgba : shuffle_bgra;
	const Vec128_U8 constant_a = force_alpha_255 ? Vec128_U8::repeat4(0, 0, 0, 0xff) : Vec128_U8(0);
	const SINTa count = count32;
	SINTa i;

	// Work in groups of 4 pixels
	for (i = 0; i <= count - 4; i += 4)
	{
		// Convert 4 pixels and apply alpha override
		const Vec128_U8 v8 = rrColor4I_to_4U8_4x(color + i) | constant_a;

		// Finally shuffle and store
		v8.shuf(shuffle).storeu(ptr + i*4);
	}

	// Tail loop for last few pixels
	for (; i < count; i++)
	{
		const Vec128_U8 v8 = rrColor4I_to_4U8(color + i) | constant_a;
		v8.shuf(shuffle).storeu_lo32(ptr + i*4);
	}
}

static void rrPixelFormat_PutColors4I_1U16(U8 * ptr, S32 count32, const rrColor4I * color)
{
	U16 * to = (U16 *)ptr;
	SINTa count = count32;
	SINTa i;

	// groups of 4
	for (i = 0; i < (count & ~3); i += 4)
	{
		// Load 4 values
		Vec128_S32 v01 = Vec128_S32::loadu_lo32(&color[i + 0].r).insert<1>(color[i + 1].r);
		Vec128_S32 v23 = Vec128_S32::loadu_lo32(&color[i + 2].r).insert<1>(color[i + 3].r);
		Vec128_S32 v = v01.unpack_lo64(v23);

		// Pack 32b -> U16
		Vec128_U16 v16 = v.to_u16_sat(v);
		v16.storeu_lo64(to + i);
	}

	// Tail loop for last few pixels is just scalar
	for (; i < count; i++)
		to[i] = (U16)RR_CLAMP(color[i].r, 0, 0xffff);
}

static void rrPixelFormat_PutColors4I_2U16(U8 * ptr, S32 count32, const rrColor4I * color)
{
	U16 * to = (U16 *)ptr;
	SINTa count = count32;
	SINTa i;

	// groups of 2
	for (i = 0; i < (count & ~1); i += 1)
	{
		Vec128_S32 v0 = Vec128_S32::loadu_lo64(color + i + 0);
		Vec128_S32 v1 = Vec128_S32::loadu_lo64(color + i + 1);
		Vec128_S32 v = v0.unpack_lo64(v1);

		// Pack 32b -> U16
		Vec128_U16 v16 = v.to_u16_sat(v);
		v16.storeu_lo64(to + i*2);
	}

	// Tail for last pixel is scalar
	if (i < count)
	{
		RR_ASSERT(i + 1 == count);
		to[i*2 + 0] = (U16)RR_CLAMP(color[i].r, 0, 0xffff);
		to[i*2 + 1] = (U16)RR_CLAMP(color[i].g, 0, 0xffff);
	}
}

#endif

void rrPixelFormat_PutColors4I( U8 * ptr, rrPixelFormat format, const rrColor4I * color, S32 count )
{
	#define CLAMP_U16(x) ((U16)RR_CLAMP(x, 0, 0xffff))
	#define CLAMP_S8(x) ((S8)RR_CLAMP(x, -128, 127))
	#define CLAMP_S16(x) ((S16)RR_CLAMP(x, -0x8000, 0x7fff))

	#define LOOP_1C(typ, a) { \
		typ * to = (typ *)ptr; \
		for LOOP(i,count) { \
			to[i] = a; \
		} \
	}
	#define LOOP_2C(typ, a, b) { \
		typ * to = (typ *)ptr; \
		for LOOP(i,count) { \
			to[i*2+0] = a; \
			to[i*2+1] = b; \
		} \
	}
	#define LOOP_3C(typ, a, b, c) { \
		typ * to = (typ *)ptr; \
		for LOOP(i,count) { \
			to[i*3+0] = a; \
			to[i*3+1] = b; \
			to[i*3+2] = c; \
		} \
	}
	#define LOOP_4C(typ, a, b, c, d) { \
		typ * to = (typ *)ptr; \
		for LOOP(i,count) { \
			to[i*4+0] = a; \
			to[i*4+1] = b; \
			to[i*4+2] = c; \
			to[i*4+3] = d; \
		} \
	}

	switch(format)
	{
#ifdef RRPIXEL_GENERIC_SIMD
	case rrPixelFormat_1_U8:		rrPixelFormat_PutColors4I_1U8(ptr, count, color); break;
	case rrPixelFormat_2_U8:		rrPixelFormat_PutColors4I_2U8(ptr, count, color); break;
	case rrPixelFormat_B8G8R8:		rrPixelFormat_PutColors4I_3U8(ptr, count, color, false); break;
	case rrPixelFormat_R8G8B8:		rrPixelFormat_PutColors4I_3U8(ptr, count, color, true); break;
	case rrPixelFormat_B8G8R8x8:	rrPixelFormat_PutColors4I_4U8(ptr, count, color, false, true); break;
	case rrPixelFormat_B8G8R8A8:	rrPixelFormat_PutColors4I_4U8(ptr, count, color, false, false); break;
	case rrPixelFormat_R8G8B8x8:	rrPixelFormat_PutColors4I_4U8(ptr, count, color, true, true); break;
	case rrPixelFormat_R8G8B8A8:	rrPixelFormat_PutColors4I_4U8(ptr, count, color, true, false); break;

	case rrPixelFormat_1_U16:		rrPixelFormat_PutColors4I_1U16(ptr, count, color); break;
	case rrPixelFormat_2_U16:		rrPixelFormat_PutColors4I_2U16(ptr, count, color); break;
#else
	case rrPixelFormat_1_U8:		LOOP_1C(U8, RR_CLAMP_U8(color[i].r)); break;
	case rrPixelFormat_2_U8:		LOOP_2C(U8, RR_CLAMP_U8(color[i].r), RR_CLAMP_U8(color[i].g)); break;
	case rrPixelFormat_B8G8R8:		LOOP_3C(U8, RR_CLAMP_U8(color[i].b), RR_CLAMP_U8(color[i].g), RR_CLAMP_U8(color[i].r)); break;
	case rrPixelFormat_R8G8B8:		LOOP_3C(U8, RR_CLAMP_U8(color[i].r), RR_CLAMP_U8(color[i].g), RR_CLAMP_U8(color[i].b)); break;
	case rrPixelFormat_B8G8R8x8:	LOOP_4C(U8, RR_CLAMP_U8(color[i].b), RR_CLAMP_U8(color[i].g), RR_CLAMP_U8(color[i].r), 255); break;
	case rrPixelFormat_B8G8R8A8:	LOOP_4C(U8, RR_CLAMP_U8(color[i].b), RR_CLAMP_U8(color[i].g), RR_CLAMP_U8(color[i].r), RR_CLAMP_U8(color[i].a)); break;
	case rrPixelFormat_R8G8B8x8:	LOOP_4C(U8, RR_CLAMP_U8(color[i].r), RR_CLAMP_U8(color[i].g), RR_CLAMP_U8(color[i].b), 255); break;
	case rrPixelFormat_R8G8B8A8:	LOOP_4C(U8, RR_CLAMP_U8(color[i].r), RR_CLAMP_U8(color[i].g), RR_CLAMP_U8(color[i].b), RR_CLAMP_U8(color[i].a)); break;

	// NOTE: U16 uses RGBA channel order since it's (for our purposes) a D3D10+ format
	case rrPixelFormat_1_U16:		LOOP_1C(U16, CLAMP_U16(color[i].r)); break;
	case rrPixelFormat_2_U16:		LOOP_2C(U16, CLAMP_U16(color[i].r), CLAMP_U16(color[i].g)); break;
#endif
		
	case rrPixelFormat_R5G6B5:
	{
		for LOOP(i,count)
		{
			rrColor32BGRA cdw;
			cdw.u.r = RR_CLAMP_U8( color[i].r );
			cdw.u.g = RR_CLAMP_U8( color[i].g );
			cdw.u.b = RR_CLAMP_U8( color[i].b );
			cdw.u.a = RR_CLAMP_U8( color[i].a );
			rrColor565Bits cc = rrColor565Bits_Quantize(cdw);
			RR_PUT16_LE(ptr + i*2,cc.w);
		}
		break;
	}
	
	case rrPixelFormat_A1R5G5B5:
	{
		for LOOP(i,count)
		{
			rrColor32BGRA cdw;
			cdw.u.r = RR_CLAMP_U8( color[i].r );
			cdw.u.g = RR_CLAMP_U8( color[i].g );
			cdw.u.b = RR_CLAMP_U8( color[i].b );
			cdw.u.a = RR_CLAMP_U8( color[i].a );
			rrColor1555Bits cc = rrColor1555Bits_Quantize(cdw);
			RR_PUT16_LE(ptr + i*2,cc.w);
		}
		break;
	}
	
	case rrPixelFormat_1_S32:		LOOP_1C(S32, color[i].r); break;
	case rrPixelFormat_2_S32:		LOOP_2C(S32, color[i].r, color[i].g); break;
	case rrPixelFormat_3_S32:		LOOP_3C(S32, color[i].r, color[i].g, color[i].b); break;
	case rrPixelFormat_4_S32:		memcpy(ptr, color, (SINTa)count * sizeof(rrColor4I)); break;

	case rrPixelFormat_3_U16:		LOOP_3C(U16, CLAMP_U16(color[i].r), CLAMP_U16(color[i].g), CLAMP_U16(color[i].b)); break;
	case rrPixelFormat_4_U16:		LOOP_4C(U16, CLAMP_U16(color[i].r), CLAMP_U16(color[i].g), CLAMP_U16(color[i].b), CLAMP_U16(color[i].a)); break;

	// NOTE: S8 uses RGBA channel order since it's (for our purposes) a D3D10+ format
	case rrPixelFormat_1_S8:		LOOP_1C(S8, CLAMP_S8(color[i].r)); break;
	case rrPixelFormat_2_S8:		LOOP_2C(S8, CLAMP_S8(color[i].r), CLAMP_S8(color[i].g)); break;
	case rrPixelFormat_3_S8:		LOOP_3C(S8, CLAMP_S8(color[i].r), CLAMP_S8(color[i].g), CLAMP_S8(color[i].b)); break;
	case rrPixelFormat_4_S8:		LOOP_4C(S8, CLAMP_S8(color[i].r), CLAMP_S8(color[i].g), CLAMP_S8(color[i].b), CLAMP_S8(color[i].a)); break;

	// NOTE: S16 uses RGBA channel order since it's (for our purposes) a D3D10+ format
	case rrPixelFormat_1_S16:		LOOP_1C(S16, CLAMP_S16(color[i].r)); break;
	case rrPixelFormat_2_S16:		LOOP_2C(S16, CLAMP_S16(color[i].r), CLAMP_S16(color[i].g)); break;
	case rrPixelFormat_3_S16:		LOOP_3C(S16, CLAMP_S16(color[i].r), CLAMP_S16(color[i].g), CLAMP_S16(color[i].b)); break;
	case rrPixelFormat_4_S16:		LOOP_4C(S16, CLAMP_S16(color[i].r), CLAMP_S16(color[i].g), CLAMP_S16(color[i].b), CLAMP_S16(color[i].a)); break;

	case rrPixelFormat_1_F16:
	case rrPixelFormat_2_F16:
	case rrPixelFormat_3_F16:
	case rrPixelFormat_4_F16:
	case rrPixelFormat_1_F32:
	case rrPixelFormat_2_F32:
	case rrPixelFormat_3_F32:
	case rrPixelFormat_4_F32:
	case rrPixelFormat_RGBE_8888:
	case rrPixelFormat_RGBE_9995:
		{
			const int CHUNK_SIZE = 64;
			rrColor4F cf[CHUNK_SIZE];
			const rrPixelFormatInfo * pfi = rrPixelFormat_GetInfo(format);
			for (int base = 0; base < count; base += CHUNK_SIZE)
			{
				int c = RR_MIN(count - base, CHUNK_SIZE);
				Colors4ItoF_Linear_Multi(cf, &color[base], c);
				rrPixelFormat_PutColors4F(ptr,format,cf,c);
				ptr += pfi->bytesPerPixel * c;
			}
		}
		break;
	
	default:
		RR_ASSERT_FAILURE("rrPixelFormat_PutColors4I for format");
		break;
	}

#undef CLAMP_U16
#undef CLAMP_S8
#undef CLAMP_S16
#undef LOOP_1C
#undef LOOP_2C
#undef LOOP_3C
#undef LOOP_4C
}

void rrPixelFormat_GetColors4F( const U8 * ptr, rrPixelFormat format, rrColor4F * color, S32 count )
{
	// you must write every field of [color]

	switch(format)
	{
	case rrPixelFormat_1_F16:
		{
		const rrFP16 * from = (const rrFP16 *)ptr;
		for LOOP(i,count)
		{
			color[i].r = rrFP16_GetFloat(&from[i]);
			color[i].g = color[i].r;
			color[i].b = color[i].r;
			color[i].a = 1.f;
		}
		}
		break;
	case rrPixelFormat_2_F16:
		{
		const rrFP16 * from = (const rrFP16 *)ptr;
		for LOOP(i,count)
		{
			color[i].r = rrFP16_GetFloat(&from[i*2+0]);
			color[i].g = rrFP16_GetFloat(&from[i*2+1]);
			color[i].b = 0.f;
			color[i].a = 1.f;
		}
		}
		break;
	case rrPixelFormat_3_F16:
		{
		const rrFP16 * from = (const rrFP16 *)ptr;
		for LOOP(i,count)
		{
			color[i].r = rrFP16_GetFloat(&from[i*3+0]);
			color[i].g = rrFP16_GetFloat(&from[i*3+1]);
			color[i].b = rrFP16_GetFloat(&from[i*3+2]);
			color[i].a = 1.f;
		}
		}
		break;
	case rrPixelFormat_4_F16:
		{
		const rrFP16 * from = (const rrFP16 *)ptr;
		for LOOP(i,count)
		{
			color[i].r = rrFP16_GetFloat(&from[i*4+0]);
			color[i].g = rrFP16_GetFloat(&from[i*4+1]);
			color[i].b = rrFP16_GetFloat(&from[i*4+2]);
			color[i].a = rrFP16_GetFloat(&from[i*4+3]);
		}
		}
		break;
		
	case rrPixelFormat_1_F32:
		{
		const F32 * from = (const F32 *)ptr;
#ifdef RRPIXEL_GENERIC_SIMD
		// NOTE(fg): could save a bit more work with more unroll but this is simplest
		VecF32x4 one_in_alpha(0.0f, 0.0f, 0.0f, 1.0f);
		for LOOP(i,count)
		{
			VecF32x4 v = VecF32x4::load_scalar(&from[i]);
			v = v.xxxw(); // broadcast load value to first 3 lanes, keep 0 in 4th lane
			v |= one_in_alpha; // insert a=1 constant
			v.storeu(&color[i].r);
		}
#else
		for LOOP(i,count)
		{
			F32 v = from[i];
			color[i].r = v;
			color[i].g = v;
			color[i].b = v;
			color[i].a = 1.f;
		}
#endif
		}
		break;
		
	case rrPixelFormat_2_F32:
		{
		const F32 * from = (const F32 *)ptr;
#ifdef RRPIXEL_GENERIC_SIMD
		// NOTE(fg); could save a bit of work here by unrolling 2x and doing single
		// 4-lane wide loads then shuffle to form two pixels worth of results, but this is not bad
		VecF32x4 const_ba(0.0f, 0.0f, 0.0f, 1.0f); // 0.0f / 1.0f constants for b/a channels
		for LOOP(i,count)
		{
			VecF32x4 v = VecF32x4::load_pair(from + i*2);
			v |= const_ba; // insert a=1 constant (b=0 we already have)
			v.storeu(&color[i].r);
		}
#else
		for LOOP(i,count)
		{
			color[i].r = from[i*2+0];
			color[i].g = from[i*2+1];
			color[i].b = 0.f;
			color[i].a = 1.f;
		}
#endif
		}
		break;
		
	case rrPixelFormat_3_F32:
		{
		const F32 * from = (const F32 *)ptr;
		for LOOP(i,count)
		{
			color[i].r = from[i*3+0];
			color[i].g = from[i*3+1];
			color[i].b = from[i*3+2];
			color[i].a = 1.f;
		}
		}
		break;
		
	case rrPixelFormat_4_F32:
		memcpy(color, ptr, count * sizeof(rrColor4F));
		break;
	
	case rrPixelFormat_RGBE_8888:
		{
		for LOOP(i,count)
			rrColor4F_From_RGBE_8888(&color[i],ptr + i*4);
		}
		break;
	
	case rrPixelFormat_RGBE_9995:
		{
		const U32 * dbw = (const U32 *)ptr;
		for LOOP(i,count)
			rrColor4F_From_RGBE_9995(&color[i],dbw[i]);
		}
		break;
		
	default: // fallback to ints :
		{
		rrPixelFormat_GetColors4I(ptr,format,(rrColor4I *)color,count);
		Colors4ItoF_Linear_Multi(color,(rrColor4I *)color,count);
		}
		break;
	}
}

void rrPixelFormat_PutColors4F( U8 * ptr, rrPixelFormat format, const rrColor4F * color, S32 count )
{
	switch(format)
	{

	case rrPixelFormat_1_F16:
		{
		rrFP16 * to = (rrFP16 *)ptr;
		for LOOP(i,count)
			rrFP16_SetFloat(&to[i],color[i].r);
		}
		break;
	case rrPixelFormat_2_F16:
		{
		rrFP16 * to = (rrFP16 *)ptr;
		for LOOP(i,count)
		{
			rrFP16_SetFloat(&to[i*2+0],color[i].r);
			rrFP16_SetFloat(&to[i*2+1],color[i].g);
		}
		}
		break;
	case rrPixelFormat_3_F16:
		{
		rrFP16 * to = (rrFP16 *)ptr;
		for LOOP(i,count)
		{
			rrFP16_SetFloat(&to[i*3+0],color[i].r);
			rrFP16_SetFloat(&to[i*3+1],color[i].g);
			rrFP16_SetFloat(&to[i*3+2],color[i].b);
		}
		}
		break;
	case rrPixelFormat_4_F16:
		{
		rrFP16 * to = (rrFP16 *)ptr;
		for LOOP(i,count)
		{
			rrFP16_SetFloat(&to[i*4+0],color[i].r);
			rrFP16_SetFloat(&to[i*4+1],color[i].g);
			rrFP16_SetFloat(&to[i*4+2],color[i].b);
			rrFP16_SetFloat(&to[i*4+3],color[i].a);
		}
		}
		break;
		
	case rrPixelFormat_1_F32:
		{
		F32 * to = (F32 *)ptr;
		for LOOP(i,count)
			to[i] = color[i].r;
		}
		break;
		
	case rrPixelFormat_2_F32:
		{
		F32 * to = (F32 *)ptr;
		for LOOP(i,count)
		{
			to[i*2+0] = color[i].r;
			to[i*2+1] = color[i].g;
		}
		}
		break;
		
	case rrPixelFormat_3_F32:
		{
		F32 * to = (F32 *)ptr;
		for LOOP(i,count)
		{
			to[i*3+0] = color[i].r;
			to[i*3+1] = color[i].g;
			to[i*3+2] = color[i].b;
		}
		}
		break;
		
	case rrPixelFormat_4_F32:
		{
		memcpy(ptr,color,count * sizeof(rrColor4F));
		}
		break;
	
	case rrPixelFormat_RGBE_8888:
		{
		for LOOP(i,count)
			rrColor4F_To_RGBE_8888(ptr+i*4,&color[i]);
		}
		break;
		
	case rrPixelFormat_RGBE_9995:
		{
		U32 * dbw = (U32 *)ptr;
		for LOOP(i,count)
			rrColor4F_To_RGBE_9995(&dbw[i],&color[i]);
		}
		break;
		
	default: // fallback to ints :
		{
			const int CHUNK_SIZE = 64;
			rrColor4I ci[CHUNK_SIZE];
			const rrPixelFormatInfo * pfi = rrPixelFormat_GetInfo(format);
			for (int base = 0; base < count; base += CHUNK_SIZE)
			{
				int c = RR_MIN(count - base, CHUNK_SIZE);
				Colors4FtoI_Linear_Multi(ci,&color[base],c);
				rrPixelFormat_PutColors4I(ptr,format,ci,c);
				ptr += pfi->bytesPerPixel * c;
			}
		}
		break;
	}
}

//---------------------------------------------------------------------------------

RR_NAMESPACE_END
