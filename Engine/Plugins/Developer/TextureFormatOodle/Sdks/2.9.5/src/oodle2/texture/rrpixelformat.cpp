// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrpixelformat.h"
#include "rrmath.h"
#include "rrcolor.h"
//#include "cbrad/cbradutil.h"
#include "rrbytepacking.h"
#include "vec128.inl"

RR_NAMESPACE_START

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

/*
// WTF no, stop this
SINTa	rrPixelFormat_MakeStride_Aligned(rrPixelFormat format, int w)
{
	SINTa bytes = rrPixelFormat_MakeStride_Minimum(format,w);
	
	// this is the aligning needed for BMP :
	bytes = rrAlignUpA(bytes,4);	
	// align stride to 16 for SSE :
	//bytes = rrAlignUp(bytes,16);	
	
	return bytes;
}
*/

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
static const rrPixelFormatInfo rrPixelFormat_R5G5B5A1_Info = INTU_INFO(4,2);
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
	CASE(rrPixelFormat_R5G5B5A1)\
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

/*
// just use RR_PIXELFORMAT_COUNT
int rrPixelFormat_GetCount()
{
	return c_formats_count;
}
*/

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

	if ( opts & rrDXTCOptions_BC345_PreserveExtremes )
	{
		if ( pf == rrPixelFormat_BC3 ) return "BC3-PE";
		if ( pf == rrPixelFormat_BC4U ) return "BC4U-PE";
		if ( pf == rrPixelFormat_BC4S ) return "BC4S-PE";
		if ( pf == rrPixelFormat_BC5U ) return "BC5U-PE";
		if ( pf == rrPixelFormat_BC5S ) return "BC5S-PE";
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

// NOTE: dest == src is legal
static void Colors4ItoF_Linear_Multi( rrColor4F * dest, const rrColor4I * src, S32 count )
{
#ifdef __RADSSE2__
	int i;

	// groups of 4
	for (i = 0; i < (count & ~3); i += 4)
	{
		Vec128 col_int0 = load128u(&src[i + 0].r);
		Vec128 col_int1 = load128u(&src[i + 1].r);
		Vec128 col_int2 = load128u(&src[i + 2].r);
		Vec128 col_int3 = load128u(&src[i + 3].r);
		VecF32x4::from_int32(col_int0).storeu(&dest[i + 0].r);
		VecF32x4::from_int32(col_int1).storeu(&dest[i + 1].r);
		VecF32x4::from_int32(col_int2).storeu(&dest[i + 2].r);
		VecF32x4::from_int32(col_int3).storeu(&dest[i + 3].r);
	}

	for (; i < count; ++i)
	{
		Vec128 col_int = load128u(&src[i].r);
		VecF32x4::from_int32(col_int).storeu(&dest[i].r);
	}
#else
	for LOOP(i,count)
		rrColor4_I_to_F_Linear(&dest[i],&src[i]);
#endif
}

#ifdef __RADSSE2__

static inline void rrPixelFormat_GetColors4I_1U8(rrColor4I * color, S32 count, const U8 * ptr)
{
	const U8 * from = ptr;
	Vec128 constant_a = _mm_setr_epi32(0, 0, 0, 0xff);
	Vec128 constant_a_4x = _mm_setr_epi32(0, 0, 0, -1);
	Vec128 mask_8b = _mm_set1_epi32(0xff);
	int i;

	// groups of 4
	for (i = 0; i < (count & ~3); i += 4)
	{
		Vec128 v = load32u(from + i); // load 4 pixels worth
		v = shuffle32<0,0,0,3>(v); // broadcast value to RGB (leave A alone)
		v = _mm_or_si128(v, constant_a_4x); // fill alpha channel with 0xff

		// now each lane holds 4 pixels worth of data, store them all
		store128u(color + i + 0, _mm_and_si128(v, mask_8b));
		store128u(color + i + 1, _mm_and_si128(_mm_srli_epi32(v, 8), mask_8b));
		store128u(color + i + 2, _mm_and_si128(_mm_srli_epi32(v, 16), mask_8b));
		store128u(color + i + 3, _mm_srli_epi32(v, 24));
	}

	// tail
	for (; i < count; ++i)
	{
		Vec128 v = _mm_cvtsi32_si128(from[i]);
		v = shuffle32<0,0,0,3>(v); // broadcast value to RGB
		v = _mm_or_si128(v, constant_a);
		store128u(color + i, v);
	}
}

static inline void rrPixelFormat_GetColors4I_2U8(rrColor4I * color, S32 count, const U8 * ptr)
{
	const U16 * from = (const U16 *)ptr;
	Vec128 constant_a = _mm_setr_epi32(0, 0, 0, 0xff);
	Vec128 constant_a_2x = _mm_setr_epi32(0, 0xff, 0, 0xff);
	int i;

	// groups of 2
	for (i = 0; i < (count & ~1); i += 2)
	{
		Vec128 v = load32u(from + i); // load 2 pixels worth

		// unpack to 32-bit lanes
		v = zext8to16_lo(v);
		v = zext16to32_lo(v);

		// store the halves with constant b/a inserted
		store128u(color + i + 0, _mm_unpacklo_epi64(v, constant_a_2x));
		store128u(color + i + 1, _mm_unpackhi_epi64(v, constant_a_2x));
	}

	// tail element if any
	if (i < count)
	{
		// load 16-bit value (two channels)
		Vec128 v = _mm_cvtsi32_si128(from[i]);

		// unpack to 32-bit lanes
		v = zext8to16_lo(v);
		v = zext16to32_lo(v);

		// add in constant a
		v = _mm_or_si128(v, constant_a);
		store128u(color + i, v);
	}
}

static inline void rrPixelFormat_GetColors4I_1U16(rrColor4I * color, S32 count, const U8 * ptr)
{
	const U16 * from = (const U16 *)ptr;
	Vec128 constant_a = _mm_setr_epi32(0, 0, 0, 0xffff);
	Vec128 mask_r = _mm_setr_epi32(0xffff, 0, 0, 0);
	int i;

	// NOTE: unlike U8, our standard conversion here does not broadcast r, just expands to (r,0,0,1)

	// groups of 2
	for (i = 0; i < (count & ~1); i += 2)
	{
		Vec128 v = load32u(from + i); // load 2 pixels worth

		// put the two result vectors in place using shifts+masking, then insert the constant a
		store128u(color + i + 0, _mm_or_si128(_mm_and_si128(v, mask_r), constant_a));
		store128u(color + i + 1, _mm_or_si128(_mm_srli_epi32(v, 16), constant_a));
	}

	// tail element if any
	if (i < count)
	{
		Vec128 v = _mm_cvtsi32_si128(from[i]);
		v = _mm_or_si128(v, constant_a);
		store128u(color + i, v);
	}
}

static inline void rrPixelFormat_GetColors4I_2U16(rrColor4I * color, S32 count, const U8 * ptr)
{
	const U32 * from = (const U32 *)ptr;
	Vec128 constant_a = _mm_setr_epi32(0, 0, 0, 0xffff);
	for LOOP(i,count)
	{
		// load 32-bit value (two channels)
		Vec128 v = _mm_cvtsi32_si128(from[i]);

		// unpack to 32-bit lanes
		v = zext16to32_lo(v);

		// insert constant a
		v = _mm_or_si128(v, constant_a);
		store128u(color + i, v);
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
#ifdef __RADSSE2__
	case rrPixelFormat_1_U8:		rrPixelFormat_GetColors4I_1U8(color, count, ptr); break;
	case rrPixelFormat_R8G8:		rrPixelFormat_GetColors4I_2U8(color, count, ptr); break;
#else
	case rrPixelFormat_1_U8:		STANDARD_COPY_LOOP(U8, from[i], from[i], from[i], 0xFF); break;
	case rrPixelFormat_R8G8:		STANDARD_COPY_LOOP(U8, from[i*2+0], from[i*2+1], 0, 0xFF); break;
#endif
	case rrPixelFormat_B8G8R8:		STANDARD_COPY_LOOP(U8, from[i*3+2], from[i*3+1], from[i*3+0], 0xFF); break;
	case rrPixelFormat_B8G8R8x8:	STANDARD_COPY_LOOP(U8, from[i*4+2], from[i*4+1], from[i*4+0], 0xFF); break;
	case rrPixelFormat_R8G8B8:		STANDARD_COPY_LOOP(U8, from[i*3+0], from[i*3+1], from[i*3+2], 0xFF); break;
	case rrPixelFormat_R8G8B8x8:	STANDARD_COPY_LOOP(U8, from[i*4+0], from[i*4+1], from[i*4+2], 0xFF); break;
	case rrPixelFormat_B8G8R8A8:	STANDARD_COPY_LOOP(U8, from[i*4+2], from[i*4+1], from[i*4+0], from[i*4+3]); break;
	case rrPixelFormat_R8G8B8A8:	STANDARD_COPY_LOOP(U8, from[i*4+0], from[i*4+1], from[i*4+2], from[i*4+3]); break;

#ifdef __RADSSE2__
	case rrPixelFormat_1_U16:		rrPixelFormat_GetColors4I_1U16(color, count, ptr); break;
	case rrPixelFormat_2_U16:		rrPixelFormat_GetColors4I_2U16(color, count, ptr); break;
#else
	case rrPixelFormat_1_U16:		STANDARD_COPY_LOOP(U16, from[i], 0, 0, 0xFFFF); break;
	case rrPixelFormat_2_U16:		STANDARD_COPY_LOOP(U16, from[i*2+0], from[i*2+1], 0, 0xFFFF); break;
#endif
	case rrPixelFormat_3_U16:		STANDARD_COPY_LOOP(U16, from[i*3+0], from[i*3+1], from[i*3+2], 0xFFFF); break;
	case rrPixelFormat_4_U16:		STANDARD_COPY_LOOP(U16, from[i*4+0], from[i*4+1], from[i*4+2], from[i*4+3]); break;

	case rrPixelFormat_1_S32:		STANDARD_COPY_LOOP(S32, from[i], 0, 0, 0xFF); break;
	case rrPixelFormat_2_S32:		STANDARD_COPY_LOOP(S32, from[i*2+0], from[i*2+1], 0, 0xFF); break;
	case rrPixelFormat_3_S32:		STANDARD_COPY_LOOP(S32, from[i*3+0], from[i*3+1], from[i*3+2], 0xFF); break;
	case rrPixelFormat_4_S32:		STANDARD_COPY_LOOP(S32, from[i*4+0], from[i*4+1], from[i*4+2], from[i*4+3]); break;

	case rrPixelFormat_1_S8:		STANDARD_COPY_LOOP(S8, from[i], 0, 0, 0x7F); break;
	case rrPixelFormat_2_S8:		STANDARD_COPY_LOOP(S8, from[i*2+0], from[i*2+1], 0, 0x7F); break;
	case rrPixelFormat_3_S8:		STANDARD_COPY_LOOP(S8, from[i*3+0], from[i*3+1], from[i*3+2], 0x7F); break;
	case rrPixelFormat_4_S8:		STANDARD_COPY_LOOP(S8, from[i*4+0], from[i*4+1], from[i*4+2], from[i*4+3]); break;

	case rrPixelFormat_1_S16:		STANDARD_COPY_LOOP(S16, from[i], 0, 0, 0x7FFF); break;
	case rrPixelFormat_2_S16:		STANDARD_COPY_LOOP(S16, from[i*2+0], from[i*2+1], 0, 0x7FFF); break;
	case rrPixelFormat_3_S16:		STANDARD_COPY_LOOP(S16, from[i*3+0], from[i*3+1], from[i*3+2], 0x7FFF); break;
	case rrPixelFormat_4_S16:		STANDARD_COPY_LOOP(S16, from[i*4+0], from[i*4+1], from[i*4+2], from[i*4+3]); break;

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
	
	case rrPixelFormat_R5G5B5A1:
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
		for LOOP(i,count)
		{
			rrColor4F cf = ((rrColor4F *)color)[i];
			rrColor4_F_to_I_Linear(&color[i],&cf);
		}
		break;
	}
	
	default:
		RR_ASSERT_FAILURE("rrPixelFormat_GetColors4I for format");
		break;
	}

#undef STANDARD_COPY_LOOP
}

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
	case rrPixelFormat_1_U8:		LOOP_1C(U8, RR_CLAMP_U8(color[i].r)); break;
	case rrPixelFormat_2_U8:		LOOP_2C(U8, RR_CLAMP_U8(color[i].r), RR_CLAMP_U8(color[i].g)); break;
	case rrPixelFormat_B8G8R8:		LOOP_3C(U8, RR_CLAMP_U8(color[i].b), RR_CLAMP_U8(color[i].g), RR_CLAMP_U8(color[i].r)); break;
	case rrPixelFormat_B8G8R8x8:	LOOP_4C(U8, RR_CLAMP_U8(color[i].b), RR_CLAMP_U8(color[i].g), RR_CLAMP_U8(color[i].r), 255); break;
	case rrPixelFormat_B8G8R8A8:	LOOP_4C(U8, RR_CLAMP_U8(color[i].b), RR_CLAMP_U8(color[i].g), RR_CLAMP_U8(color[i].r), RR_CLAMP_U8(color[i].a)); break;
	case rrPixelFormat_R8G8B8:		LOOP_3C(U8, RR_CLAMP_U8(color[i].r), RR_CLAMP_U8(color[i].g), RR_CLAMP_U8(color[i].b)); break;
	case rrPixelFormat_R8G8B8x8:	LOOP_4C(U8, RR_CLAMP_U8(color[i].r), RR_CLAMP_U8(color[i].g), RR_CLAMP_U8(color[i].b), 255); break;
	case rrPixelFormat_R8G8B8A8:	LOOP_4C(U8, RR_CLAMP_U8(color[i].r), RR_CLAMP_U8(color[i].g), RR_CLAMP_U8(color[i].b), RR_CLAMP_U8(color[i].a)); break;
		
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
	
	case rrPixelFormat_R5G5B5A1:
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
	case rrPixelFormat_4_S32:		LOOP_4C(S32, color[i].r, color[i].g, color[i].b, color[i].a); break;

	// NOTE: U16 uses RGBA channel order since it's (for our purposes) a D3D10+ format
	case rrPixelFormat_1_U16:		LOOP_1C(U16, CLAMP_U16(color[i].r)); break;
	case rrPixelFormat_2_U16:		LOOP_2C(U16, CLAMP_U16(color[i].r), CLAMP_U16(color[i].g)); break;
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
#ifdef __RADSSE2__
		// NOTE(fg): could save a bit more work with more unroll but this is simplest
		VecF32x4 one_in_alpha(0.0f, 0.0f, 0.0f, 1.0f);
		for LOOP(i,count)
		{
			VecF32x4 v = VecF32x4::load_scalar(&from[i]);
			v = v.shuf<0,0,0,3>(); // broadcast load value to first 3 lanes, keep 0 in 4th lane
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
#ifdef __RADSSE2__
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
				for LOOP(i,c)
					rrColor4_F_to_I_Linear(&ci[i],&color[base + i]);
				rrPixelFormat_PutColors4I(ptr,format,ci,c);
				ptr += pfi->bytesPerPixel * c;
			}
		}
		break;
	}
}

//---------------------------------------------------------------------------------

RR_NAMESPACE_END
