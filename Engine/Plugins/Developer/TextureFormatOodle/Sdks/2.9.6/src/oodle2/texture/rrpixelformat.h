// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRRBITMAP_PIXELFORMAT_H__
#define __RADRRBITMAP_PIXELFORMAT_H__

#include "rrbase.h"
#include "rrdxtcenums.h"

/***

PixelFormat :

(# planes) | (plane type) | (plane meaning)

# planes in 1-4  (0-3)

plane type :
	int or float
		8,16,24,32 bits per channel
		
plane meaning :
	SRGB gamma corrected vs. linear
	RGB vs YUV ?
	need these so the blitter can do the conversions ?
	 (I guess you would need other stuff like D3D , UNORM, SNORM, etc;
	 this could be passed as args into the blitter instead of being in the format)

-----------------------

rrPixelFormat names are in byte order
	rrPixelFormat_B8G8R8A8 = B,G,R,A

DXGI formats are in byte order
	DXGI_FORMAT_B8G8R8A8_UNORM is B,G,R,A in bytes
D3D_FMT is reverse-byte order (little-endian dword shift order)
	D3DFMT_A8R8G8B8 = B,G,R,A in bytes
rrColor32BGRA is memory order
	rrColor32BGRA = DXGI_FORMAT_B8G8R8A8_UNORM

rrPixelFormat_4_U8 == rrPixelFormat_B8G8R8A8

WARNING : the newer formats
_S8 and _S16 and _U16
are RGBA order
eg. _4_S8 is RGBA
the old _U8 is BGRA in the generic "_4_U8" form
so that's nice and confusing

****/

RR_NAMESPACE_START

#define RR_PIXELFORMAT_TYPE_MASK	(0xFF)
#define RR_PIXELFORMAT_TYPE_SHIFT	(0)

#define RR_PIXELFORMAT_TYPE_U8		(0)
#define RR_PIXELFORMAT_TYPE_U16		(1)
#define RR_PIXELFORMAT_TYPE_S32		(2)

#define RR_PIXELFORMAT_TYPE_F16		(3)
#define RR_PIXELFORMAT_TYPE_F32		(4)

#define RR_PIXELFORMAT_TYPE_S8	    (5)
#define RR_PIXELFORMAT_TYPE_S16		(6)
// FG 2019-08-05: no actual use case for U32 right now so not bothering with it

// CB changed 2-23-11 : just make the "class" part of type - if type is I16 or something it implies simple class
#define RR_PIXELFORMAT_TYPE_BLOCK4	(100)
#define RR_PIXELFORMAT_TYPE_BITPACK (101)

#define RR_PIXELFORMAT_NUMPLANES_SHIFT	(8)
#define RR_PIXELFORMAT_NUMPLANES_MASK	(0xFF<<RR_PIXELFORMAT_NUMPLANES_SHIFT)

// "ID" is just a way of having multiple formats that are of the same bit pattern but are distinguished
//	  eg. RGBA vs. BGRA 
#define RR_PIXELFORMAT_ID_SHIFT		(16)
#define RR_PIXELFORMAT_ID_MASK		(0xFF<<RR_PIXELFORMAT_ID_SHIFT)

/*
#define RR_PIXELFORMAT_CLASS_SHIFT	(24)
#define RR_PIXELFORMAT_CLASS_MASK	(0xFF<<RR_PIXELFORMAT_CLASS_SHIFT)

#define RR_PIXELFORMAT_CLASS_SIMPLE	 (1<<RR_PIXELFORMAT_CLASS_SHIFT)
#define RR_PIXELFORMAT_CLASS_BLOCK4	 (2<<RR_PIXELFORMAT_CLASS_SHIFT)
#define RR_PIXELFORMAT_CLASS_BITPACK (3<<RR_PIXELFORMAT_CLASS_SHIFT)

#define RR_MAKE_PIXELFORMAT_SIMPLE( numPlanes , planeType )	(enum e_rrPixelFormat)( RR_PIXELFORMAT_CLASS_SIMPLE | ((numPlanes)<<RR_PIXELFORMAT_NUMPLANES_SHIFT) | (planeType) )
#define RR_MAKE_PIXELFORMAT_BLOCK4( id )	(enum e_rrPixelFormat)( RR_PIXELFORMAT_CLASS_BLOCK4 | ((id)<<RR_PIXELFORMAT_ID_SHIFT) )
#define RR_MAKE_PIXELFORMAT_BITPACK( id )	(enum e_rrPixelFormat)( RR_PIXELFORMAT_CLASS_BITPACK | ((id)<<RR_PIXELFORMAT_ID_SHIFT) )

/**/

#define RR_MAKE_PIXELFORMAT_SIMPLE( numPlanes , planeType )	(RRPFTYPE)( ((numPlanes)<<RR_PIXELFORMAT_NUMPLANES_SHIFT) | (planeType) )
#define RR_MAKE_PIXELFORMAT_BLOCK4( id )	(RRPFTYPE)( RR_PIXELFORMAT_TYPE_BLOCK4 | ((id)<<RR_PIXELFORMAT_ID_SHIFT) )
#define RR_MAKE_PIXELFORMAT_BITPACK( id )	(RRPFTYPE)( RR_PIXELFORMAT_TYPE_BITPACK | ((id)<<RR_PIXELFORMAT_ID_SHIFT) )

#define RR_PIXELFORMAT_GET_TYPE(format)			( ((format) & RR_PIXELFORMAT_TYPE_MASK) >> RR_PIXELFORMAT_TYPE_SHIFT )
#define RR_PIXELFORMAT_GET_NUMPLANES(format)	( ((format) & RR_PIXELFORMAT_NUMPLANES_MASK) >> RR_PIXELFORMAT_NUMPLANES_SHIFT )
#define RR_PIXELFORMAT_GET_ID(format)			( ((format) & RR_PIXELFORMAT_ID_MASK) >> RR_PIXELFORMAT_ID_SHIFT )
//#define RR_PIXELFORMAT_GET_CLASS(format)		( ((format) & RR_PIXELFORMAT_CLASS_MASK) )		// >> RR_PIXELFORMAT_CLASS_SHIFT )

#define RRPFTYPE	int
typedef enum e_rrPixelFormat
{
	rrPixelFormat_Invalid = 0,

	// the rrPixelFormat_3_U8 and rrPixelFormat_4_U8 formats are BGRA order
	// 	   (2_U8 is RG)
	//	all other simple _N_ formats are RGBA order


	rrPixelFormat_1_U8 = RR_MAKE_PIXELFORMAT_SIMPLE(1,RR_PIXELFORMAT_TYPE_U8) ,
	rrPixelFormat_2_U8 = RR_MAKE_PIXELFORMAT_SIMPLE(2,RR_PIXELFORMAT_TYPE_U8) ,
	rrPixelFormat_3_U8 = RR_MAKE_PIXELFORMAT_SIMPLE(3,RR_PIXELFORMAT_TYPE_U8) , // BGR
	rrPixelFormat_4_U8 = RR_MAKE_PIXELFORMAT_SIMPLE(4,RR_PIXELFORMAT_TYPE_U8) , // BGRA = rrColor32BGRA = rrPixelFormat_B8G8R8A8

	rrPixelFormat_1_U16 = RR_MAKE_PIXELFORMAT_SIMPLE(1,RR_PIXELFORMAT_TYPE_U16) ,
	rrPixelFormat_2_U16 = RR_MAKE_PIXELFORMAT_SIMPLE(2,RR_PIXELFORMAT_TYPE_U16) ,
	rrPixelFormat_3_U16 = RR_MAKE_PIXELFORMAT_SIMPLE(3,RR_PIXELFORMAT_TYPE_U16) ,
	rrPixelFormat_4_U16 = RR_MAKE_PIXELFORMAT_SIMPLE(4,RR_PIXELFORMAT_TYPE_U16) ,

	rrPixelFormat_1_S8 = RR_MAKE_PIXELFORMAT_SIMPLE(1,RR_PIXELFORMAT_TYPE_S8) ,
	rrPixelFormat_2_S8 = RR_MAKE_PIXELFORMAT_SIMPLE(2,RR_PIXELFORMAT_TYPE_S8) ,
	rrPixelFormat_3_S8 = RR_MAKE_PIXELFORMAT_SIMPLE(3,RR_PIXELFORMAT_TYPE_S8) ,
	rrPixelFormat_4_S8 = RR_MAKE_PIXELFORMAT_SIMPLE(4,RR_PIXELFORMAT_TYPE_S8) ,

	rrPixelFormat_1_S16 = RR_MAKE_PIXELFORMAT_SIMPLE(1,RR_PIXELFORMAT_TYPE_S16) ,
	rrPixelFormat_2_S16 = RR_MAKE_PIXELFORMAT_SIMPLE(2,RR_PIXELFORMAT_TYPE_S16) ,
	rrPixelFormat_3_S16 = RR_MAKE_PIXELFORMAT_SIMPLE(3,RR_PIXELFORMAT_TYPE_S16) ,
	rrPixelFormat_4_S16 = RR_MAKE_PIXELFORMAT_SIMPLE(4,RR_PIXELFORMAT_TYPE_S16) ,

	rrPixelFormat_1_S32 = RR_MAKE_PIXELFORMAT_SIMPLE(1,RR_PIXELFORMAT_TYPE_S32) ,
	rrPixelFormat_2_S32 = RR_MAKE_PIXELFORMAT_SIMPLE(2,RR_PIXELFORMAT_TYPE_S32) ,
	rrPixelFormat_3_S32 = RR_MAKE_PIXELFORMAT_SIMPLE(3,RR_PIXELFORMAT_TYPE_S32) ,
	rrPixelFormat_4_S32 = RR_MAKE_PIXELFORMAT_SIMPLE(4,RR_PIXELFORMAT_TYPE_S32) ,
	
	rrPixelFormat_1_F16 = RR_MAKE_PIXELFORMAT_SIMPLE(1,RR_PIXELFORMAT_TYPE_F16) ,
	rrPixelFormat_2_F16 = RR_MAKE_PIXELFORMAT_SIMPLE(2,RR_PIXELFORMAT_TYPE_F16) ,
	rrPixelFormat_3_F16 = RR_MAKE_PIXELFORMAT_SIMPLE(3,RR_PIXELFORMAT_TYPE_F16) ,
	rrPixelFormat_4_F16 = RR_MAKE_PIXELFORMAT_SIMPLE(4,RR_PIXELFORMAT_TYPE_F16) ,
	
	rrPixelFormat_1_F32 = RR_MAKE_PIXELFORMAT_SIMPLE(1,RR_PIXELFORMAT_TYPE_F32) ,
	rrPixelFormat_2_F32 = RR_MAKE_PIXELFORMAT_SIMPLE(2,RR_PIXELFORMAT_TYPE_F32) ,
	rrPixelFormat_3_F32 = RR_MAKE_PIXELFORMAT_SIMPLE(3,RR_PIXELFORMAT_TYPE_F32) ,
	rrPixelFormat_4_F32 = RR_MAKE_PIXELFORMAT_SIMPLE(4,RR_PIXELFORMAT_TYPE_F32) ,
	
	//---------------------------------------------------------------------
	// BCN :
	//	pixelFormatInfo bytesPerPixel = 0 for these, use bytesPerBlock
	
	rrPixelFormat_BC1  = RR_MAKE_PIXELFORMAT_BLOCK4( 0 ),  // DXT1 : 565 + 1 ; 4bpp
	rrPixelFormat_BC2  = RR_MAKE_PIXELFORMAT_BLOCK4( 1 ),  // DXT3 : 565 + 4 ; 8bpp
	rrPixelFormat_BC3  = RR_MAKE_PIXELFORMAT_BLOCK4( 2 ),  // DXT5 : 565 + 8 ; 8bpp
	rrPixelFormat_BC4U  = RR_MAKE_PIXELFORMAT_BLOCK4( 3 ),  // 4bpp single channel, unsigned
	rrPixelFormat_BC4S  = RR_MAKE_PIXELFORMAT_BLOCK4( 4 ),  // 4bpp single channel, signed
	rrPixelFormat_BC5U  = RR_MAKE_PIXELFORMAT_BLOCK4( 5 ),  // 8bpp two channels, unsigned ; two BC4U's together
	rrPixelFormat_BC5S  = RR_MAKE_PIXELFORMAT_BLOCK4( 6 ),  // 8bpp two channels, signed ; two BC4S's together
	rrPixelFormat_BC6U = RR_MAKE_PIXELFORMAT_BLOCK4( 7 ),  // 8bpp ; unsigned variant
	rrPixelFormat_BC6S = RR_MAKE_PIXELFORMAT_BLOCK4( 8 ),  // 8bpp ; signed variant
	rrPixelFormat_BC7  = RR_MAKE_PIXELFORMAT_BLOCK4( 9 ),  // 8bpp
	
	// aliases :
	rrPixelFormat_DXT1 = rrPixelFormat_BC1,
	rrPixelFormat_DXT3 = rrPixelFormat_BC2,
	rrPixelFormat_DXT5 = rrPixelFormat_BC3,
	
	//---------------------------------------------------------------------
	// R5G6B5 are ordered by how they are shifted into the word
	//	16 bit formats are stored as LITTLE ENDIAN by convention
	
	// @@ get rid of these 565 formats?
	rrPixelFormat_R5G6B5   = RR_MAKE_PIXELFORMAT_BITPACK( 0 ),
	rrPixelFormat_R5G5B5A1 = RR_MAKE_PIXELFORMAT_BITPACK( 1 ),
	
	rrPixelFormat_Palette8 = RR_MAKE_PIXELFORMAT_BITPACK( 2 ),
	
	// @@ get rid of these RGBE formats?
	rrPixelFormat_RGBE_8888 = ( rrPixelFormat_4_U8 | ((2)<<RR_PIXELFORMAT_ID_SHIFT) ),
	rrPixelFormat_RGBE_9995 =  RR_MAKE_PIXELFORMAT_BITPACK( 3 ),	// DXGI_FORMAT_R9G9B9E5_SHAREDEXP
	
	//---------------------------------------------------------------------
	// aliases :
	// my convention here is to list the components in the order they occur as BYTES :
	//	that's the same as DX10+ and OpenGL but different from Dx9- and Windows
		
	rrPixelFormat_R8 = rrPixelFormat_1_U8,
	rrPixelFormat_R8G8 = rrPixelFormat_2_U8,
	rrPixelFormat_B8G8R8 = rrPixelFormat_3_U8,  // BGR in bytes ; as in 24-bit BMP
	rrPixelFormat_R8G8B8 = (rrPixelFormat_3_U8 | ((1)<<RR_PIXELFORMAT_ID_SHIFT) ),  // RGB in bytes
	rrPixelFormat_B8G8R8A8 = rrPixelFormat_4_U8, // BGRA in bytes , ARGB in shifts ; called ARGB in Windows / DX9 , called BGRA in DX10+ ; this is rrColor32BGRA
	rrPixelFormat_R8G8B8A8 = (rrPixelFormat_4_U8 | ((1)<<RR_PIXELFORMAT_ID_SHIFT) ), // RGBA in bytes , ABGR in shifts ; this is rrColor32RGBA
	rrPixelFormat_B8G8R8x8 = (rrPixelFormat_4_U8 | ((3)<<RR_PIXELFORMAT_ID_SHIFT) ), // BGRx in bytes (x is ignored)
	rrPixelFormat_R8G8B8x8 = (rrPixelFormat_4_U8 | ((4)<<RR_PIXELFORMAT_ID_SHIFT) ), // RGBx in bytes (x is ignored)
	
	rrPixelFormat_Color4I = rrPixelFormat_4_S32,
	rrPixelFormat_Color4F = rrPixelFormat_4_F32,
	
	//rrPixelFormat_Color4U16 = rrPixelFormat_4_U16,
	//rrPixelFormat_Color4F16 = rrPixelFormat_4_F16,
	
	//---------------------------------------------------------------------
	
	rrPixelFormat_ForceDWord = 0x7F00000
} rrPixelFormat;
#undef RRPFTYPE
#define RRPFTYPE rrPixelFormat
//---------------------------------------------------------------------------------

#define RR_PIXELFORMAT_COUNT	47

struct rrPixelFormatInfo
{
	int	channels;
	int bytesPerPixel; // 0 if blocked or compressed

	// @@ I do divides by blockSize and it's often just 1; maybe blockSize should be blockSizeShift instead
	int	blockSize; // in pixels
	int bytesPerBlock;
		// bytes Per Pixel = bytesPerBlock / blockSize

	rrbool isFloat;  // isFloat doesn't mean the storage is actually float
					// it tells if you if the data is more naturally float or int
					// so eg. R8G8B8E8 would say isFloat = true
	rrbool isSigned; // is the data meant to be interpreted as signed or not?
};

//---------------------------------------------------------------------------------

const rrPixelFormatInfo * rrPixelFormat_GetInfo(rrPixelFormat format);

// convert format to a dense index :
// index is >= 0 and < RR_PIXELFORMAT_COUNT
int rrPixelFormat_GetIndex(rrPixelFormat format);

const char * rrPixelFormat_GetName(rrPixelFormat format);

// map BCN PF + opts back to an OodleTexPub like name (BC7-RGB and such)
const char * rrPixelFormat_GetName_WithDXTCOptions(rrPixelFormat pf, rrDXTCOptions opts);

int rrPixelFormat_Get4x4BlockSizeBytes(rrPixelFormat format);
SINTa rrPixelFormat_GetSurfaceSizeBytes(rrPixelFormat format, SINTa stride, int height);

U8 * rrPixelFormat_Seek(rrPixelFormat format, U8 * ptr, SINTa stride, int x,int y);

SINTa rrPixelFormat_MakeStride_Minimum(rrPixelFormat format, int w);
//SINTa rrPixelFormat_MakeStride_Aligned(rrPixelFormat format, int w);

// map the BCN formats to just number N ; returns 0 if not block compressed
//	(use IsBlockCompressed if you just want to know if it's IsBC or not)
int rrPixelFormat_GetBCNumber(rrPixelFormat pf);

rrPixelFormat rrPixelFormat_NumberToBCN_U(int bcn);

// get the value of "1.0" in normalized scale for this format:
//	eg. 255 for U8 , 32767 for S16
int rrPixelFormat_GetNormalizedScale(rrPixelFormat fmt);

//---------------------------------------------------------------------------------
// rrPixelFormat_Is... queries
//  don't call on Invalid

static RADINLINE rrbool rrPixelFormat_IsBlockCompressed(rrPixelFormat format)
{
	RR_ASSERT( format != rrPixelFormat_Invalid );
	return ( RR_PIXELFORMAT_GET_TYPE(format) == RR_PIXELFORMAT_TYPE_BLOCK4 );
}

static RADINLINE rrbool rrPixelFormat_IsFloat(rrPixelFormat format)
{
	RR_ASSERT( format != rrPixelFormat_Invalid );
	int type = RR_PIXELFORMAT_GET_TYPE(format);
	return type == RR_PIXELFORMAT_TYPE_F32 || type == RR_PIXELFORMAT_TYPE_F16;
}

// returns whether the given format is one of the int8 ones (U8 or S8)
static RADINLINE rrbool rrPixelFormat_IsInt8(rrPixelFormat format)
{
	RR_ASSERT( format != rrPixelFormat_Invalid );
	int type = RR_PIXELFORMAT_GET_TYPE(format);
	return type == RR_PIXELFORMAT_TYPE_U8 || type == RR_PIXELFORMAT_TYPE_S8;
}

// returns whether the given format is one of the int16 ones (U16 or S16)
static RADINLINE rrbool rrPixelFormat_IsInt16(rrPixelFormat format)
{
	RR_ASSERT( format != rrPixelFormat_Invalid );
	int type = RR_PIXELFORMAT_GET_TYPE(format);
	
	if ( type == RR_PIXELFORMAT_TYPE_U16 || type == RR_PIXELFORMAT_TYPE_S16 )
		return true;
	else if ( type == RR_PIXELFORMAT_TYPE_U8 || type == RR_PIXELFORMAT_TYPE_S8 )
		return false;
	else
	{
		RR_ASSERT_FAILURE("IsInt16 : not 8 or 16 bit?");
		return false;
	}
}

//---------------------------------------------------------------------------------

struct rrColor4I;
struct rrColor4F;

void rrPixelFormat_GetColors4I( const U8 * from, rrPixelFormat format, rrColor4I * color, S32 count );
void rrPixelFormat_GetColors4F( const U8 * from, rrPixelFormat format, rrColor4F * color, S32 count );

void rrPixelFormat_PutColors4I( U8 * to, rrPixelFormat format, const rrColor4I * color, S32 count );
void rrPixelFormat_PutColors4F( U8 * to, rrPixelFormat format, const rrColor4F * color, S32 count );

//---------------------------------------------------------------------------------

//*
// only used by radbitmaptest\main_util.cpp :
extern const rrPixelFormat c_formats[];
extern const int c_formats_count;
/**/

RR_NAMESPACE_END

#endif // __RADRRBITMAP_PIXELFORMAT_H__
