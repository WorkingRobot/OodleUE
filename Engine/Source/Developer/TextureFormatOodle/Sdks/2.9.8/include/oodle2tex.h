
//===================================================
// Oodle2 Texture header
// (C) Copyright 1994-2022 Epic Games Tools LLC
//===================================================

#ifndef __OODLE2TEX_H_INCLUDED__
#define __OODLE2TEX_H_INCLUDED__

#ifndef OODLE2TEX_PUBLIC_HEADER
#define OODLE2TEX_PUBLIC_HEADER 1
#endif

#ifndef __OODLE2BASE_H_INCLUDED__
#include "oodle2base.h"
#endif

#ifdef _MSC_VER
#pragma pack(push, Oodle, 8)

#pragma warning(push)
#pragma warning(disable : 4127) // conditional is constant
#endif

// header version :
//  the DLL is incompatible when MAJOR is bumped
//  MINOR is for internal revs and bug fixes that don't affect API compatibility
#define OODLE2TEX_VERSION_MAJOR         9
#define OODLE2TEX_VERSION_MINOR         8

// OodleTextureVersion string is 1 . MAJOR . MINOR
//  don't make it from macros cuz the doc tool has to parse the string literal

#define OodleTextureVersion "2.9.8"    /*
*/

typedef enum OodleTex_Err
{
    OodleTex_Err_OK = 0,                            // no error
    OodleTex_Err_UnsupportedCPU = -1,               // CPU is not supported. Oodle Texture encoding requires SSE4.1.
    OodleTex_Err_BadBCnFormat = -2,                 // Specified BCn format is not supported.
    OodleTex_Err_BadPixelFormat = -3,               // Specified pixel format is not supported.
    OodleTex_Err_SurfaceCountMismatch = -4,         // The number of surface specified does not match the $OodleTex_Layout (without a layout, surface count must be 1).
    OodleTex_Err_BlockCountMismatch = -5,           // The number of blocks specified does not match the surface dimensions or $OodleTex_Layout.
    OodleTex_Err_LayoutFormatMismatch = -6,         // The given $OodleTex_Layout does not match the target BCn format.
    OodleTex_Err_NegativeLambda = -7,               // The specified Lagrange multiplier (lambda) is negative.
    OodleTex_Err_Internal = -8,                     // Unspecified internal error (this should not happen; if you can, please report a bug.)

    OodleTex_Err_BC7PrepHeaderCorrupt = -9,         // BC7Prep header corrupted (or unsupported versin)
    OodleTex_Err_BC7PrepOutputBufTooSmall = -10,    // BC7Prep output buffer too small for given block count
    OodleTex_Err_BC7PrepScratchBufTooSmall = -11,   // BC7Prep scratch buffer too small for input data
    OodleTex_Err_BC7PrepPayloadCorrupt = -12,       // BC7Prep payload data chunk was corrupted
    OodleTex_Err_BC7PrepNoHeader = -13,             // BC7Prep missing output header pointer on encode
    OodleTex_Err_BC7PrepIllegalBlockCount = -14,    // BC7Prep block count outside legal bounds

    OodleTex_Err_InvalidSurfaceIndex = -15,         // The given surface index is out of bounds
    OodleTex_Err_MalformedBlockIDs = -16,           // The block IDs passed to SetBlockLayout don't have the required form
    
    OodleTex_Err_BadMetric = -17,                   // OodleTex_RDO_ErrorMetric invalid
    OodleTex_Err_BadEncodeEffortLevel = -18,        // OodleTex_EncodeEffortLevel invalid

    OodleTex_Err_SurfaceSizeMismatch = -19,         // One of the surfaces passed to the Encode/Decode functions does not match the size of the corresponding surface in the $OodleTex_Layout.

    OodleTex_Err_NoLicense_Unused = -20,            // unused
    OodleTex_Err_BufferTooSmall = -21,              // Provided buffer is too small
    OodleTex_Err_SurfaceTooLarge = -22,             // One of the input surfaces is larger than the maximum supported size (width or height above $OODLETEX_MAX_SURFACE_DIMENSION)
    OodleTex_Err_BadUniversalTiling = -23,          // OodleTex_RDO_UniversalTiling invalid
    OodleTex_Err_LayoutAndUniversalTilingIncompatible = -24,    // Both layout and universal tiling specified, they're mutually exclusive.

    OodleTex_Err_BadSRGBFormat = -25,               // Linear<->sRGB conversion flag is allowed for BC1237 formats and, between 4xF32 and 4xU8 pixel formats

    OodleTex_Err_Force32 = 0x40000000       // not an actual error!
} OodleTex_Err;
/* Error codes for Oodle Texture API functions.

    Negative values indicate an actual error, non-negative values indicate success.
    
    Functions that return integers can return errors in negative numbers.
    Non-negative values indicate success, negative values correspond to an $OodleTex_Err value.
    The utility function $OodleTex_Err_GetName can be used to turn these error codes into strings.
*/

typedef enum OodleTex_PixelFormat
{
    OodleTex_PixelFormat_Invalid = 0,
    OodleTex_PixelFormat_4_U8_RGBA  = 1, // 4 uint8s per pixel: R,G,B,A
    OodleTex_PixelFormat_4_U8_BGRA  = 2, // 4 uint8s per pixel: B,G,R,A
    OodleTex_PixelFormat_4_F32_RGBA = 3, // 4 floats per pixel: R,G,B,A
    OodleTex_PixelFormat_4_F16_RGBA = 4, // 4 halfs per pixel: R,G,B,A
    OodleTex_PixelFormat_3_F32_RGB = 5, // 3 floats per pixel: R,G,B
    OodleTex_PixelFormat_3_U8_RGB  = 6, // 3 uint8s per pixel: R,G,B
    OodleTex_PixelFormat_3_U8_BGR  = 7, // 3 uint8s per pixel: B,G,R
    OodleTex_PixelFormat_2_U16 = 8, // 2 uint16s per pixel: R,G
    OodleTex_PixelFormat_2_S16 = 9, // 2 int16s per pixel: R,G
    OodleTex_PixelFormat_2_U8 = 10, // 2 uint8s per pixel: R,G
    OodleTex_PixelFormat_2_S8 = 11, // 2 int8s per pixel: R,G
    OodleTex_PixelFormat_1_U16 = 12, // 1 uint16 per pixel: R
    OodleTex_PixelFormat_1_S16 = 13, // 1 int16 per pixel: R
    OodleTex_PixelFormat_1_U8 = 14, // 1 uint8 per pixel: R
    OodleTex_PixelFormat_1_S8 = 15, // 1 int8 per pixel: R
    OodleTex_PixelFormat_4_U8_RGBx = 16, // 4 uint8s per pixel: R, G, B, (ignored) - like U3_RGB with a padding byte per pixel
    OodleTex_PixelFormat_4_U8_BGRx = 17, // 4 uint8s per pixel: B, G, R, (ignored) - like U3_BGR with a padding byte per pixel
    
    OodleTex_PixelFormat_4_U16 = 18, // 4 uint16s per pixel: R,G,B,A (bonus convenience format, not for direct encoding to BCN)
    OodleTex_PixelFormat_3_U16 = 19, // 3 uint16s per pixel: R,G,B (bonus convenience format, not for direct encoding to BCN)
    OodleTex_PixelFormat_2_F32 = 20, // 2 floats per pixel: R,G (bonus convenience format, not for direct encoding to BCN)
    OodleTex_PixelFormat_1_F32 = 21, // 1 floats per pixel: R (bonus convenience format, not for direct encoding to BCN)

    OodleTex_PixelFormat_Max = 21,
    OodleTex_PixelFormat_Force32 = 0x40000000
} OodleTex_PixelFormat;
/* OodleTex_PixelFormat describes the format of the source pixel surface.

    It is described by the number of channels, then the format of each channel.
    For example, `OodleTex_PixelFormat_2_U16` is 32-bits per pixel, two 16-bit components, unsigned.

    Multi-channel pixels listed in memory order and values are assumed to be normalized,
    eg. `OodleTex_PixelFormat_4_U8_RGBA` is like `DXGI_FORMAT_R8G8B8A8_UNORM`, and `OodleTex_PixelFormat_2_S16`
    corresponds to `DXGI_FORMAT_R16G16_SNORM`.
    
    The pixel formats are treated as normalized (UNORM or SNORM) in the D3D convention.  Opaque alpha is 255, 65535,
    or 1.0, depending on format.
    
    Float formats like `OodleTex_PixelFormat_4_F32_RGBA` can only be used with BC6 output.  (The BC6 encoder requires
    float input).
    
    BC4S and BC5S (SNORM) can only be encoded from signed integer pixel formats (S8 or S16), all other formats will refuse
    to read signed integer pixels.  
    
    The formats 1_F32 and 2_F32 cannot be directly encoded to BC4/5 but can be converted to S16 or U16 with
    $OodleTex_BlitNormalized before encoding.  They are included for convenience.

    Formats without alpha act like they have an implicit opaque alpha channel (255 for `OodleTex_PixelFormat_3_U8_RGB`
    and 1.0 for `OodleTex_PixelFormat_3_F32_RGB`).  However, 3 channel RGB behaves differently than 4 channel RGBA with
    opaque alpha in the RMSE call - with 3 channel it is assumed that "ignore alpha" was intended if not explicitly specified.
    Similarly for the RGBx and BGRx formats: if they are promoted to RGBA, opaque alpha is set, but they are different
    than RGBA in that they imply alpha should be ignored.
    
    Oodle Texture does not need to know if the pixels are SRGB gamma corrected or not.  Float input for BC6H is
    assumed to be HDR linear light (unless BCNFlag_BC6_NonRGBData is specified).
    
    See also $OodleTex_FAQ_ImageInputFormat and $OodleTex_MasteringGuide
    
*/

typedef enum OodleTex_BC
{
    OodleTex_BC_Invalid = 0,
    OodleTex_BC1    = 1, // BC1 for UNORM RGB color images; may use transparent to send black, output A can be 255 or 0.
    OodleTex_BC1_WithTransparency = 2, // BC1 with 1-bit alpha transparency
    OodleTex_BC2    = 3, // BC2 encodes 8-bit UNORM RGBA
    OodleTex_BC3    = 4, // BC3 encodes 8-bit UNORM RGBA
    OodleTex_BC4U   = 5, // BC4 UNORM. Encodes R channel only.
    OodleTex_BC4S   = 6, // BC4 SNORM. Encodes R channel only.
    OodleTex_BC5U   = 7, // BC5 UNORM. Encodes R and G channels.
    OodleTex_BC5S   = 8, // BC5 SNORM. Encodes R and G channels.
    OodleTex_BC6U   = 9, // BC6H UF16 - unsigned BC6H data.
    OodleTex_BC6S   = 10, // BC6H SF16 - signed BC6H data.
    OodleTex_BC7RGBA= 11, // BC7 encodes 8-bit UNORM RGBA
    OodleTex_BC7RGB = 12, // BC7 only using RGB channels, with alpha ignored. Increases quality in RGB channels but output A is undefined.
    OodleTex_BC_Max = 12,
    OodleTex_BC_Force32 = 0x40000000
} OodleTex_BC;
/* OodleTex_BC selection of BCN block compressed texture format.

    BC4-6 treat Unsigned and Signed as distinct format variants.

### BC1
    8 bytes per block \<br>
    RGB (BC1) or RGBA with 1 bit transparency (BC1_WithTransparency) \<br>
    BC1 gives better RGB quality (than BC1_WithTransparency) and outputs undefined in A \<br>
    should be encoded from $OodleTex_PixelFormat_4_U8_RGBA data \<br>
    See $OodleTex_FAQ_IgnoreAlpha about BC1 vs BC1_WithTransparency

### BC2
    16 bytes per block \<br>
    BC1 + fixed step alpha block \<br>
    generally use BC3 instead \<br>
    should be encoded from $OodleTex_PixelFormat_4_U8_RGBA data

### BC3
    16 bytes per block \<br>
    BC1 color + BC4 alpha \<br>
    should be encoded from $OodleTex_PixelFormat_4_U8_RGBA data

### BC4
    8 bytes per block \<br>
    supports encoding & decoding from 8 bit and 16 bit data \<br>
    use the correct signed or unsigned OodleTex_PixelFormat_1_XX format \<br>
    (eg. $OodleTex_PixelFormat_1_S16 with `OodleTex_BC4S`) \<br>
    If you use $OodleTex_PixelFormat_4_U8_RGBA, BC4 reads the first channel (R)

### BC5
    16 bytes per block \<br>
    two BC4 channels \<br>
    supports encoding & decoding from 8 bit and 16 bit data \<br>
    use the correct signed or unsigned OodleTex_PixelFormat_2_XX format \<br>
    (eg. $OodleTex_PixelFormat_2_S16 with `OodleTex_BC5S`) \<br>
    If you use $OodleTex_PixelFormat_4_U8_RGBA, BC5 reads the first two channels (RG)

### BC6H
    16 bytes per block \<br>
    floating point images, no alpha channel \<br>
    should be encoded from $OodleTex_PixelFormat_4_F32_RGBA data \<br>
    for textures that are not color images, use $OodleTex_BCNFlag_BC6_NonRGBData

### BC7
    16 bytes per block \<br>
    BC7RGB gives better RGB quality (than BC7RGBA) and outputs undefined in A \<br>
    should be encoded from $OodleTex_PixelFormat_4_U8_RGBA data \<br>
    See $OodleTex_FAQ_IgnoreAlpha about BC7RGBA vs BC7RGB

### General

    See also $OodleTex_FAQ_ImageInputFormat and $OodleTex_MasteringGuide
*/

typedef OOSTRUCT OodleTex_Surface
{
    const void *    pixels;     // Pointer to the first row of input pixel data
    OO_SINTa        rowStrideBytes; // Distance between subsequent rows of pixels in bytes
    OO_S32          width;      // width of the surface (in pixels)
    OO_S32          height;     // height of the surface (in pixels)
} OodleTex_Surface;
/* Describes a 2D surface, a 2D array of pixels in some format.

   A surface can be one of the following:

   $* a regular 2D image (or an element of a 2D texture array)
   $* a mip level of a larger base 2D image
   $* a face of a cube map at any of its mip levels
   $* a single Z-slice of a volume texture or one of its mip maps

   No matter which, surfaces describe a 2D arrangement of pixels with a row-major
   layout, with a configurable stride between rows, in case the source image has extra
   data at the end of rows for alignment or other reasons.

   _rowStrideBytes_ must be positive and >= _width_ * pixel bytes.
   _width_ and _height_ must be positive and at most $OODLETEX_MAX_SURFACE_DIMENSION.
*/

#define OODLETEX_MAX_SURFACE_DIMENSION 16384 /* Maximum dimensions of an OodleTex_Surface in either direction (width or height).

   Surfaces must not be larger than this on any axis.
*/


#define OodleTex_Surface_NumBlocks(surf)    ((( (surf)->width + 3 )/4)*(( (surf)->height + 3 )/4))  /* Number of 4x4 blocks in an OodleTex_Surface

    surfaces under 4x4 alignment are padded up to 4x4 for compression by duplicating edge pixels
*/

//idoc(parent,OodleAPI_TextureCoding)

typedef enum OodleTex_EncodeEffortLevel
{
    OodleTex_EncodeEffortLevel_Default = 0,
    OodleTex_EncodeEffortLevel_Low = 10,
    OodleTex_EncodeEffortLevel_Normal = 20,
    OodleTex_EncodeEffortLevel_High = 30,
    OodleTex_EncodeEffortLevel_Force32 = 0x40000000
} OodleTex_EncodeEffortLevel;
/* OodleTex_EncodeEffortLevel lets you dial how much time the encoder can spend to find better encodings.

    OodleTex_EncodeEffortLevel is only used by the non-RDO BCN encode API $OodleTex_EncodeBCN_LinearSurfaces

    This trades off encode time vs quality of encoding:
    lower number = faster but lower quality and
    higher number = slower but higher quality.

    Try to not hard code use of these numbers, they may change.

    We recommend level Normal or High for shipping content.

    If possible, use Default, don't hard-code a level, as that will choose the level we believe is
    usually best and is portable to different versions of Oodle Texture.
    
    OodleTex_EncodeEffortLevel is not a continuous parameter.  Only the enumerated values should be used.
*/



typedef enum OodleTex_RDOLagrangeLambda
{
    OodleTex_RDOLagrangeLambda_NearLossless = 1, // almost lossless (relative to baseline BCN)
    OodleTex_RDOLagrangeLambda_VeryHighQuality = 10,
    OodleTex_RDOLagrangeLambda_HighQuality = 20,
    OodleTex_RDOLagrangeLambda_Default = 30,
    OodleTex_RDOLagrangeLambda_MediumQuality = 40,
    OodleTex_RDOLagrangeLambda_LowQuality = 60,
    OodleTex_RDOLagrangeLambda_Min = 1,
    OodleTex_RDOLagrangeLambda_Max = 100,
    OodleTex_RDOLagrangeLambda_Force32 = 0x40000000
} OodleTex_RDOLagrangeLambda;
/* OodleTex_RDOLagrangeLambda provides reference values for the lagrange lambda parameter of OodleTex_EncodeBCN_RDO

    Lambda tells the encoder how much you care about quality vs rate (compressed size) in $RateDistortionOptimization.

    Lambda of zero you don't care about rate at all, and therefore get maximum quality (baseline BCN with no RDO).

    Lambda of 1 (OodleTex_RDOLagrangeLambda_NearLossless) should almost always be used instead of zero, 
    as it will give you near-lossless quality (relative to baseline) but can sometimes find some nice rate savings.
    In fact you can usually go to lambda 5 or 10 and still have super high quality.  Do not treat lambda=1 as the
    only near lossless option.
    
    Higher lambdas correspond to more rate reduction and lower quality.
    
    Lambda in 10-30 should be very small visual quality loss.
    
    Lambda if 30-60 will have some apparent visual quality loss, but should be acceptable in most cases.
    
    Lambda around 50 is the maximum you should use broadly without manual inspection of the results.
    
    Lambda 50-100 can be used for further rate reduction, but is more unpredictable.  It can give unacceptable
    quality loss on some images, but is okay on others.
    
    Lambda 30 is a good default starting point that will give a good balance of rate reduction and quality.
    
    Lambda is not actually a quality parameter, and it can give different quality levels on different images.
    This is intentional.  Lambda is
    more precisely an exchange rate between the different currencies of "rate" and "distortion".  It tells our
    codec how to value a decision, whether it is a profitable transaction to trade a certain rate improvement
    for some distortion penalty.  Higher lambda makes rate gains (size reduction) more value relative to a
    given distortion penalty.  See $RateDistortionOptimization
    
    Any integer greater than zero can be used for lambda, not just the enumerated values.  For example lambda of 5
    will give results between OodleTex_RDOLagrangeLambda_NearLossless (1) and OodleTex_RDOLagrangeLambda_VeryHighQuality (10).
    
*/

typedef enum OodleTex_BCNFlags
{
    OodleTex_BCNFlags_None = 0,
    OodleTex_BCNFlag_PreserveExtremes_BC3457 = 1, // BC3-5 and BC7: preserve 0 and 255 (0.0 and 1.0) in alpha channel exactly.
    OodleTex_BCNFlag_PreserveExtremes_BC345 = 1, // Alias for BCNFlags_PreserveExtremes_BC3457 (old name)
    OodleTex_BCNFlag_BC6_NonRGBData = 2, // BC6: texture contains non-image data and should be treated scalar channels
    OodleTex_BCNFlag_AvoidWideVectors = 4, // Avoid wide vector instruction sets
    OodleTex_BCNFlag_PreferWideVectors = 8, // Prefer wide vector instruction sets
    OodleTex_BCNFlag_LinearToSRGB = 16, // Convert linear float input to sRGB, currently only for BC1237 formats
    OodleTex_BCNFlag_Force32 = 0x40000000
} OodleTex_BCNFlags;
/* OodleTex_BCNFlags

    bit flags for BCN encoding options, combine with bitwise or.
    
    `OodleTex_BCNFlag_PreserveExtremes` is currently only used for BC3-5,
    it does not apply to BC1_WithTransparency & BC2 (which inherently preserve 0 and 255).
    BC5 preserves both channels. For BC4 and BC5 signed variants, it's the values decoding to -1 and +1 that
    are preserved instead of 0 and 1.
    
    OodleTex_BCNFlag_PreserveExtremes_BC3457 on BC3 and BC7 applies only to the alpha channel,
    not the RGB channels.
    
    BC6 encoding by default assumes the texture contains RGB image data that can be interpreted as linear light color,
    and uses a corresponding error metric.  If contains other values, like arbitrary floating point data, then specify
    OodleTex_BCNFlag_BC6_NonRGBData to turn off the image metric.
    
    OodleTex_BCNFlag_AvoidWideVectors can be used to turn off usage of wide vector instructions, mainly 512-bit vectors;
    on several generation of Intel CPUs, using 512-bit wide vectors dramatically lowers the peak achievable CPU clock
    frequencies. Not all the encoders in Oodle Texture benefit the same from 512-bit vectors, and other code running at
    the same time may not use them at all. Especially for mixed workloads, it can be beneficial to disable 512-bit vector
    usage in Oodle Texture when the slowdown from the decreased clock frequency outweighs the gains during texture
    encoding.

    OodleTex_BCNFlag_PreferWideVectors is the opposite and tells Oodle Texture to prefer using wide vector instructions
    whenever they are available.

    If both "Avoid" and "Prefer" wide vectors are specified simultaneously, "Avoid" takes precedence. When neither
    is specified, Oodle Texture picks a reasonable default for the detected CPU. These defaults may change between
    releases.

    Use OodleTex_BCNFlag_LinearToSRGB flag with 4xfloat input for BC1237 encodings to convert linear to sRGB colorspace.
*/

typedef enum OodleTex_BCNDecodeFlags
{
    OodleTex_BCNDecodeFlags_None = 0,
    OodleTex_BCNDecodeFlag_SRGBToLinear = 1, // Convert sRGB to linear float output, currently only for BC1237 formats
    OodleTex_BCNDecodeFlag_Force32 = 0x40000000
} OodleTex_BCNDecodeFlags;
/* OodleTex_BCNDecodeFlags

    bit flags for BCN decoding options, combine with bitwise or.

    Use OodleTex_BCNDecodeFlag_SRGBToLinear flag with 4xfloat output for BC1237 formats to convert sRGB to linear colorspace.
*/

typedef enum OodleTex_RDO_ErrorMetric
{
    OodleTex_RDO_ErrorMetric_Default = 0, // Default is Perceptual
    OodleTex_RDO_ErrorMetric_RMSE_RGBA = 1,
    OodleTex_RDO_ErrorMetric_Perceptual_RGBA = 2,
    OodleTex_RDO_ErrorMetric_Max = 2,
    OodleTex_RDO_ErrorMetric_Force32 = 0x40000000
} OodleTex_RDO_ErrorMetric;
/* Choose the way errors are scored in OodleTex_EncodeBCN_RDO.

    This allows you to specify the "D" distortion metric used in the rate-distortion optimization.
    Different distortion metrics will score errors in different ways, which changes the encoding.
    
    Typically you should use the "Perceptual" metric.  The Perceptual metric tries to preserve areas
    where it thinks errors will be very visible to the human eye (such as in smooth gradients).
    Perceptual tries to put more error where it thinks it will be hidden to the human eye, such as in
    noisy or highly textured areas.
    
    Perceptual is usually best on regular images, diffuse or albedo textures, or anything that will be
    just looked at by the human eye without reintrepetation of the values.

    The "RMSE" metric simply tries to minimize the squared error of all the channels.  This can be useful
    when the textures channels contain scalar fields which are not really an image.  This is an option
    when Perceptual gives undesirable results.
    
    Note that when measuring codec performance using RMSE or related metrics such as PSNR, you should use
    the RMSE metric, so that the codec is targeting the same goal you are measuring it by.

    Default = Perceptual.

    The difference between RGBA_Perceptual and RGB_A_Perceptual is whether the color and alpha channels
    are treated separately or joint for purposes of visual masking.  This only applies to BC3 and BC7 images
    with alpha channels, in other cases they behave the same.  When relevant, it determines whether noise in
    color/alpha can mask the other.  If A is storing a material property that should be treated separatey for
    color, usually separating them by using RGB_A will be better.

*/

typedef enum OodleTex_RDO_UniversalTiling
{
    OodleTex_RDO_UniversalTiling_Disable = 0,   // No universal tiling, the default. This option must be used when an $OodleTex_Layout is specified.
    OodleTex_RDO_UniversalTiling_256KB = 1,     // Universal tiling with 256KB blocks
    OodleTex_RDO_UniversalTiling_64KB = 2,      // Universal tiling with 64KB blocks
    OodleTex_RDO_UniversalTiling_Max = 2,
    OodleTex_RDO_UniversalTiling_Force32 = 0x40000000
} OodleTex_RDO_UniversalTiling;
/* Enables universal tiling, when desired.

    Many platforms, especially game consoles, expose GPU-specific native image layouts that employ some kind
    of texture tiling scheme. This can be targeted directly via $OodleTex_Layout, but this means that multi-platform
    projects need to potentially do separate RDO texture encodes for every target.

    The primary effect of native texture layouts as far as Oodle Texture is concerned is that large textures
    usually end up being stored as a sequence of large rectangular 2D tiles, instead of scanline by scanline.
    There is also reordering within those tiles but the effect on RDO encodes and compression performance is
    relatively minor.

    Universal tiling is a compromise: Oodle Texture internally processes a texture as individual tiles of the given
    size. For example, 256KB universal tiling for BC7 textures treats a 2D square of 128x128 blocks as a unit.
    However, both the input and output surfaces passed to Oodle Texture are still in the usual linear, row-major
    order.

    These textures can then be transformed into a number of platform-specific native orders and will in general
    result in noticeably higher compression than if the texture had been encoded in the default linear order,
    although generally somewhat worse than if the actual native $OodleTex_Layout had been used. Linear textures
    using Universal Tiling will on average compress somewhat worse than regular linear layout textures on platforms
    that store assets this way (e.g. PC or Mac), but not by much.

    This makes it possible to cache a single RDO encode in universal tiling order and share the results between
    multiple targets with different native texture tiling schemes, at a considerably saving in overall encode time.

    In general, we've found 256KB block size to give the best overall trade-off across platforms, provided that
    compression is done on independent chunks that are 256KB or larger (uncompressed). If you are paging and
    decompressing data in smaller units, consider using the 64KB variant instead. See "$OodleTex_About_UniversalTiling"
    in the "About" section of the documentation for more information.
*/

typedef enum OodleTex_RDO_Flags
{
    OodleTex_RDO_Flags_None = 0,
    OodleTex_RDO_Flags_Force32 = 0x40000000
} OodleTex_RDO_Flags;
/* Binary flags for the Oodle Texture RDO encode.

    Currently, none are defined. Use OodleTex_RDO_Flags_None.
*/

typedef OOSTRUCT OodleTex_RDO_Options
{
    OodleTex_EncodeEffortLevel      effort;     // Higher effort levels give better (smaller/higher quality) results but take longer to encode
    OodleTex_RDO_ErrorMetric        metric;     // Choice of error metric determines what the encoder focuses on
    OodleTex_BCNFlags               bcn_flags;  // General BCn encoding options
    OodleTex_RDO_UniversalTiling    universal_tiling;   // Universal tiling mode
    OodleTex_RDO_Flags              rdo_flags;  // RDO-specific encoding options
    
    OO_BOOL                         use_bc3_alpha_lambda;   // should bc3_alpha_lambda value be used (else use rgb lambda for alpha)
    OO_S32                          bc3_alpha_lambda;       // Higher lambda settings give bigger size reductions but reduce visual fidelity

    OO_U32                          not_yet_used_zero_me[8]; // fill with zero

} OodleTex_RDO_Options;
/* Detailed RDO encoding options for use with OodleTex_EncodeBCN_RDO_Ex.

    Initializing with = { } (zero-init) gives defaults for everything.

    _effort_ controls RDO encode effort. Lower-effort encodes are faster but produce slightly worse quality
    and compression ratio. The default is to use "High" effort level. "Normal" effort level typically reduces
    RDO encode time by an average factor of around 1.5 to 2 relative to "High". "Low" is faster by the same amount
    and reduces quality further. All three levels are meant to still give good quality; our recommendation is to
    use "Normal" or "Low" during iteration and day-to-day work and reserve "High" levels for shipping builds and
    overnight jobs.

    _metric_ chooses the error metric the encoder targets during RD optimization, one of $OodleTex_RDO_ErrorMetric.
    Typically $OodleTex_RDO_ErrorMetric_Default.

    _bcn_flags_ enables format-specific BCn encoding options as per $OodleTex_BCNFlags.

    _universal_tiling_ enables universal tiling mode; see $OodleTex_RDO_UniversalTiling for a short description
    and "$OodleTex_About_UniversalTiling" for more details.

    _rdo_flags_ is intended for future extension to allow us to add flags controlling RDO encoder operation.
    For now, none are defined; pass OodleTex_RDO_Flags_None.

    _use_bc3_alpha_lambda_ and _bc3_alpha_lambda_: when use_bc3_alpha_lambda is false (default), the argument lambda
    passed to $OodleTex_EncodeBCN_RDO_Ex is used for both RGB and A channels.
    When use_bc3_alpha_lambda is true, the lambda argument is used for the RGB channels and
    bc3_alpha_lambda is used for the A channel. RDO on the BC3 alpha channel can be disabled
    entirely by setting use_bc3_alpha_lambda=true, bc3_alpha_lambda=0. These options
    currently only apply to BC3.

    _not_yet_used_zero_me_ is room for future extensions and currently ignored. Initialize to 0.
*/

#define OODLETEX_JOBS_DEFAULT  ( 0) /* Use all the installed threads

    Value for num_job_threads that means to use all the installed threads
*/

#define OODLETEX_JOBS_DISABLE  (-1) /* Run single-threaded

    Value for num_job_threads that means to run single-threaded on the main thread.
*/


//idoc(parent,OodleAPI_TextureLayout)

typedef struct OodleTex_Layout OodleTex_Layout;
/* OodleTex_Layout is an opaque structure that describes the block layout for a texture.

   Oodle Texture itself deals primarily with linear-layout
   2D images, because that is the most natural form to work in for BCn compression. However,
   actual textures are more complex objects and include mip chains, cube maps, texture arrays,
   and volume textures. As far as BCn compression is concerned, all of these things are made up
   of a collection of 2D images (not all with the same size), and there are various different
   ways of laying out the resulting collection of BCn blocks in memory.

   For some targets, especially game consoles, the native texture memory layouts are exposed in
   the SDK, and it is preferable to store textures in that form, since doing so minimizes
   load-time overhead.

   However, Oodle Texture RDO encoding is not independent of block ordering; for good results,
   the RDO encoder needs to know which order the blocks are going to be stored in memory (or on
   disk). Often this means reordering the blocks not just within a 2D image, but also between
   array/volume slices and sometimes even mipmap levels.

   Setting up an `OodleTex_Layout` allows the encoder to produce blocks in hardware memory order.
*/

// function pointers to mallocs needed :

OODEFFUNC typedef void * (OODLE_CALLBACK t_fp_OodleTex_Plugin_MallocAligned)( OO_SINTa bytes, OO_S32 alignment);
/* Function pointer type for OodleMallocAligned

    $:bytes     number of bytes to allocate
    $:alignment required alignment of returned pointer
    $:return    pointer to memory allocated (must not be NULL)

    _alignment_ will always be a power of two

    _alignment_ will always be >= $OODLE_MALLOC_MINIMUM_ALIGNMENT

*/

OODEFFUNC typedef void (OODLE_CALLBACK t_fp_OodleTex_Plugin_Free)( void * ptr );
/* Function pointer type for OodleFree

    $:return    pointer to memory to free

*/

OOFUNC1 void OOFUNC2 OodleTex_Plugins_SetAllocators(
    t_fp_OodleTex_Plugin_MallocAligned * fp_OodleMallocAligned,
    t_fp_OodleTex_Plugin_Free * fp_OodleFree);
/* Set the function pointers for allocation needed by Oodle2 Core

    If these are not set, the default implementation on most platforms uses the C stdlib.
    On Microsoft platforms the default implementation uses HeapAlloc.

    These must not be changed once they are set!  Set them once then don't change them.

    

    If you want to ensure that Oodle is not doing any allocations, you can call OodleTex_Plugins_SetAllocators(NULL,NULL);
    If you do that, then any time Oodle needs to allocate memory internally, it will stop the process.
    It is STRONGLY not recommended that you ship that way.  You can verify that Oodle is not allocating, but then leave some
    fallback allocator installed when you actually ship just in case.

    Also note that on many consoles the standard allocation practices may not
    leave much heap memory for the C stdlib malloc.  In this case Oodle may fail to allocate.

*/

OODEFFUNC typedef OO_U64 (OODLE_CALLBACK t_fp_OodleTex_Plugin_RunJob)( t_fp_Oodle_Job * fp_job, void * job_data , OO_U64 * dependencies, int num_dependencies, void * user_ptr );
/* Function pointer type for OodleTex_Plugins_SetJobSystem

    $:dependencies      array of handles of other pending jobs. All guaranteed to be nonzero.
    $:num_dependencies  number of dependencies. Guaranteed to be no more than OODLE_JOB_MAX_DEPENDENCIES.
    $:user_ptr          is passed through from the OodleLZ_CompressOptions.
    $:return            handle to the async job, or 0 if it was run synchronously

    RunJob will call fp_job(job_data)

    it may be done on a thread, or it may run the function synchronously and return 0, indicating the job is already done.
    The returned OO_U64 is a handle passed to WaitJob, unless it is 0, in which case WaitJob won't get called.

    fp_job should not run until all the dependencies are done.  This function should not delete the dependencies.

    RunJob must be callable from within an Oodle Job, i.e. jobs may spawn their own sub-jobs directly.
    However, the matching WaitJob calls will only ever occur on the thread that called the
    internally threaded Oodle API function.

    See $Oodle_About_Job_Threading_Plugins
*/

OODEFFUNC typedef void (OODLE_CALLBACK t_fp_OodleTex_Plugin_WaitJob)( OO_U64 job_handle, void * user_ptr );
/* Function pointer type for OodleTex_Plugins_SetJobSystem

    $:job_handle    a job handle returned from RunJob. Never 0.
    $:user_ptr      is passed through from the OodleLZ_CompressOptions.

    Waits until the job specified by job_handle is done and cleans up any associated resources. Oodle
    will call WaitJob exactly once for every RunJob call that didn't return 0.

    If job_handle was already completed, this should clean it up without waiting.

    A handle value should not be reused by another RunJob until WaitJob has been done with that value.

    WaitJob will not be called from running jobs.  It will be only be called from the original thread that
    invoked Oodle.  If you are running Oodle from a worker thread, ensure that that thread is allowed to wait
    on other job threads.

    See $Oodle_About_Job_Threading_Plugins
*/

OOFUNC1 void OOFUNC2 OodleTex_Plugins_SetJobSystem(
    t_fp_OodleTex_Plugin_RunJob * fp_RunJob,
    t_fp_OodleTex_Plugin_WaitJob * fp_WaitJob);
/* DEPRECATED use OodleTex_Plugins_SetJobSystemAndCount instead

    See $OodleTex_Plugins_SetJobSystemAndCount
*/


OOFUNC1 void OOFUNC2 OodleTex_Plugins_SetJobSystemAndCount(
    t_fp_OodleTex_Plugin_RunJob * fp_RunJob,
    t_fp_OodleTex_Plugin_WaitJob * fp_WaitJob,
    int target_parallelism);
/* Set the function pointers for async job system needed by Oodle2 Core

    $:fp_RunJob     pointer to RunJob function
    $:fp_WaitJob    pointer to WaitJob function
    $:target_parallelism    goal of number of jobs to run simultaneously

    If these are not set, the default implementation runs jobs synchronously on the calling thread.

    These must not be changed once they are set!  Set them once then don't change them.

    _target_parallelism_ allows you to tell Oodle how many Jobs it should try to keep in flight at once.
    Depending on the operation it may not be able to split work into this many jobs (so fewer will be used),
    but it will not exceed this count.

    For Oodle Data LZ work, typically _target_parallelism_ is usually best at the number of hardware cores
    not including hyper threads).

    For Oodle Texture BCN encoding work, _target_parallelism_ is usually best as the full number of hyper cores.

    In some cases you may wish to reduce _target_parallelism_ by 1 or 2 cores to leave some of the CPU free for
    other work.

    For example on a CPU with 16 cores and 32 hardware threads, for LZ work you might set _target_parallelism_ to 15
    when calling OodleCorePlugins.  For BC7 encoding you might set _target_parallelism_ to 30 when calling OodleTexPlugins.

    NOTE : if you are using Oodle Ext, do NOT call this.  OodleX_Init will install a job system for Oodle Core.
    Note OodleX only installs automatically to Oodle Core, not Net or Tex.  See example_jobify.cpp for manual
    plugin.

    Replaces deprecated $OodleTex_Plugins_SetJobSystem

    See $Oodle_About_Job_Threading_Plugins
*/

// the main func pointer for log :
OODEFFUNC typedef void (OODLE_CALLBACK t_fp_OodleTex_Plugin_Printf)(int verboseLevel,const char * file,int line,const char * fmt,...);
/* Function pointer to Oodle Core printf

    $:verboseLevel  verbosity of the message; 0-2 ; lower = more important
    $:file          C file that sent the message
    $:line          C line that sent the message
    $:fmt           vararg printf format string

    The logging function installed here must parse varargs like printf.

    _verboseLevel_ may be used to omit verbose messages.
*/

OOFUNC1 t_fp_OodleTex_Plugin_Printf * OOFUNC2 OodleTex_Plugins_SetPrintf(t_fp_OodleTex_Plugin_Printf * fp_rrRawPrintf);
/* Install the callback used by Oodle Core for logging

    $:fp_rrRawPrintf    function pointer to your log function; may be NULL to disable all logging
    $:return            returns the previous function pointer

    Use this function to install your own printf for Oodle Core.

    The default implementation in debug builds, if you install nothing, uses the C stdio printf for logging.
    On Microsoft platforms, it uses OutputDebugString and not stdio.

    To disable all logging, call OodleTex_Plugins_SetPrintf(NULL)

    WARNING : this function is NOT thread safe!  It should be done only once and done in a place where the caller can guarantee thread safety.

    In the debug build of Oodle, you can install OodleTex_Plugin_Printf_Verbose to get more verbose logging

*/

OODEFFUNC typedef OO_BOOL (OODLE_CALLBACK t_fp_OodleTex_Plugin_DisplayAssertion)(const char * file,const int line,const char * function,const char * message);
/* Function pointer to Oodle Core assert callback

    $:file          C file that triggered the assert
    $:line          C line that triggered the assert
    $:function      C function that triggered the assert (may be NULL)
    $:message       assert message
    $:return        true to break execution at the assertion site, false to continue

    This callback is called by Oodle Core when it detects an assertion condition.

    This will only happen in debug builds.


*/

OOFUNC1 t_fp_OodleTex_Plugin_DisplayAssertion * OOFUNC2 OodleTex_Plugins_SetAssertion(t_fp_OodleTex_Plugin_DisplayAssertion * fp_rrDisplayAssertion);
/* Install the callback used by Oodle Core for asserts

    $:fp_rrDisplayAssertion function pointer to your assert display function
    $:return            returns the previous function pointer

    Use this function to install your own display for Oodle Core assertions.
    This will only happen in debug builds.

    The default implementation in debug builds, if you install nothing, uses the C stderr printf for logging,
    except on Microsoft platforms where it uses OutputDebugString.

    WARNING : this function is NOT thread safe!  It should be done only once and done in a place where the caller can guarantee thread safety.

*/

//=============================================================


OOFUNC1 void * OOFUNC2 OodleTex_Plugin_MallocAligned_Default(OO_SINTa size,OO_S32 alignment);
OOFUNC1 void OOFUNC2 OodleTex_Plugin_Free_Default(void * ptr);
OOFUNC1 void OOFUNC2 OodleTex_Plugin_Printf_Default(int verboseLevel,const char * file,int line,const char * fmt,...);
OOFUNC1 void OOFUNC2 OodleTex_Plugin_Printf_Verbose(int verboseLevel,const char * file,int line,const char * fmt,...);
OOFUNC1 OO_BOOL OOFUNC2 OodleTex_Plugin_DisplayAssertion_Default(const char * file,const int line,const char * function,const char * message);
OOFUNC1 OO_U64 OOFUNC2 OodleTex_Plugin_RunJob_Default( t_fp_Oodle_Job * fp_job, void * job_data, OO_U64 * dependencies, int num_dependencies, void * user_ptr );
OOFUNC1 void OOFUNC2 OodleTex_Plugin_WaitJob_Default( OO_U64 job_handle, void * user_ptr );

//=============================================================

//idoc(parent,OodleAPI_TextureBase)

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_LogVersion();
/* Log the version and build of the Oodle Texture Lib.

    Logs with the plugged in logging callback.  See $OodleTex_Plugins_SetPrintf.

    Returns error if CPU compatability check fails. (Oodle Texture requires SSE4.)

*/

OOFUNC1 const char * OOFUNC2 OodleTex_Err_GetName(OodleTex_Err error);
/* Provides a string naming a $OodleTex_Err enum
*/

OOFUNC1 const char * OOFUNC2 OodleTex_PixelFormat_GetName(OodleTex_PixelFormat pf);
/* Provides a string naming a OodleTex_PixelFormat enum
*/

OOFUNC1 const char * OOFUNC2 OodleTex_BC_GetName(OodleTex_BC bcn);
/* Provides a string naming a OodleTex_BC enum
*/

OOFUNC1 const char * OOFUNC2 OodleTex_RDO_UniversalTiling_GetName(OodleTex_RDO_UniversalTiling tiling);
/* Provides a string naming a OodleTex_RDO_UniversalTiling enum
*/

OOFUNC1 OO_S32 OOFUNC2 OodleTex_BC_BytesPerBlock(OodleTex_BC bcn);
/* Get number of bytes per block of BCN compressed texture.

    Number of blocks for a linear surface is
    num_blocks = ((w+3)/4) * ((h+3)/4);

    Returns an error code ($OodleTex_Err) if library initialization failed.
*/

OOFUNC1 OO_S32 OOFUNC2 OodleTex_PixelFormat_BytesPerPixel(OodleTex_PixelFormat pf);
/* Get number of bytes per pixel for a pixel format.

    Bytes per block is 16* this.

    Returns an error code ($OodleTex_Err) if library initialization failed.
*/

OOFUNC1 OodleTex_PixelFormat OOFUNC2 OodleTex_BC_GetNaturalDecodeFormat(OodleTex_BC bcn);
/* Get the natural pixel format to decode to for a given BCn format.

    You can decode any BCn format to any $OodleTex_PixelFormat, but the natural DecodeFormat is preferred.
*/

//idoc(parent,OodleAPI_TextureCoding)

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_EncodeBCN_LinearSurfaces(
    OodleTex_BC to_bcn,void * to_bcn_blocks,OO_SINTa num_blocks,
    const OodleTex_Surface * from_surfaces,OO_SINTa num_from_surfaces,OodleTex_PixelFormat from_format,
    const OodleTex_Layout * layout,
    OodleTex_EncodeEffortLevel level,OodleTex_BCNFlags flags,
    int num_job_threads,void * jobify_user_ptr);
/* Write the BCN encoding of the given collection of pixel linear surfaces.

    $:to_bcn            which $OodleTex_BC BCN format to write
    $:to_bcn_blocks     memory to write BCN encoded blocks to
    $:num_blocks        number of blocks in output _to_bcn_blocks_
    $:from_surfaces     array of $OodleTex_Surface instances for source pixels, in format _from_format_
    $:num_from_surfaces number of surfaces in _from_surfaces_
    $:from_format       format of pixels in _from_surfaces_
    $:layout            $OodleTex_Layout describing physical arrangement of blocks in _to_bcn_ (may be NULL in certain cases)
    $:level             encode effort level to dial encode time vs quality
    $:flags             bit flags for additional options, see $OodleTex_BCNFlags
    $:num_job_threads   number of plugin job system threads to use for multi-threaded encoding ($OODLETEX_JOBS_DEFAULT for all, $OODLETEX_JOBS_DISABLE for single-threaded)
    $:jobify_user_ptr   user context passed back to plugin job system
    $:return            $OodleTex_Err indicating the error condition, or $OodleTex_Err_OK on success.

    Encodes BCN data from one or more image surfaces to an array of BCN blocks.

    Reads one or more surfaces _from_surfaces_ in _from_format_, writes _to_bcn_blocks_ in _to_bcn_ format.

    When _layout_ is NULL, the encoder produces blocks for all surfaces in row-major order.
    In that case, [num_blocks] must be the sum of $OodleTex_Surface_NumBlocks for all surfaces.
    Blocks for all surfaces are assumed to be consecutive.

    Specifying a _layout_ enables handling much more complicated scenarios such as interleaved hardware
    layouts for mip maps, texture arrays, cube maps or volume textures. Especially game consoles often
    expose the actual memory layout used by the hardware, which frequently interleaves data from multiple
    2D source images in complicated ways. When a _layout_ is specified, _num_blocks_ must match the number
    of blocks specified during $OodleTex_Layout_SetBlockLayout, and _num_from_surfaces_ must match the
    number of surfaces that make up said layout.

    See also $OodleTex_EncodeBCN_Blocks for more on level, flags, etc.
*/


OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_EncodeBCN_Blocks(
    OodleTex_BC to_bcn,void * to_bcn_blocks,OO_SINTa num_blocks,
    const void * from_pixel_blocks,OodleTex_PixelFormat from_format,
    OodleTex_EncodeEffortLevel level,OodleTex_BCNFlags flags,
    int num_job_threads,void * jobify_user_ptr);
/* Write the BCN encoding of the given pixel 4x4 blocks.

    $:to_bcn            which $OodleTex_BC BCN format to write
    $:to_bcn_blocks     memory to write BCN encoded blocks to
    $:num_blocks        number of blocks in output _to_bcn_blocks_
    $:from_pixel_blocks source pixels in 4x4 block order in format _from_format_
    $:from_format       format of pixels in _from_pixel_surface_
    $:level             encode effort level to dial encode time vs quality
    $:flags             bit flags for additional options, see $OodleTex_BCNFlags
    $:num_job_threads   number of plugin job system threads to use for multi-threaded encoding ($OODLETEX_JOBS_DEFAULT for all, $OODLETEX_JOBS_DISABLE for single-threaded)
    $:jobify_user_ptr   user context passed back to plugin job system
    $:return            $OodleTex_Err indicating the error condition, or $OodleTex_Err_OK on success.
    
    Encodes BCN data in linear arrays of blocks.

    Reads surface _from_pixel_blocks_ in _from_format_, writes _to_bcn_surface_ in _to_bcn_ format.
    
    _from_pixel_blocks_ should be an array of 16-pixel (4x4) blocks;
    _to_bcn_surface_ should be allocated to at least num_blocks * $OodleTex_BC_BytesPerBlock.

    EncodeBCN will use multiple threads if a thread job system is installed via $OodleTex_Plugins_SetJobSystemAndCount.
    _num_job_threads_ should typically be set to $OODLETEX_JOBS_DEFAULT to use all workers installed, but a different
    number can be passed here to override what was set in SetJobSystemAndCount, or single-threaded execution
    on the current thread can be forced by passing $OODLETEX_JOBS_DISABLE.

    EncodeBCN produces the same encoding as $OodleTex_EncodeBCN_RDO with lambda=0

    When you need very high quality encodings, you can still get significant rate savings
    by using $OodleTex_EncodeBCN_RDO with lambda=1 (OodleTex_RDOLagrangeLambda_NearLossless).

*/


OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_EncodeBCN_RDO(
    OodleTex_BC to_bcn,void * to_bcn_blocks,OO_SINTa num_blocks,
    const OodleTex_Surface * from_surfaces,OO_SINTa num_from_surfaces,OodleTex_PixelFormat from_format,
    const OodleTex_Layout * layout,
    int rdo_lagrange_lambda,OodleTex_BCNFlags flags,
    OodleTex_RDO_ErrorMetric rdo_metric,
    int num_job_threads,void * jobify_user_ptr);
/* Write the BCN encoding of the given pixels, with rate-distortion optimization (old API).

    $:to_bcn            which $OodleTex_BC BCN format to write
    $:to_bcn_surface    memory to write BCN encoding to
    $:num_blocks        number of blocks in output _to_bcn_blocks_
    $:from_surfaces     array of $OodleTex_Surface instances for source pixels, in format _from_format_
    $:num_from_surfaces number of surfaces in _from_surfaces_
    $:from_format       format of pixels in _from_surfaces_
    $:layout            $OodleTex_Layout describing physical arrangement of blocks in _to_bcn_ (may be NULL in certain cases)
    $:rdo_lagrange_lambda   lagrange parameter to control the quality vs size tradeoff of RDO, often a value of $OodleTex_RDOLagrangeLambda
    $:flags             bit flags for additional options, see $OodleTex_BCNFlags
    $:metric            error metric to use for distortion in RD, typically $OodleTex_RDO_ErrorMetric_Default
    $:num_job_threads   number of plugin job system threads to use for multi-threaded encoding ($OODLETEX_JOBS_DEFAULT for all, $OODLETEX_JOBS_DISABLE for single-threaded)
    $:jobify_user_ptr   user context passed back to plugin job system
    $:return            $OodleTex_Err indicating the error condition, or $OodleTex_Err_OK on success.

    This entry point is provided for compatibility; newer code should prefer $OodleTex_EncodeBCN_RDO_Ex.
    
    $RateDistortionOptimization creates a BCN encoding such that it is smaller after Kraken compression.
    The smaller the after-compression result, the more error is introduced in the BCN encoding.  This quality vs
    size tradeoff is controlled by _rdo_lagrange_lambda_.
    
    _rdo_lagrange_lambda_ is a scalar that provides fine control of the rate-quality balance.  
    $OodleTex_RDOLagrangeLambda provides some example standard values.
    
    _rdo_lagrange_lambda_ = 0 means minimize distortion only, which produces the same encoding as $OodleTex_EncodeBCN_Blocks.
    As lambda gets higher, more effort is put into reducing rate at the cost of higher distortion.
            
    Reads one or more surfaces _from_surfaces_ in _from_format_, writes _to_bcn_blocks_ in _to_bcn_ format.
    
    When _layout_ is NULL, the encoder produces blocks for all surfaces in row-major order.
    In that case, [num_blocks] must be the sum of $OodleTex_Surface_NumBlocks for all surfaces.
    Blocks for all surfaces are assumed to be consecutive.

    Specifying a _layout_ enables handling much more complicated scenarios such as interleaved hardware
    layouts for mip maps, texture arrays, cube maps or volume textures. Especially game consoles often
    expose the actual memory layout used by the hardware, which frequently interleaves data from multiple
    2D source images in complicated ways. When a _layout_ is specified, _num_blocks_ must match the number
    of blocks specified during $OodleTex_Layout_SetBlockLayout, and _num_from_surfaces_ must match the
    number of surfaces that make up said layout.
    
    _to_bcn_surface_ should be allocated to at least _num_blocks_ * $OodleTex_BC_BytesPerBlock.

    EncodeBCN_RDO will use multiple threads if a thread job system is installed via $OodleTex_Plugins_SetJobSystemAndCount.
    _num_job_threads_ should typically be set to $OODLETEX_JOBS_DEFAULT to use all workers installed, but a different
    number can be passed here to override what was set in SetJobSystemAndCount, or single-threaded execution
    on the current thread can be forced by passing $OODLETEX_JOBS_DISABLE.

    _OodleTex_EncodeBCN_RDO_ is equivalent to calling _OodleTex_EncodeBCN_RDO_Ex_ with OodleTex_EncodeEffortLevel_High and
    all other OodleTex_RDO_Options as default.
    
    _OodleTex_EncodeBCN_RDO_ uses OodleTex_EncodeEffortLevel_High to maintain previous behavior.  We recommend the new
    default OodleTex_EncodeEffortLevel_Normal for faster encodings and only slightly lower quality.

*/  

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_EncodeBCN_RDO_Ex(
    OodleTex_BC to_bcn,void * to_bcn_blocks,OO_SINTa num_blocks,
    const OodleTex_Surface * from_surfaces,OO_SINTa num_from_surfaces,OodleTex_PixelFormat from_format,
    const OodleTex_Layout * layout,
    int rdo_lagrange_lambda,
    const OodleTex_RDO_Options * options,
    int num_job_threads,void * jobify_user_ptr);
/* Write the BCN encoding of the given pixels, with rate-distortion optimization (new API).

    $:to_bcn            which $OodleTex_BC BCN format to write
    $:to_bcn_surface    memory to write BCN encoding to
    $:num_blocks        number of blocks in output _to_bcn_blocks_
    $:from_surfaces     array of $OodleTex_Surface instances for source pixels, in format _from_format_
    $:num_from_surfaces number of surfaces in _from_surfaces_
    $:from_format       format of pixels in _from_surfaces_
    $:layout            $OodleTex_Layout describing physical arrangement of blocks in _to_bcn_ (may be NULL in certain cases)
    $:rdo_lagrange_lambda   lagrange parameter to control the quality vs size tradeoff of RDO, often a value of $OodleTex_RDOLagrangeLambda
    $:options           $OodleTex_RDO_Options with further encoding options.
    $:num_job_threads   number of plugin job system threads to use for multi-threaded encoding ($OODLETEX_JOBS_DEFAULT for all, $OODLETEX_JOBS_DISABLE for single-threaded)
    $:jobify_user_ptr   user context passed back to plugin job system
    $:return            $OodleTex_Err indicating the error condition, or $OodleTex_Err_OK on success.

    $RateDistortionOptimization creates a BCN encoding such that it is smaller after Kraken compression.
    The smaller the after-compression result, the more error is introduced in the BCN encoding.  This quality vs
    size tradeoff is controlled by _rdo_lagrange_lambda_. This is the new entry point for the encoder with
    more encoder options.

    _rdo_lagrange_lambda_ is a scalar that provides fine control of the rate-quality balance.
    $OodleTex_RDOLagrangeLambda provides some example standard values.

    _rdo_lagrange_lambda_ = 0 means minimize distortion only, which produces the same encoding as $OodleTex_EncodeBCN_Blocks.
    As lambda gets higher, more effort is put into reducing rate at the cost of higher distortion.

    _options_ allows specifying more encoder options like the RDO effort level (controlling encoder speed).
    This is the major difference between this function and $OodleTex_EncodeBCN_RDO.

    Reads one or more surfaces _from_surfaces_ in _from_format_, writes _to_bcn_blocks_ in _to_bcn_ format.

    When _layout_ is NULL, the encoder produces blocks for all surfaces in row-major order.
    In that case, [num_blocks] must be the sum of $OodleTex_Surface_NumBlocks for all surfaces.
    Blocks for all surfaces are assumed to be consecutive.

    Specifying a _layout_ enables handling much more complicated scenarios such as interleaved hardware
    layouts for mip maps, texture arrays, cube maps or volume textures. Especially game consoles often
    expose the actual memory layout used by the hardware, which frequently interleaves data from multiple
    2D source images in complicated ways. When a _layout_ is specified, _num_blocks_ must match the number
    of blocks specified during $OodleTex_Layout_SetBlockLayout, and _num_from_surfaces_ must match the
    number of surfaces that make up said layout.

    _to_bcn_surface_ should be allocated to at least _num_blocks_ * $OodleTex_BC_BytesPerBlock.

    EncodeBCN_RDO will use multiple threads if a thread job system is installed via $OodleTex_Plugins_SetJobSystemAndCount.
    _num_job_threads_ should typically be set to $OODLETEX_JOBS_DEFAULT to use all workers installed, but a different
    number can be passed here to override what was set in SetJobSystemAndCount, or single-threaded execution
    on the current thread can be forced by passing $OODLETEX_JOBS_DISABLE.
*/

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_DecodeBCN_Blocks(
    void * to_pixel_blocks,OodleTex_PixelFormat to_format,OO_SINTa num_blocks,
    OodleTex_BC from_bcn,const void * from_bcn_blocks
    );
/* Decode Block Compressed surface to pixels in 4x4 block order.

    $:to_pixel_blocks   memory to write output pixels to 
    $:to_format         format of pixels in _to_pixel_blocks_
    $:num_blocks        number of blocks to process
    $:from_bcn          Block Compressed texture format contained in _from_bcn_blocks_
    $:from_bcn_blocks   array of BCN blocks to read
    $:return            $OodleTex_Err indicating the error condition, or $OodleTex_Err_OK on success.

    Decodes _num_blocks_ BCN blocks and writes them to _to_pixel_blocks_ in 4x4 block order.

    _to_format_ may be $OodleTex_BC_GetNaturalDecodeFormat or any other.

    Decoding of BC1-5 is only specified within error bounds, not precisely.  We decode to a compromise
    approximation in between AMD, Apple, Intel and NVidia GPU hardware.  The approximation is also within
    the allowed tolerances of the D3D12 reference specification.  This behavior has changed in Oodle 2.9.5,
    see $OodleTex_About_BCNDecoding for details.

    See also $OodleTex_DecodeBCN_LinearSurfaces.

*/
OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_DecodeBCN_Blocks_Ex(
    void* to_pixel_blocks, OodleTex_PixelFormat to_format, OO_SINTa num_blocks,
    OodleTex_BC from_bcn, const void* from_bcn_blocks, OodleTex_BCNDecodeFlags flags
);
/* Decode Block Compressed surface to pixels in 4x4 block order.

    $:to_pixel_blocks   memory to write output pixels to
    $:to_format         format of pixels in _to_pixel_blocks_
    $:num_blocks        number of blocks to process
    $:from_bcn          Block Compressed texture format contained in _from_bcn_blocks_
    $:from_bcn_blocks   array of BCN blocks to read
    $:flags             bit flags for additional options, see $OodleTex_BCNDecodeFlags
    $:return            $OodleTex_Err indicating the error condition, or $OodleTex_Err_OK on success.

    See $OodleTex_DecodeBCN_Blocks for more details.
*/

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_DecodeBCN_LinearSurfaces(
    OodleTex_Surface * to_surfaces,OO_SINTa num_to_surfaces,OodleTex_PixelFormat to_format,
    OodleTex_BC from_bcn,const void * from_bcn_blocks,OO_SINTa num_blocks,
    const OodleTex_Layout * layout
    );
/* Decode a collection of Block Compressed surfaces to pixels in linear row-major order.

    $:to_surfaces       array of $OodleTex_Surface instances of format _to_format_, get filled with decoded pixels
    $:num_to_surfaces   number of surfaces in _to_surfaces_
    $:to_format         pixel format to output
    $:from_bcn          Block Compressed texture format contained in _from_bcn_blocks_
    $:from_bcn_blocks   array of BCN blocks to read
    $:num_blocks        number of bcn blocks in _from_bcn_blocks_
    $:layout            $OodleTex_Layout describing arrangement of blocks in _from_bcn_ (may be NULL in certain cases)
    $:return            $OodleTex_Err indicating the error condition, or $OodleTex_Err_OK on success.

    Fills one or more of the _to_surfaces_ with the decoding of BCN data from _from_bcn_blocks_.

    When _num_to_surfaces_ is 1, _layout_ may be set to NULL, in which case the
    decoder assumes the blocks are in regular linear row-major order. In this case,
    _num_blocks_ must be ((width+3)/4) * ((height+3)/4), using the width and height
    of the single destination surface.

    Specifying a _layout_ enables handling much more complicated scenarios such as interleaved hardware
    layouts for mip maps, texture arrays, cube maps or volume textures. Especially game consoles often
    expose the actual memory layout used by the hardware, which frequently interleaves data from multiple
    2D source images in complicated ways. When a _layout_ is specified, _num_blocks_ must match the number
    of blocks specified during $OodleTex_Layout_SetBlockLayout, and _num_to_surfaces_ must match the number
    of surfaces that make up said layout.
    
    Decoding of BC1-5 is only specified within error bounds, not precisely.  We decode to a compromise
    approximation in between AMD, Apple, Intel and NVidia GPU hardware.  The approximation is also within
    the allowed tolerances of the D3D12 reference specification.  This behavior has changed in Oodle 2.9.5,
    see $OodleTex_About_BCNDecoding for details.

*/

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_DecodeBCN_LinearSurfaces_Ex(
    OodleTex_Surface* to_surfaces, OO_SINTa num_to_surfaces, OodleTex_PixelFormat to_format,
    OodleTex_BC from_bcn, const void* from_bcn_blocks, OO_SINTa num_blocks,
    const OodleTex_Layout* layout, OodleTex_BCNDecodeFlags flags
);

/* Decode a collection of Block Compressed surfaces to pixels in linear row-major order.

    $:to_surfaces       array of $OodleTex_Surface instances of format _to_format_, get filled with decoded pixels
    $:num_to_surfaces   number of surfaces in _to_surfaces_
    $:to_format         pixel format to output
    $:from_bcn          Block Compressed texture format contained in _from_bcn_blocks_
    $:from_bcn_blocks   array of BCN blocks to read
    $:num_blocks        number of bcn blocks in _from_bcn_blocks_
    $:layout            $OodleTex_Layout describing arrangement of blocks in _from_bcn_ (may be NULL in certain cases)
    $:flags             bit flags for additional options, see $OodleTex_BCNDecodeFlags
    $:return            $OodleTex_Err indicating the error condition, or $OodleTex_Err_OK on success.

    See $OodleTex_DecodeBCN_LinearSurfaces for more details.
*/


//idoc(parent,OodleAPI_TextureBC7Prep)

#ifndef OODLETEXRT_BC7PREP_DEFINED
#define OODLETEXRT_BC7PREP_DEFINED

// OodleTexRT_BC7PrepHeader is duplicated in the Oodle Texture RT header
// Documented on Texture runtime side

#define OODLETEXRT_BC7PREP_MODE_COUNT 10

typedef OOSTRUCT OodleTexRT_BC7PrepHeader
{
    OO_U32 version;
    OO_U32 flags;
    OO_U32 mode_counts[OODLETEXRT_BC7PREP_MODE_COUNT];
} OodleTexRT_BC7PrepHeader;

#endif // OODLETEXRT_BC7PREP_DEFINED

// bc7prep Encode functions are in Oodle Texture
// bc7prep Decode functions are in Oodle Texture RT

OOFUNC1 OO_SINTa OOFUNC2 OodleTex_BC7Prep_MinEncodeOutputSize(OO_SINTa nblocks);
/* Returns the amount of output buffer space required (in bytes) for the
   given number of BC7 blocks when encoding, or an $OodleTex_Err on error.
*/

OOFUNC1 OO_SINTa OOFUNC2 OodleTex_BC7Prep_MinEncodeScratchSize(OO_SINTa nblocks);
/* Returns the amount of scratch buffer space required (in bytes) for the
   given number of BC7 blocks when encoding, or an $OodleTex_Err on error.
*/

OOFUNC1 OO_SINTa OOFUNC2 OodleTex_BC7Prep_Encode(OodleTexRT_BC7PrepHeader * out_header,
    void * output_buf, OO_SINTa output_size,
    const void * input_bc7, OO_SINTa num_blocks,
    void * scratch_buf, OO_SINTa scratch_size);
/* Encodes a BC7Prep data stream.

   $:out_header     Where the header for the resulting stream gets placed.
   $:output_buf     Where the payload stream gets written to.
   $:output_size    Size of output_buf in bytes (see $OodleTex_BC7Prep_MinEncodeOutputSize)
   $:input_bc7      Pointer to the BC7 blocks to be encoded.
   $:num_blocks     Number of BC7 blocks to be encoded.
   $:scratch_buf    Pointer to a scratch working buffer.
   $:scratch_size   Size of scratch_buf in bytes (see $OodleTex_BC7Prep_MinEncodeScratchSize)
   $:return         Number of bytes written to output_buf on success, a negative number ($OodleTex_Err) on error.

   The header is also required to decode the data but is returned separately.

   BC7Prep _encoding_ lives in the Oodle Texture library. BC7Prep _decoding_ is implemented by the
   function $OodleTexRT_BC7Prep_Decode, which lives in the Oodle Texture Runtime library. If you ship
   a game or application that uses BC7Prep-processed textures, you need to include the runtime.
   The main Oodle Texture library should only ever be needed for tools.

   The scratch buffer is used for all working memory during the encode process. BC7Prep encoding
   does not perform any memory allocations. You can reuse the scratch memory between subsequent
   BC7Prep_Encode calls (as long as it's large enough), but two concurrent BC7Prep_Encodes running
   on different threads need to have their private scratch buffers.
*/

//idoc(parent,OodleAPI_TextureLayout)

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_Layout_Create(
    OodleTex_BC bcn_format,
    const OodleTex_Surface * surfaces,int num_surfaces,
    OodleTex_Layout ** out_layout
    );
/* Creates a texture layout object for a BCn format texture made up of a given number of surfaces.

    $:bcn_format        Block Compressed format for the destination texture
    $:surfaces          Pointer to an array of input surfaces. For layouts, only the widths and weights of the individual surfaces matter, pointer and stride are ignored
    $:num_surfaces      Number of elements in _surfaces_
    $:out_layout        Pointer to the resulting $OodleTex_Layout written on success (not modified on error)
    $:return            $OodleTex_Err indicating the error condition, or $OodleTex_Err_OK on success.

    The layout describes how the blocks of a compressed BCn texture will be laid out in memory and is
    assumed to depend on the target texture format and the number and size of the individual surfaces,
    but not their contents. Texture layouts tell Oodle Texture how exactly the blocks in a given texture
    will be laid out in memory for a particular piece of target hardware.

    The list of _surfaces_ here should generally contain all the 2D surfaces that make up a complete
    texture object:

    $* For a regular 2D texture, this means the original image, and, if present, its mip maps, as one
    surface each, in no particular order.
    $* For a 2D texture array or cubemap, this means the base images for each element of the array
    (or face of the cubemap), plus all their mip maps, all as individual surfaces, and again
    in no particular order.
    $* For a 3D (volume) texture with a given depth _d_, you would usually pass _d_ surfaces, one per 2D slice
    through the texture.
    $* Finally a 3D texture with mip maps would have _d_ surfaces for the base mip level, max(_d_ / 2, 1) surfaces
    for the next-smaller mip level, and so forth.

    The order of surfaces in the surface array does not matter; use whatever is convenient or natural
    for you. The only important thing is that all 2D image surfaces that get combined and interleaved
    in some way into the final memory layout for the texture object on your target hardware should be present.

    Texture layout objects do not need to be created anew for every texture. The same texture layout object
    can be used multiple times if you are compressing many textures with the same dimensions and number
    of mip levels.

    After creating a texture layout object, the layout object exists but is not immediately usable
    for texture encoding just yet; first, you need to declare what the actual physical layout is.
    This is accomplished through a combination of $OodleTex_Layout_GenerateBlockIDs and
    $OodleTex_Layout_SetBlockLayout. See those functions for details.
*/

OOFUNC1 void OOFUNC2 OodleTex_Layout_Destroy(OodleTex_Layout * layout);
/* Destroys a given OodleTex_Layout object and frees all associated memory.
*/

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_Layout_GenerateBlockIDs(
    const OodleTex_Layout * layout,
    int surface_index,
    void * block_ids, OO_SINTa block_id_row_stride_bytes
    );
/* Generates the contents of the block ID texture for a given surface.

    $:layout                    The layout object to use
    $:surface_index             Index of the surface in the layout object to generate block IDs for
    $:block_ids                 Pointer to the first row of the block ID texture to be generated
    $:block_id_row_stride_bytes Stride between rows (of blocks) in the block ID texture, in bytes
    $:return                    $OodleTex_Err indicating the error condition, or $OodleTex_Err_OK on success.

    The way Oodle Texture tracks which blocks go where in the physical layout is by using a trick:
    for each of the 4x4 pixel blocks in the original texture, this function generates an ID that
    identifies both the surface it came from and the position inside the surface. These IDs are
    stored as a (generally meaningless to look at) texture in the compressed target format. This
    ID image, or a collection of several of them, can then be passed to utility functions that
    transform a linear-layout texture into the actual hardware format. Finally, you pass the result
    of this process back to Oodle Texture using $OodleTex_Layout_SetBlockLayout; since each of the block
    IDs was unique, this lets us figure out exactly where each block came from.

    In short, block_ids is (in effect) a 2D image in the format specified at layout creation time.
    For BC1 and BC4, individual blocks are 8 bytes each; the remaining formats all have 16-byte blocks.
    The image is ceil(width/4) blocks wide and ceil(height/4) blocks high, where the width
    and height are the width and height of the surface identified by surface_index specified
    at $OodleTex_Layout_Create time.
*/

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_Layout_SetBlockLayout(
    OodleTex_Layout * layout,
    const void * reordered_block_ids, OO_SINTa num_reordered_blocks
    );
/* Passes the physical block layout of a texture back to Oodle Texture.

    $:layout                The layout object
    $:reordered_block_ids   Pointer to an array of block IDs, in hardware texture layout order
    $:num_reordered_blocks  The size of the reorderd block IDs array in number of BCn blocks
    $:return                $OodleTex_Err indicating the error condition, or $OodleTex_Err_OK on success.

    Read the descriptions of $OodleTex_Layout_Create and $OodleTex_Layout_GenerateBlockIDs for
    an overview of the overall process.

    After creating 2D block ID images for every surface in the texture layout, you have the
    contents of all the pieces making up the full texture. The idea is to then pass these
    images to a texture tiling function that converts images from the original linear order
    to the actual memory layout used by the hardware; this may in turn depend on other flags
    not known to OodleTex_Layout such as tiling modes. It often also ends up interleaving
    data from several volume slices, array slices, or even mip levels.

    Oodle Texture does not know about the particulars of every console's texture tiling modes.
    Instead, by letting the console SDK itself do the tiling on a synthetic block ID texture,
    we can just look at the result and figure out where each block came from, which is all
    we really need to know to encode blocks in the right order.

    However, producing the final texture layout often adds some amount of padding bytes,
    especially between the smaller mip maps and for non-power-of-2 sizes. For this process
    to work right, you _need_ to make sure those padding bytes are initialized and set to 0.
    If in doubt, just clearing the output buffer to 0 before calling the texture tiling
    function(s) should do the job.

    This function checks the resulting layout to make sure all the block IDs are legal
    and make sense; if it does, the array of reordered block IDs is copied and stored
    as part of the OodleTex_Layout object.

    On success, the OodleTex_Layout is fully initialized and can now be used to encode
    textures with.
*/


OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_Layout_CopyBCNToLinear(
    OodleTex_BC bcn,
    OodleTex_Surface * to_surfaces,OO_SINTa num_to_surfaces,
    const void * from_bcn_blocks,OO_SINTa num_blocks,
    const OodleTex_Layout * layout
    );
/* Take dense BCN blocks in Layout order and copy them out to row-major linear BCN block surfaces

    $:bcn               the block pixel format of both source and dest surfaces
    $:to_surfaces       array of $OodleTex_Surface instances of format _bcn_, get filled with decoded pixels
    $:num_to_surfaces   number of surfaces in _to_surfaces_
    $:from_bcn_blocks   array of BCN blocks to read
    $:num_blocks        number of bcn blocks in _from_bcn_blocks_
    $:layout            $OodleTex_Layout describing arrangement of blocks in _from_bcn_ (may be NULL in certain cases)
    $:return            $OodleTex_Err indicating the error condition, or $OodleTex_Err_OK on success.

    Fills one or more of the _to_surfaces_ with the BCN data from _from_bcn_blocks_.

    _to_surfaces_ should be BCN block surfaces in the same format as _from_bcn_blocks_.  _to_surfaces_ are written in
    row major linear order.

    _to_surfaces_ for BCN blocks should have the $OodleTex_Surface set up with _width_ and _height_ in pixels, but
    _rowStrideBytes_ should be a stride to the next row of blocks.
    
    This function is analogous to $OodleTex_DecodeBCN_LinearSurfaces but no decoding is done, the blocks are just
    copied over.

*/  

//idoc(parent,OodleAPI_TextureUtils)

OOFUNC1 OO_F32 OOFUNC2 OodleTex_RMSE_Normalized_BCNAware(
    OodleTex_BC was_bcn,
    const OodleTex_Surface * surf1,OodleTex_PixelFormat format1,
    const OodleTex_Surface * surf2,OodleTex_PixelFormat format2);
/* Compute per-pixel RMSE of normalized surfaces.

    $:was_bcn               the BCn compression that was used, $OodleTex_BC_Invalid if unknown
    $:surf1                 pixel surface to compare
    $:format1               $OodleTex_PixelFormat of _surf1_
    $:surf2                 pixel surface to compare
    $:format2               $OodleTex_PixelFormat of _surf2_
    $:return                per-pixel RMSE of the delta, normalized, less than zero for error

    Compute normalized RMSE of surfaces, aware of BCn compression.
    
    If you don't know how the surfaces were made, or how they should be interpreted, then
    just pass BC_Invalid for _was_bcn_.
    
    Note that we return RMSE per pixel, not per sample as many others do.
    This is because there is some ambiguity about counting the number of channels.
    
    "Normalized" means the two surfaces are converted to equal bit depth as UNORM/SNORM.
    For example, U8 and U16 pixels would be interpreted as 8-bit and 16-bit UNORM respectively,
    converted to the floating-point values they represent, and then the RMSE calculation
    is performed in that space.
    
    The returned RMSE is U8-scaled (same as if you did RMSE on byte images), which is to say,
    it's multiplied by 255 from the result of the RMSE calculation on floats. This is done purely
    because it makes typical error results for BCn formats conveniently scaled. RMSE values
    are calculated per pixel not per channel.
    
    For BC6, an HDR floating point metric is used, not linear RMSE.  This is also done if was_bcn is
    BC_Invalid and format1 and 2 are both floating point.
    
    "BCNAware" means on formats that ignore alpha (BC1 and BC7RGB), Alpha channel differences do
    not contribute to RMSE.  On formats that encode fewer channels (BC4,BC5) only the channels they encode are compared.

    If $OodleTex_BC_Invalid is passed, no BCN awareness is applied.  When "Invalid" is used, the RMSE
    is computed over the MIN of the channel count of the two surfaces.  For example, a BC7 surface compared
    to a 24-bit RGB image will get an RMSE over 3 channels with difference in the A channel ignored.

    If _was_bcn_ is passed, the channel count of the BCN will be used even if the surfaces do not
    contain them.  So if _was_bcn_ is BC7RGBA, a 4-channel difference (with A) is always computed, even if
    one of the surfaces is only 24-bit RGB (implicit A=255 is used there).  That is, passing "BC7RGBA"
    there instead of "BC7RGB" tells the RMSE you care about alpha preservation.
    
    This function can be used to RMSE block surfaces (instead of linear) if both _surf1_ and _surf2_
    are in block layout.

    Only pixels in the smaller of the two surfaces will be compared.  So if say one surface is not
    4x4 aligned, and the other is padded up to 4x4, then only the pixels in the original area will
    be compared.
    
    For _was_bcn_ == BC1_WithTransparency, alpha is converted to 0 or 255 at a 128 threshold for diffing.

    NOTE that _was_bcn_ is optional (pass OodleTex_BC_Invalid to disable it), and if you aren't sure what the right
    BCN to pass there is, it's better to pass Invalid.
    
    For example if you just have a BC7 file and you don't know if it was compressed for BC7RGB or BC7RGBA , then just pass
    BC_Invalid for _was_bcn_.
    
    The _was_bcn_ argument should be used when you specifically know how the BCN should be interpreted or how it
    was encoded and want to enable the special handling for that mode.

    See also $OodleTex_FAQ_RMSE

*/

OOFUNC1 OodleTex_Err OOFUNC2 OodleTex_BlitNormalized(
    OodleTex_Surface * to_surf,OodleTex_PixelFormat to_format,
    const OodleTex_Surface * from_surf,OodleTex_PixelFormat from_format);
/* Copy pixel surfaces and perform normalized format conversion.

    $:to_surf               pixel surface to write
    $:to_format             $OodleTex_PixelFormat of _to_surf_
    $:from_surf             pixel surface to read
    $:from_format           $OodleTex_PixelFormat of _from_surf_
    $:return                $OodleTex_Err indicating the error condition, or $OodleTex_Err_OK on success.

    Helper for changing surface formats.

    Treats the formats as normalized following UNORM/SNORM convention, so for example
    U8 255 would convert to OO_F32 1.0, which in turn would convert to S8 127.
*/

#ifdef _MSC_VER
#pragma warning(pop)
#pragma pack(pop, Oodle)
#endif

#endif // __OODLE2TEX_H_INCLUDED__
