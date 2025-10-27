// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrcolor.h"

#ifdef __RADBIGENDIAN__
#error not endian correct
#endif

RR_NAMESPACE_START

const float c_srgb_byte_to_linear_float[256] = 
{
  0.000000f,0.000304f,0.000607f,0.000911f,0.001214f,0.001518f,0.001821f,0.002125f,0.002428f,0.002732f,0.003035f,0.003347f,0.003677f,0.004025f,0.004391f,0.004777f,
  0.005182f,0.005605f,0.006049f,0.006512f,0.006995f,0.007499f,0.008023f,0.008568f,0.009134f,0.009721f,0.010330f,0.010960f,0.011612f,0.012286f,0.012983f,0.013702f,
  0.014444f,0.015209f,0.015996f,0.016807f,0.017642f,0.018500f,0.019382f,0.020289f,0.021219f,0.022174f,0.023153f,0.024158f,0.025187f,0.026241f,0.027321f,0.028426f,
  0.029557f,0.030713f,0.031896f,0.033105f,0.034340f,0.035601f,0.036889f,0.038204f,0.039546f,0.040915f,0.042311f,0.043735f,0.045186f,0.046665f,0.048172f,0.049707f,
  0.051269f,0.052861f,0.054480f,0.056128f,0.057805f,0.059511f,0.061246f,0.063010f,0.064803f,0.066626f,0.068478f,0.070360f,0.072272f,0.074214f,0.076185f,0.078187f,
  0.080220f,0.082283f,0.084376f,0.086500f,0.088656f,0.090842f,0.093059f,0.095307f,0.097587f,0.099899f,0.102242f,0.104616f,0.107023f,0.109462f,0.111932f,0.114435f,
  0.116971f,0.119538f,0.122139f,0.124772f,0.127438f,0.130137f,0.132868f,0.135633f,0.138432f,0.141263f,0.144129f,0.147027f,0.149960f,0.152926f,0.155926f,0.158961f,
  0.162029f,0.165132f,0.168269f,0.171441f,0.174647f,0.177888f,0.181164f,0.184475f,0.187821f,0.191202f,0.194618f,0.198069f,0.201556f,0.205079f,0.208637f,0.212231f,
  0.215861f,0.219526f,0.223228f,0.226966f,0.230740f,0.234551f,0.238398f,0.242281f,0.246201f,0.250158f,0.254152f,0.258183f,0.262251f,0.266356f,0.270498f,0.274677f,
  0.278894f,0.283149f,0.287441f,0.291771f,0.296138f,0.300544f,0.304987f,0.309469f,0.313989f,0.318547f,0.323143f,0.327778f,0.332452f,0.337164f,0.341914f,0.346704f,
  0.351533f,0.356400f,0.361307f,0.366253f,0.371238f,0.376262f,0.381326f,0.386430f,0.391573f,0.396755f,0.401978f,0.407240f,0.412543f,0.417885f,0.423268f,0.428691f,
  0.434154f,0.439657f,0.445201f,0.450786f,0.456411f,0.462077f,0.467784f,0.473532f,0.479320f,0.485150f,0.491021f,0.496933f,0.502887f,0.508881f,0.514918f,0.520996f,
  0.527115f,0.533277f,0.539480f,0.545725f,0.552012f,0.558341f,0.564712f,0.571125f,0.577581f,0.584079f,0.590619f,0.597202f,0.603828f,0.610496f,0.617207f,0.623961f,
  0.630757f,0.637597f,0.644480f,0.651406f,0.658375f,0.665387f,0.672443f,0.679543f,0.686685f,0.693872f,0.701102f,0.708376f,0.715694f,0.723055f,0.730461f,0.737911f,
  0.745404f,0.752942f,0.760525f,0.768151f,0.775822f,0.783538f,0.791298f,0.799103f,0.806952f,0.814847f,0.822786f,0.830770f,0.838799f,0.846873f,0.854993f,0.863157f,
  0.871367f,0.879623f,0.887923f,0.896270f,0.904661f,0.913099f,0.921582f,0.930111f,0.938686f,0.947307f,0.955974f,0.964687f,0.973445f,0.982251f,0.991102f,1.000000f
};

#if 0
const float c_linear_float_srgb_thresholds[257] = 
{
  -999999.f,0.000152f,0.000455f,0.000759f,0.001062f,0.001366f,0.001669f,0.001973f,0.002276f,0.002580f,0.002884f,0.003188f,0.003509f,0.003848f,0.004206f,0.004582f,
  0.004977f,0.005391f,0.005825f,0.006278f,0.006751f,0.007245f,0.007759f,0.008293f,0.008848f,0.009425f,0.010023f,0.010642f,0.011283f,0.011947f,0.012632f,0.013340f,
  0.014070f,0.014823f,0.015600f,0.016399f,0.017222f,0.018068f,0.018938f,0.019832f,0.020751f,0.021693f,0.022661f,0.023652f,0.024669f,0.025711f,0.026778f,0.027870f,
  0.028988f,0.030132f,0.031301f,0.032497f,0.033719f,0.034967f,0.036242f,0.037544f,0.038872f,0.040227f,0.041610f,0.043020f,0.044457f,0.045922f,0.047415f,0.048936f,
  0.050484f,0.052062f,0.053667f,0.055301f,0.056963f,0.058655f,0.060375f,0.062124f,0.063903f,0.065711f,0.067548f,0.069415f,0.071312f,0.073239f,0.075196f,0.077183f,
  0.079200f,0.081247f,0.083326f,0.085434f,0.087574f,0.089745f,0.091946f,0.094179f,0.096443f,0.098739f,0.101066f,0.103425f,0.105816f,0.108238f,0.110693f,0.113180f,
  0.115699f,0.118251f,0.120835f,0.123451f,0.126101f,0.128783f,0.131498f,0.134247f,0.137028f,0.139843f,0.142692f,0.145574f,0.148489f,0.151439f,0.154422f,0.157439f,
  0.160491f,0.163577f,0.166697f,0.169851f,0.173040f,0.176264f,0.179522f,0.182815f,0.186144f,0.189507f,0.192905f,0.196339f,0.199808f,0.203313f,0.206853f,0.210429f,
  0.214041f,0.217689f,0.221373f,0.225092f,0.228848f,0.232641f,0.236470f,0.240335f,0.244237f,0.248175f,0.252151f,0.256163f,0.260212f,0.264299f,0.268422f,0.272583f,
  0.276781f,0.281017f,0.285290f,0.289601f,0.293950f,0.298336f,0.302761f,0.307223f,0.311724f,0.316263f,0.320840f,0.325456f,0.330110f,0.334803f,0.339534f,0.344304f,
  0.349114f,0.353962f,0.358849f,0.363775f,0.368740f,0.373745f,0.378789f,0.383873f,0.388996f,0.394159f,0.399362f,0.404604f,0.409887f,0.415209f,0.420571f,0.425974f,
  0.431417f,0.436900f,0.442424f,0.447989f,0.453593f,0.459239f,0.464925f,0.470653f,0.476421f,0.482230f,0.488080f,0.493972f,0.499905f,0.505879f,0.511894f,0.517952f,
  0.524050f,0.530191f,0.536373f,0.542597f,0.548863f,0.555171f,0.561521f,0.567913f,0.574347f,0.580824f,0.587343f,0.593905f,0.600509f,0.607156f,0.613846f,0.620578f,
  0.627354f,0.634172f,0.641033f,0.647937f,0.654885f,0.661876f,0.668910f,0.675987f,0.683109f,0.690273f,0.697482f,0.704734f,0.712029f,0.719369f,0.726753f,0.734180f,
  0.741652f,0.749168f,0.756728f,0.764332f,0.771981f,0.779675f,0.787412f,0.795195f,0.803022f,0.810894f,0.818811f,0.826772f,0.834779f,0.842831f,0.850927f,0.859069f,
  0.867257f,0.875489f,0.883767f,0.892091f,0.900460f,0.908874f,0.917335f,0.925841f,0.934393f,0.942990f,0.951634f,0.960324f,0.969060f,0.977842f,0.986671f,0.995545f,
  9999999.f
};
#endif


// From https://gist.github.com/rygorous/2203834

typedef union
{
    U32 u;
    float f;
} stbir__FP32;

static const U32 fp32_to_srgb8_tab4[104] = {
    0x0073000d, 0x007a000d, 0x0080000d, 0x0087000d, 0x008d000d, 0x0094000d, 0x009a000d, 0x00a1000d,
    0x00a7001a, 0x00b4001a, 0x00c1001a, 0x00ce001a, 0x00da001a, 0x00e7001a, 0x00f4001a, 0x0101001a,
    0x010e0033, 0x01280033, 0x01410033, 0x015b0033, 0x01750033, 0x018f0033, 0x01a80033, 0x01c20033,
    0x01dc0067, 0x020f0067, 0x02430067, 0x02760067, 0x02aa0067, 0x02dd0067, 0x03110067, 0x03440067,
    0x037800ce, 0x03df00ce, 0x044600ce, 0x04ad00ce, 0x051400ce, 0x057b00c5, 0x05dd00bc, 0x063b00b5,
    0x06970158, 0x07420142, 0x07e30130, 0x087b0120, 0x090b0112, 0x09940106, 0x0a1700fc, 0x0a9500f2,
    0x0b0f01cb, 0x0bf401ae, 0x0ccb0195, 0x0d950180, 0x0e56016e, 0x0f0d015e, 0x0fbc0150, 0x10630143,
    0x11070264, 0x1238023e, 0x1357021d, 0x14660201, 0x156601e9, 0x165a01d3, 0x174401c0, 0x182401af,
    0x18fe0331, 0x1a9602fe, 0x1c1502d2, 0x1d7e02ad, 0x1ed4028d, 0x201a0270, 0x21520256, 0x227d0240,
    0x239f0443, 0x25c003fe, 0x27bf03c4, 0x29a10392, 0x2b6a0367, 0x2d1d0341, 0x2ebe031f, 0x304d0300,
    0x31d105b0, 0x34a80555, 0x37520507, 0x39d504c5, 0x3c37048b, 0x3e7c0458, 0x40a8042a, 0x42bd0401,
    0x44c20798, 0x488e071e, 0x4c1c06b6, 0x4f76065d, 0x52a50610, 0x55ac05cc, 0x5892058f, 0x5b590559,
    0x5e0c0a23, 0x631c0980, 0x67db08f6, 0x6c55087f, 0x70940818, 0x74a007bd, 0x787d076c, 0x7c330723,
};
 
static U8 stbir__linear_to_srgb_uchar_fast(float in)
{
    static const stbir__FP32 almostone = { 0x3f7fffff }; // 1-eps
    static const stbir__FP32 minval = { (127-13) << 23 };
    U32 tab,bias,scale,t;
    stbir__FP32 f;
 
    // Clamp to [2^(-13), 1-eps]; these two values map to 0 and 1, respectively.
    // The tests are carefully written so that NaNs map to 0, same as in the reference
    // implementation.
    if (!(in > minval.f)) // written this way to catch NaNs
        in = minval.f;
    if (in > almostone.f)
        in = almostone.f;
 
    // Do the table lookup and unpack bias, scale
    f.f = in;
    tab = fp32_to_srgb8_tab4[(f.u - minval.u) >> 20];
    bias = (tab >> 16) << 9;
    scale = tab & 0xffff;
 
    // Grab next-highest mantissa bits and perform linear interpolation
    t = (f.u >> 12) & 0xff;
    return (unsigned char) ((bias + scale*t) >> 16);
}

U8 linear_unit_float_to_srgb_byte(float f)
{
	// stbir__linear_to_srgb_uchar_fast is an approximation of the exact quantization boundaries
	//   but it is very close

	/**
	
	max relative error percent = 0.11%
	i = 1
	thresholds[i] = 0.000456
	c_linear_float_srgb_thresholds[i] = 0.000455

	very small difference

	**/

	// so just do it without correction

	return stbir__linear_to_srgb_uchar_fast(f);
}

#if 0
U8 linear_unit_float_to_srgb_byte(float f)
{
	// slight modification of stbir__linear_to_srgb_uchar_fast
	//	which makes the gamma thresholds exactly match the reference formula :
	
	int v = stbir__linear_to_srgb_uchar_fast(f - 0.000264f);
	
	v += ( f > c_linear_float_srgb_thresholds[v+1] );
	
	RR_ASSERT( f >= c_linear_float_srgb_thresholds[v] );
	RR_ASSERT( f <= c_linear_float_srgb_thresholds[v+1] );
	
	RR_ASSERT( v >= 0 && v <= 255 );
	
	return (U8)v;
}
#endif

//==============================

// CB 11-03-2020 :
//	remove dependence on CRT frexp/ldexp
//	because CRT is the devil
// -> the MUSL implementations are total crap, don't handle range checks correctly
// -> OpenLIBM looks better

static float musl_frexpf(float x, int *e)
{
	union { float f; U32 i; } y = { x };
	int ee = y.i>>23 & 0xff;

	if (!ee) {
		if (x) {
			x = musl_frexpf(x*0x1p64f, e);
			*e -= 64;
		} else *e = 0;
		return x;
	} else if (ee == 0xff) {
		return x;
	}

	*e = ee - 0x7e;
	y.i &= 0x807ffffful;
	y.i |= 0x3f000000ul;
	return y.f;
}

static float musl_ldexpf(float x, int n)
{
	union {float f; U32 i;} u;
	float y = x;

	if (n > 126) {
		y *= 0x1p126f;
		n -= 126;
		if (n > 126) {
			// should be HUGE_VALF
			return ( x > 0.f ) ? RR_F32_MAX : -RR_F32_MAX;
		}
	} else if (n < -126) {
		y *= 0x1p-126f * 0x1p24f;
		n += 126 - 24;
		if (n < -126) {
			// underflow
			return 0.f;
		}
	}
	u.i = (U32)(0x7f+n)<<23;
	x = y * u.f;
	return x;
}

void rrColor4F_From_RGBE_8888(rrColor4F * to, const U8 * from)
{  
	if ( from[3] != 0 )
	{
		// -8 does the 1/256
		F32 scale = musl_ldexpf(1.0, (int)from[3] - (128+8));
		// from Greg Ward's Radiance : note the 0.5's :
		to->r = (from[0] + 0.5f) * scale;
		to->g = (from[1] + 0.5f) * scale;
		to->b = (from[2] + 0.5f) * scale;
		to->a = 255.f;
	}
	else
	{
		to->r = to->g = to->b = to->a = 0.f;
	}
}

void rrColor4F_To_RGBE_8888(U8 * to, const rrColor4F * from)
{  
	float maxComponent = RR_MAX( RR_MAX(from->r, from->g), from->b );
	
	if (maxComponent < 1e-32f) 
	{
		to[0] = to[1] = to[2] = to[3] = 0;
	}
	else
	{
		int exp;
		// Greg Ward's radiance
		// color.c line 272 :
		//double scale = frexp(maxComponent,&exp) * 255.9999/maxComponent;
		// frexp returns in [0.5,1)
		// use 255.9999 instead of 256 just to make sure we never go to 256 exactly (if frexp is 0.999999..)
		// therefore no clamp to 255 is needed here :
		
        musl_frexpf(maxComponent, &exp);
        float scale = musl_ldexpf(1.f, -exp + 8);
        
		int toR = (int)( from->r * scale );
		int toG = (int)( from->g * scale );
		int toB = (int)( from->b * scale );
		// clamps are not needed
		to[0] = U8_check(toR);
		to[1] = U8_check(toG);
		to[2] = U8_check(toB);
		to[3] = (U8) (exp + 128);
	}
	
}


void rrColor4F_From_RGBE_9995(rrColor4F * to, const U32 from)
{  
	if ( from != 0 )
	{
		U32 fromR = (from>>23) & 0x1FF;
		U32 fromG = (from>>14) & 0x1FF;
		U32 fromB = (from>>5) & 0x1FF;
		U32 fromE = (from>>0) & 0x1F;
	
		// -9 does the 1/512;
		F32 scale = musl_ldexpf(1.0, (int)fromE - 16 - 9);
		to->r = (fromR + 0.5f) * scale;
		to->g = (fromG + 0.5f) * scale;
		to->b = (fromB + 0.5f) * scale;
		to->a = 255.f;
	}
	else
	{
		to->r = to->g = to->b = to->a = 0.f;
	}
}


void rrColor4F_To_RGBE_9995(U32 * to, const rrColor4F * from)
{  
	float maxComponent = RR_MAX( RR_MAX(from->r, from->g), from->b );
	
	if (maxComponent < 1e-6f) 
	{
		*to = 0;
	}
	else
	{
		int exp;
		musl_frexpf(maxComponent,&exp);
		F32 scale = musl_ldexpf(1.0, 9 - exp);
		int toR = (int)( from->r * scale );
		int toG = (int)( from->g * scale );
		int toB = (int)( from->b * scale );
		int toE = exp + 16;
		
		// @@ clamps shouldn't be needed
		toR = RR_CLAMP(toR,0,0x1FF);
		toG = RR_CLAMP(toG,0,0x1FF);
		toB = RR_CLAMP(toB,0,0x1FF);
		toE = RR_CLAMP(toE,0,0x1F);
		*to = (toR<<23) | (toG<<14) | (toB<<5) | toE;
	}
	
}


RR_NAMESPACE_END
