// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// @cdep pre $cbtargetsse4

#include "bc4compress.h"
#include "rrmem.h"
#include "rrmath.h"
#include "templates/rrstl.h"
#include "vec128.inl"
#include <math.h>
#include <string.h>

OODLE_NS_START

// How errors in BC4 work:
//
// The BC4 encoder usually works in a fixed-point space [-0x1000,0x1000] representing
// -1.0 and +1.0, respectively, so the three values -1, 0, 1 are at exact nice integer
// values and the fixed-point scaling is sufficient to cover the 10-11 bits effective
// resolution we can coax out of BC4/BC5 in the best case. In the other direction,
// we'd like to be able to do most of our math with packed 16-bit integer operations,
// with squared per-pixel block errors fitting inside 32 bits.
//
// The maximum possible absolute difference between points in that interval is
// 0x2000 (1<<13), which squares to 0x4000000 (1<<26), and we have 16 pixels per
// block, so the largest possible error per block is 0x40000000 (1<<30), which
// also happens to be the largest power of 2 that fits in a 32-bit signed integer
// without overflowing. For BC5 with its two channels, a block could get twice
// that and should be evaluating errors using unsigned 32-bit integers when the
// two channels are considered simultaneously; however, we generally treat BC5
// as two BC4 blocks and not as an indivisible unit, so this is probably not much
// of an issue regardless.
//
// This covers the actual BC4/BC5 modes, signed and unsigned both. The final,
// separate mode is for BC3 alpha blocks, which unlike BC4/5 are specified to
// decode to 8 bits internal resolution only. HW implementations follow the spec
// here (as per our tests). That means that while for BC4/5, placing endpoints
// very close together is potentially useful to get higher resolution for the
// interpolated values, for BC3 alpha there's not much point. However, because
// BC3 has such a narrow value range and the endpoints are full resolution, it
// is often possible to reproduce 8-bit values _exactly_. To facilitate this,
// we stick with the original [0,255] value range for BC3 alpha mode and don't
// do any rescaling that would muddy the waters.
//
// In BC3 alpha mode,the maximum possible delta is 255 and we are nowhere near
// overflowing anything no matter what.

#define BC4_INIT_ERROR	0x7fffffff
#define BC4_SCALE_BITS	12 // any larger is in the overflow danger zone
#define BC4_MINV		(-(1 << BC4_SCALE_BITS))
#define BC4_MAXV		(1 << BC4_SCALE_BITS)
#define BC4_ROUND		(BC4_MAXV / 2)

struct BC4Indices
{
	U8 vals[16]; // in [0,7]

	void unpack(U64 inds)
	{
		for (int i = 0; i < 16; ++i)
		{
			vals[i] = static_cast<U8>(inds & 7);
			inds >>= 3;
		}
	}

	U64 pack() const
	{
		U64 packed = 0;
		for (int i = 15; i >= 0; i--)
		{
			RR_ASSERT(vals[i] < 8);
			packed = (packed << 3) | vals[i];
		}
		return packed;
	}
};

// Internal block representation: SNORM blocks
struct BC4BlockS
{
	typedef S8 Endpoint;
	typedef S16 Value;
	enum { type = BC4ValueType_SNorm };
	enum { minimum = BC4_MINV, maximum = BC4_MAXV };
	enum { minimum_q = -127, maximum_q = 127 };
	enum { value_mask = 127, sign_mask = 128 };

	// "Good enough" threshold: if we hit a block SSD of this
	// (or lower), we don't bother searching further.
	// In our scaled 12-bit + sign space, this means
	// we're essentially exact: adjacent quantized
	// SNORM8 endpoints are about 32 steps apart,
	// and this is 3/4 of that squared.
	enum { good_enough_ssd = 24*24 };

	S8 e0, e1; // endpoint values (_signed_ compare to determine block mode)
	BC4Indices inds;

	static inline bool in_endpoint_range(int x)	{ return x >= minimum_q && x <= maximum_q; }
	static inline bool is_extremal(int value)	{ return value <= minimum || value >= maximum; }
	static inline Endpoint quantize(int value)	{ return static_cast<Endpoint>((value * 127 + BC4_ROUND) >> BC4_SCALE_BITS); }
	static inline Value dequantize(Endpoint x)	{ return (Value)( (RR_MAX(x, -127) * maximum) / 127 ); }
	static inline Endpoint from_u8(U8 value)	{ return static_cast<Endpoint>((value & 127) - (value & 128)); /* sign extend: bit 7 has place value -128 */  }

	// Generate the block encoding
	inline U64 encode() const
	{
		U64 bits = inds.pack() << 16;
		bits |= (e0 & 255) << 0;
		bits |= (e1 & 255) << 8;
		return bits;
	}
};

// UNORM blocks
struct BC4BlockU
{
	typedef U8 Endpoint;
	typedef S16 Value;
	enum { type = BC4ValueType_UNorm };
	enum { minimum = 0, maximum = BC4_MAXV };
	enum { minimum_q = 0, maximum_q = 255 };
	enum { value_mask = 255, sign_mask = 0 };

	// "Good enough" threshold: if we hit a block SSD of this
	// (or lower), we don't bother searching further.
	// In our scaled 12-bit space, this means we're essentially exact:
	// adjacent quantized UNORM8 endpoints are about 16 steps apart,
	// and this is 3/4 of that squared
	enum { good_enough_ssd = 12*12 };

	U8 e0, e1; // endpoint values (_unsigned_ compare to determine block mode)
	BC4Indices inds;

	static inline bool in_endpoint_range(int x)	{ return x >= minimum_q && x <= maximum_q; }
	static inline bool is_extremal(int value)	{ return value <= minimum || value >= maximum; }
	static inline Endpoint quantize(int value)	{ return static_cast<Endpoint>((value * 255 + BC4_ROUND) >> BC4_SCALE_BITS); }
	static inline Value dequantize(Endpoint x)	{ return (Value)( (x * maximum) / 255 ); }
	static inline Endpoint from_u8(U8 value)	{ return static_cast<Endpoint>(value); }

	// Generate the block encoding
	inline U64 encode() const
	{
		U64 bits = inds.pack() << 16;
		bits |= (e0 & 255) << 0;
		bits |= (e1 & 255) << 8;
		return bits;
	}
};

// Alpha blocks
struct BC4BlockA
{
	typedef U8 Endpoint;
	typedef S16 Value;
	enum { type = BC4ValueType_Alpha };
	enum { minimum = 0, maximum = 255 };
	enum { minimum_q = 0, maximum_q = 255 };
	enum { value_mask = 255, sign_mask = 0 };

	// "Good enough" threshold: if we hit a block SSD of this
	// (or lower), we don't bother searching further.
	enum { good_enough_ssd = 0 };

	U8 e0, e1; // endpoint values (_unsigned_ compare to determine block mode)
	BC4Indices inds;

	static inline bool in_endpoint_range(int x)	{ return x >= minimum_q && x <= maximum_q; }
	static inline bool is_extremal(int value)	{ return value <= minimum || value >= maximum; }
	static inline Endpoint quantize(int value)	{ return static_cast<Endpoint>(value); }
	static inline Value dequantize(Endpoint x)	{ return (Value)x; }
	static inline Endpoint from_u8(U8 value)	{ return static_cast<Endpoint>(value); }

	// Generate the block encoding
	inline U64 encode() const
	{
		U64 bits = inds.pack() << 16;
		bits |= (e0 & 255) << 0;
		bits |= (e1 & 255) << 8;
		return bits;
	}
};

// Tag types for decoding
struct BC4Decode16_S
{
	typedef S8 Endpoint;
	typedef S16 Value;
	enum { minimum = -32767, maximum = 32767 };

	// SNORM8->float: max(x, -127) / 127.0
	// float->SNORM16: round(x * 32767.0)
	static inline Value dequantize(Endpoint x)	{ return (RR_MAX(x, -127) * 32767 + (x >= 0 ? 63 : -63)) / 127; }
	static inline Endpoint from_u8(U8 value)	{ return static_cast<Endpoint>((value & 127) - (value & 128)); /* sign extend: bit 7 has place value -128 */ }
};

struct BC4Decode16_U
{
	typedef U8 Endpoint;
	typedef U16 Value;
	enum { minimum = 0, maximum = 65535 };

	// UNORM8->float: x / 255.0
	// float->SNORM16: round(x * 65535.0)
	//
	// 65535 = 255 * 257, so round((x / 255) * 65535) = round(x * 257) = x * 257 since x is integer
	static inline Value dequantize(Endpoint x)	{ return x * 257; }
	static inline Endpoint from_u8(U8 value)	{ return static_cast<Endpoint>(value); }
};

struct BC4Decode8_A
{
	typedef U8 Endpoint;
	typedef U8 Value;
	enum { minimum = 0, maximum = 255 };

	static inline Value dequantize(Endpoint x)	{ return x; }
	static inline Endpoint from_u8(U8 value)	{ return value; }
};

// Returns whether the given block has 6 interpolated colors between the endpoints (true)
// or just 4 with two extremal values (false).
template<typename Tendpt>
static inline bool BC4_Is6Interp(Tendpt e0, Tendpt e1)
{
	// NOTE: the rule is the same for signed and unsigned data, but the
	// fact that the comparison is signed vs. unsigned makes a difference!
	return e0 > e1;
}

template<typename Tblock>
static void BC4_ComputePalette(typename Tblock::Value palette[8], typename Tblock::Endpoint e0, typename Tblock::Endpoint e1)
{
	typedef typename Tblock::Value Value;

	Value e0deq = Tblock::dequantize(e0);
	Value e1deq = Tblock::dequantize(e1);

	palette[0] = e0deq;
	palette[1] = e1deq;

	// TODO consider using something closer to actual HW interpolation formulas here (but which HW?)

	if (BC4_Is6Interp(e0, e1))
	{
		int base = 7 * e0deq;
		int diff = e1deq - e0deq;

		// 6 interpolated values
		palette[2] = static_cast<Value>((base + 1*diff) / 7);
		palette[3] = static_cast<Value>((base + 2*diff) / 7);
		palette[4] = static_cast<Value>((base + 3*diff) / 7);
		palette[5] = static_cast<Value>((base + 4*diff) / 7);
		palette[6] = static_cast<Value>((base + 5*diff) / 7);
		palette[7] = static_cast<Value>((base + 6*diff) / 7);
	}
	else
	{
		int base = 5 * e0deq;
		int diff = e1deq - e0deq;

		// 4 interpolated values
		palette[2] = static_cast<Value>((base + 1*diff) / 5);
		palette[3] = static_cast<Value>((base + 2*diff) / 5);
		palette[4] = static_cast<Value>((base + 3*diff) / 5);
		palette[5] = static_cast<Value>((base + 4*diff) / 5);
		palette[6] = Tblock::minimum;
		palette[7] = Tblock::maximum;
	}
}

#if defined(DO_BUILD_SSE4) || defined(DO_BUILD_NEON64)

// We only have one component, so min_i |x-pal[i]|^2 is equivalent to
// minimizing |x-pal[i]|. We can (_just_) do this staying in 16 bits
// the entire time.
static RADFORCEINLINE Vec128_U16 calc_bc4_pixel_error(const Vec128_S16 & pixels, const Vec128_S16 & pal_entry, const Vec128_U16 & index_bias)
{
	// Calculate |pixels-pal_entry|
	Vec128_U16 diff = abs_diff(pixels, pal_entry).u16();

	// Max difference is BC4_MAXV - BC4_MINV = 1 << (BC4_SCALE_BITS + 1) = 1 << 13, which is (annoyingly) _just_ too large
	// for what we're about to try, so clamp it (this should never actually make us choose the wrong thing)
	Vec128_U16 clamped = vmin(diff, Vec128_U16((1 << 13) - 1));

	// Now shift left by 3 and put the index bit in the low 3 bits
	Vec128_U16 result = clamped.sli<3>(index_bias);
	return result;
}

static RADFORCEINLINE U32 initial_err_dot_product(const Vec128_U16 & input0, const Vec128_U16 & input1)
{
	// Shift the errors down to remove best indices
	Vec128_S16 errs0 = input0.srl<3>().s16();
	Vec128_S16 errs1 = input1.srl<3>().s16();

#ifdef DO_BUILD_SSE4

	// Now compute sum of squared errors and sum between adjacent pairs
	Vec128_S32 result0 = errs0.madd(errs0);
	Vec128_S32 result1 = errs1.madd(errs1);
	return reduce_add(result0 + result1);

#else // defined(DO_BUILD_NEON64)

	// square to 32-bit & accumulate errors
	Vec128_S32 result = vmull_s16( vget_low_s16(errs0), vget_low_s16(errs0));
	result = vmlal_s16(result, vget_high_s16(errs0), vget_high_s16(errs0));
	result = vmlal_s16(result, vget_low_s16(errs1), vget_low_s16(errs1));
	result = vmlal_s16(result, vget_high_s16(errs1), vget_high_s16(errs1));
	return reduce_add(result);

#endif
}

template<typename Tblock>
static void BC4_FindIndices(const BC4SourceData& values, typename Tblock::Endpoint e0, typename Tblock::Endpoint e1, U32* pError, BC4Indices* pInds)
{
	S16 palette[8];
	BC4_ComputePalette<Tblock>(palette, e0, e1);

	Vec128_S16 v_palette = Vec128_S16::loadu(palette);
	Vec128_S16 v_pixels0 = Vec128_S16::loadu(&values.values[0]);
	Vec128_S16 v_pixels1 = Vec128_S16::loadu(&values.values[8]);

	Vec128_U16 v_besterr0 { 0xffff }; // UINT16_MAX
	Vec128_U16 v_besterr1 { 0xffff };

	Vec128_U16 v_index_bias { 0 };
	Vec128_U8 v_pal_shuffle = Vec128_U16(0x0100).u8(); // 0,1 repeating

	// Loop over palette entries and keep track of the lowest error for each pixel
	for (int index = 0; index < 8; index++)
	{
		Vec128_S16 v_pal_value = v_palette.u8().shuf(v_pal_shuffle).s16(); // broadcast current palette entry

		// Update "best possible error" estimates for all 16 pixels
		v_besterr0 = vmin(v_besterr0, calc_bc4_pixel_error(v_pixels0, v_pal_value, v_index_bias));
		v_besterr1 = vmin(v_besterr1, calc_bc4_pixel_error(v_pixels1, v_pal_value, v_index_bias));

		v_pal_shuffle += Vec128_U8(2); // advance to next palette entry
		v_index_bias += Vec128_U16(1); // increment current index
	}

	// Extract the best indices
	const Vec128_U16 v_index_mask { 7 };
	Vec128_U16 v_inds0 = v_besterr0 & v_index_mask;
	Vec128_U16 v_inds1 = v_besterr1 & v_index_mask;
	Vec128_U8 v_inds = v_inds0.to_uint8_sat(v_inds1);
	v_inds.storeu(pInds->vals);

	// Extract the sum of squared errors
	*pError = initial_err_dot_product(v_besterr0, v_besterr1);
}

#else

template<typename Tblock>
static void BC4_FindIndices(const BC4SourceData & values, typename Tblock::Endpoint e0, typename Tblock::Endpoint e1, U32 * pError, BC4Indices * pInds)
{
	S16 palette[8];
	U32 total_err = 0;

	BC4_ComputePalette<Tblock>(palette, e0, e1);

	for (int i = 0; i < 16; i++)
	{
		U32 best_err = BC4_INIT_ERROR;

		// just use brute force for now
		for (int j = 0; j < 8; j++)
		{
			int diff = values.values[i] - palette[j];
			U32 err = (static_cast<U32>(diff * diff) << 3) + j; // (err << 3) | index
			best_err = RR_MIN(best_err, err);
		}

		// save index and accumulate error
		pInds->vals[i] = best_err & 7;
		total_err += best_err >> 3;
	}

	*pError = total_err;
}

#endif

template<typename Tblock>
static bool BC4_Compress_EndPoints(Tblock * pBlock, const BC4SourceData & values, int e0, int e1, U32 * pError)
{
	RR_ASSERT( Tblock::in_endpoint_range( e0 ) );
	RR_ASSERT( Tblock::in_endpoint_range( e1 ) );

	U32 err;
	BC4Indices inds;
	BC4_FindIndices<Tblock>(values, static_cast<typename Tblock::Endpoint>(e0), static_cast<typename Tblock::Endpoint>(e1), &err, &inds);
	if ( err < *pError )
	{
		*pError = err;
		pBlock->e0 = static_cast<typename Tblock::Endpoint>(e0);
		pBlock->e1 = static_cast<typename Tblock::Endpoint>(e1);
		pBlock->inds = inds;
		return true;
	}

	return false;
}

template<typename Tpix>
static void BC5_Deinterleave(Tpix * RADRESTRICT destR, Tpix * RADRESTRICT destG, const Tpix * srcRG)
{
	for (int i = 0; i < 16; i++)
	{
		destR[i] = srcRG[i*2 + 0];
		destG[i] = srcRG[i*2 + 1];
	}
}

template<typename Tpix>
void BC4_ConvertSourcePixels(BC4SourceData * pBlock, const Tpix * pixels);

template<>
void BC4_ConvertSourcePixels(BC4SourceData * pBlock, const U8 * source8)
{
	pBlock->type = BC4ValueType_UNorm;
	for (int i = 0; i < 16; i++)
		pBlock->values[i] = static_cast<S16>(((source8[i] << BC4_SCALE_BITS) + 128) / 255);
}

template<>
void BC4_ConvertSourcePixels(BC4SourceData * pBlock, const S8 * source8)
{
	pBlock->type = BC4ValueType_SNorm;
	for (int i = 0; i < 16; i++)
	{
		int clamped = RR_MAX(source8[i], -127); // SNORM8 -128 means the same as -127
		int rounding = (clamped < 0) ? -64 : 64;
		pBlock->values[i] = static_cast<S16>(((clamped << BC4_SCALE_BITS) + rounding) / 127);
	}
}

template<>
void BC4_ConvertSourcePixels(BC4SourceData * pBlock, const U16 * source16)
{
	pBlock->type = BC4ValueType_UNorm;
	for (int i = 0; i < 16; i++)
		pBlock->values[i] = static_cast<S16>(((source16[i] << BC4_SCALE_BITS) + 32768) / 65535);
}

template<>
void BC4_ConvertSourcePixels(BC4SourceData * pBlock, const S16 * source16)
{
	pBlock->type = BC4ValueType_SNorm;
	for (int i = 0; i < 16; i++)
	{
		int clamped = RR_MAX(source16[i], -32767); // SNORM16 -32768 means the same as -32767
		int rounding = (clamped < 0) ? -16384 : 16384;
		pBlock->values[i] = static_cast<S16>(((clamped << BC4_SCALE_BITS) + rounding) / 32767);
	}
}

void BC4_ReadSourceFromBlock(BC4SourceData * pBlock, const void * src, BC4SourceFormat format)
{
	switch (format)
	{
	case BC4SourceFormat_Invalid:
		RR_BREAK();
		break;

	case BC4SourceFormat_U8:
		BC4_ConvertSourcePixels(pBlock, reinterpret_cast<const U8 *>(src));
		break;

	case BC4SourceFormat_S8:
		BC4_ConvertSourcePixels(pBlock, reinterpret_cast<const S8 *>(src));
		break;

	case BC4SourceFormat_U16:
		BC4_ConvertSourcePixels(pBlock, reinterpret_cast<const U16 *>(src));
		break;

	case BC4SourceFormat_S16:
		BC4_ConvertSourcePixels(pBlock, reinterpret_cast<const S16 *>(src));
		break;

	case BC4SourceFormat_RGBA_U8:
		{
			pBlock->type = BC4ValueType_Alpha;
			const U8 * rgba = (const U8 *)src;
			for (int i = 0; i < 16; ++i)
				pBlock->values[i] = rgba[i*4 + 3]; // alpha channel
		}
		break;
	}
}

void BC5_ReadSourceFromBlock(BC4SourceData * pBlockR, BC4SourceData * pBlockG, const void * block, BC4SourceFormat format)
{
	U8 source8_r[4*4];
	U8 source8_g[4*4];
	U16 source16_r[4*4];
	U16 source16_g[4*4];

	switch (format)
	{
	case BC4SourceFormat_Invalid:
		RR_BREAK();
		break;

	case BC4SourceFormat_U8:
		BC5_Deinterleave(source8_r, source8_g, reinterpret_cast<const U8*>(block));
		BC4_ConvertSourcePixels(pBlockR, source8_r);
		BC4_ConvertSourcePixels(pBlockG, source8_g);
		break;

	case BC4SourceFormat_S8:
		BC5_Deinterleave(source8_r, source8_g, reinterpret_cast<const U8*>(block));
		BC4_ConvertSourcePixels(pBlockR, reinterpret_cast<const S8 *>(source8_r));
		BC4_ConvertSourcePixels(pBlockG, reinterpret_cast<const S8 *>(source8_g));
		break;

	case BC4SourceFormat_U16:
		BC5_Deinterleave(source16_r, source16_g, reinterpret_cast<const U16*>(block));
		BC4_ConvertSourcePixels(pBlockR, source16_r);
		BC4_ConvertSourcePixels(pBlockG, source16_g);
		break;

	case BC4SourceFormat_S16:
		BC5_Deinterleave(source16_r, source16_g, reinterpret_cast<const U16*>(block));
		BC4_ConvertSourcePixels(pBlockR, reinterpret_cast<const S16 *>(source16_r));
		BC4_ConvertSourcePixels(pBlockG, reinterpret_cast<const S16 *>(source16_g));
		break;

	case BC4SourceFormat_RGBA_U8:
		RR_BREAK(); // will never be supported
		break;
	}
}

template<typename Tblock>
static void BC4_QuantizePair(typename Tblock::Endpoint * pELo, typename Tblock::Endpoint * pEHi, int lo, int hi)
{
	*pELo = Tblock::quantize(lo);
	*pEHi = Tblock::quantize(hi);

	if ( *pELo == *pEHi )
	{
		// if we quantized both to the same value, push them apart again
		int dequant = Tblock::dequantize(*pELo);
		if ( 2*dequant - (lo + hi) >= 0 && *pELo > Tblock::minimum_q ) // dequant >= avg(lo,hi); decrease minimum
			*pELo -= 1;
		else // dequant < avg(lo,hi) or quantized point was already min; increase maximum
			*pEHi += 1;
	}
}

template<typename Tblock>
static void BC4_CompressInternal_1(Tblock * pBlock, U32 * pError, const BC4SourceData & values, rrDXTCOptions options)
{
	typedef typename Tblock::Endpoint Endpoint;
	RR_ASSERT( Tblock::type == values.type );

	// set initial error high so we're guaranteed to take something
	*pError = BC4_INIT_ERROR;

	S32 sum = 0;
	S32 sumSqr = 0;
	S32 lo = Tblock::maximum;
	S32 hi = Tblock::minimum;

	// Extremal tells us how many of the values are extremes
	int extremal = 0;
	S32 notmax_sum = 0;
	S32 notmax_sumSqr = 0;
	S32 notmax_lo = Tblock::maximum;
	S32 notmax_hi = Tblock::minimum;

	for (int i = 0; i < 16; i++)
	{
		S16 val = values.values[i];

		sum += val;
		sumSqr += val*val;

		hi = RR_MAX(hi, val);
		lo = RR_MIN(lo, val);

		if ( Tblock::is_extremal( val ) )
		{
			extremal++;
		}
		else
		{
			notmax_sum += val;
			notmax_sumSqr += val*val;
			notmax_hi = RR_MAX(notmax_hi, val);
			notmax_lo = RR_MIN(notmax_lo, val);
		}
	}

	// Fix notmax_lo in case we didn't see any non-extremal:
	notmax_lo = RR_MIN(notmax_lo, notmax_hi);

	// If we had any extremal points, try using the 4-interp version
	Endpoint quant_notmax_lo, quant_notmax_hi;
	BC4_QuantizePair<Tblock>(&quant_notmax_lo,&quant_notmax_hi,notmax_lo,notmax_hi);
	if ( extremal != 0 )
	{
		RR_ASSERT( ! BC4_Is6Interp(quant_notmax_lo, quant_notmax_hi) );

		if ( BC4_Compress_EndPoints(pBlock,values,quant_notmax_lo,quant_notmax_hi,pError) )
		{
			if ( *pError == 0 )
				return;
		}

		// NOTE: can't assume that quant_lo is >min or quant_hi is <max for the endpoint
		// range necessarily, since notmax_lo and notmax_hi might be so close to the
		// actual max that they round to their maximum values.
	}

	// Try min/max as endpoints
	Endpoint quant_lo, quant_hi;
	BC4_QuantizePair<Tblock>(&quant_lo,&quant_hi,lo,hi);

	/* // NOT USEFUL:

	// If the values are close together, try an "exact fit" (or what would be one in 8 bits).
	// Not exact for our extra precision but still throw the option in.
	if ( (quant_hi - quant_lo) <= 7 )
	{
		int mid = RR_CLAMP((quant_hi + quant_lo)/2, Tblock::minimum_q+3, Tblock::maximum_q-4);
		int tlo = mid - 3;
		int thi = mid + 4;

		RR_ASSERT( tlo <= (int)quant_lo );
		RR_ASSERT( thi >= (int)quant_hi );

		BC4_Compress_EndPoints(pBlock,values,thi,tlo,pError);
	}
	*/

	// No special case here when values are close together, since with >8 bit input we have no guarantee
	// of being able to hit values exactly anyway.
	if ( BC4_Compress_EndPoints(pBlock,values,quant_hi,quant_lo,pError) )
	{
		if ( *pError <= Tblock::good_enough_ssd )
			return;
	}

	S32 avg, variance;
	if ( extremal < 4 )
	{
		avg = (sum + 8) >> 4;
		variance = (sumSqr - 2*avg*sum + 16*avg*avg + 8) >> 4;
	}
	else
	{
		RR_ASSERT( extremal != 16 );

		S32 notmax_count = 16 - extremal;
		avg = (notmax_sum + notmax_count/2) / notmax_count;
		variance = (notmax_sumSqr - 2*avg*notmax_sum + notmax_count*avg*avg + notmax_count/2) / notmax_count;
	}

	S32 sdev = rr_froundint( sqrtf( (float)variance ) );
	// trying too small spreads is pointless
	sdev = RR_MAX(sdev, (Tblock::maximum - Tblock::minimum) / 128);

	for (int pass = 0; pass < 8; pass++)
	{
		S32 spread = (sdev * (8 + pass)) / 8;
		
		// e0 > e1 = 6 color mode
		S32 e1 = avg - spread;
		S32 e0 = avg + spread;

		// clamp to valid range
		e1 = RR_MAX(e1, Tblock::minimum);
		e0 = RR_MIN(e0, Tblock::maximum);

		// quantize
		Endpoint quant_e0, quant_e1;
		BC4_QuantizePair<Tblock>(&quant_e1,&quant_e0,e1,e0);

		// NOTE: no assert of 6-interp mode here because we could end up with quant_e0 == quant_e1 after quant
		if ( extremal )
		{
			// Try 4-interp mode
			if ( BC4_Compress_EndPoints(pBlock,values,quant_e1,quant_e0,pError) )
			{
				if ( *pError <= Tblock::good_enough_ssd )
					return;
			}

			if ( options & rrDXTCOptions_BC345_PreserveExtremes )
			{
				// for 6-interp
				if ( hi == Tblock::maximum ) quant_e0 = Tblock::maximum_q;
				if ( lo == Tblock::minimum ) quant_e1 = Tblock::minimum_q;
			}
		}

		if ( BC4_Compress_EndPoints(pBlock,values,quant_e0,quant_e1,pError) )
		{
			if ( *pError <= Tblock::good_enough_ssd )
				return;
		}

		// no point going wider than that
		if ( quant_e0 - quant_e1 >= 254 )
			break;
	}
}

template<typename Tblock>
static void BC4_OptimizeEndPointsFromIndices(Tblock * pBlock, U32 * pError, const BC4SourceData & values, rrDXTCOptions options, bool iterate)
{
	typedef typename Tblock::Endpoint Endpoint;
	bool sixi = BC4_Is6Interp(pBlock->e0, pBlock->e1);

	// integers so we have machine numbers for weights
	// everything is scaled
	static const float c_sixi_weights[8][2] =
	{
		{ 7.f,0.f },
		{ 0.f,7.f },
		{ 6.f,1.f },
		{ 5.f,2.f },
		{ 4.f,3.f },
		{ 3.f,4.f },
		{ 2.f,5.f },
		{ 1.f,6.f }
	};

	static const float c_fouri_weights[8][2] =
	{
		{ 5.f,0.f },
		{ 0.f,5.f },
		{ 4.f,1.f },
		{ 3.f,2.f },
		{ 2.f,3.f },
		{ 1.f,4.f },
		{ 0.f,0.f },
		{ 0.f,0.f }
	};

	const float (*pWeights)[2] = sixi ? c_sixi_weights : c_fouri_weights;

	do
	{
		float AA = 0;
		float AB = 0;
		float BB = 0;
		float AX = 0;
		float BX = 0;

		int lo = Tblock::maximum, hi = Tblock::minimum;

		for(int i=0;i<16;i++)
		{
			U32 index = pBlock->inds.vals[i];
			int val = values.values[i];

			lo = RR_MIN(lo,val);
			hi = RR_MAX(hi,val);

			const float X = (float) val;
			const float A = pWeights[index][0];
			const float B = pWeights[index][1];

			AA += A*A;
			BB += B*B;
			AB += A*B;

			AX += A*X;
			BX += B*X;
		}

		// all numbers here are small enough ints that these are machine numbers, even in float
		// {AA,BB} <= (7*7)*16 < 1024
		// AB <= (3*4)*16 < 256
		//
		// det is always an exact integer, and should be 0 iff all non-extremal indices (only an issue in four-interpolated-colors mode) are the same
		float det = AA*BB - AB*AB;
		if ( det == 0 )
			break;

		double invDet = (double)pWeights[0][0] / det;

		double fA = (BB * AX - AB * BX) * invDet;
		double fB = (AA * BX - AB * AX) * invDet;

		int iA = rr_froundint( (F32) fA );
		int iB = rr_froundint( (F32) fB );

		iA = RR_CLAMP(iA,Tblock::minimum,Tblock::maximum);
		iB = RR_CLAMP(iB,Tblock::minimum,Tblock::maximum);

		// e0 > e1 is sixi order
		Endpoint quant_e0, quant_e1;
		BC4_QuantizePair<Tblock>(&quant_e1, &quant_e0, RR_MIN(iA, iB), RR_MAX(iA, iB));
		if ( !sixi )
			RR_NAMESPACE::swap(quant_e0, quant_e1);

		RR_ASSERT( BC4_Is6Interp(quant_e0, quant_e1) == sixi );

		if ( sixi ) // non-sixi always has the extremal points
		{
			// keep extremes
			if ( options & rrDXTCOptions_BC345_PreserveExtremes )
			{
				if ( hi == Tblock::maximum ) quant_e0 = Tblock::maximum_q;
				if ( lo == Tblock::minimum ) quant_e1 = Tblock::minimum_q;
			}
		}

		if ( quant_e0 == pBlock->e0 && quant_e1 == pBlock->e1 )
			break;

		if ( ! BC4_Compress_EndPoints(pBlock,values,quant_e0,quant_e1,pError) )
			break;
	} while (iterate);
}

template<typename Tblock>
static void BC4_CompressInternal_2(Tblock * pBlock, U32 * pError, const BC4SourceData & values, rrDXTCOptions options)
{
	BC4_CompressInternal_1(pBlock, pError, values, options);
	if ( *pError <= Tblock::good_enough_ssd )
		return;

	BC4_OptimizeEndPointsFromIndices(pBlock, pError, values, options, true);
}

enum
{
	TryOptions_NeedMin		= 1<<0,
	TryOptions_NeedMax		= 1<<1,
	TryOptions_NeedExtremes	= TryOptions_NeedMin | TryOptions_NeedMax,
};

template<typename Tblock>
static bool BC4_EndpointsLegal(int e0, int e1, U32 options)
{
	if (options & TryOptions_NeedExtremes)
	{
		// In 6-interpolation mode, we need to do extra checks
		if (e0 > e1)
		{
			if ((options & TryOptions_NeedMin) && e1 != Tblock::minimum_q) return false;
			if ((options & TryOptions_NeedMax) && e0 != Tblock::maximum_q) return false;
		}
	}

	// It's always required for values to be in range
	return Tblock::in_endpoint_range(e0) && Tblock::in_endpoint_range(e1);
}

template<typename Tblock>
static bool BC4_TryModeFlip(Tblock * pBlock, U32 * pError, const BC4SourceData & values, U32 options)
{
	return BC4_EndpointsLegal<Tblock>(pBlock->e1,pBlock->e0,options) &&
		BC4_Compress_EndPoints(pBlock,values,pBlock->e1,pBlock->e0,pError);
}

struct Vec2
{
	int x, y;
};

template<typename Tblock>
static int BC4_PatternSearch(Tblock * pBlock, U32 * pError, const BC4SourceData & values, U32 options, int scale, const Vec2 * pattern, int count)
{
	int start0 = pBlock->e0;
	int start1 = pBlock->e1;
	int best = -1;

	for (int i = 0; i < count; i++)
	{
		int e0 = start0 + scale * pattern[i].x;
		int e1 = start1 + scale * pattern[i].y;
		if ( BC4_EndpointsLegal<Tblock>(e0,e1,options) &&
			 BC4_Compress_EndPoints(pBlock,values,e0,e1,pError) )
			best = i;
	}

	return best;
}

template<typename Tblock>
static int BC4_PatternSearchBoth(Tblock * pBlock, U32 * pError, const BC4SourceData & values, U32 options, int scale, const Vec2 * pattern, int count)
{
	int start0 = pBlock->e0;
	int start1 = pBlock->e1;
	int best = -1;

	for (int i = 0; i < count; i++)
	{
		int e0 = start0 + scale * pattern[i].x;
		int e1 = start1 + scale * pattern[i].y;
		if ( BC4_EndpointsLegal<Tblock>(e0,e1,options) &&
			 BC4_Compress_EndPoints(pBlock,values,e0,e1,pError) )
			 best = i;
		if ( BC4_EndpointsLegal<Tblock>(e1,e0,options) &&
			 BC4_Compress_EndPoints(pBlock,values,e1,e0,pError) )
			 best = i;
	}

	return best;
}

template<typename Tblock>
static bool BC4_IteratedHexSearch(Tblock * pBlock, U32 * pError, const BC4SourceData & values, U32 options, int scale)
{
	// The hexagon pattern has a trick: suppose we take a step to the right and check how the
	// search points in the next iteration line up with those from this iteration:
	// (0 = initial point, 1 = points visited in 1st iter, 2 = new points in 2nd iter,
	// [] = point that got chosen that iter)
	//
	//     1   1   2
	//   1   0  [1]  2
	//     1   1   2
	//
	// There are only 3 new points that get added after we make a move, the rest of the
	// hexagon is in parts of the space we already explored!
	//
	// By symmetry, this is the same for all 6 movement directions. Therefore, after taking
	// a step in one of the 6 directions, all 3 of the possible moves "backwards" from our
	// most recent direction address points we've already visited, and therefore don't
	// need to be visited again.
	static const Vec2 small_hex[8] = {
		{  2,  0 }, {  1, -2 }, { -1, -2 },
		{ -2,  0 }, { -1,  2 }, {  1,  2 },
		// copies of first 2 for wrap-around
		{  2,  0 }, {  1, -2 },
	};

	int dir = 0;
	int improved = BC4_PatternSearch(pBlock,pError,values,options,scale,small_hex,6);
	if ( improved < 0 )
		return false;

	do
	{
		// dir + improved is the index of the most recent direction we took
		// dir + improved - 1 == dir + improved + 5 (mod 6) is the first of the
		// "wedge" of 3 contiguous directions we can move from this point that
		// are not repeats of directions we visited in the previous step.
		dir = (dir + improved + 5) % 6;
		improved = BC4_PatternSearch(pBlock,pError,values,options,scale,small_hex + dir, 3);
	}
	while ( improved >= 0 );

	return true;
}

// I had "IteratedDiamondSearch" here but it's just never worth it in my tests

template<typename Tblock>
static bool BC4_IteratedSquareSearch(Tblock * pBlock, U32 * pError, const BC4SourceData & values, U32 options, int scale)
{
	// This one is a tad trickier than hex or diamond.
	// Here's the idea with the sequence of points visited
	// over multiple steps of the algorithm:
	//
	//  1  1  1  2
	//  1  0 [1] 2  3
	//  1  1  1 [2] 3
	//        3  3  3
	//
	// In step 1, we move right; the next iteration adds three new points
	// (numbered 2 above), the rest is just space we've already explored.
	// The other three cardinal directions work the same way.
	//
	// In step 2, we move down and to the right; a diagonal move uncovers
	// five new points. Again the other three diagonals work the same way.
	//
	// In both cases we have a "wedge" around the direction we just moved
	// in, but it's open wider for diagonal moves than it is for horizontal
	// or vertical ones.
	static const Vec2 square[8+4] = {
		{  1,  0 }, {  1, -1 }, {  0, -1 }, { -1, -1 },
		{ -1,  0 }, { -1,  1 }, {  0,  1 }, {  1,  1 },
		// copies of first 4 for wrap-around
		{  1,  0 }, {  1, -1 }, {  0, -1 }, { -1, -1 },
	};

	int improved = BC4_PatternSearch(pBlock,pError,values,options,scale,square,8);
	if ( improved < 0 )
		return false;

	int dir = 0;
	do
	{
		dir += improved;

		// now dir is the index of the direction we picked in the last search.
		// if even, we went along a cardinal direction; if odd, it was a diagonal.
		int parity = dir & 1;

		// even parity (parity==0) searches start at dir-1 and check 3 directions
		// odd parity (parity==1) searches start at dir-2 and check 5 directins
		dir = (dir - 1 - parity) & 7;
		improved = BC4_PatternSearch(pBlock,pError,values,options,scale, square + dir,3 + 2*parity);
	}
	while ( improved >= 0 );

	return true;
}

template<typename Tblock>
void BC4_GreedyOptimizeBlock2(Tblock * pBlock, U32 * pError, const BC4SourceData & values, rrDXTCOptions options)
{
	// corresponds to a SSD of 25 for 8-bit values
	static const int base_error = (Tblock::maximum - Tblock::minimum) * (Tblock::maximum - Tblock::minimum) / 2600;

	// Just a bunch of shifts
	static const Vec2 shifts[] = {
		{ -3, -3 }, { -2, -2 }, { -1, -1 },
		{  1,  1 }, {  2,  2 }, {  3,  3 },
	};

	// Large hexagonal search pattern
	static const Vec2 large_hex[12] = {
		{  4,  0 }, {  4,  2 }, {  2,  3 }, {  0,  4 }, { -2,  3 }, { -4,  2 },
		{ -4,  0 }, { -4, -2 }, { -2, -3 }, {  0, -4 }, {  2, -3 }, {  4, -2 },
	};

	U32 try_options = 0;
	if ( (pBlock->e1 == Tblock::minimum_q || pBlock->e0 <= pBlock->e1) && (options & rrDXTCOptions_BC345_PreserveExtremes) )
		try_options |= TryOptions_NeedMin;
	if ( (pBlock->e0 == Tblock::maximum_q || pBlock->e0 <= pBlock->e1) && (options & rrDXTCOptions_BC345_PreserveExtremes) )
		try_options |= TryOptions_NeedMax;

	if ( *pError >= base_error*2 )
	{
		// First just try shifting both endpoints a bit in the same direction
		if ( BC4_PatternSearch(pBlock,pError,values,try_options,4,shifts,RR_ARRAY_SIZE(shifts)) >= 0 )
			BC4_PatternSearch(pBlock,pError,values,try_options,1,shifts,RR_ARRAY_SIZE(shifts));

		// Try really long-range hex searches, but only if we're pretty far off
		if ( *pError >= base_error*4 )
		{
			if ( *pError >= base_error*16 )
			{
				// NOTE: the search range for this is _giant_
				while (BC4_PatternSearch(pBlock,pError,values,try_options,10,large_hex,12) >= 0);
			}

			BC4_PatternSearch(pBlock,pError,values,try_options,8,large_hex,12);

			// Consider changing modes aggressively once we think we're close
			BC4_TryModeFlip(pBlock,pError,values,try_options);
			BC4_PatternSearchBoth(pBlock,pError,values,try_options,4,large_hex,12);
		}

		// Refine with iterated small hex searches at decreasing scales
		for (int scale = 4; scale > 0; scale >>= 1)
			BC4_IteratedHexSearch(pBlock,pError,values,try_options,scale);
	}

	// After narrowing in on the region, try the opposite mode
	BC4_TryModeFlip(pBlock,pError,values,try_options);

	// Final refine using iterated square search
	BC4_IteratedSquareSearch(pBlock,pError,values,try_options,1);
}

template<typename Tblock>
static void BC4_CompressInternal_3(Tblock * pBlock, U32 * pError, const BC4SourceData & values, rrDXTCOptions options)
{
	BC4_CompressInternal_2(pBlock, pError, values, options);
	if ( *pError <= Tblock::good_enough_ssd )
		return;

	BC4_GreedyOptimizeBlock2(pBlock, pError, values, options);
}

template<typename Tblock>
static void BC4_TryBruteForce(Tblock * pBlock, U64 * pMinError, const BC4SourceData & values, int e0, int e1)
{
	typedef typename Tblock::Endpoint Endpoint;

	U32 err;
	BC4Indices inds;
	BC4_FindIndices<Tblock>(values,static_cast<Endpoint>(e0),static_cast<Endpoint>(e1),&err,&inds);

	U64 scaled_err = ((U64)err << 8) | RR_ABS(e1 - e0); // break ties in favor of more compact endpoints
	if ( scaled_err < *pMinError )
	{
		*pMinError = scaled_err;
		pBlock->e0 = static_cast<Endpoint>(e0);
		pBlock->e1 = static_cast<Endpoint>(e1);
		pBlock->inds = inds;
	}
}

template<typename Tblock>
static void BC4_CompressInternal_BruteForce(Tblock * pBlock, U32 * pError, const BC4SourceData & values, rrDXTCOptions options)
{
	U64 min_error = ~(U64)0;

	if ( options & rrDXTCOptions_BC345_PreserveExtremes )
	{
		// Find the min and max values in the block
		S32 lo = Tblock::maximum;
		S32 hi = Tblock::minimum;
		for (int i = 0; i < 16; i++)
		{
			S16 val = values.values[i];
			hi = RR_MAX(hi, val);
			lo = RR_MIN(lo, val);
		}

		// Do we have either extreme?
		if ( lo == Tblock::minimum || hi == Tblock::maximum )
		{
			// Yes! Do a restricted search.
			
			// We can always use all blocks with e0 <= e1 (4-interp mode)
			for (int e0 = Tblock::minimum_q; e0 <= Tblock::maximum_q; e0++)
			{
				for (int e1 = e0; e1 <= Tblock::maximum_q; e1++)
					BC4_TryBruteForce<Tblock>(pBlock,&min_error,values,e0,e1);
			}

			// We can also always use e0 = max, e1 = min (6-interp mode)
			BC4_TryBruteForce<Tblock>(pBlock,&min_error,values,Tblock::maximum_q,Tblock::minimum_q);

			// Can we vary the high end for 6-interp mode?
			if ( hi != Tblock::maximum )  // this implies lo == minimum (from the outer if)
			{
				for (int e0 = Tblock::minimum_q + 1; e0 <= Tblock::maximum_q; e0++)
					BC4_TryBruteForce<Tblock>(pBlock,&min_error,values,e0,Tblock::minimum_q);
			}

			// Can we vary the low end for 6-interp mode?
			if ( lo != Tblock::minimum ) // this implies hi == maximum (from the outer if)
			{
				for (int e1 = Tblock::minimum_q; e1 <= Tblock::maximum_q - 1; e1++)
					BC4_TryBruteForce<Tblock>(pBlock,&min_error,values,Tblock::maximum_q,e1);
			}

			*pError = (U32)(min_error >> 8);
			return;
		}
	}

	for (int e0 = Tblock::minimum_q; e0 <= Tblock::maximum_q; e0++)
	{
		for (int e1 = Tblock::minimum_q; e1 <= Tblock::maximum_q; e1++)
			BC4_TryBruteForce<Tblock>(pBlock,&min_error,values,e0,e1);
	}

	*pError = (U32)(min_error >> 8);
}

U32 BC4_Compress_1(U8 * dest, const BC4SourceData & values, rrDXTCOptions options)
{
	U64 out_bits = 0;
	U32 error = 0;

	switch ( values.type )
	{
	case BC4ValueType_UNorm:
		{
			BC4BlockU result;
			BC4_CompressInternal_1(&result, &error, values, options);
			out_bits = result.encode();
		}
		break;
	case BC4ValueType_SNorm:
		{
			BC4BlockS result;
			BC4_CompressInternal_1(&result, &error, values, options);
			out_bits = result.encode();
		}
		break;
	case BC4ValueType_Alpha:
		{
			BC4BlockA result;
			BC4_CompressInternal_1(&result, &error, values, options);
			out_bits = result.encode();
		}
		break;
	}

	RR_PUT64_LE_UNALIGNED(dest, out_bits);
	return error;
}

U32 BC4_Compress_2(U8 * dest, const BC4SourceData & values, rrDXTCOptions options)
{
	U64 out_bits = 0;
	U32 error = 0;

	switch ( values.type )
	{
	case BC4ValueType_UNorm:
		{
			BC4BlockU result;
			BC4_CompressInternal_2(&result, &error, values, options);
			out_bits = result.encode();
		}
		break;
	case BC4ValueType_SNorm:
		{
			BC4BlockS result;
			BC4_CompressInternal_2(&result, &error, values, options);
			out_bits = result.encode();
		}
		break;
	case BC4ValueType_Alpha:
		{
			BC4BlockA result;
			BC4_CompressInternal_2(&result, &error, values, options);
			out_bits = result.encode();
		}
		break;
	}

	RR_PUT64_LE_UNALIGNED(dest, out_bits);
	return error;
}

U32 BC4_Compress_3(U8 * dest, const BC4SourceData & values, rrDXTCOptions options)
{
	U64 out_bits = 0;
	U32 error = 0;

	switch ( values.type )
	{
	case BC4ValueType_UNorm:
		{
			BC4BlockU result;
			BC4_CompressInternal_3(&result, &error, values, options);
			out_bits = result.encode();
		}
		break;
	case BC4ValueType_SNorm:
		{
			BC4BlockS result;
			BC4_CompressInternal_3(&result, &error, values, options);
			out_bits = result.encode();
		}
		break;
	case BC4ValueType_Alpha:
		{
			BC4BlockA result;
			BC4_CompressInternal_3(&result, &error, values, options);
			out_bits = result.encode();
		}
		break;
	}

	RR_PUT64_LE_UNALIGNED(dest, out_bits);
	return error;
}

U32 BC4_Compress_BruteForce(U8 * dest, const BC4SourceData & values, rrDXTCOptions options)
{
	U64 out_bits = 0;
	U32 error = 0;

	switch ( values.type )
	{
	case BC4ValueType_UNorm:
		{
			BC4BlockU result = { };
			BC4_CompressInternal_BruteForce(&result, &error, values, options);
			out_bits = result.encode();
		}
		break;
	case BC4ValueType_SNorm:
		{
			BC4BlockS result = { };
			BC4_CompressInternal_BruteForce(&result, &error, values, options);
			out_bits = result.encode();
		}
		break;
	case BC4ValueType_Alpha:
		{
			BC4BlockA result = { };
			BC4_CompressInternal_BruteForce(&result, &error, values, options);
			out_bits = result.encode();
		}
		break;
	}

	RR_PUT64_LE_UNALIGNED(dest, out_bits);
	return error;
}

void BC4_Compress(U8 * dest, const BC4SourceData & values, rrDXTCLevel level, rrDXTCOptions options)
{
	if ( level <= rrDXTCLevel_Fast )
		BC4_Compress_1(dest, values, options);
	else if ( level == rrDXTCLevel_Slow )
		BC4_Compress_2(dest, values, options);
	else if ( level == rrDXTCLevel_VerySlow )
		BC4_Compress_3(dest, values, options);
	else
		BC4_Compress_BruteForce(dest, values, options);
}

template<typename Tblock>
static U32 BC4_FindIndicesAndEncodeInternal(Tblock * pBlock, const BC4SourceData & values, U8 ep0, U8 ep1)
{
	pBlock->e0 = Tblock::from_u8(ep0);
	pBlock->e1 = Tblock::from_u8(ep1);
	U32 err;
	BC4_FindIndices<Tblock>(values,pBlock->e0,pBlock->e1,&err,&pBlock->inds);

	return err;
}

U32 BC4_FindIndicesAndEncode(U8 * dest, const BC4SourceData & values, U8 ep0, U8 ep1)
{
	U64 out_bits = 0;
	U32 error = 0;

	switch ( values.type )
	{
	case BC4ValueType_UNorm:
		{
			BC4BlockU result = { };
			error = BC4_FindIndicesAndEncodeInternal(&result, values, ep0, ep1);
			out_bits = result.encode();
		}
		break;
	case BC4ValueType_SNorm:
		{
			BC4BlockS result = { };
			error = BC4_FindIndicesAndEncodeInternal(&result, values, ep0, ep1);
			out_bits = result.encode();
		}
		break;
	case BC4ValueType_Alpha:
		{
			BC4BlockA result = { };
			error = BC4_FindIndicesAndEncodeInternal(&result, values, ep0, ep1);
			out_bits = result.encode();
		}
		break;
	}

	RR_PUT64_LE_UNALIGNED(dest, out_bits);
	return error;
}

template<typename Tblock>
static RADFORCEINLINE void BC4_DecompressInternal(typename Tblock::Value result[16], const U8 * encoded)
{
	typedef typename Tblock::Endpoint Endpoint;
	typedef typename Tblock::Value Value;

	U64 block = RR_GET64_LE_UNALIGNED(encoded);
	Endpoint e0 = Tblock::from_u8(static_cast<U8>(block >> 0));
	Endpoint e1 = Tblock::from_u8(static_cast<U8>(block >> 8));
	Value palette[8];

	BC4_ComputePalette<Tblock>(palette, e0, e1);

	U64 inds = block >> 16;
	for (int i = 0; i < 16; i++)
	{
		result[i] = palette[inds & 7];
		inds >>= 3;
	}
}

void BC4A_Decompress(U8 result[64], const U8 * encoded)
{
	U8 temp[16];
	BC4_DecompressInternal<BC4Decode8_A>(temp, encoded);

	// Put in alpha channel of result
	for (int i = 0; i < 16; i++)
		result[i*4 + 3] = temp[i];
}

void BC4S_Decompress16(S16 result[16], const U8 * encoded)
{
	BC4_DecompressInternal<BC4Decode16_S>(result, encoded);
}

void BC4U_Decompress16(U16 result[16], const U8 * encoded)
{
	BC4_DecompressInternal<BC4Decode16_U>(result, encoded);
}

template<typename Tblock>
static void BC4_DescribeEncodingInternal(BC4EndpointEncoding * encoding)
{
	encoding->value_mask = Tblock::value_mask;
	encoding->sign_mask = Tblock::sign_mask;

	encoding->min_q = Tblock::minimum_q;
	encoding->max_q = Tblock::maximum_q;
	encoding->min_deq = Tblock::minimum;
	encoding->max_deq = Tblock::maximum;

	for (int i = 0; i < 256; i++)
	{
		int val = (i & Tblock::value_mask) - (i & Tblock::sign_mask);
		encoding->dequant[i] = Tblock::dequantize(static_cast<typename Tblock::Endpoint>(val));
	}
}

void BC4_DescribeEncoding(BC4ValueType value_type, BC4EndpointEncoding * out_encoding)
{
	switch ( value_type )
	{
	case BC4ValueType_UNorm: BC4_DescribeEncodingInternal<BC4BlockU>(out_encoding); break;
	case BC4ValueType_SNorm: BC4_DescribeEncodingInternal<BC4BlockS>(out_encoding); break;
	case BC4ValueType_Alpha: BC4_DescribeEncodingInternal<BC4BlockA>(out_encoding); break;
	default: RR_ASSERT_ALWAYS(!"bad value_type");
	}
}

OODLE_NS_END

