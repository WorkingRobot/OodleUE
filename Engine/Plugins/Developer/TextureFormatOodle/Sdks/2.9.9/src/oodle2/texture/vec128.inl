// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "oodlebase.h"
#include "rrmem.h"
#include "rrsimd.h"

OODLE_NS_START

#ifdef __RADSSE2__

typedef __m128i Vec128;
typedef __m128i const & Vec128r;

static inline Vec128 load32u(const void *ptr)			{ return _mm_cvtsi32_si128(RR_GET32_NATIVE_UNALIGNED(ptr)); }
static inline Vec128 load64u(const void *ptr)			{ return _mm_loadl_epi64((const __m128i *)ptr); }
static inline Vec128 load128u(const void *ptr)			{ return _mm_loadu_si128((const __m128i *)ptr); }
static inline Vec128 load128a(const void *ptr)			{ return _mm_load_si128((const __m128i *)ptr); }
static inline void store32u(void *ptr, Vec128r x)		{ RR_PUT32_NATIVE_UNALIGNED(ptr, _mm_cvtsi128_si32(x)); }
static inline void store64u(void *ptr, Vec128r x)		{ _mm_storel_epi64((__m128i *)ptr, x); }
static inline void store128u(void *ptr, Vec128r x)		{ _mm_storeu_si128((__m128i *)ptr, x); }
static inline void store128a(void *ptr, Vec128r x)		{ _mm_store_si128((__m128i *)ptr, x); }

static inline Vec128 vec_packed_add(Vec128r a, Vec128r b, Vec128r mask_nonmsb)
{
	// sum low bits directly
	Vec128 low = _mm_add_epi64(_mm_and_si128(a, mask_nonmsb), _mm_and_si128(b, mask_nonmsb));
	// carryless sum (=XOR) in MSBs
	return _mm_xor_si128(low, _mm_andnot_si128(mask_nonmsb, _mm_xor_si128(a, b)));
}

static inline Vec128 zext8to16_lo(Vec128r x)			{ return _mm_unpacklo_epi8(x, _mm_setzero_si128()); }
static inline Vec128 zext8to16_hi(Vec128r x)			{ return _mm_unpackhi_epi8(x, _mm_setzero_si128()); }

static inline Vec128 zext16to32_lo(Vec128r x)			{ return _mm_unpacklo_epi16(x, _mm_setzero_si128()); }
static inline Vec128 zext16to32_hi(Vec128r x)			{ return _mm_unpackhi_epi16(x, _mm_setzero_si128()); }

static inline Vec128 sext16to32_lo(Vec128r x)			{ return _mm_srai_epi32(_mm_unpacklo_epi16(x, x), 16); }
static inline Vec128 sext16to32_hi(Vec128r x)			{ return _mm_srai_epi32(_mm_unpackhi_epi16(x, x), 16); }

template<int x,int y,int z,int w>
static inline Vec128 shuffle32(Vec128r v)				{ return _mm_shuffle_epi32(v, _MM_SHUFFLE(w,z,y,x)); }

template<int x,int y,int z,int w>
static inline Vec128 shuffle_two32(Vec128r a, Vec128r b){ return _mm_castps_si128(_mm_shuffle_ps(_mm_castsi128_ps(a), _mm_castsi128_ps(b), _MM_SHUFFLE(w,z,y,x))); }

// some reduction steps
static inline Vec128 reduce_add_s32_2away(Vec128r x)	{ return _mm_add_epi32(x, shuffle32<2,3,0,1>(x)); }
static inline Vec128 reduce_min_u8_8away(Vec128r x)		{ return _mm_min_epu8(x,  shuffle32<2,3,0,1>(x)); }
static inline Vec128 reduce_max_u8_8away(Vec128r x)		{ return _mm_max_epu8(x,  shuffle32<2,3,0,1>(x)); }
static inline Vec128 reduce_add_s32_1away(Vec128r x)	{ return _mm_add_epi32(x, shuffle32<1,0,3,2>(x)); }
static inline Vec128 reduce_min_u8_4away(Vec128r x)		{ return _mm_min_epu8(x,  shuffle32<1,0,3,2>(x)); }
static inline Vec128 reduce_max_u8_4away(Vec128r x)		{ return _mm_max_epu8(x,  shuffle32<1,0,3,2>(x)); }
#ifdef DO_BUILD_SSE4
static inline Vec128 reduce_min_u32_2away(Vec128r x)    { return _mm_min_epu32(x, shuffle32<2,3,0,1>(x)); }
static inline Vec128 reduce_max_u32_2away(Vec128r x)    { return _mm_max_epu32(x, shuffle32<2,3,0,1>(x)); }
static inline Vec128 reduce_min_u32_1away(Vec128r x)    { return _mm_min_epu32(x, shuffle32<1,0,3,2>(x)); }
static inline Vec128 reduce_max_u32_1away(Vec128r x)    { return _mm_max_epu32(x, shuffle32<1,0,3,2>(x)); }
#endif


static inline S32 reduce_add_s32(Vec128r x)				{ return _mm_cvtsi128_si32(reduce_add_s32_1away(reduce_add_s32_2away(x))); }
static inline U32 reduce_add_u32(Vec128r x)				{ return static_cast<U32>(reduce_add_s32(x)); }

struct Vec128_S8;
struct Vec128_S16;
struct Vec128_S32;
struct Vec128_U8;
struct Vec128_U16;
struct Vec128_U32;
struct VecF32x4;

typedef const Vec128_S8  & Vec128_S8r;
typedef const Vec128_S16 & Vec128_S16r;
typedef const Vec128_S32 & Vec128_S32r;
typedef const Vec128_U8  & Vec128_U8r;
typedef const Vec128_U16 & Vec128_U16r;
typedef const Vec128_U32 & Vec128_U32r;
typedef const VecF32x4   & VecF32x4r;

struct Vec128_S8
{
	Vec128 v;

	Vec128_S8() {}
	explicit Vec128_S8(Vec128 x) : v(x) {}
	explicit Vec128_S8(S8 s) : v(_mm_set1_epi8(s)) {}
	Vec128_S8(S8 s0, S8 s1, S8 s2, S8 s3, S8 s4, S8 s5, S8 s6, S8 s7, S8 s8, S8 s9, S8 s10, S8 s11, S8 s12, S8 s13, S8 s14, S8 s15)
		: v(_mm_setr_epi8(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15)) {}

	operator __m128i() const						{ return v; }

	// initialization and conversions
	static Vec128_S8 zero()							{ return Vec128_S8(_mm_setzero_si128()); }
	static Vec128_S8 loadu(const void *ptr)			{ return Vec128_S8(load128u(ptr)); }
	static Vec128_S8 loadu_lo64(const void *ptr)	{ return Vec128_S8(load64u(ptr)); }
	static Vec128_S8 loadu_lo32(const void *ptr)	{ return Vec128_S8(load32u(ptr)); }

	void storea(void *ptr) const					{ store128a(ptr, v); }
	void storeu(void *ptr) const					{ store128u(ptr, v); }
	void storeu_lo64(void *ptr) const				{ store64u(ptr, v); }
	void storeu_lo32(void *ptr) const				{ store32u(ptr, v); }

	// reinterpret cast
	Vec128_S16 s16() const;
	Vec128_S32 s32() const;
	Vec128_U8  u8() const;
	Vec128_U16 u16() const;
	Vec128_U32 u32() const;

	// conversion
	Vec128_S16 to_s16_lo() const;
	Vec128_S32 to_s32_lo() const;

#ifdef DO_BUILD_SSE4
	template<int index> int extract() const			{ return _mm_extract_epi8(v, index); }
	template<int index> void insert(S8 x)			{ v = _mm_insert_epi8(v, x, index); }
#endif

	// unary operations
#ifdef DO_BUILD_SSSE3
	Vec128_S8 abs() const							{ return Vec128_S8(_mm_abs_epi8(v)); }
#endif
	int movemask() const							{ return _mm_movemask_epi8(v); }

	// binary operations
	Vec128_S8 add(Vec128_S8r b) const				{ return Vec128_S8(_mm_add_epi8(v, b)); }
	Vec128_S8 add_sat(Vec128_S8r b) const			{ return Vec128_S8(_mm_adds_epi8(v, b)); }
	Vec128_S8 cmp_eq(Vec128_S8r b) const			{ return Vec128_S8(_mm_cmpeq_epi8(v, b)); }
	Vec128_S8 cmp_gt(Vec128_S8r b) const			{ return Vec128_S8(_mm_cmpgt_epi8(v, b)); }
#ifdef DO_BUILD_SSE4
#ifdef max
#undef max
#endif
#ifdef min
#undef min
#endif
	Vec128_S8 max(Vec128_S8r b) const				{ return Vec128_S8(_mm_max_epi8(v, b)); }
	Vec128_S8 min(Vec128_S8r b) const				{ return Vec128_S8(_mm_min_epi8(v, b)); }
#endif
#ifdef DO_BUILD_SSSE3
	Vec128_S8 sign(Vec128_S8r b) const				{ return Vec128_S8(_mm_sign_epi8(v, b)); }
#endif
	Vec128_S8 sub(Vec128_S8r b) const				{ return Vec128_S8(_mm_sub_epi8(v, b)); }
	Vec128_S8 sub_sat(Vec128_S8r b) const			{ return Vec128_S8(_mm_subs_epi8(v, b)); }
	Vec128_S8 unpack_hi(Vec128_S8r b) const			{ return Vec128_S8(_mm_unpackhi_epi8(v, b)); }
	Vec128_S8 unpack_lo(Vec128_S8r b) const			{ return Vec128_S8(_mm_unpacklo_epi8(v, b)); }

	Vec128_S8 andnot(Vec128r b) const				{ return Vec128_S8(_mm_andnot_si128(b, v)); } // a & ~b (mm_andnot is ~a & b)

	// ternary opeations
#ifdef DO_BUILD_SSE4
	Vec128_S8 blend(Vec128_S8r b, Vec128r mask) const
	{
		return Vec128_S8(_mm_blendv_epi8(v, b, mask));
	}
#endif

	// shuffle
#ifdef DO_BUILD_SSE4
	Vec128_S8 shuf(Vec128r m) const					{ return Vec128_S8(_mm_shuffle_epi8(v, m)); } 
#endif

	// binary assignment operators
	Vec128_S8 &operator +=(Vec128_S8r b)			{ v = add(b); return *this; }
	Vec128_S8 &operator -=(Vec128_S8r b)			{ v = sub(b); return *this; }

	Vec128_S8 &operator &=(Vec128r b)				{ v = _mm_and_si128(v, b); return *this; }
	Vec128_S8 &operator |=(Vec128r b)				{ v = _mm_or_si128(v, b);  return *this; }
	Vec128_S8 &operator ^=(Vec128r b)				{ v = _mm_xor_si128(v, b); return *this; }
};

struct Vec128_S16
{
	Vec128 v;

	Vec128_S16() {}
	explicit Vec128_S16(Vec128 x) : v(x) {}
	explicit Vec128_S16(S16 x) : v(_mm_set1_epi16(x)) {}
	Vec128_S16(S16 s0, S16 s1, S16 s2, S16 s3, S16 s4, S16 s5, S16 s6, S16 s7)
		: v(_mm_setr_epi16(s0, s1, s2, s3, s4, s5, s6, s7)) {}

	operator __m128i() const						{ return v; }

	// initialization and conversions
	static Vec128_S16 zero()						{ return Vec128_S16(_mm_setzero_si128()); }
	static Vec128_S16 loada(const void *ptr)		{ return Vec128_S16(load128a(ptr)); }
	static Vec128_S16 loadu(const void *ptr)		{ return Vec128_S16(load128u(ptr)); }
	static Vec128_S16 loadu_lo32(const void *ptr)	{ return Vec128_S16(load32u(ptr)); }
	static Vec128_S16 loadu_lo64(const void *ptr)	{ return Vec128_S16(load64u(ptr)); }
	static Vec128_S16 loadu_lo64_dup(const void *ptr)	{ return loadu_lo64(ptr).dup_lo(); }

	void storea(void *ptr) const					{ store128a(ptr, v); }
	void storeu(void *ptr) const					{ store128u(ptr, v); }
	void storeu_lo32(void *ptr) const				{ store32u(ptr, v); }
	void storeu_lo64(void *ptr) const				{ store64u(ptr, v); }

	static Vec128_S16 repeat2(S16 s0, S16 s1)		{ return Vec128_S16(s0,s1, s0,s1, s0,s1, s0,s1); }
	static Vec128_S16 repeat4(S16 s0, S16 s1, S16 s2, S16 s3)
	{
		return Vec128_S16(s0, s1, s2, s3, s0, s1, s2, s3);
	}

	static Vec128_S16 splat2(S16 s0, S16 s1, S16 s2, S16 s3)
	{
		return Vec128_S16(s0,s0, s1,s1, s2,s2, s3,s3);
	}

	// Duplicate the low half of register to the high half
	Vec128_S16 dup_lo() const						{ return Vec128_S16(shuffle32<0,1,0,1>(v)); }
	// Duplicate the high half of register to the low half
	Vec128_S16 dup_hi() const						{ return Vec128_S16(shuffle32<2,3,2,3>(v)); }

#ifdef DO_BUILD_SSE4
	Vec128_S32 to_sint32() const;
#endif
	Vec128_U8 to_uint8_sat() const;
	Vec128_U8 to_uint8_sat(Vec128_S16r b) const;

	// reinterpret cast
	Vec128_S8  s8() const;
	Vec128_S32 s32() const;
	Vec128_U8  u8() const;
	Vec128_U16 u16() const;
	Vec128_U32 u32() const;

	template<int index> int extract() const 		{ return _mm_extract_epi16(v, index); }
	template<int index>	Vec128_S16 insert(int x)	{ return Vec128_S16(_mm_insert_epi16(v, x, index)); }

	// unary operations
#ifdef DO_BUILD_SSSE3
	Vec128_S16 abs() const							{ return Vec128_S16(_mm_abs_epi16(v)); }
#endif

	// binary operations
	Vec128_S16 add(Vec128_S16r b) const				{ return Vec128_S16(_mm_add_epi16(v, b)); }
	Vec128_S16 add_sat(Vec128_S16r b) const			{ return Vec128_S16(_mm_adds_epi16(v, b)); }
	Vec128_S16 cmp_eq(Vec128_S16r b) const			{ return Vec128_S16(_mm_cmpeq_epi16(v, b)); }
	Vec128_S16 cmp_gt(Vec128_S16r b) const			{ return Vec128_S16(_mm_cmpgt_epi16(v, b)); }
	Vec128_S16 max(Vec128_S16r b) const				{ return Vec128_S16(_mm_max_epi16(v, b)); }
	Vec128_S16 min(Vec128_S16r b) const				{ return Vec128_S16(_mm_min_epi16(v, b)); }
	Vec128_S16 mulhi(Vec128_S16r b) const			{ return Vec128_S16(_mm_mulhi_epi16(v, b)); }
#ifdef DO_BUILD_SSSE3
	Vec128_S16 mulhrs(Vec128_S16r b) const 			{ return Vec128_S16(_mm_mulhrs_epi16(v, b)); }
#endif
	Vec128_S16 mullo(Vec128_S16r b) const 			{ return Vec128_S16(_mm_mullo_epi16(v, b)); }
#ifdef DO_BUILD_SSSE3
	Vec128_S16 sign(Vec128_S16r b) const			{ return Vec128_S16(_mm_sign_epi16(v, b)); }
#endif
	Vec128_S16 sub(Vec128_S16r b) const				{ return Vec128_S16(_mm_sub_epi16(v, b)); }
	Vec128_S16 sub_sat(Vec128_S16r b) const			{ return Vec128_S16(_mm_subs_epi16(v, b)); }
	Vec128_S16 unpack_lo(Vec128_S16r b) const		{ return Vec128_S16(_mm_unpacklo_epi16(v, b)); }
	Vec128_S16 unpack_hi(Vec128_S16r b) const		{ return Vec128_S16(_mm_unpackhi_epi16(v, b)); }

	Vec128_S16 andnot(Vec128r b) const				{ return Vec128_S16(_mm_andnot_si128(b, v)); } // a & ~b (mm_andnot is ~a & b)

	template<int count>	Vec128_S16 shl() const		{ return Vec128_S16(_mm_slli_epi16(v, count)); }
	template<int count>	Vec128_S16 sra() const		{ return Vec128_S16(_mm_srai_epi16(v, count)); }
	template<int count>	Vec128_S16 srl() const		{ return Vec128_S16(_mm_srli_epi16(v, count)); }

#ifdef DO_BUILD_SSE4
	template<int mask>
	Vec128_S16 blend(Vec128_S16r b) const			{ return Vec128_S16(_mm_blend_epi16(v, b, mask)); }
#endif

	Vec128_S32 madd(Vec128_S16r b) const;

	// shuffle
#ifdef DO_BUILD_SSE4
	Vec128_S16 shuf(Vec128r m) const					{ return Vec128_S16(_mm_shuffle_epi8(v, m)); } 
#endif

	// binary assignment operators
	Vec128_S16 &operator +=(Vec128_S16r b)			{ v = add(b); return *this; }
	Vec128_S16 &operator -=(Vec128_S16r b)			{ v = sub(b); return *this; }
	Vec128_S16 &operator *=(Vec128_S16r b)			{ v = mullo(b); return *this; }

	Vec128_S16 &operator &=(Vec128r b)				{ v = _mm_and_si128(v, b); return *this; }
	Vec128_S16 &operator |=(Vec128r b)				{ v = _mm_or_si128(v, b); return *this; }
	Vec128_S16 &operator ^=(Vec128r b)				{ v = _mm_xor_si128(v, b); return *this; }
};

struct Vec128_S32
{
	Vec128 v;

	Vec128_S32() {}
	explicit Vec128_S32(Vec128 x) : v(x) {}
	explicit Vec128_S32(S32 x) : v(_mm_set1_epi32(x)) {}
	Vec128_S32(S32 s0, S32 s1, S32 s2, S32 s3)
		: v(_mm_setr_epi32(s0, s1, s2, s3)) {}

	operator __m128i() const						{ return v; }

	// initialization and conversions
	static Vec128_S32 zero()						{ return Vec128_S32(_mm_setzero_si128()); }
	static Vec128_S32 loada(const void *ptr)		{ return Vec128_S32(load128a(ptr)); }
	static Vec128_S32 loadu(const void *ptr)		{ return Vec128_S32(load128u(ptr)); }
	static Vec128_S32 loadu_lo32(const void *ptr)	{ return Vec128_S32(load32u(ptr)); }

	void storea(void *ptr) const					{ store128a(ptr, v); }
	void storeu(void *ptr) const					{ store128u(ptr, v); }
	void storeu_lo32(void *ptr) const				{ store32u(ptr, v); }

#ifdef DO_BUILD_SSE4
	template<int index> S32 extract() const			{ return index ? _mm_extract_epi32(v, index) : _mm_cvtsi128_si32(v); }
	template<int index> void insert(S32 x)			{ v = _mm_insert_epi32(v, x, index); }
#endif
	template<int index> Vec128_S32 dup() const		{ return Vec128_S32(shuffle32<index,index,index,index>(v)); }

	VecF32x4   to_f32() const;
	Vec128_S16 to_s16_sat(Vec128_S32r b) const;

	// cast
	Vec128_S8  s8() const;
	Vec128_S16 s16() const;
	Vec128_U8  u8() const;
	Vec128_U16 u16() const;
	Vec128_U32 u32() const;
	VecF32x4   f32() const;

	// unary operations
	int movemask() const 							{ return _mm_movemask_ps(_mm_castsi128_ps(v)); }

	// *this must be a truth value (i.e. -1 or 0)
#ifdef DO_BUILD_SSE4
	bool all() const								{ return _mm_testc_si128(v, _mm_set1_epi32(-1)); }
	bool any() const								{ return !_mm_testz_si128(v, v); }
#else
	bool all() const								{ return movemask() == 15; }
	bool any() const								{ return movemask() != 0; }
#endif

#ifdef DO_BUILD_SSSE3
	Vec128_S32 abs() const							{ return Vec128_S32(_mm_abs_epi32(v)); }
#endif

	// binary operations
	Vec128_S32 add(Vec128_S32r b) const				{ return Vec128_S32(_mm_add_epi32(v, b)); }
	Vec128_S32 cmp_eq(Vec128_S32r b) const			{ return Vec128_S32(_mm_cmpeq_epi32(v, b)); }
	Vec128_S32 cmp_gt(Vec128_S32r b) const			{ return Vec128_S32(_mm_cmpgt_epi32(v, b)); }
#ifdef DO_BUILD_SSE4
	Vec128_S32 max(Vec128_S32r b) const				{ return Vec128_S32(_mm_max_epi32(v, b)); }
	Vec128_S32 min(Vec128_S32r b) const				{ return Vec128_S32(_mm_min_epi32(v, b)); }
	Vec128_S32 mullo(Vec128_S32r b) const 			{ return Vec128_S32(_mm_mullo_epi32(v, b)); }
#endif
#ifdef DO_BUILD_SSSE3
	Vec128_S32 sign(Vec128_S32r b) const			{ return Vec128_S32(_mm_sign_epi32(v, b)); }
#endif
	Vec128_S32 sub(Vec128_S32r b) const				{ return Vec128_S32(_mm_sub_epi32(v, b)); }
	Vec128_S32 unpack_hi(Vec128_S32r b) const		{ return Vec128_S32(_mm_unpackhi_epi32(v, b)); }
	Vec128_S32 unpack_lo(Vec128_S32r b) const		{ return Vec128_S32(_mm_unpacklo_epi32(v, b)); }
	Vec128_S32 unpack_hi64(Vec128_S32r b) const		{ return Vec128_S32(_mm_unpackhi_epi64(v, b)); }
	Vec128_S32 unpack_lo64(Vec128_S32r b) const		{ return Vec128_S32(_mm_unpacklo_epi64(v, b)); }

	Vec128_S32 andnot(Vec128r b)					{ return Vec128_S32(_mm_andnot_si128(b, v)); } // a & ~b (mm_andnot is ~a & b)

#ifdef DO_BUILD_SSE4
	// compares as 2 64-bit lanes
	Vec128_S32 cmp_eq64(Vec128_S32r b) const		{ return Vec128_S32(_mm_cmpeq_epi64(v, b)); }
#endif

	template<int count>	Vec128_S32 shl() const		{ return Vec128_S32(_mm_slli_epi32(v, count)); }
	template<int count> Vec128_S32 sra() const		{ return Vec128_S32(_mm_srai_epi32(v, count)); }
	template<int count>	Vec128_S32 srl() const		{ return Vec128_S32(_mm_srli_epi32(v, count)); }

#ifdef DO_BUILD_SSE4
	template<int mask>
	Vec128_S32 blend(Vec128_S32r b) const
	{
		return Vec128_S32(_mm_castps_si128(_mm_blend_ps(_mm_castsi128_ps(v), _mm_castsi128_ps(b), mask)));
	}

	// cond.select(a, b) is the equivalent of cond ? a : b
	// cond (*this) must be a truth value (i.e. -1 or 0)
	Vec128_S32 select(Vec128_S32r a, Vec128_S32r b) const
	{
		return Vec128_S32(_mm_blendv_epi8(b, a, v));
	}
#endif

	// binary assignment operators
	Vec128_S32 &operator +=(Vec128_S32r b)			{ v = add(b); return *this; }
	Vec128_S32 &operator -=(Vec128_S32r b)			{ v = sub(b); return *this; }
#ifdef DO_BUILD_SSE4
	Vec128_S32 &operator *=(Vec128_S32r b)			{ v = mullo(b); return *this; }
#endif

	Vec128_S32 &operator &=(Vec128r b)				{ v = _mm_and_si128(v, b); return *this; }
	Vec128_S32 &operator |=(Vec128r b)				{ v = _mm_or_si128(v, b);  return *this; }
	Vec128_S32 &operator ^=(Vec128r b	)			{ v = _mm_xor_si128(v, b); return *this; }

	// shuffles
	template<int x, int y, int z, int w>
	Vec128_S32 shuf() const							{ return Vec128_S32(shuffle32<x,y,z,w>(v)); }
	Vec128_S32 xyxy() const							{ return shuf<0,1,0,1>(); }
	Vec128_S32 zwzw() const							{ return shuf<2,3,2,3>(); }

	// blockwise swaps
	Vec128_S32 yxwz() const							{ return shuf<1,0,3,2>(); }
	Vec128_S32 zwxy() const							{ return shuf<2,3,0,1>(); }

	// broadcasts
	Vec128_S32 xxxx() const							{ return shuf<0,0,0,0>(); }

	// some reductions
#ifdef DO_BUILD_SSE4
	Vec128_U8 gather_xyzw_lo8()	const;
#endif
	Vec128_S32 sum_across() const					{ return Vec128_S32(reduce_add_s32_1away(reduce_add_s32_2away(v))); }
};

struct Vec128_U8
{
	Vec128 v;

	Vec128_U8() = default;
	explicit Vec128_U8(Vec128 x) : v(x) {}
	explicit Vec128_U8(U8 x) : v(_mm_set1_epi8(x)) {}
	Vec128_U8(U8 e0, U8 e1, U8 e2, U8 e3, U8 e4, U8 e5, U8 e6, U8 e7, U8 e8, U8 e9, U8 e10, U8 e11, U8 e12, U8 e13, U8 e14, U8 e15)
		: v(_mm_setr_epi8(e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15)) {}

	operator __m128i() const						{ return v; }

	// initialization and conversions
	static Vec128_U8 zero()							{ return Vec128_U8(_mm_setzero_si128()); }
	static Vec128_U8 loada(const void *ptr)			{ return Vec128_U8(load128a(ptr)); }
	static Vec128_U8 loadu(const void *ptr)			{ return Vec128_U8(load128u(ptr)); }
	static Vec128_U8 loadu_lo32(const void *ptr)	{ return Vec128_U8(load32u(ptr)); }
	static Vec128_U8 loadu_lo64(const void *ptr)	{ return Vec128_U8(load64u(ptr)); }

	void storea(void *ptr) const					{ store128a(ptr, v); }
	void storeu(void *ptr) const					{ store128u(ptr, v); }
	void storeu_lo32(void *ptr) const				{ store32u(ptr, v); }
	void storeu_lo64(void *ptr) const				{ store64u(ptr, v); }

	static Vec128_U8 repeat2(U8 u0, U8 u1)
	{
		return Vec128_U8(_mm_set1_epi16(((int) u1 << 8) | ((int) u0)));
	}
	static Vec128_U8 repeat4(U8 u0, U8 u1, U8 u2, U8 u3)
	{
		return Vec128_U8(_mm_set1_epi32(((int) u3 << 24) | ((int) u2 << 16) | ((int) u1 << 8) | ((int) u0)));
	}

	Vec128_S16 to_sint16_lo() const;
	Vec128_S16 to_sint16_hi() const;
	Vec128_S32 to_sint32_lo() const;
	Vec128_U16 to_uint16_lo() const;
	Vec128_U16 to_uint16_hi() const;

	// cast
	Vec128_S8  s8() const;
	Vec128_S16 s16() const;
	Vec128_S32 s32() const;
	Vec128_U16 u16() const;
	Vec128_U32 u32() const;
	VecF32x4 f32() const;

#ifdef DO_BUILD_SSE4
	template<int index> int extract() const			{ return _mm_extract_epi8(v, index); }
	template<int index> void insert(U8 x) 			{ v = _mm_insert_epi8(v, x, index); }
#endif

	// unary operations
	int movemask() const 							{ return _mm_movemask_epi8(v); }

	// binary opetations
	Vec128_U8 add(Vec128_U8r b) const				{ return Vec128_U8(_mm_add_epi8(v, b)); }
	Vec128_U8 add_sat(Vec128_U8r b) const			{ return Vec128_U8(_mm_adds_epu8(v, b)); }
	Vec128_U8 cmp_eq(Vec128_U8r b) const			{ return Vec128_U8(_mm_cmpeq_epi8(v, b)); }
	Vec128_U8 cmp_ge(Vec128_U8r b) const			{ return Vec128_U8(_mm_cmpeq_epi8(_mm_max_epu8(b, v), v)); }
	Vec128_U8 max(Vec128_U8r b) const				{ return Vec128_U8(_mm_max_epu8(v, b)); }
	Vec128_U8 min(Vec128_U8r b) const				{ return Vec128_U8(_mm_min_epu8(v, b)); }
#ifdef DO_BUILD_SSSE3
	Vec128_U8 sign(Vec128_U8r b) const				{ return Vec128_U8(_mm_sign_epi8(v, b)); }
#endif
	Vec128_U8 sub(Vec128_U8r b) const				{ return Vec128_U8(_mm_sub_epi8(v, b)); }
	Vec128_U8 sub_sat(Vec128_U8r b) const			{ return Vec128_U8(_mm_subs_epu8(v, b)); }
	Vec128_U8 unpack_hi(Vec128_U8r b) const			{ return Vec128_U8(_mm_unpackhi_epi8(v, b)); }
	Vec128_U8 unpack_lo(Vec128_U8r b) const			{ return Vec128_U8(_mm_unpacklo_epi8(v, b)); }
	Vec128_U8 unpack_lo64(Vec128_U8r b) const		{ return Vec128_U8(_mm_unpacklo_epi64(v, b)); }
	Vec128_U8 unpack_hi64(Vec128_U8r b) const		{ return Vec128_U8(_mm_unpackhi_epi64(v, b)); }

	Vec128_U8 andnot(Vec128r b) const				{ return Vec128_U8(_mm_andnot_si128(b, v)); } // a & ~b (mm_andnot is ~a & b)

#ifdef DO_BUILD_SSE4
	Vec128_U8 blend(Vec128_U8r b, Vec128r mask) const
	{
		return Vec128_U8(_mm_blendv_epi8(v, b, mask));
	}

	// cond.select(a, b) is the equivalent of cond ? a : b
	// cond (*this) must be a truth value (i.e. -1 or 0)
	Vec128_U8 select(Vec128_U8r a, Vec128_U8r b) const
	{
		return Vec128_U8(_mm_blendv_epi8(b, a, v));
	}
#endif

	// binary assignment operators
	Vec128_U8 &operator +=(Vec128_U8r b)			{ v = add(b); return *this; }
	Vec128_U8 &operator -=(Vec128_U8r b)			{ v = sub(b); return *this; }

	Vec128_U8 &operator &=(Vec128r b)				{ v = _mm_and_si128(v, b); return *this; }
	Vec128_U8 &operator |=(Vec128r b)				{ v = _mm_or_si128(v, b); return *this; }
	Vec128_U8 &operator ^=(Vec128r b)				{ v = _mm_xor_si128(v, b); return *this; }

#ifdef DO_BUILD_SSSE3
	Vec128_S16 madd_sat(Vec128_S8r b) const;
	#endif
	Vec128_S32 sad(Vec128_U8r b) const;

	// shuffle
#ifdef DO_BUILD_SSSE3
	Vec128_U8 shuf(Vec128r m) const					{ return Vec128_U8(_mm_shuffle_epi8(v, m)); } 
#endif

	// some reductions
	void reduce_max_8away()							{ v = reduce_max_u8_8away(v); }
	void reduce_max_4away()							{ v = reduce_max_u8_4away(v); }
};

struct Vec128_U16
{
	Vec128 v;

	Vec128_U16() {}
	explicit Vec128_U16(Vec128 x) : v(x) {}
	explicit Vec128_U16(U16 x) : v(_mm_set1_epi16(x)) {}
	Vec128_U16(U16 e0, U16 e1, U16 e2, U16 e3, U16 e4, U16 e5, U16 e6, U16 e7)
		: v(_mm_setr_epi16(e0, e1, e2, e3, e4, e5, e6, e7)) {}

	operator __m128i() const						{ return v; }

	// initialization and conversions
	static Vec128_U16 zero()						{ return Vec128_U16(_mm_setzero_si128()); }
	static Vec128_U16 loada(const void *ptr)		{ return Vec128_U16(load128a(ptr)); }
	static Vec128_U16 loadu(const void *ptr)		{ return Vec128_U16(load128u(ptr)); }
	static Vec128_U16 loadu_lo64(const void *ptr)	{ return Vec128_U16(load64u(ptr)); }
	static Vec128_U16 loadu_lo32(const void *ptr)	{ return Vec128_U16(load32u(ptr)); }

	void storea(void *ptr) const					{ store128a(ptr, v); }
	void storeu(void *ptr) const					{ store128u(ptr, v); }
	void storeu_lo64(void *ptr) const				{ store64u(ptr, v); }
	void storeu_lo32(void *ptr) const				{ store32u(ptr, v); }

	static Vec128_U16 repeat2(U16 e0, U16 e1)		{ return Vec128_U16(e0,e1, e0,e1, e0,e1, e0,e1); }
	static Vec128_U16 repeat4(U16 e0, U16 e1, U16 e2, U16 e3)
	{
		return Vec128_U16(e0, e1, e2, e3, e0, e1, e2, e3);
	}

	static Vec128_U16 splat2(U16 e0, U16 e1, U16 e2, U16 e3)
	{
		return Vec128_U16(e0,e0, e1,e1, e2,e2, e3,e3);
	}

	// Duplicate the low half of register to the high half
	Vec128_U16 dup_lo() const						{ return Vec128_U16(shuffle32<0,1,0,1>(v)); }
	// Duplicate the high half of register to the low half
	Vec128_U16 dup_hi() const						{ return Vec128_U16(shuffle32<2,3,2,3>(v)); }

#ifdef DO_BUILD_SSE4
	Vec128_S32 to_sint32() const;
#endif
	Vec128_U8 to_uint8_sat() const;
	Vec128_U8 to_uint8_sat(Vec128_U16r b) const;
	Vec128_U32 to_uint32_lo() const;

	// reinterpret cast
	Vec128_S8  s8() const;
	Vec128_S16 s16() const;
	Vec128_S32 s32() const;
	Vec128_U8  u8() const;
	Vec128_U32 u32() const;

	template<int index> int extract() const 		{ return _mm_extract_epi16(v, index); }
	template<int index>	Vec128_U16 insert(U16 x)	{ return Vec128_U16(_mm_insert_epi16(v, x, index)); }

	// binary operations
	Vec128_U16 add(Vec128_U16r b) const				{ return Vec128_U16(_mm_add_epi16(v, b)); }
	Vec128_U16 add_sat(Vec128_U16r b) const			{ return Vec128_U16(_mm_adds_epu16(v, b)); }
	Vec128_U16 cmp_eq(Vec128_U16r b) const			{ return Vec128_U16(_mm_cmpeq_epi16(v, b)); }
#ifdef DO_BUILD_SSE4
	Vec128_U16 max(Vec128_U16r b) const				{ return Vec128_U16(_mm_max_epu16(v, b)); }
	Vec128_U16 min(Vec128_U16r b) const				{ return Vec128_U16(_mm_min_epu16(v, b)); }
#endif
	Vec128_U16 mulhi(Vec128_U16r b) const			{ return Vec128_U16(_mm_mulhi_epu16(v, b)); }
	Vec128_U16 mullo(Vec128_U16r b) const 			{ return Vec128_U16(_mm_mullo_epi16(v, b)); }
	Vec128_U16 sub(Vec128_U16r b) const				{ return Vec128_U16(_mm_sub_epi16(v, b)); }
	Vec128_U16 sub_sat(Vec128_U16r b) const			{ return Vec128_U16(_mm_subs_epu16(v, b)); }
	Vec128_U16 unpack_lo(Vec128_U16r b) const		{ return Vec128_U16(_mm_unpacklo_epi16(v, b)); }
	Vec128_U16 unpack_hi(Vec128_U16r b) const		{ return Vec128_U16(_mm_unpackhi_epi16(v, b)); }

	Vec128_U16 andnot(Vec128r b) const				{ return Vec128_U16(_mm_andnot_si128(b, v)); } // a & ~b (mm_andnot is ~a & b)

	template<int count>	Vec128_U16 shl() const		{ return Vec128_U16(_mm_slli_epi16(v, count)); }
	template<int count>	Vec128_U16 sra() const		{ return Vec128_U16(_mm_srai_epi16(v, count)); }
	template<int count>	Vec128_U16 srl() const		{ return Vec128_U16(_mm_srli_epi16(v, count)); }

	template<int count> Vec128_U16 sli(Vec128_U16r b) const	{ return shl<count>() + b; }

#ifdef DO_BUILD_SSE4
	template<int mask>
	Vec128_U16 blend(Vec128_U16r b) const			{ return Vec128_U16(_mm_blend_epi16(v, b, mask)); }

	// cond.select(a, b) is the equivalent of cond ? a : b
	// cond (*this) must be a truth value (i.e. -1 or 0)
	Vec128_U16 select(Vec128_U16r a, Vec128_U16r b) const
	{
		return Vec128_U16(_mm_blendv_epi8(b, a, v));
	}
#endif

	// shuffle
#ifdef DO_BUILD_SSE4
	Vec128_U16 shuf(Vec128r m) const					{ return Vec128_U16(_mm_shuffle_epi8(v, m)); } 
#endif

	// binary assignment operators
	Vec128_U16 &operator +=(Vec128_U16r b)			{ v = add(b); return *this; }
	Vec128_U16 &operator -=(Vec128_U16r b)			{ v = sub(b); return *this; }
	Vec128_U16 &operator *=(Vec128_U16r b)			{ v = mullo(b); return *this; }

	Vec128_U16 &operator &=(Vec128r b)				{ v = _mm_and_si128(v, b); return *this; }
	Vec128_U16 &operator |=(Vec128r b)				{ v = _mm_or_si128(v, b); return *this; }
	Vec128_U16 &operator ^=(Vec128r b)				{ v = _mm_xor_si128(v, b); return *this; }
};

struct Vec128_U32
{
	Vec128 v;

	Vec128_U32() {}
	explicit Vec128_U32(Vec128 x) : v(x) {}
	explicit Vec128_U32(U32 x) : v(_mm_set1_epi32((S32)x)) {}
	Vec128_U32(U32 s0, U32 s1, U32 s2, U32 s3)
		: v(_mm_setr_epi32((S32)s0, (S32)s1, (S32)s2, (S32)s3)) {}

	operator __m128i() const						{ return v; }

	// initialization and conversions
	static Vec128_U32 zero()						{ return Vec128_U32(_mm_setzero_si128()); }
	static Vec128_U32 loada(const void *ptr)		{ return Vec128_U32(load128a(ptr)); }
	static Vec128_U32 loadu(const void *ptr)		{ return Vec128_U32(load128u(ptr)); }
	static Vec128_U32 loadu_lo32(const void *ptr)	{ return Vec128_U32(load32u(ptr)); }
	static Vec128_U32 loada_dup(const U32 *ptr)		{ return Vec128_U32(_mm_set1_epi32(*ptr)); }

	void storea(void *ptr) const 					{ store128a(ptr, v); }
	void storeu(void *ptr) const 					{ store128u(ptr, v); }
	void storeu_lo32(void *ptr) const 				{ store32u(ptr, v); }

#ifdef DO_BUILD_SSE4
	template<int index> U32 extract() const			{ return _mm_extract_epi8(v, index); }
	template<int index> void insert(U32 x)			{ v = _mm_insert_epi8(v, x, index); }

	template <int lane>
	void load_lane(const U32 *ptr)					{ v = _mm_insert_epi32(v, *ptr, lane); }
#endif

	// cast
	Vec128_S8  s8() const;
	Vec128_S16 s16() const;
	Vec128_S32 s32() const;
	Vec128_U8  u8() const;
	Vec128_U16 u16() const;
	VecF32x4   f32() const;

	// binary operations
	Vec128_U32 add(Vec128_U32r b) const				{ return Vec128_U32(_mm_add_epi32(v, b)); }
	Vec128_U32 cmp_eq(Vec128_U32r b) const			{ return Vec128_U32(_mm_cmpeq_epi32(v, b)); }
#ifdef DO_BUILD_SSE4
	Vec128_U32 max(Vec128_U32r b) const				{ return Vec128_U32(_mm_max_epu32(v, b)); }
	Vec128_U32 min(Vec128_U32r b) const				{ return Vec128_U32(_mm_min_epu32(v, b)); }
	Vec128_U32 mullo(Vec128_U32r b) const 			{ return Vec128_U32(_mm_mullo_epi32(v, b)); }
#endif
	Vec128_U32 sub(Vec128_U32r b) const				{ return Vec128_U32(_mm_sub_epi32(v, b)); }
	Vec128_U32 unpack_hi(Vec128_U32r b) const		{ return Vec128_U32(_mm_unpackhi_epi32(v, b)); }
	Vec128_U32 unpack_lo(Vec128_U32r b) const		{ return Vec128_U32(_mm_unpacklo_epi32(v, b)); }

	Vec128_U32 andnot(Vec128r b)					{ return Vec128_U32(_mm_andnot_si128(b, v)); } // a & ~b (mm_andnot is ~a & b)

	template<int count>	Vec128_U32 shl() const		{ return Vec128_U32(_mm_slli_epi32(v, count)); }
	template<int count> Vec128_U32 sra() const		{ return Vec128_U32(_mm_srai_epi32(v, count)); }
	template<int count>	Vec128_U32 srl() const		{ return Vec128_U32(_mm_srli_epi32(v, count)); }

#ifdef DO_BUILD_SSE4
	// shifts 128-bit value right by N bytes
	template <int N>
	Vec128_U32 srl_byte() const						{ return Vec128_U32(_mm_srli_si128(v, N)); }

	// Truncate 32-bit values to 8-bits, mod 256
	Vec128_U8 narrow32to8_mod() const
	{
		return Vec128_U8(_mm_shuffle_epi8(v, _mm_setr_epi8(0,4,8,12, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1)));
	}

	template<int mask>
	Vec128_U32 blend(Vec128_U32r b) const
	{
		return Vec128_U32(_mm_castps_si128(_mm_blend_ps(_mm_castsi128_ps(v), _mm_castsi128_ps(b), mask)));
	}

	// cond.select(a, b) is the equivalent of cond ? a : b
	// cond (*this) must be a truth value (i.e. -1 or 0)
	Vec128_U32 select(Vec128_U32r a, Vec128_U32r b) const
	{
		return Vec128_U32(_mm_blendv_epi8(b, a, v));
	}

	template <int lane>
	Vec128_U32 copy_lane(Vec128_U32r from) const	{ return Vec128_U32(_mm_blend_epi16(v, from, 3 << (lane * 2))); }

	// adds (per lane) 32-bit sum of dot products in each group of 4 bytes
	// "b" operand promises it's U7 (meaning MSB is never set)
	// [ a0,a1,a2,a3, ... ] * [ b0,b1,b2,b3, ...] => [ a0*b0 + a1*b1 + a2*b2 + a3*b3, ...]
	void add_dot_u8u7(Vec128_U8r a, Vec128_U8r b)
	{
		// Initial dot product. This is unsigned*signed which is why b operands needs to use 7 bits only.
		__m128i initial_dot = _mm_maddubs_epi16(a.v, b.v);
		// Sum pairs
		__m128i summed = _mm_madd_epi16(initial_dot, _mm_set1_epi16(1));
		// Accumulate
		v = _mm_add_epi32(v, summed);
	}
#endif

	// binary assignment operators
	Vec128_U32 &operator +=(Vec128_U32r b)			{ v = add(b); return *this; }
	Vec128_U32 &operator -=(Vec128_U32r b)			{ v = sub(b); return *this; }
#ifdef DO_BUILD_SSE4
	Vec128_U32 &operator *=(Vec128_U32r b)			{ v = mullo(b); return *this; }
#endif

	Vec128_U32 &operator &=(Vec128r b)				{ v = _mm_and_si128(v, b); return *this; }
	Vec128_U32 &operator |=(Vec128r b)				{ v = _mm_or_si128(v, b);  return *this; }
	Vec128_U32 &operator ^=(Vec128r b	)			{ v = _mm_xor_si128(v, b); return *this; }

	// shuffles
	template<int x, int y, int z, int w>
	Vec128_U32 shuf() const							{ return Vec128_U32(shuffle32<x,y,z,w>(v)); }
	Vec128_U32 xxxw() const							{ return shuf<0,0,0,3>(); }
};

// Float arithmetic is way less likely to be hopping between types all the time,
// so it's convenient to have a "batteries included" class with operator overloads
//
// The intent is just to simplify notation, it's written so it's trivial to come in
// and out of native ops so if what you want isn't in here and is contextual, just
// go for it.


//operator __m128(VecF32x4r a) { return a.v; }

struct VecF32x4
{
	__m128 v;

	VecF32x4() {}
	VecF32x4(__m128 x) : v(x) {}
	explicit VecF32x4(F32 x) : v(_mm_set1_ps(x)) {}
	VecF32x4(F32 a, F32 b, F32 c, F32 d) : v(_mm_setr_ps(a, b, c, d)) {}

	// initialization and conversions
	static VecF32x4 zero()							{ return _mm_setzero_ps(); }
	static VecF32x4 from_int32(Vec128 x)			{ return _mm_cvtepi32_ps(x); }
	//static VecF32x4 from_int32(VecF32x4r x)			{ return _mm_cvtepi32_ps(x); }
	static VecF32x4 loadu(const float *ptr)			{ return _mm_loadu_ps(ptr); } // unaligned
	static VecF32x4 loada(const float *ptr)			{ return _mm_load_ps(ptr); } // 16-byte aligned
	static VecF32x4 load_scalar(const float *ptr)	{ return _mm_load_ss(ptr); }
	static VecF32x4 load_pair(const float *ptr)		{ return _mm_castpd_ps(_mm_load_sd((const double *)ptr)); }
	static VecF32x4 bitmask(U32 a)					{ return _mm_castsi128_ps(_mm_set1_epi32(a)); }
	static VecF32x4 bitmask(U32 a, U32 b, U32 c, U32 d) { return _mm_castsi128_ps(_mm_setr_epi32((int)a, (int)b, (int)c, (int)d)); }

	Vec128_S32 to_int32_trunc()	const				{ return Vec128_S32(_mm_cvttps_epi32(v)); }
	Vec128_S32 to_int32_round()	const				{ return Vec128_S32(_mm_cvtps_epi32(v)); }
	float scalar_x() const							{ float tmp; _mm_store_ss(&tmp, v); return tmp; }
	void storeu(float *ptr)	const					{ _mm_storeu_ps(ptr, v); } // unaligned
	void storea(float *ptr)	const					{ _mm_store_ps(ptr, v); } // 16-byte aligned

	// bit casts
	Vec128_S32 s32() const							{ return Vec128_S32(_mm_castps_si128(v)); }
	Vec128_U32 u32() const							{ return Vec128_U32(_mm_castps_si128(v)); }
	Vec128_U8 u8() const							{ return Vec128_U8(_mm_castps_si128(v)); }

	// unary operations
	operator __m128() const							{ return v; }
	VecF32x4 operator -() const						{ return _mm_xor_ps(v, _mm_set1_ps(-0.0f)); }
//	VecF32x4 operator ~() const						{ return _mm_xor_ps(v, _mm_castsi128_ps(_mm_set1_epi32(-1))); }

	// binary assignment operators
	VecF32x4 &operator +=(VecF32x4r b)				{ v = _mm_add_ps(v, b); return *this; }
	VecF32x4 &operator -=(VecF32x4r b)				{ v = _mm_sub_ps(v, b); return *this; }
	VecF32x4 &operator *=(VecF32x4r b)				{ v = _mm_mul_ps(v, b); return *this; }
	VecF32x4 &operator /=(VecF32x4r b)				{ v = _mm_div_ps(v, b); return *this; }

	VecF32x4 &operator &=(VecF32x4r b)				{ v = _mm_and_ps(v, b); return *this; }
	VecF32x4 &operator |=(VecF32x4r b)				{ v = _mm_or_ps(v, b); return *this; }
	VecF32x4 &operator ^=(VecF32x4r b)				{ v = _mm_xor_ps(v, b); return *this; }

	// comparisons are not as operator overloads because they return a mask vector, not a bool
	VecF32x4 cmp_gt(VecF32x4r b) const				{ return _mm_cmpgt_ps(v, b); } // >
	VecF32x4 cmp_ge(VecF32x4r b) const				{ return _mm_cmpge_ps(v, b); } // >=
	VecF32x4 cmp_lt(VecF32x4r b) const				{ return _mm_cmplt_ps(v, b); } // <
	VecF32x4 cmp_le(VecF32x4r b) const				{ return _mm_cmple_ps(v, b); } // <=
	VecF32x4 cmp_eq(VecF32x4r b) const				{ return _mm_cmpeq_ps(v, b); } // ==
	VecF32x4 cmp_ngt(VecF32x4r b) const				{ return _mm_cmpngt_ps(v, b); } // ! >
	VecF32x4 cmp_nge(VecF32x4r b) const				{ return _mm_cmpnge_ps(v, b); } // ! >=
	VecF32x4 cmp_nlt(VecF32x4r b) const				{ return _mm_cmpnlt_ps(v, b); } // ! <
	VecF32x4 cmp_nle(VecF32x4r b) const				{ return _mm_cmpnle_ps(v, b); } // ! <=
	VecF32x4 cmp_neq(VecF32x4r b) const				{ return _mm_cmpneq_ps(v, b); } // ! ==

	// shuffles
	template<int x, int y, int z, int w>
	VecF32x4 shuf() const							{ return _mm_shuffle_ps(v, v, _MM_SHUFFLE(w,z,y,x)); }
	VecF32x4 xxxw() const							{ return shuf<0,0,0,3>(); }

	template<int lane>
	VecF32x4 dup() const							{ return _mm_shuffle_ps(v, v, _MM_SHUFFLE(lane,lane,lane,lane)); }

	// copy single lane into this
#ifdef DO_BUILD_SSE4
	template <int lane>
	VecF32x4 copy_lane(VecF32x4 from)				{ return _mm_blend_ps(v, from, 1<<lane); }
#endif

	VecF32x4 unpack_lo(VecF32x4r b) const			{ return _mm_unpacklo_ps(v, b); }
	VecF32x4 unpack_hi(VecF32x4r b) const			{ return _mm_unpackhi_ps(v, b); }

	// cyclic permutations of elements
	VecF32x4 yzwx() const							{ return shuf<1,2,3,0>(); }
	VecF32x4 wxyz() const							{ return shuf<3,0,1,2>(); }

	// cyclic permutations of first 3 elements
	VecF32x4 yzxw() const							{ return shuf<1,2,0,3>(); }
	VecF32x4 zxyw() const							{ return shuf<2,0,1,3>(); }

	// blockwise swaps
	VecF32x4 yxwz() const							{ return shuf<1,0,3,2>(); }
	VecF32x4 zwxy() const							{ return shuf<2,3,0,1>(); }

	// broadcasts
	VecF32x4 xxxx() const							{ return shuf<0,0,0,0>(); }
	VecF32x4 yyyy() const							{ return shuf<1,1,1,1>(); }
	VecF32x4 zzzz() const							{ return shuf<2,2,2,2>(); }
	VecF32x4 wwww() const							{ return shuf<3,3,3,3>(); }

	// utils
	VecF32x4 square() const							{ return _mm_mul_ps(v, v); }

	// some reductions
	VecF32x4 sum_across() const						{ VecF32x4 t = _mm_add_ps(*this, zwxy()); return _mm_add_ps(t, t.yxwz()); }
	VecF32x4 sum_across_inner_outer() const			{ VecF32x4 t = _mm_add_ps(*this, yxwz()); return _mm_add_ps(t, t.zwxy()); } // add inner then outer pairs, opposite to our default (gives different results for float)
};

#ifdef DO_BUILD_SSE4
inline Vec128_U8 Vec128_S32::gather_xyzw_lo8() const
{
	return Vec128_U8(_mm_shuffle_epi8(v, _mm_setr_epi8(0,4,8,12, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1)));
}
#endif

// Casts
inline Vec128_S16 Vec128_S8::s16() const			{ return Vec128_S16(v); }
inline Vec128_S32 Vec128_S8::s32() const			{ return Vec128_S32(v); }
inline Vec128_U8  Vec128_S8::u8() const				{ return Vec128_U8(v); }
inline Vec128_U16 Vec128_S8::u16() const			{ return Vec128_U16(v); }
inline Vec128_U32 Vec128_S8::u32() const			{ return Vec128_U32(v); }

inline Vec128_S8  Vec128_S16::s8() const			{ return Vec128_S8(v); }
inline Vec128_S32 Vec128_S16::s32() const			{ return Vec128_S32(v); }
inline Vec128_U8  Vec128_S16::u8() const			{ return Vec128_U8(v); }
inline Vec128_U16 Vec128_S16::u16() const			{ return Vec128_U16(v); }
inline Vec128_U32 Vec128_S16::u32() const			{ return Vec128_U32(v); }

inline Vec128_S8  Vec128_S32::s8() const			{ return Vec128_S8(v); }
inline Vec128_S16 Vec128_S32::s16() const			{ return Vec128_S16(v); }
inline Vec128_U8  Vec128_S32::u8() const			{ return Vec128_U8(v); }
inline Vec128_U16 Vec128_S32::u16() const			{ return Vec128_U16(v); }
inline Vec128_U32 Vec128_S32::u32() const			{ return Vec128_U32(v); }

inline Vec128_S8  Vec128_U8::s8() const				{ return Vec128_S8(v); }
inline Vec128_S16 Vec128_U8::s16() const			{ return Vec128_S16(v); }
inline Vec128_S32 Vec128_U8::s32() const			{ return Vec128_S32(v); }
inline Vec128_U16 Vec128_U8::u16() const			{ return Vec128_U16(v); }
inline Vec128_U32 Vec128_U8::u32() const			{ return Vec128_U32(v); }

inline Vec128_S8  Vec128_U16::s8() const			{ return Vec128_S8(v); }
inline Vec128_S16 Vec128_U16::s16() const			{ return Vec128_S16(v); }
inline Vec128_S32 Vec128_U16::s32() const			{ return Vec128_S32(v); }
inline Vec128_U8  Vec128_U16::u8() const			{ return Vec128_U8(v); }
inline Vec128_U32 Vec128_U16::u32() const			{ return Vec128_U32(v); }

inline Vec128_S8  Vec128_U32::s8() const			{ return Vec128_S8(v); }
inline Vec128_S16 Vec128_U32::s16() const			{ return Vec128_S16(v); }
inline Vec128_S32 Vec128_U32::s32() const			{ return Vec128_S32(v); }
inline Vec128_U8  Vec128_U32::u8() const			{ return Vec128_U8(v); }
inline Vec128_U16 Vec128_U32::u16() const			{ return Vec128_U16(v); }

inline VecF32x4   Vec128_S32::f32() const			{ return VecF32x4(_mm_castsi128_ps(v)); }
inline VecF32x4   Vec128_U32::f32() const			{ return VecF32x4(_mm_castsi128_ps(v)); }
inline VecF32x4   Vec128_U8::f32() const			{ return VecF32x4(_mm_castsi128_ps(v)); }


// Conversions
inline VecF32x4   Vec128_S32::to_f32() const		{ return VecF32x4::from_int32(v); }

#ifdef DO_BUILD_SSE4
inline Vec128_S16 Vec128_S8::to_s16_lo() const		{ return Vec128_S16(_mm_cvtepi8_epi16(v)); }
inline Vec128_S32 Vec128_S8::to_s32_lo() const		{ return Vec128_S32(_mm_cvtepi8_epi32(v)); }
inline Vec128_S16 Vec128_U8::to_sint16_lo() const	{ return Vec128_S16(_mm_cvtepu8_epi16(v)); }
inline Vec128_U16 Vec128_U8::to_uint16_lo() const	{ return Vec128_U16(_mm_cvtepu8_epi16(v)); }
#else
inline Vec128_U16 Vec128_U8::to_uint16_lo() const	{ return Vec128_U16(zext8to16_lo(v)); }
#endif
inline Vec128_S16 Vec128_U8::to_sint16_hi() const	{ return Vec128_S16(zext8to16_hi(v)); }
inline Vec128_U16 Vec128_U8::to_uint16_hi() const	{ return Vec128_U16(zext8to16_hi(v)); }
#ifdef DO_BUILD_SSE4
inline Vec128_S32 Vec128_U8::to_sint32_lo() const	{ return Vec128_S32(_mm_cvtepu8_epi32(v)); }
inline Vec128_S32 Vec128_S16::to_sint32() const		{ return Vec128_S32(_mm_cvtepi16_epi32(v)); }
#endif
inline Vec128_U8  Vec128_S16::to_uint8_sat() const	{ return Vec128_U8(_mm_packus_epi16(v, v));}
inline Vec128_U8  Vec128_S16::to_uint8_sat(Vec128_S16r b) const { return Vec128_U8(_mm_packus_epi16(v, b)); }
inline Vec128_U8  Vec128_U16::to_uint8_sat() const	{ return Vec128_U8(_mm_packus_epi16(v, v)); }
inline Vec128_U8  Vec128_U16::to_uint8_sat(Vec128_U16r b) const	{ return Vec128_U8(_mm_packus_epi16(v, b)); }
inline Vec128_U32 Vec128_U16::to_uint32_lo() const	{ return Vec128_U32(zext16to32_lo(v)); }

inline Vec128_S16 Vec128_S32::to_s16_sat(Vec128_S32r b) const { return Vec128_S16(_mm_packs_epi32(v, b)); }
//inline Vec128_S16 Vec128_U32::to_s16_sat(Vec128_U32r b) const { return Vec128_S16(_mm_packs_epi32(v, b)); }



inline Vec128_S32 Vec128_S16::madd(Vec128_S16r b) const { return Vec128_S32(_mm_madd_epi16(v, b)); }

#define RR_IMPL_BIN_OPS(op, opname) \
	RR_IMPL_BIN_OP(Vec128_S8, op, opname) \
	RR_IMPL_BIN_OP(Vec128_S16, op, opname) \
	RR_IMPL_BIN_OP(Vec128_S32, op, opname) \
	RR_IMPL_BIN_OP(Vec128_U8, op, opname) \
	RR_IMPL_BIN_OP(Vec128_U16, op, opname) \
	RR_IMPL_BIN_OP(Vec128_U32, op, opname)

#define RR_IMPL_BIN_OP(klass, op, opname) \
static inline klass op(klass##r a, klass##r b)	{ return a.opname(b); }

RR_IMPL_BIN_OPS(operator +, add)
RR_IMPL_BIN_OPS(operator -, sub)

RR_IMPL_BIN_OP(Vec128_S16, vmax, max)
RR_IMPL_BIN_OP(Vec128_U8,  vmax, max)

RR_IMPL_BIN_OP(Vec128_S16, vmin, min)
RR_IMPL_BIN_OP(Vec128_U8,  vmin, min)

#ifdef DO_BUILD_SSE4
RR_IMPL_BIN_OP(Vec128_S8,  vmax, max)
RR_IMPL_BIN_OP(Vec128_S32, vmax, max)
RR_IMPL_BIN_OP(Vec128_U16, vmax, max)
RR_IMPL_BIN_OP(Vec128_U32, vmax, max)

RR_IMPL_BIN_OP(Vec128_S8,  vmin, min)
RR_IMPL_BIN_OP(Vec128_S32, vmin, min)
RR_IMPL_BIN_OP(Vec128_U16, vmin, min)
RR_IMPL_BIN_OP(Vec128_U32, vmin, min)
#endif


RR_IMPL_BIN_OP(Vec128_S16, operator *, mullo)
RR_IMPL_BIN_OP(Vec128_U16, operator *, mullo)
#ifdef DO_BUILD_SSE4
RR_IMPL_BIN_OP(Vec128_S32, operator *, mullo)
RR_IMPL_BIN_OP(Vec128_U32, operator *, mullo)
#endif

#undef RR_IMPL_BIN_OP

#define RR_IMPL_BIN_OP(klass, op, opname) \
static inline klass operator op(klass a, Vec128r b) { a opname b; return a; }

RR_IMPL_BIN_OPS(&,&=)
RR_IMPL_BIN_OPS(|,|=)
RR_IMPL_BIN_OPS(^,^=)

#undef RR_IMPL_BIN_OP
#undef RR_IMPL_BIN_OPS

#ifdef DO_BUILD_SSSE3
inline Vec128_S16 Vec128_U8::madd_sat(Vec128_S8r b) const			{ return Vec128_S16(_mm_maddubs_epi16(v, b)); }
#endif
inline Vec128_S32 Vec128_U8::sad(Vec128_U8r b) const				{ return Vec128_S32(_mm_sad_epu8(v, b)); }

#ifdef DO_BUILD_SSE4
static inline Vec128_U8  vblend(Vec128_U8r  a, Vec128_U8r  b, Vec128r m){ return a.blend(b, m); }
static inline Vec128_U16 vblend(Vec128_U16r a, Vec128_U16r b, Vec128r m){ return vblend(a.u8(), b.u8(), m).u16(); }
#endif

static inline VecF32x4 operator +(VecF32x4r a, VecF32x4r b)		{ return _mm_add_ps(a, b); }
static inline VecF32x4 operator -(VecF32x4r a, VecF32x4r b)		{ return _mm_sub_ps(a, b); }
static inline VecF32x4 operator *(VecF32x4r a, VecF32x4r b)		{ return _mm_mul_ps(a, b); }
static inline VecF32x4 operator /(VecF32x4r a, VecF32x4r b)		{ return _mm_div_ps(a, b); }

static inline VecF32x4 operator &(VecF32x4r a, VecF32x4r b)		{ return _mm_and_ps(a, b); }
static inline VecF32x4 operator |(VecF32x4r a, VecF32x4r b)		{ return _mm_or_ps(a, b); }
static inline VecF32x4 operator ^(VecF32x4r a, VecF32x4r b)		{ return _mm_xor_ps(a, b); }
static inline VecF32x4 andnot(VecF32x4r a, VecF32x4r b)			{ return _mm_andnot_ps(b, a); } // a & ~b (mm_andnot is ~a & b)

template<int x,int y,int z,int w>
static inline Vec128_S32 shuffle_two(Vec128_S32r a, Vec128_S32r b){ return Vec128_S32(shuffle_two32<x,y,z,w>(a, b)); }

#ifdef DO_BUILD_SSE4
template<int sel>
static inline VecF32x4 vblend(VecF32x4r a, VecF32x4r b)			{ return _mm_blend_ps(a, b, sel); }
#endif

static inline VecF32x4 vmin(VecF32x4r a, VecF32x4r b)			{ return _mm_min_ps(a, b); }
static inline VecF32x4 vmax(VecF32x4r a, VecF32x4r b)			{ return _mm_max_ps(a, b); }

static inline VecF32x4 VecF32x4r_sqrt(VecF32x4r a)				{ return _mm_sqrt_ps(a); }

static inline S32 reduce_add(Vec128_S32r a)						{ return reduce_add_s32(a); }
static inline U32 reduce_add(Vec128_U32r a)						{ return reduce_add_u32(a); }

#ifdef DO_BUILD_SSE4

static inline S32 reduce_min(Vec128_S32r a)						{ Vec128_S32 t = vmin(a, a.shuf<1,0,3,2>()); t = vmin(t, t.shuf<2,3,0,1>()); return _mm_cvtsi128_si32(t); }
static inline U32 reduce_min(Vec128_U32r a)						{ Vec128_U32 t = vmin(a, a.shuf<1,0,3,2>()); t = vmin(t, t.shuf<2,3,0,1>()); return static_cast<U32>(_mm_cvtsi128_si32(t)); }

static inline S32 reduce_max(Vec128_S32r a)						{ Vec128_S32 t = vmax(a, a.shuf<1,0,3,2>()); t = vmax(t, t.shuf<2,3,0,1>()); return _mm_cvtsi128_si32(t); }
static inline U32 reduce_max(Vec128_U32r a)						{ Vec128_U32 t = vmax(a, a.shuf<1,0,3,2>()); t = vmax(t, t.shuf<2,3,0,1>()); return static_cast<U32>(_mm_cvtsi128_si32(t)); }

// shifts 256-bit value right by N bytes and returns lowest 128-bits
template <int N>
static Vec128_U32 vsrl_byte(Vec128_U32r lo, Vec128_U32r hi)		{ return Vec128_U32(_mm_alignr_epi8(hi, lo, N)); }

#endif

template<int x,int y,int z,int w>
static inline VecF32x4 shuffle_two32(VecF32x4r a, VecF32x4r b)	{ return _mm_shuffle_ps(a, b, _MM_SHUFFLE(w,z,y,x)); }

#ifdef DO_BUILD_SSE4
static inline Vec128_S16 abs_diff(Vec128_S16r a, Vec128_S16r b)	{ return (a - b).abs(); }

// rounding_shift_right(lerp * diff, 15) + lo
static inline Vec128_S16 vinterp16(Vec128_S16 lerp, Vec128_S16 diff)					{ return lerp.mulhrs(diff); }
static inline Vec128_S16 vinterp16(Vec128_S16 lerp, Vec128_S16 diff, Vec128_S16 lo) 	{ return lerp.mulhrs(diff) + lo; }
#endif

template <int N> static inline Vec128 vsrl128(Vec128r v)
{
	Vec128 lo_shift = _mm_srli_epi64(v, N);
	Vec128 hi_shift = _mm_slli_epi64(v, 64-N);
	Vec128 shifted = _mm_or_si128(lo_shift, _mm_srli_si128(hi_shift, 8));
	return shifted;
}

#endif


#ifdef DO_BUILD_NEON64

#if defined(__clang__) || defined(__GNUC__)
typedef U32 Vec128_U32un __attribute__((aligned(1)));
typedef U64 Vec128_U64un __attribute__((aligned(1)));
#else
typedef U32 Vec128_U32un;
typedef U64 Vec128_U64un;

#define Vec64_create_int8(typ, a,b,c,d,e,f,g,h) \
	vcreate_##typ((U64(a) & 0xff) | ((U64(b) & 0xff) << 8) | ((U64(c) & 0xff) << 16) | ((U64(d) & 0xff) << 24) | \
				  ((U64(e) & 0xff) << 32) | ((U64(f) & 0xff) << 40) | ((U64(g) & 0xff) << 48) | ((U64(h) & 0xff) << 56))
#define Vec64_create_int16(typ, a,b,c,d) vcreate_##typ((U64(a) & 0xffff) | ((U64(b) & 0xffff) << 16) | ((U64(c) & 0xffff) << 32) | ((U64(d) & 0xffff) << 48))
#define Vec64_create_int32(typ, a,b) vcreate_##typ((U64(a) & 0xffffffff) | ((U64(b) & 0xffffffff) << 32))

#endif

struct Vec128_S8;
struct Vec128_S16;
struct Vec128_S32;
struct Vec128_U8;
struct Vec128_U16;
struct Vec128_U32;
struct VecF32x4;

typedef const Vec128_S8  & Vec128_S8r;
typedef const Vec128_S16 & Vec128_S16r;
typedef const Vec128_S32 & Vec128_S32r;
typedef const Vec128_U8  & Vec128_U8r;
typedef const Vec128_U16 & Vec128_U16r;
typedef const Vec128_U32 & Vec128_U32r;
typedef const VecF32x4   & VecF32x4r;

struct Vec128_S8
{
	int8x16_t v;

	Vec128_S8() = default;
	Vec128_S8(int8x16_t x) : v(x) {}
	Vec128_S8(S8 a, S8 b, S8 c, S8 d, S8 e, S8 f, S8 g, S8 h, S8 i, S8 j, S8 k, S8 l, S8 m, S8 n, S8 o, S8 p)
#ifdef _MSC_VER
		: v(vcombine_s8(Vec64_create_int8(s8, a,b,c,d,e,f,g,h), Vec64_create_int8(s8, i,j,k,l,m,n,o,p))) {}
#else
		: v(int8x16_t { a,b,c,d, e,f,g,h, i,j,k,l, m,n,o,p }) {}
#endif
	explicit Vec128_S8(int8_t x) : v(vdupq_n_s8(x)) {}

	operator int8x16_t() const						{ return v; }

	static Vec128_S8 zero()							{ return vdupq_n_s8(0); }
	static Vec128_S8 loadu(const void *ptr)			{ return vld1q_s8((const int8_t*)ptr); }
	static Vec128_S8 loadu_lo32(const void *ptr)	{ return vld1q_lane_u32((const Vec128_U32un*)ptr, vdupq_n_u32(0), 0); }
	static Vec128_S8 loadu_lo64(const void *ptr)	{ return vld1q_lane_u64((const Vec128_U64un*)ptr, vdupq_n_u64(0), 0); }
	void storeu(void *ptr) const					{ vst1q_s8((int8_t*)ptr, v); }

	// reinterpret cast
	Vec128_S16 s16() const;
	Vec128_S32 s32() const;
	Vec128_U8  u8() const;
	Vec128_U16 u16() const;
	Vec128_U32 u32() const;

	// conversion
	Vec128_S16 to_s16_lo() const;
	Vec128_S32 to_s32_lo() const;

	template<int count>	Vec128_S8 shl() const		{ return vshlq_n_s8(v, count); }
	template<int count>	Vec128_S8 sra() const		{ return vshrq_n_s8(v, count); }
	template<int count>	Vec128_S8 srl() const		{ return vreinterpretq_s8_u8(vshrq_n_u8(vreinterpretq_u8_s8(v), count)); }
};

struct Vec128_S16
{
	int16x8_t v;

	Vec128_S16() = default;
	Vec128_S16(int16x8_t x) : v(x) {}
	explicit Vec128_S16(int16_t x) : v(vdupq_n_s16(x)) {}
	Vec128_S16(S16 a, S16 b, S16 c, S16 d, S16 e, S16 f, S16 g, S16 h)
#ifdef _MSC_VER
		: v (vcombine_s16(Vec64_create_int16(s16,a,b,c,d), Vec64_create_int16(s16,e,f,g,h))) {}
#else
		: v(int16x8_t { a,b,c,d, e,f,g,h, }) {}
#endif

	operator int16x8_t() const						{ return v; }

	static Vec128_S16 zero()								{ return vdupq_n_s16(0); }
	static Vec128_S16 repeat4(S16 a, S16 b, S16 c, S16 d)	{ return Vec128_S16(a,b,c,d, a,b,c,d); }
	static Vec128_S16 loada(const void *ptr)				{ return vld1q_s16((const int16_t*)ptr); }
	static Vec128_S16 loadu(const void *ptr)				{ return vld1q_s16((const int16_t*)ptr); }
	static Vec128_S16 loadu_lo32(const void *ptr)			{ return vld1q_lane_u32((const Vec128_U32un*)ptr, vdupq_n_u32(0), 0); }
	static Vec128_S16 loadu_lo64(const void *ptr)			{ return vld1q_lane_u64((const Vec128_U64un*)ptr, vdupq_n_u64(0), 0); }
	static int16x8x2_t loadu_x2(const void *ptr)			{ return vld1q_s16_x2((const int16_t*)ptr); }
	static Vec128_S16 loadu_zext8(const void *ptr)
	{
		uint16x8_t tmp = vmovl_u8(vld1_u8((const uint8_t*)ptr));
		return Vec128_S16(vreinterpretq_s16_u16(tmp));
	}
	static Vec128_S16 loadu_lo64_dup(const void *ptr)		{ return vld1q_dup_u64((const Vec128_U64un*)ptr); };
	void storea(void *ptr) const							{ vst1q_s16((int16_t*)ptr, v); }
	void storeu(void *ptr) const							{ vst1q_s16((int16_t*)ptr, v); }

	// Duplicate the low half of register to the high half
	Vec128_S16 dup_lo() const								{ return vdupq_laneq_u64(vreinterpretq_u64_s16(v), 0); }
	// Duplicate the high half of register to the low half
	Vec128_S16 dup_hi() const								{ return vdupq_laneq_u64(vreinterpretq_u64_s16(v), 1); }

	Vec128_S16 unpack_lo(Vec128_S16r b) const				{ return vzip1q_s16(v, b); }
	Vec128_S16 unpack_hi(Vec128_S16r b) const				{ return vzip2q_s16(v, b); }

	Vec128_S16 mullo(Vec128_S16r b) const 					{ return vmulq_s16(v, b); }

	// calculates (v*b) >> 16 keeping multiplication intermediate in 32-bit
	inline Vec128_S16 mulhi(Vec128_S16r b);

	// reinterpret cast
	Vec128_S8  s8() const;
	Vec128_S32 s32() const;
	Vec128_U8  u8() const;
	Vec128_U16 u16() const;
	Vec128_U32 u32() const;

	template<int count>	Vec128_S16 shl() const		{ return vshlq_n_s16(v, count); }
	template<int count>	Vec128_S16 sra() const		{ return vshrq_n_s16(v, count); }
	template<int count>	Vec128_S16 srl() const		{ return vreinterpretq_s16_u16(vshrq_n_u16(vreinterpretq_u16_s16(v), count)); }

	template<int count> Vec128_S16 rnd_sra() const	{ return vrshrq_n_s16(v, count); }

	Vec128_S32 to_sint32() const;
	Vec128_U8 to_uint8_sat() const;
	Vec128_U8 to_uint8_sat(Vec128_S16r b) const;
};

struct Vec128_S32
{
	int32x4_t v;

	Vec128_S32() = default;
	Vec128_S32(int32x4_t x) : v(x) {}
	explicit Vec128_S32(int32_t x) : v(vdupq_n_s32(x)) {}
	Vec128_S32(S32 a, S32 b, S32 c, S32 d)
#ifdef _MSC_VER
		: v(vcombine_s32(Vec64_create_int32(s32, a, b), Vec64_create_int32(s32, c, d))) {}
#else
		: v(int32x4_t { a,b,c,d }) {}
#endif

	operator int32x4_t() const						{ return v; }

	static Vec128_S32 zero()						{ return vdupq_n_s32(0); }
	static Vec128_S32 loadu(const void *ptr)		{ return vld1q_s32((const int32_t*)ptr); }
	static Vec128_S32 loadu_lo32(const void *ptr)	{ return vld1q_lane_s32((int32_t*)ptr, vdupq_n_s32(0), 0); }
	void storeu(void *ptr) const					{ vst1q_s32((int32_t*)ptr, v); }

	template<int lane> S32 extract() const			{ return vgetq_lane_s32(v, lane); }

	// reinterpret cast
	Vec128_S8  s8() const;
	Vec128_S16 s16() const;
	Vec128_U8  u8() const;
	Vec128_U16 u16() const;
	Vec128_U32 u32() const;
	VecF32x4   f32() const;

	// *this must be a truth value (i.e. -1 or 0 in all lanes)
	bool all() const								{ return vmaxvq_s32(v) == -1; }
	bool any() const								{ return vminvq_s32(v) == -1; }

	Vec128_S32 abs() const							{ return vabsq_s32(v); }

	Vec128_S32 cmp_eq(Vec128_S32 b) const			{ return vceqq_s32(v, b); }
	Vec128_S32 cmp_gt(Vec128_S32 b) const			{ return vcgtq_s32(v, b); }

	// compares as 2 64-bit lanes
	Vec128_S32 cmp_eq64(Vec128_S32r b) const		{ return vceqq_u64(v, b); }

	Vec128_S32 unpack_lo(Vec128_S32 b) const		{ return vzipq_s32(v, b).val[0]; }
	Vec128_S32 unpack_hi(Vec128_S32 b) const		{ return vzipq_s32(v, b).val[1]; }

	template<int count>	Vec128_S32 shl() const		{ return vshlq_n_s32(v, count); }
	template<int count>	Vec128_S32 sra() const		{ return vshrq_n_s32(v, count); }
	template<int count>	Vec128_S32 srl() const		{ return vreinterpretq_s32_u32(vshrq_n_u32(vreinterpretq_u32_s32(v), count)); }

	Vec128_S32 andnot(Vec128_S32r b) const			{ return vbicq_s32(v, b); }

	// mask.select(a,b) is mask ? a : b
	// mask must be a truth value (-1 or 0)
	Vec128_S32 select(Vec128_S32 a, Vec128_S32 b) const { return vbslq_s32(v, a, b); }

	// duplicate lane
	template <int lane> Vec128_S32 dup() const		{ return vdupq_laneq_s32(v, lane); }

	// blockwise swaps
	Vec128_S32 yxwz() const							{ return vrev64q_s32(v); }

	// cyclic permutations of elements
	Vec128_S32 zwxy() const							{ return vextq_s32(v, v, 2); }

	// shuffles
	Vec128_S32 xyxy() const							{ return vdupq_laneq_u64(vreinterpretq_u64_s16(v), 0); }
	Vec128_S32 zwzw() const							{ return vdupq_laneq_u64(vreinterpretq_u64_s16(v), 1); }

	Vec128_U8 gather_xyzw_lo8()	const;

	VecF32x4   to_f32() const;
	Vec128_S16 to_s16_sat(Vec128_S32r b) const;
};

struct Vec128_U8
{
	uint8x16_t v;

	Vec128_U8() = default;
	Vec128_U8(uint8x16_t x) : v(x) {}
	Vec128_U8(U8 a, U8 b, U8 c, U8 d, U8 e, U8 f, U8 g, U8 h, U8 i, U8 j, U8 k, U8 l, U8 m, U8 n, U8 o, U8 p)
#ifdef _MSC_VER
		: v(vcombine_u8(Vec64_create_int8(u8, a,b,c,d,e,f,g,h), Vec64_create_int8(u8, i,j,k,l,m,n,o,p))) {}
#else
		: v(uint8x16_t { a,b,c,d, e,f,g,h, i,j,k,l, m,n,o,p }) {}
#endif
	explicit Vec128_U8(uint8_t x) : v(vdupq_n_u8(x)) {}

	operator uint8x16_t() const						{ return v; }

	static Vec128_U8 zero()							{ return vdupq_n_u8(0); }
	static Vec128_U8 repeat2(U8 u0, U8 u1)			{ return vreinterpretq_u8_u16( vdupq_n_u16( (u1 << 8) | u0 ) ); }
	static Vec128_U8 repeat4(U8 a, U8 b, U8 c, U8 d) { return Vec128_U8(a,b,c,d, a,b,c,d, a,b,c,d, a,b,c,d); }

	static Vec128_U8 loada(const void *ptr)			{ return vld1q_u8((const uint8_t*)ptr); }
	static Vec128_U8 loadu(const void *ptr)			{ return vld1q_u8((const uint8_t*)ptr); }
	static Vec128_U8 loadu_lo32(const void *ptr)	{ return vld1q_lane_u32((const Vec128_U32un*)ptr, vdupq_n_u32(0), 0); }
	static Vec128_U8 loadu_lo64(const void *ptr)	{ return vld1q_lane_u64((const Vec128_U64un*)ptr, vdupq_n_u64(0), 0); }
	static uint8x16x4_t loadu_x4(const void* ptr) 	{ return vld1q_u8_x4((const uint8_t*)ptr); }

	void storeu(void *ptr) const					{ vst1q_u8((uint8_t*)ptr, v); }
	void storea(void *ptr) const					{ vst1q_u8((uint8_t*)ptr, v); }
	void storeu_lo64(void *ptr) const				{ vst1_u8((uint8_t*)ptr, vget_low_u8(v)); }
	void storeu_lo32(void *ptr) const				{ vst1q_lane_u32((Vec128_U32un*)ptr, vreinterpretq_u32_u8(v), 0); }

	Vec128_U16 to_uint16_lo() const;
	Vec128_S16 to_sint16_lo() const;
	Vec128_S16 to_sint16_hi() const;
	Vec128_S32 to_sint32_lo() const;

	Vec128_U8 unpack_lo(Vec128_U8r b) const			{ return vzip1q_u8(v, b); }
	Vec128_U8 unpack_hi(Vec128_U8r b) const			{ return vzip2q_u8(v, b); }
	Vec128_U8 unpack_lo64(Vec128_U8r b) const		{ return vzip1q_u64(v, b); }
	Vec128_U8 unpack_hi64(Vec128_U8r b) const		{ return vzip2q_u64(v, b); }

	// reinterpret cast
	Vec128_S8  s8() const;
	Vec128_S16 s16() const;
	Vec128_S32 s32() const;
	Vec128_U16 u16() const;
	Vec128_U32 u32() const;
	VecF32x4   f32() const;

	template<int count>	Vec128_U8 shl() const		{ return vshlq_n_u8(v, count); }
	template<int count>	Vec128_U8 sra() const		{ return vreinterpretq_u8_s8(vshrq_n_s8(vreinterpretq_s8_u8(v), count)); }
	template<int count>	Vec128_U8 srl() const		{ return vshrq_n_u8(v, count); }

	// duplicate 32-bit lane
	template <int lane> Vec128_U8 dup32() const		{ return vreinterpretq_u8_u32(vdupq_laneq_u32(vreinterpretq_u32_u8(v), lane)); }

	// shuffle
	Vec128_U8 shuf(Vec128_U8r m) const				{ return vqtbl1q_u8(v, m); }
	Vec128_U8 shuf(Vec128_S8r m) const				{ return vqtbl1q_u8(v, m.u8()); }

	// compare
	Vec128_U8 cmp_eq(Vec128_U8r b) const			{ return vceqq_u8(v, b); }

	Vec128_U8 andnot(Vec128_U8r b) const			{ return vbicq_u8(v, b); }

	// some reductions
	void reduce_max_8away()							{ v = vmaxq_u8(v, vreinterpretq_u8_u32(vextq_u32(vreinterpretq_u32_u8(v), vreinterpretq_u32_u8(v), 2))); }
	void reduce_max_4away()							{ v = vmaxq_u8(v, vreinterpretq_u8_u32(vrev64q_u32(vreinterpretq_u32_u8(v)))); }
};

struct Vec128_U16
{
	uint16x8_t v;

	Vec128_U16() = default;
	Vec128_U16(uint16x8_t x) : v(x) {}
	Vec128_U16(U16 a, U16 b, U16 c, U16 d, U16 e, U16 f, U16 g, U16 h)
#ifdef _MSC_VER
		: v(vcombine_u8(Vec64_create_int16(u16, a,b,c,d), Vec64_create_int16(u16, e,f,g,h))) {}
#else
		: v(uint16x8_t { a,b,c,d, e,f,g,h }) {}
#endif
	explicit Vec128_U16(uint16_t x) : v(vdupq_n_u16(x)) {}

	operator uint16x8_t() const								{ return v; }

	static Vec128_U16 zero()								{ return vdupq_n_u16(0); }
	static Vec128_U16 repeat4(U16 a, U16 b, U16 c, U16 d)	{ return Vec128_U16(a,b,c,d, a,b,c,d); }
	static Vec128_U16 loadu(const void *ptr)				{ return vld1q_u16((const uint16_t*)ptr); }
	static Vec128_U16 loada(const void *ptr)				{ return vld1q_u16((const uint16_t*)ptr); }
	static Vec128_U16 loadu_lo32(const void *ptr)			{ return vld1q_lane_u32((const Vec128_U32un*)ptr, vdupq_n_u32(0), 0); }
	void storeu(void *ptr) const							{ vst1q_u16((uint16_t*)ptr, v); }

	// reinterpret cast
	Vec128_S8  s8() const;
	Vec128_S16 s16() const;
	Vec128_S32 s32() const;
	Vec128_U8  u8() const;
	Vec128_U32 u32() const;

	Vec128_U8 to_uint8_sat() const							{ return vcombine_u8(vqmovn_u16(v), vqmovn_u16(v)); }
	Vec128_U8 to_uint8_sat(Vec128_U16r b) const				{ return vcombine_u8(vqmovn_u16(v), vqmovn_u16(b)); }
	Vec128_U32 to_uint32_lo() const;

	// calculates (v*b) >> 16 keeping multiplication intermediate in 32-bit
	inline Vec128_U16 mulhi(Vec128_U16r b);

	template<int count> Vec128_U16 shl() const				{ return vshlq_n_u16(v, count); }
	template<int count> Vec128_U16 sra() const				{ return vreinterpretq_u16_s16(vshrq_n_s16(vreinterpretq_s16_u16(v), count)); }
	template<int count> Vec128_U16 srl() const				{ return vshrq_n_u16(v, count); }
	template<int count> Vec128_U16 sli(Vec128_U16r b) const	{ return vsliq_n_u16(b, v, count); }

	// shuffle of bytes
	Vec128_U16 shuf(Vec128_U8r m) const						{ return Vec128_U8(vqtbl1q_u8(u8(), m)).u16(); }
};

struct Vec128_U32
{
	uint32x4_t v;

	Vec128_U32() = default;
	Vec128_U32(uint32x4_t x) : v(x) {}
	explicit Vec128_U32(uint32_t x) : v(vdupq_n_u32(x)) {}
	Vec128_U32(U32 a, U32 b, U32 c, U32 d)
#ifdef _MSC_VER
		: v(vcombine_u32(Vec64_create_int32(u32, a,b), Vec64_create_int32(u32, c,d))) {}
#else
		: v(uint32x4_t { a,b,c,d }) {}
#endif

	operator uint32x4_t() const						{ return v; }

	static Vec128_U32 zero()						{ return vdupq_n_u32(0); }
	static Vec128_U32 loada(const void *ptr)		{ return vld1q_u32((const uint32_t*)ptr); }
	static Vec128_U32 loadu(const void *ptr)		{ return vld1q_u32((const uint32_t*)ptr); }
	static Vec128_U32 loadu_lo32(const void *ptr)	{ return vld1q_lane_u32((const Vec128_U32un*)ptr, vdupq_n_u32(0), 0); }
	static Vec128_U32 loadu_lo64(const void *ptr)	{ return vld1q_lane_u64((const Vec128_U64un*)ptr, vdupq_n_u64(0), 0); }
	static Vec128_U32 loada_dup(const U32 *ptr)		{ return vld1q_dup_u32(ptr); }
	void storea(void *ptr) const					{ vst1q_u32((uint32_t*)ptr, v); }
	void storeu(void *ptr) const					{ vst1q_u32((uint32_t*)ptr, v); }
	void storeu_lo32(void *ptr) const				{ vst1q_lane_u32((Vec128_U32un*)ptr, v, 0); }

	template <int lane>
	void load_lane(const U32 *ptr)					{ v = vld1q_lane_u32(ptr, v, lane); }

	// reinterpret cast
	Vec128_S8  s8() const;
	Vec128_S16 s16() const;
	Vec128_S32 s32() const;
	Vec128_U8  u8() const;
	Vec128_U16 u16() const;
	VecF32x4   f32() const;

	template<int count> Vec128_U32 shl() const				{ return vshlq_n_u32(v, count); }
	template<int count> Vec128_U32 sra() const				{ return vreinterpretq_u32_s32(vshrq_n_s32(vreinterpretq_s32_u32(v), count)); }
	template<int count> Vec128_U32 srl() const				{ return vshrq_n_u32(v, count); }
	template<int count> Vec128_U32 sli(Vec128_U32r b) const	{ return vsliq_n_u32(b, v, count); }

	// shifts 128-bit value right by N bytes
	template <int N>
	Vec128_U32 srl_byte() const								{ return vreinterpretq_u32_u8(vextq_u8(u8(), Vec128_U8::zero(), N)); }

	// compares
	Vec128_U32 cmp_eq(Vec128_U32r b) const			{ return vceqq_u32(v, b); }
	Vec128_U32 cmp_gt(Vec128_U32r b) const			{ return vcgtq_u32(v, b); }

	// mask.select(a,b) is mask ? a : b
	// mask must be a truth value (-1 or 0)
	Vec128_U32 select(Vec128_U32 a, Vec128_U32 b) const { return vbslq_u32(v, a, b); }

	// get lane
	template <int lanen> uint32_t lane() const		{ return vgetq_lane_u32(v, lanen); }

	// duplicate lane
	template <int lane> Vec128_U32 dup() const		{ return vdupq_laneq_u32(v, lane); }

	template <int lane>
	Vec128_U32 copy_lane(Vec128_U32r from) const	{ return vcopyq_laneq_u32(v, lane, from, lane); }

	// cyclic permutations of elements
	Vec128_U32 yzwx() const							{ return vextq_u32(v, v, 1); }
	Vec128_U32 zwxy() const							{ return vextq_u32(v, v, 2); }
	Vec128_U32 wxyz() const							{ return vextq_u32(v, v, 3); }

	Vec128_U32 xxxw() const							{ return vqtbl1q_u8(v, Vec128_U8(0,1,2,3, 0,1,2,3, 0,1,2,3, 12,13,14,15)); }

	Vec128_U32 andnot(Vec128_U32r b) const			{ return vbicq_u32(v, b); }

	// Truncate 32-bit values to 8-bits, mod 256
	Vec128_U8 narrow32to8_mod() const
	{
		return u8().shuf(Vec128_U8 { 0,4,8,12, 16,16,16,16, 16,16,16,16, 16,16,16,16 });
	}

	// returns (per lane) 32-bit sum of dot products in each group of 4 bytes
	// [ a0,a1,a2,a3, ... ] * [ b0,b1,b2,b3, ...] => [ a0*b0 + a1*b1 + a2*b2 + a3*b3, ...]
	static Vec128_U32 dot(Vec128_U8r a, Vec128_U8r b)
	{
		// multiply as 16-bit
		uint16x8_t mul_lo = vmull_u8(vget_low_u8(a), vget_low_u8(b));
		uint16x8_t mul_hi = vmull_u8(vget_high_u8(a), vget_high_u8(b));

		// sum pairs of 16-bit values into 32-bit lanes
		uint32x4_t sum_lo = vpaddlq_u16(mul_lo); // 0+1,   2+3,   4+5,   6+7
		uint32x4_t sum_hi = vpaddlq_u16(mul_hi); // 8+9, 10+11, 12+13, 14+15

		// 0+1+2+3, 4+5+6+7, 8+9+10+11, 12+13+14+15
		uint32x4_t sum = vpaddq_u32(sum_lo, sum_hi);
		return sum;
	}

	// similar to mul_sum, but adds the result to current vector
	void add_dot(Vec128_U8r a, Vec128_U8r b)
	{
		// this functionality can be replaced with vdotq_u32 if UDOT is available
		v = vaddq_u32(v, dot(a, b));
	}

	// add_dot but "b" operand promises it's U7 (meaning MSB never set)
	// this operation is interesting because it can be efficiently done in SSE4 as well
	// on NEON it's straightforward
	void add_dot_u8u7(Vec128_U8r a, Vec128_U8r b)
	{
		add_dot(a, b);
	}

	static Vec128_U32 sqr_sum(Vec128_U8r a)
	{
		return dot(a, a);
	}

	void add_sqr_sum(Vec128_U8r a)
	{
		add_dot(a, a);
	}
};

struct VecF32x4
{
	float32x4_t v;

	VecF32x4() = default;
	VecF32x4(float32x4_t x) : v(x) {}
	explicit VecF32x4(float x) : v(vdupq_n_f32(x)) {}
	VecF32x4(float a, float b, float c, float d)
#ifdef _MSC_VER
	{
		const float tmp[4] = { a, b, c, d };
		v = vld1q_f32(tmp);
	}
#else
		: v(float32x4_t { a,b,c,d }) {}
#endif

	operator float32x4_t() const					{ return v; }

	static VecF32x4 zero()							{ return vdupq_n_f32(0.0f); }
	static VecF32x4 from_int32(Vec128_S32 x)		{ return vcvtq_f32_s32(x); }
	static VecF32x4 from_int32(Vec128_U32 x)		{ return vcvtq_f32_u32(x); }
	static VecF32x4 loadu(const void *ptr)			{ return vld1q_f32((const float*)ptr); }
	static VecF32x4 loada(const void *ptr)			{ return vld1q_f32((const float*)ptr); }
	static VecF32x4 load_scalar(const float *ptr)	{ return vld1q_dup_f32(ptr); }
	static VecF32x4 load_pair(const float *ptr)		{ return vcombine_f32(vld1_f32(ptr), vdup_n_f32(0.f)); }
	static float32x4x4_t loadu_x4(const void* ptr) 	{ return vld1q_f32_x4((const float*)ptr); }

	static VecF32x4 bitmask(U32 a)					{ return vreinterpretq_f32_u32(vdupq_n_u32(a)); }
	static VecF32x4 bitmask(U32 a, U32 b, U32 c, U32 d) { return Vec128_U32 { a, b, c, d }.f32(); }

	F32 scalar_x() const							{ return vdups_laneq_f32(v, 0); }
	void storeu(void *ptr) const					{ vst1q_f32((float*)ptr, v); }
	void storea(void *ptr) const					{ vst1q_f32((float*)ptr, v); }

	// bit casts
	Vec128_U8 u8() const							{ return Vec128_U8(vreinterpretq_u8_f32(v)); }
	Vec128_S32 s32() const							{ return Vec128_S32(vreinterpretq_s32_f32(v)); }
	Vec128_U32 u32() const							{ return Vec128_U32(vreinterpretq_u32_f32(v)); }

	Vec128_S32 to_int32_trunc()	const				{ return vcvtq_s32_f32(v); }
	Vec128_S32 to_int32_round()	const				{ return vcvtnq_s32_f32(v); }

	// compares
	VecF32x4 cmp_eq(VecF32x4 b) const				{ return vreinterpretq_f32_u32(vceqq_f32(v, b)); }
	VecF32x4 cmp_gt(VecF32x4 b) const				{ return vreinterpretq_f32_u32(vcgtq_f32(v, b)); }

	// get lane
	template <int lanen> F32 lane() const			{ return vgetq_lane_f32(v, lanen); }

	// duplicate lane
	template <int lane> VecF32x4 dup() const		{ return vdupq_laneq_f32(v, lane); }

	// copy single lane into this
	template <int lane>
	VecF32x4 copy_lane(VecF32x4 from)				{ return vcopyq_laneq_f32(v, lane, from, lane); }

	VecF32x4 unpack_lo(VecF32x4r b) const			{ return vzipq_f32(v, b).val[0]; }
	VecF32x4 unpack_hi(VecF32x4r b) const			{ return vzipq_f32(v, b).val[1]; }

	// broadcasts
	VecF32x4 xxxx() const							{ return dup<0>(); }
	VecF32x4 yyyy() const							{ return dup<1>(); }
	VecF32x4 zzzz() const							{ return dup<2>(); }
	VecF32x4 wwww() const							{ return dup<3>(); }

	// cyclic permutations of elements
	VecF32x4 yzwx() const							{ return vextq_f32(v, v, 1); }
	VecF32x4 wxyz() const							{ return vextq_f32(v, v, 3); }

	// cyclic permutations of first 3 elements
	VecF32x4 yzxw() const							{ return u8().shuf(Vec128_U8 { 4,5,6,7, 8,9,10,11, 0,1,2,3, 12,13,14,15 }).f32(); }
	VecF32x4 zxyw() const							{ return u8().shuf(Vec128_U8 { 8,9,10,11, 0,1,2,3, 4,5,6,7, 12,13,14,15 }).f32(); }

	// blockwise swaps
	VecF32x4 yxwz() const							{ return vrev64q_f32(v); }
	VecF32x4 zwxy() const							{ return vextq_f32(v, v, 2); }

	VecF32x4 xyyz() const							{ return u8().shuf(Vec128_U8 { 0,1,2,3, 4,5,6,7, 4,5,6,7, 8,9,10,11 }).f32(); }
	VecF32x4 xxxw() const							{ return u8().shuf(Vec128_U8 { 0,1,2,3, 0,1,2,3, 0,1,2,3, 12,13,14,15 }).f32(); }

	// utils
	VecF32x4 square() const							{ return vmulq_f32(v, v); }

	// some reduction
	VecF32x4 sum_across() const						{ VecF32x4 t = vaddq_f32(*this, zwxy()); return vaddq_f32(t, t.yxwz()); }
	VecF32x4 sum_across_inner_outer() const			{ VecF32x4 t = vpaddq_f32(v, v); return vpaddq_f32(t, t); }
};

inline Vec128_U8 Vec128_S32::gather_xyzw_lo8() const
{
	// packs low byte of each 32-bit lane into 32-bit lane of result
	return u8().shuf( Vec128_U8 { 0,4,8,12, 16,16,16,16, 16,16,16,16, 16,16,16,16 } );
}

Vec128_S16 Vec128_S16::mulhi(Vec128_S16r b)
{
	Vec128_S32 lo = vmull_s16( vget_low_s16(v), vget_low_s16(b) );
	Vec128_S32 hi = vmull_s16( vget_high_s16(v), vget_high_s16(b) );
	return vuzp2q_u16( lo.s16(), hi.s16() );
}

Vec128_U16 Vec128_U16::mulhi(Vec128_U16r b)
{
	Vec128_U32 lo = vmull_u16( vget_low_u16(v), vget_low_u16(b) );
	Vec128_U32 hi = vmull_u16( vget_high_u16(v), vget_high_u16(b) );
	return vuzp2q_u16( lo.u16(), hi.u16() );
}

inline VecF32x4 Vec128_S32::to_f32() const						{ return VecF32x4::from_int32(*this); }

// sign extend low 4 int16 to int32
inline Vec128_S32 Vec128_S16::to_sint32() const					{ return vmovl_s16(vget_low_s16(v)); }
// saturate int16 to uint8
inline Vec128_U8 Vec128_S16::to_uint8_sat() const				{ return vcombine_u8( vqmovun_s16(v), vqmovun_s16(v) ); }
// saturate 8+8 values of int16 into 16 values of uint8
inline Vec128_U8 Vec128_S16::to_uint8_sat(Vec128_S16r b) const	{ return vcombine_u8( vqmovun_s16(v), vqmovun_s16(b) ); }
// saturate 4+4 values of int32 into 8 values of int16
inline Vec128_S16 Vec128_S32::to_s16_sat(Vec128_S32r b) const	{ return vcombine_s16( vqmovn_s32(v), vqmovn_s32(b) ); }
// sign extend low 8 bytes into eight 32-bit lanes
inline Vec128_S16 Vec128_S8::to_s16_lo() const					{ return vmovl_s8(vget_low_s8(v)); }
// sign extend low 4 bytes into four 32-bit lanes
inline Vec128_S32 Vec128_S8::to_s32_lo() const					{ return vmovl_s16( vget_low_s16( vmovl_s8( vget_low_s8(v) ) ) ); }
// zero-extend low 8 bytes to U16
inline Vec128_U16 Vec128_U8::to_uint16_lo() const				{ return vmovl_u8(vget_low_u8(v)); }
// zero-extend low 8 bytes to S16
inline Vec128_S16 Vec128_U8::to_sint16_lo() const				{ return vreinterpretq_s16_u16( vmovl_u8( vget_low_u8(v) ) ); }
// zero-extend high 8 bytes to S16
inline Vec128_S16 Vec128_U8::to_sint16_hi() const				{ return vreinterpretq_s16_u16( vmovl_u8( vget_high_u8(v) ) ); }
// zero-extend low 4 bytes to S32
inline Vec128_S32 Vec128_U8::to_sint32_lo() const 				{ return vreinterpretq_s32_u32( vmovl_u16( vget_low_u16( vmovl_u8( vget_low_u8(v) ) ) ) ); }
// zero-extend low 4 uint16 to uint32
inline Vec128_U32 Vec128_U16::to_uint32_lo() const				{ return vmovl_u16(vget_low_u16(v)); }

#define RR_IMPL_CAST(tsrc, tdst, nsrc, ndst) \
	inline Vec128_##tdst Vec128_##tsrc::ndst() const { return vreinterpretq_##ndst##_##nsrc(v); }

RR_IMPL_CAST(S8,  S16, s8,  s16)
RR_IMPL_CAST(S8,  S32, s8,  s32)
RR_IMPL_CAST(S8,  U8,  s8,  u8 )
RR_IMPL_CAST(S8,  U16, s8,  u16)
RR_IMPL_CAST(S8,  U32, s8,  u32)

RR_IMPL_CAST(S16, S8,  s16, s8 )
RR_IMPL_CAST(S16, S32, s16, s32)
RR_IMPL_CAST(S16, U8,  s16, u8 )
RR_IMPL_CAST(S16, U16, s16, u16)
RR_IMPL_CAST(S16, U32, s16, u32)

RR_IMPL_CAST(S32, S8,  s32, s8 )
RR_IMPL_CAST(S32, S16, s32, s16)
RR_IMPL_CAST(S32, U8,  s32, u8 )
RR_IMPL_CAST(S32, U16, s32, u16)
RR_IMPL_CAST(S32, U32, s32, u32)

RR_IMPL_CAST(U8,  S8,  u8,  s8 )
RR_IMPL_CAST(U8,  S16, u8,  s16)
RR_IMPL_CAST(U8,  S32, u8,  s32)
RR_IMPL_CAST(U8,  U16, u8,  u16)
RR_IMPL_CAST(U8,  U32, u8,  u32)

RR_IMPL_CAST(U16, S8,  u16, s8 )
RR_IMPL_CAST(U16, S16, u16, s16)
RR_IMPL_CAST(U16, S32, u16, s32)
RR_IMPL_CAST(U16, U8,  u16, u8 )
RR_IMPL_CAST(U16, U32, u16, u32)

RR_IMPL_CAST(U32, S8,  u32, s8 )
RR_IMPL_CAST(U32, S16, u32, s16)
RR_IMPL_CAST(U32, S32, u32, s32)
RR_IMPL_CAST(U32, U8,  u32, u8 )
RR_IMPL_CAST(U32, U16, u32, u16)

inline VecF32x4 Vec128_S32::f32() const { return vreinterpretq_f32_s32(v); }
inline VecF32x4 Vec128_U8::f32() const  { return vreinterpretq_f32_u8(v); }
inline VecF32x4 Vec128_U32::f32() const { return vreinterpretq_f32_u32(v); }

#undef RR_IMPL_CAST

#define RR_TYPE_BIN_OP(type, optype, op, opname ) \
	static inline Vec128_##type operator op (Vec128_##type##r a, Vec128_##type##r b)	{ return v##opname##q_##optype(a, b); } \
	static inline Vec128_##type& operator op##= (Vec128_##type& a, Vec128_##type##r b)	{ a.v = v##opname##q_##optype(a, b); return a; }

#define RR_TYPE_BIN_OPS(type, stype, optype) \
	RR_TYPE_BIN_OP(type, optype, +, add) \
	RR_TYPE_BIN_OP(type, optype, -, sub) \
	RR_TYPE_BIN_OP(type, optype, *, mul) \
	RR_TYPE_BIN_OP(type, optype, &, and) \
	RR_TYPE_BIN_OP(type, optype, |, orr) \
	RR_TYPE_BIN_OP(type, optype, ^, eor) \
	static inline Vec128_##type operator << (Vec128_##type##r a, Vec128_##stype##r b)	{ return vshlq_##optype(a, b); } \
	static inline Vec128_##type vmin(Vec128_##type##r a, Vec128_##type##r b)			{ return vminq_##optype(a, b); } \
	static inline Vec128_##type vmax(Vec128_##type##r a, Vec128_##type##r b)			{ return vmaxq_##optype(a, b); } \
	static inline Vec128_##type abs_diff(Vec128_##type##r a, Vec128_##type##r b)		{ return vabdq_##optype(a, b); }

RR_TYPE_BIN_OPS(S8,  S8,  s8 )
RR_TYPE_BIN_OPS(S16, S16, s16)
RR_TYPE_BIN_OPS(S32, S32, s32)
RR_TYPE_BIN_OPS(U8,  S8,  u8 )
RR_TYPE_BIN_OPS(U16, S16, u16)
RR_TYPE_BIN_OPS(U32, U32, u32)

#undef RR_TYPE_BIN_OPS
#undef RR_TYPE_BIN_OP


#define RR_FLOAT_BIN_OP(op, opname) \
	static inline VecF32x4 operator op (VecF32x4r a, VecF32x4r b)		{ return v##opname##q_f32(a, b); } \
	static inline VecF32x4& operator op##= (VecF32x4& a, VecF32x4r b)	{ a.v = v##opname##q_f32(a, b); return a; }

RR_FLOAT_BIN_OP(+, add)
RR_FLOAT_BIN_OP(-, sub)
RR_FLOAT_BIN_OP(*, mul)
RR_FLOAT_BIN_OP(/, div)

#undef RR_FLOAT_BIN_OP


#define RR_FLOAT_BIT_OP(op) \
	static inline VecF32x4 operator op (VecF32x4r a, VecF32x4r b)		{ return (a.u32() op b.u32()).f32(); } \
	static inline VecF32x4& operator op##= (VecF32x4& a, VecF32x4r b)	{ a = (a.u32() op b.u32()).f32(); return a; }

RR_FLOAT_BIT_OP(&);
RR_FLOAT_BIT_OP(|);
RR_FLOAT_BIT_OP(^);

#undef RR_FLOAT_BIT_OP

static inline VecF32x4 operator - (VecF32x4r a)		{ return vnegq_f32(a); }


#define RR_SQR_OP(bits, dtype, stype, optype) \
	static inline Vec128_##dtype sqr##bits##_lo(Vec128_##stype##r a)	{ return vmull_##optype(vget_low_##optype(a), vget_low_##optype(a)); } \
	static inline Vec128_##dtype sqr##bits##_hi(Vec128_##stype##r a)	{ return vmull_##optype(vget_high_##optype(a), vget_high_##optype(a)); }

RR_SQR_OP(16, S16,  S8,  s8)
RR_SQR_OP(16, U16,  U8,  u8)
RR_SQR_OP(32, S32, S16, s16)
RR_SQR_OP(32, U32, U16, u16)

#undef RR_SQR_OP


static inline Vec128_U16 pack_narrow(Vec128_U32r a, Vec128_U32r b)	{ return vcombine_u16(vmovn_u32(a), vmovn_u32(b)); }
static inline Vec128_S16 pack_narrow(Vec128_S32r a, Vec128_S32r b)	{ return vcombine_s16(vmovn_s32(a), vmovn_s32(b)); }
static inline Vec128_U8  pack_narrow(Vec128_U16r a, Vec128_U16r b)	{ return vcombine_u8(vmovn_u16(a), vmovn_u16(b)); }
static inline Vec128_S8  pack_narrow(Vec128_S16r a, Vec128_S16r b)	{ return vcombine_s8(vmovn_s16(a), vmovn_s16(b)); }


#define RR_REDUCTION_OP(op) \
	static S8  reduce_##op(Vec128_S8 a)								{ return v##op##vq_s8(a); } \
	static S16 reduce_##op(Vec128_S16 a)							{ return v##op##vq_s16(a); } \
	static S32 reduce_##op(Vec128_S32 a)							{ return v##op##vq_s32(a); } \
	static U8  reduce_##op(Vec128_U8 a)								{ return v##op##vq_u8(a); } \
	static U16 reduce_##op(Vec128_U16 a)							{ return v##op##vq_u16(a); } \
	static U32 reduce_##op(Vec128_U32 a)							{ return v##op##vq_u32(a); }

RR_REDUCTION_OP(add);
RR_REDUCTION_OP(min);
RR_REDUCTION_OP(max);

#undef RR_REDUCTION_OP

static inline VecF32x4 vmin(VecF32x4r a, VecF32x4r b)				{ return vminq_f32(a, b); }
static inline VecF32x4 vmax(VecF32x4r a, VecF32x4r b)				{ return vmaxq_f32(a, b); }

static inline VecF32x4 VecF32x4r_sqrt(VecF32x4r a)					{ return vsqrtq_f32(a); }

static inline Vec128_S32 sext16to32_lo(Vec128_S16r x)				{ return vshrq_n_s32(vreinterpretq_s32_s16(vzip1q_s16(x, x)), 16); }
static inline Vec128_S32 sext16to32_hi(Vec128_S16r x)				{ return vshrq_n_s32(vreinterpretq_s32_s16(vzip2q_s16(x, x)), 16); }

// shifts 256-bit value right by N bytes and returns lowest 128-bits
template <int N>
static Vec128_U32 vsrl_byte(Vec128_U32r lo, Vec128_U32r hi)			{ return vreinterpretq_u32_u8(vextq_u8(lo.u8(), hi.u8(), N)); }

static inline Vec128_U8 vblend(Vec128_U8r a, Vec128_U8r b, Vec128_U8r mask)		{ return vbslq_u8(mask, b, a); }
static inline Vec128_U16 vblend(Vec128_U16r a, Vec128_U16r b, Vec128_U16r mask)	{ return vbslq_u16(mask, b, a); }

// rounding_shift_right(lerp * diff, 15) + lo
static inline Vec128_S16 vinterp16(Vec128_S16 lerp, Vec128_S16 diff) 					{ return vqrdmulhq_s16(lerp, diff); }
// this folds to SQRDMLAH on ARMv8.1 targets that have it (with Clang anyway) but is also 100% compatible with ARMv8.0
static inline Vec128_S16 vinterp16(Vec128_S16 lerp, Vec128_S16 diff, Vec128_S16 lo) 	{ return vqaddq_s16(lo, vqrdmulhq_s16(lerp, diff)); }

template <int N> static inline Vec128_U8 vsrl128(Vec128_U8r v)
{
	uint64x2_t lo_shift = vshrq_n_u64(v, N);
	uint64x2_t hi_shift = vshlq_n_u64(v, 64-N);
	uint64x2_t shifted = vorrq_u64(lo_shift, vzip2q_u64(hi_shift, lo_shift));
	return shifted;
}

#endif

OODLE_NS_END

