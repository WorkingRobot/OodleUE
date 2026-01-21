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
	template<int index> S32 extract() const			{ return _mm_extract_epi32(v, index); }
	template<int index> void insert(S32 x)			{ v = _mm_insert_epi32(v, x, index); }
#endif

	VecF32x4   to_f32() const;
	Vec128_S16 to_s16_sat(Vec128_S32r b) const;

	// cast
	Vec128_S8  s8() const;
	Vec128_S16 s16() const;
	Vec128_U8  u8() const;
	Vec128_U16 u16() const;
	Vec128_U32 u32() const;

	// unary operations
	int movemask() const 							{ return _mm_movemask_ps(_mm_castsi128_ps(v)); }

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

	Vec128_S32 andnot(Vec128r b)					{ return Vec128_S32(_mm_andnot_si128(b, v)); } // a & ~b (mm_andnot is ~a & b)

	template<int count>	Vec128_S32 shl() const		{ return Vec128_S32(_mm_slli_epi32(v, count)); }
	template<int count> Vec128_S32 sra() const		{ return Vec128_S32(_mm_srai_epi32(v, count)); }
	template<int count>	Vec128_S32 srl() const		{ return Vec128_S32(_mm_srli_epi32(v, count)); }

#ifdef DO_BUILD_SSE4
	template<int mask>
	Vec128_S32 blend(Vec128_S32r b) const
	{
		return Vec128_S32(_mm_castps_si128(_mm_blend_ps(_mm_castsi128_ps(v), _mm_castsi128_ps(b), mask)));
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

	Vec128_U8() {}
	explicit Vec128_U8(Vec128 x) : v(x) {}
	explicit Vec128_U8(S8 x) : v(_mm_set1_epi8(x)) {}
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
	Vec128_U8 cmp_gt(Vec128_U8r b) const			{ return Vec128_U8(_mm_cmpgt_epi8(v, b)); }
	Vec128_U8 max(Vec128_U8r b) const				{ return Vec128_U8(_mm_max_epu8(v, b)); }
	Vec128_U8 min(Vec128_U8r b) const				{ return Vec128_U8(_mm_min_epu8(v, b)); }
#ifdef DO_BUILD_SSSE3
	Vec128_U8 sign(Vec128_U8r b) const				{ return Vec128_U8(_mm_sign_epi8(v, b)); }
#endif
	Vec128_U8 sub(Vec128_U8r b) const				{ return Vec128_U8(_mm_sub_epi8(v, b)); }
	Vec128_U8 sub_sat(Vec128_U8r b) const			{ return Vec128_U8(_mm_subs_epu8(v, b)); }
	Vec128_U8 unpack_hi(Vec128_U8r b) const			{ return Vec128_U8(_mm_unpackhi_epi8(v, b)); }
	Vec128_U8 unpack_lo(Vec128_U8r b) const			{ return Vec128_U8(_mm_unpacklo_epi8(v, b)); }

	Vec128_U8 andnot(Vec128r b) const				{ return Vec128_U8(_mm_andnot_si128(b, v)); } // a & ~b (mm_andnot is ~a & b)

#ifdef DO_BUILD_SSE4
	Vec128_U8 blend(Vec128_U8r b, Vec128r mask) const
	{
		return Vec128_U8(_mm_blendv_epi8(v, b, mask));
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

#ifdef DO_BUILD_SSE4
	template<int mask>
	Vec128_U16 blend(Vec128_U16r b) const			{ return Vec128_U16(_mm_blend_epi16(v, b, mask)); }
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
	explicit Vec128_U32(S32 x) : v(_mm_set1_epi32(x)) {}
	Vec128_U32(S32 s0, S32 s1, S32 s2, S32 s3)
		: v(_mm_setr_epi32(s0, s1, s2, s3)) {}

	operator __m128i() const						{ return v; }

	// initialization and conversions
	static Vec128_U32 zero()						{ return Vec128_U32(_mm_setzero_si128()); }
	static Vec128_U32 loadu(const void *ptr)		{ return Vec128_U32(load128u(ptr)); }
	static Vec128_U32 loadu_lo32(const void *ptr)	{ return Vec128_U32(load32u(ptr)); }

	void storeu(void *ptr) const 					{ store128u(ptr, v); }
	void storeu_lo32(void *ptr) const 				{ store32u(ptr, v); }

#ifdef DO_BUILD_SSE4
	template<int index> U32 extract() const			{ return _mm_extract_epi8(v, index); }
	template<int index> void insert(U32 x)			{ v = _mm_insert_epi8(v, x, index); }
#endif

	// cast
	VecF32x4   f32() const;
	Vec128_S8  s8() const;
	Vec128_S16 s16() const;
	Vec128_S32 s32() const;
	Vec128_U8  u8() const;
	Vec128_U16 u16() const;

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
	template<int mask>
	Vec128_U32 blend(Vec128_U32r b) const
	{
		return Vec128_U32(_mm_castps_si128(_mm_blend_ps(_mm_castsi128_ps(v), _mm_castsi128_ps(b), mask)));
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

	Vec128_S32 to_int32_trunc()	const				{ return Vec128_S32(_mm_cvttps_epi32(v)); }
	Vec128_S32 to_int32_round()	const				{ return Vec128_S32(_mm_cvtps_epi32(v)); }
	float scalar_x() const							{ float tmp; _mm_store_ss(&tmp, v); return tmp; }
	void storeu(float *ptr)	const					{ _mm_storeu_ps(ptr, v); } // unaligned
	void storea(float *ptr)	const					{ _mm_store_ps(ptr, v); } // 16-byte aligned

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
	VecF32x4 sum_across_inner_outer() const			{ VecF32x4 t = _mm_add_ps(*this, yxwz()); return _mm_add_ps(t, t.zwxy()); } // add inner than outer pairs, opposite to our default (gives different results for float)
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

template<int x,int y,int z,int w>
static inline VecF32x4 shuffle_two32(VecF32x4r a, VecF32x4r b)	{ return _mm_shuffle_ps(a, b, _MM_SHUFFLE(w,z,y,x)); }

#endif

OODLE_NS_END

