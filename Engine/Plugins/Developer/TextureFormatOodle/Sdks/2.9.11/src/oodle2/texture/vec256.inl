// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "vec128.inl"

OODLE_NS_START

#ifdef DO_BUILD_AVX2

typedef __m256i Vec256;
typedef __m256i const & Vec256r;

static inline Vec256 load256a(const void *ptr)		{ return _mm256_load_si256((const __m256i *)ptr); }
static inline Vec256 load256u(const void *ptr)		{ return _mm256_loadu_si256((const __m256i *)ptr); }
static inline void store256a(void *ptr, Vec256r x)	{ _mm256_store_si256((__m256i *)ptr, x); }
static inline void store256u(void *ptr, Vec256r x)	{ _mm256_storeu_si256((__m256i *)ptr, x); }

static inline Vec128 lo_half(Vec256r x)				{ return _mm256_castsi256_si128(x); }
static inline Vec128 hi_half(Vec256r x)				{ return _mm256_extracti128_si256(x, 1); }
static inline Vec256 combine(Vec128r a, Vec128r b)	{ return _mm256_inserti128_si256(_mm256_castsi128_si256(a), b, 1); }
static inline Vec256 broadcast128_256(Vec128r x)	{ return _mm256_broadcastsi128_si256(x); }

static inline VecF32x4 lo_half(__m256 x)			{ return VecF32x4(_mm256_castps256_ps128(x)); }
static inline VecF32x4 hi_half(__m256 x)			{ return VecF32x4(_mm256_extractf128_ps(x, 1)); }

template<int x,int y,int z,int w>
static inline Vec256 shuffle32in128(Vec256r v)		{ return _mm256_shuffle_epi32(v, _MM_SHUFFLE(w,z,y,x)); }

template<int x,int y,int z,int w>
static inline Vec256 shuffle64(Vec256r v)			{ return _mm256_permute4x64_epi64(v, _MM_SHUFFLE(w,z,y,x)); }

static inline Vec256 vec_packed_add(Vec256r a, Vec256r b, Vec256r mask_nonmsb)
{
	// sum low bits directly
	Vec256 low = _mm256_add_epi64(_mm256_and_si256(a, mask_nonmsb), _mm256_and_si256(b, mask_nonmsb));
	// carryless sum (=XOR) in MSBs
	return _mm256_xor_si256(low, _mm256_andnot_si256(mask_nonmsb, _mm256_xor_si256(a, b)));
}

static inline Vec256 zext8to16_lo_in128(Vec256r x)			{ return _mm256_unpacklo_epi8(x, _mm256_setzero_si256()); }
static inline Vec256 zext8to16_hi_in128(Vec256r x)			{ return _mm256_unpackhi_epi8(x, _mm256_setzero_si256()); }

static inline Vec256 blend32(Vec256r a, Vec256r b, Vec256 mask )	{ return _mm256_castps_si256(_mm256_blendv_ps(_mm256_castsi256_ps(a), _mm256_castsi256_ps(b), _mm256_castsi256_ps(mask))); }

static inline S32 reduce_add_s32(Vec256r x)				{ return reduce_add_s32(_mm_add_epi32(lo_half(x), hi_half(x))); }

struct Vec256_F32;
struct Vec256_S8;
struct Vec256_S16;
struct Vec256_S32;
struct Vec256_U8;

typedef const Vec256_F32 & Vec256_F32r;
typedef const Vec256_S8  & Vec256_S8r;
typedef const Vec256_S16 & Vec256_S16r;
typedef const Vec256_S32 & Vec256_S32r;
typedef const Vec256_U8  & Vec256_U8r;

struct Vec256_F32
{
	__m256 v;

	Vec256_F32() {}
	explicit Vec256_F32(__m256 x) : v(x) {}
	explicit Vec256_F32(F32 x) : v(_mm256_set1_ps(x)) {}
	Vec256_F32(F32 e0, F32 e1, F32 e2, F32 e3, F32 e4, F32 e5, F32 e6, F32 e7)
		: v(_mm256_setr_ps(e0, e1, e2, e3, e4, e5, e6, e7)) {}

	operator __m256() const { return v; }

	static Vec256_F32 broadcast(VecF32x4 x)			{ return Vec256_F32(_mm256_permute2f128_ps(_mm256_castps128_ps256(x), _mm256_castps128_ps256(x), 0x00)); }
	static Vec256_F32 combinef(VecF32x4 a, VecF32x4 b) { return Vec256_F32(_mm256_permute2f128_ps(_mm256_castps128_ps256(a), _mm256_castps128_ps256(b), 0x20)); }
	static Vec256_F32 zero()						{ return Vec256_F32(_mm256_setzero_ps()); }
	static Vec256_F32 from_int32(Vec256 x)			{ return Vec256_F32(_mm256_cvtepi32_ps(x)); }
	static Vec256_F32 loada(const F32 *ptr)			{ return Vec256_F32(_mm256_load_ps(ptr)); } // 32-byte aligned
	static Vec256_F32 loadu(const F32 *ptr)			{ return Vec256_F32(_mm256_loadu_ps(ptr)); }

	void storea(F32 *ptr) const						{ return _mm256_store_ps(ptr, v); } // 32-byte aligned
	void storeu(F32 *ptr) const						{ return _mm256_storeu_ps(ptr, v); }

	static Vec256_F32 repeat4(F32 e0, F32 e1, F32 e2, F32 e3)
	{
		return Vec256_F32(e0, e1, e2, e3, e0, e1,e2, e3);
	}

	// conversions
	Vec256_S32 to_s32_round() const;
	Vec256_S32 to_s32_trunc() const;

	// unary operations
	Vec256_F32 operator -() const					{ return Vec256_F32(_mm256_xor_ps(v, _mm256_set1_ps(-0.0f))); }
	Vec256_F32 abs() const							{ return Vec256_F32(_mm256_andnot_ps(_mm256_set1_ps(-0.0f), v)); }
	Vec256_F32 sqrt() const							{ return Vec256_F32(_mm256_sqrt_ps(v)); }

	Vec256_F32 operator +(Vec256_F32 b) const		{ return Vec256_F32(_mm256_add_ps(v, b)); }
	Vec256_F32 operator -(Vec256_F32 b) const		{ return Vec256_F32(_mm256_sub_ps(v, b)); }
	Vec256_F32 operator *(Vec256_F32 b) const		{ return Vec256_F32(_mm256_mul_ps(v, b)); }
	Vec256_F32 operator /(Vec256_F32 b) const		{ return Vec256_F32(_mm256_div_ps(v, b)); }

	Vec256_F32 &operator +=(Vec256_F32 b)			{ v = _mm256_add_ps(v, b); return *this; }
	Vec256_F32 &operator -=(Vec256_F32 b)			{ v = _mm256_sub_ps(v, b); return *this; }
	Vec256_F32 &operator *=(Vec256_F32 b)			{ v = _mm256_mul_ps(v, b); return *this; }
	Vec256_F32 &operator /=(Vec256_F32 b)			{ v = _mm256_div_ps(v, b); return *this; }

	Vec256_F32 operator &(Vec256_F32 b) const		{ return Vec256_F32(_mm256_and_ps(v, b.v)); }
	Vec256_F32 operator |(Vec256_F32 b) const		{ return Vec256_F32(_mm256_or_ps(v, b.v)); }
	Vec256_F32 operator ^(Vec256_F32 b) const		{ return Vec256_F32(_mm256_xor_ps(v, b.v)); }
	Vec256_F32 andnot(Vec256_F32 b) const			{ return Vec256_F32(_mm256_andnot_ps(b, v)); } // a & ~b (mm_andnot is ~a & b)

	Vec256_F32 &operator &=(Vec256_F32 b)			{ v = _mm256_and_ps(v, b); return *this; }
	Vec256_F32 &operator |=(Vec256_F32 b)			{ v = _mm256_or_ps(v, b); return *this; }
	Vec256_F32 &operator ^=(Vec256_F32 b)			{ v = _mm256_xor_ps(v, b); return *this; }

	Vec256_F32 max(Vec256_F32r b) const				{ return Vec256_F32(_mm256_max_ps(v, b)); }
	Vec256_F32 min(Vec256_F32r b) const				{ return Vec256_F32(_mm256_min_ps(v, b)); }

	// comparisons are not as operator overloads because they return a mask vector, not a bool
	Vec256_F32 cmp_gt(Vec256_F32 b) const			{ return Vec256_F32(_mm256_cmp_ps(b, v, 1)); } // <
	Vec256_F32 cmp_ge(Vec256_F32 b) const			{ return Vec256_F32(_mm256_cmp_ps(b, v, 2)); } // <=
	Vec256_F32 cmp_lt(Vec256_F32 b) const			{ return Vec256_F32(_mm256_cmp_ps(v, b, 1)); } // <
	Vec256_F32 cmp_le(Vec256_F32 b) const			{ return Vec256_F32(_mm256_cmp_ps(v, b, 2)); } // <=
	Vec256_F32 cmp_eq(Vec256_F32 b) const			{ return Vec256_F32(_mm256_cmp_ps(v, b, 0)); } // ==
	Vec256_F32 cmp_ngt(Vec256_F32 b) const			{ return Vec256_F32(_mm256_cmp_ps(b, v, 5)); } // ! <
	Vec256_F32 cmp_nge(Vec256_F32 b) const			{ return Vec256_F32(_mm256_cmp_ps(b, v, 6)); } // ! <=
	Vec256_F32 cmp_nlt(Vec256_F32 b) const			{ return Vec256_F32(_mm256_cmp_ps(v, b, 5)); } // ! <
	Vec256_F32 cmp_nle(Vec256_F32 b) const			{ return Vec256_F32(_mm256_cmp_ps(v, b, 6)); } // ! <=
	Vec256_F32 cmp_neq(Vec256_F32 b) const			{ return Vec256_F32(_mm256_cmp_ps(v, b, 4)); } // ! ==

	// shuffles
	template<int x,int y,int z,int w>
	Vec256_F32 shuf_in128() const						{ return Vec256_F32(_mm256_shuffle_ps(v, v, _MM_SHUFFLE(w,z,y,x))); }

	// cond.select(a, b) is the equivalent of cond ? a : b
	// cond (*this) must be a truth value (i.e. -1 or 0)
	Vec256_F32 select(Vec256_F32 a, Vec256_F32 b) const { return Vec256_F32(_mm256_blendv_ps(b, a, v)); }
};

#define RR_DECL_INT_COMMON(typ,suffixi) \
	static typ zero()								{ return typ(_mm256_setzero_si256()); } \
	/* loads */ \
	static typ loada(const void * ptr)				{ return typ(_mm256_load_si256((const __m256i *)ptr)); } \
	static typ loadu(const void * ptr)				{ return typ(_mm256_loadu_si256((const __m256i *)ptr)); } \
	static typ loadu_lo128(const void * ptr)		{ return typ(_mm256_castsi128_si256(_mm_loadu_si128((const __m128i *)ptr))); } \
	static typ loadu_dup128(const void * ptr)		{ return typ(_mm256_broadcastsi128_si256(_mm_loadu_si128((const __m128i *)ptr))); } \
	static typ loadu_lo64(const void * ptr)			{ return typ(_mm256_castsi128_si256(_mm_loadl_epi64((const __m128i *)ptr))); } \
	static typ loadu_dup64(const void * ptr)		{ return typ(_mm256_broadcastq_epi64(_mm_loadl_epi64((const __m128i *)ptr))); } \
	static typ loadu_lo32(const void * ptr)			{ return typ(_mm256_castsi128_si256(_mm_cvtsi32_si128(RR_GET32_NATIVE_UNALIGNED(ptr)))); } \
	static typ loadu_dup32(const void * ptr)		{ return typ(_mm256_set1_epi32(RR_GET32_NATIVE_UNALIGNED(ptr))); } \
	/* stores */ \
	void storea(void * ptr)	const					{ _mm256_store_si256((__m256i *)ptr, v); } \
	void storeu(void * ptr)	const					{ _mm256_storeu_si256((__m256i *)ptr, v); } \
	void storeu_lo128(void * ptr) const				{ _mm_storeu_si128((__m128i *)ptr, _mm256_castsi256_si128(v)); } \
	void storeu_lo64(void * ptr) const				{ _mm_storel_epi64((__m128i *)ptr, _mm256_castsi256_si128(v)); } \
	void storeu_lo32(void * ptr) const				{ RR_PUT32_NATIVE_UNALIGNED(ptr, _mm256_cvtsi256_si32(v)); } \
	/* conversions and bit casts */ \
	operator __m256i() const						{ return v; } \
	Vec256_S8 s8() const; \
	Vec256_U8 u8() const; \
	Vec256_S16 s16() const; \
	Vec256_S32 s32() const; \
	Vec256_F32 f32() const; \
	/* unpacks and logic */ \
	/* NOTE the unpacks are the weird AVX/AVX2 kind (within 128-bit lanes) */ \
	typ dup_lo() const								{ return typ(shuffle64<0,1,0,1>(v)); } \
	typ unpack_lo(typ b) const						{ return typ(_mm256_unpacklo_##suffixi(v, b.v)); } \
	typ unpack_hi(typ b) const						{ return typ(_mm256_unpackhi_##suffixi(v, b.v)); } \
	typ operator &(typ b) const						{ return typ(_mm256_and_si256(v, b.v)); } \
	typ operator |(typ b) const						{ return typ(_mm256_or_si256(v, b.v)); } \
	typ operator ^(typ b) const						{ return typ(_mm256_xor_si256(v, b.v)); } \
	typ andnot(typ b) const							{ return typ(_mm256_andnot_si256(b.v, v)); } /* a & ~b (mm_andnot is ~a & n) */ \
	typ& operator &=(typ b)							{ v = _mm256_and_si256(v, b.v); return *this; } \
	typ& operator |=(typ b)							{ v = _mm256_or_si256(v, b.v); return *this; } \
	typ& operator ^=(typ b)							{ v = _mm256_xor_si256(v, b.v); return *this; } \
	/* arithmetic */ \
	typ operator +(typ b) const						{ return typ(_mm256_add_##suffixi(v, b.v)); } \
	typ operator -(typ b) const						{ return typ(_mm256_sub_##suffixi(v, b.v)); } \
	typ& operator +=(typ b)							{ v = _mm256_add_##suffixi(v, b.v); return *this; } \
	typ& operator -=(typ b)							{ v = _mm256_sub_##suffixi(v, b.v); return *this; } \
	/* cond.select(a, b) is the equivalent of cond ? a : b */ \
	/* cond (*this) must be a truth value (i.e. ~0 or 0) of the given type */ \
	typ select(typ a, typ b) const					{ return typ(_mm256_blendv_epi8(b, a, v)); } \
	/* end */

#define RR_IMPL_INT_COMMON(typ) \
	inline Vec256_S8 typ::s8() const				{ return Vec256_S8(v); } \
	inline Vec256_U8 typ::u8() const				{ return Vec256_U8(v); } \
	inline Vec256_S16 typ::s16() const				{ return Vec256_S16(v); } \
	inline Vec256_S32 typ::s32() const				{ return Vec256_S32(v); } \
	inline Vec256_F32 typ::f32() const				{ return Vec256_F32(_mm256_castsi256_ps(v)); } \
	/* end */

#define RR_IMPL_INT_SAT(typ,suffix) \
	inline typ add_sat(typ b) const					{ return typ(_mm256_adds_##suffix(v, b.v)); } \
	inline typ sub_sat(typ b) const					{ return typ(_mm256_adds_##suffix(v, b.v)); } \
	/* end */

struct Vec256_S8
{
	Vec256 v;

	Vec256_S8() {}
	explicit Vec256_S8(Vec256 x) : v(x) {}
	explicit Vec256_S8(S8 e0) : v(_mm256_set1_epi8(e0)) {}
	Vec256_S8(S8 e0, S8 e1, S8 e2, S8 e3, S8 e4, S8 e5, S8 e6, S8 e7,
			  S8 e8, S8 e9, S8 e10, S8 e11, S8 e12, S8 e13, S8 e14, S8 e15,
			  S8 e16, S8 e17, S8 e18, S8 e19, S8 e20, S8 e21, S8 e22, S8 e23,
			  S8 e24, S8 e25, S8 e26, S8 e27, S8 e28, S8 e29, S8 e30, S8 e31)
		: v(_mm256_setr_epi8(e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15,
			e16, e17, e18, e19, e20, e21, e22, e23, e24, e25, e26, e27, e28, e29, e30, e31)) {}

	RR_DECL_INT_COMMON(Vec256_S8,epi8)

	// saturating arithmetic
	RR_IMPL_INT_SAT(Vec256_S8,epi8)

	static Vec256_S8 repeat4(S8 s0, S8 s1, S8 s2, S8 s3) // xyzw xyzw xyzw xyzw xyzw ...
	{
		return Vec256_S8(s0, s1, s2, s3, s0, s1, s2, s3, s0, s1, s2, s3, s0, s1, s2, s3,
						 s0, s1, s2, s3, s0, s1, s2, s3, s0, s1, s2, s3, s0, s1, s2, s3);
	}

	static Vec256_S8 splat2(S8 s0, S8 s1, S8 s2, S8 s3, S8 s4, S8 s5, S8 s6, S8 s7,
							S8 s8, S8 s9, S8 s10, S8 s11, S8 s12, S8 s13, S8 s14, S8 s15) // xx yy zz ww ....
	{
		return Vec256_S8(s0,s0, s1,s1, s2,s2, s3,s3, s4,s4, s5,s5, s6,s6, s7,s7,
						 s8,s8, s9,s9, s10,s10, s11,s11, s12,s12, s13,s13, s14,s14, s15,s15);
	}

	static Vec256_S8 splat4(S8 s0, S8 s1, S8 s2, S8 s3, S8 s4, S8 s5, S8 s6, S8 s7) // xxxx yyyy zzzz wwww ....
	{
		return Vec256_S8(s0,s0,s0,s0, s1,s1,s1,s1, s2,s2,s2,s2, s3,s3,s3,s3,
						 s4,s4,s4,s4, s5,s5,s5,s5, s6,s6,s6,s6, s7,s7,s7,s7);
	}

	// conversion
	Vec256_S16 to_s16_lo() const;
	Vec256_S32 to_s32_lo() const;

	template<int index> int extract() const			{ return _mm256_extract_epi8(v, index); }
	template<int index> void insert(S8 x) 			{ v = _mm256_insert_epi8(v, x, index); }

	Vec256_S8 abs() const							{ return Vec256_S8(_mm256_abs_epi8(v)); }
	int movemask() const 							{ return _mm256_movemask_epi8(v); }

	Vec256_S8 cmp_eq(Vec256_S8 b) const				{ return Vec256_S8(_mm256_cmpeq_epi8(v, b)); }
	Vec256_S8 cmp_gt(Vec256_S8 b) const				{ return Vec256_S8(_mm256_cmpgt_epi8(v, b)); }
	Vec256_S8 max(Vec256_S8 b) const				{ return Vec256_S8(_mm256_max_epi8(v, b)); }
	Vec256_S8 min(Vec256_S8 b) const				{ return Vec256_S8(_mm256_min_epi8(v, b)); }
	Vec256_S8 sign(Vec256_S8 b) const				{ return Vec256_S8(_mm256_sign_epi8(v, b)); }

	Vec256_S8 shuf_in128(Vec256 m) const			{ return Vec256_S8(_mm256_shuffle_epi8(v, m)); }
};

struct Vec256_S16
{
	Vec256 v;

	Vec256_S16() {}
	explicit Vec256_S16(Vec256 x) : v(x) {}
	explicit Vec256_S16(S16 x) : v(_mm256_set1_epi16(x)) {}
	Vec256_S16(S16 e0, S16 e1, S16 e2, S16 e3, S16 e4, S16 e5, S16 e6, S16 e7, S16 e8, S16 e9, S16 e10, S16 e11, S16 e12, S16 e13, S16 e14, S16 e15)
		: v(_mm256_setr_epi16(e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15)) {}

	RR_DECL_INT_COMMON(Vec256_S16,epi16)

	// saturating arithmetic
	RR_IMPL_INT_SAT(Vec256_S16,epi16)

	// xyzw xyzw ...
	static Vec256_S16 repeat4(S16 s0, S16 s1, S16 s2, S16 s3) { return Vec256_S16(s0, s1, s2, s3, s0, s1, s2, s3, s0, s1, s2, s3, s0, s1, s2, s3); }
	// xxxx xxxx yyyy yyyy
	static Vec256_S16 splat8(S16 s0, S16 s1)		{ return Vec256_S16(s0,s0,s0,s0,s0,s0,s0,s0, s1,s1,s1,s1,s1,s1,s1,s1); }

	// conversion
	Vec256_S32 to_s32_lo() const;
	Vec256_S8  to_s8_sat(Vec256_S16r b) const;
	Vec256_U8  to_u8_sat(Vec256_S16r b) const;

	template<int index> int extract() const			{ return _mm256_extract_epi16(v, index); }
	template<int index> void insert(S16 x) 			{ v = _mm256_insert_epi16(v, x, index); }

	Vec256_S16 abs() const							{ return Vec256_S16(_mm256_abs_epi16(v)); }
	Vec256_S16 sign(Vec256_S16 b) const				{ return Vec256_S16(_mm256_sign_epi16(v, b)); }

	Vec256_S16 cmp_eq(Vec256_S16 b) const			{ return Vec256_S16(_mm256_cmpeq_epi16(v, b)); }
	Vec256_S16 cmp_gt(Vec256_S16 b) const			{ return Vec256_S16(_mm256_cmpgt_epi16(v, b)); }
	Vec256_S16 max(Vec256_S16 b) const				{ return Vec256_S16(_mm256_max_epi16(v, b)); }
	Vec256_S16 min(Vec256_S16 b) const				{ return Vec256_S16(_mm256_min_epi16(v, b)); }

	Vec256_S16 mulhi(Vec256_S16 b) const			{ return Vec256_S16(_mm256_mulhi_epi16(v, b)); }
	Vec256_S16 mulhrs(Vec256_S16 b) const 			{ return Vec256_S16(_mm256_mulhrs_epi16(v, b)); }
	Vec256_S16 mullo(Vec256_S16 b) const 			{ return Vec256_S16(_mm256_mullo_epi16(v, b)); }

	template<int count>	Vec256_S16 shl() const		{ return Vec256_S16(_mm256_slli_epi16(v, count)); }
	template<int count>	Vec256_S16 sra() const		{ return Vec256_S16(_mm256_srai_epi16(v, count)); }
	template<int count> Vec256_S16 srl() const		{ return Vec256_S16(_mm256_srli_epi16(v, count)); }

	Vec256_S32 madd(Vec256_S16r b) const;

	Vec256_S16 shuf_in128(Vec256r m) const			{ return Vec256_S16(_mm256_shuffle_epi8(v, m)); }

	Vec256_S16 operator *(Vec256_S16 b) const		{ return mullo(b); }
	Vec256_S16 &operator *=(Vec256_S16r b)			{ v = mullo(b); return *this; }
};

struct Vec256_S32
{
	Vec256 v;

	Vec256_S32() {}
	explicit Vec256_S32(Vec256 x) : v(x) {}
	explicit Vec256_S32(S32 x) : v(_mm256_set1_epi32(x)) {}
	Vec256_S32(S32 e0, S32 e1, S32 e2, S32 e3, S32 e4, S32 e5, S32 e6, S32 e7)
		: v(_mm256_setr_epi32(e0, e1, e2, e3, e4, e5, e6, e7)) {}

	RR_DECL_INT_COMMON(Vec256_S32,epi32)

	// xyzw xyzw
	static Vec256_S32 repeat4(S32 s0, S32 s1, S32 s2, S32 s3) { return Vec256_S32(s0, s1, s2, s3, s0, s1, s2, s3); }
	// xx yy zz ww ....
	static Vec256_S32 splat2(S32 s0, S32 s1, S32 s2, S32 s3) { return Vec256_S32(s0,s0, s1,s1, s2,s2, s3,s3); }
	// xxxx yyyy
	static Vec256_S32 splat4(S32 s0, S32 s1) 		{ return Vec256_S32(s0,s0,s0,s0, s1,s1,s1,s1); }

	// conversion
	Vec256_F32 to_f32() const;
	Vec256_S16 to_s16_sat(Vec256_S32r b) const;

	template<int index> int extract() const			{ return _mm256_extract_epi32(v, index); }
	template<int index> void insert(S32 x) 			{ v = _mm256_insert_epi32(v, x, index); }

	U32 movemask() const 							{ return _mm256_movemask_ps(_mm256_castsi256_ps(v)); }

	Vec256_S32 abs() const							{ return Vec256_S32(_mm256_abs_epi32(v)); }
	Vec256_S32 sign(Vec256_S32 b) const				{ return Vec256_S32(_mm256_sign_epi32(v, b)); }

	Vec256_S32 cmp_eq(Vec256_S32r b) const			{ return Vec256_S32(_mm256_cmpeq_epi32(v, b)); }
	Vec256_S32 cmp_gt(Vec256_S32r b) const			{ return Vec256_S32(_mm256_cmpgt_epi32(v, b)); }
	Vec256_S32 max(Vec256_S32r b) const				{ return Vec256_S32(_mm256_max_epi32(v, b)); }
	Vec256_S32 min(Vec256_S32r b) const				{ return Vec256_S32(_mm256_min_epi32(v, b)); }
	Vec256_S32 mullo(Vec256_S32r b) const 			{ return Vec256_S32(_mm256_mullo_epi32(v, b)); }

	Vec256_S32 shl(Vec256_S32 count) const			{ return Vec256_S32(_mm256_sllv_epi32(v, count)); }
	Vec256_S32 sra(Vec256_S32 count) const			{ return Vec256_S32(_mm256_srav_epi32(v, count)); }
	Vec256_S32 srl(Vec256_S32 count) const			{ return Vec256_S32(_mm256_srlv_epi32(v, count)); }

	template<int count> Vec256_S32 shl() const		{ return Vec256_S32(_mm256_slli_epi32(v, count)); }
	template<int count> Vec256_S32 sra() const		{ return Vec256_S32(_mm256_srai_epi32(v, count)); }
	template<int count> Vec256_S32 srl() const		{ return Vec256_S32(_mm256_srli_epi32(v, count)); }

	//Vec256_S8 shuf_in128(Vec256r m) const			{ return Vec256_S8(_mm256_shuffle_epi8(v, m)); }
	Vec256_S32 operator *(Vec256_S32 b) const		{ return mullo(b); }
	Vec256_S32 &operator *=(Vec256_S32 b)			{ *this = mullo(b); return *this; }

	Vec256_S32 operator <<(Vec256_S32 b)			{ return shl(b); }
	Vec256_S32 &operator <<=(Vec256_S32 b)			{ v = shl(b); return *this; }
	Vec256_S32 operator >>(Vec256_S32 b)			{ return sra(b); }
	Vec256_S32 &operator >>=(Vec256_S32 b)			{ v = sra(b); return *this; }

	Vec256_S32 permute(Vec256_S32r b) const			{ return Vec256_S32(_mm256_permutevar8x32_epi32(v, b)); }

	template<int x,int y,int z,int w>
	Vec256_S32 shuffle_in128() const				{ return Vec256_S32(shuffle32in128<x,y,z,w>(v)); }

	Vec256_S32 yxwz_in128() const					{ return shuffle_in128<1,0,3,2>(); }
};

struct Vec256_U8
{
	Vec256 v;

	Vec256_U8() {}
	explicit Vec256_U8(Vec256 x) : v(x) {}
	explicit Vec256_U8(U8 e0) : v(_mm256_set1_epi8(e0)) {}
	Vec256_U8(U8 e0, U8 e1, U8 e2, U8 e3, U8 e4, U8 e5, U8 e6, U8 e7,
			  U8 e8, U8 e9, U8 e10, U8 e11, U8 e12, U8 e13, U8 e14, U8 e15,
			  U8 e16, U8 e17, U8 e18, U8 e19, U8 e20, U8 e21, U8 e22, U8 e23,
			  U8 e24, U8 e25, U8 e26, U8 e27, U8 e28, U8 e29, U8 e30, U8 e31)
		: v(_mm256_setr_epi8(e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15,
			e16, e17, e18, e19, e20, e21, e22, e23, e24, e25, e26, e27, e28, e29, e30, e31)) {}

	RR_DECL_INT_COMMON(Vec256_U8,epi8)

	// saturating arithmetic
	RR_IMPL_INT_SAT(Vec256_U8,epu8)

	// conversion
	Vec256_S16 to_s16_lo() const;
	Vec256_S32 to_s32_lo() const;

	template<int index> int extract() const			{ return _mm256_extract_epi8(v, index); }
	template<int index> void insert(U8 x) 			{ v = _mm256_insert_epi8(v, x, index); }

	U32 movemask() const 							{ return _mm256_movemask_epi8(v); }

	Vec256_U8 add_sat(Vec256_U8r b) const			{ return Vec256_U8(_mm256_adds_epu8(v, b)); }
	Vec256_U8 sub_sat(Vec256_U8r b) const			{ return Vec256_U8(_mm256_subs_epu8(v, b)); }
	Vec256_U8 cmp_eq(Vec256_U8r b) const			{ return Vec256_U8(_mm256_cmpeq_epi8(v, b)); }
	Vec256_U8 cmp_gt(Vec256_U8r b) const			{ return Vec256_U8(_mm256_cmpgt_epi8(v, b)); }
	Vec256_U8 max(Vec256_U8r b) const				{ return Vec256_U8(_mm256_max_epu8(v, b)); }
	Vec256_U8 min(Vec256_U8r b) const				{ return Vec256_U8(_mm256_min_epu8(v, b)); }

	Vec256_U8 shuf_in128(Vec256r m) const			{ return Vec256_U8(_mm256_shuffle_epi8(v, m)); }

	Vec256_S16 madd_sat(Vec256_S8r b) const;
	Vec256_S32 sad(Vec256_U8r b) const;
};

RR_IMPL_INT_COMMON(Vec256_S8)
RR_IMPL_INT_COMMON(Vec256_U8)
RR_IMPL_INT_COMMON(Vec256_S16)
RR_IMPL_INT_COMMON(Vec256_S32)

// Conversions
inline Vec256_S32 Vec256_F32::to_s32_round() const	{ return Vec256_S32(_mm256_cvtps_epi32(v)); }
inline Vec256_S32 Vec256_F32::to_s32_trunc() const	{ return Vec256_S32(_mm256_cvttps_epi32(v)); }

inline Vec256_S16 Vec256_S8::to_s16_lo() const		{ return Vec256_S16(_mm256_cvtepi8_epi16(_mm256_castsi256_si128(v))); }
inline Vec256_S32 Vec256_S8::to_s32_lo() const		{ return Vec256_S32(_mm256_cvtepi8_epi32(_mm256_castsi256_si128(v))); }

inline Vec256_S32 Vec256_S16::to_s32_lo() const		{ return Vec256_S32(_mm256_cvtepi16_epi32(_mm256_castsi256_si128(v))); }

inline Vec256_F32 Vec256_S32::to_f32() const		{ return Vec256_F32(_mm256_cvtepi32_ps(v)); }

inline Vec256_S16 Vec256_U8::to_s16_lo() const		{ return Vec256_S16(_mm256_cvtepu8_epi16(_mm256_castsi256_si128(v))); }
inline Vec256_S32 Vec256_U8::to_s32_lo() const		{ return Vec256_S32(_mm256_cvtepu8_epi32(_mm256_castsi256_si128(v))); }

inline Vec256_S8  Vec256_S16::to_s8_sat(Vec256_S16r b) const	{ return Vec256_S8(_mm256_packs_epi16(v, b)); }
inline Vec256_U8  Vec256_S16::to_u8_sat(Vec256_S16r b) const	{ return Vec256_U8(_mm256_packus_epi16(v, b)); }
inline Vec256_S16 Vec256_S32::to_s16_sat(Vec256_S32r b) const	{ return Vec256_S16(_mm256_packs_epi32(v, b)); }

// Arithmetic operators
inline Vec256_S32 Vec256_S16::madd(Vec256_S16r b) const		{ return Vec256_S32(_mm256_madd_epi16(v, b)); }
inline Vec256_S16 Vec256_U8::madd_sat(Vec256_S8r b) const	{ return Vec256_S16(_mm256_maddubs_epi16(v, b)); }
inline Vec256_S32 Vec256_U8::sad(Vec256_U8r b) const		{ return Vec256_S32(_mm256_sad_epu8(v, b)); }

template<int x, int y, int z, int w>
static inline Vec256_S32 shuffle_two_in128(Vec256_S32r a, Vec256_S32r b)
{
	return Vec256_S32(_mm256_castps_si256(_mm256_shuffle_ps(_mm256_castsi256_ps(a), _mm256_castsi256_ps(b), _MM_SHUFFLE(w,z,y,x))));
}

#undef RR_DECL_INT_COMMON
#undef RR_IMPL_INT_COMMON
#undef RR_IMPL_INT_SAT

#endif

OODLE_NS_END
