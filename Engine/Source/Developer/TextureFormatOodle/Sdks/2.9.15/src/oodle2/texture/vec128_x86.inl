// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#ifndef OODLE_VEC128_IMPL_X86
#error DO NOT INCLUDE DIRECTLY - use vec128.inl!
#endif

OODLE_NS_START

typedef __m128i Vec128;
typedef __m128i const & Vec128r;

static inline Vec128 load32u(const void *ptr)			{ return _mm_cvtsi32_si128(RR_GET32_NATIVE_UNALIGNED(ptr)); }
static inline Vec128 load64u(const void *ptr)			{ return _mm_loadl_epi64((const __m128i *)ptr); }
static inline Vec128 load128u(const void *ptr)			{ return _mm_loadu_si128((const __m128i *)ptr); }
static inline Vec128 load128a(const void *ptr)			{ return _mm_load_si128((const __m128i *)ptr); }
static inline void store32u(void *ptr, Vec128 x)		{ RR_PUT32_NATIVE_UNALIGNED(ptr, _mm_cvtsi128_si32(x)); }
static inline void store64u(void *ptr, Vec128 x)		{ _mm_storel_epi64((__m128i *)ptr, x); }
static inline void store128u(void *ptr, Vec128 x)		{ _mm_storeu_si128((__m128i *)ptr, x); }
static inline void store128a(void *ptr, Vec128 x)		{ _mm_store_si128((__m128i *)ptr, x); }

static inline Vec128 vec_packed_add(Vec128 a, Vec128 b, Vec128 mask_nonmsb)
{
	// sum low bits directly
	Vec128 low = _mm_add_epi64(_mm_and_si128(a, mask_nonmsb), _mm_and_si128(b, mask_nonmsb));
	// carryless sum (=XOR) in MSBs
	return _mm_xor_si128(low, _mm_andnot_si128(mask_nonmsb, _mm_xor_si128(a, b)));
}

#ifdef DO_BUILD_SSE4
static inline Vec128 zext8to16_lo(Vec128 x)				{ return _mm_cvtepu8_epi16(x); }
static inline Vec128 sext8to16_lo(Vec128 x)				{ return _mm_cvtepi8_epi16(x); }
static inline Vec128 zext16to32_lo(Vec128 x)			{ return _mm_cvtepu16_epi32(x); }
static inline Vec128 sext16to32_lo(Vec128 x)			{ return _mm_cvtepi16_epi32(x); }

static inline Vec128 zext8to32_lo(Vec128 x)				{ return _mm_cvtepu8_epi32(x); }
static inline Vec128 sext8to32_lo(Vec128 x)				{ return _mm_cvtepi8_epi32(x); }
#else
static inline Vec128 zext8to16_lo(Vec128 x)				{ return _mm_unpacklo_epi8(x, _mm_setzero_si128()); }
static inline Vec128 sext8to16_lo(Vec128 x)				{ return _mm_srai_epi16(_mm_unpacklo_epi8(x, x), 8); }
static inline Vec128 zext16to32_lo(Vec128 x)			{ return _mm_unpacklo_epi16(x, _mm_setzero_si128()); }
static inline Vec128 sext16to32_lo(Vec128 x)			{ return _mm_srai_epi32(_mm_unpacklo_epi16(x, x), 16); }

static inline Vec128 zext8to32_lo(Vec128 x)				{ return zext16to32_lo(zext8to16_lo(x)); }
static inline Vec128 sext8to32_lo(Vec128 x)				{ Vec128 t = _mm_unpacklo_epi8(x, x); return _mm_srai_epi32(_mm_unpacklo_epi16(t, t), 24); }
#endif

static inline Vec128 zext8to16_hi(Vec128 x)				{ return _mm_unpackhi_epi8(x, _mm_setzero_si128()); }
static inline Vec128 sext8to16_hi(Vec128 x)				{ return _mm_srai_epi16(_mm_unpackhi_epi8(x, x), 8); }
static inline Vec128 zext16to32_hi(Vec128 x)			{ return _mm_unpackhi_epi16(x, _mm_setzero_si128()); }
static inline Vec128 sext16to32_hi(Vec128 x)			{ return _mm_srai_epi32(_mm_unpackhi_epi16(x, x), 16); }

template<int x,int y,int z,int w>
static inline Vec128 shuffle32(Vec128 v)				{ return _mm_shuffle_epi32(v, _MM_SHUFFLE(w,z,y,x)); }

template<int x,int y,int z,int w>
static inline Vec128 shuffle_two32(Vec128 a, Vec128 b)	{ return _mm_castps_si128(_mm_shuffle_ps(_mm_castsi128_ps(a), _mm_castsi128_ps(b), _MM_SHUFFLE(w,z,y,x))); }

// some reduction steps
static inline Vec128 reduce_add_s32_2away(Vec128 x)		{ Vec128 y = shuffle32<2,3,0,1>(x); return _mm_add_epi32(x, y); }
static inline Vec128 reduce_min_u8_8away(Vec128 x)		{ Vec128 y = shuffle32<2,3,0,1>(x); return _mm_min_epu8(x, y); }
static inline Vec128 reduce_max_u8_8away(Vec128 x)		{ Vec128 y = shuffle32<2,3,0,1>(x); return _mm_max_epu8(x, y); }
static inline Vec128 reduce_add_s32_1away(Vec128 x)		{ Vec128 y = shuffle32<1,0,3,2>(x); return _mm_add_epi32(x, y); }
static inline Vec128 reduce_min_u8_4away(Vec128 x)		{ Vec128 y = shuffle32<1,0,3,2>(x); return _mm_min_epu8(x, y); }
static inline Vec128 reduce_max_u8_4away(Vec128 x)		{ Vec128 y = shuffle32<1,0,3,2>(x); return _mm_max_epu8(x, y); }
#ifdef DO_BUILD_SSE4
static inline Vec128 reduce_min_u32_2away(Vec128 x)		{ Vec128 y = shuffle32<2,3,0,1>(x); return _mm_min_epu32(x, y); }
static inline Vec128 reduce_max_u32_2away(Vec128 x)		{ Vec128 y = shuffle32<2,3,0,1>(x); return _mm_max_epu32(x, y); }
static inline Vec128 reduce_min_u32_1away(Vec128 x)		{ Vec128 y = shuffle32<1,0,3,2>(x); return _mm_min_epu32(x, y); }
static inline Vec128 reduce_max_u32_1away(Vec128 x)		{ Vec128 y = shuffle32<1,0,3,2>(x); return _mm_max_epu32(x, y); }
#endif


static inline S32 reduce_add_s32(Vec128 x)				{ return _mm_cvtsi128_si32(reduce_add_s32_1away(reduce_add_s32_2away(x))); }
static inline U32 reduce_add_u32(Vec128 x)				{ return static_cast<U32>(reduce_add_s32(x)); }

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

#define RR_DECL_INT_COMMON(typ,suffixi) \
	static typ zero()								{ return typ(_mm_setzero_si128()); } \
	/* loads */ \
	static typ loada(const void * ptr)				{ return typ(_mm_load_si128((const __m128i *)ptr)); } \
	static typ loadu(const void * ptr)				{ return typ(_mm_loadu_si128((const __m128i *)ptr)); } \
	static typ loadu_lo64(const void * ptr)			{ return typ(_mm_loadl_epi64((const __m128i *)ptr)); } \
	static typ loadu_dup64(const void * ptr)		{ return typ(shuffle32<0,1,0,1>(loadu_lo64(ptr))); } \
	static typ loadu_lo32(const void * ptr)			{ return typ(_mm_cvtsi32_si128(RR_GET32_NATIVE_UNALIGNED(ptr))); } \
	static typ loadu_dup32(const void * ptr)		{ return typ(_mm_set1_epi32(RR_GET32_NATIVE_UNALIGNED(ptr))); } \
	/* stores */ \
	void storea(void * ptr)	const					{ _mm_store_si128((__m128i *)ptr, v); } \
	void storeu(void * ptr)	const					{ _mm_storeu_si128((__m128i *)ptr, v); } \
	void storeu_lo64(void * ptr) const				{ _mm_storel_epi64((__m128i *)ptr, v); } \
	void storeu_lo32(void * ptr) const				{ RR_PUT32_NATIVE_UNALIGNED(ptr, _mm_cvtsi128_si32(v)); } \
	/* conversions and bit casts */ \
	operator __m128i() const						{ return v; } \
	Vec128_S8 s8() const; \
	Vec128_U8 u8() const; \
	Vec128_S16 s16() const; \
	Vec128_U16 u16() const; \
	Vec128_S32 s32() const; \
	Vec128_U32 u32() const; \
	VecF32x4 f32() const; \
	/* unpacks and logic */ \
	typ unpack_lo(typ b) const						{ return typ(_mm_unpacklo_##suffixi(v, b.v)); } \
	typ unpack_hi(typ b) const						{ return typ(_mm_unpackhi_##suffixi(v, b.v)); } \
	typ operator &(typ b) const						{ return typ(_mm_and_si128(v, b.v)); } \
	typ operator |(typ b) const						{ return typ(_mm_or_si128(v, b.v)); } \
	typ operator ^(typ b) const						{ return typ(_mm_xor_si128(v, b.v)); } \
	typ andnot(typ b) const							{ return typ(_mm_andnot_si128(b.v, v)); } /* a & ~b (mm_andnot is ~a & n) */ \
	typ& operator &=(typ b)							{ v = _mm_and_si128(v, b.v); return *this; } \
	typ& operator |=(typ b)							{ v = _mm_or_si128(v, b.v); return *this; } \
	typ& operator ^=(typ b)							{ v = _mm_xor_si128(v, b.v); return *this; } \
	/* arithmetic */ \
	typ operator +(typ b) const						{ return typ(_mm_add_##suffixi(v, b.v)); } \
	typ operator -(typ b) const						{ return typ(_mm_sub_##suffixi(v, b.v)); } \
	typ& operator +=(typ b)							{ v = _mm_add_##suffixi(v, b.v); return *this; } \
	typ& operator -=(typ b)							{ v = _mm_sub_##suffixi(v, b.v); return *this; } \
	/* end */

#ifdef DO_BUILD_SSE4

#define RR_DECL_INT_SELECT(typ) \
	/* cond.select(a, b) is the equivalent of cond ? a : b */ \
	/* cond (*this) must be a truth value (i.e. ~0 or 0) of the given type */ \
	typ select(typ a, typ b) const					{ return typ(_mm_blendv_epi8(b, a, v)); } \
	/* end */

#else

#define RR_DECL_INT_SELECT(typ) \
	/* cond.select(a, b) is the equivalent of cond ? a : b */ \
	/* cond (*this) must be a truth value (i.e. ~0 or 0) of the given type */ \
	typ select(typ a, typ b) const					{ return typ(_mm_or_si128(_mm_and_si128(v, a), _mm_andnot_si128(v, b))); } \
	/* end */

#endif

#define RR_DECL_INT_SHUF(typ) \
	inline typ shuf(Vec128_S8 m) const; \
	inline typ shuf(Vec128_U8 m) const; \
	/* end */

#define RR_IMPL_INT_COMMON(typ) \
	inline Vec128_S8 typ::s8() const				{ return Vec128_S8(v); } \
	inline Vec128_U8 typ::u8() const				{ return Vec128_U8(v); } \
	inline Vec128_S16 typ::s16() const				{ return Vec128_S16(v); } \
	inline Vec128_U16 typ::u16() const				{ return Vec128_U16(v); } \
	inline Vec128_S32 typ::s32() const				{ return Vec128_S32(v); } \
	inline Vec128_U32 typ::u32() const				{ return Vec128_U32(v); } \
	inline VecF32x4 typ::f32() const				{ return VecF32x4(_mm_castsi128_ps(v)); } \
	/* end */

#define RR_IMPL_INT_SAT(typ,suffix) \
	inline typ add_sat(typ b) const					{ return typ(_mm_adds_##suffix(v, b.v)); } \
	inline typ sub_sat(typ b) const					{ return typ(_mm_subs_##suffix(v, b.v)); } \
	/* end */

#define RR_IMPL_INT_SHUF(typ) \
	inline typ typ::shuf(Vec128_S8 m) const			{ return typ(_mm_shuffle_epi8(v, m.v)); } \
	inline typ typ::shuf(Vec128_U8 m) const			{ return typ(_mm_shuffle_epi8(v, m.v)); } \
	/* end */

struct Vec128_S8
{
	Vec128 v;

	Vec128_S8() {}
	explicit Vec128_S8(Vec128 x) : v(x) {}
	explicit Vec128_S8(S8 s) : v(_mm_set1_epi8(s)) {}
	Vec128_S8(S8 s0, S8 s1, S8 s2, S8 s3, S8 s4, S8 s5, S8 s6, S8 s7, S8 s8, S8 s9, S8 s10, S8 s11, S8 s12, S8 s13, S8 s14, S8 s15)
		: v(_mm_setr_epi8(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15)) {}

	RR_DECL_INT_COMMON(Vec128_S8, epi8)
	RR_DECL_INT_SELECT(Vec128_S8)

	// saturating arithmetic
	RR_IMPL_INT_SAT(Vec128_S8, epi8)

	// conversion
	Vec128_S16 to_s16_lo() const;
	Vec128_S32 to_s32_lo() const;

	// get mask
	int movemask() const							{ return _mm_movemask_epi8(v); }

	// binary operations
	Vec128_S8 cmp_eq(Vec128_S8 b) const				{ return Vec128_S8(_mm_cmpeq_epi8(v, b)); }
	Vec128_S8 cmp_gt(Vec128_S8 b) const				{ return Vec128_S8(_mm_cmpgt_epi8(v, b)); }

	// sign twiddling
#ifdef DO_BUILD_SSSE3
	Vec128_S8 abs() const							{ return Vec128_S8(_mm_abs_epi8(v)); }
	Vec128_S8 sign(Vec128_S8 b) const				{ return Vec128_S8(_mm_sign_epi8(v, b)); }

	RR_DECL_INT_SHUF(Vec128_S8);
#endif

#ifdef DO_BUILD_SSE4
	Vec128_S8 max(Vec128_S8 b) const				{ return Vec128_S8(_mm_max_epi8(v, b)); }
	Vec128_S8 min(Vec128_S8 b) const				{ return Vec128_S8(_mm_min_epi8(v, b)); }

	template<int index> int extract() const			{ return _mm_extract_epi8(v, index); }
	template<int index> Vec128_S8 insert(S8 x)		{ return Vec128_S8(_mm_insert_epi8(v, x, index)); }
#endif
};

struct Vec128_S16
{
	Vec128 v;

	Vec128_S16() {}
	explicit Vec128_S16(Vec128 x) : v(x) {}
	explicit Vec128_S16(S16 x) : v(_mm_set1_epi16(x)) {}
	Vec128_S16(S16 s0, S16 s1, S16 s2, S16 s3, S16 s4, S16 s5, S16 s6, S16 s7)
		: v(_mm_setr_epi16(s0, s1, s2, s3, s4, s5, s6, s7)) {}

	RR_DECL_INT_COMMON(Vec128_S16, epi16);
	RR_DECL_INT_SELECT(Vec128_S16)

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

	// conversions
	Vec128_S32 to_s32_lo() const;
	Vec128_U8 to_u8_sat() const;
	Vec128_U8 to_u8_sat(Vec128_S16 b) const;

	template<int index> int extract() const 		{ return _mm_extract_epi16(v, index); }
	template<int index>	Vec128_S16 insert(int x)	{ return Vec128_S16(_mm_insert_epi16(v, x, index)); }

	// saturating arithmetic
	RR_IMPL_INT_SAT(Vec128_S16, epi16)

#ifdef DO_BUILD_SSSE3
	// sign twiddling
	Vec128_S16 abs() const							{ return Vec128_S16(_mm_abs_epi16(v)); }
	Vec128_S16 sign(Vec128_S16 b) const				{ return Vec128_S16(_mm_sign_epi16(v, b.v)); }

	// rounding multiply
	Vec128_S16 mulhrs(Vec128_S16 b) const 			{ return Vec128_S16(_mm_mulhrs_epi16(v, b.v)); }
#endif

	// binary operations
	Vec128_S16 cmp_eq(Vec128_S16 b) const			{ return Vec128_S16(_mm_cmpeq_epi16(v, b.v)); }
	Vec128_S16 cmp_gt(Vec128_S16 b) const			{ return Vec128_S16(_mm_cmpgt_epi16(v, b.v)); }
	Vec128_S16 max(Vec128_S16 b) const				{ return Vec128_S16(_mm_max_epi16(v, b.v)); }
	Vec128_S16 min(Vec128_S16 b) const				{ return Vec128_S16(_mm_min_epi16(v, b.v)); }

	Vec128_S16 mulhi(Vec128_S16 b) const			{ return Vec128_S16(_mm_mulhi_epi16(v, b.v)); }
	Vec128_S16 mullo(Vec128_S16 b) const 			{ return Vec128_S16(_mm_mullo_epi16(v, b.v)); }
	Vec128_S16 operator *(Vec128_S16 b)	const		{ return mullo(b); }
	Vec128_S16& operator *=(Vec128_S16 b)			{ v = mullo(b); return *this; }

	template<int count>	Vec128_S16 shl() const		{ return Vec128_S16(_mm_slli_epi16(v, count)); }
	template<int count>	Vec128_S16 sra() const		{ return Vec128_S16(_mm_srai_epi16(v, count)); }
	template<int count>	Vec128_S16 srl() const		{ return Vec128_S16(_mm_srli_epi16(v, count)); }

	// dot product between even-odd pairs
	Vec128_S32 madd(Vec128_S16r b) const;
};

struct Vec128_S32
{
	Vec128 v;

	Vec128_S32() {}
	explicit Vec128_S32(Vec128 x) : v(x) {}
	explicit Vec128_S32(S32 x) : v(_mm_set1_epi32(x)) {}
	Vec128_S32(S32 s0, S32 s1, S32 s2, S32 s3)
		: v(_mm_setr_epi32(s0, s1, s2, s3)) {}

	RR_DECL_INT_COMMON(Vec128_S32, epi32);
	RR_DECL_INT_SELECT(Vec128_S32);

#ifdef DO_BUILD_SSE4
	template<int index> S32 extract() const			{ return index ? _mm_extract_epi32(v, index) : _mm_cvtsi128_si32(v); }
	template<int index> Vec128_S32 insert(S32 x)	{ return Vec128_S32(_mm_insert_epi32(v, x, index)); }
#endif
	template<int index> Vec128_S32 dup() const		{ return Vec128_S32(shuffle32<index,index,index,index>(v)); }

	VecF32x4   to_f32() const;
	Vec128_S16 to_s16_sat(Vec128_S32 b) const;
#ifdef DO_BUILD_SSE4
	Vec128_U16 to_u16_sat(Vec128_S32 b) const;
#endif

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

	// sign twiddling
#ifdef DO_BUILD_SSSE3
	Vec128_S32 abs() const							{ return Vec128_S32(_mm_abs_epi32(v)); }
	Vec128_S32 sign(Vec128_S32 b) const				{ return Vec128_S32(_mm_sign_epi32(v, b.v)); }

	// Truncate 32-bit values to 8-bits, mod 256
	Vec128_U8 narrow32to8_mod() const;
#endif

	// binary operations
	Vec128_S32 cmp_eq(Vec128_S32 b) const			{ return Vec128_S32(_mm_cmpeq_epi32(v, b.v)); }
	Vec128_S32 cmp_gt(Vec128_S32 b) const			{ return Vec128_S32(_mm_cmpgt_epi32(v, b.v)); }
#ifdef DO_BUILD_SSE4
	Vec128_S32 max(Vec128_S32 b) const				{ return Vec128_S32(_mm_max_epi32(v, b.v)); }
	Vec128_S32 min(Vec128_S32 b) const				{ return Vec128_S32(_mm_min_epi32(v, b.v)); }
	Vec128_S32 mullo(Vec128_S32 b) const 			{ return Vec128_S32(_mm_mullo_epi32(v, b.v)); }

	Vec128_S32 operator *(Vec128_S32 b) const		{ return mullo(b); }
	Vec128_S32& operator *=(Vec128_S32 b)			{ v = _mm_mullo_epi32(v, b.v); return *this; }
#endif
	Vec128_S32 unpack_lo64(Vec128_S32r b) const		{ return Vec128_S32(_mm_unpacklo_epi64(v, b)); }
	Vec128_S32 unpack_hi64(Vec128_S32r b) const		{ return Vec128_S32(_mm_unpackhi_epi64(v, b)); }

#ifdef DO_BUILD_SSE4
	// compares as 2 64-bit lanes
	Vec128_S32 cmp_eq64(Vec128_S32r b) const		{ return Vec128_S32(_mm_cmpeq_epi64(v, b)); }
#endif

	template<int count>	Vec128_S32 shl() const		{ return Vec128_S32(_mm_slli_epi32(v, count)); }
	template<int count> Vec128_S32 sra() const		{ return Vec128_S32(_mm_srai_epi32(v, count)); }
	template<int count>	Vec128_S32 srl() const		{ return Vec128_S32(_mm_srli_epi32(v, count)); }

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

	RR_DECL_INT_COMMON(Vec128_U8, epi8);
	RR_DECL_INT_SELECT(Vec128_U8);

	static Vec128_U8 repeat2(U8 u0, U8 u1)
	{
		return Vec128_U8(_mm_set1_epi16(((int) u1 << 8) | ((int) u0)));
	}
	static Vec128_U8 repeat4(U8 u0, U8 u1, U8 u2, U8 u3)
	{
		return Vec128_U8(_mm_set1_epi32(((int) u3 << 24) | ((int) u2 << 16) | ((int) u1 << 8) | ((int) u0)));
	}

	Vec128_S16 to_s16_lo() const;
	Vec128_S16 to_s16_hi() const;
	Vec128_S32 to_s32_lo() const;
	Vec128_U16 to_u16_lo() const;
	Vec128_U16 to_u16_hi() const;

#ifdef DO_BUILD_SSE4
	template<int index> int extract() const			{ return _mm_extract_epi8(v, index); }
	template<int index> void insert(U8 x) 			{ v = _mm_insert_epi8(v, x, index); }

	// assuming vector is a mask (all ~0 or 0), are any lanes lit up?
	bool mask_any() const							{ return !_mm_testz_si128(v, v); }
#endif

	// unary operations
	int movemask() const 							{ return _mm_movemask_epi8(v); }

	// saturating arithmetic
	RR_IMPL_INT_SAT(Vec128_U8, epu8)

	// binary opetations
	Vec128_U8 cmp_eq(Vec128_U8r b) const			{ return Vec128_U8(_mm_cmpeq_epi8(v, b)); }
	Vec128_U8 cmp_ge(Vec128_U8r b) const			{ return Vec128_U8(_mm_cmpeq_epi8(_mm_max_epu8(b, v), v)); }
	Vec128_U8 max(Vec128_U8r b) const				{ return Vec128_U8(_mm_max_epu8(v, b)); }
	Vec128_U8 min(Vec128_U8r b) const				{ return Vec128_U8(_mm_min_epu8(v, b)); }
	Vec128_U8 unpack_lo64(Vec128_U8r b) const		{ return Vec128_U8(_mm_unpacklo_epi64(v, b)); }
	Vec128_U8 unpack_hi64(Vec128_U8r b) const		{ return Vec128_U8(_mm_unpackhi_epi64(v, b)); }

#ifdef DO_BUILD_SSSE3
	// U8*S8 mixed-type two-element dot product with saturation
	Vec128_S16 madd_sat(Vec128_S8r b) const;

	RR_DECL_INT_SHUF(Vec128_U8)
#endif
	Vec128_S32 sad(Vec128_U8r b) const;

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

	RR_DECL_INT_COMMON(Vec128_U16, epi16);
	RR_DECL_INT_SELECT(Vec128_U16);

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

	Vec128_U8 to_u8_sat() const;
	Vec128_U8 to_u8_sat(Vec128_U16 b) const;
	Vec128_S32 to_s32() const;
	Vec128_U32 to_u32_lo() const;
	Vec128_U32 to_u32_hi() const;

	template<int index> int extract() const 		{ return _mm_extract_epi16(v, index); }
	template<int index>	Vec128_U16 insert(U16 x)	{ return Vec128_U16(_mm_insert_epi16(v, x, index)); }

	// saturating arithmetic
	RR_IMPL_INT_SAT(Vec128_U16, epu16)

	// binary operations
	Vec128_U16 cmp_eq(Vec128_U16 b) const			{ return Vec128_U16(_mm_cmpeq_epi16(v, b.v)); }
#ifdef DO_BUILD_SSE4
	Vec128_U16 max(Vec128_U16 b) const				{ return Vec128_U16(_mm_max_epu16(v, b.v)); }
	Vec128_U16 min(Vec128_U16 b) const				{ return Vec128_U16(_mm_min_epu16(v, b.v)); }
#endif
	Vec128_U16 mulhi(Vec128_U16 b) const			{ return Vec128_U16(_mm_mulhi_epu16(v, b.v)); }
	Vec128_U16 mullo(Vec128_U16 b) const 			{ return Vec128_U16(_mm_mullo_epi16(v, b.v)); }
	Vec128_U16 operator *(Vec128_U16 b) const		{ return mullo(b); }
	Vec128_U16& operator *=(Vec128_U16 b)			{ v = mullo(b); return *this; }

	template<int count>	Vec128_U16 shl() const		{ return Vec128_U16(_mm_slli_epi16(v, count)); }
	template<int count>	Vec128_U16 sra() const		{ return Vec128_U16(_mm_srai_epi16(v, count)); }
	template<int count>	Vec128_U16 srl() const		{ return Vec128_U16(_mm_srli_epi16(v, count)); }

	template<int count> Vec128_U16 sli(Vec128_U16r b) const	{ return shl<count>() + b; }

#ifdef DO_BUILD_SSE4
	// Produce two U16 vectors with the low/high half squared diffs
	// between a and b:
	//   sqdiff_lo[i] = (a[i] - b[i])^2
	//   sqdiff_hi[i] = (a[i+8] - b[i+8])^2
	static void squared_diff(Vec128_U16 &sqdiff_lo, Vec128_U16 &sqdiff_hi, Vec128_U8 a, Vec128_U8 b)
	{
		Vec128_S8 plus_minus { 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1 };
		Vec128_U16 diff_lo = a.unpack_lo(b).madd_sat(plus_minus).u16();
		Vec128_U16 diff_hi = a.unpack_hi(b).madd_sat(plus_minus).u16();

		sqdiff_lo = diff_lo * diff_lo;
		sqdiff_hi = diff_hi * diff_hi;
	}
#endif
};

struct Vec128_U32
{
	Vec128 v;

	Vec128_U32() {}
	explicit Vec128_U32(Vec128 x) : v(x) {}
	explicit Vec128_U32(U32 x) : v(_mm_set1_epi32((S32)x)) {}
	Vec128_U32(U32 s0, U32 s1, U32 s2, U32 s3)
		: v(_mm_setr_epi32((S32)s0, (S32)s1, (S32)s2, (S32)s3)) {}

	RR_DECL_INT_COMMON(Vec128_U32, epi32);
	RR_DECL_INT_SELECT(Vec128_U32);

#ifdef DO_BUILD_SSE4
	template<int index> U32 extract() const			{ return _mm_extract_epi32(v, index); }
	template<int index> void insert(U32 x)			{ v = _mm_insert_epi32(v, x, index); }

	template <int lane>
	void load_lane(const U32 *ptr)					{ v = _mm_insert_epi32(v, *ptr, lane); }
#endif

	// binary operations
	Vec128_U32 cmp_eq(Vec128_U32 b) const			{ return Vec128_U32(_mm_cmpeq_epi32(v, b.v)); }
#ifdef DO_BUILD_SSE4
	Vec128_U32 max(Vec128_U32 b) const				{ return Vec128_U32(_mm_max_epu32(v, b.v)); }
	Vec128_U32 min(Vec128_U32 b) const				{ return Vec128_U32(_mm_min_epu32(v, b.v)); }
	Vec128_U32 mullo(Vec128_U32 b) const 			{ return Vec128_U32(_mm_mullo_epi32(v, b.v)); }

	Vec128_U32 operator *(Vec128_U32 b) const		{ return mullo(b); }
	Vec128_U32& operator *=(Vec128_U32 b)			{ v = _mm_mullo_epi32(v, b.v); return *this; }
#endif

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

	// Sum of squared differences of 4 U8 values per 32-bit lane
	//   result[i] = (a[i*4] - b[i*4])^2, ..., (a[i*4+3] - b[i*4+3])^2
	static Vec128_U32 ssd4(Vec128_U8 a, Vec128_U8 b)
	{
		// Compute 8-bit absolute differences between a and b
		Vec128_U8 abs_diff = a.max(b) - a.min(b);

		// Turn even and odd 8b lanes into 16-bit values
		Vec128_S16 evn = abs_diff.s16() & Vec128_S16 { 0x00ff };
		Vec128_S16 odd = abs_diff.s16().srl<8>();

		// Sum of squares of pairs
		Vec128_S32 sum_evn = evn.madd(evn);
		Vec128_S32 sum_odd = odd.madd(odd);

		return (sum_evn + sum_odd).u32();
	}

	void add_ssd4(Vec128_U8 a, Vec128_U8 b)
	{
		*this += ssd4(a, b);
	}
#endif

	// shuffles
	template<int x, int y, int z, int w>
	Vec128_U32 shuf() const							{ return Vec128_U32(shuffle32<x,y,z,w>(v)); }
	Vec128_U32 xxxw() const							{ return shuf<0,0,0,3>(); }

	// sum of squares of 4 U8 values per 32-bit lane
	static Vec128_U32 sqr_sum(Vec128_U8 a)
	{
		// Isolate even/odd numbered elements, zero-extending to 16 bits
		Vec128_S16 evn16 = a.s16() & Vec128_S16(0xff);
		Vec128_S16 odd16 = a.u16().srl<8>().s16();

		// Square and horizontally add the even/odd pairs
		Vec128_S32 squares32e = evn16.madd(evn16);
		Vec128_S32 squares32o = odd16.madd(odd16);

		// Return the sum
		return (squares32e + squares32o).u32();
	}

	void add_sqr_sum(Vec128_U8 a)
	{
		// NOTE: this matches VPDPBUUD where available
		*this += sqr_sum(a);
	}
};

// Float arithmetic is way less likely to be hopping between types all the time,
// so it's convenient to have a "batteries included" class with operator overloads
//
// The intent is just to simplify notation, it's written so it's trivial to come in
// and out of native ops so if what you want isn't in here and is contextual, just
// go for it.

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
	float scalar_x() const							{ return _mm_cvtss_f32(v); }
	void storeu(float *ptr)	const					{ _mm_storeu_ps(ptr, v); } // unaligned
	void storea(float *ptr)	const					{ _mm_store_ps(ptr, v); } // 16-byte aligned

	// bit casts
	Vec128_S32 s32() const							{ return Vec128_S32(_mm_castps_si128(v)); }
	Vec128_U32 u32() const							{ return Vec128_U32(_mm_castps_si128(v)); }
	Vec128_U8 u8() const							{ return Vec128_U8(_mm_castps_si128(v)); }

	// unary operations
	operator __m128() const							{ return v; }
	VecF32x4 operator -() const						{ return _mm_xor_ps(v, _mm_set1_ps(-0.0f)); }
	VecF32x4 abs() const							{ return _mm_and_ps(v, _mm_castsi128_ps(_mm_set1_epi32(0x7fffffff))); }
	VecF32x4 sqrt() const							{ return _mm_sqrt_ps(v); }

	// basic math and logic
	VecF32x4 operator +(VecF32x4 b)	const			{ return VecF32x4(_mm_add_ps(v, b.v)); }
	VecF32x4 operator -(VecF32x4 b)	const			{ return VecF32x4(_mm_sub_ps(v, b.v)); }
	VecF32x4 operator *(VecF32x4 b) const			{ return VecF32x4(_mm_mul_ps(v, b.v)); }
	VecF32x4 operator /(VecF32x4 b) const			{ return VecF32x4(_mm_div_ps(v, b.v)); }

	VecF32x4 &operator +=(VecF32x4 b)				{ v = _mm_add_ps(v, b); return *this; }
	VecF32x4 &operator -=(VecF32x4 b)				{ v = _mm_sub_ps(v, b); return *this; }
	VecF32x4 &operator *=(VecF32x4 b)				{ v = _mm_mul_ps(v, b); return *this; }
	VecF32x4 &operator /=(VecF32x4 b)				{ v = _mm_div_ps(v, b); return *this; }

	VecF32x4 operator &(VecF32x4 b) const			{ return VecF32x4(_mm_and_ps(v, b.v)); }
	VecF32x4 operator |(VecF32x4 b) const			{ return VecF32x4(_mm_or_ps(v, b.v)); }
	VecF32x4 operator ^(VecF32x4 b) const			{ return VecF32x4(_mm_xor_ps(v, b.v)); }
	VecF32x4 andnot(VecF32x4 b) const				{ return VecF32x4(_mm_andnot_ps(b.v, v)); } // a & ~b (mm_andnot is ~a & b)

	VecF32x4 &operator &=(VecF32x4 b)				{ v = _mm_and_ps(v, b.v); return *this; }
	VecF32x4 &operator |=(VecF32x4 b)				{ v = _mm_or_ps(v, b.v); return *this; }
	VecF32x4 &operator ^=(VecF32x4 b)				{ v = _mm_xor_ps(v, b.v); return *this; }

	// min/max
	VecF32x4 min(VecF32x4 b) const					{ return VecF32x4(_mm_min_ps(v, b.v)); }
	VecF32x4 max(VecF32x4 b) const					{ return VecF32x4(_mm_max_ps(v, b.v)); }

	// comparisons are not as operator overloads because they return a mask vector, not a bool
	VecF32x4 cmp_gt(VecF32x4r b) const				{ return _mm_cmpgt_ps(v, b.v); } // >
	VecF32x4 cmp_ge(VecF32x4r b) const				{ return _mm_cmpge_ps(v, b.v); } // >=
	VecF32x4 cmp_lt(VecF32x4r b) const				{ return _mm_cmplt_ps(v, b.v); } // <
	VecF32x4 cmp_le(VecF32x4r b) const				{ return _mm_cmple_ps(v, b.v); } // <=
	VecF32x4 cmp_eq(VecF32x4r b) const				{ return _mm_cmpeq_ps(v, b.v); } // ==
	VecF32x4 cmp_ngt(VecF32x4r b) const				{ return _mm_cmpngt_ps(v, b.v); } // ! >
	VecF32x4 cmp_nge(VecF32x4r b) const				{ return _mm_cmpnge_ps(v, b.v); } // ! >=
	VecF32x4 cmp_nlt(VecF32x4r b) const				{ return _mm_cmpnlt_ps(v, b.v); } // ! <
	VecF32x4 cmp_nle(VecF32x4r b) const				{ return _mm_cmpnle_ps(v, b.v); } // ! <=
	VecF32x4 cmp_neq(VecF32x4r b) const				{ return _mm_cmpneq_ps(v, b.v); } // ! ==

	// shuffles
	template<int x, int y, int z, int w>
	VecF32x4 shuf() const							{ return _mm_castsi128_ps(_mm_shuffle_epi32(_mm_castps_si128(v), _MM_SHUFFLE(w,z,y,x))); }
	VecF32x4 xxxw() const							{ return shuf<0,0,0,3>(); }

	template<int lane>
	VecF32x4 dup() const							{ return shuf<lane,lane,lane,lane>(); }

	// copy/insert/extract single lanes
#ifdef DO_BUILD_SSE4
	template <int lane_index> 
	float lane() const								{ return _mm_cvtss_f32((lane_index == 0) ? v : _mm_insert_ps(v, v, (lane_index << 6) | 0xe)); }

	template <int lane>
	VecF32x4 copy_lane(VecF32x4 from) const			{ return _mm_blend_ps(v, from, 1<<lane); }

	template <int src_lane, int dest_lane>
	VecF32x4 copy_from_lane_to_lane(VecF32x4 from) const { return _mm_insert_ps(v, from, (src_lane << 6) | (dest_lane << 4)); }

	template <int lane>
	VecF32x4 insert_scalar(float scalar) const		{ return _mm_insert_ps(v, _mm_set_ss(scalar), lane << 4); }

	// cond.select(a, b) is the equivalent of cond ? a : b
	// cond (*this) must be a truth value (i.e. -1 or 0)
	VecF32x4 select(VecF32x4 a, VecF32x4 b) const	{ return _mm_blendv_ps(b, a, v); }
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

RR_IMPL_INT_COMMON(Vec128_S8)
RR_IMPL_INT_COMMON(Vec128_U8)
RR_IMPL_INT_COMMON(Vec128_S16)
RR_IMPL_INT_COMMON(Vec128_U16)
RR_IMPL_INT_COMMON(Vec128_S32)
RR_IMPL_INT_COMMON(Vec128_U32)

#ifdef DO_BUILD_SSSE3
RR_IMPL_INT_SHUF(Vec128_S8)
RR_IMPL_INT_SHUF(Vec128_U8)
#endif

// Conversions
inline VecF32x4   Vec128_S32::to_f32() const		{ return VecF32x4::from_int32(v); }

inline Vec128_S16 Vec128_S8::to_s16_lo() const		{ return Vec128_S16(sext8to16_lo(v)); }
inline Vec128_S32 Vec128_S8::to_s32_lo() const		{ return Vec128_S32(sext8to32_lo(v)); }
inline Vec128_S16 Vec128_U8::to_s16_lo() const		{ return Vec128_S16(zext8to16_lo(v)); }
inline Vec128_U16 Vec128_U8::to_u16_lo() const		{ return Vec128_U16(zext8to16_lo(v)); }
inline Vec128_S16 Vec128_U8::to_s16_hi() const		{ return Vec128_S16(zext8to16_hi(v)); }
inline Vec128_U16 Vec128_U8::to_u16_hi() const		{ return Vec128_U16(zext8to16_hi(v)); }
inline Vec128_S32 Vec128_U8::to_s32_lo() const		{ return Vec128_S32(zext8to32_lo(v)); }
inline Vec128_S32 Vec128_S16::to_s32_lo() const		{ return Vec128_S32(sext16to32_lo(v)); }
inline Vec128_U8  Vec128_S16::to_u8_sat() const		{ return Vec128_U8(_mm_packus_epi16(v, v));}
inline Vec128_U8  Vec128_S16::to_u8_sat(Vec128_S16 b) const { return Vec128_U8(_mm_packus_epi16(v, b)); }
inline Vec128_U8  Vec128_U16::to_u8_sat() const		{ return Vec128_U8(_mm_packus_epi16(v, v)); }
inline Vec128_U8  Vec128_U16::to_u8_sat(Vec128_U16 b) const	{ return Vec128_U8(_mm_packus_epi16(v, b)); }
inline Vec128_U32 Vec128_U16::to_u32_lo() const		{ return Vec128_U32(zext16to32_lo(v)); }
inline Vec128_U32 Vec128_U16::to_u32_hi() const		{ return Vec128_U32(zext16to32_hi(v)); }

inline Vec128_S16 Vec128_S32::to_s16_sat(Vec128_S32 b) const { return Vec128_S16(_mm_packs_epi32(v, b)); }
#ifdef DO_BUILD_SSE4
inline Vec128_U16 Vec128_S32::to_u16_sat(Vec128_S32 b) const { return Vec128_U16(_mm_packus_epi32(v, b)); }
#endif

#ifdef DO_BUILD_SSSE3
inline Vec128_U8 Vec128_S32::narrow32to8_mod() const
{
	return Vec128_U8(_mm_shuffle_epi8(v, _mm_setr_epi8(0,4,8,12, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1)));
}
#endif

inline Vec128_S32 Vec128_S16::madd(Vec128_S16r b) const { return Vec128_S32(_mm_madd_epi16(v, b)); }

#ifdef DO_BUILD_SSSE3
inline Vec128_S16 Vec128_U8::madd_sat(Vec128_S8r b) const			{ return Vec128_S16(_mm_maddubs_epi16(v, b)); }
#endif
inline Vec128_S32 Vec128_U8::sad(Vec128_U8r b) const				{ return Vec128_S32(_mm_sad_epu8(v, b)); }

template<int x,int y,int z,int w>
static inline Vec128_S32 shuffle_two(Vec128_S32r a, Vec128_S32r b){ return Vec128_S32(shuffle_two32<x,y,z,w>(a, b)); }

static inline S32 reduce_add(Vec128_S32r a)						{ return reduce_add_s32(a); }
static inline U32 reduce_add(Vec128_U32r a)						{ return reduce_add_u32(a); }

#ifdef DO_BUILD_SSE4

static inline S32 reduce_min(Vec128_S32r a)						{ Vec128_S32 t = a.min(a.shuf<1,0,3,2>()); t = t.min(t.shuf<2,3,0,1>()); return _mm_cvtsi128_si32(t); }
static inline U32 reduce_min(Vec128_U32r a)						{ Vec128_U32 t = a.min(a.shuf<1,0,3,2>()); t = t.min(t.shuf<2,3,0,1>()); return static_cast<U32>(_mm_cvtsi128_si32(t)); }

static inline S32 reduce_max(Vec128_S32r a)						{ Vec128_S32 t = a.max(a.shuf<1,0,3,2>()); t = t.max(t.shuf<2,3,0,1>()); return _mm_cvtsi128_si32(t); }
static inline U32 reduce_max(Vec128_U32r a)						{ Vec128_U32 t = a.max(a.shuf<1,0,3,2>()); t = t.max(t.shuf<2,3,0,1>()); return static_cast<U32>(_mm_cvtsi128_si32(t)); }

// shifts 256-bit value right by N bytes and returns lowest 128-bits
template <int N>
static Vec128_U32 vsrl_byte(Vec128_U32r lo, Vec128_U32r hi)		{ return Vec128_U32(_mm_alignr_epi8(hi, lo, N)); }

#endif

template<int x,int y,int z,int w>
static inline VecF32x4 shuffle_two32(VecF32x4r a, VecF32x4r b)	{ return _mm_shuffle_ps(a, b, _MM_SHUFFLE(w,z,y,x)); }

#ifdef DO_BUILD_SSE4
static inline Vec128_U8 abs_diff(Vec128_U8 a, Vec128_U8 b)		{ return a.max(b) - a.min(b); }
static inline Vec128_S16 abs_diff(Vec128_S16 a, Vec128_S16 b)	{ return a.max(b) - a.min(b); }

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

#undef RR_DECL_INT_COMMON
#undef RR_DECL_INT_SELECT
#undef RR_DECL_INT_SHUF
#undef RR_IMPL_INT_COMMON
#undef RR_IMPL_INT_SAT
#undef RR_IMPL_INT_SHUF

OODLE_NS_END

