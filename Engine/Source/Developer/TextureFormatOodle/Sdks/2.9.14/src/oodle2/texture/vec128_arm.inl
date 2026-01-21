// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#ifndef OODLE_VEC128_IMPL_ARM
#error DO NOT INCLUDE DIRECTLY - use vec128.inl!
#endif

OODLE_NS_START

#if defined(__clang__) || defined(__GNUC__)
typedef S32 Vec128_S32un __attribute__((aligned(1)));
typedef U32 Vec128_U32un __attribute__((aligned(1)));
typedef U64 Vec128_U64un __attribute__((aligned(1)));
#else
typedef S32 Vec128_S32un;
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

#define RR_CAST_FROM_U8(suffix, v) vreinterpretq_##suffix##_u8(v)
#define RR_CAST_TO_U8(suffix, v) vreinterpretq_u8_##suffix(v)

// Dummy null casts
static RADFORCEINLINE uint8x16_t vreinterpretq_u8_u8(uint8x16_t x) { return x; }
static RADFORCEINLINE int8x16_t vreinterpretq_s8_s8(int8x16_t x) { return x; }
static RADFORCEINLINE uint16x8_t vreinterpretq_u16_u16(uint16x8_t x) { return x; }
static RADFORCEINLINE int16x8_t vreinterpretq_s16_s16(int16x8_t x) { return x; }
static RADFORCEINLINE uint32x4_t vreinterpretq_u32_u32(uint32x4_t x) { return x; }
static RADFORCEINLINE int32x4_t vreinterpretq_s32_s32(int32x4_t x) { return x; }

// Missing _low half-ops
static RADFORCEINLINE int16x8_t vmovl_low_s8(int8x16_t a) { return vmovl_s8(vget_low_s8(a)); }
static RADFORCEINLINE uint16x8_t vmovl_low_u8(uint8x16_t a) { return vmovl_u8(vget_low_u8(a)); }
static RADFORCEINLINE int16x8_t vmovl_low_s16(int16x8_t a) { return vmovl_s16(vget_low_s16(a)); }
static RADFORCEINLINE uint32x4_t vmovl_low_u16(uint16x8_t a) { return vmovl_u16(vget_low_u16(a)); }
static RADFORCEINLINE float64x2_t vcvt_low_f64_f32(float32x4_t a) { return vcvt_f64_f32(vget_low_f32(a)); }
static RADFORCEINLINE int32x4_t vabdl_low_s16(int16x8_t a, int16x8_t b) { return vabdl_s16(vget_low_s16(a), vget_low_s16(b)); }
static RADFORCEINLINE int16x8_t vmull_low_s8(int8x16_t a, int8x16_t b) { return vmull_s8(vget_low_s8(a), vget_low_s8(b)); }
static RADFORCEINLINE uint16x8_t vmull_low_u8(uint8x16_t a, uint8x16_t b) { return vmull_u8(vget_low_u8(a), vget_low_u8(b)); }
static RADFORCEINLINE int32x4_t vmull_low_s16(int16x8_t a, int16x8_t b) { return vmull_s16(vget_low_s16(a), vget_low_s16(b)); }
static RADFORCEINLINE uint32x4_t vmull_low_u16(uint16x8_t a, uint16x8_t b) { return vmull_u16(vget_low_u16(a), vget_low_u16(b)); }
static RADFORCEINLINE int32x4_t vmlal_low_s16(int32x4_t a, int16x8_t b, int16x8_t c) { return vmlal_s16(a, vget_low_s16(b), vget_low_s16(c)); }
static RADFORCEINLINE uint64x2_t vmlal_low_u32(uint64x2_t a, uint32x4_t b, uint32x4_t c) { return vmlal_u32(a, vget_low_u32(b), vget_low_u32(c)); }

#define RR_DECL_INT_COMMON(typ,suffix) \
	static typ zero()								{ return typ(vdupq_n_##suffix(0)); } \
	/* loads */ \
	static typ loada(const void * ptr)				{ return typ(RR_CAST_FROM_U8(suffix, vld1q_u8((const uint8_t*)ptr))); } \
	static typ loadu(const void * ptr)				{ return typ(RR_CAST_FROM_U8(suffix, vld1q_u8((const uint8_t*)ptr))); } \
	static typ loadu_lo64(const void * ptr)			{ return typ(RR_CAST_FROM_U8(suffix, vcombine_u8(vld1_u8((const uint8_t*)ptr), vdup_n_u8(0)))); } \
	static typ loadu_dup64(const void * ptr)		{ uint8x16_t x = vreinterpretq_u8_u64(vld1q_dup_u64((const Vec128_U64un*)ptr)); \
													  return typ(RR_CAST_FROM_U8(suffix, x)); } \
	static typ loadu_lo32(const void * ptr)			{ uint8x16_t x = vreinterpretq_u8_u32(vld1q_lane_u32((const Vec128_U32un*)ptr, vdupq_n_u32(0), 0)); \
													  return typ(RR_CAST_FROM_U8(suffix, x)); } \
	static typ loadu_dup32(const void * ptr)		{ uint8x16_t x = vreinterpretq_u8_u32(vld1q_dup_u32((const Vec128_U32un*)ptr)); \
													  return typ(RR_CAST_FROM_U8(suffix, x)); } \
	/* stores */ \
	void storea(void * ptr) const					{ vst1q_u8((uint8_t*)ptr, RR_CAST_TO_U8(suffix, v)); } \
	void storeu(void * ptr) const					{ vst1q_u8((uint8_t*)ptr, RR_CAST_TO_U8(suffix, v)); } \
	void storeu_lo64(void * ptr) const				{ vst1_u8((uint8_t*)ptr, vget_low_u8(RR_CAST_TO_U8(suffix, v))); } \
	void storeu_lo32(void * ptr) const				{ uint8x8_t x = vget_low_u8(RR_CAST_TO_U8(suffix, v)); \
													  vst1_lane_u32((Vec128_U32un*)ptr, vreinterpret_u32_u8(x), 0); } \
	/* conversions and bit casts */ \
	Vec128_S8 s8() const; \
	Vec128_U8 u8() const; \
	Vec128_S16 s16() const; \
	Vec128_U16 u16() const; \
	Vec128_S32 s32() const; \
	Vec128_U32 u32() const; \
	VecF32x4 f32() const; \
	/* unpacks and logic */ \
	typ unpack_lo(typ b) const						{ return typ(vzip1q_##suffix(v, b.v)); } \
	typ unpack_hi(typ b) const						{ return typ(vzip2q_##suffix(v, b.v)); } \
	typ operator &(typ b) const						{ return typ(vandq_##suffix(v, b.v)); } \
	typ operator |(typ b) const						{ return typ(vorrq_##suffix(v, b.v)); } \
	typ operator ^(typ b) const						{ return typ(veorq_##suffix(v, b.v)); } \
	typ andnot(typ b) const							{ return typ(vbicq_##suffix(v, b.v)); } \
	typ& operator &=(typ b)							{ v = vandq_##suffix(v, b.v); return *this; } \
	typ& operator |=(typ b)							{ v = vorrq_##suffix(v, b.v); return *this; } \
	typ& operator ^=(typ b)							{ v = veorq_##suffix(v, b.v); return *this; } \
	/* arithmetic */ \
	typ operator +(typ b) const						{ return typ(vaddq_##suffix(v, b.v)); } \
	typ operator -(typ b) const						{ return typ(vsubq_##suffix(v, b.v)); } \
	typ operator *(typ b) const						{ return typ(vmulq_##suffix(v, b.v)); } \
	typ& operator +=(typ b)							{ v = vaddq_##suffix(v, b.v); return *this; } \
	typ& operator -=(typ b)							{ v = vsubq_##suffix(v, b.v); return *this; } \
	typ& operator *=(typ b)							{ v = vmulq_##suffix(v, b.v); return *this; } \
	/* min/max */ \
	typ min(typ b) const							{ return typ(vminq_##suffix(v, b.v)); } \
	typ max(typ b) const							{ return typ(vmaxq_##suffix(v, b.v)); } \
	/* cond.select(a, b) is the equivalent of cond ? a : b */ \
	/* cond (*this) must be a truth value (i.e. ~0 or 0) of the given type */ \
	typ select(typ a, typ b) const					{ return typ(vbslq_##suffix(v, a, b)); } \
	/* shuffle */ \
	typ shuf(Vec128_S8 m) const; \
	typ shuf(Vec128_U8 m) const; \
	/* end */

#define RR_IMPL_INT_SHUF(typ,suffix) \
	inline typ typ::shuf(Vec128_S8 m) const			{ return typ(vreinterpretq_##suffix##_u8(vqtbl1q_u8(vreinterpretq_u8_##suffix(v), m))); } \
	inline typ typ::shuf(Vec128_U8 m) const			{ return typ(vreinterpretq_##suffix##_u8(vqtbl1q_u8(vreinterpretq_u8_##suffix(v), m))); } \

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

	RR_DECL_INT_COMMON(Vec128_S8, s8);

	// conversion
	Vec128_S16 to_s16_lo() const;
	Vec128_S32 to_s32_lo() const;

	template<int count>	Vec128_S8 shl() const		{ return Vec128_S8(vshlq_n_s8(v, count)); }
	template<int count>	Vec128_S8 sra() const		{ return Vec128_S8(vshrq_n_s8(v, count)); }
	template<int count>	Vec128_S8 srl() const		{ return Vec128_S8(vreinterpretq_s8_u8(vshrq_n_u8(vreinterpretq_u8_s8(v), count))); }
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

	RR_DECL_INT_COMMON(Vec128_S16, s16);

	static Vec128_S16 repeat4(S16 a, S16 b, S16 c, S16 d)	{ return Vec128_S16(a,b,c,d, a,b,c,d); }
	static int16x8x2_t loadu_x2(const void *ptr)			{ return vld1q_s16_x2((const int16_t*)ptr); }
	static Vec128_S16 loadu_zext8(const void *ptr)
	{
		uint16x8_t tmp = vmovl_u8(vld1_u8((const uint8_t*)ptr));
		return Vec128_S16(vreinterpretq_s16_u16(tmp));
	}

	// Duplicate the low half of register to the high half
	Vec128_S16 dup_lo() const								{ return Vec128_S16(vdupq_laneq_u64(vreinterpretq_u64_s16(v), 0)); }
	// Duplicate the high half of register to the low half
	Vec128_S16 dup_hi() const								{ return Vec128_S16(vdupq_laneq_u64(vreinterpretq_u64_s16(v), 1)); }

	Vec128_S16 mullo(Vec128_S16r b) const 					{ return Vec128_S16(vmulq_s16(v, b)); }

	// calculates (v*b) >> 16 keeping multiplication intermediate in 32-bit
	inline Vec128_S16 mulhi(Vec128_S16r b);

	template<int count>	Vec128_S16 shl() const		{ return Vec128_S16(vshlq_n_s16(v, count)); }
	template<int count>	Vec128_S16 sra() const		{ return Vec128_S16(vshrq_n_s16(v, count)); }
	template<int count>	Vec128_S16 srl() const		{ return Vec128_S16(vreinterpretq_s16_u16(vshrq_n_u16(vreinterpretq_u16_s16(v), count))); }

	template<int count> Vec128_S16 rnd_sra() const	{ return Vec128_S16(vrshrq_n_s16(v, count)); }

	Vec128_S32 to_s32_lo() const;
	Vec128_U8 to_u8_sat() const;
	Vec128_U8 to_u8_sat(Vec128_S16r b) const;
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

	RR_DECL_INT_COMMON(Vec128_S32, s32);

	template<int lane> S32 extract() const			{ return vgetq_lane_s32(v, lane); }
	template<int lane> Vec128_S32 insert(S32 x)		{ return Vec128_S32(vsetq_lane_s32(x, v, lane)); }

	// *this must be a truth value (i.e. -1 or 0 in all lanes)
	bool all() const								{ return vmaxvq_s32(v) == -1; }
	bool any() const								{ return vminvq_s32(v) == -1; }

	Vec128_S32 abs() const							{ return Vec128_S32(vabsq_s32(v)); }

	Vec128_S32 cmp_eq(Vec128_S32 b) const			{ return Vec128_S32(vceqq_s32(v, b)); }
	Vec128_S32 cmp_gt(Vec128_S32 b) const			{ return Vec128_S32(vcgtq_s32(v, b)); }

	// compares as 2 64-bit lanes
	Vec128_S32 cmp_eq64(Vec128_S32r b) const		{ return Vec128_S32(vceqq_u64(v, b)); }

	Vec128_S32 unpack_lo64(Vec128_S32 b) const		{ return Vec128_S32(vcombine_s32(vget_low_s32(v), vget_low_s32(b))); }
	Vec128_S32 unpack_hi64(Vec128_S32 b) const		{ return Vec128_S32(vcombine_s32(vget_high_s32(v), vget_high_s32(b))); }

	template<int count>	Vec128_S32 shl() const		{ return Vec128_S32(vshlq_n_s32(v, count)); }
	template<int count>	Vec128_S32 sra() const		{ return Vec128_S32(vshrq_n_s32(v, count)); }
	template<int count>	Vec128_S32 srl() const		{ return Vec128_S32(vreinterpretq_s32_u32(vshrq_n_u32(vreinterpretq_u32_s32(v), count))); }

	// duplicate lane
	template <int lane> Vec128_S32 dup() const		{ return Vec128_S32(vdupq_laneq_s32(v, lane)); }

	// blockwise swaps
	Vec128_S32 yxwz() const							{ return Vec128_S32(vrev64q_s32(v)); }

	// cyclic permutations of elements
	Vec128_S32 zwxy() const							{ return Vec128_S32(vextq_s32(v, v, 2)); }

	// shuffles
	Vec128_S32 xyxy() const							{ return Vec128_S32(vdupq_laneq_u64(vreinterpretq_u64_s16(v), 0)); }
	Vec128_S32 zwzw() const							{ return Vec128_S32(vdupq_laneq_u64(vreinterpretq_u64_s16(v), 1)); }

	// Truncate 32-bit values to 8-bits, mod 256
	Vec128_U8 narrow32to8_mod() const;

	VecF32x4   to_f32() const;
	Vec128_S16 to_s16_sat(Vec128_S32r b) const;
	Vec128_U16 to_u16_sat(Vec128_S32r b) const;
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

	RR_DECL_INT_COMMON(Vec128_U8, u8);

	static Vec128_U8 repeat2(U8 u0, U8 u1)			{ return Vec128_U8(vreinterpretq_u8_u16( vdupq_n_u16( (u1 << 8) | u0 ) )); }
	static Vec128_U8 repeat4(U8 a, U8 b, U8 c, U8 d) { return Vec128_U8(a,b,c,d, a,b,c,d, a,b,c,d, a,b,c,d); }

	static uint8x16x4_t loadu_x4(const void* ptr) 	{ return vld1q_u8_x4((const uint8_t*)ptr); }

	Vec128_U16 to_u16_lo() const;
	Vec128_U16 to_u16_hi() const;
	Vec128_S16 to_s16_lo() const;
	Vec128_S16 to_s16_hi() const;
	Vec128_S32 to_s32_lo() const;

	Vec128_U8 unpack_lo64(Vec128_U8r b) const		{ return Vec128_U8(vzip1q_u64(v, b)); }
	Vec128_U8 unpack_hi64(Vec128_U8r b) const		{ return Vec128_U8(vzip2q_u64(v, b)); }

	template<int count>	Vec128_U8 shl() const		{ return Vec128_U8(vshlq_n_u8(v, count)); }
	template<int count>	Vec128_U8 sra() const		{ return Vec128_U8(vreinterpretq_u8_s8(vshrq_n_s8(vreinterpretq_s8_u8(v), count))); }
	template<int count>	Vec128_U8 srl() const		{ return Vec128_U8(vshrq_n_u8(v, count)); }

	// duplicate 32-bit lane
	template <int lane> Vec128_U8 dup32() const		{ return Vec128_U8(vreinterpretq_u8_u32(vdupq_laneq_u32(vreinterpretq_u32_u8(v), lane))); }

	// compare
	Vec128_U8 cmp_eq(Vec128_U8r b) const			{ return Vec128_U8(vceqq_u8(v, b)); }

	// some reductions
	void reduce_max_8away()							{ v = vmaxq_u8(v, vreinterpretq_u8_u32(vextq_u32(vreinterpretq_u32_u8(v), vreinterpretq_u32_u8(v), 2))); }
	void reduce_max_4away()							{ v = vmaxq_u8(v, vreinterpretq_u8_u32(vrev64q_u32(vreinterpretq_u32_u8(v)))); }

	// assuming vector is a mask (all ~0 or 0), are any lanes lit up?
	bool mask_any() const							{ return vmaxvq_u8(v) != 0; }
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

	RR_DECL_INT_COMMON(Vec128_U16, u16);

	static Vec128_U16 repeat4(U16 a, U16 b, U16 c, U16 d)	{ return Vec128_U16(a,b,c,d, a,b,c,d); }

	Vec128_U8 to_u8_sat() const								{ return Vec128_U8(vqmovn_high_u16(vqmovn_u16(v), v)); }
	Vec128_U8 to_u8_sat(Vec128_U16r b) const				{ return Vec128_U8(vqmovn_high_u16(vqmovn_u16(v), b)); }
	Vec128_U32 to_u32_lo() const;
	Vec128_U32 to_u32_hi() const;

	// calculates (v*b) >> 16 keeping multiplication intermediate in 32-bit
	inline Vec128_U16 mulhi(Vec128_U16r b);

	template<int count> Vec128_U16 shl() const				{ return Vec128_U16(vshlq_n_u16(v, count)); }
	template<int count> Vec128_U16 sra() const				{ return Vec128_U16(vreinterpretq_u16_s16(vshrq_n_s16(vreinterpretq_s16_u16(v), count))); }
	template<int count> Vec128_U16 srl() const				{ return Vec128_U16(vshrq_n_u16(v, count)); }
	template<int count> Vec128_U16 sli(Vec128_U16r b) const	{ return Vec128_U16(vsliq_n_u16(b, v, count)); }

	// Produce two U16 vectors with the low/high half squared diffs
	// between a and b:
	//   sqdiff_lo[i] = (a[i] - b[i])^2
	//   sqdiff_hi[i] = (a[i+8] - b[i+8])^2
	static void squared_diff(Vec128_U16 &sqdiff_lo, Vec128_U16 &sqdiff_hi, Vec128_U8 a, Vec128_U8 b)
	{
		Vec128_U8 diff = vabdq_u8(a, b); // absolute difference fits in U8
		sqdiff_lo = vmull_low_u8(diff, diff);
		sqdiff_hi = vmull_high_u8(diff, diff);
	}
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

	RR_DECL_INT_COMMON(Vec128_U32, u32);

	template <int lane>
	void load_lane(const U32 *ptr)					{ v = vld1q_lane_u32(ptr, v, lane); }

	template<int count> Vec128_U32 shl() const				{ return Vec128_U32(vshlq_n_u32(v, count)); }
	template<int count> Vec128_U32 sra() const				{ return Vec128_U32(vreinterpretq_u32_s32(vshrq_n_s32(vreinterpretq_s32_u32(v), count))); }
	template<int count> Vec128_U32 srl() const				{ return Vec128_U32(vshrq_n_u32(v, count)); }
	template<int count> Vec128_U32 sli(Vec128_U32r b) const	{ return Vec128_U32(vsliq_n_u32(b, v, count)); }

	// shifts 128-bit value right by N bytes
	template <int N>
	Vec128_U32 srl_byte() const								{ return Vec128_U32(vreinterpretq_u32_u8(vextq_u8(u8(), Vec128_U8::zero(), N))); }

	// compares
	Vec128_U32 cmp_eq(Vec128_U32r b) const			{ return Vec128_U32(vceqq_u32(v, b)); }
	Vec128_U32 cmp_gt(Vec128_U32r b) const			{ return Vec128_U32(vcgtq_u32(v, b)); }

	// get lane
	template <int lanen> uint32_t lane() const		{ return vgetq_lane_u32(v, lanen); }

	// duplicate lane
	template <int lane> Vec128_U32 dup() const		{ return Vec128_U32(vdupq_laneq_u32(v, lane)); }

	template <int lane>
	Vec128_U32 copy_lane(Vec128_U32r from) const	{ return Vec128_U32(vcopyq_laneq_u32(v, lane, from, lane)); }

	// cyclic permutations of elements
	Vec128_U32 yzwx() const							{ return Vec128_U32(vextq_u32(v, v, 1)); }
	Vec128_U32 zwxy() const							{ return Vec128_U32(vextq_u32(v, v, 2)); }
	Vec128_U32 wxyz() const							{ return Vec128_U32(vextq_u32(v, v, 3)); }

	Vec128_U32 xxxw() const							{ return Vec128_U32(vreinterpretq_u32_u8(vqtbl1q_u8(v, Vec128_U8(0,1,2,3, 0,1,2,3, 0,1,2,3, 12,13,14,15)))); }

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
		uint16x8_t mul_lo = vmull_low_u8(a, b);
		uint16x8_t mul_hi = vmull_high_u8(a, b);

		// sum pairs of 16-bit values into 32-bit lanes
		uint32x4_t sum_lo = vpaddlq_u16(mul_lo); // 0+1,   2+3,   4+5,   6+7
		uint32x4_t sum_hi = vpaddlq_u16(mul_hi); // 8+9, 10+11, 12+13, 14+15

		// 0+1+2+3, 4+5+6+7, 8+9+10+11, 12+13+14+15
		uint32x4_t sum = vpaddq_u32(sum_lo, sum_hi);
		return Vec128_U32(sum);
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

	// Sum of squared differences of 4 U8 values per 32-bit lane
	//   result[i] = (a[i*4] - b[i*4])^2, ..., (a[i*4+3] - b[i*4+3])^2
	static Vec128_U32 ssd4(Vec128_U8 a, Vec128_U8 b)
	{
		Vec128_U8 diff = vabdq_u8(a, b); // absolute diffs
		return dot(diff, diff);
	}

	void add_ssd4(Vec128_U8 a, Vec128_U8 b)
	{
		Vec128_U8 diff = vabdq_u8(a, b); // absolute diffs
		add_dot(diff, diff);
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

	static VecF32x4 zero()							{ return VecF32x4(vdupq_n_f32(0.0f)); }
	static VecF32x4 from_int32(Vec128_S32 x)		{ return VecF32x4(vcvtq_f32_s32(x)); }
	static VecF32x4 from_int32(Vec128_U32 x)		{ return VecF32x4(vcvtq_f32_u32(x)); }
	static VecF32x4 loadu(const void *ptr)			{ return VecF32x4(vld1q_f32((const float*)ptr)); }
	static VecF32x4 loada(const void *ptr)			{ return VecF32x4(vld1q_f32((const float*)ptr)); }
	static VecF32x4 load_scalar(const float *ptr)	{ return VecF32x4(vld1q_lane_f32(ptr, vdupq_n_f32(0.f), 0)); }
	static VecF32x4 load_pair(const float *ptr)		{ return VecF32x4(vcombine_f32(vld1_f32(ptr), vdup_n_f32(0.f))); }
	static float32x4x4_t loadu_x4(const void* ptr) 	{ return vld1q_f32_x4((const float*)ptr); }

	static VecF32x4 bitmask(U32 a)					{ return VecF32x4(vreinterpretq_f32_u32(vdupq_n_u32(a))); }
	static VecF32x4 bitmask(U32 a, U32 b, U32 c, U32 d) { return Vec128_U32 { a, b, c, d }.f32(); }

	F32 scalar_x() const							{ return vdups_laneq_f32(v, 0); }
	void storeu(void *ptr) const					{ vst1q_f32((float*)ptr, v); }
	void storea(void *ptr) const					{ vst1q_f32((float*)ptr, v); }

	// bit casts
	Vec128_U8 u8() const							{ return Vec128_U8(vreinterpretq_u8_f32(v)); }
	Vec128_S32 s32() const							{ return Vec128_S32(vreinterpretq_s32_f32(v)); }
	Vec128_U32 u32() const							{ return Vec128_U32(vreinterpretq_u32_f32(v)); }

	Vec128_S32 to_int32_trunc()	const				{ return Vec128_S32(vcvtq_s32_f32(v)); }
	Vec128_S32 to_int32_round()	const				{ return Vec128_S32(vcvtnq_s32_f32(v)); }

	VecF32x4 abs() const							{ return VecF32x4(vabsq_f32(v)); }
	VecF32x4 sqrt() const							{ return VecF32x4(vsqrtq_f32(v)); }

	// arithmetic
	VecF32x4 operator -() const						{ return VecF32x4(vnegq_f32(v)); }

#define RR_BIN_OP(op, opname) \
	VecF32x4 operator op(VecF32x4 b) const			{ return VecF32x4(v##opname##q_f32(v, b.v)); } \
	VecF32x4& operator op##=(VecF32x4 b)			{ v = v##opname##q_f32(v, b.v); return *this; }
	RR_BIN_OP(+, add)
	RR_BIN_OP(-, sub)
	RR_BIN_OP(*, mul)
	RR_BIN_OP(/, div)
#undef RR_BIN_OP

	// logic
#define RR_BIT_OP(op) \
	VecF32x4 operator op(VecF32x4 b) const			{ return (u32() op b.u32()).f32(); } \
	VecF32x4 operator op##=(VecF32x4 b)				{ v = (u32() op b.u32()).f32(); return *this; }
	RR_BIT_OP(&)
	RR_BIT_OP(|)
	RR_BIT_OP(^)
#undef RR_BIT_OP

	// compares
	VecF32x4 cmp_eq(VecF32x4 b) const				{ return VecF32x4(vreinterpretq_f32_u32(vceqq_f32(v, b))); }
	VecF32x4 cmp_gt(VecF32x4 b) const				{ return VecF32x4(vreinterpretq_f32_u32(vcgtq_f32(v, b))); }

	// min/max
	VecF32x4 min(VecF32x4 b) const					{ return VecF32x4(vminq_f32(v, b.v)); }
	VecF32x4 max(VecF32x4 b) const					{ return VecF32x4(vmaxq_f32(v, b.v)); }

	// get lane
	template <int lanen> F32 lane() const			{ return vgetq_lane_f32(v, lanen); }

	// duplicate lane
	template <int lane> VecF32x4 dup() const		{ return VecF32x4(vdupq_laneq_f32(v, lane)); }

	// copy single lane into this
	template <int lane>
	VecF32x4 copy_lane(VecF32x4 from)				{ return VecF32x4(vcopyq_laneq_f32(v, lane, from, lane)); }

	template <int src_lane, int dest_lane>
	VecF32x4 copy_from_lane_to_lane(VecF32x4 from)	{ return VecF32x4(vcopyq_laneq_f32(v, dest_lane, from, src_lane)); }

	template <int lane>
	VecF32x4 insert_scalar(float scalar) const		{ return VecF32x4(vsetq_lane_f32(scalar, v, lane)); }

	// cond.select(a, b) is the equivalent of cond ? a : b
	// cond (*this) must be a truth value (i.e. -1 or 0)
	VecF32x4 select(VecF32x4 a, VecF32x4 b) const	{ return VecF32x4(vbslq_f32(vreinterpretq_u32_f32(v), a, b)); }

	VecF32x4 unpack_lo(VecF32x4r b) const			{ return VecF32x4(vzipq_f32(v, b).val[0]); }
	VecF32x4 unpack_hi(VecF32x4r b) const			{ return VecF32x4(vzipq_f32(v, b).val[1]); }

	// broadcasts
	VecF32x4 xxxx() const							{ return dup<0>(); }
	VecF32x4 yyyy() const							{ return dup<1>(); }
	VecF32x4 zzzz() const							{ return dup<2>(); }
	VecF32x4 wwww() const							{ return dup<3>(); }

	// cyclic permutations of elements
	VecF32x4 yzwx() const							{ return VecF32x4(vextq_f32(v, v, 1)); }
	VecF32x4 wxyz() const							{ return VecF32x4(vextq_f32(v, v, 3)); }

	// cyclic permutations of first 3 elements
	VecF32x4 yzxw() const							{ return u8().shuf(Vec128_U8 { 4,5,6,7, 8,9,10,11, 0,1,2,3, 12,13,14,15 }).f32(); }
	VecF32x4 zxyw() const							{ return u8().shuf(Vec128_U8 { 8,9,10,11, 0,1,2,3, 4,5,6,7, 12,13,14,15 }).f32(); }

	// blockwise swaps
	VecF32x4 yxwz() const							{ return VecF32x4(vrev64q_f32(v)); }
	VecF32x4 zwxy() const							{ return VecF32x4(vextq_f32(v, v, 2)); }

	VecF32x4 xyyz() const							{ return u8().shuf(Vec128_U8 { 0,1,2,3, 4,5,6,7, 4,5,6,7, 8,9,10,11 }).f32(); }
	VecF32x4 xxxw() const							{ return u8().shuf(Vec128_U8 { 0,1,2,3, 0,1,2,3, 0,1,2,3, 12,13,14,15 }).f32(); }

	// utils
	VecF32x4 square() const							{ return VecF32x4(vmulq_f32(v, v)); }

	// some reduction
	VecF32x4 sum_across() const						{ VecF32x4 t = *this + zwxy(); return t + t.yxwz(); }
	VecF32x4 sum_across_inner_outer() const			{ VecF32x4 t { vpaddq_f32(v, v) }; return VecF32x4 { vpaddq_f32(t, t) }; }
};

Vec128_S16 Vec128_S16::mulhi(Vec128_S16r b)
{
	Vec128_S32 lo(vmull_s16( vget_low_s16(v), vget_low_s16(b)));
	Vec128_S32 hi(vmull_high_s16(v, b));
	return Vec128_S16(vuzp2q_u16( lo.s16(), hi.s16() ));
}

Vec128_U16 Vec128_U16::mulhi(Vec128_U16r b)
{
	Vec128_U32 lo(vmull_u16( vget_low_u16(v), vget_low_u16(b) ));
	Vec128_U32 hi(vmull_high_u16(v, b));
	return Vec128_U16(vuzp2q_u16( lo.u16(), hi.u16() ));
}

inline VecF32x4 Vec128_S32::to_f32() const						{ return VecF32x4::from_int32(*this); }

// sign extend low 4 int16 to int32
inline Vec128_S32 Vec128_S16::to_s32_lo() const					{ return Vec128_S32(vmovl_low_s16(v)); }
// saturate int16 to uint8
inline Vec128_U8 Vec128_S16::to_u8_sat() const					{ return Vec128_U8(vqmovun_high_s16(vqmovun_s16(v), v)); }
// saturate 8+8 values of int16 into 16 values of uint8
inline Vec128_U8 Vec128_S16::to_u8_sat(Vec128_S16r b) const		{ return Vec128_U8(vqmovun_high_s16(vqmovun_s16(v), b)); }
// saturate 4+4 values of int32 into 8 values of int16
inline Vec128_S16 Vec128_S32::to_s16_sat(Vec128_S32r b) const	{ return Vec128_S16(vqmovn_high_s32(vqmovn_s32(v), b)); }
// saturate 4+4 values of int32 into 8 values of uint16
inline Vec128_U16 Vec128_S32::to_u16_sat(Vec128_S32r b) const	{ return Vec128_U16(vqmovun_high_s32(vqmovun_s32(v), b)); }
// sign extend low 8 bytes into eight 32-bit lanes
inline Vec128_S16 Vec128_S8::to_s16_lo() const					{ return Vec128_S16(vmovl_low_s8(v)); }
// sign extend low 4 bytes into four 32-bit lanes
inline Vec128_S32 Vec128_S8::to_s32_lo() const					{ return Vec128_S32(vmovl_low_s16(vmovl_low_s8(v))); }
// zero-extend low 8 bytes to U16
inline Vec128_U16 Vec128_U8::to_u16_lo() const					{ return Vec128_U16(vmovl_low_u8(v)); }
// zero-extend high 8 bytes to U16
inline Vec128_U16 Vec128_U8::to_u16_hi() const					{ return Vec128_U16(vmovl_high_u8(v)); }
// zero-extend low 8 bytes to S16
inline Vec128_S16 Vec128_U8::to_s16_lo() const					{ return Vec128_S16(vreinterpretq_s16_u16(vmovl_low_u8(v))); }
// zero-extend high 8 bytes to S16
inline Vec128_S16 Vec128_U8::to_s16_hi() const					{ return Vec128_S16(vreinterpretq_s16_u16(vmovl_high_u8(v))); }
// zero-extend low 4 bytes to S32
inline Vec128_S32 Vec128_U8::to_s32_lo() const					{ return Vec128_S32(vreinterpretq_s32_u32(vmovl_low_u16(vmovl_low_u8(v)))); }
// zero-extend low 4 uint16 to uint32
inline Vec128_U32 Vec128_U16::to_u32_lo() const					{ return Vec128_U32(vmovl_low_u16(v)); }
// zero-extend high 4 uint16 to uint32
inline Vec128_U32 Vec128_U16::to_u32_hi() const					{ return Vec128_U32(vmovl_high_u16(v)); }

#define RR_IMPL_CASTS(tsrc, nsrc) \
	inline Vec128_S8 Vec128_##tsrc::s8() const		{ return Vec128_S8(vreinterpretq_s8_##nsrc(v)); } \
	inline Vec128_U8 Vec128_##tsrc::u8() const		{ return Vec128_U8(vreinterpretq_u8_##nsrc(v)); } \
	inline Vec128_S16 Vec128_##tsrc::s16() const	{ return Vec128_S16(vreinterpretq_s16_##nsrc(v)); } \
	inline Vec128_U16 Vec128_##tsrc::u16() const	{ return Vec128_U16(vreinterpretq_u16_##nsrc(v)); } \
	inline Vec128_S32 Vec128_##tsrc::s32() const	{ return Vec128_S32(vreinterpretq_s32_##nsrc(v)); } \
	inline Vec128_U32 Vec128_##tsrc::u32() const	{ return Vec128_U32(vreinterpretq_u32_##nsrc(v)); } \
	inline VecF32x4 Vec128_##tsrc::f32() const		{ return VecF32x4(vreinterpretq_f32_##nsrc(v)); } \
	/* end */

RR_IMPL_CASTS(S8, s8)
RR_IMPL_CASTS(U8, u8)
RR_IMPL_CASTS(S16, s16)
RR_IMPL_CASTS(U16, u16)
RR_IMPL_CASTS(S32, s32)
RR_IMPL_CASTS(U32, u32)

#undef RR_IMPL_CASTS

RR_IMPL_INT_SHUF(Vec128_S8, s8)
RR_IMPL_INT_SHUF(Vec128_U8, u8)
RR_IMPL_INT_SHUF(Vec128_S16, s16)
RR_IMPL_INT_SHUF(Vec128_U16, u16)
RR_IMPL_INT_SHUF(Vec128_S32, s32)
RR_IMPL_INT_SHUF(Vec128_U32, u32)

#undef RR_IMPL_INT_SHUF

#define RR_TYPE_BIN_OPS(type, stype, optype) \
	static inline Vec128_##type operator << (Vec128_##type##r a, Vec128_##stype##r b)	{ return Vec128_##type(vshlq_##optype(a, b)); } \
	static inline Vec128_##type abs_diff(Vec128_##type##r a, Vec128_##type##r b)		{ return Vec128_##type(vabdq_##optype(a, b)); }

RR_TYPE_BIN_OPS(S8,  S8,  s8 )
RR_TYPE_BIN_OPS(S16, S16, s16)
RR_TYPE_BIN_OPS(S32, S32, s32)
RR_TYPE_BIN_OPS(U8,  S8,  u8 )
RR_TYPE_BIN_OPS(U16, S16, u16)
RR_TYPE_BIN_OPS(U32, U32, u32)

#undef RR_TYPE_BIN_OPS

#define RR_SQR_OP(bits, dtype, stype, optype) \
	static inline Vec128_##dtype sqr##bits##_lo(Vec128_##stype##r a)	{ return Vec128_##dtype(vmull_low_##optype(a, a)); } \
	static inline Vec128_##dtype sqr##bits##_hi(Vec128_##stype##r a)	{ return Vec128_##dtype(vmull_high_##optype(a, a)); }

RR_SQR_OP(16, S16,  S8,  s8)
RR_SQR_OP(16, U16,  U8,  u8)
RR_SQR_OP(32, S32, S16, s16)
RR_SQR_OP(32, U32, U16, u16)

#undef RR_SQR_OP

inline Vec128_U8 Vec128_S32::narrow32to8_mod() const { return u32().narrow32to8_mod(); }

static inline Vec128_U16 pack_narrow(Vec128_U32r a, Vec128_U32r b)	{ return Vec128_U16(vmovn_high_u32(vmovn_u32(a), b)); }
static inline Vec128_S16 pack_narrow(Vec128_S32r a, Vec128_S32r b)	{ return Vec128_S16(vmovn_high_s32(vmovn_s32(a), b)); }
static inline Vec128_U8  pack_narrow(Vec128_U16r a, Vec128_U16r b)	{ return Vec128_U8(vmovn_high_u16(vmovn_u16(a), b)); }
static inline Vec128_S8  pack_narrow(Vec128_S16r a, Vec128_S16r b)	{ return Vec128_S8(vmovn_high_s16(vmovn_s16(a), b)); }


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

static inline Vec128_U32 zext16to32_lo(Vec128_U16 x)				{ return Vec128_U32(vmovl_u16(vget_low_u16(x))); }
static inline Vec128_U32 zext16to32_hi(Vec128_U16 x)				{ return Vec128_U32(vmovl_high_u16(x)); }
static inline Vec128_S32 sext16to32_lo(Vec128_S16 x)				{ return Vec128_S32(vmovl_s16(vget_low_s16(x))); }
static inline Vec128_S32 sext16to32_hi(Vec128_S16 x)				{ return Vec128_S32(vmovl_high_s16(x)); }

// shifts 256-bit value right by N bytes and returns lowest 128-bits
template <int N>
static Vec128_U32 vsrl_byte(Vec128_U32r lo, Vec128_U32r hi)			{ return Vec128_U32(vreinterpretq_u32_u8(vextq_u8(lo.u8(), hi.u8(), N))); }

// rounding_shift_right(lerp * diff, 15) + lo
static inline Vec128_S16 vinterp16(Vec128_S16 lerp, Vec128_S16 diff) 					{ return Vec128_S16(vqrdmulhq_s16(lerp, diff)); }
#if _MSC_VER >= 1930 // VC2022 has this
// VC++ doesn't have an "arch" type switch for ARMv8.1 but we simply don't support Oodle Texture on Windows ARM64 sub-ARMv8.1
static inline Vec128_S16 vinterp16(Vec128_S16 lerp, Vec128_S16 diff, Vec128_S16 lo) 	{ return Vec128_S16(vqrdmlahq_s16(lo, lerp, diff)); }
#else
// this folds to SQRDMLAH on ARMv8.1 targets that have it (with Clang anyway) but is also 100% compatible with ARMv8.0
static inline Vec128_S16 vinterp16(Vec128_S16 lerp, Vec128_S16 diff, Vec128_S16 lo) 	{ return Vec128_S16(vqaddq_s16(lo, vqrdmulhq_s16(lerp, diff))); }
#endif

template <int N> static inline Vec128_U8 vsrl128(Vec128_U8r v)
{
	uint64x2_t lo_shift = vshrq_n_u64(v, N);
	uint64x2_t hi_shift = vshlq_n_u64(v, 64-N);
	uint64x2_t shifted = vorrq_u64(lo_shift, vzip2q_u64(hi_shift, lo_shift));
	return Vec128_U8(shifted);
}

#undef RR_CAST_FROM_U8
#undef RR_CAST_TO_U8
#undef RR_DECL_INT_COMMON

OODLE_NS_END

