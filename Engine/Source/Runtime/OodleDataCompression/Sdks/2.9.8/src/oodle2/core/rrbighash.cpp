// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrbighash.h"
#include "rrmem.h"
#include "rrmemutil.h"
#include <ctype.h>
#include "rrmath.h"

OODLE_NS_START

#ifdef __RADNT__
#ifndef __RADSSE2__
#error SSE2 !!
#endif
#endif

/**

TODO :

rrBigHash :
	when buffer is 16-byte aligned, could save a bunch of work
	unroll?
	
-----------

CB : the main hash is Lookup3 by Bob Jenkins
it uses only shifts so it is fast even on Cell SPE (no muls)
it is cryptographically strong
it is faster than any other strong hash tested (even Cessu's SSE)

we use Big-endian all the time so that it is platform independent

------

Note on BurtleQuad :

Runs 4 lookup3's in "parallel" using SIMD
then mixes the final state using non-SIMD to make a single output hash
this is fully robust and Bob-approved

BurtleQuad with SSE2 is about 2X faster than regular Lookup3 x64
BurtleQuad x64 is about 2X *slower* than regular Lookup3 x64
 (because the optimizer is not doing a very good job of handling the register overload)
 
------
 
BTW I am still using Big Endian.

Making SSE2 do the bswap really doesn't hurt :

BE :

burtle64             : 1.429757
burtlequad64         : 0.724052
BQ : 15D0C29DAB4B72B7

LE :

burtle64             : 1.413546
burtlequad64         : 0.653381
BQ : D0E7A0521A580E12

**/

#define BURTLEQUAD_BIGENDIAN

#ifdef BURTLEQUAD_BIGENDIAN
#ifdef __RADLITTLEENDIAN__
#define BURTLEQUAD_BSWAP
#endif
#else
#ifdef __RADBIGENDIAN__
#define BURTLEQUAD_BSWAP
#endif
#endif // BURTLEQUAD_BIGENDIAN

/*
lookup3.c, by Bob Jenkins, May 2006, Public Domain.
*/

/*
mix -- mix 3 32-bit values reversibly.
*/

#define rot(x,k)    RR_ROTL32(x,k)

#define mix(a,b,c) \
{ \
  a -= c;  a ^= rot(c, 4);  c += b; \
  b -= a;  b ^= rot(a, 6);  a += c; \
  c -= b;  c ^= rot(b, 8);  b += a; \
  a -= c;  a ^= rot(c,16);  c += b; \
  b -= a;  b ^= rot(a,19);  a += c; \
  c -= b;  c ^= rot(b, 4);  b += a; \
}

#define vec_mix(a,b,c) \
{ \
  a -= c;  a ^= vec_rot(c, 4);  c += b; \
  b -= a;  b ^= vec_rot(a, 6);  a += c; \
  c -= b;  c ^= vec_rot(b, 8);  b += a; \
  a -= c;  a ^= vec_rot(c,16);  c += b; \
  b -= a;  b ^= vec_rot(a,19);  a += c; \
  c -= b;  c ^= vec_rot(b, 4);  b += a; \
}

/*
final -- final mixing of 3 32-bit values (a,b,c) into c
*/
#define final(a,b,c) \
{ \
  c ^= b; c -= rot(b,14); \
  a ^= c; a -= rot(c,11); \
  b ^= a; b -= rot(a,25); \
  c ^= b; c -= rot(b,16); \
  a ^= c; a -= rot(c,4);  \
  b ^= a; b -= rot(a,14); \
  c ^= b; c -= rot(b,24); \
}

#define vec_final(a,b,c) \
{ \
  c ^= b; c -= vec_rot(b,14); \
  a ^= c; a -= vec_rot(c,11); \
  b ^= a; b -= vec_rot(a,25); \
  c ^= b; c -= vec_rot(b,16); \
  a ^= c; a -= vec_rot(c,4);  \
  b ^= a; b -= vec_rot(a,14); \
  c ^= b; c -= vec_rot(b,24); \
}

#if 0 // defined(__RADNEON__)

//neon!
// slower than scalar on test Cortex-A57

// runtest_nx test_all misc
// inName : src:\lztestset\lzt02
// scalar :
// hash             : 0.653 millis, 0.88 c/B, rate= 1156.26 MB/s
// h : 6493039114730617170
// neon :
// hash             : 1.001 millis, 1.35 c/B, rate= 754.10 MB/s
// h : 6493039114730617170

OODLE_NS_END
#include <arm_neon.h>
OODLE_NS_START

//uint32x4_t test = { 1,2,3,4 };

#define VEC_INIT_PRE	( uint32x4_t{
#define VEC_INIT_POST	})

//#define VEC_INIT_PRE	t_u32quad(
//#define VEC_INIT_POST	)

RAD_ALIGN(struct,t_u32quad,16)
{
	uint32x4_t data;
	
	t_u32quad() { }
	t_u32quad(const uint32x4_t d) : data(d) { }
    /*
	t_u32quad(U32 v0,U32 v1,U32 v2,U32 v3)
	{
		U32 a[4] = { v0,v1,v2,v3 };
		data = vld1q_u32(a);
	}
    /**/
		
	RADFORCEINLINE void operator += ( const t_u32quad & rhs)
	{
		data = vaddq_u32(data,rhs.data);
	}
	
	RADFORCEINLINE void operator -= ( const t_u32quad & rhs)
	{
		data = vsubq_u32(data,rhs.data);
	}
	
	RADFORCEINLINE void operator ^= ( const t_u32quad & rhs)
	{
		data = veorq_u32(data,rhs.data);
	}
	
	operator uint32x4_t() const { return data; }
};

//#define vec_rot(x,k) vorrq_u32( vshlq_n_u32(x,k) , vshrq_n_u32(x,32-k) )
// shift and insert to skip the or :
#define vec_rot(x,k) vsliq_n_u32( vshrq_n_u32(x,32-k),x, k )

static RADFORCEINLINE const uint32x4_t arm_bswap32(uint32x4_t vec)
{
	return vrev32q_u8(vec);
}

static RADFORCEINLINE void StoreVec(void * ptr, const t_u32quad & vec)
{
	vst1q_u32( (U32 *)ptr, vec );
}

static RADFORCEINLINE const t_u32quad LoadVec(const U8 * ptr, int offset)
{
	// @@ unaligned load ? :
	
	uint32x4_t qw = vld1q_u32( (const U32 *) ( ptr + offset ) );
	#ifdef BURTLEQUAD_BSWAP
	uint32x4_t swapped = arm_bswap32( qw );
	return t_u32quad( swapped );
	#else
	return t_u32quad( qw );
	#endif
}

#define LoadVec_Aligned16 LoadVec

#elif defined(__RADSSE2__)

// SSE2 version
//  * confirmed makes the same hash as non-SSE2

// 7.39 GB/s on Beasty with bswap
// 7.45 GB/s on Beasty without bswap

#define VEC_INIT_PRE	t_u32quad( _mm_set_epi32(
#define VEC_INIT_POST	))

OODLE_NS_END
#include <emmintrin.h>
#include <xmmintrin.h>
OODLE_NS_START

RAD_ALIGN(struct,t_u32quad,16)
{
	__m128i data;
	
	t_u32quad() { }
	t_u32quad(const __m128i d) : data(d) { }
		
	RADFORCEINLINE void operator += ( const t_u32quad & rhs)
	{
		data = _mm_add_epi32(data,rhs.data);
	}
	
	RADFORCEINLINE void operator -= ( const t_u32quad & rhs)
	{
		data = _mm_sub_epi32(data,rhs.data);
	}
	
	RADFORCEINLINE void operator ^= ( const t_u32quad & rhs)
	{
		data = _mm_xor_si128(data,rhs.data);
	}
};

// SSE rotate is two shifts and an or :
#define _mm_rotl_epi32(x,k) _mm_or_si128( _mm_slli_epi32(x,k) , _mm_srli_epi32(x,32-k) )

#define vec_rot(v,k) t_u32quad( _mm_rotl_epi32(v.data,k) )

//#define BINARY8(a,b,c,d,e,f,g,h) \
//	((a<<7)|(b<<6)|(c<<5)|(d<<4)|(e<<3)|(f<<2)|(g<<1)|h)

#define SHUFW(a,b,c,d) ((a<<6)|(b<<4)|(c<<2)|(d))
// SHUFWR(0,1,2,3) is a nop
#define SHUFWR(a,b,c,d) ((a)|(b<<2)|(c<<4)|(d<<6))

// could be just one pshufb in SSSE3
//	but this does not hurt speed
static RADFORCEINLINE __m128i _mm_bswap32( __m128i v )
{
	// swap the 16 bit words :
	v = _mm_shufflelo_epi16( v , SHUFWR(1,0,3,2) );
	v = _mm_shufflehi_epi16( v , SHUFWR(1,0,3,2) );
	// swap within 16 bit words :
	v = _mm_or_si128( _mm_slli_epi16( v, 8 ), _mm_srli_epi16( v, 8 ) );
	return v;
}

static RADFORCEINLINE const t_u32quad LoadVec(const U8 * ptr, int offset)
{
	// unaligned load :
	__m128i qw = _mm_loadu_si128( (const __m128i *) ( ptr + offset ) );
	#ifdef BURTLEQUAD_BIGENDIAN
	__m128i swapped = _mm_bswap32( qw );
	return t_u32quad( swapped );
	#else
	return t_u32quad( qw );
	#endif
}

static RADFORCEINLINE const t_u32quad LoadVec_Aligned16(const U8 * ptr, int offset)
{
	// aligned load :
	__m128i qw = _mm_load_si128( (const __m128i *) ( ptr + offset ) );
	#ifdef BURTLEQUAD_BIGENDIAN
	__m128i swapped = _mm_bswap32( qw );
	return t_u32quad( swapped );
	#else
	return t_u32quad( qw );
	#endif
}

static RADFORCEINLINE void StoreVec(void * ptr, const t_u32quad & vec)
{
	_mm_storeu_si128( (__m128i *) ( ptr ), vec.data );
}

#else // fallback

// unknown platform / no vector instruction set
//	use a struct of 4 U32's to mimic a vector

#define VEC_INIT_PRE	{{
#define VEC_INIT_POST	}}

struct t_u32quad
{
	U32 data[4];
	
	RADFORCEINLINE void operator += ( const t_u32quad & rhs)
	{
		data[0] += rhs.data[0];
		data[1] += rhs.data[1];
		data[2] += rhs.data[2];
		data[3] += rhs.data[3];
	}
	
	RADFORCEINLINE void operator -= ( const t_u32quad & rhs)
	{
		data[0] -= rhs.data[0];
		data[1] -= rhs.data[1];
		data[2] -= rhs.data[2];
		data[3] -= rhs.data[3];
	}
	
	RADFORCEINLINE void operator ^= ( const t_u32quad & rhs)
	{
		data[0] ^= rhs.data[0];
		data[1] ^= rhs.data[1];
		data[2] ^= rhs.data[2];
		data[3] ^= rhs.data[3];
	}
};

static RADFORCEINLINE const t_u32quad vec_rot(const t_u32quad & v,int k)
{
	t_u32quad ret;
	ret.data[0] = RR_ROTL32(v.data[0],k);
	ret.data[1] = RR_ROTL32(v.data[1],k);
	ret.data[2] = RR_ROTL32(v.data[2],k);
	ret.data[3] = RR_ROTL32(v.data[3],k);
	return ret;
}

static RADFORCEINLINE const t_u32quad LoadVec(const U8 * ptr, int offset)
{	
	t_u32quad ret;
	
	#ifdef BURTLEQUAD_BIGENDIAN
	
	#ifdef __RADBIGENDIAN__
	memcpy(&ret,(ptr+offset),16);
	#else
	ret.data[0] = RR_GET32_BE_UNALIGNED(ptr+offset);
	ret.data[1] = RR_GET32_BE_UNALIGNED(ptr+offset+4);
	ret.data[2] = RR_GET32_BE_UNALIGNED(ptr+offset+8);
	ret.data[3] = RR_GET32_BE_UNALIGNED(ptr+offset+12);
	#endif
	
	#else
	
	#ifdef __RADLITTLEENDIAN__
	memcpy(&ret,(ptr+offset),16);
	#else
	ret.data[0] = RR_GET32_LE_UNALIGNED(ptr+offset);
	ret.data[1] = RR_GET32_LE_UNALIGNED(ptr+offset+4);
	ret.data[2] = RR_GET32_LE_UNALIGNED(ptr+offset+8);
	ret.data[3] = RR_GET32_LE_UNALIGNED(ptr+offset+12);
	#endif
	
	#endif
	
	return ret;
	
	//#endif
}

#define LoadVec_Aligned16	LoadVec

static RADFORCEINLINE void StoreVec(void * ptr, const t_u32quad & vec)
{
	memcpy(ptr,&vec,16);
}

#endif // Vector implementation

typedef void* t_shuftype;

static RADFORCEINLINE t_shuftype MakeShuffle(void * ptr)
{
	return NULL;
}

static RADFORCEINLINE void rrBurtleQuad48(const U8 * k,
	t_u32quad & a,t_u32quad & b,t_u32quad & c, t_shuftype shuffle)
{
	// LoadVec must work unaligned :
	t_u32quad ta = LoadVec(k,0);
	t_u32quad tb = LoadVec(k,16);
	t_u32quad tc = LoadVec(k,32);
	
	a += ta;
	b += tb;
	c += tc;

	vec_mix(a,b,c);
}

static RADFORCEINLINE void rrBurtleQuad48_Aligned16(const U8 * k,
	t_u32quad & a,t_u32quad & b,t_u32quad & c)
{
	// LoadVec must work unaligned :
	t_u32quad ta = LoadVec_Aligned16(k,0);
	t_u32quad tb = LoadVec_Aligned16(k,16);
	t_u32quad tc = LoadVec_Aligned16(k,32);
	
	a += ta;
	b += tb;
	c += tc;

	vec_mix(a,b,c);
}
	
static RADFORCEINLINE void rrBurtleQuad_Init(
	t_u32quad & a,t_u32quad & b,t_u32quad & c,U32 length)
{
	/* Set up the internal state */
	const t_u32quad c_init_a = VEC_INIT_PRE 0xdeadbeef, 0xdeadbeef, 0xdeadbeef, 0xdeadbeef VEC_INIT_POST;
	const t_u32quad c_init_b = VEC_INIT_PRE 0x206F85B3, 0x206F85B3, 0x206F85B3, 0x206F85B3 VEC_INIT_POST;
	const t_u32quad c_init_c = VEC_INIT_PRE 0x5768B525, 0x5768B525, 0x5768B525, 0x5768B525 VEC_INIT_POST;

	a = c_init_a;
	b = c_init_b;
	c = c_init_c;

	t_u32quad lens = VEC_INIT_PRE length, length, length, length VEC_INIT_POST;
	a += lens;
	c -= lens;
}

static RADFORCEINLINE U64 rrBurtleQuad_Final(
	t_u32quad & va,t_u32quad & vb,t_u32quad & vc)
{
	// -- vector registers moved back to normal registers here --
	RAD_ALIGN(U32,a,16) [4];
	RAD_ALIGN(U32,b,16) [4];
	RAD_ALIGN(U32,c,16) [4];
		
	StoreVec(a,va);
	StoreVec(b,vb);
	StoreVec(c,vc);
	
	
	// mix the 4 independent states into final output :
	U32 fa,fb,fc;
	fa = a[0];
	fb = a[1];
	fc = a[2];
	mix(fa,fb,fc);
	fa += a[3];
	fb += b[0];
	fc += b[1];
	mix(fa,fb,fc);
	fa += b[2];
	fb += b[3];
	fc += c[0];
	mix(fa,fb,fc);
	fa += c[1];
	fb += c[2];
	fc += c[3];
	final(fa,fb,fc);

	// make sure c is the bottom 32, it's the best :
	return ((U64)fb<<32) | fc;
}
		
U64 rrBigHash64_Combine(U64 a, U64 b)
{
	/*
	// not awesome :
	U8 t[sizeof(U64)*2];
	RR_PUT64_BE(t,a);
	RR_PUT64_BE((t+sizeof(U64)),b);
	//return rrBigHash64_Scalar(t,sizeof(t));
	return rrBigHash64_SIMD(t,sizeof(t));
	*/
	
    U32 t0 = 0x5768B525;
	U32 t1 = (U32)(a>>32);
	U32 t2 = (U32)a;
    U32 t3 = 0x206F85B3;
	U32 t4 = (U32)(b>>32);
	U32 t5 = (U32)b;
	
	mix(t0,t1,t2);
	t0 += t3;
	t1 += t4;
	t2 += t5;
	mix(t0,t1,t2);

	return ((U64)t1<<32) | t2;
}

U64 rrBigHash64_SIMD( 
  const void * _buffer,
  SINTa      _length)
{
	t_u32quad a,b,c;

	const U8 * k = (const U8 *)_buffer;
	UINTr length = (UINTr)_length;
	
	rrBurtleQuad_Init(a,b,c,(U32)length);
	
	if ( rrIsAlignedPointer(_buffer,16) )
	{
		// do 8*48 to get an integer multiple of 128 for prefetch on PPC :                      
		while (length >= 384)
		{
			for(int t=0;t<8;t++)
			{
				rrBurtleQuad48_Aligned16(k,a,b,c); 
				k += 48;
			}

			length -= 384;			
		}
			     
		while (length >= 48)
		{
			rrBurtleQuad48_Aligned16(k,a,b,c);
			k += 48;
			length -= 48;
		}
	
	}
	else
	{
	
		t_shuftype shuffle = MakeShuffle((void *)_buffer);
		
		// do 8*48 to get an integer multiple of 128 for prefetch on PPC :                      
		while (length >= 384)
		{
			// unroll ? no, it's too much code
			for(int t=0;t<8;t++)
			{
				rrBurtleQuad48(k,a,b,c,shuffle); 
				k += 48; length -= 48;
			}
		}
			     
		while (length >= 48)
		{
			rrBurtleQuad48(k,a,b,c,shuffle);
			k += 48;
			length -= 48;
		}
	}
	
	if ( length > 0 )
	{
		// last partial block;
		//	do it Sean's easy way by memcpying into a temp buf with zeros in it
		RR_ASSERT( length < 48 );
		RADASSUME( length < 48 );
		RAD_ALIGN(U8,last[48],16);
		RR_ZERO(last);
		memcpy(last,k,(SINTa)length);
		rrBurtleQuad48_Aligned16(last,a,b,c);		
	}

	return rrBurtleQuad_Final(a,b,c);
}

//=======================================================================

#if 0
// The recommended rrBigHash64_Scalar :
U64 rrBigHash64_Scalar(const U8 *  bytes, SINTa size)
{
    // they pretty much all seem to be equally awesome, so just use BurtleBurtle :

    return rrHash_BurtleBurtle_64(bytes,size);
}
#endif



//=================================================================

#if 0 //-------------
// test compare code

//=================================================================================================

#define stb_big32(c)    (((c)[0]<<24) + (c)[1]*65536 + (c)[2]*256 + (c)[3])

static void stb__sha1(const U8 *chunk, U32 h[5])
{
    int i;
    U32 a,b,c,d,e;
    U32 w[80];

    for (i=0; i < 16; ++i)
    {
        w[i] = stb_big32(&chunk[i*4]);
    }
    
    for (i=16; i < 80; ++i) 
    {
        U32 t;
        t = w[i-3] ^ w[i-8] ^ w[i-14] ^ w[i-16];
        w[i] = (t + t) | (t >> 31);
    }

    a = h[0];
    b = h[1];
    c = h[2];
    d = h[3];
    e = h[4];

    #define STB__SHA1(k,f)                                       \
    {                                                            \
        U32 temp = (a << 5) + (a >> 27) + (f) + e + (k) + w[i];  \
        e = d;                                                   \
        d = c;                                                   \
        c = (b << 30) + (b >> 2);                                \
        b = a;                                                   \
        a = temp;                                                \
    }

    i=0;
    for (; i < 20; ++i) STB__SHA1(0x5a827999, d ^ (b & (c ^ d))      );
    for (; i < 40; ++i) STB__SHA1(0x6ed9eba1, b ^ c ^ d                 );
    for (; i < 60; ++i) STB__SHA1(0x8f1bbcdc, (b & c) + (d & (b ^ c)) );
    for (; i < 80; ++i) STB__SHA1(0xca62c1d6, b ^ c ^ d                 );

    #undef STB__SHA1

    h[0] += a;
    h[1] += b;
    h[2] += c;
    h[3] += d;
    h[4] += e;
}

//static void stb_sha1(U8 output[20], const U8 *buffer, U32 len)
static void stb_sha1(U32 h[5], const U8 *buffer, U32 len)
{
    U8 final_block[128];
    U32 end_start, final_len, j;
    //int i;

    //U32 h[5];

    h[0] = 0x67452301;
    h[1] = 0xefcdab89;
    h[2] = 0x98badcfe;
    h[3] = 0x10325476;
    h[4] = 0xc3d2e1f0;

    // we need to write padding to the last one or two
    // blocks, so build those first into 'final_block'

    // we have to write one special byte, plus the 8-byte length

    // compute the block where the data runs out
    end_start = len & ~63;

    // compute the earliest we can encode the length
    if (((len+9) & ~63) == end_start) 
    {
        // it all fits in one block, so fill a second-to-last block
        end_start -= 64;
    }

    final_len = end_start + 128;

    // now we need to copy the data in
    RR_ASSERT(end_start + 128 >= len+9);
    RR_ASSERT(end_start < len || len < 64-9);

    j = 0;
    if (end_start > len)
    {
        j = (U32) - (int) end_start;
    }

    for (; end_start + j < len; ++j)
    {
        final_block[j] = buffer[end_start + j];
    }
    
    final_block[j++] = 0x80;
    
    while (j < 128-5) // 5 byte length, so write 4 extra padding bytes
    {
        final_block[j++] = 0;
    }
    
    // big-endian size
    final_block[j++] = (U8) ( len >> 29 );
    final_block[j++] = (U8) ( len >> 21 );
    final_block[j++] = (U8) ( len >> 13 );
    final_block[j++] = (U8) ( len >>  5 );
    final_block[j++] = (U8) ( len <<  3 );
    RR_ASSERT(j == 128 && end_start + j == final_len);

    for (j=0; j < final_len; j += 64) 
    {
        // 512-bit chunks
        
        if (j+64 >= end_start+64)
            stb__sha1(&final_block[j - end_start], h);
        else
            stb__sha1(&buffer[j], h);
    }

    /*
    for (i=0; i < 5; ++i) 
    {
        output[i*4 + 0] = h[i] >> 24;
        output[i*4 + 1] = h[i] >> 16;
        output[i*4 + 2] = h[i] >>  8;
        output[i*4 + 3] = h[i] >>  0;
    }
    */
}

U64 rrHash_SHA1_64(const U8 * bytes, S32 size)
{
    // SHA1 is 160 bits :
    U32 sha[5];
    stb_sha1(sha,bytes,size);
    
    // evil 5 dword -> 2 dword map ?
    // do I need to be careful not to cancel one bit differences here?
    //  dunno, seems like they should be scrambled pretty well by SHA already
    
    /*
    U64 ret = (sha[0]<<32) | sha[1];
    ret += sha[2] * 698769069ULL;
    ret += (sha[3]<<32) | sha[4];
    */
    
    /*
    // how best to put 'md5' into ret ?
    U64 ret = (md5[0]<<32) | (md5[1]);
    ret += md5[2] * 0x1EF57D8A7B344E7BULL;
    ret += md5[3] * 0xD9EA571C8AF880B6ULL;
    */
    
    U32 a = sha[0], b = sha[1], c = sha[2];
    rrMix3(a,b,c);
    a += sha[3];    b += sha[4];
    rrFinalMix3(a,b,c);
    
    U64 ret = ((U64)a<<32) + ((U64)b<<24) + (U64)c;

    return ret;
}

//===============================================================
// OpenSSL MD5 :

#include <stdlib.h>
#include <string.h>

#define MD5_LONG unsigned int
#define MD5_CBLOCK  64
#define MD5_LBLOCK  (MD5_CBLOCK/4)
#define MD5_DIGEST_LENGTH 16

typedef struct MD5state_st
    {
    MD5_LONG A,B,C,D;
    MD5_LONG Nl,Nh;
    MD5_LONG data[MD5_LBLOCK];
    unsigned int num;
    } MD5_CTX;

int MD5_Init(MD5_CTX *c);
int MD5_Update(MD5_CTX *c, const void *data, size_t len);
int MD5_Final(unsigned char *md, MD5_CTX *c);
unsigned char *MD5(const unsigned char *d, size_t n, unsigned char *md);
void MD5_Transform(MD5_CTX *c, const unsigned char *b);

void md5_block_data_order (MD5_CTX *c, const void *p,size_t num);

#define HASH_LONG       MD5_LONG
#define HASH_CTX        MD5_CTX
#define HASH_CBLOCK     MD5_CBLOCK
#define HASH_UPDATE     MD5_Update
#define HASH_TRANSFORM      MD5_Transform
#define HASH_FINAL      MD5_Final
#define HASH_MAKE_STRING(c,s)   do {    \
    unsigned long ll;       \
    ll=(c)->A; HOST_l2c(ll,(s));    \
    ll=(c)->B; HOST_l2c(ll,(s));    \
    ll=(c)->C; HOST_l2c(ll,(s));    \
    ll=(c)->D; HOST_l2c(ll,(s));    \
    } while (0)
#define HASH_BLOCK_DATA_ORDER   md5_block_data_order

/*
#define F(x,y,z)    (((x) & (y))  |  ((~(x)) & (z)))
#define G(x,y,z)    (((x) & (z))  |  ((y) & (~(z))))
*/

/* As pointed out by Wei Dai <weidai@eskimo.com>, the above can be
 * simplified to the code below.  Wei attributes these optimizations
 * to Peter Gutmann's SHS code, and he attributes it to Rich Schroeppel.
 */
#define F(b,c,d)    ((((c) ^ (d)) & (b)) ^ (d))
#define G(b,c,d)    ((((b) ^ (c)) & (d)) ^ (c))
#define H(b,c,d)    ((b) ^ (c) ^ (d))
#define I(b,c,d)    (((~(d)) | (b)) ^ (c))

#define R0(a,b,c,d,k,s,t) { \
    a+=((k)+(t)+F((b),(c),(d))); \
    a=ROTATE(a,s); \
    a+=b; };\

#define R1(a,b,c,d,k,s,t) { \
    a+=((k)+(t)+G((b),(c),(d))); \
    a=ROTATE(a,s); \
    a+=b; };

#define R2(a,b,c,d,k,s,t) { \
    a+=((k)+(t)+H((b),(c),(d))); \
    a=ROTATE(a,s); \
    a+=b; };

#define R3(a,b,c,d,k,s,t) { \
    a+=((k)+(t)+I((b),(c),(d))); \
    a=ROTATE(a,s); \
    a+=b; };

/*
 * Engage compiler specific rotate intrinsic function if available.
 */
#define ROTATE(a,n) RR_ROTL32(a,n)

#ifndef ROTATE
#define ROTATE(a,n)     (((a)<<(n))|(((a)&0xffffffff)>>(32-(n))))
#endif

#define HOST_c2l(c,l)   ((l)=*((const unsigned int *)(c)), (c)+=4, l)
#define HOST_l2c(l,c)   (*((unsigned int *)(c))=(l), (c)+=4, l)

/*
 * Time for some action:-)
 */

int HASH_UPDATE (HASH_CTX *c, const void *data_, MD5_LONG len)
    {
    const unsigned char *data= (const unsigned char *) data_;
    unsigned char *p;
    HASH_LONG l;
    MD5_LONG n;

    if (len==0) return 1;

    l=(c->Nl+(((HASH_LONG)len)<<3))&0xffffffffUL;
    /* 95-05-24 eay Fixed a bug with the overflow handling, thanks to
     * Wei Dai <weidai@eskimo.com> for pointing it out. */
    if (l < c->Nl) /* overflow */
        c->Nh++;
    c->Nh+= (len>>29);  /* might cause compiler warning on 16-bit */
    c->Nl=l;

    n = c->num;
    if (n != 0)
        {
        p=(unsigned char *)c->data;

        if (len >= HASH_CBLOCK || len+n >= HASH_CBLOCK)
            {
            memcpy (p+n,data,HASH_CBLOCK-n);
            HASH_BLOCK_DATA_ORDER (c,p,1);
            n      = HASH_CBLOCK-n;
            data  += n;
            len   -= n;
            c->num = 0;
            memset (p,0,HASH_CBLOCK);   /* keep it zeroed */
            }
        else
            {
            memcpy (p+n,data,len);
            c->num += (unsigned int)len;
            return 1;
            }
        }

    n = len/HASH_CBLOCK;
    if (n > 0)
        {
        HASH_BLOCK_DATA_ORDER (c,data,n);
        n    *= HASH_CBLOCK;
        data += n;
        len  -= n;
        }

    if (len != 0)
        {
        p = (unsigned char *)c->data;
        c->num = len;
        memcpy (p,data,len);
        }
    return 1;
    }


void HASH_TRANSFORM (HASH_CTX *c, const unsigned char *data)
    {
    HASH_BLOCK_DATA_ORDER (c,data,1);
    }


int HASH_FINAL (unsigned char *md, HASH_CTX *c)
    {
    unsigned char *p = (unsigned char *)c->data;
    size_t n = c->num;

    p[n] = 0x80; /* there is always room for one */
    n++;

    if (n > (HASH_CBLOCK-8))
        {
        memset (p+n,0,HASH_CBLOCK-n);
        n=0;
        HASH_BLOCK_DATA_ORDER (c,p,1);
        }
    memset (p+n,0,HASH_CBLOCK-8-n);

    p += HASH_CBLOCK-8;
    (void)HOST_l2c(c->Nl,p);
    (void)HOST_l2c(c->Nh,p);
    p -= HASH_CBLOCK;
    HASH_BLOCK_DATA_ORDER (c,p,1);
    c->num=0;
    memset (p,0,HASH_CBLOCK);

    HASH_MAKE_STRING(c,md);

    return 1;
    }

/* Implemented from RFC1321 The MD5 Message-Digest Algorithm
 */

#define INIT_DATA_A (unsigned long)0x67452301L
#define INIT_DATA_B (unsigned long)0xefcdab89L
#define INIT_DATA_C (unsigned long)0x98badcfeL
#define INIT_DATA_D (unsigned long)0x10325476L

int MD5_Init(MD5_CTX *c)
    {
    c->A=INIT_DATA_A;
    c->B=INIT_DATA_B;
    c->C=INIT_DATA_C;
    c->D=INIT_DATA_D;
    c->Nl=0;
    c->Nh=0;
    c->num=0;
    return 1;
    }

#ifndef md5_block_data_order
#ifdef X
#undef X
#endif
void md5_block_data_order (MD5_CTX *c, const void *data_, size_t num)
    {
    const unsigned char *data = (const unsigned char *) data_;
    register MD5_LONG A,B,C,D,l;
#ifndef MD32_XARRAY
    /* See comment in crypto/sha/sha_locl.h for details. */
    MD5_LONG    XX0, XX1, XX2, XX3, XX4, XX5, XX6, XX7,
                XX8, XX9,XX10,XX11,XX12,XX13,XX14,XX15;
# define X(i)   XX##i
#else
    MD5_LONG XX[MD5_LBLOCK];
# define X(i)   XX[i]
#endif

    A=c->A;
    B=c->B;
    C=c->C;
    D=c->D;

    for (;num--;)
        {
    HOST_c2l(data,l); X( 0)=l;      HOST_c2l(data,l); X( 1)=l;
    /* Round 0 */
    R0(A,B,C,D,X( 0), 7,0xd76aa478L);   HOST_c2l(data,l); X( 2)=l;
    R0(D,A,B,C,X( 1),12,0xe8c7b756L);   HOST_c2l(data,l); X( 3)=l;
    R0(C,D,A,B,X( 2),17,0x242070dbL);   HOST_c2l(data,l); X( 4)=l;
    R0(B,C,D,A,X( 3),22,0xc1bdceeeL);   HOST_c2l(data,l); X( 5)=l;
    R0(A,B,C,D,X( 4), 7,0xf57c0fafL);   HOST_c2l(data,l); X( 6)=l;
    R0(D,A,B,C,X( 5),12,0x4787c62aL);   HOST_c2l(data,l); X( 7)=l;
    R0(C,D,A,B,X( 6),17,0xa8304613L);   HOST_c2l(data,l); X( 8)=l;
    R0(B,C,D,A,X( 7),22,0xfd469501L);   HOST_c2l(data,l); X( 9)=l;
    R0(A,B,C,D,X( 8), 7,0x698098d8L);   HOST_c2l(data,l); X(10)=l;
    R0(D,A,B,C,X( 9),12,0x8b44f7afL);   HOST_c2l(data,l); X(11)=l;
    R0(C,D,A,B,X(10),17,0xffff5bb1L);   HOST_c2l(data,l); X(12)=l;
    R0(B,C,D,A,X(11),22,0x895cd7beL);   HOST_c2l(data,l); X(13)=l;
    R0(A,B,C,D,X(12), 7,0x6b901122L);   HOST_c2l(data,l); X(14)=l;
    R0(D,A,B,C,X(13),12,0xfd987193L);   HOST_c2l(data,l); X(15)=l;
    R0(C,D,A,B,X(14),17,0xa679438eL);
    R0(B,C,D,A,X(15),22,0x49b40821L);
    /* Round 1 */
    R1(A,B,C,D,X( 1), 5,0xf61e2562L);
    R1(D,A,B,C,X( 6), 9,0xc040b340L);
    R1(C,D,A,B,X(11),14,0x265e5a51L);
    R1(B,C,D,A,X( 0),20,0xe9b6c7aaL);
    R1(A,B,C,D,X( 5), 5,0xd62f105dL);
    R1(D,A,B,C,X(10), 9,0x02441453L);
    R1(C,D,A,B,X(15),14,0xd8a1e681L);
    R1(B,C,D,A,X( 4),20,0xe7d3fbc8L);
    R1(A,B,C,D,X( 9), 5,0x21e1cde6L);
    R1(D,A,B,C,X(14), 9,0xc33707d6L);
    R1(C,D,A,B,X( 3),14,0xf4d50d87L);
    R1(B,C,D,A,X( 8),20,0x455a14edL);
    R1(A,B,C,D,X(13), 5,0xa9e3e905L);
    R1(D,A,B,C,X( 2), 9,0xfcefa3f8L);
    R1(C,D,A,B,X( 7),14,0x676f02d9L);
    R1(B,C,D,A,X(12),20,0x8d2a4c8aL);
    /* Round 2 */
    R2(A,B,C,D,X( 5), 4,0xfffa3942L);
    R2(D,A,B,C,X( 8),11,0x8771f681L);
    R2(C,D,A,B,X(11),16,0x6d9d6122L);
    R2(B,C,D,A,X(14),23,0xfde5380cL);
    R2(A,B,C,D,X( 1), 4,0xa4beea44L);
    R2(D,A,B,C,X( 4),11,0x4bdecfa9L);
    R2(C,D,A,B,X( 7),16,0xf6bb4b60L);
    R2(B,C,D,A,X(10),23,0xbebfbc70L);
    R2(A,B,C,D,X(13), 4,0x289b7ec6L);
    R2(D,A,B,C,X( 0),11,0xeaa127faL);
    R2(C,D,A,B,X( 3),16,0xd4ef3085L);
    R2(B,C,D,A,X( 6),23,0x04881d05L);
    R2(A,B,C,D,X( 9), 4,0xd9d4d039L);
    R2(D,A,B,C,X(12),11,0xe6db99e5L);
    R2(C,D,A,B,X(15),16,0x1fa27cf8L);
    R2(B,C,D,A,X( 2),23,0xc4ac5665L);
    /* Round 3 */
    R3(A,B,C,D,X( 0), 6,0xf4292244L);
    R3(D,A,B,C,X( 7),10,0x432aff97L);
    R3(C,D,A,B,X(14),15,0xab9423a7L);
    R3(B,C,D,A,X( 5),21,0xfc93a039L);
    R3(A,B,C,D,X(12), 6,0x655b59c3L);
    R3(D,A,B,C,X( 3),10,0x8f0ccc92L);
    R3(C,D,A,B,X(10),15,0xffeff47dL);
    R3(B,C,D,A,X( 1),21,0x85845dd1L);
    R3(A,B,C,D,X( 8), 6,0x6fa87e4fL);
    R3(D,A,B,C,X(15),10,0xfe2ce6e0L);
    R3(C,D,A,B,X( 6),15,0xa3014314L);
    R3(B,C,D,A,X(13),21,0x4e0811a1L);
    R3(A,B,C,D,X( 4), 6,0xf7537e82L);
    R3(D,A,B,C,X(11),10,0xbd3af235L);
    R3(C,D,A,B,X( 2),15,0x2ad7d2bbL);
    R3(B,C,D,A,X( 9),21,0xeb86d391L);

    A = c->A += A;
    B = c->B += B;
    C = c->C += C;
    D = c->D += D;
        }
    }
#endif

void MD5_Array(const unsigned char *d, size_t n, unsigned char *md)
{
    RR_ASSERT( md != NULL );

    MD5_CTX c;

    MD5_Init(&c);

    MD5_Update(&c,d,n);

    MD5_Final(md,&c);
    
    return;
}

U64 rrHash_MD5_64(const U8 * bytes, S32 size)
{
    //unsigned char m[MD5_DIGEST_LENGTH];
    U32 md5[4];
    //U64 md5[2];
    RR_COMPILER_ASSERT( sizeof(md5) == MD5_DIGEST_LENGTH );
    
    MD5_Array(bytes,size,(unsigned char *)md5);
    
    /*
    // how best to put 'md5' into ret ?
    U64 ret = (md5[0]<<32) | (md5[1]);
    ret += md5[2] * 0x1EF57D8A7B344E7BULL;
    ret += md5[3] * 0xD9EA571C8AF880B6ULL;
    */
    
    U32 a = md5[0], b = md5[1], c = md5[2];
    rrMix3(a,b,c);
    a += md5[3];
    rrFinalMix3(a,b,c);
    
    U64 ret = ((U64)a<<32) + ((U64)b<<24) + (U64)c;
    
    return ret;
}

//===============================================================

#endif //-------------------


// don't call these, they're just exposed for test :
/*

U64 rrHash_SHA1_64(const U8 * bytes, S32 size);

U64 rrHash_MD5_64(const U8 * bytes, S32 size);

U64 rrHash_BurtleBurtle_64(const U8 * bytes, S32 size);

U64 rrHash_Cessu_64(const U8 * bytes, S32 size);
*/


OODLE_NS_END
