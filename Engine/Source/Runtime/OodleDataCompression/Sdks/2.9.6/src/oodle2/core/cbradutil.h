// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_CBRADUTIL_H__
#define __RADRR_CBRADUTIL_H__

#include "rrbase.h"
#include "rrmemutil.h"
#include "rrmath.h"
#include "oodlecore.h"

OODLE_NS_START

//---------------------------------------------------------------------------

#define LOOP(var,count)	(int var=0;(var)<(count);var++)
#define LOOPBACK(var,count)	(int var=(count)-1;(var)>=0;var--)
#define LOOPVEC(var,vec)    (int var=0; (var) < (int)vec.size(); var++)
#define LOOPVECBACK(var,vec)    (int var= (int)vec.size() -1; (var)>=0; var--)
#define LOOPARRAY(var,arr)    (int var=0; (var) < (int)RR_ARRAY_SIZE(arr); var++)

//---------------------------------------------------------------------------

RADINLINE U32 S32_Square(S32 x)
{
	return x*x;
}

RADINLINE F32 F32_Square(F32 x)
{
	return x*x;
}

//---------------------------------------------------------------------------

static RADINLINE float fsquare(float x) { return x*x; }
static RADINLINE double fsquare(double x) { return x*x; }

static RADINLINE bool fequal(float f1,float f2,float epsilon)
{
	return RR_ABS(f1-f2) < epsilon;
}
static RADINLINE bool fequal(double f1,double f2,double epsilon)
{
	return RR_ABS(f1-f2) < epsilon;
}

//---------------------------------------------------------------------------

// don't use stuff like 1UL<< because "UL" is platform dependent!
#define RR_BIT_FLAG_U32(num)    ( (((U32)1)<<(num)) )
#define RR_BIT_MASK_U32(num)    ( RR_BIT_FLAG_U32(num) - 1 )

//=======================================

#if 0
// U32 bit allocator :

int RADFORCEINLINE U32_TopBitIndex( register U32 r )
{
	RR_ASSERT( r != 0 );
	U32 ret = 31 - rrClz32(r);
	RR_ASSERT( r & (RR_ONE_U32<<ret) );
	RR_ASSERT( r >= (RR_ONE_U32<<ret) );
	RR_ASSERT( r < (RR_ONE_U32<<(ret+1)) || ret == 31 );
	return (int) ret;
}

int RADFORCEINLINE U32_AllocateSlot( U32 & set )
{
	U32 inverse = ~set;
	RR_ASSERT( inverse != 0 ); // no slots free!
	int index = U32_TopBitIndex(inverse);
	set |= (RR_ONE_U32<<index);
	return index;
}

void RADFORCEINLINE U32_FreeSlot( U32 & set, int slot )
{
	RR_ASSERT( set & (RR_ONE_U32<<slot) );
	set ^= (RR_ONE_U32<<slot);
}

int RADFORCEINLINE U32_GetNextSlot( U32 & set )
{
	RR_ASSERT( set != 0 );
	//int slot = TopBitIndex(set);
	int slot = U32_TopBitIndex(set);
	// turn off bit :
	set ^= (RR_ONE_U32<<slot);
	return slot;
}

#endif

//---------------------------------------------------------------------------
// Safe conversions :

RADFORCEINLINE S32 rrS32ClampGEZero(S32 val)
{
	// if negative, signMask = FFFF
	U32 signMask = (U32)(val>>31);
	return val & (~signMask);
}

// these could easily be a template ; see also check_value_cast

OOINLINE U8 U8_clamp(U32 i)	{ return (U8) RR_CLAMP_U8(i); }
OOINLINE U8 U8_clamp(S32 i)	{ return (U8) RR_CLAMP_U8(i); }
OOINLINE U8 U8_check(U32 i)	{ U8 ret = (U8) i; RR_ASSERT( (U32)ret == i ); return ret; }
OOINLINE U8 U8_check(S32 i)	{ U8 ret = (U8) i; RR_ASSERT( (S32)ret == i ); return ret; }

OOINLINE S8 S8_clamp(U32 i)	{ return (S8) RR_MIN(i,127); }
OOINLINE S8 S8_clamp(S32 i)	{ return (S8) RR_CLAMP(i,-128,127); }
OOINLINE S8 S8_check(U32 i)	{ S8 ret = (S8) i; RR_ASSERT( (U32)ret == i ); return ret; }
OOINLINE S8 S8_check(S32 i)	{ S8 ret = (S8) i; RR_ASSERT( (S32)ret == i ); return ret; }

OOINLINE U16 U16_clamp(U32 i)	{ return (U16) RR_CLAMP_U16(i); }
OOINLINE U16 U16_clamp(S32 i)	{ return (U16) RR_CLAMP_U16(i); }
OOINLINE U16 U16_check(U32 i)	{ U16 ret = (U16) i; RR_ASSERT( (U32)ret == i ); return ret; }
OOINLINE U16 U16_check(S32 i)	{ U16 ret = (U16) i; RR_ASSERT( (S32)ret == i ); return ret; }
OOINLINE U16 U16_checkA(SINTa i)	{ U16 ret = (U16) i; RR_ASSERT( (SINTa)ret == i ); return ret; }

OOINLINE S16 S16_clamp(U32 i)	{ return (S16) RR_MIN(i,32767); }
OOINLINE S16 S16_clamp(S32 i)	{ return (S16) RR_CLAMP(i,-32768,32767); }
OOINLINE S16 S16_check(U32 i)	{ S16 ret = (S16) i; RR_ASSERT( (U32)ret == i ); return ret; }
OOINLINE S16 S16_check(S32 i)	{ S16 ret = (S16) i; RR_ASSERT( (S32)ret == i ); return ret; }

OOINLINE U32 U32_clamp(U64 i)	{ return (U32) RR_CLAMP(i,0,(U32)-1); }
OOINLINE U32 U32_clamp(S64 i)	{ return (U32) RR_CLAMP(i,0,(U32)-1); }
OOINLINE U32 U32_check(U64 i)	{ U32 ret = (U32) i; RR_ASSERT( ret == i ); return ret; }
OOINLINE U32 U32_check(S64 i)	{ U32 ret = (U32) i; RR_ASSERT( (S64)ret == i ); return ret; }

OOINLINE S32 S32_check(U64 i)	{ S32 ret = (S32)i; RR_ASSERT( (U64)ret == i ); return ret; }
OOINLINE S32 S32_check(S64 i)	{ S32 ret = (S32) i; RR_ASSERT( (S64)ret == i ); return ret; }
OOINLINE S32 S32_clamp(U64 i)	{ return (S32) RR_MIN(i,2147483647); }
OOINLINE S32 S32_clamp(S64 i)	{ return (S32) RR_CLAMP(i,(-2147483647 -1),2147483647); }

OOINLINE S32 S32_checkA(UINTa i)	{ S32 ret = (S32)i; RR_ASSERT( (UINTa)ret == i ); return ret; }
OOINLINE S32 S32_checkA(SINTa i)	{ S32 ret = (S32) i; RR_ASSERT( (SINTa)ret == i ); return ret; }
OOINLINE S32 S32_clampA(UINTa i)	{ return (S32) RR_MIN(i,2147483647); }
OOINLINE S32 S32_clampA(SINTa i)	{ return (S32) RR_CLAMP(i,(-2147483647 -1),2147483647); }

OOINLINE U32 U32_checkA(UINTa i)	{ U32 ret = (U32)i; RR_ASSERT( (UINTa)ret == i ); return ret; }
OOINLINE U32 U32_checkA(SINTa i)	{ U32 ret = (U32) i; RR_ASSERT( (SINTa)ret == i ); return ret; }

OOINLINE UINTr UINTr_checkA(UINTa i)	{ UINTr ret = (UINTr)i; RR_ASSERT( (UINTa)ret == i ); return ret; }
OOINLINE SINTr UINTr_checkA(SINTa i)	{ SINTr ret = (SINTr)i; RR_ASSERT( (SINTa)ret == i ); return ret; }

RR_COMPILER_ASSERT( sizeof(UINTr) >= sizeof(UINTa) );
// UINTr is always >= UINTa
//OOINLINE UINTr R_clampA(UINTa i)	{ return (UINTr) RR_MIN(i,RR_UINTR_MAX); }
//OOINLINE SINTr R_clampA(SINTa i)	{ return (SINTr) RR_CLAMP(i,RR_SINTR_MIN,RR_SINTR_MAX); }

// not sure how I feel about these :

OOINLINE S8  rrSigned(U8 x)  { return (S8) x; }
OOINLINE S16 rrSigned(U16 x) { return (S16) x; }
OOINLINE S32 rrSigned(U32 x) { return (S32) x; }
OOINLINE S64 rrSigned(U64 x) { return (S64) x; }

OOINLINE U8  rrUnsigned(S8 x)  { return (U8) x; }
OOINLINE U16 rrUnsigned(S16 x) { return (U16) x; }
OOINLINE U32 rrUnsigned(S32 x) { return (U32) x; }
OOINLINE U64 rrUnsigned(S64 x) { return (U64) x; }

OOINLINE S8  rrSigned(S8 x)  { return x; }
OOINLINE S16 rrSigned(S16 x) { return x; }
OOINLINE S32 rrSigned(S32 x) { return x; }
OOINLINE S64 rrSigned(S64 x) { return x; }

OOINLINE U8  rrUnsigned(U8 x)  { return x; }
OOINLINE U16 rrUnsigned(U16 x) { return x; }
OOINLINE U32 rrUnsigned(U32 x) { return x; }
OOINLINE U64 rrUnsigned(U64 x) { return x; }

OOINLINE SINTa rrSignedA(UINTa x) { return (SINTa) x; }
OOINLINE UINTa rrUnsignedA(SINTa x)  { return (UINTa) x; }
OOINLINE SINTr rrSignedR(UINTr x) { return (SINTr) x; }
OOINLINE UINTr rrUnsignedR(SINTr x)  { return (UINTr) x; }

// bleh I want void <-> T* to be warningless
/*
OOINLINE void * rrVoidCast(U8 * ptr) { return (void *) ptr; }
OOINLINE const void * rrVoidCast(const U8 * ptr) { return (const void *) ptr; }
*/
OOINLINE U8 * U8_void(void * ptr) { return (U8 *) ptr; }
OOINLINE const U8 * U8_void(const void * ptr) { return (const U8 *) ptr; }

OOINLINE char * char_void(void * ptr) { return (char *) ptr; }
OOINLINE const char * char_void(const void * ptr) { return (const char *) ptr; }

//---------------------------------------------------------------------------

// Min for mixed types so you don't have to cast
//	and they are correct on excess cases

OOINLINE S32 S32_Min(S32 a,S32 b) { return RR_MIN(a,b); }

OOINLINE S32 S32_Min(S32 a,U32 b)
{
	if ( a <= 0 ) return a;
	U32 umin = RR_MIN( (U32)a, b );
	return (S32) umin;
}

OOINLINE S32 S32_Min(S32 a,S64 b) 
{
	S64 m = RR_MIN( (S64)a, b);
	S32 r = (S32) m;
	RR_ASSERT( m == r ); // if b < -INT_MAX is bad
	return r;
}

OOINLINE S32 S32_Min(S32 a,U64 b) 
{
	if ( a <= 0 ) return a;
	U64 m = RR_MIN( (U64)a, b);
	S32 r = (S32) m;
	RR_ASSERT( m == (U64)r ); // if b < -INT_MAX is bad
	return r;
}

//---------------------------------------------------------------------------

#define FReadOk(fp,bytes,count)		( (size_t)(count) == fread(bytes,1,count,fp) )
#define FWriteOk(fp,bytes,count)	( (size_t)(count) == fwrite(bytes,1,count,fp) )

//---------------------------------------------------------------------------
// stuff for C++ classes :

//! Disallows the compiler defined copy ctor
#define RR_FORBID_COPY_CTOR(x)    x(const x&)

//! Disallows the compiler defined assignment operator
#define RR_FORBID_ASSIGNMENT(x)   void operator=(const x&)

#define RR_FORBID_CLASS_STANDARDS(x)	\
	RR_FORBID_ASSIGNMENT(x);	\
	RR_FORBID_COPY_CTOR(x)	

//-----------------------------------------------------------------------------------------------

// ScopedSet :
//	set a value in a scope
//	 and restore it to previous value on scope exit
template <typename t_type>
class ScopedSet
{
public:
	ScopedSet(t_type * ptr,const t_type & val) :
		m_ptr(ptr),m_previous(*ptr)
	{
		*m_ptr = val;
	}
	~ScopedSet()
	{
		*m_ptr = m_previous;
	}
private:
	t_type * m_ptr;
	const t_type m_previous;
	void operator = (const ScopedSet<t_type> & other);
};

//-------------------------------------------------------------------
// crazy macros : DO_ONCE and AT_STARTUP

// Macro for static-initialization one-liners.
// !! Only works in statically-linked .obj files. !!
#define RR_STATIC_AT_STARTUP(some_code)	\
namespace { static struct RR_STRING_JOIN(AtStartup_,__LINE__) { RR_STRING_JOIN(AtStartup_,__LINE__)() { some_code; } } RR_STRING_JOIN( RR_NUMBERNAME(AtStartup_) , Instance ); };

#define RR_STATIC_AT_SHUTDOWN(some_code)	\
namespace { static struct RR_STRING_JOIN(AtShutdown_,__LINE__) { ~RR_STRING_JOIN(AtShutdown_,__LINE__)() { some_code; } } RR_STRING_JOIN(  RR_NUMBERNAME(AtShutdown_) , Instance ); };

#define RR_STATIC_DO_ONCE(some_code)	\
do { static bool once = true; if ( once ) { once = false; { some_code; } } } while(0)

#define RR_STATIC_DO_N_TIMES(count,some_code)	\
do { static int counter = 0; if ( counter++ < (count) ) { some_code; } } while(0)

//-------------------------------------------------------------------

/*
#define RR_LOOP(var,count)	for(int var=0;(var)<(int)(count);var++)
#define RR_LOOPBACK(var,count)	for(int var=(int)(count)-1;(var)>=0;var--)
*/


#ifdef __RADLITTLEENDIAN__
typedef U16 U16LE;
typedef U32 U32LE;
#elif defined(__RADBIGENDIAN__)
RR_PACKED_STRUCT_START(U16LE)
{
private:
	U16	m_data;
public:
	operator U16 () const { return RR_GET16_LE_UNALIGNED(&(m_data)); }
} RR_PACKED_STRUCT_END;

RR_PACKED_STRUCT_START(U32LE)
{
private:
	U32	m_data;
public:
	operator U32 () const { return RR_GET32_LE_UNALIGNED(&(m_data)); }
} RR_PACKED_STRUCT_END;
#else
#error WTF NO ENDIAN
#endif // ENDIAN

//-------------------------------------------------------------------

// Half-open integer interval represented such that containment tests can be done
// with a single conditional branch, using unsigned arithmetic wrap-around.
//
// Compilers know to do this for intervals [a,b) where a, b are constants, but
// they can't generally do this when either of the values is variable. We can
// just be careful when constructing intervals.
struct HalfOpenS32Interval
{
	// Stored as U32s since we use unsigned arithmetic to do the testing.
	// The wraparound is crucial.
	U32 base;
	U32 len;

	// Sets up an empty interval
	HalfOpenS32Interval()
		: base(0), len(0)
	{
	}

	// Builds the interval [lo,hi)
	// When lo >= hi, this gives an empty interval.
	HalfOpenS32Interval(S32 lo, S32 hi)
		: base(0), len(0)
	{
		if (lo < hi)
		{
			base = static_cast<U32>(lo);
			len = static_cast<U32>(hi) - base;
		}
	}

	bool contains(S32 x) const
	{
		return (static_cast<U32>(x) - base) < len;
	}
};

//-------------------------------------------------------------------

OODLE_NS_END

#endif // __RADRR_CBRADUTIL_H__
