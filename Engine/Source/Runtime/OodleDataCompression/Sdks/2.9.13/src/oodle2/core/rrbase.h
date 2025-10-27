// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.
#pragma once

#ifndef __RADRR_BASEH__
#define __RADRR_BASEH__

#include "rrplatform.h"

//-------------------------------------------------
// Exported functions

#ifndef OODLLEXPORT // Platforms can define this on their own, but if they don't, use defaults
	#if defined(_WIN32) || defined(_MSC_VER) // Windows or MS compilers
		#define OODLLEXPORT __declspec(dllexport)
	#elif defined(__GNUC__) || defined(__clang__) // GNU compatible
		#define OODLLEXPORT __attribute__((visibility("default")))
	#else
		#define OODLLEXPORT
	#endif
#endif

// @@ CB : RADLINK is deprecated and should go away
//	RADLINK is internally only for callbacks
//	RADLINK is NOT related to OOEXPLINK or OOFUNC2
#ifndef RADLINK // Again, platforms can define this on their own if they want
	#define RADLINK
#endif

// OOLINK is only used for OODLE_CALLBACK

// OOEXPLINK == OOFUNC2 is how exported functions are linked
// OOEXPFUNC == OOFUNC1 is for exported functions
//		OOEXPFUNC always contains OODEFFUNC == "extern C"
//		if building DLL, OOEXPFUNC is also a DLL export
//	as of 2.9.0 refactor
//	 OOFUNC1 is NOT DLL import even when using the Oodle DLL's
//	 it is always lib import (which will use the DLL via import lib)

//---------------------------

// previously OOEXPFUNC was defined to OODEFFUNC
#undef OOEXPFUNC

// exactly one of OODLE_BUILDING_ or OODLE_IMPORT_ must be set
// @@ OODLE_IMPORT_ is no longer used for anything, so we could relax that requirement
#ifdef OODLE_BUILDING_DLL
    #define OOEXPFUNC OODEFFUNC OODLLEXPORT
        
    #if defined(OODLE_BUILDING_LIB) || defined(OODLE_IMPORT_LIB) || defined(OODLE_IMPORT_DLL)
		#error multiple OODLE_BUILDING or OODLE_IMPORT defines
	#endif
#elif defined(OODLE_BUILDING_LIB)
    #define OOEXPFUNC OODEFFUNC
        
    #if defined(OODLE_BUILDING_DLL) || defined(OODLE_IMPORT_LIB) || defined(OODLE_IMPORT_DLL)
		#error multiple OODLE_BUILDING or OODLE_IMPORT defines
	#endif
#elif defined(OODLE_IMPORT_LIB)
    #define OOEXPFUNC OODEFFUNC
        
    #if defined(OODLE_BUILDING_LIB) || defined(OODLE_BUILDING_DLL) || defined(OODLE_IMPORT_DLL)
		#error multiple OODLE_BUILDING or OODLE_IMPORT defines
	#endif
#elif defined(OODLE_IMPORT_DLL)
	#error	import dll should not see OodleCore.h
		
    #if defined(OODLE_BUILDING_LIB) || defined(OODLE_BUILDING_DLL) || defined(OODLE_IMPORT_LIB)
		#error multiple OODLE_BUILDING or OODLE_IMPORT defines
	#endif
#else
	#error	no Oodle usage define set
#endif

//-------------------------------------------------
// Aliases for old radrtl names

#define RADDEFFUNC OODEFFUNC
#define RADDEFSTART OODEFSTART
#define RADDEFEND OODEFEND
#define RADDEFAULT(val) OODEFAULT(val)

// RADEXPFUNC & RADEXPLINK are not used by Oodle anywhere
//	but are used if you try to mix in other radtypes.h products
#define RADEXPFUNC OOEXPFUNC
#define RADEXPLINK OOEXPLINK

// don't include radtypes.h :
#define __RADTYPESH__

#define RR_COMPILER_ASSERT OO_COMPILER_ASSERT
#define RADINLINE OOINLINEFUNC
#define RADSTRUCT OOSTRUCT

#if defined(__RADMACAPI__)
// On Darwin targets, we end up with S32=int,
// S64=long long, intptr_t=long, so overloads for
// 32/64 bit types don't cover intptrs and we need
// a separate overload for them if we want to cover
// all bases.
#define RR_INTA_TYPES_SEPARATE
#endif

// Platforms can override this, but this is the default for everything we compile on
#ifndef RADRESTRICT
#define RADRESTRICT __restrict
#endif

#if defined(_MSC_VER) && defined(__RADX86__) // for rotates
#include <intrin.h>
#endif
	
#ifdef __GNUC_MINOR__
	// make a combined GCC version for testing :
	#define __RAD_GCC_VERSION__ (__GNUC__ * 10000 \
						   + __GNUC_MINOR__ * 100 \
						   + __GNUC_PATCHLEVEL__)

	  /* Test for GCC > 3.2.0 */
	  // #if GCC_VERSION > 30200
#endif

#define RAD_STATEMENT_WRAPPER(exp)	do { exp } while(0)

//-------------------------------------------------
// CPU feature detection.
//
// Platforms can define these based on their CPU type if they're targeting
// a specific uArch:
//
//   __RADCORTEXA57__ for ARM Cortex-A57 CPUs
//   __RADJAGUAR__ for AMD Jaguar CPUs
//   __RADZEN2__ for AMD Zen 2 CPUs

#ifdef __RADX86__
	#ifdef __RADIPHONE__
		// x86 iphone happens for the "Simulator" build, no SSE there
	#else
		#define __RADSSE2__
	#endif
#endif

// We already set __RADNEON__ for A64 state, otherwise check the usual defines:
#if defined(__RADARM__) && (defined(__ARM_NEON__) || defined(__ARM_NEON) || defined(__RADWINRT__))
	#define __RADNEON__
#endif

#if defined(__RADWASM__) && defined(__wasm_simd128__)
	#define __RADWASM_SIMD128__
#endif

//-------------------------------------------------
// UINTr = int the size of a register

#ifdef __RAD64REGS__

	#define RAD_UINTr U64
	#define RAD_SINTr S64

	#define RAD_REGBITS 64
	#define RAD_REGBYTES 8
	
#else

	#define RAD_UINTr U32
	#define RAD_SINTr S32

	#define RAD_REGBITS 32
	#define RAD_REGBYTES 4
	
#endif

typedef RAD_SINTr SINTr;
typedef RAD_UINTr UINTr;
  
//-------------------------------------------------
// RAD_PTRBITS and such defined here without using sizeof()
//   so that they can be used in align() and other macros

#ifdef __RAD64__

	#define RAD_PTRBITS 64
	#define RAD_PTRBYTES 8
	#define RAD_TWOPTRBYTES 16

#else

	#define RAD_PTRBITS 32
	#define RAD_PTRBYTES 4
	#define RAD_TWOPTRBYTES 8

#endif

//===========================================================================
// RR_UINTa3264 is a U64 in 64-bit code and a U32 in 32-bit code
//	eg. it's pointer sized and the same type as a U32/U64 of the same size
// it's the same as UINTa but useful in some funny situations
//	like selecting a function overload

#ifdef __RAD64__
#define RR_UINTa3264	U64
#else
#define RR_UINTa3264	U32
#endif

//===========================================================================

// RADASSUME(expr) tells the compiler that expr is always true
// RADUNREACHABLE must never be reachable - even in event of error
//  eg. it's okay for compiler to generate completely invalid code after RADUNREACHABLE

#ifdef _MSC_VER
	#define RADFORCEINLINE __forceinline
	#define RADNOINLINE __declspec(noinline)
	#define RADUNREACHABLE __assume(0)
	#define RADASSUME(exp) __assume(exp)
#elif defined(__clang__)
	#ifdef _DEBUG
	#define RADFORCEINLINE inline
	#else
	#define RADFORCEINLINE inline __attribute__((always_inline))
	#endif
	#define RADNOINLINE __attribute__((noinline))

	#define RADUNREACHABLE __builtin_unreachable()
	#if __has_builtin(__builtin_assume)
	  #define RADASSUME(exp) __builtin_assume(exp)
	#else
	  #define RADASSUME(exp)
	#endif

#elif defined(__GNUC__)
	#ifdef _DEBUG
	#define RADFORCEINLINE inline
	#else
	#define RADFORCEINLINE inline __attribute__((always_inline))
	#endif
	#define RADNOINLINE __attribute__((noinline))

	#if __RAD_GCC_VERSION__ >= 40500
	#define RADUNREACHABLE __builtin_unreachable()
	#define RADASSUME(exp)  RAD_STATEMENT_WRAPPER( if ( ! (exp) ) __builtin_unreachable(); )
	#else
	#define RADUNREACHABLE do { RR_BREAK(); } while(1)
	#define RADASSUME(exp)
	#endif
#else
	// ? #define RADFORCEINLINE ?
	#define RADFORCEINLINE inline
	#define RADNOINLINE
	#define RADASSUME(x) (void)0
#endif

//===========================================================================

// Must be placed after variable declarations for code compiled as .c
#if defined(_MSC_VER) && _MSC_VER >= 1600 // in 2010 aka 10.0 and later 
  #define RR_UNUSED_VARIABLE(x) (void) (x)
#else
  #define RR_UNUSED_VARIABLE(x) (void)(sizeof(x))
#endif

//--------------------------------------------------
// Range macros

#ifndef RR_MIN
	#define RR_MIN(a,b)    ( (a) < (b) ? (a) : (b) )
#endif

#ifndef RR_MAX
	#define RR_MAX(a,b)    ( (a) > (b) ? (a) : (b) )
#endif

#ifndef RR_ABS
	#define RR_ABS(a)      ( ((a) < 0) ? -(a) : (a) )
#endif

#ifndef RR_CLAMP
	#define RR_CLAMP(val,lo,hi) RR_MAX( RR_MIN(val,hi), lo )
#endif

//--------------------------------------------------

// Platforms can override RR_BREAK and RR_CACHE_LINE_SIZE, but if they're
// not set, just provide sane defaults

#ifndef RR_CACHE_LINE_SIZE
	#define RR_CACHE_LINE_SIZE 64 // NOTE(fg): true on everything we ship as of March 2021
#endif

#ifndef RR_BREAK
	#ifdef _MSC_VER
		#define RR_BREAK() __debugbreak()
	#else
		#define RR_BREAK() __builtin_trap()
	#endif
#endif

//===========================================================================

#ifdef OODLE_NNS // NNS = No Name Space

	#define OO2NET_NS		oo2
	#define OO2TEX_NS		oo2
	#define OO2TEXRT_NS		oo2
	#define OO2_NS			oo2

#else

	#define OO2NET_NS		oo2net
	#define OO2TEX_NS		oo2tex
	#define OO2TEXRT_NS		oo2texrt
	#define OO2_NS			oo2
	
#endif

#ifdef OODLE_BUILDING_NETWORK

    #define RR_NAMESPACE       OO2NET_NS
    
#elif defined(OODLE_BUILDING_TEXTURE)

    #define RR_NAMESPACE       OO2TEX_NS

#elif defined(OODLE_BUILDING_TEXTURERT)

    #define RR_NAMESPACE       OO2TEXRT_NS

#else
    
    // assume no "BUILDING" means DATA    
    #ifndef OODLE_BUILDING_DATA
    #define OODLE_BUILDING_DATA    
    #endif
    
    #define RR_NAMESPACE       OO2_NS

#endif
    
#define RR_NAMESPACE_START namespace RR_NAMESPACE {
#define RR_NAMESPACE_END   }
#define RR_NAMESPACE_PRE	::RR_NAMESPACE::
//;
//#define RR_NAMESPACE_USE   using namespace RR_NAMESPACE;

//=================================================================
// simple RR_ASSERT :

// CB 5-27-10 : use RR_DO_ASSERTS to toggle asserts on and off :
#if (defined(_DEBUG) && !defined(NDEBUG)) || defined(ASSERT_IN_RELEASE)
  #define RR_DO_ASSERTS
#endif

/*********

rrAsserts :

RR_ASSERT(exp) - the normal assert thing, toggled with RR_DO_ASSERTS
RR_ASSERT_ALWAYS(exp) - assert that you want to test even in ALL builds (including final!)
RR_ASSERT_LITE(exp) - normal assert is not safe from threads or inside malloc; use this instead
RR_DURING_ASSERT(exp) - wrap operations that compute stuff for assert in here
RR_DO_ASSERTS - toggle tells you if asserts are enabled or not

RR_BREAK() - generate a debug break - always !
RR_ASSERT_BREAK() - RR_BREAK for asserts ; disable with RAD_NO_BREAK

RR_ASSERT_FAILURE(str)  - just break with a messsage; like assert with no condition
RR_ASSERT_FAILURE_ALWAYS(str)  - RR_ASSERT_FAILURE in release builds too
RR_CANT_GET_HERE() - put in spots execution should never go
RR_COMPILER_ASSERT(exp) - checks constant conditions at compile time

*************/

//-----------------------------------------------------------

RR_NAMESPACE_START

// rrDisplayAssertion might just log, or it might pop a message box, depending on settings
//  rrDisplayAssertion returns whether you should break or not
rrbool rrDisplayAssertion(const char * fileName,const int line,const char * function,const char * message);

//-----------------------------------------------------------

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
	// C99
	#define RR_FUNCTION_NAME __func__
#elif defined(__GNUG__) || defined(__GNUC__) || defined(_MSC_VER)
	// newer standard is __func__ instead of __FUNCTION__
	// this is const string literal, eg. "func" not func
	#define RR_FUNCTION_NAME __FUNCTION__
#else
	#define RR_FUNCTION_NAME "func"
#endif

//-----------------------------------------------------------
      
// RAD_NO_BREAK : option if you don't like your assert to break
//  CB : RR_BREAK is *always* a break ; RR_ASSERT_BREAK is optional
#ifdef RAD_NO_BREAK
#define RR_ASSERT_BREAK() 0
#else
#define RR_ASSERT_BREAK()   RR_BREAK()
#endif

//  assert_always is on FINAL !
//#define RR_ASSERT_ALWAYS(exp)      RAD_STATEMENT_WRAPPER( static int Ignored=0; if ( ! (exp) ) { if ( rrDisplayAssertion(&Ignored,__FILE__,__LINE__,RR_FUNCTION_NAME,#exp) ) RR_ASSERT_BREAK(); } )
#define RR_ASSERT_ALWAYS(exp)      do { if ( ! (exp) ) { if ( RR_NAMESPACE_PRE rrDisplayAssertion(__FILE__,__LINE__,RR_FUNCTION_NAME,#exp) ) RR_ASSERT_BREAK(); } } while(0)

// RR_ASSERT_FAILURE is like an assert without a condition - if you hit it, you're bad
//#define RR_ASSERT_FAILURE_ALWAYS(str)   RAD_STATEMENT_WRAPPER( static int Ignored=0; if ( rrDisplayAssertion(&Ignored,__FILE__,__LINE__,RR_FUNCTION_NAME,str) ) RR_ASSERT_BREAK(); )
#define RR_ASSERT_FAILURE_ALWAYS(str)           do { if ( RR_NAMESPACE_PRE rrDisplayAssertion(__FILE__,__LINE__,RR_FUNCTION_NAME,str) ) RR_ASSERT_BREAK(); } while(0)

#define RR_ASSERT_LITE_ALWAYS(exp)     RAD_STATEMENT_WRAPPER( if ( ! (exp) ) { RR_ASSERT_BREAK(); } )
	
//-----------------------------------
#ifdef RR_DO_ASSERTS 

#define RR_ASSERT(exp)           RR_ASSERT_ALWAYS(exp)
#define RR_ASSERT_LITE(exp)      RR_ASSERT_LITE_ALWAYS(exp)
// RR_DURING_ASSERT is to set up expressions or declare variables that are only used in asserts
#define RR_DURING_ASSERT(exp)   exp

#define RR_ASSERT_FAILURE(str)  RR_ASSERT_FAILURE_ALWAYS(str)

// RR_CANT_GET_HERE is for like defaults in switches that should never be hit
#define RR_CANT_GET_HERE()      RAD_STATEMENT_WRAPPER( RR_ASSERT_FAILURE("can't get here"); RADUNREACHABLE; )


#else // RR_DO_ASSERTS //-----------------------------------

#define RR_ASSERT(exp)           (void)0
#define RR_ASSERT_LITE(exp)      (void)0

#define RR_DURING_ASSERT(exp)    (void)0

#define RR_ASSERT_FAILURE(str)   (void)0

#define RR_CANT_GET_HERE()		RADUNREACHABLE

#endif // RR_DO_ASSERTS //-----------------------------------

//===================================================================

//--------------------------------------------------

// RR_LINESTRING is the current line number as a string
#define RR_STRINGIZE( L )         #L
#define RR_DO_MACRO( M, X )       M(X)
#define RR_STRINGIZE_DELAY( X )   RR_DO_MACRO( RR_STRINGIZE, X )
#define RR_LINESTRING             RR_STRINGIZE_DELAY( __LINE__ )

#define RR_CAT(X,Y)                 X ## Y

//===================================================================

// RR_STRING_JOIN joins strings in the preprocessor and works with LINESTRING
#define RR_STRING_JOIN(arg1, arg2)              RR_STRING_JOIN_DELAY(arg1, arg2)
#define RR_STRING_JOIN_DELAY(arg1, arg2)        RR_STRING_JOIN_IMMEDIATE(arg1, arg2)
#define RR_STRING_JOIN_IMMEDIATE(arg1, arg2)    arg1 ## arg2

// RR_NUMBERNAME is a macro to make a name unique, so that you can use it to declare
//    variable names and they won't conflict with each other
// using __LINE__ is broken in MSVC with /ZI , but __COUNTER__ is an MSVC extension that works

#ifdef _MSC_VER
  #define RR_NUMBERNAME(name) RR_STRING_JOIN(name,__COUNTER__)
#else
  #define RR_NUMBERNAME(name) RR_STRING_JOIN(name,__LINE__)
#endif

//===================================================================

#ifdef _MSC_VER
	#pragma warning( disable : 4103) // pragma changed by header	
	#pragma pack(8)

    // This warning is completely useless.  Turn it off in all files...
    #pragma warning( disable : 4514) // unreferenced inline function removed.
    #pragma warning( disable : 4505) // unreferenced local function has been removed

    // This security warning could be useful, but we get it on some non-useful cases, like fopen()
    #pragma warning( disable : 4996) // deprecated insecure crt call

    #pragma warning( disable : 4100) // unreferenced formal parameter
    #pragma warning( disable : 4127) // conditional expression is constant

    #pragma warning( disable : 28251) // inconsistent annotation
    #pragma warning( disable : 26812) // prefer enum class
    //#pragma warning( disable : 26451) // overflow
     
      /**
      NOTEZ : cdep disables all of these :   

      -wd4100 
      -wd4127
      -wd4201 
      -wd4512
      -wd4740       
      -wd4748 
      -wd4800 
      
      **/
#endif

//===========================================================================

#ifdef _MSC_VER
	#define RR_PRAGMA_MESSAGE(str)     message( __FILE__ "(" RR_LINESTRING ") : message: " str)
	// use like #pragma RR_PRAGMA_MESSAGE(str) , no semicolon
	// this formatting makes it clickable in the VC output window
#else
	#define RR_PRAGMA_MESSAGE(str) 
#endif

//#ifdef _DEBUG
#define RR_NO_DEFAULT_CASE default: RR_CANT_GET_HERE(); break;
//#else
// is it better for this to just be blank in release?
//	does the compiler handle __assume(0) well ?
//#define RR_NO_DEFAULT_CASE 
//#endif

// check a pointer for natural alignment :
#define RR_ASSERT_ALIGNED(ptr) RR_ASSERT( (((UINTa)(ptr)) & (sizeof(*(ptr))-1)) == 0 )

//===========================================================================

// probably s.b: RAD_DECLARE_ALIGNED(type, name, alignment)
#if defined(_MSC_VER) || defined(__RADWINRTAPI__)
	#define RAD_ALIGN(type,var,num) type __declspec(align(num)) var
#else
	#define RAD_ALIGN(type,var,num) type __attribute__ ((aligned (num))) var
#endif

// WARNING : RAD_TLS should really only be used for debug/tools stuff
//	it's not reliable because even if we are built as a lib, our lib can
//	be put into a DLL and then it doesn't work
#ifndef RAD_TLS
	#if defined(__RADWIN__)
		#ifndef __RADINDLL__
			// note that you can't use this in windows DLLs
			#define RAD_TLS(type,var)   __declspec(thread) type var
		#endif
	#elif defined(__RADLINUX__) || defined(__RADANDROID__)
		#define RAD_TLS(type,var) __thread type var
	#else
		// we don't define RAD_TLS on mac (__thread not supported on mac). Will use OodleTLS instead. 
		// RAD_TLS not defined
	#endif
#endif


//===========================================================================

// ugly: no stdlib but I still want NULL
#ifndef NULL
#define NULL    (0)
#endif

#define sizeof32(obj)	((S32)sizeof(obj))
#define sizeofA(x)		((SINTa)sizeof(x))

//===========================================================================

// RR_CLAMP32_0_N : use unsigned compare to check if its in range
//  then one more to see what side of range it's off
// another way would be since hi is usually a bit mask : 
// if x already was unsigned, the compare against zero should get removed by the optimizer :
#define RR_CLAMP32_0_N(x,hi)    ( ( ((U32)(x)) > (hi) ) ? ( ( (x) < 0 ) ? 0 : (hi) ) : (x) )
#define RR_CLAMPR_0_N(x,hi)    ( ( ((UINTr)(x)) > (hi) ) ? ( ( (x) < 0 ) ? 0 : (hi) ) : (x) )
// this can be faster if x is actually signed (use signed shift to make mask)
//#define RR_CLAMP32_0_N(x,hi)  ( ( ((U32)(x)) > (hi) ) ? ( (~((x)>>31)) & (hi) )  : (x) )

#define RR_CLAMP_255(x) RR_CLAMPR_0_N(x,255)

#define RR_CLAMP_U8(x)  (U8)RR_CLAMPR_0_N(x,0xFF)
#define RR_CLAMP_U16(x) (U16)RR_CLAMPR_0_N(x,0xFFFF)
//#define RR_CLAMP_U32(x)   (U32)RR_CLAMP_0_N(x,(S64)0xFFFFFFFF)

#define RR_MIN3(a,b,c)  RR_MIN(RR_MIN(a,b),c)
#define RR_MAX3(a,b,c)  RR_MAX(RR_MAX(a,b),c)
#define RR_MIN4(a,b,c,d)    RR_MIN(RR_MIN(a,b),RR_MIN(c,d))
#define RR_MAX4(a,b,c,d)    RR_MAX(RR_MAX(a,b),RR_MAX(c,d))

//-------------------------------------------------------------------

#define RR_UNROLL_0(x)   do {  } while(0)
#define RR_UNROLL_1(x)   do { x; } while(0)
#define RR_UNROLL_2(x)   do { x; x; } while(0)
#define RR_UNROLL_3(x)   do { x; x; x; } while(0)
#define RR_UNROLL_4(x)   do { x; x; x; x; } while(0)
#define RR_UNROLL_5(x)   do { RR_UNROLL_4(x); x; } while(0)
#define RR_UNROLL_6(x)   do { RR_UNROLL_5(x); x; } while(0)
#define RR_UNROLL_7(x)   do { RR_UNROLL_6(x); x; } while(0)
#define RR_UNROLL_8(x)   do { RR_UNROLL_4(x); RR_UNROLL_4(x); } while(0)
#define RR_UNROLL_9(x)   do { RR_UNROLL_8(x); x; } while(0)
#define RR_UNROLL_10(x)  do { RR_UNROLL_9(x); x; } while(0)
#define RR_UNROLL_11(x)  do { RR_UNROLL_10(x); x; } while(0)
#define RR_UNROLL_12(x)  do { RR_UNROLL_11(x); x; } while(0)
#define RR_UNROLL_13(x)  do { RR_UNROLL_12(x); x; } while(0)
#define RR_UNROLL_14(x)  do { RR_UNROLL_13(x); x; } while(0)
#define RR_UNROLL_15(x)  do { RR_UNROLL_14(x); x; } while(0)
#define RR_UNROLL_16(x)  do { RR_UNROLL_8(x); RR_UNROLL_8(x); } while(0)

// RR_UNROLL_I does x repeatedly with an index "i" set like you were a loop
//	"b" is the start of the looop
#define RR_UNROLL_I_0(b,x)	do {  } while(0)
#define RR_UNROLL_I_1(b,x)	do { const int i = b; x; } while(0)
#define RR_UNROLL_I_2(b,x)	do { RR_UNROLL_I_1(b,x); RR_UNROLL_I_1(b+1,x); } while(0)
#define RR_UNROLL_I_3(b,x)	do { RR_UNROLL_I_2(b,x); RR_UNROLL_I_1(b+2,x); } while(0)
#define RR_UNROLL_I_4(b,x)	do { RR_UNROLL_I_2(b,x); RR_UNROLL_I_2(b+2,x); } while(0)
#define RR_UNROLL_I_5(b,x)	do { RR_UNROLL_I_4(b,x); RR_UNROLL_I_1(b+4,x); } while(0)
#define RR_UNROLL_I_6(b,x)	do { RR_UNROLL_I_4(b,x); RR_UNROLL_I_2(b+4,x); } while(0)
#define RR_UNROLL_I_7(b,x)	do { RR_UNROLL_I_4(b,x); RR_UNROLL_I_3(b+4,x); } while(0)
#define RR_UNROLL_I_8(b,x)	do { RR_UNROLL_I_4(b,x); RR_UNROLL_I_4(b+4,x); } while(0)
#define RR_UNROLL_I_9(b,x)	do { RR_UNROLL_I_8(b,x); RR_UNROLL_I_1(b+8,x); } while(0)
#define RR_UNROLL_I_10(b,x)	do { RR_UNROLL_I_8(b,x); RR_UNROLL_I_2(b+8,x); } while(0)
#define RR_UNROLL_I_11(b,x)	do { RR_UNROLL_I_8(b,x); RR_UNROLL_I_3(b+8,x); } while(0)
#define RR_UNROLL_I_12(b,x)	do { RR_UNROLL_I_8(b,x); RR_UNROLL_I_4(b+8,x); } while(0)
#define RR_UNROLL_I_13(b,x)	do { RR_UNROLL_I_8(b,x); RR_UNROLL_I_5(b+8,x); } while(0)
#define RR_UNROLL_I_14(b,x)	do { RR_UNROLL_I_8(b,x); RR_UNROLL_I_6(b+8,x); } while(0)
#define RR_UNROLL_I_15(b,x)	do { RR_UNROLL_I_8(b,x); RR_UNROLL_I_7(b+8,x); } while(0)
#define RR_UNROLL_I_16(b,x)	do { RR_UNROLL_I_8(b,x); RR_UNROLL_I_8(b+8,x); } while(0)
#define RR_UNROLL_I_17(b,x)	do { RR_UNROLL_I_16(b,x); RR_UNROLL_I_1(b+16,x); } while(0)
#define RR_UNROLL_I_18(b,x)	do { RR_UNROLL_I_16(b,x); RR_UNROLL_I_2(b+16,x); } while(0)
#define RR_UNROLL_I_19(b,x)	do { RR_UNROLL_I_16(b,x); RR_UNROLL_I_3(b+16,x); } while(0)
#define RR_UNROLL_I_20(b,x)	do { RR_UNROLL_I_16(b,x); RR_UNROLL_I_4(b+16,x); } while(0)
#define RR_UNROLL_I_24(b,x)	do { RR_UNROLL_I_16(b,x); RR_UNROLL_I_8(b+16,x); } while(0)
#define RR_UNROLL_I_32(b,x)	do { RR_UNROLL_I_16(b,x); RR_UNROLL_I_16(b+16,x); } while(0)

// RR_STRING_JOIN_DELAY is useful here if count is itself a macro
#define RR_UNROLL_N(count,x)    do { RR_STRING_JOIN_DELAY(RR_UNROLL_,count)(x); } while(0)

#define RR_UNROLL_I_N(count,b,x)    do { RR_STRING_JOIN_DELAY(RR_UNROLL_I_,count)(b,x); } while(0)

//===========================================================================
// REPEAT_INDEXED use explicit number arguments so they can be stringized :

#define RR_REPEAT_INDEXED_0( X )
#define RR_REPEAT_INDEXED_1( X )	X(0)
#define RR_REPEAT_INDEXED_2( X )	X(0) X(1)
#define RR_REPEAT_INDEXED_3( X )	X(0) X(1) X(2)
#define RR_REPEAT_INDEXED_4( X )	X(0) X(1) X(2) X(3)
#define RR_REPEAT_INDEXED_5( X )	X(0) X(1) X(2) X(3) X(4)
#define RR_REPEAT_INDEXED_6( X )	RR_REPEAT_INDEXED_5(X) X(5)
#define RR_REPEAT_INDEXED_7( X )	RR_REPEAT_INDEXED_6(X) X(6)
#define RR_REPEAT_INDEXED_8( X )	RR_REPEAT_INDEXED_7(X) X(7)
#define RR_REPEAT_INDEXED_9( X )	RR_REPEAT_INDEXED_8(X) X(8)
#define RR_REPEAT_INDEXED_10( X )	RR_REPEAT_INDEXED_9(X) X(9)
#define RR_REPEAT_INDEXED_11( X )	RR_REPEAT_INDEXED_10(X) X(10)
#define RR_REPEAT_INDEXED_12( X )	RR_REPEAT_INDEXED_11(X) X(11)
#define RR_REPEAT_INDEXED_13( X )	RR_REPEAT_INDEXED_12(X) X(12)
#define RR_REPEAT_INDEXED_14( X )	RR_REPEAT_INDEXED_13(X) X(13)
#define RR_REPEAT_INDEXED_15( X )	RR_REPEAT_INDEXED_14(X) X(14)
#define RR_REPEAT_INDEXED_16( X )	RR_REPEAT_INDEXED_15(X) X(15)

#define RR_REPEAT_INDEXED_ARG_0(X,Y)
#define RR_REPEAT_INDEXED_ARG_1(X,Y)	X(Y,0)
#define RR_REPEAT_INDEXED_ARG_2(X,Y)	X(Y,0) X(Y,1)
#define RR_REPEAT_INDEXED_ARG_3(X,Y)	X(Y,0) X(Y,1) X(Y,2)
#define RR_REPEAT_INDEXED_ARG_4(X,Y)	X(Y,0) X(Y,1) X(Y,2) X(Y,3)
#define RR_REPEAT_INDEXED_ARG_5(X,Y)	X(Y,0) X(Y,1) X(Y,2) X(Y,3) X(Y,4)
#define RR_REPEAT_INDEXED_ARG_6(X,Y)	RR_REPEAT_INDEXED_ARG_5(X,Y) X(Y,5)
#define RR_REPEAT_INDEXED_ARG_7(X,Y)	RR_REPEAT_INDEXED_ARG_6(X,Y) X(Y,6)
#define RR_REPEAT_INDEXED_ARG_8(X,Y)	RR_REPEAT_INDEXED_ARG_7(X,Y) X(Y,7)
#define RR_REPEAT_INDEXED_ARG_9(X,Y)	RR_REPEAT_INDEXED_ARG_8(X,Y) X(Y,8)
#define RR_REPEAT_INDEXED_ARG_10(X,Y)	RR_REPEAT_INDEXED_ARG_9(X,Y) X(Y,9)
#define RR_REPEAT_INDEXED_ARG_11(X,Y)	RR_REPEAT_INDEXED_ARG_10(X,Y) X(Y,10)
#define RR_REPEAT_INDEXED_ARG_12(X,Y)	RR_REPEAT_INDEXED_ARG_11(X,Y) X(Y,11)
#define RR_REPEAT_INDEXED_ARG_13(X,Y)	RR_REPEAT_INDEXED_ARG_12(X,Y) X(Y,12)
#define RR_REPEAT_INDEXED_ARG_14(X,Y)	RR_REPEAT_INDEXED_ARG_13(X,Y) X(Y,13)
#define RR_REPEAT_INDEXED_ARG_15(X,Y)	RR_REPEAT_INDEXED_ARG_14(X,Y) X(Y,14)
#define RR_REPEAT_INDEXED_ARG_16(X,Y)	RR_REPEAT_INDEXED_ARG_15(X,Y) X(Y,15)

#define RR_MAKE_INDEXED_STRING(base,i) base RR_STRINGIZE(i),

#define RR_MAKE_INDEXED_STRING_ARRAY(base,N)	 RR_STRING_JOIN_DELAY(RR_REPEAT_INDEXED_ARG_,N) ( RR_MAKE_INDEXED_STRING , base )

// RR_MAKE_INDEXED_STRING_ARRAY(xxx,4) makes :
//	"xxx0", "xxx1", "xxx2", "xxx3",
// use like :
// static const char * strings[4] = { RR_MAKE_INDEXED_STRING_ARRAY(xxx,4) };

//===========================================================================

/*

RR_PACKED_STRUCT is a tight-packed struct on all platforms
unlike "RADSTRUCT" which is NOT packed on MSVC

Use like :

RR_PACKED_STRUCT_START(name)
{
	member blah;
} RR_PACKED_STRUCT_END;

*/

#ifdef _MSC_VER
#define RR_PACKED_STRUCT_START(name)	__pragma(pack(push, 1)) struct name
#define RR_PACKED_STRUCT_END			__pragma(pack(pop))
#else
#define RR_PACKED_STRUCT_START(name)	struct __attribute__((packed)) name
#define RR_PACKED_STRUCT_END
#endif

//===========================================================================

static RADINLINE SINTa rrPtrDiffV(void * end, void *start) { return (SINTa)( ((char *)(end)) - ((char *)(start)) ); }

// helper function to show I really am intending to put a pointer difference in an int :
static RADINLINE SINTa rrPtrDiff(SINTa val) { return val; }
static RADINLINE S32 rrPtrDiff32(SINTa val) { S32 ret = (S32) val; RR_ASSERT( (SINTa)ret == val ); return ret; }
static RADINLINE SINTr rrPtrDiffR(SINTa val) { SINTr ret = (SINTr) val; RR_ASSERT( (SINTa)ret == val ); return ret; }

//===================================================================
// 32- and 64-bit rotates

#ifdef _MSC_VER

RR_NAMESPACE_END
	RADDEFFUNC unsigned long __cdecl _lrotl(unsigned long, int);
	#pragma intrinsic(_lrotl)
	RADDEFFUNC unsigned long __cdecl _lrotr(unsigned long, int);
	#pragma intrinsic(_lrotr)
	RADDEFFUNC unsigned __int64 __cdecl _rotl64(unsigned __int64 _Val, int _Shift);
	#pragma intrinsic(_rotl64)
	RADDEFFUNC unsigned __int64 __cdecl _rotr64(unsigned __int64 _Val, int _Shift);
	#pragma intrinsic(_rotr64)
RR_NAMESPACE_START

	#define RR_ROTL32(x,k)  _lrotl((unsigned long)(x),(int)(k))
	#define RR_ROTR32(x,k)  _lrotr((unsigned long)(x),(int)(k))
	#define RR_ROTL64(x,k)  _rotl64((unsigned __int64)(x),(int)(k))
	#define RR_ROTR64(x,k)  _rotr64((unsigned __int64)(x),(int)(k))

#else

	// The other compilers we target generate rotates for this (in the vers we use)
	// this is also a reasonable fall-back
	static RADFORCEINLINE U32 rrRotl32(U32 x, U32 k) { return (x << k) | (x >> ((0 - k) & 31)); }
	static RADFORCEINLINE U32 rrRotr32(U32 x, U32 k) { return (x >> k) | (x << ((0 - k) & 31)); }
	static RADFORCEINLINE U64 rrRotl64(U64 x, U32 k) { return (x << k) | (x >> ((0 - k) & 63)); }
	static RADFORCEINLINE U64 rrRotr64(U64 x, U32 k) { return (x >> k) | (x << ((0 - k) & 63)); }
	
	#define RR_ROTL32(x,k) rrRotl32((U32)(x),(U32)(k))
	#define RR_ROTR32(x,k) rrRotr32((U32)(x),(U32)(k))
	#define RR_ROTL64(x,k) rrRotl64((U64)(x),(U32)(k))
	#define RR_ROTR64(x,k) rrRotr64((U64)(x),(U32)(k))

#endif

//===========================================================================

// RAD_EXPECT is to tell the compiler the *likely* value of an expression
//  different than RADASSUME in that expr might not have that value
//  it's use for branch code layout and static branch prediction
// condition can technically be a variable but should usually be 0 or 1

#if (defined(__GNUC__)) || defined(__clang__)

// __builtin_expect returns value of expr
#define RAD_EXPECT(expr,cond)   __builtin_expect(expr,cond)

#else

#define RAD_EXPECT(expr,cond)   (expr)

#endif

// helpers for doing an if ( ) with expect :
// if ( RAD_LIKELY(expr) ) { ... }

#define RAD_LIKELY(expr)            RAD_EXPECT(expr,1)
#define RAD_UNLIKELY(expr)          RAD_EXPECT(expr,0)

#define if_unlikely(exp)	if ( RAD_UNLIKELY( exp ) )
#define if_likely(  exp)	if ( RAD_LIKELY( exp ) )

//===================================================

//--------------------------------------------------
// Data layout macros

#define RR_ARRAY_SIZE(array)  ( sizeof(array)/sizeof(array[0]) )

// MEMBER_OFFSET tells you the offset of a member in a type
#if defined(__RADANDROID__) || defined(__RADIPHONE__)
// offsetof() gets mucked with by system headers on android, making things dependent on #include order.
#define RR_MEMBER_OFFSET(type,member) __builtin_offsetof(type, member)
#elif defined(__RADLINUX__)
#define RR_MEMBER_OFFSET(type,member) (offsetof(type, member))
#else
#define RR_MEMBER_OFFSET(type,member)  ( (size_t) (UINTa) &(((type *)0)->member) )
#endif

// MEMBER_SIZE tells you the size of a member in a type
#define RR_MEMBER_SIZE(type,member)  ( sizeof( ((type *) 0)->member) )

// just to make gcc shut up about derefing null :
#define RR_MEMBER_OFFSET_PTR(type,member,ptr)  ( (SINTa) &(((type *)(ptr))->member)  - (SINTa)(ptr) )
#define RR_MEMBER_SIZE_PTR(type,member,ptr)	( sizeof( ((type *) (ptr))->member) )

// MEMBER_TO_OWNER takes a pointer to a member and gives you back the base of the object
//  you should then RR_ASSERT( &(ret->member) == ptr );
#define RR_MEMBER_TO_OWNER(type,member,ptr)    (type *)( ((char *)(ptr)) - RR_MEMBER_OFFSET_PTR(type,member,ptr) )

//=======================================

// put a garbage export in every file to stop the annoying 4221 linker warning
#define EXPORT_SOME_CRAP(crap)  U32 RR_NUMBERNAME(crap);

//=======================================

// kill a raddefault when you're in a C file :
#define RADDEFAULTX( val )

//=======================================

// RR_NOP is a nop you can insert to breakpoint on :
static RADINLINE void rrNop() { }
#define RR_NOP()	rrNop()
// #define RR_NOP() ((void)0)
//	but that gets compiled out in debug

//=======================================      	

typedef U8 * U8ptr;
typedef U8 const * U8cptr;

//======================================= 

#if defined(__RADLINUX__) && defined(__RADX64__) // Workaround for glibc memcpy version binary compat issue
	__asm__(".symver memcpy,memcpy@GLIBC_2.2.5");
#endif

//=======================================      	

RR_NAMESPACE_END

#endif
