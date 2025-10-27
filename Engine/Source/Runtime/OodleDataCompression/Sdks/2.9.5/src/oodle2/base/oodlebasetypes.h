#ifndef OODLE_BASE_TYPES_H
#define OODLE_BASE_TYPES_H

#include <stdint.h>

#define OOCOPYRIGHT "Copyright (C) 1994-2021, Epic Games Tools LLC"

// Typedefs
typedef int8_t OO_S8;
typedef uint8_t OO_U8;
typedef int16_t OO_S16;
typedef uint16_t OO_U16;
typedef int32_t OO_S32;
typedef uint32_t OO_U32;
typedef int64_t OO_S64;
typedef uint64_t OO_U64;
typedef float OO_F32;
typedef double OO_F64;
typedef intptr_t OO_SINTa;
typedef uintptr_t OO_UINTa;
typedef int32_t OO_BOOL;

// Struct packing handling and inlining
#if defined(__GNUC__) || defined(__clang__)
	#define OOSTRUCT struct __attribute__((__packed__))
	#define OOINLINEFUNC inline
#elif defined(_MSC_VER)
	// on VC++, we use pragmas for the struct packing
	#define OOSTRUCT struct
	#define OOINLINEFUNC __inline
#endif

// Linkage stuff
#if defined(_WIN32)
	#define OOLINK __stdcall
	#define OOEXPLINK __stdcall
#else
	#define OOLINK
	#define OOEXPLINK
#endif

// C++ name demangaling
#ifdef __cplusplus
	#define OODEFFUNC extern "C"
	#define OODEFSTART extern "C" {
	#define OODEFEND }
	#define OODEFAULT( val ) =val
#else
	#define OODEFFUNC
	#define OODEFSTART
	#define OODEFEND
	#define OODEFAULT( val )
#endif

// ========================================================
// Exported function declarations
#define OOEXPFUNC OODEFFUNC

//===========================================================================
// OO_STRING_JOIN joins strings in the preprocessor and works with LINESTRING
#define OO_STRING_JOIN(arg1, arg2)              OO_STRING_JOIN_DELAY(arg1, arg2)
#define OO_STRING_JOIN_DELAY(arg1, arg2)        OO_STRING_JOIN_IMMEDIATE(arg1, arg2)
#define OO_STRING_JOIN_IMMEDIATE(arg1, arg2)    arg1 ## arg2

//===========================================================================
// OO_NUMBERNAME is a macro to make a name unique, so that you can use it to declare
//    variable names and they won't conflict with each other
// using __LINE__ is broken in MSVC with /ZI , but __COUNTER__ is an MSVC extension that works

#ifdef _MSC_VER
  #define OO_NUMBERNAME(name) OO_STRING_JOIN(name,__COUNTER__)
#else
  #define OO_NUMBERNAME(name) OO_STRING_JOIN(name,__LINE__)
#endif

//===================================================================
// simple compiler assert
// this happens at declaration time, so if it's inside a function in a C file, drop {} around it
#ifndef OO_COMPILER_ASSERT
  #if defined(__clang__)
    #define OO_COMPILER_ASSERT_UNUSED __attribute__((unused))  // hides warnings when compiler_asserts are in a local scope
  #else
    #define OO_COMPILER_ASSERT_UNUSED
  #endif

  #define OO_COMPILER_ASSERT(exp)   typedef char OO_NUMBERNAME(_dummy_array) [ (exp) ? 1 : -1 ] OO_COMPILER_ASSERT_UNUSED
#endif


#endif

