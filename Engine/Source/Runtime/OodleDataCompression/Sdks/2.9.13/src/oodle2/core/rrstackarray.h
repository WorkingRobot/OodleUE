// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_STACKARRAYH__
#define __RADRR_STACKARRAYH__

//#include "rrlog.h"

//===================================================================================
// alloca / stack array stuff

/*

in Oodle is this is only to be used for VERY SMALL allocs (<= 4K)
larger allocs should use arena (RR_SCOPE_ARENA_ARRAY or vector_a)
or something that goes stack-then-malloc like vector_st

alloca sort of sucks in general; if you run out of stack on unix alloca is
just a hard crash :(

*/

// alloca config
// - define RAD_ALLOCA to the alloca function
// - leave undefined if no alloca implementation
#ifdef __RADWIN__
  #include <malloc.h>
  #define RAD_ALLOCA_BASE _alloca
#elif defined(OODLE_ALLOCA_IN_STDLIB)
  #include <stdlib.h>
  #define RAD_ALLOCA_BASE alloca
#else
  #include <alloca.h>
  #define RAD_ALLOCA_BASE alloca
#endif

// logging variant (warning, not okay in if's and such)
//#define RAD_ALLOCA(size) RAD_ALLOCA_BASE(size); rrprintf("alloca : %d\n",(int)size)
// check nobody uses large arrays :
//#define RAD_ALLOCA(size) RAD_ALLOCA_BASE(size); RR_ASSERT( (size) <= 4096 )
#define RAD_ALLOCA(size) RAD_ALLOCA_BASE(size)

#define RR_STACK_ARRAY(name,type,count)          type * name = (type *) RAD_ALLOCA(sizeof(type)*(count))

//===================================================================================

#endif // __RADRR_STACKARRAYH__
