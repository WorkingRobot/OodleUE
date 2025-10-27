// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// Linux symbol version mess.

#include <string.h>

// memcpy has an updated version in glibc 2.14 that needs
// a new symver. Easiest way to work around this: just use
// memmove instead.

void *__wrap_memcpy( void * dest, void const *src, size_t n )
{
    return memmove( dest, src, n );
}

/* @cdep pre
   $set(lnx64linkswitches, $lnx64linkswitches -Xlinker --wrap=memcpy )
   $set(lnx64solinkswitches, $lnx64solinkswitches -Xlinker --wrap=memcpy )
*/
