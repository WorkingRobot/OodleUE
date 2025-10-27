// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(reparse,x.h)
//idoc(parent,OodleAPI_Base)

#include "rrbase.h"
#include "oodlepublicate.h"
#include "oo2baseinc.h"
#include "oo2basepub.h"

#if 0
// repeat def of OODLE_MALLOC_MINIMUM_ALIGNMENT for idoc :

//idoc(explicit,1)
//idoc(markdown,0)
#if defined(__RAD64__)
#define OODLE_MALLOC_MINIMUM_ALIGNMENT	16
#else
#define OODLE_MALLOC_MINIMUM_ALIGNMENT	8	IDOC
/* 

<div class=prototype><div class=prototype_header>// Preprocessor definition:</div>
<pre>
<span class=prototype_type>#if defined(__RAD64__)</span>
<span class=prototype_type>#define</span> OODLE_MALLOC_MINIMUM_ALIGNMENT 16
<span class=prototype_type>#else</span>
<span class=prototype_type>#define</span> OODLE_MALLOC_MINIMUM_ALIGNMENT 8
<span class=prototype_type>#endif</span>
</pre></div></div>
<div class=discussion_heading>Description</div> 
<table> 
  OodleMallocAligned will be asked to provide at least OODLE_MALLOC_MINIMUM_ALIGNMENT 
<P>
  OODLE_MALLOC_MINIMUM_ALIGNMENT is the size of two pointers.
</table>

 */
#endif
//idoc(explicit,0)
//idoc(markdown,1)

#endif



EXPORT_SOME_CRAP(oo2baseinc)
