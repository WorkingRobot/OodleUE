// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_CPPSTDH__
#define __RADRR_CPPSTDH__

/**

Cpp Std :

standard include for CPP files

do NOT include this in .h files

this is for internal use only !!

NOTE : because we do wicked #defines you need to include this as your *last* header

**/

#ifndef __cplusplus
#error this is for cplusplus yo!
#endif

#include "rrbase.h"
#include "rrmemutil.h"
#include "rrlog.h"
#include "templates/rrnew.h"
#include "templates/rrstl.h"

// urg can't really do this, I need real C++ bool sometimes
//#define bool    _RAD_DONT_USE_BOOL_

//#define delete  _RAD_DONT_USE_DELETE_

// hmm.. CB - I'd like to do this but it screws up "OodleNew(" as well
//  is there a way I can make this #define only affect people who write "new" directly,
//  not the word "new" that comes from OodleNew( ?
//#define new     _RAD_DONT_USE_NEW_




#endif // __RADRR_CPPSTDH__
