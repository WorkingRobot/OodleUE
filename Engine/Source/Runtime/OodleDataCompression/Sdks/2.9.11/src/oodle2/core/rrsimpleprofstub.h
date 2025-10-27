// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#ifdef __RADRR_SIMPLEPROFH__
#error "dont mix stub and real prof"
#endif
	
#define SIMPLEPROFILE_SCOPE_N(label,count)	
#define SIMPLEPROFILE_SCOPE(label)			
#define SIMPLEPROFILE_SCOPE_SETCOUNT(label,count)

#define SIMPLEPROFILE_SCOPE_INDEXED(label,index,max_index,count)
#define SIMPLEPROFILE_SCOPE_STRING(string,count)

#define rrSimpleProf_IsEnabled()	(false)


