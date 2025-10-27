// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.


#include "oodlecore.h"

#include "oodlestaticlzp.h"

//#include <stdio.h>
#include "rrlog.h"

#include "entropysets.h"

OODLE_NS_START

	void test()
	{
		rrprintf("hello testoo2netstub 11:31\n");
//		printf("hello testoo2netstub 11:21\n");
	}

/*
	OOFUNC1 void OOFUNC2 OodleNet_Plugins_SetJobSystem(
		t_fp_OodleNet_Plugin_RunJob * fp_RunJob,
		t_fp_OodleNet_Plugin_WaitJob * fp_WaitJob
	)
	{
		test();
	}
*/

OOFUNC1 SINTa OOFUNC2 OodleNetwork1UDP_State_Size()
{		
		rrprintf("OodleNetwork1UDP_State_Size\n");

	test();
	return 1;
}

OOFUNC1 SINTa OOFUNC2 OodleNetwork1_Shared_Size(S32 htbits)
{
		rrprintf("OodleNetwork1_Shared_Size\n");


	test();

	return 2;
}

OOFUNC1 SINTa OOFUNC2 OodleNetwork1TCP_State_Size()
{		
rrprintf("OodleNetwork1TCP_State_Size\n");

	test();

//*
	// okay:
	Histo256 histo = { {0 } };
	histo.counts[0] = 1;
	SINTa histo_sum = 1;
	entropyset_codelens_U16_256 codelens;

	entropysets_histo_to_codelens(histo,histo_sum,&codelens);
/**/

	return 3;
}

OODLE_NS_END
