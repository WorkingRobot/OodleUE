// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrbase.h"
#include "rrlinknode.h"

OODLE_NS_START

#if 0

struct Example
{
    int stuff;
    
    rrLink LN;
    
    int morestuff;
};

static void ExampleFunc()
{
    rrLinkNode list;
    rrLN_Init(&list);
    
    
    
    Example a1;

    a1.LN.AddToHead(&a1.LN);
    
    Example * pa1 = MEMBER_TO_OWNER(Example,LN,list.m_next);
    

}

#endif

S32 rrLN_ListLength(const rrLinkNode * list)
{
	rrLinkNode * node;
	S32 count = 0;
	rrLN_Walk(node,list)
	{
		count++;
		
		RR_ASSERT( node->m_next != node );
		RR_ASSERT( count < 10669000 );
	}
	return count;
}

OODLE_NS_END
