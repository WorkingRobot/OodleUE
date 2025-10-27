// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrvector.h"
#include "rrvector_s.h"
#include "rrvector_t.h"
#include "rrvector_st.h"
#include "rrvector_util.h"

EXPORT_SOME_CRAP(crap);

// this is just test code, rtlvector has no implementation cpp

//! \todo - move this to Regression

#if 0

#ifdef _MSC_VER
#pragma warning(disable:4505)
#endif

static void printint(const int & i)
{
	printf("%d,",i);
};

static vector<int>	test;
	
static void vector_test()
{	
	{
	vector<int>	test;
	test.resize(4);
	set_all(test.begin(),test.end(),0);

	test.push_back(3);
	test.push_back(test.back());
	test.push_back(test.back());
	test.push_back(test.back());
	test.push_back(test.back());
	test.push_back(test.back());
	
	for_each(test.begin(),test.end(),printint);
	puts("X");

	test.insert( test.begin() + 2, 77 );
	test.insert( test.begin() + 5, 77 );
	test.insert( test.begin() + 1, test.end() - 3, test.end());
	
	for_each(test.begin(),test.end(),printint);
	puts("X");

	test.erase( test.begin() + 1, test.begin() + 4 );
	test.erase( test.begin() + 5 );
	test.erase( test.begin() + 2 );
	for_each(test.begin(),test.end(),printint);
	puts("X");

	vector<int>	test2;
	test.swap(test2);
	test = test2;
	
	for_each(test.begin(),test.end(),printint);
	puts("X");
	
	static const int stuff[5] = { 1 };
	test.insert( test.begin() + 1, stuff, stuff + 5);

	for_each(test.begin(),test.end(),printint);

	puts("X");

	test.insert( test.end() - 3, test.end() - 6, test.end() );

	for_each(test.begin(),test.end(),printint);

	puts("X");
	
	test.insert( test.begin() + 1, stuff, stuff + 5);

	for_each(test.begin(),test.end(),printint);

	puts("X");

	test.insert( test.end() - 3, test.end() - 6, test.end() );

	for_each(test.begin(),test.end(),printint);

	puts("X");
	}

	{
	vector_t<int>	test;
	test.resize(4);
	set_all(test.begin(),test.end(),0);

	test.push_back(3);
	test.push_back(test.back());
	test.push_back(test.back());
	test.push_back(test.back());
	test.push_back(test.back());
	test.push_back(test.back());
	
	for_each(test.begin(),test.end(),printint);
	puts("X");

	test.insert( test.begin() + 2, 77 );
	test.insert( test.begin() + 5, 77 );
	test.insert( test.begin() + 1, test.end() - 3, test.end());
	
	for_each(test.begin(),test.end(),printint);
	puts("X");

	test.erase( test.begin() + 1, test.begin() + 4 );
	test.erase( test.begin() + 5 );
	test.erase( test.begin() + 2 );
	for_each(test.begin(),test.end(),printint);
	puts("X");

	vector_t<int>	test2;
	test.swap(test2);
	test = test2;
	
	for_each(test.begin(),test.end(),printint);
	puts("X");
	
	static const int stuff[5] = { 1 };
	test.insert( test.begin() + 1, stuff, stuff + 5);

	for_each(test.begin(),test.end(),printint);

	puts("X");

	test.insert( test.end() - 3, test.end() - 6, test.end() );

	for_each(test.begin(),test.end(),printint);

	puts("X");
	
	test.insert( test.begin() + 1, stuff, stuff + 5);

	for_each(test.begin(),test.end(),printint);

	puts("X");

	test.insert( test.end() - 3, test.end() - 6, test.end() );

	for_each(test.begin(),test.end(),printint);

	puts("X");
	}

	
	{
	vector_s<int,32>	test;
	test.resize(4);
	set_all(test.begin(),test.end(),0);

	test.push_back(3);
	test.push_back(test.back());
	test.push_back(test.back());
	test.push_back(test.back());
	test.push_back(test.back());
	test.push_back(test.back());
	
	for_each(test.begin(),test.end(),printint);
	puts("X");

	test.insert( test.begin() + 2, 77 );
	test.insert( test.begin() + 5, 77 );
	test.insert( test.begin() + 1, test.end() - 3, test.end());
	
	for_each(test.begin(),test.end(),printint);
	puts("X");

	test.erase( test.begin() + 1, test.begin() + 4 );
	test.erase( test.begin() + 5 );
	test.erase( test.begin() + 2 );
	for_each(test.begin(),test.end(),printint);
	puts("X");

	vector_s<int,32>	test2;
	test.swap(test2);
	test = test2;
	
	for_each(test.begin(),test.end(),printint);
	puts("X");
	
	static const int stuff[5] = { 1 };
	test.insert( test.begin() + 1, stuff, stuff + 5);

	for_each(test.begin(),test.end(),printint);

	puts("X");

	test.insert( test.end() - 3, test.end() - 6, test.end() );

	for_each(test.begin(),test.end(),printint);

	puts("X");
	
	test.insert( test.begin() + 1, stuff, stuff + 5);

	for_each(test.begin(),test.end(),printint);

	puts("X");

	test.insert( test.end() - 3, test.end() - 6, test.end() );

	for_each(test.begin(),test.end(),printint);

	puts("X");
	}
}

#endif
