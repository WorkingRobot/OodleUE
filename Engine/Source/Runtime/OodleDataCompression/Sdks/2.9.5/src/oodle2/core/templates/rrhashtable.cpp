// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrhashtable.h"
#include "rrlog.h"

OODLE_NS_START

typedef hash_table<int,int,hash_table_ops_int > t_hash;
	
/*
static void test1()
{
	typedef hash_table<int,String,hash_table_ops_int > t_hash;
	t_hash	test;
	
	test.insert(0,String("0"));
	test.insert(2,String("2"));
	
	test.insert_or_replace(0,String("1"));
	
	test.insert(0,String("0"));
	
	//test.erase( test.find(0) );
	hash_type h = hash_function(0);
	t_hash::multi_finder mf = test.find_first( h, 0 );
	while( mf )
	{
		rrprintf("%d : %s\n",mf->key,mf->data.CStr());
		mf = test.find_next(mf,h,0);
	}
}
*/

OODLE_NS_END

EXPORT_SOME_CRAP(rrhashtable)
