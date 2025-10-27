// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_HASH_FUNCTION_H__
#define __RADRR_HASH_FUNCTION_H__

#include "rrbase.h"
#include "rrhashes.h"
#include <string.h> // strcmp 
#include "oodlecore.h"

/****

hash_function

for use with hash_table , or elsewhere

no hash algorithms in this file - those are in Hashes.h

this is just a template adapter to reroute to the right algorithm for a given data type

if you want your data types to be automatically hashed nicely, implement hash_function<> for them
	(see TokenHash.h for examples on some weirder data types)

-----------

A common default implementation of hash_table_ops<> for hash_table is also here.

NOTE : do NOT template specialize on hash_table_ops - you do not need to overload it
just provide your own ops structure
hash_table_ops is a *convenience* that you can derive from so that you have to implement less

*****/

RR_NAMESPACE_START

typedef U32 hash_type;

/**

generic hash_function<> :
implement your own

**/
template <typename t_key>
OOINLINE hash_type hash_function(const t_key & t)
{
	// works for any type that implicitly converts to U32 :
	return rrHash32( t );
}

template <>
inline hash_type hash_function<U64>(const U64 & t)
{
	return rrHash64( t );
}

//*****************************************************************

// hash for char_ptr :

// ** warning "char *" and "const char *" are different types
typedef const char * char_ptr;

template <>
inline hash_type hash_function<char_ptr>(const char_ptr & t)
{
	return rrFNVHashStr(t);
}

//=========================================================================

/*
template <>
OOINLINE hash_type hash_function<UINTa>(const UINTa & t)
{
	#ifdef __RAD64__
	return rrHash64( (U64) t );
	#else
	return rrHash32( (U32) t );
	#endif
}
*/

//=========================================================================

/**
	{hash,key} pairs must have two special values
	
	EMPTY and DELETED
	
	note that these are not necessarilly just key values,
		though that is often the case
		
	for pointers 0 and 1 are good choices
	
	for ints, maybe 0x7FFFFFFF
	
**/

template <typename t_key>
struct hash_table_key_equal
{
	// to check key equality, I call key_equal
	//	by default it uses operator ==
	//	but you can override it to other things
	//	(using a helper function like this is handy when key is a basic type like char *)
	bool key_equal(const t_key & lhs,const t_key & rhs)
	{
		return lhs == rhs;
	}
	
	OOINLINE hash_type hash_key(const t_key & t)
	{
		return hash_function<t_key>( t );
	}
};

template <typename t_key,t_key empty_val,t_key deleted_val>
struct hash_table_ops : public hash_table_key_equal<t_key>
{

	void make_empty(hash_type & hash,t_key & key)
	{
		key = empty_val;
	}

	bool is_empty(const hash_type & hash,const t_key & key)
	{
		return ( key == empty_val );
	}

	void make_deleted(hash_type & hash,t_key & key)
	{
		key = deleted_val;
	}

	bool is_deleted(const hash_type & hash,const t_key & key)
	{
		return ( key == deleted_val );
	}

};

template <typename t_key>
struct hash_table_ops_mask31hash
{

	bool key_equal(const t_key & lhs,const t_key & rhs)
	{
		return lhs == rhs;
	}
	
	OOINLINE hash_type hash_key(const t_key & t)
	{
		return hash_function<t_key>( t ) & 0x7FFFFFFF;
	}
	
	void make_empty(hash_type & hash,t_key & key)
	{
		hash = 0x8EEEEEEE;
	}

	bool is_empty(const hash_type & hash,const t_key & key)
	{
		return ( hash == 0x8EEEEEEE );
	}

	void make_deleted(hash_type & hash,t_key & key)
	{
		hash = 0x8DDDDDDD;
	}

	bool is_deleted(const hash_type & hash,const t_key & key)
	{
		return ( hash == 0x8DDDDDDD );
	}

};

//=========================================================================

struct hash_table_ops_int : public hash_table_ops<int,int(0x80000000),int(0x80000001)>
{
};

struct hash_table_ops_UINTa : public hash_table_ops<UINTa,UINTa(0),UINTa(1)>
{
};

//struct hash_table_ops_charptr : public hash_table_ops<char_ptr,char_ptr(0),char_ptr(1)>
//struct hash_table_ops_charptr : public hash_table_ops<char_ptr,(char_ptr)0,(char_ptr)1>
struct hash_table_ops_charptr // : public hash_table_ops<char_ptr,0,1>
{
	bool key_equal(const char_ptr & lhs,const char_ptr & rhs)
	{
		return strcmp(lhs,rhs) == 0;
	}
	
	typedef char_ptr t_key;
	static const SINTa empty_val   = 0;
	static const SINTa deleted_val = 1;
	
	void make_empty(hash_type & hash,t_key & key)
	{
		key = (t_key) empty_val;
	}

	bool is_empty(const hash_type & hash,const t_key & key)
	{
		return ( key == (t_key)empty_val );
	}

	void make_deleted(hash_type & hash,t_key & key)
	{
		key = (t_key)deleted_val;
	}

	bool is_deleted(const hash_type & hash,const t_key & key)
	{
		return ( key == (t_key)deleted_val );
	}
};

RR_NAMESPACE_END

#endif // __RADRR_HASH_FUNCTION_H__
