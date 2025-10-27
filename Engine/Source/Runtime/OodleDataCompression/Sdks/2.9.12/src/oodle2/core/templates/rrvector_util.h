// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_VECTORUTILH__
#define __RADRR_VECTORUTILH__


#include "rrvector.h"
//#include <algorithm>
//#include "DebugUtil/Profiler.h"

RR_NAMESPACE_START

/*

CB : some of these kind of things don't work if the search operator is passed as a ref,
    sometimes you have to accept it by value


*/

template <typename t_iterator1,
			typename t_iterator2,
			typename search_class>
t_iterator1 find(const t_iterator1 & begin,const t_iterator2 & end,const search_class & what)
{
	RR_ASSERT( end >= begin );
	for(t_iterator1 it = begin;it != end;++it)
	{
		if ( *it == what )
			return it;
	}
	return end;
}

template <typename t_iterator1,
			typename t_iterator2,
			typename t_operator>
void for_each(const t_iterator1 & begin,const t_iterator2 & end,t_operator op)
{
	RR_ASSERT( end >= begin );
	for(t_iterator1 it = begin;it != end;++it)
	{
		op(*it);
	}
}

template <typename t_iterator1,
			typename t_iterator2,
			typename search_class>
t_iterator1 find_if(const t_iterator1 & begin,const t_iterator2 & end,search_class what)
{
	RR_ASSERT( end >= begin );
	for(t_iterator1 it = begin;it != end;++it)
	{
		if ( what(*it) )
			return it;
	}
	return end;
}

template <typename vec_class,
			typename search_class>
typename vec_class::iterator find(vec_class& rVector,
							const search_class& rSearch)
{
	return find(rVector.begin(),
						rVector.end(),
						rSearch);
}

template <typename vec_class,
			typename search_class>
typename vec_class::const_iterator find_c(const vec_class&    rVector,
									const search_class& rSearch)
{
	return find(rVector.begin(),
						rVector.end(),
						rSearch);
}

template <typename vec_class,
			typename search_class>
typename vec_class::iterator find_if(vec_class& rVector,
							const search_class& rSearch)
{
	return find_if(rVector.begin(),
						rVector.end(),
						rSearch);
}

template <typename vec_class,
			typename search_class>
typename vec_class::const_iterator find_if_c(const vec_class&    rVector,
									const search_class& rSearch)
{
	return find_if(rVector.begin(),
						rVector.end(),
						rSearch);
}

template <typename t_iterator1,
			typename t_iterator2,
			typename search_class>
t_iterator1 find_from_end(const t_iterator1 & begin,const t_iterator2 & end,const search_class & what)
{
	RR_ASSERT( end >= begin );
	for(t_iterator1 it = end;;)
	{
		if ( it == begin )
			return end;
		--it;
		if ( *it == what )
			return it;
	}
}

template <typename vec_class,
			typename search_class>
typename vec_class::iterator find_from_end(vec_class& rVector,
							const search_class& rSearch)
{
	return find_from_end(rVector.begin(),
						rVector.end(),
						rSearch);
}

template <typename vec_class,
			typename search_class>
typename vec_class::const_iterator find_from_end_c(const vec_class&    rVector,
									const search_class& rSearch)
{
	return find_from_end(rVector.begin(),
						rVector.end(),
						rSearch);
}

// push_back_tight : like a push_back but with tight memory allocation
template <typename V,typename T>
void push_back_tight(V * pVector,const T & elem)
{
	V other;
	other.reserve(pVector->size()+1);
	other = *pVector;
	other.push_back(elem);
	pVector->swap(other);
}

template <typename vec1_class,
			typename vec2_class>
bool data_bytes_same(const vec1_class & v1,
							const vec2_class & v2)
{
	size_t b1 = v1.size_bytes();
	size_t b2 = v2.size_bytes();
	if ( b1 != b2 ) return false;
	if ( b1 == 0  ) return true;
	return memcmp(v1.data(), v2.data(), b1 ) == 0;
}


// vector_slide_entry : slide from->to using swaps (eg. for MTF)
template <typename t_entry>
void slide_entry(t_entry * parray,int from,int to)
{
	if ( from == to ) return;
	
	if ( from > to )
	{
		while(from > to)
		{
			swap( parray[(from)] , parray[(from-1)] );
			from--;
		}
	}
	else
	{
		while(from < to)
		{
			swap( parray[(from)] , parray[(from+1)] );
			from++;
		}
	}
}

RR_NAMESPACE_END

#endif
