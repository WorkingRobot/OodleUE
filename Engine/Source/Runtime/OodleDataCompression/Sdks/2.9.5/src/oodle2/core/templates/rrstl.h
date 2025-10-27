// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_STLH__
#define __RADRR_STLH__

#include "rrbase.h"
#include "rrmem.h"
#include <stddef.h>
#include "oodlecore.h"

/*************

"rtlstl" :

very simple common templated stuff goes here

stuff like "swap"

this should be light/minimal stuff; put heavy stuff in rralgorithm.h

**************/

//-----------------------------
// default swap is byte copy
//	if you need something else use RR_DEFINE_ASSIGNMENT_SWAP for example

// RR_DEFINE_MEMBER_SWAP specializes swap for types that have a member swap()
//	use like : RR_DEFINE_MEMBER_SWAP(string,swap);
//-----------------------------------------------------------------
#define RR_DEFINE_MEMBER_SWAP(type,func)	\
RR_NAMESPACE_START							\
template<>									\
struct swap_functor<type>			\
{											\
	void operator () (type &a,type &b)		\
	{										\
		a.func(b);							\
	}										\
};											\
RR_NAMESPACE_END

//-----------------------------------------------------------------
#define RR_DEFINE_ASSIGNMENT_SWAP(type)		\
RR_NAMESPACE_START							\
template<>									\
struct swap_functor<typename type>			\
{											\
	void operator () (type &a,type &b)		\
	{										\
		assignment_swap(a,b);				\
	}										\
};											\
RR_NAMESPACE_END
//-----------------------------------------------------------------


RR_NAMESPACE_START

template <typename t_first, typename t_second>
struct pair
{
	t_first first;
	t_second second;
	
	pair() { }
	~pair() { }
	pair(const t_first & f, const t_second &s) : first(f), second(s) { }
};

// same_size_bit_cast casts the bits in memory
//	eg. it's not a value cast
template <typename t_to, typename t_fm>
t_to & same_size_bit_cast_p( t_fm & from )
{
	RR_COMPILER_ASSERT( sizeof(t_to) == sizeof(t_fm) );
	// cast through char * to make aliasing work ?
	char * ptr = (char *) &from;
	return *( (t_to *) ptr );
}

// same_size_bit_cast casts the bits in memory
//	eg. it's not a value cast
// cast with union is better on newer compilers
template <typename t_to, typename t_fm>
t_to & same_size_bit_cast_u( t_fm & from )
{
	RR_COMPILER_ASSERT( sizeof(t_to) == sizeof(t_fm) );
	union _bit_cast_union
	{
		t_fm fm;
		t_to to;		
	};
	//_bit_cast_union converter = { from };
	//return converter.to;
	_bit_cast_union * converter = (_bit_cast_union *) &from;
	return converter->to;
}

template <typename t_to, typename t_fm>
t_to & same_size_bit_cast( t_fm & from )
{
	return same_size_bit_cast_u<t_to>(from);
}

template <typename t_to, typename t_fm>
void put_bit_cast( t_to * pto, const t_fm & from )
{
	*pto = same_size_bit_cast<t_to>(from);
}

// check_value_cast just does a static_cast and makes sure you didn't wreck the value
template <typename t_to, typename t_fm>
t_to check_value_cast( const t_fm & from )
{
	t_to to = static_cast<t_to>(from);
	RR_ASSERT( static_cast<t_fm>(to) == from );
	return to;
}

// put_checked is even better than check_value_cast , when possible
//	because it doesn't require explicitly specifying the type of the destination
//	so it removes another error point
template <typename t_to, typename t_fm>
void put_checked( t_to * pto, const t_fm & from )
{
	*pto = static_cast<t_to>(from);
	RR_ASSERT( static_cast<t_fm>(*pto) == from );
}

#ifdef __GNUC__

template <int n_count>
class  __attribute__((__packed__)) Bytes
{
public:
	char bytes[n_count];
};

#else

#pragma pack(push)
#pragma pack(1)
template <int n_count>
class Bytes
{
public:
	char bytes[n_count];
};
#pragma pack(pop)

#endif

RR_COMPILER_ASSERT( sizeof( Bytes<13> ) == 13 );

template <typename t_type1,typename t_type2>
void ByteCopy(t_type1 * pTo,const t_type2 & from)
{
	typedef Bytes< sizeof(t_type1) > t_bytes;
	RR_COMPILER_ASSERT( sizeof(t_bytes) == sizeof(t_type1) );
	RR_COMPILER_ASSERT( sizeof(t_bytes) == sizeof(t_type2) );
	*(reinterpret_cast< t_bytes * >(pTo)) = reinterpret_cast< const t_bytes & >(from);
}

template <typename t_type>
void ByteSwap(t_type & a,t_type & b)
{
	RR_COMPILER_ASSERT( sizeof(sizeof(t_type)) <= 4096 ); // its on the stack, not too big!
	typedef Bytes< sizeof(t_type) > t_bytes;
	RR_COMPILER_ASSERT( sizeof(t_type) == sizeof(t_bytes) );
	t_bytes c; // don't use t_type cuz we don't want a constructor or destructor
	ByteCopy(&c,reinterpret_cast< t_bytes & >(a));
	ByteCopy(&a,b);
	ByteCopy((t_bytes *)&b,c);
}

#define RR_SWAP(ttype,a,b) do { ttype c = a; a = b; b= c; } while(0)

// assignment_swap helper
template<typename Type> OOINLINE void assignment_swap(Type &a,Type &b)
{
	RR_SWAP(Type,a,b);
}

// swap_functor : replace *this*
template<typename Type>
struct swap_functor
{
	void operator () (Type &a,Type &b)
	{
		// default swap_functor uses BYTE swap ! if you are not byteswappable you must override
		ByteSwap(a,b);
	}
};

// swap template : calls functor , DO NOT override this
template<typename Type> OOINLINE void swap(Type &a,Type &b)
{
	swap_functor<Type>()(a,b);
}

// urg : caps to avoid conflict with stdlib macros
template<typename Type> OOINLINE const Type Min(const Type &a,const Type &b)
{
	return RR_MIN(a,b);
}

template<typename Type> OOINLINE const Type Max(const Type &a,const Type &b)
{
	return RR_MAX(a,b);
}

template<class Type,class Type1,class Type2> OOINLINE Type clamp(const Type &x,const Type1 &lo,const Type2 &hi)
{
	return ( ( x < (Type)lo ) ? (Type)lo : ( x > (Type)hi ) ? (Type)hi : x );
}

//-----------------------------------------------------------------------------------------------

// copy
//	like memcpy
//	pTo and pFm must not overlap
template <class Entry>
OOINLINE void copy(Entry * pTo,const Entry * pFm,const size_t count)
{
	// assert we don't overlap in a bad way :
	RR_ASSERT( pTo != pFm || count == 0 );
	RR_ASSERT( pTo < pFm || pTo >= pFm + count );

	for(size_t i=0;i<count;i++)
	{
		pTo[i] = pFm[i];
	}
}

//-----------------------------------------------------------------------------------------------

// move
//	like memmove
//	pTo and pFm may overlap
template <class Entry>
OOINLINE void move(Entry * pTo,const Entry * pFm,const size_t count)
{
	RR_ASSERT( pTo != pFm || count == 0 );
	
	if ( pTo > pFm ) // && pTo < (pFm+count) ??
	{
		// go backwards
		for(ptrdiff_t i = count-1; i>= 0;i--)
		{
			pTo[i] = pFm[i];
		}
	}
	else // ( pTo < pFm )
	{
		// go forwards
		copy(pTo,pFm,count);
	}
}

//-----------------------------------------------------------------------------------------------

// swap
//	series of swaps
//	pArray1 and pArray2 must not overlap
template <class Entry>
OOINLINE void swap(Entry * pArray1,Entry * pArray2,const size_t count)
{
	// assert we don't overlap in a bad way :
	RR_ASSERT( pArray1 != pArray2 );
	RR_ASSERT( pArray1 < pArray2 || pArray1 >= pArray2 + count );

	for(size_t i=0;i<count;i++)
	{
		swap(pArray1[i],pArray2[i]);
	}
}

//-----------------------------------------------------------------------------------------------


// little helper : are all values in [begin,end) the same?
template <typename t_iterator>
rrbool all_same(t_iterator begin,t_iterator end)
{
	t_iterator ptr( begin+1 );
	while(ptr < end)
	{
		if ( *ptr != *begin )
			return false;
		++ptr;
	}
	return true;
}

// use std::accumulate() for iterators
template <typename t_type>
t_type sum(const t_type * begin,const t_type * end)
{
	t_type ret(0);
	for(const t_type * ptr = begin;ptr != end;++ptr)
	{
		ret += *ptr;
	}
	return ret;
}

// use std::accumulate() for iterators
template <typename t_type,
			typename t_accumulator>
t_accumulator sum(const t_type * begin,const t_type * end,t_accumulator ret)
{
	for(const t_type * ptr = begin;ptr != end;++ptr)
	{
		ret += *ptr;
	}
	return ret;
}

template <class t_iterator>
double averagefloat(const t_iterator begin,const t_iterator end)
{
	RR_ASSERT( begin != end );
	double ret(0);
	size_t count = 0;
	for(t_iterator ptr = begin;ptr != end;++ptr)
	{
		ret += *ptr;
		count ++;
	}
	if ( count > 0 )
	{
		ret /= count;
	}
	return ret;
}

template <typename t_type>
void set_all(t_type * begin,t_type * end,const t_type & val)
{
	for(t_type * ptr = begin;ptr != end;++ptr)
	{
		*ptr = val;
	}
}

template <typename T>
T array_min(const T * b,const T * e)
{
	RR_ASSERT( b != e );
	T ret = *b;
	const T * p = b+1;
	while ( p < e )
	{
		ret = RR_MIN(ret,*p);
		++p;
	}
	return ret;
}

template <typename T>
T array_max(const T * b,const T * e)
{
	RR_ASSERT( b != e );
	T ret = *b;
	const T * p = b+1;
	while ( p < e )
	{
		ret = RR_MAX(ret,*p);
		++p;
	}
	return ret;
}

template <typename t_vector>
void erase_u(t_vector & vec,const size_t index)
{
	vec[index] = vec.back();
	vec.pop_back();
}

//=====================================================================

// variant of reverse using swap instead of assignment
template<typename t_iterator>
OOINLINE void reverse(t_iterator begin,t_iterator end)
{
	--end;
	
	while(end > begin)
	{
		swap(*begin,*end);
		++begin;
		--end;
	}
}

RR_NAMESPACE_END

#endif
