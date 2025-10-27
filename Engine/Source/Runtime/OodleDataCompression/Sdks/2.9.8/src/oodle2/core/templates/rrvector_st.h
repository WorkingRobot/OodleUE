// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_VECTOR_ST_H__
#define __RADRR_VECTOR_ST_H__

#include "rrbase.h"

/*********************

"vector_st"

vector "s" on stack
then if it exceeds stack capacity, becomes a "t" (tight)

vector_st template parameter is a number of *entries* on stack (not bytes)

vector_st resizes "tight" (doesn't do size-doubling if you push back incrementally)
	generally intended to just resize once

*************************/

#include "oodlemalloc.h"
#include "rrnew.h"
#include "rrvector_flex.h"

RR_NAMESPACE_START

//}{=======================================================================================
// vector_storage_st

template <class t_entry,size_t t_stack_capacity> class vector_storage_st
{
public:
	typedef vector_storage_st<t_entry,t_stack_capacity>		this_type;

	RADFORCEINLINE vector_storage_st()
	{
		init();
	}

	RADFORCEINLINE ~vector_storage_st()
	{
		release();
	}

	void release()
	{
		if ( m_begin && m_begin != (t_entry *)m_stack_data )
		{
			rrvector_free(m_begin); //,m_capacity*sizeof(t_entry));
		}
		init();
	}
 
	void drop()
	{
		init();
	}
 
	/*
	void take(t_entry * a,size_t size)
	{
		m_begin = a;
		m_capacity = size;
	}
	*/
	
	template <size_t t_other_stack_capacity>
	void swap(vector_storage_st<t_entry,t_other_stack_capacity> & other,const size_t maxsize)
	{
		// force allocated so we can just swap pointers:
		force_allocated();
		other.force_allocated();
	
		RR_NAMESPACE_PRE swap(m_begin,other.m_begin);
		RR_NAMESPACE_PRE swap(m_capacity,other.m_capacity);
		// m_stack_data not swapped
	}
 
	//-----------------------------------------
	// simple accessors :

	t_entry *			begin()				{ return m_begin; }
	const t_entry *		begin() const		{ return m_begin; }
	size_t				capacity() const	{ return m_capacity; }
	size_t				max_size() const	{ return (1UL)<<29; }

	//-------------------------------------------------------

	RADFORCEINLINE bool needmakefit(const size_t newsize) const
	{
		return (newsize > m_capacity);
	}

	// makefit1
	// returns the *old* pointer for passing into makefit2
	//
	t_entry * makefit1(const size_t newsize,const size_t oldsize)
	{
		RR_ASSERT( needmakefit(newsize) );

		if ( newsize <= t_stack_capacity )
		{
			// we can fit on stack
		
			if ( m_begin == NULL || m_begin == (t_entry *)m_stack_data )
			{
				m_begin = (t_entry *)m_stack_data;
				m_capacity = t_stack_capacity;
			
				return NULL;
			}
			// m_begin is dynamic for some reason ?
		}
		
		t_entry * pOld = m_begin;

		// growing
		t_entry * pNew = (t_entry *) rrvector_malloc( (U32) ( newsize * sizeof(t_entry) ) );

		RR_ASSERT( pNew );

		// copy existing :
		copy_construct(pNew,pOld,oldsize);

		m_begin = pNew;
		m_capacity = newsize;

		// pOld will be deleted later

		return pOld;
	}

	void makefit2(t_entry * pOld, const size_t oldsize, const size_t oldcapacity)
	{
		if ( pOld )
		{
			destruct(pOld,oldsize);

			if ( pOld != (t_entry *)m_stack_data )
			{
				rrvector_free(pOld); //,oldcapacity*sizeof(t_entry));
			}
		}
	}

	//-------------------------------------------------------

private:
	// stack array is not t_entry because it's not constructed until its used
	RAD_ALIGN(char,m_stack_data,16) [ sizeof(t_entry) * t_stack_capacity ];
	t_entry	*	m_begin;
	size_t		m_capacity;	// how many allocated
	
	void init()
	{
		m_begin = NULL;
		m_capacity = 0;
	}
	
	void force_allocated()
	{
		if ( m_capacity == 0 )
		{
			m_begin = NULL;
		}
		else if ( m_begin == m_stack_data )
		{
			size_t mallocSize = m_capacity * sizeof(t_entry);
			t_entry * pNew = (t_entry *) rrvector_malloc( mallocSize );

			RR_ASSERT_ALWAYS( pNew != NULL );

			t_entry * pOld = m_begin;
			size_t oldsize = m_capacity;

			// copy existing :
			copy_construct(pNew,pOld,oldsize);
			
			destruct(pOld,oldsize);
			
			m_begin = pNew;
		}
	}
};

//#pragma pack(pop)
//#pragma warning(pop)

//}{=======================================================================================
// vector

template <class t_entry,size_t t_stack_capacity> class vector_st : public vector_flex<t_entry,vector_storage_st<t_entry,t_stack_capacity> >
{
public:
	//----------------------------------------------------------------------
	typedef vector_st<t_entry,t_stack_capacity>						this_type;
	typedef vector_flex<t_entry,vector_storage_st<t_entry,t_stack_capacity> >	parent_type;

	typedef typename parent_type::size_type size_type;

	//----------------------------------------------------------------------
	// constructors

	 vector_st() { }
	~vector_st() { }

	// I don't have the normal (size_t) constructor, just this (size_t,value) constructor for clarity
	vector_st(const size_type size,const t_entry & init) : parent_type(size,init)
	{
	}

	vector_st(const this_type & other) : parent_type(other)
	{
	}

	template <class input_iterator>
	vector_st(const input_iterator first,const input_iterator last)
		: parent_type(first,last)
	{
	}
};

//}{=======================================================================================

/*
template<typename t_entry>
OOINLINE void swap(vector<t_entry> &a,vector<t_entry> &b)
{
	a.swap(b);
}
*/

RR_NAMESPACE_END

#endif // __RADRR_VECTORH__
