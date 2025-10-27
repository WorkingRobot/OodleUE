// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_VECTORAH__
#define __RADRR_VECTORAH__

/*************************

vector_a (vector on arena)

like a vector_s but instead of making its own memory, uses a block you provide

*************************/

#include "rrvector_flex.h"

RR_NAMESPACE_START

//}{=======================================================================================
// vector_storage_a

template <class t_entry> class vector_storage_a
{
public:
	typedef vector_storage_a<t_entry>		this_type;

	vector_storage_a() : m_data(NULL), m_capacity(0)
	{
	}

	~vector_storage_a()
	{
	}

	void swap(this_type & other,const size_t maxsize)
	{
		RR_NAMESPACE_PRE swap(m_data,other.m_data);
		RR_NAMESPACE_PRE swap(m_capacity,other.m_capacity);
	}

	void release()
	{
		// @@ ??
		m_data = NULL;
		m_capacity = 0;
	}

	//-----------------------------------------
	// simple accessors :

	t_entry *			begin()			{ return (t_entry *) m_data; }
	const t_entry *		begin() const	{ return (const t_entry *) m_data; }
	size_t				capacity() const{ return m_capacity; }
	size_t				max_size() const{ return m_capacity; }
	//-------------------------------------------------------
	// makefit does nada on vector_storage_a

	RADFORCEINLINE bool needmakefit(const size_t newsize) const
	{
		RR_ASSERT_ALWAYS( newsize <= m_capacity );
		return false;
	}

	RADFORCEINLINE t_entry * makefit1(const size_t newsize,const size_t oldsize)
	{
		RR_ASSERT( newsize <= m_capacity );
		return NULL;
	}

	RADFORCEINLINE void makefit2(t_entry * pOld, const size_t oldsize, const size_t oldcapacity)
	{
		RR_ASSERT_FAILURE("makefit2 should never be called on vector_storage_a!");
	}

	//-------------------------------------------------------

	void provide_arena(void *pData,size_t bytes)
	{
		RR_ASSERT( m_data == NULL && m_capacity == 0 );
		
		// capacity is in elements, not bytes
		
		if ( sizeof(t_entry) <= rrvector_malloc_alignment )
		{
			m_data = pData;
			m_capacity = bytes / sizeof(t_entry);
		}
		else
		{
			UINTa start = (UINTa) pData;		
			UINTa end = start + bytes;
			start = rrAlignUpA(start,rrvector_malloc_alignment);
			m_data = (void *) start;
			m_capacity = (end - start) / sizeof(t_entry);
		}
	}
	
//private:
protected:
	void *	m_data;
	size_t	m_capacity;
};

//}{=======================================================================================
// vector_a

template <class t_entry> class vector_a : public vector_flex<t_entry,vector_storage_a<t_entry> >
{
public:
	//----------------------------------------------------------------------
	typedef vector_a<t_entry>								this_type;
	typedef vector_flex<t_entry,vector_storage_a<t_entry> >	parent_type;

	//----------------------------------------------------------------------
	// constructors

	RADFORCEINLINE  vector_a() { }
	RADFORCEINLINE ~vector_a() { }

	RADFORCEINLINE  vector_a(void *pData,int bytes) { provide_arena(pData,bytes); }

	void provide_arena(void *pData,size_t bytes)
	{
		vector_storage_a<t_entry>::provide_arena(pData,bytes);
	}

	/* Removed because of ambiguity
	RADFORCEINLINE explicit vector_a(const size_type size) : parent_type(size)
	{
	}
	*/

	template <class input_iterator>
	RADFORCEINLINE vector_a(const input_iterator first,const input_iterator last)
		: parent_type(first,last)
	{
	}

private:
	//----------------------------------------------------------------------
	// no copy :
	RADFORCEINLINE vector_a(const this_type & other)
	{
		RR_ASSERT_FAILURE_ALWAYS("no copy");
	}
};
//}{=======================================================================================

RR_NAMESPACE_END

#endif
