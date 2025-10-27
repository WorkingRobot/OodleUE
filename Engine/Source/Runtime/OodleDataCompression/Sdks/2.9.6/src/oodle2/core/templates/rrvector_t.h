// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_VECTORTH__
#define __RADRR_VECTORTH__

/*********************

"vector_t"

vector_t is a "tight" vector, which means size is equal to capacity at all times

*************************/

#include "rrstl.h"
#include "rrvector_flex.h"
#include "rrvector.h"

RR_NAMESPACE_START

//}{=======================================================================================
// vector_storage_tight

template <class t_entry> class vector_storage_tight
{
public:

	RADFORCEINLINE vector_storage_tight() : m_begin(NULL)
	{
	}

	RADFORCEINLINE ~vector_storage_tight()
	{
		release();
	}

	void release()
	{
		if ( m_begin )
			rrvector_free(m_begin);
		m_begin = NULL;
	}

	void swap(vector_storage_tight<t_entry> & other,const size_t maxsize)
	{
		swap(m_begin,other.m_begin);
	}

	//-----------------------------------------
	// simple accessors :

	t_entry *			begin()				{ return m_begin; }
	const t_entry *		begin() const		{ return m_begin; }
	size_t				capacity() const	{ return 0; }
	size_t				max_size() const	{ return (32UL)<<20; }

	//-------------------------------------------------------

	RADFORCEINLINE bool needmakefit(const size_t newsize) const
	{
		return true;
	}

	// makefit1
	// returns the *old* pointer for passing into makefit2
	//
	t_entry * makefit1(const size_t newsize,const size_t oldsize)
	{
		RR_ASSERT( needmakefit(newsize) );

		if ( newsize == oldsize )
		{
			// no change needed
			return NULL;
		}

		t_entry * pOld = m_begin;

		// growing
		t_entry * pNew = (t_entry *) rrvector_malloc( (U32) ( newsize * sizeof(t_entry) ) );

		RR_ASSERT( pNew );

		// copy existing :
		copy_construct(pNew,pOld,oldsize);

		m_begin = pNew;
		// m_size not changed

		// pOld will be deleted later

		return pOld;
	}

	void makefit2(t_entry * pOld, const size_t oldsize, const size_t oldcapacity)
	{
		if ( pOld )
		{
			destruct(pOld,oldsize);
			OodleFree(pOld);
		}
	}

	//-------------------------------------------------------

private:
	t_entry	*	m_begin;
};

//}{=======================================================================================
// vector

template <class t_entry> class vector_t : public vector_flex<t_entry,vector_storage_tight<t_entry> >
{
public:
	//----------------------------------------------------------------------
	typedef vector_t<t_entry>						this_type;
	typedef vector_flex<t_entry,vector_storage_tight<t_entry> >	parent_type;

	//----------------------------------------------------------------------
	// constructors

	 vector_t() { }
	~vector_t() { }

	/* Removed because of ambiguity
	explicit vector(const size_type size) : parent_type(size)
	{
	}
	*/

	vector_t(const this_type & other) : parent_type(other)
	{
	}

	template <class input_iterator>
	vector_t(const input_iterator first,const input_iterator last)
		: parent_type(first,last)
	{
	}
	
	// don't allow reserve :
	void reserve(const size_t newcap)
	{
		// silent no-op
		//FAIL_NOTHROW("no reserve on vector_t");
	}
};

//}{=======================================================================================

template<typename t_entry>
OOINLINE void swap(vector_t<t_entry> &a,vector_t<t_entry> &b)
{
	a.swap(b);
}

RR_NAMESPACE_END

#endif
