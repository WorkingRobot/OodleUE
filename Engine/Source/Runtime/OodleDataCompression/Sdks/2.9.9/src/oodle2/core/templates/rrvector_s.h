// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_VECTORSH__
#define __RADRR_VECTORSH__

/*********************

"vector" and "vector_s"

cb::vector look-alike

there are two leaf-level classes in here;
	"vector" and "vector_s"
the latter is a static-allocation version of vector

these are built with a templated vector_flex which takes
a templated entry and storage mechanism; vector_flex then
derives from vector_base (just for encapsulation of the
conversation with the storage mechanism).

There are then two storage mechanisms defined here :
vector_storage and vector_storage_static
This is sort of like a better version of the std allocator.
It's better because the storage policy owns the pointer, so it can
do anything to it any time, and it can have state.

---------------------------------------------------

There's a big issue with inlining here;  cb::vector heavily
inlines everything it does.  This results in good speed at the
cost of tons of code.  I let my vector member functions be
non-OOINLINE, which makes less code, but shows up slower in benchmarks.
Hard to say what's best.

*************************/

#include "rrvector_flex.h"

RR_NAMESPACE_START

//}{=======================================================================================
// vector_storage_static

template <class t_entry,size_t t_capacity> class vector_storage_static
{
public:
	typedef vector_storage_static<t_entry,t_capacity>		this_type;

	vector_storage_static()
	{
	}

	~vector_storage_static()
	{
	}

	void swap(this_type & other,const size_t maxsize)
	{
		// static arrays must do full member-wise swaps
		//	much slower than non-static swap implementation
		swap_array(begin(),other.begin(),maxsize);
	}

	void release()
	{
	}

	//-----------------------------------------
	// simple accessors :

	t_entry *			begin()			{ return (t_entry *) m_data; }
	const t_entry *		begin() const	{ return (const t_entry *) m_data; }
	size_t				capacity() const{ return t_capacity; }
	size_t				max_size() const{ return t_capacity; }
	//-------------------------------------------------------
	// makefit does nada on vector_storage_static

	RADFORCEINLINE bool needmakefit(const size_t newsize) const
	{
		RR_ASSERT_ALWAYS( newsize <= t_capacity );
		return false;
	}

	RADFORCEINLINE t_entry * makefit1(const size_t newsize,const size_t oldsize)
	{
		RR_ASSERT( newsize <= t_capacity );
		return NULL;
	}

	RADFORCEINLINE void makefit2(t_entry * pOld, const size_t oldsize, const size_t oldcapacity)
	{
		RR_ASSERT_FAILURE("makefit2 should never be called on vector_storage_static!");
	}

	//-------------------------------------------------------

private:
	// @@ : doing a char array like this breaks any alignment requirements of t_entry
	//char		m_data[ sizeof(t_entry) * t_capacity ];
	RAD_ALIGN(char,m_data,16) [ sizeof(t_entry) * t_capacity ];
};

//}{=======================================================================================
// vector_s

template <class t_entry,size_t t_capacity> class vector_s : public vector_flex<t_entry,vector_storage_static<t_entry,t_capacity> >
{
public:
	//----------------------------------------------------------------------
	typedef vector_s<t_entry,t_capacity>						this_type;
	typedef vector_flex<t_entry,vector_storage_static<t_entry,t_capacity> >	parent_type;

	//----------------------------------------------------------------------
	// constructors

	RADFORCEINLINE  vector_s() { }
	RADFORCEINLINE ~vector_s() { }

	/* Removed because of ambiguity
	RADFORCEINLINE explicit vector_s(const size_type size) : parent_type(size)
	{
	}
	*/

	RADFORCEINLINE vector_s(const this_type & other) : parent_type(other)
	{
	}

	template <class input_iterator>
	RADFORCEINLINE vector_s(const input_iterator first,const input_iterator last)
		: parent_type(first,last)
	{
	}

	//----------------------------------------------------------------------
};
//}{=======================================================================================

RR_NAMESPACE_END

#endif
