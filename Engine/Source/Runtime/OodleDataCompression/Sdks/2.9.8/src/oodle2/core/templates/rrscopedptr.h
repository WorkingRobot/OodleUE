// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_SCOPEDPTRH__
#define __RADRR_SCOPEDPTRH__

#include "templates/rrnew.h"
#include "templates/rrstl.h"

#ifdef __cplusplus

RR_NAMESPACE_START

/**

ScopedPtr is like boost::scoped_ptr
it's like a std::auto_ptr but it forbids assignsment & copies - eg. no transfer of ownership

basically it just holds a pointer and calls delete when it goes out of scope.

you can manually call "swap" to transfer ownership

ScopedPtr cannot be stored in a vector because it doesn't allow copies
we would need a ref counting pointer for that

@@ CB : ?? : probably put this in its own file

**/

template <typename t_type>
class ScopedPtr
{
public:
	typedef ScopedPtr<t_type>	this_type;

	explicit ScopedPtr(t_type * p = NULL) : m_ptr(p) { }
	~ScopedPtr() { set(NULL); }
	
	template <typename t_other>
	rrbool operator == (const ScopedPtr<t_other> & rhs) const { return m_ptr == rhs.get(); }

	template <typename t_other>
	rrbool operator != (const ScopedPtr<t_other> & rhs) const { return m_ptr != rhs.get(); }

	// this works with NULL and anything that implicitly can cast to t_type
	rrbool operator == (const t_type * const rhs) const { return m_ptr == rhs; }
	rrbool operator != (const t_type * const rhs) const { return m_ptr != rhs; }
	
	// this lets you just set me too an int !
	//operator rrbool () const { return m_ptr != NULL; }


	void set(t_type * p)
	{
		if ( m_ptr ) OodleDelete(m_ptr);
		m_ptr = p;
	}

	t_type & operator * () const { return *m_ptr; }
	t_type * operator -> () const { return m_ptr; }
	t_type * get() const { return m_ptr; }

	void swap(this_type & rhs) { swap(m_ptr,rhs.m_ptr); }

private:
	t_type * m_ptr;
	
	// forbid copies :
	ScopedPtr(const this_type & copy) { }
	void operator = (const this_type & copy) { }
};

template <typename t_type>
void swap( ScopedPtr<t_type> & lhs, ScopedPtr<t_type> & rhs )
{
	lhs.swap(rhs);
}

RR_NAMESPACE_END

#endif // cpp

#endif // __RADRR_SCOPEDPTRH__
