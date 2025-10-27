// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_VECTORFLEXH__
#define __RADRR_VECTORFLEXH__

#include "rrbase.h"

#include "rrnew.h"
#include "rrstl.h"

#include <string.h> // for memset

// vector now allocates memory with rrvector_malloc / rrvector_free - NOT plain OodleMalloc

#ifndef rrvector_malloc
#define rrvector_malloc_alignment	16
#define rrvector_malloc(size)	OodleMallocAligned(size,rrvector_malloc_alignment)
#define rrvector_free(p)		OodleFree(p)
#endif // rrvector_malloc


// toggle here :
// vector always does bounds checks in debug
//	turn this on to do bounds checks in release & throw on failure
//#define DO_VECTOR_RELEASE_CHECKS


/*! ********************
---------------------------------------------------

"vector_flex"

building block for many cb::vector look-alikes

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

#ifdef DO_VECTOR_RELEASE_CHECKS
#define RR_VECTOR_ASSERT	RR_ASSERT_ALWAYS
#else
#define RR_VECTOR_ASSERT	RR_ASSERT
#endif

RR_NAMESPACE_START

//}{=======================================================================================

/*! *

All you have to do to build a vector_flex is to provide it a storage class.

vector_storage should NOT construct the memory it holds; it is purely a
raw memory container.  vector_base adds the code that manages construction.

Description of the vector_storage policy :

//-------------------------------------------------------

template <class t_entry>
class vector_storage
{
public:
	//-------------------------------------------------------

	vector_storage();
	~vector_storage();

	void release();

	void swap(vector_storage<t_entry> & other,const size_t maxsize);

	//-----------------------------------------
	// simple accessors :

	t_entry *			begin();
	const t_entry *		begin() const;
	size_t				capacity() const;
	size_t				max_size() const;

	//-------------------------------------------------------

	bool needmakefit(const size_t newsize) const;
	t_entry * makefit1(const size_t newsize,const size_t oldsize);
	void makefit2(t_entry * pOld, const size_t oldsize, const size_t oldcapacity);
	
	//-------------------------------------------------------
};

**/

//}{=======================================================================================
// vector_base

/*! *

vector_base is just an abstraction method to make the implementation of vector_flex
cleaner

vector_base does all the conversation with vector_storage
it inherits from vector_storage, and adds an "m_size" member,
 which tracks the fraction of the storage which is current constructed.

Just like a cb::vector, the storage is always of size >= "active size"
and the non-active elements are in the state of raw memory, that is they
are not constructed.

**/

template <typename t_entry,typename t_storage> class vector_base : public t_storage
{
public:
	typedef t_storage						parent_type;
	typedef  vector_base<t_entry,t_storage>	this_type;

	using parent_type::needmakefit;

	//-----------------------------------------
	// constructors :

	RADFORCEINLINE vector_base()
	{
		init();
	}

	RADFORCEINLINE ~vector_base()
	{
		release();
	}

	vector_base(const vector_base & other)
	{
		init();
		extend_copy(other.begin(),other.size());
	}

	template <class input_iterator>
	vector_base(const input_iterator first, const input_iterator last)
	{
		init();
		assign_construct(first,last);
	}

	//-----------------------------------------

	// increase size and construct added entries with their default constructor
	void extend_default(const size_t count)
	{
		const size_t oldsize = m_size;
		if ( needmakefit(m_size+count) )
		{
			const size_t oldcapacity = capacity();
			this->makefit2( makefit1(m_size + count) , m_size , oldcapacity);
		}
		m_size += count;
		construct(begin() + oldsize,count);
	}

	// increase size and don't construct!!!!
	void extend_no_construct(const size_t count)
	{
		if ( needmakefit(m_size+count) )
		{
			const size_t oldcapacity = capacity();
			this->makefit2( makefit1(m_size + count) , m_size , oldcapacity);
		}
		m_size += count;
	}
	
	// increase size and construct added entries with their copy constructor
	// this must work fine if pFrom is pointing into myself
	//	the key is that makefit1() doesn't free the old memory, so we must
	//	do any work between makefit1 and makefit2
	void extend_copy(const t_entry * pFrom,const size_t count)
	{
		if ( needmakefit(m_size+count) )
		{
			const size_t oldsize = m_size;
			const size_t oldcapacity = capacity();
			t_entry * pOld = makefit1(m_size + count);
			copy_construct(begin() + m_size,pFrom,count);
			m_size += count;
			this->makefit2(pOld, oldsize, oldcapacity);
		}
		else
		{
			copy_construct(begin() + m_size,pFrom,count);
			m_size += count;
		}
	}

	// fast specialization of extend_copy for one addition (for push_back)
	RADFORCEINLINE void extend_copy(const t_entry & from)
	{
		if ( needmakefit(m_size+1) )
		{
			const size_t oldsize = m_size;
			const size_t oldcapacity = capacity();
			t_entry * pOld = makefit1(m_size + 1);
			copy_construct(begin() + m_size,from);
			m_size ++;
			this->makefit2(pOld, oldsize, oldcapacity);
		}
		else
		{
			copy_construct(begin() + m_size,from);
			m_size ++;
		}
	}

	// see notes on base extend_copy
	RADFORCEINLINE void extend_copy(const t_entry & from,const size_t count)
	{
		if ( needmakefit(m_size+count) )
		{
			const size_t oldsize = m_size;
			const size_t oldcapacity = capacity();
			t_entry * pOld = makefit1(m_size + count);
			copy_construct_identical(begin() + m_size,from,count);
			m_size += count;
			this->makefit2(pOld, oldsize, oldcapacity);
		}
		else
		{
			copy_construct_identical(begin() + m_size,from,count);
			m_size += count;
		}
	}

	// reduce size and destruct stuff left behind :
	RADFORCEINLINE void shrink(const size_t newsize)
	{
		RR_VECTOR_ASSERT( newsize >= 0 && newsize <= m_size );
		const size_t count = m_size - newsize;
		RR_NAMESPACE_PRE destruct(begin() + newsize,count);
		m_size = newsize;
	}

	//-----------------------------------------

	void reserve(const size_t newcap)
	{
		if ( needmakefit(newcap) )
		{
			const size_t oldcapacity = capacity();
			this->makefit2( makefit1( newcap ) , m_size, oldcapacity);
		}
	}

	// release frees any memory allocated and resizes to zero
	void release()
	{
		shrink(0);
		parent_type::release();
	}

	void drop()
	{
		parent_type::drop();
		m_size = 0;
	}

	void take(t_entry * a,size_t size)
	{
		parent_type::take(a,size);
		m_size = size;
	}
	
	void swap(this_type & other)
	{
		RR_NAMESPACE_PRE swap(m_size,other.m_size);
		parent_type::swap(other, RR_MAX(m_size,other.m_size) );
	}

	void insert(const size_t n_pos,
				 const t_entry * first, const t_entry * last);

	//-----------------------------------------

	// assign_construct :
	//	used by iterator-range and copy constructor for whole vectors
	//	fills out a randbool-new (empty) vector_base with the data from a range of iterators
	template <class input_iterator>
	void assign_construct(const input_iterator first,const input_iterator last)
	{
		RR_VECTOR_ASSERT( m_size == 0 );
		// w64
		const size_t count = (size_t)( last - first );
		if ( needmakefit(count) )
		{
			const size_t oldcapacity = capacity();
			t_entry * pOld = makefit1(count);
			m_size = count;
			uninitialized_copy(first, last, begin() );
			this->makefit2(pOld,(size_t)0,oldcapacity);
		}
		else
		{
			m_size = count;
			uninitialized_copy(first, last, begin() );
		}
	}

	// specialization to simple iterators :
	void assign_construct(const t_entry * const first,const t_entry * const last)
	{
		RR_VECTOR_ASSERT( m_size == 0 );
		// w64
		const size_t count = (size_t)( last - first );
		if ( needmakefit(count) )
		{
			const size_t oldcapacity = capacity();
			t_entry * pOld = makefit1(count);
			m_size = count;
			copy_construct(begin(),first,count);
			this->makefit2(pOld,0,oldcapacity);
		}
		else
		{
			m_size = count;
			copy_construct(begin(),first,count);
		}
	}

	//-----------------------------------------
	// simple accessors :

	t_entry *			begin()			{ return parent_type::begin(); }
	const t_entry *		begin() const	{ return parent_type::begin(); }
	t_entry *			end()			{ return parent_type::begin()+m_size; }
	const t_entry *		end() const		{ return parent_type::begin()+m_size; }
	size_t				size() const	{ return m_size; }
	size_t				capacity() const{ return parent_type::capacity(); }
	size_t				max_size() const{ return parent_type::max_size(); }

	//-----------------------------------------
private :
	//-----------------------------------------
	// data :

	size_t		m_size;		// how many in use

	//-----------------------------------------
	// maintenance :

	void init()
	{
		m_size = 0;
	}
	
	t_entry * makefit1(const size_t newsize)
	{
		return parent_type::makefit1(newsize,m_size);
	}

	//-----------------------------------------
}; // end vector_base

/**
 vector_base::insert for a chunk of elements
 this does NOT work if the elements to insert are from the same vector,
 since insert can cause allocation, and the elements to add are referred-to by pointer
**/
template<class t_entry,class t_storage>
void
vector_base<t_entry,t_storage>::insert(const size_t n_pos,
		 const t_entry * first, const t_entry * last)
{
	const ptrdiff_t n_insert = ( last - first );
	const size_t n_insert_end = n_pos + n_insert;
	const size_t old_size = size();
	const size_t oldcapacity = capacity();

	t_entry * pOld;
	if ( needmakefit(m_size+n_insert) )
		pOld = makefit1(m_size + n_insert);
	else
		pOld = NULL;

	m_size += n_insert;

	if ( n_insert_end > old_size )
	{
		// copy construct out to n_insert_end
		//	from the insert chunk
		const ptrdiff_t n_insert_copy = old_size - n_pos;
		const ptrdiff_t n_insert_append = n_insert_end - old_size;
		RR_VECTOR_ASSERT( n_insert_copy >= 0 && n_insert_append >= 0);
		RR_VECTOR_ASSERT( (n_insert_copy + n_insert_append) == n_insert );

		copy_construct( begin() + old_size, first + n_insert_copy, n_insert_append );

		// now copy from old end to new end
		copy_construct( begin() + old_size + n_insert_append, begin() + n_pos, n_insert_copy );

		// finally copy in the n_insert_copy chunk :
		copy( begin() + n_pos, first, n_insert_copy );
	}
	else
	{
		copy_construct( begin() + old_size, begin() + old_size - n_insert, n_insert );
	
		t_entry * newpos = begin() + n_pos;
	
		const ptrdiff_t count = old_size - n_insert_end;

		move( newpos + n_insert, newpos, count );
				
		copy( newpos, first, n_insert );
	}
	
	if ( pOld )
		this->makefit2(pOld, old_size, oldcapacity);
}

//}{=======================================================================================
// vector_flex

template <class t_entry,class t_storage> class vector_flex : protected vector_base<t_entry,t_storage>
{
public:
	//----------------------------------------------------------------------
	// typedefs :
	typedef t_entry				value_type;
	typedef t_entry*			iterator;
	typedef const t_entry*		const_iterator;
	typedef t_entry&			reference;
	typedef const t_entry&		const_reference;
	typedef size_t				size_type;

	typedef vector_flex<t_entry,t_storage>	this_type;
	typedef vector_base<t_entry,t_storage>	parent_type;

	//----------------------------------------------------------------------
	// constructors

	RADFORCEINLINE  vector_flex() { }
	/*RADFORCEINLINE*/ ~vector_flex() { }

	RADFORCEINLINE vector_flex(const size_type size,const value_type & init)
	{
		//parent_type::extend_default(size); 
		parent_type::extend_copy(init,size); 
	}

	RADFORCEINLINE vector_flex(const int size,const value_type & init)
	{
		//parent_type::extend_default(size); 
		parent_type::extend_copy(init,size); 
	}


	RADFORCEINLINE vector_flex(const this_type & other) :
		parent_type(other)
	{
	}

	template <class input_iterator>
	RADFORCEINLINE vector_flex(const input_iterator first,const input_iterator last)
		: parent_type(first,last)
	{
	}
	
	//---------------------------------------------------------------------------
	// simple accessors :

	// iterator support
	iterator		begin()			{ return parent_type::begin(); }
	const_iterator	begin() const	{ return parent_type::begin(); }
	iterator		end()			{ return parent_type::end(); }
	const_iterator	end() const		{ return parent_type::end(); }

	// at() with range check
	reference at(const size_type i)
	{
		RR_VECTOR_ASSERT( i >= 0 && i < parent_type::size() );
		return *(parent_type::begin() + i); 
	}
	const_reference at(const size_type i) const
	{
		RR_VECTOR_ASSERT( i >= 0 && i < parent_type::size() );
		return *(parent_type::begin() + i); 
	}

	// operator[]
	reference		operator[](const size_type i)		{ return at(i); }
	const_reference operator[](const size_type i) const { return at(i); }

	// front() and back()
	reference		front()			{ return at(0); }
	const_reference front() const	{ return at(0); }
	reference		back()			{ return at(parent_type::size()-1); }
	const_reference back() const	{ return at(parent_type::size()-1); }

	// size queries :
	size_type	size() const		{ return parent_type::size(); }
	bool		empty() const		{ return parent_type::size() == 0; }
	size_type	capacity() const	{ return parent_type::capacity(); }
	size_type	max_size() const	{ return parent_type::max_size(); }

	// size32 will assert in debug if size doesn't fit in int
	//	silently do nasty things in release
	int			size32() const		{ return check_value_cast<int>(size()); }
	
	void reserve(const size_t newcap)	{ parent_type::reserve(newcap); }

	//---------------------------------------------------------------------------
	// mutators :

	void push_back(const value_type & e)
									{ parent_type::extend_copy(e); }

	//@@ I'm not fond of push_back with no argument; it's unclear and error prone, but here for compatibility
	void push_back()				{ parent_type::extend_default(1); }

	// push_back_no_construct is dangerous!!  You must immediately construct it yourself!!
	void push_back_no_construct()	{ parent_type::extend_no_construct(1); }
	
	void pop_back()					{ parent_type::shrink( parent_type::size() - 1 ); }

	void clear()					{ parent_type::shrink(0); }

	void resize(const size_type new_size);
	void resize(const size_type new_size, const value_type & e);

	//----------------------------------------------------------------------
	// serious entry mutators :

	iterator insert(iterator position, const value_type & e);

	iterator insert(iterator position,
			  const_iterator first, const_iterator last);

	iterator erase(iterator position);

	iterator erase(iterator first, iterator last);

	//----------------------------------------------------------------------
	// serious whole mutators :

	void swap(this_type & other)
	{
		parent_type::swap(other);
	}

	//@@ I'm not fond of operator= on a whole vector; it's error prone and expensive, but here for compatibility
	void operator=(const this_type & other)
	{
		assign(other.begin(),other.end());
	}

	//----------------------------------------------------------------------
	// assign :
	//  assign does work fine for iterator ranges that are in yourself

	template <class input_iterator>
	void assign(const input_iterator first,const input_iterator last)
	{
		const size_type count = (size_type)( last - first );
		if ( size() >= count )
		{
			// don't move on top of self; can happen eg. if you do operator= on yourself
			if ( begin() != first )
			{
				move( begin(), first, count );
			}
			parent_type::shrink(count);
		}
		else
		{
			RR_VECTOR_ASSERT( ! is_iterator_in_range(first) );
			RR_VECTOR_ASSERT( (first == last) || ! is_iterator_in_range(last-1) );
			parent_type::shrink(0);
			parent_type::assign_construct(first,last);
		}
	}

	//----------------------------------------------------------------------
	// extensions :

	// @@ ? what should data do on an empty vector?  This will assert;
	//	maybe it should return NULL ?
	const t_entry * data() const	{ if ( empty() ) return NULL; else return &(at(0)); }
	t_entry * data()				{ if ( empty() ) return NULL; else return &(at(0)); }
	// gives you access to data in capacity that's not in size() ; THIS IS BAD !
	//const t_entry * data() const	{ RR_VECTOR_ASSERT( capacity() > 0 ); return &(*begin()); }
	//t_entry * data()				{ RR_VECTOR_ASSERT( capacity() > 0 ); return &(*begin()); }

	size_t size_bytes() const { return size() * sizeof(t_entry); } 
	void memset_zero() { memset(data(),0,size_bytes()); }

	// tighten releases memory that's not in use
	void tighten()
	{
		if ( capacity() != size() )
		{
			this_type other(*this);
			swap(other);
		}
	}
	
	// release frees any memory allocated and resizes to zero
	void release()
	{
		parent_type::release();
	}

	// drop stops holding the pointer and does NOT free the memory
	void drop()
	{
		parent_type::drop();
	}
	
	void take(t_entry * a,size_t size)
	{
		release();
		parent_type::take(a,size);
	}

	// fast unordered erase :
	//  now with STL-compliant signature
	iterator erase_u(iterator position)
	{
		RR_VECTOR_ASSERT( is_iterator_in_range(position) );
		if ( position != end()-1 )
			*position = back();
		pop_back();
		return position;
	}

	void erase_u(const size_t i)
	{
		if ( i != size()-1 )
			at(i) = back();
		pop_back();
	}

	// member function find :
	// just write it outrselves so we don't have to bring in <algorithm>

	template <class what_to_find>
	iterator find(const what_to_find & what) 
	{
		for(iterator it = begin();it < end();it++)
		{
			if ( *it == what )
				return it;
		}
		return end();
	}

	template <class what_to_find>
	const_iterator find(const what_to_find & what) const 
	{
		for(const_iterator it = begin();it < end();it++)
		{
			if ( *it == what )
				return it;
		}
		return end();
	}

	template <class _Predicate>
	iterator find_if(_Predicate __pred)
	{
		for(iterator it = begin();it < end();it++)
		{
			if ( __pred(*it) )
				return it;
		}
		return end();
	}

	template <class _Predicate>
	const_iterator find_if(_Predicate __pred) const
	{
		for(iterator it = begin();it < end();it++)
		{
			if ( __pred(*it) )
				return it;
		}
		return end();
	}

	// some sugar :

	template <class other_vector>
	void appendv( const other_vector & other )
	{
		insert( end(), other.begin(), other.end() );
	}

  	void append(const_iterator first, const_iterator last)
  	{
  		insert( end(), first, last );
  	}
  	
  	void append(const_iterator first, const size_t count)
  	{
  		insert( end(), first, first + count );
  	}  			
  	
	template <class other_vector>
	void insertv(iterator position, const other_vector & other )
	{
		insert( position, other.begin(), other.end() );
	}

	template <class other_vector>
	void assignv(const other_vector & other )
	{
		assign( other.begin(), other.end() );
	}
	

	//----------------------------------------------------------------------

private:
	//----------------------------------------------------------------------
	template <class input_iterator>
	bool is_iterator_in_range(const input_iterator it) const
	{
		return ( it >= begin() && it < end() );
	}
	template <class input_iterator>
	bool is_iterator_in_range_or_end(const input_iterator it) const
	{
		return ( it >= begin() && it <= end() );
	}	

}; // end vector

//}{=======================================================================================
// vector_flex serious mutators

#define T_PRE1 template<typename t_entry,typename t_storage>
#define T_PRE2 vector_flex<t_entry,t_storage>
#define T_PRE_VOID	T_PRE1 void T_PRE2
#define T_PRE_IT	T_PRE1 typename vector_flex<t_entry,t_storage>::iterator T_PRE2

//----------------------------------------------------------------------
// serious entry mutators :

T_PRE_IT::insert(const iterator position, const value_type & e)
{
	// not maximally efficient if position == end; use push_back
	RR_VECTOR_ASSERT( is_iterator_in_range_or_end(position) );

	const size_t n = ( position - begin() );
	RR_VECTOR_ASSERT( n <= size() );

	const size_t move_count = size() - n;

	if ( move_count == 0 )
	{
		push_back(e);
		return begin();
	}
	else
	{
		// extend by copy construction :
		if ( size() > 0 )
		{
			parent_type::extend_copy( back() );
		}
		// now cannot use "position" any more !

		iterator newpos = begin() + n;
		move(newpos+1,newpos, move_count );

		*newpos = e;

		return newpos;
	}
}

T_PRE_IT::insert(iterator position,
		  const_iterator first, const_iterator last)
{
	RR_VECTOR_ASSERT( is_iterator_in_range_or_end(position) );
	RR_VECTOR_ASSERT( ! is_iterator_in_range(first) );
	RR_VECTOR_ASSERT( (first == last) || ! is_iterator_in_range(last-1) );

	const ptrdiff_t n_pos = ( position - begin() );

	parent_type::insert(n_pos,first,last);

	return begin() + n_pos;
}

T_PRE_IT::erase(const iterator position)
{
	RR_VECTOR_ASSERT( is_iterator_in_range(position) );
	RR_VECTOR_ASSERT( position != end() );
	const ptrdiff_t n_pos = ( position - begin() );
	
	// slide it down :
	const ptrdiff_t newsize = size() - 1;
	move(position,position+1, newsize - n_pos );

	parent_type::shrink(newsize);
	
	return begin() + n_pos;
}

T_PRE_IT::erase(const iterator first, const iterator last)
{
	RR_VECTOR_ASSERT( is_iterator_in_range_or_end(first) );
	RR_VECTOR_ASSERT( is_iterator_in_range_or_end(last) );
	if ( last <= first )
		return end();

	const ptrdiff_t ifirst = (first - begin());
	const ptrdiff_t ilast  = (last - begin() );
	const ptrdiff_t num_removed = ilast - ifirst;
	const ptrdiff_t num_to_move = parent_type::size() - ilast;

	move(first,last, num_to_move);
	parent_type::shrink( parent_type::size() - num_removed );
	
	return begin() + ifirst;
}

T_PRE_VOID::resize(const size_type new_size)
{
	// if shrinking, must clear things being left :
	if ( new_size < parent_type::size() )
	{
		parent_type::shrink(new_size);
	}
	else
	{
		parent_type::extend_default(new_size - parent_type::size());
	}
}

T_PRE_VOID::resize(const size_type new_size, const value_type & e)
{
	// if shrinking, must clear things being left :
	if ( new_size < parent_type::size() )
	{
		parent_type::shrink(new_size);
	}
	else
	{
		const ptrdiff_t count = new_size - parent_type::size();
		parent_type::extend_copy(e,count);
	}
}

#undef T_PRE_IT
#undef T_PRE_VOID
#undef T_PRE1 
#undef T_PRE2


//}{=======================================================================================

RR_NAMESPACE_END

#endif
