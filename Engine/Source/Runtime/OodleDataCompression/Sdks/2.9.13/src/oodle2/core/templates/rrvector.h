// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_VECTORH__
#define __RADRR_VECTORH__

#include "rrbase.h"

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

There's a big issue with inlining here;  stl vector heavily
inlines everything it does.  This results in good speed at the
cost of tons of code.  I let my vector member functions be
non-OOINLINE, which makes less code, but shows up slower in benchmarks.
Hard to say what's best.

*************************/

#include "oodlemalloc.h"
#include "rrnew.h"
#include "rrvector_flex.h"
#include "oodlecore.h" // for OOODLE_PLATFORM

// you can define RR_VECTOR_MAX_GROW_BYTES yourself before including vector.h if you like
#ifndef RR_VECTOR_MAX_GROW_BYTES
#if OODLE_PLATFORM_LARGEMEMORY
#define RR_VECTOR_MAX_GROW_BYTES	(32*1024*1024)
#else // not large memory
#define RR_VECTOR_MAX_GROW_BYTES	(1024*1024)
#endif // RADNT
#endif // RR_VECTOR_MAX_GROW_BYTES

RR_NAMESPACE_START

//}{=======================================================================================
// vector_storage

template <class t_entry> class vector_storage
{
public:

	RADFORCEINLINE vector_storage()
	{
		init();
	}

	RADFORCEINLINE ~vector_storage()
	{
		release();
	}

	void release()
	{
		if ( m_begin )
		{
			rrvector_free(m_begin); //,m_capacity*sizeof(t_entry));
		}
		init();
	}
 
	void drop()
	{
		init();
	}
 
	void take(t_entry * a,size_t size)
	{
		m_begin = a;
		m_capacity = size;
	}
	
	void swap(vector_storage<t_entry> & other,const size_t maxsize)
	{
		RR_NAMESPACE_PRE swap(m_begin,other.m_begin);
		RR_NAMESPACE_PRE swap(m_capacity,other.m_capacity);
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

		t_entry * pOld = m_begin;

		// Be much more careful about growing the memory conservatively.  This changes
		//  push_back from amortized O(N) to O(N^2), but results in tighter vectors.

		enum { c_maxGrowBytes = RR_VECTOR_MAX_GROW_BYTES };	// 1 MB
		enum { c_maxGrowCount = (c_maxGrowBytes+sizeof(t_entry)-1)/sizeof(t_entry) };
		RR_COMPILER_ASSERT( c_maxGrowCount >= 1 );
		// c_maxGrowCount should limit the doubling, but not the passed in newsize
		
		// m_capacity is 0 the first time we're called
		// newsize can be passed in from reserve() so don't put a +1 on it

		// grow by doubling until we get to max grow count, then grow linearly
		size_t newcapacity = RR_MIN( m_capacity * 2 , (m_capacity + c_maxGrowCount) );
		newcapacity = RR_MAX( newcapacity, newsize );

		// if on constant should optimize out
		if ( sizeof(t_entry) == 1 )
		{
			/*
			// @@ make better use of pages ?
			//	really should let the allocator tell me this
			if ( newcapacity >= 1024 )
			{
				// round up newcapacity to be a multiple of 4096
				newcapacity = (newcapacity+4095)&(~4095);
			}
			else
			*/
			
			{
				// round up newcapacity to be a multiple of 8
				newcapacity = (newcapacity+7)&(~7);
			}
		}
		else
		{
			size_t newbytes = newcapacity * sizeof(t_entry);
			
			if ( newbytes > 65536 )
			{
				// align newbytes up :
				newbytes = (newbytes + 65535) & (~65535);
				// align newcapacity down :
				newcapacity = newbytes / sizeof(t_entry);
			}
			else
			{
				if ( newbytes < 512 )
				{
					// don't touch
				}
				else
				{
					// align up to 4096 :
					newbytes = (newbytes + 4095) & (~4095);
					newcapacity = newbytes / sizeof(t_entry);
				}
			}
		}

		RR_ASSERT( newcapacity >= newsize );			
		
		size_t mallocSize = newcapacity * sizeof(t_entry);
		t_entry * pNew = (t_entry *) rrvector_malloc( mallocSize );

		RR_ASSERT_ALWAYS( pNew != NULL );
		//memset(pNew,0xEE,mallocSize);

		// move existing :
		move_construct(pNew,pOld,oldsize);

		m_begin = pNew;
		m_capacity = newcapacity;
		// m_size not changed

		return pOld;
	}

	void makefit2(t_entry * pOld, const size_t oldsize, const size_t oldcapacity)
	{
		if ( pOld )
		{
			destruct(pOld,oldsize);

			rrvector_free(pOld); //,oldcapacity*sizeof(t_entry));
		}
	}

	//-------------------------------------------------------

private:
	t_entry	*	m_begin;
	size_t		m_capacity;	// how many allocated

	void init()
	{
		m_begin = NULL;
		m_capacity = 0;
	}
};

//#pragma pack(pop)
//#pragma warning(pop)

//}{=======================================================================================
// vector

template <class t_entry> class vector : public vector_flex<t_entry,vector_storage<t_entry> >
{
public:
	//----------------------------------------------------------------------
	typedef vector<t_entry>						this_type;
	typedef vector_flex<t_entry,vector_storage<t_entry> >	parent_type;

	typedef typename parent_type::size_type size_type;

	//----------------------------------------------------------------------
	// constructors

	 vector() { }
	~vector() { }

	// I don't have the normal (size_t) constructor, just this (size_t,value) constructor for clarity
	vector(const size_type size,const t_entry & init) : parent_type(size,init)
	{
	}

	vector(const this_type & other) : parent_type(other)
	{
	}

	vector(this_type&& other) : parent_type()
	{
		// we just default-initialized our fields, now swap with other to take ownership
		// this-> required due to two-phase name lookup
		this->swap(other);
	}

	template <class input_iterator>
	vector(const input_iterator first,const input_iterator last)
		: parent_type(first,last)
	{
	}

	this_type& operator=(const this_type & other)
	{
		parent_type::operator=(other);
		return *this;
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

//}{=======================================================================================
// vector_storage_aligned

template <class t_entry, int t_align> class vector_storage_aligned
{
public:
	// t_align must be positive pow2
	RR_COMPILER_ASSERT(t_align > 0 && (t_align & (t_align - 1)) == 0);

	RADFORCEINLINE vector_storage_aligned()
	{
		init();
	}

	RADFORCEINLINE ~vector_storage_aligned()
	{
		release();
	}

	void release()
	{
		if ( m_begin )
		{
			OodleFree(m_begin);
		}
		init();
	}

	void drop()
	{
		init();
	}

	void take(t_entry * a,size_t size)
	{
		RR_ASSERT( ((UINTa)a & (t_align - 1)) == 0 );

		m_begin = a;
		m_capacity = size;
	}

	void swap(vector_storage_aligned<t_entry,t_align> & other,const size_t maxsize)
	{
		RR_NAMESPACE_PRE swap(m_begin,other.m_begin);
		RR_NAMESPACE_PRE swap(m_capacity,other.m_capacity);
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

		t_entry * pOld = m_begin;

		// Be much more careful about growing the memory conservatively.  This changes
		//  push_back from amortized O(N) to O(N^2), but results in tighter vectors.

		enum { c_maxGrowBytes = RR_VECTOR_MAX_GROW_BYTES };	// 1 MB
		enum { c_maxGrowCount = (c_maxGrowBytes+sizeof(t_entry)-1)/sizeof(t_entry) };
		RR_COMPILER_ASSERT( c_maxGrowCount >= 1 );
		// c_maxGrowCount should limit the doubling, but not the passed in newsize

		// m_capacity is 0 the first time we're called
		// newsize can be passed in from reserve() so don't put a +1 on it

		// grow by doubling until we get to max grow count, then grow linearly
		size_t newcapacity = RR_MIN( m_capacity * 2 , (m_capacity + c_maxGrowCount) );
		newcapacity = RR_MAX( newcapacity, newsize );

		// if on constant should optimize out
		if ( sizeof(t_entry) == 1 )
		{
			/*
			// @@ make better use of pages ?
			//	really should let the allocator tell me this
			if ( newcapacity >= 1024 )
			{
				// round up newcapacity to be a multiple of 4096
				newcapacity = (newcapacity+4095)&(~4095);
			}
			else
			*/

			{
				// round up newcapacity to be a multiple of 8
				newcapacity = (newcapacity+7)&(~7);
			}
		}
		else
		{
			size_t newbytes = newcapacity * sizeof(t_entry);

			if ( newbytes > 65536 )
			{
				// align newbytes up :
				newbytes = (newbytes + 65535) & (~65535);
				// align newcapacity down :
				newcapacity = newbytes / sizeof(t_entry);
			}
			else
			{
				if ( newbytes < 512 )
				{
					// don't touch
				}
				else
				{
					// align up to 4096 :
					newbytes = (newbytes + 4095) & (~4095);
					newcapacity = newbytes / sizeof(t_entry);
				}
			}
		}

		RR_ASSERT( newcapacity >= newsize );

		size_t mallocSize = newcapacity * sizeof(t_entry);
		t_entry * pNew = (t_entry *) OodleMallocAligned( mallocSize, t_align );

		RR_ASSERT_ALWAYS( pNew != NULL );
		//memset(pNew,0xEE,mallocSize);

		// move existing :
		move_construct(pNew,pOld,oldsize);

		m_begin = pNew;
		m_capacity = newcapacity;
		// m_size not changed

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
	size_t		m_capacity;	// how many allocated

	void init()
	{
		m_begin = NULL;
		m_capacity = 0;
	}
};

//}{=======================================================================================
// vector_aligned

template <class t_entry, int t_align> class vector_aligned : public vector_flex<t_entry,vector_storage_aligned<t_entry,t_align> >
{
public:
	//----------------------------------------------------------------------
	typedef vector_aligned<t_entry, t_align> this_type;
	typedef vector_flex<t_entry,vector_storage_aligned<t_entry,t_align> > parent_type;

	typedef typename parent_type::size_type size_type;

	//----------------------------------------------------------------------
	// constructors

	 vector_aligned() { }
	~vector_aligned() { }

	// I don't have the normal (size_t) constructor, just this (size_t,value) constructor for clarity
	vector_aligned(const size_type size,const t_entry & init) : parent_type(size,init)
	{
	}

	vector_aligned(const this_type & other) : parent_type(other)
	{
	}

	vector_aligned(this_type&& other) : parent_type()
	{
		// we just default-initialized our fields, now swap with other to take ownership
		swap(other);
	}

	template <class input_iterator>
	vector_aligned(const input_iterator first,const input_iterator last)
		: parent_type(first,last)
	{
	}

	this_type& operator=(const this_type & other)
	{
		parent_type::operator=(other);
		return *this;
	}
};

RR_NAMESPACE_END

#endif // __RADRR_VECTORH__
