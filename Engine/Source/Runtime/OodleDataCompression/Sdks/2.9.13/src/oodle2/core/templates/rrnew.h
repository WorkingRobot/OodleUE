// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_NEWH__
#define __RADRR_NEWH__

//============================================================================

#include "rrbase.h"
#include "oodlemalloc.h"

#include <stddef.h> // size_t
#include <stdint.h> // intptr_t

#ifdef __cplusplus

//#include <new.h> // need this to get placement new

/*********

rrnew.h

defines OodleNew and OodleDelete
which you should call instead of new and delete to get our allocators

also entry_array stuff for vector

*************/

enum ERRPlacementNew { eRRPlacementNew };

OODLE_NS_START

//#pragma warning(push)
//#pragma warning(disable : 4345)
// warning C4345: behavior change: an object of POD type constructed with an initializer of the form () will be default-initialized

	//-----------------------------------------------------------------------------------------------

	// move, forward and associated plumbing

	template<typename T> struct remove_reference		{ typedef T type; };
	template<typename T> struct remove_reference<T&>	{ typedef T type; };
	template<typename T> struct remove_reference<T&&>	{ typedef T type; };
	template<typename T> using remove_reference_t = typename remove_reference<T>::type;

	template<typename T>
	OOINLINE constexpr typename remove_reference<T>::type&& move(T&& x)
	{
		typedef typename remove_reference<T>::type U;
		return static_cast<U&&>(x);
	}

	template<typename T>
	OOINLINE constexpr T&& forward(typename remove_reference<T>::type& x)
	{
		return static_cast<T&&>(x);
	}

	template<typename T>
	OOINLINE constexpr T&& forward(typename remove_reference<T>::type&& x)
	{
		return static_cast<T&&>(x);
	}

	/*******************

	This is a set of helper template functions for arrays of a templated type.
	It's interesting in a few ways.  

	copy(Entry * pTo,const Entry * pFm,const int count);
	move(Entry * pTo,const Entry * pFm,const int count);
	construct(Entry * pArray,const int size);
	destruct(Entry * pArray,const int size)
	construct(Entry * pEntry);
	destruct(Entry * pEntry);
	copy_construct(Entry * pEntry,const Entry & from);
	copy_construct(Entry * pArray,const int size,const Entry * pFrom);
	swap(Entry * pArray1,Entry * pArray2,const int count)

	see notes at bottom

	*******************/

	//-----------------------------------------------------------------------------------------------

	template <typename Entry, typename... Args>
	OOINLINE Entry * construct(Entry * pEntry, Args&&... args)
	{
		new (eRRPlacementNew, pEntry) Entry(forward<Args>(args)...);
		return pEntry;
	}

	//-----------------------------------------------------------------------------------------------

	template <typename Entry>
	OOINLINE Entry * destruct_virtual(Entry * pEntry)
	{
		//RR_ASSERT(pEntry);

		pEntry->~Entry();
		return pEntry;
	}
	
	template <typename Entry>
	OOINLINE Entry * destruct(Entry * pEntry)
	{
		//RR_ASSERT(pEntry);

		#ifdef _MSC_VER
		RR_COMPILER_ASSERT( ! __has_virtual_destructor(Entry) );
		#endif
	
		pEntry->Entry::~Entry();
		return pEntry;
	}
	
	//-----------------------------------------------------------------------------------------------

	template <typename Entry>
	OOINLINE Entry * default_construct(Entry * pArray,const size_t size)
	{
		//RR_ASSERT(pArray || size == 0);
		// placement new an array :
		for(size_t i=0;i<size;i++)
		{
			new (eRRPlacementNew, pArray+i) Entry;
		}
		return pArray;
	}

	//-----------------------------------------------------------------------------------------------

	template <typename Entry>
	OOINLINE Entry * destruct(Entry * pArray,const size_t size)
	{
		//RR_ASSERT(pArray || size == 0);
		// this code goes away on types with no destructor
		for(size_t i=0;i<size;i++)
		{
			destruct(pArray+i);
		}
		return pArray;
	}

	//-----------------------------------------------------------------------------------------------
	
	template <typename Entry>
	OOINLINE void copy_construct(Entry * pTo,const Entry & from)
	{
		//RR_ASSERT(pTo);

		new (eRRPlacementNew, pTo) Entry(from);
	}
	
	//-----------------------------------------------------------------------------------------------
	
	template <typename Entry>
	OOINLINE void copy_construct(Entry * pArray,const Entry * pFrom,const size_t size)
	{
		// placement new an array :
		for(size_t i=0;i<size;i++)
		{
			//RR_ASSERT( pArray && pFrom );
			copy_construct(pArray+i,pFrom[i]);
		}
	}

	//-----------------------------------------------------------------------------------------------

	template <typename Entry>
	OOINLINE void move_construct(Entry * pTo,Entry && from)
	{
		new (eRRPlacementNew, pTo) Entry(move(from));
	}

	//-----------------------------------------------------------------------------------------------

	template <typename Entry>
	OOINLINE void move_construct(Entry * pArray,Entry * pFrom,const size_t size)
	{
		// placement new an array :
		for(size_t i=0;i<size;i++)
		{
			move_construct(pArray+i,move(pFrom[i]));
		}
	}

	//-----------------------------------------------------------------------------------------------

	// Prevent VC++ lovingly copy-constructing every single element of a large vector of ints etc.
	// one by one, testing the pointer against NULL (for operator new semantics) and reloading the
	// construct-from value every time (due to potential aliasing).
	//
	// Only specializing this for obvious integer basic types right now.
	template <typename T> struct copy_construct_by_regular_write				 { enum { enable = 0 }; };
	template<>			  struct copy_construct_by_regular_write<char>  		 { enum { enable = 1 }; };
	template<>			  struct copy_construct_by_regular_write<unsigned char>  { enum { enable = 1 }; };
	template<>			  struct copy_construct_by_regular_write<signed char>    { enum { enable = 1 }; };
	template<>			  struct copy_construct_by_regular_write<unsigned short> { enum { enable = 1 }; };
	template<>			  struct copy_construct_by_regular_write<signed short>   { enum { enable = 1 }; };
	template<>			  struct copy_construct_by_regular_write<unsigned int>   { enum { enable = 1 }; };
	template<>			  struct copy_construct_by_regular_write<signed int>     { enum { enable = 1 }; };
	template<>			  struct copy_construct_by_regular_write<unsigned long>  { enum { enable = 1 }; };
	template<>			  struct copy_construct_by_regular_write<signed long>    { enum { enable = 1 }; };
	template<>			  struct copy_construct_by_regular_write<unsigned long long>	{ enum { enable = 1 }; };
	template<>			  struct copy_construct_by_regular_write<signed long long>		{ enum { enable = 1 }; };

	// copy-construct everything from the same template entry
	template <typename Entry>
	OOINLINE void copy_construct_identical(Entry * pArray,const Entry & from,const size_t size)
	{
		if (copy_construct_by_regular_write<Entry>::enable)
		{
			Entry v = from;
			for(size_t i=0;i<size;i++)
				pArray[i] = v;
		}
		else
		{
			for(size_t i=0;i<size;i++)
			{
				copy_construct(pArray+i,from);
			}
		}
	}

	//-----------------------------------------------------------------------------------------------
	
	// We need this in vector, but I don't want to include <memory> there.
	//  Note that this is slightly less flexible than the stl version,
	//  it only works with pointer iterators, not general iterators
	// has [FROM,TO] ordering unlike everything else
	template <typename Entry>
	OOINLINE Entry* uninitialized_copy(const Entry* from_begin,const Entry* from_end,
											Entry* to_ptr)
	{
		while (from_begin != from_end)
		{
			new (eRRPlacementNew, to_ptr) Entry(*from_begin);
			from_begin++;
			to_ptr++;
		}

		return to_ptr;
	}
	
	//-----------------------------------------------------------------------------------------------

	// Entryswap
	//	like memcpy
	//	pArray1 and pArray2 must not overlap
	template <typename Entry>
	OOINLINE void swap_array(Entry * pArray1,Entry * pArray2,const size_t count)
	{
		// assert we don't overlap in a bad way :
		RR_ASSERT( pArray1 != pArray2 );
		RR_ASSERT( pArray1 < pArray2 || pArray1 >= pArray2 + count );

		for(size_t i=0;i<count;i++)
		{
			RR_ASSERT( pArray1 && pArray2 );
			swap(pArray1[i],pArray2[i]);
		}
	}

	//-----------------------------------------------------------------------------------------------

	template <typename Entry>
	OOINLINE void swap_construct(Entry * pTo,Entry & from)
	{
		RR_ASSERT(pTo);

		new (eRRPlacementNew, pTo) Entry();
		swap(from,*pTo);
	}
	
	template <typename Entry>
	OOINLINE void swap_construct(Entry * pArray,Entry * pFrom,const size_t size)
	{
		// placement new an array :
		for(size_t i=0;i<size;i++)
		{
			RR_ASSERT( pArray && pFrom );
			swap_construct(pArray+i,pFrom[i]);
		}
	}
	
	//-----------------------------------------------------------------------------------------------

OODLE_NS_END


// do not call these : use OODLE_NS::construct()
//	explicit access to placement new when there's ambiguity :
//	if there are *any* custom overrides to new() then placement new becomes ambiguous	
inline void* operator new   (size_t, ERRPlacementNew, void* pReturn) { return pReturn; }
inline void  operator delete(void*,  ERRPlacementNew, void*) { }

#ifdef __STRICT_ALIGNED
// GCC : operator new with alignment (second arg is alignment)
inline void* operator new   (size_t , size_t, ERRPlacementNew, void* pReturn) { return pReturn; }
#endif

OODLE_NS_START

/***

CB : WARNING : you cannot use OodleNew(/OodleDelete on things that may be in CINIT
 because the OodleMalloc allocator isn't installed until the start of the program
 
****/
//---------------------------------------------------------

// WARNING : do not use OodleDelete on types with virtual destructors !
template <typename t_type>
void OodleDelete(t_type * obj)
{
	RR_ASSERT( obj != NULL );
	destruct(obj);
	OodleFreeSized((void *)obj,sizeof(t_type));
}

template <typename t_type>
void OodleDeleteVirtual(t_type * obj)
{
	RR_ASSERT( obj != NULL );
	destruct_virtual(obj);
	// can't use size :
	OodleFree((void *)obj);
}

template <typename t_type>
void OodleFreeOne(t_type * obj)
{
	RR_ASSERT( obj != NULL );
	OodleFreeSized((void *)obj,sizeof(t_type));
}

template <typename t_type>
void OodleDeleteArray(t_type * pArray,size_t count)
{
	RR_ASSERT( pArray != NULL );
	
	destruct(pArray,count);

	OodleFreeSized(pArray,sizeof(t_type)*count);
}

/*
template <typename t_type>
t_type * OodleMallocOne(t_type ** pobj)
{
	t_type * ptr = OODLE_MALLOC_ONE(t_type);
	*pobj = ptr;
	return ptr;
}
*/

// OodleNewT is better than the old OodleNew
//	use like :
//	Stuff * ptr = OodleNewT(Stuff) (constructor args); 
//	eg. for void args :
//	Stuff * ptr = OodleNewT(Stuff) ();
#define OodleNewT(t_type)	new (eRRPlacementNew, (t_type *) OODLE_MALLOC_ONE(t_type) ) t_type 

// old way :
//	use OodleNewT() instead
#define OodleNew(t_type)	OodleNewT(t_type)()
#define OodleNewArray(t_type,count)	OODLE_NS_PRE default_construct( OODLE_MALLOC_ARRAY(t_type,count), (size_t)(count) )

//===========================================================================

// rrScopedMalloc : simple malloc/free on construct/destruct
//	 to alloc some memory in a scope
class rrScopedMalloc
{
public:
	~rrScopedMalloc()
	{
		Free();
	}
	
    rrScopedMalloc() : m_ptr(NULL), m_size(0)
    {
    }

    explicit rrScopedMalloc(SINTa size) : m_ptr(NULL), m_size(0)
    {
		Alloc(size);
    }
    
    void * GetPtr() const { return m_ptr; }
    void * GetPtrEnd() const { return (char *)m_ptr + m_size; }
    void * data() const { return m_ptr; }
    SINTa GetSize() const { return m_size; }
    
    void * Alloc(SINTa size)
    {
		RR_ASSERT_ALWAYS( m_ptr == NULL );
		m_size = size;
		m_ptr = OodleMalloc(size);
		// m_ptr may be null
		return m_ptr;
    }
    
    template <typename t_type>
    t_type * AllocArray(SINTa count)
    {
		return (t_type *) Alloc( sizeof(t_type) * count );
    }
    
    void Free()
    {
		if ( m_ptr ) OodleFreeSized(m_ptr,m_size);
		m_ptr = NULL;
    }
    
private:
	void * m_ptr;
	SINTa m_size;
	
	//RR_FORBID_CLASS_STANDARDS(rrScopedMalloc);
	rrScopedMalloc(  const rrScopedMalloc &);
	void operator = (const rrScopedMalloc &);
};

//-----------------------------------------------------------------------------------------------

// AutoPtr : New/Delete an object in scope
//	AutoPtr<thingy> ptr( OodleNewT(thingy)(args) );
template <typename t_type>
class AutoPtr
{
public:
	typedef AutoPtr<t_type> this_type;

	AutoPtr() : m_ptr(NULL)
	{
	}
	explicit AutoPtr(t_type * ptr) :
		m_ptr(ptr)
	{
	}
	~AutoPtr()
	{
		Release();
	}
	
	operator t_type * () const { return m_ptr; }
	t_type * operator -> () const { return m_ptr; }
	
	void Swap(this_type & rhs) { swap(m_ptr,rhs.m_ptr); }
	
	void Release()
	{ 
		if ( m_ptr )
			OodleDelete(m_ptr);
		m_ptr = NULL;
	}
	
	
	//void operator = (const AutoPtr<t_type> & other) { Swap(const_cast<>)) };
	
private:
	t_type * m_ptr;

	//RR_FORBID_CLASS_STANDARDS(AutoPtr);
	AutoPtr(const this_type&);
	void operator=(this_type &);
};

OODLE_NS_END

#endif // cpp

//===========================================================================

#endif // __RADRR_NEWH__
