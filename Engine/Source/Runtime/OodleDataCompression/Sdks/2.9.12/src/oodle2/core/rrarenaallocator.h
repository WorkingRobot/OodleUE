// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RR_ARENA_ALLOC__
#define __RR_ARENA_ALLOC__

#include "oodlemalloc.h"

//---------------------------------------------------------
#ifdef __cplusplus

#include "rrmath.h" // for aligns
#include "rrlog.h" // @@ could go
#include "templates/rrnew.h"
#include "templates/rrstl.h" // for swap :(

OODLE_NS_START

/***

rrArenaAllocator helper class
you give it an array, it lets you allocator out of that array

***/

#define RR_ARENA_ALLOCATOR_ALIGNMENT	(16)

struct rrArenaAllocator
{
public:
	// allowFallback: can I call OodleMalloc if I run out of space ?
	rrArenaAllocator(void * ptr,SINTa len,rrbool allowFallback) : 
		m_base(0), m_cur(0), m_cur_max(0), m_size(0), m_unorderedStart(0), m_allowFallback(allowFallback)
	{
		RR_ASSERT( len >= 0 );
		
		// ptr can be NULL and m_size can be 0
		
		if ( ptr == NULL || len <= 0 )
		{
			m_base = 0;
			m_size = 0;
		}	
		else
		{
			m_base = rrAlignUpA((UINTa)ptr,RR_ARENA_ALLOCATOR_ALIGNMENT);
			UINTa end = (UINTa)ptr + len;
			
			if ( end > m_base )
			{
				m_size = end - m_base;
			}
			else
			{
				m_base = 0;
				m_size = 0;
			}
		}
		
		m_cur = m_cur_max = m_base;
	}
	
	~rrArenaAllocator()
	{
		RR_ASSERT( m_cur == m_base );
	}

	void * Alloc(SINTa size, rrbool forbid_fallback=false)
	{
		size = rrAlignUpA(size,RR_ARENA_ALLOCATOR_ALIGNMENT);

		SINTa extra = 0;
		if (m_unorderedStart != 0)
		{
			// try to allocate extra space at end of allocation for storing size of allocation
			extra = RR_ARENA_ALLOCATOR_ALIGNMENT;
			RR_ASSERT(extra >= 2 * sizeof(UINTa)); // on 32b targets, extra is > not == generally.
		}

		if ( m_cur + size + extra <= m_base + m_size )
		{
			void* ret = (void*)m_cur;
			m_cur += size;

			if (m_unorderedStart != 0)
			{
				// save allocation size after allocation block
				UINTa* temp = (UINTa*)m_cur;
				temp[0] = size;
				temp[1] = 0;		// Free() not called
				m_cur += extra;
			}

			// m_cur_max track high water mark of arena actually used
			//	@@ does NOT track allocs that fell back to malloc
			//	  (that's not trivial due to things like leaky)
			//	to get the full mem use, first set arena to be way too big
			//	get high water mark, then reduce
			m_cur_max = RR_MAX(m_cur_max,m_cur);
			return ret;
		}
		else if (forbid_fallback)
		{
			return NULL;
		}
		else if ( m_allowFallback)
		{
			return OodleMallocAligned(size,RR_ARENA_ALLOCATOR_ALIGNMENT);
		}
		else
		{
			RR_ASSERT(0);
			return NULL;
		}
	}
	
	// Free must be in reverse order of Alloc
	void Free(void * ptr,SINTa size)
	{
		size = rrAlignUpA(size,RR_ARENA_ALLOCATOR_ALIGNMENT);
		UINTa ptrA = (UINTa)ptr;

		if (m_unorderedStart)
		{
			// if pointer is in arena after unordered Free()'s are allowed
			if (ptrA >= m_unorderedStart && ptrA < m_base + m_size)
			{
				// temp is 2xUINTa extra data after allocated block
				UINTa* temp = (UINTa*)(ptrA + size);
				RR_ASSERT(temp[0] == (UINTa)size); // must call Free() with same size as Alloc()
				RR_ASSERT(temp[1] == 0);           // hitting this assert means double Free()
				temp[1] = 1; // remember that this allocation has Free() called on it

				const UINTa extra = RR_ARENA_ALLOCATOR_ALIGNMENT;
				if (ptrA + size + extra == m_cur)
				{
					// can "free" all allocations for which Free() was called before
					while (m_cur != m_unorderedStart && temp[1] == 1)
					{
						size = temp[0];
						m_cur -= size + extra;
						RR_ASSERT(m_cur >= m_unorderedStart);
						temp = (UINTa*)(m_cur - extra);
					}
				}
				// otherwise leave it in arena, as there are newer allocations afterwards
			}
			else
			{
				RR_ASSERT(ptr != NULL);
				OodleFree(ptr);
			}
		}
		else
		{
			// if ptr is LIFO alloc in arena:
			if (ptrA + size == m_cur)
			{
				m_cur -= size;
				RR_ASSERT(m_cur >= m_base);
			}
			else
			{
				RR_ASSERT(ptr != NULL);
				// ptr should not have been in Arena :
				RR_ASSERT(ptrA < m_base || ptrA >= (m_base + m_size));
				OodleFree(ptr);
			}
		}
	}

	rrArenaAllocator() :
		m_base(0), m_cur(0), m_cur_max(0), m_size(0), m_unorderedStart(0), m_allowFallback(0)
	{
	}
	
	SINTa GetCurAllocated() const
	{
		return (m_cur - m_base);
	}
	
	// Informational, but because of object footers, exact space requirements depend on context;
	// so don't use this to decide whether to alloc, just call Alloc with forbid_fallback=true
	// and handle failure if you want to handle non-arena allocs specially.
	SINTa GetCurAvail() const
	{
		SINTa ret =  m_size - (m_cur - m_base);
		ret -= RR_ARENA_ALLOCATOR_ALIGNMENT; // Alloc does AlignUp, make sure it will succeed!
		if (ret < 0 ) return 0;
		return ret;
	}
	
	UINTa m_base;
	UINTa m_cur;
	UINTa m_cur_max;
	UINTa m_size;
	UINTa m_unorderedStart;
	rrbool m_allowFallback;
};

// array that is allocated either from arena (if not NULL and has enough space) or with fallback to malloc
// must be freed explicitly with call to Release() using same element count as allocation
// for arena calls Free() in case rrArenaAllocatorStateSaver is being used
template <class T>
struct rrArenaArray
{
	T* m_ptr;
	void* m_alloc; // in arena allocation actual pointer may be different because of alignment

	rrArenaArray()
		: m_ptr(NULL)
		, m_alloc(NULL)
	{
	}

	~rrArenaArray()
	{
		// make sure Release() was called
		RR_ASSERT(m_ptr == NULL);
	}

	void Zero(SINTa count)
	{
		memset(m_ptr, 0, sizeof(T) * count);
	}

	void Allocate(rrArenaAllocator* arena, SINTa count, S32 alignment = 0)
	{
		SINTa size_in_bytes = sizeof(T) * count;
		if (arena)
		{
			m_alloc = arena->Alloc(size_in_bytes + alignment, true); // allocate with forbid_fallback
			if (m_alloc)
			{
				m_ptr = alignment ? (T*)rrAlignUpPointer(m_alloc, alignment) : (T*)m_alloc;
				return;
			}
		}

		// We fall through into here when arena alloc failed
		m_ptr = (T*)OodleMallocAligned(size_in_bytes, alignment ? alignment : OODLE_MALLOC_MINIMUM_ALIGNMENT);
	}

	void Release(rrArenaAllocator* arena, SINTa count, S32 alignment = 0)
	{
		if (m_ptr == NULL)
		{
			return;
		}

		if (arena && m_alloc)
		{
			if (arena->m_unorderedStart)
			{
				SINTa size_in_bytes = sizeof(T) * count;
				arena->Free(m_alloc, size_in_bytes + alignment);
			}
			m_alloc = NULL;
		}
		else
		{
			OodleFree(m_ptr);
		}
		m_ptr = NULL;
	}

	T* data()								{ return m_ptr; }
	const T* data() const					{ return m_ptr; }
	const T& operator [] (SINTa idx) const	{ return m_ptr[idx]; }
	T& operator [] (SINTa idx)				{ return m_ptr[idx]; }

private:
	rrArenaArray(const rrArenaArray&) {}
	void operator = (const rrArenaArray&) {}
};

static RADINLINE void * rrArenaAllocLeakyAligned(rrbool * pFree, rrArenaAllocator * arena, SINTa size, S32 alignment)
{
	if ( arena  )
	{
		void * ret = arena->Alloc(size+alignment, true);
		if ( ret )
		{
			ret = rrAlignUpPointer(ret,alignment);
			*pFree = false;
			return ret;
		}
	}

	void * ret = OodleMallocAligned(size,alignment);
	*pFree = true;
	return ret;
}

template <typename t_type>
t_type * rrArenaNewIfAvail(rrArenaAllocator * arena)
{
	t_type * ret = (t_type *) arena->Alloc( sizeof(t_type), true );
	if ( ! ret )
		return NULL;
	construct( ret );
	return ret;
}

// rrArenaAllocatorStateSaver just save & restore m_cur in the Arena
//	this is a hard stomp with no verification
//	frees anything done inside this scope
struct rrArenaAllocatorStateSaver
{
public:

	rrArenaAllocatorStateSaver(rrArenaAllocator * arena) :
	//, SINTa alloc_if_none) :
		m_arena(arena)
	{
		m_cur_save = arena->m_cur;
		//m_my_alloc = 0;

		/*
		if ( arena->m_base == 0 && alloc_if_none > 0 )
		{
			RR_ASSERT( m_cur_save == 0 );
			m_my_alloc = OodleMallocAligned( alloc_if_none , RR_ARENA_ALLOCATOR_ALIGNMENT );
			arena->m_base = (UINTa)m_my_alloc;
			arena->m_size = alloc_if_none;
			arena->m_cur = arena->m_cur_max = (UINTa)m_my_alloc;
		}
		*/

		// starts allowing "unordered" Alloc/Free pairs
		// each allocation will have 16 byte padding at end
		// first 8 bytes is allocation size, to allow walking backwards
		// next 8 bytes is "free" bit - set to 0 in Alloc, and 1 to Free, to know when "freeing" of this block is allowed
		m_unordered_save = arena->m_unorderedStart;
		m_arena->m_unorderedStart = m_cur_save;
	}
	
	~rrArenaAllocatorStateSaver()
	{
		if (m_arena->m_unorderedStart)
		{
			RR_ASSERT(m_cur_save >= m_arena->m_unorderedStart);

			// now make sure all allocation till "saved" position had Free() called on them
			while (m_arena->m_cur != m_arena->m_unorderedStart)
			{
				UINTa* temp = (UINTa*)(m_arena->m_cur - RR_ARENA_ALLOCATOR_ALIGNMENT);
				SINTa size = temp[0];

				// hitting this assert means that "unordered free" were used, but some Allocs() do not have corresponding Free() called
				// later m_cur is set to m_cur_save so we want to be sure that nobody will be using memory in arena from previous Allocs()
				// so here we are making sure every Alloc() has coressponding Free() called
				RR_ASSERT(temp[1] == 1); // check if Free() was called

				m_arena->m_cur -= size + RR_ARENA_ALLOCATOR_ALIGNMENT;
				//memset((void*)m_arena->m_cur, 2, size + RR_ARENA_ALLOCATOR_ALIGNMENT);
				RR_ASSERT(m_arena->m_cur >= m_arena->m_base);
			}

			m_arena->m_unorderedStart = m_unordered_save;
		}
		m_arena->m_cur = m_cur_save;
		
		/*
		if ( m_my_alloc != NULL )
		{
			RR_ASSERT( m_cur_save == 0 );
		
			OodleFree(m_my_alloc);
			
			// v2 compiles out in cdep release
			rrPrintf_v2("scratch arena max used : %d\n",(int)(m_arena->m_cur_max - m_arena->m_base));
			
			m_arena->m_base = 0;
			m_arena->m_size = 0;
			m_arena->m_cur = m_arena->m_cur_max = 0;
		}
		*/
	}
	
	rrArenaAllocator * m_arena;
	UINTa m_cur_save;
	UINTa m_unordered_save;
	//void * m_my_alloc;
};

// rrScopeArenaAlloc : do an rrArenaAllocator Alloc/Free in scope
//	rrArenaAllocator can be NULL, then we use OodleMalloc
struct rrScopeArenaAlloc
{
public:
	// if pPtr is passed, fill it in
	rrScopeArenaAlloc(SINTa size,rrArenaAllocator * pAlloc,void ** pPtr = NULL) :
		m_pAlloc(pAlloc), m_ptr(NULL), m_size(size)
	{
		//rrprintf("rrScopeArenaAlloc : %d\n",(int)size);
	
		if ( pAlloc )
		{
			m_ptr = pAlloc->Alloc(size);
		}
		else
		{
			m_ptr = OodleMallocAligned(size,RR_ARENA_ALLOCATOR_ALIGNMENT);
		}
		
		RR_ASSERT_ALWAYS( m_ptr != NULL );
			
		if ( pPtr )
			*pPtr = m_ptr;
	}
	
	rrScopeArenaAlloc() :
		m_pAlloc(NULL), m_ptr(NULL), m_size(0)
	{
	}
	
	~rrScopeArenaAlloc()
	{
		Release();
	}

	void * Alloc(SINTa size,rrArenaAllocator * pAlloc)
	{
		//rrprintf("rrScopeArenaAlloc : %d\n",(int)size);
		
		Release();
	
		m_pAlloc = pAlloc;
		m_size = size;
		
		if ( pAlloc )
		{
			m_ptr = pAlloc->Alloc(size);
		}
		else
		{
			m_ptr = OodleMallocAligned(size,RR_ARENA_ALLOCATOR_ALIGNMENT);
		}
		
		RR_ASSERT_ALWAYS( m_ptr != NULL );
		
		return m_ptr;
	}
	
	void Release()
	{
		if ( m_ptr == NULL ) return;
		
		if ( m_pAlloc )
		{
			m_pAlloc->Free(m_ptr,m_size);
		}
		else
		{
			OodleFree(m_ptr);
		}
		m_ptr = NULL;
	}

	void swap(rrScopeArenaAlloc & other)
	{
		RR_NAMESPACE_PRE swap(m_pAlloc,other.m_pAlloc);
		RR_NAMESPACE_PRE swap(m_ptr,other.m_ptr);
		RR_NAMESPACE_PRE swap(m_size,other.m_size);
	}

	rrArenaAllocator * m_pAlloc;
	void * m_ptr;
	SINTa m_size;
};

// RR_SCOPE_ARENA_ARRAY :
//		U32 array[256];
//		RR_SCOPE_ARENA_ARRAY(array,U32,256,arena);
//	arena can be null, then malloc is used
#define RR_SCOPE_ARENA_ARRAY(name,type,count,arena)	type * name; rrScopeArenaAlloc RR_NUMBERNAME(scoper) ( sizeof(type)*(count) , arena , (void **)&(name) );

#define RR_SCOPE_ARENA(name,type,arena)	RR_SCOPE_ARENA_ARRAY(name,type,1,arena)

OODLE_NS_END

#endif

#endif // __RR_ARENA_ALLOC__
