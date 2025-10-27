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
		m_base(0), m_cur(0), m_cur_max(0), m_size(0), m_allowFallback(allowFallback)
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
	
	void * Alloc(SINTa size)
	{
		size = rrAlignUpA(size,RR_ARENA_ALLOCATOR_ALIGNMENT);
		
		if ( m_cur + size <= m_base + m_size )
		{
			void * ret = (void *) m_cur;
			m_cur += size;
			// m_cur_max track high water mark of arena actually used
			//	@@ does NOT track allocs that fell back to malloc
			//	  (that's not trivial due to things like leaky)
			//	to get the full mem use, first set arena to be way too big
			//	get high water mark, then reduce
			m_cur_max = RR_MAX(m_cur_max,m_cur);
			return ret;
		}
		else if ( m_allowFallback )
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

		// if ptr is LIFO alloc in arena :
		if ( ptrA + size == m_cur )
		{
			m_cur -= size;	
			RR_ASSERT( m_cur >= m_base );
		}
		else
		{
			RR_ASSERT( ptr != NULL );
			// ptr should not have been in Arena :
			RR_ASSERT( ptrA < m_base || ptrA >= (m_base + m_size) );
			OodleFree(ptr);
		}
	}

	rrArenaAllocator() :
		m_base(0), m_cur(0), m_cur_max(0), m_size(0), m_allowFallback(0)
	{
	}
	
	SINTa GetCurAllocated() const
	{
		return (m_cur - m_base);
	}
	
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
	rrbool m_allowFallback;
};

static RADINLINE void * rrArenaAllocLeakyAligned(rrbool * pFree, rrArenaAllocator * arena, SINTa size, S32 alignment)
{
	if ( arena && arena->GetCurAvail() >= (size+alignment) )
	{
		void * ret = arena->Alloc(size+alignment);
		ret = rrAlignUpPointer(ret,alignment);
		*pFree = false;
		return ret;
	}
	else
	{
		void * ret = OodleMallocAligned(size,alignment);
		*pFree = true;
		return ret;
	}
}

template <typename t_type>
t_type * rrArenaNewIfAvail(rrArenaAllocator * arena)
{
	if ( arena->GetCurAvail() < (SINTa)sizeof(t_type) )
	{
		return NULL;
	}
	t_type * ret = (t_type *) arena->Alloc( sizeof(t_type) );
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
	}
	
	~rrArenaAllocatorStateSaver()
	{
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
