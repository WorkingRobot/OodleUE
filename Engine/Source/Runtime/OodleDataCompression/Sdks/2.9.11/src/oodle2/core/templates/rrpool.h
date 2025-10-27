// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrbase.h"
#include "oodlemalloc.h"
#include "templates/rrstl.h"
#include "templates/rrnew.h"
#include "rrarenaallocator.h"

/*******

rrPool<type>
	does construction & destruction on Free/Alloc
	
rrPoolVoid
	just provides memory of N bytes

*************/

//chunks are a linked list with pointer in the chunk
//
// in the linked list method, num_per_chunk should generally be a pow2 MINUS ONE
//	to make room for the next pointer

OODLE_NS_START

template <typename t_type,int t_bytes_per_chunk>
class rrPool
{
public:
	typedef t_type entry_type;
	
	enum 
	{
		// subtract off 2 sizeof pointers for the non-items members of Chunk
		c_bytes_per_chunk_for_items = t_bytes_per_chunk - sizeof(void *) - sizeof(rrbool),
		c_entry_size = sizeof(t_type),
		c_items_per_chunk = c_bytes_per_chunk_for_items/c_entry_size
	};
	
	rrPool() : 
		m_chunkList(NULL),
		m_freeList(NULL), m_next(c_items_per_chunk),
		m_scratch(NULL)
	{
	}
	
	~rrPool()
	{
		FreeAllChunks();
	}
	
	void Reset() 
	{
		FreeAllChunks(); 
	}

	t_type * Alloc()
	{
		if ( m_freeList )
		{
			FreeList * f = m_freeList;
			m_freeList = f->next;
			t_type * ret = (t_type *)f;
			construct(ret);
			return ret;
		}
		else
		{
			if ( m_next == c_items_per_chunk )
				AddChunk();

			Chunk * chunk = m_chunkList;
			t_type * ret = & chunk->items[m_next];
			m_next++;	
			construct(ret);
			return ret;
		}
	}
	
	void Free(t_type * entry)
	{
		RR_ASSERT( entry != NULL );
		destruct(entry);
		// reinterpret only after destructing :
		FreeList * f = (FreeList *) entry;
		f->next = m_freeList;
		m_freeList = f;
	}

	void FreeVoid(void *p) { Free( (t_type *)p ); }

	int CountChunksAllocated() const
	{
		int count = 0;
		Chunk * c = m_chunkList;
		while ( c )
		{
			count++;
			c = c->next;
		}
		return count;
	}

	S64 CountBytesAllocated() const
	{
		return CountChunksAllocated() * (S64) sizeof(Chunk);
	}
	
	RADNOINLINE void AddScratch(rrArenaAllocator *scratch)
	{
		m_scratch = scratch;
	}

	void FreeAllFromArena()
	{
		FreeAllChunks(true);
	}

private:
	struct FreeList
	{
		FreeList * next;
	};
	struct Chunk
	{
		Chunk * next;
		rrbool from_arena;
		t_type items[c_items_per_chunk];
	};

	Chunk  *	m_chunkList;
	FreeList *	m_freeList;
	int			m_next;
	rrArenaAllocator *m_scratch;

	RADNOINLINE void AddChunk()
	{
		RR_COMPILER_ASSERT( c_items_per_chunk > 0 );
		RR_COMPILER_ASSERT( sizeof(Chunk) <= t_bytes_per_chunk );

		// note : malloc - not New - do not run constructor until Alloc
		rrbool from_arena = false;
		Chunk * cur = NULL;
		if (m_scratch)
		{
			cur = (Chunk *)m_scratch->Alloc(sizeof(Chunk), true);
			if (cur != NULL)
			{
				from_arena = true;
			}
		}
		if (cur == NULL)
		{
			cur = (Chunk *)OodleMalloc(sizeof(Chunk));
		}
		m_next = 0;
		
		cur->next = m_chunkList;
		cur->from_arena = from_arena;
		m_chunkList = cur;
	}
		
	RADNOINLINE void FreeAllChunks(bool freeInArena = false)
	{
		Chunk * c = m_chunkList;
		while ( c )
		{
			Chunk * next = c->next;
			if (!c->from_arena)
			{
				OodleFreeSized(c, sizeof(*c));
			}
			else if (freeInArena)
			{
				m_scratch->Free(c, sizeof(*c));
			}
			c = next;
		}
		m_chunkList = NULL;
		
		m_freeList = NULL;
		m_next = c_items_per_chunk;
	}
};

//template <typename t_type,int t_bytes_per_chunk>
//const int rrPool<t_type,t_bytes_per_chunk>::entry_size = sizeof(t_type);

//=================================================

class rrPoolVoid
{
public:
	
	rrPoolVoid() : m_bytesPerItem(0),m_bytesPerChunk(0),
		m_chunkList(NULL),
		m_freeList(NULL)
	{
		m_next = m_bytesPerChunk;
	}
	
	rrPoolVoid(S32 bytesPerItem,
			S32 itemsPerChunk) : 
			m_bytesPerItem(bytesPerItem),
			m_bytesPerChunk(bytesPerItem * itemsPerChunk),
		m_chunkList(NULL),
		m_freeList(NULL)
	{
		m_next = m_bytesPerChunk;
	}
	
	void Init(S32 bytesPerItem,
			S32 itemsPerChunk)
	{
		Reset();
		
		m_bytesPerItem = bytesPerItem;
		m_bytesPerChunk = bytesPerItem * itemsPerChunk;
		m_next = m_bytesPerChunk;
	}	
	
	~rrPoolVoid()
	{
		FreeAllChunks();
	}
	
	void Reset() 
	{
		FreeAllChunks(); 
	}

	void * Alloc()
	{
		RR_ASSERT( m_bytesPerChunk != 0 );
		
		if ( m_freeList )
		{
			FreeList * f = m_freeList;
			m_freeList = f->next;
			return (void *)f;
		}
		else
		{
			if ( m_next == m_bytesPerChunk )
				AddChunk();
			
			RR_ASSERT( (m_next + m_bytesPerItem) <= m_bytesPerChunk );

			#if DO_CHUNKLIST_VECTOR
			Chunk * chunk = m_chunkVec.back();
			#else
			Chunk * chunk = m_chunkList;
			#endif
			U8 * ret = & chunk->bytes[m_next];
			m_next += m_bytesPerItem;	
			return ret;
		}
	}
	
	void Free(void * entry)
	{
		RR_ASSERT( entry != NULL );
		//destruct(entry);
		// reinterpret only after destructing :
		FreeList * f = (FreeList *) entry;
		f->next = m_freeList;
		m_freeList = f;
	}

	int CountChunksAllocated() const
	{
		int count = 0;
		Chunk * c = m_chunkList;
		while ( c )
		{
			count++;
			c = c->next;
		}
		return count;
	}

	S64 CountBytesAllocated() const
	{
		return CountChunksAllocated() * ( sizeof(Chunk) + (m_bytesPerChunk-1) );
	}
	
private:
	struct FreeList
	{
		FreeList * next;
	};
	struct Chunk
	{
		Chunk * next;
		U8 bytes[1];
	};

	int			m_bytesPerItem;
	int			m_bytesPerChunk;
	
	Chunk  *	m_chunkList;
	FreeList *	m_freeList;
	int			m_next;

	void AddChunk()
	{
		SINTa chunkSize = ( sizeof(Chunk) + (m_bytesPerChunk-1) );
		Chunk * cur = (Chunk *) OodleMalloc( chunkSize );
		m_next = 0;
		
		cur->next = m_chunkList;
		m_chunkList = cur;
	}
		
	void FreeAllChunks()
	{
		SINTa chunkSize = ( sizeof(Chunk) + (m_bytesPerChunk-1) );
	
		Chunk * c = m_chunkList;
		while ( c )
		{
			Chunk * next = c->next;
			OodleFreeSized(c,chunkSize);
			c = next;
		}
		m_chunkList = NULL;
		m_freeList = NULL;
		m_next = m_bytesPerChunk;
	}
};

//=================================================

OODLE_NS_END



