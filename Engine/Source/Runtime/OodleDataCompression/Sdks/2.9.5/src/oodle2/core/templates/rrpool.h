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

/*******

rrPool<type>
	does construction & destruction on Free/Alloc
	
rrPoolVoid
	just provides memory of N bytes

*************/

#define DO_CHUNKLIST_VECTOR	0

// if DO_CHUNKLIST_VECTOR , list of chunks is in a vector
// else , chunks are a linked list with pointer in the chunk
//
// in the linked list method, num_per_chunk should generally be a pow2 MINUS ONE
//	to make room for the next pointer

#ifndef DO_CHUNKLIST_VECTOR
#error pick one
#endif

#if DO_CHUNKLIST_VECTOR
#include "templates/rrvector.h"
#endif

OODLE_NS_START

template <typename t_type,int t_bytes_per_chunk>
class rrPool
{
public:
	typedef t_type entry_type;
	t_type dummy_item; // I use this just to get item size
	// Not properly a compile-time constant but hopefull optimizes out ?
	//static const int entry_size;
	
	enum 
	{
		#if DO_CHUNKLIST_VECTOR
		c_bytes_per_chunk_for_items = t_bytes_per_chunk,
		#else
		c_bytes_per_chunk_for_items = t_bytes_per_chunk - sizeof(void *),
		#endif
		c_entry_size = sizeof(t_type),
		c_items_per_chunk = c_bytes_per_chunk_for_items/c_entry_size
	};
	
	rrPool() : 
	#if !DO_CHUNKLIST_VECTOR
		m_chunkList(NULL),
	#endif
		m_freeList(NULL), m_next(c_items_per_chunk)
	{
		#if DO_CHUNKLIST_VECTOR
		m_chunkVec.reserve(256);
		#endif
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

			#if DO_CHUNKLIST_VECTOR
			Chunk * chunk = m_chunkVec.back();
			#else
			Chunk * chunk = m_chunkList;
			#endif
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
		#if DO_CHUNKLIST_VECTOR
		return (int)m_chunkVec.size();
		#else
		int count = 0;
		Chunk * c = m_chunkList;
		while ( c )
		{
			count++;
			c = c->next;
		}
		return count;
		#endif
	}

	S64 CountBytesAllocated() const
	{
		return CountChunksAllocated() * (S64) sizeof(Chunk);
	}
	
private:
	struct FreeList
	{
		FreeList * next;
	};
	struct Chunk
	{
		#if !DO_CHUNKLIST_VECTOR
		Chunk * next;
		#endif
		t_type items[c_items_per_chunk];
	};

	#if DO_CHUNKLIST_VECTOR
	vector<Chunk *>	m_chunkVec;
	#else
	Chunk  *	m_chunkList;
	#endif
	
	FreeList *	m_freeList;
	int			m_next;

	RADNOINLINE void AddChunk()
	{
		// note : malloc - not New - do not run constructor until Alloc
		Chunk * cur = (Chunk *)OodleMalloc(sizeof(Chunk));
		m_next = 0;
		
		#if DO_CHUNKLIST_VECTOR
		m_chunkVec.push_back(cur);
		#else
		cur->next = m_chunkList;
		m_chunkList = cur;
		#endif
	}
		
	RADNOINLINE void FreeAllChunks()
	{
		#if DO_CHUNKLIST_VECTOR
		for(int i=0;i<(int)m_chunkVec.size();i++)
		{
			OodleFreeSized( m_chunkVec[i] , sizeof(Chunk) );
		}
		m_chunkVec.clear();
		#else
		Chunk * c = m_chunkList;
		while ( c )
		{
			Chunk * next = c->next;
			OodleFreeSized(c, sizeof(Chunk) );
			c = next;
		}
		m_chunkList = NULL;
		#endif
		
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
	#if !DO_CHUNKLIST_VECTOR
		m_chunkList(NULL),
	#endif
		m_freeList(NULL)
	{
		#if DO_CHUNKLIST_VECTOR
		m_chunkVec.reserve(256);
		#endif
		m_next = m_bytesPerChunk;
	}
	
	rrPoolVoid(S32 bytesPerItem,
			S32 itemsPerChunk) : 
			m_bytesPerItem(bytesPerItem),
			m_bytesPerChunk(bytesPerItem * itemsPerChunk),
	#if !DO_CHUNKLIST_VECTOR
		m_chunkList(NULL),
	#endif
		m_freeList(NULL)
	{
		#if DO_CHUNKLIST_VECTOR
		m_chunkVec.reserve(256);
		#endif
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
		#if DO_CHUNKLIST_VECTOR
		return (int)m_chunkVec.size();
		#else
		int count = 0;
		Chunk * c = m_chunkList;
		while ( c )
		{
			count++;
			c = c->next;
		}
		return count;
		#endif
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
		#if !DO_CHUNKLIST_VECTOR
		Chunk * next;
		#endif
		U8 bytes[1];
	};

	int			m_bytesPerItem;
	int			m_bytesPerChunk;
	
	#if DO_CHUNKLIST_VECTOR
	vector<Chunk *>	m_chunkVec;
	#else
	Chunk  *	m_chunkList;
	#endif
	
	FreeList *	m_freeList;
	int			m_next;

	void AddChunk()
	{
		SINTa chunkSize = ( sizeof(Chunk) + (m_bytesPerChunk-1) );
		Chunk * cur = (Chunk *) OodleMalloc( chunkSize );
		m_next = 0;
		
		#if DO_CHUNKLIST_VECTOR
		m_chunkVec.push_back(cur);
		#else
		cur->next = m_chunkList;
		m_chunkList = cur;
		#endif
	}
		
	void FreeAllChunks()
	{
		SINTa chunkSize = ( sizeof(Chunk) + (m_bytesPerChunk-1) );
	
		#if DO_CHUNKLIST_VECTOR
		for(int i=0;i<(int)m_chunkVec.size();i++)
		{
			OodleFreeSized( m_chunkVec[i] , chunkSize );
		}
		m_chunkVec.clear();
		#else
		Chunk * c = m_chunkList;
		while ( c )
		{
			Chunk * next = c->next;
			OodleFreeSized(c,chunkSize);
			c = next;
		}
		m_chunkList = NULL;
		#endif
		
		m_freeList = NULL;
		m_next = m_bytesPerChunk;
	}
};

//=================================================

OODLE_NS_END



