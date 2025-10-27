// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.


#include "templates/rrvector.h"

/*****

chunkstack just provides pop_back/push_back

it's a series of arrays
so you never do big realloc/copies like vector
you just add another array on the end

*****/

template <typename t_entry, int t_maxchunksizelog2> class chunk_stack
{
public:
	enum { t_maxchunksize = 1<<t_maxchunksizelog2 };

	//----------------------------------------------------------------------
	// typedefs :
	typedef t_entry				value_type;
	typedef t_entry&			reference;
	typedef const t_entry&		const_reference;
	typedef size_t				size_type;
	
	typedef rr::vector<t_entry>	chunk_type;
	typedef chunk_stack<t_entry,t_maxchunksizelog2>	this_type;

	//----------------------------------------------------------------------
	// constructors

	chunk_stack()
	{
		init();
	}
	
	~chunk_stack()
	{
		kill();
	}
	
	void release() { kill(); init(); }
	void clear() { release(); }
	
	//=======================================================
	
	
	//---------------------------------------------------------------------------
	// simple accessors :

	// at() with range check
	reference at(const size_type i)
	{
		CB_ASSERT( i >= 0 && i < m_size );
		return *(parent_type::begin() + i); 
	}
	const_reference at(const size_type i) const
	{
		CB_ASSERT( i >= 0 && i < parent_type::size() );
		return *(parent_type::begin() + i); 
	}

	// operator[]
	reference		operator[](const size_type i)		{ return at(i); }
	const_reference operator[](const size_type i) const { return at(i); }

	// front() and back()
	reference		front()			{ RR_ASSERT( ! empty() ); return m_chunkArray[0]->front(); }
	const_reference front() const	{ RR_ASSERT( ! empty() ); return m_chunkArray[0]->front(); }
	reference		back()			{ RR_ASSERT( ! empty() ); return m_back->back(); }
	const_reference back() const	{ RR_ASSERT( ! empty() ); return m_back->back(); }

	// size queries :
	size_type	size() const		{ return m_size; }
	bool		empty() const		{ return m_size == 0; }
	size_type	capacity() const	{ return m_size; }
	size_type	max_size() const	{ return (size_t)(-1); }

	int			size32() const		{ return rr::check_value_cast<int>(size()); }
	
	void reserve(const size_type newcap)	{ parent_type::reserve(newcap); }

	//---------------------------------------------------------------------------
	// mutators :

	void push_back(const value_type & e)
	{
		if ( m_back->size() >= t_maxchunksize )
		{
			push_back_chunk();
		}
		m_back->push_back(e);
		m_size++;
	}

	void push_back()
	{
		if ( m_back->size() >= t_maxchunksize )
		{
			push_back_chunk();
		}
		m_back->push_back();
		m_size++;
	}

	void pop_back()
	{
		RR_ASSERT( ! empty() );
		RR_ASSERT( ! m_back->empty() );
		m_back->pop_back();
		m_size--;
		if ( m_back->empty() )
		{
			pop_back_chunk();
		}
	}
	
	//=======================================================
	// iterator support

	class iterator
	{
	public:
		iterator() : owner(NULL), index(0) { }
		iterator(this_type * _owner,int _index) : owner(_owner), index(_index) { }
		~iterator() { }
		// default copy-construct and such is good
		
		// pre-increment :
		iterator & operator ++ ()
		{
			ASSERT( owner != NULL && *this != owner->end() ); 
			++index;
			return *this;
		}
	
		value_type * operator -> () { ASSERT( owner != NULL); return owner->at(index); }
		value_type & operator * ()  { ASSERT( owner != NULL); return * owner->at(index); }
		
		bool operator == (iterator & rhs) { return owner == rhs.owner && index == rhs.index; }
		bool operator != (iterator & rhs) { return owner != rhs.owner || index != rhs.index; }
	
	private:
		this_type *	owner;
		int			index;
	};
	
	class const_iterator
	{
	public:
		const_iterator() : owner(NULL), index(0) { }
		const_iterator(const this_type * _owner,int _index) : owner(_owner), index(_index) { }
		~const_iterator() { }
		// default copy-construct and such is good
		
		// pre-increment :
		const_iterator & operator ++ ()
		{
			ASSERT( owner != NULL && *this != owner->end() ); 
			++index;
			return *this;
		}
	
		const value_type * operator -> () const { ASSERT( owner != NULL); return owner->at(index); }
		const value_type & operator * () const  { ASSERT( owner != NULL); return * owner->at(index); }
		
		bool operator == (const const_iterator & rhs) const { return owner == rhs.owner && index == rhs.index; }
		bool operator != (const const_iterator & rhs) const { return owner != rhs.owner || index != rhs.index; }
	
	private:
		const this_type *	owner;
		int			index;
	};
	
	iterator		begin()			{ return iterator(this,0); }
	const_iterator	begin() const	{ return const_iterator(this,0); }
	iterator		end()			{ return iterator(this,m_size); }
	const_iterator	end() const		{ return const_iterator(this,m_size); }

	//=======================================================
	
private:

	void init()
	{
		m_chunkArray.resize(1);
		m_chunkArray[0] = OodleNew(chunk_type);
		m_back = m_chunkArray[0];
		m_back->reserve(64);
		m_size = 0;
	}
	
	void kill()
	{
		for(int i=0;i<m_chunkArray.size32();i++)
		{
			chunk_type * ptr = m_chunkArray[i];
			OodleDelete(ptr);
		}
		m_chunkArray.release();
		m_back = NULL;
		m_size = 0;
	}
	
	void push_back_chunk()
	{
		// the old back should be full :
		RR_ASSERT( m_back->size() == t_maxchunksize );
		RR_ASSERT( m_back->capacity() == t_maxchunksize );
		
		m_back = OodleNew(chunk_type);
		m_chunkArray.push_back(m_back);
	}
	
	void pop_back_chunk()
	{
		RR_ASSERT( m_back->empty() );
		RR_ASSERT( m_chunkArray.size() >= 1 );
		RR_ASSERT( m_chunkArray.back() == m_back );
		if ( m_chunkArray.size() == 1 )
		{
			return; // don't pop if only one
		}
		OodleDelete(m_back);
		m_chunkArray.pop_back();
		m_back = m_chunkArray.back();
	}

	//=====================================================
private:
	
	rr::vector< chunk_type * >	m_chunkArray;
	chunk_type * m_back;
	SINTa	m_size;
	
	/**
	
	m_size is the total of all chunk sizes
	m_back = m_chunkArray.back();
	
	by convention, m_back is not empty unless the whole chunkstack is empty
	thus you can always get the last by doing m_back->back()
	
	**/
};

