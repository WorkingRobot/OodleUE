// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_HASH_TABLE_H__
#define __RADRR_HASH_TABLE_H__

#include "rrbase.h"
#include "rrhashfunction.h"
#include "rrvector.h"
#include "rrmath.h"

#ifndef ASSERT
#define ASSERT	RR_ASSERT		// @@ temp
#endif

#define DURING_ASSERT	RR_DURING_ASSERT
#define uint32	U32
#define MAX	RR_MAX
#define intlog2round rrIlog2roundf
#define ftoi		rr_ftoi

/***********

@@ todo : factor out the shared code from find/find_first/find_next

---------------------------------------------------------

hashtable is always "multi"
	eg. you can add many entries with the same key/hash if you like
	eg. the "multi" is not baked into the type, but rather in how you use it

This is a basic Knuth reprobe flat hash table

similar to Rad StringHash
	but I removed the "count" thing here for simplicity

hashtable is built on cb::vector  so it uses StlAlloc

begin() & end() let you walk an iterator over all contents
	if you insert during a head/tail walk the iterator is meaningless, but won't crash
	
entry_type structs are returned to the user
	user can erase() with this handle
	user can stuff anything he wants in ->data
	User can badly screw up the index by changing "hash" or "key" in entry_type !!
	because of this I always give back entry_type as const, but you can const_cast the data

find() is templated on any type of key, not necessarily just the type stored in the hash
	so for example you could store a hash of String but search using "char *"
	BUT if you do this you must ensure that the different types are hashed exactly the same way !
	key_equal() must support the cross-compares
	and hash_function() of the two types must be identical

---------------------------------------------------------

done : could pretty easily do a prime-mod variant as well
	prime-mod is a bit slower to look up cuz of the mod
	but safer cuz it automatically fixes bad quality hashes
	and can fit the desired fill much closer because there are more primes than powers of 2
	-> a lot slower
	
using references in find() and insert() and such causes some problems
 it means you can't pass in temporaries, and there are const annoyances
 but I don't want to unnecessarily copy nontrivial objects either
 
 interesting -
	hash insert should take the parameter by value and then *swap* them in
	vector too
	avoid copying big objects

-> maybe pile all the stuff you need to define into a helper struct ?
	did this, not sure I love it
	

*************/

RR_NAMESPACE_START

#define CB_DEFAULT_HASH_FILL_RATIO	(0.707f) // sqrt(2)/2
//#define CB_DEFAULT_HASH_FILL_RATIO	(0.6f)

//=======================================================================
// hashtableentry_hkd :
//	entry with hash, key & data stored

template <typename t_key,typename t_data,typename t_ops>
struct hashtableentry_hkd
{
	typedef t_key	key_type;
	typedef t_data	data_type;
	typedef t_ops	ops;
	
	t_data		m_data;
	
	// change_data lets you change data on entry_type ; to protect hash & key
	void change_data(const t_data & d) const { const_cast<t_data &>(m_data) = d; }
	t_data & mutable_data() const { return const_cast<t_data &>(m_data); }
	
	const data_type & data() const { return m_data; }
	
	//-----------------------------------------
	
	hash_type	m_hash;
	t_key		m_key;
	
	bool is_empty() const { return ops().is_empty( m_hash, m_key ); }
	bool is_deleted() const { return ops().is_deleted( m_hash, m_key ); }
	
	void make_empty() { ops().make_empty( m_hash, m_key ); }
	void make_deleted() { ops().make_deleted( m_hash, m_key ); }
		
	const key_type  & key()  const { return m_key; }
	hash_type hash() const { return m_hash; }
	
	bool check_hash( const hash_type h ) const { return m_hash == h; }
	
	// entry_type constructs to an is_empty() state
	hashtableentry_hkd() : m_data() , m_hash() , m_key() { make_empty(); }
	
	void set( hash_type h, const key_type & k, const data_type & d )
	{
		m_hash = h;
		m_key = k;
		m_data = d;
	}
};

//=======================================================================
// hashtableentry_kd :
//	entry with key & data stored
//	hash is computed

template <typename t_key,typename t_data,typename t_ops>
struct hashtableentry_kd
{
	typedef t_key	key_type;
	typedef t_data	data_type;
	typedef t_ops	ops;
	
	t_data		m_data;
	
	// change_data lets you change data on entry_type ; to protect hash & key
	void change_data(const t_data & d) const { const_cast<t_data &>(m_data) = d; }
	t_data & mutable_data() const { return const_cast<t_data &>(m_data); }
	
	const data_type & data() const { return m_data; }
	
	//-----------------------------------------
	
	t_key		m_key;
	
	bool is_empty() const   { return ops().is_empty( 0, m_key ); }
	bool is_deleted() const { return ops().is_deleted( 0, m_key ); }
	
	void make_empty()   
	{
		hash_type h; 
		ops().make_empty( h, m_key );
		ASSERT( is_empty() ); 
		ASSERT( ! is_deleted() );
	}
	
	void make_deleted()
	{
		hash_type h;
		ops().make_deleted( h, m_key );
		ASSERT( is_deleted() );
		ASSERT( ! is_empty() );
	}
		
	const key_type  & key()  const { return m_key; }
	hash_type hash() const { return ops().hash_key(m_key); }
	
	bool check_hash( const hash_type h ) const { return true; }
	
	// entry_type constructs to an is_empty() state
	hashtableentry_kd() : m_data() , m_key() { make_empty(); }
	
	void set( hash_type h, const key_type & k, const data_type & d )
	{
		m_key = k;
		m_data = d;
		ASSERT( h == hash() );
	}
};

//-----------------------------------------------------------------
// hashtableentry_hd :
//	entry with hash & data stored
//	key is the same as data

template <typename t_data,typename t_ops>
struct hashtableentry_hd
{
	typedef t_data	key_type;
	typedef t_data	data_type;
	typedef t_ops	ops;
	
	t_data		m_data;
	
	// change_data lets you change data on entry_type ; to protect hash & key
	void change_data(const t_data & d) const { const_cast<t_data &>(m_data) = d; }
	t_data & mutable_data() const { return const_cast<t_data &>(m_data); }
	
	const data_type & data() const { return m_data; }
	
	//-----------------------------------------
	
	hash_type	m_hash;
	
	bool is_empty() const { return ops().is_empty( m_hash, m_data ); }
	bool is_deleted() const { return ops().is_deleted( m_hash, m_data ); }
	
	void make_empty() { ops().make_empty( m_hash, m_data ); }
	void make_deleted() { ops().make_deleted( m_hash, m_data ); }
		
	const key_type  & key()  const { return m_data; }
	hash_type hash() const { return m_hash; }
	
	bool check_hash( const hash_type h ) const { return m_hash == h; }
	
	// entry_type constructs to an is_empty() state
	hashtableentry_hd() : m_data() , m_hash() { make_empty(); }
	
	void set( hash_type h, const key_type & k, const data_type & d )
	{
		m_hash = h;
		ASSERT( k == d );
		m_data = d;
	}
};

//-----------------------------------------------------------------
// hashtableentry_d :
//	entry with data stored
//	key is data
//	hash is computed
//	make_empty/make_deleted must work only on key

template <typename t_data,typename t_ops>
struct hashtableentry_d
{
	typedef t_data	key_type;
	typedef t_data	data_type;
	typedef t_ops	ops;
	
	t_data		m_data;
	
	// change_data lets you change data on entry_type ; to protect hash & key
	void change_data(const t_data & d) const { const_cast<t_data &>(m_data) = d; }
	t_data & mutable_data() const { return const_cast<t_data &>(m_data); }
	
	const data_type & data() const { return m_data; }
	
	//-----------------------------------------
	
	bool is_empty() const   { return ops().is_empty( 0, m_data ); }
	bool is_deleted() const { return ops().is_deleted( 0, m_data ); }
	
	void make_empty()   
	{
		hash_type h; 
		ops().make_empty( h, m_data );
		ASSERT( is_empty() ); 
		ASSERT( ! is_deleted() );
	}
	
	void make_deleted()
	{
		hash_type h;
		ops().make_deleted( h, m_data );
		ASSERT( is_deleted() );
		ASSERT( ! is_empty() );
	}
		
	const key_type  & key()  const { return m_data; }
	hash_type hash() const { return ops().hash_key(m_data); }
	
	bool check_hash( const hash_type h ) const { return true; }
	
	// entry_type constructs to an is_empty() state
	hashtableentry_d() : m_data() { make_empty(); }
	
	void set( hash_type h, const key_type & k, const data_type & d )
	{
		ASSERT( k == d );
		m_data = d;
		ASSERT( h == hash() );
	}
};

//=======================================================================

// could do a default template arg like this :
// = hashtable2_ops<t_key,t_key(0),t_key(1)>

template <typename t_entry> class hashtable
{
public:
	
	typedef t_entry								entry_type;
	typedef t_entry	*							entry_ptr;
	typedef t_entry	const *						entry_ptrc;
	typedef hashtable<t_entry>					this_type;
	
	typedef typename t_entry::ops		ops;
	typedef typename t_entry::key_type			key_type;
	typedef typename t_entry::data_type			data_type;
		
	//-------------------------------------------

	// constructor does no allocations, makes an empty table
	hashtable() : m_hashBits(0), m_hashMask(0), m_numInserts(0), m_numDeleted(0), m_numInsertsRebuild(0), m_hashFillRatio(CB_DEFAULT_HASH_FILL_RATIO)
	 { }
	~hashtable() 
	 { }

	//-------------------------------------------
	
	// "capacity" is the size of the table
	int capacity() const;
	// "reserve" resizes the table for an occupancy of "count" (allocated table size will be larger)
	void reserve(int count);
	// "reserve_initial_size" can only be used for the first reserve ;
	//	 it takes an explicit table size to allocate, not a number of items to reserve space for
	void reserve_initial_size(int table_size);
	
	// clear : make the table empty, but don't resize
	void clear();
	// release : free the table; restores to just-constructed state
	void release();
	// tighten : resize to minimum size (good after many erases)
	void tighten();
	
	// size : # of occupants (not empty or deleted)
	int size() const;
	// bool empty = no valid occupants
	bool empty() const;

	// set_fill_ratio does not cause a resize, but you can call tighten() after if you want
	void  set_fill_ratio(float f);
	float get_fill_ratio() const;
	
	//-------------------------------------------
	// entry_type pointers returned are TEMP
	//	they are invalidated by any further action on the table, DO NOT STORE
	
	// find returns NULL if not found
	template <typename t_findkey>
	const entry_type * find(const hash_type & hash,const t_findkey & key) const;
	
	// insert returns the one just made
	const entry_type * insert(const hash_type & hash,const key_type & key,const data_type & data);

	// insert if not found :
	//	data is used only for insert
	const entry_type * find_or_insert(const hash_type & hash,const key_type & key,const data_type & data,bool * pWasFound = NULL);

	// sedata_type ; add_or_replace ; insert_unique
	const entry_type * insert_or_replace(const hash_type & hash,const key_type & key,const data_type & data);
	
	void erase(const entry_type * entry);

	// aliases :
	
	template <typename t_findkey>
	const entry_type * find(const t_findkey & key) const;
	
	const entry_type * insert(const key_type & key,const data_type & data);
	const entry_type * insert(const entry_type & entry);
		
	const entry_type * insert_or_replace(const key_type & key,const data_type & data);
	
	// mainly for use when you have a no-data entry type
	const entry_type * insert(const key_type & key);
	const entry_type * insert_or_replace(const key_type & key);
	
	//-------------------------------------------
	// funny "end"
	//  this is an easy way to make an end() that compares to either a find_iterator or walk_iterator

	//#define INT_MAX 2147483647 
	typedef enum { eHash_End = 2147483647 } t_end;
	
	t_end end() const { return eHash_End; }
	
	//-------------------------------------------
	// multi finder :
	//  get a find_iterator from find_first
	//		then use ++ on it to step
	
	class find_iterator
	{
	public:
		find_iterator() : owner(NULL), index(0), probeCount(0), hash(), key() { }
		find_iterator(const this_type * _owner,int _index,int _probeCount, hash_type _hash, key_type _key) : 
			owner(_owner), index(_index), probeCount(_probeCount), hash(_hash), key(_key) { }
		~find_iterator() { }	
	
		const entry_type * operator -> () const { ASSERT( owner != NULL); return   owner->iterator_at(index); }
		const entry_type & operator *  () const { ASSERT( owner != NULL); return * owner->iterator_at(index); }
	
		// a few possibilities for how to expose the access
		//operator bool () const { return owner != NULL; }
		//const entry_type * ptr() const { return owner->iterator_at(index); }
		
		operator const entry_type * () const { return owner ? owner->iterator_at(index) : NULL; }
		
		// pre-increment :
		find_iterator & operator ++ () 
		{
			ASSERT( owner != NULL && *this != owner->end() ); 
			owner->find_next(this); 
			return *this; 
		}
		
		bool operator == (const t_end & e) const { return owner == NULL; }
		bool operator != (const t_end & e) const { return owner != NULL; }
		
		const entry_type * get_ptr() const { ASSERT( owner != NULL); return owner->iterator_at(index); }
		
	private:
	
		void set_null() { owner = NULL; }
		void update(int _index,int _probeCount) { index = _index; probeCount = _probeCount; }
		
		friend class hashtable<t_entry>;
		const this_type * owner;
		int	index;
		int probeCount;
		
		// find_iterator could store the hash & key as well since you must find_next with the same vals
		//	but better not to in case key is expensive to copy (?)
		hash_type hash;
		key_type	key;
	};	
	
	// call find_first , the check find_iterator != NULL and keep calling find_next
	const find_iterator find_first(const hash_type & hash,const key_type & key) const;
	//const find_iterator find_next(const find_iterator & finder,const hash_type & hash,const key_type & key) const;
	bool find_next(find_iterator * finder) const;
	
	//-------------------------------------------
	// iterator for walking over all members in the hash :
	//	skips empty & deleted entries
		
	class walk_iterator
	{
	public:
		walk_iterator() : owner(NULL), index(0) { }
		walk_iterator(const this_type * _owner,int _index) : owner(_owner), index(_index) { }
		~walk_iterator() { }
		// default copy-construct and such is good
		
		// pre-increment :
		walk_iterator & operator ++ ()
		{
			ASSERT( owner != NULL && *this != owner->end() ); 
			index = owner->iterator_next(index);
			return *this;
		}
		/*
		// post-increment :
		walk_iterator & operator ++ (int)
		{
			walk_iterator save(*this);
			index = owner->iterator_next(index);
			return save;
		}
		*/
	
		const entry_type * operator -> () const { ASSERT( owner != NULL); return owner->iterator_at(index); }
		const entry_type & operator * () const  { ASSERT( owner != NULL); return * owner->iterator_at(index); }
		
		bool operator == (const walk_iterator & rhs) const { return owner == rhs.owner && index == rhs.index; }
		bool operator != (const walk_iterator & rhs) const { return ! (*this == rhs); }
	
		bool operator == (const t_end & e) const { return index == eHash_End; }
		bool operator != (const t_end & e) const { return index != eHash_End; }
		
		const entry_type * get_ptr() const { ASSERT( owner != NULL); return owner->iterator_at(index); }
		
	private:
		friend class hashtable<t_entry>;
		const this_type *	owner;
		int			index;
	};
	
	// head/tail like begin/end :
	//	for walking over all members
	walk_iterator begin() const;
	//walk_iterator end() const;
	
	const entry_type * iterator_at(const int it) const;
	int iterator_next(const int it) const;
	
	//-------------------------------------------
	
private:

	void rebuild_table(int newSize);

	int table_index(const hash_type hash) const;
	int table_reindex(const int index,const hash_type hash,const int probeCount) const;

	vector<entry_type>	m_table;
	uint32		m_hashBits;
	uint32		m_hashMask; // (1<<hashBits)-1
	uint32		m_numInserts;		// occupancy is the number of *inserts* (not decremented when you remove members)
	uint32		m_numDeleted;		// actual number of items = occupancy - numDeleted
	uint32		m_numInsertsRebuild; //,occupancyDownSize;
	
	float		m_hashFillRatio;
};

//=========================================================================================

#define T_PRE1	template <typename t_entry>
#define T_PRE1_FINDKEY	template <typename t_entry> template <typename t_findkey>
#define T_PRE2	hashtable<t_entry>
#define T_TABLEENTRY	t_entry
#define T_KEY		typename hashtable<t_entry>::key_type
#define T_DATA		typename hashtable<t_entry>::data_type
#define T_ITERATOR	typename hashtable<t_entry>::walk_iterator
#define T_FINDER	typename hashtable<t_entry>::find_iterator

T_PRE1 int T_PRE2::size() const
{
	return m_numInserts - m_numDeleted;
}

T_PRE1 int T_PRE2::capacity() const
{
	return (int) m_table.size();
}

T_PRE1 bool T_PRE2::empty() const
{
	return size() == 0;
}

T_PRE1 void T_PRE2::reserve(int count)
{
	if ( count <= capacity() )
		return;
	
	rebuild_table(count);
}

T_PRE1 void T_PRE2::clear()
{
	// clear out table but don't free :
	m_numInserts = 0;
	m_numDeleted = 0;
	for(size_t i=0;i<m_table.size();i++)
	{
		//ops().make_empty( 
		m_table[i].make_empty();
		//m_table[i].data = data_type();
	}
}	

T_PRE1 void T_PRE2::release()
{
	m_table.release();
	m_hashBits = 0;
	m_hashMask = 0;
	m_numInserts = 0;
	m_numDeleted = 0;
	m_numInsertsRebuild = 0;
}

T_PRE1 void T_PRE2::tighten()
{
	if ( m_numDeleted == 0 )
		return;
	rebuild_table(0);
}

T_PRE1 void T_PRE2::set_fill_ratio(float f)
{
	m_hashFillRatio = f;
	m_numInsertsRebuild = ftoi( (m_hashMask+1) * m_hashFillRatio );
}

T_PRE1 float T_PRE2::get_fill_ratio() const 
{
	return m_hashFillRatio; 
}
	
T_PRE1 int T_PRE2::table_reindex(const int index,const hash_type hash,const int probeCount) const
{
	// triangular step :
	return (int) ( ( index + probeCount ) & (m_hashMask) );
}

T_PRE1 int T_PRE2::table_index(const hash_type hash) const
{
	// xor fold :
	return (int) ( ( (hash>>16) ^ hash ) & (m_hashMask) );
}
	
T_PRE1 void T_PRE2::rebuild_table(int newOccupancy)
{
	if ( m_table.empty() )
	{
		// first time :
		const int c_minHashCount = 16; // this will turn into 64 after the rounding up below ...
		newOccupancy = MAX(newOccupancy, c_minHashCount );

		// newSize = ( newOccupancy / m_hashFillRatio ) would put us right at the desired fill
		//	so bias up a little bit (assume more inserts will follow)

		// alternative way : just bias a little but ceil the log2
		//float newSize = 1.1f * newOccupancy / m_hashFillRatio;
		//m_hashBits = intlog2ceil(newSize);
		
		float newSize = 1.5f * newOccupancy / m_hashFillRatio;
		m_hashBits = intlog2round(newSize);
				
		int hashSize = 1<<(m_hashBits);

		// because we did intlog2round we may have dropped the size below acceptable
		if ( (hashSize * m_hashFillRatio) < newOccupancy * 1.1f )
		{
			m_hashBits ++;
			hashSize *= 2;
		}

		m_hashMask = hashSize-1;
		
		entry_type zero; // = { 0 };
		m_table.resize(hashSize,zero);
		
		m_numInserts = 0;
		m_numDeleted = 0;
		m_numInsertsRebuild = ftoi( hashSize * m_hashFillRatio );
		
		// make sure that just reinserting the existing items won't cause a rebuild :
		ASSERT( m_numInsertsRebuild > (uint32)newOccupancy );
	}
	else
	{
		// rebuild :
		
		vector<entry_type>	old_table;
		m_table.swap(old_table);
		ASSERT( m_table.empty() );
		
		int occupancy = m_numInserts - m_numDeleted + 16;
		newOccupancy = MAX(occupancy,newOccupancy);
		// call self : should get the other branch
		rebuild_table(newOccupancy);
		
		// not true if I'm rebuilding because of deletes
		//RR_ASSERT( sh->hashMask > newSize );
		//RR_ASSERT( sh->occupancyUpSize > newSize );
		
		for(int i=0;i<old_table.size32();i++)
		{
			if ( old_table[i].is_empty() ||
				old_table[i].is_deleted() )
			{
				continue;
			}

			insert(old_table[i]);
		}
		
		// old_table descructs here
	}
}

T_PRE1 void T_PRE2::reserve_initial_size(int size )
{
	ASSERT( m_table.empty() );

	// first time :
	const int c_minHashCount = 16; // this will turn into 64 after the rounding up below ...
	size = MAX(size, c_minHashCount );

	m_hashBits = rrIlog2ceil(size);
				
	int hashSize = 1<<(m_hashBits);

	ASSERT( hashSize >= size );

	m_hashMask = hashSize-1;
		
	entry_type zero; // = { 0 };
	m_table.resize(hashSize,zero);
		
	m_numInserts = 0;
	m_numDeleted = 0;
	m_numInsertsRebuild = ftoi( hashSize * m_hashFillRatio );		
}

T_PRE1_FINDKEY const T_TABLEENTRY * T_PRE2::find(const hash_type & hash,const t_findkey & key) const
{
	// don't ever try to find the magic values :
	ASSERT( ! ops().is_deleted(hash,key) && ! ops().is_empty(hash,key) );
	
	if ( m_hashMask == 0 )
		return NULL;
	
	int index = table_index(hash);
	int probeCount = 1;
	
	const entry_type * table = m_table.begin();
		
	while ( ! table[index].is_empty() )
	{
		if ( table[index].check_hash(hash) && 
			! table[index].is_deleted() &&
			ops().key_equal(table[index].key(),key) )
		{
			// found it
			return &table[index];
		}
	
		index = table_reindex(index,hash,probeCount);
		probeCount++;
	}
	
	return NULL;
}

T_PRE1 const T_TABLEENTRY * T_PRE2::insert(const hash_type & hash,const key_type & key,const data_type & data)
{
	// don't ever try to insert the magic values :
	DURING_ASSERT( entry_type test_entry; test_entry.set(hash,key,data); );
	ASSERT( ! test_entry.is_deleted() && ! test_entry.is_empty() );
	
	// this triggers the first table build when m_numInserts == m_numInsertsRebuild == 0
	if ( m_numInserts >= m_numInsertsRebuild )
	{
		rebuild_table(0);
	}
	m_numInserts ++;

	int index = table_index(hash);
	int probeCount = 1;
		
	entry_type * table = m_table.begin();
	
	while ( ! table[index].is_empty() &&
			! table[index].is_deleted() )
	{
		// @@ I could check to see if this exact same object is in here already and just not add it
		//	return pointer to the previous
		// currently StringHash is a "multi" hash - you can add the same key many times with different data
		//	and get them out with Find_Start / Find_Next
	
		//s_collisions++;
		index = table_reindex(index,hash,probeCount);
		probeCount++;
	}
		
	table[index].set( hash, key, data );
	
	return &table[index];
}

T_PRE1 const T_TABLEENTRY * T_PRE2::find_or_insert(const hash_type & hash,const key_type & key,const data_type & data,bool * pWasFound)
{
	// don't ever try to insert the magic values :
	DURING_ASSERT( t_entry test_entry; test_entry.set(hash,key,data); );
	ASSERT( ! test_entry.is_deleted() && ! test_entry.is_empty() );
	
	if ( m_hashMask == 0 )
	{
		rebuild_table(0);
		if ( pWasFound ) *pWasFound = false;
		return insert(hash,key,data);
	}
	
	int index = table_index(hash);
	int probeCount = 1;
	int deleted_index = -1;
	
	entry_type * table = m_table.begin();
		
	while ( ! table[index].is_empty() )
	{
		if ( table[index].is_deleted() )
		{
			// save the first deleted index in case we insert
			if ( deleted_index == -1 )
				deleted_index = index;
		}
		else if ( table[index].check_hash(hash) &&
			ops().key_equal(table[index].key(),key) )
		{
			// found it
			if ( pWasFound ) *pWasFound = true;
			return &table[index];
		}
	
		index = table_reindex(index,hash,probeCount);
		probeCount++;
	}
	
	// not found, insert :
	
	if ( pWasFound ) *pWasFound = false;
			
	// this triggers the first table build when m_numInserts == m_numInsertsRebuild == 0
	if ( m_numInserts >= m_numInsertsRebuild )
	{
		rebuild_table(0);
		// have to rescan because table changed :
		return insert(hash,key,data);
	}
	m_numInserts ++;
	
	if ( deleted_index >= 0 )
	{
		index = deleted_index;
	}
	
	ASSERT( table[index].is_empty() || table[index].is_deleted() );
	
	table[index].set( hash, key, data );
	
	return &table[index];
}

T_PRE1 const T_TABLEENTRY * T_PRE2::insert_or_replace(const hash_type & hash,const key_type & key,const data_type & data)
{
	const entry_type * te = find_or_insert(hash,key,data);
	ASSERT( te );
	// @@ if not inserted
	te->change_data( data );
	return te;
}

T_PRE1 void T_PRE2::erase(const entry_type * entry)
{
	ASSERT( entry >= m_table.begin() && entry < m_table.end() );
	entry_type * el = const_cast< entry_type *>(entry);

	el->make_deleted();
	//ops().make_deleted(el->hash,el->key);	
	//WARNING : leave el->hs.hash the same for count !!
	
	m_numDeleted++;
	
	// do NOT decrease occupancy
}

// aliases :

T_PRE1_FINDKEY const T_TABLEENTRY * T_PRE2::find(const t_findkey & key) const
{
	return find(ops().hash_key(key),key);
}

T_PRE1 const T_TABLEENTRY * T_PRE2::insert(const key_type & key,const data_type & data)
{
	return insert(ops().hash_key(key),key,data);
}

T_PRE1 const T_TABLEENTRY * T_PRE2::insert_or_replace(const key_type & key,const data_type & data)
{
	return insert_or_replace(ops().hash_key(key),key,data);
}

T_PRE1 const T_TABLEENTRY * T_PRE2::insert(const entry_type & entry)
{
	return insert(entry.hash(),entry.key(),entry.data());
}

T_PRE1 const T_TABLEENTRY * T_PRE2::insert(const key_type & key)
{
	return insert(ops().hash_key(key),key,key);
}

T_PRE1 const T_TABLEENTRY * T_PRE2::insert_or_replace(const key_type & key)
{
	return insert_or_replace(ops().hash_key(key),key,key);
}
	
T_PRE1 T_ITERATOR T_PRE2::begin() const
{
	walk_iterator it(this,-1);
	++it;
	return it;
}

/*
// tail is *past* the last valid one
T_PRE1 T_ITERATOR T_PRE2::tail() const
{
	iterator it(this,m_table.size());
	return it;
}
*/
	
T_PRE1 const T_TABLEENTRY * T_PRE2::iterator_at(const int it) const
{
	if ( it < 0 || it >= m_table.size32() ) return NULL;
	const entry_type * E = & m_table[it];
	ASSERT( ! E->is_deleted() && ! E->is_empty() );
	return E;
}

T_PRE1 int T_PRE2::iterator_next(int _index) const
{
  int index = _index;

	// step past last :
	++index;
	
	// don't go past tail
	int size = m_table.size32();
	if ( index >= size ) return eHash_End; // return tail;
	
	// look for a non-empty one :
	while ( m_table[index].is_empty() ||
			m_table[index].is_deleted() )
	{
		++index;
		if ( index >= size ) return eHash_End; // return tail;
	}
	return index;
}

// WARNING : find_first : big code dupe with ::find - keep in sync !!
T_PRE1 const T_FINDER T_PRE2::find_first(const hash_type & hash,const key_type & key) const
{
	// don't ever try to find the magic values :
	ASSERT( ! ops().is_deleted(hash,key) && ! ops().is_empty(hash,key) );
	
	if ( m_hashMask == 0 )
		return find_iterator(); // NULL
		
	int index = table_index(hash);
	int probeCount = 1;
	
	const entry_type * table = m_table.begin();
		
	while ( ! table[index].is_empty() )
	{
		if ( table[index].check_hash(hash) &&
			! table[index].is_deleted() &&
			ops().key_equal(table[index].key(),key) )
		{
			// found it
			return find_iterator(this,index,probeCount,hash,key);
		}
	
		index = table_reindex(index,hash,probeCount);
		probeCount++;
	}
	
	return find_iterator(); // NULL
	//return find_iterator(NULL,0,probeCount); // NULL
}

// WARNING : find_next : big code dupe with ::find - keep in sync !!
//T_PRE1 const T_FINDER T_PRE2::find_next(const find_iterator & finder,const hash_type & hash,const key_type & key) const
T_PRE1 bool T_PRE2::find_next(find_iterator * finder) const
{
	ASSERT( finder->owner == this );
	ASSERT( m_hashMask != 0 );
	
	int index = finder->index;
	int probeCount = finder->probeCount;
	hash_type hash = finder->hash;
	
	index = table_reindex(index,hash,probeCount);
	probeCount++;
		
	const entry_type * table = m_table.begin();
		
	while ( ! table[index].is_empty() )
	{
		if ( table[index].check_hash(hash) &&
			! table[index].is_deleted() &&
			ops().key_equal(table[index].key(),finder->key) )
		{
			// found it
			finder->update(index,probeCount);
			return true;
		}
	
		index = table_reindex(index,hash,probeCount);
		probeCount++;
	}
	
	finder->set_null();
	return false;
}
	
	
#undef T_PRE1
#undef T_PRE2
#undef T_TABLEENTRY
#undef T_ITERATOR
#undef T_FINDER

//-------------------
// old type :

template <typename t_key,typename t_data,typename t_ops> class hash_table :
	public hashtable< hashtableentry_hkd<t_key,t_data,t_ops> >
{
public:

	hash_table() { }
	~hash_table() { }
};


RR_NAMESPACE_END

#endif
