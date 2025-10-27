// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrbase.h"
#include "rrmem.h"
#include "oodlemalloc.h"
#include "oodlelzpub.h"
#include "rrprefetch.h"
#include "cbradutil.h"
#include "rrarenaallocator.h"
#include "threadprofiler.h"
#include "rrsimd.h"
#include <string.h>

OODLE_NS_START

/****

CTMF

Cache Table Match Finder

mml = 3 or 4
optional second hash

slide-down
stores hash & pos

prefetches next rows
when you prefetch next row, it stores the pointers
so if you do an optimal step like pos++
the pointers are there ready for you

-------

c_table_depth is the # of U32's for each hash slot
you only get half as many pointers currently
because [0] = pos, [1] = hash check

-------

could do a more compact U32 value
with something like 28 bits of offset + 4 bits of hash check
(or 27-5 ?)
gives you more entries per cache line, more chance of the whole table staying in cache

****/

//=======================================================================

namespace
{

#define HASH_U64_MULTIPLIER	 0xCF1BBCDCB7A56463ULL

// Shifting the multiplier up makes some of the high bytes of the input not matter
// we use LE loads so these bytes are the later ones
#define CTMF_HASH_MULTIPLIER(len) \
	((len) <= 0 ? 0 : HASH_U64_MULTIPLIER << ((8 - RR_CLAMP(len,1,8)) * 8))

static RADFORCEINLINE U64 hash_bytes_64(const U8 * ptr, U64 mult)
{
	U64 x = RR_GET64_LE_UNALIGNED(ptr) * mult;
	return x;
}

//=========================================

template <int t_len>
static RADFORCEINLINE UINTa hash_n_bytes(const U8 * ptr,int bits)
{
	U64 x = hash_bytes_64(ptr, CTMF_HASH_MULTIPLIER(t_len));
	return (UINTa)(x >> (64 - bits));
}

};

//=======================================================================

// verified, having some hash check bits helps speed :
//#define CTMF_POS_BITS	30 // 16.46 mb/s
#define CTMF_POS_BITS	26 // 17.70 mb/s
//#define CTMF_POS_BITS	25 // 18.52 mb/s
#define CTMF_POS_MASK	(((U32)1<<CTMF_POS_BITS)-1)
#define CTMF_CHECK_MASK		(~CTMF_POS_MASK)
	
template <typename t_hashtype,int t_table_depth_bits, int t_second_hash_len, int t_mml>
struct CTMF
{
	enum { c_table_depth_bits = t_table_depth_bits };
	enum { c_table_depth = 1<<t_table_depth_bits };
	enum { c_do_second_hash = t_second_hash_len>0 };
	enum { c_is_ctmf2 = 0 };
	enum { c_max_preload_pos_bits = CTMF_POS_BITS };
	
	RR_COMPILER_ASSERT( t_table_depth_bits >= 0 );
	
	typedef t_hashtype hash_type;
	
	rrArenaAllocator* m_hash_table_arena = nullptr;
	rrArenaArray<t_hashtype> m_hash_table;
	int m_table_size_bits = 0;
	U32 m_hash_row_mask = 0; 
	const U8 * m_base_ptr = nullptr;
	
	const U8 * m_next_ptr = nullptr;
	t_hashtype * m_next_row = nullptr;
	t_hashtype * m_next_row_second = nullptr;
	U64 m_hash_mul = 0;
	U32 m_next_hash = 0; // <- not used for indexing, only used for hash check values
		// (second hash uses the same m_next_hash value)
	
	CTMF()
	{
	}
	
	~CTMF()
	{
		release();
	}
		
	static SINTa get_mem_size(int table_size_bits, int hash2_bits, int chain_bits)
	{
		SINTa alloc_size = sizeof(t_hashtype)*(((SINTa)1<<table_size_bits));
		return alloc_size;
	}

	// Determines the negative match offset (i.e. match distances are negative
	// because they refer backwards, not positive as is more common for LZs)
	// from a CTMF table entry.
	static SINTa resolve_neg_offset(t_hashtype table_entry, U32 pos32)
	{
		// Low bits of CTMF entry give low CTMF_POS_BITS of position that hash was last seen.
		// Resolve the smallest negative match offset that matches those position bits.
		//
		// This computation keeps the low bits of table_entry - pos and forces all the high bits
		// (bit CTMF_POS_BITS and up) to be 1, which means we always get results in the interval
		// [-2^CTMF_POS_BITS,-1], which is exactly what we want.
		return SINTa(table_entry - pos32) | ~SINTa(CTMF_POS_MASK);
	}
	
	void allocate(int table_size_bits,int /*hash2_bits*/,int /*chain_bits*/,rrArenaAllocator * arena,int hash_len)
	{
		m_table_size_bits = table_size_bits;
		m_hash_row_mask = (1<<table_size_bits) - (c_table_depth);
		m_next_ptr = NULL;

		if (hash_len <= 0)
			hash_len = t_mml;
		RR_ASSERT( hash_len >= 3 && hash_len <= 8 );
		m_hash_mul = CTMF_HASH_MULTIPLIER(hash_len);
		
  		rrPrintf_v2("CTMF table bits : %d ; alloc size : %d kb (arena avail : %d kb)\n",
  			table_size_bits,(int)((sizeof(t_hashtype)*(((SINTa)1<<m_table_size_bits)))>>10),
			arena ? (int)(arena->GetCurAvail()>>10) : 0);

		// align to cache line so hash ways are in one line :
		m_hash_table.Allocate(arena, (SINTa)1 << m_table_size_bits, RR_CACHE_LINE_SIZE);
		m_hash_table_arena = arena;
		
		reset();
	}
	
	void release()
	{
		/* // dump usage stats
		if (m_hash_table.m_ptr)
		{
			SINTa nempty_rows = 0;
			SINTa nempty_slots = 0;
			SINTa nslots = SINTa(1) << m_table_size_bits;
			SINTa nrows = nslots / c_table_depth;
			for (SINTa i = 0; i < nslots; i += c_table_depth)
			{
				SINTa n0 = 0;
				for (SINTa j = 0; j < c_table_depth; ++j)
					n0 += m_hash_table[i + j] == 0;

				nempty_slots += n0;
				nempty_rows += n0 == c_table_depth;
			}
			rrprintf("%zd/%zd slots populated (%.2f%%)\n", nslots - nempty_slots, nslots, 100.0 - 100.0 * nempty_slots / nslots);
			rrprintf("%zd/%zd rows populated (%.2f%%)\n", nrows - nempty_rows, nrows, 100.0 - 100.0 * nempty_rows / nrows);
		}
		*/

		m_hash_table.Release(m_hash_table_arena, (SINTa)1 << m_table_size_bits, RR_CACHE_LINE_SIZE);
	}
	
	void reset()
	{
		m_hash_table.Zero((SINTa)1 << m_table_size_bits);
	}
	
	void set_base(const void *base)
	{
		m_base_ptr = U8_void(base);
	}
	
	#if 1
	// old :
	void set_base_and_preload(const void *base, const void * preload_upto, SINTa maxPreloadLen)
	{
		m_base_ptr = U8_void(base);
		
		if ( preload_upto != base )
		{
			THREADPROFILESCOPE("ctmf_preload");
		
			const U8 * ptr = U8_void(base);
			const U8 * ptrEnd = U8_void(preload_upto);
			SINTa preload_len = rrPtrDiff(ptrEnd - ptr);
			RR_ASSERT_ALWAYS( preload_len > 0 );
			if ( preload_len > maxPreloadLen )
			{
				preload_len = maxPreloadLen;
				ptr = ptrEnd - maxPreloadLen;
			}
			SINTa step = preload_len >> 18;
			step = RR_MAX(step,2);
			SINTa numThisStep = (preload_len/2)/step;
			
			set_next(ptr);
				
			for(;;)
			{
				if ( --numThisStep <= 0 )
				{
					if ( ptr >= ptrEnd )
						break;
					step >>= 1;
					RR_ASSERT_ALWAYS( step >= 1 ); 
					numThisStep = (rrPtrDiff(ptrEnd - ptr))/step;
					if ( step > 1 )
						numThisStep /= 2;
				}
				
				// next should be already set
				RR_ASSERT( m_next_ptr == ptr );
				U32 pos = (U32) rrPtrDiff( ptr - m_base_ptr );
				t_hashtype * row = m_next_row;
				t_hashtype * row_second = m_next_row_second;
				U32 hash = m_next_hash;
				
				ptr += step;
				// @@ really need to be prefetching 3 ahead or something
				//	-> tried it, can't find the win
				//prefetch_next(ptr);
				set_next(ptr);
				
				insert(row,pos,hash);
				if ( c_do_second_hash )
				{
					insert(row_second,pos,hash);
				}
			}
			RR_ASSERT( ptr == ptrEnd );
		}
	}
	#else
	// new way (prefetch ahead)
	void set_base_and_preload(const void *base, const void * preload_upto, SINTa maxPreloadLen)
	{
		m_base_ptr = U8_void(base);
		
		if ( preload_upto > base )
		{
			THREADPROFILESCOPE("ctmf_preload");
		
			const U8 * ptr = U8_void(base);
			const U8 * ptrEnd = U8_void(preload_upto);
			SINTa preload_len = rrPtrDiff(ptrEnd - ptr);
			if ( preload_len > maxPreloadLen )
			{
				preload_len = maxPreloadLen;
				ptr = ptrEnd - maxPreloadLen;
			}
			//SINTa step = preload_len >> 17;
			//step = RR_CLAMP(step,2,64);
			SINTa step = preload_len >> 18;
			step = RR_MAX(step,2);
			SINTa numThisStep = (preload_len/2)/step;
			
			for(;;)
			{						
				if ( --numThisStep <= 0 )
				{
					if ( ptr >= ptrEnd )
						break;
					
					step >>= 1;
					RR_ASSERT_ALWAYS( step >= 1 ); 
					numThisStep = (rrPtrDiff(ptrEnd - ptr))/step;
					if ( step > 1 )
						numThisStep /= 2;
				}
				
				// prefetch :
				// @@ seems to be no win here !? faster with no prefetch
				enum { prefetch_ahead = 0 };
				//enum { prefetch_ahead = 4 };
				if ( prefetch_ahead )
				{
					if ( (ptrEnd - ptr) > prefetch_ahead*step )
					{
						const U8 * prefetch_ptr = ptr + prefetch_ahead*step;

						U32 h = hash(prefetch_ptr);
						U32 index = h & m_hash_row_mask;
						t_hashtype * row = m_hash_table + index;
						
						if ( c_table_depth*sizeof(t_hashtype) > 32 )
							RR_PREFETCHRW_64B(row);
						else
							RR_PREFETCHRW_32B(row);
				
						if ( c_do_second_hash )
						{
							UINTa index2 = second_hash_index(h,prefetch_ptr);
							row = m_hash_table + index2;
							
							if ( c_table_depth*sizeof(t_hashtype) > 32 )
								RR_PREFETCHRW_64B(row);
							else
								RR_PREFETCHRW_32B(row);
						}			
					}
				}
				
				// next should be already set
				U32 pos = (U32) rrPtrDiff( ptr - m_base_ptr );
				U32 h = hash(ptr);
				U32 index = h & m_hash_row_mask;
				t_hashtype * row = m_hash_table + index;
				insert(row,pos,h);
				if ( c_do_second_hash )
				{
					UINTa index2 = second_hash_index(h,ptr);
					row = m_hash_table + index2;
					insert(row,pos,h);
				}
				
				ptr += step;
			}
			RR_ASSERT( ptr == ptrEnd );
		}
	}
	#endif
		
	void prefetch_next(const U8 * ptr)
	{
		set_next(ptr);
		
		if ( c_table_depth*sizeof(t_hashtype) > 32 ) // 16*4 == 64
		{
			RR_PREFETCHRW_64B(m_next_row);
			if ( c_do_second_hash )
			{
				RR_PREFETCHRW_64B(m_next_row_second);
			}
		}
		else
		{
			RR_PREFETCHRW_32B(m_next_row);
			if ( c_do_second_hash )
			{
				RR_PREFETCHRW_32B(m_next_row_second);
			}
		}
	}
	
	void set_next(const U8 * ptr)
	{
		m_next_ptr = ptr;
		U32 h = hash(ptr);
		m_next_hash = h;
		U32 index = h & m_hash_row_mask;
		m_next_row = m_hash_table.data() + index;
		if ( c_do_second_hash )
		{
			UINTa index2 = second_hash_index(h,ptr);
			//next_hash_second = h2;
			//U32 index2 = h2 & m_hash_row_mask;
			m_next_row_second = m_hash_table.data() + index2;
		}
	}
		
	void step_and_insert(const U8 * ptr,int len)
	{
		// add from ptr[0] already done
		// wind up with next_ptr = ptr+len;
		// don't do prefetch here
		RR_ASSERT( len > 0 );
		// without match-start backup we have m_next_ptr == ptr+1 or ptr+2 (due to lazy match)
		// with backup, ptr may have gone back more
		// m_next_ptr tells us where our inserts should start
		RR_ASSERT( m_next_ptr >= ptr+1 );

		if ( m_next_ptr >= ptr+len )
		{
			// next must be within the match
			// -> this is not true in the multi-step literal prefetch mode :
			//	(it is true otherwise)
			//RR_ASSERT( m_next_ptr <= ptr+len ); 

			if ( m_next_ptr != ptr+len )
			{
				set_next(ptr+len);
			}
			return;
		}
		
		const U8 * zeroPosPtr = m_base_ptr;

		// do the insert at next :
		insert(m_next_row,(U32)(m_next_ptr-zeroPosPtr),m_next_hash);
		if ( c_do_second_hash )
			insert(m_next_row_second,(U32)(m_next_ptr-zeroPosPtr),m_next_hash);
		
		// start after next :
		int i = rrPtrDiff32(m_next_ptr - ptr) + 1;
		
		for(;i<len;i+=i) // i += i; power of 2 partial fill
		{
			const U8 * p = ptr+i;
			U32 h = hash(p);
			U32 index = h & m_hash_row_mask;
			t_hashtype * row = m_hash_table.data() + index;
			// don't add if I've already added to this row from inside this same match span :
			//if ( row[0] >= (ptr-zeroPosPtr) ) continue; // nah
			insert(row,(U32)(p-zeroPosPtr),h);
			// @@ NOTE: no second hash update; intentionally
			//  idea : randomly/round-robin do primary or second hash?			
		}
		
		set_next(ptr+len);
	}
		
	/**
	
	step_and_insert_partial ; fewer inserts for SuperFast encoder
	
	**/
	void step_and_insert_partial(const U8 * ptr,int len)
	{
		// add from ptr[0] already done
		// wind up with next_ptr = ptr+len;
		// don't do prefetch here
		RR_ASSERT( len > 0 );
		// without match-start backup we have m_next_ptr == ptr+1 or ptr+2 (due to lazy match)
		// with backup, ptr may have gone back more
		// m_next_ptr tells us where our inserts should start
		RR_ASSERT( m_next_ptr >= ptr+1 );

		if ( m_next_ptr >= ptr+len )
		{
			// next must be within the match
			// -> this is not true in the multi-step literal prefetch mode :
			//	(it is true otherwise)
			//RR_ASSERT( m_next_ptr <= ptr+len ); 

			if ( m_next_ptr != ptr+len )
			{
				set_next(ptr+len);
			}
			return;
		}
		
		const U8 * zeroPosPtr = m_base_ptr;

		// do the insert at next :
		insert(m_next_row,(U32)(m_next_ptr-zeroPosPtr),m_next_hash);
		if ( c_do_second_hash )
			insert(m_next_row_second,(U32)(m_next_ptr-zeroPosPtr),m_next_hash);
		
		// just one more :
		{
			// len - 2 is safe cuz len >= 2
			RR_ASSERT( len >= 2 );
			int i = len - 2;
			const U8 * p = ptr+i;
			// @@ optional : check p vs m_next_ptr ?
			if ( p > m_next_ptr )
			{
				U32 h = hash(p);
				U32 index = h & m_hash_row_mask;
				t_hashtype * row = m_hash_table.data() + index;
				// don't add if I've already added to this row from inside this same match span :
				//if ( row[0] >= (ptr-zeroPosPtr) ) continue; // nah
				insert(row,(U32)(p-zeroPosPtr),h);
				// @@ NOTE: no second hash update; intentionally
				//  idea : randomly/round-robin do primary or second hash?			
			}
		}
		
		set_next(ptr+len);
	}
	
	void insert(U32 * row,U32 pos,U32 check)
	{
		RR_COMPILER_ASSERT( c_table_depth >= 1 );

		U32 new_entry = (pos & CTMF_POS_MASK) | (check & CTMF_CHECK_MASK);

		// c_table_depth == 1 at the lowest compressor levels
		if (c_table_depth == 16) // 64 bytes = 4 128-bit vectors = 2 256-bit vectors
		{
			#if defined(__AVX2__) // this is set on files that compile with -mavx2 or /arch:AVX2 (i.e. _require_ AVX2)
			__m256i a0 = _mm256_load_si256 ((const __m256i *) (row + 0));
			__m256i a1 = _mm256_loadu_si256((const __m256i *) (row + 7));
			_mm256_storeu_si256((__m256i *) (row + 1), a0);
			_mm256_store_si256 ((__m256i *) (row + 8), a1);
			#elif defined(__RADSSE2__)
			__m128i a0 = _mm_load_si128 ((const __m128i *) (row +  0));
			__m128i a1 = _mm_loadu_si128((const __m128i *) (row +  3));
			__m128i a2 = _mm_loadu_si128((const __m128i *) (row +  7));
			__m128i a3 = _mm_loadu_si128((const __m128i *) (row + 11));
			_mm_storeu_si128((__m128i *) (row +  1), a0);
			_mm_store_si128 ((__m128i *) (row +  4), a1);
			_mm_store_si128 ((__m128i *) (row +  8), a2);
			_mm_store_si128 ((__m128i *) (row + 12), a3);
			#else
			memmove(row+1,row,(c_table_depth-1)*sizeof(U32));
			#endif

			row[0] = new_entry;
		}
		else if (c_table_depth == 8) // 32 bytes = 2 128-bit vectors
		{
			#ifdef __RADSSE2__
			__m128i a0 = _mm_load_si128 ((const __m128i *) (row +  0));
			__m128i a1 = _mm_loadu_si128((const __m128i *) (row +  3));
			// could alternatively slide the lanes of a0 and insert the bottom word
			// (see depth==4) case; in my testing, comes out basically identical
			_mm_storeu_si128((__m128i *) (row +  1), a0);
			_mm_store_si128 ((__m128i *) (row +  4), a1);
			row[0] = new_entry;
			#else
			// could probably be better
			memmove(row+1,row,(c_table_depth-1)*sizeof(U32));
			row[0] = new_entry;
			#endif
		}
		else if (c_table_depth == 4) // 16 bytes
		{
			// this is Kraken Normal at the moment (depth_shift=2, depth=4)
			// move 12 bytes up by 4

			#if defined(__RADSSE2__)
			__m128i a = _mm_load_si128((const __m128i *)row);
			// Shift up by 4 bytes, insert new entry at the bottom
			a = _mm_or_si128(_mm_slli_si128(a, 4), _mm_cvtsi32_si128(new_entry));
			_mm_store_si128((__m128i *)row, a);
			#elif defined(__RAD64REGS__)
			// faster to do all 64's rather than mixed 32-64
			U64 a = RR_GET64_NATIVE(row+0);
			U64 b = RR_GET64_NATIVE(row+1);
			RR_PUT64_NATIVE(row+1,a);
			RR_PUT64_NATIVE(row+2,b);
			row[0] = new_entry;
			#else
			U32 a,b,c;
			a = row[0]; b = row[1]; c = row[2];
			row[1] = a; row[2] = b; row[3] = c;
			row[0] = new_entry;
			#endif
		}
		else if (c_table_depth == 2)
		{
			row[1] = row[0];
			row[0] = new_entry;
		}
		else if (c_table_depth == 1)
		{
			// nothing to shift!
			row[0] = new_entry;
		}
		else
		{
			// depth is always a low power of 2
			RR_COMPILER_ASSERT( c_table_depth == 1 || c_table_depth == 2 || c_table_depth == 4 \
				|| c_table_depth == 8 || c_table_depth == 16 );
		}	
	}
	
	void insert(U16 * row,U32 pos,U32 check)
	{
		RR_COMPILER_ASSERT( c_table_depth >= 1 );
		if (c_table_depth > 1)
		{
			if (c_table_depth == 2) // 4 bytes
			{
				row[1] = row[0];
			}
			else if (c_table_depth == 4) // 8 bytes
			{
				// 96.83 c/b
				U32 a = RR_GET32_NATIVE(row);
				U32 b = RR_GET32_NATIVE(row+1);
				RR_PUT32_NATIVE(row+1,a);
				RR_PUT32_NATIVE(row+2,b);
			}
			else if (c_table_depth == 8) // 16 bytes
			{
				// 126.16 c/b
				U64 t0 = RR_GET64_NATIVE(row);
				U64 t1 = RR_GET64_NATIVE(row+3);
				RR_PUT64_NATIVE(row+1,t0);
				RR_PUT64_NATIVE(row+4,t1);
			}
			else if (c_table_depth == 16) // 32 bytes
			{
				// two vectors, or let compiler memmove it
		#ifdef __RADSSE2__
				__m128i a0 = _mm_load_si128 ((const __m128i *) (row +  0));
				__m128i a1 = _mm_loadu_si128((const __m128i *) (row +  7));
				_mm_storeu_si128((__m128i *) (row +  1), a0);
				_mm_store_si128 ((__m128i *) (row +  8), a1);
		#else
				memmove(row+1,row,(c_table_depth-1)*sizeof(U16));
		#endif
			}
			else
			{
				// depth is always a low power of 2
				RR_COMPILER_ASSERT( c_table_depth == 1 || c_table_depth == 2 || c_table_depth == 4 \
					|| c_table_depth == 8 || c_table_depth == 16 );
			}	
		}
	
		row[0] = (U16)(pos); // NO CHECK
		RR_UNUSED_VARIABLE(check);
	}

	// find match candidates that pass hash check
	// copies candidate offsets in linear order to candidates[]
	// returns a bit mask of which positions are valid candidates
	//
	// generic fallback version
	RADFORCEINLINE U32 find_candidates_generic(U32 candidates[c_table_depth * 2], const t_hashtype * row, const t_hashtype * row2, U32 cur_absolute_pos, U32 hash)
	{
		U32 cur_mask = 1;
		U32 candidate_mask = 0;

		for(int d=0;d<c_table_depth;d++)
		{
			U32 hash_entry = row[d];
			candidates[d] = (cur_absolute_pos - hash_entry) & CTMF_POS_MASK;
			bool matching = ((hash_entry ^ hash) & CTMF_CHECK_MASK) == 0;
			candidate_mask |= matching ? cur_mask : 0;
			cur_mask <<= 1;
		}

		if ( c_do_second_hash && row != row2 )
		{
			for(int d=0;d<c_table_depth;d++)
			{
				U32 hash_entry = row2[d];
				candidates[d + c_table_depth] = (cur_absolute_pos - hash_entry) & CTMF_POS_MASK;
				bool matching = ((hash_entry ^ hash) & CTMF_CHECK_MASK) == 0;
				candidate_mask |= matching ? cur_mask : 0;
				cur_mask <<= 1;
			}
		}

		return candidate_mask;
	}

	// find match candidates that pass hash check
	// copies candidate offsets in linear order to candidates[]
	// returns a bit mask of which positions are valid candidates
	//
	// "real" version that has fast paths
	RADFORCEINLINE U32 find_candidates(U32 candidates[c_table_depth * 2], const t_hashtype * row, const t_hashtype * row2, U32 cur_absolute_pos, U32 hash)
	{
		if ( sizeof(t_hashtype) == 4 )
		{
		#if defined(__RADSSE2__)
			if ( c_table_depth == 8 || ( c_table_depth == 4 && c_do_second_hash ) )
			{
				// 8 candidate locations to check, either from one row or two
				__m128i vcurpos = _mm_set1_epi32(cur_absolute_pos);
				__m128i vposmask = _mm_set1_epi32(CTMF_POS_MASK);
				__m128i vrow0 = _mm_loadu_si128((const __m128i *)&row[0]);
				__m128i vrow1 = _mm_loadu_si128((const __m128i *)(c_do_second_hash ? &row2[0] : &row[4]));
				_mm_storeu_si128((__m128i *)&candidates[0], _mm_and_si128(_mm_sub_epi32(vcurpos, vrow0), vposmask));
				_mm_storeu_si128((__m128i *)&candidates[4], _mm_and_si128(_mm_sub_epi32(vcurpos, vrow1), vposmask));

				__m128i vhashck = _mm_set1_epi32(hash & CTMF_CHECK_MASK);
				__m128i vcheck = _mm_set1_epi32(CTMF_CHECK_MASK);
				__m128i vhit0 = _mm_cmpeq_epi32(_mm_and_si128(vrow0, vcheck), vhashck);
				__m128i vhit1 = _mm_cmpeq_epi32(_mm_and_si128(vrow1, vcheck), vhashck);

				__m128i vhit01 = _mm_packs_epi32(vhit0, vhit1);
				__m128i vhitfinal = _mm_packs_epi16(vhit01, _mm_setzero_si128());
				U32 candidate_mask = _mm_movemask_epi8(vhitfinal);

				// if both rows were same, score matches only once
				if ( c_do_second_hash && row == row2 )
					candidate_mask &= 0xf;

				return candidate_mask;
			}
			else if ( c_table_depth == 4 )
			{
				__m128i vcurpos = _mm_set1_epi32(cur_absolute_pos);
				__m128i vposmask = _mm_set1_epi32(CTMF_POS_MASK);
				__m128i vrow = _mm_loadu_si128((const __m128i *)&row[0]);
				_mm_storeu_si128((__m128i *)&candidates[0], _mm_and_si128(_mm_sub_epi32(vcurpos, vrow), vposmask));

				__m128i vhashck = _mm_set1_epi32(hash & CTMF_CHECK_MASK);
				__m128i vcheck = _mm_set1_epi32(CTMF_CHECK_MASK);
				__m128i vhit = _mm_cmpeq_epi32(_mm_and_si128(vrow, vcheck), vhashck);
				U32 candidate_mask = _mm_movemask_ps(_mm_castsi128_ps(vhit));

				return candidate_mask;
			}
		#elif defined(DO_BUILD_NEON64)
			if ( c_table_depth == 8 || ( c_table_depth == 4 && c_do_second_hash ) )
			{
				// 8 candidate locations to check, either from one row or two
				static RAD_ALIGN(const U16, maskbits[8], 16) = { 1,2,4,8, 16,32,64,128 };

				uint32x4_t vcurpos = vdupq_n_u32(cur_absolute_pos);
				uint32x4_t vposmask = vdupq_n_u32(CTMF_POS_MASK);
				uint32x4_t vrow0 = vld1q_u32(row);
				// NOTE: extra set of parens required for VC++ impl of NEON intrinsics
				uint32x4_t vrow1 = vld1q_u32((c_do_second_hash ? row2 : &row[4]));
				vst1q_u32(&candidates[0], vandq_u32(vsubq_u32(vcurpos, vrow0), vposmask));
				vst1q_u32(&candidates[4], vandq_u32(vsubq_u32(vcurpos, vrow1), vposmask));

				uint32x4_t vhashck = vdupq_n_u32(hash & CTMF_CHECK_MASK);
				uint32x4_t vcheck = vdupq_n_u32(CTMF_CHECK_MASK);
				uint32x4_t vhit0 = vceqq_u32(vandq_u32(vrow0, vcheck), vhashck);
				uint32x4_t vhit1 = vceqq_u32(vandq_u32(vrow1, vcheck), vhashck);
				uint16x8_t vhit01 = vuzp1q_u16(vreinterpretq_u16_u32(vhit0), vreinterpretq_u16_u32(vhit1));
				U32 candidate_mask = vaddvq_u16(vandq_u16(vhit01, vld1q_u16(maskbits)));

				// if both rows were same, score matches only once
				if ( c_do_second_hash && row == row2 )
					candidate_mask &= 0xf;

				return candidate_mask;
			}
			else if ( c_table_depth == 4 )
			{
				static RAD_ALIGN(const U32, maskbits[4], 16) = { 1,2,4,8 };

				uint32x4_t vcurpos = vdupq_n_u32(cur_absolute_pos);
				uint32x4_t vposmask = vdupq_n_u32(CTMF_POS_MASK);
				uint32x4_t vrow = vld1q_u32(row);
				vst1q_u32(&candidates[0], vandq_u32(vsubq_u32(vcurpos, vrow), vposmask));

				uint32x4_t vhashck = vdupq_n_u32(hash & CTMF_CHECK_MASK);
				uint32x4_t vcheck = vdupq_n_u32(CTMF_CHECK_MASK);
				uint32x4_t vhit = vceqq_u32(vandq_u32(vrow, vcheck), vhashck);
				U32 candidate_mask = vaddvq_u32(vandq_u32(vhit, vld1q_u32(maskbits)));

				return candidate_mask;
			}
		#endif
		}

		return find_candidates_generic(candidates, row, row2, cur_absolute_pos, hash);
	}
	
	//=========================================
	// note : weird thing about my hash here
	//	top bits are used for hash check
	//  bottom bits are *DISCARDED*
	//	 (the ones in depth_bits)
	//  hash table is indexed using table_size_bits, excluding the bottom depth bits
	//	eg. if table size is 16 bits and depth is 4 bits
	//	hash_mask is 0xFFF0
	
	RADFORCEINLINE UINTa second_hash_index(U32 h1,const U8 * ptr)
	{
		// mul and shift down :
		UINTa h = hash_n_bytes<t_second_hash_len>(ptr,m_table_size_bits);
		// turn off depth bits :
		h &= ~ (UINTa)(c_table_depth-1);
		return h;
	}
	
	// hash will be used to make the row index with & m_hash_row_mask
	//	it will also be used to grab the hash-check value
	RADFORCEINLINE U32 hash(const U8 * ptr)
	{
		U64 h = hash_bytes_64(ptr, m_hash_mul);
		// get the best bits (high bits) to the bottom :
		// the lower part of returned hash is used for the table index
		// the upper part of returned hash is used for the hash check
		// (depth bits are turned off outside when we are masked)
		// NOTE: in the hash_mul formulation, the bottom bits of h are all 0 for low hash_lens
		// so really really don't use those
		//h = (h>>32) + RR_ROTL64(h,m_table_size_bits);
		//return (U32)h;
		// I think this is marginally better in practice & perhaps better justified
		// the best bits in the hash go in the bottom (index)
		// the second best bits go the top of the returned 32-bit hash
		return RR_ROTL32( (U32)(h>>32),m_table_size_bits);
		//return RR_ROTL32( (U32)(h>>32),m_table_size_bits) + (U32)(h>>32);
	}

};

//=======================================================================

#if OODLE_PLATFORM_HAS_ADVANCED_MATCHERS

struct LRMSet;
class IncrementalMatchFinder;

IncrementalMatchFinder * CTMF_CreateMatchFinder(const U8 * ubuf,SINTa size,
		SINTa startRecordingPos RADDEFAULT(0),
		LRMSet * lrms RADDEFAULT(NULL),
		OodleLZ_Jobify jobify RADDEFAULT(OodleLZ_Jobify_Default),
		void * jobifyUserPtr RADDEFAULT(NULL),
		rrArenaAllocator *scratch_arena RADDEFAULT(NULL));

#endif // OODLE_PLATFORM_HAS_ADVANCED_MATCHERS

//=======================================================================

// it's a micro-bit faster to use a U8 check , but then you only get 24 bit offsets? maybe ok
#define CTMF2_HASH_CHECK_BITS	6
//#define CTMF2_HASH_CHECK_BITS	8
#define CTMF2_HASH_CHECK_MASK	(((U32)1<<CTMF2_HASH_CHECK_BITS)-1)
#define CTMF2_HASH_POS_BITS		(32-CTMF2_HASH_CHECK_BITS)

// t_mml2 == 0 means don't do hash2
// t_chain_steps_max == 0 means no chain, just hash1 direct
template <typename t_hash1_type,int t_mml1,int t_mml2,int t_chain_steps_max>
struct CTMF2
{
	// no need for t_chain_steps_max to be a template parameter, could be a variable
	//	it's just how I drill through the level options at the moment
	enum { c_chain_steps_max = t_chain_steps_max };
	enum { c_do_hash2 = t_mml2 > 0 };
	enum { c_do_chain = t_chain_steps_max > 1 };
	enum { c_is_ctmf2 = 1 };
	enum { c_max_preload_pos_bits = CTMF2_HASH_POS_BITS };
	
	rrArenaAllocator* m_arena = nullptr;
	rrArenaArray<t_hash1_type> m_hash1;
	rrArenaArray<U32> m_hash2;
	rrArenaArray<U16> m_chain;
	
	const U8 * m_base_ptr = nullptr;
	const U8 * m_next_ptr = nullptr;
	
	U32 m_hash1_mask = 0;
	U32 m_hash2_mask = 0;
	U32 m_chain_mask = 0;
	int m_hash1_bits = 0;
	int m_hash2_bits = 0;
	
	CTMF2()
	{
	}
	
	~CTMF2()
	{
		release();
	}
	
	static SINTa get_mem_size(int hash1_bits, int hash2_bits, int chain_bits)
	{
		SINTa ret = sizeof(t_hash1_type)<<hash1_bits;
		
		if ( c_do_hash2 )
			ret += sizeof(U32)<<hash2_bits;
			
		if ( c_do_chain )
			ret += sizeof(U16)<<chain_bits;

		return ret;	
	}
	
	void allocate(int hash1_bits, int hash2_bits, int chain_bits,rrArenaAllocator * arena,int hash_len_override)
	{
		RR_ASSERT( chain_bits <= 16 ); // fit in U16
		
		RR_ASSERT( hash_len_override == 4 && t_mml1 == 4 ); // not supported in CTMF2 
		RR_UNUSED_VARIABLE(hash_len_override);
		
		// @@ hacky wrong place to do this
		//  limit to 19 bits = 512k entries = 2M bytes
		//	trying to (almost) stay in cache
		hash1_bits = RR_MIN(hash1_bits,19);
		hash2_bits = RR_MIN(hash2_bits,19);
	
		m_hash1_bits = hash1_bits;
		m_hash2_bits = hash2_bits;
	
		m_hash1_mask = (1<<hash1_bits)-1;
		m_hash2_mask = (1<<hash2_bits)-1;
		m_chain_mask = (1<<chain_bits)-1;

		// align to cache line so hash ways are in one line :
		m_hash1.Allocate(arena, m_hash1_mask + 1, RR_CACHE_LINE_SIZE);
		m_arena = arena;

		if (c_do_hash2)
		{
			m_hash2.Allocate(arena, m_hash2_mask + 1, RR_CACHE_LINE_SIZE);
		}

		if (c_do_chain)
		{
			m_chain.Allocate(arena, m_chain_mask + 1, RR_CACHE_LINE_SIZE);
		}

		reset();
	}

	void release()
	{
		m_hash1.Release(m_arena, m_hash1_mask + 1, RR_CACHE_LINE_SIZE);
		m_hash2.Release(m_arena, m_hash2_mask + 1, RR_CACHE_LINE_SIZE);
		m_chain.Release(m_arena, m_chain_mask + 1, RR_CACHE_LINE_SIZE);
	}
	
	void reset()
	{
		m_hash1.Zero(m_hash1_mask + 1);
		if ( m_hash2.data() )
			m_hash2.Zero(m_hash2_mask + 1);
		if ( m_chain.data() )
			m_chain.Zero(m_chain_mask + 1);
	}
	
	void set_base(const void *base)
	{
		m_base_ptr = U8_void(base);
	}
	
	void set_base_and_preload(const void *base, const void * preload_upto, SINTa maxPreloadLen)
	{
		m_base_ptr = U8_void(base);
		
		if ( preload_upto != base )
		{
			THREADPROFILESCOPE("ctmf_preload");
		
			const U8 * ptr = U8_void(base);
			const U8 * ptrEnd = U8_void(preload_upto);
			SINTa preload_len = rrPtrDiff(ptrEnd - ptr);
			RR_ASSERT_ALWAYS( preload_len > 0 );
			if ( preload_len > maxPreloadLen )
			{
				preload_len = maxPreloadLen;
				ptr = ptrEnd - maxPreloadLen;
			}
			SINTa step = preload_len >> 18;
			step = RR_MAX(step,2);
			SINTa numThisStep = (preload_len/2)/step;
			
			for(;;)
			{
				if ( --numThisStep <= 0 )
				{
					if ( ptr >= ptrEnd )
						break;
					step >>= 1;
					RR_ASSERT_ALWAYS( step >= 1 ); 
					numThisStep = (rrPtrDiff(ptrEnd - ptr))/step;
					if ( step > 1 )
						numThisStep /= 2;

					// last 256k is done at step = 1 with do_chain = true
				}
				
				U32 pos = (U32) rrPtrDiff( ptr - m_base_ptr );
				UINTa h1 = hash1(ptr);
				
				#if 0 
				// -> seems to not help ? just skip it
				// update chain only last 64k
				if ( c_do_chain && (step==1) && rrPtrDiff(ptrEnd - ptr) < 65536 )
				{
					UINTa ci = pos & m_chain_mask;

					// inserting "pos" in hash1
					// link it into chain so hash1[h1] -> previous head
					
					// linking in the head :
					m_chain[ ci ] = (U16) m_hash1[ h1 ];
				}
				#endif
		
				m_hash1[ h1 ] = (t_hash1_type) pos;
				
				if ( c_do_hash2 )
				{
					insert2(ptr);
				}
				
				ptr += step;
			}
			RR_ASSERT( ptr == ptrEnd );
		}
	}
		
	void set_next(const U8 * ptr)
	{
		m_next_ptr = ptr;
	}
	
	// pass in h1 , I generally already have it
	void insert1(const U8 * ptr, UINTa h1)
	{
		RR_ASSERT( ptr >= m_next_ptr );
	
		U32 pos = (U32)(ptr - m_base_ptr);
		RR_ASSERT( h1 == hash1(ptr) );
		
		if ( c_do_chain )
		{
			UINTa ci = pos & m_chain_mask;
			
			// inserting "pos" in hash1
			// link it into chain so hash1[h1] -> previous head
			
			// linking in the head :
			m_chain[ ci ] = (U16) m_hash1[ h1 ];
		}
		m_hash1[ h1 ] = (t_hash1_type) pos;
		
		m_next_ptr = ptr+1;
	}
		
	void step_and_insert(const U8 * ptr,int len)
	{
		// @@ partial inserts?
		// @@ could do every-byte updates for hash1
		//		and partial updates for hash2
		
		const U8 * end = ptr+len;
		
//10,000,000 -> 3,561,498 =  2.849 bpb =  2.808 to 1 
//24,700,820 ->11,307,869 =  3.662 bpb =  2.184 to 1 
//24,700,820 ->11,303,139 =  3.661 bpb =  2.185 to 1 

		if ( c_do_hash2 )
		{
			// partial insert hash2
			/*
			insert2(ptr);
			insert2(ptr+len/2);
			/**/
			//*
			for(int i=0;i<len;i+=i+1)
			{
				insert2(ptr+i);
			}
			/**/
		}
		
		// we may have updated a few bytes into match already
		//	(eg. due to match backup or lazy parse lookahead)
		//ptr = RR_MAX(ptr,m_next_ptr);
		RR_ASSERT( m_next_ptr >= ptr ); // ??
		RR_ASSERT( m_next_ptr <= end ); // ??
		ptr = m_next_ptr;
		
		//insert2(ptr);
		//SUM:total   : 24,700,817 ->11,425,904 =  3.701 bpb =  2.162 to 1 
			
		while(ptr < end)
		{
			insert1(ptr,hash1(ptr));
			//insert2(ptr);
			++ptr;
		}
		
		//SUM:total   : 24,700,817 ->11,410,753 =  3.696 bpb =  2.165 to 1 

		RR_ASSERT( m_next_ptr == end );
	}
	
	void step_and_insert_partial(const U8 * ptr,int len)
	{
		step_and_insert(ptr,len);
	}
	
	UINTa hash1(const U8 * ptr)
	{
		return hash_n_bytes<t_mml1>(ptr,m_hash1_bits);
	}

	void insert2(const U8 * ptr)
	{
		if ( c_do_hash2 )
		{
			U32 check;
			U32 h2 = hash2(ptr,check);
			
			U32 pos = (U32)(ptr - m_base_ptr);
		
			// U8 way :
			//m_hash2[ h2 ] = (pos << CTMF2_HASH_CHECK_BITS) | check; 
			m_hash2[ h2 ] = (pos << CTMF2_HASH_CHECK_BITS) | (check & CTMF2_HASH_CHECK_MASK);
		}
	}
	
	U32 hash2(const U8 * ptr, U32 & check)
	{
		RR_ASSERT( c_do_hash2 );
		U32 hash2 = (U32) hash_n_bytes<t_mml2>(ptr,32);
		U32 index = hash2 >> (32 - m_hash2_bits); // take the high bits for index
		check = hash2; // will use the low bits , delay the & CTMF2_HASH_CHECK_MASK
		//check = (U8) hash2; // grab the low bits
		return index;
	}
};

//=======================================================================

// "Fast" CTMF is for HyperFast encoders
// - no prefetch (assumed to be all in L2 or L1)
// - no collision bits
// - always depth 1

// the idea is to use the inline functions with all the pointers etc. cached in local vars

namespace
{

static RADFORCEINLINE UINTa fast_ctmf_hash(U64 ptr64le, U64 mul64, int hash_shift)
{
	return (UINTa) ((ptr64le * mul64) >> hash_shift);
}

// Inserts new pos and returns previous value in that slot
template <typename t_hashtype>
static RADFORCEINLINE t_hashtype fast_ctmf_insert(t_hashtype * hash_table, U64 ptr64le, U64 hash_mul, int hash_shift, SINTa pos)
{
	UINTa hash = fast_ctmf_hash(ptr64le,hash_mul,hash_shift);

	t_hashtype hashpos = hash_table[hash];
	hash_table[hash] = static_cast<t_hashtype>(pos);
	return hashpos;
}

template <typename t_hashtype>
static RADFORCEINLINE t_hashtype fast_ctmf_insert(t_hashtype * hash_table, const U8 * ptr, U64 hash_mul, int hash_shift, SINTa pos)
{
	UINTa hash = fast_ctmf_hash(RR_GET64_LE_UNALIGNED(ptr),hash_mul,hash_shift);
		
	t_hashtype hashpos = hash_table[hash];
	hash_table[hash] = static_cast<t_hashtype>(pos);
	return hashpos;
}

};

template <typename t_hashtype>
struct FastCTMF
{
	enum { c_is_ctmf2 = 0 };
	enum { c_max_preload_pos_bits = 24 }; // HyperFasts actually have 32 pos bits, but no need to do huge preloads for htem

	typedef t_hashtype hash_type;

	rrArenaAllocator* m_hash_table_arena = nullptr;

	rrArenaArray<t_hashtype> m_hash_table;
	U64 m_hash_mul = 0;
	int m_table_size_bits = 0;
    int m_hash_shift = 0;
	const U8 * m_base_ptr = nullptr;

	FastCTMF()
	{
	}

	~FastCTMF()
	{
		release();
	}

	static SINTa get_mem_size(int table_size_bits, int hash2_bits, int chain_bits)
	{
		SINTa alloc_size = sizeof(t_hashtype)*(((SINTa)1<<table_size_bits));
		return alloc_size;
	}
	
	void allocate(int table_size_bits,int /*hash2_bits*/,int /*chain_bits*/,rrArenaAllocator * arena,int hash_len)
	{
		if (!hash_len)
			hash_len = 4;

		m_table_size_bits = table_size_bits;
		m_hash_shift = 64 - table_size_bits;

		switch (hash_len)
		{
		default:
		case 4: m_hash_mul = 2654435761ull << 32; break;
		case 5: case 6: case 7: case 8:
			m_hash_mul = HASH_U64_MULTIPLIER << ((8 - hash_len) * 8);
			break;
		}

		//rrprintf("CTMF table bits : %d ; alloc size : %d kb\n",table_size_bits,(int)((sizeof(t_hashtype)*(((SINTa)1<<m_table_size_bits)))>>10));

		m_hash_table_arena = arena;
		m_hash_table.Allocate(arena, (SINTa)1 << m_table_size_bits, RR_CACHE_LINE_SIZE);

		reset();
	}

	void release()
	{
		m_hash_table.Release(m_hash_table_arena, (SINTa)1 << m_table_size_bits, RR_CACHE_LINE_SIZE);
	}

	void reset()
	{
		m_hash_table.Zero((SINTa)1 << m_table_size_bits);
	}

	void set_base(const void *base)
	{
		m_base_ptr = U8_void(base);
	}

	void set_base_and_preload(const void *base, const void * preload_upto, SINTa maxPreloadLen)
	{
		m_base_ptr = U8_void(base);

		if ( preload_upto > base )
		{
			THREADPROFILESCOPE("fastctmf_preload");

			const U8 * ptr = U8_void(base);
			const U8 * ptrEnd = U8_void(preload_upto);
			SINTa preload_len = rrPtrDiff(ptrEnd - ptr);
			if ( preload_len > maxPreloadLen )
			{
				preload_len = maxPreloadLen;
				ptr = ptrEnd - maxPreloadLen;
			}
			//SINTa step = preload_len >> 17;
			//step = RR_CLAMP(step,2,64);
			SINTa step = preload_len >> 18;
			step = RR_MAX(step,2);
			SINTa numThisStep = (preload_len/2)/step;

			t_hashtype * hash_table = m_hash_table.data();
			const U8 * hash_base = m_base_ptr;
			U64 hash_mul = m_hash_mul;
			int hash_shift = m_hash_shift;

			for(;;)
			{
				if ( --numThisStep <= 0 )
				{
					if ( ptr >= ptrEnd )
						break;
					step >>= 1;
					RR_ASSERT_ALWAYS( step >= 1 );
					numThisStep = (rrPtrDiff(ptrEnd - ptr))/step;
					if ( step > 1 )
						numThisStep /= 2;
				}

				SINTa pos = rrPtrDiff( ptr - hash_base );
				fast_ctmf_insert(hash_table, ptr, hash_mul, hash_shift, pos);

				ptr += step;
			}
			RR_ASSERT( ptr == ptrEnd );
		}
	}
};

//=======================================================================

OODLE_NS_END
