// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_ALGORITHMH__
#define __RADRR_ALGORITHMH__



#include "rrstl.h"

/**

rralgorithm is all the rr STLy type stuff that's too heavy to be in rrstl.h

**/

RR_NAMESPACE_START

// swap3
// is: ind3=ind2; ind2=ind1; ind1=original ind3;
//  {1,2,3} -> {3,1,2} , abc -> cab
template<typename Type> OOINLINE void swap3(Type &a,Type &b,Type &c)
{
	//Type _c = c; c = b; b = a; a = _c;
	
	swap(a,b);
	swap(a,c);
}

template<typename Type>
class stdless
{
public:
    bool operator() (const Type & a,const Type & b) const
    {
        return a < b;
    }
};

template<typename Type>
class stdgreater
{
public:
    bool operator() (const Type & a,const Type & b) const
    {
        return a > b;
    }
};

template<typename Type>
class stdgreaterequal
{
public:
    bool operator() (const Type & a,const Type & b) const
    {
        return a >= b;
    }
};

template<typename Type>
class stdlessequal
{
public:
    bool operator() (const Type & a,const Type & b) const
    {
        return a <= b;
    }
};

template<typename Type>
class stdequal
{
public:
    bool operator() (const Type & a,const Type & b) const
    {
        return a == b;
    }
};

template<typename _RanIt, typename _Predicate> 
OOINLINE void stdsort(_RanIt _First, _RanIt _Last, _Predicate less)
{
    int count = rrPtrDiff32( _Last - _First );

       #define SORTINDEX _RanIt
       #define SORTFIRSTINDEX _First
       #define SORTLENGTH count
     
       #define PTRADD( ptr, val ) ( ( (char*) ptr ) + val )

       #define SORTSWAP( ind1, ind2 ) swap(*ind1,*ind2)
       #define SORTSWAP3( ind1, ind2, ind3 ) swap3(*ind1,*ind2,*ind3)

       #define SORTKEYISBEFORE( key_ind, ind ) ( less(*key_ind,*ind) )
       #define SORTKEYISAFTER( key_ind, ind ) ( less(*ind,*key_ind) )


    #include "rrsort.inl"
}

// stdsort with default comparison "less"
//	produces a sort with lowest first

template<typename Type>
OOINLINE void stdsort(Type * _First, Type * _Last)
{
    stdsort(_First,_Last, stdless<Type>() );
}

//=====================================================================

// find_extremum : 
//	  return iterator where predicate() is true vs all others
template<typename t_iterator, typename t_predicate>
OOINLINE t_iterator find_extremum(t_iterator begin,t_iterator end,t_predicate predicate)
{
	RR_ASSERT( begin != end );
	t_iterator ext = begin;
	t_iterator it = begin+1;
	while( it < end )
	{
		if ( predicate( *it, *ext ) )
		{
			ext = it;
		}
		++it;
	}
	return ext;
}
				
template<typename t_type>
OOINLINE t_type * find_min(t_type * begin,t_type * end)
{
	return find_extremum(begin,end,stdless<t_type>());
}

template<typename t_type>
OOINLINE t_type * find_max(t_type * begin,t_type * end)
{
	return find_extremum(begin,end,stdgreater<t_type>());
}

//===============================================================

// lower_bound for binary search on sorted array [first,last]
//	if comp is "less" , finds the first entry such that *pentry >= value
//  the array [first,last] should be sorted by the comparison "comp"
template<typename Type, typename Type2, typename Compare>
Type * lower_bound(Type * first, Type * last, const Type2 & value, Compare comp)
{
    SINTa count = rrPtrDiff(last - first);
 
    while (count > 0) 
    {
        SINTa step = count / 2;
		Type * it = first + step;
        if (comp(*it, value))
        {
            first = ++it;
            count -= step + 1;
        }
        else
        {
			count = step;
		}
    }
    return first;
}

//=====================================================================

// unique() predicate is an equality compare
template<typename t_iterator, typename t_predicate>
OOINLINE t_iterator unique(t_iterator begin,t_iterator end,t_predicate predicate)
{
	// skip the beginning where all adjacent pairs differ :
	t_iterator it = begin;
	while( it < (end-1) && ! predicate( it[0], it[1] ) )
		++it;
	
	if ( it >= end-1 )
		return end;
		
	// it[0] and it[1] match
	t_iterator out = it;
	++it;
	RR_ASSERT( predicate(*out,*it) );
	++it;
	while( it < end )
	{
		if ( predicate(*out,*it) )
		{
			++it;
		}
		else
		{
			++out;
			*out = *it;
			//swap(*out,*it);
			++it;
		}
	}
	return out+1;
}


template<typename t_type>
OOINLINE t_type * unique(t_type * begin,t_type * end)
{
	return unique(begin,end,stdequal<t_type>());
}

// variant of unique using swap instead of assignment
template<typename t_iterator, typename t_predicate>
OOINLINE t_iterator unique_swap(t_iterator begin,t_iterator end,t_predicate predicate)
{
	// skip the beginning where all adjacent pairs differ :
	t_iterator it = begin;
	while( it < (end-1) && ! predicate( it[0], it[1] ) )
		++it;
	
	if ( it >= end-1 )
		return end;
		
	// it[0] and it[1] match
	t_iterator out = it;
	++it;
	RR_ASSERT( predicate(*out,*it) );
	++it;
	while( it < end )
	{
		if ( predicate(*out,*it) )
		{
			++it;
		}
		else
		{
			++out;
			//*out = *it;
			swap(*out,*it);
			++it;
		}
	}
	return out+1;
}

//=====================================================================
//
// heap :

	// debug helper to check this range is a valid heap
	//	heap is an implicit binary tree
	// each node i has children at i*2+1 & i*2+2
	// each parent node should be >= than either child (if "compare" is the standard less)
	//		(that is, compare(parent,child) should not be true)
	// the two children are not ordered against eachother
    template <typename T, typename Compare>
    bool is_heap(T * first, T * last, Compare compare)
    {
		SINTa n = last - first ;
		T * cur = first;
		while( cur < last )
		{
			SINTa i = cur-first;
			
			// current node i
			// check it compares right against its parent & its children
			// this is redundant but whatevs
			
			if ( i > 0 )
			{
				SINTa parent = (i - 1)>>1;
				
				if ( compare(first[parent],first[i]) )
					return false;
			}
					
			SINTa child = i*2 + 1;
			if ( child < n )
			{
				if ( compare(first[i],first[child]) )
					return false;
				
				if ( child+1 < n && compare(first[i],first[child+1]) )
					return false;
			}
			
			cur++;
		}
		
		return true;
    }
    
	// std::heap default compare is <
	//	with < compare, heap will return the *largest* first
    
    template <typename T, typename Compare>
    void promote_heap(T * first, T * last, T * cur, Compare compare)
    {	
		RR_ASSERT( last > first );
		RR_ASSERT( cur >= first && cur < last );
		
		// promote_heap :
		// cur is a new or changed leaf
		// the heap is valid except at *cur
		// if cur is larger than parent, swap to parent and repeat
		
		//SINTa n = last-first;
		SINTa i = cur-first;
		
		while( i > 0 )
		{
			SINTa parent = (i - 1)>>1;
			
			// if ( parent >= first ) -> done
			if ( ! compare(first[parent],first[i]) )
			{
				// that's the right order, we're done
				return;
			}
			
			swap(first[parent],first[i]);
			
			i = parent;
		}
    }
    
    template <typename T, typename Compare>
    void popped_heap(T * first, T * last, Compare compare)
    {	
		RR_ASSERT( last > first );
		
		// remove_top_from_heap : remove the root node and fix the heap
		
		SINTa n = last-first;
		SINTa i = 0;
		
		for(;;)
		{
			SINTa child = i*2 + 1;
			
			if ( child >= n )
				break;
			
			// I want the larger child , if ( *(child+1) > *(child) )
			if ( child+1 < n && compare( first[child] , first[child+1] ) )
			{
				child++;
			}
			
			first[i] = first[child]; // could be a swap, better if T has sub-objects
			i = child;
		}
		
		// heap is now one element smaller :
		n--;
		
		if ( i < n )
		{
			// we deleted the leaf at i
			// can't have a hole in implicit tree structure
			// fill element [i] with last
			first[i] = first[n]; // could be a swap, better if T has sub-objects
			
			// fix the order of the new leaf at i vs its new parent
			promote_heap(first,last,first+i,compare);
		}
    }
    
    template <typename T, typename Compare>
    void pop_heap(T * first, T * last, Compare compare)
    {	
		RR_ASSERT( last > first );
		
		// as per standard STL, pop_heap pops the top of the heap
		//	and puts the popped item at the end of the array
		// -> this is a bit silly, caller should just grab *begin and do remove_top_from_heap
		
		T temp = *first;
		
		popped_heap(first,last,compare);
		
		last[-1] = temp; // a bit dumb
    }
    
    template <typename T, typename Compare>
    void push_heap(T * first, T * last, Compare compare)
    {	
		RR_ASSERT( last > first );
		// push_heap is called right after push_back
		// so [first,last-1] is a heap
		//	*(last-1) was pushed on but is not in heap order
		
		promote_heap(first,last,last-1,compare);
    }
    
    /*
    
    // valid but slow ways to make a heap :
    
    template <typename T, typename Compare>
    void make_heap(T * first, T * last, Compare compare)
    {
		stdsort(first,last,compare);
		reverse(first,last);
		
		//RR_ASSERT( is_heap(first,last,compare) );
	}

    template <typename T, typename Compare>
    void make_heap(T * first, T * last, Compare compare)
    {
		T * cur = first+2;
		while( cur <= last )
		{			
			promote_heap(first,cur,cur-1,compare);
			cur++;
		}
		
		//RR_ASSERT( is_heap(first,last,compare) );
    }
    */
	
    template <typename T, typename Compare>
    void adjust_heap(T * first, SINTa i, SINTa n, Compare compare)
    {
		// adjust_heap
		// first[i] is the root of a new heap we are fixing
		//	the children are valid heaps
		//	but first[i] may be in the wrong place
		// while it is smaller than its largest child, bubble it down with swaps
    
		for(;;)
		{
			SINTa child = i*2 + 1;
			
			if ( child >= n )
				break;
			
			// swap *i to the larger child
			
			// I want the larger child , if ( *(child+1) > *(child) )
			if ( child+1 < n && compare( first[child] , first[child+1] ) )
			{
				child++;
			}
			
			if ( compare( first[child] , first[i] ) )
			{
				// right order, done
				break;
			}
			
			swap(first[i],first[child]);
			i = child;
		}	
	}
	
	// if you changed a value in the heap and wanted to fix its location
	// you would adjust_heap to bubble the item down to its children
	//	or promote_heap to bubble it up to its parents
	
    template <typename T, typename Compare>
    void make_heap(T * first, T * last, Compare compare)
    {
		// classic Sedgewick bottom up heap construction
		// start from the tail, make valid heaps at the leaves
		// keep adding parent nodes and fixing them
    
		SINTa n = last - first;
		
		// the last (n/2) are single-node leaves, they are heaps by default
		// cur starts where the first parent with two kids start

		SINTa cur = n >> 1;		
		
		while ( cur > 0 )
		{
			--cur;
			
			//SINTa child = cur*2 + 1;
			//RR_ASSERT( child < n );
			
			// the children of cur are two heaps
			// make cur the root of a new heap
			// *cur may be out of order
			//  swap it with the larger child and repeat
			
			adjust_heap(first,cur,n,compare);			
		}
		
		//RR_ASSERT( is_heap(first,last,compare) );
    }
    
    // when Compare is not specified, the comparison op is <
    //	this makes a heap which pops the *biggest* first
    // this is the opposite of the default order with std sort
    
    template <typename T>
    bool is_heap(T * first, T * last)
    {
		return is_heap(first,last,stdless<T>());
    }	
    
    template <typename T>
    void pop_heap(T * first, T * last)
    {
		pop_heap(first,last,stdless<T>());
    }	
    
    template <typename T>
    void popped_heap(T * first, T * last)
    {	
		popped_heap(first,last,stdless<T>());
    }
    
    template <typename T>
    void push_heap(T * first, T * last)
    {
		push_heap(first,last,stdless<T>());
    }	
    
    template <typename T>
    void make_heap(T * first, T * last)
    {
		make_heap(first,last,stdless<T>());
    }

//===============================================================================

RR_NAMESPACE_END

//=====================================================

#endif
