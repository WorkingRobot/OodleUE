// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// legacy heap implementation matching the observable behavior of the EASTL
// heap we used to have for Oodle Network v5. This is due to an oversight
// where the key comparison used does not include tie-breakers and the code
// assignment depends on the order that ties are delivered in; this makes it
// so that implementation details of the underlying binary heap leak into
// the data format.
//
// Does not actually contain EASTL code at this point; it's an adapted version
// of our newer heap code to restore the old behavior, needed for v5
// compatibility of normalize_counts.

namespace netv5heap
{

// Usual heap rules: implicit binary tree,
// node i has (up to) two children at 2i+1 and 2i+2,
// parents are >= (i.e. not <) than their children

template<typename T>
void promote_heap(T * first, SINTa top, SINTa i)
{
	// promote_heap: first[i] is a new or changed leaf, the heap
	// is valid elsewhere. Walk up the tree, swapping node i with
	// its parent until heap invariant restored
	//
	// Essentially the same logic as the rralgorithm promote_heap,
	// except this one is also used for partial fix-ups from
	// adjust_heap which means it has a variable "top" instead
	// of assuming top == 0.
	while ( i > top )
	{
		SINTa parent = (i - 1) >> 1;

		// If we're not larger than our parent, heap invariant is restored
		// and we're done
		if ( ! ( first[parent] < first[i] ) )
			return;

		swap(first[parent], first[i]);
		i = parent;
	}
}

template <typename T>
void adjust_heap(T * first, SINTa top, SINTa n)
{
	// We either removed the old top-of-the-heap and replaced
	// it with something else, or otherwise have the two child
	// sub-heaps of top obeying the heap invariant and need to
	// make the node at "top" obey it too.
	//
	// The approach taken here is to first push node i all the way
	// down to a leaf (filling in the gaps left in the heap from
	// effectively removing the node that was there previously), and
	// then from the leaf position sift it back up until the heap
	// invariant is restored. This is different from our regular
	// heap implementation which checks during the sift-down process
	// and stops as soon as the heap invariant is restored, which
	// does not need a separate sift-up pass. [This is the primary
	// difference between this older heap logic and the newer one.]
	//
	// We do half-swaps on the way down here since we keep swapping
	// with the value that is initially at first[i].
	SINTa i = top;
	const T value = first[i];

	for (;;)
	{
		// Index of right child (if that's in range, the left child is too)
		SINTa child = i*2 + 2;
		if ( child >= n )
		{
			// If there is no right child but there is a left child, do the final swap
			if ( child == n )
			{
				first[i] = first[child - 1];
				i = child - 1;
			}

			break;
		}

		// Choose whichever is larger
		if ( first[child] < first[child - 1] )
			--child;

		// Do the half-swap and keep descending
		first[i] = first[child];
		i = child;
	}

	first[i] = value;
	promote_heap<T>(first, top, i);
}

template <typename T>
inline void push_heap(T * first, T * last)
{
	promote_heap<T>(first, 0, (last - first) - 1);
}

template <typename T>
inline void pop_heap(T * first, T * last)
{
	// Swap removed element just past the new end, then fix heap
	swap(first[0], last[-1]);
	adjust_heap<T>(first, 0, (last - first) - 1);
}

template <typename T>
void make_heap(T * first, T * last)
{
	// Classic Sedgewick bottom up heap construction
	// start from the tail, make valid heaps at the leaves
	// keep adding parent nodes and fixing them
	SINTa n = last - first;

	// The last (n/2) are single-node leaves, they are heaps by default;
	// cur starts where the first parent with two kids starts.
	SINTa cur = n >> 1;
	while ( cur > 0 )
	{
		--cur;
		adjust_heap<T>(first, cur, n);
	}
}

} // netv5heap

