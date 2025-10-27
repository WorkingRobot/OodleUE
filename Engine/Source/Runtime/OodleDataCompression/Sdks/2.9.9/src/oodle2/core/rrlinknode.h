// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_LINKNODEH__
#define __RADRR_LINKNODEH__

/*********************

rrLinkNode is a doubly linked list

It's a struct that you put right inside your object that you want to link, eg :

struct Stuff
{
    char myjunk[16];
    rrLinkNode hashList;
    rrLinkNode LRU;
};

Note that you can have multiple links through the same object.

*Next and *Prev should never be NULL !!
An Empty list is when ->Next = Self
Lists should be initialized to empty by calling Init()

The circular linkage lets you do lots of things without checking null, for example to pop off the first item :
    rrLN_Cut(list.Next);
just works whether or not the list is empty

list.Prev is obviously the last member in the list.

Note : the Next & Prev pointers point at the next rrLinkNode , not the next object!
To get to your object from a linknode, use MEMBER_TO_OWNER.  For example :

Stuff * curStuff;
Stuff * nextStuff = MEMBER_TO_OWNER(Stuff,hashList, curStuff.hashList.Next);
Stuff * nextByLRU = MEMBER_TO_OWNER(Stuff,LRU, curStuff.LRU.Next);



If you're doing an object with only one linkage, it's easiest to just put the rrLinkNode at the head of the object,
so you can cast directly from the Link to the object.  Check with

COMPILER_ASSERT( MEMBER_OFFSET(Stuff,listAtHead) == 0 );

********************/

#include "oodlecore.h"

OODLE_NS_START

// @@ rrLinkNode ?
struct rrLinkNode 
{
    rrLinkNode *m_next,*m_prev;
    
    // initialize with rrLN_Init() ; NULL is not valid!
}; 


#define rrLN_Init(List)                 do { (List)->m_next = List; (List)->m_prev = List; } while(0)
#define rrLN_Cut(Node)                  do { (Node)->m_prev->m_next = (Node)->m_next; (Node)->m_next->m_prev = (Node)->m_prev; rrLN_Init(Node); } while(0)
#define rrLN_Fix(Node)                  do { (Node)->m_prev->m_next = Node; (Node)->m_next->m_prev = Node; } while(0)
#define rrLN_AddAfter(Node,List)        do { (Node)->m_prev = List; (Node)->m_next = (List)->m_next; rrLN_Fix(Node); } while(0)
#define rrLN_AddBefore(Node,List)       do { (Node)->m_next = List; (Node)->m_prev = (List)->m_prev; rrLN_Fix(Node); } while(0)
#define rrLN_IsEmpty(List)              ( (List)->m_next == (List) )

// Walk does a for loop for you, like :
//  rrLinkNode * Node; rrLN_Walk(Node,List) { Node->dostuff(); } 
#define rrLN_Walk_Editting(Node,List,Holder)    for( Node = (List)->m_next; (Node) != (List) && ((Holder) = (Node)->m_next) != NULL ; Node = Holder )
#define rrLN_Walk(Node,List)            for( Node = (List)->m_next; (Node) != (List) ; Node = (Node)->m_next )
#define rrLN_WalkType(Node,List,type)            for( Node = (type *) (List)->m_next; (Node) != (List) ; Node = (type *) (Node)->m_next )
#define rrLN_WalkBack(Node,List)            for( Node = (List)->m_prev; (Node) != (List) ; Node = (Node)->m_prev )

#define rrLN_GetNextStruct(type,member,node)    RR_MEMBER_TO_OWNER(type,member,(node)->m_next)
#define rrLN_GetPrevStruct(type,member,node)    RR_MEMBER_TO_OWNER(type,member,(node)->m_prev)

S32 rrLN_ListLength(const rrLinkNode * list);

RADINLINE void rrLN_AddTail(rrLinkNode * list, rrLinkNode * node)
{
	rrLN_AddBefore(node,list);
}

RADINLINE void rrLN_AddHead(rrLinkNode * list, rrLinkNode * node)
{
	rrLN_AddAfter(node,list);
}

RADINLINE rrLinkNode * rrLN_PopHead(rrLinkNode * list)
{
	rrLinkNode * node = list->m_next;
	if ( node == list ) return NULL;
	rrLN_Cut(node);
	return node;
}

RADINLINE rrLinkNode * rrLN_PopTail(rrLinkNode * list)
{
	rrLinkNode * node = list->m_prev;
	if ( node == list ) return NULL;
	rrLN_Cut(node);
	return node;
}

//-------------------------------------------------

#ifdef __cplusplus

class rrLink : public rrLinkNode
{
public :
    rrLink()
    {
        m_next = this; m_prev = this;
    }

    inline void Init() { m_next = m_prev = this; }
    inline void Cut() { m_prev->m_next = m_next; m_next->m_prev = m_prev; Init(); }
    inline void Fix() { m_prev->m_next = this; m_next->m_prev = this; }
    inline rrbool IsEmpty() { return m_next == this; }

	// call these on the *node* and pass in the *list* ; a bit confusing I know
    inline void AddToHead(rrLinkNode * List) { m_prev = List; m_next = List->m_next; Fix(); }
    inline void AddToTail(rrLinkNode * List)  { m_next = List; m_prev = List->m_prev; Fix(); }
};

template <typename T>
class rrLinkT : public rrLink
{
public:
	typedef rrLinkT<T> this_type;
	typedef T value_type;
	
	// Self only works if T inherits from this_type
	value_type * Self()
	{
		// get parent from node :
		value_type * ret = (value_type *) this;
		// use implicit-cast to check for valid inheritance :
		RR_DURING_ASSERT( this_type * node = ret; );
		RR_ASSERT( node == this );
		return ret;
	}
	
	value_type * GetNext() { return (value_type *) m_next; }
	value_type * GetPrev() { return (value_type *) m_prev; }
	
	const value_type * GetNext() const { return (const value_type *) m_next; }
	const value_type * GetPrev() const { return (const value_type *) m_prev; }
	
	value_type * PopHead()
	{
		value_type * node = GetNext();
		if ( node == this ) return NULL;
		node->Cut();
		return node;
	}

	value_type * PopTail()
	{
		value_type * node = GetPrev();
		if ( node == this ) return NULL;
		node->Cut();
		return node;
	}
	
};

#define rrLNT_Walk_Editting(Node,List,Holder)    for( Node = (List)->GetNext(); (Node) != (List) && ((Holder) = (Node)->GetNext()) != NULL ; Node = Holder )
#define rrLNT_Walk(Node,List)            for( Node = (List)->GetNext(); (Node) != (List) ; Node = (Node)->GetNext() )

#endif // __cplusplus

OODLE_NS_END

#endif // __RADRR_LINKNODEH__
