// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//Written by Jeff Roberts

// This is free and unencumbered software released into the public domain.
//
// Anyone is free to copy, modify, publish, use, compile, sell, or
// distribute this software, either in source code form or as a compiled
// binary, for any purpose, commercial or non-commercial, and by any
// means.
//
// In jurisdictions that recognize copyright laws, the author or authors
// of this software dedicate any and all copyright interest in the
// software to the public domain. We make this dedication for the benefit
// of the public at large and to the detriment of our heirs and
// successors. We intend this dedication to be an overt act of
// relinquishment in perpetuity of all present and future rights to this
// software under copyright law.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
// OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
// ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.

/*
   This a generic sorting routine.  You set defines that setup how the sort
   should be performed and then you include this file.  It cleans up all of 
   the stuff that it defines (and you define for it) after it has been 
   included.

   The sort emitted at the statement level.  That is, it doesn't create a new
   function - it generates code right at the point it is included. You can 
   still use a function based sort if you want, of course, just make an
   holder function and then include sort.inl there.

   The sort is a tweaked quicksort that falls back to a heap sort if it
   detects that it is going quadratic.  You can also force it to just use
   heapsort if you want the code size to be smaller.

   It is 25% to 400% faster than the other sort I've tested (and I tested a
   bunch) on random data.  On highly skewed data (like 99% zeros) and/or 
   already sorted data, we are much faster than qsorts and run about twice
   as fast as other heap sorts that I've tested.  We never go quadratic.

   It's the sweet action!  See the end of the defines for some of the specific
   coolness in the code itself.
   


   Defines:

   SORTINDEX - this the type name of an index to the data that you are sorting.
     This will usually just be a pointer to the type of data you are sorting:

       #define SORTINDEX int *
       
     However, sort.inl allows you to use any type of index - integer offsets
     from the beginning of an array, or whatever.  The fastest code is 
     generated when using pointers to your elements, though.  (Why you ask, 
     would you use anything else, then?  Well, you can use integer offsets to 
     sort a set of arrays, for example.  Like, say you had an array of names, 
     and a separate array of cities - you'd want to use integer offsets so that
     you could swap the elements in both arrays).

     If you are sorting structures, then you can also add an alignment macro
     to the end of your SORTINDEX which will let platforms like the PS2 swap
     your structure more rapidly (assuming your structures are aligned). For
     example, let's say your structure is 16 bytes long, you could use:

       #define SORTINDEX mystruct * SORTALIGN(16)



    SORTFIRSTINDEX - this macro should be defined to return the first SORTINDEX.
      It is only called once at the beginning of the routine.  Usually this
      macro will just return a pointer to the base of the array to be sorted,
      or just 0, if you are using integer indexes.

        #define SORTFIRSTINDEX &array[0]



    SORTLENGTH *optional define* - this macro should be defined to return the
      number of elements to sort. It can be left undefined if you define
      SORTLASTINDEX instead.  You can also define both SORTLENGTH *and* 
      SORTLASTINDEX, if you happen to know both ahead of time (or they are 
      constant or whatever).  This macro is only called once at the beginning
      of the routine.

        #define SORTLENGTH count_of_elements



    SORTLASTINDEX *optional define* - this macro should be defined to return the
      the last valid SORTINDEX (so, it is "first+length-1"). It can be left 
      undefined if you define SORTLENGTH instead.  You can also define both 
      SORTLENGTH *and* SORTLASTINDEX, if you happen to know both ahead of time 
      (or they are constant or whatever).  This macro is only called once at the
      beginning of the routine.

        #define SORTLASINDEX &array[ MAX_ELEMENTS - 1 ]



    SORTSWAP - this macro is called with two SORTINDEXes for two elements that
      need to be swapped.  We have a predefined macro (SORTSWAPDEFAULT) that you 
      should almost always use that will do the swap quickly for a specified type, 
      so generally this macro will be defined something like:

        #define SORTSWAP( index1, index2 ) SORTSWAPDEFAULT( index1, index2, float )

      If you are sorting independent arrays, you'd do something like:

        #define SORTSWAP( index1, index2 ) \
          SORTSWAPDEFAULT( &name_array[ index1 ], &name_array[ index2 ], char * ) \
          SORTSWAPDEFAULT( &city_array[ index1 ], &city_array[ index2 ], char * ) \

      There is another predefined macro that you can use when you don't have the type
      of the data, but just a constant length.  If you wanted to use it, use something
      like:

        #define SORTSWAP( index1, index2 ) SORTSWAPLENGTH( index1, index2, sizeof( element ) + 4 )

      Finally, if the length isn't constant (like this a generic routine where
      the length is passed in), then you can use the SORTSWAPGENERIC which loops
      over the dwords until they are all swapped.



     SORTSWAP3 - this macro is just like SORTSWAP, only it takes three elements to
       swap.  Again, we have the default macros, SORTSWAP3DEFAULT, SORTSWAP3LENGTH,
       and SORTSWAP3GENERIC that you can use to rapidly swap memory in a 3 ring.  
       Note that if you write this macro yourself completely, the order of swapping
       is: ind3=ind2; ind2=ind1; ind1=original ind3;



     SORTKEYDECLARE *optional define* - sort.inl does it's compares by comparing 
       a key value against  many other indexed elements.  So, we make a copy of 
       the key from one element and then compare than key to all the others.  We 
       don't know what your key is (it can even be multiple fields), so this 
       macro allows you to define the key variables to compare against.  

       Since we have the indexes that we are comparing to anyway, you don't *have*
       to define this macro at all - you can just compare the values in the two
       elements.  That is, you only need to declare a key if it will be faster than
       repeatedly dereferencing the elements for the comparisons.  Usually the
       smaller and the simpiler the comparisons, the more likely that declaring
       a key is worthwhile.

       So, let's say you have a structure and you have a name and a zip code that 
       you are sorting by. To set this up, you would use something like this:

         #define SORTKEYDECLARE()  char * name_key; char * zip_code;



     SORTKEYSET *optional define* - this macro assigns a given indexed element's 
       key values to the key variables that you defined with SORTKEYDECLARE.  If you
       didn't declare any key variables then you don't need to define this macro
       either. From the name/zip example above, you could use something like this:

         #define SORTKEYSET( index ) name_key = index->name_field;  zip_code = index->zip_field;

       

     SORTKEYISBEFORE, SORTKEYISAFTER - these three macros do the actual comparison.  
       You must define all three because different comparisions are done at different 
       places throughout the sort.  These macros are passed to SORTINDEX values - one 
       that is the key index, and one that is the compare index.  If you used the 
       SORTKEYDECLARE and SORTKEYSET macros, then you don't use the key index at all -
       you just use your saved key variables.  So, if you were sorting by an integer 
       field, you could use macros like this:

         #define SORTKEYDECLARE() int key_value;
         #define SORTKEYSET( index ) key_value = index->int_field;
         #define SORTKEYISBEFORE( key_index, index ) key_value < index->int_field;
         #define SORTKEYISAFTER( key_index, index ) key_value > index->int_field;

     
      
      SORTINDEXADD *optional define* - this macro returns a SORTINDEX advanced by 
        an integer amount. You only need to define this macro in unusual situations
        (like when you are sorting a linked list or sorting on-disc data).  By 
        default, it is simply defined as:
        
          #define SORTINDEXADD( index, amt ) index + amt
        
      

      SORTINDEXNEXT *optional define* - this macro returns a SORTINDEX that has been
        advanced to the next element. You only need to define this macro in unusual
        situations. By default, it is simply defined as:
        
          #define SORTINDEXNEXT( index ) index + 1
        
      

      SORTINDEXPREV *optional define* - this macro returns a SORTINDEX that has been
        backed up to the previous element. You only need to define this macro in 
        unusual situations.  By default, it is simply defined as:
        
          #define SORTINDEXPREV( index ) index - 1
        
      

      SORTINDEXDIFF *optional define* - this macro returns the difference between two
        indexes as a count.  By default, it is simply defined as:
        
          #define SORTINDEXDIFF( index1, index2 ) index1 - index2
        
      

      SORTINDEXSMALLER *optional define* - this macro compares one *index* to another.
        You only need to define this macro in unusual situations. By default, it is
        simply defined as:
        
          #define SORTINDEXSMALLER( index1, index2 ) index1 < index2



      SORTTINY *optional define* - this macro tells sort.inl to just use a tiny
        sorter instead of the mega-action.  This macro just generates the simple
        tail heap sort that is only used in quadratic situations otherwise.  It's
        smaller but still fairly fast.  For simple "sort a list of strings" usage,
        normally sort.inl costs about 2K in code space.  But if you define 
        SORTTINY, then we only use about 300 bytes.  This sort is about 4 times
        slower than the full QuickSort.


      SORTNAME *optional define* - CB added this -
        if you put sort.inl in the same function more than once it will crap out with label
        redefinitions.  You can fix that by doing #define SORTNAME to something unique in
        front of either one or both of your sorts


      NDEBUG - you don't define this, your building make file or environment does -
        this is just the macro we assume is defined when building in release mode.
        We still work if it isn't defined, just not as quickly.



   Examples:

   Sort a list of integers:

     int numbers[ 64 ];

     // filled numbers here somehow

     #define SORTINDEX int *
     #define SORTFIRSTINDEX numbers
     #define SORTLENGTH 64
   
     #define SORTSWAP( ind1, ind2 ) SORTSWAPDEFAULT( ind1, ind2, int )
     #define SORTSWAP3( ind1, ind2, ind3 ) SORTSWAP3DEFAULT( ind1, ind2, ind3, int )

     #define SORTKEYDECLARE() int key;
     #define SORTKEYSET( key_ind ) key = *key_ind

     #define SORTKEYISBEFORE( key_ind, ind ) key < * ind
     #define SORTKEYISAFTER( key_ind, ind ) key > * ind

     #include "sort.inl"


   Sort two parallel arrays:

     char * names[ 256 ];
     int zips[ 256 ];

     int my_compare( char * n1, char * n2, int z1, int z2 )
     {
       int i;

       i = strcmp( n1, n2 );
       if ( i == 0 )
         i = z1 - z2;

       return( i );
     }

     // filled arrays here somehow

     #define SORTINDEX int
     #define SORTFIRSTINDEX 0
     #define SORTLENGTH 256
   
     #define SORTSWAP( ind1, ind2 ) SORTSWAPDEFAULT( names + ind1, names + ind2, char * ); \
                                    SORTSWAPDEFAULT( zips + ind1, zips + ind2, int )
     #define SORTSWAP3( ind1, ind2, ind3 ) SORTSWAP3DEFAULT( names + ind1, names + ind2, names + ind3, char * ); \
                                           SORTSWAP3DEFAULT( zips + ind1, zips + ind2, zips + ind3, int )

     #define SORTKEYDECLARE() char * key_name; int key_zip;
     #define SORTKEYSET( key_ind ) key_name = names[ key_ind ]; key_zip = zips[ key_ind ];

     #define SORTKEYISBEFORE( key_ind, ind ) ( my_compare( key_name, names[ ind ], key_zip, zips[ ind ] ) < 0 )
     #define SORTKEYISAFTER( key_ind, ind ) ( my_compare( key_name, names[ ind ], key_zip, zips[ ind ] ) > 0 )

     #include "sort.inl"


   Generate a generic sorter that uses callbacks like CRT qsort (this is
     slower than a type specific custom sort, btw):

     typedef int compare_func( void * ind1, void * ind2 );

     void generic_qsort( void * base, int count, int element_size, compare_func * comp )
     {
       #define SORTINDEX int
       #define SORTFIRSTINDEX 0
       #define SORTLENGTH count
     
       #define PTRADD( ptr, val ) ( ( (char*) ptr ) + val )

       #define SORTSWAP( ind1, ind2 ) SORTSWAPGENERIC( PTRADD( base, ind1 ), PTRADD( base, ind2 ), element_size ); 
       #define SORTSWAP3( ind1, ind2, ind3 ) SORTSWAP3GENERIC( PTRADD( base, ind1 ), PTRADD( base, ind2 ), PTRADD( base, ind3 ), element_size ); 

       #define SORTKEYISBEFORE( key_ind, ind ) ( comp( PTRADD( base, key_ind ), PTRADD( base, ind ) ) < 0 )
       #define SORTKEYISAFTER( key_ind, ind ) ( comp( PTRADD( base, key_ind ), PTRADD( base, ind ) ) > 0 )

       #include "sort.inl"
     }
     

        
   Cool algorithm stuff: 

   When we pick our median of three for the pivot value, we sort the values we looked
   at, as long as we are there.  This gives a 8% speed improvement and we don't have 
   to handle groups of 3 specifically since it just happens for free (we do still
   special case groups of 2 and 4, though).

   Our swap helper macros do a lot of trickiness to get the compiler to give optimal 
   register use during swaps for any element size up to 16 bytes (this gives us 80%
   improvments in places).  For elements larger than that we use dwords to swap 
   which is way faster than bytes. 

   The handling of equivalent elements is handled extra carefully to balance 
   needless swaps and extra compares in later stages.  We also use a pivot "area",
   not just a single pivot value.

   We do the standard trick of only semi-recursing on the smaller of the two 
   partitions to avoid the qsort ever using more than log N of stack space.  

   We flip to heapsort when we detect that we picked a crummy pivot, introsort style.
   Basically, we allow the quicksort to recurse log1.5 of the length of the data
   which we detect by offseting the stack pointer at start up time.

   We never flip to other algorithms on some smallness threshold (like 8 or 
   something).  I never saw this *ever* make any sort faster, once you handled
   3 lists with the median pivot finder, and 2 and 4 list specifically.  Once 
   you handle those cases, it was always a lose to special case other small sizes.

   We keep both left and right pointers during the qsort *and* length values.
   This is kind of silly since you can alway just do (right-left+1) to get the
   length, right?  Well, by keeping both, you never have to do any interior
   loop pointer arithmetic.  That's good when you have an element size that 
   isn't a power of 2.  In these cases, we are many times faster than normal
   sort routines (up to 8 times faster than CRT sort).  We do still have to
   do some pointer arithmetic in the fall through to heap sort, though.

   Our heap sort code is the very smallest and more stripped that I've ever seen.
   It uses no sub-functions (can't, since we are declared inline), but also does
   no duplication of code.  It kind of interlaces the pre-heap setup with the
   code that actually sorts the heap values with some duff-ish weirdness.  It is
   goto and logic weirdness heavy.  But it's fast and very small.
   
   One optimization that I didn't do in heap sort which would make it 20%
   faster in some cases is rather than swap as we move down the heap tree,
   you save the top value and then just copy down the tree.  Saves a little
   memory movement, but it would require being able to make a complete
   copy of one of the elements, which I didn't need to do anywhere else.
   Since heapsort is only the fallback sort (unless you are using the
   SORTTINY define), I didn't want to add any macro concepts just for it.
   If everyone ends up using SORTINY all the time, I'll add this logic
   and update the file.

   
   Speed:

   Compared to stl:sort we do half the number of compares and half the swaps
   and are 25% faster on random data. stl::sort uses those extra compares
   to handle sparse data (by sparse I mean all one value with some other 
   random ones - all zeros and a few positive and negative numbers).  We
   could add this as an option mode if we end up with that style of data
   frequently (which seems unlikely).

   It's about 10% faster than Sean's stb sort on random data, and much
   faster on skewed data (lots of repeats).  Sean doesn't use a pivot area
   which is the big difference.

   On non-power-of-2 swaps, we are much faster than any other sort that I've
   tested.  We handle these with custom moves and avoiding any pointer 
   arithmetic in the inner loops.

   The scanning loop for partitioning is 3 asm instructions. It's full speed
   now.  We could probably add some prefetching to help.

   Testing these algorithms is really subjective.  You can make one routine
   run twice as slow by sorting 64K elements vs 64K-1. And the opposite on
   another algorithm.  It's not really realistic to test one array size - 
   testing should use slowly growing sizes and then dump all the timings
   to a csv file so you can graph it in Excel. Otherwise, we just aren't
   going to be measuring anything real. I was semi-detailed, but I'd like
   to go back and really time this properly later on.  Once I had the 
   scanning down to 3 instructions and tallied my swaps and compares
   manually, I felt ok to leave this alone for now.



   Todos:

   Use median of 9 on large partitions (stl::sort does this, but I never 
   saw a win, when I tried it). Didn't help stl:sort if I took it out
   either, but maybe I just need better data.

   Radix sort on scalar data.
   
   SIMD moves and compares on scalar data.
   
   More hints - possibly SORTSKEWED, SORTSPARSE (create exact mid partition
   for many dupes).

   Maybe do a separate loop for lengths 4 and smaller so we don't need to do 
   those extra checks at the top of the loop.

   More testing: slowly changing block length (big difference when sorting
   64K elements vs 64K-1), large block sorts.  Create a median of 3 killer
   data set.

   How to enforce alignment in C without screwing up the code generator?

*/


#ifndef SORTINDEX
#error You must define SORTINDEX typename
#endif

#ifndef SORTSWAP
#error You must define SORTSWAP(index1,index2)
#endif

#ifndef SORTSWAP3
#error You must define SORTSWAP3(index1,index2,index3)
#endif

#ifndef SORTFIRSTINDEX
#error You must define SORTFIRSTINDEX first_index
#endif

#if !defined(SORTLENGTH) && !(defined(SORTLASTINDEX))
#error You must define either SORTLENGTH or SORTLASTINDEX (or both)
#endif

#if defined(SORTKEYDECLARE) && !defined(SORTKEYSET)
#error If you define SORTKEYDECLARE, you should define SORTKEYSET
#endif

#ifndef SORTKEYISBEFORE
#error You must define SORTKEYISBEFORE( key_index, index )
#endif

#ifndef SORTKEYISAFTER
#error You must define SORTKEYISAFTER( key_index, index )
#endif

#ifndef SORTKEYDECLARE
#define SORTKEYDECLARE()
#endif

#ifndef SORTKEYSET
#define SORTKEYSET( ignored )
#endif

#ifndef SORTINDEXADD
#define SORTINDEXADD( ind1, val ) ( ind1 + val )
#endif

#ifndef SORTINDEXDIFF
#define SORTINDEXDIFF( index1, index2 ) (SORTLENTYPE)( index1 - index2 )
#endif

#ifndef SORTINDEXNEXT
#define SORTINDEXNEXT( ind ) ( ind + 1 )
#endif

#ifndef SORTINDEXPREV
#define SORTINDEXPREV( ind ) ( ind - 1 )
#endif

#ifndef SORTINDEXSMALLER
#define SORTINDEXSMALLER( ind1, ind2 ) ( ind1 < ind2 )
#endif

#ifndef SORTNAME
#define SORTNAME    rrsort_
#endif

#ifndef SORT_STRING_JOIN
#define SORT_STRING_JOIN_HELPER(a,b)   a##b
#define SORT_STRING_JOIN(a,b)          SORT_STRING_JOIN_HELPER(a,b)
#endif

#ifndef NO_SORT_LABEL
#define NO_SORT_LABEL   SORT_STRING_JOIN(SORTNAME,no_sort)
#endif

#ifndef PARTED_LABEL
#define PARTED_LABEL   SORT_STRING_JOIN(SORTNAME,parted)
#endif

#ifndef SKIPFIRSTDEC_LABEL
#define SKIPFIRSTDEC_LABEL   SORT_STRING_JOIN(SORTNAME,skipfirstdec)
#endif

#ifndef LOOP_LABEL
#define LOOP_LABEL   SORT_STRING_JOIN(SORTNAME,loop)
#endif

#ifndef DOLOOP_LABEL
#define DOLOOP_LABEL   SORT_STRING_JOIN(SORTNAME,doloop)
#endif

#ifndef SORTIFNONPOWER2
#define SORTIFNONPOWER2( code ) if ( ( sizeof( *piv ) & ( sizeof( *piv ) -1 ) ) != 0 ) code
#endif

#ifndef SORTIFPOWER2
#define SORTIFPOWER2( code )    if ( ( sizeof( *piv ) & ( sizeof( *piv ) -1 ) ) == 0 ) code
#endif

#if defined(_MSC_VER) 
  #define SORTALIGN(num) __declspec( align( num ) )  
  #pragma warning( push )
  #pragma warning( disable : 4127 )
  #define SORTPACKED
  #define SORTU64 unsigned __int64
#else
  #define SORTPACKED 
  #define SORTALIGN(num) __attribute__((aligned(num)))
  #define SORTU64 unsigned long long
#endif


// Swapping macros=================================

#define SORTSWAPGENERIC( addr1, addr2, size )      \
do {                                               \
  typedef union { char a,t1,t2,t3; int b;} ct;     \
  ct * a1 = (ct*)addr1;                            \
  ct * a2 = (ct*)addr2;                            \
  unsigned int l = size>>2;                        \
  while(l)                                         \
  {                                                \
    int __t;                                       \
    --l;                                           \
    __t=a1->b;                                     \
    a1->b=a2->b;                                   \
    a2->b=__t;                                     \
    a1=(ct*)(((char*)a1)+4);                       \
    a2=(ct*)(((char*)a2)+4);                       \
  }                                                \
  l = size&3;                                      \
  while(l)                                         \
  {                                                \
    char __t;                                      \
    --l;                                           \
    __t=(a1->a);                                   \
    (a1->a)=(a2->a);                               \
    (a2->a)=__t;                                   \
    a1=(ct*)(((char*)a1)+1);                       \
    a2=(ct*)(((char*)a2)+1);                       \
  }                                                \
} while(0)

#define SORTSWAP3GENERIC( addr1, addr2, addr3, size ) \
do {                                               \
  typedef union { char a,t1,t2,t3; int b;} ct;     \
  ct * a1 = (ct*)addr1;                            \
  ct * a2 = (ct*)addr2;                            \
  ct * a3 = (ct*)addr3;                            \
  unsigned int l = size>>2;                        \
  while(l)                                         \
  {                                                \
    int __t;                                       \
    --l;                                           \
    __t=a3->b;                                     \
    a3->b=a2->b;                                   \
    a2->b=a1->b;                                   \
    a1->b=__t;                                     \
    a1=(ct*)(((char*)a1)+4);                       \
    a2=(ct*)(((char*)a2)+4);                       \
    a3=(ct*)(((char*)a3)+4);                       \
  }                                                \
  l = size&3;                                      \
  while(l)                                         \
  {                                                \
    char __t;                                      \
    --l;                                           \
    __t=a3->a;                                     \
    a3->a=a2->a;                                   \
    a2->a=a1->a;                                   \
    a1->a=__t;                                     \
    a1=(ct*)(((char*)a1)+1);                       \
    a2=(ct*)(((char*)a2)+1);                       \
    a3=(ct*)(((char*)a3)+1);                       \
  }                                                \
} while(0)

#ifdef NDEBUG

#define SORTSWAPFROMTO(addr1,addr2, kind, copyas ) \
do {                                               \
  typedef union { kind a; copyas b; } ct;          \
  copyas __t = ((ct*)addr1)[0].b;                  \
  ((ct*)addr1)[0].b = ((ct*)addr2)[0].b;           \
  ((ct*)addr2)[0].b = __t;                         \
} while(0)


#define SORTSWAP3FROMTO(addr1,addr2, addr3, kind, copyas )    \
do {                                               \
  typedef union { kind a; copyas b; } ct;          \
  copyas __t = ((ct*)addr3)[0].b;                  \
  ((ct*)addr3)[0].b = ((ct*)addr2)[0].b;           \
  ((ct*)addr2)[0].b = ((ct*)addr1)[0].b;           \
  ((ct*)addr1)[0].b = __t;                         \
} while(0)


#define SORTSWAPSPECIFIC(addr1,addr2, kind )       \
do {                                               \
  if ( sizeof( kind ) == 1 )                       \
    SORTSWAPFROMTO( addr1, addr2, kind, char );    \
  else if ( sizeof( kind ) == 2 )                  \
    SORTSWAPFROMTO( addr1, addr2, kind, short );   \
  else if ( sizeof( kind ) == 3 )                  \
    SORTSWAPFROMTO( addr1, addr2, kind, __ssl3 );  \
  else if ( sizeof( kind ) == 4 )                  \
    SORTSWAPFROMTO( addr1, addr2, kind, int );     \
  else if ( sizeof( kind ) == 5 )                  \
    SORTSWAPFROMTO( addr1, addr2, kind, __ssl5 );  \
  else if ( sizeof( kind ) == 6 )                  \
    SORTSWAPFROMTO( addr1, addr2, kind, __ssl6 );  \
  else if ( sizeof( kind ) == 7 )                  \
    SORTSWAPFROMTO( addr1, addr2, kind, __ssl7 );  \
  else if ( sizeof( kind ) == 8 )                  \
    SORTSWAPFROMTO( addr1, addr2, kind, __ssl8 );  \
  else if ( sizeof( kind ) == 9 )                  \
    SORTSWAPFROMTO( addr1, addr2, kind, __ssl9 );  \
  else if ( sizeof( kind ) == 10 )                 \
    SORTSWAPFROMTO( addr1, addr2, kind, __ssl10 ); \
  else if ( sizeof( kind ) == 11 )                 \
    SORTSWAPFROMTO( addr1, addr2, kind, __ssl11 ); \
  else if ( sizeof( kind ) == 12 )                 \
    SORTSWAPFROMTO( addr1, addr2, kind, __ssl12 ); \
  else if ( sizeof( kind ) == 13 )                 \
    SORTSWAPFROMTO( addr1, addr2, kind, __ssl13 ); \
  else if ( sizeof( kind ) == 14 )                 \
    SORTSWAPFROMTO( addr1, addr2, kind, __ssl14 ); \
  else if ( sizeof( kind ) == 15 )                 \
    SORTSWAPFROMTO( addr1, addr2, kind, __ssl15 ); \
  else if ( sizeof( kind ) == 16 )                 \
    SORTSWAPFROMTO( addr1, addr2, kind, __ssl16 ); \
  else                                             \
    SORTSWAPGENERIC( addr1, addr2, sizeof(kind) ); \
} while(0)


#define SORTSWAP3SPECIFIC(addr1,addr2,addr3, kind )        \
do {                                                       \
  if ( sizeof( kind ) == 1 )                               \
    SORTSWAP3FROMTO( addr1, addr2, addr3, kind, char );    \
  else if ( sizeof( kind ) == 2 )                          \
    SORTSWAP3FROMTO( addr1, addr2, addr3, kind, short );   \
  else if ( sizeof( kind ) == 3 )                          \
    SORTSWAP3FROMTO( addr1, addr2, addr3, kind, __ssl3 );  \
  else if ( sizeof( kind ) == 4 )                          \
    SORTSWAP3FROMTO( addr1, addr2, addr3, kind, int );     \
  else if ( sizeof( kind ) == 5 )                          \
    SORTSWAP3FROMTO( addr1, addr2, addr3, kind, __ssl5 );  \
  else if ( sizeof( kind ) == 6 )                          \
    SORTSWAP3FROMTO( addr1, addr2, addr3, kind, __ssl6 );  \
  else if ( sizeof( kind ) == 7 )                          \
    SORTSWAP3FROMTO( addr1, addr2, addr3, kind, __ssl7 );  \
  else if ( sizeof( kind ) == 8 )                          \
    SORTSWAP3FROMTO( addr1, addr2, addr3, kind, __ssl8 );  \
  else if ( sizeof( kind ) == 9 )                          \
    SORTSWAP3FROMTO( addr1, addr2, addr3, kind, __ssl9 );  \
  else if ( sizeof( kind ) == 10 )                         \
    SORTSWAP3FROMTO( addr1, addr2, addr3, kind, __ssl10 ); \
  else if ( sizeof( kind ) == 11 )                         \
    SORTSWAP3FROMTO( addr1, addr2, addr3, kind, __ssl11 ); \
  else if ( sizeof( kind ) == 12 )                         \
    SORTSWAP3FROMTO( addr1, addr2, addr3, kind, __ssl12 ); \
  else if ( sizeof( kind ) == 13 )                         \
    SORTSWAP3FROMTO( addr1, addr2, addr3, kind, __ssl13 ); \
  else if ( sizeof( kind ) == 14 )                         \
    SORTSWAP3FROMTO( addr1, addr2, addr3, kind, __ssl14 ); \
  else if ( sizeof( kind ) == 15 )                         \
    SORTSWAP3FROMTO( addr1, addr2, addr3, kind, __ssl15 ); \
  else if ( sizeof( kind ) == 16 )                         \
    SORTSWAP3FROMTO( addr1, addr2, addr3, kind, __ssl16 ); \
  else                                                     \
    SORTSWAP3GENERIC( addr1, addr2, addr3, sizeof(kind) ); \
} while(0)


#define SORTSWAPDEFAULT( addr1, addr2, kind )                \
    SORTSWAPSPECIFIC( addr1, addr2, kind )                   \


#define SORTSWAP3DEFAULT( addr1, addr2, addr3, kind )        \
    SORTSWAP3SPECIFIC( addr1, addr2, addr3, kind )           \


#define SORTSWAPLENGTH( addr1, addr2, len )                  \
do {                                                         \
  char forceuseconstantforlenparameter[ len ? 1 : - 1 ]={0}; \
  if ( len == 1 )                                            \
    SORTSWAPFROMTO( addr1, addr2, char, char );              \
  else if ( len == 2 )                                       \
    SORTSWAPFROMTO( addr1, addr2, short, short );            \
  else if ( len == 3 )                                       \
    SORTSWAPFROMTO( addr1, addr2, __ssl3, __ssl3 );          \
  else if ( len == 4 )                                       \
    SORTSWAPFROMTO( addr1, addr2, int, int );                \
  else if ( len == 5 )                                       \
    SORTSWAPFROMTO( addr1, addr2, __ssl5, __ssl5 );          \
  else if ( len == 6 )                                       \
    SORTSWAPFROMTO( addr1, addr2, __ssl6, __ssl6 );          \
  else if ( len == 7 )                                       \
    SORTSWAPFROMTO( addr1, addr2, __ssl7, __ssl7 );          \
  else if ( len == 8 )                                       \
    SORTSWAPFROMTO( addr1, addr2, __ssl8 , __ssl8 );         \
  else if ( len == 9 )                                       \
    SORTSWAPFROMTO( addr1, addr2, __ssl9, __ssl9 );          \
  else if ( len == 10 )                                      \
    SORTSWAPFROMTO( addr1, addr2, __ssl10, __ssl10 );        \
  else if ( len == 11 )                                      \
    SORTSWAPFROMTO( addr1, addr2, __ssl11, __ssl11 );        \
  else if ( len == 12 )                                      \
    SORTSWAPFROMTO( addr1, addr2, __ssl12, __ssl12 );        \
  else if ( len == 13 )                                      \
    SORTSWAPFROMTO( addr1, addr2, __ssl13, __ssl13 );        \
  else if ( len == 14 )                                      \
    SORTSWAPFROMTO( addr1, addr2, __ssl14, __ssl14 );        \
  else if ( len == 15 )                                      \
    SORTSWAPFROMTO( addr1, addr2, __ssl15, __ssl15 );        \
  else if ( len == 16 )                                      \
    SORTSWAPFROMTO( addr1, addr2, __ssl16, __ssl16 );        \
  else                                                       \
    SORTSWAPGENERIC( addr1, addr2, len );                    \
} while(0)


#define SORTSWAP3LENGTH( addr1, addr2, addr3, len )          \
do {                                                         \
  char forceuseconstantforlenparameter[ len ? 1 : - 1 ]={0}; \
  if ( len == 1 )                                            \
    SORTSWAP3FROMTO( addr1, addr2, addr3, char, char );      \
  else if ( len == 2 )                                       \
    SORTSWAP3FROMTO( addr1, addr2, addr3, short, short );    \
  else if ( len == 3 )                                       \
    SORTSWAP3FROMTO( addr1, addr2, addr3, __ssl3, __ssl3 );  \
  else if ( len == 4 )                                       \
    SORTSWAP3FROMTO( addr1, addr2, addr3, int, int );        \
  else if ( len == 5 )                                       \
    SORTSWAP3FROMTO( addr1, addr2, addr3, __ssl5, __ssl5 );  \
  else if ( len == 6 )                                       \
    SORTSWAP3FROMTO( addr1, addr2, addr3, __ssl6, __ssl6 );  \
  else if ( len == 7 )                                       \
    SORTSWAP3FROMTO( addr1, addr2, addr3, __ssl7, __ssl7 );  \
  else if ( len == 8 )                                       \
    SORTSWAP3FROMTO( addr1, addr2, addr3, __ssl8, __ssl8 );  \
  else if ( len == 9 )                                       \
    SORTSWAP3FROMTO( addr1, addr2, addr3, __ssl9, __ssl9 );  \
  else if ( len == 10 )                                      \
    SORTSWAP3FROMTO( addr1, addr2, addr3, __ssl10, __ssl10 );\
  else if ( len == 11 )                                      \
    SORTSWAP3FROMTO( addr1, addr2, addr3, __ssl11, __ssl11 );\
  else if ( len == 12 )                                      \
    SORTSWAP3FROMTO( addr1, addr2, addr3, __ssl12, __ssl12 );\
  else if ( len == 13 )                                      \
    SORTSWAP3FROMTO( addr1, addr2, addr3, __ssl13, __ssl13 );\
  else if ( len == 14 )                                      \
    SORTSWAP3FROMTO( addr1, addr2, addr3, __ssl14, __ssl14 );\
  else if ( len == 15 )                                      \
    SORTSWAP3FROMTO( addr1, addr2, addr3, __ssl15, __ssl15 );\
  else if ( len == 16 )                                      \
    SORTSWAP3FROMTO( addr1, addr2, addr3, __ssl16, __ssl16 );\
  else                                                       \
    SORTSWAP3FROMTO( addr1, addr2, addr3, len );            \
} while(0)

#else

#define SORTSWAPDEFAULT( addr1, addr2, kind )                \
    SORTSWAPGENERIC( addr1, addr2, sizeof( kind ) )

#define SORTSWAPLENGTH( addr1, addr2, len )                  \
    SORTSWAPGENERIC( addr1, addr2, len )

#define SORTSWAP3DEFAULT( addr1, addr2, addr3, kind )        \
    SORTSWAP3GENERIC( addr1, addr2, addr3, sizeof( kind ) )
 
#define SORTSWAP3LENGTH( addr1, addr2, addr3, len )          \
    SORTSWAP3GENERIC( addr1, addr2, addr3, len )

#endif

// ================================================



// Sorting code starts here!

{
  #if !defined(R5900)
    #if defined(_PUSHPOP_SUPPORTED) || (defined(PRAGMA_STRUCT_PACKPUSH) && PRAGMA_STRUCT_PACKPUSH)
      #pragma pack(push,1)
    #else
      #pragma pack(1)
    #endif
  #endif
  #ifdef SORTLENTYPE
    #undef SORTLENTYPE
  #endif
  #if defined(_WIN64)
    #define SORTLENTYPE unsigned __int64
  #else
    #define SORTLENTYPE unsigned int
  #endif

  typedef struct SORTPACKED __ssl3 { short s; char c; } __ssl3;
  typedef struct SORTPACKED __ssl5 { int i; char c; } __ssl5;
  typedef struct SORTPACKED __ssl6 { int i; short s; } __ssl6;
  typedef struct SORTPACKED __ssl7 { int i; short s; char c; } __ssl7;
  typedef struct SORTPACKED __ssl8 { SORTU64 u; } __ssl8;
  typedef struct SORTPACKED __ssl9 { SORTU64 u; char c; } __ssl9;
  typedef struct SORTPACKED __ssl10 { SORTU64 u; short s; } __ssl10;
  typedef struct SORTPACKED __ssl11 { SORTU64 u; short s; char c; } __ssl11;
  typedef struct SORTPACKED __ssl12 { SORTU64 u; int i; } __ssl12;
  typedef struct SORTPACKED __ssl13 { SORTU64 u; int i; char c; } __ssl13;
  typedef struct SORTPACKED __ssl14 { SORTU64 u; int i; short s; } __ssl14;
  typedef struct SORTPACKED __ssl15 { SORTU64 u; int i; short s; char c; } __ssl15;
  typedef struct SORTPACKED __ssl16 { SORTU64 u1; SORTU64 u2; } __ssl16;
  #if !defined(R5900)
    #if defined(_PUSHPOP_SUPPORTED) || (defined(PRAGMA_STRUCT_PACKPUSH) && PRAGMA_STRUCT_PACKPUSH)
      #pragma pack(pop)
    #else
      #pragma pack()
    #endif
  #endif

  SORTINDEX left;
  SORTINDEX right;
  SORTLENTYPE length;
  SORTKEYDECLARE()

#ifndef SORTTINY
  // stack for left, right, and length (we keep track of the length
  //   so that we never subtract pointers - in case of non-power-2
  //   sort sizes - we are *much* faster there)

  typedef struct stks
  {
    SORTINDEX left;
    SORTINDEX right;
    SORTLENTYPE len;
  } stks;

  stks stk[ 64 ];
  stks * stk_ptr = stk + 64;

#endif

  #ifdef SORTLENGTH
    length = SORTLENGTH;
    left = (SORTINDEX) ( SORTFIRSTINDEX );
    #ifdef SORTLASTINDEX
      right = (SORTINDEX) ( SORTLASTINDEX );
    #else
      right = SORTINDEXADD( left, ( length - 1 ) );
    #endif
  #else
    left = (SORTINDEX) ( SORTFIRSTINDEX );
    right = (SORTINDEX) ( SORTLASTINDEX );
    #ifdef SORTLENGTH
      length = SORTLENGTH;
    #else
      length = SORTINDEXDIFF( right, left ) + 1; 
    #endif
  #endif

  if ( length > 1 )
  {
   
#ifndef SORTTINY

    // we use the stk_ptr to tell when to flip to heap.
    //   when we hit the end of the stack, heap it, so
    //    back the start of the stack to log1.5 of len
    {
      SORTLENTYPE levs = length;
      do
      {
        --stk_ptr;
        levs = ( levs >> 1 ) + ( levs >> 2 );
      }
      while ( levs );
      stk_ptr[ -1 ].len = 0;
    }

    while( length )
    {
      while( length > 1 )
      {
        SORTINDEX piv;

        if ( length == 2 )
        {
          SORTKEYSET( right );
          if ( SORTKEYISBEFORE( right, left ) )
            SORTSWAP( left, right );
          break;
        }
        else
        {
          SORTINDEX mid = SORTINDEXADD( left, ( length >> 1 ) );

          // this chooses our pivot and sorts the three values
          SORTKEYSET( mid );
          if ( SORTKEYISBEFORE( mid, left ) )
          {
            if ( SORTKEYISBEFORE( mid, right ) )
            {
              SORTKEYSET( left );
              if ( SORTKEYISBEFORE( left, right ) )
                SORTSWAP( left, mid  ); // was TLR
              else
                SORTSWAP3( mid, left, right ); // was TRL
            }
            else
              SORTSWAP( left, right ); // was RTL
          }
          else
          {
            if ( SORTKEYISAFTER( mid, right ) )
            {
              SORTKEYSET( left );
              if ( SORTKEYISBEFORE( left, right ) )
                SORTSWAP( mid, right ); // was LRT
              else
                SORTSWAP3( left, mid, right ); // was RLT
            }
            // else  // was LTR
          }

          if ( length <= 4 )
          {
            if ( length == 3 )
              break;

            // piv used here as a temp
            piv = SORTINDEXNEXT( left );

            SORTKEYSET( piv );
            if ( SORTKEYISAFTER( piv, mid ) )
              if ( SORTKEYISBEFORE( piv, right ) )
                SORTSWAP( piv, mid );
              else
                SORTSWAP3( mid, piv, right );
            else
              if ( SORTKEYISBEFORE( piv, left ) )
                SORTSWAP( left, piv );
            break;
          }

          // have we hit end of stack, if so, use heap
          if ( stk_ptr == stk + 64 )
#endif
          {
            // unusual heap sort
            SORTINDEX __i; SORTINDEX ind; SORTINDEX __v; SORTINDEX __n;
            SORTLENTYPE __s, __k;

            __s = length >> 1;
            __i = SORTINDEXADD( left, __s );

           LOOP_LABEL:
            --__s;
            __i = SORTINDEXPREV( __i );
            ind = __i;
            __k = ( __s << 1 ) + 1;

           DOLOOP_LABEL:
            __v = SORTINDEXADD( left, __k );
            __n = SORTINDEXNEXT( __v );
            SORTKEYSET( __v );
            if ( ( ( __n <= right ) ) && ( SORTKEYISBEFORE( __v, __n ) ) )
            {
              ++__k;
              __v = __n;
              SORTKEYSET( __v );
            }

            if ( SORTKEYISAFTER( __v, ind ) )
            {
              SORTSWAP( ind, __v );
              ind = __v;
              __k = ( __k << 1 ) + 1;

              if ( __k < length )
                goto DOLOOP_LABEL;
            }

            if ( __s ) goto LOOP_LABEL;

             --length;
            SORTSWAP( left, right );
            right = SORTINDEXPREV( right );
            ind = left;
            __k = 1;
            if ( length > 1 )
              goto DOLOOP_LABEL;
          } 
#ifndef SORTTINY
          else          
          {
            // quick sort
            SORTINDEX l; SORTINDEX r;
            SORTINDEX lpiv;
            SORTINDEX rpiv;
            SORTLENTYPE ll; // left side
            SORTLENTYPE mpl; //mid+left length

            SORTIFNONPOWER2( ll = 0 );
            piv = left;
            l = left;
            r = right;

            SORTKEYSET( mid ); //copy before the swap (no LHS on ppc) this is really piv we are loading
            SORTSWAP( mid, piv );

            for(;;) 
            {
              do
              {
                r = SORTINDEXPREV( r );
              } while ( SORTKEYISBEFORE( piv, r ) ); 
              if ( !SORTINDEXSMALLER( l, r ) ) 
                break;

              do
              { 
                SORTIFNONPOWER2( ++ll );
                l = SORTINDEXNEXT( l );
              } while ( SORTKEYISAFTER( piv, l ) );  

              if ( !SORTINDEXSMALLER( l, r ) ) 
              {
                SORTIFNONPOWER2( --ll );
                l = SORTINDEXPREV( l );
                break;
              }

              SORTSWAP( l, r );
            }

            SORTKEYSET( piv ); // copy before the swap (no LHS on ppc) 
                               // weird because next we swap piv and l, 
                               // but then piv is *set* to l (so we do
                               // load from piv even though we are swapping

            SORTSWAP( piv, l );
            piv = l;

            // now we expand the center to include all the matching pivots
            SORTIFNONPOWER2( mpl = ll );
            rpiv = piv;
            do
            {
              SORTIFNONPOWER2( ++mpl );
              rpiv = SORTINDEXNEXT( rpiv );
            } while ( ( SORTINDEXSMALLER( rpiv, right ) ) && ( !( SORTKEYISBEFORE( piv, rpiv ) ) ) );

            lpiv = piv;
            for(;;)
            {
              lpiv = SORTINDEXPREV( lpiv );
              if ( ( ! ( SORTINDEXSMALLER( left, lpiv ) ) ) || ( SORTKEYISAFTER( piv, lpiv ) ) )
                break;
              SORTIFNONPOWER2( --ll );
            }

            // get length from right side of part to right
            SORTIFNONPOWER2( length -= mpl );

            SORTIFPOWER2(
              length = SORTINDEXDIFF( right, rpiv ) + 1;
              ll = SORTINDEXDIFF( lpiv, left ) + 1;
            )
            
            if ( length < ll )
            {
              // put small right on stack
              stk_ptr->left = rpiv;
              stk_ptr->right = right;
              stk_ptr->len = length;
              ++stk_ptr;

              right = lpiv;
              length = ll;
            } 
            else
            {
              // put small left on stack
              stk_ptr->left = left;
              stk_ptr->right = lpiv;
              stk_ptr->len = ll;
              ++stk_ptr;

              left = rpiv;
            }
          }
        }
      }
        
      --stk_ptr;
      left = stk_ptr->left;
      right = stk_ptr->right;
      length = stk_ptr->len;
    }
#endif
  }
}


#undef SORTINDEX
#undef SORTSWAP
#undef SORTSWAP3
#undef SORTFIRSTINDEX

#ifdef SORTLENGTH
#undef SORTLENGTH
#endif

#ifdef SORTLASTINDEX
#undef SORTLASTINDEX
#endif

#ifdef SORTKEYDECLARE
#undef SORTKEYDECLARE
#endif

#ifdef SORTKEYSET
#undef SORTKEYSET
#endif
 
#undef SORTKEYISBEFORE
#undef SORTKEYISAFTER
#undef SORTINDEXADD
#undef SORTINDEXDIFF
#undef SORTINDEXNEXT
#undef SORTINDEXPREV
#undef SORTINDEXSMALLER
#undef SORTRESTRICT
#undef SORTSWAPGENERIC
#undef SORTSWAP3GENERIC
#undef SORTSWAPSPECIFIC
#undef SORTSWAP3SPECIFIC
#undef SORTSWAPDEFAULT
#undef SORTSWAPLENGTH
#undef SORTSWAP3DEFAULT
#undef SORTSWAP3LENGTH
#undef SORTSWAPFROMTO
#undef SORTSWAP3FROMTO
#undef SORTTINY
#undef SORTPACKED
#undef SORTALIGN
#undef SORTU64
#undef NO_SORT_LABEL
#undef PARTED_LABEL
#undef LOOP_LABEL
#undef DOLOOP_LABEL
#undef SKIPFIRSTDEC_LABEL
#undef SORTNAME
#undef SORT_STRING_JOIN
#undef SORT_STRING_JOIN_HELPER
#undef SORTIFPOWER2
#undef SORTIFNONPOWER2
#undef SORTLENTYPE

#ifdef _MSC_VER
  #pragma warning( pop )
#endif
