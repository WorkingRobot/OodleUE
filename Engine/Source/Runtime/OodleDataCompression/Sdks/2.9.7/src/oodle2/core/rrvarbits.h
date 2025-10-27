// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_VARBITSBH__
#define __RADRR_VARBITSBH__

#include "rrbase.h"
#include "rrprefetch.h"
#include "rrfuzztesthelper.h" // optional
#include "oodlecore.h"

/*************

rrVarBits :

most significant bits go out first
    that is, you can input values one bit at a time thusly :
        value = (value<<1) + rrVarBits_Get1Bit();

Always read/writes the array big-endian
can read/write single bytes or 32 bits or whatever

Get/Put/etc work with the special cases bits == 0 and 32
    eg. if you say GetBits(0) it will return the value 0 and not touch the struct

rrVarBits is a simple struct
    you can save your place in the bitstream by just copying the struct
        rrVarBits save(vb);
    then restore your place by assigning it back
        vb = save;
    useful for speculative IO and debugging

most of the type you should be working with rrVarBits_Locals()

Locals() takes a prefix which is used to make the local var names
All the macros take the prefix as argument.
	eg.
	rrVarBits_Locals(x) defines x_bits;
	then you can say rrVarBits_Get(x);

The macros for locals also work on the struct - just pass struct.m , eg.
	rrVarBits s;
	rrVarBits_Get(s.m);

C++ functions should use rrVarBits_FuncArgs
	and callers use PassArgs
	
C functions should take an rrVarBits_Struct
	and callers can use rrVarBits_StructInit and rrVarBits_Copy

NOTEZ : Get/Put do NOT fill/flush the buffer for you !
 you must manually call Refill or Output
 
 the idea is that you can do several Gets or Puts without a Refill/Output
 as long as you know the total can't exceed N bits

Get :

 always has >= N bits available (if you do Refill)

Put :

 Bits are top-to-bottom
 but they are kept at the bottom of the register
 
 eg. to add more bits, you do
   bits <<= add
   bits |= new

 BitLen is the number currently waiting to be output
 
 you can only output up to N bits !

(N could be 25 bits, but currently is only 24 bits, because we always leave one bit zero to fix the
	Huffman	decode last branch problem with full-of-1-bits )
(and 64 bit version has 32 bits)

If you want to be safe, like in utility code, you can just 
	Refill() before every Get()
	Output() before every Put()
	

***************/

OODLE_NS_START

#if 1 //def __RADX86__
#ifdef _DEBUG
#ifdef __cplusplus
// inlines give you some more debug checks, that's all
#define RR_VARBITS_INLINES
#endif
#endif
#endif

#ifdef __RAD64REGS__
#define RR_VB_64
#endif

#ifdef RR_VB_64

#define RR_VARBITSTYPE		U64
#define RR_VARBITSTYPESIGNED	S64
#define RR_VARBITSTYPELEN	64
#define RR_VARBITSTYPEBYTES	8
#define RR_BITSTOPMASK		(1UL64<<(RR_VARBITSTYPELEN-1))


// 64-bit branchless reloading
#define RR_MINBITSAVAILABLE 56

  // (RR_MINBITSAVAILABLE could be 57)
  
#else

#define RR_VARBITSTYPE		U32
#define RR_VARBITSTYPESIGNED	S32
#define RR_VARBITSTYPELEN	32
#define RR_VARBITSTYPEBYTES	4
#define RR_BITSTOPMASK		(1UL<<(RR_VARBITSTYPELEN-1))
#define RR_MINBITSAVAILABLE 24

  // (RR_MINBITSAVAILABLE could be 25)

#endif // RR_VB_64

// NOTE(fg): As of 2016-03-29, SINTr for bitlentype seems to win on x64 too.
#define RR_BITLENTYPE	SINTr

//=========================================================================================================

// variable bit macros
extern const RAD_ALIGN(U32,c_rrBitMask32[33],32);
extern const RAD_ALIGN(U64,c_rrBitMask64[65],32);
	
// this generates clrlwi on PPC, but only if bits is const
#define rrGetBitMask32_Not0(bits)	(((U32)0xffffffff)>>(U32)(32-(bits)))
#define rrGetBitMask64_Not0(bits)	(((U64)0xffffffffffffffffULL)>>(UINTr)(64-(bits)))
// doesn't work with bits == 32 :
// this will generate an and with a constant :
#define rrGetBitMask32_Not32(bits)  ( (1UL<<(bits)) - 1 )
#define rrGetBitMask64_Not64(bits)  ( (1ULL<<(bits)) - 1 )

// mask with mbits variable :
//  table lookup :
//	on x86, doing the shift is just as fast, but you'd have to avoid the 0 or 32 special cases
#define rrMaskInBits32_V(val,mbits)	( (val) & c_rrBitMask32[mbits] )
#define rrMaskInBits64_V(val,mbits)	( (val) & c_rrBitMask64[mbits] )

// mask with mbits constant :
#ifdef __RADX86__
#define rrMaskInBits32_C(val,mbits)	( (mbits == 32) ? (val) : ((val) & rrGetBitMask32_Not32(mbits)) )
#define rrMaskInBits64_C(val,mbits)	( (mbits == 64) ? (val) : ((val) & rrGetBitMask64_Not64(mbits)) )
#else
#define rrMaskInBits32_C(val,mbits)	( (mbits == 0) ? 0 : ((val) & rrGetBitMask32_Not0(mbits)) )
#define rrMaskInBits64_C(val,mbits)	( (mbits == 0) ? 0 : ((val) & rrGetBitMask64_Not0(mbits)) )
#endif

// rrMaskInBitsR_V : register size mask
#ifdef RR_VB_64
#define c_rrBitMaskVB c_rrBitMask64
#define rrMaskInBitsR_V	rrMaskInBits64_V
#define rrMaskInBitsR_C	rrMaskInBits64_C
#else
#define c_rrBitMaskVB c_rrBitMask32
#define rrMaskInBitsR_V	rrMaskInBits32_V
#define rrMaskInBitsR_C	rrMaskInBits32_C
#endif

// @@ this rrMaskInBits is not really used anymore, now that the PowerPC-centric Rotate method is removed

//=========================================================================================================

#define NVB(pre,post)	pre ## post

// rrVarBits uses BigEndian to get/put U32's :
// PC is around 7.5 clocks LE, 7.8 clocks BE
#define RR_VB_Put32(ptr,val)			RR_PUT32_BE(ptr,val)
#define RR_VB_Put64(ptr,val)			RR_PUT64_BE(ptr,val)

#define RR_VB_Get64_Unsafe(ptr,end)		RR_GET64_BE(ptr)
#define RR_VB_Get32_Unsafe(ptr,end)		RR_GET32_BE(ptr)
#define RR_VB_Get8_Unsafe(ptr,end)		(*(ptr))

// @@ CB TODO : get rid of all the _Safe crap and always use the scratch-copy method instead

#define RR_VB_Get64_Safe(ptr,end)	( ( rrPtrDiff((end)-(ptr)) >= 8 ) ? RR_GET64_BE(ptr) : rrVB_Get64BE_Masked(ptr,end) )
#define RR_VB_Get32_Safe(ptr,end)	( ( rrPtrDiff((end)-(ptr)) >= 4 ) ? RR_GET32_BE(ptr) : rrVB_Get32BE_Masked(ptr,end) )
#define RR_VB_Get8_Safe(ptr,end)		( ( rrPtrDiff((end)-(ptr)) > 0 ) ? (*(ptr)) : 0 )


//U32 rrVB_Get32BE_Masked(const U8 * ptr,const U8 * end);
//U64 rrVB_Get64BE_Masked(const U8 * ptr,const U8 * end);

// these are only used when ptr is too close to end to do a full word grab :
// it seems to help a tiny bit to have these as inlines
//	even though they're basically never used
//	just because having them as function calls adds lots of pre/post code

static RADINLINE U32 rrVB_Get32BE_Masked(const U8 * ptr,const U8 * end)
{
	//*
	U32 b0 = RR_VB_Get8_Safe(ptr+0,end);
	U32 b1 = RR_VB_Get8_Safe(ptr+1,end);
	U32 b2 = RR_VB_Get8_Safe(ptr+2,end);
	U32 b3 = RR_VB_Get8_Safe(ptr+3,end);
	return (b0<<24) | (b1<<16) | (b2<<8) | b3;
	/*/
	U32 t = 0;
	memcpy(&t,ptr,(end-ptr));
	return RR_GET32_BE(&t);
	/**/
}

static RADINLINE U64 rrVB_Get64BE_Masked(const U8 * ptr,const U8 * end)
{
	//*
	U64 first  = rrVB_Get32BE_Masked(ptr,end);
	U64 second = rrVB_Get32BE_Masked(ptr+4,end);
	return (first<<32) | second;
	/*/
	U64 t = 0;
	memcpy(&t,ptr,(end-ptr));
	return RR_GET64_BE(&t);
	/**/
}

//=======================================================

typedef struct _rrVarBits_
{
	U8 * m_cur;
	U8 * m_end;
	RR_VARBITSTYPE m_bits;
	RR_BITLENTYPE m_inv_bitlen;  // _inv_bitlen = MINBITSAVAILABLE - numbits
} rrVarBits;

#define rrVarBits_Temps()   RR_VARBITSTYPE rrVarBits_temp = 0;  RR_UNUSED_VARIABLE(rrVarBits_temp)

#define rrVarBits_Locals(pre)   RR_VARBITSTYPE NVB(pre,_bits) = 0; RR_BITLENTYPE NVB(pre,_inv_bitlen) = 0; U8 * NVB(pre,_cur) = NULL; U8 * NVB(pre,_end) = NULL; RR_UNUSED_VARIABLE( NVB(pre,_end) );

#define rrVarBits_Copy(to,from)	do { NVB(to,_bits) = NVB(from,_bits); NVB(to,_inv_bitlen) = NVB(from,_inv_bitlen); NVB(to,_cur) = NVB(from,_cur); NVB(to,_end) = NVB(from,_end); } while(0)
#define rrVarBits_StructInit(pre) { NVB(pre,_cur) , NVB(pre,_end) , NVB(pre,_bits) , NVB(pre,_inv_bitlen) }

// FuncArgs/PassArgs : should only be used with OOINLINE functions (and C++)
//	lets you pass locals and make functions that act as if they were macros			
//#define rrVarBits_FuncArgs(pre)	RR_VARBITSTYPE & NVB(pre,_bits), RR_BITLENTYPE & NVB(pre,_inv_bitlen), U8 * & NVB(pre,_cur)
#define rrVarBits_FuncArgs(pre)	RR_VARBITSTYPE & NVB(pre,_bits), RR_BITLENTYPE & NVB(pre,_inv_bitlen), U8 * & NVB(pre,_cur), U8 * & NVB(pre,_end)
#define rrVarBits_PassArgs(pre)	NVB(pre,_bits), NVB(pre,_inv_bitlen), NVB(pre,_cur), NVB(pre,_end)

//=========================================================================================================

#define RRVB_MUNGE_BITS(pre)	FUZZ_MUNGE( NVB(pre,_bits) ) 

//=========================================================================================================

#ifdef RR_VB_64
#define rrVarBits_RotL(pre,num)  NVB(pre,_bits) = RR_ROTL64( NVB(pre,_bits) , num )
#else
#define rrVarBits_RotL(pre,num)  NVB(pre,_bits) = RR_ROTL32( NVB(pre,_bits) , num )
#endif

#define rrVarBits_Bits(vb)		NVB(vb,_bits)
//#define rrVarBits_Bits(vb)		( ({ NVB(vb,_bits) = FUZZ_MUNGE( NVB(vb,_bits) ); NVB(vb,_bits); }) )

// BitLen is the # of bits in the buffer
//	 for a Get stream, it's the # you can get before needing a Refill
//	 for a Put stream, it's the # you have already put - so you can put (RR_MINBITSAVAILABLE - BitLen) more before Output
#ifdef RR_VB_64
	#define rrVarBits_BitLen(pre)	(63 - NVB(pre,_inv_bitlen))
	#define rrVarBits_Reset(pre)	NVB(pre,_bits) = 0, NVB(pre,_inv_bitlen) = 63
   // this is clearly worse :
   //#define rrVarBits_BitLen(pre)	( - NVB(pre,_inv_bitlen))
   //#define rrVarBits_Reset(pre)	NVB(pre,_bits) = 0, NVB(pre,_inv_bitlen) = 0
#else
	#define rrVarBits_BitLen(pre)	(RR_MINBITSAVAILABLE - NVB(pre,_inv_bitlen))
	#define rrVarBits_Reset(pre)	NVB(pre,_bits) = 0, NVB(pre,_inv_bitlen) = RR_MINBITSAVAILABLE
#endif

#define rrVarBits_BitsWriteable(pre)	(RR_VARBITSTYPELEN - rrVarBits_BitLen(pre))


#define macro_rrVarBits_Get(pre,count)  ( \
	rrVarBits_temp = NVB(pre,_bits) >> (RR_VARBITSTYPELEN - (count)), \
	NVB(pre,_bits) <<= count,  \
	NVB(pre,_inv_bitlen) += count, \
	rrVarBits_temp \
	)

#define macro_rrVarBits_Get_0Ok(pre,count)  ( \
	rrVarBits_temp = (NVB(pre,_bits)>>1) >> (RR_VARBITSTYPELEN-1 - (count)), \
	NVB(pre,_bits) <<= count,  \
	NVB(pre,_inv_bitlen) += count, \
	rrVarBits_temp \
	)
	
// this form is nice for branching on; it's just a cmp and then jmp :	
#define rrVarBits_Get1(pre)  ( \
	rrVarBits_temp = ( (RR_VARBITSTYPESIGNED)NVB(pre,_bits) < 0 ), \
	NVB(pre,_bits) <<= 1,  \
	NVB(pre,_inv_bitlen) += 1, \
	rrVarBits_temp \
	)

#define RR_VARBITS_GET_CAN_BE_ZERO	0
	
// could make 0 work for constant count :
//#define macro_rrVarBits_Get_C	macro_rrVarBits_Get_0Ok
#define macro_rrVarBits_Get_C	macro_rrVarBits_Get
#define macro_rrVarBits_Get_V	macro_rrVarBits_Get

/*
// Get1 could probably do something trickier like *= 2 on bits and then check the carry flag
#define rrVarBits_Get1(pre)  ( \
	rrVarBits_temp = NVB(pre,_bits) >> (RR_VARBITSTYPELEN - 1), \
	NVB(pre,_bits) <<= 1,  \
	NVB(pre,_inv_bitlen) += 1, \
	rrVarBits_temp \
	)
*/
	
// changed 10-27-15 : no longer allowed to get 0 !
#define rrVarBits_GetInto(into,pre,count)  ( \
	(into) = NVB(pre,_bits) >> (RR_VARBITSTYPELEN - (count)), \
	NVB(pre,_bits) <<= count,  \
	NVB(pre,_inv_bitlen) += count \
	)
	
// Peek count can't be 0	
#define macro_rrVarBits_Peek(pre,count)		( NVB(pre,_bits) >> (RR_VARBITSTYPELEN - count) )
#define macro_rrVarBits_Peek_0Ok(pre,count)  ( (NVB(pre,_bits)>>1) >> (RR_VARBITSTYPELEN-1 - (count)) )

// Use 0 is okay
#define macro_rrVarBits_Use(pre,count)		( NVB(pre,_bits) <<= count, NVB(pre,_inv_bitlen) += count )

#define rrVarBits_Use_V	rrVarBits_Use
#define rrVarBits_Use_C	rrVarBits_Use

// is next bit on ?
//#define rrVarBits_Test1(vb)  ( rrVarBits_Bits(vb) >= (((RR_VARBITSTYPE)1)<<(RR_VARBITSTYPELEN-1)) )
#define rrVarBits_Test1(vb)  ( (RR_VARBITSTYPESIGNED)rrVarBits_Bits(vb) < 0 )
	
#define rrVarBits_Use1(pre)  ( \
	NVB(pre,_bits) <<= 1, \
	NVB(pre,_inv_bitlen) ++ \
	)

// Put is fast when count is constant
#define macro_rrVarBits_Put(pre,val,count)  do { \
	RR_ASSERT( (rrVarBits_BitLen(pre) + count) <= RR_VARBITSTYPELEN ); \
	RR_ASSERT( (RR_VARBITSTYPE)(val) < ((RR_VARBITSTYPE)1<<(count)) ); \
	NVB(pre,_bits) <<= count; \
	NVB(pre,_bits) |= val; \
	NVB(pre,_inv_bitlen) -= count; \
	} while(0)	

// handy aliases for putting 1 bit :
#define rrVarBits_Puta1(vb)	rrVarBits_Put(vb,1,1)
#define rrVarBits_Puta0(vb)	rrVarBits_Put(vb,0,1)

//===========================================================================================================
// Open, Flush :

// note : rrVarBits_GetOpen does a Refill so you have bits
//	it will also Get to align

#define rrVarBits_PutOpen(pre,ptr) rrVarBits_Reset(pre), NVB(pre,_cur) = ptr

#define rrVarBits_GetOpen_NoRefill(pre,ptr,endPtr) do { rrVarBits_Reset(pre); NVB(pre,_cur) = (U8 *) ptr;  NVB(pre,_end) = (U8 *) endPtr; } while(0)	
#define rrVarBits_GetOpen(pre,ptr,endPtr) do { rrVarBits_GetOpen_NoRefill(pre,ptr,endPtr); rrVarBits_Refill_Safe_Align(pre); } while(0)
	
#define rrVarBits_PutFlush8(pre) do { rrVarBits_Output(pre); \
	if ( rrVarBits_BitLen(pre) > 0 ) { RR_BITLENTYPE n = 64 - rrVarBits_BitLen(pre); n &= 7; if ( n != 0 ) { rrVarBits_Put(pre,0,n); } rrVarBits_OutputBytes(pre); } \
	RR_ASSERT( rrVarBits_BitLen(pre) == 0 ); } while(0)

#define rrVarBits_PutFlush8Back(pre) do { rrVarBits_OutputBack(pre); \
	if ( rrVarBits_BitLen(pre) > 0 ) { RR_BITLENTYPE n = 64 - rrVarBits_BitLen(pre); n &= 7; if ( n != 0 ) { rrVarBits_Put(pre,0,n); } rrVarBits_OutputBytesBack(pre); } \
	RR_ASSERT( rrVarBits_BitLen(pre) == 0 ); } while(0)

//===========================================================================================================
// Output and Refill :

// RRVB_PREFETCH is done in Refill
//#define RRVB_PREFETCH(ptr)
// *1 beats *2 slightly
#if 0//defined(__RADARM__)
#define RRVB_PREFETCH(ptr)		RR_PREFETCHR_CL((const U8 *)(ptr) + RR_CACHE_LINE_SIZE)
#define RRVB_PREFETCHBACK(ptr)	RR_PREFETCHR_CL((const U8 *)(ptr) - RR_CACHE_LINE_SIZE)
#else
#define RRVB_PREFETCH(ptr)
#define RRVB_PREFETCHBACK(ptr)
#endif

#ifdef RR_VB_64

// rrVarBits_OutputBytes : slow version for now, just used for final flush
#define rrVarBits_OutputBytes(pre)  do { \
	while ( rrVarBits_BitLen(pre) >= 8 ) { \
		*(NVB(pre,_cur)) = (U8)(NVB(pre,_bits) >> (rrVarBits_BitLen(pre) - 8)); \
		NVB(pre,_cur) ++; \
		NVB(pre,_inv_bitlen) += 8; \
		NVB(pre,_bits) = rrMaskInBits32_V(NVB(pre,_bits),rrVarBits_BitLen(pre)); \
	} } while(0)	
	
#define rrVarBits_OutputBytesBack(pre)  do { \
	while ( rrVarBits_BitLen(pre) >= 8 ) { \
		NVB(pre,_cur) --; \
		*(NVB(pre,_cur)) = (U8)(NVB(pre,_bits) >> (rrVarBits_BitLen(pre) - 8)); \
		NVB(pre,_inv_bitlen) += 8; \
		NVB(pre,_bits) = rrMaskInBits32_V(NVB(pre,_bits),rrVarBits_BitLen(pre)); \
	} } while(0)	
	
// 64 bit varbits, refill 64 branchless

// Output : bits is bitlen long at the bottom of the word;
//	put the top of the word out first

// Output 64 at a time :	
// just put what we have
// could check if bytes_output == 0 here ? might help speed in some cases
// this Output requires 8 bytes of trash space
#define rrVarBits_Output(pre)  do { \
		RR_BITLENTYPE bytes_output = rrVarBits_BitLen(pre)>>3; \
		RR_VB_Put64(NVB(pre,_cur),(NVB(pre,_bits) << (64 - rrVarBits_BitLen(pre)))); \
		NVB(pre,_cur) += bytes_output; \
		NVB(pre,_inv_bitlen) += bytes_output<<3; \
	} while(0)	

#define rrVarBits_OutputBack(pre)  do { \
		RR_BITLENTYPE bytes_output = rrVarBits_BitLen(pre)>>3; \
		RR_PUT64_LE((NVB(pre,_cur)-8),(NVB(pre,_bits) << (64 - rrVarBits_BitLen(pre)))); \
		NVB(pre,_cur) -= bytes_output; \
		NVB(pre,_inv_bitlen) += bytes_output<<3; \
	} while(0)	
	
//	NVB(pre,_bits) = rrMaskInBits32_V(NVB(pre,_bits),rrVarBits_BitLen(pre))); \
		
// storing bitlen inverted is a liability here
// because we don't branch on it
// if I am going to store it inverted, it should be 64 - bitlen , not MINBITS - bitlen

// don't refill to 64 bits
// just because if I do then >> BitLen gets fucked because >>64 doesn't work				
			

#define rrVarBits_Refill_Unsafe(pre) do { \
		RR_ASSERT( rrVarBits_BitLen(pre) >= 0 && rrVarBits_BitLen(pre) < 64 ); \
		RR_BITLENTYPE bytes_consumed = (63 - rrVarBits_BitLen(pre)) >> 3; \
		U64 next = RR_VB_Get64_Unsafe(NVB(pre,_cur),NVB(pre,_end)); \
		NVB(pre,_bits) |= next >> rrVarBits_BitLen(pre); \
		NVB(pre,_cur) += bytes_consumed; \
		NVB(pre,_inv_bitlen) -= bytes_consumed<<3; \
		RRVB_PREFETCH( NVB(pre,_cur) ); \
		RRVB_MUNGE_BITS(pre); } while(0)	
	
#define rrVarBits_RefillBack_Unsafe(pre) do { \
		RR_ASSERT( rrVarBits_BitLen(pre) >= 0 && rrVarBits_BitLen(pre) < 64 ); \
		RR_BITLENTYPE bytes_consumed = (63 - rrVarBits_BitLen(pre)) >> 3; \
		U64 next = RR_GET64_LE(NVB(pre,_cur)-8); \
		NVB(pre,_bits) |= next >> rrVarBits_BitLen(pre); \
		NVB(pre,_cur) -= bytes_consumed; \
		NVB(pre,_inv_bitlen) -= bytes_consumed<<3; \
		RRVB_PREFETCHBACK( NVB(pre,_cur) ); \
		RRVB_MUNGE_BITS(pre); } while(0)	
		
#define rrVarBits_Refill_Safe(pre) do { \
		RR_ASSERT( rrVarBits_BitLen(pre) >= 0 && rrVarBits_BitLen(pre) < 64 ); \
		RR_BITLENTYPE bytes_consumed = (63 - rrVarBits_BitLen(pre)) >> 3; \
		U64 next = RR_VB_Get64_Safe(NVB(pre,_cur),NVB(pre,_end)); \
		NVB(pre,_bits) |= next >> rrVarBits_BitLen(pre); \
		NVB(pre,_cur) += bytes_consumed; \
		NVB(pre,_inv_bitlen) -= bytes_consumed<<3; \
		RRVB_PREFETCH( NVB(pre,_cur) ); \
		RRVB_MUNGE_BITS(pre); } while(0)	

#define rrVarBits_Refill_Safe_Align( pre )				\
do {													\
	if ( rrVarBits_BitLen(pre) == 0 )					\
	{													\
		U64 b = RR_VB_Get8_Safe(NVB(pre,_cur),NVB(pre,_end));\
		NVB(pre,_cur) ++;								\
		NVB(pre,_bits) = (b << 56 );					\
		NVB(pre,_inv_bitlen) -= 8;						\
	}													\
	RRVB_MUNGE_BITS(pre);								\
	rrVarBits_Refill_Safe(pre);								\
} while(0)


#else // not RR_VB_64

// Output : bits is bitlen long at the bottom of the word;
//	put the top of the word out first
	
// BitLen = 24 - _inv_bitlen ; BitLen needs to be < 8 , so _inv_bitlen needs to be > 16
#define rrVarBits_Output(pre)  do { \
	if ( NVB(pre,_inv_bitlen) <= 16 ) { \
		NVB(pre,_inv_bitlen) += 8; \
		*NVB(pre,_cur)++ = (U8)(NVB(pre,_bits) >> (24 - NVB(pre,_inv_bitlen))) ; \
		if ( NVB(pre,_inv_bitlen) <= 16 ) { \
			NVB(pre,_inv_bitlen) += 8; \
			*NVB(pre,_cur)++ = (U8)(NVB(pre,_bits) >> (24 - NVB(pre,_inv_bitlen))) ; \
			if ( NVB(pre,_inv_bitlen) <= 16 ) { \
				NVB(pre,_inv_bitlen) += 8; \
				*NVB(pre,_cur)++ = (U8)(NVB(pre,_bits) >> (24 - NVB(pre,_inv_bitlen))) ; \
				if ( NVB(pre,_inv_bitlen) <= 16 ) { \
					NVB(pre,_inv_bitlen) += 8; \
					*NVB(pre,_cur)++ = (U8)(NVB(pre,_bits) >> (24 - NVB(pre,_inv_bitlen))) ; \
				} \
			} \
		} \
	} \
	} while(0)

#define rrVarBits_OutputBack(pre)  do { \
	if ( NVB(pre,_inv_bitlen) <= 16 ) { \
		NVB(pre,_inv_bitlen) += 8; NVB(pre,_cur)--; \
		*NVB(pre,_cur) = (U8)(NVB(pre,_bits) >> (24 - NVB(pre,_inv_bitlen))) ; \
		if ( NVB(pre,_inv_bitlen) <= 16 ) { \
			NVB(pre,_inv_bitlen) += 8; NVB(pre,_cur)--; \
			*NVB(pre,_cur) = (U8)(NVB(pre,_bits) >> (24 - NVB(pre,_inv_bitlen))) ; \
			if ( NVB(pre,_inv_bitlen) <= 16 ) { \
				NVB(pre,_inv_bitlen) += 8; NVB(pre,_cur)--; \
				*NVB(pre,_cur) = (U8)(NVB(pre,_bits) >> (24 - NVB(pre,_inv_bitlen))) ; \
				if ( NVB(pre,_inv_bitlen) <= 16 ) { \
					NVB(pre,_inv_bitlen) += 8; NVB(pre,_cur)--; \
					*NVB(pre,_cur) = (U8)(NVB(pre,_bits) >> (24 - NVB(pre,_inv_bitlen))) ; \
				} \
			} \
		} \
	} \
	} while(0)
	
#define rrVarBits_OutputBytes	rrVarBits_Output
#define rrVarBits_OutputBytesBack	rrVarBits_OutputBack


#define rrVarBits_Refill_Unsafe(pre) do { \
		RR_ASSERT( rrVarBits_BitLen(pre) >= 0 && rrVarBits_BitLen(pre) < 32 ); \
		RR_BITLENTYPE bytes_consumed = (31 - rrVarBits_BitLen(pre)) >> 3; \
		U32 next = RR_VB_Get32_Unsafe(NVB(pre,_cur),NVB(pre,_end)); \
		NVB(pre,_bits) |= next >> rrVarBits_BitLen(pre); \
		NVB(pre,_cur) += bytes_consumed; \
		NVB(pre,_inv_bitlen) -= bytes_consumed<<3; \
		RRVB_PREFETCH( NVB(pre,_cur) ); \
		RRVB_MUNGE_BITS(pre); } while(0)	

#define rrVarBits_RefillBack_Unsafe(pre) do { \
		RR_ASSERT( rrVarBits_BitLen(pre) >= 0 && rrVarBits_BitLen(pre) < 32 ); \
		RR_BITLENTYPE bytes_consumed = (31 - rrVarBits_BitLen(pre)) >> 3; \
		U32 next = RR_GET32_LE(NVB(pre,_cur)-4); \
		NVB(pre,_bits) |= next >> rrVarBits_BitLen(pre); \
		NVB(pre,_cur) -= bytes_consumed; \
		NVB(pre,_inv_bitlen) -= bytes_consumed<<3; \
		RRVB_PREFETCHBACK( NVB(pre,_cur) ); \
		RRVB_MUNGE_BITS(pre); } while(0)	
		
// checks on _inv_bitlen should really be >=0
//	but then I might fill all 32 , which is not allowed cuz of Huff decode method
//	so check > 0 , that way I only fill 31
#define rrVarBits_Refill_Safe(pre)	do {												\
	if ( NVB(pre,_inv_bitlen) > 0 )													\
	{																				\
		RRVB_PREFETCH( NVB(pre,_cur) );												\
		NVB(pre,_bits) |= (U32)RR_VB_Get8_Safe(NVB(pre,_cur),NVB(pre,_end)) << NVB(pre,_inv_bitlen);		\
		NVB(pre,_cur) ++;															\
		NVB(pre,_inv_bitlen) -= 8;													\
																					\
		while ( NVB(pre,_inv_bitlen) > 0 )											\
		{																			\
			NVB(pre,_bits) |= (U32)RR_VB_Get8_Safe(NVB(pre,_cur),NVB(pre,_end)) << NVB(pre,_inv_bitlen);	\
			NVB(pre,_cur) ++;														\
			NVB(pre,_inv_bitlen) -= 8;												\
		}																			\
		RRVB_MUNGE_BITS(pre);														\
	}																				\
} while(0)

#define rrVarBits_Refill_Safe_Align	rrVarBits_Refill_Safe

#endif // RR_VB_64


//===========================================================================================

// EndPtr is the pointer past the last varbits
//	eg. EndPtr - init is the byte lenght of varbit data
// for example you can do a GetAlign8 by doing GetOpen( rrVarBits_GetEndPtr() )
// note - you do not need to call PutFlush8 before using these, it will tell you the size as if you had done a flush
//	however you must flush when you finish the stream of course

#define rrVarBits_PutEndPtr(vb)	( NVB(vb,_cur) + (rrVarBits_BitLen(vb) + 7)/8 )
#define rrVarBits_GetEndPtr(vb)	( NVB(vb,_cur) - (rrVarBits_BitLen(vb)    )/8 )
#define rrVarBitsBack_GetEndPtr(vb)	( NVB(vb,_cur) + (rrVarBits_BitLen(vb)    )/8 )

// SizeBytes would be the # of bytes we write after flushing
#define rrVarBits_PutSizeBytes(vb,basePtr)	rrPtrDiff( rrVarBits_PutEndPtr( vb ) - (basePtr) )
#define rrVarBits_GetSizeBytes(vb,basePtr)	rrPtrDiff( rrVarBits_GetEndPtr( vb ) - (basePtr) )

// SizeBits : useful for stat counting
//	this is <= 8*SizeBytes
#define rrVarBits_PutSizeBits(vb,basePtr)	( (S64)( NVB(vb,_cur) - (basePtr) )*8 + rrVarBits_BitLen(vb) )
#define rrVarBits_GetSizeBits(vb,basePtr)	( (S64)( NVB(vb,_cur) - (basePtr) )*8 - rrVarBits_BitLen(vb) )
#define rrVarBitsBack_PutSizeBits(vb,endPtr)	( (S64)( (endPtr) - NVB(vb,_cur) )*8 + rrVarBits_BitLen(vb) )
#define rrVarBitsBack_GetSizeBits(vb,endPtr)	( (S64)( (endPtr) - NVB(vb,_cur) )*8 - rrVarBits_BitLen(vb) )

//===================================================================

#ifndef RR_VARBITS_INLINES

#define rrVarBits_Get_C	macro_rrVarBits_Get_C
#define rrVarBits_Get_V	macro_rrVarBits_Get_V
#define rrVarBits_Get_0Ok	macro_rrVarBits_Get_0Ok
#define rrVarBits_Put	macro_rrVarBits_Put
#define rrVarBits_Use	macro_rrVarBits_Use
#define rrVarBits_Peek	macro_rrVarBits_Peek

#else // RR_VARBITS_INLINES

// OOINLINE wrappers of macros so you get arg type checking and such :

RADINLINE RR_VARBITSTYPE inl_rrVarBits_Get(rrVarBits_FuncArgs(vb),RR_BITLENTYPE len)
{
	// changed 10-27-15 : no longer allowed to get 0 !
	RR_ASSERT( len > 0 && len <= rrVarBits_BitLen(vb) );
    rrVarBits_Temps();
    return macro_rrVarBits_Get_V(vb,len);      
}

RADINLINE RR_VARBITSTYPE inl_rrVarBits_Get_0Ok(rrVarBits_FuncArgs(vb),RR_BITLENTYPE len)
{
	RR_ASSERT( len >= 0 && len <= rrVarBits_BitLen(vb) );
    rrVarBits_Temps();
    return macro_rrVarBits_Get_0Ok(vb,len);      
}

#undef rrVarBits_Get_V
#undef rrVarBits_Get_0Ok
#undef rrVarBits_Get_C
#define rrVarBits_Get_V(vb,len) inl_rrVarBits_Get(rrVarBits_PassArgs(vb),len)
#define rrVarBits_Get_0Ok(vb,len) inl_rrVarBits_Get_0Ok(rrVarBits_PassArgs(vb),len)
#define rrVarBits_Get_C(vb,len) inl_rrVarBits_Get(rrVarBits_PassArgs(vb),len)

RADINLINE RR_VARBITSTYPE inl_rrVarBits_Peek(rrVarBits_FuncArgs(vb),RR_BITLENTYPE len)
{ 
	RR_ASSERT( len > 0 && len <= rrVarBits_BitLen(vb) );
    return macro_rrVarBits_Peek(vb,len);
}

#undef rrVarBits_Peek
#define rrVarBits_Peek(vb,len) inl_rrVarBits_Peek(rrVarBits_PassArgs(vb),len)

RADINLINE RR_VARBITSTYPE inl_rrVarBits_Use(rrVarBits_FuncArgs(vb),RR_BITLENTYPE len)
{ 
	RR_ASSERT( len >= 0 && len <= rrVarBits_BitLen(vb) );
    return macro_rrVarBits_Use(vb,len);
}

#undef rrVarBits_Use
#define rrVarBits_Use(vb,len) inl_rrVarBits_Use(rrVarBits_PassArgs(vb),len)

// __v=(val)&c_rrBitMask[__s]
RADINLINE void inl_rrVarBits_Put(rrVarBits_FuncArgs(vb),RR_VARBITSTYPE val,RR_BITLENTYPE len)
{ 
	RR_ASSERT( len <= RR_MINBITSAVAILABLE );
    RR_ASSERT( val < ((RR_VARBITSTYPE)1<<len) );   
    RR_ASSERT( len <= rrVarBits_BitsWriteable(vb) );
    macro_rrVarBits_Put(vb,val,len);
}

#undef rrVarBits_Put
#define rrVarBits_Put(vb,val,size) inl_rrVarBits_Put(rrVarBits_PassArgs(vb),val,size)

#endif // RR_VARBITS_INLINES

//===========================================================================================
// rrVarBits_CountLeadingZeros
// get the number of leading 0's in the varbits
// useful for decoding unary/exp-golomb etc.
// NOTE this now does NOT handle the bits==0 case
//	you need a separate check for bits==0 , and when that happens probably also check corruption

// rrVB_CountLZ is the size of the varbits bits register
#ifdef RR_VB_64
#define rrVB_CountLZ rrClz64
#else
#define rrVB_CountLZ rrClz32
#endif

// rrVB_CheckNotZero ; assert that bits is not zero then pass it through
#ifdef RR_DO_ASSERTS
RADINLINE RR_VARBITSTYPE rrVB_CheckNotZero(RR_VARBITSTYPE bits)
{
	RR_ASSERT( bits != 0 );
	return bits;
}
#else
#define rrVB_CheckNotZero(bits)	bits
#endif

#define rrVarBits_CountLeadingZeros(vb)	rrVB_CountLZ( rrVB_CheckNotZero( rrVarBits_Bits(vb) ) )

//===========================================================================================

/*
// can do a GetInto without the Temp
#ifndef rrVarBits_GetInto
#define rrVarBits_GetInto(into,pre,count)	(into) = macro_rrVarBits_Get_V(pre,count)
#endif
*/

//===========================================================================================

OODLE_NS_END

#endif // __RADRR_VARBITSBH__
