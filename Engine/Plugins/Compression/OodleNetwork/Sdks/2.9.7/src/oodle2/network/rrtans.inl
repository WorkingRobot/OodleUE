// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef RRTANS_INL
#define RRTANS_INL

OODLE_NS_START

struct rrtans_encode_entry { U16 * packed_table_ptr; U16 max_state_thresh; U8 max_state_numbits; };

#ifdef _MSC_VER
#pragma pack(push)
#pragma pack(1)
#endif

RADSTRUCT rrtans_decode_entry_U8 { U8 sym; U8 num_bits; U16 next_state; };
RR_COMPILER_ASSERT( sizeof(rrtans_decode_entry_U8) == 4 );
	
#ifdef _MSC_VER
#pragma pack(pop)
#endif

struct tans_decode_entry_U16 { U16 sym; U16 num_bits; U16 next_state; };

//=================================================================================	
/**

rrVarBitsBack

backwards bit output
read with regular rrVarBits

rrVarBitsBack uses the same variables as rrVarBits
but they do NOT mean the same thing!
do NOT mix and match
except for struct init / arg passing / etc

_inv_bitlen here is actually the # of bits in state

**/

#ifdef RR_VB_64
#define RR_ROTR_VB	RR_ROTR64
#else
#define RR_ROTR_VB	RR_ROTR32
#endif

#define RR_VARBITSBACK_MINBITSAVAILABLE		(RR_VARBITSTYPELEN-8)

#define rrVarBitsBack_Reset(pre)			NVB(pre,_bits) = 0, NVB(pre,_inv_bitlen) = 0
#define rrVarBitsBack_PutOpen(pre,endptr)	rrVarBitsBack_Reset(pre), NVB(pre,_cur) = endptr, NVB(pre,_end) = endptr

#define rrVarBitsBack_NumBits(pre)	NVB(pre,_inv_bitlen)

#define rrVarBitsBack_Put(pre,val,nb) do { \
		RR_ASSERT( NVB(pre,_inv_bitlen) + rrGetBitLevelR_V(val) < RR_VARBITSTYPELEN ); \
		NVB(pre,_bits) >>= nb; \
		NVB(pre,_bits) |= RR_ROTR_VB((RR_VARBITSTYPE)(val),nb); \
		NVB(pre,_inv_bitlen) += nb; \
	} while(0)
		
#ifdef RR_VB_64

#define rrVarBitsBack_Output(pre) do { \
		U64 out = ( NVB(pre,_bits) >> (64 - NVB(pre,_inv_bitlen)) ); \
		RR_PUT64_BE_UNALIGNED( (NVB(pre,_cur) - 8) , out ); \
		NVB(pre,_cur) -= NVB(pre,_inv_bitlen)>>3; \
		NVB(pre,_inv_bitlen) &= 7; \
	} while(0)

#else

#define rrVarBitsBack_Output(pre) do { \
		U32 out = ( NVB(pre,_bits) >> (32 - NVB(pre,_inv_bitlen)) ); \
		RR_PUT32_BE_UNALIGNED( (NVB(pre,_cur) - 4) , out ); \
		NVB(pre,_cur) -= NVB(pre,_inv_bitlen)>>3; \
		NVB(pre,_inv_bitlen) &= 7; \
	} while(0)
	
#endif

// do NOT call Flush more than once; it is not a NOP	
#define rrVarBitsBack_FlushPartial(pre) do { \
		while ( NVB(pre,_inv_bitlen) > 8 ) { \
			NVB(pre,_cur)--; \
			*NVB(pre,_cur) = (U8) ( NVB(pre,_bits) >> (RR_VARBITSTYPELEN - NVB(pre,_inv_bitlen)) ); \
			NVB(pre,_inv_bitlen) -= 8; \
		} \
		if ( NVB(pre,_inv_bitlen) == 0 ) { \
			NVB(pre,_inv_bitlen) = 8; \
		} else { \
			RR_ASSERT( NVB(pre,_inv_bitlen) >= 1 && NVB(pre,_inv_bitlen) <= 8 ); \
			NVB(pre,_cur)--; \
			*NVB(pre,_cur) = (U8) (NVB(pre,_bits) >> (RR_VARBITSTYPELEN - 8)); \
		} \
		RR_ASSERT( NVB(pre,_inv_bitlen) >= 1 && NVB(pre,_inv_bitlen) <= 8 ); \
	} while(0)
// NVB(pre,_inv_bitlen) is the number of bits in *NVB(pre,_cur)
// the last bits are the top of the byte

// FlushPartialHeader writes the partial bitlen of the last byte
//	tries to fit it in the last byte if last bitlen is <= 5
#define rrVarBitsBack_FlushPartialHeader(pre) do { \
		rrVarBitsBack_FlushPartial(pre); \
		RR_ASSERT( NVB(pre,_inv_bitlen) >= 1 && NVB(pre,_inv_bitlen) <= 8 ); \
		if ( NVB(pre,_inv_bitlen) > 5 ) NVB(pre,_cur)--; \
		*(NVB(pre,_cur)) = (U8)( (*(NVB(pre,_cur)) & 0xF8) | ( NVB(pre,_inv_bitlen) - 1 ) ); \
		} while(0)

// use rrVarBits_GetOpen_PartialHeader instead of rrVarBits_GetOpen
//	reads header from rrVarBitsBack_FlushPartialHeader
#define rrVarBits_GetOpen_PartialHeader(pre,ptr,endPtr) do { \
	rrVarBits_Reset(pre); NVB(pre,_cur) = (U8 *) ptr;  NVB(pre,_end) = (U8 *) endPtr; \
	RR_VARBITSTYPE hdr = RR_VB_Get8_Unsafe(NVB(pre,_cur),NVB(pre,_end));  NVB(pre,_cur) ++; \
	NVB(pre,_inv_bitlen) -= (RR_BITLENTYPE)( (hdr & 7) + 1 ); \
	RR_ASSERT( rrVarBits_BitLen(pre) == (RR_BITLENTYPE)((hdr & 7) + 1) ); \
	if ( rrVarBits_BitLen(pre) <= 5 ) { \
		NVB(pre,_bits) = (hdr & 0xF8) << (RR_VARBITSTYPELEN - 8); \
	} else { \
		hdr = RR_VB_Get8_Unsafe(NVB(pre,_cur),NVB(pre,_end));  NVB(pre,_cur) ++; \
		NVB(pre,_bits) = hdr << (RR_VARBITSTYPELEN - 8); \
	} \
	rrVarBits_Refill_Safe_Align(pre); \
	} while(0)

//===================================================

// remember to encode BACKWARDS
// you need to output bits backwards too!
// encode does not include rrVarBits_Output

struct rrTANS_Encoder
{
	S32 L, L_bits;
	S32 alphabet, max_alphabet;

	U16 * encode_table_packed; // L of them
	
	rrtans_encode_entry * encode_sym_table; // alphabet of them

	// U16 normalized_counts; // keep or discard ?
};
		
#define RRTANS_ENCODE(eetable,state,vb,sym) \
	do { \
		const rrtans_encode_entry * ee = eetable+(sym); \
		RR_BITLENTYPE msnb = ee->max_state_numbits; \
		msnb += ( state >= ee->max_state_thresh ); \
		rrVarBitsBack_Put(vb, state ,msnb); \
		state = ee->packed_table_ptr[ state>>msnb ]; \
	} while(0)

#define RRTANS_ENCODE_VARS(eetable,state) \
	const rrtans_encode_entry * eetable = 0; \
	UINTr state = 0;

#define RRTANS_ENCODE_START(eetable,state,tansenc) \
	eetable = (tansenc)->encode_sym_table; \
	state = (tansenc)->L;

#define RRTANS_ENCODE_FINISH(state,vb,tansenc) \
	rrVarBitsBack_Put(vb, state , (tansenc)->L_bits+1 );
	
// encode start :
// state = L

// encode finish :
// putbits(state,L_bits+1);
	
//=================================================================================	

struct rrTANS_Decoder
{
	S32 L, L_bits;
	S32 alphabet, max_alphabet;

	// one or the other :
	rrtans_decode_entry_U8 * decode_table_U8;
	tans_decode_entry_U16 * decode_table_U16;
};

//=================================================================================	
	
#define RRTANS_DECODE_VARS_U8(detable,state) \
	const rrtans_decode_entry_U8 * detable = 0; UINTr state = 0;
	
#define RRTANS_DECODE_VARS_U16(detable,state) \
	const tans_decode_entry_U16 * detable = 0; UINTr state = 0;
		
#define RRTANS_DECODE_START_U8(detable,state,tansdec,vb) \
	detable = (tansdec)->decode_table_U8 - (tansdec)->L; \
	state = rrVarBits_Get_V(vb,(tansdec)->L_bits+1 );
	
#define RRTANS_DECODE_START_U16(detable,state,tansdec,vb) \
	detable = (tansdec)->decode_table_U16 - (tansdec)->L; \
	state = rrVarBits_Get_V(vb,(tansdec)->L_bits+1 );

// need a branch to protect num_bits == 0
// this could be detected when the table is built
//	it only occurs if the MPS symbol count is >= 50%

// protect VarBits_Get_V if it can't handle zero :
//	(or use rrVarBits_Get_0Ok)
#define RRTANS_IF_NB_ZERO(nb)  if ( (nb) != 0 ) 

#define RRTANS_DECODE_U8(detable,state,vb,result) do { \
		const rrtans_decode_entry_U8 * de = (detable) + state; \
		RR_BITLENTYPE nb = de->num_bits; \
		state = de->next_state; \
		RRTANS_IF_NB_ZERO(nb) state |= rrVarBits_Get_V(vb,nb); \
		(result) = de->sym; \
	} while(0)
	
#define RRTANS_DECODE_U16(detable,state,vb,result) do { \
		const tans_decode_entry_U16 * de = (detable) + state; \
		RR_BITLENTYPE nb = de->num_bits; \
		state = de->next_state; \
		RRTANS_IF_NB_ZERO(nb) state |= rrVarBits_Get_V(vb,nb); \
		(result) = de->sym; \
	} while(0)


// no decode finish

//=================================================================================	

OODLE_NS_END
	
#endif // RRTANS_INL

	