// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_TANSH__
#define __RADRR_TANSH__

#include "oodlecore.h"

OODLE_NS_START

/*

rrTANS

*/

/**

rrTANS
Encoder & Decoder can be allocated by doing your own malloc/free of the right _Size()

**/

struct rrTANS_Encoder;
struct rrTANS_Decoder;

SINTa rrTANS_Encoder_Size( S32 L_bits, S32 max_alphabet );
SINTa rrTANS_Decoder_Size( S32 L_bits, S32 max_alphabet );

/**

Call _Init after allocating to set up the internal pointers

you may _Init the same memory as many times as you want.
To change "L" or "max_alphabet" , just call _Init again

**/

rrTANS_Encoder * rrTANS_Encoder_Init( void * memory, SINTa memSize, S32 L_bits, S32 max_alphabet );
rrTANS_Decoder * rrTANS_Decoder_Init( void * memory, SINTa memSize, S32 L_bits, S32 max_alphabet );

/**

normalized counts must sum to L

_Fill builds the encode/tables

**/

void rrTANS_Encoder_Fill( rrTANS_Encoder * tables, const U32 * normalized_counts, int alphabet );
void rrTANS_Decoder_Fill( rrTANS_Decoder * tables, const U32 * normalized_counts, int alphabet );


typedef struct _rrVarBits_ rrVarBits;

void rrTANS_PackCounts(rrVarBits * vb,
						S32 L_bits, S32 max_alphabet,
						const U32 * counts, S32 alphabet,
						S32 num_non_zero,
						bool normalized);

rrbool rrTANS_UnPackCounts(rrVarBits * vb,
						S32 L_bits, S32 max_alphabet,
						U32 * counts,
						bool normalized);
						
OODLE_NS_END

#endif // __RADRR_TANSH__
