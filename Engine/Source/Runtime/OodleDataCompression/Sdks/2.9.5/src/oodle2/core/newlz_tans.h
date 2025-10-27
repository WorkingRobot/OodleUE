// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_TANSH__
#define __RADRR_TANSH__

#include "oodlecore.h"

/*

newlz_tans

*/

OODLE_NS_START

/**

newlz_tans
Encoder & Decoder can be allocated by doing your own malloc/free of the right _Size()

**/

struct newlz_tans_Encoder;
struct newlz_tans_Decoder;

SINTa newlz_tans_Encoder_Size( S32 L_bits );
SINTa newlz_tans_Decoder_Size( S32 L_bits );

/**

Call _Init after allocating to set up the internal pointers

you may _Init the same memory as many times as you want.
To change "L" or "max_alphabet" , just call _Init again

**/

newlz_tans_Encoder * newlz_tans_Encoder_Init( void * memory, SINTa memSize, S32 L_bits );
newlz_tans_Decoder * newlz_tans_Decoder_Init( void * memory, SINTa memSize, S32 L_bits );

/**

normalized counts must sum to L

_Fill builds the encode/tables

**/

struct newlz_tans_UnpackedCounts
{
	int num_singles;	// number of symbols with 1 slot
	int num_larger;		// number of symbols with >1 slot

	U8 singles[256];	// values of symbols with 1 slot (ascending order)
	U32 larger[256];	// per symbol: (value<<16) | count (ascending order)
};

void newlz_tans_Encoder_Fill( newlz_tans_Encoder * tables, const U32 * normalized_counts, int alphabet );
void newlz_tans_Decoder_Fill( newlz_tans_Decoder * tables, const newlz_tans_UnpackedCounts * counts );

typedef struct _rrVarBits_ rrVarBits;

void newlz_tans_PackCounts(rrVarBits * vb,
						S32 L_bits,
						const U32 * counts, S32 alphabet,
						S32 num_non_zero);

bool newlz_tans_UnPackCounts(rrVarBits * vb,
						S32 L_bits,
						newlz_tans_UnpackedCounts * counts);
						
OODLE_NS_END

#endif // __RADRR_TANSH__
