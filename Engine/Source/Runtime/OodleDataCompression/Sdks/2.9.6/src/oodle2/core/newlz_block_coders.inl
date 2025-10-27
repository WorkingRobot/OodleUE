// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.


OODLE_NS_START

// The corresponding decoder for newLZ_encode_alphabet_shape_num_EG.
// Reads at most 8 bits.
static RADINLINE int newLZ_decode_alphabet_shape_num_EG(int num_syms, rrVarBits_FuncArgs(vb))
{
	RR_ASSERT(0 < num_syms && num_syms <= 256);
	int num_eg = 0;

	if (num_syms != 256)
	{
		rrVarBits_Temps();
		int num_eg_bound = RR_MIN(num_syms, 257-num_syms)*2; // *strictly* larger than num_eg; >=2

		// this is just rrVarBits_ReadFlat
		U32 nbits = 32 - rrClz32(num_eg_bound - 1);
		int large_thresh = (1 << nbits) - num_eg_bound;
		int peek = (int)rrVarBits_Peek(vb, nbits);
		if ((peek>>1) < large_thresh)
		{
			num_eg = peek>>1;
			rrVarBits_Use(vb, nbits-1);
		}
		else
		{
			num_eg = peek - large_thresh;
			rrVarBits_Use(vb, nbits);
		}
	}

	return num_eg;
}

OODLE_NS_END
