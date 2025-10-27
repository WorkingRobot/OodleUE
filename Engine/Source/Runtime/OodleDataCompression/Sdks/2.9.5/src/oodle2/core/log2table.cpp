// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "log2table.h"

OODLE_NS_START

// RR_LOG2TABLE_SHIFT == 13

// This 32-entry table gives accuracy to within 3 units in the last place
// in our 14-bit fixed point cost. That's good enough for our purposes.
// table[i] = round(kBitCost*log2(1.0 + i/tableSize))

//U32 g_log2tabled_bk_bias = 0;

/*
static const U16 c_log2table_bk[] = 
{
  0,364,716,1059,1392,1716,2031,2338,
  2637,2929,3214,3492,3764,4029,4289,4543,
  4792,5036,5274,5509,5738,5963,6184,6401,
  6614,6823,7029,7231,7429,7625,7817,8006,
  8192
};
*/

const U16 c_log2table_bk[] = 
{
  0,183,364,541,716,889,1059,1227,
  1392,1555,1716,1874,2031,2186,2338,2489,
  2637,2784,2929,3072,3214,3354,3492,3629,
  3764,3897,4029,4160,4289,4417,4543,4668,
  4792,4914,5036,5156,5274,5392,5509,5624,
  5738,5851,5963,6074,6184,6293,6401,6508,
  6614,6719,6823,6926,7029,7130,7231,7330,
  7429,7527,7625,7721,7817,7912,8006,8099,
  8192
};

RR_COMPILER_ASSERT( RR_ARRAY_SIZE(c_log2table_bk) == bk_tablesize+1 );

const S32 c_log2tabled_bk_32[32] = { 
	9999999, 262144, 253952, 249160, 245760, 243123, 240968, 239146, 237568, 236176, 234931, 233804, 232776, 231830, 230954, 230139, 229376, 228660, 227984, 227345, 226739, 226162, 225612, 225087, 224584, 224102, 223638, 223192, 222762, 222347, 221947, 221559
};

RR_COMPILER_ASSERT( RR_ARRAY_SIZE(c_log2tabled_bk_32) == LOG2TABLED_BK_32_TABLE_SIZE );

F32 rrlog2_bk_U64(U64 x)
{
	F32 ret = 0.f;
	
	if ( x > (U64)RR_U32_MAX )
	{
		int clz = rrClz64(x);
		RR_ASSERT( clz < 32 );
		int s = 32 - clz;
		x >>= s;
		ret = (F32) s;
	}
	
	return ret + rrlog2_bk((U32)x);
}

OODLE_NS_END
