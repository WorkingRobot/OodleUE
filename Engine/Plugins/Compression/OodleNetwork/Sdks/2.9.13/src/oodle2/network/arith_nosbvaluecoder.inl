// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

OODLE_NS_START

/**

for LZP - 

I tried doing firstrawbit per nosb
(eg. by sending 2*topbit|firstrawbit)

without :
LZP models take : 98440 bytes per channel, 12582944 bytes shared
LZP [8|19] : 222.1 -> 139.2 average = 1.596:1 = 37.33% reduction
29029 packets; 6448016 -> 4041030 total bytes = 62.67% of original

with :
LZP models take : 130184 bytes per channel
LZP [8|19] : 222.1 -> 139.1 average = 1.597:1 = 37.38% reduction

-> microscopic compression win, lots of memory use -> not worth it

**/

template <int t_context_bits, int t_num_unary>
struct O1ACNOSBValueCoder
{
	O1BinaryArithCoder<t_context_bits,1>	m_o1ac_unary[t_num_unary];
	O1BinaryArithCoder<t_context_bits,4>	m_o1ac_nosb;
	O1BinaryArithCoder<t_context_bits,1>	m_o1ac_firstbit;

	void init()
	{
		for LOOP(nu,t_num_unary)
			m_o1ac_unary[nu].init();
		m_o1ac_nosb.init();
		m_o1ac_firstbit.init();
	}

	void encode(rrArithEncoder * ac,int val,int context)
	{
		RR_ASSERT( val >= 0 );

		if ( val < t_num_unary )
		{
			for(int i = 0;i<val;i++)
			{
				m_o1ac_unary[i].encode(ac,0,context);
			}
			m_o1ac_unary[val].encode(ac,1,context);
			return;
		}
		
		for(int i = 0;i<t_num_unary;i++)
		{
			m_o1ac_unary[i].encode(ac,0,context);
		}

		RR_ASSERT( val >= t_num_unary );
		// make it a 1->inf thing
		val = val + 1 - t_num_unary;
		RR_ASSERT( val >= 1 );
		
		int topbit = rrGetBitLevel_V_NonZero(val) - 1;
		RR_ASSERT( val >= (1<<topbit) && val < (2<<topbit) );
		
		// write topbit :
		if ( topbit == 0 )
		{
			m_o1ac_nosb.encode(ac,0,context);	
			return;
		}
		else if ( topbit == 1 )
		{
			m_o1ac_nosb.encode(ac,1,context);	
			m_o1ac_firstbit.encode(ac,val&1,context);
			return;
		}
		
		int firstrawbitmask = 1<<(topbit-1);
		int firstrawbit = (val&firstrawbitmask) ? 1:0;
		
		if ( topbit < 15 )
		{
			m_o1ac_nosb.encode(ac,topbit,context);	
		}
		else
		{
			m_o1ac_nosb.encode(ac,15,context);	
			rrArithPutBits(ac,topbit-15,4);
		}
		
		m_o1ac_firstbit.encode(ac,firstrawbit,context);
		
		// bottom bits just raw :
		val &= firstrawbitmask-1;
		rrArithPutBits(ac,val,topbit-1);
	}
	
	int decode(rrArithDecoder * ac,int context)
	{
		for(int nu = 0;nu<t_num_unary;nu++)
		{
			if ( m_o1ac_unary[nu].decode(ac,context) )
			{
				return nu;
			}
		}
		
		int topbit = m_o1ac_nosb.decode(ac,context);
		
		if ( topbit == 0 )
		{
			return t_num_unary;
		}
		else if ( topbit == 1 )
		{
			int firstrawbit = m_o1ac_firstbit.decode(ac,context);
			return t_num_unary + 1 + firstrawbit;
		}
		else if ( topbit == 15 )
		{
			topbit += rrArithGetBits(ac,4);
		}
		
		int firstrawbit = m_o1ac_firstbit.decode(ac,context);

		int val = (2 | firstrawbit)<<(topbit-1);
		val += rrArithGetBits(ac,topbit-1);
		
		return val-1 + t_num_unary;
	}
};


template <int t_context_bits, int t_num_unary>
struct O1ACNOSBValueCountingNotReallyACoder
{
	O1BinaryCountingNotReallyACoder<t_context_bits,1>	m_o1ac_unary[t_num_unary];
	O1BinaryCountingNotReallyACoder<t_context_bits,4>	m_o1ac_nosb1;
	O1CountingNotReallyACoder<t_context_bits,16>	m_o1ac_nosb2;
	O1BinaryCountingNotReallyACoder<t_context_bits,1>	m_o1ac_firstbit;

	void init()
	{
		for LOOP(nu,t_num_unary)
			m_o1ac_unary[nu].init();
		m_o1ac_nosb1.init();
		m_o1ac_nosb2.init();
		m_o1ac_firstbit.init();
	}

	void encode(rrArithEncoder * ac,int val,int context)
	{
		RR_ASSERT( val >= 0 );

		if ( val < t_num_unary )
		{
			for(int i = 0;i<val;i++)
			{
				m_o1ac_unary[i].encode(ac,0,context);
			}
			m_o1ac_unary[val].encode(ac,1,context);
			return;
		}
		
		for(int i = 0;i<t_num_unary;i++)
		{
			m_o1ac_unary[i].encode(ac,0,context);
		}

		RR_ASSERT( val >= t_num_unary );
		// make it a 1->inf thing
		val = val + 1 - t_num_unary;
		RR_ASSERT( val >= 1 );
		
		int topbit = rrGetBitLevel_V_NonZero(val) - 1;
		RR_ASSERT( val >= (1<<topbit) && val < (2<<topbit) );
		
		// write topbit :
		if ( topbit == 0 )
		{
			m_o1ac_nosb1.encode(ac,0,context);	
			m_o1ac_nosb2.encode(ac,0,context);	
			return;
		}
		else if ( topbit == 1 )
		{
			m_o1ac_nosb1.encode(ac,1,context);	
			m_o1ac_nosb2.encode(ac,1,context);	
			m_o1ac_firstbit.encode(ac,val&1,context);
			return;
		}
		
		int firstrawbitmask = 1<<(topbit-1);
		int firstrawbit = (val&firstrawbitmask) ? 1:0;
		
		if ( topbit < 15 )
		{
			m_o1ac_nosb1.encode(ac,topbit,context);	
			m_o1ac_nosb2.encode(ac,topbit,context);	
		}
		else
		{
			m_o1ac_nosb1.encode(ac,15,context);	
			m_o1ac_nosb2.encode(ac,15,context);	
			//rrArithPutBits(ac,topbit-15,4);
		}
		
		m_o1ac_firstbit.encode(ac,firstrawbit,context);
		
		// bottom bits just raw :
		//val &= firstrawbitmask-1;
		//rrArithPutBits(ac,val,topbit-1);
	}
};

//=====================================
/// WARNING : BIG COPY PASTE FOR CONST VERSION

template <int t_context_bits, int t_num_unary>
struct O1ACNOSBValueCoder_Const
{
	O1BinaryArithcoder_Const<t_context_bits,1>	m_o1ac_unary[t_num_unary];
	//O1BinaryArithcoder_Const<t_context_bits,4>	m_o1ac_nosb;
	O1O0ArithCoder_Const<t_context_bits,O0ArithCoder_Const<U16,16,14> >	m_o1ac_nosb;
	O1BinaryArithcoder_Const<t_context_bits,1>	m_o1ac_firstbit;

	void init()
	{
		for LOOP(nu,t_num_unary)
			m_o1ac_unary[nu].init();
		m_o1ac_nosb.init();
		m_o1ac_firstbit.init();
	}

	void encode(rrArithEncoder * ac,int val,int context)
	{
		RR_ASSERT( val >= 0 );

		if ( val < t_num_unary )
		{
			for(int i = 0;i<val;i++)
			{
				m_o1ac_unary[i].encode(ac,0,context);
			}
			m_o1ac_unary[val].encode(ac,1,context);
			return;
		}
		
		for(int i = 0;i<t_num_unary;i++)
		{
			m_o1ac_unary[i].encode(ac,0,context);
		}

		RR_ASSERT( val >= t_num_unary );
		// make it a 1->inf thing
		val = val + 1 - t_num_unary;
		RR_ASSERT( val >= 1 );
		
		int topbit = rrGetBitLevel_V_NonZero(val) - 1;
		RR_ASSERT( val >= (1<<topbit) && val < (2<<topbit) );
		
		// write topbit :
		if ( topbit == 0 )
		{
			m_o1ac_nosb.encode(ac,0,context);	
			return;
		}
		else if ( topbit == 1 )
		{
			m_o1ac_nosb.encode(ac,1,context);	
			m_o1ac_firstbit.encode(ac,val&1,context);
			return;
		}
		
		int firstrawbitmask = 1<<(topbit-1);
		int firstrawbit = (val&firstrawbitmask) ? 1:0;
		
		if ( topbit < 15 )
		{
			m_o1ac_nosb.encode(ac,topbit,context);	
		}
		else
		{
			m_o1ac_nosb.encode(ac,15,context);	
			rrArithPutBits(ac,topbit-15,4);
		}
		
		m_o1ac_firstbit.encode(ac,firstrawbit,context);
		
		// bottom bits just raw :
		val &= firstrawbitmask-1;
		rrArithPutBits(ac,val,topbit-1);
	}
	
	int decode(rrArithDecoder * ac,int context)
	{
		for(int nu = 0;nu<t_num_unary;nu++)
		{
			if ( m_o1ac_unary[nu].decode(ac,context) )
			{
				return nu;
			}
		}
		
		int topbit = m_o1ac_nosb.decode(ac,context);
		
		if ( topbit == 0 )
		{
			return t_num_unary;
		}
		else if ( topbit == 1 )
		{
			int firstrawbit = m_o1ac_firstbit.decode(ac,context);
			return t_num_unary + 1 + firstrawbit;
		}
		else if ( topbit == 15 )
		{
			topbit += rrArithGetBits(ac,4);
		}
		
		int firstrawbit = m_o1ac_firstbit.decode(ac,context);

		int val = (2 | firstrawbit)<<(topbit-1);
		val += rrArithGetBits(ac,topbit-1);
		
		return val-1 + t_num_unary;
	}
};


template <int t_context_bits>
struct O1ACNOSBValueCoder<t_context_bits,0>
{
	O1BinaryArithCoder<t_context_bits,4>	m_o1ac_nosb;
	O1BinaryArithCoder<t_context_bits,1>	m_o1ac_firstbit;

	void init()
	{
		m_o1ac_nosb.init();
		m_o1ac_firstbit.init();
	}

	void encode(rrArithEncoder * ac,int val,int context)
	{
		RR_ASSERT( val >= 0 );

		// make it a 1->inf thing
		val = val + 1;
		RR_ASSERT( val >= 1 );
		
		int topbit = rrGetBitLevel_V_NonZero(val) - 1;
		RR_ASSERT( val >= (1<<topbit) && val < (2<<topbit) );
		
		// write topbit :
		if ( topbit == 0 )
		{
			m_o1ac_nosb.encode(ac,0,context);	
			return;
		}
		else if ( topbit == 1 )
		{
			m_o1ac_nosb.encode(ac,1,context);	
			m_o1ac_firstbit.encode(ac,val&1,context);
			return;
		}
		
		int firstrawbitmask = 1<<(topbit-1);
		int firstrawbit = (val&firstrawbitmask) ? 1:0;
		
		if ( topbit < 15 )
		{
			m_o1ac_nosb.encode(ac,topbit,context);	
		}
		else
		{
			m_o1ac_nosb.encode(ac,15,context);	
			rrArithPutBits(ac,topbit-15,4);
		}
		
		m_o1ac_firstbit.encode(ac,firstrawbit,context);
		
		// bottom bits just raw :
		val &= firstrawbitmask-1;
		rrArithPutBits(ac,val,topbit-1);
	}
	
	int decode(rrArithDecoder * ac,int context)
	{
		int topbit = m_o1ac_nosb.decode(ac,context);
		
		if ( topbit == 0 )
		{
			return 0;
		}
		else if ( topbit == 1 )
		{
			int firstrawbit = m_o1ac_firstbit.decode(ac,context);
			return 1 + firstrawbit;
		}
		else if ( topbit == 15 )
		{
			topbit += rrArithGetBits(ac,4);
		}
		
		int firstrawbit = m_o1ac_firstbit.decode(ac,context);

		int val = (2 | firstrawbit)<<(topbit-1);
		val += rrArithGetBits(ac,topbit-1);
		
		return val-1;
	}
};

OODLE_NS_END

