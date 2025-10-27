// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

OODLE_NS_START

template <int t_context_bits, int t_symbol_bits>
struct O1BinaryArithCoder
{
	enum { context_flag = 1<<t_context_bits };
	enum { bin_count = (1<<(t_context_bits+t_symbol_bits)) - context_flag };
	// waste some space for simpler addressing ?
	// doesn't seem to help
	//enum { bin_count = (1<<(t_context_bits+t_symbol_bits)) };
	rrArithBinaryModel bin[bin_count];
	
	void init()
	{
		for LOOP(i,bin_count)
			bin[i] = RR_BINARY_MODEL_INIT_VAL;
		//rrMemSet32_Aligned(bin,RR_BINARY_MODEL_INIT_VAL,bin_count*sizeofrrArithBinaryModel_Check);
	}
	
	O1BinaryArithCoder() { init(); }
	~O1BinaryArithCoder() { }
	
	void encode(rrArithEncoder * ac,int val,int context)
	{
		RR_ASSERT( context < context_flag );
		RR_ASSERT( val >= 0 && val < (1<<t_symbol_bits) );
		
		int ctx = context | context_flag;
		
		for(int i=t_symbol_bits-1;i>=0;i--)
		{
			int bit = (val>>i) & 1;
			rrArithBinaryModelEncode(ac,bit,&( bin[ctx-context_flag] ));
			ctx = (ctx<<1) | bit;
		}
	}
	
	int decode(rrArithDecoder * ac,int context)
	{
		RR_ASSERT( context < context_flag );
		
		int val = 0;
		int ctx = context | context_flag;
		for(int i=t_symbol_bits-1;i>=0;i--)
		{
			int bit = rrArithBinaryModelDecode(ac,&( bin[ctx-context_flag] ));
			RR_ASSERT( bit == 0 || bit == 1 );
			val |= bit<<i;
			ctx = (ctx<<1) | bit;
		}
		return val;
	}
	
	void encode_noadapt(rrArithEncoder * ac,int val,int context) const
	{
		RR_ASSERT( context < context_flag );
		RR_ASSERT( val >= 0 && val < (1<<t_symbol_bits) );
		
		int ctx = context | context_flag;
		
		for(int i=t_symbol_bits-1;i>=0;i--)
		{
			int bit = (val>>i) & 1;
			rrArithBinaryModelEncodeNoAdapt(ac,bit,bin[ctx-context_flag]);
			ctx = (ctx<<1) | bit;
		}
	}
	
	int decode_noadapt(rrArithDecoder * ac,int context) const 
	{
		RR_ASSERT( context < context_flag );
		
		int val = 0;
		int ctx = context | context_flag;
		for(int i=t_symbol_bits-1;i>=0;i--)
		{
			int bit = rrArithBinaryModelDecodeNoAdapt(ac,bin[ctx-context_flag]);
			RR_ASSERT( bit == 0 || bit == 1 );
			val |= bit<<i;
			ctx = (ctx<<1) | bit;
		}
		return val;
	}
	
	#if 0
	//*
	int shift8(int context,int val)
	{
		context <<= 8;
		context |= val;
		context &= (context_flag-1);
		return context;
	}
	/*/	
	// I'd rather shift context *down* to keep the top bits
	//  hmm , worse
	int shift8(int context,int val)
	{
		RR_COMPILER_ASSERT( t_context_bits >= 8 );
		RR_ASSERT( val < 256 );
		context >>= 8;
		context |= (val << (t_context_bits-8));
		return context;
	}
	/**/
	
	void encode_array(rrArithEncoder * ac,const U8 * ptr,int size,int context)
	{
		RR_ASSERT( t_symbol_bits == 8 );
		for(int i=0;i<size;i++)
		{
			int val = ptr[i];
			encode(ac,val,context);
			context = shift8(context,val);
		}
	}
	
	void decode_array(rrArithDecoder * ac,U8 * ptr,int size,int context)
	{
		RR_ASSERT( t_symbol_bits == 8 );
		for(int i=0;i<size;i++)
		{
			int val = decode(ac,context);
			ptr[i] = (U8)val;
			context = shift8(context,val);
		}
	}
	#endif
};


template <int t_context_bits>
struct O1BinaryArithCoder<t_context_bits,1>
{
	enum { context_count = 1<<t_context_bits };
	rrArithBinaryModel bin[context_count];
	
	void init()
	{
		for LOOP(i,context_count)
			bin[i] = RR_BINARY_MODEL_INIT_VAL;
		//rrMemSet32_Aligned(bin,RR_BINARY_MODEL_INIT_VAL,bin_count*sizeofrrArithBinaryModel_Check);
	}
	
	O1BinaryArithCoder() { init(); }
	~O1BinaryArithCoder() { }
	
	void encode(rrArithEncoder * ac,int val,int context)
	{
		RR_ASSERT( context < context_count );
		RR_ASSERT( val == 0 || val == 1 );
		
		rrArithBinaryModelEncode(ac,val,&( bin[context] ));
	}
	
	int decode(rrArithDecoder * ac,int context)
	{
		RR_ASSERT( context < context_count );
		
		int val = rrArithBinaryModelDecode(ac,&( bin[context] ));
		RR_ASSERT( val == 0 || val == 1 );
		return val;
	}
	
	void encode_noadapt(rrArithEncoder * ac,int val,int context) const
	{
		RR_ASSERT( context < context_count );
		RR_ASSERT( val == 0 || val == 1 );
		
		rrArithBinaryModelEncodeNoAdapt(ac,val,bin[context]);
	}
	
	int decode_noadapt(rrArithDecoder * ac,int context) const 
	{
		RR_ASSERT( context < context_count );
		
		int val = rrArithBinaryModelDecodeNoAdapt(ac,bin[context]);
		return val;
	}
};	


template <int t_context_bits, int t_symbol_bits>
struct O1BinaryArithcoder_Const : public O1BinaryArithCoder<t_context_bits,t_symbol_bits>
{	
	typedef O1BinaryArithCoder<t_context_bits,t_symbol_bits> parent_type;

	O1BinaryArithcoder_Const() { }
	~O1BinaryArithcoder_Const() { }
	
	void encode(rrArithEncoder * ac,int val,int context) const
	{
		parent_type::encode_noadapt(ac,val,context);
	}
	
	int decode(rrArithDecoder * ac,int context) const
	{
		return parent_type::decode_noadapt(ac,context);
	}
};
	
	
// can just do an array of O0ArithCoder

template <int t_context_bits,typename t_count,int t_alphabet>
struct O1O0ArithCoder
{
	enum { context_count = (1<<(t_context_bits)) };
	
	O0ArithCoder<t_count,t_alphabet> m_o0s[context_count];
	
	O1O0ArithCoder() { init(); }
	
	void init()
	{
		for LOOP(i,context_count)
			m_o0s[i].init();
	}
	
	void encode(rrArithEncoder * ac,int val,int context)
	{
		RR_ASSERT( context < context_count );
		RR_ASSERT( val >= 0 && val < t_alphabet );

		m_o0s[context].encode(ac,val);
	}
	
	void encode_withexclude(rrArithEncoder * ac,int val,int context, int excl)
	{
		RR_ASSERT( context < context_count );
		RR_ASSERT( val >= 0 && val < t_alphabet );
		RR_ASSERT( val != excl );
		
		m_o0s[context].encode_withexclude(ac,val,excl);
	}
	
	int decode(rrArithDecoder * ac,int context)
	{
		RR_ASSERT( context < context_count );
		
		int val = m_o0s[context].decode(ac);
		
		return val;
	}
	
	int decode_withexclude(rrArithDecoder * ac,int context,int excl)
	{
		RR_ASSERT( context < context_count );
		
		int val = m_o0s[context].decode_withexclude(ac,excl);
		
		return val;
	}
};

template <int t_context_bits,typename t_o0type>
struct O1O0ArithCoder_Const
{
	enum { context_count = (1<<(t_context_bits)) };
	
	t_o0type m_o1[context_count];
	
	O1O0ArithCoder_Const() { init(); }
	
	void init()
	{
		for LOOP(i,context_count)
			m_o1[i].init();
	}
		
	void encode(rrArithEncoder * ac,int val,int context) const
	{
		RR_ASSERT( context < context_count );

		m_o1[context].encode_noadapt(ac,val);
	}
	
	void encode_withexclude(rrArithEncoder * ac,int val,int context, int excl) const
	{
		RR_ASSERT( context < context_count );
		RR_ASSERT( val != excl );
		
		m_o1[context].encode_noadapt_with_exclude(ac,val,excl);
	}
	
	int decode(rrArithDecoder * ac,int context) const
	{
		RR_ASSERT( context < context_count );
		
		int val = m_o1[context].decode_noadapt(ac);
		
		return val;
	}
	
	int decode_withexclude(rrArithDecoder * ac,int context,int excl) const
	{
		RR_ASSERT( context < context_count );
		
		int val = m_o1[context].decode_noadapt_with_exclude(ac,excl);
		
		return val;
	}
};



template <int t_context_bits, int t_symbol_bits>
struct O1BinaryCountingNotReallyACoder
{
	enum { context_flag = 1<<t_context_bits };
	enum { bin_count = (1<<(t_context_bits+t_symbol_bits)) - context_flag };

	struct BinaryCounts
	{
		U32 n0,n1;
		
		void init() { n0 = n1 = 1; }
		
		void inc(int bit)
		{
			if ( bit ) n1++;
			else n0 ++;
		}
		
		rrArithBinaryModel get_p0() const
		{
			U32 p0 = ((U64)RR_BINARY_MODEL_TOT * n0 + 1) / (n0 + n1);
			p0 = RR_CLAMP(p0,4,RR_BINARY_MODEL_TOT-4);
			return (rrArithBinaryModel) p0;
		}
	};

	BinaryCounts bin[bin_count];
	
	void init()
	{
		for LOOP(i,bin_count)
			bin[i].init();
	}
	
	O1BinaryCountingNotReallyACoder() { init(); }
	~O1BinaryCountingNotReallyACoder() { }
	
	void encode(rrArithEncoder * ac,int val,int context)
	{
		RR_ASSERT( context < context_flag );
		RR_ASSERT( val >= 0 && val < (1<<t_symbol_bits) );
		
		int ctx = context | context_flag;
		
		for(int i=t_symbol_bits-1;i>=0;i--)
		{
			int bit = (val>>i) & 1;
			bin[ctx-context_flag].inc(bit);
			ctx = (ctx<<1) | bit;
		}
	}	
};



template <int t_context_bits,int t_alphabet>
struct O1CountingNotReallyACoder
{
	enum { context_count = (1<<(t_context_bits)) };

	//struct Histo { U16 count[t_alphabet]; };
	struct Histo { U32 count[t_alphabet]; };
	Histo m_o1[context_count];
	Histo m_o0;
	
	void init()
	{
		//rrMemSetZero(m_o1,context_count*sizeof(Histo));
		//RR_ZERO(m_o0);
		for(int c=0;c<context_count;c++)
		{
			for(int v=0;v<t_alphabet;v++)
			{
				m_o1[c].count[v] = 1;
			}
		}
		for(int v=0;v<t_alphabet;v++)
		{
			m_o0.count[v] = 1;
		}
	}
	
	O1CountingNotReallyACoder() { init(); }
	~O1CountingNotReallyACoder() { }
	
	void encode(rrArithEncoder * ac,int val,int context)
	{
		RR_ASSERT( context < context_count );
		RR_ASSERT( val >= 0 && val < t_alphabet );

		m_o1[context].count[val] ++;
		m_o0.count[val] ++;
		
		//rrArithPutBits(ac,val,8);
		// @@ could go ahead and encode something properly
		//	so that ac gets the right output size
	}
	
	void encode_withexclude(rrArithEncoder * ac,int val,int context, int excl)
	{
		RR_ASSERT( val != excl );
		encode(ac,val,context);
	}
};

OODLE_NS_END
