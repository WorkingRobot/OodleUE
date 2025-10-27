// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "arith_o0.inl"
#include "arith_symbolranker.inl"

OODLE_NS_START

template <int t_srnumsyms,int t_contextbits,int t_alphabet,typename t_sym>
struct O1SRCoder
{
	enum { num_contexts = 1<<t_contextbits };
	
	SRCoder<t_srnumsyms,t_sym> m_o1[num_contexts];

	enum { o0_inc = 2 };
	O0ArithCoder<U16,t_alphabet> m_o0;

	O1SRCoder() { }
	
	void init()
	{
		for LOOP(c,num_contexts)
		{
			m_o1[c].init();
		}
		m_o0.init();
	}
	
	void encode(rrArithEncoder * ac,int sym,int context)
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		if ( m_o1[context].encode(ac,sym) )
			return;
			
		m_o0.encode_noadapt_with_excludelist(ac,sym,m_o1[context].m_syms,t_srnumsyms);

		m_o0.inc_count(sym,o0_inc);
		m_o1[context].update_escape(sym);
	}
	
	int decode(rrArithDecoder * ac,int context)
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		
		int sym = m_o1[context].decode(ac);
		
		if ( sym == -1 )
		{
			sym = m_o0.decode_noadapt_with_excludelist(ac,m_o1[context].m_syms,t_srnumsyms);
			RR_ASSERT( sym >= 0 && sym < t_alphabet );
			m_o0.inc_count(sym,o0_inc);	
			m_o1[context].update_escape(sym);
		}
		
		return sym;	
	}
	
	void encode_withexclude(rrArithEncoder * ac,int sym,int context,int exc)
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		if ( m_o1[context].encode_withexclude(ac,sym,&exc) )
			return;
		
		const t_sym * o1_excludes = m_o1[context].m_syms;
		
		if ( exc < 0 )
		{
			m_o0.encode_noadapt_with_excludelist(ac,sym,o1_excludes,t_srnumsyms);
		}
		else
		{
			t_sym excludes[t_srnumsyms+1];
			memcpy(excludes,o1_excludes,t_srnumsyms*sizeof(t_sym));
			excludes[t_srnumsyms] = check_value_cast<t_sym>(exc);
			
			m_o0.encode_noadapt_with_excludelist(ac,sym,excludes,t_srnumsyms+1);
		}
		
		m_o0.inc_count(sym,o0_inc);
		m_o1[context].update_escape(sym);
	}
	
	int decode_withexclude(rrArithDecoder * ac,int context,int exc)
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		
		int sym = m_o1[context].decode_withexclude(ac,&exc);
		
		if ( sym == -1 )
		{
			const t_sym * o1_excludes = m_o1[context].m_syms;
		
			if ( exc < 0 )
			{
				sym = m_o0.decode_noadapt_with_excludelist(ac,o1_excludes,t_srnumsyms);
			}
			else
			{
				t_sym excludes[t_srnumsyms+1];
				memcpy(excludes,o1_excludes,t_srnumsyms*sizeof(t_sym));
				excludes[t_srnumsyms] = check_value_cast<t_sym>(exc);
			
				sym = m_o0.decode_noadapt_with_excludelist(ac,excludes,t_srnumsyms+1);
			}
			
			RR_ASSERT( sym >= 0 && sym < t_alphabet );
			m_o0.inc_count(sym,o0_inc);	
			m_o1[context].update_escape(sym);
		}
		
		return sym;	
	}	
	
	//===========================================
	
	void encode_noadapt(rrArithEncoder * ac,int sym,int context) const
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		if ( m_o1[context].encode_noadapt(ac,sym) )
			return;
			
		m_o0.encode_noadapt_with_excludelist(ac,sym,m_o1[context].m_syms,t_srnumsyms);
	}
	
	int decode_noadapt(rrArithDecoder * ac,int context) const
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		
		int sym = m_o1[context].decode_noadapt(ac);
		
		if ( sym == -1 )
		{
			sym = m_o0.decode_noadapt_with_excludelist(ac,m_o1[context].m_syms,t_srnumsyms);
		}
		
		return sym;	
	}
	
	void encode_withexclude_noadapt(rrArithEncoder * ac,int sym,int context,int exc) const
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		if ( m_o1[context].encode_withexclude_noadapt(ac,sym,&exc) )
			return;
		
		const t_sym * o1_excludes = m_o1[context].m_syms;
		
		if ( exc < 0 )
		{
			m_o0.encode_noadapt_with_excludelist(ac,sym,o1_excludes,t_srnumsyms);
		}
		else
		{
			t_sym excludes[t_srnumsyms+1];
			memcpy(excludes,o1_excludes,t_srnumsyms*sizeof(t_sym));
			excludes[t_srnumsyms] = check_value_cast<t_sym>(exc);
			
			m_o0.encode_noadapt_with_excludelist(ac,sym,excludes,t_srnumsyms+1);
		}
	}
	
	int decode_withexclude_noadapt(rrArithDecoder * ac,int context,int exc) const
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		
		int sym = m_o1[context].decode_withexclude_noadapt(ac,&exc);
		
		if ( sym == -1 )
		{
			const t_sym * o1_excludes = m_o1[context].m_syms;
		
			if ( exc < 0 )
			{
				sym = m_o0.decode_noadapt_with_excludelist(ac,o1_excludes,t_srnumsyms);
			}
			else
			{
				t_sym excludes[t_srnumsyms+1];
				memcpy(excludes,m_o1[context].m_syms,t_srnumsyms*sizeof(t_sym));
				excludes[t_srnumsyms] = exc;
			
				sym = m_o0.decode_noadapt_with_excludelist(ac,excludes,t_srnumsyms+1);
			}
		}
		
		return sym;	
	}	
	
};


template <int t_srnumsyms,int t_contextbits,int t_alphabet,typename t_sym>
struct O1SRCoder_Const
{
	enum { num_contexts = 1<<t_contextbits };
	
	SRCoder_Const<t_srnumsyms,t_sym> m_o1[num_contexts];

	O0ArithCoder_Const<U16,t_alphabet,14> m_o0;

	O1SRCoder_Const() { }
	
	void init()
	{
		for LOOP(c,num_contexts)
		{
			m_o1[c].init();
		}
		m_o0.init();
	}
	
	void encode(rrArithEncoder * ac,int sym,int context) const
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		if ( m_o1[context].encode(ac,sym) )
			return;
			
		m_o0.encode_noadapt_with_excludelist(ac,sym,m_o1[context].m_syms,t_srnumsyms);
	}
	
	int decode(rrArithDecoder * ac,int context) const
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		
		int sym = m_o1[context].decode(ac);
		
		if ( sym >= 0 )
			return sym;

		sym = m_o0.decode_noadapt_with_excludelist(ac,m_o1[context].m_syms,t_srnumsyms);
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		return sym;	
	}
	
	void encode_withexclude(rrArithEncoder * ac,int sym,int context,int exc)
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		if ( m_o1[context].encode_withexclude(ac,sym,&exc) )
			return;
		
		const t_sym * o1_excludes = m_o1[context].m_syms;
		
		if ( exc < 0 )
		{
			m_o0.encode_noadapt_with_excludelist(ac,sym,o1_excludes,t_srnumsyms);
		}
		else
		{
			// @@ ugly building excludelist
			t_sym excludes[t_srnumsyms+1];
			memcpy(excludes,o1_excludes,t_srnumsyms*sizeof(t_sym));
			excludes[t_srnumsyms] = check_value_cast<t_sym>(exc);
			
			m_o0.encode_noadapt_with_excludelist(ac,sym,excludes,t_srnumsyms+1);
		}
	}
	
	int decode_withexclude(rrArithDecoder * ac,int context,int exc)
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		
		int sym = m_o1[context].decode_withexclude(ac,&exc);
		
		if ( sym >= 0 )
			return sym;

		const t_sym * o1_excludes = m_o1[context].m_syms;
	
		if ( exc < 0 )
		{
			return m_o0.decode_noadapt_with_excludelist(ac,o1_excludes,t_srnumsyms);
		}
		else
		{
			t_sym excludes[t_srnumsyms+1];
			memcpy(excludes,o1_excludes,t_srnumsyms*sizeof(t_sym));
			excludes[t_srnumsyms] = check_value_cast<t_sym>(exc);
		
			return m_o0.decode_noadapt_with_excludelist(ac,excludes,t_srnumsyms+1);
		}
	}
	
	//===========================================
	
	void encode_noadapt(rrArithEncoder * ac,int sym,int context) const
	{
		return encode(ac,sym,context);
	}
	
	int decode_noadapt(rrArithDecoder * ac,int context) const
	{
		return decode(ac,context);
	}
	
	void encode_withexclude_noadapt(rrArithEncoder * ac,int sym,int context,int exc) const
	{
		encode_withexclude(ac,sym,context,exc);
	}
	
	int decode_withexclude_noadapt(rrArithDecoder * ac,int context,int exc) const
	{
		return decode_withexclude(ac,context,exc);
	}	
	
};


//=====================================================================

template <int t_contextbits>
struct O1SRCoder4U8
{
	enum { t_alphabet = 256 };
	enum { num_contexts = 1<<t_contextbits };
	
	SRCoder4U8 m_o1[num_contexts];

	enum { o0_inc = 2 };
	O0ArithCoder<U16,256> m_o0;

	O1SRCoder4U8() { }
	
	void init()
	{
		for LOOP(c,num_contexts)
		{
			m_o1[c].init();
		}
		m_o0.init();
	}
	
	void encode(rrArithEncoder * ac,int sym,int context)
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		SRCoder4U8 & o1 = m_o1[context];
		
		if ( o1.encode(ac,sym) )
			return;
		
		int excludes[4] = { o1.getsym(0), o1.getsym(1), o1.getsym(2), o1.getsym(3) };
		
		m_o0.encode_noadapt_with_excludelist(ac,sym,excludes,4);

		m_o0.inc_count(sym,o0_inc);
		o1.update_escape(sym);
	}
	
	int decode(rrArithDecoder * ac,int context)
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		
		SRCoder4U8 & o1 = m_o1[context];
		
		int sym = o1.decode(ac);
		
		if ( sym == -1 )
		{
			int excludes[4] = { o1.getsym(0), o1.getsym(1), o1.getsym(2), o1.getsym(3) };
		
			sym = m_o0.decode_noadapt_with_excludelist(ac,excludes,4);
			RR_ASSERT( sym >= 0 && sym < 256 );
			m_o0.inc_count(sym,o0_inc);	
			o1.update_escape(sym);
		}
		
		return sym;	
	}
	
	void encode_withexclude(rrArithEncoder * ac,int sym,int context,int exc)
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		SRCoder4U8 & o1 = m_o1[context];
		
		if ( o1.encode_withexclude(ac,sym,&exc) )
			return;
		
		int excludes[5] = { o1.getsym(0), o1.getsym(1), o1.getsym(2), o1.getsym(3), exc };
		int num_excludes = (exc < 0) ? 4 : 5;
		
		m_o0.encode_noadapt_with_excludelist(ac,sym,excludes,num_excludes);
		
		m_o0.inc_count(sym,o0_inc);
		o1.update_escape(sym);
	}
	
	int decode_withexclude(rrArithDecoder * ac,int context,int exc)
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		
		SRCoder4U8 & o1 = m_o1[context];
		
		int sym = o1.decode_withexclude(ac,&exc);
		
		if ( sym == -1 )
		{
			int excludes[5] = { o1.getsym(0), o1.getsym(1), o1.getsym(2), o1.getsym(3), exc };
			int num_excludes = (exc < 0) ? 4 : 5;

			sym = m_o0.decode_noadapt_with_excludelist(ac,excludes,num_excludes);
			
			RR_ASSERT( sym >= 0 && sym < t_alphabet );
			m_o0.inc_count(sym,o0_inc);	
			o1.update_escape(sym);
		}
		
		return sym;	
	}	
	
	void encode_withexcludes(rrArithEncoder * ac,int sym,int context,int exc1,int exc2)
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		SRCoder4U8 & o1 = m_o1[context];
		
		if ( o1.encode_withexcludes(ac,sym,&exc1,&exc2) )
			return;
		
		int excludes[6] = { o1.getsym(0), o1.getsym(1), o1.getsym(2), o1.getsym(3) };
		int num_excludes = 4;
		if ( exc1 >= 0 ) excludes[num_excludes++] = exc1;
		if ( exc2 >= 0 ) excludes[num_excludes++] = exc2;
		
		m_o0.encode_noadapt_with_excludelist(ac,sym,excludes,num_excludes);
		
		m_o0.inc_count(sym,o0_inc);
		o1.update_escape(sym);
	}
	
	int decode_withexcludes(rrArithDecoder * ac,int context,int exc1,int exc2)
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		
		SRCoder4U8 & o1 = m_o1[context];
		
		int sym = o1.decode_withexcludes(ac,&exc1,&exc2);
		
		if ( sym == -1 )
		{
			int excludes[6] = { o1.getsym(0), o1.getsym(1), o1.getsym(2), o1.getsym(3) };
			int num_excludes = 4;
			if ( exc1 >= 0 ) excludes[num_excludes++] = exc1;
			if ( exc2 >= 0 ) excludes[num_excludes++] = exc2;

			sym = m_o0.decode_noadapt_with_excludelist(ac,excludes,num_excludes);
			
			RR_ASSERT( sym >= 0 && sym < t_alphabet );
			m_o0.inc_count(sym,o0_inc);	
			o1.update_escape(sym);
		}
		
		return sym;	
	}	
	
	//============================================================
	
	void encode_noadapt(rrArithEncoder * ac,int sym,int context) const
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		const SRCoder4U8 & o1 = m_o1[context];
		
		if ( o1.encode_noadapt(ac,sym) )
			return;
		
		int excludes[4] = { o1.getsym(0), o1.getsym(1), o1.getsym(2), o1.getsym(3) };
		
		m_o0.encode_noadapt_with_excludelist(ac,sym,excludes,4);
	}
	
	int decode_noadapt(rrArithDecoder * ac,int context) const
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		
		const SRCoder4U8 & o1 = m_o1[context];
		
		int sym = o1.decode_noadapt(ac);
		
		if ( sym == -1 )
		{
			int excludes[4] = { o1.getsym(0), o1.getsym(1), o1.getsym(2), o1.getsym(3) };
		
			sym = m_o0.decode_noadapt_with_excludelist(ac,excludes,4);
			RR_ASSERT( sym >= 0 && sym < 256 );
		}
		
		return sym;	
	}
	
	void encode_withexclude_noadapt(rrArithEncoder * ac,int sym,int context,int exc) const
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		const SRCoder4U8 & o1 = m_o1[context];
		
		if ( o1.encode_withexclude_noadapt(ac,sym,&exc) )
			return;
		
		int excludes[5] = { o1.getsym(0), o1.getsym(1), o1.getsym(2), o1.getsym(3), exc };
		int num_excludes = (exc < 0) ? 4 : 5;
		
		m_o0.encode_noadapt_with_excludelist(ac,sym,excludes,num_excludes);
	}
	
	int decode_withexclude_noadapt(rrArithDecoder * ac,int context,int exc) const
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		
		const SRCoder4U8 & o1 = m_o1[context];
		
		int sym = o1.decode_withexclude_noadapt(ac,&exc);
		
		if ( sym == -1 )
		{
			int excludes[5] = { o1.getsym(0), o1.getsym(1), o1.getsym(2), o1.getsym(3), exc };
			int num_excludes = (exc < 0) ? 4 : 5;

			sym = m_o0.decode_noadapt_with_excludelist(ac,excludes,num_excludes);
		}
		
		return sym;	
	}
	
	void encode_withexcludes_noadapt(rrArithEncoder * ac,int sym,int context,int exc1,int exc2) const
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		const SRCoder4U8 & o1 = m_o1[context];
		
		if ( o1.encode_withexcludes_noadapt(ac,sym,&exc1,&exc2) )
			return;
		
		int excludes[6] = { o1.getsym(0), o1.getsym(1), o1.getsym(2), o1.getsym(3) };
		int num_excludes = 4;
		if ( exc1 >= 0 ) excludes[num_excludes++] = exc1;
		if ( exc2 >= 0 ) excludes[num_excludes++] = exc2;
		
		m_o0.encode_noadapt_with_excludelist(ac,sym,excludes,num_excludes);
	}
	
	int decode_withexcludes_noadapt(rrArithDecoder * ac,int context,int exc1,int exc2) const
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		
		const SRCoder4U8 & o1 = m_o1[context];
		
		int sym = o1.decode_withexcludes_noadapt(ac,&exc1,&exc2);
		
		if ( sym == -1 )
		{
			int excludes[6] = { o1.getsym(0), o1.getsym(1), o1.getsym(2), o1.getsym(3) };
			int num_excludes = 4;
			if ( exc1 >= 0 ) excludes[num_excludes++] = exc1;
			if ( exc2 >= 0 ) excludes[num_excludes++] = exc2;

			sym = m_o0.decode_noadapt_with_excludelist(ac,excludes,num_excludes);
		}
		
		return sym;	
	}
};


template <int t_contextbits>
struct O1SRCoder4U8_Const : public O1SRCoder4U8<t_contextbits>
{
	typedef O1SRCoder4U8<t_contextbits> parent_type;

	O1SRCoder4U8_Const() { }

	void encode(rrArithEncoder * ac,int sym,int context) const
	{
		parent_type::encode_noadapt(ac,sym,context);
	}
	
	int decode(rrArithDecoder * ac,int context) const
	{
		return parent_type::decode_noadapt(ac,context);
	}
	
	void encode_withexclude(rrArithEncoder * ac,int sym,int context,int exc) const
	{
		return parent_type::encode_withexclude_noadapt(ac,sym,context,exc);
	}
	
	int decode_withexclude(rrArithDecoder * ac,int context,int exc) const
	{
		return parent_type::decode_withexclude_noadapt(ac,context,exc);
	}
	
	void encode_withexcludes(rrArithEncoder * ac,int sym,int context,int exc1,int exc2) const
	{
		return parent_type::encode_withexcludes_noadapt(ac,sym,context,exc1,exc2);
	}
		
	int decode_withexcludes(rrArithDecoder * ac,int context,int exc1,int exc2) const
	{
		return parent_type::decode_withexcludes_noadapt(ac,context,exc1,exc2);
	}
};

OODLE_NS_END

