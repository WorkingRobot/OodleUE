// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "arith_o0.inl"
#include "arith_symbolranker.inl"

OODLE_NS_START

/*******************

SContext

= "small context"

linear list of up to N symbols in each context
sorted by count so that in skewed distributions we find the MPS quickly

escapes down to order0

-------------------------

@@@@ TODO :

1. I'm not sure about my zero-frequency estimator

2. I'm not sure about my novel symbol replacement strategy
	currently I always replace the lowest count
	perhaps don't replace it if its count is higher than X (but then do scale it down)
	perhaps always renorm after replacing? mm or something else to prevent thrashing
		perhaps if "escape" hits a certain sum then renorm

*******************/

#define scontext_escape_init	1
#define scontext_count_inc		2
#define scontext_count_novel	2
#define scontext_escape_inc		2
#define scontext_escape_dec		0
//#define scontext_total_renorm	512
#define scontext_total_renorm	4096

template <int t_scnumsyms,typename t_sym,typename t_count>
struct SContext
{
	t_sym	m_syms[t_scnumsyms];
	t_count m_counts[t_scnumsyms];
	S32		m_numsyms; // 0 <= t_scnumsyms;
	S32		m_total; // = sum{m_counts} + m_escape
	S32		m_escape;
	
	void init()
	{
		m_numsyms = 0;
		m_escape = scontext_escape_init;
		m_total = m_escape;
	}

	int find(int val) const
	{
		for(int s=0;s<m_numsyms;s++)
		{
			if ( m_syms[s] == (t_sym)val )
				return s;
		}
		return -1;
	}

	bool encode(rrArithEncoder * ac,int val)
	{
		RR_ASSERT( m_escape > 0 );		
		RR_ASSERT( m_total >= m_escape + m_numsyms );		
		
		S32 low = 0;
		for(int s=0;s<m_numsyms;s++)
		{
			S32 cur = m_counts[s];
			if ( m_syms[s] == (t_sym)val )
			{
				rrArithEncodeRange(ac,low,cur,m_total);
				update_found(s);
				return true;
			}
			low += cur;
		}
		RR_ASSERT( low == m_total - m_escape );

		rrArithEncodeRange(ac,m_total-m_escape,m_escape,m_total);
		// m_escape will be adjusted in update_escape
		return false;
	}

	int decode(rrArithDecoder * ac)
	{
		RR_ASSERT( m_escape > 0 );		
		RR_ASSERT( m_total >= m_escape + m_numsyms );		
		
		S32 target = rrArithDecodePeek(ac,m_total);
		
		S32 low = 0;
		for(int s=0;s<m_numsyms;s++)
		{
			S32 cur = m_counts[s];
			low += cur;
			if ( target < low )
			{
				rrArithDecodeRemove(ac,low-cur,cur,m_total);
				S32 ret = m_syms[s];
				update_found(s);
				return ret;
			}
		}
		RR_ASSERT( low == m_total - m_escape );
		
		rrArithDecodeRemove(ac,m_total-m_escape,m_escape,m_total);
		
		return -1;
	}
	
	bool encode_withexclude(rrArithEncoder * ac,int val,int * pexc)
	{
		int exc = *pexc;
		RR_ASSERT( val != exc );
		S32 low = 0;
		for(int s=0;s<m_numsyms;s++)
		{
			if ( m_syms[s] == exc )
			{
				*pexc = -1;
				continue;
			}
			S32 cur = m_counts[s];
			if ( m_syms[s] == (t_sym)val )
			{
				// keep iterating to get total
				int slot = s;
				
				S32 total = low+cur;
				
				for(s=slot+1;s<m_numsyms;s++)
				{
					if ( m_syms[s] == exc )
					{
						*pexc = -1;
						continue;
					}
					S32 cur = m_counts[s];
					total += cur;
				}
				total += m_escape;
				RR_ASSERT( total <= m_total );
				
				rrArithEncodeRange(ac,low,cur,total);
				update_found(slot);
				return true;
			}
			low += cur;
		}
		RR_ASSERT( low <= m_total - m_escape );
		
		rrArithEncodeRange(ac,low,m_escape,low+m_escape);
		// m_escape will be adjusted in update_escape
		return false;
	}

	int decode_withexclude(rrArithDecoder * ac,int * pexc)
	{
		int exc = *pexc;
		S32 total = m_escape;
		for(int s=0;s<m_numsyms;s++)
		{
			if ( m_syms[s] == exc )
			{
				*pexc = -1;
				continue;
			}
			total += m_counts[s];
		}
		
		S32 target = rrArithDecodePeek(ac,total);
		RR_ASSERT( target <= total );
		
		S32 low = 0;
		for(int s=0;s<m_numsyms;s++)
		{
			if ( m_syms[s] == exc )
				continue;
			S32 cur = m_counts[s];
			low += cur;
			if ( target < low )
			{
				rrArithDecodeRemove(ac,low-cur,cur,total);
				S32 ret = m_syms[s];
				update_found(s);
				return ret;
			}
		}
		RR_ASSERT( low <= m_total - m_escape );

		RR_ASSERT( target >= total - m_escape );
		
		rrArithDecodeRemove(ac,total-m_escape,m_escape,total);
		
		return -1;
	}
	

	bool encode_noadapt(rrArithEncoder * ac,int val) const
	{
		S32 low = 0;
		for(int s=0;s<m_numsyms;s++)
		{
			S32 cur = m_counts[s];
			if ( m_syms[s] == (t_sym)val )
			{
				rrArithEncodeRange(ac,low,cur,m_total);
				return true;
			}
			low += cur;
		}
		RR_ASSERT( low == m_total - m_escape );
		
		rrArithEncodeRange(ac,m_total-m_escape,m_escape,m_total);
		// m_escape will be adjusted in update_escape
		return false;
	}	
	
	int decode_noadapt(rrArithDecoder * ac) const 
	{
		U32 target = rrArithDecodePeek(ac,m_total);
		
		S32 low = 0;
		for(int s=0;s<m_numsyms;s++)
		{
			S32 cur = m_counts[s];
			low += cur;
			if ( target < low )
			{
				rrArithDecodeRemove(ac,low-cur,cur,m_total);
				S32 ret = m_syms[s];
				return ret;
			}
		}
		RR_ASSERT( low == m_total - m_escape );
		
		rrArithDecodeRemove(ac,m_total-m_escape,m_escape,m_total);
		
		return -1;
	}
	
	bool encode_withexclude_noadapt(rrArithEncoder * ac,int val,int * pexc) const
	{
		int exc = *pexc;
		RR_ASSERT( val != exc );
		S32 low = 0;
		for(int s=0;s<m_numsyms;s++)
		{
			if ( m_syms[s] == exc )
			{
				*pexc = -1;
				continue;
			}
			S32 cur = m_counts[s];
			if ( m_syms[s] == (t_sym)val )
			{
				// keep iterating to get total
				int slot = s;
				
				S32 total = low+cur;
				
				for(s=slot+1;s<m_numsyms;s++)
				{
					if ( m_syms[s] == exc )
					{
						*pexc = -1;
						continue;
					}
					S32 cur = m_counts[s];
					total += cur;
				}
				total += m_escape;
				RR_ASSERT( total <= m_total );
				
				rrArithEncodeRange(ac,low,cur,total);
				return true;
			}
			low += cur;
		}
		RR_ASSERT( low <= m_total - m_escape );
		
		rrArithEncodeRange(ac,low,m_escape,low+m_escape);
		// m_escape will be adjusted in update_escape
		return false;
	}

	int decode_withexclude_noadapt(rrArithDecoder * ac,int * pexc) const
	{
		int exc = *pexc;
		RR_ASSERT( val != exc );
		S32 total = m_escape;
		for(int s=0;s<m_numsyms;s++)
		{
			if ( m_syms[s] == exc )
			{
				*pexc = -1;
				continue;
			}
			total += m_counts[s];
		}
		
		U32 target = rrArithDecodePeek(ac,total);
		RR_ASSERT( target <= total );
		
		S32 low = 0;
		for(int s=0;s<m_numsyms;s++)
		{
			if ( m_syms[s] == exc )
				continue;
			S32 cur = m_counts[s];
			low += cur;
			if ( target < low )
			{
				rrArithDecodeRemove(ac,low-cur,cur,total);
				S32 ret = m_syms[s];
				return ret;
			}
		}
		RR_ASSERT( low <= m_total - m_escape );

		RR_ASSERT( target >= total - m_escape );
		
		rrArithDecodeRemove(ac,total-m_escape,m_escape,total);
		
		return -1;
	}
	
	void update_found(int slot)
	{
		RR_ASSERT( m_counts[slot] > 0 );
		S32 to = m_counts[slot] + scontext_count_inc;
		if ( to >= (S32)((t_count)(-1)) )
		{
			renorm();
		}
		m_counts[slot] += scontext_count_inc;
		m_total += scontext_count_inc;
		if ( scontext_escape_dec > 0 && m_escape > scontext_escape_dec )
		{
			m_escape -= scontext_escape_dec;
			m_total  -= scontext_escape_dec;
		}
		if ( m_total >= scontext_total_renorm )
		{
			renorm();
		}
		// bubble sort :
		while( slot > 0 && m_counts[slot-1] <= m_counts[slot] )
		{
			swap(m_syms[slot-1],m_syms[slot]);
			swap(m_counts[slot-1],m_counts[slot]);
			slot--;
		}
	}
	
	void update_escape(int val)
	{
		m_escape += scontext_escape_inc;
		m_total += scontext_escape_inc;
		
		if ( m_numsyms < t_scnumsyms )
		{
			m_syms[m_numsyms] = check_value_cast<t_sym>(val);
			m_counts[m_numsyms] = scontext_count_novel;
			m_total += scontext_count_novel;
			m_numsyms++;
		}
		else
		{
			m_total -= m_counts[t_scnumsyms-1];
			m_syms[t_scnumsyms-1] = check_value_cast<t_sym>(val);
			m_counts[t_scnumsyms-1] = scontext_count_novel;
			m_total += scontext_count_novel;
			
			int slot = m_numsyms-1;
			// bubble sort :
			while( slot > 0 && m_counts[slot-1] <= m_counts[slot] )
			{
				swap(m_syms[slot-1],m_syms[slot]);
				swap(m_counts[slot-1],m_counts[slot]);
				slot--;
			}
		}
	}
	
	void renorm()
	{
		//*
		
		// just half escape count
		
		m_escape = (m_escape/2) + 1;
		//m_escape = (m_escape+1)/2;
		m_total = m_escape;
		for(int s=0;s<m_numsyms;s++)
		{
			m_counts[s] = (m_counts[s]+1)/2;
			RR_ASSERT(m_counts[s] > 0 );
			m_total += m_counts[s];
		}
		RR_ASSERT( m_total <= scontext_total_renorm );
		
		/*/
		
		// rebuild escape count from the number of low counts
		// doesn't really work cuz most of the "novel" symbols are off the end
		
		m_escape = scontext_escape_init;
		m_total = 0;
		for(int s=0;s<m_numsyms;s++)
		{
			m_counts[s] = (m_counts[s]+1)/2;
			RR_ASSERT(m_counts[s] > 0 );
			m_total += m_counts[s];
			if ( m_counts[s] <= scontext_count_novel )
			{
				m_escape += scontext_escape_inc;
			}
		}
		m_total += m_escape;
		
		/**/
	}
};

template <int t_scnumsyms,typename t_sym,typename t_count,int t_numcontexts,int t_alphabet>
struct O1SContextCoder
{
	SContext<t_scnumsyms,t_sym,t_count> m_o1[t_numcontexts];

	enum { num_contexts = t_numcontexts };
	enum { o0_inc = 2 };
	O0ArithCoder<U16,t_alphabet> m_o0;

	O1SContextCoder() { }
	
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
			
		m_o0.encode_noadapt_with_excludelist(ac,sym,m_o1[context].m_syms,m_o1[context].m_numsyms);

		m_o0.inc_count(sym,o0_inc);
		m_o1[context].update_escape(sym);
	}
	
	int decode(rrArithDecoder * ac,int context)
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		
		int sym = m_o1[context].decode(ac);
		
		if ( sym == -1 )
		{
			sym = m_o0.decode_noadapt_with_excludelist(ac,m_o1[context].m_syms,m_o1[context].m_numsyms);
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
		int ns = m_o1[context].m_numsyms;
		
		if ( exc < 0 )
		{
			m_o0.encode_noadapt_with_excludelist(ac,sym,o1_excludes,ns);
		}
		else
		{
			t_sym excludes[t_scnumsyms+1];
			memcpy(excludes,o1_excludes,ns*sizeof(t_sym));
			excludes[ns] = check_value_cast<t_sym>(exc);
			
			m_o0.encode_noadapt_with_excludelist(ac,sym,excludes,ns+1);
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
			int ns = m_o1[context].m_numsyms;
		
			if ( exc < 0 )
			{
				sym = m_o0.decode_noadapt_with_excludelist(ac,o1_excludes,ns);
			}
			else
			{
				t_sym excludes[t_scnumsyms+1];
				memcpy(excludes,o1_excludes,ns*sizeof(t_sym));
				excludes[ns] = check_value_cast<t_sym>(exc);
			
				sym = m_o0.decode_noadapt_with_excludelist(ac,excludes,ns+1);
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
			
		m_o0.encode_noadapt_with_excludelist(ac,sym,m_o1[context].m_syms,m_o1[context].m_numsyms);
	}
	
	int decode_noadapt(rrArithDecoder * ac,int context) const
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		
		int sym = m_o1[context].decode_noadapt(ac);
		
		if ( sym == -1 )
		{
			sym = m_o0.decode_noadapt_with_excludelist(ac,m_o1[context].m_syms,m_o1[context].m_numsyms);
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
		int ns = m_o1[context].m_numsyms;
				
		if ( exc < 0 )
		{
			m_o0.encode_noadapt_with_excludelist(ac,sym,o1_excludes,ns);
		}
		else
		{
			t_sym excludes[t_scnumsyms+1];
			memcpy(excludes,o1_excludes,ns*sizeof(t_sym));
			excludes[ns] = check_value_cast<t_sym>(exc);
			
			m_o0.encode_noadapt_with_excludelist(ac,sym,excludes,ns+1);
		}
	}
	
	int decode_withexclude_noadapt(rrArithDecoder * ac,int context,int exc) const
	{
		RR_ASSERT( context >= 0 && context < num_contexts );
		
		int sym = m_o1[context].decode_withexclude_noadapt(ac,&exc);
		
		if ( sym == -1 )
		{
			const t_sym * o1_excludes = m_o1[context].m_syms;		
			int ns = m_o1[context].m_numsyms;
		
			if ( exc < 0 )
			{
				sym = m_o0.decode_noadapt_with_excludelist(ac,o1_excludes,ns);
			}
			else
			{
				t_sym excludes[t_scnumsyms+1];
				memcpy(excludes,m_o1[context].m_syms,ns*sizeof(t_sym));
				excludes[ns] = exc;
			
				sym = m_o0.decode_noadapt_with_excludelist(ac,excludes,ns+1);
			}
		}
		
		return sym;	
	}	
	
};

OODLE_NS_END
