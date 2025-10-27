// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

OODLE_NS_START

template <typename t_count,int t_alphabet>
struct O0ArithCoder
{
	t_count m_counts[t_alphabet];
	
	enum { num_16s = (t_alphabet+15)/16 };
	enum { num_16sfull = t_alphabet/16 };
	enum { num_innonfull16 = (t_alphabet&0xF) };
	//enum { isthereanonfull16 = (num_16s != num_16sfull) };
	enum { indexnonfull16 = num_16sfull };

	// t_count should be unsigned :
	RR_COMPILER_ASSERT( ((t_count)(-1)) > ((t_count)(-2)) );

	#define t_count_max	((t_count)(-1))
		
	S32 m_sum16s[num_16s];
	S32 m_sum;
	
	O0ArithCoder()
	{
		init();
	}
	
	void init()
	{
		for LOOP(i,t_alphabet)
		{
			m_counts[i] = 1;
		}
		for LOOP(i,num_16sfull)
		{
			m_sum16s[i] = 16;
		}
		if ( (int)indexnonfull16 < (int)num_16s ) // ( isthereanonfull16 )
		{
			#if defined(__clang__)
			# pragma clang diagnostic push
			# pragma clang diagnostic ignored "-Warray-bounds"
			#endif
			
				m_sum16s[indexnonfull16] = num_innonfull16;
			
			#if defined(__clang__)
			# pragma clang diagnostic pop
			#endif
		}
		m_sum = t_alphabet;
		
		//RR_ASSERT( m_sum == sum(m_counts,m_counts+t_alphabet,(S32)0) );
		//RR_ASSERT( m_sum == sum(m_sum16s,m_sum16s+num_16s,(S32)0) );
	}
	
	void set_from_histo(const U32 * histo)
	{
		m_sum = 0;
		rrMemSetZero(m_sum16s,sizeof(m_sum16s));
		
		U32 histomax = 0;
		for LOOP(i,t_alphabet)
		{
			histomax = RR_MAX(histomax,histo[i]);
		}
		
		F32 scale = 1.f;
		if ( histomax >= t_count_max )
			scale = (F32)t_count_max / (histomax+1);
		
		for LOOP(i,t_alphabet)
		{
			S32 c = rr_froundint( histo[i] * scale + 1 );
			c = RR_CLAMP(c, 1, t_count_max);
			m_counts[i] = (t_count) c;
			m_sum16s[(i/16)] += c;
			m_sum += c;
		}
		
		//RR_ASSERT( m_sum == sum(m_counts,m_counts+t_alphabet,(S32)0) );
		//RR_ASSERT( m_sum == sum(m_sum16s,m_sum16s+num_16s,(S32)0) );
	}
	
	void renorm()
	{
		m_sum = 0;
		rrMemSetZero(m_sum16s,sizeof(m_sum16s));
		
		for LOOP(i,t_alphabet)
		{
			t_count c = m_counts[i];
			c = (c>>1) + 1;
			m_counts[i] = c;
			m_sum16s[(i/16)] += c;
			m_sum += c;
		}
		
		//RR_ASSERT( m_sum == sum(m_counts,m_counts+t_alphabet,(S32)0) );
		//RR_ASSERT( m_sum == sum(m_sum16s,m_sum16s+num_16s,(S32)0) );
	}
	
	void inc_count(int sym,int inc)
	{
		S32 prev = m_counts[sym];
		S32 next = prev + inc;
		if ( next > (S32) t_count_max )
		{
			renorm();
			
			prev = m_counts[sym];
			next = prev + inc;
			RR_ASSERT( next <= (S32) t_count_max );
		}
		
		m_counts[sym] = check_value_cast<t_count>(next);
		m_sum16s[(sym/16)] += inc;
		m_sum += inc;	
	}
	
	void encode_noadapt(rrArithEncoder * ac, int sym) const
	{
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		S32 lo = 0;
		
		int i16 = sym/16;
		RR_ASSERT( sym >= i16*16 && sym < (i16+1)*16 );
		for LOOP(i,i16)
		{
			lo += m_sum16s[i];
		}
		for (int i = i16*16;i<sym;i++)
		{
			lo += m_counts[i];
		}
		
		//RR_ASSERT( m_sum == sum(m_counts,m_counts+t_alphabet,(S32)0) );
		//RR_ASSERT( lo == sum(m_counts,m_counts+sym,(S32)0) );
		
		rrArithEncodeRange(ac,lo,m_counts[sym],m_sum);
	}
	
	int decode_noadapt(rrArithDecoder * ac) const
	{
		//RR_ASSERT( m_sum == sum(m_counts,m_counts+t_alphabet,(S32)0) );
		
		S32 target = rrArithDecodePeek(ac,m_sum);
		
		int i16;
		S32 lo = 0;
		for (i16=0;i16<num_16s;i16++)
		{
			S32 next = lo + m_sum16s[i16];
			if ( target < next )
				break;
			lo = next;
		}
		int i = i16*16;
		for(;;)
		{
			RR_ASSERT( i < t_alphabet );
			S32 next = lo + m_counts[i];
			if ( target < next )
				break;
			lo = next;
			i++;
		}
		
		RR_ASSERT( i < t_alphabet );
		RR_ASSERT( target >= lo && target < lo+m_counts[i] );
		
		//RR_ASSERT( lo == sum(m_counts,m_counts+i,(S32)0) );
		
		rrArithDecodeRemove(ac,lo,m_counts[i],m_sum);

		return i;		
	}
	
	void encode_noadapt_with_exclude(rrArithEncoder * ac, int sym, int exclude) const
	{
		RR_ASSERT( sym != exclude );
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		S32 lo = 0;
		
		int i16 = sym/16;
		RR_ASSERT( sym >= i16*16 && sym < (i16+1)*16 );
		for LOOP(i,i16)
		{
			lo += m_sum16s[i];
		}
		for (int i = i16*16;i<sym;i++)
		{
			lo += m_counts[i];
		}
		
		//RR_ASSERT( m_sum == sum(m_counts,m_counts+t_alphabet,(S32)0) );
		//RR_ASSERT( lo == sum(m_counts,m_counts+sym,(S32)0) );
		
		S32 sum = m_sum;
		S32 exc_count = m_counts[exclude];
		if ( exclude < sym )
		{
			lo -= exc_count;
		}
		sum -= exc_count;
		
		rrArithEncodeRange(ac,lo,m_counts[sym],sum);
	}
	
	int decode_noadapt_with_exclude(rrArithDecoder * ac, int exclude) const
	{
		//RR_ASSERT( m_sum == sum(m_counts,m_counts+t_alphabet,(S32)0) );
		
		int i;
		
		S32 sum = m_sum;
		S32 exc_count = m_counts[exclude];
		sum -= exc_count;
		
		S32 target = rrArithDecodePeek(ac,sum);
		
		#if 1
		
		int exclude16 = exclude/16;
				
		int i16;
		S32 lo = 0;
		for (i16=0;i16<exclude16;i16++)
		{
			S32 next = lo + m_sum16s[i16];
			if ( target < next )
				goto got16_noexcl;
			lo = next;
		}
		
		// 16 of exclude :
		RR_ASSERT( i16 == exclude16 );
		{
			S32 next = lo + m_sum16s[i16] - exc_count;
			if ( target < next )
			{
				i = i16*16;
				RR_ASSERT( i <= exclude && i+16 > exclude );
				
				for(;;i++)
				{
					RR_ASSERT( i < t_alphabet );
					if ( i == exclude )
					{
						continue;
					}
					S32 next = lo + m_counts[i];
					if ( target < next )
					{
						goto got_i;
					}
					lo = next;
				}
				//RR_CANT_GET_HERE();
			}
			lo = next;
			i16++;
		}
		
		for(;i16<num_16s;i16++)
		{
			S32 next = lo + m_sum16s[i16];
			if ( target < next )
				break;
			lo = next;		
		}
				
		got16_noexcl:
		
		i = i16*16;
		RR_ASSERT( i > exclude || exclude >= i+16 ); // no need to check
		for(;;i++)
		{
			RR_ASSERT( i < t_alphabet );
			S32 next = lo + m_counts[i];
			if ( target < next )
				break;
			lo = next;
		}
		
		got_i:
		
		RR_ASSERT( i < t_alphabet );
		RR_ASSERT( i != exclude );
		RR_ASSERT( m_counts[i] != 0 );
		RR_ASSERT( target >= lo && target < lo+m_counts[i] );
		
		//RR_ASSERT( lo == sum(m_counts,m_counts+i,(S32)0) );
		
		RR_ASSERT( sum > 0 );
		RR_ASSERT( lo >= 0 && lo+m_counts[i] <= sum );
		rrArithDecodeRemove(ac,lo,m_counts[i],sum);
		return i;

		#else
		
		S32 lo = 0;
		for(i=0;;i++)
		{
			if ( i == exclude )
				continue;
			S32 next = lo + m_counts[i];
			if ( target < next )
				break;
			lo = next;			
		}
		
		RR_ASSERT( i < t_alphabet );
		RR_ASSERT( i != exclude );
		RR_ASSERT( target >= lo && target < lo+m_counts[i] );
		
		rrArithDecodeRemove(ac,lo,m_counts[i],sum);
		return i;
		
		#endif
	}
	
	
	template <typename t_exclude>
	void encode_noadapt_with_excludelist(rrArithEncoder * ac, int sym, 
									const t_exclude * excludes, const int num_excludes ) const
	{
		//SIMPLEPROFILE_SCOPE(o0_with_excludelist);
			
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		S32 lo = 0;
		
		int i16 = sym/16;
		RR_ASSERT( sym >= i16*16 && sym < (i16+1)*16 );
		for LOOP(i,i16)
		{
			lo += m_sum16s[i];
		}
		for (int i = i16*16;i<sym;i++)
		{
			lo += m_counts[i];
		}
		
		//RR_ASSERT( m_sum == sum(m_counts,m_counts+t_alphabet,(S32)0) );
		//RR_ASSERT( lo == sum(m_counts,m_counts+sym,(S32)0) );
		
		S32 sum = m_sum;
		
		for LOOP(e,num_excludes)
		{
			int exclude = excludes[e];
			RR_ASSERT( sym != exclude );
			S32 exc_count = m_counts[exclude];
			if ( exclude < sym )
			{
				RR_ASSERT( lo >= exc_count );
				lo -= exc_count;
			}
			sum -= exc_count;
		}
		
		RR_ASSERT( lo + m_counts[sym] <= sum );
		
		rrArithEncodeRange(ac,lo,m_counts[sym],sum);
	}
	
	template <typename t_exclude>	
	int decode_noadapt_with_excludelist(rrArithDecoder * ac, 
							const t_exclude * excludes, const int num_excludes ) const
	{
		//RR_ASSERT( m_sum == sum(m_counts,m_counts+t_alphabet,(S32)0) );
		
		S32 sum16s[num_16s];
		memcpy(sum16s,m_sum16s,num_16s*sizeof(S32));
	
		S32 sum = m_sum;
	
		for LOOP(e,num_excludes)
		{
			S32 exclude = excludes[e];
			RR_ASSERT( exclude >= 0 && exclude < t_alphabet );
			S32 exc_count = m_counts[exclude];
			sum -= exc_count;
			sum16s[exclude/16] -= exc_count;
		}
	
		S32 target = rrArithDecodePeek(ac,sum);
		
		int i16;
		S32 lo = 0;
		for (i16=0;i16<num_16s;i16++)
		{
			S32 next = lo + sum16s[i16];
			if ( target < next )
				break;
			lo = next;
		}
		int i = i16*16;
		
		RR_ASSERT( i16 < num_16s );
		if ( sum16s[i16] == m_sum16s[i16] )
		{
			// no excludes here
			for(;;i++)
			{
				RR_ASSERT( i < t_alphabet );
				S32 next = lo + m_counts[i];
				if ( target < next )
					break;
				lo = next;
			}
		}
		else // excludes here
		{
			for(;;i++)
			{
				RR_ASSERT( i < t_alphabet );
				
				int cnt = m_counts[i];

				// @@ have to check i against excludes that are in *this 16 only*
				//	not all of them for each i
				//	in theory I already categorie
				for LOOP(e,num_excludes)
				{
					int exclude = excludes[e];
					if ( exclude == i )
					{
						cnt = 0;
						break;
					}
				}
				
				S32 next = lo + cnt;
				if ( target < next )
					break;
				lo = next;
			}
		}
		
		RR_ASSERT( i < t_alphabet );
		RR_ASSERT( target >= lo && target < lo+m_counts[i] );
		
		//RR_ASSERT( lo == sum(m_counts,m_counts+i,(S32)0) );
		
		rrArithDecodeRemove(ac,lo,m_counts[i],sum);

		return i;		
	}
	
	//=======================================================================
	
	void encode_inc_count(rrArithEncoder * ac, int sym,const int inc = 1)
	{
		encode_noadapt(ac,sym);
		inc_count(sym,inc);
	}
	
	int decode_inc_count(rrArithDecoder * ac,const int inc = 1)
	{
		int sym = decode_noadapt(ac);
		inc_count(sym,inc);
		return sym;
	}
	
	void encode_with_exclude_inc_count(rrArithEncoder * ac, int sym, int exclude,const int inc = 1)
	{
		encode_noadapt_with_exclude(ac,sym,exclude);
		inc_count(sym,inc);
	}
	
	int decode_with_exclude_inc_count(rrArithDecoder * ac, int exclude,const int inc = 1)
	{
		int sym = decode_noadapt_with_exclude(ac,exclude);
		inc_count(sym,inc);	
		return sym;
	}
};




template <typename t_count,int t_alphabet,int t_sum_shift>
struct O0ArithCoder_Const
{
	t_count m_cumprobs[t_alphabet+1];
	
	enum { c_sum = 1<<t_sum_shift };
	
	// t_count should be unsigned :
	RR_COMPILER_ASSERT( ((t_count)(-1)) > ((t_count)(-2)) );

	#define t_count_max	((t_count)(-1))
		
	RR_COMPILER_ASSERT( c_sum < t_count_max );
		
	O0ArithCoder_Const()
	{
		init();
	}
	
	void init()
	{
		// invalidate :
		m_cumprobs[0] = 1;
	}
	
	S32 sum() const
	{
		RR_ASSERT( m_cumprobs[t_alphabet] == c_sum );
		return c_sum;
	}
	
	S32 count(int sym) const
	{
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		S32 ret = m_cumprobs[sym+1] - m_cumprobs[sym];
		RR_ASSERT( ret >= 1 );
		return ret;
	}
	
	void set_from_histo(const U32 * histo)
	{		
		RR_ASSERT( t_count_max >= t_alphabet*4 );
		
		U32 normalized_counts[t_alphabet];
		normalize_counts(normalized_counts,histo,t_alphabet,c_sum);
		
		set_from_histo_normalized(normalized_counts);
	}
		
	void set_from_histo_normalized(const U32 * normalized_counts)
	{		
		m_cumprobs[0] = 0;
		
		for LOOP(i,t_alphabet)
		{
			S32 c = normalized_counts[i];
			RR_ASSERT( c >= 1 && c < t_count_max );
			S32 cum = m_cumprobs[i] + c;
			m_cumprobs[i+1] = check_value_cast<t_count>(cum);
		}
		
		RR_ASSERT_ALWAYS( m_cumprobs[t_alphabet] == c_sum );
	}
	
	void renorm()
	{
		RR_ASSERT_FAILURE_ALWAYS("invalid call on const");
	}
	
	void inc_count(int sym,int inc)
	{
		RR_ASSERT_FAILURE_ALWAYS("invalid call on const");
	}
	
	// m_o1ac_lit_nomatch uses this
	void encode_noadapt(rrArithEncoder * ac, int sym) const
	{
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		S32 lo = m_cumprobs[sym];
		S32 c = m_cumprobs[sym+1] - lo;
		
		rrArithEncodeRangePow2(ac,lo,c,t_sum_shift);
	}
	
	void encode_noadapt_norenorm(rrArithEncoder * ac, int sym) const
	{
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		const t_count * cp = m_cumprobs+sym;
		S32 lo = cp[0];
		S32 c = cp[1] - lo;
		
		rrArithEncodeRangePow2_NoRenorm(ac,lo,c,t_sum_shift);
	}
	
	int decode_noadapt(rrArithDecoder * ac) const
	{
		S32 target = rrArithDecodePeekPow2(ac,t_sum_shift);
		
		#if 0
		
		// linear search for now :
		for(int i=0;;i++)
		{
			RR_ASSERT( i < t_alphabet );
		
			S32 lo = m_cumprobs[i];
			
			RR_ASSERT( target >= lo );
			
			S32 hi = m_cumprobs[i+1];
			
			if ( target < hi )
			{
				rrArithDecodeRemovePow2(ac,lo,hi-lo,t_sum_shift);
				return i;
			}
		}
		
		#else
		
		// binary search :
		
		const t_count * plow = lower_bound(m_cumprobs,m_cumprobs+t_alphabet,target,stdlessequal<S32>());
		RR_ASSERT( plow > m_cumprobs );
		RR_ASSERT( plow <= m_cumprobs+t_alphabet );
		S32 hi = plow[0];
		plow--;
		S32 lo = plow[0];
		RR_ASSERT( lo <= target );
		RR_ASSERT( hi > target );
		
		rrArithDecodeRemovePow2(ac,lo,hi-lo,t_sum_shift);
		int i = (int)(plow - m_cumprobs);
		RR_ASSERT( lo == m_cumprobs[i] );
		return i;
		
		#endif
	}
	
	#if 0
	void encode_noadapt_with_exclude(rrArithEncoder * ac, int sym, int exclude) const
	{
		RR_ASSERT( sym != exclude );
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		S32 lo = m_cumprobs[sym];
				
		S32 c = count(sym);
		S32 s = sum();
		
		S32 exc_count = count(exclude);
		s -= exc_count;
		if ( exclude < sym )
		{
			lo -= exc_count;
		}
		
		rrArithEncodeRange(ac,lo,c,s);
	}
	
	int decode_noadapt_with_exclude(rrArithDecoder * ac, int exclude) const
	{
		S32 s = sum();
		
		S32 exc_count = count(exclude);
		s -= exc_count;
		
		S32 target = rrArithDecodePeek(ac,s);
		
		// @@@@ TODO : need binary search
		
		// linear search for now :
		for(int i=0;i<exclude;i++)
		{
			RR_ASSERT( i < t_alphabet );
		
			S32 lo = m_cumprobs[i];
			
			RR_ASSERT( target >= lo );
			
			S32 hi = m_cumprobs[i+1];
			
			if ( target < hi )
			{
				rrArithDecodeRemove(ac,lo,hi-lo,s);
				return i;
			}		
		}
		
		for(int i=0;;i++)
		{
			RR_ASSERT( i < t_alphabet );
		
			S32 lo = m_cumprobs[i] - exc_count;
			
			RR_ASSERT( target >= lo );
			
			S32 hi = m_cumprobs[i+1] - exc_count;
			
			if ( target < hi )
			{
				rrArithDecodeRemove(ac,lo,hi-lo,s);
				return i;
			}		
		}
	}
	#endif
	
	#if 0
	template <typename t_sym>	
	void encode_noadapt_with_excludelist(rrArithEncoder * ac, int sym, 
									const t_sym * excludes, const int num_excludes ) const
	{
		//SIMPLEPROFILE_SCOPE(o0_with_excludelist);
			
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		S32 lo = m_cumprobs[sym];
				
		S32 c = count(sym);
		S32 s = sum();
		
		RR_ASSERT( lo+c <= s );
		
		for LOOP(e,num_excludes)
		{
			int exclude = excludes[e];
			RR_ASSERT( sym != exclude );
			S32 exc_count = count(exclude);
			if ( exclude < sym )
			{
				RR_ASSERT( lo >= exc_count );
				lo -= exc_count;
			}
			s -= exc_count;
		}

		RR_ASSERT( lo >= 0 && lo+c <= s );
				
		rrArithEncodeRange(ac,lo,c,s);
	}
	
	
	template <typename t_sym>	
	int decode_noadapt_with_excludelist(rrArithDecoder * ac, 
							const t_sym * excludes, const int num_excludes ) const
	{
		// @@@@ TODO : SLOW!!!
	
		S32 s = sum();
		
		for LOOP(e,num_excludes)
		{
			int exclude = excludes[e];
			S32 exc_count = count(exclude);
			s -= exc_count;
		}
			
		S32 target = rrArithDecodePeek(ac,s);

		S32 lo = 0;
						
		for(int i=0;;i++)
		{
			RR_ASSERT( i < t_alphabet );
			RR_ASSERT( target >= lo );
			
			S32 cnt = count(i);
		
			for LOOP(e,num_excludes)
			{
				int exclude = excludes[e];
				if ( exclude == i )
				{
					cnt = 0;
					break;
				}
			}
				
			S32 next = lo + cnt;
			
			if ( target < next )
			{
				rrArithDecodeRemove(ac,lo,cnt,s);
				return i;
			}
			
			lo = next;
		}
	}
	#endif
	
	//=======================================================================
};


template <typename t_count,typename t_symbol,int t_alphabet,int t_sum_shift>
struct O0ArithCoderJump_Const
{
	//t_count m_cumprobs[t_alphabet+1];
	//enum { c_vec_size = 16 }; // 128 bits = 8 shorts
	enum { c_vec_size = 8 }; // 128 bits = 8 shorts
	t_count m_cumprobs[t_alphabet+c_vec_size]; // pad for overrun
	
	//enum { c_jumpsizeshift = 8 };
	//enum { c_jumpsizeshift = 7 };
	enum { c_jumpsizeshift = 6 };
	enum { c_jumpsize = 1<<c_jumpsizeshift };
	enum { c_jumpindexshift = t_sum_shift - c_jumpsizeshift };
	t_symbol m_jumptable[c_jumpsize];
	
	enum { c_sum = 1<<t_sum_shift };
	
	// t_count should be unsigned :
	RR_COMPILER_ASSERT( ((t_count)(-1)) > ((t_count)(-2)) );

	#define t_count_max	((t_count)(-1))
		
	RR_COMPILER_ASSERT( c_sum < t_count_max );
		
	O0ArithCoderJump_Const()
	{
		init();
	}
	
	void init()
	{
	}
	
	S32 sum() const
	{
		RR_ASSERT( m_cumprobs[t_alphabet] == c_sum );
		return c_sum;
	}
	
	S32 count(int sym) const
	{
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		S32 ret = m_cumprobs[sym+1] - m_cumprobs[sym];
		RR_ASSERT( ret >= 1 );
		return ret;
	}
	
	void set_from_histo(const U32 * histo,int for_oodle_major_version)
	{		
		RR_ASSERT( t_count_max >= t_alphabet*4 );
		
		U32 histo_sum = rrSumOfHistogram(histo,t_alphabet);
		
		U32 normalized_counts[t_alphabet];
		normalize_counts(for_oodle_major_version,normalized_counts,c_sum,histo,histo_sum,t_alphabet);
		
		set_from_histo_normalized(normalized_counts);
	}
		
	void set_from_histo_normalized(const U32 * normalized_counts)
	{		
		m_cumprobs[0] = 0;
		
		for LOOP(i,t_alphabet)
		{
			S32 c = normalized_counts[i];
			RR_ASSERT( c >= 1 && c < t_count_max );
			S32 cum = m_cumprobs[i] + c;
			m_cumprobs[i+1] = check_value_cast<t_count>(cum);
		}
		
		RR_ASSERT_ALWAYS( m_cumprobs[t_alphabet] == c_sum );
	
		// extra padding :
		for (int i=t_alphabet+1;i<t_alphabet+c_vec_size;i++)
		{
			m_cumprobs[i] = c_sum+1;
		}
		
		// make jump table :
		int sym = 0;
		for(int jumpindex=0;jumpindex<c_jumpsize;jumpindex++)
		{
			int jumptarget = jumpindex<<c_jumpindexshift;
			RR_ASSERT( m_cumprobs[sym] <= jumptarget );
			while( m_cumprobs[sym+1] <= jumptarget )
				sym++;
			RR_ASSERT( sym < t_alphabet );
			m_jumptable[jumpindex] = check_value_cast<t_symbol>(sym);
		}
	}
	
	/*
	void renorm()
	{
		RR_ASSERT_FAILURE_ALWAYS("invalid call on const");
	}
	
	void inc_count(int sym,int inc)
	{
		RR_ASSERT_FAILURE_ALWAYS("invalid call on const");
	}
	*/
	
	// m_o1ac_lit_nomatch uses this
	void encode_noadapt(rrArithEncoder * ac, int sym) const
	{
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		S32 lo = m_cumprobs[sym];
		S32 c = m_cumprobs[sym+1] - lo;
		
		rrArithEncodeRangePow2(ac,lo,c,t_sum_shift);
	}
	
	void encode_noadapt_norenorm(rrArithEncoder * ac, int sym) const
	{
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		
		const t_count * cp = m_cumprobs+sym;
		S32 lo = cp[0];
		S32 c = cp[1] - lo;
		
		rrArithEncodeRangePow2_NoRenorm(ac,lo,c,t_sum_shift);
	}
	
	int decode_noadapt(rrArithDecoder * ac) const
	{
		int ret = decode_noadapt_norenorm(ac);
		rrArithDecodeRenorm(ac);
		return ret;
	}
	
	int decode_noadapt_norenorm(rrArithDecoder * ac) const
	{
		S32 target = rrArithDecodePeekPow2(ac,t_sum_shift);
		
		RR_ASSERT( target < c_sum ); // True on non-corrupt data
		target &= (c_sum-1); // FUZZ SAFETY for OodleNetwork
		// @@ could do a branch and return failure, but then I have to drill out an error code
		
		// jump search :
		S32 jumpindex = target >> c_jumpindexshift;
		S32 sym = m_jumptable[jumpindex];

		RR_ASSERT( target >= m_cumprobs[sym] );

		#ifdef __RADSSE2__
		// vecsize=8
		// Jaguar : 9.87 mb/s (6)
		// Jaguar : 9.20 mb/s (8)
		// vecsize=16
		// Jaguar : 9.72 mb/s (6)
		// Jaguar : 9.03 mb/s (8)

		__m128i xvec = _mm_set1_epi16((S16) target);
        
		RR_ASSERT( sym < t_alphabet );
		RR_ASSERT( target >= m_cumprobs[sym] );

		while ( target >= m_cumprobs[sym+c_vec_size] )
		{
			sym += c_vec_size;
			
			RR_ASSERT( sym < t_alphabet );
			RR_ASSERT( target >= m_cumprobs[sym] );
		}
		
		const U16 * cumfreq = m_cumprobs+sym+1;

		RR_COMPILER_ASSERT( c_vec_size == 8 );
		
		__m128i cumfreq0 = _mm_loadu_si128((const __m128i *) &cumfreq[0]);
		
		// do compares and find val slot index
		__m128i gt0 = _mm_cmpgt_epi16(cumfreq0, xvec);
        
        // getting 8-bit mask on 16-bit flags
        //	so each bit is duplicated
		U32 mask = _mm_movemask_epi8(gt0);
		RR_ASSERT( mask != 0 );
		U32 index = rrCtz32( mask );

		RR_ASSERT( index >= 0 && index < 16 );
		
		// divide index by 2 for bit duplication
		sym += index>>1;
		
		S32 lo = m_cumprobs[sym];
		S32 hi = m_cumprobs[sym+1];
		
		RR_ASSERT( target >= lo && target < hi );
	
		rrArithDecodeRemovePow2_NoRenorm(ac,lo,hi-lo,t_sum_shift);
		return sym;		
				

		#else

		#if 1
		// Jaguar : 9.59 mb/s (6)
		// Jaguar : 9.59 mb/s (7)
		// Jaguar : 9.71 mb/s (8)

		// 9.60
		// 9.64
		// 9.45

		#if 0
		// do a first step branchless
		// seems to hurt
		RR_ASSERT( sym < t_alphabet );
		RR_ASSERT( target >= m_cumprobs[sym] );
		
		sym += (target >= m_cumprobs[sym+1]) ? 1 : 0;
		#endif
		
		// separate out first check
		// so it gets its own branch predictor
		RR_ASSERT( sym < t_alphabet );
		RR_ASSERT( target >= m_cumprobs[sym] );
		
		S32 hi = m_cumprobs[sym+1];
		
		if_likely ( target < hi )
		{
			S32 lo = m_cumprobs[sym];
			rrArithDecodeRemovePow2_NoRenorm(ac,lo,hi-lo,t_sum_shift);
			return sym;
		}

		sym++;

		// linear search for now :
		for(;;sym++)
		{
			RR_ASSERT( sym < t_alphabet );			
			RR_ASSERT( target >= m_cumprobs[sym] );
			
			hi = m_cumprobs[sym+1];
			
			if_likely ( target < hi )
			{
				S32 lo = m_cumprobs[sym];
				rrArithDecodeRemovePow2_NoRenorm(ac,lo,hi-lo,t_sum_shift);
				return sym;
			}
		}
		
		#else
		// Jaguar : 9.62 mb/s (6)
		// tiny tiny tiny win over naive linear search
		
		// multi-linear search :
		for(;;)
		{
			RR_ASSERT( sym < t_alphabet );		
			RR_ASSERT( target >= m_cumprobs[sym] );
			
			if_unlikely ( target >= m_cumprobs[sym+4] )
			{
				sym += 4;
				continue;
			}
			
			// target is in the next 4
			
			S32 hi1 = m_cumprobs[sym+1];
			S32 hi2 = m_cumprobs[sym+2];
			S32 hi3 = m_cumprobs[sym+3];
			
			/*
			U32 diff1 = ~(target - hi1);
			U32 diff2 = ~(target - hi2);
			U32 diff3 = ~(target - hi3);
			/*/
			U32 diff1 = hi1 - (target+1);
			U32 diff2 = hi2 - (target+1);
			U32 diff3 = hi3 - (target+1);
			/**/
			
			// get the sign bits :
			U32 step = (diff1&0x10000) + (diff2&0x10000) + (diff3&0x10000);
			sym += (step>>16);
			
			S32 lo = m_cumprobs[sym];
			S32 hi = m_cumprobs[sym+1];
			RR_ASSERT( target >= lo );
			RR_ASSERT( target < hi );
			
			rrArithDecodeRemovePow2_NoRenorm(ac,lo,hi-lo,t_sum_shift);
			return sym;
		}
		
		#endif
		
		#endif
						
		/*
		// binary search :
		
		const t_count * plow = lower_bound(m_cumprobs,m_cumprobs+t_alphabet,target,stdlessequal<S32>());
		RR_ASSERT( plow > m_cumprobs );
		RR_ASSERT( plow <= m_cumprobs+t_alphabet );
		S32 hi = plow[0];
		plow--;
		S32 lo = plow[0];
		RR_ASSERT( lo <= target );
		RR_ASSERT( hi > target );
		
		rrArithDecodeRemovePow2(ac,lo,hi-lo,t_sum_shift);
		int i = (int)(plow - m_cumprobs);
		RR_ASSERT( lo == m_cumprobs[i] );
		return i;
		*/
	}
	
	//=======================================================================
};

// fat is theoretically less cache misses & better dependency chain
//	but not any faster in practice
// memory use of the FAT model is insane
//#define O0ATDC_FAT	1
#define O0ATDC_FAT	0

template <typename t_count,typename t_sym,int t_alphabet,int t_sum_shift>
struct O0ArithTableDecoder_Const
{
	typedef O0ArithCoder_Const<t_count,t_alphabet,t_sum_shift> t_encoder;

	// @@ store t_encoder here ? (old way)
	//	or separately ?

	#if O0ATDC_FAT
	// packing doesn't seem to matter
	//RR_PACKED_STRUCT_START(decodetableentry)
	//#pragma pack(push)
	//#pragma pack(2)
	struct decodetableentry
	{
		t_count lo;
		t_count c;
		t_sym   sym;
	};
	//RR_PACKED_STRUCT_END
	//#pragma pack(pop)
	decodetableentry m_decodetable[1<<t_sum_shift];
	#else
	t_sym m_decodetable[1<<t_sum_shift];
	#endif
	
	enum { c_sum = 1<<t_sum_shift };
		
	O0ArithTableDecoder_Const()
	{
		// check that t_sym can hold t_alphabet :
		RR_COMPILER_ASSERT( (t_sym)(t_alphabet) == t_alphabet );
	}
	
	void init()
	{
	}
	
	S32 sum() const
	{
		//RR_ASSERT( m_cumprobs[t_alphabet] == c_sum );
		return c_sum;
	}
	
	void set_from_histo(t_encoder * pencoder,const U32 * histo)
	{		
		RR_ASSERT( t_count_max >= t_alphabet*4 );
		
		U32 normalized_counts[t_alphabet];
		normalize_counts(normalized_counts,histo,t_alphabet,c_sum);
		
		set_from_histo_normalized(pencoder,normalized_counts);
	}
		
	void set_from_histo_normalized(t_encoder * pencoder,const U32 * normalized_counts)
	{
		pencoder->set_from_histo_normalized(normalized_counts);
	
		const t_count * cumprobs = pencoder->m_cumprobs;
	
		RR_ASSERT_ALWAYS( cumprobs[t_alphabet] == c_sum );
		
		for(int sym=0;sym<t_alphabet;sym++)
		{
			int lo = cumprobs[sym];
			int hi = cumprobs[sym+1];
			int prob = hi - lo;

			if ( prob == 0 ) continue;
			// this is a memset :
			for(int cumprob=lo;cumprob<hi;cumprob++)
			{
				#if O0ATDC_FAT
				m_decodetable[cumprob].sym = (t_sym) sym;
				m_decodetable[cumprob].lo = (t_count)lo;
				m_decodetable[cumprob].c = (t_count)prob;		
				#else
				m_decodetable[cumprob] = (t_sym) sym;
				#endif	
			}
		}
	}
			
	int decode_noadapt(const t_encoder * pencoder,rrArithDecoder * ac) const
	{
		U32 target = rrArithDecodePeekPow2(ac,t_sum_shift);
		
		RR_ASSERT( target < c_sum ); // true on non-corrupt data
		// fuzz safety for Oodle Network; this is a NOP on valid data
		// @@ could do a branch and return failure, but then I have to drill out an error code
		target &= (c_sum-1);
		/*
		if ( target >= c_sum )
		{
			rrprintfvar(target);
			rrprintf("code : %d\n",ac->code);
			rrprintf("range: %d\n",ac->range);
			//return 0;
			target = c_sum-1;
		}
		*/
		
		#if O0ATDC_FAT
		int lo		= m_decodetable[target].lo;
		int count	= m_decodetable[target].c;
		int sym		= m_decodetable[target].sym;
		#else
		int sym		= m_decodetable[target];

		RR_ASSERT( sym >= 0 && sym < t_alphabet );

		int lo		= pencoder->m_cumprobs[sym];
		int hi		= pencoder->m_cumprobs[sym+1];
		int count	= hi-lo;
		#endif
		
		rrArithDecodeRemovePow2(ac,lo,count,t_sum_shift);
		return sym;
	}
};


template <typename t_count,int t_alphabet,int t_sum_shift>
struct O0ArithCoder_Const_ExcludeInContext :
	public O0ArithCoder_Const<t_count,t_alphabet,t_sum_shift>
{
	typedef O0ArithCoder_Const<t_count,t_alphabet,t_sum_shift> parent_type;
	
	using parent_type::c_sum;
	using parent_type::m_cumprobs;
	using parent_type::encode_noadapt;
	using parent_type::decode_noadapt;

	O0ArithCoder_Const_ExcludeInContext()
	{
	}
	
	// same as parent except the assert		
	void set_from_histo_with_exclude(const U32 * histo, int exc)
	{
		RR_ASSERT( t_count_max >= t_alphabet*4 );
		RR_ASSERT( histo[exc] == 0 );
		
		U32 normalized_counts[t_alphabet];
		normalize_counts(normalized_counts,histo,t_alphabet,c_sum);
		
		m_cumprobs[0] = 0;
		
		for LOOP(i,t_alphabet)
		{
			S32 c = normalized_counts[i];
			RR_ASSERT( c < t_count_max );
			RR_ASSERT( c >= 1 || i == exc );
			S32 cum = m_cumprobs[i] + c;
			m_cumprobs[i+1] = check_value_cast<t_count>(cum);
		}
		
		RR_ASSERT_ALWAYS( m_cumprobs[t_alphabet] == c_sum );
		RR_ASSERT( count_zero_ok(exc) == 0 );
	}
			
	S32 count_zero_ok(int sym) const
	{
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		S32 ret = m_cumprobs[sym+1] - m_cumprobs[sym];
		return ret;
	}
	
	// m_o1ac_lit_match uses this
	void encode_noadapt_with_exclude(rrArithEncoder * ac, int sym, int exclude) const
	{
		RR_ASSERT( sym != exclude );
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		RR_ASSERT( count_zero_ok(exclude) == 0 );
		
		encode_noadapt(ac,sym);
	}
	
	int decode_noadapt_with_exclude(rrArithDecoder * ac, int exclude) const
	{
		RR_ASSERT( count_zero_ok(exclude) == 0 );
				
		return decode_noadapt(ac);
	}
	
	//=======================================================================
};




template <typename t_count,int t_alphabet,int t_sum_shift>
struct O0ArithCoder_Const_ExcludeBitFlags
{
	t_count m_counts[t_alphabet];
	
	enum { c_sum = 1<<t_sum_shift };
	
	// t_count should be unsigned :
	RR_COMPILER_ASSERT( ((t_count)(-1)) > ((t_count)(-2)) );

	#define t_count_max	((t_count)(-1))
		
	RR_COMPILER_ASSERT( c_sum < t_count_max );
		
	O0ArithCoder_Const_ExcludeBitFlags()
	{
	}
			
	void set_from_histo(const U32 * histo)
	{		
		RR_ASSERT( t_count_max >= t_alphabet*4 );
		
		U32 normalized_counts[t_alphabet];
		normalize_counts(normalized_counts,histo,t_alphabet,c_sum);
		
		set_from_histo_normalized(normalized_counts);
	}
		
	void set_from_histo_normalized(const U32 * normalized_counts)
	{		
		for LOOP(i,t_alphabet)
		{
			S32 c = normalized_counts[i];
			RR_ASSERT( c >= 1 && c < t_count_max );
			m_counts[i] = check_value_cast<t_count>(c);
		}
	}
		
	// m_o1ac_lit_nomatch uses this
	void encode_noadapt(rrArithEncoder * ac, int sym, const BitFlags256 & excludes) const
	{
		RR_ASSERT( sym >= 0 && sym < t_alphabet );
		RR_ASSERT( ! excludes.test(sym) );
		
		S32 low = 0;
		
		for(int i=0;i<sym;i++)
		{
			if ( excludes.test(i) )
				continue;
			
			low += m_counts[i];
		}
		
		S32 cur = m_counts[sym];
		S32 sum = low + cur;

		for(int i=sym+1;i<t_alphabet;i++)
		{
			if ( excludes.test(i) )
				continue;
			
			sum += m_counts[i];
		}	
				
		rrArithEncodeRange(ac,low,cur,sum);
	}
		
	int decode_noadapt(rrArithDecoder * ac, const BitFlags256 & excludes) const
	{
		S32 sum = 0;
		
		for(int i=0;i<t_alphabet;i++)
		{
			if ( excludes.test(i) )
				continue;
			
			sum += m_counts[i];
		}
		
		RR_ASSERT( sum > 0 );
		
		S32 target = rrArithDecodePeek(ac,sum);
				
		// linear search for now :
		S32 low = 0;
		for(int i=0;;i++)
		{
			RR_ASSERT( i < t_alphabet );

			if ( excludes.test(i) )
				continue;				
		
			S32 cur = m_counts[i];
			
			RR_ASSERT( target >= low );
			
			S32 hi = low + cur;
			
			if ( target < hi )
			{
				rrArithDecodeRemove(ac,low,cur,sum);
				return i;
			}
			
			low = hi;
		}
	}
};

OODLE_NS_END
