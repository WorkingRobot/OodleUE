// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "templates/rralgorithm.h"
#include "rrstackarray.h"

OODLE_NS_START

/**

SymbolRank - alternative -
	code a binary "is it any of these"
		then codes for which one it is
	faster to decode
	perhaps nicer adaptation
	
**/

namespace histo_to_sr
{

	struct sym_and_count
	{
		U32 sym;
		U32 count;
		
		// note backwrads :
		bool operator < (const sym_and_count & rhs) const { return count > rhs.count; }
	};
	
	static U32 sort_histo(sym_and_count * to,int to_size,
					const U32 * histo, int histo_size)
	{
		// sort the whole histo; take the top N symbols
		// @@ std partial_sort would be a lot faster
		U32 tot = 0;
		RR_STACK_ARRAY(sc,sym_and_count,histo_size);
		for LOOP(i,histo_size)
		{
			sc[i].sym = i;
			sc[i].count = histo[i];
			tot += histo[i];
		}
		stdsort(sc,sc+histo_size);
		RR_ASSERT( sc[0].count >= sc[1].count );
		memcpy(to,sc,to_size*sizeof(sym_and_count));
		return tot;
	}
	

};

template <int t_numsyms, typename t_sym>
struct SRCoder
{
	rrArithBinaryModel m_probs[t_numsyms];
	t_sym m_syms[t_numsyms];
	
	#define SRCODER_ANYMATCH		m_probs[t_numsyms-1]
	#define SRCODER_ANYMATCH_PTR	&(SRCODER_ANYMATCH)
		
	SRCoder()
	{
		init();
	}	
	
	void init()
	{
		for LOOP(s,t_numsyms)
		{
			//m_probs[s] = RR_BINARY_MODEL_INIT_VAL;
			// probablility of this guy being it is much less than 50/50
			m_probs[s] = RR_BINARY_MODEL_TOT - (RR_BINARY_MODEL_TOT/256);
			m_syms[s] = (t_sym) s; // @@ ?? initial symbols ?
		}
		rrArithBinaryModelRenorm(SRCODER_ANYMATCH_PTR);
	}
	
	void set_from_histo(const U32 * histo, int histo_size)
	{
		histo_to_sr::sym_and_count syms[t_numsyms];
		U32 tot = histo_to_sr::sort_histo(syms,t_numsyms,histo,histo_size);
	
		U32 sum_in_sr = 0;
		for LOOP(i,t_numsyms)
		{
			if ( syms[i].count == 0 )
			{
				// zeros don't work
				syms[i].count ++;
				tot ++;
			}
			m_syms[i] = (t_sym) syms[i].sym;
			sum_in_sr += syms[i].count;
		}
		
		RR_ASSERT( sum_in_sr >= t_numsyms );
		RR_ASSERT( tot >= sum_in_sr );
	
		U32 Prob_NotSR = rr_froundint( RR_BINARY_MODEL_TOT * (F32)(tot+1 - sum_in_sr) / (tot+2) );
		Prob_NotSR = RR_CLAMP(Prob_NotSR,32,RR_BINARY_MODEL_TOT-32);
	
		*(SRCODER_ANYMATCH_PTR) = rrArithBinaryModel_Check( Prob_NotSR );
		
		
		for LOOP(i,t_numsyms-1)
		{
			// Pme = syms[i].count / sum_in_sr
			// 1 means its me, so 0 = P of not me
			
			RR_ASSERT( sum_in_sr >= 2 );
			RR_ASSERT( syms[i].count >= 1 );
			RR_ASSERT( sum_in_sr > syms[i].count );
			
			U32 PNotMe = rr_froundint( RR_BINARY_MODEL_TOT * (F32)(sum_in_sr - syms[i].count + 1) / (sum_in_sr + 1) );
			PNotMe = RR_CLAMP(PNotMe,32,RR_BINARY_MODEL_TOT-32);
			m_probs[i] = rrArithBinaryModel_Check( PNotMe );
			// make sum_in_sr always the sum of the remainder
			sum_in_sr -= syms[i].count;
		}
		
		RR_ASSERT( *(SRCODER_ANYMATCH_PTR) == Prob_NotSR ); 
	}
	
	void update_found(int slot)
	{
		// MTF or just move up one? or move to middle?
		//	I recall move-to-one-before-head being good
		// -> MTF is best
		
		// MTF
		// 79.8
		while(slot > 0)
		{
			swap(m_syms[slot-1],m_syms[slot]);
			slot--;
		}
	}
	
	bool encode(rrArithEncoder *ac, int sym)
	{
		for LOOP(s,t_numsyms)
		{
			if ( sym == m_syms[s] )
			{
				rrArithBinaryModelEncode(ac,1,SRCODER_ANYMATCH_PTR);
				
				for LOOP(pi,s)
				{
					rrArithBinaryModelEncode(ac,0,&m_probs[pi]);
				}
				if ( s < t_numsyms-1 )		
				{
					rrArithBinaryModelEncode(ac,1,&m_probs[s]);
				}
				
				update_found(s);
				return true;
			}
		}
		
		rrArithBinaryModelEncode(ac,0,SRCODER_ANYMATCH_PTR);
				
		return false;
	}
	
	int decode(rrArithDecoder * ac)
	{
		if ( rrArithBinaryModelDecode(ac,SRCODER_ANYMATCH_PTR) )
		{		
			for LOOP(s,t_numsyms-1)
			{
				if ( rrArithBinaryModelDecode(ac,&m_probs[s]) )
				{
					int ret = m_syms[s];
					update_found(s);
					return ret;
				}
			}
			
			int ret = m_syms[t_numsyms-1];
			update_found(t_numsyms-1);
			return ret;			
		}
				
		return -1;	
	}
	
	bool encode_withexclude(rrArithEncoder *ac, int sym,int * pexc)
	{
		int exc = *pexc;
		RR_ASSERT( sym != exc );
	
		for LOOP(s,t_numsyms)
		{			
			if ( sym == m_syms[s] )
			{
				rrArithBinaryModelEncode(ac,1,SRCODER_ANYMATCH_PTR);
				
				// do not have to set *pexc cuz I did the coding
				for LOOP(pi,s)
				{
					if ( m_syms[pi] == exc )
					{
						continue;
					}
					rrArithBinaryModelEncode(ac,0,&m_probs[pi]);
				}
				if ( s < t_numsyms-1 )		
				{
					rrArithBinaryModelEncode(ac,1,&m_probs[s]);
				}
				
				update_found(s);
				return true;
			}
		}
		
		// set *pexc
		for LOOP(s,t_numsyms)
		{
			if ( m_syms[s] == exc )
			{
				*pexc = -1;
				break;
			}
		}
		
		rrArithBinaryModelEncode(ac,0,SRCODER_ANYMATCH_PTR);
									
		return false;
	}
	
	int decode_withexclude(rrArithDecoder * ac,int * pexc)
	{
		int exc = *pexc;
		
		if ( rrArithBinaryModelDecode(ac,SRCODER_ANYMATCH_PTR) )
		{	
			// do not need to set pexc
			// cuz I do the coding
					
			for LOOP(s,t_numsyms-1)
			{			
				if ( m_syms[s] == exc )
					continue;
				
				if ( rrArithBinaryModelDecode(ac,&m_probs[s]) )
				{
					int ret = m_syms[s];
					update_found(s);
					return ret;
				}
			}
			
			int ret = m_syms[t_numsyms-1];
			update_found(t_numsyms-1);
			return ret;			
		}
		else
		{
			// have to check pexc
			
			for LOOP(s,t_numsyms)
			{
				if ( m_syms[s] == exc )
				{
					*pexc = -1;
					continue;
				}
			}
		}
						
		return -1;	
	}
	
	// after decode returns -1, you get the sym, then call update_escape :
	void update_escape(int sym)
	{
		RR_ASSERT( sym >= 0 );
		
		// insert last, mid, head?

		// insert one ahead of last :
		// 79.7 :
		m_syms[t_numsyms-1] = m_syms[t_numsyms-2];
		m_syms[t_numsyms-2] = (t_sym) sym;
	}
	
	//===========================================
	
	bool encode_noadapt(rrArithEncoder *ac, int sym) const
	{
		//SIMPLEPROFILE_SCOPE(SRCoder_encode_noadapt);
	
		for LOOP(s,t_numsyms)
		{
			if ( sym == m_syms[s] )
			{
				rrArithBinaryModelEncodeNoAdapt(ac,1,SRCODER_ANYMATCH);
				
				for LOOP(pi,s)
				{
					rrArithBinaryModelEncodeNoAdapt(ac,0,m_probs[pi]);
				}
				if ( s < t_numsyms-1 )		
				{
					rrArithBinaryModelEncodeNoAdapt(ac,1,m_probs[s]);
				}
				
				//update_found(s);
				return true;
			}
		}
		
		rrArithBinaryModelEncodeNoAdapt(ac,0,SRCODER_ANYMATCH);
				
		return false;
	}
	
	int decode_noadapt(rrArithDecoder * ac) const
	{
		if ( rrArithBinaryModelDecodeNoAdapt(ac,SRCODER_ANYMATCH) )
		{		
			for LOOP(s,t_numsyms-1)
			{
				if ( rrArithBinaryModelDecodeNoAdapt(ac,m_probs[s]) )
				{
					int ret = m_syms[s];
					//update_found(s);
					return ret;
				}
			}
			
			int ret = m_syms[t_numsyms-1];
			//update_found(t_numsyms-1);
			return ret;			
		}
				
		return -1;	
	}
	
	bool encode_withexclude_noadapt(rrArithEncoder *ac, int sym,int * pexc) const
	{
		//SIMPLEPROFILE_SCOPE(SRCoder_encode_withexc_noadapt);
		
		int exc = *pexc;
		RR_ASSERT( sym != exc );
	
		for LOOP(s,t_numsyms)
		{			
			if ( sym == m_syms[s] )
			{
				rrArithBinaryModelEncodeNoAdapt(ac,1,SRCODER_ANYMATCH);
				
				// do not have to set *pexc cuz I did the coding
				for LOOP(pi,s)
				{
					if ( m_syms[pi] == exc )
					{
						continue;
					}
					rrArithBinaryModelEncodeNoAdapt(ac,0,m_probs[pi]);
				}
				if ( s < t_numsyms-1 )		
				{
					rrArithBinaryModelEncodeNoAdapt(ac,1,m_probs[s]);
				}
				
				//update_found(s);
				return true;
			}
		}
		
		// set *pexc
		for LOOP(s,t_numsyms)
		{
			if ( m_syms[s] == exc )
			{
				*pexc = -1;
				break;
			}
		}
		
		rrArithBinaryModelEncodeNoAdapt(ac,0,SRCODER_ANYMATCH);
									
		return false;
	}
	
	int decode_withexclude_noadapt(rrArithDecoder * ac,int * pexc) const
	{
		int exc = *pexc;
		
		if ( rrArithBinaryModelDecodeNoAdapt(ac,SRCODER_ANYMATCH) )
		{	
			// do not need to set pexc
			// cuz I do the coding
					
			for LOOP(s,t_numsyms-1)
			{			
				if ( m_syms[s] == exc )
					continue;
				
				if ( rrArithBinaryModelDecodeNoAdapt(ac,m_probs[s]) )
				{
					int ret = m_syms[s];
					//update_found(s);
					return ret;
				}
			}
			
			int ret = m_syms[t_numsyms-1];
			//update_found(t_numsyms-1);
			return ret;			
		}
		else
		{
			// have to check pexc
			
			for LOOP(s,t_numsyms)
			{
				if ( m_syms[s] == exc )
				{
					*pexc = -1;
					continue;
				}
			}
		}
						
		return -1;	
	}
};

template <int t_numsyms, typename t_sym>
struct SRCoder_Const
{
	// BitFlags256 is for a quick test to see if a symbol is in the SR at all
	BitFlags256 m_flags;
	rrArithBinaryModel m_probs[t_numsyms];
	t_sym m_syms[t_numsyms];
	
	#define SRCODER_ANYMATCH		m_probs[t_numsyms-1]
	#define SRCODER_ANYMATCH_PTR	&(SRCODER_ANYMATCH)
		
	SRCoder_Const()
	{
		init();
	}	
	
	void init()
	{
		// invalidate
		SRCODER_ANYMATCH = 0;
	}
	
	void set_from_histo(const U32 * histo, int histo_size)
	{
		m_flags.init();
	
		histo_to_sr::sym_and_count syms[t_numsyms];
		U32 tot = histo_to_sr::sort_histo(syms,t_numsyms,histo,histo_size);
	
		U32 sum_in_sr = 0;
		for LOOP(i,t_numsyms)
		{
			if ( syms[i].count == 0 )
			{
				// zeros don't work
				syms[i].count ++;
				tot ++;
			}
			m_syms[i] = (t_sym) syms[i].sym;
			sum_in_sr += syms[i].count;
			
			m_flags.set( syms[i].sym );
		}
		
		RR_ASSERT( sum_in_sr >= t_numsyms );
		RR_ASSERT( tot >= sum_in_sr );
	
		U32 Prob_NotSR = rr_froundint( RR_BINARY_MODEL_TOT * (F32)(tot+1 - sum_in_sr) / (tot+2) );
		Prob_NotSR = RR_CLAMP(Prob_NotSR,32,RR_BINARY_MODEL_TOT-32);
	
		*(SRCODER_ANYMATCH_PTR) = rrArithBinaryModel_Check( Prob_NotSR );
		
		
		for LOOP(i,t_numsyms-1)
		{
			// Pme = syms[i].count / sum_in_sr
			// 1 means its me, so 0 = P of not me
			
			RR_ASSERT( sum_in_sr >= 2 );
			RR_ASSERT( syms[i].count >= 1 );
			RR_ASSERT( sum_in_sr > syms[i].count );
			
			U32 PNotMe = rr_froundint( RR_BINARY_MODEL_TOT * (F32)(sum_in_sr - syms[i].count + 1) / (sum_in_sr + 1) );
			PNotMe = RR_CLAMP(PNotMe,32,RR_BINARY_MODEL_TOT-32);
			m_probs[i] = rrArithBinaryModel_Check( PNotMe );
			// make sum_in_sr always the sum of the remainder
			sum_in_sr -= syms[i].count;
		}
		
		RR_ASSERT( *(SRCODER_ANYMATCH_PTR) == Prob_NotSR ); 
	}
	
	void update_found(int slot)
	{
		RR_ASSERT_FAILURE_ALWAYS("bad");
	}
	
	bool encode(rrArithEncoder *ac, int sym) const
	{
		if ( m_flags.test(sym) )
		{
			SIMPLEPROFILE_SCOPE(SRCoder_encode_1);
			
			for LOOP(s,t_numsyms)
			{
				if ( sym == m_syms[s] )
				{
					rrArithBinaryModelEncodeNoAdapt(ac,1,SRCODER_ANYMATCH);
					
					for LOOP(pi,s)
					{
						rrArithBinaryModelEncodeNoAdapt(ac,0,m_probs[pi]);
					}
					if ( s < t_numsyms-1 )		
					{
						rrArithBinaryModelEncodeNoAdapt(ac,1,m_probs[s]);
					}
					return true;
				}
			}
			
			RR_CANT_GET_HERE();
		}
		else
		{
			SIMPLEPROFILE_SCOPE(SRCoder_encode_0);		
		
			rrArithBinaryModelEncodeNoAdapt(ac,0,SRCODER_ANYMATCH);
				
			return false;
		}	
	}
	
	int decode(rrArithDecoder * ac) const
	{
		if ( rrArithBinaryModelDecodeNoAdapt(ac,SRCODER_ANYMATCH) )
		{		
			for LOOP(s,t_numsyms-1)
			{
				if ( rrArithBinaryModelDecodeNoAdapt(ac,m_probs[s]) )
				{
					int ret = m_syms[s];
					return ret;
				}
			}
			
			int ret = m_syms[t_numsyms-1];
			return ret;			
		}
				
		return -1;	
	}
	
	bool encode_withexclude(rrArithEncoder *ac, int sym,int * pexc) const
	{
		int exc = *pexc;
		RR_ASSERT( sym != exc );
			
		if ( m_flags.test(sym) )
		{
			SIMPLEPROFILE_SCOPE(SRCoder_encode_1_exc);
			
			for LOOP(s,t_numsyms)
			{
				if ( sym == m_syms[s] )
				{
					rrArithBinaryModelEncodeNoAdapt(ac,1,SRCODER_ANYMATCH);
					
					// do not have to set *pexc cuz I did the coding
					for LOOP(pi,s)
					{
						if ( m_syms[pi] == exc )
						{
							continue;
						}
						rrArithBinaryModelEncodeNoAdapt(ac,0,m_probs[pi]);
					}
					if ( s < t_numsyms-1 )		
					{
						rrArithBinaryModelEncodeNoAdapt(ac,1,m_probs[s]);
					}
					
					return true;
				}
			}
			
			RR_CANT_GET_HERE();
		}
		else
		{
			SIMPLEPROFILE_SCOPE(SRCoder_encode_0_exc);

			// set *pexc
			if ( m_flags.test(exc) )
			{
				*pexc = -1;
			}
			
			rrArithBinaryModelEncodeNoAdapt(ac,0,SRCODER_ANYMATCH);
										
			return false;
		}
	}
	
	int decode_withexclude(rrArithDecoder * ac,int * pexc) const
	{
		int exc = *pexc;
		
		if ( rrArithBinaryModelDecodeNoAdapt(ac,SRCODER_ANYMATCH) )
		{	
			// do not need to set pexc
			// cuz I do the coding
					
			for LOOP(s,t_numsyms-1)
			{			
				if ( m_syms[s] == exc )
					continue;
				
				if ( rrArithBinaryModelDecodeNoAdapt(ac,m_probs[s]) )
				{
					int ret = m_syms[s];
					return ret;
				}
			}
			
			int ret = m_syms[t_numsyms-1];
			return ret;			
		}
		else
		{
			// set *pexc
			if ( m_flags.test(exc) )
			{
				*pexc = -1;
			}
			return -1;	
		}
	}
	
	// after decode returns -1, you get the sym, then call update_escape :
	void update_escape(int sym)
	{
		RR_ASSERT_FAILURE_ALWAYS("bad");
	}
	
	//===========================================
	
	bool encode_noadapt(rrArithEncoder *ac, int sym) const
	{
		return encode(ac,sym);
	}
	
	int decode_noadapt(rrArithDecoder * ac) const
	{
		return decode(ac);
	}
	
	bool encode_withexclude_noadapt(rrArithEncoder *ac, int sym,int * pexc) const
	{
		return encode_withexclude(ac,sym,pexc);
	}
	
	int decode_withexclude_noadapt(rrArithDecoder * ac,int * pexc) const
	{
		return decode_withexclude(ac,pexc);
	}
};

/***********

Faster SymbolRank :
	keep 4 symbols in a dword
	do a check for "is this symbol any of the four"
	replicate symbol to all 4 byte poses
	xor
	do that "is any byte here a zero" check
	
***********/

struct SRCoder4U8
{
	enum { t_numsyms = 4 };

	U32 m_foursyms;
	rrArithBinaryModel m_probany;
	rrArithBinaryModel m_probs[3];
	
	SRCoder4U8()
	{
		init();
	}	
	
	int getsym(int index) const
	{
		return ( m_foursyms >> (index*8) ) & 0xFF;
	}
	
	void init()
	{
		m_probany = RR_BINARY_MODEL_INIT_VAL;
		for LOOP(s,3)
		{
			m_probs[s] = RR_BINARY_MODEL_TOT - (RR_BINARY_MODEL_TOT/256);
		}
		m_foursyms = 0x00FF200A; // @@ ?? initial symbols ?
	}
	
	void set_from_histo(const U32 * histo, int histo_size)
	{
		SRCoder<4,U8> src;
		src.set_from_histo(histo,histo_size);
		
		m_foursyms = 
			(src.m_syms[0]) |
			(src.m_syms[1]<<8) |
			(src.m_syms[2]<<16) |
			(src.m_syms[3]<<24);

		m_probany = src.m_probs[3];
		m_probs[0] = src.m_probs[0];
		m_probs[1] = src.m_probs[1];
		m_probs[2] = src.m_probs[2];
	}
	
	void update_found(int slot)
	{
		U32 in = m_foursyms;
		
		// MTF :
		switch(slot)
		{
		case 0:
			return;
		
		case 1: // ABCD -> BACD
			{
			U32 up = in << 8 ;
			U32 dn = in >> 8 ;
			
			U32 out = (in & 0xFFFF0000) | (up & 0xFF00) | (dn & 0x00FF);
			
			m_foursyms = out;
			return;
			}
		
		case 2: // ABCD -> CABD
			{
			U32 up = in << 8 ;
			U32 dn = in >> 16;
			
			U32 out = (in & 0xFF000000) | (up & 0x00FFFF00) | (dn & 0x00FF);
			
			m_foursyms = out;
			return;
			}
		
		case 3: // ABCD -> DABC
			{
			m_foursyms = RR_ROTL32(in,8);
			return;
			}
			
		RR_NO_DEFAULT_CASE
		}
	}
	
	bool encode(rrArithEncoder *ac, int sym)
	{
		RR_ASSERT( sym < 256 );
		// replicate byte to dword :
		U32 dw = (sym<<16) | (sym);
		dw |= (dw<<8);
		U32 test = dw ^ m_foursyms;
		if ( RR_U32_HAS_ZERO_BYTE(test) )
		{
			// match somewhere 
			
			rrArithBinaryModelEncode(ac,1,&m_probany);
			
			// @@ find the zero byte?
		
			for LOOP(s,3)
			{
				if ( sym == getsym(s) )
				{
					rrArithBinaryModelEncode(ac,1,&m_probs[s]);
					update_found(s);
					return true;
				}
				else
				{
					rrArithBinaryModelEncode(ac,0,&m_probs[s]);
				}
			}
			
			RR_ASSERT( sym == getsym(3) );
			update_found(3);
			return true;
		}
		else
		{
			// no match

			rrArithBinaryModelEncode(ac,0,&m_probany);
			return false;
		}
	}
	
	int decode(rrArithDecoder * ac)
	{
		if ( rrArithBinaryModelDecode(ac,&m_probany) )
		{			
			for LOOP(s,3)
			{
				if ( rrArithBinaryModelDecode(ac,&m_probs[s]) )
				{
					int ret = getsym(s);
					update_found(s);
					return ret;
				}
			}
			
			int ret = getsym(3);
			update_found(3);
			return ret;
		}
		else
		{
			return -1;	
		}		
	}
	
	bool encode_withexclude(rrArithEncoder *ac, int sym,int * pexc)
	{
		int exc = *pexc;
		RR_ASSERT( sym != exc );
		
		RR_ASSERT( sym < 256 );
		// replicate byte to dword :
		U32 sym_dw = (sym<<16) | (sym);
		sym_dw |= (sym_dw<<8);
		U32 sym_test = sym_dw ^ m_foursyms;
		
		U32 exc_dw = (exc<<16) | (exc);
		exc_dw |= (exc_dw<<8);
		U32 exc_test = exc_dw ^ m_foursyms;
		
		if ( RR_U32_HAS_ZERO_BYTE(sym_test) )
		{
			// match somewhere 
			
			rrArithBinaryModelEncode(ac,1,&m_probany);
								
			for LOOP(s,3)
			{
				int cur = getsym(s);
				if ( cur == sym )
				{
					rrArithBinaryModelEncode(ac,1,&m_probs[s]);
					update_found(s);
					return true;
				}
				else if ( cur == exc )
				{
					continue;
				}
				else
				{
					rrArithBinaryModelEncode(ac,0,&m_probs[s]);
				}
			}
			
			RR_ASSERT( sym == getsym(3) );
			update_found(3);
			return true;
		}
		else
		{
			 // no match
			 
			rrArithBinaryModelEncode(ac,0,&m_probany);
			
			 // 
			if ( RR_U32_HAS_ZERO_BYTE(exc_test) )
			{
				// exclude somewhere
				*pexc = -1;
			}
			 
			return false;
		}
	}
	
	int decode_withexclude(rrArithDecoder * ac,int * pexc)
	{
		int exc = *pexc;
		
		U32 exc_dw = (exc<<16) | (exc);
		exc_dw |= (exc_dw<<8);
		U32 exc_test = exc_dw ^ m_foursyms;
		
		if ( rrArithBinaryModelDecode(ac,&m_probany) )
		{	
			for LOOP(s,3)
			{
				if ( getsym(s) == exc )
				{
					continue;
				}
				
				if ( rrArithBinaryModelDecode(ac,&m_probs[s]) )
				{
					int ret = getsym(s);
					update_found(s);
					return ret;
				}
			}
			
			int ret = getsym(3);
			update_found(3);
			return ret;
		}
		else
		{
			if ( RR_U32_HAS_ZERO_BYTE(exc_test) )
			{
				// exclude somewhere
				*pexc = -1;
			}
		}
		
		return -1;
	}
	
	bool encode_withexcludes(rrArithEncoder *ac, int sym,int * pexc1,int * pexc2)
	{
		int exc1 = *pexc1;
		RR_ASSERT( sym != exc1 );
		int exc2 = *pexc2;
		RR_ASSERT( sym != exc2 );
		RR_ASSERT( exc1 != exc2 && exc1 >= 0 && exc2 >= 0 );
		
		RR_ASSERT( sym < 256 );
		// replicate byte to dword :
		U32 sym_dw = (sym<<16) | (sym);
		sym_dw |= (sym_dw<<8);
		U32 sym_test = sym_dw ^ m_foursyms;
		
		U32 exc1_dw = (exc1<<16) | (exc1);
		exc1_dw |= (exc1_dw<<8);
		U32 exc1_test = exc1_dw ^ m_foursyms;
		
		U32 exc2_dw = (exc2<<16) | (exc2);
		exc2_dw |= (exc2_dw<<8);
		U32 exc2_test = exc2_dw ^ m_foursyms;
		
		if ( RR_U32_HAS_ZERO_BYTE(sym_test) )
		{
			// match somewhere 
			
			rrArithBinaryModelEncode(ac,1,&m_probany);
								
			for LOOP(s,3)
			{
				int cur = getsym(s);
				if ( cur == sym )
				{
					rrArithBinaryModelEncode(ac,1,&m_probs[s]);
					update_found(s);
					return true;
				}
				else if ( cur == exc1 || cur == exc2 )
				{
					continue;
				}
				else
				{
					rrArithBinaryModelEncode(ac,0,&m_probs[s]);
				}
			}
			
			RR_ASSERT( sym == getsym(3) );
			update_found(3);
			return true;
		}
		else
		{
			 // no match
			 
			rrArithBinaryModelEncode(ac,0,&m_probany);
			
			 // 
			if ( RR_U32_HAS_ZERO_BYTE(exc1_test) )
			{
				// exclude somewhere
				*pexc1 = -1;
			}
			if ( RR_U32_HAS_ZERO_BYTE(exc2_test) )
			{
				// exclude somewhere
				*pexc2 = -1;
			}
			 
			return false;
		}
	}
	
	int decode_withexcludes(rrArithDecoder * ac,int * pexc1,int * pexc2)
	{
		int exc1 = *pexc1;
		U32 exc1_dw = (exc1<<16) | (exc1);
		exc1_dw |= (exc1_dw<<8);
		U32 exc1_test = exc1_dw ^ m_foursyms;
		
		int exc2 = *pexc2;
		U32 exc2_dw = (exc2<<16) | (exc2);
		exc2_dw |= (exc2_dw<<8);
		U32 exc2_test = exc2_dw ^ m_foursyms;
		
		RR_ASSERT( exc1 != exc2 && exc1 >= 0 && exc2 >= 0 );
		
		if ( rrArithBinaryModelDecode(ac,&m_probany) )
		{	
			for LOOP(s,3)
			{
				int sym = getsym(s);
				if ( sym == exc1 || sym == exc2 )
				{
					continue;
				}
				
				if ( rrArithBinaryModelDecode(ac,&m_probs[s]) )
				{
					update_found(s);
					return sym;
				}
			}
			
			int ret = getsym(3);
			RR_ASSERT( ret != exc1 && ret != exc2 );
			update_found(3);
			return ret;
		}
		else
		{
			if ( RR_U32_HAS_ZERO_BYTE(exc1_test) )
			{
				// exclude somewhere
				*pexc1 = -1;
			}
			if ( RR_U32_HAS_ZERO_BYTE(exc2_test) )
			{
				// exclude somewhere
				*pexc2 = -1;
			}
		}
		
		return -1;
	}
	
	// after decode returns -1, you get the sym, then call update_escape :
	void update_escape(int sym)
	{
		#if 0
		// insert one ahead of last :
		// 79.7 :
		m_syms[t_numsyms-1] = m_syms[t_numsyms-2];
		m_syms[t_numsyms-2] = sym;
		#endif

		RR_ASSERT( sym >= 0 && sym < 256 );

		U32 in = m_foursyms;
		U32 up = in << 8 ;
		U32 out = (in & 0x0000FFFF) | (sym << 16) | (up & 0xFF000000);
			
		m_foursyms = out;
		
		RR_ASSERT( getsym(t_numsyms-2) == sym );
	}
	
	//=====================================================
	
	bool encode_noadapt(rrArithEncoder *ac, int sym) const
	{
		RR_ASSERT( sym < 256 );
		// replicate byte to dword :
		U32 dw = (sym<<16) | (sym);
		dw |= (dw<<8);
		U32 test = dw ^ m_foursyms;
		if ( RR_U32_HAS_ZERO_BYTE(test) )
		{
			// match somewhere 
			
			rrArithBinaryModelEncodeNoAdapt(ac,1,m_probany);
			
			// @@ faster to find the zero byte in "test" somehow ?
		
			for LOOP(s,3)
			{
				if ( sym == getsym(s) )
				{
					rrArithBinaryModelEncodeNoAdapt(ac,1,m_probs[s]);
					//update_found(s);
					return true;
				}
				else
				{
					rrArithBinaryModelEncodeNoAdapt(ac,0,m_probs[s]);
				}
			}
			
			RR_ASSERT( sym == getsym(3) );
			//update_found(3);
			return true;
		}
		else
		{
			// no match

			rrArithBinaryModelEncodeNoAdapt(ac,0,m_probany);
			return false;
		}
	}
	
	int decode_noadapt(rrArithDecoder * ac) const
	{
		if ( rrArithBinaryModelDecodeNoAdapt(ac,m_probany) )
		{			
			for LOOP(s,3)
			{
				if ( rrArithBinaryModelDecodeNoAdapt(ac,m_probs[s]) )
				{
					int ret = getsym(s);
					//update_found(s);
					return ret;
				}
			}
			
			int ret = getsym(3);
			//update_found(3);
			return ret;
		}
		else
		{
			return -1;	
		}		
	}
	
	bool encode_withexclude_noadapt(rrArithEncoder *ac, int sym,int * pexc) const
	{
		int exc = *pexc;
		RR_ASSERT( sym != exc );
		
		RR_ASSERT( sym < 256 );
		// replicate byte to dword :
		U32 sym_dw = (sym<<16) | (sym);
		sym_dw |= (sym_dw<<8);
		U32 sym_test = sym_dw ^ m_foursyms;
		
		U32 exc_dw = (exc<<16) | (exc);
		exc_dw |= (exc_dw<<8);
		U32 exc_test = exc_dw ^ m_foursyms;
		
		if ( RR_U32_HAS_ZERO_BYTE(sym_test) )
		{
			// match somewhere 
			
			rrArithBinaryModelEncodeNoAdapt(ac,1,m_probany);
								
			for LOOP(s,3)
			{
				int cur = getsym(s);
				if ( cur == sym )
				{
					rrArithBinaryModelEncodeNoAdapt(ac,1,m_probs[s]);
					//update_found(s);
					return true;
				}
				else if ( cur == exc )
				{
					continue;
				}
				else
				{
					rrArithBinaryModelEncodeNoAdapt(ac,0,m_probs[s]);
				}
			}
			
			RR_ASSERT( sym == getsym(3) );
			//update_found(3);
			return true;
		}
		else
		{
			 // no match
			 
			rrArithBinaryModelEncodeNoAdapt(ac,0,m_probany);
			
			 // 
			if ( RR_U32_HAS_ZERO_BYTE(exc_test) )
			{
				// exclude somewhere
				*pexc = -1;
			}
			 
			return false;
		}
	}
	
	int decode_withexclude_noadapt(rrArithDecoder * ac,int * pexc) const
	{
		int exc = *pexc;
		
		U32 exc_dw = (exc<<16) | (exc);
		exc_dw |= (exc_dw<<8);
		U32 exc_test = exc_dw ^ m_foursyms;
		
		if ( rrArithBinaryModelDecodeNoAdapt(ac,m_probany) )
		{	
			for LOOP(s,3)
			{
				if ( getsym(s) == exc )
				{
					continue;
				}
				
				if ( rrArithBinaryModelDecodeNoAdapt(ac,m_probs[s]) )
				{
					int ret = getsym(s);
					//update_found(s);
					return ret;
				}
			}
			
			int ret = getsym(3);
			//update_found(3);
			return ret;
		}
		else
		{
			if ( RR_U32_HAS_ZERO_BYTE(exc_test) )
			{
				// exclude somewhere
				*pexc = -1;
			}
		}
		
		return -1;
	}
	
	bool encode_withexcludes_noadapt(rrArithEncoder *ac, int sym,int * pexc1,int * pexc2) const
	{
		int exc1 = *pexc1;
		RR_ASSERT( sym != exc1 );
		int exc2 = *pexc2;
		RR_ASSERT( sym != exc2 );
		RR_ASSERT( exc1 != exc2 && exc1 >= 0 && exc2 >= 0 );
		
		RR_ASSERT( sym < 256 );
		// replicate byte to dword :
		U32 sym_dw = (sym<<16) | (sym);
		sym_dw |= (sym_dw<<8);
		U32 sym_test = sym_dw ^ m_foursyms;
		
		U32 exc1_dw = (exc1<<16) | (exc1);
		exc1_dw |= (exc1_dw<<8);
		U32 exc1_test = exc1_dw ^ m_foursyms;
		
		U32 exc2_dw = (exc2<<16) | (exc2);
		exc2_dw |= (exc2_dw<<8);
		U32 exc2_test = exc2_dw ^ m_foursyms;
		
		if ( RR_U32_HAS_ZERO_BYTE(sym_test) )
		{
			// match somewhere 
			
			rrArithBinaryModelEncodeNoAdapt(ac,1,m_probany);
								
			for LOOP(s,3)
			{
				int cur = getsym(s);
				if ( cur == sym )
				{
					rrArithBinaryModelEncodeNoAdapt(ac,1,m_probs[s]);
					return true;
				}
				else if ( cur == exc1 || cur == exc2 )
				{
					continue;
				}
				else
				{
					rrArithBinaryModelEncodeNoAdapt(ac,0,m_probs[s]);
				}
			}
			
			RR_ASSERT( sym == getsym(3) );
			return true;
		}
		else
		{
			 // no match
			 
			rrArithBinaryModelEncodeNoAdapt(ac,0,m_probany);
			
			 // 
			if ( RR_U32_HAS_ZERO_BYTE(exc1_test) )
			{
				// exclude somewhere
				*pexc1 = -1;
			}
			if ( RR_U32_HAS_ZERO_BYTE(exc2_test) )
			{
				// exclude somewhere
				*pexc2 = -1;
			}
			 
			return false;
		}
	}
	
	int decode_withexcludes_noadapt(rrArithDecoder * ac,int * pexc1,int * pexc2) const
	{
		int exc1 = *pexc1;
		U32 exc1_dw = (exc1<<16) | (exc1);
		exc1_dw |= (exc1_dw<<8);
		U32 exc1_test = exc1_dw ^ m_foursyms;
		
		int exc2 = *pexc2;
		U32 exc2_dw = (exc2<<16) | (exc2);
		exc2_dw |= (exc2_dw<<8);
		U32 exc2_test = exc2_dw ^ m_foursyms;
		
		RR_ASSERT( exc1 != exc2 && exc1 >= 0 && exc2 >= 0 );
		
		if ( rrArithBinaryModelDecodeNoAdapt(ac,m_probany) )
		{	
			for LOOP(s,3)
			{
				int sym = getsym(s);
				if ( sym == exc1 || sym == exc2 )
				{
					continue;
				}
				
				if ( rrArithBinaryModelDecodeNoAdapt(ac,m_probs[s]) )
				{
					//update_found(s);
					return sym;
				}
			}
			
			int ret = getsym(3);
			RR_ASSERT( ret != exc1 && ret != exc2 );
			//update_found(3);
			return ret;
		}
		else
		{
			if ( RR_U32_HAS_ZERO_BYTE(exc1_test) )
			{
				// exclude somewhere
				*pexc1 = -1;
			}
			if ( RR_U32_HAS_ZERO_BYTE(exc2_test) )
			{
				// exclude somewhere
				*pexc2 = -1;
			}
		}
		
		return -1;
	}
};

OODLE_NS_END
