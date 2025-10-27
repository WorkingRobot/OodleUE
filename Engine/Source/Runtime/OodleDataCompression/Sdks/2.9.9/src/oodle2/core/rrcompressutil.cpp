// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrcompressutil.h"
#include "rrlogutil.h"
#include "rrmath.h"
#include "log2table.h"
#include "cbradutil.h"
#include "templates/rrstl.h"
#include "templates/rrvector.h"
#include "templates/rralgorithm.h"
#include "rrstackarray.h"
#include "rrvarbitcodes.h"

OODLE_NS_START

#include "eastl_heap.h"

//===============================================================================

// 18,446,744,073,709,550,000

/*
 * you have to drop a \n on the string yourself
 *
 */
void rrLogCompression(S64 UnPackedLen,S64 PackedLen)
{
	RR_ASSERT( UnPackedLen >= 0 && PackedLen >= 0 );
	if ( UnPackedLen <= 0 || PackedLen <= 0 )
	{
		rrprintf("%10" RR_S64_FMT " -> %10" RR_S64_FMT " (abnormal)", UnPackedLen,PackedLen);
		return;
	}

	U32 BPB,BPBdec;
	U32 Ratio,Ratiodec;
	
	F32 BPBF = (F32) ( PackedLen * 8.0 / UnPackedLen );

	BPB = (U32) rr_froundint( BPBF * 1000 );
	BPBdec = BPB - (BPB/1000)*1000;
	BPB /= 1000;

	Ratio = (U32) rr_froundint((F32)(UnPackedLen * 1000.0 / PackedLen)); 
	Ratiodec = Ratio - (Ratio/1000)*1000;
	Ratio /= 1000;

	//rrprintf(" %7lu -> %7lu = %2u.%03u bpb = %2u.%03u to 1 ",
	//	UnPackedLen,PackedLen,BPB,BPBdec,Ratio,Ratiodec);

	char l1[60];
	char l2[60];
	rrsprintfcommas(l1,UnPackedLen);
	rrsprintfcommas(l2,PackedLen);

	rrprintf("%10s ->%10s = %2u.%03u bpb = %2u.%03u to 1 ",
		l1,l2,BPB,BPBdec,Ratio,Ratiodec);
}

struct sort_value
{
	U32 value;
	int index;
};

bool operator < (const sort_value & lhs, const sort_value & rhs)
{
	return lhs.value > rhs.value; // highest value!
}

void rrLogHighestValues(const U32 * values,int value_count,int num_to_log)
{
	vector<sort_value> sort_values;
	sort_values.resize(value_count);
	for LOOP(i,value_count)
	{
		sort_values[i].value = values[i];
		sort_values[i].index = i;
	}

	//partial_sort(sort_values.begin(),sort_values.begin()+num_to_log,sort_values.begin()+value_count);
	// screw it, sort 'em all, I don't have partial_sort now :
	stdsort(sort_values.begin(),sort_values.end());

	for LOOP(i,num_to_log)
	{
		rrprintf("%2d: %4d: %d\n",i,sort_values[i].index,sort_values[i].value);
	}
}

SINTa rrCountNumSame(const void * buf1,const void * buf2,SINTa size)
{
	const U8 * ptr1 = (const U8 *) buf1;
	const U8 * ptr2 = (const U8 *) buf2;

	RR_ASSERT_ALWAYS( ptr1 && ptr2 );
	
	SINTa numSame = 0;
	for(;numSame < size;numSame++)
	{
		if ( ptr1[numSame] != ptr2[numSame] )
			break;
	}	
	
	return numSame;
}

rrbool rrCheckBuffersSame(const void * buf1,const void * buf2,SINTa size)
{
	SINTa sameLen = rrCountNumSame(buf1,buf2,size);
	
	if ( sameLen < size )
		rrPrintf_v0("same : %d/%d\n",(int)sameLen,(int)size);
	
	RR_ASSERT_ALWAYS( sameLen == size );
	return sameLen == size;
}


//===============================================================================

struct sort_sym
{
	S32 sym;
	F32 rank;
};

bool operator < (const sort_sym & lhs, const sort_sym & rhs)
{
	// legacy way :
	// Oodle Network 2.6.0 - 2.8.11 behavior is to have NO explicit order here for ties
	//	with the new heap
	// Oodle Network <= 2.5.5 also had no explicit order but used the old EASTL heap
	//	

	#if 0
	// @@@@
	// proposed new way for Oodle 2.9.x :
	//	explicitly order tied ranks so we get deterministic behavior
	if ( lhs.rank == rhs.rank )
 		return lhs.sym < rhs.sym;
	#endif

	return lhs.rank < rhs.rank;
}


static const int c_logoneplusinv_size = 32;
static const F32 c_logoneplusinv[] = 
{
    0.000000f,  0.693147f,  0.405465f,  0.287682f,
    0.223144f,  0.182322f,  0.154151f,  0.133531f,
    0.117783f,  0.105361f,  0.095310f,  0.087011f,
    0.080043f,  0.074108f,  0.068993f,  0.064539f,
    0.060625f,  0.057158f,  0.054067f,  0.051293f,
    0.048790f,  0.046520f,  0.044452f,  0.042560f,
    0.040822f,  0.039221f,  0.037740f,  0.036368f,
    0.035091f,  0.033902f,  0.032790f,  0.031749f
};

static const int c_logoneminusinv_size = 32;
static const F32 c_logoneminusinv[] = 
{
    0.000000f,  0.000000f, -0.693147f, -0.405465f,
   -0.287682f, -0.223144f, -0.182322f, -0.154151f,
   -0.133531f, -0.117783f, -0.105361f, -0.095310f,
   -0.087011f, -0.080043f, -0.074108f, -0.068993f,
   -0.064539f, -0.060625f, -0.057158f, -0.054067f,
   -0.051293f, -0.048790f, -0.046520f, -0.044452f,
   -0.042560f, -0.040822f, -0.039221f, -0.037740f,
   -0.036368f, -0.035091f, -0.033902f, -0.032790f
};

// log(1 + 1/i)
// log(1+x)=x - x2/2 + x3/3 - x4/4 + ....
static RADINLINE F32 logoneplusinv(int i)
{
	RR_ASSERT( i >= 1 );
	if ( i < (int)RR_ARRAY_SIZE(c_logoneplusinv) )
		return c_logoneplusinv[i];
	
	F32 x = 1.f/i;
	return x - x*x*0.5f;
}

// log(1 - 1/i)
// log(1-x)=-x - x2/2 - x3/3 - x4/4 + ....
static RADINLINE F32 logoneminusinv(int i)
{
	RR_ASSERT( i >= 2 );
	if ( i < (int)RR_ARRAY_SIZE(c_logoneminusinv) )
		return c_logoneminusinv[i];
	
	F32 x = 1.f/i;
	return - x - x*x*0.5f;
}

/**

WARNING:
normalize_counts is used by Oodle Network Compact/Uncompact
it cannot change behavior!
same inputs must produce same outputs

normalize_counts changed from Oodle 2.5.5 to 2.6.0
when the heap changed (removed EASTL)
because sort_sym wasn't breaking rank ties explicitly

**/
S32 normalize_counts_v6(U32 * to, int to_sum_desired, const U32 * from, int from_sum, int alphabet)
{
	RR_ASSERT( to != from );
	
	RR_ASSERT_ALWAYS( from_sum > 0 );
	
	RR_DURING_ASSERT( U32 from_sum_check = sum(from,from+alphabet) );
	RR_ASSERT( (U32)from_sum == from_sum_check );
		
	S32 num_non_zero = 0;	
	U32 to_sum = 0;

	#if 1 // @@@@

	// legacy way
	// to make sure we match old Oodle Network Compact/Uncompact behavior :

	double scale = (double)to_sum_desired / from_sum;

	for LOOP(i,alphabet)
	{
		if ( from[i] == 0 )
		{
			to[i] = 0;
		}
		else
		{
			// @@ certainly could use fixed point here instead of floats
			//	not sure there's any reason for that though
		
			double from_scaled = from[i] * scale;
			U32 down = (U32)( from_scaled );
			
			// to[i] = down + (down*(2*frac-1)+frac^2 > 0)
			
			//to[i] = ( from_scaled*from_scaled < down*(down+1) ) ? down : down+1;
			to[i] = down + ( from_scaled*from_scaled > down*(down+1) );
			
			RR_ASSERT( to[i] > 0 );
			to_sum += to[i];
			
			num_non_zero++;
		}
	}

	#else
	
	// make sure from * to count fits in U32 :
	RR_ASSERT( ((U64)from_sum * to_sum_desired) < (RR_ONE_U64<<32) );

	// proposed new way for Oodle 2.9.x using ints
	//	to make sure the results are deterministic
	// same using ints :

	U64 to_sum_desired_sqr = (U64)to_sum_desired*to_sum_desired;
	U64 from_sum_sqr = (U64)from_sum*from_sum;

	for LOOP(i,alphabet)
	{
		// this phase is independent on each lane, could be SIMD N-wide

		if ( from[i] == 0 )
		{
			to[i] = 0;
		}
		else
		{		
			U32 fromc = from[i];
			U32 down = (U32)( (fromc * to_sum_desired) / from_sum );
			
			// U64 muls to make sure they fit :
			to[i] = down + ( fromc*fromc*to_sum_desired_sqr > down*(down+1)*from_sum_sqr );

			RR_ASSERT( to[i] > 0 );
			to_sum += to[i];
			
			num_non_zero++;
		}
	}
	
	#endif

	S32 correction = to_sum_desired - to_sum;

	//rrprintfvar(correction);

	if ( correction != 0 )
	{
		RR_STACK_ARRAY(heap,sort_sym,alphabet);
		SINTa heap_size = 0;

		if ( correction > 0 )
		{
			for LOOP(i,alphabet)
			{
				if ( from[i] == 0 ) continue;
				RR_ASSERT( to[i] != 0 );

				// when to[i] is large you could use log(1+x) taylor series here
				// could just have a table when to[i] is small
				float change = logoneplusinv(to[i]) * from[i];
			
				heap[heap_size].sym = i;
				heap[heap_size].rank = change;
				heap_size++;
			}
		}
		else
		{
			RR_ASSERT( correction < 0 );
			for LOOP(i,alphabet)
			{
				if ( from[i] == 0 ) continue;
				RR_ASSERT( to[i] != 0 );
				if ( to[i] > 1 )
				{
					// when to[i] is large you could use log(1+x) taylor series here
					// could just have a table when to[i] is small
					float change = logoneminusinv(to[i]) * from[i];
				
					heap[heap_size].sym = i;
					heap[heap_size].rank = change;
					heap_size++;
				}
			}
		}
			
		make_heap(heap,heap+heap_size);
			
		if ( correction > 0 )
		{
			while( correction != 0 )
			{
				RR_ASSERT_ALWAYS( heap_size > 0 );
				sort_sym ss = heap[0];
				popped_heap(heap,heap+heap_size);
				heap_size--;
				
				int i = ss.sym;
				RR_ASSERT( from[i] != 0 );
				
				//rrprintf("%d : %f : %d\n",ss.sym,ss.rank,to[i]);
				
				to[i] += 1;
				correction -= 1;
			
				float change = logoneplusinv(to[i]) * from[i];
			
				heap[heap_size].sym = i;
				heap[heap_size].rank = change;
				heap_size++;
				push_heap(heap,heap+heap_size);
			}
		}
		else
		{
			while( correction != 0 )
			{
				RR_ASSERT_ALWAYS( heap_size > 0 );
				sort_sym ss = heap[0];
				popped_heap(heap,heap+heap_size);
				heap_size--;
				
				int i = ss.sym;
				RR_ASSERT( from[i] != 0 );
				
				//rrprintf("%d : %f : %d\n",ss.sym,ss.rank,to[i]);
				
				RR_ASSERT( to[i] > 1 );
				to[i] -= 1;
				correction += 1;
			
				if ( to[i] > 1 )
				{
					float change = logoneminusinv(to[i]) * from[i];
				
					heap[heap_size].sym = i;
					heap[heap_size].rank = change;
					heap_size++;
					push_heap(heap,heap+heap_size);
				}								
			}
		}
	}
	
	RR_ASSERT( sum(to,to+alphabet) == (U32)to_sum_desired );
	
	return num_non_zero;
}

S32 normalize_counts_v5(U32 * to, int to_sum_desired, const U32 * from, int from_sum, int alphabet)
{
	RR_ASSERT( to != from );
	
	RR_ASSERT_ALWAYS( from_sum > 0 );
	
	RR_DURING_ASSERT( U32 from_sum_check = sum(from,from+alphabet) );
	RR_ASSERT( (U32)from_sum == from_sum_check );
	
	double scale = (double)to_sum_desired / from_sum;

	S32 num_non_zero = 0;	
	U32 to_sum = 0;
	for LOOP(i,alphabet)
	{
		if ( from[i] == 0 )
		{
			to[i] = 0;
		}
		else
		{
			double from_scaled = from[i] * scale;
			U32 down = (U32)( from_scaled );
			
			to[i] = down + ( from_scaled*from_scaled > down*(down+1) );
			
			RR_ASSERT( to[i] > 0 );
			to_sum += to[i];
			
			num_non_zero++;
		}
	}
	
	S32 correction = to_sum_desired - to_sum;
	if ( correction != 0 )
	{
		RR_STACK_ARRAY(heap,sort_sym,alphabet);
		SINTa heap_size = 0;

		if ( correction > 0 )
		{
			for LOOP(i,alphabet)
			{
				if ( from[i] == 0 ) continue;
				RR_ASSERT( to[i] != 0 );

				float change = logoneplusinv(to[i]) * from[i];
			
				heap[heap_size].sym = i;
				heap[heap_size].rank = change;
				heap_size++;
			}
		}
		else
		{
			RR_ASSERT( correction < 0 );
			for LOOP(i,alphabet)
			{
				if ( from[i] == 0 ) continue;
				RR_ASSERT( to[i] != 0 );
				if ( to[i] > 1 )
				{
					float change = logoneminusinv(to[i]) * from[i];
				
					heap[heap_size].sym = i;
					heap[heap_size].rank = change;
					heap_size++;
				}
			}
		}
			
		netv5heap::make_heap(heap,heap+heap_size);
			
		if ( correction > 0 )
		{
			while( correction != 0 )
			{
				RR_ASSERT_ALWAYS( heap_size > 0 );
				netv5heap::pop_heap(heap,heap+heap_size);
				heap_size--;
				sort_sym ss = heap[heap_size];
				
				int i = ss.sym;
				RR_ASSERT( from[i] != 0 );
				
				to[i] += 1;
				correction -= 1;
			
				float change = logoneplusinv(to[i]) * from[i];
			
				heap[heap_size].sym = i;
				heap[heap_size].rank = change;
				heap_size++;
				netv5heap::push_heap(heap,heap+heap_size);
			}
		}
		else
		{
			while( correction != 0 )
			{
				RR_ASSERT_ALWAYS( heap_size > 0 );
				netv5heap::pop_heap(heap,heap+heap_size);
				heap_size--;
				sort_sym ss = heap[heap_size];
				
				int i = ss.sym;
				RR_ASSERT( from[i] != 0 );
				
				RR_ASSERT( to[i] > 1 );
				to[i] -= 1;
				correction += 1;
			
				if ( to[i] > 1 )
				{
					float change = logoneminusinv(to[i]) * from[i];
				
					heap[heap_size].sym = i;
					heap[heap_size].rank = change;
					heap_size++;
					netv5heap::push_heap(heap,heap+heap_size);
				}				
			}
		}
	}
	
	RR_ASSERT( RR_NAMESPACE::sum(to,to+alphabet) == (U32)to_sum_desired );
	
	return num_non_zero;
}

//=========================================================

// from is [alphabet] , to is [alphabet+1] , to[0] = 0 and to[alphabet] = sum
void counts_to_cumfreqs(U32 *to, const U32 *from, int alphabet)
{
	U32 sum = 0;
	to[0] = 0;
	for(int i=0;i<alphabet;)
	{
		sum += from[i];
		i++;
		to[i] = sum;
	}
}

//=======================================================================================


static RADINLINE int GetRiceCodeLen(int val,int rice_bits)
{
    return (val >> rice_bits) + 1 + rice_bits;
}

int EntropyOfCountsRice(int * counts,int numCounts,int rice_bits)
{
	/*
    U32 tot = 0;
    for(int i=0;i<numCounts;i++)
    {
        tot += counts[i];
    }
    */
    int H = 0;
    for(int i=0;i<numCounts;i++)
    {
        if ( counts[i] > 0 )
        {
            int codeLen = GetRiceCodeLen(i,rice_bits);
            H += counts[i] * codeLen;
        }
    }
    return H;
}

int EntropyOfDeltaRice(int * counts,int numCounts,int mean,int rice_bits)
{
	/*
    U32 tot = 0;
    for(int i=0;i<numCounts;i++)
    {
        tot += counts[i];
    }
    */
    int H = 0;
    for(int i=0;i<numCounts;i++)
    {
        if ( counts[i] > 0 )
        {
            int code = rrFoldUpNegatives( i - mean);
            int codeLen = GetRiceCodeLen(code,rice_bits);
            H += counts[i] * codeLen;
        }
    }
    return H;
}

int EntropyOfCountsExpGolomb(const U32 * counts,int numCounts,int expgolomb_k)
{
    int H = 0;
    for(int i=0;i<numCounts;i++)
    {
        if ( counts[i] > 0 )
        {
			int codeLen = rrVarBits_CountBits_ExpGolomb(i,expgolomb_k);
            H += counts[i] * codeLen;
		}
	}
	return H;
}

//=======================================

// use my log2 to avoid libc use on Linux :
static double rrlog10(double x)
{
	return rrlog2_F64(x) * 0.301030;
}

#if 0 // ndef __RADINDLL__

/**

find the lambda that optimizes this weissman range

IF this lambda optimizes this weissman
then it will be at the point where

dJ = 0
dW = 0

solve for lambda ...

... but I can't get sensible numbers out of this

**/

#include "rrtime.h"

double lambda_from_Weissman(double r, double s, double lo, double hi, double W)
{
	// hi == 0 means infinity
	double inv_hi = 0.0;
	if ( hi > 0 ) inv_hi = 1/hi;
	
	// ln vs log10 scale foofa ; W is in log10, convert to ln :
	W *= 0.4342945; // log10(e) = 1/ln(10) 
	
	double T = ( W * (r + s/lo) * (r + s*inv_hi) ) / ( r*r * ( 1/lo - 1*inv_hi ) );
	double L = (T - s) / r;
	
	// L is in MB/s , convert to bytes/cycle :
	F64 ticks_per_second = 1000000.0 / rrTicksToSeconds(1000000);
	F64 scale = 1000000.0 / ticks_per_second;
	
	// L is in MB/s , convert to bytes/cycle :
	double lambda = L * scale;
	
	rrprintf("W [%d-%d], L = %0.5f\n",(int)lo,(int)hi,lambda);
	return L;
}

#endif

double WeissmanScore(double ratio, double speed, double disk_speed_lo, double disk_speed_hi )
{
	double hi = 0;
	// disk_speed_hi == 0 for Weissman infinity
	if ( disk_speed_hi > 0 )
		hi = ratio * rrlog10( 1 + speed/(disk_speed_hi*ratio) );
	double lo = ratio * rrlog10( 1 + speed/(disk_speed_lo*ratio) );
	double w = lo - hi;
	
	#if 0 // ndef __RADINDLL__
	double lambda = lambda_from_Weissman(ratio,speed,disk_speed_lo,disk_speed_hi,w);
	lambda;
	#endif
	
	return w;
}

OODLE_NS_END
