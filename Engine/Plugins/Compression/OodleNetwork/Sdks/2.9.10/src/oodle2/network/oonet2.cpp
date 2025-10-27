// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "oonet2.h"
#include "rrlog.h"
#include "rrlogutil.h"
#include "histogram.h"
#include "entropysets.h"
#include "cbradutil.h"
#include "templates/rrvector.h"
#include "templates/rralgorithm.h"

OODLE_NS_START

#define CONTEXT_O3_BITS		20
#define CONTEXT_O3_COUNT	(1<<CONTEXT_O3_BITS)

static inline int context_o3(const U8 * ptr)
{
	// 20 bit context
	int ret = ptr[-1] + (ptr[-2]<<8) + ((ptr[-3]>>4)<<16);
	return ret;
}

/*

first 3 bytes of each packet get special contexts
1st : no context
2nd : o1
3rd : o2

*/

static inline int context_first()
{
	return CONTEXT_O3_COUNT;
}

static inline int context_second(const U8 * ptr)
{
	return CONTEXT_O3_COUNT + 1 + ptr[-1];
}

static inline int context_third(const U8 * ptr)
{
	return CONTEXT_O3_COUNT + 1 + 256 + ptr[-1] + (ptr[-2]<<8);
}

#define CONTEXT_COUNT	(CONTEXT_O3_COUNT + 1 + 256 + 65536)

#define MIN_TRAIN_PACKET_SIZE	16

struct histo16
{
	U16 counts[256];
	U32 total; // == sum of counts
	U32 total_unnormalized;
};

static inline void count(histo16 & es, int b)
{
	es.counts[b] ++;
	es.total ++;
	es.total_unnormalized ++;
	if ( es.counts[b] == 0xFFFF )
	{
		// normalize
		es.total = 0;
		for LOOP(i,256)
		{
			int c = es.counts[i];
			c = (c>>1);
			if ( c == 0 ) c = 1;
			es.counts[i] = (U16) c;
			es.total += c;
		}
	}
}

// make counts[] fit in S16 :
static inline void normalize_to_s16(histo16 & es)
{
	// normalize so max count <= 32767 , fits in S16 (not U16)
	if ( es.total < 32768 ) return;
	// find max :
	
	// @@ SIMD? _mm_max_epu16 is SSE4
	
	int max_count = 0;
	for LOOP(i,256)
	{
		max_count = RR_MAX(max_count,es.counts[i]);
	}
	if ( max_count < 32768 ) return;
	
	// need to scale :
	es.total = 0;
	// factor = 32767 / max_count
	for LOOP(i,256)
	{
		int c = es.counts[i];
		c = ((c * 32767)/max_count);
		if ( c == 0 ) c = 1;
		es.counts[i] = (U16) c;
		es.total += c;
	}
}

void OodleNetwork2_Test(
		const void ** train_packet_pointers, 
		const S32 * train_packet_sizes,
		S32 num_train_packets,
		const void ** test_packet_pointers, 
		const S32 * test_packet_sizes,
		S32 num_test_packets
		)
{
	rrprintf("OodleNetwork2_Test {\n");

	vector<histo16> histograms;
	histograms.resize(CONTEXT_COUNT); // 0.5 GB alloc !
	histograms.memset_zero();
	
	entropyset histo_o0;
	RR_ZERO(histo_o0);
	
	for LOOP(p,num_train_packets)
	{
		S32 packet_len = train_packet_sizes[p];
		if ( packet_len < MIN_TRAIN_PACKET_SIZE )
			continue;
		
		const U8 * packet_ptr = U8_void( train_packet_pointers[p] );
		const U8 * packet_end = packet_ptr + packet_len;
		
		int c;
		c = context_first();
		count(histograms[c],*packet_ptr++);
		
		c = context_second(packet_ptr);
		count(histograms[c],*packet_ptr++);
		
		c = context_third(packet_ptr);
		count(histograms[c],*packet_ptr++);
		
		while(packet_ptr<packet_end)
		{			
			c = context_o3(packet_ptr);
			int b = *packet_ptr++;

			histo_o0.histo.counts[b] ++;

			count(histograms[c],b);
		}
	}
	
	// U16 -> S16 histograms
	// make the overall o1 and o0 histograms
	
	int num_zero = 0;
	int num_1to10_det = 0;
	int num_1to10_nondet = 0;
	int num_over10 = 0;
	int num_1to10_det_count = 0;
	int num_1to10_nondet_count = 0;
	int num_over10_count = 0;
	
	for LOOP(c,CONTEXT_COUNT)
	{
		if ( histograms[c].total == 0 ) num_zero++;
		else if ( histograms[c].total <= 10 )
		{
			bool det = true;
			for LOOP(i,256)
			{
				if ( histograms[c].counts[i] == 0 ) continue;
				if ( histograms[c].counts[i] != histograms[c].total )
				{
					det = false;
					break;
				}
			}
			if ( det ) {
				num_1to10_det++; num_1to10_det_count += histograms[c].total;
			} else {
				num_1to10_nondet++; num_1to10_nondet_count += histograms[c].total;
			}
		}
		else { num_over10++; num_over10_count += histograms[c].total_unnormalized; }
	
		if ( histograms[c].total == 0 )
			continue;
	
		normalize_to_s16(histograms[c]);
	}
	
	rrprintfvar(num_zero);
	rrprintfvar(num_1to10_det);
	rrprintfvar(num_1to10_nondet);
	rrprintfvar(num_over10);
	rrprintfvar(num_1to10_det_count);
	rrprintfvar(num_1to10_nondet_count);
	rrprintfvar(num_over10_count);
	
/*	
	
num_1to10_det : 262356
num_1to10_nondet : 571157
num_over10 : 74221
num_1to10_det_count : 316943
num_1to10_nondet_count : 2228246
num_over10_count : 4926679

the low count contexts are NOT mostly deterministic

the total count is NOT mostly in contexts with count > 10

you cannot just ignore the contexts with count <= 10 , they are ~ 1/3 of all counts

*/

	vector<int> totals;
	totals.resize(CONTEXT_COUNT);
	for LOOP(c,CONTEXT_COUNT)
	{
		totals[c] = histograms[c].total_unnormalized;
	}
	stdsort(totals.begin(),totals.end(),stdgreater<int>());
	
	for LOOP(i,20)
		rrprintfvar(totals[i]);

/**

first 20 :
totals[i] : 110129
totals[i] : 15721
totals[i] : 14590
totals[i] : 13946
totals[i] : 13507
totals[i] : 13416
totals[i] : 12863
totals[i] : 12847
totals[i] : 12847
totals[i] : 12847
totals[i] : 12845
totals[i] : 12078
totals[i] : 11439
totals[i] : 11437
totals[i] : 11010
totals[i] : 10717
totals[i] : 10661
totals[i] : 10602
totals[i] : 10348
totals[i] : 10168

**/

	// k-means ++
	
	
	
	rrprintf("OodleNetwork2_Test done. }\n");
}

OODLE_NS_END

		
		