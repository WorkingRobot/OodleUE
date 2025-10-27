// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "newlz_arrays.h"

#include "rrvarbits.h"
#include "histogram.h"
#include "cbradutil.h"
#include "rrlog.h"
#include "log2table.h"

#include "newlz_arrays.inl"
#include "newlz_arrays_huff.h"
#include "newlz_arrays_tans.h"
#include "newlz_arrays_rle.h"
#include "newlz_multiarrays.h"
#include "newlz_shared.h"
#include "newlz_speedfit.h"
#include "newlz_simd.h"
#include "cpux86.h"
#include "rrarenaallocator.h"
#include "oodleconfigvalues.h"

//#include "rrsimpleprof.h"
#include "rrsimpleprofstub.h"

OODLE_NS_START

/***

newlz_arrays

newLZ_put_array driver is here

newLZ_get_array is in newlz_arrays.inl
	it needs to be inlined for the point-at-uncompressed case (eg. Selkie)

---

raw (uncompressed)
huff
	huff3/huff6
tans

--

split multi-array
N-array indexed splits?

****/

#if 0

// array dumping

#include <stdio.h>
#include "../ext/oodleutil.h"

static void test_save_tans_array(const U8 * from,SINTa from_len)
{
	static int s_count = 0;
	
	char filename[1024];
	sprintf(filename,"r:\\tans_arrays\\%d",s_count);
	s_count++;
	
	OodleUtil_NonIOQ_SyncIO_WriteWholeFile(filename,from,from_len);
}

#endif


// newLZ_put_array : 
SINTa newLZ_put_array_histo(U8 * to,U8 * to_end, const U8 * from, SINTa from_len, 
	const U32 * histogram, U32 flags, F32 lambda, const OodleSpeedFit * speedfit, F32 * pJ, F32 deadline_t,
	rrArenaAllocator * arena,
	int compression_level // OodleLZ_CompressionLevel
	)
{
	SIMPLEPROFILE_SCOPE_N(put_array_histo,from_len);
	
	RR_ASSERT( from_len >= 0 ); // strictly required
	// from_len == NEWLZ_ARRAY_SIZE_MASK+1 is okay here
	//	because compressed arrays put (from_len-1)&MASK
	RR_ASSERT( from_len <= (NEWLZ_ARRAY_SIZE_MASK+1) );
	
	// not required, just verifying code flow :
	RR_ASSERT( from_len >= NEWLZ_HUFF_ARRAY_MIN_SIZE ); // tiny arrays should have already been put uncompressed
	
		
	/*
	// changed 03-28-2019 , no longer pre-checking room
	if ( (from_len + 5) > rrPtrDiff(to_end - to) )
	{
		ooLogError("not enough comp space\n");
		return -1;
	}
	*/
	
	// just check for room for the minimum size array ; 5 byte header ; rle memset is 6 bytes
	if ( rrPtrDiff(to_end - to) < 6 )
	{
		// not an error, can be legit expansion
		rrPrintf_v2("newLZ_put_array_histo: no comp space\n");
		return -1;
	}
	
	//RR_ASSERT( verify_histo(from,from_len,histogram,256) );
	U32 highest_histo_count = simd_find_max_U32_256(histogram);
	RR_ASSERT( highest_histo_count >= (U32)(from_len/256) );
	RR_ASSERT( highest_histo_count <= (U32)from_len );
	
	//RR_ASSERT( flags != 0 ); // check if Selkie is calling me
	
	// we have a histo already, so doing rrIsMemset here, could just use the histo instead
	//if ( from_len > 0 && rrIsMemset(from,from_len) )
	if ( from_len > 0 && highest_histo_count == (U32)from_len )
	{
		return newLZ_put_array_memset(to,to_end,from,from_len,flags,lambda,speedfit,pJ,arena);
	}
			
	U8 * to_ptr = to;
	
	// NO newLZ_speedfit_memcpy_time in J_raw - we don't memcpy
	F32 J_raw = (F32)from_len+3;
	
	F32 min_J = RR_MIN(*pJ,J_raw); // *pJ is what was already in there (eg LZ for whole-huff chunks)
	
	SINTa comp_len = from_len;
	U32 array_type = NEWLZ_ARRAY_TYPE_UNCOMPRESSED;
	F32 J_comp = LAGRANGE_COST_INVALID;

	// NOTE : "comp_len" does NOT contain the 5 byte header size
	//	but J does !!

	/**
	
	newlz_array_estimate_complen_bits is a pretty solid under-estimate of comp len
	it's not a strict under-estimate (could use the pure entropy for that), but close
	
	that means if newlz_array_estimate_complen_bits can't beat the "must be under" len
	it's very unlikely that huff or tans or rle will
	
	-> this is true for the pure order0 entropy coders, but NOT true for splits
	you can have arrays that are compressible when split but not as a whole
	the question is how common is that, do we care?
	
	---------------
	
	call runtest_win64 test_all comptime -c8 -z3 -e5 r:\testsets\gametestset -a

	trying all :	
	SUM:total : 49,000,000 ->19,717,127 =  3.219 bpb =  2.485 to 1
	SUM:encode           : 2.032 seconds, 71.70 c/b, rate= 24.12 mb/s

	with estimate early out :
	SUM:total : 49,000,000 ->19,757,692 =  3.226 bpb =  2.480 to 1
	SUM:encode           : 1.988 seconds, 70.15 c/b, rate= 24.65 mb/s
	
	with RLE pulled out : (doesn't help)
	SUM:total : 49,000,000 ->19,756,337 =  3.226 bpb =  2.480 to 1
	SUM:encode           : 2.020 seconds, 71.28 c/b, rate= 24.26 mb/s
	SUM:decode           : 103.337 millis, 3.65 c/b, rate= 474.17 mb/s
	
	-> not really compelling here
		(compression penalty's not bad, but the speed gain is near-nop too)
	
	-> wasn't compelling for Kraken -z1 either
		the issue is Kraken's lambda means huff is taken a lot
		and the early-out reject doesn't trigger often
		(-z1 doesn't try whole huff chunks)
	
	---------------------
	
	What's the maximum win for this?
	Mermaid SuperFast with a higher lambda so huff is rarely chosen over raw
	
	call runtest_win64 test_all comptime -c9 -z1 -zs512 -e5 r:\testsets\gametestset -a
	
	trying all :	
	SUM:total : 49,000,000 ->24,734,823 =  4.038 bpb =  1.981 to 1
	SUM:encode           : 675.580 millis, 23.84 c/b, rate= 72.53 mb/s

	with estimate early out :
	SUM:total : 49,000,000 ->24,741,322 =  4.039 bpb =  1.980 to 1
	SUM:encode           : 638.342 millis, 22.52 c/b, rate= 76.76 mb/s
	
	-> looks like a good tradeoff here
	
	at default lamda/SSTB :
	
	trying all :
	SUM:total : 49,000,000 ->23,248,467 =  3.796 bpb =  2.108 to 1
	SUM:encode           : 678.306 millis, 23.94 c/b, rate= 72.24 mb/s

	with estimate early out :
	SUM:total : 49,000,000 ->23,271,323 =  3.799 bpb =  2.106 to 1
	SUM:encode           : 660.967 millis, 23.32 c/b, rate= 74.13 mb/s
	
	-> yes
	
	---------------------

	without 1/128 early out :

	Mermaid, SuperFast, rand_1M :
	 1,048,576 -> 1,048,584 =  8.000 bpb =  1.000 to 1
	encode (x10)     : 2.422 millis, 3.99 c/b, rate= 432.99 mb/s
	decode (x30)     : 0.234 millis, 0.39 c/b, rate= 4482.32 mb/s

	with 1/128 early out :

	Mermaid, SuperFast, rand_1M :
	 1,048,576 -> 1,048,584 =  8.000 bpb =  1.000 to 1
	encode (x10)     : 2.383 millis, 3.93 c/b, rate= 440.10 mb/s
	decode (x30)     : 0.240 millis, 0.40 c/b, rate= 4371.65 mb/s

	with 1/128 early out :

	inName : R:\testsets\CCC\mine\rand_1M
	inSize64 : 1048576
	huffencode       : 1.819 millis, 3.00 c/b, rate= 576.53 mb/s
	test_huffpub x40 :  1,048,576 -> 1,048,600 =  8.000 bpb =  1.000 to 1
	huffdecode       : 0.326 millis, 0.54 c/b, rate= 3213.28 mb/s
	SimpleProf            :seconds  calls     count :  clk/call clk/count      min
	CountHistoArrayU8     : 0.2071    960 125829120 :  373012.3      2.85     2.58

	without 1/128 early out :

	inName : R:\testsets\CCC\mine\rand_1M
	inSize64 : 1048576
	huffencode       : 1.904 millis, 3.14 c/b, rate= 550.70 mb/s
	test_huffpub x40 :  1,048,576 -> 1,048,600 =  8.000 bpb =  1.000 to 1
	huffdecode       : 0.296 millis, 0.49 c/b, rate= 3548.13 mb/s
	SimpleProf            :seconds  calls     count :  clk/call clk/count      min
	CountHistoArrayU8     : 0.2066    960 125829120 :  372184.9      2.84     2.58
	put_array_huff        : 0.0110    960 125829120 :   19741.7      0.15     0.14
	put_array_huff_build  : 0.0103    960       960 :   18524.4  18524.42 99999.99

	the early out just doesn't save much
	without it, we build huff tables, then estimate complen and bail out
	the huff table build time is very low compared to the histogramming time which dominates
	I suppose it could make a bigger difference on tiny arrays
	but I'm just not seeing it

	**/
	
	bool try_compression = true;

	//=============================================	
	#if 1
	
	// @@@@ DO THIS ?
	// does this do anything good? not really seeing it
	
	// -> this is to help speed at -z1 levels
	//	on nearly incompressible data
	// -> currently not really seeing much speed gain
	
	// test highest count must be probability > (1/128)
	// this is a pretty conservative test,
	//	if it's true we almost never get any compression at all
	// this also doesn't help speed significantly
	// it can detect truly random data
	// but even near random incompressible data will often have some single count > (1/128)
	// (this is a much weaker early out than the est_comp_len below)

	if ( compression_level < 3 ) // @@ what cutoff ? different for Mermaid - Leviathan ?
	{		
		if ( highest_histo_count < (U32)(from_len>>7) )
		{
			try_compression = false;
		}
	}
	
	#endif
	//=============================================	
		

	if ( try_compression )	
	{

	// try RLE first (?)
	//  note RLE is not checking NEWLZ_HUFF_ARRAY_MIN_SIZE , it tries even on tiny arrays
	if ( flags & NEWLZ_ARRAY_FLAG_ALLOW_RLE )
	{
		F32 rle_J_comp_add = 5 + lambda * speedfit->rle(from_len );
		SINTa rle_comp_len_must_be_under = (SINTa)(min_J - rle_J_comp_add);

		//rle_comp_len_must_be_under = RR_MIN(rle_comp_len_must_be_under, from_len-from_len/3);

		// determine target comp len
		SINTa rle_target_comp_len = (to_end - to) - 5;
		rle_target_comp_len = RR_MIN(rle_target_comp_len, rle_comp_len_must_be_under);

		if ( rle_target_comp_len > 0 )
		{
			// @@@@ RLE putter violates the principle of not touching [to] 
			//	unless it can beat rle_target_comp_len
			// so I have to put to a temp buffer :
			//	@@ would simplify things to just share this for all the comp putters
			//	 many of them do the same thing internally
			
			void * rle_comp_bufv;
			rrScopeArenaAlloc alloc(rle_target_comp_len,arena,&rle_comp_bufv);
			U8 * rle_comp_buf = (U8 *)rle_comp_bufv;
		
			// internally checks NEWLZ_ARRAY_FLAG_ALLOW_RLEHUFF
			F32 rleJ = LAGRANGE_COST_INVALID;
			SINTa rle_comp_len = newLZ_put_array_rle(rle_comp_buf,rle_comp_buf+rle_target_comp_len,from,from_len,lambda,speedfit,&rleJ,arena,flags,compression_level);
			
			RR_ASSERT( rleJ >= rle_comp_len + rle_J_comp_add - 0.01 ); // float slop

			if ( rle_comp_len <= rle_target_comp_len && rleJ < min_J )
			{
				// rle_target_comp_len is explicitly made so we fit in to_end
				RR_ASSERT( (rle_comp_len+5) <= (to_end - to) );
				memcpy(to+5,rle_comp_buf,rle_comp_len);
			
				// rle_comp_len == 1 means memset array
				RR_ASSERT( rle_comp_len > 1 );
			
				array_type = NEWLZ_ARRAY_TYPE_RLE;
				comp_len = rle_comp_len;
				J_comp = rleJ;
				RR_ASSERT( lambda > 0.f || J_comp == comp_len+5 );
				//J_comp = rle_comp_len + rle_J_comp_add;
				RR_ASSERT( J_comp <= min_J ); // should only have chosen this if it's better
				min_J = RR_MIN(min_J,J_comp);
			}
		}
	}

	// don't bother if we can't possibly make the required gain :
	if ( from_len >= NEWLZ_HUFF_ARRAY_MIN_SIZE )
	{

		// there is no NEWLZ_ARRAY_FLAG_ALLOW_HUFF , it's always on
		//if ( flags & NEWLZ_ARRAY_FLAG_ALLOW_HUFF )
		{
			// note Huff no longer has to handle single symbol alphabet, RLE gets it
			// -> except in v5 mode, then Huff still does it
			
			F32 huff_J = min_J;

			// put at to+5 for header space
			U32 huff_type = 0;
			SINTa huff_comp_len = newLZ_put_array_huff(to+5,to_end,from,from_len,histogram,lambda,speedfit,&huff_J,deadline_t,&huff_type,flags,arena,compression_level);
			// huff_comp_len < 0 means failed but did not touch [to]
			if ( huff_comp_len > from_len )
			{
				// failed and wiped out [to]
				J_comp = LAGRANGE_COST_INVALID;
			}
			else if ( huff_comp_len >= 0 )
			{
				// huff_comp_len >= 0 means we put something to "to"
				//	otherwise we left what was in there because it was J-better
				RR_ASSERT( huff_type == NEWLZ_ARRAY_TYPE_HUFF || huff_type == NEWLZ_ARRAY_TYPE_HUFF6 );
				array_type = huff_type;
				comp_len = huff_comp_len;
				J_comp = huff_J;
				RR_ASSERT( J_comp >= comp_len+5 );
				RR_ASSERT( lambda > 0.f || J_comp == comp_len+5 );
				RR_ASSERT( J_comp <= min_J ); // should only have chosen this if it's better
				min_J = J_comp;				
			}
		}
		
		if ( flags & NEWLZ_ARRAY_FLAG_ALLOW_TANS )
		{
			// try TANS as well
			
			// leave huff data in [to] if its J is better :
			
			F32 tans_J = min_J;

			//RR_ASSERT( verify_histo(from,from_len,histogram,256) );
	
			// put at to+5 for header space
			SINTa tans_comp_len = newLZ_put_array_tans(to+5,to_end,from,from_len,histogram,lambda,speedfit,&tans_J,arena);

			// verify histo after TANS because it fucks it up :
			RR_ASSERT( verify_histo(from,from_len,histogram,256) );
	
			if ( tans_comp_len >= 0 )
			{
				RR_ASSERT( tans_comp_len < from_len );
				
				/*
				// log when we choose TANS over huff :
				rrprintf("TANS chosen : huff=%d vs tans=%d (from %d) = %d gained = %.3f bpb\n",
					comp_len,tans_comp_len,from_len,
					comp_len-tans_comp_len,
					(comp_len-tans_comp_len)*8.0/from_len);
				*/
				
				// dump just the arrays where TANS was chosen
				//test_save_tans_array(from,from_len);
				
				RR_ASSERT( tans_J <= min_J ); // should only have chosen this if it's better
				RR_ASSERT( tans_J >= tans_comp_len+5 );
				RR_ASSERT( lambda > 0.f || tans_J == tans_comp_len+5 );
				
				array_type = NEWLZ_ARRAY_TYPE_TANS;
				comp_len = tans_comp_len;
				min_J = J_comp = tans_J;
			}
		}
		
		if ( flags & NEWLZ_ARRAY_FLAG_ALLOW_SPLIT )
		{
			// @@?? don't try split unless one of our compress attempts so far
			//		has beaten what was previously in *pJ
			//		(early out for whole huff chunks for example)
			// @@?? maybe allow split_N but not split_indexed ?

			/*
			
			this does hurt compression a measurable amount
			but the encode speed benefit is enormous
			need to do some more study to see when this early out is okay
			-> it looks like doing this only on whole-huff chunks is the way to go

			Kraken, Optimal3, lzt99 : 
			
			before :
			24,700,820 -> 9,515,426 =  3.082 bpb =  2.596 to 1 

			early-out always :
			24,700,820 -> 9,564,359 =  3.098 bpb =  2.583 to 1 
			
			early-out only on whole-huff chunks :
			24,700,820 -> 9,518,135 =  3.083 bpb =  2.595 to 1 

			*/

			// 07-07-2019
			// this is trying to detect incompressible random data
			//	and not bother trying splits on it
			// it can make mistakes because the whole array histo may be entropy 8
			//	but there still could be splittable portions that are compressible
			//
			//-> @@ TODO : could probably have a slightly smarter early out here
			//like just grab two random histos from the array and see if either has entropy < 7.5

			if ( J_comp < *pJ || from_len < 128*1024 || compression_level >= 9 )
			{
				// internally checks NEWLZ_ARRAY_FLAG_ALLOW_SPLIT_INDEXED
				F32 split_J = LAGRANGE_COST_INVALID;
				SINTa split_comp_len = newLZ_put_array_sub_split(to+5,to_end,from,from_len,histogram,arena,compression_level,flags,lambda,speedfit,min_J,&split_J);
				if ( split_comp_len >= 0 )
				{
					RR_ASSERT( split_comp_len < from_len );
					
					array_type = NEWLZ_ARRAY_TYPE_SPLIT;
					comp_len = split_comp_len;
					J_comp = split_J;
					RR_ASSERT( lambda > 0.f || J_comp == comp_len+5 );
					RR_ASSERT( J_comp <= min_J ); // should only have chosen this if it's better
					min_J = J_comp;	
				}
			}
		}
	}
	
	} // try_compression
		
	if ( J_raw <= J_comp )
	{
		// if existing is better, put nothing :
		if ( *pJ <= J_raw )
		{
			// huff should not have put anything at [to]
			RR_ASSERT( J_comp == LAGRANGE_COST_INVALID );
		
			return -1;
		}
	
		if ( from_len > NEWLZ_ARRAY_SIZE_MASK )
		{
			ooLogError("from_len too big !?\n");
			return -1;
		}
		
		// changed 03-28-2019 , no longer pre-checking room
		if ( (from_len + 3) > rrPtrDiff(to_end - to) )
		{
			// not an error
			rrPrintf_v2("not enough comp space for uncomp array\n");
			return -1;
		}
	
		to_ptr = newLZ_put_array_uncomp_header(to_ptr,from_len);
		
		memcpy(to_ptr,from,from_len);
		to_ptr += from_len;
		
		*pJ = J_raw;
	}
	else
	{
		RR_ASSERT( J_comp == min_J ); // J_comp also better than *pJ previous contents
	
		to_ptr = newLZ_put_array_comp_header(to_ptr,array_type,from_len,comp_len);

		// don't do +5 on J_comp here, everybody above already did it
		RR_ASSERT( J_comp >= comp_len+5 );
		RR_ASSERT( lambda > 0.f || J_comp == comp_len+5 );

		*pJ = J_comp;
	}
	
	return rrPtrDiff( to_ptr - to );
}

SINTa newLZ_put_array_uncompressed(U8 * to,U8 * to_end, const U8 * from, SINTa from_len)
{
	RR_ASSERT( from_len >= 0 );
	// mask is 256k-1
	// from_len can be as large as 256k (whole OODLELZ_BLOCK)
	//	but if I ever did that, it would be a failure to compress
	//	which I will signal at the block level
	//RR_ASSERT( from_len <= (NEWLZ_ARRAY_SIZE_MASK+1) );
	//RR_ASSERT( from_len <= NEWLZ_ARRAY_SIZE_MASK );
	if ( from_len > NEWLZ_ARRAY_SIZE_MASK )
	{
		ooLogError("from_len too big !?\n");
		return -1;
	}
	
	if ( (from_len + 3) > rrPtrDiff(to_end - to) )
	{
		// not an error
		rrPrintf_v2("not enough comp space for uncomp array\n");
		return -1;
	}
	
	U8 * to_ptr = to;
	
	to_ptr = newLZ_put_array_uncomp_header(to_ptr,from_len);
	
	memcpy(to_ptr,from,from_len);
	to_ptr += from_len;
		
	return rrPtrDiff( to_ptr - to );
}

SINTa newLZ_put_array_memset(U8 * to,U8 * to_end, const U8 * from, SINTa from_len,
							U32 flags, F32 lambda,const OodleSpeedFit * speedfit, F32 * pJ, rrArenaAllocator * arena)
{
	// avoid the possibility that uncompressed is smaller than this header
	//	we put 6 or 8 bytes; uncomp puts 3 + from_len
	// also avoid from_len == 0 here, give that to uncompressed
	if ( from_len < 6 )
	{
		// this code path is not used at the moment
		// len > NEWLZ_HUFF_ARRAY_MIN_SIZE
		*pJ = from_len + 3.f;
		return newLZ_put_array_uncompressed(to,to_end,from,from_len);
	}

	RR_ASSERT( rrIsMemset(from,from_len) );

	// _RLE_MEMSET is sometimes on even when _RLE is off
	if ( flags & NEWLZ_ARRAY_FLAG_ALLOW_RLE_MEMSET )
	{
		// whole-memset arrays are sent as RLE - starting in version 6
		RR_ASSERT( g_OodleLZ_BackwardsCompatible_MajorVersion >= 6 );
	
		const int rle_memset_complen = 6;
	
		if ( (to_end - to) < rle_memset_complen ) return -1;
		
		F32 J_memset = rle_memset_complen + lambda * speedfit->memcpy(from_len);
	
		// if existing is better, put nothing : (@@ is this possible?)
		if ( *pJ <= J_memset )
		{
			return -1;
		}
		
		*pJ = J_memset;
		
		// one byte payload = the memset value
		to[5] = from[0];
		
		// newLZ_put_array_comp_header advances you by payload comp len (pass in 1) :
		U8 * to_ptr = newLZ_put_array_comp_header(to,NEWLZ_ARRAY_TYPE_RLE,from_len,1);

		// don't do +5 on J_comp here, everybody above already did it
		SINTa comp_len = rrPtrDiff( to_ptr - to );
		RR_ASSERT( comp_len == rle_memset_complen );

		RR_ASSERT( J_memset >= comp_len );
		RR_ASSERT( lambda > 0.f || J_memset == comp_len );
					
		return comp_len;
	}
	else
	{
		// use Huff :
		
		const int huff_memset_complen = 8;
		
		if ( (to_end - to) < huff_memset_complen ) return -1;
		
		F32 J_memset = huff_memset_complen + lambda * speedfit->memcpy(from_len);
		
		// if existing is better, put nothing : (@@ is this possible?)
		if ( *pJ <= J_memset )
		{
			return -1;
		}
		
		*pJ = J_memset;
		
		U8 * to_ptr = newLZ_put_array_huff3_memset(to,from_len,from[0]);
		SINTa comp_len = rrPtrDiff( to_ptr - to );
		RR_ASSERT( comp_len == huff_memset_complen );
		
		RR_ASSERT( J_memset >= comp_len );
		RR_ASSERT( lambda > 0.f || J_memset == comp_len );
					
		return comp_len;		
	}
}

SINTa newLZ_put_array(U8 * to,U8 * to_end, const U8 * from, SINTa from_len, 
	U32 entropy_type, F32 lambda,const OodleSpeedFit * speedfit, F32 * pJ, F32 deadline_t,
	rrArenaAllocator * arena,
	int compression_level, // OodleLZ_CompressionLevel
	U32 * optional_histo /*= NULL*/
	)
{
	if ( from_len <= NEWLZ_HUFF_ARRAY_MIN_SIZE )
	{
		// NOTE : opt_histo not filled - should be zeroed before
		RR_ASSERT( optional_histo == NULL || optional_histo[57] == 0 );
	
		// can't possibly qualify
		F32 raw_J = 3 + (F32)from_len;
		*pJ = raw_J;
		return newLZ_put_array_uncompressed(to,to_end,from,from_len);
	}
	else
	{
		U32 histo[256]; // 1k @@ arena ?
		CountHistoArrayU8(from,from_len,histo,256);
		
		// optional_histo no longer tries to track what the coder decides (eg. flattening if it chooses uncompressed)
		//	it's just the true histo
		if ( optional_histo )
			memcpy(optional_histo,histo,256*sizeof(U32));
		
		return newLZ_put_array_histo(to,to_end,from,from_len,histo,entropy_type,lambda,speedfit,pJ,deadline_t,arena,compression_level);
	}	
}

SINTa newLZ_reput_array_small(U8 * comp,SINTa array_comp_len, F32 * pJ)
{
	// array_comp_len contains the header
	//	it's the not the same as the comp_len payload
	
	RR_ASSERT( *pJ >= array_comp_len );
	
	if ( array_comp_len > 0xFFF+5 )
	{
		// impossible to fit in small, get out
		return array_comp_len;
	}
	
	U8 first_byte = *comp;
	RR_ASSERT( first_byte < 0x80 ); // not small already
	
	SINTa raw_len;
	SINTa get_comp_len = newLZ_get_arraylens(comp,comp+array_comp_len,&raw_len,128*1024);
	RR_ASSERT( get_comp_len == array_comp_len ); get_comp_len;
	
	U32 array_type = first_byte>>4;
	
	if ( array_type == NEWLZ_ARRAY_TYPE_UNCOMPRESSED )
	{
		// uncompressed
		
		RR_ASSERT( array_comp_len == raw_len+3 );
		
		if ( raw_len > 0xFFF )
			return array_comp_len;
	
		// fits, do it :
		
		// 2 byte header :
		U16 header = (U16)raw_len | 0x8000;
		
		RR_PUT16_BE( comp, header );
		
		// save 1 byte :
		memmove(comp+2,comp+3,raw_len);
		
		RR_ASSERT( *comp >= 0x80 );
		
		*pJ -= 1.f;
		
		return raw_len+2;
	}
	else
	{
		SINTa data_comp_len = array_comp_len - 5;
		RR_ASSERT( data_comp_len > 0 );
		RR_ASSERT( raw_len > data_comp_len );
		
		if ( data_comp_len > NEWLZ_ARRAY_SMALL_SIZE_MASK )
			return array_comp_len;
		
		SINTa packed_raw_len = raw_len - data_comp_len - 1;
		RR_ASSERT( packed_raw_len >= 0 );
		
		if ( packed_raw_len > NEWLZ_ARRAY_SMALL_SIZE_MASK )
			return array_comp_len;
	
		// fits, do it :
		
		// 3 byte header :
		RR_COMPILER_ASSERT( (NEWLZ_ARRAY_SMALL_SIZE_BITS*2) == 20 );
		U32 header = (0x80<<16) | (array_type<<20) | ((U32)packed_raw_len<<NEWLZ_ARRAY_SMALL_SIZE_BITS) | (U32)data_comp_len;
		RR_PUT24_BE_OVERRUNOK(comp,header);
		
		// save 2 bytes :
		memmove(comp+3,comp+5,data_comp_len);
		
		RR_ASSERT( *comp >= 0x80 );
		
		*pJ -= 2.f;
		
		return data_comp_len+3;
	}	
}

SINTa newLZ_put_array_small_allowed(U8 * to,U8 * to_end, const U8 * from, SINTa from_len,
	U32 entropy_flags, F32 lambda,const OodleSpeedFit * speedfit, F32 * pJ, F32 deadline_t,
	rrArenaAllocator * arena,
	int compression_level,
	U32 * optional_histo)
{
	SINTa to_len = newLZ_put_array(to,to_end,from,from_len,entropy_flags,lambda,speedfit,pJ,deadline_t,arena,compression_level,optional_histo);

	// should not ever get here in earlier version modes :
	//	 (this is only used for sub-arrays and sub-arrays are only in new array types that are >= v6)
	RR_ASSERT( g_OodleLZ_BackwardsCompatible_MajorVersion >= 6 );

	if ( to_len < 0 )
		return -1;
	
	SINTa new_to_len = newLZ_reput_array_small(to,to_len,pJ);
	
	RR_ASSERT( new_to_len <= to_len );
	RR_ASSERT( new_to_len >= to_len-2 );

	RR_ASSERT( *pJ >= new_to_len );
	RR_ASSERT( lambda > 0.f || *pJ == new_to_len );

	return new_to_len;
}
	
	

SINTa newLZ_get_arraylens(const U8 * from, const U8 * from_end, SINTa * pto_len, SINTa to_len_max)
{
	// @@ BEWARE : MUCH CODE DUPE WITH newLZ_get_array

	const U8 * from_ptr = from;

	// can I get 2 bytes? :
	REQUIRE_FUZZ_RETURN( 2 <= rrPtrDiff( from_end - from_ptr ) , -1 );
	
	U8 first_byte = *from_ptr;
	U32 array_type = (first_byte>>4);
	
	if ( first_byte >= 0x80 ) // small flag
	{
		array_type &= 7;
		
		if ( array_type == NEWLZ_ARRAY_TYPE_UNCOMPRESSED )
		{
			// 2 byte header
			U32 header = RR_GET16_BE(from_ptr);
			from_ptr += 2;
			
			SINTa to_len = (SINTa)(header & 0xFFF);
			
			if ( to_len > to_len_max ) NEWLZ_ARRAY_RETURN_FAILURE();
			if ( from_ptr+to_len > from_end ) NEWLZ_ARRAY_RETURN_FAILURE();
					
			from_ptr += to_len;
			*pto_len = to_len;
			return rrPtrDiff( from_ptr - from );
		}
		else
		{
			// 3 byte header
			// must always have >= 1 payload bytes, so check for 4 :
			
			REQUIRE_FUZZ_RETURN( 4 <= rrPtrDiff( from_end - from_ptr ), -1 );
			
			U32 header = RR_GET24_BE_OVERRUNOK(from_ptr);
			from_ptr += 3;
			
			RR_ASSERT( array_type != NEWLZ_ARRAY_TYPE_UNCOMPRESSED ); // uncomp handled outside
			if ( array_type >= NEWLZ_ARRAY_TYPE_COUNT )
			{
				rrPrintf_v2("newLZ_get_array : invalid array_type\n");
				NEWLZ_ARRAY_RETURN_FAILURE();
			}

			// 10 bits comp, 10 bits raw + 4 bits header

			SINTa comp_len = (SINTa)(header & NEWLZ_ARRAY_SMALL_SIZE_MASK);
			if ( from_ptr + comp_len > from_end )
				NEWLZ_ARRAY_RETURN_FAILURE();

			SINTa to_len = (SINTa)((header>>NEWLZ_ARRAY_SMALL_SIZE_BITS) & (NEWLZ_ARRAY_SMALL_SIZE_MASK));
			to_len += comp_len + 1;

			if ( to_len > to_len_max ) NEWLZ_ARRAY_RETURN_FAILURE();
			//if ( comp_len >= to_len ) NEWLZ_ARRAY_RETURN_FAILURE();
			RR_ASSERT( comp_len < to_len );
			
			from_ptr += comp_len;
			RR_ASSERT( from_ptr <= from_end );

			*pto_len = to_len;
			return rrPtrDiff( from_ptr - from );
		}		
	}
	else
	{
		// can I get 4 bytes? :
		if ( 4 > rrPtrDiff( from_end - from_ptr ) )
		{
			// could be a 3-byte header at end of stream with zero payload
			// -> this cannot occur currently in valid Kraken/Mermaid/Selkie data
			//	 they always have >= 1 byte after the "arrays" data
			//	 but you could hit it on corrupt/truncated data, or in some future codec
			if ( 3 == rrPtrDiff( from_end - from_ptr ) )
			{		
				// get 3-byte header :
				U32 header = RR_GET24_BE_NOOVERRUN(from_ptr);
				// array type UNCOMPRESSED = 0 , and 0 bytes of data = whole header zero
				REQUIRE_FUZZ_RETURN( header == 0 , -1 );
				
				*pto_len = 0;
				
				return 3;
			}
			
			NEWLZ_ARRAY_RETURN_FAILURE();
		}
		
		if ( array_type == NEWLZ_ARRAY_TYPE_UNCOMPRESSED )
		{	
			// get 3-byte header : (but reads 4, hence the 4-byte check above)
			U32 header32 = RR_GET24_BE_OVERRUNOK(from_ptr);
				
			from_ptr += 3;
			SINTa to_len = (SINTa)(header32 & NEWLZ_ARRAY_SIZE_MASK );
			
			// I have spare bits here that're always zero :
			//RR_ASSERT( (header>>NEWLZ_ARRAY_SIZE_BITS) == 0 );
			if ( (header32>>NEWLZ_ARRAY_SIZE_BITS) != 0 ) NEWLZ_ARRAY_RETURN_FAILURE();
			
			if ( to_len > to_len_max ) NEWLZ_ARRAY_RETURN_FAILURE();
			if ( to_len > rrPtrDiff( from_end - from_ptr ) ) NEWLZ_ARRAY_RETURN_FAILURE();
					
			from_ptr += to_len;
			*pto_len = to_len;
			return rrPtrDiff( from_ptr - from );
		}
		else
		{
			RR_ASSERT( array_type != NEWLZ_ARRAY_TYPE_UNCOMPRESSED ); // uncomp handled outside
			if ( array_type >= NEWLZ_ARRAY_TYPE_COUNT )
			{
				rrPrintf_v2("newLZ_get_array : invalid array_type\n");
				NEWLZ_ARRAY_RETURN_FAILURE();
			}

			if ( 5 > rrPtrDiff( from_end - from_ptr ) ) NEWLZ_ARRAY_RETURN_FAILURE();

			// re-get as 5-byte header :
			U64 header64 = *from_ptr++;
			header64 <<= 32;
			header64 += RR_GET32_BE_UNALIGNED(from_ptr);
			from_ptr+= 4;
			RR_ASSERT( (header64>>36) == array_type );

			SINTa comp_len = (SINTa)(header64 & (NEWLZ_ARRAY_SIZE_MASK));
			if ( comp_len > rrPtrDiff( from_end - from_ptr ) )
				NEWLZ_ARRAY_RETURN_FAILURE();

			SINTa to_len = (SINTa)((header64>>NEWLZ_ARRAY_SIZE_BITS) & (NEWLZ_ARRAY_SIZE_MASK));
			to_len++;

			if ( to_len > to_len_max ) NEWLZ_ARRAY_RETURN_FAILURE();
			if ( comp_len >= to_len ) NEWLZ_ARRAY_RETURN_FAILURE();
			RR_ASSERT( comp_len < to_len );

			RR_ASSERT( comp_len <= rrPtrDiff( from_end - from_ptr ) );
			from_ptr += comp_len;

			*pto_len = to_len;
			return rrPtrDiff( from_ptr - from );
		}
	}
}

SINTa newLZ_get_array_comp(U32 array_type, U8ptr * ptr_to, const U8 * from, const U8 * from_end, SINTa * pto_len, SINTa to_len_max,
								U8 * scratch_ptr, U8 * scratch_end)
{
	RR_ASSERT( array_type != NEWLZ_ARRAY_TYPE_UNCOMPRESSED ); // uncomp handled outside
	if ( array_type >= NEWLZ_ARRAY_TYPE_COUNT )
	{
		rrPrintf_v2("newLZ_get_array : invalid array_type\n");
		NEWLZ_ARRAY_RETURN_FAILURE();
	}

	const U8 * from_ptr = from;
	RR_ASSERT( from_ptr < from_end ); // has always been checked already
	
	SINTa comp_len;
	SINTa to_len;
		
	if ( *from_ptr >= 0x80 ) // small flag
	{
		// small header
		// 3 byte header + min 1 byte payload
		//	(and I want to grab a u32) so check 4 :
		REQUIRE_FUZZ_RETURN( 4 <= rrPtrDiff( from_end - from_ptr ), -1 );
		
		U32 header = RR_GET24_BE_OVERRUNOK(from_ptr);
		from_ptr += 3;
		
		// 10 bits comp, 10 bits raw + 4 bits header

		comp_len = (SINTa)(header & NEWLZ_ARRAY_SMALL_SIZE_MASK);
		
		#if 0
		// force it to take truncated data :

		if ( from_ptr + comp_len > from_end )
		{
			comp_len = from_end - from_ptr;
			if ( comp_len <= 0 )
				NEWLZ_ARRAY_RETURN_FAILURE();
		}
		#endif			
			
		if ( comp_len > rrPtrDiff( from_end - from_ptr ) )
			NEWLZ_ARRAY_RETURN_FAILURE();

		to_len = (SINTa)((header>>NEWLZ_ARRAY_SMALL_SIZE_BITS) & (NEWLZ_ARRAY_SMALL_SIZE_MASK));
		to_len += comp_len + 1;

		if ( to_len > to_len_max ) NEWLZ_ARRAY_RETURN_FAILURE();
		// automatic :
		//if ( comp_len >= to_len ) NEWLZ_ARRAY_RETURN_FAILURE();
	}
	else
	{
		REQUIRE_FUZZ_RETURN( 5 <= rrPtrDiff( from_end - from_ptr ) , -1 );

		// re-get as 5-byte header :
		U64 header = *from_ptr++;
		header <<= 32;
		header += RR_GET32_BE_UNALIGNED(from_ptr);
		from_ptr+= 4;
		RR_ASSERT( (header>>36) == array_type );

		comp_len = (SINTa)(header & (NEWLZ_ARRAY_SIZE_MASK));
		
		#if 0
		// force it to take truncated data :

		if ( from_ptr + comp_len > from_end )
		{
			comp_len = from_end - from_ptr;
			if ( comp_len <= 0 )
				NEWLZ_ARRAY_RETURN_FAILURE();
		}
		#endif	
		
		if ( comp_len > rrPtrDiff( from_end - from_ptr ) )
			NEWLZ_ARRAY_RETURN_FAILURE();

		to_len = (SINTa)((header>>NEWLZ_ARRAY_SIZE_BITS) & (NEWLZ_ARRAY_SIZE_MASK));
		to_len++;
		
		if ( to_len > to_len_max ) NEWLZ_ARRAY_RETURN_FAILURE();
		if ( comp_len >= to_len ) NEWLZ_ARRAY_RETURN_FAILURE();
	}

	RR_ASSERT( comp_len < to_len );

	//SIMPLEPROFILE_SCOPE_N(newLZ_get_array_huff,to_len);
	// if [to] is scratch, advance scratch to not overlap :
	if ( *ptr_to == scratch_ptr )
	{
		RR_ASSERT( to_len_max <= (scratch_end - scratch_ptr) );
		if ( (scratch_end-scratch_ptr) < to_len ) NEWLZ_ARRAY_RETURN_FAILURE();
		scratch_ptr += to_len;
	}

	SINTa comp_used;

	if ( array_type == NEWLZ_ARRAY_TYPE_SPLIT )
	{	
		newlz_array_get_printf("split %d->%d { ",(int)to_len,(int)comp_len);
	
		comp_used = newLZ_get_array_split(from_ptr,comp_len,*ptr_to,to_len,scratch_ptr,scratch_end);
		
		newlz_array_get_printf(" }");
	}
	else if ( array_type == NEWLZ_ARRAY_TYPE_RLE )
	{
		newlz_array_get_printf("[rle %d->%d ",(int)to_len,(int)comp_len);
		
		comp_used = newLZ_get_array_rle(from_ptr,comp_len,*ptr_to,to_len,scratch_ptr,scratch_end);
		
		newlz_array_get_printf("]");
	}
	else if ( array_type == NEWLZ_ARRAY_TYPE_TANS )
	{
		newlz_array_get_printf("[tans %d->%d]",(int)to_len,(int)comp_len);
		comp_used = newlz_get_array_tans(from_ptr,comp_len,*ptr_to,to_len,scratch_ptr,scratch_end);
	}
	else
	{
		newlz_array_get_printf("[huff %d->%d]",(int)to_len,(int)comp_len);
		comp_used = newlz_get_array_huff(from_ptr,comp_len,*ptr_to,to_len,array_type == NEWLZ_ARRAY_TYPE_HUFF6);
	}
	
	// this also catches comp_used == -1 (error return)
	if ( comp_len != comp_used )
		NEWLZ_ARRAY_RETURN_FAILURE();

	RR_ASSERT( comp_len <= rrPtrDiff( from_end - from_ptr ) );
	from_ptr += comp_len;
	SINTa from_len_used = rrPtrDiff( from_ptr - from );

	#ifdef SPEEDFITTING
	if ( g_speedfitter_stage == 2 )
	{
		speedfitter_stage2_add_array(*ptr_to,to_len);
	}
	#endif
	
	*pto_len = to_len;
	return from_len_used;
}

//===================================================================

OODLE_NS_END

