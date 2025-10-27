// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.



//-----------------------
// derived :

#if FAST_SLIDING_WINDOW
#error nope
#endif

/*
#define FAST_HASH_BITS          (FAST_HASH_TOTAL_BITS-FAST_HASH_DEPTH_SHIFT)
#define FAST_HASH_SIZE          (1<<FAST_HASH_BITS)
#define FAST_HASH_MASK          (FAST_HASH_SIZE-1)
*/

#undef FAST_HASH_DEPTH
#define FAST_HASH_DEPTH         (1<<FAST_HASH_DEPTH_SHIFT)

/*
#if FAST_HASH_DEPTH == 1
#error nope
#endif
*/

#undef FAST_HASH_CYCLE_MASK
#define FAST_HASH_CYCLE_MASK    (FAST_HASH_DEPTH-1)

#undef FAST_HASH_INDEX
#if FAST_HASH_DEPTH > 1
#define FAST_HASH_INDEX(h,d)    ( ((h)<<FAST_HASH_DEPTH_SHIFT) + (d) )
#else
#define FAST_HASH_INDEX(h,d)    (h)
#endif

#undef FAST_HASH_FUNC
#define FAST_HASH_FUNC(ptr,dword)	( LZB_Hash4(dword) & hash_table_mask )


#define FAST_MATCH_AHEAD(rp,matchOff)	do { \
		rp += 4; \
		while( rp < rpEndSafe )	{ \
			UINTr big1 = *((UINTr *)(rp)); \
			UINTr big2 = *((UINTr *)(rp-matchOff)); \
			if ( big1 == big2 ) \
			{ rp += sizeof(UINTr); continue; } \
			else \
			{ rp += GetNumBytesZeroNeverAllR(big1^big2); break; } \
		} rp = RR_MIN(rp,rpMatchEnd); } while(0)


static void RR_STRING_JOIN( FAST_NAME , _Preload )(
		SINTa * hashTableA,
		int hashTableSizeBits,
		const U8 * rawBufStart,
		SINTa dictionaryBackup )
{
	#if FAST_HASH_DEPTH > 1
	int hashCycle = 0;
	#endif

	U32 hash_table_mask = (1U<<(hashTableSizeBits - FAST_HASH_DEPTH_SHIFT)) - 1;

	U16 * hashTable16 = (U16 *)hashTableA;

	const U8 * ptr = rawBufStart - dictionaryBackup;
	#if FAST_MAX_DICTIONARYBACKUP_VARIABLESTEP
	int step = 16;
	SINTa numThisStep = (dictionaryBackup/2)/step;
	#endif
	for(;;)
	{
		#if FAST_MAX_DICTIONARYBACKUP_VARIABLESTEP
		//RR_COMPILER_ASSERT( (FAST_MAX_DICTIONARYBACKUP/8) <= FAST_HASH_SIZE );
		if ( --numThisStep <= 0 )
		{
			if ( ptr >= rawBufStart )
				break;
			step >>= 1;
			RR_ASSERT( step >= 1 ); 
			numThisStep = (rrPtrDiff(rawBufStart - ptr))/step;
			if ( step > 1 )
				numThisStep /= 2;
		}
		#else
		// no point in doing more than hash :
		//RR_COMPILER_ASSERT( FAST_MAX_DICTIONARYBACKUP <= FAST_HASH_SIZE );
		if ( ptr >= rawBufStart )
			break;
		const int step = 1;
		#endif
		
		U32 hash = FAST_HASH_FUNC(ptr, RR_GET32_NATIVE_UNALIGNED(ptr) );
		SINTa pos = rrPtrDiff(ptr - rawBufStart);
		hashTable16[ FAST_HASH_INDEX(hash,hashCycle) ] = (U16) pos;
		#if FAST_HASH_DEPTH > 1
		hashCycle = (hashCycle+1)&FAST_HASH_CYCLE_MASK;
		#endif
		ptr += step;
	}
	RR_ASSERT_ALWAYS( ptr == rawBufStart );
}


static SINTa RR_STRING_JOIN( FAST_NAME , _Sub ) (const U8 * raw, SINTa rawLen, U8 * comp, 
									const OodleLZB_CompressFast_Context * fh,
									const U8 * blockMatchLimitPtr, LZQuantumHeader * pQH)
{
	SIMPLEPROFILE_SCOPE_N(lzbfast_sub,rawLen);
	THREADPROFILEFUNC();
	
	//rrprintf("%s\n",RR_STRINGIZE_DELAY(FAST_NAME));
	
	U8 * cp = comp;
	U8 * compExpandedPtr = comp + rawLen;
		
	const U8 * rp = raw;
	const U8 * rpEnd = raw+rawLen;

	// we can match up to rpEnd
	//	but matches can't start past rpEndSafe

	#if LZB_END_WITH_LITERALS
	const U8 * rpMatchEnd = rpEnd - 1;
	#else
	const U8 * rpMatchEnd = rpEnd;
	#endif
	rpMatchEnd = RR_MIN(rpMatchEnd, blockMatchLimitPtr);
	
	const U8 * rpEndSafe = rpMatchEnd - LZB_MML;
	
	if ( rpEndSafe <= raw )
	{
		// can't compress
		return rawLen+1;
	}
	
	const U8 * literals_start = rp;

	#if FAST_HASH_DEPTH > 1
	int hashCycle = 0;
	#endif
	    
	SINTa * hashTableA = fh->m_tablePos;
	U16 * hashTable16 = (U16 *)hashTableA;
	
	int hashTableSizeBits = fh->m_tableSizeBits;
	U32 hash_table_mask = (1U<<(hashTableSizeBits - FAST_HASH_DEPTH_SHIFT)) - 1;
	
	const U8 * windowBase = fh->m_windowBase;
	SINTa windowMask = fh->m_windowMask;
	RR_UNUSED_VARIABLE(windowMask);
	
	RR_ASSERT( windowMask == -1 );
	RR_ASSERT( ! fh->IsSlidingWindow() );
	
	const U8 * zeroPosPtr = fh->m_zeroPosPtr;
	RR_ASSERT( (SINTa)rp >= (SINTa)zeroPosPtr ); // zeroPosPtr can go negative!
	RR_ASSERT( zeroPosPtr + fh->m_posThisReset == raw );
		
	//#if ! FAST_SLIDING_WINDOW	
	//if ( ! fh->IsSlidingWindow() )
	{
		if ( rp == zeroPosPtr ) // && ( fh->m_dicBase == raw || fh->m_dicBase == NULL ) )
		{
			// hash table has pos 0 in it
			//	step ahead one :	
			rp++;
		}
	}
	//#endif
	
	for(;;)
	{	
		S32 best_matchOff = 0; // initialization just for fucking warning
		const U8 * best_mp;
 
		#ifdef FAST_MULTISTEP_LITERALS_SHIFT   				
		S32 failedMatches = (1<<FAST_MULTISTEP_LITERALS_SHIFT) + 3;
		#endif
		
		U32 rp32 = RR_GET32_NATIVE_UNALIGNED(rp);
		U32 hash = FAST_HASH_FUNC(rp, rp32 );
		SINTa curpos;
		 
		#ifdef DO_FAST_2ND_HASH
		U32 hash2;
		#endif
			
		// literals :
		for(;;)		
		{    			
			#ifdef FAST_MULTISTEP_LITERALS_SHIFT
			S32 stepLiterals = (failedMatches>>FAST_MULTISTEP_LITERALS_SHIFT);
			RR_ASSERT( stepLiterals >= 1 );

			++failedMatches;
			#endif
			
			curpos = rrPtrDiff(rp - zeroPosPtr);	
			RR_ASSERT( curpos >= 0 );
			
			#ifdef DO_FAST_2ND_HASH
			hash2 = ( LZB_SecondHash4(rp32) ) & hash_table_mask;
			#endif

			best_mp = 0;

			for(int d=0;d<FAST_HASH_DEPTH;d++)
			{
				U16 hashpos16 = hashTable16[ FAST_HASH_INDEX(hash,d) ];
				
				S32 matchOff = (U16)(curpos - hashpos16);
				RR_ASSERT( matchOff >= 0 );
				
				if ( matchOff <= LZB_MAX_OFFSET )
				{
					const U32 hashrp32 = RR_GET32_NATIVE_UNALIGNED(rp - matchOff);

					if ( rp32 == hashrp32 && matchOff != 0 )
					{
						const U8 * mp = rp;
						FAST_MATCH_AHEAD(mp,matchOff);
						if ( mp > best_mp )
						{
							best_mp = mp;
							best_matchOff = matchOff;
						}
					}
				}
			}
	    	            
			#ifdef DO_FAST_2ND_HASH

			for(int d=0;d<FAST_HASH_DEPTH;d++)
			{
				U16 hashpos16 = hashTable16[ FAST_HASH_INDEX(hash2,d) ];
				
				S32 matchOff = (U16)(curpos - hashpos16);
				RR_ASSERT( matchOff >= 0 );
				
				if ( matchOff <= LZB_MAX_OFFSET )
				{
					const U32 hashrp32 = RR_GET32_NATIVE_UNALIGNED(rp - matchOff);

					if ( rp32 == hashrp32 && matchOff != 0 )
					{
						const U8 * mp = rp;
						FAST_MATCH_AHEAD(mp,matchOff);
						if ( mp > best_mp )
						{
							best_mp = mp;
							best_matchOff = matchOff;
						}
					}
				}
			} 
			
			#endif

			if ( best_mp != 0 )
			{
				// match
				break;
			}

			//---------------------------
			// update hash :
					
			hashTable16[ FAST_HASH_INDEX(hash,hashCycle) ] = (U16) curpos;
	                                	            
			#ifdef DO_FAST_2ND_HASH
			// do NOT step hashCycle !
			//hashCycle = (hashCycle+1)&FAST_HASH_CYCLE_MASK;
			hashTable16[ FAST_HASH_INDEX(hash2,hashCycle) ] = (U16) curpos;
			#endif
			
			#if FAST_HASH_DEPTH > 1
			hashCycle = (hashCycle+1)&FAST_HASH_CYCLE_MASK;
			#endif

			#ifdef FAST_MULTISTEP_LITERALS_SHIFT			
			rp += stepLiterals;
			#else
			rp ++;
			#endif
		
			if ( rp >= rpEndSafe )
				goto done;
				
			rp32 = RR_GET32_NATIVE_UNALIGNED(rp);
			hash = FAST_HASH_FUNC(rp, rp32 );
		
		}
		
		//-------------------------------
		//found_match:

		// found something

        //-------------------------
        // update hash now so lazy can see it :
        
        #if 1 // pretty important to compression
		hashTable16[ FAST_HASH_INDEX(hash,hashCycle) ] = (U16) curpos;
                                	            
		#ifdef DO_FAST_2ND_HASH
		// do NOT step hashCycle !
		//hashCycle = (hashCycle+1)&FAST_HASH_CYCLE_MASK;
		hashTable16[ FAST_HASH_INDEX(hash2,hashCycle) ] = (U16) curpos;
		#endif
		
		#if FAST_HASH_DEPTH > 1
		hashCycle = (hashCycle+1)&FAST_HASH_CYCLE_MASK;
		#endif
		#endif
		
				
		//-------------------------------
    	// rp is now at the *end* of the match
		
		const U8 * match_start = rp;
		rp = best_mp;
		
		//-------------------------------
		
		// check lazy match too
		#ifdef DO_FAST_LAZY_MATCH
		if (rp< rpEndSafe)
		{
			const U8 * lazyrp = match_start + 1;
			SINTa lazypos = rrPtrDiff(lazyrp - zeroPosPtr);

			U32 lazyrp32 = RR_GET32_NATIVE_UNALIGNED(lazyrp);

			S32 best_lazymatchOff = 0;  // initialization just for fucking warning	
			const U8 * best_lazymp;				
			
			U32 lazyHash = FAST_HASH_FUNC(lazyrp, lazyrp32 );
			
			#ifdef DO_FAST_2ND_HASH
			U32 lazyhash2 = LZB_SecondHash4(lazyrp32) & hash_table_mask;
			#endif
			
			best_lazymp = 0;
									
			for(int d=0;d<FAST_HASH_DEPTH;d++)
			{			
				U16 hashpos16 = hashTable16[ FAST_HASH_INDEX(lazyHash,d) ];
				
				S32 lazymatchOff = (U16)(lazypos - hashpos16);
				RR_ASSERT( lazymatchOff >= 0 );
				
				if ( lazymatchOff <= LZB_MAX_OFFSET )
				{
					const U32 hashrp32 = RR_GET32_NATIVE_UNALIGNED(lazyrp - lazymatchOff);

					if ( lazyrp32 == hashrp32 && lazymatchOff != 0 )
					{
						const U8 * mp = lazyrp;
						FAST_MATCH_AHEAD(mp,lazymatchOff);
						if ( mp > best_lazymp )
						{
							best_lazymp = mp;
							best_lazymatchOff = lazymatchOff;
						}
					}
				}
			}
        			
			#ifdef DO_FAST_2ND_HASH
			for(int d=0;d<FAST_HASH_DEPTH;d++)
			{
				U16 hashpos16 = hashTable16[ FAST_HASH_INDEX(lazyhash2,d) ];
				
				S32 lazymatchOff = (U16)(lazypos - hashpos16);
				RR_ASSERT( lazymatchOff >= 0 );
				
				if ( lazymatchOff <= LZB_MAX_OFFSET )
				{
					const U32 hashrp32 = RR_GET32_NATIVE_UNALIGNED(lazyrp - lazymatchOff);

					if ( lazyrp32 == hashrp32 && lazymatchOff != 0 )
					{
						const U8 * mp = lazyrp;
						FAST_MATCH_AHEAD(mp,lazymatchOff);
						if ( mp > best_lazymp )
						{
							best_lazymp = mp;
							best_lazymatchOff = lazymatchOff;
						}
					}
				}
			}  
			#endif
			
			// 15327362
			if ( best_lazymp >= rp+3 
			//&&
			//	match_start != literals_start + 14 )
			//	match_start != literals_start + 15 )
			)
			{
					// yes take the lazy match
					
					// put a literal :
					match_start++;
							      
					// I had a bug where lazypos was set wrong for the hash fill
					// it set it to the *end* of the normal match
					// and for some reason that helped compression WTF WTF						              
					//SINTa lazypos = rrPtrDiff(rp - zeroPosPtr); // 233647528
					// with correct lazypos : 233651228	
					
					// really this shouldn't be necessary at all
					// because I do an update of hash at all positions in the match including first!
					#if 1	 // with update disabled - 233690274			    
							                 
					hashTable16[ FAST_HASH_INDEX(lazyHash,hashCycle) ] = (U16) lazypos;
	                                        	            
					#ifdef DO_FAST_2ND_HASH
					// do NOT step hashCycle !
					hashTable16[ FAST_HASH_INDEX(lazyhash2,hashCycle) ] = (U16) lazypos;
					#endif
					
					#if FAST_HASH_DEPTH > 1
					hashCycle = (hashCycle+1)&FAST_HASH_CYCLE_MASK;
					#endif
					
					#endif
					
					// and then drop out and do the lazy match :
					//matchLen = lazymatchLen;
					best_matchOff = best_lazymatchOff;
					rp = best_lazymp;
			}
		}
		#endif			  
		
		//---------------------------------------------------
		// now chose greedy or lazy
		
		#if 1
		// back up start of match that we missed due to stepLiterals !
		// make sure we don't read off the start of the array
		// NOTE(fg): only try backup when lrl>0
		if ( match_start > literals_start )
		{
			const U8 * hashrp = match_start - best_matchOff;
			while ( hashrp > windowBase && match_start[-1] == hashrp[-1] )
			{
				hashrp--;
				match_start--;
				if ( match_start == literals_start )
					break;
			}
		}
		#endif
		
		S32 matchLen = rrPtrDiff32( rp - match_start );
		RR_ASSERT( matchLen >= 4 );
					
		//===============================================
		// chose a match
		//	output LRL (if any) and match
		
		S32 cur_lrl = rrPtrDiff32(match_start - literals_start);

		// catch expansion while writing :
		if ( cp+cur_lrl >= compExpandedPtr )
		{
			return rawLen+1;
		}
			
		cp = LZB_Output(cp,cur_lrl,literals_start,matchLen,best_matchOff);
						
		// skip the match :
		literals_start = rp;		
		
		if ( rp >= rpEndSafe )
			break;
		
		// step & update hashes :
		//  (I already did cur pos)
		#ifdef DO_FAST_UPDATE_MATCH_HASHES
		//int step = 1;
		//int step = (curMatchLen>>4) + 1; // hurts only a little

		// don't bother if it takes us to the end :      
		//	(this check is not for speed it's to avoid the access violation)          
		const U8 * ptr = match_start;
		U16 pos16 = (U16) rrPtrDiff( ptr - zeroPosPtr );
		for(;ptr<rp;ptr++)
		{
			U32 hash = FAST_HASH_FUNC( ptr, RR_GET32_NATIVE_UNALIGNED(ptr) );
			hashTable16[ FAST_HASH_INDEX(hash,hashCycle) ] = pos16; pos16++;
			//hashCycle = (hashCycle+1)&FAST_HASH_CYCLE_MASK;
			// helps a bit to NOT step cycle here
			//  the hash entries that come inside a match are of much lower quality
		}
		#endif
	}

	done:
	
	int cur_lrl = rrPtrDiff32(rpEnd - literals_start);
	#if LZB_END_WITH_LITERALS
	ASSERT_RELEASE(cur_lrl > 0 );
	#endif
		
	if ( cur_lrl > 0 )
	{
		// catch expansion while writing :
		if ( cp+cur_lrl >= compExpandedPtr )
		{
			return rawLen+1;
		}
		
		cp = LZB_OutputLast(cp,cur_lrl,literals_start);
	}
		
	SINTa compLen = rrPtrDiff( cp - comp );

	return compLen;
}


static SINTa FAST_NAME (const U8 * raw, SINTa rawLen, U8 * comp,
		void * matcher,
		const U8 * dictionaryBase, const U8 * blockMatchLimitPtr, 
		const U8 * bufferEndPtr,
		LZQuantumHeader * pQH)
{
	OodleLZB_CompressFast_Context * fh = (OodleLZB_CompressFast_Context *) matcher;
	fh->m_posThisReset = rrPtrDiff(raw - fh->m_zeroPosPtr );
	
	return RR_STRING_JOIN( FAST_NAME , _Sub )(raw,rawLen,comp,fh,blockMatchLimitPtr,pQH);
}
