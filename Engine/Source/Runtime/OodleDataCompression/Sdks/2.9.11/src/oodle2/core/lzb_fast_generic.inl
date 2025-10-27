// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.



//-----------------------
// derived :

/*
#define FAST_HASH_BITS          (FAST_HASH_TOTAL_BITS-FAST_HASH_DEPTH_SHIFT)
#define FAST_HASH_SIZE          (1<<FAST_HASH_BITS)
#define FAST_HASH_MASK          (FAST_HASH_SIZE-1)
*/

#undef FAST_HASH_DEPTH
#define FAST_HASH_DEPTH         (1<<FAST_HASH_DEPTH_SHIFT)
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



static void RR_STRING_JOIN( FAST_NAME , _Preload )(
		SINTa * hashTable,
		int hashTableSizeBits,
		const U8 * rawBufStart,
		SINTa dictionaryBackup )
{
	#if FAST_HASH_DEPTH > 1
	int hashCycle = 0;
	#endif

	U32 hash_table_mask = (1U<<(hashTableSizeBits - FAST_HASH_DEPTH_SHIFT)) - 1;

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
		hashTable[ FAST_HASH_INDEX(hash,hashCycle) ] = rrPtrDiff(ptr - rawBufStart);
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
	    
	SINTa * hashTable = fh->m_tablePos;
	int hashTableSizeBits = fh->m_tableSizeBits;
	U32 hash_table_mask = (1U<<(hashTableSizeBits - FAST_HASH_DEPTH_SHIFT)) - 1;
	
	const U8 * windowBase = fh->m_windowBase;
	SINTa windowMask = fh->m_windowMask;
	RR_UNUSED_VARIABLE(windowMask);
	
	#if FAST_SLIDING_WINDOW	
	// instead of maxOffset, use minMatchPos
	//	that way as you go through the chunk, maxOffset increases
	//	(this is for the sliding window memcpy situation)
	SINTa minMatchPos = fh->m_posThisReset - fh->m_maxOffset;
	// could be false if window is bigger than buf ?
	//RR_ASSERT( fh->IsSlidingWindow() );
	#else
	RR_ASSERT_ALWAYS( windowMask == -1 );
	RR_ASSERT( ! fh->IsSlidingWindow() );
	#endif
	
	const U8 * zeroPosPtr = fh->m_zeroPosPtr;
	RR_ASSERT( (SINTa)rp >= (SINTa)zeroPosPtr ); // zeroPosPtr can go negative!
	RR_ASSERT( zeroPosPtr + fh->m_posThisReset == raw );
		
	//#if ! FAST_SLIDING_WINDOW	
	if ( ! fh->IsSlidingWindow() )
	{
		if ( rp == zeroPosPtr ) // && ( fh->m_dicBase == raw || fh->m_dicBase == NULL ) )
		{
			// hash table has pos 0 in it
			//	step ahead one :	
			rp++;
		}
	}
	//#endif
	
	#ifdef FAST_MULTISTEP_LITERALS_SHIFT
	S32 failedMatches = 0;
	#endif
			
	while(rp<=rpEndSafe)
	{
		SINTa curpos = rrPtrDiff(rp - zeroPosPtr);	
		RR_ASSERT( curpos >= 0 );
	
    	S32 matchLen = 0;
    	S32 matchOff = 0;
    	
        U32 rp32 = RR_GET32_NATIVE_UNALIGNED(rp);
		U32 hash = FAST_HASH_FUNC(rp, rp32 );
		
		#if FAST_HASH_DEPTH == 1
		SINTa hashpos = hashTable[ FAST_HASH_INDEX(hash,0) ];
		RR_ASSERT( hashpos < curpos );
		
		#if FAST_SLIDING_WINDOW
		{
			if ( hashpos >= minMatchPos )
			{
				matchLen = LZB_MatchLen_Windowed(rp,rpMatchEnd,hashpos,windowBase,windowMask);
				matchOff = S32_checkA(curpos - hashpos);
			}
		}
		#else
		{
			const U8 * hashrp = windowBase + hashpos;
			const U32 hashrp32 = RR_GET32_NATIVE_UNALIGNED(hashrp);

			matchLen = getmatchlen_mml4_two32(rp32,hashrp32,rp,hashrp,rpMatchEnd);
			matchOff = rrPtrDiff32( rp - hashrp );
			RR_ASSERT( matchOff >= 1 );
		}
		#endif
				
		#else
		
		for(int d=0;d<FAST_HASH_DEPTH;d++)
		{
			SINTa hashpos = hashTable[ FAST_HASH_INDEX(hash,d) ];
			RR_ASSERT( hashpos < curpos );
			
			#if FAST_SLIDING_WINDOW
			if ( hashpos >= minMatchPos )
			{			
				S32 ml = LZB_MatchLen_Windowed(rp,rpMatchEnd,hashpos,windowBase,windowMask);
				if ( ml > matchLen )
				{
					S32 off = S32_checkA(curpos - hashpos);
					if ( off <= LZB_MAX_OFFSET )
					{
						matchLen = ml;
						matchOff = off;
					}
				}
			}
			#else
			RR_ASSERT( windowMask == -1 );
			//RR_ASSERT( hashpos >= 0 );
			const U8 * hashrp = windowBase + hashpos;
			matchLen = LZB_FastCheckMatch(rp32,rp,rpMatchEnd,hashrp,windowBase,matchLen,&matchOff);
			#endif
		}
		#endif
    	            
		#ifdef DO_FAST_2ND_HASH

		U32 hash2 = ( LZB_SecondHash4(rp32) ) & hash_table_mask;

		for(int d=0;d<FAST_HASH_DEPTH;d++)
		{
			SINTa hashpos = hashTable[ FAST_HASH_INDEX(hash2,d) ];
			RR_ASSERT( hashpos < curpos );
			
			#if FAST_SLIDING_WINDOW
			if ( hashpos >= minMatchPos )
			{			
				S32 ml = LZB_MatchLen_Windowed(rp,rpMatchEnd,hashpos,windowBase,windowMask);
				if ( ml > matchLen )
				{
					S32 off = S32_checkA(curpos - hashpos);
					if ( off <= LZB_MAX_OFFSET )
					{
						matchLen = ml;
						matchOff = off;
					}
				}
			}
			#else
			RR_ASSERT( windowMask == -1 );
			const U8 * hashrp = windowBase + hashpos;
			matchLen = LZB_FastCheckMatch(rp32,rp,rpMatchEnd,hashrp,matchLen,&matchOff);
			#endif
		} 
		#endif
                
        //-------------------------
        // update hash now so lazy can see it :
        
		hashTable[ FAST_HASH_INDEX(hash,hashCycle) ] = curpos;
                                	            
		#ifdef DO_FAST_2ND_HASH
		// do NOT step hashCycle !
		//hashCycle = (hashCycle+1)&FAST_HASH_CYCLE_MASK;
		hashTable[ FAST_HASH_INDEX(hash2,hashCycle) ] = curpos;
		#endif
		
		#if FAST_HASH_DEPTH > 1
		hashCycle = (hashCycle+1)&FAST_HASH_CYCLE_MASK;
		#endif
		
		//-------------------------------
    	
    	#if FAST_HASH_DEPTH == 1
		if ( matchLen == 0 || ! LZB_IsAllowedMatch(matchLen,matchOff) )
		#else
		if ( matchLen == 0 ) // no matches
		#endif
		
		{
			#ifdef FAST_MULTISTEP_LITERALS_SHIFT
			
			++failedMatches;
			
			S32 stepLiterals = 1 + (failedMatches>>FAST_MULTISTEP_LITERALS_SHIFT);
			RR_ASSERT( stepLiterals >= 1 );
	
			rp += stepLiterals;
			
			// note : no hash update inside the literal step
			
			#else
			
			// literal :
			rp++;
			
			#endif
			
			continue;
		}
	
		// found something
	
		#ifdef FAST_MULTISTEP_LITERALS_SHIFT
		failedMatches = 0;
		#endif
		
		//-------------------------------
				
    	RR_ASSERT( matchLen >= LZB_MML );
			
		//-------------------------------
		
		// check lazy match too
		#ifdef DO_FAST_LAZY_MATCH
		if (rp< rpEndSafe)
		{
			const U8 * lazyrp = rp + 1;
			SINTa lazypos = curpos + 1;

			U32 lazyrp32 = RR_GET32_NATIVE_UNALIGNED(lazyrp);

			//S32 last_bestml=0,last_bestoff=0;
    						
			// cur_lrl > 0 always    			
						
			S32 bestml = 0;
			S32 bestoff = 0;
			
			U32 lazyHash = FAST_HASH_FUNC(lazyrp, lazyrp32 );
			for(int d=0;d<FAST_HASH_DEPTH;d++)
			{
				SINTa hashpos = hashTable[ FAST_HASH_INDEX(lazyHash,d) ];
				RR_ASSERT( hashpos < lazypos );
				
				#if FAST_SLIDING_WINDOW
				if ( hashpos >= minMatchPos )
				{			
					S32 ml = LZB_MatchLen_Windowed(lazyrp,rpMatchEnd,hashpos,windowBase,windowMask);
					if ( ml > bestml )
					{
						S32 off = S32_checkA(lazypos - hashpos);
						if ( off <= LZB_MAX_OFFSET )
						{
							bestml = ml;
							bestoff = off;
						}
					}
				}
				#else
				RR_ASSERT( windowMask == -1 );
				const U8 * hashrp = windowBase + hashpos;
				bestml = LZB_FastCheckMatch(lazyrp32,lazyrp,rpMatchEnd,hashrp,windowBase,bestml,&bestoff);
				#endif
			}
        
			#ifdef DO_FAST_2ND_HASH
			U32 lazyhash2 = LZB_SecondHash4(lazyrp32) & hash_table_mask;
			for(int d=0;d<FAST_HASH_DEPTH;d++)
			{
				SINTa hashpos = hashTable[ FAST_HASH_INDEX(lazyhash2,d) ];
				RR_ASSERT( hashpos < lazypos );
				
				#if FAST_SLIDING_WINDOW
				if ( hashpos >= minMatchPos )
				{			
					S32 ml = LZB_MatchLen_Windowed(lazyrp,rpMatchEnd,hashpos,windowBase,windowMask);
					if ( ml > bestml )
					{
						S32 off = S32_checkA(lazypos - hashpos);
						if ( off <= LZB_MAX_OFFSET )
						{
							bestml = ml;
							bestoff = off;
						}
					}
				}
				#else
				RR_ASSERT( windowMask == -1 );
				const U8 * hashrp = windowBase + hashpos;
				bestml = LZB_FastCheckMatch(lazyrp32,lazyrp,rpMatchEnd,hashrp,bestml,&bestoff);
				#endif
			}  
			#endif
					
			if ( bestml-matchLen >= 2 )
			{
				// yes take the lazy match
				
				// put a literal :
				rp++;
						                
				hashTable[ FAST_HASH_INDEX(lazyHash,hashCycle) ] = lazypos;
                                        	            
				#ifdef DO_FAST_2ND_HASH
				// do NOT step hashCycle !
				hashTable[ FAST_HASH_INDEX(lazyhash2,hashCycle) ] = lazypos;
				#endif
				
				#if FAST_HASH_DEPTH > 1
				hashCycle = (hashCycle+1)&FAST_HASH_CYCLE_MASK;
				#endif
				
				// and then drop out and do the lazy match :
				matchLen = bestml;
				matchOff = bestoff;
			}
		}
		#endif				  
		
		//===============================================
		// chose a match
		//	output LRL (if any) and match
		
		RR_ASSERT( rp+matchLen <= rpMatchEnd );
		
		#ifdef DO_FAST_UPDATE_MATCH_HASHES
		const U8 * match_start_ptr = rp;
		#endif
		
		S32 cur_lrl = rrPtrDiff32(rp - literals_start);

		// catch expansion while writing :
		if ( cp+cur_lrl >= compExpandedPtr )
		{
			return rawLen+1;
		}
			
		cp = LZB_Output(cp,cur_lrl,literals_start,matchLen,matchOff);
						
		// skip the match :
		rp += matchLen;		
		literals_start = rp;		
		
		// step & update hashes :
		//  (I already did cur pos)
		#ifdef DO_FAST_UPDATE_MATCH_HASHES
		//int step = 1;
		//int step = (curMatchLen>>4) + 1; // hurts only a little

		// don't bother if it takes us to the end :      
		//	(this check is not for speed it's to avoid the access violation)          
		if ( rp < rpEndSafe )
		{
			const U8 * ptr = match_start_ptr;
			for(;ptr<rp;ptr++)
			{
				U32 hash = FAST_HASH_FUNC( ptr, RR_GET32_NATIVE_UNALIGNED(ptr) );
				hashTable[ FAST_HASH_INDEX(hash,hashCycle) ] = rrPtrDiff( ptr - zeroPosPtr );
				//hashCycle = (hashCycle+1)&FAST_HASH_CYCLE_MASK;
				// helps a bit to NOT step cycle here
				//  the hash entries that come inside a match are of much lower quality
			}
		}
		#endif
					
	}
	
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
