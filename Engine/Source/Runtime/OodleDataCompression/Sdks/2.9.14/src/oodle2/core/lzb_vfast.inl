// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.



//-----------------------
// derived :

RR_COMPILER_ASSERT( FAST_HASH_DEPTH_SHIFT == 0 );

#undef FAST_HASH_FUNC
//#define FAST_HASH_FUNC(ptr,dword)	( LZB_Hash4(dword) & hash_table_mask )
// I was setting hash table bits to 17, but this only gives you 16 usable bits :
//#define FAST_HASH_FUNC(ptr,dword)	( (((dword)*2654435761U)>>16) & hash_table_mask )
#ifdef __RADPPC__
// for slow variable shift platforms :
#define FAST_HASH_FUNC(ptr,dword)	( RR_ROTR32((dword)*2654435761U,16) & ((1UL<<(hashTableSizeBits)) - 1) )
#else
// this is faster on platforms with fast variable shifts :
#define FAST_HASH_FUNC(ptr,dword)	( ((dword)*2654435761U)>>(32 - hashTableSizeBits) )
#endif


static void RR_STRING_JOIN( FAST_NAME , _Preload )(
		SINTa * hashTableMem,
		int hashTableSizeBits,
		const U8 * rawBufStart,
		SINTa dictionaryBackup )
{
	U16 * hashTable16 = (U16 *) hashTableMem;
	
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
		//hashTable[ hash ] = pos;
		hashTable16[ hash ] = (U16)pos;
		ptr += step;
	}
	RR_ASSERT_ALWAYS( ptr == rawBufStart );
}


static SINTa RR_STRING_JOIN( FAST_NAME , _Sub ) (const U8 * raw, SINTa rawLen, U8 * comp, 
									const OodleLZB_CompressFast_Context * fh,
									const U8 * blockMatchLimitPtr, LZQuantumHeader * pQH)
{
	SIMPLEPROFILE_SCOPE_N(lzbfast_sub,rawLen);
	//THREADPROFILEFUNC();
	
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

	//SINTa * hashTable = fh->m_tablePos;
	U16 * hashTable16 = (U16 *) fh->m_tablePos;
	int hashTableSizeBits = fh->m_tableSizeBits;
	
	// windowsBase is used for backup checking :
	const U8 * windowBase = fh->m_windowBase;
	SINTa windowMask = fh->m_windowMask;
	RR_UNUSED_VARIABLE(windowMask);
	
	#if FAST_SLIDING_WINDOW	
	#error
	#else
	RR_ASSERT( windowMask == -1 );
	RR_ASSERT( ! fh->IsSlidingWindow() );
	#endif
	
	const U8 * zeroPosPtr = fh->m_zeroPosPtr;
	RR_ASSERT( (SINTa)rp >= (SINTa)zeroPosPtr ); // zeroPosPtr can go negative!
	RR_ASSERT( zeroPosPtr + fh->m_posThisReset == raw );
		
	//#if ! FAST_SLIDING_WINDOW	
	RR_ASSERT( ! fh->IsSlidingWindow() );
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
		U32 rp32 = RR_GET32_NATIVE_UNALIGNED(rp);
		U32 hash = FAST_HASH_FUNC(rp, rp32 );
		const U8 * hashrp;
		S32 matchOff;	
		UINTr failedMatches;
		
		#if 0
		// first loop with step = 1
		// seems to hurt on slower CPUs when set to an equal # of 1-steps
		//	(of course helps speed if you do fewer 1-steps, but hurts compression)
		//	(you can cut it down to just two 1-steps and unroll them)
		// @@ step1count ?
		//int step1count = (1<<FAST_MULTISTEP_LITERALS_SHIFT); // full count
		//int step1count = (1<<(FAST_MULTISTEP_LITERALS_SHIFT-1)); // half count
		int step1count = (1<<FAST_MULTISTEP_LITERALS_SHIFT) - 3;
		
		while(step1count--)
		{	    				
			SINTa curpos = rrPtrDiff(rp - zeroPosPtr);	
			RR_ASSERT( curpos >= 0 );
			
			U16 hashpos16 = hashTable16[hash];
			hashTable16[ hash ] = (U16) curpos;
			
			matchOff = (U16)(curpos - hashpos16);
			RR_ASSERT( matchOff >= 0 );
			
			// hide the check for offset == 0 in the late branch
			if ( matchOff <= LZB_MAX_OFFSET )
			{
				hashrp = rp - matchOff;
						
				const U32 hashrp32 = RR_GET32_NATIVE_UNALIGNED(hashrp);

				if ( rp32 == hashrp32 && matchOff != 0 )
				{
					goto found_match;
				}
			}
			                               		
			//-------------------------------
			if ( ++rp >= rpEndSafe )
				goto done;
				
			rp32 = RR_GET32_NATIVE_UNALIGNED(rp);
			hash = FAST_HASH_FUNC(rp, rp32 );
		}		    	

		// step starts at 2 :
		failedMatches = (2<<FAST_MULTISTEP_LITERALS_SHIFT);
		#else
		failedMatches = (1<<FAST_MULTISTEP_LITERALS_SHIFT) + 3;
		#endif
		
		// literals :
		for(;;)		
		{			    				
			SINTa curpos = rrPtrDiff(rp - zeroPosPtr);	
			RR_ASSERT( curpos >= 0 );
			
			U16 hashpos16 = hashTable16[hash];
			hashTable16[ hash ] = (U16) curpos;
			
			matchOff = (U16)(curpos - hashpos16);
			RR_ASSERT( matchOff >= 0 );
			
			// hide the check for offset == 0 in the late branch
			if ( matchOff <= LZB_MAX_OFFSET )
			{
				hashrp = rp - matchOff;
						
				const U32 hashrp32 = RR_GET32_NATIVE_UNALIGNED(hashrp);

				if ( rp32 == hashrp32 && matchOff != 0 )
				{
					goto found_match;
				}
			}
			                               		
			//-------------------------------
			
			UINTr stepLiterals = (failedMatches>>FAST_MULTISTEP_LITERALS_SHIFT);
			RR_ASSERT( stepLiterals >= 1 );

			++failedMatches;
			
			rp += stepLiterals;
		
			if ( rp >= rpEndSafe )
				goto done;
				
			rp32 = RR_GET32_NATIVE_UNALIGNED(rp);
			hash = FAST_HASH_FUNC(rp, rp32 );
		}
		
		//-------------------------------
		found_match:
		
		// found something

		#if 1 // DO BACKUP?
		// back up start of match that we missed due to stepLiterals (or cache table)
		// make sure we don't read off the start of the array (windowBase)
		
		/*
		
		doing backup on a small hash table is better (space/speed) than no backup on a big hash table

		*/
		
		/*
		// 138.52 mb/s
		// NOTE(fg): Only try backup if we had at least one literal.
		// This is a fairly major win.
		if ( rp > literals_start )
		{
			while ( hashrp > windowBase && rp[-1] == hashrp[-1] )
			{
				hashrp--;
				rp--;
				if ( rp == literals_start )
					break;
			}
		}
		S32 cur_lrl = rrPtrDiff32(rp - literals_start);
		/*/
		
		// alternative backup using counter :
		// 139.70 mb/s
		S32 cur_lrl = rrPtrDiff32(rp - literals_start);
		int neg_max_backup = - RR_MIN(cur_lrl , rrPtrDiff32(hashrp - windowBase) );
		//int neg_max_backup = RR_MAX(-cur_lrl , rrPtrDiff32(windowBase - hashrp) );
		int neg_backup = -1;
		if( neg_backup >= neg_max_backup && rp[neg_backup] == hashrp[neg_backup] )
		{
			neg_backup--;
			while( neg_backup >= neg_max_backup && rp[neg_backup] == hashrp[neg_backup] )
			{
				neg_backup--;
			}
			neg_backup++;
			rp += neg_backup;
			cur_lrl += neg_backup;
			RR_ASSERT( cur_lrl >= 0 );
			RR_ASSERT( cur_lrl == rrPtrDiff32(rp - literals_start) );
		}
		/**/
		
		#else

		S32 cur_lrl = rrPtrDiff32(rp - literals_start);
		
		#endif		

		// catch expansion while writing :
		if ( cp+cur_lrl >= compExpandedPtr )
		{
			return rawLen+1;
		}
		
		RR_ASSERT( matchOff >= 1 );
	
		//---------------------------------------
		// find rest of match len
		// save pointer to start of match
		// walk rp ahead to end of match
		const U8 * match_start = rp;
		rp += 4;
				
		while( rp < rpEndSafe )
		{
			UINTr big1 = *((UINTr *)(rp));
			UINTr big2 = *((UINTr *)(rp-matchOff));
	    
			if ( big1 == big2 )
			{
				rp += sizeof(UINTr);
				continue;
			}
			else
			{
				rp += GetNumBytesZeroNeverAllR(big1^big2);  
				break;
			}
		}
		rp = RR_MIN(rp,rpMatchEnd);
		S32 matchLen = rrPtrDiff32( rp - match_start );
		
		//===============================================
		// chose a match
		//	output LRL (if any) and match
		
		cp = LZB_Output(cp,cur_lrl,literals_start,matchLen,matchOff);
						
		// skip the match :
		literals_start = rp;
		
		if ( rp >= rpEndSafe )
			goto done;	
		
		// note: we skipped over the match without adding ANY hash entries
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
