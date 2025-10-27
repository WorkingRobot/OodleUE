// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.



OODLE_NS_START

//#define NEWLZF_ASSERT_HEAVY	RR_ASSERT
#define NEWLZF_ASSERT_HEAVY(x)

//===========================================================

struct newLZF_chunk_arrays
{
	// to verify we're on the right arrays :
	U8 * chunk_ptr;
	U8 * scratch_ptr;

	// made by phase1, consumed by phase 2:
	
	const U8 * excesses_ptr;
	// excesses_ptr ends at comp_end EXCEPT on in-place data, where it is copied to scratch
	const U8 * excesses_end;
	
	// const pointers here because these might be in the compressed dat
	//  so really you can't touch them :
	
	const U8 * packets_ptr;
	SINTa packets_count;
	SINTa packets_count1;
	
	const U8 * literals_ptr;
	const U8 * literals_end;
	
	const U8 * off16_ptr;
	const U8 * off16_end;
		
	// non-const because this is always in scratch :
	
	U32 * escape_offsets1;
	U32 * escape_offsets2;
	SINTa escape_offsets_count1;
	SINTa escape_offsets_count2;
	
	// set for each parse chunk :
	U32 * escape_offsets_ptr;
	U32 * escape_offsets_end;
	const U8 * packets_end;
};

//==============================================================================

// newlzf packet is 1-3-4

#define NEWLZF_NUM_LAST_OFFSETS		1
#define NEWLZF_PACKET_OFFSETS_COUNT	(NEWLZF_NUM_LAST_OFFSETS+1)

#define NEWLZF_PACKET_LRL_COUNT		8
#define NEWLZF_PACKET_ML_COUNT		16

#define NEWLZF_PACKET_LRL_MAX	(NEWLZF_PACKET_LRL_COUNT-1)
#define NEWLZF_PACKET_ML_MAX	(NEWLZF_PACKET_ML_COUNT-1)

#define NEWLZF_PACKET_COUNT		(NEWLZF_PACKET_OFFSETS_COUNT*NEWLZF_PACKET_LRL_COUNT*NEWLZF_PACKET_ML_COUNT)

// NEWLZF_PACKET_LRL_COUNT * NEWLZF_PACKET_ML_COUNT 
#define NEWLZF_PACKET_LO_FLAG		128

RR_COMPILER_ASSERT(	NEWLZF_PACKET_COUNT == 256 ); // packet fits in U8
		
// min offset 8 is for the LRL sub SIMD
#define NEWLZF_MIN_OFFSET		8

#define NEWLZF_OFFSET_FOURBYTE_SHIFT		22
#define NEWLZF_OFFSET_FOURBYTE_MASK			((1<<NEWLZF_OFFSET_FOURBYTE_SHIFT) - 1)
#define NEWLZF_OFFSET_FOURBYTE_THRESHOLD	((1<<24) - (1<<NEWLZF_OFFSET_FOURBYTE_SHIFT))
// offset < NEWLZF_OFFSET_FOURBYTE_THRESHOLD goes in 3 bytes
// offset >= NEWLZF_OFFSET_FOURBYTE_THRESHOLD goes in 4 bytes
// THRESHOLD == 0xF00000

/**

NEWLZF_OFF24_MML : MML for 24-bit offset match
this is a match that goes in the escape pathway

needs to be long enough to make this rare in the decoder

affects space & speed

**/

// this is in the format and sets the minimum that OFF24_MML can be :
// @@ - it's a bit of a waste to use the same value here for Selkie & Mermaid
//	  Selkie has a higher off24 MML
//	  it means some values of the packet byte can never be used
#define NEWLZF_OFF24_MML_DECODE		8

/**

LRL/ML EXCESS parameters

when should a long LRL/ML be sent with the escape mechanism
instead of as a sequence of packets

affects space & speed

NEWLZF_LRL_EXCESS is in the format; eg. it can't be changed once decoder is locked down
	(because it's added on when packet==0 escape is signaled)
	
NEWLZF_ML_EXCESS is NOT in the format, it's just an encoder-side parse choice tweak

-> changed 06/30 -> ML_EXCESS is in the format now for special packet value == 1

**/

// lrl >= NEWLZF_LRL_EXCESS is the trigger
//   64 makes sense here as it's 9*7+1
//	next value down would be 57 , then 50
// NEWLZF_LRL_EXCESS is in the format , cannot change it
#define NEWLZF_LRL_EXCESS	64

// ml >= NEWLZF_ML_EXCESS is the trigger
//	at 60 ml can go in 4 packets
//	61 is the threshold to take 5 packets

//#define NEWLZF_ML_EXCESS	61
//#define NEWLZF_ML_EXCESS	76
#define NEWLZF_ML_EXCESS	91
// 6*15+1 = 91


#define newlzf_copy16(to,from)	lz_copy8(to,from); lz_copy8(to+8,from+8)

#define NEWLZF_ONE_SIMPLE_PACKET(packet) do { \
		RR_ASSERT( packet >= 24 ); \
		int lrl = packet & 0x7; \
		int ml = (packet >> 3) & 0xF; \
		S32 next_offset = -(S32) RR_GET16_LE(off16_ptr); \
		S32 offset_mask = (S32)(packet>>7)-1; \
		newlz_literals_copy8<NEWLZF_DECODE_LITERALS_TYPE>(to_ptr,to_ptr+neg_offset,literals_ptr); \
		to_ptr += lrl; literals_ptr += lrl; \
		neg_offset ^= (next_offset ^ neg_offset) & offset_mask; \
		off16_ptr += offset_mask&2; \
		const U8 * mp = to_ptr + neg_offset; \
		NEWLZF_FIRST_CHUNK_FUZZ( mp >= window_base ); \
		newlzf_copy16(to_ptr,mp); \
		to_ptr += ml; \
	} while(0)

#define NEWLZF_ONE_SIMPLE_PACKET_TO_PTR_SAFE(packet) do { \
		RR_ASSERT( packet >= 24 ); \
		int lrl = packet & 0x7; \
		int ml = (packet >> 3) & 0xF; \
		S32 next_offset = -(S32) RR_GET16_LE(off16_ptr); \
		S32 offset_mask = (S32)(packet>>7)-1; \
		REQUIRE_FUZZ_RETURN( (to_ptr+8 <= overrun_chunk_end), NULL ); \
		newlz_literals_copy8<NEWLZF_DECODE_LITERALS_TYPE>(to_ptr,to_ptr+neg_offset,literals_ptr); \
		to_ptr += lrl; literals_ptr += lrl; \
		neg_offset ^= (next_offset ^ neg_offset) & offset_mask; \
		off16_ptr += offset_mask&2; \
		const U8 * mp = to_ptr + neg_offset; \
		NEWLZF_FIRST_CHUNK_FUZZ( mp >= window_base ); \
		REQUIRE_FUZZ_RETURN( (to_ptr+16 <= overrun_chunk_end), NULL ); \
		newlzf_copy16(to_ptr,mp); \
		to_ptr += ml; \
	} while(0)

// from pointer is advanced
static RADFORCEINLINE SINTr newlzf_getv(U8cptr & from,const U8 * end)
{
	// @@ yuck - on corrupt data this just returns 0, doesn't abort decompression
	if ( from >= end ) return 0;
	SINTr b = *from++;
	if ( b > 251 )
	{
		if ( from+2 > end ) return 0;
		SINTr up = RR_GET16_LE(from);
		from += 2;
		b += (up<<2);
	}
	return b;
}



//===========================================================

extern const U8 * newLZF_decode_parse_raw_sse4(
	U8 * chunk_ptr, SINTa chunk_len, U8 * overrun_chunk_end,
	U8 * window_base,
	const U8 * comp_end,
	newLZF_chunk_arrays * arrays,
	S32 * p_neg_offset
	);
	
extern const U8 * newLZF_decode_parse_sub_sse4(
	U8 * chunk_ptr, SINTa chunk_len, U8 * overrun_chunk_end,
	U8 * window_base,
	const U8 * comp_end,
	newLZF_chunk_arrays * arrays,
	S32 * p_neg_offset
	);
	
extern const U8 * newLZF_decode_parse_raw_sse4_first(
	U8 * chunk_ptr, SINTa chunk_len, U8 * overrun_chunk_end,
	U8 * window_base,
	const U8 * comp_end,
	newLZF_chunk_arrays * arrays,
	S32 * p_neg_offset
	);
	
extern const U8 * newLZF_decode_parse_sub_sse4_first(
	U8 * chunk_ptr, SINTa chunk_len, U8 * overrun_chunk_end,
	U8 * window_base,
	const U8 * comp_end,
	newLZF_chunk_arrays * arrays,
	S32 * p_neg_offset
	);
	
extern SINTa newlzf_unpack_escape_offsets_sse4(const U8 * comp_base, const U8 * comp_end,
	U32 * offsets, SINTa offset_count , SINTa max_offset);

//===========================================================

// NEWLZF_ESCAPE_OFFSET_PAD_ZERO_COUNT must be >= NEWLZF_ESCAPE_PREFETCH_INDEX
//	because we index ahead without checking
#define NEWLZF_ESCAPE_OFFSET_PAD_ZERO_COUNT	8

// how far ahead to prefetch :
// @@ TODO : probably should be tuned differently per-platform
//	-> currently tuned for Jaguar/mobile phones
//	-> something in [2-3] looks best
//extern S32 g_Oodle_Debug_TweakValue;
//#define NEWLZF_ESCAPE_PREFETCH_INDEX	g_Oodle_Debug_TweakValue // (--dt)
//#define NEWLZF_ESCAPE_PREFETCH_INDEX	0	// 0 = prefetch the next offset
//#define NEWLZF_ESCAPE_PREFETCH_INDEX	1
//#define NEWLZF_ESCAPE_PREFETCH_INDEX	2
#define NEWLZF_ESCAPE_PREFETCH_INDEX	3 // good!
//#define NEWLZF_ESCAPE_PREFETCH_INDEX	4

/**

tested 2 vs 3 on PC ; no perceivable difference
prefetch on PC doesn't make a huge difference anyway
it's really the slow-RAM platforms where it's important

NEWLZF_ESCAPE_PREFETCH_INDEX :

files that are super escape-heavy (webster) want a very far prefetch ahead
	-> very strong jump from 0 to 2
files that are not so escape-heavy (silesia_mozilla) aren't so sensitive
	but prefer 1 or 2

**/

OODLE_NS_END
