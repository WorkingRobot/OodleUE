// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrbase.h"
#include "oodlelzpub.h"
#include "newlz_speedfit.h"
#include "newlz_arrays.h"

OODLE_NS_START

/**

ivb is always in profile mode (no turbo)
skl is turbo off

**/

static RADINLINE F32 speedfit_blend(F32 ivb, F32 a57, F32 jag, F32 skl)
{
	return ( ivb + a57 + jag + skl ) * 0.25f;
}

static F32 speedfit_default_memcpy(SINTa len)
{
	F32 ivb = 28.000f + 0.125f * len;
	F32 a57 = 53.000f + 0.171f * len;
	F32 jag = 58.000f + 0.256f * len;
	F32 skl = 29.000f + 0.083f * len;

	return speedfit_blend(ivb, a57, jag, skl);
}

static F32 speedfit_default_simd_mul_s32_sub_u8(SINTa len)
{
	F32 ivb = 28.000f + 0.595f * len;
	F32 a57 = 53.000f + 1.050f * len;
	F32 jag = 62.000f + 1.179f * len;
	F32 skl = 33.000f + 0.567f * len;

	return speedfit_blend(ivb, a57, jag, skl);
}

static F32 speedfit_default_simd_interleave_8x2(SINTa len)
{
	F32 ivb = 24.000f + 0.270f * len;
	F32 a57 = 53.000f + 0.428f * len;
	F32 jag = 62.000f + 0.550f * len;
	F32 skl = 33.000f + 0.213f * len;

	return speedfit_blend(ivb, a57, jag, skl);
}

static F32 speedfit_default_get_offsets44(SINTa offsets_count)
{
	F32 ivb =  62.070f +  7.186f * offsets_count;
	F32 a57 =  43.106f +  8.891f * offsets_count;
	F32 jag = 135.580f + 12.846f * offsets_count;
	F32 skl =  62.485f +  7.566f * offsets_count;

	return speedfit_blend(ivb, a57, jag, skl);
}

static F32 speedfit_default_get_offsetsalt(SINTa offsets_count)
{
	F32 ivb =  75.901f +  8.183f * offsets_count;
	F32 a57 =  80.014f +  6.598f * offsets_count;
	F32 jag = 152.175f + 14.464f * offsets_count;
	F32 skl =  74.069f +  8.403f * offsets_count;

	return speedfit_blend(ivb, a57, jag, skl);
}

static F32 speedfit_default_get_excesses(SINTa excesses_count, SINTa excesses_u32_count)
{
	F32 ivb =  42.933f + 0.478f * excesses_count + 21.527f * excesses_u32_count;
	F32 a57 =  36.646f + 0.746f * excesses_count + 32.345f * excesses_u32_count;
	F32 jag = 115.731f + 0.815f * excesses_count + 36.682f * excesses_u32_count;
	F32 skl =  48.796f + 0.453f * excesses_count + 20.770f * excesses_u32_count;

	return speedfit_blend(ivb, a57, jag, skl);
}

static F32 speedfit_default_get_multiarrays(SINTa num_intervals, SINTa tot_raw_len)
{
	F32 ivb = 0.000f + 46.245f * num_intervals + 0.125f * tot_raw_len;
	F32 a57 = 0.000f + 76.846f * num_intervals + 0.322f * tot_raw_len;
	F32 jag = 0.000f + 45.477f * num_intervals + 0.215f * tot_raw_len;
	F32 skl = 0.000f + 41.626f * num_intervals + 0.077f * tot_raw_len;

	return speedfit_blend(ivb, a57, jag, skl);
}

static F32 speedfit_default_newlzf_unpack_escape_offsets(SINTa offsets_count)
{
	F32 ivb =  56.010f + 1.285f * offsets_count;
	F32 a57 =  33.347f + 3.369f * offsets_count;
	F32 jag = 133.394f + 2.446f * offsets_count;
	F32 skl =  67.640f + 1.032f * offsets_count;

	return speedfit_blend(ivb, a57, jag, skl);
}

static F32 speedfit_default_huff3(SINTa len, SINTa num_non_zero, SINTa num_alphabet_runs, F32 bpb)
{
	// NOTE: these speedfits don't use num_alphabet_runs. Using them would make the cost estimate
	// more accurate, but doing so requires determining the runs before the huff early-out
	// which causes a sizable encode-time regression on tiny chunks (test with 1k chunks).
	//
	// Currently not using num_alphabet_runs (see uses_huff_alphabet_runs in OodleSpeedFit)
	// for that reason.
	F32 ivb = 1880.931f + 3.243f * len + 10.960f * num_non_zero;
	F32 a57 = 2219.653f + 2.993f * len + 24.622f * num_non_zero;
	F32 jag = 2889.858f + 2.468f * len + 21.296f * num_non_zero;
	F32 skl = 2029.866f + 2.699f * len +  8.459f * num_non_zero;

	return speedfit_blend(ivb, a57, jag, skl);
}

static F32 speedfit_default_huff6(SINTa len, SINTa num_non_zero, SINTa num_alphabet_runs, F32 bpb)
{
	// NOTE: these speedfits don't use num_alphabet_runs. Using them would make the cost estimate
	// more accurate, but doing so requires determining the runs before the huff early-out
	// which causes a sizable encode-time regression on tiny chunks (test with 1k chunks).
	//
	// Currently not using num_alphabet_runs (see uses_huff_alphabet_runs in OodleSpeedFit)
	// for that reason.
	F32 ivb = 2029.917f + 2.436f * len + 10.792f * num_non_zero;
	F32 a57 = 2540.026f + 2.087f * len + 20.994f * num_non_zero;
	F32 jag = 3227.433f + 2.501f * len + 18.925f * num_non_zero;
	F32 skl = 2084.978f + 1.875f * len +  8.951f * num_non_zero;

	return speedfit_blend(ivb, a57, jag, skl);
}

static F32 speedfit_default_tans(SINTa len, SINTa num_non_zero, SINTa L)
{
	F32 ivb =  642.078f + 3.175f * len + 52.016f * num_non_zero + 1.895f * L;
	F32 a57 = 1073.963f + 2.963f * len + 77.065f * num_non_zero + 1.695f * L;
	F32 jag = 1313.768f + 3.951f * len + 78.930f * num_non_zero + 4.139f * L;
	F32 skl =  705.924f + 2.324f * len + 49.328f * num_non_zero + 1.423f * L;

	return speedfit_blend(ivb, a57, jag, skl);
}

static F32 speedfit_default_rle(SINTa len)
{
	F32 ivb = 284.970f + 0.172f * len;
	F32 a57 = 326.121f + 0.282f * len;
	F32 jag = 388.669f + 0.377f * len;
	F32 skl = 274.270f + 0.161f * len;

	return speedfit_blend(ivb, a57, jag, skl);
}

#define SPEEDFIT_PARSE_CONSTANT_TIME	200.f

static F32 speedfit_default_parse_Selkie(SINTa chunk_len, SINTa num_packets, SINTa num_escapes, SINTa num_literals)
{
	F32 ivb = SPEEDFIT_PARSE_CONSTANT_TIME + 0.371f * chunk_len + 5.259f * num_packets + 25.474f * num_escapes + 0.131f * num_literals;
	F32 a57 = SPEEDFIT_PARSE_CONSTANT_TIME + 0.414f * chunk_len + 6.678f * num_packets + 62.007f * num_escapes + 0.065f * num_literals;
	F32 jag = SPEEDFIT_PARSE_CONSTANT_TIME + 0.562f * chunk_len + 8.190f * num_packets + 75.523f * num_escapes + 0.008f * num_literals;
	F32 skl = SPEEDFIT_PARSE_CONSTANT_TIME + 0.272f * chunk_len + 5.018f * num_packets + 29.297f * num_escapes + 0.070f * num_literals;

	return speedfit_blend(ivb, a57, jag, skl);
}

static F32 speedfit_default_parse_Mermaid(SINTa chunk_len, SINTa num_packets, SINTa num_escapes)
{
	F32 ivb = SPEEDFIT_PARSE_CONSTANT_TIME + 0.363f * chunk_len + 5.393f * num_packets + 29.655f * num_escapes;
	F32 a57 = SPEEDFIT_PARSE_CONSTANT_TIME + 0.429f * chunk_len + 6.977f * num_packets + 49.739f * num_escapes;
	F32 jag = SPEEDFIT_PARSE_CONSTANT_TIME + 0.538f * chunk_len + 8.676f * num_packets + 69.864f * num_escapes;
	F32 skl = SPEEDFIT_PARSE_CONSTANT_TIME + 0.255f * chunk_len + 5.364f * num_packets + 30.818f * num_escapes;

	return speedfit_blend(ivb, a57, jag, skl);
}

static F32 speedfit_default_parse_Mermaid_subliterals(SINTa num_literals)
{
	F32 ivb = 0.324f * num_literals;
	F32 a57 = 0.433f * num_literals;
	F32 jag = 0.550f * num_literals;
	F32 skl = 0.289f * num_literals;

	return speedfit_blend(ivb, a57, jag, skl);
}

static F32 speedfit_default_parse_Kraken(SINTa chunk_len, SINTa num_packets, SINTa num_excesses)
{
	F32 ivb = SPEEDFIT_PARSE_CONSTANT_TIME + 0.405f * chunk_len + 15.213f * num_packets +  4.017f * num_excesses;
	F32 a57 = SPEEDFIT_PARSE_CONSTANT_TIME + 0.419f * chunk_len + 19.861f * num_packets + 10.898f * num_excesses;
	F32 jag = SPEEDFIT_PARSE_CONSTANT_TIME + 0.647f * chunk_len + 24.886f * num_packets +  9.685f * num_excesses;
	F32 skl = SPEEDFIT_PARSE_CONSTANT_TIME + 0.305f * chunk_len + 13.591f * num_packets +  4.394f * num_excesses;

	return speedfit_blend(ivb, a57, jag, skl);
}

static F32 speedfit_default_parse_Kraken_subliterals(SINTa num_literals)
{
	F32 ivb = 0.144f * num_literals;
	F32 a57 = 0.292f * num_literals;
	F32 jag = 0.322f * num_literals;
	F32 skl = 0.129f * num_literals;

	return speedfit_blend(ivb, a57, jag, skl);
}

static F32 speedfit_default_parse_Leviathan(SINTa chunk_len, SINTa num_packets, SINTa num_excesses)
{
	F32 ivb = SPEEDFIT_PARSE_CONSTANT_TIME + 0.407f * chunk_len + 18.920f * num_packets + 3.716f * num_excesses;
	F32 a57 = SPEEDFIT_PARSE_CONSTANT_TIME + 0.445f * chunk_len + 19.738f * num_packets + 7.407f * num_excesses;
	F32 jag = SPEEDFIT_PARSE_CONSTANT_TIME + 0.664f * chunk_len + 25.981f * num_packets + 7.029f * num_excesses;
	F32 skl = SPEEDFIT_PARSE_CONSTANT_TIME + 0.330f * chunk_len + 16.504f * num_packets + 3.000f * num_excesses;

	return speedfit_blend(ivb, a57, jag, skl);
}

static F32 speedfit_default_parse_Leviathan_literals(SINTa num_literals, int literal_mode)
{
	RR_ASSERT( literal_mode >= 0 && literal_mode < 6 );

	static const F32 ivb_time_per_literal[] =
	{
		0.131f,  0.000f,  0.554f,  1.568f,  8.869f,  3.946f
	};
	static const F32 a57_time_per_literal[] =
	{
		0.184f,  0.000f,  0.874f,  2.388f, 10.141f,  5.858f
	};
	static const F32 jag_time_per_literal[] =
	{
		0.186f,  0.000f,  1.149f,  2.626f, 11.111f,  6.167f
	};
	static const F32 skl_time_per_literal[] =
	{
		0.105f,  0.000f,  0.606f,  1.629f,  8.053f,  3.795f
	};

	F32 ivb = ivb_time_per_literal[literal_mode] * num_literals;
	F32 a57 = a57_time_per_literal[literal_mode] * num_literals;
	F32 jag = jag_time_per_literal[literal_mode] * num_literals;
	F32 skl = skl_time_per_literal[literal_mode] * num_literals;

	return speedfit_blend(ivb, a57, jag, skl);
}

static F32 speedfit_default_parse_Leviathan_packetpos(SINTa num_packets)
{
	F32 ivb = 3.988f * num_packets;
	F32 a57 = 2.820f * num_packets;
	F32 jag = 2.291f * num_packets;
	F32 skl = 4.958f * num_packets;

	return speedfit_blend(ivb, a57, jag, skl);
}

static void speedfit_default_deadlines_Kraken(OodleKrakenChunkDeadlines * deadlines, SINTa chunk_len)
{
	deadlines->whole_chunk = ARRAY_DEADLINE_HUGE;
	deadlines->literals_only = ARRAY_DEADLINE_HUGE;

	deadlines->literal = ARRAY_DEADLINE_HUGE;
	deadlines->packet = ARRAY_DEADLINE_HUGE;
	deadlines->offsets = ARRAY_DEADLINE_HUGE;
	deadlines->excesses_u8 = ARRAY_DEADLINE_HUGE;
	deadlines->parse = ARRAY_DEADLINE_HUGE;
}

static const OodleSpeedFit g_speedfit_default =
{
	false, // uses_huff_alphabet_runs

	speedfit_default_memcpy,
	speedfit_default_simd_mul_s32_sub_u8,
	speedfit_default_simd_interleave_8x2,

	speedfit_default_get_offsets44,
	speedfit_default_get_offsetsalt,
	speedfit_default_get_excesses,
	speedfit_default_get_multiarrays,
	speedfit_default_newlzf_unpack_escape_offsets,

	speedfit_default_huff3,
	speedfit_default_huff6,
	speedfit_default_tans,
	speedfit_default_rle,

	speedfit_default_parse_Selkie,
	speedfit_default_parse_Mermaid,
	speedfit_default_parse_Mermaid_subliterals,
	speedfit_default_parse_Kraken,
	speedfit_default_parse_Kraken_subliterals,
	speedfit_default_parse_Leviathan,
	speedfit_default_parse_Leviathan_literals,
	speedfit_default_parse_Leviathan_packetpos,

	speedfit_default_deadlines_Kraken,
	NULL, // recompute_Kraken_J
};

const OodleSpeedFit * speedfit_get_default()
{
	return &g_speedfit_default;
}

OODLE_NS_END
