// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once
#ifndef __RADRR_LOGUTIL_H__
#define __RADRR_LOGUTIL_H__

//#include "rrLogCB.h"
#include "rrlog.h"

OODLE_NS_START

//---------------------------------------------------
// helpers :

void rrsprintfcommas(char * into,U64 number);
void rrsprintfcommasf(char * into,F64 number,int numdecimals);

void rrPrintfCommas(U64 number);
void rrPrintfCommasf(F64 number,int numdecimals);

// puts a 64-bit number into 9-char numeric output :
//	if < 1B , = %9d , else adds "k" or "m" suffix
void rrsprintf9km(char * into,S64 number);

// rrPrintfBinary prints bits with the MSB first
void rrPrintfBinary(U32 val,int bits);

// these print arrays like bin2c :
void rrPrintfBin2C(const U8 * data,int size,const char * name);
void rrPrintfS32Array(const S32 * data,int size,const char * name,int columns,int width);
void rrPrintfF32Array(const F32 * data,int size,const char * name,int columns,int width,int decimals);

// rrPrintStringWithLength prints a string without a terminating null
void rrPrintStringWithLength(const char *data, S32 len);


// example :
//	rrPrintfArrayGeneric(lo_state_codes,sizeof(lo_state_codes[0]),false,LO_NUM_STATES,"lo_state_codes",8,"0x%08X","U32");
// if not dataBits64 then data is 32 bit
void rrPrintfArrayGeneric(const void * data,int dataStride,rrbool dataBits64,
		int size,const char * name,int columns,
		const char * dataFmt,
		const char * dataTypeLabel);
		

//-----------------------------------

#ifdef __cplusplus

//=================================================
// template helper to print an array like C code :

template <typename T>
static void rrPrintfArray(const T * table,const char * typedesc,const char * name,const int count,const char * printfFmt)
{
	rrPrintf("%s %s[%d] = { \n\t",typedesc,name,count);

	for(int i=0;i<count;i++)
	{
		rrPrintf(printfFmt,table[i]);
		if ( i < count-1 )
			rrPrintf(", ");
			
		if ( (i%40) == 39 )
			rrPrintf("\n\t");
	}

	rrPrintf("\n};\n\n");
	
}

#define RR_PRINTF_ARRAY(var,typedesc,printfmt) rrPrintfArray(var,typedesc,#var,RR_ARRAY_SIZE(var),printfmt)
#define RR_PRINTF_ARRAY_INT(var)	RR_PRINTF_ARRAY(var,"const int","%d")
#define RR_PRINTF_ARRAY_FLOAT(var)	RR_PRINTF_ARRAY(var,"const float","%f")

//=================================================
// rrprintfvar ; similar to cblib/lprintfvar
//	just uses function overloads

static void rrprintfvalue(S8 val ) { rrprintf("%d",(int)val); }
static void rrprintfvalue(U8 val ) { rrprintf("%u",(int)val); }
static void rrprintfvalue(S16 val ) { rrprintf("%d",(int)val); }
static void rrprintfvalue(U16 val ) { rrprintf("%u",(int)val); }
static void rrprintfvalue(S32 val ) { rrprintf("%d",val); }
static void rrprintfvalue(U32 val ) { rrprintf("%u",val); }
static void rrprintfvalue(S64 val ) { rrprintf(RR_S64_FMT,val); }
static void rrprintfvalue(U64 val ) { rrprintf(RR_U64_FMT,val); }
static void rrprintfvalue(float val  ) { rrprintf("%f",val); }
static void rrprintfvalue(double val ) { rrprintf("%g",val); }
static void rrprintfvalue(const char * val ) { rrprintf("%s",val); }

#ifdef RR_INTA_TYPES_SEPARATE
static void rrprintfvalue(SINTa val) { rrprintfvalue((RR_STRING_JOIN(S,RAD_PTRBITS))val); }
static void rrprintfvalue(UINTa val) { rrprintfvalue((RR_STRING_JOIN(U,RAD_PTRBITS))val); }
#endif

// rrprintfvar outputs "name : value\n"
#define rrprintfvar(v)	do { rrprintf("%s : ",RR_STRINGIZE(v)); rrprintfvalue(v); rrprintf("\n"); } while(0)

// rrprintfcvar outputs "type name = value;\n" , like a C variable
#define rrprintfcvar(t,v)	do { rrprintf("%s %s = ",t,RR_STRINGIZE(v)); rrprintfvalue(v); rrprintf(";\n"); } while(0)
#define rrprintfcvar_int(v)	rrprintfcvar("int",v)
		
//===============================================

#define RR_64_FMT_16HEX	RR_64_FMT_HEX

#endif // __cplusplus

OODLE_NS_END

#endif // __RADRR_LOGUTIL_H__
