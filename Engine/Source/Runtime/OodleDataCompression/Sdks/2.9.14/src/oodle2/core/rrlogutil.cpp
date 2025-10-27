// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrlogutil.h"
#include "rrmem.h"
#include <string.h>
#include "rrsprintf.h"

OODLE_NS_START

void rrsprintfcommas(char * into,U64 number)
{
	if ( number == 0 )
	{
		strcpy(into,"0");
		return;
	}

	U64 place = 1000;
	while ( number >= place )
	{
		U64 nextplace = place * 1000;
		if ( nextplace <= place )
		{
			// overflow; eg if number == -1
			strcpy(into,"U64_overflow");
			return;
		}
		place = nextplace;
	}
	// number < place and number >= place/1000
	place /= 1000;
	
	U64 remainder = number;
	
	char * ptr = into;
	bool first = true;
	
	while( place > 0 )
	{
		int cur = (int)(remainder/place);
		remainder -= cur*place;
	
		if ( ! first )
		{
			*ptr++ = ',';
			rrsprintf(ptr,"%03d",cur);		
		}
		else
		{
			first = false;
			rrsprintf(ptr,"%d",cur);
		}
		ptr += strlen(ptr);
		
		place /= 1000;
		
		if ( (ptr - into) > 30 )
			break;
	}
}

void rrsprintf9km(char * into,S64 number)
{
	if ( number < 1000000000 )
		rrsprintf(into,"%9d",(int)number);
	else if ( number < 100000000000LL )
		rrsprintf(into,"%8dk",(int)(number/1000) );
	else
		rrsprintf(into,"%8dm",(int)(number/1000000));
}
			
void rrsprintfcommasf(char * into,F64 number,int numdecimals)
{
	if ( number < 0 )
	{
		*into++ = '-';
		number = - number;
	}
	
	U64 intpart = (U64)number;
	F64 fraction = number - intpart;
	
	rrsprintfcommas(into,intpart);
	
	if ( numdecimals > 0 )
	{
		char * ptr = into + strlen(into);
		//rrsprintf(into,"%0*d",numdecimals,(int)(fraction*(10^numdecimals)));
		RR_ASSERT( fraction < 1.0 );
		for(int i=0;i<numdecimals;i++)
		{
			fraction *= 10.0;
			int v = (int)fraction;
			RR_ASSERT( v >= 0 && v < 10 );
			fraction -= v;
			*ptr++ = (char)(v + '0');
			//if ( (i%3) == 2 )
			//	*ptr++ = ','
		}
		*ptr = 0;
	}
}

void rrPrintfCommas(U64 number)
{
	char buf[64];
	rrsprintfcommas(buf,number);
	rrPrintf("%s",buf);
}

void rrPrintfCommasf(F64 number,int numdecimals)
{
	char buf[64];
	rrsprintfcommasf(buf,number,numdecimals);
	rrPrintf("%s",buf);
}


// rrPrintfBinary prints bits with the MSB first
void rrPrintfBinary(U32 val,int bits)
{
    if ( bits == 0 )
        return;
    for(--bits;bits>=0;bits--)
    {
        int b = (val>>bits)&1;

        if ( b ) rrprintf("1");
        else rrprintf("0");
    }
}

void rrPrintfBin2C(const U8 * data,int size,const char * name)
{
    rrprintf("\nstatic const int %s_size = %d;\n",name,size);
    rrprintf("static const U8 %s[] = \n",name);
    rrprintf("{\n  ");
    for(int i=0;i<size;i++)
    {
        rrprintf("0x%02X",data[i]);
        if ( i < size-1 )
            rrprintf(",");
        if ( (i%30) == 29 )
            rrprintf("\n");
    }
    rrprintf("\n};\n\n");
}

void rrPrintfArrayGeneric(const void * data,int dataStride,rrbool dataBits64,
		int size,const char * name,int columns,
		const char * dataFmt,
		const char * dataTypeLabel)
{
    rrprintf("\nstatic const int %s_size = %d;\n",name,size);
    rrprintf("static const %s %s[] = \n",dataTypeLabel,name);
    rrprintf("{\n");
    const void * data_ptr = data;
    for(int i=0;i<size;i++)
    {
        if ( (i%columns) == 0 )
			rrprintf("  ");
		
		if ( dataBits64 )
	    {
	        rrprintf(dataFmt,RR_GET64_NATIVE(data_ptr));
	    }
	    else if ( dataStride < 4 )
	    {
			U32 val;
			if ( dataStride == 2 )
				val = RR_GET16_NATIVE(data_ptr);
			else if ( dataStride == 1 )
				val = *((U8 *)data_ptr);
			else if ( dataStride == 3 )
			{
				#ifdef __RADLITTLEENDIAN__
				val = RR_GET24_LE_NOOVERRUN(data_ptr);
				#else
				val = RR_GET24_BE_NOOVERRUN(data_ptr);
				#endif
			}
			else
				val = 0;
			
	        rrprintf(dataFmt,val);
	    }
	    else
	    {
	        rrprintf(dataFmt,RR_GET32_NATIVE(data_ptr));
	    }
	    
        if ( i < size-1 )
            rrprintf(",");
        if ( (i%columns) == (columns-1) )
            rrprintf("\n");
          
        data_ptr = (const void *) ( (SINTa)data_ptr + dataStride );
    }
    if ( (size%columns) != 0 ) rrprintf("\n");
    rrprintf("};\n");
}

void rrPrintfS32Array(const S32 * data,int size,const char * name,int columns,int width)
{
    rrprintf("\nstatic const int %s_size = %d;\n",name,size);
    rrprintf("static const S32 %s[] = \n",name);
    rrprintf("{\n");
    for(int i=0;i<size;i++)
    {
        if ( (i%columns) == 0 )
			rrprintf("  ");
        rrprintf("%*d",width,data[i]);
        if ( i < size-1 )
            rrprintf(",");
        if ( (i%columns) == (columns-1) )
            rrprintf("\n");
    }
    if ( (size%columns) != 0 ) rrprintf("\n");
    rrprintf("};\n");
}

void rrPrintfF32Array(const F32 * data,int size,const char * name,int columns,int width,int decimals)
{
    rrprintf("\nstatic const int %s_size = %d;\n",name,size);
    rrprintf("static const F32 %s[] = \n",name);
    rrprintf("{\n");
    for(int i=0;i<size;i++)
    {
        if ( (i%columns) == 0 )
			rrprintf("  ");
        rrprintf("%*.*ff",width,decimals,data[i]);
        if ( i < size-1 )
            rrprintf(",");
        if ( (i%columns) == (columns-1) )
            rrprintf("\n");
    }
    if ( (size%columns) != 0 ) rrprintf("\n");
    rrprintf("};\n");
}

void rrPrintStringWithLength(const char *data, S32 len)
{
    S32 i;
    for (i=0; i < len; ++i)
        rrprintf("%c", data[i]);
}


OODLE_NS_END
