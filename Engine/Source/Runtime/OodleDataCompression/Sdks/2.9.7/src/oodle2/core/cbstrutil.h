// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRR_CBSTRUTIL_H__
#define __RADRR_CBSTRUTIL_H__

#include "rrbase.h"
#include "oodlecore.h"

#include <string.h>

OODLE_NS_START

//-----------------------------------------------------------
// char * string utils

int get_last_number(const char * str);
int get_first_number(const char * str);

// argstr supports -oxxx or "-o xxx" or "-o" "xxx" or "-o=xxx"
//	argstr returns a direct pointer into the char * argv data
const char * argstr(int argc,const char * const argv[],int & i);
int argint(int argc,const char * const argv[],int & i);
double argfloat(int argc,const char * const argv[],int & i);

// macro_tolower should be safe and is nice for constants
#define macro_tolower(c)	( ( (c) >= 'A' && (c) <= 'Z' ) ? ( (c) | 0x20 ) : (c) )

// unsafe_tolower works on alpha chars and also leaves number digits alone
#define unsafe_tolower(c)	( (c) | 0x20 )

void strtolower(char * str);

// tokenchar promotes a char like we do for the CRC - makes it upper case & back slashes
OOINLINE char tokenchar(char c)
{
	// '/' is 47 = 0x2F
	// '\' is 92 = 0x5C
	// / is unaffected by unsafe_tolower, but \ is, so prefer /
	if ( c == '\\' )
		c = '/';

	return (char)macro_tolower(c);
	//return unsafe_tolower(c);
}

void sprintfcommas(char * into,S64 number);
void sprintfcommasf(char * into,double number,int numdecimals);
   
bool iswhitespace(char c);

const char * skipwhitespace(const char * str,bool skipeols);

// returns true if the beginning of str matches pre
bool strpresame(const char * str,const char * pre);
bool stripresame(const char * str,const char * pre);
bool strtpresame(const char * str,const char * pre);

// returns length that prefix is the same , return is in [0,MIN(strlen(str1),strlen(str2)]
int strpresamelen(const char * str1,const char * str2);
int stripresamelen(const char * str1,const char * str2);
int strtpresamelen(const char * str1,const char * str2);

// strrep changed fm to to
void strrep(char * str,char fm,char to);

// insert "fm" at to : ; returns ptr of end of insertion
char * strins(char *to,const char *fm);

// count # of occurence of 'c' in str
int strcount(const char * str,char c);

// strrchr or end
//	to go to the end of a token section
OOINLINE const char * strrchrorend(const char * str,char c)
{
	const char * ptr = strrchr(str,c);
	if ( ptr )
		return ptr;
	else
		return str + strlen(str);
}
OOINLINE char * strrchrorend(char * str,char c)
{
	char * ptr = strrchr(str,c);
	if ( ptr )
		return ptr;
	else
		return str + strlen(str);
}

// length limited strcpy ; ensures [to] has a null terminator
// unlike strncpy , does not copy chars after the null
//  (strlcpy is what you thought strncpy should be)
rrbool strlcpy(char * to,const char *fm,int to_size);

// old alias (prefer strlcpy) :
#define strncpynull strlcpy

// strlcat -VERY DIFFERENT FROM STRNCAT
// cats fm onto to
// keeps final length <= count-1
//  (ensures to is null terminated and fits in array size count)
rrbool strlcat(char * to,const char *fm,int to_size);

const char * stristr(const char * search_in,const char * search_for);
const char * strrstr(const char * search_in,const char * search_for);
const char * strristr(const char * search_in,const char * search_for);
const char * strichr(const char * search_in,const char search_for);

// find the first char the matches anything in search_set :
const char * strchrset(const char * search_in,const char * search_set);

const char * strchr2(const char * ptr,char c1,char c2);
const char * strrchr2(const char * ptr,char c1,char c2);
const char * strchreol(const char * ptr);
const char * skipeols(const char *ptr);

// const adapters
OOINLINE char * strchr2(char * ptr,char c1,char c2) { return (char *) strchr2( (const char *)ptr,c1,c2 ); }
OOINLINE char * strrchr2(char * ptr,char c1,char c2) { return (char *) strrchr2( (const char *)ptr,c1,c2 ); }
OOINLINE char * strchreol(char * ptr) { return (char *) strchreol( (const char *)ptr ); }
OOINLINE char * skipeols(char *ptr) { return (char *) skipeols( (const char *)ptr ); }

// "orend" returns strend(str) instead of NULL if not found
const char * strchrorend(const char * ptr,char c);
const char * strstrorend(const char * ptr,const char * substr);

OOINLINE char * strchrorend(char * ptr,char c) { return (char *) strchrorend((const char*)ptr,c); }
OOINLINE char * strstrorend(char * ptr,const char * substr) { return (char *) strstrorend((const char*)ptr,substr); }

OOINLINE bool iseol(char c) { return c == '\n' || c == '\r'; }

bool isallwhitespace( const char * pstr );
void killtailingwhite( char * ptr );

//strstrend : handy call for when you want to find a substr and get the pointer to the END of it :
OOINLINE const char * strstrend(const char * search_in,const char * search_for)
{
	const char * ptr = strstr(search_in,search_for);
	if ( ! ptr ) return NULL;
	return ptr + strlen(search_for);
}

OOINLINE const char * stristrend(const char * search_in,const char * search_for)
{
	const char * ptr = stristr(search_in,search_for);
	if ( ! ptr ) return NULL;
	return ptr + strlen(search_for);
}

//strstrend : handy call for when you want to find a substr and get the pointer to the END of it :
OOINLINE char * strstrend(char * search_in,const char * search_for)
{
	return (char *)strstrend( (const char *)search_in, search_for );
}

OOINLINE char * stristrend(char * search_in,const char * search_for)
{
	return (char *)stristrend( (const char *)search_in, search_for );
}

OOINLINE char * skipwhitespace(char * str, bool skipeols)
{
	return (char *)skipwhitespace( (const char *)str , skipeols);
}
OOINLINE char * stristr(char * search_in,const char * search_for)
{
	return (char *)stristr( (const char *)search_in, search_for );
}
OOINLINE char * strrstr(char * search_in,const char * search_for)
{
	return (char *)strrstr( (const char *)search_in, search_for );
}
OOINLINE char * strristr(char * search_in,const char * search_for)
{
	return (char *)strristr( (const char *)search_in, search_for );
}
	
OOINLINE const char * strend(const char * ptr) // strend points you at the null
{
	return ptr + strlen(ptr);
}
OOINLINE char * strend(char * ptr)
{
	return ptr + strlen(ptr);
}
	
OOINLINE void strcatc(char * onto,char c)
{
	char * end = strend(onto);
	RR_ASSERT( end[0] == 0 );
	end[0] = c;
	end[1] = 0;
}

int  strtcmp(const char *s1, const char *s2);
int  mystricmp(const char *s1, const char *s2);
void mystrrev(char * s);
int  mymemicmp(const void *b1, const void *b2, int n);
int  mymemtcmp(const void *b1, const void *b2, int n);

#ifdef __GNUC__
#define _stricmp strcasecmp
#endif

OOINLINE bool strisame(const char * s1,const char * s2)
{
#ifdef _MSC_VER
	return _stricmp(s1,s2) == 0;
#else
	return strcasecmp(s1,s2) == 0;
#endif
}

OOINLINE bool strsame(const char * s1,const char * s2)
{
	return strcmp(s1,s2) == 0;
}

//-----------------------------------------------------------
// simple tokenization stuff :

char * SkipCppCommentsAndWhiteSpace(const char * ptr , bool skipeols);
char * SkipCppComments(const char * ptr );
char * FindMatchingBrace(const char * start);
// MultiLineStrChr is just strchr but aware of cpp comments
char * MultiLineStrChr(const char * start,int c);
char * TokToComma(char * ptr);

// returns pointer to the string that was in quotes (without the quotes)
//	ptr is advanced past the string
char * ExtractQuotedString(char * ptr,char ** pPtr);


void killtailingwhiteandcomments( char * ptr );
bool stripresameadvance( char ** pptr , const char * match , bool skipeols);

// stripresameadvanceskipwhite :
//	returns true if string at *pptr has a preisame match with "match"
//	skips past match if so
//	also skips pre and post white
bool stripresameadvance_skipcpp( char ** pptr , const char * match , bool skipeols);

// toktoadvance :
//	destructive tokenize starting at *pptr
//	up to any char in "endset" (eg. endset is something like ")},;")
//	fills ptokchar with the char of endset that was found
//	returned pointer is the token without head&tail white
//	advances *pptr to after the endset char
//	also skips pre and post white
char * toktoadvance_skipcpp( char ** pptr, const char * endset , char * ptokchar , bool skipeols);

// if ptr is "X" return X , destructive tokenization
//	else return ptr
char * dequote_if_quoted(char * ptr);

bool isnumber(const char * ptr);

OODLE_NS_END


#endif // __RADRR_CBSTRUTIL_H__
