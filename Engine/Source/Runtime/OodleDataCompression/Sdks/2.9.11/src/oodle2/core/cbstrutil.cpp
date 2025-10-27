// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "cbstrutil.h"

#include <ctype.h>
#include <stdlib.h>
#include "rrlog.h"
#include "rrsprintf.h"
#include "templates/rrstl.h"

OODLE_NS_START

void strtolower(char * str)
{
	// not right for UTF8 ; actually it's not terrible
	while( *str )
	{
		*str = (char) tolower(*str);
		str++;
	}
}

int get_last_number(const char * str)
{
	const char * ptr = strend(str) - 1;
	while( ptr > str && ! isdigit(*ptr) )
		ptr --;
	if ( ! isdigit(*ptr) ) return 0;
	while ( ptr > str && isdigit(ptr[-1]) ) ptr--;
	return atoi(ptr);
}
		
		
int get_first_number(const char * str)
{
	const char * ptr = str;
	while ( *ptr && ! isdigit(*ptr) )
		ptr++;
		
	return atoi(ptr);
}

bool iswhitespace(char c)
{
	//return ( c== ' ' || c== '\t' || c== '\n' || c== '\r' );
	return !! ::isspace(c);
}

const char * skipwhitespace(const char * str, bool skipeols)
{
	RR_ASSERT( str != NULL );

	if ( skipeols )
	{
		while( iswhitespace(*str) )
			str++;
	}
	else
	{
		while( iswhitespace(*str) && ! iseol(*str) )
			str++;
	}
	
	return str;
}

void killtailingwhite( char * ptr )
{
	char * pend = ptr + strlen(ptr) - 1;
	while( pend >= ptr )
	{
		if ( iswhitespace(*pend) )
		{
			*pend = 0;
		}
		else
		{
			break;
		}
		pend--;
	}
}

int strpresamelen(const char * str1,const char * str2)
{
	int matchLen = 0;
	while( str1[matchLen] == str2[matchLen] )
	{
		if ( str1[matchLen] == 0 )
			return matchLen;
		
		matchLen++;
	}
	return matchLen;
}

int stripresamelen(const char * str1,const char * str2)
{
	int matchLen = 0;
	while( toupper(str1[matchLen]) == toupper(str2[matchLen]) )
	{
		if ( str1[matchLen] == 0 )
			return matchLen;
		
		matchLen++;
	}
	return matchLen;
}

int strtpresamelen(const char * str1,const char * str2)
{
	int matchLen = 0;
	while( tokenchar(str1[matchLen]) == tokenchar(str2[matchLen]) )
	{
		if ( str1[matchLen] == 0 )
			return matchLen;
		
		matchLen++;
	}
	return matchLen;
}

bool strpresame(const char * str,const char * pre)
{
	while(*pre)
	{
		if ( *str != *pre )
			return false;
		str++;
		pre++;
	}
	return true;
}

bool stripresame(const char * str,const char * pre)
{
	while(*pre)
	{
		if ( toupper(*str) != toupper(*pre) )
			return false;
		str++;
		pre++;
	}
	return true;
}

bool strtpresame(const char * str,const char * pre)
{
	while(*pre)
	{
		if ( tokenchar(*str) != tokenchar(*pre) )
			return false;
		str++;
		pre++;
	}
	return true;
}


// note : this is actually NOT a valid strncpy
//	since strncpy is supposed to always copy "count" chars
//	this is a strlcpy
rrbool strlcpy(char * to,const char *fm,int count)
{
	if ( count <= 0 )
		return false;

	for(int len=0;len < count;len++)
	{		
		to[len] = fm[len];
		
		if ( fm[len] == 0 )
		{
			return true;
		}
	}
	
	to[count-1] = 0;
	return false;
}

rrbool strlcat(char * to,const char *fm,int count) // count is the size of "to"
{
	int len = (int)strlen(to);
	RR_ASSERT( len <= count );
	return strlcpy(to+len,fm,count-len);
}
	
const char * stristr(const char * search_in,const char * search_for)
{
	while ( *search_in )
	{
		if ( toupper(*search_in) == toupper(*search_for) )
		{
			const char * in_ptr = search_in + 1;
			const char * for_ptr= search_for + 1;
			while ( *for_ptr && toupper(*for_ptr) == toupper(*in_ptr) )
			{
				for_ptr++; in_ptr++;
			}
			if ( ! *for_ptr) 
				return search_in;
		}
		search_in++;
	}

	return NULL;
}

const char * strristr(const char * search_in,const char * search_for)
{
	const char * search_in_ptr = search_in + strlen(search_in) - 1;
	while ( search_in_ptr >= search_in )
	{
		if ( toupper(*search_in_ptr) == toupper(*search_for) )
		{
			const char * in_ptr = search_in_ptr + 1;
			const char * for_ptr= search_for + 1;
			while ( *for_ptr && toupper(*for_ptr) == toupper(*in_ptr) )
			{
				for_ptr++; in_ptr++;
			}
			if ( ! *for_ptr) 
				return search_in_ptr;
		}
		search_in_ptr--;
	}

	return NULL;
}

const char * strrstr(const char * search_in,const char * search_for)
{
	const char * search_in_ptr = search_in + strlen(search_in) - 1;
	while ( search_in_ptr >= search_in )
	{
		if ( (*search_in_ptr) == (*search_for) )
		{
			const char * in_ptr = search_in_ptr + 1;
			const char * for_ptr= search_for + 1;
			while ( *for_ptr && (*for_ptr) == (*in_ptr) )
			{
				for_ptr++; in_ptr++;
			}
			if ( ! *for_ptr) 
				return search_in_ptr;
		}
		search_in_ptr--;
	}

	return NULL;
}

const char * strichr(const char * search_in,const char search_for)
{
	while ( *search_in )
	{
		if ( toupper(*search_in) == toupper(search_for) )
		{
			return search_in;
		}
		search_in++;
	}

	return NULL;
}

const char * strchrset(const char * search_in,const char * search_set)
{
	// set flags :
	char mask[256] = { 0 };
	while( *search_set )
	{
		mask[ (U8)*search_set ] = 1;
		search_set++;
	}
	
	while( *search_in )
	{
		if ( mask[ (U8)*search_in ] )
			return search_in;
		search_in++;
	}
	
	return NULL;
}

const char * strchr2(const char * ptr,char c1,char c2)
{	
	while ( *ptr )
	{
		if ( *ptr == c1 || *ptr == c2 )
			return ptr;
		ptr++;
	}
	
	return NULL;
}

const char * strrchr2(const char * ptr,char c1,char c2)
{	
	const char * end = strend(ptr);
	end--;
	while ( end >= ptr )
	{
		if ( *end == c1 || *end == c2 )
			return end;
		end--;
	}
	
	return NULL;
}

const char * strchreol(const char * ptr)
{
	return strchr2(ptr,'\r','\n');
}

const char * skipeols(const char *ptr)
{
	while( *ptr == '\r' || *ptr == '\n' )
	{
		ptr++;
	}
	return ptr;
}

// strrep changed fm to to
void strrep(char * str,char fm,char to)
{
	while(*str)
	{
		if ( *str == fm )
			*str = to;
		str++;
	}
}

// insert "fm" at "to"
char * strins(char *to,const char *fm)
{
	size_t tolen = strlen(to);
	size_t fmlen = strlen(fm);
	char *newto,*oldto;

	newto = to+fmlen+tolen; oldto = to+tolen;
	tolen++;
	while(tolen--) *newto-- = *oldto--;

	while(fmlen--) *to++ = *fm++;

	return to;
}

// count # of occurence of 'c' in str
int strcount(const char * str,char c)
{
	int count = 0;
	while(*str)
	{
		if ( *str == c )
			count++;
		str++;
	}
	return count;
}


/*
const char * skipspaces(const char * str)
{
	ASSERT( ::isspace(' ') );
	ASSERT( ::isspace('\t') );
	ASSERT( ::isspace('\r') );
	ASSERT( ::isspace('\n') );
	ASSERT( str != NULL );
	while ( *str )
	{
		if ( !::isspace( *str ) )
		{
			return str;
		}
		str++;
	}
	return str;
}
*/

bool isallwhitespace( const char * pstr )
{
	if ( pstr==NULL )
	{
		return true;
	}

	const char * pFirstNonSpace = skipwhitespace(pstr,true);

	return ( *pFirstNonSpace == 0 );
}

const char * strchrorend(const char * ptr,char c)
{
	const char * ret = strchr(ptr,c);
	return ret ? ret : strend(ptr);
}

const char * strstrorend(const char * ptr,const char * substr)
{
	const char * ret = strstr(ptr,substr);
	return ret ? ret : strend(ptr);
}


// argstr supports -oxxx or "-o xxx" or "-o" "xxx" or "-o=xxx"
//	argstr returns a direct pointer into the char * argv data
const char * argstr(int argc,const char * const argv[],int & i)
{
	const char * cur = argv[i]+2;
	cur = skipwhitespace(cur,true);
	if ( *cur == '=' ) cur++; // support -o=r:\t.rmv style args as well
	cur = skipwhitespace(cur,true);
	if ( *cur )
	{
		return cur;
	}

	if ( i == argc-1 )
	{
		rrprintf("no int value found for arg!\n");
		i = argc;
		return 0;
	}

	i++;
	return argv[i];
}

int argint(int argc,const char * const argv[],int & i)
{
	const char * cur = argv[i];
	if ( *cur == '-' || *cur == '/' ) cur += 2;
	cur = skipwhitespace(cur,true);
	if ( *cur && isalpha(*cur) ) cur++;
	if ( *cur )
	{
		char * endptr;
		int ret = (int)strtol(cur,&endptr,10);
		if ( endptr != cur )
			return ret;
	}

	if ( i >= argc-1 )
	{
		RR_ASSERT_FAILURE("no int value found for arg!");
		return 0;
	}

	i++;
	return atoi(argv[i]);
}

double argfloat(int argc,const char * const argv[],int & i)
{
	const char * cur = argv[i];
	if ( *cur == '-' || *cur == '/' ) cur += 2;
	cur = skipwhitespace(cur,true);
	if ( *cur && isalpha(*cur) ) cur++;
	if ( *cur )
	{
		char * endptr;
		double ret = strtod(cur,&endptr);
		if ( endptr != cur )
			return ret;
	}

	if ( i >= argc-1 )
	{
		RR_ASSERT_FAILURE("no int value found for arg!");
		return 0;
	}

	i++;
	return atof(argv[i]);
}

void sprintfcommas(char * into,S64 number)
{
	if ( number < 0 )
	{
		number = -number;
		*into++ = '-';
	}
	else if ( number == 0 )
	{
		strcpy(into,"0");
		return;
	}

	S64 place = 1000;
	while ( number >= place )
	{
		place *= 1000;
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

void sprintfcommasf(char * into,double number,int numdecimals)
{
	if ( number < 0 )
	{
		*into++ = '-';
		number = - number;
	}
	
	U64 intpart = (U64)number;
	double fraction = number - intpart;
	
	sprintfcommas(into,intpart);
	
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

//================================================


char * SkipCppComments(const char * ptr )
{
	if ( ptr[0] == '/' && ptr[1] == '/' )
	{
		// C++ - style comment, skip this line 
		ptr += 2;
		while ( *ptr != 0 && *ptr != '\n' )
		{
			ptr++;
		}
	}
	return const_cast<char *>(ptr);
}

char * SkipCppCommentsAndWhiteSpace(const char * ptr , bool skipeols)
{
	for(;;)
	{
		const char * ptr2;
		ptr2 = skipwhitespace(ptr,skipeols);
		ptr2 = SkipCppComments(ptr2);
		if ( ptr == ptr2 ) return const_cast<char *>(ptr);
		ptr = ptr2;
	}
}

char * FindMatchingBrace(const char * start)
{
	const char * ptr = start;
	RR_ASSERT( *ptr == '{' );
	ptr++;
	int depth = 0;
	for(;;)
	{
		ptr = SkipCppComments(ptr);
		if ( *ptr == 0 )
		{
			//lrintf("Unmatched Braces : %s\n",start);
			return NULL;
		}
		if ( *ptr == '{' )
		{
			depth++;
		}
		if ( *ptr == '}' )
		{
			if ( depth == 0 )
			{
				// that's it
				ptr++;
				return const_cast<char *>(ptr);
			}
			depth--;
		}
		ptr++;
	}
}

char * MultiLineStrChr(const char * start,int c)
{
	const char * ptr = start;
	for(;;)
	{
		if ( *ptr == c )
			return const_cast<char *>(ptr);
		ptr = SkipCppComments(ptr);
		if ( *ptr == 0 )
		{
			return NULL;
		}
		ptr++;
	}
}

char * TokToComma(char * ptr)
{
	char * comma = strchr(ptr,',');
	if ( comma == NULL )
		return NULL;

	// skip over { } ; eg. handle "{a,b,c}," - I want to go to the comma
	//	after the ending brace, not any , inside
	char * brace = strchr(ptr,'{');
	if ( brace != NULL && brace < comma )
	{
		char * brace2 = FindMatchingBrace(brace);
		if ( brace2 == NULL )
		{
			return NULL;
		}
		comma = strchr(brace2,',');
		if ( comma == NULL )
			return NULL;
	}

	*comma = 0;
	return comma+1;
}

// returns pointer to the string that was in quotes (without the quotes)
//	ptr is advanced past the string
char * ExtractQuotedString(char * ptr,char ** pPtr)
{
	ptr = strchr(ptr,'\"');
	if ( ! ptr )
		return NULL;
	ptr++;
			
	char * name = ptr;
	
	ptr = strchr(ptr,'\"');
	if ( ! ptr )
		return NULL;
	*ptr = 0;
	ptr++;
	
	if ( pPtr )
		*pPtr = ptr;
	
	return name;
}

char * dequote_if_quoted(char * ptr)
{
	char * dequoted = ExtractQuotedString(ptr,NULL);
	if ( dequoted ) return dequoted;
	else return ptr;
}


// -999.999
bool isnumber(const char * ptr)
{
	ptr = skipwhitespace(ptr,false);
	if ( *ptr == '-' || *ptr == '+' ) ptr++;
	while( isdigit(*ptr) ) ptr++;
	if ( *ptr == '.' ) ptr++;
	while( isdigit(*ptr) ) ptr++;
	if ( tolower(*ptr) == 'e' )
	{
		// optional exponent
		ptr++;
		if ( *ptr == '-' || *ptr == '+' ) ptr++;
		while( isdigit(*ptr) ) ptr++;
	}
	ptr = skipwhitespace(ptr,false);
	if ( *ptr != 0 )
		return false;
	else
		return true;	
}

//=======================================================

void mystrrev(char * s)
{
	char * last = s + strlen(s) - 1;
	while ( last > s )
	{
		swap(*s,*last);
		s++;
		last--;
	}
}

int mystricmp(const char *s1, const char *s2)
{
	int c1,c2;
	do 
	{
		c1 = *s1++;
		c2 = *s2++;
		c1 = (char) tolower( c1 );
		c2 = (char) tolower( c2 );
	}
	while((c1 == c2) && (c1 != 0));
	return (int) c1-c2;
}

int strtcmp(const char *s1, const char *s2)
{
	char c1,c2;
	do 
	{
		c1 = *s1++;
		c2 = *s2++;
		c1 = tokenchar( c1 );
		c2 = tokenchar( c2 );
	}
	while((c1 == c2) && (c1 != 0));
	return (int) c1-c2;
}

int mymemicmp(const void *b1, const void *b2, int n)
{
	const U8 * s1 = (const U8 *)b1;
	const U8 * s2 = (const U8 *)b2;
	for(int i=0;i<n;i++)
	{
		int c1 = (char) tolower( s1[i] );
		int c2 = (char) tolower( s2[i] );
		int d = c1 - c2;
		if ( d != 0 )
			return d;
	}
	return 0;
}

int mymemtcmp(const void *b1, const void *b2, int n)
{
	const U8 * s1 = (const U8 *)b1;
	const U8 * s2 = (const U8 *)b2;
	for(int i=0;i<n;i++)
	{
		int c1 = (char) tokenchar( s1[i] );
		int c2 = (char) tokenchar( s2[i] );
		int d = c1 - c2;
		if ( d != 0 )
			return d;
	}
	return 0;
}

//================================================


void killtailingwhiteandcomments( char * ptr )
{
	char * pend = ptr + strlen(ptr) - 1;
	while( pend >= ptr )
	{
		if ( iswhitespace(*pend) )
		{
			*pend = 0;
		}
		else if ( (pend - ptr) >= 4 && *pend == '/'&& pend[-1] == '*' )
		{
			// saw an end-of-comment, look for a beginn
			char * look = pend-2;
			while ( ! (*look == '*' && look[-1] == '/') )
			{
				look--;
				if ( look <= ptr ) return;
			}
			RR_ASSERT( *look == '*' );
			look--;
			RR_ASSERT( *look == '/' );
			*look = 0;
			pend = look;
		}
		else
		{
			break;
		}
		pend--;
	}
}


bool stripresameadvance( char ** pptr , const char * match , bool skipeols)
{
	char *ptr = *pptr;
	ptr = skipwhitespace(ptr,skipeols);
	if ( stripresame(ptr,match) )
	{
		ptr += strlen(match);
		ptr = skipwhitespace(ptr,skipeols);
		*pptr = ptr;
		return true;
	}
	return false;
}

// stripresameadvanceskipwhite :
//	returns true if string at *pptr has a preisame match with "match"
//	skips past match if so
//	also skips pre and post white
bool stripresameadvance_skipcpp( char ** pptr , const char * match , bool skipeols)
{
	char *ptr = *pptr;
	ptr = SkipCppCommentsAndWhiteSpace(ptr,skipeols);
	if ( stripresame(ptr,match) )
	{
		ptr += strlen(match);
		ptr = SkipCppCommentsAndWhiteSpace(ptr,skipeols);
		*pptr = ptr;
		return true;
	}
	return false;
}

// toktoadvance :
//	destructive tokenize starting at *pptr
//	up to any char in "endset" (eg. endset is something like ")},;")
//	fills ptokchar with the char of endset that was found
//	returned pointer is the token without head&tail white
//	advances *pptr to after the endset char
//	also skips pre and post white
char * toktoadvance_skipcpp( char ** pptr, const char * endset , char * ptokchar , bool skipeols)
{
	char *ptr = *pptr;
	
	ptr = SkipCppCommentsAndWhiteSpace(ptr,skipeols);
	char * start = ptr;
	
	ptr = (char *)strchrset(ptr,endset);
	if ( ptr == NULL )
		return NULL;
	
	if ( ptokchar )
		*ptokchar = *ptr;
	*ptr = 0;
	ptr++;
	ptr = SkipCppCommentsAndWhiteSpace(ptr,skipeols);
	*pptr = ptr;
	
	killtailingwhiteandcomments(start);		
	return start;
}

OODLE_NS_END


