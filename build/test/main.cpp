/*   OZIP
    Ozip high-performance compression utility supporting file to file and streaming coding.
    Functionality imitates gzip.
    Ozip requires the Oodle SDK 2.6 or higher.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/stat.h>
#include <assert.h>
#include <signal.h>
#include <errno.h>
#include "oodle2.h"
#include <fcntl.h>

#define OZIP_VER "0.1.1"

#if defined(unix) || defined(__unix__) || defined(__unix)
# define PLATFORM_UNIX
#endif

#if defined(_WIN32)  || defined(_WIN64)
# define PLATFORM_WIN
#endif

#ifdef PLATFORM_WIN
#include <Windows.h>
#include <io.h>
#pragma warning (disable : 4996)        //fopen_s warning
#define o_read _read
#define o_fileno _fileno

#ifdef _WIN64
#define o_stat _stat64
#else
#define o_stat stat
#endif

#ifdef LOGGING_ENABLED
#include<stdarg.h>
FILE* logfile = NULL;
char logfilename[] = "c:\\test\\OZIP.log";
#endif

#endif

#ifdef PLATFORM_UNIX
#include <unistd.h>
#include <sys/select.h>
#define exit _exit

#if defined PATH_MAX && PATH_MAX >= 256
# define MAX_PATH PATH_MAX   
#else
#define MAX_PATH 4096 //!problematic
#endif

#endif

#ifdef __APPLE__
#define PLATFORM_MAC
#include <unistd.h>
#endif

#ifndef o_stat
#define o_stat stat
#endif

#ifndef NULL
#define NULL 0
#endif

#ifndef o_read
#define o_read read
#endif

#ifndef o_fileno
#define o_fileno fileno
#endif

#ifndef max
#define max(a,b)            (((a) > (b)) ? (a) : (b))
#endif

#ifndef min
#define min(a,b)            (((a) < (b)) ? (a) : (b))
#endif

#if defined(_MSC_VER) && _MSC_VER >= 1600 // in 2010 aka 10.0 and later 
#define RR_UNUSED_VARIABLE(x) (void) (x)
#else
#define RR_UNUSED_VARIABLE(x) (void)(sizeof(x))
#endif

#define NO_OODLEX

#ifdef NO_OODLEX

#ifdef PLATFORM_WIN
#define isatty _isatty
#define fileno _fileno
#endif

#define RADCOPYRIGHT OOCOPYRIGHT
typedef OO_U32 U32;
typedef OO_U64 U64;
typedef OO_SINTa SINTa;
typedef OO_S32 S32;

// Align to 4kb; use VirtualAlloc on windows
static void* OodleXMallocBig(OO_SINTa bytes)
{
#ifdef PLATFORM_WIN
    return VirtualAlloc(NULL, bytes, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
#else
    return OodleCore_Plugin_MallocAligned_Default(bytes, 0x1000);
#endif
}

static void OodleXFreeBig(void* ptr)
{
#ifdef PLATFORM_WIN
    VirtualFree(ptr, 0, MEM_RELEASE);
#else
    OodleCore_Plugin_Free_Default(ptr);
#endif
}

#include <chrono>

static OO_F64 OodleX_GetSeconds()
{
    auto now = std::chrono::system_clock::now().time_since_epoch();
    return std::chrono::duration_cast<std::chrono::nanoseconds>(now).count() / 1e9;
}

enum OodleX_Init_GetDefaults_DebugSystems
{
    OodleX_Init_GetDefaults_DebugSystems_No = 0,
    OodleX_Init_GetDefaults_DebugSystems_Yes = 1,
    OodleX_Init_GetDefaults_DebugSystems_Force32 = 0x40000000
};
enum OodleX_Init_GetDefaults_Threads
{
    OodleX_Init_GetDefaults_Threads_No = 0,
    OodleX_Init_GetDefaults_Threads_Yes = 1,
    OodleX_Init_GetDefaults_Threads_Force32 = 0x40000000
};

static OO_BOOL OodleX_Init_Default(OO_U32 oodle_header_version,
    OodleX_Init_GetDefaults_DebugSystems debugSystems OODEFAULT(OodleX_Init_GetDefaults_DebugSystems_Yes),
    OodleX_Init_GetDefaults_Threads threads OODEFAULT(OodleX_Init_GetDefaults_Threads_Yes))
{
    return true;
}

enum OodleX_Shutdown_LogLeaks
{
    OodleX_Shutdown_LogLeaks_No = 0,
    OodleX_Shutdown_LogLeaks_Yes = 1,
    OodleX_Shutdown_LogLeaks_Force32 = 0x40000000
};

enum OodleX_Shutdown_DebugBreakOnLeaks
{
    OodleX_Shutdown_DebugBreakOnLeaks_No = 0,
    OodleX_Shutdown_DebugBreakOnLeaks_Yes = 1,
    OodleX_Shutdown_DebugBreakOnLeaks_Force32 = 0x40000000
};

static void OodleX_Shutdown(const char* threadProfileLogName OODEFAULT(NULL),
    OodleX_Shutdown_LogLeaks logLeaks OODEFAULT(OodleX_Shutdown_LogLeaks_Yes),
    OO_U64 allocStartCounter OODEFAULT(0),
    OodleX_Shutdown_DebugBreakOnLeaks debugBreakOnLeaks OODEFAULT(OodleX_Shutdown_DebugBreakOnLeaks_No))
{

}
#endif

#ifndef MAX_PATH
#define MAX_PATH 260
#endif

#define HEADER_V1 601
#define DEFAULT_COMPRESSOR OodleLZ_Compressor_Kraken
#define DEFAULT_LEVEL OodleLZ_CompressionLevel_Normal

static const U32 c_ozip_4cc = ('o') | ('z' << 8) | ('i' << 16) | ('p' << 24);  //header magic word
static const U32 g_headerversion = HEADER_V1;
//default compression settings
int g_scratchmemsize = 0;
void* g_scratchmemory = NULL;
int g_level = DEFAULT_LEVEL;
OodleLZ_Compressor g_compressor = DEFAULT_COMPRESSOR;
U32 g_rawbufferlimit = 64 * 1024 * 1024;
U32 g_blocklimit = 512 * 1024;                    //min bytes of data to trigger a compress while waiting on stream
U32 g_contextlimit = 16 * 1024 * 1024;
U32 g_shortwait = 100;                  //ms timeout
U32 g_longwait = 1000;                  //ms timeout

struct OodleLZ_CompressOptions compressoptions = { };

FILE* g_istream = NULL;
FILE* g_ostream = NULL;

char* g_infilename;
char g_outfilename[MAX_PATH];

//flags
bool g_outputstd = false;
bool g_compressflag = false;
bool g_decompressflag = false;
bool g_forceoverwrite = false;
bool g_benchmark_mode = false;
bool g_keeporiginal = false;
bool g_bequiet = false;
bool g_beverbose = false;
bool g_testfile = false;
bool g_verify = false;

bool g_printedhelp = false;
bool g_cleanonexit = false;
bool g_f2f = false;

//Prints to log on windows
void logprintf(const char* _Format, ...)
{
#ifdef LOGGING_ENABLED
#ifdef PLATFORM_WIN
    if (_Stream == NULL) _Stream = stderr;
    va_list _ArgList;
    va_start(_ArgList, _Format);
    //vfprintf(_Stream, _Format, _ArgList);
    char buf[1024];
    vsnprintf(buf, sizeof(buf), _Format, _ArgList);
    va_end(_ArgList);
    fprintf(logfile, buf);
    OutputDebugStringA(buf);
#endif
#endif
    RR_UNUSED_VARIABLE(_Format);
}

//Logging helpers for tracking memory operations
#ifdef LOGGING_ENABLED
void ozipmove(void* dest, void* source, int size)
{
    logprintf("memmove size: %i\n", size);
    memmove(dest, source, size);
}
void ozipmcpy(void* dest, void* source, int size)
{
    logprintf("memmove size: %i\n", size);
    memcpy(dest, source, size);
}
void* ozip_mall(int size)
{
    logprintf("malloc size: %i\n", size);
    return malloc(size);
}
//Logging macro
#define memmove ozipmove
#define memcpy ozipmcpy
#define malloc ozip_mall
#endif

//Prints error message, cleans outfile if necessary, exits.
void Abort(const char  ErrorMessage[] = NULL)
{
    if (!g_bequiet)
    {
        if (ErrorMessage) fprintf(stderr, "OZIP:%s\n", ErrorMessage);
        fprintf(stderr, "OZIP: Aborting...\n");
    }
    if (g_cleanonexit) remove(g_outfilename);
    exit(1);
}

//Catches signals and Aborts().
void ozip_signal_catcher(sig_atomic_t signal)
{
    if (signal == SIGINT || signal == SIGTERM)
    {
        Abort("Interrupt/Termination Signal");
    }
    if (signal == SIGFPE)
    {
        Abort("Signal floating point error. Shouldn't happen!");
    }
    else
        Abort("Unknown signal");
}

//Catches mem signals and Aborts()
void ozip_memsignal_catcher(sig_atomic_t signal)
{
#ifdef PLATFORM_UNIX
    if (signal == SIGBUS && !g_bequiet) fprintf(stderr, "OZIP: SigBus signal ");
#else
    signal;
#endif
    Abort("Memory access violation\n");
}

//malloc wrapper. Aborts on fail
char* ozip_malloc(int size)
{
    char* mempointer = (char*)malloc(size);
    if (!mempointer) Abort("malloc failed.\n");
    return mempointer;
}


//Prints standard help options
void print_help()
{
    g_printedhelp = true;
    if (!g_bequiet)
    {
        fprintf(stderr, "OZIP: usage: ozip [file] [options]\n");
        fprintf(stderr, "compresses stdin to stdout by default\n");
        fprintf(stderr, "creates/strips *.ooz for compressed/uncompressed file targets.\n");
        fprintf(stderr, "Input files delete on success (without --keep)\n");
        fprintf(stderr, "-H                      prints Oodle compressor options help\n");
        fprintf(stderr, "-c --stdout             outputs to stdout (decompress only). \n");
        fprintf(stderr, "-d --decompress         decompress\n");
        fprintf(stderr, "-z --compress           compress\n");
        fprintf(stderr, "-k --keep               keep original file (otherwise deleted)\n");
        fprintf(stderr, "-f --force              overwrite output file if exists\n");
        fprintf(stderr, "-t --test               tests existing compressed file validity\n");
        fprintf(stderr, "-K --verify             verify file during compression\n");
        fprintf(stderr, "-q --quiet              no prints\n");
        fprintf(stderr, "-v --verbose            prints lots\n");
        fprintf(stderr, "-s --small              low memory use ~2.5MB  (compression only)\n");
        fprintf(stderr, "-b                      benchmark, print timing and write no output\n");
        fprintf(stderr, "-(1-9)                  compression level 1-9 (use -ol for hyperfasts)\n");
        fprintf(stderr, "-m[k] --compressor=[k]  [k/l/m/s] Oodle compressor selection\n");
        fprintf(stderr, "--kraken --mermaid --selkie --leviathan     Oodle compressor choice\n");
        fprintf(stderr, "--fast                  low latency streaming compression\n");
        fprintf(stderr, "--best                  best compression, slow encode. (Leviathan 9)\n");
        fprintf(stderr, "--bufferlimit=[1-256]   (MB) max buffer size for compression.\n");
        fprintf(stderr, "--blocklimit=[8+]       (KB) minimum block size in KB for streaming\n");
        fprintf(stderr, "--contextlimit=[8+]     (KB) context limit KB. trades memory use for ratio\n");
#ifdef PLATFORM_UNIX
        fprintf(stderr, "--timeout=[1000]        time in ms to wait on inactive stdin during compression\n");
#endif
        fprintf(stderr, "OZIP ver:%s built: %s @ %s\n", OZIP_VER, __DATE__, __TIME__);

    }
}

//Prints additional oodle compression options help
void print_oodle_help()
{
    g_printedhelp = true;
    if (!g_bequiet)
    {
        fprintf(stderr, "OZIP: -H Additional Oodle compression options:\n");
        fprintf(stderr, "      -o?   --oodlelist                           list Oodle compressorss\n");
        fprintf(stderr, "      -oc#  --compressor=# { 8,9,11,13 }          compressor choice\n");
        fprintf(stderr, "      -ol#  --level=# [%d - %d]                    compression level\n", (int)OodleLZ_CompressionLevel_Min, (int)OodleLZ_CompressionLevel_Max);
        fprintf(stderr, "      -os#  --spacespeedtradeoff=# [64, 1024]     tweak size vs decode time\n");
        fprintf(stderr, "      -om#  --minmatchlength=# [0, 10]            see Oodle docs\n");
        fprintf(stderr, "      -or   --seekresets                          seek chunk resets\n");
        fprintf(stderr, "      -oC#  --seekchunklen=# {2^k >= 2^18}        seek chunk size\n");
        fprintf(stderr, "      -oo#  --offsetlimit=#                       max reference distance in kb\n");
        fprintf(stderr, "      -ot#  --matchbits=# [10, 28]                match table size\n");
        fprintf(stderr, "      -od#  --localmatchsize=# { 2^k <= 2<<30 }   local dictionary size \n");
        fprintf(stderr, "      -oL   --makelrm={0,1}                       use long range matches\n");
        fprintf(stderr, "      -ok   --quantumCRC                          encoder CRC for decode\n");
        fprintf(stderr, "      -ov#                                        backwards compatible to rev#\n");
        fprintf(stderr, "OZIP: built with Oodle SDK Version %s\n", OodleVersion);
        fprintf(stderr, "ozip is public domain and here: https://github.com/jamesbloom/ozip/\n");
        fprintf(stderr, "%s\n", RADCOPYRIGHT);
        fprintf(stderr, "use of Oodle requires an Oodle license.\n");
    }
}

//Prints enum vals for shorthand advanced oodle compression settings
void print_oodle_enums()
{
    g_printedhelp = true;
    if (!g_bequiet)
    {
        fprintf(stderr, "OZIP:   Oodle Compressors:\n");
        fprintf(stderr, "        Kraken = 8      Default compressor. Good Compression, fast\n");
        fprintf(stderr, "        Leviathan = 13  Best compression, slower encodes\n");
        fprintf(stderr, "        Mermaid = 9     Crazy fast, still decent compression\n");
        fprintf(stderr, "        Selkie = 11     Fastest\n");
        fprintf(stderr, "        Hydra = 12      Tuneable composite of above\n\n");
        fprintf(stderr, "        Oodle Levels:\n");
        fprintf(stderr, "        HyperFast = -4 to -1\n");
        fprintf(stderr, "        None = 0\n");
        fprintf(stderr, "        SuperFast = 1\n");
        fprintf(stderr, "        VeryFast = 2\n");
        fprintf(stderr, "        Fast = 3\n");
        fprintf(stderr, "        Normal = 4\n");
        fprintf(stderr, "        Optimal = 5-9\n");
    }
}

U64 get_file_size(const char* filename)
{
    struct o_stat stat_buf;
    U32 filestats = o_stat(filename, &stat_buf);
    return ((filestats == 0) ? stat_buf.st_size : -1);
}

bool file_exists(const char* filename)
{
    struct o_stat buf;
    int Result = o_stat(filename, &buf);
    return Result == 0;
}

//Switch on char input to set global oodle compressor
void set_compressor(char inchar)
{
    char compresschar = ((inchar <= 90) ? (inchar + 32) : inchar); //tolower
    switch (compresschar)
    {
        case 'k':
        {
            g_compressor = OodleLZ_Compressor_Kraken;
            break;
        }
        case 'l':
        {
            g_compressor = OodleLZ_Compressor_Leviathan;
            break;
        }
        case 'm':
        {
            g_compressor = OodleLZ_Compressor_Mermaid;
            break;
        }
        case 's':
        {
            g_compressor = OodleLZ_Compressor_Selkie;
            break;
        }
        case 'h':
        {
            g_compressor = OodleLZ_Compressor_Hydra;
            break;
        }
        default:
        {
            if (!g_bequiet) fprintf(stderr, "OZIP: invalid compressor selection '%c' use 'k(kraken),l(eviathian),m(ermaid),s(elkie),h(ydra)\n", compresschar);
        }
    }
    if (g_beverbose) fprintf(stderr, "OZIP: set compressor = %i\n", (int)g_compressor);
}

void set_minmatchlen(const char* valuepoint)
{
    int parsedval = atoi(valuepoint);
    if (parsedval >= 0 && parsedval <= 10)
    {
        compressoptions.minMatchLen = parsedval;
        if (g_beverbose) fprintf(stderr, "OZIP: set minMatchLen = %i\n", compressoptions.minMatchLen);
    }
    else
    {
        if (!g_bequiet) fprintf(stderr, "OZIP: invalid argument: minmatchlen. must be in range[0,10]. Using default\n");
    }
}

void set_lrm(const char* valuepoint)
{
    if ((*valuepoint == '1') || (*valuepoint == '0'))
    {
        compressoptions.makeLongRangeMatcher = *valuepoint - '0';
        if (g_beverbose) fprintf(stderr, "OZIP: set makeLongRangeMatcher = %i\n", compressoptions.makeLongRangeMatcher);
    }
    else
    {
        if (!g_bequiet) fprintf(stderr, "OZIP: invalid arg --makelrm= takes 0 or 1.\n");
    }
}

void set_sstb(const char* valuepoint)
{
    int parsedval = atoi(valuepoint);
    if (parsedval >= 1 && parsedval <= 4096)
    {
        compressoptions.spaceSpeedTradeoffBytes = parsedval;
        if (g_beverbose) fprintf(stderr, "OZIP: set spaceSpeedTradeoffBytes = %i\n", compressoptions.spaceSpeedTradeoffBytes);
    }
    else
    {
        if (!g_bequiet) fprintf(stderr, "OZIP: invalid argument: spacespeedtradeoffbytes. must be in range[1,4096]. Using default\n");
    }
}

void set_seekchunklen(const char* valuepoint)
{
    int parsedval = atoi(valuepoint);
    if (((parsedval & (parsedval - 1)) == 0) && parsedval >= OODLELZ_BLOCK_LEN)
    {
        compressoptions.seekChunkLen = parsedval;
        compressoptions.seekChunkReset = true;
        if (g_beverbose) fprintf(stderr, "OZIP: set seekChunkLen = %i\n", compressoptions.seekChunkLen);
    }
    else
    {
        if (!g_bequiet) fprintf(stderr, "OZIP: invalid argument: Seekchunklen. must be power of 2 >= 1<<18. Using default\n");
    }
}

void set_offsetlimit(const char* valuepoint)
{
    int parsedval = atoi(valuepoint);
    if (parsedval >= 0)
    {
        /*
        if (parsedval <= 256)
        {
            if (!g_bequiet) fprintf(stderr, "OZIP: Warning: Offsetlimit=%i,  values < 256 act as 0\n", parsedval);
        }
        */
        parsedval *= 1024;
        compressoptions.dictionarySize = parsedval;
        if (g_contextlimit > (U32)parsedval)
        {
            g_contextlimit = parsedval;
        }
        if (g_beverbose) fprintf(stderr, "OZIP: set dictionarySize = %i\n", compressoptions.dictionarySize);
    }
    else
    {
        if (!g_bequiet) fprintf(stderr, "OZIP: invalid argument. offset limit must be greater than 256\n");
    }
}

void set_localdict(const char* valuepoint)
{
    int parsedval = atoi(valuepoint);
    if (((parsedval & (parsedval - 1)) == 0) && parsedval <= OODLELZ_LOCALDICTIONARYSIZE_MAX)
    {
        compressoptions.maxLocalDictionarySize = parsedval;
        if (g_beverbose) fprintf(stderr, "OZIP: set maxLocalDictionarySize = %i\n", compressoptions.maxLocalDictionarySize);
    }
    else
    {
        if (!g_bequiet) fprintf(stderr, "OZIP: invalid argument: maxlocaldictsize. must be power of 2 <= 1<<30. Using default\n");
    }
}

void set_matchtablesize(const char* valuepoint)
{
    int parsedval = atoi(valuepoint);
    if (parsedval >= 10 && parsedval <= 28)
    {
        compressoptions.matchTableSizeLog2 = parsedval;
        if (g_beverbose) fprintf(stderr, "OZIP: set matchTableSizeLog2 = %i\n", compressoptions.matchTableSizeLog2);
    }
    else
    {
        if (!g_bequiet) fprintf(stderr, "OZIP: invalid argument: matchtablesize. must be in range[10,28]. Using default\n");
    }
}

//Handles advanced oodle option chars from command line arguments.
//Arg is char pointer to argument position right after "-o".
void handle_oodle_opt(const char* opt_in_arg)
{
    const char* valuepoint = opt_in_arg + 1;
    if (*valuepoint == '=')
    {
        valuepoint += 1;
    }
    switch (*opt_in_arg)
    {
        case'?':
        {
            print_oodle_enums();
            break;
        }
        case'm':
        {
            set_minmatchlen(valuepoint);
            break;
        }
        case'r':
        {
            compressoptions.seekChunkReset = true;
            if (g_beverbose) fprintf(stderr, "OZIP: set seekChunkReset = %i\n", compressoptions.seekChunkReset);
            break;
        }
        case'C':
        {
            set_seekchunklen(valuepoint);
            break;
        }
        case's':
        {
            set_sstb(valuepoint);
            break;
        }
        case 'v':
        {
            char major = *valuepoint;
            if (major >= '0' && major <= OodleVersion[2])
            {
                struct OodleConfigValues oodleconfig;
                Oodle_GetConfigValues(&oodleconfig);
                oodleconfig.m_OodleLZ_BackwardsCompatible_MajorVersion = major - '0';
                Oodle_SetConfigValues(&oodleconfig);

                if (g_beverbose) fprintf(stderr, "OZIP: set BackwardsCompatible_MajorVersion = %i\n", oodleconfig.m_OodleLZ_BackwardsCompatible_MajorVersion);
            }
            else
            {
                if (!g_bequiet) fprintf(stderr, "OZIP: invalid BackwardsCompatible_MajorVersion = %i\n", major - '0');
            }
            break;
        }
        case'o':
        {
            set_offsetlimit(valuepoint);
            break;
        }
        case't':
        {
            set_matchtablesize(valuepoint);
            break;
        }
        case'd':
        {
            set_localdict(valuepoint);
            break;
        }
        case'L':
        {
            set_lrm(valuepoint);
            break;
        }
        case'k':
        {
            compressoptions.sendQuantumCRCs = true;
            if (g_beverbose) fprintf(stderr, "OZIP: set sendQuantumCRCs = %i\n", compressoptions.sendQuantumCRCs);
            break;
        }
    }
}

//Switch on option flags. sets global command line parameter flags.
void handle_opt(char optchar, char* arg = NULL)
{
    switch (optchar)
    {
        case '=':
            break;
        case 'd':
        {
            g_decompressflag = true;
            if (g_beverbose) fprintf(stderr, "OZIP: force decompression\n");
            break;
        }
        case 'c':
        {
            g_outputstd = true;
            if (g_beverbose) fprintf(stderr, "OZIP: write to stdout\n");
            break;
        }
        case 'k':
        {
            g_keeporiginal = true;
            if (g_beverbose) fprintf(stderr, "OZIP: keep original\n");
            break;
        }
        case 'z':
        {
            g_compressflag = true;
            if (g_beverbose) fprintf(stderr, "OZIP: force compression\n");
            break;
        }
        case 'h':
        {
            print_help();
            break;
        }
        case 'K':
        {
            g_verify = true;
            if (g_beverbose) fprintf(stderr, "OZIP: verify compressed output\n");
            break;
        }
        case 'V': // version
        case 'L': // license
        {
            fprintf(stderr, "OZIP v.%s\n", OZIP_VER);
            fprintf(stderr, " built with Oodle SDK Version %s\n", OodleVersion);
            fprintf(stderr, "ozip is public domain and here: https://github.com/jamesbloom/ozip/\n");
            fprintf(stderr, "%s\n", RADCOPYRIGHT);
            fprintf(stderr, "use of Oodle requires an Oodle license.\n");
            break;
        }
        case 'v':
        {
            g_beverbose = true;
            if (g_beverbose) fprintf(stderr, "OZIP: verbose\n");
            break;
        }
        case 'q':
        {
            g_bequiet = true;
            g_beverbose = false;
            break;
        }
        case 's':
        {
            //small
            g_rawbufferlimit = 4 * 1024 * 1024;
            g_blocklimit = 128 * 1024;
            g_contextlimit = 1024 * 1024;
            if (g_beverbose) fprintf(stderr, "OZIP: small memory mode\n");
            break;
        }
        case 't':
        {
            g_testfile = true;
            if (g_beverbose) fprintf(stderr, "OZIP: test compressed input\n");
            break;
        }
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
        {
            g_level = optchar - '0';
            if (g_beverbose) fprintf(stderr, "OZIP: level = %i\n", g_level);
            break;
        }
        case 'f':
        {
            g_forceoverwrite = true;
            if (g_beverbose) fprintf(stderr, "OZIP: force overwrite output\n");
            break;
        }
        case 'X':
        {
            char compresschar = arg[13]; //first 13 characters are "--Compressor="
            set_compressor(compresschar);
            break;
        }
        case 'C':
        {
            int newcontextlimit = atoi(&arg[14]);
            if (g_contextlimit >= 256)
            {
                g_contextlimit = newcontextlimit * 1024;
                if (g_beverbose) fprintf(stderr, "OZIP: contextlimit = %i\n", g_contextlimit);
            }
            else
            {
                if (!g_bequiet) fprintf(stderr, "OZIP: min contextlimit = 256k");
            }
            break;
        }
        case 'Y':
        {
            int timeout = atoi(&arg[10]);  //first 10 characters are "--timeout="
            if (timeout >= 50)
            {
                g_longwait = timeout;
            }
            else
            {
                g_longwait = 50;
                if (!g_bequiet) fprintf(stderr, "OZIP: --timeout minimum is 50ms. using 50ms\n");
            }
            if (g_shortwait <= g_longwait)
            {
                g_shortwait = g_longwait / 2;
                g_longwait = g_longwait / 2;
            }
            break;
        }
        case 'm':
        {
            char* mm = strchr(arg, optchar);
            mm += 1;
            if (*mm == '=') mm += 1;
            set_compressor(*mm);
            break;
        }
        case 'b':
        {
            g_benchmark_mode = true;
            break;
        }
        case 'B':
        {
            int newblocklimit = atoi(&arg[13]);
            if (newblocklimit >= 4)
            {
                g_blocklimit = newblocklimit * 1024;
                if (g_beverbose) fprintf(stderr, "OZIP: blocklimit = %i\n", g_blocklimit);
            }
            else
            {
                if (!g_bequiet) fprintf(stderr, "OZIP: min blocksize = 4kb.  using default %ikb", g_blocklimit / 1024);
            }
            if (newblocklimit < 128)
            {
                if (!g_bequiet) fprintf(stderr, "OZIP: Warning: blocklimit below 128kb may reduce compression ratio\n");
            }
            break;
        }
        case 'H':
            print_oodle_help();
            break;
    }
}

//Compares 2 strings for equality up to length of str2
//If equal, returns pointer to the position on str1 corresponding to null terminator on str2.
//For "param=" parsing.

const char* compare_end(const char* str1, const char* str2)
{
    size_t n = strlen(str2);
    if (memcmp(str1, str2, n) == 0)
    {
        return (str1 + n);
    }
    else
    {
        return 0;
    }
}

void set_compressor(const char* valuepoint)
{
    int compressornum = atoi(valuepoint);
    if (compressornum == OodleLZ_Compressor_Kraken ||
        compressornum == OodleLZ_Compressor_Leviathan ||
        compressornum == OodleLZ_Compressor_Selkie ||
        compressornum == OodleLZ_Compressor_Mermaid ||
        compressornum == OodleLZ_Compressor_Hydra)
    {
        g_compressor = (OodleLZ_Compressor)compressornum;
        if (g_beverbose) fprintf(stderr, "OZIP: compressor = %i\n", (int)g_compressor);
    }
    else
    {
        if (!g_bequiet) fprintf(stderr, "OZIP: invalid args. compressor choice ivalid. use [8,9,11,12,13]\n");
    }
}
void set_level(const char* valuepoint)
{
    int parsedlevel = atoi(valuepoint);
    if (parsedlevel < OodleLZ_CompressionLevel_Min || parsedlevel > OodleLZ_CompressionLevel_Max)
    {
        if (!g_bequiet) fprintf(stderr, "OZIP: invalid arg - compressor level only valid in [%i , %i].\n", (int)OodleLZ_CompressionLevel_Min, (int)OodleLZ_CompressionLevel_Max);
    }
    else
    {
        g_level = parsedlevel;
        if (g_beverbose) fprintf(stderr, "OZIP: Using compressor level %i\n", g_level);
    }
}

//Parses command line for advanced oodle options
void parse_oodle_opt(int argc, char** const argv)
{
    const char* valuepoint = 0;
    //first get compressor and level so we can fetch defaults
    for (int optind = 1; optind < argc; optind++)
    {
        if ((valuepoint = compare_end(argv[optind], "--compressor=")) != NULL)
        {
            set_compressor(valuepoint);
            continue;
        }
        if ((valuepoint = compare_end(argv[optind], "--level=")) != NULL)
        {
            set_level(valuepoint);
            continue;
        }

        if ((valuepoint = compare_end(argv[optind], "-oc")) != NULL)
        {
            set_compressor(valuepoint);
            continue;
        }
        if ((valuepoint = compare_end(argv[optind], "-ol")) != NULL)
        {
            set_level(valuepoint);
        }
    }
    //fetch defaults
    const struct OodleLZ_CompressOptions* constoptions = OodleLZ_CompressOptions_GetDefault(g_compressor, (OodleLZ_CompressionLevel)g_level);
    compressoptions = *constoptions;

    for (int optind = 1; optind < argc; optind++)
    {
        if ((valuepoint = compare_end(argv[optind], "--oodlelist")) != NULL)
        {
            print_oodle_enums();
            continue;
        }
        if ((valuepoint = compare_end(argv[optind], "--minmatchlength=")) != NULL)
        {
            set_minmatchlen(valuepoint);
            continue;
        }
        if ((valuepoint = compare_end(argv[optind], "--seekresets")) != NULL)
        {
            compressoptions.seekChunkReset = true;
            if (g_beverbose) fprintf(stderr, "OZIP: seekChunkReset\n");
            continue;
        }
        if ((valuepoint = compare_end(argv[optind], "--spacespeedtradeoff=")) != NULL)
        {
            set_sstb(valuepoint);
            continue;
        }
        if ((valuepoint = compare_end(argv[optind], "--seekchunklen=")) != NULL)
        {
            set_seekchunklen(valuepoint);
            continue;
        }
        if ((valuepoint = compare_end(argv[optind], "--offsetlimit=")) != NULL)
        {
            set_offsetlimit(valuepoint);
            continue;
        }
        if ((valuepoint = compare_end(argv[optind], "--matchbits=")) != NULL)
        {
            set_matchtablesize(valuepoint);
            continue;
        }
        if ((valuepoint = compare_end(argv[optind], "--localmatchsize=")) != NULL)
        {
            set_localdict(valuepoint);
            continue;
        }
        if ((valuepoint = compare_end(argv[optind], "--makelrm=")) != NULL)
        {
            set_lrm(valuepoint);
        }
        if ((valuepoint = compare_end(argv[optind], "--quantumCRC")) != NULL)
        {
            compressoptions.sendQuantumCRCs = true;
            if (g_beverbose) fprintf(stderr, "OZIP: sendQuantumCRCs\n");
        }
    }
    for (int optind = 1; optind < argc; optind++)
    {
        if (argv[optind][0] == '-')
        {
            if (argv[optind][1] == 'o')
            {
                handle_oodle_opt(&argv[optind][2]);
            }
        }
    }
}

//Parses args for options. calls handle_opt for valid options.
//Once normal options set, calls parse_oodle_opt for advanced settings.
void parse_opt(int argc, char** const argv)
{
    const char  shortoptions[] = "HhdzKkmftcqvLVs123456789f=b";
    //exclude from longoptions anything who's short flag doesn't match first char
    //compress,stdout,compressor=,fast,blocklimit=,contextlimit=,timeout=
    const char* longoptions[] = { "help","decompress","force","keep","quiet","test","verbose" };
    const int longoptcount = 7;
    for (int optind = 1; optind < argc; optind++)
    {
        if (argv[optind][0] == '-')
        {
            if (argv[optind][1] == '-')    //is --
            {
                //handle long options
                const char* option = argv[optind] + 2;

                //deal with special cases where first char of long isn't the same as short flag.
                if (strcmp(option, "stdout") == 0)
                {
                    handle_opt('c');
                }
                if (strcmp(option, "compress") == 0)
                {
                    handle_opt('z');
                }
                if (strcmp(option, "fast") == 0)
                {
                    //fast
                    g_level = 1;
                    g_blocklimit = 256 * 1024;
                    if (g_beverbose) fprintf(stderr, "OZIP: fast mode\n");
                }
                if (strcmp(option, "best") == 0)
                {
                    //best
                    g_level = 9;
                    g_compressor = OodleLZ_Compressor_Leviathan;
                    g_rawbufferlimit = 64 * 1024 * 1024;
                    if (g_beverbose) fprintf(stderr, "OZIP: best mode (highest compression)\n");
                }
                if (strncmp(option, "Compressor=", 10) == 0)
                {
                    handle_opt('X', argv[optind]);
                }
                if (strncmp(option, "blocklimit=", 10) == 0)
                {
                    handle_opt('B', argv[optind]);
                }
                if (strcmp(option, "license") == 0)
                {
                    handle_opt('L');
                }
                if (strcmp(option, "version") == 0)
                {
                    handle_opt('V');
                }
                if (strcmp(option, "contextlimit=") == 0)
                {
                    handle_opt('C', argv[optind]);
                }
                if (strcmp(option, "timeout=") == 0)
                {
                    handle_opt('Y', argv[optind]);
                }
                //longopt compressors
                if (strcmp(option, "leviathan") == 0)
                {
                    g_compressor = OodleLZ_Compressor_Leviathan;
                    if (g_beverbose) fprintf(stderr, "OZIP: compressor = %i\n", (int)g_compressor);
                }
                if (strcmp(option, "kraken") == 0)
                {
                    g_compressor = OodleLZ_Compressor_Kraken;
                    if (g_beverbose) fprintf(stderr, "OZIP: compressor = %i\n", (int)g_compressor);
                }
                if (strcmp(option, "mermaid") == 0)
                {
                    g_compressor = OodleLZ_Compressor_Mermaid;
                    if (g_beverbose) fprintf(stderr, "OZIP: compressor = %i\n", (int)g_compressor);
                }
                if (strcmp(option, "selkie") == 0)
                {
                    g_compressor = OodleLZ_Compressor_Selkie;
                    if (g_beverbose) fprintf(stderr, "OZIP: compressor = %i\n", (int)g_compressor);
                }
                if (strcmp(option, "hydra") == 0)
                {
                    g_compressor = OodleLZ_Compressor_Hydra;
                    if (g_beverbose) fprintf(stderr, "OZIP: compressor = %i\n", (int)g_compressor);
                }
                if (strcmp(option, "verify") == 0)
                {
                    g_verify = true;
                    if (g_beverbose) fprintf(stderr, "OZIP: verify\n");
                }
                //new negative compression levels
                if (strcmp(option, "1") == 0)
                {
                    g_level = -1;
                    if (g_beverbose) fprintf(stderr, "OZIP: level = %i\n", g_level);
                }
                if (strcmp(option, "2") == 0)
                {
                    g_level = -2;
                    if (g_beverbose) fprintf(stderr, "OZIP: level = %i\n", g_level);
                }
                if (strcmp(option, "3") == 0)
                {
                    g_level = -3;
                    if (g_beverbose) fprintf(stderr, "OZIP: level = %i\n", g_level);
                }
                if (strcmp(option, "4") == 0)
                {
                    g_level = -4;
                    if (g_beverbose) fprintf(stderr, "OZIP: level = %i\n", g_level);
                }
                const char* valpoint;
                if ((valpoint = compare_end(option, "bufferlimit=")) != NULL)
                {
                    int newlimit = atoi(valpoint);
                    if (newlimit > 0 && newlimit <= 256)
                    {
                        g_rawbufferlimit = newlimit * 1024 * 1024;
                        if (g_beverbose) fprintf(stderr, "OZIP: rawbufferlimit = %i\n", (int)g_rawbufferlimit);
                    }
                    else
                    {
                        if (!g_bequiet) fprintf(stderr, "OZIP: opt --bufferlimit= must be in [1,256]\n");
                    }
                }
                //check the rest of the long options whose first char matches the short flag
                for (int i = 0; i < longoptcount; i++)
                {
                    if (strcmp(option, longoptions[i]) == 0)
                    {
                        handle_opt(option[0]);
                    }
                }
            }
            else
            {
                //handle short option (potentially combined)
                // (exclude -ox two char oodle options)
                for (int i = 1; (argv[optind][i] != 0 && argv[optind][i] != 'o'); i++)
                {
                    const char* p = strchr(shortoptions, argv[optind][i]);
                    if (p == NULL)
                    {
                        //if (!g_bequiet) fprintf(stderr, "Ozip: ignored unrecognized opt flag -%c\n", argv[optind][shortind]);
                        continue;
                    }
                    else
                    {
                        handle_opt(*p, argv[optind]);
                    }
                }
            }
        }
    }

    //check oodle opts after others and then validate.
    //	(-ox two char options)
    parse_oodle_opt(argc, argv);
    OodleLZ_CompressOptions_Validate(&compressoptions);

    //validate cmd line parameters
    if (g_compressflag && g_decompressflag)
    {
        Abort("Invalid options: both compress and decompress invoked");
    }
    if (g_compressflag && g_testfile)
    {
        if (!g_bequiet) fprintf(stderr, "OZIP: got compress and test flags. ignoring test.\n");
        g_testfile = false;
    }
    if (g_decompressflag && g_verify)
    {
        if (!g_bequiet) fprintf(stderr, "OZIP: got decompress and verify flags. ignoring verify.\n");
        g_verify = false;
    }
    if (g_level < OodleLZ_CompressionLevel_Min || g_level > OodleLZ_CompressionLevel_Max)
    {
        if (!g_bequiet) fprintf(stderr, "OZIP: invalid arg - compressor level only valid in [%i , %i].\n", (int)OodleLZ_CompressionLevel_Min, (int)OodleLZ_CompressionLevel_Max);
        g_level = DEFAULT_LEVEL;
    }
    g_beverbose &= !g_bequiet;
}

//Write wrapper. Aborts on fail
U32 ozip_write(void* buffer, U32 elementsize, U32 elementcount, FILE* stream)
{
    size_t written = fwrite(buffer, elementsize, elementcount, stream);
    if (written != elementcount)
    {
        Abort("Failed to write to stream");
    }
    return (U32)written;
}

//grows buffer 4x in size or to rawbufferlimit.
//for streaming in bytes to compressor
void grow_buffer(char*& buffer, U32& buffersize)
{
    if (buffersize < g_rawbufferlimit)
    {
        logprintf("rawbuffersize<limit growing 4x\n");
        int newbuffersize = min(g_rawbufferlimit, buffersize * 4);
        char* newbuffer = ozip_malloc(newbuffersize);

        memcpy(newbuffer, buffer, buffersize);
        free(buffer);
        buffer = newbuffer;
        buffersize = newbuffersize;

    }
}

//checks endianness
bool is_big_endian()
{
    union
    {
        U32 i;
        char bytes[4];
    } xx = { 0x01020304 };
    return xx.bytes[0] == 1;
}




//switches endianness
U32 swap32(U32 inttoswap)
{
    inttoswap = ((inttoswap << 8) & 0xFF00FF00) | ((inttoswap >> 8) & 0xFF00FF);
    return (inttoswap << 16) | (inttoswap >> 16);
}

//switches endianness
U64 swap64(U64 inttoswap)
{
    inttoswap = ((inttoswap << 8) & 0xFF00FF00FF00FF00ULL) | ((inttoswap >> 8) & 0x00FF00FF00FF00FFULL);
    inttoswap = ((inttoswap << 16) & 0xFFFF0000FFFF0000ULL) | ((inttoswap >> 16) & 0x0000FFFF0000FFFFULL);
    return (inttoswap << 32) | (inttoswap >> 32);
}

void ozip_write32(U32 towrite)
{
    if (is_big_endian())
    {
        towrite = swap32(towrite);
    }
    ozip_write(&towrite, 4, 1, g_ostream);
}

void ozip_write64(U64 towrite)
{
    if (is_big_endian())
    {
        towrite = swap64(towrite);
    }
    ozip_write(&towrite, 8, 1, g_ostream);
}

bool ozip_read32(U32* tofill)
{
    if (fread(tofill, 4, 1, g_istream) != 1) return false;
    if (is_big_endian()) *tofill = swap32(*tofill);
    return true;
}

bool ozip_read64(U64* tofill)
{
    if (fread(tofill, 8, 1, g_istream) != 1) return false;
    if (is_big_endian()) *tofill = swap64(*tofill);
    return true;
}


//Writes 16 bytes ozip header to stream {magicword, rawsize, contextsize, compresssedsize}
//writes littleendian.
void put_block_header(U32 bytesread, U32 contextsize, U32 compsize)
{
    ozip_write32(c_ozip_4cc);
    ozip_write32(bytesread);
    ozip_write32(contextsize);
    ozip_write32(compsize);
}

//Reads block header from g_istream {magicword, rawsize, contextsize, compresssedsize}
//Returns true only for complete headers.
bool get_block_header(U32& rawsize, U32& contextsize, U32& compsize)
{
    U32 fourcc = 0;
    ozip_read32(&fourcc);
    if (fourcc != c_ozip_4cc) return false;
    if (!ozip_read32(&rawsize)) return false;
    if (!ozip_read32(&contextsize)) return false;
    if (!ozip_read32(&compsize)) return false;
    if (compsize == 0)
    {
        Abort("Corrupted compressed data. Got zero compsize in block header");   //maybe just fail back to handle_file so other args could continue.
    }
    logprintf("read header rawsize = %llu, contextsize = %llu, compsize = %llu\n", rawsize, contextsize, compsize);
    return true;
}

//For unix with nonblocking streams, waits for bytes in stream for 'milisectime' timeout.
//Returns true if it times out.
#ifdef PLATFORM_UNIX
bool timeout_on_stream(U32 milisectime)
{
    fd_set rfds;
    struct timeval tv;
    FD_ZERO(&rfds);
    FD_SET(o_fileno(g_istream), &rfds);
    tv.tv_sec = 0;   //seconds
    tv.tv_usec = milisectime * 1000;   //microseconds
    int state = select(o_fileno(g_istream) + 1, &rfds, NULL, NULL, &tv); //state==0 is timeout. 
    return (state == 0);
}
#endif

//Reads bytes from stream
//Returns true if has bytes read to compress.
//Returns false at eof or timeout(unix only) with no bytes in buffer 
//If buffer filled, grows 4x at a time to bufferlimit
bool got_bytes_to_compress(U32& bytesread, U32& rawbufsize, char*& rawbufbase, U32 dataoffset)
{
    for (;;)
    {
        if ((dataoffset + bytesread) == rawbufsize)
        {
            if (!g_bequiet && g_f2f) fprintf(stderr, ".");

            if (rawbufsize < g_rawbufferlimit)
            {
                grow_buffer(rawbufbase, rawbufsize);
            }
            else
            {
                return true;
            }
        }
        int bytestoread = rawbufsize - (dataoffset + bytesread);

        int gotbytes = o_read(o_fileno(g_istream), rawbufbase + dataoffset + bytesread, bytestoread);

        if (gotbytes > 0)
        {
            bytesread += gotbytes;
        }
        if (feof(g_istream) || gotbytes == 0) //read returning zero also indicates eof
        {
            return bytesread > 0;
        }
        if (gotbytes < bytestoread)
        {
            if (bytesread > g_blocklimit)
            {
                return true;
            }
        }
#ifdef PLATFORM_UNIX
        if (gotbytes < 0)
        {
            if (timeout_on_stream(g_shortwait)) //returns true if timedout, otherwise falls through to read again
            {
                if (bytesread > 4 * 1024)
                {
                    //waited a little, have some bytes to compress so fire a compress
                    return true;
                }
                else if (timeout_on_stream(g_longwait))
                {
                    return (bytesread > 0);  //can return false, truncating file if there's 1100ms with no activity. 
                }
            }
        }
#endif
    }
}

//Determines new context size for compression.
//Never bigger than bufferlimit-blocksize, contextlimit, or bytes available in buffer.
//If next block would run off end of rawbuffer, memmove new context back to base of buffer.
//Update's offset but doesn't reset bytesread.
void update_context_buffer(U32& contextsize, char*& rawbufbase, U32& rawbufsize, U32& dataoffset, U32 bytesread)
{
    U32 newcontextsize = min(contextsize + bytesread, g_contextlimit);
    newcontextsize = min(newcontextsize, g_rawbufferlimit / 2);
    // round down to multiple of OODLELZ_BLOCK_LEN :
    newcontextsize &= ~(OODLELZ_BLOCK_LEN - 1);
    if (dataoffset + bytesread + (g_blocklimit / 2) >= rawbufsize)  //reader will only read what it has room for, allow half block encodes to avoid a memmove?
    {
        logprintf("next block will overshoot rawbuffersize\n");
        if (rawbufsize < g_rawbufferlimit)              //could make buffer bigger
        {
            logprintf("growing buffer 4x (or to limit\n");
            int newbuffersize = min(g_rawbufferlimit, rawbufsize * 4);
            char* newbuffer = ozip_malloc(newbuffersize);
            memcpy(newbuffer, rawbufbase + dataoffset + bytesread - newcontextsize, newcontextsize);
            free(rawbufbase);
            rawbufbase = newbuffer;
            rawbufsize = newbuffersize;
        }
        else  //buffer at max. copy context from end to start.
        {
            logprintf("buffer already at max (%i), moving context to base\n", rawbufsize);
            memmove(rawbufbase, rawbufbase + dataoffset + bytesread - newcontextsize, newcontextsize);
        }
        dataoffset = newcontextsize;
    }
    //we have room. continue.
    else
    {
        dataoffset += bytesread;
    }
    contextsize = newcontextsize;
}

//writes ozip4cc to stream, header version. values written to stream little-endian.
void put_file_header(U64 filesize)
{
    ozip_write32(c_ozip_4cc);
    ozip_write32(g_headerversion);
    ozip_write64(filesize);
}

//Reads little endian stream header and reverses if necessary.
bool get_file_header(U64& rawfilesize)
{
    U32 temp32;
    if (!ozip_read32(&temp32)) return false;
    if (temp32 != c_ozip_4cc)
    {
        if (!g_bequiet) fprintf(stderr, "OZIP: input not ozip compressed. Invalid header\n");
        return false;
    }
    if (!ozip_read32(&temp32))
    {
        if (!g_bequiet) fprintf(stderr, "OZIP: input not valid ozip compressed data.\n");
        return false;
    }

    /*HEADER_V1 following 4 byte 4cc and 4byte version number, just 8 bytes for filesize if known.*/
    if (temp32 == HEADER_V1)
    {
        ozip_read64(&rawfilesize);
        return true;
    }
    else
    {
        if (!g_bequiet) fprintf(stderr, "OZIP: unsupported header format\n");
        return false;

    }
}
//Compresses stream to stream using contextsize bytes of dictionary data from the previous blocks.
//First writes "ozip" as first 4 bytes, then 4 bytes of header version, followed by header data.
//Writes 16 byte headers preceding each block (U32 ozipmagic, rawsize, contextsize,compressedsize)
bool compress(U64 filesize)
{
#ifdef PLATFORM_UNIX
    fcntl(fileno(g_istream), F_SETFL, O_NONBLOCK);
#endif
    U64 totalrawsize = 0;
    U64 totalcompsize = 0;
    if (g_beverbose)
    {
        fprintf(stderr, "OZIP: Compressing with compressor=%i (%s), level=%i (%s)\n", (int)g_compressor, OodleLZ_Compressor_GetName(g_compressor), g_level, OodleLZ_CompressionLevel_GetName((OodleLZ_CompressionLevel)g_level));
        fprintf(stderr, "OZIP: rawbufferlimit=%iKB, blocksize=%iKB\n", g_rawbufferlimit / 1024, g_blocklimit / 1024);
    }
    else if (!g_bequiet && g_f2f) fprintf(stderr, "OZIP: Compressing %s\nOZIP:...", g_infilename ? g_infilename : "");
    logprintf("compress() start for %s\n", g_infilename);
    logprintf("Using compressor=%i, level=%i\n", g_compressor, g_level);
    //streamheader
    put_file_header(filesize);
    //Buffers   
    char* compbuf = NULL;
    U32 compbufsize = 0;
    U32 rawbufsize = (U32)min(g_rawbufferlimit, filesize);
    rawbufsize = max(rawbufsize, g_blocklimit);

    logprintf("allocs rawbuf initially to %i\n", rawbufsize);
    char* rawbufbase = ozip_malloc(rawbufsize);
    U32 dataoffset = 0;
    U32 contextsize = 0;
    U32 bytesread = 0;
    //read/comp loop
    while (got_bytes_to_compress(bytesread, rawbufsize, rawbufbase, dataoffset))
    {
        if (compbufsize < (U32)OodleLZ_GetCompressedBufferSizeNeeded(g_compressor, bytesread))
        {
            compbufsize = (5 * (U32)OodleLZ_GetCompressedBufferSizeNeeded(g_compressor, bytesread) / 4); //overshoot to limit realloc if sizes grow slowly
            logprintf("alloc compbuf at 1.2 * oodleLZgetsizeneeded\n");
            char* newcompbuf = ozip_malloc(compbufsize);
            free(compbuf);
            compbuf = newcompbuf;
        }
        logprintf("Got blocksize, bufferlimit, or eof\n");
        logprintf("compression firing oodle_compress for %llu\n", bytesread);
        if (!g_bequiet && g_beverbose)fprintf(stderr, "OZIP: uncompressed block size = %u\n", bytesread);

        U32 compsize = (U32)OodleLZ_Compress(g_compressor, rawbufbase + dataoffset, bytesread, compbuf, (OodleLZ_CompressionLevel)g_level, &compressoptions, rawbufbase + dataoffset - contextsize, NULL, g_scratchmemory, g_scratchmemsize);
        if (compsize > 0)
        {
            //raw,context,compsize.
            if (!g_bequiet && g_beverbose)fprintf(stderr, "OZIP: compressed block size = %u\n", compsize);
            if (!g_bequiet && !g_beverbose && g_f2f)fprintf(stderr, ".");
            totalrawsize += bytesread;
            totalcompsize += compsize;
            put_block_header(bytesread, contextsize, compsize);
            ozip_write(compbuf, 1, compsize, g_ostream);
        }
        else
        {
            if (!g_bequiet) fprintf(stderr, "\nCompression failed for %s\n", g_infilename);
            free(compbuf);
            free(rawbufbase);
            return false;
        }
        update_context_buffer(contextsize, rawbufbase, rawbufsize, dataoffset, bytesread);
        bytesread = 0;
    }
    if (!g_bequiet) fprintf(stderr, "\nCompressed: %llu bytes -> %llu bytes.    %.2f::1\n", totalrawsize, totalcompsize, ((float)totalrawsize / totalcompsize));
    fflush(g_ostream);
    free(compbuf);
    free(rawbufbase);
    return true;
}

//Guarantees rawbuffer has space for next decompress
//Grows/memmoves buffer as necessary 
//Stays below rawbufferlimit unless single block+context is larger
void ensure_buffer(char*& rawbuffer, U32& rawbufsize, U32 contextsize, U32 rawsize, U32& dataoffset)
{
    if (dataoffset + rawsize > rawbufsize)
    {
        logprintf("Next decomp will overrun rawbuffer\n");
        //will overrun rawbuf. need to either copy to base of current buffer or allocate new larger one...
        if (rawbufsize < g_rawbufferlimit)
        {
            logprintf("growing buffer 4x (or to limit)\n");
            U32 newbufsize = min(g_rawbufferlimit, rawbufsize * 4);
            newbufsize = max(newbufsize, contextsize + rawsize); //don't let decomp fail due to rawbufferlimit, always be willing to allocate at least that minimum 
            char* newrawbuffer = ozip_malloc(newbufsize);
            memcpy(newrawbuffer, rawbuffer + dataoffset - contextsize, contextsize);
            free(rawbuffer);
            rawbuffer = newrawbuffer;
            rawbufsize = newbufsize;
        }
        else
        {
            //rawbufsize at (or even beyond) rawbuflimit. 
            if (contextsize + rawsize > rawbufsize)    //still don't let decomp fail due to insufficient bufsize
            {
                logprintf("!!!!!! Limit too small for a single decode. growing to min size for this decode\n");
                U32 newbufsize = contextsize + rawsize;
                char* newrawbuffer = ozip_malloc(newbufsize);
                memcpy(newrawbuffer, rawbuffer + dataoffset - contextsize, contextsize);
                free(rawbuffer);
                rawbuffer = newrawbuffer;
                rawbufsize = newbufsize;
            }
            else
            {
                logprintf("Buffer at limit, moving context to head of buffer\n");
                //stable case. buffer size at limit much larger than block size, reached end of block and need to reset
                if (contextsize > dataoffset)Abort("Invalid encoded data. Context > preceding data");
                memmove(rawbuffer, rawbuffer + dataoffset - contextsize, contextsize);
            }
        }
        dataoffset = contextsize;
    }
}

//Decompresses stream to stream
//Expects "ozip" magic word as first 4 bytes of stream. next 8bytes assumed for filesize(0 if unknown).
//Then reads 16 bit headers (U32 "ozip", rawsize, contextsize, compsize) for each data block
bool decompress(FILE* verifyagainst = NULL)
{
    if (!g_bequiet && g_testfile) fprintf(stderr, "OZIP:Testing file contents of %s...\n", g_infilename);
    if (!g_bequiet && !g_verify && g_f2f) fprintf(stderr, "OZIP: Decompressing %s \n", g_infilename ? g_infilename : "");
    logprintf(" decompress() start for %s\n", g_infilename);
    U32 rawbufsize = 0;
    char* rawbuffer = NULL;
    U32 dataoffset = 0;
    //prealloc Oodle decoder scratch memory;
    logprintf("alloc scratchmem\n");

    U32 verifybuffersize = 0;
    char* verifybuffer = NULL;

    U32 compbufsize = 0;
    char* compbuf = NULL;

    U64 rawfilesize = 0;
    if (!get_file_header(rawfilesize)) return false;
    if (rawfilesize)
    {
        logprintf("got rawsize=%llu from file\n", rawfilesize);
        rawbufsize = (U32)min(g_rawbufferlimit, rawfilesize);
        rawbuffer = ozip_malloc(rawbufsize);
    }
    else
    {
        rawbufsize = g_blocklimit * 2;
        rawbuffer = ozip_malloc(rawbufsize);
    }
    //decomp block loop
    for (;;)
    {
        logprintf("blockstart\n");
        U32  rawsize = 0;
        U32  contextsize = 0;
        U32  compsize = 0;
        if (!get_block_header(rawsize, contextsize, compsize))
        {
            if (compbufsize == 0) //never got a header
            {
                if (!g_bequiet) fprintf(stderr, "OZIP: input not ozip compressed.\n");
                return false;
            }
            logprintf("No more headers. must be EOF\n");
            break;     //exit point for loop
        }
        if (!rawsize || !compsize)
        {
            if (!g_bequiet) fprintf(stderr, "OZIP: Decomp failed. bad header for compressed block.\n");
            return false;
        }
        ensure_buffer(rawbuffer, rawbufsize, contextsize, rawsize, dataoffset);
        if (compbufsize < compsize)
        {
            logprintf("compbufsize too small (%i < %i). allocing 1.2 * compressed size\n", compbufsize, compsize);
            free(compbuf);
            compbufsize = (U32)(5 * (compsize >> 2)); //overshoot 25% to limit mallocs
            compbuf = ozip_malloc(compbufsize);
        }
        U32 bytesread = 0;
        //get bytes to decompress
        while (bytesread < compsize)
        {
            U32 thisread = (U32)fread(compbuf + bytesread, 1, compsize - bytesread, g_istream);
            logprintf("fread got %i of %i  (previous total %i of %i)\n", thisread, compsize, bytesread, compsize);
            if (thisread > 0)
            {
                bytesread += thisread;
            }
            else
            {
                if (feof(g_istream))
                {
                    if (bytesread == compsize)
                    {
                        //unreachable
                        if (g_beverbose) fprintf(stderr, "OZIP: decomp read finished at eof.\n");
                        break;
                    }
                    else
                    {
                        Abort("Premature EOF in decode stream");
                    }
                }
                if (ferror(g_istream))
                {
                    Abort("ferror(istream)");
                }
                //got zero bytes but not eof or ferror? impossible if nonblocking. no need for nonblocking cause nothing can happen until we get completeblock.
            }
        }
        logprintf("read %i bytes this block\n", bytesread);
        logprintf("oodle_decomp   compsize=%u   rawsize=%u    contextsize=%u\n,", compsize, rawsize, contextsize);
        U32 bytesdecompressed = (U32)OodleLZ_Decompress(compbuf, compsize, rawbuffer + dataoffset, rawsize, OodleLZ_FuzzSafe_Yes, OodleLZ_CheckCRC_Yes, OodleLZ_Verbosity_None, rawbuffer + dataoffset - contextsize, 0, NULL, NULL, g_scratchmemory, g_scratchmemsize);   //! sinta merely for backwards compatibility? doesn't return negative anymore?
        bytesdecompressed -= contextsize;

        if (bytesdecompressed != rawsize)
        {
            //uhoh
            if (!g_bequiet) fprintf(stderr, "OZIP:bytesdecomped!=uncompressedsize\n");
            if (!g_bequiet) fprintf(stderr, "OZIP: only decomped %i bytes \n", bytesdecompressed);
            logprintf("OZIP: only decomped %i bytes \n", bytesdecompressed);
            free(compbuf);
            free(rawbuffer);
            return false;
        }
        if (!g_testfile && !g_verify)
        {
            ozip_write(rawbuffer + dataoffset, 1, rawsize, g_ostream);
        }
        else if (g_verify && verifyagainst)  //for testing compression
        {
            if (verifybuffersize < rawsize)
            {
                if (verifybuffer) free(verifybuffer);
                verifybuffersize = rawsize;
                verifybuffer = ozip_malloc(verifybuffersize);
            }
            U32 vbytesread = 0;
            while (vbytesread < rawsize)
            {
                U32 gotbytes = (U32)fread((void*)(verifybuffer + vbytesread), 1, rawsize - vbytesread, verifyagainst);
                vbytesread += gotbytes;
                if (feof(verifyagainst)) break;
                if (ferror(verifyagainst)) break;
            }
            if (memcmp(verifybuffer, rawbuffer + dataoffset, rawsize) != 0)
            {
                free(compbuf);
                free(rawbuffer);
                free(verifybuffer);
                return false;
            }
        }
        dataoffset += bytesdecompressed;
    }
    free(compbuf);
    free(rawbuffer);
    if (verifybuffer) free(verifybuffer);
    return true;
}

bool is_dir(char* filename)
{
#ifdef PLATFORM_WIN
    DWORD fileattr = GetFileAttributesA(filename);
    return ((fileattr & FILE_ATTRIBUTE_DIRECTORY) == FILE_ATTRIBUTE_DIRECTORY);
#elif defined(PLATFORM_UNIX) || defined(PLATFORM_MAC)
    struct o_stat sb;
    return (o_stat(filename, &sb) == 0 && S_ISDIR(sb.st_mode));
#else
#error PLATFORM not set
#endif
}

//Copies instring to outstring
//Tries to trim the file extension 'ext'.
//Returns true if it found the extension and trimmed it.
bool trim(char* instring, char* outstring, const char ext[])
{
    strcpy(outstring, instring);
    U32 i = 1;
    U32 length = (U32)strlen(instring);
    while (instring[length - i] == ext[strlen(ext) - i])
    {
        if (instring[length - i] == '.')
        {
            outstring[length - i] = 0;
            return true;
        }
        i++;
    }
    return false;
}

//If compressing, appends .ooz extensions
//If decompressing, removes .ooz extension
//Checks file existence against overwrite flag
//Returns true if compression/decompress should procede.
bool set_out_filename()
{
    bool bWasOoz = trim(g_infilename, g_outfilename, ".ooz");
    if (g_decompressflag)
    {
        if (bWasOoz)
        {
            //all good. procede.
        }
        else
        {
            //target file not an .ooz. attempt decompress anyway? append .noz
            strcat(g_outfilename, ".noz");
            if (!g_bequiet) fprintf(stderr, "OZIP: Warning, infile not an '.ooz' extension. Attempting decompression to %s\n", g_outfilename);
        }
    }
    else
    {
        if (bWasOoz)
        {
            //decomp unspecified so compressing, found an ooz, ignoring the file
            if (!g_bequiet) fprintf(stderr, "OZIP:Skipping %s   Already an .ooz\n", g_infilename);
            return false;
        }
        else
        {
            //decomp unspecificied. assuming compression. if .noz, strip.
            trim(g_infilename, g_outfilename, ".noz");
            strcat(g_outfilename, ".ooz");
        }
    }
    if (file_exists(g_outfilename) && !g_forceoverwrite)
    {
        if (!g_bequiet) fprintf(stderr, "OZIP:\"%s\" already exists. use -f --force to overwrite\n", g_outfilename);
        return false;
    }
    return true;
}

//Prepare and call compress/decompress stream to stream.
bool handle_p2p(U64 filesize = 0)
{
#ifdef PLATFORM_WIN
    if (g_istream == stdin)
    {
        if (_setmode(o_fileno(g_istream), _O_BINARY) == -1) Abort("Failed to set stdin to binary");
    }
    if (g_ostream == stdout)
    {
        if (_setmode(o_fileno(g_ostream), _O_BINARY) == -1) Abort("Failed to set stdout to binary");
    }
#endif

    if (g_decompressflag || g_testfile)
    {
        bool success = decompress();
        if (g_testfile && !g_bequiet) fprintf(stderr, "OZIP: File contents %s or uncompressed.\n", (success ? "valid" : "invalid"));
        return success;
    }
    else
    {
        if (isatty(fileno(g_ostream)) && !g_forceoverwrite)
        {
            if (!g_bequiet) fprintf(stderr, "OZIP: won't write compressed data to a terminal without -f --force.\n");
            return false;
        }
        bool success = compress(filesize);
        return success;
    }
}

int g_benchmark_files = 0;
U64 g_benchmark_raw_bytes = 0;
U64 g_benchmark_comp_bytes = 0;
double g_benchmark_enc_time = 0;
double g_benchmark_dec_time = 0;

const double c_benchmark_min_time = 2.0;
const int c_benchmark_min_reps = 2;

#ifdef _MSC_VER
#define U64_FMT "I64u"
#else
#define U64_FMT "llu"
#endif

void benchmark_print_line(FILE* fp, const char* name, U64 raw_bytes, U64 comp_bytes, double enc_time, double dec_time, char eol)
{
    double ratio = (double)raw_bytes / comp_bytes;
    double enc_MBs = (raw_bytes / 1000000.0) / enc_time;
    double dec_MBs = (dec_time == 0.0) ? 0.0 : ((raw_bytes / 1000000.0) / dec_time);

    char compressor_char = OodleLZ_Compressor_GetName(g_compressor)[0];

    // match Zstd -b formatting :	
    // 3# 12 files         : 211938580 ->  66981689 (3.164),  53.9 MB/s , 223.0 MB/s

    char shortname[18];
    strncpy(shortname, name, 17);
    shortname[17] = 0;

    if (ratio < 10.0)
    {
        fprintf(fp, "%c%2d %-17s: %9" U64_FMT " -> %9" U64_FMT " (%5.3f),%6.1f MB/s,%7.1f MB/s%c",
            compressor_char, g_level,
            shortname, raw_bytes, comp_bytes, ratio, enc_MBs, dec_MBs, eol);
    }
    else
    {
        fprintf(fp, "%c%2d %-17s: %9" U64_FMT " -> %9" U64_FMT " (%5.2f),%6.1f MB/s,%7.1f MB/s%c",
            compressor_char, g_level,
            shortname, raw_bytes, comp_bytes, ratio, enc_MBs, dec_MBs, eol);
    }
}

char g_benchmark_filename[24];

bool benchmark(const char* fullfilename, U64 infilesize)
{
    // make filepart of filename
    const char* pname = fullfilename + strlen(fullfilename) - 1;
    while (pname > fullfilename)
    {
        pname--;
        if (*pname == '/' || *pname == '\\')
        {
            pname++;
            break;
        }
    }
    strncpy(g_benchmark_filename, pname, 20);
    g_benchmark_filename[19] = 0;
    pname = g_benchmark_filename;

    int in_size = (int)infilesize;
    if ((U64)in_size != infilesize)
    {
        fprintf(stderr, "OZIP: benchmark 32 bit file sizes only\n");
        return false;
    }

    // use OodleXMallocBig instead of ozip_malloc
    // there seems to be a complicated occasional performance impact of the allocator used
    // I just want to use the same thing that I use in Oodle (such as example_lz_chart) for consistency
    void* in_buf = OodleXMallocBig(in_size);
    void* comp_buf = OodleXMallocBig((int)OodleLZ_GetCompressedBufferSizeNeeded(g_compressor, in_size));

    if (in_buf == NULL || comp_buf == NULL) return false;

    size_t in_got = fread(in_buf, 1, in_size, g_istream);
    if (in_got != (size_t)in_size)
        return false;

    SINTa comp_size = -1;
    double enc_time = 99999999.9;

    {
        int reps = 0;
        double total_seconds = 0;
        double last_log_time = 0;
        for (;;)
        {
            double dt = OodleX_GetSeconds();

            comp_size = OodleLZ_Compress(g_compressor, in_buf, in_size, comp_buf, (OodleLZ_CompressionLevel)g_level, &compressoptions, NULL, NULL, g_scratchmemory, g_scratchmemsize);

            dt = OodleX_GetSeconds() - dt;

            if (comp_size == OODLELZ_FAILED)
                return false;

            total_seconds += dt;
            enc_time = min(enc_time, dt);
            reps++;

            if (total_seconds - last_log_time > 1.0)
            {
                last_log_time = total_seconds;
                // this printing does hurt timing, even though it's outside the timer scope :
                benchmark_print_line(stderr, pname, in_size, comp_size, enc_time, 0.0, '\r');
                fflush(stderr);
            }

            if (reps >= c_benchmark_min_reps && total_seconds >= c_benchmark_min_time)
                break;
        }
    }

    double dec_time = 99999999.9;

    {
        int reps = 0;
        double total_seconds = 0;
        double last_log_time = 0;
        for (;;)
        {
            double dt = OodleX_GetSeconds();

            SINTa dec_got = OodleLZ_Decompress(comp_buf, comp_size, in_buf, in_size,
                OodleLZ_FuzzSafe_Yes, OodleLZ_CheckCRC_No, OodleLZ_Verbosity_None,
                NULL, 0, NULL, NULL, g_scratchmemory, g_scratchmemsize,
                OodleLZ_Decode_Unthreaded);

            dt = OodleX_GetSeconds() - dt;

            //printf("\ndt : %.3f millis\n",dt*1000);

            if (dec_got != in_size)
                return false;

            total_seconds += dt;
            dec_time = min(dec_time, dt);
            reps++;

            if (total_seconds - last_log_time > 1.0)
            {
                last_log_time = total_seconds;
                // this printing does hurt timing, even though it's outside the timer scope :
                benchmark_print_line(stderr, pname, in_size, comp_size, enc_time, dec_time, '\r');
                fflush(stderr);
            }

            if (reps >= c_benchmark_min_reps && total_seconds >= c_benchmark_min_time)
                break;
        }
    }

    OodleXFreeBig(in_buf);
    OodleXFreeBig(comp_buf);

    g_benchmark_files++;
    g_benchmark_raw_bytes += in_size;
    g_benchmark_comp_bytes += comp_size;
    g_benchmark_enc_time += enc_time;
    g_benchmark_dec_time += dec_time;

    return true;
}

void benchmark_finish()
{
    if (g_benchmark_files == 0 || g_benchmark_raw_bytes == 0)
    {
        printf("benchmark: no files!\n");
        return;
    }

    if (g_benchmark_files == 1)
    {
        // stdout or stderr ?
        // reuse g_benchmark_filename made by the one benchmark run
        benchmark_print_line(stdout, g_benchmark_filename, g_benchmark_raw_bytes, g_benchmark_comp_bytes, g_benchmark_enc_time, g_benchmark_dec_time, '\n');
    }
    else
    {
        char num_files_string[80];
        sprintf(num_files_string, "%d files", g_benchmark_files);

        // stdout or stderr ?
        benchmark_print_line(stdout, num_files_string, g_benchmark_raw_bytes, g_benchmark_comp_bytes, g_benchmark_enc_time, g_benchmark_dec_time, '\n');
    }
}

//Handles potential filename argument
//Ignore directories.
//Sets instream/outstream to appropriate targets and calls compress/decompress
bool handle_file(char* infilecandidate)
{
    bool success;
    if (!file_exists(infilecandidate))
    {
        if (!g_bequiet) fprintf(stderr, "OZIP: %s not found...\n", infilecandidate);
        return false;
    }
    //ignore directory targets
    if (is_dir(infilecandidate))
    {
        return false;
    }
    g_infilename = infilecandidate;
    U64 infilesize = get_file_size(g_infilename);
    FILE* infilestream = fopen(g_infilename, "rb");
    if (!infilestream)
    {
        if (!g_bequiet) fprintf(stderr, "OZIP: Couldn't open file %s\n", g_infilename);
        return false;
    }

    //instream is set
    g_istream = infilestream;

    if (g_benchmark_mode)
    {
        success = benchmark(g_infilename, infilesize);
    }
    else if (g_outputstd)
    {
        g_ostream = stdout;

        success = handle_p2p(infilesize);
    }
    else
    {
        if (!g_testfile)                  //if just testing input file, don't need output
        {
            if (!set_out_filename()) return false;
            g_ostream = fopen(g_outfilename, "wb");
            g_cleanonexit = true;
            if (!g_ostream)
            {
                if (!g_bequiet) fprintf(stderr, "OZIP: failed to open outfile %s\n", g_outfilename);
                return false;
            }
        }
        g_f2f = true;
        success = handle_p2p(infilesize);
        if (!g_testfile)
        {
            if (success && g_verify)
            {
                rewind(g_istream);
                if (!g_istream)
                {
                    if (!g_bequiet) fprintf(stderr, "OZIP: Failed to reopen original file for verification\n");
                    success = false;
                }
                fclose(g_ostream); g_ostream = NULL;
                g_istream = fopen(g_outfilename, "rb");
                if (!g_istream)
                {
                    if (!g_bequiet) fprintf(stderr, "OZIP: Failed to open compressed file for verification.\n");
                    success = false;
                }
                bool decompverified = decompress(g_istream);
                fclose(g_istream); g_istream = NULL;
                if (!decompverified)
                {
                    if (!g_bequiet) fprintf(stderr, "OZIP: File verification failed.\n");
                    success = false;
                }
                else
                {
                    if (!g_bequiet) fprintf(stderr, "OZIP: File verification succeeded.\n");

                }
            }

            if (g_ostream)
            {
                fclose(g_ostream); g_ostream = NULL;
            }
            if (g_istream)
            {
                fclose(g_istream); g_istream = NULL;
            }

            if (!success)
            {
                if (remove(g_outfilename) != 0)
                {
                    if (!g_bequiet) fprintf(stderr, "OZIP: Failed to clean up junk file %s\n", g_outfilename);
                }
            }
            else
            {
#if defined (PLATFORM_UNIX) || defined (PLATFORM_MAC)
                struct stat info;
                int status = o_stat(g_infilename, &info);
                if (status == 0)
                {
                    chmod(g_outfilename, info.st_mode);
                    chown(g_outfilename, info.st_uid, info.st_gid);
                }
#endif                  
                if (!g_keeporiginal)
                {
                    remove(g_infilename);    //could check return values, and warn?
                }
            }
        }

    }
    if (g_istream && g_istream != stdin)
    {
        fclose(g_istream); g_istream = NULL;
    }
    if (g_ostream && g_ostream != stdout)
    {
        fclose(g_ostream); g_ostream = NULL;
    }
    return success;
}

void register_signal_handlers()
{
    //register signal handlers
    signal(SIGTERM, ozip_signal_catcher);
    signal(SIGINT, ozip_signal_catcher);
    signal(SIGFPE, ozip_signal_catcher);
    signal(SIGSEGV, ozip_memsignal_catcher);
#ifdef PLATFORM_UNIX
    signal(SIGBUS, ozip_memsignal_catcher);
#endif
}

//Iterates on file args.
//Does naive wildcard expansion for * in filenames on windows via findnextfile api.
//Returns true if it found a nonopt command line parameter.
bool iterate_on_file_args(int argc, char* argv[])
{
    bool didsomething = false;
    bool foundgoodfile = false;
    for (int i = 1; i < argc; i++)
    {
        if (argv[i][0] != '-')
        {
            g_cleanonexit = false;
            didsomething = true;
#ifdef PLATFORM_WIN
            //naive wildcard expansion. only supports single wildcard in filename portion
            //for additional support consider linking with setargv.obj per https://docs.microsoft.com/en-us/cpp/c-language/expanding-wildcard-arguments
            if (strchr(argv[i], '*'))
            {
                typedef struct wg_infilename
                {
                    char x[MAX_PATH];
                } wg_infilename;
                wg_infilename  filetoprocess;
                //filestohandle = (wg_infilename*)calloc(100 * MAX_PATH, 1);
                //int fileindex = 0;
                HANDLE filehandle;
                WIN32_FIND_DATAA filedata;
                filehandle = FindFirstFileA(argv[i], &filedata);
                if (filehandle != INVALID_HANDLE_VALUE)
                {
                    do
                    {
                        strncpy(filetoprocess.x, argv[i], MAX_PATH);
                        char* p = strrchr(filetoprocess.x, '\\');
                        if (p)
                        {
                            p[1] = 0;
                            strncat(filetoprocess.x, filedata.cFileName, MAX_PATH - strlen(filetoprocess.x));
                        }
                        else
                        {
                            strncpy(filetoprocess.x, filedata.cFileName, MAX_PATH);
                        }
                        //fileindex++;
                        g_cleanonexit = false;
                        foundgoodfile |= handle_file(filetoprocess.x);
                    } while (FindNextFileA(filehandle, &filedata));
                }
                if (!foundgoodfile)
                {
                    if (!g_bequiet) fprintf(stderr, "OZIP: Found no files matching '%s'\n", argv[i]);
                }
            }
            else
            {
                handle_file(argv[i]);
            }
#else
            handle_file(argv[i]);
#endif
        }
    }
    return didsomething;
}

//If no nonopt(-) args attempts to comp/decome from stdin to stdout.
int main(int argc, char* argv[])
{

#ifdef LOGGING_ENABLED
    logfile = fopen(logfilename, "w");
#endif
    logprintf("OZIP ver:%s built: %s @ %s\n", OZIP_VER, __DATE__, __TIME__);

    // get OodleX threads for Jobify :
    //	@@ need only 2 worker threads, could make less
    OodleX_Init_Default(OODLE_HEADER_VERSION,
        OodleX_Init_GetDefaults_DebugSystems_No,
        OodleX_Init_GetDefaults_Threads_Yes);

    // OodleX_Init automatically installs itself to Oodle Core Job plugins
    //	Oodle Core LZ compress default option is jobify yes

    register_signal_handlers();

    //set opts
    parse_opt(argc, argv);

    if (!g_printedhelp)
    {
        S32 max_decoder_memory_needed = OodleLZDecoder_MemorySizeNeeded(OodleLZ_Compressor_Invalid, -1);
        // 1 MB for encoder :
        g_scratchmemsize = max(1024 * 1024, max_decoder_memory_needed);
        g_scratchmemory = ozip_malloc(g_scratchmemsize);

        //operate on file args
        bool didsomething = iterate_on_file_args(argc, argv);

        if (g_benchmark_mode)
        {
            benchmark_finish();
            didsomething = true;
        }

        if (!didsomething)   //didn't operate on file arg, default to stdin to stdout mode.
        {
            g_istream = stdin;
            g_ostream = stdout;
            if (!handle_p2p())
            {
                if (!g_bequiet) fprintf(stderr, "OZIP: For help, type: ozip -h\n");
            }
        }

        free(g_scratchmemory); g_scratchmemory = NULL;
    }

    OodleX_Shutdown();

    return 0;
}



//internal build system
/*
@cdep pre

$cbPreBuildSetup

$DefaultsSub

$path($projectspath/oodle2/include, *.c*;*.h;*.inl)

$addtocswitches( -DOODLE_IMPORT_LIB )
$addtocswitches( -I$projectspath/oodle2/include)

$set(NoAutoInclude,1)

$if( $Or($equals($mybuildplatform,win32),$equals($mybuildplatform,win64)),
$requiresbinary(oldnames.lib), )

$cbcdeprequirelibs

*/

/*
@cdep post

$cbPostBuildSetup

$BuildSub

*/