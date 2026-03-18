
//===================================================
// Oodle2 Texture runtime header
// (C) Copyright 1994-2022 Epic Games Tools LLC
//===================================================

#ifndef __OODLE2TEXRT_H_INCLUDED__
#define __OODLE2TEXRT_H_INCLUDED__

#ifndef OODLE2TEXRT_PUBLIC_HEADER
#define OODLE2TEXRT_PUBLIC_HEADER 1
#endif

#ifndef __OODLE2BASE_H_INCLUDED__
#include "oodle2base.h"
#endif

#ifdef _MSC_VER
#pragma pack(push, Oodle, 8)

#pragma warning(push)
#pragma warning(disable : 4127) // conditional is constant
#endif

// header version :
//  the DLL is incompatible when MAJOR is bumped
//  MINOR is for internal revs and bug fixes that don't affect API compatibility
#define OODLE2TEXRT_VERSION_MAJOR           9
#define OODLE2TEXRT_VERSION_MINOR           16

// OodleTextureRTVersion string is 1 . MAJOR . MINOR
//  don't make it from macros cuz the doc tool has to parse the string literal

#define OodleTextureRTVersion "2.9.16"    /*
*/

typedef enum OodleTexRT_Err
{
    OodleTexRT_Err_OK = 0,                          // no error
    OodleTexRT_Err_Internal = -101,                 // Unspecified internal error (this should not happen; if you can, please report a bug.)

    OodleTexRT_Err_BC7PrepHeaderCorrupt = -102,     // BC7Prep header corrupted (or unsupported versin)
    OodleTexRT_Err_BC7PrepOutputBufTooSmall = -103, // BC7Prep output buffer too small for given block count
    OodleTexRT_Err_BC7PrepScratchBufTooSmall = -104,// BC7Prep scratch buffer too small for input data
    OodleTexRT_Err_BC7PrepPayloadCorrupt = -105,        // BC7Prep payload data chunk was corrupted
    OodleTexRT_Err_BC7PrepInputTooSmall = -106,         // BC7Prep input data to small for given header block count
    OodleTexRT_Err_BC7PrepTooManyBlocks = -107,     // Too many blocks in a single BC7Prep blob for GPU decode (limits exist in some GPU variants)

    OodleTexRT_Err_GPU_OutOfShaderMem = -108,       // Insufficient shader memory passed to GPU init
    OodleTexRT_Err_GPU_BadMemoryType = -109,        // The given GPU resource memory type is not permitted in this context.

    OodleTexRT_Err_Force32 = 0x40000000     // not an actual error!
} OodleTexRT_Err;
/* Error codes for Oodle Texture Runtime API functions.

    Negative values indicate an actual error, non-negative values indicate success.

    Functions that return integers can return errors in negative numbers.
    Non-negative values indicate success, negative values correspond to an $OodleTexRT_Err value.
    The utility function $OodleTexRT_Err_GetName can be used to turn these error codes into strings.
*/

//idoc(parent,OodleAPI_TextureRTBC7Prep)

#ifndef OODLETEXRT_BC7PREP_DEFINED
#define OODLETEXRT_BC7PREP_DEFINED

#define OODLETEXRT_BC7PREP_MODE_COUNT 10 /* Number of distinct mode streams in BC7Prep data.

   8 regular BC7 modes, one raw passthrough, one special.
*/

typedef OOSTRUCT OodleTexRT_BC7PrepHeader
{
    OO_U32 version;
    OO_U32 flags;
    OO_U32 mode_counts[OODLETEXRT_BC7PREP_MODE_COUNT];
} OodleTexRT_BC7PrepHeader;
/* Contains metadata required by the decoder to read a BC7Prep data block.

    Header information is always needed and interpreted on the CPU, even
    when actual BC7Prep decoding takes place in a compute shader on the GPU.
    With compute shader decoding, the payload data needs to be in suitably-aligned
    GPU-accessible memory, whereas the header information should be in
    CPU-accessible, cached memory.

    Because of these different requirements, we explicitly store the header
    data elsewhere. We recommend storing it alongside other texture metadata
    in your files.
*/

#endif // OODLETEXRT_BC7PREP_DEFINED

typedef enum OodleTexRT_BC7PrepDecodeFlags
{
    OodleTexRT_BC7PrepDecodeFlags_None = 0, // No special processing.
    OodleTexRT_BC7PrepDecodeFlags_DestinationIsCached = 1, // Destination is in write-back cached memory.
    OodleTexRT_BC7PrepDecodeFlags_AvoidWideVectors = 2, // Avoid using wide (>=256b) vector instructions.

    OodleTexRT_BC7PrepDecodeFlags_Force32 = 0x40000000
} OodleTexRT_BC7PrepDecodeFlags;
/* OodleTexRT_BC7PrepDecodeFlags controls BC7Prep decoding.

    Several of these flag values may be ORed together.

    `OodleTexRT_BC7PrepDecodeFlags_DestinationIsCached` may be used when the destination address
    is in cached memory, i.e. not uncached or write combined. This enables slightly faster
    decoding, up to about 10%. However, setting this flag when the destination is actually in
    write-combined memory will frequently result in a 2x slowdown instead, so tread carefully.

    `OodleTexRT_BC7PrepDecodeFlags_AvoidWideVectors` disables usage of wide (>=256-bit) vector
    instructions even on CPUs that support it. On certain CPUs (mainly Intel CPUs derived
    from the Broadwell or Skylake microarchitectures) any use of a large class of 256-bit vector
    instructions will reduce the maximum clock rate by up to 20%; furthermore, for most non-server
    SKUs, this reduction affects all cores in the system, even when only a single core uses wide
    vector instructions. BC7Prep decoding generally only runs in short bursts; you can pass this
    flag to avoid use of 256-bit vectors in the decoder if nothing else in your app uses them, to
    avoid penalizing all other threads in the system.
*/

#ifdef __cplusplus
// Operator overload to enable ORing flags together in a type-safe manner in C++.
static inline OodleTexRT_BC7PrepDecodeFlags operator |(const OodleTexRT_BC7PrepDecodeFlags a, const OodleTexRT_BC7PrepDecodeFlags b)
{
    return static_cast<OodleTexRT_BC7PrepDecodeFlags>(static_cast<OO_U32>(a) | static_cast<OO_U32>(b));
}

static inline OodleTexRT_BC7PrepDecodeFlags& operator |=(OodleTexRT_BC7PrepDecodeFlags & a, const OodleTexRT_BC7PrepDecodeFlags b)
{
    a = static_cast<OodleTexRT_BC7PrepDecodeFlags>(static_cast<OO_U32>(a) | static_cast<OO_U32>(b));
    return a;
}
#endif // __cplusplus

// function pointers to mallocs needed :

OODEFFUNC typedef void * (OODLE_CALLBACK t_fp_OodleTexRT_Plugin_MallocAligned)( OO_SINTa bytes, OO_S32 alignment);
/* Function pointer type for OodleMallocAligned

    $:bytes     number of bytes to allocate
    $:alignment required alignment of returned pointer
    $:return    pointer to memory allocated (must not be NULL)

    _alignment_ will always be a power of two

    _alignment_ will always be >= $OODLE_MALLOC_MINIMUM_ALIGNMENT

*/

OODEFFUNC typedef void (OODLE_CALLBACK t_fp_OodleTexRT_Plugin_Free)( void * ptr );
/* Function pointer type for OodleFree

    $:ptr   pointer to memory to free

*/

OOFUNC1 void OOFUNC2 OodleTexRT_Plugins_SetAllocators(
    t_fp_OodleTexRT_Plugin_MallocAligned * fp_OodleMallocAligned,
    t_fp_OodleTexRT_Plugin_Free * fp_OodleFree);
/* Set the function pointers for allocations by Oodle.

    If these are not set, the default implementation on most platforms uses the C stdlib.
    On Microsoft platforms the default implementation uses HeapAlloc.

    These must not be changed once they are set!  Set them once then don't change them.

    

    If you want to ensure that Oodle is not doing any allocations, you can call OodleTexRT_Plugins_SetAllocators(NULL,NULL);
    If you do that, then any time Oodle needs to allocate memory internally, it will stop the process.
    It is STRONGLY not recommended that you ship that way.  You can verify that Oodle is not allocating, but then leave some
    fallback allocator installed when you actually ship just in case.

    Also note that on many consoles the standard allocation practices may not
    leave much heap memory for the C stdlib malloc.  In this case Oodle may fail to allocate.

*/

OODEFFUNC typedef OO_U64 (OODLE_CALLBACK t_fp_OodleTexRT_Plugin_RunJob)( t_fp_Oodle_Job * fp_job, void * job_data , OO_U64 * dependencies, int num_dependencies, void * user_ptr );
/* Function pointer type for OodleTexRT_Plugins_SetJobSystem

    $:dependencies      array of handles of other pending jobs. All guaranteed to be nonzero.
    $:num_dependencies  number of dependencies. Guaranteed to be no more than OODLE_JOB_MAX_DEPENDENCIES.
    $:user_ptr          is passed through from the OodleLZ_CompressOptions.
    $:return            handle to the async job, or 0 if it was run synchronously

    RunJob will call fp_job(job_data)

    it may be done on a thread, or it may run the function synchronously and return 0, indicating the job is already done.
    The returned OO_U64 is a handle passed to WaitJob, unless it is 0, in which case WaitJob won't get called.

    fp_job should not run until all the dependencies are done.  This function should not delete the dependencies.

    RunJob must be callable from within an Oodle Job, i.e. jobs may spawn their own sub-jobs directly.
    However, the matching WaitJob calls will only ever occur on the thread that called the
    internally threaded Oodle API function.

    See $Oodle_About_Job_Threading_Plugins
*/

OODEFFUNC typedef void (OODLE_CALLBACK t_fp_OodleTexRT_Plugin_WaitJob)( OO_U64 job_handle, void * user_ptr );
/* Function pointer type for OodleTexRT_Plugins_SetJobSystem

    $:job_handle    a job handle returned from RunJob. Never 0.
    $:user_ptr      is passed through from the OodleLZ_CompressOptions or OodleTex_Encode call.

    Waits until the job specified by job_handle is done and cleans up any associated resources. Oodle
    will call WaitJob exactly once for every RunJob call that didn't return 0.

    If job_handle was already completed, this should clean it up without waiting.

    A handle value should not be reused by another RunJob until WaitJob has been done with that value.

    WaitJob will not be called from running jobs.  It will be only be called from the original thread that
    invoked Oodle.  If you are running Oodle from a worker thread, ensure that that thread is allowed to wait
    on other job threads.

    See $Oodle_About_Job_Threading_Plugins
*/

OOFUNC1 void OOFUNC2 OodleTexRT_Plugins_SetJobSystem(
    t_fp_OodleTexRT_Plugin_RunJob * fp_RunJob,
    t_fp_OodleTexRT_Plugin_WaitJob * fp_WaitJob);
/* DEPRECATED use OodleTexRT_Plugins_SetJobSystemAndCount instead

    See $OodleTexRT_Plugins_SetJobSystemAndCount
*/


OOFUNC1 void OOFUNC2 OodleTexRT_Plugins_SetJobSystemAndCount(
    t_fp_OodleTexRT_Plugin_RunJob * fp_RunJob,
    t_fp_OodleTexRT_Plugin_WaitJob * fp_WaitJob,
    int target_parallelism);
/* Set the function pointers for async job system used by Oodle.

    $:fp_RunJob     pointer to RunJob function
    $:fp_WaitJob    pointer to WaitJob function
    $:target_parallelism    goal of number of jobs to run simultaneously

    If these are not set, the default implementation runs jobs synchronously on the calling thread.

    These must not be changed once they are set!  Set them once then don't change them.

    _target_parallelism_ allows you to tell Oodle how many Jobs it should try to keep in flight at once.
    Depending on the operation it may not be able to split work into this many jobs (so fewer will be used),
    but it will not exceed this count.

    For Oodle Data LZ work, typically _target_parallelism_ is usually best at the number of hardware cores
    not including hyper threads).

    For Oodle Texture BCN encoding work, _target_parallelism_ is usually best as the full number of hyper cores.

    In some cases you may wish to reduce _target_parallelism_ by 1 or 2 cores to leave some of the CPU free for
    other work.

    For example on a CPU with 16 cores and 32 hardware threads, for LZ work you might set _target_parallelism_ to 15
    when calling OodleCorePlugins.  For BC7 encoding you might set _target_parallelism_ to 30 when calling OodleTexPlugins.

    NOTE : if you are using Oodle Ext, do NOT call this.  OodleX_Init will install a job system for Oodle Core.
    Note OodleX only installs automatically to Oodle Core, not Net or Tex.  See example_jobify.cpp for manual
    plugin.

    Replaces deprecated $OodleTexRT_Plugins_SetJobSystem

    See $Oodle_About_Job_Threading_Plugins
*/

// the main func pointer for log :
OODEFFUNC typedef void (OODLE_CALLBACK t_fp_OodleTexRT_Plugin_Printf)(int verboseLevel,const char * file,int line,const char * fmt,...);
/* Function pointer to Oodle Core printf

    $:verboseLevel  verbosity of the message; 0-2 ; lower = more important
    $:file          C file that sent the message
    $:line          C line that sent the message
    $:fmt           vararg printf format string

    The logging function installed here must parse varargs like printf.

    _verboseLevel_ may be used to omit verbose messages.
*/

OOFUNC1 t_fp_OodleTexRT_Plugin_Printf * OOFUNC2 OodleTexRT_Plugins_SetPrintf(t_fp_OodleTexRT_Plugin_Printf * fp_rrRawPrintf);
/* Install the callback used by Oodle Core for logging

    $:fp_rrRawPrintf    function pointer to your log function; may be NULL to disable all logging
    $:return            returns the previous function pointer

    Use this function to install your own printf for Oodle Core.

    The default implementation in debug builds, if you install nothing, uses the C stdio printf for logging.
    On Microsoft platforms, it uses OutputDebugString and not stdio.

    To disable all logging, call OodleTexRT_Plugins_SetPrintf(NULL)

    WARNING : this function is NOT thread safe!  It should be done only once and done in a place where the caller can guarantee thread safety.

    In the debug build of Oodle, you can install OodleTexRT_Plugin_Printf_Verbose to get more verbose logging

*/

OODEFFUNC typedef OO_BOOL (OODLE_CALLBACK t_fp_OodleTexRT_Plugin_DisplayAssertion)(const char * file,const int line,const char * function,const char * message);
/* Function pointer to Oodle Core assert callback

    $:file          C file that triggered the assert
    $:line          C line that triggered the assert
    $:function      C function that triggered the assert (may be NULL)
    $:message       assert message
    $:return        true to break execution at the assertion site, false to continue

    This callback is called by Oodle Core when it detects an assertion condition.

    This will only happen in debug builds.


*/

OOFUNC1 t_fp_OodleTexRT_Plugin_DisplayAssertion * OOFUNC2 OodleTexRT_Plugins_SetAssertion(t_fp_OodleTexRT_Plugin_DisplayAssertion * fp_rrDisplayAssertion);
/* Install the callback used by Oodle Core for asserts

    $:fp_rrDisplayAssertion function pointer to your assert display function
    $:return            returns the previous function pointer

    Use this function to install your own display for Oodle Core assertions.
    This will only happen in debug builds.

    The default implementation in debug builds, if you install nothing, uses the C stderr printf for logging,
    except on Microsoft platforms where it uses OutputDebugString.

    WARNING : this function is NOT thread safe!  It should be done only once and done in a place where the caller can guarantee thread safety.

*/

//=============================================================


OOFUNC1 void * OOFUNC2 OodleTexRT_Plugin_MallocAligned_Default(OO_SINTa size,OO_S32 alignment);
OOFUNC1 void OOFUNC2 OodleTexRT_Plugin_Free_Default(void * ptr);
OOFUNC1 void OOFUNC2 OodleTexRT_Plugin_Printf_Default(int verboseLevel,const char * file,int line,const char * fmt,...);
OOFUNC1 void OOFUNC2 OodleTexRT_Plugin_Printf_Verbose(int verboseLevel,const char * file,int line,const char * fmt,...);
OOFUNC1 OO_BOOL OOFUNC2 OodleTexRT_Plugin_DisplayAssertion_Default(const char * file,const int line,const char * function,const char * message);
OOFUNC1 OO_U64 OOFUNC2 OodleTexRT_Plugin_RunJob_Default( t_fp_Oodle_Job * fp_job, void * job_data, OO_U64 * dependencies, int num_dependencies, void * user_ptr );
OOFUNC1 void OOFUNC2 OodleTexRT_Plugin_WaitJob_Default( OO_U64 job_handle, void * user_ptr );

//=============================================================

//idoc(parent,OodleAPI_TextureRTBase)

OOFUNC1 OodleTexRT_Err OOFUNC2 OodleTexRT_LogVersion();
/* Log the version and build of the Oodle Texture Runtime library.

    Logs with the plugged in logging callback.  See $OodleTexRT_Plugins_SetPrintf.

    Will return an error if internal initialization failed.
*/

OOFUNC1 const char * OOFUNC2 OodleTexRT_Err_GetName(OodleTexRT_Err error);
/* Maps $OodleTexRT_Err enum values to a corresponding string.

    Intended for when you want to turn an error code into an error message.
*/

//idoc(parent,OodleAPI_TextureRTBC7Prep)

OOFUNC1 OodleTexRT_Err OOFUNC2 OodleTexRT_BC7Prep_ReadHeader(const OodleTexRT_BC7PrepHeader * header,
    OO_SINTa * out_num_blocks, OO_SINTa * out_payload_size);
/* Reads and validates the header and determines the size of the decoded and coded BC7 blocks.

    $:header            Header for a BC7Prep data chunk.
    $:out_num_blocks    If non-null, receives number of coded BC7 blocks in this blob.
    $:out_payload_size  If non-null, receives size of corresponding BC7Prep chunk in bytes.
    $:return            OodleTexRT_OK on success, an error code if the header is malformed.

    These numbers are useful for memory management. The block count determines how large the
    data will be after decoding (and also how much scratch memory is required as per
    $OodleTexRT_BC7Prep_MinDecodeScratchSize) and the payload size determines the size of the
    BC7Prep data itself, in case you're not storing it anywhere else.

    This function is provided for convenience. If you use a fixed limit on the number of blocks in
    a single BC7Prep chunk and store their sizes elsewhere, you can just allocate all memory buffers
    once up front and don't need to call this function at all.
*/

OOFUNC1 OO_SINTa OOFUNC2 OodleTexRT_BC7Prep_MinDecodeScratchSize(OO_SINTa nblocks);
/* Returns how much decoder scratch memory (in bytes) is required for the given number of BC7 blocks.

    BC7Prep decoding temporarily needs some memory during decoding, the scratch working buffer.
    Its required size depends on the number of blocks encoded. On error (negative result), returns
    an $OodleTexRT_Err.

    You can either decide on a global maximum _nblocks_ and pre-allocate BC7Prep decoder scratch
    memory once for that many blocks, or manage everything more dynamically and use
    $OodleTexRT_BC7Prep_ReadHeader to figure out how many blocks a given BC7Prep chunk decodes to.

    If you need to decode multiple BC7Prep blocks in sequence, it is recommended to allocate
    the scratch buffer once (for the maximum _nblocks_ required) and reuse it between calls. Note
    that if you're concurrently decoding BC7Prep blocks on multiple threads, each thread neads
    its own scratch buffer.
*/

OOFUNC1 OO_SINTa OOFUNC2 OodleTexRT_BC7Prep_Decode(void * output_buf, OO_SINTa output_size,
    const void * bc7prep_data, OO_SINTa bc7prep_data_size,
    const OodleTexRT_BC7PrepHeader * header, OodleTexRT_BC7PrepDecodeFlags flags,
    void * scratch_buf, OO_SINTa scratch_size);
/* Decodes a BC7Prep data stream.

   $:output_buf         Where the decoded BC7 blocks get written to.
   $:output_size        Size of _output_buf_ in bytes.
   $:bc7prep_data       The encoded BC7Prep data chunk.
   $:bc7prep_data_size  Size of _bc7prep_data_ in bytes.
   $:header             The header for the data blob.
   $:flags              Decode flags. See notes below. Pass $OodleTexRT_BC7PrepDecodeFlags_None by default.
   $:scratch_buf        Pointer to a scratch working buffer.
   $:scratch_size       Size of _scratch_buf_ in bytes.
   $:return             Number of bytes written to _output_buf_ on success, a negative number ($OodleTexRT_Err) on error.

    Decodes the given BC7Prep data chunk to _output_buf_; _output_size_ must be at least 16 times the number of
    BC7 blocks encoded in the given BC7Prep data chunk.

    Returns number of bytes written; on success this equals the BC7 block size (16 bytes) times the number
    of blocks in the given BC7Prep chunk.

    You can determine the number of blocks and _bc7prep_data_size_ from the header using
    $OodleTexRT_BC7Prep_ReadHeader.

    If _output_buf_ is known to be in cached memory (i.e. not write-combined memory like GPU memory
    on consoles or sometimes GPU upload heaps on PC), you may set _flags_ to
    $OodleTexRT_BC7PrepDecodeFlags_DestinationIsCached. This is up to 20% faster when the destination is
    in cached memory, but sometimes takes 2x as long when it's not. When in doubt, leave it off.

    The scratch buffer is used for all working memory during the decode process. BC7Prep decoding
    does not perform any memory allocations. You can reuse the scratch memory between subsequent
    BC7Prep_Decode calls (as long as it's large enough), but two concurrent BC7Prep_Decodes running
    on different threads need to have their private scratch buffers.

    BC7Prep _encoding_ is performed by $OodleTex_BC7Prep_Encode and lives in the main Oodle Texture
    library, not the runtime.
*/

#ifdef _MSC_VER
#pragma warning(pop)
#pragma pack(pop, Oodle)
#endif

#endif // __OODLE2TEXRT_H_INCLUDED__
