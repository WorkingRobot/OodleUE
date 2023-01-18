
//===================================================
// Oodle2 Network header
// (C) Copyright 2018-2022 Epic Games Tools LLC
//===================================================

#ifndef __OODLE2_NET_H_INCLUDED__
#define __OODLE2_NET_H_INCLUDED__

#ifndef OODLE2NET_PUBLIC_HEADER
#define OODLE2NET_PUBLIC_HEADER 1
#endif

#ifndef __OODLE2BASE_H_INCLUDED__
#include "oodle2base.h"
#endif

#ifdef _MSC_VER
#pragma pack(push, Oodle2Net, 8)

#pragma warning(push)
#pragma warning(disable : 4127) // conditional is constant
#endif

// header version :
//  the DLL is incompatible when MAJOR is bumped
//  MINOR is for internal revs and bug fixes that don't affect API compatibility
#define OODLE2NET_VERSION_MAJOR         9
#define OODLE2NET_VERSION_MINOR         6

// OodleNetworkVersion string is 1 . MAJOR . MINOR
//  don't make it from macros cuz the doc tool has to parse the string literal

#define OodleNetworkVersion "2.9.6"    /*
*/

// function pointers to mallocs needed :

OODEFFUNC typedef void * (OODLE_CALLBACK t_fp_OodleNet_Plugin_MallocAligned)( OO_SINTa bytes, OO_S32 alignment);
/* Function pointer type for OodleMallocAligned

    $:bytes     number of bytes to allocate
    $:alignment required alignment of returned pointer
    $:return    pointer to memory allocated (must not be NULL)

    _alignment_ will always be a power of two

    _alignment_ will always be >= $OODLE_MALLOC_MINIMUM_ALIGNMENT

*/

OODEFFUNC typedef void (OODLE_CALLBACK t_fp_OodleNet_Plugin_Free)( void * ptr );
/* Function pointer type for OodleFree

    $:return    pointer to memory to free

*/

OOFUNC1 void OOFUNC2 OodleNet_Plugins_SetAllocators(
    t_fp_OodleNet_Plugin_MallocAligned * fp_OodleMallocAligned,
    t_fp_OodleNet_Plugin_Free * fp_OodleFree);
/* Set the function pointers for allocation needed by Oodle2 Core

    If these are not set, the default implementation on most platforms uses the C stdlib.
    On Microsoft platforms the default implementation uses HeapAlloc.

    These must not be changed once they are set!  Set them once then don't change them.

    

    If you want to ensure that Oodle is not doing any allocations, you can call OodleNet_Plugins_SetAllocators(NULL,NULL);
    If you do that, then any time Oodle needs to allocate memory internally, it will stop the process.
    It is STRONGLY not recommended that you ship that way.  You can verify that Oodle is not allocating, but then leave some
    fallback allocator installed when you actually ship just in case.

    Also note that on many consoles the standard allocation practices may not
    leave much heap memory for the C stdlib malloc.  In this case Oodle may fail to allocate.

*/

OODEFFUNC typedef OO_U64 (OODLE_CALLBACK t_fp_OodleNet_Plugin_RunJob)( t_fp_Oodle_Job * fp_job, void * job_data , OO_U64 * dependencies, int num_dependencies, void * user_ptr );
/* Function pointer type for OodleNet_Plugins_SetJobSystem

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

OODEFFUNC typedef void (OODLE_CALLBACK t_fp_OodleNet_Plugin_WaitJob)( OO_U64 job_handle, void * user_ptr );
/* Function pointer type for OodleNet_Plugins_SetJobSystem

    $:job_handle    a job handle returned from RunJob. Never 0.
    $:user_ptr      is passed through from the OodleLZ_CompressOptions.

    Waits until the job specified by job_handle is done and cleans up any associated resources. Oodle
    will call WaitJob exactly once for every RunJob call that didn't return 0.

    If job_handle was already completed, this should clean it up without waiting.

    A handle value should not be reused by another RunJob until WaitJob has been done with that value.

    WaitJob will not be called from running jobs.  It will be only be called from the original thread that
    invoked Oodle.  If you are running Oodle from a worker thread, ensure that that thread is allowed to wait
    on other job threads.

    See $Oodle_About_Job_Threading_Plugins
*/

OOFUNC1 void OOFUNC2 OodleNet_Plugins_SetJobSystem(
    t_fp_OodleNet_Plugin_RunJob * fp_RunJob,
    t_fp_OodleNet_Plugin_WaitJob * fp_WaitJob);
/* DEPRECATED use OodleNet_Plugins_SetJobSystemAndCount instead

    See $OodleNet_Plugins_SetJobSystemAndCount
*/


OOFUNC1 void OOFUNC2 OodleNet_Plugins_SetJobSystemAndCount(
    t_fp_OodleNet_Plugin_RunJob * fp_RunJob,
    t_fp_OodleNet_Plugin_WaitJob * fp_WaitJob,
    int target_parallelism);
/* Set the function pointers for async job system needed by Oodle2 Core

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

    Replaces deprecated $OodleNet_Plugins_SetJobSystem

    See $Oodle_About_Job_Threading_Plugins
*/

// the main func pointer for log :
OODEFFUNC typedef void (OODLE_CALLBACK t_fp_OodleNet_Plugin_Printf)(int verboseLevel,const char * file,int line,const char * fmt,...);
/* Function pointer to Oodle Core printf

    $:verboseLevel  verbosity of the message; 0-2 ; lower = more important
    $:file          C file that sent the message
    $:line          C line that sent the message
    $:fmt           vararg printf format string

    The logging function installed here must parse varargs like printf.

    _verboseLevel_ may be used to omit verbose messages.
*/

OOFUNC1 t_fp_OodleNet_Plugin_Printf * OOFUNC2 OodleNet_Plugins_SetPrintf(t_fp_OodleNet_Plugin_Printf * fp_rrRawPrintf);
/* Install the callback used by Oodle Core for logging

    $:fp_rrRawPrintf    function pointer to your log function; may be NULL to disable all logging
    $:return            returns the previous function pointer

    Use this function to install your own printf for Oodle Core.

    The default implementation in debug builds, if you install nothing, uses the C stdio printf for logging.
    On Microsoft platforms, it uses OutputDebugString and not stdio.

    To disable all logging, call OodleNet_Plugins_SetPrintf(NULL)

    WARNING : this function is NOT thread safe!  It should be done only once and done in a place where the caller can guarantee thread safety.

    In the debug build of Oodle, you can install OodleNet_Plugin_Printf_Verbose to get more verbose logging

*/

OODEFFUNC typedef OO_BOOL (OODLE_CALLBACK t_fp_OodleNet_Plugin_DisplayAssertion)(const char * file,const int line,const char * function,const char * message);
/* Function pointer to Oodle Core assert callback

    $:file          C file that triggered the assert
    $:line          C line that triggered the assert
    $:function      C function that triggered the assert (may be NULL)
    $:message       assert message
    $:return        true to break execution at the assertion site, false to continue

    This callback is called by Oodle Core when it detects an assertion condition.

    This will only happen in debug builds.


*/

OOFUNC1 t_fp_OodleNet_Plugin_DisplayAssertion * OOFUNC2 OodleNet_Plugins_SetAssertion(t_fp_OodleNet_Plugin_DisplayAssertion * fp_rrDisplayAssertion);
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


OOFUNC1 void * OOFUNC2 OodleNet_Plugin_MallocAligned_Default(OO_SINTa size,OO_S32 alignment);
OOFUNC1 void OOFUNC2 OodleNet_Plugin_Free_Default(void * ptr);
OOFUNC1 void OOFUNC2 OodleNet_Plugin_Printf_Default(int verboseLevel,const char * file,int line,const char * fmt,...);
OOFUNC1 void OOFUNC2 OodleNet_Plugin_Printf_Verbose(int verboseLevel,const char * file,int line,const char * fmt,...);
OOFUNC1 OO_BOOL OOFUNC2 OodleNet_Plugin_DisplayAssertion_Default(const char * file,const int line,const char * function,const char * message);
OOFUNC1 OO_U64 OOFUNC2 OodleNet_Plugin_RunJob_Default( t_fp_Oodle_Job * fp_job, void * job_data, OO_U64 * dependencies, int num_dependencies, void * user_ptr );
OOFUNC1 void OOFUNC2 OodleNet_Plugin_WaitJob_Default( OO_U64 job_handle, void * user_ptr );

//=============================================================

//=====================================================

#define OODLENETWORK1_MAX_DICTIONARY_SIZE   (1<<24)     /* Maximum size of dictionary for OodleNetwork1

*/

//=====================================================

typedef struct OodleNetwork1_Shared OodleNetwork1_Shared;
/* Opaque data type for OodleNetwork1_Shared

    This data is made from the shared static dictionary.  After it is made it is const,
    and can be used by all compression channels.

    This data is filled by $OodleNetwork1_Shared_SetWindow.

    You can allocate and free it yourself.  It must be of size  $OodleNetwork1_Shared_Size.

    Your server must have one of these, and each client must have the exact same one.   
*/

typedef struct OodleNetwork1TCP_State OodleNetwork1TCP_State;
/* Opaque data type for OodleNetwork1TCP_State

    This data is per-channel and adapts to the channel.  There must be one for each
    encoder and one for each decoder.

    This data is initialized either with $OodleNetwork1TCP_State_InitAsCopy or $OodleNetwork1TCP_State_Reset.

    You can allocate and free it yourself.  It must be of size  $OodleNetwork1TCP_State_Size.

    For compression only of server->client data, your server must have one of these objects for each transmission channel (client).     The client must have a matching one to receive from the server.  They must be kept in sync - each one must get the same calls to Encode or Decode in the same order.  If they ever get out of sync (eg. due to lost connection), then they must both be reset in the same way. (either $OodleNetwork1TCP_State_InitAsCopy or $OodleNetwork1TCP_State_Reset)
        
*/

// OodleNetwork1_Shared is just the Hash

#define OODLENETWORK1_HASH_BITS_DEFAULT (19)    /* Good default value for OodleNetwork1 hash table size log2
*/

// State is the per-channel coder
//  client will train an initial one
//  then copy from trained coder
//  need to be able to serialize it in/out (with possible endian correction)

#define OODLENETWORK1_DECOMP_BUF_OVERREAD_LEN   (5) /* compressed buffer must be sized to at least compLen+OODLENETWORK1_DECOMP_BUF_OVERREAD_LEN

    (note that this is strictly less than OodleNetwork1_CompressedBufferSizeNeeded(rawLen))

*/

OOFUNC1 OO_SINTa OOFUNC2 OodleNetwork1_Shared_Size(OO_S32 htbits);
/* Returns the size of memory required for an $OodleNetwork1_Shared object

    $:htbits    size of the OodleNetwork1 hash table (log2) ; typically 18-21 such as $OODLENETWORK1_HASH_BITS_DEFAULT

    Shared and State are allocated with malloc( Size() )
    
*/

OOFUNC1 OO_SINTa OOFUNC2 OodleNetwork1TCP_State_Size(void);
/* Returns the size of memory required for an $OodleNetwork1TCP_State object

    Shared and State are allocated with malloc( Size() )

*/

OOFUNC1 OO_SINTa OOFUNC2 OodleNetwork1_CompressedBufferSizeNeeded(OO_SINTa rawLen);
/* Returns the size of memory required for the compressed buffer passed to $OodleNetwork1TCP_Encode$

*/

OOFUNC1 void OOFUNC2 OodleNetwork1_Shared_SetWindow( OodleNetwork1_Shared * data,
        OO_S32 htbits,
        const void * window,
        OO_S32 window_size );
/* Fill a OodleNetwork1_Shared from provided data

    $:data  $OodleNetwork1_Shared object to fill
    $:htbits    size of the OodleNetwork1 hash table (log2) ; typically 18-21 such as $OODLENETWORK1_HASH_BITS_DEFAULT
    $:window    bytes of static dictionary data to use for compression
    $:window_size   size of window ; should be <= $OODLENETWORK1_MAX_DICTIONARY_SIZE
    
    This must be done on both the client and server to fill the $OodleNetwork1_Shared object used for compression.

    _window_ should be some typical transmitted data.  The better you can make it represent the common data seen, the better compression will be.  The most common types of packets should be placed at the end of the window.  

    You must load _window_ from disk somehow, or it could be data that you already have in memory for some other purpose - any data which both the client and server have exactly the same copy of can be used as the compression dictionary.  To save and load the window from disk you should generally use one of the standard $OodleLZ_About compressors.

    NOTE : window is not copied ; do not free it!  $OodleNetwork1_Shared keeps pointers into window, it must be kept allocated while this $OodleNetwork1_Shared is in use.

    You may call SetWindow multiple times on the same Shared data for purposes of training.

*/

OOFUNC1 void OOFUNC2 OodleNetwork1TCP_State_Reset(
        OodleNetwork1TCP_State * state);
/* Initialize a $OodleNetwork1TCP_State

    $:state $OodleNetwork1TCP_State to initialize
    
    Resets _state_ to a null state.
    
    Generally it is better to make a trained initial state with $OodleNetwork1TCP_Train
    and then use $OodleNetwork1TCP_State_InitAsCopy.
*/
        
OOFUNC1 void OOFUNC2 OodleNetwork1TCP_State_InitAsCopy(
        OodleNetwork1TCP_State * state, const OodleNetwork1TCP_State * from );
/* Initialize a $OodleNetwork1TCP_State as a copy of another state

    $:state $OodleNetwork1TCP_State to initialize
    $:from  $OodleNetwork1TCP_State that was previously filled, to copy from
    
    Resets _state_ to a copy of _from_ state.
    
    Generally _from_ was made with $OodleNetwork1TCP_Train and then saved to disk so that it could be stored on both the client and server.
*/

OOFUNC1 void OOFUNC2 OodleNetwork1TCP_Train(
        OodleNetwork1TCP_State * state,
        const OodleNetwork1_Shared * shared,
        const void ** training_packet_pointers, 
        const OO_S32 * training_packet_sizes,
        OO_S32 num_training_packets);
/* Fill a $OodleNetwork1TCP_State from training data

    $:state     the $OodleNetwork1TCP_State which is filled out; this state should not need to be initialized in any way before calling Train, it will be reset internally.
    $:shared    the $OodleNetwork1_Shared data to use in compression ; this shared data should already have had $OodleNetwork1_Shared_SetWindow done on it.
    $:training_packet_pointers  array of pointers to packet data; array of size num_training_packets
    $:training_packet_sizes     array of sizes of packets; array of size num_training_packets
    $:num_training_packets      number of packets
        
    OodleNetwork1TCP_Train uses the provided training packet data to initialize _state_.
    
    The training packet data provided here should not overlap the window passed to $OodleNetwork1_Shared_SetWindow ; it should not come from the same source or you will get false training.

    You may call $OodleNetwork1_Shared_SetWindow and $OodleNetwork1TCP_Train many times with different windows to optimize the window selection.

    Once training is done, the resulting $OodleNetwork1TCP_State should be written to disk and used by both the client and server as the initial channel state in $OodleNetwork1TCP_State_InitAsCopy.
*/

OOFUNC1 OO_SINTa OOFUNC2 OodleNetwork1TCP_Encode(
        OodleNetwork1TCP_State * state,
        const OodleNetwork1_Shared * shared,
        const void * raw, OO_SINTa rawLen,
        void * comp );
/* Encode a packet

    $:state     state of this compression channel; will be mutated
    $:shared    const shared compression context
    $:raw       packet bytes to compress
    $:rawLen    size of the packet to compress ; can be >= 0
    $:comp      output compressed bytes; must be allocated to at least $OodleNetwork1_CompressedBufferSizeNeeded bytes
    $:return    length of output compressed data written to _comp_ ; the returned compLen is strictly <= rawLen

    Encodes one packet.  _state_ is mutated, learning from this packet for future packets.

    The returned compLen will never be greater than rawLen, because OodleNetwork1 won't send packets that expand under compression (it just sends them uncompressed) - however it may write further than that during the compression attempt.  Do not use the returned compLen to check the size of the compressed buffer needed.

*/

OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1TCP_Decode(
        OodleNetwork1TCP_State * state,
        const OodleNetwork1_Shared * shared,
        const void * comp, OO_SINTa compLen,
        void * raw, OO_SINTa rawLen );
/* Decode a packet

    $:state     state of this compression channel; will be mutated
    $:shared    const shared compression context
    $:comp      compressed packet received
    $:compLen   size of compressed data
    $:raw       output decompressed packet
    $:rawLen    size of the packet to write
    $:return    false for failure

    Decodes one packet.  _state_ is mutated, learning from this packet for future packets.

    The _rawLen_ provided here must match the length used in $OodleNetwork1TCP_Encode when creating this compressed packet.  The OodleNetwork1 data is headerless, it's up to you to send the packet decompressed size in your own header.

    If corrupt data is detected, false is returned.
    
    If the number of compressed bytes consumed does not match _compLen_, false is returned.
    If the number of output bytes does not match _rawLen_, false is returned.
    
    This function, however, does not do verify data integrity.  It will return 'true' if the correct number of bytes are coded,
    even if the data does not match.

    The buffer _comp_ must be allowed to read at least _compLen_ + OODLENETWORK1_DECOMP_BUF_OVERREAD_LEN bytes
    (note that this is strictly less than OodleNetwork1_CompressedBufferSizeNeeded(_rawLen_) , so that may be used as well)

*/

//=====================================================

typedef struct OodleNetwork1UDP_State OodleNetwork1UDP_State;
/* Opaque type for an $OodleNetwork1UDP_State

    OodleNetwork1UDP uses a $OodleNetwork1_Shared just like the non-UDP OodleNetwork1
*/

OOFUNC1 void OOFUNC2 OodleNetwork1UDP_Train(
        OodleNetwork1UDP_State * state,
        const OodleNetwork1_Shared * shared,
        const void ** training_packet_pointers, 
        const OO_S32 * training_packet_sizes,
        OO_S32 num_training_packets);
/* Fill a $OodleNetwork1UDP_State from training data

    $:state     the $OodleNetwork1UDP_State which is filled out; this state should not need to be initialized in any way before calling Train, it will be reset internally.
    $:shared    the $OodleNetwork1_Shared data to use in compression ; this shared data should already have had $OodleNetwork1_Shared_SetWindow done on it.
    $:training_packet_pointers  array of pointers to packet data; array of size num_training_packets
    $:training_packet_sizes     array of sizes of packets; array of size num_training_packets
    $:num_training_packets      number of packets
        
    OodleNetwork1UDP_Train uses the provided training packet data to initialize _state_.
    
    The training packet data provided here should not overlap the window passed to $OodleNetwork1_Shared_SetWindow ; it should not come from the same source or you will get false training.

    You may call $OodleNetwork1_Shared_SetWindow and $OodleNetwork1UDP_Train many times with different windows to optimize the window selection.

    Once training is done, the resulting $OodleNetwork1UDP_State should be written to disk and used by both the client and server.

    There's no need to copy a $OodleNetwork1UDP_State , the same state object can be used by all encoders and decoders.
*/

OOFUNC1 OO_SINTa OOFUNC2 OodleNetwork1UDP_State_Size(void);
/* Returns the size of memory required for an $OodleNetwork1UDP_State object

    Shared and State are allocated with malloc( Size() )

*/

OOFUNC1 OO_SINTa OOFUNC2 OodleNetwork1UDP_Encode(
        const OodleNetwork1UDP_State * state,
        const OodleNetwork1_Shared * shared,
        const void * raw, OO_SINTa rawLen,
        void * comp );
/* Encode a packet

    $:state     const shared compression state
    $:shared    const shared compression context
    $:raw       packet bytes to compress
    $:rawLen    size of the packet to compress ; can be >= 0
    $:comp      output compressed bytes; must be allocated to at least $OodleNetwork1_CompressedBufferSizeNeeded bytes
    $:return    length of output compressed data written to _comp_ ; the returned compLen is strictly <= rawLen

    Encodes one packet.
    
    _state_ and _shared_ are both const and can be shared by all encoders and decoders.
    
    The returned compLen will never be greater than rawLen, because OodleNetwork1 won't send packets that expand under compression (it just sends them uncompressed) - however it may write further than that during the compression attempt.  Do not use the returned compLen to check the size of the compressed buffer needed.

*/

OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1UDP_Decode(
        const OodleNetwork1UDP_State * state,
        const OodleNetwork1_Shared * shared,
        const void * comp,  OO_SINTa compLen,
        void * raw, OO_SINTa rawLen );
/* Decode a packet

    $:state     const shared compression state
    $:shared    const shared compression context
    $:comp      compressed packet received
    $:compLen   size of compressed data
    $:raw       output decompressed packet
    $:rawLen    size of the packet to write
    $:return    false for failure

    Decodes one packet. 
    
    _state_ and _shared_ are both const and can be shared by all encoders and decoders.

    The _rawLen_ provided here must match the length used in $OodleNetwork1UDP_Encode when creating this compressed packet.  The OodleNetwork1 data is headerless, it's up to you to send the packet decompressed size in your own header.

    If corrupt data is detected, false is returned.
    
    If the number of compressed bytes consumed does not match _compLen_, false is returned.
    If the number of output bytes does not match _rawLen_, false is returned.
    
    This function, however, does not do verify data integrity.  It will return 'true' if the correct number of bytes are coded,
    even if the data does not match.

    The buffer _comp_ must be allowed to read at least _compLen_ + OODLENETWORK1_DECOMP_BUF_OVERREAD_LEN bytes
    (note that this is strictly less than OodleNetwork1_CompressedBufferSizeNeeded(_rawLen_) , so that may be used as well)

*/

//=============================================================

typedef struct OodleNetwork1UDP_StateCompacted OodleNetwork1UDP_StateCompacted;
/* Opaque type for an $OodleNetwork1UDP_StateCompacted

    Compacted version of OodleNetwork1UDP_State
    
    Used to decrease the size of OodleNetwork1UDP_State for storage.
    You cannot code with a OodleNetwork1UDP_StateCompacted.
*/

OOFUNC1 OO_SINTa OOFUNC2 OodleNetwork1UDP_StateCompacted_MaxSize(void);
/* Returns the size of memory required for an $OodleNetwork1UDP_StateCompacted object

    Shared and State are allocated with malloc( Size() )

*/

OOFUNC1 OO_SINTa OOFUNC2 OodleNetwork1UDP_State_Compact_ForVersion( OodleNetwork1UDP_StateCompacted * to, const OodleNetwork1UDP_State * from, OO_S32 for_oodle_major_version);
/* See $OodleNetwork1UDP_State_Compact
* 
* takes oodle_major_version to target
* 
* Oodle Network Compacted state changed from major version 5 to 6 (eg 2.5.5 to 2.6.0)
* 
*/

OOFUNC1 OO_SINTa OOFUNC2 OodleNetwork1UDP_State_Compact( OodleNetwork1UDP_StateCompacted * to, const OodleNetwork1UDP_State * from );
/* Fills a OodleNetwork1UDP_StateCompacted from a OodleNetwork1UDP_State

    $:to    filled out
    $:from  read
    $:return number of bytes filled in _to_

    Use this when the OodleNetwork1UDP_State is created to make a Compacted state to save to a file.

    _to_ should be allocated to at least $OodleNetwork1UDP_StateCompacted_MaxSize

    Note - use the return value to save only the prefix of the Compacted state.

*/

OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1UDP_State_Uncompact_ForVersion( OodleNetwork1UDP_State * to, const OodleNetwork1UDP_StateCompacted * from, OO_S32 for_oodle_major_version);
/* See $OodleNetwork1UDP_State_Uncompact
* 
* takes oodle_major_version to target
* 
* Oodle Network Compacted state changed from major version 5 to 6 (eg 2.5.5 to 2.6.0)
* 
*/

OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1UDP_State_Uncompact( OodleNetwork1UDP_State * to, const OodleNetwork1UDP_StateCompacted * from );
/* Fills a OodleNetwork1UDP_State from a OodleNetwork1UDP_StateCompacted

    $:to    filled out
    $:from  read
    $:return false if invalid data is detected

    Use this when the OodleNetwork1UDP_StateCompacted is read from a file, so that the OodleNetwork1UDP_State can be used for coding.  You may discard the OodleNetwork1UDP_StateCompacted after this call.

    _to_ should be allocated to $OodleNetwork1UDP_State_Size bytes.

    NOTE : the return value here is not a robust way to detect tampered data.  You should use encryption and/or a safe checksum on your saved model file data.

*/      

//============================================================

OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1_SelectDictionarySupported(void);
/* Returns whether this version of the library can build new dictionaries.
 
   This functionality is only available on host platforms, not embedded
   devices, game consoles, phones etc.
*/

OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1_SelectDictionaryFromPackets(
        void * dictionary_to_fill,
        OO_S32 dictionary_size,
        OO_S32 htbits,
        const void ** dictionary_packet_pointers, 
        const OO_S32 * dictionary_packet_sizes,
        OO_S32 num_dictionary_packets,
        const void ** test_packet_pointers, 
        const OO_S32 * test_packet_sizes,
        OO_S32 num_test_packets
        );
/* Build a dictionary for OodleNetwork1 from packets

        $:dictionary_to_fill    must be allocated by you to _dictionary_size_ bytes; filled out
        $:dictionary_size       size of _dictionary_to_fill_ (should be <= $OODLENETWORK1_MAX_DICTIONARY_SIZE)
        $:htbits                hash table bits; should be the same value you pass to $OodleNetwork1_Shared_SetWindow
        $:dictionary_packet_pointers    pointers to packet data, _num_dictionary_packets_ of them 
        $:dictionary_packet_sizes       packet sizes, _num_dictionary_packets_ of them
        $:num_dictionary_packets        number of packets used to make the dictionary
        $:test_packet_pointers          pointers to packet data, _num_test_packets_ of them
        $:test_packet_sizes             packet sizes, _num_test_packets_ of them
        $:num_test_packets              number of packets used to test the dictionary
        $:return                true on success, false otherwise.
    
    Fills out a dictionary from packets by rating the packets and choosing the most useful.
    
    Can be used with OodleNetwork1 UDP or TCP.
    
    The _dictionary_packet_pointers_ are used to fill the dictionary.  The _test_packet_pointers_
    are used to evaluate the quality of the dictionary.  They should not be the same packets!
    They should both be independent random representative samples of your network traffic.
    
    The packets used here should not be the same ones used for training!  (eg. with
    $OodleNetwork1UDP_Train or OodleNetwork1TCP_Train).
    
    This function is to be called before $OodleNetwork1_Shared_SetWindow.

    This function may take a lot of time if the _test_packet_pointers_ set is too large.  It should
    be large enough to provide good test ratings, but too large slows it down for no reason.
    Something like 50 MB of packet data is usually sufficient.

    See TestOodleNetwork1PacketCoder_SelectDictionaryAndTrain in $example_packet
    
    On platforms where $OodleNetwork1_SelectDictionarySupported returns false, this
    function is not supported.
    
    It's advised to run this function in 64-bit, as it can use a lot of memory.
*/      

OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1_SelectDictionaryFromPackets_Trials(
        void * dictionary_to_fill,
        OO_S32 dictionary_size,
        OO_S32 htbits,
        const void ** dictionary_packet_pointers, 
        const OO_S32 * dictionary_packet_sizes,
        OO_S32 num_dictionary_packets,
        const void ** test_packet_pointers, 
        const OO_S32 * test_packet_sizes,
        OO_S32 num_test_packets,
        OO_S32 num_trials,
        double randomness_percent,
        OO_S32 num_generations
        );
/* Multi-Trial variant of $OodleNetwork1_SelectDictionaryFromPackets

        $:num_trials    number of trials per generation; 5-20 is a good starting range
        $:randomness_percent    randomness of trials; this is a percent of standard, 100 is a good default; 50-200 is a useful range
        $:num_generations       number of generations; 1 is fine, more is slower
        $:return                true on success, false otherwise.
    
    This function runs the packet selector of $OodleNetwork1_SelectDictionaryFromPackets
    repeatedly with some randomness.  Variation of packet selection can sometimes give
    slightly better dictionaries.
    
    The success of the trial is measured on the "test_packet" set.  Make sure that the
    "dictionary_packet" set are independently randomly drawn from the packet source data
    wrst the "test_packet" set so that over-training degeneracies are not created.
    
    On platforms where $OodleNetwork1_SelectDictionarySupported returns false, this
    function is not supported.
    
    It's advised to run this function in 64-bit, as it can use a lot of memory.
*/

//===============================================================

#ifdef _MSC_VER
#pragma warning(pop)
#pragma pack(pop, Oodle2Net)
#endif

#endif // __OODLE2_NET_H_INCLUDED__
