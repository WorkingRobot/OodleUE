// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(parent,OodleAPI_OodleNetwork1)
#pragma once

#include "oodlebase.h"

OODLE_NS_START

PUBTYPESTART

PUBTYPEEND

PUBSTART

//=====================================================

#define OODLENETWORK1_MAX_DICTIONARY_SIZE	(1<<24)		IDOC
/* Maximum size of dictionary for OodleNetwork1

*/

//=====================================================

IDOC typedef struct OodleNetwork1_Shared OodleNetwork1_Shared;
/* Opaque data type for OodleNetwork1_Shared

	This data is made from the shared static dictionary.  After it is made it is const,
	and can be used by all compression channels.

	This data is filled by $OodleNetwork1_Shared_SetWindow.

	You can allocate and free it yourself.  It must be of size	$OodleNetwork1_Shared_Size.

	Your server must have one of these, and each client must have the exact same one.	
*/

IDOC typedef struct OodleNetwork1TCP_State OodleNetwork1TCP_State;
/* Opaque data type for OodleNetwork1TCP_State

	This data is per-channel and adapts to the channel.  There must be one for each
	encoder and one for each decoder.

	This data is initialized either with $OodleNetwork1TCP_State_InitAsCopy or $OodleNetwork1TCP_State_Reset.

	You can allocate and free it yourself.  It must be of size	$OodleNetwork1TCP_State_Size.

	For compression only of server->client data, your server must have one of these objects for each transmission channel (client).  	The client must have a matching one to receive from the server.  They must be kept in sync - each one must get the same calls to Encode or Decode in the same order.  If they ever get out of sync (eg. due to lost connection), then they must both be reset in the same way. (either $OodleNetwork1TCP_State_InitAsCopy or $OodleNetwork1TCP_State_Reset)
		
*/

// OodleNetwork1_Shared is just the Hash

#define OODLENETWORK1_HASH_BITS_DEFAULT	(19)	IDOC
/* Good default value for OodleNetwork1 hash table size log2
*/

// State is the per-channel coder
//  client will train an initial one
//	then copy from trained coder
//	need to be able to serialize it in/out (with possible endian correction)

#define OODLENETWORK1_DECOMP_BUF_OVERREAD_LEN	(5)	IDOC
/* compressed buffer must be sized to at least compLen+OODLENETWORK1_DECOMP_BUF_OVERREAD_LEN

	(note that this is strictly less than OodleNetwork1_CompressedBufferSizeNeeded(rawLen))

*/

IDOC OOFUNC1 OO_SINTa OOFUNC2 OodleNetwork1_Shared_Size(OO_S32 htbits);
/* Returns the size of memory required for an $OodleNetwork1_Shared object

	$:htbits	size of the OodleNetwork1 hash table (log2) ; typically 18-21 such as $OODLENETWORK1_HASH_BITS_DEFAULT

	Shared and State are allocated with malloc( Size() )
	
*/

IDOC OOFUNC1 OO_SINTa OOFUNC2 OodleNetwork1TCP_State_Size(void);
/* Returns the size of memory required for an $OodleNetwork1TCP_State object

	Shared and State are allocated with malloc( Size() )

*/

IDOC OOFUNC1 OO_SINTa OOFUNC2 OodleNetwork1_CompressedBufferSizeNeeded(OO_SINTa rawLen);
/* Returns the size of memory required for the compressed buffer passed to $OodleNetwork1TCP_Encode$

*/

IDOC OOFUNC1 void OOFUNC2 OodleNetwork1_Shared_SetWindow( OodleNetwork1_Shared * data,
		OO_S32 htbits,
		const void * window,
		OO_S32 window_size );
/* Fill a OodleNetwork1_Shared from provided data

	$:data	$OodleNetwork1_Shared object to fill
	$:htbits	size of the OodleNetwork1 hash table (log2) ; typically 18-21 such as $OODLENETWORK1_HASH_BITS_DEFAULT
	$:window	bytes of static dictionary data to use for compression
	$:window_size	size of window ; should be <= $OODLENETWORK1_MAX_DICTIONARY_SIZE
	
	This must be done on both the client and server to fill the $OodleNetwork1_Shared object used for compression.

	_window_ should be some typical transmitted data.  The better you can make it represent the common data seen, the better compression will be.  The most common types of packets should be placed at the end of the window.  

	You must load _window_ from disk somehow, or it could be data that you already have in memory for some other purpose - any data which both the client and server have exactly the same copy of can be used as the compression dictionary.  To save and load the window from disk you should generally use one of the standard $OodleLZ_About compressors.

	NOTE : window is not copied ; do not free it!  $OodleNetwork1_Shared keeps pointers into window, it must be kept allocated while this $OodleNetwork1_Shared is in use.

	You may call SetWindow multiple times on the same Shared data for purposes of training.

*/

IDOC OOFUNC1 void OOFUNC2 OodleNetwork1TCP_State_Reset(
		OodleNetwork1TCP_State * state);
/* Initialize a $OodleNetwork1TCP_State

	$:state	$OodleNetwork1TCP_State to initialize
	
	Resets _state_ to a null state.
	
	Generally it is better to make a trained initial state with $OodleNetwork1TCP_Train
	and then use $OodleNetwork1TCP_State_InitAsCopy.
*/
		
IDOC OOFUNC1 void OOFUNC2 OodleNetwork1TCP_State_InitAsCopy(
		OodleNetwork1TCP_State * state, const OodleNetwork1TCP_State * from );
/* Initialize a $OodleNetwork1TCP_State as a copy of another state

	$:state	$OodleNetwork1TCP_State to initialize
	$:from	$OodleNetwork1TCP_State that was previously filled, to copy from
	
	Resets _state_ to a copy of _from_ state.
	
	Generally _from_ was made with $OodleNetwork1TCP_Train and then saved to disk so that it could be stored on both the client and server.
*/

IDOC OOFUNC1 void OOFUNC2 OodleNetwork1TCP_Train(
		OodleNetwork1TCP_State * state,
		const OodleNetwork1_Shared * shared,
		const void ** training_packet_pointers, 
		const OO_S32 * training_packet_sizes,
		OO_S32 num_training_packets);
/* Fill a $OodleNetwork1TCP_State from training data

	$:state		the $OodleNetwork1TCP_State which is filled out; this state should not need to be initialized in any way before calling Train, it will be reset internally.
	$:shared	the $OodleNetwork1_Shared data to use in compression ; this shared data should already have had $OodleNetwork1_Shared_SetWindow done on it.
	$:training_packet_pointers	array of pointers to packet data; array of size num_training_packets
	$:training_packet_sizes		array of sizes of packets; array of size num_training_packets
	$:num_training_packets		number of packets
		
	OodleNetwork1TCP_Train uses the provided training packet data to initialize _state_.
	
	The training packet data provided here should not overlap the window passed to $OodleNetwork1_Shared_SetWindow ; it should not come from the same source or you will get false training.

	You may call $OodleNetwork1_Shared_SetWindow and $OodleNetwork1TCP_Train many times with different windows to optimize the window selection.

	Once training is done, the resulting $OodleNetwork1TCP_State should be written to disk and used by both the client and server as the initial channel state in $OodleNetwork1TCP_State_InitAsCopy.
*/

IDOC OOFUNC1 OO_SINTa OOFUNC2 OodleNetwork1TCP_Encode(
		OodleNetwork1TCP_State * state,
		const OodleNetwork1_Shared * shared,
		const void * raw, OO_SINTa rawLen,
		void * comp );
/* Encode a packet

	$:state		state of this compression channel; will be mutated
	$:shared	const shared compression context
	$:raw		packet bytes to compress
	$:rawLen	size of the packet to compress ; can be >= 0
	$:comp		output compressed bytes; must be allocated to at least $OodleNetwork1_CompressedBufferSizeNeeded bytes
	$:return	length of output compressed data written to _comp_ ; the returned compLen is strictly <= rawLen

	Encodes one packet.  _state_ is mutated, learning from this packet for future packets.

	The returned compLen will never be greater than rawLen, because OodleNetwork1 won't send packets that expand under compression (it just sends them uncompressed) - however it may write further than that during the compression attempt.  Do not use the returned compLen to check the size of the compressed buffer needed.

*/

IDOC OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1TCP_Decode(
		OodleNetwork1TCP_State * state,
		const OodleNetwork1_Shared * shared,
		const void * comp, OO_SINTa compLen,
		void * raw, OO_SINTa rawLen );
/* Decode a packet

	$:state		state of this compression channel; will be mutated
	$:shared	const shared compression context
	$:comp		compressed packet received
	$:compLen	size of compressed data
	$:raw		output decompressed packet
	$:rawLen	size of the packet to write
	$:return	false for failure

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

IDOC typedef struct OodleNetwork1UDP_State OodleNetwork1UDP_State;
/* Opaque type for an $OodleNetwork1UDP_State

	OodleNetwork1UDP uses a $OodleNetwork1_Shared just like the non-UDP OodleNetwork1
*/

IDOC OOFUNC1 void OOFUNC2 OodleNetwork1UDP_Train(
		OodleNetwork1UDP_State * state,
		const OodleNetwork1_Shared * shared,
		const void ** training_packet_pointers, 
		const OO_S32 * training_packet_sizes,
		OO_S32 num_training_packets);
/* Fill a $OodleNetwork1UDP_State from training data

	$:state		the $OodleNetwork1UDP_State which is filled out; this state should not need to be initialized in any way before calling Train, it will be reset internally.
	$:shared	the $OodleNetwork1_Shared data to use in compression ; this shared data should already have had $OodleNetwork1_Shared_SetWindow done on it.
	$:training_packet_pointers	array of pointers to packet data; array of size num_training_packets
	$:training_packet_sizes		array of sizes of packets; array of size num_training_packets
	$:num_training_packets		number of packets
		
	OodleNetwork1UDP_Train uses the provided training packet data to initialize _state_.
	
	The training packet data provided here should not overlap the window passed to $OodleNetwork1_Shared_SetWindow ; it should not come from the same source or you will get false training.

	You may call $OodleNetwork1_Shared_SetWindow and $OodleNetwork1UDP_Train many times with different windows to optimize the window selection.

	Once training is done, the resulting $OodleNetwork1UDP_State should be written to disk and used by both the client and server.

	There's no need to copy a $OodleNetwork1UDP_State , the same state object can be used by all encoders and decoders.
*/

IDOC OOFUNC1 OO_SINTa OOFUNC2 OodleNetwork1UDP_State_Size(void);
/* Returns the size of memory required for an $OodleNetwork1UDP_State object

	Shared and State are allocated with malloc( Size() )

*/

IDOC OOFUNC1 OO_SINTa OOFUNC2 OodleNetwork1UDP_Encode(
		const OodleNetwork1UDP_State * state,
		const OodleNetwork1_Shared * shared,
		const void * raw, OO_SINTa rawLen,
		void * comp );
/* Encode a packet

	$:state		const shared compression state
	$:shared	const shared compression context
	$:raw		packet bytes to compress
	$:rawLen	size of the packet to compress ; can be >= 0
	$:comp		output compressed bytes; must be allocated to at least $OodleNetwork1_CompressedBufferSizeNeeded bytes
	$:return	length of output compressed data written to _comp_ ; the returned compLen is strictly <= rawLen

	Encodes one packet.
	
	_state_ and _shared_ are both const and can be shared by all encoders and decoders.
	
	The returned compLen will never be greater than rawLen, because OodleNetwork1 won't send packets that expand under compression (it just sends them uncompressed) - however it may write further than that during the compression attempt.  Do not use the returned compLen to check the size of the compressed buffer needed.

*/

IDOC OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1UDP_Decode(
		const OodleNetwork1UDP_State * state,
		const OodleNetwork1_Shared * shared,
		const void * comp,  OO_SINTa compLen,
		void * raw, OO_SINTa rawLen );
/* Decode a packet

	$:state		const shared compression state
	$:shared	const shared compression context
	$:comp		compressed packet received
	$:compLen	size of compressed data
	$:raw		output decompressed packet
	$:rawLen	size of the packet to write
	$:return	false for failure

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

IDOC typedef struct OodleNetwork1UDP_StateCompacted OodleNetwork1UDP_StateCompacted;
/* Opaque type for an $OodleNetwork1UDP_StateCompacted

	Compacted version of OodleNetwork1UDP_State
	
	Used to decrease the size of OodleNetwork1UDP_State for storage.
	You cannot code with a OodleNetwork1UDP_StateCompacted.
*/

IDOC OOFUNC1 OO_SINTa OOFUNC2 OodleNetwork1UDP_StateCompacted_MaxSize(void);
/* Returns the size of memory required for an $OodleNetwork1UDP_StateCompacted object

	Shared and State are allocated with malloc( Size() )

*/

IDOC OOFUNC1 OO_SINTa OOFUNC2 OodleNetwork1UDP_State_Compact_ForVersion( OodleNetwork1UDP_StateCompacted * to, const OodleNetwork1UDP_State * from, OO_S32 for_oodle_major_version);
/* See $OodleNetwork1UDP_State_Compact
* 
* takes oodle_major_version to target
* 
* Oodle Network Compacted state changed from major version 5 to 6 (eg 2.5.5 to 2.6.0)
* 
*/

IDOC OOFUNC1 OO_SINTa OOFUNC2 OodleNetwork1UDP_State_Compact( OodleNetwork1UDP_StateCompacted * to, const OodleNetwork1UDP_State * from );
/* Fills a OodleNetwork1UDP_StateCompacted from a OodleNetwork1UDP_State

	$:to	filled out
	$:from	read
	$:return number of bytes filled in _to_

	Use this when the OodleNetwork1UDP_State is created to make a Compacted state to save to a file.

	_to_ should be allocated to at least $OodleNetwork1UDP_StateCompacted_MaxSize

	Note - use the return value to save only the prefix of the Compacted state.

*/

IDOC OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1UDP_State_Uncompact_ForVersion( OodleNetwork1UDP_State * to, const OodleNetwork1UDP_StateCompacted * from, OO_S32 for_oodle_major_version);
/* See $OodleNetwork1UDP_State_Uncompact
* 
* takes oodle_major_version to target
* 
* Oodle Network Compacted state changed from major version 5 to 6 (eg 2.5.5 to 2.6.0)
* 
*/

IDOC OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1UDP_State_Uncompact( OodleNetwork1UDP_State * to, const OodleNetwork1UDP_StateCompacted * from );
/* Fills a OodleNetwork1UDP_State from a OodleNetwork1UDP_StateCompacted

	$:to	filled out
	$:from	read
	$:return false if invalid data is detected

	Use this when the OodleNetwork1UDP_StateCompacted is read from a file, so that the OodleNetwork1UDP_State can be used for coding.  You may discard the OodleNetwork1UDP_StateCompacted after this call.

	_to_ should be allocated to $OodleNetwork1UDP_State_Size bytes.

	NOTE : the return value here is not a robust way to detect tampered data.  You should use encryption and/or a safe checksum on your saved model file data.

*/		

//============================================================

IDOC OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1_SelectDictionarySupported(void);
/* Returns whether this version of the library can build new dictionaries.
 
   This functionality is only available on host platforms, not embedded
   devices, game consoles, phones etc.
*/

IDOC OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1_SelectDictionaryFromPackets(
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

		$:dictionary_to_fill	must be allocated by you to _dictionary_size_ bytes; filled out
		$:dictionary_size		size of _dictionary_to_fill_ (should be <= $OODLENETWORK1_MAX_DICTIONARY_SIZE)
		$:htbits				hash table bits; should be the same value you pass to $OodleNetwork1_Shared_SetWindow
		$:dictionary_packet_pointers	pointers to packet data, _num_dictionary_packets_ of them 
		$:dictionary_packet_sizes		packet sizes, _num_dictionary_packets_ of them
		$:num_dictionary_packets		number of packets used to make the dictionary
		$:test_packet_pointers			pointers to packet data, _num_test_packets_ of them
		$:test_packet_sizes				packet sizes, _num_test_packets_ of them
		$:num_test_packets				number of packets used to test the dictionary
		$:return				true on success, false otherwise.
	
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

IDOC OOFUNC1 OO_BOOL OOFUNC2 OodleNetwork1_SelectDictionaryFromPackets_Trials(
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

		$:num_trials	number of trials per generation; 5-20 is a good starting range
		$:randomness_percent	randomness of trials; this is a percent of standard, 100 is a good default; 50-200 is a useful range
		$:num_generations		number of generations; 1 is fine, more is slower
		$:return				true on success, false otherwise.
	
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
	
PUBEND

//=====================================================

OODLE_NS_END
