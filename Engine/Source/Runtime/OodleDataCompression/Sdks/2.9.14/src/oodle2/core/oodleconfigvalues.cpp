// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

//idoc(reparse,x.h)
#include "oodleconfigvalues.h"
#include "oodlecore.h"
#include "oodleversion.h"

OODLE_NS_START

/***********

NOTEZ : OodlePubThunks publicates all this in
struct OodleXConfigValues


*************/

S32 g_Oodle_Debug_TweakValue = 0;


S32 g_OodleLZ_LW_MinSizeLZHFallback = 512;


// LRM gets a lot more compression at low step & hash length (on some files)
// but also takes a lot more memory & time
S32 g_OodleLZ_LW_LRM_step = 6;
S32 g_OodleLZ_LW_LRM_hashLength = 8;
// faster, worse compression :
//S32 g_OodleLZ_LW_LRM_step = 8;
//S32 g_OodleLZ_LW_LRM_hashLength = 12;
S32 g_OodleLZ_LW_LRM_jumpbits = 10;


#if OODLE_PLATFORM_LARGEMEMORY
S32 g_OodleLZ_Decoder_Max_Stack_Size = 32*1024;
#else
S32 g_OodleLZ_Decoder_Max_Stack_Size = 4*1024;
#endif

//S32 g_OodleLZ_Small_Buffer_LZ_Fallback_Size = 2048;
//S32 g_OodleLZ_Small_Buffer_LZ_Fallback_Size = 0; // 08/08/2017 change default to 0 so it doesn't happen unless requested

S32 g_OodleLZ_BackwardsCompatible_MajorVersion = OODLE2_VERSION_MAJOR;

bool g_Oodle_UsageWarningsDisabled = false;

//===================================

//idoc(parent,OodleAPI_Base)

PUBSTART


//=====================================================


IDOC typedef OOSTRUCT OodleConfigValues
{
	OO_S32 m_OodleLZ_LW_LRM_step;			// LZHLW LRM : bytes between LRM entries
	OO_S32 m_OodleLZ_LW_LRM_hashLength;	// LZHLW LRM : bytes hashed for each LRM entries
	OO_S32 m_OodleLZ_LW_LRM_jumpbits;		// LZHLW LRM : bits of hash used for jump table

	OO_S32 m_OodleLZ_Decoder_Max_Stack_Size;	// if OodleLZ_Decompress needs to allocator a Decoder object, and it's smaller than this size, it's put on the stack instead of the heap
	OO_S32 m_OodleLZ_Small_Buffer_LZ_Fallback_Size_Unused; // deprecated
	OO_S32 m_OodleLZ_BackwardsCompatible_MajorVersion; // if you need to encode streams that can be read with an older version of Oodle, set this to the Oodle2 MAJOR version number that you need compatibility with.  eg to be compatible with oodle 2.7.3 you would put 7 here

	OO_U32 m_oodle_header_version;	// = OODLE_HEADER_VERSION

} OodleConfigValues;
/* OodleConfigValues

	Struct of user-settable low level config values.  See $Oodle_SetConfigValues.

	May have different defaults per platform.
*/

IDOC OOFUNC1 void OOFUNC2 Oodle_GetConfigValues(OodleConfigValues * ptr);
/* Get $OodleConfigValues

	$:ptr	filled with OodleConfigValues

	Gets the current $OodleConfigValues.

	May be different per platform.
*/

IDOC OOFUNC1 void OOFUNC2 Oodle_SetConfigValues(const OodleConfigValues * ptr);
/* Set $OodleConfigValues

	$:ptr	your desired OodleConfigValues

	Sets the global $OodleConfigValues from your struct.

	You should call $Oodle_GetConfigValues to fill the struct, then change the values you
	want to change, then call $Oodle_SetConfigValues.

	This should generally be done before doing anything with Oodle (eg. even before OodleX_Init).
	Changing OodleConfigValues after Oodle has started has undefined effects.
*/

IDOC typedef enum Oodle_UsageWarnings
{
	Oodle_UsageWarnings_Enabled = 0,
	Oodle_UsageWarnings_Disabled = 1,
	Oodle_UsageWarnings_Force32 = 0x40000000
} Oodle_UsageWarnings;
/* Whether Oodle usage warnings are enable or disabled. */

IDOC OOFUNC1 void OOFUNC2 Oodle_SetUsageWarnings(Oodle_UsageWarnings state);
/* Enables or disables Oodle usage warnings.

    $:state    whether usage warnings should be enabled or disabled.

   Usage warnings are enabled by default and try to be low-noise, but in case you want to
   disable them, this is how.

   This should generally be done once at startup.  Setting this state while there are Oodle
   calls running on other threads has undefined results.
*/

PUBEND


#define XX_LIST \
	XX(OodleLZ_LW_LRM_step)\
	XX(OodleLZ_LW_LRM_hashLength)\
	XX(OodleLZ_LW_LRM_jumpbits) \
	XX(OodleLZ_Decoder_Max_Stack_Size) \
	XX(OodleLZ_BackwardsCompatible_MajorVersion)

OOFUNC1 void OOFUNC2 Oodle_GetConfigValues(OodleConfigValues * ptr)
{
	//OOFUNCSTART

	#define XX(suf) RR_CAT(ptr->m_,suf) = RR_CAT(g_,suf);
	XX_LIST
	#undef XX

	ptr->m_oodle_header_version = OODLE_HEADER_VERSION;
}

OOFUNC1 void OOFUNC2 Oodle_SetConfigValues(const OodleConfigValues * ptr)
{
	//OOFUNCSTART

	if ( ptr->m_oodle_header_version != OODLE_HEADER_VERSION )
	{
		// often before OodleX_Init :
		RR_ASSERT_LITE(false);
		return;
	}

	#define XX(suf) RR_CAT(g_,suf) = RR_CAT(ptr->m_,suf);
	XX_LIST
	#undef XX
}

OOFUNC1 void OOFUNC2 Oodle_SetUsageWarnings(Oodle_UsageWarnings state)
{
	g_Oodle_UsageWarningsDisabled = (state == Oodle_UsageWarnings_Disabled);
}

//===========================================================

OODLE_NS_END
