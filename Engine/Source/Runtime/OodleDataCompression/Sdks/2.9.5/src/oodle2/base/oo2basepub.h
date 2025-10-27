// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

/***

oo2basepub.h : NOT included anywhere
only used for publicating

for things you want in the public oodle2base.h
but NOT internally

****/

PUBPUSH
PUBPRI(-10040)
#if 0
PUBTYPESTART

//===================================================
// Oodle2 Base header
// (C) Copyright 1994-2021 Epic Games Tools LLC
//===================================================

#ifndef __OODLE2BASE_H_INCLUDED__
#define __OODLE2BASE_H_INCLUDED__

#ifndef OODLE2BASE_PUBLIC_HEADER
#define OODLE2BASE_PUBLIC_HEADER 1
#endif

#ifdef _MSC_VER
#pragma pack(push, Oodle, 8)

#pragma warning(push)
#pragma warning(disable : 4127) // conditional is constant
#endif
PUBTYPEEND
#endif
#endif
PUBINCLUDE("v:\devel\projects\oodle2\base\oodlebasetypes.h")
PUBPRI(-10030)
PUBTYPESTART
// Oodle2 base header

PUBTYPEEND
PUBPOP

PUBPUSH
PUBPRI(1999)
#if 0
PUBSTART
#ifdef _MSC_VER
#pragma warning(pop)
#pragma pack(pop, Oodle)
#endif

#endif // __OODLE2BASE_H_INCLUDED__
PUBEND
PUBPOP


#if 0 // does not affect includers of OodleCore.h
// pub :
PUBPRI(-10010)
PUBSTART

	// Check build flags
	#if defined(OODLE_BUILDING_LIB) || defined(OODLE_BUILDING_DLL)
        #error Should not see OODLE_BUILDING set for users of oodle.h
	#endif

PUBEND
#endif
