;// Copyright Epic Games, Inc. All Rights Reserved.
;// This source file is licensed solely to users who have
;// accepted a valid Unreal Engine license agreement 
;// (see e.g., https://www.unrealengine.com/eula), and use
;// of this source file is governed by such agreement.

; Kraken/NewLZ Huffman common NASM include file

%include "asmlib_x86_x64.inc"

; This must match NewLZ parameters exactly!
%define CODELEN_LIMIT           11

%define HUFF_TABLE_SIZE         (1 << CODELEN_LIMIT)
%define HUFF_TABLE_MASK         (HUFF_TABLE_SIZE - 1)

; This must match the C++ KrakenHuffState exactly!
struc KrakenHuffState
        .decodeptr      resp 2 ; U8 *decodeptr[2]
        .decodeend      resp 2 ; U8 *decodeend[2]
        .strm0_end      resp 2 ; const U8 *strm0_end[2]
        .bitp           resp 6 ; const U8 *bitp[6]
        .bits           resd 6 ; U32 bits[6]
        .bitc           resd 6 ; U32 bitc[6]
        .tables:        ; alternate label
        .lentab         resd HUFF_TABLE_SIZE
        .symtab         resb HUFF_TABLE_SIZE
endstruc

%define TANS_CODELEN_LIMIT      11
%define TANS_TABLE_SIZE         (1 << TANS_CODELEN_LIMIT)

; This must match C++ KrakenTansState exactly!
struc KrakenTansState
        .table          resp 1 ; TableEntry *table (format varies)
        .decodeptr      resp 1 ; U8 *decodeptr
        .decodeend      resp 1 ; U8 *decodeend
        .bitp           resp 2 ; const U8 *bitp[2]
        .bits           resd 2 ; U32 bits[2]
        .bitc           resd 2 ; U32 bitc[2]
        .tans_state     resd 5 ; U32 tans_state[5]
endstruc

; Accessors for KHS fields that assume the state is kept in some register
; which may in turn be offset by some constant.

; Address of a 32-bit and pointer-sized array element, respectively.
%define STATE32(name,ind) (STATE_BASE - STATE_OFFS + STATE_TYPE %+ . %+ name + (ind)*4)
%define STATE64(name,ind) (STATE_BASE - STATE_OFFS + STATE_TYPE %+ . %+ name + (ind)*8)
%define STATEP(name,ind)  (STATE_BASE - STATE_OFFS + STATE_TYPE %+ . %+ name + (ind)*PTR_SIZE)

; Set state base
%macro set_state_reg 2 ; args: type, reg
        %define STATE_TYPE %1
        %define STATE_BASE %2
        %assign STATE_OFFS 0
%endmacro

; Rebase STATE_BASE by adding <amount> to it; keep track of the cumulative
; offset to keep STATE32 and STATEP working.
%macro offset_state_by 1 ; args: amount
        add             STATE_BASE, (%1)
        %assign         STATE_OFFS (STATE_OFFS + (%1))
%endmacro

