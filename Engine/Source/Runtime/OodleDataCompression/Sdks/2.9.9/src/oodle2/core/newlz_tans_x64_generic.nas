;// Copyright Epic Games, Inc. All Rights Reserved.
;// This source file is licensed solely to users who have
;// accepted a valid Unreal Engine license agreement 
;// (see e.g., https://www.unrealengine.com/eula), and use
;// of this source file is governed by such agreement.

%include "newlz_huff_common.inc"

%define NUM_STREAMS             5
%define NUM_DOUBLE_ROUNDS       1

%define NUM_ROUNDS              (2*NUM_DOUBLE_ROUNDS)
%define DECS_PER_ITER           (NUM_ROUNDS*NUM_STREAMS)

; max offset we read bytes from (relative to base offset at start of loop)
; we do two advances from current offs (consuming exact number of bytes read so far),
; then do a 8B read from there
;
; NB this doesn't guarantee that pointers won't cross, but it does guarantee that no
; out-of-bounds memory reads occur.
%define BYTES_READ_AHEAD_BOUND  ((NUM_DOUBLE_ROUNDS*NUM_STREAMS*TANS_CODELEN_LIMIT + 7)/8 + 8)

%ifdef BMI2

    %define FUNCNAME oodle_newLZ_tans_x64_bmi2_kern
    
    %macro CLZ64 2 ; args: out, in
            lzcnt           %1, %2
    %endmacro

    %macro SHR64_BY_CL 1 ; args: reg
            shrx            %1, %1, rcx
    %endmacro

%else

    %define FUNCNAME oodle_newLZ_tans_x64_generic_kern
    
    %macro CLZ64 2 ; args: out, in
            bsr             %1, %2
            xor             %1, 63
    %endmacro

    %macro SHR64_BY_CL 1 ; args: reg
            shr             %1, cl
    %endmacro

%endif

        ; extern "C" bool FUNCNAME(KrakenTansState *state);
leaf_func_with_prologue FUNCNAME
        load_first_arg  r10
        set_state_reg   KrakenTansState, r10
        offset_state_by 112                     ; disp8 offsets are [-128,127]; maximize useful range from r10

        mov             rdi, [STATEP(decodeptr,0)]
        mov             r14, [STATEP(decodeend,0)]

        ; Check whether the output stream is long enough to run our bulk loop
        mov             rax, r14
        sub             rax, rdi
        cmp             rax, DECS_PER_ITER
        jb              .no_bulk

        mov             r11, [STATEP(bitp,0)]
        mov             r13, [STATEP(bitp,1)]

        ; Now check input stream size
        mov             rax, r13
        sub             rax, r11
        cmp             rax, BYTES_READ_AHEAD_BOUND
        jb              .no_bulk

        sub             r13, BYTES_READ_AHEAD_BOUND

        ; Move decodeend forward so we stop the bulk loop before we run a risk
        ; of writing past the end of the dest buffer
        sub             r14, DECS_PER_ITER-1

        ; Get ready for main loop
        mov             eax, [STATE32(tans_state,0)]
        mov             edx, [STATE32(tans_state,1)]
        mov             ebp, [STATE32(tans_state,2)]
        mov             r8d, [STATE32(tans_state,3)]
        mov             r9d, [STATE32(tans_state,4)]
        mov             r12, (1<<63)                            ; marker bit

        ; Initial bitbuf fill
%macro INITIAL_FILL 5 ; args: bits bitp offs dir ind
        mov             ecx, [STATE32(bitc,%5)]                 ; count in current buf (invariant: <8)
        neg             ecx
        jz              %%was_empty                             ; if 0, nothing to do
        sub             %2, %4                                  ; bitp -= dir (step back to byte containing partial bits)
        add             ecx, 8                                  ; 8 - count = bitpos
%%was_empty:
        mov             %1, [%2+%3]                             ; fetch bytes
%if %4 != 1
        or              %1, 128                                 ; marker
        bswap           %1
%else
        or              %1, r12                                 ; marker
%endif
        SHR64_BY_CL     %1                                      ; consume partial
%endmacro

        INITIAL_FILL    rbx, r11, 0, 1, 0
        INITIAL_FILL    r15, r13, BYTES_READ_AHEAD_BOUND-8, -1, 1

        ; Fetch table ptr
        mov             rsi, [STATEP(table,0)]

        ; main decode loop
        ; rax = state0
        ; rcx = scratch
        ; rdx = state1
        ; rbx = bits0
        ; rbp = state2
        ; rsi = table
        ; rdi = decodeptr
        ; r8  = state3
        ; r9  = state4
        ; r10 = s
        ; r11 = in0
        ; r12 = marker
        ; r13 = in1
        ; r14 = decodeend
        ; r15 = bits1

        jmp             .bulk_inner
        hot_trace_begin 16 ; NOTE(fg): align starting at 16 for Jcc erratum workaround
.bulk_inner:
        ; near end of data?
        ; NOTE(fg): at offset 16 in 32B line
        cmp             r11, r13
        ja              .bulk_done

        ; table layout: { U32 mask; U8 len; U8 sym; U16 next_state; }

%ifndef BMI2

%macro DECONE 4 ; args: stateout32 statein64 bits64 bits32
        mov             ecx, [rsi + %2*8 + 4]                   ; cl=len ch=sym hi16=nextst
        mov             %1, [rsi + %2*8 + 0]                    ; mask
        mov             [rdi + out_offs], ch
        and             %1, %4                                  ; low bits for getbits
        SHR64_BY_CL     %3                                      ; consume bits
        shr             ecx, 16                                 ; extract nextst
        add             %1, ecx
        %assign out_offs out_offs + 1
%endmacro

%else

%macro DECONE 4 ; args: stateout32 statein64 bits64 bits32
        mov             ecx, [rsi + %2*8 + 4]                   ; cl=len ch=sym hi16=nextst
        bzhi            %1, %4, ecx                             ; mask low bits of bitbuf
        mov             [rdi + out_offs], ch
        shrx            %3, %3, rcx                             ; consume bits
        shr             ecx, 16                                 ; extract nextst
        add             %1, ecx
        %assign out_offs out_offs + 1
%endmacro

%endif

%macro ADVANCE_REFILL 3 ; bitbuf bitptr dir
        mov             ecx, 7
        CLZ64           %1, %1                                  ; bits consumed
        and             rcx, %1                                 ; leftover_bits
        shr             %1, 3                                   
%if %3 > 0
        add             %2, %1                                  ; in += bits_consumed >> 3
        mov             %1, [%2]                                ; next
        or              %1, r12                                 ; next | marker
%else
        sub             %2, %1
        mov             %1, [%2+BYTES_READ_AHEAD_BOUND-8]
        or              %1, 128                                 ; marker
        bswap           %1                                      ; next
%endif
        SHR64_BY_CL     %1                                      ; consume partial
%endmacro

        add             rdi, DECS_PER_ITER
        %assign out_offs -DECS_PER_ITER

%rep NUM_DOUBLE_ROUNDS
        DECONE          eax, rax, rbx, ebx
        DECONE          edx, rdx, rbx, ebx
        DECONE          ebp, rbp, rbx, ebx
        DECONE          r8d, r8, rbx, ebx
        DECONE          r9d, r9, rbx, ebx
        ADVANCE_REFILL  rbx, r11, 1

        DECONE          eax, rax, r15, r15d
        DECONE          edx, rdx, r15, r15d
        DECONE          ebp, rbp, r15, r15d
        DECONE          r8d, r8, r15, r15d
        DECONE          r9d, r9, r15, r15d
        ADVANCE_REFILL  r15, r13, -1
%endrep

        ; check whether we're done
        ; NOTE(fg): not near a 32B boundary in BMI2 mode
        cmp             rdi, r14
        jb              .bulk_inner

        hot_trace_end

.bulk_done:
        ; store final decodeptr
        mov             [STATEP(decodeptr,0)], rdi

        ; undo in1 bias
        add             r13, BYTES_READ_AHEAD_BOUND

        ; store final TANS states
        mov             [STATE32(tans_state,0)], eax
        mov             [STATE32(tans_state,1)], edx
        mov             [STATE32(tans_state,2)], ebp
        mov             [STATE32(tans_state,3)], r8d
        mov             [STATE32(tans_state,4)], r9d

        ; translate bit buffer state to bitcount form for careful loop
%macro STOREONE 5 ; args: bitbuf, in, dir, ind, bitbufd
        CLZ64           rcx, %1                 ; leftover bit count
        jz              %%store                 ; bitpos != 0?
        add             %2, %3                  ; update ptr
        neg             ecx                     ; bitc is bits left = 8-bitpos
        add             ecx, 8
%%store:
        mov             [STATE32(bits,%4)], %5  ; store bits
        mov             [STATEP(bitp,%4)], %2   ; store ptr
        mov             [STATE32(bitc,%4)], ecx ; store count
%endmacro
        
        STOREONE        rbx, r11, 1, 0, ebx     ; stream 0+
        STOREONE        r15, r13, -1, 1, r15d   ; stream 1-

.no_bulk:
        mov             eax, 1

.epilogue:
        leaf_func_epilogue_and_ret

