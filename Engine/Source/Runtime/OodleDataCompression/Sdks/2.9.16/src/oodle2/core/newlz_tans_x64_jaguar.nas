;// Copyright Epic Games, Inc. All Rights Reserved.
;// This source file is licensed solely to users who have
;// accepted a valid Unreal Engine license agreement 
;// (see e.g., https://www.unrealengine.com/eula), and use
;// of this source file is governed by such agreement.

%include "newlz_huff_common.inc"

%define NUM_STREAMS             5
%define NUM_DOUBLE_ROUNDS       3 ; can't actually change this easily!

%define NUM_ROUNDS              (2*NUM_DOUBLE_ROUNDS)
%define DECS_PER_ITER           (NUM_ROUNDS*NUM_STREAMS)
%define DEC_SAFETY_MARGIN       (15 + 16) ; write 16 bytes, advance ptr by 15, write 16 bytes

; max offset we read bytes from (relative to base offset at start of loop)
; we do two advances from current offs (consuming exact number of bytes read so far),
; then do a 8B read from there
;
; NB this doesn't guarantee that pointers won't cross, but it does guarantee that no
; out-of-bounds memory reads occur.
%define BYTES_READ_AHEAD_BOUND  ((NUM_DOUBLE_ROUNDS*NUM_STREAMS*TANS_CODELEN_LIMIT + 7)/8 + 8)

        ; extern "C" bool oodle_newLZ_tans_x64_jaguar_kern(KrakenTansState *state);
leaf_func_with_prologue oodle_newLZ_tans_x64_jaguar_kern
        load_first_arg  rsi
        set_state_reg   KrakenTansState, rsi
        offset_state_by 112                     ; disp8 offsets are [-128,127]; maximize useful range from rsi

        mov             rdi, [STATEP(decodeptr,0)]

        ; Check whether the output stream is long enough to run our bulk loop
        mov             rax, [STATEP(decodeend,0)]
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

        ; Bias in1 pointer - we want to detect when there's a risk of us reading outside
        ; [in0,in1) during the iteration. (Which might be outside the buffer)
        sub             r13, BYTES_READ_AHEAD_BOUND

        ; Get ready for main loop
        mov             eax, [STATE32(tans_state,0)]
        mov             edx, [STATE32(tans_state,1)]
        mov             ebp, [STATE32(tans_state,2)]
        mov             r8d, [STATE32(tans_state,3)]
        mov             r9d, [STATE32(tans_state,4)]
        xor             ecx, ecx                                ; bits=empty
        mov             r10d, 0x300                             ; bextr mask for leftover_bits
        mov             r12, (1<<63)                            ; marker bit
        pxor            xmm0, xmm0

        ; Initial bitbuf fill
%macro INITIAL_FILL 5 ; args: bits bitp offs dir ind
        mov             ecx, [STATE32(bitc,%5)]                 ; count in current buf (invariant: <8)
        neg             ecx
        jz              %%was_empty                             ; if 0, nothing to do
        sub             %2, %4                                  ; bitp -= dir (step back to byte containing partial bits)
        add             ecx, 8                                  ; 8 - count = bitpos
%%was_empty:
%if %4 == 1
        mov             %1, [%2+%3]                             ; fetch bytes
%else
        movbe           %1, [%2+%3]                             ; fetch bytes
%endif
        or              %1, r12                                 ; marker
        shr             %1, cl                                  ; consume partial
%endmacro

        INITIAL_FILL    rbx, r11, 0, 1, 0
        INITIAL_FILL    r15, r13, BYTES_READ_AHEAD_BOUND-8, -1, 1

        ; Now move rsi to point at our table; leave original value of rsi on stack.
        ; Also compute bulk decodeend - we stop early so we don't run a risk of
        ; writing past the end of the output buffer - and store it on stack.
        push            rsi
        mov             rcx, [STATEP(decodeend,0)]
        sub             rcx, DEC_SAFETY_MARGIN
        push            rcx

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
        ; r10 = bextr extract const
        ; r11 = in0
        ; r12 = marker
        ; r13 = in1
        ; r14 = state5 (temp during decode)
        ; r15 = bits1
        ; xmm0 = output buf

        align           16
.bulk_inner:
        ; near end of data?
        cmp             r11, r13
        ja              .bulk_done

        ; table layout: { U32 mask; U8 len; U8 sym; U16 next_state; }
        ; NOTE: we use that the high byte of mask is always 0!

%macro DECONE 4 ; args: stateout32 statein64 bits64 bits32
        %ifidni %1,eax
                %define out16 ax
        %elifidni %1,edx
                %define out16 dx
        %elifidni %1,ebp
                %define out16 bp
        %elifidni %1,r8d
                %define out16 r8w
        %elifidni %1,r9d
                %define out16 r9w
        %elifidni %1,r14d
                %define out16 r14w
        %else
                %fatal %1 is not a supported destination
        %endif
        movzx           ecx, word [rsi + %2*8 + 3]              ; bextr_ctrl=len<<8
        pinsrb          xmm0, [rsi + %2*8 + 5], insert_offs     ; sym
        bextr           %1, %4, ecx                             ; state' = bits & ((1<<len)-1)
        shr             ecx, 8                                  ; move len to cl
        add             out16, [rsi + %2*8 + 6]                 ; state' += next_state_base
        shr             %3, cl                                  ; bits >>= len
		%undef out16
        %assign insert_offs insert_offs + 1
%endmacro

%macro FLUSH_OUTPUT 0
        movdqu          [rdi+out_offs], xmm0
        %assign out_offs out_offs + insert_offs
        %assign insert_offs 0
%endmacro

%macro ADVANCE_REFILL 3 ; bitbuf bitptr dir
        lzcnt           %1, %1                                  ; bits_consumed
        bextr           rcx, %1, r10                            ; leftover_bits (=bits_consumed & 7)
        shr             %1, 3                                   
%if %3 > 0
        add             %2, %1                                  ; in += bits_consumed >> 3
        mov             %1, [%2]                                ; next
%else
        sub             %2, %1
        movbe           %1, [%2+BYTES_READ_AHEAD_BOUND-8]       ; next
%endif
        or              %1, r12                                 ; next | marker
        shr             %1, cl                                  ; consume partial
%endmacro

        add             rdi, DECS_PER_ITER
        %assign insert_offs 0
        %assign out_offs -DECS_PER_ITER

        ; first round
        DECONE          r14d, rax, rbx, ebx     ; state5 <- state0
        DECONE          eax, rdx, rbx, ebx      ; state0 <- state1
        DECONE          edx, rbp, rbx, ebx      ; state1 <- state2
        DECONE          ebp, r8, rbx, ebx       ; state2 <- state3
        DECONE          r8d, r9, rbx, ebx       ; state3 <- state4
        ADVANCE_REFILL  rbx, r11, 1

        ; second round
        DECONE          r9d, r14, r15, r15d     ; state4 <- state0
        DECONE          r14d, rax, r15, r15d    ; state5 <- state0
        DECONE          eax, rdx, r15, r15d     ; state0 <- state1
        DECONE          edx, rbp, r15, r15d     ; state1 <- state2
        DECONE          ebp, r8, r15, r15d      ; state2 <- state3
        ADVANCE_REFILL  r15, r13, -1

        ; third round
        DECONE          r8d, r9, rbx, ebx       ; state3 <- state4
        DECONE          r9d, r14, rbx, ebx      ; state4 <- state0
        DECONE          r14d, rax, rbx, ebx     ; state5 <- state0
        DECONE          eax, rdx, rbx, ebx      ; state0 <- state1
        DECONE          edx, rbp, rbx, ebx      ; state1 <- state2
        ADVANCE_REFILL  rbx, r11, 1
        FLUSH_OUTPUT

        ; fourth round
        DECONE          ebp, r8, r15, r15d      ; state2 <- state3
        DECONE          r8d, r9, r15, r15d      ; state3 <- state4
        DECONE          r9d, r14, r15, r15d     ; state4 <- state0
        DECONE          r14d, rax, r15, r15d    ; state5 <- state0
        DECONE          eax, rdx, r15, r15d     ; state0 <- state1
        ADVANCE_REFILL  r15, r13, -1

        ; fifth round
        DECONE          edx, rbp, rbx, ebx      ; state1 <- state2
        DECONE          ebp, r8, rbx, ebx       ; state2 <- state3
        DECONE          r8d, r9, rbx, ebx       ; state3 <- state4
        DECONE          r9d, r14, rbx, ebx      ; state4 <- state0
        DECONE          r14d, rax, rbx, ebx     ; state5 <- state0
        ADVANCE_REFILL  rbx, r11, 1

        ; sixth round
        DECONE          eax, rdx, r15, r15d     ; state0 <- state1
        DECONE          edx, rbp, r15, r15d     ; state1 <- state2
        DECONE          ebp, r8, r15, r15d      ; state2 <- state3
        DECONE          r8d, r9, r15, r15d      ; state3 <- state4
        DECONE          r9d, r14, r15, r15d     ; state4 <- state0
        ADVANCE_REFILL  r15, r13, -1
        FLUSH_OUTPUT

        ; check whether we're done
        cmp             rdi, [rsp]              ; copy of decodeend on stack
        jb              .bulk_inner

.bulk_done:
        ; pop copy of decodeend and then original rsi
        pop             rcx                     ; munged decodeend isn't needed anymore
        pop             rsi

        ; store final decodeptr
        mov             [STATEP(decodeptr,0)], rdi

        ; undo in1 ptr bias
        add             r13, BYTES_READ_AHEAD_BOUND

        ; store final TANS states
        mov             [STATE32(tans_state,0)], eax
        mov             [STATE32(tans_state,1)], edx
        mov             [STATE32(tans_state,2)], ebp
        mov             [STATE32(tans_state,3)], r8d
        mov             [STATE32(tans_state,4)], r9d

        ; translate bit buffer state to bitcount from for careful loop
        ; rcx contains leftover_bits from last advance
%macro STOREONE 5 ; args: bitbuf, in, dir, ind, bitbufb
        lzcnt           rcx, %1                 ; leftover bit count
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

