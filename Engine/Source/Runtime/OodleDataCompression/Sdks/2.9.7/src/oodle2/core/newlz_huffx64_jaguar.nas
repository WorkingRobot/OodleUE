;// Copyright Epic Games, Inc. All Rights Reserved.
;// This source file is licensed solely to users who have
;// accepted a valid Unreal Engine license agreement 
;// (see e.g., https://www.unrealengine.com/eula), and use
;// of this source file is governed by such agreement.

%include "newlz_huff_common.inc"

%define BITBUF_BEXTR_INIT       (0 | (CODELEN_LIMIT<<8))
%define N_DECS_PER_REFILL       (56 / CODELEN_LIMIT)
%define TRIPLE_DECS_PER_REFILL  (3*N_DECS_PER_REFILL)
%define BYTES_WRITTEN_PER_ITER  (16) ; how many bytes we touch with stores per iter (not how many bytes are actually decoded, that's TRIPLE_DECS_PER_REFILL)

        ; extern "C" bool oodle_newLZ_huffx64_jaguar_kern(KrakenHuffState *state);
leaf_func_with_prologue oodle_newLZ_huffx64_jaguar_kern
        load_first_arg  rsi ; KrakenHuffState* state
        set_state_reg   KrakenHuffState, rsi

        mov             r9, [STATEP(bitp,1)]    ; in1
        mov             r10, [STATEP(bitp,2)]   ; in2

        mov             rax, r9
        sub             rax, r10                ; in1-in2
        cmp             rax, 8
        jb              .no_bulk

        ; decode loop setup
        mov             rdi, [STATEP(decodeptr,0)]
        mov             r14, [STATEP(decodeend,0)]

        sub             r14, BYTES_WRITTEN_PER_ITER-1 ; so we stop short of stomping past the end of the buffer
        sub             rdi, r14                ; -bytes_left_to_decode
        jns             .no_bulk

        ; main decode loop
        ; rax = scratch
        ; rbx = bitextr0
        ; rcx = bitextr1
        ; rdx = bitextr2
        ; rbp = bextr const
        ; rsi = &s->tables
        ; rdi = -bytes_left_to_decode
        ; r8  = in0
        ; r9  = in1
        ; r10 = in2
        ; r11 = bits0 (only live in inner loop)
        ; r12 = bits1 (only live in inner loop)
        ; r13 = bits2 (only live in inner loop)
        ; r14 = decodeend
        ; r15 = <unused>

        mov             r8,  [STATEP(bitp,0)]   ; in0
        sub             r9, 8                   ; in1 -= 8
        mov             ebx, BITBUF_BEXTR_INIT  ; bitextr0
        mov             ecx, BITBUF_BEXTR_INIT  ; bitextr1
        mov             edx, BITBUF_BEXTR_INIT  ; bitextr2
        mov             ebp, 0x303              ; bextr for bytes step
        offset_state_by KrakenHuffState.tables              ; rsi -> &s->tables

        align           16
.bulk_inner:
        ; non-crossing invariant: in0 <= in2 && in2 <= in1
        cmp             r8, r10
        ja              .bulk_done
        cmp             r10, r9
        ja              .bulk_done

        ; refill stream 0
        mov             r11, [r8]               ; read next bits0
        and             ebx, ~0x38              ; keep bit offset within byte

        ; refill stream 1
        movbe           r12, [r9]               ; read next bits1
        and             rcx, ~0x38              ; keep bit offset within byte

        ; refill stream 2
        mov             r13, [r10]              ; read next bits2
        and             rdx, ~0x38              ; keep bit offset within byte

        %assign i 0
%rep N_DECS_PER_REFILL
        ; stream 0
        bextr           rax, r11, rbx           ; peek
        add             bl, [rsi+rax*2] ; add len
        vpinsrb         xmm0, xmm0, [rsi+rax*2+1], i+0 ; insert sym

        ; stream 1
        bextr           rax, r12, rcx           ; peek
        add             cl, [rsi+rax*2] ; add len
        vpinsrb         xmm0, xmm0, [rsi+rax*2+1], i+1 ; insert sym

        ; stream 2
        bextr           rax, r13, rdx           ; peek
        add             dl, [rsi+rax*2] ; add len
        vpinsrb         xmm0, xmm0, [rsi+rax*2+1], i+2 ; insert sym

        %assign i i+3
%endrep
        %undef i

        ; final advances
        bextr           rax, rbx, rbp           ; num_bytes_step0
        add             r8, rax                 ; in0 += num_bytes_step0
        bextr           rax, rcx, rbp           ; num_bytes_step1
        sub             r9, rax                 ; in1 -= num_bytes_step1
        bextr           rax, rdx, rbp           ; num_bytes_step2
        add             r10, rax                ; in2 += num_bytes_step2

        vmovdqu         [rdi+r14], xmm0
        add             rdi, TRIPLE_DECS_PER_REFILL
        js              .bulk_inner             ; loop while bytes_to_decode > 0

.bulk_done:
        add             r9, 8                   ; in1 += 8 (undo prep)
        add             rdi, r14

        ; validate pointers
        cmp             r8, r10                 ; we need in0 <= in2 && in2 <= in1
        ja              .bad_stream
        cmp             r10, r9
        jb              .transition             ; if in2 < in1, it's fine
        jne             .bad_stream             ; if in2 > in1, it's definitely bad

        ; in1 == in2; i.e. 0 bytes left in either stream.
        ; this can work, but only if both in1 and in2 have (bitpos & 7) == 0
        ; (i.e. fully consumed their respective bytes)
        test            rcx, 7
        jnz             .bad_stream             ; (bitpos1 & 7) != 0 -> bad
        test            rdx, 7
        jnz             .bad_stream             ; (bitpos2 & 7) != 0 -> bad

.transition:
        ; transition to bitcount form for careful loop
        mov             [STATEP(decodeptr,0)], rdi

%macro STOREONE 4 ; inp, inoffs, dirop, ind
        and             ecx, 7                  ; numbits &= 7
        jz              %%store                 ; assumes bits[ind] is zero
        movzx           eax, byte [%1+%2]       ; fetch next byte
        %3              %1                      ; update ptr
        shr             eax, cl                 ; align it to shift
        neg             ecx                     ; bitc is bits left =8-bitpos
        add             ecx, 8
        mov             [STATE32(bits,%4)], eax ; store leftover bits
%%store:
        mov             [STATEP(bitp,%4)], %1
        mov             [STATE32(bitc,%4)], ecx
%endmacro

        ; store stream 1 first (freeing up rcx)
        STOREONE        r9, -1, dec, 1

        ; stream 0
        mov             rcx, rbx
        STOREONE        r8, 0, inc, 0

        ; stream 2
        mov             rcx, rdx
        STOREONE        r10, 0, inc, 2

.no_bulk:
        mov             eax, 1
        jmp             .epilogue

.bad_stream:
        xor             eax, eax

.epilogue:
        leaf_func_epilogue_and_ret

