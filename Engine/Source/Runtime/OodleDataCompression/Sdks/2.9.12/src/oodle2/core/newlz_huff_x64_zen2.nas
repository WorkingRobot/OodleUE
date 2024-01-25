;// Copyright Epic Games, Inc. All Rights Reserved.
;// This source file is licensed solely to users who have
;// accepted a valid Unreal Engine license agreement 
;// (see e.g., https://www.unrealengine.com/eula), and use
;// of this source file is governed by such agreement.

%include "newlz_huff_common.inc"

%define N_DECS_PER_REFILL       (55 / CODELEN_LIMIT) ; 55 because we start reading at bit 1, see below
%define TRIPLE_DECS_PER_REFILL  (3*N_DECS_PER_REFILL)
%define BYTES_WRITTEN_PER_ITER  (16) ; how many bytes we touch with stores per iter (not how many bytes are actually decoded, that's TRIPLE_DECS_PER_REFILL)

        ; extern "C" bool oodle_newLZ_huff_x64_zen2_kern(KrakenHuffState *state);
leaf_func_with_prologue oodle_newLZ_huff_x64_zen2_kern
        load_first_arg  rsi ; KrakenHuffState* state
        set_state_reg   KrakenHuffState, rsi
        offset_state_by KrakenHuffState.tables ; so our len loads are offset 0 (i.e. pure base+index) which is lower-latency on Zen

        mov             r9, [STATEP(bitp,1)]    ; in1
        mov             r10, [STATEP(bitp,2)]   ; in2

        mov             rcx, r9
        sub             rcx, r10                ; in1-in2
        cmp             rcx, 8
        jb              .no_bulk

        mov             rdi, [STATEP(decodeptr,0)]
        mov             r14, [STATEP(decodeend,0)]

        mov             rcx, r14
        sub             rcx, rdi
        cmp             rcx, BYTES_WRITTEN_PER_ITER
        jb              .no_bulk

        ; Initialize bitc at 1 not 0, because we actually grab bits starting from bit 1 not bit 0,
        ; saving us the *2 on the table addressing. This cuts 1 cycle off the table access latency,
        ; and the huff3 decode loop is latency-bound.
        ;
        ; This does mean that bitc contains not the usable number of bits but instead the usable
        ; number of bits minus 1. It also means we effectively lose one bit of the bit buffer.
        ; Luckily, we need 55 not 56 bits (for 5*11 bits of Huff codes) so this _just_ works out!

        mov             r8, [STATEP(bitp,0)]    ; in0
        xor             r11, r11                ; bits0
        mov             eax, 1                  ; bitc0
        xor             r12, r12                ; bits1
        mov             ebx, 1                  ; bitc1
        xor             r13, r13                ; bits2
        mov             edx, 1                  ; bitc2

        sub             r9, 8                   ; in1 -= 8
        sub             r14, BYTES_WRITTEN_PER_ITER-1 ; to make the fast decoder stop before we get too close
        vpxor           xmm0, xmm0, xmm0

        ; main decode loop
        ; rax = bitc0
        ; rbx = bitc1
        ; rcx = scratch
        ; rdx = bitc2
        ; rsi = (char *)s + offsetof(KrakenHuffState,tables)
        ; rdi = decodeptr
        ; rbp = scratch
        ; r8  = in0
        ; r9  = in1
        ; r10 = in2
        ; r11 = bits0
        ; r12 = bits1
        ; r13 = bits2
        ; r14 = decodeend
        ; r15 = scratch
        ; xmm0 = output buf

        align           16
.bulk_inner:
        ; non-crossing invariant: in0 <= in2 && in2 <= in1
        cmp             r8, r10
        ja              .bulk_done
        cmp             r10, r9
        ja              .bulk_done

        add             rdi, TRIPLE_DECS_PER_REFILL
        mov             r15d, 0x303

        ; refill streams
        shlx            rbp, [r8], rax          ; next0 << bitc0
        bextr           ecx, eax, r15d          ; 7 - bytes_consumed0
        or              r11, rbp                ; bits0 |= next0 << bitc0
        sub             r8, rcx                 ; in0 += bytes_consumed0 - 7
        or              eax, 56                 ; bitc0 |= 56

        movbe           rbp, [r9]               ; next1 = load_be(in1)
        shlx            rbp, rbp, rbx           ; next1 << bitc1
        bextr           ecx, ebx, r15d          ; 7 - bytes_consumed1
        or              r12, rbp                ; bits1 |= next1 << bitc1
        add             r9, rcx                 ; in1 -= bytes_consumed1 - 7
        or              ebx, 56                 ; bitc1 |= 56

        shlx            rbp, [r10], rdx         ; next2 << bitc2
        bextr           ecx, edx, r15d          ; 7 - bytes_consumed2
        or              r13, rbp                ; bits2 |= next2 << bitc2
        sub             r10, rcx                ; in2 += bytes_consumed2 - 7
        or              edx, 56                 ; bitc2 |= 56

        %assign out_offs 0

        mov             r15, ~(HUFF_TABLE_MASK << 1)

%macro DECONE 2 ; args: bits, bitc
        andn            rcx, r15, %1            ; peek
        vpinsrb         xmm0, xmm0, [STATE32(tables,0)+rcx+1], out_offs ; insert sym
        movzx           rcx, byte [STATE32(tables,0)+rcx] ; len
        shrx            %1, %1, rcx             ; bits >>= len
        sub             %2, ecx                 ; bitc -= len
        %assign out_offs out_offs+1
%endmacro

        ; first iter has some other update work left to do
        DECONE          r11, eax                ; stream 0

        add             r8, 7                   ; in0 += 7 (compensate -7 from bytes_consumed0)

        DECONE          r12, ebx                ; stream 1

        sub             r9, 7                   ; in1 -= 7 (compensate +7 from bytes_consumed1)

        DECONE          r13, edx                ; stream 2

        add             r10, 7                  ; in2 += 7 (compensate -7 from bytes_consumed2)

%rep N_DECS_PER_REFILL-1
        DECONE          r11, eax                ; stream 0
        DECONE          r12, ebx                ; stream 1
        DECONE          r13, edx                ; stream 2
%endrep

        ; Emit output bytes
        vmovdqu         [rdi - TRIPLE_DECS_PER_REFILL], xmm0

        ; check whether we're done
        cmp             rdi, r14                ; do {...} while (decodeptr<decodeend)
        jb              .bulk_inner

.bulk_done:
        add             r9, 8                   ; in1 += 8 (undo prep)

        ; transition to careful logic
        mov             [STATEP(decodeptr,0)], rdi

%macro STOREONE 6 ; args: bits32, bitc32, bitc64, inp, dirop, ind
        ; Shift out the garbage bit below the bits we read,
        ; and update the bit count accordingly.
        shr             %1, 1
        sub             %2, 1

        mov             ecx, 7
        and             ecx, %2                 ; bitc & 7
        shr             %2, 3                   ; bitc >> 3
        %5              %4, %3                  ; in +- (bitc >> 3)
        mov             [STATEP(bitp,%6)], %4
        mov             [STATE32(bitc,%6)], ecx
        mov             [STATE32(bits,%6)], %1
%endmacro

        STOREONE        r11d, eax, rax, r8, sub, 0   ; stream 0
        STOREONE        r12d, ebx, rbx, r9, add, 1   ; stream 1
        STOREONE        r13d, edx, rdx, r10, sub, 2  ; stream 2

.no_bulk:
        mov             eax, 1

        leaf_func_epilogue_and_ret

