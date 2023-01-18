;// Copyright Epic Games, Inc. All Rights Reserved.
;// This source file is licensed solely to users who have
;// accepted a valid Unreal Engine license agreement 
;// (see e.g., https://www.unrealengine.com/eula), and use
;// of this source file is governed by such agreement.

%include "newlz_huff_common.inc"

%define N_DECS_PER_REFILL       (56 / CODELEN_LIMIT)
%define TRIPLE_DECS_PER_REFILL  (3*N_DECS_PER_REFILL)

%ifdef BMI2

    %define FUNCNAME oodle_newLZ_huff_x64_bmi2_kern
    
    %macro SHL64_BY_CL 1 ; args: reg
            shlx            %1, %1, rcx
    %endmacro

    %macro SHR64_BY_CL 1 ; args: reg
            shrx            %1, %1, rcx
    %endmacro

%else

    %define FUNCNAME oodle_newLZ_huff_x64_generic_kern
    
    %macro SHL64_BY_CL 1 ; args: reg
            shl             %1, cl
    %endmacro

    %macro SHR64_BY_CL 1 ; args: reg
            shr             %1, cl
    %endmacro

%endif

        ; extern "C" bool FUNCNAME(KrakenHuffState *state);
leaf_func_with_prologue FUNCNAME
        load_first_arg  rsi ; KrakenHuffState* state
        set_state_reg   KrakenHuffState, rsi
        offset_state_by 112                     ; offset so "tables" isn't outside disp8 range from base

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
        cmp             rcx, TRIPLE_DECS_PER_REFILL ; need at least TRIPLE_DECS_PER_REFILL bytes since fast decoder always writes that many
        jb              .no_bulk

        mov             r8, [STATEP(bitp,0)]    ; in0
        xor             r11, r11                ; bits0
        xor             rax, rax                ; bitc0
        xor             r12, r12                ; bits1
        xor             rbx, rbx                ; bitc1
        xor             r13, r13                ; bits2
        xor             rdx, rdx                ; bitc2

        sub             r9, 8                   ; in1 -= 8
        sub             r14, TRIPLE_DECS_PER_REFILL-1 ; to make the fast decoder stop before we get too close

        ; main decode loop
        ; rax = al = bitc0
        ; rbx = bl = bitc1
        ; rcx = scratch
        ; rdx = dl = bitc2
        ; rsi = (char *)s + 112 
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

        align           16
.bulk_inner:
        ; non-crossing invariant: in0 <= in2 && in2 <= in1
        cmp             r8, r10
        ja              .bulk_done
        cmp             r10, r9
        ja              .bulk_done

        add             rdi, TRIPLE_DECS_PER_REFILL

        ; refill streams
        mov             rbp, [r8]               ; next0 = load(in0)
        movzx           rcx, al                 ; bitc0
        SHL64_BY_CL     rbp                     ; next0 << bitc0
        mov             r15, [r9]               ; load(in1)
        or              r11, rbp                ; bits0 |= next0 << bitc0
        shr             ecx, 3                  ; 7 - bytes_consumed0
        sub             r8, rcx                 ; in0 += bytes_consumed0 - 7
        bswap           r15                     ; next1 = load_be(in1)
        or              al, 56                  ; bitc0 |= 56

        movzx           rcx, bl                 ; bitc1
        SHL64_BY_CL     r15                     ; next1 << bitc1
        mov             rbp, [r10]              ; next2 = load(in2)
        or              r12, r15                ; bits1 |= next1 << bitc1
        shr             ecx, 3                  ; 7 - bytes_consumed1
        add             r9, rcx                 ; in1 -= bytes_consumed1 - 7

        movzx           rcx, dl                 ; bitc2
        SHL64_BY_CL     rbp                     ; next2 << bitc2
        or              r13, rbp                ; bits2 |= next2 << bitc2
        shr             ecx, 3                  ; 7 - bytes_consumed2
        sub             r10, rcx                ; in2 += bytes_consumed2 - 7

        %assign out_offs -TRIPLE_DECS_PER_REFILL

%macro DECONE 2 ; args: bits, bitc
        mov             ecx, HUFF_TABLE_MASK
        and             rcx, %1                 ; peek
        movzx           rcx, word [STATE32(tables,0)+rcx*2] ; cl=len ch=sym
        SHR64_BY_CL     %1                      ; bits >>= len
        sub             %2, cl                  ; bitc -= len
        mov             [rdi+out_offs], ch      ; store sym
        %assign out_offs out_offs+1
%endmacro

        ; first iter has some other update work left to do
        DECONE          r11, al                 ; stream 0

        add             r8, 7                   ; in0 += 7 (compensate -7 from bytes_consumed0)
        or              bl, 56                  ; bitc1 |= 56

        DECONE          r12, bl                 ; stream 1

        sub             r9, 7                   ; in1 -= 7 (compensate +7 from bytes_consumed1)
        or              dl, 56                  ; bitc2 |= 56

        DECONE          r13, dl                 ; stream 2

        add             r10, 7                  ; in2 += 7 (compensate -7 from bytes_consumed2)

%rep N_DECS_PER_REFILL-1
        DECONE          r11, al                 ; stream 0
        DECONE          r12, bl                 ; stream 1
        DECONE          r13, dl                 ; stream 2
%endrep

        ; check whether we're done
        cmp             rdi, r14                ; do {...} while (decodeptr<decodeend)
        jb              .bulk_inner

.bulk_done:
        add             r9, 8                   ; in1 += 8 (undo prep)

        ; transition to careful logic
        mov             [STATEP(decodeptr,0)], rdi

%macro STOREONE 6 ; args: bits32, bitc32, bitc64, inp, dirop, ind
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

