;// Copyright Epic Games, Inc. All Rights Reserved.
;// This source file is licensed solely to users who have
;// accepted a valid Unreal Engine license agreement 
;// (see e.g., https://www.unrealengine.com/eula), and use
;// of this source file is governed by such agreement.

%include "newlz_huff_common.inc"

%define N_DECS_PER_REFILL       (56 / CODELEN_LIMIT)
%define TRIPLE_DECS_PER_REFILL  (3*N_DECS_PER_REFILL)

        ; extern "C" bool oodle_newLZ_huffx86_sse2_kern(KrakenHuffState *state);
leaf_func_with_prologue oodle_newLZ_huffx86_sse2_kern
        load_first_arg  esi ; KrakenHuffState* state
        set_state_reg   KrakenHuffState, esi

        mov             ecx, [STATEP(bitp,1)]   ; in1
        mov             edx, [STATEP(bitp,2)]   ; in2

        mov             eax, ecx
        sub             eax, edx                ; in1-in2
        cmp             eax, 8
        jb              .no_bulk

        mov             edi, [STATEP(decodeptr,0)]
        mov             ebp, [STATEP(decodeend,0)]

        mov             eax, ebp
        sub             eax, edi
        cmp             eax, TRIPLE_DECS_PER_REFILL-1
        jbe             .no_bulk

        mov             ebx, [STATEP(bitp,0)]   ; in0
        pxor            xmm0, xmm0              ; bits0
        pxor            xmm2, xmm2              ; bitc0
        pxor            xmm6, xmm6              ; bits1
        pxor            xmm7, xmm7              ; bitc1
        pxor            xmm1, xmm1              ; bits2
        pxor            xmm3, xmm3              ; bitc2

        sub             ecx, 8                  ; in1 -= 8
        sub             ebp, TRIPLE_DECS_PER_REFILL-1
        offset_state_by 112

        ; main decode loop
        ; eax = scratch
        ; ebx = in0
        ; ecx = in1
        ; edx = in2
        ; esi = s
        ; edi = decodeptr
        ; ebp = decodeend
        ; xmm0 = bits0
        ; xmm1 = bits2
        ; xmm2 = bitc0
        ; xmm3 = bitc2
        ; xmm4 = scratch
        ; xmm5 = scratch during refill / uint64(56) during core decode
        ; xmm6 = bits1
        ; xmm7 = bitc1

        align           16
.bulk_inner:
        ; non-crossing invariant: in0 <= in2 && in2 <= in1
        cmp             ebx, edx
        ja              .bulk_done
        cmp             edx, ecx
        ja              .bulk_done

        ; refill streams
        movq            xmm5, [ebx]             ; next0 = load(in0)
        movq            xmm4, [ecx]             ; in1 load
        psllq           xmm5, xmm2              ; next0 << bitc0
        por             xmm0, xmm5              ; bits0 |= next0 << bitc0
        movd            eax, xmm2
        add             edi, TRIPLE_DECS_PER_REFILL
        pshuflw         xmm4, xmm4, 0x1b        ; reverse words in in1 qword (1st part of BSWAP)
        movdqa          xmm5, xmm4
        xor             eax, 63                 ; 63 - bitc0
        psrlw           xmm4, 8
        psllw           xmm5, 8
        shr             eax, 3                  ; bytes_consumed0
        add             ebx, eax                ; in0 += bytes_consumed0
        por             xmm4, xmm5              ; next1 = load_be(in1) (BSWAP complete)
        movq            xmm5, [edx]             ; next2 = load(in2)
        psllq           xmm4, xmm7              ; next1 << bitc1
        por             xmm6, xmm4              ; bits1 |= next1 << bitc1
        psllq           xmm5, xmm3              ; next2 << bitc2
        por             xmm1, xmm5              ; bits2 |= next2 << bitc2
        mov             eax, 56                 ; constant for refill
        movd            xmm5, eax
        por             xmm2, xmm5              ; bitc0 |= 56

        %assign out_offs -TRIPLE_DECS_PER_REFILL

%macro DECONE 3 ; args: bits, bitc, scratch
        movd            eax, %1                 ; bits
        and             eax, HUFF_TABLE_MASK    ; peek
        movd            %3, [STATE32(lentab,0) + eax*4] ; len
        movzx           eax, byte [STATE32(symtab,0) + eax] ; sym
        psrlq           %1, %3                  ; bits >>= len
        psubd           %2, %3                  ; bitc -= len
        mov             [edi+out_offs], al      ; store sym
        %assign out_offs out_offs+1
%endmacro

        ; decode the bytes
        DECONE          xmm0, xmm2, xmm4        ; stream 0

        ; update stream 1 ptr
        movd            eax, xmm7
        shr             eax, 3                  ; 7 - bytes_consumed1
        por             xmm7, xmm5              ; bits1 |= 56
        lea             ecx, [ecx+eax-7]        ; in1 -= bytes_consumed1

        DECONE          xmm6, xmm7, xmm4        ; stream 1

        ; update stream 2 ptr
        movd            eax, xmm3
        por             xmm3, xmm5              ; bitc2 |= 56
        xor             eax, 63                 ; 63 - bitc2
        shr             eax, 3                  ; bytes_consumed2
        add             edx, eax                ; in2 += bytes_consumed2

        DECONE          xmm1, xmm3, xmm4        ; stream 2

%if N_DECS_PER_REFILL < 2
%error N_DECS_PER_REFILL < 2
%endif

%rep N_DECS_PER_REFILL-1
        DECONE          xmm0, xmm2, xmm4        ; stream 0
        DECONE          xmm6, xmm7, xmm4        ; stream 1
        DECONE          xmm1, xmm3, xmm4        ; stream 2
%endrep

%undef out_offs

        ; check whether we're done
        cmp             edi, ebp                ; do {...} while (decodeptr<decodeend)
        jb              .bulk_inner

.bulk_done:
        add             ecx, 8                  ; in1 += 8 (undo prep)

        ; transition to careful logic
        mov             [STATEP(decodeptr,0)], edi

%macro STOREONE 5 ; args: bits, bitc, inp, dirop, ind
        mov             edi, 7
        movd            eax, %2                 ; c=bitc
        and             edi, eax                ; c & 7
        shr             eax, 3                  ; c >> 3
        %4              %3, eax                 ; in +- (c >> 3)
        mov             [STATEP(bitp,%5)], %3
        mov             [STATE32(bitc,%5)], edi
        movd            [STATE32(bits,%5)], %1
%endmacro

        STOREONE        xmm0, xmm2, ebx, sub, 0 ; stream 0
        STOREONE        xmm6, xmm7, ecx, add, 1 ; stream 1
        STOREONE        xmm1, xmm3, edx, sub, 2 ; stream 2

.no_bulk:
        mov             eax, 1

        leaf_func_epilogue_and_ret

