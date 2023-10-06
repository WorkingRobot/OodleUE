;// Copyright Epic Games, Inc. All Rights Reserved.
;// This source file is licensed solely to users who have
;// accepted a valid Unreal Engine license agreement 
;// (see e.g., https://www.unrealengine.com/eula), and use
;// of this source file is governed by such agreement.

%include "newlz_huff_common.inc"

%define N_DECS_PER_REFILL       (56 / CODELEN_LIMIT)
%define TRIPLE_DECS_PER_REFILL  (3*N_DECS_PER_REFILL)
%define BYTES_WRITTEN_PER_ITER  (16) ; how many bytes we touch with stores per iter (not how many bytes are actually decoded, that's TRIPLE_DECS_PER_REFILL)

        ; extern "C" bool oodle_newLZ_huff6_x64_zen2_kern(KrakenHuffState *state);
leaf_func_with_prologue oodle_newLZ_huff6_x64_zen2_kern

        load_first_arg  rsi ; KrakenHuffState* state
        set_state_reg   KrakenHuffState, rsi
        offset_state_by 112                     ; disp8 offsets are [-128,127]; maximize useful range from rsi

        ; Stream layout: doubled-up version of regular 3-stream layout, even if
        ; the resulting numbering is a bit strange. Two triples:
        ;   strm0-> | strm2-> | <-strm1    write to decodeptr[0]
        ;   strm3-> | strm5-> | <-strm4    write to decodeptr[1]

        ; Extra invariant:
        ;   decodeend[1] - decodeptr[1] <= decodeend[0] - decodeptr[0]
        ; i.e. the second triple of streams doesn't encode more bytes than the first.
        ;
        ; This is implicitly guaranteed by the way the stream partitioning works:
        ; the first half gets (numbytes + 1)/2 bytes (i.e. it rounds up), the second
        ; half gets the remainder.

        ; Check whether the short stream triple (streams 3-5, decodeptr 1) is
        ; large enough to run our bulk loop
        mov             rax, [STATEP(decodeend,1)]
        sub             rax, [STATEP(decodeptr,1)]
        cmp             rax, BYTES_WRITTEN_PER_ITER ; need at least BYTES_WRITTEN_PER_ITER bytes since our loop writes that many
        jb              .no_bulk

%macro INITTRIPLE 4 ; args: in0 in1 in2 baseind
        mov             %1, [STATEP(bitp,%4+0)] ; in0
        mov             %2, [STATEP(bitp,%4+1)] ; in1
        mov             %3, [STATEP(bitp,%4+2)] ; in2
        mov             rcx, %2
        sub             rcx, %3                 ; in1-in2
        cmp             rcx, 8                  ; need at least 8 bytes between (in2,in1) pair
        jb              .no_bulk
        sub             %2, 8                   ; in1 -= 8 (reverse stream setup)
%endmacro

        INITTRIPLE      rax,rbx,rdx, 0          ; streams 0-2
        INITTRIPLE      rbp,r8, r9,  3          ; streams 3-5

        ; Move decodeend1 forward so we stop the bulk loop before we run a risk
        ; of writing past the end of the dest buffer
        sub             qword [STATEP(decodeend,1)], BYTES_WRITTEN_PER_ITER-1

        ; Get ready for main loop
        xor             r10, r10                ; bits0=empty
        xor             r11, r11                ; bits1=empty
        xor             r12, r12                ; bits2=empty
        xor             r13, r13                ; bits3=empty
        xor             r14, r14                ; bits4=empty
        xor             r15, r15                ; bits5=empty

        vmovq           xmm0, [STATEP(decodeptr,0)]
        vmovq           xmm1, [STATEP(decodeptr,1)]
        mov             ecx, TRIPLE_DECS_PER_REFILL
        vmovq           xmm2, rcx
        vpxor           xmm3, xmm3, xmm3
        vpxor           xmm4, xmm4, xmm4

        ; main decode loop
        ; rax = in0
        ; rbx = in1
        ; rcx = scratch
        ; rdx = in2
        ; rbp = in3
        ; rsi = &s->tables
        ; rdi = scratch 2
        ; r8  = in4
        ; r9  = in5
        ; r10 = bits0
        ; r11 = bits1
        ; r12 = bits2
        ; r13 = bits3
        ; r14 = bits4
        ; r15 = bits5
        ; xmm0 = decodeptr0 backing store
        ; xmm1 = decodeptr1 backing store
        ; xmm2 = decodeptr advance
        ; xmm3 = output buf 0
        ; xmm4 = output buf 1

        align           16
.bulk_inner:
        mov             rdi, (1<<63)            ; marker bit

%macro CHECKNOCROSS 4 ; args: in0 in1 in2 bail
        ; non-crossing invariant: in0 <= in2 && in2 <= in1
        cmp             %1, %3
        ja              %4
        cmp             %3, %2
        ja              %4
%endmacro

%macro REFILLTRIPLE 6 ; args: bits0 in0 bits1 in1 bits2 in2
        mov             rcx, rdi                ; marker
        or              rcx, [%2]               ; next0 | marker
        shrx            %1, rcx, %1             ; consume partial0

        movbe           rcx, [%4]               ; next1
        or              rcx, rdi                ; next1 | marker
        shrx            %3, rcx, %3             ; consume partial1
        prefetcht0      [%4 - 128]              ; prefetch backwards-moving stream

        mov             rcx, rdi                ; marker
        or              rcx, [%6]               ; next2 | marker
        shrx            %5, rcx, %5             ; consume partial2
%endmacro

        CHECKNOCROSS    rax,rbx,rdx, .bulk_done
        CHECKNOCROSS    rbp,r8,r9, .bulk_done

        ; refill
        REFILLTRIPLE    r10,rax, r11,rbx, r12,rdx ; streams 0-2
        REFILLTRIPLE    r13,rbp, r14,r8,  r15,r9  ; streams 3-5

        ; advance decodeptrs
        vpaddq          xmm0, xmm0, xmm2
        vpaddq          xmm1, xmm1, xmm2

        ; set up mask reg
        mov             rdi, ~HUFF_TABLE_MASK

        %assign out_offs_xmm3 0
        %assign out_offs_xmm4 0

%macro DECONE 2 ; args: bits destreg
        andn            rcx, rdi, %1            ; peek
        vpinsrb         %2, %2, [STATE32(tables,0)+rcx*2+1], out_offs_%[%2] ; sym
        movzx           ecx, byte [STATE32(tables,0)+rcx*2] ; len
        shrx            %1, %1, rcx             ; bits >>= len
        %assign out_offs_%[%2] out_offs_%[%2]+1
%endmacro

        ; Final decode from bit buffer doesn't need to shift to set up
        ; bit buffer for next decode, so we can start working on
        ; advancing the stream pointer. This turns the final variable shift
        ; into a single add (the remaining instructions don't change substantially)
        ; which is a throughput win on Intel's newer uArchs when not using SHRX, and
        ; also relaxes the critical path slightly.
%macro FINALDEC 3 ; args: bits bitsb destreg
        andn            rcx, rdi, %1            ; peek
        lzcnt           %1, %1                  ; bits_consumed (prior to current sym)
        vpinsrb         %3, %3, [STATE32(tables,0)+rcx*2+1], out_offs_%[%3] ; sym
        add             %2, [STATE32(tables,0)+rcx*2] ; bits_consumed (current sym included)
        %assign out_offs_%[%3] out_offs_%[%3]+1
%endmacro

%rep N_DECS_PER_REFILL-1
        DECONE          r10,xmm3                ; stream 0
        DECONE          r11,xmm3                ; stream 1
        DECONE          r12,xmm3                ; stream 2

        DECONE          r13,xmm4                ; stream 3
        DECONE          r14,xmm4                ; stream 4
        DECONE          r15,xmm4                ; stream 5
%endrep

        ; Final decode round
        FINALDEC        r10,r10b,xmm3           ; stream 0
        FINALDEC        r11,r11b,xmm3           ; stream 1
        FINALDEC        r12,r12b,xmm3           ; stream 2

        FINALDEC        r13,r13b,xmm4           ; stream 3
        FINALDEC        r14,r14b,xmm4           ; stream 4
        FINALDEC        r15,r15b,xmm4           ; stream 5

        ; emit first half of bytes
        movq            rdi, xmm0               ; decodeptr[0]
        vmovdqu         [rdi - TRIPLE_DECS_PER_REFILL], xmm3
        mov             edi, 0x0303

%macro ADVANCESTREAM 3 ; args: bits in dirop
        bextr           rcx, %1, rdi            ; bits_consumed >> 3
        %3              %2, rcx                 ; in +-= bits_consumed >> 3
        and             %1, 7                   ; leftover_bits
%endmacro

        ; advance stream pointers
        ADVANCESTREAM   r10,rax, add            ; stream0+
        ADVANCESTREAM   r11,rbx, sub            ; stream1-
        ADVANCESTREAM   r12,rdx, add            ; stream2+

        ADVANCESTREAM   r13,rbp, add            ; stream3+
        ADVANCESTREAM   r14,r8,  sub            ; stream4-
        ADVANCESTREAM   r15,r9,  add            ; stream5+

        ; emit bytes
        movq            rdi, xmm1               ; decodeptr[1]
        vmovdqu         [rdi - TRIPLE_DECS_PER_REFILL], xmm4

        ; check whether we're done
        cmp             rdi, [STATEP(decodeend,1)] ; do {...} while (decodeptr[1]<decodeend[1])
        jb              .bulk_inner
        jmp             .bulk_done

.bulk_done:
        ; store final decodeptrs
        movq            [STATEP(decodeptr,0)], xmm0
        movq            [STATEP(decodeptr,1)], xmm1

        ; undo decodeend[1] subtract
        add             qword [STATEP(decodeend,1)], BYTES_WRITTEN_PER_ITER-1

        ; Check triples: check that our stream pointers are all good
%macro CHECKTRIPLE 6 ; args: bits0 in0 bits1 in1 bits2 in2
        add             %4, 8                   ; in1 += 8 (undo reverse stream setup from init)

        ; non-crossing invariant: in0 <= in2 && in2 <= in1
        cmp             %2, %6
        ja              .bad_stream             ; in0 > in2: definitely bad
        cmp             %6, %4
        jb              %%pointers_ok           ; in2 < in1: all good
        jne             .bad_stream             ; in2 > in1: definitely bad

        ; in1 == in2, i.e. 0 bytes left in either stream
        ; this can be valid, but only if bitpos1=bitpos2=0
        ; (i.e. the bytes are fully consumed)
        mov             rcx, %3
        or              rcx, %5
        jnz             .bad_stream

%%pointers_ok:
%endmacro

        CHECKTRIPLE     r10,rax, r11,rbx, r12,rdx ; check streams 0-2
        CHECKTRIPLE     r13,rbp, r14,r8,  r15,r9  ; check streams 3-5

        ; translate stream state to bitcount form for careful loop

%macro STOREONE 5 ; args: bitpos, in, inoffs, dirop, ind
        mov             rcx, %1
        test            %1, %1                  ; bitpos != 0?
        jz              %%store
        movzx           edi, byte [%2+%3]       ; fetch next byte
        %4              %2                      ; update ptr
        shr             edi, cl                 ; consume partial byte
        neg             ecx                     ; bitc is bits left =8-bitpos
        add             ecx, 8
        mov             [STATE32(bits,%5)], edi ; store leftover bits
%%store:
        mov             [STATEP(bitp,%5)], %2   ; store ptr
        mov             [STATE32(bitc,%5)], ecx ; store count
%endmacro

        STOREONE        r10,rax,  0,inc, 0      ; stream 0+
        STOREONE        r11,rbx, -1,dec, 1      ; stream 1-
        STOREONE        r12,rdx,  0,inc, 2      ; stream 2+
        STOREONE        r13,rbp,  0,inc, 3      ; stream 3+
        STOREONE        r14,r8,  -1,dec, 4      ; stream 4-
        STOREONE        r15,r9,   0,inc, 5      ; stream 5+

.no_bulk:
        mov             eax, 1
        jmp             .epilogue

.bad_stream:
        xor             eax, eax

.epilogue:
        leaf_func_epilogue_and_ret

