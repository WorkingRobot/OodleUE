;// Copyright Epic Games, Inc. All Rights Reserved.
;// This source file is licensed solely to users who have
;// accepted a valid Unreal Engine license agreement 
;// (see e.g., https://www.unrealengine.com/eula), and use
;// of this source file is governed by such agreement.

%include "newlz_huff_common.inc"

%define N_DECS_PER_REFILL       (56 / CODELEN_LIMIT)
%define TRIPLE_DECS_PER_REFILL  (3*N_DECS_PER_REFILL)

%ifdef BMI2

    %define FUNCNAME oodle_newLZ_huff6_x64_bmi2_kern
    
    %macro CLZ64 2 ; args: out, in
            lzcnt           %1, %2
    %endmacro

    %macro SHR64_BY_CL 1 ; args: reg
            shrx            %1, %1, rcx
    %endmacro

	; Always enable Raptor Lake workaround on BMI2 targets.
	; Empirically, perf impact is low even on unaffected machines,
	; so not worth having an extra CPU path for this.
    %define RAPTOR_LAKE_WORKAROUND

%else

    %define FUNCNAME oodle_newLZ_huff6_x64_generic_kern
    
    %macro CLZ64 2 ; args: out, in
            bsr             %1, %2
            xor             %1, 63
    %endmacro

    %macro SHR64_BY_CL 1 ; args: reg
            shr             %1, cl
    %endmacro

%endif

        ; extern "C" bool FUNCNAME(KrakenHuffState *state);
leaf_func_with_prologue FUNCNAME

        load_first_arg  rsi ; KrakenHuffState* state
        set_state_reg   KrakenHuffState, rsi
%ifdef RAPTOR_LAKE_WORKAROUND
        offset_state_by KrakenHuffState.tables ; so our table loads are offset 0 (makes them smaller)
%else
        offset_state_by 112                     ; disp8 offsets are [-128,127]; maximize useful range from rsi
%endif

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
        cmp             rax, TRIPLE_DECS_PER_REFILL ; need at least TRIPLE_DECS_PER_REFILL bytes since our loop writes that many
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
        sub             qword [STATEP(decodeend,1)], TRIPLE_DECS_PER_REFILL-1

        ; Get ready for main loop
        xor             r10, r10                ; bits0=empty
        xor             r11, r11                ; bits1=empty
        xor             r12, r12                ; bits2=empty
        xor             r13, r13                ; bits3=empty
        xor             r14, r14                ; bits4=empty
        xor             r15, r15                ; bits5=empty


        movq            xmm0, [STATEP(decodeptr,0)]
        movq            xmm1, [STATEP(decodeptr,1)]
        mov             ecx, TRIPLE_DECS_PER_REFILL
        movq            xmm2, rcx
        movq            xmm3, [STATEP(bitp,0)]

        ; main decode loop
        ; rax = decodeptr0 (main) / in0 (during refill)
        ; rbx = in1
        ; rcx = scratch
        ; rdx = in2
        ; rbp = in3
        ; rsi = &s->tables
        ; rdi = decodeptr1 (main) / marker (during refill)
        ; r8  = in4
        ; r9  = in5 / huff mask
        ; r10 = bits0
        ; r11 = bits1
        ; r12 = bits2
        ; r13 = bits3
        ; r14 = bits4
        ; r15 = bits5
        ; xmm0 = decodeptr0 backing store
        ; xmm1 = decodeptr1 backing store
        ; xmm2 = decodeptr advance
        ; xmm3 = in0 backing store
        ; xmm4 = in5 backing store

        hot_trace_begin                         ; also aligns to 32

.bulk_inner:
%macro CHECKNOCROSS 4 ; args: in0 in1 in2 bail
        ; non-crossing invariant: in0 <= in2 && in2 <= in1
        cmp             %1, %3
        ja              %4
        cmp             %3, %2
        ja              %4
%endmacro

%macro REFILLTRIPLE 6 ; args: bits0 in0 bits1 in1 bits2 in2
%ifdef BMI2
        mov             rcx, [%2]               ; next0
        or              rcx, rdi                ; next0 | marker
        shrx            %1, rcx, %1             ; consume partial0

        mov             rcx, [%4]               ; LE next1
        or              rcx, 128                ; bswap(next1 | marker)
        bswap           rcx                     ; next1
        shrx            %3, rcx, %3             ; consume partial1
        prefetcht0      [%4 - 128]              ; prefetch backwards-moving stream

        mov             rcx, [%6]               ; next2
        or              rcx, rdi                ; next2 | marker
        shrx            %5, rcx, %5             ; consume partial2
%else
        mov             rcx, %1                 ; leftover_bits0
        mov             %1, [%2]                ; next0
        or              %1, rdi                 ; next0 | marker
        SHR64_BY_CL     %1                      ; consume partial0

        mov             rcx, %3                 ; leftover_bits1
        mov             %3, [%4]
        or              %3, 128                 ; bswap(next1 | marker)
        bswap           %3                      ; next1 | marker
        SHR64_BY_CL     %3                      ; consume partial1
        prefetcht0      [%4 - 128]              ; prefetch backwards-moving stream

        mov             rcx, %5                 ; leftover_bits2
        mov             %5, [%6]                ; next2
        or              %5, rdi                 ; next2 | marker
        SHR64_BY_CL     %5                      ; consume partial2
%endif
%endmacro

        CHECKNOCROSS    rax,rbx,rdx, .bulk_done

        ; NOTE(fg): these instructions are here so that the "no-crossing"
        ; tests/branches don't span 32b boundaries in the instruction
        ; stream (Jcc erratum workaround)
        ;
        ; check listing if you move anything in here around!
        mov             rdi, (1<<63)            ; marker bit

        ; advance decodeptrs
        paddq           xmm0, xmm2
        paddq           xmm1, xmm2

        CHECKNOCROSS    rbp,r8,r9, .ran_out_post_advance

        ; refill and move decodeptrs over to integer regs
        REFILLTRIPLE    r10,rax, r11,rbx, r12,rdx ; streams 0-2
        movq            rax, xmm0               ; decodeptr[0]
        REFILLTRIPLE    r13,rbp, r14,r8,  r15,r9  ; streams 3-5
        movq            rdi, xmm1               ; decodeptr[1]

        ; advance decodeptrs and move over to integer regs
%ifdef BMI2
        movq            xmm4, r9
        mov             r9, ~HUFF_TABLE_MASK
%endif

        %assign out_offs_rax -TRIPLE_DECS_PER_REFILL
        %assign out_offs_rdi -TRIPLE_DECS_PER_REFILL

%macro DECONE 2 ; args: bits destreg
%ifdef BMI2
        andn            rcx, r9, %1             ; peek
%else
        mov             ecx, HUFF_TABLE_MASK
        and             rcx, %1                 ; peek
%endif
        movzx           ecx, word [STATE32(tables,0)+rcx*2] ; cl=len ch=sym
        SHR64_BY_CL     %1                      ; bits >>= len
%ifdef RAPTOR_LAKE_WORKAROUND
        shr             ecx, 8                  ; avoid mov [...], ch to work around 13th/14th gen Intel bug
        mov             [%2+out_offs_%[%2]], cl ; store sym
%else
        mov             [%2+out_offs_%[%2]], ch ; store sym
%endif
        %assign out_offs_%[%2] out_offs_%[%2]+1
%endmacro

        ; Final decode from bit buffer doesn't need to shift to set up
        ; bit buffer for next decode, so we can start working on
        ; advancing the stream pointer. This turns the final variable shift
        ; into a single add (the remaining instructions don't change substantially)
        ; which is a throughput win on Intel's newer uArchs when not using SHRX, and
        ; also relaxes the critical path slightly.
%macro FINALDEC 2 ; args: bits destreg
%ifdef BMI2
        andn            rcx, r9, %1             ; peek
%else
        mov             ecx, HUFF_TABLE_MASK
        and             rcx, %1                 ; peek
%endif
        movzx           ecx, word [STATE32(tables,0)+rcx*2] ; cl=len ch=sym
        CLZ64           %1, %1                  ; bits_consumed (prior to current sym)
        add             %1, rcx                 ; bits_consumed (current sym included)
%ifdef RAPTOR_LAKE_WORKAROUND
        shr             ecx, 8                  ; avoid mov [...], ch to work around 13th/14th gen Intel bug
        mov             [%2+out_offs_%[%2]], cl ; store sym
%else
        mov             [%2+out_offs_%[%2]], ch ; store sym
%endif
        %assign out_offs_%[%2] out_offs_%[%2]+1
%endmacro

%rep N_DECS_PER_REFILL-1
        DECONE          r10,rax                 ; stream 0
        DECONE          r11,rax                 ; stream 1
        DECONE          r12,rax                 ; stream 2

        DECONE          r13,rdi                 ; stream 3
        DECONE          r14,rdi                 ; stream 4
        DECONE          r15,rdi                 ; stream 5
%endrep

        ; Final decode round
        FINALDEC        r10,rax                 ; stream 0
        FINALDEC        r11,rax                 ; stream 1
        FINALDEC        r12,rax                 ; stream 2

        FINALDEC        r13,rdi                 ; stream 3
        FINALDEC        r14,rdi                 ; stream 4
        FINALDEC        r15,rdi                 ; stream 5

        ; reload in0
        movq            rax, xmm3
%ifdef BMI2
        movq            r9, xmm4
%endif

%macro ADVANCESTREAM 4 ; args: bits bitsb in dirop
%ifdef BMI2
		; not a functional change, code size change for Jcc erratum workaround (sigh)
        movzx           ecx, %2
        shr             ecx, 3
%else
        movzx           rcx, %2
        shr             rcx, 3
%endif
        %4              %3, rcx                 ; in +-= bits_consumed >> 3
        and             %1, 7                   ; leftover_bits
%endmacro

        ; advance stream pointers
        ADVANCESTREAM   r10,r10b,rax, add       ; stream0+
        ADVANCESTREAM   r11,r11b,rbx, sub       ; stream1-
        ADVANCESTREAM   r12,r12b,rdx, add       ; stream2+

        ; spill in0 for next iter
        movq            xmm3, rax               ; save in0 for next iter

        ADVANCESTREAM   r13,r13b,rbp, add       ; stream3+
        ADVANCESTREAM   r14,r14b,r8,  sub       ; stream4-
        ADVANCESTREAM   r15,r15b,r9,  add       ; stream5+

        ; check whether we're done
        ; NOTE(fg): cmp/branch here are close to 32B boundary in BMI2 (Raptor Lake workaround)
		; path, hence the finessing code size with the ADVANCESTREAM tweak above.
        cmp             rdi, [STATEP(decodeend,1)] ; do {...} while (decodeptr[1]<decodeend[1])
        jb              .bulk_inner
        hot_trace_end
        jmp             .bulk_done

.ran_out_post_advance:
        ; if we ran out of bytes, undo final advance
        psubq           xmm0, xmm2
        psubq           xmm1, xmm2

.bulk_done:
        ; store final decodeptrs
        movq            [STATEP(decodeptr,0)], xmm0
        movq            [STATEP(decodeptr,1)], xmm1

        ; undo decodeend[1] subtract
        add             qword [STATEP(decodeend,1)], TRIPLE_DECS_PER_REFILL-1

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

