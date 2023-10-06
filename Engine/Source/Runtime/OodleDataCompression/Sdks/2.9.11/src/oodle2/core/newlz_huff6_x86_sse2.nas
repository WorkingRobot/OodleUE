;// Copyright Epic Games, Inc. All Rights Reserved.
;// This source file is licensed solely to users who have
;// accepted a valid Unreal Engine license agreement 
;// (see e.g., https://www.unrealengine.com/eula), and use
;// of this source file is governed by such agreement.

%include "newlz_huff_common.inc"

; The high concept is the same as regular x64 huff6:
; use an explicit marker bit at the top to keep track of how many bits were
; consumed, reducing both live registers (no need to maintain bit count
; per stream) and instructions (no bitcount updates per symbol). Just like
; the regular huff3 SSE2 decoder, we maintain the bit buffers themselves in
; MMX/XMM registers because MMX/SSE2 provide 64-bit shifts in 32-bit mode.
;
; This takes care of essentially all of the decoding.

; The clincher is that we don't have a 64-bit leading zero count readily
; available. It's possible to store the 64-bit bit buffer to memory, read
; the two halves, do BSRs, and merge the results conditionally, but that's
; a lot of work per stream (more than getting rid of the bit count saves).
;
; So instead, improvise a leading zero count from spare parts. The idea
; is to use SSE2 double-precision arithmetic. Take something like 2.0 in
; IEEE-754 double:
;
;   Sgn  Exponent                      Mantissa
;   +-+-----------+----------------------------------------------------+
;   |0|10000000000|0000000000000000000000000000000000000000000000000000|
;   +-+-----------+----------------------------------------------------+
;
; The mantissa has 52 bits (with an implicit leading 1 bit that's not
; stored); by replacing the mantissa bits, we get evenly spaced integers
; between 2 (inclusive) and 4 (exclusive).
;
; Now suppose we put some other value in the mantissa bits, like this:

;   +-+-----------+----------------------------------------------------+
;   |0|10000000000|0000111010101101000111011100011111110110101111011011|
;   +-+-----------+----------------------------------------------------+
;
; if we subtract 2.0 from this value, this gets rid of the implied
; leading 1 bit; the resulting number has to be re-normalized, which
; will end up decrementing the exponent by 1+clz(mantissa). Note we're
; subtracting two exactly representable numbers with an exactly
; representable result; the result is always exact and normalized, so it
; does not depend on the active FP rounding mode, nor on DAZ/FTZ flags.
;
; We can therefore build a 52-bit CLZ using nothing but bitwise ops,
; double-precision math and integer shifts. The basic operation is this:
; (pseudocode)
;
;   // u contains a 52-bit uint
;   // x is a XMM register
;   
;   x = u | double_to_bits(2.0);  // OR in exponent
;   double(x) -= 2.0;             // exponent is now 1023 - clz52(u)
;   x >>= 52;                     // x is now 1023 - clz52(u)
;   x ^= 1023;                    // x is now clz52(u)
;
; As a bonus, all of these are real vector operations, so we can actually
; process two at a time in SSE2 regs, reducing overall cost further.
;
; How do we guarantee that "u" is in fact a 52-bit uint? Well, simple:
; just shift right by 12 first! That means we lose some of the bits in
; our bit buffer, but it's still way better than 32.
;
; Our bit buffer refill works like this:
;   bitbuf = (load64b(ptr) | (1u << 63)) >> (leftover_bits & 7)
; 
; The marker bit at the top is used to tell us (via CLZ) how many bits
; we consumed. After the shift by "leftover_bits", we have between
; (63-7) = 56 and (63 - 0) = 63 bits of payload in the bit buffer, plus
; the marker bit.
; 
; Shifting right by 12 guarantees that the max of 64 bits (marker
; included) in the buffer will fit inside a 52-bit uint. It also means
; that the maximum number of bits we're allowed to *consume* (i.e. shift
; out) before the CLZ is 56-12=44 bits. Any more and we risk shifting out
; or marker bit (which breaks everything).
;
; With the default 11-bit code len, that means we can do a full 4 decodes
; per refill. Not quite as good as the 5 decs/refill we can do with a
; "proper" 64-bit version, but way better than the 2 decs/refill we get in
; a 32-bit decoder.
;
; Note that the aforementioned upper bound of a maximum of 64 bits in the
; buffer (including marker) is pessimistic; every symbol we decode is at
; least 1 bit, so after 4 decodes, we're actually down to a maximum of 60
; bits in the buffer, and could pre-shift by a bit less. However, turns
; out that for 11-bit, we don't need to play it that close. It is
; potentially interesting with 12-bit codes, where a 48-bit maximum and
; 4 decodes/refill works out neatly.

%define CLZ_PRE_SHIFT           (64 - 52)               ; how many bits to shift out of a 64b uint to fit inside a double mantissa
%define MIN_BITS_PER_REFILL     (56 - CLZ_PRE_SHIFT)    ; the pre-shift costs us a few effective bits per refill
%define N_DECS_PER_REFILL       (MIN_BITS_PER_REFILL / CODELEN_LIMIT)
%define TRIPLE_DECS_PER_REFILL  (3*N_DECS_PER_REFILL)

        [section .data]

        align           16

cClzBiasDbl             dq 2.0, 2.0             ; the magic CLZ bias (see explanation above)

        [section .text]

        ; extern "C" bool oodle_newLZ_huff6_x86_sse2_kern(KrakenHuffState *state);
leaf_func_with_prologue oodle_newLZ_huff6_x86_sse2_kern
        load_first_arg  esi ; KrakenHuffState* state
        set_state_reg   KrakenHuffState, esi
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
        mov             eax, [STATEP(decodeend,1)]
        sub             eax, [STATEP(decodeptr,1)]
        cmp             eax, TRIPLE_DECS_PER_REFILL ; need at least TRIPLE_DECS_PER_REFILL bytes since our loop writes that many
        jb              .no_bulk

%macro INITTRIPLE 3 ; args: in1 in2 baseind
        ; nothing to do for stream 0 (we leave the ptr in mem)
        mov             %1, [STATEP(bitp,%3+1)] ; in1
        mov             %2, [STATEP(bitp,%3+2)] ; in2
        mov             eax, %1
        sub             eax, %2                 ; in1-in2
        cmp             eax, 8                  ; need at least 8 bytes between (in2,in1) pair
        jb              .no_bulk
        sub             %1, 8                   ; in1 -= 8 (reverse stream setup)
%endmacro

        INITTRIPLE      ebx,ecx, 0              ; streams 0-2
        INITTRIPLE      edx,ebp, 3              ; streams 3-5

        ; Move decodeend1 forward so we stop the bulk loop before we run a risk
        ; of writing past the end of the dest buffer
        sub             dword [STATEP(decodeend,1)], TRIPLE_DECS_PER_REFILL-1

        ; Get ready for main loop
        xorps           xmm1, xmm1              ; clear leftover bit counts
        xorps           xmm3, xmm3
        xorps           xmm5, xmm5

        mov             [STATEP(bitp,1)], ebx   ; spill pre-inc in1
        mov             edi, [STATEP(decodeptr,1)]

        ; eax = scratch
        ; ebx = decodeptr0 (main) / in1 (during refill)
        ; ecx = in2
        ; edx = in4
        ; ebp = in5
        ; esi = state + 112
        ; edi = decodeptr1 (main)
        ; xmm0 = bits0
        ; xmm1 = bits1 / leftover_bits[0,1]
        ; xmm2 = bits2
        ; xmm3 = bits3 / leftover_bits[2,3]
        ; xmm4 = bits4
        ; xmm5 = bits5 / leftover_bits[4,5]
        ; xmm6 = scratch
        ; xmm7 = marker

        align           16
.bulk_inner:
        mov             eax, [STATEP(bitp,0)]   ; in0 ptr
        pcmpeqd         xmm7, xmm7              ; all-1
        psllq           xmm7, 63                ; marker bit only

%macro CHECKNOCROSS 4 ; args: in0 in1 in2 bail
        ; non-crossing invariant: in0 <= in2 && in2 <= in1
        cmp             %3, %1
        jb              %4
        cmp             %3, %2
        ja              %4
%endmacro

        ; no-crossing invariant checks
        CHECKNOCROSS    eax,ebx,ecx, .bulk_done

        movq            xmm0, [eax]             ; next0
        mov             eax, [STATEP(bitp,3)]   ; in3 ptr

        CHECKNOCROSS    eax,edx,ebp, .bulk_done

        ; refill bit buffers
        por             xmm0, xmm7              ; next0 | marker
        psrlq           xmm0, xmm1              ; consume partial0

        pshufd          xmm6, xmm1, 0xee        ; leftover_bits1
        movq            xmm1, [ebx]             ; in1 load - need to reverse
        pshuflw         xmm1, xmm1, 0x1b        ; reverse words in qword (1st part of BSWAP)
        movdqa          xmm2, xmm1
        psrlw           xmm1, 8
        psllw           xmm2, 8
        por             xmm1, xmm2              ; next1 (BSWAP complete)
        por             xmm1, xmm7              ; next1 | marker
        psrlq           xmm1, xmm6              ; consume partial1

        mov             ebx, [STATEP(decodeptr,0)] ; reload decodeptr0

        movq            xmm2, [ecx]             ; next2
        por             xmm2, xmm7              ; next2 | marker
        psrlq           xmm2, xmm3              ; consume partial2

        pshufd          xmm6, xmm3, 0xee        ; leftover_bits3
        movq            xmm3, [eax]             ; next3
        por             xmm3, xmm7              ; next3 | marker
        psrlq           xmm3, xmm6              ; consume partial3

        movq            xmm4, [edx]             ; in4 load - need to reverse
        pshuflw         xmm4, xmm4, 0x1b        ; reverse words in qword (1st part of BSWAP)
        movdqa          xmm6, xmm4
        psrlw           xmm4, 8
        psllw           xmm6, 8
        por             xmm4, xmm6              ; next4 (BSWAP complete)
        por             xmm4, xmm7              ; next4 | marker
        psrlq           xmm4, xmm5              ; consume partial4

        pshufd          xmm6, xmm5, 0xee        ; leftover_bits5
        movq            xmm5, [ebp]             ; next5
        por             xmm5, xmm7              ; next5 | marker
        psrlq           xmm5, xmm6              ; consume partial5

        ; reload and advance decodeptrs
        add             ebx, TRIPLE_DECS_PER_REFILL
        add             edi, TRIPLE_DECS_PER_REFILL
        mov             [STATEP(decodeptr,0)], ebx

        %assign out_offs_ebx -TRIPLE_DECS_PER_REFILL
        %assign out_offs_edi -TRIPLE_DECS_PER_REFILL

%macro DECONE 2 ; args: bits, destreg
        movd            eax, %1                 ; bits
        and             eax, HUFF_TABLE_MASK    ; peek
        movd            xmm6, [STATE32(lentab,0)+eax*4] ; len
        movzx           eax, byte [STATE32(symtab,0)+eax] ; sym
        psrlq           %1, xmm6                ; bits >>= len
        mov             [%2+out_offs_%[%2]], al ; store sym
        %assign out_offs_%[%2] out_offs_%[%2]+1
%endmacro

        ; load the first magic CLZ constant (explanation at the top)
        movaps          xmm7, [cClzBiasDbl]

%rep N_DECS_PER_REFILL
        DECONE          xmm0,ebx                ; stream 0
        DECONE          xmm1,ebx                ; stream 1
        DECONE          xmm2,ebx                ; stream 2

        DECONE          xmm3,edi                ; stream 3
        DECONE          xmm4,edi                ; stream 4
        DECONE          xmm5,edi                ; stream 5
%endrep

        ; materialize the second magic CLZ constant (explanation at the top)
        pcmpeqd         xmm6, xmm6              ; start with all ones
        psrlq           xmm6, 64-10             ; final XOR mask is 1023 (see explanation above)

%macro ADVANCEPAIR 6 ; arg: bits0 in0 op0 bits1 in1 op1
        movlhps         %1, %4                  ; pack stream pair bits
        psrlq           %1, CLZ_PRE_SHIFT       ; make sure our CLZ value fits inside a double mantissa
        orpd            %1, xmm7                ; guerrilla CLZ step 1: bias
        subpd           %1, xmm7                ; guerrilla CLZ step 2: sub to renorm (puts bias-<lzcnt> in exp)
        psrlq           %1, 52                  ; guerrilla CLZ step 3: extract exp
        pxor            %1, xmm6                ; guerrilla CLZ step 4: bias-(bias-<lzcnt>) = <lzcnt>
        movdqa          %4, %1                  ; save leftover counts
        psrlq           %1, 3                   ; compute advance byte counts

        ; advance first ptr
        movd            eax, %1
        %3              %2, eax

        ; advance second ptr
        pextrw          eax, %1, 4
        %6              %5, eax
%endmacro

        ; reload in1
        mov             ebx, [STATEP(bitp,1)]

        ; advance stream pointers
        ADVANCEPAIR     xmm0,[STATEP(bitp,0)],add, xmm1,ebx,sub ; stream0+ stream1-
        ADVANCEPAIR     xmm2,ecx,add, xmm3,[STATEP(bitp,3)],add ; stream2+ stream3+
        ADVANCEPAIR     xmm4,edx,sub, xmm5,ebp,add              ; stream4- stream5+

        ; mask bitcounts for all streams
        psrlq           xmm6, 10-3              ; turn final XOR mask (1023) into leftover bit mask (7)
        pand            xmm1, xmm6
        pand            xmm3, xmm6
        pand            xmm5, xmm6

        ; spill updated in1 for next iter
        mov             [STATEP(bitp,1)], ebx
        
        ; check whether we're done
        cmp             edi, [STATEP(decodeend,1)] ; do {...} while (decodeptr[1]<decodeend[1])
        jb              .bulk_inner

.bulk_done:
        ; unpack bitcounts
        movdqa          xmm0, xmm1
        psrldq          xmm1, 8
        movdqa          xmm2, xmm3
        psrldq          xmm3, 8
        movdqa          xmm4, xmm5
        psrldq          xmm5, 8

        ; store final decodeptr1
        mov             [STATEP(decodeptr,1)], edi

        ; undo decodeend[1] subtract
        add             dword [STATEP(decodeend,1)], TRIPLE_DECS_PER_REFILL-1

        ; Check triples: check that our stream pointers are all good
%macro CHECKTRIPLE 6 ; args: bits0 in0 bits1 in1 bits2 in2
        add             %4, 8                   ; in1 += 8 (undo reverse stream setup from init)

        ; non-crossing invariant: in0 <= in2 && in2 <= in1
        cmp             %6, %2
        jb              .bad_stream             ; in0 > in2: definitely bad
        cmp             %6, %4
        jb              %%pointers_ok           ; in2 < in1: all good
        jne             .bad_stream             ; in2 > in1: definitely bad

        ; in1 == in2, i.e. 0 bytes left in either stream
        ; this can be valid, but only if bitpos1=bitpos2=0
        ; (i.e. the bytes are fully consumed)
        movd            eax, %3
        movd            edi, %5
        or              eax, edi
        jnz             .bad_stream

%%pointers_ok:
%endmacro

        CHECKTRIPLE     xmm0,[STATEP(bitp,0)], xmm1,ebx, xmm2,ecx ; check streams 0-2
        CHECKTRIPLE     xmm3,[STATEP(bitp,3)], xmm4,edx, xmm5,ebp ; check streams 3-5

        ; translate stream state to bitcount form for careful loop

        ; first, store all remaining stream pointers
        mov             [STATEP(bitp,1)], ebx
        mov             [STATEP(bitp,2)], ecx
        mov             [STATEP(bitp,4)], edx
        mov             [STATEP(bitp,5)], ebp

%macro STOREONE 4 ; args: bitpos, ind, inoffs, dirop
        movd            ecx, %1
        test            ecx, ecx                ; bitpos != 0?
        jz              %%store
        mov             edi, [STATEP(bitp,%2)]  ; fetch bitp
        movzx           edi, byte [edi+%3]      ; fetch next byte from stream
        %4              dword [STATEP(bitp,%2)] ; update ptr
        shr             edi, cl                 ; consume partial byte
        neg             ecx                     ; bitc is bits left =8-bitpos
        add             ecx, 8
        mov             [STATE32(bits,%2)], edi ; store leftover bits
%%store:
        mov             [STATE32(bitc,%2)], ecx ; store count
%endmacro

        STOREONE        xmm0,0,  0,inc          ; stream 0+
        STOREONE        xmm1,1, -1,dec          ; stream 1-
        STOREONE        xmm2,2,  0,inc          ; stream 2+
        STOREONE        xmm3,3,  0,inc          ; stream 3+
        STOREONE        xmm4,4, -1,dec          ; stream 4-
        STOREONE        xmm5,5,  0,inc          ; stream 5+

.no_bulk:
        mov             eax, 1
        jmp             .epilogue

.bad_stream:
        xor             eax, eax

.epilogue:
        leaf_func_epilogue_and_ret

