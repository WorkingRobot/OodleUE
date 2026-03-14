;// Copyright Epic Games, Inc. All Rights Reserved.
;// This source file is licensed solely to users who have
;// accepted a valid Unreal Engine license agreement 
;// (see e.g., https://www.unrealengine.com/eula), and use
;// of this source file is governed by such agreement.

%include "newlz_huff_common.inc"

struc KrakenHuffEncState
	.inptr		resp 1
	.inend		resp 1
	.codeptr	resp 1
	.lenptr		resp 1
	.outp  		resp 3
	.outb   	resq 3
	.outc    	resd 3
endstruc

%ifdef BMI2

	%define NAME oodle_enchuff3_x64_bmi2_kern

	%macro ENCONE 4 ; args: bitbuf index bitcnt8 bitcnt64
		movzx			eax, byte [rsi + %2]		; input byte
		shlx			rcx, [rbp + rax*8], %4		; shifted code
		add				%3, [rbx + rax]				; update count
		or				%1, rcx						; insert shifted code
	%endmacro

%elifdef JAGUAR

	%define NAME oodle_enchuff3_x64_jaguar_kern

	; on Jaguar, do one 16-byte vmovdqu per iteration and then grab the
	; source bytes via vpextrb, to reduce number of loads (since Jaguar
	; has "only" one load unit)

	%macro ENCONE 4 ; args: bitbuf index bitcnt8 bitcnt64
		vpextrb			eax, xmm0, %2				; input byte
		movzx			ecx, %3
		add				%3, [rbx + rax]				; len
		mov				rax, [rbp + rax*8]			; code
		shl				rax, cl						; shifted code
		or				%1, rax						; insert shifted code
	%endmacro

%else

	%define NAME oodle_enchuff3_x64_generic_kern

	%macro ENCONE 4 ; args: bitbuf index bitcnt8 bitcnt64
		movzx			eax, byte [rsi + %2]		; input byte
		movzx			ecx, %3
		add				%3, [rbx + rax]				; len
		mov				rax, [rbp + rax*8]			; code
		shl				rax, cl						; shifted code
		or				%1, rax						; insert shifted code
	%endmacro

%endif

	; extern "C" void NAME(KrakenHuffEncState * st)
leaf_func_with_prologue NAME
	load_first_arg	rdi
	set_state_reg	KrakenHuffEncState, rdi

	mov				rbx, [STATEP(lenptr,0)]
	mov				rbp, [STATEP(codeptr,0)]
	mov				rsi, [STATEP(inptr,0)]

	mov				r8, [STATEP(outp,0)]
	mov				r9, [STATE64(outb,0)]
	movzx			edx, byte [STATE32(outc,0)]
	mov				r10, [STATEP(outp,1)]
	mov				r11, [STATE64(outb,1)]
	movzx			r14d, byte [STATE32(outc,1)]
	mov				r12, [STATEP(outp,2)]
	mov				r13, [STATE64(outb,2)]
	movzx			r15d, byte [STATE32(outc,2)]

	; Outer loop promises only to call with >=16 input bytes
	; loop until <16 left
	; (this is for the Jaguar loop which does 16-byte reads;
	; regular x64 version reads 15 bytes, but close enough.)
	sub				qword [STATEP(inend,0)], 16

	; rax = scratch
	; rbx = code len ptr
	; rcx = scratch
	; rdx = bitc0 (dl)
	; rbp = code tab ptr
	; rsi = inptr
	; rdi = state
	; r8  = outptr0
	; r9  = outbits0
	; r10 = outptr1
	; r11 = outbits2
	; r12 = outptr2
	; r13 = outbits2
	; r14 = bitc1 (r14b)
	; r15 = bitc2 (r15b)

	jmp				.inner
	radalign		32, 16 ; NOTE(fg): for Jcc erratum, align us to an address that's =16 mod 32
.inner:

%macro FINISTREAM 4 ; args: bitptr bitbuf rev? bitcnt8
%if %3 == 0
	mov				[%1], %2					; emit bitbuf bits
%elifdef JAGUAR
	movbe			[%1-8], %2					; emit bitbuf bits
%else
	mov				rax, %2						; bitbuf
	bswap			rax
	mov				[%1-8], rax					; emit bitbuf bits
%endif
	movzx			ecx, %4						; zero-extend bit count
	and				%4, 7						; bit count post-emit
	and				ecx, ~7						; number of bits emitted
%ifdef BMI2
	shrx			%2, %2, rcx					; shift out emitted bits
%else
	shr				%2, cl						; shift out emitted bits
%endif
	shr				ecx, 3						; number of bytes emitted
%if %3 == 0
	add				%1, rcx						; update write ptr
%else
	sub				%1, rcx						; update write ptr
%endif
%endmacro

%ifdef JAGUAR
	vmovdqu			xmm0, [rsi]					; load the source bytes
%endif

	%assign offs 0
%rep 5
	ENCONE			r9,(offs+0),dl,rdx
	ENCONE			r11,(offs+1),r14b,r14
	ENCONE			r13,(offs+2),r15b,r15
	%assign offs offs+3
%endrep
	add				rsi, 15						; fileptr += 15
	FINISTREAM		r8,r9,0,dl
	FINISTREAM		r10,r11,1,r14b
	FINISTREAM		r12,r13,0,r15b

	; NOTE(fg): branch is in a safe spot
	cmp				rsi, [STATEP(inend,0)]
	jbe				.inner

	add				qword [STATEP(inend,0)], 16	; restore inend

	; Zero-extend final bit counts to 32 bits
	movzx			edx, dl
	movzx			r14d, r14b
	movzx			r15d, r15b

	; Store final output buf contents and ptrs
	mov				[STATEP(inptr,0)], rsi
	mov				[STATEP(outp,0)], r8
	mov				[STATE64(outb,0)], r9
	mov				[STATE32(outc,0)], edx
	mov				[STATEP(outp,1)], r10
	mov				[STATE64(outb,1)], r11
	mov				[STATE32(outc,1)], r14d
	mov				[STATEP(outp,2)], r12
	mov				[STATE64(outb,2)], r13
	mov				[STATE32(outc,2)], r15d

.done:
	leaf_func_epilogue_and_ret

; vim:ts=4:sts=4:sw=4:noet
