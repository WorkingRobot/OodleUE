;// Copyright Epic Games, Inc. All Rights Reserved.
;// This source file is licensed solely to users who have
;// accepted a valid Unreal Engine license agreement 
;// (see e.g., https://www.unrealengine.com/eula), and use
;// of this source file is governed by such agreement.

%include "newlz_huff_common.inc"

%define ELEMS_PER_SLICE		256	; must be a multiple of 4
%define BYTES_PER_SLICE		(ELEMS_PER_SLICE*4)
%define NUM_SLICES			4

struc OodleHistogramContext
	.inptr			resp 1
	.inlen			resp 1
	.counts			resp 1
	.nsyms_flags	resp 1	; bit 16 set: add to counts instead of overwriting
	.scratch		resd NUM_SLICES * ELEMS_PER_SLICE
endstruc

; Increments histogram bucket by 1
%macro INC1 2 ; slice, bucket
	inc				dword [rbp + (%1 % NUM_SLICES)*BYTES_PER_SLICE + %2*4]
%endmacro

; Increments histogram bucket by 2
%macro INC2 2 ; slice, bucket
	add				dword [rbp + (%1 % NUM_SLICES)*BYTES_PER_SLICE + %2*4], 2
%endmacro

; Updates sum for 8 bytes
%macro SUM8 5 ; reg64 reg32 reg8L reg8H INC
	movzx			eax, %3
	%5				0, rax
	movzx			eax, %4
	shr				%1, 16
	%5				1, rax

	movzx			eax, %3
	%5				2, rax
	movzx			eax, %4
	shr				%1, 16
	%5				3, rax

	movzx			eax, %3
	%5				4, rax
	movzx			eax, %4
	shr				%2, 16
	%5				5, rax

	movzx			eax, %3
	%5				6, rax
	movzx			eax, %4
	%5				7, rax
%endmacro

	; extern "C" void oodle_histo_x64_generic(OodleHistogramContext * ctx)
leaf_func_with_prologue oodle_histo_x64_generic
	load_first_arg	rdi
	set_state_reg	OodleHistogramContext, rdi

	; initialize scratch counts to zero
	mov				rsi, [STATEP(counts,0)]
	mov				rdx, rdi
	lea				rdi, [STATE32(scratch,0)]
	mov				ecx, NUM_SLICES*ELEMS_PER_SLICE/2
	xor				eax, eax
	rep				stosq
	mov				rdi, rdx

	mov				rsi, [STATEP(inptr,0)]
	mov				rcx, [STATEP(inlen,0)]

	lea				rbp, [STATE32(scratch,0)]

	; offset ptr from end and flip sense of count
	; 24 not 16 from end because of readahead
	lea				rsi, [rsi + rcx - 24]
	neg				rcx
	add				rcx, 24		; -(count - 24)
	jge				.tail		; >=0 means count was <=24

	; preload first 8 bytes
	mov				rbx, [rsi + rcx]
	jmp				.inner

	radalign		32, 16 ; NOTE(fg): for Jcc erratum, align us to addr that's =16 mod 32

.inner:
	; The load here is for the _next_ block of 8 increments. This turns out to
	; be significantly faster than loading the 64-bit value right before the
	; increments, despite out-of-order execution.

	mov				rdx, [rsi + rcx + 8]
	cmp				rbx, rdx				; if next 64b match current 64b, can save work! (Also, check for runs.)
	je				.add_double
	SUM8			rbx, ebx, bl, bh, INC1

	mov				rbx, [rsi + rcx + 16]
	SUM8			rdx, edx, dl, dh, INC1

	add				rcx, 16
	js 				.inner

.tail:
	; undo offset
	add				rsi, 24
	sub				rcx, 24
    jz          	.dosums

	; handle tail bytes
.taillp:
	movzx			eax, byte [rsi + rcx]
	inc				dword [rbp + rax*4]
	add				rcx, 1
	jnz				.taillp

.dosums:
	; sum sub-histos to final destination
	mov				rax, [STATEP(nsyms_flags,0)]
	mov				rdi, [STATEP(counts,0)]

	movzx			ecx, ax
	shl				ecx, 2		; number of bytes

	test			eax, (1<<16) ; is "add onto counts" flag set?
	jz				.sum_set

	; ---- Final summing loop that adds onto counts
.sum_add:
	test			ecx, 0xf	; are we aligned yet?
	jz				.sum_add_bulk

	mov				eax, [rdi + rcx]
	%assign i 0
%rep NUM_SLICES
	add				eax, [rbp + rcx + i*BYTES_PER_SLICE]
    %assign i i+1
%endrep
	mov				[rdi + rcx], eax
	sub				ecx, 4
	jmp				.sum_add

.sum_add_bulk:
	sub				ecx, 16
	js				.done

.sum_add_bulk_lp:
	movdqa			xmm0, [rdi + rcx] ; existing value from "counts"
    %assign i 0
%rep NUM_SLICES
	paddd			xmm0, [rbp + rcx + i*BYTES_PER_SLICE]
    %assign i i+1
%endrep
	movdqu			[rdi + rcx], xmm0
	sub				ecx, 16
	jns				.sum_add_bulk_lp
	jmp				.done

	; ---- Final summing loop that overwrites counts
.sum_set:
	test			ecx, 0xf	; are we aligned yet?
	jz				.sum_set_bulk

	mov				eax, [rbp + rcx + 0*BYTES_PER_SLICE]
	%assign i 1
%rep NUM_SLICES-1
	add				eax, [rbp + rcx + i*BYTES_PER_SLICE]
	%assign i i+1
%endrep
	mov				[rdi + rcx], eax
	sub				ecx, 4
	jmp				.sum_set

.sum_set_bulk:
	sub				ecx, 16
	js				.done

.sum_set_bulk_lp:
	movdqa			xmm0, [rbp + rcx + 0*BYTES_PER_SLICE]
    %assign i 1
%rep NUM_SLICES-1
	paddd			xmm0, [rbp + rcx + i*BYTES_PER_SLICE]
    %assign i i+1
%endrep
	movdqu			[rdi + rcx], xmm0
	sub				ecx, 16
	jns				.sum_set_bulk_lp

.done:
	leaf_func_epilogue_and_ret

	radalign		32, 16
.add_double:
	; if we get here, we have a pair where the first and second 8 bytes in a group

	; check for sequence of all the same byte
	; x ^ (x << 8) XORs every byte (but byte 0) with its predecessor
	; if that is <256, all 16 bytes are the same value
	shl				rdx, 8
	xor				rdx, rbx
	cmp				rdx, 256
	jnb				.not_all_same ; NOTE(fg): this branch is not the hot path

	; hot path: add 16 to a single bucket
	add				dword [rbp + rdx*4], 16

	mov				rbx, [rsi + rcx + 16]
	add				rcx, 16
	js 				.inner
	jmp				.tail

	radalign		16
.not_all_same:
	; add 2 to each bucket since we confirmed that we have the same
	; 64-bit sequence twice
	SUM8			rbx, ebx, bl, bh, INC2

	mov				rbx, [rsi + rcx + 16]
	add				rcx, 16
	js 				.inner
	jmp				.tail

; vim:ts=4:sts=4:sw=4:noet
