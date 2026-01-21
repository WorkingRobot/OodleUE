;// Copyright Epic Games, Inc. All Rights Reserved.
;// This source file is licensed solely to users who have
;// accepted a valid Unreal Engine license agreement 
;// (see e.g., https://www.unrealengine.com/eula), and use
;// of this source file is governed by such agreement.

%include "asmlib_x86_x64.inc"

	; NOTE: we're saving and restoring way more regs than necessary here

	; extern "C" uint64_t oodle_x64_wc_probe(const void *ptr);
leaf_func_with_prologue oodle_x64_wc_probe
	load_first_arg	rsi

	; do a memory fence, then 16 reads from the same address, then another
	; memory fence, and time via RDTSC how long it takes.
	;
	; if the reads are cached, this is fast.
	; if they're uncached or in WC memory, it is anything but.

	mfence
	rdtsc
	shl				rdx, 32
	lea				r8, [rax + rdx]
%rep 16
	movzx			eax, byte [rsi]
%endrep
	mfence
	rdtsc
	shl				rdx, 32
	add				rax, rdx
	sub				rax, r8
	leaf_func_epilogue_and_ret
