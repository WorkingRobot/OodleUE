;// Copyright Epic Games, Inc. All Rights Reserved.
;// This source file is licensed solely to users who have
;// accepted a valid Unreal Engine license agreement 
;// (see e.g., https://www.unrealengine.com/eula), and use
;// of this source file is governed by such agreement.

; Oodle NASM library

; detect some parameters from output format
%ifidn __OUTPUT_FORMAT__,win32
        %define resp resd
        %define PTR_SIZE 4
        %define LEADING_UNDERSCORES
        %define CALLEE_SAVE_GPRS ebp,ebx,esi,edi
        %define BYTES_PER_ARG 4
        %define CDECL32_ABI
%elifidn __OUTPUT_FORMAT__,elf32
        %define resp resd
        %define PTR_SIZE 4
        %define LEADING_UNDERSCORES
        %define CALLEE_SAVE_GPRS ebp,ebx,esi,edi
        %define BYTES_PER_ARG 4
        %define CDECL32_ABI
		%define ELF_FORMAT
%elifidn __OUTPUT_FORMAT__,macho32
        %define resp resd
        %define PTR_SIZE 4
        %define LEADING_UNDERSCORES
        %define CALLEE_SAVE_GPRS ebp,ebx,esi,edi
        %define BYTES_PER_ARG 4
        %define CDECL32_ABI
%elifidn __OUTPUT_FORMAT__,win64
        %define resp resq
        %define PTR_SIZE 8
        %define CALLEE_SAVE_GPRS rbx,rbp,rsi,rdi,r12,r13,r14,r15
        %define BYTES_PER_ARG 8
        %define WIN64_ABI
%elifidn __OUTPUT_FORMAT__,elf64
        %define resp resq
        %define PTR_SIZE 8
        %define CALLEE_SAVE_GPRS rbx,rbp,r12,r13,r14,r15
        %define BYTES_PER_ARG 8
        %define SYSV64_ABI
		%define ELF_FORMAT
%elifidn __OUTPUT_FORMAT__,macho64
        %define resp resq
        %define PTR_SIZE 8
        %define LEADING_UNDERSCORES
        %define CALLEE_SAVE_GPRS rbx,rbp,r12,r13,r14,r15
        %define BYTES_PER_ARG 8
        %define SYSV64_ABI
%else
%fatal "Unrecognized output format!"
%endif

; ---------------------------------------------------------------------------

; Custom alignment code; doesn't just align to a target multiple, but rather
; lets us hit a specific value mod the given alignment (second parameter)
;
; based on NASM's official smartalign macro

; based on alignmode P6; works for both 32b and 64b code
%define RADALIGN_1B 0x90
%define RADALIGN_2B 0x66,0x90
%define RADALIGN_3B 0x0f,0x1f,0x00
%define RADALIGN_4B 0x0f,0x1f,0x40,0x00
%define RADALIGN_5B 0x0f,0x1f,0x44,0x00,0x00
%define RADALIGN_6B 0x66,0x0f,0x1f,0x44,0x00,0x00
%define RADALIGN_7B 0x0f,0x1f,0x80,0x00,0x00,0x00,0x00
%define RADALIGN_8B 0x0f,0x1f,0x84,0x00,0x00,0x00,0x00,0x00
%define RADALIGN_JMP_THRESHOLD 16

; radalign <align> <modulo=0>
%imacro radalign 1-2.nolist 0
	sectalign %1   ; align the segment as well
	%push
	%assign %$offs (%1) + (%2)
	%assign %$pad ((%$offs - (($-$$) % (%1))) % (%1))
    %if __PASS__ == 0 || __PASS__ == 3
		times %$pad nop
	%else
		%if %$pad > RADALIGN_JMP_THRESHOLD
			jmp %$end
			; can't re-use pad here since $ will have changed
			times ((%$offs - (($-$$) % (%1))) % (%1)) nop
		%else
			times (%$pad / 8) db RADALIGN_8B
			%assign %$pad %$pad % 8
			%if %$pad > 0
				db RADALIGN_%[%$pad]B
			%endif
		%endif
	%endif
%$end:
	%pop
%endmacro

; Flag stack as non-executable (for ELF)
%ifdef ELF_FORMAT
	section .note.GNU-stack progbits noalloc noexec nowrite
%endif

; Declare text section with 32-byte alignment

	section .text align=32

; cglobal name: Defines a decorated name for a C function given the platform ABI
%macro cglobal 1
    %ifdef LEADING_UNDERSCORES
		%define %1 _%1
    %endif
	%ifdef ELF_FORMAT
		global %1:function (%1.fn_end_mark - %1)
	%else
		global %1
	%endif

    %assign prologue_pushcount 0
%endmacro

; prologue_push: save multiple regs (passed as arguments) and keep track of how many
; so we know how to find stack args later
%macro prologue_push 1-*
    %rep %0
        push %1
        %rotate 1
    %endrep
    %assign prologue_pushcount prologue_pushcount+%0
%endmacro

; epilogue_pop: restore multiple regs (list passed as arguments, same order as in prologue_push)
%macro epilogue_pop 1-*
    %rep %0
        %rotate -1
        pop %1
    %endrep
    %assign prologue_pushcount prologue_pushcount-%0
%endmacro

; stackarg_offs: offset for stack-passed argument i (which is not necessarily argument i)
%define stackarg_offs(i) ((prologue_pushcount + 1 + (i))*BYTES_PER_ARG)

; load_first_arg: loads first arg to given destination reg
%macro load_first_arg 1
    %ifdef CDECL32_ABI
        mov %1, [esp+stackarg_offs(0)]
    %elifdef SYSV64_ABI
        mov %1, rdi ; first arg in rdi
    %elifdef WIN64_ABI
        mov %1, rcx ; first arg in rcx
    %else
        %error "Unknown ABI!"
    %endif
%endmacro

; win64_get_regnum: assigns preprocessor "regnum" to be the x64 register number
; corresponding to the given register name. Used to generate Win64 unwind
; tables.
%macro win64_get_regnum 1.nolist
    %ifidni %1,rax
        %assign regnum 0
    %elifidni %1,rcx
        %assign regnum 1
    %elifidni %1,rdx
        %assign regnum 2
    %elifidni %1,rbx
        %assign regnum 3
    %elifidni %1,rsp
        %assign regnum 4
    %elifidni %1,rbp
        %assign regnum 5
    %elifidni %1,rsi
        %assign regnum 6
    %elifidni %1,rdi
        %assign regnum 7
    %elifidni %1,r8
        %assign regnum 8
    %elifidni %1,r9
        %assign regnum 9
    %elifidni %1,r10
        %assign regnum 10
    %elifidni %1,r11
        %assign regnum 11
    %elifidni %1,r12
        %assign regnum 12
    %elifidni %1,r13
        %assign regnum 13
    %elifidni %1,r14
        %assign regnum 14
    %elifidni %1,r15
        %assign regnum 15
    %else
        %fatal %1 is not a valid 64-bit register.
    %endif
%endmacro

; win64_setup_unwind_code: used to generate Win64 exception handling tables
; sets up prologue size in offs_in_prologue, unwind code in unwindcode
%macro win64_setup_unwind_code 1-*
    %assign offs_in_prologue 0
    %xdefine unwindcode
    %rep %0
        %rotate 1
        win64_get_regnum %1
        ; unwind code:
        ; CodeOffset=offs_in_prologue
        ; UnwindOp=UWOP_PUSH_NONVOL=0, OpInfo=<register number>
        %xdefine unwindcode offs_in_prologue,(regnum<<4)|0,unwindcode

        ; push for upper 8 GPRs is 2 bytes, lower 8 is 1 byte
        %if regnum >= 8
            %assign offs_in_prologue offs_in_prologue+2
        %else
            %assign offs_in_prologue offs_in_prologue+1
        %endif
    %endrep
%endmacro

; leaf_func_with_prologue name: declares a public leaf function, generates a prologue, and on Win64
; also sets up unwind data.
%macro leaf_func_with_prologue 1

%ifdef WIN64_ABI ; the fun never stops!
    [section .pdata rdata align=4] ; RUNTIME_FUNC description
    dd %1 wrt ..imagebase               ; start of func
    dd %1%+.fn_end_mark wrt ..imagebase ; end of func
    dd unwind_%+%1 wrt ..imagebase      ; ptr to unwind data

    [section .xdata rdata align=8] ; UNWIND_INFO stuff
unwind_%+%1:
    win64_setup_unwind_code CALLEE_SAVE_GPRS
    db 1                                ; version 1, no exception handler
    db offs_in_prologue                 ; size of prologue in bytes
    db (.unwind_end-.unwind_begin)/2    ; number of unwind codes
    db 0                                ; no frame register
.unwind_begin:
    db unwindcode
.unwind_end:
    %undef offs_in_prologue
    %undef unwindcode
    %undef regnum
%endif

    section .text
    cglobal %1
%1:
    prologue_push CALLEE_SAVE_GPRS
%endmacro

; leaf_func_epilogue_and_ret: must be the last thing in a leaf function
%macro leaf_func_epilogue_and_ret 0
    epilogue_pop CALLEE_SAVE_GPRS
    ret
.fn_end_mark: ; for Win64 unwind info and ELF symbol lens
%endmacro

; Hot trace markup: makes it so the next instruction is 32B
; aligned and adds IACA markers if desired (IACA_MARKERS defined)
%macro hot_trace_begin 0-1 0
%ifdef IACA_MARKERS
    radalign 32, 22+(%1) ; marker is 10 bytes
    ud2 ; 2B
    mov ebx, 111 ; 5B
    db 0x64, 0x67, 0x90 ; 3B
%else
    radalign 32, (%1)
%endif
%endmacro

%macro hot_trace_end 0
%ifdef IACA_MARKERS
    mov ebx, 222 ; 5B
    db 0x64, 0x67, 0x90 ; 3B
    ud2 ; 2B
%endif
%endmacro
