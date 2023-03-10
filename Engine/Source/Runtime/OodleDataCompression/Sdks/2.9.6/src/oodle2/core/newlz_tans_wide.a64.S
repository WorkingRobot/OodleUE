// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

// This file was generated by sketchy. DO NOT EDIT!
// Scheduling model: Apple M1
// vim:ts=4

#include "asmlib_arm_a64.inc"

		.text
		.globl MANGLE(oodle_newLZ_tans_a64_kern)
		.align 4
		FUNCTION_PRE_DECL(oodle_newLZ_tans_a64_kern)
MANGLE(oodle_newLZ_tans_a64_kern):
		.cfi_startproc
.L0:
		ldr		x2, [x0, #8]
		ldr		x3, [x0, #16]
		stp		x19, x20, [sp, #-80]!
		stp		x21, x22, [sp, #16]
		stp		x23, x24, [sp, #32]
		stp		x25, x26, [sp, #48]
		str		x27, [sp, #64]
		sub		x3, x3, x2								// nbytesleft = decend - decptr
		subs	xzr, x3, #9
		b.ls	.L1										// -> return true
.L2:
		ldp		x4, x8, [x0, #24]
		sub		x14, x8, x4								// in1 - in0
		subs	xzr, x14, #8
		b.cc	.L1										// -> return true
.L3:
		ldp		w12, w15, [x0, #56]
		ldp		w19, w22, [x0, #64]
		ldr		x1, [x0]								// table ptr
		ldr		w25, [x0, #72]
		ldp		w5, w9, [x0, #40]						// bits[0] / bits[1]
		ldp		w6, w10, [x0, #48]						// bitc[0] / bitc[1]
		sub		x3, x3, #9
		sub		x8, x8, #8
		ldr		x26, [x1, x25, lsl #3]					// TANS 4 table pre-load
		ldr		x13, [x1, x12, lsl #3]					// TANS 0 table pre-load
		ldr		x16, [x1, x15, lsl #3]					// TANS 1 table pre-load
		ldr		x20, [x1, x19, lsl #3]					// TANS 2 table pre-load
		ldr		x23, [x1, x22, lsl #3]					// TANS 3 table pre-load
.L4:
		subs	xzr, x4, x8
		b.hi	.L5										// -> Post-decode
		.align	4
.L6: // Main decode loop body
		ldr		x7, [x4]								// refill 0 load
		ldr		x11, [x8]								// refill 1 load
		ubfm	x14, x13, #32, #47						// TANS 0 len/sym
		ubfm	x17, x16, #32, #47						// TANS 1 len/sym
		ubfm	x21, x20, #32, #47						// TANS 2 len/sym
		ubfm	x24, x23, #32, #47						// TANS 3 len/sym
		ubfm	x27, x26, #32, #47						// TANS 4 len/sym
		prfum	#1, [x4, #128]							// refill 0 prefetch
		sturh	w27, [x2, #3]							// TANS 4 emit
		sturh	w24, [x2, #2]							// TANS 3 emit
		sturh	w21, [x2, #1]							// TANS 2 emit
		sturh	w17, [x2]								// TANS 1 emit
		add		x4, x4, #7								// refill 0 advance 1
		prfum	#1, [x8, #-128]							// refill 1 prefetch
		sub		x8, x8, #7								// refill 1 advance 1
		subs	x3, x3, #10								// (nbytesleft -= TANS_DECS_PER_ITER) > 0
		prfum	#16, [x2, #128]							// output prefetch
		lslv	x7, x7, x6								// refill 0 align
		rev		x11, x11								// refill 1 LE->BE
		orr		x5, x5, x7								// refill 0 insert
		ubfm	x7, x6, #3, #5							// refill 0 7-bytes_consumed
		orr		x6, x6, #56								// refill 0 new bitcount
		lslv	x11, x11, x10							// refill 1 align
		and		w12, w5, w13							// TANS 0 masked bits
		ubfm	x13, x13, #48, #63						// TANS 0 next_st
		lsrv	x5, x5, x14								// TANS 0 consume bits
		sub		x6, x6, x14								// TANS 0 update bitc
		rev16	w14, w14								// TANS 0 sym->low byte
		orr		x9, x9, x11								// refill 1 insert
		add		x12, x12, x13							// TANS 0 add next_st
		and		w15, w5, w16							// TANS 1 masked bits
		ubfm	x16, x16, #48, #63						// TANS 1 next_st
		lsrv	x5, x5, x17								// TANS 1 consume bits
		strb	w14, [x2]								// TANS 0 emit
		sub		x6, x6, x17								// TANS 1 update bitc
		ubfm	x11, x10, #3, #5						// refill 1 7-bytes_consumed
		ldr		x13, [x1, x12, lsl #3]					// TANS 0 table pre-load
		add		x15, x15, x16							// TANS 1 add next_st
		and		w19, w5, w20							// TANS 2 masked bits
		ubfm	x20, x20, #48, #63						// TANS 2 next_st
		lsrv	x5, x5, x21								// TANS 2 consume bits
		sub		x6, x6, x21								// TANS 2 update bitc
		orr		x10, x10, #56							// refill 1 new bitcount
		ldr		x16, [x1, x15, lsl #3]					// TANS 1 table pre-load
		add		x19, x19, x20							// TANS 2 add next_st
		and		w22, w5, w23							// TANS 3 masked bits
		ubfm	x23, x23, #48, #63						// TANS 3 next_st
		lsrv	x5, x5, x24								// TANS 3 consume bits
		sub		x6, x6, x24								// TANS 3 update bitc
		add		x8, x8, x11								// refill 1 advance 2
		ldr		x20, [x1, x19, lsl #3]					// TANS 2 table pre-load
		add		x22, x22, x23							// TANS 3 add next_st
		and		w25, w5, w26							// TANS 4 masked bits
		ubfm	x26, x26, #48, #63						// TANS 4 next_st
		lsrv	x5, x5, x27								// TANS 4 consume bits
		sub		x6, x6, x27								// TANS 4 update bitc
		sub		x4, x4, x7								// refill 0 advance 2
		ldr		x23, [x1, x22, lsl #3]					// TANS 3 table pre-load
		add		x25, x25, x26							// TANS 4 add next_st
		ccmp	x4, x8, #2, gt							// && in0 <= in1
		ldr		x26, [x1, x25, lsl #3]					// TANS 4 table pre-load
		ubfm	x14, x13, #32, #47						// TANS 0 len/sym
		and		w12, w9, w13							// TANS 0 masked bits
		ubfm	x13, x13, #48, #63						// TANS 0 next_st
		ubfm	x17, x16, #32, #47						// TANS 1 len/sym
		lsrv	x9, x9, x14								// TANS 0 consume bits
		sub		x10, x10, x14							// TANS 0 update bitc
		add		x12, x12, x13							// TANS 0 add next_st
		rev16	w14, w14								// TANS 0 sym->low byte
		ldr		x13, [x1, x12, lsl #3]					// TANS 0 table pre-load
		ubfm	x21, x20, #32, #47						// TANS 2 len/sym
		and		w15, w9, w16							// TANS 1 masked bits
		lsrv	x9, x9, x17								// TANS 1 consume bits
		sub		x10, x10, x17							// TANS 1 update bitc
		ubfm	x16, x16, #48, #63						// TANS 1 next_st
		ubfm	x24, x23, #32, #47						// TANS 3 len/sym
		and		w19, w9, w20							// TANS 2 masked bits
		lsrv	x9, x9, x21								// TANS 2 consume bits
		ubfm	x20, x20, #48, #63						// TANS 2 next_st
		sub		x10, x10, x21							// TANS 2 update bitc
		add		x15, x15, x16							// TANS 1 add next_st
		ldr		x16, [x1, x15, lsl #3]					// TANS 1 table pre-load
		and		w22, w9, w23							// TANS 3 masked bits
		lsrv	x9, x9, x24								// TANS 3 consume bits
		ubfm	x27, x26, #32, #47						// TANS 4 len/sym
		ubfm	x23, x23, #48, #63						// TANS 3 next_st
		add		x19, x19, x20							// TANS 2 add next_st
		sub		x10, x10, x24							// TANS 3 update bitc
		ldr		x20, [x1, x19, lsl #3]					// TANS 2 table pre-load
		and		w25, w9, w26							// TANS 4 masked bits
		ubfm	x26, x26, #48, #63						// TANS 4 next_st
		add		x22, x22, x23							// TANS 3 add next_st
		lsrv	x9, x9, x27								// TANS 4 consume bits
		sub		x10, x10, x27							// TANS 4 update bitc
		sturh	w27, [x2, #8]							// TANS 4 emit
		sturh	w24, [x2, #7]							// TANS 3 emit
		ldr		x23, [x1, x22, lsl #3]					// TANS 3 table pre-load
		add		x25, x25, x26							// TANS 4 add next_st
		sturh	w21, [x2, #6]							// TANS 2 emit
		sturh	w17, [x2, #5]							// TANS 1 emit
		strb	w14, [x2, #5]							// TANS 0 emit
		add		x2, x2, #10								// advance ptr
		ldr		x26, [x1, x25, lsl #3]					// TANS 4 table pre-load
		b.ls	.L6										// -> Main decode loop body
.L5: // Post-decode
		add		x8, x8, #8
		ubfm	x14, x6, #3, #5							// finish 0 remaining bytes
		ubfm	x17, x10, #3, #5						// finish 1 remaining bytes
		and		x6, x6, #7								// finish 0 leftover bitc
		and		x10, x10, #7							// finish 1 leftover bitc
		str		x2, [x0, #8]
		stp		w12, w15, [x0, #56]						// state0/state1
		stp		w19, w22, [x0, #64]						// state2/state3
		sub		x4, x4, x14								// finish 0 backtrack
		add		x8, x8, x17								// finish 1 backtrack
		str		w25, [x0, #72]							// state4
		stp		w5, w9, [x0, #40]						// store bits
		stp		x4, x8, [x0, #24]						// store bitp
		stp		w6, w10, [x0, #48]						// store bitc
.L1: // return true
		ldr		x27, [sp, #64]
		ldp		x25, x26, [sp, #48]
		ldp		x23, x24, [sp, #32]
		ldp		x21, x22, [sp, #16]
		mov		x0, #1									// return val=true
		ldp		x19, x20, [sp], #80
		ret		
		FUNCTION_POST_DECL(oodle_newLZ_tans_a64_kern)
		.cfi_endproc
