// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "rrfixedfloat.h"

EXPORT_SOME_CRAP(rrfixedfloat);

/*******

static __declspec(align(16)) unsigned int abs[4]      = {0x7FFFFFFF, 0x7FFFFFFF, 0x7FFFFFFF, 0x7FFFFFFF};
static __declspec(align(16)) unsigned int infinity[4] = {0x47FFEFFF, 0x47FFEFFF, 0x47FFEFFF, 0x47FFEFFF};
static __declspec(align(16)) unsigned int denormal[4] = {0x38800000, 0x38800000, 0x38800000, 0x38800000};
static __declspec(align(16)) unsigned int fixup[4]    = {0x48000000, 0x48000000, 0x48000000, 0x48000000};
static __declspec(align(16)) unsigned int round1[4]   = {0x00000001, 0x00000001, 0x00000001, 0x00000001};
static __declspec(align(16)) unsigned int round2[4]   = {0x00000FFF, 0x00000FFF, 0x00000FFF, 0x00000FFF};
static __declspec(align(16)) unsigned int sign[4]     = {0x80000000, 0x80000000, 0x80000000, 0x80000000};
static __declspec(align(16)) unsigned int base[4]     = {0x00007FFF, 0x00007FFF, 0x00007FFF, 0x00007FFF};
static __declspec(align(16)) unsigned int integer[4]  = {0x52000000, 0x52000000, 0x52000000, 0x52000000};

__asm
{
    movaps	xmm1, xmm0   // Input in xmm0

    // Compute masks
    andps	xmm0, abs
    movaps	xmm2, xmm0
    movaps	xmm3, xmm0
    cmpnltps	xmm2, infinity
    cmpltps	xmm3, denormal

    // Denormal case
    movaps	xmm6, xmm0
    mulps	xmm6, integer
    cvttps2dq	xmm6, xmm6

    // Normal case and combine
    paddd	xmm0, fixup
    andps	xmm6, xmm3
    andnps	xmm3, xmm0
    orps	xmm6, xmm3

    // Correct rounding
    movaps	xmm0, xmm6
    psrld	xmm0, 13
    andps	xmm0, round1
    paddd	xmm0, round2
    paddd	xmm0, xmm6

    // Combine with sign and infinity
    psrld	xmm0, 13
    andps	xmm1, sign
    psrld	xmm1, 16
    orps	xmm0, xmm2
    andps	xmm0, base
    orps	xmm0, xmm1   // Result in lower words of each element
}

***************/

/*******

static __declspec(align(16)) unsigned int abs[4]      = {0x7FFFFFFF, 0x7FFFFFFF, 0x7FFFFFFF, 0x7FFFFFFF};
static __declspec(align(16)) unsigned int infinity[4] = {0x47FFEFFF, 0x47FFEFFF, 0x47FFEFFF, 0x47FFEFFF};
static __declspec(align(16)) unsigned int denormal[4] = {0x38800000, 0x38800000, 0x38800000, 0x38800000};
static __declspec(align(16)) unsigned int mantissa[4] = {0x007FFFFF, 0x007FFFFF, 0x007FFFFF, 0x007FFFFF};
static __declspec(align(16)) unsigned int oneDot[4]   = {0x00800000, 0x00800000, 0x00800000, 0x00800000};
static __declspec(align(16)) unsigned int exponent[4] = {0x7F800000, 0x7F800000, 0x7F800000, 0x7F800000};
static __declspec(align(16)) unsigned int fixup[4]    = {0x48000000, 0x48000000, 0x48000000, 0x48000000};
static __declspec(align(16)) unsigned int round1[4]   = {0x00000001, 0x00000001, 0x00000001, 0x00000001};
static __declspec(align(16)) unsigned int round2[4]   = {0x00000FFF, 0x00000FFF, 0x00000FFF, 0x00000FFF};
static __declspec(align(16)) unsigned int sign[4]     = {0x80000000, 0x80000000, 0x80000000, 0x80000000};
static __declspec(align(16)) unsigned int base[4]     = {0x00007FFF, 0x00007FFF, 0x00007FFF, 0x00007FFF};
static __declspec(align(16)) unsigned int adjust[4]   = {0x07000000, 0x07000000, 0x07000000, 0x07000000};

__asm
{
    movaps	xmm1, xmm0   // Input in xmm0

    // Compute masks
    andps	xmm0, abs
    movaps	xmm2, xmm0
    movaps	xmm3, xmm0
    cmpnltps	xmm2, infinity
    cmpltps	xmm3, denormal

    // Denormal case
    movaps	xmm4, xmm0
    andps	xmm4, exponent
    paddd	xmm4, adjust
    movaps	xmm6, xmm0
    andps	xmm6, mantissa
    orps	xmm6, oneDot
    cvtdq2ps	xmm6, xmm6
    mulps	xmm6, xmm4
    cvttps2dq	xmm6, xmm6

    // Normal case and combine
    paddd	xmm0, fixup
    andps	xmm6, xmm3
    andnps	xmm3, xmm0
    orps	xmm6, xmm3

    // Correct rounding
    movaps	xmm0, xmm6
    psrld	xmm0, 13
    andps	xmm0, round1
    paddd	xmm0, round2
    paddd	xmm0, xmm6

    // Combine with sign and infinity
    psrld	xmm0, 13
    andps	xmm1, sign
    psrld	xmm1, 16
    orps	xmm0, xmm2
    andps	xmm0, base
    orps	xmm0, xmm1   // Result in lower words of each element
}

***************************/
