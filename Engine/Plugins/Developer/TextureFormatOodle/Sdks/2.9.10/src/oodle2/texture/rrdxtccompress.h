// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#ifndef __RADRRBITMAP_DXTC_COMPRESS_H__
#define __RADRRBITMAP_DXTC_COMPRESS_H__

#include "rrcolor.h"
#include "rrdxtcblock.h"
#include "rrdxtcenums.h"
#include "rrvecc.h"
#include "cpudispatch.h"

RR_NAMESPACE_START

//-------------------------------------------------------------------------------------------

void rrCompressDXT1Block(rrDXT1Block * pBlock,const rrColorBlock4x4 & colors, rrDXTCLevel level, rrDXTCOptions options, bool isBC23ColorBlock);

//-------------------------------------------------------------------------------------------

#define RR_DXTC_INIT_ERROR	RR_DXTC_ERROR_BIG

// DXTC compress functions replace the value of *pBlock if *pError can be improved
// *pError is SSD

void rrCompressDXT1_0(rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXTCOptions options, rrDXT1PaletteMode mode);
void rrCompressDXT1_1(rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXTCOptions options, rrDXT1PaletteMode mode);
void rrCompressDXT1_2(rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXTCOptions options, rrDXT1PaletteMode mode);
void rrCompressDXT1_3(rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXTCOptions options, rrDXT1PaletteMode mode);

//void DXT1_AnnealBlock(  rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXT1PaletteMode mode);
// DXT1_GreedyOptimizeBlock should be done after DXT1_AnnealBlock
//void DXT1_GreedyOptimizeBlock(rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXT1PaletteMode mode, bool do_joint_optimization);

class BC1EndpointLLSSolver
{
public:
	BC1EndpointLLSSolver(bool fourc);

	void accumulate(const rrColorBlock4x4 & colors, U32 indices);

	// Solves for endpoints A/B.
	// Returns true if the LLS system has a (unique) solution
	bool solve_endpoints(rrColor565Bits * pA, rrColor565Bits * pB) const;

private:
	// Terms of A^T A matrix
	int AA, AB, BB;

	// Dot products of data with weights
	// [3] is junk (kept for SIMD)
	F32 AX[4], BX[4];

	// Mode flags
	bool fourc;
	float normalization_factor;
};

// DXT1_OptimizeEndPointsFromIndices_Inherit_Reindex : read indices from pBlock, keep same fource state, find endpoints then reindex
bool DXT1_OptimizeEndPointsFromIndices_Inherit_Reindex(CpuDispatchFlags dispatch, rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXT1PaletteMode mode);
bool DXT1_OptimizeEndPointsFromIndices_Inherit_NoReindex(CpuDispatchFlags dispatch, rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXT1PaletteMode mode);

// DXT1_OptimizeEndPointsFromIndicesIterative : do DXT1_OptimizeEndPointsFromIndices_Inherit_Reindex while improving
void DXT1_OptimizeEndPointsFromIndicesIterative(CpuDispatchFlags dispatch, rrDXT1Block * pBlock,U32 * pError, const rrColorBlock4x4 & colors, rrDXT1PaletteMode mode);

//-------------------------------------------------------------------------------------------

RR_NAMESPACE_END

#endif // __RADRRBITMAP_DXTC_COMPRESS_H__
