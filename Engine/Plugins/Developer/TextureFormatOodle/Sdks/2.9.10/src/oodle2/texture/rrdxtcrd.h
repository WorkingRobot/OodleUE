// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

#include "rrsurface.h"
#include "rrdxtcenums.h"

RR_NAMESPACE_START

struct BlockSurface;
struct OodleTex_Layout;

//--------------------------------------------

enum EDXTCRD_Metric
{
	eDXTCRD_Metric_Min=1,
	eDXTCRD_Metric_RMSE_RGBA=1,
	eDXTCRD_Metric_Perceptual_RGBA=2,
	eDXTCRD_Metric_Max=2,
	//eDXTCRD_Metric_Perceptual_RGB_A=3,
	//eDXTCRD_Metric_Perceptual_R_G_B_A=4,
	//eDXTCRD_Metric_Max=4,
	eDXTCRD_Metric_Perceptual_InternalLegacyDefault=eDXTCRD_Metric_Perceptual_RGBA,
		// eDXTCRD_Metric_Perceptual_Default is the legacy/internal default
		//	not the public API default
};

//--------------------------------------------

void rrDXTCRD_SetDefaults(rrDXTCRD_Options * rdopts);

// rrDXTCRD_Encode_RDO : encode baseline then ReadBaseline
//	called by public API
bool rrDXTCRD_Encode_RDO(BlockSurface * to_blocks,const BlockSurface * from_blocks,
				const rrSurface * from_surfaces, int num_from_surfaces, const OodleTex_Layout * layout,
				int lambda,rrDXTCOptions options,void * jobify_user_ptr,int num_workers,EDXTCRD_Metric metric,
				const rrDXTCRD_Options &rdopts);

bool rrDXTCRD_Encode_RDO_LinearSurface(rrSurface * to_surface,
				const rrSurface * from_surface,
				int lambda,rrDXTCOptions options,void * jobify_user_ptr,int num_workers,EDXTCRD_Metric metric,
				const rrDXTCRD_Options &rdopts);

//--------------------------------------------
// make an RDO Context for the source image that can be used repeatedly
//	then encode with _ReadContext to various lambdas

struct rrDXTCRD_Context;		
rrDXTCRD_Context * rrDXTCRD_Context_New();
void rrDXTCRD_Context_Delete(rrDXTCRD_Context * ctx);

rrbool rrDXTCRD_Context_Init(rrDXTCRD_Context * ctx,
				rrPixelFormat to_pf,
				const BlockSurface * from_blocks, const rrSurface * from_surfaces,int num_from_surfaces, const OodleTex_Layout * layout,
				rrDXTCOptions options,void * jobify_user_ptr,int num_workers,
				EDXTCRD_Metric metric,const rrDXTCRD_Options &rdopts);

bool rrDXTCRD_Encode_RDO_ReadContext_Ex(
	const rrDXTCRD_Context * ctx,
	BlockSurface * to_blocks,
	int lambda,const rrDXTCRD_Options &rdopts,
	void * jobify_user_ptr, int num_workers);
bool rrDXTCRD_Encode_RDO_ReadContext(
	const rrDXTCRD_Context * ctx,
	BlockSurface * to_blocks,
	int lambda,void * jobify_user_ptr, int num_workers);

const BlockSurface * rrDXTCRD_Context_GetActivity(rrDXTCRD_Context * ctx);
const BlockSurface * rrDXTCRD_Context_GetBaseline(rrDXTCRD_Context * ctx);
const BlockSurface * rrDXTCRD_Context_GetFrom(rrDXTCRD_Context * ctx);
rrDXTCOptions rrDXTCRD_Context_GetOptions(rrDXTCRD_Context * ctx);

//--------------------------------------------

RR_NAMESPACE_END
