// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.


RR_NAMESPACE_START

typedef U16 Color565;

static inline rrColor565Bits Quantize( const rrColor32BGRA & color )
{
	return rrColor565Bits_Quantize(color);
}

static inline rrColor565Bits ToUnion( const Color565 & w )
{
	rrColor565Bits ret;
	ret.w = w;
	return ret;
}

#define DeltaSqrRGB rrColor32BGRA_DeltaSqrRGB
#define DeltaSqrRGBA rrColor32BGRA_DeltaSqrRGBA

RR_NAMESPACE_END


