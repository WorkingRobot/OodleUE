// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#pragma once

RR_NAMESPACE_START

template< typename t_functor >	
double GoldenSearch1d( t_functor & func, double lo, double v_lo, double m1, double v_m1, double hi, double v_hi, double minstep )
{
	RR_ASSERT( minstep > 0 );
	
	const double rho = 0.381966;
	const double irho = 1.0 - rho; // = (sqrt(5)-1)/2 

	// four points :
	// [lo,m1,m2,hi]

	RR_DURING_ASSERT( double check_m1 = irho*lo + rho*hi );
	RR_ASSERT( fequal(m1,check_m1,1e-5) );
	double m2 = irho*hi + rho*lo;

	double v_m2 = func( m2 );
	
	while( (m1-lo) > minstep )
	{
		// step to [lo,m1,m2] or [m1,m2,hi]
		// only one func eval per iteration :
		if ( RR_MIN(v_lo,v_m1) < RR_MIN(v_hi,v_m2) )
		{
			hi = m2; v_hi = v_m2;
			m2 = m1; v_m2 = v_m1;
			m1 = irho*lo + rho*hi;
			v_m1 = func( m1 );
		}
		else
		{
			lo = m1; v_lo = v_m1;
			m1 = m2; v_m1 = v_m2;
			m2 = irho*hi + rho*lo;
			v_m2 = func( m2 );
		}
		
		RR_ASSERT( fequal(m2, irho*hi + rho*lo,1e-5) );
		RR_ASSERT( fequal(m1, irho*lo + rho*hi,1e-5) );
	}
	
	// could pick the better triple and use QuadraticExtremum here
	
	/*
	//return (lo+hi)/2.0;
	
	if ( v_m1 < v_m2 ) return m1;
	else return m2;
	
	/*/
	// return best of the 4 samples :
	if ( v_lo < v_m1 ) { v_m1 = v_lo; m1 = lo; }
	if ( v_hi < v_m2 ) { v_m2 = v_hi; m2 = hi; }
	
	if ( v_m1 < v_m2 ) return m1;
	else return m2;
	/**/
}

template< typename t_functor >	
double GoldenSearch1d( t_functor & func, double lo, double v_lo, double hi, double v_hi, double minstep )
{
	const double rho = 0.381966;
	const double irho = 1.0 - rho; // = (sqrt(5)-1)/2 

	double m1 = irho*lo + rho*hi;
	double v_m1 = func( m1 );
	
	return GoldenSearch1d(func,lo,v_lo,m1,v_m1,hi,v_hi,minstep);
}

template< typename t_functor >	
double GoldenSearch1d( t_functor & func, double lo, double hi, double minstep )
{
	double v_lo = func( lo );
	double v_hi = func( hi );
	
	return GoldenSearch1d( func, lo, v_lo, hi, v_hi, minstep );
}

RR_NAMESPACE_END

