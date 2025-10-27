// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

#include "linalg.h"
#include <math.h>

OODLE_NS_START

bool LinAlg_CholeskyUTU(F32 * matrix, int dim)
{
	// The factorization we want is (for an example 3x3 matrix):
	//
	//   U^T U = A (where A symmetric)
	//
	// writing it out: (and omitting the symmetric parts of A
	// that are assumed to not be stored in the source)
	//
	//   |u11        | |u11 u12 u13|   |a11 a12 a13|
	//   |u12 u22    | |    u22 u23| = |    a22 a23|
	//   |u13 u23 u33| |        u33|   |(sym)   a33|
	//
	// We implement a row-wise factorization variant.
	//
	// Multiplying out yields: (first row of results)
	//
	//   u11 * u11 = a11  => u11 = sqrt(a11)
	//   u11 * u12 = a12  => u12 = a12 / u11
	//   u11 * u13 = a13  => u13 = a13 / u11
	//
	// Second row of results:
	//
	//   u12 * u12 + u22 * u22 = a22  => u22 = sqrt(a22 - u12*u12)
	//   u12 * u13 + u22 * u23 = a23  => u23 = (a23 - u12*u13) / u22
	//
	// which allows us to deduce the formulas used below.

	for (int i = 0; i < dim; i++)
	{
		// Diagonal entry:
		// Take diagonal entry a_ii, and subtract the squares of
		// all elements in the column above a_ii.
		F64 diag = matrix[i*dim + i];
		for (int j = 0; j < i; j++)
			diag -= (F64)matrix[j*dim + i] * (F64)matrix[j*dim + i];

		// If this is <=0, the matrix is not positive definite; bail.
		if (diag <= 0.0)
			return false;

		// Matrix is positive definite, take the square root and continue.
		diag = sqrt(diag);
		matrix[i*dim + i] = (F32)diag;

		// Rest of the row
		for (int j = i + 1; j < dim; j++)
		{
			// Take a_ij, subtract the dot product of the parts of
			// columns i and j above row i, and then divide by the
			// just-computed diagonal element for row i.
			F64 sum = matrix[i*dim + j];
			for (int k = 0; k < i; k++)
				sum -= (F64)matrix[k*dim + i] * (F64)matrix[k*dim + j];

			matrix[i*dim + j] = (F32)(sum / diag);
		}
	}

	return true;
}

void LinAlg_CholeskySolve(const F32 * matrix, int dim, F32 * b)
{
	// b = U^T U x = U^T (U x)
	// therefore, let
	//   U x = c
	// then first solve
	//   U^T c = b (forward substitution)
	// for c, then
	//   U x = c (backward substitution)
	// for x.
	
	// Forward substitution: U^T c = b.
	// Writing it out:
	//
	//   |u11        | |c1|   |b1|
	//   |u12 u22    | |c2| = |b2|
	//   |u13 u23 u33| |c3|   |b3|
	//
	// so
	//
	//   u11 c1 = b1                   => c1 = b1 / u11
	//   u12 c1 + u22 c2 = b2          => c2 = (b2 - u12 c1) / u22
	//   u13 c1 + u23 c2 + u33 c3 = b3 => c3 = (b3 - u13 c1 - u23 c2) / u33
	//
	// and so forth.
	for (int i = 0; i < dim; i++)
	{
		F64 sum = b[i];
		for (int j = 0; j < i; j++)
			sum -= (F64)b[j] * (F64)matrix[j*dim + i];

		b[i] = (F32)(sum / matrix[i*dim + i]);
	}

	// Backward substitution: U x = c.
	//
	// Again, written out:
	// 
	//   |u11 u12 u13| |x1| = |c1|
	//   |    u22 u23| |x2| = |c2|
	//   |        u33| |x3| = |c3|
	//
	// so
	//
	//   u33 x3 = c3                   => x3 = c3 / u33
	//   u22 x2 + u23 x3 = c2          => x2 = (c2 - u23 x3) / u22
	//   u11 x1 + u12 x2 + u13 x3 = c1 => x1 = (c1 - u12 x2 - u13 x3) / u11
	for (int i = dim - 1; i >= 0; i--)
	{
		F64 sum = b[i];
		for (int j = i + 1; j < dim; j++)
			sum -= (F64)b[j] * (F64)matrix[i*dim + j];

		b[i] = (F32)(sum / matrix[i*dim + i]);
	}
}

template <typename T> static inline T sqr(T t) { return t * t; }
template <typename T> static inline void swap(T & a, T & b) { T c = a; a = b; b = c; }

// Perform rotation of M_ij, M_kl:
//     ┌    ┐   ┌     ┐┌    ┐
//     │M_kl│   │c  −s││M_kl│
//     │    │ = │     ││    │
//     │M_ij│   │s   c││M_ij│
//     └    ┘   └     ┘└    ┘
// where x and y are the indices of elements M_kl and M_ij in M.
static inline void Jacobi_Rotate(F32 * M, int x, int y, F32 c, F32 s)
{
	F32 M_x = M[x];
	F32 M_y = M[y];
	M[x] = c*M_x - s*M_y;
	M[y] = c*M_y + s*M_x;
}

int LinAlg_Jacobi(F32 * evals, F32 * evecs, const F32 * A_in, int dim, JacobiSort sort)
{
	F32 * A = new F32[dim*dim];
	memcpy(A, A_in, dim*dim*sizeof(F32));

	for (int i = 0; i < dim; ++i)
		evals[i] = A[i * dim + i];

	if (evecs)
		for (int i = 0; i < dim; ++i)
			for (int j = 0; j < dim; ++j)
				evecs[i * dim + j] = (i == j) ? 1.0f : 0.0f;

	int changed, rotations = 0;
	do {
		changed = 0;
		for (int p = 0; p < dim; ++p)
			for (int q = p + 1; q < dim; ++q)
			{
				// Calculate rotation
				// Details for the follwing process can be found in http://iqcc.udg.edu/~vybo/DOCENCIA/PROGRA/Diagonalization.pdf
				F32 App = evals[p];
				F32 Aqq = evals[q];
				F32 Apq = A[p*dim + q];

				F32 t = 1; // = tan(θ)
				F32 theta = Aqq - App;
				if (theta != 0.0)
				{
					t = 0.0;
					if (Apq != 0.0)
					{
						theta /= (2 * Apq);
						// t satisfies: t^2 + 2*t*theta - 1 = 0
						// (choose the root which has the smaller absolute value)
						t = 1.0f / (sqrtf(1 + sqr(theta)) + fabsf(theta));
						if (theta < 0.0f)
							t = -t;
					}
				}
				F32 c = 1.0f / sqrtf(1 + sqr(t));
				F32 s = c*t;
				F32 App1 = App - t * Apq;
				F32 Aqq1 = Aqq + t * Apq;

				if (App1 != App || Aqq1 != Aqq)
				{
					changed = 1;
					rotations++;
					evals[p] = App1;
					evals[q] = Aqq1;
					A[p*dim + q] = 0.0f;
					for (int i = 0; i < p; ++i)
						Jacobi_Rotate(A, i*dim + p, i*dim + q, c, s);

					for (int i = p+1; i < q ; ++i)
						Jacobi_Rotate(A, p*dim + i, i*dim + q, c, s);

					for (int i = q+1; i < dim; ++i)
						Jacobi_Rotate(A, p*dim + i, q*dim + i, c, s);

					if (evecs)
						for (int i = 0; i < dim; ++i)
							Jacobi_Rotate(evecs, i*dim + p, i*dim + q, c, s);
				}
			}
	}
	while (changed != 0);

	delete [] A;

	if (sort != JACOBI_NO_SORT)
	{
		for (int i = 0; i < dim-1; ++i)
		{
			int m = i;
			for (int j = i+1; j < dim; ++j)
				if (fabsf(evals[j]) > fabsf(evals[m]))
					m = j;
			if (i != m)
			{
				swap(evals[i], evals[m]);
				for (int j = 0; j < dim; ++j)
					swap(evecs[j*dim + i], evecs[j*dim + m]);
			}
		}
	}

	return rotations;
}

OODLE_NS_END

