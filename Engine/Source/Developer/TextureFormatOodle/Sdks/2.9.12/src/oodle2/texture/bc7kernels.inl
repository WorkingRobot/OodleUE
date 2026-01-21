// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

OODLE_NS_START

namespace internal {

// Number of iterations in the power iteration for subset PCA.
// In practice there seems to be very little value in using more than 3.
// NOTE: if you increase this, need to watch value ranges; need to
// rescale every few iterations to avoid blow-up.
static constexpr int SUBSET_PCA_NITER = 3;

static inline F32 * subset_chan(F32 * base, BC7SubsetChan channel, SINTa stride)
{
	return &base[static_cast<SINTa>(channel) * stride];
}

// ---- RGB version

template<typename T>
T cov_max_norm3(T diag, T a, T b)
{
	return diag.max(a.abs()).max(b.abs());
}

// Cov matrix elements along diagonals. Array indices:
//
// |0  3  5|
// |3  1  4|
// |5  4  2|
template<typename T>
void covar_times_vec3(T out_vec[3], const T cov[6], const T in_vec[3])
{
	T vx = in_vec[0];
	T vy = in_vec[1];
	T vz = in_vec[2];

	out_vec[0] = cov[0]*vx + cov[3]*vy + cov[5]*vz;
	out_vec[1] = cov[1]*vy + cov[4]*vz + cov[3]*vx;
	out_vec[2] = cov[2]*vz + cov[5]*vx + cov[4]*vy;
}

template<typename T>
struct SubsetCovarCalcRGB
{
	static constexpr int NPIXELS = 16;
	using VecInt = typename T::VecInt;
	using VecFlt = typename T::VecFlt;

	VecInt pix_stats[9][NPIXELS]; // 3 sums, 6 sums-of-products for covars

	void init(const U8 pixels[])
	{
		for (SINTa i = 0; i < NPIXELS; ++i)
		{
			VecInt r { pixels[i*4 + 0] };
			VecInt g { pixels[i*4 + 1] };
			VecInt b { pixels[i*4 + 2] };

			pix_stats[0][i] = r;
			pix_stats[1][i] = g;
			pix_stats[2][i] = b;
			pix_stats[3][i] = r * r;
			pix_stats[4][i] = g * g;
			pix_stats[5][i] = b * b;
			pix_stats[6][i] = r * g;
			pix_stats[7][i] = g * b;
			pix_stats[8][i] = b * r;
		}
	}

	void query(VecInt sumi[9], const U32 * subset_masks)
	{
		for (SINTa i = 0; i < 9; ++i)
			sumi[i] = VecInt::zero();

		VecInt subset_mask = VecInt::loadu(subset_masks);
		for (SINTa j = 0; j < NPIXELS; ++j)
		{
			VecInt mask = VecInt::zero() - (subset_mask & VecInt(1)); // ~0 if mask bit 0 is set, 0 otherwise
			for (SINTa i = 0; i < 9; ++i)
				sumi[i] += pix_stats[i][j] & mask;

			subset_mask = subset_mask.template srl<1>();
		}
	}
};

template<typename T>
RADFORCEINLINE void subset_prepare_finalize_rgb(F32 * results, const typename T::VecInt sumi[9], const F32 subset_scale[], SINTa stride)
{
	using VecInt = typename T::VecInt;
	using VecFlt = typename T::VecFlt;

	// Scaled mean; real mean = sum / count = sum * sf, but that scaling by count (number of pixels in the subset)
	// is actually what we want for the error metrics anyway.
	VecFlt sf = VecFlt::loadu(subset_scale);
	VecFlt smean_r = VecFlt::from_int32(sumi[0]);
	VecFlt smean_g = VecFlt::from_int32(sumi[1]);
	VecFlt smean_b = VecFlt::from_int32(sumi[2]);

	// Regular cov = covi * sf - mean * mean^T, but again we actually want things scaled by number of pixels in
	// the subset, so we get
	//   cov' = covi - sf * smean * smean^T
	//
	// Cov matrix elements along diagonals. Array indices:
	//
	// |0  3  5|
	// |3  1  4|
	// |5  4  2|
	VecFlt cov[6];
	cov[0] = VecFlt::from_int32(sumi[3]) - sf * (smean_r * smean_r);
	cov[1] = VecFlt::from_int32(sumi[4]) - sf * (smean_g * smean_g);
	cov[2] = VecFlt::from_int32(sumi[5]) - sf * (smean_b * smean_b);

	cov[3] = VecFlt::from_int32(sumi[6]) - sf * (smean_r * smean_g);
	cov[4] = VecFlt::from_int32(sumi[7]) - sf * (smean_g * smean_b);
	cov[5] = VecFlt::from_int32(sumi[8]) - sf * (smean_b * smean_r);

	// Start iterating from whichever basis vector has the largest
	// max norm under multiplication by cov
	VecFlt v[3] { cov[0], cov[3], cov[5] };
	VecFlt v_norm = cov_max_norm3(cov[0], cov[3], cov[5]);

	VecFlt v1_norm = cov_max_norm3(cov[1], cov[4], cov[3]);
	VecFlt v1_larger = v1_norm.cmp_gt(v_norm);
	v[0] = v1_larger.select(cov[3], v[0]);
	v[1] = v1_larger.select(cov[1], v[1]);
	v[2] = v1_larger.select(cov[4], v[2]);
	v_norm = v_norm.max(v1_norm);

	VecFlt v2_norm = cov_max_norm3(cov[2], cov[5], cov[4]);
	VecFlt v2_larger = v2_norm.cmp_gt(v_norm);
	v[0] = v2_larger.select(cov[5], v[0]);
	v[1] = v2_larger.select(cov[4], v[1]);
	v[2] = v2_larger.select(cov[2], v[2]);

	// Do a few more iterations
	for (SINTa j = 0; j < SUBSET_PCA_NITER; ++j)
		covar_times_vec3(v, cov, v);

	// Pre-scale before normalize to avoid overflow
	VecFlt overall_max = v[0].abs().max(v[1].abs()).max(v[2].abs());
	VecFlt prescale = overall_max.cmp_gt(VecFlt { 0x1p+60f }).select(VecFlt { 0x1p-68f }, VecFlt { 1.0f });
	v[0] *= prescale;
	v[1] *= prescale;
	v[2] *= prescale;

	// Compute squared len
	VecFlt len_sq = v[0]*v[0] + v[1]*v[1] + v[2]*v[2];

	// Normalize vector, but if tiny, just make it 0 instead
	VecFlt rcp_len = (VecFlt(1.0f) / len_sq).sqrt();
	rcp_len &= len_sq.cmp_gt(VecFlt(0x1p-60));

	v[0] *= rcp_len;
	v[1] *= rcp_len;
	v[2] *= rcp_len;

	// Overall energy in subset is determined by trace of covariance
	VecFlt overall_energy = cov[0] + cov[1] + cov[2];

	// Fraction of covariance captured by PCA direction is given by Rayleigh coefficient
	VecFlt vlast[3];
	covar_times_vec3(vlast, cov, v);
	VecFlt pca_energy = v[0]*vlast[0] + v[1]*vlast[1] + v[2]*vlast[2];

	// Store results
	overall_energy.storeu(subset_chan(results, BC7SubsetChan::OverallErr, stride));
	pca_energy.storeu(subset_chan(results, BC7SubsetChan::PcaErr, stride));

	v[0].storeu(subset_chan(results, BC7SubsetChan::PcaR, stride));
	v[1].storeu(subset_chan(results, BC7SubsetChan::PcaG, stride));
	v[2].storeu(subset_chan(results, BC7SubsetChan::PcaB, stride));
	VecFlt::zero().storeu(subset_chan(results, BC7SubsetChan::PcaA, stride));
}

template<typename T>
void subset_prepare_rgb(F32 * results, const U8 pixels[], const U32 subset_masks[], const F32 subset_scale[], SINTa subset_count, SINTa stride)
{
	using VecInt = typename T::VecInt;

	SubsetCovarCalcRGB<T> covar;
	covar.init(pixels);

	// Process subsets
	for (SINTa i = 0; i < subset_count; i += T::LANES)
	{
		// Determine summary sums for this subset
		VecInt sumi[9];
		covar.query(sumi, &subset_masks[i]);

		subset_prepare_finalize_rgb<T>(&results[i], sumi, &subset_scale[i], stride);
	}
}

// ---- RGBA version

// Cov matrix elements along diagonals (with wrap-around). Array indices:
//
// |0  4  8  7|
// |4  1  5  9|
// |8  5  2  6|
// |7  9  6  3|
template<typename T>
T cov_max_norm4(T diag, T a, T b, T c)
{
	return diag.max(a.abs()).max(b.abs()).max(c.abs());
}

template<typename T>
void covar_times_vec4(T out_vec[4], const T cov[10], const T in_vec[4])
{
	T vx = in_vec[0];
	T vy = in_vec[1];
	T vz = in_vec[2];
	T vw = in_vec[3];

	out_vec[0] = cov[0]*vx + cov[4]*vy + cov[8]*vz + cov[7]*vw;
	out_vec[1] = cov[1]*vy + cov[5]*vz + cov[9]*vw + cov[4]*vx;
	out_vec[2] = cov[2]*vz + cov[6]*vw + cov[8]*vx + cov[5]*vy;
	out_vec[3] = cov[3]*vw + cov[7]*vx + cov[9]*vy + cov[6]*vz;
}

template<typename T>
struct SubsetCovarCalcRGBA
{
	static constexpr int NPIXELS = 16;
	using VecInt = typename T::VecInt;

	VecInt pix_stats[14][NPIXELS]; // 4 sums, 10 sums-of-products for covars
	//VecInt sum_all[14];

	void init(const U8 pixels[])
	{
		//for (SINTa i = 0; i < 14; ++i)
		//	sum_all[i] = VecInt::zero();

		for (SINTa i = 0; i < NPIXELS; ++i)
		{
			VecInt r { pixels[i*4 + 0] };
			VecInt g { pixels[i*4 + 1] };
			VecInt b { pixels[i*4 + 2] };
			VecInt a { pixels[i*4 + 3] };

			pix_stats[ 0][i] = r;
			pix_stats[ 1][i] = g;
			pix_stats[ 2][i] = b;
			pix_stats[ 3][i] = a;

			pix_stats[ 4][i] = r * r;
			pix_stats[ 5][i] = g * g;
			pix_stats[ 6][i] = b * b;
			pix_stats[ 7][i] = a * a;

			pix_stats[ 8][i] = r * g;
			pix_stats[ 9][i] = g * b;
			pix_stats[10][i] = b * a;
			pix_stats[11][i] = a * r;

			pix_stats[12][i] = r * b;
			pix_stats[13][i] = g * a;

			//for (SINTa j = 0; j < 14; ++j)
			//	sum_all[j] += pix_stats[j][i];
		}
	}

	void query(VecInt sumi[14], const U32 * subset_masks)
	{
		for (SINTa k = 0; k < 14; ++k)
			sumi[k] = VecInt::zero();

		VecInt subset_mask = VecInt::loadu(subset_masks);
		for (SINTa j = 0; j < NPIXELS; ++j)
		{
			VecInt mask = VecInt::zero() - (subset_mask & VecInt(1)); // ~0 if mask bit 0 is set, 0 otherwise
			for (SINTa k = 0; k < 14; ++k)
				sumi[k] += pix_stats[k][j] & mask;

			subset_mask = subset_mask.template srl<1>();
		}
	}

	//void complement(VecInt sumi[14])
	//{
	//	for (SINTa i = 0; i < 14; ++i)
	//		sumi[i] = sum_all[i] - sumi[i];
	//}
};

template<typename T>
RADFORCEINLINE void subset_prepare_finalize_rgba(F32 * results, const typename T::VecInt sumi[14], const F32 subset_scale[], SINTa stride)
{
	using VecInt = typename T::VecInt;
	using VecFlt = typename T::VecFlt;

	// Scaled mean; real mean = sum / count = sum * sf, but that scaling by count (number of pixels in the subset)
	// is actually what we want for the error metrics anyway.
	VecFlt sf = VecFlt::loadu(subset_scale);
	VecFlt smean_r = VecFlt::from_int32(sumi[0]);
	VecFlt smean_g = VecFlt::from_int32(sumi[1]);
	VecFlt smean_b = VecFlt::from_int32(sumi[2]);
	VecFlt smean_a = VecFlt::from_int32(sumi[3]);

	// Regular cov = covi * sf - mean * mean^T, but again we actually want things scaled by number of pixels in
	// the subset, so we get
	//   cov' = covi - sf * smean * smean^T
	//
	// Cov matrix elements along diagonals (with wrap-around). Array indices:
	//
	// |0  4  8  7|
	// |4  1  5  9|
	// |8  5  2  6|
	// |7  9  6  3|
	VecFlt cov[10];
	cov[0] = VecFlt::from_int32(sumi[ 4]) - sf * (smean_r * smean_r);
	cov[1] = VecFlt::from_int32(sumi[ 5]) - sf * (smean_g * smean_g);
	cov[2] = VecFlt::from_int32(sumi[ 6]) - sf * (smean_b * smean_b);
	cov[3] = VecFlt::from_int32(sumi[ 7]) - sf * (smean_a * smean_a);

	cov[4] = VecFlt::from_int32(sumi[ 8]) - sf * (smean_r * smean_g);
	cov[5] = VecFlt::from_int32(sumi[ 9]) - sf * (smean_g * smean_b);
	cov[6] = VecFlt::from_int32(sumi[10]) - sf * (smean_b * smean_a);
	cov[7] = VecFlt::from_int32(sumi[11]) - sf * (smean_a * smean_r);

	cov[8] = VecFlt::from_int32(sumi[12]) - sf * (smean_r * smean_b);
	cov[9] = VecFlt::from_int32(sumi[13]) - sf * (smean_g * smean_a);

	// Start iterating from whichever basis vector has the largest
	// max norm under multiplication by cov
	VecFlt v[4] { cov[0], cov[4], cov[8], cov[7] };
	VecFlt v_norm = cov_max_norm4(cov[0], cov[4], cov[8], cov[7]);

	VecFlt v1_norm = cov_max_norm4(cov[1], cov[5], cov[9], cov[4]);
	VecFlt v1_larger = v1_norm.cmp_gt(v_norm);
	v[0] = v1_larger.select(cov[4], v[0]);
	v[1] = v1_larger.select(cov[1], v[1]);
	v[2] = v1_larger.select(cov[5], v[2]);
	v[3] = v1_larger.select(cov[9], v[3]);
	v_norm = v_norm.max(v1_norm);

	VecFlt v2_norm = cov_max_norm4(cov[2], cov[6], cov[8], cov[5]);
	VecFlt v2_larger = v2_norm.cmp_gt(v_norm);
	v[0] = v2_larger.select(cov[8], v[0]);
	v[1] = v2_larger.select(cov[5], v[1]);
	v[2] = v2_larger.select(cov[2], v[2]);
	v[3] = v2_larger.select(cov[6], v[3]);
	v_norm = v_norm.max(v2_norm);

	VecFlt v3_norm = cov_max_norm4(cov[3], cov[7], cov[9], cov[6]);
	VecFlt v3_larger = v3_norm.cmp_gt(v_norm);
	v[0] = v3_larger.select(cov[7], v[0]);
	v[1] = v3_larger.select(cov[9], v[1]);
	v[2] = v3_larger.select(cov[6], v[2]);
	v[3] = v3_larger.select(cov[3], v[3]);

	// Do a few more iterations
	for (SINTa j = 0; j < SUBSET_PCA_NITER; ++j)
		covar_times_vec4(v, cov, v);

	// Pre-scale before normalize to avoid overflow
	VecFlt overall_max = v[0].abs().max(v[1].abs()).max(v[2].abs()).max(v[3].abs());
	VecFlt prescale = overall_max.cmp_gt(VecFlt { 0x1p+60f }).select(VecFlt { 0x1p-68f }, VecFlt { 1.0f });
	v[0] *= prescale;
	v[1] *= prescale;
	v[2] *= prescale;
	v[3] *= prescale;

	// Compute squared len
	VecFlt len_sq = v[0]*v[0] + v[1]*v[1] + v[2]*v[2] + v[3]*v[3];

	// Normalize vector, but if tiny, just make it 0 instead
	VecFlt rcp_len = (VecFlt(1.0f) / len_sq).sqrt();
	rcp_len &= len_sq.cmp_gt(VecFlt(0x1p-60));

	v[0] *= rcp_len;
	v[1] *= rcp_len;
	v[2] *= rcp_len;
	v[3] *= rcp_len;

	// Overall energy in subset is determined by trace of covariance
	VecFlt overall_energy = cov[0] + cov[1] + cov[2] + cov[3];

	// Fraction of covariance captured by PCA direction is given by Rayleigh coefficient
	VecFlt vlast[4];
	covar_times_vec4(vlast, cov, v);
	VecFlt pca_energy = v[0]*vlast[0] + v[1]*vlast[1] + v[2]*vlast[2] + v[3]*vlast[3];

	// Store results
	overall_energy.storeu(subset_chan(results, BC7SubsetChan::OverallErr, stride));
	pca_energy.storeu(subset_chan(results, BC7SubsetChan::PcaErr, stride));
	v[0].storeu(subset_chan(results, BC7SubsetChan::PcaR, stride));
	v[1].storeu(subset_chan(results, BC7SubsetChan::PcaG, stride));
	v[2].storeu(subset_chan(results, BC7SubsetChan::PcaB, stride));
	v[3].storeu(subset_chan(results, BC7SubsetChan::PcaA, stride));
}

template<typename T>
void subset_prepare_rgba(F32 * results, const U8 pixels[], const U32 subset_masks[], const F32 subset_scale[], SINTa subset_count, SINTa stride)
{
	using VecInt = typename T::VecInt;

	SubsetCovarCalcRGBA<T> covar;
	covar.init(pixels);

	// Process subsets
	for (SINTa i = 0; i < subset_count; i += T::LANES)
	{
		// Determine summary sums for this subset
		VecInt sumi[14];
		covar.query(sumi, &subset_masks[i]);

		subset_prepare_finalize_rgba<T>(&results[i], sumi, &subset_scale[i], stride);
	}
}

// ---- 1-subset version (modes 4-6). RGBA with channel shuffles.

template<typename T>
struct SubsetCovarCalc1Subset
{
	static constexpr int NPIXELS = 16;
	using VecInt = typename T::VecInt;

	void compute(VecInt sumi[14], const U8 pixels[], const S8 * shuffles)
	{
		for (SINTa k = 0; k < 14; ++k)
			sumi[k] = VecInt::zero();

		VecInt shuf = VecInt::loadu(shuffles);
		VecInt c255 { 255 };

		for (SINTa i = 0; i < NPIXELS; ++i)
		{
			VecInt pixel = VecInt::loadu_dup32(&pixels[i*4]);
			VecInt shuffled = pixel.u8().shuf(shuf.s8()).s32();

			VecInt r = shuffled & c255;
			VecInt g = shuffled.template srl<8>() & c255;
			VecInt b = shuffled.template srl<16>() & c255;
			VecInt a = shuffled.template srl<24>();

			sumi[ 0] += r;
			sumi[ 1] += g;
			sumi[ 2] += b;
			sumi[ 3] += a;

			sumi[ 4] += r * r;
			sumi[ 5] += g * g;
			sumi[ 6] += b * b;
			sumi[ 7] += a * a;

			sumi[ 8] += r * g;
			sumi[ 9] += g * b;
			sumi[10] += b * a;
			sumi[11] += a * r;

			sumi[12] += r * b;
			sumi[13] += g * a;
		}
	}
};

template<typename T>
void subset_prepare_rgba_1subset(F32 * results, const U8 pixels[], SINTa stride)
{
	using VecInt = typename T::VecInt;
	constexpr int num_subsets = 5;
	static constexpr S8 expand_shuffles[8*4] =
	{
		0,1,2,3,
		0,1,2,-1,
		3,1,2,-1,
		0,3,2,-1,
		0,1,3,-1,

		0,1,2,3,
		0,1,2,3,
		0,1,2,3,
	};
	static constexpr F32 subset_scale[8] =
	{
		1.0f/16.0f, 1.0f/16.0f, 1.0f/16.0f, 1.0f/16.0f,
		1.0f/16.0f, 1.0f/16.0f, 1.0f/16.0f, 1.0f/16.0f,
	};

	SubsetCovarCalc1Subset<T> covar;

	for (SINTa i = 0; i < num_subsets; i += T::LANES)
	{
		VecInt sumi[14];
		covar.compute(sumi, pixels, &expand_shuffles[i*4]);

		subset_prepare_finalize_rgba<T>(&results[i], sumi, subset_scale, stride);
	}
}

} // internal namespace

OODLE_NS_END

