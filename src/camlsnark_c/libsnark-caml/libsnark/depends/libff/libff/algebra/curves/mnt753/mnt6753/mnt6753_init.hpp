/** @file
 *****************************************************************************

 Declaration of interfaces for initializing MNT6.

 *****************************************************************************
 * @author     This file is part of libff, developed by SCIPR Lab
 *             and contributors (see AUTHORS).
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef MNT6753_INIT_HPP_
#define MNT6753_INIT_HPP_

#include <libff/algebra/curves/mnt753/mnt46753_common.hpp>
#include <libff/algebra/curves/public_params.hpp>
#include <libff/algebra/fields/fp.hpp>
#include <libff/algebra/fields/fp3.hpp>
#include <libff/algebra/fields/fp6_2over3.hpp>

namespace libff {

#define mnt6753_modulus_r mnt46753_modulus_B
#define mnt6753_modulus_q mnt46753_modulus_A

const mp_size_t mnt6753_r_bitcount = mnt46753_B_bitcount;
const mp_size_t mnt6753_q_bitcount = mnt46753_A_bitcount;

const mp_size_t mnt6753_r_limbs = mnt46753_B_limbs;
const mp_size_t mnt6753_q_limbs = mnt46753_A_limbs;

extern bigint<mnt6753_r_limbs> mnt6753_modulus_r;
extern bigint<mnt6753_q_limbs> mnt6753_modulus_q;

typedef Fp_model<mnt6753_r_limbs, mnt6753_modulus_r> mnt6753_Fr;
typedef Fp_model<mnt6753_q_limbs, mnt6753_modulus_q> mnt6753_Fq;
typedef Fp3_model<mnt6753_q_limbs, mnt6753_modulus_q> mnt6753_Fq3;
typedef Fp6_2over3_model<mnt6753_q_limbs, mnt6753_modulus_q> mnt6753_Fq6;
typedef mnt6753_Fq6 mnt6753_GT;

// parameters for twisted short Weierstrass curve E'/Fq3 : y^2 = x^3 + (a * twist^2) * x + (b * twist^3)
extern mnt6753_Fq3 mnt6753_twist;
extern mnt6753_Fq3 mnt6753_twist_coeff_a;
extern mnt6753_Fq3 mnt6753_twist_coeff_b;
extern mnt6753_Fq mnt6753_twist_mul_by_a_c0;
extern mnt6753_Fq mnt6753_twist_mul_by_a_c1;
extern mnt6753_Fq mnt6753_twist_mul_by_a_c2;
extern mnt6753_Fq mnt6753_twist_mul_by_b_c0;
extern mnt6753_Fq mnt6753_twist_mul_by_b_c1;
extern mnt6753_Fq mnt6753_twist_mul_by_b_c2;
extern mnt6753_Fq mnt6753_twist_mul_by_q_X;
extern mnt6753_Fq mnt6753_twist_mul_by_q_Y;

// parameters for pairing
extern bigint<mnt6753_q_limbs> mnt6753_ate_loop_count;
extern bool mnt6753_ate_is_loop_count_neg;
extern bigint<6*mnt6753_q_limbs> mnt6753_final_exponent;
extern bigint<mnt6753_q_limbs> mnt6753_final_exponent_last_chunk_abs_of_w0;
extern bool mnt6753_final_exponent_last_chunk_is_w0_neg;
extern bigint<mnt6753_q_limbs> mnt6753_final_exponent_last_chunk_w1;

void init_mnt6753_params();

class mnt6753_G1;
class mnt6753_G2;

} // libff

#endif // MNT6753_INIT_HPP_
