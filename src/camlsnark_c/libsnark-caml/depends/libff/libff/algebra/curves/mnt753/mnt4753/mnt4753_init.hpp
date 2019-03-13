/** @file
 *****************************************************************************

 Declaration of interfaces for initializing MNT4.

 *****************************************************************************
 * @author     This file is part of libff, developed by SCIPR Lab
 *             and contributors (see AUTHORS).
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef MNT4753_INIT_HPP_
#define MNT4753_INIT_HPP_

#include <libff/algebra/curves/mnt753/mnt46753_common.hpp>
#include <libff/algebra/curves/public_params.hpp>
#include <libff/algebra/fields/fp.hpp>
#include <libff/algebra/fields/fp2.hpp>
#include <libff/algebra/fields/fp4.hpp>

namespace libff {

#define mnt4753_modulus_r mnt46753_modulus_A
#define mnt4753_modulus_q mnt46753_modulus_B

const mp_size_t mnt4753_r_bitcount = mnt46753_A_bitcount;
const mp_size_t mnt4753_q_bitcount = mnt46753_B_bitcount;

const mp_size_t mnt4753_r_limbs = mnt46753_A_limbs;
const mp_size_t mnt4753_q_limbs = mnt46753_B_limbs;

extern bigint<mnt4753_r_limbs> mnt4753_modulus_r;
extern bigint<mnt4753_q_limbs> mnt4753_modulus_q;

typedef Fp_model<mnt4753_r_limbs, mnt4753_modulus_r> mnt4753_Fr;
typedef Fp_model<mnt4753_q_limbs, mnt4753_modulus_q> mnt4753_Fq;
typedef Fp2_model<mnt4753_q_limbs, mnt4753_modulus_q> mnt4753_Fq2;
typedef Fp4_model<mnt4753_q_limbs, mnt4753_modulus_q> mnt4753_Fq4;
typedef mnt4753_Fq4 mnt4753_GT;

// parameters for twisted short Weierstrass curve E'/Fq2 : y^2 = x^3 + (a * twist^2) * x + (b * twist^3)
extern mnt4753_Fq2 mnt4753_twist;
extern mnt4753_Fq2 mnt4753_twist_coeff_a;
extern mnt4753_Fq2 mnt4753_twist_coeff_b;
extern mnt4753_Fq mnt4753_twist_mul_by_a_c0;
extern mnt4753_Fq mnt4753_twist_mul_by_a_c1;
extern mnt4753_Fq mnt4753_twist_mul_by_b_c0;
extern mnt4753_Fq mnt4753_twist_mul_by_b_c1;
extern mnt4753_Fq mnt4753_twist_mul_by_q_X;
extern mnt4753_Fq mnt4753_twist_mul_by_q_Y;

// parameters for pairing
extern bigint<mnt4753_q_limbs> mnt4753_ate_loop_count;
extern bool mnt4753_ate_is_loop_count_neg;
extern bigint<4*mnt4753_q_limbs> mnt4753_final_exponent;
extern bigint<mnt4753_q_limbs> mnt4753_final_exponent_last_chunk_abs_of_w0;
extern bool mnt4753_final_exponent_last_chunk_is_w0_neg;
extern bigint<mnt4753_q_limbs> mnt4753_final_exponent_last_chunk_w1;

void init_mnt4753_params();

class mnt4753_G1;
class mnt4753_G2;

} // libff

#endif // MNT4753_INIT_HPP_
