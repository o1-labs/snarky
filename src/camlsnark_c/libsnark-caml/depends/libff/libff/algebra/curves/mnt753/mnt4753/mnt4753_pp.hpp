/** @file
 *****************************************************************************

 Declaration of interfaces for public parameters of MNT4.

 *****************************************************************************
 * @author     This file is part of libff, developed by SCIPR Lab
 *             and contributors (see AUTHORS).
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef MNT4753_PP_HPP_
#define MNT4753_PP_HPP_

#include <libff/algebra/curves/mnt753/mnt4753/mnt4753_g1.hpp>
#include <libff/algebra/curves/mnt753/mnt4753/mnt4753_g2.hpp>
#include <libff/algebra/curves/mnt753/mnt4753/mnt4753_init.hpp>
#include <libff/algebra/curves/mnt753/mnt4753/mnt4753_pairing.hpp>
#include <libff/algebra/curves/public_params.hpp>

namespace libff {

class mnt4753_pp {
public:
    typedef mnt4753_Fr Fp_type;
    typedef mnt4753_G1 G1_type;
    typedef mnt4753_G2 G2_type;
    typedef mnt4753_G1_precomp G1_precomp_type;
    typedef mnt4753_G2_precomp G2_precomp_type;
    typedef mnt4753_affine_ate_G1_precomputation affine_ate_G1_precomp_type;
    typedef mnt4753_affine_ate_G2_precomputation affine_ate_G2_precomp_type;
    typedef mnt4753_Fq Fq_type;
    typedef mnt4753_Fq2 Fqe_type;
    typedef mnt4753_Fq4 Fqk_type;
    typedef mnt4753_GT GT_type;

    static const bool has_affine_pairing = true;

    static void init_public_params();
    static mnt4753_GT final_exponentiation(const mnt4753_Fq4 &elt);

    static mnt4753_G1_precomp precompute_G1(const mnt4753_G1 &P);
    static mnt4753_G2_precomp precompute_G2(const mnt4753_G2 &Q);

    static mnt4753_Fq4 miller_loop(const mnt4753_G1_precomp &prec_P,
                                const mnt4753_G2_precomp &prec_Q);

    static mnt4753_affine_ate_G1_precomputation affine_ate_precompute_G1(const mnt4753_G1 &P);
    static mnt4753_affine_ate_G2_precomputation affine_ate_precompute_G2(const mnt4753_G2 &Q);
    static mnt4753_Fq4 affine_ate_miller_loop(const mnt4753_affine_ate_G1_precomputation &prec_P,
                                           const mnt4753_affine_ate_G2_precomputation &prec_Q);

    static mnt4753_Fq4 affine_ate_e_over_e_miller_loop(const mnt4753_affine_ate_G1_precomputation &prec_P1,
                                                    const mnt4753_affine_ate_G2_precomputation &prec_Q1,
                                                    const mnt4753_affine_ate_G1_precomputation &prec_P2,
                                                    const mnt4753_affine_ate_G2_precomputation &prec_Q2);
    static mnt4753_Fq4 affine_ate_e_times_e_over_e_miller_loop(const mnt4753_affine_ate_G1_precomputation &prec_P1,
                                                            const mnt4753_affine_ate_G2_precomputation &prec_Q1,
                                                            const mnt4753_affine_ate_G1_precomputation &prec_P2,
                                                            const mnt4753_affine_ate_G2_precomputation &prec_Q2,
                                                            const mnt4753_affine_ate_G1_precomputation &prec_P3,
                                                            const mnt4753_affine_ate_G2_precomputation &prec_Q3);

    static mnt4753_Fq4 double_miller_loop(const mnt4753_G1_precomp &prec_P1,
                                       const mnt4753_G2_precomp &prec_Q1,
                                       const mnt4753_G1_precomp &prec_P2,
                                       const mnt4753_G2_precomp &prec_Q2);

    /* the following are used in test files */
    static mnt4753_Fq4 pairing(const mnt4753_G1 &P,
                            const mnt4753_G2 &Q);
    static mnt4753_Fq4 reduced_pairing(const mnt4753_G1 &P,
                                    const mnt4753_G2 &Q);
    static mnt4753_Fq4 affine_reduced_pairing(const mnt4753_G1 &P,
                                           const mnt4753_G2 &Q);

    mnt4753_Fq2 twist = mnt4753_twist;

    bigint<mnt4753_q_limbs> final_exponent_last_chunk_abs_of_w0 = mnt4753_final_exponent_last_chunk_abs_of_w0;
    bool final_exponent_last_chunk_is_w0_neg = mnt4753_final_exponent_last_chunk_is_w0_neg;
    bigint<mnt4753_q_limbs> final_exponent_last_chunk_w1 = mnt4753_final_exponent_last_chunk_w1;
};

} // libff

#endif // MNT4753_PP_HPP_
