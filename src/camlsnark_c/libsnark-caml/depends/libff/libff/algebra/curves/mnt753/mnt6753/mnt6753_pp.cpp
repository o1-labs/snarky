/** @file
 *****************************************************************************

 Implementation of interfaces for public parameters of MNT6.

 See mnt6753_pp.hpp .

 *****************************************************************************
 * @author     This file is part of libff, developed by SCIPR Lab
 *             and contributors (see AUTHORS).
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#include <libff/algebra/curves/mnt753/mnt6753/mnt6753_pp.hpp>

namespace libff {

void mnt6753_pp::init_public_params()
{
    init_mnt6753_params();
}

mnt6753_GT mnt6753_pp::final_exponentiation(const mnt6753_Fq6 &elt)
{
    return mnt6753_final_exponentiation(elt);
}

mnt6753_G1_precomp mnt6753_pp::precompute_G1(const mnt6753_G1 &P)
{
    return mnt6753_precompute_G1(P);
}

mnt6753_G2_precomp mnt6753_pp::precompute_G2(const mnt6753_G2 &Q)
{
    return mnt6753_precompute_G2(Q);
}


mnt6753_Fq6 mnt6753_pp::miller_loop(const mnt6753_G1_precomp &prec_P,
                              const mnt6753_G2_precomp &prec_Q)
{
    return mnt6753_miller_loop(prec_P, prec_Q);
}

mnt6753_affine_ate_G1_precomputation mnt6753_pp::affine_ate_precompute_G1(const mnt6753_G1 &P)
{
    return mnt6753_affine_ate_precompute_G1(P);
}

mnt6753_affine_ate_G2_precomputation mnt6753_pp::affine_ate_precompute_G2(const mnt6753_G2 &Q)
{
    return mnt6753_affine_ate_precompute_G2(Q);
}

mnt6753_Fq6 mnt6753_pp::affine_ate_miller_loop(const mnt6753_affine_ate_G1_precomputation &prec_P,
                                         const mnt6753_affine_ate_G2_precomputation &prec_Q)
{
    return mnt6753_affine_ate_miller_loop(prec_P, prec_Q);
}

mnt6753_Fq6 mnt6753_pp::double_miller_loop(const mnt6753_G1_precomp &prec_P1,
                                     const mnt6753_G2_precomp &prec_Q1,
                                     const mnt6753_G1_precomp &prec_P2,
                                     const mnt6753_G2_precomp &prec_Q2)
{
    return mnt6753_double_miller_loop(prec_P1, prec_Q1, prec_P2, prec_Q2);
}

mnt6753_Fq6 mnt6753_pp::affine_ate_e_over_e_miller_loop(const mnt6753_affine_ate_G1_precomputation &prec_P1,
                                                  const mnt6753_affine_ate_G2_precomputation &prec_Q1,
                                                  const mnt6753_affine_ate_G1_precomputation &prec_P2,
                                                  const mnt6753_affine_ate_G2_precomputation &prec_Q2)
{
    return mnt6753_affine_ate_miller_loop(prec_P1, prec_Q1) * mnt6753_affine_ate_miller_loop(prec_P2, prec_Q2).unitary_inverse();
}

mnt6753_Fq6 mnt6753_pp::affine_ate_e_times_e_over_e_miller_loop(const mnt6753_affine_ate_G1_precomputation &prec_P1,
                                                          const mnt6753_affine_ate_G2_precomputation &prec_Q1,
                                                          const mnt6753_affine_ate_G1_precomputation &prec_P2,
                                                          const mnt6753_affine_ate_G2_precomputation &prec_Q2,
                                                          const mnt6753_affine_ate_G1_precomputation &prec_P3,
                                                          const mnt6753_affine_ate_G2_precomputation &prec_Q3)
{
    return ((mnt6753_affine_ate_miller_loop(prec_P1, prec_Q1) * mnt6753_affine_ate_miller_loop(prec_P2, prec_Q2)) *
            mnt6753_affine_ate_miller_loop(prec_P3, prec_Q3).unitary_inverse());
}

mnt6753_Fq6 mnt6753_pp::pairing(const mnt6753_G1 &P,
                          const mnt6753_G2 &Q)
{
    return mnt6753_pairing(P, Q);
}

mnt6753_Fq6 mnt6753_pp::reduced_pairing(const mnt6753_G1 &P,
                                  const mnt6753_G2 &Q)
{
    return mnt6753_reduced_pairing(P, Q);
}

mnt6753_Fq6 mnt6753_pp::affine_reduced_pairing(const mnt6753_G1 &P,
                                         const mnt6753_G2 &Q)
{
    return mnt6753_affine_reduced_pairing(P, Q);
}

} // libff
