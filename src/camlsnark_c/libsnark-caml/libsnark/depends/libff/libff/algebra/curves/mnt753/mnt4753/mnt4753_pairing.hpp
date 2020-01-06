/** @file
 *****************************************************************************

 Declaration of interfaces for pairing operations on MNT4753.

 *****************************************************************************
 * @author     This file is part of libff, developed by SCIPR Lab
 *             and contributors (see AUTHORS).
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef MNT4753_PAIRING_HPP_
#define MNT4753_PAIRING_HPP_

#include <vector>

#include <libff/algebra/curves/mnt753/mnt4753/mnt4753_init.hpp>

namespace libff {

/* final exponentiation */

mnt4753_Fq4 mnt4753_final_exponentiation_last_chunk(const mnt4753_Fq4 &elt,
                                              const mnt4753_Fq4 &elt_inv);
mnt4753_Fq4 mnt4753_final_exponentiation_first_chunk(const mnt4753_Fq4 &elt,
                                               const mnt4753_Fq4 &elt_inv);
mnt4753_GT mnt4753_final_exponentiation(const mnt4753_Fq4 &elt);

/* affine ate miller loop */

struct mnt4753_affine_ate_G1_precomputation {
    mnt4753_Fq PX;
    mnt4753_Fq PY;
    mnt4753_Fq2 PY_twist_squared;
};

struct mnt4753_affine_ate_coeffs {
    // TODO: trim (not all of them are needed)
    mnt4753_Fq2 old_RX;
    mnt4753_Fq2 old_RY;
    mnt4753_Fq2 gamma;
    mnt4753_Fq2 gamma_twist;
    mnt4753_Fq2 gamma_X;
};

struct mnt4753_affine_ate_G2_precomputation {
    mnt4753_Fq2 QX;
    mnt4753_Fq2 QY;
    std::vector<mnt4753_affine_ate_coeffs> coeffs;
};

mnt4753_affine_ate_G1_precomputation mnt4753_affine_ate_precompute_G1(const mnt4753_G1& P);
mnt4753_affine_ate_G2_precomputation mnt4753_affine_ate_precompute_G2(const mnt4753_G2& Q);

mnt4753_Fq4 mnt4753_affine_ate_miller_loop(const mnt4753_affine_ate_G1_precomputation &prec_P,
                                     const mnt4753_affine_ate_G2_precomputation &prec_Q);

/* ate pairing */

struct mnt4753_ate_G1_precomp {
    mnt4753_Fq PX;
    mnt4753_Fq PY;
    mnt4753_Fq2 PX_twist;
    mnt4753_Fq2 PY_twist;

    bool operator==(const mnt4753_ate_G1_precomp &other) const;
    friend std::ostream& operator<<(std::ostream &out, const mnt4753_ate_G1_precomp &prec_P);
    friend std::istream& operator>>(std::istream &in, mnt4753_ate_G1_precomp &prec_P);
};

struct mnt4753_ate_dbl_coeffs {
    mnt4753_Fq2 c_H;
    mnt4753_Fq2 c_4C;
    mnt4753_Fq2 c_J;
    mnt4753_Fq2 c_L;

    bool operator==(const mnt4753_ate_dbl_coeffs &other) const;
    friend std::ostream& operator<<(std::ostream &out, const mnt4753_ate_dbl_coeffs &dc);
    friend std::istream& operator>>(std::istream &in, mnt4753_ate_dbl_coeffs &dc);
};

struct mnt4753_ate_add_coeffs {
    mnt4753_Fq2 c_L1;
    mnt4753_Fq2 c_RZ;

    bool operator==(const mnt4753_ate_add_coeffs &other) const;
    friend std::ostream& operator<<(std::ostream &out, const mnt4753_ate_add_coeffs &dc);
    friend std::istream& operator>>(std::istream &in, mnt4753_ate_add_coeffs &dc);
};

struct mnt4753_ate_G2_precomp {
    mnt4753_Fq2 QX;
    mnt4753_Fq2 QY;
    mnt4753_Fq2 QY2;
    mnt4753_Fq2 QX_over_twist;
    mnt4753_Fq2 QY_over_twist;
    std::vector<mnt4753_ate_dbl_coeffs> dbl_coeffs;
    std::vector<mnt4753_ate_add_coeffs> add_coeffs;

    bool operator==(const mnt4753_ate_G2_precomp &other) const;
    friend std::ostream& operator<<(std::ostream &out, const mnt4753_ate_G2_precomp &prec_Q);
    friend std::istream& operator>>(std::istream &in, mnt4753_ate_G2_precomp &prec_Q);
};

mnt4753_ate_G1_precomp mnt4753_ate_precompute_G1(const mnt4753_G1& P);
mnt4753_ate_G2_precomp mnt4753_ate_precompute_G2(const mnt4753_G2& Q);

mnt4753_Fq4 mnt4753_ate_miller_loop(const mnt4753_ate_G1_precomp &prec_P,
                                    const mnt4753_ate_G2_precomp &prec_Q);
mnt4753_Fq4 mnt4753_ate_double_miller_loop(const mnt4753_ate_G1_precomp &prec_P1,
                                           const mnt4753_ate_G2_precomp &prec_Q1,
                                           const mnt4753_ate_G1_precomp &prec_P2,
                                           const mnt4753_ate_G2_precomp &prec_Q2);

mnt4753_Fq4 mnt4753_ate_pairing(const mnt4753_G1& P,
                          const mnt4753_G2 &Q);
mnt4753_GT mnt4753_ate_reduced_pairing(const mnt4753_G1 &P,
                                 const mnt4753_G2 &Q);

/* choice of pairing */

typedef mnt4753_ate_G1_precomp mnt4753_G1_precomp;
typedef mnt4753_ate_G2_precomp mnt4753_G2_precomp;

mnt4753_G1_precomp mnt4753_precompute_G1(const mnt4753_G1& P);

mnt4753_G2_precomp mnt4753_precompute_G2(const mnt4753_G2& Q);

mnt4753_Fq4 mnt4753_miller_loop(const mnt4753_G1_precomp &prec_P,
                          const mnt4753_G2_precomp &prec_Q);

mnt4753_Fq4 mnt4753_double_miller_loop(const mnt4753_G1_precomp &prec_P1,
                                 const mnt4753_G2_precomp &prec_Q1,
                                 const mnt4753_G1_precomp &prec_P2,
                                 const mnt4753_G2_precomp &prec_Q2);

mnt4753_Fq4 mnt4753_pairing(const mnt4753_G1& P,
                      const mnt4753_G2 &Q);

mnt4753_GT mnt4753_reduced_pairing(const mnt4753_G1 &P,
                             const mnt4753_G2 &Q);

mnt4753_GT mnt4753_affine_reduced_pairing(const mnt4753_G1 &P,
                                    const mnt4753_G2 &Q);

} // libff

#endif // MNT4753_PAIRING_HPP_
