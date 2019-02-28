/** @file
 *****************************************************************************

 Declaration of interfaces for pairing operations on MNT6.

 *****************************************************************************
 * @author     This file is part of libff, developed by SCIPR Lab
 *             and contributors (see AUTHORS).
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef MNT6753_PAIRING_HPP_
#define MNT6753_PAIRING_HPP_

#include <vector>

#include <libff/algebra/curves/mnt753/mnt6753/mnt6753_init.hpp>

namespace libff {

/* final exponentiation */

mnt6753_Fq6 mnt6753_final_exponentiation_last_chunk(const mnt6753_Fq6 &elt,
                                              const mnt6753_Fq6 &elt_inv);
mnt6753_Fq6 mnt6753_final_exponentiation_first_chunk(const mnt6753_Fq6 &elt,
                                               const mnt6753_Fq6 &elt_inv);
mnt6753_GT mnt6753_final_exponentiation(const mnt6753_Fq6 &elt);

/* affine ate miller loop */

struct mnt6753_affine_ate_G1_precomputation {
    mnt6753_Fq PX;
    mnt6753_Fq PY;
    mnt6753_Fq3 PY_twist_squared;
};

struct mnt6753_affine_ate_coeffs {
    // TODO: trim (not all of them are needed)
    mnt6753_Fq3 old_RX;
    mnt6753_Fq3 old_RY;
    mnt6753_Fq3 gamma;
    mnt6753_Fq3 gamma_twist;
    mnt6753_Fq3 gamma_X;
};

struct mnt6753_affine_ate_G2_precomputation {
    mnt6753_Fq3 QX;
    mnt6753_Fq3 QY;
    std::vector<mnt6753_affine_ate_coeffs> coeffs;
};

mnt6753_affine_ate_G1_precomputation mnt6753_affine_ate_precompute_G1(const mnt6753_G1& P);
mnt6753_affine_ate_G2_precomputation mnt6753_affine_ate_precompute_G2(const mnt6753_G2& Q);

mnt6753_Fq6 mnt6753_affine_ate_miller_loop(const mnt6753_affine_ate_G1_precomputation &prec_P,
                                     const mnt6753_affine_ate_G2_precomputation &prec_Q);

/* ate pairing */

struct mnt6753_ate_G1_precomp {
    mnt6753_Fq PX;
    mnt6753_Fq PY;
    mnt6753_Fq3 PX_twist;
    mnt6753_Fq3 PY_twist;

    bool operator==(const mnt6753_ate_G1_precomp &other) const;
    friend std::ostream& operator<<(std::ostream &out, const mnt6753_ate_G1_precomp &prec_P);
    friend std::istream& operator>>(std::istream &in, mnt6753_ate_G1_precomp &prec_P);
};

struct mnt6753_ate_dbl_coeffs {
    mnt6753_Fq3 c_H;
    mnt6753_Fq3 c_4C;
    mnt6753_Fq3 c_J;
    mnt6753_Fq3 c_L;

    bool operator==(const mnt6753_ate_dbl_coeffs &other) const;
    friend std::ostream& operator<<(std::ostream &out, const mnt6753_ate_dbl_coeffs &dc);
    friend std::istream& operator>>(std::istream &in, mnt6753_ate_dbl_coeffs &dc);
};

struct mnt6753_ate_add_coeffs {
    mnt6753_Fq3 c_L1;
    mnt6753_Fq3 c_RZ;

    bool operator==(const mnt6753_ate_add_coeffs &other) const;
    friend std::ostream& operator<<(std::ostream &out, const mnt6753_ate_add_coeffs &dc);
    friend std::istream& operator>>(std::istream &in, mnt6753_ate_add_coeffs &dc);
};

struct mnt6753_ate_G2_precomp {
    mnt6753_Fq3 QX;
    mnt6753_Fq3 QY;
    mnt6753_Fq3 QY2;
    mnt6753_Fq3 QX_over_twist;
    mnt6753_Fq3 QY_over_twist;
    std::vector<mnt6753_ate_dbl_coeffs> dbl_coeffs;
    std::vector<mnt6753_ate_add_coeffs> add_coeffs;

    bool operator==(const mnt6753_ate_G2_precomp &other) const;
    friend std::ostream& operator<<(std::ostream &out, const mnt6753_ate_G2_precomp &prec_Q);
    friend std::istream& operator>>(std::istream &in, mnt6753_ate_G2_precomp &prec_Q);
};

mnt6753_ate_G1_precomp mnt6753_ate_precompute_G1(const mnt6753_G1& P);
mnt6753_ate_G2_precomp mnt6753_ate_precompute_G2(const mnt6753_G2& Q);

mnt6753_Fq6 mnt6753_ate_miller_loop(const mnt6753_ate_G1_precomp &prec_P,
                              const mnt6753_ate_G2_precomp &prec_Q);
mnt6753_Fq6 mnt6753_ate_double_miller_loop(const mnt6753_ate_G1_precomp &prec_P1,
                                     const mnt6753_ate_G2_precomp &prec_Q1,
                                     const mnt6753_ate_G1_precomp &prec_P2,
                                     const mnt6753_ate_G2_precomp &prec_Q2);

mnt6753_Fq6 mnt6753_ate_pairing(const mnt6753_G1& P,
                          const mnt6753_G2 &Q);
mnt6753_GT mnt6753_ate_reduced_pairing(const mnt6753_G1 &P,
                                 const mnt6753_G2 &Q);

/* choice of pairing */

typedef mnt6753_ate_G1_precomp mnt6753_G1_precomp;
typedef mnt6753_ate_G2_precomp mnt6753_G2_precomp;

mnt6753_G1_precomp mnt6753_precompute_G1(const mnt6753_G1& P);

mnt6753_G2_precomp mnt6753_precompute_G2(const mnt6753_G2& Q);

mnt6753_Fq6 mnt6753_miller_loop(const mnt6753_G1_precomp &prec_P,
                          const mnt6753_G2_precomp &prec_Q);

mnt6753_Fq6 mnt6753_double_miller_loop(const mnt6753_G1_precomp &prec_P1,
                                 const mnt6753_G2_precomp &prec_Q1,
                                 const mnt6753_G1_precomp &prec_P2,
                                 const mnt6753_G2_precomp &prec_Q2);

mnt6753_Fq6 mnt6753_pairing(const mnt6753_G1& P,
                      const mnt6753_G2 &Q);

mnt6753_GT mnt6753_reduced_pairing(const mnt6753_G1 &P,
                             const mnt6753_G2 &Q);

mnt6753_GT mnt6753_affine_reduced_pairing(const mnt6753_G1 &P,
                                    const mnt6753_G2 &Q);

} // libff

#endif // MNT6753_PAIRING_HPP_
