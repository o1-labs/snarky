#include <cstdint>
#include <gtest/gtest.h>
#include <iostream>
#include <vector>

#include <libff/algebra/curves/edwards/edwards_pp.hpp>
#include "libiop/algebra/exponentiation.hpp"
#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/linearized_polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/common/common.hpp"

namespace libiop {

TEST(PolynomialTest, Initialization) {
    const linearized_polynomial<gf64> lin_poly =
        linearized_polynomial<gf64>::random_linearized_polynomial(4);
}

template<typename FieldT>
void subspace_test_helper(const std::size_t dim,
                          const linearized_polynomial<FieldT> &Z_S,
                          const polynomial<FieldT> &Z_S_poly,
                          const polynomial<FieldT> &truncated_Z_S_poly,
                          const FieldT &highest_coeff,
                          const affine_subspace<FieldT> &S,
                          const bool vanishes)
{
    const std::vector<FieldT> S_elements = S.all_elements();
    EXPECT_EQ(S_elements.size(), S.num_elements());
    const std::vector<FieldT> Z_S_over_S = Z_S.evaluations_over_subspace(S);
    const std::vector<FieldT> truncated_Z_S_poly_over_S =
        truncated_Z_S_poly.evaluations_over_field_subset(field_subset<FieldT>(S));

    EXPECT_EQ(Z_S_over_S.size(), S.num_elements());
    EXPECT_EQ(truncated_Z_S_poly_over_S.size(), S.num_elements());

    for (std::size_t i = 0; i < S.num_elements(); ++i)
    {
        const FieldT linearized_result = Z_S.evaluation_at_point(S_elements[i]);
        const FieldT expanded_result = Z_S_poly.evaluation_at_point(S_elements[i]);
        EXPECT_EQ(expanded_result, linearized_result);

        const FieldT truncated_result = truncated_Z_S_poly.evaluation_at_point(S_elements[i]);
        const FieldT high_term = highest_coeff * power<FieldT>(S_elements[i], (1ull<<dim));
        const FieldT combined_result = truncated_result + high_term;
        EXPECT_EQ(combined_result, expanded_result);

        EXPECT_EQ(linearized_result, Z_S_over_S[i]);
        EXPECT_EQ(truncated_result, truncated_Z_S_poly_over_S[i]);

        if (vanishes)
        {
            EXPECT_EQ(linearized_result, FieldT(0));
        }
        else
        {
            EXPECT_NE(linearized_result, FieldT(0));
        }
    }

}

TEST(PolynomialTest, SubspaceEval) {
    typedef gf64 FieldT;

    const std::size_t dim = 10;
    const affine_subspace<FieldT> S = affine_subspace<FieldT>::random_affine_subspace(dim);

    const linearized_polynomial<FieldT> Z_S =
        vanishing_polynomial<FieldT>(S).get_linearized_polynomial();
    const polynomial<FieldT> Z_S_poly =
        Z_S.expand_as_polynomial();
    EXPECT_EQ(Z_S.degree(), Z_S_poly.degree());

    EXPECT_EQ(Z_S_poly.num_terms(), (1ull<<dim) + 1);
    const FieldT highest_coeff = Z_S_poly[Z_S_poly.degree()];
    EXPECT_EQ(highest_coeff, FieldT(1));

    polynomial<FieldT> truncated_Z_S_poly = Z_S_poly;
    truncated_Z_S_poly[truncated_Z_S_poly.degree()] = FieldT(0);
    truncated_Z_S_poly.set_degree(truncated_Z_S_poly.degree()-1);
    EXPECT_EQ(truncated_Z_S_poly.num_terms(), S.num_elements());
    EXPECT_TRUE(truncated_Z_S_poly.num_terms_at_most((1ull<<(dim-1))+1));

    subspace_test_helper<FieldT>(dim, Z_S, Z_S_poly, truncated_Z_S_poly, highest_coeff, S, true);

    const affine_subspace<FieldT> Sprime = affine_subspace<FieldT>::random_affine_subspace(dim);
    subspace_test_helper<FieldT>(dim, Z_S, Z_S_poly, truncated_Z_S_poly, highest_coeff, Sprime, false);
}

TEST(PolynomialTest, LinearizedPolynomialDivisionTest) {
    typedef gf64 FieldT;

    for (std::size_t deg_P = 0; deg_P < 1000; ++deg_P)
    {
        const polynomial<FieldT> P = polynomial<FieldT>::random_polynomial(deg_P);
        for (std::size_t Z_exponent = 0; Z_exponent < 10; ++Z_exponent)
        {
            const linearized_polynomial<FieldT> Z =
                linearized_polynomial<FieldT>::random_linearized_polynomial(Z_exponent);

            const std::pair<polynomial<FieldT>, polynomial<FieldT> > QR =
                polynomial_over_linearized_polynomial<FieldT>(P, Z);
            const polynomial<FieldT>& Q = QR.first;
            const polynomial<FieldT>& R = QR.second;

            EXPECT_EQ(R.degree(), Z.degree()-1);
            EXPECT_EQ(Q.degree(), (P.degree() <= Z.degree() ? 0 : P.degree()-Z.degree()));

            const FieldT x = FieldT::random_element();
            const FieldT
                Px = P.evaluation_at_point(x),
                Zx = Z.evaluation_at_point(x),
                Qx = Q.evaluation_at_point(x),
                Rx = R.evaluation_at_point(x);

            EXPECT_EQ(Px, Qx * Zx + Rx);
        }
    }
}

}
