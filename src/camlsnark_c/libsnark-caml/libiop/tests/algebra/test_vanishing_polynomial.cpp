#include <cstdint>
#include <gtest/gtest.h>
#include <iostream>
#include <vector>

#include <libff/algebra/curves/edwards/edwards_pp.hpp>
#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/exponentiation.hpp"
#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/common/common.hpp"

namespace libiop {

template<typename FieldT>
void run_vanishing_polynomial_multiplication_test(FieldT shift) {
    const std::size_t dim = 10;
    const std::size_t p_deg = 100;
    const field_subset<FieldT> S = field_subset<FieldT>(1ull << dim, shift);

    const vanishing_polynomial<FieldT> Z_S(S);
    const polynomial<FieldT> p = polynomial<FieldT>::random_polynomial(p_deg);
    const polynomial<FieldT> product = Z_S * p;
    ASSERT_EQ(product.num_terms(), p_deg + (1ull << dim));
    for (std::size_t i = 0; i < (1ull << dim) + p_deg + 5; i++) {
        FieldT x = FieldT(i);
        FieldT product_x = product.evaluation_at_point(x);
        FieldT expected_x = Z_S.evaluation_at_point(x) * p.evaluation_at_point(x);
        ASSERT_TRUE(product_x == expected_x);
    }
}

TEST(PolynomialTest, TestGf64VanishingPolynomialMultiplication) {
    gf64 shift = gf64(1ull << 20);
    run_vanishing_polynomial_multiplication_test<gf64>(gf64::zero());
    run_vanishing_polynomial_multiplication_test<gf64>(shift);
}

TEST(PolynomialTest, TestEdwardsVanishingPolynomialMultiplication) {
    edwards_pp::init_public_params();
    run_vanishing_polynomial_multiplication_test<edwards_Fr>(edwards_Fr::one());
    run_vanishing_polynomial_multiplication_test<edwards_Fr>(edwards_Fr::multiplicative_generator);
}

template<typename FieldT>
void run_vanishing_polynomial_evaluations_test(
    const field_subset<FieldT> &vp_domain, const field_subset<FieldT> &evaluation_domain) {
    const vanishing_polynomial<FieldT> P(vp_domain);
    const std::vector<FieldT> evals = P.evaluations_over_field_subset(evaluation_domain);
    ASSERT_EQ(evals.size(), evaluation_domain.num_elements());
    for (std::size_t i = 0; i < evaluation_domain.num_elements(); i++) {
        const FieldT expected = P.evaluation_at_point(evaluation_domain.element_by_index(i));
        ASSERT_TRUE(evals[i] == expected) << "evaluations didn't match on element " << i <<
            ", evaluation domain type: " << evaluation_domain.type() <<
            ", vp domain size: " << vp_domain.num_elements() <<
            ", eval domain size: " << evaluation_domain.num_elements();
    }
    if (is_power_of_2(evaluation_domain.num_elements()))
    {
        const polynomial<FieldT> P_as_poly(IFFT_over_field_subset<FieldT>(evals, evaluation_domain));
        size_t minimal_num_terms = P_as_poly.minimal_num_terms();
        size_t actual_degree = (minimal_num_terms == 0) ? 0 : minimal_num_terms - 1;
        ASSERT_LE(actual_degree, P.degree());
    }
}

TEST(PolynomialTest, MultiplicativeVanishingPolynomialTest) {
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;

    for (std::size_t dim_vp = 1; dim_vp < 10; ++dim_vp)
    {
        const field_subset<FieldT> vp_domain(1ull << dim_vp);
        const field_subset<FieldT> vp_domain_coset(1ull << dim_vp,
            FieldT::multiplicative_generator);
        for (std::size_t dim_S = 1; dim_S < 10; ++dim_S)
        {
            const field_subset<FieldT> S(1ull << dim_S);
            const field_subset<FieldT> S_coset(
                1ull << dim_S, FieldT::multiplicative_generator);
            run_vanishing_polynomial_evaluations_test<FieldT>(vp_domain, S);
            run_vanishing_polynomial_evaluations_test<FieldT>(vp_domain, S_coset);
            run_vanishing_polynomial_evaluations_test<FieldT>(vp_domain_coset, S);
            run_vanishing_polynomial_evaluations_test<FieldT>(vp_domain_coset, S_coset);
       }
    }
}

TEST(PolynomialTest, MultiplicativeVanishingPolynomialNonPowerOf2DomainsTest) {
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;
    // vector of some select factors of |F| - 1.
    std::vector<size_t> factors({128, 3, 5, 7, 17, 29, 53, 79, 113});

    for (size_t vp_factor_start_index = 0; vp_factor_start_index < 3; vp_factor_start_index++)
    {
        for (size_t num_vp_factors = 1; num_vp_factors <= 1; ++num_vp_factors)
        {
            size_t vp_order = 1;
            for (size_t i = 0; i < num_vp_factors; i++)
            {
                vp_order *= factors[i + vp_factor_start_index];
            }
            const field_subset<FieldT> vp_domain(vp_order);
            const field_subset<FieldT> vp_domain_coset(vp_order,
                FieldT::multiplicative_generator);
            for (size_t S_factor_start_point = 0; S_factor_start_point < 3; S_factor_start_point++)
            {
                for (size_t num_S_factors = 1; num_S_factors <= 1; ++num_S_factors)
                {
                    size_t S_order = 1;
                    for (size_t i = 0; i < num_S_factors; i++)
                    {
                        S_order *= factors[i + S_factor_start_point];
                    }
                    const field_subset<FieldT> S(S_order);
                    const field_subset<FieldT> S_coset(
                        S_order, FieldT::multiplicative_generator);
                    run_vanishing_polynomial_evaluations_test<FieldT>(vp_domain, S);
                    run_vanishing_polynomial_evaluations_test<FieldT>(vp_domain, S_coset);
                    run_vanishing_polynomial_evaluations_test<FieldT>(vp_domain_coset, S);
                    run_vanishing_polynomial_evaluations_test<FieldT>(vp_domain_coset, S_coset);
                }
            }
        }
    }
}

TEST(PolynomialTest, MultiplicativeVanishingPolynomialDivisionTest) {
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;

    for (std::size_t dim_vp = 1; dim_vp < 10; ++dim_vp)
    {
        const field_subset<FieldT> G(1ull << dim_vp);
        const vanishing_polynomial<FieldT> vp = vanishing_polynomial<FieldT>(G);
        for (std::size_t dim_P = dim_vp; dim_P < 10; ++dim_P)
        {
            const field_subset<FieldT> S(1ull << (dim_P + 1));
            const polynomial<FieldT> poly = polynomial<FieldT>::random_polynomial(1ull << dim_P);
            const std::pair<polynomial<FieldT>, polynomial<FieldT>> div_pair =
                polynomial_over_vanishing_polynomial<FieldT>(
                polynomial<FieldT>(poly), vp);
            const std::vector<FieldT> quotient_evals = FFT_over_field_subset<FieldT>(div_pair.first.coefficients(), S);
            const std::vector<FieldT> remainder_evals = FFT_over_field_subset<FieldT>(div_pair.second.coefficients(), S);
            const std::vector<FieldT> vp_evals = vp.evaluations_over_field_subset(S);

            for (std::size_t i = 0; i < S.num_elements(); i++) {
                const FieldT expected = poly.evaluation_at_point(S.element_by_index(i));
                const FieldT actual = quotient_evals[i] * vp_evals[i] + remainder_evals[i];
                ASSERT_TRUE(actual == expected) << "evaluations didn't match on element " << i <<
                    ", vp dim: " << dim_vp << " P dim: " << dim_P;
            }
        }
    }
}

}
