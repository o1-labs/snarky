#include <cstdint>
#include <gtest/gtest.h>
#include <iostream>
#include <vector>

#include <libff/algebra/curves/edwards/edwards_pp.hpp>
#include <libff/algebra/curves/alt_bn128/alt_bn128_pp.hpp>

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/fields/gf32.hpp"
#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/fields/gf128.hpp"
#include "libiop/algebra/fields/gf192.hpp"
#include "libiop/algebra/fields/gf256.hpp"
#include "libiop/algebra/polynomials/bivariate_lagrange_polynomial.hpp"
#include "libiop/algebra/polynomials/lagrange_polynomial.hpp"

namespace libiop {

template<typename FieldT>
void run_lagrange_polynomial_evaluations_test(
    const field_subset<FieldT> &vp_domain,
    const field_subset<FieldT> &evaluation_domain,
    const FieldT x_point)
{
    const bivariate_lagrange_polynomial<FieldT> B = bivariate_lagrange_polynomial<FieldT>(vp_domain);
    const lagrange_polynomial<FieldT> P = lagrange_polynomial<FieldT>(x_point, vp_domain);
    const std::vector<FieldT> evals = P.evaluations_over_field_subset(evaluation_domain);

    const lagrange_polynomial<FieldT> P_unnormalized = lagrange_polynomial<FieldT>(x_point, vp_domain, false);
    const std::vector<FieldT> evals_unnormalized = P_unnormalized.evaluations_over_field_subset(evaluation_domain);
    for (std::size_t i = 0; i < evaluation_domain.num_elements(); i++) {
        const FieldT y = evaluation_domain.element_by_index(i);
        const FieldT eval_at_pt = P.evaluation_at_point(y);
        ASSERT_TRUE(eval_at_pt == evals[i]) <<
            "evaluation at point didn't match evaluations over domain on element " << i <<
            ", evaluation domain type: " << field_subset_type_names[evaluation_domain.type()] <<
            ", vp domain dim: " << vp_domain.dimension() <<
            ", eval domain dim: " << evaluation_domain.dimension();

        const FieldT unnormalized_eval_at_pt = P_unnormalized.evaluation_at_point(y);
        ASSERT_TRUE(unnormalized_eval_at_pt == evals_unnormalized[i]) <<
            "evaluation at point of the unnormalized polynomials didn't match evaluations over domain on element " << i <<
            ", evaluation domain type: " << field_subset_type_names[evaluation_domain.type()] <<
            ", vp domain dim: " << vp_domain.dimension() <<
            ", eval domain dim: " << evaluation_domain.dimension();
        ASSERT_TRUE(B.evaluation_at_point(x_point, y) == unnormalized_eval_at_pt);
    }

    polynomial<FieldT> lagrange_poly(IFFT_over_field_subset<FieldT>(evals, evaluation_domain));
    ASSERT_LE(lagrange_poly.minimal_num_terms() - 1, P.degree());
}

template<typename FieldT>
void test_lagrange_polynomial_systematic_relation(const field_subset<FieldT> &vp_domain)
{
    size_t x_index = std::rand() % vp_domain.num_elements();
    FieldT x = vp_domain.element_by_index(x_index);
    const lagrange_polynomial<FieldT> P = lagrange_polynomial<FieldT>(x, vp_domain);
    for (std::size_t i = 0; i < vp_domain.num_elements(); i++)
    {
        if (i == x_index)
        {
            ASSERT_TRUE(P.evaluation_at_point(vp_domain.element_by_index(i)) == FieldT::one());
        }
        else
        {
            ASSERT_TRUE(P.evaluation_at_point(vp_domain.element_by_index(i)) == FieldT::zero());
        }
    }
}

template<typename FieldT>
void run_lagrange_polynomial_tests_for_field(size_t dimension_upperbound, size_t non_power_of_2 = 1)
{
    for (size_t dim_vp = 1; dim_vp < dimension_upperbound; ++dim_vp)
    {
        const field_subset<FieldT> vp_domain(non_power_of_2 * (1ull << dim_vp));
        const field_subset<FieldT> vp_domain_coset(
            non_power_of_2 * (1ull << dim_vp), vp_domain.element_outside_of_subset());
        test_lagrange_polynomial_systematic_relation(vp_domain);
        test_lagrange_polynomial_systematic_relation(vp_domain_coset);
        for (size_t dim_S = 1; dim_S < dimension_upperbound + log2(non_power_of_2); ++dim_S)
        {
            const FieldT x = FieldT::random_element();
            const field_subset<FieldT> S(1ull << dim_S);
            const field_subset<FieldT> S_coset(
                1ull << dim_S, S.element_outside_of_subset());
            const field_subset<FieldT> S_coset_containing_x(1ull << dim_S, x);
            run_lagrange_polynomial_evaluations_test<FieldT>(vp_domain, S, x);
            run_lagrange_polynomial_evaluations_test<FieldT>(vp_domain, S_coset, x);
            run_lagrange_polynomial_evaluations_test<FieldT>(vp_domain, S_coset_containing_x, x);
            run_lagrange_polynomial_evaluations_test<FieldT>(vp_domain_coset, S, x);
            run_lagrange_polynomial_evaluations_test<FieldT>(vp_domain_coset, S_coset, x);
            run_lagrange_polynomial_evaluations_test<FieldT>(vp_domain_coset, S_coset_containing_x, x);
       }
    }
}

TEST(PolynomialTest, MultiplicativeLagrangePolynomialTest)
{
    edwards_pp::init_public_params();
    run_lagrange_polynomial_tests_for_field<edwards_Fr>(10, 3);

    alt_bn128_pp::init_public_params();
    run_lagrange_polynomial_tests_for_field<alt_bn128_Fr>(10, 3);
}

TEST(PolynomialTest, AdditiveLagrangePolynomialTest)
{
    const size_t dimension = 10;
    run_lagrange_polynomial_tests_for_field<gf64>(dimension);
    run_lagrange_polynomial_tests_for_field<gf128>(dimension);
    run_lagrange_polynomial_tests_for_field<gf192>(dimension);
    run_lagrange_polynomial_tests_for_field<gf256>(dimension);
}

}
