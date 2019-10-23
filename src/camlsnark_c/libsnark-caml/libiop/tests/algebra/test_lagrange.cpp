#include <cstdint>
#include <gtest/gtest.h>
#include <vector>

#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/lagrange.hpp"
#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/utils.hpp"
#include <libff/algebra/curves/edwards/edwards_pp.hpp>
#include <libff/algebra/curves/alt_bn128/alt_bn128_pp.hpp>


namespace libiop {

template<typename FieldT>
void run_lagrange_test(const field_subset<FieldT> &domain) {
    const std::size_t dim = domain.dimension();
    lagrange_cache<FieldT> L_cache(domain, false);

    const std::size_t poly_deg = 1ull<<dim;
    const polynomial<FieldT> poly = polynomial<FieldT>::random_polynomial(poly_deg);

    const std::vector<FieldT> poly_evals = FFT_over_field_subset(poly.coefficients(), domain);

    const FieldT point = FieldT::random_element();
    const FieldT evaluation = poly.evaluation_at_point(point);

    const std::vector<FieldT> lagrange_coeffs = lagrange_coefficients<FieldT>(domain, point);
    ASSERT_EQ(lagrange_coeffs.size(), poly_deg);
    ASSERT_EQ(poly_evals.size(), poly_deg);

    FieldT interpolation = FieldT(0);
    for (std::size_t i = 0 ; i < poly_deg; ++i)
    {
        interpolation += lagrange_coeffs[i] * poly_evals[i];
    }

    EXPECT_TRUE(evaluation == interpolation);
    const std::vector<FieldT> lagrange_coeffs2 = L_cache.coefficients_for(point);
    ASSERT_TRUE(lagrange_coeffs == lagrange_coeffs2);
}

TEST(Test, LagrangeTest) {
    const std::size_t dim = 10;
    const field_subset<gf64> additive_domain(
        affine_subspace<gf64>::random_affine_subspace(dim));
    run_lagrange_test<gf64>(additive_domain);
    edwards_pp::init_public_params();
    const field_subset<edwards_Fr> multiplicative_domain(
        1ull << dim, edwards_Fr::one());
    run_lagrange_test<edwards_Fr>(multiplicative_domain);
    const field_subset<edwards_Fr> multiplicative_domain_with_offset(
        1ull << dim, edwards_Fr::multiplicative_generator);
    run_lagrange_test<edwards_Fr>(multiplicative_domain_with_offset);

    libff::alt_bn128_pp::init_public_params();
    const field_subset<alt_bn128_Fr> altbn_domain(
        1ull << dim, alt_bn128_Fr::one());
    run_lagrange_test<alt_bn128_Fr>(altbn_domain);
}

template<typename FieldT>
void run_intersecting_lagrange_test(const field_subset<FieldT> &domain) {
    const std::size_t dim = domain.dimension();
    lagrange_cache<FieldT> L_cache(domain, false, true);

    for (std::size_t i = 0; i < 10; i++) {
        const std::size_t eval_pos = rand() % domain.num_elements();
        const FieldT point = domain.element_by_index(eval_pos);
        const std::vector<FieldT> lagrange_coeffs = L_cache.coefficients_for(point);
        for (std::size_t j = 0; j < domain.num_elements(); j++) {
            if (j == eval_pos) {
                EXPECT_TRUE(lagrange_coeffs[j] == FieldT::one());
            } else {
                EXPECT_TRUE(lagrange_coeffs[j] == FieldT::zero());
            }
        }
    }
}

TEST(InterpolationDomainIntersects, LagrangeTest) {
    const std::size_t dim = 10;
    const field_subset<gf64> additive_domain(
        affine_subspace<gf64>::random_affine_subspace(dim));
    run_intersecting_lagrange_test<gf64>(additive_domain);
    edwards_pp::init_public_params();
    const field_subset<edwards_Fr> multiplicative_domain(
        1ull << dim, edwards_Fr::one());
    run_intersecting_lagrange_test<edwards_Fr>(multiplicative_domain);
    const field_subset<edwards_Fr> multiplicative_domain_with_offset(
        1ull << dim, edwards_Fr::multiplicative_generator);
    run_intersecting_lagrange_test<edwards_Fr>(multiplicative_domain_with_offset);

    libff::alt_bn128_pp::init_public_params();
    const field_subset<alt_bn128_Fr> altbn_domain(
        1ull << dim, alt_bn128_Fr::one());
    run_intersecting_lagrange_test<alt_bn128_Fr>(altbn_domain);
}

TEST(CacheTest, LagrangeTest) {
    // This ensures that the lagrange cache correctly caches the previous evaluation
    typedef gf64 FieldT;

    const std::size_t dim = 17;
    const field_subset<FieldT> domain(
        affine_subspace<FieldT>::random_affine_subspace(dim));
    lagrange_cache<FieldT> L_cache(domain, true);

    const std::size_t poly_deg = 1ull<<dim;
    const polynomial<FieldT> poly = polynomial<FieldT>::random_polynomial(poly_deg);

    const std::vector<FieldT> poly_evals = poly.evaluations_over_field_subset(domain);

    const FieldT point = FieldT::random_element();
    const FieldT evaluation = poly.evaluation_at_point(point);

    std::vector<FieldT> lagrange_coeffs = L_cache.coefficients_for(point);
    ASSERT_EQ(lagrange_coeffs.size(), poly_deg);
    ASSERT_EQ(poly_evals.size(), poly_deg);

    // test correctness
    FieldT interpolation = FieldT(0);
    for (std::size_t i = 0 ; i < poly_deg; ++i)
    {
        interpolation += lagrange_coeffs[i] * poly_evals[i];
    }
    EXPECT_EQ(evaluation, interpolation);
    // test altering one evaluation won't affect future evaluations
    FieldT orig_val = lagrange_coeffs[0];
    lagrange_coeffs[0] = lagrange_coeffs[0] + FieldT(1);
    std::vector<FieldT> lagrange_coeffs2 = L_cache.coefficients_for(point);
    EXPECT_EQ(orig_val, lagrange_coeffs2[0]);
    lagrange_coeffs[0] = orig_val;
    EXPECT_EQ(lagrange_coeffs, lagrange_coeffs2);
    // Check that it was actually cached by ensuring the following terminates in reasonable time.
    // (if unsure, try turning off the cache property and see if it slows)
    for (std::size_t i = 0; i < 10000; i++) {
        std::vector<FieldT> lagrange_coeffs3 = L_cache.coefficients_for(point);
    }
}

}
