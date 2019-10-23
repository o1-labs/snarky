#include <cstdint>
#include <stdexcept>

#include <gtest/gtest.h>

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/common/common.hpp"
#include "libiop/protocols/encoded/common/boundary_constraint.hpp"
#include "libiop/tests/protocols/utilities.cpp"

namespace libiop {

template<typename FieldT>
void run_test(const field_subset<FieldT> codeword_domain,
              const std::vector<FieldT> codeword,
              const size_t degree,
              const FieldT evaluation_point,
              const FieldT claimed_oracle_eval,
              const bool expect_pass)
{
    iop_protocol<FieldT> IOP;
    const domain_handle codeword_domain_handle = IOP.register_domain(codeword_domain);

    bool make_zk = false;
    oracle_handle_ptr poly_handle = std::make_shared<oracle_handle>(
        IOP.register_oracle(codeword_domain_handle, degree, make_zk));

    single_boundary_constraint<FieldT> boundary_constraint(codeword_domain);
    boundary_constraint.set_evaluation_point_and_eval(evaluation_point, claimed_oracle_eval);
    oracle_handle_ptr boundary_constraint_handle = std::make_shared<virtual_oracle_handle>(
        IOP.register_virtual_oracle(
            codeword_domain_handle,
            degree - 1,
            {poly_handle},
            std::make_shared<single_boundary_constraint<FieldT>>(boundary_constraint)));

    IOP.seal_interaction_registrations();
    IOP.seal_query_registrations();
    IOP.submit_oracle(poly_handle, codeword);

    test_oracles_degree_and_consistency(
        IOP,
        {boundary_constraint_handle},
        codeword_domain,
        expect_pass);
}

template<typename FieldT>
void run_random_test(const size_t poly_degree, const bool expect_pass) {
    const size_t codeword_domain_dim = log2(poly_degree) + 2;
    const field_subset<FieldT> unshifted_codeword_domain(1ull << codeword_domain_dim);
    const FieldT codeword_domain_shift = unshifted_codeword_domain.element_outside_of_subset();

    const field_subset<FieldT> codeword_domain(1ull << codeword_domain_dim, codeword_domain_shift);

    polynomial<FieldT> poly = polynomial<FieldT>::random_polynomial(poly_degree);
    std::vector<FieldT> codeword = FFT_over_field_subset<FieldT>(
        poly.coefficients(), codeword_domain);
    const FieldT random_eval_pt = FieldT::random_element();
    FieldT poly_eval_at_pt;
    if (expect_pass)
    {
        poly_eval_at_pt = poly.evaluation_at_point(random_eval_pt);
    }
    else if (!expect_pass)
    {
        poly_eval_at_pt = FieldT::random_element();
    }

    run_test(codeword_domain, codeword, poly_degree,
            random_eval_pt, poly_eval_at_pt, expect_pass);
}

TEST(AdditiveSucceedingTests, BoundaryConstraintTest) {
    typedef gf64 FieldT;
    bool passing_tests = true;
    for (std::size_t log2_poly_degree = 6; log2_poly_degree < 12; log2_poly_degree++) {
        size_t poly_degree = 1ull << log2_poly_degree;
        run_random_test<FieldT>(
            poly_degree,
            passing_tests);
    }
}

TEST(MultiplicativeSucceedingTests, BoundaryConstraintTest) {
    alt_bn128_pp::init_public_params();
    typedef alt_bn128_Fr FieldT;
    bool passing_tests = true;
    for (std::size_t log2_poly_degree = 6; log2_poly_degree < 12; log2_poly_degree++) {
        size_t poly_degree = 1ull << log2_poly_degree;
        run_random_test<FieldT>(
            poly_degree,
            passing_tests);
    }
}

TEST(AdditiveFailingTests, BoundaryConstraintTest) {
    typedef gf64 FieldT;
    bool failing_tests = false;
    for (std::size_t log2_poly_degree = 6; log2_poly_degree < 12; log2_poly_degree++) {
        size_t poly_degree = 1ull << log2_poly_degree;
        run_random_test<FieldT>(
            poly_degree,
            failing_tests);
    }
}

TEST(MultiplicativeFailingTests, BoundaryConstraintTest) {
    alt_bn128_pp::init_public_params();
    typedef alt_bn128_Fr FieldT;
    bool failing_tests = false;
    for (std::size_t log2_poly_degree = 6; log2_poly_degree < 12; log2_poly_degree++) {
        size_t poly_degree = 1ull << log2_poly_degree;
        run_random_test<FieldT>(
            poly_degree,
            failing_tests);
    }
}

}