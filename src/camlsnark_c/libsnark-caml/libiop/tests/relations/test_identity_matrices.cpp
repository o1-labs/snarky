#include <cstdint>
#include <stdexcept>

#include <gtest/gtest.h>

#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/common/common.hpp"
#include "libiop/relations/succinct_matrices/identity.hpp"
#include <libff/algebra/curves/edwards/edwards_pp.hpp>
#include <libff/algebra/curves/alt_bn128/alt_bn128_pp.hpp>

namespace libiop {

template<typename FieldT>
void run_identity_test(const size_t dim)
{
    identity_matrix<FieldT> identity(1ull << dim);
    /** Constructing a suitable polynomial_base */
    field_subset<FieldT> domain(1ull << dim);
    vanishing_polynomial<FieldT> vp(domain);
    std::shared_ptr<polynomial_base<FieldT>> poly = vp.associated_k_to_1_map();

    std::shared_ptr<polynomial_base<FieldT>> transformed_poly = identity.extend_Mz(poly);
    /** Test equality of polynomials */
    ASSERT_EQ(transformed_poly->degree(), poly->degree());
    for (size_t i = 0; i < 10; i++)
    {
        const FieldT rand_pt = FieldT::random_element();
        ASSERT_TRUE(transformed_poly->evaluation_at_point(rand_pt) == poly->evaluation_at_point(rand_pt));
    }
}

TEST(AdditiveIdentityTest, SimpleTest) {
    typedef gf64 FieldT;
    for (size_t i = 1; i < 10; i++)
    {
        run_identity_test<FieldT>(i);
    }
}

TEST(MultiplicativeIdentityTest, SimpleTest) {
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;
    for (size_t i = 1; i < 10; i++)
    {
        run_identity_test<FieldT>(i);
    }
}

template<typename FieldT>
void run_shifted_identity_test(const size_t dim)
{
    field_subset<FieldT> domain(1ull << dim);
    successor_ordering<FieldT> ordering(domain);
    shifted_identity_matrix<FieldT> shifted_identity(domain, ordering);
    /** Constructing a suitable polynomial_base */
    vanishing_polynomial<FieldT> vp(domain);
    std::shared_ptr<polynomial_base<FieldT>> poly = vp.associated_k_to_1_map();

    std::shared_ptr<polynomial_base<FieldT>> transformed_poly = shifted_identity.extend_Mz(poly);
    /** Test correctness of polynomials */
    std::vector<FieldT> systematic_evals = transformed_poly->evaluations_over_field_subset(domain);
    ASSERT_EQ(transformed_poly->degree(), shifted_identity.Mz_degree(poly->degree()));
    FieldT cur_elem = ordering.first_elem();
    ASSERT_TRUE(transformed_poly->evaluation_at_point(cur_elem) == FieldT::zero());
    ASSERT_TRUE(systematic_evals[0] == FieldT::zero());
    for (size_t i = 1; i < domain.num_elements(); i++)
    {
        const FieldT prev_elem = cur_elem;
        cur_elem = ordering.next_elem(cur_elem);
        const FieldT actual_eval = transformed_poly->evaluation_at_point(cur_elem);
        const FieldT expected = poly->evaluation_at_point(prev_elem);
        ASSERT_TRUE(actual_eval == expected) << "error on index " << i;
        ASSERT_TRUE(systematic_evals[i] == actual_eval) << "error on index " << i;
    }

    /** Test degree of polynomials */
    field_subset<FieldT> extended_domain(1ull << (dim + 2));
    std::vector<FieldT> evals = transformed_poly->evaluations_over_field_subset(extended_domain);
    polynomial<FieldT> actual_transformed_poly(IFFT_over_field_subset<FieldT>(evals, extended_domain));
    ASSERT_EQ(actual_transformed_poly.minimal_num_terms() - 1, shifted_identity.Mz_degree(poly->degree()));
    for (size_t i = 0; i < 10; i++)
    {
        FieldT rand_elem = FieldT::random_element();
        FieldT actual_eval = actual_transformed_poly.evaluation_at_point(rand_elem);
        FieldT expected_eval = transformed_poly->evaluation_at_point(rand_elem);
        ASSERT_TRUE(actual_eval == expected_eval);
    }
}

TEST(MultiplicativeShiftedIdentityTest, SimpleTest) {
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;
    for (size_t i = 1; i < 10; i++)
    {
        run_shifted_identity_test<FieldT>(i);
    }
}

}
