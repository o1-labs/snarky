#include <cstdint>
#include <gtest/gtest.h>
#include <iostream>
#include <vector>
#include <unordered_map>

#include <libff/algebra/curves/edwards/edwards_pp.hpp>
#include <libff/algebra/curves/alt_bn128/alt_bn128_pp.hpp>
#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/exponentiation.hpp"
#include "libiop/algebra/fields/gf32.hpp"
#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/fields/gf128.hpp"
#include "libiop/algebra/fields/gf192.hpp"
#include "libiop/algebra/fields/gf256.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/algebra/trace_embedding/successor_ordering.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/common/common.hpp"

namespace libiop {

template<typename FieldT>
void run_successor_ordering_test(field_subset<FieldT> &domain) {
    /** This does the successor operation for every element in the domain.
     *  It checks that every new element hasn't appeared before.
     *  We do this with an O(N^2) algorithm.
     *  Neither libff's field elements, nor the bigints implement a comparison operator, or hash operator,
     *  so we can't use a sorted map or hashmap.
     *  We can get around this by doing a string stream, and then doing a hashmap, but that is not implemented yet.
     *  The N^2 algorithm is not a real issue though, since its N^2 equality operations, not field operations.
     *
     *  It then ensures that the new element is in the domain.
     */
    std::vector<FieldT> seen_elements;
    seen_elements.reserve(domain.num_elements());
    successor_ordering<FieldT> ordering(domain);
    FieldT cur = ordering.first_elem();
    seen_elements.emplace_back(cur);

    for (size_t i = 1; i < domain.num_elements(); i++)
    {
        cur = ordering.next_elem(cur);
        for (size_t j = 0; j < seen_elements.size(); j++)
        {
            ASSERT_FALSE(seen_elements[j] == cur) << "The ordering hit an already reached value. ";
        }
        ASSERT_TRUE(domain.element_in_subset(cur)) << "The ordering returned an element outside the subgroup";
        seen_elements.emplace_back(cur);
    }
}

template<typename FieldT>
void run_successor_ordering_consistency_and_degree_tests(
    field_subset<FieldT> &ordering_domain, field_subset<FieldT> &eval_domain)
{
    /** This tests that the successor ordering is consistent between evaluation at point
     *  and evaluation over field_subset.
     *  It then tests the degree of the successor ordering is the degree it claims to be.
     */
    successor_ordering<FieldT> ordering(ordering_domain);
    std::vector<FieldT> evals = ordering.piecewise_polynomial()->evaluations_over_field_subset(eval_domain);
    for (size_t i = 0; i < eval_domain.num_elements(); i++)
    {
        const FieldT cur_elem = eval_domain.element_by_index(i);
        const FieldT expected = ordering.next_elem(cur_elem);
        ASSERT_TRUE(evals[i] == expected) <<
            "evaluation_over_domain and evaluation_at_point differed at index " << i;
    }
    polynomial<FieldT> successor_poly(IFFT_over_field_subset<FieldT>(evals, eval_domain));
    ASSERT_LE(successor_poly.minimal_num_terms() - 1, ordering.piecewise_polynomial()->degree());
}

template<typename FieldT>
void successor_ordering_tests_for_field(const size_t dimension)
{
    field_subset<FieldT> domain(1ull << dimension);
    run_successor_ordering_test(domain);
    field_subset<FieldT> shifted_domain(1ull << dimension, domain.element_outside_of_subset());
    run_successor_ordering_test(shifted_domain);

    const size_t extra_dimensions = 2;
    field_subset<FieldT> extended_domain(1ull << (dimension + extra_dimensions));
    field_subset<FieldT> shifted_extended_domain(1ull << (dimension + extra_dimensions),
        extended_domain.element_outside_of_subset());

    run_successor_ordering_consistency_and_degree_tests(domain, domain);
    run_successor_ordering_consistency_and_degree_tests(domain, shifted_domain);
    run_successor_ordering_consistency_and_degree_tests(shifted_domain, domain);
    run_successor_ordering_consistency_and_degree_tests(domain, extended_domain);
    run_successor_ordering_consistency_and_degree_tests(domain, shifted_extended_domain);
    run_successor_ordering_consistency_and_degree_tests(shifted_domain, extended_domain);
    run_successor_ordering_consistency_and_degree_tests(shifted_domain, shifted_extended_domain);
}

TEST(MultiplicativeSuccessorOrdering, TestOrdering)
{
    size_t dimension = 12;
    edwards_pp::init_public_params();
    successor_ordering_tests_for_field<edwards_Fr>(dimension);

    alt_bn128_pp::init_public_params();
    successor_ordering_tests_for_field<alt_bn128_Fr>(dimension);
}

TEST(AdditiveSuccessorOrdering, TestOrdering)
{
    const size_t dimension = 10;
    successor_ordering_tests_for_field<gf64>(dimension);
    successor_ordering_tests_for_field<gf128>(dimension);
    successor_ordering_tests_for_field<gf192>(dimension);
    successor_ordering_tests_for_field<gf256>(dimension);
}

}
