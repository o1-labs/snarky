#include <cstdint>
#include <gtest/gtest.h>
#include <iostream>
#include <vector>
#include <unordered_map>
#include <utility>

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
#include "libiop/algebra/trace_embedding/bivariate_embedding.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/algebra/field_subset/basis_utils.hpp"
#include "libiop/common/common.hpp"

namespace libiop {

template<typename FieldT>
void run_bivariate_embedding_test(field_subset<FieldT> &H,
    field_subset<FieldT> &row_domain, field_subset<FieldT> &col_domain) {
    /** This does the bivariate embedding operation for every element in H.
     *  It checks that every pair of corresponding elements in H1 X H2 hasn't appeared before.
     *  We do this with an O(|H|^2) algorithm.
     *  Neither libff's field elements, nor the bigints implement a comparison operator, or hash operator,
     *  so we can't use a sorted map or hashmap.
     *  We can get around this by doing a string stream, and then doing a hashmap, but that is not implemented yet.
     *  The N^2 algorithm is not a real issue though, since its N^2 equality operations, not field operations.
     *
     *  It then ensures that the constituent H1 and H2 components are in the respective domain.
     */
    std::vector<std::pair<FieldT, FieldT>> seen_elements;
    seen_elements.reserve(H.num_elements());
    bivariate_embedding<FieldT> embedding(H, row_domain, col_domain);
    vanishing_polynomial<FieldT> row_vp(row_domain);
    vanishing_polynomial<FieldT> col_vp(col_domain);

    for (size_t i = 0; i < H.num_elements(); i++)
    {
        const FieldT cur = H.element_by_index(i);
        const FieldT row_component = embedding.project_to_row(cur);
        const FieldT col_component = embedding.project_to_col(cur);
        // H1 and H2 don't have the standard basis in the additive tests,
        // so element_in_subset is currently unimplemented.
        EXPECT_TRUE(row_vp.evaluation_at_point(row_component) == FieldT::zero()) <<
            "The row component was not in the row domain on index " << i;
        ASSERT_TRUE(col_vp.evaluation_at_point(col_component) == FieldT::zero()) <<
            "The column component was not in the column domain on index " << i;

        const std::pair<FieldT, FieldT> embedding_pair =
            std::make_pair(row_component, col_component);
        for (size_t j = 0; j < seen_elements.size(); j++)
        {
            ASSERT_FALSE(seen_elements[j] == embedding_pair) << "The embedding produced a duplicate value";
        }
        seen_elements.emplace_back(embedding_pair);
    }
    ASSERT_EQ(seen_elements.size(), H.num_elements());
}

template<typename FieldT>
void run_bivariate_embedding_consistency_and_degree_tests(field_subset<FieldT> &H,
    field_subset<FieldT> &row_domain, field_subset<FieldT> &col_domain, field_subset<FieldT> &eval_domain)
{
    /** This tests that the bivariate embedding's constituent polynomials are consistent
     *  with the claimed polynomials, and tests that the polynomials have the correct degree. */
    bivariate_embedding<FieldT> embedding(H, row_domain, col_domain);
    std::vector<FieldT> row_evals = embedding.polynomial_map_into_row_domain()->evaluations_over_field_subset(eval_domain);
    std::vector<FieldT> col_evals = embedding.polynomial_map_into_col_domain()->evaluations_over_field_subset(eval_domain);
    for (size_t i = 0; i < eval_domain.num_elements(); i++)
    {
        const FieldT cur_elem = eval_domain.element_by_index(i);
        const FieldT expected_row_projection = embedding.project_to_row(cur_elem);
        const FieldT expected_col_projection = embedding.project_to_col(cur_elem);
        ASSERT_TRUE(row_evals[i] == expected_row_projection) <<
            "H1 polynomial's evaluation_over_domain and evaluation_at_point differed at index " << i;
        ASSERT_TRUE(col_evals[i] == expected_col_projection) <<
            "H2 polynomial's evaluation_over_domain and evaluation_at_point differed at index " << i;
    }
    polynomial<FieldT> row_poly(IFFT_over_field_subset<FieldT>(row_evals, eval_domain));
    polynomial<FieldT> col_poly(IFFT_over_field_subset<FieldT>(col_evals, eval_domain));
    EXPECT_EQ(row_poly.minimal_num_terms(), col_domain.num_elements() + 1);
    EXPECT_EQ(col_poly.minimal_num_terms(), row_domain.num_elements() + 1);
}

template<typename FieldT>
void run_bivariate_embedding_composed_projection_tests(field_subset<FieldT> &H,
    field_subset<FieldT> &row_domain, field_subset<FieldT> &col_domain, field_subset<FieldT> &eval_domain)
{
    /** This tests that the bivariate embedding's constituent polynomials are consistent
     *  with the claimed polynomials, and tests that the polynomials have the correct degree. */
    bivariate_embedding<FieldT> embedding(H, row_domain, col_domain);
    /** Sample polynomial, currently set to x^3 + x^2 + x + 1 */
    polynomial<FieldT> poly({FieldT::one(), FieldT::one(), FieldT::one(), FieldT::one()});
    std::shared_ptr<polynomial_base<FieldT>> shared_poly = std::make_shared<polynomial<FieldT>>(poly);
    std::shared_ptr<polynomial_base<FieldT>> composed_with_row_projection =
        embedding.compose_polynomial_with_row_projection(shared_poly);
    std::vector<FieldT> composed_with_row_projection_evals =
        composed_with_row_projection->evaluations_over_field_subset(eval_domain);
    std::shared_ptr<polynomial_base<FieldT>> composed_with_col_projection =
        embedding.compose_polynomial_with_col_projection(shared_poly);
    std::vector<FieldT> composed_with_col_projection_evals =
        composed_with_col_projection->evaluations_over_field_subset(eval_domain);
    for (size_t i = 0; i < eval_domain.num_elements(); i++)
    {
        const FieldT cur_elem = eval_domain.element_by_index(i);
        const FieldT row_projection = embedding.project_to_row(cur_elem);
        const FieldT col_projection = embedding.project_to_col(cur_elem);
        ASSERT_TRUE(composed_with_row_projection_evals[i] == poly.evaluation_at_point(row_projection)) <<
            "row composition was incorrect at index " << i;
        ASSERT_TRUE(composed_with_col_projection_evals[i] == poly.evaluation_at_point(col_projection)) <<
            "col composition was incorrect at index " << i;
    }
    polynomial<FieldT> row_poly(IFFT_over_field_subset<FieldT>(composed_with_row_projection_evals, eval_domain));
    polynomial<FieldT> col_poly(IFFT_over_field_subset<FieldT>(composed_with_col_projection_evals, eval_domain));
    ASSERT_EQ(row_poly.minimal_num_terms() - 1, shared_poly->degree() * col_domain.num_elements());
    ASSERT_EQ(col_poly.minimal_num_terms() - 1, shared_poly->degree() * row_domain.num_elements());
}

template<typename FieldT>
void additive_bivariate_embedding_tests_for_field(const size_t dimension)
{
    field_subset<FieldT> H(1ull << dimension);
    field_subset<FieldT> H_extended(1ull << (dimension + 2));
    field_subset<FieldT> H_extended_shifted(1ull << (dimension + 2), H_extended.element_outside_of_subset());
    for (size_t row_dim = 1; row_dim < dimension; row_dim++)
    {
        // We split H into two parts, called left and right for convenience
        // such that the union of the basis vectors of these equal H.
        // Then we construct the row domain and column domain accordingly.s
        field_subset<FieldT> V(1ull << row_dim);
        vanishing_polynomial<FieldT> V_vp(V);
        std::vector<FieldT> W_basis =
            monomial_basis<FieldT>(dimension - row_dim, row_dim);
        field_subset<FieldT> W = field_subset<FieldT>(affine_subspace<FieldT>(W_basis));
        vanishing_polynomial<FieldT> W_vp(W);

        std::shared_ptr<polynomial_base<FieldT>> V_vp_shared = V_vp.associated_k_to_1_map();
        std::shared_ptr<polynomial_base<FieldT>> W_vp_shared = W_vp.associated_k_to_1_map();

        std::vector<FieldT> row_basis = transform_basis_by_polynomial(W_vp_shared, V.basis());
        std::vector<FieldT> col_basis = transform_basis_by_polynomial(V_vp_shared, W_basis);
        field_subset<FieldT> row_domain = field_subset<FieldT>(affine_subspace<FieldT>(row_basis));
        field_subset<FieldT> col_domain = field_subset<FieldT>(affine_subspace<FieldT>(col_basis));
        run_bivariate_embedding_test<FieldT>(H, row_domain, col_domain);

        run_bivariate_embedding_consistency_and_degree_tests(H, row_domain, col_domain, H_extended);
        run_bivariate_embedding_consistency_and_degree_tests(H, row_domain, col_domain, H_extended_shifted);
    }
}

template<typename FieldT>
void multiplicative_bivariate_embedding_tests_for_field(const size_t order, const size_t largest_power_of_two_dividing_order)
{
    field_subset<FieldT> H(order);
    field_subset<FieldT> H_extended(largest_power_of_two_dividing_order << 3);
    field_subset<FieldT> H_extended_shifted(largest_power_of_two_dividing_order << 3, H_extended.element_outside_of_subset());
    field_subset<FieldT> H1(largest_power_of_two_dividing_order);
    field_subset<FieldT> H2(order / largest_power_of_two_dividing_order);
    run_bivariate_embedding_test<FieldT>(H, H1, H2);
    run_bivariate_embedding_test<FieldT>(H, H2, H1);

    run_bivariate_embedding_consistency_and_degree_tests(H, H1, H2, H_extended);
    run_bivariate_embedding_consistency_and_degree_tests(H, H1, H2, H_extended_shifted);
    run_bivariate_embedding_composed_projection_tests(H, H1, H2, H_extended);
    run_bivariate_embedding_composed_projection_tests(H, H1, H2, H_extended_shifted);
    run_bivariate_embedding_composed_projection_tests(H, H2, H1, H_extended);
    run_bivariate_embedding_composed_projection_tests(H, H2, H1, H_extended_shifted);
}

TEST(MultiplicativeBivariateEmbedding, TestOrdering)
{
    // quick, hacky test. Will be heavily modified.
    // it suggests we need a list of the factors for the field somewhere.
    size_t dimension = 10;
    size_t order = (1ull << dimension) * 3;
    edwards_pp::init_public_params();
    multiplicative_bivariate_embedding_tests_for_field<edwards_Fr>(order, 1ull << dimension);
}

TEST(AdditiveBivariateEmbedding, TestOrdering)
{
    const size_t dimension = 10;
    additive_bivariate_embedding_tests_for_field<gf64>(dimension);
    additive_bivariate_embedding_tests_for_field<gf128>(dimension);
    additive_bivariate_embedding_tests_for_field<gf192>(dimension);
    additive_bivariate_embedding_tests_for_field<gf256>(dimension);
}

}
