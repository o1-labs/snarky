#include <cstdint>
#include <stdexcept>

#include <gtest/gtest.h>

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/common/common.hpp"
#include "libiop/protocols/encoded/r1cs_rs_iop/fractal_indexer.hpp"
#include "libiop/relations/examples/r1cs_examples.hpp"
#include "libiop/relations/r1cs.hpp"
#include "libiop/relations/variable.hpp"
#include "libiop/tests/protocols/utilities.cpp"

namespace libiop {

/** Gets the indexer's output oracles,
 *  and test the systematic relation of \hat{M}
 *  as constructed from the indexer's output */
template<typename FieldT>
void run_indexer_test(
    const field_subset<FieldT> &indexing_domain,
    const field_subset<FieldT> &summation_domain,
    const field_subset<FieldT> &codeword_domain,
    const std::shared_ptr<sparse_matrix<FieldT>> matrix)
{
    iop_protocol<FieldT> IOP;
    const domain_handle codeword_domain_handle = IOP.register_domain(codeword_domain);
    const domain_handle summation_domain_handle = IOP.register_domain(summation_domain);
    const domain_handle indexing_domain_handle = IOP.register_domain(indexing_domain);
    const size_t input_variable_dim = 0;

    matrix_indexer<FieldT> indexer(
        IOP,
        indexing_domain_handle,
        summation_domain_handle,
        codeword_domain_handle,
        input_variable_dim,
        matrix);
    indexer.register_oracles();
    IOP.seal_interaction_registrations();
    IOP.seal_query_registrations();
    indexer.compute_oracles();
    const std::vector<FieldT> row_codeword =
        *IOP.get_oracle_evaluations(indexer.get_all_oracle_handles()[0]).get();
    const std::vector<FieldT> col_codeword =
        *IOP.get_oracle_evaluations(indexer.get_all_oracle_handles()[1]).get();
    const std::vector<FieldT> val_codeword =
        *IOP.get_oracle_evaluations(indexer.get_all_oracle_handles()[2]).get();
    // const std::vector<FieldT> row_times_col_codeword =
    //     IOP.get_oracle_evaluations(indexer.get_all_oracle_handles()[3]);

    bivariate_lagrange_polynomial<FieldT> u_H(summation_domain);
    const polynomial<FieldT> row_poly(
        IFFT_over_field_subset<FieldT>(row_codeword, codeword_domain));
    const polynomial<FieldT> col_poly(
        IFFT_over_field_subset<FieldT>(col_codeword, codeword_domain));
    const polynomial<FieldT> val_poly(
        IFFT_over_field_subset<FieldT>(val_codeword, codeword_domain));
    // const polynomial<FieldT> row_times_col_poly(
    //     IFFT_over_field_subset<FieldT>(row_times_col_codeword, codeword_domain));
    // EXPECT_EQ(row_poly.minimal_num_terms(), indexing_domain.num_elements());
    // EXPECT_EQ(col_poly.minimal_num_terms(), indexing_domain.num_elements());
    // EXPECT_EQ(val_poly.minimal_num_terms(), indexing_domain.num_elements());


    /** \hat{M}(X,Y) = sum_{k in K} u_H(row(k), X)u_H(col(k), Y)val(k)
     *  where K = indexing domain, u_H = bivariate lagrange poly
     *  So we check this for all X,Y in H  */

    for (size_t row_index = 0; row_index < summation_domain.num_elements(); row_index++)
    {
        linear_combination<FieldT> matrix_row = matrix->get_row(row_index);
        const FieldT row_sel = summation_domain.element_by_index(row_index);
        for (size_t col_index = 0; col_index < summation_domain.num_elements(); col_index++)
        {
            const FieldT col_sel = summation_domain.element_by_index(col_index);
            /** We now check that
             *  M(X, Y) = sum_{k in K} u_H(row(k), X) u_H(col(k), Y) val(k)*/
            FieldT M_eval = FieldT::zero();
            for (size_t k_index = 0; k_index < indexing_domain.num_elements(); k_index++)
            {
                const FieldT k = indexing_domain.element_by_index(k_index);
                const FieldT row_at_k = row_poly.evaluation_at_point(k);
                const FieldT col_at_k = col_poly.evaluation_at_point(k);
                const FieldT val_at_k = val_poly.evaluation_at_point(k);
                // const FieldT row_times_col_at_k = row_times_col_poly.evaluation_at_point(k);
                // ASSERT_TRUE(row_times_col_at_k == row_at_k * col_at_k);
                FieldT row_derivative = u_H.evaluation_at_point(
                    row_at_k, row_sel);
                FieldT col_derivative = u_H.evaluation_at_point(
                    col_at_k, col_sel);
                M_eval += row_derivative * col_derivative * val_at_k;
            }
            bool term_in_matrix = false;
            for (auto &term : matrix_row)
            {
                if (term.index_ == col_index)
                {
                    term_in_matrix = true;
                    ASSERT_TRUE(M_eval == term.coeff_) <<
                        "row_index " << row_index << " col_index " << col_index;
                }
            }
            if (!term_in_matrix)
            {
                ASSERT_TRUE(M_eval == FieldT::zero()) <<
                    "row_index " << row_index << " col_index " << col_index;
            }
        }
    }
}

template<typename FieldT>
void run_random_indexer_test(std::size_t domain_dim) {
    // TODO: Remove r1cs generation code in favor of just generating M randomly
    const std::size_t num_constraints = 1 << domain_dim;
    const std::size_t num_inputs = (1 << (domain_dim - 2)) - 1;
    const std::size_t num_variables = (1 << domain_dim) - 1;
    r1cs_example<FieldT> r1cs_params = generate_r1cs_example<FieldT>(
        num_constraints, num_inputs, num_variables);
    std::shared_ptr<r1cs_constraint_system<FieldT> > cs =
        std::make_shared<r1cs_constraint_system<FieldT> >(r1cs_params.constraint_system_);
    r1cs_sparse_matrix<FieldT> M(cs, r1cs_sparse_matrix_A);
    std::shared_ptr<sparse_matrix<FieldT>> shared_M =
        std::make_shared<r1cs_sparse_matrix<FieldT>>(M);

    const size_t codeword_domain_dim = 4 + domain_dim;
    const size_t indexing_domain_dim = log2(M.num_nonzero_entries());
    field_subset<FieldT> unshifted_codeword_domain(1ull << codeword_domain_dim);
    FieldT shift = unshifted_codeword_domain.element_outside_of_subset();

    const field_subset<FieldT> codeword_domain(1ull << codeword_domain_dim, shift);
    const field_subset<FieldT> summation_domain(1ull << domain_dim);
    const field_subset<FieldT> indexing_domain(1ull << indexing_domain_dim);

    run_indexer_test<FieldT>(indexing_domain, summation_domain, codeword_domain, shared_M);
}

template<typename FieldT>
void run_all_indexer_tests()
{
    for (size_t domain_dim = 3; domain_dim < 6; domain_dim++)
    {
        run_random_indexer_test<FieldT>(domain_dim);
    }
}

TEST(AdditiveTests, IndexerTest) {
    run_all_indexer_tests<gf64>();
    run_all_indexer_tests<gf128>();
    run_all_indexer_tests<gf192>();
    run_all_indexer_tests<gf256>();
}

TEST(MultiplicativeTests, IndexerTest) {
    edwards_pp::init_public_params();
    alt_bn128_pp::init_public_params();
    run_all_indexer_tests<edwards_Fr>();
    run_all_indexer_tests<alt_bn128_Fr>();
}

}