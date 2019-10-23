#include <cstdint>
#include <stdexcept>

#include <gtest/gtest.h>

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/common/common.hpp"
#include "libiop/protocols/encoded/lincheck/holographic_lincheck.hpp"
#include "libiop/relations/examples/r1cs_examples.hpp"
#include "libiop/relations/r1cs.hpp"
#include "libiop/relations/variable.hpp"
#include "libiop/tests/protocols/utilities.cpp"

namespace libiop {

/** Runs the test on multi_lincheck, and just checks the result from sumcheck */
template<typename FieldT>
void run_black_box_multi_lincheck_test(
    const field_subset<FieldT> summation_domain,
    const field_subset<FieldT> input_variable_domain,
    const field_subset<FieldT> codeword_domain,
    const std::vector<std::vector<FieldT> > Mzs_over_codeword_domain,
    const std::vector<FieldT> fz_over_codeword_domain,
    std::vector<r1cs_sparse_matrix<FieldT> > matrices,
    field_subset_type domain_type,
    bool make_zk,
    std::size_t query_bound,
    bool exp_pass)
{
    std::size_t make_zk_int = make_zk ? 1 : 0;
    /** TODO: Make matrices with different index domain sizes */
    size_t index_domain_size = 0;
    for (size_t i = 0; i < matrices.size(); i++)
    {
        const size_t cur_size = libiop::round_to_next_power_of_2(
            matrices[i].num_nonzero_entries());
        if (cur_size > index_domain_size)
        {
            index_domain_size = cur_size;
        }
    }
    field_subset<FieldT> index_domain(index_domain_size);

    iop_protocol<FieldT> IOP;
    const domain_handle codeword_domain_handle = IOP.register_domain(codeword_domain);
    const domain_handle index_domain_handle = IOP.register_domain(index_domain);
    const domain_handle summation_domain_handle = IOP.register_domain(summation_domain);

    std::vector<std::shared_ptr<matrix_indexer<FieldT> >> matrix_indexers;
    std::vector<std::shared_ptr<sparse_matrix<FieldT> >> lincheck_matrices;
    std::vector<std::vector<oracle_handle_ptr>> indexed_handles;
    for (std::size_t i = 0; i < matrices.size(); i++)
    {
        lincheck_matrices.emplace_back(
            std::make_shared<r1cs_sparse_matrix<FieldT> >(matrices[i]));
        matrix_indexers.emplace_back(
            std::make_shared<matrix_indexer<FieldT>>(
                IOP,
                index_domain_handle,
                summation_domain_handle,
                codeword_domain_handle,
                input_variable_domain.dimension(),
                lincheck_matrices[i]));
        matrix_indexers[i]->register_oracles();
        indexed_handles.emplace_back(matrix_indexers[i]->get_all_oracle_handles());
    }
    IOP.signal_index_registrations_done();

    const std::size_t b = make_zk ? query_bound : 0;
    const std::size_t fz_degree = summation_domain.num_elements() + b;
    const std::size_t Mz_degree = summation_domain.num_elements() + b;
    oracle_handle_ptr fz_handle =
        std::make_shared<oracle_handle>(IOP.register_oracle(codeword_domain_handle, fz_degree, make_zk));
    std::vector<oracle_handle_ptr> Mz_handles;
    for (std::size_t i = 0; i < matrices.size(); i++) {
        Mz_handles.emplace_back(
            std::make_shared<oracle_handle>(
                IOP.register_oracle(codeword_domain_handle, Mz_degree, make_zk)
            ));
    }
    const size_t dummy_security_parameter = 64;
    holographic_lincheck_parameters<FieldT> params(
        dummy_security_parameter,
        summation_domain.dimension(),
        make_zk,
        domain_type);

    holographic_multi_lincheck<FieldT> multi_lincheck(
        IOP,
        codeword_domain_handle,
        summation_domain_handle,
        input_variable_domain.dimension(),
        lincheck_matrices,
        fz_handle,
        Mz_handles,
        params);
    multi_lincheck.set_index_oracles(index_domain_handle, indexed_handles);

    multi_lincheck.register_challenge_alpha();
    multi_lincheck.register_response_alpha();
    multi_lincheck.register_challenge_beta();
    multi_lincheck.register_response_beta();

    IOP.seal_interaction_registrations();
    IOP.seal_query_registrations();
    for (size_t i = 0; i < matrices.size(); i++)
    {
        matrix_indexers[i]->compute_oracles();
    }
    IOP.signal_index_submissions_done();
    IOP.submit_oracle(fz_handle, fz_over_codeword_domain);
    for (std::size_t i = 0; i < matrices.size(); i++) {
        IOP.submit_oracle(Mz_handles[i], Mzs_over_codeword_domain[i]);
    }
    multi_lincheck.submit_sumcheck_masking_polynomials();
    IOP.signal_prover_round_done();
    multi_lincheck.calculate_response_alpha();
    IOP.signal_prover_round_done();
    multi_lincheck.calculate_response_beta();
    IOP.signal_prover_round_done();

    for (size_t repetition = 0; repetition < params.num_repetitions(); repetition++)
    {
        std::vector<oracle_handle_ptr> handles =
            multi_lincheck.get_all_oracle_handles_for_repetition(repetition);
        oracle_handle_ptr rational_sumcheck_q_handle = handles[handles.size() - 1];
        handles.pop_back();
        test_oracles_degree_and_consistency(IOP, handles, codeword_domain, exp_pass);
        /** Test rational sumcheck q separately because our indexing causes sub-maximal
         *  degree polynomials.    */
        const std::vector<FieldT> rational_sumcheck_q_evals =
            *IOP.get_oracle_evaluations(rational_sumcheck_q_handle).get();
        const std::size_t expected_degree = IOP.get_oracle_degree(rational_sumcheck_q_handle);
        const std::size_t actual_degree = degree_bound_from_evals(
            rational_sumcheck_q_evals, codeword_domain);
        // printf("actual degree %lu, expected degree %lu\n", actual_degree, expected_degree);
        ASSERT_LE(actual_degree, expected_degree) << "rational sumcheck q did not have correct degree.";
    }
}

template<typename FieldT>
std::vector<FieldT> calculate_Mz(r1cs_sparse_matrix<FieldT> M,
    std::vector<FieldT> z) {
    std::vector<FieldT> Mz;
    for (size_t i = 0; i < M.num_rows(); ++i)
    {
        FieldT Mz_i = FieldT::zero();
        linear_combination<FieldT> row = M.get_row(i);
        for (auto &lt : row.terms)
        {
            Mz_i += z[lt.index_] * lt.coeff_;
        }
        Mz.emplace_back(Mz_i);
    }
    return Mz;
}

template<typename FieldT>
std::vector<FieldT> LDE_fz(std::vector<FieldT> z,
                           field_subset<FieldT> variable_domain,
                           field_subset<FieldT> input_variable_domain) {
    std::vector<FieldT> z_rearranged;
    z_rearranged.resize(variable_domain.num_elements());
    for (std::size_t i = 0; i < variable_domain.num_elements(); i++) {
        std::size_t index = variable_domain.reindex_by_subset(input_variable_domain.dimension(), i);
        z_rearranged[index] = z[i];
    }
    return IFFT_over_field_subset(z_rearranged, variable_domain);
}

template<typename FieldT>
void run_random_multi_lincheck_instance(std::size_t matrix_dim,
                                        std::size_t input_variable_domain_dim,
                                        bool make_zk,
                                        size_t num_matrices,
                                        field_subset_type domain_type)
{
    // TODO: Remove r1cs generation code in favor of just generating M randomly
    const std::size_t num_constraints = 1 << matrix_dim;
    const std::size_t num_inputs = (1 << input_variable_domain_dim) - 1;
    const std::size_t num_variables = (1 << matrix_dim) - 1;
    r1cs_example<FieldT> r1cs_params = generate_r1cs_example<FieldT>(
        num_constraints, num_inputs, num_variables);

    const std::size_t codeword_domain_dim = 4 + matrix_dim;
    FieldT shift;
    if (domain_type == affine_subspace_type) {
        shift = FieldT(1ull << codeword_domain_dim);
    } else if (domain_type == multiplicative_coset_type) {
        shift = FieldT::multiplicative_generator;
    }

    const field_subset<FieldT> summation_domain(1ull << matrix_dim);
    const field_subset<FieldT> input_variable_domain(1ull << input_variable_domain_dim);
    const field_subset<FieldT> codeword_domain(1ull << codeword_domain_dim, shift);
    std::vector<FieldT> variable_assignment({ FieldT::one() });
    variable_assignment.insert(variable_assignment.end(),
        r1cs_params.primary_input_.begin(), r1cs_params.primary_input_.end());
    variable_assignment.insert(variable_assignment.end(),
        r1cs_params.auxiliary_input_.begin(), r1cs_params.auxiliary_input_.end());

    const std::size_t query_bound = num_inputs + 2;
    // calculate Mz
    std::shared_ptr<r1cs_constraint_system<FieldT> > cs =
        std::make_shared<r1cs_constraint_system<FieldT> >(r1cs_params.constraint_system_);
    std::vector<r1cs_sparse_matrix_type> matrix_types({
        r1cs_sparse_matrix_A, r1cs_sparse_matrix_B, r1cs_sparse_matrix_C
    });
    std::vector<r1cs_sparse_matrix<FieldT>> matrices;
    std::vector<std::vector<FieldT>> Mzs_over_codeword_domain;
    for (size_t i = 0; i < num_matrices; i++)
    {
        matrices.emplace_back(r1cs_sparse_matrix<FieldT>(cs, matrix_types[i]));
        const std::vector<FieldT> Mz = calculate_Mz(matrices[i], variable_assignment);
        std::vector<FieldT> Mz_over_codeword_domain = FFT_over_field_subset(
            IFFT_over_field_subset(Mz, summation_domain), codeword_domain);
        if (make_zk) {
            Mz_over_codeword_domain = make_codeword_zk(Mz_over_codeword_domain, query_bound,
                summation_domain, codeword_domain);
        }
        Mzs_over_codeword_domain.emplace_back(Mz_over_codeword_domain);
    }
    std::vector<FieldT> fz_over_codeword_domain = FFT_over_field_subset(
        LDE_fz(variable_assignment, summation_domain, input_variable_domain), codeword_domain);
    if (make_zk) {
        fz_over_codeword_domain = make_codeword_zk(fz_over_codeword_domain, query_bound,
            summation_domain, codeword_domain);
    }

    run_black_box_multi_lincheck_test(summation_domain, input_variable_domain, codeword_domain,
        Mzs_over_codeword_domain, fz_over_codeword_domain, matrices,
        domain_type, make_zk, query_bound, true);
    // run_multi_lincheck_test(summation_domain, input_variable_domain, codeword_domain,
    //     {Mz_over_codeword_domain}, fz_over_codeword_domain, {M},
    //     domain_type, make_zk, query_bound, true, FieldT::random_element(), {FieldT::one()});
}

TEST(AdditiveSucceedingTests, LincheckTest) {
    typedef gf64 FieldT;
    for (std::size_t matrix_dim = 6; matrix_dim < 8; matrix_dim++) {
        for (size_t num_matrices = 1; num_matrices < 4; num_matrices++)
        {
            std::size_t input_variable_domain_dim = matrix_dim - 2;
            for (std::size_t make_zk_param = 0; make_zk_param < 2; make_zk_param++) {
                run_random_multi_lincheck_instance<FieldT>(
                    matrix_dim,
                    input_variable_domain_dim,
                    (make_zk_param == 1),
                    num_matrices,
                    affine_subspace_type);
            }
        }
    }
}

TEST(MultiplicativeSucceedingTests, LincheckTest) {
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;
    for (std::size_t matrix_dim = 6; matrix_dim < 8; matrix_dim++)
    {
        for (size_t num_matrices = 1; num_matrices < 4; num_matrices++)
        {
            std::size_t input_variable_domain_dim = matrix_dim - 2;
            for (std::size_t make_zk_param = 0; make_zk_param < 2; make_zk_param++) {
                run_random_multi_lincheck_instance<FieldT>(
                    matrix_dim,
                    input_variable_domain_dim,
                    (make_zk_param == 1),
                    num_matrices,
                    multiplicative_coset_type);
            }
        }
    }
}

// This tests the following failing scenarios:
// one element is wrong in fz
// TODO: Add more failing scenarios
template<typename FieldT>
void run_failing_single_lincheck_instances(std::size_t matrix_dim,
                                  std::size_t input_variable_domain_dim,
                                  field_subset_type domain_type,
                                  bool make_zk)
{
    const std::size_t num_constraints = 1 << matrix_dim;
    const std::size_t num_inputs = (1 << input_variable_domain_dim) - 1;
    const std::size_t num_variables = (1 << matrix_dim) - 1;
    r1cs_example<FieldT> r1cs_params = generate_r1cs_example<FieldT>(
        num_constraints, num_inputs, num_variables);

    const std::size_t codeword_domain_dim = 4 + matrix_dim;
    FieldT shift;
    if (domain_type == affine_subspace_type) {
        shift = FieldT(1ull << codeword_domain_dim);
    } else if (domain_type == multiplicative_coset_type) {
        shift = FieldT::multiplicative_generator;
    }

    const field_subset<FieldT> summation_domain(1ull << matrix_dim);
    const field_subset<FieldT> input_variable_domain(1ull << input_variable_domain_dim);
    const field_subset<FieldT> codeword_domain(1ull << codeword_domain_dim, shift);
    std::vector<FieldT> variable_assignment({ FieldT::one() });
    variable_assignment.insert(variable_assignment.end(),
        r1cs_params.primary_input_.begin(), r1cs_params.primary_input_.end());
    variable_assignment.insert(variable_assignment.end(),
        r1cs_params.auxiliary_input_.begin(), r1cs_params.auxiliary_input_.end());

    const std::size_t query_bound = num_inputs + 2;
    // calculate Mz
    std::shared_ptr<r1cs_constraint_system<FieldT> > cs =
        std::make_shared<r1cs_constraint_system<FieldT> >(r1cs_params.constraint_system_);
    r1cs_sparse_matrix<FieldT> M(cs, r1cs_sparse_matrix_A);
    const std::vector<FieldT> Mz = calculate_Mz(M, variable_assignment);
    std::vector<FieldT> Mz_over_codeword_domain = FFT_over_field_subset(
        IFFT_over_field_subset(Mz, summation_domain), codeword_domain);
    // Alter one element of auxiliary input,
    std::size_t index = std::rand() % num_variables;
    variable_assignment[index] = FieldT::random_element();
    std::vector<FieldT> fz_over_codeword_domain = FFT_over_field_subset(
        LDE_fz(variable_assignment, summation_domain, input_variable_domain), codeword_domain);
    if (make_zk) {
        Mz_over_codeword_domain = make_codeword_zk(Mz_over_codeword_domain, query_bound,
            summation_domain, codeword_domain);
        fz_over_codeword_domain = make_codeword_zk(fz_over_codeword_domain, query_bound,
            summation_domain, codeword_domain);
    }

    run_black_box_multi_lincheck_test(summation_domain, input_variable_domain, codeword_domain,
        {Mz_over_codeword_domain}, fz_over_codeword_domain, {M},
        domain_type, make_zk, query_bound, false);
    // run_multi_lincheck_test(summation_domain, input_variable_domain, codeword_domain,
    //     {Mz_over_codeword_domain}, fz_over_codeword_domain, {M},
    //     domain_type, make_zk, query_bound, false, FieldT::random_element(), {FieldT::one()});
}

TEST(AdditiveFailingTests, LincheckTest) {
    typedef gf64 FieldT;
    run_failing_single_lincheck_instances<FieldT>(7, 5, affine_subspace_type, false);
    run_failing_single_lincheck_instances<FieldT>(7, 5, affine_subspace_type, true);
}

TEST(MultiplicativeFailingTests, LincheckTest) {
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;
    run_failing_single_lincheck_instances<FieldT>(7, 5, multiplicative_coset_type, false);
    run_failing_single_lincheck_instances<FieldT>(7, 5, multiplicative_coset_type, true);
}

}