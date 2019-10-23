#include <cstdint>
#include <stdexcept>

#include <gtest/gtest.h>

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/common/common.hpp"
#include "libiop/protocols/encoded/lincheck/basic_lincheck.hpp"
#include "libiop/relations/examples/r1cs_examples.hpp"
#include "libiop/relations/r1cs.hpp"
#include "libiop/relations/variable.hpp"
#include "libiop/tests/protocols/utilities.cpp"

namespace libiop {

/** Runs the test on multi_lincheck, and just checks the result from sumcheck */
template<typename FieldT>
void run_black_box_multi_lincheck_test(
    const field_subset<FieldT> constraint_domain,
    const field_subset<FieldT> variable_domain,
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

    iop_protocol<FieldT> IOP;
    const domain_handle codeword_domain_handle = IOP.register_domain(codeword_domain);
    const domain_handle constraint_domain_handle = IOP.register_domain(constraint_domain);
    const domain_handle variable_domain_handle = IOP.register_domain(variable_domain);

    std::vector<std::shared_ptr<sparse_matrix<FieldT> >> lincheck_matrices;
    for (std::size_t i = 0; i < matrices.size(); i++){
        lincheck_matrices.emplace_back(
            std::make_shared<r1cs_sparse_matrix<FieldT> >(matrices[i]));
    }

    const std::size_t b = make_zk ? query_bound : 0;
    const std::size_t fz_degree = variable_domain.num_elements() + b;
    const std::size_t Mz_degree = constraint_domain.num_elements() + b;
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
    basic_lincheck_parameters<FieldT> params(dummy_security_parameter,
                                             constraint_domain.dimension(),
                                             make_zk,
                                             domain_type);

    multi_lincheck<FieldT> multi_lincheck(
        IOP,
        codeword_domain_handle,
        constraint_domain_handle,
        variable_domain_handle,
        input_variable_domain.dimension(),
        lincheck_matrices,
        fz_handle,
        Mz_handles,
        params);

    multi_lincheck.register_challenge();
    multi_lincheck.register_proof();
    IOP.seal_interaction_registrations();
    IOP.seal_query_registrations();
    IOP.submit_oracle(fz_handle, fz_over_codeword_domain);
    for (std::size_t i = 0; i < matrices.size(); i++) {
        IOP.submit_oracle(Mz_handles[i], Mzs_over_codeword_domain[i]);
    }
    multi_lincheck.submit_sumcheck_masking_polynomials();
    IOP.signal_prover_round_done();
    multi_lincheck.calculate_and_submit_proof();
    IOP.signal_prover_round_done();

    std::vector<oracle_handle_ptr> handles = multi_lincheck.get_all_oracle_handles();
    test_oracles_degree_and_consistency(IOP, handles, codeword_domain, exp_pass);
}


/* This test ensures the lincheck codeword has correct evaluations over the systematic domain.
 * Only works in the case with one matrix.*/
template<typename FieldT>
void test_single_lincheck_q(const polynomial<FieldT> &q_alpha_poly,
    const FieldT &alpha,
    const std::vector<FieldT> &r_Mz,
    const std::vector<FieldT> &Mz_over_codeword_dom,
    const std::vector<FieldT> &fz_over_codeword_dom,
    const r1cs_sparse_matrix<FieldT> &M,
    const field_subset<FieldT> &codeword_domain,
    const field_subset<FieldT> &summation_domain,
    const field_subset<FieldT> &constraint_domain,
    const field_subset<FieldT> &variable_domain,
    const std::size_t input_variable_domain_dim)
{
    std::vector<FieldT> q_alpha = q_alpha_poly.coefficients();
    // convert codewords from codeword domain to summation domain

    const std::vector<FieldT> Mz_coeff = IFFT_over_field_subset(Mz_over_codeword_dom, codeword_domain);
    const std::vector<FieldT> fz_coeff = IFFT_over_field_subset(fz_over_codeword_dom, codeword_domain);
    // use naive_FFT for the conversion to sum domain,
    // as these polynomials are of degree > summation domain
    const std::vector<FieldT> q_alpha_over_sum_dom = naive_FFT(q_alpha, summation_domain);
    const std::vector<FieldT> Mz_over_sum_dom = naive_FFT(Mz_coeff, summation_domain);
    const std::vector<FieldT> fz_over_sum_dom = naive_FFT(fz_coeff, summation_domain);

    std::vector<FieldT> alpha_powers;
    FieldT cur = FieldT::one();
    for (std::size_t i = 0; i < constraint_domain.num_elements(); i++) {
        alpha_powers.emplace_back(cur);
        cur *= alpha;
    }

    // construct p_alpha^1
    std::vector<FieldT> p_alpha_1(summation_domain.num_elements(), FieldT::zero());
    for (std::size_t i = 0; i < constraint_domain.num_elements(); i++) {
        const std::size_t index = summation_domain.reindex_by_subset(
            constraint_domain.dimension(), i);
        p_alpha_1[index] = alpha_powers[i];
    }
    // construct p_alpha^2
    std::vector<FieldT> p_alpha_2(summation_domain.num_elements(), FieldT::zero());
    for (std::size_t i = 0; i < constraint_domain.num_elements(); ++i)
    {
        const linear_combination<FieldT> row = M.get_row(i);
        for (auto &term : row.terms)
        {
            const std::size_t variable_index = variable_domain.reindex_by_subset(
                input_variable_domain_dim, term.index_);
            const std::size_t index = summation_domain.reindex_by_subset(
                variable_domain.dimension(), variable_index);
            p_alpha_2[index] += r_Mz[0] * term.coeff_ * alpha_powers[i];
        }
    }

    // check q_alpha over the systematic part
    for (std::size_t i = 0; i < summation_domain.num_elements(); ++i) {
        FieldT lhs = q_alpha_over_sum_dom[i];
        FieldT rhs = r_Mz[0] * p_alpha_1[i] * Mz_over_sum_dom[i] - p_alpha_2[i] * fz_over_sum_dom[i];
        ASSERT_TRUE(lhs == rhs) << "lincheck's q_alpha failed on the " << i << "th entry";
    }
}

/** Runs the test on multi_lincheck, and checks the outputted codewords
 *  degree, and evaluations on the systematic domain.
 */
template<typename FieldT>
void run_multi_lincheck_test(
    const field_subset<FieldT> constraint_domain,
    const field_subset<FieldT> variable_domain,
    const field_subset<FieldT> input_variable_domain,
    const field_subset<FieldT> codeword_domain,
    const std::vector<std::vector<FieldT> > Mzs_over_codeword_domain,
    const std::vector<FieldT> fz_over_codeword_domain,
    const std::vector<r1cs_sparse_matrix<FieldT> > matrices,
    const field_subset_type domain_type,
    const bool make_zk,
    const std::size_t query_bound,
    const bool exp_pass,
    const FieldT alpha,
    const std::vector<FieldT> r_Mz)
{
    std::size_t make_zk_int = make_zk ? 1 : 0;
    /* Set up input to the protocol */
    field_subset<FieldT> summation_domain;
    if (constraint_domain.dimension() > variable_domain.dimension()) {
        summation_domain = constraint_domain;
    } else {
        summation_domain = variable_domain;
    }

    std::vector<std::shared_ptr<sparse_matrix<FieldT> >> lincheck_matrices;
    for (std::size_t i = 0; i < matrices.size(); i++){
        lincheck_matrices.emplace_back(
            std::make_shared<r1cs_sparse_matrix<FieldT> >(matrices[i]));
    }

    const std::size_t b = make_zk ? query_bound : 0;
    const std::size_t fz_degree = variable_domain.num_elements() + b;
    const std::size_t Mz_degree = constraint_domain.num_elements() + b;
    const std::size_t lincheck_degree = summation_domain.num_elements() +
        std::max({fz_degree, Mz_degree}) - 1;

    multi_lincheck_virtual_oracle<FieldT> multi_lincheck(
        codeword_domain,
        constraint_domain,
        variable_domain,
        summation_domain,
        input_variable_domain.dimension(),
        lincheck_matrices);

    multi_lincheck.set_challenge(alpha, r_Mz);

    std::vector<std::shared_ptr<std::vector<FieldT>>> constituent_codewords;
    constituent_codewords.emplace_back(
        std::make_shared<std::vector<FieldT>>(fz_over_codeword_domain));
    for (std::size_t i = 0; i < Mzs_over_codeword_domain.size(); i++) {
        constituent_codewords.emplace_back(
            std::make_shared<std::vector<FieldT>>(Mzs_over_codeword_domain[i]));
    }

    std::vector<FieldT> lincheck_q = *multi_lincheck.evaluated_contents(constituent_codewords).get();
    const std::size_t actual_degree = degree_bound_from_evals(lincheck_q, codeword_domain);
    EXPECT_EQ(actual_degree, lincheck_degree);
    const polynomial<FieldT> poly_lincheck_q(IFFT_over_field_subset(lincheck_q, codeword_domain));

    if (Mzs_over_codeword_domain.size() == 1) {
        test_single_lincheck_q(poly_lincheck_q, alpha, r_Mz,
            Mzs_over_codeword_domain[0], fz_over_codeword_domain,
            matrices[0], codeword_domain, summation_domain,
            constraint_domain, variable_domain, input_variable_domain.dimension());

        for (std::size_t i = 0; i < 10; i++) {
            std::size_t evaluation_index = std::rand() % codeword_domain.num_elements();
            FieldT evaluation_elem = codeword_domain.element_by_index(evaluation_index);
            const std::vector<FieldT> constituent_evaluations(
                {fz_over_codeword_domain[evaluation_index],
                Mzs_over_codeword_domain[0][evaluation_index]});
            FieldT eval = multi_lincheck.evaluation_at_point(
                evaluation_index,
                evaluation_elem,
                constituent_evaluations);
            EXPECT_TRUE(eval == lincheck_q[evaluation_index]) << "multi_lincheck evaluation at point was inconsistent";
        }
    }

    const FieldT sum_over_domain = sum_over_default_field_subset(poly_lincheck_q, summation_domain);
    if (exp_pass) {
        EXPECT_TRUE(sum_over_domain == FieldT::zero());
    } else {
        EXPECT_FALSE(sum_over_domain == FieldT::zero()) << "multi_lincheck unexpectedly passed";
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
void run_random_multi_lincheck_instance(std::size_t constraint_domain_dim,
                                         std::size_t variable_domain_dim,
                                         std::size_t input_variable_domain_dim,
                                         bool make_zk,
                                         size_t num_matrices,
                                         field_subset_type domain_type) {
    // TODO: Remove r1cs generation code in favor of just generating M randomly
    const std::size_t num_constraints = 1 << constraint_domain_dim;
    const std::size_t num_inputs = (1 << input_variable_domain_dim) - 1;
    const std::size_t num_variables = (1 << variable_domain_dim) - 1;
    r1cs_example<FieldT> r1cs_params = generate_r1cs_example<FieldT>(
        num_constraints, num_inputs, num_variables);

    const std::size_t codeword_domain_dim = 4 +
        std::max(constraint_domain_dim, variable_domain_dim);
    FieldT shift;
    if (domain_type == affine_subspace_type) {
        shift = FieldT(1ull << codeword_domain_dim);
    } else if (domain_type == multiplicative_coset_type) {
        shift = FieldT::multiplicative_generator;
    }

    const field_subset<FieldT> constraint_domain(1ull << constraint_domain_dim);
    const field_subset<FieldT> variable_domain(1ull << variable_domain_dim);
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
            IFFT_over_field_subset(Mz, constraint_domain), codeword_domain);
        if (make_zk) {
            Mz_over_codeword_domain = make_codeword_zk(Mz_over_codeword_domain, query_bound,
                constraint_domain, codeword_domain);
        }
        Mzs_over_codeword_domain.emplace_back(Mz_over_codeword_domain);
    }
    std::vector<FieldT> fz_over_codeword_domain = FFT_over_field_subset(
        LDE_fz(variable_assignment, variable_domain, input_variable_domain), codeword_domain);
    if (make_zk) {
        fz_over_codeword_domain = make_codeword_zk(fz_over_codeword_domain, query_bound,
            variable_domain, codeword_domain);
    }

    run_black_box_multi_lincheck_test(constraint_domain, variable_domain, input_variable_domain, codeword_domain,
        Mzs_over_codeword_domain, fz_over_codeword_domain, matrices,
        domain_type, make_zk, query_bound, true);
    std::vector<FieldT> r_Mz;
    for (size_t i = 0; i < num_matrices; i++)
    {
        r_Mz.emplace_back(FieldT::random_element());
    }
    run_multi_lincheck_test(constraint_domain, variable_domain, input_variable_domain, codeword_domain,
        Mzs_over_codeword_domain, fz_over_codeword_domain, matrices,
        domain_type, make_zk, query_bound, true, FieldT::random_element(), r_Mz);
}

TEST(AdditiveSucceedingTests, LincheckTest) {
    typedef gf64 FieldT;
    for (std::size_t constraint_domain_dim = 5; constraint_domain_dim < 8; constraint_domain_dim++) {
        for (std::size_t variable_domain_dim = 5; variable_domain_dim < 8; variable_domain_dim++) {
            std::size_t input_variable_domain_dim = variable_domain_dim - 2;
            for (std::size_t make_zk_param = 0; make_zk_param < 2; make_zk_param++)
            {
                for (size_t num_matrices = 1; num_matrices < 4; num_matrices++)
                {
                    run_random_multi_lincheck_instance<FieldT>(
                        constraint_domain_dim,
                        variable_domain_dim,
                        input_variable_domain_dim,
                        (make_zk_param == 1),
                        num_matrices,
                        affine_subspace_type);
                }
            }
        }
    }
}

TEST(MultiplicativeSucceedingTests, LincheckTest) {
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;
    for (std::size_t constraint_domain_dim = 6; constraint_domain_dim < 8; constraint_domain_dim++) {
        for (std::size_t variable_domain_dim = 6; variable_domain_dim < 8; variable_domain_dim++) {
            std::size_t input_variable_domain_dim = variable_domain_dim - 2;
            for (std::size_t make_zk_param = 0; make_zk_param < 2; make_zk_param++)
            {
                for (size_t num_matrices = 1; num_matrices < 4; num_matrices++)
                {
                    run_random_multi_lincheck_instance<FieldT>(
                        constraint_domain_dim,
                        variable_domain_dim,
                        input_variable_domain_dim,
                        (make_zk_param == 1),
                        num_matrices,
                        multiplicative_coset_type);
                }
            }
        }
    }
}

// This tests the following failing scenarios:
// one element is wrong in fz
// TODO: Add more failing scenarios
template<typename FieldT>
void run_failing_single_lincheck_instances(std::size_t constraint_domain_dim,
                                  std::size_t variable_domain_dim,
                                  std::size_t input_variable_domain_dim,
                                  field_subset_type domain_type,
                                  bool make_zk)
{
    EXPECT_GE(constraint_domain_dim, variable_domain_dim) << "Due to " <<
        "r1cs_example_generator logic, only num_constraint number of variables " <<
        "have non-zero corresponding columns";
    const std::size_t num_constraints = 1 << constraint_domain_dim;
    const std::size_t num_inputs = (1 << input_variable_domain_dim) - 1;
    const std::size_t num_variables = (1 << variable_domain_dim) - 1;
    r1cs_example<FieldT> r1cs_params = generate_r1cs_example<FieldT>(
        num_constraints, num_inputs, num_variables);

    const std::size_t codeword_domain_dim = 4 +
        std::max(constraint_domain_dim, variable_domain_dim);
    FieldT shift;
    if (domain_type == affine_subspace_type) {
        shift = FieldT(1ull << codeword_domain_dim);
    } else if (domain_type == multiplicative_coset_type) {
        shift = FieldT::multiplicative_generator;
    }

    const field_subset<FieldT> constraint_domain(1ull << constraint_domain_dim);
    const field_subset<FieldT> variable_domain(1ull << variable_domain_dim);
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
        IFFT_over_field_subset(Mz, constraint_domain), codeword_domain);
    // Alter one element of auxiliary input,
    std::size_t index = std::rand() % num_variables;
    variable_assignment[index] = FieldT::random_element();
    std::vector<FieldT> fz_over_codeword_domain = FFT_over_field_subset(
        LDE_fz(variable_assignment, variable_domain, input_variable_domain), codeword_domain);
    if (make_zk) {
        Mz_over_codeword_domain = make_codeword_zk(Mz_over_codeword_domain, query_bound,
            constraint_domain, codeword_domain);
        fz_over_codeword_domain = make_codeword_zk(fz_over_codeword_domain, query_bound,
            variable_domain, codeword_domain);
    }

    run_black_box_multi_lincheck_test(constraint_domain, variable_domain, input_variable_domain, codeword_domain,
        {Mz_over_codeword_domain}, fz_over_codeword_domain, {M},
        domain_type, make_zk, query_bound, false);
    run_multi_lincheck_test(constraint_domain, variable_domain, input_variable_domain, codeword_domain,
        {Mz_over_codeword_domain}, fz_over_codeword_domain, {M},
        domain_type, make_zk, query_bound, false, FieldT::random_element(), {FieldT::one()});
}

TEST(AdditiveFailingTests, LincheckTest) {
    typedef gf64 FieldT;
    run_failing_single_lincheck_instances<FieldT>(7, 7, 5, affine_subspace_type, false);
    run_failing_single_lincheck_instances<FieldT>(7, 7, 5, affine_subspace_type, true);
}

TEST(MultiplicativeFailingTests, LincheckTest) {
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;
    run_failing_single_lincheck_instances<FieldT>(7, 7, 5, multiplicative_coset_type, false);
}

}