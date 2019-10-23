#include <cstdint>
#include <memory>
#include <stdexcept>

#include <gtest/gtest.h>

#include "libiop/algebra/fft.hpp"
#include "libiop/common/common.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/protocols/encoded/sumcheck/rational_sumcheck.hpp"
#include "libiop/tests/protocols/utilities.cpp"

namespace libiop {

template<typename FieldT>
void run_rational_sumcheck_test(
    const std::size_t summation_domain_dim,
    const std::size_t codeword_domain_dim,
    const polynomial<FieldT> numerator,
    const polynomial<FieldT> denominator,
    const std::vector<FieldT> rational_over_summation_domain,
    const FieldT expected_sum,
    field_subset_type domain_type, bool exp_pass)
{
    const std::size_t summation_domain_size = 1ull << summation_domain_dim;
    const std::size_t codeword_domain_size = 1ull << codeword_domain_dim;
    const field_subset<FieldT> unshifted_codeword_domain(codeword_domain_size);
    const FieldT codeword_domain_shift = unshifted_codeword_domain.element_outside_of_subset();

    const field_subset<FieldT> summation_domain(summation_domain_size);
    const field_subset<FieldT> codeword_domain(codeword_domain_size, codeword_domain_shift);

    /* Set up the blueprint for the protocol */
    iop_protocol<FieldT> IOP;
    const domain_handle summation_domain_handle = IOP.register_domain(summation_domain);
    const domain_handle codeword_domain_handle = IOP.register_domain(codeword_domain);
    const bool make_zk = false;
    const size_t numerator_degree_bound = numerator.num_terms();
    const size_t denominator_degree_bound = denominator.num_terms();

    oracle_handle numerator_oracle_handle =
        IOP.register_oracle(codeword_domain_handle, numerator_degree_bound, make_zk);
    oracle_handle denominator_oracle_handle =
        IOP.register_oracle(codeword_domain_handle, denominator_degree_bound, make_zk);

    rational_sumcheck_protocol<FieldT> sumcheck(
        IOP,
        summation_domain_handle,
        codeword_domain_handle,
        numerator_degree_bound,
        denominator_degree_bound,
        domain_type);

    sumcheck.register_summation_oracle(
        std::make_shared<oracle_handle>(numerator_oracle_handle),
        std::make_shared<oracle_handle>(denominator_oracle_handle));
    sumcheck.register_proof();
    IOP.seal_interaction_registrations();
    IOP.seal_query_registrations();

    /* Proving */
    std::vector<FieldT> numerator_over_codeword_domain =
        FFT_over_field_subset<FieldT>(numerator.coefficients(), codeword_domain);
    oracle<FieldT> numerator_oracle = oracle<FieldT>(numerator_over_codeword_domain);
    IOP.submit_oracle(numerator_oracle_handle, std::move(numerator_oracle));

    std::vector<FieldT> denominator_over_codeword_domain =
        FFT_over_field_subset<FieldT>(denominator.coefficients(), codeword_domain);
    oracle<FieldT> denominator_oracle = oracle<FieldT>(denominator_over_codeword_domain);
    IOP.submit_oracle(denominator_oracle_handle, std::move(denominator_oracle));

    sumcheck.calculate_and_submit_proof(rational_over_summation_domain);
    IOP.signal_prover_round_done();

    /* Verification */
    sumcheck.construct_verifier_state(expected_sum);
    test_oracles_degree_and_consistency(
        IOP, {sumcheck.get_reextended_oracle_handle()}, codeword_domain, true);
    test_oracles_degree_and_consistency(
        IOP, {sumcheck.get_constraint_oracle_handle()}, codeword_domain, exp_pass);
}

template<typename FieldT>
std::tuple<polynomial<FieldT>, polynomial<FieldT>, FieldT> get_random_rational_and_sum(
    const std::size_t numerator_degree_bound,
    const std::size_t denominator_degree_bound,
    const field_subset<FieldT> summation_domain)
{
    polynomial<FieldT> numerator =
        polynomial<FieldT>::random_polynomial(numerator_degree_bound);
    polynomial<FieldT> denominator =
        polynomial<FieldT>::random_polynomial(denominator_degree_bound);
    FieldT sum = sum_of_rational_over_domain(numerator, denominator, summation_domain);

    return std::make_tuple(numerator, denominator, sum);
}

template<typename FieldT>
void run_random_sumcheck_test(
    const std::size_t summation_domain_dim,
    const std::size_t codeword_domain_dim,
    const std::size_t numerator_degree_bound,
    const std::size_t denominator_degree_bound,
    const field_subset_type domain_type)
{
    const field_subset<FieldT> summation_domain(1ull << summation_domain_dim);
    const std::tuple<polynomial<FieldT>, polynomial<FieldT>, FieldT>
        rational_and_sum =
        get_random_rational_and_sum(numerator_degree_bound, denominator_degree_bound, summation_domain);

    const polynomial<FieldT> numerator = std::get<0>(rational_and_sum);
    const polynomial<FieldT> denominator = std::get<1>(rational_and_sum);
    const std::vector<FieldT> rational_over_summation_domain =
        compute_rational_over_domain<FieldT>(numerator, denominator, summation_domain);

    bool expect_constraint_pass = true;
    run_rational_sumcheck_test(
        summation_domain_dim, codeword_domain_dim,
        numerator, denominator, rational_over_summation_domain,
        std::get<2>(rational_and_sum),
        domain_type, expect_constraint_pass);
}

TEST(SumcheckAdditiveTestGf64, SimpleTest) {
    typedef gf64 FieldT;

    const std::size_t summation_domain_dim = 5;
    const std::size_t codeword_domain_dim = 9;
    /* somewhere in the middle between the sizes of summation and codeword spaces */
    const std::size_t numerator_degree_bound_small = 1 << 6;
    const std::size_t numerator_degree_bound_large = 1 << 8;
    const std::size_t denominator_degree_bound = 1 << 7;

    run_random_sumcheck_test<FieldT>(summation_domain_dim,
                                     codeword_domain_dim,
                                     numerator_degree_bound_small,
                                     denominator_degree_bound,
                                     affine_subspace_type);

    run_random_sumcheck_test<FieldT>(summation_domain_dim,
                                     codeword_domain_dim,
                                     numerator_degree_bound_large,
                                     denominator_degree_bound,
                                     affine_subspace_type);
}

TEST(SumcheckMultiplicativeTest, SimpleTest) {
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;

    const std::size_t summation_domain_dim = 5;
    const std::size_t codeword_domain_dim = 9;
    /* somewhere in the middle between the sizes of summation and codeword spaces */
    const std::size_t numerator_degree_bound_small = 1 << 6;
    const std::size_t numerator_degree_bound_large = 1 << 8;
    const std::size_t denominator_degree_bound = 1 << 7;

    run_random_sumcheck_test<FieldT>(summation_domain_dim,
                                     codeword_domain_dim,
                                     numerator_degree_bound_small,
                                     denominator_degree_bound,
                                     multiplicative_coset_type);

    run_random_sumcheck_test<FieldT>(summation_domain_dim,
                                     codeword_domain_dim,
                                     numerator_degree_bound_large,
                                     denominator_degree_bound,
                                     multiplicative_coset_type);
}

template<typename FieldT>
void run_sumcheck_soundness_tests(
    const std::size_t summation_domain_dim,
    const std::size_t codeword_domain_dim,
    const std::size_t numerator_degree_bound,
    const std::size_t denominator_degree_bound,
    const field_subset_type domain_type)
{
    /** This tests 3 situations:
     *  1) An incorrect sum is claimed
     *  2) p is an incorrect low degree extension
     *  3) both of the above
    */
    const field_subset<FieldT> summation_domain(1ull << summation_domain_dim);
    const std::tuple<polynomial<FieldT>, polynomial<FieldT>, FieldT>
        rational_and_sum =
        get_random_rational_and_sum(numerator_degree_bound, denominator_degree_bound, summation_domain);

    const polynomial<FieldT> numerator = std::get<0>(rational_and_sum);
    const polynomial<FieldT> denominator = std::get<1>(rational_and_sum);
    const std::vector<FieldT> rational_over_summation_domain =
        compute_rational_over_domain<FieldT>(numerator, denominator, summation_domain);

    /** The incorrect one has a different first element in its systematic domain. */
    std::vector<FieldT> incorrect_rational_over_summation_domain(rational_over_summation_domain);
    incorrect_rational_over_summation_domain[0] = FieldT::random_element();

    const FieldT correct_sum = std::get<2>(rational_and_sum);
    const FieldT incorrect_sum = FieldT::random_element();

    bool expect_constraint_pass = false;
    /** Test incorrect sum */
    run_rational_sumcheck_test(
        summation_domain_dim, codeword_domain_dim,
        numerator, denominator, rational_over_summation_domain,
        incorrect_sum,
        domain_type, expect_constraint_pass);
    /** Test incorrect LDE */
    run_rational_sumcheck_test(
        summation_domain_dim, codeword_domain_dim,
        numerator, denominator, incorrect_rational_over_summation_domain,
        correct_sum,
        domain_type, expect_constraint_pass);
    /** Test incorrect sum & LDE */
    run_rational_sumcheck_test(
        summation_domain_dim, codeword_domain_dim,
        numerator, denominator, incorrect_rational_over_summation_domain,
        incorrect_sum,
        domain_type, expect_constraint_pass);
}

TEST(SumcheckFailingAdditiveTestsGf64, SimpleTest) {
    typedef gf64 FieldT;

    const std::size_t summation_domain_dim = 5;
    const std::size_t codeword_domain_dim = 9;
    /* somewhere in the middle between the sizes of summation and codeword spaces */
    const std::size_t numerator_degree_bound_small = 1 << 6;
    const std::size_t numerator_degree_bound_large = 1 << 8;
    const std::size_t denominator_degree_bound = 1 << 7;

    run_sumcheck_soundness_tests<FieldT>(
        summation_domain_dim,
        codeword_domain_dim,
        numerator_degree_bound_small,
        denominator_degree_bound,
        affine_subspace_type);

    run_sumcheck_soundness_tests<FieldT>(
        summation_domain_dim,
        codeword_domain_dim,
        numerator_degree_bound_large,
        denominator_degree_bound,
        affine_subspace_type);
}

TEST(SumcheckFailingMultiplicativeTestsEdwards, SimpleTest) {
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;

    const std::size_t summation_domain_dim = 5;
    const std::size_t codeword_domain_dim = 9;
    /* somewhere in the middle between the sizes of summation and codeword spaces */
    const std::size_t numerator_degree_bound_small = 1 << 6;
    const std::size_t numerator_degree_bound_large = 1 << 8;
    const std::size_t denominator_degree_bound = 1 << 7;

    run_sumcheck_soundness_tests<FieldT>(
        summation_domain_dim,
        codeword_domain_dim,
        numerator_degree_bound_small,
        denominator_degree_bound,
        multiplicative_coset_type);

    run_sumcheck_soundness_tests<FieldT>(
        summation_domain_dim,
        codeword_domain_dim,
        numerator_degree_bound_large,
        denominator_degree_bound,
        multiplicative_coset_type);
}

}
