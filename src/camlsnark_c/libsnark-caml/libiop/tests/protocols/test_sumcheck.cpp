#include <cstdint>
#include <memory>
#include <stdexcept>

#include <gtest/gtest.h>

#include "libiop/algebra/fft.hpp"
#include "libiop/common/common.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/protocols/encoded/sumcheck/sumcheck.hpp"
#include "libiop/tests/protocols/utilities.cpp"

namespace libiop {

template<typename FieldT>
void run_sumcheck_test(const std::size_t summation_domain_dim,
                       const std::size_t codeword_domain_dim,
                       const std::size_t poly_degree_bound,
                       const std::vector<polynomial<FieldT>> polys_to_sumcheck,
                       const std::vector<FieldT> expected_sums,
                       bool make_zk, field_subset_type domain_type, bool exp_pass)
{
    const std::size_t summation_domain_size = 1ull << summation_domain_dim;
    const std::size_t codeword_domain_size = 1ull << codeword_domain_dim;
    const field_subset<FieldT> unshifted_codeword_domain(1ull << codeword_domain_dim);
    const FieldT codeword_domain_shift = unshifted_codeword_domain.element_outside_of_subset();

    const field_subset<FieldT> summation_domain(summation_domain_size);
    const field_subset<FieldT> codeword_domain(codeword_domain_size, codeword_domain_shift);

    /* Set up the blueprint for the protocol */
    iop_protocol<FieldT> IOP;
    const domain_handle summation_domain_handle = IOP.register_domain(summation_domain);
    const domain_handle codeword_domain_handle = IOP.register_domain(codeword_domain);

    std::vector<oracle_handle> poly_oracle_handles;
    poly_oracle_handles.resize(polys_to_sumcheck.size());
    for (std::size_t i = 0; i < polys_to_sumcheck.size(); i++) {
        poly_oracle_handles[i] = IOP.register_oracle(codeword_domain_handle, poly_degree_bound, make_zk);
    }

    batch_sumcheck_protocol<FieldT> sumcheck(IOP,
                                             summation_domain_handle,
                                             codeword_domain_handle,
                                             poly_degree_bound,
                                             make_zk,
                                             domain_type);
    if (make_zk) {
        sumcheck.register_masking_polynomial();
    }
    for (std::size_t i = 0; i < polys_to_sumcheck.size(); i++) {
        sumcheck.attach_oracle_for_summing(std::make_shared<oracle_handle>(poly_oracle_handles[i]), expected_sums[i]);
    }

    sumcheck.register_challenge();
    /* in principle there could have been other IOP messages exchanged in between these calls */
    sumcheck.register_proof();
    IOP.seal_interaction_registrations();

    IOP.seal_query_registrations();

    /* Proving */
    for (std::size_t i = 0; i < polys_to_sumcheck.size(); i++) {
        std::vector<FieldT> poly_over_codeword_domain = FFT_over_field_subset(
            polys_to_sumcheck[i].coefficients(), codeword_domain);
        oracle<FieldT> poly_oracle = oracle<FieldT>(poly_over_codeword_domain);
        IOP.submit_oracle(poly_oracle_handles[i], std::move(poly_oracle));
    }

    if (make_zk) {
        sumcheck.submit_masking_polynomial();
    }

    /* again, there could have been interleaved things here */
    IOP.signal_prover_round_done();
    sumcheck.calculate_and_submit_proof();
    IOP.signal_prover_round_done();

    /* Verification -- do a naive LDT on all oracles */
    const std::vector<FieldT> masking_poly_evals = *IOP.get_oracle_evaluations(
        std::make_shared<oracle_handle>(sumcheck.get_masking_poly_oracle_handle())).get();
    const std::vector<FieldT> h_evals = *IOP.get_oracle_evaluations(
        std::make_shared<oracle_handle>(sumcheck.get_h_oracle_handle())).get();
    const std::vector<FieldT> g_evals = *IOP.get_oracle_evaluations(
        std::make_shared<virtual_oracle_handle>(sumcheck.get_g_oracle_handle())).get();

    // Ensure that evaluation at point matches oracle evaluations for a few points
    for (std::size_t i = 0; i < 5; i++) {
        std::size_t evaluation_index = std::rand() % codeword_domain.num_elements();
        FieldT eval = IOP.get_oracle_evaluation_at_point(
            std::make_shared<virtual_oracle_handle>(sumcheck.get_g_oracle_handle()),
            evaluation_index
        );
        EXPECT_TRUE(eval == g_evals[evaluation_index]) << "sumcheck evaluation at point was inconsistent";
    }

    const std::size_t masking_poly_degree_bound =
        degree_bound_from_evals(masking_poly_evals, codeword_domain);
    const std::size_t h_degree_bound =
        degree_bound_from_evals(h_evals, codeword_domain);
    const std::size_t g_degree_bound =
        degree_bound_from_evals(g_evals, codeword_domain);

    if (make_zk) {
        EXPECT_EQ(masking_poly_degree_bound, poly_degree_bound);
    }

    if (exp_pass) {
        EXPECT_LE(h_degree_bound, poly_degree_bound - summation_domain.num_elements());
        EXPECT_LE(g_degree_bound, summation_domain.num_elements() - 1);
    } else {
        EXPECT_GE(h_degree_bound, poly_degree_bound - summation_domain.num_elements());
        EXPECT_GT(g_degree_bound, summation_domain.num_elements() - 1);
    }
}

template<typename FieldT>
std::pair<std::vector<polynomial<FieldT>>, std::vector<FieldT>>get_random_polys_and_sum(
    const std::size_t num_polys,
    const field_subset<FieldT> summation_domain,
    const std::size_t poly_degree_bound,
    const field_subset_type domain_type) {

    std::vector<FieldT> betas;
    std::vector<polynomial<FieldT>> sumcheck_polynomials;
    betas.resize(num_polys);
    sumcheck_polynomials.resize(num_polys);
    for (std::size_t i = 0; i < num_polys; i++) {
        sumcheck_polynomials[i] = polynomial<FieldT>::random_polynomial(poly_degree_bound);
        betas[i] = sum_over_default_field_subset(
            sumcheck_polynomials[i], summation_domain);
    }
    return std::make_pair(sumcheck_polynomials, betas);
}

template<typename FieldT>
void run_random_sumcheck_test(const std::size_t summation_domain_dim,
              const std::size_t codeword_domain_dim,
              const std::size_t poly_degree_bound,
              const std::size_t num_polys,
              const bool make_zk,
              const field_subset_type domain_type)
{
    const field_subset<FieldT> summation_domain(1ull << summation_domain_dim);
    const std::pair<std::vector<polynomial<FieldT>>, std::vector<FieldT>> polynomials_and_sums =
        get_random_polys_and_sum(num_polys, summation_domain, poly_degree_bound, domain_type);

    run_sumcheck_test(summation_domain_dim, codeword_domain_dim, poly_degree_bound,
        polynomials_and_sums.first, polynomials_and_sums.second, make_zk, domain_type, true);
}

/** SumcheckDegreeBoundTest tests ensures correctness of the above helper function,
 *  degree_bound_from_evals */
TEST(SumcheckDegreeBoundTest, SimpleTest) {
    typedef gf64 FieldT;

    const std::size_t dim = 10;
    const affine_subspace<FieldT> domain =
        linear_subspace<FieldT>::standard_basis(dim);

    for (std::size_t degree_bound = 1; degree_bound < domain.num_elements(); ++degree_bound)
    {
        const auto poly = polynomial<FieldT>::random_polynomial(degree_bound);
        const std::vector<FieldT> evals = additive_FFT_wrapper<FieldT>(poly.coefficients(), domain);
        const std::size_t computed_degree_bound = degree_bound_from_evals(evals, field_subset<FieldT>(domain));
        EXPECT_EQ(degree_bound, computed_degree_bound);
    }
}

TEST(SumcheckAdditiveLemmaTest, SimpleTest) {
    /** SumcheckAdditiveLemmaTest ensures that the lemma in the sumcheck holds true. */
    typedef gf64 FieldT;

    const std::size_t summation_domain_dim = 10;
    const std::size_t poly_degree_bound = 1234; /* a bit larger than summation domain */

    const affine_subspace<FieldT> summation_domain =
        linear_subspace<FieldT>::standard_basis(summation_domain_dim);
    const field_subset<FieldT> summation_subset = field_subset<FieldT>(summation_domain);

    const std::pair<std::vector<polynomial<FieldT>>, std::vector<FieldT>> poly_and_sum =
        get_random_polys_and_sum(1, summation_subset, poly_degree_bound, affine_subspace_type);
    const polynomial<FieldT> poly = poly_and_sum.first[0];
    const FieldT sum = poly_and_sum.second[0];

    /* by Lemma 5.4 everything vanishes except \beta * \sum_{a \in H} a^{|H|-1},
       and by Lemma 5.5 this power sum can also be computed from vanishing polynomial */
    FieldT power_sum = FieldT::zero();
    for (auto &el : summation_domain.all_elements())
    {
        power_sum += libiop::power(el, summation_domain.num_elements() - 1);
    }
    const linearized_polynomial<FieldT> Z =
        vanishing_polynomial_from_subspace(summation_domain);
    const FieldT eps = Z[1]; /* coefficient for the linear term */
    EXPECT_EQ(eps, power_sum);

    const std::size_t codeword_domain_dim = 13;
    const affine_subspace<FieldT> codeword_domain =
        linear_subspace<FieldT>::standard_basis(codeword_domain_dim);
    const std::size_t computed_poly_degree_bound = degree_bound_from_evals(
        additive_FFT_wrapper<FieldT>(poly.coefficients(), codeword_domain),
        field_subset<FieldT>(codeword_domain));
    EXPECT_EQ(computed_poly_degree_bound, poly_degree_bound);
}

TEST(SumcheckAdditiveTestGf64, SimpleTest) {
    typedef gf64 FieldT;

    const std::size_t summation_domain_dim = 8;
    const std::size_t codeword_domain_dim_11 = 11;
    /* somewhere in the middle between the sizes of summation and codeword spaces */
    const std::size_t poly_degree_bound_9_bit = 500;
    const std::size_t poly_degree_bound_11_bit = 1 << 11;
    const std::size_t codeword_domain_dim_13 = 13;

    run_random_sumcheck_test<FieldT>(summation_domain_dim, codeword_domain_dim_11, poly_degree_bound_9_bit, 3, true, affine_subspace_type);
    run_random_sumcheck_test<FieldT>(summation_domain_dim, codeword_domain_dim_11, poly_degree_bound_9_bit, 3, false, affine_subspace_type);
    run_random_sumcheck_test<FieldT>(summation_domain_dim, codeword_domain_dim_13, poly_degree_bound_11_bit, 4, true, affine_subspace_type);
}

TEST(SumcheckMultiplicativeTest, SimpleTest) {
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;

    const std::size_t summation_domain_dim = 4;
    const std::size_t codeword_domain_dim = 6;
    /* somewhere in the middle between the sizes of summation and codeword spaces */
    const std::size_t poly_degree_bound = 20;

    run_random_sumcheck_test<FieldT>(summation_domain_dim, codeword_domain_dim, poly_degree_bound, 1, false, multiplicative_coset_type);
    run_random_sumcheck_test<FieldT>(summation_domain_dim, codeword_domain_dim, poly_degree_bound, 3, false, multiplicative_coset_type);
    run_random_sumcheck_test<FieldT>(summation_domain_dim, codeword_domain_dim, poly_degree_bound, 1, true, multiplicative_coset_type);
    run_random_sumcheck_test<FieldT>(summation_domain_dim, codeword_domain_dim, poly_degree_bound, 3, true, multiplicative_coset_type);
}

/** Test that when choosing random polynomials, randomly changing one sum
 *  will cause the verification to fail.
 *  TODO: create test vectors which cover more interesting cases. */
template<typename FieldT>
void test_sumcheck_on_invalid_input(const field_subset_type domain_type) {
    const std::size_t summation_domain_dim = 8;
    const std::size_t codeword_domain_dim = 12;
    const std::size_t poly_degree_bound = 500;
    const field_subset<FieldT> summation_subset(1ull << summation_domain_dim);

    /* Create test expected sums, and polynomials */
    std::pair<std::vector<polynomial<FieldT>>, std::vector<FieldT>> polynomials_and_sums =
        get_random_polys_and_sum(2, summation_subset, poly_degree_bound, domain_type);
    std::vector<polynomial<FieldT>> sumcheck_polynomials = polynomials_and_sums.first;
    std::vector<FieldT> betas  = polynomials_and_sums.second;

    /** First try changing the first, and then the last expected sum only.
     *  This is to ensure that the amortization logic doesn't ignore either of the two.
     */
    /* Change first of the expected sums */
    FieldT betaCpy = betas[0];
    betas[0] = FieldT::random_element();
    run_sumcheck_test(summation_domain_dim, codeword_domain_dim, poly_degree_bound,
        sumcheck_polynomials, betas, true, domain_type, false);  // zk case
    run_sumcheck_test(summation_domain_dim, codeword_domain_dim, poly_degree_bound,
        sumcheck_polynomials, betas, false, domain_type, false); // non-zk

    /* Change second of the expected sums */
    betas[0] = betaCpy;
    betas[1] = FieldT::random_element();
    run_sumcheck_test(summation_domain_dim, codeword_domain_dim, poly_degree_bound,
        sumcheck_polynomials, betas, true, domain_type, false);  // zk case
    run_sumcheck_test(summation_domain_dim, codeword_domain_dim, poly_degree_bound,
        sumcheck_polynomials, betas, false, domain_type, false); // non-zk

    /* Run sumcheck with two polynomials, f(x) and g(x)
     * * f claims to sum to g's value
     * * g claims to sum to f's value
     * This ensures the polynomials have a random linear combination, and aren't just summed. */
    polynomials_and_sums =
        get_random_polys_and_sum(2, summation_subset, poly_degree_bound, domain_type);
    sumcheck_polynomials = polynomials_and_sums.first;
    betas = polynomials_and_sums.second;
    betaCpy = betas[0];
    betas[0] = betas[1];
    betas[1] = betaCpy;
    run_sumcheck_test(summation_domain_dim, codeword_domain_dim, poly_degree_bound,
        sumcheck_polynomials, betas, true, domain_type, false);  // zk case
    run_sumcheck_test(summation_domain_dim, codeword_domain_dim, poly_degree_bound,
        sumcheck_polynomials, betas, false, domain_type, false); // non-zk
}

TEST(SumcheckFailingAdditiveTestsGf64, SimpleTest) {
    typedef gf64 FieldT;
    test_sumcheck_on_invalid_input<FieldT>(affine_subspace_type);
}

TEST(SumcheckFailingMultiplicativeTestsGf64, SimpleTest) {
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;
    test_sumcheck_on_invalid_input<FieldT>(multiplicative_coset_type);
}

}
