#include <cstdint>
#include <stdexcept>

#include <gtest/gtest.h>

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/common/common.hpp"
#include "libiop/protocols/encoded/common/rational_linear_combination.hpp"
#include "libiop/tests/protocols/utilities.cpp"

namespace libiop {

template<typename FieldT>
void run_test(const field_subset<FieldT> systematic_domain,
              const field_subset<FieldT> codeword_domain,
              const std::vector<std::vector<FieldT>> numerator_codewords,
              const std::vector<std::vector<FieldT>> denominator_codewords)
{
    iop_protocol<FieldT> IOP;
    const domain_handle systematic_domain_handle = IOP.register_domain(systematic_domain);
    const domain_handle codeword_domain_handle = IOP.register_domain(codeword_domain);
    const size_t num_rationals = numerator_codewords.size();

    std::vector<oracle_handle_ptr> numerator_oracles(num_rationals);
    std::vector<oracle_handle_ptr> denominator_oracles(num_rationals);
    bool make_zk = false;
    for (size_t i = 0; i < num_rationals; i++)
    {
        numerator_oracles[i] = std::make_shared<oracle_handle>(
            IOP.register_oracle(codeword_domain_handle, systematic_domain.num_elements(), make_zk));
        denominator_oracles[i] = std::make_shared<oracle_handle>(
            IOP.register_oracle(codeword_domain_handle, systematic_domain.num_elements(), make_zk));
    }
    rational_linear_combination<FieldT> combination(
        IOP, num_rationals, numerator_oracles, denominator_oracles);

    auto verifier_msg_handle = IOP.register_verifier_random_message(num_rationals);
    /* Dummy registration */
    IOP.register_oracle(codeword_domain_handle, 1, make_zk);
    IOP.seal_interaction_registrations();
    IOP.seal_query_registrations();
    for (size_t i = 0; i < num_rationals; i++)
    {
        IOP.submit_oracle(numerator_oracles[i], numerator_codewords[i]);
        IOP.submit_oracle(denominator_oracles[i], denominator_codewords[i]);
    }
    std::vector<FieldT> rand_coeffs = IOP.obtain_verifier_random_message(verifier_msg_handle);
    combination.set_coefficients(rand_coeffs);

    bool expect_pass = true;
    test_oracles_degree_and_consistency(
        IOP,
        {combination.get_numerator_handle(), combination.get_denominator_handle()},
        codeword_domain,
        expect_pass);

    std::vector<FieldT> combined_numerator_codeword =
        *IOP.get_oracle_evaluations(combination.get_numerator_handle()).get();
    std::vector<FieldT> combined_denominator_codeword =
        *IOP.get_oracle_evaluations(combination.get_denominator_handle()).get();

    /** Check that the resultant codeowrd is actually a random linear combination */
    for (size_t i = 0; i < codeword_domain.num_elements(); i++)
    {
        FieldT actual = combined_numerator_codeword[i] * combined_denominator_codeword[i].inverse();
        FieldT expected = FieldT::zero();
        for (size_t j = 0; j < num_rationals; j++)
        {
            expected += rand_coeffs[j] * numerator_codewords[j][i] *
                (denominator_codewords[j][i].inverse());
        }
        ASSERT_TRUE(actual == expected);
    }
}

template<typename FieldT>
void run_random_test(const size_t systematic_domain_dim, const size_t num_rationals) {
    const size_t codeword_domain_dim = systematic_domain_dim + 2;
    const field_subset<FieldT> unshifted_codeword_domain(1ull << codeword_domain_dim);
    const FieldT codeword_domain_shift = unshifted_codeword_domain.element_outside_of_subset();

    const field_subset<FieldT> systematic_domain(1ull << systematic_domain_dim);
    const field_subset<FieldT> codeword_domain(1ull << codeword_domain_dim, codeword_domain_shift);

    const std::vector<std::vector<FieldT>> numerator_codewords =
        create_n_codewords_of_given_degree(
            codeword_domain, 1ull << systematic_domain_dim, num_rationals);
    const std::vector<std::vector<FieldT>> denominator_codewords =
        create_n_codewords_of_given_degree(
            codeword_domain, 1ull << systematic_domain_dim, num_rationals);
    run_test(systematic_domain, codeword_domain, numerator_codewords, denominator_codewords);
}

TEST(AdditiveSucceedingTests, RowcheckTest) {
    typedef gf64 FieldT;
    for (std::size_t systematic_domain_dim = 6; systematic_domain_dim < 8; systematic_domain_dim++) {
        for (std::size_t num_rationals = 1; num_rationals < 4; num_rationals++) {
            run_random_test<FieldT>(
                systematic_domain_dim,
                num_rationals);
        }
    }
}

TEST(MultiplicativeSucceedingTests, RowcheckTest) {
    alt_bn128_pp::init_public_params();
    typedef alt_bn128_Fr FieldT;
    for (std::size_t systematic_domain_dim = 6; systematic_domain_dim < 8; systematic_domain_dim++) {
        for (std::size_t num_rationals = 1; num_rationals < 4; num_rationals++) {
            run_random_test<FieldT>(
                systematic_domain_dim,
                num_rationals);
        }
    }
}

}