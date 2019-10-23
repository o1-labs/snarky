#include <algorithm>
#include <cstdint>
#include <iostream>

#include <gtest/gtest.h>

#include "libiop/algebra/fields/gf64.hpp"
#include <libff/algebra/curves/alt_bn128/alt_bn128_pp.hpp>
#include "libiop/algebra/fields/utils.hpp"
#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/field_subset/subgroup.hpp"
#include "libiop/common/common.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/protocols/ldt/fri/fri_ldt.hpp"

namespace libiop {

template<typename FieldT>
field_subset<FieldT> make_domain_helper(const std::size_t dimension,
                                        const typename libiop::enable_if<is_multiplicative<FieldT>::value, FieldT>::type elem)
{
    libiop::UNUSED(elem); // only to identify field type

    const std::size_t size = 1ull << dimension;
    return field_subset<FieldT>(size, FieldT::multiplicative_generator);
}

template<typename FieldT>
field_subset<FieldT> make_domain_helper(const std::size_t dimension,
                                        const typename libiop::enable_if<is_additive<FieldT>::value, FieldT>::type elem)
{
    libiop::UNUSED(elem); // only to identify field type

    const size_t size = 1ull << dimension;
    return field_subset<FieldT>(size, FieldT(size));
}

std::vector<std::size_t> random_vector_that_sums_to(const std::size_t sum)
{
    std::size_t remaining = sum;
    std::vector<std::size_t> vec;
    while (remaining > 1)
    {
        std::size_t next = (std::rand() % (remaining - 1)) + 1;
        vec.push_back(next);
        remaining -= next;
    }
    if (remaining == 1)
    {
        vec.push_back(1);
    }
    return vec;
}

template<typename FieldT>
bool run_test(const std::size_t codeword_domain_dim,
              const std::vector<std::size_t> localization_parameter_array,
              const std::size_t RS_extra_dimensions,
              const std::size_t poly_degree_bound,
              const std::size_t num_interactive_repetitions = 0, /* auto filled for 129 bits of security if 0 */
              const std::size_t num_query_repetitions = 0)
{
    const polynomial<FieldT> poly = polynomial<FieldT>::random_polynomial(poly_degree_bound);

    /* Set up the protocol blueprint */
    iop_protocol<FieldT> IOP;

    const field_subset<FieldT> codeword_domain =
        make_domain_helper<FieldT>(codeword_domain_dim, FieldT::zero());
    const domain_handle codeword_domain_handle = IOP.register_domain(codeword_domain);
    const oracle_handle poly_handle = IOP.register_oracle(codeword_domain_handle, poly_degree_bound, false);
    const std::vector<oracle_handle_ptr> poly_handles = {std::make_shared<oracle_handle>(poly_handle) };
    const size_t security_bits = 129;
    const size_t tested_poly_degree_bound = 1ull << (codeword_domain_dim - RS_extra_dimensions);
    const size_t absolute_proximity_parameter = tested_poly_degree_bound / 2; /* arbitrary */
    const FRI_soundness_type soundness_type = FRI_soundness_type::heuristic;
    FRI_protocol_parameters<FieldT> params(security_bits, security_bits, soundness_type,
                                           tested_poly_degree_bound, codeword_domain_dim, RS_extra_dimensions,
                                           absolute_proximity_parameter,
                                           localization_parameter_array);
    if (num_interactive_repetitions != 0 || num_query_repetitions != 0)
    {
        params.override_security_parameters(num_interactive_repetitions, num_query_repetitions);
    }

    FRI_protocol<FieldT> FRI(IOP,
                             params,
                             codeword_domain_handle,
                             poly_handles);

    FRI.register_interactions();
    IOP.seal_interaction_registrations();

    FRI.register_queries();
    IOP.seal_query_registrations();

    /* Prover */
    oracle<FieldT> poly_oracle(FFT_over_field_subset<FieldT>(poly.coefficients(), codeword_domain));
    IOP.submit_oracle(poly_handle, std::move(poly_oracle));
    IOP.signal_prover_round_done();
    FRI.calculate_and_submit_proof();

    /* Verifier */
    return FRI.verifier_predicate();
}

TEST(FRITrueTest, SimpleTest) {
    typedef gf64 FieldT;

    /* Common parameters */
    const std::size_t codeword_domain_dim = 10;
    const std::size_t RS_extra_dimensions = 2; /* \rho = 2^{-RS_extra_dimensions} */

    std::vector<std::size_t> localization_parameter_array({4, 2});
    localization_parameter_array.insert(localization_parameter_array.begin(), 1);

    const std::size_t poly_degree_bound = 1ull << (codeword_domain_dim - RS_extra_dimensions);

    bool result = run_test<FieldT>(codeword_domain_dim, localization_parameter_array, RS_extra_dimensions, poly_degree_bound);
    EXPECT_TRUE(result);
}

TEST(FRIFalseTest, SimpleTest) {
    typedef gf64 FieldT;

    /* Common parameters */
    const std::size_t codeword_domain_dim = 12;
    const std::size_t RS_extra_dimensions = 3; /* \rho = 2^{-RS_extra_dimensions} */
    
    std::vector<std::size_t> localization_parameter_array(4, 2);
    localization_parameter_array.insert(localization_parameter_array.begin(), 1);

    const std::size_t poly_degree_bound = (1ull << (codeword_domain_dim - RS_extra_dimensions)) + 1;

    bool result = run_test<FieldT>(codeword_domain_dim, localization_parameter_array, RS_extra_dimensions, poly_degree_bound);
    EXPECT_FALSE(result);
}

TEST(FRIMultiplicativeTrueTest, SimpleTest) {
    alt_bn128_pp::init_public_params();

    typedef alt_bn128_Fr FieldT;

    /* Common parameters */
    const std::size_t codeword_domain_dim = 12;
    const std::size_t RS_extra_dimensions = 3; /* \rho = 2^{-RS_extra_dimensions} */
    
    std::vector<std::size_t> localization_parameter_array(4, 2);
    localization_parameter_array.insert(localization_parameter_array.begin(), 1);

    const std::size_t poly_degree_bound = 1ull << (codeword_domain_dim - RS_extra_dimensions);

    bool result = run_test<FieldT>(codeword_domain_dim, localization_parameter_array, RS_extra_dimensions, poly_degree_bound);
    EXPECT_TRUE(result);
}

TEST(FRIMultiplicativeFalseTest, SimpleTest) {
    alt_bn128_pp::init_public_params();

    typedef alt_bn128_Fr FieldT;

    /* Common parameters */
    const std::size_t codeword_domain_dim = 12;
    const std::size_t RS_extra_dimensions = 3; /* \rho = 2^{-RS_extra_dimensions} */
    
    std::vector<std::size_t> localization_parameter_array(4, 2);
    localization_parameter_array.insert(localization_parameter_array.begin(), 1);

    const std::size_t poly_degree_bound = (1ull << (codeword_domain_dim - RS_extra_dimensions)) + 1;

    bool result = run_test<FieldT>(codeword_domain_dim, localization_parameter_array, RS_extra_dimensions, poly_degree_bound);
    EXPECT_FALSE(result);
}

TEST(FRITrueEarlyStopTest, SimpleTest) {
    typedef gf64 FieldT;

    /* Common parameters */
    const std::size_t codeword_domain_dim = 12;
    const std::size_t RS_extra_dimensions = 3; /* \rho = 2^{-RS_extra_dimensions} */
    
    std::vector<std::size_t> localization_parameter_array(1, 2);
    localization_parameter_array.insert(localization_parameter_array.begin(), 1);
    
    const std::size_t poly_degree_bound = 1ull << (codeword_domain_dim - RS_extra_dimensions);

    bool result = run_test<FieldT>(codeword_domain_dim, localization_parameter_array, RS_extra_dimensions, poly_degree_bound);
    EXPECT_TRUE(result);
}

TEST(FRIFalseEarlyStopTest, SimpleTest) {
    typedef gf64 FieldT;

    /* Common parameters */
    const std::size_t codeword_domain_dim = 12;
    const std::size_t RS_extra_dimensions = 3; /* \rho = 2^{-RS_extra_dimensions} */
    
    std::vector<std::size_t> localization_parameter_array(1, 2);
    localization_parameter_array.insert(localization_parameter_array.begin(), 1);

    const std::size_t poly_degree_bound = (1ull << (codeword_domain_dim - RS_extra_dimensions)) + 1;

    bool result = run_test<FieldT>(codeword_domain_dim, localization_parameter_array, RS_extra_dimensions, poly_degree_bound);
    EXPECT_FALSE(result);
}

TEST(FRIMultiplicativeTrueEarlyStopTest, SimpleTest) {
    alt_bn128_pp::init_public_params();

    typedef alt_bn128_Fr FieldT;

    /* Common parameters */
    const std::size_t codeword_domain_dim = 12;
    const std::size_t RS_extra_dimensions = 3; /* \rho = 2^{-RS_extra_dimensions} */
    
    std::vector<std::size_t> localization_parameter_array(1, 2);
    localization_parameter_array.insert(localization_parameter_array.begin(), 1);

    const std::size_t poly_degree_bound = 1ull << (codeword_domain_dim - RS_extra_dimensions);

    bool result = run_test<FieldT>(codeword_domain_dim, localization_parameter_array, RS_extra_dimensions, poly_degree_bound);
    EXPECT_TRUE(result);
}

TEST(FRIMultiplicativeFalseEarlyStopTest, SimpleTest) {
    alt_bn128_pp::init_public_params();

    typedef alt_bn128_Fr FieldT;

    /* Common parameters */
    const std::size_t codeword_domain_dim = 12;
    const std::size_t RS_extra_dimensions = 3; /* \rho = 2^{-RS_extra_dimensions} */

    std::vector<std::size_t> localization_parameter_array(1, 2);
    localization_parameter_array.insert(localization_parameter_array.begin(), 1);

    const std::size_t poly_degree_bound = (1ull << (codeword_domain_dim - RS_extra_dimensions)) + 1;

    bool result = run_test<FieldT>(codeword_domain_dim, localization_parameter_array, RS_extra_dimensions, poly_degree_bound);
    EXPECT_FALSE(result);
}

TEST(FRITrueRandomTest, SimpleTest) {
    typedef gf64 FieldT;

    /* Common parameters */
    const std::size_t codeword_domain_dim = 12;
    const std::size_t RS_extra_dimensions = 3; /* \rho = 2^{-RS_extra_dimensions} */

    std::size_t remaining = codeword_domain_dim - RS_extra_dimensions - 1;
    std::vector<std::size_t> localization_parameter_array = random_vector_that_sums_to(remaining);
    localization_parameter_array.insert(localization_parameter_array.begin(), 1);

    const std::size_t poly_degree_bound = 1ull << (codeword_domain_dim - RS_extra_dimensions);

    bool result = run_test<FieldT>(codeword_domain_dim, localization_parameter_array, RS_extra_dimensions, poly_degree_bound);
    EXPECT_TRUE(result);
}

TEST(FRIFalseRandomTest, SimpleTest) {
    typedef gf64 FieldT;

    /* Common parameters */
    const std::size_t codeword_domain_dim = 12;
    const std::size_t RS_extra_dimensions = 3; /* \rho = 2^{-RS_extra_dimensions} */
    
    std::size_t remaining = codeword_domain_dim - RS_extra_dimensions - 1;
    std::vector<std::size_t> localization_parameter_array = random_vector_that_sums_to(remaining);
    localization_parameter_array.insert(localization_parameter_array.begin(), 1);

    const std::size_t poly_degree_bound = (1ull << (codeword_domain_dim - RS_extra_dimensions)) + 1;

    bool result = run_test<FieldT>(codeword_domain_dim, localization_parameter_array, RS_extra_dimensions, poly_degree_bound);
    EXPECT_FALSE(result);
}

TEST(FRIMultiplicativeTrueRandomTest, SimpleTest) {
    alt_bn128_pp::init_public_params();

    typedef alt_bn128_Fr FieldT;

    /* Common parameters */
    const std::size_t codeword_domain_dim = 12;
    const std::size_t RS_extra_dimensions = 2; /* \rho = 2^{-RS_extra_dimensions} */
    
    std::size_t remaining = codeword_domain_dim - RS_extra_dimensions - 1;
    std::vector<std::size_t> localization_parameter_array = random_vector_that_sums_to(remaining);
    localization_parameter_array.insert(localization_parameter_array.begin(), 1);

    const std::size_t poly_degree_bound = 1ull << (codeword_domain_dim - RS_extra_dimensions);

    bool result = run_test<FieldT>(codeword_domain_dim, localization_parameter_array, RS_extra_dimensions, poly_degree_bound);
    EXPECT_TRUE(result);
}

TEST(FRIMultiplicativeFalseRandomTest, SimpleTest) {
    alt_bn128_pp::init_public_params();
    typedef alt_bn128_Fr FieldT;

    /* Common parameters */
    const std::size_t codeword_domain_dim = 12;
    const std::size_t RS_extra_dimensions = 2; /* \rho = 2^{-RS_extra_dimensions} */
    
    std::size_t remaining = codeword_domain_dim - RS_extra_dimensions - 1;
    std::vector<std::size_t> localization_parameter_array = random_vector_that_sums_to(remaining);
    localization_parameter_array.insert(localization_parameter_array.begin(), 1);

    const std::size_t poly_degree_bound = (1ull << (codeword_domain_dim - RS_extra_dimensions)) + 1;

    bool result = run_test<FieldT>(codeword_domain_dim, localization_parameter_array, RS_extra_dimensions, poly_degree_bound);
    EXPECT_FALSE(result);
}

}
