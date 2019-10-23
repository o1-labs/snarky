#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <iostream>

#include <gtest/gtest.h>

#include "libiop/algebra/fields/gf64.hpp"
#include <libff/algebra/curves/alt_bn128/alt_bn128_pp.hpp>
#include "libiop/algebra/fft.hpp"
#include "libiop/common/common.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/protocols/ldt/fri/fri_ldt.hpp"
#include "libiop/protocols/ldt/ldt_reducer.hpp"
#include "libiop/protocols/ldt/direct_ldt/direct_ldt.hpp"

namespace libiop {

// TODO: Add tests that use virtual oracles as well.
template<typename FieldT, typename single_LDT_type>
bool run_test(const std::size_t codeword_domain_dim,
              const std::size_t RS_extra_dimensions,
              std::vector<std::size_t> poly_degree_bounds,
              std::vector<polynomial<FieldT>> polys, 
              const bool make_zk,
              bool is_FRI,
              bool is_direct,
              bool should_be_true)
{
    std::vector<std::size_t> localization_parameter_array(3, 2);
    localization_parameter_array.insert(localization_parameter_array.begin(), 1);

    const std::size_t num_reductions = localization_parameter_array.size();

    const size_t ldt_reducer_dummy_soundness = 65; /* We override this with the below */
    const size_t num_ldt_instances = std::rand() % 6 + 1;

    std::cout << "Codeword domain dimension: " << codeword_domain_dim << "\n"
              << "RS_extra_dimensions: " << RS_extra_dimensions << "\n"
              << "poly_degree_bounds: " << poly_degree_bounds << "\n"
              << "num_reductions: " << num_reductions << "\n"
              << "\n";

    std::size_t num_vectors = polys.size();

    /* Set up the protocol blueprint */
    iop_protocol<FieldT> IOP;

    const field_subset<FieldT> codeword_domain(1ull << codeword_domain_dim);
    const size_t tested_degree_bound = codeword_domain.num_elements() >> RS_extra_dimensions;
    const domain_handle codeword_domain_handle = IOP.register_domain(codeword_domain);
    std::vector<oracle_handle> poly_handles;
    for (size_t i = 0; i < num_vectors; ++i)
    {
        poly_handles.emplace_back(IOP.register_oracle(codeword_domain_handle, poly_degree_bounds[i], make_zk));
    }
    std::vector<oracle_handle_ptr> poly_handle_ptrs;
    for (size_t i = 0; i < num_vectors; ++i)
    {
        poly_handle_ptrs.emplace_back(std::make_shared<oracle_handle>(poly_handles[i]));
    }

    /* No virtual oracles, so constraint_degree_bound = tested_degree_bound */
    LDT_instance_reducer_params<FieldT> params(ldt_reducer_dummy_soundness, LDT_reducer_soundness_type::proven,
        codeword_domain_dim, tested_degree_bound, tested_degree_bound, make_zk);
    params.override_security_parameter(num_ldt_instances);

    LDT_instance_reducer<FieldT, single_LDT_type> LDT(IOP,
                                                      codeword_domain_handle,
                                                      params);
    if (is_FRI)
    {
        const size_t FRI_security_bits = 64;
        const FRI_soundness_type soundness_type = FRI_soundness_type::heuristic;
        FRI_protocol_parameters<FieldT> ldt_params(FRI_security_bits, FRI_security_bits, soundness_type,
                                                   tested_degree_bound, codeword_domain_dim, RS_extra_dimensions,
                                                   params.absolute_proximity_parameter(),
                                                   localization_parameter_array);
        std::shared_ptr<multi_LDT_parameter_base<FieldT>> shared_params = 
            std::make_shared<FRI_protocol_parameters<FieldT>>(ldt_params);
        LDT.set_LDT_params(shared_params);
    }
    if (is_direct)
    {        
        const size_t direct_LDT_security_bits = 64;
        direct_LDT_parameters<FieldT> ldt_params(direct_LDT_security_bits, tested_degree_bound, RS_extra_dimensions,
            params.absolute_proximity_parameter());
        std::shared_ptr<multi_LDT_parameter_base<FieldT>> shared_params = 
            std::make_shared<direct_LDT_parameters<FieldT>>(ldt_params);
        LDT.set_LDT_params(shared_params);
    }

    LDT.register_interactions(poly_handle_ptrs);
    IOP.seal_interaction_registrations();

    LDT.register_queries();
    IOP.seal_query_registrations();

    /* Prover */
    std::vector<oracle<FieldT>> poly_oracles;
    for (size_t i = 0; i < num_vectors; ++i)
    {
        IOP.submit_oracle(poly_handles[i], FFT_over_field_subset<FieldT>(polys[i].coefficients(), codeword_domain));
    }
    LDT.submit_masking_polynomial();
    IOP.signal_prover_round_done();
    LDT.calculate_and_submit_proof();

    /* Verifier */
    return LDT.verifier_predicate();
}

template<typename FieldT>
std::vector<polynomial<FieldT>> generate_n_polys_of_degree_d(const std::size_t n, const std::size_t d) {
    std::vector<polynomial<FieldT>> polys;
    for (size_t i = 0; i < n; ++i)
    {
        polys.emplace_back(polynomial<FieldT>::random_polynomial(d));
    }
    return polys;
}

/** Runs the following tests: fix a codeword domain dim, rs inverse log rate.
 *  * Ensure that 3 polynomials of the same poly degree bound pass multi_ldt tests
 *  * Ensure that 4 polynomials, with 3 of the same poly degree bound, and the 4th of smaller degree, passes.
 */
template<typename FieldT, typename single_LDT_type>
void run_passing_tests(const bool make_zk, bool is_FRI, bool is_direct)
{
    const std::size_t codeword_domain_dim = 12;
    const std::size_t RS_extra_dimensions = 3;
    const std::size_t poly_degree_bound = 1ull << (codeword_domain_dim - RS_extra_dimensions);

    std::size_t num_vectors = 3;
    std::vector<std::size_t> poly_degree_bounds({poly_degree_bound, poly_degree_bound, poly_degree_bound});

    std::vector<polynomial<FieldT>> polys = generate_n_polys_of_degree_d<FieldT>(num_vectors, poly_degree_bound);
    EXPECT_TRUE((run_test<FieldT, single_LDT_type>(codeword_domain_dim, RS_extra_dimensions, poly_degree_bounds,
        polys, make_zk, is_FRI, is_direct, true))) <<
        "multi_ldt did not pass with " << num_vectors << " vectors of the same (valid) poly_degree_bound";
    polys.emplace_back(polynomial<FieldT>::random_polynomial(poly_degree_bound / 2));
    poly_degree_bounds.emplace_back((poly_degree_bound / 2));
    EXPECT_TRUE((run_test<FieldT, single_LDT_type>(codeword_domain_dim, RS_extra_dimensions, poly_degree_bounds,
        polys, make_zk, is_FRI, is_direct, true))) <<
        "multi_ldt did not pass when ran with " << num_vectors << " of the same (valid) poly_degree_bound," 
        << " and the " << (num_vectors + 1) << "th is of degree poly_degree_bound / 2";
}

template<typename FieldT, typename single_LDT_type>
void run_failing_tests(const bool make_zk, bool is_FRI, bool is_direct)
{
    const std::size_t codeword_domain_dim = 12;
    const std::size_t RS_extra_dimensions = 3;
    std::size_t max_valid_degree_bound = 1ull << (codeword_domain_dim - RS_extra_dimensions);
    std::vector<polynomial<FieldT>> polys;
    std::vector<std::size_t> poly_degree_bounds;
    std::size_t num_vectors = 3; // TODO: Iterate through different numbers here?

    // test case 1:
    // * All polynomials claim to have degree max_valid_degree_bound
    // * All polynomials actually have degree max_valid_degree_bound + 1.
    polys = generate_n_polys_of_degree_d<FieldT>(num_vectors, max_valid_degree_bound + 1);
    poly_degree_bounds = {max_valid_degree_bound, max_valid_degree_bound, max_valid_degree_bound};
    EXPECT_FALSE((run_test<FieldT, single_LDT_type>(codeword_domain_dim, RS_extra_dimensions, poly_degree_bounds,
        polys, make_zk, is_FRI, is_direct, false))) <<
        "multi_ldt passed with 3 polynomials claiming to have the same degree (which is a power of 2). " << 
        "All had degree (claimed degree + 1).";

    // test case 2:
    // * All polynomials claim to have degree max_valid_degree_bound
    // * All but one polynomial has degree max_valid_degree_bound
    // * Remaining polynomial has degree max_valid_degree_bound + 1
    polys = generate_n_polys_of_degree_d<FieldT>(num_vectors, max_valid_degree_bound);
    std::size_t rand_index = std::rand() % num_vectors;
    polys[rand_index] = polynomial<FieldT>::random_polynomial(max_valid_degree_bound + 1);
    EXPECT_FALSE((run_test<FieldT, single_LDT_type>(codeword_domain_dim, RS_extra_dimensions, poly_degree_bounds,
        polys, make_zk, is_FRI, is_direct, false))) <<
        "multi_ldt passed with 3 polynomials claiming to have the same degree (which is a power of 2). " << 
        "One had degree (claimed degree + 1).";
    
    // test case 3:
    // * All but one polynomial claims to have degree max_valid_degree_bound
    // * All but that same polynomial has degree max_valid_degree_bound
    // * Remaining polynomial claims to have degree max_valid_degree_bound / 2
    // * Remaining polynomial has degree max_valid_degree_bound + 1
    polys = generate_n_polys_of_degree_d<FieldT>(num_vectors, max_valid_degree_bound);
    rand_index = std::rand() % num_vectors;
    polys[rand_index] = polynomial<FieldT>::random_polynomial(1 + (max_valid_degree_bound / 2));
    poly_degree_bounds[rand_index] = max_valid_degree_bound / 2;
    EXPECT_FALSE((run_test<FieldT, single_LDT_type>(codeword_domain_dim, RS_extra_dimensions, poly_degree_bounds,
        polys, make_zk, is_FRI, is_direct, false))) <<
        "multi_ldt passed when one polynomial had degree greater than what it claimed.";
    
    // test case 4:
    // Same as the last test case, but that different polynomial now has as a random degree in
    // in (1, max_valid_degree_bound - 1), instead of being a power of 2. (And actual degree one greater)
    std::size_t rand_degree = 1 + (std::rand() % (max_valid_degree_bound - 2));
    polys[rand_index] = polynomial<FieldT>::random_polynomial(1 + rand_degree);
    poly_degree_bounds[rand_index] = rand_degree;
    EXPECT_FALSE((run_test<FieldT, single_LDT_type>(codeword_domain_dim, RS_extra_dimensions, poly_degree_bounds,
        polys, make_zk, is_FRI, is_direct, false))) <<
        "multi_ldt passed when one polynomial had degree greater than what it claimed.";
    
    // TODO: repeat test cases 5,6, but with more invalid polynomials of smaller degrees.
}

TEST(MultiLDTFRITrueZKTest, MultiplicativeTest) {
    alt_bn128_pp::init_public_params();
    typedef alt_bn128_Fr FieldT;
    run_passing_tests<FieldT, FRI_protocol<FieldT>>(true, true, false);
}

TEST(MultiLDTFRITrueNonZKTest, MultiplicativeTest) {
    alt_bn128_pp::init_public_params();
    typedef alt_bn128_Fr FieldT;
    run_passing_tests<FieldT, FRI_protocol<FieldT>>(false, true, false);
}

TEST(MultiLDTFRIFalseZKTest, MultiplicativeTest) {
    alt_bn128_pp::init_public_params();
    typedef alt_bn128_Fr FieldT;
    run_failing_tests<FieldT, FRI_protocol<FieldT>>(true, true, false);
}

TEST(MultiLDTFRIFalseNonZKTest, MultiplicativeTest) {
    alt_bn128_pp::init_public_params();
    typedef alt_bn128_Fr FieldT;
    run_failing_tests<FieldT, FRI_protocol<FieldT>>(false, true, false);
}

TEST(MultiLDTDirectTrueZKTest, MultiplicativeTest) {
    alt_bn128_pp::init_public_params();
    typedef alt_bn128_Fr FieldT;
    run_passing_tests<FieldT, direct_LDT_protocol<FieldT>>(true, false, true);
}

TEST(MultiLDTDirectTrueNonZKTest, MultiplicativeTest) {
    alt_bn128_pp::init_public_params();
    typedef alt_bn128_Fr FieldT;
    run_passing_tests<FieldT, direct_LDT_protocol<FieldT>>(false, false, true);
}

TEST(MultiLDTDirectFalseZKTest, MultiplicativeTest) {
    alt_bn128_pp::init_public_params();
    typedef alt_bn128_Fr FieldT;
    run_failing_tests<FieldT, direct_LDT_protocol<FieldT>>(true, false, true);
}

TEST(MultiLDTDirectFalseNonZKTest, MultiplicativeTest) {
    alt_bn128_pp::init_public_params();
    typedef alt_bn128_Fr FieldT;
    run_failing_tests<FieldT, direct_LDT_protocol<FieldT>>(false, false, true);
}

TEST(MultiLDTFRITrueZKTest, AdditiveTest) {
    alt_bn128_pp::init_public_params();
    typedef alt_bn128_Fr FieldT;
    run_passing_tests<FieldT, FRI_protocol<FieldT>>(true, true, false);
}

TEST(MultiLDTFRITrueNonZKTest, AdditiveTest) {
    typedef gf64 FieldT;
    run_passing_tests<FieldT, FRI_protocol<FieldT>>(false, true, false);
}

TEST(MultiLDTFRIFalseZKTest, AdditiveTest) {
    typedef gf64 FieldT;
    run_failing_tests<FieldT, FRI_protocol<FieldT>>(true, true, false);
}

TEST(MultiLDTFRIFalseNonZKTest, AdditiveTest) {
    typedef gf64 FieldT;
    run_failing_tests<FieldT, FRI_protocol<FieldT>>(false, true, false);
}

TEST(MultiLDTDirectTrueZKTest, AdditiveTest) {
    typedef gf64 FieldT;
    run_passing_tests<FieldT, direct_LDT_protocol<FieldT>>(true, false, true);
}

TEST(MultiLDTDirectTrueNonZKTest, AdditiveTest) {
    typedef gf64 FieldT;
    run_passing_tests<FieldT, direct_LDT_protocol<FieldT>>(false, false, true);
}

TEST(MultiLDTDirectFalseZKTest, AdditiveTest) {
    typedef gf64 FieldT;
    run_failing_tests<FieldT, direct_LDT_protocol<FieldT>>(true, false, true);
}

TEST(MultiLDTDirectFalseNonZKTest, AdditiveTest) {
    typedef gf64 FieldT;
    run_failing_tests<FieldT, direct_LDT_protocol<FieldT>>(false, false, true);
}

}