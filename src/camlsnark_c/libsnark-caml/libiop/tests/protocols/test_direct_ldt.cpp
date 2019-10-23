#include <algorithm>
#include <cstdint>
#include <iostream>

#include <gtest/gtest.h>

#include <libff/algebra/curves/edwards/edwards_pp.hpp>

#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/field_subset/subgroup.hpp"
#include "libiop/common/common.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/iop/utilities/batching.hpp"
#include "libiop/protocols/ldt/direct_ldt/direct_ldt.hpp"

namespace libiop {

template<typename FieldT>
bool run_test(const std::vector<polynomial<FieldT>> polys,
              const field_subset<FieldT> codeword_domain,
              const std::size_t RS_extra_dimensions,
              const std::size_t generated_polys_degree_bound,
              const std::size_t num_queries = 0)
{
    /* Set up the protocol blueprint */
    iop_protocol<FieldT> IOP;
    const domain_handle codeword_domain_handle = IOP.register_domain(codeword_domain);
    const std::vector<oracle_handle_ptr> poly_handles =
        register_n_oracles(IOP, polys.size(), codeword_domain_handle, generated_polys_degree_bound, false);
    const size_t query_soundness_bits = 64;
    const size_t tested_poly_degree_bound = 1ull << (codeword_domain.dimension() - RS_extra_dimensions);
    /* fractional_proximity = (1 - p) / 3 */
    const size_t tested_proximity = (codeword_domain.num_elements() - tested_poly_degree_bound) / 3;
    direct_LDT_parameters<FieldT> params(query_soundness_bits, tested_poly_degree_bound, 
        RS_extra_dimensions, tested_proximity);
    if (num_queries != 0)
    {
        params.override_security_parameter(num_queries);
    }
    params.print();

    direct_LDT_protocol<FieldT> LDT(IOP,
                                    params,
                                    codeword_domain_handle,
                                    poly_handles);

    LDT.register_interactions();
    IOP.seal_interaction_registrations();

    LDT.register_queries();
    IOP.seal_query_registrations();

    /* Prover */
    for (size_t i = 0; i < polys.size(); i++)
    {
        oracle<FieldT> poly_oracle(FFT_over_field_subset<FieldT>(polys[i].coefficients(), codeword_domain));
        IOP.submit_oracle(poly_handles[i], std::move(poly_oracle));
    }
    IOP.signal_prover_round_done();
    LDT.calculate_and_submit_proof();

    /* Verifier */
    return LDT.verifier_predicate();
}

TEST(DirectLDTTrueTest, SimpleTest) {
    typedef gf64 FieldT;
    
    const std::size_t codeword_domain_dim = 12;
    const std::size_t RS_extra_dimensions = 3; /* \rho = 2^{-RS_extra_dimensions} */
    const std::size_t poly_degree_bound = 1ull << (codeword_domain_dim - RS_extra_dimensions);

    const polynomial<FieldT> poly1 = polynomial<FieldT>::random_polynomial(poly_degree_bound);
    const polynomial<FieldT> poly2 = polynomial<FieldT>::random_polynomial(poly_degree_bound - 10);
    const affine_subspace<FieldT> codeword_domain =
        linear_subspace<FieldT>::standard_basis(codeword_domain_dim);

    const bool result = run_test<FieldT>({poly1, poly2}, codeword_domain, RS_extra_dimensions, poly_degree_bound);
    
    EXPECT_TRUE(result);
}

TEST(DirectLDTFalseTest, SimpleTest) {
    typedef gf64 FieldT;

    // one of two polynomials is of too high of a degree.
    const std::size_t codeword_domain_dim = 12;
    const std::size_t RS_extra_dimensions = 3; /* \rho = 2^{-RS_extra_dimensions} */
    const std::size_t poly_degree_bound1 = (1ull << codeword_domain_dim) - 1; // Too big (ignores rate)
    const std::size_t poly_degree_bound2 = (1ull << codeword_domain_dim) - 3;

    const polynomial<FieldT> poly1 = polynomial<FieldT>::random_polynomial(poly_degree_bound1);
    const polynomial<FieldT> poly2 = polynomial<FieldT>::random_polynomial(poly_degree_bound2);
    const affine_subspace<FieldT> codeword_domain =
        linear_subspace<FieldT>::standard_basis(codeword_domain_dim);
    const std::size_t num_queries = 10;

    /* Ensure we catch if only the first oracle is bad, only the second, and if both oracles are bad. */
    bool result = run_test<FieldT>({poly1, poly2}, codeword_domain, RS_extra_dimensions, poly_degree_bound2, num_queries);
    EXPECT_FALSE(result);
    result = run_test<FieldT>({poly2, poly1}, codeword_domain, RS_extra_dimensions, poly_degree_bound2, num_queries);
    EXPECT_FALSE(result);
    result = run_test<FieldT>({poly1, poly1}, codeword_domain, RS_extra_dimensions, poly_degree_bound2, num_queries);
    EXPECT_FALSE(result);
}

TEST(DirectLDTMultSubgroupTrueTest, SimpleTest) {
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;
    
    const std::size_t codeword_domain_dim = 12;
    
    const std::size_t RS_extra_dimensions = 3; /* \rho = 2^{-RS_extra_dimensions} */
    const std::size_t poly_degree_bound = 1ull << (codeword_domain_dim - RS_extra_dimensions);
    const polynomial<FieldT> poly1 = polynomial<FieldT>::random_polynomial(poly_degree_bound);
    const polynomial<FieldT> poly2 = polynomial<FieldT>::random_polynomial(poly_degree_bound - 10);
    const std::vector<polynomial<FieldT>> polys = {poly1, poly2};
    const field_subset<FieldT> codeword_domain(1ull << codeword_domain_dim);

    const bool result = run_test<FieldT>(polys, codeword_domain, RS_extra_dimensions, poly_degree_bound);
    
    EXPECT_TRUE(result);
}
    
TEST(DirectLDTMultSubgroupFalseTest, SimpleTest) {
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;

    // one of two polynomials is of too high of a degree.
    const std::size_t codeword_domain_dim = 12;
    const std::size_t RS_extra_dimensions = 3; /* \rho = 2^{-RS_extra_dimensions} */
    const std::size_t poly_degree_bound1 = (1ull << codeword_domain_dim) - 1; // Too big (ignores rate)
    const std::size_t poly_degree_bound2 = (1ull << codeword_domain_dim) - 3;

    const polynomial<FieldT> poly1 = polynomial<FieldT>::random_polynomial(poly_degree_bound1);
    const polynomial<FieldT> poly2 = polynomial<FieldT>::random_polynomial(poly_degree_bound2);
    std::vector<polynomial<FieldT>> polys = {poly1, poly2};
    const field_subset<FieldT> codeword_domain(1ull << codeword_domain_dim);
    const std::size_t num_queries = 10;

    /* Ensure we catch if only the first oracle is bad, only the second, and if both oracles are bad. */
    bool result = run_test<FieldT>(polys, codeword_domain, RS_extra_dimensions, poly_degree_bound2, num_queries);
    EXPECT_FALSE(result);
    polys = {poly2, poly1};
    result = run_test<FieldT>(polys, codeword_domain, RS_extra_dimensions, poly_degree_bound2, num_queries);
    EXPECT_FALSE(result);
    polys = {poly1, poly1};
    result = run_test<FieldT>(polys, codeword_domain, RS_extra_dimensions, poly_degree_bound2, num_queries);
    EXPECT_FALSE(result);
}

}
