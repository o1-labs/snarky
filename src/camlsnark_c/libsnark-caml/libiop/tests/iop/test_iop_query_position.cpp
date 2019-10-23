#include <cstdint>
#include <stdexcept>
#include <gtest/gtest.h>
#include <vector>

#include "libiop/algebra/fields/gf64.hpp"
#include <libff/algebra/curves/edwards/edwards_pp.hpp>
#include <libff/algebra/curves/alt_bn128/alt_bn128_pp.hpp>
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/iop/utilities/query_positions.hpp"

namespace libiop {

template<typename FieldT>
void run_query_to_queries_for_entire_coset_test(
    const field_subset<FieldT> codeword_domain,
    const size_t initial_query_pos,
    const size_t coset_size,
    const std::vector<size_t> expected_query_positions)
{
     /* Initialize IOP */
    iop_protocol<FieldT> IOP;
    const domain_handle codeword_domain_handle = IOP.register_domain(codeword_domain);
    IOP.register_oracle(codeword_domain_handle, 0, false);
    IOP.seal_interaction_registrations();

    /* TODO: move this to utilities */
    query_position_handle base_handle = 
        IOP.register_deterministic_query_position(
            { },
            [initial_query_pos]
            (const std::vector<std::size_t> &seed_positions)
            -> std::size_t {
                return initial_query_pos;
            });
    std::vector<query_position_handle> query_positions = 
        query_position_to_queries_for_entire_coset(IOP, base_handle, codeword_domain, coset_size);

    ASSERT_EQ(expected_query_positions.size(), query_positions.size());
    for (size_t i = 0; i < expected_query_positions.size(); i++)
    {
        size_t actual_query_pos = IOP.obtain_query_position(query_positions[i]);
        ASSERT_EQ(actual_query_pos, expected_query_positions[i]);
    }
}

TEST(QueryEntireCosetTest, AdditiveTest) {
    typedef gf64 FieldT;
    size_t codeword_domain_dim = 10;
    FieldT codeword_domain_offset = FieldT::zero();
    size_t coset_size = 4;
    field_subset<FieldT> codeword_domain(1ull << codeword_domain_dim, codeword_domain_offset);
    size_t initial_query_pos = 0;
    std::vector<size_t> expected_query_pos({0,1,2,3});
    run_query_to_queries_for_entire_coset_test<FieldT>(
        codeword_domain, initial_query_pos, coset_size, expected_query_pos);
    
    /* 2nd element, in the 5th coset */
    initial_query_pos = 5;
    expected_query_pos = std::vector<size_t> ({4,5,6,7});
    run_query_to_queries_for_entire_coset_test<FieldT>(
        codeword_domain, initial_query_pos, coset_size, expected_query_pos);
}

TEST(QueryEntireCosetTest, MultiplicativeTest) {
    alt_bn128_pp::init_public_params();
    typedef alt_bn128_Fr FieldT;

    size_t codeword_domain_dim = 10;
    FieldT codeword_domain_shift = FieldT::one();
    size_t coset_size = 4;
    field_subset<FieldT> codeword_domain(1ull << codeword_domain_dim, codeword_domain_shift);
    size_t initial_query_pos = 0;
    size_t offset = 1ull << (codeword_domain_dim - 2);
    std::vector<size_t> expected_query_pos({0,offset,2*offset,3*offset});
    run_query_to_queries_for_entire_coset_test<FieldT>(
        codeword_domain, initial_query_pos, coset_size, expected_query_pos);
    
    /* 2nd element, in the 5th coset */
    initial_query_pos = (1ull << (codeword_domain_dim - 1)) + 5;
    expected_query_pos = std::vector<size_t> ({5,5 + offset, 5 + offset * 2, 5 + offset * 3});
    run_query_to_queries_for_entire_coset_test<FieldT>(
        codeword_domain, initial_query_pos, coset_size, expected_query_pos);
}

}