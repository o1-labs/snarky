#include <cstdint>

#include <gtest/gtest.h>

#include "libiop/algebra/fields/gf64.hpp"
#include <libff/algebra/curves/alt_bn128/alt_bn128_pp.hpp>
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/tests/snark/dummy_bcs_protocol.hpp"
#include "libiop/snark/common/bcs_prover.hpp"
#include "libiop/snark/common/bcs_indexer.hpp"
#include "libiop/snark/common/bcs_verifier.hpp"

namespace libiop {

const size_t security_parameter = 40;
const size_t hash_size = security_parameter * 2 / 8;

template<typename FieldT>
bcs_transformation_parameters<FieldT> get_bcs_parameters()
{
    bcs_transformation_parameters<FieldT> bcs_parameters;
    bcs_parameters.security_parameter = security_parameter;
    bcs_parameters.field_hasher = blake2b_field_element_hash<FieldT>;
    bcs_parameters.zk_hasher = blake2b_zk_element_hash;
    bcs_parameters.compression_hasher = blake2b_two_to_one_hash;
    bcs_parameters.FieldT_randomness_extractor = blake2b_FieldT_randomness_extractor<FieldT>;
    bcs_parameters.integer_randomness_extractor = blake2b_integer_randomness_extractor;
    return bcs_parameters;
}

/** BCS transform allows multiple oracles of different domains.
 *  TODO: Make this support oracles from multiple domains in a single round
 *  TODO: Add soundness tests
 */
template<typename FieldT>
void run_test(const field_subset<FieldT> codeword_domain,
              const size_t num_oracles_per_round,
              const size_t num_rounds,
              const std::vector<round_parameters<FieldT>> round_params,
              const std::vector<std::vector<size_t>> query_positions,
              const bool make_zk,
              const bool preprocessing,
              const size_t expected_size = 0) {
    bcs_transformation_parameters<FieldT> bcs_parameters = get_bcs_parameters<FieldT>();
    /* Decide on query positions according to round_params */
    /* Run indexer */
    bcs_indexer<FieldT> indexer_IOP(bcs_parameters);
    if (preprocessing)
    {
        domain_handle codeword_domain_handle = indexer_IOP.register_domain(codeword_domain);
        dummy_protocol<FieldT> proto(indexer_IOP,
            num_oracles_per_round, num_rounds, round_params,
            codeword_domain_handle, make_zk, preprocessing);
        indexer_IOP.seal_interaction_registrations();
        indexer_IOP.seal_query_registrations();
        proto.calculate_index();
    }

    /* Run prover */
    bcs_prover<FieldT> prover_IOP(bcs_parameters);
    if (preprocessing)
    {
        bcs_prover_index<FieldT> prover_index = indexer_IOP.get_bcs_prover_index();
        prover_IOP = bcs_prover<FieldT>(
            bcs_parameters, prover_index);
    }

    domain_handle codeword_domain_handle = prover_IOP.register_domain(codeword_domain);
    dummy_protocol<FieldT> proto(prover_IOP,
        num_oracles_per_round, num_rounds, round_params,
        codeword_domain_handle, make_zk, preprocessing);
    prover_IOP.seal_interaction_registrations();
    prover_IOP.seal_query_registrations();
    if (preprocessing)
    {
        bcs_prover_index<FieldT> prover_index = indexer_IOP.get_bcs_prover_index();
        prover_IOP.submit_prover_index(prover_index.iop_index_);
    }
    proto.calculate_and_submit_response();

    for (size_t r = 0; r < num_rounds; r++)
    {
        for (size_t i = 0; i < query_positions[r].size(); i++)
        {
            std::vector<oracle_handle_ptr> handles = proto.get_oracle_handles_for_round(r);
            for (oracle_handle_ptr handle : handles)
            {
                bool record = true;
                FieldT eval = prover_IOP.get_oracle_evaluation_at_point(handle, query_positions[r][i], record);
                libiop::UNUSED(eval);
            }
        }
    }

    bcs_transformation_transcript<FieldT> transcript = prover_IOP.get_transcript();
    printf("Entering verifier\n");
    /* Run verifier */
    bcs_verifier<FieldT> verifier_IOP(bcs_parameters, transcript);
    if(preprocessing)
    {
        verifier_IOP = bcs_verifier<FieldT>(
            bcs_parameters, transcript, indexer_IOP.get_verifier_index());
    }

    codeword_domain_handle = verifier_IOP.register_domain(codeword_domain);
    dummy_protocol<FieldT> verifyier_dummy_proto(verifier_IOP,
        num_oracles_per_round, num_rounds, round_params,
        codeword_domain_handle, make_zk, preprocessing);
    verifier_IOP.seal_interaction_registrations();
    EXPECT_TRUE(verifier_IOP.transcript_is_valid());
    for (size_t r = 0; r < num_rounds; r++)
    {
        for (size_t i = 0; i < query_positions[r].size(); i++)
        {
            std::vector<oracle_handle_ptr> handles = verifyier_dummy_proto.get_oracle_handles_for_round(r);
            size_t oracle_index = 0;
            for (oracle_handle_ptr handle : handles)
            {
                FieldT eval = verifier_IOP.get_oracle_evaluation_at_point(handle, query_positions[r][i], true);
                EXPECT_TRUE(verifyier_dummy_proto.check_eval_at_point(r, oracle_index, query_positions[r][i], eval));
                oracle_index++;
            }
        }
    }

    if (expected_size != 0)
    {
        EXPECT_EQ(transcript.size_in_bytes(), expected_size);
    }
}

template<typename FieldT>
void run_single_round_test(const field_subset<FieldT> codeword_domain,
                           const size_t num_oracles,
                           const round_parameters<FieldT> round_params,
                           const std::vector<size_t> query_positions,
                           const bool make_zk,
                           const bool preprocessing,
                           const size_t expected_size = 0) {
    std::vector<std::vector<size_t>> query_postions_for_all_rounds({query_positions});
    std::vector<round_parameters<FieldT>> round_params_for_all_rounds({round_params});
    size_t num_rounds = 1;
    run_test<FieldT>(codeword_domain,
                     num_oracles, num_rounds,
                     round_params_for_all_rounds, query_postions_for_all_rounds,
                     make_zk, preprocessing, expected_size);
}

TEST(BasicOneRoundTests, BCSTest) {
    /* Theres no special casing within the code between additive and multiplicative at default round params. */
    typedef gf64 FieldT;
    size_t dim = 6;
    field_subset<FieldT> codeword_domain(1ull << dim);
    size_t num_oracles = 1;
    round_parameters<FieldT> round_params = round_parameters<FieldT>();
    std::vector<size_t> query_positions({0, 1, 2, 3});
    bool zk = false;
    bool preprocessing = false;
    size_t expected_proof_size = 5*hash_size + query_positions.size() * num_oracles*sizeof(FieldT);
    run_single_round_test<FieldT>(codeword_domain, num_oracles, round_params, query_positions,
        zk, preprocessing, expected_proof_size);

    zk = true;
    expected_proof_size += query_positions.size() * hash_size;
    run_single_round_test<FieldT>(codeword_domain, num_oracles, round_params, query_positions,
        zk, preprocessing, expected_proof_size);

    num_oracles = 2;
    query_positions = std::vector<size_t>({0});
    zk = false;

    expected_proof_size = 7*hash_size + query_positions.size() * num_oracles*sizeof(FieldT);
    run_single_round_test<FieldT>(codeword_domain, num_oracles, round_params, query_positions,
        zk, preprocessing, expected_proof_size);

    query_positions = std::vector<size_t>({0, 31});
    expected_proof_size = 10*hash_size + query_positions.size() * num_oracles*sizeof(FieldT);
    run_single_round_test<FieldT>(codeword_domain, num_oracles, round_params, query_positions,
        zk, preprocessing, expected_proof_size);

    query_positions = std::vector<size_t>({0, 32});
    expected_proof_size = 11*hash_size + query_positions.size() * num_oracles*sizeof(FieldT);
    run_single_round_test<FieldT>(codeword_domain, num_oracles, round_params, query_positions,
        zk, preprocessing, expected_proof_size);
}

TEST(BasicTwoRoundTests, BCSTest) {
    /* Theres no special casing within the code between additive and multiplicative at default round params. */
    const size_t num_rounds = 2;
    typedef gf64 FieldT;
    size_t dim = 7;
    field_subset<FieldT> codeword_domain(1ull << dim);
    size_t num_oracles_per_round = 3;
    bool zk = false;
    bool preprocessing = false;
    round_parameters<FieldT> round1_params = round_parameters<FieldT>();
    std::vector<size_t> round1_query_positions({0, 1, 2, 3});
    size_t round1_proof_size = 6*hash_size + round1_query_positions.size() * num_oracles_per_round*sizeof(FieldT);
    round_parameters<FieldT> round2_params = round_parameters<FieldT>();
    std::vector<size_t> round2_query_positions({0, 1, 2, 3});
    size_t round2_proof_size = 6*hash_size + round2_query_positions.size() * num_oracles_per_round*sizeof(FieldT);

    std::vector<round_parameters<FieldT>> all_round_params({round1_params, round2_params});
    std::vector<std::vector<size_t>> all_query_positions({round1_query_positions, round2_query_positions});
    size_t expected_proof_size = round1_proof_size + round2_proof_size;
    run_test<FieldT>(codeword_domain,
                     num_oracles_per_round, num_rounds,
                     all_round_params, all_query_positions,
                     zk, preprocessing, expected_proof_size);
    round2_query_positions = std::vector<size_t>({32, 33, 34, 35});
    run_test<FieldT>(codeword_domain,
                     num_oracles_per_round, num_rounds,
                     all_round_params, all_query_positions,
                     zk, preprocessing, expected_proof_size);

    zk = true;
    expected_proof_size += (round1_query_positions.size() + round2_query_positions.size()) * hash_size;
    run_test<FieldT>(codeword_domain,
                     num_oracles_per_round, num_rounds,
                     all_round_params, all_query_positions,
                     zk, preprocessing, expected_proof_size);
}

TEST(HolographicTwoRoundTests, BCSTest) {
    /* Theres no special casing within the code between additive and multiplicative at default round params. */
    const size_t num_rounds = 2;
    typedef gf64 FieldT;
    size_t dim = 7;
    field_subset<FieldT> codeword_domain(1ull << dim);
    size_t num_oracles_per_round = 3;
    bool zk = false;
    bool preprocessing = true;
    round_parameters<FieldT> round1_params = round_parameters<FieldT>();
    std::vector<size_t> round1_query_positions({0, 1, 2, 3});
    size_t round1_proof_size = 5*hash_size + round1_query_positions.size() * num_oracles_per_round*sizeof(FieldT);
    round_parameters<FieldT> round2_params = round_parameters<FieldT>();
    std::vector<size_t> round2_query_positions({0, 1, 2, 3});
    size_t round2_proof_size = 6*hash_size + round2_query_positions.size() * num_oracles_per_round*sizeof(FieldT);

    std::vector<round_parameters<FieldT>> all_round_params({round1_params, round2_params});
    std::vector<std::vector<size_t>> all_query_positions({round1_query_positions, round2_query_positions});
    size_t expected_proof_size = round1_proof_size + round2_proof_size;
    run_test<FieldT>(codeword_domain,
                     num_oracles_per_round, num_rounds,
                     all_round_params, all_query_positions,
                     zk, preprocessing, expected_proof_size);
    round2_query_positions = std::vector<size_t>({32, 33, 34, 35});
    run_test<FieldT>(codeword_domain,
                     num_oracles_per_round, num_rounds,
                     all_round_params, all_query_positions,
                     zk, preprocessing, expected_proof_size);

    zk = true;
    expected_proof_size += (round2_query_positions.size()) * hash_size;
    run_test<FieldT>(codeword_domain,
                     num_oracles_per_round, num_rounds,
                     all_round_params, all_query_positions,
                     zk, preprocessing, expected_proof_size);
}

TEST(AdditiveHolographicTests, BCSTest) {
    typedef gf64 FieldT;
    size_t dim = 6;
    const size_t num_rounds = 2;
    size_t num_oracles = 1;
    std::vector<size_t> query_positions({0, 1, 2, 3});

    field_subset<FieldT> codeword_domain(1ull << dim);
    field_subset<FieldT> quotient_map_domain = codeword_domain.get_subset_of_order(4);
    round_parameters<FieldT> round_params = round_parameters<FieldT>(quotient_map_domain);

    std::vector<round_parameters<FieldT>> all_round_params = {round_params, round_params};
    std::vector<std::vector<size_t>> all_query_positions({query_positions, query_positions});

    bool zk = false;
    bool preprocessing = true;
    size_t expected_round1_proof_size = 4*hash_size + query_positions.size() * num_oracles*sizeof(FieldT);
    size_t expected_round2_proof_size = 5*hash_size + query_positions.size() * num_oracles*sizeof(FieldT);
    size_t expected_proof_size = expected_round1_proof_size + expected_round2_proof_size;
    run_test<FieldT>(codeword_domain, num_oracles, num_rounds, all_round_params,
        all_query_positions, zk, preprocessing, expected_proof_size);

    zk = true;
    expected_proof_size += hash_size;
    run_test<FieldT>(codeword_domain, num_oracles, num_rounds, all_round_params,
        all_query_positions, zk, preprocessing, expected_proof_size);
}

TEST(MultiplicativeHolographicTests, BCSTest) {
    libff::alt_bn128_pp::init_public_params();
    typedef libff::alt_bn128_Fr FieldT;
    size_t dim = 6;
    const size_t num_rounds = 2;
    size_t num_oracles = 1;
    std::vector<size_t> query_positions({0, 16, 32, 48});

    field_subset<FieldT> codeword_domain(1ull << dim);
    field_subset<FieldT> quotient_map_domain = codeword_domain.get_subset_of_order(4);
    round_parameters<FieldT> round_params = round_parameters<FieldT>(quotient_map_domain);

    std::vector<round_parameters<FieldT>> all_round_params = {round_params, round_params};
    std::vector<std::vector<size_t>> all_query_positions({query_positions, query_positions});

    bool zk = false;
    bool preprocessing = true;
    size_t expected_round1_proof_size = 4*hash_size + query_positions.size() * num_oracles*sizeof(FieldT);
    size_t expected_round2_proof_size = 5*hash_size + query_positions.size() * num_oracles*sizeof(FieldT);
    size_t expected_proof_size = expected_round1_proof_size + expected_round2_proof_size;
    run_test<FieldT>(codeword_domain, num_oracles, num_rounds, all_round_params,
        all_query_positions, zk, preprocessing, expected_proof_size);

    zk = true;
    expected_proof_size += hash_size;
    run_test<FieldT>(codeword_domain, num_oracles, num_rounds, all_round_params,
        all_query_positions, zk, preprocessing, expected_proof_size);
}

TEST(AdditiveSingleOracleTest, BCSTest) {
    /* TODO: Add more complex test cases */
    typedef gf64 FieldT;
    size_t dim = 6;
    field_subset<FieldT> codeword_domain(1ull << dim);
    size_t num_oracles = 1;
    field_subset<FieldT> quotient_map_domain = codeword_domain.get_subset_of_order(4);
    round_parameters<FieldT> round_params = round_parameters<FieldT>(quotient_map_domain);
    std::vector<size_t> query_positions({0, 1, 2, 3});
    bool zk = false;
    bool preprocessing = false;
    size_t expected_proof_size = 5*hash_size + query_positions.size() * num_oracles*sizeof(FieldT);
    run_single_round_test<FieldT>(codeword_domain, num_oracles, round_params,
        query_positions, zk, preprocessing, expected_proof_size);

    zk = true;
    expected_proof_size += hash_size;
    run_single_round_test<FieldT>(codeword_domain, num_oracles, round_params,
        query_positions, zk, preprocessing, expected_proof_size);
}

TEST(AdditiveMultiOracleTest, BCSTest) {
    /* TODO: Add more complex test cases */
    typedef gf64 FieldT;
    size_t dim = 6;
    field_subset<FieldT> codeword_domain(1ull << dim);
    size_t num_oracles = 4;
    field_subset<FieldT> quotient_map_domain = codeword_domain.get_subset_of_order(2);
    round_parameters<FieldT> round_params = round_parameters<FieldT>(quotient_map_domain);
    std::vector<size_t> query_positions({0, 2, 1, 3});
    bool zk = false;
    bool preprocessing = false;
    size_t expected_proof_size = 5*hash_size + query_positions.size() * num_oracles*sizeof(FieldT);
    run_single_round_test<FieldT>(codeword_domain, num_oracles, round_params, query_positions,
        zk, preprocessing, expected_proof_size);

    zk = true;
    expected_proof_size += 2*hash_size;
    run_single_round_test<FieldT>(codeword_domain, num_oracles, round_params, query_positions,
        zk, preprocessing, expected_proof_size);
}

TEST(MultiplicativeSingleOracleTest, BCSTest) {
    /* TODO: Add more complex test cases */
    libff::alt_bn128_pp::init_public_params();
    typedef libff::alt_bn128_Fr FieldT;
    size_t dim = 6;
    field_subset<FieldT> codeword_domain(1ull << dim);
    size_t num_oracles = 1;
    field_subset<FieldT> quotient_map_domain = codeword_domain.get_subset_of_order(4);
    round_parameters<FieldT> round_params = round_parameters<FieldT>(quotient_map_domain);
    std::vector<size_t> query_positions({0, 16, 32, 48});
    bool zk = false;
    bool preprocessing = false;
    size_t expected_proof_size = 5*hash_size + query_positions.size() * num_oracles*sizeof(FieldT);
    run_single_round_test<FieldT>(codeword_domain, num_oracles, round_params, query_positions,
        zk, preprocessing, expected_proof_size);

    zk = true;
    expected_proof_size += hash_size;
    run_single_round_test<FieldT>(codeword_domain, num_oracles, round_params, query_positions,
        zk, preprocessing, expected_proof_size);
}

TEST(MultiplicativeMultiOracleTest, BCSTest) {
    /* TODO: Add more complex test cases */
    libff::alt_bn128_pp::init_public_params();
    typedef libff::alt_bn128_Fr FieldT;
    size_t dim = 6;
    field_subset<FieldT> codeword_domain(1ull << dim);
    size_t num_oracles = 4;
    field_subset<FieldT> quotient_map_domain = codeword_domain.get_subset_of_order(4);
    round_parameters<FieldT> round_params = round_parameters<FieldT>(quotient_map_domain);
    std::vector<size_t> query_positions({0, 16, 32, 48, 2, 18, 34, 50});
    bool zk = false;
    bool preprocessing = false;
    size_t expected_proof_size = 5*hash_size + query_positions.size() * num_oracles*sizeof(FieldT);
    run_single_round_test<FieldT>(codeword_domain, num_oracles, round_params, query_positions,
        zk, preprocessing, expected_proof_size);

    zk = true;
    expected_proof_size += 2*hash_size;
    run_single_round_test<FieldT>(codeword_domain, num_oracles, round_params, query_positions,
        zk, preprocessing, expected_proof_size);
}

TEST(DifferentRoundParameters, BCSTest) {
    const size_t num_rounds = 2;
    typedef gf64 FieldT;
    size_t dim = 7;
    field_subset<FieldT> codeword_domain(1ull << dim);
    size_t num_oracles_per_round = 3;
    bool zk = false;
    bool preprocessing = false;
    round_parameters<FieldT> round1_params = round_parameters<FieldT>(codeword_domain.get_subset_of_order(2));
    std::vector<size_t> round1_query_positions({0, 1, 2, 3});
    size_t round1_proof_size = 6*hash_size + round1_query_positions.size() * num_oracles_per_round*sizeof(FieldT);
    round_parameters<FieldT> round2_params = round_parameters<FieldT>(codeword_domain.get_subset_of_order(4));
    std::vector<size_t> round2_query_positions({4, 5, 6, 7});
    size_t round2_proof_size = 6*hash_size + round2_query_positions.size() * num_oracles_per_round*sizeof(FieldT);

    std::vector<round_parameters<FieldT>> all_round_params({round1_params, round2_params});
    std::vector<std::vector<size_t>> all_query_positions({round1_query_positions, round2_query_positions});
    size_t expected_proof_size = round1_proof_size + round2_proof_size;
    run_test<FieldT>(codeword_domain,
                     num_oracles_per_round, num_rounds,
                     all_round_params, all_query_positions,
                     zk, preprocessing, expected_proof_size);
    round2_query_positions = std::vector<size_t>({32, 33, 34, 35});
    run_test<FieldT>(codeword_domain,
                     num_oracles_per_round, num_rounds,
                     all_round_params, all_query_positions,
                     zk, preprocessing, expected_proof_size);

    zk = true;
    expected_proof_size += ((round1_query_positions.size() / 2) + (round2_query_positions.size() / 4)) * hash_size;
    run_test<FieldT>(codeword_domain,
                     num_oracles_per_round, num_rounds,
                     all_round_params, all_query_positions,
                     zk, preprocessing, expected_proof_size);
}

}
