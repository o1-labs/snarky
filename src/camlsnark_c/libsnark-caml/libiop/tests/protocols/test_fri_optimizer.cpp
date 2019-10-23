#include <cstdint>
#include <gtest/gtest.h>
#include <vector>

#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/protocols/ldt/fri/fri_aux.hpp"
#include "libiop/protocols/ldt/fri/argument_size_optimizer.hpp"
#include "libiop/snark/fri_snark.hpp"
#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/utils.hpp"
#include <libff/algebra/curves/edwards/edwards_pp.hpp>
#include <libff/algebra/curves/alt_bn128/alt_bn128_pp.hpp>


namespace libiop {

/* Test FRI*/
TEST(FriOptimizerPredictedProofSizeTest, OptimizerTest)
{
    typedef gf192 FieldT;
    const std::vector<size_t> locality_vector = {1};
    const std::vector<size_t> localization_vector = {1, 3, 3};
    const size_t codeword_dim = 18;
    const size_t num_queries = 32;
    const size_t RS_extra_dimensions = 2;
    const size_t max_tested_degree = 1ull << (codeword_dim - RS_extra_dimensions);
    const size_t hash_size_in_bytes = 32;
    const bool debug = false;

    const size_t expected_size = argument_size_predictor<FieldT>(
        locality_vector, localization_vector, codeword_dim,
        num_queries, max_tested_degree, hash_size_in_bytes);

    const size_t num_samples = 30;
    size_t total_argument_size = 0;
    FRI_snark_parameters<FieldT> params = {codeword_dim, 128, RS_extra_dimensions, 0,
        localization_vector, 1, num_queries, locality_vector[0], additive_field_type};
    for (size_t i = 0; i < num_samples; i++)
    {
        FRI_snark_proof<FieldT> proof = FRI_snark_prover<FieldT>(params);
        total_argument_size += proof.size_in_bytes();
        if (debug)
        {
            FRI_snark_print_detailed_argument_size(params, proof);
        }
    }
    size_t calculated_argument_size = total_argument_size / num_samples;
    printf("calculated argument size %lu\n", calculated_argument_size);
    printf("expected argument size %lu\n", expected_size);
    float size_difference = float(expected_size) - float(calculated_argument_size);
    float percentage_differrence = size_difference / float(calculated_argument_size);
    float max_delta = .04;
    ASSERT_GE(max_delta, abs(percentage_differrence));
}

/* Test that all localization vectors of the correct size are produced */
// TEST(LocalizationVectorGeneratorTests, OptimizerAuxTest) {
//     std::vector<std::vector<size_t>> all_loc_vector_size_3 =
//         all_localization_vectors(3);
//     std::vector<std::vector<size_t>> expected_loc_vector_size_3(
//         {{1}, {1, 1}, {1, 1, 1}, {1, 2}});
//     ASSERT_EQ(all_loc_vector_size_3, expected_loc_vector_size_3);
//     std::vector<std::vector<size_t>> all_loc_vector_size_4 =
//         all_localization_vectors(4);
//     std::vector<std::vector<size_t>> expected_loc_vector_size_4(
//         {{1}, {1, 1}, {1, 1, 1}, {1, 1, 1, 1},
//         {1, 1, 2}, {1, 2}, {1, 2, 1}, {1, 3}});
//     ASSERT_EQ(all_loc_vector_size_4, expected_loc_vector_size_4);
// }
}
