#include <cstdint>

#include <gtest/gtest.h>

#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/snark/aurora_snark.hpp"
#include "libiop/relations/examples/r1cs_examples.hpp"
#include <libff/algebra/curves/edwards/edwards_pp.hpp>

namespace libiop {

TEST(AuroraSnarkTest, SimpleTest) {
    /* Set up R1CS */
    typedef gf64 FieldT;

    const std::size_t num_constraints = 1 << 13;
    const std::size_t num_inputs = (1 << 5) - 1;
    const std::size_t num_variables = (1 << 13) - 1;
    const size_t security_parameter = 128;
    const size_t RS_extra_dimensions = 2;
    const size_t FRI_localization_parameter = 3;
    const LDT_reducer_soundness_type ldt_reducer_soundness_type = LDT_reducer_soundness_type::optimistic_heuristic;
    const FRI_soundness_type fri_soundness_type = FRI_soundness_type::heuristic;
    const field_subset_type domain_type = affine_subspace_type;

    r1cs_example<FieldT> r1cs_params = generate_r1cs_example<FieldT>(
        num_constraints, num_inputs, num_variables);
    EXPECT_TRUE(r1cs_params.constraint_system_.is_satisfied(
        r1cs_params.primary_input_, r1cs_params.auxiliary_input_));

    /* Actual SNARK test */
    for (std::size_t i = 0; i < 2; i++) {
        const bool make_zk = (i == 0) ? false : true;
        aurora_snark_parameters<FieldT> params(security_parameter,
                                               ldt_reducer_soundness_type,
                                               fri_soundness_type,
                                               FRI_localization_parameter,
                                               RS_extra_dimensions,
                                               make_zk,
                                               domain_type,
                                               num_constraints,
                                               num_variables);
        const aurora_snark_argument<FieldT> argument = aurora_snark_prover<FieldT>(
            r1cs_params.constraint_system_,
            r1cs_params.primary_input_,
            r1cs_params.auxiliary_input_,
            params);

        printf("iop size in bytes %lu\n", argument.IOP_size_in_bytes());
        printf("bcs size in bytes %lu\n", argument.BCS_size_in_bytes());
        printf("argument size in bytes %lu\n", argument.size_in_bytes());

        const bool bit = aurora_snark_verifier<FieldT>(
            r1cs_params.constraint_system_,
            r1cs_params.primary_input_,
            argument,
            params);

        EXPECT_TRUE(bit) << "failed on make_zk = " << i << " test";
    }
}

TEST(AuroraSnarkMultiplicativeTest, SimpleTest) {
    /* Set up R1CS */
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;

    const size_t num_constraints = 1 << 13;
    const size_t num_inputs = (1 << 5) - 1;
    const size_t num_variables = (1 << 13) - 1;
    const size_t security_parameter = 128;
    const size_t RS_extra_dimensions = 2;
    const size_t FRI_localization_parameter = 3;
    const LDT_reducer_soundness_type ldt_reducer_soundness_type = LDT_reducer_soundness_type::optimistic_heuristic;
    const FRI_soundness_type fri_soundness_type = FRI_soundness_type::heuristic;
    const field_subset_type domain_type = multiplicative_coset_type;

    r1cs_example<FieldT> r1cs_params = generate_r1cs_example<FieldT>(
        num_constraints, num_inputs, num_variables);
    EXPECT_TRUE(r1cs_params.constraint_system_.is_satisfied(
        r1cs_params.primary_input_, r1cs_params.auxiliary_input_));

    /* Actual SNARK test */
    for (std::size_t i = 0; i < 2; i++) {
        const bool make_zk = (i == 0) ? false : true;
        aurora_snark_parameters<FieldT> params(security_parameter,
                                               ldt_reducer_soundness_type,
                                               fri_soundness_type,
                                               FRI_localization_parameter,
                                               RS_extra_dimensions,
                                               make_zk,
                                               domain_type,
                                               num_constraints,
                                               num_variables);
        const aurora_snark_argument<FieldT> argument = aurora_snark_prover<FieldT>(
            r1cs_params.constraint_system_,
            r1cs_params.primary_input_,
            r1cs_params.auxiliary_input_,
            params);

        printf("iop size in bytes %lu\n", argument.IOP_size_in_bytes());
        printf("bcs size in bytes %lu\n", argument.BCS_size_in_bytes());
        printf("argument size in bytes %lu\n", argument.size_in_bytes());
        const bool bit = aurora_snark_verifier<FieldT>(
            r1cs_params.constraint_system_,
            r1cs_params.primary_input_,
            argument,
            params);

        EXPECT_TRUE(bit) << "failed on make_zk = " << i << " test";
    }
}

}
