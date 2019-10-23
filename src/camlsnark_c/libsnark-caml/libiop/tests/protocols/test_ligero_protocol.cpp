#include <cstdint>
#include <stdexcept>

#include <gtest/gtest.h>

#include <libff/algebra/curves/edwards/edwards_pp.hpp>

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/common/common.hpp"
#include "libiop/protocols/encoded/ligero/ligero.hpp"
#include "libiop/relations/examples/r1cs_examples.hpp"
#include "libiop/relations/r1cs.hpp"
#include "libiop/relations/variable.hpp"

namespace libiop {

template<typename FieldT>
bool run_test(r1cs_constraint_system<FieldT> constraint_system,
              r1cs_primary_input<FieldT> primary_input,
              r1cs_auxiliary_input<FieldT> auxiliary_input,
              bool make_zk,
              field_subset_type domain_type)
{
    /* Set up the blueprint for the protocol */
    iop_protocol<FieldT> IOP;
    
    float height_width_ratio = 0.1;
    std::size_t RS_extra_dimensions = 2;

    std::size_t num_vars = constraint_system.num_variables() + 1;
    
    std::size_t systematic_domain_size = (std::size_t) ceil(sqrt(num_vars / height_width_ratio));
    systematic_domain_size = round_to_next_power_of_2(systematic_domain_size);
    
    const std::size_t systematic_domain_dim = log2(systematic_domain_size);
    std::size_t num_oracles_input = (std::size_t) ceil(((float) num_vars) / systematic_domain_size);

    const std::size_t codeword_domain_dim = systematic_domain_dim + RS_extra_dimensions;
    const std::size_t codeword_domain_size = 1ull << codeword_domain_dim;
    
    print_indent(); printf("codeword subspace dim: %zu\n", codeword_domain_dim);
    
    const std::size_t matrix_width = systematic_domain_size * num_oracles_input;
    
    std::size_t matrix_height = constraint_system.num_constraints();
    if (matrix_height % systematic_domain_size != 0)
    {
        matrix_height += systematic_domain_size - matrix_height % systematic_domain_size;
    }
    const std::size_t num_oracles_vectors = matrix_height / systematic_domain_size;

    const std::size_t extended_systematic_domain_size = systematic_domain_size << 1;
    
    print_indent(); printf("num oracles for vectors / R1CS constraints (m_2): %zu\n", num_oracles_vectors);
    
    FieldT shift;

    if (domain_type == multiplicative_coset_type)
    {
        shift = FieldT::multiplicative_generator;
    }
    else
    {
        shift = FieldT(codeword_domain_size);
    }

    field_subset<FieldT> codeword_domain(codeword_domain_size);
    field_subset<FieldT> systematic_domain(systematic_domain_size, shift);
    field_subset<FieldT> extended_systematic_domain(extended_systematic_domain_size, shift);

    const domain_handle codeword_domain_handle = IOP.register_domain(codeword_domain);
    const domain_handle systematic_domain_handle = IOP.register_domain(systematic_domain);
    const domain_handle extended_systematic_domain_handle = IOP.register_domain(extended_systematic_domain);

    encoded_ligero_parameters parameters;
    parameters.num_interaction_phase_repetitions_ = 6;
    parameters.num_query_phase_repetitions_ = 2;
    parameters.make_zk_ = make_zk;
    parameters.field_subset_type_ = domain_type;
    parameters.matrix_width_ = matrix_width;
    parameters.matrix_height_ = matrix_height;
    parameters.num_oracles_input_ = num_oracles_input;
    parameters.num_oracles_vectors_ = num_oracles_vectors;
    
    interleaved_r1cs_protocol<FieldT> proto(IOP,
                                            codeword_domain_handle,
                                            systematic_domain_handle,
                                            extended_systematic_domain_handle,
                                            constraint_system,
                                            parameters);
    
    proto.attach_oracles();
    
    proto.register_linear_combinations();
    proto.register_responses();
    IOP.seal_interaction_registrations();
    
    proto.register_queries();
    IOP.seal_query_registrations();
    
    /* Proving */
    proto.submit_witness_oracles(primary_input, auxiliary_input);
    if (make_zk)
    {
        proto.submit_blinding_vector_oracles();
    }
    IOP.signal_prover_round_done();
    
    proto.calculate_and_submit_proof(primary_input);
    IOP.signal_prover_round_done();
    
    /* Verification */
    return proto.verifier_predicate(primary_input);
    return false;
}

TEST(LigeroTrueTest, SimpleTest) {
    typedef gf64 FieldT;

    std::size_t num_constraints = 16;
    std::size_t num_inputs = 8;
    std::size_t num_variables = 15;
    r1cs_example<FieldT> ex = generate_r1cs_example<FieldT>(num_constraints, num_inputs, num_variables);
    
    r1cs_constraint_system<FieldT> constraint_system = ex.constraint_system_;
    r1cs_primary_input<FieldT> primary_input = ex.primary_input_;
    r1cs_auxiliary_input<FieldT> auxiliary_input = ex.auxiliary_input_;
    
    ASSERT_TRUE(constraint_system.is_satisfied(primary_input, auxiliary_input));
    
    EXPECT_TRUE(run_test<FieldT>(constraint_system, primary_input, auxiliary_input, true, affine_subspace_type));
    EXPECT_TRUE(run_test<FieldT>(constraint_system, primary_input, auxiliary_input, false, affine_subspace_type));
}

TEST(LigeroTrueMultiplicativeTest, SimpleTest) {
    edwards_pp::init_public_params();

    typedef edwards_Fr FieldT;

    std::size_t num_constraints = 16;
    std::size_t num_inputs = 8;
    std::size_t num_variables = 15;
    r1cs_example<FieldT> ex = generate_r1cs_example<FieldT>(num_constraints, num_inputs, num_variables);
    
    r1cs_constraint_system<FieldT> constraint_system = ex.constraint_system_;
    r1cs_primary_input<FieldT> primary_input = ex.primary_input_;
    r1cs_auxiliary_input<FieldT> auxiliary_input = ex.auxiliary_input_;
    
    ASSERT_TRUE(constraint_system.is_satisfied(primary_input, auxiliary_input));
    
    EXPECT_TRUE(run_test<FieldT>(constraint_system, primary_input, auxiliary_input, true, multiplicative_coset_type));
    EXPECT_TRUE(run_test<FieldT>(constraint_system, primary_input, auxiliary_input, false, multiplicative_coset_type));
}
    
TEST(LigeroWrongPrimaryInputTest, SimpleTest) {
    typedef gf64 FieldT;

    std::size_t num_constraints = 16;
    std::size_t num_inputs = 8;
    std::size_t num_variables = 15;
    r1cs_example<FieldT> ex = generate_r1cs_example<FieldT>(num_constraints, num_inputs, num_variables);
    
    r1cs_constraint_system<FieldT> constraint_system = ex.constraint_system_;
    r1cs_primary_input<FieldT> primary_input = ex.primary_input_;
    r1cs_auxiliary_input<FieldT> auxiliary_input = ex.auxiliary_input_;
    
    ASSERT_TRUE(constraint_system.is_satisfied(primary_input, auxiliary_input));
    
    for (size_t i = 0; i < primary_input.size(); ++i)
    {
        primary_input[i] += FieldT(1);
    }
    
    EXPECT_FALSE(run_test<FieldT>(constraint_system, primary_input, auxiliary_input, true, affine_subspace_type));
    EXPECT_FALSE(run_test<FieldT>(constraint_system, primary_input, auxiliary_input, false, affine_subspace_type));
}

TEST(LigeroWrongPrimaryInputMultiplicativeTest, SimpleTest) {
    edwards_pp::init_public_params();

    typedef edwards_Fr FieldT;

    std::size_t num_constraints = 16;
    std::size_t num_inputs = 8;
    std::size_t num_variables = 15;
    r1cs_example<FieldT> ex = generate_r1cs_example<FieldT>(num_constraints, num_inputs, num_variables);
    
    r1cs_constraint_system<FieldT> constraint_system = ex.constraint_system_;
    r1cs_primary_input<FieldT> primary_input = ex.primary_input_;
    r1cs_auxiliary_input<FieldT> auxiliary_input = ex.auxiliary_input_;
    
    ASSERT_TRUE(constraint_system.is_satisfied(primary_input, auxiliary_input));
    
    for (size_t i = 0; i < primary_input.size(); ++i)
    {
        primary_input[i] += FieldT(1);
    }
    
    EXPECT_FALSE(run_test<FieldT>(constraint_system, primary_input, auxiliary_input, true, multiplicative_coset_type));
    EXPECT_FALSE(run_test<FieldT>(constraint_system, primary_input, auxiliary_input, false, multiplicative_coset_type));
}
    
TEST(LigeroWrongAuxiliaryInputTest, SimpleTest) {
    typedef gf64 FieldT;

    std::size_t num_constraints = 16;
    std::size_t num_inputs = 8;
    std::size_t num_variables = 15;
    r1cs_example<FieldT> ex = generate_r1cs_example<FieldT>(num_constraints, num_inputs, num_variables);
    
    r1cs_constraint_system<FieldT> constraint_system = ex.constraint_system_;
    r1cs_primary_input<FieldT> primary_input = ex.primary_input_;
    r1cs_auxiliary_input<FieldT> auxiliary_input = ex.auxiliary_input_;
    
    ASSERT_TRUE(constraint_system.is_satisfied(primary_input, auxiliary_input));

    for (size_t i = 0; i < auxiliary_input.size(); ++i)
    {
        auxiliary_input[i] += FieldT(1);
    }

    EXPECT_FALSE(run_test<FieldT>(constraint_system, primary_input, auxiliary_input, true, affine_subspace_type));
    EXPECT_FALSE(run_test<FieldT>(constraint_system, primary_input, auxiliary_input, false, affine_subspace_type));
}

TEST(LigeroWrongAuxiliaryInputMultiplicativeTest, SimpleTest) {
    edwards_pp::init_public_params();

    typedef edwards_Fr FieldT;

    std::size_t num_constraints = 16;
    std::size_t num_inputs = 8;
    std::size_t num_variables = 15;
    r1cs_example<FieldT> ex = generate_r1cs_example<FieldT>(num_constraints, num_inputs, num_variables);
    
    r1cs_constraint_system<FieldT> constraint_system = ex.constraint_system_;
    r1cs_primary_input<FieldT> primary_input = ex.primary_input_;
    r1cs_auxiliary_input<FieldT> auxiliary_input = ex.auxiliary_input_;
    
    ASSERT_TRUE(constraint_system.is_satisfied(primary_input, auxiliary_input));

    for (size_t i = 0; i < auxiliary_input.size(); ++i)
    {
        auxiliary_input[i] += FieldT(1);
    }

    EXPECT_FALSE(run_test<FieldT>(constraint_system, primary_input, auxiliary_input, true, multiplicative_coset_type));
    EXPECT_FALSE(run_test<FieldT>(constraint_system, primary_input, auxiliary_input, false, multiplicative_coset_type));
}

}
