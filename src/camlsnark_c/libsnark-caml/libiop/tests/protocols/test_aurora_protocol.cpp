#include <cstdint>
#include <stdexcept>

#include <gtest/gtest.h>

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/common/common.hpp"
#include "libiop/protocols/encoded/r1cs_rs_iop/r1cs_rs_iop.hpp"
#include "libiop/relations/examples/r1cs_examples.hpp"
#include "libiop/relations/r1cs.hpp"
#include "libiop/relations/variable.hpp"
#include "libiop/tests/protocols/utilities.cpp"

namespace libiop {

template<typename FieldT>
void run_test(field_subset_type domain_type) {
for (std::size_t constraint_domain_dim = 7; constraint_domain_dim < 9; constraint_domain_dim++) {
for (std::size_t variable_domain_dim = 7; variable_domain_dim < 9; variable_domain_dim++) {
    std::size_t input_variable_domain_dim = variable_domain_dim - 2;
    for (std::size_t make_zk_param = 0; make_zk_param < 2; make_zk_param++) {

        const bool make_zk = make_zk_param ? true : false;
        printf("running components test with constraint dim %lu, variable dim %lu, make_zk %s, is_multiplicative %s\n",
            constraint_domain_dim, variable_domain_dim,
            make_zk ? "true" : "false",
            (domain_type == multiplicative_coset_type) ? "true" : "false");
        /* Prepare a simple R1CS */
        const std::size_t num_constraints = 1 << constraint_domain_dim;
        const std::size_t num_inputs = (1 << input_variable_domain_dim) - 1;
        const std::size_t num_variables = (1 << variable_domain_dim) - 1;

        r1cs_example<FieldT> r1cs_params = generate_r1cs_example<FieldT>(
            num_constraints, num_inputs, num_variables);
        std::shared_ptr<r1cs_constraint_system<FieldT>> cs =
            std::make_shared<r1cs_constraint_system<FieldT>>(r1cs_params.constraint_system_);

        /* Set up the blueprint for the protocol */

        const std::size_t RS_extra_dimensions = 2; /* \rho = 1/4 */

        const std::size_t codeword_domain_dim = RS_extra_dimensions +
            + (make_zk ? 2 : 1)
            + std::max(constraint_domain_dim, variable_domain_dim);

        const field_subset<FieldT> unshifted_codeword_domain(1ull << codeword_domain_dim);
        const FieldT codeword_domain_shift = unshifted_codeword_domain.element_outside_of_subset();

        const field_subset<FieldT> constraint_domain(1ull << constraint_domain_dim);
        const field_subset<FieldT> variable_domain(1ull << variable_domain_dim);
        const field_subset<FieldT> codeword_domain(1ull << codeword_domain_dim, codeword_domain_shift);

        field_subset<FieldT> summation_domain;
        if (constraint_domain_dim > variable_domain_dim) {
            summation_domain = constraint_domain;
        } else {
            summation_domain = variable_domain;
        }

        // See aurora_iop.tcc for the correct way this gets parameterized for the IOP.
        const std::size_t query_bound = num_inputs + 2;

        iop_protocol<FieldT> IOP;

        const domain_handle constraint_domain_handle = IOP.register_domain(constraint_domain);
        const domain_handle variable_domain_handle = IOP.register_domain(variable_domain);
        const domain_handle codeword_domain_handle = IOP.register_domain(codeword_domain);

        const size_t dummy_security_parameter = 64;
        const bool holographic = false;
        const encoded_aurora_parameters<FieldT> params(dummy_security_parameter,
                                                       codeword_domain_dim,
                                                       constraint_domain_dim,
                                                       summation_domain.dimension(),
                                                       query_bound,
                                                       make_zk,
                                                       holographic,
                                                       domain_type);

        encoded_aurora_protocol<FieldT> proto(IOP,
                                              constraint_domain_handle,
                                              variable_domain_handle,
                                              codeword_domain_handle,
                                              cs,
                                              params);

        proto.register_challenge();
        proto.register_proof();
        IOP.seal_interaction_registrations();

        IOP.seal_query_registrations();

        /* Proving */
        proto.submit_witness_oracles(r1cs_params.primary_input_, r1cs_params.auxiliary_input_);
        IOP.signal_prover_round_done();

        proto.calculate_and_submit_proof();
        IOP.signal_prover_round_done();

        /* Verification */
        /* verify relative to the statement that the prover used */
        std::vector<oracle_handle_ptr> handles = proto.get_all_oracle_handles();
        test_oracles_degree_and_consistency(IOP, handles, codeword_domain, true);
        }
    }
}
}

TEST(R1CSAdditiveProtocolTest, R1CSTest) {
    typedef gf64 FieldT;
    run_test<FieldT>(affine_subspace_type);
}

TEST(R1CSMultiplicativeProtocolTest, R1CSTest) {
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;
    run_test<FieldT>(multiplicative_coset_type);
}

}