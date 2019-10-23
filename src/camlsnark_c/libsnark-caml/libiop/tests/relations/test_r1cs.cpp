#include <cstdint>
#include <stdexcept>

#include <gtest/gtest.h>

#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/common/common.hpp"
#include "libiop/relations/r1cs.hpp"
#include "libiop/relations/variable.hpp"
#include "libiop/relations/examples/r1cs_examples.hpp"
#include <libff/algebra/curves/edwards/edwards_pp.hpp>
#include <libff/algebra/curves/alt_bn128/alt_bn128_pp.hpp>

namespace libiop {

TEST(R1CSTest, SimpleTest) {
    typedef gf64 FieldT;

    r1cs_constraint_system<FieldT> constraints;
    constraints.primary_input_size_ = 1;
    constraints.auxiliary_input_size_ = 1;

    variable<FieldT> const1(0), x(1), y(2);
    constraints.add_constraint(r1cs_constraint<FieldT>({ x },
                                                       { y },
                                                       { const1 }));

    const FieldT r = FieldT::random_element();
    const FieldT rinv = r.inverse();

    EXPECT_TRUE(constraints.is_satisfied({r}, {rinv}));
    EXPECT_FALSE(constraints.is_satisfied({r}, {r}));
}

TEST(R1CSGeneratorTest, SimpleTests) {
    for (std::size_t variable_dim = 8; variable_dim < 10; variable_dim++) {
        for (std::size_t constraint_dim = 8; constraint_dim < 10; constraint_dim++) {
            const std::size_t num_variables = (1ull << variable_dim) - 1;
            const std::size_t num_inputs = (1ull << (variable_dim - 2)) - 1;
            const std::size_t num_constraints = (1ull << constraint_dim);

            r1cs_example<gf64> r1cs_params_gf64 = generate_r1cs_example<gf64>(
                num_constraints, num_inputs, num_variables);
            EXPECT_TRUE(r1cs_params_gf64.constraint_system_.is_satisfied(r1cs_params_gf64.primary_input_,
                                                                        r1cs_params_gf64.auxiliary_input_));
            
            libff::edwards_pp::init_public_params();
            r1cs_example<libff::edwards_Fr> r1cs_params_edwards = generate_r1cs_example<libff::edwards_Fr>(
                num_constraints, num_inputs, num_variables);
            EXPECT_TRUE(r1cs_params_edwards.constraint_system_.is_satisfied(r1cs_params_edwards.primary_input_,
                                                                            r1cs_params_edwards.auxiliary_input_));
            
            libff::alt_bn128_pp::init_public_params();
            r1cs_example<libff::alt_bn128_Fr> r1cs_params_alt_bn = generate_r1cs_example<libff::alt_bn128_Fr>(
                num_constraints, num_inputs, num_variables);
            EXPECT_TRUE(r1cs_params_alt_bn.constraint_system_.is_satisfied(r1cs_params_alt_bn.primary_input_,
                                                                        r1cs_params_alt_bn.auxiliary_input_));
        }
    }
}

}
