/** @file
 *****************************************************************************

 Declaration of interfaces for a R1CS example, as well as functions to sample
 R1CS examples with prescribed parameters (according to some distribution).

 *****************************************************************************
 * @author     This file is adapted from libsnark, developed by SCIPR Lab
 *             and contributors
 *             (see AUTHORS for libsnark and here for libiop).
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef RELATIONS__EXAMPLES__R1CS_EXAMPLES_HPP_
#define RELATIONS__EXAMPLES__R1CS_EXAMPLES_HPP_

#include "libiop/relations/r1cs.hpp"

namespace libiop {

/**
 * A R1CS example comprises a R1CS constraint system, R1CS input, and R1CS witness.
 */
template<typename FieldT>
struct r1cs_example {
    r1cs_constraint_system<FieldT> constraint_system_;
    r1cs_primary_input<FieldT> primary_input_;
    r1cs_auxiliary_input<FieldT> auxiliary_input_;

    r1cs_example<FieldT>() = default;
    r1cs_example<FieldT>(const r1cs_example<FieldT> &other) = default;
    r1cs_example<FieldT>(const r1cs_constraint_system<FieldT> &constraint_system,
                         const r1cs_primary_input<FieldT> &primary_input,
                         const r1cs_auxiliary_input<FieldT> &auxiliary_input) :
        constraint_system_(constraint_system),
        primary_input_(primary_input),
        auxiliary_input_(auxiliary_input)
    {};
    r1cs_example<FieldT>(r1cs_constraint_system<FieldT> &&constraint_system,
                         r1cs_primary_input<FieldT> &&primary_input,
                         r1cs_auxiliary_input<FieldT> &&auxiliary_input) :
        constraint_system_(std::move(constraint_system)),
        primary_input_(std::move(primary_input)),
        auxiliary_input_(std::move(auxiliary_input))
    {};
};

/**
 * Generate a R1CS example such that:
 * - the number of constraints of the R1CS constraint system is num_constraints;
 * - the number of inputs of the R1CS constraint system is num_inputs;
 * - the number of variables (excluding the constant 1 variable) of
     the R1CS constraint system is num_variables;
 */
template<typename FieldT>
r1cs_example<FieldT> generate_r1cs_example(const size_t num_constraints,
                                           const size_t num_inputs,
                                           const size_t num_variables);

} // libiop

#include "libiop/relations/examples/r1cs_examples.tcc"

#endif // RELATIONS__EXAMPLES__R1CS_EXAMPLES_HPP_
