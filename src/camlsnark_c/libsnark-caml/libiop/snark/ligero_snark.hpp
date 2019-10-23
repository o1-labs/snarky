/**@file
 *****************************************************************************
 Interleaved R1CS zk-STARK obtained by combining Ligero IOP for R1CS and the
 BCS16 transformation.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_SNARK_LIGERO_SNARK_HPP_
#define LIBIOP_SNARK_LIGERO_SNARK_HPP_

#include <cstddef>
#include <iostream>

#include "libiop/algebra/fields/utils.hpp"
#include "libiop/protocols/ligero_iop.hpp"
#include "libiop/relations/r1cs.hpp"
#include "libiop/snark/common/bcs_common.hpp"
#include "libiop/snark/common/bcs_prover.hpp"
#include "libiop/snark/common/bcs_verifier.hpp"

namespace libiop {

template<typename FieldT>
struct ligero_snark_parameters {
    size_t security_level_;
    float height_width_ratio_;
    size_t RS_extra_dimensions_;
    LDT_reducer_soundness_type LDT_reducer_soundness_type_ = LDT_reducer_soundness_type::proven;
    bool make_zk_;
    field_subset_type domain_type_;

    void describe();
};

template<typename FieldT>
using ligero_snark_argument = bcs_transformation_transcript<FieldT>;

template<typename FieldT>
ligero_snark_argument<FieldT> ligero_snark_prover(const r1cs_constraint_system<FieldT> &constraint_system,
                                                  const r1cs_primary_input<FieldT> &primary_input,
                                                  const r1cs_auxiliary_input<FieldT> &auxiliary_input,
                                                  const ligero_snark_parameters<FieldT> &parameters);

template<typename FieldT>
bool ligero_snark_verifier(const r1cs_constraint_system<FieldT> &constraint_system,
                                     const r1cs_primary_input<FieldT> &primary_input,
                                     const ligero_snark_argument<FieldT> &proof,
                                     const ligero_snark_parameters<FieldT> &parameters);

} // namespace libiop

#include "libiop/snark/ligero_snark.tcc"

#endif // LIBIOP_SNARK_LIGERO_SNARK_HPP_
