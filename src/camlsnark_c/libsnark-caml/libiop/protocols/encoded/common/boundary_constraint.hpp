/**@file
*****************************************************************************
Virtual oracle for boundary constraints
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_ENCODED_COMMON_BOUNDARY_CONSTRAINT_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_COMMON_BOUNDARY_CONSTRAINT_HPP_

#include <cstring>
#include <cstddef>
#include <map>
#include <memory>
#include <vector>

#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/algebra/utils.hpp"
#include "libiop/iop/oracles.hpp"

namespace libiop {

/** A boundary constraint proves that an oracle has a certain evaluation at a point.
 *  This point can be anywhere in the domain.
 *  If the point were in the codeword domain,
 *  it does not suffice to just evaluate the oracle at that point as the prover
 *  could corrupt that particular evaluation.
 *
 *  Instead, if the verifier wants to know that an oracle f is such that hat(f)(alpha) = beta
 *  the verifier checks that x - alpha divides f - beta, which is a virtual oracle.
 *
 *  Our protocols currently need at most 1 boundary constraint.
 *  If a protocol requires more boundary constraints, it may be worth creating a
 *  "multi-boundary constraint" oracle, that combines boundary constraints with
 *  a random linear combination, in order to save on batch inversions.
*/
template<typename FieldT>
class single_boundary_constraint : public virtual_oracle<FieldT> {
protected:
    field_subset<FieldT> codeword_domain_;
    FieldT eval_point_;
    FieldT oracle_evaluation_;
public:
    single_boundary_constraint(const field_subset<FieldT> &codeword_domain);
    void set_evaluation_point_and_eval(const FieldT eval_point, const FieldT oracle_eval);
    virtual std::shared_ptr<std::vector<FieldT>> evaluated_contents(
        const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const;
    virtual FieldT evaluation_at_point(
        const std::size_t evaluation_position,
        const FieldT evaluation_point,
        const std::vector<FieldT> &constituent_oracle_evaluations) const;
};

} // libiop

#include "libiop/protocols/encoded/common/boundary_constraint.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_COMMON_BOUNDARY_CONSTRAINT_HPP_
