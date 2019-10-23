/**@file
*****************************************************************************
Virtual oracle for taking the shifted random linear combination
required in the LDT reducer.

The shifting refers to any oracle f of sub-maximal degree having
a corresponding oracle of the form
    x^{max degree - degree(f)} * f(x)
appearing in the random linear combiniation.
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_LDT_LDT_REDUCER_AUX_HPP_
#define LIBIOP_PROTOCOLS_LDT_LDT_REDUCER_AUX_HPP_

#include <algorithm>

#include "libiop/algebra/exponentiation.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/iop/utilities/batching.hpp"
#include "libiop/protocols/ldt/multi_ldt_base.hpp"

namespace libiop {

template<typename FieldT>
class combined_LDT_virtual_oracle : public virtual_oracle<FieldT> {
protected:
    field_subset<FieldT> codeword_domain_;
    std::vector<std::size_t> input_oracle_degrees_;
    std::vector<FieldT> coefficients_;

    std::size_t num_input_oracles_;
    std::size_t num_random_coefficients_;
    std::size_t max_degree_;

    std::vector<std::size_t> submaximal_oracle_indices_;
    std::vector<std::size_t> maximal_oracle_indices_;
public:
    combined_LDT_virtual_oracle(const field_subset<FieldT> &codeword_domain,
                                const std::vector<std::size_t>& input_oracle_degrees);

    void set_random_coefficients(const std::vector<FieldT>& random_coefficients);

    virtual std::shared_ptr<std::vector<FieldT>> evaluated_contents(
        const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const;

    virtual FieldT evaluation_at_point(
        const std::size_t evaluation_position,
        const FieldT evaluation_point,
        const std::vector<FieldT> &constituent_oracle_evaluations) const;
};

} // namespace libiop

#include "libiop/protocols/ldt/ldt_reducer_aux.tcc"

#endif // LIBIOP_PROTOCOLS_LDT_LDT_REDUCER_AUX_HPP_
