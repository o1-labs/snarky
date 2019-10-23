/**@file
*****************************************************************************
Virtual oracle for random linear combinations of other oracles
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_ENCODED_COMMON_RANDOM_LINEAR_COMBINATION_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_COMMON_RANDOM_LINEAR_COMBINATION_HPP_

#include <cstring>
#include <cstddef>
#include <map>
#include <memory>
#include <vector>

#include "libiop/algebra/lagrange.hpp"

namespace libiop {

template<typename FieldT>
class random_linear_combination_oracle : public virtual_oracle<FieldT> {
protected:
    std::vector<FieldT> random_coefficients_;
    std::size_t num_oracles_;
public:
    random_linear_combination_oracle(const std::size_t num_oracles);
    void set_random_coefficients(const std::vector<FieldT>& random_coefficients);
    virtual std::shared_ptr<std::vector<FieldT>> evaluated_contents(
        const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const;
    virtual FieldT evaluation_at_point(
        const std::size_t evaluation_position,
        const FieldT evaluation_point,
        const std::vector<FieldT> &constituent_oracle_evaluations) const;
};

} // libiop

#include "libiop/protocols/encoded/common/random_linear_combination.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_COMMON_RANDOM_LINEAR_COMBINATION_HPP_
