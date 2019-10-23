/**@file
 *****************************************************************************
 Helper methods shared between multiple sumchecks
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_ENCODED_SUMCHECK_AUX_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_SUMCHECK_AUX_HPP_

#include <vector>
#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/algebra/field_subset/subgroup.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/algebra/exponentiation.hpp"

namespace libiop {

template<typename FieldT>
std::vector<FieldT> constant_times_subspace_to_order_H_minus_1(
    const FieldT constant,
    const affine_subspace<FieldT> &subspace,
    const size_t H);

} // namespace libiop

#include "libiop/protocols/encoded/sumcheck/sumcheck_aux.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_SUMCHECK_AUX_HPP_
