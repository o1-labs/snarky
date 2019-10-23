/**@file
 *****************************************************************************
 Utility methods for IOP query position handles
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_IOP_UTILITIES_QUERY_POSITIONS_HPP_
#define LIBIOP_IOP_UTILITIES_QUERY_POSITIONS_HPP_

#include <vector>

#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/iop/iop.hpp"

namespace libiop {

/** TODO: Come up with better name
 *
 */
template<typename FieldT>
std::vector<query_position_handle> query_position_to_queries_for_entire_coset(
    iop_protocol<FieldT> &IOP,
    const query_position_handle &initial_query,
    const field_subset<FieldT> &domain,
    const size_t coset_size);

} // namespace libiop

#include "libiop/iop/utilities/query_positions.tcc"

#endif // LIBIOP_IOP_UTILITIES_QUERY_POSITIONS_HPP_
