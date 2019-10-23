/**@file
*****************************************************************************
Common functions across different linchecks
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_ENCODED_LINCHECK_COMMON_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_LINCHECK_COMMON_HPP_

#include <cstring>
#include <cstddef>
#include <map>
#include <memory>
#include <vector>

#include "libiop/relations/sparse_matrix.hpp"
#include "libiop/algebra/polynomials/lagrange_polynomial.hpp"
#include "libiop/algebra/polynomials/bivariate_lagrange_polynomial.hpp"
#include "libiop/algebra/lagrange.hpp"
#include "libiop/iop/iop.hpp"


namespace libiop {

template<typename FieldT>
std::vector<FieldT> compute_p_alpha_M(
    const size_t input_variable_dim,
    const field_subset<FieldT> &summation_domain,
    const std::vector<FieldT> &p_alpha_over_H,
    const std::vector<FieldT> &r_Mz,
    const std::vector<std::shared_ptr<sparse_matrix<FieldT> >> &matrices);

} // libiop

#include "libiop/protocols/encoded/lincheck/common.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_LINCHECK_COMMON_HPP_
