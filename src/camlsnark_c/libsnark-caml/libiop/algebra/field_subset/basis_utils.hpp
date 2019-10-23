/**@file
 *****************************************************************************
 Utility functions for subspace bases.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_ALGEBRA_BASIS_UTILS_HPP_
#define LIBIOP_ALGEBRA_BASIS_UTILS_HPP_

#include <cstddef>
#include <vector>
#include "libiop/algebra/fields/utils.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"

namespace libiop {

/** Creates a basis of monomials, of the specified dimension.
 *  This method returns the basis x^i, x^{i + 1}, ..., x^{i + d - 1},
 *  where i is the smallest exponent, and d is the dimension.
 */
template<typename FieldT>
std::vector<FieldT> monomial_basis(const size_t dimension, const size_t smallest_exponent);

/** Returns the evaluations of the provided polynomial over the basis. */
template<typename FieldT>
std::vector<FieldT> transform_basis_by_polynomial(
    const std::shared_ptr<polynomial_base<FieldT>> transform, const std::vector<FieldT> basis);

} // namespace libiop

#include "libiop/algebra/field_subset/basis_utils.tcc"

#endif // LIBIOP_ALGEBRA_BASIS_UTILS_HPP_