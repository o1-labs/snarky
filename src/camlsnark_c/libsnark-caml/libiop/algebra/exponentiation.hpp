/**@file
 *****************************************************************************
 Implementation of exponentiation algorithms.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_ALGEBRA_EXPONENTIATION_HPP_
#define LIBIOP_ALGEBRA_EXPONENTIATION_HPP_

#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/algebra/field_subset/subgroup.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"

namespace libiop {

template<typename FieldT>
FieldT power(const FieldT &base, const std::size_t exponent);

template<typename FieldT>
std::vector<FieldT> subset_element_powers(const field_subset<FieldT> &S,
                                          const std::size_t exponent);

template<typename FieldT>
std::vector<FieldT> subspace_element_powers(const affine_subspace<FieldT> &S,
                                            const std::size_t exponent);

template<typename FieldT>
std::vector<FieldT> coset_element_powers(const multiplicative_coset<FieldT> &S,
                                         const std::size_t exponent);

} // namespace libiop

#include "libiop/algebra/exponentiation.tcc"

#endif // LIBIOP_ALGEBRA_EXPONENTIATION_HPP_
