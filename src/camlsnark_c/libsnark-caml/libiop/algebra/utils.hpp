/**@file
 *****************************************************************************
 Implementation of useful operations over vectors.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_ALGEBRA_UTILS_HPP_
#define LIBIOP_ALGEBRA_UTILS_HPP_

#include <cstdint>
#include <vector>

namespace libiop {

template<typename T>
void bitreverse_vector(std::vector<T> &a);

template<typename T>
std::vector<T> random_vector(const std::size_t count);

template<typename T>
std::vector<T> all_subset_sums(const std::vector<T> &basis, const T& shift = 0)
#if defined(__clang__)
    ;
#else
    __attribute__((optimize("unroll-loops")));
#endif

template<typename FieldT>
std::vector<FieldT> batch_inverse(const std::vector<FieldT> &vec, const bool has_zeroes=false);

template<typename FieldT>
std::vector<FieldT> batch_inverse_and_mul(const std::vector<FieldT> &vec, const FieldT &k, const bool has_zeroes=false);

template<typename FieldT>
void mut_batch_inverse(std::vector<FieldT> &vec);

/** un-optimized simple GCD procedure */
size_t gcd(const size_t a, const size_t b);

} // namespace libiop

#include "libiop/algebra/utils.tcc"

#endif // LIBIOP_ALGEBRA_UTILS_HPP_
