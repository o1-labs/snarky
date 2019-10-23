/**@file
 *****************************************************************************
 Common routines.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_COMMON_COMMON_HPP_
#define LIBIOP_COMMON_COMMON_HPP_

#include <cstddef>
#include <cstdio>
#include <cstdint>
#include <iostream>
#include <vector>
#include <math.h>

namespace libiop {

/* returns ceil(log2(n)), so 1ul<<log2(n) is the smallest power of 2,
   that is not less than n. */
std::size_t log2(std::size_t n);

std::size_t round_to_next_power_of_2(const std::size_t n);
bool is_power_of_2(const std::size_t n);

std::size_t bitreverse(std::size_t n, const std::size_t l);

long double add_soundness_error_bits(const size_t bits1, const size_t bits2);
long double add_soundness_error_bits(const long double bits1, const long double bits2);

/* A variadic template to suppress unused argument warnings */
template<typename ... Types>
void UNUSED(Types&&...) {}

/* Print a vector in the form { elem0 elem1 elem2 ... }, with a newline at the end*/
template<typename T>
void print_vector(std::vector<T> &vec);
template<typename T>
void print_vector(std::vector<T> vec);

} // namespace libiop

#endif // LIBIOP_COMMON_COMMON_HPP_
