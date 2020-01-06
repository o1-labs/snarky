/** @file
 *****************************************************************************

 Declaration of functionality that is shared among MNT curves.

 *****************************************************************************
 * @author     This file is part of libff, developed by SCIPR Lab
 *             and contributors (see AUTHORS).
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef MNT46753_COMMON_HPP_
#define MNT46753_COMMON_HPP_

#include <libff/algebra/fields/bigint.hpp>

namespace libff {

const mp_size_t mnt46753_A_bitcount = 753;
const mp_size_t mnt46753_B_bitcount = 753;

const mp_size_t mnt46753_A_limbs = (mnt46753_A_bitcount+GMP_NUMB_BITS-1)/GMP_NUMB_BITS;
const mp_size_t mnt46753_B_limbs = (mnt46753_B_bitcount+GMP_NUMB_BITS-1)/GMP_NUMB_BITS;

extern bigint<mnt46753_A_limbs> mnt46753_modulus_A;
extern bigint<mnt46753_B_limbs> mnt46753_modulus_B;

} // libff

#endif
