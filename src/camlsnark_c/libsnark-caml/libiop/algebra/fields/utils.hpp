/**@file
 *****************************************************************************
 API hack for treating fields as additive or multiplicative
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef LIBIOP_ALGEBRA_FIELDS_UTILS_HPP_
#define LIBIOP_ALGEBRA_FIELDS_UTILS_HPP_

#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/fields/gf128.hpp"
#include "libiop/algebra/fields/gf192.hpp"
#include "libiop/algebra/fields/gf256.hpp"

#include <libff/algebra/fields/fp.hpp>

namespace libiop {

using namespace libff;

template<typename FieldT>
struct is_additive {
    static const bool value = false;
};

template<>
struct is_additive<gf64> {
    static const bool value = true;
};

template<>
struct is_additive<gf128> {
    static const bool value = true;
};

template<>
struct is_additive<gf192> {
    static const bool value = true;
};

template<>
struct is_additive<gf256> {
    static const bool value = true;
};

template<typename FieldT>
struct is_multiplicative {
    static const bool value = false;
};

template<mp_size_t n, const bigint<n>& modulus>
struct is_multiplicative<Fp_model<n, modulus>> {
    static const bool value = true;
};

enum field_type {
    multiplicative_field_type = 1,
    additive_field_type = 2
};

template<bool B, class T = void>
struct enable_if { typedef void* type; };

template<class T>
struct enable_if<true, T> { typedef T type; };

template<typename FieldT>
field_type get_field_type(const typename libiop::enable_if<is_multiplicative<FieldT>::value, FieldT>::type elem)
{
    libiop::UNUSED(elem); // only to identify field type
    return multiplicative_field_type;
}

template<typename FieldT>
field_type get_field_type(const typename libiop::enable_if<is_additive<FieldT>::value, FieldT>::type elem)
{
    libiop::UNUSED(elem); // only to identify field type
    return additive_field_type;
}

template<typename FieldT>
std::size_t log_of_field_size_helper(
    typename libiop::enable_if<is_multiplicative<FieldT>::value, FieldT>::type field_elem)
{
    return FieldT::size_in_bits();
}

template<typename FieldT>
std::size_t log_of_field_size_helper(
    typename libiop::enable_if<is_additive<FieldT>::value, FieldT>::type field_elem)
{
    return FieldT::extension_degree();
}

template<typename FieldT>
std::size_t soundness_log_of_field_size_helper(
    typename libiop::enable_if<is_multiplicative<FieldT>::value, FieldT>::type field_elem)
{
    /** size in bits is the number of bits needed to represent a field element.
     *  However there isn't perfect alignment between the number of bits and the number of field elements,
     *  there could be a factor of two difference.
     *  For calculating soundness, we use the log of field size as number of bits - 1,
     *  as (2 << returned) size lower bounds the actual size.
    */
    return FieldT::size_in_bits() - 1;
}

template<typename FieldT>
std::size_t soundness_log_of_field_size_helper(
    typename libiop::enable_if<is_additive<FieldT>::value, FieldT>::type field_elem)
{
    return FieldT::extension_degree();
}

} // namespace libiop

#endif // LIBIOP_ALGEBRA_FIELDS_UTILS_HPP