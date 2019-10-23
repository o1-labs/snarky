/**@file
*****************************************************************************
Implementation for FRI localizer polynomial
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_LDT_FRI_LOCALIZER_POLYNOMIAL_HPP_
#define LIBIOP_PROTOCOLS_LDT_FRI_LOCALIZER_POLYNOMIAL_HPP_

#include <functional>

#include "libiop/algebra/fields/utils.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"

namespace libiop {

/** The FRI localizer polynomial is a k to 1 map for the domain it is defined on,
 *  k being 2^(localization parameter).
 *  We define the localizer polynomial from the "localizer domain",
 *  which is a coset of size k and no affine shift of the domain which we want the localize.
 *  In the additive case this polynomial is the vanishing polynomial of the localizer domain.
 *  In the multiplicative case, it is the polynomial x^{2^{n}}, n being the localization parameter.
 *  Note that the localizer domain is of size 2^n, so we derive the polynomial from the localizer domain.
 *  TODO: Delete this class in favor of vanishing polynomial . associated k to 1 map.
 */
template<typename FieldT>
class localizer_polynomial {
    protected:
    field_subset_type type_;
    // additive case
    linearized_polynomial<FieldT> additive_poly_;
    // multiplicative case
    std::size_t degree_;
    public:
    explicit localizer_polynomial(const field_subset<FieldT> domain);
    FieldT evaluation_at_point(const FieldT &evalpoint) const;
    linearized_polynomial<FieldT> get_linearized_polynomial() const;
};

}

#include "libiop/protocols/ldt/fri/localizer_polynomial.tcc"

#endif // LIBIOP_PROTOCOLS_LDT_FRI_LOCALIZER_POLYNOMIAL_HPP_