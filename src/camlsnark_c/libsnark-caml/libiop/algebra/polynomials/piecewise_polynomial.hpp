/**@file
 *****************************************************************************
 Abstract class for piecewise polynomials.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_ALGEBRA_POLYNOMIALS_PIECEWISE_POLYNOMIAL_HPP_
#define LIBIOP_ALGEBRA_POLYNOMIALS_PIECEWISE_POLYNOMIAL_HPP_

#include <cstddef>
#include <vector>

#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"

namespace libiop {

/** See Succinct Aurora Definition 6.11 for a more formal definition.
 *  TODO: Sync the above line with paper name, and finalized definition number.
 *  Let H be a subset of our field.
 *  An efficient piecewise polynomial P on H is a pair (S, F),
 *  where S is a partition of H, (S = (S_1, ..., S_n)),
 *  and F is a set of functions (F = (f_1, ..., f_n) ∈ Field[X]^n).
 *  This pair has the following property:
 *     - s_i is the unique polynomial of degree less than |S| which is an indicator for S_i in S.
 *       This means for x in S_i, s_i(x) = 1, and for x in S / S_i, s_i(x) = 0.
 *     - s_i can be evaluated in polylog(|S|)
 *     - f_i can be evaluated in polylog(|S|)
 *     - n in polylog(|S|)
 *     - for all x in S_i, F(x) = f_i(x)
 *  Consequently we can evaluate P in polylog time as:
 *      P(x) = sum s_i(x)f_i(x)
 *  The piecewise degree of P is max degree(f_i),
 *  and this is important as any polynomial g can be composed with P as:
 *      g(P(x)) = sum s_i(x) g(f_i(x))
 *  so the total degree is |S| + deg(g) * piecewise degree,
 *  as opposed to |S| * deg(g) if this were normal polynomial composition.
 */
template<typename FieldT>
class piecewise_polynomial_base : public polynomial_base<FieldT> {
public:
    virtual size_t piecewise_degree() const = 0;
    virtual std::shared_ptr<piecewise_polynomial_base<FieldT>> compose(
        const std::shared_ptr<polynomial_base<FieldT>> poly) const;
    // TODO: Add static method for figuring out composed degree.
};

} // namespace libiop

#endif // LIBIOP_ALGEBRA_POLYNOMIALS_PIECEWISE_POLYNOMIAL_HPP_
