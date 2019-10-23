/**@file
 *****************************************************************************
 Class for succinct lagrange polynomials.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_ALGEBRA_POLYNOMIALS_LAGRANGE_POLYNOMIAL_HPP_
#define LIBIOP_ALGEBRA_POLYNOMIALS_LAGRANGE_POLYNOMIAL_HPP_

#include <cstddef>
#include <vector>

#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"

namespace libiop {

/** TODO: Update this description for normalization being default.
 *  Lagrange polynomials in this codebase refer to succinct polynomials that are low degree extensions
 *  of the unnormalized lagrange basis, evaluated at a given position.
 *
 *  The unnormalized lagrange polynomial basis for a domain S is the set:
 *      { Z_S(X) / (X - s) : s \in S }
 *  The low degree extension of the above is the bivariate polynomial:
 *      f(X, Y) = (Z_S(X) - Z_S(Y)) / (X - Y)
 *  This low degree extension identifies elements in S with Y.
 *  We can see that f is degree |S| - 1 in Y,
 *  as (X - Y) divides the numerator from definition of vanishing polynomials.
 *
 *  The lagrange polynomial implemented here is the evaluation of f in the x variable
 *  at the prescribed point `a`. In particular the lagrange polynomial is:
 *      f(a, Y) = (Z_S(a) - Z_S(Y)) / (a - Y)
 *
 *  A flag is also included to create an unnormalized lagrange basis.
 */
template<typename FieldT>
class lagrange_polynomial : public polynomial_base<FieldT> {
protected:
    FieldT x_;
    field_subset<FieldT> S_;
    vanishing_polynomial<FieldT> Z_S_;
    FieldT Z_S_at_x_;
    bool is_normalized_;
    FieldT normalization_coefficient_;
public:
    explicit lagrange_polynomial() {};
    explicit lagrange_polynomial(const FieldT x_evaluation,
                                 const field_subset<FieldT> &S);
    explicit lagrange_polynomial(const FieldT x_evaluation,
                                 const field_subset<FieldT> &S,
                                 const bool is_normalized);

    FieldT evaluation_at_point(const FieldT &evalpoint) const;
    std::vector<FieldT> evaluations_over_field_subset(const field_subset<FieldT> &S) const;

    polynomial<FieldT> expand_as_polynomial() const;

    size_t degree() const;
};

} // namespace libiop

#include "libiop/algebra/polynomials/lagrange_polynomial.tcc"

#endif // LIBIOP_ALGEBRA_POLYNOMIALS_LAGRANGE_POLYNOMIAL_HPP_
