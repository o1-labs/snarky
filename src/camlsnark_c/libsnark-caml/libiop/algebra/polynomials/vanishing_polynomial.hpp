/**@file
 *****************************************************************************
 Classes for vanishing polynomials for field subsets.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_ALGEBRA_POLYNOMIALS_VANISHING_POLYNOMIAL_HPP_
#define LIBIOP_ALGEBRA_POLYNOMIALS_VANISHING_POLYNOMIAL_HPP_

#include <cstddef>
#include <vector>

#include "libiop/algebra/exponentiation.hpp"
#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/algebra/field_subset/subgroup.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/linearized_polynomial.hpp"

namespace libiop {

/** A vanishing polynomial for a subset H in F is denoted as Z_H.
 *  It is the unique monic polynomial of degree |H| such that
 *      for all x in H, Z_H(x) = 0
 *  This class implements vanishing polynomials for subspaces and subgroups,
 *  as in these settings the polynomial can be constructed in polylog(|H|),
 *  and evaluated in log(|H|). */
template<typename FieldT>
class vanishing_polynomial : public polynomial_base<FieldT> {
private:
    field_subset_type type_;
    std::size_t vp_degree_;
    // subspace type
    linearized_polynomial<FieldT> linearized_polynomial_;
    // multiplicative coset type
    FieldT vp_offset_; /* offset^|H| for cosets, 1 for subgroups */

public:
    explicit vanishing_polynomial() {};
    explicit vanishing_polynomial(const field_subset<FieldT> &S);
    explicit vanishing_polynomial(const affine_subspace<FieldT> &S);
    explicit vanishing_polynomial(const multiplicative_coset<FieldT> &S);

    FieldT evaluation_at_point(const FieldT &evalpoint) const;
    std::vector<FieldT> evaluations_over_field_subset(const field_subset<FieldT> &S) const;
    std::vector<FieldT> evaluations_over_subspace(const affine_subspace<FieldT> &S) const;
    std::vector<FieldT> evaluations_over_coset(const multiplicative_coset<FieldT> &S) const;

    /** As the vanishing polynomial is often a k->1 map,
     *  this returns the unique |S|/k evaluations. */
    std::vector<FieldT> unique_evaluations_over_field_subset(const field_subset<FieldT> &S) const;

    FieldT formal_derivative_at_point(const FieldT &evalpoint) const;

    polynomial<FieldT> operator*(const polynomial<FieldT> &p) const;

    std::size_t degree() const;
    FieldT constant_coefficient() const;
    std::shared_ptr<polynomial_base<FieldT>> associated_k_to_1_map();
    field_subset<FieldT> associated_k_to_1_map_at_domain(const field_subset<FieldT> domain) const;

    linearized_polynomial<FieldT> get_linearized_polynomial() const;
    polynomial<FieldT> get_polynomial() const;
    field_subset_type type() const;
};

// Returns the quotient and remainder of f / Z
template<typename FieldT>
std::pair<polynomial<FieldT>,
          polynomial<FieldT> >
polynomial_over_vanishing_polynomial(const polynomial<FieldT> &f,
                                     const vanishing_polynomial<FieldT> &Z);

template<typename FieldT>
linearized_polynomial<FieldT> vanishing_polynomial_from_subspace(const affine_subspace<FieldT> &S);

} // namespace libiop

#include "libiop/algebra/polynomials/vanishing_polynomial.tcc"

#endif // LIBIOP_ALGEBRA_POLYNOMIALS_VANISHING_POLYNOMIAL_HPP_
