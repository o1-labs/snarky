/**@file
 *****************************************************************************
 Classes for Successor Ordering
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_ALGEBRA_TRACE_EMBEDDING_SUCCESSOR_ORDERING_HPP_
#define LIBIOP_ALGEBRA_TRACE_EMBEDDING_SUCCESSOR_ORDERING_HPP_

#include "libiop/algebra/exponentiation.hpp"
#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/algebra/polynomials/lagrange_polynomial.hpp"
#include "libiop/algebra/polynomials/piecewise_polynomial.hpp"

namespace libiop {

/** A successor ordering is an ordering of a set S contains a designated first element of the field,
 *  and a piecewise polynomial f which induces an ordering on the set by mapping every point in the set
 *  to its "successor".
 *  The ordering induces a bijection from Z_{|S|} to S, such that:
 *      0 -> first element,
 *      1 -> f(first element),
 *      2 -> f(f(first element)),
 *  etc.
 *  The piecewise polynomial has the additional constraint that it must have piecewise degree 1.
 */
template<typename FieldT>
class successor_ordering_base {
public:
    virtual FieldT first_elem() const;
    virtual FieldT next_elem(const FieldT &cur_elem) const;

    virtual std::shared_ptr<piecewise_polynomial_base<FieldT>> piecewise_polynomial() const;
};

/** Successor polynomial for any multiplicative coset. */
template<typename FieldT>
class multiplicative_successor_polynomial : public piecewise_polynomial_base<FieldT> {
protected:
    FieldT generator_;
    bool composed_;
    std::shared_ptr<polynomial_base<FieldT>> composed_poly_;

    /** Internal constructor for composition */
    multiplicative_successor_polynomial<FieldT>(const FieldT &generator,
        const std::shared_ptr<polynomial_base<FieldT>> composed_poly);
public:
    multiplicative_successor_polynomial<FieldT>() {};
    multiplicative_successor_polynomial<FieldT>(const FieldT &generator);
    FieldT evaluation_at_point(const FieldT &eval_point) const;
    std::vector<FieldT> evaluations_over_field_subset(const field_subset<FieldT> &S) const;
    polynomial<FieldT> expand_as_polynomial() const;

    size_t degree() const;
    size_t piecewise_degree() const;

    std::shared_ptr<piecewise_polynomial_base<FieldT>> compose(
        const std::shared_ptr<polynomial_base<FieldT>> poly) const;
};

template<typename FieldT>
class multiplicative_successor_ordering : public successor_ordering_base<FieldT> {
protected:
    field_subset<FieldT> domain_;
    multiplicative_successor_polynomial<FieldT> successor_polynomial_;
public:
    multiplicative_successor_ordering<FieldT>() {};
    multiplicative_successor_ordering<FieldT>(const field_subset<FieldT> &domain);
    FieldT first_elem() const;
    FieldT next_elem(const FieldT &cur_elem) const;

    std::shared_ptr<piecewise_polynomial_base<FieldT>> piecewise_polynomial() const;
};

/** Successor polynomial for additive subspaces of F_2^n which have a standard basis.
 *
 *  If the need arises in future protocols, a separate evaluation path can be created for
 *  evaluations within the systematic domain, as that can be done much more efficiently.
 *
 *  This implements the additive successor polynomial described within [BCGGRS19] (Succinct Aurora)
 */
template<typename FieldT>
class additive_successor_polynomial : public piecewise_polynomial_base<FieldT> {
protected:
    affine_subspace<FieldT> subspace_;
    /** Primitive polynomial of identified smaller field, evaluated at generator of multiplicative group */
    FieldT primitive_polynomial_at_multiplicative_generator_;
    FieldT multiplicative_generator_;
    /** Z_S_truncated = Z_S' in the paper, it refers to the subspace with the last basis vector omitted. */
    vanishing_polynomial<FieldT> Z_S_truncated_;
    FieldT Z_S_truncated_at_multiplicative_generator_to_i_minus_one_;
    lagrange_polynomial<FieldT> lagrange_indicator_polynomial_;
    FieldT L_0_coefficient_;
    FieldT L_1_coefficient_;
public:
    additive_successor_polynomial<FieldT>() {};
    additive_successor_polynomial<FieldT>(const affine_subspace<FieldT> &S);
    FieldT evaluation_at_point(const FieldT &eval_point) const;

    std::vector<FieldT> evaluations_over_field_subset(const field_subset<FieldT> &U) const;
    polynomial<FieldT> expand_as_polynomial() const;

    size_t degree() const;
    size_t piecewise_degree() const;

    std::shared_ptr<piecewise_polynomial_base<FieldT>> compose(
        const std::shared_ptr<polynomial_base<FieldT>> poly) const;
};

template<typename FieldT>
class additive_successor_ordering : public successor_ordering_base<FieldT> {
protected:
    affine_subspace<FieldT> subspace_;
    additive_successor_polynomial<FieldT> successor_polynomial_;
public:
    additive_successor_ordering<FieldT>() {};
    additive_successor_ordering<FieldT>(const field_subset<FieldT> &domain);
    FieldT first_elem() const;
    FieldT next_elem(const FieldT &cur_elem) const;

    std::shared_ptr<piecewise_polynomial_base<FieldT>> piecewise_polynomial() const;
};

template<typename FieldT>
class successor_ordering : public successor_ordering_base<FieldT> {
protected:
    field_subset_type field_subset_type_;
    multiplicative_successor_ordering<FieldT> multiplicative_ordering_;
    additive_successor_ordering<FieldT> additive_ordering_;
public:
    successor_ordering<FieldT>(const field_subset<FieldT> &domain);
    FieldT first_elem() const;
    FieldT next_elem(const FieldT &cur_elem) const;

    std::shared_ptr<piecewise_polynomial_base<FieldT>> piecewise_polynomial() const;
};

} // namespace libiop

#include "libiop/algebra/trace_embedding/multiplicative_successor_ordering.tcc"
#include "libiop/algebra/trace_embedding/additive_successor_ordering.tcc"
#include "libiop/algebra/trace_embedding/wrapper_successor_ordering.tcc"

#endif // LIBIOP_ALGEBRA_TRACE_EMBEDDING_SUCCESSOR_ORDERING_HPP_
