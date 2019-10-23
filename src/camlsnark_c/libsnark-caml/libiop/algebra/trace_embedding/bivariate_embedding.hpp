/**@file
 *****************************************************************************
 Classes for Bivariate Embedding
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_ALGEBRA_TRACE_EMBEDDING_BIVARIATE_EMBEDDING_HPP_
#define LIBIOP_ALGEBRA_TRACE_EMBEDDING_BIVARIATE_EMBEDDING_HPP_

#include "libiop/algebra/exponentiation.hpp"
#include "libiop/algebra/field_subset/basis_utils.hpp"
#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/common/common.hpp"

namespace libiop {

template<typename FieldT>
class polynomial_composed_with_projection;

/** A bivariate embedding consists 3 field subsets H, H_1, H_2,
 *  and contains 2, f_1 : H -> H_1 of degree |H_2|, and f_2 : H -> H_2 of degree |H_1|
 *  such that f(h) = (f_1(h), f_2(h)) is a bijection between H and H_1 X H_2.
 *  H corresponds to a table, H_1 corresponds to rows, and H_2 corresponds to the columns.
 *  This bivariate embedding is efficient, it can be evaluated in O(log(|H|)) field operations,
 *  and constructed in O(log^2(|H|)) field operations.
 */
template<typename FieldT>
class bivariate_embedding {
protected:
    std::shared_ptr<polynomial_base<FieldT>> projection_into_row_;
    std::shared_ptr<polynomial_base<FieldT>> projection_into_col_;

    /** These may be off by a constant term.
     *  They are used for their associated k to 1 map methods. */
    vanishing_polynomial<FieldT> projection_into_row_vp_;
    vanishing_polynomial<FieldT> projection_into_col_vp_;
public:
    bivariate_embedding<FieldT>() {};
    bivariate_embedding<FieldT>(const field_subset<FieldT> &table_domain,
        const field_subset<FieldT> &row_domain, const field_subset<FieldT> &col_domain);
    FieldT project_to_row(const FieldT &x) const;
    FieldT project_to_col(const FieldT &x) const;

    std::shared_ptr<polynomial_base<FieldT>> polynomial_map_into_row_domain() const;
    std::shared_ptr<polynomial_base<FieldT>> polynomial_map_into_col_domain() const;

    std::shared_ptr<polynomial_base<FieldT>> compose_polynomial_with_row_projection(
        const std::shared_ptr<polynomial_base<FieldT>> &poly) const;
    std::shared_ptr<polynomial_base<FieldT>> compose_polynomial_with_col_projection(
        const std::shared_ptr<polynomial_base<FieldT>> &poly) const;
};

} // namespace libiop

#include "libiop/algebra/trace_embedding/bivariate_embedding.tcc"

#endif // LIBIOP_ALGEBRA_TRACE_EMBEDDING_BIVARIATE_EMBEDDING_HPP_
