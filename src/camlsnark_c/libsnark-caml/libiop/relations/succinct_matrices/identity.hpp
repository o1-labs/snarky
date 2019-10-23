/**@file
 *****************************************************************************
 Implementations of Identity and Shifted Identity succinct matrices.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_RELATIONS_SUCCINCT_MATRICES_IDENTITY_HPP_
#define LIBIOP_RELATIONS_SUCCINCT_MATRICES_IDENTITY_HPP_

#include "libiop/relations/succinct_matrix.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/trace_embedding/successor_ordering.hpp"

namespace libiop {

/** The identity matrix over FieldT, with the specified number of rows. */
template<typename FieldT>
class identity_matrix : public succinct_matrix<FieldT> {
protected:
    size_t num_rows_;
public:
    explicit identity_matrix() {};
    explicit identity_matrix(size_t num_rows);

    size_t num_rows() const;
    size_t num_columns() const;
    std::shared_ptr<polynomial_base<FieldT>> extend_Mz(const std::shared_ptr<polynomial_base<FieldT>> &z) const;

    /** If z has a degree d polynomial extension,
     *  this function returns the degree of the efficient polynomial extension of Mz. */
    size_t Mz_degree(const size_t z_degree) const;
};

/** The shifted identity matrix over FieldT.
 *  Given a field subset of size S, which has a successor ordering, this constructs a matrix in F^{S x S},
 *  such that there is a one on the super diagnol. As an example, the 3x3 shifted identity matrix is:
 *  0 1 0
 *  0 0 1
 *  0 0 0 */
template<typename FieldT>
class shifted_identity_matrix : public succinct_matrix<FieldT> {
protected:
    field_subset<FieldT> S_;
    successor_ordering<FieldT> successor_ordering_;
public:
    explicit shifted_identity_matrix() {};
    explicit shifted_identity_matrix(const field_subset<FieldT> &S,
                                     const successor_ordering<FieldT> &ordering);

    size_t num_rows() const;
    size_t num_columns() const;
    std::shared_ptr<polynomial_base<FieldT>> extend_Mz(const std::shared_ptr<polynomial_base<FieldT>> &z) const;

    /** If z has a degree d polynomial extension,
     *  this function returns the degree of the efficient polynomial extension of Mz. */
    size_t Mz_degree(const size_t z_degree) const;
};

} // namespace libiop

#include "libiop/relations/succinct_matrices/identity.tcc"

#endif // LIBIOP_RELATIONS_SUCCINCT_MATRICES_IDENTITY_HPP_
