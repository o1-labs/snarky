/** @file
 *****************************************************************************
 Interfaces for succinct and semi-succinct matrices.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef LIBIOP_RELATIONS_SUCCINCT_MATRIX_HPP_
#define LIBIOP_RELATIONS_SUCCINCT_MATRIX_HPP_

#include <cstddef>
#include <memory>

#include "libiop/relations/sparse_matrix.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"

namespace libiop {

template<typename FieldT>
class succinct_matrix {
public:
    explicit succinct_matrix() {};

    virtual size_t num_rows() const;
    virtual size_t num_columns() const;
    virtual std::shared_ptr<polynomial_base<FieldT>> extend_Mz(const std::shared_ptr<polynomial_base<FieldT>> &z) const;

    /** If z has a degree d polynomial extension,
     *  this function returns the degree of the efficient polynomial extension of Mz. */
    virtual size_t Mz_degree(const size_t z_degree) const;

    virtual ~succinct_matrix() = default;
};

/** A semi-succinct matrix M is a matrix which can be written as A tensor B,
 *  where A is a succinct matrix, and B is an unstructured (but small) matrix. */
template<typename FieldT>
class semisuccinct_matrix {
public:
    semisuccinct_matrix() = default;

    virtual std::shared_ptr<succinct_matrix<FieldT>> get_succinct_matrix() const;
    virtual std::shared_ptr<sparse_matrix<FieldT>> get_unstructured_matrix() const;

    virtual ~semisuccinct_matrix() = default;
};

template<typename FieldT>
class standard_semisuccinct_matrix : public semisuccinct_matrix<FieldT> {
protected:
    std::shared_ptr<succinct_matrix<FieldT>> succinct_matrix_;
    std::shared_ptr<sparse_matrix<FieldT>> sparse_matrix_;
public:
    standard_semisuccinct_matrix() = default;
    standard_semisuccinct_matrix(std::shared_ptr<succinct_matrix<FieldT>> succinct,
                                 std::shared_ptr<sparse_matrix<FieldT>> sparse);

    std::shared_ptr<succinct_matrix<FieldT>> get_succinct_matrix() const;
    std::shared_ptr<sparse_matrix<FieldT>> get_unstructured_matrix() const;
};

} // namespace libiop

#include "libiop/relations/succinct_matrix.tcc"

#endif // LIBIOP_RELATIONS_SUCCINCT_MATRIX_HPP_
