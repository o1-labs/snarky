/** @file
 *****************************************************************************
 Interfaces for sparse matrices together with adapters for R1CS A/B/C matrices.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef LIBIOP_RELATIONS_SPARSE_MATRIX_HPP_
#define LIBIOP_RELATIONS_SPARSE_MATRIX_HPP_

#include <cstddef>
#include <memory>

#include "libiop/relations/r1cs.hpp"
#include "libiop/relations/variable.hpp"

namespace libiop {

template<typename FieldT>
class sparse_matrix {
public:
    sparse_matrix() = default;

    virtual linear_combination<FieldT> get_row(const std::size_t row_index) const;
    virtual std::size_t num_rows() const;
    virtual std::size_t num_columns() const;
    virtual std::size_t num_nonzero_entries() const;

    virtual ~sparse_matrix() = default;
};

enum r1cs_sparse_matrix_type {
    r1cs_sparse_matrix_A = 1,
    r1cs_sparse_matrix_B = 2,
    r1cs_sparse_matrix_C = 3
};

std::vector<r1cs_sparse_matrix_type> all_r1cs_sparse_matrix_types(
    {r1cs_sparse_matrix_A, r1cs_sparse_matrix_B, r1cs_sparse_matrix_C});

template<typename FieldT>
class r1cs_sparse_matrix : public sparse_matrix<FieldT> {
protected:
    std::shared_ptr<r1cs_constraint_system<FieldT> > constraint_system_;
    r1cs_sparse_matrix_type matrix_type_;
public:
    r1cs_sparse_matrix(
        std::shared_ptr<r1cs_constraint_system<FieldT> > constraint_system,
        const r1cs_sparse_matrix_type matrix_type);

    virtual linear_combination<FieldT> get_row(const std::size_t row_index) const;
    virtual std::size_t num_rows() const;
    virtual std::size_t num_columns() const;
    virtual std::size_t num_nonzero_entries() const;
};

} // libiop

#include "libiop/relations/sparse_matrix.tcc"

#endif // LIBIOP_RELATIONS_SPARSE_MATRIX_HPP_
