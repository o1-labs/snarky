/**@file
*****************************************************************************
Fractal Indexer interfaces.
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_ENCODED_AURORA_FRACTAL_INDEXER_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_AURORA_FRACTAL_INDEXER_HPP_

#include <cstddef>
#include <memory>
#include <stdexcept>
#include <vector>

#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/bivariate_lagrange_polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/protocols/encoded/common/random_linear_combination.hpp"
#include "libiop/relations/sparse_matrix.hpp"

namespace libiop {

/** This produces a the row, col, and val polynomials as described in Holographic Aurora,
 *  for a given matrix.
 *
 *  It is given a domain K, such that |K| > the number of non-zero terms in the matrix.
 *  The row, col, and val polynomials are (roughly) such that forall k in |K|:
 *    * row(K[k]) = row-index of the k-th non-zero element
 *    * col(K[k]) = column-index of the k-th non-zero element
 *    * val(K[k]) = value of the k-th non-zero element
 *
 *  We asssume the non-zero elements of the matrix are ordered in some manner,
 *  we currently order them row-wise for simplicity. */
template<typename FieldT>
class matrix_indexer {
protected:
    iop_protocol<FieldT> &IOP_;
    domain_handle index_domain_handle_;  /* In Holographic Aurora this is referred to as K */
    domain_handle matrix_domain_handle_; /* In Holographic Aurora this is referred to as H */
    domain_handle codeword_domain_handle_; /* In Holographic Aurora this is referred to as L */
    size_t input_variable_dim_;
    std::shared_ptr<sparse_matrix<FieldT>> matrix_;

    field_subset<FieldT> index_domain_;
    field_subset<FieldT> matrix_domain_;
    field_subset<FieldT> codeword_domain_;

    oracle_handle row_oracle_handle_;
    oracle_handle col_oracle_handle_;
    oracle_handle val_oracle_handle_;
    oracle_handle row_times_col_oracle_handle_;

    bivariate_lagrange_polynomial<FieldT> bivariate_lagrange_poly_;
public:
    /* Initialization and registration */
    matrix_indexer(iop_protocol<FieldT> &IOP,
                   const domain_handle &index_domain,
                   const domain_handle &matrix_domain,
                   const domain_handle &codeword_domain,
                   const size_t input_variable_dim,
                   const std::shared_ptr<sparse_matrix<FieldT>> matrix);

    void register_oracles();

    oracle_handle get_row_oracle_handle() const;
    oracle_handle get_col_oracle_handle() const;
    oracle_handle get_val_oracle_handle() const;
    oracle_handle get_row_times_col_oracle_handle() const;

    /* Ran in the indexer */
    std::vector<std::vector<FieldT>> compute_oracles_over_K();
    void compute_oracles();

    std::vector<oracle_handle_ptr> get_all_oracle_handles() const;
};

} // namespace libiop

#include "libiop/protocols/encoded/r1cs_rs_iop/fractal_indexer.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_AURORA_FRACTAL_INDEXER_HPP_
