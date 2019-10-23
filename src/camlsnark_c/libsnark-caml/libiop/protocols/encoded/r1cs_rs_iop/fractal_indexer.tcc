#include <memory>
#include <stdexcept>

#include "libiop/algebra/exponentiation.hpp"
#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/utils.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/common/profiling.hpp"

namespace libiop {

/* Initialize domains, domain sizes, and the degree of g and h */
template<typename FieldT>
matrix_indexer<FieldT>::matrix_indexer(
    iop_protocol<FieldT> &IOP,
    const domain_handle &index_domain_handle,
    const domain_handle &matrix_domain_handle,
    const domain_handle &codeword_domain_handle,
    const size_t input_variable_dim,
    const std::shared_ptr<sparse_matrix<FieldT>> matrix) :
    IOP_(IOP),
    index_domain_handle_(index_domain_handle),
    matrix_domain_handle_(matrix_domain_handle),
    codeword_domain_handle_(codeword_domain_handle),
    input_variable_dim_(input_variable_dim),
    matrix_(matrix)
{
    this->index_domain_ = this->IOP_.get_domain(this->index_domain_handle_);
    this->matrix_domain_ = this->IOP_.get_domain(this->matrix_domain_handle_);
    this->codeword_domain_ = this->IOP_.get_domain(this->codeword_domain_handle_);
}

template<typename FieldT>
void matrix_indexer<FieldT>::register_oracles()
{
    const size_t oracle_degree_bound = this->index_domain_.num_elements();
    assert(oracle_degree_bound >= this->matrix_->num_nonzero_entries());

    this->row_oracle_handle_ =
        this->IOP_.register_index_oracle(
            this->codeword_domain_handle_, oracle_degree_bound);
    this->col_oracle_handle_ =
        this->IOP_.register_index_oracle(
            this->codeword_domain_handle_, oracle_degree_bound);
    this->val_oracle_handle_ =
        this->IOP_.register_index_oracle(
            this->codeword_domain_handle_, oracle_degree_bound);
    // this->row_times_col_oracle_handle_ =
    //     this->IOP_.register_index_oracle(
    //         this->codeword_domain_handle_, oracle_degree_bound);
}

template<typename FieldT>
std::vector<std::vector<FieldT>> matrix_indexer<FieldT>::compute_oracles_over_K()
{
    /** Given a matrix M, this produces an index for M',
     *  where M'_ij = M_ji u_H(j, j)
     *
     *  The index for a matrix S is 3 univariate polynomials row, col, val,
     *  each of degree |K|, where K is a smooth domain such that |K| >= |S|.
     *
     *  The definitions of row, col, val have the property that:
     *  S(x, y) = sum_{k in K} val(k) * u_H(row(k), x) * u_H(col(k), y)
     *
     *
     *  Since we are indexing M', we index row, col for M,
     *  and we swap row and col to get the corresponding polynomials for M'.
     *  val_M'(k) = val_M(k) * u_H(row(k), row(k))
     *            = M_{row(k), col(k)} / (u_H(col(k), col(k)))
     *  So we index val_M'(k) directly */
    this->bivariate_lagrange_poly_ =
        bivariate_lagrange_polynomial<FieldT>(this->matrix_domain_);
    /** evaluations over K */
    std::vector<FieldT> row_evals;
    std::vector<FieldT> col_evals;
    std::vector<FieldT> val_evals;
    // std::vector<FieldT> row_times_col_evals;
    row_evals.reserve(this->index_domain_.num_elements());
    col_evals.reserve(this->index_domain_.num_elements());
    val_evals.reserve(this->index_domain_.num_elements());
    // row_times_col_evals.reserve(this->index_domain_.num_elements());

    size_t k_index = 0;
    for (size_t i = 0; i < this->matrix_->num_rows(); i++)
    {
        const linear_combination<FieldT> row = this->matrix_->get_row(i);
        const FieldT row_index_elem = this->matrix_domain_.element_by_index(i);

        for (auto &term : row.terms)
        {
            row_evals.emplace_back(row_index_elem);
            const size_t col_index =
                this->matrix_domain_.reindex_by_subset(this->input_variable_dim_, term.index_);
            const FieldT col_index_elem = this->matrix_domain_.element_by_index(col_index);
            col_evals.emplace_back(col_index_elem);
            // row_times_col_evals.emplace_back(row_index_elem * col_index_elem);

            const FieldT col_derivative =
                this->bivariate_lagrange_poly_.evaluation_at_point(col_index_elem, col_index_elem);
            const FieldT val_eval = term.coeff_ * col_derivative.inverse();
            val_evals.emplace_back(val_eval);
        }
    }
    /* We are dealing with the transpose */
    row_evals.swap(col_evals);
    row_evals.resize(this->index_domain_.num_elements(),
                     this->index_domain_.element_by_index(0));
    col_evals.resize(this->index_domain_.num_elements(),
                     this->index_domain_.element_by_index(0));
    val_evals.resize(this->index_domain_.num_elements(), FieldT::zero());
    return {row_evals, col_evals, val_evals};
}

template<typename FieldT>
void matrix_indexer<FieldT>::compute_oracles()
{
    std::vector<std::vector<FieldT>> index_oracles_over_K = this->compute_oracles_over_K();
    /** TODO: Handle domain conversion in another function
     *  to reduce memory overhead and code duplication.*/
    const std::vector<FieldT> row_poly_over_codeword_domain
        = FFT_over_field_subset<FieldT>(
            IFFT_over_field_subset<FieldT>(
                index_oracles_over_K[0], this->index_domain_),
            this->codeword_domain_);
    this->IOP_.submit_oracle(this->row_oracle_handle_, row_poly_over_codeword_domain);

    const std::vector<FieldT> col_poly_over_codeword_domain
        = FFT_over_field_subset<FieldT>(
            IFFT_over_field_subset<FieldT>(
                index_oracles_over_K[1], this->index_domain_),
            this->codeword_domain_);
    this->IOP_.submit_oracle(this->col_oracle_handle_, col_poly_over_codeword_domain);

    // row_times_col_evals.resize(this->index_domain_.num_elements(), FieldT::zero());
    // const std::vector<FieldT> row_times_col_poly_over_codeword_domain
    //     = FFT_over_field_subset<FieldT>(
    //         IFFT_over_field_subset<FieldT>(
    //             row_times_col_evals, this->index_domain_),
    //         this->codeword_domain_);
    // this->IOP_.submit_oracle(this->row_times_col_oracle_handle_, row_times_col_poly_over_codeword_domain);

    const std::vector<FieldT> val_poly_over_codeword_domain
        = FFT_over_field_subset<FieldT>(
            IFFT_over_field_subset<FieldT>(
                index_oracles_over_K[2], this->index_domain_),
            this->codeword_domain_);
    this->IOP_.submit_oracle(this->val_oracle_handle_, val_poly_over_codeword_domain);
}

template<typename FieldT>
oracle_handle matrix_indexer<FieldT>::get_row_oracle_handle() const
{
    return this->row_oracle_handle_;
}

template<typename FieldT>
oracle_handle matrix_indexer<FieldT>::get_col_oracle_handle() const
{
    return this->col_oracle_handle_;
}

template<typename FieldT>
oracle_handle matrix_indexer<FieldT>::get_val_oracle_handle() const
{
    return this->val_oracle_handle_;
}

template<typename FieldT>
oracle_handle matrix_indexer<FieldT>::get_row_times_col_oracle_handle() const
{
    return this->row_times_col_oracle_handle_;
}

template<typename FieldT>
std::vector<oracle_handle_ptr> matrix_indexer<FieldT>::get_all_oracle_handles() const
{
    std::vector<oracle_handle_ptr> result;
    result.emplace_back(std::make_shared<oracle_handle>(this->get_row_oracle_handle()));
    result.emplace_back(std::make_shared<oracle_handle>(this->get_col_oracle_handle()));
    result.emplace_back(std::make_shared<oracle_handle>(this->get_val_oracle_handle()));
    // result.emplace_back(std::make_shared<oracle_handle>(this->get_row_times_col_oracle_handle()));
    return result;
}

} // namespace libiop
