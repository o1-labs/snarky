#include <cmath>
#include <stdexcept>

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/lagrange.hpp"
#include "libiop/common/profiling.hpp"

namespace libiop {

template<typename FieldT>
interleaved_lincheck_et_protocol<FieldT>::interleaved_lincheck_et_protocol(
    iop_protocol<FieldT> &IOP,
    const domain_handle &codeword_domain_handle,
    const domain_handle &systematic_domain_handle,
    const domain_handle &extended_systematic_domain_handle,
    const std::size_t num_oracles,
    const std::size_t num_queries,
    const std::size_t num_interactions,
    const bool make_zk,
    const field_subset_type domain_type,
    const naive_sparse_matrix<FieldT> constraint_matrix,
    const std::vector<FieldT> target_vector) :
    IOP_(IOP),
    codeword_domain_handle_(codeword_domain_handle),
    systematic_domain_handle_(systematic_domain_handle),
    extended_systematic_domain_handle_(extended_systematic_domain_handle),
    num_oracles_(num_oracles),
    num_queries_(num_queries),
    num_interactions_(num_interactions),
    make_zk_(make_zk),
    field_subset_type_(domain_type),
    constraint_matrix_(constraint_matrix),
    target_vector_(target_vector)
{
    this->codeword_domain_ = this->IOP_.get_domain(this->codeword_domain_handle_);
    this->codeword_domain_size_ = this->codeword_domain_.num_elements();

    this->systematic_domain_ = this->IOP_.get_domain(this->systematic_domain_handle_);
    this->systematic_domain_size_ = this->systematic_domain_.num_elements();

    this->extended_systematic_domain_ = this->IOP_.get_domain(this->extended_systematic_domain_handle_);
    this->extended_systematic_domain_size_ = this->extended_systematic_domain_.num_elements();

    this->response_size_ = 2 * this->systematic_domain_size_;
}

template<typename FieldT>
void interleaved_lincheck_et_protocol<FieldT>::attach_input_vector_row_oracles(
    const std::vector<oracle_handle_ptr> &handles)
{
    assert(handles.size() == this->num_oracles_);

    this->input_vector_row_oracle_handles_ = handles;
}

template<typename FieldT>
void interleaved_lincheck_et_protocol<FieldT>::attach_blinding_vector_row_oracles(const std::vector<oracle_handle_ptr> &handles)
{
    assert(handles.size() == this->num_interactions_);

    this->blinding_vector_row_oracle_handles_ = handles;
}

template<typename FieldT>
void interleaved_lincheck_et_protocol<FieldT>::register_linear_combinations()
{
    this->random_linear_combination_handles_.resize(this->num_interactions_);
    for (size_t i = 0; i < this->num_interactions_; ++i)
    {
        this->random_linear_combination_handles_[i] = this->IOP_.register_verifier_random_message(this->constraint_matrix_.size());
    }
}

template<typename FieldT>
void interleaved_lincheck_et_protocol<FieldT>::register_responses()
{
    this->response_handles_.resize(this->num_interactions_);
    for (size_t i = 0; i < this->num_interactions_; ++i)
    {
         this->response_handles_[i] = this->IOP_.register_prover_message(this->response_size_);
    }
}

template<typename FieldT>
void interleaved_lincheck_et_protocol<FieldT>::register_queries()
{
    std::vector<random_query_position_handle> query_position_handles;
    query_position_handles.resize(this->num_queries_);

    for (size_t i = 0; i < this->num_queries_; ++i)
    {
        query_position_handles[i] = this->IOP_.register_random_query_position(this->codeword_domain_handle_);
    }

    this->register_queries_for_given_positions(query_position_handles);
}

template<typename FieldT>
void interleaved_lincheck_et_protocol<FieldT>::register_queries_for_given_positions(
    std::vector<random_query_position_handle> query_position_handles)
{
    this->query_position_handles_ = std::move(query_position_handles);

    this->input_vector_row_oracles_by_position_handles_.resize(this->num_queries_);
    if (this->make_zk_)
    {
        this->blinding_vector_row_oracles_by_position_handles_.resize(this->num_queries_);
    }

    for (size_t i = 0; i < this->num_queries_; ++i)
    {
        std::vector<query_handle> input_new_list;
        this->input_vector_row_oracles_by_position_handles_[i] = input_new_list;
        this->input_vector_row_oracles_by_position_handles_[i].resize(this->num_oracles_);

        for (size_t j = 0; j < this->num_oracles_; ++j)
        {
            this->input_vector_row_oracles_by_position_handles_[i][j] =
                this->IOP_.register_query(this->input_vector_row_oracle_handles_[j], this->query_position_handles_[i]);
        }

        if (this->make_zk_)
        {
            std::vector<query_handle> blinding_new_list;
            this->blinding_vector_row_oracles_by_position_handles_[i] = blinding_new_list;
            this->blinding_vector_row_oracles_by_position_handles_[i].resize(this->num_interactions_);

            for (size_t j = 0; j < this->num_interactions_; ++j)
            {
                this->blinding_vector_row_oracles_by_position_handles_[i][j] =
                    this->IOP_.register_query(this->blinding_vector_row_oracle_handles_[j], this->query_position_handles_[i]);
            }
        }
    }
}

/* Proving */
template<typename FieldT>
void interleaved_lincheck_et_protocol<FieldT>::calculate_and_submit_responses()
{
    /* The prover computes the evaluations of the polynomial over the entire codeword domain, based
       on the oracle evaluations. (If the claim is true, this should match the explicitly computed
       dot product of the random linear combination and the target vector, in the equality test.)
       Then the prover interpolates the polynomial's coefficients, and sends them. */

    for (size_t i = 0; i < this->num_interactions_; ++i)
    {
        const std::vector<FieldT> random_linear_combination =
            this->IOP_.obtain_verifier_random_message(this->random_linear_combination_handles_[i]);

        std::vector<FieldT> evals_of_response_poly(this->codeword_domain_size_, FieldT(0));

        /** Multiply constraint matrix by random values, to build a
         * vector of s_i evaluations */
        std::vector<FieldT> row_vector(this->num_oracles_ * this->systematic_domain_size_, FieldT(0));
        for (size_t j = 0; j < this->constraint_matrix_.size(); ++j)
        {
            std::map<std::size_t, FieldT> row = this->constraint_matrix_[j];
            typename std::map<std::size_t, FieldT>::iterator it;
            for (it = row.begin(); it != row.end(); it++)
            {
                const std::size_t idx = it->first;
                const FieldT val = it->second;
                row_vector[idx] += random_linear_combination[j] * val;
            }
        }

        for (size_t j = 0; j < this->num_oracles_; ++j)
        {
            const std::size_t start = this->systematic_domain_size_ * j;
            const std::size_t end = this->systematic_domain_size_ + start;

            const typename std::vector<FieldT>::const_iterator row_first = row_vector.begin() + start;
            const typename std::vector<FieldT>::const_iterator row_last = row_vector.begin() + end;
            const std::vector<FieldT> current_vector(row_first, row_last);

            const std::vector<FieldT> poly_coefficients =
                IFFT_over_field_subset<FieldT>(current_vector, this->systematic_domain_);
            const std::vector<FieldT> current_evaluations =
                FFT_over_field_subset<FieldT>(poly_coefficients, this->codeword_domain_);

            const std::shared_ptr<std::vector<FieldT>> row_evaluations =
                this->IOP_.get_oracle_evaluations(this->input_vector_row_oracle_handles_[j]);

            for (size_t a = 0; a < this->codeword_domain_size_; ++a)
            {
                evals_of_response_poly[a] += current_evaluations[a] * row_evaluations->operator[](a);
            }
        }

        if (this->make_zk_)
        {
            const std::shared_ptr<std::vector<FieldT>> blinding_vec =
                this->IOP_.get_oracle_evaluations(this->blinding_vector_row_oracle_handles_[i]);
            for (size_t a = 0; a < this->codeword_domain_size_; ++a)
            {
                evals_of_response_poly[a] += blinding_vec->operator[](a);
            }
        }

        const std::vector<FieldT> response_poly_coefficients =
            IFFT_over_field_subset<FieldT>(evals_of_response_poly, this->codeword_domain_);
        std::vector<FieldT> reduced_coefficients(&response_poly_coefficients[0],
                                                 &response_poly_coefficients[this->response_size_]);

        this->IOP_.submit_prover_message(this->response_handles_[i], std::move(reduced_coefficients));
    }
}

/* Verification */
template<typename FieldT>
bool interleaved_lincheck_et_protocol<FieldT>::verifier_predicate()
{
    const std::vector<FieldT> codeword_elements = this->codeword_domain_.all_elements(); // eta

    for (size_t h = 0; h < this->num_interactions_; ++h)
    {
        const std::vector<FieldT> random_linear_combination =
            this->IOP_.obtain_verifier_random_message(this->random_linear_combination_handles_[h]);

        /* EQUALITY TEST: does the polynomial that was sent sum to the value that it should if the
           claimed statement is true? */

        enter_block("Lincheck: equality test (polynomial matches randomized vector)");
        std::vector<FieldT> response = this->IOP_.receive_prover_message(this->response_handles_[h]); // coefficients of p_0
        const std::vector<FieldT> evaluations = FFT_over_field_subset<FieldT>(response, this->extended_systematic_domain_);
        const polynomial<FieldT> response_poly(std::move(response));

        FieldT equality_lhs(0);
        for (size_t d = 0; d < this->systematic_domain_size_; ++d)
        {
            const std::size_t idx = this->extended_systematic_domain_.reindex_by_subset(
                this->systematic_domain_.dimension(), d);
            equality_lhs += evaluations[idx];
        }

        /* s_i doesn't have to be calculated, since the target vector is already known. */
        FieldT equality_rhs(0);
        for (size_t i = 0; i < this->num_oracles_; ++i)
        {
            for (size_t d = 0; d < this->systematic_domain_size_; ++d)
            {
                const std::size_t i_d = i * this->systematic_domain_size_ + d;
                equality_rhs += random_linear_combination[i_d] * this->target_vector_[i_d];
            }
        }

        if (equality_lhs != equality_rhs)
        {
#ifdef DEBUG
            print_indent();
            printf("[LHS] sent polynomial value sum_{d in [l]} q(zeta_d) = ");
            equality_lhs.print();
            printf("\n");

            print_indent();
            printf("[RHS] randomized target vector sum_{i in [m], d in [l]} r_id b_id = ");
            equality_rhs.print();
            printf("\n");
#endif // DEBUG
            return false;
        }
        leave_block("Lincheck: equality test (polynomial matches randomized vector)");

        /* Preparation for consistency test. */

        std::vector<polynomial<FieldT>> randomized_matrix_row_polys;

        /* Multiply matrix by random values. */
        std::vector<FieldT> randomized_constraint_matrix(this->num_oracles_ * this->systematic_domain_size_, FieldT(0));
        for (size_t j = 0; j < this->constraint_matrix_.size(); ++j)
        {
            std::map<std::size_t, FieldT> row = this->constraint_matrix_[j];
            typename std::map<std::size_t, FieldT>::iterator it;
            for (it = row.begin(); it != row.end(); it++)
            {
                const std::size_t idx = it->first;
                const FieldT val = it->second;
                randomized_constraint_matrix[idx] += random_linear_combination[j] * val;
            }
        }

        /* Split vector into rows over the systematic domain, to interpolate into polynomials (for
           the consistency test). */
        for (size_t i = 0; i < this->num_oracles_; ++i)
        {
            const std::size_t start = this->systematic_domain_size_ * i;
            const std::size_t end = this->systematic_domain_size_ + start;

            const typename std::vector<FieldT>::const_iterator vec_first = randomized_constraint_matrix.begin() + start;
            const typename std::vector<FieldT>::const_iterator vec_last = randomized_constraint_matrix.begin() + end;
            const std::vector<FieldT> current_vector(vec_first, vec_last);

            std::vector<FieldT> coefficients =
                IFFT_over_field_subset<FieldT>(current_vector, this->systematic_domain_);
            polynomial<FieldT> poly(std::move(coefficients));
            randomized_matrix_row_polys.emplace_back(poly);
        }

        /* CONSISTENCY TEST: do the polynomial's values match those of the oracles? */

        enter_block("Lincheck: querying and performing consistency tests");
        for (size_t k = 0; k < this->num_queries_; ++k)
        {
            const random_query_position_handle this_position_handle = this->query_position_handles_[k];
            const std::size_t j = this->IOP_.obtain_query_position(this_position_handle);

            FieldT consistency_test_lhs(0);
            for (size_t i = 0; i < this->num_oracles_; ++i)
            {
                const FieldT this_value = this->IOP_.obtain_query_response(this->input_vector_row_oracles_by_position_handles_[k][i]);
                consistency_test_lhs += randomized_matrix_row_polys[i].evaluation_at_point(codeword_elements[j]) * this_value;
            }
            if (this->make_zk_)
            {
                consistency_test_lhs += this->IOP_.obtain_query_response(this->blinding_vector_row_oracles_by_position_handles_[k][h]);
            }
            const FieldT consistency_test_rhs = response_poly.evaluation_at_point(codeword_elements[j]);

            if (consistency_test_lhs != consistency_test_rhs)
            {
#ifdef DEBUG
                print_indent(); printf("For interactive repetition h = %zu and query position = %zu,\n", h, j);

                print_indent(); print_indent();
                printf("[LHS] value from oracles sum_{i in [m_1]} s_i,h(eta_j) * U^x_i,j + u_h'[h] = ");
                consistency_test_lhs.print();
                printf("\n");

                print_indent(); print_indent();
                printf("[RHS] polynomial value p_0(eta_j) = ");
                consistency_test_rhs.print();
                printf("\n");
#endif // DEBUG
                return false;
            }
        }
        leave_block("Lincheck: querying and performing consistency tests");
    }

    return true;
}

} // namespace libiop
