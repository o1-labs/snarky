#include <stdexcept>

#include "libiop/algebra/fft.hpp"
#include "libiop/common/profiling.hpp"

namespace libiop {

template<typename FieldT>
interleaved_rowcheck_protocol<FieldT>::interleaved_rowcheck_protocol(
    iop_protocol<FieldT> &IOP,
    const domain_handle &codeword_domain_handle,
    const domain_handle &systematic_domain_handle,
    const domain_handle &extended_systematic_domain_handle,
    const std::size_t num_oracles,
    const std::size_t num_queries,
    const std::size_t num_interactions,
    const bool make_zk,
    const field_subset_type domain_type) :
    IOP_(IOP),
    codeword_domain_handle_(codeword_domain_handle),
    systematic_domain_handle_(systematic_domain_handle),
    num_oracles_(num_oracles),
    num_queries_(num_queries),
    num_interactions_(num_interactions),
    make_zk_(make_zk),
    field_subset_type_(domain_type)
{
    this->codeword_domain_ = this->IOP_.get_domain(this->codeword_domain_handle_);
    this->codeword_domain_size_ = this->codeword_domain_.num_elements();

    this->systematic_domain_ = this->IOP_.get_domain(this->systematic_domain_handle_);
    this->systematic_domain_size_ = this->systematic_domain_.num_elements();

    this->extended_systematic_domain_ = this->IOP_.get_domain(extended_systematic_domain_handle);

    this->response_size_ = 2 * this->systematic_domain_size_;
}

template<typename FieldT>
void interleaved_rowcheck_protocol<FieldT>::attach_vector_row_oracles(const std::vector<oracle_handle_ptr> &x_handles,
                                                                      const std::vector<oracle_handle_ptr> &y_handles,
                                                                      const std::vector<oracle_handle_ptr> &z_handles)
{
    assert(x_handles.size() == this->num_oracles_);
    assert(y_handles.size() == this->num_oracles_);
    assert(z_handles.size() == this->num_oracles_);

    this->x_vector_row_oracle_handles_ = x_handles;
    this->y_vector_row_oracle_handles_ = y_handles;
    this->z_vector_row_oracle_handles_ = z_handles;
}

template<typename FieldT>
void interleaved_rowcheck_protocol<FieldT>::attach_blinding_vector_row_oracles(
    const std::vector<oracle_handle_ptr> &handles)
{
    assert(handles.size() == this->num_interactions_);

    this->blinding_vector_row_oracle_handles_ = handles;
}

template<typename FieldT>
void interleaved_rowcheck_protocol<FieldT>::register_linear_combinations()
{
    this->random_linear_combination_handles_.resize(this->num_interactions_);
    for (size_t i = 0; i < this->num_interactions_; ++i)
    {
        this->random_linear_combination_handles_[i] =
            this->IOP_.register_verifier_random_message(this->num_oracles_ * this->systematic_domain_size_);
    }
}

template<typename FieldT>
void interleaved_rowcheck_protocol<FieldT>::register_responses()
{
    this->response_handles_.resize(this->num_interactions_);
    for (size_t i = 0; i < this->num_interactions_; ++i)
    {
        this->response_handles_[i] = this->IOP_.register_prover_message(this->response_size_);
    }
}

template<typename FieldT>
void interleaved_rowcheck_protocol<FieldT>::register_queries()
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
void interleaved_rowcheck_protocol<FieldT>::register_queries_for_given_positions(
    std::vector<random_query_position_handle> query_position_handles)
{
    this->query_position_handles_ = std::move(query_position_handles);

    this->x_vector_row_oracles_by_position_handles_.resize(this->num_queries_);
    this->y_vector_row_oracles_by_position_handles_.resize(this->num_queries_);
    this->z_vector_row_oracles_by_position_handles_.resize(this->num_queries_);
    if (this->make_zk_)
    {
        this->blinding_vector_row_oracles_by_position_handles_.resize(this->num_queries_);
    }

    for (size_t i = 0; i < this->num_queries_; ++i)
    {
        std::vector<query_handle> x_new_list;
        this->x_vector_row_oracles_by_position_handles_[i] = x_new_list;
        this->x_vector_row_oracles_by_position_handles_[i].resize(this->num_oracles_);

        std::vector<query_handle> y_new_list;
        this->y_vector_row_oracles_by_position_handles_[i] = y_new_list;
        this->y_vector_row_oracles_by_position_handles_[i].resize(this->num_oracles_);

        std::vector<query_handle> z_new_list;
        this->z_vector_row_oracles_by_position_handles_[i] = z_new_list;
        this->z_vector_row_oracles_by_position_handles_[i].resize(this->num_oracles_);

        for (size_t j = 0; j < this->num_oracles_; ++j)
        {
            this->x_vector_row_oracles_by_position_handles_[i][j] =
                this->IOP_.register_query(this->x_vector_row_oracle_handles_[j], this->query_position_handles_[i]);
            this->y_vector_row_oracles_by_position_handles_[i][j] =
                this->IOP_.register_query(this->y_vector_row_oracle_handles_[j], this->query_position_handles_[i]);
            this->z_vector_row_oracles_by_position_handles_[i][j] =
                this->IOP_.register_query(this->z_vector_row_oracle_handles_[j], this->query_position_handles_[i]);
        }

        if (this->make_zk_)
        {
            std::vector<query_handle> blinding_new_list;
            this->blinding_vector_row_oracles_by_position_handles_[i] = blinding_new_list;
            this->blinding_vector_row_oracles_by_position_handles_[i].resize(this->num_interactions_);

            for (size_t h = 0; h < this->num_interactions_; ++h)
            {
                this->blinding_vector_row_oracles_by_position_handles_[i][h] =
                    this->IOP_.register_query(this->blinding_vector_row_oracle_handles_[h], this->query_position_handles_[i]);
            }
        }
    }
}

/* Proving */
template<typename FieldT>
void interleaved_rowcheck_protocol<FieldT>::calculate_and_submit_responses()
{
    /* The prover computes the evaluations of the polynomial over the entire codeword domain, based
       on the oracle evaluations. (If the claim is true, this should equal 0 over the entire
       systematic domain.) Then the prover interpolates the polynomial's coefficients, and sends
       them. */

    for (size_t h = 0; h < this->num_interactions_; ++h)
    {
        const std::vector<FieldT> random_linear_combination =
            this->IOP_.obtain_verifier_random_message(this->random_linear_combination_handles_[h]);

        std::vector<FieldT> evals_of_response_poly(this->codeword_domain_size_, FieldT(0)); // evaluations of p_0

        /** Build the response polynomial's evaluations row by row. */
        for (size_t i = 0; i < this->num_oracles_; ++i)
        {
            const std::shared_ptr<std::vector<FieldT>> p_x_row_evaluations =
                this->IOP_.get_oracle_evaluations(this->x_vector_row_oracle_handles_[i]);

            const std::shared_ptr<std::vector<FieldT>> p_y_row_evaluations =
                this->IOP_.get_oracle_evaluations(this->y_vector_row_oracle_handles_[i]);

            const std::shared_ptr<std::vector<FieldT>> p_z_row_evaluations =
                this->IOP_.get_oracle_evaluations(this->z_vector_row_oracle_handles_[i]);

            /** For each column, add to that column's corresponding response polynomial evaluation
             *      r * (p_x[col] * p_y[col] - p_z[col])
             *  from this row. */
            for (size_t j = 0; j < this->codeword_domain_size_; ++j)
            {
                FieldT val = (p_x_row_evaluations->operator[](j) * p_y_row_evaluations->operator[](j))
                             - p_z_row_evaluations->operator[](j);
                evals_of_response_poly[j] += random_linear_combination[i] * val;
            }
        }

        if (this->make_zk_)
        {
            const std::shared_ptr<std::vector<FieldT>> blinding_vec =
                this->IOP_.get_oracle_evaluations(this->blinding_vector_row_oracle_handles_[h]);
            for (size_t i = 0; i < this->codeword_domain_size_; ++i)
            {
                evals_of_response_poly[i] += blinding_vec->operator[](i);
            }
        }

        const std::vector<FieldT> response_poly_coefficients =
            IFFT_over_field_subset<FieldT>(evals_of_response_poly, this->codeword_domain_);

        std::vector<FieldT> reduced_coefficients(&response_poly_coefficients[0],
                                                 &response_poly_coefficients[this->response_size_]);
        this->IOP_.submit_prover_message(this->response_handles_[h], std::move(reduced_coefficients));
    }
}

/* Verification */
template<typename FieldT>
bool interleaved_rowcheck_protocol<FieldT>::verifier_predicate()
{
    const std::vector<FieldT> codeword_elements = this->codeword_domain_.all_elements(); // eta

    for (size_t h = 0; h < this->num_interactions_; ++h)
    {
        /* EQUALITY TEST: does the polynomial that was sent equal 0 over the entire systematic
           domain, as it should if the claimed statement is true? */

        enter_block("Rowcheck: equality test (p_0 equals 0 within systematic domain)");
        std::vector<FieldT> response = this->IOP_.receive_prover_message(this->response_handles_[h]); // coefficients of p_0
        const std::vector<FieldT> evaluations = FFT_over_field_subset<FieldT>(response, this->extended_systematic_domain_);
        const polynomial<FieldT> response_poly(std::move(response));

        for (size_t d = 0; d < this->systematic_domain_size_; ++d)
        {
            const std::size_t idx = this->extended_systematic_domain_.reindex_by_subset(
                this->systematic_domain_.dimension(), d);
            if (evaluations[idx] != FieldT(0))
            {
#ifdef DEBUG
                print_indent(); printf("For interactive repetition h = %zu and systematic domain location d = %zu,\n", h, d);

                print_indent(); print_indent();
                printf("polynomial value p_0(zeta_d) = ");
                evaluations[idx].print();
                printf(" != 0\n");
#endif // DEBUG
                return false;
            }
        }
        leave_block("Rowcheck: equality test (p_0 equals 0 within systematic domain)");

        const std::vector<FieldT> random_linear_combination =
            this->IOP_.obtain_verifier_random_message(this->random_linear_combination_handles_[h]);

        /* CONSISTENCY TEST: do the polynomial's values match those of the oracles? */

        enter_block("Rowcheck: querying and performing consistency tests");
        for (size_t k = 0; k < this->num_queries_; ++k)
        {
            const random_query_position_handle this_position_handle = this->query_position_handles_[k];
            const std::size_t j = this->IOP_.obtain_query_position(this_position_handle);

            FieldT lhs(0);
            for (size_t i = 0; i < this->num_oracles_; ++i)
            {
                const FieldT U_x_value = this->IOP_.obtain_query_response(this->x_vector_row_oracles_by_position_handles_[k][i]);
                const FieldT U_y_value = this->IOP_.obtain_query_response(this->y_vector_row_oracles_by_position_handles_[k][i]);
                const FieldT U_z_value = this->IOP_.obtain_query_response(this->z_vector_row_oracles_by_position_handles_[k][i]);

                const FieldT val = U_x_value * U_y_value - U_z_value;

                lhs += random_linear_combination[i] * val;
            }
            if (this->make_zk_)
            {
                lhs += this->IOP_.obtain_query_response(this->blinding_vector_row_oracles_by_position_handles_[k][h]);
            }

            const FieldT rhs = response_poly.evaluation_at_point(codeword_elements[j]);

            if (lhs != rhs)
            {
#ifdef DEBUG
                print_indent(); printf("For interactive repetition h = %zu and query location j = %zu,\n", h, j);

                print_indent(); print_indent();
                printf("[LHS] value from oracles sum_{i in [m]} r_i * [U^x_i,j * U^y_i,j - U^z_i,j] + u_h'[j] = ");
                lhs.print();
                printf("\n");

                print_indent(); print_indent();
                printf("[RHS] polynomial value p_0(eta_j) = ");
                rhs.print();
                printf("\n");
#endif // DEBUG
                return false;
            }
        }
        leave_block("Rowcheck: querying and performing consistency tests");
    }

    return true;
}

} // namespace libiop

