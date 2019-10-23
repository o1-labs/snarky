#include <cassert>
#include <cmath>
#include <stdexcept>

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/lagrange.hpp"
#include "libiop/common/profiling.hpp"

namespace libiop {

template<typename FieldT>
interleaved_lincheck_ot_protocol<FieldT>::interleaved_lincheck_ot_protocol(
    iop_protocol<FieldT> &IOP,
    const domain_handle &codeword_domain_handle,
    const domain_handle &systematic_domain_handle,
    const domain_handle &extended_systematic_domain_handle,
    const std::size_t num_oracles_input,
    const std::size_t num_oracles_target,
    const std::size_t num_queries,
    const std::size_t num_interactions,
    const bool make_zk,
    const field_subset_type domain_type,
    const naive_sparse_matrix<FieldT> constraint_matrix) :
    IOP_(IOP),
    codeword_domain_handle_(codeword_domain_handle),
    systematic_domain_handle_(systematic_domain_handle),
    num_oracles_input_(num_oracles_input),
    num_oracles_target_(num_oracles_target),
    num_queries_(num_queries),
    num_interactions_(num_interactions),
    make_zk_(make_zk),
    field_subset_type_(domain_type),
    constraint_matrix_(constraint_matrix)
{
    this->codeword_domain_ = this->IOP_.get_domain(this->codeword_domain_handle_);
    this->codeword_domain_size_ = this->codeword_domain_.num_elements();

    this->systematic_domain_ = this->IOP_.get_domain(this->systematic_domain_handle_);
    this->systematic_domain_size_ = this->systematic_domain_.num_elements();

    this->extended_systematic_domain_ = this->IOP_.get_domain(extended_systematic_domain_handle);

    this->response_size_ = 2 * this->systematic_domain_size_;
}

template<typename FieldT>
void interleaved_lincheck_ot_protocol<FieldT>::attach_input_vector_row_oracles(
    const std::vector<oracle_handle_ptr> &handles)
{
    assert(handles.size() == this->num_oracles_input_);

    this->input_vector_row_oracle_handles_ = handles;
}

template<typename FieldT>
void interleaved_lincheck_ot_protocol<FieldT>::attach_target_vector_row_oracles(
    const std::vector<oracle_handle_ptr> &handles)
{
    assert(handles.size() == this->num_oracles_target_);

    this->target_vector_row_oracle_handles_ = handles;
}

template<typename FieldT>
void interleaved_lincheck_ot_protocol<FieldT>::attach_blinding_vector_row_oracles(
    const std::vector<oracle_handle_ptr> &handles)
{
    assert(handles.size() == this->num_interactions_);

    this->blinding_vector_row_oracle_handles_ = handles;
}

template<typename FieldT>
void interleaved_lincheck_ot_protocol<FieldT>::register_linear_combinations()
{
    this->random_linear_combination_handles_.resize(this->num_interactions_);
    for (size_t i = 0; i < this->num_interactions_; ++i)
    {
        this->random_linear_combination_handles_[i] = this->IOP_.register_verifier_random_message(this->constraint_matrix_.size());
    }
}

template<typename FieldT>
void interleaved_lincheck_ot_protocol<FieldT>::register_responses()
{
    this->response_handles_.resize(this->num_interactions_);
    for (size_t i = 0; i < this->num_interactions_; ++i)
    {
         this->response_handles_[i] = this->IOP_.register_prover_message(this->response_size_);
    }
}

template<typename FieldT>
void interleaved_lincheck_ot_protocol<FieldT>::register_queries()
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
void interleaved_lincheck_ot_protocol<FieldT>::register_queries_for_given_positions(
    std::vector<random_query_position_handle> query_position_handles)
{
    this->query_position_handles_ = std::move(query_position_handles);

    this->input_vector_row_oracles_by_position_handles_.resize(this->num_queries_);
    this->target_vector_row_oracles_by_position_handles_.resize(this->num_queries_);
    if (this->make_zk_)
    {
        this->blinding_vector_row_oracles_by_position_handles_.resize(this->num_queries_);
    }

    for (size_t i = 0; i < this->num_queries_; ++i)
    {
        std::vector<query_handle> input_new_list;
        this->input_vector_row_oracles_by_position_handles_[i] = input_new_list;
        this->input_vector_row_oracles_by_position_handles_[i].resize(this->num_oracles_input_);

        for (size_t j = 0; j < this->num_oracles_input_; ++j)
        {
            this->input_vector_row_oracles_by_position_handles_[i][j] =
                this->IOP_.register_query(this->input_vector_row_oracle_handles_[j],
                                          this->query_position_handles_[i]);
        }

        std::vector<query_handle> target_new_list;
        this->target_vector_row_oracles_by_position_handles_[i] = target_new_list;
        this->target_vector_row_oracles_by_position_handles_[i].resize(this->num_oracles_target_);

        for (size_t j = 0; j < this->num_oracles_target_; ++j)
        {
            this->target_vector_row_oracles_by_position_handles_[i][j] =
                this->IOP_.register_query(this->target_vector_row_oracle_handles_[j],
                                          this->query_position_handles_[i]);
        }

        if (this->make_zk_)
        {
            std::vector<query_handle> blinding_new_list;
            this->blinding_vector_row_oracles_by_position_handles_[i] = blinding_new_list;
            this->blinding_vector_row_oracles_by_position_handles_[i].resize(this->num_interactions_);

            for (size_t j = 0; j < this->num_interactions_; ++j)
            {
                this->blinding_vector_row_oracles_by_position_handles_[i][j] =
                    this->IOP_.register_query(this->blinding_vector_row_oracle_handles_[j],
                                            this->query_position_handles_[i]);
            }
        }
    }
}

template<typename FieldT>
std::vector<std::vector<FieldT>> interleaved_lincheck_ot_protocol<FieldT>::all_random_linear_combinations()
{
    /* Helper function to allow use of the same random values in all the lincheck subroutines of an
       interleaved_r1cs (Ligero) run. */

    std::vector<std::vector<FieldT>> random_linear_combinations;
    random_linear_combinations.resize(this->num_interactions_);
    for (size_t i = 0; i < this->num_interactions_; ++i)
    {
        random_linear_combinations[i] =
            this->IOP_.obtain_verifier_random_message(this->random_linear_combination_handles_[i]);
    }

    return random_linear_combinations;
}

/* Proving */
template<typename FieldT>
void interleaved_lincheck_ot_protocol<FieldT>::calculate_and_submit_responses()
{
    /* Convenience function: supply 0 values for supplementary input, supplementary target, and random linear combinations. */

    const std::vector<FieldT> supplementary_input(this->num_oracles_input_ * this->systematic_domain_size_, FieldT(0));
    const std::vector<FieldT> supplementary_target(this->num_oracles_target_ * this->systematic_domain_size_, FieldT(0));
    const std::vector<std::vector<FieldT>> random_linear_combinations;
    this->calculate_and_submit_responses(supplementary_input, 0, supplementary_target, 0, random_linear_combinations);
}

template<typename FieldT>
void interleaved_lincheck_ot_protocol<FieldT>::calculate_and_submit_responses(const std::vector<FieldT> &supplementary_input,
                                                                              const std::size_t supplementary_input_size,
                                                                              const std::vector<FieldT> &supplementary_target,
                                                                              const std::size_t supplementary_target_size,
                                                                              std::vector<std::vector<FieldT>> random_linear_combinations)
{
    /* The prover computes the evaluations of the polynomial over the entire codeword domain, based
       on the oracle evaluations. (If the claim is true, this should sum to 0 over the
       systematic domain.) Then the prover interpolates the polynomial's coefficients, and sends
       them. */

    /* To check consistency with a public (prefix of the) input, we accept that input (followed by
       0s) as an argument, encode it, and add it to the encoded (hidden) input, which should start
       with 0s. By linearity, this will give the right result iff the correct overall input begins
       with the public prefix. */
    enter_block("Input consistency");
    std::vector<std::vector<FieldT>> supplementary_input_vectors;
    supplementary_input_vectors.resize(this->num_oracles_input_);
    const std::size_t num_supplementary_input_vectors = (std::size_t) ceil((supplementary_input_size + 0.0) / this->systematic_domain_size_);
    for (size_t i = 0; i < num_supplementary_input_vectors; ++i)
    {
        const std::size_t start = i * this->systematic_domain_size_;
        const std::size_t end = start + this->systematic_domain_size_;

        const std::vector<FieldT> row(&supplementary_input[start], &supplementary_input[end]);
        const std::vector<FieldT> row_coefficients =
            IFFT_over_field_subset<FieldT>(row, this->systematic_domain_);
        std::vector<FieldT> row_vector =
            FFT_over_field_subset<FieldT>(row_coefficients, this->codeword_domain_);
        supplementary_input_vectors[i] = std::move(row_vector);
    }
    for (size_t i = num_supplementary_input_vectors; i < this->num_oracles_input_; ++i)
    {
        supplementary_input_vectors[i] = std::vector<FieldT>(this->codeword_domain_size_, FieldT(0));
    }

    std::vector<std::vector<FieldT>> supplementary_target_vectors;
    supplementary_target_vectors.resize(this->num_oracles_target_);
    const std::size_t num_supplementary_target_vectors = (std::size_t) ceil((supplementary_target_size + 0.0) / this->systematic_domain_size_);
    for (size_t i = 0; i < num_supplementary_target_vectors; ++i)
    {
        const std::size_t start = i * this->systematic_domain_size_;
        const std::size_t end = start + this->systematic_domain_size_;

        const std::vector<FieldT> row(&supplementary_target[start], &supplementary_target[end]);
        const std::vector<FieldT> row_coefficients =
            IFFT_over_field_subset<FieldT>(row, this->systematic_domain_);
        std::vector<FieldT> row_vector =
            FFT_over_field_subset<FieldT>(row_coefficients, this->codeword_domain_);
        supplementary_target_vectors[i] = std::move(row_vector);
    }
    for (size_t i = num_supplementary_target_vectors; i < this->num_oracles_target_; ++i)
    {
        supplementary_target_vectors[i] = std::vector<FieldT>(this->codeword_domain_size_, FieldT(0));
    }
    leave_block("Input consistency");

    /* We accept values for the random linear combinations as a parameter, to ensure consistency.
       If none are given, we use the actual random values sent by the verifier. */
    if (random_linear_combinations.size() == 0)
    {
        random_linear_combinations.resize(this->num_interactions_);
        for (size_t h = 0; h < this->num_interactions_; ++h)
        {
            random_linear_combinations[h] =
                this->IOP_.obtain_verifier_random_message(this->random_linear_combination_handles_[h]);
        }
    }

    for (size_t h = 0; h < this->num_interactions_; ++h)
    {
        /* each interaction will have a different random linear combination */
        const std::vector<FieldT> random_linear_combination = random_linear_combinations[h];

        std::vector<FieldT> evals_of_response_poly(this->codeword_domain_size_, FieldT(0));

        /** Set row vector to be the concatenation of evaluations of s_i in the systematic domain.
         * It can be thought of as a flattened matrix with num_oracles_input rows, and
         * systematic_domain_size columns, where each row is the evaluations for a given s_i. */
        std::vector<FieldT> row_vector(this->num_oracles_input_ * this->systematic_domain_size_, FieldT(0));
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

        /** handles creating the component of p for
         *  the sum over all output oracles: r_i * f_{x,i} */
        for (size_t i = 0; i < this->num_oracles_target_; ++i)
        {
            const std::size_t I = i * this->systematic_domain_size_;
            std::vector<FieldT> current_vector;
            for (size_t j = 0; j < this->systematic_domain_size_; ++j)
            {
                const std::size_t x = I + j;
                current_vector.emplace_back(random_linear_combination[x]);
            }

            const std::vector<FieldT> poly_coefficients =
                IFFT_over_field_subset<FieldT>(current_vector, this->systematic_domain_);
            const std::vector<FieldT> current_evaluations =
                FFT_over_field_subset<FieldT>(poly_coefficients, this->codeword_domain_);

            const std::shared_ptr<std::vector<FieldT>> row_evaluations =
                this->IOP_.get_oracle_evaluations(this->target_vector_row_oracle_handles_[i]);

            for (size_t a = 0; a < this->codeword_domain_size_; ++a)
            {
                evals_of_response_poly[a] += current_evaluations[a] *
                    (row_evaluations->operator[](a) + supplementary_target_vectors[i][a]);
            }
        }

        /** handles creating the component of p for
         *  the sum over all input oracles: s_i * f_{y,i} */
        for (size_t i = 0; i < this->num_oracles_input_; ++i)
        {
            const std::size_t start = this->systematic_domain_size_ * i;
            const std::size_t end = this->systematic_domain_size_ + start;

            const typename std::vector<FieldT>::const_iterator row_first = row_vector.begin() + start;
            const typename std::vector<FieldT>::const_iterator row_last = row_vector.begin() + end;
            const std::vector<FieldT> current_vector(row_first, row_last);

            const std::vector<FieldT> poly_coefficients =
                IFFT_over_field_subset<FieldT>(current_vector, this->systematic_domain_);
            const std::vector<FieldT> current_evaluations =
                FFT_over_field_subset<FieldT>(poly_coefficients, this->codeword_domain_);

            const std::shared_ptr<std::vector<FieldT>> row_evaluations =
                this->IOP_.get_oracle_evaluations(this->input_vector_row_oracle_handles_[i]);

            for (size_t a = 0; a < this->codeword_domain_size_; ++a)
            {
                evals_of_response_poly[a] -= current_evaluations[a] *
                    (row_evaluations->operator[](a) + supplementary_input_vectors[i][a]);
            }
        }

        if (this->make_zk_)
        {
            const std::shared_ptr<std::vector<FieldT>> blinding_vec =
                this->IOP_.get_oracle_evaluations(this->blinding_vector_row_oracle_handles_[h]);
            for (size_t a = 0; a < this->codeword_domain_size_; ++a)
            {
                evals_of_response_poly[a] += blinding_vec->operator[](a);
            }
        }

        const std::vector<FieldT> response_poly_coefficients =
            IFFT_over_field_subset<FieldT>(evals_of_response_poly, this->codeword_domain_);
        std::vector<FieldT> reduced_coefficients(&response_poly_coefficients[0],
                                                 &response_poly_coefficients[this->response_size_]);

        this->IOP_.submit_prover_message(this->response_handles_[h], std::move(reduced_coefficients));
    }
}

template<typename FieldT>
std::vector<FieldT> interleaved_lincheck_ot_protocol<FieldT>::all_query_points()
{
    const std::vector<FieldT> elems = this->codeword_domain_.all_elements();

    std::vector<FieldT> points;
    for (size_t k = 0; k < this->num_queries_; ++k)
    {
        const random_query_position_handle this_position_handle = this->query_position_handles_[k];
        const std::size_t j = this->IOP_.obtain_query_position(this_position_handle);
        points.emplace_back(elems[j]);
    }

    return points;
}

template<typename FieldT>
std::vector<std::vector<FieldT>> interleaved_lincheck_ot_protocol<FieldT>::lagrange_coefficients_for_query_points(
    std::vector<FieldT> query_points)
{
    std::vector<std::vector<FieldT>> all_coefficients;
    for (size_t i = 0; i < query_points.size(); ++i)
    {
        std::vector<FieldT> these_coefficients = lagrange_coefficients(this->systematic_domain_.subspace(),
                                                                       query_points[i]);
        all_coefficients.emplace_back(std::move(these_coefficients));
    }
    return all_coefficients;
}

/* Verification */
template<typename FieldT>
bool interleaved_lincheck_ot_protocol<FieldT>::verifier_predicate()
{
    const std::vector<FieldT> supplementary_input(this->num_oracles_input_ * this->systematic_domain_size_, FieldT(0));
    const std::vector<FieldT> supplementary_target(this->num_oracles_target_ * this->systematic_domain_size_, FieldT(0));
    const std::vector<std::vector<FieldT>> random_linear_combinations;
    const std::vector<std::vector<FieldT>> lagrange_coefficients;
    return this->verifier_predicate(supplementary_input, 0, supplementary_target, 0, random_linear_combinations, lagrange_coefficients);
}

template<typename FieldT>
bool interleaved_lincheck_ot_protocol<FieldT>::verifier_predicate(const std::vector<FieldT> &supplementary_input,
                                                                  const std::size_t supplementary_input_size,
                                                                  const std::vector<FieldT> &supplementary_target,
                                                                  const std::size_t supplementary_target_size,
                                                                  std::vector<std::vector<FieldT>> random_linear_combinations,
                                                                  std::vector<std::vector<FieldT>> lagrange_coefficients)
{
    enter_block("Input consistency");
    std::vector<std::vector<FieldT>> supplementary_input_vectors;
    supplementary_input_vectors.resize(this->num_oracles_input_);
    const std::size_t num_supplementary_input_vectors = (std::size_t) ceil((supplementary_input_size + 0.0) / this->systematic_domain_size_);
    for (size_t i = 0; i < num_supplementary_input_vectors; ++i)
    {
        const std::size_t start = i * this->systematic_domain_size_;
        const std::size_t end = start + this->systematic_domain_size_;

        const std::vector<FieldT> row(&supplementary_input[start], &supplementary_input[end]);
        const std::vector<FieldT> row_coefficients =
            IFFT_over_field_subset<FieldT>(row, this->systematic_domain_);
        std::vector<FieldT> row_vector =
            FFT_over_field_subset<FieldT>(row_coefficients, this->codeword_domain_);
        supplementary_input_vectors[i] = std::move(row_vector);
    }
    for (size_t i = num_supplementary_input_vectors; i < this->num_oracles_input_; ++i)
    {
        supplementary_input_vectors[i] = std::vector<FieldT>(this->codeword_domain_size_, FieldT(0));
    }

    std::vector<std::vector<FieldT>> supplementary_target_vectors;
    supplementary_target_vectors.resize(this->num_oracles_target_);
    const std::size_t num_supplementary_target_vectors = (std::size_t) ceil((supplementary_target_size + 0.0) / this->systematic_domain_size_);
    for (size_t i = 0; i < num_supplementary_target_vectors; ++i)
    {
        const std::size_t start = i * this->systematic_domain_size_;
        const std::size_t end = start + this->systematic_domain_size_;

        const std::vector<FieldT> row(&supplementary_target[start], &supplementary_target[end]);
        const std::vector<FieldT> row_coefficients =
            IFFT_over_field_subset<FieldT>(row, this->systematic_domain_);
        std::vector<FieldT> row_vector =
            FFT_over_field_subset<FieldT>(row_coefficients, this->codeword_domain_);
        supplementary_target_vectors[i] = std::move(row_vector);
    }
    for (size_t i = num_supplementary_target_vectors; i < this->num_oracles_target_; ++i)
    {
        supplementary_target_vectors[i] = std::vector<FieldT>(this->codeword_domain_size_, FieldT(0));
    }
    leave_block("Input consistency");

    if (random_linear_combinations.size() == 0)
    {
        random_linear_combinations.resize(this->num_interactions_);
        for (size_t i = 0; i < this->num_interactions_; ++i)
        {
            random_linear_combinations[i] =
                this->IOP_.obtain_verifier_random_message(this->random_linear_combination_handles_[i]);
        }
    }

    bool using_lagrange = this->field_subset_type_ == affine_subspace_type;

    if (using_lagrange && lagrange_coefficients.size() == 0)
    {
        const std::vector<FieldT> query_points = this->all_query_points();
        lagrange_coefficients = this->lagrange_coefficients_for_query_points(query_points);
    }

    const std::vector<FieldT> codeword_elements = this->codeword_domain_.all_elements(); // eta

    for (size_t h = 0; h < this->num_interactions_; ++h)
    {
        /* EQUALITY TEST: does the polynomial that was sent sum to 0 over the systematic domain, as
           it should if the claimed statement is true? */

        enter_block("Lincheck: equality test (p_0 sums to 0 over systematic domain)");
        std::vector<FieldT> response = this->IOP_.receive_prover_message(this->response_handles_[h]); // coefficients of p_0
        const std::vector<FieldT> evaluations = FFT_over_field_subset<FieldT>(response, this->extended_systematic_domain_);
        polynomial<FieldT> response_poly(std::move(response));

        FieldT equality_lhs(0);
        for (size_t d = 0; d < this->systematic_domain_size_; ++d)
        {
            const std::size_t idx = this->extended_systematic_domain_.reindex_by_subset(
                this->systematic_domain_.dimension(), d);
            equality_lhs += evaluations[idx];
        }

        if (equality_lhs != FieldT(0))
        {
#ifdef DEBUG
            print_indent(); printf("For interactive repetition h = %zu\n", h);

            print_indent(); print_indent();
            printf("sum_{d in [l]} p_0,h(zeta_d) = ");
            equality_lhs.print();
            printf(" != 0\n");
#endif // DEBUG
            return false;
        }
        leave_block("Lincheck: equality test (p_0 sums to 0 over systematic domain)");

        /* Preparation for consistency test. */

        const std::vector<FieldT> random_linear_combination = random_linear_combinations[h];

        /* Split vector into rows over the systematic domain, to interpolate into polynomials (for
           the consistency test). */
        std::vector<std::vector<FieldT>> random_linear_combination_row_vectors;
        for (size_t i = 0; i < this->num_oracles_target_; ++i)
        {
            const std::size_t start = this->systematic_domain_size_ * i;
            const std::size_t end = this->systematic_domain_size_ + start;

            const typename std::vector<FieldT>::const_iterator first = random_linear_combination.begin() + start;
            const typename std::vector<FieldT>::const_iterator last = random_linear_combination.begin() + end;
            std::vector<FieldT> random_linear_combination_row_vector(first, last);
            random_linear_combination_row_vectors.emplace_back(std::move(random_linear_combination_row_vector));
        }

        /* Multiply matrix by random values. (building s_i) */
        std::vector<FieldT> randomized_matrix_vector(this->num_oracles_input_ * this->systematic_domain_size_, FieldT(0));
        for (size_t j = 0; j < this->constraint_matrix_.size(); ++j)
        {
            std::map<std::size_t, FieldT> row = this->constraint_matrix_[j];
            typename std::map<std::size_t, FieldT>::iterator it;
            for (it = row.begin(); it != row.end(); it++)
            {
                const std::size_t idx = it->first;
                const FieldT val = it->second;
                randomized_matrix_vector[idx] += random_linear_combination[j] * val;
            }
        }

        /* Split vector into rows over the systematic domain, to interpolate into polynomials (for
           the consistency test). */
        std::vector<std::vector<FieldT>> randomized_matrix_row_vectors;
        for (size_t i = 0; i < this->num_oracles_input_; ++i)
        {
            const std::size_t start = this->systematic_domain_size_ * i;
            const std::size_t end = this->systematic_domain_size_ + start;

            const typename std::vector<FieldT>::const_iterator first = randomized_matrix_vector.begin() + start;
            const typename std::vector<FieldT>::const_iterator last = randomized_matrix_vector.begin() + end;
            std::vector<FieldT> randomized_matrix_row_vector(first, last);
            randomized_matrix_row_vectors.emplace_back(std::move(randomized_matrix_row_vector));
        }

        /* CONSISTENCY TEST: do the polynomial's values match those of the oracles? */

        /* Can we use Lagrange interpolation? */
        if (using_lagrange)
        {
            std::vector<FieldT> codeword_elems = this->codeword_domain_.all_elements();
            const std::vector<FieldT> systematic_elements = this->systematic_domain_.all_elements();
            for (size_t k = 0; k < this->num_queries_ && using_lagrange; ++k)
            {
                const random_query_position_handle this_position_handle = this->query_position_handles_[k];
                const std::size_t j = this->IOP_.obtain_query_position(this_position_handle);
                const FieldT eval_point = codeword_elems[j];
                for (size_t t = 0; t < this->systematic_domain_size_; ++t)
                {
                    if (systematic_elements[t] == eval_point)
                    {
                        /* If the evaluation point is part of the domain, Lagrange doesn't work (all the coefficients
                        are 0), so we have to do it naively. */
                        using_lagrange = false;
                        break;
                    }
                }
            }
        }

        /* If we aren't using Lagrange, perform precomputation. */
        std::vector<std::vector<FieldT>> random_linear_combination_row_evals;
        std::vector<std::vector<FieldT>> randomized_matrix_row_vector_evals;
        if (!using_lagrange)
        {
            /* TODO: efficiency running FFTs to evaluate here vs saving polynomials and evaluating them
            in tests depends on parameters */
            enter_block("Precomputing for consistency tests");
            random_linear_combination_row_evals.reserve(this->num_oracles_target_);
            for (size_t i = 0; i < this->num_oracles_target_; ++i)
            {
                const std::vector<FieldT> random_linear_combination_row_vector = random_linear_combination_row_vectors[i];
                std::vector<FieldT> row_vector_poly_coeffs = IFFT_over_field_subset(random_linear_combination_row_vector,
                                                                                    this->systematic_domain_);
                std::vector<FieldT> row_vector_poly_evals = FFT_over_field_subset(row_vector_poly_coeffs,
                                                                                this->codeword_domain_);
                random_linear_combination_row_evals.emplace_back(std::move(row_vector_poly_evals));
            }

            randomized_matrix_row_vector_evals.reserve(this->num_oracles_input_);
            for (size_t i = 0; i < this->num_oracles_input_; ++i)
            {
                std::vector<FieldT> row_vector_poly_coeffs = IFFT_over_field_subset(randomized_matrix_row_vectors[i],
                                                                                    this->systematic_domain_);
                std::vector<FieldT> row_vector_poly_evals = FFT_over_field_subset(row_vector_poly_coeffs,
                                                                                this->codeword_domain_);
                randomized_matrix_row_vector_evals.emplace_back(std::move(row_vector_poly_evals));
            }
            leave_block("Precomputing for consistency tests");
        }

        enter_block("Lincheck: querying and performing consistency tests");
        for (size_t k = 0; k < this->num_queries_; ++k)
        {
            const random_query_position_handle this_position_handle = this->query_position_handles_[k];
            const std::size_t j = this->IOP_.obtain_query_position(this_position_handle);

            std::vector<FieldT> lagrange_coefficients_at_this_point;
            if (using_lagrange)
            {
                lagrange_coefficients_at_this_point = lagrange_coefficients[k];
            }

            FieldT consistency_test_lhs(0);

            /* Subtracts the target vector's corresponding sum: ∑ r_i * F_x[i, a_k] to consistency_test_lhs */
            for (size_t i = 0; i < this->num_oracles_target_; ++i)
            {
                const std::vector<FieldT> random_linear_combination_row_vector = random_linear_combination_row_vectors[i];

                const FieldT target_oracle_value = this->IOP_.obtain_query_response(this->target_vector_row_oracles_by_position_handles_[k][i]) +
                                                   supplementary_target_vectors[i][j];

                FieldT eval(0);
                const FieldT eval_point = this->codeword_domain_.element_by_index(j);

                if (using_lagrange)
                {
                    const std::vector<FieldT> systematic_domain_elements = this->systematic_domain_.all_elements();
                    for (size_t t = 0; t < this->systematic_domain_size_; ++t)
                    {
                        eval += random_linear_combination_row_vector[t] * lagrange_coefficients_at_this_point[t];
                    }
                }
                else
                {
                    eval = random_linear_combination_row_evals[i][j];
                }

                consistency_test_lhs += eval * target_oracle_value;
            }

            /* Adds the input oracle's corresponding sum: ∑ s_i * F_y[i, a_k] to consistency_test_lhs */
            for (size_t i = 0; i < this->num_oracles_input_; ++i)
            {
                const std::vector<FieldT> randomized_matrix_row_vector = randomized_matrix_row_vectors[i];

                const FieldT input_oracle_value = this->IOP_.obtain_query_response(this->input_vector_row_oracles_by_position_handles_[k][i]) +
                                                  supplementary_input_vectors[i][j];

                FieldT eval(0);
                const FieldT eval_point = this->codeword_domain_.element_by_index(j);

                if (using_lagrange)
                {
                    const std::vector<FieldT> systematic_domain_elements = this->systematic_domain_.all_elements();
                    for (size_t t = 0; t < this->systematic_domain_size_; ++t)
                    {
                        eval += randomized_matrix_row_vector[t] * lagrange_coefficients_at_this_point[t];
                    }
                }
                else
                {
                    eval = randomized_matrix_row_vector_evals[i][j];
                }

                consistency_test_lhs -= eval * input_oracle_value;
            }

            if (this->make_zk_)
            {
                consistency_test_lhs += this->IOP_.obtain_query_response(this->blinding_vector_row_oracles_by_position_handles_[k][h]);
            }
            const FieldT consistency_test_rhs = response_poly.evaluation_at_point(codeword_elements[j]);

            if (consistency_test_lhs != consistency_test_rhs)
            {
#ifdef DEBUG
                print_indent(); printf("For interactive repetition h = %zu and query position j = %zu,\n", h, j);

                print_indent(); print_indent();
                printf("[LHS] value from oracles sum_{i in [m_1]} r_i,h(eta_j) * U^x_i,j ");
                printf("- sum_{i in [m_2]} s_i,h(eta_j) * U^y_i,j + u_h'[h] = ");
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
