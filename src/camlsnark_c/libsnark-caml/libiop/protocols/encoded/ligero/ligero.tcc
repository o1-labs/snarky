#include <cmath>
#include <stdexcept>

#include "libiop/algebra/lagrange.hpp"
#include "libiop/algebra/fft.hpp"
#include "libiop/common/common.hpp"
#include "libiop/common/profiling.hpp"

namespace libiop {

template<typename FieldT>
interleaved_r1cs_protocol<FieldT>::interleaved_r1cs_protocol(
    iop_protocol<FieldT> &IOP,
    const domain_handle &codeword_domain_handle,
    const domain_handle &systematic_domain_handle,
    const domain_handle &extended_systematic_domain_handle,
    const r1cs_constraint_system<FieldT> &constraint_system,
    const encoded_ligero_parameters &parameters) :
    IOP_(IOP),
    constraint_system_(constraint_system),
    parameters_(parameters),
    codeword_domain_handle_(codeword_domain_handle),
    systematic_domain_handle_(systematic_domain_handle),
    extended_systematic_domain_handle_(extended_systematic_domain_handle)
{
    this->num_queries_ = parameters.num_query_phase_repetitions_;
    this->num_interactions_ = parameters.num_interaction_phase_repetitions_;
    this->make_zk_ = parameters.make_zk_;
    this->field_subset_type_ = parameters.field_subset_type_;

    this->matrix_width_ = parameters.matrix_width_;
    this->matrix_height_ = parameters.matrix_height_;
    this->num_oracles_input_ = parameters.num_oracles_input_;
    this->num_oracles_vectors_ = parameters.num_oracles_vectors_;

    this->codeword_domain_ = this->IOP_.get_domain(this->codeword_domain_handle_);
    this->systematic_domain_ = this->IOP_.get_domain(this->systematic_domain_handle_);
    this->extended_systematic_domain_ = this->IOP_.get_domain(this->extended_systematic_domain_handle_);

    this->codeword_domain_size_ = this->codeword_domain_.num_elements();
    this->systematic_domain_size_ = this->systematic_domain_.num_elements();
    this->extended_systematic_domain_size_ = this->extended_systematic_domain_.num_elements();

    // TODO: We currently don't tell encoded ligero the solved query bound,
    //       though we solve for the correct query bound and queries within set_queries.
    //       There is a bug in the encoded ligero codebase when sampling polynomials
    //       that are b-wise independent, for too large of a b. (encoding independence parameter)
    //       We plan on addressing this soon, however we wanted to get the library online.
    this->encoding_independence_ = 3;

    print_indent(); printf("num oracles for vectors / R1CS constraints (m_2): %zu\n", this->num_oracles_vectors_);

    /* Get R1CS matrices */
    this->A_matrix_ = this->constraint_system_.A_matrix();
    this->B_matrix_ = this->constraint_system_.B_matrix();
    this->C_matrix_ = this->constraint_system_.C_matrix();

    /* Add extra rows */
    this->A_matrix_.resize(this->matrix_height_);
    this->B_matrix_.resize(this->matrix_height_);
    this->C_matrix_.resize(this->matrix_height_);

    this->lincheck_A_.reset(new interleaved_lincheck_ot_protocol<FieldT>(this->IOP_,
                                                                         this->codeword_domain_handle_,
                                                                         this->systematic_domain_handle_,
                                                                         this->extended_systematic_domain_handle_,
                                                                         this->num_oracles_input_,
                                                                         this->num_oracles_vectors_,
                                                                         this->num_queries_,
                                                                         this->num_interactions_,
                                                                         this->make_zk_,
                                                                         this->field_subset_type_,
                                                                         this->A_matrix_));
    this->lincheck_B_.reset(new interleaved_lincheck_ot_protocol<FieldT>(this->IOP_,
                                                                         this->codeword_domain_handle_,
                                                                         this->systematic_domain_handle_,
                                                                         this->extended_systematic_domain_handle_,
                                                                         this->num_oracles_input_,
                                                                         this->num_oracles_vectors_,
                                                                         this->num_queries_,
                                                                         this->num_interactions_,
                                                                         this->make_zk_,
                                                                         this->field_subset_type_,
                                                                         this->B_matrix_));
    this->lincheck_C_.reset(new interleaved_lincheck_ot_protocol<FieldT>(this->IOP_,
                                                                         this->codeword_domain_handle_,
                                                                         this->systematic_domain_handle_,
                                                                         this->extended_systematic_domain_handle_,
                                                                         this->num_oracles_input_,
                                                                         this->num_oracles_vectors_,
                                                                         this->num_queries_,
                                                                         this->num_interactions_,
                                                                         this->make_zk_,
                                                                         this->field_subset_type_,
                                                                         this->C_matrix_));
    this->rowcheck_.reset(new interleaved_rowcheck_protocol<FieldT>(this->IOP_,
                                                                    this->codeword_domain_handle_,
                                                                    this->systematic_domain_handle_,
                                                                    this->extended_systematic_domain_handle_,
                                                                    this->num_oracles_vectors_,
                                                                    this->num_queries_,
                                                                    this->num_interactions_,
                                                                    this->make_zk_,
                                                                    this->field_subset_type_));

    /* Register oracles */
    for (size_t i = 0; i < this->num_oracles_input_; ++i)
    {
        this->w_vector_handles_.emplace_back(std::make_shared<oracle_handle>(
            this->IOP_.register_oracle(this->codeword_domain_handle_, this->systematic_domain_size_, this->make_zk_)));
    }
    for (size_t i = 0; i < this->num_oracles_vectors_; ++i)
    {
        this->a_vector_handles_.emplace_back(std::make_shared<oracle_handle>(
            this->IOP_.register_oracle(this->codeword_domain_handle_, this->systematic_domain_size_, this->make_zk_)));
        this->b_vector_handles_.emplace_back(std::make_shared<oracle_handle>(
            this->IOP_.register_oracle(this->codeword_domain_handle_, this->systematic_domain_size_, this->make_zk_)));
        this->c_vector_handles_.emplace_back(std::make_shared<oracle_handle>(
            this->IOP_.register_oracle(this->codeword_domain_handle_, this->systematic_domain_size_, this->make_zk_)));
    }
    this->concatenated_vector_handles_ = this->w_vector_handles_; // Copy
    this->concatenated_vector_handles_.insert(this->concatenated_vector_handles_.end(),
                                              this->a_vector_handles_.begin(),
                                              this->a_vector_handles_.end());
    this->concatenated_vector_handles_.insert(this->concatenated_vector_handles_.end(),
                                              this->b_vector_handles_.begin(),
                                              this->b_vector_handles_.end());
    this->concatenated_vector_handles_.insert(this->concatenated_vector_handles_.end(),
                                              this->c_vector_handles_.begin(),
                                              this->c_vector_handles_.end());

    if (this->make_zk_)
    {
        this->lincheck_A_blinding_vector_handles_.resize(this->num_interactions_);
        this->lincheck_B_blinding_vector_handles_.resize(this->num_interactions_);
        this->lincheck_C_blinding_vector_handles_.resize(this->num_interactions_);
        this->rowcheck_blinding_vector_handles_.resize(this->num_interactions_);
        for (size_t i = 0; i < this->num_interactions_; ++i)
        {
            this->lincheck_A_blinding_vector_handles_[i] = std::make_shared<oracle_handle>(
                IOP.register_oracle(this->codeword_domain_handle_, this->systematic_domain_size_, this->make_zk_));
            this->lincheck_B_blinding_vector_handles_[i] = std::make_shared<oracle_handle>(
                IOP.register_oracle(this->codeword_domain_handle_, this->systematic_domain_size_, this->make_zk_));
            this->lincheck_C_blinding_vector_handles_[i] = std::make_shared<oracle_handle>(
                IOP.register_oracle(this->codeword_domain_handle_, this->systematic_domain_size_, this->make_zk_));
            this->rowcheck_blinding_vector_handles_[i] = std::make_shared<oracle_handle>(
                IOP.register_oracle(this->codeword_domain_handle_, this->systematic_domain_size_, this->make_zk_));
        }
    }
}

template<typename FieldT>
std::vector<oracle_handle_ptr> interleaved_r1cs_protocol<FieldT>::concatenated_vector_handles() const
{
    return this->concatenated_vector_handles_;
}

template<typename FieldT>
void interleaved_r1cs_protocol<FieldT>::attach_oracles()
{
    this->lincheck_A_->attach_input_vector_row_oracles(this->w_vector_handles_);
    this->lincheck_A_->attach_target_vector_row_oracles(this->a_vector_handles_);

    this->lincheck_B_->attach_input_vector_row_oracles(this->w_vector_handles_);
    this->lincheck_B_->attach_target_vector_row_oracles(this->b_vector_handles_);

    this->lincheck_C_->attach_input_vector_row_oracles(this->w_vector_handles_);
    this->lincheck_C_->attach_target_vector_row_oracles(this->c_vector_handles_);

    this->rowcheck_->attach_vector_row_oracles(this->a_vector_handles_,
                                               this->b_vector_handles_,
                                               this->c_vector_handles_);

    if (this->make_zk_)
    {
        this->lincheck_A_->attach_blinding_vector_row_oracles(this->lincheck_A_blinding_vector_handles_);
        this->lincheck_B_->attach_blinding_vector_row_oracles(this->lincheck_B_blinding_vector_handles_);
        this->lincheck_C_->attach_blinding_vector_row_oracles(this->lincheck_C_blinding_vector_handles_);
        this->rowcheck_->attach_blinding_vector_row_oracles(this->rowcheck_blinding_vector_handles_);
    }
}

template<typename FieldT>
void interleaved_r1cs_protocol<FieldT>::register_linear_combinations()
{
    this->lincheck_A_->register_linear_combinations(); /* linchecks B and C will use the same one */
    this->rowcheck_->register_linear_combinations();
}

template<typename FieldT>
void interleaved_r1cs_protocol<FieldT>::register_responses()
{
    this->lincheck_A_->register_responses();
    this->lincheck_B_->register_responses();
    this->lincheck_C_->register_responses();
    this->rowcheck_->register_responses();
}

template<typename FieldT>
void interleaved_r1cs_protocol<FieldT>::register_queries()
{
    std::vector<random_query_position_handle> query_position_handles;
    query_position_handles.resize(this->num_queries_);

    for (size_t i = 0; i < this->num_queries_; ++i)
    {
        query_position_handles[i] = this->IOP_.register_random_query_position(this->codeword_domain_handle_);
    }

    this->lincheck_A_->register_queries_for_given_positions(query_position_handles);
    this->lincheck_B_->register_queries_for_given_positions(query_position_handles);
    this->lincheck_C_->register_queries_for_given_positions(query_position_handles);
    this->rowcheck_->register_queries_for_given_positions(query_position_handles);
}

/* Proving */
template<typename FieldT>
void interleaved_r1cs_protocol<FieldT>::submit_witness_oracles(const r1cs_primary_input<FieldT> &primary_input,
                                                               const r1cs_auxiliary_input<FieldT> &auxiliary_input)
{
    enter_block("Submit witness oracles");

    enter_block("Generate extended witness and auxiliary witness");
    /* construct z = (1, v, w) */
    std::vector<FieldT> extended_witness = std::vector<FieldT>(1, FieldT(1));
    extended_witness.insert(extended_witness.end(),
                            primary_input.begin(),
                            primary_input.end());
    extended_witness.insert(extended_witness.end(),
                            auxiliary_input.begin(),
                            auxiliary_input.end());

    const std::size_t extra_elements = this->matrix_width_ - extended_witness.size();
    for (size_t i = 0; i < extra_elements; ++i)
    {
        extended_witness.push_back(FieldT(0));
    }

    /* Extended witness, with primary input replaced by 0s */
    std::vector<FieldT> auxiliary_only_witness = std::vector<FieldT>(1 + primary_input.size(), FieldT(0));
    auxiliary_only_witness.insert(auxiliary_only_witness.end(),
                                  auxiliary_input.begin(),
                                  auxiliary_input.end());
    leave_block("Generate extended witness and auxiliary witness");

    enter_block("Perform matrix multiplications");
    std::vector<FieldT> a_result_vector;
    for (size_t i = 0; i < this->matrix_height_; ++i)
    {
        FieldT sum(0);
        std::map<std::size_t, FieldT> row = this->A_matrix_[i];
        typename std::map<std::size_t, FieldT>::iterator it;
        for (it = row.begin(); it != row.end(); it++)
        {
            std::size_t idx = it->first;
            FieldT val = it->second;
            sum += val * extended_witness[idx];
        }
        a_result_vector.push_back(sum);
    }

    std::vector<FieldT> b_result_vector;
    for (size_t i = 0; i < this->matrix_height_; ++i)
    {
        FieldT sum(0);
        std::map<std::size_t, FieldT> row = this->B_matrix_[i];
        typename std::map<std::size_t, FieldT>::iterator it;
        for (it = row.begin(); it != row.end(); it++)
        {
            std::size_t idx = it->first;
            FieldT val = it->second;
            sum += val * extended_witness[idx];
        }
        b_result_vector.push_back(sum);
    }

    std::vector<FieldT> c_result_vector;
    for (size_t i = 0; i < this->matrix_height_; ++i)
    {
        FieldT sum(0);
        std::map<std::size_t, FieldT> row = this->C_matrix_[i];
        typename std::map<std::size_t, FieldT>::iterator it;
        for (it = row.begin(); it != row.end(); it++)
        {
            std::size_t idx = it->first;
            FieldT val = it->second;
            sum += val * extended_witness[idx];
        }
        c_result_vector.push_back(sum);
    }
    leave_block("Perform matrix multiplications");

    enter_block("Submit input oracles");
    for (size_t i = 0; i < this->num_oracles_input_; ++i)
    {
        const std::size_t start = i * this->systematic_domain_size_;
        const std::size_t end = start + this->systematic_domain_size_;

        const std::vector<FieldT> w_row(&auxiliary_only_witness[start], &auxiliary_only_witness[end]);
        const std::vector<FieldT> w_row_coefficients =
            IFFT_over_field_subset<FieldT>(w_row, this->systematic_domain_);
        oracle<FieldT> w_row_oracle(FFT_over_field_subset(w_row_coefficients, this->codeword_domain_));
        this->IOP_.submit_oracle(this->w_vector_handles_[i], std::move(w_row_oracle));
    }
    leave_block("Submit input oracles");

    enter_block("Submit vector oracles");
    for (size_t i = 0; i < this->num_oracles_vectors_; ++i)
    {
        const std::size_t start = i * this->systematic_domain_size_;
        const std::size_t end = start + this->systematic_domain_size_;

        const std::vector<FieldT> a_row(&a_result_vector[start], &a_result_vector[end]);
        const std::vector<FieldT> a_row_coefficients =
            IFFT_over_field_subset<FieldT>(a_row, this->systematic_domain_);
        oracle<FieldT> a_row_oracle(FFT_over_field_subset(a_row_coefficients, this->codeword_domain_));
        this->IOP_.submit_oracle(this->a_vector_handles_[i], std::move(a_row_oracle));

        const std::vector<FieldT> b_row(&b_result_vector[start], &b_result_vector[end]);
        const std::vector<FieldT> b_row_coefficients =
            IFFT_over_field_subset<FieldT>(b_row, this->systematic_domain_);
        oracle<FieldT> b_row_oracle(FFT_over_field_subset(b_row_coefficients, this->codeword_domain_));
        this->IOP_.submit_oracle(this->b_vector_handles_[i], std::move(b_row_oracle));

        const std::vector<FieldT> c_row(&c_result_vector[start], &c_result_vector[end]);
        const std::vector<FieldT> c_row_coefficients =
            IFFT_over_field_subset<FieldT>(c_row, this->systematic_domain_);
        oracle<FieldT> c_row_oracle(FFT_over_field_subset(c_row_coefficients, this->codeword_domain_));
        this->IOP_.submit_oracle(this->c_vector_handles_[i], std::move(c_row_oracle));
    }
    leave_block("Submit vector oracles");
    leave_block("Submit witness oracles");
}

template<typename FieldT>
void interleaved_r1cs_protocol<FieldT>::submit_zero_sum_blinding_vector(const oracle_handle_ptr &handle)
{
    std::vector<FieldT> elems;
    elems.resize(this->systematic_domain_size_);
    FieldT sum(0);
    for (size_t i = 0; i < this->systematic_domain_size_ - 1; ++i)
    {
        elems[i] = FieldT::random_element();
        sum += elems[i];
    }
    elems[this->systematic_domain_size_ - 1] = -sum;

    const std::vector<FieldT> coeffs = IFFT_over_field_subset<FieldT>(elems, this->systematic_domain_);
    const std::vector<FieldT> vector = FFT_over_field_subset<FieldT>(coeffs, this->codeword_domain_);
    oracle<FieldT> vector_oracle(vector);
    this->IOP_.submit_oracle(handle, std::move(vector_oracle));
}

template<typename FieldT>
void interleaved_r1cs_protocol<FieldT>::submit_zero_blinding_vector(const oracle_handle_ptr &handle)
{
    std::vector<FieldT> elems(this->extended_systematic_domain_size_, FieldT(0));
    for (size_t i = this->systematic_domain_size_; i < this->systematic_domain_size_ + this->encoding_independence_; ++i)
    {
        const std::size_t index = this->extended_systematic_domain_.reindex_by_subset(
            this->systematic_domain_.dimension(), i);
        elems[index] = FieldT::random_element();
    }

    const std::vector<FieldT> coeffs = IFFT_over_field_subset<FieldT>(elems, this->extended_systematic_domain_);
    const std::vector<FieldT> vector = FFT_over_field_subset<FieldT>(coeffs, this->codeword_domain_);
    oracle<FieldT> vector_oracle(vector);
    this->IOP_.submit_oracle(handle, std::move(vector_oracle));
}

template<typename FieldT>
void interleaved_r1cs_protocol<FieldT>::submit_blinding_vector_oracles()
{
    assert(this->make_zk_);

    for (size_t i = 0; i < this->num_interactions_; ++i)
    {
        this->submit_zero_sum_blinding_vector(this->lincheck_A_blinding_vector_handles_[i]);
        this->submit_zero_sum_blinding_vector(this->lincheck_B_blinding_vector_handles_[i]);
        this->submit_zero_sum_blinding_vector(this->lincheck_C_blinding_vector_handles_[i]);
        this->submit_zero_blinding_vector(this->rowcheck_blinding_vector_handles_[i]);
    }
}

template<typename FieldT>
void interleaved_r1cs_protocol<FieldT>::calculate_and_submit_proof(const r1cs_primary_input<FieldT> &primary_input)
{
    enter_block("Calculating and submitting proof");

    /* construct additional input as (1,v,0) */
    const std::size_t input_size = this->num_oracles_input_ * this->systematic_domain_size_;
    std::vector<FieldT> additional_input = std::vector<FieldT>(1, FieldT(1));
    additional_input.insert(additional_input.end(),
                            primary_input.begin(),
                            primary_input.end());
    const std::size_t additional_input_size = additional_input.size();
    additional_input.insert(additional_input.end(),
                            input_size - additional_input_size,
                            FieldT(0));
    const std::size_t target_size = this->num_oracles_vectors_ * this->systematic_domain_size_;
    std::vector<FieldT> additional_target = std::vector<FieldT>(target_size, FieldT(0));

    std::vector<std::vector<FieldT>> random_linear_combinations = this->lincheck_A_->all_random_linear_combinations();
    enter_block("Calculating and submitting response: Lincheck A");
    this->lincheck_A_->calculate_and_submit_responses(additional_input, additional_input_size, additional_target, 0, random_linear_combinations);
    leave_block("Calculating and submitting response: Lincheck A");
    enter_block("Calculating and submitting response: Lincheck B");
    this->lincheck_B_->calculate_and_submit_responses(additional_input, additional_input_size, additional_target, 0, random_linear_combinations);
    leave_block("Calculating and submitting response: Lincheck B");
    enter_block("Calculating and submitting response: Lincheck C");
    this->lincheck_C_->calculate_and_submit_responses(additional_input, additional_input_size, additional_target, 0, random_linear_combinations);
    leave_block("Calculating and submitting response: Lincheck C");

    enter_block("Calculating and submitting response: Rowcheck");
    this->rowcheck_->calculate_and_submit_responses();
    leave_block("Calculating and submitting response: Rowcheck");
    leave_block("Calculating and submitting proof");
}

/* Verification */
template<typename FieldT>
bool interleaved_r1cs_protocol<FieldT>::verifier_predicate(const r1cs_primary_input<FieldT> &primary_input)
{
    const std::size_t input_size = this->num_oracles_input_ * this->systematic_domain_size_;
    std::vector<FieldT> additional_input = std::vector<FieldT>(1, FieldT(1));
    additional_input.insert(additional_input.end(),
                            primary_input.begin(),
                            primary_input.end());
    const std::size_t additional_input_size = additional_input.size();
    additional_input.insert(additional_input.end(),
                            input_size - additional_input_size,
                            FieldT(0));
    const std::size_t target_size = this->num_oracles_vectors_ * this->systematic_domain_size_;
    std::vector<FieldT> additional_target = std::vector<FieldT>(target_size, FieldT(0));

    enter_block("Getting Lagrange coefficients for Lincheck tests");
    const std::vector<FieldT> query_points = this->lincheck_A_->all_query_points();
    std::vector<std::vector<FieldT>> lagrange_coefficients;
    if (this->field_subset_type_ == affine_subspace_type)
    {
        lagrange_coefficients =
            this->lincheck_A_->lagrange_coefficients_for_query_points(query_points);
    }
    leave_block("Getting Lagrange coefficients for Lincheck tests");

    const std::vector<std::vector<FieldT>> random_linear_combinations = this->lincheck_A_->all_random_linear_combinations();
    enter_block("Checking predicate for Lincheck A");
    if (!this->lincheck_A_->verifier_predicate(additional_input, additional_input_size, additional_target, 0, random_linear_combinations, lagrange_coefficients))
    {
        print_indent(); printf("Interleaved Lincheck for A matrix failed\n");
        return false;
    }
    leave_block("Checking predicate for Lincheck A");

    enter_block("Checking predicate for Lincheck B");
    if (!this->lincheck_B_->verifier_predicate(additional_input, additional_input_size, additional_target, 0, random_linear_combinations, lagrange_coefficients))
    {
        print_indent(); printf("Interleaved Lincheck for B matrix failed\n");
        return false;
    }
    leave_block("Checking predicate for Lincheck B");

    enter_block("Checking predicate for Lincheck C");
    if (!this->lincheck_C_->verifier_predicate(additional_input, additional_input_size, additional_target, 0, random_linear_combinations, lagrange_coefficients))
    {
        print_indent(); printf("Interleaved Lincheck for C matrix failed\n");
        return false;
    }
    leave_block("Checking predicate for Lincheck C");

    enter_block("Checking predicate for Rowcheck");
    if (!this->rowcheck_->verifier_predicate())
    {
        print_indent(); printf("Interleaved Rowcheck failed\n");
        return false;
    }
    leave_block("Checking predicate for Rowcheck");

    return true;
}

} // namespace libiop

