namespace libiop {

template<typename FieldT>
holographic_multi_lincheck_virtual_oracle<FieldT>::holographic_multi_lincheck_virtual_oracle(
    const field_subset<FieldT> &codeword_domain,
    const field_subset<FieldT> &summation_domain,
    const std::size_t input_variable_dim,
    const std::vector<std::shared_ptr<sparse_matrix<FieldT> >> &matrices) :
    codeword_domain_(codeword_domain),
    summation_domain_(summation_domain),
    input_variable_dim_(input_variable_dim),
    matrices_(matrices)
{
}

template<typename FieldT>
void holographic_multi_lincheck_virtual_oracle<FieldT>::set_challenge(const FieldT &alpha, const std::vector<FieldT> r_Mz) {
    /* Set r_Mz */
    if (r_Mz.size() != this->matrices_.size()) {
        throw std::invalid_argument("Not enough random linear combination coefficients were provided");
    }
    this->r_Mz_ = r_Mz;

    enter_block("multi_lincheck construct p_alpha_prime");
    const bool normalized = false;
    this->p_alpha_prime_ = lagrange_polynomial<FieldT>(alpha, this->summation_domain_, normalized);
    leave_block("multi_lincheck construct p_alpha_prime");
}

template<typename FieldT>
std::shared_ptr<std::vector<FieldT>> holographic_multi_lincheck_virtual_oracle<FieldT>::evaluated_contents(
    const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const
{
    enter_block("multi_lincheck evaluated contents");
    if (constituent_oracle_evaluations.size() != this->matrices_.size() + 2)
    {
        throw std::invalid_argument("multi_lincheck uses more constituent oracles than what was provided.");
    }

    /* p_{alpha}^1 in [BCRSVW18] */
    std::vector<FieldT> p_alpha_prime_over_codeword_domain =
        this->p_alpha_prime_.evaluations_over_field_subset(this->codeword_domain_);

    const std::size_t n = this->codeword_domain_.num_elements();

    const std::shared_ptr<std::vector<FieldT>> &fz = constituent_oracle_evaluations[0];
    /* Random linear combination of Mz's */
    std::vector<FieldT> f_combined_Mz(n, FieldT::zero());
    for (std::size_t i = 0; i < n; i++) {
        for (std::size_t m = 0; m < this->matrices_.size(); m++) {
            f_combined_Mz[i] += this->r_Mz_[m] * constituent_oracle_evaluations[m + 1]->operator[](i);
        }
    }

    std::shared_ptr<std::vector<FieldT>> result = std::make_shared<std::vector<FieldT>>();
    result->reserve(n);
    const size_t p_alpha_M_index = this->matrices_.size() + 1;
    for (std::size_t i = 0; i < n; ++i)
    {
        result->emplace_back(
            f_combined_Mz[i] * p_alpha_prime_over_codeword_domain[i] -
            fz->operator[](i) * constituent_oracle_evaluations[p_alpha_M_index]->operator[](i));
    }
    leave_block("multi_lincheck evaluated contents");
    return result;
}

template<typename FieldT>
FieldT holographic_multi_lincheck_virtual_oracle<FieldT>::evaluation_at_point(
    const std::size_t evaluation_position,
    const FieldT evaluation_point,
    const std::vector<FieldT> &constituent_oracle_evaluations) const
{
    UNUSED(evaluation_position);
    if (constituent_oracle_evaluations.size() != this->matrices_.size() + 2)
    {
        throw std::invalid_argument("multi_lincheck uses more constituent oracles than what was provided.");
    }

    FieldT p_alpha_prime_X = this->p_alpha_prime_.evaluation_at_point(evaluation_point);
    const size_t p_alpha_M_index = this->matrices_.size() + 1;
    FieldT p_alpha_M_X = constituent_oracle_evaluations[p_alpha_M_index];

    const FieldT &fz_X = constituent_oracle_evaluations[0];
    FieldT f_combined_Mz_x = FieldT::zero();
    for (std::size_t i = 0; i < this->r_Mz_.size(); i++) {
        f_combined_Mz_x += this->r_Mz_[i] * constituent_oracle_evaluations[i + 1];
    }

    return (f_combined_Mz_x * p_alpha_prime_X - fz_X * p_alpha_M_X);
}

// ---------------------------------------------------------------

template<typename FieldT>
single_matrix_denominator<FieldT>::single_matrix_denominator(
    const field_subset<FieldT> &codeword_domain,
    const field_subset<FieldT> &summation_domain,
    const std::size_t input_variable_dim) :
    codeword_domain_(codeword_domain),
    summation_domain_(summation_domain),
    input_variable_dim_(input_variable_dim)
{
}

template<typename FieldT>
void single_matrix_denominator<FieldT>::set_challenge(
    const FieldT &row_query_point,
    const FieldT &column_query_point)
{
    this->row_query_point_ = row_query_point;
    this->column_query_point_ = column_query_point;
}

template<typename FieldT>
std::shared_ptr<std::vector<FieldT>> single_matrix_denominator<FieldT>::evaluated_contents(
    const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const
{
    if (constituent_oracle_evaluations.size() != 2)
    {
        throw std::invalid_argument("single_matrix_denominator was expecting row, col oracles as input");
    }
    const size_t n = constituent_oracle_evaluations[0]->size();
    std::shared_ptr<std::vector<FieldT>> result = std::make_shared<std::vector<FieldT>>();
    result->reserve(n);
    for (size_t i = 0; i < n; i++)
    {
        const FieldT eval = (constituent_oracle_evaluations[0]->operator[](i) - this->row_query_point_) *
            (constituent_oracle_evaluations[1]->operator[](i) - this->column_query_point_);
        result->emplace_back(eval);
    }
    return result;
}
template<typename FieldT>
FieldT single_matrix_denominator<FieldT>::evaluation_at_point(
    const std::size_t evaluation_position,
    const FieldT evaluation_point,
    const std::vector<FieldT> &constituent_oracle_evaluations) const
{
    UNUSED(evaluation_position);
    if (constituent_oracle_evaluations.size() != 2)
    {
        throw std::invalid_argument("single_matrix_denominator was expecting row, col oracles as input");
    }
    const FieldT shifted_row_val = constituent_oracle_evaluations[0] - this->row_query_point_;
    const FieldT shifted_col_val = constituent_oracle_evaluations[1] - this->column_query_point_;
    return shifted_row_val * shifted_col_val;
}

} // libiop
