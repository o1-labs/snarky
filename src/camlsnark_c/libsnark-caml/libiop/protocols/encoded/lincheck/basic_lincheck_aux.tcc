namespace libiop {

template<typename FieldT>
multi_lincheck_virtual_oracle<FieldT>::multi_lincheck_virtual_oracle(
    const field_subset<FieldT> &codeword_domain,
    const field_subset<FieldT> &constraint_domain,
    const field_subset<FieldT> &variable_domain,
    const field_subset<FieldT> &summation_domain,
    const std::size_t input_variable_dim,
    const std::vector<std::shared_ptr<sparse_matrix<FieldT> >> &matrices) :
    codeword_domain_(codeword_domain),
    constraint_domain_(constraint_domain),
    variable_domain_(variable_domain),
    summation_domain_(summation_domain),
    input_variable_dim_(input_variable_dim),
    matrices_(matrices)
{
    assert(this->use_lagrange_ == false);
    // To use lagrange, the lagrange coefficients cache should be passed in as an argument,
    // with caching set to true.
    // This amortizes the lagrange coefficient calculation across all repetitions.
    if (this->use_lagrange_)
    {
        std::shared_ptr<lagrange_cache<FieldT>> lagrange_coefficients_cache; /* passed in */
        this->lagrange_coefficients_cache_ = lagrange_coefficients_cache;
    }
}

template<typename FieldT>
void multi_lincheck_virtual_oracle<FieldT>::set_challenge(const FieldT &alpha, const std::vector<FieldT> r_Mz) {
    /* Set r_Mz */
    if (r_Mz.size() != this->matrices_.size()) {
        throw std::invalid_argument("Not enough random linear combination coefficients were provided");
    }
    this->r_Mz_ = r_Mz;

    enter_block("multi_lincheck compute alpha powers");
    /** Set alpha powers */
    std::vector<FieldT> alpha_powers;
    alpha_powers.reserve(this->constraint_domain_.num_elements());
    // TODO: Make a method for this in algebra, that lowers the data dependency
    FieldT cur = FieldT::one();
    for (std::size_t i = 0; i < this->constraint_domain_.num_elements(); i++) {
        alpha_powers.emplace_back(cur);
        cur *= alpha;
    }
    leave_block("multi_lincheck compute alpha powers");

    enter_block("multi_lincheck compute p_alpha_prime");
    /** This essentially places alpha powers into the correct spots,
     *  such that the zeroes when the |constraint domain| < summation domain
     *  are placed correctly. */
    std::vector<FieldT> p_alpha_prime_over_summation_domain(
        this->summation_domain_.num_elements(), FieldT::zero());
    for (std::size_t i = 0; i < this->constraint_domain_.num_elements(); i++) {
        const std::size_t element_index = this->summation_domain_.reindex_by_subset(
            this->constraint_domain_.dimension(), i);
        p_alpha_prime_over_summation_domain[element_index] = alpha_powers[i];
    }
    leave_block("multi_lincheck compute p_alpha_prime");

    /* Set p_alpha_ABC_evals */
    enter_block("multi_lincheck compute p_alpha_ABC");
    std::vector<FieldT> p_alpha_ABC_evals(
        this->summation_domain_.num_elements(), FieldT::zero());
    for (std::size_t m_index = 0; m_index < this->matrices_.size(); m_index++)
    {
        const std::shared_ptr<sparse_matrix<FieldT>> M = this->matrices_[m_index];
        // M is cons_domain X var_domain
        for (std::size_t i = 0; i < this->constraint_domain_.num_elements(); i++)
        {
            const linear_combination<FieldT> row = M->get_row(i);

            for (auto &term : row.terms)
            {
                // TODO: Could we instead pass in domains that had this reindexing handled already within them?
                const std::size_t variable_index = this->variable_domain_.reindex_by_subset(
                    this->input_variable_dim_, term.index_);
                const std::size_t summation_index = this->summation_domain_.reindex_by_subset(
                    this->variable_domain_.dimension(), variable_index);
                p_alpha_ABC_evals[summation_index] +=
                    this->r_Mz_[m_index] * term.coeff_ * alpha_powers[i];
            }
        }
    }
    leave_block("multi_lincheck compute p_alpha_ABC");
    // To use lagrange, the following IFFTs must also be moved to evaluated contents
    if (this->use_lagrange_)
    {
        this->alpha_powers_ = alpha_powers;
        this->p_alpha_ABC_evals_ = p_alpha_ABC_evals;
    }
    enter_block("multi_lincheck IFFT p_alphas");
    this->p_alpha_ABC_ = polynomial<FieldT>(
        IFFT_over_field_subset<FieldT>(p_alpha_ABC_evals, this->summation_domain_));
    this->p_alpha_prime_ = polynomial<FieldT>(
        IFFT_over_field_subset<FieldT>(p_alpha_prime_over_summation_domain, this->summation_domain_));
    leave_block("multi_lincheck IFFT p_alphas");
}

template<typename FieldT>
std::shared_ptr<std::vector<FieldT>> multi_lincheck_virtual_oracle<FieldT>::evaluated_contents(
    const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const
{
    enter_block("multi_lincheck evaluated contents");
    if (constituent_oracle_evaluations.size() != this->matrices_.size() + 1)
    {
        throw std::invalid_argument("multi_lincheck uses more constituent oracles than what was provided.");
    }

    /* p_{alpha}^1 in [BCRSVW18] */
    std::vector<FieldT> p_alpha_prime_over_codeword_domain =
        FFT_over_field_subset<FieldT>(this->p_alpha_prime_.coefficients(), this->codeword_domain_);

    /* p_{alpha}^2 in [BCRSVW18] */
    const std::vector<FieldT> p_alpha_ABC_over_codeword_domain =
        FFT_over_field_subset<FieldT>(this->p_alpha_ABC_.coefficients(), this->codeword_domain_);

    const std::size_t n = this->codeword_domain_.num_elements();

    const std::shared_ptr<std::vector<FieldT>> &fz = constituent_oracle_evaluations[0];
    /* Random linear combination of Mz's */
    std::vector<FieldT> f_combined_Mz(n, FieldT::zero());
    for (std::size_t i = 0; i < n; i++) {
        for (std::size_t m = 0; m < this->matrices_.size(); m++) {
            f_combined_Mz[i] += this->r_Mz_[m] * constituent_oracle_evaluations[m + 1]->operator[](i);
        }
    }

    // TODO: Investigate if combining these loops improves speed.
    // It would improve cache efficiency, but its not immediate how it will affect compiler optimizations
    // (re-structuring loop for data parallelism, unrolling, etc.)
    // However, this time may become negligible after making the result over an intermediate sumcheck domain
    std::shared_ptr<std::vector<FieldT>> result = std::make_shared<std::vector<FieldT>>();
    result->reserve(n);
    for (std::size_t i = 0; i < n; ++i)
    {
        result->emplace_back(
            f_combined_Mz[i] * p_alpha_prime_over_codeword_domain[i] -
            fz->operator[](i) * p_alpha_ABC_over_codeword_domain[i]);
    }
    leave_block("multi_lincheck evaluated contents");
    return result;
}

template<typename FieldT>
FieldT multi_lincheck_virtual_oracle<FieldT>::evaluation_at_point(
    const std::size_t evaluation_position,
    const FieldT evaluation_point,
    const std::vector<FieldT> &constituent_oracle_evaluations) const
{
    UNUSED(evaluation_position);
    if (constituent_oracle_evaluations.size() != this->matrices_.size() + 1)
    {
        throw std::invalid_argument("multi_lincheck uses more constituent oracles than what was provided.");
    }

    FieldT p_alpha_prime_X = this->p_alpha_prime_.evaluation_at_point(evaluation_point);
    FieldT p_alpha_ABC_X = this->p_alpha_ABC_.evaluation_at_point(evaluation_point);
    if (this->use_lagrange_)
    {
        const std::vector<FieldT> lagrange_coefficients =
            this->lagrange_coefficients_cache_->coefficients_for(evaluation_point);
        for (size_t i = 0; i < this->constraint_domain_.num_elements(); ++i)
        {
            const std::size_t summation_index = this->summation_domain_.reindex_by_subset(
                this->constraint_domain_.dimension(), i);
            p_alpha_prime_X += lagrange_coefficients[summation_index] * this->alpha_powers_[i];
        }
        for (std::size_t i = 0; i < this->summation_domain_.num_elements(); ++i)
        {
            p_alpha_ABC_X += lagrange_coefficients[i] * this->p_alpha_ABC_evals_[i];
        }
    }

    const FieldT &fz_X = constituent_oracle_evaluations[0];
    FieldT f_combined_Mz_x = FieldT::zero();
    for (std::size_t i = 0; i < this->r_Mz_.size(); i++) {
        f_combined_Mz_x += this->r_Mz_[i] * constituent_oracle_evaluations[i + 1];
    }

    return (f_combined_Mz_x * p_alpha_prime_X - fz_X * p_alpha_ABC_X);
}

} // libiop
