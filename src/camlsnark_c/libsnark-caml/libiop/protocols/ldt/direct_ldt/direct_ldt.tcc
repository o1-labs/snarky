namespace libiop {

template<typename FieldT>
direct_LDT_parameters<FieldT>::direct_LDT_parameters(const size_t query_soundness_bits,
                                                     const size_t poly_degree_bound,
                                                     const size_t RS_extra_dimensions,
                                                     const size_t absolute_proximity_parameter) :
    query_soundness_bits_(query_soundness_bits),
    poly_degree_bound_(poly_degree_bound),
    RS_extra_dimensions_(RS_extra_dimensions),
    absolute_proximity_parameter_(absolute_proximity_parameter)
{
    this->codeword_domain_dim_ = libiop::log2(this->poly_degree_bound_) + RS_extra_dimensions;
    const long double codeword_domain_size = (long double)(1ull << this->codeword_domain_dim_);
    /** query_soundness_error = (1 - fractional_proximity)^{num_queries}]x
     *  Consequently the bits of security is:
     *      -query_soundness_bits = num_queries * log(1 - fractional_proximity)
     *  Rearranging for num_queries we get:
     *      num_queries = ceil(-query_soundness_bits / log(1 - fractional_proximity)) */
    this->fractional_proximity_parameter_ = ((long double) absolute_proximity_parameter) / codeword_domain_size;
    const long double denominator = log2l(1 - this->fractional_proximity_parameter_);
    const long double casted_query_soundness_bits = (long double)query_soundness_bits;
    const long double num_queries = ceil(-casted_query_soundness_bits / denominator);
    this->num_queries_ = std::max<size_t>(1, (size_t)num_queries);
}

template<typename FieldT>
long double direct_LDT_parameters<FieldT>::achieved_query_soundness() const
{
    const long double soundness_per_query = log2l(1 - this->fractional_proximity_parameter_);
    return -((long double)this->num_queries_) * soundness_per_query;
}

template<typename FieldT>
void direct_LDT_parameters<FieldT>::override_security_parameter(const size_t num_queries)
{
    this->overrided_num_queries_ = true;
    this->num_queries_ = num_queries;
}
template<typename FieldT>
size_t direct_LDT_parameters<FieldT>::num_queries() const
{
    return this->num_queries_;
}
template<typename FieldT>
size_t direct_LDT_parameters<FieldT>::poly_degree_bound() const
{
    return this->poly_degree_bound_;
}

template<typename FieldT>
void direct_LDT_parameters<FieldT>::print() const
{
    printf("\nDirect LDT parameters\n");
    if (this->overrided_num_queries_)
    {
        print_indent(); printf("===WARNING=== Direct LDT security parameter was overridden\n");
    }
    print_indent(); printf("* target query soundness error (bits): %zu\n", this->query_soundness_bits_);
    print_indent(); printf("* achieved query soundness error (bits): %.1Lf\n", this->achieved_query_soundness());
    print_indent(); printf("* codeword domain dimension: %zu\n", this->codeword_domain_dim_);
    print_indent(); printf("* RS extra dimensions: %zu\n", this->RS_extra_dimensions_);
    print_indent(); printf("* absolute proximity parameter: %zu\n", this->absolute_proximity_parameter_);
    print_indent(); printf("* fractional proximity parameter: %Lf\n", this->fractional_proximity_parameter_);
    print_indent(); printf("* number of queries: %zu\n", this->num_queries_);
}

template<typename FieldT>
direct_LDT_protocol<FieldT>::direct_LDT_protocol(iop_protocol<FieldT> &IOP,
                                                 multi_LDT_parameter_base<FieldT> &params,
                                                 const domain_handle &codeword_domain_handle,
                                                 const std::vector<oracle_handle_ptr> &poly_handles) :
multi_LDT_base<FieldT>(IOP, params, codeword_domain_handle, poly_handles)
{
    direct_LDT_parameters<FieldT> *LDT_params = dynamic_cast<direct_LDT_parameters<FieldT>*>(&params);
    if (LDT_params == 0)
    {
        throw std::invalid_argument("parameters passed to direct LDT are not of type direct_LDT_parameters");
    }
    this->params_ = *LDT_params;
    this->codeword_domain_ = this->IOP_.get_domain(this->codeword_domain_handle_);
}

template<typename FieldT>
void direct_LDT_protocol<FieldT>::register_interactions()
{
    /* Ensure we send the coefficients after we've committed to the polynomial */
    this->empty_verifier_message_handle_ = this->IOP_.register_verifier_random_message(0);

    this->prover_coefficients_handles_.resize(this->poly_handles_.size());
    for (size_t i = 0; i < this->poly_handles_.size(); i++)
    {
        this->prover_coefficients_handles_[i] = this->IOP_.register_prover_message(this->params_.poly_degree_bound());
    }
}

template<typename FieldT>
void direct_LDT_protocol<FieldT>::register_queries()
{
    this->query_position_handles_.resize(this->params_.num_queries());
    this->query_handles_.resize(this->params_.num_queries());

    for (std::size_t i = 0; i < this->params_.num_queries(); ++i)
    {
        this->query_position_handles_[i] = this->IOP_.register_random_query_position(this->codeword_domain_handle_);

        this->query_handles_[i] = register_queries_for_same_pos(
            this->IOP_, this->poly_handles_, this->query_position_handles_[i]);
    }
}

template<typename FieldT>
void direct_LDT_protocol<FieldT>::calculate_and_submit_proof()
{
    const std::vector<FieldT> empty_verifier_message =
        this->IOP_.obtain_verifier_random_message(this->empty_verifier_message_handle_);

    /* Send coefficients of every provided polynomial */
    for (size_t i = 0; i < this->poly_handles_.size(); i++)
    {
        std::shared_ptr<std::vector<FieldT>> evaluations = this->IOP_.get_oracle_evaluations(this->poly_handles_[i]);

        /* Get coefficients, and resize it to be the correct size. */
        std::vector<FieldT> poly_coefficients = IFFT_over_field_subset<FieldT>(*evaluations.get(), this->codeword_domain_);
        poly_coefficients.resize(this->params_.poly_degree_bound());

        this->IOP_.submit_prover_message(this->prover_coefficients_handles_[i], std::move(poly_coefficients));
    }

    this->IOP_.signal_prover_round_done();
}

template<typename FieldT>
bool direct_LDT_protocol<FieldT>::verifier_predicate()
{
    for (size_t i = 0; i < this->poly_handles_.size(); i++)
    {
        std::vector<FieldT> poly_coefficients = this->IOP_.receive_prover_message(this->prover_coefficients_handles_[i]);
        polynomial<FieldT> poly(std::move(poly_coefficients));
        for (size_t j = 0; j < this->params_.num_queries(); j++)
        {
            std::size_t position = this->IOP_.obtain_query_position(this->query_position_handles_[j]);
            FieldT evalpoint = this->codeword_domain_.element_by_index(position);

            FieldT predicted = poly.evaluation_at_point(evalpoint);
            FieldT actual = this->IOP_.obtain_query_response(this->query_handles_[j][i]);

            if (actual != predicted)
            {
                return false;
            }
        }
    }

    return true;
}

} // namespace libiop
