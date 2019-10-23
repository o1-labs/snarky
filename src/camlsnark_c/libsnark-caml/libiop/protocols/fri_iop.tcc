namespace libiop {

template<typename FieldT>
FRI_iop_protocol<FieldT>::FRI_iop_protocol(iop_protocol<FieldT> &IOP,
                                           const std::vector<FieldT> evaluations,
                                           const FRI_iop_protocol_parameters &parameters) :
    IOP_(IOP),
    parameters_(parameters)
{
    const std::size_t codeword_domain_dim = this->parameters_.codeword_domain_dim_;
    const bool make_zk = false;

    const field_subset<FieldT> codeword_domain(1ull << codeword_domain_dim);

    const domain_handle codeword_domain_handle = IOP.register_domain(codeword_domain);
    /* One input oracle, TODO: Make this a parameter for more fine grained instrumentation */
    const size_t oracle_locality = 1;

    this->protocol_ = std::make_shared<dummy_protocol<FieldT> >(this->IOP_,
                                                                oracle_locality,
                                                                parameters.RS_extra_dimensions_,
                                                                codeword_domain_handle,
                                                                make_zk);

    const std::size_t poly_degree_bound = 1ull << (codeword_domain_dim - this->parameters_.RS_extra_dimensions_);

    /** We set the LDT instance reducer parameters to something,
     *  and immediately override it to have one repetition.
     *
     *  Similarly we do not need to worry about the proximity parameter,
     *  as we override the number of query and interactive repetitions on FRI. */
    const size_t dummy_ldt_reducer_soundness_bits = 10;
    LDT_instance_reducer_params<FieldT> LDT_reducer_params(
        dummy_ldt_reducer_soundness_bits,
        LDT_reducer_soundness_type::optimistic_heuristic,
        codeword_domain_dim,
        poly_degree_bound,
        poly_degree_bound,
        make_zk);

    this->LDT_ = std::make_shared<LDT_instance_reducer<FieldT, FRI_protocol<FieldT>>>(
        this->IOP_,
        codeword_domain_handle,
        LDT_reducer_params);

    std::vector<size_t> localization_parameters = parameters.localization_parameter_array_;
    if (parameters.localization_parameter_array_.size() == 0)
    {
        localization_parameters =
            FRI_protocol_parameters<FieldT>::localization_parameter_to_array(
                parameters.localization_parameter_,
                codeword_domain_dim,
                parameters.RS_extra_dimensions_);
    }
    this->IOP_.set_round_parameters(
        round_parameters<FieldT>(field_subset<FieldT>(
            1ull << localization_parameters[0])));

    /** We override this immediately,
     *  and set interactive / query repetitions based on the provided parameters. */
    const size_t dummy_fri_soundness_bits = 10;
    FRI_protocol_parameters<FieldT> FRI_params(
            dummy_fri_soundness_bits,
            dummy_fri_soundness_bits,
            FRI_soundness_type::heuristic,
            poly_degree_bound,
            codeword_domain_dim,
            parameters.RS_extra_dimensions_,
            LDT_reducer_params.absolute_proximity_parameter(),
            localization_parameters);
    FRI_params.override_security_parameters(parameters.num_interactive_repetitions_, parameters.num_query_repetitions_);
    FRI_params.print();
    std::shared_ptr<multi_LDT_parameter_base<FieldT>> shared_LDT_params =
        std::make_shared<FRI_protocol_parameters<FieldT>>(FRI_params);
    this->LDT_->set_LDT_params(shared_LDT_params);
}

template<typename FieldT>
void FRI_iop_protocol<FieldT>::register_interactions()
{
    std::vector<oracle_handle_ptr> oracle_handle_ptrs;
    oracle_handle_ptrs.push_back(this->protocol_->get_oracle_handle());
    this->LDT_->register_interactions(oracle_handle_ptrs);
}

template<typename FieldT>
void FRI_iop_protocol<FieldT>::register_queries()
{
    this->LDT_->register_queries();
}

template<typename FieldT>
void FRI_iop_protocol<FieldT>::produce_proof()
{
    this->protocol_->calculate_and_submit_response();
    this->LDT_->submit_masking_polynomial();
    this->IOP_.signal_prover_round_done();
    this->LDT_->calculate_and_submit_proof(); /* and signal done internally */
}

template<typename FieldT>
bool FRI_iop_protocol<FieldT>::verifier_predicate()
{
    bool decision = this->protocol_->verifier_predicate();

    decision &= this->LDT_->verifier_predicate();

    return decision;
}

} // libiop
