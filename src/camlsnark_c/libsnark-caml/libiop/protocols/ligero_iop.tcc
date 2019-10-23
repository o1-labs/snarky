namespace libiop {

template<typename FieldT>
ligero_iop_parameters<FieldT>::ligero_iop_parameters(const size_t security_parameter,
                                                     const LDT_reducer_soundness_type soundness_type,
                                                     const size_t RS_extra_dimensions,
                                                     const float height_width_ratio,
                                                     const bool make_zk,
                                                     const field_subset_type domain_type,
                                                     const size_t num_constraints,
                                                     const size_t num_variables) :
    security_parameter_(security_parameter),
    soundness_type_(soundness_type),
    RS_extra_dimensions_(RS_extra_dimensions),
    height_width_ratio_(height_width_ratio),
    make_zk_(make_zk),
    domain_type_(domain_type),
    num_constraints_(num_constraints),
    num_variables_(num_variables)
{
    size_t num_vars = num_variables + 1;
    size_t systematic_domain_size = (std::size_t) ceil(sqrt(num_vars / this->height_width_ratio_));
    systematic_domain_size = round_to_next_power_of_2(systematic_domain_size);
    this->systematic_domain_dim_ = log2(systematic_domain_size);
    this->codeword_domain_dim_ = this->systematic_domain_dim_ + this->RS_extra_dimensions_;
    this->set_soundness_parameters();
}

template<typename FieldT>
void ligero_iop_parameters<FieldT>::set_soundness_parameters()
{
    /** Ligero has four sources of soundness error.
     *  There is query error from the direct LDT, interactive error from the LDT reducer,
     *  interactive error from the encoded protocol, and query error from the encoded protocol.
     *  We allocate error for each of these components equally, namely we allocate
     *  2^{-security_parameter - 2}.
     *
     *  In Ligero, the encoded protocol's query soundness error depends on the proximity parameter.
     *  Due to this, we initialize the ldt reducer first, and we use the proximity parameter
     *  in the encoded protocol's configuration.
     *  More precisely, we use:
     *      min(ldt_reducer_proximity_parameter, (1 - p) / 4))
     *  as it must be less than the latter term per the paper
     */

    const size_t interactive_soundness_bits = this->security_parameter_ + 2;
    const size_t query_soundness_bits = this->security_parameter_ + 2;

    /** While larger domains are used within Ligero, those are not low degree tested.
     *  Rational constraints are not used either, so constraint_degree_bound = tested_degree_bound. */
    const size_t systematic_domain_size = 1ull << this->systematic_domain_dim_;
    const size_t max_tested_degree_bound = systematic_domain_size;
    const size_t max_constraint_degree_bound = max_tested_degree_bound;
    this->ldt_reducer_params_ = LDT_instance_reducer_params<FieldT>(
            interactive_soundness_bits,
            this->soundness_type_,
            this->codeword_domain_dim_,
            max_tested_degree_bound,
            max_constraint_degree_bound,
            this->make_zk_);
    /** TODO: We should make an encoded ligero parameter classs. */
    // TODO: We currently don't tell encoded ligero the solved query bound,
    //       though we solve for the correct query bound and queries within set_queries.
    //       There is a bug in the encoded ligero codebase when sampling polynomials
    //       that are b-wise independent, for too large of a b. (encoding independence parameter)
    //       We plan on addressing this soon, however we wanted to get the library online.
    //       However this does not affect reported argument size.
    this->configure_encoded_ligero_params(this->num_variables_, this->num_constraints_);
    this->set_encoded_ligero_interactions(interactive_soundness_bits);
    /** Parameterize direct LDT and RS-encoded Ligero's queries. */
    this->set_queries(query_soundness_bits, max_tested_degree_bound);
}

template<typename FieldT>
void ligero_iop_parameters<FieldT>::set_encoded_ligero_interactions(const size_t interactive_soundness_bits)
{
    /** Encoded protocol's interactive soundness error is (1 / F)^(num_interactive_repetitions)
     *  Consequently the bits of security is:
     *      -Interactive_soundness_bits = (num_interactive_repetitions) * log(1 / |F|)
     *  Rearranging, we get:
     *      num_interactive_repetitions = ceil(-Interactive_soundness_bits / log(1 / |F|))
     *  We compute the logarithm using the identity:
     *      log(1 / |F|) = -log(|F|)
     */
    const long double field_size_bits = (long double)(soundness_log_of_field_size_helper<FieldT>(FieldT::zero()));
    const long double denominator = -field_size_bits;
    const long double num_interactive_repetitions = ceil(-1.0 * interactive_soundness_bits / denominator);
    this->encoded_ligero_params_.num_interaction_phase_repetitions_ =
        std::max<size_t>(1, size_t(num_interactive_repetitions));
}

template<typename FieldT>
void ligero_iop_parameters<FieldT>::set_queries(const size_t query_soundness_bits,
                                                const size_t max_tested_degree_bound)
{
    /** Query soundness error is (query_error + fractional_proximity_parameter)^(num_query_repetitions)
     *  Where query_error is:
     *      query_error = (2H + 2b - 2) / L
     *  and where fractional_proximity_parameter is:
     *      <= min(LDT reducer output, (1 - (2H + 2b - 1) / L) / 4)
     *  Consequently the bits of security is:
     *      -Query_soundness_bits = (num_query_repetitions) * log(query_error + fractional_proximity_parameter)
     *  Rearranging, we get:
     *      num_query_repetitions = ceil(-Query_soundness_bits / log(query_error + fractional_proximity_parameter))
     */

    const long double codeword_domain_size = (long double)(1ull << this->codeword_domain_dim_);
    if (!this->make_zk_) {
        const size_t query_bound = 0;
        const size_t query_error_numerator = 2*(1ull << this->systematic_domain_dim_) - 2;
        const long double query_error = ((long double) query_error_numerator) / codeword_domain_size;
        this->calculate_encoded_ligero_proximity_parameters(query_bound);
        this->direct_ldt_params_ = direct_LDT_parameters<FieldT>(
            query_soundness_bits,
            max_tested_degree_bound,
            this->RS_extra_dimensions_,
            this->absolute_encoded_ligero_proximity_parameter_);

        const long double error_per_query = log2l(query_error + this->fractional_encoded_ligero_proximity_parameter_);
        const long double num_query_repetitions = ceil(-1.0 * query_soundness_bits / error_per_query);
        this->encoded_ligero_params_.num_query_phase_repetitions_ =
            std::max<size_t>(1, size_t(num_query_repetitions));
    } else if(this->make_zk_) {
        /** To find the query error in the zk case, we need to know the exact degree bounds.
         *  The degree bound depends on the query bound
         *  However the query bound depends on the encoded ligero queries, which depends on the query error.
         *
         *  To resolve this cyclicity, we first estimate the query bound as direct_ldt.num_queries() + 1,
         *  see the number of queries encoded Ligero uses, and update the estimate.
         *  We keep on doing this estimate update routine until we get something idempotent,
         *  where the calculated number of queries is the same as the estimate.
         */
        size_t estimated_num_queries = 1;
        size_t last_num_queries = 0; /* Just has to be different from estimate */
        while (estimated_num_queries != last_num_queries)
        {
            const size_t query_bound = estimated_num_queries + 1;
            const size_t query_error_numerator = 2*(1ull << this->systematic_domain_dim_)+ 2*query_bound - 2;
            const size_t max_poly_degree = query_error_numerator + 1;
            if (max_poly_degree > (1ull << this->codeword_domain_dim_))
            {
                printf("Query bound is too large for codeword domain dimension %lu, \
increasing codeword domain dimension\n", this->codeword_domain_dim_);
                this->RS_extra_dimensions_ += 1;
                this->codeword_domain_dim_ += 1;
                this->set_soundness_parameters();
                return;
            }
            /** \epsilon_q in the paper */
            const long double query_error = ((long double) query_error_numerator) / codeword_domain_size;
            this->calculate_encoded_ligero_proximity_parameters(query_bound);
            this->direct_ldt_params_ = direct_LDT_parameters<FieldT>(
                query_soundness_bits,
                max_tested_degree_bound,
                this->RS_extra_dimensions_,
                this->absolute_encoded_ligero_proximity_parameter_);

            const long double error_per_query = log2l(query_error + this->fractional_encoded_ligero_proximity_parameter_);
            const long double num_query_repetitions = ceil(-1.0 * query_soundness_bits / error_per_query);
            estimated_num_queries = last_num_queries;
            last_num_queries = std::max<size_t>(1, size_t(num_query_repetitions)) + this->direct_ldt_params_.num_queries();
        }
        this->encoded_ligero_params_.num_query_phase_repetitions_ =
            std::max<size_t>(1, size_t(estimated_num_queries)) - this->direct_ldt_params_.num_queries();
        // TODO: See above note w.r.t. to passing query bound to encoded ligero.
        this->query_bound_ = estimated_num_queries + 1;
    }
}

template<typename FieldT>
void ligero_iop_parameters<FieldT>::calculate_encoded_ligero_proximity_parameters(const size_t query_bound)
{
    /** min_absolute_proximity_parameter = ((L - 2H -2b + 1) / 4) - 1,
     *  the final -1 coming from the strict less than requirement. */
    const size_t min_absolute_proximity_parameter =
        ((1ull << this->codeword_domain_dim_) - 2*(1ull << this->systematic_domain_dim_) - 2*query_bound + 1) / 4
        - 1;
    if (this->ldt_reducer_params_.absolute_proximity_parameter() <= min_absolute_proximity_parameter)
    {
        this->absolute_encoded_ligero_proximity_parameter_ =
            this->ldt_reducer_params_.absolute_proximity_parameter();
    }
    else
    {
        this->absolute_encoded_ligero_proximity_parameter_ = min_absolute_proximity_parameter;
    }
    const long double codeword_domain_size = (long double)(1ull << this->codeword_domain_dim_);
    this->fractional_encoded_ligero_proximity_parameter_ =
        (long double)(this->absolute_encoded_ligero_proximity_parameter_) / codeword_domain_size;
}

template<typename FieldT>
void ligero_iop_parameters<FieldT>::configure_encoded_ligero_params(const size_t num_variables,
                                                                    const size_t num_constraints)
{
    size_t num_vars = num_variables + 1;
    const size_t systematic_domain_size = 1ull << this->systematic_domain_dim_;
    this->num_oracles_input_ = (size_t) ceil(((float) num_vars) / systematic_domain_size);
    std::size_t matrix_width = systematic_domain_size * this->num_oracles_input_;

    std::size_t matrix_height = num_constraints;
    if (matrix_height % systematic_domain_size != 0)
    {
        matrix_height += systematic_domain_size - matrix_height % systematic_domain_size;
    }
    this->num_oracle_vectors_ = matrix_height / systematic_domain_size;

    this->encoded_ligero_params_.make_zk_ = this->make_zk_;
    this->encoded_ligero_params_.field_subset_type_ = this->domain_type_;
    this->encoded_ligero_params_.matrix_width_ = matrix_width;
    this->encoded_ligero_params_.matrix_height_ = matrix_height;
    this->encoded_ligero_params_.num_oracles_input_ = this->num_oracles_input_;
    this->encoded_ligero_params_.num_oracles_vectors_ = this->num_oracle_vectors_;
}

template<typename FieldT>
size_t ligero_iop_parameters<FieldT>::systematic_domain_dim() const
{
    return this->systematic_domain_dim_;
}
template<typename FieldT>
size_t ligero_iop_parameters<FieldT>::RS_extra_dimensions() const
{
    return this->RS_extra_dimensions_;
}
template<typename FieldT>
bool ligero_iop_parameters<FieldT>::make_zk() const
{
    return this->make_zk_;
}
template<typename FieldT>
field_subset_type ligero_iop_parameters<FieldT>::domain_type() const
{
    return this->domain_type_;
}

template<typename FieldT>
long double ligero_iop_parameters<FieldT>::achieved_encoded_ligero_interactive_soundness_error() const
{
    const long double field_size_bits = (long double)(soundness_log_of_field_size_helper<FieldT>(FieldT::zero()));
    const long double soundness_bits_per_instance = -field_size_bits;
    const long double casted_num_interactive_repetitions =
        (long double)(this->encoded_ligero_params_.num_interaction_phase_repetitions_);
    return -soundness_bits_per_instance * casted_num_interactive_repetitions;
}

template<typename FieldT>
long double ligero_iop_parameters<FieldT>::achieved_encoded_ligero_query_soundness_error() const
{
    const size_t query_error_numerator = 2*(1ull << this->systematic_domain_dim_)+ 2*this->query_bound_ - 2;
    const long double codeword_domain_size = (long double)(1ull << this->codeword_domain_dim_);
    /** \epsilon_q in the paper */
    const long double query_error = ((long double) query_error_numerator) / codeword_domain_size;
    const long double error_per_query = log2l(query_error + this->fractional_encoded_ligero_proximity_parameter_);
    const long double casted_num_queries =
        (long double)(this->encoded_ligero_params_.num_query_phase_repetitions_);
    return -error_per_query * casted_num_queries;
}

template<typename FieldT>
long double ligero_iop_parameters<FieldT>::achieved_soundness() const{
    const long double query_soundness_bits = this->direct_ldt_params_.achieved_query_soundness();
    const long double ldt_reducer_interactive_soundness_bits = this->ldt_reducer_params_.achieved_soundness();
    const long double ligero_interactive_soundness_bits =
        this->achieved_encoded_ligero_interactive_soundness_error();
    const long double ligero_query_soundness_bits =
        this->achieved_encoded_ligero_query_soundness_error();
    long double error = 0;
    error += exp2l(-1.0 * query_soundness_bits);
    error += exp2l(-1.0 * ldt_reducer_interactive_soundness_bits);
    error += exp2l(-1.0 * ligero_interactive_soundness_bits);
    error += exp2l(-1.0 * ligero_query_soundness_bits);
    const long double soundness_error_in_bits = -1.0 * log2l(error);
    return soundness_error_in_bits;
}

template<typename FieldT>
void ligero_iop_parameters<FieldT>::print() const
{
    printf("\nLigero IOP parameters\n");
    print_indent(); printf("* target security parameter = %zu\n", this->security_parameter_);
    print_indent(); printf("* achieved security parameter = %.1Lf\n", this->achieved_soundness());
    print_indent(); printf("* encoded ligero interactions = %lu\n",
        this->encoded_ligero_params_.num_interaction_phase_repetitions_);
    print_indent(); printf("* encoded ligero queries = %lu\n",
        this->encoded_ligero_params_.num_query_phase_repetitions_);
    print_indent(); printf("* encoded ligero absolute proximity parameter = %lu\n",
        this->absolute_encoded_ligero_proximity_parameter_);
    print_indent(); printf("* encoded ligero fractional proximity parameter = %Lf\n",
        this->fractional_encoded_ligero_proximity_parameter_);
    print_indent(); printf("* RS extra dimensions = %zu\n", this->RS_extra_dimensions_);
    print_indent(); printf("* systematic domain dim = %zu\n", this->systematic_domain_dim_);
    print_indent(); printf("* codeword domain dim = %zu\n", this->codeword_domain_dim_);
    print_indent(); printf("* num oracles for input = %zu\n", this->num_oracles_input_);
    print_indent(); printf("* num oracle vectors = %zu\n", this->num_oracle_vectors_);
    print_indent(); printf("* make zk = %s\n", (this->make_zk_ ? "true" : "false"));
    print_indent(); printf("* domain type = %s\n", field_subset_type_names[this->domain_type_]);
    this->ldt_reducer_params_.print();
    this->direct_ldt_params_.print();
}

template<typename FieldT>
ligero_iop<FieldT>::ligero_iop(iop_protocol<FieldT> &IOP,
                               const r1cs_constraint_system<FieldT> &constraint_system,
                               const ligero_iop_parameters<FieldT> &parameters) :
    IOP_(IOP),
    constraint_system_(constraint_system),
    parameters_(parameters)
{
    const size_t systematic_domain_size = 1ull << parameters.systematic_domain_dim();
    const size_t codeword_domain_dim = parameters.systematic_domain_dim() + this->parameters_.RS_extra_dimensions();
    const size_t codeword_domain_size = 1ull << codeword_domain_dim;

    this->codeword_domain_ = field_subset<FieldT>(codeword_domain_size);
    FieldT systematic_domain_shift = this->codeword_domain_.element_outside_of_subset();

    field_subset<FieldT> systematic_domain(systematic_domain_size, systematic_domain_shift);
    field_subset<FieldT> extended_systematic_domain(systematic_domain_size << 1, systematic_domain_shift);

    domain_handle codeword_domain_handle = this->IOP_.register_domain(this->codeword_domain_);
    domain_handle systematic_domain_handle = this->IOP_.register_domain(systematic_domain);
    domain_handle extended_systematic_domain_handle = this->IOP_.register_domain(extended_systematic_domain);

    this->protocol_ = std::make_shared<interleaved_r1cs_protocol<FieldT> >(this->IOP_,
                                                                           codeword_domain_handle,
                                                                           systematic_domain_handle,
                                                                           extended_systematic_domain_handle,
                                                                           constraint_system,
                                                                           this->parameters_.encoded_ligero_params_);

    this->LDT_reducer_ = std::make_shared<LDT_instance_reducer<FieldT, direct_LDT_protocol<FieldT> > >(
        this->IOP_,
        codeword_domain_handle,
        this->parameters_.ldt_reducer_params_);
}

template<typename FieldT>
void ligero_iop<FieldT>::register_interactions()
{
    this->protocol_->attach_oracles();
    this->protocol_->register_linear_combinations();
    this->protocol_->register_responses();

    std::shared_ptr<multi_LDT_parameter_base<FieldT>> LDT_params =
        std::make_shared<direct_LDT_parameters<FieldT>>(this->parameters_.direct_ldt_params_);
    this->LDT_reducer_->set_LDT_params(LDT_params);

    this->LDT_reducer_->register_interactions(this->protocol_->concatenated_vector_handles());
}

template<typename FieldT>
void ligero_iop<FieldT>::register_queries()
{
    this->protocol_->register_queries();
    this->LDT_reducer_->register_queries();
}

template<typename FieldT>
void ligero_iop<FieldT>::produce_proof(const r1cs_primary_input<FieldT> &primary_input,
                                                          const r1cs_auxiliary_input<FieldT> &auxiliary_input)
{
    this->protocol_->submit_witness_oracles(primary_input, auxiliary_input);
    if (this->parameters_.make_zk())
    {
        this->protocol_->submit_blinding_vector_oracles();
    }
    this->LDT_reducer_->submit_masking_polynomial();

    this->IOP_.signal_prover_round_done();
    this->protocol_->calculate_and_submit_proof(primary_input);
    this->IOP_.signal_prover_round_done();
    this->LDT_reducer_->calculate_and_submit_proof();
}

template<typename FieldT>
bool ligero_iop<FieldT>::verifier_predicate(const r1cs_primary_input<FieldT> &primary_input)
{
    enter_block("Check Interleaved R1CS verifier predicate");
    bool decision = this->protocol_->verifier_predicate(primary_input);
    leave_block("Check Interleaved R1CS verifier predicate");

    enter_block("Check LDT verifier predicate");
    decision &= this->LDT_reducer_->verifier_predicate();
    leave_block("Check LDT verifier predicate");

    return decision;
}

template<typename FieldT>
void ligero_iop<FieldT>::submit_random_blinding_vector(const oracle_handle_ptr &handle)
{
    polynomial<FieldT> random_poly = polynomial<FieldT>::random_polynomial(this->systematic_domain_size_);
    std::vector<FieldT> random_vector = FFT_over_field_subset<FieldT>(random_poly.coefficients(), this->codeword_domain_);
    oracle<FieldT> random_vector_oracle(random_vector);
    this->IOP_.submit_oracle(handle, std::move(random_vector_oracle));
}

} // libiop
