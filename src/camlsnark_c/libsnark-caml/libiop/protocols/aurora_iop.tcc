namespace libiop {

template<typename FieldT>
aurora_iop_parameters<FieldT>::aurora_iop_parameters(const size_t security_parameter,
                                                     const size_t RS_extra_dimensions,
                                                     const bool make_zk,
                                                     const field_subset_type domain_type,
                                                     const size_t num_constraints,
                                                     const size_t num_variables) :
    security_parameter_(security_parameter),
    RS_extra_dimensions_(RS_extra_dimensions),
    make_zk_(make_zk),
    domain_type_(domain_type)
{
    this->constraint_domain_dim_ = log2(num_constraints);
    this->variable_domain_dim_ = log2(num_variables + 1);
    this->summation_domain_dim_ = std::max<size_t>(this->constraint_domain_dim_, this->variable_domain_dim_);
    /* Extra dimensions required due to constructed oracles requiring larger rate. */
    this->extra_systematic_dims_ = (this->make_zk_ ? 2 : 0);
    this->codeword_domain_dim_ =
        this->summation_domain_dim_ + extra_systematic_dims_ + this->RS_extra_dimensions_;
}

template<typename FieldT>
void aurora_iop_parameters<FieldT>::set_ldt_parameters(size_t localization_parameter,
                                                       FRI_soundness_type fri_soundness_type,
                                                       LDT_reducer_soundness_type ldt_reducer_soundness_type)
{
   const std::vector<size_t> localization_parameter_array =
        FRI_protocol_parameters<FieldT>::localization_parameter_to_array(
            localization_parameter,
            this->codeword_domain_dim_,
            this->RS_extra_dimensions_);
    this->set_ldt_parameters(localization_parameter_array, fri_soundness_type, ldt_reducer_soundness_type);
}

template<typename FieldT>
void aurora_iop_parameters<FieldT>::set_ldt_parameters(std::vector<size_t> localization_parameters,
                                                       FRI_soundness_type fri_soundness_type,
                                                       LDT_reducer_soundness_type ldt_reducer_soundness_type)
{
    /** We have to parameterize everything such that:
     *     query soundness error + interactive soundness error < 2^{-security_parameter}
     *  We allocate an equal amount of error to each subcomponent, (2^{-security parameter - 1}).
     *  The interactive soundness has to be further sub-divided into 3 components:
     *    interactive sounndness error for the encoded Aurora protocol, LDT instance reducer, and FRI.
     *  The error we allocate to each of these components is (2^{-security parameter - 3}),
     *  since 3(2^{-security parameter - 3}) < 2^{-security parameter - 1}.
     *  Eventually we should put a larger portion of the interactive soundness error onto the encoded Aurora protocol,
     *  and less so on the LDT components.
     */
    const size_t query_soundness_error_bits = this->security_parameter_ + 1;
    const size_t interactive_soundness_error_bits = this->security_parameter_ + 3;
    /** Now we need to initialize the reducer parameters, and the FRI parameters.
     *  To initialize these, we need to know the tested and constraint degree bounds.
     *  In the non-zk case, we can compute these directly.
     *  In the zk case, the degree bound is dependent on FRI's parameterization.
     */
    if(!this->make_zk_) {
        this->query_bound_ = 0;
        this->encoded_aurora_params_ = encoded_aurora_parameters<FieldT>(
            interactive_soundness_error_bits,
            this->codeword_domain_dim_,
            this->constraint_domain_dim_,
            this->summation_domain_dim_,
            this->query_bound_,
            this->make_zk_,
            false,
            this->domain_type_);
        const size_t max_tested_degree_bound =
            this->encoded_aurora_params_.max_tested_degree_bound();
        const size_t max_constraint_degree_bound =
            this->encoded_aurora_params_.max_constraint_degree_bound();
        this->LDT_reducer_params_ = LDT_instance_reducer_params<FieldT>(
            interactive_soundness_error_bits,
            ldt_reducer_soundness_type,
            this->codeword_domain_dim_,
            max_tested_degree_bound,
            max_constraint_degree_bound,
            this->make_zk_);
        this->FRI_params_ = FRI_protocol_parameters<FieldT>(
            interactive_soundness_error_bits,
            query_soundness_error_bits,
            fri_soundness_type,
            max_tested_degree_bound,
            this->codeword_domain_dim_,
            this->RS_extra_dimensions_,
            this->LDT_reducer_params_.absolute_proximity_parameter(),
            localization_parameters);
    } else if (this->make_zk_) {
        /** To initialize the parameters optimally in the zk case, we need to know the exact degree bounds.
         *  However, the degree bounds depend on the query bound, which we only know after FRI is parameterized.
         *  FRI needs the proximity parameter, which depends on the LDT reducer, which requires the degree bounds.
         *  To handle this cyclicity, we first estimate query bound as 0,
         *  and parameterize the LDT reducer and FRI according to the estimate.
         *  We then obtain a query bound from FRI (number of queries to input oracles + 1),
         *  and we repeat the parameterization with this new query bound,
         *  since that will cause a larger rate and therefore require a smaller proximity parameter.
         *  We keep repeating until the query bound stops increasing.
         */
        size_t estimated_num_queries = 0;
        size_t last_num_queries = 1; /* Just has to be different from estimate */
        while (estimated_num_queries != last_num_queries)
        {
            const size_t query_bound = estimated_num_queries + 1;
            this->encoded_aurora_params_ = encoded_aurora_parameters<FieldT>(
                interactive_soundness_error_bits,
                this->codeword_domain_dim_,
                this->constraint_domain_dim_,
                this->summation_domain_dim_,
                query_bound,
                this->make_zk_,
                false,
                this->domain_type_);
            const size_t max_tested_degree_bound = this->encoded_aurora_params_.max_tested_degree_bound();
            const size_t max_constraint_degree_bound = this->encoded_aurora_params_.max_constraint_degree_bound();
            /** Round max tested degree bound to something FRI can test.
             *  The LDT reducer handles shifting rates so that they are all tested at the correct rate. */
            const size_t max_LDT_tested_degree_bound =
                FRI_protocol_parameters<FieldT>::next_testable_degree_bound(max_tested_degree_bound,
                                                                            localization_parameters);

            /** We need that max_LDT_tested_degree_bound * (1 << RS_extra_dimensions) <= codeword domain size,
             *  if this isn't the case, we make the codeword domain bigger.
             *  We check this as:
             *      max_LDT_tested_degree > 2^(codeword domain dimension - RS extra dimensions)
             *  max_LDT_tested_degree is solved for by going from smallest query size to actual query size,
             *  so this shouldn't cause us to go to a larger domain size than necessary. */
            if (max_LDT_tested_degree_bound > 1ull << (this->codeword_domain_dim_ - this->RS_extra_dimensions_))
            {
                printf("Query bound is too large for codeword domain dimension %lu, \
increasing codeword domain dimension\n",
                    this->codeword_domain_dim_);
                this->codeword_domain_dim_ += 1;
                this->set_ldt_parameters(localization_parameters, fri_soundness_type, ldt_reducer_soundness_type);
                /* Break the recursion */
                return;
            }
            this->LDT_reducer_params_ = LDT_instance_reducer_params<FieldT>(
                interactive_soundness_error_bits,
                ldt_reducer_soundness_type,
                this->codeword_domain_dim_,
                max_LDT_tested_degree_bound,
                max_constraint_degree_bound,
                this->make_zk_);
            this->FRI_params_ = FRI_protocol_parameters<FieldT>(
                interactive_soundness_error_bits,
                query_soundness_error_bits,
                fri_soundness_type,
                max_LDT_tested_degree_bound,
                this->codeword_domain_dim_,
                this->RS_extra_dimensions_,
                this->LDT_reducer_params_.absolute_proximity_parameter(),
                localization_parameters);
            last_num_queries = estimated_num_queries;
            estimated_num_queries = this->FRI_params_.queries_to_input_oracles();
        }
        this->query_bound_ = estimated_num_queries + 1;
    }
}

template<typename FieldT>
size_t aurora_iop_parameters<FieldT>::RS_extra_dimensions() const {
    return this->RS_extra_dimensions_;
}

template<typename FieldT>
bool aurora_iop_parameters<FieldT>::make_zk() const {
    return this->make_zk_;
}

template<typename FieldT>
field_subset_type aurora_iop_parameters<FieldT>::domain_type() const {
    return this->domain_type_;
}

template<typename FieldT>
size_t aurora_iop_parameters<FieldT>::query_bound() const {
    return this->query_bound_;
}

template<typename FieldT>
std::vector<size_t> aurora_iop_parameters<FieldT>::locality_vector() const {
    std::vector<size_t> protocol_locality = this->encoded_aurora_params_.locality_vector();
    size_t ldt_reducer_locality = this->LDT_reducer_params_.locality();
    const size_t zk_round_index = 0;
    protocol_locality[zk_round_index] += ldt_reducer_locality;
    return protocol_locality;
}

template<typename FieldT>
size_t aurora_iop_parameters<FieldT>::constraint_domain_dim() const {
    return this->constraint_domain_dim_;
}
template<typename FieldT>
size_t aurora_iop_parameters<FieldT>::variable_domain_dim() const {
    return this->variable_domain_dim_;
}
template<typename FieldT>
size_t aurora_iop_parameters<FieldT>::codeword_domain_dim() const {
    return this->codeword_domain_dim_;
}
template<typename FieldT>
long double aurora_iop_parameters<FieldT>::achieved_soundness() const{
    const long double query_soundness_bits = this->FRI_params_.achieved_query_soundness();
    const long double fri_interactive_soundness_bits = this->FRI_params_.achieved_interactive_soundness();
    const long double ldt_reducer_interactive_soundness_bits = this->LDT_reducer_params_.achieved_soundness();
    const long double multi_lincheck_interactive_soundness_bits =
        this->encoded_aurora_params_.multi_lincheck_params_.achieved_interactive_soundness();
    long double error = 0;
    error += exp2l(-1.0 * query_soundness_bits);
    error += exp2l(-1.0 * fri_interactive_soundness_bits);
    error += exp2l(-1.0 * ldt_reducer_interactive_soundness_bits);
    error += exp2l(-1.0 * multi_lincheck_interactive_soundness_bits);
    const long double soundness_error_in_bits = -1.0 * log2l(error);
    return soundness_error_in_bits;
}
template<typename FieldT>
void aurora_iop_parameters<FieldT>::print() const
{
    printf("\nAurora IOP parameters\n");
    print_indent(); printf("* target security parameter = %zu\n", this->security_parameter_);
    print_indent(); printf("* achieved security parameter = %.1Lf\n", this->achieved_soundness());
    print_indent(); printf("* RS extra dimensions = %zu\n", this->RS_extra_dimensions_);
    print_indent(); printf("* codeword domain dim = %zu\n", this->codeword_domain_dim_);
    print_indent(); printf("* constraint domain dim = %zu\n", this->constraint_domain_dim_);
    print_indent(); printf("* variable domain dim = %zu\n", this->variable_domain_dim_);
    print_indent(); printf("* query bound = %zu\n", this->query_bound_);
    print_indent(); printf("* make zk = %s\n", (this->make_zk_ ? "true" : "false"));
    print_indent(); printf("* domain type = %s\n", field_subset_type_names[this->domain_type_]);
    this->encoded_aurora_params_.multi_lincheck_params_.print();
    this->LDT_reducer_params_.print();
    this->FRI_params_.print();
}

template<typename FieldT>
aurora_iop<FieldT>::aurora_iop(iop_protocol<FieldT> &IOP,
                               const r1cs_constraint_system<FieldT> &constraint_system,
                               const aurora_iop_parameters<FieldT> &parameters) :
    IOP_(IOP),
    constraint_system_(constraint_system),
    parameters_(parameters)
{
    /** Choosing the affine shift for the codeword domain relies
     *  on the default domains being subsets of one another.
     *  To choose the shift, we take a domain of the same size as the codeword domain,
     *  take an element outside of the subset, and make that the shift. */
    const field_subset<FieldT> unshifted_codeword_domain(1ull << parameters.codeword_domain_dim());
    const FieldT codeword_domain_shift = unshifted_codeword_domain.element_outside_of_subset();

    const field_subset<FieldT> constraint_domain(1ull << parameters.constraint_domain_dim());
    const field_subset<FieldT> variable_domain(1ull << parameters.variable_domain_dim());
    const field_subset<FieldT> codeword_domain(1ull << parameters.codeword_domain_dim(), codeword_domain_shift);

    const domain_handle constraint_domain_handle = IOP.register_domain(constraint_domain);
    const domain_handle variable_domain_handle = IOP.register_domain(variable_domain);
    this->codeword_domain_handle_ = IOP.register_domain(codeword_domain);

    std::shared_ptr<r1cs_constraint_system<FieldT> > cs =
            std::make_shared<r1cs_constraint_system<FieldT> >(constraint_system);

    this->protocol_ = std::make_shared<encoded_aurora_protocol<FieldT> >(
        this->IOP_,
        constraint_domain_handle,
        variable_domain_handle,
        this->codeword_domain_handle_,
        cs,
        parameters.encoded_aurora_params_);
    this->LDT_reducer_ = std::make_shared<LDT_instance_reducer<FieldT, FRI_protocol<FieldT> > >(
        this->IOP_,
        this->codeword_domain_handle_,
        this->parameters_.LDT_reducer_params_);
    round_parameters<FieldT> round1_params(this->parameters_.FRI_params_.quotient_map_domain(codeword_domain));
    this->IOP_.set_round_parameters(round1_params);
}

template<typename FieldT>
void aurora_iop<FieldT>::register_interactions()
{
    this->protocol_->register_challenge();
    this->protocol_->register_proof();
    field_subset<FieldT> codeword_domain = this->IOP_.get_domain(this->codeword_domain_handle_);
    round_parameters<FieldT> round2_params(this->parameters_.FRI_params_.quotient_map_domain(codeword_domain));
    this->IOP_.set_round_parameters(round2_params);
    // LDT initialization must come after register_proof
    const std::vector<oracle_handle_ptr> oracle_handle_ptrs = this->protocol_->get_all_oracle_handles();

    std::shared_ptr<multi_LDT_parameter_base<FieldT>> LDT_params =
        std::make_shared<FRI_protocol_parameters<FieldT>>(this->parameters_.FRI_params_);
    this->LDT_reducer_->set_LDT_params(LDT_params);
    this->LDT_reducer_->register_interactions(oracle_handle_ptrs);
}

template<typename FieldT>
void aurora_iop<FieldT>::register_queries()
{
    this->LDT_reducer_->register_queries();
}

template<typename FieldT>
void aurora_iop<FieldT>::produce_proof(const r1cs_primary_input<FieldT> &primary_input,
                                       const r1cs_auxiliary_input<FieldT> &auxiliary_input)
{
    this->protocol_->submit_witness_oracles(primary_input, auxiliary_input);
    this->LDT_reducer_->submit_masking_polynomial();
    this->IOP_.signal_prover_round_done();
    this->protocol_->calculate_and_submit_proof();
    this->IOP_.signal_prover_round_done(); /* LDT will send a challenge */
    this->LDT_reducer_->calculate_and_submit_proof(); /* and signal done internally */
}

template<typename FieldT>
bool aurora_iop<FieldT>::verifier_predicate(const r1cs_primary_input<FieldT> &primary_input)
{
    enter_block("Construct R1CS verifier state");
    this->protocol_->construct_verifier_state(primary_input);
    leave_block("Construct R1CS verifier state");

    enter_block("Check LDT verifier predicate");
    const bool decision = this->LDT_reducer_->verifier_predicate();
    leave_block("Check LDT verifier predicate");

    return decision;
}

} // libiop
