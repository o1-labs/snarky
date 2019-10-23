namespace libiop {

template<typename FieldT>
fractal_iop_parameters<FieldT>::fractal_iop_parameters(
    const size_t security_parameter,
    const size_t RS_extra_dimensions,
    const bool make_zk,
    const std::shared_ptr<r1cs_constraint_system<FieldT>> &constraint_system) :
    security_parameter_(security_parameter),
    RS_extra_dimensions_(RS_extra_dimensions),
    make_zk_(make_zk),
    constraint_system_(constraint_system)
{
    /** We require the matrices to be square, and have size that is a power of 2 */
    if (!is_power_of_2(this->constraint_system_->num_constraints()))
    {
        throw std::invalid_argument(
            "Holographic Aurora requires the number of constraints to be a power of two");
    }
    if (this->constraint_system_->num_constraints() != this->constraint_system_->num_variables() + 1)
    {
        throw std::invalid_argument(
            "Holographic Aurora requires the matrices to be square");
    }

    const size_t max_num_nonzero_entries_per_matrix = std::max({
        r1cs_sparse_matrix<FieldT>(this->constraint_system_, r1cs_sparse_matrix_A).num_nonzero_entries(),
        r1cs_sparse_matrix<FieldT>(this->constraint_system_, r1cs_sparse_matrix_B).num_nonzero_entries(),
        r1cs_sparse_matrix<FieldT>(this->constraint_system_, r1cs_sparse_matrix_C).num_nonzero_entries(),
    });
    const size_t index_domain_dim = log2(max_num_nonzero_entries_per_matrix);
    this->index_domain_ = field_subset<FieldT>(1ull << index_domain_dim);
    this->matrix_domain_ = field_subset<FieldT>(this->constraint_system_->num_constraints());
    /** TODO: Calculate max tested degree / constraint degree precisely.
     *  TODO: Make decision w.r.t. rational linear combination */
    const size_t max_tested_degree = 6 * this->index_domain_.num_elements();
    this->codeword_domain_dim_ = log2(max_tested_degree) + this->RS_extra_dimensions_;

    const field_subset<FieldT> unshifted_codeword_domain(1ull << this->codeword_domain_dim_);
    const FieldT codeword_domain_shift = unshifted_codeword_domain.element_outside_of_subset();
    this->codeword_domain_ = field_subset<FieldT>(
        1ull << this->codeword_domain_dim_, codeword_domain_shift);
}

template<typename FieldT>
void fractal_iop_parameters<FieldT>::set_ldt_parameters(
    size_t localization_parameter,
    FRI_soundness_type fri_soundness_type,
    LDT_reducer_soundness_type ldt_reducer_soundness_type)
{
   const std::vector<size_t> localization_parameter_array =
        FRI_protocol_parameters<FieldT>::localization_parameter_to_array(
            localization_parameter,
            this->codeword_domain_.dimension(),
            this->RS_extra_dimensions_);
    this->set_ldt_parameters(localization_parameter_array, fri_soundness_type, ldt_reducer_soundness_type);
}

template<typename FieldT>
void fractal_iop_parameters<FieldT>::set_ldt_parameters(
    std::vector<size_t> localization_parameters,
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
    const bool holographic = true;
    /** Now we need to initialize the reducer parameters, and the FRI parameters.
     *  To initialize these, we need to know the tested and constraint degree bounds.
     *  We currently put everything into a single LDT,
     *  bottlenecked by the rational linear combination.
     */
    /* TODO: Get from elsewhere */
    const size_t max_tested_degree_bound =
        6 * this->index_domain_.num_elements();
    const size_t max_constraint_degree_bound =
        7 * this->index_domain_.num_elements();
    /** Round max tested degree bound to something FRI can test.
     *  The LDT reducer handles shifting rates so that they are all tested at the correct rate. */
    const size_t max_LDT_tested_degree_bound =
        FRI_protocol_parameters<FieldT>::next_testable_degree_bound(max_tested_degree_bound,
                                                                    localization_parameters);

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

    this->query_bound_ = this->FRI_params_.queries_to_input_oracles();
    this->encoded_aurora_params_ = encoded_aurora_parameters<FieldT>(
        interactive_soundness_error_bits,
        this->codeword_domain_dim_,
        this->matrix_domain_.dimension(),
        this->matrix_domain_.dimension(),
        this->query_bound_,
        this->make_zk_,
        holographic,
        this->codeword_domain_.type());
}

template<typename FieldT>
size_t fractal_iop_parameters<FieldT>::RS_extra_dimensions() const {
    return this->RS_extra_dimensions_;
}

template<typename FieldT>
bool fractal_iop_parameters<FieldT>::make_zk() const {
    return this->make_zk_;
}

template<typename FieldT>
std::shared_ptr<r1cs_constraint_system<FieldT>>
    fractal_iop_parameters<FieldT>::constraint_system() const
{
    return this->constraint_system_;
}

template<typename FieldT>
field_subset<FieldT> fractal_iop_parameters<FieldT>::index_domain() const
{
    return this->index_domain_;
}

template<typename FieldT>
field_subset<FieldT> fractal_iop_parameters<FieldT>::matrix_domain() const
{
    return this->matrix_domain_;
}

template<typename FieldT>
field_subset<FieldT> fractal_iop_parameters<FieldT>::codeword_domain() const
{
    return this->codeword_domain_;
}

template<typename FieldT>
std::vector<size_t> fractal_iop_parameters<FieldT>::locality_vector() const {
    std::vector<size_t> protocol_locality = this->encoded_aurora_params_.locality_vector();
    size_t ldt_reducer_locality = this->LDT_reducer_params_.locality();
    const size_t zk_round_index = 1;
    protocol_locality[zk_round_index] += ldt_reducer_locality;
    return protocol_locality;
}

template<typename FieldT>
long double fractal_iop_parameters<FieldT>::achieved_soundness() const{
    const long double query_soundness_bits = this->FRI_params_.achieved_query_soundness();
    const long double fri_interactive_soundness_bits = this->FRI_params_.achieved_interactive_soundness();
    const long double ldt_reducer_interactive_soundness_bits = this->LDT_reducer_params_.achieved_soundness();
    const long double holographic_lincheck_interactive_soundness_bits =
        this->encoded_aurora_params_.holographic_lincheck_params_.achieved_interactive_soundness();
    long double error = 0;
    error += exp2l(-1.0 * query_soundness_bits);
    error += exp2l(-1.0 * fri_interactive_soundness_bits);
    error += exp2l(-1.0 * ldt_reducer_interactive_soundness_bits);
    error += exp2l(-1.0 * holographic_lincheck_interactive_soundness_bits);
    const long double soundness_error_in_bits = -1.0 * log2l(error);
    return soundness_error_in_bits;
}

template<typename FieldT>
void fractal_iop_parameters<FieldT>::print() const
{
    printf("\nFractal hIOP parameters\n");
    print_indent(); printf("* target security parameter = %zu\n", this->security_parameter_);
    print_indent(); printf("* achieved security parameter = %.1Lf\n", this->achieved_soundness());
    print_indent(); printf("* RS extra dimensions = %zu\n", this->RS_extra_dimensions_);
    print_indent(); printf("* matrix domain dim = %zu\n", this->matrix_domain_.dimension());
    print_indent(); printf("* index domain dim = %zu\n", this->index_domain_.dimension());
    print_indent(); printf("* codeword domain dim = %zu\n", this->codeword_domain_dim_);
    print_indent(); printf("* query bound = %zu\n", this->query_bound_);
    print_indent(); printf("* make zk = %s\n", (this->make_zk_ ? "true" : "false"));
    print_indent(); printf("* domain type = %s\n", field_subset_type_names[this->matrix_domain_.type()]);

    this->encoded_aurora_params_.holographic_lincheck_params_.print();
    this->LDT_reducer_params_.print();
    this->FRI_params_.print();
}

template<typename FieldT>
fractal_iop<FieldT>::fractal_iop(
    iop_protocol<FieldT> &IOP,
    const fractal_iop_parameters<FieldT> &parameters) :
    IOP_(IOP),
    parameters_(parameters)
{
    const field_subset<FieldT> index_domain = parameters.index_domain();
    const field_subset<FieldT> matrix_domain = parameters.matrix_domain();
    const field_subset<FieldT> codeword_domain = parameters.codeword_domain();

    this->index_domain_handle_ = IOP.register_domain(index_domain);
    this->matrix_domain_handle_ = IOP.register_domain(matrix_domain);
    this->codeword_domain_handle_ = IOP.register_domain(codeword_domain);

    /** TODO: This may change as we alter how we LDT OOD index oracles. */
    this->register_index_oracles();
    this->protocol_ = std::make_shared<encoded_aurora_protocol<FieldT> >(
        this->IOP_,
        this->matrix_domain_handle_,
        this->matrix_domain_handle_,
        this->codeword_domain_handle_,
        this->parameters_.constraint_system(),
        parameters.encoded_aurora_params_);
    this->protocol_->set_index_oracles(this->index_domain_handle_, this->indexed_handles_);
    this->LDT_reducer_ = std::make_shared<LDT_instance_reducer<FieldT, FRI_protocol<FieldT>>>(
        this->IOP_,
        this->codeword_domain_handle_,
        this->parameters_.LDT_reducer_params_);
    round_parameters<FieldT> round0_params(this->parameters_.FRI_params_.quotient_map_domain(codeword_domain));
    this->IOP_.set_round_parameters(round0_params);
}

template<typename FieldT>
void fractal_iop<FieldT>::register_index_oracles()
{
    const size_t input_variable_dim = log2(this->parameters_.constraint_system()->num_inputs());
    for (size_t i = 0; i < 3; i++)
    {
        std::shared_ptr<sparse_matrix<FieldT>> M
            = std::make_shared<r1cs_sparse_matrix<FieldT>>(
                this->parameters_.constraint_system(),
                all_r1cs_sparse_matrix_types[i]);
        this->matrix_indexers_.emplace_back(matrix_indexer<FieldT>(
            this->IOP_,
            this->index_domain_handle_,
            this->matrix_domain_handle_,
            this->codeword_domain_handle_,
            input_variable_dim,
            M));
        this->matrix_indexers_[i].register_oracles();
        this->indexed_handles_.emplace_back(
            this->matrix_indexers_[i].get_all_oracle_handles());
    }

    field_subset<FieldT> codeword_domain =
        this->IOP_.get_domain(this->codeword_domain_handle_);
    round_parameters<FieldT> round0_params(
        this->parameters_.FRI_params_.quotient_map_domain(codeword_domain));
    this->IOP_.set_round_parameters(round0_params);
    this->IOP_.signal_index_registrations_done();
}

template<typename FieldT>
void fractal_iop<FieldT>::register_interactions()
{
    field_subset<FieldT> codeword_domain = this->IOP_.get_domain(this->codeword_domain_handle_);
    this->protocol_->register_challenge();
    round_parameters<FieldT> round2_params(this->parameters_.FRI_params_.quotient_map_domain(codeword_domain));
    this->IOP_.set_round_parameters(round2_params);
    this->protocol_->register_proof();
    round_parameters<FieldT> round3_params(this->parameters_.FRI_params_.quotient_map_domain(codeword_domain));
    this->IOP_.set_round_parameters(round3_params);
    // LDT initialization must come after register_proof
    const std::vector<oracle_handle_ptr> oracle_handle_ptrs = this->protocol_->get_all_oracle_handles();

    std::shared_ptr<multi_LDT_parameter_base<FieldT>> LDT_params =
        std::make_shared<FRI_protocol_parameters<FieldT>>(this->parameters_.FRI_params_);
    this->LDT_reducer_->set_LDT_params(LDT_params);
    this->LDT_reducer_->register_interactions(oracle_handle_ptrs);
}

template<typename FieldT>
void fractal_iop<FieldT>::register_queries()
{
    this->LDT_reducer_->register_queries();
}

template<typename FieldT>
void fractal_iop<FieldT>::produce_index()
{
    for (size_t i = 0; i < this->matrix_indexers_.size(); i++)
    {
        this->matrix_indexers_[i].compute_oracles();
    }
    this->IOP_.signal_index_submissions_done();
}

template<typename FieldT>
void fractal_iop<FieldT>::produce_proof(
    const r1cs_primary_input<FieldT> &primary_input,
    const r1cs_auxiliary_input<FieldT> &auxiliary_input,
    iop_prover_index<FieldT> &index)
{
    this->IOP_.submit_prover_index(index);
    this->protocol_->submit_witness_oracles(primary_input, auxiliary_input);
    this->LDT_reducer_->submit_masking_polynomial();
    this->IOP_.signal_prover_round_done();
    this->protocol_->calculate_and_submit_proof();
    this->IOP_.signal_prover_round_done(); /* LDT will send a challenge */
    this->LDT_reducer_->calculate_and_submit_proof(); /* and signal done internally */
}

template<typename FieldT>
bool fractal_iop<FieldT>::verifier_predicate(const r1cs_primary_input<FieldT> &primary_input)
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
