namespace libiop {

template<typename FieldT>
basic_lincheck_parameters<FieldT>::basic_lincheck_parameters(
    const size_t interactive_security_parameter,
    const size_t constraint_domain_dim,
    const bool make_zk,
    const field_subset_type domain_type) :
    interactive_security_parameter_(interactive_security_parameter),
    constraint_domain_dim_(constraint_domain_dim),
    make_zk_(make_zk),
    domain_type_(domain_type)
{
    /** Lincheck's soundness error in the Aurora paper is shown to be less than:
     *      ((constraint_domain_size) / |F|) + sumcheck's soundness error
     *  The error can in fact be written as less than or equal to:
     *      ((constraint_domain_size - 1) / |F|) + sumcheck's soundness error
     *  This can be derived from the following minor observation on the lincheck's soundness analysis:
     *      The "sum of q_alpha" polynomial (denoted in the paper as h)'s degree in alpha is
     *      (constraint domain size - 1), since the ordering is zero indexed.
     *      Therefore the number of zero's of the polynomial is at most (constraint domain - 1),
     *      thereby yielding that its less than or equal to (constraint_domain_size - 1) / |F|.
     *
     *  Multi lincheck runs n different lincheck instances,
     *  but reduces them to a single sumcheck instance, as noted in lincheck.md.
     *  While it takes a computational shortcut, the output is the same as though it
     *  produced n different lincheck codewords, and ran batch sumcheck.
     *  The total error for a single repetition is:
     *      (constraint_domain_size - 1) / |F| + batch sumcheck's soundness error
     *  Where batch sumcheck's soundness error is the error of a random linear combination, namely:
     *      1 / |F|
     *  Therefore the overall error per repetition is:
     *      constraint_domain_size / |F|
     *
     *  Some intuition for the error only increasing by 1/|F| for multi lincheck:
     *      If a single lincheck is unsatisfied, its sum will be non-zero with probability 1 - soundness error
     *      If multiple linchecks are unsatisfied, then the probability that at least one of their sums is non-zero
     *          is less than or equal to 1 - soundness error.
     *      If at least one lincheck has a non-zero sum,
     *          the result of the random linear combination of all the sums will also be non-zero
     *          with probability (1 - 1/|F|), per the properties of the random linear combination.
     *
     *  The overall soundness error for multi lincheck is:
     *      (constraint_domain_size / |F|)^{multi lincheck repetitions}
     *  Consequently the bits of security is:
     *      -Interactive_soundness_bits = (multi lincheck repetitions) * log(constraint_domain_size / |F|)
     *  Rearranging for the number of repetitions, we get:
     *      multi lincheck repetitions = ceil(-Interactive_soundness_bits / log(constraint_domain_size / |F|))
     *  We compute the denominator as:
     *      log(constraint_domain_size / |F|) = constraint_domain_dim - log(|F|)
     */
    const long double field_size_bits = (long double)(soundness_log_of_field_size_helper<FieldT>(FieldT::zero()));
    const long double denominator = (long double)(this->constraint_domain_dim_) - field_size_bits;
    const long double multi_lincheck_repetitions =
        ceil(-1.0 * (long double)(this->interactive_security_parameter_) / denominator);
    this->multi_lincheck_repetitions_ = std::max<size_t>(1, size_t(multi_lincheck_repetitions));
}

template<typename FieldT>
bool basic_lincheck_parameters<FieldT>::make_zk() const
{
    return this->make_zk_;
}

template<typename FieldT>
field_subset_type basic_lincheck_parameters<FieldT>::domain_type() const
{
    return this->domain_type_;
}

template<typename FieldT>
size_t basic_lincheck_parameters<FieldT>::multi_lincheck_repetitions() const
{
    return this->multi_lincheck_repetitions_;
}

template<typename FieldT>
void basic_lincheck_parameters<FieldT>::override_security_parameter(const size_t multi_lincheck_repetitions)
{
    this->interactive_security_parameter_ = 0;
    this->override_security_parameter_ = true;
    this->multi_lincheck_repetitions_ = multi_lincheck_repetitions;
}

template<typename FieldT>
size_t basic_lincheck_parameters<FieldT>::locality() const
{
    /** sumcheck h, plus masking polynomial. The g oracle is virtual so it is not included. */
    const size_t sumcheck_locality = 1 + (this->make_zk_ ? 1 : 0);
    /** multi lincheck is currently parameterized at one sumcheck per repetition. */
    return this->multi_lincheck_repetitions_ * sumcheck_locality;
}

template<typename FieldT>
long double basic_lincheck_parameters<FieldT>::achieved_interactive_soundness() const
{
    const long double field_size_bits = (long double)(soundness_log_of_field_size_helper<FieldT>(FieldT::zero()));
    const long double soundness_per_repetition = (long double)(this->constraint_domain_dim_) - field_size_bits;
    return -((long double)this->multi_lincheck_repetitions_) * soundness_per_repetition;
}


template<typename FieldT>
void basic_lincheck_parameters<FieldT>::print() const
{
    printf("\nMulti lincheck parameters\n");
    if (this->override_security_parameter_)
    {
        print_indent(); printf("===WARNING=== Multi lincheck security parameter was overridden\n");
    }
    print_indent(); printf("* target interactive soundness error (bits) = %zu\n", this->interactive_security_parameter_);
    print_indent(); printf("* achieved interactive soundness error (bits) = %.1Lf\n", this->achieved_interactive_soundness());
    print_indent(); printf("* interactive repetitions = %zu\n", this->multi_lincheck_repetitions_);
    print_indent(); printf("* constraint domain dim = %zu\n", this->constraint_domain_dim_);
    print_indent(); printf("* make zk = %s\n", (this->make_zk_ ? "true" : "false"));
    print_indent(); printf("* domain type = %s\n", field_subset_type_names[this->domain_type_]);
}

template<typename FieldT>
multi_lincheck<FieldT>::multi_lincheck(
    iop_protocol<FieldT> &IOP,
    const domain_handle &codeword_domain_handle,
    const domain_handle &constraint_domain_handle,
    const domain_handle &variable_domain_handle,
    const std::size_t input_variable_dim,
    const std::vector<std::shared_ptr<sparse_matrix<FieldT> >> matrices,
    const oracle_handle_ptr fz_handle,
    const std::vector<oracle_handle_ptr> Mz_handles,
    const basic_lincheck_parameters<FieldT> params):
    IOP_(IOP),
    codeword_domain_handle_(codeword_domain_handle),
    constraint_domain_handle_(constraint_domain_handle),
    variable_domain_handle_(variable_domain_handle),
    num_matrices_(matrices.size()),
    params_(params)
{
    if (this->num_matrices_ < 1) {
        throw std::invalid_argument("multi_lincheck expects at least one matrix");
    }
    if (Mz_handles.size() != this->num_matrices_) {
        throw std::invalid_argument("inconsistent number of Mz_handles and matrices passed into multi lincheck.");
    }
    const field_subset<FieldT> codeword_domain = this->IOP_.get_domain(this->codeword_domain_handle_);
    const field_subset<FieldT> constraint_domain = this->IOP_.get_domain(this->constraint_domain_handle_);
    const field_subset<FieldT> variable_domain = this->IOP_.get_domain(this->variable_domain_handle_);
    field_subset<FieldT> summation_domain;
    if (constraint_domain.dimension() > variable_domain.dimension()) {
        this->summation_domain_handle_ = this->constraint_domain_handle_;
        summation_domain = constraint_domain;
    } else {
        this->summation_domain_handle_ = this->variable_domain_handle_;
        summation_domain = variable_domain;
    }

    this->constituent_oracle_handles_.emplace_back(fz_handle);
    for (std::size_t i = 0; i < Mz_handles.size(); i++) {
        this->constituent_oracle_handles_.emplace_back(Mz_handles[i]);
    }
    /** lincheck degree is: max(deg(fz * p_a^1), deg(Mz * p_a^2))
     *  = |summation domain| + max(deg(fz), deg(Mz)) - 1 */
    const std::size_t fz_degree = IOP.get_oracle_degree(fz_handle);
    const std::size_t Mz_degree = IOP.get_oracle_degree(Mz_handles[0]);
    this->lincheck_degree_ = summation_domain.num_elements() +
        std::max({fz_degree, Mz_degree}) - 1;

    const size_t sumcheck_degree_bound = this->lincheck_degree_;
    this->sumchecks_.resize(this->params_.multi_lincheck_repetitions());
    this->multi_lincheck_virtual_oracles_.resize(this->params_.multi_lincheck_repetitions());

    /** See multi_lincheck_aux.hpp for explanation of this flag, it is currently unused though. */
    const bool using_lagrange = false;
    if (using_lagrange)
    {
        /** Only cache across repetitions if there is more than 1 repetition */
        const bool cache_evaluations = this->params_.multi_lincheck_repetitions() > 1;
        std::shared_ptr<lagrange_cache<FieldT>> lagrange_coefficients_cache =
            std::make_shared<lagrange_cache<FieldT> >(summation_domain, cache_evaluations);
    }

    for (size_t i = 0; i < this->params_.multi_lincheck_repetitions(); i++)
    {
        this->sumchecks_[i] = std::make_shared<batch_sumcheck_protocol<FieldT> >(
            this->IOP_,
            this->summation_domain_handle_,
            this->codeword_domain_handle_,
            this->lincheck_degree_,
            this->params_.make_zk(),
            this->params_.domain_type());

        if (this->params_.make_zk()) {
            this->sumchecks_[i]->register_masking_polynomial();
        }

        this->multi_lincheck_virtual_oracles_[i] = std::make_shared<multi_lincheck_virtual_oracle<FieldT>>(
            codeword_domain,
            constraint_domain,
            variable_domain,
            summation_domain,
            input_variable_dim,
            matrices);
    }
}

template<typename FieldT>
void multi_lincheck<FieldT>::register_challenge()
{
    /** For every repetition, we need one random coefficient for alpha, and one random coefficient per matrix. */
    this->alpha_handles_ = register_n_verifier_messages(this->IOP_, this->params_.multi_lincheck_repetitions(), 1);
    this->random_coefficient_handles_ =
        register_n_verifier_messages(this->IOP_, this->params_.multi_lincheck_repetitions(), this->num_matrices_);

    for (size_t i = 0; i < this->params_.multi_lincheck_repetitions(); i++)
    {
        virtual_oracle_handle multi_lincheck_oracle_handle = this->IOP_.register_virtual_oracle(
            this->codeword_domain_handle_,
            this->lincheck_degree_,
            this->constituent_oracle_handles_,
            this->multi_lincheck_virtual_oracles_[i]);
        /** TODO: Eventually refactor to have sumcheck guarantee high soundness on its own,
         *        so from the perspective of lincheck it will pass of everything into sumcheck.  */
        /* claimed_sum is implicitly 0, which is intentional */
        this->sumchecks_[i]->attach_oracle_for_summing(std::make_shared<virtual_oracle_handle>(multi_lincheck_oracle_handle));
        this->sumchecks_[i]->register_challenge();
    }
}

template<typename FieldT>
void multi_lincheck<FieldT>::register_proof()
{
    for (size_t i = 0; i < this->params_.multi_lincheck_repetitions(); i++)
    {
        this->sumchecks_[i]->register_proof();
    }
}

template<typename FieldT>
void multi_lincheck<FieldT>::submit_sumcheck_masking_polynomials()
{
    for (size_t i = 0; i < this->params_.multi_lincheck_repetitions(); i++)
    {
        if (this->params_.make_zk()) {
            this->sumchecks_[i]->submit_masking_polynomial();
        }
    }
}

template<typename FieldT>
void multi_lincheck<FieldT>::calculate_and_submit_proof()
{
    enter_block("multi_lincheck: Calculate and submit proof");
    for (size_t i = 0; i < this->params_.multi_lincheck_repetitions(); i++)
    {
        const FieldT alpha = this->IOP_.obtain_verifier_random_message(this->alpha_handles_[i])[0];
        const std::vector<FieldT> r_Mz = this->IOP_.obtain_verifier_random_message(this->random_coefficient_handles_[i]);

        this->multi_lincheck_virtual_oracles_[i]->set_challenge(alpha, r_Mz);

        this->sumchecks_[i]->calculate_and_submit_proof();
    }
    leave_block("multi_lincheck: Calculate and submit proof");
}

template<typename FieldT>
void multi_lincheck<FieldT>::construct_verifier_state()
{
    for (size_t i = 0; i < this->params_.multi_lincheck_repetitions(); i++)
    {
        const FieldT alpha = this->IOP_.obtain_verifier_random_message(this->alpha_handles_[i])[0];
        const std::vector<FieldT> r_Mz = this->IOP_.obtain_verifier_random_message(this->random_coefficient_handles_[i]);

        this->multi_lincheck_virtual_oracles_[i]->set_challenge(alpha, r_Mz);

        this->sumchecks_[i]->construct_verifier_state();
    }
}

template<typename FieldT>
std::vector<oracle_handle_ptr> multi_lincheck<FieldT>::get_all_oracle_handles()
{
    std::vector<oracle_handle_ptr> result;
    for (size_t i = 0; i < this->params_.multi_lincheck_repetitions(); i++)
    {
        std::vector<oracle_handle_ptr> sumcheck_handles = this->sumchecks_[i]->get_all_oracle_handles();
        result.insert(result.end(), sumcheck_handles.begin(), sumcheck_handles.end());
    }
    return result;
}

} // libiop
