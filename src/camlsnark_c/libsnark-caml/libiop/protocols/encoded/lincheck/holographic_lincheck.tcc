namespace libiop {

template<typename FieldT>
holographic_lincheck_parameters<FieldT>::holographic_lincheck_parameters(
    const size_t interactive_security_parameter,
    const size_t constraint_domain_dim,
    const bool make_zk,
    const field_subset_type domain_type) :
    interactive_security_parameter_(interactive_security_parameter),
    constraint_domain_dim_(constraint_domain_dim),
    make_zk_(make_zk),
    domain_type_(domain_type)
{
    /** Holographic Lincheck's soundness error in the Fractal paper is shown to be less than:
     *      2(|H| - 1) / (|F| - |H|)
     *  Because we implement a 'multi lincheck',
     *  the soundness error is actually:
     *      2(|H| - 1) / (|F| - |H|) + 1/|F|.
     *  We upper bound this at: 2|H| / (|F| - |H|)
     *
     *  In calculating this, we assume that |F| - |H| ~= |F|.
     *  To achieve the security parameter, we repeat lincheck
     *  k times, where k is the smallest integer such that:
     *      -log_2( (2|H| / |F|)^k ) >= security parameter
     *  which is equivalent to:
     *      k = ceil(security parameter / -log_2((2|H| / |F|)) )
     */
    const long double field_size_bits = (long double)(soundness_log_of_field_size_helper<FieldT>(FieldT::zero()));
    /* log_2(2|H| / |F|) = 1 + log(|H|) - log(|F|)*/
    const long double soundness_per_repetitiion = (long double)(
        1 + this->constraint_domain_dim_) - field_size_bits;
    const long double holographic_lincheck_repetitions =
        ceil(-1.0 * (long double)(this->interactive_security_parameter_) / soundness_per_repetitiion);
    this->num_repetitions_ = std::max<size_t>(1, size_t(holographic_lincheck_repetitions));
}

template<typename FieldT>
bool holographic_lincheck_parameters<FieldT>::make_zk() const
{
    return this->make_zk_;
}

template<typename FieldT>
field_subset_type holographic_lincheck_parameters<FieldT>::domain_type() const
{
    return this->domain_type_;
}

template<typename FieldT>
void holographic_lincheck_parameters<FieldT>::override_security_parameter(const size_t num_repetitions)
{
    this->interactive_security_parameter_ = 0;
    this->override_security_parameter_ = true;
    this->num_repetitions_ = num_repetitions;
}

template<typename FieldT>
size_t holographic_lincheck_parameters<FieldT>::num_repetitions() const
{
    return this->num_repetitions_;
}

template<typename FieldT>
size_t holographic_lincheck_parameters<FieldT>::locality() const
{
    throw std::logic_error("holographic lincheck locality vector isn't implemented yet");
}

template<typename FieldT>
long double holographic_lincheck_parameters<FieldT>::achieved_interactive_soundness() const
{
    /* Soundness is 2|H| / (|F| - |H|). We assume (|F| - |H|) ~= |F| */
    const long double field_size_bits = (long double)(soundness_log_of_field_size_helper<FieldT>(FieldT::zero()));
    const long double soundness_per_repetition = (long double)(1 + this->constraint_domain_dim_) - field_size_bits;
    const long double num_repetitions = (long double)this->num_repetitions_;
    return -(num_repetitions) * soundness_per_repetition;
}

template<typename FieldT>
void holographic_lincheck_parameters<FieldT>::print() const
{
    printf("\nHolographic Multi lincheck parameters\n");
    if (this->override_security_parameter_)
    {
        print_indent(); printf("===WARNING=== Holographic Multi lincheck security parameter was overridden\n");
    }
    print_indent(); printf("* target interactive soundness error (bits) = %zu\n", this->interactive_security_parameter_);
    print_indent(); printf("* achieved interactive soundness error (bits) = %.1Lf\n", this->achieved_interactive_soundness());
    print_indent(); printf("* interactive repetitions = %zu\n", this->num_repetitions_);
    print_indent(); printf("* constraint domain dim = %zu\n", this->constraint_domain_dim_);
    print_indent(); printf("* make zk = %s\n", (this->make_zk_ ? "true" : "false"));
    print_indent(); printf("* domain type = %s\n", field_subset_type_names[this->domain_type_]);
}

template<typename FieldT>
holographic_multi_lincheck<FieldT>::holographic_multi_lincheck(
    iop_protocol<FieldT> &IOP,
    const domain_handle &codeword_domain_handle,
    const domain_handle &summation_domain_handle,
    const std::size_t input_variable_dim,
    const std::vector<std::shared_ptr<sparse_matrix<FieldT> >> matrices,
    const oracle_handle_ptr fz_handle,
    const std::vector<oracle_handle_ptr> Mz_handles,
    const holographic_lincheck_parameters<FieldT> params):
    IOP_(IOP),
    codeword_domain_handle_(codeword_domain_handle),
    summation_domain_handle_(summation_domain_handle),
    input_variable_dim_(input_variable_dim),
    matrices_(matrices),
    num_matrices_(matrices.size()),
    params_(params)
{
    if (this->num_matrices_ < 1) {
        throw std::invalid_argument("multi_lincheck expects at least one matrix");
    }
    if (Mz_handles.size() != this->num_matrices_) {
        throw std::invalid_argument("inconsistent number of Mz_handles and matrices passed into multi lincheck.");
    }
    this->codeword_domain_ = this->IOP_.get_domain(this->codeword_domain_handle_);
    this->summation_domain_ = this->IOP_.get_domain(this->summation_domain_handle_);

    this->constituent_oracle_handles_.emplace_back(fz_handle);
    for (std::size_t i = 0; i < Mz_handles.size(); i++)
    {
        this->constituent_oracle_handles_.emplace_back(Mz_handles[i]);
    }
    /** lincheck degree is: max(deg(fz * p_a^1), deg(Mz * p_a^2))
     *  = |summation domain| + max(deg(fz), deg(Mz)) - 1 */
    const std::size_t fz_degree = IOP.get_oracle_degree(fz_handle);
    const std::size_t Mz_degree = IOP.get_oracle_degree(Mz_handles[0]);
    this->lincheck_degree_ = this->summation_domain_.num_elements() +
        std::max({fz_degree, Mz_degree}) - 1;

    const size_t sumcheck_degree_bound = this->lincheck_degree_;

    this->sumcheck_H_.resize(this->params_.num_repetitions());
    this->multi_lincheck_virtual_oracle_.resize(this->params_.num_repetitions());
    this->t_boundary_constraint_.resize(this->params_.num_repetitions());
    for (size_t repetition = 0; repetition < this->params_.num_repetitions(); repetition++)
    {
        this->sumcheck_H_[repetition] =
            std::make_shared<batch_sumcheck_protocol<FieldT> >(
                this->IOP_,
                this->summation_domain_handle_,
                this->codeword_domain_handle_,
                this->lincheck_degree_,
                this->params_.make_zk(),
                this->params_.domain_type());

        if (this->params_.make_zk()) {
            this->sumcheck_H_[repetition]->register_masking_polynomial();
        }

        /** TODO: Rename. Perhaps Lincheck equation virtual oracle? */
        this->multi_lincheck_virtual_oracle_[repetition] =
            std::make_shared<holographic_multi_lincheck_virtual_oracle<FieldT>>(
                this->codeword_domain_,
                this->summation_domain_,
                input_variable_dim,
                matrices);

        this->t_boundary_constraint_[repetition] =
            std::make_shared<single_boundary_constraint<FieldT>>(this->codeword_domain_);
    }
}

template<typename FieldT>
void holographic_multi_lincheck<FieldT>::set_index_oracles(
    const domain_handle &indexed_domain_handle,
    const std::vector<std::vector<oracle_handle_ptr>> indexed_handles)
{
    if(indexed_handles.size() != this->num_matrices_)
    {
        throw std::invalid_argument("Incorrect number of sets of indexed oracles");
    }
    for (size_t i = 0; i < this->num_matrices_; i++)
    {
        if (indexed_handles[i].size() != 3)
        {
            throw std::invalid_argument("Incorrect number of indexed oracles within set");
        }
    }

    this->index_domain_handle_ = indexed_domain_handle;
    this->index_domain_ = this->IOP_.get_domain(this->index_domain_handle_);
    const size_t single_numerator_degree = this->index_domain_.num_elements();
    const size_t single_denominator_degree = 2 * this->index_domain_.num_elements() - 1;
    /** the combined numerator is a sum of polynomials of the form:
     *      N_i * prod_{j != i} D_j
     *  so it has the following degree */
    const size_t combined_numerator_degree = single_numerator_degree +
        (this->num_matrices_ - 1) * single_denominator_degree - (this->num_matrices_ - 1);
    const size_t combined_denominator_degree =
        this->num_matrices_ * single_denominator_degree - (this->num_matrices_ - 1);

    this->matrix_denominators_.resize(this->params_.num_repetitions());
    this->matrix_numerator_handles_.resize(this->params_.num_repetitions());
    this->matrix_denominator_handles_.resize(this->params_.num_repetitions());
    this->sumcheck_K_.resize(this->params_.num_repetitions());

    for (size_t repetition = 0; repetition < this->params_.num_repetitions(); repetition++)
    {
        for (size_t i = 0; i < this->num_matrices_; i++)
        {
            this->matrix_denominators_[repetition].emplace_back(
                std::make_shared<single_matrix_denominator<FieldT>>(
                    this->codeword_domain_,
                    this->index_domain_,
                    this->input_variable_dim_
                ));
            const size_t position_of_val_in_indexed_handles = 2;
            this->matrix_numerator_handles_[repetition].emplace_back(
                indexed_handles[i][position_of_val_in_indexed_handles]);
            this->matrix_denominator_handles_[repetition].emplace_back(
                std::make_shared<virtual_oracle_handle>(
                    this->IOP_.register_virtual_oracle(
                        this->codeword_domain_handle_,
                        single_denominator_degree,
                        {indexed_handles[i][0], indexed_handles[i][1]},
                        this->matrix_denominators_[repetition][i])));
        }

        this->sumcheck_K_[repetition] = std::make_shared<rational_sumcheck_protocol<FieldT>>(
            this->IOP_,
            this->index_domain_handle_,
            this->codeword_domain_handle_,
            combined_numerator_degree,
            combined_denominator_degree,
            this->index_domain_.type());
    }
}

template<typename FieldT>
void holographic_multi_lincheck<FieldT>::register_challenge_alpha()
{
    /** For every repetition,
     *  we need one random coefficient for alpha, and one random coefficient per matrix. */
    this->alpha_handle_ = register_n_verifier_messages(this->IOP_, this->params_.num_repetitions(), 1);
    this->random_coefficient_handle_ =
        register_n_verifier_messages(this->IOP_, this->params_.num_repetitions(), this->num_matrices_);

}

template<typename FieldT>
void holographic_multi_lincheck<FieldT>::register_response_alpha()
{
    /** In this round, the lincheck problem gets reduced to sumcheck.
     *  The following virtual oracle is constructed:
     *      lincheck_oracle(x) := p(alpha, x) * f_Mz(x) - t_1(x) * f_z(x)
     *  t_1(x) is nominally p_M(alpha, x),
     *  the verifier will check this in the subsequent round.
     *  If t_1(x) is actually p_M(alpha, x), then lincheck is satisfied if
     *  the lincheck oracle sums to 0.
    */
    /* p_{alpha, M} contains no information about the input */

    this->t_oracle_handle_.resize(this->params_.num_repetitions());
    for (size_t repetition = 0; repetition < this->params_.num_repetitions(); repetition++)
    {
        bool make_zk = false;
        this->t_oracle_handle_[repetition] = this->IOP_.register_oracle(
            this->codeword_domain_handle_, this->summation_domain_.num_elements(), make_zk);

        std::vector<oracle_handle_ptr> constituent_handles(this->constituent_oracle_handles_);
        constituent_handles.emplace_back(std::make_shared<oracle_handle>(this->t_oracle_handle_[repetition]));
        virtual_oracle_handle multi_lincheck_oracle_handle = this->IOP_.register_virtual_oracle(
            this->codeword_domain_handle_,
            this->lincheck_degree_,
            constituent_handles,
            this->multi_lincheck_virtual_oracle_[repetition]);
        /* claimed_sum is implicitly 0, which is intentional */
        this->sumcheck_H_[repetition]->attach_oracle_for_summing(
            std::make_shared<virtual_oracle_handle>(multi_lincheck_oracle_handle));
    }
}

template<typename FieldT>
void holographic_multi_lincheck<FieldT>::register_challenge_beta()
{
    this->beta_handle_ = register_n_verifier_messages(this->IOP_, this->params_.num_repetitions(), 1);
    for (size_t repetition = 0; repetition < this->params_.num_repetitions(); repetition++)
    {
        this->sumcheck_H_[repetition]->register_challenge();
    }
}

template<typename FieldT>
void holographic_multi_lincheck<FieldT>::register_response_beta()
{
    /** In this round, the verifier begins testing that t_1 is correct.
     *  The verifier wants to check that
     *      t_1(x) = p_M(alpha, x)
     *  Its shown in [COS19] that p_M(alpha, x) = M'(alpha, x),
     *  where M'(x, y) = M_{yx} * p(y, y).
     *  To check that t_1(x) = M'(alpha, x),
     *  the verifier samples a random point beta, and checks that
     *      t_1(beta) = M'(alpha, beta)
     *  The prover sends the claimed value of M' at alpha, beta.
     *  It checks that t_1(beta) = claimed value with a boundary constraint.
     *  As shown in [COS19],
     *      M(alpha, beta) = sum_{k in K} val(k) / ((row(k) - alpha)(col(k) - beta))
     *  The prover / verifier engage in rational sumcheck to check that this also
     *  equals the claimed value.
     *
     *  TODO: Phrase better
     *  The shifting by p(y, y) for M' is accounted for within val.
     *  As this is a multi lincheck, we take a rational linear combination
     *  of the rationals. This increases constraint degree & tested degree.
     *  We could index more oracles,
     *  for a trade-off between #indexed oracles / argument size, and prover time.
     */
    this->M_at_alpha_beta_ = register_n_prover_messages(this->IOP_, this->params_.num_repetitions(), 1);
    this->rational_linear_combination_.resize(this->params_.num_repetitions());
    this->t_boundary_constraint_handle_.resize(this->params_.num_repetitions());
    for (size_t repetition = 0; repetition < this->params_.num_repetitions(); repetition++)
    {
        this->rational_linear_combination_[repetition] =
            std::make_shared<rational_linear_combination<FieldT>>(
                this->IOP_,
                this->num_matrices_,
                this->matrix_numerator_handles_[repetition],
                this->matrix_denominator_handles_[repetition]);

        this->sumcheck_K_[repetition]->register_summation_oracle(
            this->rational_linear_combination_[repetition]->get_numerator_handle(),
            this->rational_linear_combination_[repetition]->get_denominator_handle());

            std::vector<oracle_handle_ptr> constituent_handles({
                std::make_shared<oracle_handle>(this->t_oracle_handle_[repetition])});
            this->t_boundary_constraint_handle_[repetition] =
                this->IOP_.register_virtual_oracle(
                    this->codeword_domain_handle_,
                    this->summation_domain_.num_elements() - 1,
                    constituent_handles,
                    this->t_boundary_constraint_[repetition]);

        this->sumcheck_H_[repetition]->register_proof();
        this->sumcheck_K_[repetition]->register_proof();
    }
}

template<typename FieldT>
void holographic_multi_lincheck<FieldT>::submit_sumcheck_masking_polynomials()
{
    if (this->params_.make_zk())
    {
        for (size_t repetition = 0;
                repetition < this->params_.num_repetitions(); repetition++)
        {
            this->sumcheck_H_[repetition]->submit_masking_polynomial();
        }
    }
}

template<typename FieldT>
void holographic_multi_lincheck<FieldT>::calculate_response_alpha()
{
    this->r_Mz_.resize(this->params_.num_repetitions());
    this->p_alpha_.resize(this->params_.num_repetitions());
    this->p_alpha_over_H_.resize(this->params_.num_repetitions());
    this->p_alpha_M_poly_.resize(this->params_.num_repetitions());
    for (size_t repetition = 0; repetition < this->params_.num_repetitions(); repetition++)
    {
        enter_block("Calculate alpha_summation_oracle");
        const FieldT alpha = this->IOP_.obtain_verifier_random_message(this->alpha_handle_[repetition])[0];
        this->r_Mz_[repetition] = this->IOP_.obtain_verifier_random_message(this->random_coefficient_handle_[repetition]);
        /** Unnormalized,
         *  as this is needed for the reduction from the 3 round protocol variant to 2 round variant
         *  TODO: Explain this w/o mentioning 3 round variant, as that will no longer exist eventually */
        const bool normalized = false;
        this->p_alpha_[repetition] =
            lagrange_polynomial<FieldT>(alpha, this->summation_domain_, normalized);
        this->p_alpha_over_H_[repetition] =
            this->p_alpha_[repetition].evaluations_over_field_subset(this->summation_domain_);

        enter_block("multi_lincheck compute p_alpha_M");
        std::vector<FieldT> p_alpha_M = compute_p_alpha_M(
            this->input_variable_dim_, this->summation_domain_,
            this->p_alpha_over_H_[repetition], this->r_Mz_[repetition],
            this->matrices_);
        std::vector<FieldT> p_alpha_M_over_L =
            FFT_over_field_subset<FieldT>(p_alpha_M, this->codeword_domain_);
        leave_block("multi_lincheck compute p_alpha_M");
        /* t is the provers alleged commitment to p_alpha_M */
        this->IOP_.submit_oracle(this->t_oracle_handle_[repetition], std::move(p_alpha_M_over_L));
        this->p_alpha_M_poly_[repetition] = polynomial<FieldT>(std::move(p_alpha_M));

        this->multi_lincheck_virtual_oracle_[repetition]->set_challenge(alpha, this->r_Mz_[repetition]);

        leave_block("Calculate alpha_summation_oracle");
    }
}

template<typename FieldT>
std::vector<std::shared_ptr<std::vector<FieldT>>> convert_to_shared(
    std::vector<std::vector<FieldT>> vec)
{
    std::vector<std::shared_ptr<std::vector<FieldT>>> result;
    for (size_t i = 0; i < vec.size(); i++)
    {
        result.emplace_back(
            std::make_shared<std::vector<FieldT>>(std::move(vec[i])));
    }
    return result;
}

template<typename FieldT>
void holographic_multi_lincheck<FieldT>::calculate_response_beta()
{
    this->set_rational_linear_combination_coefficients();
    this->set_matrix_denominator_challenges();

    for (size_t repetition = 0; repetition < this->params_.num_repetitions(); repetition++)
    {
        enter_block("Calculate beta_summation_oracle");
        const FieldT beta = this->IOP_.obtain_verifier_random_message(
            this->beta_handle_[repetition])[0];
        /** We have to compute the combined rational function over K,
         *  to pass into rational sumcheck.    */
        std::vector<std::shared_ptr<std::vector<FieldT>>> numerator_oracles_over_K;
        std::vector<std::shared_ptr<std::vector<FieldT>>> denominator_oracles_over_K;
        enter_block("Compute rational function over K");
        for (size_t i = 0; i < this->num_matrices_; i++)
        {
            /** TODO: Also index evals over K instead of Re-IFFTing here */
            matrix_indexer<FieldT> indexer(
                this->IOP_,
                this->index_domain_handle_,
                this->summation_domain_handle_,
                this->codeword_domain_handle_,
                this->input_variable_dim_,
                this->matrices_[i]);
            std::vector<std::shared_ptr<std::vector<FieldT>>> index_evals_over_K =
                convert_to_shared<FieldT>(indexer.compute_oracles_over_K());

            numerator_oracles_over_K.emplace_back(index_evals_over_K[2]);
            index_evals_over_K.pop_back();
            denominator_oracles_over_K.emplace_back(
                this->matrix_denominators_[repetition][i]->evaluated_contents(index_evals_over_K));
        }
        std::vector<FieldT> combined_rational_over_K =
            this->rational_linear_combination_[repetition]->evaluated_contents(
                numerator_oracles_over_K, denominator_oracles_over_K);
        leave_block("Compute rational function over K");

        this->sumcheck_K_[repetition]->calculate_and_submit_proof(combined_rational_over_K);
        const FieldT M_at_alpha_beta = this->sumcheck_K_[repetition]->get_claimed_sum();
        this->IOP_.submit_prover_message(
            this->M_at_alpha_beta_[repetition], {M_at_alpha_beta});
        this->t_boundary_constraint_[repetition]->set_evaluation_point_and_eval(beta, M_at_alpha_beta);

        this->sumcheck_H_[repetition]->calculate_and_submit_proof();

        leave_block("Calculate beta_summation_oracle");
    }
}

template<typename FieldT>
void holographic_multi_lincheck<FieldT>::set_rational_linear_combination_coefficients()
{
    vanishing_polynomial<FieldT> Z_H(this->summation_domain_);
    for (size_t repetition = 0; repetition < this->params_.num_repetitions(); repetition++)
    {
        const FieldT alpha = this->IOP_.obtain_verifier_random_message(this->alpha_handle_[repetition])[0];
        const FieldT beta = this->IOP_.obtain_verifier_random_message(this->beta_handle_[repetition])[0];
        std::vector<FieldT> rational_linear_combination_coefficients;
        const FieldT shift = Z_H.evaluation_at_point(alpha) * Z_H.evaluation_at_point(beta);
        for (size_t i = 0; i < this->num_matrices_; i++)
        {
            rational_linear_combination_coefficients.emplace_back(
                shift * this->r_Mz_[repetition][i]);
        }
        this->rational_linear_combination_[repetition]->set_coefficients(
            rational_linear_combination_coefficients);
    }
}

template<typename FieldT>
void holographic_multi_lincheck<FieldT>::set_matrix_denominator_challenges()
{
    for (size_t repetition = 0; repetition < this->params_.num_repetitions(); repetition++)
    {
        const FieldT alpha = this->IOP_.obtain_verifier_random_message(this->alpha_handle_[repetition])[0];
        const FieldT beta = this->IOP_.obtain_verifier_random_message(this->beta_handle_[repetition])[0];
        for (size_t i = 0; i < this->num_matrices_; i++)
        {
            this->matrix_denominators_[repetition][i]->set_challenge(beta, alpha);
        }
    }
}

template<typename FieldT>
void holographic_multi_lincheck<FieldT>::construct_verifier_state()
{
    this->p_alpha_.resize(this->params_.num_repetitions());
    this->r_Mz_.resize(this->params_.num_repetitions());

    for (size_t repetition = 0; repetition < this->params_.num_repetitions(); repetition++)
    {
        const FieldT alpha = this->IOP_.obtain_verifier_random_message(this->alpha_handle_[repetition])[0];
        /** We use un-normalized lagrange polynomials */
        const bool normalized = false;
        this->p_alpha_[repetition] = lagrange_polynomial<FieldT>(alpha, this->summation_domain_, normalized);
        this->r_Mz_[repetition] = this->IOP_.obtain_verifier_random_message(this->random_coefficient_handle_[repetition]);

        this->multi_lincheck_virtual_oracle_[repetition]->set_challenge(alpha, this->r_Mz_[repetition]);

        const FieldT beta = this->IOP_.obtain_verifier_random_message(this->beta_handle_[repetition])[0];
        /** get the claimed evaluation of t(beta) from the prover message,
         *  check that t actually evaluates to that via boundary constraint,
         *  and then check that
         *  t_1(beta) = sum_{h in H} p(alpha, h) M(h, beta)
         */
        const FieldT claimed_value_of_t_at_beta =
            this->IOP_.receive_prover_message(this->M_at_alpha_beta_[repetition])[0];
        this->t_boundary_constraint_[repetition]->set_evaluation_point_and_eval(beta, claimed_value_of_t_at_beta);
        this->sumcheck_H_[repetition]->construct_verifier_state();
        this->sumcheck_K_[repetition]->construct_verifier_state(claimed_value_of_t_at_beta);
    }
    this->set_rational_linear_combination_coefficients();
    this->set_matrix_denominator_challenges();
}

template<typename FieldT>
std::vector<oracle_handle_ptr> holographic_multi_lincheck<FieldT>::get_all_oracle_handles_for_repetition(size_t repetition)
{
    std::vector<oracle_handle_ptr> result;
    result.emplace_back(std::make_shared<oracle_handle>(this->t_oracle_handle_[repetition]));
    result.emplace_back(std::make_shared<virtual_oracle_handle>(
        this->t_boundary_constraint_handle_[repetition]));
    std::vector<oracle_handle_ptr> sumcheck_H_handles =
        this->sumcheck_H_[repetition]->get_all_oracle_handles();
    result.insert(result.end(), sumcheck_H_handles.begin(), sumcheck_H_handles.end());
    std::vector<oracle_handle_ptr> sumcheck_K_handles =
        this->sumcheck_K_[repetition]->get_all_oracle_handles();
    result.insert(result.end(), sumcheck_K_handles.begin(), sumcheck_K_handles.end());
    return result;
}

template<typename FieldT>
std::vector<oracle_handle_ptr> holographic_multi_lincheck<FieldT>::get_all_oracle_handles()
{
    std::vector<oracle_handle_ptr> result = get_all_oracle_handles_for_repetition(0);
    for (size_t repetition = 1; repetition < this->params_.num_repetitions(); repetition++)
    {
        std::vector<oracle_handle_ptr> cur_repetition_handles =
            this->get_all_oracle_handles_for_repetition(repetition);
        result.insert(result.end(), cur_repetition_handles.begin(), cur_repetition_handles.end());
    }
    return result;
}

} // libiop
