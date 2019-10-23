namespace libiop {

template<typename FieldT>
dummy_protocol<FieldT>::dummy_protocol(
    iop_protocol<FieldT> &IOP,
    const size_t num_oracles_per_round,
    const size_t num_rounds,
    const std::vector<round_parameters<FieldT>> round_params,
    const domain_handle codeword_domain_handle,
    const bool make_zk,
    const bool holographic) :
    IOP_(IOP),
    num_oracles_per_round_(num_oracles_per_round),
    num_rounds_(num_rounds),
    codeword_domain_handle_(codeword_domain_handle),
    make_zk_(make_zk),
    holographic_(holographic)
{
    this->codeword_domain_ = this->IOP_.get_domain(this->codeword_domain_handle_);
    this->codeword_domain_size_ = this->codeword_domain_.num_elements();
    const size_t oracle_degree = this->codeword_domain_size_ - 1;

    /** If holographic, round 0 will be index oracles. */
    this->oracle_handle_ptrs_.resize(this->num_rounds_);
    for (size_t r = 0; r < this->num_rounds_; ++r)
    {
        if (r != 0)
        {
            // Add verifier messages to increase round
            this->verifier_msg_handles_.emplace_back(
                this->IOP_.register_verifier_random_message(1));
        }
        this->IOP_.set_round_parameters(round_params[r]);
        this->oracle_handle_ptrs_[r].reserve(this->num_oracles_per_round_);

        /** Since this is only used with the BCS transformation,
         *  we only make 1 oracle zk as this _ought_ to be equivalent to all oracles for this domain
         *  being zk, and so tests will catch if this is not the case. */
        std::size_t rand_zk_index = std::rand() % this->num_oracles_per_round_;
        for (size_t i = 0; i < this->num_oracles_per_round_; ++i)
        {
            const bool make_oracle_zk = make_zk && (rand_zk_index == i);
            oracle_handle oracle_i_handle;
            if (this->holographic_ && r == 0)
            {
                oracle_i_handle = this->IOP_.register_index_oracle(
                    this->codeword_domain_handle_, oracle_degree);
            }
            else
            {
                oracle_i_handle = this->IOP_.register_oracle(
                    this->codeword_domain_handle_, oracle_degree, make_oracle_zk);
            }
            this->oracle_handle_ptrs_[r].emplace_back(
                std::make_shared<oracle_handle>(oracle_i_handle));
        }
        if (this->holographic_ && r == 0)
        {
            this->IOP_.signal_index_registrations_done();
        }
    }
}

/* Indexing */
template<typename FieldT>
void dummy_protocol<FieldT>::calculate_index()
{
    const size_t r = 0;
    const size_t round_val =
            this->codeword_domain_size_ * this->codeword_domain_size_ * r;
    for (size_t i = 0; i < this->num_oracles_per_round_; ++i)
    {
        std::vector<FieldT> evaluations(this->codeword_domain_size_);
        for (size_t j = 0; j < this->codeword_domain_size_; ++j)
        {
            evaluations[j] = FieldT(round_val + i*this->codeword_domain_size_ + j);
        }
        this->IOP_.submit_oracle(this->oracle_handle_ptrs_[r][i], std::move(evaluations));
    }
    this->IOP_.signal_index_submissions_done();
}

template<typename FieldT>
void dummy_protocol<FieldT>::calculate_and_submit_response()
{
    const size_t start_round = this->holographic_ ? 1 : 0;
    for (size_t r = start_round; r < this->num_rounds_; ++r)
    {
        if (r != 0)
        {
            this->IOP_.obtain_verifier_random_message(this->verifier_msg_handles_[r - 1]);
        }
        const size_t round_val =
            this->codeword_domain_size_ * this->codeword_domain_size_ * r;
        for (size_t i = 0; i < this->num_oracles_per_round_; ++i)
        {
            std::vector<FieldT> evaluations(this->codeword_domain_size_);
            for (size_t j = 0; j < this->codeword_domain_size_; ++j)
            {
                evaluations[j] = FieldT(round_val + i*this->codeword_domain_size_ + j);
            }
            this->IOP_.submit_oracle(this->oracle_handle_ptrs_[r][i], std::move(evaluations));
        }
        this->IOP_.signal_prover_round_done();
    }
}

/* Verification */
template<typename FieldT>
bool dummy_protocol<FieldT>::check_eval_at_point(const size_t round_index,
                                                 const size_t oracle_index,
                                                 const size_t eval_pos,
                                                 const FieldT eval)
{
    const size_t round_val =
        this->codeword_domain_size_ * this->codeword_domain_size_ * round_index;
    const size_t oracle_val = this->codeword_domain_size_ * oracle_index;
    return eval == FieldT(round_val + oracle_val + eval_pos);
}

template<typename FieldT>
std::vector<oracle_handle_ptr> dummy_protocol<FieldT>::get_oracle_handles_for_round(const size_t round)
{
    return this->oracle_handle_ptrs_[round];
}

} // namespace libiop
