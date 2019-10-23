namespace libiop {

template<typename FieldT>
bcs_verifier<FieldT>::bcs_verifier(const bcs_transformation_parameters<FieldT> &parameters,
                                   const bcs_transformation_transcript<FieldT> &transcript) :
    bcs_protocol<FieldT>(parameters),
    transcript_(transcript),
    is_preprocessing_(false)
{
}

template<typename FieldT>
bcs_verifier<FieldT>::bcs_verifier(const bcs_transformation_parameters<FieldT> &parameters,
                                   const bcs_transformation_transcript<FieldT> &transcript,
                                   const bcs_verifier_index<FieldT> &index) :
    bcs_protocol<FieldT>(parameters),
    transcript_(transcript),
    is_preprocessing_(true),
    index_(index)
{
    /** We check that the indexer provided the correct number of roots and messages in
     *  seal_interaction_registrations()    */
    this->transcript_.MT_roots_.insert(
        this->transcript_.MT_roots_.begin(),
        index.index_MT_roots_.begin(),
        index.index_MT_roots_.end());
    this->transcript_.prover_messages_.insert(
        this->transcript_.prover_messages_.begin(),
        index.indexed_messages_.begin(),
        index.indexed_messages_.end());
}

template<typename FieldT>
void bcs_verifier<FieldT>::seal_interaction_registrations()
{
    enter_block("verifier_seal_interaction_registrations");
    bcs_protocol<FieldT>::seal_interaction_registrations();

    /* Compute pseudorandom state (chaining together the MT roots) */
    hash_digest cur_state = "";

    std::size_t processed_MTs = 0;
    std::vector<size_t> MT_index_to_round;
    for (std::size_t round = 0; round < this->num_interaction_rounds_; ++round)
    {
        /* Update the pseudorandom state for the oracle messages. */
        const domain_to_oracles_map mapping = this->oracles_in_round(round);
        if (this->is_preprocessing_ && round == 0)
        {
            if (mapping.size() != this->index_.index_MT_roots_.size())
            {
                throw std::invalid_argument("Index had an incorrect number of MT roots");
            }
            if (this->num_prover_messages_at_end_of_round_[0]
                 != this->index_.indexed_messages_.size())
            {
                throw std::invalid_argument("Index had an incorrect number of prover messages");
            }
        }

        for (auto &kv : mapping)
        {
            MT_index_to_round.emplace_back(round);
            /* We aren't using the oracles at this point, but we know that
               one MT root corresponds to each oracle in this round. */
            UNUSED(kv);
            cur_state = this->parameters_.compression_hasher(cur_state,
                                                             this->transcript_.MT_roots_[processed_MTs++],
                                                             this->digest_len_bytes_);
        }

        /* Add the prover message hash as a "root" and update the pseudorandom state */
        const hash_digest message_hash = this->compute_message_hash(round, this->transcript_.prover_messages_);
        cur_state = this->parameters_.compression_hasher(cur_state, message_hash, this->digest_len_bytes_);

        this->pseudorandom_state_.emplace_back(cur_state);
    }

    /* Validate all MT queries relative to the transcript. */
    this->transcript_is_valid_ = true;

    for (std::size_t MT_idx = 0; MT_idx < this->transcript_.MT_roots_.size(); ++MT_idx)
    {
        auto &root = this->transcript_.MT_roots_[MT_idx];
        std::vector<std::size_t> &query_positions = this->transcript_.query_positions_[MT_idx];
        std::vector<std::size_t> &MT_leaf_positions = this->transcript_.MT_leaf_positions_[MT_idx];
        std::vector<std::vector<FieldT> > &query_responses = this->transcript_.query_responses_[MT_idx];
        auto &proof = this->transcript_.MT_set_membership_proofs_[MT_idx];

        // Step 1) serialize query responses into leaf responses
        std::vector<std::vector< FieldT> > MT_leaf_columns =
            this->query_responses_to_MT_leaf_responses(query_positions,
                                                       query_responses,
                                                       MT_index_to_round[MT_idx]);
        // Step 2) hash each leaf column
        std::vector<hash_digest> column_hashes;
        for (auto &v : MT_leaf_columns)
        {
            column_hashes.emplace_back(this->parameters_.field_hasher(
                                       v,
                                       this->digest_len_bytes_));
        }
        // Step 3) validate proof

        const bool proof_is_valid = this->Merkle_trees_[MT_idx].validate_set_membership_proof(
            root,
            MT_leaf_positions,
            column_hashes,
            proof);

        if (!proof_is_valid)
        {
            this->transcript_is_valid_ = false;
        }
    }

    /* Finally populate things for obtaining query responses */
    this->parse_query_responses_from_transcript();
    leave_block("verifier_seal_interaction_registrations");
}

template<typename FieldT>
void bcs_verifier<FieldT>::parse_query_responses_from_transcript()
{
    std::size_t processed_MTs = 0;
    for (std::size_t round = 0; round < this->num_interaction_rounds_; ++round)
    {
        const domain_to_oracles_map mapping = this->oracles_in_round(round);
        const round_parameters<FieldT> round_params = this->get_round_parameters(round);

        /* For each oracle, pick out positions and values from the transcript
            and store them. */
        for (auto &kv : mapping)
        {
            std::size_t oracles_processed_for_MT = 0;
            for (auto &oh : kv.second)
            {
                for (std::size_t i = 0; i < this->transcript_.query_positions_[processed_MTs].size(); ++i)
                {
                    const std::size_t pos_idx = transcript_.query_positions_[processed_MTs][i];
                    const FieldT value = transcript_.query_responses_[processed_MTs][i][oracles_processed_for_MT];

                    this->oracle_id_and_pos_idx_to_value_[std::make_pair(oh.id(), pos_idx)] = value;
                }
                ++oracles_processed_for_MT;
            }

            ++processed_MTs;
        }
    }
}

template<typename FieldT>
std::vector<std::vector<FieldT> > bcs_verifier<FieldT>::query_responses_to_MT_leaf_responses(
    std::vector<size_t> &query_positions,
    std::vector<std::vector<FieldT> > &query_responses,
    const size_t round)
{
    const round_parameters<FieldT> round_params = this->get_round_parameters(round);
    if (round_params.quotient_map_size_ == 1)
    {
        return query_responses;
    }
    return this->Merkle_trees_[round].serialize_leaf_values_by_coset(
        query_positions,
        query_responses,
        round_params.quotient_map_size_
    );
}

template<typename FieldT>
void bcs_verifier<FieldT>::signal_prover_round_done()
{
    throw std::logic_error("Verifier IOP is not meant for proving.");
}

template<typename FieldT>
void bcs_verifier<FieldT>::signal_index_submissions_done()
{
    throw std::logic_error("Verifier IOP is not meant for indexing.");
}

template<typename FieldT>
std::vector<FieldT> bcs_verifier<FieldT>::obtain_verifier_random_message(
    const verifier_random_message_handle &random_message)
{
    const std::size_t message_length = this->verifier_random_message_registrations_[random_message.id()].size();

    /* Find the index of the round containing this message. */
    const std::size_t round = (std::lower_bound(this->num_verifier_random_messages_at_end_of_round_.begin(),
                                                this->num_verifier_random_messages_at_end_of_round_.end(),
                                                random_message.id() + 1) -
                               this->num_verifier_random_messages_at_end_of_round_.begin());

    /* Use the pseudorandom state from the PREVIOUS round (that's how the
       "random" message is generated, because the pseudorandom state is
       constructed at the END of each round.) */
    hash_digest prev_pseudorandom_state;
    if (round == 0)
    {
        prev_pseudorandom_state = "";
    }
    else
    {
        prev_pseudorandom_state = this->pseudorandom_state_[round - 1];
    }

    const std::vector<FieldT> result =
        this->parameters_.FieldT_randomness_extractor(prev_pseudorandom_state,
                                                      random_message.id(),
                                                      message_length);
    this->verifier_random_messages_[random_message.id()] = result;

#ifdef DEBUG
    printf("verifier: random message id=%zu, round=%zu\n", random_message.id(), round);
    for (auto &v : result)
    {
        v.print();
    }
#endif // DEBUG

    return result;
}

template<typename FieldT>
FieldT bcs_verifier<FieldT>::get_oracle_evaluation_at_point(
    const oracle_handle_ptr &handle,
    const std::size_t evaluation_position,
    const bool record)
{
    UNUSED(record);

    if (std::dynamic_pointer_cast<virtual_oracle_handle>(handle))
    {
        /* If virtual oracle, we just defer to the original code (the
           get_oracle_evaluation_at_point function in iop_protocol),
           which call this function to get each of the constituent
           evaluations. */
        return bcs_protocol<FieldT>::get_oracle_evaluation_at_point(handle, evaluation_position, false);
    }
    else if (std::dynamic_pointer_cast<oracle_handle>(handle))
    {
        /* If real oracle, use our saved values that we saved from
           the transcript. */
        auto it = this->oracle_id_and_pos_idx_to_value_.find(std::make_pair(handle->id(), evaluation_position));

#ifdef DEBUG
        printf("query: oracle %zu at position %zu\n", handle->id(), evaluation_position);
#endif // DEBUG

        if (it == this->oracle_id_and_pos_idx_to_value_.end())
        {
            throw std::logic_error("Got a request for a query position that's unavailable in the proof.");
        }
        return it->second;
    }
    else
    {
        throw std::invalid_argument("oracle type not supported");
    }
}

template<typename FieldT>
std::vector<FieldT> bcs_verifier<FieldT>::receive_prover_message(const prover_message_handle &message)
{
    return this->transcript_.prover_messages_[message.id()];
}

template<typename FieldT>
bool bcs_verifier<FieldT>::transcript_is_valid() const
{
    return this->transcript_is_valid_;
}

} // namespace libiop
