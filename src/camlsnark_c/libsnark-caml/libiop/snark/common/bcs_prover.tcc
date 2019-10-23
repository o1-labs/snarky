namespace libiop {

template<typename FieldT>
bcs_prover<FieldT>::bcs_prover(const bcs_transformation_parameters<FieldT> &parameters) :
    bcs_protocol<FieldT>(parameters),
    is_preprocessing_(false)
{
}

template<typename FieldT>
bcs_prover<FieldT>::bcs_prover(const bcs_transformation_parameters<FieldT> &parameters,
                               bcs_prover_index<FieldT> &index) :
    bcs_protocol<FieldT>(parameters),
    is_preprocessing_(true)
{
    this->num_indexed_MTs_ = index.index_MTs_.size();
    std::swap(this->Merkle_trees_, index.index_MTs_);
    this->indexed_prover_messages_ = index.indexed_messages_;
}

template<typename FieldT>
void bcs_prover<FieldT>::signal_prover_round_done()
{
    enter_block("Finish prover round");
    iop_protocol<FieldT>::signal_prover_round_done();
    std::size_t ended_round = this->num_prover_rounds_done_-1;
    const domain_to_oracles_map mapping = this->oracles_in_round(ended_round);
    const round_parameters<FieldT> round_params = this->get_round_parameters(ended_round);

    /* First, go through all the oracle messages in this round and
       compress each one using a Merkle Tree. */
    hash_digest cur_state = (this->pseudorandom_state_.empty() ? "" : *this->pseudorandom_state_.rbegin());
    std::vector<hash_digest> roots;
    for (auto &kv : mapping)
    {
        std::vector<std::shared_ptr<std::vector<FieldT>>> all_evaluated_contents;
        for (auto &v : kv.second)
        {
            all_evaluated_contents.emplace_back(this->oracles_[v.id()].evaluated_contents());
        }
        enter_block("Construct Merkle tree");
        this->Merkle_trees_[this->processed_MTs_].construct_with_leaves_serialized_by_cosets(
            all_evaluated_contents, round_params.quotient_map_size_);
        leave_block("Construct Merkle tree");
        cur_state = this->parameters_.compression_hasher(cur_state,
                                                         this->Merkle_trees_[this->processed_MTs_].get_root(),
                                                         this->digest_len_bytes_);

        ++this->processed_MTs_;
    }

    const hash_digest message_hash = this->compute_message_hash(ended_round, this->prover_messages_);
    cur_state = this->parameters_.compression_hasher(cur_state, message_hash, this->digest_len_bytes_);

    /* Add the prover message hash as a "root" and update the pseudorandom state */
    this->pseudorandom_state_.emplace_back(cur_state);

    leave_block("Finish prover round");
}

template<typename FieldT>
void bcs_prover<FieldT>::seal_query_registrations()
{
    iop_protocol<FieldT>::seal_query_registrations();
}

template<typename FieldT>
void bcs_prover<FieldT>::signal_index_submissions_done()
{
    if (!this->is_preprocessing_)
    {
        throw std::invalid_argument("Didn't provide prover index to BCS prover");
    }
    iop_protocol<FieldT>::signal_prover_round_done();
    std::size_t ended_round = this->num_prover_rounds_done_-1;
    const domain_to_oracles_map mapping = this->oracles_in_round(ended_round);
    // Question: Do the index oracles need to update the state,
    // I don't think so, but I am doing it out of convenience with the rest of the compilation.

    /* First, go through all the oracle messages in this round and
       fill in the merkle tree from the index. */
    hash_digest cur_state = (this->pseudorandom_state_.empty() ? "" : *this->pseudorandom_state_.rbegin());
    for (auto &kv : mapping)
    {
        /* MT is already created for the prover */
        cur_state = this->parameters_.compression_hasher(cur_state,
                                                         this->Merkle_trees_[this->processed_MTs_].get_root(),
                                                         this->digest_len_bytes_);
        ++this->processed_MTs_;
    }

    const hash_digest message_hash = this->compute_message_hash(ended_round, this->prover_messages_);
    cur_state = this->parameters_.compression_hasher(cur_state, message_hash, this->digest_len_bytes_);

    /* Add the prover message hash as a "root" and update the pseudorandom state */
    this->pseudorandom_state_.emplace_back(cur_state);
}

/* Each "random" verifier message is deterministically constructed from the previous
   one, so that both prover and verifier can reconstruct. */
template<typename FieldT>
std::vector<FieldT> bcs_prover<FieldT>::obtain_verifier_random_message(
    const verifier_random_message_handle &random_message)
{
    /* TODO: technical debt */
    iop_protocol<FieldT>::obtain_verifier_random_message(random_message);

    const std::size_t message_length = this->verifier_random_message_registrations_[random_message.id()].size();
    /* Find the index of the round containing this message. */
    const std::size_t round = (std::lower_bound(this->num_verifier_random_messages_at_end_of_round_.begin(),
                                                this->num_verifier_random_messages_at_end_of_round_.end(),
                                                random_message.id() + 1) -
                               this->num_verifier_random_messages_at_end_of_round_.begin());
#ifdef DEBUG
    printf("prover: random message id=%zu, round=%zu\n", random_message.id(), round);
#endif

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

    return result;
}

template<typename FieldT>
FieldT bcs_prover<FieldT>::obtain_query_response(const query_handle &query)
{
    /* Just defer to oracles. */
    return iop_protocol<FieldT>::obtain_query_response(query);
}

template<typename FieldT>
void bcs_prover<FieldT>::remove_index_info_from_transcript(bcs_transformation_transcript<FieldT> &transcript)
{
    if (!this->is_preprocessing_)
    {
        throw std::invalid_argument("This should only be called on preprocessing SNARKs");
    }
    size_t num_indexed_prover_messages = this->indexed_prover_messages_.size();
    // Remove the first N elements, and shift everything else down by N indices
    transcript.prover_messages_.erase(
        transcript.prover_messages_.begin(),
        transcript.prover_messages_.begin() + num_indexed_prover_messages);
    transcript.MT_roots_.erase(
        transcript.MT_roots_.begin(),
        transcript.MT_roots_.begin() + this->num_indexed_MTs_);
}

template<typename FieldT>
bcs_transformation_transcript<FieldT> bcs_prover<FieldT>::get_transcript()
{
    bcs_transformation_transcript<FieldT> result;

    /*
      Easy part: fill in (explicit) prover messages and MT roots.
    */
    result.prover_messages_ = this->prover_messages_;

    for (auto &MT : this->Merkle_trees_)
    {
        result.MT_roots_.emplace_back(MT.get_root());
    }

    /*
      Make sure all queries are hit.
    */
    for (std::size_t query_id = 0; query_id < this->query_registrations_.size(); ++query_id)
    {
        this->obtain_query_response(query_handle(query_id));
        /* this will populate oracle_id_to_query_positions_ (a hack used
           below in constructing the transcript to collect query positions) */
    }

    /*
      Go over all MTs and prepare multi membership proofs. (Now that we
      know both what the oracle messages are and where they'll be queried,
      we can find authentication paths.)
    */
    result.total_depth_without_pruning = 0;
    std::size_t MT_idx = 0;
    for (std::size_t round = 0; round < this->num_interaction_rounds_; ++round)
    {
        const domain_to_oracles_map mapping = this->oracles_in_round(round);
        const round_parameters<FieldT> round_params = this->get_round_parameters(round);

        /* Go over each oracle message in this round. */
        for (auto &kv : mapping)
        {
            /* Where is this oracle going to be queried? */
            std::set<std::size_t> query_positions_set;
            std::set<std::size_t> MT_leaf_positions_set;
            const std::size_t num_leaves = this->domains_[kv.first.id()].num_elements() / round_params.quotient_map_size_;
            for (auto oracle_h : kv.second)
            {
                for (auto pos : this->oracle_id_to_query_positions_[oracle_h.id()])
                {
                    query_positions_set.insert(pos);
                    MT_leaf_positions_set.insert(
                        query_position_to_merkle_tree_position(pos, num_leaves, round_params));
#ifdef DEBUG
                    printf("record: oracle %zu at position %zu\n", oracle_h.id(), pos);
#endif // DEBUG
                }
            }

            std::vector<std::size_t> query_positions(query_positions_set.begin(), query_positions_set.end());
            std::vector<std::size_t> MT_leaf_positions(MT_leaf_positions_set.begin(), MT_leaf_positions_set.end());

            /* What are the values at those query positions? */
            std::vector<std::vector<FieldT> > values;
            for (auto pos : query_positions)
            {
                std::vector<FieldT> column;
                for (auto oracle_h : kv.second)
                {
                    column.emplace_back(this->get_oracle_evaluation_at_point(std::make_shared<oracle_handle>(oracle_h), pos));
                }
                values.emplace_back(column);
            }

            result.total_depth_without_pruning += MT_leaf_positions.size() * this->Merkle_trees_[MT_idx].depth();

            result.query_positions_.emplace_back(query_positions);
            result.MT_leaf_positions_.emplace_back(MT_leaf_positions);
            result.query_responses_.emplace_back(values);

            /* Generate a combined authentication path for all those queries. */
            const merkle_tree_set_membership_proof proof =
                this->Merkle_trees_[MT_idx].get_set_membership_proof(MT_leaf_positions);

            result.MT_set_membership_proofs_.emplace_back(proof);

            ++MT_idx;
        }
    }

    if (this->is_preprocessing_)
    {
        this->remove_index_info_from_transcript(result);
    }

    return result;
}

template<typename FieldT>
std::size_t bcs_prover<FieldT>::MT_size() const
{
    std::size_t MT_size = 0;
    for (auto &MT : this->Merkle_trees_)
    {
        MT_size += MT.num_total_bytes();
    }

    return MT_size;
}

template<typename FieldT>
std::size_t bcs_prover<FieldT>::state_size() const
{
    return (this->num_bytes_across_all_oracles() + this->MT_size());
}

template<typename FieldT>
void bcs_prover<FieldT>::describe_sizes() const
{
    print_indent(); printf("* Total size of proof oracles (bytes): %zu\n", this->num_bytes_across_all_oracles());
    print_indent(); printf("* Total size of Merkle tree (bytes): %zu\n", this->MT_size());
    print_indent(); printf("* Total size of prover state (bytes): %zu\n", this->state_size());
}

} // namespace libiop
