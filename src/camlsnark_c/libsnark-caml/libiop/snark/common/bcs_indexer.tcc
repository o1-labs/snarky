namespace libiop {

template<typename FieldT>
bcs_indexer<FieldT>::bcs_indexer(const bcs_transformation_parameters<FieldT> &parameters) :
    bcs_protocol<FieldT>(parameters)
{
}

template<typename FieldT>
void bcs_indexer<FieldT>::signal_prover_round_done()
{
    throw std::invalid_argument("Indexer should not be used for proving"
        " (signal prover round done called)");
}

template<typename FieldT>
void bcs_indexer<FieldT>::signal_index_submissions_done()
{
    enter_block("Merkelize indexed oracles");
    iop_protocol<FieldT>::signal_prover_round_done();
    std::size_t ended_round = this->num_prover_rounds_done_-1;
    if (ended_round != 0)
    {
        throw std::invalid_argument("Index submissions should be round 0");
    }
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
            std::shared_ptr<std::vector<FieldT>> oracle_contents = this->oracles_[v.id()].evaluated_contents();
            all_evaluated_contents.emplace_back(oracle_contents);
        }
        enter_block("Construct Merkle tree");
        this->Merkle_trees_[this->MTs_processed_].construct_with_leaves_serialized_by_cosets(
            all_evaluated_contents, round_params.quotient_map_size_);
        leave_block("Construct Merkle tree");
        cur_state = this->parameters_.compression_hasher(cur_state,
                                                         this->Merkle_trees_[this->MTs_processed_].get_root(),
                                                         this->digest_len_bytes_);

        ++this->MTs_processed_;
        /* Now make the oracles in a form suitable for creating an index */
        for (auto &v : kv.second)
        {
            std::shared_ptr<std::vector<FieldT>> oracle_contents = this->oracles_[v.id()].evaluated_contents();
            this->indexed_oracles_.emplace_back(*oracle_contents.get());
            this->oracles_[v.id()].erase_contents();
        }
    }

    /* Hash explicitly sent prover messages */
    const hash_digest message_hash = this->compute_message_hash(ended_round, this->prover_messages_);
    cur_state = this->parameters_.compression_hasher(cur_state, message_hash, this->digest_len_bytes_);

    /* Add the prover message hash as a "root" and update the pseudorandom state */
    this->pseudorandom_state_.emplace_back(cur_state);

    leave_block("Merkelize indexed oracles");
}

template<typename FieldT>
std::vector<FieldT> bcs_indexer<FieldT>::obtain_verifier_random_message(
    const verifier_random_message_handle &random_message)
{
    throw std::invalid_argument("Should not be calling this on the indexing IOP");
}

template<typename FieldT>
FieldT bcs_indexer<FieldT>::obtain_query_response(const query_handle &query)
{
    throw std::invalid_argument("Should not be calling this on the indexing IOP");
}

template<typename FieldT>
bcs_verifier_index<FieldT> bcs_indexer<FieldT>::get_verifier_index()
{
    bcs_verifier_index<FieldT> index;
    for (size_t i = 0; i < this->MTs_processed_; i++)
    {
        index.index_MT_roots_.emplace_back(this->Merkle_trees_[i].get_root());
    }
    index.indexed_messages_ = this->prover_messages_;
    return index;
}

template<typename FieldT>
bcs_prover_index<FieldT> bcs_indexer<FieldT>::get_bcs_prover_index()
{
    if (this->get_prover_index_has_been_called_)
    {
        printf("Prover index has already been extracted from object "
            "due to memory optimizations, this operation can only be done once.\n");
        throw std::invalid_argument("Prover index has already been extracted");
    }
    bcs_prover_index<FieldT> index;
    index.index_MTs_ = this->Merkle_trees_;
    index.index_MTs_.erase(index.index_MTs_.begin() + this->MTs_processed_,
        index.index_MTs_.end());
    index.indexed_messages_ = this->prover_messages_;
    index.indexed_messages_.erase(
        index.indexed_messages_.begin() + this->num_prover_messages_at_end_of_round_[0],
        index.indexed_messages_.end());
    std::swap(index.iop_index_.all_oracle_evals_, this->indexed_oracles_);
    index.iop_index_.prover_messages_ = index.indexed_messages_;
    this->get_prover_index_has_been_called_ = true;
    return index;
}

} // namespace libiop
