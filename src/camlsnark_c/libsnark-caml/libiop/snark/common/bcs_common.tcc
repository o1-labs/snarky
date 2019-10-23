#include <algorithm>
#include <numeric>
#include <set>

#include "libiop/algebra/fft.hpp"
#include "libiop/common/profiling.hpp"

namespace libiop {

template<typename FieldT>
std::size_t bcs_transformation_transcript<FieldT>::IOP_size_in_bytes() const
{

    const size_t field_size =
        (log_of_field_size_helper<FieldT>(FieldT::zero()) + 7) / 8;
    const std::size_t prover_messages_length =
        std::accumulate(this->prover_messages_.begin(),
                        this->prover_messages_.end(),
                        0,
                        [] (const std::size_t av,
                            const std::vector<FieldT> &msg) {
                            return av + msg.size();
                        });
    const std::size_t prover_messages_size =
        field_size * prover_messages_length;

    const std::size_t query_responses_length =
        std::accumulate(this->query_responses_.begin(),
                        this->query_responses_.end(),
                        0,
                        [] (const std::size_t av,
                            const std::vector<std::vector<FieldT> > &resp) {
                            return av + (resp.empty() ? 0 : resp.size() * resp[0].size());
                        });
    const std::size_t query_responses_size =
        field_size * query_responses_length;

    return (prover_messages_size +
            query_responses_size);
}


template<typename FieldT>
std::size_t bcs_transformation_transcript<FieldT>::BCS_size_in_bytes() const
{
    const std::size_t MT_roots_size =
        std::accumulate(this->MT_roots_.begin(),
                        this->MT_roots_.end(),
                        0,
                        [] (const std::size_t av, const hash_digest &h) {
                            return av + h.size();
                        });

    const std::size_t MT_set_membership_proofs_size =
        std::accumulate(this->MT_set_membership_proofs_.begin(),
                        this->MT_set_membership_proofs_.end(),
                        0,
                        [] (const std::size_t av,
                            const merkle_tree_set_membership_proof &pi) {
                            return av + pi.size_in_bytes();
                        });

    return (MT_roots_size +
            MT_set_membership_proofs_size);
}

template<typename FieldT>
std::size_t bcs_transformation_transcript<FieldT>::size_in_bytes() const
{
    return (this->IOP_size_in_bytes() +
            this->BCS_size_in_bytes());
}

template<typename FieldT>
std::size_t bcs_transformation_transcript<FieldT>::BCS_size_in_bytes_without_pruning() const
{
    const std::size_t MT_roots_size =
        std::accumulate(this->MT_roots_.begin(),
                        this->MT_roots_.end(),
                        0,
                        [] (const std::size_t av, const hash_digest &h) {
                            return av + h.size();
                        });

    const std::size_t digest_size_bytes = this->MT_roots_[0].size();

    return (MT_roots_size + digest_size_bytes * total_depth_without_pruning);
}

template<typename FieldT>
std::size_t bcs_transformation_transcript<FieldT>::size_in_bytes_without_pruning() const
{
    return (this->IOP_size_in_bytes() +
            this->BCS_size_in_bytes_without_pruning());
}

template<typename FieldT>
bcs_protocol<FieldT>::bcs_protocol(const bcs_transformation_parameters<FieldT> &parameters) :
    iop_protocol<FieldT>(),
    parameters_(parameters)
{
    this->digest_len_bytes_ = 2 * (this->parameters_.security_parameter / 8);
    printf("\nBCS parameters\n");
    print_indent(); printf("* digest_len (bytes) = %zu\n", this->digest_len_bytes_);
    print_indent(); printf("* digest_len (bits) = %zu\n", 8 * this->digest_len_bytes_);
}

template<typename FieldT>
void bcs_protocol<FieldT>::seal_interaction_registrations()
{
    iop_protocol<FieldT>::seal_interaction_registrations();

    /* Now that all the interactions have been registered, we know how
       many messages there are, so we can prepare the Merkle trees. */
    for (std::size_t round = 0; round < this->num_interaction_rounds_; ++round)
    {
        const domain_to_oracles_map mapping = this->oracles_in_round(round);
        const round_parameters<FieldT> round_params = this->get_round_parameters(round);

        /* Don't double instantiate holographic MTs in the prover */
        if (this->is_holographic_ && round == 0)
        {
            /** The prover will already have the indexed MTs instantiated here,
             *  the verifier won't. So if we are in the prover,
             *  the following check will pass*/
            if (this->Merkle_trees_.size() > 0)
            {
                continue;
            }
        }
        for (auto &kv : mapping)
        {
            // Make the Merkle tree ZK if at least one oracle in this merkle tree is zk.
            bool make_zk = false;
            for (size_t i = 0; i < kv.second.size(); i++)
            {
                const size_t id = kv.second[i].id();
                const oracle_registration registration = this->oracle_registrations_[id];
                if (registration.make_zk())
                {
                    make_zk = true;
                    break;
                }
            }
            /* Make this a per-oracle setting as well */
            const std::size_t size = this->domains_[kv.first.id()].num_elements() / round_params.quotient_map_size_;
            const merkle_tree<FieldT> MT(size,
                                         this->parameters_.field_hasher,
                                         this->parameters_.zk_hasher,
                                         this->parameters_.compression_hasher,
                                         this->digest_len_bytes_,
                                         make_zk,
                                         this->parameters_.security_parameter);
            this->Merkle_trees_.emplace_back(MT);
        }
    }
}

template<typename FieldT>
void bcs_protocol<FieldT>::set_round_parameters(const round_parameters<FieldT> &params) {
    /* Rounds are 0 indexed. */
    std::size_t cur_round = this->num_interaction_rounds();
    if (cur_round == this->round_params_.size() - 1) {
        throw std::logic_error("Already set round parameters for this round");
    }
    /* Set blank round params until one round before current param. */
    while (this->round_params_.size() < cur_round) {
        this->round_params_.emplace_back(round_parameters<FieldT>());
    }
    this->round_params_.emplace_back(params);
}

template<typename FieldT>
std::vector<size_t> bcs_protocol<FieldT>::get_MT_depths() const
{
    std::vector<size_t> depths;
    for (size_t i = 0; i < this->Merkle_trees_.size(); i++)
    {
        depths.emplace_back(this->Merkle_trees_[i].depth());
    }
    return depths;
}

template<typename FieldT>
std::vector<bool> bcs_protocol<FieldT>::get_MT_zk_flags() const
{
    std::vector<bool> make_zk_flags;
    for (size_t i = 0; i < this->Merkle_trees_.size(); i++)
    {
        make_zk_flags.emplace_back(this->Merkle_trees_[i].zk());
    }
    return make_zk_flags;
}

template<typename FieldT>
std::vector<round_parameters<FieldT>> bcs_protocol<FieldT>::get_all_round_params() const
{
    std::vector<round_parameters<FieldT>> all_round_params;
    for (size_t i = 0; i < this->Merkle_trees_.size(); i++)
    {
        all_round_params.emplace_back(this->get_round_parameters(i));
    }
    return all_round_params;
}

template<typename FieldT>
round_parameters<FieldT> bcs_protocol<FieldT>::get_round_parameters(const std::size_t round) const {
    if (round >= this->round_params_.size()) {
        return round_parameters<FieldT>();
    }
    return this->round_params_[round];
}

template<typename FieldT>
std::size_t bcs_protocol<FieldT>::obtain_random_query_position(const random_query_position_handle &position)
{
    /* Always use the last pseudorandom state */
    const std::size_t subspace_size = this->domains_[
        this->random_query_position_registrations_[position.id()].domain().id()].num_elements();
    const std::size_t result =
        this->parameters_.integer_randomness_extractor(*this->pseudorandom_state_.rbegin(),
                                                       position.id(),
                                                       subspace_size);
    return result;
}

template<typename FieldT>
hash_digest bcs_protocol<FieldT>::compute_message_hash(
    const size_t round,
    const std::vector<std::vector<FieldT> > &all_prover_messages) const
{
    /* Hash explicitly sent prover messages */
    const std::size_t min_message_id =
        (round == 0 ? 0 : this->num_prover_messages_at_end_of_round_[round - 1]);
    const std::size_t max_message_id = this->num_prover_messages_at_end_of_round_[round];

    std::vector<FieldT> message_concat = { FieldT::zero() };
    for (std::size_t message_id = min_message_id; message_id < max_message_id; ++message_id)
    {
        message_concat.insert(message_concat.end(),
                              all_prover_messages[message_id].begin(),
                              all_prover_messages[message_id].end());

    }
#ifdef DEBUG
    printf("Message concat (min_message_id=%zu, max_message_id=%zu, num_prover_rounds_done-1=%zu:\n",
           min_message_id,
           max_message_id,
           ended_round);
    for (auto &v : message_concat)
    {
        v.print();
    }
#endif // DEBUG

    return this->parameters_.field_hasher(message_concat, this->digest_len_bytes_);
}

/* Serializes the provided oracle's evaluated contents into multiple rows,
   and appends these rows to all_oracles_evaluated_contents.
   It is serialized into multiple rows according to the quotient map relation in round params.
   The default round parameters result in oracle_evaluated_contents being appended as a single row. */
template<typename FieldT>
void bcs_protocol<FieldT>::serialize_leaf_data_by_round_params(
    const std::vector<FieldT> &oracle_evaluated_contents,
    std::vector<std::vector<FieldT>> &all_evaluated_contents,
    const domain_handle &evaluation_domain_handle,
    const round_parameters<FieldT> &round_params)
{
    if (round_params.quotient_map_size_ == 1) {
        all_evaluated_contents.emplace_back(oracle_evaluated_contents);
        return;
    }
    std::size_t start_row = all_evaluated_contents.size();
    /** Add a vector initialized to 0 to all_evaluated_contents,
     *  quotient_map_size times. */
    std::size_t num_cosets = oracle_evaluated_contents.size() / round_params.quotient_map_size_;
    for (std::size_t i = 0; i < round_params.quotient_map_size_; i++) {
        std::vector<FieldT> vector_for_ith_element_of_each_coset(num_cosets);
        all_evaluated_contents.emplace_back(std::move(vector_for_ith_element_of_each_coset));
    }
    /** Each leaf will now contain the evaluations of every element of the coset being queried.
     *  This is done such that within a leaf's column, the elements of the coset appear in order. */
    field_subset<FieldT> evaluation_domain = this->get_domain(evaluation_domain_handle);
    if (evaluation_domain.type() != round_params.quotient_map_type_)
    {
        throw std::invalid_argument("round params' domain type does not match the oracle evaluation's domain.");
    }
    if (evaluation_domain.type() == affine_subspace_type) {
        /** This assumes that the quotient map domain's basis vectors are the first
         *  log_2(|Q|) basis vectors of the evaluation domain.
         *
         *  When this is the case, elements of the same coset are consecutive
         *  elements of the evaluation domain.
         *  This follows from how we index subspaces.
         */
        std::size_t coset_index = 0; /* Which element within a single coset are we considering */
        for (std::size_t i = 0; i < oracle_evaluated_contents.size(); i++) {
            all_evaluated_contents[start_row + coset_index][i / round_params.quotient_map_size_]
                = oracle_evaluated_contents[i];
            coset_index = (coset_index + 1) % round_params.quotient_map_size_;
        }
    } else if (evaluation_domain.type() == multiplicative_coset_type) {
        /** In the multiplicative setting,
         *  Let i be the index of element x \in evaluation_domain.
         *  Then [x] has elements with indices i, i + |H|/|Q|, i + 2|H|/|Q|, ... i + (|Q| - 1) |H|/|Q|.
         *  These are then grouped into the same leaf.
         *
         *  For cache efficiency, this is done by iterating through every 1st element of a coset, then 2nd element, etc.
         */
        for (std::size_t i = 0; i < round_params.quotient_map_size_; i++)
        {
            // TODO: Replace this loop with std::move or just direct copying for better memory efficiency
            for (std::size_t j = 0; j < num_cosets; j++)
            {
                all_evaluated_contents[start_row + i][j] =
                    oracle_evaluated_contents[i*num_cosets + j];
            }
        }
    } else {
        throw std::invalid_argument("BCS16 IOP - Unknown field_subset type for quotient map domain");
    }
}

template<typename FieldT>
std::size_t query_position_to_merkle_tree_position(const std::size_t query_position,
                                                   const std::size_t num_leaves,
                                                   const round_parameters<FieldT> &round_params)
{
    if (round_params.quotient_map_size_ == 1) {
        return query_position;
    }
    if (round_params.quotient_map_type_ == affine_subspace_type) {
        return query_position / round_params.quotient_map_size_;
    } else if (round_params.quotient_map_type_ == multiplicative_coset_type) {
        return query_position % num_leaves;
    }
    return 0;
}

template<typename FieldT>
void print_detailed_transcript_data(
    const bool holographic,
    const bcs_transformation_transcript<FieldT> &transcript,
    const bcs_transformation_parameters<FieldT> &params,
    const std::vector<size_t> MT_depths,
    const std::vector<bool> make_zk,
    const std::vector<round_parameters<FieldT>> round_params)
{
    /* Calculate round by round details */

    const size_t digest_len_bytes = 2 * (params.security_parameter / 8);
    const size_t field_size = (log_of_field_size_helper<FieldT>(FieldT::zero()) + 7) / 8;
    std::vector<size_t> two_to_one_hashes_by_round;
    std::vector<size_t> leaf_hashes_by_round;
    std::vector<size_t> zk_hashes_by_round;
    std::vector<size_t> IOP_size_by_round;
    std::vector<size_t> BCS_size_by_round;
    size_t total_prover_message_size;

    for (size_t round = 0; round < MT_depths.size(); round++)
    {
        const size_t MT_size = 1ull << MT_depths[round];
        merkle_tree<FieldT> MT(MT_size,
                               params.field_hasher,
                               params.zk_hasher,
                               params.compression_hasher,
                               digest_len_bytes,
                               false,
                               params.security_parameter);

        /** We have to merge the query positions that correspond to the same
            leaf after applying the round parameters */
        std::vector<size_t> query_positions;
        for (size_t i = 0; i < transcript.query_positions_[round].size(); i++)
        {
            size_t query_position = transcript.query_positions_[round][i];
            size_t MT_position = query_position_to_merkle_tree_position(
                query_position, MT_size, round_params[round]);
            if(std::find(query_positions.begin(), query_positions.end(), MT_position) == query_positions.end()) {
                /* query positions does not contain x */
                query_positions.emplace_back(MT_position);
            }
        }
        size_t num_two_to_one_hashes_in_round =
            MT.count_hashes_to_verify_set_membership_proof(
            query_positions);
        two_to_one_hashes_by_round.emplace_back(num_two_to_one_hashes_in_round);
        const size_t num_values_per_leaf = transcript.query_responses_[round][0].size();
        const size_t num_leaves = transcript.query_responses_[round].size();
        leaf_hashes_by_round.emplace_back(num_values_per_leaf * num_leaves);

        if (make_zk[round]) {
            zk_hashes_by_round.emplace_back(num_leaves);
        } else {
            zk_hashes_by_round.emplace_back(0);
        }

        // TODO: Should we change sizeof(FieldT) to FieldT::num_bits / 8
        IOP_size_by_round.emplace_back(
            leaf_hashes_by_round[round] * field_size);
        /* MT root + membership proof size (includes zk hash) */
        BCS_size_by_round.emplace_back(
            transcript.MT_set_membership_proofs_[round].size_in_bytes()
            + digest_len_bytes);
    }
    size_t num_prover_messages = 0;
    for (size_t i = 0; i < transcript.prover_messages_.size(); i++)
    {
        num_prover_messages += transcript.prover_messages_[i].size();
    }
    total_prover_message_size = num_prover_messages * field_size;
    if (holographic)
    {
        BCS_size_by_round[0] -= digest_len_bytes;
    }

    /* Print summary of argument size first */
    printf("\n");

    print_indent(); printf("* Argument size in bytes (IOP): %zu\n", transcript.IOP_size_in_bytes());
    print_indent(); printf("* Argument size in bytes (BCS): %zu\n", transcript.BCS_size_in_bytes());
    print_indent(); printf("* Argument size in bytes (total): %zu\n", transcript.size_in_bytes());

    printf("\nIf we were to remove pruning of authentication paths in BCS,\n"
            "the argument would have the following sizes:\n");
    print_indent(); printf("* Argument size in bytes (BCS, no pruning): %zu\n", transcript.BCS_size_in_bytes_without_pruning());
    print_indent(); printf("* Argument size in bytes (total, no pruning): %zu\n", transcript.size_in_bytes_without_pruning());

    printf("\n");
    printf("total prover messages size: %lu\n", total_prover_message_size);
    const size_t total_two_to_one_hashes = std::accumulate(
        two_to_one_hashes_by_round.begin(), two_to_one_hashes_by_round.end(), 0);
    const size_t total_leaves_hashed = std::accumulate(
        leaf_hashes_by_round.begin(), leaf_hashes_by_round.end(), 0);
    const size_t total_zk_hashes = std::accumulate(
        zk_hashes_by_round.begin(), zk_hashes_by_round.end(), 0);
    const size_t total_hashes = total_two_to_one_hashes + total_leaves_hashed + total_zk_hashes;
    printf("total two to one hashes: %lu\n", total_two_to_one_hashes);
    printf("total leaves hashed: %lu\n", total_leaves_hashed);
    printf("total hashes: %lu\n", total_hashes);
    printf("\n");

    printf("Transcript info by round\n");
    printf("Per round IOP sizes don't include prover messages\n");
    for (size_t round = 0; round < MT_depths.size(); round++)
    {
        printf("\nround %lu\n", round);
        printf("MT_depth %lu\n", MT_depths[round]);
        printf("IOP size: %lu bytes\n", IOP_size_by_round[round]);
        printf("BCS size: %lu bytes\n", BCS_size_by_round[round]);
        printf("number of two to one hashes: %lu\n", two_to_one_hashes_by_round[round]);
        printf("number of leaves hashed: %lu\n", leaf_hashes_by_round[round]);
        if (make_zk[round])
        {
            printf("number of zk hashes: %lu\n", zk_hashes_by_round[round]);
        }
    }
    printf("\n\n");
}

} // namespace libiop
