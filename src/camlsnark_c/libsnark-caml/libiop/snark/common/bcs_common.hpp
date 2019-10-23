/**@file
 *****************************************************************************
 Specialized IOP that implements the BCS16 transformation.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_SNARK_COMMON_BCS16_IOP_HPP_
#define LIBIOP_SNARK_COMMON_BCS16_IOP_HPP_

#include <algorithm>
#include <cstddef>
#include <map>
#include <vector>

#include "libiop/iop/iop.hpp"
#include "libiop/snark/common/hashing.hpp"
#include "libiop/snark/common/merkle_tree.hpp"

namespace libiop {

template<typename FieldT>
struct bcs_transformation_parameters {
    std::size_t security_parameter; /* TODO: possibly revisit in the future */

    field_element_hash_function<FieldT> field_hasher;
    zk_element_hash_function zk_hasher;
    two_to_one_hash_function compression_hasher;
    FieldT_randomness_extractor_function<FieldT> FieldT_randomness_extractor;
    integer_randomness_extractor_function integer_randomness_extractor; /* for extracting query positions */
};

template<typename FieldT>
struct bcs_transformation_transcript {
/*
  When preprocessing, the transcript will not include the preprocessed MT roots,
  or prover messages, these are added back by the verifier.

  TODO:

  For now transcript contains all the query positions (but we do not
  report their size). This is because the most space-efficient way to
  implement Merkle tree pruning emits just auxiliary hash values (and
  assumes that the positions will be specified *AT ONCE*).

  However, verifier, interacting with IOP, makes queries one by
  one. Multiple options here:

  a) run verifier twice. Once giving fake answers (and using the
  learned query positions to verify the MT), and the second time --
  verification should be deterministic, e.g. no early aborts -- giving
  real answers.
  b) count query positions as part of the proof.
  c) have some infrastructure for "deferred"/lazily given answers answers.

  As we are now focusing on the proof size, we assume we will be doing
  (a), but for deadline hacks are having verifier with space
  complexity as (a/c), but time complexity as (b). This will get
  resolved.

  TODO:

  Investigate if MT roots can be omitted in ROM (?)
 */

    /* Explicit (non-oracle) prover messages. */
    std::vector<std::vector<FieldT> > prover_messages_;
    /* Each oracle message is compressed using a Merkle Tree. */
    std::vector<hash_digest> MT_roots_;
    /* Locations in codeword domain queried for each message. */
    std::vector<std::vector<std::size_t> > query_positions_;
    /* Each query response is a vector. */
    std::vector<std::vector<std::vector<FieldT> > > query_responses_;
    /* The MT leafs may be combinations of multiple query positions, so
     * we track these separately. */
    std::vector<std::vector<std::size_t> > MT_leaf_positions_;
    /* All authentication paths for a particular Merkle Tree are combined into a single
       "multi membership proof" for efficiency. */
    std::vector<merkle_tree_set_membership_proof> MT_set_membership_proofs_;

    /* just for benchmarking purposes -- total depth without pruning */
    std::size_t total_depth_without_pruning;

    std::size_t IOP_size_in_bytes() const;
    std::size_t BCS_size_in_bytes() const;
    std::size_t size_in_bytes() const;

    std::size_t BCS_size_in_bytes_without_pruning() const;
    std::size_t size_in_bytes_without_pruning() const;
};

/** The verification index is used in protocols that have an indexer */
template<typename FieldT>
struct bcs_verifier_index {
    std::vector<hash_digest> index_MT_roots_;
    std::vector<std::vector<FieldT>> indexed_messages_;
};

template<typename FieldT>
struct bcs_prover_index {
    std::vector<merkle_tree<FieldT>> index_MTs_;
    std::vector<std::vector<FieldT>> indexed_messages_;
    iop_prover_index<FieldT> iop_index_;
};

template<typename FieldT>
class bcs_protocol : public iop_protocol<FieldT> {
protected:
    size_t num_index_trees = 0;
    /* We use one Merkle Tree to compress each prover response oracle. */
    std::vector<merkle_tree<FieldT> > Merkle_trees_;
    /* Pseudorandom state is chained from one round to the next (using
       the new MT root) by both the prover and the verifier and is used
       to generate "random" verifier messages. */
    /** TODO: Finalize on state depending on primary input */
    std::vector<hash_digest> pseudorandom_state_;
    bcs_transformation_parameters<FieldT> parameters_;
    std::vector<round_parameters<FieldT>> round_params_;

    std::size_t digest_len_bytes_;
public:
    bcs_protocol(const bcs_transformation_parameters<FieldT> &parameters);
    void set_round_parameters(const round_parameters<FieldT> &params);
    virtual void seal_interaction_registrations();

    /* Used in profiling */
    std::vector<size_t> get_MT_depths() const;
    std::vector<bool> get_MT_zk_flags() const;
    std::vector<round_parameters<FieldT>> get_all_round_params() const;
protected:
    round_parameters<FieldT> get_round_parameters(const std::size_t round) const;
    virtual std::size_t obtain_random_query_position(const random_query_position_handle &position);

    hash_digest compute_message_hash(const size_t round,
                                     const std::vector<std::vector<FieldT>> &all_prover_messages) const;

    void serialize_leaf_data_by_round_params(const std::vector<FieldT> &oracle_evaluated_contents,
                                             std::vector<std::vector<FieldT>> &all_oracles_evaluated_contents,
                                             const domain_handle &evaluation_domain,
                                             const round_parameters<FieldT> &round_params);
};

template<typename FieldT>
std::size_t query_position_to_merkle_tree_position(const std::size_t query_position,
                                                   const std::size_t num_leaves,
                                                   const round_parameters<FieldT> &round_params);

template<typename FieldT>
void print_detailed_transcript_data(
    const bool holographic,
    const bcs_transformation_transcript<FieldT> &transcript,
    const bcs_transformation_parameters<FieldT> &params,
    const std::vector<size_t> MT_depths,
    const std::vector<bool> make_zk,
    const std::vector<round_parameters<FieldT>> round_params);

} // namespace libiop

#include "libiop/snark/common/bcs_common.tcc"

#endif // LIBIOP_SNARK_COMMON_BCS16_IOP_HPP_
