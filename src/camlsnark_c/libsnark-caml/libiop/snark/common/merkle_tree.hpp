/**@file
 *****************************************************************************
 Merkle tree interfaces.

 Includes support for zero knowledge merkle trees, and set membership-proofs.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_SNARK_COMMON_MERKLE_TREE_HPP_
#define LIBIOP_SNARK_COMMON_MERKLE_TREE_HPP_

#include <cstddef>
#include <numeric>
#include <vector>
#include <bits/stdc++.h>

#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/snark/common/hashing.hpp"

namespace libiop {

/* Authentication paths for a set of positions */
struct merkle_tree_set_membership_proof {
    std::vector<hash_digest> auxiliary_hashes;
    std::vector<hash_digest> randomness_hashes;

    /* TODO: Write a test for this */
    std::size_t size_in_bytes() const
    {
        return std::accumulate(this->auxiliary_hashes.begin(),
                               this->auxiliary_hashes.end(),
                               0,
                               [] (const std::size_t av, const hash_digest &h) { return av + h.size(); }) +
               std::accumulate(this->randomness_hashes.begin(),
                               this->randomness_hashes.end(),
                               0,
                               [] (const std::size_t av, const hash_digest &h) { return av + h.size(); });
    }
};

template<typename FieldT>
class merkle_tree {
protected:
    bool constructed_;
    std::vector<hash_digest> inner_nodes_;

    std::size_t num_leaves_;
    field_element_hash_function<FieldT> leaf_hasher_;
    zk_element_hash_function zk_leaf_hasher_;
    two_to_one_hash_function node_hasher_;
    std::size_t digest_len_bytes_;
    bool make_zk_;
    std::size_t num_zk_bytes_;

    /* Each element will be hashed (individually) to produce a random hash digest. */
    std::vector<hash_digest> zk_leaf_randomness_elements_;
    void sample_leaf_randomness();
    void compute_inner_nodes();
public:
    /* Create a merkle tree with the given configuration.
    If make_zk is true, 2 * security parameter random bytes will be appended to each leaf
    before hashing, to prevent a low entropy leaf value from being inferred
    from its hash. */
    merkle_tree(const std::size_t num_leaves,
                const field_element_hash_function<FieldT> &leaf_hasher,
                const zk_element_hash_function &zk_leaf_hasher,
                const two_to_one_hash_function &node_hasher,
                const std::size_t digest_len_bytes,
                const bool make_zk,
                const std::size_t security_parameter);

    /** This treats each leaf as a column.
     * e.g. The ith leaf is the vector formed by leaf_contents[j][i] for all j */
    void construct(const std::vector<std::shared_ptr<std::vector<FieldT>>> &leaf_contents);
    // TODO: Remove this overload in favor of only using the former
    void construct(const std::vector<std::vector<FieldT> > &leaf_contents);
    /** Leaf contents is a table with `r` rows
     *  (`r` typically being the number of oracles)
     *  and (MT_num_leaves * coset_serialization_size) columns.
     *  Each MT leaf is the serialization of a table with `r` rows,
     *  and coset_serialization_size columns.
     *
     *  This is done here rather than the BCS layer to avoid needing to copy the data,
     *  as this will take a significant amount of memory.
     */
    void construct_with_leaves_serialized_by_cosets(
        const std::vector<std::shared_ptr<std::vector<FieldT>>> &leaf_contents,
        size_t coset_serialization_size);

    /** Takes in a set of query positions to input oracles to a domain of size:
     *  `num_leaves * coset_serialization_size`,
     *  and the associated evaluations for each query position.
     *
     *  This function then serializes these evaluations into leaf entries.
     *  The rows of a leaf entry are the same as in the eva
    */
    std::vector<std::vector<FieldT>> serialize_leaf_values_by_coset(
        const std::vector<size_t> &query_positions,
        const std::vector<std::vector<FieldT> > &query_responses,
        const size_t coset_serialization_size) const;

    hash_digest get_root() const;

    merkle_tree_set_membership_proof get_set_membership_proof(
        const std::vector<std::size_t> &positions) const;
    bool validate_set_membership_proof(
        const hash_digest &root,
        const std::vector<std::size_t> &positions,
        const std::vector<hash_digest> &contents_hashes,
        const merkle_tree_set_membership_proof &proof);

    /* Returns number of two to one hashes */
    size_t count_hashes_to_verify_set_membership_proof(
        const std::vector<std::size_t> &positions) const;

    std::size_t num_leaves() const;
    std::size_t depth() const;
    bool zk() const;
    std::size_t num_total_bytes() const;
};

} // namespace libiop

#include "libiop/snark/common/merkle_tree.tcc"

#endif // LIBIOP_SNARK_COMMON_MERKLE_TREE_HPP_
