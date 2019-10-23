#include <algorithm>
#include <stdexcept>

#include "libiop/common/cpp17_bits.hpp"
#include "libiop/common/common.hpp"
#include "libiop/common/profiling.hpp"

#include <sodium/randombytes.h>

namespace libiop {

template<typename FieldT>
merkle_tree<FieldT>::merkle_tree(const std::size_t num_leaves,
                                 const field_element_hash_function<FieldT> &leaf_hasher,
                                 const zk_element_hash_function &zk_leaf_hasher,
                                 const two_to_one_hash_function &node_hasher,
                                 const std::size_t digest_len_bytes,
                                 const bool make_zk,
                                 const std::size_t security_parameter) :
    num_leaves_(num_leaves),
    leaf_hasher_(leaf_hasher),
    zk_leaf_hasher_(zk_leaf_hasher),
    node_hasher_(node_hasher),
    digest_len_bytes_(digest_len_bytes),
    make_zk_(make_zk),
    num_zk_bytes_((security_parameter * 2 + 7) / 8) /* = ceil((2 * security_parameter_bits) / 8) */
{
    if (num_leaves < 2 || !is_power_of_2(num_leaves))
    {
        /* Handling num_leaves-1 Merkle trees adds little complexity but is not really worth it */
        throw std::invalid_argument("Merkle tree size must be a power of two, and at least 2.");
    }

    this->constructed_ = false;
}

template<typename FieldT>
void merkle_tree<FieldT>::sample_leaf_randomness()
{
    enter_block("BCS: Sample randomness");
    assert(this->zk_leaf_randomness_elements_.size() == 0);
    this->zk_leaf_randomness_elements_.reserve(this->num_leaves_);

    /* This uses a batch size since the libsodium API makes no guarantee for the maximum supported
    * vector size. */
    const size_t num_rand_bytes = this->num_leaves_ * this->num_zk_bytes_;
    const size_t rand_batch_size = 1ull << 20; /* 1 MB */
    const size_t num_batches = num_rand_bytes / rand_batch_size;
    const size_t leafs_per_batch = (num_batches == 0) ? 0 : this->num_leaves_ / num_batches;
    for (size_t i = 0; i < num_batches; i++) {
        std::vector<uint8_t> batch_randomness;
        batch_randomness.resize(rand_batch_size);
        randombytes_buf(&batch_randomness[0], rand_batch_size);
        std::vector<uint8_t>::const_iterator first = batch_randomness.begin();
        std::vector<uint8_t>::const_iterator last = batch_randomness.begin() + this->num_zk_bytes_;
        for (size_t i = 0; i < leafs_per_batch; ++i) {
            std::string rand_str(first, last);
            this->zk_leaf_randomness_elements_.emplace_back(rand_str);
            first += this->num_zk_bytes_;
            last += this->num_zk_bytes_;
        }
    }
    const size_t remaining_leaves = this->num_leaves_ - leafs_per_batch * num_batches;
    for (size_t i = 0; i < remaining_leaves; ++i)
    {
        std::vector<uint8_t> rand_leaf;
        rand_leaf.resize(this->num_zk_bytes_);
        randombytes_buf(&rand_leaf[0], this->num_zk_bytes_);
        std::string rand_str(rand_leaf.begin(), rand_leaf.end());
        this->zk_leaf_randomness_elements_.push_back(rand_str);
    }
    leave_block("BCS: Sample randomness");
}

template<typename FieldT>
void merkle_tree<FieldT>::construct(const std::vector<std::vector<FieldT> > &leaf_contents)
{
    std::vector<std::shared_ptr<std::vector<FieldT>>> shared_leaves;
    for (size_t i = 0; i < leaf_contents.size(); i++)
    {
        shared_leaves.emplace_back(
            std::make_shared<std::vector<FieldT>>(leaf_contents[i]));
    }
    this->construct_with_leaves_serialized_by_cosets(shared_leaves, 1);
}

template<typename FieldT>
void merkle_tree<FieldT>::construct(const std::vector<std::shared_ptr<std::vector<FieldT>>> &leaf_contents)
{
    this->construct_with_leaves_serialized_by_cosets(leaf_contents, 1);
}

template<typename FieldT>
void merkle_tree<FieldT>::construct_with_leaves_serialized_by_cosets(
    const std::vector<std::shared_ptr<std::vector<FieldT>> > &leaf_contents,
    const size_t coset_serialization_size)
{
    /* Check that the input is as expected */
    if (this->constructed_)
    {
        throw std::logic_error("Attempting to double-construct a Merkle tree.");
    }
    for (auto &v : leaf_contents)
    {
        if ((v->size() / coset_serialization_size) != this->num_leaves_)
        {
            throw std::logic_error("Attempting to construct a Merkle tree with a constituent vector of wrong size");
        }
    }

    /* Sample randomness for zk merkle trees */
    if (this->make_zk_)
    {
        this->sample_leaf_randomness();
    }

    this->inner_nodes_.resize(2 * this->num_leaves_ - 1);
    /* Domain with the same size as inputs, used for getting coset positions */
    field_subset<FieldT> leaf_domain(leaf_contents[0]->size());
    /* First hash the leaves. Since we are putting an entire coset into a leaf,
     * our slice is of size num_input_oracles * coset_size */
    std::vector<FieldT> slice(leaf_contents.size() * coset_serialization_size,
        FieldT::zero());
    for (std::size_t i = 0; i < this->num_leaves_; ++i)
    {
        std::vector<size_t> positions_in_this_slice =
            leaf_domain.all_positions_in_coset_i(i, coset_serialization_size);
        for (size_t j = 0; j < coset_serialization_size; j++)
        {
            for (size_t k = 0; k < leaf_contents.size(); k++)
            {
                slice[j + k*coset_serialization_size] =
                    leaf_contents[k]->operator[](positions_in_this_slice[j]);
            }
        }

        hash_digest digest;
        if (this->make_zk_)
        {
            const hash_digest orig_digest = this->leaf_hasher_(slice, this->digest_len_bytes_);
            const hash_digest randomness = this->zk_leaf_randomness_elements_[i];
            digest = this->node_hasher_(orig_digest, randomness, this->digest_len_bytes_);
        }
        else
        {
            digest = this->leaf_hasher_(slice, this->digest_len_bytes_);
        }
        this->inner_nodes_[(this->num_leaves_ - 1) + i] = digest;
    }

    /* Then hash all the layers */
    this->compute_inner_nodes();
    this->constructed_ = true;
}

template<typename FieldT>
std::vector<std::vector<FieldT>> merkle_tree<FieldT>::serialize_leaf_values_by_coset(
    const std::vector<size_t> &query_positions,
    const std::vector<std::vector<FieldT> > &query_responses,
    const size_t coset_serialization_size) const
{
    /* Domain with the same size as num_leaves * coset_serializiation_size */
    field_subset<FieldT> leaf_domain(this->num_leaves_ * coset_serialization_size);
    // Initialize all the columns
    std::vector<std::vector<FieldT>> MT_leaf_columns(
        query_positions.size() / coset_serialization_size);
    const size_t leaf_size = query_responses[0].size() * coset_serialization_size;
    for (size_t i = 0; i < MT_leaf_columns.size(); i++)
    {
        MT_leaf_columns[i] = std::vector<FieldT>(leaf_size);
    }
    /** Elements within a given coset appear in order,
     * so we simply store the index for the next element of the coset,
     * and increment as we see new positions belonging to this coset. */
    std::vector<size_t> intra_coset_index(MT_leaf_columns.size(), 0);
    std::map<size_t, size_t> MT_leaf_pos_to_response_index;
    size_t next_response_index = 0;
    for (size_t i = 0; i < query_positions.size(); i++)
    {
        const size_t query_position = query_positions[i];
        const size_t MT_leaf_index = leaf_domain.coset_index(query_position, coset_serialization_size);
        std::map<size_t, size_t>::iterator it = MT_leaf_pos_to_response_index.find(MT_leaf_index);
        /* For supported domain types, new MT leaf positions appear in order of query positions.
         * If we don't yet know the index of this leaf within the queried for leaves,
         * we can find it by simply incrementing the prior leaf's index. */
        if (it == MT_leaf_pos_to_response_index.end()) {
            MT_leaf_pos_to_response_index[MT_leaf_index] = next_response_index;
            next_response_index++;
        }
        const size_t MT_response_index = MT_leaf_pos_to_response_index[MT_leaf_index];
        const size_t index_in_coset = intra_coset_index[MT_response_index];
        intra_coset_index[MT_response_index]++;
        for (size_t j = 0; j < query_responses[i].size(); j++)
        {
            const size_t oracle_index = j*coset_serialization_size;
            MT_leaf_columns[MT_response_index][oracle_index + index_in_coset] =
                query_responses[i][j];
        }
    }
    return MT_leaf_columns;
}

template<typename FieldT>
void merkle_tree<FieldT>::compute_inner_nodes()
{
    // TODO: Better document this function, its hashing layer by layer.
    std::size_t n = (this->num_leaves_ - 1) / 2;
    while (true)
    {
        // TODO: Evaluate how much time is spent in hashing vs memory access.
        // For better memory efficiency, we could hash sub-tree by sub-tree
        // in an unrolled recursive fashion.
        for (std::size_t j = n; j <= 2*n; ++j)
        {
            const hash_digest& left = this->inner_nodes_[2*j + 1];
            const hash_digest& right = this->inner_nodes_[2*j + 2];
            const hash_digest digest = this->node_hasher_(left, right, this->digest_len_bytes_);

            this->inner_nodes_[j] = digest;
        }
        if (n > 0)
        {
            n /= 2;
        }
        else
        {
            break;
        }
    }
}

template<typename FieldT>
hash_digest merkle_tree<FieldT>::get_root() const
{
    if (!this->constructed_)
    {
        throw std::logic_error("Attempting to obtain a Merkle tree root without constructing the tree first.");
    }

    return inner_nodes_[0];
}

template<typename FieldT>
merkle_tree_set_membership_proof merkle_tree<FieldT>::get_set_membership_proof(
    const std::vector<std::size_t> &positions) const
{
    if (!this->constructed_)
    {
        throw std::logic_error("Attempting to obtain a Merkle tree authentication path without constructing the tree first.");
    }

    if (positions.empty())
    {
        merkle_tree_set_membership_proof result;
        return result;
    }

    std::vector<std::size_t> S = positions; /* sorted set of positions */
    std::sort(S.begin(), S.end());
    S.erase(std__unique(S.begin(), S.end()), S.end()); /* remove possible duplicates */

    if (std::any_of(S.begin(), S.end(),
                    [this](const std::size_t pos) { return pos >= this->num_leaves_; }))
    {
        throw std::invalid_argument("All positions must be between 0 and num_leaves-1.");
    }

    merkle_tree_set_membership_proof result;

    if (this->make_zk_)
    {
        /* add random hashes, in order, to the beginning (one for each query) */
        for (auto &pos : S)
        {
            const hash_digest random_digest = this->zk_leaf_randomness_elements_[pos];
            result.randomness_hashes.emplace_back(random_digest);
        }
    }

    /* now, add auxiliary hashes for the path from each query to the root, skipping overlaps */

    /* transform leaf positions to indices in this->inner_nodes_ */
    for (auto &pos : S)
    {
        pos += (this->num_leaves_ - 1);
    }

    while (true) /* for every layer */
    {
        auto it = S.begin();
        if (*it == 0 && it == --S.end())
        {
            /* we have arrived at the root */
            break;
        }

        std::vector<std::size_t> new_S;
        while (it != S.end())
        {
            const std::size_t it_pos = *it;
            auto next_it = ++it;

            /* Always process parent. */
            new_S.emplace_back((it_pos - 1)/2);

            if ((it_pos & 1) == 0)
            {
                /* We are the right node, so there was no left node
                   (o.w. would have been processed in b)
                   below). Insert it as auxiliary */
                result.auxiliary_hashes.emplace_back(this->inner_nodes_[it_pos - 1]);
            }
            else
            {
                /* We are the left node. Two cases: */
                if (next_it == S.end() || *next_it != it_pos + 1)
                {
                    /* a) Our right sibling is not in S, so we must
                       insert auxiliary. */
                    result.auxiliary_hashes.emplace_back(this->inner_nodes_[it_pos + 1]);
                }
                else
                {
                    /* b) Our right sibling is in S. So don't need
                       auxiliary and skip over the right sibling.
                       (Note that only one parent will be processed.)
                    */
                    ++next_it;
                }
            }
            it = next_it;
        }

        std::swap(S, new_S);
    }

    return result;
}

template<typename FieldT>
bool merkle_tree<FieldT>::validate_set_membership_proof(
    const hash_digest &root,
    const std::vector<std::size_t> &positions,
    const std::vector<hash_digest> &contents_hashes,
    const merkle_tree_set_membership_proof &proof)
{
    if (positions.size() != contents_hashes.size())
    {
        throw std::invalid_argument("The number of positions and hashes provided must match.");
    }

    if (positions.empty())
    {
        if (proof.auxiliary_hashes.empty())
        {
            return true;
        }
        else
        {
            throw std::invalid_argument("Invalid proof for the empty subset.");
        }
    }

    auto rand_it = proof.randomness_hashes.begin();
    auto aux_it = proof.auxiliary_hashes.begin();

    typedef std::pair<std::size_t, hash_digest> pos_and_digest_t;
    std::vector<pos_and_digest_t> S;
    S.reserve(positions.size());
    // TODO: Refactor this to have a single std::vector all contents hashes, and make each case modify that
    // with a single transform at the bottom.
    if (this->make_zk_)
    {
        std::vector<hash_digest> combined_hashes;
        for (auto &contents_hash : contents_hashes)
        {
            const hash_digest random_digest = *rand_it++;
            combined_hashes.emplace_back(this->node_hasher_(contents_hash, random_digest, this->digest_len_bytes_));
        }
        std::transform(positions.begin(), positions.end(), combined_hashes.begin(),
                   std::back_inserter(S),
                   [](const std::size_t pos, const hash_digest &hash) {
                       return std::make_pair(pos, hash);
                   });
    }
    else
    {
        std::transform(positions.begin(), positions.end(), contents_hashes.begin(),
                   std::back_inserter(S),
                   [](const std::size_t pos, const hash_digest &hash) {
                       return std::make_pair(pos, hash);
                   });
    }

    S.erase(std__unique(S.begin(), S.end()), S.end()); /* remove possible duplicates */

    if (std__adjacent_find(S.begin(), S.end(),
                           [] (const pos_and_digest_t &p1,
                               const pos_and_digest_t &p2) {
                               return (p1.first == p2.first && p1.second != p2.second);
                           }) != S.end())
    {
        throw std::invalid_argument("Duplicate position with unequal hash values.");
    }

    if (std::any_of(S.begin(), S.end(),
                    [this](const pos_and_digest_t &pos) {
                        return pos.first >= this->num_leaves_;
                    }))
    {
        throw std::invalid_argument("All positions must be between 0 and num_leaves-1.");
    }

    /* transform to sorted set of indices */
    for (auto &pos : S)
    {
        pos.first += (this->num_leaves_ - 1);
    }

    while (true) /* for every layer */
    {
        auto it = S.begin();
        if (it->first == 0 && it == --S.end())
        {
            /* we have arrived at the root */
            break;
        }

        std::vector<std::pair<std::size_t, hash_digest> > new_S;
        while (it != S.end())
        {
            const std::size_t it_pos = it->first;
            const hash_digest it_hash = it->second;

            auto next_it = ++it;

            hash_digest left_hash;
            hash_digest right_hash;

            if ((it_pos & 1) == 0)
            {
                /* We are the right node, so there was no left node
                   (o.w. would have been processed in b)
                   below). Take it from the auxiliary. */
                left_hash = *aux_it++;
                right_hash = it_hash;
            }
            else
            {
                /* We are the left node. Two cases: */
                left_hash = it_hash;

                if (next_it == S.end() || next_it->first != it_pos + 1)
                {
                    /* a) Our right sibling is not in S, so we must
                       take an auxiliary. */
                    right_hash = *aux_it++;
                }
                else
                {
                    /* b) Our right sibling is in S. So don't need
                       auxiliary and skip over the right sibling.
                       (Note that only one parent will be processed.)
                    */
                    right_hash = next_it->second;
                    ++next_it;
                }
            }

            const std::size_t parent_pos = (it_pos - 1)/2;
            const hash_digest parent_hash = this->node_hasher_(left_hash, right_hash,
                                                               this->digest_len_bytes_);
            new_S.emplace_back(std::make_pair(parent_pos, parent_hash));

            it = next_it;
        }

        std::swap(S, new_S);
    }

    if (aux_it != proof.auxiliary_hashes.end())
    {
        throw std::logic_error("Validation did not consume the entire proof.");
    }

    return (S.begin()->second == root);
}

template<typename FieldT>
size_t merkle_tree<FieldT>::count_hashes_to_verify_set_membership_proof(
    const std::vector<std::size_t> &positions) const
{
    /** This goes layer by layer,
     *  and counts the number of hashes needed to be computed.
     *  Essentially when moving up a layer in the verifier,
     *  every unique parent is one hash that has to be computed.  */
    size_t num_two_to_one_hashes =  0;
    std::vector<size_t> cur_pos_set = positions;
    sort(cur_pos_set.begin(), cur_pos_set.end());
    assert(cur_pos_set[cur_pos_set.size() - 1] < this->num_leaves());
    for (size_t cur_depth = this->depth(); cur_depth > 0; cur_depth--)
    {
        // contains positions in range [0, 2^{cur_depth - 1})
        std::vector<size_t> next_pos_set;
        for (size_t i = 0; i < cur_pos_set.size(); i++)
        {
            size_t parent_pos = cur_pos_set[i] / 2;
            // Check that parent pos isn't already in next pos set
            if (next_pos_set.size() == 0
                || next_pos_set[next_pos_set.size() - 1] != parent_pos)
            {
                next_pos_set.emplace_back(parent_pos);
            }
        }
        num_two_to_one_hashes += next_pos_set.size();
        cur_pos_set = next_pos_set;
    }
    return num_two_to_one_hashes;
}

template<typename FieldT>
std::size_t merkle_tree<FieldT>::num_leaves() const
{
    return (this->num_leaves_);
}

template<typename FieldT>
std::size_t merkle_tree<FieldT>::depth() const
{
    return log2(this->num_leaves_);
}

template<typename FieldT>
bool merkle_tree<FieldT>::zk() const
{
    return this->make_zk_;
}

template<typename FieldT>
std::size_t merkle_tree<FieldT>::num_total_bytes() const
{
    return (this->digest_len_bytes_ * (2 * this->num_leaves() - 1));
}


} // libiop
