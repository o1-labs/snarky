#include <cstdint>
#include <gtest/gtest.h>
#include <vector>

#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/utils.hpp"
#include "libiop/snark/common/hashing.hpp"
#include "libiop/snark/common/merkle_tree.hpp"

namespace libiop {

template<typename FieldT>
void run_simple_MT_test(const std::size_t size, const std::size_t digest_len_bytes, const bool make_zk,
                        const std::size_t security_parameter) {
    merkle_tree<FieldT> tree(size,
                             blake2b_field_element_hash<FieldT>,
                             blake2b_zk_element_hash,
                             blake2b_two_to_one_hash,
                             digest_len_bytes,
                             make_zk,
                             security_parameter);

    const std::vector<FieldT> vec1 = random_vector<FieldT>(size);
    const std::vector<FieldT> vec2 = random_vector<FieldT>(size);

    tree.construct({ vec1, vec2 });

    const hash_digest root = tree.get_root();

    for (std::size_t i = 0; i < size; ++i)
    {
        /* membership proof for the set {i} */
        const std::vector<size_t> set = {i};

        const merkle_tree_set_membership_proof ap = tree.get_set_membership_proof(set);
        const hash_digest contents_hash = blake2b_field_element_hash<FieldT>(
            { vec1[i], vec2[i] }, digest_len_bytes);

        const bool is_valid = tree.validate_set_membership_proof(
            root,
            set,
            {contents_hash},
            ap);
        EXPECT_TRUE(is_valid);

        const hash_digest reverse_hash = blake2b_field_element_hash<FieldT>(
            { vec2[i], vec1[i] }, digest_len_bytes);

        const bool reverse_is_valid = tree.validate_set_membership_proof(
            root,
            set,
            {reverse_hash},
            ap);
        if (vec1[i] == vec2[i])
        {
            EXPECT_EQ(contents_hash, reverse_hash);
            EXPECT_TRUE(reverse_is_valid);
        }
        else
        {
            EXPECT_NE(contents_hash, reverse_hash);
            EXPECT_FALSE(reverse_is_valid);
        }
    }
}

TEST(MerkleTreeTest, SimpleTest) {
    typedef gf64 FieldT;

    const std::size_t size = 16;
    const std::size_t digest_len_bytes = 256/8;
    const std::size_t security_parameter = 128;
    run_simple_MT_test<FieldT>(size, digest_len_bytes, false, security_parameter);
}

TEST(MerkleTreeZKTest, SimpleTest) {
    typedef gf64 FieldT;

    const std::size_t size_small = 16;
    const std::size_t size_large = 1ull << 18; /* The goal is to test batch randomness logic */
    const std::size_t digest_len_bytes = 256/8;
    const std::size_t security_parameter = 128;
    run_simple_MT_test<FieldT>(size_small, digest_len_bytes, true, security_parameter);
    run_simple_MT_test<FieldT>(size_large, digest_len_bytes, true, security_parameter);
}

TEST(MerkleTreeTest, MultiTest) {
    typedef gf64 FieldT;

    const std::size_t size = 8;
    const std::size_t digest_len_bytes = 256/8;

    merkle_tree<FieldT> tree(size,
                             blake2b_field_element_hash<FieldT>,
                             blake2b_zk_element_hash,
                             blake2b_two_to_one_hash,
                             digest_len_bytes,
                             false,
                             128);

    const std::vector<FieldT> vec1 = random_vector<FieldT>(size);
    const std::vector<FieldT> vec2 = random_vector<FieldT>(size);

    tree.construct({ vec1, vec2 });

    const hash_digest root = tree.get_root();

    std::vector<hash_digest> vec_hashes;
    for (std::size_t i = 0; i < size; ++i)
    {
        hash_digest contents_hash = blake2b_field_element_hash<FieldT>(
            { vec1[i], vec2[i] }, digest_len_bytes);
        vec_hashes.emplace_back(contents_hash);
    }

    for (std::size_t subset = 0; subset < (1ull<<size); ++subset)
    {
        std::vector<std::size_t> subset_elements;
        std::vector<hash_digest> subset_hashes;
        for (std::size_t k = 0; k < size; ++k)
        {
            if (subset & (1ull<<k))
            {
                subset_elements.emplace_back(k);
                subset_hashes.emplace_back(vec_hashes[k]);
            }
        }

        const merkle_tree_set_membership_proof mp = tree.get_set_membership_proof(subset_elements);

        const bool is_valid = tree.validate_set_membership_proof(root,
                                                                   subset_elements,
                                                                   subset_hashes,
                                                                   mp);
        EXPECT_TRUE(is_valid);
    }
}

TEST(MerkleTreeZKTest, MultiTest) {
    typedef gf64 FieldT;

    const std::size_t size = 8;
    const std::size_t digest_len_bytes = 256/8;

    merkle_tree<FieldT> tree(size,
                             blake2b_field_element_hash<FieldT>,
                             blake2b_zk_element_hash,
                             blake2b_two_to_one_hash,
                             digest_len_bytes,
                             true,
                             128);

    const std::vector<FieldT> vec1 = random_vector<FieldT>(size);
    const std::vector<FieldT> vec2 = random_vector<FieldT>(size);

    tree.construct({ vec1, vec2 });

    const hash_digest root = tree.get_root();

    std::vector<hash_digest> vec_hashes;
    for (std::size_t i = 0; i < size; ++i)
    {
        hash_digest contents_hash = blake2b_field_element_hash<FieldT>(
            { vec1[i], vec2[i] }, digest_len_bytes);
        vec_hashes.emplace_back(contents_hash);
    }

    for (std::size_t subset = 0; subset < (1ull<<size); ++subset)
    {
        std::vector<std::size_t> subset_elements;
        std::vector<hash_digest> subset_hashes;
        for (std::size_t k = 0; k < size; ++k)
        {
            if (subset & (1ull<<k))
            {
                subset_elements.emplace_back(k);
                subset_hashes.emplace_back(vec_hashes[k]);
            }
        }

        const merkle_tree_set_membership_proof mp = tree.get_set_membership_proof(subset_elements);

        const bool is_valid = tree.validate_set_membership_proof(root,
                                                                   subset_elements,
                                                                   subset_hashes,
                                                                   mp);
        EXPECT_TRUE(is_valid);
    }
}

TEST(MerkleTreeTwoToOneHashTest, SimpleTest)
{
    typedef gf64 FieldT;
    bool make_zk = false;
    merkle_tree<FieldT> tree(8,
                             blake2b_field_element_hash<FieldT>,
                             blake2b_zk_element_hash,
                             blake2b_two_to_one_hash,
                             32,
                             make_zk,
                             128);
    std::vector<size_t> positions = {1, 3, 6, 7};
    size_t expected_num_hashes = 6;
    size_t actual_num_hashes = tree.count_hashes_to_verify_set_membership_proof(positions);
    ASSERT_EQ(expected_num_hashes, actual_num_hashes);
}

}
