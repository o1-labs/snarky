/**@file
 *****************************************************************************
 Hash function declarations and implementations.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_SNARK_COMMON_HASHING_HPP_
#define LIBIOP_SNARK_COMMON_HASHING_HPP_

#include <functional>
#include <string>
#include <vector>

namespace libiop {

typedef std::string hash_digest;

template<typename FieldT>
using field_element_hash_function = std::function<hash_digest(
        const std::vector<FieldT>&,
        const std::size_t)>;

typedef std::function<hash_digest(
    const hash_digest&,
    const hash_digest&,
    const std::size_t)> two_to_one_hash_function;

typedef std::function<hash_digest(
    const std::vector<uint8_t>&,
    const std::size_t)> zk_element_hash_function;

template<typename FieldT>
using FieldT_randomness_extractor_function = std::function<std::vector<FieldT>(
    const hash_digest&,
    const std::size_t,
    const std::size_t)>;

typedef std::function<std::size_t(
    const hash_digest&,
    const std::size_t,
    const std::size_t)> integer_randomness_extractor_function;

template<typename FieldT>
class hash_circuit_description
{
public:
    /* leaf hash and 2 to 1 hash complexities */
    static size_t arity_m_hash_complexity(
        const size_t m);
    static size_t hash_chain_complexity(
        const size_t sponge_state_size,
        const size_t input_size);
};

template<typename FieldT>
hash_digest blake2b_field_element_hash(const std::vector<FieldT> &data,
                                       const std::size_t digest_len_bytes);

template<typename FieldT>
std::vector<FieldT> blake2b_FieldT_randomness_extractor(const hash_digest &root,
                                                        const std::size_t index,
                                                        const std::size_t num_elements);

/* Returns a random integer of size less than upper_bound using input root, and key index */
std::size_t blake2b_integer_randomness_extractor(const hash_digest &root,
                                                 const std::size_t index,
                                                 const std::size_t upper_bound);

hash_digest blake2b_zk_element_hash(const std::vector<uint8_t> &first,
                                        const std::size_t digest_len_bytes);

hash_digest blake2b_two_to_one_hash(const hash_digest &first,
                                    const hash_digest &second,
                                    const std::size_t digest_len_bytes);

} // namespace libiop

#include "libiop/snark/common/hashing.tcc"

#endif // LIBIOP_SNARK_COMMON_HASHING_HPP_
