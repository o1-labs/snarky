#include "sodium/crypto_generichash_blake2b.h"
#include "libiop/algebra/fields/utils.hpp"
#include <cstring>
#include <stdexcept>

namespace libiop {

template<typename FieldT>
hash_digest blake2b_field_element_hash(const std::vector<FieldT> &data,
                                       const std::size_t digest_len_bytes)
{

    hash_digest result(digest_len_bytes, 'X');

    /* see https://download.libsodium.org/doc/hashing/generic_hashing.html */
    const int status = crypto_generichash_blake2b((unsigned char*)&result[0],
                                                  digest_len_bytes,
                                                  (result.empty() ? NULL : (unsigned char*)&data[0]),
                                                  sizeof(FieldT) * data.size(),
                                                  NULL, 0);
    if (status != 0)
    {
        throw std::runtime_error("Got non-zero status from crypto_generichash_blake2b. (Is digest_len_bytes correct?)");
    }


    return result;
}

template<typename FieldT>
FieldT blake2b_FieldT_rejection_sample(
    typename libiop::enable_if<is_additive<FieldT>::value, FieldT>::type _,
    unsigned char* root_plus_index,
    size_t root_plus_index_size,
    const size_t key,
    const size_t key_increment)
{
    /* No need for rejection sampling, since our binary fields are word-aligned */
    FieldT el;
    const int status = crypto_generichash_blake2b((unsigned char*)&el,
                                                   sizeof(el),
                                                   root_plus_index,
                                                   root_plus_index_size,
                                                   (unsigned char*)&key, sizeof(key));
    if (status != 0)
    {
        throw std::runtime_error("Got non-zero status from crypto_generichash_blake2b. (Is digest_len_bytes correct?)");
    }
    return el;
}

template<typename FieldT>
FieldT blake2b_FieldT_rejection_sample(
    typename libiop::enable_if<is_multiplicative<FieldT>::value, FieldT>::type _,
    unsigned char* root_plus_index,
    size_t root_plus_index_size,
    const size_t key,
    const size_t key_increment)
{
    FieldT el;
    bool valid = false;
    size_t cur_key = key;
    const size_t bits_per_limb = 8 * sizeof(mp_limb_t);
    const size_t num_limbs = sizeof(el.mont_repr) / sizeof(mp_limb_t);
    while (!valid)
    {
        /* crypto generichash is keyed */
        const int status = crypto_generichash_blake2b((unsigned char*)&el.mont_repr,
                                                      sizeof(el.mont_repr),
                                                      root_plus_index,
                                                      root_plus_index_size,
                                                      (unsigned char*)&cur_key,
                                                      sizeof(cur_key));
        if (status != 0)
        {
            throw std::runtime_error("Got non-zero status from crypto_generichash_blake2b. (Is digest_len_bytes correct?)");
        }
        /* clear all bits higher than MSB of modulus */
        size_t bitno = sizeof(el.mont_repr) * 8 - 1;
        while (FieldT::mod.test_bit(bitno) == false)
        {
            const std::size_t part = bitno / bits_per_limb;
            const std::size_t bit = bitno - (bits_per_limb*part);

            el.mont_repr.data[part] &= ~(1ul<<bit);
            bitno--;
        }
        /* if el.data is < modulus its valid, otherwise repeat (rejection sampling) */
        if (mpn_cmp(el.mont_repr.data, FieldT::mod.data, num_limbs) < 0)
        {
            valid = true;
        }

        cur_key += key_increment;
    }
    return el;
}

template<typename FieldT>
std::vector<FieldT> blake2b_FieldT_randomness_extractor(const hash_digest &root,
                                                        const std::size_t index,
                                                        const std::size_t num_elements)
{
    const std::size_t root_plus_index_size = root.size() + sizeof(index);
    unsigned char* root_plus_index = (unsigned char*)(malloc(root_plus_index_size));
    memcpy(root_plus_index, &root[0], root.size());
    memcpy(root_plus_index + root.size(), &index, sizeof(index));

    std::vector<FieldT> result;
    result.reserve(num_elements);

    for (std::size_t i = 0; i < num_elements; ++i)
    {
        FieldT el = blake2b_FieldT_rejection_sample<FieldT>(
            FieldT::zero(),
            root_plus_index, root_plus_index_size,
            i, num_elements);

        result.emplace_back(el);
    }

    free(root_plus_index);

    return result;
}

}
