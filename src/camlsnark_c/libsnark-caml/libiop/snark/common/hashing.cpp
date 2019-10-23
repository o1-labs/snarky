#include "sodium/crypto_generichash_blake2b.h"
#include <stdexcept>

#include "libiop/common/common.hpp"
#include "libiop/snark/common/hashing.hpp"

namespace libiop {

hash_digest blake2b_zk_element_hash(const std::vector<uint8_t> &bytes,
                                        const std::size_t digest_len_bytes)
{
    hash_digest result(digest_len_bytes, 'X');

    /* see https://download.libsodium.org/doc/hashing/generic_hashing.html */
    const int status = crypto_generichash_blake2b((unsigned char*)&result[0],
                                                  digest_len_bytes,
                                                  (unsigned char*)&bytes[0],
                                                  bytes.size() * sizeof(uint8_t),
                                                  NULL, 0);
    if (status != 0)
    {
        throw std::runtime_error("Got non-zero status from crypto_generichash_blake2b. (Is digest_len_bytes correct?)");
    }

    return result;
}

hash_digest blake2b_two_to_one_hash(const hash_digest &first,
                                    const hash_digest &second,
                                    const std::size_t digest_len_bytes)
{
    const hash_digest first_plus_second = first + second;

    hash_digest result(digest_len_bytes, 'X');

    /* see https://download.libsodium.org/doc/hashing/generic_hashing.html */
    const int status = crypto_generichash_blake2b((unsigned char*)&result[0],
                                                  digest_len_bytes,
                                                  (unsigned char*)&first_plus_second[0],
                                                  first_plus_second.size(),
                                                  NULL, 0);
    if (status != 0)
    {
        throw std::runtime_error("Got non-zero status from crypto_generichash_blake2b. (Is digest_len_bytes correct?)");
    }

    return result;
}

std::size_t blake2b_integer_randomness_extractor(const hash_digest &root,
                                                 const std::size_t index,
                                                 const std::size_t upper_bound)
{
    /* TODO: only required to make % below not biased. we should
       implement rejection sampling when we add prime field support */
    if (!is_power_of_2(upper_bound))
    {
        throw std::invalid_argument("upper_bound must be a power of two.");
    }

    std::size_t result;
    const int status = crypto_generichash_blake2b((unsigned char*)&result,
                                                  sizeof(result),
                                                  (unsigned char*)&root[0],
                                                  root.size(),
                                                  (unsigned char*)&index, sizeof(index));

    if (status != 0)
    {
        throw std::runtime_error("Got non-zero status from crypto_generichash_blake2b. (Is digest_len_bytes correct?)");
    }

    return result % upper_bound;
}

}
