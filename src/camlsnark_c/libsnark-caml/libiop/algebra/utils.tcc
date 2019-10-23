#include <cassert>
#include <sodium/randombytes.h>

#include "libiop/common/common.hpp"

namespace libiop {

template<typename T>
std::vector<T> all_subset_sums(const std::vector<T> &basis, const T& shift)
{
    const size_t m = basis.size();
    std::vector<T> result;
    /* as we are I/O-bound here, reserve + emplace_back is ~2x faster
       then pre-initializing with zero and then overwriting (as per
       benchmark_vector_op.cpp) */
    result.reserve(1ull<<m);

    result.emplace_back(shift);

    for (size_t i = 0; i < m; ++i)
    {
        const size_t l = (1ull<<i);
        for (size_t j = 0; j < l; ++j)
        {
            result.emplace_back(result[j] + basis[i]);
        }
    }

    return result;
}

template<typename FieldT>
std::vector<FieldT> batch_inverse(const std::vector<FieldT> &vec, const bool has_zeroes)
{
    return batch_inverse_and_mul(vec, FieldT::one(), has_zeroes);
}

template<typename FieldT>
std::vector<FieldT> batch_inverse_and_mul_internal(const std::vector<FieldT> &vec, const FieldT &k)
{
    /** Montgomery batch inversion trick.
     *  This assumes that all elements of the input are non-zero.
     *  It also multiplies every element by k, which can be done with one multiplication.
     */
    std::vector<FieldT> R;
    R.reserve(vec.size());

    FieldT c = vec[0];
    R.emplace_back(c);

    // Set R[i] to be the product of all vec[j], where j <= 0 <= i
    for (size_t i = 1; i < vec.size(); ++i)
    {
        c *= vec[i];
        R.emplace_back(c);
    }

    FieldT c_inv = c.inverse() * k;

    for (size_t i = vec.size()-1; i > 0; --i)
    {
        R[i] = R[i-1] * c_inv;
        c_inv *= vec[i];
    }

    R[0] = c_inv;

    return R;
}

template<typename FieldT>
std::vector<FieldT> batch_inverse_and_mul(const std::vector<FieldT> &vec, const FieldT &k, const bool has_zeroes)
{
    /** Montgomery batch inversion trick.
     *  This wraps the internal batch inverse and mul to handle 0's.
     *  If we need more efficiency, we can make an entirely separate codepath for the case with zeroes,
     *  and get rid of the loop that searches for zeroes.
     *  We omit this optimization, as has_zeroes=false in the verifiers code path. */
    if (has_zeroes)
    {
        std::vector<FieldT> vec_copy(vec);
        std::vector<size_t> zero_locations;
        FieldT zero = FieldT::zero();
        for (std::size_t i = 0; i < vec.size(); i++)
        {
            if (vec_copy[i] == zero)
            {
                zero_locations.emplace_back(i);
                vec_copy[i] = FieldT::one();
            }
        }
        std::vector<FieldT> result = batch_inverse_and_mul_internal(vec_copy, k);
        for (std::size_t i = 0; i < zero_locations.size(); i++)
        {
            result[zero_locations[i]] = zero;
        }
        return result;
    }
    else
    {
        return batch_inverse_and_mul_internal(vec, k);
    }
}

template<typename FieldT>
void mut_batch_inverse(std::vector<FieldT> &vec)
{
    /** Montgomery batch inversion trick, which mutates vec.
     *  This assumes that all elements of the input are non-zero.
     *
     *  The primary advantage of mut_batch_inverse is that instead of
     *  heap allocating a vector of size vec.size() internally, an array
     *  can be stack allocated. This matters more as the size of vec grows.
     */

    /* Not using std::vector in order to make this stack allocated. */
    FieldT vec_copy[vec.size()];

    FieldT c = vec[0];
    vec_copy[0] = c;

    // Set R[i] to be the product of all vec[j], where j <= 0 <= i
    for (size_t i = 1; i < vec.size(); ++i)
    {
        vec_copy[i] = vec[i];
        c *= vec[i];
        vec[i] = c;
    }

    FieldT c_inv = c.inverse();

    for (size_t i = vec.size()-1; i > 0; --i)
    {
        vec[i] = vec[i-1] * c_inv;
        c_inv *= vec_copy[i];
    }

    vec[0] = c_inv;

    return;
}


template<typename T>
void bitreverse_vector(std::vector<T> &a)
{
    const size_t n = a.size();
    const size_t logn = log2(n);
    assert(n == 1ull<<logn);

    for (size_t k = 0; k < n; ++k)
    {
        const size_t rk = bitreverse(k, logn);
        if (k < rk)
        {
            std::swap(a[k], a[rk]);
        }
    }
}

template<typename T>
std::vector<T> random_vector(const std::size_t count)
{
    std::vector<T> result(count);

    randombytes_buf(result.data(), count * sizeof(T));

    return result;
}

template<typename FieldT>
std::vector<FieldT> random_FieldT_vector(const std::size_t count)
{
    std::vector<FieldT> result;
    result.reserve(count);
    for (std::size_t i = 0; i < count; i++) {
        result.emplace_back(FieldT::random_element());
    }

    return result;
}

size_t gcd(const size_t a, const size_t b)
{
    return b == 0 ? a : gcd(b, a % b);
}

} // namespace libiop
