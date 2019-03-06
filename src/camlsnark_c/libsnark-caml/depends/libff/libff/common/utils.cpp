/** @file
 *****************************************************************************
 Implementation of misc math and serialization utility functions
 *****************************************************************************
 * @author     This file is part of libff, developed by SCIPR Lab
 *             and contributors (see AUTHORS).
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#include <algorithm>
#include <cassert>
#include <cstdarg>
#include <cstdint>

#include <libff/common/utils.hpp>

namespace libff {

size_t get_power_of_two(size_t n)
{
    n--;
    n |= n >> 1;
    n |= n >> 2;
    n |= n >> 4;
    n |= n >> 8;
    n |= n >> 16;
    n++;

    return n;
}

size_t log2(size_t n)
/* returns ceil(log2(n)), so 1ul<<log2(n) is the smallest power of 2,
   that is not less than n. */
{
    size_t r = ((n & (n-1)) == 0 ? 0 : 1); // add 1 if n is not power of 2

    while (n > 1)
    {
        n >>= 1;
        r++;
    }

    return r;
}

size_t to_twos_complement(int i, size_t w)
{
    assert(i >= -(1l<<(w-1)));
    assert(i < (1l<<(w-1)));
    return (i >= 0) ? i : i + (1l<<w);
}

int from_twos_complement(size_t i, size_t w)
{
    assert(i < (1ul<<w));
    return (i < (1ul<<(w-1))) ? i : i - (1ul<<w);
}

size_t bitreverse(size_t n, const size_t l)
{
    size_t r = 0;
    for (size_t k = 0; k < l; ++k)
    {
        r = (r << 1) | (n & 1);
        n >>= 1;
    }
    return r;
}

bit_vector int_list_to_bits(const std::initializer_list<unsigned long> &l, const size_t wordsize)
{
    bit_vector res(wordsize*l.size());
    for (size_t i = 0; i < l.size(); ++i)
    {
        for (size_t j = 0; j < wordsize; ++j)
        {
            res[i*wordsize + j] = (*(l.begin()+i) & (1ul<<(wordsize-1-j)));
        }
    }
    return res;
}

long long div_ceil(long long x, long long y)
{
    return (x + (y-1)) / y;
}

bool is_little_endian()
{
    uint64_t a = 0x12345678;
    unsigned char *c = (unsigned char*)(&a);
    return (*c = 0x78);
}

std::string FORMAT(const std::string &prefix, const char* format, ...)
{
    const static size_t MAX_FMT = 256;
    char buf[MAX_FMT];
    va_list args;
    va_start(args, format);
    vsnprintf(buf, MAX_FMT, format, args);
    va_end(args);

    return prefix + std::string(buf);
}

void serialize_bit_vector(std::ostream &out, const bit_vector &v)
{
    out << v.size() << "\n";
    for (size_t i = 0; i < v.size(); ++i)
    {
        out << v[i] << "\n";
    }
}

void deserialize_bit_vector(std::istream &in, bit_vector &v)
{
    size_t size;
    in >> size;
    v.resize(size);
    for (size_t i = 0; i < size; ++i)
    {
        bool b;
        in >> b;
        v[i] = b;
    }
}

size_t k_adicity(size_t k, size_t n)
{
    size_t r = 0;
    while (n > 1)
    {
        if (n % k == 0) {
            r += 1;
            n /= k;
        } else {
            return r;
        }
    }
    return r;
}

size_t pow_int(size_t base, size_t exp)
{
    size_t res = 1;
    while (exp)
    {
        if (exp & 1)
        {
            res *= base;
        }
        exp /= 2;
        base *= base;
    }
    return res;
}

size_t mixed_radix_FFT_permute(size_t two_adicity, size_t q_adicity, size_t q, size_t N, size_t i)
{
    /*
    This is the permutation obtained by splitting into
    2 groups two_adicity times and then
    q groups q_adicity many times
    It can be efficiently described as follows
    write 
    i = 2^0 b_0 + 2^1 b_1 + ... + 2^{two_adicity - 1} b_{two_adicity - 1} 
      + 2^two_adicity ( x_0 + q^1 x_1 + .. + q^{q_adicity-1} x_{q_adicity-1})
    We want to return
    j = b_0 (N/2) + b_1 (N/ 2^2) + ... + b_{two_adicity-1} (N/ 2^two_adicity)
      + x_0 (N / 2^two_adicity / q) + .. + x_{q_adicity-1} (N / 2^two_adicity / q^q_adicity) */
    size_t res = 0;
    size_t shift = N;

    for (size_t s = 0; s < two_adicity; ++s)
    {
      shift = shift / 2;
      res += (i % 2) * shift;
      i = i / 2;
    }

    for (size_t s = 0; s < q_adicity; ++s)
    {
        shift = shift / q;
        res += (i % q) * shift;
        i = i / q;
    }

    return res;
}

} // libff
