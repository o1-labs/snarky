#include <cstdio>

#define __STDC_FORMAT_MACROS
#include <inttypes.h>

#include <sodium/randombytes.h>

#include "libiop/algebra/fields/gf192.hpp"

#ifdef USE_ASM
#include <emmintrin.h>
#include <smmintrin.h>
#include <immintrin.h>
#endif

namespace libiop {

using std::size_t;

const uint64_t gf192::modulus_;
gf192 gf192::multiplicative_generator = gf192(2);

gf192::gf192() : value_{0, 0, 0}
{
}

gf192::gf192(const uint64_t value_low) : value_{value_low, 0, 0}
{
}

gf192::gf192(const uint64_t value_high, const uint64_t value_mid, const uint64_t value_low) :
    value_{value_low, value_mid, value_high}
{
}

std::vector<uint64_t> gf192::as_words() const
{
    return std::vector<uint64_t>({this->value_[0], this->value_[1], this->value_[2]});
}

gf192& gf192::operator+=(const gf192 &other)
{
    this->value_[0] ^= other.value_[0];
    this->value_[1] ^= other.value_[1];
    this->value_[2] ^= other.value_[2];
    return (*this);
}

gf192& gf192::operator-=(const gf192 &other)
{
    this->value_[0] ^= other.value_[0];
    this->value_[1] ^= other.value_[1];
    this->value_[2] ^= other.value_[2];
    return (*this);
}

gf192& gf192::operator*=(const gf192 &other)
{
    /* Does not require *this and other to be different, and therefore
       also works for squaring, implemented below. */
#ifdef USE_ASM
    /* load the two operands and the modulus into 128-bit registers.
       we load corresponding limbs of both operands into a single register,
       because it lets us implement Karatsuba (see below) with fewer 128-bit
       xors. */
    const __m128i ab0 = _mm_set_epi64x(this->value_[0], other.value_[0]);
    const __m128i ab1 = _mm_set_epi64x(this->value_[1], other.value_[1]);
    const __m128i ab2 = _mm_set_epi64x(this->value_[2], other.value_[2]);
    const __m128i modulus = _mm_loadl_epi64((const __m128i*) &(this->modulus_));

    /* here we implement a Karatsuba-like approach for multiplying 3-limb numbers.
    given
      a = a0 + B * a1 + B^2 * a2
      b = b0 + B * b1 + B^2 * b2
    we can compute
      c = c0 + ... + B^4 * c4
    (where ai and bi are < B, but ci are < B^2)
    with 6 multiplications as follows:
      1. c0 = a0 * b0
      2. c4 = a2 * b2
      3. t  = a1 * b1
      4. c1 = (a0 + a1) * (b0 + b1) - c0 - t
      5. c3 = (a1 + a2) * (b1 + b2) - c4 - t
      6. c2 = (a0 + a2) * (b0 + b2) - c0 - c4 + t */
    __m128i c0 = _mm_clmulepi64_si128(ab0, ab0, 0x01); /* multiply low and high halves */
    __m128i c4 = _mm_clmulepi64_si128(ab2, ab2, 0x01);

    __m128i t = _mm_clmulepi64_si128(ab1, ab1, 0x01);

    __m128i xor01 = _mm_xor_si128(ab0, ab1);
    __m128i c1 = _mm_clmulepi64_si128(xor01, xor01, 0x01);
    c1 = _mm_xor_si128(_mm_xor_si128(c1, c0), t);

    __m128i xor12 = _mm_xor_si128(ab1, ab2);
    __m128i c3 = _mm_clmulepi64_si128(xor12, xor12, 0x01);
    c3 = _mm_xor_si128(_mm_xor_si128(c3, c4), t);

    __m128i xor02 = _mm_xor_si128(ab0, ab2);
    __m128i c2 = _mm_clmulepi64_si128(xor02, xor02, 0x01);
    c2 = _mm_xor_si128(_mm_xor_si128(_mm_xor_si128(c2, c0), c4), t);

    /* now let's compute
         d = d0 + B^2 * d1 + B^4 d2
       where d = c and di < B^2 */
    __m128i d0 = _mm_xor_si128(c0, _mm_slli_si128(c1, 8));
    __m128i d2 = _mm_xor_si128(c4, _mm_srli_si128(c3, 8));
    __m128i d1 = _mm_xor_si128(_mm_xor_si128(c2, _mm_srli_si128(c1, 8)),
                                _mm_slli_si128(c3, 8));

    /* done with the multiplication, time to reduce */

    /* reduce w.r.t. high half of d2 */
    __m128i tmp = _mm_clmulepi64_si128(d2, modulus, 0x01);
    d1 = _mm_xor_si128(d1, tmp);

    /* reduce w.r.t. low half of d2 */
    tmp = _mm_clmulepi64_si128(d2, modulus, 0x00);
    d1 = _mm_xor_si128(d1, _mm_srli_si128(tmp, 8));
    d0 = _mm_xor_si128(d0, _mm_slli_si128(tmp, 8));

    /* reduce w.r.t. high half of d1 */
    tmp = _mm_clmulepi64_si128(d1, modulus, 0x01);
    d0 = _mm_xor_si128(d0, tmp);

    /* done, now just store everything back into this->value_ */
    _mm_storeu_si128((__m128i*) &this->value_[0], d0);
    _mm_storel_epi64((__m128i*) &this->value_[2], d1);

    return (*this);
#else
    /* Slow, but straight-forward */
    uint64_t shifted[3] = {this->value_[0], this->value_[1], this->value_[2]};
    uint64_t result[3] = {0, 0, 0};

    for (size_t i = 0; i < 3; ++i)
    {
        for (size_t j = 0; j < 64; ++j)
        {
            if (other.value_[i] & (1ull << j))
            {
                result[0] ^= shifted[0];
                result[1] ^= shifted[1];
                result[2] ^= shifted[2];
            }

            if (shifted[2] & (1ull << 63))
            {
                shifted[2] = (shifted[2] << 1) | (shifted[1] >> 63);
                shifted[1] = (shifted[1] << 1) | (shifted[0] >> 63);
                shifted[0] = (shifted[0] << 1) ^ this->modulus_;
            } else {
                shifted[2] = (shifted[2] << 1) | (shifted[1] >> 63);
                shifted[1] = (shifted[1] << 1) | (shifted[0] >> 63);
                shifted[0] = shifted[0] << 1;
            }
        }

    }

    this->value_[0] = result[0];
    this->value_[1] = result[1];
    this->value_[2] = result[2];

    return (*this);
#endif
}

void gf192::square()
{
    this->operator*=(*this);
}

gf192 gf192::operator+(const gf192 &other) const
{
    gf192 result(*this);
    return (result+=(other));
}

gf192 gf192::operator-(const gf192 &other) const
{
    gf192 result(*this);
    return (result-=(other));
}

gf192 gf192::operator-() const
{
    return gf192(*this);
}

gf192 gf192::operator*(const gf192 &other) const
{
    gf192 result(*this);
    return (result*=(other));
}

gf192 gf192::squared() const
{
    gf192 result(*this);
    result.square();
    return result;
}

/* calculate el^{-1} as el^{2^{192}-2}. the addition chain below
   requires 210 mul/sqr operations total. */
gf192 gf192::inverse() const
{
    gf192 a(*this);

    gf192 result(0);
    gf192 prev_result(0);
    for (size_t i = 0; i <= 6; ++i)
    {
        /* entering the loop a = el^{2^{2^i}-1} */
        gf192 b = a;
        for (size_t j = 0; j < (1ul<<i); ++j)
        {
            b.square();
        }
        /* after the loop b = a^{2^i} = el^{2^{2^i}*(2^{2^i}-1)} */
        a *= b;
        /* now a = el^{2^{2^{i+1}}-1} */

        prev_result = result;
        if (i == 0)
        {
            result = b;
        }
        else
        {
            result *= b;
        }
    }

    /* now result = el^{2^128-2}, prev_result = el^{2^64-2} */
    for (size_t i = 0; i < (1ul<<6); ++i) {
        result.square();
    }
    prev_result.square();
    /* now result = el^{2^192 - 2*2^64}, prev_result = el^{2*2^64 - 4},
       thus el^{2^192 - 2} = result * prev_result * el^{2} */
    return result * prev_result * this->squared();
}

void gf192::randomize()
{
    randombytes_buf(&this->value_, 192/8);
}

bool gf192::operator==(const gf192 &other) const
{
    return ((this->value_[0] == other.value_[0]) &&
            (this->value_[1] == other.value_[1]) &&
            (this->value_[2] == other.value_[2]));
}

bool gf192::operator!=(const gf192 &other) const
{
    return !(this->operator==(other));
}

bool gf192::is_zero() const
{
    return (this->value_[0] == 0) && (this->value_[1] == 0) && (this->value_[2] == 0);
}

void gf192::print() const
{
    printf("%016" PRIx64 "%016" PRIx64 "%016" PRIx64 "\n", this->value_[2], this->value_[1], this->value_[0]);
}

gf192 gf192::random_element()
{
    gf192 result;
    result.randomize();
    return result;
}

gf192 gf192::zero()
{
    return gf192(0);
}

gf192 gf192::one()
{
    return gf192(1);
}

} // namespace libiop
