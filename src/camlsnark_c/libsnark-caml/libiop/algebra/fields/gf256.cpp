#include <cstdio>

#define __STDC_FORMAT_MACROS
#include <inttypes.h>

#include <sodium/randombytes.h>

#include "libiop/algebra/fields/gf256.hpp"

#ifdef USE_ASM
#include <emmintrin.h>
#include <smmintrin.h>
#include <immintrin.h>
#endif

namespace libiop {

using std::size_t;

const uint64_t gf256::modulus_;
gf256 gf256::multiplicative_generator = gf256(2);

gf256::gf256() : value_{0, 0, 0, 0}
{
}

gf256::gf256(const uint64_t value_low) : value_{value_low, 0, 0, 0}
{
}

gf256::gf256(const uint64_t value_high, const uint64_t value_midh,
             const uint64_t value_midl, const uint64_t value_low) :
    value_{value_low, value_midl, value_midh, value_high}
{
}

std::vector<uint64_t> gf256::as_words() const
{
    return std::vector<uint64_t>({this->value_[0], this->value_[1], this->value_[2], this->value_[3]});
}

gf256& gf256::operator+=(const gf256 &other)
{
    this->value_[0] ^= other.value_[0];
    this->value_[1] ^= other.value_[1];
    this->value_[2] ^= other.value_[2];
    this->value_[3] ^= other.value_[3];
    return (*this);
}

gf256& gf256::operator-=(const gf256 &other)
{
    this->value_[0] ^= other.value_[0];
    this->value_[1] ^= other.value_[1];
    this->value_[2] ^= other.value_[2];
    this->value_[3] ^= other.value_[3];
    return (*this);
}

gf256& gf256::operator*=(const gf256 &other)
{
    /* Does not require *this and other to be different, and therefore
       also works for squaring, implemented below. */
#ifdef USE_ASM
    /* depending on the manufacturer and generation of a CPU, the PCLMUL
       instruction might take different amounts of time.
       empirically, it appears that on recent Intel CPUs, PCLMUL is so fast that
       a naive multiplicator that uses 16 PCLMULs is faster than anything more
       complicated (because time spent doing non-PCLMUL operations dominates).
       on AMD CPUs, however, more complicated multiplicators (e.g. Karatsuba,
       which uses a total of 9 multiplications) can be faster.

       thus we use a preprocessor flag to choose between a naive and a Karatsuba
       multiplicator. */
#ifdef ASM_MINIMIZE_CLMULS
    /* here we implement a Karatsuba-like approach for multiplying 4-limb numbers.

       given
         a = a0 + B * a1 + B^2 * a2 + B^3 * a3
         b = b0 + B * b1 + B^2 * b2 + B^3 * b3
       we can compute
         c = a * b = c0 + ... + B^6 * c6
       (where ai and bi are < B, but ci are < B^2)
       with 9 multiplications as follows:
         1. c0 = a0 * b0
         2. c6 = a3 * b3
         3. t  = a1 * b1
         4. u  = a2 * b2
         5. c1 = (a0 + a1) * (b0 + b1) - c0 - t
         6. c2 = (a0 + a2) * (b0 + b2) - c0 + t - u
         7. c5 = (a2 + a3) * (b2 + b3) - c6 - u
         8. c4 = (a1 + a3) * (b1 + b3) - c6 + u - t
         9. c3 = (a0 + a1 + a2 + a3) * (b0 + b1 + b2 + b3)
                 - c0 - c1 - c2 - c4 - c5 - c6 */

    /* load the two operands and the modulus into 128-bit registers.
       we load corresponding limbs of both operands into a single register,
       because it lets us implement Karatsuba with fewer 128-bit xors. */
    const __m128i ab0 = _mm_set_epi64x(this->value_[0], other.value_[0]);
    const __m128i ab1 = _mm_set_epi64x(this->value_[1], other.value_[1]);
    const __m128i ab2 = _mm_set_epi64x(this->value_[2], other.value_[2]);
    const __m128i ab3 = _mm_set_epi64x(this->value_[3], other.value_[3]);
    const __m128i modulus = _mm_loadl_epi64((const __m128i*) &(this->modulus_));
    __m128i c0 = _mm_clmulepi64_si128(ab0, ab0, 0x01); /* multiply low and high halves */
    __m128i c6 = _mm_clmulepi64_si128(ab3, ab3, 0x01);

    __m128i t = _mm_clmulepi64_si128(ab1, ab1, 0x01);
    __m128i u = _mm_clmulepi64_si128(ab2, ab2, 0x01);

    __m128i xor01 = _mm_xor_si128(ab0, ab1);
    __m128i c1 = _mm_clmulepi64_si128(xor01, xor01, 0x01);
    __m128i xor_c0_t = _mm_xor_si128(c0, t);
    c1 = _mm_xor_si128(c1, xor_c0_t);

    __m128i xor02 = _mm_xor_si128(ab0, ab2);
    __m128i c2 = _mm_clmulepi64_si128(xor02, xor02, 0x01);
    c2 = _mm_xor_si128(_mm_xor_si128(c2, xor_c0_t), u);

    __m128i xor23 = _mm_xor_si128(ab2, ab3);
    __m128i c5 = _mm_clmulepi64_si128(xor23, xor23, 0x01);
    __m128i xor_c6_u = _mm_xor_si128(c6, u);
    c5 = _mm_xor_si128(c5, xor_c6_u);

    __m128i xor13 = _mm_xor_si128(ab1, ab3);
    __m128i c4 = _mm_clmulepi64_si128(xor13, xor13, 0x01);
    c4 = _mm_xor_si128(_mm_xor_si128(c4, xor_c6_u), t);

    __m128i xor0123 = _mm_xor_si128(xor02, xor13);
    __m128i c3 = _mm_clmulepi64_si128(xor0123, xor0123, 0x01);
    c3 = _mm_xor_si128(_mm_xor_si128(_mm_xor_si128(
         _mm_xor_si128(_mm_xor_si128(_mm_xor_si128(
         c3, c0), c1), c2), c4), c5), c6);

#else // ASM_MINIMIZE_CLMULS
    /* here we compute the same c as in Karatsuba, but by just naively
       multiplying all pairs of limbs of the operands and adding together
       the results that correspond to the same shift. */
    const __m128i a_low = _mm_loadu_si128((const __m128i*) &(this->value_[0]));
    const __m128i a_high = _mm_loadu_si128((const __m128i*) &(this->value_[2]));
    const __m128i b_low = _mm_loadu_si128((const __m128i*) &(other.value_[0]));
    const __m128i b_high = _mm_loadu_si128((const __m128i*) &(other.value_[2]));
    const __m128i modulus = _mm_loadl_epi64((const __m128i*) &(this->modulus_));

    __m128i m00 = _mm_clmulepi64_si128(a_low, b_low, 0x00);
    __m128i m01 = _mm_clmulepi64_si128(a_low, b_low, 0x10);
    __m128i m10 = _mm_clmulepi64_si128(a_low, b_low, 0x01);
    __m128i m11 = _mm_clmulepi64_si128(a_low, b_low, 0x11);
    __m128i m20 = _mm_clmulepi64_si128(a_high, b_low, 0x00);
    __m128i m21 = _mm_clmulepi64_si128(a_high, b_low, 0x10);
    __m128i m30 = _mm_clmulepi64_si128(a_high, b_low, 0x01);
    __m128i m31 = _mm_clmulepi64_si128(a_high, b_low, 0x11);
    __m128i m02 = _mm_clmulepi64_si128(a_low, b_high, 0x00);
    __m128i m03 = _mm_clmulepi64_si128(a_low, b_high, 0x10);
    __m128i m12 = _mm_clmulepi64_si128(a_low, b_high, 0x01);
    __m128i m13 = _mm_clmulepi64_si128(a_low, b_high, 0x11);
    __m128i m22 = _mm_clmulepi64_si128(a_high, b_high, 0x00);
    __m128i m23 = _mm_clmulepi64_si128(a_high, b_high, 0x10);
    __m128i m32 = _mm_clmulepi64_si128(a_high, b_high, 0x01);
    __m128i m33 = _mm_clmulepi64_si128(a_high, b_high, 0x11);

    __m128i c0 = m00;
    __m128i c1 = _mm_xor_si128(m01, m10);
    __m128i c2 = _mm_xor_si128(_mm_xor_si128(m02, m11), m20);
    __m128i c3 = _mm_xor_si128(_mm_xor_si128(_mm_xor_si128(m03, m12), m21), m30);
    __m128i c4 = _mm_xor_si128(_mm_xor_si128(m13, m22), m31);
    __m128i c5 = _mm_xor_si128(m23, m32);
    __m128i c6 = m33;

#endif // ASM_MINIMIZE_CLMULS

    /* this part is common to both multiplication algorithms:
       given the 6 overlapping 128-bit limbs such that
       a * b = c0 + (c1 << 64) + (c2 << 128) + (c3 << 192) + ... (c6 << 384)
       merge them into non-overlapping 128-bit limbs
       a * b = d0 + (d1 << 128) + (d2 << 256) + (d3 << 384) */
    __m128i d0 = _mm_xor_si128(c0, _mm_slli_si128(c1, 8));
    __m128i d1 = _mm_xor_si128(_mm_xor_si128(c2, _mm_srli_si128(c1, 8)), _mm_slli_si128(c3, 8));
    __m128i d2 = _mm_xor_si128(_mm_xor_si128(c4, _mm_srli_si128(c3, 8)), _mm_slli_si128(c5, 8));
    __m128i d3 = _mm_xor_si128(c6, _mm_srli_si128(c5, 8));

    /* done with the multiplication, time to reduce */

    /* reduce w.r.t. high half of d3 */
    __m128i tmp = _mm_clmulepi64_si128(d3, modulus, 0x01);
    d2 = _mm_xor_si128(d2, _mm_srli_si128(tmp, 8));
    d1 = _mm_xor_si128(d1, _mm_slli_si128(tmp, 8));

    /* reduce w.r.t. low half of d3 */
    tmp = _mm_clmulepi64_si128(d3, modulus, 0x00);
    d1 = _mm_xor_si128(d1, tmp);

    /* reduce w.r.t. high half of d2 */
    tmp = _mm_clmulepi64_si128(d2, modulus, 0x01);
    d1 = _mm_xor_si128(d1, _mm_srli_si128(tmp, 8));
    d0 = _mm_xor_si128(d0, _mm_slli_si128(tmp, 8));

    /* reduce w.r.t. low half of d2 */
    tmp = _mm_clmulepi64_si128(d2, modulus, 0x00);
    d0 = _mm_xor_si128(d0, tmp);

    /* done, now just store everything back into this->value_ */
    _mm_storeu_si128((__m128i*) &this->value_[0], d0);
    _mm_storeu_si128((__m128i*) &this->value_[2], d1);

    return (*this);
#else
    /* Slow, but straight-forward */
    uint64_t shifted[4] = {this->value_[0], this->value_[1],
                           this->value_[2], this->value_[3]};
    uint64_t result[4] = {0, 0, 0, 0};

    for (size_t i = 0; i < 4; ++i)
    {
        for (size_t j = 0; j < 64; ++j)
        {
            if (other.value_[i] & (1ull << j))
            {
                result[0] ^= shifted[0];
                result[1] ^= shifted[1];
                result[2] ^= shifted[2];
                result[3] ^= shifted[3];
            }

            bool reduce = (shifted[3] & (1ull << 63));

            shifted[3] = (shifted[3] << 1) | (shifted[2] >> 63);
            shifted[2] = (shifted[2] << 1) | (shifted[1] >> 63);
            shifted[1] = (shifted[1] << 1) | (shifted[0] >> 63);
            shifted[0] = shifted[0] << 1;

            if (reduce)
            {
                shifted[0] ^= this->modulus_;
            }
        }

    }

    this->value_[0] = result[0];
    this->value_[1] = result[1];
    this->value_[2] = result[2];
    this->value_[3] = result[3];
#endif

    return (*this);
}

void gf256::square()
{
    this->operator*=(*this);
}

gf256 gf256::operator+(const gf256 &other) const
{
    gf256 result(*this);
    return (result+=(other));
}

gf256 gf256::operator-(const gf256 &other) const
{
    gf256 result(*this);
    return (result-=(other));
}

gf256 gf256::operator-() const
{
    return gf256(*this);
}

gf256 gf256::operator*(const gf256 &other) const
{
    gf256 result(*this);
    return (result*=(other));
}

gf256 gf256::squared() const
{
    gf256 result(*this);
    result.square();
    return result;
}

/* calculate el^{-1} as el^{2^{256}-2}. the addition chain below
   requires 270 mul/sqr operations total. */
gf256 gf256::inverse() const
{
    gf256 a(*this);

    gf256 result(0);
    for (size_t i = 0; i <= 7; ++i)
    {
        /* entering the loop a = el^{2^{2^i}-1} */
        gf256 b = a;
        for (size_t j = 0; j < (1ul<<i); ++j)
        {
            b.square();
        }
        /* after the loop b = a^{2^i} = el^{2^{2^i}*(2^{2^i}-1)} */
        a *= b;
        /* now a = el^{2^{2^{i+1}}-1} */

        if (i == 0)
        {
            result = b;
        }
        else
        {
            result *= b;
        }
    }
    /* now result = el^{2^256-2} */
    return result;
}

void gf256::randomize()
{
    randombytes_buf(&this->value_, 256/8);
}

bool gf256::operator==(const gf256 &other) const
{
    return ((this->value_[0] == other.value_[0]) &&
            (this->value_[1] == other.value_[1]) &&
            (this->value_[2] == other.value_[2]) &&
            (this->value_[3] == other.value_[3]));
}

bool gf256::operator!=(const gf256 &other) const
{
    return !(this->operator==(other));
}

bool gf256::is_zero() const
{
    return (this->value_[0] == 0) && (this->value_[1] == 0) &&
           (this->value_[2] == 0) && (this->value_[3] == 0);
}

void gf256::print() const
{
    printf("%016" PRIx64 "%016" PRIx64 "%016" PRIx64 "%016" PRIx64 "\n",
           this->value_[3], this->value_[2],
           this->value_[1], this->value_[0]);
}

gf256 gf256::random_element()
{
    gf256 result;
    result.randomize();
    return result;
}

gf256 gf256::zero()
{
    return gf256(0);
}

gf256 gf256::one()
{
    return gf256(1);
}

} // namespace libiop
