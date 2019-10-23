#include <cstdio>

#define __STDC_FORMAT_MACROS
#include <inttypes.h>

#include <sodium/randombytes.h>

#include "libiop/algebra/fields/gf64.hpp"

#ifdef USE_ASM
#include <emmintrin.h>
#include <smmintrin.h>
#include <immintrin.h>
#endif

namespace libiop {

using std::size_t;

const uint64_t gf64::modulus_;
gf64 gf64::multiplicative_generator = gf64(2);

gf64::gf64() : value_(0)
{
}

gf64::gf64(const uint64_t value) : value_(value)
{
}

std::vector<uint64_t> gf64::as_words() const
{
    return std::vector<uint64_t>({this->value_});
}

gf64& gf64::operator+=(const gf64 &other)
{
    this->value_ ^= other.value_;
    return (*this);
}

gf64& gf64::operator-=(const gf64 &other)
{
    this->value_ ^= other.value_;
    return (*this);
}

// multiplication over GF(2^k) is carryless multiplication
gf64& gf64::operator*=(const gf64 &other)
{
    /* Does not require *this and other to be different, and therefore
       also works for squaring, implemented below. */
#ifdef USE_ASM
    const __m128i modulus = _mm_loadl_epi64((const __m128i*)&(this->modulus_));
    const __m128i mul128 = _mm_clmulepi64_si128(_mm_loadl_epi64((const __m128i*)&(this->value_)),
                                                _mm_loadl_epi64((const __m128i*)&(other.value_)), 0);

    /* reduce the 64 higher order bits of mul128. Output is 96 bits since modulus < 2^64 */
    const __m128i mul96 = _mm_clmulepi64_si128(modulus, mul128, 0x10); /* use high half of mul128 */
    __m128i rem = _mm_xor_si128(mul128, mul96);

    /* reduce the 32 higher order bits of mul96 */
    const __m128i mul64 = _mm_clmulepi64_si128(modulus, mul96, 0x10); /* use high half of mul96 */

    rem = _mm_xor_si128(rem, mul64);
    this->value_ = (uint64_t)_mm_movepi64_pi64(rem);

    return (*this);
#else
    /* Slow, but straight-forward */
    uint64_t result = 0;
    uint64_t shifted = this->value_;

    for (uint64_t i = 0; i < 64; ++i)
    {
        if (other.value_ & (1ull << i))
        {
            result ^= shifted;
        }
        if (shifted & (1ul << 63))
        {
            shifted <<= 1;
            shifted ^= this->modulus_;
        }
        else
        {
            shifted <<= 1;
        }
    }

    this->value_ = result;

    return (*this);
#endif
}

void gf64::square()
{
    this->operator*=(*this);
}

gf64 gf64::operator+(const gf64 &other) const
{
    gf64 result(*this);
    return (result+=(other));
}

gf64 gf64::operator-(const gf64 &other) const
{
    gf64 result(*this);
    return (result-=(other));
}

gf64 gf64::operator-() const
{
    /* additive inverse matches the element itself */
    return gf64(*this);
}

gf64 gf64::operator*(const gf64 &other) const
{
    gf64 result(*this);
    return (result*=(other));
}

gf64 gf64::squared() const
{
    gf64 result(*this);
    result.square();
    return result;
}

// repeatedly square pt, num_times. For use in inverse.
void square_multi(gf64* pt, int8_t num_times)
{
    for (size_t i = 0; i < num_times; i++)
    {
        (*pt).square();
    }
}

/* calculate el^{-1} as el^{2^{64}-2}. the addition chain below
   requires 74 mul/sqr operations total. It was found using the
   Bergeron-Berstel-Brlek-Duboc method implemented in
   https://github.com/kwantam/addchain. */
gf64 gf64::inverse() const
{
    // comments on the right side are of the form operation_number : exponent at the set variable
    gf64 t0 = *this;        //    1 : 1
    gf64 t1 = t0 * t0;      //    2 : 2
    gf64 t2 = t1 * t0;      //    3 : 3
    t0 = t2 * t2;      //    4 : 6
    t0.square();       //    5 : 12
    t1 *= t0;          //    6 : 14
    t2 *= t0;          //    7 : 15
    t0 = t2 * t2;      //    8 : 30
    t0.square();       //    9 : 60
    t0.square();       //   10 : 120
    t0.square();       //   11 : 240
    t1 *= t0;          //   12 : 254
    t2 *= t0;          //   13 : 255
    t0 = t2 * t2;      //   14 : 510
    t0.square();       //   15 : 1020
    t0.square();       //   16 : 2040
    t0.square();       //   17 : 4080
    t0.square();       //   18 : 8160
    t0.square();       //   19 : 16320
    t0.square();       //   20 : 32640
    t0.square();       //   21 : 65280
    t1 *= t0;          //   22 : 65534
    t2 *= t0;          //   23 : 65535
    t0 = t2 * t2;      //   24 : 131070
    t0.square();       //   25 : 262140
    t0.square();       //   26 : 524280
    t0.square();       //   27 : 1048560
    t0.square();       //   28 : 2097120
    t0.square();       //   29 : 4194240
    t0.square();       //   30 : 8388480
    t0.square();       //   31 : 16776960
    t0.square();       //   32 : 33553920
    t0.square();       //   33 : 67107840
    t0.square();       //   34 : 134215680
    t0.square();       //   35 : 268431360
    t0.square();       //   36 : 536862720
    t0.square();       //   37 : 1073725440
    t0.square();       //   38 : 2147450880
    t0.square();       //   39 : 4294901760
    t1 *= t0;          //   40 : 4294967294
    t0 *= t2;          //   41 : 4294967295
    for (int i = 0; i < 32; i++) {
        t0.square();   // 42-73: 8589934590 - 18446744069414584320
    }
    t0 *= t1;          //   74 : 18446744073709551614
    return t0;
}

void gf64::randomize()
{
    randombytes_buf(&this->value_, 64/8);
}

bool gf64::operator==(const gf64 &other) const
{
    return (this->value_ == other.value_);
}

bool gf64::operator!=(const gf64 &other) const
{
    return !(this->operator==(other));
}

void gf64::print() const
{
    printf("%016" PRIx64 "\n", this->value_);
}

bool gf64::is_zero() const
{
    return (this->value_ == 0);
}

gf64 gf64::zero()
{
    return gf64(0);
}

gf64 gf64::one()
{
    return gf64(1);
}

gf64 gf64::random_element()
{
    gf64 result;
    result.randomize();
    return result;
}

} // namespace libiop
