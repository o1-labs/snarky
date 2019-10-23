#include <cstdio>

#define __STDC_FORMAT_MACROS
#include <inttypes.h>

#include <sodium/randombytes.h>

#include "libiop/algebra/fields/gf32.hpp"

#ifdef USE_ASM
#include <emmintrin.h>
#include <smmintrin.h>
#include <immintrin.h>
#endif

namespace libiop {

using std::size_t;

const uint32_t gf32::modulus_;
gf32 gf32::multiplicative_generator = gf32(2);

gf32::gf32() : value_(0)
{
}

gf32::gf32(const uint32_t value) : value_(value)
{
}

std::vector<uint64_t> gf32::as_words() const
{
    return std::vector<uint64_t>({uint64_t(this->value_)});
}

gf32& gf32::operator+=(const gf32 &other)
{
    this->value_ ^= other.value_;
    return (*this);
}

gf32& gf32::operator-=(const gf32 &other)
{
    this->value_ ^= other.value_;
    return (*this);
}

// multiplication over GF(2^k) is carryless multiplication
gf32& gf32::operator*=(const gf32 &other)
{
    /* Does not require *this and other to be different, and therefore
       also works for squaring, implemented below. */

    /* Slow, but straight-forward */
    uint32_t result = 0;
    uint32_t shifted = this->value_;

    for (uint32_t i = 0; i < 32; ++i)
    {
        if (other.value_ & (1ull << i))
        {
            result ^= shifted;
        }
        if (shifted & (1ul << 31))
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
}

void gf32::square()
{
    this->operator*=(*this);
}

gf32 gf32::operator+(const gf32 &other) const
{
    gf32 result(*this);
    return (result+=(other));
}

gf32 gf32::operator-(const gf32 &other) const
{
    gf32 result(*this);
    return (result-=(other));
}

gf32 gf32::operator-() const
{
    /* additive inverse matches the element itself */
    return gf32(*this);
}

gf32 gf32::operator*(const gf32 &other) const
{
    gf32 result(*this);
    return (result*=(other));
}

gf32 gf32::squared() const
{
    gf32 result(*this);
    result.square();
    return result;
}

// repeatedly square pt, num_times. For use in inverse.
void square_multi(gf32* pt, int8_t num_times)
{
    for (size_t i = 0; i < num_times; i++)
    {
        (*pt).square();
    }
}

/* calculate el^{-1} as el^{2^{32}-2}. */
gf32 gf32::inverse() const
{
    gf32 a(*this);

    gf32 result(0);
    for (size_t i = 0; i <= 4; ++i)
    {
        /* entering the loop a = el^{2^{2^i}-1} */
        gf32 b = a;
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
    /* now result = el^{2^32-2} */
    return result;
}

void gf32::randomize()
{
    randombytes_buf(&this->value_, 32/8);
}

bool gf32::operator==(const gf32 &other) const
{
    return (this->value_ == other.value_);
}

bool gf32::operator!=(const gf32 &other) const
{
    return !(this->operator==(other));
}

void gf32::print() const
{
    printf("%u\n", this->value_);
}

bool gf32::is_zero() const
{
    return (this->value_ == 0);
}

gf32 gf32::zero()
{
    return gf32(0);
}

gf32 gf32::one()
{
    return gf32(1);
}

gf32 gf32::random_element()
{
    gf32 result;
    result.randomize();
    return result;
}

} // namespace libiop
