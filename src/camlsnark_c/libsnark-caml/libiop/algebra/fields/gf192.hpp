/**@file
 *****************************************************************************
 Declaration of GF(2^192) finite field.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_ALGEBRA_GF192_HPP_
#define LIBIOP_ALGEBRA_GF192_HPP_

#include <cstddef>
#include <cstdint>
#include <vector>

namespace libiop {

/* gf192 implements the field GF(2)/(x^192 + x^7 + x^2 + x + 1).
   Elements are represented internally with three uint64s */
class gf192 {
public:
    // x^192 + x^7 + x^2 + x + 1
    static const constexpr uint64_t modulus_ = 0b10000111;
    static const constexpr uint64_t num_bits = 192;

    explicit gf192();
    /* we need a constructor that only initializes the low half of value_ to
       be able to do gf192(0) and gf192(1). */
    explicit gf192(const uint64_t value_low);
    explicit gf192(const uint64_t value_high, const uint64_t value_mid, const uint64_t value_low);
    /** Returns the constituent bits in 64 bit words, in little-endian order */
    std::vector<uint64_t> as_words() const;

    gf192& operator+=(const gf192 &other);
    gf192& operator-=(const gf192 &other);
    gf192& operator*=(const gf192 &other);
    void square();

    gf192 operator+(const gf192 &other) const;
    gf192 operator-(const gf192 &other) const;
    gf192 operator-() const;
    gf192 operator*(const gf192 &other) const;
    gf192 squared() const;

    gf192 inverse() const;

    void randomize();

    bool operator==(const gf192 &other) const;
    bool operator!=(const gf192 &other) const;

    bool is_zero() const;

    void print() const;

    static gf192 random_element();

    static gf192 zero();
    static gf192 one();
    static gf192 multiplicative_generator; // generator of gf192^*

    static std::size_t extension_degree() { return 192; }
private:
    /* little-endian */
    uint64_t value_[3];
};

} // namespace libiop

#endif // LIBIOP_ALGEBRA_GF192_HPP_
