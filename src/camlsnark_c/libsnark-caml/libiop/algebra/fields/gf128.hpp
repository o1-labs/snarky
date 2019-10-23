/**@file
 *****************************************************************************
 Declaration of GF(2^128) finite field.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_ALGEBRA_GF128_HPP_
#define LIBIOP_ALGEBRA_GF128_HPP_

#include <cstddef>
#include <cstdint>
#include <vector>

namespace libiop {

/* gf128 implements the field GF(2)/(x^128 + x^7 + x^2 + x + 1).
   Elements are represented internally with two uint64s */
class gf128 {
public:
    // x^128 + x^7 + x^2 + x + 1
    static const constexpr uint64_t modulus_ = 0b10000111;
    static const constexpr uint64_t num_bits = 128;

    explicit gf128();
    /* we need a constructor that only initializes the low half of value_ to
       be able to do gf128(0) and gf128(1). */
    explicit gf128(const uint64_t value_low);
    explicit gf128(const uint64_t value_high, const uint64_t value_low);
    /** Returns the constituent bits in 64 bit words, in little-endian order */
    std::vector<uint64_t> as_words() const;

    gf128& operator+=(const gf128 &other);
    gf128& operator-=(const gf128 &other);
    gf128& operator*=(const gf128 &other);
    void square();

    gf128 operator+(const gf128 &other) const;
    gf128 operator-(const gf128 &other) const;
    gf128 operator-() const;
    gf128 operator*(const gf128 &other) const;
    gf128 squared() const;

    gf128 inverse() const;

    void randomize();

    bool operator==(const gf128 &other) const;
    bool operator!=(const gf128 &other) const;

    bool is_zero() const;

    void print() const;

    static gf128 random_element();

    static gf128 zero();
    static gf128 one();
    static gf128 multiplicative_generator; // generator of gf128^*

    static std::size_t extension_degree() { return 128; }
private:
    /* little-endian */
    uint64_t value_[2];
};

} // namespace libiop

#endif // LIBIOP_ALGEBRA_GF128_HPP_
