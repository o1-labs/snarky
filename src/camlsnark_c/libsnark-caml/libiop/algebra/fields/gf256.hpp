/**@file
 *****************************************************************************
 Declaration of GF(2^256) finite field.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_ALGEBRA_GF256_HPP_
#define LIBIOP_ALGEBRA_GF256_HPP_

#include <cstddef>
#include <cstdint>
#include <vector>

namespace libiop {

/* x^256 + x^10 + x^5 + x^2 + 1 */
/* gf256 implements the field GF(2)/(x^256 + x^10 + x^5 + x^2 + 1).
   Elements are represented internally with four uint64s */
class gf256 {
public:
    // x^256 + x^10 + x^5 + x^2 + 1
    static const constexpr uint64_t modulus_ = 0b10000100101;
    static const constexpr uint64_t num_bits = 256;

    explicit gf256();
    /* we need a constructor that only initializes the low 64 bits of value_ to
       be able to do gf256(0) and gf256(1). */
    explicit gf256(const uint64_t value_low);
    explicit gf256(const uint64_t value_high, const uint64_t value_midh,
                   const uint64_t value_midl, const uint64_t value_low);
    /** Returns the constituent bits in 64 bit words, in little-endian order */
    std::vector<uint64_t> as_words() const;

    gf256& operator+=(const gf256 &other);
    gf256& operator-=(const gf256 &other);
    gf256& operator*=(const gf256 &other);
    void square();

    gf256 operator+(const gf256 &other) const;
    gf256 operator-(const gf256 &other) const;
    gf256 operator-() const;
    gf256 operator*(const gf256 &other) const;
    gf256 squared() const;

    gf256 inverse() const;

    void randomize();

    bool operator==(const gf256 &other) const;
    bool operator!=(const gf256 &other) const;

    bool is_zero() const;

    void print() const;

    static gf256 random_element();

    static gf256 zero();
    static gf256 one();
    static gf256 multiplicative_generator; // generator of gf256^*

    static std::size_t extension_degree() { return 256; }
private:
    /* little-endian */
    uint64_t value_[4];
};

} // namespace libiop

#endif // LIBIOP_ALGEBRA_GF256_HPP_
