/**@file
 *****************************************************************************
 Declaration of GF(2^64) finite field.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_ALGEBRA_GF64_HPP_
#define LIBIOP_ALGEBRA_GF64_HPP_

#include <cstddef>
#include <cstdint>
#include <vector>

namespace libiop {

/* gf64 implements the field GF(2)/[x^64 + x^4 + x^3 + x + 1].
   Elements are represented internally with a single uint64 */
class gf64 {
public:
    // x^64 + x^4 + x^3 + x + 1. The assembly code assumes that no term other
    // than x^64 is greater than x^31, to enable faster multiplication.
    static const constexpr uint64_t modulus_ = 0b11011;
    static const constexpr uint64_t num_bits = 64;

    explicit gf64();
    explicit gf64(const uint64_t value);
    std::vector<uint64_t> as_words() const;

    gf64& operator+=(const gf64 &other);
    gf64& operator-=(const gf64 &other);
    gf64& operator*=(const gf64 &other);
    void square();

    gf64 operator+(const gf64 &other) const;
    gf64 operator-(const gf64 &other) const;
    gf64 operator-() const;
    gf64 operator*(const gf64 &other) const;
    gf64 squared() const;

    gf64 inverse() const;

    void randomize();

    bool operator==(const gf64 &other) const;
    bool operator!=(const gf64 &other) const;

    bool is_zero() const;

    void print() const;

    static gf64 random_element();

    static gf64 zero();
    static gf64 one();
    static gf64 multiplicative_generator; // generator of gf64^*

    static std::size_t extension_degree() { return 64; }
private:
    uint64_t value_;
};

} // namespace libiop

#endif // LIBIOP_ALGEBRA_GF64_HPP_
