/**@file
 *****************************************************************************
 Declaration of GF(2^32) finite field.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_ALGEBRA_GF32_HPP_
#define LIBIOP_ALGEBRA_GF32_HPP_

#include <cstddef>
#include <cstdint>
#include <vector>

namespace libiop {

/* gf32 implements the field GF(2)/[x^32 + x^22 + x^2 + x^1 + 1].
   Elements are represented internally with a single uint32 */
class gf32 {
public:
    // x^32 + x^22 + x^2 + x^1 + 1
    static const constexpr uint32_t modulus_ = 0b10000000000000000000111;
    static const constexpr uint64_t num_bits = 32;

    explicit gf32();
    explicit gf32(const uint32_t value);
    std::vector<uint64_t> as_words() const;

    gf32& operator+=(const gf32 &other);
    gf32& operator-=(const gf32 &other);
    gf32& operator*=(const gf32 &other);
    void square();

    gf32 operator+(const gf32 &other) const;
    gf32 operator-(const gf32 &other) const;
    gf32 operator-() const;
    gf32 operator*(const gf32 &other) const;
    gf32 squared() const;

    gf32 inverse() const;

    void randomize();

    bool operator==(const gf32 &other) const;
    bool operator!=(const gf32 &other) const;

    bool is_zero() const;

    void print() const;

    static gf32 random_element();

    static gf32 zero();
    static gf32 one();
    static gf32 multiplicative_generator; // generator of gf32^*

    static std::size_t extension_degree() { return 32; }
private:
    uint32_t value_;
};

} // namespace libiop

#endif // LIBIOP_ALGEBRA_GF32_HPP_
