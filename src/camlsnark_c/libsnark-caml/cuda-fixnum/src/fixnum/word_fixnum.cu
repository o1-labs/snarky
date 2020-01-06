#pragma once

#include "fixnum/internal/primitives.cu"

namespace cuFIXNUM {

template< typename T >
class word_fixnum {
public:
    typedef T digit;
    typedef word_fixnum fixnum;

    static constexpr int BYTES = sizeof(T);
    static constexpr int BITS = BYTES * 8;

private:
    digit x;

    // TODO: These should be private
public:
    __device__ __forceinline__
    operator digit () const { return x; }

    __device__ __forceinline__
    operator digit &() { return x; }

public:
    __device__ __forceinline__
    word_fixnum() { }

    __device__ __forceinline__
    word_fixnum(digit z) : x(z) { }

    __device__ __forceinline__
    static void
    set_if(fixnum &s, fixnum a, int cond) {
        s = a & -(digit)cond;
    }

    // TODO: Implement/use something like numeric_limits<T>::max() for this
    // and most_negative().
    // FIXME: These two functions assume that T is unsigned.
    __device__ __forceinline__
    static constexpr fixnum
    most_positive() { return ~(fixnum)0; }

    __device__ __forceinline__
    static constexpr fixnum
    most_negative() { return zero(); };

    __device__ __forceinline__
    static constexpr fixnum
    zero() { return (fixnum)0; }

    __device__ __forceinline__
    static constexpr fixnum
    one() { return (fixnum)1; }

    __device__ __forceinline__
    static constexpr fixnum
    two() { return (fixnum)2; }

    __device__ __forceinline__
    static void
    add(fixnum &s, fixnum a, fixnum b) {
        s = a + b;
    }

    // TODO: this function does not follow the convention of later '*_cy'
    // functions of accumulating the carry into cy.
    __device__ __forceinline__
    static void
    add_cy(fixnum &s, digit &cy, fixnum a, fixnum b) {
        s = a + b;
        cy = s < a;
    }

    __device__ __forceinline__
    static void
    add_cyio(fixnum &s, digit &cy, fixnum a, fixnum b) {
        s = a + cy;
        cy = s < a;
        s += b;
        cy |= s < b;
    }

    __device__ __forceinline__
    static void
    add_cc(fixnum &s, fixnum a, fixnum b) {
        internal::add_cc(s, a, b);
    }

    __device__ __forceinline__
    static void
    addc(fixnum &s, fixnum a, fixnum b) {
        internal::addc(s, a, b);
    }

    __device__ __forceinline__
    static void
    addc_cc(fixnum &s, fixnum a, fixnum b) {
        internal::addc_cc(s, a, b);
    }

    __device__ __forceinline__
    static void
    incr(fixnum &s) {
        ++s;
    }

    __device__ __forceinline__
    static void
    sub(fixnum &d, fixnum a, fixnum b) {
        d = a - b;
    }

    __device__ __forceinline__
    static void
    sub_br(fixnum &d, digit &br, fixnum a, fixnum b) {
        d = a - b;
        br = d > a;
    }

    __device__ __forceinline__
    static void
    neg(fixnum &ma, fixnum a) {
        ma = -a;
    }

    __device__ __forceinline__
    static void
    mul_lo(fixnum &lo, fixnum a, fixnum b) {
        lo = a * b;
    }

    // hi * 2^32 + lo = a * b
    __device__ __forceinline__
    static void
    mul_hi(fixnum &hi, fixnum a, fixnum b) {
        internal::mul_hi(hi, a, b);
    }

    // hi * 2^32 + lo = a * b
    __device__ __forceinline__
    static void
    mul_wide(fixnum &hi, fixnum &lo, fixnum a, fixnum b) {
        internal::mul_wide(hi, lo, a, b);
    }

    // (hi, lo) = a * b + c
    __device__ __forceinline__
    static void
    mad_wide(fixnum &hi, fixnum &lo, fixnum a, fixnum b, fixnum c) {
        internal::mad_wide(hi, lo, a, b, c);
    }

    // lo = a * b + c (mod 2^32)
    __device__ __forceinline__
    static void
    mad_lo(fixnum &lo, fixnum a, fixnum b, fixnum c) {
        internal::mad_lo(lo, a, b, c);
    }

    // as above but increment cy by the mad carry
    __device__ __forceinline__
    static void
    mad_lo_cy(fixnum &lo, fixnum &cy, fixnum a, fixnum b, fixnum c) {
        internal::mad_lo_cc(lo, a, b, c);
        internal::addc(cy, cy, 0);
    }

    __device__ __forceinline__
    static void
    mad_hi(fixnum &hi, fixnum a, fixnum b, fixnum c) {
        internal::mad_hi(hi, a, b, c);
    }

    // as above but increment cy by the mad carry
    __device__ __forceinline__
    static void
    mad_hi_cy(fixnum &hi, fixnum &cy, fixnum a, fixnum b, fixnum c) {
        internal::mad_hi_cc(hi, a, b, c);
        internal::addc(cy, cy, 0);
    }

    // TODO: There are weird and only included for mul_wide
    __device__ __forceinline__
    static void
    mad_lo_cc(fixnum &lo, fixnum a, fixnum b, fixnum c) {
        internal::mad_lo_cc(lo, a, b, c);
    }

    // Returns the reciprocal for d.
    __device__ __forceinline__
    static fixnum
    quorem(fixnum &q, fixnum &r, fixnum n, fixnum d) {
        return quorem_wide(q, r, zero(), n, d);
    }

    // Accepts a reciprocal for d.
    __device__ __forceinline__
    static void
    quorem(fixnum &q, fixnum &r, fixnum n, fixnum d, fixnum v) {
        quorem_wide(q, r, zero(), n, d, v);
    }

    // Returns the reciprocal for d.
    // NB: returns q = r = fixnum::MAX if n_hi > d.
    __device__ __forceinline__
    static fixnum
    quorem_wide(fixnum &q, fixnum &r, fixnum n_hi, fixnum n_lo, fixnum d) {
        return internal::quorem_wide(q, r, n_hi, n_lo, d);
    }

    // Accepts a reciprocal for d.
    // NB: returns q = r = fixnum::MAX if n_hi > d.
    __device__ __forceinline__
    static void
    quorem_wide(fixnum &q, fixnum &r, fixnum n_hi, fixnum n_lo, fixnum d, fixnum v) {
        internal::quorem_wide(q, r, n_hi, n_lo, d, v);
    }

    __device__ __forceinline__
    static void
    rem_2exp(fixnum &r, fixnum n, unsigned k) {
        unsigned kp = BITS - k;
        r = (n << kp) >> kp;
    }

    /*
     * Count Leading Zeroes in x.
     *
     * TODO: This is not an intrinsic quality of a digit, so probably shouldn't
     * be in the interface.
     */
    __device__ __forceinline__
    static int
    clz(fixnum x) {
        return internal::clz(x);
    }

    /*
     * Count Trailing Zeroes in x.
     *
     * TODO: This is not an intrinsic quality of a digit, so probably shouldn't
     * be in the interface.
     */
    __device__ __forceinline__
    static int
    ctz(fixnum x) {
        return internal::ctz(x);
    }

    __device__ __forceinline__
    static int
    cmp(fixnum a, fixnum b) {
        // TODO: There is probably a PTX instruction for this.
        int br = (a - b) > a;
        return br ? -br : (a != b);
    }

    __device__ __forceinline__
    static int
    is_max(fixnum a) { return a == most_positive(); }

    __device__ __forceinline__
    static int
    is_min(fixnum a) { return a == most_negative(); }

    __device__ __forceinline__
    static int
    is_zero(fixnum a) { return a == zero(); }

    __device__ __forceinline__
    static void
    min(fixnum &m, fixnum a, fixnum b) {
        internal::min(m, a, b);
    }

    __device__ __forceinline__
    static void
    max(fixnum &m, fixnum a, fixnum b) {
        internal::max(m, a, b);
    }

    __device__ __forceinline__
    static void
    lshift(fixnum &z, fixnum x, unsigned b) {
        z = x << b;
    }

    __device__ __forceinline__
    static void
    lshift(fixnum &z, fixnum &overflow, fixnum x, unsigned b) {
        internal::lshift(overflow, z, 0, x, b);
    }

    __device__ __forceinline__
    static void
    rshift(fixnum &z, fixnum x, unsigned b) {
        z = x >> b;
    }

    __device__ __forceinline__
    static void
    rshift(fixnum &z, fixnum &underflow, fixnum x, unsigned b) {
        internal::rshift(z, underflow, x, 0, b);
    }

    /*
     * Return 1/b (mod 2^BITS) where b is odd.
     *
     * Source: MCA, Section 2.5.
     */
    __device__ __forceinline__
    static void
    modinv_2exp(fixnum &x, fixnum b) {
        internal::modinv_2exp(x, b);
    }

    /*
     * Return 1 if x = 2^n for some n, 0 otherwise. (Caveat: Returns 1 for x = 0
     * which is not a binary power.)
     *
     * FIXME: This doesn't belong here.
     */
    template< typename uint_type >
    __device__ __forceinline__
    static int
    is_binary_power(uint_type x) {
        //static_assert(std::is_unsigned<uint_type>::value == true,
        //              "template type must be unsigned");
        return ! (x & (x - 1));
    }

    /*
     * y >= x such that y = 2^n for some n. NB: This really is "inclusive"
     * next, i.e. if x is a binary power we just return it.
     *
     * FIXME: This doesn't belong here.
     */
    __device__ __forceinline__
    static fixnum
    next_binary_power(fixnum x) {
        return is_binary_power(x)
            ? x
            : (fixnum)((digit)1 << (BITS - clz(x)));
    }
};

typedef word_fixnum<std::uint32_t> u32_fixnum;
typedef word_fixnum<std::uint64_t> u64_fixnum;

} // End namespace cuFIXNUM
