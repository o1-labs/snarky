#pragma once

#include <cooperative_groups.h>

#include "primitives.cu"

/*
 * var is the basic register type that we deal with. The
 * interpretation of (one or more) such registers is determined by the
 * struct used, e.g. digit, fixnum, etc.
 */
typedef std::uint64_t var;

static constexpr size_t ELT_LIMBS = 12;
static constexpr size_t ELT_BYTES = ELT_LIMBS * sizeof(var);

static constexpr size_t BIG_WIDTH = ELT_LIMBS + 4; // = 16


struct digit {
    static constexpr int BYTES = sizeof(var);
    static constexpr int BITS = BYTES * 8;

    __device__ __forceinline__
    static void
    add(var &s, var a, var b) {
        s = a + b;
    }

    __device__ __forceinline__
    static void
    add_cy(var &s, int &cy, var a, var b) {
        s = a + b;
        cy = s < a;
    }

    __device__ __forceinline__
    static void
    sub(var &d, var a, var b) {
        d = a - b;
    }

    __device__ __forceinline__
    static void
    sub_br(var &d, int &br, var a, var b) {
        d = a - b;
        br = d > a;
    }

    __device__ __forceinline__
    static var
    zero() { return 0ULL; }

    __device__ __forceinline__
    static int
    is_max(var a) { return a == ~0ULL; }

    __device__ __forceinline__
    static int
    is_min(var a) { return a == 0ULL; }

    __device__ __forceinline__
    static int
    is_zero(var a) { return a == zero(); }

    __device__ __forceinline__
    static void
    mul_lo(var &lo, var a, var b) {
        lo = a * b;
    }

    // lo = a * b + c (mod 2^64)
    __device__ __forceinline__
    static void
    mad_lo(var &lo, var a, var b, var c) {
        internal::mad_lo(lo, a, b, c);
    }

    // as above but increment cy by the mad carry
    __device__ __forceinline__
    static void
    mad_lo_cy(var &lo, int &cy, var a, var b, var c) {
        internal::mad_lo_cc(lo, a, b, c);
        internal::addc(cy, cy, 0);
    }

    __device__ __forceinline__
    static void
    mad_hi(var &hi, var a, var b, var c) {
        internal::mad_hi(hi, a, b, c);
    }

    // as above but increment cy by the mad carry
    __device__ __forceinline__
    static void
    mad_hi_cy(var &hi, int &cy, var a, var b, var c) {
        internal::mad_hi_cc(hi, a, b, c);
        internal::addc(cy, cy, 0);
    }
};


struct fixnum {
    // 16 because digit::BITS * 16 = 1024 > 768 = digit::bits * 12
    // Must be < 32 for effective_carries to work.
    static constexpr unsigned WIDTH = 16;

    // TODO: Previous versiona allowed 'auto' return type here instead
    // of this mess
    __device__
    static cooperative_groups::thread_block_tile<WIDTH>
    layout() {
        return cooperative_groups::tiled_partition<WIDTH>(
            cooperative_groups::this_thread_block());
    }

    __device__ __forceinline__
    static var
    zero() { return digit::zero(); }

    __device__ __forceinline__
    static var
    one() {
        auto t = layout().thread_rank();
        return (var)(t == 0);
    }

    __device__
    static void
    add_cy(var &r, int &cy_hi, const var &a, const var &b) {
        int cy;
        digit::add_cy(r, cy, a, b);
        // r propagates carries iff r = FIXNUM_MAX
        var r_cy = effective_carries(cy_hi, digit::is_max(r), cy);
        digit::add(r, r, r_cy);
    }

    __device__
    static void
    add(var &r, const var &a, const var &b) {
        int cy_hi;
        add_cy(r, cy_hi, a, b);
    }

    __device__
    static void
    sub_br(var &r, int &br_lo, const var &a, const var &b) {
        int br;
        digit::sub_br(r, br, a, b);
        // r propagates borrows iff r = FIXNUM_MIN
        var r_br = effective_carries(br_lo, digit::is_min(r), br);
        digit::sub(r, r, r_br);
    }

    __device__
    static void
    sub(var &r, const var &a, const var &b) {
        int br_lo;
        sub_br(r, br_lo, a, b);
    }

    __device__ static uint32_t nonzero_mask(var r) {
        return fixnum::layout().ballot( ! digit::is_zero(r));
    }

    __device__ static int is_zero(var r) {
        return nonzero_mask(r) == 0U;
    }

    __device__ static int most_sig_dig(var x) {
        enum { UINT32_BITS = 8 * sizeof(uint32_t) };

        uint32_t a = nonzero_mask(x);
        return UINT32_BITS - (internal::clz(a) + 1);
    }

    __device__ static int cmp(var x, var y) {
        var r;
        int br;
        sub_br(r, br, x, y);
        // r != 0 iff x != y. If x != y, then br != 0 => x < y.
        return nonzero_mask(r) ? (br ? -1 : 1) : 0;
    }

    __device__
    static var
    effective_carries(int &cy_hi, int propagate, int cy) {
        uint32_t allcarries, p, g;
        auto grp = fixnum::layout();

        g = grp.ballot(cy);                       // carry generate
        p = grp.ballot(propagate);                // carry propagate
        allcarries = (p | g) + g;                 // propagate all carries
        cy_hi = (allcarries >> grp.size()) & 1;   // detect hi overflow
        allcarries = (allcarries ^ p) | (g << 1); // get effective carries
        return (allcarries >> grp.thread_rank()) & 1;
    }
};
