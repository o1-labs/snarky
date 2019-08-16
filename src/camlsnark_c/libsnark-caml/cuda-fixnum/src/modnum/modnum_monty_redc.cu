#pragma once

#include "modnum/internal/monty.cu"

namespace cuFIXNUM {

template< typename fixnum_ >
class modnum_monty_redc {
public:
    typedef fixnum_ fixnum;
    typedef fixnum modnum;

    __device__ modnum_monty_redc(fixnum mod)
    : monty(mod) {
        if ( ! monty.is_valid) return;

        modinv<fixnum> minv;
        minv(inv_mod, mod, fixnum::BITS);
        fixnum::neg(inv_mod, inv_mod);
#ifndef NDEBUG
        fixnum tmp;
        fixnum::mul_lo(tmp, inv_mod, mod);
        fixnum::add(tmp, tmp, fixnum::one());
        assert(fixnum::is_zero(tmp));
#endif
    }

    __device__ modnum zero() const { return monty.zero(); }
    __device__ modnum one() const { return monty.one(); }
    __device__ void add(modnum &z, modnum x, modnum y) const { monty.add(z, x, y); }
    __device__ void sub(modnum &z, modnum x, modnum y) const { monty.sub(z, x, y); }
    __device__ void neg(modnum &z, modnum x, modnum y) const { monty.neg(z, x); }

    __device__ void sqr(modnum &z, modnum x) const {
        // FIXME: Fix this hack!
        z = zero();
        if (!monty.is_valid) return;

        modnum a_hi, a_lo;
        fixnum::sqr_wide(a_hi, a_lo, x);
        redc(z, a_hi, a_lo);
    }

    __device__ void mul(modnum &z, modnum x, modnum y) const {
        // FIXME: Fix this hack!
        z = zero();
        if (!monty.is_valid) return;

        modnum a_hi, a_lo;
        fixnum::mul_wide(a_hi, a_lo, x, y);
        redc(z, a_hi, a_lo);
    }

    // TODO: Might be worth specialising multiplication for this case, since one of
    // the operands is known.
    __device__ void to_modnum(modnum &z, fixnum x) const {
        mul(z, x, monty.Rsqr_mod);
    }

    __device__ void from_modnum(fixnum &z, modnum x) const {
        //mul(z, x, fixnum::one());
        redc(z, fixnum::zero(), x);
    }

private:
    internal::monty<fixnum> monty;
    // inv_mod * mod = -1 % 2^fixnum::BITS.
    fixnum inv_mod;

    __device__ void redc(fixnum &r, fixnum a_hi, fixnum a_lo) const;
};


template< typename fixnum >
__device__ void
modnum_monty_redc<fixnum>::redc(fixnum &r, fixnum a_hi, fixnum a_lo) const {
    typedef typename fixnum::digit digit;
    fixnum b, s_hi, s_lo;
    digit cy, c;

    // FIXME: Fix this hack!
    r = zero();
    if (!monty.is_valid) return;

    fixnum::mul_lo(b, a_lo, inv_mod);

    // This section is essentially s = floor(mad_wide(b, mod, a) / R)

    // TODO: Can we employ the trick to avoid a multiplication because we
    // know b = am' (mod R)?
    fixnum::mul_wide(s_hi, s_lo, b, monty.mod);
    // TODO: Only want the carry; find a cheaper way to determine that
    // without doing the full addition.
    fixnum::add_cy(s_lo, cy, s_lo, a_lo);

    // TODO: The fact that we need to turn cy into a fixnum before using it in
    // arithmetic should be handled more cleanly. Also, this code is already in
    // the private function digit_to_fixnum() in ''warp_fixnum.cu'.
    int L = fixnum::layout::laneIdx();
    cy = (L == 0) ? cy : digit::zero();

    // TODO: The assert below fails; work out why.
#if 0
    // NB: b = am' (mod R) => a + bm = a + amm' = 2a (mod R). So surely
    // all I need to propagate is the top bit of a_lo?
    fixnum top_bit, dummy;
    fixnum::lshift(dummy, top_bit, a_lo, 1);
    assert(digit::cmp(cy, top_bit) == 0);
#endif
    fixnum::add_cy(r, cy, s_hi, cy);
    fixnum::add_cy(r, c, r, a_hi);
    digit::add(cy, cy, c);
    assert(cy == !!cy); // cy = 0 or 1.

    monty.normalise(r, cy);
}

} // End namespace cuFIXNUM
