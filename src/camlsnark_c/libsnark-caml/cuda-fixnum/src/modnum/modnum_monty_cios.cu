#pragma once

#include "functions/modinv.cu"
#include "modnum/internal/monty.cu"

namespace cuFIXNUM {

template< typename fixnum_ >
class modnum_monty_cios {
public:
    typedef fixnum_ fixnum;
    typedef fixnum modnum;

    __device__ modnum_monty_cios(fixnum modulus);

    __device__ modnum zero() const { return monty.zero(); }
    __device__ modnum one() const { return monty.one(); }
    __device__ void add(modnum &z, modnum x, modnum y) const { monty.add(z, x, y); }
    __device__ void sub(modnum &z, modnum x, modnum y) const { monty.sub(z, x, y); }
    __device__ void neg(modnum &z, modnum x, modnum y) const { monty.neg(z, x); }

    /**
     * z <- x * y
     */
    __device__ void mul(modnum &z, modnum x, modnum y) const;

    /**
     * z <- x^2
     */
    __device__ void sqr(modnum &z, modnum x) const {
        mul(z, x, x);
    }

    // TODO: Might be worth specialising multiplication for this case, since one of
    // the operands is known.
    __device__ void to_modnum(modnum &z, fixnum x) const {
        mul(z, x, monty.Rsqr_mod);
    }

    // TODO: Might be worth specialising multiplication for this case, since one of
    // the operands is known.
    __device__ void from_modnum(fixnum &z, modnum x) const {
        mul(z, x, fixnum::one());
    }

private:
    typedef typename fixnum::digit digit;
    // TODO: Check whether we can get rid of this declaration
    static constexpr int WIDTH = fixnum::SLOT_WIDTH;

    internal::monty<fixnum> monty;

    // inv_mod * mod = -1 % 2^digit::BITS.
    digit  inv_mod;
};


template< typename fixnum >
__device__
modnum_monty_cios<fixnum>::modnum_monty_cios(fixnum mod)
: monty(mod)
{
    if ( ! monty.is_valid)
        return;

    // TODO: Tidy this up.
    modinv<fixnum> minv;
    fixnum im;
    minv(im, mod, digit::BITS);
    digit::neg(inv_mod, im);
    // TODO: Ugh.
    typedef typename fixnum::layout layout;
    // TODO: Can we avoid this broadcast?
    inv_mod = layout::shfl(inv_mod, 0);
    assert(1 + inv_mod * layout::shfl(mod, 0) == 0);
}

/*
 * z = x * y (mod) in Monty form.
 *
 * Spliced multiplication/reduction implementation of Montgomery
 * modular multiplication.  Specifically it is the CIOS (coursely
 * integrated operand scanning) splice.
 */
template< typename fixnum >
__device__ void
modnum_monty_cios<fixnum>::mul(modnum &z, modnum x, modnum y) const
{
    typedef typename fixnum::layout layout;
    // FIXME: Fix this hack!
    z = zero();
    if (!monty.is_valid) { return; }

    int L = layout::laneIdx();
    digit tmp;
    digit::mul_lo(tmp, x, inv_mod);
    digit::mul_lo(tmp, tmp, fixnum::get(y, 0));
    digit cy = digit::zero();

    for (int i = 0; i < WIDTH; ++i) {
        digit u;
        digit xi = fixnum::get(x, i);
        digit z0 = fixnum::get(z, 0);
        digit tmpi = fixnum::get(tmp, i);

        digit::mad_lo(u, z0, inv_mod, tmpi);

        digit::mad_lo_cy(z, cy, monty.mod, u, z);
        digit::mad_lo_cy(z, cy, y, xi, z);

        assert(L || digit::is_zero(z));  // z[0] must be 0
        z = layout::shfl_down0(z, 1); // Shift right one word

        digit::add_cy(z, cy, z, cy);

        digit::mad_hi_cy(z, cy, monty.mod, u, z);
        digit::mad_hi_cy(z, cy, y, xi, z);
    }
    // Resolve carries
    digit msw = fixnum::top_digit(cy);
    cy = layout::shfl_up0(cy, 1); // left shift by 1
    fixnum::add_cy(z, cy, z, cy);
    digit::add(msw, msw, cy);
    assert(msw == !!msw); // msw = 0 or 1.

    monty.normalise(z, (int)msw);
}

} // End namespace cuFIXNUM
