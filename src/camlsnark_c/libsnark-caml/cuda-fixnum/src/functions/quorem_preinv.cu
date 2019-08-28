#pragma once

#include "functions/quorem.cu"

namespace cuFIXNUM {

/*
 * Quotient and remainder via Barrett reduction.
 *
 * div: the divisor
 * mu: floor(2^(2*NBITS) / div) where NBITS = FIXNUM_BITS (note: mu has an
 * implicit hi bit).
 */
template< typename fixnum >
class quorem_preinv {
public:
    __device__ quorem_preinv(fixnum div);

    // Assume clz(A) <= clz(div)
    __device__ void operator()(fixnum &q, fixnum &r, fixnum A_hi, fixnum A_lo) const;

    // Just return the remainder.
    __device__ void operator()(fixnum &r, fixnum A_hi, fixnum A_lo) const {
        fixnum q;
        (*this)(q, r, A_hi, A_lo);
    }

    // TODO: This should be somewhere more appropriate.
    __device__ static void reciprocal_approx(fixnum &mu, fixnum div);

private:
    static constexpr int WIDTH = fixnum::SLOT_WIDTH;
    typedef typename fixnum::digit digit;

    // Note that mu has an implicit hi bit that is always on.
    fixnum div, mu;
    int lz;
};

// Assumes div has been normalised already.
// NB: Result has implicit hi bit on.
// TODO: This function should be generalised and made available more widely
template< typename fixnum >
__device__ void
quorem_preinv<fixnum>::reciprocal_approx(fixnum &mu, fixnum div)
{
    // Let B = 2^FIXNUM_BITS

    // Initial estimate is 2*B - div = B + (B - div)  (implicit hi bit)
    // TODO: Use better initial estimate: (48/17) - (32/17)*div (see
    // https://en.wikipedia.org/wiki/Division_algorithm#Newton-Raphson_division)
    fixnum::neg(mu, div);

    // If we initialise mu = 2*B - div, then the error is 1.0 - mu*div/B^2 < 1/4.
    // In general, the error after iteration k = 0, 1, ... less than 1/(4^(2^k)).
    // We need an error less than 1/B^2, hence k >= log2(log2(B)).
    static constexpr uint32_t BITS = fixnum::BITS;
    // FIXME: For some reason this code doesn't converge as fast as it should.
    const int NITERS = internal::ctz(BITS); // TODO: Make ctz, hence NITERS, a constexpr
    int L = fixnum::layout::laneIdx();

    // TODO: Instead of calculating/using floor(B^2/div), calculate/use the
    // equivalent  floor((B^2 - 1)/div) - B  as described in the Möller & Granlund
    // paper; this should allow simplification because there's no implicit hi bit
    // in mu to account for.
    for (int i = 0; i < NITERS; ++i) {
        digit cy, br;
        fixnum a, b, c, d, e;

        // (hi, lo) = B^2 - mu*div. This is always positive.
        fixnum::mul_wide(a, b, mu, div);
        fixnum::add_cy(a, cy, a, div);  // account for hi bit of mu
        // cy will be 1 when mu = floor(B^2/div), which happens on the last iteration
        assert(digit::is_zero(cy));
        fixnum::sub_br(b, br, fixnum::zero(), b); // br == 0 iff b == 0.
        br = (L == 0) ? br : digit::zero();
        fixnum::neg(a, a);
        fixnum::sub(a, a, br);

        // TODO: a + c is actually correct to within a single bit; investigate
        // whether using a mu that is off by one bit matters? If it does, we
        // should only do this correction on the last iteration.
        // TODO: Implement fused-multiply-add and use it here for "a*mu + b".
        fixnum::mul_wide(c, d, a, mu);
        fixnum::add_cy(d, cy, d, b);
        cy = (L == 0) ? cy : digit::zero();
        fixnum::add_cy(c, cy, c, cy);
        assert(digit::is_zero(cy));

        // cy is the single extra bit that propogates to (a + c)
        fixnum::mul_hi(e, mu, b);
        fixnum::add_cy(d, cy, d, e);
        cy = (L == 0) ? cy : digit::zero();

        // mu += a + c + cy_in
        fixnum::add_cy(a, cy, a, cy);  assert(digit::is_zero(cy));
        fixnum::add_cy(mu, cy, mu, c); assert(digit::is_zero(cy));
        fixnum::add_cy(mu, cy, mu, a); assert(digit::is_zero(cy));
    }
}


/*
 * Create a quorem_preinv object.
 *
 * Raise an error if div does not have a sufficiently high bit switched
 * on.
 */
template< typename fixnum >
__device__
quorem_preinv<fixnum>::quorem_preinv(fixnum div_)
    : div(div_)
{
    lz = quorem<fixnum>::normalise_divisor(div);
    reciprocal_approx(mu, div);
}

/*
 * Return the quotient and remainder of A after division by div.
 *
 * Uses Barret reduction.  See HAC, Algo 14.42, and MCA, Algo 2.5.
 */
template< typename fixnum >
__device__ void
quorem_preinv<fixnum>::operator()(
    fixnum &q, fixnum &r, fixnum A_hi, fixnum A_lo) const
{
    fixnum t;
    int L = fixnum::layout::laneIdx();

    // Normalise A
    // TODO: Rather than normalising A, we should incorporate the
    // normalisation factor into the algorithm at the appropriate
    // place.
    t = quorem<fixnum>::normalise_dividend(A_hi, A_lo, lz);
    assert(fixnum::is_zero(t));

    // q = "A_hi * mu / 2^NBITS"
    // TODO: the lower half of the product, t, is unused, so we might
    // be able to use a mul_hi() function that only calculates an
    // approximate answer (see Short Product discussion at MCA,
    // Section 3.3 (from Section 2.4.1, p59)).
    fixnum::mul_wide(q, t, A_hi, mu);
    // TODO: For some reason (void)cy; does stop the compiler complaining about
    // cy being assigned but not used. Find a better way to avoid the warning
    // than this preprocessor crap.
#ifndef NDEBUG
    digit cy;
    fixnum::add_cy(q, cy, q, A_hi); // mu has implicit hi bit
    assert(digit::is_zero(cy));
#else
    fixnum::add(q, q, A_hi); // mu has implicit hi bit
#endif

    quorem<fixnum>::quorem_with_candidate_quotient(q, r, A_hi, A_lo, div, q);

    // Denormalise r
    fixnum lo_bits;
    fixnum::rshift(r, lo_bits, r, lz);
    assert(fixnum::is_zero(lo_bits));
}

} // End namespace cuFIXNUM
