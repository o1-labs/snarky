#pragma once

namespace cuFIXNUM {

/*
 * Calculate the modular inverse.
 * TODO: Only supports moduli of the form 2^k at the moment.
 */
template< typename fixnum >
struct modinv {
    /*
     * Return x = 1/b (mod 2^k).  Must have 0 < k <= BITS.
     *
     * Source: MCA Algorithm 1.10.
     *
     * TODO: Calculate this using the multiple inversion trick (MCA 2.5.1)
     */
    __device__ void operator()(fixnum &x, fixnum b, int k) const {
        typedef typename fixnum::digit digit;
        // b must be odd
        digit b0 = fixnum::get(b, 0);
        assert(k > 0 && k <= fixnum::BITS);

        digit binv;
        digit::modinv_2exp(binv, b0);
        x = fixnum::zero();
        fixnum::set(x, binv, 0);
        if (k <= digit::BITS) {
            digit::rem_2exp(x, x, k);
            return;
        }

        // Hensel lift x from (mod 2^WORD_BITS) to (mod 2^k)
        // FIXME: Double-check this condition on k!
        while (k >>= 1) {
            fixnum t;
            // TODO: Make multiplications faster by using the "middle
            // product" (see MCA 1.4.5 and 3.3.2).
            fixnum::mul_lo(t, b, x);
            fixnum::sub(t, fixnum::one(), t);
            fixnum::mul_lo(t, t, x);
            fixnum::add(x, x, t);
        }
    }
};

} // End namespace cuFIXNUM
