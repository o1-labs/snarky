#pragma once

#include "functions/quorem_preinv.cu"
#include "functions/multi_modexp.cu"
#include "modnum/modnum_monty_cios.cu"

namespace cuFIXNUM {

template< typename fixnum >
class paillier_encrypt {
public:
    __device__ paillier_encrypt(fixnum n_)
        : n(n_), n_sqr(square(n_)), pow(n_sqr, n_), mod_n2(n_sqr) { }

    /*
     * NB: In reality, the values r^n should be calculated out-of-band or
     * stock-piled and piped into an encryption function.
     */
    __device__ void operator()(fixnum &ctxt, fixnum m, fixnum r) const {
        // TODO: test this properly
        //assert(fixnum::slot_layout::laneIdx() < fixnum::SLOT_WIDTH/2 || m == 0);
        fixnum::mul_lo(m, m, n);
        fixnum::incr_cy(m);
        pow(r, r);
        fixnum c_hi, c_lo;
        fixnum::mul_wide(c_hi, c_lo, m, r);
        mod_n2(ctxt, c_hi, c_lo);
    }

private:
    typedef modnum_monty_cios<fixnum> modnum;

    fixnum n;
    fixnum n_sqr;
    modexp<modnum> pow;
    quorem_preinv<fixnum> mod_n2;

    // TODO: It is flipping stupid that this is necessary.
    __device__ fixnum square(fixnum n) {
        fixnum n2;
        // TODO: test this properly
        //assert(fixnum::slot_layout::laneIdx() < fixnum::SLOT_WIDTH/2 || n == 0);
        fixnum::sqr_lo(n2, n);
        return n2;
    }
};

} // End namespace cuFIXNUM
