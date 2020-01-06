#pragma once

#include "functions/quorem_preinv.cu"
#include "functions/multi_modexp.cu"
#include "modnum/modnum_monty_cios.cu"

namespace cuFIXNUM {

template< typename fixnum >
class chinese {
public:
    __device__ chinese(fixnum p, fixnum q);

    __device__ void operator()(fixnum &m, fixnum mp, fixnum mq) const;

private:
    // TODO: These all have width = WIDTH/2, so this is a waste of
    // space, and (worse) the operations below waste cycles.
    fixnum p, q, c;  // c = p^-1 (mod q)

    quorem_preinv<fixnum> mod_q;
};

template< typename fixnum >
__device__
chinese<fixnum>::chinese(fixnum p_, fixnum q_)
    : p(p_), q(q_), mod_q(q)
{
    typedef modnum_monty_cios<fixnum> modnum;

    // TODO: q is now stored here and in mod_q; need to work out how
    // to share q between them.  Probably best just to provide quorem_preinv
    // with an accessor to the divisor.

    // TODO: Make modinv use xgcd and use modinv instead.
    // Use a^(q-2) = 1 (mod q)
    fixnum qm2, two = fixnum::two();
    fixnum::sub(qm2, q, two);
    multi_modexp<modnum> minv(q);
    minv(c, p, qm2);
}


/*
 * CRT on Mp and Mq.
 *
 * Mp, Mq, p, q must all be WIDTH/2 digits long
 *
 * Source HAC, Note 14.75.
 */
template< typename fixnum >
__device__ void
chinese<fixnum>::operator()(fixnum &m, fixnum mp, fixnum mq) const
{
    typedef typename fixnum::digit digit;
    // u = (mq - mp) * c (mod q)
    fixnum u, t, hi, lo;
    digit br;
    fixnum::sub_br(u, br, mq, mp);

    // TODO: It would be MUCH better to ensure that the mul_wide
    // and mod_q parts of this condition occur on the main
    // execution path to avoid long warp divergence.
    if (br) {
        // Mp > Mq
        // TODO: Can't I get this from u above?  Need a negation
        // function; maybe use "method of complements".
        fixnum::sub_br(u, br, mp, mq);
        assert(digit::is_zero(br));

        // TODO: Replace mul_wide with the equivalent mul_lo
        //digit_mul(hi, lo, u, c, width/2);
        fixnum::mul_wide(hi, lo, u, c);
        assert(digit::is_zero(hi));

        t = fixnum::zero();
        //quorem_rem(mod_q, t, hi, lo, width/2);
        mod_q(t, hi, lo);

        // TODO: This is a mess.
        if ( ! fixnum::is_zero(t)) {
            fixnum::sub_br(u, br, q, t);
            assert(digit::is_zero(br));
        } else {
            u = t;
        }
    } else {
        // Mp < Mq
        // TODO: Replace mul_wide with the equivalent mul_lo
        //digit_mul(hi, lo, u, c, width/2);
        fixnum::mul_wide(hi, lo, u, c);
        assert(digit::is_zero(hi));

        u = fixnum::zero();
        //quorem_rem(mod_q, u, hi, lo, width/2);
        mod_q(u, hi, lo);
    }
    // TODO: Replace mul_wide with the equivalent mul_lo
    //digit_mul(hi, lo, u, p, width/2);
    fixnum::mul_wide(hi, lo, u, p);
    //shfl_up(hi, width/2, width);
    //t = (L < width/2) ? lo : hi;
    assert(digit::is_zero(hi));
    t = lo;

    //digit_add(m, mp, t, width);
    fixnum::add(m, mp, t);
}

} // End namespace cuFIXNUM
