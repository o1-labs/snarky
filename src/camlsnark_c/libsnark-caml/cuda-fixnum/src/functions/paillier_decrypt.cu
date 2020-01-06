#pragma once

#include "functions/quorem_preinv.cu"
#include "functions/divexact.cu"
#include "functions/chinese.cu"
#include "functions/multi_modexp.cu"
#include "modnum/modnum_monty_cios.cu"

namespace cuFIXNUM {

template< typename fixnum >
class paillier_decrypt_mod;

template< typename fixnum >
class paillier_decrypt {
public:
    __device__ paillier_decrypt(fixnum p, fixnum q)
        : n(prod(p, q))
        , crt(p, q)
        , decrypt_modp(p, n)
        , decrypt_modq(q, n) {  }

    __device__ void operator()(fixnum &ptxt, fixnum ctxt_hi, fixnum ctxt_lo) const;

private:
    // We only need this in the constructor to initialise decrypt_mod[pq], but we
    // store it here because it's the only way to save the computation and pass
    // it to the constructors of decrypt_mod[pq].
    fixnum n;

    // Secret key is (p, q).
    paillier_decrypt_mod<fixnum> decrypt_modp, decrypt_modq;

    // TODO: crt and decrypt_modq both compute and hold quorem_preinv(q); find a
    // way to share them.
    chinese<fixnum> crt;

    // TODO: It is flipping stupid that this is necessary.
    __device__ fixnum prod(fixnum p, fixnum q) {
        fixnum n;
        // TODO: These don't work when SLOT_WIDTH = 0
        //assert(fixnum::slot_layout::laneIdx() < fixnum::SLOT_WIDTH/2 || p == 0);
        //assert(fixnum::slot_layout::laneIdx() < fixnum::SLOT_WIDTH/2 || q == 0);
        fixnum::mul_lo(n, p, q);
        return n;
    }
};

/**
 * Decrypt the ciphertext c = (c_hi, c_lo) and put the resulting plaintext in m.
 *
 * m, c_hi and c_lo must be PLAINTEXT_DIGITS long.
 */
template< typename fixnum >
__device__ void
paillier_decrypt<fixnum>::operator()(fixnum &ptxt, fixnum ctxt_hi, fixnum ctxt_lo) const
{
    fixnum mp, mq;
    decrypt_modp(mp, ctxt_hi, ctxt_lo);
    decrypt_modq(mq, ctxt_hi, ctxt_lo);
    crt(ptxt, mp, mq);
}


template< typename fixnum >
class paillier_decrypt_mod {
public:
    __device__ paillier_decrypt_mod(fixnum p, fixnum n);

    __device__ void operator()(fixnum &mp, fixnum c_hi, fixnum c_lo) const;

private:
    // FIXME: These all have width = WIDTH/2, so this is a waste of
    // space, and (worse) the operations below waste cycles.

    // Precomputation of
    //   L((1 + n)^(p - 1) mod p^2)^-1 (mod p)
    // for CRT, where n = pq is the public key, and L(x) = (x-1)/p.
    fixnum h;

    // We only need this in the constructor to initialise mod_p2 and pow, but we
    // store it here because it's the only way to save the computation and pass
    // it to the constructors of mod_p2 and pow.
    fixnum p_sqr;

    // Exact division by p
    divexact<fixnum> div_p;
    // Remainder after division by p.
    quorem_preinv<fixnum> mod_p;
    // Remainder after division by p^2.
    quorem_preinv<fixnum> mod_p2;

    // Modexp for x |--> x^(p - 1) (mod p^2)
    typedef modnum_monty_cios<fixnum> modnum;
    modexp<modnum> pow;

    // TODO: It is flipping stupid that these are necessary.
    __device__ fixnum square(fixnum p) {
        fixnum p2;
        // TODO: This doesn't work when SLOT_WIDTH = 0
        //assert(fixnum::slot_layout::laneIdx() < fixnum::SLOT_WIDTH/2 || p == 0);
        fixnum::sqr_lo(p2, p);
        return p2;
    }
    __device__ fixnum sub1(fixnum p) {
        fixnum pm1;
        fixnum::sub(pm1, p, fixnum::one());
        return pm1;
    }
};


template< typename fixnum >
__device__
paillier_decrypt_mod<fixnum>::paillier_decrypt_mod(fixnum p, fixnum n)
    : p_sqr(square(p))
    , div_p(p)
    , mod_p(p)
    , mod_p2(p_sqr)
    , pow(p_sqr, sub1(p))
{
    typedef typename fixnum::digit digit;
    digit cy;
    fixnum t = n;
    cy = fixnum::incr_cy(t);
    // n is the product of primes, and 2^(2^k) - 1 has (at least) k factors,
    // hence n is less than 2^FIXNUM_BITS - 1, hence incrementing n shouldn't
    // overflow.
    assert(digit::is_zero(cy));
    // TODO: Check whether reducing t is necessary.
    mod_p2(t, fixnum::zero(), t);
    pow(t, t);
    fixnum::decr_br(t);
    div_p(t, t);

    // TODO: Make modinv use xgcd and use modinv instead.
    // Use a^(p-2) = 1 (mod p)
    fixnum pm2;
    fixnum::sub(pm2, p, fixnum::two());
    multi_modexp<modnum> minv(p);
    minv(h, t, pm2);
}

/*
 * Decrypt ciphertext (c_hi, c_lo) and put the result in mp.
 *
 * Decryption mod p of c is put in the (bottom half of) mp.
 */
template< typename fixnum >
__device__ void
paillier_decrypt_mod<fixnum>::operator()(fixnum &mp, fixnum c_hi, fixnum c_lo) const
{
    fixnum c, u, hi, lo;
    // mp = c_hi * 2^n + c_lo (mod p^2)  which is nonzero because p != q
    mod_p2(c, c_hi, c_lo);

    pow(u, c);
    fixnum::decr_br(u);
    div_p(u, u);
    // Check that the high half of u is now zero.
    // TODO: This doesn't work when SLOT_WIDTH = 0
    //assert(fixnum::slot_layout::laneIdx() < fixnum::SLOT_WIDTH/2 || u == 0);

    // TODO: make use of the fact that u and h are half-width.
    fixnum::mul_wide(hi, lo, u, h);
    assert(fixnum::is_zero(hi));
    mod_p(mp, hi, lo);
}

} // End namespace cuFIXNUM
