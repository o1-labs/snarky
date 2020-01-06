#pragma once

#include "arith.cu"

// All algorithms from
// https://www.hyperelliptic.org/EFD/g1p/auto-shortw-jacobian.html#addition-add-2007-bl
template< typename FF, int CRV_A, typename Grp >
struct ec_jac {
    typedef FF field_type;

    // NB: This is corresponds to the group of rational points for
    // curves over prime field; it is a lie for curves over extension
    // fields.
    typedef Grp group_type;

    // TODO: See if using Chudnovsky coordinates improves things much
    // (ie. store Z^2 and Z^3 as well)
    FF x, y, z;

    static constexpr int NELTS = 3 * FF::DEGREE; // *3 for x, y and z

    __device__
    static void
    load_affine(ec_jac &P, const var *mem) {
        FF::load(P.x, mem);
        FF::load(P.y, mem + FF::DEGREE * ELT_LIMBS);
        FF::set_one(P.z);

        // FIXME: This is an odd convention, but that's how they do it.
        if (FF::is_zero(P.y))
            set_zero(P);
    }

    __device__
    static void
    load_jac(ec_jac &P, const var *mem) {
        FF::load(P.x, mem);
        FF::load(P.y, mem + FF::DEGREE * ELT_LIMBS);
        FF::load(P.z, mem + 2 * FF::DEGREE * ELT_LIMBS);
    }

    __device__
    static int
    is_affine(const ec_jac &P) {
        FF one;
        FF::set_one(one);
        return FF::are_equal(P.z, one);
    }

    __device__
    static int
    are_equal(const ec_jac &P, const ec_jac &Q) {
        FF zPzP, zPzPzP, zQzQ, zQzQzQ;

        FF::sqr(zPzP, P.z);
        FF::sqr(zQzQ, Q.z);

        FF t0, t1;
        FF::mul(t0, P.x, zQzQ);
        FF::mul(t1, Q.x, zPzP);

        if ( ! FF::are_equal(t0, t1))
            return 0;

        // x-coordinates are equal; now check the y-coordinates.

        FF::mul(zPzPzP, zPzP, P.z);
        FF::mul(zQzQzQ, zQzQ, Q.z);
        FF::mul(t0, P.y, zQzQzQ);
        FF::mul(t1, Q.y, zPzPzP);

        return FF::are_equal(t0, t1);
    }

#if 0
    __device__
    static void
    store_affine(var *mem, const ec_jac &P) {
        FF z_inv, z2_inv, z3_inv, aff_x, aff_y;

        // NB: Very expensive!
        // TODO: Consider (i) doing this on the host and (ii) implementing
        // simultaneous inversion.
        FF::inv(z inv, P.z);
        FF::sqr(z2_inv, z_inv);
        FF::mul(z3_inv, z2_inv, z_inv);

        FF::mul(aff_x, P.x, z2_inv);
        FF::store(mem, aff_x);

        FF::mul(aff_y, P.y, z3_inv);
        FF::store(mem + FF::DEGREE * ELT_LIMBS, aff_y);
    }
#endif

    __device__
    static void
    store_jac(var *mem, const ec_jac &P) {
        FF::store(mem, P.x);
        FF::store(mem + FF::DEGREE * ELT_LIMBS, P.y);
        FF::store(mem + 2 * FF::DEGREE * ELT_LIMBS, P.z);
    }

    __device__
    static void
    set_zero(ec_jac &P) {
        FF::set_one(P.x);
        FF::set_one(P.y);
        FF::set_zero(P.z);
    }

    __device__
    static int
    is_zero(const ec_jac &P) { return FF::is_zero(P.z); }

#if 0
    // TODO: Needs double-checking
    __device__
    static void
    mixed_dbl(ec_jac &R, const ec_jac &P) {
        FF xx, yy, yyyy, s, m, t, t0, t1;

        FF::sqr(xx, P.x);      // XX = X1^2
        FF::sqr(yy, P.y);      // YY = Y1^2
        FF::sqr(yyyy, yy);     // YYYY = YY^2
        FF::add(s, P.x, yy);   // t0 = X1+YY
        FF::sqr(s, s);         // t1 = t0^2
        FF::sub(s, s, xx);     // t2 = t1-XX
        FF::sub(s, s, yyyy);   // t3 = t2-YYYY
        mul_<2>::x(s);         // S = 2*t3
        mul_<3>::x(m, xx);     // t4 = 3*XX

        // FIXME: Won't work
        FF::add(m, m, CRV_A);  // M = t4+a

        FF::sqr(t, m);         // t5 = M^2
        mul_<2>::x(t0, s);     // t6 = 2*S
        FF::sub(t, t, t0);     // T = t5-t6
        R.x = t;               // X3 = T
        mul_<2>::x(R.z, P.y);  // Z3 = 2*Y1
        FF::sub(t0, s, t);     // t7 = S-T
        mul_<8>::x(t1, yyyy);  // t8 = 8*YYYY
        FF::mul(R.y, m, t0);   // t9 = M*t7
        FF::sub(R.y, R.y, t1); // Y3 = t9-t8
    }
#endif

    __device__
    static void
    mixed_add(ec_jac &R, const ec_jac &P, const ec_jac &Q) {
        // Would be better to know that Q != 0
        if (is_zero(Q)) {
            R = P;
            return;
        } else if (is_zero(P)) {
            R = Q;
            return;
        }
        assert(is_affine(Q));

        FF z1z1, u2, s2, h, hh, i, j, r, v;
        FF t0, t1;

        FF::sqr(z1z1, P.z);     // Z1Z1 = Z1^2
        FF::mul(u2, Q.x, z1z1); // U2 = X2*Z1Z1
        FF::mul(s2, Q.y, P.z);
        FF::mul(s2, s2, z1z1);  // S2 = Y2*Z1*Z1Z1
        if (FF::are_equal(u2, P.x) && FF::are_equal(s2, P.y)) {
            // P == Q
            //mixed_dbl(R, Q);
            dbl(R, Q);
            return;
        }
        FF::sub(h, u2, P.x);    // H = U2-X1
        FF::sqr(hh, h);         // HH = H^2
        mul_<4>::x(i, hh);      // I = 4*HH
        FF::mul(j, h, i);       // J = H*I
        FF::sub(r, s2, P.y);    // t1 = S2-Y1
        mul_<2>::x(r, r);       // r = 2*t1
        FF::mul(v, P.x, i);     // V = X1*I

        FF::sqr(t0, r);         // t2 = r^2
        mul_<2>::x(t1, v);      // t3 = 2*V
        FF::sub(t0, t0, j);     // t4 = t2-J
        FF::sub(R.x, t0, t1);   // X3 = t4-t3

        FF::sub(t0, v, R.x);    // t5 = V-X3
        FF::mul(t1, P.y, j);    // t6 = Y1*J
        mul_<2>::x(t1, t1);     // t7 = 2*t6
        FF::mul(t0, r, t0);     // t8 = r*t5
        FF::sub(R.y, t0, t1);   // Y3 = t8-t7

        FF::add(t0, P.z, h);    // t9 = Z1+H
        FF::sqr(t0, t0);        // t10 = t9^2
        FF::sub(t0, t0, z1z1);  // t11 = t10-Z1Z1
        FF::sub(R.z, t0, hh);   // Z3 = t11-HH
    }

    // NB: This is not valid if P = Q or if P == 0 or Q == 0
    __device__
    static void
    add_unsafe(ec_jac &R, const ec_jac &P, const ec_jac &Q) {
        FF z1z1, z2z2, u1, u2, s1, s2, h, i, j, r, v;
        FF t0, t1;

        FF::sqr(z1z1, P.z); // Z1Z1 = Z1^2
        FF::sqr(z2z2, Q.z); // Z2Z2 = Z2^2
        FF::mul(u1, P.x, z2z2); // U1 = X1*Z2Z2
        FF::mul(u2, Q.x, z1z1); // U2 = X2*Z1Z1
        FF::mul(s1, P.y, Q.z);
        FF::mul(s1, s1, z2z2); // S1 = Y1*Z2*Z2Z2
        FF::mul(s2, Q.y, P.z);
        FF::mul(s2, s2, z1z1); // S2 = Y2*Z1*Z1Z1
        FF::sub(h, u2, u1); // H = U2-U1
        mul_<2>::x(i, h);
        FF::sqr(i, i); // I = (2*H)^2
        FF::mul(j, h, i); // J = H*I
        FF::sub(r, s2, s1);
        mul_<2>::x(r, r); // r = 2*(S2-S1)
        FF::mul(v, u1, i); // V = U1*I

        // X3 = r^2-J-2*V
        FF::sqr(t0, r);
        FF::sub(t0, t0, j);
        mul_<2>::x(t1, v);
        FF::sub(R.x, t0, t1);

        // Y3 = r*(V-X3)-2*S1*J
        FF::sub(t0, v, R.x);
        FF::mul(t0, r, t0);
        FF::mul(t1, s1, j);
        mul_<2>::x(t1, t1);
        FF::sub(R.y, t0, t1);

        // Z3 = ((Z1+Z2)^2-Z1Z1-Z2Z2)*H
        FF::add(t0, P.z, Q.z);
        FF::sqr(t0, t0);
        FF::add(t1, z1z1, z2z2);
        FF::sub(t0, t0, t1);
        FF::mul(R.z, t0, h);
    }

    __device__
    static void
    add(ec_jac &R, const ec_jac &P, const ec_jac &Q) {
        // TODO: It should be the caller's responsibility to check if
        // the operands are zero
        // Need P != 0 and Q != 0 for computation below to work
        if (is_zero(P)) {
            R = Q;
            return;
        } else if (is_zero(Q)) {
            R = P;
            return;
        }

        // need to save P (or Q) just in case &R = &P and we need to
        // double P after the add.
        ec_jac PP = P;
        add_unsafe(R, P, Q);

        // If P = Q, then add returns all zeros.
        if (FF::is_zero(R.x) && FF::is_zero(R.y) && FF::is_zero(R.z)) {
            dbl(R, PP);
        }
    }

    __device__
    static void
    dbl(ec_jac &R, const ec_jac &P) {
        FF xx, yy, yyyy, zz, s, m, t;
        FF t0, t1;

#ifndef NDEBUG
        // TODO: It should be the caller's responsibility to check if
        // the operand is zero
        // Need P != 0 for computation below to work.
        if (is_zero(P)) {
            set_zero(R);
            return;
        }
#endif

        FF::sqr(xx, P.x); // XX = X1^2
        FF::sqr(yy, P.y); // YY = Y1^2
        FF::sqr(yyyy, yy); // YYYY = YY^2
        FF::sqr(zz, P.z); // ZZ = Z1^2
        FF::add(t0, P.x, yy);
        FF::sqr(t0, t0);
        FF::add(t1, xx, yyyy);
        FF::sub(t0, t0, t1);
        mul_<2>::x(s, t0); // S = 2*((X1+YY)^2-XX-YYYY)
        mul_<3>::x(t0, xx);
        FF::sqr(t1, zz);
        mul_<CRV_A>::x(t1, t1);
        FF::add(m, t0, t1); // M = 3*XX+a*ZZ^2
        FF::sqr(t0, m);
        mul_<2>::x(t1, s);
        FF::sub(t, t0, t1); // T = M^2-2*S

        // X3 = T
        R.x = t;

        // NB: Need to do Z3 before Y3 in case &R = &P, since we need
        // to use P.y here.
        // Z3 = (Y1+Z1)^2-YY-ZZ
        FF::add(t0, P.y, P.z);
        FF::sqr(t0, t0);
        FF::add(t1, yy, zz);
        FF::sub(R.z, t0, t1);

        // Y3 = M*(S-T)-8*YYYY
        FF::sub(t0, s, t);
        FF::mul(t0, m, t0);
        mul_<8>::x(t1, yyyy);
        FF::sub(R.y, t0, t1);
    }

    template< int EXP >
    __device__ __forceinline__
    static void
    mul_2exp(ec_jac &R, const ec_jac &P) {
        dbl(R, P);
        #pragma unroll
        for (int k = 1; k < EXP; ++k)
            dbl(R, R);
    }

    __device__
    static void
    neg(ec_jac &R, const ec_jac &P) {
        R.x = P.x;
        FF::neg(R.y, P.y);
        R.z = P.z;
    }

    __device__
    static void
    mul(ec_jac &R, const var &n, const ec_jac &P) {
        // TODO: This version makes an effort to prevent intrawarp
        // divergence at a performance cost. This is probably no
        // longer a worthwhile trade-off.

        // TODO: Work out how to use add instead of add_safe.

        static constexpr int WINDOW_SIZE = 5;

        // TODO: I think it is better to use the remainder window
        // first rather than last. When it's last we sometimes miss
        // opportunities to use precomputed values.

        // Window decomposition: digit::BITS = q * WINDOW_SIZE + r.
        static constexpr unsigned WINDOW_REM_BITS = digit::BITS % WINDOW_SIZE;
        static constexpr unsigned WINDOW_MAX = (1U << WINDOW_SIZE);

        static constexpr unsigned WINDOW_MASK = (1U << WINDOW_SIZE) - 1U;
        static constexpr unsigned WINDOW_REM_MASK = (1U << WINDOW_REM_BITS) - 1U;

        if (is_zero(P)) {
            R = P;
            return;
        }

        /* G[t] = [t]P, t >= 0 */
        // TODO: This should be precomputed for all P.
        ec_jac G[WINDOW_MAX];
        set_zero(G[0]);
        G[1] = P;
        dbl(G[2], P);
        for (int t = 3; t < WINDOW_MAX; ++t)
            add(G[t], G[t - 1], P);

        auto g = fixnum::layout();

        int digit_idx = fixnum::most_sig_dig(n);
        if (digit_idx < 0) {
            // n == 0
            R = G[0];
            return;
        }

        // First iteration
        var f = g.shfl(n, digit_idx);

        // "Remainder"
        int j = digit::BITS - WINDOW_REM_BITS;
        var win = (f >> j) & WINDOW_REM_MASK;
        R = G[win];
        j -= WINDOW_SIZE;

        for (; j >= 0; j -= WINDOW_SIZE) {
            mul_2exp<WINDOW_SIZE>(R, R);
            win = (f >> j) & WINDOW_MASK;
            add(R, R, G[win]);
        }

        --digit_idx;
        for ( ; digit_idx >= 0; --digit_idx) {
            var f = g.shfl(n, digit_idx);
            var win; // TODO: Morally this should be an int

            // "Remainder"
            int j = digit::BITS - WINDOW_REM_BITS;
            mul_2exp<WINDOW_REM_BITS>(R, R);
            win = (f >> j) & WINDOW_REM_MASK;
            add(R, R, G[win]);

            j -= WINDOW_SIZE;

            for (; j >= 0; j -= WINDOW_SIZE) {
                mul_2exp<WINDOW_SIZE>(R, R);
                win = (f >> j) & WINDOW_MASK;
                add(R, R, G[win]);
            }
        }
    }
};



typedef ec_jac< Fp_MNT4, 2, Fp_MNT6 > ECp_MNT4;
typedef ec_jac< Fp2_MNT4, 2*13, Fp_MNT6 > ECp2_MNT4;

typedef ec_jac< Fp_MNT6, 11, Fp_MNT4 > ECp_MNT6;
typedef ec_jac< Fp3_MNT6, -1, Fp_MNT4 > ECp3_MNT6;
