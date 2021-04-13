#pragma once

#include "fixnum.cu"

__device__ __constant__
const var MOD_Q[BIG_WIDTH] = {
    0x5e9063de245e8001ULL, 0xe39d54522cdd119fULL,
    0x638810719ac425f0ULL, 0x685acce9767254a4ULL,
    0xb80f0da5cb537e38ULL, 0xb117e776f218059dULL,
    0x99d124d9a15af79dULL, 0x7fdb925e8a0ed8dULL,
    0x5eb7e8f96c97d873ULL, 0xb7f997505b8fafedULL,
    0x10229022eee2cdadULL, 0x1c4c62d92c411ULL

    , 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL // just to make an even 16
};

// -Q^{-1} (mod 2^64)
static constexpr var Q_NINV_MOD = 0xf2044cfbe45e7fffULL;

// 2^768 mod Q
__device__ __constant__
const var X_MOD_Q[BIG_WIDTH] = {
    0x98a8ecabd9dc6f42ULL, 0x91cd31c65a034686ULL,
    0x97c3e4a0cd14572eULL, 0x79589819c788b601ULL,
    0xed269c942108976fULL, 0x1e0f4d8acf031d68ULL,
    0x320c3bb713338559ULL, 0x598b4302d2f00a62ULL,
    0x4074c9cbfd8ca621ULL, 0xfa47edb3865e88cULL,
    0x95455fb31ff9a195ULL, 0x7b479ec8e242ULL

    , 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL // just to make an even 16
};


//template< const var *mod_, const var ninv_mod_, const var *binpow_mod_ >
//struct modulus_info {
struct MNT4_MOD {
    __device__ __forceinline__ static int lane() { return fixnum::layout().thread_rank(); }
    __device__ __forceinline__ static var mod() { return MOD_Q[lane()]; }
    static constexpr var ninv_mod = Q_NINV_MOD;
    __device__ __forceinline__ static var monty_one() { return X_MOD_Q[lane()]; }
};
//typedef modulus_info<MOD_Q, Q_NINV_MOD, X_MOD_Q> MNT4_MOD;

__device__ __constant__
const var MOD_R[BIG_WIDTH] = {
    0xd90776e240000001ULL, 0x4ea099170fa13a4fULL,
    0xd6c381bc3f005797ULL, 0xb9dff97634993aa4ULL,
    0x3eebca9429212636ULL, 0xb26c5c28c859a99bULL,
    0x99d124d9a15af79dULL, 0x7fdb925e8a0ed8dULL,
    0x5eb7e8f96c97d873ULL, 0xb7f997505b8fafedULL,
    0x10229022eee2cdadULL, 0x1c4c62d92c411ULL

    , 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL // just to make an even 16
};

// -R^{-1} (mod 2^64)
const var R_NINV_MOD = 0xc90776e23fffffffULL;

// 2^768 mod R
__device__ __constant__
const var X_MOD_R[BIG_WIDTH] = {
    0xb99680147fff6f42ULL, 0x4eb16817b589cea8ULL,
    0xa1ebd2d90c79e179ULL, 0xf725caec549c0daULL,
    0xab0c4ee6d3e6dad4ULL, 0x9fbca908de0ccb62ULL,
    0x320c3bb713338498ULL, 0x598b4302d2f00a62ULL,
    0x4074c9cbfd8ca621ULL, 0xfa47edb3865e88cULL,
    0x95455fb31ff9a195ULL, 0x7b479ec8e242ULL

    , 0x0ULL, 0x0ULL, 0x0ULL, 0x0ULL // just to make an even 16
};

struct MNT6_MOD {
    __device__ __forceinline__ static int lane() { return fixnum::layout().thread_rank(); }
    __device__ __forceinline__ static var mod() { return MOD_R[lane()]; }
    static constexpr var ninv_mod = R_NINV_MOD;
    __device__ __forceinline__ static var monty_one() { return X_MOD_R[lane()]; }
};

// Apparently we still can't do partial specialisation of function
// templates in C++, so we do it in a class instead. Woot.
template< int n >
struct mul_ {
    template< typename G >
    __device__ static void x(G &z, const G &x);
};

template<>
template< typename G >
__device__ void
mul_<2>::x(G &z, const G &x) {
    // TODO: Shift by one bit
    G::add(z, x, x);
}

template<>
template< typename G >
__device__ void
mul_<4>::x(G &z, const G &x) {
    // TODO: Shift by two bits
    mul_<2>::x(z, x);  // z = 2x
    mul_<2>::x(z, z);  // z = 4x
}

template<>
template< typename G >
__device__ void
mul_<8>::x(G &z, const G &x) {
    // TODO: Shift by three bits
    mul_<4>::x(z, x);  // z = 4x
    mul_<2>::x(z, z);  // z = 8x
}

template<>
template< typename G >
__device__ void
mul_<16>::x(G &z, const G &x) {
    // TODO: Shift by four bits
    mul_<8>::x(z, x);  // z = 8x
    mul_<2>::x(z, z);  // z = 16x
}

template<>
template< typename G >
__device__ void
mul_<32>::x(G &z, const G &x) {
    // TODO: Shift by five bits
    mul_<16>::x(z, x); // z = 16x
    mul_<2>::x(z, z);  // z = 32x
}

template<>
template< typename G >
__device__ void
mul_<64>::x(G &z, const G &x) {
    // TODO: Shift by six bits
    mul_<32>::x(z, x); // z = 32x
    mul_<2>::x(z, z);  // z = 64x
}

template<>
template< typename G >
__device__ void
mul_<3>::x(G &z, const G &x) {
    G t;
    mul_<2>::x(t, x);
    G::add(z, t, x);
}

template<>
template< typename G >
__device__ void
mul_<11>::x(G &z, const G &x) {
    // TODO: Do this without carry/overflow checks
    // TODO: Check that this is indeed optimal
    // 11 = 8 + 2 + 1
    G t;
    mul_<2>::x(t, x);  // t = 2x
    G::add(z, t, x);   // z = 3x
    mul_<4>::x(t, t);  // t = 8x
    G::add(z, z, t);   // z = 11x
}

template<>
template< typename G >
__device__ void
mul_<13>::x(G &z, const G &x) {
    // 13 = 8 + 4 + 1
    G t;
    mul_<4>::x(t, x);  // t = 4x
    G::add(z, t, x);   // z = 5x
    mul_<2>::x(t, t);  // t = 8x
    G::add(z, z, t);   // z = 13x
}

template<>
template< typename G >
__device__ void
mul_<26>::x(G &z, const G &x) {
    // 26 = 16 + 8 + 2
    G t;
    mul_<2>::x(z, x); // z = 2x
    mul_<4>::x(t, z); // t = 8x
    G::add(z, z, t);  // z = 10x
    mul_<2>::x(t, t); // t = 16x
    G::add(z, z, t);  // z = 26x
}

template<>
template< typename G >
__device__ void
mul_<121>::x(G &z, const G &x) {
    // 121 = 64 + 32 + 16 + 8 + 1
    G t;
    mul_<8>::x(t, x); // t = 8x
    G::add(z, t, x);  // z = 9x
    mul_<2>::x(t, t); // t = 16x
    G::add(z, z, t);  // z = 25x
    mul_<2>::x(t, t); // t = 32x
    G::add(z, z, t);  // z = 57x
    mul_<2>::x(t, t); // t = 64x
    G::add(z, z, t);  // z = 121x
}

// TODO: Bleughk! This is obviously specific to MNT6 curve over Fp3.
template<>
template< typename Fp3 >
__device__ void
mul_<-1>::x(Fp3 &z, const Fp3 &x) {
    // multiply by (0, 0, 11) = 11 x^2  (where x^3 = alpha)
    static constexpr int CRV_A = 11;
    static constexpr int ALPHA = 11;
    Fp3 y = x;
    mul_<CRV_A * ALPHA>::x(z.a0, y.a1);
    mul_<CRV_A * ALPHA>::x(z.a1, y.a2);
    mul_<CRV_A>::x(z.a2, y.a0);
}


template< typename modulus_info >
struct Fp {
    typedef Fp PrimeField;

    var a;

    static constexpr int DEGREE = 1;

    __device__
    static void
    load(Fp &x, const var *mem) {
        int t = fixnum::layout().thread_rank();
        x.a = (t < ELT_LIMBS) ? mem[t] : 0UL;
    }

    __device__
    static void
    store(var *mem, const Fp &x) {
        int t = fixnum::layout().thread_rank();
        if (t < ELT_LIMBS)
            mem[t] = x.a;
    }

    __device__
    static int
    are_equal(const Fp &x, const Fp &y) { return fixnum::cmp(x.a, y.a) == 0; }

    __device__
    static void
    set_zero(Fp &x) { x.a = fixnum::zero(); }

    __device__
    static int
    is_zero(const Fp &x) { return fixnum::is_zero(x.a); }

    __device__
    static void
    set_one(Fp &x) { x.a = modulus_info::monty_one(); }

    __device__
    static void
    add(Fp &zz, const Fp &xx, const Fp &yy) {
        int br;
        var x = xx.a, y = yy.a, z, r;
        var mod = modulus_info::mod();
        fixnum::add(z, x, y);
        fixnum::sub_br(r, br, z, mod);
        zz.a = br ? z : r;
    }

    __device__
    static void
    neg(Fp &z, const Fp &x) {
        var mod = modulus_info::mod();
        fixnum::sub(z.a, mod, x.a);
    }

    __device__
    static void
    sub(Fp &z, const Fp &x, const Fp &y) {
        int br;
        var r, mod = modulus_info::mod();
        fixnum::sub_br(r, br, x.a, y.a);
        if (br)
            fixnum::add(r, r, mod);
        z.a = r;
    }

    __device__
    static void
    mul(Fp &zz, const Fp &xx, const Fp &yy) {
        auto grp = fixnum::layout();
        int L = grp.thread_rank();
        var mod = modulus_info::mod();

        var x = xx.a, y = yy.a, z = digit::zero();
        var tmp;
        digit::mul_lo(tmp, x, modulus_info::ninv_mod);
        digit::mul_lo(tmp, tmp, grp.shfl(y, 0));
        int cy = 0;

        for (int i = 0; i < ELT_LIMBS; ++i) {
            var u;
            var xi = grp.shfl(x, i);
            var z0 = grp.shfl(z, 0);
            var tmpi = grp.shfl(tmp, i);

            digit::mad_lo(u, z0, modulus_info::ninv_mod, tmpi);
            digit::mad_lo_cy(z, cy, mod, u, z);
            digit::mad_lo_cy(z, cy, y, xi, z);

            assert(L || z == 0);  // z[0] must be 0
            z = grp.shfl_down(z, 1); // Shift right one word
            z = (L >= ELT_LIMBS - 1) ? 0 : z;

            digit::add_cy(z, cy, z, cy);
            digit::mad_hi_cy(z, cy, mod, u, z);
            digit::mad_hi_cy(z, cy, y, xi, z);
        }
        // Resolve carries
        int msb = grp.shfl(cy, ELT_LIMBS - 1);
        cy = grp.shfl_up(cy, 1); // left shift by 1
        cy = (L == 0) ? 0 : cy;

        fixnum::add_cy(z, cy, z, cy);
        msb += cy;
        assert(msb == !!msb); // msb = 0 or 1.

        // br = 0 ==> z >= mod
        var r;
        int br;
        fixnum::sub_br(r, br, z, mod);
        if (msb || br == 0) {
            // If the msb was set, then we must have had to borrow.
            assert(!msb || msb == br);
            z = r;
        }
        zz.a = z;
    }

    __device__
    static void
    sqr(Fp &z, const Fp &x) {
        // TODO: Find a faster way to do this. Actually only option
        // might be full squaring with REDC.
        mul(z, x, x);
    }

#if 0
    __device__
    static void
    inv(Fp &z, const Fp &x) {
        // FIXME: Implement!  See HEHCC Algorithm 11.12.
        z = x;
    }
#endif

    __device__
    static void
    from_monty(Fp &y, const Fp &x) {
        Fp one;
        one.a = fixnum::one();
        mul(y, x, one);
    }
};



// Reference for multiplication and squaring methods below:
// https://pdfs.semanticscholar.org/3e01/de88d7428076b2547b60072088507d881bf1.pdf

template< typename Fp, int ALPHA >
struct Fp2 {
    typedef Fp PrimeField;

    // TODO: Use __builtin_align__(8) or whatever they use for the
    // builtin vector types.
    Fp a0, a1;

    static constexpr int DEGREE = 2;

    __device__
    static void
    load(Fp2 &x, const var *mem) {
        Fp::load(x.a0, mem);
        Fp::load(x.a1, mem + ELT_LIMBS);
    }

    __device__
    static void
    store(var *mem, const Fp2 &x) {
        Fp::store(mem, x.a0);
        Fp::store(mem + ELT_LIMBS, x.a1);
    }

    __device__
    static int
    are_equal(const Fp2 &x, const Fp2 &y) {
        return Fp::are_equal(x.a0, y.a0) && Fp::are_equal(x.a1, y.a1);
    }

    __device__
    static void
    set_zero(Fp2 &x) { Fp::set_zero(x.a0); Fp::set_zero(x.a1); }

    __device__
    static int
    is_zero(const Fp2 &x) { return Fp::is_zero(x.a0) && Fp::is_zero(x.a1); }

    __device__
    static void
    set_one(Fp2 &x) { Fp::set_one(x.a0); Fp::set_zero(x.a1); }

    __device__
    static void
    add(Fp2 &s, const Fp2 &x, const Fp2 &y) {
        Fp::add(s.a0, x.a0, y.a0);
        Fp::add(s.a1, x.a1, y.a1);
    }

    __device__
    static void
    sub(Fp2 &s, const Fp2 &x, const Fp2 &y) {
        Fp::sub(s.a0, x.a0, y.a0);
        Fp::sub(s.a1, x.a1, y.a1);
    }

    __device__
    static void
    mul(Fp2 &p, const Fp2 &a, const Fp2 &b) {
        Fp a0_b0, a1_b1, a0_plus_a1, b0_plus_b1, c, t0, t1;

        Fp::mul(a0_b0, a.a0, b.a0);
        Fp::mul(a1_b1, a.a1, b.a1);

        Fp::add(a0_plus_a1, a.a0, a.a1);
        Fp::add(b0_plus_b1, b.a0, b.a1);
        Fp::mul(c, a0_plus_a1, b0_plus_b1);

        mul_<ALPHA>::x(t0, a1_b1);
        Fp::sub(t1, c, a0_b0);

        Fp::add(p.a0, a0_b0, t0);
        Fp::sub(p.a1, t1, a1_b1);
    }


    __device__
    static void
    sqr(Fp2 &s, const Fp2 &a) {
        Fp a0_a1, a0_plus_a1, a0_plus_13_a1, t0, t1, t2;

        Fp::mul(a0_a1, a.a0, a.a1);
        Fp::add(a0_plus_a1, a.a0, a.a1);
        mul_<ALPHA>::x(t0, a.a1);
        Fp::add(a0_plus_13_a1, a.a0, t0);
        Fp::mul(t0, a0_plus_a1, a0_plus_13_a1);
        // TODO: Could do mul_14 to save a sub?
        Fp::sub(t1, t0, a0_a1);
        mul_<ALPHA>::x(t2, a0_a1);
        Fp::sub(s.a0, t1, t2);
        mul_<2>::x(s.a1, a0_a1);
    }
};


template< typename Fp, int ALPHA >
struct Fp3 {
    typedef Fp PrimeField;

    // TODO: Use __builtin_align__(8) or whatever they use for the
    // builtin vector types.
    Fp a0, a1, a2;

    static constexpr int DEGREE = 3;

    __device__
    static void
    load(Fp3 &x, const var *mem) {
        Fp::load(x.a0, mem);
        Fp::load(x.a1, mem + ELT_LIMBS);
        Fp::load(x.a2, mem + 2*ELT_LIMBS);
    }

    __device__
    static void
    store(var *mem, const Fp3 &x) {
        Fp::store(mem, x.a0);
        Fp::store(mem + ELT_LIMBS, x.a1);
        Fp::store(mem + 2*ELT_LIMBS, x.a2);
    }

    __device__
    static int
    are_equal(const Fp3 &x, const Fp3 &y) {
        return Fp::are_equal(x.a0, y.a0)
            && Fp::are_equal(x.a1, y.a1)
            && Fp::are_equal(x.a2, y.a2);
    }

    __device__
    static void
    set_zero(Fp3 &x) {
        Fp::set_zero(x.a0);
        Fp::set_zero(x.a1);
        Fp::set_zero(x.a2);
    }

    __device__
    static int
    is_zero(const Fp3 &x) {
        return Fp::is_zero(x.a0)
            && Fp::is_zero(x.a1)
            && Fp::is_zero(x.a2);
    }

    __device__
    static void
    set_one(Fp3 &x) {
        Fp::set_one(x.a0);
        Fp::set_zero(x.a1);
        Fp::set_zero(x.a2);
    }

    __device__
    static void
    add(Fp3 &s, const Fp3 &x, const Fp3 &y) {
        Fp::add(s.a0, x.a0, y.a0);
        Fp::add(s.a1, x.a1, y.a1);
        Fp::add(s.a2, x.a2, y.a2);
    }

    __device__
    static void
    sub(Fp3 &s, const Fp3 &x, const Fp3 &y) {
        Fp::sub(s.a0, x.a0, y.a0);
        Fp::sub(s.a1, x.a1, y.a1);
        Fp::sub(s.a2, x.a2, y.a2);
    }

    __device__
    static void
    mul(Fp3 &p, const Fp3 &a, const Fp3 &b) {
        Fp a0_b0, a1_b1, a2_b2;
        Fp a0_plus_a1, a1_plus_a2, a0_plus_a2, b0_plus_b1, b1_plus_b2, b0_plus_b2;
        Fp t0, t1, t2;

        Fp::mul(a0_b0, a.a0, b.a0);
        Fp::mul(a1_b1, a.a1, b.a1);
        Fp::mul(a2_b2, a.a2, b.a2);

        // TODO: Consider interspersing these additions among the
        // multiplications above.
        Fp::add(a0_plus_a1, a.a0, a.a1);
        Fp::add(a1_plus_a2, a.a1, a.a2);
        Fp::add(a0_plus_a2, a.a0, a.a2);

        Fp::add(b0_plus_b1, b.a0, b.a1);
        Fp::add(b1_plus_b2, b.a1, b.a2);
        Fp::add(b0_plus_b2, b.a0, b.a2);

        Fp::mul(t0, a1_plus_a2, b1_plus_b2);
        Fp::add(t1, a1_b1, a2_b2);
        Fp::sub(t0, t0, t1);
        mul_<ALPHA>::x(t0, t0);
        Fp::add(p.a0, a0_b0, t0);

        Fp::mul(t0, a0_plus_a1, b0_plus_b1);
        Fp::add(t1, a0_b0, a1_b1);
        mul_<ALPHA>::x(t2, a2_b2);
        Fp::sub(t2, t2, t1);
        Fp::add(p.a1, t0, t2);

        Fp::mul(t0, a0_plus_a2, b0_plus_b2);
        Fp::sub(t1, a1_b1, a0_b0);
        Fp::sub(t1, t1, a2_b2);
        Fp::add(p.a2, t0, t1);
    }

    __device__
    static void
    sqr(Fp3 &s, const Fp3 &a) {
        Fp a0a0, a1a1, a2a2;
        Fp a0_plus_a1, a1_plus_a2, a0_plus_a2;
        Fp t0, t1;

        Fp::sqr(a0a0, a.a0);
        Fp::sqr(a1a1, a.a1);
        Fp::sqr(a2a2, a.a2);

        // TODO: Consider interspersing these additions among the
        // squarings above.
        Fp::add(a0_plus_a1, a.a0, a.a1);
        Fp::add(a1_plus_a2, a.a1, a.a2);
        Fp::add(a0_plus_a2, a.a0, a.a2);

        Fp::sqr(t0, a1_plus_a2);
        // TODO: Remove sequential data dependencies (here and elsewhere)
        Fp::sub(t0, t0, a1a1);
        Fp::sub(t0, t0, a2a2);
        mul_<ALPHA>::x(t0, t0);
        Fp::add(s.a0, a0a0, t0);

        Fp::sqr(t0, a0_plus_a1);
        Fp::sub(t0, t0, a0a0);
        Fp::sub(t0, t0, a1a1);
        mul_<ALPHA>::x(t1, a2a2);
        Fp::add(s.a1, t0, t1);

        Fp::sqr(t0, a0_plus_a2);
        Fp::sub(t0, t0, a0a0);
        Fp::add(t0, t0, a1a1);
        Fp::sub(s.a2, t0, a2a2);
    }
};

typedef Fp<MNT4_MOD> Fp_MNT4;
typedef Fp2<Fp_MNT4, 13> Fp2_MNT4;

typedef Fp<MNT6_MOD> Fp_MNT6;
typedef Fp3<Fp_MNT6, 11> Fp3_MNT6;
