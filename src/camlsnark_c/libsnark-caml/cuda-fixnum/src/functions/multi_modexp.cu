#pragma once

#include "functions/internal/modexp_impl.cu"
#include "modnum/modnum_monty_cios.cu"

namespace cuFIXNUM {

template<
    typename modnum_tp,
    int WINDOW_SIZE = internal::bytes_to_k_ary_window_size(modnum_tp::fixnum::BYTES) >
class multi_modexp {
    static_assert(WINDOW_SIZE >= 1 && WINDOW_SIZE < modnum_tp::fixnum::digit::BITS,
        "Invalid window size.");

    // TODO: Generalise multi_modexp so that it can work with any modular
    // multiplication algorithm.
    const modnum_tp modnum;

public:
    typedef typename modnum_tp::fixnum fixnum;

    __device__ multi_modexp(fixnum mod)
    : modnum(mod) { }

    __device__ void operator()(fixnum &z, fixnum x, fixnum e) const;
};


/*
 * Left-to-right k-ary exponentiation (see [HAC, Algorithm 14.82]).
 *
 * If it is known that the exponents given to this function will be small, then
 * a better window size can be chosen. The window size should be the left value
 * in the pair below whose right value is the largest less than the exponent.
 * For example, exponents of 192 bits should take the window 4 corresponding to
 * 122.
 *
 * [[1, 1], [2, 7], [3, 35], [4, 122], [5, 369], [6, 1044],
 *  [7, 2823], [8, 7371], [9, 18726], [10, 46490]]
 *
 * See the documentation in "functions/internal/modexp_impl.cu" for more
 * information.
 *
 * TODO: The basic algorithm is applied to each word of the exponent in turn, so
 * the last window used on each exponent word will be smaller than WINDOW_SIZE.
 * Need a better way to scan the exponent so that the same WINDOW_SIZE is used
 * throughout.
 *
 * TODO: Should only start the algorithm at the msb of e; it will result in many
 * idle threads, but the current setup means they do pointless work; at least if
 * they're idle they might make space for other work to be done. Document the
 * fact that inputs should be ordered such that groups with similar exponents
 * are together.
 *
 * NB: I don't immediately see how to use the "modified" variant [HAC, Algo
 * 14.83] since there the number of squarings depends on the 2-adic valuation of
 * the window value.
 */
template< typename modnum_tp, int WINDOW_SIZE >
__device__ void
multi_modexp<modnum_tp, WINDOW_SIZE>::operator()(fixnum &z, fixnum x, fixnum e) const
{
    typedef typename modnum_tp::fixnum::digit digit;
    static constexpr int WIDTH = fixnum::SLOT_WIDTH;

    // Window decomposition: digit::BITS = q * WINDOW_SIZE + r.
    static constexpr int WINDOW_REM_BITS = digit::BITS % WINDOW_SIZE;
    static constexpr int WINDOW_MAX = (1U << WINDOW_SIZE);

    /* G[t] = z^t, t >= 0 */
    fixnum G[WINDOW_MAX];
    modnum.to_modnum(z, x);
    G[0] = modnum.one();
    for (int t = 1; t < WINDOW_MAX; ++t) {
        G[t] = G[t - 1];
        modnum.mul(G[t], G[t], z);
    }

    z = G[0];
    for (int i = WIDTH - 1; i >= 0; --i) {
        digit f = fixnum::get(e, i);

        // TODO: The squarings are noops on the first iteration (i =
        // w-1) and should be removed.
        digit win; // TODO: Morally this should be an int
        for (int j = digit::BITS - WINDOW_SIZE; j >= 0; j -= WINDOW_SIZE) {
            // TODO: For some bizarre reason, it is significantly
            // faster to do this loop than it is to unroll the 5
            // statements manually.  Idem for the remainder below.
            // Investigate how this is even possible!
            for (int k = 0; k < WINDOW_SIZE; ++k)
                modnum.sqr(z, z);
            digit fj;
            // win = (f >> j) & WINDOW_MAIN_MASK;
            digit::rshift(fj, f, j);
            digit::rem_2exp(win, fj, WINDOW_SIZE);
            modnum.mul(z, z, G[win]);
        }

        // Remainder
        for (int k = 0; k < WINDOW_REM_BITS; ++k)
            modnum.sqr(z, z);
        //win = f & WINDOW_REM_MASK;
        digit::rem_2exp(win, f, WINDOW_REM_BITS);
        modnum.mul(z, z, G[win]);
    }
    modnum.from_modnum(z, z);
}

} // End namespace cuFIXNUM
