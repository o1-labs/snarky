#pragma once

#include "functions/internal/modexp_impl.cu"
#include "modnum/modnum_monty_cios.cu"

namespace cuFIXNUM {

template< typename modnum_tp >
class modexp {
    typedef typename modnum_tp::fixnum fixnum;
    typedef typename fixnum::digit digit;

    // Decomposition of the exponent for use in the constant-width sliding-window
    // algorithm.  Allocated & deallocated once per thread block. Ref:
    // https://docs.nvidia.com/cuda/cuda-c-programming-guide/#per-thread-block-allocation
    // TODO: Consider storing the whole exp_wins array in shared memory.
    uint32_t *exp_wins;
    int exp_wins_len;
    int window_size;

    const modnum_tp modnum;

    // Helper functions for decomposing the exponent into windows.
    __device__ uint32_t
    scan_window(int &hi_idx, fixnum &n, int max_window_bits);

    __device__ int
    scan_zero_window(int &hi_idx, fixnum &n);

    __device__ uint32_t
    scan_nonzero_window(int &hi_idx, fixnum &n, int max_window_bits);

public:
    /*
     * NB: It is assumed that the caller has reduced exp and mod using knowledge
     * of their properties (e.g. reducing exp modulo phi(mod), CRT, etc.).
     */
    __device__ modexp(fixnum mod, fixnum exp);

    __device__ ~modexp();

    __device__ void operator()(fixnum &z, fixnum x) const;
};


template< typename modnum_tp >
__device__ uint32_t
modexp<modnum_tp>::scan_nonzero_window(int &hi_idx, fixnum &n, int max_window_bits) {
    uint32_t bits_remaining = hi_idx + 1, win_bits;
    digit w, lsd = fixnum::bottom_digit(n);

    internal::min(win_bits, bits_remaining, max_window_bits);
    digit::rem_2exp(w, lsd, win_bits);
    fixnum::rshift(n, n, win_bits);
    hi_idx -= win_bits;

    return w;
}


template< typename modnum_tp >
__device__ int
modexp<modnum_tp>::scan_zero_window(int &hi_idx, fixnum &n) {
    int nzeros = fixnum::two_valuation(n);
    fixnum::rshift(n, n, nzeros);
    hi_idx -= nzeros;
    return nzeros;
}


template< typename modnum_tp >
__device__ uint32_t
modexp<modnum_tp>::scan_window(int &hi_idx, fixnum &n, int max_window_bits) {
    int nzeros;
    uint32_t window;
    nzeros = scan_zero_window(hi_idx, n);
    window = scan_nonzero_window(hi_idx, n, max_window_bits);
    // top half is the odd window, bottom half is nzeros
    // TODO: fix magic number
    return (window << 16) | nzeros;
}


template< typename modnum_tp >
__device__
modexp<modnum_tp>::modexp(fixnum mod, fixnum exp)
    : modnum(mod)
{
    // sliding window decomposition
    int hi_idx;

    hi_idx = fixnum::msb(exp);
    window_size = internal::bits_to_clnw_window_size(hi_idx + 1);

    uint32_t *data;
    int L = fixnum::layout::laneIdx();
    // TODO: This does one malloc per slot; the sliding window exponentiation
    // only really makes sense with fixed exponent, so we should be able to arrange
    // things so we only need one malloc per warp or even one malloc per thread block.
    if (L == 0) {
        int max_windows;
        internal::ceilquo(max_windows, fixnum::BITS, window_size);
        // NB: Default heap on the device is 8MB.
        data = (uint32_t *) malloc(max_windows * sizeof(uint32_t));
        // FIXME: Handle this error properly.
        assert(data != nullptr);
    }
    // Broadcast data to each thread in the slot.
    exp_wins = (uint32_t *) __shfl_sync(fixnum::layout::mask(), (uintptr_t)data, 0, fixnum::layout::WIDTH);
    uint32_t *ptr = exp_wins;
    while (hi_idx >= 0)
        *ptr++ = scan_window(hi_idx, exp, window_size);
    exp_wins_len = ptr - exp_wins;
}


template< typename modnum_tp >
__device__
modexp<modnum_tp>::~modexp()
{
    if (fixnum::layout::laneIdx() == 0)
        free(exp_wins);
}


template< typename modnum_tp >
__device__ void
modexp<modnum_tp>::operator()(fixnum &z, fixnum x) const
{
    static constexpr int WINDOW_MAX_BITS = 16;
    static constexpr int WINDOW_LEN_MASK = (1UL << WINDOW_MAX_BITS) - 1UL;
    // TODO: Actual maximum is 16 at the moment (see above), but it will very
    // rarely need to be more than 7. Consider storing G in shared memory to
    // remove the need for WINDOW_MAX_BITS altogether.
    static constexpr int WINDOW_MAX_BITS_REDUCED = 7;
    static constexpr int WINDOW_MAX_VAL_REDUCED = 1U << WINDOW_MAX_BITS_REDUCED;
    assert(window_size <= WINDOW_MAX_BITS_REDUCED);

    // We need to know that exp_wins_len > 0 when z is initialised just before
    // the main loop.
    if (exp_wins_len == 0) {
        //z = fixnum::one();
        // TODO: This complicated way of producing a 1 is to
        // accommodate the possibility that monty.is_valid is false.
        modnum.from_modnum(z, modnum.one());
        return;
    }

    // TODO: handle case of small exponent specially

    int window_max = 1U << window_size;
    /* G[t] = z^(2t + 1) t >= 0 (odd powers of z) */
    fixnum G[WINDOW_MAX_VAL_REDUCED / 2];
    modnum.to_modnum(z, x);
    G[0] = z;
    if (window_size > 1) {
        modnum.sqr(z, z);
        for (int t = 1; t < window_max / 2; ++t) {
            G[t] = G[t - 1];
            modnum.mul(G[t], G[t], z);
        }
    }

    // Iterate over windows from most significant window to least significant
    // (i.e. reverse order from the order they're stored).
    const uint32_t *windows = exp_wins + exp_wins_len - 1;
    uint32_t win = *windows--;
    uint16_t two_val = win & WINDOW_LEN_MASK;
    uint16_t e = win >> WINDOW_MAX_BITS;

    z = G[e / 2];
    while (two_val-- > 0)
        modnum.sqr(z, z);

    while (windows >= exp_wins) {
        two_val = window_size;
        while (two_val-- > 0)
            modnum.sqr(z, z);

        win = *windows--;
        two_val = win & WINDOW_LEN_MASK;
        e = win >> WINDOW_MAX_BITS;

        modnum.mul(z, z, G[e / 2]);
        while (two_val-- > 0)
            modnum.sqr(z, z);
    }
    modnum.from_modnum(z, z);
}

} // End namespace cuFIXNUM
