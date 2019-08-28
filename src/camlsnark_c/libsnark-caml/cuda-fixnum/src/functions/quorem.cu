#pragma once

namespace cuFIXNUM {

/*
 * Quotient and remainder via long-division.
 *
 * Source: MCA Algo 1.6, HAC Algo 14.20.
 *
 * TODO: Implement Svoboda divisor preconditioning (using
 * Newton-Raphson iteration to calculate floor(beta^(n+1)/div)) (see
 * MCA Algo 1.7).
 */
template< typename fixnum >
class quorem {
    static constexpr int WIDTH = fixnum::SLOT_WIDTH;
    typedef typename fixnum::digit digit;

public:
    __device__ void operator()(
        fixnum &q, fixnum &r,
        fixnum A, fixnum div) const;

    // TODO: These functions obviously belong somewhere else. The need
    // to be available to both quorem (here) and quorem_preinv.
    static __device__ int normalise_divisor(fixnum &div);
    static __device__ fixnum normalise_dividend(fixnum &u, int k);
    static __device__ fixnum normalise_dividend(fixnum &u_hi, fixnum &u_lo, int k);
    static __device__ void quorem_with_candidate_quotient(
        fixnum &quo, fixnum &rem,
        fixnum A_hi, fixnum A_lo, fixnum div, fixnum q);
};

template< typename fixnum >
__device__ int
quorem<fixnum>::normalise_divisor(fixnum &div) {
    static constexpr int BITS = fixnum::BITS;
    int lz = BITS - (fixnum::msb(div) + 1);
    fixnum overflow;
    fixnum::lshift(div, overflow, div, lz);
    assert(fixnum::is_zero(overflow));
    return lz;
}

// TODO: Ideally the algos would be written to incorporate the
// normalisation factor, rather than "physically" normalising the
// dividend.
template< typename fixnum >
__device__ fixnum
quorem<fixnum>::normalise_dividend(fixnum &u, int k) {
    fixnum overflow;
    fixnum::lshift(u, overflow, u, k);
    return overflow;
}

// TODO: Ideally the algos would be written to incorporate the
// normalisation factor, rather than "physically" normalising the
// dividend.
template< typename fixnum >
__device__ fixnum
quorem<fixnum>::normalise_dividend(fixnum &u_hi, fixnum &u_lo, int k) {
    fixnum hi_part, middle_part;
    fixnum::lshift(u_hi, hi_part, u_hi, k);
    fixnum::lshift(u_lo, middle_part, u_lo, k);
    digit cy;
    fixnum::add_cy(u_hi, cy, u_hi, middle_part);
    assert(digit::is_zero(cy));
    return hi_part;
}

template< typename fixnum >
__device__ void
quorem<fixnum>::quorem_with_candidate_quotient(
    fixnum &quo, fixnum &rem,
    fixnum A_hi, fixnum A_lo, fixnum div, fixnum q)
{
    fixnum hi, lo, r, t, msw;
    digit br;
    int L = fixnum::layout::laneIdx();

    // (hi, lo) = q*d
    fixnum::mul_wide(hi, lo, q, div);

    // (msw, r) = A - q*d
    fixnum::sub_br(r, br, A_lo, lo);
    fixnum::sub_br(msw, t, A_hi, hi);
    assert(digit::is_zero(t));  // A_hi >= hi

    // TODO: Could skip these two lines if we could pass br to the last
    // sub_br above as a "borrow in".
    // Make br into a fixnum
    br = (L == 0) ? br : digit::zero(); // digit to fixnum
    fixnum::sub_br(msw, t, msw, br);
    assert(digit::is_zero(t));  // msw >= br
    assert((L == 0 && digit::cmp(msw, 4) < 0)
           || digit::is_zero(msw)); // msw < 4 (TODO: possibly should have msw < 3)
    // Broadcast
    msw = fixnum::layout::shfl(msw, 0);

    // NB: Could call incr_cy in the loops instead; as is, it will
    // incur an extra add_cy even when msw is 0 and r < d.
    digit q_inc = digit::zero();
    while ( ! digit::is_zero(msw)) {
        fixnum::sub_br(r, br, r, div);
        digit::sub(msw, msw, br);
        digit::incr(q_inc);
    }
    fixnum::sub_br(t, br, r, div);
    while (digit::is_zero(br)) {
        r = t;
        digit::incr(q_inc);
        fixnum::sub_br(t, br, r, div);
    }
    // TODO: Replace loops above with something like the one below,
    // which will reduce warp divergence a bit.
#if 0
    fixnum tmp, q_inc;
    while (1) {
        br = fixnum::sub_br(tmp, r, div);
        if (msw == 0 && br == 1)
            break;
        msr -= br;
        ++q_inc;
        r = tmp;
    }
#endif

    q_inc = (L == 0) ? q_inc : digit::zero();
    fixnum::add(q, q, q_inc);

    quo = q;
    rem = r;
}

#if 0
template< typename fixnum >
__device__ void
quorem<fixnum>::operator()(
    fixnum &q_hi, fixnum &q_lo, fixnum &r,
    fixnum A_hi, fixnum A_lo, fixnum div) const
{
    int k = normalise_divisor(div);
    fixnum t = normalise_dividend(A_hi, A_lo, k);
    assert(t == 0); // dividend too big.

    fixnum r_hi;
    (*this)(q_hi, r_hi, A_hi, div);

    // FIXME WRONG! r_hi is not a good enough candidate quotient!
    // Do div2by1 of (r_hi, A_lo) by div using that r_hi < div.
    // r_hi is now the candidate quotient
    fixnum qq = r_hi;
    if (fixnum::cmp(A_lo, div) > 0)
        fixnum::incr_cy(qq);

    quorem_with_candidate_quotient(q_lo, r, r_hi, A_lo, div, qq);

    digit lo_bits = fixnum::rshift(r, r, k);
    assert(lo_bits == 0);
}
#endif

// TODO: Implement a specifically *parallel* algorithm for division,
// such as those of Takahashi.
template< typename fixnum >
__device__ void
quorem<fixnum>::operator()(
    fixnum &q, fixnum &r, fixnum A, fixnum div) const
{
    int n = fixnum::most_sig_dig(div) + 1;
    assert(n >= 0); // division by zero.

    digit div_msw = fixnum::get(div, n - 1);

    // TODO: Factor out the normalisation code.
    int k = digit::clz(div_msw); // guaranteed to be >= 0, since div_msw != 0

    // div is normalised when its msw is >= 2^(WORD_BITS - 1),
    // i.e. when its highest bit is on, i.e. when the number of
    // leading zeros of msw is 0.
    if (k > 0) {
        fixnum h;
        // Normalise div by shifting it to the left.
        fixnum::lshift(div, h, div, k);
        assert(fixnum::is_zero(h));
        fixnum::lshift(A, h, A, k);
        // FIXME: We should be able to handle this case.
        assert(fixnum::is_zero(h));  // FIXME: check if h == 0 using cmp() and zero()
        digit::lshift(div_msw, div_msw, k);
    }

    int m = fixnum::most_sig_dig(A) - n + 1;
    // FIXME: Just return div in this case
    assert(m >= 0); // dividend too small

    // TODO: Work out if we can just incorporate the normalisation factor k
    // into the subsequent algorithm, rather than actually modifying div and A.

    q = r = fixnum::zero();

    // Set q_m
    digit qj;
    fixnum dj, tmp;
    // TODO: Urgh.
    typedef typename fixnum::layout layout;
    dj = layout::shfl_up0(div, m);
    digit br;
    fixnum::sub_br(tmp, br, A, dj);
    if (br) qj = fixnum::zero(); // dj > A
    else { qj = fixnum::one(); A = tmp; }

    fixnum::set(q, qj, m);

    digit dinv = internal::quorem_reciprocal(div_msw);
    for (int j = m - 1; j >= 0; --j) {
        digit a_hi, a_lo, hi, dummy;

        // (q_hi, q_lo) = floor((a_{n+j} B + a_{n+j-1}) / div_msw)
        // TODO: a_{n+j} is a_{n+j-1} from the previous iteration; hence I
        // should be able to get away with just one call to get() per
        // iteration.
        // TODO: Could normalise A on the fly here, one word at a time.
        a_hi = fixnum::get(A, n + j);
        a_lo = fixnum::get(A, n + j - 1);

        // TODO: uquorem_wide has a bad branch at the start which will
        // cause trouble when div_msw < a_hi is not universally true
        // across the warp. Need to investigate ways to alleviate that.
        digit::quorem_wide(qj, dummy, a_hi, a_lo, div_msw, dinv);

        dj = layout::shfl_up0(div, j);
        hi = fixnum::mul_digit(tmp, qj, dj);
        assert(digit::is_zero(hi));

        int iters = 0;
        fixnum AA;
        while (1) {
            fixnum::sub_br(AA, br, A, tmp);
            if (!br)
                break;
            fixnum::sub_br(tmp, br, tmp, dj);
            assert(digit::is_zero(br));
            --qj;
            ++iters;
        }
        A = AA;
        assert(iters <= 2); // MCA, Proof of Theorem 1.3.
        fixnum::set(q, qj, j);
    }
    // Denormalise A to produce r.
    fixnum::rshift(r, tmp, A, k);
    assert(fixnum::is_zero(tmp)); // Above division should be exact.
}

} // End namespace cuFIXNUM
