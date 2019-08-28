

template< typename digit >
__device__ __forceinline__ void
hand_add(digit &r, digit a, digit b)
{
    r = a + b;
}


template< typename digit, int NAIL_BITS >
struct nail_data
{
    typedef typename digit digit;
    // FIXME: This doesn't work if digit is signed
    constexpr digit DIGIT_MAX = ~(digit)0;
    constexpr int DIGIT_BITS = sizeof(digit) * 8;
    constexpr int NON_NAIL_BITS = DIGIT_BITS - NAIL_BITS;
    constexpr digit NAIL_MASK = DIGIT_MAX << NON_NAIL_BITS;
    constexpr digit NON_NAIL_MASK = ~NAIL_MASK;
    constexpr digit NON_NAIL_MAX = NON_NAIL_MASK; // alias

    // A nail must fit in an int.
    static_assert(NAIL_BITS > 0 && NAIL_BITS < sizeof(int) * 8,
            "invalid number of nail bits");
};


// TODO: This is ugly
template< typename digit, int NAIL_BITS >
__device__ __forceinline__ int
hand_extract_nail(digit &r)
{
    typedef nail_data<digit, NAIL_BITS> nd;

    // split r into nail and non-nail parts
    nail = r >> nd::NON_NAIL_BITS;
    r &= nd::NON_NAIL_MASK;
    return nail;
}


/*
 * Current cost of nail resolution is 4 vote functions.
 */
template< typename digit, int NAIL_BITS >
__device__ int
hand_resolve_nails(digit &r)
{
    // TODO: Make this work with a general width
    constexpr int WIDTH = warpSize;
    // TODO: This is ugly
    typedef nail_data<digit, NAIL_BITS> nd;
    typedef subwarp_data<WIDTH> subwarp;

    int nail, nail_hi;
    nail = hand_extract_nail<digit, NAIL_BITS>(r);
    nail_hi = subwarp::shfl(nail, subwarp::toplaneIdx);

    nail = subwarp::shfl_up0(nail, 1);
    r += nail;

    // nail is 0 or 1 this time
    nail = hand_extract_nail<digit, NAIL_BITS>(r);

    return nail_hi + hand_resolve_cy(r, nail, nd::NON_NAIL_MAX);
}


template< typename digit, int NAIL_BITS, int WIDTH = warpSize >
__device__ void
hand_mullo_nail(digit &r, digit a, digit b)
{
    // FIXME: We shouldn't need nail bits to divide the width
    static_assert(!(WIDTH % NAIL_BITS), "nail bits does not divide width");
    // FIXME: also need to check that digit has enough space for the
    // accumulated nails.

    typedef subwarp_data<WIDTH> subwarp;

    digit n = 0; // nails

    r = 0;
    for (int i = WIDTH - 1; i >= 0; --i) {
        // FIXME: Should this be NAIL_BITS/2? Because there are two
        // additions (hi & lo)? Maybe at most one of the two additions
        // will cause an overflow? For example, 0xff * 0xff = 0xfe01
        // so overflow is likely in the first case and unlikely in the
        // second...
        for (int j = 0; j < NAIL_BITS; ++j, --i) {
            digit aa = subwarp::shfl(a, i);

            // TODO: See if using umad.wide improves this.
            umad_hi(r, aa, b, r);
            r = subwarp::shfl_up0(r, 1);
            umad_lo(r, aa, b, r);
        }
        // FIXME: Supposed to shuffle up n by NAIL_BITS digits
        // too. Can this be avoided?
        n += hand_extract_nails(r);
    }
    n = subwarp::shfl_up0(n, 1);
    hand_add(r, r, n);
    hand_resolve_nails(r);
}

