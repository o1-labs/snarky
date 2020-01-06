#pragma once

namespace cuFIXNUM {

namespace internal
{
    /*
     * Return floor(log2(x)). In particular, if x = 2^b, return b.
     */
    __device__
    constexpr unsigned
    floorlog2(unsigned x) {
        return x == 1 ? 0 : 1 + floorlog2(x >> 1);
    }

    /*
     * The following function gives a reasonable choice of WINDOW_SIZE in the k-ary
     * modular exponentiation method for a fixnum of B = 2^b bytes.
     *
     * The origin of the table is as follows. The expected number of multiplications
     * for the k-ary method with n-bit exponent and d-bit window is given by
     *
     *   T(n, d) = 2^d - 2 + n - d + (n/d - 1)*(1 - 2^-d)
     *
     * (see Koç, C. K., 1995, "Analysis of Sliding Window Techniques for
     * Exponentiation", Equation 1). The following GP script calculates the values
     * of n at which the window size should increase (maximum n = 65536):
     *
     *   ? T(n,d) = 2^d - 2 + n - d + (n/d - 1) * (1 - 2^-d);
     *   ? M = [ vecsort([[n, d, T(n, d)*1.] | d <- [1 .. 16]], 3)[1][2] | n <- [1 .. 65536] ];
     *   ? maxd = M[65536]
     *   10
     *   ? [[d, vecmin([n | n <- [1 .. 65536], M[n] == d])] | d <- [1 .. maxd]]
     *   [[1, 1], [2, 7], [3, 35], [4, 122], [5, 369], [6, 1044], [7, 2823], [8, 7371], [9, 18726], [10, 46490]]
     *
     * Table entry i is the window size for a fixnum of 8*(2^i) bits (e.g. 512 =
     * 8*2^6 bits falls between 369 and 1044, so the window size is that of the
     * smaller, 369, so 5 is in place i = 6).
     */
    // NB: For some reason we're not allowed to put this table in the definition
    // of bytes_to_window_size().
    constexpr int BYTES_TO_K_ARY_WINDOW_SIZE_TABLE[] = {
       -1,
       -1, //bytes bits
        2, // 2^2    32
        3, // 2^3    64
        4, // 2^4   128
        4, // 2^5   256
        5, // 2^6   512
        5, // 2^7  1024
        6, // 2^8  2048
        7, // 2^9  4096
        8, //2^10  8192
        8, //2^11 16384
        9, //2^12 32768
        10,//2^13 65536
    };

    __device__
    constexpr int
    bytes_to_k_ary_window_size(unsigned bytes) {
        return BYTES_TO_K_ARY_WINDOW_SIZE_TABLE[floorlog2(bytes)];
    }


    /*
     * This Table 2 from Koç, C. K., 1995, "Analysis of Sliding Window
     * Techniques for Exponentiation".
     *
     * The resolution of this table is higher than the one above because it's
     * used in the fixed exponent modexp code and can benefit from using the
     * precise bit length of the exponent, whereas the table above has to
     * accommodate multiple different exponents simultaneously.
     */
    __constant__
    int BYTES_TO_CLNW_WINDOW_SIZE_TABLE[] = {
       -1, // bits
        4, //  128
        5, //  256
        5, //  384
        5, //  512
        6, //  640
        6, //  768
        6, //  896
        6, // 1024
        6, // 1152
        6, // 1280
        6, // 1408
        6, // 1536
        6, // 1664
        7, // 1792
        7, // 1920
        7, // 2048
    };

    __device__
    constexpr int
    bits_to_clnw_window_size(unsigned bits) {
        // The chained ternary condition is forced upon us by the Draconian
        // constraints of C++11 constexpr functions.
        return
            bits < 64 ? 2 :
            bits < 128 ? 3 :
            bits > 2048 ? 7 :
            BYTES_TO_CLNW_WINDOW_SIZE_TABLE[(bits / 8) / 16];
    }

} // End namespace internal

} // End namespace cuFIXNUM
