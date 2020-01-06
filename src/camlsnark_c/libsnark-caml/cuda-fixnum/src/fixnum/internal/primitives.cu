#pragma once

#include <cstdint>
#include <cassert>
#include <type_traits>

namespace cuFIXNUM {

namespace internal {
    typedef std::uint32_t u32;
    typedef std::uint64_t u64;

    __device__ __forceinline__
    void
    addc(u32 &s, u32 a, u32 b) {
        asm ("addc.u32 %0, %1, %2;"
             : "=r"(s)
             : "r"(a), "r" (b));
    }

    __device__ __forceinline__
    void
    add_cc(u32 &s, u32 a, u32 b) {
        asm ("add.cc.u32 %0, %1, %2;"
             : "=r"(s)
             : "r"(a), "r" (b));
    }

    __device__ __forceinline__
    void
    addc_cc(u32 &s, u32 a, u32 b) {
        asm ("addc.cc.u32 %0, %1, %2;"
             : "=r"(s)
             : "r"(a), "r" (b));
    }

    __device__ __forceinline__
    void
    addc(u64 &s, u64 a, u64 b) {
        asm ("addc.u64 %0, %1, %2;"
             : "=l"(s)
             : "l"(a), "l" (b));
    }

    __device__ __forceinline__
    void
    add_cc(u64 &s, u64 a, u64 b) {
        asm ("add.cc.u64 %0, %1, %2;"
             : "=l"(s)
             : "l"(a), "l" (b));
    }

    __device__ __forceinline__
    void
    addc_cc(u64 &s, u64 a, u64 b) {
        asm ("addc.cc.u64 %0, %1, %2;"
             : "=l"(s)
             : "l"(a), "l" (b));
    }

    /*
     * hi * 2^n + lo = a * b
     */
    __device__ __forceinline__
    void
    mul_hi(u32 &hi, u32 a, u32 b) {
        asm ("mul.hi.u32 %0, %1, %2;"
             : "=r"(hi)
             : "r"(a), "r"(b));
    }

    __device__ __forceinline__
    void
    mul_hi(u64 &hi, u64 a, u64 b) {
        asm ("mul.hi.u64 %0, %1, %2;"
             : "=l"(hi)
             : "l"(a), "l"(b));
    }


    /*
     * hi * 2^n + lo = a * b
     */
    __device__ __forceinline__
    void
    mul_wide(u32 &hi, u32 &lo, u32 a, u32 b) {
        // TODO: Measure performance difference between this and the
        // equivalent:
        //   mul.hi.u32 %0, %2, %3
        //   mul.lo.u32 %1, %2, %3
        asm ("{\n\t"
             " .reg .u64 tmp;\n\t"
             " mul.wide.u32 tmp, %2, %3;\n\t"
             " mov.b64 { %1, %0 }, tmp;\n\t"
             "}"
             : "=r"(hi), "=r"(lo)
             : "r"(a), "r"(b));
    }

    __device__ __forceinline__
    void
    mul_wide(u64 &hi, u64 &lo, u64 a, u64 b) {
        asm ("mul.hi.u64 %0, %2, %3;\n\t"
             "mul.lo.u64 %1, %2, %3;"
             : "=l"(hi), "=l"(lo)
             : "l"(a), "l"(b));
    }

    /*
     * (hi, lo) = a * b + c
     */
    __device__ __forceinline__
    void
    mad_wide(u32 &hi, u32 &lo, u32 a, u32 b, u32 c) {
        asm ("{\n\t"
             " .reg .u64 tmp;\n\t"
             " mad.wide.u32 tmp, %2, %3, %4;\n\t"
             " mov.b64 { %1, %0 }, tmp;\n\t"
             "}"
             : "=r"(hi), "=r"(lo)
             : "r"(a), "r"(b), "r"(c));
    }

    __device__ __forceinline__
    void
    mad_wide(u64 &hi, u64 &lo, u64 a, u64 b, u64 c) {
        asm ("mad.lo.cc.u64 %1, %2, %3, %4;\n\t"
             "madc.hi.u64 %0, %2, %3, 0;"
             : "=l"(hi), "=l"(lo)
             : "l"(a), "l" (b), "l"(c));
    }

    // lo = a * b + c (mod 2^n)
    __device__ __forceinline__
    void
    mad_lo(u32 &lo, u32 a, u32 b, u32 c) {
        asm ("mad.lo.u32 %0, %1, %2, %3;"
             : "=r"(lo)
             : "r"(a), "r" (b), "r"(c));
    }

    __device__ __forceinline__
    void
    mad_lo(u64 &lo, u64 a, u64 b, u64 c) {
        asm ("mad.lo.u64 %0, %1, %2, %3;"
             : "=l"(lo)
             : "l"(a), "l" (b), "l"(c));
    }


    // as above but with carry in cy
    __device__ __forceinline__
    void
    mad_lo_cc(u32 &lo, u32 a, u32 b, u32 c) {
        asm ("mad.lo.cc.u32 %0, %1, %2, %3;"
             : "=r"(lo)
             : "r"(a), "r" (b), "r"(c));
    }

    __device__ __forceinline__
    void
    mad_lo_cc(u64 &lo, u64 a, u64 b, u64 c) {
        asm ("mad.lo.cc.u64 %0, %1, %2, %3;"
             : "=l"(lo)
             : "l"(a), "l" (b), "l"(c));
    }

    __device__ __forceinline__
    void
    madc_lo_cc(u32 &lo, u32 a, u32 b, u32 c) {
        asm ("madc.lo.cc.u32 %0, %1, %2, %3;"
             : "=r"(lo)
             : "r"(a), "r" (b), "r"(c));
    }

    __device__ __forceinline__
    void
    madc_lo_cc(u64 &lo, u64 a, u64 b, u64 c) {
        asm ("madc.lo.cc.u64 %0, %1, %2, %3;"
             : "=l"(lo)
             : "l"(a), "l" (b), "l"(c));
    }

    __device__ __forceinline__
    void
    mad_hi(u32 &hi, u32 a, u32 b, u32 c) {
        asm ("mad.hi.u32 %0, %1, %2, %3;"
             : "=r"(hi)
             : "r"(a), "r" (b), "r"(c));
    }

    __device__ __forceinline__
    void
    mad_hi(u64 &hi, u64 a, u64 b, u64 c) {
        asm ("mad.hi.u64 %0, %1, %2, %3;"
             : "=l"(hi)
             : "l"(a), "l" (b), "l"(c));
    }

    __device__ __forceinline__
    void
    mad_hi_cc(u32 &hi, u32 a, u32 b, u32 c) {
        asm ("mad.hi.cc.u32 %0, %1, %2, %3;"
             : "=r"(hi)
             : "r"(a), "r" (b), "r"(c));
    }

    __device__ __forceinline__
    void
    mad_hi_cc(u64 &hi, u64 a, u64 b, u64 c) {
        asm ("mad.hi.cc.u64 %0, %1, %2, %3;"
             : "=l"(hi)
             : "l"(a), "l" (b), "l"(c));
    }

    __device__ __forceinline__
    void
    madc_hi_cc(u32 &hi, u32 a, u32 b, u32 c) {
        asm ("madc.hi.cc.u32 %0, %1, %2, %3;"
             : "=r"(hi)
             : "r"(a), "r" (b), "r"(c));
    }

    __device__ __forceinline__
    void
    madc_hi_cc(u64 &hi, u64 a, u64 b, u64 c) {
        asm ("madc.hi.cc.u64 %0, %1, %2, %3;\n\t"
             : "=l"(hi)
             : "l"(a), "l" (b), "l"(c));
    }

    // Source: https://docs.nvidia.com/cuda/parallel-thread-execution/#logic-and-shift-instructions-shf
    __device__ __forceinline__
    void
    lshift(u32 &out_hi, u32 &out_lo, u32 in_hi, u32 in_lo, unsigned b) {
        asm ("shf.l.clamp.b32 %1, %2, %3, %4;\n\t"
             "shl.b32 %0, %2, %4;"
             : "=r"(out_lo), "=r"(out_hi) : "r"(in_lo), "r"(in_hi), "r"(b));
    }

    /*
     * Left shift by b bits; b <= 32.
     * Source: https://docs.nvidia.com/cuda/parallel-thread-execution/#logic-and-shift-instructions-shf
     */
    __device__ __forceinline__
    void
    lshift_b32(u64 &out_hi, u64 &out_lo, u64 in_hi, u64 in_lo, unsigned b) {
        assert(b <= 32);
        asm ("{\n\t"
             " .reg .u32 t1;\n\t"
             " .reg .u32 t2;\n\t"
             " .reg .u32 t3;\n\t"
             " .reg .u32 t4;\n\t"
             " .reg .u32 t5;\n\t"
             " .reg .u32 t6;\n\t"
             " .reg .u32 t7;\n\t"
             " .reg .u32 t8;\n\t"
             // (t4, t3, t2, t1) = (in_hi, in_lo)
             " mov.b64 { t3, t4 }, %3;\n\t"
             " mov.b64 { t1, t2 }, %2;\n\t"
             " shf.l.clamp.b32 t8, t3, t4, %4;\n\t"
             " shf.l.clamp.b32 t7, t2, t3, %4;\n\t"
             " shf.l.clamp.b32 t6, t1, t2, %4;\n\t"
             " shl.b32 t5, t1, %4;\n\t"
             " mov.b64 %1, { t7, t8 };\n\t"
             " mov.b64 %0, { t5, t6 };\n\t"
             "}"
             : "=l"(out_lo), "=l"(out_hi) : "l"(in_lo), "l"(in_hi), "r"(b));
    }

    __device__ __forceinline__
    void
    lshift(u64 &out_hi, u64 &out_lo, u64 in_hi, u64 in_lo, unsigned b) {
        assert(b <= 64);
        unsigned c = min(b, 32);
        lshift_b32(out_hi, out_lo, in_hi, in_lo, c);
        lshift_b32(out_hi, out_lo, out_hi, out_lo, b - c);
    }

    // Source: https://docs.nvidia.com/cuda/parallel-thread-execution/#logic-and-shift-instructions-shf
    __device__ __forceinline__
    void
    rshift(u32 &out_hi, u32 &out_lo, u32 in_hi, u32 in_lo, unsigned b) {
        asm ("shf.r.clamp.b32 %0, %2, %3, %4;\n\t"
             "shr.b32 %1, %2, %4;"
             : "=r"(out_lo), "=r"(out_hi) : "r"(in_lo), "r"(in_hi), "r"(b));
    }

    /*
     * Right shift by b bits; b <= 32.
     * Source: https://docs.nvidia.com/cuda/parallel-thread-execution/#logic-and-shift-instructions-shf
     */
    __device__ __forceinline__
    void
    rshift_b32(u64 &out_hi, u64 &out_lo, u64 in_hi, u64 in_lo, unsigned b) {
        assert(b <= 32);
        asm ("{\n\t"
             " .reg .u32 t1;\n\t"
             " .reg .u32 t2;\n\t"
             " .reg .u32 t3;\n\t"
             " .reg .u32 t4;\n\t"
             " .reg .u32 t5;\n\t"
             " .reg .u32 t6;\n\t"
             " .reg .u32 t7;\n\t"
             " .reg .u32 t8;\n\t"
             // (t4, t3, t2, t1) = (in_hi, in_lo)
             " mov.b64 { t1, t2 }, %2;\n\t"
             " mov.b64 { t3, t4 }, %3;\n\t"
             " shf.r.clamp.b32 t5, t1, t2, %4;\n\t"
             " shf.r.clamp.b32 t6, t2, t3, %4;\n\t"
             " shf.r.clamp.b32 t7, t3, t4, %4;\n\t"
             " shr.b32 t8, t4, %4;\n\t"
             " mov.b64 %0, { t5, t6 };\n\t"
             " mov.b64 %1, { t7, t8 };\n\t"
             "}"
             : "=l"(out_lo), "=l"(out_hi) : "l"(in_lo), "l"(in_hi), "r"(b));
    }

    __device__ __forceinline__
    void
    rshift(u64 &out_hi, u64 &out_lo, u64 in_hi, u64 in_lo, unsigned b) {
        assert(b <= 64);
        unsigned c = min(b, 32);
        rshift_b32(out_hi, out_lo, in_hi, in_lo, c);
        rshift_b32(out_hi, out_lo, out_hi, out_lo, b - c);
    }

    /*
     * Count Leading Zeroes in x.
     */
    __device__ __forceinline__
    int
    clz(u32 x) {
        int n;
        asm ("clz.b32 %0, %1;" : "=r"(n) : "r"(x));
        return n;
    }

    __device__ __forceinline__
    int
    clz(u64 x) {
        int n;
        asm ("clz.b64 %0, %1;" : "=r"(n) : "l"(x));
        return n;
    }

    /*
     * Count Trailing Zeroes in x.
     */
    __device__ __forceinline__
    int
    ctz(u32 x) {
        int n;
        asm ("{\n\t"
             " .reg .u32 tmp;\n\t"
             " brev.b32 tmp, %1;\n\t"
             " clz.b32 %0, tmp;\n\t"
             "}"
             : "=r"(n) : "r"(x));
        return n;
    }

    __device__ __forceinline__
    int
    ctz(u64 x) {
        int n;
        asm ("{\n\t"
             " .reg .u64 tmp;\n\t"
             " brev.b64 tmp, %1;\n\t"
             " clz.b64 %0, tmp;\n\t"
             "}"
             : "=r"(n) : "l"(x));
        return n;
    }

    __device__ __forceinline__
    void
    min(u32 &m, u32 a, u32 b) {
        asm ("min.u32 %0, %1, %2;" : "=r"(m) : "r"(a), "r"(b));
    }

    __device__ __forceinline__
    void
    min(u64 &m, u64 a, u64 b) {
        asm ("min.u64 %0, %1, %2;" : "=l"(m) : "l"(a), "l"(b));
    }

    __device__ __forceinline__
    void
    max(u32 &m, u32 a, u32 b) {
        asm ("max.u32 %0, %1, %2;" : "=r"(m) : "r"(a), "r"(b));
    }

    __device__ __forceinline__
    void
    max(u64 &m, u64 a, u64 b) {
        asm ("max.u64 %0, %1, %2;" : "=l"(m) : "l"(a), "l"(b));
    }

    __device__ __forceinline__
    void
    modinv_2exp(u32 &x, u32 b) {
        assert(b & 1);

        x = (2U - b * b) * b;
        x *= 2U - b * x;
        x *= 2U - b * x;
        x *= 2U - b * x;
    }

    __device__ __forceinline__
    void
    modinv_2exp(u64 &x, u64 b) {
        assert(b & 1);

        x = (2UL - b * b) * b;
        x *= 2UL - b * x;
        x *= 2UL - b * x;
        x *= 2UL - b * x;
        x *= 2UL - b * x;
    }

    /*
     * For 512 <= d < 1024,
     *
     *   RECIPROCAL_TABLE_32[d - 512] = floor((2^24 - 2^14 + 2^9)/d)
     *
     * Total space at the moment is 512*2 = 1024 bytes.
     *
     * TODO: Investigate whether alternative storage layouts are better; examples:
     *
     *  - redundantly store each element in a uint32_t
     *  - pack two uint16_t values into each uint32_t
     *  - is __constant__ the right storage specifier? Maybe load into shared memory?
     *    Shared memory seems like an excellent choice (48k available per SM), though
     *    I'll need to be mindful of bank conflicts (perhaps circumvent by having
     *    many copies of the data in SM?).
     *  - perhaps reading an element from memory is slower than simply calculating
     *    floor((2^24 - 2^14 + 2^9)/d) in assembly?
     */
    __device__ __constant__
    uint16_t
    RECIPROCAL_TABLE_32[0x200] =
    {
        0x7fe1, 0x7fa1, 0x7f61, 0x7f22, 0x7ee3, 0x7ea4, 0x7e65, 0x7e27,
        0x7de9, 0x7dab, 0x7d6d, 0x7d30, 0x7cf3, 0x7cb6, 0x7c79, 0x7c3d,
        0x7c00, 0x7bc4, 0x7b89, 0x7b4d, 0x7b12, 0x7ad7, 0x7a9c, 0x7a61,
        0x7a27, 0x79ec, 0x79b2, 0x7979, 0x793f, 0x7906, 0x78cc, 0x7894,
        0x785b, 0x7822, 0x77ea, 0x77b2, 0x777a, 0x7742, 0x770b, 0x76d3,
        0x769c, 0x7665, 0x762f, 0x75f8, 0x75c2, 0x758c, 0x7556, 0x7520,
        0x74ea, 0x74b5, 0x7480, 0x744b, 0x7416, 0x73e2, 0x73ad, 0x7379,
        0x7345, 0x7311, 0x72dd, 0x72aa, 0x7277, 0x7243, 0x7210, 0x71de,
        0x71ab, 0x7179, 0x7146, 0x7114, 0x70e2, 0x70b1, 0x707f, 0x704e,
        0x701c, 0x6feb, 0x6fba, 0x6f8a, 0x6f59, 0x6f29, 0x6ef9, 0x6ec8,
        0x6e99, 0x6e69, 0x6e39, 0x6e0a, 0x6ddb, 0x6dab, 0x6d7d, 0x6d4e,
        0x6d1f, 0x6cf1, 0x6cc2, 0x6c94, 0x6c66, 0x6c38, 0x6c0a, 0x6bdd,
        0x6bb0, 0x6b82, 0x6b55, 0x6b28, 0x6afb, 0x6acf, 0x6aa2, 0x6a76,
        0x6a49, 0x6a1d, 0x69f1, 0x69c6, 0x699a, 0x696e, 0x6943, 0x6918,
        0x68ed, 0x68c2, 0x6897, 0x686c, 0x6842, 0x6817, 0x67ed, 0x67c3,
        0x6799, 0x676f, 0x6745, 0x671b, 0x66f2, 0x66c8, 0x669f, 0x6676,
        0x664d, 0x6624, 0x65fc, 0x65d3, 0x65aa, 0x6582, 0x655a, 0x6532,
        0x650a, 0x64e2, 0x64ba, 0x6493, 0x646b, 0x6444, 0x641c, 0x63f5,
        0x63ce, 0x63a7, 0x6381, 0x635a, 0x6333, 0x630d, 0x62e7, 0x62c1,
        0x629a, 0x6275, 0x624f, 0x6229, 0x6203, 0x61de, 0x61b8, 0x6193,
        0x616e, 0x6149, 0x6124, 0x60ff, 0x60da, 0x60b6, 0x6091, 0x606d,
        0x6049, 0x6024, 0x6000, 0x5fdc, 0x5fb8, 0x5f95, 0x5f71, 0x5f4d,
        0x5f2a, 0x5f07, 0x5ee3, 0x5ec0, 0x5e9d, 0x5e7a, 0x5e57, 0x5e35,
        0x5e12, 0x5def, 0x5dcd, 0x5dab, 0x5d88, 0x5d66, 0x5d44, 0x5d22,
        0x5d00, 0x5cde, 0x5cbd, 0x5c9b, 0x5c7a, 0x5c58, 0x5c37, 0x5c16,
        0x5bf5, 0x5bd4, 0x5bb3, 0x5b92, 0x5b71, 0x5b51, 0x5b30, 0x5b10,
        0x5aef, 0x5acf, 0x5aaf, 0x5a8f, 0x5a6f, 0x5a4f, 0x5a2f, 0x5a0f,
        0x59ef, 0x59d0, 0x59b0, 0x5991, 0x5972, 0x5952, 0x5933, 0x5914,
        0x58f5, 0x58d6, 0x58b7, 0x5899, 0x587a, 0x585b, 0x583d, 0x581f,
        0x5800, 0x57e2, 0x57c4, 0x57a6, 0x5788, 0x576a, 0x574c, 0x572e,
        0x5711, 0x56f3, 0x56d5, 0x56b8, 0x569b, 0x567d, 0x5660, 0x5643,
        0x5626, 0x5609, 0x55ec, 0x55cf, 0x55b2, 0x5596, 0x5579, 0x555d,
        0x5540, 0x5524, 0x5507, 0x54eb, 0x54cf, 0x54b3, 0x5497, 0x547b,
        0x545f, 0x5443, 0x5428, 0x540c, 0x53f0, 0x53d5, 0x53b9, 0x539e,
        0x5383, 0x5368, 0x534c, 0x5331, 0x5316, 0x52fb, 0x52e0, 0x52c6,
        0x52ab, 0x5290, 0x5276, 0x525b, 0x5240, 0x5226, 0x520c, 0x51f1,
        0x51d7, 0x51bd, 0x51a3, 0x5189, 0x516f, 0x5155, 0x513b, 0x5121,
        0x5108, 0x50ee, 0x50d5, 0x50bb, 0x50a2, 0x5088, 0x506f, 0x5056,
        0x503c, 0x5023, 0x500a, 0x4ff1, 0x4fd8, 0x4fbf, 0x4fa6, 0x4f8e,
        0x4f75, 0x4f5c, 0x4f44, 0x4f2b, 0x4f13, 0x4efa, 0x4ee2, 0x4eca,
        0x4eb1, 0x4e99, 0x4e81, 0x4e69, 0x4e51, 0x4e39, 0x4e21, 0x4e09,
        0x4df1, 0x4dda, 0x4dc2, 0x4daa, 0x4d93, 0x4d7b, 0x4d64, 0x4d4d,
        0x4d35, 0x4d1e, 0x4d07, 0x4cf0, 0x4cd8, 0x4cc1, 0x4caa, 0x4c93,
        0x4c7d, 0x4c66, 0x4c4f, 0x4c38, 0x4c21, 0x4c0b, 0x4bf4, 0x4bde,
        0x4bc7, 0x4bb1, 0x4b9a, 0x4b84, 0x4b6e, 0x4b58, 0x4b41, 0x4b2b,
        0x4b15, 0x4aff, 0x4ae9, 0x4ad3, 0x4abd, 0x4aa8, 0x4a92, 0x4a7c,
        0x4a66, 0x4a51, 0x4a3b, 0x4a26, 0x4a10, 0x49fb, 0x49e5, 0x49d0,
        0x49bb, 0x49a6, 0x4990, 0x497b, 0x4966, 0x4951, 0x493c, 0x4927,
        0x4912, 0x48fe, 0x48e9, 0x48d4, 0x48bf, 0x48ab, 0x4896, 0x4881,
        0x486d, 0x4858, 0x4844, 0x482f, 0x481b, 0x4807, 0x47f3, 0x47de,
        0x47ca, 0x47b6, 0x47a2, 0x478e, 0x477a, 0x4766, 0x4752, 0x473e,
        0x472a, 0x4717, 0x4703, 0x46ef, 0x46db, 0x46c8, 0x46b4, 0x46a1,
        0x468d, 0x467a, 0x4666, 0x4653, 0x4640, 0x462c, 0x4619, 0x4606,
        0x45f3, 0x45e0, 0x45cd, 0x45ba, 0x45a7, 0x4594, 0x4581, 0x456e,
        0x455b, 0x4548, 0x4536, 0x4523, 0x4510, 0x44fe, 0x44eb, 0x44d8,
        0x44c6, 0x44b3, 0x44a1, 0x448f, 0x447c, 0x446a, 0x4458, 0x4445,
        0x4433, 0x4421, 0x440f, 0x43fd, 0x43eb, 0x43d9, 0x43c7, 0x43b5,
        0x43a3, 0x4391, 0x437f, 0x436d, 0x435c, 0x434a, 0x4338, 0x4327,
        0x4315, 0x4303, 0x42f2, 0x42e0, 0x42cf, 0x42bd, 0x42ac, 0x429b,
        0x4289, 0x4278, 0x4267, 0x4256, 0x4244, 0x4233, 0x4222, 0x4211,
        0x4200, 0x41ef, 0x41de, 0x41cd, 0x41bc, 0x41ab, 0x419a, 0x418a,
        0x4179, 0x4168, 0x4157, 0x4147, 0x4136, 0x4125, 0x4115, 0x4104,
        0x40f4, 0x40e3, 0x40d3, 0x40c2, 0x40b2, 0x40a2, 0x4091, 0x4081,
        0x4071, 0x4061, 0x4050, 0x4040, 0x4030, 0x4020, 0x4010, 0x4000
    };

    __device__ __forceinline__
    uint32_t
    lookup_reciprocal(uint32_t d10) {
        assert((d10 >> 9) == 1);
        return RECIPROCAL_TABLE_32[d10 - 0x200];
    }


    /*
     * Source: Niels Möller and Torbjörn Granlund, “Improved division by
     * invariant integers”, IEEE Transactions on Computers, 11 June
     * 2010. https://gmplib.org/~tege/division-paper.pdf
     */
    __device__ __forceinline__
    uint32_t
    quorem_reciprocal(uint32_t d)
    {
        // Top bit must be set, i.e. d must be already normalised.
        assert((d >> 31) == 1);

        uint32_t d0_mask, d10, d21, d31, v0, v1, v2, v3, e, t0, t1;

        d0_mask = -(uint32_t)(d & 1); // 0 if d&1=0, 0xFF..FF if d&1=1.
        d10 = d >> 22;
        d21 = (d >> 11) + 1;
        d31 = d - (d >> 1);  // ceil(d/2) = d - floor(d/2)

        v0 = lookup_reciprocal(d10); // 15 bits
        mul_hi(t0, v0 * v0, d21);
        v1 = (v0 << 4) - t0 - 1;   // 18 bits
        e = -(v1 * d31) + ((v1 >> 1) & d0_mask);
        mul_hi(t0, v1, e);
        v2 = (v1 << 15) + (t0 >> 1); // 33 bits (hi bit is implicit)
        mul_wide(t1, t0, v2, d);
        t1 += d + ((t0 + d) < d);
        v3 = v2 - t1; // 33 bits (hi bit is implicit)
        return v3;
    }

    /*
     * For 256 <= d < 512,
     *
     *   RECIPROCAL_TABLE_64[d - 256] = floor((2^19 - 3*2^9)/d)
     *
     * Total space ATM is 256*2 = 512 bytes. Entries range from 10 to 11
     * bits, so with some clever handling of hi bits, we could get three
     * entries per 32 bit word, reducing the size to about 256*11/8 = 352
     * bytes.
     *
     * TODO: Investigate whether alternative storage layouts are better;
     * see RECIPROCAL_TABLE_32 above for ideas.
     */
    __device__ __constant__
    uint16_t
    RECIPROCAL_TABLE_64[0x100] =
    {
        0x7fd, 0x7f5, 0x7ed, 0x7e5, 0x7dd, 0x7d5, 0x7ce, 0x7c6,
        0x7bf, 0x7b7, 0x7b0, 0x7a8, 0x7a1, 0x79a, 0x792, 0x78b,
        0x784, 0x77d, 0x776, 0x76f, 0x768, 0x761, 0x75b, 0x754,
        0x74d, 0x747, 0x740, 0x739, 0x733, 0x72c, 0x726, 0x720,
        0x719, 0x713, 0x70d, 0x707, 0x700, 0x6fa, 0x6f4, 0x6ee,
        0x6e8, 0x6e2, 0x6dc, 0x6d6, 0x6d1, 0x6cb, 0x6c5, 0x6bf,
        0x6ba, 0x6b4, 0x6ae, 0x6a9, 0x6a3, 0x69e, 0x698, 0x693,
        0x68d, 0x688, 0x683, 0x67d, 0x678, 0x673, 0x66e, 0x669,
        0x664, 0x65e, 0x659, 0x654, 0x64f, 0x64a, 0x645, 0x640,
        0x63c, 0x637, 0x632, 0x62d, 0x628, 0x624, 0x61f, 0x61a,
        0x616, 0x611, 0x60c, 0x608, 0x603, 0x5ff, 0x5fa, 0x5f6,
        0x5f1, 0x5ed, 0x5e9, 0x5e4, 0x5e0, 0x5dc, 0x5d7, 0x5d3,
        0x5cf, 0x5cb, 0x5c6, 0x5c2, 0x5be, 0x5ba, 0x5b6, 0x5b2,
        0x5ae, 0x5aa, 0x5a6, 0x5a2, 0x59e, 0x59a, 0x596, 0x592,
        0x58e, 0x58a, 0x586, 0x583, 0x57f, 0x57b, 0x577, 0x574,
        0x570, 0x56c, 0x568, 0x565, 0x561, 0x55e, 0x55a, 0x556,
        0x553, 0x54f, 0x54c, 0x548, 0x545, 0x541, 0x53e, 0x53a,
        0x537, 0x534, 0x530, 0x52d, 0x52a, 0x526, 0x523, 0x520,
        0x51c, 0x519, 0x516, 0x513, 0x50f, 0x50c, 0x509, 0x506,
        0x503, 0x500, 0x4fc, 0x4f9, 0x4f6, 0x4f3, 0x4f0, 0x4ed,
        0x4ea, 0x4e7, 0x4e4, 0x4e1, 0x4de, 0x4db, 0x4d8, 0x4d5,
        0x4d2, 0x4cf, 0x4cc, 0x4ca, 0x4c7, 0x4c4, 0x4c1, 0x4be,
        0x4bb, 0x4b9, 0x4b6, 0x4b3, 0x4b0, 0x4ad, 0x4ab, 0x4a8,
        0x4a5, 0x4a3, 0x4a0, 0x49d, 0x49b, 0x498, 0x495, 0x493,
        0x490, 0x48d, 0x48b, 0x488, 0x486, 0x483, 0x481, 0x47e,
        0x47c, 0x479, 0x477, 0x474, 0x472, 0x46f, 0x46d, 0x46a,
        0x468, 0x465, 0x463, 0x461, 0x45e, 0x45c, 0x459, 0x457,
        0x455, 0x452, 0x450, 0x44e, 0x44b, 0x449, 0x447, 0x444,
        0x442, 0x440, 0x43e, 0x43b, 0x439, 0x437, 0x435, 0x432,
        0x430, 0x42e, 0x42c, 0x42a, 0x428, 0x425, 0x423, 0x421,
        0x41f, 0x41d, 0x41b, 0x419, 0x417, 0x414, 0x412, 0x410,
        0x40e, 0x40c, 0x40a, 0x408, 0x406, 0x404, 0x402, 0x400
    };

    __device__ __forceinline__
    uint64_t
    lookup_reciprocal(uint64_t d9) {
        assert((d9 >> 8) == 1);
        return RECIPROCAL_TABLE_64[d9 - 0x100];
    }

    /*
     * Source: Niels Möller and Torbjörn Granlund, “Improved division by
     * invariant integers”, IEEE Transactions on Computers, 11 June
     * 2010. https://gmplib.org/~tege/division-paper.pdf
     */
    __device__ __forceinline__
    uint64_t
    quorem_reciprocal(uint64_t d)
    {
        // Top bit must be set, i.e. d must be already normalised.
        assert((d >> 63) == 1);

        uint64_t d0_mask, d9, d40, d63, v0, v1, v2, v3, v4, e, t0, t1;

        d0_mask = -(uint64_t)(d & 1); // 0 if d&1=0, 0xFF..FF if d&1=1.
        d9 = d >> 55;
        d40 = (d >> 24) + 1;
        d63 = d - (d >> 1);  // ceil(d/2) = d - floor(d/2)

        v0 = lookup_reciprocal(d9); // 11 bits
        t0 = v0 * v0 * d40;
        v1 = (v0 << 11) - (t0 >> 40) - 1;   // 21 bits
        t0 = v1 * ((1UL << 60) - (v1 * d40));
        v2 = (v1 << 13) + (t0 >> 47); // 34 bits

        e = -(v2 * d63) + ((v1 >> 1) & d0_mask);
        mul_hi(t0, v2, e);
        v3 = (v2 << 31) + (t0 >> 1);  // 65 bits (hi bit is implicit)
        mul_wide(t1, t0, v3, d);
        t1 += d + ((t0 + d) < d);
        v4 = v3 - t1;   // 65 bits (hi bit is implicit)
        return v4;
    }


    template< typename uint_tp >
    __device__ __forceinline__
    int
    quorem_normalise_divisor(uint_tp &d) {
        int cnt = clz(d);
        d <<= cnt;
        return cnt;
    }

    template< typename uint_tp >
    __device__ __forceinline__
    uint_tp
    quorem_normalise_dividend(uint_tp &u_hi, uint_tp &u_lo, int cnt) {
        // TODO: For 32 bit operands we can just do the following
        // asm ("shf.l.clamp.b32 %0, %1, %0, %2;\n\t"
        //      "shl.b32 %1, %1, %2;"
        //     : "+r"(u_hi), "+r"(u_lo) : "r"(cnt));
        //
        // For 64 bits it's a bit more long-winded
        // Inspired by https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#logic-and-shift-instructions-shf
        // asm ("{\n\t"
        //     " .reg .u32 t1;\n\t"
        //     " .reg .u32 t2;\n\t"
        //     " .reg .u32 t3;\n\t"
        //     " .reg .u32 t4;\n\t"
        //     " mov.b64 { t1, t2 }, %0;\n\t"
        //     " mov.b64 { t3, t4 }, %1;\n\t"
        //     " shf.l.clamp.b32 t4, t3, t4, %2;\n\t"
        //     " shf.l.clamp.b32 t3, t2, t3, %2;\n\t"
        //     " shf.l.clamp.b32 t2, t1, t2, %2;\n\t"
        //     " shl.b32 t1, t1, %2;\n\t"
        //     " mov.b64 %0, { t1, t2 };\n\t"
        //     " mov.b64 %1, { t3, t4 };\n\t"
        //     "}"
        //     : "+l"(u_lo), "+l"(u_hi) : "r"(cnt));

        static constexpr int WORD_BITS = sizeof(uint_tp) * 8;
        uint_tp overflow = u_hi >> (WORD_BITS - cnt);
        uint_tp u_hi_lsb = u_lo >> (WORD_BITS - cnt);
    #ifndef __CUDA_ARCH__
        // Compensate for the fact that, unlike CUDA, shifts by WORD_BITS
        // are undefined in C.
        // u_hi_lsb = 0 if cnt=0 or u_hi_lsb if cnt!=0.
        u_hi_lsb &= -(uint_tp)!!cnt;
        overflow &= -(uint_tp)!!cnt;
    #endif
        u_hi = (u_hi << cnt) | u_hi_lsb;
        u_lo <<= cnt;
        return overflow;
    }

    /*
     * Suppose Q and r satisfy U = Qd + r, where Q = (q_hi, q_lo) and U =
     * (u_hi, u_lo) are two-word numbers. This function returns q = min(Q,
     * 2^WORD_BITS - 1) and r = U - Qd if q = Q or r = q in the latter
     * case.  v should be set to quorem_reciprocal(d).
     *
     * CAVEAT EMPTOR: d and {u_hi, u_lo} need to be normalised (using the
     * functions provided) PRIOR to being passed to this
     * function. Similarly, the resulting remainder r (but NOT the
     * quotient q) needs to be denormalised (i.e. right shift by the
     * normalisation factor) after receipt.
     *
     * Source: Niels Möller and Torbjörn Granlund, “Improved division by
     * invariant integers”, IEEE Transactions on Computers, 11 June
     * 2010. https://gmplib.org/~tege/division-paper.pdf
     */
    template< typename uint_tp >
    __device__
    void
    quorem_wide_normalised(
        uint_tp &q, uint_tp &r,
        uint_tp u_hi, uint_tp u_lo, uint_tp d, uint_tp v)
    {
        static_assert(std::is_unsigned<uint_tp>::value == true,
                    "template type must be unsigned");
        if (u_hi > d) {
            q = r = (uint_tp)-1;
            return;
        }

        uint_tp q_hi, q_lo, mask;

        mul_wide(q_hi, q_lo, u_hi, v);
        q_lo += u_lo;
        q_hi += u_hi + (q_lo < u_lo) + 1;
        r = u_lo - q_hi * d;

        // Branch is unpredicable
        //if (r > q_lo) { --q_hi; r += d; }
        mask = -(uint_tp)(r > q_lo);
        q_hi += mask;
        r += mask & d;

        // Branch is very unlikely to be taken
        if (r >= d) { r -= d; ++q_hi; }
        //mask = -(uint_tp)(r >= d);
        //q_hi -= mask;
        //r -= mask & d;

        q = q_hi;
    }

    /*
     * As above, but calculate, then return, the precomputed inverse for d.
     * Normalisation of the divisor and dividend is performed then thrown away.
     */
    template< typename uint_tp >
    __device__ __forceinline__
    uint_tp
    quorem_wide(
        uint_tp &q, uint_tp &r,
        uint_tp u_hi, uint_tp u_lo, uint_tp d)
    {
        static_assert(std::is_unsigned<uint_tp>::value == true,
                    "template type must be unsigned");
        int lz = quorem_normalise_divisor(d);
        uint_tp overflow = quorem_normalise_dividend(u_hi, u_lo, lz);
        uint_tp v = quorem_reciprocal(d);
        if (overflow) { q = r = (uint_tp)-1; return v; }
        quorem_wide_normalised(q, r, u_hi, u_lo, d, v);
        assert((r & (((uint_tp)1 << lz) - 1U)) == 0);
        r >>= lz;
        return v;
    }

    /*
     * As above, but uses a given precomputed inverse. If the precomputed
     * inverse comes from quorem_reciprocal() rather than from quorem_wide()
     * above, then make sure the divisor given to quorem_reciprocal() was
     * normalised with quorem_normalise_divisor() first.
     */
    template< typename uint_tp >
    __device__ __forceinline__
    void
    quorem_wide(
        uint_tp &q, uint_tp &r,
        uint_tp u_hi, uint_tp u_lo, uint_tp d, uint_tp v)
    {
        static_assert(std::is_unsigned<uint_tp>::value == true,
                      "template type must be unsigned");
        int lz = quorem_normalise_divisor(d);
        uint_tp overflow = quorem_normalise_dividend(u_hi, u_lo, lz);
        if (overflow) { q = r = -(uint_tp)1; }
        quorem_wide_normalised(q, r, u_hi, u_lo, d, v);
        assert((r & (((uint_tp)1 << lz) - 1U)) == 0);
        r >>= lz;
    }

    /*
     * ceiling(n / d)
     */
    template< typename T >
    __device__ __forceinline__
    void
    ceilquo(T &q, T n, T d) {
        q = (n + d - 1) / d;
    }

} // End namespace internal

} // End namespace cuFIXNUM
