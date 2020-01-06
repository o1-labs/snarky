#include <cstdio>
#include <cstring>
#include <cassert>

#include "fixnum/warp_fixnum.cu"
#include "array/fixnum_array.h"
#include "functions/modexp.cu"
#include "functions/multi_modexp.cu"
#include "modnum/modnum_monty_redc.cu"
#include "modnum/modnum_monty_cios.cu"

using namespace std;
using namespace cuFIXNUM;

template< typename fixnum >
struct mul_lo {
    __device__ void operator()(fixnum &r, fixnum a) {
        fixnum s;
        fixnum::mul_lo(s, a, a);
        r = s;
    }
};

template< typename fixnum >
struct mul_wide {
    __device__ void operator()(fixnum &r, fixnum a) {
        fixnum rr, ss;
        fixnum::mul_wide(ss, rr, a, a);
        r = ss;
    }
};

template< typename fixnum >
struct sqr_wide {
    __device__ void operator()(fixnum &r, fixnum a) {
        fixnum rr, ss;
        fixnum::sqr_wide(ss, rr, a);
        r = ss;
    }
};

template< typename modnum >
struct my_modexp {
    typedef typename modnum::fixnum fixnum;

    __device__ void operator()(fixnum &z, fixnum x) {
        modexp<modnum> me(x, x);
        fixnum zz;
        me(zz, x);
        z = zz;
    };
};

template< typename modnum >
struct my_multi_modexp {
    typedef typename modnum::fixnum fixnum;

    __device__ void operator()(fixnum &z, fixnum x) {
        multi_modexp<modnum> mme(x);
        fixnum zz;
        mme(zz, x, x);
        z = zz;
    };
};

template< int fn_bytes, typename word_fixnum, template <typename> class Func >
void bench(int nelts) {
    typedef warp_fixnum<fn_bytes, word_fixnum> fixnum;
    typedef fixnum_array<fixnum> fixnum_array;

    if (nelts == 0) {
        puts(" -*-  nelts == 0; skipping...  -*-");
        return;
    }

    uint8_t *input = new uint8_t[fn_bytes * nelts];
    for (int i = 0; i < fn_bytes * nelts; ++i)
        input[i] = (i * 17 + 11) % 256;

    fixnum_array *res, *in;
    in = fixnum_array::create(input, fn_bytes * nelts, fn_bytes);
    res = fixnum_array::create(nelts);

    // warm up
    fixnum_array::template map<Func>(res, in);

    clock_t c = clock();
    fixnum_array::template map<Func>(res, in);
    c = clock() - c;

    double secinv = (double)CLOCKS_PER_SEC / c;
    double total_MiB = fixnum::BYTES * (double)nelts / (1 << 20);
    printf(" %4d   %3d    %6.1f   %7.3f  %12.1f\n",
           fixnum::BITS, fixnum::digit::BITS, total_MiB,
           1/secinv, nelts * 1e-3 * secinv);

    delete in;
    delete res;
    delete[] input;
}

template< template <typename> class Func >
void bench_func(const char *fn_name, int nelts) {
    printf("Function: %s, #elts: %de3\n", fn_name, (int)(nelts * 1e-3));
    printf("fixnum digit  total data   time       Kops/s\n");
    printf(" bits  bits     (MiB)    (seconds)\n");
    bench<4, u32_fixnum, Func>(nelts);
    bench<8, u32_fixnum, Func>(nelts);
    bench<16, u32_fixnum, Func>(nelts);
    bench<32, u32_fixnum, Func>(nelts);
    bench<64, u32_fixnum, Func>(nelts);
    bench<128, u32_fixnum, Func>(nelts);
    puts("");

    bench<8, u64_fixnum, Func>(nelts);
    bench<16, u64_fixnum, Func>(nelts);
    bench<32, u64_fixnum, Func>(nelts);
    bench<64, u64_fixnum, Func>(nelts);
    bench<128, u64_fixnum, Func>(nelts);
    bench<256, u64_fixnum, Func>(nelts);
    puts("");
}

template< typename fixnum >
using modexp_redc = my_modexp< modnum_monty_redc<fixnum> >;

template< typename fixnum >
using modexp_cios = my_modexp< modnum_monty_cios<fixnum> >;

template< typename fixnum >
using multi_modexp_redc = my_multi_modexp< modnum_monty_redc<fixnum> >;

template< typename fixnum >
using multi_modexp_cios = my_multi_modexp< modnum_monty_cios<fixnum> >;

int main(int argc, char *argv[]) {
    long m = 1;
    if (argc > 1)
        m = atol(argv[1]);
    m = std::max(m, 1000L);

    bench_func<mul_lo>("mul_lo", m);
    puts("");
    bench_func<mul_wide>("mul_wide", m);
    puts("");
    bench_func<sqr_wide>("sqr_wide", m);
    puts("");
    bench_func<modexp_redc>("modexp redc", m / 100);
    puts("");
    bench_func<modexp_cios>("modexp cios", m / 100);
    puts("");

    bench_func<modexp_redc>("multi modexp redc", m / 100);
    puts("");
    bench_func<modexp_cios>("multi modexp cios", m / 100);
    puts("");

    return 0;
}
