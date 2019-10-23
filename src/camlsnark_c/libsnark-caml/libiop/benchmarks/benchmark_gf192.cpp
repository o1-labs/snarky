#include <cmath>
#include <cstddef>
#include <vector>
#include <benchmark/benchmark.h>

#include "libiop/algebra/fields/gf192.hpp"
#include "libiop/algebra/utils.hpp"

namespace libiop {

static void BM_gf192_mul_vec(benchmark::State &state)
{
    const size_t sz = state.range(0);
    const std::vector<gf192> avec = random_vector<gf192>(sz);
    const std::vector<gf192> bvec = random_vector<gf192>(sz);

    std::vector<gf192> cvec(sz);

    for (auto _ : state)
    {
        for (size_t i = 0; i < sz; ++i)
        {
            cvec[i] = avec[i] * bvec[i];
        }
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_gf192_mul_vec)->Range(1<<10, 1<<20)->Unit(benchmark::kMicrosecond);

static void BM_gf192_inverse_vec(benchmark::State& state)
{
    const size_t sz = state.range(0);
    const std::vector<gf192> vec = random_vector<gf192>(sz);

    std::vector<gf192> result(sz);

    for (auto _ : state)
    {
        for (size_t i = 0; i < sz; ++i)
        {
            result[i] = vec[i].inverse();
        }
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_gf192_inverse_vec)->Range(1<<10, 1<<16)->Unit(benchmark::kMicrosecond);

}

BENCHMARK_MAIN();
