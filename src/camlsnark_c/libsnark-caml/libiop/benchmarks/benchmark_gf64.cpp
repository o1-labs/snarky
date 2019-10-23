#include <cmath>
#include <cstddef>
#include <vector>
#include <benchmark/benchmark.h>

#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/utils.hpp"

namespace libiop {

static void BM_gf64_mul_vec(benchmark::State &state)
{
    const size_t sz = state.range(0);
    const std::vector<gf64> avec = random_vector<gf64>(sz);
    const std::vector<gf64> bvec = random_vector<gf64>(sz);

    std::vector<gf64> cvec(sz);

    for (auto _ : state)
    {
        for (size_t i = 0; i < sz; ++i)
        {
            cvec[i] = avec[i] * bvec[i];
        }
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_gf64_mul_vec)->Range(1<<10, 1<<20)->Unit(benchmark::kMicrosecond);

static void BM_gf64_mul_vec_data_dependency(benchmark::State &state)
{
    const size_t sz = state.range(0);
    const std::vector<gf64> avec = random_vector<gf64>(sz);
    const std::vector<gf64> bvec = random_vector<gf64>(sz);

    gf64 sum = gf64(0);

    for (auto _ : state)
    {
        for (size_t i = 0; i < sz; ++i)
        {
            sum += avec[i] * bvec[i];
        }
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_gf64_mul_vec_data_dependency)->Range(1<<10, 1<<20)->Unit(benchmark::kMicrosecond);

// The goal of this is to measure how long N multiplications take, with no cache misses
static void BM_gf64_mul(benchmark::State &state)
{
    // Update per what google benchmark claims the L1 cache size for this system is.
    const size_t L1_cache_size = 1ull << 15; // 32 KB
    // divides by 4, since it uses avec, and bvec, and do not optimize will force flushes to memory
    const size_t num_elems = (L1_cache_size / (4 * sizeof(gf64)));

    const size_t sz = state.range(0);
    const std::vector<gf64> avec = random_vector<gf64>(num_elems);
    std::vector<gf64> bvec = random_vector<gf64>(num_elems);

    const size_t num_iters = sz / num_elems; // may be off by 1 iteration, but that should be negligible

    for (auto _ : state)
    {
        for (size_t i = 0; i < num_iters; ++i)
        {
            for (size_t j = 0; j < num_elems; j++) {
                bvec[j] = avec[j] * bvec[j];
            }
        }
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_gf64_mul)->Range(1<<20, 1<<28)->Unit(benchmark::kMicrosecond);

// The goal of this is to measure how long N *= ops take, with no cache misses
static void BM_gf64_mul_equals(benchmark::State &state)
{
    // Update per what google benchmark claims the L1 cache size for this system is.
    const size_t L1_cache_size = 1ull << 15; // 32 KB
    // divides by 4, since it uses avec, and bvec, and do not optimize will force flushes to memory
    const size_t num_elems = (L1_cache_size / (4 * sizeof(gf64)));

    const size_t sz = state.range(0);
    std::vector<gf64> avec = random_vector<gf64>(num_elems);
    const std::vector<gf64> bvec = random_vector<gf64>(num_elems);

    const size_t num_iters = sz / num_elems; // may be off by 1 iteration, but that should be negligible

    for (auto _ : state)
    {
        for (size_t i = 0; i < num_iters; ++i)
        {
            for (size_t j = 0; j < num_elems; j++) {
                avec[j] *= bvec[j];
            }
        }
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_gf64_mul_equals)->Range(1<<20, 1<<28)->Unit(benchmark::kMicrosecond);

static void BM_gf64_inverse_vec(benchmark::State& state)
{
    const size_t sz = state.range(0);
    const std::vector<gf64> vec = random_vector<gf64>(sz);

    std::vector<gf64> result(sz);

    for (auto _ : state)
    {
        for (size_t i = 0; i < sz; ++i)
        {
            result[i] = vec[i].inverse();
        }
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_gf64_inverse_vec)->Range(1<<10, 1<<16)->Unit(benchmark::kMicrosecond);

}

BENCHMARK_MAIN();
