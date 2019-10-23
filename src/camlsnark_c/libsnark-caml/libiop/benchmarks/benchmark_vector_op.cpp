#include <cmath>
#include <cstddef>
#include <vector>
#include <benchmark/benchmark.h>

#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/utils.hpp"
#include "libiop/common/common.hpp"

namespace libiop {

static void BM_all_gf64_subset_sums(benchmark::State &state)
{
    const size_t sz = state.range(0);
    const size_t log_sz = log2(sz);

    const std::vector<gf64> basis = random_vector<gf64>(log_sz);
    const gf64 shift = gf64::random_element();

    for (auto _ : state)
    {
        all_subset_sums<gf64>(basis, shift);
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_all_gf64_subset_sums)->Range(1ull<<4, 1ull<<20)->Unit(benchmark::kMicrosecond);

static void BM_random_gf64_vector(benchmark::State &state)
{
    const size_t sz = state.range(0);

    for (auto _ : state)
    {
        const std::vector<gf64> vec = random_vector<gf64>(sz);
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_random_gf64_vector)->Range(1ull<<4, 1ull<<20)->Unit(benchmark::kMicrosecond);

}

BENCHMARK_MAIN();
