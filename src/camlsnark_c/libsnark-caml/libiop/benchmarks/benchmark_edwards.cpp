#include <vector>
#include <benchmark/benchmark.h>

#include "libiop/algebra/fields/utils.hpp"
#include "libiop/common/common.hpp"
#include "libiop/algebra/utils.hpp"
#include <libff/algebra/curves/edwards/edwards_pp.hpp>

namespace libiop {

static void BM_edwards_mul_vec(benchmark::State &state)
{
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;
    const size_t sz = state.range(0);
    const std::vector<FieldT> avec = random_vector<FieldT>(sz);
    const std::vector<FieldT> bvec = random_vector<FieldT>(sz);

    std::vector<FieldT> cvec(sz);

    for (auto _ : state)
    {
        for (size_t i = 0; i < sz; ++i)
        {
            cvec[i] = avec[i] * bvec[i];
        }
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_edwards_mul_vec)->Range(1<<10, 1<<20)->Unit(benchmark::kMicrosecond);

static void BM_edwards_mul_vec_data_dependency(benchmark::State &state)
{
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;
    const size_t sz = state.range(0);
    const std::vector<FieldT> avec = random_vector<FieldT>(sz);
    const std::vector<FieldT> bvec = random_vector<FieldT>(sz);

    FieldT sum = FieldT::zero();

    for (auto _ : state)
    {
        for (size_t i = 0; i < sz; ++i)
        {
            sum += avec[i] * bvec[i];
        }
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_edwards_mul_vec_data_dependency)->Range(1<<10, 1<<20)->Unit(benchmark::kMicrosecond);

static void BM_edwards_inverse_vec(benchmark::State& state)
{
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;
    const size_t sz = state.range(0);
    const std::vector<FieldT> vec = random_vector<FieldT>(sz);

    std::vector<FieldT> result(sz);

    for (auto _ : state)
    {
        for (size_t i = 0; i < sz; ++i)
        {
            result[i] = vec[i].inverse();
        }
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_edwards_inverse_vec)->Range(1<<10, 1<<16)->Unit(benchmark::kMicrosecond);

}

BENCHMARK_MAIN();
