#include <vector>
#include <benchmark/benchmark.h>

#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/lagrange.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/common/common.hpp"
#include <libff/algebra/curves/edwards/edwards_pp.hpp>

namespace libiop {

static void BM_lagrange_additive(benchmark::State &state)
{
    typedef gf64 FieldT;

    const size_t sz = state.range(0);
    const size_t log_sz = log2(sz);

    const affine_subspace<FieldT> domain =
        affine_subspace<FieldT>::random_affine_subspace(log_sz);
    const FieldT interpolation_point = FieldT::random_element();

    for (auto _ : state)
    {
        const std::vector<FieldT> result = lagrange_coefficients<FieldT>(
            domain, interpolation_point);
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_lagrange_additive)->Range(1ull<<4, 1ull<<24)->Unit(benchmark::kMicrosecond);

static void BM_lagrange_multiplicative(benchmark::State &state)
{
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;

    const size_t sz = state.range(0);
    const size_t log_sz = log2(sz);

    const field_subset<FieldT> domain(1ull << log_sz);
    const FieldT interpolation_point = FieldT::random_element();

    for (auto _ : state)
    {
        const std::vector<FieldT> result = lagrange_coefficients<FieldT>(
            domain, interpolation_point);
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_lagrange_multiplicative)->Range(1ull<<4, 1ull<<24)->Unit(benchmark::kMicrosecond);

static void BM_lagrange_additive_cached(benchmark::State &state)
{
    typedef gf64 FieldT;

    const size_t sz = state.range(0);
    const size_t log_sz = log2(sz);

    const affine_subspace<FieldT> domain =
        affine_subspace<FieldT>::random_affine_subspace(log_sz);
    lagrange_cache<FieldT> L_cache(domain, false);
    const FieldT interpolation_point = FieldT::random_element();

    for (auto _ : state)
    {
        benchmark::DoNotOptimize(L_cache.coefficients_for(
            interpolation_point));
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_lagrange_additive_cached)->Range(1ull<<4, 1ull<<24)->Unit(benchmark::kMicrosecond);

static void BM_lagrange_multiplicative_cached(benchmark::State &state)
{
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;

    const size_t sz = state.range(0);
    const size_t log_sz = log2(sz);

    const field_subset<FieldT> domain(1ull << log_sz);
    lagrange_cache<FieldT> L_cache(domain, false);
    const FieldT interpolation_point = FieldT::random_element();

    for (auto _ : state)
    {
        benchmark::DoNotOptimize(L_cache.coefficients_for(
            interpolation_point));
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_lagrange_multiplicative_cached)->Range(1ull<<4, 1ull<<24)->Unit(benchmark::kMicrosecond);

}

BENCHMARK_MAIN();
