#include <vector>
#include <benchmark/benchmark.h>

#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/algebra/utils.hpp"
#include "libiop/common/common.hpp"
#include <libff/algebra/curves/edwards/edwards_pp.hpp>

namespace libiop {

static void BM_vanishing_polynomial_from_subspace(benchmark::State &state)
{
    typedef gf256 FieldT;

    const size_t sz = state.range(0);
    const size_t log_sz = log2(sz);

    const affine_subspace<FieldT> domain =
        affine_subspace<FieldT>::random_affine_subspace(log_sz);

    for (auto _ : state)
    {
        benchmark::DoNotOptimize(vanishing_polynomial<FieldT>(domain));
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_vanishing_polynomial_from_subspace)->Range(1ull<<10, 1ull<<20)->Unit(benchmark::kMicrosecond);

static void BM_gf64_linearized_polynomial_times_small_polynomial(benchmark::State &state)
{
    typedef gf64 FieldT;

    const size_t sz = state.range(0);
    const size_t log_sz = log2(sz);
    const size_t small_poly_degree = 1000;

    const affine_subspace<FieldT> domain =
        affine_subspace<FieldT>::random_affine_subspace(log_sz);
    const vanishing_polynomial<FieldT> Z(domain);
    const polynomial<FieldT> poly =
        polynomial<FieldT>::random_polynomial(small_poly_degree);

    for (auto _ : state)
    {
        benchmark::DoNotOptimize(Z * poly);
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_gf64_linearized_polynomial_times_small_polynomial)->Range(1ull<<10, 1ull<<20)->Unit(benchmark::kMicrosecond);

static void BM_edwards_linearized_polynomial_times_small_polynomial(benchmark::State &state)
{
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;

    const size_t sz = state.range(0);
    const size_t log_sz = log2(sz);
    const size_t small_poly_degree = 1000;

    const field_subset<FieldT> domain(1ull << log_sz, FieldT(0));
    const vanishing_polynomial<FieldT> Z(domain);
    const polynomial<FieldT> poly =
        polynomial<FieldT>::random_polynomial(small_poly_degree);

    for (auto _ : state)
    {
        benchmark::DoNotOptimize(Z * poly);
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_edwards_linearized_polynomial_times_small_polynomial)->Range(1ull<<10, 1ull<<20)->Unit(benchmark::kMicrosecond);

static void BM_gf64_polynomial_over_vanishing_polynomial(benchmark::State &state)
{
    typedef gf64 FieldT;

    const size_t sz = state.range(0);
    const size_t log_sz = log2(sz);
    const size_t poly_degree = sz;
    const size_t vanishing_poly_degree = sz >> 1;

    const field_subset<FieldT> domain(vanishing_poly_degree);
    const vanishing_polynomial<FieldT> Z(domain);
    const polynomial<FieldT> poly =
        Z * polynomial<FieldT>::random_polynomial(poly_degree - vanishing_poly_degree);

    for (auto _ : state)
    {
        benchmark::DoNotOptimize(polynomial_over_vanishing_polynomial(poly, Z));
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_gf64_polynomial_over_vanishing_polynomial)->Range(1ull<<10, 1ull<<20)->Unit(benchmark::kMicrosecond);

static void BM_edwards_polynomial_over_vanishing_polynomial(benchmark::State &state)
{
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;

    const size_t sz = state.range(0);
    const size_t log_sz = log2(sz);
    const size_t poly_degree = sz;
    const size_t vanishing_poly_degree = sz >> 1;

    const field_subset<FieldT> domain(vanishing_poly_degree);
    const vanishing_polynomial<FieldT> Z(domain);
    const polynomial<FieldT> poly =
        Z * polynomial<FieldT>::random_polynomial(poly_degree - vanishing_poly_degree);

    for (auto _ : state)
    {
        benchmark::DoNotOptimize(polynomial_over_vanishing_polynomial(poly, Z));
    }


    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_edwards_polynomial_over_vanishing_polynomial)->Range(1ull<<10, 1ull<<20)->Unit(benchmark::kMicrosecond);
}

BENCHMARK_MAIN();
