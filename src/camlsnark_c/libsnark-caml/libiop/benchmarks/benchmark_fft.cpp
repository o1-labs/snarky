#include <vector>
#include <benchmark/benchmark.h>

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/algebra/utils.hpp"
#include "libiop/common/common.hpp"
#include <libff/algebra/curves/edwards/edwards_pp.hpp>


namespace libiop {

/* This benchmarks on a random affine subspace,
   not an arbitrary domain in the field. This is due to
   that being the domain which it will be compared against
   for the other fft's. */
static void BM_naive_FFT(benchmark::State &state)
{
    typedef gf64 FieldT;

    const size_t sz = state.range(0);
    const size_t log_sz = log2(sz);

    const std::vector<FieldT> v = random_vector<FieldT>(sz);

    const affine_subspace<FieldT> domain =
        affine_subspace<FieldT>::random_affine_subspace(log_sz);

    for (auto _ : state)
    {
        const std::vector<FieldT> result = naive_FFT<FieldT>(v, domain);
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_naive_FFT)->Range(1ull<<4, 1ull<<15)->Unit(benchmark::kMicrosecond);

static void BM_additive_FFT(benchmark::State &state)
{
    typedef gf64 FieldT;

    const size_t sz = state.range(0);
    const size_t log_sz = log2(sz);

    const std::vector<FieldT> poly_coeffs = random_vector<FieldT>(sz);

    const affine_subspace<FieldT> domain =
        affine_subspace<FieldT>::random_affine_subspace(log_sz);

    for (auto _ : state)
    {
        const std::vector<FieldT> result = additive_FFT<FieldT>(poly_coeffs, domain);
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_additive_FFT)->Range(1ull<<4, 1ull<<20)->Unit(benchmark::kMicrosecond);

static void BM_additive_IFFT(benchmark::State &state)
{
    typedef gf64 FieldT;

    const size_t sz = state.range(0);
    const size_t log_sz = log2(sz);

    const std::vector<FieldT> evals = random_vector<FieldT>(sz);

    const affine_subspace<FieldT> domain =
        affine_subspace<FieldT>::random_affine_subspace(log_sz);

    for (auto _ : state)
    {
        const std::vector<FieldT> result = additive_FFT<FieldT>(evals, domain);
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_additive_IFFT)->Range(1ull<<4, 1ull<<20)->Unit(benchmark::kMicrosecond);

static void BM_multiplicative_subgroup_FFT(benchmark::State &state)
{
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;

    const size_t sz = state.range(0);
    const size_t log_sz = log2(sz);

    const std::vector<FieldT> poly_coeffs = random_vector<FieldT>(sz);

    /* Speed is slower when shift != 1 */
    const multiplicative_coset<FieldT> domain = multiplicative_coset<FieldT>(sz);
    domain.fft_cache();

    for (auto _ : state)
    {
        const std::vector<FieldT> result = multiplicative_FFT<FieldT>(poly_coeffs, domain);
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_multiplicative_subgroup_FFT)->Range(1ull<<4, 1ull<<20)->Unit(benchmark::kMicrosecond);

static void BM_multiplicative_subgroup_IFFT(benchmark::State &state)
{
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;

    const size_t sz = state.range(0);
    const size_t log_sz = log2(sz);

    const std::vector<FieldT> poly_coeffs = random_vector<FieldT>(sz);

    /* Speed is slower when shift != 1 */
    const multiplicative_coset<FieldT> domain = multiplicative_coset<FieldT>(sz);

    for (auto _ : state)
    {
        const std::vector<FieldT> result = multiplicative_IFFT<FieldT>(poly_coeffs, domain);
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_multiplicative_subgroup_IFFT)->Range(1ull<<4, 1ull<<20)->Unit(benchmark::kMicrosecond);

static void BM_multiplicative_coset_FFT(benchmark::State &state)
{
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;

    const size_t sz = state.range(0);
    const size_t log_sz = log2(sz);

    const std::vector<FieldT> poly_coeffs = random_vector<FieldT>(sz);

    const multiplicative_coset<FieldT> domain(sz, FieldT::multiplicative_generator);
    domain.fft_cache();

    for (auto _ : state)
    {
        const std::vector<FieldT> result = multiplicative_FFT<FieldT>(poly_coeffs, domain);
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_multiplicative_coset_FFT)->Range(1ull<<4, 1ull<<20)->Unit(benchmark::kMicrosecond);

static void BM_multiplicative_coset_IFFT(benchmark::State &state)
{
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;

    const size_t sz = state.range(0);
    const size_t log_sz = log2(sz);

    const std::vector<FieldT> poly_coeffs = random_vector<FieldT>(sz);

    /* Speed is slower when shift != 1 */
    const multiplicative_coset<FieldT> domain(sz, FieldT::multiplicative_generator);

    for (auto _ : state)
    {
        const std::vector<FieldT> result = multiplicative_IFFT<FieldT>(poly_coeffs, domain);
    }

    state.SetItemsProcessed(state.iterations() * sz);
}

BENCHMARK(BM_multiplicative_coset_IFFT)->Range(1ull<<4, 1ull<<20)->Unit(benchmark::kMicrosecond);
}

BENCHMARK_MAIN();
