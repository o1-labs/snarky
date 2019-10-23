#include <vector>
#include <benchmark/benchmark.h>

#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/algebra/utils.hpp"
#include "libiop/common/common.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/protocols/encoded/sumcheck/sumcheck.hpp"
#include "libiop/relations/examples/r1cs_examples.hpp"
#include "libiop/relations/r1cs.hpp"

namespace libiop {

template<typename FieldT>
std::pair<std::vector<polynomial<FieldT>>, std::vector<FieldT>>get_random_polys_and_sum(
    const std::size_t num_polys,
    const field_subset<FieldT> summation_domain,
    const std::size_t poly_degree_bound) {

    std::vector<FieldT> betas;
    std::vector<polynomial<FieldT>> sumcheck_polynomials;
    betas.resize(num_polys);
    sumcheck_polynomials.resize(num_polys);
    for (std::size_t i = 0; i < num_polys; i++) {
        sumcheck_polynomials[i] = polynomial<FieldT>::random_polynomial(poly_degree_bound);
        betas[i] = sum_over_default_field_subset(
            sumcheck_polynomials[i], summation_domain);
    }
    return std::make_pair(sumcheck_polynomials, betas);
}
template<typename FieldT>
FieldT sum_over_default_field_subset(const polynomial<FieldT> &P,
                                     const field_subset<FieldT> &S) {
    // This assumes that S was created over the default basis.
    // It then creates an extended domain, and does an FFT to convert P to evaluations
    // in that extended domain.
    const std::size_t dim = std::max(log2(P.num_terms()), S.dimension());
    const field_subset<FieldT> extended_subset(1ull << dim);
    const std::vector<FieldT> evals = FFT_over_field_subset(P.coefficients(), extended_subset);
    FieldT sum = FieldT::zero();
    for (std::size_t i = 0; i < S.num_elements(); i++)
    {
        sum += evals[extended_subset.reindex_by_subset(S.dimension(),i)];
    }
    return sum;
}

static void BM_sumcheck_additive_eval_at_point(benchmark::State &state) {
    typedef gf64 FieldT;
    const bool make_zk = false;
    const size_t sz = state.range(0);
    const size_t summation_domain_dim = log2(sz);
    const std::size_t RS_extra_dimensions = 2; /* \rho = 1/4 */
    const size_t codeword_domain_dim = RS_extra_dimensions + 2 + summation_domain_dim;
    FieldT shift = FieldT(1ull << codeword_domain_dim);

    const std::size_t summation_domain_size = 1ull << summation_domain_dim;
    const std::size_t codeword_domain_size = 1ull << codeword_domain_dim;
    const std::size_t poly_degree_bound = 1ull << (summation_domain_dim + 1);
    const field_subset<FieldT> summation_domain(summation_domain_size);
    const field_subset<FieldT> codeword_domain(codeword_domain_size, shift);

    iop_protocol<FieldT> IOP;
    const domain_handle summation_domain_handle = IOP.register_domain(summation_domain);
    const domain_handle codeword_domain_handle = IOP.register_domain(codeword_domain);

    const std::size_t num_polys = 3;
    std::vector<oracle_handle> poly_oracle_handles;
    const std::pair<std::vector<polynomial<FieldT>>, std::vector<FieldT>> polynomials_and_sums =
        get_random_polys_and_sum(num_polys, summation_domain, poly_degree_bound);
    poly_oracle_handles.resize(num_polys);
    for (std::size_t i = 0; i < num_polys; i++) {
        poly_oracle_handles[i] = IOP.register_oracle(codeword_domain_handle, poly_degree_bound, make_zk);
    }

    batch_sumcheck_protocol<FieldT> sumcheck(IOP,
                                             summation_domain_handle,
                                             codeword_domain_handle,
                                             poly_degree_bound,
                                             make_zk,
                                             affine_subspace_type);
    if (make_zk) {
        sumcheck.register_masking_polynomial();
    }
    for (std::size_t i = 0; i < num_polys; i++) {
        sumcheck.attach_oracle_for_summing(
            std::make_shared<oracle_handle>(poly_oracle_handles[i]), polynomials_and_sums.second[i]);
    }
    sumcheck.register_challenge();
    /* in principle there could have been other IOP messages exchanged in between these calls */
    sumcheck.register_proof();
    IOP.seal_interaction_registrations();

    IOP.seal_query_registrations();

    /* Proving */
    for (std::size_t i = 0; i < num_polys; i++) {
        std::vector<FieldT> poly_over_codeword_domain = FFT_over_field_subset(
            polynomials_and_sums.first[i].coefficients(), codeword_domain);
        oracle<FieldT> poly_oracle = oracle<FieldT>(poly_over_codeword_domain);
        IOP.submit_oracle(poly_oracle_handles[i], std::move(poly_oracle));
    }
    if (make_zk) {
        sumcheck.submit_masking_polynomial();
    }

    /* again, there could have been interleaved things here */
    IOP.signal_prover_round_done();
    sumcheck.calculate_and_submit_proof();
    IOP.signal_prover_round_done();

    /* Verification */
    /* verify relative to the statement that the prover used */
    oracle_handle_ptr g_handle = std::make_shared<virtual_oracle_handle>(sumcheck.get_g_oracle_handle());
    for (auto _ : state)
    {
        std::size_t evaluation_index = std::rand() % codeword_domain.num_elements();
        benchmark::DoNotOptimize(IOP.get_oracle_evaluation_at_point(g_handle, evaluation_index, false));
    }
}

BENCHMARK(BM_sumcheck_additive_eval_at_point)->Range(1ull<<8, 1ull<<20)->Unit(benchmark::kMicrosecond);
}

BENCHMARK_MAIN();
