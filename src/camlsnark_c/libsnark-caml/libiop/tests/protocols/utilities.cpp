#include <cstdint>
#include <memory>
#include <stdexcept>

#include <gtest/gtest.h>

#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/iop/iop.hpp"

#include <libff/algebra/curves/edwards/edwards_pp.hpp>
#include <libff/algebra/curves/alt_bn128/alt_bn128_pp.hpp>

namespace libiop {

/** This method is deprecated in favor of the latter method,
 * It is kept for debugging purposes. */
template<typename FieldT>
FieldT sum_over_field_subset(const polynomial<FieldT> &P,
                             const field_subset<FieldT> &S) {
    FieldT sum = FieldT::zero();
    for (auto &el : S.all_elements())
    {
        sum += P.evaluation_at_point(el);
    }
    return sum;
}

template<typename FieldT>
FieldT sum_over_default_field_subset(const polynomial<FieldT> &P,
                                     const field_subset<FieldT> &S) {
    // This assumes that S was created over the default basis.
    // It then creates an extended domain, and does an FFT to convert P to evaluations
    // in that extended domain.
    const std::size_t dim = std::max(log2(P.num_terms()), log2(S.num_elements()));
    FieldT sum = FieldT::zero();
    if (is_power_of_2(S.num_elements()))
    {
        const field_subset<FieldT> extended_subset(1ull << dim);
        const std::vector<FieldT> evals = FFT_over_field_subset(P.coefficients(), extended_subset);
        for (std::size_t i = 0; i < S.num_elements(); i++)
        {
            sum += evals[extended_subset.reindex_by_subset(S.dimension(),i)];
        }
    }
    else
    {
        const std::vector<FieldT> evals = naive_FFT(P.coefficients(), S);
        for (std::size_t i = 0; i < S.num_elements(); i++)
        {
            sum += evals[i];
        }
    }
    return sum;
}

template<typename FieldT>
std::vector<FieldT> compute_rational_over_domain(
    const polynomial<FieldT> &numerator,
    const polynomial<FieldT> &denominator,
    const field_subset<FieldT> &summation_domain)
{
    // TODO: Implement FFTs for the case where polynomial degree > domain size
    std::vector<FieldT> numerator_over_summation_domain =
        naive_FFT<FieldT>(numerator.coefficients(), summation_domain);
    std::vector<FieldT> denominator_over_summation_domain =
        naive_FFT<FieldT>(denominator.coefficients(), summation_domain);
    denominator_over_summation_domain = batch_inverse(denominator_over_summation_domain);
    std::vector<FieldT> rational_over_summation_domain;
    for (size_t i = 0; i < summation_domain.num_elements(); i++)
    {
        rational_over_summation_domain.emplace_back(
            numerator_over_summation_domain[i] * denominator_over_summation_domain[i]);
    }
    return rational_over_summation_domain;
}

template<typename FieldT>
FieldT sum_of_rational_over_domain(
    const polynomial<FieldT> &numerator,
    const polynomial<FieldT> &denominator,
    const field_subset<FieldT> &summation_domain)
{
    std::vector<FieldT> rational_over_summation_domain =
        compute_rational_over_domain(numerator, denominator, summation_domain);
    FieldT sum = FieldT::zero();
    for (size_t i = 0; i < summation_domain.num_elements(); i++)
    {
        sum += rational_over_summation_domain[i];
    }
    return sum;
}

template<typename FieldT>
std::size_t degree_bound_from_evals(const std::vector<FieldT> &evals,
                                    const field_subset<FieldT> &domain)
{
    const polynomial<FieldT> poly_from_evals(IFFT_over_field_subset<FieldT>(evals, domain));
    const std::size_t minimal_num_terms = poly_from_evals.minimal_num_terms();

    return minimal_num_terms;
}

template<typename FieldT>
std::size_t degree_bound_from_coeffs(const std::vector<FieldT> &coeffs)
{
    for (size_t i = coeffs.size() - 1; i > -1; i--)
    {
        if (coeffs[i] != FieldT::zero())
        {
            return i;
        }
    }
    return 0;
}

template<typename FieldT>
void test_oracle_consistency(iop_protocol<FieldT> &IOP,
                             const oracle_handle_ptr &handle,
                             const std::vector<FieldT> &oracle_evals,
                             const field_subset<FieldT> &codeword_domain) {
    for (std::size_t i = 0; i < 10; i++) {
        std::size_t evaluation_index = std::rand() % codeword_domain.num_elements();
        const FieldT point_eval = IOP.get_oracle_evaluation_at_point(handle, evaluation_index, false);
        EXPECT_TRUE(point_eval == oracle_evals[evaluation_index]) << "evaluation at point was inconsistent at index " <<
            evaluation_index;
    }
}

template<typename FieldT>
void test_oracles_degree_and_consistency(iop_protocol<FieldT> &IOP,
                                        const std::vector<oracle_handle_ptr> &handles,
                                        const field_subset<FieldT> &codeword_domain,
                                        const bool exp_pass,
                                        const bool permit_LTE = false)
{
    bool passed = true;
    for (std::size_t i = 0; i < handles.size(); i++)
    {
        const oracle_handle_ptr handle = handles[i];
        const std::vector<FieldT> evals = *IOP.get_oracle_evaluations(handle).get();
        const std::size_t expected_degree = IOP.get_oracle_degree(handle);
        const std::size_t actual_degree = degree_bound_from_evals(evals, codeword_domain);
        if (exp_pass) {
            // TODO: Make handles have a concept of human readable name.
            if (!permit_LTE)
            {
                // Check strict equality
                ASSERT_EQ(expected_degree, actual_degree) << "handle " << i << " did not have correct degree.";
            }
            else
            {
                ASSERT_LE(actual_degree, expected_degree) << "handle " << i << " did not have correct degree.";
            }

        } else {
            passed = passed & (actual_degree <= expected_degree);
        }
        test_oracle_consistency(IOP, handle, evals, codeword_domain);
    }
    if (!exp_pass) {
        ASSERT_FALSE(passed) << "All of the oracle handles had passing degrees.";
    }
}

template<typename FieldT>
std::vector<FieldT> make_codeword_zk(std::vector<FieldT> codeword,
                                     std::size_t query_bound,
                                     field_subset<FieldT> systematic_domain,
                                     field_subset<FieldT> codeword_domain) {
    const vanishing_polynomial<FieldT> constraint_vp(systematic_domain);
    const std::vector<FieldT> constraint_vp_over_codeword_domain =
        constraint_vp.evaluations_over_field_subset(codeword_domain);
    const polynomial<FieldT> R = polynomial<FieldT>::random_polynomial(query_bound);
    const std::vector<FieldT> R_over_codeword_domain = FFT_over_field_subset(
        R.coefficients(), codeword_domain);
    std::vector<FieldT> zk_codeword;
    for (std::size_t i = 0; i < codeword_domain.num_elements(); ++i)
    {
        zk_codeword.emplace_back(codeword[i] +
            R_over_codeword_domain[i] * constraint_vp_over_codeword_domain[i]);
    }
    return zk_codeword;
}

template<typename FieldT>
std::vector<std::vector<FieldT>> create_n_codewords_of_given_degree(
    field_subset<FieldT> codeword_domain,
    size_t degree,
    size_t n)
{
    std::vector<std::vector<FieldT>> codewords(n);
    for (size_t i = 0; i < n; i++)
    {
        polynomial<FieldT> poly = polynomial<FieldT>::random_polynomial(degree);
        std::vector<FieldT> codeword = FFT_over_field_subset<FieldT>(
            poly.coefficients(), codeword_domain);
        codewords[i] = codeword;
    }
    return codewords;
}

}