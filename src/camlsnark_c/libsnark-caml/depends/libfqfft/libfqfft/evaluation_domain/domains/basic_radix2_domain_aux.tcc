/** @file
 *****************************************************************************

 Implementation of interfaces for auxiliary functions for the "basic radix-2" evaluation domain.

 See basic_radix2_domain_aux.hpp .

 *****************************************************************************
 * @author     This file is part of libfqfft, developed by SCIPR Lab
 *             and contributors (see AUTHORS).
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef BASIC_RADIX2_DOMAIN_AUX_TCC_
#define BASIC_RADIX2_DOMAIN_AUX_TCC_

#include <algorithm>
#include <vector>

#ifdef MULTICORE
#include <omp.h>
#endif

#include <libff/algebra/fields/field_utils.hpp>
#include <libff/common/utils.hpp>

#include <libfqfft/tools/exceptions.hpp>

#ifdef DEBUG
#include <libff/common/profiling.hpp>
#endif

namespace libfqfft {

#ifdef MULTICORE
#define _basic_radix2_FFT _basic_parallel_radix2_FFT
#else
#define _basic_radix2_FFT _basic_serial_FFT
#endif

/*
 Below we make use of pseudocode from [CLRS 2n Ed, pp. 864].
 Also, note that it's the caller's responsibility to multiply by 1/N.
 */
template<typename FieldT>
void _basic_serial_mixed_radix_FFT(std::vector<FieldT> &a, const FieldT &omega)
{
    // Conceptually, this FFT first splits into 2 sub-arrays two_adicity many times,
    // and then splits into q sub-arrays q_adicity many times.

    const size_t n = a.size();
    const size_t q = FieldT::small_subgroup_base;

    const size_t q_adicity = libff::k_adicity(q, n);
    const size_t q_part = libff::pow_int(q, q_adicity);

    const size_t two_adicity = libff::k_adicity(2, n);
    const size_t two_part = 1u << two_adicity;

    if (n != q_part * two_part) throw DomainSizeException("expected n == (1u << logn)");

    size_t m = 1; // invariant: m = 2^{s-1}

    if (q_adicity > 0)
    {
        // If we're using the other radix, we have to do two things differently than in the radix 2 case.
        // 1. Applying the index permutation is a bit more complicated. It isn't an involution
        // (like it is in the radix 2 case) so we need to remember which elements we've moved as we go along
        // and can't use the trick of just swapping when processing the first element of a 2-cycle.
        //
        // 2. We need to do q_adicity many merge passes, each of which is a bit more complicated than the
        // specialized q=2 case.

        // Applying the permutation
        std::vector<bool> seen(n, false);
        for (size_t k = 0; k < n; ++k)
        {
            size_t i = k;
            FieldT a_i = a[i];
            while (! seen[i])
            {
                size_t dest = libff::mixed_radix_FFT_permute(two_adicity, q_adicity, q, n, i);

                FieldT a_dest = a[dest];
                a[dest] = a_i;

                seen[i] = true;

                a_i = a_dest;
                i = dest;
            }
        }

        const FieldT omega_q = omega ^ (n / q);
        std::vector<FieldT> qth_roots(q);
        qth_roots[0] = FieldT::one();
        for (size_t i = 1; i < q; ++i) {
            qth_roots[i] = qth_roots[i-1] * omega_q;
        }

        std::vector<FieldT> terms(q-1);

        // Doing the q_adicity passes.
        for (size_t s = 1; s <= q_adicity; ++s)
        {
            const FieldT w_m = omega ^ (n / (q*m));
            asm volatile  ("/* pre-inner */");
            for (size_t k = 0; k < n; k += q*m)
            {
                FieldT w_j = FieldT::one(); // w_j is omega_m ^ j
                for (size_t j = 0; j < m; ++j)
                {
                    FieldT base_term = a[k+j];
                    FieldT w_j_i = w_j;
                    for (size_t i = 1; i < q; ++i) {
                        terms[i-1] = w_j_i * a[k+j + i*m];
                        w_j_i *= w_j;
                    }

                    for (size_t i = 0; i < q; ++i) {
                        a[k+j + i*m] = base_term;
                        for (size_t l = 1; l < q; ++l) {
                          a[k+j + i*m] += qth_roots[(i*l) % q] * terms[l-1];
                        }
                    }

                    w_j *= w_m;
                }
            }
            asm volatile ("/* post-inner */");
            m *= q;
        }
    }
    else
    {
        /* swapping in place (from Storer's book) */
        for (size_t k = 0; k < n; ++k)
        {
            const size_t rk = libff::bitreverse(k, two_adicity);
            if (k < rk)
                std::swap(a[k], a[rk]);
        }
    }

    for (size_t s = 1; s <= two_adicity; ++s)
    {
        // w_m is 2^s-th root of unity now
        const FieldT w_m = omega^(n/(2*m));

        asm volatile  ("/* pre-inner */");
        for (size_t k = 0; k < n; k += 2*m)
        {
            FieldT w = FieldT::one();
            for (size_t j = 0; j < m; ++j)
            {
                const FieldT t = w * a[(k+m)+j];
                a[(k+m)+j] = a[k+j] - t;
                a[k+j] += t;
                w *= w_m;
            }
        }
        asm volatile ("/* post-inner */");
        m *= 2;
    }
}

template<typename FieldT>
void _basic_serial_radix2_FFT(std::vector<FieldT> &a, const FieldT &omega)
{
    const size_t n = a.size(), logn = log2(n);
    if (n != (1u << logn)) throw DomainSizeException("expected n == (1u << logn)");

    /* swapping in place (from Storer's book) */
    for (size_t k = 0; k < n; ++k)
    {
        const size_t rk = libff::bitreverse(k, logn);
        if (k < rk)
            std::swap(a[k], a[rk]);
    }

    size_t m = 1; // invariant: m = 2^{s-1}
    for (size_t s = 1; s <= logn; ++s)
    {
        // w_m is 2^s-th root of unity now
        const FieldT w_m = omega^(n/(2*m));

        asm volatile  ("/* pre-inner */");
        for (size_t k = 0; k < n; k += 2*m)
        {
            FieldT w = FieldT::one();
            for (size_t j = 0; j < m; ++j)
            {
                const FieldT t = w * a[k+j+m];
                a[k+j+m] = a[k+j] - t;
                a[k+j] += t;
                w *= w_m;
            }
        }
        asm volatile ("/* post-inner */");
        m *= 2;
    }
}

template<typename FieldT>
void _basic_serial_FFT(std::vector<FieldT> &a, const FieldT &omega)
{
    if (FieldT::small_subgroup_defined)
    {
        _basic_serial_mixed_radix_FFT(a, omega);
    }
    else
    {
        _basic_serial_radix2_FFT(a, omega);
    }
}

template<typename FieldT>
void _basic_parallel_radix2_FFT_inner(std::vector<FieldT> &a, const FieldT &omega, const size_t log_cpus)
{
    const size_t num_chunks = 1ul<<log_cpus;

    const size_t m = a.size();
    const size_t two_adicity = libff::k_adicity(2, m);
    const size_t two_part = 1u << two_adicity;

    if (FieldT::small_subgroup_defined)
    {
        const size_t q = FieldT::small_subgroup_base;
        const size_t q_adicity = libff::k_adicity(q, m);
        const size_t q_part = libff::pow_int(q, q_adicity);

        if (m != q_part * two_part) throw DomainSizeException("expected m == q_part * two_part");
    }
    else
    {
        if (m != two_part) throw DomainSizeException("expected m == two_part");
    }

    if (two_part < num_chunks)
    {
        _basic_serial_FFT(a, omega);
        return;
    }

    assert (m % num_chunks == 0);
    std::vector<std::vector<FieldT> > tmp(num_chunks);
    for (size_t j = 0; j < num_chunks; ++j)
    {
        tmp[j].resize(m / num_chunks, FieldT::zero());
    }

#ifdef MULTICORE
    #pragma omp parallel for
#endif
    for (size_t j = 0; j < num_chunks; ++j)
    {
        const FieldT omega_j = omega^j;
        const FieldT omega_step = omega^(j*(m / num_chunks));

        FieldT elt = FieldT::one();
        for (size_t i = 0; i < (m/num_chunks); ++i)
        {
            for (size_t s = 0; s < num_chunks; ++s)
            {
                // invariant: elt is omega^(j*idx)
                const size_t idx = (i + s*(m/num_chunks)) % m;
                tmp[j][i] += a[idx] * elt;
                elt *= omega_step;
            }
            elt *= omega_j;
        }
    }

    const FieldT omega_num_chunks = omega^num_chunks;

#ifdef MULTICORE
    #pragma omp parallel for
#endif
    for (size_t j = 0; j < num_chunks; ++j)
    {
        _basic_serial_FFT(tmp[j], omega_num_chunks);
    }

#ifdef MULTICORE
    #pragma omp parallel for
#endif
    for (size_t i = 0; i < num_chunks; ++i)
    {
        for (size_t j = 0; j < (m/num_chunks); ++j)
        {
            // now: i = idx >> (log_m - log_cpus) and j = idx % (1u << (log_m - log_cpus)), for idx = ((i<<(log_m-log_cpus))+j) % (1u << log_m)
            a[j * num_chunks + i] = tmp[i][j];
        }
    }
}

template<typename FieldT>
void _basic_parallel_radix2_FFT(std::vector<FieldT> &a, const FieldT &omega)
{
#ifdef MULTICORE
    const size_t num_chunks = omp_get_max_threads();
#else
    const size_t num_chunks = 1;
#endif
    const size_t log_cpus = ((num_chunks & (num_chunks - 1)) == 0 ? log2(num_chunks) : log2(num_chunks) - 1);

#ifdef DEBUG
    libff::print_indent(); printf("* Invoking parallel FFT on 2^%zu CPUs (omp_get_max_threads = %zu)\n", log_cpus, num_chunks);
#endif

    if (log_cpus == 0)
    {
        _basic_serial_FFT(a, omega);
    }
    else
    {
        _basic_parallel_radix2_FFT_inner(a, omega, log_cpus);
    }
}

template<typename FieldT>
void _multiply_by_coset(std::vector<FieldT> &a, const FieldT &g)
{
    FieldT u = g;
    for (size_t i = 1; i < a.size(); ++i)
    {
        a[i] *= u;
        u *= g;
    }
}

template<typename FieldT>
std::vector<FieldT> _basic_radix2_evaluate_all_lagrange_polynomials(const size_t m, const FieldT &t)
{
    if (m == 1)
    {
        return std::vector<FieldT>(1, FieldT::one());
    }

    bool err = false;
    const FieldT omega = libff::get_root_of_unity<FieldT>(m, err);
    if (err) {
      throw DomainSizeException("Failed get_root_of_unity");
    }

    std::vector<FieldT> u(m, FieldT::zero());

    /*
     If t equals one of the roots of unity in S={omega^{0},...,omega^{m-1}}
     then output 1 at the right place, and 0 elsewhere
     */

    if ((t^m) == (FieldT::one()))
    {
        FieldT omega_i = FieldT::one();
        for (size_t i = 0; i < m; ++i)
        {
            if (omega_i == t) // i.e., t equals omega^i
            {
                u[i] = FieldT::one();
                return u;
            }

            omega_i *= omega;
        }
    }

    /*
     Otherwise, if t does not equal any of the roots of unity in S,
     then compute each L_{i,S}(t) as Z_{S}(t) * v_i / (t-\omega^i)
     where:
     - Z_{S}(t) = \prod_{j} (t-\omega^j) = (t^m-1), and
     - v_{i} = 1 / \prod_{j \neq i} (\omega^i-\omega^j).
     Below we use the fact that v_{0} = 1/m and v_{i+1} = \omega * v_{i}.
     */

    const FieldT Z = (t^m)-FieldT::one();
    FieldT l = Z * FieldT(m).inverse();
    FieldT r = FieldT::one();
    for (size_t i = 0; i < m; ++i)
    {
        u[i] = l * (t - r).inverse();
        l *= omega;
        r *= omega;
    }

    return u;
}

} // libfqfft

#endif // BASIC_RADIX2_DOMAIN_AUX_TCC_
