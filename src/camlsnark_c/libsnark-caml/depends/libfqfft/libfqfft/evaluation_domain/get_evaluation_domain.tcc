/** @file
 *****************************************************************************

 Imeplementation of interfaces for evaluation domains.

 See evaluation_domain.hpp .

 We currently implement, and select among, three types of domains:
 - "basic radix-2": the domain has size m = 2^k and consists of the m-th roots of unity
 - "extended radix-2": the domain has size m = 2^{k+1} and consists of "the m-th roots of unity" union "a coset"
 - "step radix-2": the domain has size m = 2^k + 2^r and consists of "the 2^k-th roots of unity" union "a coset of 2^r-th roots of unity"

 *****************************************************************************
 * @author     This file is part of libfqfft, developed by SCIPR Lab
 *             and contributors (see AUTHORS).
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef GET_EVALUATION_DOMAIN_TCC_
#define GET_EVALUATION_DOMAIN_TCC_

#include <libfqfft/evaluation_domain/domains/arithmetic_sequence_domain.hpp>
#include <libfqfft/evaluation_domain/domains/basic_radix2_domain.hpp>
#include <libfqfft/evaluation_domain/domains/extended_radix2_domain.hpp>
#include <libfqfft/evaluation_domain/domains/geometric_sequence_domain.hpp>
#include <libfqfft/evaluation_domain/domains/step_radix2_domain.hpp>
#include <libfqfft/evaluation_domain/evaluation_domain.hpp>
#include <libfqfft/tools/exceptions.hpp>

namespace libfqfft {

// Should be
// min { n : N | n = 2^a * q^b, n >= min_size, b <= small_subgroup_power, a <= s }.
template<typename FieldT>
size_t best_mixed_domain_size(const size_t min_size)
{
    size_t best = std::numeric_limits<size_t>::max();
    for (size_t b = 0; b <= FieldT::small_subgroup_power; ++b)
    {
        size_t r = libff::pow_int(FieldT::small_subgroup_base, b);

        size_t two_adicity = 0;
        while (r < min_size)
        {
            r *= 2;
            ++two_adicity;
        }

        if (two_adicity <= FieldT::s)
        {
            best = std::min(best, r);
        }
    }

    return best;
}

template<typename FieldT>
std::shared_ptr<evaluation_domain<FieldT> > get_evaluation_domain(const size_t min_size)
{
    std::shared_ptr<evaluation_domain<FieldT> > result;

    const size_t big = 1ul<<(libff::log2(min_size)-1);
    const size_t small = min_size - big;
    const size_t rounded_small = (1ul<<libff::log2(small));

    bool err = false;
    auto basic = new basic_radix2_domain<FieldT>(min_size, err);
    if (!err) {
      result.reset(basic);
      return result;
    }
    err = false;

    auto extended = new extended_radix2_domain<FieldT>(min_size, err);
    if (!err) {
      result.reset(extended);
      return result;
    }
    err = false;

    auto step = new step_radix2_domain<FieldT>(min_size, err);
    if (!err) {
      result.reset(step);
      return result;
    }
    err = false;

    auto basic2 = new basic_radix2_domain<FieldT>(big + rounded_small, err);
    if (!err) {
      result.reset(basic2);
      return result;
    }
    err = false;

    auto extended2 = new extended_radix2_domain<FieldT>(big + rounded_small, err);
    if (!err) {
      result.reset(extended2);
      return result;
    }
    err = false;

    auto step2 = new step_radix2_domain<FieldT>(big + rounded_small, err);
    if (!err) {
      result.reset(step2);
      return result;
    }
    err = false;

    if (FieldT::small_subgroup_defined) {
      const size_t s = best_mixed_domain_size<FieldT>(min_size);
      auto mixed = new basic_radix2_domain<FieldT>(s, err);
      if (!err) {
        result.reset(mixed);
        return result;
      }
      err = false;
    }

    auto geometric = new geometric_sequence_domain<FieldT>(min_size, err);
    if (!err) {
      result.reset(geometric);
      return result;
    }
    err = false;

    auto arithmetic = new arithmetic_sequence_domain<FieldT>(min_size, err);
    if (!err) {
      result.reset(arithmetic);
      return result;
    }
    err = false;

    throw DomainSizeException("get_evaluation_domain: no matching domain");
}

} // libfqfft

#endif // GET_EVALUATION_DOMAIN_TCC_
