/**@file
*****************************************************************************
Rational Sumcheck interface
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_ENCODED_RATIONAL_SUMCHECK_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_RATIONAL_SUMCHECK_HPP_

#include <cstddef>
#include <memory>
#include <stdexcept>
#include <vector>

#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/protocols/encoded/sumcheck/sumcheck_aux.hpp"

namespace libiop {

template<typename FieldT>
class sumcheck_constraint_oracle;

/* Rational Sumcheck protocol from [COS19] */
template<typename FieldT>
class rational_sumcheck_protocol {
protected:
    iop_protocol<FieldT> &IOP_;
    domain_handle summation_domain_handle_;
    domain_handle codeword_domain_handle_;
    size_t numerator_degree_bound_;
    size_t denominator_degree_bound_;

    oracle_handle_ptr numerator_handle_;
    oracle_handle_ptr denominator_handle_;

    oracle_handle reextended_oracle_handle_;
    virtual_oracle_handle constraint_oracle_handle_;

    field_subset<FieldT> summation_domain_;
    field_subset<FieldT> codeword_domain_;

    std::size_t summation_domain_size_;
    std::size_t reextended_oracle_degree_; /* Equal to |H| - 1 */
    std::size_t constraint_oracle_degree_;

    verifier_random_message_handle challenge_handle_;
    const field_subset_type field_subset_type_;

    std::shared_ptr<sumcheck_constraint_oracle<FieldT> > constraint_oracle_;
    FieldT claimed_sum_;
public:
    /* Initialization and registration */
    rational_sumcheck_protocol(iop_protocol<FieldT> &IOP,
                               const domain_handle &summation_domain_handle,
                               const domain_handle &codeword_domain_handle,
                               const std::size_t numerator_degree_bound,
                               const std::size_t denominator_degree_bound,
                               const field_subset_type domain_type);

    void register_summation_oracle(const oracle_handle_ptr &numerator,
                                   const oracle_handle_ptr &denominator);
    void register_proof();

    /* Proving */
    /** Since the summation domain is often the systematic domain,
     *  these values can typically be constructed more efficiently within the protocol. */
    void calculate_and_submit_proof(
        const std::vector<FieldT> &rational_function_over_summation_domain);
    FieldT get_claimed_sum() const;

    /* Verification */
    void construct_verifier_state(const FieldT claimed_sum);

    oracle_handle_ptr get_reextended_oracle_handle() const;
    oracle_handle_ptr get_constraint_oracle_handle() const;
    std::vector<oracle_handle_ptr> get_all_oracle_handles() const;
};

} // namespace libiop

#include "libiop/protocols/encoded/sumcheck/rational_sumcheck.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_RATIONAL_SUMCHECK_HPP_
