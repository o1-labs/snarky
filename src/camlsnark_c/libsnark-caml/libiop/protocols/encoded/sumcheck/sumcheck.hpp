/**@file
*****************************************************************************
Polynomial Sumcheck interfaces.
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_ENCODED_SUMCHECK_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_SUMCHECK_HPP_

#include <cstddef>
#include <memory>
#include <stdexcept>
#include <vector>

#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/protocols/encoded/common/random_linear_combination.hpp"
#include "libiop/protocols/encoded/sumcheck/sumcheck_aux.hpp"

namespace libiop {

template<typename FieldT>
class sumcheck_g_oracle;

template<typename FieldT>
class batch_sumcheck_protocol {
protected:
    iop_protocol<FieldT> &IOP_;
    domain_handle summation_domain_handle_; /* In [BCRSVW18] this is referred to as H */
    domain_handle codeword_domain_handle_;  /* In [BCRSVW18] this is referred to as L */
    std::size_t degree_bound_;              /* In [BCRSVW18] this is referred to as d */

    /* In [BCRSVW18] the masking polynomial is the polynomial corresponding to the randomly
       chosen RS codeword q.
       We sample the masking polynomial as a random polynomial that sums to 0 over H,
       as this doesn't impact zero knowledge,
       but is computationally cheap and allows us to remove that prover message */
    oracle_handle masking_poly_handle_;
    polynomial<FieldT> masking_poly_;
    oracle<FieldT> masking_poly_oracle_;

    /* In [BCRSVW18] this is referred to as c. It is used to scale the polynomial in the
       zero knowledge sum check variant. */
    verifier_random_message_handle challenge_handle_;

    /* f = g + Z_H * h */
    oracle_handle h_handle_;
    virtual_oracle_handle g_handle_;

    field_subset<FieldT> summation_domain_;
    field_subset<FieldT> codeword_domain_;

    std::size_t summation_domain_size_;
    std::size_t g_degree_; /* Equal to |H| - 1 */
    std::size_t h_degree_; /* Equal to degree of input polynomial - |H| */

    const bool make_zk_;
    const field_subset_type field_subset_type_;

    std::map<size_t, size_t> oracle_uid_to_registration_index_;
    std::vector<oracle_handle_ptr> oracle_handles_;
    std::vector<FieldT> claimed_sums_;

    std::shared_ptr<random_linear_combination_oracle<FieldT> > combined_f_oracle_;
    std::shared_ptr<sumcheck_g_oracle<FieldT> > g_oracle_;
    virtual_oracle_handle combined_f_oracle_handle_;

    FieldT get_combined_claimed_sum(std::vector<FieldT> challenge);
public:
    /* Initialization and registration */
    batch_sumcheck_protocol(iop_protocol<FieldT> &IOP,
                            const domain_handle &summation_domain,
                            const domain_handle &codeword_domain,
                            const std::size_t degree_bound,
                            const bool make_zk,
                            const field_subset_type domain_type);
    void register_masking_polynomial();
    void register_challenge();
    void attach_oracle_for_summing(const oracle_handle_ptr &handle,
                                   const FieldT claimed_sum = FieldT::zero());
    void register_proof();

    /* For use when the prover sends the oracle's sum during the protocol */
    void set_oracle_claimed_sum(const oracle_handle_ptr &handle,
                                const FieldT claimed_sum);

    oracle_handle get_masking_poly_oracle_handle() const;
    oracle_handle get_h_oracle_handle() const;
    virtual_oracle_handle get_g_oracle_handle() const;

    /* Proving */
    void submit_masking_polynomial();  /* Set the masking polynomial*/
    void calculate_and_submit_proof(); /* Set verifier_random_message */

    /* Verification */
    void construct_verifier_state();

    std::vector<oracle_handle_ptr> get_all_oracle_handles();
};

} // namespace libiop

#include "libiop/protocols/encoded/sumcheck/sumcheck.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_SUMCHECK_HPP_
