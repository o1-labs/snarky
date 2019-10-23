/**@file
 *****************************************************************************
 Dummy protocol for use in BCS transformation testing.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef LIBIOP_PROTOCOLS_ENCODED_DUMMY_PROTOCOL_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_DUMMY_PROTOCOL_HPP_

#include <cstddef>
#include <memory>
#include <vector>

#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/iop/iop.hpp"

namespace libiop {

template<typename FieldT>
class dummy_protocol {
protected:
    iop_protocol<FieldT> &IOP_;
    size_t num_oracles_per_round_;
    size_t num_rounds_;
    domain_handle codeword_domain_handle_;
    /** Since this is only used with the BCS transformation,
     *  we only make 1 oracle zk as this _ought_ to be equivalent to all oracles for this domain
     *  being zk, and so tests will catch if this is not the case. */
    bool make_zk_;
    bool holographic_;

    field_subset<FieldT> codeword_domain_;
    size_t codeword_domain_size_;

   std::vector<std::vector<oracle_handle_ptr>> oracle_handle_ptrs_;
   std::vector<verifier_random_message_handle> verifier_msg_handles_;

public:
    /* Initialization and registration */
    dummy_protocol(iop_protocol<FieldT> &IOP,
                   const size_t num_oracles_per_round,
                   const size_t num_rounds,
                   const std::vector<round_parameters<FieldT>> round_params,
                   const domain_handle codeword_domain_handle,
                   const bool make_zk,
                   const bool holographic);

    /* Indexing */
    void calculate_index();

    /* Proving */
    void calculate_and_submit_response();

    /* Verification */
    bool check_eval_at_point(const size_t round_index,
                             const size_t oracle_index,
                             const size_t eval_pos,
                             const FieldT eval);

    std::vector<oracle_handle_ptr> get_oracle_handles_for_round(const size_t round);
};

} // namespace libiop

#include "libiop/tests/snark/dummy_bcs_protocol.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_DUMMY_PROTOCOL_HPP_
