/**@file
 *****************************************************************************
 Interfaces for Interleaved Rowcheck, with oracle target (the target is given
 as an oracle, not a public vector). Tests that messages encoded by
 interleaved RS codes satisfy a given quadratic equation. Based on
 Test-Quadratic-Constraints from Ligero protocol [ACIV17], section 4.3.

 Specifically, checks that oracles of presumed Reed-Solomon codewords,
 encoding messages x, y, and z, satisfy x ⦿ y + a ⦿ z = b, for given a and b
 (where ⦿ denotes pointwise product).
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_ENCODED_LIGERO_INTERLEAVED_ROWCHECK_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_LIGERO_INTERLEAVED_ROWCHECK_HPP_

#include <cstddef>
#include <memory>
#include <vector>

#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/iop/iop.hpp"

namespace libiop {

template<typename FieldT>
class interleaved_rowcheck_protocol {
protected:
    iop_protocol<FieldT> &IOP_;
    domain_handle codeword_domain_handle_;
    domain_handle systematic_domain_handle_;

    field_subset<FieldT> codeword_domain_;
    field_subset<FieldT> systematic_domain_;
    field_subset<FieldT> extended_systematic_domain_;

    std::size_t codeword_domain_size_;
    std::size_t systematic_domain_size_;

    std::size_t response_size_;

    std::size_t num_oracles_;
    std::size_t num_queries_;

    std::size_t num_interactions_; // sigma in Ligero paper

    const bool make_zk_;
    const field_subset_type field_subset_type_;

    std::vector<verifier_random_message_handle> random_linear_combination_handles_;
    std::vector<prover_message_handle> response_handles_;

    std::vector<oracle_handle_ptr> x_vector_row_oracle_handles_;
    std::vector<oracle_handle_ptr> y_vector_row_oracle_handles_;
    std::vector<oracle_handle_ptr> z_vector_row_oracle_handles_;

    /* Blinding vectors should encode 0 vector over systematic subspace */
    std::vector<oracle_handle_ptr> blinding_vector_row_oracle_handles_;

    std::vector<random_query_position_handle> query_position_handles_;
    std::vector<std::vector<query_handle>> x_vector_row_oracles_by_position_handles_;
    std::vector<std::vector<query_handle>> y_vector_row_oracles_by_position_handles_;
    std::vector<std::vector<query_handle>> z_vector_row_oracles_by_position_handles_;
    std::vector<std::vector<query_handle>> blinding_vector_row_oracles_by_position_handles_;
public:
    /* Initialization and registration */
    interleaved_rowcheck_protocol(iop_protocol<FieldT> &IOP,
                                  const domain_handle &codeword_domain_handle,
                                  const domain_handle &systematic_domain_handle,
                                  const domain_handle &extended_systematic_domain_handle,
                                  const std::size_t num_oracles,
                                  const std::size_t num_queries,
                                  const std::size_t num_interactions,
                                  const bool make_zk,
                                  const field_subset_type domain_type);
    void attach_vector_row_oracles(const std::vector<oracle_handle_ptr> &x_handles,
                                   const std::vector<oracle_handle_ptr> &y_handles,
                                   const std::vector<oracle_handle_ptr> &z_handles);
    void attach_blinding_vector_row_oracles(const std::vector<oracle_handle_ptr> &handles);
    void register_linear_combinations();
    void register_responses();

    void register_queries();
    void register_queries_for_given_positions(std::vector<random_query_position_handle> query_position_handles);

    /* Proving */
    void calculate_and_submit_responses();

    /* Verification */
    bool verifier_predicate();
};

} // namespace libiop

#include "libiop/protocols/encoded/ligero/interleaved_rowcheck.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_LIGERO_INTERLEAVED_ROWCHECK_HPP_
