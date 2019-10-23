/**@file
 *****************************************************************************
 Interfaces for Interleaved Lincheck, with oracle target (the target is given
 as an oracle, not a public vector). Tests that messages encoded by interleaved
 RS codes satisfy a given linear relation.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_ENCODED_LIGERO_INTERLEAVED_LINCHECK_OT_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_LIGERO_INTERLEAVED_LINCHECK_OT_HPP_

#include <cstddef>
#include <memory>
#include <vector>

#include "libiop/algebra/fields/utils.hpp"
#include "libiop/algebra/field_subset/subgroup.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/relations/r1cs.hpp"

namespace libiop {

template<typename FieldT>
class interleaved_lincheck_ot_protocol {
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

    const std::size_t num_oracles_input_;
    const std::size_t num_oracles_target_;
    const std::size_t num_queries_;
    const std::size_t num_interactions_; // sigma in Ligero paper

    const bool make_zk_;
    const field_subset_type field_subset_type_;

    const naive_sparse_matrix<FieldT> constraint_matrix_;

    std::vector<verifier_random_message_handle> random_linear_combination_handles_;
    std::vector<prover_message_handle> response_handles_;

    std::vector<oracle_handle_ptr> input_vector_row_oracle_handles_;
    std::vector<oracle_handle_ptr> target_vector_row_oracle_handles_;
    std::vector<oracle_handle_ptr> blinding_vector_row_oracle_handles_;

    std::vector<random_query_position_handle> query_position_handles_;
    std::vector<std::vector<query_handle>> input_vector_row_oracles_by_position_handles_;
    std::vector<std::vector<query_handle>> target_vector_row_oracles_by_position_handles_;
    std::vector<std::vector<query_handle>> blinding_vector_row_oracles_by_position_handles_;
public:
    /* Initialization and registration */
    interleaved_lincheck_ot_protocol(iop_protocol<FieldT> &IOP,
                                     const domain_handle &codeword_domain_handle,
                                     const domain_handle &systematic_domain_handle,
                                     const domain_handle &extended_systematic_domain_handle,
                                     const std::size_t num_oracles_input,
                                     const std::size_t num_oracles_target,
                                     const std::size_t num_queries,
                                     const std::size_t num_interactions,
                                     const bool make_zk,
                                     const field_subset_type domain_type,
                                     const naive_sparse_matrix<FieldT> constraint_matrix);
    void attach_input_vector_row_oracles(const std::vector<oracle_handle_ptr> &handles);
    void attach_target_vector_row_oracles(const std::vector<oracle_handle_ptr> &handles);
    void attach_blinding_vector_row_oracles(const std::vector<oracle_handle_ptr> &handles);
    void register_linear_combinations();
    void register_responses();

    void register_queries();
    void register_queries_for_given_positions(std::vector<random_query_position_handle> query_position_handles);

    std::vector<std::vector<FieldT>> all_random_linear_combinations();

    /* Proving */
    void calculate_and_submit_responses();
    void calculate_and_submit_responses(const std::vector<FieldT> &supplementary_input,
                                        const std::size_t supplementary_input_size,
                                        const std::vector<FieldT> &supplementary_target,
                                        const std::size_t supplementary_target_size,
                                        std::vector<std::vector<FieldT>> random_linear_combinations);

    std::vector<FieldT> all_query_points();
    std::vector<std::vector<FieldT>> lagrange_coefficients_for_query_points(std::vector<FieldT> query_points);

    /* Verification */
    bool verifier_predicate();
    bool verifier_predicate(const std::vector<FieldT> &supplementary_input,
                            const std::size_t supplementary_input_size,
                            const std::vector<FieldT> &supplementary_target,
                            const std::size_t supplementary_target_size,
                            std::vector<std::vector<FieldT>> random_linear_combinations,
                            std::vector<std::vector<FieldT>> lagrange_coefficients);
};

} // namespace libiop

#include "libiop/protocols/encoded/ligero/interleaved_lincheck_ot.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_LIGERO_INTERLEAVED_LINCHECK_OT_HPP_
