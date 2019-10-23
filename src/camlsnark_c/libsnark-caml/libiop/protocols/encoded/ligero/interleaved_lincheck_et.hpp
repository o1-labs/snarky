/**@file
*****************************************************************************
Interfaces for Interleaved Lincheck, with explicit target (here, the target is
given as a public vector, rather than an oracle). Tests a message encoded by
an interleaved RS code satisfies a given set of linear constraints. Based on
Test-Linear-Constraints from Ligero protocol [ACIV17], section 4.2.
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_ENCODED_LIGERO_INTERLEAVED_LINCHECK_ET_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_LIGERO_INTERLEAVED_LINCHECK_ET_HPP_

#include <cstddef>
#include <memory>
#include <vector>

#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/relations/r1cs.hpp"

namespace libiop {

template<typename FieldT>
class interleaved_lincheck_et_protocol {
protected:
    iop_protocol<FieldT> &IOP_;
    domain_handle codeword_domain_handle_;
    domain_handle systematic_domain_handle_;
    domain_handle extended_systematic_domain_handle_;

    field_subset<FieldT> codeword_domain_;
    field_subset<FieldT> systematic_domain_;
    field_subset<FieldT> extended_systematic_domain_;

    std::size_t codeword_domain_size_;
    std::size_t systematic_domain_size_;
    std::size_t extended_systematic_domain_size_;

    std::size_t response_size_;

    std::size_t num_oracles_;
    std::size_t num_queries_;
    std::size_t num_interactions_;

    bool make_zk_;
    field_subset_type field_subset_type_;

    const naive_sparse_matrix<FieldT> constraint_matrix_;
    const std::vector<FieldT> target_vector_;

    std::vector<verifier_random_message_handle> random_linear_combination_handles_;
    std::vector<prover_message_handle> response_handles_;

    std::vector<oracle_handle_ptr> input_vector_row_oracle_handles_;
    std::vector<oracle_handle_ptr> blinding_vector_row_oracle_handles_;

    std::vector<random_query_position_handle> query_position_handles_;
    std::vector<std::vector<query_handle>> input_vector_row_oracles_by_position_handles_;
    std::vector<std::vector<query_handle>> blinding_vector_row_oracles_by_position_handles_;
public:
    /* Initialization and registration */
    interleaved_lincheck_et_protocol(iop_protocol<FieldT> &IOP,
                                     const domain_handle &codeword_domain,
                                     const domain_handle &systematic_domain,
                                     const domain_handle &extended_systematic_domain,
                                     const std::size_t num_oracles,
                                     const std::size_t num_queries,
                                     const std::size_t num_interactions,
                                     const bool make_zk,
                                     const field_subset_type domain_type,
                                     const naive_sparse_matrix<FieldT> constraint_matrix,
                                     const std::vector<FieldT> target_vector);
    void attach_input_vector_row_oracles(const std::vector<oracle_handle_ptr> &handles);
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

#include "libiop/protocols/encoded/ligero/interleaved_lincheck_et.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_LIGERO_INTERLEAVED_LINCHECK_ET_HPP_
