/**@file
 *****************************************************************************
 Ligero protocol for R1CS (from [ACIV17])
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_ENCODED_LIGERO_LIGERO_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_LIGERO_LIGERO_HPP_

#include <cstddef>
#include <memory>
#include <vector>

#include "libiop/iop/iop.hpp"
#include "libiop/relations/r1cs.hpp"

#include "libiop/algebra/fields/utils.hpp"
#include "libiop/algebra/field_subset/subgroup.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/protocols/encoded/ligero/interleaved_lincheck_ot.hpp"
#include "libiop/protocols/encoded/ligero/interleaved_rowcheck.hpp"

namespace libiop {

struct encoded_ligero_parameters {
    std::size_t num_interaction_phase_repetitions_;
    std::size_t num_query_phase_repetitions_;
    bool make_zk_;
    field_subset_type field_subset_type_;

    std::size_t matrix_width_;
    std::size_t matrix_height_;
    std::size_t num_oracles_input_;
    std::size_t num_oracles_vectors_;
};

template<typename FieldT>
class interleaved_r1cs_protocol {
protected:
    iop_protocol<FieldT> &IOP_;
    r1cs_constraint_system<FieldT> constraint_system_;
    encoded_ligero_parameters parameters_;

    domain_handle codeword_domain_handle_;
    domain_handle systematic_domain_handle_;
    domain_handle extended_systematic_domain_handle_;

    field_subset<FieldT> codeword_domain_;
    field_subset<FieldT> systematic_domain_;
    field_subset<FieldT> extended_systematic_domain_;

    std::size_t codeword_domain_size_;
    std::size_t systematic_domain_size_;
    std::size_t extended_systematic_domain_size_;

    std::shared_ptr<interleaved_lincheck_ot_protocol<FieldT> > lincheck_A_;
    std::shared_ptr<interleaved_lincheck_ot_protocol<FieldT> > lincheck_B_;
    std::shared_ptr<interleaved_lincheck_ot_protocol<FieldT> > lincheck_C_;
    std::shared_ptr<interleaved_rowcheck_protocol<FieldT> > rowcheck_;

    std::size_t RS_extra_dimensions_;

    std::size_t matrix_height_;
    std::size_t matrix_width_;

    std::size_t num_oracles_input_;
    std::size_t num_oracles_vectors_;
    std::size_t num_queries_;
    std::size_t num_interactions_;

    bool make_zk_;
    field_subset_type field_subset_type_;

    std::size_t encoding_independence_;

    std::vector<oracle_handle_ptr> w_vector_handles_;
    std::vector<oracle_handle_ptr> a_vector_handles_;
    std::vector<oracle_handle_ptr> b_vector_handles_;
    std::vector<oracle_handle_ptr> c_vector_handles_;

    std::vector<oracle_handle_ptr> concatenated_vector_handles_;

    std::vector<oracle_handle_ptr> lincheck_A_blinding_vector_handles_;
    std::vector<oracle_handle_ptr> lincheck_B_blinding_vector_handles_;
    std::vector<oracle_handle_ptr> lincheck_C_blinding_vector_handles_;
    std::vector<oracle_handle_ptr> rowcheck_blinding_vector_handles_;

    naive_sparse_matrix<FieldT> A_matrix_;
    naive_sparse_matrix<FieldT> B_matrix_;
    naive_sparse_matrix<FieldT> C_matrix_;
public:
    /* Initialization and registration */
    interleaved_r1cs_protocol(iop_protocol<FieldT> &IOP,
                              const domain_handle &codeword_domain_handle,
                              const domain_handle &systematic_domain_handle,
                              const domain_handle &extended_systematic_domain_handle,
                              const r1cs_constraint_system<FieldT> &constraint_system,
                              const encoded_ligero_parameters &parameters);

    std::vector<oracle_handle_ptr> concatenated_vector_handles() const;

    void attach_oracles();

    void register_linear_combinations();
    void register_responses();

    void register_queries();

    /* Proving */
    void submit_witness_oracles(const r1cs_primary_input<FieldT> &primary_input,
                                const r1cs_auxiliary_input<FieldT> &auxiliary_input);
    void submit_blinding_vector_oracles();
    void calculate_and_submit_proof(const r1cs_primary_input<FieldT> &primary_input);

    /* Verification */
    bool verifier_predicate(const r1cs_primary_input<FieldT> &primary_input);

protected:
    void submit_zero_sum_blinding_vector(const oracle_handle_ptr &handle);
    void submit_zero_blinding_vector(const oracle_handle_ptr &handle);
};

} // namespace libiop

#include "libiop/protocols/encoded/ligero/ligero.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_LIGERO_LIGERO_HPP_

