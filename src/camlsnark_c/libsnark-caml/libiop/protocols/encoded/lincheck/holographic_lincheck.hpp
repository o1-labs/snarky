/**@file
*****************************************************************************
holographic multi lincheck protocol
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_ENCODED_LINCHECK_HOLOGRAPHIC_LINCHECK_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_LINCHECK_HOLOGRAPHIC_LINCHECK_HPP_

#include <cstring>
#include <cstddef>
#include <map>
#include <memory>
#include <vector>

#include "libiop/algebra/lagrange.hpp"
#include "libiop/algebra/fft.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/iop/utilities/batching.hpp"
#include "libiop/protocols/encoded/r1cs_rs_iop/fractal_indexer.hpp"
#include "libiop/protocols/encoded/common/boundary_constraint.hpp"
#include "libiop/protocols/encoded/common/rational_linear_combination.hpp"
#include "libiop/protocols/encoded/lincheck/holographic_lincheck_aux.hpp"
#include "libiop/protocols/encoded/sumcheck/sumcheck.hpp"
#include "libiop/protocols/encoded/sumcheck/rational_sumcheck.hpp"
#include "libiop/relations/sparse_matrix.hpp"

namespace libiop {


template<typename FieldT>
class holographic_lincheck_parameters {
protected:
    size_t interactive_security_parameter_;
    size_t constraint_domain_dim_;
    bool make_zk_;
    field_subset_type domain_type_;

    size_t num_repetitions_;

    /** TODO: Eventually parameterize number of separate sumcheck instances separately. */
    bool override_security_parameter_ = false;
public:
    holographic_lincheck_parameters() {};
    holographic_lincheck_parameters(const size_t interactive_security_parameter,
                                          const size_t constraint_domain_dim_,
                                          const bool make_zk,
                                          const field_subset_type domain_type);

    /** UNSAFE!
     *  This is intended to allow experimentation with multi lincheck parameterizations. */
    void override_security_parameter(const size_t multi_lincheck_repetitions);

    size_t num_repetitions() const;
    bool make_zk() const;
    field_subset_type domain_type() const;

    size_t locality() const;
    long double achieved_interactive_soundness() const;

    void print() const;
};

template<typename FieldT>
class holographic_multi_lincheck {
protected:
    iop_protocol<FieldT> &IOP_;
    const domain_handle codeword_domain_handle_;
    const domain_handle summation_domain_handle_;
    const size_t input_variable_dim_;
    const std::vector<std::shared_ptr<sparse_matrix<FieldT>>> matrices_;
    const std::size_t num_matrices_;
    const holographic_lincheck_parameters<FieldT> params_;

    domain_handle index_domain_handle_;

    field_subset<FieldT> codeword_domain_;
    field_subset<FieldT> summation_domain_;
    field_subset<FieldT> index_domain_;
    std::size_t lincheck_degree_;
    std::vector<oracle_handle_ptr> constituent_oracle_handles_;

    /** We use independent randomness for each lincheck repetition,
        hence these are all vectors of random messages. */
    std::vector<verifier_random_message_handle> alpha_handle_;
    std::vector<verifier_random_message_handle> random_coefficient_handle_; /* each message holds num_matrices elements */
    std::vector<verifier_random_message_handle> beta_handle_;

    std::vector<prover_message_handle> M_at_alpha_beta_;

    std::vector<std::vector<FieldT>> r_Mz_;
    std::vector<lagrange_polynomial<FieldT>> p_alpha_;
    std::vector<std::vector<FieldT>> p_alpha_over_H_;

    std::vector<polynomial<FieldT>> p_alpha_M_poly_;
    std::vector<oracle_handle> t_oracle_handle_;
    std::vector<virtual_oracle_handle> t_boundary_constraint_handle_;

    std::vector<std::vector<oracle_handle_ptr>> matrix_numerator_handles_;
    std::vector<std::vector<oracle_handle_ptr>> matrix_denominator_handles_;

    std::vector<std::shared_ptr<holographic_multi_lincheck_virtual_oracle<FieldT>>>
        multi_lincheck_virtual_oracle_;
    std::vector<std::shared_ptr<single_boundary_constraint<FieldT>>>
        t_boundary_constraint_;
    std::vector<std::vector<std::shared_ptr<single_matrix_denominator<FieldT>>>>
        matrix_denominators_;
    std::vector<std::shared_ptr<rational_linear_combination<FieldT>>>
        rational_linear_combination_;
    std::vector<std::shared_ptr<batch_sumcheck_protocol<FieldT>>>
        sumcheck_H_;
    std::vector<std::shared_ptr<rational_sumcheck_protocol<FieldT>>>
        sumcheck_K_;

    void set_rational_linear_combination_coefficients();
    void set_matrix_denominator_challenges();
public:
    holographic_multi_lincheck(
        iop_protocol<FieldT> &IOP,
        const domain_handle &codeword_domain_handle,
        const domain_handle &summation_domain_handle,
        const std::size_t input_variable_dim,
        const std::vector<std::shared_ptr<sparse_matrix<FieldT> >> matrices,
        const oracle_handle_ptr fz_handle,
        const std::vector<oracle_handle_ptr> Mz_handles,
        const holographic_lincheck_parameters<FieldT> params);

    /** Expects indexed oracle handles such that:
     *  indexed_handles[i] = {row_M, col_M, val_M}, where M = matrices[i]
     */
    void set_index_oracles(const domain_handle &indexed_domain_handle,
                           const std::vector<std::vector<oracle_handle_ptr>> indexed_handles);

    /** For brevity we give letter names to several components of the protocol
     *  alpha is the randomness used to reduce lincheck to sumcheck
     *  t is the oracle that the prover claims to be p_M(X, alpha)
     *  beta is the randomness used to reduce checking that t is valid
     *       to querying M'(beta, alpha),
     *       where M' is essentially a "shifted" version of the transpose of M.
     *
     */
    void register_challenge_alpha();
    void register_response_alpha();
    void register_challenge_beta();
    void register_response_beta();

    void submit_sumcheck_masking_polynomials();
    void calculate_response_alpha();
    void calculate_response_beta();
    void construct_verifier_state();
    std::vector<oracle_handle_ptr> get_all_oracle_handles_for_repetition(const size_t repetition);
    std::vector<oracle_handle_ptr> get_all_oracle_handles();
};

} // libiop

#include "libiop/protocols/encoded/lincheck/holographic_lincheck.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_LINCHECK_HOLOGRAPHIC_LINCHECK_HPP_
