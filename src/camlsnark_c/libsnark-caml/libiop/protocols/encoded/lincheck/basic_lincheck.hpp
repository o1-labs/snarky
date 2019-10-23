/**@file
*****************************************************************************
Basic R1CS multi lincheck oracle, from the Aurora paper.
This has a linear time verifier,
as it does not exploit succinct matrices or holography.
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_ENCODED_LINCHECK_BASIC_LINCHECK_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_LINCHECK_BASIC_LINCHECK_HPP_

#include <cstring>
#include <cstddef>
#include <map>
#include <memory>
#include <vector>

#include "libiop/algebra/lagrange.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/iop/utilities/batching.hpp"
#include "libiop/protocols/encoded/lincheck/basic_lincheck_aux.hpp"
#include "libiop/protocols/encoded/sumcheck/sumcheck.hpp"
#include "libiop/relations/sparse_matrix.hpp"

namespace libiop {

template<typename FieldT>
class basic_lincheck_parameters {
protected:
    size_t interactive_security_parameter_;
    size_t constraint_domain_dim_;
    bool make_zk_;
    field_subset_type domain_type_;

    /** TODO: Eventually parameterize number of separate sumcheck instances separately. */
    size_t multi_lincheck_repetitions_;
    bool override_security_parameter_ = false;
public:
    basic_lincheck_parameters() {};
    basic_lincheck_parameters(const size_t interactive_security_parameter,
                              const size_t constraint_domain_dim_,
                              const bool make_zk,
                              const field_subset_type domain_type);

    /** UNSAFE!
     *  This is intended to allow experimentation with multi lincheck parameterizations. */
    void override_security_parameter(const size_t multi_lincheck_repetitions);

    size_t multi_lincheck_repetitions() const;
    bool make_zk() const;
    field_subset_type domain_type() const;

    size_t locality() const;
    long double achieved_interactive_soundness() const;

    void print() const;
};

template<typename FieldT>
class multi_lincheck {
protected:
    iop_protocol<FieldT> &IOP_;
    const domain_handle codeword_domain_handle_;
    const domain_handle constraint_domain_handle_;
    const domain_handle variable_domain_handle_;
    const std::size_t num_matrices_;
    const basic_lincheck_parameters<FieldT> params_;

    domain_handle summation_domain_handle_;

    std::size_t lincheck_degree_;
    std::vector<oracle_handle_ptr> constituent_oracle_handles_;

    std::vector<verifier_random_message_handle> alpha_handles_; /* holds multi_lincheck_repetitions elements */
    std::vector<verifier_random_message_handle> random_coefficient_handles_; /* holds num_matrices * multi_lincheck_repetitions elements */

    std::vector<std::shared_ptr<multi_lincheck_virtual_oracle<FieldT>>> multi_lincheck_virtual_oracles_;
    // Currently we reduce every multi lincheck instance to its own sumcheck instance
    std::vector<std::shared_ptr<batch_sumcheck_protocol<FieldT>>> sumchecks_;

public:
    multi_lincheck(iop_protocol<FieldT> &IOP,
                   const domain_handle &codeword_domain_handle,
                   const domain_handle &constraint_domain_handle,
                   const domain_handle &variable_domain_handle,
                   const std::size_t input_variable_dim,
                   const std::vector<std::shared_ptr<sparse_matrix<FieldT> >> matrices,
                   const oracle_handle_ptr fz_handle,
                   const std::vector<oracle_handle_ptr> Mz_handles,
                   const basic_lincheck_parameters<FieldT> params);

    void register_challenge();
    void register_proof();

    void submit_sumcheck_masking_polynomials();
    void calculate_and_submit_proof(); /* Set verifier_random_message */
    void construct_verifier_state();
    std::vector<oracle_handle_ptr> get_all_oracle_handles();
};

} // libiop

#include "libiop/protocols/encoded/lincheck/basic_lincheck.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_LINCHECK_BASIC_LINCHECK_HPP_
