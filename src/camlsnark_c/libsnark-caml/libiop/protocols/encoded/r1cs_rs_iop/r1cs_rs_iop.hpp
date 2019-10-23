/**@file
*****************************************************************************
RS-encoded Aurora protocol for R1CS
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_ENCODED_AURORA_AURORA_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_AURORA_AURORA_HPP_

#include <cstddef>
#include <functional>
#include <memory>
#include <vector>
#include "gtest/gtest_prod.h"

#include "libiop/iop/iop.hpp"
#include "libiop/protocols/encoded/lincheck/basic_lincheck.hpp"
#include "libiop/protocols/encoded/lincheck/holographic_lincheck.hpp"
#include "libiop/protocols/encoded/common/rowcheck.hpp"
#include "libiop/relations/r1cs.hpp"

namespace libiop {

/** Notation key
 *   L         : Codeword domain - The domain which the codewords lie in.
 *   H_1       : Constraint domain - identifies every constraint uniquely.
 *   H_2       : Variable domain - identifies each variable uniquely,
 *               The input variables occur at the first k locations.
 *  H_1 U H_2  : Summation domain - This is the domain that multi_lincheck has sumcheck sum over.
 *               Only used in this file insofar as is needed for parameterization.
 *  H_2^{<= k} : Input variable domain - the first k elements of the variable domain,
 *               identifies the primary input of the R1CS instance.
 *  multi_lincheck : Runs n different lincheck instances together.
 *                   This is useful from an abstraction point of view, and an efficiency point of view.
 *                   See lincheck/multi_lincheck.md which describes the efficiency gain.
 *  fw, fz,
 *  fAz, fBz, fCz : These are all as described in the paper.
 *                  In particular they are low degree extensions of w, z, Az, etc. to corresponding correct domain.
 *                  In the case of fw, its not quite the low degree extension of w, but instead slightly adjusted
 *                  so that the evaluations at the witness positions work out correctly when constructing fz.
 */

template<typename FieldT>
class fz_virtual_oracle;

template<typename FieldT>
class encoded_aurora_parameters {
protected:
    size_t interactive_security_parameter_;

    size_t codeword_domain_dim_;
    size_t constraint_domain_dim_;
    size_t summation_domain_dim_;
    size_t query_bound_;
    bool make_zk_;
    bool holographic_;
    field_subset_type domain_type_;

public:
    encoded_aurora_parameters() {};
    encoded_aurora_parameters(const size_t interactive_security_parameter,
                              const size_t codeword_domain_dim,
                              const size_t constraint_domain_dim,
                              const size_t summation_domain_dim,
                              const size_t query_bound,
                              const bool make_zk,
                              const bool holographic,
                              const field_subset_type type);

    size_t max_tested_degree_bound() const;
    size_t max_constraint_degree_bound() const;
    std::vector<size_t> locality_vector() const;

    bool make_zk() const;
    bool holographic() const;
    size_t query_bound() const;

    void print();

    basic_lincheck_parameters<FieldT> multi_lincheck_params_;
    holographic_lincheck_parameters<FieldT> holographic_lincheck_params_;
};

template<typename FieldT>
class encoded_aurora_protocol {
protected:
    iop_protocol<FieldT> &IOP_;
    domain_handle constraint_domain_handle_;
    domain_handle variable_domain_handle_;
    domain_handle codeword_domain_handle_;
    std::shared_ptr<r1cs_constraint_system<FieldT>> constraint_system_;
    encoded_aurora_parameters<FieldT> params_;

    field_subset<FieldT> constraint_domain_,
        variable_domain_,
        codeword_domain_,
        input_variable_domain_; /* mostly for convenience to avoid calling this->IOP_.get_domain(..._handle) every time */

    oracle_handle fw_handle_;
    oracle_handle fAz_handle_, fBz_handle_, fCz_handle_;

    std::shared_ptr<holographic_multi_lincheck<FieldT> > holographic_multi_lincheck_;
    std::shared_ptr<multi_lincheck<FieldT> > multi_lincheck_;

    std::size_t fw_mask_degree_;
    polynomial<FieldT> fw_mask_;
    std::vector<FieldT> f_1v_coefficients_;
    std::vector<FieldT> fw_over_codeword_domain_;

    polynomial<FieldT> R_Az_, R_Bz_, R_Cz_;
    std::vector<FieldT> fprime_Az_over_codeword_domain_,
        fprime_Bz_over_codeword_domain_,
        fprime_Cz_over_codeword_domain_;

    std::shared_ptr<r1cs_sparse_matrix<FieldT> > r1cs_A_, r1cs_B_, r1cs_C_;

    std::shared_ptr<fz_virtual_oracle<FieldT> > fz_oracle_;
    virtual_oracle_handle fz_oracle_handle_;

    std::shared_ptr<rowcheck_ABC_virtual_oracle<FieldT> > rowcheck_oracle_;
    virtual_oracle_handle rowcheck_oracle_handle_;

    void compute_fprime_ABCz_over_codeword_domain(
        std::vector<FieldT> &Az, std::vector<FieldT> &Bz, std::vector<FieldT> &Cz);
    void register_witness_oracles();
    FRIEND_TEST(TestAdditiveR1CSComponents, R1CSTest);
    FRIEND_TEST(TestMultiplicativeR1CSComponents, R1CSTest);

public:
    /* Initialization and registration */
    encoded_aurora_protocol(iop_protocol<FieldT> &IOP,
                            const domain_handle &constraint_domain_handle,
                            const domain_handle &variable_domain_handle,
                            const domain_handle &codeword_domain_handle,
                            const std::shared_ptr<r1cs_constraint_system<FieldT>> &constraint_system,
                            const encoded_aurora_parameters<FieldT> &params);

    /* TODO: Make two separate constructors, after we have holographic aurora params */
    void set_index_oracles(const domain_handle &indexed_domain_handle,
                           const std::vector<std::vector<oracle_handle_ptr>> indexed_handles);

    void register_challenge();
    void register_proof();

    /* Proving */
    void submit_witness_oracles(const r1cs_primary_input<FieldT> &primary_input,
                                const r1cs_auxiliary_input<FieldT> &auxiliary_input);
    void calculate_and_submit_proof();

    /* Verification */
    void construct_verifier_state(const r1cs_primary_input<FieldT> &primary_input);

    void print_soundness_error(const std::size_t LDT_query_repetitions,
                               const std::size_t RS_extra_dimensions);

    std::vector<oracle_handle_ptr> get_all_oracle_handles();
};

} // namespace libiop

#include "libiop/protocols/encoded/r1cs_rs_iop/r1cs_rs_iop.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_AURORA_AURORA_HPP_
