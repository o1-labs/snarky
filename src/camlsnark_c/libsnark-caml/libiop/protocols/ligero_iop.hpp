/**@file
*****************************************************************************
Full Ligero protocol for R1CS (encoded R1CS + trivial LDT)
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_LIGERO_IOP_HPP_
#define LIBIOP_PROTOCOLS_LIGERO_IOP_HPP_

#include <cstddef>
#include <functional>
#include <memory>
#include <vector>

#include "libiop/iop/iop.hpp"
#include "libiop/protocols/encoded/ligero/ligero.hpp"
#include "libiop/protocols/ldt/ldt_reducer.hpp"
#include "libiop/protocols/ldt/direct_ldt/direct_ldt.hpp"
#include "libiop/relations/r1cs.hpp"

namespace libiop {

template<typename FieldT>
class ligero_iop_parameters {
    protected:
    std::size_t security_parameter_;
    LDT_reducer_soundness_type soundness_type_;
    size_t RS_extra_dimensions_;
    float height_width_ratio_;
    bool make_zk_;
    field_subset_type domain_type_;
    size_t num_constraints_;
    size_t num_variables_;

    size_t absolute_encoded_ligero_proximity_parameter_;
    long double fractional_encoded_ligero_proximity_parameter_;

    size_t systematic_domain_dim_;
    size_t codeword_domain_dim_;
    size_t num_oracles_input_;
    size_t num_oracle_vectors_;
    size_t query_bound_;

    void configure_encoded_ligero_params(const size_t num_variables, const size_t num_constraints);
    void set_encoded_ligero_interactions(const size_t interactive_soundness_error);
    void set_queries(const size_t query_soundness_error, const size_t max_tested_degree);
    void calculate_encoded_ligero_proximity_parameters(const size_t query_bound);
    void set_soundness_parameters();
    public:
    ligero_iop_parameters(const size_t security_parameter,
                          const LDT_reducer_soundness_type soundness_type,
                          const size_t RS_extra_dimensions,
                          const float height_width_ratio,
                          const bool make_zk,
                          const field_subset_type domain_type,
                          const size_t num_constraints,
                          const size_t num_variables);
    size_t systematic_domain_dim() const;
    size_t RS_extra_dimensions() const;
    bool make_zk() const;
    field_subset_type domain_type() const;

    long double achieved_encoded_ligero_interactive_soundness_error() const;
    long double achieved_encoded_ligero_query_soundness_error() const;
    long double achieved_soundness() const;
    void print() const;

    LDT_instance_reducer_params<FieldT> ldt_reducer_params_;
    direct_LDT_parameters<FieldT> direct_ldt_params_;
    encoded_ligero_parameters encoded_ligero_params_;
};

template<typename FieldT>
class ligero_iop {
protected:
    iop_protocol<FieldT> &IOP_;
    r1cs_constraint_system<FieldT> constraint_system_;
    ligero_iop_parameters<FieldT> parameters_;

    field_subset<FieldT> codeword_domain_;

    std::shared_ptr<interleaved_r1cs_protocol<FieldT>> protocol_;
    std::shared_ptr<LDT_instance_reducer<FieldT, direct_LDT_protocol<FieldT>>> LDT_reducer_;
public:
    /* Initialization and registration */
    ligero_iop(iop_protocol<FieldT> &IOP,
               const r1cs_constraint_system<FieldT> &constraint_system,
               const ligero_iop_parameters<FieldT> &parameters);

    void register_interactions();
    void register_queries();

    /* Proving */
    void produce_proof(const r1cs_primary_input<FieldT> &primary_input,
                       const r1cs_auxiliary_input<FieldT> &auxiliary_input);

    /* Verification */
    bool verifier_predicate(const r1cs_primary_input<FieldT> &primary_input);
protected:
    void submit_random_blinding_vector(const oracle_handle_ptr &handle);
};


} // namespace libiop

#include "libiop/protocols/ligero_iop.tcc"

#endif // LIBIOP_PROTOCOLS_LIGERO_IOP_HPP_
