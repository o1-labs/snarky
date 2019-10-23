/**@file
*****************************************************************************
Full protocol for R1CS (encoded R1CS + FRI LDT)
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_AURORA_IOP_HPP_
#define LIBIOP_PROTOCOLS_AURORA_IOP_HPP_

#include <cstddef>
#include <functional>
#include <memory>
#include <vector>

#include "libiop/iop/iop.hpp"
#include "libiop/protocols/encoded/r1cs_rs_iop/r1cs_rs_iop.hpp"
#include "libiop/protocols/ldt/fri/fri_ldt.hpp"
#include "libiop/protocols/ldt/ldt_reducer.hpp"
#include "libiop/relations/r1cs.hpp"

namespace libiop {

template<typename FieldT>
class aurora_iop_parameters {
    protected:
    size_t security_parameter_;
    size_t RS_extra_dimensions_;
    bool make_zk_;
    field_subset_type domain_type_;

    size_t extra_systematic_dims_;
    size_t constraint_domain_dim_;
    size_t variable_domain_dim_;
    size_t summation_domain_dim_;
    size_t codeword_domain_dim_;
    size_t query_bound_;

    public:
    aurora_iop_parameters() {};
    aurora_iop_parameters(const size_t security_parameter,
                          const size_t RS_extra_dimensions,
                          const bool make_zk,
                          const libiop::field_subset_type domain_type,
                          const size_t num_constraints,
                          const size_t num_variables);

    void set_ldt_parameters(size_t localization_parameter,
                            FRI_soundness_type fri_soundness_type,
                            LDT_reducer_soundness_type ldt_reducer_soundness_type);
    void set_ldt_parameters(std::vector<size_t> localization_parameters,
                            FRI_soundness_type soundness_type,
                            LDT_reducer_soundness_type ldt_reducer_soundness_type);

    size_t RS_extra_dimensions() const;
    bool make_zk() const;
    field_subset_type domain_type() const;
    size_t constraint_domain_dim() const;
    size_t variable_domain_dim() const;
    size_t codeword_domain_dim() const;
    size_t query_bound() const;
    std::vector<size_t> locality_vector() const;

    long double achieved_soundness() const;
    void print() const;
    LDT_instance_reducer_params<FieldT> LDT_reducer_params_;
    FRI_protocol_parameters<FieldT> FRI_params_;
    encoded_aurora_parameters<FieldT> encoded_aurora_params_;
};

template<typename FieldT>
class aurora_iop {
protected:
    iop_protocol<FieldT> &IOP_;

    r1cs_constraint_system<FieldT> constraint_system_;
    aurora_iop_parameters<FieldT> parameters_;

    domain_handle codeword_domain_handle_;

    std::shared_ptr<encoded_aurora_protocol<FieldT> > protocol_;
    std::shared_ptr<LDT_instance_reducer<FieldT, FRI_protocol<FieldT>> > LDT_reducer_;
public:
    /* Initialization and registration */
    aurora_iop(iop_protocol<FieldT> &IOP,
               const r1cs_constraint_system<FieldT> &constraint_system,
               const aurora_iop_parameters<FieldT> &parameters);

    void register_interactions();
    void register_queries();

    /* Proving */
    void produce_proof(const r1cs_primary_input<FieldT> &primary_input,
                       const r1cs_auxiliary_input<FieldT> &auxiliary_input);

    /* Verification */
    bool verifier_predicate(const r1cs_primary_input<FieldT> &primary_input);
};


} // namespace libiop

#include "libiop/protocols/aurora_iop.tcc"

#endif // LIBIOP_PROTOCOLS_AURORA_IOP_HPP_
