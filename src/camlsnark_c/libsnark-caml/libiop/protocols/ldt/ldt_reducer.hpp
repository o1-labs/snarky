/**@file
*****************************************************************************
Interface for an efficient low-degree test for multiple oracles, in which the
verifier sends random coefficients to the prover and runs an LDT on the
random combination of oracles.
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_LDT_LDT_REDUCER_HPP_
#define LIBIOP_PROTOCOLS_LDT_LDT_REDUCER_HPP_

#include <algorithm>

#include "libiop/algebra/exponentiation.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/iop/utilities/batching.hpp"
#include "libiop/protocols/ldt/ldt_reducer_aux.hpp"
#include "libiop/protocols/ldt/multi_ldt_base.hpp"

namespace libiop {

enum class LDT_reducer_soundness_type {
    /** Proximity parameters are chosen per bounds proven in the Aurora paper */
    proven = 1,
    /** min(1 - max_tested_rate, 1 - max_constraint_rate)
     *  This is a hypothesis merely for testing. */
    optimistic_heuristic = 2,
};

const char* LDT_reducer_soundness_type_to_string(LDT_reducer_soundness_type soundness_type);

template<typename FieldT>
class LDT_instance_reducer_params {
protected:
    size_t interactive_soundness_bits_;
    LDT_reducer_soundness_type soundness_type_;
    size_t codeword_domain_dim_;
    size_t max_tested_degree_bound_;
    size_t max_constraint_degree_bound_;
    bool make_zk_;

    size_t num_output_LDT_instances_;
    size_t absolute_proximity_parameter_;
    long double fractional_proximity_parameter_;
    bool override_security_parameter_ = false;
public:
    LDT_instance_reducer_params() {};
    /** The max tested degree bound is the maximum degree amongst all codewords being low degree tested.
     *  The max constraint degree bound is needed if testing virtual oracles.
     *  Recall that a virtual oracle tests rational constraints, and rational constraints are
     *  composed of a numerator, denominator arithmetic circuits, and a degree to test.
     *  We write that the max constraint degree bound is:
     *     max(rate(Numerator; \vec{p}), tested degree + degree(Denominator)).
     *  For more information regarding the max constraint degree bound,
     *  see the README.md for this directory, or the rational constraint definition in the Aurora paper.
     */
    LDT_instance_reducer_params(const size_t interactive_security_bits,
                                const LDT_reducer_soundness_type soundness_type,
                                const size_t codeword_domain_dim,
                                const size_t max_tested_degree_bound,
                                const size_t max_constraint_degree_bound,
                                const bool make_zk);

    /** UNSAFE!
     *  This is intended to allow experimentation with LDT reducer parameterizations. */
    void override_security_parameter(const size_t num_output_LDT_instances);

    size_t max_tested_degree_bound() const;
    size_t RS_extra_dimensions() const;
    bool make_zk() const;
    size_t absolute_proximity_parameter() const;
    size_t num_output_LDT_instances() const;

    size_t locality() const;
    long double achieved_soundness() const;
    void print() const;
};

template<typename FieldT, typename multi_LDT_type>
class LDT_instance_reducer {
protected:
    iop_protocol<FieldT> &IOP_;
    domain_handle codeword_domain_handle_;
    LDT_instance_reducer_params<FieldT> reducer_params_;
    std::vector<oracle_handle_ptr> input_oracle_handles_;
    std::vector<std::size_t> input_oracle_degrees_;

    field_subset<FieldT> codeword_domain_;

    std::size_t num_random_coefficients_;
    std::vector<verifier_random_message_handle> random_coefficients_handles_;
    std::vector<oracle_handle_ptr> blinding_vector_handles_;

    std::vector<std::shared_ptr<combined_LDT_virtual_oracle<FieldT> > > combined_oracles_;
    std::vector<virtual_oracle_handle> combined_oracle_handles_;

    std::shared_ptr<multi_LDT_type> multi_LDT_;
    std::shared_ptr<multi_LDT_parameter_base<FieldT>> multi_LDT_params_;
public:
    /* Initialization and registration */
    LDT_instance_reducer(iop_protocol<FieldT> &IOP,
                         const domain_handle &codeword_domain_handle,
                         const LDT_instance_reducer_params<FieldT> &reducer_params);

    void set_LDT_params(std::shared_ptr<multi_LDT_parameter_base<FieldT>> &multi_LDT_params);
    void register_interactions(const std::vector<oracle_handle_ptr> &input_oracle_handles);
    void register_queries();

    /* Proving */
    void submit_masking_polynomial();
    void calculate_and_submit_proof();

    /* Verification */
    bool verifier_predicate();
};

} // namespace libiop

#include "libiop/protocols/ldt/ldt_reducer.tcc"

#endif // LIBIOP_PROTOCOLS_LDT_LDT_REDUCER_HPP_
