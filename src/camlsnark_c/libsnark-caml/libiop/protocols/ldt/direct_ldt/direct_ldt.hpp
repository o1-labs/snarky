/**@file
 *****************************************************************************
 Interface for direct low-degree test. The prover sends purported coefficients
 of the low-degree polynomial represented by the oracle.
 The verifier queries multiple locations of the oracle, and checks if they are
 consistent with the purported coefficients.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_LDT_DIRECT_LDT_DIRECT_LDT_HPP_
#define LIBIOP_PROTOCOLS_LDT_DIRECT_LDT_DIRECT_LDT_HPP_

#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/iop/utilities/batching.hpp"
#include "libiop/protocols/ldt/multi_ldt_base.hpp"

namespace libiop {

template<typename FieldT>
class direct_LDT_parameters : public multi_LDT_parameter_base<FieldT> {
    protected:
    size_t query_soundness_bits_;
    size_t poly_degree_bound_;
    size_t RS_extra_dimensions_;
    size_t absolute_proximity_parameter_;

    size_t codeword_domain_dim_;
    long double fractional_proximity_parameter_;
    size_t num_queries_;
    bool overrided_num_queries_ = false;
    public:
    direct_LDT_parameters() {};
    direct_LDT_parameters(const size_t query_soundness_bits,
                          const size_t poly_degree_bound,
                          const size_t RS_extra_dimensions,
                          const size_t absolute_proximity_parameter);

    /** UNSAFE!
     *  This is intended to allow experimentation with Direct LDT parameterization. */
    void override_security_parameter(const size_t num_queries);
    size_t num_queries() const;
    size_t poly_degree_bound() const;

    long double achieved_query_soundness() const;
    void print() const;

    virtual ~direct_LDT_parameters() {};
};

/* Query handles for queries for a single position, across all the inputted oracles. */
using query_handle_for_all_oracles = std::vector<query_handle>;

template<typename FieldT>
class direct_LDT_protocol : public multi_LDT_base<FieldT> {
protected:
    direct_LDT_parameters<FieldT> params_;
    field_subset<FieldT> codeword_domain_;

    std::vector<random_query_position_handle> query_position_handles_;
    /* Indexed by query index, and then by input oracle index */
    std::vector<query_handle_for_all_oracles> query_handles_;

    verifier_random_message_handle empty_verifier_message_handle_;
    /* A handle for each inputted oracle */
    std::vector<prover_message_handle> prover_coefficients_handles_;
public:
    /* Initialization and registration */
    direct_LDT_protocol(iop_protocol<FieldT> &IOP,
                        multi_LDT_parameter_base<FieldT> &params,
                        const domain_handle &codeword_domain_handle,
                        const std::vector<oracle_handle_ptr> &poly_handles);

    void register_interactions();
    void register_queries();

    /* Proving */
    void calculate_and_submit_proof();

    /* Verification */
    bool verifier_predicate();

    virtual ~direct_LDT_protocol() {};
};

} // namespace libiop

#include "libiop/protocols/ldt/direct_ldt/direct_ldt.tcc"

#endif // LIBIOP_PROTOCOLS_LDT_DIRECT_LDT_DIRECT_LDT_HPP_
