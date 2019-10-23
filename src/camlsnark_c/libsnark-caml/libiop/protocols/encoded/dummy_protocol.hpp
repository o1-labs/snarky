/**@file
 *****************************************************************************
 Dummy protocol for use in FRI SNARK for profiling.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef LIBIOP_PROTOCOLS_ENCODED_DUMMY_PROTOCOL_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_DUMMY_PROTOCOL_HPP_

#include <cstddef>
#include <memory>
#include <vector>

#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/iop/iop.hpp"

namespace libiop {

template<typename FieldT>
class dummy_oracle : public virtual_oracle<FieldT> {
protected:
    std::size_t num_oracles_;
public:
    dummy_oracle(const std::size_t num_oracles);
    virtual std::shared_ptr<std::vector<FieldT>> evaluated_contents(
        const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const;
    virtual FieldT evaluation_at_point(
        const std::size_t evaluation_position,
        const FieldT evaluation_point,
        const std::vector<FieldT> &constituent_oracle_evaluations) const;
};

template<typename FieldT>
class dummy_protocol {
protected:
    iop_protocol<FieldT> &IOP_;
    std::size_t num_oracles_;
    std::size_t RS_extra_dimensions_;
    domain_handle codeword_domain_handle_;
    bool make_zk_;

    field_subset<FieldT> codeword_domain_;

    std::size_t codeword_domain_size_;
    std::size_t codeword_domain_dim_;
    std::size_t degree_;

    std::vector<oracle_handle> constituent_oracles_;
    std::vector<oracle_handle_ptr> constituent_oracle_ptrs_;
    std::shared_ptr<dummy_oracle<FieldT> > oracle_;
    oracle_handle_ptr oracle_handle_ptr_;
public:
    /* Initialization and registration */
    dummy_protocol(iop_protocol<FieldT> &IOP,
                   const std::size_t num_oracles,
                   const std::size_t RS_extra_dimensions,
                   const domain_handle codeword_domain_handle,
                   const bool make_zk);

    /* Proving */
    void calculate_and_submit_response();

    /* Verification */
    bool verifier_predicate();

    oracle_handle_ptr get_oracle_handle();
};

} // namespace libiop

#include "libiop/protocols/encoded/dummy_protocol.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_DUMMY_PROTOCOL_HPP_
